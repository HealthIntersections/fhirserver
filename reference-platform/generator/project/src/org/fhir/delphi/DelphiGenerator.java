package org.fhir.delphi;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.hl7.fhir.dstu2.model.DateTimeType;
import org.hl7.fhir.dstu3.model.OperationDefinition;
import org.hl7.fhir.dstu3.model.OperationDefinition.OperationDefinitionParameterComponent;
import org.hl7.fhir.dstu3.model.OperationDefinition.OperationParameterUse;
import org.hl7.fhir.dstu3.model.StructureDefinition;
import org.hl7.fhir.utilities.Utilities;

/**
 * Generates the delphi reference implementation
 * 
 * todo: the delphi reference implementation depends on too much HL7Connect infrastructure.
 * 
 * @author Grahame
 *
 */
public class DelphiGenerator {

  // this is to generate code with generics, but due to limitations with 
  // delphi's generics (no covariance) it it not used. retained for possible
  // future use if delphi generics are ever sorted out
  private boolean generics = false; 
  
  public enum ClassCategory {
    Type, Component, Resource, AbstractResource
  }

  private DelphiCodeGenerator defCodeType;
  private DelphiCodeGenerator defCodeRes;
  private DelphiCodeGenerator defCodeConstGen;
  private DelphiCodeGenerator defCodeOp;
  private DelphiCodeGenerator prsrCode;
  private DelphiCodeGenerator defIndexer;
  private Definitions definitions;

  private Map<ElementDefn, String> typeNames = new HashMap<ElementDefn, String>();
  private List<String> enumsDone = new ArrayList<String>();
  private Map<String, Integer> enumSizes = new HashMap<String, Integer>();

  private List<ElementDefn> enums = new ArrayList<ElementDefn>();
  private List<String> enumNames = new ArrayList<String>();
  private List<ElementDefn> strucs  = new ArrayList<ElementDefn>();
  private List<String> lists = new ArrayList<String>();

  private StringBuilder workingParserX;
  private StringBuilder workingParserXA;
  private StringBuilder workingComposerX;
  private StringBuilder workingComposerXA;
  private StringBuilder workingParserJ;
  private StringBuilder workingComposerJ;
  private StringBuilder workingParserR;
  private StringBuilder workingComposerR;
  private StringBuilder factoryIntf;
  private StringBuilder factoryImpl;
  private StringBuilder factoryByName;
  private StringBuilder indexHeaders;
  private StringBuilder indexBody;
  private StringBuilder indexMethods;

  private StringBuilder prsrRegX = new StringBuilder();
  private StringBuilder srlsRegX = new StringBuilder();
  private StringBuilder prsrRegJ = new StringBuilder();
  private StringBuilder srlsRegJ = new StringBuilder();
  private StringBuilder srlsRegR = new StringBuilder();
  private StringBuilder prsrImpl = new StringBuilder();
  private StringBuilder prsrdefX = new StringBuilder();
  private StringBuilder srlsdefX = new StringBuilder();
  private StringBuilder prsrdefJ = new StringBuilder();
  private StringBuilder srlsdefJ = new StringBuilder();
  private StringBuilder prsrdefR = new StringBuilder();
  private StringBuilder srlsdefR = new StringBuilder();
  private StringBuilder prsrFragJ = new StringBuilder();
  private StringBuilder prsrFragX = new StringBuilder();
  private StringBuilder prsrFragR = new StringBuilder();
  private StringBuilder prsrDTX = new StringBuilder();
  private StringBuilder prsrDTJ = new StringBuilder();
  private StringBuilder prsrDTR = new StringBuilder();
  private StringBuilder compXBase = new StringBuilder();
  private StringBuilder compJBase = new StringBuilder();
  private StringBuilder compRBase = new StringBuilder();
  private Map<String, String> simpleTypes = new HashMap<String, String>();

  private List<String> types = new ArrayList<String>();
  private List<String> constants = new ArrayList<String>();
  private List<String> opNames = new ArrayList<String>();
  private String destDir;
  
  
  public DelphiGenerator(String destDir) {
    super();
    this.destDir = destDir;
  }

  public void generate(Definitions definitions, String version, DateTimeType dateTimeType, int dstuID)  throws Exception {
    simpleTypes.put("TFhirId", "String");
    
    start(destDir, version, dateTimeType, dstuID);
    initParser(version, dateTimeType);

    this.definitions = definitions;

    generate(definitions.getInfrastructure().get("Element"), "TFHIRBase", false, ClassCategory.Type, true);
    generate(definitions.getInfrastructure().get("BackboneElement"), "TFHIRElement", false, ClassCategory.Type, true);

    parserGap();
    generateElement();
    generatePrimitive(new DefinedCode("enum", "", ""), "TFhirPrimitiveType", true, false);

    for (DefinedCode n : definitions.getPrimitives().values()) {
      if (n instanceof PrimitiveType)
        generatePrimitive(n, "TFhirPrimitiveType", false, false);
    }
    for (DefinedCode n : definitions.getPrimitives().values()) {
      if (!(n instanceof PrimitiveType))
        generatePrimitive(n, ((DefinedStringPattern) n).getBase().contains(" ") ? "TFhirType" : "TFhir"+Utilities.capitalize(((DefinedStringPattern) n).getBase()), false, true);
    }
    parserGap();

    Set<String> done = new HashSet<String>();
    boolean alldone = false;
    while (!alldone) {
      alldone = true;
      for (String s : definitions.getBaseResources().keySet()) {
        ResourceDefn n = definitions.getBaseResources().get(s);
        if (!done.contains(n.getName())) {
          if (Utilities.noString(n.getRoot().typeCode()) || done.contains(n.getRoot().typeCode())) { 
            generate(n.getRoot(), Utilities.noString(n.getRoot().typeCode()) ? "TFHIRBase" : "TFhir"+n.getRoot().typeCode(), true, n.isAbstract() ? ClassCategory.AbstractResource : ClassCategory.Resource, n.isAbstract());
            if (!n.isAbstract()) {
              prsrRegX.append("  else if element.baseName = '"+n.getName()+"' Then\r\n    result := Parse"+n.getName()+"(element, path+'/"+n.getName()+"')\r\n");
              srlsRegX.append("    frt"+n.getName()+": Compose"+n.getName()+"(xml, '"+n.getName()+"', TFhir"+n.getName()+"(resource));\r\n");
              prsrRegJ.append("  else if s = '"+n.getName()+"' Then\r\n    result := Parse"+n.getName()+"(jsn)\r\n");
              srlsRegJ.append("    frt"+n.getName()+": Compose"+n.getName()+"(json, '"+n.getName()+"', TFhir"+n.getName()+"(resource));\r\n");
              srlsRegR.append("    frt"+n.getName()+": Compose"+n.getName()+"(this, '%%%%', '"+n.getName()+"', TFhir"+n.getName()+"(resource), -1);\r\n");
            }
            done.add(n.getName());
          } else
            alldone = false;
        }
      }
    }

    parserGap();

    for (ElementDefn n : definitions.getInfrastructure().values()) {
      if (!n.getName().equals("Element") && !n.getName().equals("BackboneElement")) {
        if (!hasExplicitParent(n))
          generate(n, "TFHIRType", false, ClassCategory.Type, false);
      }
    }

    for (ElementDefn n : definitions.getTypes().values()) {
      if (!hasExplicitParent(n))
        generate(n, "TFhirType", false, ClassCategory.Type, false);
    }

    for (ElementDefn n : definitions.getStructures().values()) {
      if (!hasExplicitParent(n))
        generate(n, "TFhirType", false, ClassCategory.Type, false);
    }

    for (ElementDefn n : definitions.getInfrastructure().values()) {
      if (!n.getName().equals("Element") && !n.getName().equals("BackboneElement")) {
        if (hasExplicitParent(n))
          generate(n, "TFHIR"+n.typeCode(), false, ClassCategory.Type, false);
      }
    }

    for (ElementDefn n : definitions.getTypes().values()) {
      if (hasExplicitParent(n))
        generate(n, "TFhir"+n.typeCode(), false, ClassCategory.Type, false);
    }

    for (ElementDefn n : definitions.getStructures().values()) {
      if (hasExplicitParent(n))
        generate(n, "TFhir"+n.typeCode(), false, ClassCategory.Type, false);
    }

    parserGap();
    for (String s : definitions.sortedResourceNames()) {
      ResourceDefn n = definitions.getResources().get(s);
      generate(n.getRoot(), "TFhir"+n.getRoot().typeCode(), true, ClassCategory.Resource, false);
      generateSearchEnums(n);
      generateIndexInformation(n);
      prsrRegX.append("  else if element.baseName = '"+n.getName()+"' Then\r\n    result := Parse"+n.getName()+"(element, path+'/"+n.getName()+"')\r\n");
      srlsRegX.append("    frt"+n.getName()+": Compose"+n.getName()+"(xml, '"+n.getName()+"', TFhir"+n.getName()+"(resource));\r\n");
      prsrRegJ.append("  else if s = '"+n.getName()+"' Then\r\n    result := Parse"+n.getName()+"(jsn)\r\n");
      srlsRegJ.append("    frt"+n.getName()+": Compose"+n.getName()+"(json, '"+n.getName()+"', TFhir"+n.getName()+"(resource));\r\n");
      srlsRegR.append("    frt"+n.getName()+": Compose"+n.getName()+"(this, '%%%%', '"+n.getName()+"', TFhir"+n.getName()+"(resource), -1);\r\n");
    }

    opStart();
    for (OperationDefinition op : definitions.getOperations()) 
      genOp(op);
    
    finishIndexer();
    
    defCodeConstGen.enumConsts.add("  FHIR_GENERATED_VERSION = '"+version+"';\r\n");
    defCodeConstGen.enumConsts.add("  FHIR_GENERATED_DATE = '"+dateTimeType.asStringValue()+"';\r\n");
    defCodeRes.classDefs.add("  {@Class TFhirResourceFactory : TFHIRBaseFactory\r\n");
    defCodeRes.classDefs.add("     FHIR factory: class constructors and general useful builders\r\n");
    defCodeRes.classDefs.add("  }\r\n");
    defCodeRes.classDefs.add(" TFhirResourceFactory = class (TFHIRBaseFactory)\r\n  public\r\n"+factoryIntf.toString()+"    function makeByName(const name : String) : TFHIRBase;\r\n  end;\r\n");
    types.add("TFhirResourceFactory");
    defCodeRes.classImpls.add(factoryImpl.toString());
    defCodeRes.classImpls.add("function TFHIRResourceFactory.makeByName(const name : String) : TFHIRBase;\r\nbegin\r\n  "+factoryByName.toString().substring(7)+"  else\r\n    result := nil;\r\nend;\r\n\r\n");
    defCodeType.finish();
    defCodeRes.finish();
    defCodeOp.finish();
    defCodeConstGen.finish();
    defIndexer.finish();

    prsrCode.classDefs.add(buildParser());
    prsrCode.classImpls.add(prsrImpl.toString());
    prsrCode.finish();
  }

  private void finishIndexer() {
    defIndexer.classDefs.add("  TFHIRIndexBuilder = class (TAdvObject)\r\n"+
     "  private\r\n"+
        indexHeaders.toString()+
     "  public\r\n"+
     "    procedure registerIndexes(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);\r\n"+
     " end;\r\n");
    defIndexer.classImpls.add(
        indexMethods.toString()+
        "procedure TFHIRIndexBuilder.registerIndexes(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);\r\n"+
        "begin\r\n"+
        indexBody.toString()+
        "end;\r\n");
  }

  private void opStart() {
  }
  
  private void genOp(OperationDefinition op) throws IOException {
    String name = Utilities.capitalize(getPascalName(op.getCode()));
    if (opNames.contains(name))
      return;
    opNames.add(name);

    defCodeOp.classDefs.add("  //Operation "+op.getCode()+" ("+op.getName()+")");

    genOp(op, op.getParameter(), OperationParameterUse.IN, "Request");
    genOp(op, op.getParameter(), OperationParameterUse.OUT, "Response");
  }
  
  private void genOp(OperationDefinition op, List<OperationDefinitionParameterComponent> plist, OperationParameterUse use, String suffix) throws IOException {
    String name = Utilities.capitalize(getPascalName(op.getCode()));
    StringBuilder fields = new StringBuilder();
    StringBuilder accessors = new StringBuilder();
    StringBuilder properties = new StringBuilder();
    StringBuilder destroy = new StringBuilder();
    StringBuilder create = new StringBuilder();
    StringBuilder screate = new StringBuilder();
    StringBuilder params = new StringBuilder();
    StringBuilder gencreate = new StringBuilder();
    StringBuilder names = new StringBuilder();
    List<String> vars = new ArrayList<String>();
    
    for (OperationDefinitionParameterComponent p : plist) {
      if (use == null || p.getUse() == use) {
        String pn = getPascalName(p.getName());
        String pt = null;
        boolean complex = false;
        if (p.getType() == null) {
          pt = "TFHIR"+name+"Op"+Utilities.capitalize(pn);
          genOp(op, p.getPart(), null, Utilities.capitalize(pn));
          complex = true;
        }
        if (pt == null)
          pt = simpleTypes.get("TFhir"+Utilities.capitalize(p.getType()));
        if (pt == null)
          pt = "TFhir"+p.getType();
        if (pt.equals("TFhirtoken"))
          pt = "String";
        if (pt.equals("TFhirAny"))
          pt = "TFhirResource";
        if (pt.equals("TFhir*"))
          pt = "TFhirType";
        boolean obj = !Utilities.existsInList(pt, "String", "Boolean", "Integer");
        boolean list = (p.getMax().equals("*") || Integer.parseInt(p.getMax()) > 1);
        names.append(", '"+p.getName()+"'");
       
        if (list) {
          if (!obj) {
            fields.append("    F"+Utilities.capitalize(pn)+"List : TList<"+pt+">;\r\n");
            gencreate.append("  F"+Utilities.capitalize(pn)+"List := TList<"+pt+">.create;\r\n");
            destroy.append("  F"+Utilities.capitalize(pn)+"List.free;\r\n");
            properties.append("    property "+pn+"List : TList<"+pt+"> read F"+Utilities.capitalize(pn)+"List;\r\n");

            vars.add(pt);
            String v = "v"+Integer.toString(vars.size());
            create.append("  for p in params."+( use == null ? "partList" : "parameterList")+" do\r\n"+
                "    if p.name = '"+p.getName()+"' then\r\n");
            params.append("    for "+v+" in F"+Utilities.capitalize(pn)+"List do\r\n");
            create.append("      F"+Utilities.capitalize(pn)+"List.Add((p.value as TFhir"+Utilities.capitalize(p.getType())+").value);{ob.1}\r\n");
            params.append("      result.AddParameter('"+p.getName()+"', TFhir"+Utilities.capitalize(p.getType())+".create("+v+"));\r\n");
           
          } else {
            fields.append("    F"+Utilities.capitalize(pn)+"List : TAdvList<"+pt+">;\r\n");
            gencreate.append("  F"+Utilities.capitalize(pn)+"List := TAdvList<"+pt+">.create;\r\n");
            destroy.append("  F"+Utilities.capitalize(pn)+"List.free;\r\n");
            properties.append("    property "+pn+"List : TAdvList<"+pt+"> read F"+Utilities.capitalize(pn)+"List;\r\n");

            vars.add(pt);
            String v = "v"+Integer.toString(vars.size());
            create.append("  for p in params."+( use == null ? "partList" : "parameterList")+" do\r\n"+
                "    if p.name = '"+p.getName()+"' then\r\n");
            params.append("    for "+v+" in F"+Utilities.capitalize(pn)+"List do\r\n");
            if (isResource(pt.substring(5))) {
              create.append("      F"+Utilities.capitalize(pn)+"List.Add((p.resource as "+pt+").Link);{ob.2}\r\n");
              params.append("      result.AddParameter('"+p.getName()+"', "+v+".Link);\r\n");
            } else if (isPrimitive(p.getType())) {
              create.append("      F"+Utilities.capitalize(pn)+"List.Add(p.value.Link);{c}\r\n");
              params.append("      result.AddParameter('"+p.getName()+"', "+v+".Link);\r\n");
            } else if (complex) {
              create.append("      F"+Utilities.capitalize(pn)+"List.Add("+pt+".create(p));{a}\r\n");
              params.append("      result.AddParameter("+v+".asParams('"+p.getName()+"'));\r\n");
            } else {
              create.append("      F"+Utilities.capitalize(pn)+"List.Add((p.value as "+pt+").Link);{a}\r\n");
              params.append("      result.AddParameter('"+p.getName()+"', "+v+".Link);\r\n");
            }
          }
        } else {
          fields.append("    F"+Utilities.capitalize(pn)+" : "+pt+";\r\n");
          if (obj) {
            accessors.append("    procedure Set"+Utilities.capitalize(pn)+"(value : "+pt+");\r\n");
            destroy.append("  F"+Utilities.capitalize(pn)+".free;\r\n");
            defCodeOp.classImpls.add("procedure TFHIR"+name+"Op"+suffix+".Set"+Utilities.capitalize(pn)+"(value : "+pt+");\r\nbegin\r\n  F"+Utilities.capitalize(pn)+".free;\r\n  F"+Utilities.capitalize(pn)+" := value;\r\nend;\r\n");
          }
          properties.append("    property "+pn+" : "+pt+" read F"+Utilities.capitalize(pn)+" write "+(obj ? "Set" : "F")+Utilities.capitalize(pn)+";\r\n");
          
          if (!pt.equals("Boolean")) {
            if (pt.equals("String"))
              params.append("    if (F"+Utilities.capitalize(pn)+" <> '') then\r\n");
            else
              params.append("    if (F"+Utilities.capitalize(pn)+" <> nil) then\r\n");
          }
          if (!isPrimitive(p.getType()) && !"token".equals(p.getType())) {
            if (isResource(pt.substring(5))) {
              params.append("      result.addParameter('"+p.getName()+"', F"+Utilities.capitalize(pn)+".Link);{oz.5a}\r\n");
              create.append("  F"+Utilities.capitalize(pn)+" := (params.res['"+p.getName()+"'] as "+pt+").Link;{ob.5a}\r\n");
            } else if (pt.equals("String")) {
              params.append("      result.addParameter('"+p.getName()+"', TFHIR"+Utilities.capitalize(p.getType())+".create(F"+Utilities.capitalize(pn)+"));{oz.5b}\r\n");
              create.append("  F"+Utilities.capitalize(pn)+" := (params.param['"+p.getName()+"'].value as TFHIR"+Utilities.capitalize(p.getType())+").Value; {ob.5b}\r\n");
            } else if (complex) {
              params.append("      result.addParameter(F"+Utilities.capitalize(pn)+".asParams('"+p.getName()+"'));{oz.5c}\r\n");
              create.append("  F"+Utilities.capitalize(pn)+" := "+pt+".create(params.param['"+p.getName()+"']); {ob.5c}\r\n");
            } else {
              params.append("      result.addParameter('"+p.getName()+"', F"+Utilities.capitalize(pn)+".Link);{oz.5d}\r\n");
              create.append("  F"+Utilities.capitalize(pn)+" := (params.param['"+p.getName()+"'].value as "+pt+").Link; {ob.5d}\r\n");
            }
          } else if (obj) {
            params.append("      result.addParameter('"+p.getName()+"', TFHIR"+Utilities.capitalize(p.getType())+".create(F"+Utilities.capitalize(pn)+"));{oz.5e}\r\n");
            create.append("  F"+Utilities.capitalize(pn)+" := (params.param['"+p.getName()+"'].value as TFHIR"+Utilities.capitalize(p.getType())+").value;\r\n");
          } else {
            params.append("      result.addParameter('"+p.getName()+"', TFHIR"+Utilities.capitalize(p.getType())+".create(F"+Utilities.capitalize(pn)+"));{oz.5f}\r\n");
            if (pt.equals("Boolean")) {
              create.append("  F"+Utilities.capitalize(pn)+" := params.bool['"+p.getName()+"'];\r\n");
              screate.append("  F"+Utilities.capitalize(pn)+" := StrToBool(params.getVar('"+p.getName()+"'));\r\n");
            } else {
              create.append("  F"+Utilities.capitalize(pn)+" := params.str['"+p.getName()+"'];\r\n");
              screate.append("  F"+Utilities.capitalize(pn)+" := params.getVar('"+p.getName()+"');\r\n");
            }
          }
        }
      }
    }

    if (use == null) 
    defCodeOp.classDefs.add(
        "  TFHIR"+name+"Op"+suffix+" = class (TFHIROperationObject)\r\n  private\r\n"+
            fields.toString()+
            accessors.toString()+
            "  protected\r\n"+
            "    function isKnownName(name : String) : boolean; override;\r\n"+
            "  public\r\n"+
            "    constructor Create; overload;\r\n"+
            "    constructor Create(params : TFhirParametersParameter); overload;\r\n"+
            "    destructor Destroy; override;\r\n"+
            "    function asParams(name : String) : TFHIRParametersParameter;\r\n"+
            properties.toString()+
        "  end;\r\n");
    else
      defCodeOp.classDefs.add(
        "  TFHIR"+name+"Op"+suffix+" = class (TFHIROperation"+suffix+")\r\n  private\r\n"+
            fields.toString()+
            accessors.toString()+
            "  protected\r\n"+
            "    function isKnownName(name : String) : boolean; override;\r\n"+
            "  public\r\n"+
            "    constructor Create; overload; override;\r\n"+
            "    destructor Destroy; override;\r\n"+
            "    procedure load(params : TFHIRParameters); overload; override;\r\n"+
            "    procedure load(params : TParseMap); overload; override;\r\n"+
            "    function asParams : TFHIRParameters; override;\r\n"+
            properties.toString()+
        "  end;\r\n");
    defCodeOp.classImpls.add("constructor TFHIR"+name+"Op"+suffix+".create;\r\n"+
        "begin\r\n"+
        "  inherited create();\r\n"+
        gencreate.toString()+
        "end;\r\n");
    if (use == null)
      defCodeOp.classImpls.add("constructor TFHIR"+name+"Op"+suffix+".create(params : TFhirParametersParameter);\r\n"+
        (gencreate.length() > 0 ? "var\r\n  p : TFhirParametersParameter;\r\n" : "")+
        "begin\r\n"+
        "  inherited create();\r\n"+
        gencreate.toString()+
        create.toString()+
        "  loadExtensions(params);\r\n"+
        "end;\r\n");
    if (use != null) {
      defCodeOp.classImpls.add("procedure TFHIR"+name+"Op"+suffix+".load(params : "+(use == null ? "TFhirParametersParameter" : "TFHIRParameters")+");\r\n"+
        (gencreate.length() > 0 ? "var\r\n  p : TFhirParametersParameter;\r\n" : "")+
        "begin\r\n"+
        create.toString()+
        "  loadExtensions(params);\r\n"+
        "end;\r\n");
      defCodeOp.classImpls.add("procedure TFHIR"+name+"Op"+suffix+".load(params : TParseMap);\r\n"+
          "begin\r\n"+
          screate.toString()+
          "  loadExtensions(params);\r\n"+
          "end;\r\n");
    }
    defCodeOp.classImpls.add("destructor TFHIR"+name+"Op"+suffix+".Destroy;\r\nbegin\r\n"+destroy.toString()+"  inherited;\r\nend;\r\n");
    if (use == null) 
      defCodeOp.classImpls.add("function TFHIR"+name+"Op"+suffix+".asParams(name : String) : TFhirParametersParameter;");
    else
      defCodeOp.classImpls.add("function TFHIR"+name+"Op"+suffix+".asParams : TFhirParameters;");
    if (vars.size() > 0) {
      defCodeOp.classImpls.add("var");
      for (int i = 0; i < vars.size(); i++) {
        defCodeOp.classImpls.add("  v"+Integer.toString(i+1)+" : "+vars.get(i)+";");
      }
    }
    if (use == null) 
    defCodeOp.classImpls.add(
        "begin\r\n"+
        "  result := TFHIRParametersParameter.create;\r\n"+
        "  try\r\n"+
        "    result.name := name;\r\n"+
        params.toString()+
        "    writeExtensions(result);\r\n"+
        "    result.link;\r\n  finally\r\n    result.free;\r\n  end;\r\nend;\r\n");
    else
    defCodeOp.classImpls.add(
        "begin\r\n"+
        "  result := TFHIRParameters.create;\r\n"+
        "  try\r\n"+
        params.toString()+
        "    writeExtensions(result);\r\n"+
        "    result.link;\r\n  finally\r\n    result.free;\r\n  end;\r\nend;\r\n");
    defCodeOp.classImpls.add("function TFHIR"+name+"Op"+suffix+".isKnownName(name : String) : boolean;");
    if (names.length() == 0)
      defCodeOp.classImpls.add(
          "begin\r\n"+
          "  result := false;\r\n"+
          "end;\r\n");
    else
    defCodeOp.classImpls.add(
        "begin\r\n"+
        "  result := StringArrayExists(["+names.substring(2)+"], name);\r\n"+
        "end;\r\n");
  }

  private boolean isResource(String name) {
    return definitions.hasResource(name) || name.equals("Resource") || name.equals("DomainResource") || name.equals("Parameters");
  }

  private String getPascalName(String code) {
    StringBuilder b = new StringBuilder();
    boolean up = false;
    for (char ch : code.toCharArray()) {
      if (Character.isAlphabetic(ch)) {
        if (up)
          b.append(Character.toUpperCase(ch));
        else
          b.append(ch);
        up = false;
      } else 
        up = true;
    }
    if (GeneratorUtils.isDelphiReservedWord(b.toString()))
      return b.toString()+"_";
    else
      return b.toString();
  }

  private void parserGap() {
    prsrdefX.append("\r\n");
    prsrdefJ.append("\r\n");
    srlsdefX.append("\r\n");
    srlsdefJ.append("\r\n");
    srlsdefR.append("\r\n");
    srlsdefR.append("\r\n");
  }

  private void start(String implDir, String version, DateTimeType dateTimeType, int dstuID)
      throws UnsupportedEncodingException, FileNotFoundException, Exception {
    defCodeRes = new DelphiCodeGenerator(new FileOutputStream(Utilities.path(implDir, "FHIRResources.pas")), dstuID);
    defCodeRes.start();
    defCodeRes.name = "FHIRResources";
    defCodeRes.comments.add("FHIR v"+version+" generated "+dateTimeType.asStringValue());
    defCodeRes.precomments.add("!Wrapper uses FHIRBase, FHIRBase_Wrapper, FHIRTypes, FHIRTypes_Wrapper, DateAndTime, DateAndTime_Wrapper");
    defCodeRes.uses.add("SysUtils");
    defCodeRes.uses.add("Classes");
    defCodeRes.uses.add("StringSupport");
    defCodeRes.uses.add("DecimalSupport");
    defCodeRes.uses.add("AdvBuffers");
    if (generics)
      defCodeRes.uses.add("AdvGenerics");
    defCodeRes.uses.add("DateAndTime");
    defCodeRes.uses.add("FHIRBase");
    defCodeRes.uses.add("FHIRTypes");
    defCodeRes.usesImpl.add("FHIRUtilities");

    defCodeConstGen = new DelphiCodeGenerator(new FileOutputStream(Utilities.path(implDir, "FHIRConstants.pas")), dstuID);
    defCodeConstGen.start();
    defCodeConstGen.name = "FHIRConstants";
    defCodeConstGen.comments.add("FHIR v"+version+" generated "+dateTimeType.asStringValue());
    defCodeConstGen.precomments.add("!Wrapper uses FHIRBase, FHIRBase_Wrapper, FHIRTypes, FHIRTypes_Wrapper, FHIRResources, FHIRResources_Wrapper");
    defCodeConstGen.precomments.add("!ignore ALL_RESOURCE_TYPES");
    defCodeConstGen.uses.add("SysUtils");
    defCodeConstGen.uses.add("Classes");
    defCodeConstGen.uses.add("StringSupport");
    defCodeConstGen.uses.add("DecimalSupport");
    defCodeConstGen.uses.add("AdvBuffers");
    if (generics)
      defCodeConstGen.uses.add("AdvGenerics");
    defCodeConstGen.uses.add("DateAndTime");
    defCodeConstGen.uses.add("FHIRBase");
    defCodeConstGen.uses.add("FHIRTypes");
    defCodeConstGen.uses.add("FHIRResources");

    defIndexer = new DelphiCodeGenerator(new FileOutputStream(Utilities.path(implDir, "FHIRIndexInformation.pas")), dstuID);
    defIndexer.start();
    defIndexer.name = "FHIRIndexInformation";
    defIndexer.comments.add("FHIR v"+version+" generated "+dateTimeType.asStringValue());
    defIndexer.uses.add("SysUtils");
    defIndexer.uses.add("Classes");
    defIndexer.uses.add("StringSupport");
    defIndexer.uses.add("DecimalSupport");
    defIndexer.uses.add("AdvBuffers");
    defIndexer.uses.add("DateAndTime");
    defIndexer.uses.add("FHIRIndexManagers");
    defIndexer.uses.add("FHIRResources");
    defIndexer.uses.add("FHIRTypes");
    defIndexer.uses.add("FHIRConstants");
    defIndexer.uses.add("FHIRSupport");

    indexHeaders = new StringBuilder();
    indexBody = new StringBuilder();
    indexMethods = new StringBuilder();

    
    defCodeType = new DelphiCodeGenerator(new FileOutputStream(Utilities.path(implDir, "FHIRTypes.pas")), dstuID);
    defCodeType.start();
    defCodeType.name = "FHIRTypes";
    defCodeType.comments.add("FHIR v"+version+" generated "+dateTimeType.asStringValue());
    defCodeType.precomments.add("!Wrapper uses FHIRBase, FHIRBase_Wrapper");
    defCodeType.uses.add("Classes");
    defCodeType.uses.add("SysUtils");
    defCodeType.uses.add("DecimalSupport");
    defCodeType.uses.add("StringSupport");
    //    defCodeType.uses.add("AdvWideStringLists");
    defCodeType.uses.add("AdvBuffers");
    if (generics)
      defCodeType.uses.add("AdvGenerics");
    defCodeType.uses.add("EncdDecd");
    defCodeType.uses.add("DateAndTime");
    defCodeType.uses.add("FHIRBase");
    defCodeType.usesImpl.add("FHIRUtilities");

    factoryIntf = new StringBuilder();
    factoryImpl = new StringBuilder();
    factoryByName = new StringBuilder();


    prsrCode = new DelphiCodeGenerator(new FileOutputStream(Utilities.path(implDir, "FHIRParser.pas")), dstuID);
    prsrCode.start();
    prsrCode.name = "FHIRParser";
    
    defCodeOp = new DelphiCodeGenerator(new FileOutputStream(Utilities.path(implDir, "FHIROperations.pas")), dstuID);
    defCodeOp.start();
    defCodeOp.name = "FHIROperations";
    defCodeOp.comments.add("FHIR v"+version+" generated "+dateTimeType.asStringValue());
    defCodeOp.precomments.add("!Wrapper uses FHIRBase, FHIRBase_Wrapper, FHIRTypes, FHIRTypes_Wrapper, DateAndTime, DateAndTime_Wrapper");
    defCodeOp.uses.add("SysUtils");
    defCodeOp.uses.add("Classes");
    defCodeOp.uses.add("Generics.Collections");
    defCodeOp.uses.add("StringSupport");
    defCodeOp.uses.add("DecimalSupport");
    defCodeOp.uses.add("AdvBuffers");
    defCodeOp.uses.add("AdvGenerics");
    defCodeOp.uses.add("ParseMap");
    defCodeOp.uses.add("DateAndTime");
    defCodeOp.uses.add("FHIRBase");
    defCodeOp.uses.add("FHIRTypes");
    defCodeOp.uses.add("FHIRResources");
    defCodeOp.uses.add("FHIROpBase");
    defCodeOp.usesImpl.add("FHIRUtilities");
  }

  private void generate(ElementDefn root, String superClass, boolean resource, ClassCategory category, boolean isAbstract) throws Exception {
    typeNames.clear();
    enums.clear();
    strucs.clear();
    enumNames.clear();
    Map<ElementDefn, List<String>> paths = new HashMap<ElementDefn, List<String>>();

    for (ElementDefn e : root.getElements()) {
      scanNestedTypes(root, root.getName(), e, root.getName(), paths);
    }

    for (ElementDefn e : enums) {
      generateEnum(e);
    }
    for (ElementDefn e : strucs) {
      generateType(e, category == ClassCategory.Resource ? ClassCategory.Component : category, paths.get(e));
    }

    if (category == ClassCategory.AbstractResource) 
      genTypeAbstract(root, "TFhir"+root.getName(), superClass, category);
    else
      genType(root, "TFhir"+root.getName(), superClass, category, isAbstract);
    
  }

  private DelphiCodeGenerator getCode(ClassCategory category) {
    switch (category) {
    case Type:
      return defCodeType;
    case Component:
      return defCodeRes;
    case Resource:
      return defCodeRes;
    case AbstractResource:
      return defCodeRes;
    }
    return null;
  }

  private void genType(ElementDefn root, String tn, String superClass, ClassCategory category, boolean isAbstract) throws Exception {
    prsrdefX.append("    function Parse"+root.getName()+"(element : IXmlDomElement; path : string) : TFhir"+root.getName()+";\r\n");
    if (!root.getName().equals("Element") && !root.getName().equals("BackboneElement"))
      prsrdefX.append("    function Parse"+root.getName()+"Child(element : TFhir"+root.getName()+"; path : string; child : IXmlDomElement) : boolean;\r\n");
    srlsdefX.append("    procedure Compose"+root.getName()+"(xml : TXmlBuilder; name : string; elem : TFhir"+root.getName()+");\r\n");
    if (!root.getName().equals("Element") && !root.getName().equals("BackboneElement"))
      srlsdefX.append("    procedure Compose"+root.getName()+"Children(xml : TXmlBuilder; elem : TFhir"+root.getName()+");\r\n");
    prsrdefJ.append("    function Parse"+root.getName()+"(jsn : TJsonObject) : TFhir"+root.getName()+"; overload;\r\n");
    if (!root.getName().equals("Element") && !root.getName().equals("BackboneElement"))
      prsrdefJ.append("    procedure Parse"+root.getName()+"Properties(jsn : TJsonObject; result : TFhir"+root.getName()+"); overload;\r\n");
    srlsdefJ.append("    procedure Compose"+root.getName()+"(json : TJSONWriter; name : string; elem : TFhir"+root.getName()+"; noObj : boolean = false);"+"\r\n");
    prsrdefR.append("    function Parse"+root.getName()+"(rdf : TObject) : TFhir"+root.getName()+"; overload;\r\n");
    if (root.getName().equals("Coding"))
      srlsdefR.append("    procedure Compose"+root.getName()+"(inCodeable : boolean; parent :  TRDFComplex; parentType, name : String; elem : TFhir"+root.getName()+"; index : integer);"+"\r\n");
    else
      srlsdefR.append("    procedure Compose"+root.getName()+"(parent :  TRDFComplex; parentType, name : String; elem : TFhir"+root.getName()+"; index : integer);"+"\r\n");
    prsrFragJ.append("  else if (type_ = '"+tn+"') then\r\n    result := parse"+root.getName()+"(jsn)\r\n");
    prsrFragX.append("  else if SameText(element.NodeName, '"+tn+"') then\r\n    result := parse"+root.getName()+"(element, element.nodeName)\r\n");
    prsrFragR.append("  else if SameText(element.NodeName, '"+tn+"') then\r\n    result := parse"+root.getName()+"(rdf, element.nodeName)\r\n");
    if (category == ClassCategory.Type && !isAbstract) {
      prsrDTJ.append("  else if (type_ = "+tn+") then\r\n    result := parse"+root.getName()+"(jsn)\r\n");
      prsrDTX.append("  else if (type_ = "+tn+") then\r\n    result := parse"+root.getName()+"(element, name)\r\n");
      prsrDTR.append("  else if (type_ = "+tn+") then\r\n    result := parse"+root.getName()+"(rdf, name)\r\n");
    }
    if (!isAbstract) {
      compJBase.append("  else if (base is "+tn+") then\r\n    compose"+root.getName()+"(json, name, "+tn+"(base), false)\r\n");
      compXBase.append("  else if (base is "+tn+") then\r\n    compose"+root.getName()+"(xml, name,  "+tn+"(base))\r\n");
      compRBase.append("  else if (base is "+tn+") then\r\n    compose"+root.getName()+"(section, name,  "+tn+"(base), -1)\r\n");
    }
    workingParserX = new StringBuilder();
    workingParserXA = new StringBuilder();
    workingComposerX = new StringBuilder();
    workingComposerXA = new StringBuilder();
    workingParserJ = new StringBuilder();
    workingComposerJ = new StringBuilder();
    workingParserR = new StringBuilder();
    workingComposerR = new StringBuilder();


    StringBuilder def = new StringBuilder();
    StringBuilder defPriv1 = new StringBuilder();
    StringBuilder defPriv2 = new StringBuilder();
    StringBuilder defPub = new StringBuilder();
    StringBuilder impl = new StringBuilder();
    StringBuilder create = new StringBuilder();
    StringBuilder destroy = new StringBuilder();
    StringBuilder assign = new StringBuilder();
    StringBuilder getkids = new StringBuilder();
    StringBuilder getkidsvars = new StringBuilder();
    StringBuilder getprops = new StringBuilder();
    StringBuilder getpropsvars = new StringBuilder();
    StringBuilder setprops = new StringBuilder();
    StringBuilder makeprops = new StringBuilder();
    impl.append("{ "+tn+" }\r\n\r\n");
  
    for (ElementDefn e : root.getElements()) {
      generateField(e, root.getName(), defPriv1, defPriv2, defPub, impl, create, destroy, assign, getkids, getkidsvars, getprops, getpropsvars, setprops, makeprops, tn, "", category, tn.equals("TFhirExtension"));
    }

    def.append("  {@Class "+tn+" : "+superClass+"\r\n");
    def.append("    "+makeDocoSafe(root.getDefinition())+"\r\n");
    def.append("  }\r\n");
    def.append("  "+tn+" = class ("+superClass+")\r\n");
    types.add(tn);
    factoryIntf.append("    {@member new"+tn.substring(5)+"\r\n      create a new "+root.getName()+"\r\n    }\r\n    {!script nolink}\r\n    function new"+tn.substring(5)+" : "+tn+";\r\n");    
    factoryImpl.append("function TFhirResourceFactory.new"+tn.substring(5)+" : "+tn+";\r\nbegin\r\n  result := "+tn+".create;\r\nend;\r\n\r\n");
    factoryByName.append("  else if name = '"+tn.substring(5)+"' then\r\n    result := new"+tn.substring(5)+"()\r\n");
    def.append("  private\r\n");
    def.append(defPriv1.toString());
    def.append(defPriv2.toString());
    def.append("  protected\r\n");
    if (category == ClassCategory.Resource) {
      def.append("    function GetResourceType : TFhirResourceType; override;\r\n");      
    }
    def.append("    Procedure GetChildrenByName(child_name : string; list : "+listForm("TFHIRObject")+"); override;\r\n");
    def.append("    Procedure ListProperties(oList : "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues : Boolean); Override;\r\n");
    def.append("  public\r\n");
    def.append("    constructor Create; Override;\r\n");
    def.append("    destructor Destroy; override;\r\n");
    def.append("    {!script hide}\r\n");
    def.append("    procedure Assign(oSource : TAdvObject); override;\r\n");
    def.append("    function Link : "+tn+"; overload;\r\n");
    def.append("    function Clone : "+tn+"; overload;\r\n");
    def.append("    procedure setProperty(propName : string; propValue : TFHIRObject); override;\r\n");
    def.append("    function makeProperty(propName : string): TFHIRObject; override;\r\n");
    def.append("    function fhirType : string; override;\r\n");
    def.append("    function equalsDeep(other : TFHIRBase) : boolean; override;\r\n");
    def.append("    function equalsShallow(other : TFHIRBase) : boolean; override;\r\n");
    def.append("    {!script show}\r\n");
    def.append("  published\r\n");
    def.append(defPub.toString());
    def.append("  end;\r\n");
    def.append("\r\n");
    StringBuilder impl2 = new StringBuilder();
    impl2.append("{ "+tn+" }\r\n\r\n");
    impl2.append("constructor "+tn+".Create;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  inherited;\r\n");
    impl2.append(create.toString());
    impl2.append("end;\r\n\r\n");

    impl2.append("destructor "+tn+".Destroy;\r\n");
    impl2.append("begin\r\n");
    impl2.append(destroy.toString());
    impl2.append("  inherited;\r\n");
    impl2.append("end;\r\n\r\n");
    if (category == ClassCategory.Resource) {
      impl2.append("function "+tn+".GetResourceType : TFhirResourceType;\r\nbegin\r\n  result := frt"+root.getName()+";\r\nend;\r\n\r\n");       
    }

    impl2.append("procedure "+tn+".Assign(oSource : TAdvObject);\r\n");
    impl2.append("begin\r\n");
    impl2.append("  inherited;\r\n");
    impl2.append(assign.toString());
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".GetChildrenByName(child_name : string; list : "+listForm("TFHIRObject")+");\r\n");
    if (getkidsvars.length() > 0) {
      impl2.append("var\r\n");
      impl2.append(getkidsvars.toString());
    }
    impl2.append("begin\r\n");
    impl2.append("  inherited;\r\n");
    impl2.append(getkids.toString());
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".ListProperties(oList: "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues: Boolean);\r\n");
    if (getpropsvars.length() > 0) {
      impl2.append("var\r\n  prop : TFHIRProperty;\r\n");      
      impl2.append(getpropsvars.toString());
    }
    impl2.append("begin\r\n");
    impl2.append("  inherited;\r\n");
    impl2.append(getprops.toString());
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".setProperty(propName: string; propValue: TFHIRObject);\r\n");
    impl2.append("begin\r\n");
    if (setprops.length() > 7) {
      impl2.append("  "+setprops.toString().substring(7));
      impl2.append("  else inherited;\r\n");
    } else {
      impl2.append("  inherited;\r\n");
    }
    impl2.append("end;\r\n\r\n");
    impl2.append("function "+tn+".makeProperty(propName: string) : TFHIRObject;\r\n");
    impl2.append("begin\r\n");
    if (makeprops.length() > 7) {
      impl2.append("  "+makeprops.toString().substring(7));
      impl2.append("  else result := inherited makeProperty(propName);\r\n");
    } else
      impl2.append("  result := inherited makeProperty(propName);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function "+tn+".fhirType : string;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := '"+root.getName()+"';\r\n");
    impl2.append("end;\r\n\r\n");

    generateEquals(root, tn, impl2);

    impl2.append("function "+tn+".Link : "+tn+";\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := "+tn+"(inherited Link);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function "+tn+".Clone : "+tn+";\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := "+tn+"(inherited Clone);\r\n");
    impl2.append("end;\r\n\r\n");
    getCode(category).classDefs.add(def.toString());
    getCode(category).classImpls.add(impl2.toString() + impl.toString());
    getCode(category).classFwds.add("  "+tn+" = class;\r\n");
    generateParser(root, tn, category, !superClass.equals("TFHIRObject"), root.typeCode());
    defineList(tn, tn+"List", null, category, category == ClassCategory.AbstractResource, false);
  }

  private void genTypeAbstract(ElementDefn root, String tn, String superClass, ClassCategory category) throws Exception {
    boolean isBase = Utilities.noString(root.typeCode());
    if (root.getName().equals("Resource"))
      generateResource();

    StringBuilder def = new StringBuilder();
    StringBuilder defPriv1 = new StringBuilder();
    StringBuilder defPriv2 = new StringBuilder();
    StringBuilder defPub = new StringBuilder();
    StringBuilder impl = new StringBuilder();
    StringBuilder create = new StringBuilder();
    StringBuilder destroy = new StringBuilder();
    StringBuilder assign = new StringBuilder();
    StringBuilder getkids = new StringBuilder();
    StringBuilder getkidsvars = new StringBuilder();
    StringBuilder getprops = new StringBuilder();
    StringBuilder getpropsvars = new StringBuilder();
    StringBuilder setprops = new StringBuilder();
    StringBuilder makeprops = new StringBuilder();
    impl.append("{ "+tn+" }\r\n\r\n");

    workingParserX = new StringBuilder();
    workingParserXA = new StringBuilder();
    workingComposerX = new StringBuilder();
    workingComposerXA = new StringBuilder();
    workingParserJ = new StringBuilder();
    workingComposerJ = new StringBuilder();
    workingParserR = new StringBuilder();
    workingComposerR = new StringBuilder();

    for (ElementDefn e : root.getElements()) {
      generateField(e, root.getName(), defPriv1, defPriv2, defPub, impl, create, destroy, assign, getkids, getkidsvars, getprops, getpropsvars, setprops, makeprops, tn, "", category, e.getName().equals("Extension"));
    }

    def.append("  {@Class "+tn+" : "+superClass+"\r\n");
    def.append("    "+makeDocoSafe(root.getDefinition())+"\r\n");
    def.append("  }\r\n");
    def.append("  "+tn+" = {abstract} class ("+superClass+")\r\n");
    types.add(tn);
    def.append("  private\r\n");
    def.append(defPriv1.toString());
    def.append(defPriv2.toString());
    def.append("  protected\r\n");
    def.append("    Procedure GetChildrenByName(child_name : string; list : "+listForm("TFHIRObject")+"); override;\r\n");
    def.append("    Procedure ListProperties(oList : "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues : Boolean); Override;\r\n");
    if (isBase) {
      def.append("    function GetResourceType : TFhirResourceType; virtual; abstract;\r\n");
    }
    def.append("  public\r\n");
    def.append("    constructor Create; Override;\r\n");
    def.append("    destructor Destroy; override;\r\n");
    def.append("    {!script hide}\r\n");
    def.append("    procedure Assign(oSource : TAdvObject); override;\r\n");
    def.append("    function Link : "+tn+"; overload;\r\n");
    def.append("    function Clone : "+tn+"; overload;\r\n");
    def.append("    procedure setProperty(propName : string; propValue : TFHIRObject); override;\r\n");
    def.append("    function makeProperty(propName : string) : TFHIRObject; override;\r\n");
    def.append("    function fhirType : string; override;\r\n");
    def.append("    function equalsDeep(other : TFHIRBase) : boolean; override;\r\n");
    def.append("    function equalsShallow(other : TFHIRBase) : boolean; override;\r\n");
    def.append("    {!script show}\r\n");
    def.append("  published\r\n");
    if (isBase) {
      def.append("    Property ResourceType : TFhirResourceType read GetResourceType;\r\n\r\n");
    }
    def.append(defPub.toString());
    def.append("  end;\r\n");
    def.append("\r\n");
    if (root.getName().equals("Resource"))
      def.append("  TFhirResourceClass = class of TFhirResource;\r\n");

    StringBuilder impl2 = new StringBuilder();
    impl2.append("{ "+tn+" }\r\n\r\n");
    impl2.append("constructor "+tn+".Create;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  inherited;\r\n");
    impl2.append(create.toString());
    impl2.append("end;\r\n\r\n");

    impl2.append("destructor "+tn+".Destroy;\r\n");
    impl2.append("begin\r\n");
    impl2.append(destroy.toString());
    impl2.append("  inherited;\r\n");
    impl2.append("end;\r\n\r\n");

    impl2.append("procedure "+tn+".Assign(oSource : TAdvObject);\r\n");
    impl2.append("begin\r\n");
    impl2.append("  inherited;\r\n");
    impl2.append(assign.toString());
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".GetChildrenByName(child_name : string; list : "+listForm("TFHIRObject")+");\r\n");
    if (getkidsvars.length() > 0) {
      impl2.append("var\r\n");
      impl2.append(getkidsvars.toString());
    }
    impl2.append("begin\r\n");
    impl2.append("  inherited;\r\n");
    impl2.append(getkids.toString());
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".ListProperties(oList: "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues: Boolean);\r\n");
    if (getpropsvars.length() > 0) {
      impl2.append("var\r\n  prop : TFHIRProperty;\r\n");      
      impl2.append(getpropsvars.toString());
    }
    impl2.append("begin\r\n");
    impl2.append("  inherited;\r\n");
    impl2.append(getprops.toString());
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".setProperty(propName: string; propValue: TFHIRObject);\r\n");
    impl2.append("begin\r\n");
    impl2.append("  "+setprops.toString().substring(7));
    impl2.append("  else inherited;\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function "+tn+".makeProperty(propName: string) : TFHIRObject;\r\n");
    impl2.append("begin\r\n");
    if (makeprops.length() > 7) {
      impl2.append("  "+makeprops.toString().substring(7));
      impl2.append("  else result := inherited makeProperty(propName);\r\n");
    } else
      impl2.append("  result := inherited makeProperty(propName);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function "+tn+".fhirType : string;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := '"+root.getName()+"';\r\n");
    impl2.append("end;\r\n\r\n");
    generateEquals(root, tn, impl2);


    impl2.append("function "+tn+".Link : "+tn+";\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := "+tn+"(inherited Link);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function "+tn+".Clone : "+tn+";\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := "+tn+"(inherited Clone);\r\n");
    impl2.append("end;\r\n\r\n");
    getCode(category).classDefs.add(def.toString());
    getCode(category).classImpls.add(impl2.toString() + impl.toString());
    getCode(category).classFwds.add("  "+tn+" = class;\r\n");
    defineList(tn, tn+"List", null, category, true, false);

    prsrdefX.append("    Procedure Parse"+root.getName()+"Attributes(resource : "+tn+"; path : string; element : IXmlDomElement);\r\n");
    prsrImpl.append("Procedure TFHIRXmlParser.Parse"+root.getName()+"Attributes(resource : "+tn+"; path : string; element : IXmlDomElement);\r\n");
    prsrImpl.append("begin\r\n");
    if (!isBase)
      prsrImpl.append("  Parse"+root.typeCode()+"Attributes(resource, path, element);\r\n");
    else
      prsrImpl.append("  GetObjectLocation(resource, element);\r\n");      
    prsrImpl.append(workingParserXA.toString());
    prsrImpl.append("end;\r\n\r\n");
    prsrdefX.append("    Function Parse"+root.getName()+"Child(resource : "+tn+"; path : string; child : IXmlDomElement) : boolean;\r\n");
    prsrImpl.append("Function TFHIRXmlParser.Parse"+root.getName()+"Child(resource : "+tn+"; path : string; child : IXmlDomElement) : boolean;\r\n");
    prsrImpl.append("begin\r\n");
    prsrImpl.append("  result := true;\r\n");
    prsrImpl.append("  "+workingParserX.toString().substring(11).replace("      ", "  ").replace("result.", "resource."));
    if (isBase)
      prsrImpl.append("  else\r\n");
    else
      prsrImpl.append("  else if not parse"+root.typeCode()+"Child(resource, path, child) then\r\n");
    prsrImpl.append("    result := false;\r\n");
    prsrImpl.append("end;\r\n\r\n");

    prsrdefJ.append("    procedure Parse"+root.getName()+"Properties(jsn : TJsonObject; resource : "+tn+");\r\n");
    prsrImpl.append("procedure TFHIRJsonParser.Parse"+root.getName()+"Properties(jsn : TJsonObject; resource : "+tn+");\r\n");
    prsrImpl.append("begin\r\n");
    if (!isBase)
      prsrImpl.append("  Parse"+root.typeCode()+"Properties(jsn, resource);\r\n");
    else {
      prsrImpl.append("  resource.LocationStart := jsn.LocationStart;\r\n");
      prsrImpl.append("  resource.LocationEnd := jsn.LocationEnd;\r\n");
    }
      
    prsrImpl.append(workingParserJ.toString().replace("    ", "  ").replace("result.", "resource."));
    prsrImpl.append("end;\r\n\r\n");


    srlsdefX.append("    Procedure Compose"+root.getName()+"Attributes(xml : TXmlBuilder; resource : "+tn+");\r\n");
    prsrImpl.append("Procedure TFHIRXmlComposer.Compose"+root.getName()+"Attributes(xml : TXmlBuilder; resource : "+tn+");\r\n");
    prsrImpl.append("begin\r\n");
    if (!isBase)
      prsrImpl.append("  Compose"+root.typeCode()+"Attributes(xml, resource);\r\n");
    prsrImpl.append(workingComposerXA.toString());        
    prsrImpl.append("end;\r\n\r\n");
    srlsdefX.append("    Procedure Compose"+root.getName()+"Children(xml : TXmlBuilder; elem : "+tn+");\r\n");
    prsrImpl.append("Procedure TFHIRXmlComposer.Compose"+root.getName()+"Children(xml : TXmlBuilder; elem : "+tn+");\r\n");
    prsrImpl.append("var\r\n");
    prsrImpl.append("  i : integer;\r\n");
    prsrImpl.append("begin\r\n");
    if (!isBase)
      prsrImpl.append("  compose"+root.typeCode()+"Children(xml, elem);\r\n");
    prsrImpl.append(workingComposerX.toString());        
    prsrImpl.append("end;\r\n\r\n");

    srlsdefJ.append("    Procedure Compose"+root.getName()+"Properties(json : TJSONWriter; elem : "+tn+");\r\n");
    prsrImpl.append("Procedure TFHIRJsonComposer.Compose"+root.getName()+"Properties(json : TJSONWriter; elem : "+tn+");\r\n");
    prsrImpl.append("var\r\n");
    prsrImpl.append("  i : integer;\r\n");
    prsrImpl.append("begin\r\n");
    if (!isBase)
      prsrImpl.append("  Compose"+root.typeCode()+"Properties(json, elem);\r\n");
    prsrImpl.append(workingComposerJ.toString());        
    prsrImpl.append("end;\r\n\r\n");

    srlsdefR.append("    Procedure Compose"+root.getName()+"(this : TRDFComplex; parentType, name : String; elem : "+tn+"; index : integer); overload;\r\n");
    prsrImpl.append("Procedure TFHIRRDFComposer.Compose"+root.getName()+"(this : TRDFComplex; parentType, name : String; elem : "+tn+"; index : integer);\r\n");
    prsrImpl.append("var\r\n");
    prsrImpl.append("  i : integer;\r\n");
    prsrImpl.append("begin\r\n");
    if (!isBase)
      prsrImpl.append("  Compose"+root.typeCode()+"(this, '"+root.getName()+"', name, elem, index);\r\n");
    prsrImpl.append(workingComposerR.toString().replace("%%%%", root.getName()));        
    prsrImpl.append("end;\r\n\r\n");
  }
//
//  private void genResource(ResourceDefn root, String tn, String superClass, ClassCategory category) throws Exception {
//    prsrdefX.append("    function Parse"+root.getName()+"(element : IXmlDomElement; path : string) : TFhir"+root.getName()+";\r\n");
//    srlsdefX.append("    procedure Compose"+root.getName()+"(xml : TXmlBuilder; name : string; elem : TFhir"+root.getName()+");\r\n");
//    prsrdefJ.append("    function Parse"+root.getName()+"(jsn : TJsonObject) : TFhir"+root.getName()+"; overload; {b|}\r\n");
//    srlsdefJ.append("    procedure Compose"+root.getName()+"(json : TJSONWriter; name : string; elem : TFhir"+root.getName()+"; noObj : boolean = false);\r\n");
//    compJBase.append("  else if (base is "+tn+") then\r\n    compose"+root.getName()+"(json, name, "+tn+"(base), false)\r\n");
//    compXBase.append("  else if (base is "+tn+") then\r\n    compose"+root.getName()+"(xml, name,  "+tn+"(base))\r\n");
//
//    workingParserX = new StringBuilder();
//    workingParserXA = new StringBuilder();
//    workingComposerX = new StringBuilder();
//    workingComposerXA = new StringBuilder();
//    workingParserJ = new StringBuilder();
//    workingComposerJ = new StringBuilder();
//
//    generateSearchEnums(root);
//
//    StringBuilder def = new StringBuilder();
//    StringBuilder defPriv1 = new StringBuilder();
//    StringBuilder defPriv2 = new StringBuilder();
//    StringBuilder defPub = new StringBuilder();
//    StringBuilder impl = new StringBuilder();
//    StringBuilder create = new StringBuilder();
//    StringBuilder destroy = new StringBuilder();
//    StringBuilder assign = new StringBuilder();
//    StringBuilder getkids = new StringBuilder();
//    StringBuilder getkidsvars = new StringBuilder();
//    StringBuilder getprops = new StringBuilder();
//    StringBuilder getpropsvars = new StringBuilder();
//    StringBuilder setprops = new StringBuilder();
//    impl.append("{ "+tn+" }\r\n\r\n");
//
//    for (ElementDefn e : root.getRoot().getElements()) {
//      generateField(e, root.getRoot().getName(), defPriv1, defPriv2, defPub, impl, create, destroy, assign, getkids, getkidsvars, getprops, getpropsvars, setprops, tn, "", ClassCategory.Component, e.getName().equals("Extension"));
//    }
//
//    def.append("  {@Class "+tn+" : "+superClass+"\r\n");
//    def.append("    "+makeDocoSafe(root.getDefinition())+"\r\n");
//    def.append("  }\r\n");
//    def.append("  "+tn+" = class ("+superClass+")\r\n");
//    types.add(tn);
//    factoryIntf.append("    {@member new"+tn.substring(5)+"\r\n      create a new "+root.getName()+"\r\n    }\r\n    {!script nolink}\r\n    function new"+tn.substring(5)+" : "+tn+";\r\n");    
//    factoryImpl.append("function TFhirResourceFactory.new"+tn.substring(5)+" : "+tn+";\r\nbegin\r\n  result := "+tn+".create;\r\nend;\r\n\r\n");
//    factoryByName.append("  else if name = '"+tn.substring(5)+"' then\r\n    result := new"+tn.substring(5)+"()\r\n");
//    def.append("  private\r\n");
//    def.append(defPriv1.toString());
//    def.append(defPriv2.toString());
//    def.append("  protected\r\n");
//    def.append("    Procedure GetChildrenByName(child_name : string; list : "+listForm("TFHIRObject")+"); override;\r\n");
//    def.append("    Procedure ListProperties(oList : "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues : Boolean); Override;\r\n");
//    if (category == ClassCategory.Resource) {
//      def.append("    function GetResourceType : TFhirResourceType; override;\r\n");      
//    }
//    def.append("  public\r\n");
//    def.append("    constructor Create; Override;\r\n");
//    def.append("    destructor Destroy; override;\r\n");
//    def.append("    {!script hide}\r\n");
//    def.append("    procedure Assign(oSource : TAdvObject); override;\r\n");
//    def.append("    function Link : "+tn+"; overload;\r\n");
//    def.append("    function Clone : "+tn+"; overload;\r\n");
//    def.append("    procedure setProperty(propName : string; propValue : TFHIRObject); override;\r\n");
//    def.append("    function fhirType : string; override;\r\n");
//    def.append("    function equalsDeep(other : TFHIRBase) : boolean; override;\r\n");
//    def.append("    function equalsShallow(other : TFHIRBase) : boolean; override;\r\n");
//    def.append("    {!script show}\r\n");
//    def.append("  published\r\n");
//    def.append(defPub.toString());
//    def.append("  end;\r\n");
//    def.append("\r\n");
//    StringBuilder impl2 = new StringBuilder();
//    impl2.append("{ "+tn+" }\r\n\r\n");
//    impl2.append("constructor "+tn+".Create;\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  inherited;\r\n");
//    impl2.append(create.toString());
//    impl2.append("end;\r\n\r\n");
//
//    impl2.append("destructor "+tn+".Destroy;\r\n");
//    impl2.append("begin\r\n");
//    impl2.append(destroy.toString());
//    impl2.append("  inherited;\r\n");
//    impl2.append("end;\r\n\r\n");
//    if (category == ClassCategory.Resource) {
//      impl2.append("function "+tn+".GetResourceType : TFhirResourceType;\r\nbegin\r\n  result := frt"+root.getName()+";\r\nend;\r\n\r\n");       
//    }
//
//    impl2.append("procedure "+tn+".Assign(oSource : TAdvObject);\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  inherited;\r\n");
//    impl2.append(assign.toString());
//    impl2.append("end;\r\n\r\n");
//    impl2.append("procedure "+tn+".GetChildrenByName(child_name : string; list : "+listForm("TFHIRObject")+");\r\n");
//    if (getkidsvars.length() > 0) {
//      impl2.append("var\r\n");
//      impl2.append(getkidsvars.toString());
//    }
//    impl2.append("begin\r\n");
//    impl2.append("  inherited;\r\n");
//    impl2.append(getkids.toString());
//    impl2.append("end;\r\n\r\n");
//    impl2.append("procedure "+tn+".ListProperties(oList: "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues: Boolean);\r\n");
//    if (getpropsvars.length() > 0) {
//      impl2.append("var\r\n  prop : TFHIRProperty;\r\n");      
//      impl2.append(getpropsvars.toString());
//    }
//    impl2.append("begin\r\n");
//    impl2.append("  inherited;\r\n");
//    impl2.append(getprops.toString());
//    impl2.append("end;\r\n\r\n");
//    impl2.append("procedure "+tn+".setProperty(propName : string; propValue: TFHIRObject);\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  "+setprops.toString().substring(7));
//    impl2.append("  else inherited;\r\n");
//    impl2.append("end;\r\n\r\n");
//    impl2.append("function "+tn+".fhirType : string;\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  result := '"+root.getName()+"';\r\n");
//    impl2.append("end;\r\n\r\n");
//    generateEquals(root.getRoot(), tn, impl2);
//
//    impl2.append("function "+tn+".Link : "+tn+";\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  result := "+tn+"(inherited Link);\r\n");
//    impl2.append("end;\r\n\r\n");
//    impl2.append("function "+tn+".Clone : "+tn+";\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  result := "+tn+"(inherited Clone);\r\n");
//    impl2.append("end;\r\n\r\n");
//    getCode(category).classDefs.add(def.toString());
//    getCode(category).classImpls.add(impl2.toString() + impl.toString());
//    getCode(category).classFwds.add("  "+tn+" = class;\r\n");
//    generateParser(tn, ClassCategory.Resource, !superClass.equals("TFHIRObject"), root.getRoot().typeCode());
//  }

  private void generateIndexInformation(ResourceDefn r) throws Exception {
    StringBuilder b = new StringBuilder();
    indexHeaders.append("    procedure buildIndexesFor"+r.getName()+"(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);\r\n");
    indexBody.append("  buildIndexesFor"+r.getName()+"(Indexes, compartments);\r\n");
    b.append("procedure TFHIRIndexBuilder.buildIndexesFor"+r.getName()+"(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);\r\n");
    b.append("begin\r\n");
    List<String> names = new ArrayList<String>();
    Map<String, SearchParameterDefn> params = new HashMap<String, SearchParameterDefn>();
    names.addAll(r.getSearchParams().keySet());
    params.putAll(r.getSearchParams());
    String pn = r.getRoot().typeCode();
    while (!Utilities.noString(pn)) {
      ResourceDefn rd = definitions.getBaseResources().get(pn);
      names.addAll(rd.getSearchParams().keySet());
      params.putAll(rd.getSearchParams());
      pn = rd.getRoot().typeCode();
    }
    Collections.sort(names);
    for (String name : names) {
      SearchParameterDefn sp = params.get(name);
      b.append("  indexes.add('"+r.getName()+"', '"+sp.getCode()+"', '"+defCodeType.escape(sp.getDescription())+"', SearchParamType"+getTitle(sp.getType().toString())+", "+
          getTarget(sp.getWorkingTargets(), 800)+", '"+defCodeType.escape(sp.getExpression())+"', SearchXpathUsage"+Utilities.capitalize(sp.getxPathUsage().toCode())+");\r\n");
    }

    
    for (Compartment c : definitions.getCompartments()) {
      if (c.getResources().containsKey(r) && !c.getResources().get(r).isEmpty()) {
        b.append("  compartments.register(frt"+c.getName()+", '"+r.getName()+"', [");
        boolean first = true;
        for (String s : c.getResources().get(r)) {
          if (first)
            first = false;
          else
            b.append(", ");
          b.append("'"+s+"'");
        }
        b.append("]);\r\n");
      }
    }

    b.append("end;\r\n\r\n");
    indexMethods.append(b.toString());
  }
  
  
  private void generateSearchEnums(ResourceDefn r) throws Exception {
    StringBuilder def = new StringBuilder();
    StringBuilder con3 = new StringBuilder();

    String tn = "TSearchParams"+r.getName();
    String prefix = "sp"+r.getName()+"_";

    if (!enumsDone.contains(prefix)) {
      enumsDone.add(prefix);
      def.append("  {@Enum "+tn+"\r\n");
      def.append("    Search Parameters for "+r.getName()+"\r\n");
      def.append("  }\r\n");
      def.append("  "+tn+" = (\r\n");
      constants.add(tn);

      con3.append("  CODES_"+tn+" : Array["+tn+"] of String = (");
      int l4 = 0;
      int l2 = 0;
      int l7 = 0;

      List<String> names = new ArrayList<String>();
      Map<String, SearchParameterDefn> params = new HashMap<String, SearchParameterDefn>();
      names.addAll(r.getSearchParams().keySet());
      params.putAll(r.getSearchParams());
      String pn = r.getRoot().typeCode();
      while (!Utilities.noString(pn)) {
        ResourceDefn rd = definitions.getBaseResources().get(pn);
        names.addAll(rd.getSearchParams().keySet());
        params.putAll(rd.getSearchParams());
        pn = rd.getRoot().typeCode();
      }

      Collections.sort(names);
      int l = names.size();
      int i = 0;
      for (String name : names) {
        SearchParameterDefn p = params.get(name);
        i++;
        String n = p.getCode().replace("$", "_");
        String d = makeDocoSafe(p.getDescription());
        String nf = n.replace("-", "").replace("[x]", "x");
        String t = "\r\n      "+getTarget(p.getWorkingTargets(), 900);
//        if (900 - l7 < t.length()) {
//          l7 = 10;
//          t = "\r\n    "+t;
//        }
//        if (t.length() + l7 > 900) {
//          int c = 900 - l7;
//          while (t.charAt(c) != ' ')
//            c--;
//          t = t.substring(0, c)+"\r\n        "+t.substring(c);
//          l7 = t.length() - c;
//        }
          
        if (i == l) {
          def.append("    "+prefix+getTitle(nf)+"); {@enum.value \""+p.getCode()+"\" "+prefix+getTitle(nf)+" "+d+" }\r\n");
          con3.append("'"+defCodeType.escape(n)+"');");
        }
        else {
          def.append("    "+prefix+getTitle(nf)+", {@enum.value \""+p.getCode()+"\" "+prefix+getTitle(nf)+" "+d+" }\r\n");
          con3.append("'"+defCodeType.escape(n)+"', ");
        }
      }

      defCodeConstGen.enumDefs.add(def.toString());
      defCodeConstGen.enumConsts.add(con3.toString());
    }
  }

  private String breakString(String s) {
    if (s.length() > 250)
      return s.substring(0, 250)+"'+'"+s.substring(250);
    else
      return s;
  }

  private String getTarget(Set<String> targets, int l) throws Exception {
    if ((targets.size() == 1 && targets.contains("Any")) || (targets.size() == definitions.getResources().size()))
      return "ALL_RESOURCE_TYPE_NAMES";

    StringBuilder s = new StringBuilder();
    s.append("[");
    int i = 1;
    boolean first = true;
    for (String p : targets) {
      if (definitions.hasResource(p)) {
        if (!first)
          s.append(", ");
        s.append("'"+p+"'");
        first = false;
        i = i + p.length()+5;
        if (i > l) {
          s.append("\r\n      ");
          i = 6;
        }
      }
    }
    s.append("]");
    return s.toString();
  }

  private void generateEnum(ElementDefn e) throws Exception {
    String tn = typeNames.get(e);
    BindingSpecification cd = e.getBinding();
    List<DefinedCode> ac = cd.getAllCodes();
    
    if (ac.size() == 0)
      throw new Error("Element "+e.getName()+" codes have no codes on binding "+e.getBinding().getName());
    
    // this is a work around for an erroneous definition. It should be an error not patched here
    Set<String> codes = new HashSet<String>();
    List<DefinedCode> deletes = new ArrayList<DefinedCode>();
    for (DefinedCode dc : ac) {
      if (codes.contains(dc.getCode()))
        deletes.add(dc);
      else
        codes.add(dc.getCode());
    }
    ac.removeAll(deletes);
    codes.clear();
    
    enumSizes.put(tn, ac.size());


    String prefix = tn.substring(0, tn.length()-4).substring(5);
    if (!enumsDone.contains(prefix)) {
      enumsDone.add(prefix);
      StringBuilder def = new StringBuilder();
      StringBuilder con = new StringBuilder();
      StringBuilder conS = new StringBuilder();
      def.append("  {@Enum "+tn+"\r\n");
      def.append("    "+makeDocoSafe(cd.getDefinition())+" from "+e.getBinding().getUri()+"\r\n");
      def.append("  }\r\n");
      def.append("  "+tn+" = (\r\n");
      int cl = 0;
      int cls = 0;
      con.append("  CODES_"+tn+" : Array["+tn+"] of String = (");
      conS.append("  SYSTEMS_"+tn+" : Array["+tn+"] of String = (");
      constants.add(tn);

      int l = ac.size();
      int i = 0;
      def.append("    "+prefix+"Null,  {@enum.value "+prefix+"Null Value is missing from Instance }\r\n");
      con.append("'', ");
      conS.append("'', ");
      for (DefinedCode c : ac) {
        i++;
        String cc = c.getCode();
        if (cc.equals("-"))
          cc = "Minus";
        else if (cc.equals("+"))
          cc = "Plus";
        else {
          cc = cc.replace("-", " ").replace("+", " ");
          cc = Utilities.camelCase(cc);
          cc = cc.replace(">=", "greaterOrEquals").replace("<=", "lessOrEquals").replace("<", "lessThan").replace(">", "greaterThan").replace("=", "equal");
        }
        if (GeneratorUtils.isDelphiReservedWord(prefix + cc))
          cc = cc + "_";
        if (codes.contains(cc)) {
          int ci = 1;
          while (codes.contains(cc+Integer.toString(ci)))
            ci++;
          cc = cc+Integer.toString(ci);
        }
        codes.add(cc);
        
        cc = prefix + getTitle(cc);
        if ((con.length() - cl) + c.getCode().length() > 1010) {
          con.append("\r\n    ");
          cl = con.length();
        }
        if ((conS.length() - cls) + c.getSystem().length() > 1010) {
          conS.append("\r\n    ");
          cls = conS.length();
        }
        if (i == l) {
          def.append("    "+cc+"); {@enum.value "+cc+" "+makeDocoSafe(c.getDefinition())+" }\r\n");
          con.append("'"+c.getCode()+"');");
          conS.append("'"+c.getSystem()+"');");
        }
        else {
          def.append("    "+cc+", {@enum.value "+cc+" "+makeDocoSafe(c.getDefinition())+" }\r\n");
          con.append("'"+c.getCode()+"', ");
          conS.append("'"+c.getSystem()+"', ");
        }
      }
      def.append("  "+tn+"List = set of "+tn+";\r\n");
      defCodeType.enumDefs.add(def.toString());
      defCodeType.enumConsts.add(con.toString());
      defCodeType.enumConsts.add(conS.toString());
      defCodeType.enumProcs.add("Function "+tn+"ListAsInteger(aSet : "+tn+"List) : Integer; overload;");
      defCodeType.enumProcs.add("Function IntegerAs"+tn+"List(i : integer) : "+tn+"List; overload;");


      StringBuilder impl = new StringBuilder();

      impl.append("function "+tn+"ListAsInteger(aSet : "+tn+"List) : Integer;\r\n");
      impl.append("var\r\n");
      impl.append("  a : "+tn+";\r\n");
      impl.append("begin\r\n");
      impl.append("  result := 0;\r\n");
      impl.append("  for a := low("+tn+") to high("+tn+") do\r\n");
      impl.append("  begin\r\n");
      impl.append("    assert(ord(a) < 32);\r\n");
      impl.append("    if a in aSet then\r\n");
      impl.append("      result := result + 1 shl (ord(a));\r\n");
      impl.append("  end;\r\n");
      impl.append("end;\r\n\r\n");

      impl.append("function IntegerAs"+tn+"List(i : Integer) : "+tn+"List;\r\n");
      impl.append("var\r\n");
      impl.append("  aLoop : "+tn+";\r\n");
      impl.append("begin\r\n");
      impl.append("  result := [];\r\n");
      impl.append("  for aLoop := low("+tn+") to high("+tn+") Do\r\n");
      impl.append("  begin\r\n");
      impl.append("    assert(ord(aLoop) < 32);\r\n");
      impl.append("    if i and (1 shl (ord(aLoop))) > 0 Then\r\n");
      impl.append("      result := result + [aLoop];\r\n");
      impl.append("  end;\r\n");
      impl.append(" end;\r\n\r\n");

      defCodeType.classImpls.add(impl.toString());
    }
  }

  private String enumName(String substring) {
    if (substring.equalsIgnoreCase("type"))
      return "Type_";
    else
      return substring;
  }

  private void generateType(ElementDefn e, ClassCategory category, List<String> paths) throws Exception {
    String tn = typeNames.get(e);

    prsrdefX.append("    function Parse"+tn.substring(5)+"(element : IXmlDomElement; path : string) : "+tn+";\r\n");
    if (!e.getName().equals("Element") && !e.getName().equals("BackboneElement"))
      prsrdefX.append("    function Parse"+tn.substring(5)+"Child(element : TFhir"+tn.substring(5)+"; path : string; child : IXmlDomElement) : boolean;\r\n");
    srlsdefX.append("    procedure Compose"+tn.substring(5)+"(xml : TXmlBuilder; name : string; elem : "+tn+");\r\n");
    if (!e.getName().equals("Element") && !e.getName().equals("BackboneElement"))
      srlsdefX.append("    procedure Compose"+tn.substring(5)+"Children(xml : TXmlBuilder; elem : "+tn+");\r\n");
    prsrdefJ.append("    function Parse"+tn.substring(5)+"(jsn : TJsonObject) : "+tn+"; overload; {b\\}\r\n");
    if (!e.getName().equals("Element") && !e.getName().equals("BackboneElement"))
      prsrdefJ.append("    procedure Parse"+tn.substring(5)+"Properties(jsn : TJsonObject; result : "+tn+"); overload; {b\\}\r\n");
    srlsdefJ.append("    procedure Compose"+tn.substring(5)+"(json : TJSONWriter; name : string; elem : "+tn+"; noObj : boolean = false)"+(tn.equals("TFhirExtension") ? " override;" : "")+";\r\n");
    srlsdefR.append("    procedure Compose"+tn.substring(5)+"(parent :  TRDFComplex; parentType, name : String; elem : "+tn+"; index : integer)"+(tn.equals("TFhirExtension") ? " override;" : "")+";\r\n");
    compJBase.append("  else if (base is "+tn+") then\r\n    compose"+tn.substring(5)+"(json, name, "+tn+"(base), false)\r\n");
    compXBase.append("  else if (base is "+tn+") then\r\n    compose"+tn.substring(5)+"(xml, name,  "+tn+"(base))\r\n");
    compRBase.append("  else if (base is "+tn+") then\r\n    compose"+tn.substring(5)+"(section, name,  "+tn+"(base), -1)\r\n");

    workingParserX = new StringBuilder();
    workingParserXA = new StringBuilder();
    workingComposerX = new StringBuilder();
    workingComposerXA = new StringBuilder();
    workingParserJ = new StringBuilder();
    workingComposerJ = new StringBuilder();
    workingParserR = new StringBuilder();
    workingComposerR = new StringBuilder();

    StringBuilder def = new StringBuilder();
    StringBuilder defPriv1 = new StringBuilder();
    StringBuilder defPriv2 = new StringBuilder();
    StringBuilder defPub = new StringBuilder();
    StringBuilder impl = new StringBuilder();
    StringBuilder create = new StringBuilder();
    StringBuilder destroy = new StringBuilder();
    StringBuilder assign = new StringBuilder();
    StringBuilder getkids = new StringBuilder();
    StringBuilder getkidsvars = new StringBuilder();
    StringBuilder getprops = new StringBuilder();
    StringBuilder getpropsvars = new StringBuilder();
    StringBuilder setprops = new StringBuilder();
    StringBuilder makeprops = new StringBuilder();

    def.append("  {@Class "+tn+" : TFhirElement\r\n");
    def.append("    "+makeDocoSafe(e.getDefinition())+"\r\n");
    def.append("  }\r\n");
    if (category == ClassCategory.Component)
      def.append("  "+tn+" = class (TFhirBackboneElement)\r\n");
    else
      def.append("  "+tn+" = class (TFhirElement)\r\n");
    types.add(tn);
    factoryIntf.append("    {@member new"+tn.substring(5)+"\r\n      create a new "+e.getName()+"\r\n    }\r\n    {!script nolink}\r\n    function new"+tn.substring(5)+" : "+tn+";\r\n");    
    factoryImpl.append("function TFhirResourceFactory.new"+tn.substring(5)+" : "+tn+";\r\nbegin\r\n  result := "+tn+".create;\r\nend;\r\n\r\n");
    for (String p : paths)
      factoryByName.append("  else if name = '"+p+"' then\r\n    result := new"+tn.substring(5)+"()\r\n");
    impl.append("{ "+tn+" }\r\n\r\n");

    for (ElementDefn c : e.getElements()) {
      generateField(c, e.getPath(), defPriv1, defPriv2, defPub, impl, create, destroy, assign, getkids, getkidsvars, getprops, getpropsvars, setprops, makeprops, tn, "", category, e.getName().equals("Extension"));
    }

    def.append("  private\r\n");
    def.append(defPriv1.toString());
    def.append(defPriv2.toString());
    def.append("  protected\r\n");
    def.append("    Procedure GetChildrenByName(child_name : string; list : "+listForm("TFHIRObject")+"); override;\r\n");
    def.append("    Procedure ListProperties(oList : "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues : Boolean); Override;\r\n");
    def.append("  public\r\n");
    def.append("    constructor Create; Override;\r\n");
    def.append("    destructor Destroy; override;\r\n");
    def.append("    {!script hide}\r\n");
    def.append("    procedure Assign(oSource : TAdvObject); override;\r\n");
    def.append("    function Link : "+tn+"; overload;\r\n");
    def.append("    function Clone : "+tn+"; overload;\r\n");
    def.append("    procedure setProperty(propName : string; propValue : TFHIRObject); override;\r\n");
    def.append("    function makeProperty(propName : string) : TFHIRObject; override;\r\n");
    def.append("    function fhirType : string; override;\r\n");
    def.append("    function equalsDeep(other : TFHIRBase) : boolean; override;\r\n");
    def.append("    function equalsShallow(other : TFHIRBase) : boolean; override;\r\n");
    def.append("    {!script show}\r\n");
    def.append("  published\r\n");
    def.append(defPub.toString());
    def.append("  end;\r\n");
    def.append("\r\n");
    StringBuilder impl2 = new StringBuilder();
    impl2.append("{ "+tn+" }\r\n\r\n");
    impl2.append("constructor "+tn+".Create;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  inherited;\r\n");
    impl2.append(create.toString());
    impl2.append("end;\r\n\r\n");

    impl2.append("destructor "+tn+".Destroy;\r\n");
    impl2.append("begin\r\n");
    impl2.append(destroy.toString());
    impl2.append("  inherited;\r\n");
    impl2.append("end;\r\n\r\n");

    impl2.append("procedure "+tn+".Assign(oSource : TAdvObject);\r\n");
    impl2.append("begin\r\n");
    impl2.append("  inherited;\r\n");
    impl2.append(assign.toString());
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".GetChildrenByName(child_name : string; list : "+listForm("TFHIRObject")+");\r\n");
    if (getkidsvars.length() > 0) {
      impl2.append("var\r\n");
      impl2.append(getkidsvars.toString());
    }
    impl2.append("begin\r\n");
    impl2.append("  inherited;\r\n");
    impl2.append(getkids.toString());
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".ListProperties(oList: "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues: Boolean);\r\n");
    if (getpropsvars.length() > 0) {
      impl2.append("var\r\n  prop : TFHIRProperty;\r\n");      
      impl2.append(getpropsvars.toString());
    }
    impl2.append("begin\r\n");
    impl2.append("  inherited;\r\n");
    impl2.append(getprops.toString());
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".setProperty(propName : string; propValue: TFHIRObject);\r\n");
    impl2.append("begin\r\n");
    impl2.append("  "+setprops.toString().substring(7));
    impl2.append("  else inherited;\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function "+tn+".makeProperty(propName : string) : TFHIRObject;\r\n");
    impl2.append("begin\r\n");
    if (makeprops.length() > 7) {
      impl2.append("  "+makeprops.toString().substring(7));
      impl2.append("  else result := inherited makeProperty(propName);\r\n");
    } else
      impl2.append("  result := inherited makeProperty(propName);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function "+tn+".fhirType : string;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := '"+e.getName()+"';\r\n");
    impl2.append("end;\r\n\r\n");


    impl2.append("function "+tn+".Link : "+tn+";\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := "+tn+"(inherited Link);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function "+tn+".Clone : "+tn+";\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := "+tn+"(inherited Clone);\r\n");
    impl2.append("end;\r\n\r\n");
    generateEquals(e, tn, impl2);

    getCode(category).classDefs.add(def.toString());
    getCode(category).classImpls.add(impl2.toString() + impl.toString());
    getCode(category).classFwds.add("  "+tn+" = class;\r\n");
    generateParser(e, tn, category, true, e.typeCode());
    defineList(tn, tn+"List", null, category, category == ClassCategory.AbstractResource, false);
  }

  private void generateEquals(ElementDefn e, String tn, StringBuilder b) throws IOException {
    b.append("function "+tn+".equalsDeep(other : TFHIRBase) : boolean; \r\n");
    b.append("var\r\n");
    b.append("  o : "+tn+";\r\n");
    b.append("begin\r\n");
    b.append("  if (not inherited equalsDeep(other)) then\r\n");
    b.append("    result := false\r\n");
    b.append("  else if (not (other is "+tn+")) then\r\n");
    b.append("    result := false\r\n");
    b.append("  else\r\n");
    b.append("  begin\r\n");
    b.append("    o := "+tn+"(other);\r\n");
    b.append("    result := ");
    boolean first = true;
    int col = 18;
    for (ElementDefn c : e.getElements()) {
      if (first)
        first = false;
      else {
        b.append(" and ");
        col = col+5;
      }
      if (col > 80) {
        col = 6;
        b.append("\r\n      ");
      }
      String name = getElementName(c.getName());
      if (name.endsWith("[x]"))
        name = name.substring(0, name.length()-3);
      if (c.unbounded())
        name = name + "List";
      else
        name = name + "Element";
      b.append("compareDeep("+name+", o."+name+", true)");
      col = col+21 + name.length()*2;
    }
    if (first)
      b.append("true"); 
    b.append(";\r\n");
    b.append("  end;\r\n");
    b.append("end;\r\n\r\n");
    
    b.append("function "+tn+".equalsShallow(other : TFHIRBase) : boolean; \r\n");
    b.append("var\r\n");
    b.append("  o : "+tn+";\r\n");
    b.append("begin\r\n");
    b.append("  if (not inherited equalsShallow(other)) then\r\n");
    b.append("    result := false\r\n");
    b.append("  else if (not (other is "+tn+")) then\r\n");
    b.append("    result := false\r\n");
    b.append("  else\r\n");
    b.append("  begin\r\n");
    b.append("    o := "+tn+"(other);\r\n");
    b.append("    result := ");

    first = true;
    col = 18;
    for (ElementDefn c : e.getElements()) {
      if (isJavaPrimitive(c)) {
        if (first)
          first = false;
        else {
          b.append(" and ");
          col = col+5;
        }
        if (col > 80) {
          col = 6;
          b.append("\r\n      ");
        }
        String name = getElementName(c.getName());
        if (name.endsWith("[x]"))
          name = name.substring(0, name.length()-3);
        if (c.unbounded())
          name = name + "List";
        else
          name = name + "Element";
        b.append("compareValues("+name+", o."+name+", true)");
        col = col+21 + name.length()*2;
      }
    }
    if (first)
      b.append("true"); 
    b.append(";\r\n");
    b.append("  end;\r\n");
    b.append("end;\r\n\r\n");
  }
  
  private void generatePrimitiveEquals(DefinedCode t, String tn, StringBuilder b) throws IOException {
    b.append("function "+tn+".equalsDeep(other : TFHIRBase) : boolean; \r\n");
    b.append("var\r\n");
    b.append("  o : "+tn+";\r\n");
    b.append("begin\r\n");
    b.append("  if (not inherited equalsDeep(other)) then\r\n");
    b.append("    result := false\r\n");
    b.append("  else if (not (other is "+tn+")) then\r\n");
    b.append("    result := false\r\n");
    b.append("  else\r\n");
    b.append("  begin\r\n");
    b.append("    o := "+tn+"(other);\r\n");
    b.append("    result := o.value = value;\r\n");
    b.append("  end;\r\n");
    b.append("end;\r\n\r\n");
    
    b.append("function "+tn+".equalsShallow(other : TFHIRBase) : boolean; \r\n");
    b.append("var\r\n");
    b.append("  o : "+tn+";\r\n");
    b.append("begin\r\n");
    b.append("  if (not inherited equalsShallow(other)) then\r\n");
    b.append("    result := false\r\n");
    b.append("  else if (not (other is "+tn+")) then\r\n");
    b.append("    result := false\r\n");
    b.append("  else\r\n");
    b.append("  begin\r\n");
    b.append("    o := "+tn+"(other);\r\n");
    b.append("    result := o.value = value;\r\n");
    b.append("  end;\r\n");
    b.append("end;\r\n\r\n");
  }

  protected boolean isJavaPrimitive(ElementDefn e) {
    return e.getTypes().size() == 1 && (isPrimitive(e.typeCode()) || e.typeCode().equals("xml:lang"));
  }

  protected boolean isPrimitive(String name) {
    if (Utilities.noString(name))
      return false;
    if (name.equals("token"))
      return false;
    return definitions.hasPrimitiveType(name) || (name.endsWith("Type") && definitions.getPrimitives().containsKey(name.substring(0, name.length()-4)));
  }

  private void generateParser(ElementDefn e, String tn, ClassCategory category, boolean isElement, String parent) throws Exception {
    String s = workingParserX.toString().replace("result.", "element.");
    prsrImpl.append(
        "function TFHIRXmlParser.Parse"+tn.substring(5)+"(element : IXmlDomElement; path : string) : "+tn+";\r\n"+
            "var\r\n"+
        "  child : IXMLDOMElement;\r\n");

    prsrImpl.append(
        "begin\r\n"+
            "  result := "+tn+".create;\r\n"+
        "  try\r\n");
    if (isElement)
      if (category == ClassCategory.Resource)
        prsrImpl.append("    parse"+parent+"Attributes(result, path, element);\r\n");
      else
        prsrImpl.append("    parseElementAttributes(result, path, element);\r\n");

    prsrImpl.append(workingParserXA.toString());

    prsrImpl.append(
        "    child := FirstChild(element);\r\n"+
        "    while (child <> nil) do\r\n"+
        "    begin\r\n"+
        "      if not Parse"+tn.substring(5)+"Child(result, path, child) then\r\n"+
        "        UnknownContent(child, path);\r\n"+
        "      child := NextSibling(child);\r\n"+
        "    end;\r\n");
    if (isElement)
      prsrImpl.append(
          "    closeOutElement(result, element);\r\n");
    prsrImpl.append(
        "\r\n"+
            "    result.link;\r\n"+
            "  finally\r\n"+
            "    result.free;\r\n"+
            "  end;\r\n"+
            "end;\r\n\r\n"
        );
    if (!tn.equals("TFhirElement") && !tn.equals("TFhirBackboneElement")) {
      prsrImpl.append(
          "function TFHIRXmlParser.Parse"+tn.substring(5)+"Child(element : "+tn+"; path : string; child : IXmlDomElement) : boolean;\r\n"+
          "begin\r\n"+
          "  result := true;\r\n");

      if (s.length() >= 11)
        prsrImpl.append("      "+s.substring(11)+"      else ");
      else
        prsrImpl.append("      ");

      if (!isElement)
        prsrImpl.append("\r\n");
      else if (category == ClassCategory.Resource)
        prsrImpl.append("if Not Parse"+parent+"Child(element, path, child) then\r\n");
      else if (category == ClassCategory.Component)
        prsrImpl.append("if Not ParseBackboneElementChild(element, path, child) then\r\n");
      else if (hasExplicitParent(e))
        prsrImpl.append("if Not Parse"+e.typeCode()+"Child(element, path, child) then\r\n");
      else
        prsrImpl.append("if Not ParseElementChild(element, path, child) then\r\n");
      prsrImpl.append("    result := false;\r\n");
      prsrImpl.append("end;\r\n\r\n");
    }
    s = workingComposerX.toString();
    prsrImpl.append(
        "procedure TFHIRXmlComposer.Compose"+tn.substring(5)+"(xml : TXmlBuilder; name : String; elem : "+tn+");\r\n");
    prsrImpl.append(
        "begin\r\n"+
        "  if (elem = nil) then\r\n    exit;\r\n");
    if (isElement)
      if (category == ClassCategory.Resource)
        prsrImpl.append("  compose"+parent+"Attributes(xml, elem);\r\n");
      else 
        prsrImpl.append("  composeElementAttributes(xml, elem);\r\n");
    prsrImpl.append(workingComposerXA.toString());        
    prsrImpl.append(
        "  xml.open(name);\r\n");
    prsrImpl.append("  compose"+tn.substring(5)+"Children(xml, elem);\r\n");
    if (isElement)
      prsrImpl.append("  closeOutElement(xml, elem);\r\n");
    prsrImpl.append(
        "  xml.close(name);\r\n"+
            "end;\r\n\r\n"
        );

    if (!tn.equals("TFhirElement") && !tn.equals("TFhirBackboneElement")) {
      prsrImpl.append(
          "procedure TFHIRXmlComposer.Compose"+tn.substring(5)+"Children(xml : TXmlBuilder; elem : "+tn+");\r\n");
      boolean var = false;
      if (s.contains("for i := ")) {
        prsrImpl.append("var\r\n  i : integer;\r\n");
        var = true;
      }
      if (s.contains("ext := ")) {
        if (!var) 
          prsrImpl.append("var\r\n");
        prsrImpl.append("  ext : boolean;\r\n");
        prsrImpl.append("  val : boolean;\r\n");
      }
      prsrImpl.append(
          "begin\r\n");
      if (isElement)
        if (category == ClassCategory.Resource)
          prsrImpl.append("  compose"+parent+"Children(xml, elem);\r\n");
        else if (category == ClassCategory.Component)
          prsrImpl.append("  composeBackboneElementChildren(xml, elem);\r\n");
        else if (Utilities.noString(parent))
          prsrImpl.append("  composeElementChildren(xml, elem);\r\n");
        else
          prsrImpl.append("  compose"+parent+"Children(xml, elem);\r\n");

      prsrImpl.append(s);
      prsrImpl.append(
          "end;\r\n\r\n"
          );
    }
    
    prsrdefJ.append("    procedure Parse"+tn.substring(5)+"(jsn : TJsonObject; ctxt : "+listForm("TFHIRObject")+"); overload; {b.}\r\n");
    prsrImpl.append("procedure TFHIRJsonParser.Parse"+tn.substring(5)+"(jsn : TJsonObject; ctxt : "+listForm("TFHIRObject")+");\r\n");
    prsrImpl.append("begin\r\n");
    prsrImpl.append("  ctxt.add(Parse"+tn.substring(5)+"(jsn)); {2}\r\n");
    prsrImpl.append("end;\r\n\r\n");

    prsrImpl.append(
        "function TFHIRJsonParser.Parse"+tn.substring(5)+"(jsn : TJsonObject) : "+tn+";\r\n"+
            "begin\r\n"+
            "  result := "+tn+".create;\r\n"+
        "  try\r\n"+
        "    Parse"+tn.substring(5)+"Properties(jsn, result);\r\n"+ 
        "    result.link;\r\n"+
            "  finally\r\n"+
            "    result.free;\r\n"+
            "  end;\r\n"+
            "end;\r\n\r\n"
        );
    if (!tn.equals("TFhirElement") && !tn.equals("TFhirBackboneElement")) {
      prsrImpl.append(
      "procedure TFHIRJsonParser.Parse"+tn.substring(5)+"Properties(jsn : TJsonObject; result : "+tn+");\r\n"+
          "begin\r\n");
      s = workingParserJ.toString();
      if (isElement) {
        if (category == ClassCategory.Resource)
          prsrImpl.append("    Parse"+parent+"Properties(jsn, result);\r\n");
        else if (category == ClassCategory.Component)
          prsrImpl.append("    ParseBackboneElementProperties(jsn, result);\r\n");
        else if (Utilities.noString(parent))
          prsrImpl.append("    ParseElementProperties(jsn, result);\r\n");
        else
          prsrImpl.append("    Parse"+parent+"Properties(jsn, result);\r\n");
      }
      prsrImpl.append(s);
      prsrImpl.append(
      "end;\r\n\r\n");
    }
    
    s = workingComposerJ.toString();
    prsrImpl.append(
        "procedure TFHIRJsonComposer.Compose"+tn.substring(5)+"(json : TJSONWriter; name : string; elem : "+tn+"; noObj : boolean = false);\r\n");
    boolean var = false;
    if (s.contains("for i := ")) { // || category == ClassCategory.Resource) {
      prsrImpl.append("var\r\n  i : integer;\r\n");
      var = true;
    }
    if (s.contains("ext := ")) {
      if (!var) 
        prsrImpl.append("var\r\n");
      prsrImpl.append("  ext : boolean;\r\n");
      prsrImpl.append("  val : boolean;\r\n");
    }
    
    if (category == ClassCategory.Resource)
      prsrImpl.append(
          "begin\r\n"+
          "  if (elem = nil) then\r\n    exit;\r\n");
    else 
      prsrImpl.append(
          "begin\r\n"+
              "  if (elem = nil) then\r\n    exit;\r\n"+
          "  if not noObj then json.valueObject(name);\r\n");
    if (isElement)
      if (category == ClassCategory.Resource)
        prsrImpl.append("  Compose"+parent+"Properties(json, elem);\r\n");
      else if (category == ClassCategory.Component)
        prsrImpl.append("  ComposeBackboneElementProperties(json, elem);\r\n");
      else if (!Utilities.noString(parent))
        prsrImpl.append("  Compose"+parent+"(json, '', elem, true);\r\n");
      else if (!tn.substring(5).equals("Element"))
        prsrImpl.append("  ComposeElementProperties(json, elem);\r\n");
    
    
    prsrImpl.append(s);
    if (category == ClassCategory.Resource)
      prsrImpl.append(
          "end;\r\n\r\n");
    else
      prsrImpl.append(
          "  if not noObj then json.finishObject;\r\n"+
          "end;\r\n\r\n");

    s = workingComposerR.toString().replace("%%%%", tn.substring(5));
    // special cases in RDF
    if (tn.substring(5).equals("Quantity")) {
      s = s + "  // special case. system and code are simplified Coding, so we use the generic Coding approach\r\n"+
"  if (SummaryOption in [soFull, soSummary, soText, soData]) and ((elem.systemElement <> nil) or (elem.codeElement <> nil)) then\r\n"+
"  begin\r\n"+
"    cb := this.predicate('fhir:Quantity.code');\r\n"+
"    cb.predicate('a', 'fhir:ConceptBase');\r\n"+
"    c := cb.predicate('fhir:ConceptBase.coding');\r\n"+
"    ComposeUri(c, 'CodingBase', 'system', elem.systemElement, -1);\r\n"+
"    ComposeCode(c, 'CodingBase', 'code', elem.codeElement, -1);\r\n"+
"    if elem.unit_Element <> nil then\r\n"+
"      ComposeString(cb, 'ConceptBase', 'text', elem.unit_Element, -1);\r\n"+
"  end;\r\n";
    }
    if (tn.substring(5).equals("CodeableConcept")) {
      s = s.replace("CodeableConcept", "ConceptBase").replace("ComposeCoding(false, ", "ComposeCoding(true, ");
    }
    if (tn.substring(5).equals("Coding")) {
      s = s.replace("Coding", "CodingBase");
    }
    
    if (tn.substring(5).equals("Coding"))
      prsrImpl.append("procedure TFHIRRDFComposer.Compose"+tn.substring(5)+"(inCodeable : boolean; parent :  TRDFComplex; parentType, name : String; elem : "+tn+"; index : integer);\r\n");
    else
      prsrImpl.append("procedure TFHIRRDFComposer.Compose"+tn.substring(5)+"(parent :  TRDFComplex; parentType, name : String; elem : "+tn+"; index : integer);\r\n");
    prsrImpl.append("var\r\n  this : TRDFComplex;\r\n");
    if (tn.substring(5).equals("Quantity")) 
      prsrImpl.append("var\r\n  cb, c : TRDFComplex;\r\n");
    if (s.contains("for i := "))
      prsrImpl.append("  i : integer;\r\n");
    if (s.contains("ext := ")) {
      prsrImpl.append("  ext : boolean;\r\n");
      prsrImpl.append("  val : boolean;\r\n");
    }
    prsrImpl.append(
        "begin\r\n"+
        "  if (elem = nil) then\r\n    exit;\r\n");
    if (tn.substring(5).equals("Element"))
      prsrImpl.append("    this := parent;\r\n");
    else 
      prsrImpl.append(
        "  if (parentType = '') then\r\n"+    
        "    this := parent\r\n"+    
        "  else\r\n"+    
        "  begin\r\n"+    
        "    this := parent.predicate('fhir:'+parentType+'.'+name);\r\n"+
        (tn.substring(5).equals("Coding") ? "    if inCodeable then\r\n      this.predicate('a', 'fhir:CodingBase')\r\n    else\r\n      this.predicate('a', 'fhir:Coding');\r\n" : "    this.predicate('a', 'fhir:"+tn.substring(5)+"');\r\n")+
        (tn.substring(5).equals("CodeableConcept") ? "    this.predicate('a', 'fhir:ConceptBase');\r\n" : "")+
        "  end;\r\n");
    if (tn.substring(5).equals("Coding")) {
      prsrImpl.append("  if not inCodeable then\r\n"+
          "  begin\r\n"+
          "    this.predicate('a', 'fhir:ConceptBase');\r\n"+
          "    this := this.predicate('fhir:ConceptBase.coding');\r\n"+
          "    this.predicate('a', 'fhir:CodingBase');\r\n"+
          "  end;\r\n");
    }
    if (category == ClassCategory.Resource)
      prsrImpl.append("  compose"+parent+"(this, '"+tn.substring(5)+"', name, elem, index);\r\n");
    else if (category == ClassCategory.Component)
      prsrImpl.append("  composeBackboneElement(this, '"+tn.substring(5)+"', name, elem, index);\r\n");
    else if (tn.substring(5).equals("Element"))
      prsrImpl.append("  if (index > -1)  then\r\n    this.predicate('fhir:index', inttostr(index));\r\n");
    else if (tn.substring(5).equals("CodeableConcept"))
      prsrImpl.append("  composeElement(this, 'ConceptBase', name, elem, index);\r\n");
    else if (tn.substring(5).equals("Coding"))
      prsrImpl.append("  composeElement(this, 'CodingBase', name, elem, index);\r\n");
    else
      prsrImpl.append("  composeElement(this, '"+tn.substring(5)+"', name, elem, index);\r\n");

    prsrImpl.append(s);
    prsrImpl.append(
        "end;\r\n\r\n"
        );
  }

  private boolean hasExplicitParent(ElementDefn e) {
    boolean res = !(Utilities.noString(e.typeCode()) || e.typeCode().equals("Type") || e.typeCode().equals("Structure") || e.typeCode().equals("Element"));
    return res;
  }

  private void scanNestedTypes(ElementDefn root, String path, ElementDefn e, String literalPath, Map<ElementDefn, List<String>> paths) throws Exception {
    literalPath = literalPath +"."+e.getName();
    
    String tn = null;
    if (e.typeCode().equals("code") && e.hasBinding() && !e.getBinding().getName().equals("FHIRDefinedType")) {
      BindingSpecification cd = e.getBinding();
      if (cd != null && (cd.getBinding() == BindingSpecification.BindingMethod.CodeList)) {
        tn = "TFhir"+enumName(getTitle(getCodeList(cd.getReference()).substring(1)))+"Enum";
        if (!enumNames.contains(tn)) {
          enumNames.add(tn);
          enums.add(e);
        }
        typeNames.put(e,  tn);
      }
      if (cd != null && (cd.getBinding() == BindingSpecification.BindingMethod.ValueSet) && !e.getBinding().getName().equals("FHIRDefinedType")) {
        tn = "TFhir"+enumName(getTitle(getCodeList(urlTail(cd.getReference()))))+"Enum";
        if (!enumNames.contains(tn)) {
          enumNames.add(tn);
          enums.add(e);
        }
        typeNames.put(e,  tn);
      }
    }
    if (!paths.containsKey(e))
      paths.put(e, new ArrayList<String>());
    paths.get(e).add(literalPath);
    if (tn == null) {
      if (e.usesCompositeType()) {
        tn = typeNames.get(getElementForPath(root, e.typeCode().substring(1)));
        typeNames.put(e,  tn);
      } else if (e.getTypes().size() > 0) {
        tn = getTypeName(e);
        typeNames.put(e,  tn);
      } else 
      {
        tn = "TFhir"+path+getTitle(e.getName());
        if (tn.equals("TFhirPractitionerRole") && !Utilities.noString(path))
          tn = "TFhirPractitionerRoleComp";
        strucs.add(e);
        typeNames.put(e,  tn);
        for (ElementDefn c : e.getElements()) {
          scanNestedTypes(root, path+getTitle(e.getName()), c, literalPath, paths);
        }
      }
    }
  }

  private String urlTail(String reference) {
    return reference.substring(reference.lastIndexOf('/')+1);
  }

  private Object getElementForPath(ElementDefn root, String pathname) throws Exception {
    String[] path = pathname.split("\\.");
    if (!path[0].equals(root.getName()))
      throw new Exception("Element Path '"+pathname+"' is not legal in this context");
    ElementDefn res = root;
    for (int i = 1; i < path.length; i++)
    {
      String en = path[i];
      if (en.length() == 0)
        throw new Exception("Improper path "+pathname);
      ElementDefn t = res.getElementByName(en);
      if (t == null) {
        throw new Exception("unable to resolve "+pathname);
      }
      res = t; 
    }
    return res;

  }

  private String getCodeList(String binding) {
    StringBuilder b = new StringBuilder();
    boolean up = true;
    for (char ch: binding.toCharArray()) {
      if (ch == '-')
        up = true;
      else if (up) {
        b.append(Character.toUpperCase(ch));
        up = false;
      }
      else        
        b.append(ch);
    }
    return b.toString();
  }

  private void generateField(ElementDefn e, String path, StringBuilder defPriv1, StringBuilder defPriv2, StringBuilder defPub, StringBuilder impl, StringBuilder create, StringBuilder destroy, StringBuilder assign, StringBuilder getkids, StringBuilder getkidsvars, StringBuilder getprops, StringBuilder getpropsvars, StringBuilder setprops, StringBuilder makeprops, String cn, String pt, ClassCategory category, boolean isExtension) throws Exception {
    
    String tn;
    if (e.getTypes().size() > 0 && e.getTypes().get(0).isUnboundGenericParam())
      tn = pt;
    else
      tn = typeNames.get(e);
    if (tn == null) {
      if (e.getName().equals("extension"))
        tn = "TFhirExtension";
      else
        tn = getTypeName(e);
    }


    String parse = null;
    String propV = "F"+getTitle(getElementName(e.getName()));
    if (typeIsSimple(tn)) {
      if (enumNames.contains(tn)) {        
        parse = "ParseEnum(CODES_"+tn+", SYSTEMS_"+tn+", path+'/"+e.getName()+"', child)";
      } else if (tn.equals("Integer") || tn.equals("UnsignedInt")  || tn.equals("PositiveInt")) {
        parse = "StringToInteger32(child.text)";
        propV = "inttostr("+propV+ ")";
      } else if (tn.equals("Boolean")) {
        parse = "StringToBoolean(child.text)";
        propV = "LCBooleanToString("+propV+ ")";
      } else if (tn.equals("TDateAndTime")) {
        parse = "TDateAndTime.createXml(child.text)";
        propV = propV+".AsXML";
      } else if (tn.equals("TFhirXHtmlNode"))
        parse = "ParseXhtml(child)";
      else
        parse = "child.text";
    } else if (tn.equals("TSmartDecimal")) 
      propV = propV+".asString";
    String parseJ1 = null;
    if (enumNames.contains(tn)) {
      parseJ1 = "ParseEnumValue(SYSTEMS_"+tn+"', CODES_"+tn+"')";
    } else if (tn.equals("Integer") || tn.equals("UnsignedInt")  || tn.equals("PositiveInt")) {
      parseJ1 = "ParseIntegerValue(path+'."+e.getName()+"')";
    } else if (tn.equals("Boolean") || tn.equals("TFhirBoolean")) {
      parseJ1 = "ParseBooleanValue(path+'."+e.getName()+"')";
    } else if (tn.equals("TDateAndTime") || tn.equals("TFhirDateTime") || tn.equals("TFhirDate") || tn.equals("TFhirInstant")) {
      parseJ1 = "ParseDateAndTimeValue(path+'."+e.getName()+"')";
    } else if (tn.equals("TFhirXHtmlNode")) {
      parseJ1 = "ParseXhtml(path+'."+e.getName()+"')";
    } else {
      parseJ1 = "ParseStringValue(path+'."+e.getName()+"')";
    }
    String srlsd = "Text";
    String srlsdJ = "Prop";
    String srls = "#";
    String srlsr = "Text";
    if (typeIsSimple(tn)) {
      if (enumNames.contains(tn)) {
        srls = "CODES_"+tn+"[#]";
      } else if (tn.equals("Integer") || tn.equals("UnsignedInt")  || tn.equals("PositiveInt")) {
        srls = "IntegerToString(#)";
      } else if (tn.equals("Boolean")) {
        srls = "LCBooleanToString(#)";
      } else if (tn.equals("TDateAndTime")) {
        srls = "#.AsXml";
      };
    }

    String s = getElementName(e.getName());
    String sumSet = "";
    if (s.equals("id") || path == null || path.startsWith("Bundle") || path.startsWith("Parameters") || definitions.hasBaseType(path) )
      sumSet = "soFull, soSummary, soText, soData";
    else if ((s.equals("text") && path.equals("DomainResource")) || (path.equals("Narrative")) )
      sumSet = "soFull, soText";
    else if (e.isSummary())
      sumSet = "soFull, soSummary, soData";
    else
      sumSet = "soFull, soData";
      
//    boolean summary = e.isSummary() || noSummaries;
//    String sumAnd = summary ? "" : "Not SummaryOnly and ";
//    String sum2 = summary ? "" : "if not SummaryOnly then\r\n    ";
    if (e.unbounded()) {
      if (enumNames.contains(tn)) {         
        defPriv1.append("    F"+getTitle(s)+" : "+listForm("TFhirEnum")+";\r\n");
        defPriv2.append("    function Get"+Utilities.capitalize(s)+" : "+listForm("TFhirEnum")+";\r\n");
        defPriv2.append("    function GetHas"+Utilities.capitalize(s)+" : Boolean;\r\n");
        if (enumSizes.get(tn) < 32) {
          defPriv2.append("    Function Get"+getTitle(s)+"ST : "+listForm(tn)+";\r\n");
          defPriv2.append("    Procedure Set"+getTitle(s)+"ST(value : "+listForm(tn)+");\r\n");
        }
        assign.append("  if ("+cn+"(oSource).F"+getTitle(s)+" = nil) then\r\n");
        assign.append("  begin\r\n");
        assign.append("    F"+getTitle(s)+".free;\r\n");
        assign.append("    F"+getTitle(s)+" := nil;\r\n");
        assign.append("  end\r\n");
        assign.append("  else\r\n");
        assign.append("  begin\r\n");
        assign.append("    F"+getTitle(s)+" := "+listForm("TFHIREnum")+".Create(SYSTEMS_"+tn+", CODES_"+tn+");\r\n");
        if (enumSizes.get(tn) < 32) {
          defPub.append("    {@member "+s+"\r\n");
          defPub.append("      "+makeDocoSafe(e.getDefinition())+"\r\n");
          defPub.append("    }\r\n");
          defPub.append("    property "+s+" : "+listForm(tn)+" read Get"+getTitle(s)+"ST write Set"+getTitle(s)+"ST;\r\n");
          defPub.append("    property "+s+"List : "+listForm("TFhirEnum")+" read Get"+getTitle(s)+";\r\n");
          assign.append("    F"+getTitle(s)+".Assign("+cn+"(oSource).F"+getTitle(s)+");\r\n");
        } else {
          defPub.append("    property "+s+" : "+listForm("TFhirEnum")+" read Get"+getTitle(s)+";\r\n");
          defPub.append("    property "+s+"List : "+listForm("TFhirEnum")+" read Get"+getTitle(s)+";\r\n");
          assign.append("    F"+getTitle(s)+".Assign("+cn+"(oSource).F"+getTitle(s)+");\r\n");
        }
        assign.append("  end;\r\n");
        defPub.append("    property has"+Utilities.capitalize(s)+" : boolean read GetHas"+getTitle(s)+";\r\n");
        // no, do it lazy create.append("  F"+getTitle(s)+" := "+listForm("TFHIREnum")+".Create;\r\n");
        impl.append("Function "+cn+".Get"+getTitle(s)+" : "+listForm("TFhirEnum")+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    F"+getTitle(s)+" := "+listForm("TFHIREnum")+".Create(SYSTEMS_"+tn+", CODES_"+tn+");\r\n  result := F"+getTitle(s)+";\r\nend;\r\n\r\n");
        impl.append("Function "+cn+".GetHas"+getTitle(s)+" : boolean;\r\nbegin\r\n  result := (F"+getTitle(s)+" <> nil) and (F"+getTitle(s)+".count > 0);\r\nend;\r\n\r\n");
        destroy.append("  F"+getTitle(s)+".Free;\r\n");
        if (generics)
          getkidsvars.append("  o : TFHIREnum;\r\n");
        if (e.getName().endsWith("[x]") || e.getName().equals("[type]")) 
          if (generics)
            getkids.append("  if StringStartsWith(child_name, '"+getPropertyName(e.getName())+"') Then\r\n    for o in F"+getTitle(s)+" do\r\n       list.add(o.Link);\r\n");
          else
            getkids.append("  if StringStartsWith(child_name, '"+getPropertyName(e.getName())+"') Then\r\n    list.addAll(F"+getTitle(s)+");\r\n");
        else
          if (generics)
            getkids.append("  if (child_name = '"+e.getName()+"') Then\r\n     for o in F"+getTitle(s)+" do\r\n       list.add(o.Link);\r\n");
          else
            getkids.append("  if (child_name = '"+e.getName()+"') Then\r\n     list.addAll(F"+getTitle(s)+");\r\n");
        if (generics) {
          getpropsvars.append("  o : TFHIREnum;\r\n");
          getprops.append("  prop := oList[oList.add(TFHIRProperty.create(self, '"+e.getName()+"', '"+breakConstant(e.typeCode())+"'))];\r\n  for o in F"+getTitle(s)+" do\r\n      prop,list.add(o.Link){3};\r\n");
        } else
          getprops.append("  oList.add(TFHIRProperty.create(self, '"+e.getName()+"', '"+breakConstant(e.typeCode())+"', true, TFHIREnum, F"+getTitle(s)+".Link)){3};\r\n");
        if (e.getName().endsWith("[x]"))
          throw new Exception("Not done yet");
        setprops.append("  else if (propName = '"+e.getName()+"') then F"+getTitle(s)+".add(asEnum(SYSTEMS_"+tn+", CODES_"+tn+", propValue)) {1}\r\n");
        String obj = "";
        if (enumSizes.get(tn) < 32) {
          impl.append("Function "+cn+".Get"+getTitle(s)+"ST : "+listForm(tn)+";\r\n  var i : integer;\r\nbegin\r\n  result := [];\r\n  if F"+s+" <> nil then\r\n    for i := 0 to F"+s+".count - 1 do\r\n      result := result + ["+tn+"(StringArrayIndexOfSensitive(CODES_"+tn+", F"+s+"[i].value))];\r\nend;\r\n\r\n");
          impl.append("Procedure "+cn+".Set"+getTitle(s)+"ST(value : "+listForm(tn)+");\r\nvar a : "+tn+";\r\nbegin\r\n  if F"+s+" = nil then\r\n    F"+s+" := TFhirEnumList.create(SYSTEMS_"+tn+", CODES_"+tn+");\r\n  F"+s+".clear;\r\n  for a := low("+tn+") to high("+tn+") do\r\n    if a in value then\r\n      begin\r\n         if F"+s+" = nil then\r\n           F"+s+" := TFhirEnumList.create(SYSTEMS_"+tn+", CODES_"+tn+");\r\n         F"+s+".add(TFhirEnum.create(SYSTEMS_"+tn+"[a], CODES_"+tn+"[a]));\r\n      end;\r\nend;\r\n\r\n");
          obj = "List";
        }

        workingParserX.append("      else if (child.baseName = '"+e.getName()+"') then\r\n"+
            "        result."+s+obj+".Add("+parse+"){y.1}\r\n");
        workingComposerX.append("  if SummaryOption in ["+sumSet+"] then\r\n    for i := 0 to elem."+s+obj+".Count - 1 do\r\n"+
              "      ComposeEnum(xml, '"+e.getName()+"', elem."+s+obj+"[i], CODES_"+tn+");\r\n");

        if (!specialCaseInRDF(cn, e.getName()))
          workingComposerR.append("  if SummaryOption in ["+sumSet+"] then\r\n    for i := 0 to elem."+s+obj+".Count - 1 do\r\n"+
              "      ComposeEnum(this, '%%%%', '"+e.getName()+"', elem."+s+obj+"[i], CODES_"+tn+", SYSTEMS_"+tn+", i);\r\n");

        workingParserJ.append(
            "    if jsn.has('"+e.getName()+"') or jsn.has('_"+e.getName()+"') then\r\n"+
                "      iterateEnumArray(jsn.vArr['"+e.getName()+"'], jsn.vArr['_"+e.getName()+"'], jsn.path+'/"+e.getName()+"', result."+s+obj+", parseEnum, CODES_"+tn+", SYSTEMS_"+tn+");\r\n");

        workingComposerJ.append("  if (SummaryOption in ["+sumSet+"]) and (elem."+s+obj+".Count > 0) then\r\n");
        workingComposerJ.append(
            "  begin\r\n"+
                "    val := false;\r\n"+    
                "    ext := false;\r\n"+    
                "    for i := 0 to elem."+s+obj+".Count - 1 do\r\n"+
                "    begin\r\n"+
                "      val := val or (elem."+s+obj+"[i].hasPrimitiveValue);\r\n"+
                "      ext := ext or ((elem."+s+obj+"[i].id <> '') or (elem."+s+obj+"[i].hasExtensionList));\r\n"+
                "    end;\r\n"+
                "    if val then\r\n"+
                "    begin\r\n"+
                "      json.valueArray('"+e.getName()+"');\r\n"+
                "      for i := 0 to elem."+s+obj+".Count - 1 do\r\n"+
                "        ComposeEnumValue(json, '', elem."+s+obj+"[i], CODES_"+tn+", true);\r\n"+
                "      json.FinishArray;\r\n"+
                "    end;\r\n"+
                "    if ext then\r\n"+
                "    begin\r\n"+
                "      json.valueArray('_"+e.getName()+"');\r\n"+
                "      for i := 0 to elem."+s+obj+".Count - 1 do\r\n"+
                "        ComposeEnumProps(json, '', elem."+s+obj+"[i], CODES_"+tn+", true);\r\n"+
                "      json.FinishArray;\r\n"+
                "    end;\r\n"+
            "  end;\r\n");

        if (!specialCaseInRDF(cn, e.getName()))
        workingComposerR.append("  if SummaryOption in ["+sumSet+"] then\r\n    for i := 0 to elem."+s+obj+".Count - 1 do\r\n"+
            "      ComposeEnum(this, '%%%%', '"+e.getName()+"', elem."+s+obj+"[i], CODES_"+tn+", SYSTEMS_"+tn+", i);\r\n");
      } else {
        String tnl;
        if (tn.contains("{"))
          tnl = listForm(tn.substring(0, tn.indexOf('{')))+tn.substring(tn.indexOf('{'));
        else
          tnl = listForm(tn);
        s = s+"List";
        defPriv1.append("    F"+s+" : "+tnl+";\r\n");
        defPub.append("    {@member "+s+"\r\n");
        defPub.append("      "+makeDocoSafe(e.getDefinition())+"\r\n");
        defPub.append("    }\r\n");
        defPriv2.append("    function Get"+Utilities.capitalize(s)+" : "+tnl+";\r\n");
        defPriv2.append("    function GetHas"+Utilities.capitalize(s)+" : Boolean;\r\n");
        defPub.append("    property "+s+" : "+tnl+" read Get"+getTitle(s)+";\r\n");
        defPub.append("    property has"+Utilities.capitalize(s)+" : boolean read GetHas"+getTitle(s)+";\r\n");
        defPub.append("\r\n");
        impl.append("Function "+cn+".Get"+getTitle(s)+" : "+tnl+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    F"+getTitle(s)+" := "+tnl+".Create;\r\n  result := F"+getTitle(s)+";\r\nend;\r\n\r\n");
        impl.append("Function "+cn+".GetHas"+getTitle(s)+" : boolean;\r\nbegin\r\n  result := (F"+getTitle(s)+" <> nil) and (F"+getTitle(s)+".count > 0);\r\nend;\r\n\r\n");
        // create.append("  F"+getTitle(s)+z" := "+tnl+".Create;\r\n");
        destroy.append("  F"+getTitle(s)+".Free;\r\n");
        assign.append("  if ("+cn+"(oSource).F"+getTitle(s)+" = nil) then\r\n");
        assign.append("  begin\r\n");
        assign.append("    F"+getTitle(s)+".free;\r\n");
        assign.append("    F"+getTitle(s)+" := nil;\r\n");
        assign.append("  end\r\n");
        assign.append("  else\r\n");
        assign.append("  begin\r\n");
        assign.append("    F"+getTitle(s)+" := "+tnl+".Create;\r\n");
        assign.append("    F"+getTitle(s)+".Assign("+cn+"(oSource).F"+getTitle(s)+");\r\n");
        assign.append("  end;\r\n");
        if (generics) {
          getkidsvars.append("  o"+getTitle(s)+" : "+tn+";\r\n");
          getkids.append("  if (child_name = '"+e.getName()+"') Then\r\n    for o"+getTitle(s)+" in F"+getTitle(s)+" do\r\n      list.add(o"+getTitle(s)+");\r\n");
        } else
          getkids.append("  if (child_name = '"+e.getName()+"') Then\r\n    list.addAll(F"+getTitle(s)+");\r\n");
        if (generics) {
          getpropsvars.append("  o"+getTitle(s)+" : "+tn+";\r\n");
          getprops.append("  prop := oList[oList.add(TFHIRProperty.create(self, '"+e.getName()+"', '"+breakConstant(e.typeCode())+"'))];\r\n  for o"+getTitle(s)+" in F"+getTitle(s)+" do\r\n    prop.List.add(o"+getTitle(s)+".Link){3a};\r\n");
        } else
          getprops.append("  oList.add(TFHIRProperty.create(self, '"+e.getName()+"', '"+breakConstant(e.typeCode())+"', true, "+tn+", F"+getTitle(s)+".Link)){3};\r\n");
        if (e.getName().endsWith("[x]"))
          throw new Exception("Not done yet");
        if (typeIsPrimitive(e.typeCode()))
          setprops.append("  else if (propName = '"+e.getName()+"') then "+getTitle(s)+".add(as"+tn.substring(5)+"(propValue)){2}\r\n");
        else
          setprops.append("  else if (propName = '"+e.getName()+"') then "+getTitle(s)+".add(propValue as "+tn+"){2}\r\n");
        if (!e.typeCode().equals("Resource"))
          makeprops.append("  else if (propName = '"+e.getName()+"') then result := "+getTitle(s)+".append(){2}\r\n");

        if (!typeIsSimple(tn)) {
          if (!e.getName().equals("[type]") && !e.getName().contains("[x]")) {
            parse = "Parse"+parseName(tn)+"(child, path+'/"+e.getName()+"')";
            if (!typeIsPrimitive(e.typeCode()))
              parseJ1 = "Parse"+parseName(tn)+"(path+'."+e.getName()+"')";
            srlsd = "Compose"+parseName(tn);
            srlsdJ = "Compose"+parseName(tn);
            srlsr = "Compose"+parseNameR(tn);
          } else {
            throw new Exception("not supported at "+e.getName()+" - complex type "+tn);
          }
        };
        workingParserX.append("      else if (child.baseName = '"+e.getName()+"') then\r\n"+
            "        result."+s+".Add("+parse+"){y.2}\r\n");
        workingComposerX.append("  if SummaryOption in ["+sumSet+"] then\r\n    for i := 0 to elem."+s+".Count - 1 do\r\n"+
              "      "+srlsd+"(xml, '"+e.getName()+"', "+getParam3(tn)+srls.replace("#", "elem."+s+"[i]")+");\r\n");
        if (!specialCaseInRDF(cn, e.getName()))
        workingComposerR.append("  if SummaryOption in ["+sumSet+"] then\r\n    for i := 0 to elem."+s+".Count - 1 do\r\n"+
            "      "+srlsr+"("+(srlsr.equals("ComposeCoding") ? "false, " : "")+"this, '%%%%', '"+e.getName()+"', "+srls.replace("#", "elem."+s+"[i]")+", i);\r\n");
        if (typeIsPrimitive(e.typeCode())) 
          workingParserJ.append(
              "      if jsn.has('"+e.getName()+"') or jsn.has('_"+e.getName()+"') then\r\n"+
                  "      iteratePrimitiveArray(jsn.vArr['"+e.getName()+"'], jsn.vArr['_"+e.getName()+"'], result."+s+", parse"+parseName(tn)+");\r\n");
        else 
          workingParserJ.append("    if jsn.has('"+e.getName()+"') then\r\n"+
              "      iterateArray(jsn.vArr['"+e.getName()+"'], result."+s+", parse"+parseName(tn)+");\r\n");

        workingComposerJ.append("  if (SummaryOption in ["+sumSet+"]) and (elem."+s+".Count > 0) then\r\n");
        if (typeIsPrimitive(e.typeCode())) 
          workingComposerJ.append(
              "  begin\r\n"+
                  "    ext := false;\r\n"+    
                  "    val := false;\r\n"+    
                  "    for i := 0 to elem."+s+".Count - 1 do\r\n"+
                  "    begin\r\n"+
                  "      ext := ext or ((elem."+s+"[i].id <> '') or (elem."+s+"[i].hasExtensionList) {no-comments or (elem."+s+"[i].hasComments)});\r\n"+
                  "      val := val or (elem."+s+"[i].hasPrimitiveValue);\r\n"+
                  "    end;\r\n"+
                  "    if val then\r\n"+
                  "    begin\r\n"+
                  "      json.valueArray('"+e.getName()+"');\r\n"+
                  "      for i := 0 to elem."+s+".Count - 1 do\r\n"+
                  "        "+srlsdJ+"Value(json, '',"+srls.replace("#", "elem."+s+"[i]")+", true);\r\n"+
                  "      json.FinishArray;\r\n"+
                  "    end;\r\n"+
                  "    if ext then\r\n"+
                  "    begin\r\n"+
                  "      json.valueArray('_"+e.getName()+"');\r\n"+
                  "      for i := 0 to elem."+s+".Count - 1 do\r\n"+
                  "        "+srlsdJ+"Props(json, '',"+srls.replace("#", "elem."+s+"[i]")+", true);\r\n"+
                  "      json.FinishArray;\r\n"+
                  "    end;\r\n"+
              "  end;\r\n");
       else
          workingComposerJ.append(
              "  begin\r\n"+
                  "    json.valueArray('"+e.getName()+"');\r\n"+
                  "    for i := 0 to elem."+s+".Count - 1 do\r\n"+
                  "      "+srlsdJ+"(json, '', "+getParam3(tn)+srls.replace("#", "elem."+s+"[i]")+"); {z - "+e.typeCode()+"}\r\n"+
                  "    json.FinishArray;\r\n"+
              "  end;\r\n");
      }
    } else {
      if (enumNames.contains(tn)) {         
        defPriv1.append("    F"+getTitle(s)+" : TFhirEnum;\r\n"); 
        defPriv2.append("    Procedure Set"+getTitle(s)+"(value : TFhirEnum);\r\n");
        defPriv2.append("    Function Get"+getTitle(s)+"ST : "+tn+";\r\n");
        defPriv2.append("    Procedure Set"+getTitle(s)+"ST(value : "+tn+");\r\n");
        defPub.append("    {@member "+s+"\r\n");
        defPub.append("      "+makeDocoSafe(e.getDefinition())+"\r\n");
        defPub.append("    }\r\n");
        defPub.append("    property "+s+" : "+tn+" read Get"+getTitle(s)+"ST write Set"+getTitle(s)+"ST;\r\n");
        defPub.append("    property "+s+"Element : TFhirEnum read F"+getTitle(s)+" write Set"+getTitle(s)+";\r\n");
      } else {
        defPriv1.append("    F"+getTitle(s)+" : "+tn+";\r\n");
        defPriv2.append("    Procedure Set"+getTitle(s)+"(value : "+tn+");\r\n");
        if (simpleTypes.containsKey(tn)) {
          String sn = simpleTypes.get(tn);
          defPriv2.append("    Function Get"+getTitle(s)+"ST : "+sn+";\r\n");
          defPriv2.append("    Procedure Set"+getTitle(s)+"ST(value : "+sn+");\r\n");
          defPub.append("    {@member "+s+"\r\n");
          defPub.append("      Typed access to "+makeDocoSafe(e.getDefinition())+"\r\n");
          defPub.append("    }\r\n");
          defPub.append("    property "+s+" : "+sn+" read Get"+getTitle(s)+"ST write Set"+getTitle(s)+"ST;\r\n");
          defPub.append("    {@member "+s+"Element\r\n");
          defPub.append("      "+makeDocoSafe(e.getDefinition())+"\r\n");
          defPub.append("    }\r\n");
          defPub.append("    property "+s+"Element : "+tn+" read F"+getTitle(s)+" write Set"+getTitle(s)+";\r\n");
        } else {
          defPub.append("    {@member "+s+"\r\n");
          defPub.append("      Typed access to "+makeDocoSafe(e.getDefinition())+" (defined for API consistency)\r\n");
          defPub.append("    }\r\n");
          defPub.append("    property "+s+" : "+tn+" read F"+getTitle(s)+" write Set"+getTitle(s)+";\r\n");
          defPub.append("    {@member "+s+"Element\r\n");
          defPub.append("      "+makeDocoSafe(e.getDefinition())+"\r\n");
          defPub.append("    }\r\n");
          defPub.append("    property "+s+"Element : "+tn+" read F"+getTitle(s)+" write Set"+getTitle(s)+";\r\n");
        }
      }
      defPub.append("\r\n");
      if (typeIsSimple(tn) && !tn.equals("TFhirXHtmlNode")) {
        if (enumNames.contains(tn)) {         
          impl.append("Procedure "+cn+".Set"+getTitle(s)+"(value : TFhirEnum);\r\nbegin\r\n  F"+getTitle(s)+".free;\r\n  F"+getTitle(s)+" := value;\r\nend;\r\n\r\n");
          impl.append("Function "+cn+".Get"+getTitle(s)+"ST : "+tn+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    result := "+tn+"(0)\r\n  else\r\n    result := "+tn+"(StringArrayIndexOfSensitive(CODES_"+tn+", F"+getTitle(s)+".value));\r\nend;\r\n\r\n");
          impl.append("Procedure "+cn+".Set"+getTitle(s)+"ST(value : "+tn+");\r\nbegin\r\n  if ord(value) = 0 then\r\n    "+getTitle(s)+"Element := nil\r\n  else\r\n    "+getTitle(s)+"Element := TFhirEnum.create(SYSTEMS_"+tn+"[value], CODES_"+tn+"[value]);\r\nend;\r\n\r\n");
          setprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := asEnum(SYSTEMS_"+tn+", CODES_"+tn+", propValue)\r\n");
        } else {
          impl.append("Procedure "+cn+".Set"+getTitle(s)+"(value : TFhirEnum);\r\nbegin\r\n  F"+getTitle(s)+".free;\r\n  F"+getTitle(s)+" := value;\r\nend;\r\n\r\n");
          impl.append("Procedure "+cn+".Set"+getTitle(s)+"ST(value : "+tn+");\r\nbegin\r\n  if ord(value) = 0 then\r\n    "+getTitle(s)+" := nil\r\n  else\r\n    "+getTitle(s)+" := TFhirEnum.create(SYSTEMS_"+tn+"[value], CODES_"+tn+"[value]);\r\nend;\r\n\r\n");
          setprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+" := asEnum(SYSTEMS_"+tn+", CODES_"+tn+", propValue)\r\n");
        }
        assign.append("  F"+getTitle(s)+" := "+cn+"(oSource).F"+getTitle(s)+".Link;\r\n");
        getkids.append("  if (child_name = '"+e.getName()+"') Then\r\n     list.add(F"+getTitle(s)+".Link);\r\n");
        getprops.append("  oList.add(TFHIRProperty.create(self, '"+e.getName()+"', '"+breakConstant(e.typeCode())+"', false, TFHIREnum, "+propV+".Link));{1}\r\n");
        if (e.getName().endsWith("[x]"))
          throw new Exception("Not done yet");
        if (e.isXmlAttribute())
          workingParserXA.append("    result."+s+"ST := fix me (and compose)! GetAttribute(element, '"+e.getName()+"');\r\n");
        else  
          workingParserX.append("      else if (child.baseName = '"+e.getName()+"') then\r\n        result."+s+"Element := "+parse+"{1a}\r\n");
        workingParserJ.append("    if jsn.has('"+e.getName()+"') or jsn.has('_"+e.getName()+"')  then\r\n"+
            "      result."+s+"Element := parseEnum(jsn.path+'/"+e.getName()+"', jsn['"+e.getName()+"'], jsn.vObj['_"+e.getName()+"'], CODES_"+tn+", SYSTEMS_"+tn+");\r\n");
        destroy.append("  F"+getTitle(s)+".free;\r\n");
        if (enumNames.contains(tn)) {         
          workingComposerX.append("  if (SummaryOption in ["+sumSet+"]) then\r\n     ComposeEnum(xml, '"+e.getName()+"', elem."+getTitle(s)+"Element, CODES_"+tn+");\r\n");
          if (!specialCaseInRDF(cn, e.getName()))
          workingComposerR.append("  if (SummaryOption in ["+sumSet+"]) then\r\n     ComposeEnum(this, '%%%%', '"+e.getName()+"', elem."+getTitle(s)+"Element, CODES_"+tn+", SYSTEMS_"+tn+", -1);\r\n");
          workingComposerJ.append("  if (SummaryOption in ["+sumSet+"]) then\r\n     ComposeEnumValue(json, '"+e.getName()+"', elem."+getTitle(s)+"Element, CODES_"+tn+", false);\r\n");
          workingComposerJ.append("  if (SummaryOption in ["+sumSet+"]) then\r\n     ComposeEnumProps(json, '"+e.getName()+"', elem."+getTitle(s)+"Element, CODES_"+tn+", false);\r\n");
        } else {
          workingComposerX.append("  if (SummaryOption in ["+sumSet+"]) then\r\n     Compose"+tn+"(xml, '"+e.getName()+"', elem."+getTitle(s)+");{x.1}\r\n");
          if (!specialCaseInRDF(cn, e.getName()))
          workingComposerR.append("  if (SummaryOption in ["+sumSet+"]) then\r\n     Compose"+tn+"("+(tn.equals("Coding") ? "false, " : "")+"this, '%%%%', '"+e.getName()+"', elem."+getTitle(s)+", -1);{x.1}\r\n");
          workingComposerJ.append("  if (SummaryOption in ["+sumSet+"]) then\r\n     Compose"+tn+"Value(json, '"+e.getName()+"', elem."+getTitle(s)+", false); {1}\r\n");        
          workingComposerJ.append("  if (SummaryOption in ["+sumSet+"]) then\r\n     Compose"+tn+"Props(json, '"+e.getName()+"', elem."+getTitle(s)+", false); {y}\r\n");        
        }
      }
      else {
        impl.append("Procedure "+cn+".Set"+getTitle(s)+"(value : "+tn+");\r\nbegin\r\n  F"+getTitle(s)+".free;\r\n  F"+getTitle(s)+" := value;\r\nend;\r\n\r\n");
        if (simpleTypes.containsKey(tn)) {
          String sn = simpleTypes.get(tn);
          if (sn.equals("String")) {
            impl.append("Function "+cn+".Get"+getTitle(s)+"ST : "+sn+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    result := ''\r\n  else\r\n    result := F"+getTitle(s)+".value;\r\nend;\r\n\r\n");
            impl.append("Procedure "+cn+".Set"+getTitle(s)+"ST(value : "+sn+");\r\nbegin\r\n  if value <> '' then\r\n  begin\r\n    if F"+getTitle(s)+" = nil then\r\n      F"+getTitle(s)+" := "+tn+".create;\r\n    F"+getTitle(s)+".value := value\r\n  end\r\n  else if F"+getTitle(s)+" <> nil then\r\n    F"+getTitle(s)+".value := '';\r\nend;\r\n\r\n");
          } else if (sn.equals("Boolean")) {
            impl.append("Function "+cn+".Get"+getTitle(s)+"ST : "+sn+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    result := false\r\n  else\r\n    result := F"+getTitle(s)+".value;\r\nend;\r\n\r\n");
            impl.append("Procedure "+cn+".Set"+getTitle(s)+"ST(value : "+sn+");\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    F"+getTitle(s)+" := "+tn+".create;\r\n  F"+getTitle(s)+".value := value\r\nend;\r\n\r\n");
          } else {
            impl.append("Function "+cn+".Get"+getTitle(s)+"ST : "+sn+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    result := nil\r\n  else\r\n    result := F"+getTitle(s)+".value;\r\nend;\r\n\r\n");
            impl.append("Procedure "+cn+".Set"+getTitle(s)+"ST(value : "+sn+");\r\nbegin\r\n  if value <> nil then\r\n  begin\r\n    if F"+getTitle(s)+" = nil then\r\n      F"+getTitle(s)+" := "+tn+".create;\r\n    F"+getTitle(s)+".value := value\r\n  end\r\n  else if F"+getTitle(s)+" <> nil then\r\n    F"+getTitle(s)+".value := nil;\r\nend;\r\n\r\n");
          }
          assign.append("  "+s+"Element := "+cn+"(oSource)."+s+"Element.Clone;\r\n");
        } else
          assign.append("  "+s+" := "+cn+"(oSource)."+s+".Clone;\r\n");
        destroy.append("  F"+getTitle(s)+".free;\r\n");
        if (e.getName().endsWith("[x]"))
          getkids.append("  if (child_name = '"+e.getName()+"') or (child_name = '"+e.getName().substring(0, e.getName().length()-3)+"') Then\r\n     list.add(F"+getTitle(s)+".Link);\r\n");
        else
          getkids.append("  if (child_name = '"+e.getName()+"') Then\r\n     list.add(F"+getTitle(s)+".Link);\r\n");
        getprops.append("  oList.add(TFHIRProperty.create(self, '"+e.getName()+"', '"+breakConstant(e.typeCode())+"', false, "+tn+", "+propV+".Link));{2}\r\n");
        if (e.getName().endsWith("[x]")) {
          if (!typeIsPrimitive(e.typeCode()))
            setprops.append("  else if (propName.startsWith('"+e.getName().substring(0, e.getName().length()-3)+"')) then "+propV.substring(1)+" := propValue as "+tn+"{4}\r\n");
          else 
            setprops.append("  else if (propName.startsWith('"+e.getName().substring(0, e.getName().length()-3)+"')) then "+propV.substring(1)+" := propValue as "+tn+"{5}\r\n");
        } else {
          if (!typeIsPrimitive(e.typeCode()))
            if (simpleTypes.containsKey(tn))
              setprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := propValue as "+tn+"{4a}\r\n");
            else {
              setprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+" := propValue as "+tn+"{4b}\r\n");
              makeprops.append("  else if (propName = '"+e.getName()+"') then begin "+propV.substring(1)+" := "+tn+".create(); result := "+propV.substring(1)+"; end{4b}\r\n");
            }
          else
            if (!simpleTypes.containsKey(tn)) {
              setprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+" := propValue as "+tn+"{5b}\r\n");          
              makeprops.append("  else if (propName = '"+e.getName()+"') then begin "+propV.substring(1)+" := "+tn+".create(); result := "+propV.substring(1)+"; end{5b}\r\n");
            } else if (tn.equals("TFhirCode"))
              setprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := asCode(propValue)\r\n");
            else
              setprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := as"+tn.substring(5)+"(propValue){5a}\r\n");          
            
        }
        if (e.getName().contains("[x]") && e.getTypes().size() > 1) {
          String pfx = e.getName().replace("[x]", "");
          int t = e.getTypes().size();
          TypeRef lt = lastTypeNotDerived(e.getTypes());
          int i = 0;
          // the order here is important - derived types have to be done first
          for (TypeRef td : e.getTypes()) {
            if (isDerivedType(td)) {
              boolean last = i == t - 1;
              genTypeSerialisers(e, cn, s, sumSet, pfx, i == 0, last, td);
              i++;
            }
          }
          for (TypeRef td : e.getTypes()) {
            if (!isDerivedType(td)) {
              boolean last = i == t - 1 || td == lt;
              genTypeSerialisers(e, cn, s, sumSet, pfx, i == 0, last, td);
              i++;
            }
          }

        } else if (!e.getName().equals("[type]") && !e.getName().contains("[x]")) {
          if (e.isXmlAttribute()) {
            workingParserXA.append("    result."+s+" := GetAttribute(element, '"+e.getName()+"');{x.4}\r\n");
            workingComposerXA.append("  Attribute(xml, '"+e.getName()+"', elem."+s+"  );\r\n");
          } else {  
            if (typeIsPrimitive(e.typeCode())) { 
              workingParserX.append("      else if (child.baseName = '"+e.getName()+"') then\r\n        result."+s+"Element := Parse"+parseName(tn)+"(child, path+'/"+e.getName()+"') {b}\r\n");
              workingComposerX.append("  if (SummaryOption in ["+sumSet+"]) then\r\n    Compose"+parseName(tn)+"(xml, '"+e.getName()+"', elem."+s+"Element);{x.2b}\r\n");
            } else {
              workingParserX.append("      else if (child.baseName = '"+e.getName()+"') then\r\n        result."+s+" := Parse"+parseName(tn)+"(child, path+'/"+e.getName()+"') {b}\r\n");
              workingComposerX.append("  if (SummaryOption in ["+sumSet+"]) then\r\n    Compose"+parseName(tn)+"(xml, '"+e.getName()+"', "+getParam3(tn)+"elem."+s+");{x.2a}\r\n");
            }
          }
          if (!specialCaseInRDF(cn, e.getName()))
          workingComposerR.append("  if (SummaryOption in ["+sumSet+"]) then\r\n    Compose"+parseNameR(tn)+"("+(parseNameR(tn).equals("Coding") ? "false, " : "")+"this, '%%%%', '"+e.getName()+"', elem."+s+"Element, -1);{x.2c}\r\n");
            if (typeIsPrimitive(e.typeCode())) {
              workingParserJ.append("    if jsn.has('"+e.getName()+"') or jsn.has('_"+e.getName()+"') then\r\n        result."+s+"Element := Parse"+parseName(tn)+"(jsn['"+e.getName()+"'], jsn.vObj['_"+e.getName()+"']);{q}\r\n");
            } else if (e.typeCode().equals("xhtml"))
              workingParserJ.append("    if jsn.has('"+e.getName()+"') then\r\n        result."+s+" := Parse"+parseName(tn)+"(jsn.path+'.div', jsn['"+e.getName()+"']);{q}\r\n");
            else
              workingParserJ.append("    if jsn.has('"+e.getName()+"') then\r\n        result."+s+" := Parse"+parseName(tn)+"(jsn.vObj['"+e.getName()+"']);{q}\r\n");
            if (typeIsPrimitive(e.typeCode())) {
              workingComposerJ.append("  if (SummaryOption in ["+sumSet+"]) then\r\n    Compose"+parseName(tn)+"Value(json, '"+e.getName()+"', elem."+s+"Element, false);\r\n");
              workingComposerJ.append("  if (SummaryOption in ["+sumSet+"]) then\r\n    Compose"+parseName(tn)+"Props(json, '"+e.getName()+"', elem."+s+"Element, false);\r\n");
            } else
              workingComposerJ.append("  if (SummaryOption in ["+sumSet+"]) then\r\n    Compose"+parseName(tn)+"(json, '"+e.getName()+"', "+getParam3(tn)+"elem."+s+"); {a}\r\n");
        } else {
          String pfx = e.getName().contains("[x]") ? e.getName().replace("[x]", "") : "";
          int i = 0;
          for (DefinedCode cd : definitions.getPrimitives().values()) {
            if (!(cd instanceof PrimitiveType))
              generatePrimitiveTypesSerialiser(e, cn, s, sumSet, pfx, i, cd);
            i++;
          }
          for (DefinedCode cd : definitions.getPrimitives().values()) {
            if (cd instanceof PrimitiveType)
              generatePrimitiveTypesSerialiser(e, cn, s, sumSet, pfx, i, cd);
            i++;
          }
          for (ElementDefn ed : definitions.getTypes().values()) {
            workingParserX.append("      else if (child.baseName = '"+pfx+getTitle(ed.getName())+"') then\r\n        result."+s+" := Parse"+getTitle(ed.getName())+"(child, path+'."+pfx+getTitle(ed.getName())+"') {e"+ed.getName()+"}\r\n");
            workingComposerX.append("  else if (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhir"+getTitle(ed.getName())+") {8} then\r\n    Compose"+getTitle(ed.getName())+"(xml, '"+pfx+getTitle(ed.getName())+"', TFhir"+getTitle(ed.getName())+"(elem."+s+"))\r\n");
            if (!specialCaseInRDF(cn, e.getName())) 
              workingComposerR.append("  else if (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhir"+getTitle(ed.getName())+") {8} then\r\n    Compose"+getTitle(ed.getName())+"("+(ed.getName().equals("Coding") ? "false, " : "")+"this, '%%%%', '"+pfx+getTitle(ed.getName())+"', TFhir"+getTitle(ed.getName())+"(elem."+s+"), -1)\r\n");
            workingParserJ.append("    if jsn.has('"+pfx+getTitle(ed.getName())+"') {a7} then\r\n        result."+s+" := Parse"+getTitle(ed.getName())+"(jsn.vObj['"+pfx+getTitle(ed.getName())+"']);\r\n");
            workingComposerJ.append("  else if (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhir"+getTitle(ed.getName())+") then\r\n    Compose"+getTitle(ed.getName())+"(json, '"+pfx+getTitle(ed.getName())+"', TFhir"+getTitle(ed.getName())+"(elem."+s+"))\r\n");
          }
          int t = definitions.getStructures().size();
          i = 0;
          for (ElementDefn ed : definitions.getStructures().values()) {
            workingParserX.append("      else if (child.baseName = '"+pfx+getTitle(ed.getName())+"') then\r\n        result."+s+" := Parse"+getTitle(ed.getName())+"(child, path+'/"+pfx+getTitle(ed.getName())+"') {f}\r\n");
            workingComposerX.append("  else if (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhir"+getTitle(ed.getName())+") {9} then\r\n    Compose"+getTitle(ed.getName())+"(xml, '"+pfx+getTitle(ed.getName())+"', TFhir"+getTitle(ed.getName())+"(elem."+s+"))"+(i < t-1 ? "" : ";")+"\r\n");
            if (!specialCaseInRDF(cn, e.getName()))
            workingComposerR.append("  else if (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhir"+getTitle(ed.getName())+") {9} then\r\n    Compose"+getTitle(ed.getName())+"(this, '%%%%', '"+pfx+getTitle(ed.getName())+"', TFhir"+getTitle(ed.getName())+"(elem."+s+"), -1)"+(i < t-1 ? "" : ";")+"\r\n");
            workingParserJ.append("    if jsn.has('"+pfx+getTitle(ed.getName())+"') {a9} then\r\n        result."+s+" := Parse"+getTitle(ed.getName())+"(jsn.vObj['"+pfx+getTitle(ed.getName())+"']);\r\n");
            workingComposerJ.append("  else if (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhir"+getTitle(ed.getName())+") then\r\n    Compose"+getTitle(ed.getName())+"(json, '"+pfx+getTitle(ed.getName())+"', TFhir"+getTitle(ed.getName())+"(elem."+s+"))"+(i < t-1 ? "" : ";")+"\r\n");
            i++;
          }
        }
      }
    }
  }

  private TypeRef lastTypeNotDerived(List<TypeRef> types) {
    TypeRef result = null;
    for (int i = 0; i < types.size(); i++)
      if (!isDerivedType(types.get(i)))
        result = types.get(i);
    return result;
  }

  private void generatePrimitiveTypesSerialiser(ElementDefn e, String cn, String s, String sumSet, String pfx, int i,
      DefinedCode cd) {
    workingParserX.append("      else if (child.baseName = '"+pfx+getTitle(cd.getCode())+"') then\r\n        result."+s+" := Parse"+getTitle(cd.getCode())+"(child, path+'."+pfx+getTitle(cd.getCode())+"') {c}\r\n");
    String ptn = "TFhir"+getTitle(cd.getCode());
    workingComposerX.append("  "+(i > 0 ? "else " : "")+"if (SummaryOption in ["+sumSet+"]) and (elem."+s+" is "+ptn+") {1} then\r\n    Compose"+ptn.substring(5)+"(xml, '"+pfx+getTitle(cd.getCode())+"', "+ptn+"(elem."+s+"))\r\n");
    if (!specialCaseInRDF(cn, e.getName()))
    workingComposerR.append("  "+(i > 0 ? "else " : "")+"if (SummaryOption in ["+sumSet+"]) and (elem."+s+" is "+ptn+") {1} then\r\n    Compose"+ptn.substring(5)+"(this, '%%%%', '"+pfx+getTitle(cd.getCode())+"', "+ptn+"(elem."+s+"), -1)\r\n");
    workingParserJ.append("    if jsn.has('"+pfx+getTitle(cd.getCode())+"') or jsn.has('_"+pfx+getTitle(cd.getCode())+"') then\r\n        result."+s+" := Parse"+getTitle(cd.getCode())+"(jsn['"+pfx+getTitle(cd.getCode())+"'], jsn.vObj['_"+pfx+getTitle(cd.getCode())+"']);\r\n");
    workingComposerJ.append("  "+(i > 0 ? "else " : "")+"if (SummaryOption in ["+sumSet+"]) and (elem."+s+" is "+ptn+") then\r\n"+
        "  begin\r\n"+
        "    Compose"+ptn.substring(5)+"Value(json, '"+pfx+getTitle(cd.getCode())+"', "+ptn+"(elem."+s+"), false);\r\n"+
        "    Compose"+ptn.substring(5)+"Props(json, '"+pfx+getTitle(cd.getCode())+"', "+ptn+"(elem."+s+"), false)\r\n"+
        "  end\r\n");
  }

  private boolean isDerivedType(TypeRef td) {
    return !(definitions.getPrimitives().get(td.getName()) instanceof PrimitiveType); 
  }

  private void genTypeSerialisers(ElementDefn e, String cn, String s, String sumSet, String pfx, boolean first, boolean last, TypeRef td) throws Exception {
    if (td.isResourceReference()) {
      workingParserX.append("      else if (child.baseName = '"+pfx+"Reference') then\r\n        result."+s+" := ParseReference(child, path+'/"+pfx+"Reference') {a}\r\n");
      workingComposerX.append("  "+(first ? "if" : "else if")+" (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhirReference) {2} then\r\n    ComposeReference(xml, '"+pfx+"Reference', TFhirReference(elem."+s+"))"+(last?";" : "")+"\r\n");
      workingParserJ.append("    if jsn.has('"+pfx+"Reference') {a3} then\r\n      result."+s+" := ParseReference(jsn.vObj['"+pfx+"Reference']);\r\n");
      workingComposerJ.append("  "+(first ? "if" : "else if")+" (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhirReference) then\r\n    ComposeReference(json, '"+pfx+"Reference', TFhirReference(elem."+s+"))"+(last?";" : "")+"\r\n");
      if (!specialCaseInRDF(cn, e.getName()))
      workingComposerR.append("  "+(first ? "if" : "else if")+" (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhirReference) {2} then\r\n    ComposeReference(this, '%%%%', '"+pfx+"Reference', TFhirReference(elem."+s+"), -1)"+(last?";" : "")+"\r\n");
    }
    else {
      if (td.hasParams())
        throw new Exception("Type "+td.summary()+" has parameters");                
      String tname = td.getName();
      String tpname = tname; 
      workingParserX.append("      else if (child.baseName = '"+pfx+getTitle(tpname)+"') then\r\n        result."+s+" := Parse"+getTitle(tname)+"(child, path+'/"+pfx+getTitle(tname)+"'){x.3}\r\n");
      workingComposerX.append("  "+(first ? "if" : "else if")+" (SummaryOption in ["+sumSet+"]) and (elem."+s+" is "+getTypeName(tname)+") {6} then\r\n    Compose"+getTitle(tname)+"(xml, '"+pfx+getTitle(tpname)+"', "+getTypeName(tname)+"(elem."+s+"))"+(last?";" : "")+"\r\n");
      if (typeIsPrimitive(tname)) {
        workingComposerJ.append("  "+(first ? "if" : "else if")+" (SummaryOption in ["+sumSet+"]) and (elem."+s+" is "+getTypeName(tname)+") then \r\n"+
            "  begin\r\n"+
            "    Compose"+getTitle(tname)+"Value(json, '"+pfx+getTitle(tpname)+"', "+getTypeName(tname)+"(elem."+s+"), false);\r\n"+
            "    Compose"+getTitle(tname)+"Props(json, '"+pfx+getTitle(tpname)+"', "+getTypeName(tname)+"(elem."+s+"), false);\r\n  end"+(last?";" : "")+"\r\n");
        workingParserJ.append("    if jsn.has('"+pfx+getTitle(tpname)+"') or jsn.has('_"+pfx+getTitle(tname)+"') then\r\n      result."+s+" := parse"+Utilities.capitalize(tname)+"(jsn['"+pfx+getTitle(tpname)+"'], jsn.vObj['_"+pfx+getTitle(tpname)+"']);\r\n");
      } else {
        workingComposerJ.append("  "+(first ? "if" : "else if")+" (SummaryOption in ["+sumSet+"]) and (elem."+s+" is "+getTypeName(tname)+") then \r\n"+
            "    Compose"+getTitle(tname)+"(json, '"+pfx+getTitle(tpname)+"', "+getTypeName(tname)+"(elem."+s+")) "+(last?";" : "")+"\r\n");
        workingParserJ.append("    if jsn.has('"+pfx+getTitle(tpname)+"') {a4} then\r\n      result."+s+" := Parse"+getTitle(tname)+"(jsn.vObj['"+pfx+getTitle(tpname)+"']);\r\n");
      }
      if (!specialCaseInRDF(cn, e.getName()))
      workingComposerR.append("  "+(first ? "if" : "else if")+" (SummaryOption in ["+sumSet+"]) and (elem."+s+" is "+getTypeName(tname)+") {6} then\r\n    Compose"+getTitle(tname)+"("+(tname.equals("Coding") ? "false, " : "")+"this, '%%%%', '"+pfx+getTitle(tpname)+"', "+getTypeName(tname)+"(elem."+s+"), -1)"+(last?";" : "")+"\r\n");
    }
  }

  private boolean specialCaseInRDF(String tn, String name) {
    String type = tn.substring(5);
    if (type.equals("Quantity"))
      return Utilities.existsInList(name, "system", "code");
    else
      return false;
  }

  private String primitiveParse(String name, String prefix) {
    if (name.equals("integer") || name.equals("UnsignedInt")  || name.equals("PositiveInt")) 
      return "ParseIntegerValue(path+'."+prefix+'.'+Utilities.capitalize(name)+"')";
    if (name.equals("boolean")) 
      return "ParseBooleanValue(path+'."+prefix+'.'+Utilities.capitalize(name)+"')";
    if (name.equals("instant") || name.equals("dateTime") || name.equals("date")) 
      return "ParseDateAndTimeValue(path+'."+prefix+'.'+Utilities.capitalize(name)+"')";
    return "ParseStringValue(path+'."+prefix+'.'+Utilities.capitalize(name)+"')";
  }

  private boolean typeIsPrimitive(String tn) {
    return tn.equalsIgnoreCase("uri") || tn.equalsIgnoreCase("datetime") || tn.equalsIgnoreCase("code") || tn.equalsIgnoreCase("boolean")
        || tn.equalsIgnoreCase("integer")  || tn.equals("unsignedInt")  || tn.equals("positiveInt")|| tn.equalsIgnoreCase("instant") || tn.equalsIgnoreCase("time")
        || tn.equalsIgnoreCase("datetime") || tn.equalsIgnoreCase("date") || tn.equalsIgnoreCase("id") || tn.equalsIgnoreCase("oid")
        || tn.equalsIgnoreCase("decimal") || tn.equalsIgnoreCase("string") || tn.equalsIgnoreCase("base64Binary") || tn.equalsIgnoreCase("markdown");
  }

  private String breakConstant(String typeCode) {
    if (typeCode.length() < 255)
      return typeCode;
    else
      return typeCode.substring(0, 250)+"'+'"+typeCode.substring(250);
  }

  private String parseName(String tn) {
    if (tn.equals("TFhirResource"))
      return "InnerResource";
    else
      return tn.startsWith("TFhir") ? tn.substring(5) : tn.substring(1);
  }

  private String parseNameR(String tn) {
      return tn.startsWith("TFhir") ? tn.substring(5) : tn.substring(1);
  }

  private String getParam3(String tn) {
    if (tn.equals("TFhirResource"))  
      return "elem, ";
    else
      return "";
  }


  private void defineList(String tn, String tnl, String sn, ClassCategory category, boolean isAbstract, boolean isEnum) {
    if (generics)
      return; 
    if (tnl.contains("{"))
      tnl = tnl.substring(0, tnl.indexOf("{"));
    if (tn.contains("{"))
      tn = tn.substring(0, tn.indexOf("{"));
    if (!lists.contains(tnl)) {
      lists.add(tn+"List");
      String tt = tn.substring(1);
      getCode(category).classFwds.add("  "+tn+"List = class;\r\n");
      types.add(tn+"List");
      getCode(category).classDefs.add(
              "  "+tnl+"Enumerator = class (TAdvObject)\r\n"+
              "  private\r\n"+
              "    FIndex : integer;\r\n"+
              "    FList : "+tnl+";\r\n"+
              "    function GetCurrent : "+tn+";\r\n"+
              "  public\r\n"+
              "    constructor Create(list : "+tnl+");\r\n"+
              "    Destructor Destroy; override;\r\n"+
              "    function MoveNext : boolean;\r\n"+
              "    property Current : "+tn+" read GetCurrent;\r\n"+
              "  end;\r\n\r\n");


      getCode(category).classDefs.add(
          "  {@Class "+tn+"List\r\n"+
              "    A list of "+tt+"\r\n"+
              "  }\r\n"+
              "  "+tn+"List = class ("+listForm("TFHIRObject")+")\r\n"+
              "  private\r\n");
      if (isEnum) {
        getCode(category).classDefs.add("    FSystems : Array Of String;\r\n");
        getCode(category).classDefs.add("    FCodes : Array Of String;\r\n");
      }
      
      getCode(category).classDefs.add(
              "    function GetItemN(index : Integer) : "+tn+";\r\n"+
              "    procedure SetItemN(index : Integer; value : "+tn+");\r\n"+
              "  public\r\n");
      if (isEnum)
        getCode(category).classDefs.add("    constructor Create(Systems, Codes : Array Of String);\r\n");

      getCode(category).classDefs.add(
              "    {!script hide}\r\n"+
              "    function Link : "+tn+"List; Overload;\r\n"+
              "    function Clone : "+tn+"List; Overload;\r\n"+
              "    function GetEnumerator : "+tnl+"Enumerator;\r\n"+
              "    {!script show}\r\n"+
          "    \r\n");
      if (!isAbstract)
        getCode(category).classDefs.add(
            "    {@member Append\r\n"+
                "      Add a "+tt+" to the end of the list.\r\n"+
                "    }\r\n"+
                "    function Append : "+tn+";\r\n");
      getCode(category).classDefs.add(
          "    \r\n"+
              "    {@member AddItem\r\n"+
              "      Add an already existing "+tt+" to the end of the list.\r\n"+
              "    }\r\n"+
              "    procedure AddItem(value : "+tn+"); overload;\r\n");
      if (sn != null)    
        getCode(category).classDefs.add(
            "    \r\n"+
                "    {@member AddItem\r\n"+
                "      Add an already existing "+tt+" to the end of the list.\r\n"+
                "    }\r\n"+
                "    procedure AddItem(value : "+sn+"); overload;\r\n");
          getCode(category).classDefs.add(
              "    \r\n"+
              "    {@member IndexOf\r\n"+
              "      See if an item is already in the list. returns -1 if not in the list\r\n"+
              "    }\r\n"+
              "    \r\n"+
              "    {@member IndexOf\r\n"+
              "      See if an item is already in the list. returns -1 if not in the list\r\n"+
              "    }\r\n"+
              "    function IndexOf(value : "+tn+") : Integer;\r\n"+
          "    \r\n");
      if (!isAbstract)
        getCode(category).classDefs.add(
            "    {@member Insert\r\n"+
                "      Insert "+tt+" before the designated index (0 = first item)\r\n"+
                "    }\r\n"+
                "    function Insert(index : Integer) : "+tn+";\r\n"+
            "    \r\n");
      getCode(category).classDefs.add(
          "    {@member InsertItem\r\n"+
              "       Insert an existing "+tt+" before the designated index (0 = first item)\r\n"+
              "    }\r\n"+
              "    procedure InsertItem(index : Integer; value : "+tn+");\r\n"+
              "    \r\n"+
              "    {@member Item\r\n"+
              "       Get the iIndexth "+tt+". (0 = first item)\r\n"+
              "    }\r\n"+
              "    \r\n"+
              "    {@member Item\r\n"+
              "       Get the iIndexth "+tt+". (0 = first item)\r\n"+
              "    }\r\n"+
              "    procedure SetItemByIndex(index : Integer; value : "+tn+");\r\n"+
              "    \r\n"+
              "    {@member Count\r\n"+
              "      The number of items in the collection\r\n"+
              "    }\r\n"+
              "    function Item(index : Integer) : "+tn+";\r\n"+
              "    \r\n"+
              "    {@member Count\r\n"+
              "      The number of items in the collection\r\n"+
              "    }\r\n"+
              "    function Count : Integer; Overload;\r\n"+
              "    \r\n"+
              "    {@member remove\r\n"+
              "      Remove the indexth item. The first item is index 0.\r\n"+
              "    }\r\n"+
              "    procedure Remove(index : Integer);\r\n"+
              "    {@member ClearItems\r\n"+
              "      Remove All Items from the list\r\n"+
              "    }\r\n"+
              "    procedure ClearItems;\r\n"+
              "    \r\n"+
              "    Property "+Utilities.pluralizeMe(tt)+"[index : Integer] : "+tn+" read GetItemN write SetItemN; default;\r\n"+
              "  End;\r\n"+
              "\r\n"  
          );
      getCode(category).classImpls.add(
          "{ "+tnl+"Enumerator }\r\n"+
              "\r\n"+
              "Constructor "+tnl+"Enumerator.Create(list : "+tnl+");\r\n"+
              "begin\r\n"+
              "  inherited Create;\r\n"+
              "  FIndex := -1;\r\n"+
              "  FList := list;\r\n"+
              "end;\r\n"+
              "\r\n"+
              "Destructor "+tnl+"Enumerator.Destroy;\r\n"+
              "begin\r\n"+
              "  FList.Free;\r\n"+
              "  inherited;\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tnl+"Enumerator.MoveNext : boolean;\r\n"+
              "begin\r\n"+
              "  inc(FIndex);\r\n"+
              "  Result := FIndex < FList.count;\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tnl+"Enumerator.GetCurrent : "+tn+";\r\n"+
              "begin\r\n"+
              "  Result := FList[FIndex];\r\n"+
              "end;\r\n"+
          "\r\n");
      getCode(category).classImpls.add(
          "{ "+tn+"List }\r\n"+
              "procedure "+tn+"List.AddItem(value: "+tn+");\r\n"+
              "begin\r\n"+
              "  assert(value.ClassName = '"+tn+"', 'Attempt to add an item of type '+value.ClassName+' to a List of "+tn+"');\r\n"+
              "  add(value);\r\n"+
              "end;\r\n"+
          "\r\n");
      if (isEnum)
        getCode(category).classImpls.add("constructor "+tn+"List.Create(Systems, Codes : Array Of String);\r\n"+
            "var\r\n"+
            "  i : integer;\r\n"+
            "begin\r\n"+
            "  inherited create;\r\n"+
            "  SetLength(FSystems, length(systems));\r\n"+
            "  SetLength(FCodes, length(codes));\r\n"+
            "  for i := 0 to length(systems) - 1 do\r\n"+
            "  begin\r\n"+
            "    FSystems[i] := systems[i];\r\n"+
            "    FCodes[i] := codes[i];\r\n"+
            "  end;\r\n"+
            "end;\r\n"
            );
      if (sn != null)    
        if (isEnum)
          getCode(category).classImpls.add(
              "procedure "+tn+"List.AddItem(value: "+sn+");\r\n"+
              "begin\r\n"+
              "  add("+tn+".create(FSystems[StringArrayIndexOf(FCodes, value)], value));\r\n"+
              "end;\r\n"+
          "\r\n");
        else
          getCode(category).classImpls.add(
          "procedure "+tn+"List.AddItem(value: "+sn+");\r\n"+
          "begin\r\n"+
          "  add("+tn+".create(value));\r\n"+
          "end;\r\n"+
      "\r\n");
      if (!isAbstract)
        getCode(category).classImpls.add(
            "function "+tn+"List.Append: "+tn+";\r\n"+
                "begin\r\n"+
                "  result := "+tn+".create;\r\n"+
                "  try\r\n"+
                "    add(result.Link);\r\n"+
                "  finally\r\n"+
                "    result.free;\r\n"+
                "  end;\r\n"+
                "end;\r\n"+
            "\r\n");
      getCode(category).classImpls.add(
          "procedure "+tn+"List.ClearItems;\r\n"+
              "begin\r\n"+
              "  Clear;\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tnl+".GetEnumerator : "+tnl+"Enumerator;\r\n"+
              "begin\r\n"+
              "  result := "+tnl+"Enumerator.Create(self.link);\r\n"+
              "end;\r\n\r\n"+
              "function "+tn+"List.Clone: "+tn+"List;\r\n"+
              "begin\r\n"+
              "  result := "+tn+"List(inherited Clone);\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tn+"List.Count: Integer;\r\n"+
              "begin\r\n"+
              "  result := Inherited Count;\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tn+"List.GetItemN(index: Integer): "+tn+";\r\n"+
              "begin\r\n"+
              "  result := "+tn+"(ObjectByIndex[index]);\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tn+"List.IndexOf(value: "+tn+"): Integer;\r\n"+
              "begin\r\n"+
              "  result := IndexByReference(value);\r\n"+
              "end;\r\n"+
          "\r\n");
      if (!isAbstract)
        getCode(category).classImpls.add(
            "function "+tn+"List.Insert(index: Integer): "+tn+";\r\n"+
                "begin\r\n"+
                "  result := "+tn+".create;\r\n"+
                "  try\r\n"+
                "    inherited insert(index, result.Link);\r\n"+
                "  finally\r\n"+
                "    result.free;\r\n"+
                "  end;\r\n"+
                "end;\r\n"+
            "\r\n");
      getCode(category).classImpls.add(
          "procedure "+tn+"List.InsertItem(index: Integer; value: "+tn+");\r\n"+
              "begin\r\n"+
              "  assert(value is "+tn+");\r\n"+
              "  Inherited Insert(index, value);\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tn+"List.Item(index: Integer): "+tn+";\r\n"+
              "begin\r\n"+
              "  result := "+tn+"(ObjectByIndex[index]);\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tn+"List.Link: "+tn+"List;\r\n"+
              "begin\r\n"+
              "  result := "+tn+"List(inherited Link);\r\n"+
              "end;\r\n"+
              "\r\n"+
              "procedure "+tn+"List.Remove(index: Integer);\r\n"+
              "begin\r\n"+
              "  DeleteByIndex(index);\r\n"+
              "end;\r\n"+
              "\r\n"+
              "procedure "+tn+"List.SetItemByIndex(index: Integer; value: "+tn+");\r\n"+
              "begin\r\n"+
              "  assert(value is "+tn+");\r\n"+
              "  "+Utilities.pluralizeMe(tt)+"[index] := value;\r\n"+
              "end;\r\n"+
              "\r\n"+
              "procedure "+tn+"List.SetItemN(index: Integer; value: "+tn+");\r\n"+
              "begin\r\n"+
              "  assert(value is "+tn+");\r\n"+
              "  ObjectByIndex[index] := value;\r\n"+
              "end;\r\n"        
          ); 
    }
  }

  private boolean typeIsSimple(String tn) {
    if (tn == null)
      return false;
    return tn.equals("String") || tn.equals("Integer")  || tn.equals("unsignedInt")  || tn.equals("positiveInt") || tn.equals("Boolean") || tn.equals("TDateAndTime") || tn.equals("TFhirXHtmlNode")  || enumNames.contains(tn);
  }

  private String getTitle(String name) {
    if (name.length() < 2)
      return name.toUpperCase();
    else
      return name.substring(0, 1).toUpperCase()+ name.substring(1);
  }

  private String getElementName(String name) {
    name = name.replace("[x]", "").replace("[type]", "value");
    if (GeneratorUtils.isDelphiReservedWord(name) || name.equals("fScore"))
      return name+"_";
    else
      return name;
  }

  private String getPropertyName(String name) {
    return name.replace("[x]", "").replace("[type]", "value");
  }

  private String getTypeName(ElementDefn e) throws Exception {
    if (e.getTypes().size() > 1) {
      return "TFhirType";
    } else if (e.getTypes().size() == 0) {
      throw new Exception("not supported");
    } else {
      return getTypename(e.getTypes().get(0));
    }
  }

  private String getTypename(TypeRef type) throws Exception {
    if (type.getParams().size() == 1) {     
      if (type.isResourceReference())
        return "TFhirReference{"+getTypeName(type.getParams().get(0))+"}";
      else if (type.getName().equals("Interval"))
        return "TInterval_"+type.getParams().get(0);
      else
        throw new Exception("not supported: "+type.summary());
    } else if (type.getParams().size() > 1) {
      if (type.isResourceReference())
        return "TFhirReference{Resource}";
      else
        throw new Exception("not supported");
    } else {
      return getTypeName(type.getName());
    }
  }

  private String getTypeName(String tn) {
    if (tn == null) {
      return "";
    } else if (tn.equals("xml:lang")) {
      return "TFhirString";
    } else if (tn.equals("markdown")) {
      return "TFhirMarkdown";
    } else if (tn.equals("xhtml")) {
      return "TFhirXHtmlNode"; 
    } else if (tn.equals("*")) {
      return "TFhirType";
    } else if (tn.equals("Any")) {
      return "TFhirReference";
    } else {
      return "TFhir"+getTitle(tn);
    }
  }

  private void generatePrimitive(DefinedCode t, String parent, boolean isEnum, boolean derived) throws IOException {
    StringBuilder def = new StringBuilder();
    String tn = Utilities.capitalize(t.getCode());
    String pn = "String";
    if (tn.equals("Date") || tn.equals("DateTime") || tn.equals("Instant"))
      pn = "TDateAndTime";
    if (tn.equals("Boolean"))
      pn = "Boolean";
    if (tn.equals("Base64Binary"))
      pn = "TBytes";

    factoryIntf.append("    {@member new"+tn+"\r\n      create a new "+t.getCode()+"\r\n    }\r\n    {!script nolink}\r\n    function new"+tn+" : TFhir"+tn+";\r\n");
    factoryImpl.append("function TFhirResourceFactory.new"+tn+" : TFhir"+tn+";\r\nbegin\r\n  result := TFhir"+tn+".create;\r\nend;\r\n\r\n");
    factoryByName.append("  else if name = '"+tn+"' then\r\n    result := new"+tn+"()\r\n");

    factoryIntf.append("    {@member make"+tn+"\r\n      create a new "+t.getCode()+" with the given value\r\n    }\r\n    {!script nolink}\r\n    function make"+tn+"(value : "+pn+") : TFhir"+tn+";\r\n");
    factoryImpl.append("function TFhirResourceFactory.make"+tn+"(value : "+pn+") : TFhir"+tn+";\r\nbegin\r\n  result := TFhir"+tn+".create;\r\n  result.value := value;\r\nend;\r\n\r\n");

    if (!tn.equals("Enum")) {
      compJBase.append("  else if (base is TFhir"+tn+") then\r\n    compose"+tn+"Value(json, name, TFhir"+tn+"(base), false)\r\n");
      compXBase.append("  else if (base is TFhir"+tn+") then\r\n    compose"+tn+"(xml, name,  TFhir"+tn+"(base))\r\n");
      compRBase.append("  else if (base is TFhir"+tn+") then\r\n    compose"+tn+"(section, name,  TFhir"+tn+"(base), -1)\r\n");
    }
    
    simpleTypes.put("TFhir"+tn, pn);
    def.append("  {@Class TFhir"+tn+" : "+parent+"\r\n");
    def.append("    a complex "+tn+" - has an Id attribute, and extensions.\r\n");
    def.append("    \r\n");
    def.append("    Used where a FHIR element is a "+tn+", and extensions\r\n");
    def.append("  }\r\n");
    def.append("  TFhir"+tn+" = class ("+parent+")\r\n");
    types.add("TFhir"+tn);
    def.append("  Private\r\n");
    if (!derived) {
      def.append("    FValue: "+pn+";\r\n");
      if (tn.equals("Enum"))
        def.append("    FSystem: String;\r\n");
      
      def.append("    procedure setValue(value: "+pn+");\r\n");
      def.append("  protected\r\n");
      def.append("    Procedure GetChildrenByName(child_name : string; list : "+listForm("TFHIRObject")+"); override;\r\n");
      def.append("    Procedure ListProperties(oList : "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues : Boolean); Override;\r\n");
      def.append("    function AsStringValue : String; Override;\r\n");
    }
    def.append("  Public\r\n");
    if (tn.equals("Enum"))
      def.append("    constructor Create(system : String; value : "+pn+"); overload;\r\n");
    else
      def.append("    constructor Create(value : "+pn+"); overload;\r\n");
    def.append("    Destructor Destroy; override;\r\n");
    def.append("    \r\n");
    def.append("    {!script hide}\r\n");
    def.append("    Function Link : TFhir"+tn+"; Overload;\r\n");
    def.append("    Function Clone : TFhir"+tn+"; Overload;\r\n");
    if (!derived) {
      def.append("    procedure Assign(oSource : TAdvObject); override;\r\n");
      def.append("    function equalsDeep(other : TFHIRBase) : boolean; override;\r\n");
      def.append("    function equalsShallow(other : TFHIRBase) : boolean; override;\r\n");
    }
    def.append("    function fhirType : string; override;\r\n");
    def.append("    {!script show}\r\n");
    if (!derived) {
      def.append("  Published\r\n");
      def.append("    {@member value\r\n");
      def.append("      The actual value of the "+t.getCode()+"\r\n");
      def.append("    }\r\n");
      def.append("    property value : "+pn+" read FValue write SetValue;\r\n");
    }
    if (tn.equals("Enum"))
      def.append("    property system : String read FSystem write FSystem;\r\n");
    def.append("  End;    \r\n");
    def.append("\r\n");

    StringBuilder impl2 = new StringBuilder();
    impl2.append("{ TFhir"+tn+" }\r\n\r\n");

    if (tn.equals("Enum")) {
      impl2.append("Constructor TFhir"+tn+".Create(system : String; value : "+pn+");\r\n");
      impl2.append("begin\r\n");
      impl2.append("  Create;\r\n");
      impl2.append("  FSystem := system;\r\n");
      impl2.append("  FValue := value;\r\n");
      impl2.append("end;\r\n\r\n");  
    } else {
      impl2.append("Constructor TFhir"+tn+".Create(value : "+pn+");\r\n");
      impl2.append("begin\r\n");
      impl2.append("  Create;\r\n");
      impl2.append("  FValue := value;\r\n");
      if (tn.equals("Date"))
        impl2.append("  value.Precision := dtpDay;\r\n");
      impl2.append("end;\r\n\r\n");
    }

    impl2.append("Destructor TFhir"+tn+".Destroy;\r\n");
    impl2.append("begin\r\n");
    if (!derived) {
      if (!pn.equals("String") && !pn.equals("Boolean") && !pn.equals("TBytes"))
        impl2.append("  FValue.free;\r\n");
    }
    impl2.append("  inherited;\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function TFhir"+tn+".fhirType : string;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := '"+(t.getCode().equals("enum") ? "code" : t.getCode())+"';\r\n");
    impl2.append("end;\r\n\r\n");

    if (!derived) {

      impl2.append("procedure TFhir"+tn+".GetChildrenByName(child_name : string; list : "+listForm("TFHIRObject")+");\r\n");
      impl2.append("begin\r\n");
      impl2.append("  inherited;\r\n");
      impl2.append("  if child_name = 'value' then\r\n    list.add(TFHIRObjectText.create(value));\r\n");
      impl2.append("end;\r\n\r\n");
      impl2.append("procedure TFhir"+tn+".ListProperties(oList: "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues: Boolean);\r\n");
      impl2.append("begin\r\n");
      impl2.append("  inherited;\r\n");
      impl2.append("  if (bPrimitiveValues) then\r\n");
      if (pn.equals("Boolean"))
        impl2.append("    oList.add(TFHIRProperty.create(self, 'value', '"+breakConstant(t.getCode())+"', false, nil, LCBooleanToString(FValue)));\r\n");
      else if (!pn.equals("String") && !pn.equals("TBytes"))
        impl2.append("    if (FValue <> nil) then\r\n      oList.add(TFHIRProperty.create(self, 'value', '"+breakConstant(t.getCode())+"', false, nil, FValue.toString));\r\n");
      else 
        impl2.append("    oList.add(TFHIRProperty.create(self, 'value', '"+breakConstant(t.getCode())+"', false, nil, FValue));\r\n");
      impl2.append("end;\r\n\r\n");


      impl2.append("procedure TFhir"+tn+".Assign(oSource : TAdvObject);\r\n");
      impl2.append("begin\r\n");
      impl2.append("  inherited;\r\n");
      if (!pn.equals("String") && !pn.equals("Boolean") && !pn.equals("TBytes")) 
        impl2.append("  FValue := TFhir"+tn+"(oSource).Value.Link;\r\n");
      else 
        impl2.append("  FValue := TFhir"+tn+"(oSource).Value;\r\n");
      impl2.append("end;\r\n\r\n");

      impl2.append("function TFhir"+tn+".AsStringValue : string;\r\n");
      impl2.append("begin\r\n");
      if (pn.equals("Boolean"))
        impl2.append("  result := LCBooleanToString(FValue);\r\n");
      else if (pn.equals("TBytes"))
        impl2.append("  if (length(FValue) = 0) then result := '' else result := EncodeBase64(@FValue[0], length(FValue));\r\n");
      else if (tn.equals("DateTime") || tn.equals("Date") || tn.equals("Instant") ) {
        impl2.append("  if (FValue = nil) then\r\n");
        impl2.append("    result := ''\r\n");
        impl2.append("  else\r\n");
        impl2.append("    result := FValue.asXml;\r\n");
      } else if (!pn.equals("String")) {
        impl2.append("  if (FValue = nil) then\r\n");
        impl2.append("    result := ''\r\n");
        impl2.append("  else\r\n");
        impl2.append("    result := FValue.toString;\r\n");
      } else 
        impl2.append("  result := FValue;\r\n");
      impl2.append("end;\r\n\r\n");
      generatePrimitiveEquals(t, "TFhir"+tn, impl2);
    }

    impl2.append("function TFhir"+tn+".Link : TFhir"+tn+";\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := TFhir"+tn+"(inherited Link);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function TFhir"+tn+".Clone : TFhir"+tn+";\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := TFhir"+tn+"(inherited Clone);\r\n");
    impl2.append("end;\r\n\r\n");
    if (!derived) {
      impl2.append("procedure TFhir"+tn+".setValue(value : "+pn+");\r\n");
      impl2.append("begin\r\n");
      if (!pn.equals("String") && !pn.equals("Boolean") && !pn.equals("TBytes")) 
        impl2.append("  FValue.free;\r\n");
      impl2.append("  FValue := value;\r\n");
      impl2.append("end;\r\n\r\n");
    }    

    if (isEnum) {
      prsrdefX.append("    function Parse"+tn+"(Const aNames, aSystems : Array Of String; path : String; element : IXmlDomElement) : TFhir"+tn+";\r\n");
      prsrImpl.append("function TFHIRXmlParser.Parse"+tn+"(Const aNames, aSystems : Array Of String; path : String; element : IXmlDomElement) : TFhir"+tn+";\r\n");
      prsrImpl.append("var\r\n");
      prsrImpl.append("  child : IXMLDOMElement;\r\n");
      prsrImpl.append("  i : integer;\r\n");
      prsrImpl.append("begin\r\n");
      prsrImpl.append("  result := TFhir"+tn+".create;\r\n");
      prsrImpl.append("  try\r\n");
      prsrImpl.append("    ParseElementAttributes(result, path, element);\r\n");
      prsrImpl.append("    result.value := GetAttribute(element, 'value');\r\n");
      prsrImpl.append("    i := StringArrayIndexOfSensitive(aNames, result.value);\r\n");
      prsrImpl.append("    if i < 0 then\r\n");
      prsrImpl.append("      raise Exception.create('unknown code: '+result.value+' from a set of choices of '+StringArrayToCommaString(aNames)+' for \"'+path+'\"');\r\n");
      prsrImpl.append("    result.system := aSystems[i];\r\n");
      prsrImpl.append("    child := FirstChild(element);\r\n");
      prsrImpl.append("    while (child <> nil) do\r\n");
      prsrImpl.append("    begin\r\n");
      prsrImpl.append("      if Not ParseElementChild(result, path, child) then\r\n");
      prsrImpl.append("         UnknownContent(child, path);\r\n");
      prsrImpl.append("      child := NextSibling(child);\r\n");
      prsrImpl.append("    end;\r\n");
      prsrImpl.append("    closeOutElement(result, element);\r\n\r\n");
      prsrImpl.append("    result.link;\r\n");
      prsrImpl.append("  finally\r\n");
      prsrImpl.append("    result.free;\r\n");
      prsrImpl.append("  end;\r\n");
      prsrImpl.append("end;\r\n\r\n");

      prsrdefJ.append("    procedure ParseEnum(path, value : string; jsn : TJsonObject; ctxt : "+listForm("TFHIRObject")+"; Const aNames, aSystems : Array Of String); overload;\r\n");
      prsrImpl.append("procedure TFHIRJsonParser.ParseEnum(path, value : string; jsn : TJsonObject; ctxt : "+listForm("TFHIRObject")+"; Const aNames, aSystems : Array Of String);\r\n");
      prsrImpl.append("begin\r\n");
      prsrImpl.append("  ctxt.add(ParseEnum(path, value, jsn, aNames, aSystems));\r\n");
      prsrImpl.append("end;\r\n\r\n");
      prsrdefJ.append("    function ParseEnum(path, value : string; jsn : TJsonObject; Const aNames, aSystems : Array Of String) : TFHIREnum; overload;\r\n");
      prsrImpl.append("function TFHIRJsonParser.ParseEnum(path, value : string; jsn : TJsonObject; Const aNames, aSystems : Array Of String) : TFHIREnum;\r\n");
      prsrImpl.append("var\r\n");
      prsrImpl.append("  i : integer;\r\n");
      prsrImpl.append("begin\r\n");
      prsrImpl.append("  i := StringArrayIndexOfSensitive(aNames, value);\r\n");
      prsrImpl.append("  if (value <> '') and (i < 0) then\r\n");
      prsrImpl.append("    raise Exception.create('unknown code: '+value+' from a set of choices of '+StringArrayToCommaString(aNames)+' for \"'+path+'\"');\r\n");
      prsrImpl.append("  result := TFHIREnum.create;\r\n");
      prsrImpl.append("  try\r\n");
      prsrImpl.append("    result.value := value;\r\n");
      prsrImpl.append("    result.system := aSystems[i];\r\n");
      prsrImpl.append("    if (jsn <> nil) then\r\n");
      prsrImpl.append("      parseElementProperties(jsn, result);\r\n");
      prsrImpl.append("    result.link;\r\n");
      prsrImpl.append("  finally\r\n");
      prsrImpl.append("    result.free;\r\n");
      prsrImpl.append("  end;\r\n");
      prsrImpl.append("end;\r\n\r\n");


      srlsdefX.append("    Procedure Compose"+tn+"(xml : TXmlBuilder; name : String; value : TFhir"+tn+"; Const aNames : Array Of String);\r\n");
      prsrImpl.append("Procedure TFHIRXmlComposer.Compose"+tn+"(xml : TXmlBuilder; name : String; value : TFhir"+tn+"; Const aNames : Array Of String);\r\n");
      prsrImpl.append("begin\r\n");
      prsrImpl.append("  if (value = nil) then\r\n");
      prsrImpl.append("    exit;\r\n");
      prsrImpl.append("  composeElementAttributes(xml, value);\r\n");
      prsrImpl.append("  attribute(xml, 'value', value.value);\r\n");
      prsrImpl.append("  xml.open(name);\r\n");
      prsrImpl.append("  composeElementChildren(xml, value);\r\n");
      prsrImpl.append("  closeOutElement(xml, value);\r\n");
      prsrImpl.append("  xml.close(name);\r\n");
      prsrImpl.append("end;\r\n\r\n");
      srlsdefJ.append("    Procedure Compose"+tn+"Value(json : TJSONWriter; name : String; value : TFhir"+tn+"; Const aNames : Array Of String; inArray : boolean);\r\n");
      srlsdefJ.append("    Procedure Compose"+tn+"Props(json : TJSONWriter; name : String; value : TFhir"+tn+"; Const aNames : Array Of String; inArray : boolean);\r\n");
      prsrImpl.append("Procedure TFHIRJsonComposer.Compose"+tn+"Value(json : TJSONWriter; name : String; value : TFhir"+tn+"; Const aNames : Array Of String; inArray : boolean);\r\n");
      prsrImpl.append("begin\r\n");
      prsrImpl.append("  if (value = nil) or (value.Value = '') then\r\n");
      prsrImpl.append("  begin\r\n");
      prsrImpl.append("    if inArray then\r\n");
      prsrImpl.append("      propNull(json, name);\r\n");
      prsrImpl.append("    exit;\r\n");
      prsrImpl.append("  end\r\n");
      prsrImpl.append("  else\r\n");
      prsrImpl.append("    prop(json, name, value.value);\r\n");
      prsrImpl.append("end;\r\n\r\n");
      prsrImpl.append("Procedure TFHIRJsonComposer.Compose"+tn+"Props(json : TJSONWriter; name : String; value : TFhir"+tn+"; Const aNames : Array Of String; inArray : boolean);\r\n");
      prsrImpl.append("begin\r\n");
      prsrImpl.append("  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments) }) then\r\n");
      prsrImpl.append("  begin\r\n");
      prsrImpl.append("    if inArray then\r\n");
      prsrImpl.append("      propNull(json, name);\r\n");
      prsrImpl.append("    exit;\r\n");
      prsrImpl.append("  end\r\n");
      prsrImpl.append("  else\r\n");
      prsrImpl.append("  begin\r\n");
      prsrImpl.append("    if (inArray) then\r\n");
      prsrImpl.append("      json.valueObject('')\r\n");
      prsrImpl.append("    else\r\n");
      prsrImpl.append("      json.valueObject('_'+name);\r\n");
      prsrImpl.append("    ComposeElementProperties(json, value);\r\n");
      prsrImpl.append("    json.finishObject;\r\n");
      prsrImpl.append("  end;\r\n");
      prsrImpl.append("end;\r\n\r\n");

      srlsdefR.append("    Procedure Compose"+tn+"(parent :  TRDFComplex; parentType, name : String; value : TFhir"+tn+"; Const aNames, aSystems : Array Of String; index : integer);\r\n");
      prsrImpl.append("Procedure TFHIRRDFComposer.Compose"+tn+"(parent :  TRDFComplex; parentType, name : String; value : TFhir"+tn+"; Const aNames, aSystems : Array Of String; index : integer);\r\n");
      prsrImpl.append("var\r\n");
      prsrImpl.append("  this, c, cc, cs : TRDFComplex;\r\n");
      prsrImpl.append("begin\r\n");
      prsrImpl.append("  if (value = nil) then\r\n");
      prsrImpl.append("    exit;\r\n");
      prsrImpl.append("  this := parent.predicate('fhir:'+parentType+'.'+name);\r\n");
      prsrImpl.append("  this.predicate('a', 'fhir:code');\r\n");
      prsrImpl.append("  this.predicate('a', 'fhir:ConceptBase');\r\n");
      prsrImpl.append("  c := this.predicate('fhir:ConceptBase.coding');\r\n");
      prsrImpl.append("  c.predicate('a', 'fhir:CodingBase');\r\n");
      prsrImpl.append("  cc := c.predicate('fhir:CodingBase.code');\r\n");
      prsrImpl.append("  cc.predicate('a', 'fhir:codeBase');\r\n");
      prsrImpl.append("  cc.predicate('fhir:value', ttlLiteral(value.value));\r\n");
      prsrImpl.append("  cs := c.predicate('fhir:CodingBase.system');\r\n");
      prsrImpl.append("  cs.predicate('a', 'fhir:string');\r\n");
      prsrImpl.append("  cs.predicate('fhir:value', ttlLiteral(aSystems[StringArrayIndexOf(aNames, value.value)]));\r\n");
      prsrImpl.append("  composeElement(this, parentType, name, value, index);\r\n");
      prsrImpl.append("end;\r\n\r\n");
    } else {
      prsrdefX.append("    function Parse"+tn+"(element : IXmlDomElement; path : string) : TFhir"+tn+";\r\n");
      prsrImpl.append("function TFHIRXmlParser.Parse"+tn+"(element : IXmlDomElement; path : string) : TFhir"+tn+";\r\n");
      prsrImpl.append("var\r\n");
      prsrImpl.append("  child : IXMLDOMElement;\r\n");
      prsrImpl.append("begin\r\n");
      prsrImpl.append("  result := TFhir"+tn+".create;\r\n");
      prsrImpl.append("  try\r\n");
      prsrImpl.append("    ParseElementAttributes(result, path, element);\r\n");
      if (pn.equals("Boolean"))
        prsrImpl.append("    result.value := StringToBoolean(GetAttribute(element, 'value'));\r\n");
      else  if (!pn.equals("String"))
        prsrImpl.append("    result.value := to"+pn+"(GetAttribute(element, 'value'));\r\n");
      else
        prsrImpl.append("    result.value := GetAttribute(element, 'value');\r\n");
      prsrImpl.append("    child := FirstChild(element);\r\n");
      prsrImpl.append("    while (child <> nil) do\r\n");
      prsrImpl.append("    begin\r\n");
      prsrImpl.append("      if Not ParseElementChild(result, path, child) then\r\n");
      prsrImpl.append("         UnknownContent(child, path);\r\n");
      prsrImpl.append("      child := NextSibling(child);\r\n");
      prsrImpl.append("    end;\r\n");
      prsrImpl.append("    closeOutElement(result, element);\r\n\r\n");
      prsrImpl.append("    result.link;\r\n");
      prsrImpl.append("  finally\r\n");
      prsrImpl.append("    result.free;\r\n");
      prsrImpl.append("  end;\r\n");
      prsrImpl.append("end;\r\n\r\n");


      prsrdefJ.append("    procedure Parse"+tn+"(value : string; jsn : TJsonObject; ctxt : "+listForm("TFHIRObject")+"); overload;\r\n");
      prsrImpl.append("procedure TFHIRJsonParser.Parse"+tn+"(value : string; jsn : TJsonObject; ctxt : "+listForm("TFHIRObject")+");\r\n");
      prsrImpl.append("begin\r\n");
      prsrImpl.append("  ctxt.add(Parse"+tn+"(value, jsn)) {1};\r\n");
      prsrImpl.append("end;\r\n\r\n");
      prsrdefJ.append("    function Parse"+tn+"(value : string; jsn : TJsonObject) : TFHIR"+tn+"; overload;\r\n");
      prsrImpl.append("function TFHIRJsonParser.Parse"+tn+"(value : string; jsn : TJsonObject) : TFHIR"+tn+";\r\n");
      prsrImpl.append("begin\r\n");
      prsrImpl.append("  result := TFhir"+tn+".Create;\r\n");
      prsrImpl.append("  try\r\n");
      if (pn.equals("Boolean"))
        prsrImpl.append("    result.value := StringToBoolean(value);\r\n");
      else if (!pn.equals("String"))
        prsrImpl.append("     result.value := to"+pn+"(value);\r\n");
      else
        prsrImpl.append("    result.value := value;\r\n");
      prsrImpl.append("    if (jsn <> nil) then\r\n");
      prsrImpl.append("      parseElementProperties(jsn, result);\r\n");
      prsrImpl.append("    result.Link;\r\n");
      prsrImpl.append("  finally\r\n");
      prsrImpl.append("    result.Free;\r\n");
      prsrImpl.append("  end;\r\n");
      prsrImpl.append("end;\r\n\r\n");








      srlsdefX.append("    Procedure Compose"+tn+"(xml : TXmlBuilder; name : String; value : TFhir"+tn+");\r\n");
      prsrImpl.append("Procedure TFHIRXmlComposer.Compose"+tn+"(xml : TXmlBuilder; name : String; value : TFhir"+tn+");\r\n");
      prsrImpl.append("begin\r\n");
      if (pn.equals("Boolean"))
        prsrImpl.append("  if (value = nil) then\r\n");
      else if (!pn.equals("String"))
        prsrImpl.append("  if (value = nil) or ((value.value = nil) and (value.extensionList.count = 0)) then\r\n");
      else 
        prsrImpl.append("  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then\r\n");
      prsrImpl.append("    exit;\r\n");
      prsrImpl.append("  composeElementAttributes(xml, value);\r\n");
      if (pn.equals("Boolean")) {
        prsrImpl.append("  attribute(xml, 'value', LCBooleanToString(value.value));\r\n");
      } else if (!pn.equals("String")) {
        prsrImpl.append("  if (value.value <> nil) then\r\n");
        prsrImpl.append("    attribute(xml, 'value', asString(value.value));\r\n");
      } else
        prsrImpl.append("  attribute(xml, 'value', value.value);\r\n");
      prsrImpl.append("  xml.open(name);\r\n");
      prsrImpl.append("  composeElementChildren(xml, value);\r\n");
      prsrImpl.append("  closeOutElement(xml, value);\r\n");
      prsrImpl.append("  xml.close(name);\r\n");
      prsrImpl.append("end;\r\n\r\n");
      srlsdefJ.append("    Procedure Compose"+tn+"Value(json : TJSONWriter; name : String; value : TFhir"+tn+"; inArray : boolean);\r\n");
      srlsdefJ.append("    Procedure Compose"+tn+"Props(json : TJSONWriter; name : String; value : TFhir"+tn+"; inArray : boolean);\r\n");
      prsrImpl.append("Procedure TFHIRJsonComposer.Compose"+tn+"Value(json : TJSONWriter; name : String; value : TFhir"+tn+"; inArray : boolean);\r\n");
      prsrImpl.append("begin\r\n");
      if (pn.equals("Boolean"))
        prsrImpl.append("  if (value = nil) then\r\n");
      else if (!pn.equals("String"))
        prsrImpl.append("  if (value = nil) or (value.value = nil) then\r\n");
      else 
        prsrImpl.append("  if (value = nil) or (value.value = '') then\r\n");
      prsrImpl.append("  begin\r\n");
      prsrImpl.append("    if inArray then\r\n");
      prsrImpl.append("      propNull(json, name);\r\n");
      prsrImpl.append("    exit;\r\n");
      prsrImpl.append("  end\r\n");
      prsrImpl.append("  else\r\n");
      if (pn.equals("Boolean"))
        prsrImpl.append("    prop(json, name, value.value);\r\n");
      else if (!pn.equals("String"))
        prsrImpl.append("    prop(json, name, asString(value.value));\r\n");
      else if (tn.equals("Decimal") || tn.equals("Integer") || tn.equals("UnsignedInt")  || tn.equals("PositiveInt"))
        prsrImpl.append("    propNumber(json, name, value.value);\r\n");
      else
        prsrImpl.append("    prop(json, name, value.value);\r\n");
      prsrImpl.append("end;\r\n\r\n");
      prsrImpl.append("Procedure TFHIRJsonComposer.Compose"+tn+"Props(json : TJSONWriter; name : String; value : TFhir"+tn+"; inArray : boolean);\r\n");
      prsrImpl.append("begin\r\n");
      prsrImpl.append("  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then\r\n");
      prsrImpl.append("  begin\r\n");
      prsrImpl.append("    if inArray then\r\n");
      prsrImpl.append("      propNull(json, name);\r\n");
      prsrImpl.append("    exit;\r\n");
      prsrImpl.append("  end\r\n");
      prsrImpl.append("  else\r\n");
      prsrImpl.append("  begin\r\n");
      prsrImpl.append("    if (inArray) then\r\n");
      prsrImpl.append("      json.valueObject('')\r\n");
      prsrImpl.append("    else\r\n");
      prsrImpl.append("      json.valueObject('_'+name);\r\n");
      prsrImpl.append("    ComposeElementProperties(json, value);\r\n");
      prsrImpl.append("    json.finishObject;\r\n");
      prsrImpl.append("  end;\r\n");
      prsrImpl.append("end;\r\n\r\n");

      srlsdefR.append("    Procedure Compose"+tn+"(parent :  TRDFComplex; parentType, name : String; value : TFhir"+tn+"; index : integer);\r\n");
      prsrImpl.append("Procedure TFHIRRDFComposer.Compose"+tn+"(parent :  TRDFComplex; parentType, name : String; value : TFhir"+tn+"; index : integer);\r\n");
      prsrImpl.append("var\r\n");
      prsrImpl.append("  this : TRDFComplex;\r\n");
      prsrImpl.append("begin\r\n");
      prsrImpl.append("  if (value = nil) then\r\n");
      prsrImpl.append("    exit;\r\n");
      prsrImpl.append("  this := parent.predicate('fhir:'+parentType+'.'+name);\r\n");
      prsrImpl.append("  this.predicate('a', 'fhir:"+t.getCode()+"');\r\n");
      prsrImpl.append("  this.predicate('fhir:value', ttlLiteral(asString(value.value)));\r\n");
      prsrImpl.append("  composeElement(this, parentType, name, value, index);\r\n");
      prsrImpl.append("end;\r\n\r\n");
    }

    defCodeType.classDefs.add(def.toString());
    defCodeType.classImpls.add(impl2.toString());
    defCodeType.classFwds.add("  TFhir"+tn+" = class;\r\n");
    defineList("TFhir"+tn, "TFhir"+tn+"List", pn, ClassCategory.Type, false, isEnum);
  }

  private String listForm(String name) {
    return generics ? "TAdvList<"+name+">" : name+"List";
  }

  private void generateElement() {
    StringBuilder def = new StringBuilder();

//    def.append("  {@Class TFhirElement : TFHIRBase\r\n");
//    def.append("    Base Element Definition - extensions, ids\r\n");
//    def.append("  }\r\n");
//    def.append("  TFhirElement = {abstract} class (TFHIRBase)\r\n");
//    types.add("TFhirElement");
//    def.append("  private\r\n");
//    def.append("    FXmlId: String;\r\n");
//    def.append("    FExtensionList : "+listForm("TFhirExtension")+";\r\n");
//    def.append("    function GetExtensionList: "+listForm("TFhirExtension")+";\r\n");
//    def.append("  protected\r\n");
//    def.append("    Procedure GetChildrenByName(child_name : string; list : "+listForm("TFHIRObject")+"); override;\r\n");
//    def.append("    Procedure ListProperties(oList : "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues : Boolean); Override;\r\n");
//    def.append("  public\r\n");
//    def.append("    destructor Destroy; override;\r\n");
//    def.append("    {!script hide}\r\n");
//    def.append("    procedure Assign(oSource : TAdvObject); override;\r\n");
//    def.append("    function Link : TFhirElement; overload;\r\n");
//    def.append("    function Clone : TFhirElement; overload;\r\n");
//    def.append("    function HasExtensions : Boolean;\r\n");
//    def.append("    {!script show}\r\n");
//    def.append("  published\r\n");
//    def.append("    {@member xmlId\r\n");
//    def.append("      the value of the xml id attribute, if present.\r\n");
//    def.append("    }\r\n");
//    def.append("    property xmlId : String read FXmlId write FXmlId;\r\n");
//    def.append("    {@member ExtensionList\r\n");
//    def.append("      Extensions on this value\r\n");
//    def.append("    }\r\n");
//    def.append("    property ExtensionList : "+listForm("TFhirExtension")+" read GetExtensionList;\r\n");
//    def.append("  end;\r\n");
//    def.append("  \r\n");
//    def.append("\r\n");
    def.append("  {@Class TFhirType : TFhirElement\r\n");
    def.append("    A base FHIR type - (polymorphism support)\r\n");
    def.append("  }\r\n");
    def.append("  TFhirType = class (TFhirElement)\r\n");
    types.add("TFhirType");
    def.append("  Public\r\n");
    def.append("    {!script hide}\r\n");
    def.append("    Function Link : TFhirType; Overload;\r\n");
    def.append("    Function Clone : TFhirType; Overload;\r\n");
    def.append("    {!script show}\r\n");
    def.append("  End;\r\n");   
    def.append("  TFHIRTypeClass = class of TFhirType;\r\n");
    def.append("  \r\n");
    def.append("  {@Class TFHIRPrimitiveType : TFhirType\r\n");
    def.append("    A base FHIR type - (polymorphism support)\r\n");
    def.append("  }\r\n");
    def.append("  TFHIRPrimitiveType = class (TFhirType)\r\n");
    types.add("TFHIRPrimitiveType");
    def.append("  Private\r\n");
    def.append("    Function GetStringValue : String;\r\n");
    def.append("    Function AsStringValue : String; Virtual;\r\n");
    def.append("  Public\r\n");
    def.append("    {!script hide}\r\n");
    def.append("    Function Link : TFHIRPrimitiveType; Overload;\r\n");
    def.append("    Function Clone : TFHIRPrimitiveType; Overload;\r\n");
    def.append("    Property StringValue : String read GetStringValue;\r\n");
    def.append("    function isPrimitive : boolean; override;\r\n");
    def.append("    function hasPrimitiveValue : boolean; override;\r\n");
    def.append("    function primitiveValue : string; override;\r\n");
    def.append("    {!script show}\r\n");
    def.append("  End;\r\n");   
    def.append("  TFHIRPrimitiveTypeClass = class of TFHIRPrimitiveType;\r\n");
    def.append("  \r\n");

    StringBuilder impl2 = new StringBuilder();
//    impl2.append("{ TFhirElement }\r\n\r\n");
//
//    impl2.append("destructor TFhirElement.Destroy;\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  FExtensionList.Free;\r\n");
//    impl2.append("  inherited;\r\n");
//    impl2.append("end;\r\n\r\n");
//
//    impl2.append("procedure TFhirElement.GetChildrenByName(child_name : string; list : "+listForm("TFHIRObject")+");\r\n");
//    if (generics)
//      impl2.append("var\r\n  o : TFHIRExtension;\r\n");      
//    impl2.append("begin\r\n");
//    impl2.append("  inherited;\r\n");
//    impl2.append("  if child_name = '@id' then\r\n    list.add(TFHIRObjectText.create(FXmlId));\r\n");
//    if (generics)
//      impl2.append("  if (child_name = 'extension') Then\r\n    for o in FExtensionList do\r\n      list.add(o.Link);\r\n");
//    else
//      impl2.append("  if (child_name = 'extension') Then\r\n    list.addAll(FExtensionList);\r\n");
//    impl2.append("end;\r\n\r\n");
//    impl2.append("procedure TFhirElement.ListProperties(oList: "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues: Boolean);\r\n");
//    if (generics) {
//      impl2.append("var\r\n");
//      impl2.append("  prop : TFHIRProperty;\r\n");
//      impl2.append("  o : TFHIRExtension;\r\n");
//    }
//    impl2.append("begin\r\n");
//    impl2.append("  inherited;\r\n");
//    impl2.append("  oList.add(TFHIRProperty.create(self, 'xml:id', 'string', FXmlId));\r\n");
//    if (generics) {
//      impl2.append("  prop := oList[oList.add(TFHIRProperty.create(self, 'extension', 'Extension'))];\r\n");
//      impl2.append("  if FExtensionList <> nil then;\r\n");
//      impl2.append("    for o in FExtensionList do\r\n");
//      impl2.append("      prop.List.add(o.Link);\r\n");
//    } else 
//      impl2.append("  oList.add(TFHIRProperty.create(self, 'extension', 'Extension', FExtensionList));\r\n");
//    impl2.append("end;\r\n\r\n");
//
//
//    impl2.append("procedure TFhirElement.Assign(oSource : TAdvObject);\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  inherited;\r\n");
//    impl2.append("  FXmlId := TFhirElement(oSource).FXmlId;\r\n");
//    impl2.append("  if TFhirElement(oSource).HasExtensions then\r\n    extensionList.assign(TFhirElement(oSource).extensionList)\r\n"+
//        "  else if FExtensionList <> nil then\r\n  begin\r\n    FExtensionList.free;\r\n    FExtensionList := nil;\r\n  end;\r\n");
//    impl2.append("end;\r\n\r\n");
//
//    impl2.append("function TFhirElement.Link : TFhirElement;\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  result := TFhirElement(inherited Link);\r\n");
//    impl2.append("end;\r\n\r\n");
//    impl2.append("function TFhirElement.Clone : TFhirElement;\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  result := TFhirElement(inherited Clone);\r\n");
//    impl2.append("end;\r\n\r\n");
//    impl2.append("function TFhirElement.GetExtensionList : "+listForm("TFhirExtension")+";\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  if FExtensionList = nil then\r\n    FExtensionList := "+listForm("TFhirExtension")+".Create;\r\n  result := FExtensionList;\r\n");
//    impl2.append("end;\r\n\r\n");
//    impl2.append("function TFhirElement.HasExtensions : boolean;\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  result := (FExtensionList <> nil) and (FExtensionList.count > 0);\r\n");
//    impl2.append("end;\r\n\r\n");

//    def.append("  {@Class TFhirBackboneElement : TFHIRBase\r\n");
//    def.append("    Base Element Definition - extensions, ids\r\n");
//    def.append("  }\r\n");
//    def.append("  TFHIRBackboneElement = {abstract} class (TFhirElement)\r\n");
//    types.add("TFHIRBackboneElement");
//    def.append("  private\r\n");
//    def.append("    FModifierExtensionList : "+listForm("TFhirExtension")+";\r\n");
//    def.append("    function GetModifierExtensionList: "+listForm("TFhirExtension")+";\r\n");
//    def.append("  protected\r\n");
//    def.append("    Procedure GetChildrenByName(child_name : string; list : "+listForm("TFHIRObject")+"); override;\r\n");
//    def.append("    Procedure ListProperties(oList : "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues : Boolean); Override;\r\n");
//    def.append("  public\r\n");
//    def.append("    destructor Destroy; override;\r\n");
//    def.append("    {!script hide}\r\n");
//    def.append("    procedure Assign(oSource : TAdvObject); override;\r\n");
//    def.append("    function Link : TFHIRBackboneElement; overload;\r\n");
//    def.append("    function Clone : TFHIRBackboneElement; overload;\r\n");
//    def.append("    function HasModifierExtensions : Boolean;\r\n");
//    def.append("    {!script show}\r\n");
//    def.append("  published\r\n");
//    def.append("    {@member ModifierExtensionList\r\n");
//    def.append("      Modifier Extensions on this value\r\n");
//    def.append("    }\r\n");
//    def.append("    property ModifierExtensionList : "+listForm("TFhirExtension")+" read GetModifierExtensionList;\r\n");
//    def.append("  end;\r\n");
//    def.append("  \r\n");
//    def.append("\r\n");
//
//    impl2.append("{ TFHIRBackboneElement }\r\n\r\n");
//
//    impl2.append("destructor TFHIRBackboneElement.Destroy;\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  FModifierExtensionList.Free;\r\n");
//    impl2.append("  inherited;\r\n");
//    impl2.append("end;\r\n\r\n");
//
//    impl2.append("procedure TFHIRBackboneElement.GetChildrenByName(child_name : string; list : "+listForm("TFHIRObject")+");\r\n");
//    if (generics)
//      impl2.append("var\r\n  o : TFHIRExtension;\r\n");      
//    impl2.append("begin\r\n");
//    impl2.append("  inherited;\r\n");
//    if (generics)
//      impl2.append("  if (child_name = 'modifierExtension') Then\r\n    for o in FModifierExtensionList do\r\n      list.add(o.Link);\r\n");
//    else
//      impl2.append("  if (child_name = 'modifierExtension') Then\r\n    list.addAll(FModifierExtensionList);\r\n");
//    impl2.append("end;\r\n\r\n");
//    impl2.append("procedure TFHIRBackboneElement.ListProperties(oList: "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues: Boolean);\r\n");
//    if (generics) {
//      impl2.append("var\r\n");
//      impl2.append("  prop : TFHIRProperty;\r\n");
//      impl2.append("  o : TFHIRExtension;\r\n");
//    }
//    impl2.append("begin\r\n");
//    impl2.append("  inherited;\r\n");
//    if (generics) {
//      impl2.append("  prop := oList[oList.add(TFHIRProperty.create(self, 'modifierExtension', 'Extension'))];\r\n");
//      impl2.append("  if FModifierExtensionList <> nil then;\r\n");
//      impl2.append("    for o in FModifierExtensionList do\r\n");
//      impl2.append("      prop.List.add(o.Link);\r\n");
//    } else
//      impl2.append("  oList.add(TFHIRProperty.create(self, 'modifierExtension', 'Extension', FModifierExtensionList));\r\n");
//    impl2.append("end;\r\n\r\n");
//
//    impl2.append("procedure TFHIRBackboneElement.Assign(oSource : TAdvObject);\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  inherited;\r\n");
//    impl2.append("  if TFHIRBackboneElement(oSource).HasModifierExtensions then\r\n    ModifierExtensionList.assign(TFHIRBackboneElement(oSource).ModifierextensionList)\r\n"+
//        "  else if FModifierExtensionList <> nil then\r\n  begin\r\n    FModifierExtensionList.free;\r\n    FModifierExtensionList := nil;\r\n  end;\r\n");
//    impl2.append("end;\r\n\r\n");
//
//    impl2.append("function TFHIRBackboneElement.Link : TFHIRBackboneElement;\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  result := TFHIRBackboneElement(inherited Link);\r\n");
//    impl2.append("end;\r\n\r\n");
//    impl2.append("function TFHIRBackboneElement.Clone : TFHIRBackboneElement;\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  result := TFHIRBackboneElement(inherited Clone);\r\n");
//    impl2.append("end;\r\n\r\n");
//    impl2.append("function TFHIRBackboneElement.GetModifierExtensionList : "+listForm("TFhirExtension")+";\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  if FModifierExtensionList = nil then\r\n    FModifierExtensionList := "+listForm("TFhirExtension")+".Create;\r\n  result := FModifierExtensionList;\r\n");
//    impl2.append("end;\r\n\r\n");
//    impl2.append("function TFHIRBackboneElement.HasModifierExtensions : boolean;\r\n");
//    impl2.append("begin\r\n");
//    impl2.append("  result := (FModifierExtensionList <> nil) and (FModifierExtensionList.count > 0);\r\n");
//    impl2.append("end;\r\n\r\n");

    impl2.append("{ TFhirType }\r\n\r\n");
    impl2.append("function TFhirType.Link : TFhirType;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := TFhirType(inherited Link);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function TFhirType.Clone : TFhirType;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := TFhirType(inherited Clone);\r\n");
    impl2.append("end;\r\n\r\n");

    impl2.append("{ TFHIRPrimitiveType }\r\n\r\n");
    impl2.append("function TFHIRPrimitiveType.Link : TFHIRPrimitiveType;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := TFHIRPrimitiveType(inherited Link);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function TFHIRPrimitiveType.Clone : TFHIRPrimitiveType;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := TFHIRPrimitiveType(inherited Clone);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function TFHIRPrimitiveType.GetStringValue : string;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  if self = nil then\r\n");
    impl2.append("    result := ''\r\n");
    impl2.append("  else\r\n");
    impl2.append("    result := AsStringValue;\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function TFHIRPrimitiveType.AsStringValue : string;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  raise Exception.create('need to override '+ClassName+'.AsStringValue');\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function TFHIRPrimitiveType.isPrimitive: boolean;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := true;\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function TFHIRPrimitiveType.hasPrimitiveValue: boolean;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := StringValue <> '';\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function TFHIRPrimitiveType.primitiveValue: string;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := StringValue;\r\n");
    impl2.append("end;\r\n\r\n");

    prsrdefX.append("    Procedure ParseElementAttributes(value : TFhirElement; path : string; element : IXmlDomElement);\r\n");
    prsrImpl.append("Procedure TFHIRXmlParser.ParseElementAttributes(value : TFhirElement; path : string; element : IXmlDomElement);\r\n");
    prsrImpl.append("begin\r\n");
    prsrImpl.append("  TakeCommentsStart(value);\r\n");
    prsrImpl.append("  GetObjectLocation(value, element);\r\n");
    prsrImpl.append("  value.Id := GetAttribute(element, 'id');\r\n");
    prsrImpl.append("end;\r\n\r\n");

    prsrdefX.append("    Function ParseBackboneElementChild(element : TFhirBackboneElement; path : string; child : IXmlDomElement) : boolean;\r\n");
    prsrImpl.append("Function TFHIRXmlParser.ParseBackboneElementChild(element : TFhirBackboneElement; path : string; child : IXmlDomElement) : boolean;\r\n");
    prsrImpl.append("begin\r\n");
    prsrImpl.append("  result := true;\r\n");
    prsrImpl.append("  if (child.baseName = 'modifierExtension') then\r\n");
    prsrImpl.append("    element.ModifierExtensionList.add(ParseExtension(child, path+'/modifierExtension'))\r\n");
    prsrImpl.append("  else\r\n");
    prsrImpl.append("    result := ParseElementChild(element, path, child);\r\n");
    prsrImpl.append("end;\r\n\r\n");

    prsrdefX.append("    Function ParseElementChild(element : TFhirElement; path : string; child : IXmlDomElement) : boolean;\r\n");
    prsrImpl.append("Function TFHIRXmlParser.ParseElementChild(element : TFhirElement; path : string; child : IXmlDomElement) : boolean;\r\n");
    prsrImpl.append("begin\r\n");
    prsrImpl.append("  result := true;\r\n");
    prsrImpl.append("  if (child.baseName = 'extension') then\r\n");
    prsrImpl.append("    element.ExtensionList.add(ParseExtension(child, path+'/extension'))\r\n");
    prsrImpl.append("  else\r\n");
    prsrImpl.append("    result := false;\r\n");
    prsrImpl.append("end;\r\n\r\n");

    prsrdefJ.append("    procedure ParseElementProperties(jsn : TJsonObject; element : TFhirElement);\r\n");
    prsrImpl.append("procedure TFHIRJsonParser.ParseElementProperties(jsn : TJsonObject; element : TFhirElement);\r\n");
    prsrImpl.append("begin\r\n");
    prsrImpl.append("  parseComments(element, jsn);\r\n\r\n");
    prsrImpl.append("  element.LocationStart := jsn.LocationStart;\r\n");
    prsrImpl.append("  element.LocationEnd := jsn.LocationEnd;\r\n");
    prsrImpl.append("  if jsn.has('id') then\r\n");
    prsrImpl.append("    element.Id := jsn['id']\r\n");
    prsrImpl.append("  else if jsn.has('_id') then\r\n");
    prsrImpl.append("    element.Id := jsn['_id'];\r\n");
    prsrImpl.append("  if jsn.has('extension') then\r\n");
    prsrImpl.append("    iterateArray(jsn.vArr['extension'], element.extensionList, parseExtension);\r\n");
    prsrImpl.append("end;\r\n\r\n");

    prsrdefJ.append("    procedure ParseBackboneElementProperties(jsn : TJsonObject; element : TFhirBackboneElement);\r\n");
    prsrImpl.append("procedure TFHIRJsonParser.ParseBackboneElementProperties(jsn : TJsonObject; element : TFhirBackboneElement);\r\n");
    prsrImpl.append("begin\r\n");
    prsrImpl.append("  parseElementProperties(jsn, element);\r\n\r\n");
    prsrImpl.append("  if jsn.has('modifierExtension') then\r\n");
    prsrImpl.append("    iterateArray(jsn.vArr['modifierExtension'], element.modifierExtensionList, parseExtension);\r\n");
    prsrImpl.append("end;\r\n\r\n");

    srlsdefX.append("    Procedure ComposeElementAttributes(xml : TXmlBuilder; element : TFhirElement);\r\n");
    prsrImpl.append("Procedure TFHIRXmlComposer.ComposeElementAttributes(xml : TXmlBuilder; element : TFhirElement);\r\n");
    prsrImpl.append("begin\r\n");
    prsrImpl.append("  CommentsStart(xml, element);\r\n");
    prsrImpl.append("  Attribute(xml, 'id', element.Id);\r\n");
    prsrImpl.append("end;\r\n\r\n");
    srlsdefX.append("    Procedure ComposeElementChildren(xml : TXmlBuilder; element : TFhirElement);\r\n");
    prsrImpl.append("Procedure TFHIRXmlComposer.ComposeElementChildren(xml : TXmlBuilder; element : TFhirElement);\r\n");
    prsrImpl.append("var\r\n");
    prsrImpl.append("  i : integer;\r\n");
    prsrImpl.append("begin\r\n");
    prsrImpl.append("  if element.hasExtensionList then\r\n");
    prsrImpl.append("    for i := 0 to element.extensionList.count - 1 do\r\n");
    prsrImpl.append("      ComposeExtension(xml, 'extension', element.extensionList[i]);\r\n");
    prsrImpl.append("end;\r\n\r\n");
    srlsdefX.append("    Procedure ComposeBackboneElementChildren(xml : TXmlBuilder; element : TFhirBackboneElement);\r\n");
    prsrImpl.append("Procedure TFHIRXmlComposer.ComposeBackboneElementChildren(xml : TXmlBuilder; element : TFhirBackboneElement);\r\n");
    prsrImpl.append("var\r\n");
    prsrImpl.append("  i : integer;\r\n");
    prsrImpl.append("begin\r\n");
    prsrImpl.append("  ComposeElementChildren(xml, element);\r\n");
    prsrImpl.append("  if element.hasModifierExtensionList then\r\n");
    prsrImpl.append("    for i := 0 to element.modifierExtensionList.count - 1 do\r\n");
    prsrImpl.append("      ComposeExtension(xml, 'modifierExtension', element.modifierExtensionList[i]);\r\n");
    prsrImpl.append("end;\r\n\r\n");
    srlsdefJ.append("    Procedure ComposeElementProperties(json : TJSONWriter; elem : TFhirElement);\r\n");
    prsrImpl.append("Procedure TFHIRJsonComposer.ComposeElementProperties(json : TJSONWriter; elem : TFhirElement);\r\n");
    prsrImpl.append("var\r\n");
    prsrImpl.append("  i : integer;\r\n");
    prsrImpl.append("begin\r\n");
    prsrImpl.append("  {no-comments composeComments(json, elem);}\r\n");
    prsrImpl.append("  Prop(json, 'id', elem.Id);\r\n");
    prsrImpl.append("  if elem.hasExtensionList then\r\n");
    prsrImpl.append("  begin\r\n");
    prsrImpl.append("    json.valueArray('extension');\r\n");
    prsrImpl.append("    for i := 0 to elem.extensionList.Count - 1 do\r\n");
    prsrImpl.append("      ComposeExtension(json, '',elem.extensionList[i]);\r\n");
    prsrImpl.append("    json.FinishArray;\r\n");
    prsrImpl.append("  end;\r\n");
    prsrImpl.append("end;\r\n\r\n");
    srlsdefJ.append("    Procedure ComposeBackboneElementProperties(json : TJSONWriter; elem : TFhirBackboneElement);\r\n");
    prsrImpl.append("Procedure TFHIRJsonComposer.ComposeBackboneElementProperties(json : TJSONWriter; elem : TFhirBackboneElement);\r\n");
    prsrImpl.append("var\r\n");
    prsrImpl.append("  i : integer;\r\n");
    prsrImpl.append("begin\r\n");
    prsrImpl.append("  ComposeElementProperties(json, elem);\r\n");
    prsrImpl.append("  if elem.hasModifierExtensionList then\r\n");
    prsrImpl.append("  begin\r\n");
    prsrImpl.append("    json.valueArray('modifierExtension');\r\n");
    prsrImpl.append("    for i := 0 to elem.modifierExtensionList.Count - 1 do\r\n");
    prsrImpl.append("      ComposeExtension(json, '', elem.modifierExtensionList[i]);\r\n");
    prsrImpl.append("    json.FinishArray;\r\n");
    prsrImpl.append("  end;\r\n");
    prsrImpl.append("end;\r\n\r\n");

    defCodeType.classDefs.add(def.toString());
    defCodeType.classImpls.add(impl2.toString());
//    defCodeType.classFwds.add("  TFhirElement = class;\r\n");
//    defineList("TFhirElement", "TFhirElementList", null, ClassCategory.Type, true);


  }

  private void generateResource() throws Exception {
    String prefix = "frt";
    StringBuilder def = new StringBuilder();
    StringBuilder con = new StringBuilder();

    def.append("  {@Enum TFhirResourceType\r\n");
    def.append("    Enumeration of known resource types\r\n");
    def.append("  }\r\n");
    def.append("  TFhirResourceType = (\r\n");
    con.append("  CODES_TFhirResourceType : Array[TFhirResourceType] of String = (");
    def.append("    frtNull, {@enum.value Resource type not known / not Specified }\r\n");
    con.append("'', ");
    int conl = con.length(); 
    constants.add("TFhirResourceType");

    List<String> types = new ArrayList<String>();
    for (String s : definitions.getResources().keySet()) 
      types.add(s);
    for (String s : definitions.getBaseResources().keySet())
      if (!definitions.getBaseResources().get(s).isAbstract())
        types.add(s);
    Collections.sort(types);

    
    for (int i = 0; i < types.size(); i++)  {
      String s = types.get(i);
      String s2 = prefix + getTitle(s);
      if (GeneratorUtils.isDelphiReservedWord(s2))
        s2 = s2 + "_";
      con.append("'"+s);
      def.append("    "+s2+", {@enum.value "+makeDocoSafe(definitions.getResourceByName(s).getDefinition())+" }\r\n");
      con.append("', ");
      conl = conl + s.length()+4;
      if (conl > 1000) {
        con.append("\r\n      ");
        conl = 6;
      }
    }

    con.append(" 'Custom');");
    def.append("    frtCustom);\r\n  TFhirResourceTypeSet = set of TFhirResourceType;");

    int last = 0;

  

    con.append("\r\n  LOWERCASE_CODES_TFhirResourceType : Array[TFhirResourceType] of String = (");
    con.append("'', ");
    for (int i = 0; i < types.size(); i++) {
      String s = types.get(i);
      con.append("'"+s.toLowerCase()+"',\r\n     ");
    }
    con.append("'custom');\r\n     ");

    con.append("\r\n  CLASSES_TFhirResourceType : Array[TFhirResourceType] of TFhirResourceClass = (");
    con.append("nil, ");
    for (int i = 0; i < types.size(); i++) {
      String s = types.get(i);
       con.append("TFhir"+getTitle(s)+",\r\n     ");
    }
    con.append("nil);\r\n     ");

    con.append("\r\n  ALL_RESOURCE_TYPES = [");
    for (int i = 0; i < types.size(); i++) {
      String s = types.get(i);
      String s2 = prefix + getTitle(s);
      if (GeneratorUtils.isDelphiReservedWord(s2))
        s2 = s2 + "_";
        con.append(s2+",\r\n     ");
    }
    con.append("frtCustom];\r\n     ");

    con.append("\r\n  ALL_RESOURCE_TYPE_NAMES : Array of String = [");
    for (int i = 0; i < types.size(); i++) {
      String s = types.get(i);
      String s2 = getTitle(s);
      con.append("'"+s2+"',\r\n     ");
    }
    con.append("'Custom'];\r\n     ");


    defCodeRes.enumDefs.add(def.toString());
    defCodeConstGen.enumConsts.add(con.toString());


  }


  private String makeDocoSafe(String string) {
    if (string == null)
      return "";
    string = Utilities.normaliseEolns(string);
    while (string.contains("]{") && string.contains("}") && string.indexOf("]{") < string.indexOf("}")) {
      string = string.substring(0, string.indexOf("]{")+1)+string.substring(string.indexOf("}")+1);
    }
    string = string.replace("}", "))");
    if (string.length() > 980) {
      int i = 979;
      while (string.charAt(i) != ' ')
        i--;
      string = string.substring(0, i)+"\r\n    "+string.substring(i);
    }
    return string;
  }

  private void initParser(String version, DateTimeType dateTimeType) {
    prsrCode.uses.add("SysUtils");
    prsrCode.uses.add("Classes");
    prsrCode.uses.add("ActiveX");
    prsrCode.uses.add("StringSupport");
    prsrCode.uses.add("DateSupport");
    prsrCode.uses.add("MsXml");
    prsrCode.uses.add("FHIRParserBase");
    prsrCode.uses.add("DateAndTime");
    prsrCode.uses.add("RDFUtilities");
    prsrCode.uses.add("FHIRBase");
    prsrCode.uses.add("FHIRResources");
    prsrCode.uses.add("FHIRConstants");
    prsrCode.uses.add("FHIRTypes");
    prsrCode.uses.add("MsXmlParser");
    prsrCode.uses.add("XmlBuilder");
    prsrCode.uses.add("AdvJSON");
    prsrCode.uses.add("AdvStringMatches");
    prsrCode.comments.add("FHIR v"+version+" generated "+dateTimeType.asStringValue());

    prsrImpl.append(
        "{ TFHIRXmlParser }\r\n"+
            "\r\n"
        );

  }

  private String buildParser() {

    prsrImpl.append(
        "function TFHIRXmlParser.ParseResource(element : IXmlDomElement; path : String) : TFhirResource;\r\n"+
            "begin\r\n"+
            "  if (element = nil) Then\r\n"+
            "    Raise Exception.Create('error - element is nil')\r\n"+
            prsrRegX.toString()+
            "  else\r\n"+
            "    raise Exception.create('Error: the element '+element.baseName+' is not recognised as a valid resource name');\r\n" +
            "end;\r\n\r\n"
        );

    prsrImpl.append(
        "procedure TFHIRXmlComposer.ComposeResource(xml : TXmlBuilder; resource: TFhirResource; links : TFhirBundleLinkList);\r\n"+
            "begin\r\n"+
            "  if (resource = nil) Then\r\n"+
            "    Raise Exception.Create('error - resource is nil');\r\n"+
            "  Case resource.ResourceType of\r\n"+
            srlsRegX.toString()+
            "  else\r\n"+
            "    raise Exception.create('Internal error: the resource type '+CODES_TFhirResourceType[resource.ResourceType]+' is not a valid resource type');\r\n" +
            "  end;\r\n"+
            "end;\r\n\r\n"
        );

    prsrImpl.append(
        "function TFHIRJsonParser.ParseResource(jsn : TJsonObject) : TFhirResource;\r\n"+
            "var\r\n" +
            "  s : String;\r\n" +
            "begin\r\n"+
            "  s := jsn['resourceType'];\r\n "+
            prsrRegJ.toString().substring(6)+
            "  else\r\n"+
            "    raise Exception.create('error: the element '+s+' is not a valid resource name');\r\n" +
            "end;\r\n\r\n"
        );

    prsrImpl.append(
        "function TFHIRJsonParser.ParseFragment(jsn : TJsonObject; type_ : String) : TFhirBase;\r\n"+
            "begin\r\n  "+
            prsrFragJ.toString().substring(6)+
            "  else\r\n"+
            "    raise Exception.create('error: the element '+type_+' is not a valid fragment name');\r\n" +
            "end;\r\n\r\n"
        );

    prsrImpl.append(
        "function TFHIRXmlParser.ParseFragment(element : IXMLDOMElement) : TFhirBase;\r\n"+
            "begin\r\n  "+
            prsrFragX.toString().substring(6)+
            "  else\r\n"+
            "    raise Exception.create('error: the element '+element.nodeName+' is not a valid fragment name');\r\n" +
            "end;\r\n\r\n"
        );

    prsrImpl.append(
        "function TFHIRJsonParser.ParseDataType(jsn : TJsonObject; name : String; type_ : TFHIRTypeClass) : TFHIRType;\r\n"+
            "begin\r\n  "+
            prsrDTJ.toString().substring(6)+
            "  else\r\n"+
            "    raise Exception.create('Unknown Type');\r\n" +
            "end;\r\n\r\n"
        );

    prsrImpl.append(
        "function TFHIRXmlParser.ParseDataType(element : IXMLDOMElement; name : String; type_ : TFHIRTypeClass) : TFhirType;\r\n"+
            "begin\r\n  "+
            "  if (name <> '') and (name <> element.baseName) then\r\n"+
            "    raise Exception.Create('Expected Name mismatch : expected \"'+name+'\"+, but found \"'+element.baseName+'\"');\r\n"+
            prsrDTX.toString().substring(6)+
            "  else\r\n"+
            "    raise Exception.create('Unknown Type');\r\n" +
            "end;\r\n\r\n"
        );

    prsrImpl.append(
        "procedure TFHIRXmlComposer.ComposeBase(xml : TXmlBuilder; name : String; base : TFHIRBase);\r\n"+
            "begin\r\n  "+
            compXBase.toString().substring(6)+
            "  else\r\n"+
            "    raise Exception.create('Unknown Type '+base.className);\r\n" +
            "end;\r\n\r\n"
        );

    prsrImpl.append(
        "procedure TFHIRJsonComposer.ComposeBase(json: TJSONWriter; name: String; base: TFHIRBase);\r\n"+
            "begin\r\n  "+
            compJBase.toString().substring(6)+
            "  else\r\n"+
            "    raise Exception.create('Unknown Type '+base.className);\r\n" +
            "end;\r\n\r\n"
        );

    prsrImpl.append(
        "procedure TFHIRJsonComposer.ComposeResource(json : TJSONWriter; resource: TFhirResource; links : TFhirBundleLinkList);\r\n"+
            "begin\r\n"+
            "  if (resource = nil) Then\r\n"+
            "    Raise Exception.Create('error - resource is nil');\r\n"+
            "  json.value('resourceType', CODES_TFhirResourceType[resource.ResourceType]);\r\n"+
            "  Case resource.ResourceType of\r\n"+
            srlsRegJ.toString()+
            "  else\r\n"+
            "    raise Exception.create('Internal error: the resource type '+CODES_TFhirResourceType[resource.ResourceType]+' is not a valid resource type');\r\n" +
            "  end;\r\n"+
            "end;\r\n\r\n"
        );

    
    prsrImpl.append(
            "procedure TFHIRRDFComposer.ComposeResource(parent : TRDFComplex; resource : TFhirResource);\r\n"+
                "var\r\n"+
                "  this : TRDFComplex;\r\n"+
                "begin\r\n"+
            "  if (resource = nil) Then\r\n"+
            "    Raise Exception.Create('error - resource is nil');\r\n"+
            "  this := parent;\r\n"+
            "  Case resource.ResourceType of\r\n"+
            srlsRegR.toString().replace("%%%%", "")+
            "  else\r\n"+
            "    raise Exception.create('Internal error: the resource type '+CODES_TFhirResourceType[resource.ResourceType]+' is not a valid resource type');\r\n" +
            "  end;\r\n"+
            "end;\r\n\r\n"
        );

    

    return
        "  TFHIRXmlParser = class (TFHIRXmlParserBase)\r\n"+
        "  protected\r\n"+
        prsrdefX.toString()+
        "    function ParseResource(element : IxmlDomElement; path : String) : TFhirResource; override;\r\n"+
        "    function ParseDataType(element : IXmlDomElement; name : String; type_ : TFHIRTypeClass) : TFHIRType; override;\r\n"+
        "  public\r\n"+
        "    function ParseFragment(element : IxmlDomElement) : TFhirBase; overload;\r\n"+
        "  end;\r\n\r\n"+
        "  TFHIRXmlComposer = class (TFHIRXmlComposerBase)\r\n"+
        "  protected\r\n"+
        srlsdefX.toString()+
        "    procedure ComposeResource(xml : TXmlBuilder; resource : TFhirResource; links : TFhirBundleLinkList); override;\r\n"+
        "    procedure ComposeBase(xml : TXmlBuilder; name : String; base : TFHIRBase); override;\r\n"+
        "  end;\r\n\r\n"+
        "  TFHIRJsonParser = class (TFHIRJsonParserBase)\r\n"+
        "  protected\r\n"+
        prsrdefJ.toString()+
        "    function ParseResource(jsn : TJsonObject) : TFhirResource; override;\r\n"+
        "    function ParseDataType(jsn : TJsonObject; name : String; type_ : TFHIRTypeClass) : TFHIRType; override;\r\n"+
        "  public\r\n"+
        "    function ParseFragment(jsn : TJsonObject; type_ : String) : TFhirBase;  overload;\r\n"+
        "  end;\r\n\r\n"+
        "  TFHIRJsonComposer = class (TFHIRJsonComposerBase)\r\n"+
        "  protected\r\n"+
        srlsdefJ.toString()+
        "    procedure ComposeResource(json : TJSONWriter; resource : TFhirResource; links : TFhirBundleLinkList); override;\r\n"+
        "    procedure ComposeBase(json : TJSONWriter; name : String; base : TFHIRBase); override;\r\n"+
        "  end;\r\n\r\n"+
        "  TFHIRRDFComposer = class (TFHIRRDFComposerBase)\r\n"+
        "  protected\r\n"+
        srlsdefR.toString()+
        "  \r\n"+
        "    procedure ComposeResource(parent :  TRDFComplex; resource : TFhirResource); overload; override;\r\n"+
        "  end;\r\n\r\n";
  }

}
