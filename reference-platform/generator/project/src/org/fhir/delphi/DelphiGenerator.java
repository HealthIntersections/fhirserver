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
import org.hl7.fhir.utilities.TextFile;
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
  private DelphiCodeGenerator prsrCodeX;
  private DelphiCodeGenerator prsrCodeJ;
  private DelphiCodeGenerator prsrCodeT;
  private DelphiCodeGenerator jsCode;
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
  private StringBuilder workingParserT;
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
  private StringBuilder prsrRegT = new StringBuilder();
  private StringBuilder prsrImplX = new StringBuilder();
  private StringBuilder prsrImplJ = new StringBuilder();
  private StringBuilder prsrImplT = new StringBuilder();
  private StringBuilder prsrdefX = new StringBuilder();
  private StringBuilder srlsdefX = new StringBuilder();
  private StringBuilder prsrdefJ = new StringBuilder();
  private StringBuilder prsrdefT = new StringBuilder();
  private StringBuilder srlsdefJ = new StringBuilder();
  private StringBuilder prsrdefR = new StringBuilder();
  private StringBuilder srlsdefR = new StringBuilder();
  private StringBuilder prsrFragJ = new StringBuilder();
  private StringBuilder prsrFragX = new StringBuilder();
  private StringBuilder prsrFragR = new StringBuilder();
  private StringBuilder prsrDTX = new StringBuilder();
  private StringBuilder prsrDTJ = new StringBuilder();
  private StringBuilder prsrDTT = new StringBuilder();
  private StringBuilder prsrDTR = new StringBuilder();
  private StringBuilder compXBase = new StringBuilder();
  private StringBuilder compJBase = new StringBuilder();
  private StringBuilder compRBase = new StringBuilder();
  private StringBuilder jsRegM = new StringBuilder();
  
  private Map<String, String> simpleTypes = new HashMap<String, String>();

  private List<String> types = new ArrayList<String>();
  private List<String> constants = new ArrayList<String>();
  private List<String> opNames = new ArrayList<String>();
  private String destDir;
  private String pubVersion;


  public DelphiGenerator(String destDir) {
    super();
    this.destDir = destDir;
  }

  public void generate(Definitions definitions, String version, DateTimeType dateTimeType, int dstuID)  throws Exception {
    simpleTypes.put("TFhirId", "String");
    this.pubVersion = Integer.toString(dstuID);
    StringBuilder include = new StringBuilder();

    start(destDir, version, dateTimeType, Integer.toString(dstuID));
    initParser(version, dateTimeType);

    this.definitions = definitions;

    generate(definitions.getInfrastructure().get("Element"), "TFHIRObject", false, ClassCategory.Type, true, false);
    generate(definitions.getInfrastructure().get("BackboneElement"), "TFHIRElement", false, ClassCategory.Type, true, false);

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
            if (!n.isAbstract()) { 
              include.append("{$DEFINE FHIR_"+s.toUpperCase()+"}\r\n");
              ifdef(s.toUpperCase());
            }
            generate(n.getRoot(), Utilities.noString(n.getRoot().typeCode()) ? "TFHIRObject" : "TFhir"+n.getRoot().typeCode(), true, n.isAbstract() ? ClassCategory.AbstractResource : ClassCategory.Resource, n.isAbstract(), n.isSpecial());
            if (!n.isAbstract()) {
              prsrRegX.append("  else if element.localName = '"+n.getName()+"' Then\r\n    result := Parse"+n.getName()+"(element, path+'/"+n.getName()+"')\r\n");
              srlsRegX.append("    frt"+n.getName()+": Compose"+n.getName()+"(xml, '"+n.getName()+"', TFhir"+n.getName()+"(resource));\r\n");
              prsrRegJ.append("  else if s = '"+n.getName()+"' Then\r\n    result := Parse"+n.getName()+"(jsn)\r\n");
              srlsRegJ.append("    frt"+n.getName()+": Compose"+n.getName()+"(json, '"+n.getName()+"', TFhir"+n.getName()+"(resource));\r\n");
              srlsRegR.append("    frt"+n.getName()+": Compose"+n.getName()+"(this, '%%%%', '"+n.getName()+"', TFhir"+n.getName()+"(resource), true, -1);\r\n");
              prsrRegT.append("  else if s = '"+n.getName()+"' Then\r\n    result := Parse"+n.getName()+"(obj)\r\n");
              endif(s.toUpperCase());
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
          generate(n, "TFHIRType", false, ClassCategory.Type, false, false);
      }
    }

    for (ElementDefn n : definitions.getTypes().values()) {
      if (!hasExplicitParent(n))
        generate(n, "TFhirType", false, ClassCategory.Type, false, false);
    }

    for (ElementDefn n : definitions.getStructures().values()) {
      if (!hasExplicitParent(n))
        generate(n, "TFhirType", false, ClassCategory.Type, false, false);
    }

    for (ElementDefn n : definitions.getInfrastructure().values()) {
      if (!n.getName().equals("Element") && !n.getName().equals("BackboneElement")) {
        if (hasExplicitParent(n))
          generate(n, "TFHIR"+n.typeCode(), false, ClassCategory.Type, false, false);
      }
    }

    for (ElementDefn n : definitions.getTypes().values()) {
      if (hasExplicitParent(n))
        generate(n, "TFhir"+n.typeCode(), false, ClassCategory.Type, false, false);
    }

    for (ElementDefn n : definitions.getStructures().values()) {
      if (hasExplicitParent(n))
        generate(n, "TFhir"+n.typeCode(), false, ClassCategory.Type, false, false);
    }

    parserGap();
    for (String s : definitions.sortedResourceNames()) {
      include.append("{$DEFINE FHIR_"+s.toUpperCase()+"}\r\n");
      ifdef(s.toUpperCase());
      ResourceDefn n = definitions.getResources().get(s);
      generate(n.getRoot(), "TFhir"+n.getRoot().typeCode(), true, ClassCategory.Resource, false, false);
      generateSearchEnums(n);
      generateIndexInformation(n);
      prsrRegX.append("  {$IFDEF FHIR_"+s.toUpperCase()+"}\r\n   else if element.localName = '"+n.getName()+"' Then\r\n    result := Parse"+n.getName()+"(element, path+'/"+n.getName()+"') \r\n  {$ENDIF}\r\n");
      srlsRegX.append("  {$IFDEF FHIR_"+s.toUpperCase()+"}\r\n     frt"+n.getName()+": Compose"+n.getName()+"(xml, '"+n.getName()+"', TFhir"+n.getName()+"(resource)); \r\n  {$ENDIF}\r\n");
      prsrRegJ.append("  {$IFDEF FHIR_"+s.toUpperCase()+"}\r\n   else if s = '"+n.getName()+"' Then\r\n    result := Parse"+n.getName()+"(jsn) \r\n  {$ENDIF}\r\n");
      srlsRegJ.append("  {$IFDEF FHIR_"+s.toUpperCase()+"}\r\n     frt"+n.getName()+": Compose"+n.getName()+"(json, '"+n.getName()+"', TFhir"+n.getName()+"(resource));\r\n   {$ENDIF}\r\n");
      srlsRegR.append("  {$IFDEF FHIR_"+s.toUpperCase()+"}\r\n     frt"+n.getName()+": Compose"+n.getName()+"(this, '%%%%', '"+n.getName()+"', TFhir"+n.getName()+"(resource), true, -1); \r\n  {$ENDIF}\r\n");
      prsrRegT.append("  {$IFDEF FHIR_"+s.toUpperCase()+"}\r\n   else if s = '"+n.getName()+"' Then\r\n    result := Parse"+n.getName()+"(obj) \r\n  {$ENDIF}\r\n");
      endif(s.toUpperCase());
    }

    opStart();
    for (OperationDefinition op : definitions.getOperations()) 
      genOp(op);

    finishIndexer();

    TextFile.stringToFile(include.toString(), Utilities.path(destDir, "fhir.incn"));
    
    defCodeConstGen.enumConsts.add("  FHIR_GENERATED_VERSION = '"+version+"';\r\n");
    defCodeConstGen.enumConsts.add("  FHIR_GENERATED_PUBLICATION = '"+pubVersion+"';\r\n");
    defCodeConstGen.enumConsts.add("  FHIR_GENERATED_DATE = '"+dateTimeType.asStringValue()+"';\r\n");
    defCodeRes.classDefs.add("  {@Class TFhirResourceFactory : TFHIRObjectFactory\r\n");
    defCodeRes.classDefs.add("     FHIR factory: class constructors and general useful builders\r\n");
    defCodeRes.classDefs.add("  }\r\n");
    defCodeRes.classDefs.add(" TFhirResourceFactory = class (TFHIRObjectFactory)\r\n  public\r\n"+factoryIntf.toString()+"    function makeByName(const name : String) : TFHIRObject;\r\n  end;\r\n");
    types.add("TFhirResourceFactory");
    defCodeRes.classImpls.add(factoryImpl.toString());
    defCodeRes.classImpls.add("function TFHIRResourceFactory.makeByName(const name : String) : TFHIRObject;\r\nbegin\r\n  "+factoryByName.toString().substring(7)+"  else\r\n    result := nil;\r\nend;\r\n\r\n");
    defCodeType.finish();
    defCodeRes.finish();
    defCodeOp.finish();
    defCodeConstGen.finish();
    defIndexer.finish();
    
    jsCode.procsPub.add("procedure registerFHIRTypes(js : TFHIRJavascript);\r\n");
    jsCode.procs.add("procedure registerFHIRTypes(js : TFHIRJavascript);\r\n");
    jsCode.procs.add("begin\r\n");
    jsCode.procs.add(jsRegM.toString());
    jsCode.procs.add("end;\r\n");
    jsCode.finish();

    prsrCodeX.classDefs.add(buildParserXml());
    prsrCodeX.classImpls.add(prsrImplX.toString());
    prsrCodeX.finish();
    prsrCodeJ.classDefs.add(buildParserJson());
    prsrCodeJ.classImpls.add(prsrImplJ.toString());
    prsrCodeJ.finish();
    prsrCodeT.classDefs.add(buildParserTurtle());
    prsrCodeT.classImpls.add(prsrImplT.toString());
    prsrCodeT.finish();
  }

  private void ifdef(String n) {
    defCodeRes.ifdef(n);
    prsrRegX.append("{$IFDEF FHIR_"+n+"}\r\n");
    srlsRegX.append("{$IFDEF FHIR_"+n+"}\r\n");
    prsrRegJ.append("{$IFDEF FHIR_"+n+"}\r\n");
    srlsRegJ.append("{$IFDEF FHIR_"+n+"}\r\n");
    srlsRegR.append("{$IFDEF FHIR_"+n+"}\r\n");
    prsrRegT.append("{$IFDEF FHIR_"+n+"}\r\n");
    prsrImplX.append("{$IFDEF FHIR_"+n+"}\r\n");
    prsrImplJ.append("{$IFDEF FHIR_"+n+"}\r\n");
    prsrImplT.append("{$IFDEF FHIR_"+n+"}\r\n");
    prsrdefX.append("{$IFDEF FHIR_"+n+"}\r\n");
    srlsdefX.append("{$IFDEF FHIR_"+n+"}\r\n");
    prsrdefJ.append("{$IFDEF FHIR_"+n+"}\r\n");
    prsrdefT.append("{$IFDEF FHIR_"+n+"}\r\n");
    srlsdefJ.append("{$IFDEF FHIR_"+n+"}\r\n");
    prsrdefR.append("{$IFDEF FHIR_"+n+"}\r\n");
    srlsdefR.append("{$IFDEF FHIR_"+n+"}\r\n");
    prsrFragJ.append("{$IFDEF FHIR_"+n+"}\r\n");
    prsrFragX.append("{$IFDEF FHIR_"+n+"}\r\n");
    prsrFragR.append("{$IFDEF FHIR_"+n+"}\r\n");
    compXBase.append("{$IFDEF FHIR_"+n+"}\r\n");
    compJBase.append("{$IFDEF FHIR_"+n+"}\r\n");
    compRBase.append("{$IFDEF FHIR_"+n+"}\r\n");
    factoryIntf.append("{$IFDEF FHIR_"+n+"}\r\n");   
    factoryImpl.append("{$IFDEF FHIR_"+n+"}\r\n");
    factoryByName.append("{$IFDEF FHIR_"+n+"}\r\n");

  }

  private void endif(String n) {
    defCodeRes.endif(n);
    prsrRegX.append("{$ENDIF FHIR_"+n+"}\r\n");
    srlsRegX.append("{$ENDIF FHIR_"+n+"}\r\n");
    prsrRegJ.append("{$ENDIF FHIR_"+n+"}\r\n");
    srlsRegJ.append("{$ENDIF FHIR_"+n+"}\r\n");
    srlsRegR.append("{$ENDIF FHIR_"+n+"}\r\n");
    prsrRegT.append("{$ENDIF FHIR_"+n+"}\r\n");
    prsrImplX.append("{$ENDIF FHIR_"+n+"}\r\n");
    prsrImplJ.append("{$ENDIF FHIR_"+n+"}\r\n");
    prsrImplT.append("{$ENDIF FHIR_"+n+"}\r\n");
    prsrdefX.append("{$ENDIF FHIR_"+n+"}\r\n");
    srlsdefX.append("{$ENDIF FHIR_"+n+"}\r\n");
    prsrdefJ.append("{$ENDIF FHIR_"+n+"}\r\n");
    prsrdefT.append("{$ENDIF FHIR_"+n+"}\r\n");
    srlsdefJ.append("{$ENDIF FHIR_"+n+"}\r\n");
    prsrdefR.append("{$ENDIF FHIR_"+n+"}\r\n");
    srlsdefR.append("{$ENDIF FHIR_"+n+"}\r\n");
    prsrFragJ.append("{$ENDIF FHIR_"+n+"}\r\n");
    prsrFragX.append("{$ENDIF FHIR_"+n+"}\r\n");
    prsrFragR.append("{$ENDIF FHIR_"+n+"}\r\n");
    compXBase.append("{$ENDIF FHIR_"+n+"}\r\n");
    compJBase.append("{$ENDIF FHIR_"+n+"}\r\n");
    compRBase.append("{$ENDIF FHIR_"+n+"}\r\n");    
    factoryIntf.append("{$ENDIF FHIR_"+n+"}\r\n");   
    factoryImpl.append("{$ENDIF FHIR_"+n+"}\r\n");
    factoryByName.append("{$ENDIF FHIR_"+n+"}\r\n");
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

    genOp(op, op.getParameter(), OperationParameterUse.IN, "Request", "Req");
    genOp(op, op.getParameter(), OperationParameterUse.OUT, "Response", "Resp");
  }

  private void genOp(OperationDefinition op, List<OperationDefinitionParameterComponent> plist, OperationParameterUse use, String suffix, String prefix) throws IOException {
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
    boolean usesS = false;
    for (OperationDefinitionParameterComponent p : plist) {
      if (use == null || p.getUse() == use) {
        String pn = getPascalName(p.getName());
        String pt = null;
        boolean complex = false;
        if (p.getType() == null) {
          pt = "TFHIR"+name+"Op"+prefix+Utilities.capitalize(pn);
          genOp(op, p.getPart(), null, prefix+Utilities.capitalize(pn), prefix);
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
        boolean obj = !Utilities.existsInList(pt, "String", "Boolean", "Integer", "TDateTimeEx");
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
            screate.append("  for s in params.getVar('"+p.getName()+"').Split([';']) do\r\n");
            screate.append("    F"+Utilities.capitalize(pn)+"List.add(s); \r\n");
            usesS = true;

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
              screate.append("  !F"+Utilities.capitalize(pn)+" := StrToBoolDef(params.getVar('"+p.getName()+"'), false); - 3\r\n");
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
            else if (pt.equals("TDateTimeEx"))
              params.append("    if (F"+Utilities.capitalize(pn)+".notNull) then\r\n");
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
              create.append("  if params.param['"+p.getName()+"'] <> nil then\r\n    F"+Utilities.capitalize(pn)+" := (params.param['"+p.getName()+"'].value as "+pt+").Link; {ob.5d}\r\n");
            }
          } else if (obj) {
            params.append("      result.addParameter('"+p.getName()+"', TFHIR"+Utilities.capitalize(p.getType())+".create(F"+Utilities.capitalize(pn)+"));{oz.5e}\r\n");
            create.append("  F"+Utilities.capitalize(pn)+" := (params.param['"+p.getName()+"'].value as TFHIR"+Utilities.capitalize(p.getType())+").value;\r\n");
          } else {
            params.append("      result.addParameter('"+p.getName()+"', TFHIR"+Utilities.capitalize(p.getType())+".create(F"+Utilities.capitalize(pn)+"));{oz.5f}\r\n");
            if (pt.equals("Boolean")) {
              create.append("  F"+Utilities.capitalize(pn)+" := params.bool['"+p.getName()+"'];\r\n");
              screate.append("  F"+Utilities.capitalize(pn)+" := StrToBoolDef(params.getVar('"+p.getName()+"'), false);\r\n");
            } else if (pt.equals("TDateTimeEx")) {
              create.append("  F"+Utilities.capitalize(pn)+" := TDateTimeEx.fromXml(params.str['"+p.getName()+"']);\r\n");
              screate.append("  F"+Utilities.capitalize(pn)+" := TDateTimeEx.fromXml(params.getVar('"+p.getName()+"'));\r\n");
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
              "    constructor Create; overload; override;\r\n"+
              "    constructor Create(params : TFhirParametersParameter); overload; override;\r\n"+
              "    destructor Destroy; override;\r\n"+
              "    function asParams(name : String) : TFHIRParametersParameter; override;\r\n"+
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
          (usesS ? "var\r\n  s : String;\r\n" : "")+
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
    prsrdefT.append("\r\n");
    srlsdefX.append("\r\n");
    srlsdefJ.append("\r\n");
    srlsdefR.append("\r\n");
    srlsdefR.append("\r\n");
  }

  private void start(String implDir, String version, DateTimeType dateTimeType, String dstuID)
      throws UnsupportedEncodingException, FileNotFoundException, Exception {
    String sfxDstuID = "";
    defCodeRes = new DelphiCodeGenerator(new FileOutputStream(Utilities.path(implDir, "FHIRResources"+sfxDstuID+".pasn")), dstuID, sfxDstuID);
    defCodeRes.start();
    defCodeRes.name = "FHIRResources";
    defCodeRes.comments.add("FHIR v"+version+" generated "+dateTimeType.asStringValue());
    defCodeRes.uses.add("SysUtils");
    defCodeRes.uses.add("Classes");
    defCodeRes.uses.add("StringSupport");
    defCodeRes.uses.add("DecimalSupport");
    defCodeRes.uses.add("AdvBuffers");
    if (generics)
      defCodeRes.uses.add("AdvGenerics");
    defCodeRes.uses.add("DateSupport");
    defCodeRes.uses.add("FHIRBase");
    defCodeRes.uses.add("FHIRTypes");
    defCodeRes.usesImpl.add("FHIRUtilities");

    jsCode = new DelphiCodeGenerator(new FileOutputStream(Utilities.path(implDir, "FHIRJavascriptReg"+sfxDstuID+".pas")), dstuID, sfxDstuID);
    jsCode.start();
    jsCode.name = "FHIRJavascriptReg";
    jsCode.comments.add("FHIR v"+version+" generated "+dateTimeType.asStringValue());
    jsCode.uses.add("Javascript");
    jsCode.uses.add("FHIRJavascript");

    defCodeConstGen = new DelphiCodeGenerator(new FileOutputStream(Utilities.path(implDir, "FHIRConstants"+sfxDstuID+".pasn")), dstuID, sfxDstuID);
    defCodeConstGen.start();
    defCodeConstGen.name = "FHIRConstants";
    defCodeConstGen.comments.add("FHIR v"+version+" generated "+dateTimeType.asStringValue());
    defCodeConstGen.precomments.add("!ignore ALL_RESOURCE_TYPES");
    defCodeConstGen.uses.add("SysUtils");
    defCodeConstGen.uses.add("Classes");
    defCodeConstGen.uses.add("StringSupport");
    defCodeConstGen.uses.add("DecimalSupport");
    defCodeConstGen.uses.add("AdvBuffers");
    if (generics)
      defCodeConstGen.uses.add("AdvGenerics");
    defCodeConstGen.uses.add("DateSupport");
    defCodeConstGen.uses.add("FHIRBase");
    defCodeConstGen.uses.add("FHIRTypes");
    defCodeConstGen.uses.add("FHIRResources");

    defIndexer = new DelphiCodeGenerator(new FileOutputStream(Utilities.path(implDir, "FHIRIndexInformation"+sfxDstuID+".pasn")), dstuID, sfxDstuID);
    defIndexer.start();
    defIndexer.name = "FHIRIndexInformation";
    defIndexer.comments.add("FHIR v"+version+" generated "+dateTimeType.asStringValue());
    defIndexer.uses.add("SysUtils");
    defIndexer.uses.add("Classes");
    defIndexer.uses.add("StringSupport");
    defIndexer.uses.add("DecimalSupport");
    defIndexer.uses.add("AdvBuffers");
    defIndexer.uses.add("DateSupport");
    defIndexer.uses.add("FHIRIndexBase");
    defIndexer.uses.add("FHIRResources");
    defIndexer.uses.add("FHIRTypes");
    defIndexer.uses.add("FHIRConstants");
    defIndexer.uses.add("FHIRSupport");

    indexHeaders = new StringBuilder();
    indexBody = new StringBuilder();
    indexMethods = new StringBuilder();


    defCodeType = new DelphiCodeGenerator(new FileOutputStream(Utilities.path(implDir, "FHIRTypes"+sfxDstuID+".pasn")), dstuID, sfxDstuID);
    defCodeType.start();
    defCodeType.name = "FHIRTypes";
    defCodeType.comments.add("FHIR v"+version+" generated "+dateTimeType.asStringValue());
    defCodeType.uses.add("Classes");
    defCodeType.uses.add("SysUtils");
    defCodeType.uses.add("DecimalSupport");
    defCodeType.uses.add("StringSupport");
    //    defCodeType.uses.add("AdvWideStringLists");
    defCodeType.uses.add("AdvBuffers");
    if (generics)
      defCodeType.uses.add("AdvGenerics");
    defCodeType.uses.add("EncdDecd");
    defCodeType.uses.add("DateSupport");
    defCodeType.uses.add("FHIRBase");
    defCodeType.usesImpl.add("FHIRUtilities");

    factoryIntf = new StringBuilder();
    factoryImpl = new StringBuilder();
    factoryByName = new StringBuilder();


    prsrCodeX = new DelphiCodeGenerator(new FileOutputStream(Utilities.path(implDir, "FHIRParserXml"+sfxDstuID+".pasn")), dstuID, sfxDstuID);
    prsrCodeX.start();
    prsrCodeX.name = "FHIRParserXml";

    prsrCodeJ = new DelphiCodeGenerator(new FileOutputStream(Utilities.path(implDir, "FHIRParserJson"+sfxDstuID+".pasn")), dstuID, sfxDstuID);
    prsrCodeJ.start();
    prsrCodeJ.name = "FHIRParserJson";

    prsrCodeT = new DelphiCodeGenerator(new FileOutputStream(Utilities.path(implDir, "FHIRParserTurtle"+sfxDstuID+".pasn")), dstuID, sfxDstuID);
    prsrCodeT.start();
    prsrCodeT.name = "FHIRParserTurtle";

    defCodeOp = new DelphiCodeGenerator(new FileOutputStream(Utilities.path(implDir, "FHIROperations"+sfxDstuID+".pasn")), dstuID, sfxDstuID);
    defCodeOp.start();
    defCodeOp.name = "FHIROperations";
    defCodeOp.comments.add("FHIR v"+version+" generated "+dateTimeType.asStringValue());
    defCodeOp.uses.add("SysUtils");
    defCodeOp.uses.add("Classes");
    defCodeOp.uses.add("Generics.Collections");
    defCodeOp.uses.add("StringSupport");
    defCodeOp.uses.add("DecimalSupport");
    defCodeOp.uses.add("AdvBuffers");
    defCodeOp.uses.add("AdvGenerics");
    defCodeOp.uses.add("ParseMap");
    defCodeOp.uses.add("DateSupport");
    defCodeOp.uses.add("FHIRBase");
    defCodeOp.uses.add("FHIRTypes");
    defCodeOp.uses.add("FHIRResources");
    defCodeOp.uses.add("FHIROpBase");
    defCodeOp.usesImpl.add("FHIRUtilities");
  }

  private void generate(ElementDefn root, String superClass, boolean resource, ClassCategory category, boolean isAbstract, boolean isSpecial) throws Exception {
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
      genTypeAbstract(root, "TFhir"+root.getName(), superClass, category, isSpecial);
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
    prsrdefX.append("    function Parse"+root.getName()+"(element : TMXmlElement; path : string) : TFhir"+root.getName()+";\r\n");
    if (!root.getName().equals("Element") && !root.getName().equals("BackboneElement"))
      prsrdefX.append("    function Parse"+root.getName()+"Child(element : TFhir"+root.getName()+"; path : string; child : TMXmlElement) : boolean;\r\n");
    srlsdefX.append("    procedure Compose"+root.getName()+"(xml : TXmlBuilder; name : string; elem : TFhir"+root.getName()+");\r\n");
    if (!root.getName().equals("Element") && !root.getName().equals("BackboneElement"))
      srlsdefX.append("    procedure Compose"+root.getName()+"Children(xml : TXmlBuilder; elem : TFhir"+root.getName()+");\r\n");
    prsrdefJ.append("    function Parse"+root.getName()+"(jsn : TJsonObject) : TFhir"+root.getName()+"; overload;\r\n");
    prsrdefT.append("    function Parse"+root.getName()+"(obj : TTurtleComplex) : TFhir"+root.getName()+"; overload;\r\n");
    if (!root.getName().equals("Element") && !root.getName().equals("BackboneElement")) {
      prsrdefJ.append("    procedure Parse"+root.getName()+"Properties(jsn : TJsonObject; result : TFhir"+root.getName()+"); overload;\r\n");
      prsrdefT.append("    procedure Parse"+root.getName()+"Properties(obj : TTurtleComplex; result : TFhir"+root.getName()+"); overload;\r\n");
    }
    srlsdefJ.append("    procedure Compose"+root.getName()+"(json : TJSONWriter; name : string; elem : TFhir"+root.getName()+"; noObj : boolean = false);"+"\r\n");
    prsrdefR.append("    function Parse"+root.getName()+"(rdf : TObject) : TFhir"+root.getName()+"; overload;\r\n");
    srlsdefR.append("    procedure Compose"+root.getName()+"(parent :  TTurtleComplex; parentType, name : String; elem : TFhir"+root.getName()+"; useType : boolean; index : integer);"+"\r\n");
    prsrFragJ.append("  else if (type_ = '"+tn+"') then\r\n    result := parse"+root.getName()+"(jsn)\r\n");
    prsrFragX.append("  else if SameText(element.Name, '"+tn+"') then\r\n    result := parse"+root.getName()+"(element, element.Name)\r\n");
    prsrFragR.append("  else if SameText(type_, '"+tn.substring(5)+"') then\r\n    result := parse"+root.getName()+"(obj)\r\n");
    if (category == ClassCategory.Type && !isAbstract) {
      prsrDTJ.append("  else if (type_ = "+tn+") then\r\n    result := parse"+root.getName()+"(jsn)\r\n");
      prsrDTT.append("  else if (type_ = "+tn+") then\r\n    result := parse"+root.getName()+"(obj)\r\n");
      prsrDTX.append("  else if (type_ = "+tn+") then\r\n    result := parse"+root.getName()+"(element, name)\r\n");
      prsrDTR.append("  else if (type_ = "+tn+") then\r\n    result := parse"+root.getName()+"(rdf, name)\r\n");
    }
    if (!isAbstract) {
      compJBase.append("  else if (base is "+tn+") then\r\n    compose"+root.getName()+"(json, name, "+tn+"(base), false)\r\n");
      compXBase.append("  else if (base is "+tn+") then\r\n    compose"+root.getName()+"(xml, name,  "+tn+"(base))\r\n");
      compRBase.append("  else if (base is "+tn+") then\r\n    compose"+root.getName()+"(section, name,  "+tn+"(base), true, -1)\r\n");
    }
    String tj = tn.substring(5);
    if (!isAbstract)
      jsRegM.append("  define"+tj+"Js(js); \r\n");
    StringBuilder jsReg = new StringBuilder();
    if (!superClass.equalsIgnoreCase("TFhirObject")) {
      if (superClass.equalsIgnoreCase("TFhirType")) {
        jsReg.append("  defineElementPropsJs(js, def);\r\n");
      } else {
        jsReg.append("  define"+superClass.substring(5)+"PropsJs(js, def);\r\n");
      }
    }

    workingParserX = new StringBuilder();
    workingParserXA = new StringBuilder();
    workingComposerX = new StringBuilder();
    workingComposerXA = new StringBuilder();
    workingParserJ = new StringBuilder();
    workingComposerJ = new StringBuilder();
    workingParserT = new StringBuilder();
    workingComposerR = new StringBuilder();


    StringBuilder def = new StringBuilder();
    StringBuilder defPriv1 = new StringBuilder();
    StringBuilder defPriv2 = new StringBuilder();
    StringBuilder defPub = new StringBuilder();
    StringBuilder impl = new StringBuilder();
    StringBuilder create = new StringBuilder();
    StringBuilder destroy = new StringBuilder();
    StringBuilder assign = new StringBuilder();
    StringBuilder empty = new StringBuilder();
    StringBuilder getkids = new StringBuilder();
    StringBuilder getkidsvars = new StringBuilder();
    StringBuilder getprops = new StringBuilder();
    StringBuilder getpropsvars = new StringBuilder();
    StringBuilder setprops = new StringBuilder();
    StringBuilder insprops = new StringBuilder();
    StringBuilder makeprops = new StringBuilder();
    StringBuilder delprops = new StringBuilder();
    StringBuilder replprops = new StringBuilder();
    StringBuilder reorderprops = new StringBuilder();
    impl.append("{ "+tn+" }\r\n\r\n");

    for (ElementDefn e : root.getElements()) {
      generateField(e, root.getName(), defPriv1, defPriv2, defPub, impl, create, destroy, assign, empty, getkids, getkidsvars, getprops, getpropsvars, setprops, insprops, makeprops, delprops, replprops, reorderprops, tn, "", category, tn.equals("TFhirExtension"), jsReg, tj);
    }

    def.append("  {@Class "+tn+" : "+superClass+"\r\n");
    def.append("    "+makeDocoSafe(root.getDefinition())+"\r\n");
    def.append("  }\r\n");
    def.append("  "+tn+" = class ("+superClass+")\r\n");
    types.add(tn);
    factoryIntf.append("    {@member new"+tn.substring(5)+"\r\n      create a new "+root.getName()+"\r\n    }\r\n    {!script nolink}\r\n    function new"+tn.substring(5)+" : "+tn+";\r\n");    
    factoryImpl.append("function TFhirResourceFactory.new"+tn.substring(5)+" : "+tn+";\r\nbegin\r\n  result := "+tn+".create;\r\nend;\r\n\r\n");
    factoryByName.append("  else if name = '"+tn.substring(5)+"' then\r\n    result := new"+tn.substring(5)+"()\r\n");
    def.append("  protected\r\n");
    def.append(defPriv1.toString());
    def.append(defPriv2.toString());
    def.append("  \r\n");
    if (category == ClassCategory.Resource) {
      def.append("    function GetResourceType : TFhirResourceType; override;\r\n");      
    }
    def.append("    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;\r\n");
    def.append("    Procedure ListProperties(oList : "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues : Boolean); Override;\r\n");
    def.append("  public\r\n");
    def.append("    constructor Create; Override;\r\n");
    def.append("    destructor Destroy; override;\r\n");
    def.append("    {!script hide}\r\n");
    def.append("    procedure Assign(oSource : TAdvObject); override;\r\n");
    def.append("    function Link : "+tn+"; overload;\r\n");
    def.append("    function Clone : "+tn+"; overload;\r\n");
    def.append("    procedure setProperty(propName : string; propValue : TFHIRObject); override;\r\n");
    def.append("    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;\r\n");
    def.append("    function createPropertyValue(propName : string): TFHIRObject; override;\r\n");
    def.append("    procedure deleteProperty(propName : string; value : TFHIRObject); override;\r\n");
    def.append("    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;\r\n");
    def.append("    procedure reorderProperty(propName : string; source, destination : integer); override;\r\n");
    def.append("    function fhirType : string; override;\r\n");
    def.append("    function equalsDeep(other : TFHIRObject) : boolean; override;\r\n");
    def.append("    function equalsShallow(other : TFHIRObject) : boolean; override;\r\n");
    def.append("    function isEmpty : boolean; override;\r\n");
    if (tn.equals("TFhirElement")) {
      def.append("    function getId : String; override;\r\n");
    }
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
    if (tn.equals("TFhirElement")) {
      impl2.append("function TFHIRElement.getId : string;\r\n");
      impl2.append("begin\r\n");
      impl2.append("  result := id;\r\n");
      impl2.append("end;\r\n\r\n");
    }

    impl2.append("procedure "+tn+".Assign(oSource : TAdvObject);\r\n");
    impl2.append("begin\r\n");
    impl2.append("  inherited;\r\n");
    impl2.append(assign.toString());
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".GetChildrenByName(child_name : string; list : TFHIRSelectionList);\r\n");
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
    impl2.append("procedure "+tn+".insertProperty(propName: string; propValue: TFHIRObject; index : integer);\r\n");
    impl2.append("begin\r\n");
    if (insprops.length() > 7) {
      impl2.append("  "+insprops.toString().substring(7));
      impl2.append("  else inherited;\r\n");
    } else {
      impl2.append("  inherited;\r\n");
    }
    impl2.append("end;\r\n\r\n");
    impl2.append("function "+tn+".createPropertyValue(propName: string) : TFHIRObject;\r\n");
    impl2.append("begin\r\n");
    if (makeprops.length() > 7) {
      impl2.append("  "+makeprops.toString().substring(7));
      impl2.append("  else result := inherited createPropertyValue(propName);\r\n");
    } else
      impl2.append("  result := inherited createPropertyValue(propName);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".deleteProperty(propName : string; value : TFHIRObject);\r\n");
    impl2.append("begin\r\n");
    if (delprops.length() > 7) {
      impl2.append("  "+delprops.toString().substring(7));
      impl2.append("  else\r\n    inherited deleteProperty(propName, value);\r\n");
    } else
      impl2.append("  inherited deleteProperty(propName, value);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".replaceProperty(propName : string; existing, new : TFHIRObject);\r\n");
    impl2.append("begin\r\n");
    if (replprops.length() > 7) {
      impl2.append("  "+replprops.toString().substring(7));
      impl2.append("  else\r\n    inherited replaceProperty(propName, existing, new);\r\n");
    } else
      impl2.append("  inherited replaceProperty(propName, existing, new);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".reorderProperty(propName : string; source, destination : integer);\r\n");
    impl2.append("begin\r\n");
    if (reorderprops.length() > 7) {
      impl2.append("  "+reorderprops.toString().substring(7));
      impl2.append("  else\r\n    inherited reorderProperty(propName, source, destination);\r\n");
    } else
      impl2.append("  inherited reorderProperty(propName, source, destination);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function "+tn+".fhirType : string;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := '"+root.getName()+"';\r\n");
    impl2.append("end;\r\n\r\n");

    impl2.append("function "+tn+".isEmpty : boolean;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := inherited isEmpty "+Utilities.splitLineForLength(empty.toString(), 30, 4, 1023)+";\r\n");
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
    String parent = root.typeCode();
    if (!Utilities.noString(parent) && definitions.getBaseResources().containsKey(parent)) {
      ResourceDefn rd = definitions.getResourceByName(parent);
      if (rd.isSpecial())
        parent = rd.getRoot().typeCode();
    }
    generateParser(root, tn, category, !superClass.equals("TFHIRObject"), parent);
    defineList(tn, tn+"List", null, category, category == ClassCategory.AbstractResource, false);
    
    StringBuilder jsClass = new StringBuilder(); 
    jsClass.append("procedure define"+tj+"PropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);\r\n");
    jsClass.append("begin\r\n");
    jsClass.append(jsReg.toString());
    jsClass.append("end;\r\n\r\n");
    if (!isAbstract) {
      jsClass.append("procedure define"+tj+"Js(js : TFHIRJavascript);\r\n");
      jsClass.append("var\r\n  def : TJavascriptClassDefinition;\r\nbegin\r\n");
      jsClass.append("  def := js.defineClass('"+tj+"', nil, '"+tj+"', js.FHIRFactoryJs);\r\n");
      jsClass.append("  define"+tj+"PropsJs(js, def);\r\n");
      jsClass.append("end;\r\n\r\n");
    }
    jsCode.procs.add(jsClass.toString());
  }

  private void genTypeAbstract(ElementDefn root, String tn, String superClass, ClassCategory category, boolean isSpecial) throws Exception {
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
    StringBuilder empty = new StringBuilder();
    StringBuilder getkids = new StringBuilder();
    StringBuilder getkidsvars = new StringBuilder();
    StringBuilder getprops = new StringBuilder();
    StringBuilder getpropsvars = new StringBuilder();
    StringBuilder setprops = new StringBuilder();
    StringBuilder insprops = new StringBuilder();
    StringBuilder makeprops = new StringBuilder();
    StringBuilder delprops = new StringBuilder();
    StringBuilder replprops = new StringBuilder();
    StringBuilder reorderprops = new StringBuilder();
    impl.append("{ "+tn+" }\r\n\r\n");

    String tj = tn.substring(5);
    StringBuilder jsReg = new StringBuilder();
    if (!superClass.equalsIgnoreCase("TFhirObject")) {
      if (superClass.equalsIgnoreCase("TFhirType")) {
        jsReg.append("  defineElementPropsJs(js, def);\r\n");
      } else {
        jsReg.append("  define"+superClass.substring(5)+"PropsJs(js, def);\r\n");
      }
    }
    
    workingParserX = new StringBuilder();
    workingParserXA = new StringBuilder();
    workingComposerX = new StringBuilder();
    workingComposerXA = new StringBuilder();
    workingParserJ = new StringBuilder();
    workingComposerJ = new StringBuilder();
    workingParserT = new StringBuilder();
    workingComposerR = new StringBuilder();

    for (ElementDefn e : root.getElements()) {
      generateField(e, root.getName(), defPriv1, defPriv2, defPub, impl, create, destroy, assign, empty, getkids, getkidsvars, getprops, getpropsvars, setprops, insprops, makeprops, delprops, replprops, reorderprops, tn, "", category, e.getName().equals("Extension"), jsReg, tj);
    }

    def.append("  {@Class "+tn+" : "+superClass+"\r\n");
    def.append("    "+makeDocoSafe(root.getDefinition())+"\r\n");
    def.append("  }\r\n");
    def.append("  "+tn+" = {abstract} class ("+superClass+")\r\n");
    types.add(tn);
    def.append("  protected\r\n");
    def.append(defPriv1.toString());
    def.append(defPriv2.toString());
    def.append("  \r\n");
    def.append("    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;\r\n");
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
    def.append("    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;\r\n");
    def.append("    function createPropertyValue(propName : string) : TFHIRObject; override;\r\n");
    def.append("    procedure deleteProperty(propName : string; value : TFHIRObject); override;\r\n");
    def.append("    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;\r\n");
    def.append("    procedure reorderProperty(propName : string; source, destination : integer); override;\r\n");
    def.append("    function fhirType : string; override;\r\n");
    def.append("    function equalsDeep(other : TFHIRObject) : boolean; override;\r\n");
    def.append("    function equalsShallow(other : TFHIRObject) : boolean; override;\r\n");
    def.append("    function isEmpty : boolean; override;\r\n");
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
    impl2.append("procedure "+tn+".GetChildrenByName(child_name : string; list : TFHIRSelectionList);\r\n");
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
    impl2.append("procedure "+tn+".insertProperty(propName: string; propValue: TFHIRObject; index : integer);\r\n");
    impl2.append("begin\r\n");
    if (insprops.length() > 7) {
      impl2.append("  "+insprops.toString().substring(7));
      impl2.append("  else inherited;\r\n");
    } else {
      impl2.append("  inherited;\r\n");
    }
    impl2.append("end;\r\n\r\n");
    impl2.append("function "+tn+".createPropertyValue(propName: string) : TFHIRObject;\r\n");
    impl2.append("begin\r\n");
    if (makeprops.length() > 7) {
      impl2.append("  "+makeprops.toString().substring(7));
      impl2.append("  else result := inherited createPropertyValue(propName);\r\n");
    } else
      impl2.append("  result := inherited createPropertyValue(propName);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".deleteProperty(propName: string; value : TFHIRObject);\r\n");
    impl2.append("begin\r\n");
    if (delprops.length() > 7) {
      impl2.append("  "+delprops.toString().substring(7));
      impl2.append("  else\r\n    inherited deleteProperty(propName, value);\r\n");
    } else
      impl2.append("  inherited deleteProperty(propName, value);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".replaceProperty(propName : string; existing, new : TFHIRObject);\r\n");
    impl2.append("begin\r\n");
    if (replprops.length() > 7) {
      impl2.append("  "+replprops.toString().substring(7));
      impl2.append("  else\r\n    inherited replaceProperty(propName, existing, new);\r\n");
    } else
      impl2.append("  inherited replaceProperty(propName, existing, new);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".reorderProperty(propName : string; source, destination : integer);\r\n");
    impl2.append("begin\r\n");
    if (reorderprops.length() > 7) {
      impl2.append("  "+reorderprops.toString().substring(7));
      impl2.append("  else\r\n    inherited reorderProperty(propName, source, destination);\r\n");
    } else
      impl2.append("  inherited reorderProperty(propName, source, destination);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function "+tn+".fhirType : string;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := '"+root.getName()+"';\r\n");
    impl2.append("end;\r\n\r\n");
    generateEquals(root, tn, impl2);

    impl2.append("function "+tn+".isEmpty : boolean;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := inherited isEmpty "+empty.toString()+";\r\n");
    impl2.append("end;\r\n\r\n");

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

    StringBuilder jsClass = new StringBuilder(); 
    jsClass.append("procedure define"+tj+"PropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);\r\n");
    jsClass.append("begin\r\n");
    if (tj.equalsIgnoreCase("MetadataResource")) {
      String s = jsReg.toString();
      s = s.substring(0, s.indexOf("\r")+2);
      jsClass.append(s);
    } else
      jsClass.append(jsReg.toString());
    jsClass.append("end;\r\n\r\n");
    jsCode.procs.add(jsClass.toString());
    
    if (!isSpecial) {
      prsrdefX.append("    Procedure Parse"+root.getName()+"Attributes(resource : "+tn+"; path : string; element : TMXmlElement);\r\n");
      prsrImplX.append("Procedure TFHIRXmlParser.Parse"+root.getName()+"Attributes(resource : "+tn+"; path : string; element : TMXmlElement);\r\n");
      prsrImplX.append("begin\r\n");
      if (!isBase)
        prsrImplX.append("  Parse"+root.typeCode()+"Attributes(resource, path, element);\r\n");
      else
        prsrImplX.append("  GetObjectLocation(resource, element);\r\n");      
      prsrImplX.append(workingParserXA.toString());
      prsrImplX.append("end;\r\n\r\n");
      prsrdefX.append("    Function Parse"+root.getName()+"Child(resource : "+tn+"; path : string; child : TMXmlElement) : boolean;\r\n");
      prsrImplX.append("Function TFHIRXmlParser.Parse"+root.getName()+"Child(resource : "+tn+"; path : string; child : TMXmlElement) : boolean;\r\n");
      prsrImplX.append("begin\r\n");
      prsrImplX.append("  result := true;\r\n");
      prsrImplX.append("  "+workingParserX.toString().substring(11).replace("      ", "  ").replace("result.", "resource."));
      if (isBase)
        prsrImplX.append("  else\r\n");
      else
        prsrImplX.append("  else if not parse"+root.typeCode()+"Child(resource, path, child) then\r\n");
      prsrImplX.append("    result := false;\r\n");
      prsrImplX.append("end;\r\n\r\n");

      prsrdefJ.append("    procedure Parse"+root.getName()+"Properties(jsn : TJsonObject; resource : "+tn+");\r\n");
      prsrdefT.append("    procedure Parse"+root.getName()+"Properties(obj : TTurtleComplex; resource : "+tn+");\r\n");
      prsrImplJ.append("procedure TFHIRJsonParser.Parse"+root.getName()+"Properties(jsn : TJsonObject; resource : "+tn+");\r\n");
      prsrImplJ.append("begin\r\n");
      if (!isBase)
        prsrImplJ.append("  Parse"+root.typeCode()+"Properties(jsn, resource);\r\n");
      else {
        prsrImplJ.append("  resource.LocationStart := jsn.LocationStart;\r\n");
        prsrImplJ.append("  resource.LocationEnd := jsn.LocationEnd;\r\n");
      }

      prsrImplJ.append(workingParserJ.toString().replace("    ", "  ").replace("result.", "resource."));
      prsrImplJ.append("end;\r\n\r\n");

      prsrImplT.append("procedure TFHIRTurtleParser.Parse"+root.getName()+"Properties(obj : TTurtleComplex; resource : "+tn+");\r\n");
      if (workingParserT.toString().contains("for item") || workingParserT.toString().contains("if obj.has(")) {
        prsrImplT.append("var\r\n");
        prsrImplT.append("  item : TTurtleComplex;\r\n");
      }      
      prsrImplT.append("begin\r\n");
      if (!isBase)
        prsrImplT.append("  Parse"+root.typeCode()+"Properties(obj, resource);\r\n");
      else {
        prsrImplT.append("  resource.LocationStart := obj.Start;\r\n");
        prsrImplT.append("  resource.LocationEnd := obj.Stop;\r\n");
      }

      prsrImplT.append(workingParserT.toString().replace("    ", "  ").replace("result.", "resource."));
      prsrImplT.append("end;\r\n\r\n");

      srlsdefX.append("    Procedure Compose"+root.getName()+"Attributes(xml : TXmlBuilder; resource : "+tn+");\r\n");
      prsrImplX.append("Procedure TFHIRXmlComposer.Compose"+root.getName()+"Attributes(xml : TXmlBuilder; resource : "+tn+");\r\n");
      prsrImplX.append("begin\r\n");
      if (!isBase)
        prsrImplX.append("  Compose"+root.typeCode()+"Attributes(xml, resource);\r\n");
      prsrImplX.append(workingComposerXA.toString());        
      prsrImplX.append("end;\r\n\r\n");
      srlsdefX.append("    Procedure Compose"+root.getName()+"Children(xml : TXmlBuilder; elem : "+tn+");\r\n");
      prsrImplX.append("Procedure TFHIRXmlComposer.Compose"+root.getName()+"Children(xml : TXmlBuilder; elem : "+tn+");\r\n");
      if (workingComposerX.toString().contains("i :=")) {
        prsrImplX.append("var\r\n");
        prsrImplX.append("  i : integer;{z.a}\r\n");
      }
      prsrImplX.append("begin\r\n");
      if (!isBase)
        prsrImplX.append("  compose"+root.typeCode()+"Children(xml, elem);\r\n");
      prsrImplX.append(workingComposerX.toString());        
      prsrImplX.append("end;\r\n\r\n");

      srlsdefJ.append("    Procedure Compose"+root.getName()+"Properties(json : TJSONWriter; elem : "+tn+");\r\n");
      prsrImplJ.append("Procedure TFHIRJsonComposer.Compose"+root.getName()+"Properties(json : TJSONWriter; elem : "+tn+");\r\n");
      if (workingComposerJ.toString().contains("i :=")) {
        prsrImplJ.append("var\r\n");
        prsrImplJ.append("  i : integer{z.b};\r\n");
      }
      prsrImplJ.append("begin\r\n");
      if (!isBase)
        prsrImplJ.append("  Compose"+root.typeCode()+"Properties(json, elem);\r\n");
      prsrImplJ.append(workingComposerJ.toString());        
      prsrImplJ.append("end;\r\n\r\n");

      srlsdefR.append("    Procedure Compose"+root.getName()+"(this : TTurtleComplex; parentType, name : String; elem : "+tn+"; useType : boolean; index : integer); overload;\r\n");
      prsrImplT.append("Procedure TFHIRTurtleComposer.Compose"+root.getName()+"(this : TTurtleComplex; parentType, name : String; elem : "+tn+"; useType : boolean; index : integer);\r\n");
      if (workingComposerR.toString().contains("i :=")) {
        prsrImplT.append("var\r\n");
        prsrImplT.append("  i : integer{z.c};\r\n");
      }
      prsrImplT.append("begin\r\n");
      if (!isBase)
        prsrImplT.append("  Compose"+root.typeCode()+"(this, '', name, elem, false, index);\r\n");
      prsrImplT.append(workingComposerR.toString().replace("%%%%", root.getName()));        
      prsrImplT.append("end;\r\n\r\n");
    }
  }
  //
  //  private void genResource(ResourceDefn root, String tn, String superClass, ClassCategory category) throws Exception {
  //    prsrdefX.append("    function Parse"+root.getName()+"(element : TMXmlElement; path : string) : TFhir"+root.getName()+";\r\n");
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
  //    def.append("  protected\r\n");
  //    def.append(defPriv1.toString());
  //    def.append(defPriv2.toString());
  //    def.append("  \r\n");
  //    def.append("    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList"); override;\r\n");
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
  //    def.append("    function equalsDeep(other : TFHIRObject) : boolean; override;\r\n");
  //    def.append("    function equalsShallow(other : TFHIRObject) : boolean; override;\r\n");
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
  //    impl2.append("procedure "+tn+".GetChildrenByName(child_name : string; list : TFHIRSelectionList);\r\n");
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
    indexHeaders.append("    {$IFDEF FHIR_"+r.getName().toUpperCase()+"}\r\n    procedure buildIndexesFor"+r.getName()+"(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);\r\n    {$ENDIF}\r\n");
    indexBody.append("  {$IFDEF FHIR_"+r.getName().toUpperCase()+"}\r\n  buildIndexesFor"+r.getName()+"(Indexes, compartments);\r\n  {$ENDIF}\r\n");
    b.append("{$IFDEF FHIR_"+r.getName().toUpperCase()+"}\r\nprocedure TFHIRIndexBuilder.buildIndexesFor"+r.getName()+"(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);\r\n");
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
      String s = "  indexes.add('"+r.getName()+"', '"+sp.getCode()+"', '"+defCodeType.escape(removeEoln(sp.getDescription()))+"', SearchParamType"+getTitle(sp.getType().toString())+", "+
          getTarget(sp.getWorkingTargets(), 800)+", '";
      int llen = getLastLineLength(s);
      b.append(s);
      b.append(breakStringConstIntoLines(defCodeType.escape(sp.getExpression()), llen));
      b.append("', SearchXpathUsage"+Utilities.capitalize(sp.getxPathUsage() == null ? "" : sp.getxPathUsage().toCode())+");\r\n");
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

    b.append("end;\r\n{$ENDIF}\r\n\r\n");
    indexMethods.append(b.toString());
  }


  private Object breakStringConstIntoLines(String str, int llen) {
    while (llen + getLastLineLength(str) > 1000) {
      str = str.substring(0, str.length()-(1000-llen))+"'+\r\n   '"+str.substring(0, str.length()-(1000-llen));
    }
    return str;
  }

  private int getLastLineLength(String s) {
    return s.length() - s.lastIndexOf("\n") - 1;
  }

  private String removeEoln(String s) {
    return s.replace("\r", " ").replace("\n", " ").replace("\t", "  ");
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
        if (rd.getSearchParams() != null) {
          names.addAll(rd.getSearchParams().keySet());
          params.putAll(rd.getSearchParams());
        }
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
          def.append("    "+prefix+getTitle(nf)+"); \r\n");
          con3.append("'"+defCodeType.escape(n)+"');");
        }
        else {
          def.append("    "+prefix+getTitle(nf)+", \r\n");
          con3.append("'"+defCodeType.escape(n)+"', ");
        }
      }

      defCodeConstGen.enumDefs.add("{$IFDEF FHIR_"+r.getName().toUpperCase()+"}\r\n"+def.toString()+"{$ENDIF}\r\n");
      defCodeConstGen.enumConsts.add("{$IFDEF FHIR_"+r.getName().toUpperCase()+"}\r\n"+Utilities.splitLineForLength(con3.toString(), 2, 6, 1023)+"\r\n{$ENDIF}");
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
      def.append("    "+prefix+"Null, // Value is missing from Instance \r\n");
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
          def.append("    "+cc+"); \r\n");
          con.append("'"+c.getCode()+"');");
          conS.append("'"+c.getSystem()+"');");
        }
        else {
          def.append("    "+cc+", \r\n");
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

    prsrdefX.append("    function Parse"+tn.substring(5)+"(element : TMXmlElement; path : string) : "+tn+";\r\n");
    if (!e.getName().equals("Element") && !e.getName().equals("BackboneElement"))
      prsrdefX.append("    function Parse"+tn.substring(5)+"Child(element : TFhir"+tn.substring(5)+"; path : string; child : TMXmlElement) : boolean;\r\n");
    srlsdefX.append("    procedure Compose"+tn.substring(5)+"(xml : TXmlBuilder; name : string; elem : "+tn+");\r\n");
    if (!e.getName().equals("Element") && !e.getName().equals("BackboneElement"))
      srlsdefX.append("    procedure Compose"+tn.substring(5)+"Children(xml : TXmlBuilder; elem : "+tn+");\r\n");
    prsrdefJ.append("    function Parse"+tn.substring(5)+"(jsn : TJsonObject) : "+tn+"; overload; {b\\}\r\n");
    prsrdefT.append("    function Parse"+tn.substring(5)+"(obj : TTurtleComplex) : "+tn+"; overload; {b\\}\r\n");
    if (!e.getName().equals("Element") && !e.getName().equals("BackboneElement")) {
      prsrdefJ.append("    procedure Parse"+tn.substring(5)+"Properties(jsn : TJsonObject; result : "+tn+"); overload; {b\\}\r\n");
      prsrdefT.append("    procedure Parse"+tn.substring(5)+"Properties(obj : TTurtleComplex; result : "+tn+"); overload; {b\\}\r\n");
    }
    srlsdefJ.append("    procedure Compose"+tn.substring(5)+"(json : TJSONWriter; name : string; elem : "+tn+"; noObj : boolean = false)"+(tn.equals("TFhirExtension") ? " override;" : "")+";\r\n");
    srlsdefR.append("    procedure Compose"+tn.substring(5)+"(parent :  TTurtleComplex; parentType, name : String; elem : "+tn+"; useType : boolean; index : integer)"+(tn.equals("TFhirExtension") ? " override;" : "")+";\r\n");
    compJBase.append("  else if (base is "+tn+") then\r\n    compose"+tn.substring(5)+"(json, name, "+tn+"(base), false)\r\n");
    compXBase.append("  else if (base is "+tn+") then\r\n    compose"+tn.substring(5)+"(xml, name,  "+tn+"(base))\r\n");
    compRBase.append("  else if (base is "+tn+") then\r\n    compose"+tn.substring(5)+"(section, name,  "+tn+"(base), true, -1)\r\n");
    String tj = tn.substring(5);
    jsRegM.append("  define"+tj+"Js(js); \r\n");
    StringBuilder jsReg = new StringBuilder();
    if (category == ClassCategory.Component) {
      jsReg.append("  defineBackboneElementPropsJs(js, def);\r\n");
    } else {
      jsReg.append("  defineElementPropsJs(js, def);\r\n");
    }

    workingParserX = new StringBuilder();
    workingParserXA = new StringBuilder();
    workingComposerX = new StringBuilder();
    workingComposerXA = new StringBuilder();
    workingParserJ = new StringBuilder();
    workingComposerJ = new StringBuilder();
    workingParserT = new StringBuilder();
    workingComposerR = new StringBuilder();

    StringBuilder def = new StringBuilder();
    StringBuilder defPriv1 = new StringBuilder();
    StringBuilder defPriv2 = new StringBuilder();
    StringBuilder defPub = new StringBuilder();
    StringBuilder impl = new StringBuilder();
    StringBuilder create = new StringBuilder();
    StringBuilder destroy = new StringBuilder();
    StringBuilder assign = new StringBuilder();
    StringBuilder empty = new StringBuilder();
    StringBuilder getkids = new StringBuilder();
    StringBuilder getkidsvars = new StringBuilder();
    StringBuilder getprops = new StringBuilder();
    StringBuilder getpropsvars = new StringBuilder();
    StringBuilder setprops = new StringBuilder();
    StringBuilder insprops = new StringBuilder();
    StringBuilder makeprops = new StringBuilder();
    StringBuilder delprops = new StringBuilder();
    StringBuilder replprops = new StringBuilder();
    StringBuilder reorderprops = new StringBuilder();

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
      generateField(c, e.getPath(), defPriv1, defPriv2, defPub, impl, create, destroy, assign, empty, getkids, getkidsvars, getprops, getpropsvars, setprops, insprops, makeprops, delprops, replprops, reorderprops, tn, "", category, e.getName().equals("Extension"), jsReg, tj);
    }

    def.append("  protected\r\n");
    def.append(defPriv1.toString());
    def.append(defPriv2.toString());
    def.append("  \r\n");
    def.append("    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;\r\n");
    def.append("    Procedure ListProperties(oList : "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues : Boolean); Override;\r\n");
    def.append("  public\r\n");
    def.append("    constructor Create; Override;\r\n");
    def.append("    destructor Destroy; override;\r\n");
    def.append("    {!script hide}\r\n");
    def.append("    procedure Assign(oSource : TAdvObject); override;\r\n");
    def.append("    function Link : "+tn+"; overload;\r\n");
    def.append("    function Clone : "+tn+"; overload;\r\n");
    def.append("    procedure setProperty(propName : string; propValue : TFHIRObject); override;\r\n");
    def.append("    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;\r\n");
    def.append("    function createPropertyValue(propName : string) : TFHIRObject; override;\r\n");
    def.append("    procedure deleteProperty(propName : string; value : TFHIRObject); override;\r\n");
    def.append("    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;\r\n");
    def.append("    procedure reorderProperty(propName : string; source, destination : integer); override;\r\n");
    def.append("    function fhirType : string; override;\r\n");
    def.append("    function equalsDeep(other : TFHIRObject) : boolean; override;\r\n");
    def.append("    function equalsShallow(other : TFHIRObject) : boolean; override;\r\n");
    def.append("    function isEmpty : boolean; override;\r\n");
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
    impl2.append("procedure "+tn+".GetChildrenByName(child_name : string; list : TFHIRSelectionList);\r\n");
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
    impl2.append("procedure "+tn+".insertProperty(propName: string; propValue: TFHIRObject; index : integer);\r\n");
    impl2.append("begin\r\n");
    if (insprops.length() > 7) {
      impl2.append("  "+insprops.toString().substring(7));
      impl2.append("  else inherited;\r\n");
    } else {
      impl2.append("  inherited;\r\n");
    }
    impl2.append("end;\r\n\r\n");
    impl2.append("function "+tn+".createPropertyValue(propName : string) : TFHIRObject;\r\n");
    impl2.append("begin\r\n");
    if (makeprops.length() > 7) {
      impl2.append("  "+makeprops.toString().substring(7));
      impl2.append("  else result := inherited createPropertyValue(propName);\r\n");
    } else
      impl2.append("  result := inherited createPropertyValue(propName);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".deleteProperty(propName: string; value : TFHIRObject);\r\n");
    impl2.append("begin\r\n");
    if (delprops.length() > 7) {
      impl2.append("  "+delprops.toString().substring(7));
      impl2.append("  else\r\n    inherited deleteProperty(propName, value);\r\n");
    } else
      impl2.append("  inherited deleteProperty(propName, value);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".replaceProperty(propName : string; existing, new : TFHIRObject);\r\n");
    impl2.append("begin\r\n");
    if (replprops.length() > 7) {
      impl2.append("  "+replprops.toString().substring(7));
      impl2.append("  else\r\n    inherited replaceProperty(propName, existing, new);\r\n");
    } else
      impl2.append("  inherited replaceProperty(propName, existing, new);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("procedure "+tn+".reorderProperty(propName : string; source, destination : integer);\r\n");
    impl2.append("begin\r\n");
    if (reorderprops.length() > 7) {
      impl2.append("  "+reorderprops.toString().substring(7));
      impl2.append("  else\r\n    inherited reorderProperty(propName, source, destination);\r\n");
    } else
      impl2.append("  inherited reorderProperty(propName, source, destination);\r\n");
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

    impl2.append("function "+tn+".isEmpty : boolean;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := inherited isEmpty "+empty.toString()+";\r\n");
    impl2.append("end;\r\n\r\n");

    getCode(category).classDefs.add(def.toString());
    getCode(category).classImpls.add(impl2.toString() + impl.toString());
    getCode(category).classFwds.add("  "+tn+" = class;\r\n");
    generateParser(e, tn, category, true, e.typeCode());
    defineList(tn, tn+"List", null, category, category == ClassCategory.AbstractResource, false);

    StringBuilder jsClass = new StringBuilder(); 
    jsClass.append("procedure define"+tj+"PropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition);\r\n");
    jsClass.append("begin\r\n");
    if (tj.equalsIgnoreCase("MetadataResource")) 
      jsClass.append("3. "+jsReg.toString());
    else
      jsClass.append(jsReg.toString());
    jsClass.append("end;\r\n\r\n");
    jsClass.append("procedure define"+tj+"Js(js : TFHIRJavascript);\r\n");
    jsClass.append("var\r\n  def : TJavascriptClassDefinition;\r\nbegin\r\n");
    jsClass.append("  def := js.defineClass('"+tj+"', nil, '"+tj+"', js.FHIRFactoryJs);\r\n");
    jsClass.append("  define"+tj+"PropsJs(js, def);\r\n");
    jsClass.append("end;\r\n\r\n");
    jsCode.procs.add(jsClass.toString());
  }

  private void generateEquals(ElementDefn e, String tn, StringBuilder b) throws IOException {
    b.append("function "+tn+".equalsDeep(other : TFHIRObject) : boolean; \r\n");
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
      if (!c.isInherited()){
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
    }
    if (first)
      b.append("true"); 
    b.append(";\r\n");
    b.append("  end;\r\n");
    b.append("end;\r\n\r\n");

    b.append("function "+tn+".equalsShallow(other : TFHIRObject) : boolean; \r\n");
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
      if (!c.isInherited()){
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
    }
    if (first)
      b.append("true"); 
    b.append(";\r\n");
    b.append("  end;\r\n");
    b.append("end;\r\n\r\n");
  }

  private void generatePrimitiveEquals(DefinedCode t, String tn, StringBuilder b) throws IOException {
    b.append("function "+tn+".equalsDeep(other : TFHIRObject) : boolean; \r\n");
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
    if (Utilities.existsInList(t.getCode(), "date", "dateTime", "instant"))
      b.append("    result := o.value.equal(value);\r\n");
    else
      b.append("    result := o.value = value;\r\n");
    b.append("  end;\r\n");
    b.append("end;\r\n\r\n");

    b.append("function "+tn+".equalsShallow(other : TFHIRObject) : boolean; \r\n");
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
    if (Utilities.existsInList(t.getCode(), "date", "dateTime", "instant"))
      b.append("    result := o.value.equal(value);\r\n");
    else
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
    prsrImplX.append(
        "function TFHIRXmlParser.Parse"+tn.substring(5)+"(element : TMXmlElement; path : string) : "+tn+";\r\n"+
            "var\r\n"+
        "  child : TMXmlElement;\r\n");

    prsrImplX.append(
        "begin\r\n"+
            "  result := "+tn+".create;\r\n"+
        "  try\r\n");
    if (isElement)
      if (category == ClassCategory.Resource)
        prsrImplX.append("    parse"+parent+"Attributes(result, path, element);\r\n");
      else
        prsrImplX.append("    parseElementAttributes(result, path, element);\r\n");

    prsrImplX.append(workingParserXA.toString());

    prsrImplX.append(
        "    child := FirstChild(element);\r\n"+
            "    while (child <> nil) do\r\n"+
            "    begin\r\n"+
            "      if not Parse"+tn.substring(5)+"Child(result, path, child) then\r\n"+
            "        UnknownContent(child, path);\r\n"+
            "      child := NextSibling(child);\r\n"+
        "    end;\r\n");
    if (isElement)
      prsrImplX.append(
          "    closeOutElement(result, element);\r\n");
    prsrImplX.append(
        "\r\n"+
            "    result.link;\r\n"+
            "  finally\r\n"+
            "    result.free;\r\n"+
            "  end;\r\n"+
            "end;\r\n\r\n"
        );
    if (!tn.equals("TFhirElement") && !tn.equals("TFhirBackboneElement")) {
      prsrImplX.append(
          "function TFHIRXmlParser.Parse"+tn.substring(5)+"Child(element : "+tn+"; path : string; child : TMXmlElement) : boolean;\r\n"+
              "begin\r\n"+
          "  result := true;\r\n");

      if (s.length() >= 11)
        prsrImplX.append("      "+s.substring(11)+"      else ");
      else
        prsrImplX.append("      ");

      if (!isElement)
        prsrImplX.append("\r\n");
      else if (category == ClassCategory.Resource)
        prsrImplX.append("if Not Parse"+parent+"Child(element, path, child) then\r\n");
      else if (category == ClassCategory.Component)
        prsrImplX.append("if Not ParseBackboneElementChild(element, path, child) then\r\n");
      else if (hasExplicitParent(e))
        prsrImplX.append("if Not Parse"+e.typeCode()+"Child(element, path, child) then\r\n");
      else
        prsrImplX.append("if Not ParseElementChild(element, path, child) then\r\n");
      prsrImplX.append("    result := false;\r\n");
      prsrImplX.append("end;\r\n\r\n");
    }
    s = workingComposerX.toString();
    prsrImplX.append(
        "procedure TFHIRXmlComposer.Compose"+tn.substring(5)+"(xml : TXmlBuilder; name : String; elem : "+tn+");\r\n");
    prsrImplX.append(
        "begin\r\n"+
        "  if (elem = nil) then\r\n    exit;\r\n");
    if (isElement)
      if (category == ClassCategory.Resource)
        prsrImplX.append("  compose"+parent+"Attributes(xml, elem);\r\n");
      else 
        prsrImplX.append("  composeElementAttributes(xml, elem);\r\n");
    prsrImplX.append(workingComposerXA.toString());        
    prsrImplX.append(
        "  xml.open(name);\r\n");
    prsrImplX.append("  compose"+tn.substring(5)+"Children(xml, elem);\r\n");
    if (isElement)
      prsrImplX.append("  closeOutElement(xml, elem);\r\n");
    prsrImplX.append(
        "  xml.close(name);\r\n"+
            "end;\r\n\r\n"
        );

    if (!tn.equals("TFhirElement") && !tn.equals("TFhirBackboneElement")) {
      prsrImplX.append(
          "procedure TFHIRXmlComposer.Compose"+tn.substring(5)+"Children(xml : TXmlBuilder; elem : "+tn+");\r\n");
      boolean var = false;
      if (s.contains("for i := ")) {
        prsrImplX.append("var\r\n  i : integer;\r\n");
        var = true;
      }
      if (s.contains("ext := ")) {
        if (!var) 
          prsrImplX.append("var\r\n");
        prsrImplX.append("  ext : boolean;\r\n");
        prsrImplX.append("  val : boolean;\r\n");
      }
      prsrImplX.append(
          "begin\r\n");
      if (isElement)
        if (category == ClassCategory.Resource)
          prsrImplX.append("  compose"+parent+"Children(xml, elem);\r\n");
        else if (category == ClassCategory.Component)
          prsrImplX.append("  composeBackboneElementChildren(xml, elem);\r\n");
        else if (Utilities.noString(parent))
          prsrImplX.append("  composeElementChildren(xml, elem);\r\n");
        else
          prsrImplX.append("  compose"+parent+"Children(xml, elem);\r\n");

      prsrImplX.append(s);
      prsrImplX.append(
          "end;\r\n\r\n"
          );
    }

    prsrdefJ.append("    procedure Parse"+tn.substring(5)+"(jsn : TJsonObject; ctxt : "+listForm("TFHIRObject")+"); overload; {b.}\r\n");
    prsrImplJ.append("procedure TFHIRJsonParser.Parse"+tn.substring(5)+"(jsn : TJsonObject; ctxt : "+listForm("TFHIRObject")+");\r\n");
    prsrImplJ.append("begin\r\n");
    prsrImplJ.append("  ctxt.add(Parse"+tn.substring(5)+"(jsn)); {2}\r\n");
    prsrImplJ.append("end;\r\n\r\n");

    prsrImplJ.append(
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
    prsrImplT.append(
        "function TFHIRTurtleParser.Parse"+tn.substring(5)+"(obj : TTurtleComplex) : "+tn+";\r\n"+
            "begin\r\n"+
            "  if (obj = nil) then\r\n"+
            "    exit(nil);\r\n"+      
            "  result := "+tn+".create;\r\n"+
            "  try\r\n"+
            "    Parse"+tn.substring(5)+"Properties(obj, result);\r\n"+ 
            "    result.link;\r\n"+
            "  finally\r\n"+
            "    result.free;\r\n"+
            "  end;\r\n"+
            "end;\r\n\r\n"
        );
    if (!tn.equals("TFhirElement") && !tn.equals("TFhirBackboneElement")) {
      prsrImplJ.append(
          "procedure TFHIRJsonParser.Parse"+tn.substring(5)+"Properties(jsn : TJsonObject; result : "+tn+");\r\n"+
          "begin\r\n");
      s = workingParserJ.toString();
      if (isElement) {
        if (category == ClassCategory.Resource)
          prsrImplJ.append("    Parse"+parent+"Properties(jsn, result);\r\n");
        else if (category == ClassCategory.Component)
          prsrImplJ.append("    ParseBackboneElementProperties(jsn, result);\r\n");
        else if (Utilities.noString(parent))
          prsrImplJ.append("    ParseElementProperties(jsn, result);\r\n");
        else
          prsrImplJ.append("    Parse"+parent+"Properties(jsn, result);\r\n");
      }
      prsrImplJ.append(s);
      prsrImplJ.append(
          "end;\r\n\r\n");
      prsrImplT.append(
          "procedure TFHIRTurtleParser.Parse"+tn.substring(5)+"Properties(obj : TTurtleComplex; result : "+tn+");\r\n");
      if (workingParserT.toString().contains("for item") || workingParserT.toString().contains("if obj.has(")) {
        prsrImplT.append("var\r\n");
        prsrImplT.append("  item : TTurtleComplex;\r\n");
      }
      prsrImplT.append("begin\r\n");
      s = workingParserT.toString();
      if (isElement) {
        if (category == ClassCategory.Resource)
          prsrImplT.append("    Parse"+parent+"Properties(obj, result);\r\n");
        else if (category == ClassCategory.Component)
          prsrImplT.append("    ParseBackboneElementProperties(obj, result);\r\n");
        else if (Utilities.noString(parent))
          prsrImplT.append("    ParseElementProperties(obj, result);\r\n");
        else
          prsrImplT.append("    Parse"+parent+"Properties(obj, result);\r\n");
      }
      prsrImplT.append(s);
      prsrImplT.append(
          "end;\r\n\r\n");
    }

    s = workingComposerJ.toString();
    prsrImplJ.append(
        "procedure TFHIRJsonComposer.Compose"+tn.substring(5)+"(json : TJSONWriter; name : string; elem : "+tn+"; noObj : boolean = false);\r\n");
    boolean var = false;
    if (s.contains("for i := ")) { // || category == ClassCategory.Resource) {
      prsrImplJ.append("var\r\n  i : integer;\r\n");
      var = true;
    }
    if (s.contains("ext := ")) {
      if (!var) 
        prsrImplJ.append("var\r\n");
      prsrImplJ.append("  ext : boolean;\r\n");
      prsrImplJ.append("  val : boolean;\r\n");
    }

    if (category == ClassCategory.Resource)
      prsrImplJ.append(
          "begin\r\n"+
          "  if (elem = nil) then\r\n    exit;\r\n");
    else 
      prsrImplJ.append(
          "begin\r\n"+
              "  if (elem = nil) then\r\n    exit;\r\n"+
          "  if not noObj then json.valueObject(name);\r\n");
    if (isElement)
      if (category == ClassCategory.Resource)
        prsrImplJ.append("  Compose"+parent+"Properties(json, elem);\r\n");
      else if (category == ClassCategory.Component)
        prsrImplJ.append("  ComposeBackboneElementProperties(json, elem);\r\n");
      else if (!Utilities.noString(parent))
        prsrImplJ.append("  Compose"+parent+"(json, '', elem, true);\r\n");
      else if (!tn.substring(5).equals("Element"))
        prsrImplJ.append("  ComposeElementProperties(json, elem);\r\n");


    prsrImplJ.append(s);
    if (category == ClassCategory.Resource)
      prsrImplJ.append(
          "end;\r\n\r\n");
    else
      prsrImplJ.append(
          "  if not noObj then json.finishObject;\r\n"+
          "end;\r\n\r\n");

    s = workingComposerR.toString().replace("%%%%", e.getPath());

    prsrImplT.append("procedure TFHIRTurtleComposer.Compose"+tn.substring(5)+"(parent :  TTurtleComplex; parentType, name : String; elem : "+tn+"; useType : boolean; index : integer);\r\n");
    prsrImplT.append("var\r\n  this : TTurtleComplex;\r\n");
    if (tn.substring(5).equals("Quantity")) 
      prsrImplT.append("var\r\n  cb, c : TTurtleComplex;\r\n");
    if (s.contains("for i := "))
      prsrImplT.append("  i : integer;\r\n");
    if (s.contains("ext := ")) {
      prsrImplT.append("  ext : boolean;\r\n");
      prsrImplT.append("  val : boolean;\r\n");
    }
    prsrImplT.append(
        "begin\r\n"+
        "  if (elem = nil) then\r\n    exit;\r\n");
    if (tn.substring(5).equals("Element"))
      prsrImplT.append("    this := parent;\r\n");
    else 
      prsrImplT.append(
          "  if (parentType = '') then\r\n"+    
              "    this := parent\r\n"+    
              "  else\r\n"+    
              "  begin\r\n"+    
              "    this := parent.addPredicate('fhir:'+parentType+'.'+name);\r\n"+
              "    if (useType) then\r\n"+
              "      this.addPredicate('a', 'fhir:"+tn.substring(5)+"'); {z}\r\n"+
          "  end;\r\n");
    if (category == ClassCategory.Resource)
      prsrImplT.append("  compose"+parent+"(this, '', name, elem, false, index);\r\n");
    else if (category == ClassCategory.Component)
      prsrImplT.append("  composeBackboneElement(this, '', name, elem, false, index);\r\n");
    else if (tn.substring(5).equals("Element"))
      prsrImplT.append("  if (index > -1)  then\r\n    this.addPredicate('fhir:index', inttostr(index), 'int');\r\n");
    else
      prsrImplT.append("  composeElement(this, '', name, elem, false, index);\r\n");

    prsrImplT.append(s);
    prsrImplT.append(
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

  private void generateField(ElementDefn e, String path, StringBuilder defPriv1, StringBuilder defPriv2, StringBuilder defPub, StringBuilder impl, StringBuilder create, StringBuilder destroy, StringBuilder assign, StringBuilder empty, StringBuilder getkids, StringBuilder getkidsvars, StringBuilder getprops, StringBuilder getpropsvars, StringBuilder setprops, StringBuilder insprops, StringBuilder makeprops, StringBuilder delprops, StringBuilder replprops, StringBuilder reorderprops, String cn, String pt, ClassCategory category, boolean isExtension, StringBuilder jsReg, String tj) throws Exception {

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
      } else if (tn.equals("TDateTimeEx")) {
        parse = "TDateTimeEx.fromXml(child.text)";
        propV = propV+".toXML";
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
    } else if (tn.equals("TDateTimeEx") || tn.equals("TFhirDateTime") || tn.equals("TFhirDate") || tn.equals("TFhirInstant")) {
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
      } else if (tn.equals("TDateTimeEx")) {
        srls = "#.ToXml";
      };
    }

    String s = getElementName(e.getName());
    String sumSet = "";
    if (s.equals("total") && path.startsWith("Bundle"))
      sumSet = "soFull, soSummary, soText, soData, soCount";
    else if (s.equals("id") || path == null || path.startsWith("Bundle") || path.startsWith("Parameters") || definitions.hasBaseType(path) )
      sumSet = "soFull, soSummary, soText, soData";
    else if ((s.equals("text") && path.equals("DomainResource")) || (path.equals("Narrative")) )
      sumSet = "soFull, soText";
    else if (e.isSummary())
      sumSet = "soFull, soSummary, soData";
    else
      sumSet = "soFull, soData";
    String dc = (category == ClassCategory.Resource || category == ClassCategory.AbstractResource) && !path.contains(".") && !path.contains("Bundle") && !s.equals("id") ? "and doCompose('"+s+"') " : "";
    String defaultValueTest = "";
    if (e.hasDefaultValue()) {
      if (e.typeCode().equals("boolean"))  
        defaultValueTest = "and (not isCanonical or (elem."+s+" <> "+e.getDefaultValue()+")) ";
      else if (e.typeCode().equals("code"))
        defaultValueTest = "and (not isCanonical or (elem."+s+"Element <> nil) and (elem."+s+"Element.primitiveValue <> '"+e.getDefaultValue()+"')) ";
      else // decimal code integer positiveInt unsignedInt
        defaultValueTest = "and (not isCanonical or (elem."+s+" <> '"+e.getDefaultValue()+"')) ";
    }

    //    boolean summary = e.isSummary() || noSummaries;
    //    String sumAnd = summary ? "" : "Not SummaryOnly and ";
    //    String sum2 = summary ? "" : "if not SummaryOnly then\r\n    ";
    if (e.unbounded()) {
      jsRegisterUnboundedField(jsReg, tj, tn, e);
      if (enumNames.contains(tn)) {
        if (!e.isInherited()) {
          defPriv1.append("    F"+getTitle(s)+" : "+listForm("TFhirEnum")+";\r\n");
          empty.append(" and isEmptyProp(F"+getTitle(s)+")");
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
        }
        defPub.append("    property has"+Utilities.capitalize(s)+" : boolean read GetHas"+getTitle(s)+";\r\n");
        // no, do it lazy create.append("  F"+getTitle(s)+" := "+listForm("TFHIREnum")+".Create;\r\n");
        if (!e.isInherited()) {
          impl.append("Function "+cn+".Get"+getTitle(s)+" : "+listForm("TFhirEnum")+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    F"+getTitle(s)+" := "+listForm("TFHIREnum")+".Create(SYSTEMS_"+tn+", CODES_"+tn+");\r\n  result := F"+getTitle(s)+";\r\nend;\r\n\r\n");
          impl.append("Function "+cn+".GetHas"+getTitle(s)+" : boolean;\r\nbegin\r\n  result := (F"+getTitle(s)+" <> nil) and (F"+getTitle(s)+".count > 0);\r\nend;\r\n\r\n");
          destroy.append("  F"+getTitle(s)+".Free;\r\n");
          if (generics)
            getkidsvars.append("  o : TFHIREnum;\r\n");
          if (e.getName().endsWith("[x]") || e.getName().equals("[type]")) 
            if (generics)
              getkids.append("  if StringStartsWith(child_name, '"+getPropertyName(e.getName())+"') Then\r\n    for o in F"+getTitle(s)+" do\r\n       list.add(self.link, '"+e.getName()+"', o.Link);\r\n");
            else
              getkids.append("  if StringStartsWith(child_name, '"+getPropertyName(e.getName())+"') Then\r\n    list.addAll(self, '"+e.getName()+"', F"+getTitle(s)+");\r\n");
          else
            if (generics)
              getkids.append("  if (child_name = '"+e.getName()+"') Then\r\n     for o in F"+getTitle(s)+" do\r\n       list.add(self.link, '"+e.getName()+"', o.Link);\r\n");
            else
              getkids.append("  if (child_name = '"+e.getName()+"') Then\r\n     list.addAll(self, '"+e.getName()+"', F"+getTitle(s)+");\r\n");
          if (generics) {
            getpropsvars.append("  o : TFHIREnum;\r\n");
            getprops.append("  prop := oList[oList.add(TFHIRProperty.create(self, '"+e.getName()+"', '"+breakConstant(e.typeCode())+"'))];\r\n  for o in F"+getTitle(s)+" do\r\n      prop,list.add(o.Link){3};\r\n");
          } else
            getprops.append("  oList.add(TFHIRProperty.create(self, '"+e.getName()+"', '"+breakConstant(e.typeCode())+"', true, TFHIREnum, F"+getTitle(s)+".Link)){3};\r\n");
          if (e.getName().endsWith("[x]"))
            throw new Exception("Not done yet");
          setprops.append("  else if (propName = '"+e.getName()+"') then "+getTitle(s)+"List.add(asEnum(SYSTEMS_"+tn+", CODES_"+tn+", propValue)) {1}\r\n");
          insprops.append("  else if (propName = '"+e.getName()+"') then F"+getTitle(s)+".insertItem(index, asEnum(SYSTEMS_"+tn+", CODES_"+tn+", propValue)) {1}\r\n");
          reorderprops.append("  else if (propName = '"+e.getName()+"') then F"+getTitle(s)+".move(source, destination) {1}\r\n");
        }
        String obj = "";
        if (enumSizes.get(tn) < 32) {
          if (!e.isInherited()) {
            impl.append("Function "+cn+".Get"+getTitle(s)+"ST : "+listForm(tn)+";\r\n  var i : integer;\r\nbegin\r\n  result := [];\r\n  if F"+s+" <> nil then\r\n    for i := 0 to F"+s+".count - 1 do\r\n      result := result + ["+tn+"(StringArrayIndexOfSensitive(CODES_"+tn+", F"+s+"[i].value))];\r\nend;\r\n\r\n");
            impl.append("Procedure "+cn+".Set"+getTitle(s)+"ST(value : "+listForm(tn)+");\r\nvar a : "+tn+";\r\nbegin\r\n  if F"+s+" = nil then\r\n    F"+s+" := TFhirEnumList.create(SYSTEMS_"+tn+", CODES_"+tn+");\r\n  F"+s+".clear;\r\n  for a := low("+tn+") to high("+tn+") do\r\n    if a in value then\r\n      begin\r\n         if F"+s+" = nil then\r\n           F"+s+" := TFhirEnumList.create(SYSTEMS_"+tn+", CODES_"+tn+");\r\n         F"+s+".add(TFhirEnum.create(SYSTEMS_"+tn+"[a], CODES_"+tn+"[a]));\r\n      end;\r\nend;\r\n\r\n");
          }
          obj = "List";
        }
        

        workingParserX.append("      else if (child.localName = '"+e.getName()+"') then\r\n"+
            "        result."+s+obj+".Add("+parse+"){y.1}\r\n");
        workingComposerX.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n    for i := 0 to elem."+s+obj+".Count - 1 do\r\n"+
            "      ComposeEnum(xml, '"+e.getName()+"', elem."+s+obj+"[i], CODES_"+tn+");\r\n");

        workingComposerR.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n    for i := 0 to elem."+s+obj+".Count - 1 do\r\n"+
              "      ComposeEnum(this, '%%%%', '"+e.getName()+"', elem."+s+obj+"[i], CODES_"+tn+", SYSTEMS_"+tn+", false, i); {x.d1}\r\n");

        workingParserJ.append(
            "    if jsn.has('"+e.getName()+"') or jsn.has('_"+e.getName()+"') then\r\n"+
                "      iterateEnumArray(jsn.vArr['"+e.getName()+"'], jsn.vArr['_"+e.getName()+"'], jsn.path+'/"+e.getName()+"', result."+s+obj+", parseEnum, CODES_"+tn+", SYSTEMS_"+tn+");\r\n");

        workingParserT.append(
            "    for item in obj.complexes('http://hl7.org/fhir/"+e.getPath()+"') do\r\n"+
            "      result."+s+obj+".Add(parseEnum(item, CODES_"+tn+", SYSTEMS_"+tn+"));\r\n");

        workingComposerJ.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+obj+".Count > 0) then\r\n");
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

          workingComposerR.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n    for i := 0 to elem."+s+obj+".Count - 1 do\r\n"+
              "      ComposeEnum(this, '%%%%', '"+e.getName()+"', elem."+s+obj+"[i], CODES_"+tn+", SYSTEMS_"+tn+", false, i);{x.d2}\r\n");
      } else {
        String tnl;
        if (tn.contains("{"))
          tnl = listForm(tn.substring(0, tn.indexOf('{')))+tn.substring(tn.indexOf('{'));
        else
          tnl = listForm(tn);
        s = s+"List";
        defPub.append("    {@member "+s+"\r\n");
        defPub.append("      "+makeDocoSafe(e.getDefinition())+"\r\n");
        defPub.append("    }\r\n");
        if (!e.isInherited()) {
          defPriv1.append("    F"+s+" : "+tnl+";\r\n");
          empty.append(" and isEmptyProp(F"+s+")");
          defPriv2.append("    function Get"+Utilities.capitalize(s)+" : "+tnl+";\r\n");
          defPriv2.append("    function GetHas"+Utilities.capitalize(s)+" : Boolean;\r\n");
        }
        defPub.append("    property "+s+" : "+tnl+" read Get"+getTitle(s)+";\r\n");
        defPub.append("    property has"+Utilities.capitalize(s)+" : boolean read GetHas"+getTitle(s)+";\r\n");
        defPub.append("\r\n");
        if (!e.isInherited()) {
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
          assign.append("    if F"+getTitle(s)+" = nil then\r\n      F"+getTitle(s)+" := "+tnl+".Create;\r\n");
          assign.append("    F"+getTitle(s)+".Assign("+cn+"(oSource).F"+getTitle(s)+");\r\n");
          assign.append("  end;\r\n");
          if (generics) {
            getkidsvars.append("  o"+getTitle(s)+" : "+tn+";\r\n");
            getkids.append("  if (child_name = '"+e.getName()+"') Then\r\n    for o"+getTitle(s)+" in F"+getTitle(s)+" do\r\n      list.add(self.link, '"+e.getName()+"', o"+getTitle(s)+");\r\n");
          } else
            getkids.append("  if (child_name = '"+e.getName()+"') Then\r\n    list.addAll(self, '"+e.getName()+"', F"+getTitle(s)+");\r\n");
          if (generics) {
            getpropsvars.append("  o"+getTitle(s)+" : "+tn+";\r\n");
            getprops.append("  prop := oList[oList.add(TFHIRProperty.create(self, '"+e.getName()+"', '"+breakConstant(e.typeCode())+"'))];\r\n  for o"+getTitle(s)+" in F"+getTitle(s)+" do\r\n    prop.List.add(o"+getTitle(s)+".Link){3a};\r\n");
          } else
            getprops.append("  oList.add(TFHIRProperty.create(self, '"+e.getName()+"', '"+breakConstant(e.typeCode())+"', true, "+tn+", F"+getTitle(s)+".Link)){3};\r\n");
          if (e.getName().endsWith("[x]"))
            throw new Exception("Not done yet");
          if (typeIsPrimitive(e.typeCode())) {
            setprops.append("  else if (propName = '"+e.getName()+"') then "+getTitle(s)+".add(as"+tn.substring(5)+"(propValue)){2}\r\n");
            insprops.append("  else if (propName = '"+e.getName()+"') then "+getTitle(s)+".insertItem(index, as"+tn.substring(5)+"(propValue)){2}\r\n");
            reorderprops.append("  else if (propName = '"+e.getName()+"') then "+getTitle(s)+".move(source, destination){2}\r\n");
          } else {
            setprops.append("  else if (propName = '"+e.getName()+"') then "+getTitle(s)+".add(propValue as "+tn+"){2a}\r\n");
            insprops.append("  else if (propName = '"+e.getName()+"') then "+getTitle(s)+".insertItem(index, propValue as "+tn+"){2a}\r\n");
            reorderprops.append("  else if (propName = '"+e.getName()+"') then "+getTitle(s)+".move(source, destination){2a}\r\n");
          }
          if (!e.typeCode().equals("Resource")) 
            makeprops.append("  else if (propName = '"+e.getName()+"') then result := "+getTitle(s)+".new(){2}\r\n");
          delprops.append("  else if (propName = '"+e.getName()+"') then deletePropertyValue('"+e.getName()+"', "+getTitle(s)+", value) {2}\r\n");
          replprops.append("  else if (propName = '"+e.getName()+"') then replacePropertyValue('"+e.getName()+"', "+getTitle(s)+", existing, new) {2}\r\n");
        }
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
        workingParserX.append("      else if (child.localName = '"+e.getName()+"') then\r\n"+
            "        result."+s+".Add("+parse+"){y.2}\r\n");
        workingComposerX.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n    for i := 0 to elem."+s+".Count - 1 do\r\n"+
            "      "+srlsd+"(xml, '"+e.getName()+"', "+getParam3(tn)+srls.replace("#", "elem."+s+"[i]")+");\r\n");
        if (srlsr.equals("ComposeResource")) 
          workingComposerR.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n    for i := 0 to elem."+s+".Count - 1 do\r\n"+
              "      ComposeInnerResource(this, '%%%%', '"+e.getName()+"', "+srls.replace("#", "elem."+s+"[i]")+", false, i);{x.d3}\r\n");
        else
          workingComposerR.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n    for i := 0 to elem."+s+".Count - 1 do\r\n"+
              "      "+srlsr+"(this, '%%%%', '"+e.getName()+"', "+srls.replace("#", "elem."+s+"[i]")+", false, i);{x.d3}\r\n");
        if (typeIsPrimitive(e.typeCode())) { 
          workingParserJ.append(
              "      if jsn.has('"+e.getName()+"') or jsn.has('_"+e.getName()+"') then\r\n"+
                  "      iteratePrimitiveArray(jsn.vArr['"+e.getName()+"'], jsn.vArr['_"+e.getName()+"'], result."+s+", parse"+parseName(tn)+");\r\n");
        } else { 
          workingParserJ.append("    if jsn.has('"+e.getName()+"') then\r\n"+
              "      iterateArray(jsn.vArr['"+e.getName()+"'], result."+s+", parse"+parseName(tn)+");\r\n");
        }
        workingParserT.append("    for item in obj.complexes('http://hl7.org/fhir/"+e.getPath()+"') do\r\n"+
            "      result."+s+".Add(parse"+parseName(tn)+"(item));\r\n");

        workingComposerJ.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+".Count > 0) then\r\n");
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
        if (!e.isInherited()) {
          defPriv1.append("    F"+getTitle(s)+" : TFhirEnum;\r\n"); 
          empty.append(" and isEmptyProp(F"+getTitle(s)+")");
          defPriv2.append("    Procedure Set"+getTitle(s)+"(value : TFhirEnum);\r\n");
          defPriv2.append("    Function Get"+getTitle(s)+"ST : "+tn+";\r\n");
          defPriv2.append("    Procedure Set"+getTitle(s)+"ST(value : "+tn+");\r\n");
        }
        defPub.append("    {@member "+s+"\r\n");
        defPub.append("      "+makeDocoSafe(e.getDefinition())+"\r\n");
        defPub.append("    }\r\n");
        defPub.append("    property "+s+" : "+tn+" read Get"+getTitle(s)+"ST write Set"+getTitle(s)+"ST;\r\n");
        defPub.append("    property "+s+"Element : TFhirEnum read F"+getTitle(s)+" write Set"+getTitle(s)+";\r\n");
      } else {
        if (!e.isInherited()) {
          defPriv1.append("    F"+getTitle(s)+" : "+tn+";\r\n");
          empty.append(" and isEmptyProp(F"+getTitle(s)+")");
          defPriv2.append("    Procedure Set"+getTitle(s)+"(value : "+tn+");\r\n");
        }
        if (simpleTypes.containsKey(tn)) {
          String sn = simpleTypes.get(tn);
          if (!e.isInherited()) {
            defPriv2.append("    Function Get"+getTitle(s)+"ST : "+sn+";\r\n");
            defPriv2.append("    Procedure Set"+getTitle(s)+"ST(value : "+sn+");\r\n");
          }
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
      jsRegisterSingletonField(jsReg, tj, tn, e);
      if (typeIsSimple(tn) && !tn.equals("TFhirXHtmlNode")) {
        if (!e.isInherited()) {
          if (enumNames.contains(tn)) {         
            impl.append("Procedure "+cn+".Set"+getTitle(s)+"(value : TFhirEnum);\r\nbegin\r\n  F"+getTitle(s)+".free;\r\n  F"+getTitle(s)+" := value;\r\nend;\r\n\r\n");
            impl.append("Function "+cn+".Get"+getTitle(s)+"ST : "+tn+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    result := "+tn+"(0)\r\n  else\r\n    result := "+tn+"(StringArrayIndexOfSensitive(CODES_"+tn+", F"+getTitle(s)+".value));\r\nend;\r\n\r\n");
            impl.append("Procedure "+cn+".Set"+getTitle(s)+"ST(value : "+tn+");\r\nbegin\r\n  if ord(value) = 0 then\r\n    "+getTitle(s)+"Element := nil\r\n  else\r\n    "+getTitle(s)+"Element := TFhirEnum.create(SYSTEMS_"+tn+"[value], CODES_"+tn+"[value]);\r\nend;\r\n\r\n");
            setprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := asEnum(SYSTEMS_"+tn+", CODES_"+tn+", propValue)\r\n");
            replprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := asEnum(SYSTEMS_"+tn+", CODES_"+tn+", new){4}\r\n");          
            delprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := nil\r\n");
          } else {
            impl.append("Procedure "+cn+".Set"+getTitle(s)+"(value : TFhirEnum);\r\nbegin\r\n  F"+getTitle(s)+".free;\r\n  F"+getTitle(s)+" := value;\r\nend;\r\n\r\n");
            impl.append("Procedure "+cn+".Set"+getTitle(s)+"ST(value : "+tn+");\r\nbegin\r\n  if ord(value) = 0 then\r\n    "+getTitle(s)+" := nil\r\n  else\r\n    "+getTitle(s)+" := TFhirEnum.create(SYSTEMS_"+tn+"[value], CODES_"+tn+"[value]);\r\nend;\r\n\r\n");
            setprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+" := asEnum(SYSTEMS_"+tn+", CODES_"+tn+", propValue)\r\n");
            replprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := new as "+tn+"{4}\r\n");          
            delprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := nil\r\n");
          }
          assign.append("  F"+getTitle(s)+" := "+cn+"(oSource).F"+getTitle(s)+".Link;\r\n");
          getkids.append("  if (child_name = '"+e.getName()+"') Then\r\n     list.add(self.link, '"+e.getName()+"', F"+getTitle(s)+".Link);\r\n");
          getprops.append("  oList.add(TFHIRProperty.create(self, '"+e.getName()+"', '"+breakConstant(e.typeCode())+"', false, TFHIREnum, "+propV+".Link));{1}\r\n");
        }
        if (e.getName().endsWith("[x]"))
          throw new Exception("Not done yet");
        if (e.isXmlAttribute())
          workingParserXA.append("    result."+s+"ST := fix me (and compose)! GetAttribute(element, '"+e.getName()+"');\r\n");
        else  
          workingParserX.append("      else if (child.localName = '"+e.getName()+"') then\r\n        result."+s+"Element := "+parse+"{1a}\r\n");
        workingParserJ.append("    if jsn.has('"+e.getName()+"') or jsn.has('_"+e.getName()+"')  then\r\n"+
            "      result."+s+"Element := parseEnum(jsn.path+'/"+e.getName()+"', jsn['"+e.getName()+"'], jsn.vObj['_"+e.getName()+"'], CODES_"+tn+", SYSTEMS_"+tn+");\r\n");
        workingParserT.append("    result."+s+"Element := ParseEnum(obj.complex('http://hl7.org/fhir/"+e.getPath()+"'), CODES_"+tn+", SYSTEMS_"+tn+");\r\n");
        if (!e.isInherited()) {
          destroy.append("  F"+getTitle(s)+".free;\r\n");
        }
        if (enumNames.contains(tn)) {         
          workingComposerX.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n     ComposeEnum(xml, '"+e.getName()+"', elem."+getTitle(s)+"Element, CODES_"+tn+");\r\n");
          workingComposerR.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n     ComposeEnum(this, '%%%%', '"+e.getName()+"', elem."+getTitle(s)+"Element, CODES_"+tn+", SYSTEMS_"+tn+", false, -1);{x.d4}\r\n");
          workingComposerJ.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n     ComposeEnumValue(json, '"+e.getName()+"', elem."+getTitle(s)+"Element, CODES_"+tn+", false);\r\n");
          workingComposerJ.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n     ComposeEnumProps(json, '"+e.getName()+"', elem."+getTitle(s)+"Element, CODES_"+tn+", false);\r\n");
        } else {
          workingComposerX.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n     Compose"+tn+"(xml, '"+e.getName()+"', elem."+getTitle(s)+");{x.1}\r\n");
          workingComposerR.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n     Compose"+tn+"(this, '%%%%', '"+e.getName()+"', elem."+getTitle(s)+", false, -1);{x.1}\r\n");
          workingComposerJ.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n     Compose"+tn+"Value(json, '"+e.getName()+"', elem."+getTitle(s)+", false); {1}\r\n");        
          workingComposerJ.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n     Compose"+tn+"Props(json, '"+e.getName()+"', elem."+getTitle(s)+", false); {y}\r\n");        
        }
      } else {
        if (!e.isInherited()) {
          impl.append("Procedure "+cn+".Set"+getTitle(s)+"(value : "+tn+");\r\nbegin\r\n  F"+getTitle(s)+".free;\r\n  F"+getTitle(s)+" := value;\r\nend;\r\n\r\n");
          if (simpleTypes.containsKey(tn)) {
            String sn = simpleTypes.get(tn);
            if (sn.equals("String")) {
              impl.append("Function "+cn+".Get"+getTitle(s)+"ST : "+sn+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    result := ''\r\n  else\r\n    result := F"+getTitle(s)+".value;\r\nend;\r\n\r\n");
              impl.append("Procedure "+cn+".Set"+getTitle(s)+"ST(value : "+sn+");\r\nbegin\r\n  if value <> '' then\r\n  begin\r\n    if F"+getTitle(s)+" = nil then\r\n      F"+getTitle(s)+" := "+tn+".create;\r\n    F"+getTitle(s)+".value := value\r\n  end\r\n  else if F"+getTitle(s)+" <> nil then\r\n    F"+getTitle(s)+".value := '';\r\nend;\r\n\r\n");
            } else if (sn.equals("Boolean")) {
              impl.append("Function "+cn+".Get"+getTitle(s)+"ST : "+sn+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    result := false\r\n  else\r\n    result := F"+getTitle(s)+".value;\r\nend;\r\n\r\n");
              impl.append("Procedure "+cn+".Set"+getTitle(s)+"ST(value : "+sn+");\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    F"+getTitle(s)+" := "+tn+".create;\r\n  F"+getTitle(s)+".value := value\r\nend;\r\n\r\n");
            } else if (sn.equals("TDateTimeEx")) {
              impl.append("Function "+cn+".Get"+getTitle(s)+"ST : "+sn+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    result := TDateTimeEx.makeNull\r\n  else\r\n    result := F"+getTitle(s)+".value;\r\nend;\r\n\r\n");
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
            getkids.append("  if (child_name = '"+e.getName()+"') or (child_name = '"+e.getName().substring(0, e.getName().length()-3)+"') Then\r\n     list.add(self.link, '"+e.getName()+"', F"+getTitle(s)+".Link);\r\n");
          else
            getkids.append("  if (child_name = '"+e.getName()+"') Then\r\n     list.add(self.link, '"+e.getName()+"', F"+getTitle(s)+".Link);\r\n");
          getprops.append("  oList.add(TFHIRProperty.create(self, '"+e.getName()+"', '"+breakConstant(e.typeCode())+"', false, "+tn+", "+propV+".Link));{2}\r\n");
          if (e.getName().endsWith("[x]")) {
            if (!typeIsPrimitive(e.typeCode()))
              setprops.append("  else if (propName.startsWith('"+e.getName().substring(0, e.getName().length()-3)+"')) then "+propV.substring(1)+" := propValue as "+tn+"{4}\r\n");
            else 
              setprops.append("  else if (propName.startsWith('"+e.getName().substring(0, e.getName().length()-3)+"')) then "+propV.substring(1)+" := propValue as "+tn+"{5}\r\n");
          } else {
            delprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := nil\r\n");
            if (!typeIsPrimitive(e.typeCode()) && !e.typeCode().equals("xhtml")) {
              replprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := new as "+tn+"{4}\r\n");          
              if (simpleTypes.containsKey(tn))
                setprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := propValue as "+tn+"{4a}\r\n");
              else {
                setprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+" := propValue as "+tn+"{4b}\r\n");
                if (typeIsAbstract(e.typeCode())) {
                  makeprops.append("  else if (propName = '"+e.getName()+"') then raise Exception.create('Cannot make property "+propV.substring(1)+"')\r\n");
                } else {
                  makeprops.append("  else if (propName = '"+e.getName()+"') then result := "+tn+".create(){4b}\r\n");
                }
              }
            } else {
              if (!simpleTypes.containsKey(tn) && !tn.equals("TFhirXHtmlNode")) {
                setprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+" := propValue as "+tn+"{5b}\r\n");          
                replprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := new as "+tn+"{5b}\r\n");          
                makeprops.append("  else if (propName = '"+e.getName()+"') then result := "+tn+".create() {5b}\r\n");
              } else if (tn.equals("TFhirCode")) {
                setprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := asCode(propValue)\r\n");
                replprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := asCode(new){5b}\r\n");          
              } else {
                setprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := as"+tn.substring(5)+"(propValue){5a}\r\n");          
                replprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := as"+tn.substring(5)+"(new){5b}\r\n");          
              }
            }
          }
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
              workingParserX.append("      else if (child.localName = '"+e.getName()+"') then\r\n        result."+s+"Element := Parse"+parseName(tn)+"(child, path+'/"+e.getName()+"') {b}\r\n");
              workingComposerX.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n    Compose"+parseName(tn)+"(xml, '"+e.getName()+"', elem."+s+"Element);{x.2b}\r\n");
            } else {
              workingParserX.append("      else if (child.localName = '"+e.getName()+"') then\r\n        result."+s+" := Parse"+parseName(tn)+"(child, path+'/"+e.getName()+"') {b}\r\n");
              workingComposerX.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n    Compose"+parseName(tn)+"(xml, '"+e.getName()+"', "+getParam3(tn)+"elem."+s+");{x.2a}\r\n");
            }
          }
          if (typeIsPrimitive(e.typeCode())) {
            workingParserJ.append("    if jsn.has('"+e.getName()+"') or jsn.has('_"+e.getName()+"') then\r\n        result."+s+"Element := Parse"+parseName(tn)+"(jsn['"+e.getName()+"'], jsn.vObj['_"+e.getName()+"']);{q}\r\n");
            workingParserT.append("    result."+s+"Element := Parse"+parseName(tn)+"(obj.complex('http://hl7.org/fhir/"+e.getPath()+"'));{q1}\r\n");
            workingComposerR.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n    Compose"+parseNameR(tn)+"(this, '%%%%', '"+e.getName()+"', elem."+s+"Element, false, -1);{x.2ea}\r\n");
          } else if (e.typeCode().equals("xhtml")) {
            workingParserJ.append("    if jsn.has('"+e.getName()+"') then\r\n        result."+s+" := Parse"+parseName(tn)+"(jsn.path+'.div', jsn['"+e.getName()+"']);{q2}\r\n");
            workingParserT.append("    result."+s+" := Parse"+parseName(tn)+"(obj.stringLiteral('http://hl7.org/fhir/"+e.getPath()+"'));{q2}\r\n");
            workingComposerR.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n    Compose"+parseNameR(tn)+"(this, '%%%%', '"+e.getName()+"', elem."+s+"Element, false, -1);{x.2eb}\r\n");
          } else {
            workingParserJ.append("    if jsn.has('"+e.getName()+"') then\r\n        result."+s+" := Parse"+parseName(tn)+"(jsn.vObj['"+e.getName()+"']);{q3}\r\n");
            if (tn.equals("TFhirResource")) {
              workingParserT.append("    result."+s+" := Parse"+parseName(tn)+"(obj.predicate('http://hl7.org/fhir/"+e.getPath()+"'));{q3a}\r\n");
              workingComposerR.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n    ComposeInnerResource(this, '%%%%', '"+e.getName()+"', elem."+s+"Element, false, -1);{x.2ec}\r\n");
            } else {
              workingParserT.append("    result."+s+" := Parse"+parseName(tn)+"(obj.complex('http://hl7.org/fhir/"+e.getPath()+"'));{q3b}\r\n");
              workingComposerR.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n    Compose"+parseNameR(tn)+"(this, '%%%%', '"+e.getName()+"', elem."+s+"Element, false, -1);{x.2f}\r\n");
            }
          }
          if (typeIsPrimitive(e.typeCode())) {
            workingComposerJ.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n    Compose"+parseName(tn)+"Value(json, '"+e.getName()+"', elem."+s+"Element, false);\r\n");
            workingComposerJ.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n    Compose"+parseName(tn)+"Props(json, '"+e.getName()+"', elem."+s+"Element, false);\r\n");
          } else
            workingComposerJ.append("  if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) then\r\n    Compose"+parseName(tn)+"(json, '"+e.getName()+"', "+getParam3(tn)+"elem."+s+"); {a}\r\n");
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
            workingParserX.append("      else if (child.localName = '"+pfx+getTitle(ed.getName())+"') then\r\n        result."+s+" := Parse"+getTitle(ed.getName())+"(child, path+'."+pfx+getTitle(ed.getName())+"') {e"+ed.getName()+"}\r\n");
            workingComposerX.append("  else if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhir"+getTitle(ed.getName())+") {8} then\r\n    Compose"+getTitle(ed.getName())+"(xml, '"+pfx+getTitle(ed.getName())+"', TFhir"+getTitle(ed.getName())+"(elem."+s+"))\r\n");
            workingComposerR.append("  else if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhir"+getTitle(ed.getName())+") {8} then\r\n    Compose"+getTitle(ed.getName())+"(this, '%%%%', '"+pfx+getTitle(ed.getName())+"', TFhir"+getTitle(ed.getName())+"(elem."+s+"), false, -1){x.d5}\r\n");
            workingParserJ.append("    if jsn.has('"+pfx+getTitle(ed.getName())+"') {a7} then\r\n        result."+s+" := Parse"+getTitle(ed.getName())+"(jsn.vObj['"+pfx+getTitle(ed.getName())+"']);\r\n");
            workingParserT.append("    if obj.has('"+pfx+getTitle(ed.getName())+"') {a7} then\r\n        result."+s+" := Parse"+getTitle(ed.getName())+"(jsn.vObj['"+pfx+getTitle(ed.getName())+"']);\r\n");
            workingComposerJ.append("  else if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhir"+getTitle(ed.getName())+") then\r\n    Compose"+getTitle(ed.getName())+"(json, '"+pfx+getTitle(ed.getName())+"', TFhir"+getTitle(ed.getName())+"(elem."+s+"))\r\n");
          }
          int t = definitions.getStructures().size();
          i = 0;
          for (ElementDefn ed : definitions.getStructures().values()) {
            workingParserX.append("      else if (child.localName = '"+pfx+getTitle(ed.getName())+"') then\r\n        result."+s+" := Parse"+getTitle(ed.getName())+"(child, path+'/"+pfx+getTitle(ed.getName())+"') {f}\r\n");
            workingComposerX.append("  else if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhir"+getTitle(ed.getName())+") {9} then\r\n    Compose"+getTitle(ed.getName())+"(xml, '"+pfx+getTitle(ed.getName())+"', TFhir"+getTitle(ed.getName())+"(elem."+s+"))"+(i < t-1 ? "" : ";")+"\r\n");
            workingComposerR.append("  else if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhir"+getTitle(ed.getName())+") {9} then\r\n    Compose"+getTitle(ed.getName())+"(this, '%%%%', '"+pfx+getTitle(ed.getName())+"', TFhir"+getTitle(ed.getName())+"(elem."+s+"), true, -1)"+(i < t-1 ? "" : ";")+"{x.d6}\r\n");
            workingParserJ.append("    if jsn.has('"+pfx+getTitle(ed.getName())+"') {a9} then\r\n        result."+s+" := Parse"+getTitle(ed.getName())+"(jsn.vObj['"+pfx+getTitle(ed.getName())+"']);\r\n");
            workingParserT.append("//t.5    if jsn.has('"+pfx+getTitle(ed.getName())+"') {a9} then\r\n        result."+s+" := Parse"+getTitle(ed.getName())+"(jsn.vObj['"+pfx+getTitle(ed.getName())+"']);\r\n");
            workingComposerJ.append("  else if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhir"+getTitle(ed.getName())+") then\r\n    Compose"+getTitle(ed.getName())+"(json, '"+pfx+getTitle(ed.getName())+"', TFhir"+getTitle(ed.getName())+"(elem."+s+"))"+(i < t-1 ? "" : ";")+"\r\n");
            i++;
          }
        }
      }
    }
  }

  private void jsRegisterSingletonPrimitiveField(StringBuilder jsReg, String tj, ElementDefn e, String specType) {
    // handle split between primitive value and id/extension
    // 1st: primitive value
    String en = getElementName(e.getName());

    if (specType.equals("")) {
      String rawTn = getRawTnForType(e.typeCode());
      jsReg.append("  js.registerElement(def, '"+tj+"', '"+e.getName()+specType+"', '"+e.typeCode()+"', js.getFHIR"+rawTn+"Prop, js.setFHIR"+rawTn+"Prop);\r\n");
    } else {
      String rawTn = getRawTnForType(Utilities.uncapitalize(specType));
      jsReg.append("  js.registerElement(def, '"+tj+"', '"+e.getName().replace("[x]", "")+specType+"', '"+Utilities.uncapitalize(specType)+"', js.getFHIR"+rawTn+"Prop, js.setFHIR"+rawTn+"Prop);\r\n");
    }        
  }

  private String getRawTnForType(String type) {
    if (Utilities.existsInList(type, "date", "dateTime", "instant"))
      return "DateTime";
    if (Utilities.existsInList(type, "decimal"))
      return "Decimal";
    if (Utilities.existsInList(type, "integer", "positiveInt", "nonNegativeInt"))
      return "Integer";
    if (Utilities.existsInList(type, "boolean"))
      return "Boolean";
    if (Utilities.existsInList(type, "base64Binary"))
      return "Binary";
    return "String";
  }

  private void jsRegisterSingletonField(StringBuilder jsReg, String tj, String tn, ElementDefn e) {
    if (e.getName().contains("[x]")) {
      for (TypeRef t : e.getTypes()) {
        if (isPrimitive(t.getName())) {
          jsRegisterSingletonPrimitiveField(jsReg, tj, e, Utilities.capitalize(t.getName()));
        } else { 
          String enf = e.getName().replace("[x]", "");
          String en = getElementName(e.getName().replace("[x]", ""));
  
          jsReg.append("  js.registerElement(def, '"+tj+"', '"+enf+t.getName()+"', '"+t.getName()+"', js.getFHIRObjectProp, js.setFHIRObjectProp);\r\n");
        }
      }
    } else {
      if (isPrimitive(e.typeCode())) {
        jsRegisterSingletonPrimitiveField(jsReg, tj, e, "");
      } else { 
        String en = getElementName(e.getName());
        if (tn.contains("{"))
          tn = tn.substring(0,  tn.indexOf("{"));

        if (Utilities.noString(e.typeCode())) {

          String bt = typeNames.get(e).substring(5); 
          jsReg.append("  js.registerElement(def, '"+tj+"', '"+e.getName()+"', '"+bt+"', js.getFHIRObjectProp, js.setFHIRObjectProp);\r\n");
        } else
          jsReg.append("  js.registerElement(def, '"+tj+"', '"+e.getName()+"', '"+e.typeCode()+"', js.getFHIRObjectProp, js.setFHIRObjectProp);\r\n");
      }
    }
  }

  private void jsRegisterUnboundedField(StringBuilder jsReg, String tj, String tn, ElementDefn e) {
    if (isPrimitive(e.typeCode())) {
//      jsRegisterSingletonSimpleField(jsClass, jsReg, jsLoad, tj, e, null);
    } else { 
      String en = getElementName(e.getName());
      if (tn.contains("{"))
        tn = tn.substring(0,  tn.indexOf("{"));

      if (Utilities.noString(e.typeCode())) {

        String bt = typeNames.get(e).substring(5); 
        jsReg.append("  js.registerElement(def, '"+tj+"', '"+e.getName()+"', '"+bt+"', js.getFHIRArrayProp, js.setFHIRArrayProp);\r\n");
      } else
        jsReg.append("  js.registerElement(def, '"+tj+"', '"+e.getName()+"', '"+e.typeCode()+"', js.getFHIRArrayProp, js.setFHIRArrayProp);\r\n");
    }
  }

  private boolean typeIsAbstract(String type) {
    return Utilities.existsInList(type, "Element", "BackboneElement", "Resource", "DomainResource");
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
    String dc = ""; // path.contains(".") ? "" : "and doCompose(elem) ";
    String defaultValueTest = "";
    workingParserX.append("      else if (child.localName = '"+pfx+getTitle(cd.getCode())+"') then\r\n        result."+s+" := Parse"+getTitle(cd.getCode())+"(child, path+'."+pfx+getTitle(cd.getCode())+"') {c}\r\n");
    String ptn = "TFhir"+getTitle(cd.getCode());
    workingComposerX.append("  "+(i > 0 ? "else " : "")+"if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+" is "+ptn+") {1} then\r\n    Compose"+ptn.substring(5)+"(xml, '"+pfx+getTitle(cd.getCode())+"', "+ptn+"(elem."+s+"))\r\n");
    workingComposerR.append("  "+(i > 0 ? "else " : "")+"if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+" is "+ptn+") {1} then\r\n    Compose"+ptn.substring(5)+"(this, '%%%%', '"+pfx+getTitle(cd.getCode())+"', "+ptn+"(elem."+s+"), false, -1){x.d7}\r\n");
    workingParserJ.append("    if jsn.has('"+pfx+getTitle(cd.getCode())+"') or jsn.has('_"+pfx+getTitle(cd.getCode())+"') then\r\n        result."+s+" := Parse"+getTitle(cd.getCode())+"(jsn['"+pfx+getTitle(cd.getCode())+"'], jsn.vObj['_"+pfx+getTitle(cd.getCode())+"']);\r\n");
    workingParserT.append("//t.6    if jsn.has('"+pfx+getTitle(cd.getCode())+"') or jsn.has('_"+pfx+getTitle(cd.getCode())+"') then\r\n        result."+s+" := Parse"+getTitle(cd.getCode())+"(jsn['"+pfx+getTitle(cd.getCode())+"'], jsn.vObj['_"+pfx+getTitle(cd.getCode())+"']);\r\n");
    workingComposerJ.append("  "+(i > 0 ? "else " : "")+"if not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+" is "+ptn+") then\r\n"+
        "  begin\r\n"+
        "    Compose"+ptn.substring(5)+"Value(json, '"+pfx+getTitle(cd.getCode())+"', "+ptn+"(elem."+s+"), false);\r\n"+
        "    Compose"+ptn.substring(5)+"Props(json, '"+pfx+getTitle(cd.getCode())+"', "+ptn+"(elem."+s+"), false)\r\n"+
        "  end\r\n");
  }

  private boolean isDerivedType(TypeRef td) {
    return !(definitions.getPrimitives().get(td.getName()) instanceof PrimitiveType); 
  }

  private void genTypeSerialisers(ElementDefn e, String cn, String s, String sumSet, String pfx, boolean first, boolean last, TypeRef td) throws Exception {
    String dc = ""; // path.contains(".") ? "" : "and doCompose(elem) ";
    String defaultValueTest = "";
    if (td.isResourceReference()) {
      workingParserX.append("      else if (child.localName = '"+pfx+"Reference') then\r\n        result."+s+" := ParseReference(child, path+'/"+pfx+"Reference') {a}\r\n");
      workingComposerX.append("  "+(first ? "if" : "else if")+" not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhirReference) {2} then\r\n    ComposeReference(xml, '"+pfx+"Reference', TFhirReference(elem."+s+"))"+(last?";" : "")+"\r\n");
      workingParserJ.append("    if jsn.has('"+pfx+"Reference') {a3} then\r\n      result."+s+" := ParseReference(jsn.vObj['"+pfx+"Reference']);\r\n");
      workingParserT.append("    if obj.has('http://hl7.org/fhir/"+e.getPathHead()+"."+pfx+"Reference', item) {a3} then\r\n      result."+s+" := ParseReference(item);\r\n");
      workingComposerJ.append("  "+(first ? "if" : "else if")+" not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhirReference) then\r\n    ComposeReference(json, '"+pfx+"Reference', TFhirReference(elem."+s+"))"+(last?";" : "")+"\r\n");
      workingComposerR.append("  "+(first ? "if" : "else if")+" not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+" is TFhirReference) {2} then\r\n    ComposeReference(this, '%%%%', '"+pfx+"Reference', TFhirReference(elem."+s+"), false,-1)"+(last?";" : "")+"{x.d8}\r\n");
    }
    else {
      if (td.hasParams())
        throw new Exception("Type "+td.summary()+" has parameters");                
      String tname = td.getName();
      String tpname = tname; 
      workingParserX.append("      else if (child.localName = '"+pfx+getTitle(tpname)+"') then\r\n        result."+s+" := Parse"+getTitle(tname)+"(child, path+'/"+pfx+getTitle(tname)+"'){x.3}\r\n");
      workingComposerX.append("  "+(first ? "if" : "else if")+" not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+" is "+getTypeName(tname)+") {6} then\r\n    Compose"+getTitle(tname)+"(xml, '"+pfx+getTitle(tpname)+"', "+getTypeName(tname)+"(elem."+s+"))"+(last?";" : "")+"\r\n");
      if (typeIsPrimitive(tname)) {
        workingComposerJ.append("  "+(first ? "if" : "else if")+" not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+" is "+getTypeName(tname)+") then \r\n"+
            "  begin\r\n"+
            "    Compose"+getTitle(tname)+"Value(json, '"+pfx+getTitle(tpname)+"', "+getTypeName(tname)+"(elem."+s+"), false);\r\n"+
            "    Compose"+getTitle(tname)+"Props(json, '"+pfx+getTitle(tpname)+"', "+getTypeName(tname)+"(elem."+s+"), false);\r\n  end"+(last?";" : "")+"\r\n");
        workingParserJ.append("    if jsn.has('"+pfx+getTitle(tpname)+"') or jsn.has('_"+pfx+getTitle(tname)+"') then\r\n      result."+s+" := parse"+Utilities.capitalize(tname)+"(jsn['"+pfx+getTitle(tpname)+"'], jsn.vObj['_"+pfx+getTitle(tpname)+"']);\r\n");
      } else {
        workingComposerJ.append("  "+(first ? "if" : "else if")+" not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+" is "+getTypeName(tname)+") then \r\n"+
            "    Compose"+getTitle(tname)+"(json, '"+pfx+getTitle(tpname)+"', "+getTypeName(tname)+"(elem."+s+")) "+(last?";" : "")+"\r\n");
        workingParserJ.append("    if jsn.has('"+pfx+getTitle(tpname)+"') {a4} then\r\n      result."+s+" := Parse"+getTitle(tname)+"(jsn.vObj['"+pfx+getTitle(tpname)+"']);\r\n");
      }
      workingParserT.append("    if obj.has('http://hl7.org/fhir/"+e.getPathHead()+"."+pfx+getTitle(tpname)+"', item) then\r\n      result."+s+" := parse"+Utilities.capitalize(tname)+"(item);\r\n");
      workingComposerR.append("  "+(first ? "if" : "else if")+" not elem.noCompose "+defaultValueTest+dc+"and (SummaryOption in ["+sumSet+"]) and (elem."+s+" is "+getTypeName(tname)+") {6} then\r\n    Compose"+getTitle(tname)+"(this, '%%%%', '"+pfx+getTitle(tpname)+"', "+getTypeName(tname)+"(elem."+s+"), false, -1)"+(last?";" : "")+"{x.d9}\r\n");
    }
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
              "  protected\r\n"+
              "    function ItemClass : TAdvObjectClass; override;\r\n"+
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
              "function "+tn+"List.ItemClass: TAdvObjectClass;\r\n"+
              "begin\r\n"+
              "  result := "+tn+";\r\n"+
              "end;\r\n"+
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
    return tn.equals("String") || tn.equals("Integer")  || tn.equals("unsignedInt")  || tn.equals("positiveInt") || tn.equals("Boolean") || tn.equals("TDateTimeEx") || tn.equals("TFhirXHtmlNode")  || enumNames.contains(tn);
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
      pn = "TDateTimeEx";
    if (tn.equals("Boolean"))
      pn = "Boolean";
    if (tn.equals("Base64Binary"))
      pn = "TBytes";

    factoryIntf.append("    {@member new"+tn+"\r\n      create a new "+t.getCode()+"\r\n    }\r\n    {!script nolink}\r\n    function new"+tn+" : TFhir"+tn+";\r\n");
    factoryImpl.append("function TFhirResourceFactory.new"+tn+" : TFhir"+tn+";\r\nbegin\r\n  result := TFhir"+tn+".create;\r\nend;\r\n\r\n");
    factoryByName.append("  else if name = '"+t.getCode()+"' then\r\n    result := new"+tn+"()\r\n");

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
      def.append("    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;\r\n");
      def.append("    Procedure ListProperties(oList : "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues : Boolean); Override;\r\n");
      def.append("    function AsStringValue : String; Override;\r\n");
      def.append("    procedure SetStringValue(value : String); Override;\r\n");
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
      def.append("    function equalsDeep(other : TFHIRObject) : boolean; override;\r\n");
      def.append("    function equalsShallow(other : TFHIRObject) : boolean; override;\r\n");
      def.append("    function isEmpty : boolean; override;\r\n");
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
      if (tn.equals("Date"))
        impl2.append("  FValue := value.fixPrecision(dtpDay);\r\n");
      else
        impl2.append("  FValue := value;\r\n");
      impl2.append("end;\r\n\r\n");
    }

    impl2.append("Destructor TFhir"+tn+".Destroy;\r\n");
    impl2.append("begin\r\n");
    if (!derived) {
      if (!pn.equals("String") && !pn.equals("Boolean") && !pn.equals("TBytes") && !pn.equals("TDateTimeEx"))
        impl2.append("  FValue.free;\r\n");
    }
    impl2.append("  inherited;\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function TFhir"+tn+".fhirType : string;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := '"+(t.getCode().equals("enum") ? "code" : t.getCode())+"';\r\n");
    impl2.append("end;\r\n\r\n");

    if (!derived) {

      impl2.append("procedure TFhir"+tn+".GetChildrenByName(child_name : string; list : TFHIRSelectionList);\r\n");
      impl2.append("begin\r\n");
      impl2.append("  inherited;\r\n");
      impl2.append("  if child_name = 'value' then\r\n    list.add(self.link, 'value', TFHIRObjectText.create(value));\r\n");
      impl2.append("end;\r\n\r\n");
      impl2.append("procedure TFhir"+tn+".ListProperties(oList: "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues: Boolean);\r\n");
      impl2.append("begin\r\n");
      impl2.append("  inherited;\r\n");
      impl2.append("  if (bPrimitiveValues) then\r\n");
      if (pn.equals("Boolean"))
        impl2.append("    oList.add(TFHIRProperty.create(self, 'value', '"+breakConstant(t.getCode())+"', false, nil, LCBooleanToString(FValue)));\r\n");
      else if (pn.equals("TDateTimeEx"))
        impl2.append("    if (FValue.notNull) then\r\n      oList.add(TFHIRProperty.create(self, 'value', '"+breakConstant(t.getCode())+"', false, nil, FValue.toString));\r\n");
      else if (!pn.equals("String") && !pn.equals("TBytes"))
        impl2.append("    if (FValue <> nil) then\r\n      oList.add(TFHIRProperty.create(self, 'value', '"+breakConstant(t.getCode())+"', false, nil, FValue.toString));\r\n");
      else 
        impl2.append("    oList.add(TFHIRProperty.create(self, 'value', '"+breakConstant(t.getCode())+"', false, nil, FValue));\r\n");
      impl2.append("end;\r\n\r\n");


      impl2.append("procedure TFhir"+tn+".Assign(oSource : TAdvObject);\r\n");
      impl2.append("begin\r\n");
      impl2.append("  inherited;\r\n");
      if (!Utilities.existsInList(pn, "String", "Boolean", "TBytes", "TDateTimeEx")) 
        impl2.append("  FValue := TFhir"+tn+"(oSource).Value.Link;\r\n");
      else 
        impl2.append("  FValue := TFhir"+tn+"(oSource).Value;\r\n");
      impl2.append("end;\r\n\r\n");

      impl2.append("function TFhir"+tn+".AsStringValue : string;\r\n");
      impl2.append("begin\r\n");
      if (pn.equals("Boolean"))
        impl2.append("  result := LCBooleanToString(FValue);\r\n");
      else if (pn.equals("TBytes"))
        impl2.append("  if (length(FValue) = 0) then result := '' else result := string(EncodeBase64(@FValue[0], length(FValue)));\r\n");
      else if (tn.equals("DateTime") || tn.equals("Date") || tn.equals("Instant") ) {
        impl2.append("  if (FValue.null) then\r\n");
        impl2.append("    result := ''\r\n");
        impl2.append("  else\r\n");
        impl2.append("    result := FValue.toXml;\r\n");
      } else if (!pn.equals("String")) {
        impl2.append("  if (FValue = nil) then\r\n");
        impl2.append("    result := ''\r\n");
        impl2.append("  else\r\n");
        impl2.append("    result := FValue.toString;\r\n");
      } else 
        impl2.append("  result := FValue;\r\n");
      impl2.append("end;\r\n\r\n");
      impl2.append("procedure TFhir"+tn+".SetStringValue(value : string);\r\n");
      impl2.append("begin\r\n");
      if (pn.equals("Boolean"))
        impl2.append("  FValue := StringToBoolean(value);\r\n");
      else if (pn.equals("TBytes"))
        impl2.append("  if (length(value) = 0) then SetLength(FValue, 0) else FValue := DecodeBase64(value);\r\n");
      else if (tn.equals("DateTime") || tn.equals("Date") || tn.equals("Instant") ) {
        impl2.append("  if (value = '') then\r\n");
        impl2.append("    FValue := TDateTimeEx.makeNull\r\n");
        impl2.append("  else\r\n");
        impl2.append("    FValue := TDateTimeEx.fromXml(value);\r\n");
      } else if (!pn.equals("String")) {
        impl2.append("  !if (FValue = nil) then\r\n");
        impl2.append("    !result := ''\r\n");
        impl2.append("  !else\r\n");
        impl2.append("    !result := FValue.toString;\r\n");
      } else 
        impl2.append("  FValue := value;\r\n");
      impl2.append("end;\r\n\r\n");
      generatePrimitiveEquals(t, "TFhir"+tn, impl2);
      impl2.append("function TFhir"+tn+".isEmpty : boolean;\r\n");
      impl2.append("begin\r\n");
      if (pn.equals("Boolean"))
        impl2.append("  result := false;\r\n");
      else if (pn.equals("TBytes"))
        impl2.append("  result := inherited isEmpty and (length(FValue) = 0);\r\n");
      else if (tn.equals("DateTime") || tn.equals("Date") || tn.equals("Instant") ) 
        impl2.append("  result := inherited isEmpty and (FValue.null);\r\n");
      else
        impl2.append("  result := inherited isEmpty and (FValue = '');\r\n");
      impl2.append("end;\r\n\r\n");
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
      if (!pn.equals("String") && !pn.equals("Boolean") && !pn.equals("TBytes") && !pn.equals("TDateTimeEx")) 
        impl2.append("  FValue.free;\r\n");
      impl2.append("  FValue := value;\r\n");
      impl2.append("end;\r\n\r\n");
    }    

    if (isEnum) {
      prsrdefX.append("    function Parse"+tn+"(Const aNames, aSystems : Array Of String; path : String; element : TMXmlElement) : TFhir"+tn+";\r\n");
      prsrImplX.append("function TFHIRXmlParser.Parse"+tn+"(Const aNames, aSystems : Array Of String; path : String; element : TMXmlElement) : TFhir"+tn+";\r\n");
      prsrImplX.append("var\r\n");
      prsrImplX.append("  child : TMXmlElement;\r\n");
      prsrImplX.append("  i : integer;\r\n");
      prsrImplX.append("begin\r\n");
      prsrImplX.append("  result := TFhir"+tn+".create;\r\n");
      prsrImplX.append("  try\r\n");
      prsrImplX.append("    ParseElementAttributes(result, path, element);\r\n");
      prsrImplX.append("    result.value := GetAttribute(element, 'value');\r\n");
      prsrImplX.append("    i := StringArrayIndexOfSensitive(aNames, result.value);\r\n");
      prsrImplX.append("    if i < 0 then\r\n");
      prsrImplX.append("      raise Exception.create('unknown code: '+result.value+' from a set of choices of '+StringArrayToCommaString(aNames)+' for \"'+path+'\"');\r\n");
      prsrImplX.append("    result.system := aSystems[i];\r\n");
      prsrImplX.append("    child := FirstChild(element);\r\n");
      prsrImplX.append("    while (child <> nil) do\r\n");
      prsrImplX.append("    begin\r\n");
      prsrImplX.append("      if Not ParseElementChild(result, path, child) then\r\n");
      prsrImplX.append("         UnknownContent(child, path);\r\n");
      prsrImplX.append("      child := NextSibling(child);\r\n");
      prsrImplX.append("    end;\r\n");
      prsrImplX.append("    closeOutElement(result, element);\r\n\r\n");
      prsrImplX.append("    result.link;\r\n");
      prsrImplX.append("  finally\r\n");
      prsrImplX.append("    result.free;\r\n");
      prsrImplX.append("  end;\r\n");
      prsrImplX.append("end;\r\n\r\n");

      prsrdefJ.append("    procedure ParseEnum(path, value : string; jsn : TJsonObject; ctxt : "+listForm("TFHIRObject")+"; Const aNames, aSystems : Array Of String); overload;\r\n");
      prsrImplJ.append("procedure TFHIRJsonParser.ParseEnum(path, value : string; jsn : TJsonObject; ctxt : "+listForm("TFHIRObject")+"; Const aNames, aSystems : Array Of String);\r\n");
      prsrImplJ.append("begin\r\n");
      prsrImplJ.append("  ctxt.add(ParseEnum(path, value, jsn, aNames, aSystems));\r\n");
      prsrImplJ.append("end;\r\n\r\n");
      prsrdefJ.append("    function ParseEnum(path, value : string; jsn : TJsonObject; Const aNames, aSystems : Array Of String) : TFHIREnum; overload;\r\n");
      prsrImplJ.append("function TFHIRJsonParser.ParseEnum(path, value : string; jsn : TJsonObject; Const aNames, aSystems : Array Of String) : TFHIREnum;\r\n");
      prsrImplJ.append("var\r\n");
      prsrImplJ.append("  i : integer;\r\n");
      prsrImplJ.append("begin\r\n");
      prsrImplJ.append("  i := StringArrayIndexOfSensitive(aNames, value);\r\n");
      prsrImplJ.append("  if (value <> '') and (i < 0) then\r\n");
      prsrImplJ.append("    raise Exception.create('unknown code: '+value+' from a set of choices of '+StringArrayToCommaString(aNames)+' for \"'+path+'\"');\r\n");
      prsrImplJ.append("  result := TFHIREnum.create;\r\n");
      prsrImplJ.append("  try\r\n");
      prsrImplJ.append("    result.value := value;\r\n");
      prsrImplJ.append("    result.system := aSystems[i];\r\n");
      prsrImplJ.append("    if (jsn <> nil) then\r\n");
      prsrImplJ.append("      parseElementProperties(jsn, result);\r\n");
      prsrImplJ.append("    result.link;\r\n");
      prsrImplJ.append("  finally\r\n");
      prsrImplJ.append("    result.free;\r\n");
      prsrImplJ.append("  end;\r\n");
      prsrImplJ.append("end;\r\n\r\n");

      prsrdefT.append("    function ParseEnum(obj : TTurtleComplex; Const aNames, aSystems : Array Of String) : TFHIREnum; overload;\r\n");
      prsrImplT.append("function TFHIRTurtleParser.ParseEnum(obj : TTurtleComplex; Const aNames, aSystems : Array Of String) : TFHIREnum;\r\n");
      prsrImplT.append("var\r\n");
      prsrImplT.append("  i : integer;\r\n");
      prsrImplT.append("  value : String;\r\n");
      prsrImplT.append("begin\r\n");
      prsrImplT.append("  if obj = nil then\r\n");
      prsrImplT.append("    exit(nil);\r\n");
      prsrImplT.append("\r\n");
      prsrImplT.append("  if (obj.has('http://hl7.org/fhir/value')) then\r\n");
      prsrImplT.append("    value := obj.stringLiteral('http://hl7.org/fhir/value');\r\n");
      prsrImplT.append("  i := StringArrayIndexOfSensitive(aNames, value);\r\n");
      prsrImplT.append("  if (value <> '') and (i < 0) then\r\n");
      prsrImplT.append("    raise Exception.create('unknown code: '+value+' from a set of choices of '+StringArrayToCommaString(aNames));\r\n");
      prsrImplT.append("  result := TFHIREnum.create;\r\n");
      prsrImplT.append("  try\r\n");
      prsrImplT.append("    result.value := value;\r\n");
      prsrImplT.append("    result.system := aSystems[i];\r\n");
      prsrImplT.append("    parseElementProperties(obj, result);\r\n");
      prsrImplT.append("    result.link;\r\n");
      prsrImplT.append("  finally\r\n");
      prsrImplT.append("    result.free;\r\n");
      prsrImplT.append("  end;\r\n");
      prsrImplT.append("end;\r\n\r\n");


      srlsdefX.append("    Procedure Compose"+tn+"(xml : TXmlBuilder; name : String; value : TFhir"+tn+"; Const aNames : Array Of String);\r\n");
      prsrImplX.append("Procedure TFHIRXmlComposer.Compose"+tn+"(xml : TXmlBuilder; name : String; value : TFhir"+tn+"; Const aNames : Array Of String);\r\n");
      prsrImplX.append("begin\r\n");
      prsrImplX.append("  if (value = nil) then\r\n");
      prsrImplX.append("    exit;\r\n");
      prsrImplX.append("  composeElementAttributes(xml, value);\r\n");
      prsrImplX.append("  attribute(xml, 'value', value.value);\r\n");
      prsrImplX.append("  xml.open(name);\r\n");
      prsrImplX.append("  composeElementChildren(xml, value);\r\n");
      prsrImplX.append("  closeOutElement(xml, value);\r\n");
      prsrImplX.append("  xml.close(name);\r\n");
      prsrImplX.append("end;\r\n\r\n");
      srlsdefJ.append("    Procedure Compose"+tn+"Value(json : TJSONWriter; name : String; value : TFhir"+tn+"; Const aNames : Array Of String; inArray : boolean);\r\n");
      srlsdefJ.append("    Procedure Compose"+tn+"Props(json : TJSONWriter; name : String; value : TFhir"+tn+"; Const aNames : Array Of String; inArray : boolean);\r\n");
      prsrImplJ.append("Procedure TFHIRJsonComposer.Compose"+tn+"Value(json : TJSONWriter; name : String; value : TFhir"+tn+"; Const aNames : Array Of String; inArray : boolean);\r\n");
      prsrImplJ.append("begin\r\n");
      prsrImplJ.append("  if (value = nil) or (value.Value = '') then\r\n");
      prsrImplJ.append("  begin\r\n");
      prsrImplJ.append("    if inArray then\r\n");
      prsrImplJ.append("      propNull(json, name);\r\n");
      prsrImplJ.append("    exit;\r\n");
      prsrImplJ.append("  end\r\n");
      prsrImplJ.append("  else\r\n");
      prsrImplJ.append("    prop(json, name, value.value);\r\n");
      prsrImplJ.append("end;\r\n\r\n");
      prsrImplJ.append("Procedure TFHIRJsonComposer.Compose"+tn+"Props(json : TJSONWriter; name : String; value : TFhir"+tn+"; Const aNames : Array Of String; inArray : boolean);\r\n");
      prsrImplJ.append("begin\r\n");
      prsrImplJ.append("  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments) }) then\r\n");
      prsrImplJ.append("  begin\r\n");
      prsrImplJ.append("    if inArray then\r\n");
      prsrImplJ.append("      propNull(json, name);\r\n");
      prsrImplJ.append("    exit;\r\n");
      prsrImplJ.append("  end\r\n");
      prsrImplJ.append("  else\r\n");
      prsrImplJ.append("  begin\r\n");
      prsrImplJ.append("    if (inArray) then\r\n");
      prsrImplJ.append("      json.valueObject('')\r\n");
      prsrImplJ.append("    else\r\n");
      prsrImplJ.append("      json.valueObject('_'+name);\r\n");
      prsrImplJ.append("    ComposeElementProperties(json, value);\r\n");
      prsrImplJ.append("    json.finishObject;\r\n");
      prsrImplJ.append("  end;\r\n");
      prsrImplJ.append("end;\r\n\r\n");

      srlsdefR.append("    Procedure Compose"+tn+"(parent :  TTurtleComplex; parentType, name : String; value : TFhir"+tn+"; Const aNames, aSystems : Array Of String; useType : boolean; index : integer);\r\n");
      prsrImplT.append("Procedure TFHIRTurtleComposer.Compose"+tn+"(parent :  TTurtleComplex; parentType, name : String; value : TFhir"+tn+"; Const aNames, aSystems : Array Of String; useType : boolean; index : integer);\r\n");
      prsrImplT.append("var\r\n");
      prsrImplT.append("  this : TTurtleComplex;\r\n");
      prsrImplT.append("begin\r\n");
      prsrImplT.append("  if (value = nil) then\r\n");
      prsrImplT.append("    exit;\r\n");
      prsrImplT.append("  this := parent.addPredicate('fhir:'+parentType+'.'+name);\r\n");
      prsrImplT.append("  if (useType) then\r\n");
      prsrImplT.append("    this.addPredicate('a', 'fhir:code');\r\n");
      prsrImplT.append("  this.addPredicate('fhir:value', ttlLiteral(value.value));\r\n");
      prsrImplT.append("  composeElement(this, parentType, name, value, false, index);\r\n");
      prsrImplT.append("end;\r\n\r\n");
    } else {
      prsrdefX.append("    function Parse"+tn+"(element : TMXmlElement; path : string) : TFhir"+tn+";\r\n");
      prsrImplX.append("function TFHIRXmlParser.Parse"+tn+"(element : TMXmlElement; path : string) : TFhir"+tn+";\r\n");
      prsrImplX.append("var\r\n");
      prsrImplX.append("  child : TMXmlElement;\r\n");
      prsrImplX.append("begin\r\n");
      prsrImplX.append("  result := TFhir"+tn+".create;\r\n");
      prsrImplX.append("  try\r\n");
      prsrImplX.append("    ParseElementAttributes(result, path, element);\r\n");
      if (pn.equals("Boolean"))
        prsrImplX.append("    result.value := StringToBoolean(GetAttribute(element, 'value'));\r\n");
      else  if (!pn.equals("String"))
        prsrImplX.append("    result.value := to"+pn+"(GetAttribute(element, 'value'));\r\n");
      else
        prsrImplX.append("    result.value := GetAttribute(element, 'value');\r\n");
      prsrImplX.append("    child := FirstChild(element);\r\n");
      prsrImplX.append("    while (child <> nil) do\r\n");
      prsrImplX.append("    begin\r\n");
      prsrImplX.append("      if Not ParseElementChild(result, path, child) then\r\n");
      prsrImplX.append("         UnknownContent(child, path);\r\n");
      prsrImplX.append("      child := NextSibling(child);\r\n");
      prsrImplX.append("    end;\r\n");
      prsrImplX.append("    closeOutElement(result, element);\r\n\r\n");
      prsrImplX.append("    result.link;\r\n");
      prsrImplX.append("  finally\r\n");
      prsrImplX.append("    result.free;\r\n");
      prsrImplX.append("  end;\r\n");
      prsrImplX.append("end;\r\n\r\n");


      prsrdefJ.append("    procedure Parse"+tn+"(value : string; jsn : TJsonObject; ctxt : "+listForm("TFHIRObject")+"); overload;\r\n");
      prsrImplJ.append("procedure TFHIRJsonParser.Parse"+tn+"(value : string; jsn : TJsonObject; ctxt : "+listForm("TFHIRObject")+");\r\n");
      prsrImplJ.append("begin\r\n");
      prsrImplJ.append("  ctxt.add(Parse"+tn+"(value, jsn)) {1};\r\n");
      prsrImplJ.append("end;\r\n\r\n");
      prsrdefT.append("    function Parse"+tn+"(obj : TTurtleComplex) : TFHIR"+tn+"; overload;\r\n");
      prsrdefJ.append("    function Parse"+tn+"(value : string; jsn : TJsonObject) : TFHIR"+tn+"; overload;\r\n");
      prsrImplJ.append("function TFHIRJsonParser.Parse"+tn+"(value : string; jsn : TJsonObject) : TFHIR"+tn+";\r\n");
      prsrImplJ.append("begin\r\n");
      prsrImplJ.append("  result := TFhir"+tn+".Create;\r\n");
      prsrImplJ.append("  try\r\n");
      if (pn.equals("Boolean"))
        prsrImplJ.append("    result.value := StringToBoolean(value);\r\n");
      else if (!pn.equals("String"))
        prsrImplJ.append("     result.value := to"+pn+"(value);\r\n");
      else
        prsrImplJ.append("    result.value := value;\r\n");
      prsrImplJ.append("    if (jsn <> nil) then\r\n");
      prsrImplJ.append("      parseElementProperties(jsn, result);\r\n");
      prsrImplJ.append("    result.Link;\r\n");
      prsrImplJ.append("  finally\r\n");
      prsrImplJ.append("    result.Free;\r\n");
      prsrImplJ.append("  end;\r\n");
      prsrImplJ.append("end;\r\n\r\n");
      prsrImplT.append("function TFHIRTurtleParser.Parse"+tn+"(obj : TTurtleComplex) : TFHIR"+tn+";\r\n");
      prsrImplT.append("begin\r\n");
      prsrImplT.append("  if (obj = nil) then\r\n");
      prsrImplT.append("    exit(nil);\r\n");      
      prsrImplT.append("  result := TFhir"+tn+".Create;\r\n");
      prsrImplT.append("  try\r\n");
      prsrImplT.append("    if (obj.has('http://hl7.org/fhir/value')) then\r\n");
      if (pn.equals("Boolean"))
        prsrImplT.append("     result.value := StringToBoolean(obj.stringLiteral('http://hl7.org/fhir/value'));\r\n");
      else if (!pn.equals("String"))
        prsrImplT.append("      result.value := to"+pn+"(obj.stringLiteral('http://hl7.org/fhir/value'));\r\n");
      else
        prsrImplT.append("      result.value := obj.stringLiteral('http://hl7.org/fhir/value');\r\n");
      prsrImplT.append("    parseElementProperties(obj, result);\r\n");
      prsrImplT.append("    result.Link;\r\n");
      prsrImplT.append("  finally\r\n");
      prsrImplT.append("    result.Free;\r\n");
      prsrImplT.append("  end;\r\n");
      prsrImplT.append("end;\r\n\r\n");

      srlsdefX.append("    Procedure Compose"+tn+"(xml : TXmlBuilder; name : String; value : TFhir"+tn+");\r\n");
      prsrImplX.append("Procedure TFHIRXmlComposer.Compose"+tn+"(xml : TXmlBuilder; name : String; value : TFhir"+tn+");\r\n");
      prsrImplX.append("begin\r\n");
      if (pn.equals("Boolean"))
        prsrImplX.append("  if (value = nil) then\r\n");
      else if (pn.equals("TDateTimeEx"))
        prsrImplX.append("  if (value = nil) or ((value.value.null) and (value.extensionList.count = 0)) then\r\n");
      else if (!pn.equals("String"))
        prsrImplX.append("  if (value = nil) or ((value.value = nil) and (value.extensionList.count = 0)) then\r\n");
      else 
        prsrImplX.append("  if (value = nil) or ((value.value = '') and (value.extensionList.count = 0)) then\r\n");
      prsrImplX.append("    exit;\r\n");
      prsrImplX.append("  composeElementAttributes(xml, value);\r\n");
      if (pn.equals("Boolean")) {
        prsrImplX.append("  attribute(xml, 'value', LCBooleanToString(value.value));\r\n");
      } else if (pn.equals("TDateTimeEx")) {
        prsrImplX.append("  if (value.value.notNull) then\r\n");
        prsrImplX.append("    attribute(xml, 'value', asString(value.value));\r\n");
      } else if (!pn.equals("String")) {
        prsrImplX.append("  if (value.value <> nil) then\r\n");
        prsrImplX.append("    attribute(xml, 'value', asString(value.value));\r\n");
      } else
        prsrImplX.append("  attribute(xml, 'value', value.value);\r\n");
      prsrImplX.append("  xml.open(name);\r\n");
      prsrImplX.append("  composeElementChildren(xml, value);\r\n");
      prsrImplX.append("  closeOutElement(xml, value);\r\n");
      prsrImplX.append("  xml.close(name);\r\n");
      prsrImplX.append("end;\r\n\r\n");
      srlsdefJ.append("    Procedure Compose"+tn+"Value(json : TJSONWriter; name : String; value : TFhir"+tn+"; inArray : boolean);\r\n");
      srlsdefJ.append("    Procedure Compose"+tn+"Props(json : TJSONWriter; name : String; value : TFhir"+tn+"; inArray : boolean);\r\n");
      prsrImplJ.append("Procedure TFHIRJsonComposer.Compose"+tn+"Value(json : TJSONWriter; name : String; value : TFhir"+tn+"; inArray : boolean);\r\n");
      prsrImplJ.append("begin\r\n");
      if (pn.equals("Boolean"))
        prsrImplJ.append("  if (value = nil) then\r\n");
      else if (pn.equals("TDateTimeEx"))
        prsrImplJ.append("  if (value = nil) or (value.value.null) then\r\n");
      else if (!pn.equals("String"))
        prsrImplJ.append("  if (value = nil) or (value.value = nil) then\r\n");
      else 
        prsrImplJ.append("  if (value = nil) or (value.value = '') then\r\n");
      prsrImplJ.append("  begin\r\n");
      prsrImplJ.append("    if inArray then\r\n");
      prsrImplJ.append("      propNull(json, name);\r\n");
      prsrImplJ.append("    exit;\r\n");
      prsrImplJ.append("  end\r\n");
      prsrImplJ.append("  else\r\n");
      if (pn.equals("Boolean"))
        prsrImplJ.append("    prop(json, name, value.value);\r\n");
      else if (!pn.equals("String"))
        prsrImplJ.append("    prop(json, name, asString(value.value));\r\n");
      else if (tn.equals("Decimal") || tn.equals("Integer") || tn.equals("UnsignedInt")  || tn.equals("PositiveInt"))
        prsrImplJ.append("    propNumber(json, name, value.value);\r\n");
      else
        prsrImplJ.append("    prop(json, name, value.value);\r\n");
      prsrImplJ.append("end;\r\n\r\n");
      prsrImplJ.append("Procedure TFHIRJsonComposer.Compose"+tn+"Props(json : TJSONWriter; name : String; value : TFhir"+tn+"; inArray : boolean);\r\n");
      prsrImplJ.append("begin\r\n");
      prsrImplJ.append("  if (value = nil) or ((value.Id = '') and (not value.hasExtensionList) {no-comments and (not value.hasComments)}) then\r\n");
      prsrImplJ.append("  begin\r\n");
      prsrImplJ.append("    if inArray then\r\n");
      prsrImplJ.append("      propNull(json, name);\r\n");
      prsrImplJ.append("    exit;\r\n");
      prsrImplJ.append("  end\r\n");
      prsrImplJ.append("  else\r\n");
      prsrImplJ.append("  begin\r\n");
      prsrImplJ.append("    if (inArray) then\r\n");
      prsrImplJ.append("      json.valueObject('')\r\n");
      prsrImplJ.append("    else\r\n");
      prsrImplJ.append("      json.valueObject('_'+name);\r\n");
      prsrImplJ.append("    ComposeElementProperties(json, value);\r\n");
      prsrImplJ.append("    json.finishObject;\r\n");
      prsrImplJ.append("  end;\r\n");
      prsrImplJ.append("end;\r\n\r\n");

      srlsdefR.append("    Procedure Compose"+tn+"(parent :  TTurtleComplex; parentType, name : String; value : TFhir"+tn+"; useType : boolean; index : integer);\r\n");
      prsrImplT.append("Procedure TFHIRTurtleComposer.Compose"+tn+"(parent :  TTurtleComplex; parentType, name : String; value : TFhir"+tn+"; useType : boolean; index : integer);\r\n");
      prsrImplT.append("var\r\n");
      prsrImplT.append("  this : TTurtleComplex;\r\n");
      prsrImplT.append("begin\r\n");
      prsrImplT.append("  if (value = nil) then\r\n");
      prsrImplT.append("    exit;\r\n");
      prsrImplT.append("  this := parent.addPredicate('fhir:'+parentType+'.'+name);\r\n");
      prsrImplT.append("  if (useType) then\r\n");
      prsrImplT.append("    this.addPredicate('a', 'fhir:"+t.getCode()+"');\r\n");
      prsrImplT.append("  this.addPredicate('fhir:value', ttlLiteral(asString(value.value))"+typeParam(tn)+");\r\n");
      prsrImplT.append("  composeElement(this, parentType, name, value, false, index);\r\n");
      prsrImplT.append("end;\r\n\r\n");
    }

    defCodeType.classDefs.add(def.toString());
    defCodeType.classImpls.add(impl2.toString());
    defCodeType.classFwds.add("  TFhir"+tn+" = class;\r\n");
    defineList("TFhir"+tn, "TFhir"+tn+"List", pn, ClassCategory.Type, false, isEnum);
  }

  private String typeParam(String tn) {
    if (tn.equalsIgnoreCase("boolean")) return ", 'xsd:boolean'";
    if (tn.equalsIgnoreCase("integer")) return ", 'xsd:int'";
    if (tn.equalsIgnoreCase("string")) return ", 'xsd:string'";
    if (tn.equalsIgnoreCase("decimal")) return ", 'xsd:decimal'";
    if (tn.equalsIgnoreCase("uri")) return ", 'xsd:anyURI'";
    if (tn.equalsIgnoreCase("base64Binary")) return ", 'xsd:base64Binary'";
    if (tn.equalsIgnoreCase("instant")) return ", 'xsd:dateTime'";
    if (tn.equalsIgnoreCase("time")) return ", 'xsd:time'";
    if (tn.equalsIgnoreCase("code")) return ", 'xsd:token'";
    if (tn.equalsIgnoreCase("oid")) return ", 'xsd:anyURI'";
    if (tn.equalsIgnoreCase("id")) return ", 'xsd:string'";
    if (tn.equalsIgnoreCase("markdown")) return ", 'xsd:string'";
    if (tn.equalsIgnoreCase("unsignedInt")) return ", 'xsd:nonNegativeInteger'";
    if (tn.equalsIgnoreCase("positiveInt")) return ", 'xsd:positiveInteger'";
    if (tn.equalsIgnoreCase("date")) return ", dateXsdType(value.value)";
    if (tn.equalsIgnoreCase("dateTime")) return ", dateXsdType(value.value)";
    return "";
  }

  private String listForm(String name) {
    return generics ? "TAdvList<"+name+">" : name+"List";
  }

  private void generateElement() {
    StringBuilder def = new StringBuilder();

    //    def.append("  {@Class TFhirElement : TFHIRObject\r\n");
    //    def.append("    Base Element Definition - extensions, ids\r\n");
    //    def.append("  }\r\n");
    //    def.append("  TFhirElement = {abstract} class (TFHIRObject)\r\n");
    //    types.add("TFhirElement");
    //    def.append("  private\r\n");
    //    def.append("    FXmlId: String;\r\n");
    //    def.append("    FExtensionList : "+listForm("TFhirExtension")+";\r\n");
    //    def.append("    function GetExtensionList: "+listForm("TFhirExtension")+";\r\n");
    //    def.append("  protected\r\n");
    //    def.append("    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;\r\n");
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
    def.append("    Procedure SetStringValue(value : String); virtual;\r\n");
    def.append("    Function AsStringValue : String; Virtual;\r\n");
    def.append("  Public\r\n");
    def.append("    {!script hide}\r\n");
    def.append("    Function Link : TFHIRPrimitiveType; Overload;\r\n");
    def.append("    Function Clone : TFHIRPrimitiveType; Overload;\r\n");
    def.append("    Property StringValue : String read GetStringValue write SetStringValue;\r\n");
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
    //    impl2.append("procedure TFhirElement.GetChildrenByName(child_name : string; list : TFHIRSelectionList)+");\r\n");
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

    //    def.append("  {@Class TFhirBackboneElement : TFHIRObject\r\n");
    //    def.append("    Base Element Definition - extensions, ids\r\n");
    //    def.append("  }\r\n");
    //    def.append("  TFHIRBackboneElement = {abstract} class (TFhirElement)\r\n");
    //    types.add("TFHIRBackboneElement");
    //    def.append("  private\r\n");
    //    def.append("    FModifierExtensionList : "+listForm("TFhirExtension")+";\r\n");
    //    def.append("    function GetModifierExtensionList: "+listForm("TFhirExtension")+";\r\n");
    //    def.append("  protected\r\n");
    //    def.append("    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;\r\n");
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
    //    impl2.append("procedure TFHIRBackboneElement.GetChildrenByName(child_name : string; list : TFHIRSelectionList);\r\n");
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
    impl2.append("procedure TFHIRPrimitiveType.SetStringValue(value: String);\r\n" + 
        "begin\r\n" + 
        "  raise Exception.Create('Need to override '+ClassName+'.setStringValue');\r\n" + 
        "end;\r\n" + 
        "\r\n");
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

    prsrdefX.append("    Procedure ParseElementAttributes(value : TFhirElement; path : string; element : TMXmlElement);\r\n");
    prsrImplX.append("Procedure TFHIRXmlParser.ParseElementAttributes(value : TFhirElement; path : string; element : TMXmlElement);\r\n");
    prsrImplX.append("begin\r\n");
    prsrImplX.append("  TakeCommentsStart(value);\r\n");
    prsrImplX.append("  GetObjectLocation(value, element);\r\n");
    prsrImplX.append("  value.Id := GetAttribute(element, 'id');\r\n");
    prsrImplX.append("end;\r\n\r\n");

    prsrdefX.append("    Function ParseBackboneElementChild(element : TFhirBackboneElement; path : string; child : TMXmlElement) : boolean;\r\n");
    prsrImplX.append("Function TFHIRXmlParser.ParseBackboneElementChild(element : TFhirBackboneElement; path : string; child : TMXmlElement) : boolean;\r\n");
    prsrImplX.append("begin\r\n");
    prsrImplX.append("  result := true;\r\n");
    prsrImplX.append("  if (child.localName = 'modifierExtension') then\r\n");
    prsrImplX.append("    element.ModifierExtensionList.add(ParseExtension(child, path+'/modifierExtension'))\r\n");
    prsrImplX.append("  else\r\n");
    prsrImplX.append("    result := ParseElementChild(element, path, child);\r\n");
    prsrImplX.append("end;\r\n\r\n");

    prsrdefX.append("    Function ParseElementChild(element : TFhirElement; path : string; child : TMXmlElement) : boolean;\r\n");
    prsrImplX.append("Function TFHIRXmlParser.ParseElementChild(element : TFhirElement; path : string; child : TMXmlElement) : boolean;\r\n");
    prsrImplX.append("begin\r\n");
    prsrImplX.append("  result := true;\r\n");
    prsrImplX.append("  if (child.localName = 'extension') then\r\n");
    prsrImplX.append("    element.ExtensionList.add(ParseExtension(child, path+'/extension'))\r\n");
    prsrImplX.append("  else\r\n");
    prsrImplX.append("    result := false;\r\n");
    prsrImplX.append("end;\r\n\r\n");

    prsrdefJ.append("    procedure ParseElementProperties(jsn : TJsonObject; element : TFhirElement);\r\n");
    prsrdefT.append("    procedure ParseElementProperties(obj : TTurtleComplex; element : TFhirElement);\r\n");
    prsrImplJ.append("procedure TFHIRJsonParser.ParseElementProperties(jsn : TJsonObject; element : TFhirElement);\r\n");
    prsrImplJ.append("begin\r\n");
    prsrImplJ.append("  parseComments(element, jsn);\r\n\r\n");
    prsrImplJ.append("  element.LocationStart := jsn.LocationStart;\r\n");
    prsrImplJ.append("  element.LocationEnd := jsn.LocationEnd;\r\n");
    prsrImplJ.append("  if jsn.has('id') then\r\n");
    prsrImplJ.append("    element.Id := jsn['id']\r\n");
    prsrImplJ.append("  else if jsn.has('_id') then\r\n");
    prsrImplJ.append("    element.Id := jsn['_id'];\r\n");
    prsrImplJ.append("  if jsn.has('extension') then\r\n");
    prsrImplJ.append("    iterateArray(jsn.vArr['extension'], element.extensionList, parseExtension);\r\n");
    prsrImplJ.append("end;\r\n\r\n");
    prsrImplT.append("procedure TFHIRTurtleParser.ParseElementProperties(obj : TTurtleComplex; element : TFhirElement);\r\n");
    prsrImplT.append("var\r\n");
    prsrImplT.append("  item : TTurtleComplex;\r\n");
    prsrImplT.append("begin\r\n");
    prsrImplT.append("  element.LocationStart := obj.Start;\r\n");
    prsrImplT.append("  element.LocationEnd := obj.Stop;\r\n");
    prsrImplT.append("  element.idElement := ParseId(obj.complex('http://hl7.org/fhir/Element.id'));{q1}\r\n");
    prsrImplT.append("  for item in obj.complexes('http://hl7.org/fhir/Element.extension') do\r\n");
    prsrImplT.append("    element.extensionList.Add(parseExtension(item));\r\n");
    prsrImplT.append("end;\r\n\r\n");


    prsrdefJ.append("    procedure ParseBackboneElementProperties(jsn : TJsonObject; element : TFhirBackboneElement);\r\n");
    prsrdefT.append("    procedure ParseBackboneElementProperties(obj : TTurtleComplex; element : TFhirBackboneElement);\r\n");
    prsrImplJ.append("procedure TFHIRJsonParser.ParseBackboneElementProperties(jsn : TJsonObject; element : TFhirBackboneElement);\r\n");
    prsrImplJ.append("begin\r\n");
    prsrImplJ.append("  parseElementProperties(jsn, element);\r\n\r\n");
    prsrImplJ.append("  if jsn.has('modifierExtension') then\r\n");
    prsrImplJ.append("    iterateArray(jsn.vArr['modifierExtension'], element.modifierExtensionList, parseExtension);\r\n");
    prsrImplJ.append("end;\r\n\r\n");

    prsrImplT.append("procedure TFHIRTurtleParser.ParseBackboneElementProperties(obj : TTurtleComplex; element : TFhirBackboneElement);\r\n");
    prsrImplT.append("var\r\n");
    prsrImplT.append("  item : TTurtleComplex;\r\n");
    prsrImplT.append("begin\r\n");
    prsrImplT.append("  parseElementProperties(obj, element);\r\n");
    prsrImplT.append("  for item in obj.complexes('http://hl7.org/fhir/Element.modifierExtension') do\r\n");
    prsrImplT.append("    element.modifierExtensionList.Add(parseExtension(item));\r\n");
    prsrImplT.append("end;\r\n\r\n");

    srlsdefX.append("    Procedure ComposeElementAttributes(xml : TXmlBuilder; element : TFhirElement);\r\n");
    prsrImplX.append("Procedure TFHIRXmlComposer.ComposeElementAttributes(xml : TXmlBuilder; element : TFhirElement);\r\n");
    prsrImplX.append("begin\r\n");
    prsrImplX.append("  CommentsStart(xml, element);\r\n");
    prsrImplX.append("  Attribute(xml, 'id', element.Id);\r\n");
    prsrImplX.append("end;\r\n\r\n");
    srlsdefX.append("    Procedure ComposeElementChildren(xml : TXmlBuilder; element : TFhirElement);\r\n");
    prsrImplX.append("Procedure TFHIRXmlComposer.ComposeElementChildren(xml : TXmlBuilder; element : TFhirElement);\r\n");
    prsrImplX.append("var\r\n");
    prsrImplX.append("  i : integer;\r\n");
    prsrImplX.append("begin\r\n");
    prsrImplX.append("  if element.hasExtensionList then\r\n");
    prsrImplX.append("    for i := 0 to element.extensionList.count - 1 do\r\n");
    prsrImplX.append("      ComposeExtension(xml, 'extension', element.extensionList[i]);\r\n");
    prsrImplX.append("end;\r\n\r\n");
    srlsdefX.append("    Procedure ComposeBackboneElementChildren(xml : TXmlBuilder; element : TFhirBackboneElement);\r\n");
    prsrImplX.append("Procedure TFHIRXmlComposer.ComposeBackboneElementChildren(xml : TXmlBuilder; element : TFhirBackboneElement);\r\n");
    prsrImplX.append("var\r\n");
    prsrImplX.append("  i : integer;\r\n");
    prsrImplX.append("begin\r\n");
    prsrImplX.append("  ComposeElementChildren(xml, element);\r\n");
    prsrImplX.append("  if element.hasModifierExtensionList then\r\n");
    prsrImplX.append("    for i := 0 to element.modifierExtensionList.count - 1 do\r\n");
    prsrImplX.append("      ComposeExtension(xml, 'modifierExtension', element.modifierExtensionList[i]);\r\n");
    prsrImplX.append("end;\r\n\r\n");
    srlsdefJ.append("    Procedure ComposeElementProperties(json : TJSONWriter; elem : TFhirElement);\r\n");
    prsrImplJ.append("Procedure TFHIRJsonComposer.ComposeElementProperties(json : TJSONWriter; elem : TFhirElement);\r\n");
    prsrImplJ.append("var\r\n");
    prsrImplJ.append("  i : integer;\r\n");
    prsrImplJ.append("begin\r\n");
    prsrImplJ.append("  {no-comments composeComments(json, elem);}\r\n");
    prsrImplJ.append("  Prop(json, 'id', elem.Id);\r\n");
    prsrImplJ.append("  if elem.hasExtensionList then\r\n");
    prsrImplJ.append("  begin\r\n");
    prsrImplJ.append("    json.valueArray('extension');\r\n");
    prsrImplJ.append("    for i := 0 to elem.extensionList.Count - 1 do\r\n");
    prsrImplJ.append("      ComposeExtension(json, '',elem.extensionList[i]);\r\n");
    prsrImplJ.append("    json.FinishArray;\r\n");
    prsrImplJ.append("  end;\r\n");
    prsrImplJ.append("end;\r\n\r\n");
    srlsdefJ.append("    Procedure ComposeBackboneElementProperties(json : TJSONWriter; elem : TFhirBackboneElement);\r\n");
    prsrImplJ.append("Procedure TFHIRJsonComposer.ComposeBackboneElementProperties(json : TJSONWriter; elem : TFhirBackboneElement);\r\n");
    prsrImplJ.append("var\r\n");
    prsrImplJ.append("  i : integer;\r\n");
    prsrImplJ.append("begin\r\n");
    prsrImplJ.append("  ComposeElementProperties(json, elem);\r\n");
    prsrImplJ.append("  if elem.hasModifierExtensionList then\r\n");
    prsrImplJ.append("  begin\r\n");
    prsrImplJ.append("    json.valueArray('modifierExtension');\r\n");
    prsrImplJ.append("    for i := 0 to elem.modifierExtensionList.Count - 1 do\r\n");
    prsrImplJ.append("      ComposeExtension(json, '', elem.modifierExtensionList[i]);\r\n");
    prsrImplJ.append("    json.FinishArray;\r\n");
    prsrImplJ.append("  end;\r\n");
    prsrImplJ.append("end;\r\n\r\n");

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
    def.append("    frtNull, // Resource type not known / not Specified \r\n");
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
      con.append("{$IFDEF FHIR_"+s.toUpperCase()+"}'"+s);
      def.append("    {$IFDEF FHIR_"+s.toUpperCase()+"}"+s2+", {$ENDIF}\r\n");
      con.append("',{$ENDIF}\r\n      ");
      conl = conl + s.length()+4;
      if (conl > 1000) {
        con.append("\r\n      ");
        conl = 6;
      }
    }

    con.append(" 'Custom');");
    def.append("    frtCustom);\r\n  TFhirResourceTypeSet = set of TFhirResourceType;");

    con.append("\r\n  LOWERCASE_CODES_TFhirResourceType : Array[TFhirResourceType] of String = (");
    con.append("'', ");
    for (int i = 0; i < types.size(); i++) {
      String s = types.get(i);
      con.append("{$IFDEF FHIR_"+s.toUpperCase()+"}'"+s.toLowerCase()+"',{$ENDIF}\r\n     ");
    }
    con.append("'custom');\r\n     ");

    con.append("\r\n  CLASSES_TFhirResourceType : Array[TFhirResourceType] of TFhirResourceClass = (");
    con.append("nil, ");
    for (int i = 0; i < types.size(); i++) {
      String s = types.get(i);
      con.append("{$IFDEF FHIR_"+s.toUpperCase()+"}TFhir"+getTitle(s)+",{$ENDIF}\r\n     ");
    }
    con.append("nil);\r\n     ");

    con.append("\r\n  ALL_RESOURCE_TYPES = [");
    for (int i = 0; i < types.size(); i++) {
      String s = types.get(i);
      String s2 = prefix + getTitle(s);
      if (GeneratorUtils.isDelphiReservedWord(s2))
        s2 = s2 + "_";
      con.append("{$IFDEF FHIR_"+s.toUpperCase()+"}"+s2+",{$ENDIF}\r\n     ");
    }
    con.append("frtCustom];\r\n     ");

    con.append("\r\n  ALL_RESOURCE_TYPE_NAMES : Array [TFHIRResourceType] of String = ('--None--', ");
    for (int i = 0; i < types.size(); i++) {
      String s = types.get(i);
      String s2 = getTitle(s);
      con.append("{$IFDEF FHIR_"+s.toUpperCase()+"}'"+s2+"',{$ENDIF}\r\n     ");
    }
    con.append("'Custom');\r\n     ");


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
    prsrCodeX.uses.add("SysUtils");
    prsrCodeX.uses.add("Classes");
    prsrCodeX.uses.add("StringSupport");
    prsrCodeX.uses.add("DateSupport");
    prsrCodeX.uses.add("DecimalSupport");
    prsrCodeX.uses.add("FHIRParserBase");
    prsrCodeX.uses.add("FHIRBase");
    prsrCodeX.uses.add("FHIRResources");
    prsrCodeX.uses.add("FHIRConstants");
    prsrCodeX.uses.add("FHIRTypes");
    prsrCodeX.uses.add("AdvStringMatches");

    prsrCodeJ.uses.add("SysUtils");
    prsrCodeJ.uses.add("Classes");
    prsrCodeJ.uses.add("StringSupport");
    prsrCodeJ.uses.add("DateSupport");
    prsrCodeJ.uses.add("DecimalSupport");
    prsrCodeJ.uses.add("FHIRParserBase");
    prsrCodeJ.uses.add("FHIRBase");
    prsrCodeJ.uses.add("FHIRResources");
    prsrCodeJ.uses.add("FHIRConstants");
    prsrCodeJ.uses.add("FHIRTypes");
    prsrCodeJ.uses.add("AdvStringMatches");

    prsrCodeT.uses.add("SysUtils");
    prsrCodeT.uses.add("Classes");
    prsrCodeT.uses.add("StringSupport");
    prsrCodeT.uses.add("DateSupport");
    prsrCodeT.uses.add("DecimalSupport");
    prsrCodeT.uses.add("FHIRParserBase");
    prsrCodeT.uses.add("FHIRBase");
    prsrCodeT.uses.add("FHIRResources");
    prsrCodeT.uses.add("FHIRConstants");
    prsrCodeT.uses.add("FHIRTypes");
    prsrCodeT.uses.add("AdvStringMatches");

    prsrCodeX.uses.add("XmlBuilder");
    prsrCodeX.uses.add("MXml");
    prsrCodeJ.uses.add("AdvJSON");
    prsrCodeT.uses.add("TurtleParser");
    
    prsrCodeX.comments.add("FHIR v"+version+" generated "+dateTimeType.asStringValue());
    prsrCodeJ.comments.add("FHIR v"+version+" generated "+dateTimeType.asStringValue());
    prsrCodeT.comments.add("FHIR v"+version+" generated "+dateTimeType.asStringValue());

    prsrImplX.append(
        "{ TFHIRXmlParser }\r\n"+
            "\r\n"
        );

    prsrImplJ.append(
        "{ TFHIRJsonParser }\r\n"+
            "\r\n"
        );

    prsrImplT.append(
        "{ TFHIRTurtleParser }\r\n"+
            "\r\n"
        );

  }

  private String buildParserXml() {

    prsrImplX.append(
        "function TFHIRXmlParser.ParseResource(element : TMXmlElement; path : String) : TFhirResource;\r\n"+
            "begin\r\n"+
            "  if (element = nil) Then\r\n"+
            "    Raise Exception.Create('error - element is nil')\r\n"+
            prsrRegX.toString()+
            "  else\r\n"+
            "    raise Exception.create('Error: the element '+element.localName+' is not recognised as a valid resource name');\r\n" +
            "end;\r\n\r\n"
        );

    prsrImplX.append(
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
    prsrImplX.append(
        "function TFHIRXmlParser.ParseFragment(element : TMXmlElement) : TFHIRObject;\r\n"+
            "begin\r\n  "+
            prsrFragX.toString().substring(6)+
            "  else\r\n"+
            "    raise Exception.create('error: the element '+element.Name+' is not a valid fragment name');\r\n" +
            "end;\r\n\r\n"
        );
    prsrImplX.append(
        "function TFHIRXmlParser.ParseDataType(element : TMXmlElement; name : String; type_ : TFHIRTypeClass) : TFhirType;\r\n"+
            "begin\r\n  "+
            "  if (name <> '') and (name <> element.localName) then\r\n"+
            "    raise Exception.Create('Expected Name mismatch : expected \"'+name+'\"+, but found \"'+element.localName+'\"');\r\n"+
            prsrDTX.toString().substring(6)+
            "  else\r\n"+
            "    raise Exception.create('Unknown Type');\r\n" +
            "end;\r\n\r\n"
        );
    prsrImplX.append(
        "procedure TFHIRXmlComposer.ComposeBase(xml : TXmlBuilder; name : String; base : TFHIRObject);\r\n"+
            "begin\r\n  "+
            compXBase.toString().substring(6)+
            "  else\r\n"+
            "    raise Exception.create('Unknown Type '+base.className);\r\n" +
            "end;\r\n\r\n"
        );

    return
        "  TFHIRXmlParser = class (TFHIRXmlParserBase)\r\n"+
        "  protected\r\n"+
        prsrdefX.toString()+
        "    function ParseResource(element : TMXmlElement; path : String) : TFhirResource; override;\r\n"+
        "    function ParseDataType(element : TMXmlElement; name : String; type_ : TFHIRTypeClass) : TFHIRType; override;\r\n"+
        "  public\r\n"+
        "    function ParseFragment(element : TMXmlElement) : TFHIRObject; overload;\r\n"+
        "  end;\r\n\r\n"+
        "  TFHIRXmlComposer = class (TFHIRXmlComposerBase)\r\n"+
        "  protected\r\n"+
        srlsdefX.toString()+
        "    procedure ComposeResource(xml : TXmlBuilder; resource : TFhirResource; links : TFhirBundleLinkList); override;\r\n"+
        "    procedure ComposeBase(xml : TXmlBuilder; name : String; base : TFHIRObject); override;\r\n"+
        "  end;\r\n\r\n";
  }
  
  private String buildParserJson() {
    prsrImplJ.append(
        "function TFHIRJsonParser.ParseResource(jsn : TJsonObject) : TFhirResource;\r\n"+
            "var\r\n" +
            "  s : String;\r\n" +
            "begin\r\n"+
            "  s := jsn['resourceType'];\r\n "+
            removeFirst(prsrRegJ.toString(), "else ")+
            "  else\r\n"+
            "    raise Exception.create('error: the element '+s+' is not a valid resource name');\r\n" +
            "end;\r\n\r\n"
        );

    prsrImplJ.append(
        "function TFHIRJsonParser.ParseFragment(jsn : TJsonObject; type_ : String) : TFHIRObject;\r\n"+
            "begin\r\n  "+
            prsrFragJ.toString().substring(6)+
            "  else\r\n"+
            "    raise Exception.create('error: the element '+type_+' is not a valid fragment name');\r\n" +
            "end;\r\n\r\n"
        );
    prsrImplJ.append(
        "function TFHIRJsonParser.ParseDataType(jsn : TJsonObject; name : String; type_ : TFHIRTypeClass) : TFHIRType;\r\n"+
            "begin\r\n  "+
            prsrDTJ.toString().substring(6)+
            "  else\r\n"+
            "    raise Exception.create('Unknown Type');\r\n" +
            "end;\r\n\r\n"
        );

    prsrImplJ.append(
        "procedure TFHIRJsonComposer.ComposeBase(json: TJSONWriter; name: String; base: TFHIRObject);\r\n"+
            "begin\r\n  "+
            compJBase.toString().substring(6)+
            "  else\r\n"+
            "    raise Exception.create('Unknown Type '+base.className);\r\n" +
            "end;\r\n\r\n"
        );

    prsrImplJ.append(
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

    return
        "  TFHIRJsonParser = class (TFHIRJsonParserBase)\r\n"+
        "  protected\r\n"+
        prsrdefJ.toString()+
        "    function ParseResource(jsn : TJsonObject) : TFhirResource; override;\r\n"+
        "    function ParseDataType(jsn : TJsonObject; name : String; type_ : TFHIRTypeClass) : TFHIRType; override;\r\n"+
        "  public\r\n"+
        "    function ParseFragment(jsn : TJsonObject; type_ : String) : TFHIRObject;  overload;\r\n"+
        "  end;\r\n\r\n"+
        "  TFHIRJsonComposer = class (TFHIRJsonComposerBase)\r\n"+
        "  protected\r\n"+
        srlsdefJ.toString()+
        "    procedure ComposeResource(json : TJSONWriter; resource : TFhirResource; links : TFhirBundleLinkList); override;\r\n"+
        "    procedure ComposeBase(json : TJSONWriter; name : String; base : TFHIRObject); override;\r\n"+
        "  end;\r\n\r\n";
  }

  private String removeFirst(String string, String find) {
    int i = string.indexOf(find);
    return string.substring(0, i) + string.substring(i+find.length());
  }

  private String buildParserTurtle() {
    prsrImplT.append(
        "function TFHIRTurtleParser.ParseFragment(obj : TTurtleComplex; type_ : String) : TFHIRObject;\r\n"+
            "begin\r\n  "+
            prsrFragR.toString().substring(6)+
            "  else\r\n"+
            "    raise Exception.create('error: the element '+type_+' is not a valid fragment name');\r\n" +
            "end;\r\n\r\n"
        );

    prsrImplT.append(
        "function TFHIRTurtleParser.ParseDataType(obj : TTurtleComplex; name : String; type_ : TFHIRTypeClass) : TFHIRType;\r\n"+
            "begin\r\n  "+
            prsrDTT.toString().substring(6)+
            "  else\r\n"+
            "    raise Exception.create('Unknown Type');\r\n" +
            "end;\r\n\r\n"
        );


    prsrImplT.append(
        "procedure TFHIRTurtleComposer.ComposeResource(parent : TTurtleComplex; resource : TFhirResource);\r\n"+
            "var\r\n"+
            "  this : TTurtleComplex;\r\n"+
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


    prsrImplT.append(
        "function TFHIRTurtleParser.ParseResource(obj : TTurtleComplex) : TFhirResource;\r\n"+
            "var\r\n" +
            "  s : String;\r\n" +
            "begin\r\n"+
            "  s := rdfsType(obj);\r\n "+
            removeFirst(prsrRegT.toString(), "else ")+
            "  else\r\n"+
            "    raise Exception.create('error: the element '+s+' is not a valid resource name');\r\n" +
            "end;\r\n\r\n"
        );


    return
        "  TFHIRTurtleParser = class (TFHIRTurtleParserBase)\r\n"+
        "  protected\r\n"+
        prsrdefT.toString()+
        "    function ParseResource(obj : TTurtleComplex) : TFhirResource; override;\r\n"+
        "    function ParseDataType(obj : TTurtleComplex; name : String; type_ : TFHIRTypeClass) : TFHIRType; override;\r\n"+
        "  public\r\n"+
        "    function ParseFragment(obj : TTurtleComplex; type_ : String) : TFHIRObject;  overload;\r\n"+
        "  end;\r\n\r\n"+
        "  TFHIRTurtleComposer = class (TFHIRTurtleComposerBase)\r\n"+
        "  protected\r\n"+
        srlsdefR.toString()+
        "  \r\n"+
        "    procedure ComposeResource(parent :  TTurtleComplex; resource : TFhirResource); overload; override;\r\n"+
        "  end;\r\n\r\n";
  }

}
