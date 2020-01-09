package org.fhir.pascal.generator.codegen;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.fhir.delphi.GeneratorUtils;
import org.fhir.pascal.generator.engine.Configuration;
import org.fhir.pascal.generator.engine.Definitions;
import org.hl7.fhir.r5.model.Enumerations.FHIRAllTypes;
import org.hl7.fhir.r5.model.Enumerations.OperationParameterUse;
import org.hl7.fhir.r5.model.OperationDefinition;
import org.hl7.fhir.r5.model.OperationDefinition.OperationDefinitionParameterComponent;
import org.hl7.fhir.r5.model.StructureDefinition;
import org.hl7.fhir.r5.model.StructureDefinition.StructureDefinitionKind;
import org.hl7.fhir.utilities.Utilities;


public class OperationsGenerator extends BaseGenerator {


  private StringBuilder o1 = new StringBuilder();
  private StringBuilder o2 = new StringBuilder();
  private Set<String> opNames = new HashSet<String>();

  public OperationsGenerator(Definitions definitions, Configuration configuration, Date genDate, String version) throws UnsupportedEncodingException {
    super(definitions, configuration, version, genDate);
  }

	public void generate(String filename) throws Exception {
	  String template = config.getTemplate("FHIR.R5.Operations");
    template = template.replace("{{mark}}", startVMarkValue());
    template = template.replace("{{op.intf}}", o1.toString());
    template = template.replace("{{op.impl}}", o2.toString());

    OutputStreamWriter w = new OutputStreamWriter(new FileOutputStream(filename));
    w.write(template);
		w.flush();
		w.close();
	}

  public void genOperation(OperationDefinition op) throws IOException {
    String name = Utilities.capitalize(getPascalName(op.getCode()));
    if (opNames.contains(name))
      return;
    opNames.add(name);

    line(o1, "  //Operation "+op.getCode()+" ("+op.getName()+")");

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
          pt = simpleTypes.get("TFhir"+Utilities.capitalize(p.getType().toCode()));
        if (pt == null)
          pt = "TFhir"+p.getType().toCode();
        if (pt.equals("TFhirAny"))
          pt = "TFhirResource";
        if (pt.equals("TFhir*"))
          pt = "TFhirType";
        if (pt.equals("TFhirElement"))
          pt = "TFhirDataType";
        boolean obj = !Utilities.existsInList(pt, "String", "Boolean", "Integer", "TFslDateTime");
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
            create.append("      F"+Utilities.capitalize(pn)+"List.Add((p.value as TFhir"+Utilities.capitalize(p.getType().toCode())+").value);"+marker()+"\r\n");
            params.append("      result.AddParameter('"+p.getName()+"', TFhir"+Utilities.capitalize(p.getType().toCode())+".create("+v+"));\r\n");
            screate.append("  for s in params.getVar('"+p.getName()+"').Split([';']) do\r\n");
            screate.append("    F"+Utilities.capitalize(pn)+"List.add(s); \r\n");
            usesS = true;

          } else {
            fields.append("    F"+Utilities.capitalize(pn)+"List : TFslList<"+pt+">;\r\n");
            gencreate.append("  F"+Utilities.capitalize(pn)+"List := TFslList<"+pt+">.create;\r\n");
            destroy.append("  F"+Utilities.capitalize(pn)+"List.free;\r\n");
            properties.append("    property "+pn+"List : TFslList<"+pt+"> read F"+Utilities.capitalize(pn)+"List;\r\n");

            vars.add(pt);
            String v = "v"+Integer.toString(vars.size());
            create.append("  for p in params."+( use == null ? "partList" : "parameterList")+" do\r\n"+
                "    if p.name = '"+p.getName()+"' then\r\n");
            params.append("    for "+v+" in F"+Utilities.capitalize(pn)+"List do\r\n");
            if (isResource(pt.substring(5))) {
              create.append("      F"+Utilities.capitalize(pn)+"List.Add((p.resource as "+pt+").Link);"+marker()+"\r\n");
              params.append("      result.AddParameter('"+p.getName()+"', "+v+".Link);\r\n");
            } else if (isPrimitive(p.getType())) {
              create.append("      F"+Utilities.capitalize(pn)+"List.Add(p.value.Link);"+marker()+"\r\n");
              params.append("      result.AddParameter('"+p.getName()+"', "+v+".Link);\r\n");
              screate.append("  !F"+Utilities.capitalize(pn)+" := StrToBoolDef(params.getVar('"+p.getName()+"'), false); - 3\r\n");
            } else if (complex) {
              create.append("      F"+Utilities.capitalize(pn)+"List.Add("+pt+".create(p));"+marker()+"\r\n");
              params.append("      result.AddParameter("+v+".asParams('"+p.getName()+"'));\r\n");
            } else {
              create.append("      F"+Utilities.capitalize(pn)+"List.Add((p.value as "+pt+").Link);"+marker()+"\r\n");
              params.append("      result.AddParameter('"+p.getName()+"', "+v+".Link);\r\n");
            }
          }
        } else {
          fields.append("    F"+Utilities.capitalize(pn)+" : "+pt+";\r\n");
          if (obj) {
            accessors.append("    procedure Set"+Utilities.capitalize(pn)+"(value : "+pt+");\r\n");
            destroy.append("  F"+Utilities.capitalize(pn)+".free;\r\n");
            line(o2, "procedure TFHIR"+name+"Op"+suffix+".Set"+Utilities.capitalize(pn)+"(value : "+pt+");\r\nbegin\r\n  F"+Utilities.capitalize(pn)+".free;\r\n  F"+Utilities.capitalize(pn)+" := value;\r\nend;");
          }
          properties.append("    property "+pn+" : "+pt+" read F"+Utilities.capitalize(pn)+" write "+(obj ? "Set" : "F")+Utilities.capitalize(pn)+";\r\n");

          if (!pt.equals("Boolean")) {
            if (pt.equals("String"))
              params.append("    if (F"+Utilities.capitalize(pn)+" <> '') then\r\n");
            else if (pt.equals("TFslDateTime"))
              params.append("    if (F"+Utilities.capitalize(pn)+".notNull) then\r\n");
            else
              params.append("    if (F"+Utilities.capitalize(pn)+" <> nil) then\r\n");
          }
          if (!isPrimitive(p.getType()) && !"token".equals(p.getType().toCode())) {
            if (isResource(p.getType().toCode()) || p.getType() == FHIRAllTypes.ANY) {
              params.append("      result.addParameter('"+p.getName()+"', F"+Utilities.capitalize(pn)+".Link);"+marker()+"\r\n");
              create.append("  F"+Utilities.capitalize(pn)+" := (params.res['"+p.getName()+"'] as "+pt+").Link;"+marker()+"\r\n");
            } else if (pt.equals("String")) {
              params.append("      result.addParameter('"+p.getName()+"', TFHIR"+Utilities.capitalize(p.getType().toCode())+".create(F"+Utilities.capitalize(pn)+"));"+marker()+"\r\n");
              create.append("  if params.param['"+p.getName()+"'] <> nil then\r\n    F"+Utilities.capitalize(pn)+" := (params.param['"+p.getName()+"'].value as TFHIR"+Utilities.capitalize(p.getType().toCode())+").Value; "+marker()+"\r\n");
            } else if (pt.equals("Boolean")) {
              params.append("      result.addParameter('"+p.getName()+"', TFHIR"+Utilities.capitalize(p.getType().toCode())+".create(F"+Utilities.capitalize(pn)+"));"+marker()+"\r\n");
              create.append("  if params.param['"+p.getName()+"'] <> nil then\r\n    F"+Utilities.capitalize(pn)+" := (params.param['"+p.getName()+"'].value as TFHIR"+pt+").Value; "+marker()+"\r\n");
            } else if (pt.equals("TFslDateTime")) {
              params.append("      result.addParameter('"+p.getName()+"', TFHIR"+Utilities.capitalize(p.getType().toCode())+".create(F"+Utilities.capitalize(pn)+"));"+marker()+"\r\n");
              create.append("  if params.param['"+p.getName()+"'] <> nil then\r\n    F"+Utilities.capitalize(pn)+" := (params.param['"+p.getName()+"'].value as TFHIRDateTime).Value; "+marker()+"\r\n");
            } else if (complex) {
              params.append("      result.addParameter(F"+Utilities.capitalize(pn)+".asParams('"+p.getName()+"'));"+marker()+"\r\n");
              create.append("  F"+Utilities.capitalize(pn)+" := "+pt+".create(params.param['"+p.getName()+"']); "+marker()+"\r\n");
            } else {
              params.append("      result.addParameter('"+p.getName()+"', F"+Utilities.capitalize(pn)+".Link);"+marker()+"\r\n");
              create.append("  if params.param['"+p.getName()+"'] <> nil then\r\n    F"+Utilities.capitalize(pn)+" := (params.param['"+p.getName()+"'].value as "+pt+").Link;"+marker()+"\r\n");
            }
          } else if (obj) {
            params.append("      result.addParameter('"+p.getName()+"', TFHIR"+Utilities.capitalize(p.getType().toCode())+".create(F"+Utilities.capitalize(pn)+"));"+marker()+"\r\n");
            create.append("  F"+Utilities.capitalize(pn)+" := (params.param['"+p.getName()+"'].value as TFHIR"+Utilities.capitalize(p.getType().toCode())+").value;\r\n");
          } else {
            params.append("      result.addParameter('"+p.getName()+"', TFHIR"+Utilities.capitalize(p.getType().toCode())+".create(F"+Utilities.capitalize(pn)+"));"+marker()+"\r\n");
            if (pt.equals("Boolean")) {
              create.append("  F"+Utilities.capitalize(pn)+" := params.bool['"+p.getName()+"'];\r\n");
              screate.append("  F"+Utilities.capitalize(pn)+" := StrToBoolDef(params.getVar('"+p.getName()+"'), false);\r\n");
            } else if (pt.equals("TFslDateTime")) {
              create.append("  F"+Utilities.capitalize(pn)+" := TFslDateTime.fromXml(params.str['"+p.getName()+"']);\r\n");
              screate.append("  F"+Utilities.capitalize(pn)+" := TFslDateTime.fromXml(params.getVar('"+p.getName()+"'));\r\n");
            } else {
              create.append("  F"+Utilities.capitalize(pn)+" := params.str['"+p.getName()+"'];\r\n");
              screate.append("  F"+Utilities.capitalize(pn)+" := params.getVar('"+p.getName()+"');\r\n");
            }
          }
        }
      }
    }

    if (use == null) 
      o1.append(
          "  TFHIR"+name+"Op"+suffix+" = class (TFHIROperationObject)\r\n  private\r\n"+
              fields.toString()+
              accessors.toString()+
              "  protected\r\n"+
              "    function isKnownName(name : String) : boolean; override;\r\n"+
              "  public\r\n"+
              "    constructor Create; overload; override;\r\n"+
              "    constructor Create(params : TFhirParametersParameter"+"); overload; override;\r\n"+
              "    destructor Destroy; override;\r\n"+
              "    function asParams(name : String) : TFHIRParametersParameter"+"; override;\r\n"+
              properties.toString()+
          "  end;\r\n");
    else
      o1.append(
          "  TFHIR"+name+"Op"+suffix+" = class (TFHIROperation"+suffix+")\r\n  private\r\n"+
              fields.toString()+
              accessors.toString()+
              "  protected\r\n"+
              "    function isKnownName(name : String) : boolean; override;\r\n"+
              "  public\r\n"+
              "    constructor Create; overload; override;\r\n"+
              "    destructor Destroy; override;\r\n"+
              "    procedure load(params : TFHIRParameters"+"); overload; override;\r\n"+
              "    procedure load(params : TParseMap); overload; override;\r\n"+
              "    function asParams : TFHIRParameters"+"; override;\r\n"+
              properties.toString()+
          "  end;\r\n");
    o2.append("constructor TFHIR"+name+"Op"+suffix+".create;\r\n"+
        "begin\r\n"+
        "  inherited create();\r\n"+
        gencreate.toString()+
        "end;\r\n");
    if (use == null)
      o2.append("constructor TFHIR"+name+"Op"+suffix+".create(params : TFhirParametersParameter"+");\r\n"+
          (gencreate.length() > 0 ? "var\r\n  p : TFhirParametersParameter"+";\r\n" : "")+
          "begin\r\n"+
          "  inherited create();\r\n"+
          gencreate.toString()+
          create.toString()+
          "  loadExtensions(params);\r\n"+
          "end;\r\n");
    if (use != null) {
      o2.append("procedure TFHIR"+name+"Op"+suffix+".load(params : "+(use == null ? "TFhirParametersParameter"+"" : "TFHIRParameters"+"")+");\r\n"+
          (gencreate.length() > 0 ? "var\r\n  p : TFhirParametersParameter"+";\r\n" : "")+
          "begin\r\n"+
          create.toString()+
          "  loadExtensions(params);\r\n"+
          "end;\r\n");
      o2.append("procedure TFHIR"+name+"Op"+suffix+".load(params : TParseMap);\r\n"+
          (usesS ? "var\r\n  s : String;\r\n" : "")+
          "begin\r\n"+
          screate.toString()+
          "  loadExtensions(params);\r\n"+
          "end;\r\n");
    }
    o2.append("destructor TFHIR"+name+"Op"+suffix+".Destroy;\r\nbegin\r\n"+destroy.toString()+"  inherited;\r\nend;\r\n");
    if (use == null) 
      o2.append("function TFHIR"+name+"Op"+suffix+".asParams(name : String) : TFhirParametersParameter"+";");
    else
      o2.append("function TFHIR"+name+"Op"+suffix+".asParams : TFhirParameters"+";");
    if (vars.size() > 0) {
      o2.append("var");
      for (int i = 0; i < vars.size(); i++) {
        o2.append("  v"+Integer.toString(i+1)+" : "+vars.get(i)+";");
      }
    }
    if (use == null) 
      o2.append(
          "begin\r\n"+
              "  result := TFHIRParametersParameter"+".create;\r\n"+
              "  try\r\n"+
              "    result.name := name;\r\n"+
              params.toString()+
              "    writeExtensions(result);\r\n"+
          "    result.link;\r\n  finally\r\n    result.free;\r\n  end;\r\nend;\r\n");
    else
      o2.append(
          "begin\r\n"+
              "  result := TFHIRParameters"+".create;\r\n"+
              "  try\r\n"+
              params.toString()+
              "    writeExtensions(result);\r\n"+
          "    result.link;\r\n  finally\r\n    result.free;\r\n  end;\r\nend;\r\n");
    o2.append("function TFHIR"+name+"Op"+suffix+".isKnownName(name : String) : boolean;");
    if (names.length() == 0)
      o2.append(
          "begin\r\n"+
              "  result := false;\r\n"+
          "end;\r\n");
    else
      o2.append(
          "begin\r\n"+
              "  result := StringArrayExists(["+names.substring(2)+"], name);\r\n"+
          "end;\r\n");
  }


  private boolean isPrimitive(FHIRAllTypes type) {
    return type == null ? false : simpleTypes.containsKey(type.toCode());
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
}
