package org.fhir.pascal.generator.codegen;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.Date;

import org.fhir.delphi.DelphiGenerator.ClassCategory;
import org.fhir.pascal.generator.analysis.Analysis;
import org.fhir.pascal.generator.analysis.TypeInfo;
import org.fhir.pascal.generator.codegen.ClassGenerator.ClassGeneratorCategory;
import org.fhir.pascal.generator.engine.Configuration;
import org.fhir.pascal.generator.engine.Definitions;
import org.hl7.fhir.r5.model.ElementDefinition;
import org.hl7.fhir.r5.model.ElementDefinition.TypeRefComponent;
import org.hl7.fhir.utilities.Utilities;



public class JavascriptGenerator extends BaseGenerator {

  
  private StringBuilder r1 = new StringBuilder();
  private StringBuilder r2 = new StringBuilder();

  public JavascriptGenerator(Definitions definitions, Configuration configuration, Date genDate, String version) throws UnsupportedEncodingException {
    super(definitions, configuration, version, genDate);
  }

	public void generate(String filename) throws Exception {
	  String template = config.getTemplate("FHIR.R5.Javascript");

    template = template.replace("{{mark}}", startVMarkValue());
    template = template.replace("{{js.register.routines}}", r1.toString());
    template = template.replace("{{js.register.reg}}", r2.toString());

    OutputStreamWriter w = new OutputStreamWriter(new FileOutputStream(filename));
    w.write(template);
		w.flush();
		w.close();
	}

  public void seeType(Analysis analysis) {
    if (analysis.getName().equals("Base")) {
      return; // we don't generate base.
    }
    for (TypeInfo ti : analysis.getTypeList()) {
      generateType(analysis, ti, ClassGeneratorCategory.Type);
    }
    generateType(analysis, analysis.getRootType(), ClassGeneratorCategory.Type);
    
  }

  public void seeResource(Analysis analysis) {
    if (analysis.getName().equals("Base")) {
      return; // we don't generate base.
    }
    if (!analysis.isAbstract()) {
//      xpr3.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
    }
    for (TypeInfo ti : analysis.getTypeList()) {
      generateType(analysis, ti, ClassGeneratorCategory.Component);    
    }
    generateType(analysis, analysis.getRootType(), ClassGeneratorCategory.Resource);
    if (!analysis.isAbstract()) {
//      xpr3.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
    }    
    
  }

  private void generateType(Analysis analysis, TypeInfo ti, ClassGeneratorCategory category) {
    String tn = ti.getName();
    String tj = tn.substring(5);
    if (!analysis.isAbstract())
      r2.append("  define"+tj+"Js(js, vs); \r\n");
    StringBuilder jsReg = new StringBuilder();
    jsReg.append("  define"+ti.getAncestorName()+"PropsJs(js, def, xv);\r\n");

    for (ElementDefinition e : ti.getChildren()) {
      generateField(analysis, ti, e, tn, "", category, jsReg, tj);
    }

    
    StringBuilder jsClass = new StringBuilder(); 
    jsClass.append("procedure define"+tj+"PropsJs(js : TFHIRJavascript; def : TJavascriptClassDefinition; xv : boolean);\r\n");
    jsClass.append("begin\r\n");
    jsClass.append(jsReg.toString());
    jsClass.append("end;\r\n\r\n");
    if (!analysis.isAbstract()) {
      jsClass.append("procedure define"+tj+"Js(js : TFHIRJavascript; xv : boolean);\r\n");
      jsClass.append("var\r\n  def : TJavascriptClassDefinition;\r\nbegin\r\n");
      jsClass.append("  def := js.defineClass('"+tj+"'+v(xv), nil, "/*+"'"+tj+"'+v(xv), "*/+"js.FHIRFactoryJs);\r\n");
      jsClass.append("  define"+tj+"PropsJs(js, def, xv);\r\n");
      jsClass.append("end;\r\n\r\n");
    }
    r1.append(jsClass.toString());
    
  }

  private void generateField(Analysis analysis, TypeInfo ti, ElementDefinition e, String tn, String string, ClassGeneratorCategory category, StringBuilder jsReg, String tj) {
    if (e.unbounded()) {
      jsRegisterUnboundedField(jsReg, tj, tn, e);
    } else {
      jsRegisterSingletonField(jsReg, tj, tn, e);
    }

  }
	
  private void jsRegisterSingletonPrimitiveField(StringBuilder jsReg, String tj, ElementDefinition e, String specType) {
    // handle split between primitive value and id/extension
    // 1st: primitive value
    String en = getElementName(e.getName());

    if (specType.equals("")) {
      String rawTn = getRawTnForType(e.typeSummary());
      jsReg.append("  js.registerElement(def, '"+tj+"'+v(xv), '"+en+specType+"', '"+e.typeSummary()+"'+v(xv), js.getFHIR"+rawTn+"Prop, js.setFHIR"+rawTn+"Prop);\r\n");
    } else {
      String rawTn = getRawTnForType(Utilities.uncapitalize(specType));
      jsReg.append("  js.registerElement(def, '"+tj+"'+v(xv), '"+en.replace("[x]", "")+specType+"', '"+Utilities.uncapitalize(specType)+"'+v(xv), js.getFHIR"+rawTn+"Prop, js.setFHIR"+rawTn+"Prop);\r\n");
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

  private void jsRegisterSingletonField(StringBuilder jsReg, String tj, String tn, ElementDefinition e) {
    if (e.getName().contains("[x]")) {
      for (TypeRefComponent t : e.getType()) {
        if (isPrimitive(t.getCode())) {
          jsRegisterSingletonPrimitiveField(jsReg, tj, e, Utilities.capitalize(t.getName()));
        } else { 
          String enf = e.getName().replace("[x]", "");
          String en = getElementName(e.getName().replace("[x]", ""));
  
          jsReg.append("  js.registerElement(def, '"+tj+"'+v(xv), '"+enf+t.getName()+"', '"+t.getName()+"'+v(xv), js.getFHIRObjectProp, js.setFHIRObjectProp);\r\n");
        }
      }
    } else {
      if (isPrimitive(e.typeSummary())) {
        jsRegisterSingletonPrimitiveField(jsReg, tj, e, "");
      } else { 
        String en = getElementName(e.getName());
        if (tn.contains("{"))
          tn = tn.substring(0,  tn.indexOf("{"));

        if (Utilities.noString(e.typeSummary())) {
//          String bt = typeNames.get(e).substring(5); 
          jsReg.append("  js.registerElement(def, '"+tj+"'+v(xv), '"+e.getName()+"', '??"+"'+v(xv), js.getFHIRObjectProp, js.setFHIRObjectProp);\r\n");
        } else
          jsReg.append("  js.registerElement(def, '"+tj+"'+v(xv), '"+e.getName()+"', '"+e.typeSummary()+"'+v(xv), js.getFHIRObjectProp, js.setFHIRObjectProp);\r\n");
      }
    }
  }

  private void jsRegisterUnboundedField(StringBuilder jsReg, String tj, String tn, ElementDefinition e) {
    if (isPrimitive(e.typeSummary())) {
//      jsRegisterSingletonSimpleField(jsClass, jsReg, jsLoad, tj, e, null);
    } else { 
      String en = getElementName(e.getName());
      if (tn.contains("{"))
        tn = tn.substring(0,  tn.indexOf("{"));

      if (Utilities.noString(e.typeSummary())) {
        jsReg.append("  js.registerElement(def, '"+tj+"'+v(xv), '"+e.getName()+"', '??"+"'+v(xv), js.getFHIRArrayProp, js.setFHIRArrayProp);\r\n");
      } else
        jsReg.append("  js.registerElement(def, '"+tj+"'+v(xv), '"+e.getName()+"', '"+e.typeSummary()+"'+v(xv), js.getFHIRArrayProp, js.setFHIRArrayProp);\r\n");
    }
  }



}
