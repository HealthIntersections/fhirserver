package org.fhir.pascal.generator.codegen;

import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import org.fhir.delphi.DefinedCode;
import org.fhir.delphi.ElementDefn;
import org.fhir.pascal.generator.analysis.Analysis;
import org.fhir.pascal.generator.analysis.TypeInfo;
import org.fhir.pascal.generator.codegen.ClassGenerator.ClassGeneratorCategory;
import org.fhir.pascal.generator.codegen.FormatsGenerator.StructureDefinitionSorter;
import org.fhir.pascal.generator.engine.Configuration;
import org.fhir.pascal.generator.engine.Definitions;
import org.hl7.fhir.r5.model.ElementDefinition;
import org.hl7.fhir.r5.model.ElementDefinition.PropertyRepresentation;
import org.hl7.fhir.r5.model.ElementDefinition.TypeRefComponent;
import org.hl7.fhir.r5.model.StructureDefinition.StructureDefinitionKind;
import org.hl7.fhir.r5.model.StructureDefinition;
import org.hl7.fhir.utilities.Utilities;

/*
Copyright (c) 2011+, HL7, Inc
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this 
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice, 
   this list of conditions and the following disclaimer in the documentation 
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to 
   endorse or promote products derived from this software without specific 
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
POSSIBILITY OF SUCH DAMAGE.

*/


public class FormatsGenerator extends BaseGenerator {

  public class StructureDefinitionSorter implements Comparator<StructureDefinition> {

    @Override
    public int compare(StructureDefinition o1, StructureDefinition o2) {
      return o1.getUrl().compareTo(o2.getUrl());
    }

  }

  private class DefinitionsGroup {
    private StringBuilder parser = new StringBuilder();
    private StringBuilder composer = new StringBuilder();
    private StringBuilder impl = new StringBuilder();
    
    public void pline(String line) {
      parser.append(line+"\r\n");
    }
    public void cline(String line) {
      composer.append(line+"\r\n");
    }
    public void iline(String line) {
      impl.append(line+"\r\n");
    }
    
    public void def(String def) {
      pline("{$IFDEF "+def+"}");
      cline("{$IFDEF "+def+"}");
      iline("{$IFDEF "+def+"}");
    }

    public void undef(String def) {
      pline("{$ENDIF "+def+"}");
      cline("{$ENDIF "+def+"}");
      iline("{$ENDIF "+def+"}");
    }
  }
  private DefinitionsGroup xta = new DefinitionsGroup();
  private DefinitionsGroup xtc = new DefinitionsGroup();
  private DefinitionsGroup xra = new DefinitionsGroup();
  private DefinitionsGroup xrc = new DefinitionsGroup();
  private StringBuilder xpr3 = new StringBuilder();
  private StringBuilder xcr3 = new StringBuilder();
  private StringBuilder xpf3 = new StringBuilder();
  private StringBuilder xpt3 = new StringBuilder();
  private StringBuilder xcb3 = new StringBuilder();

  private DefinitionsGroup jta = new DefinitionsGroup();
  private DefinitionsGroup jtc = new DefinitionsGroup();
  private DefinitionsGroup jra = new DefinitionsGroup();
  private DefinitionsGroup jrc = new DefinitionsGroup();
  private StringBuilder jr3 = new StringBuilder();
  private StringBuilder jf3 = new StringBuilder();
  private StringBuilder jt3 = new StringBuilder();
  private StringBuilder jc3 = new StringBuilder();
  private StringBuilder jc4 = new StringBuilder();

  private DefinitionsGroup tta = new DefinitionsGroup();
  private DefinitionsGroup ttc = new DefinitionsGroup();
  private DefinitionsGroup tra = new DefinitionsGroup();
  private DefinitionsGroup trc = new DefinitionsGroup();
  private StringBuilder tpf3 = new StringBuilder();
  private StringBuilder tpt3 = new StringBuilder();
  private StringBuilder tcr3 = new StringBuilder();
  private StringBuilder tpr3 = new StringBuilder();

  private StringBuilder workingParserX;
  private StringBuilder workingParserXA;
  private StringBuilder workingComposerX;
  private StringBuilder workingComposerXA;
  private StringBuilder workingParserJ;
  private StringBuilder workingComposerJ;
  private StringBuilder workingParserT;
  private StringBuilder workingComposerR;
  private List<StructureDefinition> typeList = new ArrayList<StructureDefinition>();

  public FormatsGenerator(Definitions definitions, Configuration configuration, Date genDate, String version) throws UnsupportedEncodingException {
    super(definitions, configuration, version, genDate);
  }

  public void generateXml(String filename) throws Exception {
	  String template = config.getTemplate("FHIR.R5.Xml");
	  template = template.replace("{{mark}}", startVMarkValue());
	  template = template.replace("{{types.abstract.parser.intf}}", xta.parser.toString());
	  template = template.replace("{{types.concrete.parser.intf}}", xtc.parser.toString());
	  template = template.replace("{{resources.abstract.parser.intf}}", xra.parser.toString());
	  template = template.replace("{{resources.concrete.parser.intf}}", xrc.parser.toString());
	  template = template.replace("{{types.abstract.composer.intf}}", xta.composer.toString());
	  template = template.replace("{{types.concrete.composer.intf}}", xtc.composer.toString());
	  template = template.replace("{{resources.abstract.composer.intf}}", xra.composer.toString());
	  template = template.replace("{{resources.concrete.composer.intf}}", xrc.composer.toString());
	  template = template.replace("{{types.abstract.impl}}", xta.impl.toString());
	  template = template.replace("{{types.concrete.impl}}", xtc.impl.toString());
	  template = template.replace("{{resources.abstract.impl}}", xra.impl.toString());
	  template = template.replace("{{resources.concrete.impl}}", xrc.impl.toString());
	  template = template.replace("{{parse.resource}}", xpr3.toString());
	  template = template.replace("{{compose.resource}}", xcr3.toString());
	  template = template.replace("{{parse.fragment}}", xpf3.toString());
	  template = template.replace("{{parse.type}}", xpt3.toString());
	  template = template.replace("{{compose.base}}", xcb3.toString());

	  OutputStreamWriter w = new OutputStreamWriter(new FileOutputStream(filename));
    w.write(template);
		w.flush();
		w.close();
	}
	

  public void generateJson(String filename) throws Exception {
    String template = config.getTemplate("FHIR.R5.Json");
    
    template = template.replace("{{mark}}", startVMarkValue());
    template = template.replace("{{abstract.types.intf.parser}}", jta.parser.toString());
    template = template.replace("{{concrete.types.intf.parser}}", jtc.parser.toString());
    template = template.replace("{{resources.intf.parser}}", jra.parser.toString()+jrc.parser.toString());
    template = template.replace("{{abstract.types.intf.composer}}", jta.composer.toString());
    template = template.replace("{{concrete.types.intf.composer}}", jtc.composer.toString());
    template = template.replace("{{resources.intf.composer}}", jra.composer.toString()+jrc.composer.toString());
    template = template.replace("{{abstract.types.impl}}", jta.impl.toString());
    template = template.replace("{{concrete.types.impl}}", jtc.impl.toString());
    template = template.replace("{{resources.impl}}", jra.impl.toString()+jrc.impl.toString());
    template = template.replace("{{resources.parse}}", jr3.toString());
    template = template.replace("{{fragments.parse}}", jf3.toString());
    template = template.replace("{{types.parse}}", jt3.toString());
    template = template.replace("{{compose.all}}", jc3.toString());
    template = template.replace("{{compose.resource}}", jc4.toString());

    OutputStreamWriter w = new OutputStreamWriter(new FileOutputStream(filename));
    w.write(template);
    w.flush();
    w.close();
  }
  

  public void generateTurtle(String filename) throws Exception {
    String template = config.getTemplate("FHIR.R5.Turtle");
    template = template.replace("{{mark}}", startVMarkValue());
    template = template.replace("{{parse.types.abstract.intf}}", tta.parser.toString());
    template = template.replace("{{parse.types.concrete.intf}}", ttc.parser.toString());
    template = template.replace("{{parse.resources.abstract.intf}}", tra.parser.toString());
    template = template.replace("{{parse.resources.concrete.intf}}", trc.parser.toString());
    template = template.replace("{{compose.types.abstract.intf}}", tta.composer.toString());
    template = template.replace("{{compose.types.concrete.intf}}", ttc.composer.toString());
    template = template.replace("{{compose.resources.abstract.intf}}", tra.composer.toString());
    template = template.replace("{{compose.resources.concrete.intf}}", trc.composer.toString());
    template = template.replace("{{types.abstract.impl}}", tta.impl.toString());
    template = template.replace("{{types.concrete.impl}}", ttc.impl.toString());
    template = template.replace("{{resources.abstract.impl}}", tra.impl.toString());
    template = template.replace("{{resources.concrete.impl}}", trc.impl.toString());
    template = template.replace("{{parser.reg.fragment}}", tpf3.toString());
    template = template.replace("{{parser.reg.type}}", tpt3.toString());
    template = template.replace("{{compose.reg.resource}}", tcr3.toString());
    template = template.replace("{{parser.reg.resource}}", tpr3.toString());

    OutputStreamWriter w = new OutputStreamWriter(new FileOutputStream(filename));
    w.write(template);
    w.flush();
    w.close();
  }
  
  public void seeType(Analysis analysis) throws Exception {
    if (analysis.getName().equals("Base")) {
      return; // we don't generate base.
    }
    typeList.add(analysis.getStructure());
    for (TypeInfo ti : analysis.getTypeList()) {
      if (analysis.isAbstract()) { 
        generateType(analysis, ti, ClassGeneratorCategory.Type, xta, jta, tta);    
      } else {
        generateType(analysis, ti, ClassGeneratorCategory.Type, xtc, jtc, ttc);
      }
    }

    if (analysis.isAbstract()) { 
      generateAbstractType(analysis, analysis.getRootType(), ClassGeneratorCategory.Type, xta, jta, tta);
    } else {
      generateType(analysis, analysis.getRootType(), ClassGeneratorCategory.Type, xtc, jtc, ttc);
      xpf3.append("  else if SameText(element.Name, '"+analysis.getName()+"') then\r\n");
      xpf3.append("    result := parse"+analysis.getName()+"(element, element.Name)\r\n");
      xpt3.append("  else if (type_ = "+analysis.getClassName()+") then\r\n");
      xpt3.append("    result := parse"+analysis.getName()+"(element, name)\r\n");
      xcb3.append("  else if (base is "+analysis.getClassName()+") then\r\n");
      xcb3.append("    compose"+analysis.getName()+"(xml, name,  "+analysis.getClassName()+"(base))\r\n");
      jf3.append("  else if (type_ = '"+analysis.getName()+"') or (type_ = '"+analysis.getClassName()+"')  then\r\n");
      jf3.append("    result := parse"+analysis.getName()+"(jsn)\r\n");
      jt3.append("  else if (type_ = "+analysis.getClassName()+") then\r\n");
      jt3.append("    result := parse"+analysis.getName()+"(jsn)\r\n");
      jc3.append("  else if (base is "+analysis.getClassName()+") then\r\n");
      jc3.append("    compose"+analysis.getName()+"(json, name, "+analysis.getClassName()+"(base), false)\r\n");
      tpf3.append("  else if SameText(type_, '"+analysis.getName()+"') then\r\n");
      tpf3.append("    result := parse"+analysis.getName()+"(obj)\r\n");
      tpt3.append("  else if (type_ = "+analysis.getClassName()+") then\r\n");
      tpt3.append("    result := parse"+analysis.getName()+"(obj)\r\n");
    }        
  }

  public void seeResource(Analysis analysis) throws Exception {
    if (analysis.getName().equals("Base")) {
      return; // we don't generate base.
    }
    if (!analysis.isAbstract()) {
      xrc.def(analysis.getDefineName());
      jrc.def(analysis.getDefineName());
      trc.def(analysis.getDefineName());

      xpr3.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
      xcr3.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
      xpf3.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
      jr3.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
      jf3.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
      jc3.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
      jc4.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
      tpf3.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
      tcr3.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
      tpr3.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
    }
    for (TypeInfo ti : analysis.getTypeList()) {
      if (analysis.isAbstract()) { 
        generateType(analysis, ti, ClassGeneratorCategory.Component, xra, jra, tra);
      } else {
        generateType(analysis, ti, ClassGeneratorCategory.Component, xrc, jrc, trc);    

      }
//        compJBase.append("  else if (base is "+tn+") then\r\n    compose"+tn.substring(5)+"(json, name, "+tn+"(base), false)\r\n");
//        compXBase.append("  else if (base is "+tn+") then\r\n    compose"+tn.substring(5)+"(xml, name,  "+tn+"(base))\r\n");
//        compRBase.append("  else if (base is "+tn+") then\r\n    compose"+tn.substring(5)+"(section, name,  "+tn+"(base), true, -1)\r\n");
    }

    if (analysis.isAbstract()) { 
      generateAbstractType(analysis, analysis.getRootType(), ClassGeneratorCategory.Resource, xra, jra, tra);
    } else {
      generateType(analysis, analysis.getRootType(), ClassGeneratorCategory.Resource, xrc, jrc, trc);
      xpr3.append("  else if element.localName = '"+analysis.getName()+"' Then\r\n");
      xpr3.append("    result := Parse"+analysis.getName()+"(element, path+'/"+analysis.getName()+"')\r\n");
      xcr3.append("    frt"+analysis.getName()+": Compose"+analysis.getName()+"(xml, '"+analysis.getName()+"', "+analysis.getClassName()+"(resource));\r\n");
      xpf3.append("  else if SameText(element.Name, '"+analysis.getName()+"') then\r\n");
      xpf3.append("    result := parse"+analysis.getName()+"(element, element.Name)\r\n");
      xcb3.append("  else if (base is "+analysis.getClassName()+") then\r\n");
      xcb3.append("    compose"+analysis.getName()+"(xml, name,  "+analysis.getClassName()+"(base))\r\n");
      jr3.append("  else if s = '"+analysis.getName()+"' Then\r\n");
      jr3.append("    result := Parse"+analysis.getName()+"(jsn)\r\n");
      jf3.append("  else if (type_ = '"+analysis.getName()+"') or (type_ = '"+analysis.getClassName()+"')  then\r\n");
      jf3.append("    result := parse"+analysis.getName()+"(jsn)\r\n");
      jc3.append("  else if (base is "+analysis.getClassName()+") then\r\n");
      jc3.append("    compose"+analysis.getName()+"(json, name, "+analysis.getClassName()+"(base), false)\r\n");
      jc4.append("    frt"+analysis.getName()+": Compose"+analysis.getName()+"(json, '"+analysis.getName()+"', "+analysis.getClassName()+"(resource));\r\n");
      tpf3.append("  else if SameText(type_, '"+analysis.getName()+"') then\r\n");
      tpf3.append("    result := parse"+analysis.getName()+"(obj)\r\n");
      tcr3.append("    frt"+analysis.getName()+": Compose"+analysis.getName()+"(this, '', '"+analysis.getName()+"', "+analysis.getClassName()+"(resource), true, -1);\r\n");
      tpr3.append("  else if s = '"+analysis.getName()+"' Then\r\n");
      tpr3.append("    result := Parse"+analysis.getName()+"(obj) \r\n");
    }    
    if (!analysis.isAbstract()) {
      xrc.undef(analysis.getDefineName());
      jrc.undef(analysis.getDefineName());
      trc.undef(analysis.getDefineName());
      xpr3.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
      xcr3.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
      xpf3.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
      jr3.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
      jf3.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
      jc3.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
      jc4.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
      tpf3.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
      tcr3.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
      tpr3.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
    }    
  }
	

  private void generateAbstractType(Analysis analysis, TypeInfo ti, ClassGeneratorCategory category, DefinitionsGroup xml, DefinitionsGroup json, DefinitionsGroup ttl) throws Exception {
    String tn = ti.getName();
    
    workingParserX = new StringBuilder();
    workingParserXA = new StringBuilder();
    workingComposerX = new StringBuilder();
    workingComposerXA = new StringBuilder();
    workingParserJ = new StringBuilder();
    workingComposerJ = new StringBuilder();
    workingParserT = new StringBuilder();
    workingComposerR = new StringBuilder();

    for (ElementDefinition c : ti.getChildren()) {
      generateField(analysis, ti, c, category);
    }


    xml.parser.append("    Procedure Parse"+ti.getDefn().getName()+"Attributes(value : "+tn+"; path : string; element : TMXmlElement);\r\n");
    xml.impl.append("Procedure TFHIRXmlParser.Parse"+ti.getDefn().getName()+"Attributes(value : "+tn+"; path : string; element : TMXmlElement);\r\n");
    xml.impl.append("begin\r\n");
//    if (!isBase)
      xml.impl.append("  Parse"+ti.getAncestorName()+"Attributes(value, path, element);\r\n");
//    else
//      xml.impl.append("  GetObjectLocation(value, element);\r\n");      
    xml.impl.append(workingParserXA.toString());
    xml.impl.append("end;\r\n\r\n");

    xml.parser.append("    Function Parse"+ti.getDefn().getName()+"Child(value : "+tn+"; path : string; child : TMXmlElement) : boolean;\r\n");
    xml.impl.append("Function TFHIRXmlParser.Parse"+ti.getDefn().getName()+"Child(value : "+tn+"; path : string; child : TMXmlElement) : boolean;\r\n");
    xml.impl.append("begin\r\n");
    xml.impl.append("  result := true;\r\n");
    if (workingParserX.length() < 11) {
      xml.impl.append("  if not parse"+ti.getAncestorName()+"Child(value, path, child) then\r\n");      
    } else {
      xml.impl.append("  "+workingParserX.toString().substring(11).replace("      ", "  "));
      xml.impl.append("  else if not parse"+ti.getAncestorName()+"Child(value, path, child) then\r\n");
    }
//    if (isBase)
//      xml.impl.append("  else\r\n");
//    else
    xml.impl.append("    result := false;\r\n");
    xml.impl.append("end;\r\n\r\n");

    json.parser.append("    procedure Parse"+ti.getDefn().getName()+"Properties(jsn : TJsonObject; value : "+tn+");\r\n");
    ttl.parser.append("    procedure Parse"+ti.getDefn().getName()+"Properties(obj : TTurtleComplex; value : "+tn+");\r\n");
    json.impl.append("procedure TFHIRJsonParser.Parse"+ti.getDefn().getName()+"Properties(jsn : TJsonObject; value : "+tn+");\r\n");
    json.impl.append("begin\r\n");
//    if (!isBase)
      json.impl.append("  Parse"+ti.getAncestorName()+"Properties(jsn, value); {jp2}\r\n");
//    else {
//      json.impl.append("  value.LocationStart := jsn.LocationStart;\r\n");
//      json.impl.append("  value.LocationEnd := jsn.LocationEnd;\r\n");
//    }

    json.impl.append(workingParserJ.toString().replace("    ", "  "));
    json.impl.append("end;\r\n\r\n");

    ttl.impl.append("procedure TFHIRTurtleParser.Parse"+ti.getDefn().getName()+"Properties(obj : TTurtleComplex; value : "+tn+");\r\n");
    if (workingParserT.toString().contains("for item") || workingParserT.toString().contains("if obj.has(")) {
      ttl.impl.append("var\r\n");
      ttl.impl.append("  item : TTurtleComplex;\r\n");
    }      
    ttl.impl.append("begin\r\n");
//    if (!isBase)
      ttl.impl.append("  Parse"+ti.getAncestorName()+"Properties(obj, value);\r\n");
//    else {
//      ttl.impl.append("  value.LocationStart := obj.Start;\r\n");
//      ttl.impl.append("  value.LocationEnd := obj.Stop;\r\n");
//    }

    ttl.impl.append(workingParserT.toString().replace("    ", "  "));
    ttl.impl.append("end;\r\n\r\n");

    xml.composer.append("    Procedure Compose"+ti.getDefn().getName()+"Attributes(xml : TXmlBuilder; value : "+tn+");\r\n");
    xml.impl.append("Procedure TFHIRXmlComposer.Compose"+ti.getDefn().getName()+"Attributes(xml : TXmlBuilder; value : "+tn+");\r\n");
    xml.impl.append("begin\r\n");
//    if (!isBase)
      xml.impl.append("  Compose"+ti.getAncestorName()+"Attributes(xml, value);\r\n");
    xml.impl.append(workingComposerXA.toString());        
    xml.impl.append("end;\r\n\r\n");
    xml.composer.append("    Procedure Compose"+ti.getDefn().getName()+"Children(xml : TXmlBuilder; value : "+tn+");\r\n");
    xml.impl.append("Procedure TFHIRXmlComposer.Compose"+ti.getDefn().getName()+"Children(xml : TXmlBuilder; value : "+tn+");\r\n");
    if (workingComposerX.toString().contains("i :=")) {
      xml.impl.append("var\r\n");
      xml.impl.append("  i : integer;"+marker()+"\r\n");
    }
    xml.impl.append("begin\r\n");
//    if (!isBase)
      xml.impl.append("  compose"+ti.getAncestorName()+"Children(xml, value);\r\n");
    xml.impl.append(workingComposerX.toString());        
    xml.impl.append("end;\r\n\r\n");

    json.composer.append("    Procedure Compose"+ti.getDefn().getName()+"Properties(json : TJSONWriter; value : "+tn+");\r\n");
    json.impl.append("Procedure TFHIRJsonComposer.Compose"+ti.getDefn().getName()+"Properties(json : TJSONWriter; value : "+tn+");\r\n");
    if (workingComposerJ.toString().contains("i :=")) {
      json.impl.append("var\r\n");
      json.impl.append("  i : integer"+marker()+";\r\n");
    }
    json.impl.append("begin\r\n");
//    if (!isBase)
      json.impl.append("  Compose"+ti.getAncestorName()+"Properties(json, value); "+marker()+"\r\n");
    json.impl.append(workingComposerJ.toString());        
    json.impl.append("end;\r\n\r\n");

    ttl.composer.append("    Procedure Compose"+ti.getDefn().getName()+"(this : TTurtleComplex; parentType, name : String; value : "+tn+"; useType : boolean; index : integer); overload;\r\n");
    ttl.impl.append("Procedure TFHIRTurtleComposer.Compose"+ti.getDefn().getName()+"(this : TTurtleComplex; parentType, name : String; value : "+tn+"; useType : boolean; index : integer);\r\n");
    if (workingComposerR.toString().contains("i :=")) {
      ttl.impl.append("var\r\n");
      ttl.impl.append("  i : integer;"+marker()+"\r\n");
    }
    ttl.impl.append("begin\r\n");
//    if (!isBase)
      ttl.impl.append("  Compose"+ti.getAncestorName()+"(this, '', name, value, false, index);\r\n");
    ttl.impl.append(workingComposerR.toString().replace("%%%%", ti.getName()));        
    ttl.impl.append("end;\r\n\r\n");
    
  }

  protected void generateType(Analysis analysis, TypeInfo ti, ClassGeneratorCategory category, DefinitionsGroup xml, DefinitionsGroup json, DefinitionsGroup ttl) throws Exception {
    String tn = ti.getName();

    xml.parser.append("    function Parse"+tn.substring(5)+"(element : TMXmlElement; path : string) : "+tn+";\r\n");
//    if (!e.getName().equals("Element") && !e.getName().equals("BackboneElement"))
      xml.parser.append("    function Parse"+tn.substring(5)+"Child(value : TFhir"+tn.substring(5)+"; path : string; child : TMXmlElement) : boolean;\r\n");
    xml.composer.append("    procedure Compose"+tn.substring(5)+"(xml : TXmlBuilder; name : string; value : "+tn+");\r\n");
//    if (!e.getName().equals("Element") && !e.getName().equals("BackboneElement"))
      xml.composer.append("    procedure Compose"+tn.substring(5)+"Children(xml : TXmlBuilder; value : "+tn+");\r\n");
    json.parser.append("    function Parse"+tn.substring(5)+"(jsn : TJsonObject) : "+tn+"; overload; {b\\}\r\n");
    ttl.parser.append("    function Parse"+tn.substring(5)+"(obj : TTurtleComplex) : "+tn+"; overload; {b\\}\r\n");
//    if (!e.getName().equals("Element") && !e.getName().equals("BackboneElement")) {
      json.parser.append("    procedure Parse"+tn.substring(5)+"Properties(jsn : TJsonObject; value : "+tn+"); overload; {b\\}\r\n");
      ttl.parser.append("    procedure Parse"+tn.substring(5)+"Properties(obj : TTurtleComplex; value : "+tn+"); overload; {b\\}\r\n");
//   }
    json.composer.append("    procedure Compose"+tn.substring(5)+"(json : TJSONWriter; name : string; value : "+tn+"; noObj : boolean = false);\r\n");
    ttl.composer.append("    procedure Compose"+tn.substring(5)+"(parent :  TTurtleComplex; parentType, name : String; value : "+tn+"; useType : boolean; index : integer);\r\n");
//    if (category == ClassCategory.Component) {
//      jsReg.append("  defineBackboneElementPropsJs(js, def);\r\n");
//    } else {
//      jsReg.append("  defineElementPropsJs(js, def);\r\n");
//    }

    workingParserX = new StringBuilder();
    workingParserXA = new StringBuilder();
    workingComposerX = new StringBuilder();
    workingComposerXA = new StringBuilder();
    workingParserJ = new StringBuilder();
    workingComposerJ = new StringBuilder();
    workingParserT = new StringBuilder();
    workingComposerR = new StringBuilder();

    for (ElementDefinition c : ti.getChildren()) {
      generateField(analysis, ti, c, category);
    }

    generateParser(analysis, ti, tn, category, true, ti.getAncestorName(), xml, json, ttl);
  }

  private void generateField(Analysis analysis, TypeInfo ti, ElementDefinition e, ClassGeneratorCategory category) throws Exception {
    String tn = e.getUserString("pascal.type");
    String parse = null;
    String propV = "F"+getTitle(getElementName(e.getName()));
    if (e.hasUserData("pascal.enum")) {        
      parse = "ParseEnum(CODES_"+tn+", SYSTEMS_"+tn+", child, path+'/"+e.getName()+"')";
    } else if (typeIsSimple(tn)) {
       if (tn.equals("Integer") || tn.equals("UnsignedInt")  || tn.equals("PositiveInt")) {
        parse = "StringToInteger32(child.text)";
        propV = "inttostr("+propV+ ")";
      } else if (tn.equals("Integer64")) {
        parse = "StringToInteger64(child.text)";
        propV = "int64tostr("+propV+ ")";
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
    if (e.hasUserData("pascal.enum")) {        
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
      if (e.hasUserData("pascal.enum")) {        
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
    if (s.equals("total") && e.getPath().startsWith("Bundle"))
      sumSet = "soFull, soSummary, soText, soData, soCount";
    else if (s.equals("id") || e.getPath().startsWith("Bundle") || e.getPath().startsWith("Parameters"))
      sumSet = "soFull, soSummary, soText, soData";
    else if ((s.equals("text") && e.getPath().equals("DomainResource")) || (e.getPath().equals("Narrative")) )
      sumSet = "soFull, soText";
    else if (e.getIsSummary())
      sumSet = "soFull, soSummary, soData";
    else
      sumSet = "soFull, soData";
    String dc = (category == ClassGeneratorCategory.Resource) && !e.getPath().contains(".") && !e.getPath().contains("Bundle") && !s.equals("id") ? " and doCompose('"+s+"')" : "";
    String defaultValueTest = "";

    //    boolean summary = e.isSummary() || noSummaries;
    //    String sumAnd = summary ? "" : "Not SummaryOnly and ";
    //    String sum2 = summary ? "" : "if not SummaryOnly then\r\n    ";
    if (e.unbounded()) {
      if (e.hasUserData("pascal.enum")) {
        String obj = "";
        if (getEnumSize(e) < 32) {
          obj = "List";
        }

        workingParserX.append("      else if (child.localName = '"+e.getName()+"') then\r\n"+
            "        value."+s+obj+".Add("+parse+"){y.1}\r\n");
        workingComposerX.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"for i := 0 to value."+s+obj+".Count - 1 do\r\n"+
            "      ComposeEnum(xml, '"+e.getName()+"', value."+s+obj+"[i], CODES_"+tn+");\r\n");

        workingComposerR.append("  "+(e.getMax().equals("0") ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"for i := 0 to value."+s+obj+".Count - 1 do\r\n"+
              "      ComposeEnum(this, '%%%%', '"+e.getName()+"', value."+s+obj+"[i], CODES_"+tn+", SYSTEMS_"+tn+", false, i); {x.d1}\r\n");

        workingParserJ.append(
            "    if jsn.has('"+e.getName()+"') or jsn.has('_"+e.getName()+"') then\r\n"+
                "      iterateEnumArray(jsn.vArr['"+e.getName()+"'], jsn.vArr['_"+e.getName()+"'], jsn.path+'/"+e.getName()+"', value."+s+obj+", parseEnum, CODES_"+tn+", SYSTEMS_"+tn+");\r\n");

        workingParserT.append(
            "    for item in obj.complexes('http://hl7.org/fhir/"+e.getPath()+"') do\r\n"+
            "      value."+s+obj+".Add(parseEnum(item, CODES_"+tn+", SYSTEMS_"+tn+"));\r\n");

        workingComposerJ.append("  if "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" and ":"")+"(value."+s+obj+".Count > 0) then\r\n");
        workingComposerJ.append(
            "  begin\r\n"+
                "    val := false;\r\n"+    
                "    ext := false;\r\n"+    
                "    for i := 0 to value."+s+obj+".Count - 1 do\r\n"+
                "    begin\r\n"+
                "      val := val or (value."+s+obj+"[i].hasPrimitiveValue);\r\n"+
                "      ext := ext or ((value."+s+obj+"[i].id <> '') or (value."+s+obj+"[i].hasExtensionList));\r\n"+
                "    end;\r\n"+
                "    if val then\r\n"+
                "    begin\r\n"+
                "      json.valueArray('"+e.getName()+"');\r\n"+
                "      for i := 0 to value."+s+obj+".Count - 1 do\r\n"+
                "        ComposeEnumValue(json, '', value."+s+obj+"[i], CODES_"+tn+", true);\r\n"+
                "      json.FinishArray;\r\n"+
                "    end;\r\n"+
                "    if ext then\r\n"+
                "    begin\r\n"+
                "      json.valueArray('_"+e.getName()+"');\r\n"+
                "      for i := 0 to value."+s+obj+".Count - 1 do\r\n"+
                "        ComposeEnumProps(json, '', value."+s+obj+"[i], CODES_"+tn+", true);\r\n"+
                "      json.FinishArray;\r\n"+
                "    end;\r\n"+
            "  end;\r\n");

          workingComposerR.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"for i := 0 to value."+s+obj+".Count - 1 do\r\n"+
              "      ComposeEnum(this, '%%%%', '"+e.getName()+"', value."+s+obj+"[i], CODES_"+tn+", SYSTEMS_"+tn+", false, i);"+marker()+"\r\n");
      } else {
        String tnl;
        if (tn.contains("{"))
          tnl = listForm(tn.substring(0, tn.indexOf('{')))+tn.substring(tn.indexOf('{'));
        else
          tnl = listForm(tn);
        s = s+"List";
        if (!typeIsSimple(tn)) {
          if (!e.getName().equals("[type]") && !e.getName().contains("[x]")) {
            parse = "Parse"+parseName(tn)+"(child, path+'/"+e.getName()+"')";
            if (!typeIsPrimitive(e.typeSummary()))
              parseJ1 = "Parse"+parseName(tn)+"(path+'."+e.getName()+"')";
            srlsd = "Compose"+parseName(tn);
            srlsdJ = "Compose"+parseName(tn);
            srlsr = "Compose"+parseNameR(tn);
          } else {
            throw new Exception("not supported at "+e.getName()+" - complex type "+tn);
          }
        };
        workingParserX.append("      else if (child.localName = '"+e.getName()+"') then\r\n"+
            "        value."+s+".Add("+parse+")"+marker()+"\r\n");
        workingComposerX.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"for i := 0 to value."+s+".Count - 1 do\r\n"+
            "      "+srlsd+"(xml, '"+e.getName()+"', "+getParam3(tn)+srls.replace("#", "value."+s+"[i]")+");\r\n");
        if (srlsr.equals("ComposeResource")) 
          workingComposerR.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"for i := 0 to value."+s+".Count - 1 do\r\n"+
              "      ComposeInnerResource(this, '%%%%', '"+e.getName()+"', "+srls.replace("#", "value."+s+"[i]")+", false, i);"+marker()+"\r\n");
        else
          workingComposerR.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"for i := 0 to value."+s+".Count - 1 do\r\n"+
              "      "+srlsr+"(this, '%%%%', '"+e.getName()+"', "+srls.replace("#", "value."+s+"[i]")+", false, i);"+marker()+"\r\n");
        if (typeIsPrimitive(e.typeSummary())) { 
          workingParserJ.append(
              "      if jsn.has('"+e.getName()+"') or jsn.has('_"+e.getName()+"') then\r\n"+
                  "      iteratePrimitiveArray(jsn.vArr['"+e.getName()+"'], jsn.vArr['_"+e.getName()+"'], value."+s+", parse"+parseName(tn)+");\r\n");
        } else { 
          workingParserJ.append("    if jsn.has('"+e.getName()+"') then\r\n"+
              "      iterateArray(jsn.vArr['"+e.getName()+"'], value."+s+", parse"+parseName(tn)+");\r\n");
        }
        workingParserT.append("    for item in obj.complexes('http://hl7.org/fhir/"+e.getPath()+"') do\r\n"+
            "      value."+s+".Add(parse"+parseName(tn)+"(item));\r\n");

        workingComposerJ.append("  if "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" and ":"")+"(value."+s+".Count > 0) then\r\n");
        if (typeIsPrimitive(e.typeSummary())) 
          workingComposerJ.append(
              "  begin\r\n"+
                  "    ext := false;\r\n"+    
                  "    val := false;\r\n"+    
                  "    for i := 0 to value."+s+".Count - 1 do\r\n"+
                  "    begin\r\n"+
                  "      ext := ext or ((value."+s+"[i].id <> '') or (value."+s+"[i].hasExtensionList) {no-comments or (value."+s+"[i].hasComments)});\r\n"+
                  "      val := val or (value."+s+"[i].hasPrimitiveValue);\r\n"+
                  "    end;\r\n"+
                  "    if val then\r\n"+
                  "    begin\r\n"+
                  "      json.valueArray('"+e.getName()+"');\r\n"+
                  "      for i := 0 to value."+s+".Count - 1 do\r\n"+
                  "        "+srlsdJ+"Value(json, '',"+srls.replace("#", "value."+s+"[i]")+", true);\r\n"+
                  "      json.FinishArray;\r\n"+
                  "    end;\r\n"+
                  "    if ext then\r\n"+
                  "    begin\r\n"+
                  "      json.valueArray('_"+e.getName()+"');\r\n"+
                  "      for i := 0 to value."+s+".Count - 1 do\r\n"+
                  "        "+srlsdJ+"Props(json, '',"+srls.replace("#", "value."+s+"[i]")+", true);\r\n"+
                  "      json.FinishArray;\r\n"+
                  "    end;\r\n"+
              "  end;\r\n");
        else
          workingComposerJ.append(
              "  begin\r\n"+
                  "    json.valueArray('"+e.getName()+"');\r\n"+
                  "    for i := 0 to value."+s+".Count - 1 do\r\n"+
                  "      "+srlsdJ+"(json, '', "+getParam3(tn)+srls.replace("#", "value."+s+"[i]")+");"+marker()+"\r\n"+
                  "    json.FinishArray;\r\n"+
              "  end;\r\n");
      }
    } else {
      if (typeIsSimple(tn) && !tn.equals("TFhirXHtmlNode")) {
        if (e.hasRepresentation(PropertyRepresentation.XMLATTR))
          workingParserXA.append("    value."+s+"ST := fix me (and compose)! GetAttribute(element, '"+e.getName()+"');\r\n");
        else  
          workingParserX.append("      else if (child.localName = '"+e.getName()+"') then\r\n        value."+s+"Element := "+parse+""+marker()+"\r\n");
        workingParserJ.append("    if jsn.has('"+e.getName()+"') or jsn.has('_"+e.getName()+"')  then\r\n"+
            "      value."+s+"Element := parseEnum(jsn.path+'/"+e.getName()+"', jsn.node['"+e.getName()+"'], jsn.vObj['_"+e.getName()+"'], CODES_"+tn+", SYSTEMS_"+tn+");\r\n");
        workingParserT.append("    value."+s+"Element := ParseEnum(obj.complex('http://hl7.org/fhir/"+e.getPath()+"'), CODES_"+tn+", SYSTEMS_"+tn+");\r\n");
        if (e.unbounded()) {
          workingComposerX.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"ComposeEnum(xml, '"+e.getName()+"', value."+getTitle(s)+"Element, CODES_"+tn+");\r\n");
          workingComposerR.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"ComposeEnum(this, '%%%%', '"+e.getName()+"', value."+getTitle(s)+"Element, CODES_"+tn+", SYSTEMS_"+tn+", false, -1);"+marker()+"\r\n");
          workingComposerJ.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"ComposeEnumValue(json, '"+e.getName()+"', value."+getTitle(s)+"Element, CODES_"+tn+", false);\r\n");
          workingComposerJ.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"ComposeEnumProps(json, '"+e.getName()+"', value."+getTitle(s)+"Element, CODES_"+tn+", false);\r\n");
        } else {
          workingComposerX.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"Compose"+tn+"(xml, '"+e.getName()+"', value."+getTitle(s)+");"+marker()+"\r\n");
          workingComposerR.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"Compose"+tn+"(this, '%%%%', '"+e.getName()+"', value."+getTitle(s)+", false, -1);"+marker()+"\r\n");
          workingComposerJ.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"Compose"+tn+"Value(json, '"+e.getName()+"', value."+getTitle(s)+", false);"+marker()+"\r\n");        
          workingComposerJ.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"Compose"+tn+"Props(json, '"+e.getName()+"', value."+getTitle(s)+", false);"+marker()+"\r\n");        
        }
      } else {
        if (e.getName().contains("[x]") && e.getType().size() > 1) {
          String pfx = e.getName().replace("[x]", "");
          int t = e.getType().size();
          TypeRefComponent lt = lastTypeNotDerived(e.getType());
          int i = 0;
          // the order here is important - derived types have to be done first
          for (TypeRefComponent td : e.getType()) {
            if (isDerivedType(td)) {
              boolean last = i == t - 1;
              genTypeSerialisers(e, s, sumSet, pfx, i == 0, last, td);
              i++;
            }
          }
          for (TypeRefComponent td : e.getType()) {
            if (!isDerivedType(td)) {
              boolean last = i == t - 1 || td == lt;
              genTypeSerialisers(e, s, sumSet, pfx, i == 0, last, td);
              i++;
            }
          }

        } else if (!e.getName().equals("[type]") && !e.getName().contains("[x]")) {
          if (e.hasRepresentation(PropertyRepresentation.XMLATTR)) {
            workingParserXA.append("    "+(!analysis.isAbstract() ? "result" : "value")+"."+s+" := GetAttribute(element, '"+e.getName()+"');"+marker()+"\r\n");
            workingComposerXA.append("  Attribute(xml, '"+e.getName()+"', value."+s+"  );\r\n");
          } else {
            if (e.hasUserData("pascal.enum")) {
              workingParserX.append("      else if (child.localName = '"+e.getName()+"') then\r\n        value."+s+"Element := ParseEnum(CODES_"+tn+", SYSTEMS_"+tn+", child, path+'/"+e.getName()+"')"+marker()+"\r\n");
              workingComposerX.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"ComposeEnum(xml, '"+e.getName()+"', value."+s+"Element, CODES_"+tn+");"+marker()+"\r\n");              
            } else if (typeIsPrimitive(e.typeSummary())) { 
              workingParserX.append("      else if (child.localName = '"+e.getName()+"') then\r\n        value."+s+"Element := Parse"+parseName(tn)+"(child, path+'/"+e.getName()+"')"+marker()+"\r\n");
              workingComposerX.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"Compose"+parseName(tn)+"(xml, '"+e.getName()+"', value."+s+"Element);"+marker()+"\r\n");
            } else {
              workingParserX.append("      else if (child.localName = '"+e.getName()+"') then\r\n        value."+s+" := Parse"+parseName(tn)+"(child, path+'/"+e.getName()+"')"+marker()+"\r\n");
              workingComposerX.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"Compose"+parseName(tn)+"(xml, '"+e.getName()+"', "+getParam3(tn)+"value."+s+");"+marker()+"\r\n");
            }
          }
          if (e.hasUserData("pascal.enum")) {
            workingParserJ.append("    if jsn.has('"+e.getName()+"') or jsn.has('_"+e.getName()+"') then\r\n        value."+s+"Element := parseEnum(jsn.path+'/"+e.getName()+"', jsn.node['"+e.getName()+"'], jsn.vObj['_"+e.getName()+"'], CODES_"+tn+", SYSTEMS_"+tn+");"+marker()+"\r\n");
            workingParserT.append("    value."+s+"Element := ParseEnum(obj.complex('http://hl7.org/fhir/"+e.getPath()+"'), CODES_"+tn+", SYSTEMS_"+tn+");"+marker()+"\r\n");
            workingComposerR.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"ComposeEnum(this, '%%%%', '"+e.getName()+"', value."+s+"Element, CODES_"+tn+", SYSTEMS_"+tn+", false, -1);"+marker()+"\r\n");
          } else if (typeIsPrimitive(e.typeSummary())) {
            workingParserJ.append("    if jsn.has('"+e.getName()+"') or jsn.has('_"+e.getName()+"') then\r\n        value."+s+"Element := parse"+parseName(tn)+"(jsn.node['"+e.getName()+"'], jsn.vObj['_"+e.getName()+"']);"+marker()+"\r\n");
            workingParserT.append("    value."+s+"Element := Parse"+parseName(tn)+"(obj.complex('http://hl7.org/fhir/"+e.getPath()+"'));"+marker()+"\r\n");
            workingComposerR.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"Compose"+parseNameR(tn)+"(this, '%%%%', '"+e.getName()+"', value."+s+"Element, false, -1);"+marker()+"\r\n");
          } else if (e.typeSummary().equals("xhtml")) {
            workingParserJ.append("    if jsn.has('"+e.getName()+"') then\r\n        value."+s+" := parse"+parseName(tn)+"(jsn.path+'.div', jsn.node['"+e.getName()+"']);"+marker()+"\r\n");
            workingParserT.append("    value."+s+" := Parse"+parseName(tn)+"(obj.stringLiteral('http://hl7.org/fhir/"+e.getPath()+"'));"+marker()+"\r\n");
            workingComposerR.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"Compose"+parseNameR(tn)+"(this, '%%%%', '"+e.getName()+"', value."+s+"Element, false, -1);"+marker()+"\r\n");
          } else {
            workingParserJ.append("    if jsn.has('"+e.getName()+"') then\r\n        value."+s+" := Parse"+parseName(tn)+"(jsn.vObj['"+e.getName()+"']);"+marker()+"\r\n");
            if (tn.equals("TFhirResource")) {
              workingParserT.append("    value."+s+" := Parse"+parseName(tn)+"(obj.predicate('http://hl7.org/fhir/"+e.getPath()+"'));"+marker()+"\r\n");
              workingComposerR.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"ComposeInnerResource(this, '%%%%', '"+e.getName()+"', value."+s+"Element, false, -1);"+marker()+"\r\n");
            } else {
              workingParserT.append("    value."+s+" := Parse"+parseName(tn)+"(obj.complex('http://hl7.org/fhir/"+e.getPath()+"'));"+marker()+"\r\n");
              workingComposerR.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"Compose"+parseNameR(tn)+"(this, '%%%%', '"+e.getName()+"', value."+s+"Element, false, -1);"+marker()+"\r\n");
            }
          }
          if (e.hasUserData("pascal.enum")) {
            workingComposerJ.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"ComposeEnumValue(json, '"+e.getName()+"', value."+s+"Element, CODES_"+tn+", false);"+marker()+"\r\n");
          } else if (typeIsPrimitive(e.typeSummary())) {
            workingComposerJ.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"Compose"+parseName(tn)+"Value(json, '"+e.getName()+"', value."+s+"Element, false);"+marker()+"\r\n");
            workingComposerJ.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"Compose"+parseName(tn)+"Props(json, '"+e.getName()+"', value."+s+"Element, false);"+marker()+"\r\n");
          } else
            workingComposerJ.append("  "+(e.getMin() == 0 ? "if (SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" then\r\n    ":"")+"Compose"+parseName(tn)+"(json, '"+e.getName()+"', "+getParam3(tn)+"value."+s+");"+marker()+"\r\n");
          
        } else {
          String pfx = e.getName().contains("[x]") ? e.getName().replace("[x]", "") : "";
          int i = 0;
          for (StructureDefinition sd : getPrimitives()) {
            if (!sd.getBaseDefinition().equals("http://hl7.org/fhir/StructureDefinition/PrimitiveType")) {
              generatePrimitiveTypesSerialiser(e, s, sumSet, pfx, i, sd);
              i++;
            }
          }
          for (StructureDefinition sd : getPrimitives()) {
            if (sd.getBaseDefinition().equals("http://hl7.org/fhir/StructureDefinition/PrimitiveType")) {
              generatePrimitiveTypesSerialiser(e, s, sumSet, pfx, i, sd);
              i++;
            }
          }
          for (StructureDefinition sd : typeList ) {
            workingParserX.append("      else if (child.localName = '"+pfx+getTitle(sd.getName())+"') then\r\n        value."+s+" := Parse"+getTitle(sd.getName())+"(child, path+'."+pfx+getTitle(sd.getName())+"')"+marker()+"\r\n");
            workingComposerX.append("  else if "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"]) "+defaultValueTest+dc+" and ":"")+"(value."+s+" is TFhir"+getTitle(sd.getName())+") {8} then\r\n    Compose"+getTitle(sd.getName())+"(xml, '"+pfx+getTitle(sd.getName())+"', TFhir"+getTitle(sd.getName())+"(value."+s+"))\r\n");
            workingComposerR.append("  else if "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"]) "+defaultValueTest+dc+" and ":"")+"(value."+s+" is TFhir"+getTitle(sd.getName())+") {8} then\r\n    Compose"+getTitle(sd.getName())+"(this, '%%%%', '"+pfx+getTitle(sd.getName())+"', TFhir"+getTitle(sd.getName())+"(value."+s+"), false, -1)"+marker()+"\r\n");
            workingParserJ.append("    if jsn.has('"+pfx+getTitle(sd.getName())+"') {a7} then\r\n        value."+s+" := Parse"+getTitle(sd.getName())+"(jsn.vObj['"+pfx+getTitle(sd.getName())+"']);\r\n");
            workingParserT.append("    if obj.has('"+pfx+getTitle(sd.getName())+"') {a7} then\r\n        value."+s+" := Parse"+getTitle(sd.getName())+"(jsn.vObj['"+pfx+getTitle(sd.getName())+"']);\r\n");
            workingComposerJ.append("  else if "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"]) "+defaultValueTest+dc+" and ":"")+"(value."+s+" is TFhir"+getTitle(sd.getName())+") then\r\n    Compose"+getTitle(sd.getName())+"(json, '"+pfx+getTitle(sd.getName())+"', TFhir"+getTitle(sd.getName())+"(value."+s+"))\r\n");
          }
          int t = typeList.size();
          i = 0;
          for (StructureDefinition sd : typeList ) {
            workingParserX.append("      else if (child.localName = '"+pfx+getTitle(sd.getName())+"') then\r\n        value."+s+" := Parse"+getTitle(sd.getName())+"(child, path+'/"+pfx+getTitle(sd.getName())+"')"+marker()+"\r\n");
            workingComposerX.append("  else if "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"]) "+defaultValueTest+dc+" and ":"")+"(value."+s+" is TFhir"+getTitle(sd.getName())+") {9} then\r\n    Compose"+getTitle(sd.getName())+"(xml, '"+pfx+getTitle(sd.getName())+"', TFhir"+getTitle(sd.getName())+"(value."+s+"))"+(i < t-1 ? "" : ";")+"\r\n");
            workingComposerR.append("  else if "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"]) "+defaultValueTest+dc+" and ":"")+"(value."+s+" is TFhir"+getTitle(sd.getName())+") {9} then\r\n    Compose"+getTitle(sd.getName())+"(this, '%%%%', '"+pfx+getTitle(sd.getName())+"', TFhir"+getTitle(sd.getName())+"(value."+s+"), true, -1)"+(i < t-1 ? "" : ";")+""+marker()+"\r\n");
            workingParserJ.append("    if jsn.has('"+pfx+getTitle(sd.getName())+"') {a9} then\r\n        value."+s+" := Parse"+getTitle(sd.getName())+"(jsn.vObj['"+pfx+getTitle(sd.getName())+"']);\r\n");
            workingParserT.append("//t.5    if jsn.has('"+pfx+getTitle(sd.getName())+"') {a9} then\r\n        value."+s+" := Parse"+getTitle(sd.getName())+"(jsn.vObj['"+pfx+getTitle(sd.getName())+"']);\r\n");
            workingComposerJ.append("  else if "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"]) "+defaultValueTest+dc+" and ":"")+"(value."+s+" is TFhir"+getTitle(sd.getName())+") then\r\n    Compose"+getTitle(sd.getName())+"(json, '"+pfx+getTitle(sd.getName())+"', TFhir"+getTitle(sd.getName())+"(value."+s+"))"+(i < t-1 ? "" : ";")+"\r\n");
            i++;
          }
        }
      }
    }
  }
  
  private void generatePrimitiveTypesSerialiser(ElementDefinition e, String s, String sumSet, String pfx, int i, StructureDefinition sd) {
    String dc = ""; // path.contains(".") ? "" : "and doCompose(value) ";
    String defaultValueTest = "";
    workingParserX.append("      else if (child.localName = '"+pfx+getTitle(sd.getName())+"') then\r\n        value."+s+" := Parse"+getTitle(sd.getName())+"(child, path+'."+pfx+getTitle(sd.getName())+"')"+marker()+"\r\n");
    String ptn = "TFhir"+getTitle(sd.getName());
    workingComposerX.append("  "+(i > 0 ? "else " : "")+"if "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" and ":"")+"(value."+s+" is "+ptn+") {1} then\r\n    Compose"+ptn.substring(5)+"(xml, '"+pfx+getTitle(sd.getName())+"', "+ptn+"(value."+s+"))\r\n");
    workingComposerR.append("  "+(i > 0 ? "else " : "")+"if "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" and ":"")+"(value."+s+" is "+ptn+") {1} then\r\n    Compose"+ptn.substring(5)+"(this, '%%%%', '"+pfx+getTitle(sd.getName())+"', "+ptn+"(value."+s+"), false, -1)"+marker()+"\r\n");
    workingParserJ.append("    if jsn.has('"+pfx+getTitle(sd.getName())+"') or jsn.has('_"+pfx+getTitle(sd.getName())+"') then\r\n        value."+s+" := parse"+getTitle(sd.getName())+"(jsn.node['"+pfx+getTitle(sd.getName())+"'], jsn.vObj['_"+pfx+getTitle(sd.getName())+"']);\r\n");
    workingParserT.append("//t.6    if jsn.has('"+pfx+getTitle(sd.getName())+"') or jsn.has('_"+pfx+getTitle(sd.getName())+"') then\r\n        value."+s+" := parse"+getTitle(sd.getName())+"(jsn.node['"+pfx+getTitle(sd.getName())+"'], jsn.vObj['_"+pfx+getTitle(sd.getName())+"']);\r\n");
    workingComposerJ.append("  "+(i > 0 ? "else " : "")+"if "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" and ":"")+"(value."+s+" is "+ptn+") then\r\n"+
        "  begin\r\n"+
        "    Compose"+ptn.substring(5)+"Value(json, '"+pfx+getTitle(sd.getName())+"', "+ptn+"(value."+s+"), false);\r\n"+
        "    Compose"+ptn.substring(5)+"Props(json, '"+pfx+getTitle(sd.getName())+"', "+ptn+"(value."+s+"), false)\r\n"+
        "  end\r\n");
  }


  private List<StructureDefinition> getPrimitives() {
    List<StructureDefinition> value = new ArrayList<StructureDefinition>();
    for (StructureDefinition sd : definitions.getStructures().getList()) {
      if (sd.getKind() == StructureDefinitionKind.PRIMITIVETYPE) {
        value.add(sd);
      }
    }
    Collections.sort(value, new StructureDefinitionSorter());
    return value;
  }

  private String parseName(String tn) {
    if (tn.equals("TFhirResource"))
      return "InnerResource";
    else if (tn.equals(""))
      return "??";
    else
      return tn.startsWith("TFhir") ? tn.substring(5) : tn.substring(1);
  }

  private String parseNameR(String tn) {
    if (tn.equals(""))
      return "??";
    else
      return tn.startsWith("TFhir") ? tn.substring(5) : tn.substring(1);
  }
  
  private String getParam3(String tn) {
    if (tn.equals("TFhirResource"))  
      return "value, ";
    else
      return "";
  }

  private TypeRefComponent lastTypeNotDerived(List<TypeRefComponent> list) {
    TypeRefComponent result = null;
    for (int i = 0; i < list.size(); i++)
      if (!isDerivedType(list.get(i)))
        result = list.get(i);
    return result;
  }

  private boolean isDerivedType(TypeRefComponent td) {
    StructureDefinition sd = definitions.getType(td.getWorkingCode());
    return !sd.getBaseDefinition().equals("http://hl7.org/fhir/StructureDefinition/PrimitiveType"); 
  }

  private void genTypeSerialisers(ElementDefinition e, String s, String sumSet, String pfx, boolean first, boolean last, TypeRefComponent td) throws Exception {
    String dc = ""; // path.contains(".") ? "" : "and doCompose(value) ";
    String defaultValueTest = "";
    if (td.isResourceReference()) {
      workingParserX.append("      else if (child.localName = '"+pfx+"Reference') then\r\n        value."+s+" := ParseReference(child, path+'/"+pfx+"Reference')"+marker()+"\r\n");
      workingComposerX.append("  "+(first ? "if" : "else if")+" "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" and ":"")+"(value."+s+" is TFhirReference"+") {2} then\r\n    ComposeReference(xml, '"+pfx+"Reference', TFhirReference"+"(value."+s+"))"+(last?";" : "")+"\r\n");
      workingParserJ.append("    if jsn.has('"+pfx+"Reference') {a3} then\r\n      value."+s+" := ParseReference(jsn.vObj['"+pfx+"Reference']);\r\n");
      workingParserT.append("    if obj.has('"+pfx+"Reference', item) {a3} then\r\n      value."+s+" := ParseReference(item);\r\n");
      workingComposerJ.append("  "+(first ? "if" : "else if")+" "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" and ":"")+"(value."+s+" is TFhirReference"+") then\r\n    ComposeReference(json, '"+pfx+"Reference', TFhirReference(value."+s+"))"+(last?";" : "")+"\r\n");
      workingComposerR.append("  "+(first ? "if" : "else if")+" "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" and ":"")+"(value."+s+" is TFhirReference"+") {2} then\r\n    ComposeReference(this, '%%%%', '"+pfx+"Reference', TFhirReference"+"(value."+s+"), false,-1)"+(last?";" : "")+""+marker()+"\r\n");
    }
    else {
      String tname = td.getName();
      String tpname = tname; 
      workingParserX.append("      else if (child.localName = '"+pfx+getTitle(tpname)+"') then\r\n        value."+s+" := Parse"+getTitle(tname)+"(child, path+'/"+pfx+getTitle(tname)+"')"+marker()+"\r\n");
      workingComposerX.append("  "+(first ? "if" : "else if")+" "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" and ":"")+"(value."+s+" is "+getTypeName(tname)+") {6} then\r\n    Compose"+getTitle(tname)+"(xml, '"+pfx+getTitle(tpname)+"', "+getTypeName(tname)+"(value."+s+"))"+(last?";" : "")+"\r\n");
      if (typeIsPrimitive(tname)) {
        workingComposerJ.append("  "+(first ? "if" : "else if")+" "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" and ":"")+"(value."+s+" is "+getTypeName(tname)+") then \r\n"+
            "  begin\r\n"+
            "    Compose"+getTitle(tname)+"Value(json, '"+pfx+getTitle(tpname)+"', "+getTypeName(tname)+"(value."+s+"), false);\r\n"+
            "    Compose"+getTitle(tname)+"Props(json, '"+pfx+getTitle(tpname)+"', "+getTypeName(tname)+"(value."+s+"), false);\r\n  end"+(last?";" : "")+"\r\n");
        workingParserJ.append("    if jsn.has('"+pfx+getTitle(tpname)+"') or jsn.has('_"+pfx+getTitle(tname)+"') then\r\n      value."+s+" := parse"+Utilities.capitalize(tname)+"(jsn.node['"+pfx+getTitle(tpname)+"'], jsn.vObj['_"+pfx+getTitle(tpname)+"']);\r\n");
      } else {
        workingComposerJ.append("  "+(first ? "if" : "else if")+" "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" and ":"")+"(value."+s+" is "+getTypeName(tname)+") then \r\n"+
            "    Compose"+getTitle(tname)+"(json, '"+pfx+getTitle(tpname)+"', "+getTypeName(tname)+"(value."+s+")) "+(last?";" : "")+"\r\n");
        workingParserJ.append("    if jsn.has('"+pfx+getTitle(tpname)+"') {a4} then\r\n      value."+s+" := Parse"+getTitle(tname)+"(jsn.vObj['"+pfx+getTitle(tpname)+"']);\r\n");
      }
      workingParserT.append("    if obj.has('"+pfx+getTitle(tpname)+"', item) then\r\n      value."+s+" := parse"+Utilities.capitalize(tname)+"(item);\r\n");
      workingComposerR.append("  "+(first ? "if" : "else if")+" "+(e.getMin() == 0 ? "(SummaryOption in ["+sumSet+"])"+defaultValueTest+dc+" and ":"")+"(value."+s+" is "+getTypeName(tname)+") {6} then\r\n    Compose"+getTitle(tname)+"(this, '%%%%', '"+pfx+getTitle(tpname)+"', "+getTypeName(tname)+"(value."+s+"), false, -1)"+(last?";" : "")+""+marker()+"\r\n");
    }
  }

  private void generateParser(Analysis analysis, TypeInfo ti, String tn, ClassGeneratorCategory category, boolean isElement, String parent, DefinitionsGroup xml, DefinitionsGroup json, DefinitionsGroup ttl) throws Exception {
    String s = workingParserX.toString();
    xml.impl.append(
        "function TFHIRXmlParser"+".Parse"+tn.substring(5)+"(element : TMXmlElement; path : string) : "+tn+";\r\n"+
            "var\r\n"+
        "  child : TMXmlElement;\r\n");

    xml.impl.append(
        "begin\r\n"+
            "  result := "+tn+".create;\r\n"+
        "  try\r\n");
    if (isElement)
      if (category == ClassGeneratorCategory.Resource)
        xml.impl.append("    parse"+parent+"Attributes(result, path, element);\r\n");
      else
        xml.impl.append("    parseElementAttributes(result, path, element);\r\n");

    xml.impl.append(workingParserXA.toString());

    xml.impl.append(
        "    child := FirstChild(element);\r\n"+
            "    while (child <> nil) do\r\n"+
            "    begin\r\n"+
            "      if not Parse"+tn.substring(5)+"Child(result, path, child) then\r\n"+
            "        UnknownContent(child, path);\r\n"+
            "      child := NextSibling(child);\r\n"+
        "    end;\r\n");
    if (isElement)
      xml.impl.append(
          "    closeOutElement(result, element);\r\n");
    xml.impl.append(
        "\r\n"+
            "    result.link;\r\n"+
            "  finally\r\n"+
            "    result.free;\r\n"+
            "  end;\r\n"+
            "end;\r\n\r\n"
        );
    if (!tn.equals("TFhirElement") && !tn.equals("TFhirBackboneElement")) {
      xml.impl.append(
          "function TFHIRXmlParser"+".Parse"+tn.substring(5)+"Child(value : "+tn+"; path : string; child : TMXmlElement) : boolean;\r\n"+
              "begin\r\n"+
          "  result := true;\r\n");

      if (s.length() >= 11)
        xml.impl.append("      "+s.substring(11)+"      else ");
      else
        xml.impl.append("      ");

      if (!isElement)
        xml.impl.append("\r\n");
      else if (category == ClassGeneratorCategory.Resource)
        xml.impl.append("if Not Parse"+parent+"Child(value, path, child) then\r\n");
      else if (category == ClassGeneratorCategory.Component)
        xml.impl.append("if Not ParseBackboneElementChild(value, path, child) then\r\n");
//      else if (hasExplicitParent(e))
//        xml.impl.append("if Not Parse"+e.typeCode()+"Child(value, path, child) then\r\n");
      else
        xml.impl.append("if Not ParseElementChild(value, path, child) then\r\n");
      xml.impl.append("    result := false;\r\n");
      xml.impl.append("end;\r\n\r\n");
    }
    s = workingComposerX.toString();
    xml.impl.append(
        "procedure TFHIRXmlComposer"+".Compose"+tn.substring(5)+"(xml : TXmlBuilder; name : String; value : "+tn+");\r\n");
    xml.impl.append(
        "begin\r\n"+
        "  if (value = nil) then\r\n    exit;\r\n");
    if (isElement)
      if (category == ClassGeneratorCategory.Resource)
        xml.impl.append("  compose"+parent+"Attributes(xml, value);\r\n");
      else 
        xml.impl.append("  composeElementAttributes(xml, value);\r\n");
    xml.impl.append(workingComposerXA.toString());        
    xml.impl.append(
        "  xml.open(name);\r\n");
    xml.impl.append("  compose"+tn.substring(5)+"Children(xml, value);\r\n");
    if (isElement)
      xml.impl.append("  closeOutElement(xml, value);\r\n");
    xml.impl.append(
        "  xml.close(name);\r\n"+
            "end;\r\n\r\n"
        );

    if (!tn.equals("TFhirElement") && !tn.equals("TFhirBackboneElement")) {
      xml.impl.append(
          "procedure TFHIRXmlComposer"+".Compose"+tn.substring(5)+"Children(xml : TXmlBuilder; value : "+tn+");\r\n");
      boolean var = false;
      if (s.contains("for i := ")) {
        xml.impl.append("var\r\n  i : integer;\r\n");
        var = true;
      }
      if (s.contains("ext := ")) {
        if (!var) 
          xml.impl.append("var\r\n");
        xml.impl.append("  ext : boolean;\r\n");
        xml.impl.append("  val : boolean;\r\n");
      }
      xml.impl.append(
          "begin\r\n");
      if (isElement)
//        if (category == ClassGeneratorCategory.Resource)
          xml.impl.append("  compose"+parent+"Children(xml, value);\r\n");
//        else if (category == ClassGeneratorCategory.Component)
//          xml.impl.append("  composeBackboneElementChildren(xml, value);\r\n");
//        else if (Utilities.noString(parent))
//          xml.impl.append("  composeElementChildren(xml, value);\r\n");
//        else
//          xml.impl.append("  compose"+parent+"Children(xml, value);\r\n");

      xml.impl.append(s);
      xml.impl.append(
          "end;\r\n\r\n"
          );
    }

    json.parser.append("    procedure Parse"+tn.substring(5)+"(jsn : TJsonObject; ctxt : "+listForm("TFHIRObject")+"); overload; "+marker()+"\r\n");
    json.impl.append("procedure TFHIRJsonParser"+".Parse"+tn.substring(5)+"(jsn : TJsonObject; ctxt : "+listForm("TFHIRObject")+");\r\n");
    json.impl.append("begin\r\n");
    json.impl.append("  ctxt.add(Parse"+tn.substring(5)+"(jsn)); "+marker()+"\r\n");
    json.impl.append("end;\r\n\r\n");

    json.impl.append(
        "function TFHIRJsonParser"+".Parse"+tn.substring(5)+"(jsn : TJsonObject) : "+tn+";\r\n"+
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
    ttl.impl.append(
        "function TFHIRTurtleParser"+".Parse"+tn.substring(5)+"(obj : TTurtleComplex) : "+tn+";\r\n"+
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
      json.impl.append(
          "procedure TFHIRJsonParser"+".Parse"+tn.substring(5)+"Properties(jsn : TJsonObject; value : "+tn+");\r\n"+
          "begin\r\n");
      s = workingParserJ.toString();
      json.impl.append("    Parse"+parent+"Properties(jsn, value);"+marker()+"\r\n");
      json.impl.append(s);
      json.impl.append(
          "end;\r\n\r\n");
      ttl.impl.append(
          "procedure TFHIRTurtleParser"+".Parse"+tn.substring(5)+"Properties(obj : TTurtleComplex; value : "+tn+");\r\n");
      if (workingParserT.toString().contains("for item") || workingParserT.toString().contains("if obj.has(")) {
        ttl.impl.append("var\r\n");
        ttl.impl.append("  item : TTurtleComplex;\r\n");
      }
      ttl.impl.append("begin\r\n");
      s = workingParserT.toString();
      ttl.impl.append("    Parse"+parent+"Properties(obj, value);\r\n");
      ttl.impl.append(s);
      ttl.impl.append(
          "end;\r\n\r\n");
    }

    s = workingComposerJ.toString();
    json.impl.append(
        "procedure TFHIRJsonComposer"+".Compose"+tn.substring(5)+"(json : TJSONWriter; name : string; value : "+tn+"; noObj : boolean = false);\r\n");
    boolean var = false;
    if (s.contains("for i := ")) { // || category == ClassCategory.Resource) {
      json.impl.append("var\r\n  i : integer;\r\n");
      var = true;
    }
    if (s.contains("ext := ")) {
      if (!var) 
        json.impl.append("var\r\n");
      json.impl.append("  ext : boolean;\r\n");
      json.impl.append("  val : boolean;\r\n");
    }

    if (category == ClassGeneratorCategory.Resource)
      json.impl.append(
          "begin\r\n"+
          "  if (value = nil) then\r\n    exit;\r\n");
    else 
      json.impl.append(
          "begin\r\n"+
              "  if (value = nil) then\r\n    exit;\r\n"+
          "  if not noObj then json.valueObject(name);\r\n");
    if (definitions.getType(parent).getAbstract())
      json.impl.append("  Compose"+parent+"Properties(json, value);"+marker()+"\r\n");
    else
      json.impl.append("  Compose"+parent+"(json, '', value, true);"+marker()+"\r\n");

    json.impl.append(s);
    if (category == ClassGeneratorCategory.Resource)
      json.impl.append(
          "end;\r\n\r\n");
    else
      json.impl.append(
          "  if not noObj then json.finishObject;\r\n"+
          "end;\r\n\r\n");

    s = workingComposerR.toString().replace("%%%%", ti.getDefn().getPath());

    ttl.impl.append("procedure TFHIRTurtleComposer"+".Compose"+tn.substring(5)+"(parent :  TTurtleComplex; parentType, name : String; value : "+tn+"; useType : boolean; index : integer);\r\n");
    ttl.impl.append("var\r\n  this : TTurtleComplex;\r\n");
    if (tn.substring(5).equals("Quantity")) 
      ttl.impl.append("var\r\n  cb, c : TTurtleComplex;\r\n");
    if (s.contains("for i := "))
      ttl.impl.append("  i : integer;\r\n");
    if (s.contains("ext := ")) {
      ttl.impl.append("  ext : boolean;\r\n");
      ttl.impl.append("  val : boolean;\r\n");
    }
    ttl.impl.append(
        "begin\r\n"+
        "  if (value = nil) then\r\n    exit;\r\n");
    if (tn.substring(5).equals("Element"))
      ttl.impl.append("    this := parent;\r\n");
    else 
      ttl.impl.append(
          "  if (parentType = '') then\r\n"+    
              "    this := parent\r\n"+    
              "  else\r\n"+    
              "  begin\r\n"+    
              "    this := parent.addPredicate('fhir:'+parentType+'.'+name);\r\n"+
              "    if (useType) then\r\n"+
              "      this.addPredicate('a', 'fhir:"+tn.substring(5)+"');"+marker()+"\r\n"+
          "  end;\r\n");
    if (category == ClassGeneratorCategory.Resource)
      ttl.impl.append("  compose"+parent+"(this, '', name, value, false, index);\r\n");
    else if (category == ClassGeneratorCategory.Component)
      ttl.impl.append("  composeBackboneElement(this, '', name, value, false, index);\r\n");
    else if (tn.substring(5).equals("Element"))
      ttl.impl.append("  if (index > -1)  then\r\n    this.addPredicate('fhir:index', inttostr(index), 'int');\r\n");
    else
      ttl.impl.append("  composeElement(this, '', name, value, false, index);\r\n");

    ttl.impl.append(s);
    ttl.impl.append(
        "end;\r\n\r\n"
        );
  }

}
