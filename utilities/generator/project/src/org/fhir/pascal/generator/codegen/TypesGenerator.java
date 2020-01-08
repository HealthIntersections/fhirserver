package org.fhir.pascal.generator.codegen;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.Date;

import org.fhir.pascal.generator.analysis.Analysis;
import org.fhir.pascal.generator.analysis.TypeInfo;
import org.fhir.pascal.generator.engine.Configuration;
import org.fhir.pascal.generator.engine.Definitions;



public class TypesGenerator extends ClassGenerator {

  
  private StringBuilder ta0 = new StringBuilder();
  private StringBuilder tc0 = new StringBuilder();
  private StringBuilder ta1 = new StringBuilder();
  private StringBuilder tc1 = new StringBuilder();
  private StringBuilder ta2 = new StringBuilder();
  private StringBuilder tc2 = new StringBuilder();


  public TypesGenerator(Definitions definitions, Configuration configuration, Date genDate, String version) throws UnsupportedEncodingException {
    super(definitions, configuration, version, genDate);
  }

	public void generate(String filename) throws Exception {
	  String template = config.getTemplate("FHIR.R5.Types");
	  
	  template = template.replace("{{mark}}", startVMarkValue());
	  template = template.replace("{{type.abstract.fwds}}", ta0.toString());
	  template = template.replace("{{type.concrete.fwds}}", tc0.toString());
	  template = template.replace("{{type.abstract.intf}}", ta1.toString());
	  template = template.replace("{{type.concrete.intf}}", tc1.toString());
	  template = template.replace("{{type.abstract.impl}}", ta2.toString());
	  template = template.replace("{{type.concrete.impl}}", tc2.toString());

	  OutputStreamWriter w = new OutputStreamWriter(new FileOutputStream(filename));
    w.write(template);
		w.flush();
		w.close();
	}

	
  public void seeType(Analysis analysis) throws Exception {
    if (analysis.getName().equals("Base")) {
      return; // we don't generate base.
    }
    for (TypeInfo ti : analysis.getTypeList()) {
      if (analysis.isAbstract()) {
        generateType(analysis, ti, ClassGeneratorCategory.Component, ta0, ta1, ta2);
      } else {
        generateType(analysis, ti, ClassGeneratorCategory.Component, tc0, tc1, tc2);
      }
    }

    if (analysis.isAbstract()) { 
      generateAbstractType(analysis, analysis.getRootType(), ClassGeneratorCategory.Type, ta0, ta1, ta2);
    } else {
      generateType(analysis, analysis.getRootType(), ClassGeneratorCategory.Type, tc0, tc1, tc2);
    }    
  }



}
