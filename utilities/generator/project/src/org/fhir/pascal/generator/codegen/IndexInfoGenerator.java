package org.fhir.pascal.generator.codegen;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.Date;

import org.fhir.pascal.generator.analysis.Analysis;
import org.fhir.pascal.generator.engine.Configuration;
import org.fhir.pascal.generator.engine.Definitions;



public class IndexInfoGenerator extends BaseGenerator {

  
  private StringBuilder i1 = new StringBuilder();
  private StringBuilder i2 = new StringBuilder();
  private StringBuilder i3 = new StringBuilder();

  public IndexInfoGenerator(Definitions definitions, Configuration configuration, Date genDate, String version) throws UnsupportedEncodingException {
    super(definitions, configuration, version, genDate);
  }

	public void generate(String filename) throws Exception {
	  String template = config.getTemplate("FHIR.R5.IndexInfo");
    template = template.replace("{{mark}}", startVMarkValue());
    template = template.replace("{{index-intf}}", i1.toString());
    template = template.replace("{{index-impl}}", i2.toString());
    template = template.replace("{{index.reg}}", i3.toString());

    OutputStreamWriter w = new OutputStreamWriter(new FileOutputStream(filename));
    w.write(template);
		w.flush();
		w.close();
	}

  public void seeResource(Analysis analysis) {
    // TODO Auto-generated method stub
    
  }
	


}
