package org.fhir.pascal.generator.codegen;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.Date;

import org.fhir.pascal.generator.analysis.Analysis;
import org.fhir.pascal.generator.engine.Configuration;
import org.fhir.pascal.generator.engine.Definitions;



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
    // TODO Auto-generated method stub
    
  }

  public void seeResource(Analysis analysis) {
    // TODO Auto-generated method stub
    
  }
	


}
