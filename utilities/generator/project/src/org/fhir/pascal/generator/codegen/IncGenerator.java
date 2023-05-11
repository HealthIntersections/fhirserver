package org.fhir.pascal.generator.codegen;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.Date;

import org.fhir.pascal.generator.analysis.Analysis;
import org.fhir.pascal.generator.engine.Configuration;
import org.fhir.pascal.generator.engine.Definitions;



public class IncGenerator extends BaseGenerator {

  
  private StringBuilder inc = new StringBuilder();

  public IncGenerator(Definitions definitions, Configuration configuration, Date genDate, String version) throws UnsupportedEncodingException {
    super(definitions, configuration, version, genDate);
  }

	public void generate(String filename) throws Exception {
	  String template = config.getTemplate("fhir5.inc");
    template = template.replace("{{mark}}", startVMarkValue());
    template = template.replace("{{rt.define}}", inc .toString());
    
    OutputStreamWriter w = new OutputStreamWriter(new FileOutputStream(filename));
    w.write(template);
		w.flush();
		w.close();
	}

  public void seeResource(Analysis analysis) {
    if (!analysis.isAbstract() && !analysis.isInterface()) { 
      inc.append("{$DEFINE "+analysis.getDefineName()+"}\r\n");
    }
  }
	

}
