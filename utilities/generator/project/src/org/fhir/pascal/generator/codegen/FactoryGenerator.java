package org.fhir.pascal.generator.codegen;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.Date;

import org.fhir.pascal.generator.analysis.Analysis;
import org.fhir.pascal.generator.analysis.TypeInfo;
import org.fhir.pascal.generator.engine.Configuration;
import org.fhir.pascal.generator.engine.Definitions;
import org.hl7.fhir.utilities.TextFile;
import org.hl7.fhir.utilities.Utilities;



public class FactoryGenerator extends BaseGenerator {

  StringBuilder gen = new StringBuilder();
  
  public FactoryGenerator(Definitions definitions, Configuration configuration, Date genDate, String version) throws UnsupportedEncodingException {
    super(definitions, configuration, version, genDate);
  }

	public void generate(String filename) throws Exception {
	  String source = TextFile.fileToString(Utilities.path(config.masterSource(), "FHIR.R5.Factory.pas"));
	  int start = source.indexOf("{gen-factory-start}");
	  int end = source.indexOf("{gen-factory-end}");
    source = source.substring(0, start+"{gen-factory-start}".length()+1) + gen.toString()+source.substring(end);
    
    OutputStreamWriter w = new OutputStreamWriter(new FileOutputStream(filename));
    w.write(source);
    w.flush();
    w.close();
	}

  public void seeType(Analysis analysis) {
    if (!analysis.isAbstract()) {
      line(gen, "  else if name = '"+analysis.getName()+"' then");
      line(gen, "    result := "+analysis.getClassName()+".create()");
    }
  }


  public void seeResource(Analysis analysis) {
    // TODO Auto-generated method stub
    line(gen, "{$IFDEF "+analysis.getDefineName()+"}");
    for (TypeInfo ti : analysis.getTypeList()) {
      line(gen, "  else if name = '"+ti.getDefn().getPath()+"' then");
      line(gen, "    result := "+ti.getName()+".create()");
    }
    if (!analysis.isAbstract() && !analysis.isInterface()) {
      line(gen, "  else if name = '"+analysis.getName()+"' then");
      line(gen, "    result := "+analysis.getClassName()+".create()");
    }      
    line(gen, "{$ENDIF "+analysis.getDefineName()+"}");    
  }
	


}
