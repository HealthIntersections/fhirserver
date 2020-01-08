package org.fhir.pascal.generator.codegen;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.Date;

import org.fhir.pascal.generator.analysis.Analysis;
import org.fhir.pascal.generator.analysis.TypeInfo;
import org.fhir.pascal.generator.engine.Configuration;
import org.fhir.pascal.generator.engine.Definitions;

public class ResourcesGenerator extends ClassGenerator {
  
  private StringBuilder rt = new StringBuilder();
  private StringBuilder rf = new StringBuilder();
  private StringBuilder ra1 = new StringBuilder();
  private StringBuilder rc1 = new StringBuilder();
  private StringBuilder ra2 = new StringBuilder();
  private StringBuilder rc2 = new StringBuilder();

  public ResourcesGenerator(Definitions definitions, Configuration configuration, Date genDate, String version) throws UnsupportedEncodingException {
    super(definitions, configuration, version, genDate);
    line(rt, "type");
    line(rt, "  TFhirResourceType = (");
    line(rt, "    frtNull, // Resource type not known / not Specified ");
  }

	public void generate(String filename) throws Exception {
	  finish();
	  String template = config.getTemplate("FHIR.R5.Resources");
    template = template.replace("{{mark}}", startVMarkValue());
    template = template.replace("{{$rt.enum}}", rt.toString());
    template = template.replace("{{reslist-fwds}}", rf.toString());
    template = template.replace("{{res.abstract.intf}}", ra1.toString());
    template = template.replace("{{res.concrete.intf}}", rc1.toString());
    template = template.replace("{{res.abstract.impl}}", ra2.toString());
    template = template.replace("{{res.concrete.impl}}", rc2.toString());

    OutputStreamWriter w = new OutputStreamWriter(new FileOutputStream(filename));
    w.write(template);
		w.flush();
		w.close();
	}

  private void finish() {
    line(rt, "    frtCustom);");
    line(rt, "  TFhirResourceTypeSet = set of TFhirResourceType;");
  }

  public void seeResource(Analysis analysis) throws Exception {
    if (analysis.getName().equals("Base")) {
      return; // we don't generate base.
    }
    if (!analysis.isAbstract()) {
      rf.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
      rc1.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
      rc2.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
    }
    for (TypeInfo ti : analysis.getTypeList()) {
      if (analysis.isAbstract()) {
        generateType(analysis, ti, ClassGeneratorCategory.Component, rf, ra1, ra2);
      } else {
        generateType(analysis, ti, ClassGeneratorCategory.Component, rf, rc1, rc2);
      }
    }

    if (analysis.isAbstract()) { 
      generateAbstractType(analysis, analysis.getRootType(), ClassGeneratorCategory.Resource, rf, ra1, ra2);
    } else {
      line(rt, "    {$IFDEF "+analysis.getDefineName()+"}frt"+analysis.getName()+", {$ENDIF}");
      generateType(analysis, analysis.getRootType(), ClassGeneratorCategory.Resource, rf, rc1, rc2);
    }    
    if (!analysis.isAbstract()) {
      rf.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
      rc1.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
      rc2.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
    }
  }
	


}
