package org.fhir.pascal.generator.codegen;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.fhir.pascal.generator.analysis.Analysis;
import org.fhir.pascal.generator.analysis.TypeInfo;
import org.fhir.pascal.generator.engine.Configuration;
import org.fhir.pascal.generator.engine.Definitions;
import org.hl7.fhir.utilities.Utilities;

public class ResourcesGenerator extends ClassGenerator {

  private ResourcesGeneratorData data = new ResourcesGeneratorData(new StringBuilder(), new StringBuilder(), new StringBuilder(),
      new StringBuilder(), new StringBuilder(), new StringBuilder());

  private Map<String, ResourcesGeneratorData> parts = new HashMap<>();

  private ResourcesGeneratorData getData(String name) {
    if (parts.containsKey(name)) {
      return parts.get(name);
    }
    ResourcesGeneratorData dt = new ResourcesGeneratorData(new StringBuilder(), new StringBuilder(), new StringBuilder(), new StringBuilder(), new StringBuilder(), new StringBuilder());
    parts.put(name, dt);        
    return dt;
  }

  public ResourcesGenerator(Definitions definitions, Configuration configuration, Date genDate, String version) throws UnsupportedEncodingException {
    super(definitions, configuration, version, genDate);
    ResourcesGeneratorData dt = getData("base");
    
    line(dt.rt, "type");
    line(dt.rt, "  TFhirResourceType = (");
    line(dt.rt, "    frtNull, // Resource type not known / not Specified ");
  }

  public void generate(String filename) throws Exception {
    finish();
    for (String n : parts.keySet()) {
      String template;
      if (config.hasTemplate("fhir{N}_resources_"+n)) {
        template = config.getTemplate("fhir{N}_resources_"+n);
      } else {
        template = config.getTemplate("fhir{N}_resources_X");        
      }
      ResourcesGeneratorData dt = getData(n);
      template = template.replace("{{mark}}", startVMarkValue());
      template = template.replace("{{$rt.enum}}", dt.rt.toString());
      template = template.replace("{{N}}", n);
      template = template.replace("{{canonical}}", n.equals("canonical") ? "" : ", fhir"+N()+"_resources_canonical");
      template = template.replace("{{reslist-fwds}}", dt.rf.toString());
      template = template.replace("{{res.abstract.intf}}", dt.ra1.toString());
      template = template.replace("{{res.concrete.intf}}", dt.rc1.toString());
      template = template.replace("{{res.abstract.impl}}", dt.ra2.toString());
      template = template.replace("{{res.concrete.impl}}", dt.rc2.toString());

      OutputStreamWriter w = new OutputStreamWriter(new FileOutputStream(Utilities.path(Utilities.getDirectoryForFile(filename), "fhir"+N()+"_resources_"+n+".pas")));
      w.write(template);
      w.flush();
      w.close();      
    }
    String template = config.getTemplate("fhir{N}_resources");
    template = template.replace("{{mark}}", startVMarkValue());
    template = template.replace("{{$rt.enum}}", data.rt.toString());
    template = template.replace("{{redeclare}}", data.redeclare.toString());
    template = template.replace("{{reslist-fwds}}", data.rf.toString());
    template = template.replace("{{res.abstract.intf}}", data.ra1.toString());
    template = template.replace("{{res.concrete.intf}}", data.rc1.toString());
    template = template.replace("{{res.abstract.impl}}", data.ra2.toString());
    template = template.replace("{{res.concrete.impl}}", data.rc2.toString());

    OutputStreamWriter w = new OutputStreamWriter(new FileOutputStream(filename));
    w.write(template);
    w.flush();
    w.close();
  }

  private void finish() {
    ResourcesGeneratorData dt = getData("base");
    line(dt.rt, "    frtCustom);");
    line(dt.rt, "  TFhirResourceTypeSet = set of TFhirResourceType;");
  }

  public void seeResource(Analysis analysis) throws Exception {
    if (analysis.getName().equals("Base")) {
      return; // we don't generate base.
    }
    String group = config.getIni().getStringProperty("resources", analysis.getName());
    if (group == null) {
      throw new Error("Uncategorised resource "+ analysis.getName());
    }
    ResourcesGeneratorData dt = getData(group);
    
    if (!analysis.isAbstract()) {
      dt.rf.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
      dt.rc1.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
      dt.rc2.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
      data.redeclare.append("{$IFDEF "+analysis.getDefineName()+"}\r\n");
    }
    for (TypeInfo ti : analysis.getTypeList()) {
      String n = null; 
      if (analysis.isAbstract()) {
        n = generateType(analysis, ti, ClassGeneratorCategory.Component, dt.rf, dt.ra1, dt.ra2);
      } else {
        n = generateType(analysis, ti, ClassGeneratorCategory.Component, dt.rf, dt.rc1, dt.rc2);
      }
      data.redeclare.append("  "+n+" = fhir"+N()+"_resources_"+group+"."+n+";\r\n");
      data.redeclare.append("  "+n+"List = fhir"+N()+"_resources_"+group+"."+n+"List;\r\n");
    }

    if (analysis.isAbstract()) { 
      generateAbstractType(analysis, analysis.getRootType(), ClassGeneratorCategory.Resource, dt.rf, dt.ra1, dt.ra2);
    } else {
      line(getData("base").rt, "    {$IFDEF "+analysis.getDefineName()+"}frt"+analysis.getName()+", {$ENDIF}");
      String n = generateType(analysis, analysis.getRootType(), ClassGeneratorCategory.Resource, dt.rf, dt.rc1, dt.rc2);
      data.redeclare.append("  "+n+" = fhir"+N()+"_resources_"+group+"."+n+";\r\n");
      data.redeclare.append("  "+n+"List = fhir"+N()+"_resources_"+group+"."+n+"List;\r\n");
    }    
    if (!analysis.isAbstract()) {
      dt.rf.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
      dt.rc1.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
      dt.rc2.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
      data  .redeclare.append("{$ENDIF "+analysis.getDefineName()+"}\r\n");
    }
  }



}
