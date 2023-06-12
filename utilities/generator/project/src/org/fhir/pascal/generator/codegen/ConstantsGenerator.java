package org.fhir.pascal.generator.codegen;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.fhir.pascal.generator.analysis.Analysis;
import org.fhir.pascal.generator.engine.Configuration;
import org.fhir.pascal.generator.engine.Definitions;
import org.hl7.fhir.r5.model.CodeType;
import org.hl7.fhir.r5.model.Enumeration;
import org.hl7.fhir.r5.model.Enumerations.AllResourceTypes;
import org.hl7.fhir.r5.model.SearchParameter;
import org.hl7.fhir.r5.model.StructureDefinition;
import org.hl7.fhir.utilities.Utilities;
import org.hl7.fhir.utilities.VersionUtilities;



public class ConstantsGenerator extends BaseGenerator {

  public class StructureDefinitionSorter implements Comparator<StructureDefinition> {

    @Override
    public int compare(StructureDefinition arg0, StructureDefinition arg1) {
      return arg0.getUrl().compareTo(arg1.getUrl());
    }
  }

  private Map<StructureDefinition, Set<SearchParameter>> params = new HashMap<>();
  
  private StringBuilder rt = new StringBuilder();
  private StringBuilder spe = new StringBuilder();
  private StringBuilder spc = new StringBuilder();

  private PascalArrayBuilder rtc;
  private PascalArrayBuilder rtcl;
  private PascalArrayBuilder rtcc;
  private StringBuilder rac;
  private PascalArrayBuilder ran;
  
  public ConstantsGenerator(Definitions definitions, Configuration configuration, Date genDate, String version) throws UnsupportedEncodingException {
    super(definitions, configuration, version, genDate);
    
    rtc = new PascalArrayBuilder("TFhirResourceType", true);
    rtc.addEntry("");
    rtcl = new PascalArrayBuilder("LOWERCASE_CODES_TFhirResourceType", "TFhirResourceType", true);
    rtcl.addEntry("");
    rtcc = new PascalArrayBuilder("CLASSES_TFhirResourceType", "TFhirResourceType", true);
    rtcc.setItemType("TFhirResourceClass");
    rtcc.addEntry("nil");
    rac = new StringBuilder();
    rac.append("  ALL_RESOURCE_TYPES = [\r\n");
    ran = new PascalArrayBuilder("ALL_RESOURCE_TYPE_NAMES", "TFhirResourceType", true);
    ran.addEntry("--None--");
    
  }

	public void generate(String filename) throws Exception {
	  doGen();
	  String template = config.getTemplate("fhir{N}_constants");    
    template = template.replace("{{mark}}", startVMarkValue());
    template = template.replace("{{ver-mmp}}", version);
    template = template.replace("{{ver-mm}}", VersionUtilities.getMajMin(version));
    template = template.replace("{{ver-m}}", version.substring(0, 1));
    template = template.replace("{{date}}", config.DATE_FORMAT().format(genDate));
    template = template.replace("{{rt.consts}}", rt.toString());
    template = template.replace("{{search.param.enums}}", spe.toString());
    template = template.replace("{{search.param.consts}}", spc.toString());

    OutputStreamWriter w = new OutputStreamWriter(new FileOutputStream(filename));
    w.write(template);
    w.flush();
    w.close();
	}

  private void doGen() {
    rtc.addEntry("Custom");
    rtcl.addEntry("custom");
    rtcc.addEntry("nil");
    ran.addEntry("Custom");
    rac.append("    frtCustom];\r\n\r\n");


    List<StructureDefinition> sdl = new ArrayList<StructureDefinition>();
    sdl.addAll(params.keySet());
    Collections.sort(sdl, new StructureDefinitionSorter());
    for (StructureDefinition sd : sdl) {
      line(spe, "{$IFDEF "+getDefineName(sd)+"}");
      line(spe, "  // Search Parameters for "+sd.getName());
      line(spe, "  TSearchParams"+sd.getType()+" = (");
      PascalArrayBuilder ab = new PascalArrayBuilder("TSearchParams"+sd.getName());
      ab.setDefine(getDefineName(sd));
      List<String> pn = new ArrayList<>();
      for (SearchParameter sp : params.get(sd)) {
        pn.add(sp.getCode()+" {"+sp.getUrl()+"}");
      }
      Collections.sort(pn);
      boolean first = true;
      for (String n : pn) {
        if (first) first = false; else line(spe, ",");
        frag(spe, "    sp"+sd.getType()+"_"+Utilities.capitalize(n.replace("-", "").replace("[x]", "x")));
        ab.addEntry(n);
      }
      line(spe, ");");
      line(spe, "{$ENDIF "+getDefineName(sd)+"}"); 
      line(spe, "");
      spc.append(ab.build());
    }
    rt.append(rtc.build());
    rt.append(rtcl.build());
    rt.append(rtcc.build());
    rt.append(rac.toString());
    rt.append(ran.build());
  }

  public void seeResource(Analysis analysis) {
    if (!analysis.isAbstract() && !analysis.isInterface()) {
      params.put(analysis.getStructure(), new HashSet<SearchParameter>());
      rtc.addEntryDefine(analysis.getName(), analysis.getDefineName());
      rtcl.addEntryDefine(analysis.getName().toLowerCase(), analysis.getDefineName());
      rtcc.addEntryDefine(analysis.getClassName(), analysis.getDefineName());
      rac.append("    {$IFDEF "+analysis.getDefineName()+"} frt"+analysis.getName()+", {$ENDIF}\r\n");
      ran.addEntryDefine(analysis.getName(), analysis.getDefineName());
    }    
  }

  public void seeSearchParam(SearchParameter spd) {
    if (Utilities.existsInList(spd.getUrl(), "http://hl7.org/fhir/SearchParameter/example", "http://hl7.org/fhir/SearchParameter/example-reference", "http://hl7.org/fhir/SearchParameter/example-extension")) {
      return;
    }
    for (CodeType rt : spd.getBase()) {
      StructureDefinition sd = findSearchParam(rt.getCode());
      if (sd == null) {
        List<StructureDefinition> sdl = getDescendentList(rt.getCode());
        for (StructureDefinition sdd : sdl) {
          if (params.containsKey(sdd)) {
            params.get(sdd).add(spd);
          }
        }
      } else {
        params.get(sd).add(spd);
      }
    }
    
  }

  private List<StructureDefinition> getDescendentList(String code) {
    List<StructureDefinition> res = new ArrayList<StructureDefinition>();
    for (StructureDefinition sd : definitions.getStructures().getList()) {
      if (InheritsFrom(sd, code)) {
        res.add(sd);
      }
    }
    return res;
  }

  private boolean InheritsFrom(StructureDefinition sd, String code) {
    if (!sd.hasBaseDefinition()) {
      return false;
    }
    StructureDefinition base = definitions.getStructures().get(sd.getBaseDefinition());
    if (base == null) {
      return false;
    }
    if (base.getType().equals(code)) {
      return true;
    }
    return InheritsFrom(base, code);
  }

  private StructureDefinition findSearchParam(String code) {
    for (StructureDefinition sd : params.keySet()) {
      if (sd.getType().equals(code)) {
        return sd;
      }
    }
    return null;
  }
	


}
