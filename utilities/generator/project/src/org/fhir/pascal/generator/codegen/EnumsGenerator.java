package org.fhir.pascal.generator.codegen;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.fhir.pascal.generator.analysis.EnumInfo;
import org.fhir.pascal.generator.engine.Configuration;
import org.fhir.pascal.generator.engine.Definitions;
import org.hl7.fhir.r5.model.ElementDefinition;
import org.hl7.fhir.r5.model.StructureDefinition;
import org.hl7.fhir.r5.model.ValueSet;
import org.hl7.fhir.r5.model.ValueSet.ValueSetExpansionContainsComponent;
import org.hl7.fhir.utilities.Utilities;



public class EnumsGenerator extends BaseGenerator {

  
  private StringBuilder ed = new StringBuilder();
  private StringBuilder ec = new StringBuilder();
  private StringBuilder ev = new StringBuilder();
  private StringBuilder ec2 = new StringBuilder();
  private StringBuilder uc = new StringBuilder();
  private Map<String, String> uris = new HashMap<>();
  
  public EnumsGenerator(Definitions definitions, Configuration configuration, Date genDate, String version) throws UnsupportedEncodingException {
    super(definitions, configuration, version, genDate);
    
    addUri("URI_SNOMED", "http://snomed.info/sct");
    addUri("URI_LOINC", "http://loinc.org");
    addUri("URI_UCUM", "http://unitsofmeasure.org");
    addUri("URI_RXNORM", "http://www.nlm.nih.gov/research/umls/rxnorm");
    addUri("URI_CVX", "http://hl7.org/fhir/sid/cvx");
    addUri("URI_ATC", "http://www.whocc.no/atc");
    addUri("URI_NDC", "http://hl7.org/fhir/sid/ndc");
    addUri("URI_GTIN", "https://www.gs1.org/gtin");
    addUri("URI_BCP13", "urn:ietf:bcp:13");
    addUri("URI_BCP47", "urn:ietf:bcp:47");
    addUri("URI_11073", "urn:iso:std:iso:11073:10101");
    addUri("URI_3166", "urn:iso:std:iso:3166");
    addUri("URI_URIs", "urn:ietf:rfc:3986");
    addUri("URI_DICOM", "http://dicom.nema.org/resources/ontology/DCM");
    addUri("URI_PCLOCD", "https://fhir.infoway-inforoute.ca/CodeSystem/pCLOCD");
    addUri("URI_CPT", "http://www.ama-assn.org/go/cpt");
    addUri("URI_NDFRT", "http://hl7.org/fhir/ndfrt");
    addUri("URI_MEDRT", "http://hl7.org/fhir/medrt");
    addUri("URI_UNII", "http://fdasis.nlm.nih.gov");
    addUri("URI_ICD10", "http://hl7.org/fhir/sid/icd-10");
    addUri("URI_ICD9", "http://hl7.org/fhir/sid/icd-9");
    addUri("URI_AUSTRALIAN_PASSPORT_NUMBER", "urn:oid:2.16.840.1.113883.4.330.36");
    addUri("URI_FHIR_AUDIT_OBJECT_ROLE_R4", "http://terminology.hl7.org/CodeSystem/object-role");
    addUri("URI_FHIR_AUDIT_OBJECT_ROLE_R3", "http://hl7.org/fhir/object-role");
    addUri("URI_FHIR_AUDIT_OBJECT_LIFE_CYCLE_R3_DICOM", "http://hl7.org/fhir/dicom-audit-lifecycle");
    addUri("URI_FHIR_AUDIT_OBJECT_LIFE_CYCLE_R3_ISO", "http://hl7.org/fhir/iso-21089-lifecycle");
    addUri("URI_FHIR_AUDIT_OBJECT_LIFE_CYCLE_R4_DICOM", "http://terminology.hl7.org/CodeSystem/dicom-audit-lifecycle");
    addUri("URI_FHIR_AUDIT_OBJECT_LIFE_CYCLE_R4_ISO", "http://terminology.hl7.org/CodeSystem/iso-21089-lifecycle");
    addUri("URI_FHIR_SECURITY_SOURCE_TYPE_R3", "http://hl7.org/fhir/security-source-type");
    addUri("URI_FHIR_SECURITY_SOURCE_TYPE_R4", "http://terminology.hl7.org/CodeSystem/security-source-type");
    addUri("URI_FHIR_AUDIT_EVENT_TYPE_R4", "http://terminology.hl7.org/CodeSystem/audit-event-type");
    addUri("URI_FHIR_AUDIT_EVENT_TYPE_R3", "http://hl7.org/fhir/audit-event-type");
    addUri("URI_FHIR_AUDIT_ENTITY_TYPE_R4", "http://terminology.hl7.org/CodeSystem/audit-entity-type");
    addUri("URI_FHIR_AUDIT_ENTITY_TYPE_R3", "http://hl7.org/fhir/audit-entity-type");
    addUri("URI_FHIR_AUDIT_EVENT_OUTCOME", "http://hl7.org/fhir/audit-event-outcome");
    addUri("URI_FHIR_RESTFUL_OP", "http://hl7.org/fhir/restful-operation");

  }

	private void addUri(String cnst, String uri) {
    uris.put(uri, cnst);    
  }

  public void generate(String filename) throws Exception {
	  String template = config.getTemplate("fhir{N}_enums");
    template = template.replace("{{mark}}", startVMarkValue());
    template = template.replace("{{enum.decl}}", ed.toString());
    template = template.replace("{{enum.consts}}", ec.toString());
    template = template.replace("{{enum.conv}}", ev.toString());
    template = template.replace("{{enum.conv.impl}}", ec2.toString());
    template = template.replace("{{uri.consts}}", uc.toString());
    OutputStreamWriter w = new OutputStreamWriter(new FileOutputStream(filename));
    w.write(template);
    w.flush();
    w.close();
	}

  public void genEnums() {
    process();
  }

  private void process() {
    ec.append("const\r\n");
    
    Map<String, EnumInfo> list = new HashMap<>();
    for (StructureDefinition sd : definitions.getStructures().getList()) {
      for (ElementDefinition ed : sd.getSnapshot().getElement()) {
        if (ed.hasUserData("pascal.enum")) {
          EnumInfo ei = (EnumInfo) ed.getUserData("pascal.enum");
          list.put(ei.getName(), ei);
        }
      }
    }
    for (String n : sorted(list.keySet())) {
      seeEnum(list.get(n));
    }    
  }

  private void seeEnum(EnumInfo enumInfo) {
    String n = enumInfo.abbreviation();
    String tn = "TFhir"+n+(n.endsWith("Enum") ? "" : "Enum");
    ValueSet vse = (ValueSet) enumInfo.getValueSet().getUserData("expansion");
    
    line(ed, "  // "+vse.getDescription()+" (from "+vse.getUrl()+")");
    line(ed, "  "+tn+" = (");
    line(ed, "    "+n+"Null, // Value is missing from Instance");
    
    int c = 1;
    for (ValueSetExpansionContainsComponent cc : vse.getExpansion().getContains()) {
      line(ed, "    "+n+fixCode(cc.getCode()+(c == vse.getExpansion().getContains().size() ? ");" : ",")));
      c++;
    }
    if (vse.getExpansion().getContains().size() < 255) {
      line(ed, "  "+tn+"List = set of "+tn+";\r\n\r\n");
    }
    
    PascalArrayBuilder ab = new PascalArrayBuilder(tn, false);
    ab.addEntry("");
    for (ValueSetExpansionContainsComponent cc : vse.getExpansion().getContains()) {
      ab.addEntry(cc.getCode());
    }
    ec.append(ab.build());
    
//    int ls = vse.getExpansion().getContains().size()-1;
//    String system = vse.getExpansion().getContains().get(ls).getSystem();
//    while (ls > 0 && system.equals(vse.getExpansion().getContains().get(ls).getSystem())) {
    //      ls--;
    //    }
    ab = new PascalArrayBuilder("SYSTEMS_"+tn, tn, false);
    ab.setItemType("String");
    ab.setQuoteStrings(false);
    ab.addEntry("FHIR_URI_NONE");
    for (ValueSetExpansionContainsComponent cc : vse.getExpansion().getContains()) {
      ab.addEntry(getUriConst(cc.getSystem()));
    }
    ec.append(ab.build());


    if (vse.getExpansion().getContains().size() < 255) {
      line(ev, "function "+tn+"ListAsInteger(aSet : "+tn+"List) : Integer; overload;");
      line(ev, "function IntegerAs"+tn+"List(i : integer) : "+tn+"List; overload;");


      line(ec2, "function "+tn+"ListAsInteger(aSet : "+tn+"List) : Integer;");
      line(ec2, "var");
      line(ec2, "  a : "+tn+";");
      line(ec2, "begin");
      line(ec2, "  result := 0;");
      line(ec2, "  for a := low("+tn+") to high("+tn+") do");
      line(ec2, "  begin");
      line(ec2, "    assert(ord(a) < 32);");
      line(ec2, "    if (a in aSet) then");
      line(ec2, "      result := result + 1 shl (ord(a));");
      line(ec2, "  end;");
      line(ec2, "end;");
      line(ec2, "");
      line(ec2, "function IntegerAs"+tn+"List(i : Integer) : "+tn+"List;");
      line(ec2, "var");
      line(ec2, "  aLoop : "+tn+";");
      line(ec2, "begin");
      line(ec2, "  result := [];");
      line(ec2, "  for aLoop := low("+tn+") to high("+tn+") do");
      line(ec2, "  begin");
      line(ec2, "    assert(ord(aLoop) < 32);");
      line(ec2, "    if (i and (1 shl (ord(aLoop))) > 0) then");
      line(ec2, "      result := result + [aLoop];");
      line(ec2, "  end;");
      line(ec2, "end;");
      line(ec2, "");
    }

  }

  private String getUriConst(String system) {
    if (uris.containsKey(system)) {
      return uris.get(system);
    }
    String res = "FHIR_URI_"+utail(system).replace("-", "_").toUpperCase();
    if (uris.values().contains(res)) {
      throw new Error("bang! ("+system+")");
    }
    uris.put(system, res);
    uc.append("  "+res+" = '"+system+"';\r\n");
    return res;
  }

  private String utail(String system) {
    return system.substring(system.lastIndexOf("/")+1);
  }

  private String fixCode(String cc) {
    if (cc.equals("-"))
      cc = "Minus";
    else if (cc.equals("+"))  
      cc = "Plus";
    else {
      cc = cc.replace("-", " ").replace("+", " ").replace(".", " ");
      cc = Utilities.camelCase(cc);
      cc = cc.replace(">=", "greaterOrEquals").replace("<=", "lessOrEquals").replace("<", "lessThan").replace(">", "greaterThan").replace("!=", "notEqual").replace("=", "equal");
    }
    return Utilities.capitalize(cc);
  }

  private List<String> sorted(Set<String> keySet) {
    List<String> res = new ArrayList<>();
    res.addAll(keySet);
    Collections.sort(res);
    return res;
  }
	


}
