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
  
  public EnumsGenerator(Definitions definitions, Configuration configuration, Date genDate, String version) throws UnsupportedEncodingException {
    super(definitions, configuration, version, genDate);
  }

	public void generate(String filename) throws Exception {
	  String template = config.getTemplate("FHIR.R5.Enums");
    template = template.replace("{{mark}}", startVMarkValue());
    template = template.replace("{{enum.decl}}", ed.toString());
    template = template.replace("{{enum.consts}}", ec.toString());
    template = template.replace("{{enum.conv}}", ev.toString());
    template = template.replace("{{enum.conv.impl}}", ec2.toString());
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
    String n = enumInfo.getName();
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
    ab.addEntry("");
    for (ValueSetExpansionContainsComponent cc : vse.getExpansion().getContains()) {
      ab.addEntry(cc.getSystem());
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
