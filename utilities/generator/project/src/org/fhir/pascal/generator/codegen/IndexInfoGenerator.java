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

import org.fhir.pascal.generator.analysis.Analysis;
import org.fhir.pascal.generator.engine.Configuration;
import org.fhir.pascal.generator.engine.Definitions;
import org.hl7.fhir.r5.model.CodeType;
import org.hl7.fhir.r5.model.CompartmentDefinition;
import org.hl7.fhir.r5.model.CompartmentDefinition.CompartmentDefinitionResourceComponent;
import org.hl7.fhir.r5.model.Enumerations.SearchParamType;
import org.hl7.fhir.r5.model.SearchParameter;
import org.hl7.fhir.r5.model.StringType;
import org.hl7.fhir.r5.model.StructureDefinition;
import org.hl7.fhir.r5.model.StructureDefinition.StructureDefinitionKind;
import org.hl7.fhir.utilities.Utilities;



public class IndexInfoGenerator extends BaseGenerator {

  private StringBuilder indexHeaders = new StringBuilder();
  private StringBuilder indexBody = new StringBuilder();
  private StringBuilder indexMethods = new StringBuilder();
  private List<String> allResourceNames = new ArrayList<>();

  public IndexInfoGenerator(Definitions definitions, Configuration configuration, Date genDate, String version) throws UnsupportedEncodingException {
    super(definitions, configuration, version, genDate);
    for (StructureDefinition sd : definitions.getStructures().getSortedList()) {
      if (!sd.getAbstract() && sd.getKind() == StructureDefinitionKind.RESOURCE) {
        allResourceNames.add(sd.getType());
      }
    }
  }

	public void generate(String filename) throws Exception {
	  String template = config.getTemplate("fhir5_indexinfo");
    template = template.replace("{{mark}}", startVMarkValue());
    template = template.replace("{{index-intf}}", indexHeaders.toString());
    template = template.replace("{{index-impl}}", indexMethods.toString());
    template = template.replace("{{index.reg}}", indexBody.toString());

    OutputStreamWriter w = new OutputStreamWriter(new FileOutputStream(filename));
    w.write(template);
		w.flush();
		w.close();
	}

	public void seeResource(Analysis analysis) {
	  if (!analysis.isAbstract()) {
	    StringBuilder b = new StringBuilder();
	    indexHeaders.append("    {$IFDEF "+analysis.getDefineName()+"}\r\n    procedure buildIndexesFor"+analysis.getName()+"(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);\r\n    {$ENDIF}\r\n");
	    indexBody.append("  {$IFDEF "+analysis.getDefineName()+"}\r\n  buildIndexesFor"+analysis.getName()+"(Indexes, compartments);\r\n  {$ENDIF}\r\n");
	    b.append("{$IFDEF "+analysis.getDefineName()+"}\r\nprocedure TFHIRIndexBuilderR5.buildIndexesFor"+analysis.getName()+"(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);\r\n");
	    b.append("begin\r\n");
	    for (SearchParameter sp : analysis.getAllSearchParams()) {
	      String s = "  indexes.add('"+analysis.getName()+"', '"+sp.getCode()+"', '"+escape(removeEoln(extractDoco(sp.getDescription(), analysis.getName())))+"', spt"+(sp.getType()== SearchParamType.SPECIAL ? "NULL" : sp.getType().toString())+", "+
	          getTarget(sp.getTarget(), 800)+", '";
	      int llen = getLastLineLength(s);
	      b.append(s);
	      b.append(breakStringConstIntoLines(escape(sp.getExpression()), llen));
	      b.append("', sxp"+(sp.getXpathUsage() == null ? "Null" : Utilities.capitalize(sp.getXpathUsage().toCode()))+");\r\n");
	    }
	    b.append("  indexes.add('"+analysis.getName()+"', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);\r\n");


	    for (CompartmentDefinition c : definitions.getCompartments().getSortedList()) {
	      CompartmentDefinitionResourceComponent cc = getByResource(c, analysis.getName()); 
	          if (cc != null && cc.hasParam()) {
	            b.append("  compartments.register('"+c.getCode().toCode()+"', '"+analysis.getName()+"', [");
	            boolean first = true;
	            for (StringType s : cc.getParam()) {
	              if (first)
	                first = false;
	              else
	                b.append(", ");
	              b.append("'"+s.asStringValue()+"'");
	            }
	            b.append("]);\r\n");
	          }
	    }

	    b.append("end;\r\n{$ENDIF "+analysis.getDefineName()+"}\r\n\r\n");
	    indexMethods.append(b.toString());
	  }
  }


	private String extractDoco(String description, String resource) {
	  if (description == null) {
	    return "";
	  }
	  if (description.startsWith("Multiple Resources")) {
	    String[] lines = description.split("\\r");
	    for (String line : lines) {
	      String s = line.trim();
	      if (s.startsWith("* ["+resource)) {
	        int i = s.indexOf("):");
	        if (i > 0) {
	          return s.substring(i);
	        }
	      }
	    }
	    return "??";
	  }
	  return description;
	}


  private CompartmentDefinitionResourceComponent getByResource(CompartmentDefinition c, String name) {
    for (CompartmentDefinitionResourceComponent t : c.getResource()) {
      if (t.getCode().equals(name)) {
        return t;
      }
    }
    return null;
  }

  private String getTarget(List<CodeType> list, int l) {
    if ((list.size() == 1 && list.get(0).getCode().equals("Resource")) || hasAllResources(list)) {
      return "ALL_RESOURCE_TYPE_NAMES";
    }

    StringBuilder s = new StringBuilder();
    s.append("[");
    int i = 1;
    boolean first = true;
    for (CodeType p : list) {
      if (isResource(p.getValue())) {
        if (!first)
          s.append(", ");
        s.append("'"+p+"'");
        first = false;
        i = i + p.getValue().length()+5;
        if (i > l) {
          s.append("\r\n      ");
          i = 6;
        }
      }
    }
    s.append("]");
    return s.toString();
  }


  private boolean hasAllResources(List<CodeType> list) {
    for (String name : allResourceNames) {
      if (!Utilities.existsInList(name, "Parameters")) {
        boolean found = false;
        for (CodeType c : list) {
          found = found || (c.getValue().equals(name));
        }
        if (!found) {
          return false;
        }
      }
    }
    return true;
  }

  private int getLastLineLength(String s) {
    return s.length() - s.lastIndexOf("\n") - 1;
  }



  private String breakStringConstIntoLines(String str, int llen) {
    if (llen + str.length() < 1000) 
      return str;
    StringBuilder b = new StringBuilder();
    while (str.length() > 1000-llen) {
      b.append(str.substring(0, 1000-llen)+"'+\r\n   '");
      str = str.substring(1000-llen);
      llen = 4;
    }
    b.append(str);
    return b.toString();
  }

}
