package org.fhir.delphi;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.hl7.fhir.dstu2.model.Enumerations.BindingStrength;

public class DefinitionDumper {

  private Map<String, BindingSpecification> bindings = new HashMap<String, BindingSpecification>();
  
  public String dumpDefinitions(Definitions definitions) throws Exception {
    StringBuilder b = new StringBuilder();
    dumpPrimitives(b, definitions.getPrimitives());
    dumpTypes("types", b, definitions.getTypes());
    dumpTypes("structures", b, definitions.getStructures());
    dumpTypes("infrastructure", b, definitions.getInfrastructure());
    dumpResources("base", b, definitions.getBaseResources());
    dumpResources("actual", b, definitions.getResources());
  
    dumpBindings(b, definitions);
    return b.toString();
  }

  private void dumpBindings(StringBuilder b, Definitions definitions) throws Exception {
    b.append("Bindings");
    b.append("\r\n");
    for (String s : sorted(bindings.keySet())) {
      BindingSpecification bs = bindings.get(s);
      b.append("  ");
      b.append(bs.getName());
      b.append(" : ");
      b.append(s);
      b.append("; ");
      b.append(bs.getReference());
      b.append("\r\n");
      if (bs.hasAllCodes()) {
        List<DefinedCode> ac = bs.getAllCodes();
        for (DefinedCode c : ac) { 
          b.append("    ");
          b.append(c.getSystem());
          b.append("#");
          b.append(c.getCode());
          b.append(" \"");
          b.append(c.getDisplay());
          b.append("\"");
          b.append("\r\n");
        }
      }
    }
    b.append("\r\n");    
  }

  private void dumpResources(String name, StringBuilder b, Map<String, ResourceDefn> resources) {
    b.append(name);
    b.append("\r\n");
    for (String s : sorted(resources.keySet())) {
      ResourceDefn rd = resources.get(s);
      dumpElement(b, 2, rd.getRoot());
    }            
    b.append("\r\n");
  }

  private void dumpTypes(String name, StringBuilder b, Map<String, TypeDefn> types) {
    b.append(name);
    b.append("\r\n");
    for (String s : sorted(types.keySet())) {
      TypeDefn td = types.get(s);
      dumpElement(b, 2, td);
    }        
    b.append("\r\n");
  }

  private void dumpElement(StringBuilder b, int indent, ElementDefn ed) {
    for (int i = 0; i < indent; i++) {
      b.append(" ");
    }
    b.append(ed.getName());
    b.append(" : ");
    b.append(ed.typeCode());
    b.append("[");
    b.append(ed.describeCardinality());
    b.append("]");
    if (ed.typeCode().equals("code") && ed.hasBinding() && ed.getBinding().getStrength() == BindingStrength.REQUIRED) {
      b.append(" <<");
//      if (ed.getBinding().getValueSet() != null) {
//        b.append(ed.getBinding().getValueSet().getUrl());
//        bindings.put(ed.getBinding().getValueSet().getUrl(), ed.getBinding());
//      }
//      else {
//        b.append(" --> ");
//        b.append(ed.getBinding().getReference());
//      }
      b.append(">>");
    }
    b.append(" // ");
    if (ed.getShortDefn() != null)
    b.append(ed.getShortDefn());
    b.append("\r\n");
    for (ElementDefn child : ed.getElements()) {
      dumpElement(b, indent+2, child);
    }
  }

  private void dumpPrimitives(StringBuilder b, Map<String, DefinedCode> primitives) {
    b.append("Primitives\r\n");
    for (String s : sorted(primitives.keySet())) {
      DefinedCode cd = primitives.get(s);
      b.append("  ");
      b.append(cd.getCode());
      b.append(" (");
      b.append(cd.getAbstract());
      b.append(") :");
      b.append(cd.getParent());
      b.append(" // ");
      if (cd.getDefinition() != null)
        b.append(cd.getDefinition());
      b.append("\r\n");
    }    
    b.append("\r\n");
  }

  private List<String> sorted(Set<String> keySet) {
    List<String> result = new ArrayList<String>();
    result.addAll(keySet);
    Collections.sort(result);
    return result;
  }
}


