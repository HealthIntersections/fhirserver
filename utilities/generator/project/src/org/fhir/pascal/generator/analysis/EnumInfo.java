package org.fhir.pascal.generator.analysis;

import org.hl7.fhir.r5.model.ValueSet;

public class EnumInfo {

  private String name;
  private ValueSet valueSet;
  private boolean shared;
  private String abbrev;
  
  public EnumInfo(String name) {
    this.name = name;
  }
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }
  public ValueSet getValueSet() {
    return valueSet;
  }
  public void setValueSet(ValueSet valueSet) {
    this.valueSet = valueSet;
  }
  public boolean isShared() {
    return shared;
  }
  public void setShared(boolean shared) {
    this.shared = shared;
  }
  public String getAbbrev() {
    return abbrev;
  }
  public void setAbbrev(String abbrev) {
    this.abbrev = abbrev;
  }
  public String abbreviation() {
    return abbrev != null ? abbrev : name;
  }
  
  
}
