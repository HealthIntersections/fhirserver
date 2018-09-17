package org.fhir.delphi;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Compartment {

  private String name;
  private String title;
  private String description;
  private Map<ResourceDefn, List<String>> resources = new HashMap<ResourceDefn, List<String>>();
  
  public String getName() {
    return name;
  }
  public void setName(String name) {
    this.name = name;
  }
  public String getTitle() {
    return title;
  }
  public void setTitle(String title) {
    this.title = title;
  }
  public String getDescription() {
    return description;
  }
  public void setDescription(String description) {
    this.description = description;
  }
  public Map<ResourceDefn, List<String>> getResources() {
    return resources;
  }
  
  
  public List<String> getPathsForName(String name) {
    for (ResourceDefn r : resources.keySet()) {
      if (r.getName().equals(name)) 
        return resources.get(r);
    }
    return null;
  }
  public String getUri() {
    return "http://hl7.org/fhir/compartment/"+getTitle();

  }
  
  
}
