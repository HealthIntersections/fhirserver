package org.fhir.delphi;
import java.util.HashMap;
import java.util.Map;

public class ResourceDefn  {

  private Map<String, SearchParameterDefn> searchParams = new HashMap<String, SearchParameterDefn>();

  private boolean abstract_;

  private String name = null;
  private String display;
  private String definition = null;
  private TypeDefn root;   
  private boolean special;
  
  public String getName()
  {
    return name;
  }

  public void setName(String name)
  {
    this.name = name;
  }

  public String getDefinition()
  {
    return definition;
  }

  public void setDefinition(String def)
  {
    this.definition = def;
  }



  public TypeDefn getRoot()
  {
    return root;
  }

  public void setRoot(TypeDefn root)
  {
    this.root = root;
  }

  public Map<String, SearchParameterDefn> getSearchParams() {
    return searchParams;
  }


  public boolean isAbstract() {
    return abstract_;
  }

  public void setAbstract(boolean abstract_) {
    this.abstract_ = abstract_;
  }

  public String getDisplay() {
    return display;
  }

  public void setDisplay(String display) {
    this.display = display;
  }

  public boolean isSpecial() {
    return special;
  }

  public void setSpecial(boolean special) {
    this.special = special;
  }

  
  
}
