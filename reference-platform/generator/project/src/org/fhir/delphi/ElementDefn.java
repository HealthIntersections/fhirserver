package org.fhir.delphi;

/*
 Copyright (c) 2011+, HL7, Inc
 All rights reserved.

 Redistribution and use in source and binary forms, with or without modification, 
 are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this 
 list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice, 
 this list of conditions and the following disclaimer in the documentation 
 and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to 
 endorse or promote products derived from this software without specific 
 prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
 IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
 PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
 WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
 POSSIBILITY OF SUCH DAMAGE.

 */
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.fhir.delphi.BindingSpecification.BindingMethod;
import org.hl7.fhir.dstu2.model.ElementDefinition;
import org.hl7.fhir.dstu2.model.ElementDefinition.ElementDefinitionBindingComponent;
import org.hl7.fhir.dstu2.model.ElementDefinition.TypeRefComponent;
import org.hl7.fhir.dstu2.model.Enumerations.BindingStrength;
import org.hl7.fhir.dstu2.model.StructureDefinition;
import org.hl7.fhir.dstu2.model.ValueSet;
import org.hl7.fhir.exceptions.FHIRException;
import org.hl7.fhir.utilities.Utilities;


public class ElementDefn {

  public static final int MAX_NEG = -1000000;
  
  private List<TypeRef> types = new ArrayList<TypeRef>();
  private List<ElementDefn> elements = new ArrayList<ElementDefn>();
 
  private Integer minCardinality;
  private Integer maxCardinality;
  private Boolean summaryItem; // whether this is included in a summary
  private boolean xmlAttribute;
  private BindingSpecification binding;
  private String name;
  private String shortDefn;
  private String definition;
  private String path;
  private String statedType; // explicitly stated type (=xxxx)
  private boolean inherited;
  private String defaultValue;
  


  public String getDefinition() {
    return (definition == null || "".equals(definition)) ? Utilities.appendPeriod(shortDefn)
        : definition;
  }
  
  public void setDefinition(String definition) {
    this.definition = definition;
  }

  public void setElements(List<ElementDefn> elements) {
    this.elements = elements;
  }

  public List<ElementDefn> getElements() {
    return elements;
  }

  public boolean hasNestedElements() {
    return elements != null && !elements.isEmpty();
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public boolean hasName() {
    return this.name != null && !this.name.equals("");
  }

  public String getShortDefn() {
    return shortDefn;
  }

  public void setShortDefn(String shortDefn) {
    this.shortDefn = shortDefn;
  }

  public boolean hasShortDefn() {
    return shortDefn != null && !"".equals(shortDefn);
  }

  
  public ElementDefn getElementByName(String name, boolean throughChoice, Definitions definitions, String purpose) throws Exception {
    String n = name.contains(".") ? name.substring(0, name.indexOf(".")) : name;
    String t = name.contains(".") ? name.substring(name.indexOf(".") + 1) : null;
    if (n.equals(this.name) && t != null)
      return getElementByName(t);
    
    ElementDefn focus = this;
    
    if (typeCode().startsWith("@")) {
      String s = typeCode().substring(1);
      focus = definitions.getElementDefn(s.substring(0, s.indexOf(".")));
      focus = focus.getElementForPath(s, definitions, purpose, throughChoice);
    }
      
    for (int i = focus.elements.size() - 1; i >= 0; i--) {
      ElementDefn e = focus.elements.get(i);
      if (nameMatches(n, e, throughChoice, definitions))
        return t == null ? e : e.getElementByName(t);
    }
    return null;
  }

  private boolean nameMatches(String n, ElementDefn e, boolean throughChoice, Definitions definitions) {
    if (e.getName().equals(n))
      return true;
    else if (!throughChoice || !e.getName().endsWith("[x]"))
      return false;
    else {
      String b = e.getName().substring(0, e.getName().indexOf("["));
      if (!n.startsWith(b))
        return false;
      String tn = n.substring(b.length());
      if (e.typeCode().equals("*") && definitions != null) {
        for (TypeRef t : definitions.getKnownTypes()) {
          if (!definitions.getInfrastructure().containsKey(t.getName())) {
            if (t.getName().equalsIgnoreCase(tn))
              return true;
          }
        }
      } else for (TypeRef t : e.getTypes()) 
        if (t.getName().equalsIgnoreCase(tn))
          return true;
      return false;
    }
  }

  public ElementDefn getElementByName(String name) {
    String n = name.contains(".") ? name.substring(0, name.indexOf("."))
        : name;
    String t = name.contains(".") ? name.substring(name.indexOf(".") + 1)
        : null;
    if (n.equals(this.name) && t != null)
      return getElementByName(t);

    for (int i = elements.size() - 1; i >= 0; i--) {
      ElementDefn e = elements.get(i);
      if (nameMatches(n, e, false, null))
        return t == null ? e : e.getElementByName(t);
//      if (e.getName().length() > name.length()
//          && e.getName().substring(0, name.length())
//              .equalsIgnoreCase(name)
//          && e.getElements().size() == 1
//          && e.getElements().get(0).getName().equalsIgnoreCase(name))
//        return e.getElements().get(0);
    }
    return null;
  }

  public BindingSpecification getBinding() {
    return binding;
  }

  public void setBinding(BindingSpecification binding) {
    this.binding = binding;
  }

  // public String getId() {
  // return id;
  // }
  //
  // public void setId(String id) {
  // this.id = id;
  // }


  public Integer getMinCardinality() {
    return minCardinality;
  }

  public void setMinCardinality(Integer minCardinality) {
    this.minCardinality = minCardinality;
  }

  public Integer getMaxCardinality() {
    return maxCardinality;
  }

  public void setMaxCardinality(Integer maxCardinality) {
    this.maxCardinality = maxCardinality;
  }

  public String describeCardinality() {
    String min = minCardinality == null ? "" : minCardinality.toString();
    String max = maxCardinality == null ? "" : maxCardinality == Integer.MAX_VALUE ? "*" : maxCardinality.toString();
    return min + ".." + max;
  }

  // public String textForCardinality() {
  // if (maxCardinality != null) {
  // if (maxCardinality == 1)
  // if (minCardinality == 0)
  // return "?One";
  // else
  // return "One";
  // else
  // return "??";
  // } else if (minCardinality == 0)
  // return "Zero+";
  // else
  // return "One+";
  // }

  public boolean hasDefinition() {
    return (this.definition != null && !this.definition.equals(""))
        || (shortDefn != null && !this.shortDefn.equals(""));
  }

  public boolean unbounded() {
    return maxCardinality != null && maxCardinality == Integer.MAX_VALUE;
  }

  public boolean hasBinding() {
    return binding != null;
  }

  // If an element with children explicitly declares a typename
  // ('=<typename>' in Excel "Type" column), a resource-local type is
  // defined and its name stored on the parent element.
  private String declaredTypeName = null;
  
  public String getDeclaredTypeName()
  {
    return declaredTypeName;
  }
  
  public void setDeclaredTypeName(String typeName)
  {
    this.declaredTypeName = typeName;
  }
  
  

  public List<TypeRef> getTypes() {
    return types;
  }
  
  public boolean hasOnlyType(String name) {
    return types.size() == 1 && types.get(0).getName().equals(name);
  }

  public boolean hasType(String name) {
    for (TypeRef t : types) {
      if (t.getName().equals(name))
        return true;
    }
    return false;
  }

  
  public String typeCode() {
    StringBuilder tn = new StringBuilder();
    boolean first = true;
    for (TypeRef t : types) {
      if (!first)
        tn.append("|");
      first = false;
      tn.append(t.getName());
      if (t.hasParams()) {
        tn.append("(");
        boolean f = true;
        for (String s : t.getParams()) {
          if (!f)
            tn.append("|");
          f = false;
          tn.append(s);
        }
        tn.append(")");
      }
    }
    return tn.toString();
  }

  
  public boolean usesCompositeType() {
    return this.typeCode().startsWith("@");
  }
  

  /**
   * Warning: this method is only safe to call if the owner element has type
   * "resource". The element names "id" and "text" are used in other contexts
   * for valid element names
   * 
   * @return if this element is a standard Resource element like 'id',
   *         'extension' and 'text'
   */
  public boolean isBaseResourceElement() {
    return getName().equals("id") || getName().equals("extension")
        || getName().equals("text");
  }

  public boolean isBoundCode() {
    return typeCode().equals("code") && hasBinding();
  }


  public boolean isXhtmlElement() {
    return !types.isEmpty() && types.get(0).isXhtml();
  }

  
  public ElementDefn getElementForPath(String pathname, Definitions definitions, String purpose, boolean throughChoice) throws Exception {
    String[] path = pathname.split("\\.");

    if (!path[0].equals(getName()))
      throw new Exception("Element Path '" + pathname
          + "' is not legal in this context ("+purpose+") - expected "+getName()+" found "+path[0]);

    ElementDefn res = this;

    for (int i = 1; i < path.length; i++) {
      String en = path[i];
      if (en.startsWith("extension("))
        return null; // don't resolve these here
      if (en.length() == 0)
        throw new Exception("Improper path " + pathname);
      ElementDefn t = null;

      if (res.typeCode().startsWith("@")) {
        res = this.getElementForPath(res.typeCode().substring(1), definitions, purpose, throughChoice);
      } else if (definitions.dataTypeIsSharedInfo(res.typeCode())) {
        res = definitions.getElementDefn(res.typeCode());
      } else if (definitions.hasType(res.typeCode())) {
        res = definitions.getElementDefn(res.typeCode());
      }
      t = res.getElementByName(en, throughChoice, definitions, purpose);
      if (t == null) {
        throw new Exception("unable to resolve " + pathname+" for purpose "+purpose);
      }
      res = t;

    }

    return res;
  }

       



  public boolean hasStatedType() {
    return statedType != null;
  }

  public String getStatedType() {
    return statedType;
  }

  public void setStatedType(String statedType) {
    this.statedType = statedType;
  }

  public boolean isSummary() {
    return summaryItem != null && summaryItem; 
  }

  public boolean hasSummaryItem() {
    return summaryItem != null; 
  }

  public Boolean isSummaryItem() {
    return summaryItem;
  }

  public void setSummaryItem(Boolean summaryItem) {
    this.summaryItem = summaryItem;
  }


  public boolean usesType(String name) {
    for (TypeRef t : getTypes()) {
      if (t.summary().equals(name) || (t.getName().equals(name) && name.equals("Reference")))
        return true;
    }
    return false;
  }

  public boolean isMandatory() {
    return (minCardinality != null && maxCardinality != null && minCardinality == 1 && maxCardinality == 1);    
  }

  public boolean isXmlAttribute() {
    return xmlAttribute;
  }

  public void setXmlAttribute(boolean xmlAttribute) {
    this.xmlAttribute = xmlAttribute;
  }

  public boolean hasStatedProfile() {
    if (types.isEmpty())
      return false;
    else for (TypeRef t : types)
      if (t.getProfile() != null)
        return true;
    return false;
  }

  public boolean eliminated() {
    return getMaxCardinality() != null && getMaxCardinality() == 0;
  }

  public String getPath() {
    return path;
  }

  public void setPath(String path) {
    this.path = path;
  }

  public String getPathTail() {
    return path.contains(".") ? path.substring(path.lastIndexOf(".")+1) : path;
  } 

  public String getPathHead() {
    return path.contains(".") ? path.substring(0, path.lastIndexOf(".")) : "";
  } 

  public void loadFrom(ElementDefinition ed, StructureDefinition sd, Map<String, ValueSet> vsmap) throws FHIRException {
    path = ed.getPath();
    name = Utilities.oidTail(ed.getPath());
    shortDefn = ed.getShort();
    definition = ed.getDefinition();
    minCardinality = ed.getMin();
    maxCardinality = "*".equals(ed.getMax()) ? Integer.MAX_VALUE : Integer.parseInt(ed.getMax());
    summaryItem = ed.getIsSummary();
    xmlAttribute = ed.getRepresentation().size() > 0;
    if (ed.getDefaultValue() != null)
      defaultValue = ed.getDefaultValue().primitiveValue();
 
    if (ed.hasNameReference()) {
      TypeRef tr = new TypeRef();
      ElementDefinition edr = getElementDefinitionByName(sd, ed.getNameReference());
      tr.setName("@"+edr.getPath());
      types.add(tr);
    } else {
      for (TypeRefComponent t : ed.getType()) {
        if (t.getCode().equals("Reference") && t.hasProfile()) {
          String ref = t.getProfile().get(0).getValue().substring(40);
          if (ref.equals("Resource"))
            ref = "Any";
          if (types.size() > 0 && types.get(types.size() -1).getName().equals("Reference")) {
            types.get(types.size() -1).getParams().add(ref);
          } else {
            TypeRef tr = new TypeRef();
            tr.setName("Reference");
            tr.getParams().add(ref);
            types.add(tr);
          }
//        } else if (t.hasProfile() ) {
//          String ref = t.getProfile().get(0).getValue().substring(40);
//          TypeRef tr = new TypeRef();
//          tr.setName(ref);
//          types.add(tr);
        } else {
          TypeRef tr = new TypeRef();
          tr.setName(t.getCode());
          types.add(tr);
        }
      }
    }

    if (types.size() == 1 && (types.get(0).getName().equals("Element") || types.get(0).getName().equals("BackboneElement"))) 
      types.clear();
    if (typeCode().equals("boolean|integer|decimal|base64Binary|instant|string|uri|date|dateTime|time|code|oid|id|unsignedInt|positiveInt|markdown|Annotation|Attachment|Identifier|CodeableConcept|Coding|Quantity|Range|Period|Ratio|SampledData|Signature|HumanName|Address|ContactPoint|Timing|Reference|Meta")) {
      types.clear();
      types.add(new TypeRef("*"));
    }
      
    if (ed.hasBinding() && ed.getBinding().getStrength() == BindingStrength.REQUIRED && "code".equals(typeCode())) {
      setBinding(convert(ed.getBinding(), vsmap, path));
    }
  }

  public void loadFrom(org.hl7.fhir.dstu3.model.ElementDefinition ed, org.hl7.fhir.dstu3.model.StructureDefinition sd, Map<String, org.hl7.fhir.dstu3.model.ValueSet> vsmap) throws FHIRException, org.hl7.fhir.exceptions.FHIRException {
    path = ed.getPath();
    name = Utilities.oidTail(ed.getPath());
    shortDefn = ed.getShort();
    definition = ed.getDefinition();
    minCardinality = ed.getMin();
    maxCardinality = "*".equals(ed.getMax()) ? Integer.MAX_VALUE : Integer.parseInt(ed.getMax());
    summaryItem = ed.getIsSummary();
    xmlAttribute = ed.getRepresentation().size() > 0;
    if (ed.getDefaultValue() != null)
      defaultValue = ed.getDefaultValue().primitiveValue();
 
    if (ed.hasContentReference()) {
      TypeRef tr = new TypeRef();
      org.hl7.fhir.dstu3.model.ElementDefinition edr = getElementDefinitionByName(sd, ed.getContentReference());
      tr.setName("@"+edr.getPath());
      types.add(tr);
    } else {
      for (org.hl7.fhir.dstu3.model.ElementDefinition.TypeRefComponent t : ed.getType()) {
        if (t.getCode() == null)
          throw new Error("no code on "+ed.getPath());
        if (t.getCode().equals("Reference") && t.hasTargetProfile()) {
          String ref = t.getTargetProfile().substring(40);
          if (ref.equals("Resource"))
            ref = "Any";
          if (types.size() > 0 && types.get(types.size() -1).getName().equals("Reference")) {
            types.get(types.size() -1).getParams().add(ref);
          } else {
            TypeRef tr = new TypeRef();
            tr.setName("Reference");
            tr.getParams().add(ref);
            types.add(tr);
          }
//        } else if (t.hasProfile() ) {
//          String ref = t.getProfile().get(0).getValue().substring(40);
//          TypeRef tr = new TypeRef();
//          tr.setName(ref);
//          types.add(tr);
        } else {
          TypeRef tr = new TypeRef();
          tr.setName(t.getCode());
          types.add(tr);
        }
      }
    }

    if (types.size() == 1 && (types.get(0).getName().equals("Element") || types.get(0).getName().equals("BackboneElement"))) 
      types.clear();
    if (typeCode().equals("boolean|integer|decimal|base64Binary|instant|string|uri|date|dateTime|time|code|oid|id|unsignedInt|positiveInt|markdown|Annotation|Attachment|Identifier|CodeableConcept|Coding|Quantity|Range|Period|Ratio|SampledData|Signature|HumanName|Address|ContactPoint|Timing|Reference|Meta")) {
      types.clear();
      types.add(new TypeRef("*"));
    }
      
    if (ed.hasBinding() && ed.getBinding().getStrength() == org.hl7.fhir.dstu3.model.Enumerations.BindingStrength.REQUIRED && "code".equals(typeCode())) {
      setBinding(convert(ed.getBinding(), vsmap));
    }
  }
  
  public void loadFrom(org.hl7.fhir.r4.model.ElementDefinition ed, org.hl7.fhir.r4.model.StructureDefinition sd, Map<String, org.hl7.fhir.r4.model.ValueSet> vsmap) throws FHIRException, org.hl7.fhir.exceptions.FHIRException {
    path = ed.getPath();
    name = Utilities.oidTail(ed.getPath());
    shortDefn = ed.getShort();
    definition = ed.getDefinition();
    minCardinality = ed.getMin();
    maxCardinality = "*".equals(ed.getMax()) ? Integer.MAX_VALUE : Integer.parseInt(ed.getMax());
    summaryItem = ed.getIsSummary();
    xmlAttribute = ed.getRepresentation().size() > 0;
    if (ed.getDefaultValue() != null)
      defaultValue = ed.getDefaultValue().primitiveValue();
 
    if (ed.hasContentReference()) {
      TypeRef tr = new TypeRef();
      org.hl7.fhir.r4.model.ElementDefinition edr = getElementDefinitionByName(sd, ed.getContentReference());
      tr.setName("@"+edr.getPath());
      types.add(tr);
    } else {
      for (org.hl7.fhir.r4.model.ElementDefinition.TypeRefComponent t : ed.getType()) {
        if (t.getCode() == null)
          throw new Error("no code on "+ed.getPath());
        if (t.getCode().equals("Reference") && t.hasTargetProfile()) {
          String ref = t.getTargetProfile().substring(40);
          if (ref.equals("Resource"))
            ref = "Any";
          if (types.size() > 0 && types.get(types.size() -1).getName().equals("Reference")) {
            types.get(types.size() -1).getParams().add(ref);
          } else {
            TypeRef tr = new TypeRef();
            tr.setName("Reference");
            tr.getParams().add(ref);
            types.add(tr);
          }
//        } else if (t.hasProfile() ) {
//          String ref = t.getProfile().get(0).getValue().substring(40);
//          TypeRef tr = new TypeRef();
//          tr.setName(ref);
//          types.add(tr);
        } else {
          TypeRef tr = new TypeRef();
          tr.setName(t.getCode());
          types.add(tr);
        }
      }
    }

    if (types.size() == 1 && (types.get(0).getName().equals("Element") || types.get(0).getName().equals("BackboneElement"))) 
      types.clear();
    if (typeCode().equals("boolean|integer|decimal|base64Binary|instant|string|uri|date|dateTime|time|code|oid|id|unsignedInt|positiveInt|markdown|Annotation|Attachment|Identifier|CodeableConcept|Coding|Quantity|Range|Period|Ratio|SampledData|Signature|HumanName|Address|ContactPoint|Timing|Reference|Meta")) {
      types.clear();
      types.add(new TypeRef("*"));
    }
      
    if (ed.hasBinding() && ed.getBinding().getStrength() == org.hl7.fhir.r4.model.Enumerations.BindingStrength.REQUIRED && "code".equals(typeCode())) {
      setBinding(convert(ed.getBinding(), vsmap));
    }
  }
  
  private BindingSpecification convert(ElementDefinitionBindingComponent b, Map<String, ValueSet> vsmap, String path) throws FHIRException {
    BindingSpecification bs = new BindingSpecification(null, declaredTypeName, false);
    bs.setBindingMethod(BindingMethod.CodeList);
    bs.setDefinition(b.getDescription());
    bs.setStrength(BindingStrength.REQUIRED);
    if (b.hasValueSetReference()) {
      ValueSet vs = vsmap.get(b.getValueSetReference().getReference());
      if (vs == null)
        throw new Error("Value Set "+b.getValueSetReference().getReference()+" not found, referenced from "+path);
      bs.setUri(vs.getUrl());
      bs.loadFromExpansion(vs);
      bs.setName(vs.getName().replace(" ", ""));
      bs.setReference(urlTail(vs.getUrl()));
    } else if (b.hasValueSetUriType()) {
      return null;
    }
    return bs;
  }

  private BindingSpecification convert(org.hl7.fhir.dstu3.model.ElementDefinition.ElementDefinitionBindingComponent b, Map<String, org.hl7.fhir.dstu3.model.ValueSet> vsmap) throws FHIRException, org.hl7.fhir.exceptions.FHIRException {
    BindingSpecification bs = new BindingSpecification(null, declaredTypeName, false);
    bs.setBindingMethod(BindingMethod.CodeList);
    bs.setDefinition(b.getDescription());
    bs.setStrength(BindingStrength.REQUIRED);
    if (b.hasValueSetReference()) {
      org.hl7.fhir.dstu3.model.ValueSet vs = vsmap.get(b.getValueSetReference().getReference());
      if (vs == null)
        throw new Error("Unable to find value set "+b.getValueSetReference().getReference());
      bs.loadFromExpansion(vs);
      bs.setUri(vs.getUrl());
      bs.setName(vs.getName().replace(" ", ""));
      bs.setReference(urlTail(vs.getUrl()));
    } else if (b.hasValueSetUriType()) {
      return null;
    }
    return bs;
  }

  private BindingSpecification convert(org.hl7.fhir.r4.model.ElementDefinition.ElementDefinitionBindingComponent b, Map<String, org.hl7.fhir.r4.model.ValueSet> vsmap) throws FHIRException, org.hl7.fhir.exceptions.FHIRException {
    BindingSpecification bs = new BindingSpecification(null, declaredTypeName, false);
    bs.setBindingMethod(BindingMethod.CodeList);
    bs.setDefinition(b.getDescription());
    bs.setStrength(BindingStrength.REQUIRED);
    if (b.hasValueSetReference()) {
      org.hl7.fhir.r4.model.ValueSet vs = vsmap.get(b.getValueSetReference().getReference());
      if (vs == null)
        throw new Error("Unable to find value set "+b.getValueSetReference().getReference());
      bs.loadFromExpansion(vs);
      bs.setUri(vs.getUrl());
      bs.setName(vs.getName().replace(" ", ""));
      bs.setReference(urlTail(vs.getUrl()));
    } else if (b.hasValueSetUriType()) {
      return null;
    }
    return bs;
  }

  private String urlTail(String url) {
    return "#"+url.substring(url.lastIndexOf("/")+1);
  }

  private ElementDefinition getElementDefinitionByName(StructureDefinition sd, String name) {
    for (ElementDefinition t : sd.getDifferential().getElement()) {
      if (name.equals(t.getName()))
        return t;
    }
    return null;
  } 

  private org.hl7.fhir.dstu3.model.ElementDefinition getElementDefinitionByName(org.hl7.fhir.dstu3.model.StructureDefinition sd, String name) {
    for (org.hl7.fhir.dstu3.model.ElementDefinition t : sd.getDifferential().getElement()) {
      if (name.equals("#"+t.getId()))
        return t;
    }
    return null;
  }

  private org.hl7.fhir.r4.model.ElementDefinition getElementDefinitionByName(org.hl7.fhir.r4.model.StructureDefinition sd, String name) {
    for (org.hl7.fhir.r4.model.ElementDefinition t : sd.getDifferential().getElement()) {
      if (name.equals("#"+t.getId()))
        return t;
    }
    return null;
  }

  public boolean isInherited() {
    return inherited;
  }

  public void setInherited(boolean inherited) {
    this.inherited = inherited;
  }

  public String getDefaultValue() {
    return defaultValue;
  }
  
  public boolean hasDefaultValue() {
    return defaultValue != null;
  }

  public void setDefaultValue(String defaultValue) {
    this.defaultValue = defaultValue;
  } 


}

