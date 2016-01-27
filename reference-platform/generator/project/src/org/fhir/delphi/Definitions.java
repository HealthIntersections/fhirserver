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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.hl7.fhir.dstu2.model.ConceptMap;
import org.hl7.fhir.dstu2.model.DateTimeType;
import org.hl7.fhir.dstu2.model.NamingSystem;
import org.hl7.fhir.dstu2.model.StructureDefinition;
import org.hl7.fhir.dstu2.model.ValueSet;
import org.hl7.fhir.dstu2.model.StructureDefinition.ExtensionContext;

/**
 * This class is the root to all the definitions in FHIR. There are the
 * following kinds of items: DefinedResources - a named list of resources that
 * are defined, with element definitions for the resources Known Resources - a
 * list of Resource names with their definitions for all known definitions,
 * whether defined or not Concept Domains - a list of concept domains Events - a
 * list of message events Documents - a list of defined document profiles
 * 
 * @author Grahame
 * 
 */
public class Definitions {
  
  private String version;
  private DateTimeType genDate;
  

  /// value sets
  private Map<String, BindingSpecification> commonBindings = new HashMap<String, BindingSpecification>();
  private Map<String, ValueSet> boundValueSets = new HashMap<String, ValueSet>(); // indexed by ValueSet.url
  private Set<BindingSpecification> unresolvedBindings = new HashSet<BindingSpecification>();
  private List<BindingSpecification> allBindings = new ArrayList<BindingSpecification>();

  // base definitions - types and resources of various kinds
  private Map<String, DefinedCode> primitives = new HashMap<String, DefinedCode>();
	private Map<String, TypeDefn> types = new HashMap<String, TypeDefn>();
	private Map<String, TypeDefn> structures = new HashMap<String, TypeDefn>();
	private Map<String, TypeDefn> infrastructure = new HashMap<String, TypeDefn>();
  private Map<String, ResourceDefn> baseResources = new HashMap<String, ResourceDefn>();
  private Map<String, ResourceDefn> resources = new HashMap<String, ResourceDefn>();

  // indexes of above
  private Map<String, DefinedCode> knownResources = new HashMap<String, DefinedCode>();
  private List<TypeRef> knownTypes = new ArrayList<TypeRef>();
  private Map<String, ArrayList<String>> statusCodes = new HashMap<String, ArrayList<String>>();

  // access to raw resources - to be removed and replaced by worker context at some stage
  private Map<String, ValueSet> valuesets = new HashMap<String, ValueSet>();
  private Map<String, ConceptMap> conceptMaps = new HashMap<String, ConceptMap>();
  private Map<String, ValueSet> codeSystems = new HashMap<String, ValueSet>();
  private Map<String, ValueSet> extraValuesets = new HashMap<String, ValueSet>();
  private Set<String> styleExemptions = new HashSet<String>();
  
  private List<Compartment> compartments = new ArrayList<Compartment>();
  
  // Returns the root TypeDefn of a CompositeType or Resource,
	// excluding future Resources (as they don't have definitions yet).
	public TypeDefn getElementDefn(String name) throws Exception {
    
		TypeDefn root = null;
		if (types.containsKey(name))
			root = types.get(name);
		if (structures.containsKey(name))
			root = structures.get(name);
		if (infrastructure.containsKey(name))
			root = infrastructure.get(name);
    if (baseResources.containsKey(name))
      return baseResources.get(name).getRoot();
		if (resources.containsKey(name))
			root = resources.get(name).getRoot();
		if (root == null)
			throw new Exception("unable to find resource or composite type " + name);
		return root;
	}

  // Returns true if the root ElementDefn of a CompositeType or Resource can be found, 
  // excluding future Resources (as they don't have definitions yet).
  public boolean hasElementDefn(String name) {
    ElementDefn root = null;
    if (types.containsKey(name))
      root = types.get(name);
    if (structures.containsKey(name))
      root = structures.get(name);
    if (infrastructure.containsKey(name))
      root = infrastructure.get(name);
    if (baseResources.containsKey(name))
      root = baseResources.get(name).getRoot();
    if (resources.containsKey(name))
      root = resources.get(name).getRoot();
    return root != null;
  }

//	// Returns a list of Bindings as found on the "Bindings" tab in
//	// terminologies/bindings.xml and the "Binding" column on
//	// CompositeTypes and Resources.
//	public Map<String, BindingSpecification> getBindings() {
//		return bindings;
//	}
//
//
//	public BindingSpecification getBindingByName(String name) {
//		return bindings.get(name);
//	}
	
	// Returns all PrimitiveTypes (both imported and with a
	// restriction pattern as found in the primitives.xls
	// file on the "Imports" and "String Patterns" tab.
	public Map<String, DefinedCode> getPrimitives() {
		return primitives;
	}

	// List the CompositeTypes as found under [types] that aren't
	// ConstrainedTypes.
	public Map<String, TypeDefn> getTypes() {
		return types;
	}

	// List the CompositeTypes as found under [structures] that aren't
	// ConstrainedTypes.
	public Map<String, TypeDefn> getStructures() {
		return structures;
	}

	// List the CompositeTypes as found under [infrastructure] that aren't
	// ConstrainedTypes.
	public Map<String, TypeDefn> getInfrastructure() {
		return infrastructure;
	}

	// List of resources, excluding future resources
	public Map<String, ResourceDefn> getResources() {
		return resources;
	}
	

	public ResourceDefn getResourceByName(String name) throws Exception {
		ResourceDefn root = null;
		if (resources.containsKey(name))
			root = resources.get(name);
    if (root == null)
      root = baseResources.get(name);
		if (root == null)
			throw new Exception("unable to find resource '" + name+"'");
		return root;
	}

	public boolean hasResource(String name) {
		return resources.containsKey(name);
	}
	
	
	// List of all names of Resources (as code), including "future" resources
	// (but not special resources, as these aren't resources)
	public Map<String, DefinedCode> getKnownResources() {
		return knownResources;
	}

	// List of all CompositeTypes (constrained and unconstrained)
	// and PrimitiveTypes (both imported and with restrictions)
	public List<TypeRef> getKnownTypes() {
		return knownTypes;
	}

	public boolean hasType(String name) {
		for (TypeRef td : knownTypes) {
			if (td.getName().equals(name))
				return true;
		}

		return false;
	}

//  public BindingSpecification getBindingByReference(String ref, BindingSpecification other) {
//    for (BindingSpecification b : bindings.values()) {
//      if (ref.equals(b.getReference()) && other != b)
//        return b;
//    }
//    return null;
//  }
//  
//  public BindingSpecification getBindingByReference(String ref) {
//    for (BindingSpecification b : bindings.values()) {
//      if (ref.equals(b.getReference()))
//        return b;
//    }
//    return null;
//  }
//  
  public boolean dataTypeIsSharedInfo(String name)  {
    try {
      return hasElementDefn(name) && getElementDefn(name).typeCode().equals("SharedDefinition");
    } catch (Exception e) {
      e.printStackTrace();
      return false;
    }
  }

  public Map<String, ResourceDefn> getBaseResources() {
    return baseResources;
  }

  public Map<String, BindingSpecification> getCommonBindings() {
    return commonBindings;
  }

  private List<String> sortedNames;
  private List<String> vsFixups = new ArrayList<String>();
  private List<NamingSystem> namingSystems = new ArrayList<NamingSystem>();
  private Set<String> structuralPages = new HashSet<String>();
  private boolean loaded;
  
  public List<String> sortedResourceNames() {
    if (sortedNames == null) {
      sortedNames = new ArrayList<String>();
      sortedNames.addAll(getResources().keySet());
      Collections.sort(sortedNames);
    }
    return sortedNames;
  }

  public Map<String, ConceptMap> getConceptMaps() {
    return conceptMaps;
  }

  public Map<String, ValueSet> getValuesets() {
    return valuesets;
  }

  public Map<String, ValueSet> getCodeSystems() {
    return codeSystems;
  }

  public Map<String, ValueSet> getExtraValuesets() {
    return extraValuesets;
  }

  public boolean hasPrimitiveType(String name) {
    return primitives.containsKey(name);
  }
  
  public Map<String, ArrayList<String>> getStatusCodes() {
    return statusCodes;
  }


  public void checkContextValid(ExtensionContext contextType, String value, String context) throws Exception {
    if (contextType == ExtensionContext.DATATYPE) {
      if (value.equals("*") || value.equals("Any"))
        return;
      if (primitives.containsKey(value))
        return;
      String[] parts = value.split("\\.");
      if (hasType(parts[0]) && getElementByPath(parts, "check extension context") != null)
        return;
      
      throw new Error("The data type context '"+value+"' is not valid @ "+context);
      
    } else if (contextType == ExtensionContext.RESOURCE) {
      if (value.startsWith("@"))
        value = value.substring(1);
      if (value.equals("*") || value.equals("Any"))
        return;
      String[] parts = value.split("\\.");
      if (sortedResourceNames().contains(value))
        return;
      if (getElementByPath(parts, "check extension context") != null)
        return;
      
      throw new Error("The resource context '"+value+"' is not valid @ "+context);
    } else
    throw new Error("not checked yet @ "+context);
    
  }

  private Object getElementByPath(String[] parts, String purpose) throws Exception {
    ElementDefn e;
    try {
      e = getElementDefn(parts[0]);
    } catch (Exception e1) {
     return null;
    }
    int i = 1;
    while (e != null && i < parts.length) {
      e = e.getElementByName(parts[i], true, this, purpose);
      i++;
    }
    return e;
  }

  public Map<String, ValueSet> getBoundValueSets() {
    return boundValueSets;
  }

  public Set<BindingSpecification> getUnresolvedBindings() {
    return unresolvedBindings;
  }

  public List<BindingSpecification> getAllBindings() {
    return allBindings;
  }

  public boolean isResource(String name) {
    return hasResource(name);
  }

  public List<String> getVsFixups() {
    return vsFixups;
  }


  public List<NamingSystem> getNamingSystems() {
    return namingSystems;
  }

  public Set<String> getStructuralPages() {
    return structuralPages;
  }

  public Set<String> getStyleExemptions() {
    return styleExemptions;
  }

  public boolean isLoaded() {
    return loaded;
  }

  public void setLoaded(boolean loaded) {
    this.loaded = loaded;
  }


  public boolean hasConcreteResource(String name) {
    for (ResourceDefn rd : baseResources.values())
      if (!rd.isAbstract() && rd.getName().equals(name))
        return true;
    return resources.containsKey(name);
  }

  public boolean hasBaseType(String name) {
    if (name == null)
      return false;
    for (DefinedCode dc : primitives.values()) {
      if (/* dc instanceof PrimitiveType && */ dc.getCode().equals(name))
        return true;
    }
    return name.equals("xhtml") || types.containsKey(name) || structures.containsKey(name) || infrastructure.containsKey(name);
  }

  public boolean hasAbstractResource(String name) {
    for (ResourceDefn rd : baseResources.values())
      if (rd.isAbstract() && rd.getName().equals(name))
        return true;
    return false;
  }

  public String getVersion() {
    return version;
  }

  public void setVersion(String version) {
    this.version = version;
  }

  public DateTimeType getGenDate() {
    return genDate;
  }

  public void setGenDate(DateTimeType genDate) {
    this.genDate = genDate;
  }

  public List<Compartment> getCompartments() {
    return compartments;
  }
  
  public Compartment getCompartmentByName(String n) {
    for (Compartment c : compartments)
      if (c.getName().equals(n))
        return c;
    return null;
  }


}
