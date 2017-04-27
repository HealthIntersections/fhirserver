package org.fhir.delphi;

import java.io.FileInputStream;
import java.util.HashMap;
import java.util.Map;

import org.hl7.fhir.convertors.VersionConvertor_10_30;
import org.hl7.fhir.dstu2.formats.XmlParser;
import org.hl7.fhir.dstu2.model.Bundle;
import org.hl7.fhir.dstu2.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.dstu2.model.Conformance;
import org.hl7.fhir.dstu2.model.OperationDefinition;
import org.hl7.fhir.dstu2.model.SearchParameter;
import org.hl7.fhir.dstu2.model.StructureDefinition;
import org.hl7.fhir.dstu2.model.ValueSet;
import org.hl7.fhir.dstu2.utils.ToolingExtensions;
import org.hl7.fhir.utilities.Utilities;

public class DefinitionsLoader2 {

  public Definitions loadDefinitions(String src) throws Exception {
    Bundle types = (Bundle) new XmlParser().parse(new FileInputStream(Utilities.path(src, "profiles-types.xml")));
    Bundle resources = (Bundle) new XmlParser().parse(new FileInputStream(Utilities.path(src, "profiles-resources.xml")));
    Bundle searchParams = (Bundle) new XmlParser().parse(new FileInputStream(Utilities.path(src, "search-parameters.xml")));
    Bundle valuesets = (Bundle) new XmlParser().parse(new FileInputStream(Utilities.path(src, "expansions.xml")));
    Map<String, ValueSet> vsmap = new HashMap<String, ValueSet>();
    for (BundleEntryComponent entry : valuesets.getEntry()) {
      vsmap.put(((ValueSet) entry.getResource()).getUrl(), (ValueSet) entry.getResource());
    }

    Definitions def = new Definitions();
    for (BundleEntryComponent entry : types.getEntry()) {
      if (entry.getResource() instanceof StructureDefinition) 
        processType(def, (StructureDefinition) entry.getResource(), vsmap);
      else
        System.out.println("unhandled entry in types: "+entry.getResource().fhirType());
    }
    for (BundleEntryComponent entry : resources.getEntry()) {
      if (entry.getResource() instanceof StructureDefinition) 
        processResource(def, (StructureDefinition) entry.getResource(), vsmap);
      else if (entry.getResource() instanceof Conformance) 
        processConformance(def, (Conformance) entry.getResource());
      else if (entry.getResource() instanceof OperationDefinition)
        def.getOperations().add(new VersionConvertor_10_30(null).convertOperationDefinition((OperationDefinition) entry.getResource())); 
      else
        System.out.println("unhandled entry in resources: "+entry.getResource().fhirType());
    }
    for (BundleEntryComponent entry : searchParams.getEntry()) {
      if (entry.getResource() instanceof SearchParameter) 
        processSearchParam(def, (SearchParameter) entry.getResource());
      else 
        System.out.println("unhandled entry in search parameters: "+entry.getResource().fhirType());
    }
    return def;
  }

  private void processConformance(Definitions def, Conformance resource) {
    def.setVersion(resource.getFhirVersion());
    def.setGenDate(resource.getDateElement());
  }

  private static void processSearchParam(Definitions def, SearchParameter sp) throws Exception {
    SearchParameterDefn spd = new SearchParameterDefn().loadR2(sp.getCode(), sp.getDescription(), sp.getType(), sp.getXpathUsage(), sp.getTarget(), null);
    def.getResourceByName(sp.getBase()).getSearchParams().put(spd.getCode(), spd);
  }

  private static void processResource(Definitions def, StructureDefinition sd, Map<String, ValueSet> vsmap) throws Exception {
    ResourceDefn rd = new ResourceDefn();
    rd.setName(sd.getName());
    rd.setAbstract(sd.getAbstract());
    rd.setDisplay(sd.getDisplay());
    rd.setDefinition(sd.getDifferential().getElement().get(0).getDefinition());
    TypeDefn type = new TypeDefn();
    type.loadFrom(sd.getDifferential().getElement().get(0), sd, vsmap);
    rd.setRoot(type);
    for (int i = 1; i < sd.getDifferential().getElement().size(); i++) { 
      ElementDefn ed = new ElementDefn();
      ed.loadFrom(sd.getDifferential().getElement().get(i), sd, vsmap);
      type.getElementForPath(parentPath(ed.getPath()), def, "build", true).getElements().add(ed);
    }
    if (rd.isAbstract() || sd.getId().equals("Parameters"))
      def.getBaseResources().put(rd.getName(), rd);
    else
      def.getResources().put(rd.getName(), rd);
  }

  private static String parentPath(String path) {
    return path.substring(0, path.lastIndexOf("."));
  }

  private static void processType(Definitions def, StructureDefinition sd, Map<String, ValueSet> vsmap) throws Exception {
    if (isPrimitive(sd)) {
      def.getPrimitives().put(sd.getId(), makePrimitive(sd));
    } else if (!sd.hasConstrainedType()) {
      TypeDefn type = new TypeDefn();
      type.loadFrom(sd.getDifferential().getElement().get(0), sd, vsmap);
      for (int i = 1; i < sd.getDifferential().getElement().size(); i++) { 
        ElementDefn ed = new ElementDefn();
        ed.loadFrom(sd.getDifferential().getElement().get(i), sd, vsmap);
        type.getElementForPath(parentPath(ed.getPath()), def, "build", true).getElements().add(ed);
      }
      Map<String, TypeDefn> map = getTypesMapForName(def, type.getName());
      type.getTypes().clear();
      if (map == def.getInfrastructure() && !Utilities.existsInList(sd.getId(), "Narrative", "Extension")) {
        if (!sd.getId().equals("Element"))
         type.getTypes().add(new TypeRef("Element"));
      } else if (Utilities.existsInList(sd.getId(), "Meta")) {
        type.getTypes().add(new TypeRef("Element"));        
      } else if (map == def.getStructures() && !Utilities.existsInList(sd.getId(), "ElementDefinition", "Meta"))
        type.getTypes().add(new TypeRef("Structure"));
      else
        type.getTypes().add(new TypeRef("Type"));
      map.put(type.getName(), type);
    }
  }

  private static Map<String, TypeDefn> getTypesMapForName(Definitions def, String name) {
    if (Utilities.existsInList(name, "BackboneElement", "Element", "Extension", "Narrative"))
      return def.getInfrastructure();
    if (Utilities.existsInList(name, "Timing", "Meta", "HumanName", "ElementDefinition", "ContactPoint", "Address"))
      return def.getStructures();
    return def.getTypes();
  }

  private static DefinedCode makePrimitive(StructureDefinition sd) {
    if (sd.hasConstrainedType()) {
      DefinedStringPattern dsp = new DefinedStringPattern();
      dsp.setCode(sd.getId());
      dsp.setDefinition(removeprefix(sd.getDescription()));
      dsp.setComment(sd.getSnapshot().getElement().get(0).getComments());
      dsp.setSchema(ToolingExtensions.readStringExtension(sd.getSnapshot().getElement().get(2).getType().get(0).getCodeElement(), ToolingExtensions.EXT_XML_TYPE));
      dsp.setJsonType(ToolingExtensions.readStringExtension(sd.getSnapshot().getElement().get(2).getType().get(0).getCodeElement(), ToolingExtensions.EXT_JSON_TYPE));
      dsp.setBase(sd.getConstrainedType());
      return dsp;
    } else {
      PrimitiveType pt = new PrimitiveType();
      pt.setCode(sd.getId());
      pt.setDefinition(removeprefix(sd.getDescription()));
      pt.setComment(sd.getSnapshot().getElement().get(0).getComments());
      pt.setSchemaType(ToolingExtensions.readStringExtension(sd.getSnapshot().getElement().get(3).getType().get(0).getCodeElement(), ToolingExtensions.EXT_XML_TYPE));
      pt.setJsonType(ToolingExtensions.readStringExtension(sd.getSnapshot().getElement().get(3).getType().get(0).getCodeElement(), ToolingExtensions.EXT_JSON_TYPE));
      if (!pt.getSchemaType().startsWith("xs:"))
        pt.setSchemaType("xs:"+pt.getSchemaType());
      return pt;
    }
  }

  private static String removeprefix(String description) {
    int i = description.indexOf("Type: ");
    if (i < 0)
      i = description.indexOf("type: ");
    if (i > 0)
      return description.substring(i+6);
    return description;
  }

  private static boolean isPrimitive(StructureDefinition sd) {
    return Utilities.existsInList(sd.getId(), "boolean", "integer", "decimal", "base64Binary", "instant", "string", "uri", "date", "dateTime", "time", "code", "oid", "uuid", "id", "unsignedInt", "positiveInt", "markdown");
  }


}
