package org.fhir.delphi;

import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.convertors.VersionConvertor_10_30;
import org.hl7.fhir.convertors.VersionConvertor_10_40;
import org.hl7.fhir.convertors.VersionConvertor_30_40;
import org.hl7.fhir.convertors.conv10_30.OperationDefinition10_30;
import org.hl7.fhir.dstu2.formats.JsonParser;
import org.hl7.fhir.dstu2.formats.XmlParser;
import org.hl7.fhir.dstu2.model.Bundle;
import org.hl7.fhir.dstu2.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.dstu2.model.Conformance;
import org.hl7.fhir.dstu2.model.OperationDefinition;
import org.hl7.fhir.dstu2.model.Resource;
import org.hl7.fhir.dstu2.model.SearchParameter;
import org.hl7.fhir.dstu2.model.StructureDefinition;
import org.hl7.fhir.dstu2.model.StructureDefinition.StructureDefinitionKind;
import org.hl7.fhir.dstu2.model.ValueSet;
import org.hl7.fhir.dstu2.utils.ToolingExtensions;
import org.hl7.fhir.r4.model.CompartmentDefinition;
import org.hl7.fhir.utilities.Utilities;
import org.hl7.fhir.utilities.cache.NpmPackage;
import org.hl7.fhir.utilities.cache.PackageCacheManager;
import org.hl7.fhir.utilities.cache.ToolsVersion;

public class DefinitionsLoader2 {

  public Definitions loadDefinitions(String ver) throws Exception {
    PackageCacheManager pcm = new PackageCacheManager(true, ToolsVersion.TOOLS_VERSION);
    NpmPackage npm = pcm.loadPackage("hl7.fhir.core.gen", ver);
    
    List<StructureDefinition> sdl = new ArrayList<>();
    List<SearchParameter> spl = new ArrayList<>();
    List<OperationDefinition> odl = new ArrayList<>();
    Map<String, ValueSet> vsm = new HashMap<>();
    
    for (String name : npm.listResources("StructureDefinition", "SearchParameter", "ValueSet", "OperationDefinition", "CompartmentDefinition")) {
      Resource res = new JsonParser().parse(npm.load("package", name));
      
      if (res.fhirType().equals("StructureDefinition")) {
        StructureDefinition sd = (StructureDefinition) res;
        sdl.add(sd);
      }
      if (res.fhirType().equals("SearchParameter")) {
        SearchParameter sp = (SearchParameter) res;
        spl.add(sp);
      }
      if (res.fhirType().equals("OperationDefinition")) {
        OperationDefinition od = (OperationDefinition) res;
        odl.add(od);
      }
      if (res.fhirType().equals("ValueSet")) {
        ValueSet vs = (ValueSet) res;
        vsm.put(vs.getUrl(), vs);
      }        
    }


    Definitions def = new Definitions();
    def.setVersion(ver);
    for (StructureDefinition sd : sdl) {
      def.setGenDate(sd.getDateElement());
      if (!sd.hasConstrainedType() && (sd.getKind() == StructureDefinitionKind.DATATYPE)) { 
        System.out.println("dt: " +sd.getId());
        processType(def, sd, vsm);
      }
    }
    for (StructureDefinition sd : sdl) {
      if (!sd.hasConstrainedType() && (sd.getKind() == StructureDefinitionKind.RESOURCE)) {
        System.out.println("res: " +sd.getId());
        processResource(def, sd, vsm);
      }
    }
    for (OperationDefinition od : odl) {
      def.getOperations().add(OperationDefinition10_30.convertOperationDefinition(od));
    }
    
    for (SearchParameter sp : spl) {
      processSearchParam(def, sp);
    }

    return def;
  }

  private void processConformance(Definitions def, Conformance resource) {
    def.setVersion(resource.getFhirVersion());
    def.setGenDate(resource.getDateElement());
  }

  private static void processSearchParam(Definitions def, SearchParameter sp) throws Exception {
    SearchParameterDefn spd = new SearchParameterDefn().loadR2(sp.getCode(), sp.getDescription(), sp.getType(), sp.getXpathUsage(), sp.getTarget(), null);
    if (sp.hasBase())
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
