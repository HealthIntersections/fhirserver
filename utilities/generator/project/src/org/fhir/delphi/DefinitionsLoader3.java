package org.fhir.delphi;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.convertors.VersionConvertor_10_30;
import org.hl7.fhir.convertors.conv10_30.SearchParameter10_30;
import org.hl7.fhir.dstu3.formats.JsonParser;
import org.hl7.fhir.dstu3.model.CapabilityStatement;
import org.hl7.fhir.dstu3.model.CodeType;
import org.hl7.fhir.dstu3.model.CompartmentDefinition;
import org.hl7.fhir.dstu3.model.CompartmentDefinition.CompartmentDefinitionResourceComponent;
import org.hl7.fhir.dstu3.model.OperationDefinition;
import org.hl7.fhir.dstu3.model.Resource;
import org.hl7.fhir.dstu3.model.SearchParameter;
import org.hl7.fhir.dstu3.model.StringType;
import org.hl7.fhir.dstu3.model.StructureDefinition;
import org.hl7.fhir.dstu3.model.StructureDefinition.StructureDefinitionKind;
import org.hl7.fhir.dstu3.model.StructureDefinition.TypeDerivationRule;
import org.hl7.fhir.dstu3.model.ValueSet;
import org.hl7.fhir.dstu3.utils.ToolingExtensions;
import org.hl7.fhir.exceptions.FHIRException;
import org.hl7.fhir.utilities.Utilities;
import org.hl7.fhir.utilities.cache.FilesystemPackageCacheManager;
import org.hl7.fhir.utilities.cache.NpmPackage;
import org.hl7.fhir.utilities.cache.ToolsVersion;

public class DefinitionsLoader3 {
  
  private List<ResourceDefn> markList = new ArrayList<ResourceDefn>();
  
  public Definitions loadDefinitions(String ver) throws Exception {
    FilesystemPackageCacheManager pcm = new FilesystemPackageCacheManager(true, ToolsVersion.TOOLS_VERSION);
    NpmPackage npm = pcm.loadPackage("hl7.fhir.core.gen", ver);
    
    List<StructureDefinition> sdl = new ArrayList<>();
    List<SearchParameter> spl = new ArrayList<>();
    List<OperationDefinition> odl = new ArrayList<>();
    List<CompartmentDefinition> cdl = new ArrayList<>();
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
      if (res.fhirType().equals("CompartmentDefinition")) {
        CompartmentDefinition cd = (CompartmentDefinition) res;
        cdl.add(cd);
      }
      if (res.fhirType().equals("ValueSet")) {
        ValueSet vs = (ValueSet) res;
        vsm.put(vs.getUrl(), vs);
      }        
    }


    Definitions def = new Definitions();
    def.setVersion(ver);
    for (StructureDefinition sd : sdl) {
      def.setGenDate(VersionConvertor_10_30.convertDateTime(sd.getDateElement()));
      if (sd.getDerivation() != TypeDerivationRule.CONSTRAINT && (sd.getKind() == StructureDefinitionKind.PRIMITIVETYPE || sd.getKind() == StructureDefinitionKind.COMPLEXTYPE)) 
        processType(def, sd, vsm);
    }
    for (StructureDefinition sd : sdl) {
      if ((sd.getDerivation() != TypeDerivationRule.CONSTRAINT && (sd.getKind() == StructureDefinitionKind.RESOURCE)) || sd.getId().equals("MetadataResource")) {
        processResource(def, sd, vsm);
      }
    }
    for (OperationDefinition od : odl) {
      def.getOperations().add(od);
    }
    
    for (CompartmentDefinition cd : cdl) {
      processCompartmentDefinition(def, cd);
    }
    for (SearchParameter sp : spl) {
      processSearchParam(def, sp);
    }

    for (ResourceDefn rd : markList)
      mark(def, rd);
    return def;
  }

  private void mark(Definitions def, ResourceDefn rd) throws Exception {
    ResourceDefn p = def.getResourceByName(rd.getRoot().typeCode());
    for (ElementDefn e : rd.getRoot().getElements()) {
      if (hasName(p, e.getName()))
        e.setInherited(true);
    }
  }

  private boolean hasName(ResourceDefn p, String name) {
    for (ElementDefn e : p.getRoot().getElements())
      if (e.getName().equals(name))
        return true;
    return false;
  }

  private void processCompartmentDefinition(Definitions def, CompartmentDefinition cd) throws Exception {
    Compartment c = new Compartment();
    c.setName(cd.getCode().toCode());
    c.setTitle(cd.getName());
    c.setDescription(cd.getDescription());
    for (CompartmentDefinitionResourceComponent r : cd.getResource()) {
      ResourceDefn rd = def.getResourceByName(r.getCode());
      List<String> list = new ArrayList<>();
      for (StringType s : r.getParam())
        list.add(s.asStringValue());
      c.getResources().put(rd, list);
    }
    def.getCompartments().add(c);
  }
   
  private void processCapabilityStatement(Definitions def, CapabilityStatement resource) throws FHIRException {
    def.setVersion(resource.getFhirVersion());
    def.setGenDate(VersionConvertor_10_30.convertDateTime(resource.getDateElement()));
  }

  private void processSearchParam(Definitions def, SearchParameter sp) throws Exception {
    for (CodeType ct : sp.getBase()) {
      SearchParameterDefn spd = new SearchParameterDefn().loadR3(sp.getCode(), pickDescription(sp.getDescription(), ct.asStringValue()), VersionConvertor_10_30.convertSearchParamType(sp.getTypeElement()).getValue(),                                                                                                                                                                 
          sp.getTarget(), SearchParameter10_30.convertXPathUsageType(sp.getXpathUsage()), sp.getExpression());
      def.getResourceByName(ct.asStringValue()).getSearchParams().put(spd.getCode(), spd);
    }
  }

  
  private String pickDescription(String description, String rn) {
    String[] list = description.split("\\* ");
    for (String entry : list) {
      String[] parts = entry.split("\\:");
      if (parts.length == 2 && parts[0].contains(rn))
        return parts[1].trim();
    }
    return description;
  }

  private void processResource(Definitions def, StructureDefinition sd, Map<String, ValueSet> vsmap) throws Exception {
    ResourceDefn rd = new ResourceDefn();
    rd.setName(sd.getName());
    rd.setAbstract(sd.getAbstract());
    rd.setDisplay(sd.getTitle());
    rd.setDefinition(sd.getDifferential().getElement().get(0).getDefinition());
    if (sd.getKind() == StructureDefinitionKind.LOGICAL) {
      rd.setAbstract(true);
      rd.setSpecial(true);
    }
    TypeDefn type = new TypeDefn();
    type.loadFrom(sd.getDifferential().getElement().get(0), sd, vsmap);
    rd.setRoot(type);
    if (sd.hasBaseDefinition()) {
      if (ToolingExtensions.hasExtension(sd.getBaseDefinitionElement(), ToolingExtensions.EXT_CODE_GENERATION_PARENT)) { 
        markList.add(rd);
        type.getTypes().add(new TypeRef(ToolingExtensions.readStringExtension(sd.getBaseDefinitionElement(), ToolingExtensions.EXT_CODE_GENERATION_PARENT)));
      } else
        type.getTypes().add(new TypeRef(sd.getBaseDefinition().substring(40)));
    }
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
    } else if (sd.getDerivation() != TypeDerivationRule.CONSTRAINT) {
      TypeDefn type = new TypeDefn();
      type.loadFrom(sd.getDifferential().getElement().get(0), sd, vsmap);
      for (int i = 1; i < sd.getDifferential().getElement().size(); i++) { 
        ElementDefn ed = new ElementDefn();
        ed.loadFrom(sd.getDifferential().getElement().get(i), sd, vsmap);
        type.getElementForPath(parentPath(ed.getPath()), def, "build", true).getElements().add(ed);
        
        // work around a signficant change in STU3
        if (ed.getPath().equals("Element.id")) {
          ed.getTypes().clear();
          ed.getTypes().add(new TypeRef().setName("id"));
        }
      }
      Map<String, TypeDefn> map = getTypesMapForName(def, type.getName());
      type.getTypes().clear();
      if (sd.hasBaseDefinition() && sd.getBaseDefinition().startsWith("http://hl7.org/fhir/StructureDefinition/"))
        type.getTypes().add(new TypeRef(sd.getBaseDefinition().substring(40)));
      else if (map == def.getInfrastructure() && !Utilities.existsInList(sd.getId(), "Narrative", "Extension")) {
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
    System.out.println("type "+sd.getId());
    if (!sd.getBaseDefinition().equals("http://hl7.org/fhir/StructureDefinition/Element")) {
      DefinedStringPattern dsp = new DefinedStringPattern();
      dsp.setCode(sd.getId());
      dsp.setDefinition(removeprefix(sd.getDescription()));
      dsp.setComment(sd.getSnapshot().getElement().get(0).getComment());
      dsp.setSchema(ToolingExtensions.readStringExtension(sd.getSnapshot().getElement().get(2).getType().get(0).getCodeElement(), ToolingExtensions.EXT_XML_TYPE));
      dsp.setJsonType(ToolingExtensions.readStringExtension(sd.getSnapshot().getElement().get(2).getType().get(0).getCodeElement(), ToolingExtensions.EXT_JSON_TYPE));
      dsp.setBase(sd.getBaseDefinition().substring(40));
      return dsp;
    } else {
      PrimitiveType pt = new PrimitiveType();
      pt.setCode(sd.getId());
      pt.setDefinition(removeprefix(sd.getDescription()));
      pt.setComment(sd.getSnapshot().getElement().get(0).getComment());
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
    return sd.getKind() == StructureDefinitionKind.PRIMITIVETYPE || Utilities.existsInList(sd.getId(), "boolean", "integer", "decimal", "base64Binary", "instant", "string", "uri", "date", "dateTime", "time", "code", "oid", "uuid", "id", "unsignedInt", "positiveInt", "markdown");
  }


}
