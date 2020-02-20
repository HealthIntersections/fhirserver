package org.fhir.pascal.generator.engine;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.fhir.pascal.generator.analysis.Analyser;
import org.fhir.pascal.generator.analysis.Analysis;
import org.fhir.pascal.generator.codegen.ConstantsGenerator;
import org.fhir.pascal.generator.codegen.EnumsGenerator;
import org.fhir.pascal.generator.codegen.FactoryGenerator;
import org.fhir.pascal.generator.codegen.FormatsGenerator;
import org.fhir.pascal.generator.codegen.IncGenerator;
import org.fhir.pascal.generator.codegen.IndexInfoGenerator;
import org.fhir.pascal.generator.codegen.JavascriptGenerator;
import org.fhir.pascal.generator.codegen.OperationsGenerator;
import org.fhir.pascal.generator.codegen.ResourcesGenerator;
import org.fhir.pascal.generator.codegen.TypesGenerator;
import org.hl7.fhir.r5.model.ElementDefinition;
import org.hl7.fhir.r5.model.Enumerations.BindingStrength;
import org.hl7.fhir.r5.model.OperationDefinition;
import org.hl7.fhir.r5.model.SearchParameter;
import org.hl7.fhir.r5.model.StructureDefinition;
import org.hl7.fhir.r5.model.StructureDefinition.StructureDefinitionKind;
import org.hl7.fhir.r5.model.StructureDefinition.TypeDerivationRule;
import org.hl7.fhir.r5.model.ValueSet;
import org.hl7.fhir.utilities.Utilities;
import org.hl7.fhir.utilities.cache.NpmPackage;
import org.hl7.fhir.utilities.cache.PackageCacheManager;
import org.hl7.fhir.utilities.cache.ToolsVersion;

public class PascalGenerator {
  
  public static void main(String[] args) throws Exception {
    System.out.println("FHIR Pascal Code Generator");
    if (args.length != 2) {
      System.out.println("Usage: invoke with 2 command line parameters to generate HAPI R5 code");
      System.out.println("1: fhir version to generate from (e.g. 4.2.0 or 'current'");
      System.out.println("2: The root path of the local copy of the github repo");
    } else {
      String version = args[0];
      String path = args[1];
      new PascalGenerator().generate(version, path);
    }
  }

  private void generate(String version, String path) throws Exception {
    long start = System.currentTimeMillis();
    Date date = new Date();
    
    String tp = Utilities.path(path, "utilities", "generator", "project", "templates");
    String sp = Utilities.path(path, "library", "r5");
    String dest = Utilities.path(path, "library", "r5gen");
    System.out.println("Load templates from "+tp);
    System.out.println("Master source in "+sp);
    System.out.println("Produce source in  "+dest);
    Configuration config = new Configuration(tp, sp, dest);
    
    PackageCacheManager pcm = new PackageCacheManager(true, ToolsVersion.TOOLS_VERSION);
    System.out.println("Cache: "+pcm.getFolder());
    System.out.println("Load hl7.fhir.r5.core");
    NpmPackage npm = pcm.loadPackage("hl7.fhir.r5.core", version);
    Definitions master = DefinitionsLoader.load(npm); 
    
    markValueSets(master, config);
    
    System.out.println("Load hl7.fhir.r5.expansions");
    Definitions expansions = DefinitionsLoader.load(pcm.loadPackage("hl7.fhir.r5.expansions", version));
    
    System.out.println("Process Expansions");
    updateExpansions(master, expansions);
    
    System.out.println("Prepare");   
    ConstantsGenerator cgen = new ConstantsGenerator(master, config, date, npm.version());
    FactoryGenerator fgen = new FactoryGenerator(master, config, date, npm.version());
    IncGenerator igen = new IncGenerator(master, config, date, npm.version());
    IndexInfoGenerator iigen = new IndexInfoGenerator(master, config, date, npm.version());
    JavascriptGenerator jsgen = new JavascriptGenerator(master, config, date, npm.version());
    OperationsGenerator ogen = new OperationsGenerator(master, config, date, npm.version());
    ResourcesGenerator rgen = new ResourcesGenerator(master, config, date, npm.version());
    TypesGenerator tgen = new TypesGenerator(master, config, date, npm.version());
    FormatsGenerator fmtgen = new FormatsGenerator(master, config, date, npm.version());
    
    System.out.println("Generate Model");   
    
    // process abstracts in particular order
    processType(config, master, fgen, jsgen, tgen, fmtgen, master.getType("Element"));
    processType(config, master, fgen, jsgen, tgen, fmtgen, master.getType("BackboneElement"));
    processType(config, master, fgen, jsgen, tgen, fmtgen, master.getType("DataType"));
    processType(config, master, fgen, jsgen, tgen, fmtgen, master.getType("BackboneType"));
    processType(config, master, fgen, jsgen, tgen, fmtgen, master.getType("PrimitiveType"));
    
    for (StructureDefinition sd : master.getStructures().getList()) {
      if (sd.getDerivation() == TypeDerivationRule.SPECIALIZATION && sd.getKind() == StructureDefinitionKind.COMPLEXTYPE) {
        if (!Utilities.existsInList(sd.getName(), "Base", "PrimitiveType") && !sd.getName().contains(".") && !sd.getAbstract() && !sd.getAbstract() && "http://hl7.org/fhir/StructureDefinition/DataType".equals(sd.getBaseDefinition())) {
          processType(config, master, fgen, jsgen, tgen, fmtgen, sd);

        }
      }
    }
    for (StructureDefinition sd : master.getStructures().getList()) {
      if (sd.getDerivation() == TypeDerivationRule.SPECIALIZATION && sd.getKind() == StructureDefinitionKind.COMPLEXTYPE) {
        if (!Utilities.existsInList(sd.getName(), "Base", "PrimitiveType") && !sd.getName().contains(".") && !sd.getAbstract() && !sd.getAbstract() && !"http://hl7.org/fhir/StructureDefinition/DataType".equals(sd.getBaseDefinition())) {
          processType(config, master, fgen, jsgen, tgen, fmtgen, sd);

        }
      }
    }

    // process abstracts in particular order
    processResource(config, master, cgen, fgen, igen, iigen, jsgen, rgen, fmtgen, master.getType("Resource"));
    processResource(config, master, cgen, fgen, igen, iigen, jsgen, rgen, fmtgen, master.getType("DomainResource"));
    processResource(config, master, cgen, fgen, igen, iigen, jsgen, rgen, fmtgen, master.getType("CanonicalResource"));
    processResource(config, master, cgen, fgen, igen, iigen, jsgen, rgen, fmtgen, master.getType("MetadataResource"));

    for (StructureDefinition sd : master.getStructures().getList()) {
      if (sd.getDerivation() == TypeDerivationRule.SPECIALIZATION && sd.getKind() == StructureDefinitionKind.RESOURCE) {
        if (!Utilities.existsInList(sd.getName(), "Base", "PrimitiveType") && !sd.getName().contains(".") && !sd.getAbstract()) {
          processResource(config, master, cgen, fgen, igen, iigen, jsgen, rgen, fmtgen, sd);
        }
      }
    }

    System.out.println(" .. Operations");
    for (OperationDefinition od : master.getOperations().getSortedList()) {
      ogen.genOperation(od);
    }
    System.out.println(" .. Search Parameters");
    for (SearchParameter spp : master.getSearchParams().getSortedList()) {
      cgen.seeSearchParam(spp);
    }
    System.out.println(" .. Enumerations");
    EnumsGenerator egen = new EnumsGenerator(master, config, date, npm.version());
    egen.genEnums();
    egen.generate(Utilities.path(dest,  "FHIR.R5.Enums.pas"));
        
    System.out.println("Finish");   
    cgen.generate(Utilities.path(dest, "FHIR.R5.Constants.pas"));
    fgen.generate(Utilities.path(dest, "FHIR.R5.Factory.pas"));
    igen.generate(Utilities.path(dest, "FHIR.R5.inc"));
    iigen.generate(Utilities.path(dest, "FHIR.R5.IndexInfo.pas"));
    jsgen.generate(Utilities.path(dest, "FHIR.R5.Javascript.pas"));
    ogen.generate(Utilities.path(dest, "FHIR.R5.Operations.pas"));
    rgen.generate(Utilities.path(dest, "FHIR.R5.Resources.pas"));
    tgen.generate(Utilities.path(dest,  "FHIR.R5.Types.pas"));
    fmtgen.generateJson(Utilities.path(dest, "FHIR.R5.Json.pas"));
    fmtgen.generateXml(Utilities.path(dest,  "FHIR.R5.Xml.pas"));
    fmtgen.generateTurtle(Utilities.path(dest,  "FHIR.R5.Turtle.pas"));

    System.out.println("Done ("+Long.toString(System.currentTimeMillis()-start)+"ms)");   
    
  }

  public void processResource(Configuration config, Definitions master, ConstantsGenerator cgen, FactoryGenerator fgen, IncGenerator igen,
      IndexInfoGenerator iigen, JavascriptGenerator jsgen, ResourcesGenerator rgen, FormatsGenerator fmtgen,
      StructureDefinition sd) throws Exception {
    String name = javaName(sd.getName());

    System.out.println(" .. "+name);
    Analyser jca = new Analyser(master, config);
    Analysis analysis = jca.analyse(sd);
    
    rgen.seeResource(analysis);
    cgen.seeResource(analysis);
    fgen.seeResource(analysis);
    igen.seeResource(analysis);
    iigen.seeResource(analysis);
    jsgen.seeResource(analysis);
    fmtgen.seeResource(analysis);
  }

  public void processType(Configuration config, Definitions master, FactoryGenerator fgen, JavascriptGenerator jsgen, 
      TypesGenerator tgen, FormatsGenerator fmtgen, StructureDefinition sd) throws Exception {
    String name = javaName(sd.getName());
    System.out.println(" .. "+name);
    Analyser jca = new Analyser(master, config);
    Analysis analysis = jca.analyse(sd);
    
    tgen.seeType(analysis); 
    fgen.seeType(analysis); 
    fmtgen.seeType(analysis);
    jsgen.seeType(analysis);
  }

  @SuppressWarnings("unchecked")
  private void markValueSets(Definitions defns, Configuration config) {
    for (StructureDefinition sd : defns.getStructures().getList()) {
      if (sd.getDerivation() == TypeDerivationRule.SPECIALIZATION && sd.getKind() != StructureDefinitionKind.PRIMITIVETYPE && !sd.getName().contains(".")) {
        for (ElementDefinition ed : sd.getSnapshot().getElement()) {
          if (ed.hasBinding() && ed.getBinding().hasValueSet() && ed.getBinding().getStrength() == BindingStrength.REQUIRED) {
            ValueSet vs = defns.getValuesets().get(ed.getBinding().getValueSet());
            if (vs != null) {
              if (!vs.hasUserData("usages")) {
                vs.setUserData("usages", new ArrayList<>());
              }
              List<String> list = (List<String>) vs.getUserData("usages");
              if (!list.contains(sd.getName())) {
                list.add(sd.getName());
              }
            }
          }
        }
      }
    }

    for (ValueSet vs : defns.getValuesets().getList()) {
      List<String> list = (List<String>) vs.getUserData("usages");
      boolean shared = false;
      if (list != null && list.size() > 1) {
        shared = true;
      }
//      if (config.getIni().hasProperty("shared", vs.getUrl())) {
//        shared = config.getIni().getBooleanProperty("shared", vs.getUrl());
//      }
      if (shared) {
        vs.setUserData("shared", true);
      }
    }
  }

  private String javaName(String name) {
    return "List".equals(name) ? "ListResource" : name;
  }

  private void updateExpansions(Definitions master, Definitions expansions) {
    for (ValueSet vs: master.getValuesets().getList()) {
      ValueSet vse = expansions.getValuesets().get(vs.getUrl());
      if (vse != null) {
        vs.setUserData("expansion", vse);
      }
    }    
  }




}
