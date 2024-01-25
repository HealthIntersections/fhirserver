package org.fhir.pascal.generator.codegen;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.fhir.pascal.generator.analysis.Analysis;
import org.fhir.pascal.generator.analysis.EnumInfo;
import org.fhir.pascal.generator.analysis.TypeInfo;
import org.fhir.pascal.generator.engine.Configuration;
import org.fhir.pascal.generator.engine.Definitions;
import org.hl7.fhir.r5.model.ElementDefinition;
import org.hl7.fhir.r5.model.ElementDefinition.PropertyRepresentation;
import org.hl7.fhir.r5.model.ElementDefinition.TypeRefComponent;
import org.hl7.fhir.utilities.Utilities;

public abstract class ClassGenerator extends BaseGenerator {

  public enum ClassGeneratorCategory {
    Type, Resource, Component;
  }

  private List<String> lists = new ArrayList<String>();
  private List<String> types = new ArrayList<String>();
  String rId = "5";

  public ClassGenerator(Definitions definitions, Configuration config, String version, Date genDate) throws UnsupportedEncodingException {
    super(definitions, config, version, genDate);
  }

  protected String generateType(Analysis analysis, TypeInfo ti, ClassGeneratorCategory category, StringBuilder fwds, StringBuilder intf, StringBuilder impl) throws Exception {
    String tn = ti.getName();

    String tj = tn.substring(5);

    StringBuilder def = new StringBuilder();
    StringBuilder defPriv1 = new StringBuilder();
    StringBuilder fields = new StringBuilder();
    StringBuilder sizers = new StringBuilder();
    StringBuilder defPriv2 = new StringBuilder();
    StringBuilder defPub = new StringBuilder();
    StringBuilder impli = new StringBuilder();
    StringBuilder create = new StringBuilder();
    StringBuilder destroy = new StringBuilder();
    StringBuilder assign = new StringBuilder();
    LineLimitedStringBuilder empty = new LineLimitedStringBuilder(36, 6);
    StringBuilder getkids = new StringBuilder();
    StringBuilder getkidsvars = new StringBuilder();
    StringBuilder getprops = new StringBuilder();
    StringBuilder getpropsvars = new StringBuilder();
    StringBuilder setprops = new StringBuilder();
    StringBuilder insprops = new StringBuilder();
    StringBuilder makeprops = new StringBuilder();
    StringBuilder propTypes = new StringBuilder();
    StringBuilder delprops = new StringBuilder();
    StringBuilder replprops = new StringBuilder();
    StringBuilder reorderprops = new StringBuilder();

    def.append("  // "+makeDocoSafe(ti.getDefn().getDefinition(), "// ")+"\r\n");
    def.append("  "+tn+" = class (TFhir"+ti.getAncestorName()+")\r\n");
    types.add(tn);
    
    for (ElementDefinition c : ti.getChildren()) {
      generateField(analysis, ti, c, getInheritedElement(analysis, ti, c), defPriv1, defPriv2, defPub, impli, create, destroy, assign, empty, getkids, getkidsvars, getprops, getpropsvars, setprops, insprops, makeprops, propTypes, delprops, replprops, reorderprops, tn, "", category, ti.getName().equals("Extension"), tj, fields, sizers);
    }

    if (config.hasTemplate(tn+".private")) {
      def.append("  private\r\n");
      def.append(config.getTemplate(tn+".private"));
    } 
    def.append("  protected\r\n");
    def.append(defPriv1.toString());
    def.append(defPriv2.toString());
    def.append("  \r\n");
    if (category == ClassGeneratorCategory.Resource && !analysis.isAbstract()) {
      def.append("    function GetResourceType : TFhirResourceType; override;\r\n");      
    }
    if (ti.hasChildren()) {
      def.append("    procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;\r\n");
      def.append("    procedure ListProperties(oList : "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues : Boolean); override;\r\n");
      def.append("    procedure listFieldsInOrder(fields : TStringList); override;\r\n");
      def.append("    function sizeInBytesV(magic : integer) : cardinal; override;\r\n");
    }
    if (config.hasTemplate(tn+".protected")) {
      def.append(config.getTemplate(tn+".protected"));
    }
    def.append("  public\r\n");
    if (ti.hasChildren()) {
      def.append("    constructor Create; override;\r\n");
      def.append("    destructor Destroy; override;\r\n");
      def.append("    procedure Assign(oSource : TFslObject); override;\r\n");
    }
    def.append("    function Link : "+tn+"; overload;\r\n");
    def.append("    function Clone : "+tn+"; overload;\r\n");
    if (ti.hasChildren()) {
      def.append("    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;\r\n");
      def.append("    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;\r\n");
      def.append("    function createPropertyValue(propName : string) : TFHIRObject; override;\r\n");
      def.append("    function getTypesForProperty(propName : string): String; override;\r\n");
      def.append("    procedure deleteProperty(propName : string; value : TFHIRObject); override;\r\n");
      def.append("    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;\r\n");
      def.append("    procedure reorderProperty(propName : string; source, destination : integer); override;\r\n");
    }
    def.append("    function fhirType : string; override;\r\n");
    if (ti.hasChildren()) {
      def.append("    function Equals(other : TObject) : boolean; override;\r\n");
      def.append("    function isEmpty : boolean; override;\r\n");
    }
    if (config.hasTemplate(tn+".public")) {
      def.append(config.getTemplate(tn+".public"));
    }
    def.append("  {$IFNDEF FPC}published{$ENDIF}\r\n");
    def.append(defPub.toString());
    if (config.hasTemplate(tn+".published")) {
      def.append(config.getTemplate(tn+".published"));
    }
    def.append("  end;\r\n");
    def.append("\r\n");
    StringBuilder impl2 = new StringBuilder();
    impl2.append("{ "+tn+" }\r\n\r\n");
    if (ti.hasChildren()) {
      impl2.append("constructor "+tn+".Create;\r\n");
      impl2.append("begin\r\n");
      impl2.append("  inherited;\r\n");
      impl2.append(create.toString());
      impl2.append("end;\r\n\r\n");

      impl2.append("destructor "+tn+".Destroy;\r\n");
      impl2.append("begin\r\n");
      impl2.append(destroy.toString());
      impl2.append("  inherited;\r\n");
      impl2.append("end;\r\n\r\n");

      impl2.append("procedure "+tn+".Assign(oSource : TFslObject);\r\n");
      impl2.append("begin\r\n");
      impl2.append("  inherited;\r\n");
      impl2.append(assign.toString());
      impl2.append("end;\r\n\r\n");
      
      if (category == ClassGeneratorCategory.Resource && !analysis.isAbstract()) {
        impl2.append("function "+tn+".GetResourceType : TFhirResourceType;\r\n");
        impl2.append("begin\r\n");
        impl2.append("  result := frt"+analysis.getName()+";\r\n");
        impl2.append("end;\r\n");
        impl2.append("\r\n");
      }
      
      impl2.append("procedure "+tn+".GetChildrenByName(child_name : string; list : TFHIRSelectionList);\r\n");
      if (getkidsvars.length() > 0) {
        impl2.append("var\r\n");
        impl2.append(getkidsvars.toString());
      }
      impl2.append("begin\r\n");
      impl2.append("  inherited;\r\n");
      impl2.append(getkids.toString());
      impl2.append("end;\r\n\r\n");
      impl2.append("procedure "+tn+".ListProperties(oList: "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues: Boolean);\r\n");
      if (getpropsvars.length() > 0) {
        impl2.append("var\r\n  prop : TFHIRProperty;\r\n");      
        impl2.append(getpropsvars.toString());
      }
      impl2.append("begin\r\n");
      impl2.append("  inherited;\r\n");
      impl2.append(getprops.toString());
      impl2.append("end;\r\n\r\n");
      impl2.append("function "+tn+".setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;\r\n");
      impl2.append("begin\r\n");
      impl2.append("  "+(setprops == null || setprops.length() < 7 ? "??"  : setprops.toString().substring(7)));
      impl2.append("  else result := inherited setProperty(propName, propValue);\r\n");
      impl2.append("end;\r\n\r\n");
      impl2.append("procedure "+tn+".insertProperty(propName: string; propValue: TFHIRObject; index : integer);\r\n");
      impl2.append("begin\r\n");
      if (insprops.length() > 7) {
        impl2.append("  "+insprops.toString().substring(7));
        impl2.append("  else inherited;\r\n");
      } else {
        impl2.append("  inherited;\r\n");
      }
      impl2.append("end;\r\n\r\n");
      impl2.append("function "+tn+".createPropertyValue(propName : string) : TFHIRObject;\r\n");
      impl2.append("begin\r\n");
      if (makeprops.length() > 7) {
        impl2.append("  "+makeprops.toString().substring(7));
        impl2.append("  else result := inherited createPropertyValue(propName);\r\n");
      } else
        impl2.append("  result := inherited createPropertyValue(propName);\r\n");
      impl2.append("end;\r\n\r\n");
      impl2.append("function "+tn+".getTypesForProperty(propName: string) : String;\r\n");
      impl2.append("begin\r\n");
      if (propTypes.length() > 7) {
        impl2.append("  "+propTypes.toString().substring(7));
        impl2.append("  else result := inherited getTypesForProperty(propName);\r\n");
      } else
        impl2.append("  result := inherited getTypesForProperty(propName);\r\n");
      impl2.append("end;\r\n\r\n");
      impl2.append("procedure "+tn+".deleteProperty(propName: string; value : TFHIRObject);\r\n");
      impl2.append("begin\r\n");
      if (delprops.length() > 7) {
        impl2.append("  "+delprops.toString().substring(7));
        impl2.append("  else\r\n    inherited deleteProperty(propName, value);\r\n");
      } else
        impl2.append("  inherited deleteProperty(propName, value);\r\n");
      impl2.append("end;\r\n\r\n");
      impl2.append("procedure "+tn+".replaceProperty(propName : string; existing, new : TFHIRObject);\r\n");
      impl2.append("begin\r\n");
      if (replprops.length() > 7) {
        impl2.append("  "+replprops.toString().substring(7));
        impl2.append("  else\r\n    inherited replaceProperty(propName, existing, new);\r\n");
      } else
        impl2.append("  inherited replaceProperty(propName, existing, new);\r\n");
      impl2.append("end;\r\n\r\n");
      impl2.append("procedure "+tn+".reorderProperty(propName : string; source, destination : integer);\r\n");
      impl2.append("begin\r\n");
      if (reorderprops.length() > 7) {
        impl2.append("  "+reorderprops.toString().substring(7));
        impl2.append("  else\r\n    inherited reorderProperty(propName, source, destination);\r\n");
      } else
        impl2.append("  inherited reorderProperty(propName, source, destination);\r\n");
      impl2.append("end;\r\n\r\n");
    }
    impl2.append("function "+tn+".fhirType : string;\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := '"+(ti == analysis.getRootType() ? analysis.getName() : ti.getDefn().getPath())+"';\r\n");
    impl2.append("end;\r\n\r\n");

    impl2.append("function "+tn+".Link : "+tn+";\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := "+tn+"(inherited Link);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function "+tn+".Clone : "+tn+";\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := "+tn+"(inherited Clone);\r\n");
    impl2.append("end;\r\n\r\n");
    if (ti.hasChildren()) {
      generateEquals(analysis, ti, tn, impl2);

      impl2.append("function "+tn+".isEmpty : boolean;\r\n");
      impl2.append("begin\r\n");
      impl2.append("  result := inherited isEmpty "+empty.toString()+";\r\n");
      impl2.append("end;\r\n\r\n");

      impl2.append("procedure "+tn+".listFieldsInOrder(fields : TStringList);\r\n");
      impl2.append("begin;\r\n");
      impl2.append("  inherited listFieldsInOrder(fields);\r\n");  
      impl2.append(fields.toString());  
      impl2.append("end;\r\n\r\n");
      impl2.append("function "+tn+".sizeInBytesV(magic : integer) : cardinal;\r\n");
      impl2.append("begin;\r\n");
      impl2.append("  result := inherited sizeInBytesV(magic);\r\n");  
      impl2.append(sizers.toString());  
      impl2.append("end;\r\n\r\n");
    }

    intf.append(def.toString());
    impl.append(impl2.toString() + impli.toString());
    fwds.append("  "+tn+" = class;\r\n");
    if (config.hasTemplate(tn+".implementation")) {
      def.append(config.getTemplate(tn+".implementation"));
    }
    defineList(tn, tn+"List", null, category, ti == analysis.getRootType() && analysis.isAbstract(), false, fwds, intf, impl);
    return tn;
  }
  
  protected void generateAbstractType(Analysis analysis, TypeInfo ti, ClassGeneratorCategory category, StringBuilder fwds, StringBuilder intf, StringBuilder impl) throws Exception {
    StringBuilder def = new StringBuilder();
    StringBuilder defPriv1 = new StringBuilder();
    StringBuilder fields = new StringBuilder();
    StringBuilder sizers = new StringBuilder();
    StringBuilder defPriv2 = new StringBuilder();
    StringBuilder defPub = new StringBuilder();
    StringBuilder impli = new StringBuilder();
    StringBuilder create = new StringBuilder();
    StringBuilder destroy = new StringBuilder();
    StringBuilder assign = new StringBuilder();
    LineLimitedStringBuilder empty = new LineLimitedStringBuilder(36, 6);
    StringBuilder getkids = new StringBuilder();
    StringBuilder getkidsvars = new StringBuilder();
    StringBuilder getprops = new StringBuilder();
    StringBuilder getpropsvars = new StringBuilder();
    StringBuilder setprops = new StringBuilder();
    StringBuilder insprops = new StringBuilder();
    StringBuilder makeprops = new StringBuilder();
    StringBuilder propTypes = new StringBuilder();
    StringBuilder delprops = new StringBuilder();
    StringBuilder replprops = new StringBuilder();
    StringBuilder reorderprops = new StringBuilder();
    String tn = ti.getName();
    boolean isBase = analysis.getAncestor() == null || analysis.getAncestor().getType().equals("Base");
    
    String tj = tn.substring(5);
    
    for (ElementDefinition e : ti.getChildren()) {
      generateField(analysis, ti, e, getInheritedElement(analysis, ti, e), defPriv1, defPriv2, defPub, impli, create, destroy, assign, empty, getkids, getkidsvars, getprops, getpropsvars, setprops, insprops, makeprops, propTypes, delprops, replprops, reorderprops, tn, "", category, e.getName().equals("Extension"), tj, fields, sizers);
    }

    def.append("  // "+makeDocoSafe(analysis.getStructure().getDescription(), "// ")+"\r\n");
    def.append("  "+tn+" = class abstract (TFhir"+(tn.equals("TFhirResource") ? "Resource5" :    analysis.getAncestor() == null ? "Base" : analysis.getAncestor().getName())+")\r\n");
    if (config.hasTemplate(tn+".private")) {
      def.append("  private\r\n");
      def.append(config.getTemplate(tn+".private"));
    }
    if (ti.hasChildren() || config.hasTemplate(tn+".protected")) {
      def.append("  protected\r\n");
    }
    def.append(defPriv1.toString());
    def.append(defPriv2.toString());
    def.append("  \r\n");
    if (ti.hasChildren()) {
      def.append("    procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;\r\n");
      def.append("    procedure ListProperties(oList : "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues : Boolean); override;\r\n");
      def.append("    procedure listFieldsInOrder(fields : TStringList); override;\r\n");
      def.append("    function sizeInBytesV(magic : integer) : cardinal; override;\r\n");
    }
    if (tn.equals("TFhirResource")) {
      def.append("    function GetResourceType : TFhirResourceType; virtual; abstract;\r\n");
      def.append("    function GetProfileVersion : TFHIRVersion; override;\r\n");
      def.append("    procedure SetProfileVersion(v : TFHIRVersion); override;\r\n");
    } 
    if (config.hasTemplate(tn+".protected")) {
      def.append(config.getTemplate(tn+".protected"));
    }
    def.append("  public\r\n");
    if (ti.hasChildren()) {
    def.append("    constructor Create; override;\r\n");
    def.append("    destructor Destroy; override;\r\n");
    def.append("    procedure Assign(oSource : TFslObject); override;\r\n");
    }
    def.append("    function Link : "+tn+"; overload;\r\n");
    def.append("    function Clone : "+tn+"; overload;\r\n");
    if (ti.hasChildren()) {
    def.append("    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;\r\n");
    def.append("    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;\r\n");
    def.append("    function createPropertyValue(propName : string) : TFHIRObject; override;\r\n");
    def.append("    function getTypesForProperty(propName : string): String; override;\r\n");
    def.append("    procedure deleteProperty(propName : string; value : TFHIRObject); override;\r\n");
    def.append("    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;\r\n");
    def.append("    procedure reorderProperty(propName : string; source, destination : integer); override;\r\n");
    }
    if (isBase) {
      def.append("    function getId : string; override;\r\n");
      def.append("    procedure setIdValue(id : String); override;\r\n");
    } else if (tn.equals("TFhirDomainResource")) {
      def.append("    function isDomainResource : boolean; override;\r\n");
      def.append("    function hasExtension(url : string) : boolean; override;\r\n");
      def.append("    function getExtensionString(url : String) : String; override;\r\n");
      def.append("    function extensionCount(url : String) : integer; override;\r\n");
      def.append("    function extensions(url : String) : TFslList<TFHIRObject>; override;\r\n");
      def.append("    procedure addExtension(url : String; value : TFHIRObject); override;\r\n");
    }
    if (tn.equals("TFhirResource")) 
      def.append("    procedure checkNoImplicitRules(place, role : String); override;\r\n");
    if (ti.hasChildren()) {
      def.append("    function Equals(other : TObject) : boolean; override;\r\n");
      def.append("    function isEmpty : boolean; override;\r\n");
    }
    if (config.hasTemplate(tn+".public")) {
      def.append(config.getTemplate(tn+".public"));
    }
    if (tn.equals("TFhirResource") || ti.hasChildren() || config.hasTemplate(tn+".published")) {
      def.append("  {$IFNDEF FPC}published{$ENDIF}\r\n");
    }
    if (tn.equals("TFhirResource")) {
      def.append("    property ResourceType : TFhirResourceType read GetResourceType;\r\n\r\n");
    } 
    def.append(defPub.toString());
    if (config.hasTemplate(tn+".published")) {
      def.append(config.getTemplate(tn+".published"));
    }
    def.append("  end;\r\n");
    def.append("\r\n");
    if (analysis.getName().equals("Resource")) {
      def.append("  TFhirResourceClass = class of TFhirResource;\r\n");
    } else if (tn.equals("TFhirDataType")) {
      def.append("  TFHIRDataTypeClass = class of TFhirDataType;\r\n\r\n");
    } else if (tn.equals("TFhirPrimitiveType")) {
      def.append("  TFHIRPrimitiveTypeClass = class of TFHIRPrimitiveType;\r\n\r\n");
    }
    StringBuilder impl2 = new StringBuilder();
    impl2.append("{ "+tn+" }\r\n\r\n");
    if (ti.hasChildren()) {
      impl2.append("constructor "+tn+".Create;\r\n");
      impl2.append("begin\r\n");
      impl2.append("  inherited;\r\n");
      impl2.append(create.toString());
      impl2.append("end;\r\n\r\n");

      impl2.append("destructor "+tn+".Destroy;\r\n");
      impl2.append("begin\r\n");
      impl2.append(destroy.toString());
      impl2.append("  inherited;\r\n");
      impl2.append("end;\r\n\r\n");

      impl2.append("procedure "+tn+".Assign(oSource : TFslObject);\r\n");
      impl2.append("begin\r\n");
      impl2.append("  inherited;\r\n");
      impl2.append(assign.toString());
      impl2.append("end;\r\n\r\n");
      impl2.append("procedure "+tn+".GetChildrenByName(child_name : string; list : TFHIRSelectionList);\r\n");
      if (getkidsvars.length() > 0) {
        impl2.append("var\r\n");
        impl2.append(getkidsvars.toString());
      }
      impl2.append("begin\r\n");
      impl2.append("  inherited;\r\n");
      impl2.append(getkids.toString());
      impl2.append("end;\r\n\r\n");
    }
    if (isBase) {
      impl2.append("function "+tn+".getId: string;\r\n");
      impl2.append("begin\r\n");
      impl2.append("  result := GetIdST;\r\n");
      impl2.append("end;\r\n");
      impl2.append("\r\n");
      impl2.append("procedure "+tn+".setIdValue(id: String);\r\n");
      impl2.append("begin\r\n");
      impl2.append("  SetIdSt(id);\r\n");
      impl2.append("end;\r\n");
      impl2.append("\r\n");
      if (tn.equals("TFhirResource")) {
        impl2.append("procedure TFhirResource.SetProfileVersion(v : TFHIRVersion);\r\n");
        impl2.append("var\r\n");
        impl2.append("   i : integer;\r\n");
        impl2.append("begin\r\n");
        impl2.append("  if Meta = nil then\r\n");
        impl2.append("    Meta := TFhirMeta.Create;\r\n");
        impl2.append("  for i := Meta.profileList.Count - 1 downto 0 do\r\n");
        impl2.append("    if isVersionUrl(Meta.profileList[i].value, fhirType) then\r\n");
        impl2.append("      Meta.profileList.DeleteByIndex(i);\r\n");
        impl2.append("  Meta.profileList.Append.value := 'http://hl7.org/fhir/'+PF_CONST[v]+'/StructureDefinition/'+fhirType;\r\n");
        impl2.append("end;\r\n");
        impl2.append("\r\n");
        impl2.append("function TFhirResource.GetProfileVersion : TFHIRVersion;\r\n");
        impl2.append("var\r\n");
        impl2.append("   p : TFHIRUri;\r\n");
        impl2.append("begin\r\n");
        impl2.append("  result := fhirVersionUnknown;\r\n");
        impl2.append("  if Meta <> nil then\r\n");
        impl2.append("    for p in Meta.profileList do\r\n");
        impl2.append("    begin\r\n");
        impl2.append("      if (p.value = 'http://hl7.org/fhir/1.0/StructureDefinition/'+fhirType) then\r\n");
        impl2.append("        exit(fhirVersionRelease2);\r\n");
        impl2.append("      if (p.value = 'http://hl7.org/fhir/3.0/StructureDefinition/'+fhirType) then\r\n");
        impl2.append("        exit(fhirVersionRelease3);\r\n");
        impl2.append("      if (p.value = 'http://hl7.org/fhir/3.4/StructureDefinition/'+fhirType) then\r\n");
        impl2.append("        exit(fhirVersionRelease4);\r\n");
        impl2.append("    end;\r\n");
        impl2.append("end;\r\n");
        impl2.append("\r\n");
        impl2.append("procedure TFhirResource.checkNoImplicitRules(place, role: String);\r\n");
        impl2.append("begin\r\n");
        impl2.append("  if implicitRules <> '' then\r\n");
        impl2.append("    raise EUnsafeOperation.Create('The resource '+role+' has an unknown implicitRules tag at '+place);\r\n");
        impl2.append("end;\r\n\r\n");
      }
    } else if (tn.equals("TFhirDomainResource")) {
      impl2.append("procedure TFhirDomainResource.addExtension(url: String; value: TFHIRObject);\r\n");
      impl2.append("var\r\n");
      impl2.append("  ex : TFhirExtension;\r\n");
      impl2.append("begin\r\n");
      impl2.append("  ex := extensionList.Append;\r\n");
      impl2.append("  ex.url := url;\r\n");
      impl2.append("  ex.value := value as TFhirDataType;\r\n");
      impl2.append("end;\r\n");
      impl2.append("\r\n");
      impl2.append("function TFhirDomainResource.extensionCount(url: String): integer;\r\n");
      impl2.append("var\r\n");
      impl2.append("  ex : TFhirExtension;\r\n");
      impl2.append("begin\r\n");
      impl2.append("  result := 0;\r\n");
      impl2.append("  for ex in ExtensionList do\r\n");
      impl2.append("    if ex.url = url then\r\n");
      impl2.append("      inc(result);\r\n");
      impl2.append("end;\r\n");
      impl2.append("      \r\n");
      impl2.append("function TFhirDomainResource.extensions(url: String): TFslList<TFHIRObject>;\r\n");
      impl2.append("var\r\n");
      impl2.append("  ex : TFhirExtension;\r\n");
      impl2.append("begin\r\n");
      impl2.append("  result := TFslList<TFHIRObject>.Create;\r\n");
      impl2.append("  try\r\n");
      impl2.append("    for ex in ExtensionList do\r\n");
      impl2.append("      if ex.url = url then\r\n");
      impl2.append("        result.Add(ex.Link);\r\n");
      impl2.append("    result.link;\r\n");
      impl2.append("  finally\r\n");
      impl2.append("    result.free;\r\n");
      impl2.append("  end;\r\n");
      impl2.append("end;\r\n");
      impl2.append("\r\n");
      impl2.append("function TFhirDomainResource.isDomainResource: boolean;\r\n");
      impl2.append("begin\r\n");
      impl2.append("  result := true;\r\n");
      impl2.append("end;\r\n");
      impl2.append("\r\n");
      impl2.append("function TFhirDomainResource.getExtensionString(url: String): String;\r\n");
      impl2.append("var\r\n");
      impl2.append("  ex : TFhirExtension;\r\n");
      impl2.append("begin\r\n");
      impl2.append("  result := '';\r\n");
      impl2.append("  for ex in ExtensionList do\r\n");
      impl2.append("  begin\r\n");
      impl2.append("    if ex.url = url then\r\n");
      impl2.append("    begin\r\n");
      impl2.append("      if not ex.value.isPrimitive then\r\n");
      impl2.append("        raise EFHIRException.Create('Complex extension '+url)\r\n");
      impl2.append("      else if result <> '' then\r\n");
      impl2.append("        raise EFHIRException.Create('Duplicate extension '+url)\r\n");
      impl2.append("      else\r\n");
      impl2.append("        result := ex.value.primitiveValue;\r\n");
      impl2.append("    end;\r\n");
      impl2.append("  end;\r\n");
      impl2.append("end;\r\n");
      impl2.append("\r\n");
      impl2.append("function TFhirDomainResource.hasExtension(url: string): boolean;\r\n");
      impl2.append("var\r\n");
      impl2.append("  ex : TFhirExtension;\r\n");
      impl2.append("begin\r\n");
      impl2.append("  result := false;\r\n");
      impl2.append("  for ex in ExtensionList do\r\n");
      impl2.append("    if ex.url = url then\r\n");
      impl2.append("      exit(true);\r\n");
      impl2.append("end;\r\n");
      impl2.append("\r\n");
    } 
    if (ti.hasChildren()) {
      impl2.append("procedure "+tn+".ListProperties(oList: "+listForm("TFHIRProperty")+"; bInheritedProperties, bPrimitiveValues: Boolean);\r\n");
      if (getpropsvars.length() > 0) {
        impl2.append("var\r\n  prop : TFHIRProperty;\r\n");      
        impl2.append(getpropsvars.toString());
      }
      impl2.append("begin\r\n");
      impl2.append("  inherited;\r\n");
      impl2.append(getprops.toString());
      impl2.append("end;\r\n\r\n");
      impl2.append("function "+tn+".setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;\r\n");
      impl2.append("begin\r\n");
      if (setprops.length() < 5) {
        impl2.append("  result := inherited setProperty(propName, propValue);\r\n");
      } else {
        impl2.append("  "+setprops.toString().substring(7));
        impl2.append("  else\r\n    result := inherited setProperty(propName, propValue);\r\n");
      }
      impl2.append("end;\r\n\r\n");
      impl2.append("procedure "+tn+".insertProperty(propName: string; propValue: TFHIRObject; index : integer);\r\n");
      impl2.append("begin\r\n");
      if (insprops.length() > 7) {
        impl2.append("  "+insprops.toString().substring(7));
        impl2.append("  else inherited;\r\n");
      } else {
        impl2.append("  inherited;\r\n");
      }
      impl2.append("end;\r\n\r\n");
      impl2.append("function "+tn+".createPropertyValue(propName: string) : TFHIRObject;\r\n");
      impl2.append("begin\r\n");
      if (makeprops.length() > 7) {
        impl2.append("  "+makeprops.toString().substring(7));
        impl2.append("  else result := inherited createPropertyValue(propName);\r\n");
      } else
        impl2.append("  result := inherited createPropertyValue(propName);\r\n");
      impl2.append("end;\r\n\r\n");
      impl2.append("function "+tn+".getTypesForProperty(propName: string) : String;\r\n");
      impl2.append("begin\r\n");
      if (propTypes.length() > 7) {
        impl2.append("  "+propTypes.toString().substring(7));
        impl2.append("  else result := inherited getTypesForProperty(propName);\r\n");
      } else
        impl2.append("  result := inherited getTypesForProperty(propName);\r\n");
      impl2.append("end;\r\n\r\n");
      impl2.append("procedure "+tn+".deleteProperty(propName: string; value : TFHIRObject);\r\n");
      impl2.append("begin\r\n");
      if (delprops.length() > 7) {
        impl2.append("  "+delprops.toString().substring(7));
        impl2.append("  else\r\n    inherited deleteProperty(propName, value);\r\n");
      } else
        impl2.append("  inherited deleteProperty(propName, value);\r\n");
      impl2.append("end;\r\n\r\n");
      impl2.append("procedure "+tn+".replaceProperty(propName : string; existing, new : TFHIRObject);\r\n");
      impl2.append("begin\r\n");
      if (replprops.length() > 7) {
        impl2.append("  "+replprops.toString().substring(7));
        impl2.append("  else\r\n    inherited replaceProperty(propName, existing, new);\r\n");
      } else
        impl2.append("  inherited replaceProperty(propName, existing, new);\r\n");
      impl2.append("end;\r\n\r\n");
      impl2.append("procedure "+tn+".reorderProperty(propName : string; source, destination : integer);\r\n");
      impl2.append("begin\r\n");
      if (reorderprops.length() > 7) {
        impl2.append("  "+reorderprops.toString().substring(7));
        impl2.append("  else\r\n    inherited reorderProperty(propName, source, destination);\r\n");
      } else
        impl2.append("  inherited reorderProperty(propName, source, destination);\r\n");
      impl2.append("end;\r\n\r\n");
      generateEquals(analysis, ti, tn, impl2);

      impl2.append("function "+tn+".isEmpty : boolean;\r\n");
      impl2.append("begin\r\n");
      impl2.append("  result := inherited isEmpty "+empty.toString()+";\r\n");
      impl2.append("end;\r\n\r\n");
      
      impl2.append("procedure "+tn+".listFieldsInOrder(fields : TStringList);\r\n");
      impl2.append("begin;\r\n");
      impl2.append("  inherited listFieldsInOrder(fields);\r\n");  
      impl2.append(fields.toString());
      impl2.append("end;\r\n\r\n");
      impl2.append("function "+tn+".sizeInBytesV(magic : integer) : cardinal;\r\n");
      impl2.append("begin;\r\n");
      impl2.append("  result := inherited sizeInBytesV(magic);\r\n");  
      impl2.append(sizers.toString());  
      impl2.append("end;\r\n\r\n");      
    }
    impl2.append("function "+tn+".Link : "+tn+";\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := "+tn+"(inherited Link);\r\n");
    impl2.append("end;\r\n\r\n");
    impl2.append("function "+tn+".Clone : "+tn+";\r\n");
    impl2.append("begin\r\n");
    impl2.append("  result := "+tn+"(inherited Clone);\r\n");
    impl2.append("end;\r\n\r\n");
    
    intf.append(def.toString());
    impl.append(impl2.toString() + impli.toString());
    fwds.append("  "+tn+" = class;\r\n");
    if (config.hasTemplate(tn+".implementation")) {
      impl.append(config.getTemplate(tn+".implementation"));
    }

    if (tn.equals("TFhirResource")) { 
      defineList(tn, tn+"List", null, category, true, false, fwds, intf, impl);
    }
  }

    
  private void defineList(String tn, String tnl, String sn, ClassGeneratorCategory category, boolean isAbstract, boolean isEnum, StringBuilder fwds, StringBuilder intf, StringBuilder impl) {
    if (tnl.contains("{"))
      tnl = tnl.substring(0, tnl.indexOf("{"));
    if (tn.contains("{"))
      tn = tn.substring(0, tn.indexOf("{"));
    if (!lists.contains(tnl)) {
      lists.add(tn+"List");
      String tt = tn.substring(1);
      fwds.append("  "+tn+"List = class;\r\n");
      types.add(tn+"List");
      intf.append(
          "  "+tnl+"Enumerator = class (TFslObject)\r\n"+
              "  private\r\n"+
              "    FIndex : integer;\r\n"+
              "    FList : "+tnl+";\r\n"+
              "    function GetCurrent : "+tn+";\r\n"+
              "  protected\r\n"+
              "    function sizeInBytesV(magic : integer) : cardinal; override;\r\n"+
              "  public\r\n"+
              "    constructor Create(list : "+tnl+");\r\n"+
              "    destructor Destroy; override;\r\n"+
              "    function MoveNext : boolean;\r\n"+
              "    property Current : "+tn+" read GetCurrent;\r\n"+
          "  end;\r\n\r\n");


      intf.append(
              "  "+tn+"List = class ("+listForm("TFHIRObject")+")\r\n"+
          "  private\r\n");
      if (isEnum) {
        intf.append("    FSystems : Array Of String;\r\n");
        intf.append("    FCodes : Array Of String;\r\n");
      }

      intf.append(
          "    function GetItemN(index : Integer) : "+tn+";\r\n"+
              "    procedure SetItemN(index : Integer; value : "+tn+");\r\n"+
              "  protected\r\n"+
              "    function ItemClass : TFslObjectClass; override;\r\n"+
          "  public\r\n");
      if (isEnum)
        intf.append("    constructor Create(Systems, Codes : Array Of String);\r\n");

      intf.append(
              "    function Link : "+tn+"List; overload;\r\n"+
              "    function Clone : "+tn+"List; overload;\r\n"+
              "    function GetEnumerator : "+tnl+"Enumerator;\r\n"+
          "    \r\n");
      if (!isAbstract)
        intf.append(
                "    //  Add a "+tt+" to the end of the list.\r\n"+
                "    function Append : "+tn+";\r\n");
      intf.append(
          "    \r\n"+
              "    // Add an already existing "+tt+" to the end of the list.\r\n"+
              "    function AddItem(value : "+tn+") : "+tn+"; overload;\r\n");
      if (sn != null)    
        intf.append(
            "    \r\n"+
                "    // Add an already existing "+tt+" to the end of the list.\r\n"+
                "    function AddItem(value : "+sn+") : "+sn+"; overload;\r\n");
      intf.append(
          "    \r\n"+
              "    // See if an item is already in the list. returns -1 if not in the list\r\n"+
              "    function IndexOf(value : "+tn+") : Integer;\r\n"+
          "    \r\n");
      if (!isAbstract)
        intf.append(
                "    // Insert "+tt+" before the designated index (0 = first item)\r\n"+
                "    function Insert(index : Integer) : "+tn+";\r\n"+
            "    \r\n");
      intf.append(
              "    // Insert an existing "+tt+" before the designated index (0 = first item)\r\n"+
              "    procedure InsertItem(index : Integer; value : "+tn+");\r\n"+
              "    \r\n"+
              "    // Get the iIndexth "+tt+". (0 = first item)\r\n"+
              "    procedure SetItemByIndex(index : Integer; value : "+tn+");\r\n"+
              "    \r\n"+
              "    // The number of items in the collection\r\n"+
              "    function Item(index : Integer) : "+tn+";\r\n"+
              "    \r\n"+
              "    // The number of items in the collection\r\n"+
              "    function Count : Integer; overload;\r\n"+
              "    \r\n"+
              "    // Remove the indexth item. The first item is index 0.\r\n"+
              "    procedure Remove(index : Integer);\r\n"+
              "    \r\n"+
              "    // Remove All Items from the list\r\n"+
              "    procedure ClearItems;\r\n"+
              "    \r\n"+
              "    property "+Utilities.pluralizeMe(tt)+"[index : Integer] : "+tn+" read GetItemN write SetItemN; default;\r\n"+
              "  End;\r\n"+
              "\r\n"  
          );
      impl.append(
          "{ "+tnl+"Enumerator }\r\n"+
              "\r\n"+
              "constructor "+tnl+"Enumerator.Create(list : "+tnl+");\r\n"+
              "begin\r\n"+
              "  inherited Create;\r\n"+
              "  FIndex := -1;\r\n"+
              "  FList := list;\r\n"+
              "end;\r\n"+
              "\r\n"+
              "destructor "+tnl+"Enumerator.Destroy;\r\n"+
              "begin\r\n"+
              "  FList.free;\r\n"+
              "  inherited;\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tnl+"Enumerator.MoveNext : boolean;\r\n"+
              "begin\r\n"+
              "  inc(FIndex);\r\n"+
              "  Result := FIndex < FList.count;\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tnl+"Enumerator.GetCurrent : "+tn+";\r\n"+
              "begin\r\n"+
              "  Result := FList[FIndex];\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tnl+"Enumerator.sizeInBytesV(magic : integer) : cardinal;\r\n"+
              "begin\r\n"+
              "  result := inherited sizeInBytesV(magic);\r\n"+
              "  inc(result, FList.sizeInBytes(magic));\r\n"+
              "end;\r\n"+
          "\r\n");
      impl.append(
          "{ "+tn+"List }\r\n\r\n"+
              "function "+tn+"List.AddItem(value: "+tn+"): "+tn+";\r\n"+
              "begin\r\n"+
              "  assert(value.ClassName = '"+tn+"', 'Attempt to add an item of type '+value.ClassName+' to a List of "+tn+"');\r\n"+
              "  add(value);\r\n"+
              "  result := value;\r\n"+
              "end;\r\n"+
          "\r\n");
      if (isEnum)
        impl.append("constructor "+tn+"List.Create(Systems, Codes : Array Of String);\r\n"+
            "var\r\n"+
            "  i : integer;\r\n"+
            "begin\r\n"+
            "  inherited Create;\r\n"+
            "  SetLength(FSystems, length(systems));\r\n"+
            "  SetLength(FCodes, length(codes));\r\n"+
            "  for i := 0 to length(systems) - 1 do\r\n"+
            "  begin\r\n"+
            "    FSystems[i] := systems[i];\r\n"+
            "    FCodes[i] := codes[i];\r\n"+
            "  end;\r\n"+
            "end;\r\n"
            );
      if (sn != null) {    
        if (isEnum) {
          impl.append(
              "function "+tn+"List.AddItem(value: "+sn+"): "+sn+";\r\n"+
                  "begin\r\n"+
                  "  result := \"+tn+\".Create(FSystems[StringArrayIndexOf(FCodes, value)], value);\r\n"+
                  "  add(result);\r\n"+
                  "end;\r\n"+
              "\r\n");
        } else {
          impl.append(
              "function "+tn+"List.AddItem(value: "+sn+"): "+sn+";\r\n"+
                  "begin\r\n"+
                  "  result := \"+tn+\".Create(value);\r\n"+
                  "  add(result);\r\n"+
                  "end;\r\n"+
              "\r\n");
        }
      }
      if (!isAbstract) {
        impl.append(
            "function "+tn+"List.Append: "+tn+";\r\n"+
                "begin\r\n"+
                "  result := "+tn+".Create;\r\n"+
                "  try\r\n"+
                "    add(result.Link);\r\n"+
                "  finally\r\n"+
                "    result.free;\r\n"+
                "  end;\r\n"+
                "end;\r\n"+
            "\r\n");
      }
      impl.append(
          "procedure "+tn+"List.ClearItems;\r\n"+
              "begin\r\n"+
              "  Clear;\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tnl+".GetEnumerator : "+tnl+"Enumerator;\r\n"+
              "begin\r\n"+
              "  result := "+tnl+"Enumerator.Create(self.link);\r\n"+
              "end;\r\n\r\n"+
              "function "+tn+"List.Clone: "+tn+"List;\r\n"+
              "begin\r\n"+
              "  result := "+tn+"List(inherited Clone);\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tn+"List.Count: Integer;\r\n"+
              "begin\r\n"+
              "  result := Inherited Count;\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tn+"List.GetItemN(index: Integer): "+tn+";\r\n"+
              "begin\r\n"+
              "  result := "+tn+"(ObjectByIndex[index]);\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tn+"List.ItemClass: TFslObjectClass;\r\n"+
              "begin\r\n"+
              "  result := "+tn+";\r\n"+
              "end;\r\n"+
              "function "+tn+"List.IndexOf(value: "+tn+"): Integer;\r\n"+
              "begin\r\n"+
              "  result := IndexByReference(value);\r\n"+
              "end;\r\n"+
          "\r\n");
      if (!isAbstract) {
        impl.append(
            "function "+tn+"List.Insert(index: Integer): "+tn+";\r\n"+
                "begin\r\n"+
                "  result := "+tn+".Create;\r\n"+
                "  try\r\n"+
                "    inherited insert(index, result.Link);\r\n"+
                "  finally\r\n"+
                "    result.free;\r\n"+
                "  end;\r\n"+
                "end;\r\n"+
            "\r\n");
      }
      impl.append(
          "procedure "+tn+"List.InsertItem(index: Integer; value: "+tn+");\r\n"+
              "begin\r\n"+
              "  assert(value is "+tn+");\r\n"+
              "  Inherited Insert(index, value);\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tn+"List.Item(index: Integer): "+tn+";\r\n"+
              "begin\r\n"+
              "  result := "+tn+"(ObjectByIndex[index]);\r\n"+
              "end;\r\n"+
              "\r\n"+
              "function "+tn+"List.Link: "+tn+"List;\r\n"+
              "begin\r\n"+
              "  result := "+tn+"List(inherited Link);\r\n"+
              "end;\r\n"+
              "\r\n"+
              "procedure "+tn+"List.Remove(index: Integer);\r\n"+
              "begin\r\n"+
              "  DeleteByIndex(index);\r\n"+
              "end;\r\n"+
              "\r\n"+
              "procedure "+tn+"List.SetItemByIndex(index: Integer; value: "+tn+");\r\n"+
              "begin\r\n"+
              "  assert(value is "+tn+");\r\n"+
              "  "+Utilities.pluralizeMe(tt)+"[index] := value;\r\n"+
              "end;\r\n"+
              "\r\n"+
              "procedure "+tn+"List.SetItemN(index: Integer; value: "+tn+");\r\n"+
              "begin\r\n"+
              "  assert(value is "+tn+");\r\n"+
              "  ObjectByIndex[index] := value;\r\n"+
              "end;\r\n"        
          ); 
      
    }
    if (!impl.toString().endsWith("\r\n\r\n")) {
      impl.append("\r\n");
    }
  }

  
  private void generateEquals(Analysis analysis, TypeInfo ti, String tn, StringBuilder b) throws IOException {
    b.append("function "+tn+".equals(other : TObject) : boolean; \r\n");
    b.append("var\r\n");
    b.append("  o : "+tn+";\r\n");
    b.append("begin\r\n");
    b.append("  if (not inherited equals(other)) then\r\n");
    b.append("    result := false\r\n");
    b.append("  else if (not (other is "+tn+")) then\r\n");
    b.append("    result := false\r\n");
    b.append("  else\r\n");
    b.append("  begin\r\n");
    b.append("    o := "+tn+"(other);\r\n");
    b.append("    result := ");
    boolean first = true;
    int col = 18;
    for (ElementDefinition c : ti.getChildren()) {
      ElementDefinition base = getInheritedElement(analysis, ti, c);
      if (base == null) {
        if (first)
          first = false;
        else {
          b.append(" and ");
          col = col+5;
        }
        if (col > 80) {
          col = 6;
          b.append("\r\n      ");
        }
        String name = getElementName(c.getName());
        if (name.endsWith("[x]"))
          name = name.substring(0, name.length()-3);
        if (c.unbounded())
          name = name + "List";
        else
          name = name + "Element";
        b.append("compareDeep("+name+", o."+name+", true)");
        col = col+21 + name.length()*2;
      }
    }
    if (first)
      b.append("true"); 
    b.append(";\r\n");
    b.append("  end;\r\n");
    b.append("end;\r\n\r\n");

  }

  private void generateField(Analysis analysis, TypeInfo ti, ElementDefinition e, ElementDefinition base, StringBuilder defPriv1, StringBuilder defPriv2, StringBuilder defPub, StringBuilder impli, StringBuilder create, StringBuilder destroy, 
        StringBuilder assign, LineLimitedStringBuilder empty, StringBuilder getkids, StringBuilder getkidsvars, StringBuilder getprops, StringBuilder getpropsvars, StringBuilder setprops, StringBuilder insprops, StringBuilder makeprops,
        StringBuilder propTypes, StringBuilder delprops, StringBuilder replprops, StringBuilder reorderprops, String cn, String pt, ClassGeneratorCategory category, boolean isExtension, String tj, StringBuilder fields, StringBuilder sizers) throws Exception {

    String tn = e.getUserString("pascal.type");
    String path = e.getPath();

    String parse = null;
    String propV = "F"+getTitle(getElementName(e.getName()));
    if (e.hasRepresentation(PropertyRepresentation.XMLATTR)) { 
      fields.append("  fields.add('@"+e.getName()+"');\r\n");
    } else {
      fields.append("  fields.add('"+e.getName()+"');\r\n");
    }
    String s = getElementName(e.getName());
    String sumSet = "";
    if (s.equals("total") && path.startsWith("Bundle"))
      sumSet = "soFull, soSummary, soText, soData, soCount";
    else if (s.equals("id") || path == null || path.startsWith("Bundle") || path.startsWith("Parameters") || analysis.isAbstract() || analysis.isInterface() )
      sumSet = "soFull, soSummary, soText, soData";
    else if ((s.equals("text") && path.equals("DomainResource")) || (path.equals("Narrative")) )
      sumSet = "soFull, soText";
    else if (e.getIsSummary())
      sumSet = "soFull, soSummary, soData";
    else
      sumSet = "soFull, soData";
    String dc = (category == ClassGeneratorCategory.Resource) && !path.contains(".") && !path.contains("Bundle") && !s.equals("id") ? " and doCompose('"+s+"')" : "";
    boolean narrowCardinality = (base != null) && !base.getMax().equals(e.getMax());
    
    //    boolean summary = e.isSummary() || noSummaries;
    //    String sumAnd = summary ? "" : "Not SummaryOnly and ";
    //    String sum2 = summary ? "" : "if not SummaryOnly then\r\n    ";
    if (e.unbounded()) {
      if (e.hasUserData("pascal.enum")) {
        if (base == null) {
          sizers.append("  inc(result, F"+getTitle(s)+".sizeInBytes(magic));\r\n");
          defPriv1.append("    F"+getTitle(s)+" : "+listForm("TFhirEnum")+";\r\n");
          destroy.append("  F"+getTitle(s)+".free;\r\n");
          empty.append(" and isEmptyProp(F"+getTitle(s)+")");
        }
        defPriv2.append("    function Get"+Utilities.capitalize(s)+" : "+listForm("TFhirEnum")+";\r\n");
        defPriv2.append("    function GetHas"+Utilities.capitalize(s)+" : Boolean;\r\n");
        if (getEnumSize(e) < 32) {
          defPriv2.append("    function Get"+getTitle(s)+"ST : "+listForm(tn)+";\r\n");
          defPriv2.append("    procedure Set"+getTitle(s)+"ST(value : "+listForm(tn)+");\r\n");
        }
        if (base == null) {
          assign.append("  if ("+cn+"(oSource).F"+getTitle(s)+" = nil) then\r\n");
          assign.append("  begin\r\n");
          assign.append("    F"+getTitle(s)+".free;\r\n");
          assign.append("    F"+getTitle(s)+" := nil;\r\n");
          assign.append("  end\r\n");
          assign.append("  else\r\n");
          assign.append("  begin\r\n");
          assign.append("    F"+getTitle(s)+" := "+listForm("TFhirEnum")+".Create(SYSTEMS_"+tn+", CODES_"+tn+");\r\n");
        }
        if (getEnumSize(e) < 32) {
          defPub.append("    // "+makeDocoSafe(e.getDefinition(), "// ")+"\r\n");
          defPub.append("    property "+s+" : "+listForm(tn)+" read Get"+getTitle(s)+"ST write Set"+getTitle(s)+"ST;\r\n");
          defPub.append("    property "+s+"List : "+listForm("TFhirEnum")+" read Get"+getTitle(s)+";\r\n");
          if (base == null) {
            assign.append("    F"+getTitle(s)+".Assign("+cn+"(oSource).F"+getTitle(s)+");\r\n");
          }
        } else {
          defPub.append("    property "+s+" : "+listForm("TFhirEnum")+" read Get"+getTitle(s)+";\r\n");
          defPub.append("    property "+s+"List : "+listForm("TFhirEnum")+" read Get"+getTitle(s)+";\r\n");
          if (base == null) {
            assign.append("    F"+getTitle(s)+".Assign("+cn+"(oSource).F"+getTitle(s)+");\r\n");
          }
        }
        if (base == null) {
          assign.append("  end;\r\n");
        }
        defPub.append("    property has"+Utilities.capitalize(s)+" : boolean read GetHas"+getTitle(s)+";\r\n");
        // no, do it lazy create.append("  F"+getTitle(s)+" := "+listForm("TFhirEnum")+".Create;\r\n");
        impli.append("function "+cn+".Get"+getTitle(s)+" : "+listForm("TFhirEnum")+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    F"+getTitle(s)+" := "+listForm("TFhirEnum")+".Create(SYSTEMS_"+tn+", CODES_"+tn+");\r\n  result := F"+getTitle(s)+";\r\nend;\r\n\r\n");
        impli.append("function "+cn+".GetHas"+getTitle(s)+" : boolean;\r\nbegin\r\n  result := (F"+getTitle(s)+" <> nil) and (F"+getTitle(s)+".count > 0);\r\nend;\r\n\r\n");
        if (base == null) {
          if (e.getName().endsWith("[x]") || e.getName().equals("[type]")) 
            getkids.append("  if StringStartsWith(child_name, '"+getPropertyName(e.getName())+"') Then\r\n    list.addAll(self, '"+e.getName()+"', F"+getTitle(s)+");\r\n");
          else
            getkids.append("  if (child_name = '"+e.getName()+"') Then\r\n     list.addAll(self, '"+e.getName()+"', F"+getTitle(s)+");\r\n");
          getprops.append("  oList.add(TFHIRProperty.Create(self, '"+e.getName()+"', '"+breakConstant(e.typeSummaryVB())+"', true, TFhirEnum, F"+getTitle(s)+".Link))"+marker()+";\r\n");
          propTypes.append("  else if (propName = '"+e.getName()+"') then result := '"+breakConstant(e.typeSummaryVB())+"'\r\n");
          if (e.getName().endsWith("[x]"))
            throw new Exception("Not done yet");
          setprops.append("  else if (propName = '"+e.getName()+"') then\r\n  begin\r\n    "+getTitle(s)+"List.add(asEnum(SYSTEMS_"+tn+", CODES_"+tn+", propValue));"+marker()+"\r\n    result := propValue;\r\n  end\r\n");
          insprops.append("  else if (propName = '"+e.getName()+"') then F"+getTitle(s)+".insertItem(index, asEnum(SYSTEMS_"+tn+", CODES_"+tn+", propValue))"+marker()+"\r\n");
          reorderprops.append("  else if (propName = '"+e.getName()+"') then F"+getTitle(s)+".move(source, destination)"+marker()+"\r\n");
        }
        String obj = "";
        if (getEnumSize(e) < 32) {
          impli.append("function "+cn+".Get"+getTitle(s)+"ST : "+listForm(tn)+";\r\n  var i : integer;\r\nbegin\r\n  result := [];\r\n  if F"+s+" <> nil then\r\n    for i := 0 to F"+s+".count - 1 do\r\n      result := result + ["+tn+"(StringArrayIndexOfSensitive(CODES_"+tn+", F"+s+"[i].value))];\r\nend;\r\n\r\n");
          impli.append("procedure "+cn+".Set"+getTitle(s)+"ST(value : "+listForm(tn)+");\r\nvar a : "+tn+";\r\nbegin\r\n  if F"+s+" = nil then\r\n    F"+s+" := TFhirEnumList.Create(SYSTEMS_"+tn+", CODES_"+tn+");\r\n  F"+s+".clear;\r\n  for a := low("+tn+") to high("+tn+") do\r\n    if a in value then\r\n      begin\r\n         if F"+s+" = nil then\r\n           F"+s+" := TFhirEnumList.Create(SYSTEMS_"+tn+", CODES_"+tn+");\r\n         F"+s+".add(TFhirEnum.Create(SYSTEMS_"+tn+"[a], CODES_"+tn+"[a]));\r\n      end;\r\nend;\r\n\r\n");
          obj = "List";
        }
      } else {
        String tnl;
        if (tn.contains("{"))
          tnl = listForm(tn.substring(0, tn.indexOf('{')))+tn.substring(tn.indexOf('{'));
        else
          tnl = listForm(tn);
        s = s+"List";
        defPub.append("    // "+makeDocoSafe(e.getDefinition(), "// ")+"\r\n");
        if (base == null) { 
          sizers.append("  inc(result, "+propV+"List.sizeInBytes(magic));\r\n");
          defPriv1.append("    F"+s+" : "+tnl+";\r\n");
          destroy.append("  F"+getTitle(s)+".free;\r\n");
          empty.append(" and isEmptyProp(F"+s+")");
        }
        defPriv2.append("    function Get"+Utilities.capitalize(s)+" : "+tnl+";\r\n");
        defPriv2.append("    function GetHas"+Utilities.capitalize(s)+" : Boolean;\r\n");
        defPub.append("    property "+s+" : "+tnl+" read Get"+getTitle(s)+";\r\n");
        defPub.append("    property has"+Utilities.capitalize(s)+" : boolean read GetHas"+getTitle(s)+";\r\n");
        defPub.append("\r\n");
        impli.append("function "+cn+".Get"+getTitle(s)+" : "+tnl+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    F"+getTitle(s)+" := "+tnl+".Create;\r\n  result := F"+getTitle(s)+";\r\nend;\r\n\r\n");
        impli.append("function "+cn+".GetHas"+getTitle(s)+" : boolean;\r\nbegin\r\n  result := (F"+getTitle(s)+" <> nil) and (F"+getTitle(s)+".count > 0);\r\nend;\r\n\r\n");
        // create.append("  F"+getTitle(s)+z" := "+tnl+".Create;\r\n");
        if (base == null) {
          assign.append("  if ("+cn+"(oSource).F"+getTitle(s)+" = nil) then\r\n");
          assign.append("  begin\r\n");
          assign.append("    F"+getTitle(s)+".free;\r\n");
          assign.append("    F"+getTitle(s)+" := nil;\r\n");
          assign.append("  end\r\n");
          assign.append("  else\r\n");
          assign.append("  begin\r\n");
          assign.append("    if F"+getTitle(s)+" = nil then\r\n      F"+getTitle(s)+" := "+tnl+".Create;\r\n");
          assign.append("    F"+getTitle(s)+".Assign("+cn+"(oSource).F"+getTitle(s)+");\r\n");
          assign.append("  end;\r\n");
        }
        if (base == null) { 
          getkids.append("  if (child_name = '"+e.getName()+"') Then\r\n    list.addAll(self, '"+e.getName()+"', F"+getTitle(s)+");\r\n");
          getprops.append("  oList.add(TFHIRProperty.Create(self, '"+e.getName()+"', '"+breakConstant(e.typeSummaryVB())+"', true, "+tn+", F"+getTitle(s)+".Link))"+marker()+";\r\n");
          propTypes.append("  else if (propName = '"+e.getName()+"') then result := '"+breakConstant(e.typeSummaryVB())+"'\r\n");
          if (e.getName().endsWith("[x]"))
            throw new Exception("Not done yet");
          if (typeIsPrimitive(e.typeSummary())) {
            setprops.append("  else if (propName = '"+e.getName()+"') then\r\n  begin\r\n    "+getTitle(s)+".add(as"+tn.substring(5)+"(propValue)){2};     result := propValue;\r\n\r\n  end\r\n");
            insprops.append("  else if (propName = '"+e.getName()+"') then "+getTitle(s)+".insertItem(index, as"+tn.substring(5)+"(propValue))"+marker()+"\r\n");
            reorderprops.append("  else if (propName = '"+e.getName()+"') then "+getTitle(s)+".move(source, destination)"+marker()+"\r\n");
          } else {
            setprops.append("  else if (propName = '"+e.getName()+"') then\r\n  begin\r\n    "+getTitle(s)+".add(propValue as "+tn+")"+marker()+";\r\n    result := propValue;\r\n  end\r\n");
            insprops.append("  else if (propName = '"+e.getName()+"') then "+getTitle(s)+".insertItem(index, propValue as "+tn+")"+marker()+"\r\n");
            reorderprops.append("  else if (propName = '"+e.getName()+"') then "+getTitle(s)+".move(source, destination)"+marker()+"\r\n");
          }
          if (!e.typeSummary().equals("Resource")) 
            makeprops.append("  else if (propName = '"+e.getName()+"') then result := "+getTitle(s)+".new()"+marker()+"\r\n");
          delprops.append("  else if (propName = '"+e.getName()+"') then deletePropertyValue('"+e.getName()+"', "+getTitle(s)+", value)"+marker()+"\r\n");
          replprops.append("  else if (propName = '"+e.getName()+"') then replacePropertyValue('"+e.getName()+"', "+getTitle(s)+", existing, new)"+marker()+"\r\n");
        }
      }
    } else {
      if (e.hasUserData("pascal.enum")) {
        if (base == null) {
          defPriv1.append("    F"+getTitle(s)+" : TFhirEnum;\r\n"); 
          empty.append(" and isEmptyProp(F"+getTitle(s)+")");
        }
        defPriv2.append("    procedure Set"+getTitle(s)+"(value : TFhirEnum);\r\n");
        defPriv2.append("    function Get"+getTitle(s)+"ST : "+tn+";\r\n");
        defPriv2.append("    procedure Set"+getTitle(s)+"ST(value : "+tn+");\r\n");
        defPub.append("    // "+makeDocoSafe(e.getDefinition(), "// ")+"\r\n");
        defPub.append("    property "+s+" : "+tn+" read Get"+getTitle(s)+"ST write Set"+getTitle(s)+"ST;\r\n");
        defPub.append("    property "+s+"Element : TFhirEnum read F"+getTitle(s)+" write Set"+getTitle(s)+";\r\n");
      } else {
        if (base == null) {
          defPriv1.append("    F"+getTitle(s)+" : "+tn+";\r\n");
          empty.append(" and isEmptyProp(F"+getTitle(s)+")");
        }
        defPriv2.append("    procedure Set"+getTitle(s)+"(value : "+tn+");\r\n");
        if (simpleTypes.containsKey(tn)) {
          String sn = simpleTypes.get(tn);
          defPriv2.append("    function Get"+getTitle(s)+"ST : "+sn+";\r\n");
          defPriv2.append("    procedure Set"+getTitle(s)+"ST(value : "+sn+");\r\n");
          defPub.append("    // Typed access to "+makeDocoSafe(e.getDefinition(), "// ")+"\r\n");
          defPub.append("    property "+s+" : "+sn+" read Get"+getTitle(s)+"ST write Set"+getTitle(s)+"ST;\r\n");
          defPub.append("    // "+makeDocoSafe(e.getDefinition(), "// ")+"\r\n");
          defPub.append("    property "+s+"Element : "+tn+" read F"+getTitle(s)+" write Set"+getTitle(s)+";\r\n");
        } else {
          defPub.append("    // Typed access to "+makeDocoSafe(e.getDefinition(), "// ")+" (defined for API consistency)\r\n");
          if (base != null && !base.getMax().equals("1")) {
            defPriv2.append("    function Get"+getTitle(s)+" : "+tn+";\r\n");
            defPub.append("    property "+s+" : "+tn+" read Get"+getTitle(s)+" write Set"+getTitle(s)+"; // !! change cardinality - base has a list\r\n");            
            defPub.append("    // "+makeDocoSafe(e.getDefinition(), "// ")+"\r\n");
            defPub.append("    property "+s+"Element : "+tn+" read Get"+getTitle(s)+" write Set"+getTitle(s)+";\r\n");
          } else {
            defPub.append("    property "+s+" : "+tn+" read F"+getTitle(s)+" write Set"+getTitle(s)+";\r\n");
            defPub.append("    // "+makeDocoSafe(e.getDefinition(), "// ")+"\r\n");
            defPub.append("    property "+s+"Element : "+tn+" read F"+getTitle(s)+" write Set"+getTitle(s)+";\r\n");
          }
        }
      }
      defPub.append("\r\n");
      if (typeIsSimple(tn) && !tn.equals("TFhirXHtmlNode")) {
        if (e.hasUserData("pascal.enum")) {
          impli.append("procedure "+cn+".Set"+getTitle(s)+"(value : TFhirEnum);\r\nbegin\r\n  F"+getTitle(s)+".free;\r\n  F"+getTitle(s)+" := value;\r\nend;\r\n\r\n");
          impli.append("function "+cn+".Get"+getTitle(s)+"ST : "+tn+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    result := "+tn+"(0)\r\n  else\r\n    result := "+tn+"(StringArrayIndexOfSensitive(CODES_"+tn+", F"+getTitle(s)+".value));\r\nend;\r\n\r\n");
          impli.append("procedure "+cn+".Set"+getTitle(s)+"ST(value : "+tn+");\r\nbegin\r\n  if ord(value) = 0 then\r\n    "+getTitle(s)+"Element := nil\r\n  else\r\n    "+getTitle(s)+"Element := TFhirEnum.Create(SYSTEMS_"+tn+"[value], CODES_"+tn+"[value]);\r\nend;\r\n\r\n");
          setprops.append("  else if (propName = '"+e.getName()+"') then\r\n  begin\r\n    "+propV.substring(1)+"Element := asEnum(SYSTEMS_"+tn+", CODES_"+tn+", propValue);\r\n    result := propValue\r\n  end\r\n");
          replprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := asEnum(SYSTEMS_"+tn+", CODES_"+tn+", new)"+marker()+"\r\n");          
          delprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := nil\r\n");
        } else {
          impli.append("procedure "+cn+".Set"+getTitle(s)+"(value : TFhirEnum);\r\nbegin\r\n  F"+getTitle(s)+".free;\r\n  F"+getTitle(s)+" := value;\r\nend;\r\n\r\n");
          impli.append("procedure "+cn+".Set"+getTitle(s)+"ST(value : "+tn+");\r\nbegin\r\n  if ord(value) = 0 then\r\n    "+getTitle(s)+" := nil"+marker()+"\r\n  else\r\n    "+getTitle(s)+" := TFhirEnum.Create(SYSTEMS_"+tn+rId+"[value], CODES_"+tn+rId+"[value]);\r\nend;\r\n\r\n");
          setprops.append("  else if (propName = '"+e.getName()+"') then\r\n  begin\r\n    "+propV.substring(1)+" := asEnum(SYSTEMS_"+tn+", CODES_"+tn+", propValue)\r\n    result := propValue;\r\n  end\r\n");
          replprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := new as "+tn+""+marker()+"\r\n");          
          delprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := nil\r\n");
        }
        if (base == null) {
          assign.append("  F"+getTitle(s)+" := "+cn+"(oSource).F"+getTitle(s)+".Link;\r\n");
        }
        if (base == null) { 
          getkids.append("  if (child_name = '"+e.getName()+"') Then\r\n     list.add(self.link, '"+e.getName()+"', F"+getTitle(s)+".Link);\r\n");
          getprops.append("  oList.add(TFHIRProperty.Create(self, '"+e.getName()+"', '"+breakConstant(e.typeSummaryVB())+"', false, TFhirEnum, "+propV+".Link));"+marker()+"\r\n");
          propTypes.append("  else if (propName = '"+e.getName()+"') then result := '"+breakConstant(e.typeSummaryVB())+"'\r\n");
          if (e.getName().endsWith("[x]"))
            throw new Exception("Not done yet");
          destroy.append("  F"+getTitle(s)+".free;\r\n");
        }
      } else {
        if (e.hasUserData("pascal.enum")) {
          impli.append("procedure "+cn+".Set"+getTitle(s)+"(value : TFhirEnum);\r\nbegin\r\n  F"+getTitle(s)+".free;\r\n  F"+getTitle(s)+" := value;\r\nend;\r\n\r\n");
          impli.append("function "+cn+".Get"+getTitle(s)+"ST : "+tn+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    result := "+tn+"(0)\r\n  else\r\n    result := "+tn+"(StringArrayIndexOfSensitive(CODES_"+tn+", F"+getTitle(s)+".value));\r\nend;\r\n\r\n");
          impli.append("procedure "+cn+".Set"+getTitle(s)+"ST(value : "+tn+");\r\nbegin\r\n  if ord(value) = 0 then\r\n    "+getTitle(s)+"Element := nil\r\n  else\r\n    "+getTitle(s)+"Element := TFhirEnum.Create(SYSTEMS_"+tn+"[value], CODES_"+tn+"[value]);\r\nend;\r\n\r\n");
        } else if (narrowCardinality) {
          impli.append("function "+cn+".Get"+getTitle(s)+" : "+tn+";\r\nbegin\r\n  if F"+getTitle(s)+"List = nil then\r\n    result := nil\r\n  else if F"+getTitle(s)+"List.count > 0 then\r\n    result := F"+getTitle(s)+"List[0]\r\n  else\r\n    result := nil;\r\nend;\r\n\r\n");
          impli.append("procedure "+cn+".Set"+getTitle(s)+"(value : "+tn+");\r\nbegin\r\n  if F"+getTitle(s)+"List = nil then\r\n    F"+getTitle(s)+"List := "+tn+"List.Create;\r\n  F"+getTitle(s)+"List.clear;\r\n  if (value <> nil) then\r\n    F"+getTitle(s)+"List.add(value);"+marker()+"\r\nend;\r\n\r\n");
        } else {
          impli.append("procedure "+cn+".Set"+getTitle(s)+"(value : "+tn+");\r\nbegin\r\n  F"+getTitle(s)+".free;\r\n  F"+getTitle(s)+" := value;"+marker()+"\r\nend;\r\n\r\n");
        }
        if (simpleTypes.containsKey(tn)) {
          String sn = simpleTypes.get(tn);
          if (sn.equals("String")) {
            impli.append("function "+cn+".Get"+getTitle(s)+"ST : "+sn+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    result := ''\r\n  else\r\n    result := F"+getTitle(s)+".value;\r\nend;\r\n\r\n");
            impli.append("procedure "+cn+".Set"+getTitle(s)+"ST(value : "+sn+");\r\nbegin\r\n  if value <> '' then\r\n  begin\r\n    if F"+getTitle(s)+" = nil then\r\n      F"+getTitle(s)+" := "+tn+".Create;\r\n    F"+getTitle(s)+".value := value\r\n  end\r\n  else if F"+getTitle(s)+" <> nil then\r\n    F"+getTitle(s)+".value := '';\r\nend;\r\n\r\n");
          } else if (sn.equals("Boolean")) {
            impli.append("function "+cn+".Get"+getTitle(s)+"ST : "+sn+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    result := false\r\n  else\r\n    result := F"+getTitle(s)+".value;\r\nend;\r\n\r\n");
            impli.append("procedure "+cn+".Set"+getTitle(s)+"ST(value : "+sn+");\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    F"+getTitle(s)+" := "+tn+".Create;\r\n  F"+getTitle(s)+".value := value\r\nend;\r\n\r\n");
          } else if (sn.equals("TFslDateTime")) {
            impli.append("function "+cn+".Get"+getTitle(s)+"ST : "+sn+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    result := TFslDateTime.makeNull\r\n  else\r\n    result := F"+getTitle(s)+".value;\r\nend;\r\n\r\n");
            impli.append("procedure "+cn+".Set"+getTitle(s)+"ST(value : "+sn+");\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    F"+getTitle(s)+" := "+tn+".Create;\r\n  F"+getTitle(s)+".value := value\r\nend;\r\n\r\n");
          } else {
            impli.append("function "+cn+".Get"+getTitle(s)+"ST : "+sn+";\r\nbegin\r\n  if F"+getTitle(s)+" = nil then\r\n    result := nil"+marker()+"\r\n  else\r\n    result := F"+getTitle(s)+".value;\r\nend;\r\n\r\n");
            impli.append("procedure "+cn+".Set"+getTitle(s)+"ST(value : "+sn+");\r\nbegin\r\n  if value <> nil then\r\n  begin\r\n    if F"+getTitle(s)+" = nil then\r\n      F"+getTitle(s)+" := "+tn+".Create;\r\n    F"+getTitle(s)+".value := value\r\n  end\r\n  else if F"+getTitle(s)+" <> nil then\r\n    F"+getTitle(s)+".value := nil;\r\nend;\r\n\r\n");
          }
          if (base == null) {
            assign.append("  "+s+"Element := "+cn+"(oSource)."+s+"Element.Clone;\r\n");
          }
        } else if (base == null) {
          if (e.hasUserData("pascal.enum")) {
            assign.append("  "+s+"Element := "+cn+"(oSource)."+s+"Element.Clone;\r\n");
          } else {
            assign.append("  "+s+" := "+cn+"(oSource)."+s+".Clone;\r\n");
          }
        }
        if (base == null) {
          destroy.append("  F"+getTitle(s)+".free;\r\n");
        }
        if (base == null) { 
          if (e.getName().endsWith("[x]"))
            getkids.append("  if (child_name = '"+e.getName()+"') or (child_name = '"+e.getName().substring(0, e.getName().length()-3)+"') Then\r\n     list.add(self.link, '"+e.getName()+"', F"+getTitle(s)+".Link);\r\n");
          else
            getkids.append("  if (child_name = '"+e.getName()+"') Then\r\n     list.add(self.link, '"+e.getName()+"', F"+getTitle(s)+".Link);\r\n");
          if (e.hasUserData("pascal.enum")) {
            getprops.append("  oList.add(TFHIRProperty.Create(self, '"+e.getName()+"', '"+breakConstant(e.typeSummaryVB())+"', false, TFhirEnum, "+propV+".Link));"+marker()+"\r\n");
          } else {
            getprops.append("  oList.add(TFHIRProperty.Create(self, '"+e.getName()+"', '"+breakConstant(e.typeSummaryVB())+"', false, "+tn+", "+propV+".Link));"+marker()+"\r\n");          
          }
          propTypes.append("  else if (propName = '"+e.getName()+"') then result := '"+breakConstant(e.typeSummaryVB())+"'\r\n");
          if (e.getName().endsWith("[x]")) {
            String ename = e.getName().replace("[x]", "");
            StringBuilder types = new StringBuilder("[");
            boolean first = true;
            for (TypeRefComponent t : e.getType()) {
              if (first) first = false; else types.append(", ");
              types.append("'"+Utilities.capitalize(t.getName())+"'");
            }
            types.append("]");              
            if (!typeIsPrimitive(e.typeSummary())) {
              setprops.append("  else if (isMatchingName(propName, '"+ename+"', "+types+")) then\r\n  begin\r\n    "+propV.substring(1)+" := propValue as "+tn+""+marker()+";\r\n    result := propValue;\r\n  end\r\n");
            } else { 
              setprops.append("  else if (isMatchingName(propName, '"+ename+"', "+types+")) then\r\n  begin\r\n    "+propV.substring(1)+" := propValue as "+tn+""+marker()+";\r\n   result := propValue;\r\n  end\r\n");
            }
            delprops.append("  else if (isMatchingName(propName, '"+ename+"', "+types+")) then "+propV.substring(1)+"Element := nil"+marker()+"\r\n");
            replprops.append("  else if (isMatchingName(propName, '"+ename+"', "+types+")) then "+propV.substring(1)+"Element := new as "+tn+""+marker()+"\r\n");          
            makeprops.append("  else if (isMatchingName(propName, '"+ename+"', "+types+")) then raise EFHIRException.Create('Cannot make property "+propV.substring(1)+"')"+marker()+"\r\n");
          } else {
            delprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := nil\r\n");
            if (!typeIsPrimitive(e.typeSummary()) && !e.typeSummary().equals("xhtml")) {
              replprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := new as "+tn+""+marker()+"\r\n");          
              if (simpleTypes.containsKey(tn))
                setprops.append("  else if (propName = '"+e.getName()+"') then\r\n  begin\r\n    "+propV.substring(1)+"Element := propValue as "+tn+""+marker()+";\r\n    result := propValue;\r\n  end\r\n");
              else {
                setprops.append("  else if (propName = '"+e.getName()+"') then\r\n  begin\r\n    "+propV.substring(1)+" := propValue as "+tn+""+marker()+";\r\n    result := propValue;\r\n  end\r\n");
                if (typeIsAbstract(e.typeSummary()) && !Utilities.existsInList(e.typeSummary(), "Element", "BackboneElement")) {
                  makeprops.append("  else if (propName = '"+e.getName()+"') then raise EFHIRException.Create('Cannot make property "+propV.substring(1)+"')\r\n");
                } else {
                  makeprops.append("  else if (propName = '"+e.getName()+"') then result := "+tn+".Create()"+marker()+"\r\n");
                }
              }
            } else {
              if (e.hasUserData("pascal.enum")) {
                EnumInfo ei = (EnumInfo) e.getUserData("pascal.enum");
                setprops.append("  else if (propName = '"+e.getName()+"') then\r\n  begin\r\n    "+propV.substring(1)+"Element := asEnum(SYSTEMS_"+tn+", CODES_"+tn+", propValue)"+marker()+";\r\n    result := propValue;\r\n  end\r\n");          
                replprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := asEnum(SYSTEMS_"+tn+", CODES_"+tn+", new)"+marker()+"\r\n");          
                makeprops.append("  else if (propName = '"+e.getName()+"') then result := TFhirEnum.Create(SYSTEMS_"+tn+"["+ei.abbreviation()+"Null], CODES_"+tn+"["+ei.abbreviation()+"Null]) "+marker()+"\r\n");
              } else if (!simpleTypes.containsKey(tn) && !tn.equals("TFhirXHtmlNode")) {
                setprops.append("  else if (propName = '"+e.getName()+"') then\r\n  begin\r\n    "+propV.substring(1)+" := propValue as "+tn+""+marker()+";\r\n    result := propValue;\r\n  end\r\n");          
                replprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := new as "+tn+""+marker()+"\r\n");          
                makeprops.append("  else if (propName = '"+e.getName()+"') then result := "+tn+".Create() "+marker()+"\r\n");
              } else if (tn.equals("TFhirCode"+rId)) {
                setprops.append("  else if (propName = '"+e.getName()+"') then\r\n  begin\r\n    "+propV.substring(1)+"Element := asCode"+rId+"(propValue);\r\n    result := propValue;\r\n  end\r\n");
                replprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := asCode"+rId+"(new)"+marker()+"\r\n");          
                makeprops.append("  else if (propName = '"+e.getName()+"') then result := "+tn+".Create()"+marker()+"\r\n");
              } else {
                setprops.append("  else if (propName = '"+e.getName()+"') then\r\n  begin\r\n    "+propV.substring(1)+"Element := as"+tn.substring(5)+"(propValue)"+marker()+";\r\n    result := propValue;\r\n  end\r\n");          
                replprops.append("  else if (propName = '"+e.getName()+"') then "+propV.substring(1)+"Element := as"+tn.substring(5)+"(new)"+marker()+"\r\n");          
                makeprops.append("  else if (propName = '"+e.getName()+"') then result := "+tn+".Create()"+marker()+"\r\n");
              }
            }
          }
        }
      }
    }
  }

  private boolean typeIsAbstract(String t) {
    org.hl7.fhir.r5.model.StructureDefinition sd = definitions.getStructures().get("http://hl7.org/fhir/StructureDefinition/"+t);
    return sd != null && sd.getAbstract();
  }

  private String breakConstant(String typeCode) {
    if (typeCode.length() < 255)
      return typeCode;
    else
      return typeCode.substring(0, 250)+"'+'"+typeCode.substring(250);
  }


}
