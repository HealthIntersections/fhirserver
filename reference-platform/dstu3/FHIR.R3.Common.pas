unit FHIR.R3.Common;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

interface

uses
  SysUtils, Classes, Generics.Collections,
  FHIR.Support.Generics, FHIR.Support.DateTime, FHIR.Support.System,
  FHIR.Base.Objects, FHIR.Base.Common,  FHIR.Base.Lang,
  FHIR.R3.Types, FHIR.R3.Resources, FHIR.R3.Operations;

const
  ExceptionTypeTranslations : array [TExceptionType] of TFhirIssueTypeEnum = (IssueTypeNull, IssueTypeInvalid, IssueTypeStructure, IssueTypeRequired, IssueTypeValue,
    IssueTypeInvariant, IssueTypeSecurity, IssueTypeLogin, IssueTypeUnknown, IssueTypeExpired, IssueTypeForbidden, IssueTypeSuppressed, IssueTypeProcessing,
    IssueTypeNotSupported, IssueTypeDuplicate, IssueTypeNotFound, IssueTypeTooLong, IssueTypeCodeInvalid, IssueTypeExtension, IssueTypeTooCostly, IssueTypeBusinessRule,
    IssueTypeConflict, IssueTypeIncomplete, IssueTypeTransient, IssueTypeLockError, IssueTypeNoStore, IssueTypeException, IssueTypeTimeout, IssueTypeThrottled, IssueTypeInformational);

  ISSUE_SEVERITY_MAP : array [TFhirIssueSeverityEnum] of TIssueSeverity = (isNull, isFatal, isError, isWarning, isInformation);
  ISSUE_SEVERITY_MAP2 : array [TIssueSeverity] of TFhirIssueSeverityEnum = (IssueSeverityNull, IssueSeverityFatal, IssueSeverityError, IssueSeverityWarning, IssueSeverityInformation);
  INTERACTION_MAP : array [TFHIRInteraction] of TFhirTypeRestfulInteractionEnum = (TypeRestfulInteractionRead, TypeRestfulInteractionSearchType, TypeRestfulInteractionHistoryType, TypeRestfulInteractionCreate, TypeRestfulInteractionUpdate, TypeRestfulInteractionDelete, TypeRestfulInteractionPatch);
  INTERACTION_MAP2 : array [TFHIRInteraction] of TFhirSystemRestfulInteractionEnum = (SystemRestfulInteractionNull, SystemRestfulInteractionSearchSystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem);
  MAP_SearchParamType : array [TFhirSearchParamTypeEnum] of TFHIRSearchParamType = (sptString, sptNumber, sptDate, sptString, sptToken, sptReference, sptComposite, sptQuantity, sptUri);
  MAP_SEARCH_MODE : array [TFhirSearchEntryModeEnum] of TFHIRBundleEntrySearchMode = (smUnknown, smMatch, smInclude, smOutcome);
  MAP_ELEMENT_DEFINITION_BINDING : array [TFhirBindingStrengthEnum] of TElementDefinitionBinding = (edbNone, edbRequired, edbExtensible, edbPreferred, edpExample);
  MAP_TFilterOperator : array [TFhirFilterOperatorEnum] of TFilterOperator = (foNull, foEqual, foIsA, foDescendentOf, foIsNotA, foRegex, foIn, foNotIn, foGeneralizes, foExists);
  MAP_TFhirConceptPropertyTypeEnum : array [TFhirConceptPropertyTypeEnum] of TFhirCodeSystemPropertyType = (cptNull, cptCode, cptCoding, cptString, cptInteger, cptBoolean, cptDateTime);
  MAP_TFHIRSearchParamType1 : array [TFhirSearchParamTypeEnum] of TFHIRSearchParamType = (sptNull, sptNumber, sptDate, sptString, sptToken, sptReference, sptComposite, sptQuantity, sptUri);
  MAP_TFHIRSearchParamType2 : array [TFhirSearchParamType] of TFHIRSearchParamTypeEnum = (SearchParamTypeNull, SearchParamTypeNumber, SearchParamTypeDate, SearchParamTypeString, SearchParamTypeToken, SearchParamTypeReference, SearchParamTypeComposite, SearchParamTypeQuantity, SearchParamTypeUri);

type
  TFHIRExtension3 = class (TFHIRExtensionW)
  public
    function url : String; override;
    function value : TFHIRObject; override;
  end;

  TFHIRCoding3 = class (TFHIRCodingW)
  public
    function system : String; override;
    function code : String; override;
    function version : String; override;
    function display : String; override;
  end;

  TFhirOperationOutcome3 = class (TFhirOperationOutcomeW)
  public
    function hasText : boolean; override;
    function text : String; override;
    function code : TExceptionType; override;
    procedure addIssue(issue : TFhirOperationOutcomeIssueW); override;
  end;

  TFHIRBundleEntry3 = class (TFHIRBundleEntryW)
  private
    function entry : TFhirBundleEntry;
  public
    function searchMode : TFHIRBundleEntrySearchMode; override;
    function searchModeE : TFHIRObject; override;
    function searchScoreE : TFHIRObject; override;
    function resource : TFHIRResourceV; override;
  end;


  TFHIRBundle3 = class (TFHIRBundleW)
  private
    function bundle : TFhirBundle;
  public
    function next(bnd : TFHIRResourceV) : String; overload; override;
    procedure addEntries(bnd : TFHIRResourceV); override;
    procedure clearLinks; override;
    function entries : TFslList<TFhirBundleEntryW>; override;
    procedure listLinks(links : TDictionary<String, String>); override;
    function GetLink(rel: String): String; override;
    procedure SetLink(rel: String; const Value: String); override;
    function total : TFHIRObject; override;
  end;

  TFHIROperationOutcomeIssue3 = class (TFHIROperationOutcomeIssueW)
  private
    function issue : TFHIROperationOutcomeIssue;
  public
    function display : String; override;
    function severity : TIssueSeverity; override;
  end;

  TFHIRSearchParamDefinition3 = class (TFHIRSearchParamDefinitionW)
  private
    function param : TFhirCapabilityStatementRestResourceSearchParam;
  public
    function name : String; override;
    function documentation : String; override;
    function type_ : TFHIRSearchParamType; override;
  end;

  TFHIRCapabilityStatement3 = class (TFHIRCapabilityStatementW)
  private
    function statement : TFhirCapabilityStatement;
  public
    function hasRest : boolean; override;
    function hasSecurity(system, code : String) : boolean; override;

    procedure readSmartExtension(var authorize, token, register: String); override;
    function hasFormat(fmt : String) : boolean; override;

    function supportsType(name : String; interaction : TFHIRInteraction) : boolean; override;
    procedure listTypes(interactions : TFHIRInteractions; names : TStrings); override;
    procedure listSearchParams(name : String; list : TFslList<TFHIRSearchParamDefinitionW>); override;
  end;

  TFhirElementDefinition3 = class (TFhirElementDefinitionW)
  private
    function edefn : TFhirElementDefinition;
  public
    function path : String; override;
    function min : integer; override;
    function max : integer; override;
    function defn : String; override;
    function types : String; override;
    function typeList : TArray<String>; override;
    function explicitTypeName : String; override;
    function binding : TElementDefinitionBinding; override;
    function isSummary : boolean; override;
    function valueSet : String; override;
  end;

  TFHIRStructureDefinition3 = class (TFhirStructureDefinitionW)
  private
    function sd : TFhirStructureDefinition;
  public
    function kind : TStructureDefinitionKind; override;
    function name : String; override;
    function url : String; override;
    function type_ : String; override;
    function elements : TFslList<TFHIRElementDefinitionW>; override;
  end;

  TFhirParametersParameter3 = class (TFhirParametersParameterW)
  private
    function parameter : TFhirParametersParameter;
  protected
    function GetValue: TFHIRObject; override;
    procedure SetValue(const Value: TFHIRObject); override;
    procedure populateList; override;
    function GetParameterParameter(name: String): TFhirParametersParameterW; override;
    function GetResourceParameter(name: String): TFHIRResourceV; override;
    function GetStringParameter(name: String): String; override;
  public
    function name : String; override;
    function hasValue : boolean; override;
    property value : TFHIRObject read GetValue write SetValue;
    function appendPart(name : String) : TFhirParametersParameterW; override;
  end;

  TFHIRParameters3 = class (TFHIRParametersW)
  private
    function parameter : TFhirParameters;
  protected
    procedure populateList; override;
  public
    procedure addParamBool(name : String; value : boolean); override;
    procedure addParamStr(name : String; value : string); override;
    procedure addParamCode(name : String; value : string); override;
    function bool(name : String) : boolean; override;
    function str(name : String) : String; override;
    function appendParameter(name : String) : TFhirParametersParameterW; override;
  end;

  TFhirValueSetExpansionContains3 = class (TFhirValueSetExpansionContainsW)
  public
    function getSystem : String; override;
    function getCode : String; override;
    function getDisplay : String; override;
    procedure SetCode(const Value: String); override;
    procedure SetDisplay(const Value: String); override;
    procedure SetSystem(const Value: String); override;
    function contains : TFslList<TFhirValueSetExpansionContainsW>; override;
  end;

  TFhirValueSetExpansion3 = class (TFhirValueSetExpansionW)
  private
    function exp : TFhirValueSetExpansion;
  public
    procedure addParam(name, value : String); override;
    function hasParam(name : string) : boolean; overload; override;
    function hasParam(name, value : string) : boolean; overload; override;
    procedure copyParams(source : TFhirValueSetExpansionW); override;
    procedure addContains(item : TFhirValueSetExpansionContainsW); override;
    function addContains : TFhirValueSetExpansionContainsW; override;
    function contains : TFslList<TFhirValueSetExpansionContainsW>; override;
  end;

  TFhirValueSetComposeIncludeFilter3 = class (TFhirValueSetComposeIncludeFilterW)
  public
    function prop : String; override;
    function op : TFilterOperator; override;
    function value : String; override;
  end;

  TFhirValueSetComposeIncludeConceptDesignation3 = class (TFhirValueSetComposeIncludeConceptDesignationW)
  public
    function language : String; override;
    function value : String; override;
  end;

  TFhirValueSetComposeIncludeConcept3 = class (TFhirValueSetComposeIncludeConceptW)
  public
    function code : String; override;
    function display : String; override;
    function designations : TFslList<TFhirValueSetComposeIncludeConceptDesignationW>; override;
  end;

  TFhirValueSetComposeInclude3 = class (TFhirValueSetComposeIncludeW)
  public
    function valueSets : TArray<String>; override;
    function system : String; override;
    function version : String; override;
    function hasConcepts : boolean; override;
    function concepts : TFslList<TFhirValueSetComposeIncludeConceptW>; override;
    function hasFilters : boolean; override;
    function filters : TFslList<TFhirValueSetComposeIncludeFilterW>; override;
  end;

  TFHIRValueSet3 = class (TFHIRValueSetW)
  private
    function vs : TFhirValueSet;
  public
    function name : String; override;
    function url : String; override;
    function checkCompose(place, role : String) : boolean; override;
    function imports : TArray<String>; override;
    function inlineCS : TFHIRValueSetCodeSystemW; override;
    function includes : TFslList<TFhirValueSetComposeIncludeW>; override;
    function excludes : TFslList<TFhirValueSetComposeIncludeW>; override;
    procedure clearDefinition; override;
    function hasExpansion : boolean; override;
    function expansion : TFhirValueSetExpansionW; override;
    function forceExpansion : TFhirValueSetExpansionW; override;
  end;

  TFhirCodeSystemConceptProperty3 = class (TFhirCodeSystemConceptPropertyW)
  public
    function code : String; override;
    function value : TFHIRObject; override;
  end;

  TFhirCodeSystemConceptDesignation3 = class (TFhirCodeSystemConceptDesignationW)
  public
    function language : String; override;
    function useGen : String; override;
    function use : TFHIRObject; override;
    function value : String; override;
  end;

  TFhirCodeSystemConcept3 = class (TFhirCodeSystemConceptW)
  private
    function c : TFhirCodeSystemConcept;
  public
    function code : String; override;
    function display : String; override;
    function definition : String; override;
    function conceptList : TFslList<TFhirCodeSystemConceptW>; override;
    function concept(ndx : integer) : TFhirCodeSystemConceptW; override;
    function conceptCount : integer; override;
    function hasConcept(c : TFhirCodeSystemConceptW) : boolean; override;
    function designationCount : integer; override;
    function designations : TFslList<TFhirCodeSystemConceptDesignationW>; override;
    function properties : TFslList<TFhirCodeSystemConceptPropertyW>; override;
    function displayTag(tag : String) : String; override;
    procedure setDisplayTag(tag, value : String); override;
  end;


  TFhirCodeSystemProperty3 = class (TFhirCodeSystemPropertyW)
  public
    function code : String; override;
    function type_ : TFhirCodeSystemPropertyType; override;
  end;

  TFhirCodeSystem3 = class (TFhirCodeSystemW)
  private
    function cs : TFhirCodeSystem;
  public
    function url : String; override;
    function name : String; override;
    function version : String; override;
    function description : String; override;
    function copyright : String; override;
    function language : String; override;

    function properties : TFslList<TFhirCodeSystemPropertyW>;  override;
    // this is special because it's owned
    function conceptList : TFslList<TFhirCodeSystemConceptW>; override;
    function concept(ndx : integer) : TFhirCodeSystemConceptW; override;
    function conceptCount : integer; override;
    function hasConcept(c : TFhirCodeSystemConceptW) : boolean; override;

    function isAbstract(c : TFhirCodeSystemConceptW) : boolean; override;
    function getParents(c : TFhirCodeSystemConceptW) : TFslList<TFhirCodeSystemConceptW>; override;
    function getChildren(c : TFhirCodeSystemConceptW) : TFslList<TFhirCodeSystemConceptW>; override;
  end;


  TFHIRLookupOpRespProperty3 = class (TFHIRLookupOpRespPropertyW)
  public
    function GetDescription: string; override;
    procedure SetDescription(const Value: string); override;
    function GetValue: TFHIRObject; override;
    procedure SetValue(const Value: TFHIRObject); override;
  end;

  TFHIRLookupOpRespDesignation3 = class (TFHIRLookupOpRespDesignationW)
  public
    function GetUse: TFHIRObject; override;
    procedure SetUse(const Value: TFHIRObject); override;
  end;

  TFHIRLookupOpResponse3 = class (TFHIRLookupOpResponseW)
  public
    function addProp(name : string) : TFHIRLookupOpRespPropertyW; override;
    function addDesignation(system, code, display, value : string) : TFHIRLookupOpRespDesignationW; overload; override;
    function addDesignation(lang, value : string) : TFHIRLookupOpRespDesignationW; overload; override;
    function GetVersion: String; override;
    procedure SetVersion(const Value: String); override;
    procedure addExtension(name, value : String); overload; override;
    procedure addExtension(name : String; value : boolean); overload; override;
  end;


implementation

uses
  FHIR.R3.Utilities;

{ TFhirOperationOutcome3 }

procedure TFhirOperationOutcome3.addIssue(issue: TFhirOperationOutcomeIssueW);
begin
  (Fres as TFhirOperationOutcome).issueList.Add((issue.Element as TFhirOperationOutcomeIssue).Link);
end;

function TFhirOperationOutcome3.code: TExceptionType;
var
  a : TExceptionType;
  op : TFhirOperationOutcome;
begin
  op := Fres as TFhirOperationOutcome;
  result := etNull;
  if not op.issueList.IsEmpty then
    for a := low(TExceptionType) to High(TExceptionType) do
      if ExceptionTypeTranslations[a] = op.issueList[0].code then
       exit(a);
end;

function TFhirOperationOutcome3.hasText: boolean;
var
  op : TFhirOperationOutcome;
begin
  op := Fres as TFhirOperationOutcome;
  if (op.text <> nil) and (op.text.div_ <> nil) then
    result := true
  else if (op.issueList.Count > 0) and (op.issueList[0].diagnostics <> '') then
    result := true
  else
    result := false;
end;

function TFhirOperationOutcome3.text: String;
var
  op : TFhirOperationOutcome;
begin
  op := Fres as TFhirOperationOutcome;
  if (op.text <> nil) and (op.text.div_ <> nil) then
    result := op.text.div_.AsPlainText
  else if (op.issueList.Count > 0) and (op.issueList[0].diagnostics <> '') then
    result := op.issueList[0].diagnostics
  else
    result := '';
end;

{ TFHIRBundle3 }

procedure TFHIRBundle3.addEntries(bnd: TFHIRResourceV);
var
  b : TFHIRBundle;
begin
  b := bnd as TFHIRBundle;
  bundle.entryList.AddAll(b.entryList);
end;

function TFHIRBundle3.bundle: TFhirBundle;
begin
  result := resource as TFHIRBundle;
end;

procedure TFHIRBundle3.clearLinks;
var
  b : TFHIRBundle;
begin
  b := resource as TFHIRBundle;
  bundle.link_List.Clear;
end;

function TFHIRBundle3.entries: TFslList<TFhirBundleEntryW>;
var
  be : TFHIRBundleEntry;
begin
  result := TFslList<TFhirBundleEntryW>.create;
  try
    for be in bundle.entryList do
      result.Add(TFhirBundleEntry3.create(be.Link));
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRBundle3.GetLink(rel: String): String;
begin
  result := bundle.Links[rel];
end;

procedure TFHIRBundle3.listLinks(links: TDictionary<String, String>);
var
  bl : TFhirBundleLink;
begin
  links.Clear;
  for bl in bundle.link_List do
    links.AddOrSetValue(bl.relation, bl.url);
end;

function TFHIRBundle3.next(bnd: TFHIRResourceV): String;
var
  b : TFHIRBundle;
begin
  b := bnd as TFHIRBundle;
  result := b.Links['next'];
end;

procedure TFHIRBundle3.SetLink(rel: String; const Value: String);
begin
  bundle.Links[rel] := value;
end;

function TFHIRBundle3.total: TFHIRObject;
begin
  result := bundle.totalElement;
end;

{ TFHIROperationOutcomeIssue3 }

function TFHIROperationOutcomeIssue3.display: String;
var
  i : TFHIROperationOutcomeIssue;
begin
  i := issue;
  result := i.diagnostics;
  if (i.details <> nil) and (i.details.text <> '') then
    result := i.details.text;
end;

function TFHIROperationOutcomeIssue3.issue: TFHIROperationOutcomeIssue;
begin
  result := Element as TFHIROperationOutcomeIssue;
end;

function TFHIROperationOutcomeIssue3.severity: TIssueSeverity;
begin
  result := ISSUE_SEVERITY_MAP[issue.severity];
end;


{ TFHIRCapabilityStatement3 }

function TFHIRCapabilityStatement3.hasFormat(fmt: String): boolean;
begin
  result := statement.formatList.hasCode(fmt);
end;

function TFHIRCapabilityStatement3.hasRest: boolean;
var
  cs : TFHIRCapabilityStatement;
begin
  result := statement.restList.Count > 0;
end;

function TFHIRCapabilityStatement3.hasSecurity(system, code: String): boolean;
var
  cs : TFHIRCapabilityStatement;
  cc : TFhirCodeableConcept;
begin
  cs := statement;
  result := false;
  if (cs.restList[0].security <> nil) then
    for cc in cs.restList[0].security.serviceList do
      if cc.hasCode(system, code) then
        exit(true);
end;

procedure TFHIRCapabilityStatement3.listSearchParams(name: String; list: TFslList<TFHIRSearchParamDefinitionW>);
var
  r : TFhirCapabilityStatementRest;
  it : TFhirCapabilityStatementRestInteraction;
  rr : TFhirCapabilityStatementRestResource;
  int : TFhirCapabilityStatementRestResourceInteraction;
  i : TFHIRInteraction;
  sp : TFhirCapabilityStatementRestResourceSearchParam;
begin
  for r in statement.restList do
  begin
    if r.mode = RestfulCapabilityModeServer then
    begin
      if name = 'All Types' then
        for sp in r.searchParamList do
          list.Add(TFHIRSearchParamDefinition3.create(sp.Link))
      else
      begin
        for rr in r.resourceList do
        begin
          if CODES_TFHIRResourceTypesEnum[rr.type_] = name then
            for sp in rr.searchParamList do
              list.Add(TFHIRSearchParamDefinition3.create(sp.Link))
        end;
      end;
    end;
  end;
end;

procedure TFHIRCapabilityStatement3.listTypes(interactions: TFHIRInteractions; names: TStrings);
var
  r : TFhirCapabilityStatementRest;
  it : TFhirCapabilityStatementRestInteraction;
  rr : TFhirCapabilityStatementRestResource;
  ok : boolean;
  int : TFhirCapabilityStatementRestResourceInteraction;
  i : TFHIRInteraction;
begin
  for r in statement.restList do
  begin
    if r.mode = RestfulCapabilityModeServer then
    begin
      ok := false;
      for it in r.interactionList do
        for i in ALL_INTERACTIONS do
          if (i in interactions) and (it.code = INTERACTION_MAP2[i]) then
            ok := true;
      if ok then
        names.Add('All Types');
      for rr in r.resourceList do
      begin
        ok := false;
        for int in rr.interactionList do
          for i in ALL_INTERACTIONS do
            if (i in interactions) and (int.code = INTERACTION_MAP[i]) then
              ok := true;
        if ok then
          names.Add(CODES_TFHIRResourceTypesEnum[rr.type_]);
      end;
    end;
  end;
end;

procedure TFHIRCapabilityStatement3.readSmartExtension(var authorize, token, register: String);
var
  ex1, ex2 : TFhirExtension;
begin
  for ex1 in statement.restList[0].security.extensionList do
    if ex1.url = 'http://fhir-registry.smarthealthit.org/StructureDefinition/oauth-uris' then
      for ex2 in ex1.extensionList do
        if ex2.url = 'authorize' then
          authorize := TFHIRUri(ex2.value).value
        else if ex2.url = 'token' then
          token := TFHIRUri(ex2.value).value
        else if ex2.url = 'register' then
          register := TFHIRUri(ex2.value).value;
end;

function TFHIRCapabilityStatement3.statement: TFhirCapabilityStatement;
begin
  result := FRes as TFHIRCapabilityStatement;
end;

function TFHIRCapabilityStatement3.supportsType(name: String; interaction: TFHIRInteraction): boolean;
var
  r : TFhirCapabilityStatementRest;
  it : TFhirCapabilityStatementRestInteraction;
  rr : TFhirCapabilityStatementRestResource;
  ok : boolean;
  int : TFhirCapabilityStatementRestResourceInteraction;
  i : TFHIRInteraction;
begin
  result := false;
  for r in statement.restList do
  begin
    if r.mode = RestfulCapabilityModeServer then
    begin
      if name = 'All Types' then
      begin
        ok := false;
        for it in r.interactionList do
          for i in ALL_INTERACTIONS do
            if (i = interaction) and (it.code = INTERACTION_MAP2[i]) then
              ok := true;
        exit(ok);
      end
      else
      begin
        for rr in r.resourceList do
        begin
          if CODES_TFHIRResourceTypesEnum[rr.type_] = name then
          begin
            ok := false;
            for int in rr.interactionList do
              for i in ALL_INTERACTIONS do
                if (i = interaction) and (int.code = INTERACTION_MAP[i]) then
                  ok := true;
            if ok then
              exit(ok);
          end;
        end;
      end;
    end;
  end;
end;

{ TFhirParametersParameter3 }

function TFhirParametersParameter3.appendPart(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter3.Create(parameter.partList.Append.Link);
  TFhirParametersParameter3(result).parameter.name := name;
  PartList.Add(result);
end;

function TFhirParametersParameter3.GetParameterParameter(name: String): TFhirParametersParameterW;
var
  t : TFhirParametersParameterW;
begin
  populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(t);
end;

function TFhirParametersParameter3.GetResourceParameter(name: String): TFHIRResourceV;
var
  t : TFhirParametersParameterW;
begin
  populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter3(t).parameter.resource);
end;

function TFhirParametersParameter3.GetStringParameter(name: String): String;
var
  t : TFhirParametersParameterW;
begin
  populateList;
  result := '';
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter3(t).parameter.value.primitiveValue);
end;

function TFhirParametersParameter3.GetValue: TFHIRObject;
begin
  result := parameter.value;
end;

function TFhirParametersParameter3.hasValue: boolean;
begin
  result := parameter.value <> nil;
end;

function TFhirParametersParameter3.name: String;
begin
  result := parameter.name;
end;

function TFhirParametersParameter3.parameter: TFhirParametersParameter;
begin
  result := Element as TFhirParametersParameter;
end;

procedure TFhirParametersParameter3.populateList;
var
  t : TFhirParametersParameter;
begin
  inherited;
  for t in parameter.partList do
    FList.Add(TFhirParametersParameter3.Create(t.Link));
end;

procedure TFhirParametersParameter3.SetValue(const Value: TFHIRObject);
begin
  parameter.value := value as TFHIRType;
end;

{ TFHIRParameters3 }

procedure TFHIRParameters3.addParamBool(name: String; value: boolean);
begin
  parameter.AddParameter(name, TFHIRBoolean.Create(value));
end;

procedure TFHIRParameters3.addParamCode(name, value: string);
begin
  parameter.AddParameter(name, TFHIRCode.Create(value));
end;

procedure TFHIRParameters3.addParamStr(name, value: string);
begin
  parameter.AddParameter(name, TFHIRString.Create(value));
end;

function TFHIRParameters3.appendParameter(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter3.Create(parameter.parameterList.Append.link);
  TFhirParametersParameter3(result).parameter.name := name;
  ParameterList.Add(result);
end;

function TFHIRParameters3.bool(name: String): boolean;
begin
  result := parameter.bool[name];
end;

function TFHIRParameters3.parameter: TFhirParameters;
begin
  result := Resource as TFhirParameters;
end;

procedure TFHIRParameters3.populateList;
var
  t : TFhirParametersParameter;
begin
  inherited;
  for t in parameter.parameterList do
    Flist.Add(TFhirParametersParameter3.Create(t.Link));
end;

function TFHIRParameters3.str(name: String): String;
begin
  result := parameter.str[name];
end;

{ TFHIRStructureDefinition3 }

function TFHIRStructureDefinition3.elements: TFslList<TFHIRElementDefinitionW>;
var
  ed : TFhirElementDefinition;
begin
  result := TFslList<TFHIRElementDefinitionW>.create;
  try
    for ed in sd.snapshot.elementList do
      result.Add(TFhirElementDefinition3.create(ed.Link));
    result.link;
  finally
    result.Free;
  end;
end;

function TFHIRStructureDefinition3.kind: TStructureDefinitionKind;
begin
  case sd.kind of
    StructureDefinitionKindPrimitiveType : result := sdkPrimitive;
    StructureDefinitionKindComplexType :
      if type_ = 'Extension' then
          result := sdkExtension
        else
          result := sdkDataType;
    StructureDefinitionKindResource :result := sdkResource;
    StructureDefinitionKindLogical : result := sdkResource;
  else
    raise EFHIRException.create('Unhandled value');
  end;
end;

function TFHIRStructureDefinition3.name: String;
begin
  result := sd.name;
end;

function TFHIRStructureDefinition3.sd: TFhirStructureDefinition;
begin
  result := resource as TFhirStructureDefinition;
end;

function TFHIRStructureDefinition3.type_: String;
begin
  result := sd.type_;
end;

function TFHIRStructureDefinition3.url: String;
begin
  result := sd.url;
end;

{ TFHIRSearchParamDefinition3 }

function TFHIRSearchParamDefinition3.documentation: String;
begin
  result := param.documentation;
end;

function TFHIRSearchParamDefinition3.name: String;
begin
  result := param.name;
end;

function TFHIRSearchParamDefinition3.param: TFhirCapabilityStatementRestResourceSearchParam;
begin
  result := FElement as TFhirCapabilityStatementRestResourceSearchParam;
end;

function TFHIRSearchParamDefinition3.type_: TFHIRSearchParamType;
begin
  result := MAP_SearchParamType[param.type_];
end;

{ TFhirElementDefinition3 }

function TFhirElementDefinition3.binding: TElementDefinitionBinding;
var
  b : TFhirElementDefinitionBinding;
begin
  b := edefn.binding;
  if b = nil then
    result := edbNone
  else
    result := MAP_ELEMENT_DEFINITION_BINDING[b.strength];
end;

function TFhirElementDefinition3.defn: String;
begin
  result := edefn.definition;
end;

function TFhirElementDefinition3.edefn: TFhirElementDefinition;
begin
  result := element as TFhirElementDefinition;
end;

function TFhirElementDefinition3.explicitTypeName: String;
begin
  result := edefn.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-explicit-type-name');
end;

function TFhirElementDefinition3.isSummary: boolean;
begin
  result := edefn.isSummary;
end;

function TFhirElementDefinition3.max: integer;
begin
  if edefn.max = '*' then
    result := MaxInt
  else
    result := StrToInt(edefn.max);
end;

function TFhirElementDefinition3.min: integer;
begin
  result := StrToInt(edefn.min);
end;

function TFhirElementDefinition3.path: String;
begin
  result := edefn.path;
end;

function TFhirElementDefinition3.typeList: TArray<String>;
var
  ed : TFhirElementDefinition;
  i : integer;
begin
  ed := edefn;
  Setlength(result, ed.type_List.Count);
  for i := 0 to ed.type_List.Count - 1 do
    result[i] := ed.type_List[i].code;
end;

function TFhirElementDefinition3.types: String;
var
  t : TFhirElementDefinitionType;
begin
  result := '';
  for t in edefn.type_List do
  begin
    if result <> '' then
      result := result+'|';
    result := result + t.code;
  end;
end;

function TFhirElementDefinition3.valueSet: String;
var
  b : TFhirElementDefinitionBinding;
begin
  b := edefn.binding;
  result := '';
  if b <> nil then
    if b.valueSet is TFhirUri then
      result := TFhirUri(b.valueSet).value
    else if b.valueSet is TFhirReference then
      result := TFhirReference(b.valueSet).reference
end;

{ TFHIRBundleEntry3 }

function TFHIRBundleEntry3.entry: TFhirBundleEntry;
begin
  result := element as TFhirBundleEntry;
end;

function TFHIRBundleEntry3.resource: TFHIRResourceV;
begin
  result := entry.resource;
end;

function TFHIRBundleEntry3.searchMode: TFHIRBundleEntrySearchMode;
begin
  if entry.search = nil then
    result := smUnknown
  else
    result := MAP_SEARCH_MODE[entry.search.mode];
end;

function TFHIRBundleEntry3.searchModeE: TFHIRObject;
begin
  if entry.search = nil then
    result := nil
  else
    result := entry.search.modeElement;
end;

function TFHIRBundleEntry3.searchScoreE: TFHIRObject;
begin
  if entry.search = nil then
    result := nil
  else
    result := entry.search.scoreElement;
end;

{ TFHIRValueSet3 }

function TFHIRValueSet3.checkCompose(place, role: String): boolean;
begin
  result := vs.compose <> nil;
  if result then
    vs.compose.checkNoModifiers(place, role, []);
end;

procedure TFHIRValueSet3.clearDefinition;
begin
  vs.purpose := '';
  vs.compose := nil;
  vs.description := '';
  vs.contactList.Clear;
  vs.copyright := '';
  vs.publisher := '';
  vs.extensionList.Clear;
  vs.text := nil;
end;

function TFHIRValueSet3.excludes: TFslList<TFhirValueSetComposeIncludeW>;
var
  c : TFhirValueSetComposeInclude;
begin
  result := TFslList<TFhirValueSetComposeIncludeW>.create(vs.compose.excludeList.Count);
  for c in vs.compose.excludeList do
    result.Add(TFhirValueSetComposeInclude3.Create(c.Link));
end;

function TFHIRValueSet3.expansion: TFhirValueSetExpansionW;
begin
  result := TFhirValueSetExpansion3.create(vs.expansion.Link);
end;

function TFHIRValueSet3.forceExpansion: TFhirValueSetExpansionW;
begin
  vs.expansion := TFhirValueSetExpansion.create;
  vs.expansion.timestamp := TDateTimeEx.makeUTC;
  vs.expansion.identifier := NewGuidURN;
  result := TFhirValueSetExpansion3.create(vs.expansion.Link);
end;

function TFHIRValueSet3.hasExpansion: boolean;
begin
  result := vs.expansion <> nil;
end;

function TFHIRValueSet3.imports: TArray<String>;
begin
  SetLength(result, 0);
end;

function TFHIRValueSet3.includes: TFslList<TFhirValueSetComposeIncludeW>;
var
  c : TFhirValueSetComposeInclude;
begin
  result := TFslList<TFhirValueSetComposeIncludeW>.create(vs.compose.includeList.Count);
  for c in vs.compose.includeList do
    result.Add(TFhirValueSetComposeInclude3.Create(c.Link));
end;

function TFHIRValueSet3.inlineCS: TFHIRValueSetCodeSystemW;
begin
  result := nil;
end;

function TFHIRValueSet3.name: String;
begin
  result := vs.name;
end;

function TFHIRValueSet3.url: String;
begin
  result := vs.url;
end;

function TFHIRValueSet3.vs: TFhirValueSet;
begin
  result := Resource as TFHIRValueSet;
end;

{ TFhirValueSetComposeInclude3 }

function TFhirValueSetComposeInclude3.concepts: TFslList<TFhirValueSetComposeIncludeConceptW>;
var
  i : TFhirValueSetComposeIncludeConcept;
begin
  result := TFslList<TFhirValueSetComposeIncludeConceptW>.create((Element as TFhirValueSetComposeInclude).ConceptList.Count);
  for i in (Element as TFhirValueSetComposeInclude).ConceptList do
    result.Add(TFhirValueSetComposeIncludeConcept3.Create(i.Link));
end;

function TFhirValueSetComposeInclude3.filters: TFslList<TFhirValueSetComposeIncludeFilterW>;
var
  i : TFhirValueSetComposeIncludeFilter;
begin
  result := TFslList<TFhirValueSetComposeIncludeFilterW>.create((Element as TFhirValueSetComposeInclude).filterList.Count);
  for i in (Element as TFhirValueSetComposeInclude).filterList do
    result.Add(TFhirValueSetComposeIncludeFilter3.Create(i.Link));
end;

function TFhirValueSetComposeInclude3.hasConcepts: boolean;
begin
  result := (Element as TFhirValueSetComposeInclude).conceptList.Count > 0;
end;

function TFhirValueSetComposeInclude3.hasFilters: boolean;
begin
  result := (Element as TFhirValueSetComposeInclude).filterList.Count > 0;
end;

function TFhirValueSetComposeInclude3.system: String;
begin
  result := (Element as TFhirValueSetComposeInclude).system;
end;

function TFhirValueSetComposeInclude3.valueSets: TArray<String>;
var
  i : integer;
begin
  SetLength(result, TFhirValueSetComposeInclude(element).valueSetList.count);
  for i := 0 to TFhirValueSetComposeInclude(element).valueSetList.count - 1 do
    result[i] :=  TFhirValueSetComposeInclude(element).valueSetList[i].value;
end;

function TFhirValueSetComposeInclude3.version: String;
begin
  result := (Element as TFhirValueSetComposeInclude).version;
end;

{ TFhirValueSetComposeIncludeFilter3 }

function TFhirValueSetComposeIncludeFilter3.op: TFilterOperator;
begin
  result := MAP_TFilterOperator[(Element as TFhirValueSetComposeIncludeFilter).op];
end;

function TFhirValueSetComposeIncludeFilter3.prop: String;
begin
  result := (Element as TFhirValueSetComposeIncludeFilter).property_;
end;

function TFhirValueSetComposeIncludeFilter3.value: String;
begin
  result := (Element as TFhirValueSetComposeIncludeFilter).value;
end;

{ TFhirValueSetComposeIncludeConcept3 }

function TFhirValueSetComposeIncludeConcept3.code: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).code;
end;

function TFhirValueSetComposeIncludeConcept3.designations: TFslList<TFhirValueSetComposeIncludeConceptDesignationW>;
var
  item : TFhirValueSetComposeIncludeConceptDesignation;
begin
  result := TFslList<TFhirValueSetComposeIncludeConceptDesignationW>.create;
  for item in (Element as TFhirValueSetComposeIncludeConcept).designationList do
    result.Add(TFhirValueSetComposeIncludeConceptDesignation3.create(item.Link));
end;

function TFhirValueSetComposeIncludeConcept3.display: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).display;
end;

{ TFHIRLookupOpResponse3 }

function TFHIRLookupOpResponse3.addDesignation(system, code, display, value: string): TFHIRLookupOpRespDesignationW;
var
  p : TFHIRLookupOpRespDesignation;
begin
  p := TFHIRLookupOpRespDesignation.create;
  try
    p.use := TFHIRCoding.Create;
    p.use.system := system;
    p.use.display := display;
    p.use.code := code;
    p.value := value;
    (op as TFHIRLookupOpResponse).designationList.Add(p.link as TFHIRLookupOpRespDesignation);
    result := TFHIRLookupOpRespDesignation3.create(p.Link);
  finally
    p.free;
  end;
end;

function TFHIRLookupOpResponse3.addDesignation(lang, value: string): TFHIRLookupOpRespDesignationW;
var
  p : TFHIRLookupOpRespDesignation;
begin
  p := TFHIRLookupOpRespDesignation.create;
  try
    p.language := lang;
    p.value := value;
    (op as TFHIRLookupOpResponse).designationList.Add(p.link as TFHIRLookupOpRespDesignation);
    result := TFHIRLookupOpRespDesignation3.create(p.Link);
  finally
    p.free;
  end;
end;

procedure TFHIRLookupOpResponse3.addExtension(name: String; value: boolean);
begin
  (op as TFHIRLookupOpResponse).addExtension(name, value);
end;

procedure TFHIRLookupOpResponse3.addExtension(name, value: String);
begin
  (op as TFHIRLookupOpResponse).addExtension(name, value);
end;

function TFHIRLookupOpResponse3.addProp(name: string): TFHIRLookupOpRespPropertyW;
var
  p : TFHIRLookupOpRespProperty_;
begin
  p := TFHIRLookupOpRespProperty_.create;
  try
    p.code := name;
    (op as TFHIRLookupOpResponse).property_List.Add(p.link as TFHIRLookupOpRespProperty_);
    result := TFHIRLookupOpRespProperty3.create(p.Link);
  finally
    p.Free;
  end;
end;

function TFHIRLookupOpResponse3.GetVersion: String;
begin
  result := (op as TFHIRLookupOpResponse).version;
end;

procedure TFHIRLookupOpResponse3.SetVersion(const Value: String);
begin
  (op as TFHIRLookupOpResponse).version := value;
end;

{ TFHIRLookupOpRespDesignation3 }

function TFHIRLookupOpRespDesignation3.GetUse: TFHIRObject;
begin
  result := (op as TFHIRLookupOpRespDesignation).use;
end;

procedure TFHIRLookupOpRespDesignation3.SetUse(const Value: TFHIRObject);
begin
  (op as TFHIRLookupOpRespDesignation).use := value as TFhirCoding;
end;

{ TFHIRLookupOpRespProperty3 }

function TFHIRLookupOpRespProperty3.GetDescription: string;
begin
  result := (op as TFHIRLookupOpRespProperty_).description;
end;

function TFHIRLookupOpRespProperty3.GetValue: TFHIRObject;
begin
  result := (op as TFHIRLookupOpRespProperty_).value;
end;

procedure TFHIRLookupOpRespProperty3.SetDescription(const Value: string);
begin
  (op as TFHIRLookupOpRespProperty_).description := value;
end;

procedure TFHIRLookupOpRespProperty3.SetValue(const Value: TFHIRObject);
begin
  (op as TFHIRLookupOpRespProperty_).value := value as TFhirType;
end;

{ TFHIRExtension3 }

function TFHIRExtension3.url: String;
begin
  result := (Element as TFHIRExtension).url;
end;

function TFHIRExtension3.value: TFHIRObject;
begin
  result := (Element as TFHIRExtension).value;
end;

{ TFHIRCoding3 }

function TFHIRCoding3.code: String;
begin
  result := (element as TFHIRCoding).code;
end;

function TFHIRCoding3.display: String;
begin
  result := (element as TFHIRCoding).display;
end;

function TFHIRCoding3.system: String;
begin
  result := (element as TFHIRCoding).system;
end;

function TFHIRCoding3.version: String;
begin
  result := (element as TFHIRCoding).version;
end;

{ TFhirCodeSystemProperty3 }

function TFhirCodeSystemProperty3.code: String;
begin
  result := (Element as TFhirCodeSystemProperty).code;
end;

function TFhirCodeSystemProperty3.type_: TFhirCodeSystemPropertyType;
begin
  result := MAP_TFhirConceptPropertyTypeEnum[(Element as TFhirCodeSystemProperty).type_];
end;

{ TFhirCodeSystemConceptProperty3 }

function TFhirCodeSystemConceptProperty3.code: String;
begin
  result := (Element as TFhirCodeSystemConceptProperty).code;
end;

function TFhirCodeSystemConceptProperty3.value: TFHIRObject;
begin
  result := (Element as TFhirCodeSystemConceptProperty).value;
end;

{ TFhirCodeSystemConceptDesignation3 }

function TFhirCodeSystemConceptDesignation3.language: String;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).language;
end;

function TFhirCodeSystemConceptDesignation3.use: TFHIRObject;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).use;
end;

function TFhirCodeSystemConceptDesignation3.useGen: String;
begin
  result := gen((Element as TFhirCodeSystemConceptDesignation).use)
end;

function TFhirCodeSystemConceptDesignation3.value: String;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).value;
end;

{ TFhirCodeSystemConcept3 }

function TFhirCodeSystemConcept3.c: TFhirCodeSystemConcept;
begin
  result := Element as TFhirCodeSystemConcept;
end;

function TFhirCodeSystemConcept3.code: String;
begin
  result := c.code;
end;

function TFhirCodeSystemConcept3.concept(ndx: integer): TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConcept3.create((element as TFhirCodeSystemConcept).conceptList[ndx].Link);
end;

function TFhirCodeSystemConcept3.conceptCount: integer;
begin
  result := c.conceptList.Count;
end;

function TFhirCodeSystemConcept3.conceptList: TFslList<TFhirCodeSystemConceptW>;
var
  i : TFHIRCodeSystemConcept;
begin
  if FConceptList = nil then
  begin
    FConceptList := TFslList<TFHIRCodeSystemConceptW>.create;
    for i in (element as TFhirCodeSystemConcept).conceptList do
      FConceptList.Add(TFhirCodeSystemConcept3.create(i.Link));
  end;
  result := FConceptList;
end;

function TFhirCodeSystemConcept3.definition: String;
begin
  result := c.definition;
end;

function TFhirCodeSystemConcept3.designationCount: integer;
begin
  result := c.designationList.Count;
end;

function TFhirCodeSystemConcept3.designations: TFslList<TFhirCodeSystemConceptDesignationW>;
var
  i : TFhirCodeSystemConceptDesignation;
begin
  result := TFslList<TFhirCodeSystemConceptDesignationW>.create;
  for i in c.designationList do
    result.Add(TFhirCodeSystemConceptDesignation3.Create(i.Link));
end;

function TFhirCodeSystemConcept3.display: String;
begin
  result := c.display;
end;

function TFhirCodeSystemConcept3.displayTag(tag: String): String;
begin
  result := c.displayElement.Tags[tag];
end;

function TFhirCodeSystemConcept3.hasConcept(c: TFhirCodeSystemConceptW): boolean;
var
  i : TFHIRCodeSystemConcept;
begin
  result := false;
  for i in (element as TFhirCodeSystemConcept).conceptList do
      if i.code = c.code then
        exit(true);
end;

function TFhirCodeSystemConcept3.properties: TFslList<TFhirCodeSystemConceptPropertyW>;
var
  i : TFhirCodeSystemConceptProperty;
begin
  result := TFslList<TFhirCodeSystemConceptPropertyW>.create;
  for i in c.property_List do
    result.Add(TFhirCodeSystemConceptProperty3.Create(i.Link));
end;

procedure TFhirCodeSystemConcept3.setDisplayTag(tag, value: String);
begin
  c.displayElement.Tags[tag] := value;
end;

{ TFhirCodeSystem3 }

function TFhirCodeSystem3.concept(ndx: integer): TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConcept3.create(cs.conceptList[ndx].Link);
end;

function TFhirCodeSystem3.conceptCount: integer;
begin
  result := cs.conceptList.Count;
end;

function TFhirCodeSystem3.conceptList: TFslList<TFhirCodeSystemConceptW>;
var
  i : TFHIRCodeSystemConcept;
begin
  if FConceptList = nil then
  begin
    FConceptList := TFslList<TFHIRCodeSystemConceptW>.create;
    for i in (resource as TFhirCodeSystem).conceptList do
      FConceptList.Add(TFhirCodeSystemConcept3.create(i.Link));
  end;
  result := FConceptList;
end;

function TFhirCodeSystem3.copyright: String;
begin
  result := cs.copyright;
end;

function TFhirCodeSystem3.cs: TFhirCodeSystem;
begin
  result := Resource as TFhirCodeSystem;
end;

function TFhirCodeSystem3.description: String;
begin
  result := cs.description;
end;

function TFhirCodeSystem3.getChildren(c: TFhirCodeSystemConceptW): TFslList<TFhirCodeSystemConceptW>;
var
  list : TFhirCodeSystemConceptList;
  i :  TFhirCodeSystemConcept;
begin
  list := cs.getChildren(c.element as TFhirCodeSystemConcept);
  try
    result := TFslList<TFhirCodeSystemConceptW>.create;
    try
      for i in list do
        result.Add(TFhirCodeSystemConcept3.Create(i.Link));
      result.link;
    finally
      result.Free;
    end;
  finally
    c.free;
  end;
end;

function TFhirCodeSystem3.getParents(c: TFhirCodeSystemConceptW): TFslList<TFhirCodeSystemConceptW>;
var
  list : TFhirCodeSystemConceptList;
  i :  TFhirCodeSystemConcept;
begin
  list := cs.getParents(c.element as TFhirCodeSystemConcept);
  try
    result := TFslList<TFhirCodeSystemConceptW>.create;
    try
      for i in list do
        result.Add(TFhirCodeSystemConcept3.Create(i.Link));
      result.link;
    finally
      result.Free;
    end;
  finally
    c.free;
  end;
end;

function TFhirCodeSystem3.hasConcept(c: TFhirCodeSystemConceptW): boolean;
var
  i : TFHIRCodeSystemConcept;
begin
  result := false;
  for i in (resource as TFhirCodeSystem).conceptList do
      if i.code = c.code then
        exit(true);
end;

function TFhirCodeSystem3.isAbstract(c: TFhirCodeSystemConceptW): boolean;
begin
  result := cs.isAbstract(c.Element as TFhirCodeSystemConcept);
end;

function TFhirCodeSystem3.language: String;
begin
  result := cs.language;
end;

function TFhirCodeSystem3.name: String;
begin
  result := cs.name;
end;

function TFhirCodeSystem3.properties: TFslList<TFhirCodeSystemPropertyW>;
var
  i : TFhirCodeSystemProperty;
begin
  result := TFslList<TFhirCodeSystemPropertyW>.create;
  for i in cs.property_List do
    result.Add(TFhirCodeSystemProperty3.Create(i.Link));
end;

function TFhirCodeSystem3.url: String;
begin
  result := cs.url;
end;

function TFhirCodeSystem3.version: String;
begin
  result := cs.version;
end;

{ TFhirValueSetExpansion3 }

procedure TFhirValueSetExpansion3.addContains(item: TFhirValueSetExpansionContainsW);
begin
  exp.containsList.Add(item.Element as TFhirValueSetExpansionContains);
end;

function TFhirValueSetExpansion3.addContains: TFhirValueSetExpansionContainsW;
begin
  result := TFhirValueSetExpansionContains3.Create((element as TFhirValueSetExpansionContains).containsList.Append.Link);
end;

procedure TFhirValueSetExpansion3.addParam(name, value: String);
begin
  exp.AddParam(name, value);
end;

function TFhirValueSetExpansion3.contains: TFslList<TFhirValueSetExpansionContainsW>;
var
  item : TFhirValueSetExpansionContains;
begin
  result := TFslList<TFhirValueSetExpansionContainsW>.create;
  for item in (Element as TFhirValueSetExpansion).containsList do
    result.Add(TFhirValueSetExpansionContains3.Create(item.Link));
end;

procedure TFhirValueSetExpansion3.copyParams(source: TFhirValueSetExpansionW);
var
  param : TFhirValueSetExpansionParameter;
begin
  for param in (source.Element as TFhirValueSetExpansion).parameterList do
    (Element as TFhirValueSetExpansion).parameterList.Add(param.Link);
end;

function TFhirValueSetExpansion3.exp: TFhirValueSetExpansion;
begin
  result := element as TFhirValueSetExpansion;
end;

function TFhirValueSetExpansion3.hasParam(name, value: string): boolean;
var
  param : TFhirValueSetExpansionParameter;
begin
  result := false;
  for param in (Element as TFhirValueSetExpansion).parameterList do
    if (param.name = name) and (param.value.primitiveValue = value) then
      exit(true);
end;

function TFhirValueSetExpansion3.hasParam(name: string): boolean;
var
  param : TFhirValueSetExpansionParameter;
begin
  result := false;
  for param in (Element as TFhirValueSetExpansion).parameterList do
    if (param.name = name) then
      exit(true);
end;

{ TFhirValueSetExpansionContains3 }

function TFhirValueSetExpansionContains3.getcode: String;
begin
  result := (Element as TFhirValueSetExpansionContains).code;
end;

function TFhirValueSetExpansionContains3.contains: TFslList<TFhirValueSetExpansionContainsW>;
var
  item : TFhirValueSetExpansionContains;
begin
  result := TFslList<TFhirValueSetExpansionContainsW>.create;
  for item in (Element as TFhirValueSetExpansionContains).containsList do
    result.Add(TFhirValueSetExpansionContains3.Create(item.Link));
end;

function TFhirValueSetExpansionContains3.getdisplay: String;
begin
  result := (Element as TFhirValueSetExpansionContains).display;
end;

function TFhirValueSetExpansionContains3.getsystem: String;
begin
  result := (Element as TFhirValueSetExpansionContains).system;
end;

procedure TFhirValueSetExpansionContains3.SetCode(const Value: String);
begin
  (Element as TFhirValueSetExpansionContains).code := value;
end;

procedure TFhirValueSetExpansionContains3.SetDisplay(const Value: String);
begin
  (Element as TFhirValueSetExpansionContains).display := value;
end;

procedure TFhirValueSetExpansionContains3.SetSystem(const Value: String);
begin
  (Element as TFhirValueSetExpansionContains).system := value;
end;

{ TFhirValueSetComposeIncludeConceptDesignation3 }

function TFhirValueSetComposeIncludeConceptDesignation3.language: String;
begin
  result := (element as TFhirValueSetComposeIncludeConceptDesignation).language;
end;

function TFhirValueSetComposeIncludeConceptDesignation3.value: String;
begin
  result := (element as TFhirValueSetComposeIncludeConceptDesignation).value;
end;

end.
