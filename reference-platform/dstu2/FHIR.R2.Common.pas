unit FHIR.R2.Common;

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
  FHIR.Support.Generics, FHIR.Support.Strings, FHIR.Support.DateTime, FHIR.Support.System,
  FHIR.Base.Objects, FHIR.Base.Common, FHIR.R2.Types, FHIR.R2.Resources, FHIR.R2.Operations;

const
  ExceptionTypeTranslations : array [TExceptionType] of TFhirIssueTypeEnum = (IssueTypeNull, IssueTypeInvalid, IssueTypeStructure, IssueTypeRequired, IssueTypeValue,
    IssueTypeInvariant, IssueTypeSecurity, IssueTypeLogin, IssueTypeUnknown, IssueTypeExpired, IssueTypeForbidden, IssueTypeSuppressed, IssueTypeProcessing,
    IssueTypeNotSupported, IssueTypeDuplicate, IssueTypeNotFound, IssueTypeTooLong, IssueTypeCodeInvalid, IssueTypeExtension, IssueTypeTooCostly, IssueTypeBusinessRule,
    IssueTypeConflict, IssueTypeIncomplete, IssueTypeTransient, IssueTypeLockError, IssueTypeNoStore, IssueTypeException, IssueTypeTimeout, IssueTypeThrottled, IssueTypeInformational);

  ISSUE_SEVERITY_MAP : array [TFhirIssueSeverityEnum] of TIssueSeverity = (isNull, isFatal, isError, isWarning, isInformation);
  ISSUE_SEVERITY_MAP2 : array [TIssueSeverity] of TFhirIssueSeverityEnum = (IssueSeverityNull, IssueSeverityFatal, IssueSeverityError, IssueSeverityWarning, IssueSeverityInformation);
  INTERACTION_MAP : array [TFHIRInteraction] of TFhirTypeRestfulInteractionEnum = (TypeRestfulInteractionRead, TypeRestfulInteractionSearchType, TypeRestfulInteractionHistoryType, TypeRestfulInteractionCreate, TypeRestfulInteractionUpdate, TypeRestfulInteractionDelete, TypeRestfulInteractionNull);
  INTERACTION_MAP2 : array [TFHIRInteraction] of TFhirSystemRestfulInteractionEnum = (SystemRestfulInteractionNull, SystemRestfulInteractionSearchSystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem, SystemRestfulInteractionHistorySystem);
  MAP_SearchParamType : array [TFhirSearchParamTypeEnum] of TFHIRSearchParamType = (sptString, sptNumber, sptDate, sptString, sptToken, sptReference, sptComposite, sptQuantity, sptUri);
  MAP_SEARCH_MODE : array [TFhirSearchEntryModeEnum] of TFHIRBundleEntrySearchMode = (smUnknown, smMatch, smInclude, smOutcome);
  MAP_ELEMENT_DEFINITION_BINDING : array [TFhirBindingStrengthEnum] of TElementDefinitionBinding = (edbNone, edbRequired, edbExtensible, edbPreferred, edpExample);
  MAP_TFilterOperator : array [TFhirFilterOperatorEnum] of TFilterOperator = (foNull, foEqual, foIsA, foIsNotA, foRegex, foIn, foNotIn);
  MAP_TFHIRSearchParamType1 : array [TFhirSearchParamTypeEnum] of TFHIRSearchParamType = (sptNull, sptNumber, sptDate, sptString, sptToken, sptReference, sptComposite, sptQuantity, sptUri);
  MAP_TFHIRSearchParamType2 : array [TFhirSearchParamType] of TFHIRSearchParamTypeEnum = (SearchParamTypeNull, SearchParamTypeNumber, SearchParamTypeDate, SearchParamTypeString, SearchParamTypeToken, SearchParamTypeReference, SearchParamTypeComposite, SearchParamTypeQuantity, SearchParamTypeUri);

type
  TFHIRExtension2 = class (TFHIRExtensionW)
  public
    function url : String; override;
    function value : TFHIRObject; override;
  end;

  TFHIRCoding2 = class (TFHIRCodingW)
  public
    function system : String; override;
    function code : String; override;
    function version : String; override;
    function display : String; override;
  end;

  TFhirOperationOutcome2 = class (TFhirOperationOutcomeW)
  public
    function hasText : boolean; override;
    function text : String; override;
    function code : TExceptionType; override;
    procedure addIssue(issue : TFhirOperationOutcomeIssueW); override;
  end;

  TFHIRBundleEntry2 = class (TFHIRBundleEntryW)
  private
    function entry : TFhirBundleEntry;
  public
    function searchMode : TFHIRBundleEntrySearchMode; override;
    function searchModeE : TFHIRObject; override;
    function searchScoreE : TFHIRObject; override;
    function resource : TFHIRResourceV; override;
  end;


  TFHIRBundle2 = class (TFHIRBundleW)
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

  TFHIROperationOutcomeIssue2 = class (TFHIROperationOutcomeIssueW)
  private
    function issue : TFHIROperationOutcomeIssue;
  public
    function display : String; override;
    function severity : TIssueSeverity; override;
  end;

  TFHIRSearchParamDefinition2 = class (TFHIRSearchParamDefinitionW)
  private
    function param : TFhirConformanceRestResourceSearchParam;
  public
    function name : String; override;
    function documentation : String; override;
    function type_ : TFHIRSearchParamType; override;
  end;

  TFHIRCapabilityStatement2 = class (TFHIRCapabilityStatementW)
  private
    function statement : TFhirConformance;
  public
    function hasRest : boolean; override;
    function hasSecurity(system, code : String) : boolean; override;

    procedure readSmartExtension(var authorize, token, register: String); override;
    function hasFormat(fmt : String) : boolean; override;

    function supportsType(name : String; interaction : TFHIRInteraction) : boolean; override;
    procedure listTypes(interactions : TFHIRInteractions; names : TStrings); override;
    procedure listSearchParams(name : String; list : TFslList<TFHIRSearchParamDefinitionW>); override;
  end;

  TFhirElementDefinition2 = class (TFhirElementDefinitionW)
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

  TFHIRStructureDefinition2 = class (TFhirStructureDefinitionW)
  private
    function sd : TFhirStructureDefinition;
  public
    function kind : TStructureDefinitionKind; override;
    function name : String; override;
    function url : String; override;
    function type_ : String; override;
    function elements : TFslList<TFHIRElementDefinitionW>; override;
  end;

  TFhirParametersParameter2 = class (TFhirParametersParameterW)
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

  TFHIRParameters2 = class (TFHIRParametersW)
  private
    function parameter : TFhirParameters;
  protected
    procedure populateList; override;
  public
    procedure addParamBool(name : String; value : boolean); override;
    procedure addParamStr(name : String; value : string); override;
    procedure addParamCode(name : String; value : string); override;
    function appendParameter(name : String) : TFhirParametersParameterW; override;
  end;

  TFhirValueSetExpansionContains2 = class (TFhirValueSetExpansionContainsW)
  public
    function getsystem : String; override;
    function getcode : String; override;
    function getdisplay : String; override;
    procedure SetCode(const Value: String); override;
    procedure SetDisplay(const Value: String); override;
    procedure SetSystem(const Value: String); override;
    function contains : TFslList<TFhirValueSetExpansionContainsW>; override;
  end;

  TFhirValueSetExpansion2 = class (TFhirValueSetExpansionW)
  private
    function exp : TFhirValueSetExpansion;
  public
    procedure addParam(name, value : String); override;
    function hasParam(name : string) : boolean; overload; override;
    function hasParam(name, value : string) : boolean; overload; override;
    procedure copyParams(source : TFhirValueSetExpansionW); override;
    procedure addContains(item : TFhirValueSetExpansionContainsW); overload; override;
    function addContains : TFhirValueSetExpansionContainsW; overload; override;
    function contains : TFslList<TFhirValueSetExpansionContainsW>; override;
  end;

  TFhirValueSetComposeIncludeFilter2 = class (TFhirValueSetComposeIncludeFilterW)
  public
    function prop : String; override;
    function op : TFilterOperator; override;
    function value : String; override;
  end;

  TFhirValueSetComposeIncludeConcept2 = class (TFhirValueSetComposeIncludeConceptW)
  public
    function code : String; override;
    function display : String; override;
    function designations : TFslList<TFhirValueSetComposeIncludeConceptDesignationW>; override;
  end;

  TFhirValueSetComposeInclude2 = class (TFhirValueSetComposeIncludeW)
  public
    function system : String; override;
    function version : String; override;
    function valueSets : TArray<String>; override;
    function hasConcepts : boolean; override;
    function concepts : TFslList<TFhirValueSetComposeIncludeConceptW>; override;
    function hasFilters : boolean; override;
    function filters : TFslList<TFhirValueSetComposeIncludeFilterW>; override;
  end;

  TFhirCodeSystemConceptDesignation2 = class (TFhirCodeSystemConceptDesignationW)
  public
    function language : String; override;
    function useGen : String; override;
    function use : TFHIRObject; override;
    function value : String; override;
  end;

  TFhirCodeSystemConcept2 = class (TFhirCodeSystemConceptW)
  public
    function code : String; override;
    function display : String; override;
    function definition : String; override;
    function conceptList : TFslList<TFhirCodeSystemConceptW>; override;
    function conceptCount : integer; override;
    function designationCount : integer; override;
    function designations : TFslList<TFhirCodeSystemConceptDesignationW>; override;
    function concept(ndx : integer) : TFhirCodeSystemConceptW; override;
    function hasConcept(c : TFhirCodeSystemConceptW) : boolean; override;
    function properties : TFslList<TFhirCodeSystemConceptPropertyW>; override;
    function displayTag(tag : String) : String; override;
    procedure setDisplayTag(tag, value : String); override;
  end;

  TFHIRValueSetCodeSystem2 = class (TFHIRValueSetCodeSystemW)
  public
    function system : String; override;
    function concepts : TFslList<TFHIRCodeSystemConceptW>; override;
//    function version : String; override;
  end;

  TFhirCodeSystem2 = class (TFhirCodeSystemW)
  private
    function vs : TFhirValueSet;
    function cs : TFHIRValueSetCodeSystem;
  public
    function name : String; override;
    function url : String; override;
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

  TFHIRValueSet2 = class (TFHIRValueSetW)
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

  TFHIRLookupOpRespDesignation2 = class (TFHIRLookupOpRespDesignationW)
  public
    function GetUse: TFHIRObject; override;
    procedure SetUse(const Value: TFHIRObject); override;
  end;

  TFHIRLookupOpResponse2 = class (TFHIRLookupOpResponseW)
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
  FHIR.R2.Utilities;

{ TFhirOperationOutcome2 }

procedure TFhirOperationOutcome2.addIssue(issue: TFhirOperationOutcomeIssueW);
begin
  (Fres as TFhirOperationOutcome).issueList.Add((issue.Element as TFhirOperationOutcomeIssue).Link);
end;

function TFhirOperationOutcome2.code: TExceptionType;
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

function TFhirOperationOutcome2.hasText: boolean;
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

function TFhirOperationOutcome2.text: String;
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

{ TFHIRBundle2 }

procedure TFHIRBundle2.addEntries(bnd: TFHIRResourceV);
var
  b : TFHIRBundle;
begin
  b := bnd as TFHIRBundle;
  bundle.entryList.AddAll(b.entryList);
end;

function TFHIRBundle2.bundle: TFhirBundle;
begin
  result := resource as TFHIRBundle;
end;

procedure TFHIRBundle2.clearLinks;
var
  b : TFHIRBundle;
begin
  b := resource as TFHIRBundle;
  bundle.link_List.Clear;
end;

function TFHIRBundle2.entries: TFslList<TFhirBundleEntryW>;
var
  be : TFHIRBundleEntry;
begin
  result := TFslList<TFhirBundleEntryW>.create;
  try
    for be in bundle.entryList do
      result.Add(TFhirBundleEntry2.create(be.Link));
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRBundle2.GetLink(rel: String): String;
begin
  result := bundle.Links[rel];
end;

procedure TFHIRBundle2.listLinks(links: TDictionary<String, String>);
var
  bl : TFhirBundleLink;
begin
  links.Clear;
  for bl in bundle.link_List do
    links.AddOrSetValue(bl.relation, bl.url);
end;

function TFHIRBundle2.next(bnd: TFHIRResourceV): String;
var
  b : TFHIRBundle;
begin
  b := bnd as TFHIRBundle;
  result := b.Links['next'];
end;

procedure TFHIRBundle2.SetLink(rel: String; const Value: String);
begin
  bundle.Links[rel] := value;
end;

function TFHIRBundle2.total: TFHIRObject;
begin
  result := bundle.totalElement;
end;

{ TFHIROperationOutcomeIssue2 }

function TFHIROperationOutcomeIssue2.display: String;
var
  i : TFHIROperationOutcomeIssue;
begin
  i := issue;
  result := i.diagnostics;
  if (i.details <> nil) and (i.details.text <> '') then
    result := i.details.text;
end;

function TFHIROperationOutcomeIssue2.issue: TFHIROperationOutcomeIssue;
begin
  result := Element as TFHIROperationOutcomeIssue;
end;

function TFHIROperationOutcomeIssue2.severity: TIssueSeverity;
begin
  result := ISSUE_SEVERITY_MAP[issue.severity];
end;


{ TFHIRCapabilityStatement2 }

function TFHIRCapabilityStatement2.hasFormat(fmt: String): boolean;
begin
  result := statement.formatList.hasCode(fmt);
end;

function TFHIRCapabilityStatement2.hasRest: boolean;
var
  cs : TFHIRCapabilityStatement;
begin
  result := statement.restList.Count > 0;
end;

function TFHIRCapabilityStatement2.hasSecurity(system, code: String): boolean;
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

procedure TFHIRCapabilityStatement2.listSearchParams(name: String; list: TFslList<TFHIRSearchParamDefinitionW>);
var
  r : TFhirConformanceRest;
  it : TFhirConformanceRestInteraction;
  rr : TFhirConformanceRestResource;
  int : TFhirConformanceRestResourceInteraction;
  i : TFHIRInteraction;
  sp : TFhirConformanceRestResourceSearchParam;
begin
  for r in statement.restList do
  begin
    if r.mode = RestfulCapabilityModeServer then
    begin
      if name = 'All Types' then
        for sp in r.searchParamList do
          list.Add(TFHIRSearchParamDefinition2.create(sp.Link))
      else
      begin
        for rr in r.resourceList do
        begin
          if CODES_TFHIRResourceTypesEnum[rr.type_] = name then
            for sp in rr.searchParamList do
              list.Add(TFHIRSearchParamDefinition2.create(sp.Link))
        end;
      end;
    end;
  end;
end;

procedure TFHIRCapabilityStatement2.listTypes(interactions: TFHIRInteractions; names: TStrings);
var
  r : TFhirConformanceRest;
  it : TFhirConformanceRestInteraction;
  rr : TFhirConformanceRestResource;
  ok : boolean;
  int : TFhirConformanceRestResourceInteraction;
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

procedure TFHIRCapabilityStatement2.readSmartExtension(var authorize, token, register: String);
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

function TFHIRCapabilityStatement2.statement: TFhirCapabilityStatement;
begin
  result := FRes as TFhirConformance;
end;

function TFHIRCapabilityStatement2.supportsType(name: String; interaction: TFHIRInteraction): boolean;
var
  r : TFhirConformanceRest;
  it : TFhirConformanceRestInteraction;
  rr : TFhirConformanceRestResource;
  ok : boolean;
  int : TFhirConformanceRestResourceInteraction;
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

{ TFhirParametersParameter2 }

function TFhirParametersParameter2.appendPart(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter2.Create(parameter.partList.Append.Link);
  TFhirParametersParameter2(result).parameter.name := name;
  PartList.Add(result);
end;

function TFhirParametersParameter2.GetParameterParameter(name: String): TFhirParametersParameterW;
var
  t : TFhirParametersParameterW;
begin
  populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(t);
end;

function TFhirParametersParameter2.GetResourceParameter(name: String): TFHIRResourceV;
var
  t : TFhirParametersParameterW;
begin
  populateList;
  result := nil;
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter2(t).parameter.resource);
end;

function TFhirParametersParameter2.GetStringParameter(name: String): String;
var
  t : TFhirParametersParameterW;
begin
  populateList;
  result := '';
  for t in FList do
    if t.name = name then
      exit(TFhirParametersParameter2(t).parameter.value.primitiveValue);
end;

function TFhirParametersParameter2.GetValue: TFHIRObject;
begin
  result := parameter.value;
end;

function TFhirParametersParameter2.hasValue: boolean;
begin
  result := parameter.value <> nil;
end;

function TFhirParametersParameter2.name: String;
begin
  result := parameter.name;
end;

function TFhirParametersParameter2.parameter: TFhirParametersParameter;
begin
  result := Element as TFhirParametersParameter;
end;

procedure TFhirParametersParameter2.populateList;
var
  t : TFhirParametersParameter;
begin
  inherited;
  for t in parameter.partList do
    FList.Add(TFhirParametersParameter2.Create(t.Link));
end;

procedure TFhirParametersParameter2.SetValue(const Value: TFHIRObject);
begin
  parameter.value := value as TFHIRType;
end;

{ TFHIRParameters2 }

procedure TFHIRParameters2.addParamBool(name: String; value: boolean);
begin
  parameter.AddParameter(name, TFHIRBoolean.Create(value));
end;

procedure TFHIRParameters2.addParamCode(name, value: string);
begin
  parameter.AddParameter(name, TFHIRCode.Create(value));
end;

procedure TFHIRParameters2.addParamStr(name, value: string);
begin
  parameter.AddParameter(name, TFHIRString.Create(value));
end;

function TFHIRParameters2.appendParameter(name: String): TFhirParametersParameterW;
begin
  result := TFhirParametersParameter2.Create(parameter.parameterList.Append.link);
  TFhirParametersParameter2(result).parameter.name := name;
  ParameterList.Add(result);
end;

function TFHIRParameters2.parameter: TFhirParameters;
begin
  result := Resource as TFhirParameters;
end;

procedure TFHIRParameters2.populateList;
var
  t : TFhirParametersParameter;
begin
  inherited;
  for t in parameter.parameterList do
    FList.Add(TFhirParametersParameter2.Create(t.Link));
end;

{ TFHIRStructureDefinition2 }

function TFHIRStructureDefinition2.elements: TFslList<TFHIRElementDefinitionW>;
var
  ed : TFhirElementDefinition;
begin
  result := TFslList<TFHIRElementDefinitionW>.create;
  try
    for ed in sd.snapshot.elementList do
      result.Add(TFhirElementDefinition2.create(ed.Link));
    result.link;
  finally
    result.Free;
  end;
end;

function TFHIRStructureDefinition2.kind: TStructureDefinitionKind;
begin
  case sd.kind of
    StructureDefinitionKindDatatype :
      if type_ = 'Extension' then
        result := sdkExtension
      else if StringArrayExistsSensitive(['boolean', 'integer', 'string', 'decimal', 'uri', 'base64Binary', 'instant', 'date', 'dateTime', 'time'], type_) then
        result := sdkPrimitive
      else
        result := sdkDataType;
    StructureDefinitionKindResource : result := sdkResource;
    StructureDefinitionKindLogical : result := sdkResource;
  else
    raise Exception.Create('Unknown value');
  end;
end;

function TFHIRStructureDefinition2.name: String;
begin
  result := sd.name;
end;

function TFHIRStructureDefinition2.sd: TFhirStructureDefinition;
begin
  result := resource as TFhirStructureDefinition;
end;

function TFHIRStructureDefinition2.type_: String;
begin
  result := sd.type_;
end;

function TFHIRStructureDefinition2.url: String;
begin
  result := sd.url;
end;

{ TFHIRSearchParamDefinition2 }

function TFHIRSearchParamDefinition2.documentation: String;
begin
  result := param.documentation;
end;

function TFHIRSearchParamDefinition2.name: String;
begin
  result := param.name;
end;

function TFHIRSearchParamDefinition2.param: TFhirConformanceRestResourceSearchParam;
begin
  result := FElement as TFhirConformanceRestResourceSearchParam;
end;

function TFHIRSearchParamDefinition2.type_: TFHIRSearchParamType;
begin
  result := MAP_SearchParamType[param.type_];
end;

{ TFhirElementDefinition2 }

function TFhirElementDefinition2.binding: TElementDefinitionBinding;
var
  b : TFhirElementDefinitionBinding;
begin
  b := edefn.binding;
  if b = nil then
    result := edbNone
  else
    result := MAP_ELEMENT_DEFINITION_BINDING[b.strength];
end;

function TFhirElementDefinition2.defn: String;
begin
  result := edefn.definition;
end;

function TFhirElementDefinition2.edefn: TFhirElementDefinition;
begin
  result := element as TFhirElementDefinition;
end;

function TFhirElementDefinition2.explicitTypeName: String;
begin
  result := edefn.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-explicit-type-name');
end;

function TFhirElementDefinition2.isSummary: boolean;
begin
  result := edefn.isSummary;
end;

function TFhirElementDefinition2.max: integer;
begin
  if edefn.max = '*' then
    result := MaxInt
  else
    result := StrToInt(edefn.max);
end;

function TFhirElementDefinition2.min: integer;
begin
  result := StrToInt(edefn.min);
end;

function TFhirElementDefinition2.path: String;
begin
  result := edefn.path;
end;

function TFhirElementDefinition2.typeList: TArray<String>;
var
  ed : TFhirElementDefinition;
  i : integer;
begin
  ed := edefn;
  Setlength(result, ed.type_List.Count);
  for i := 0 to ed.type_List.Count - 1 do
    result[i] := ed.type_List[i].code;
end;

function TFhirElementDefinition2.types: String;
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

function TFhirElementDefinition2.valueSet: String;
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

{ TFHIRBundleEntry2 }

function TFHIRBundleEntry2.entry: TFhirBundleEntry;
begin
  result := element as TFhirBundleEntry;
end;

function TFHIRBundleEntry2.resource: TFHIRResourceV;
begin
  result := entry.resource;
end;

function TFHIRBundleEntry2.searchMode: TFHIRBundleEntrySearchMode;
begin
  if entry.search = nil then
    result := smUnknown
  else
    result := MAP_SEARCH_MODE[entry.search.mode];
end;


function TFHIRBundleEntry2.searchModeE: TFHIRObject;
begin
  if entry.search = nil then
    result := nil
  else
    result := entry.search.modeElement;
end;

function TFHIRBundleEntry2.searchScoreE: TFHIRObject;
begin
  if entry.search = nil then
    result := nil
  else
    result := entry.search.scoreElement;
end;

{ TFHIRValueSet2 }

function TFHIRValueSet2.checkCompose(place, role: String): boolean;
begin
  result := vs.compose <> nil;
  if result then
    vs.compose.checkNoModifiers(place, role, []);
end;

procedure TFHIRValueSet2.clearDefinition;
begin
  vs.codeSystem := nil;
  vs.requirements := '';
  vs.compose := nil;
  vs.description := '';
  vs.contactList.Clear;
  vs.copyright := '';
  vs.publisher := '';
  vs.extensionList.Clear;
  vs.text := nil;
end;

function TFHIRValueSet2.excludes: TFslList<TFhirValueSetComposeIncludeW>;
var
  c : TFhirValueSetComposeInclude;
begin
  result := TFslList<TFhirValueSetComposeIncludeW>.create(vs.compose.excludeList.Count);
  for c in vs.compose.excludeList do
    result.Add(TFhirValueSetComposeInclude2.Create(c.Link));
end;

function TFHIRValueSet2.expansion: TFhirValueSetExpansionW;
begin
  result := TFhirValueSetExpansion2.create(vs.expansion.Link);
end;

function TFHIRValueSet2.forceExpansion: TFhirValueSetExpansionW;
begin
  vs.expansion := TFhirValueSetExpansion.create;
  vs.expansion.timestamp := TDateTimeEx.makeUTC;
  vs.expansion.identifier := NewGuidURN;
  result := TFhirValueSetExpansion2.create(vs.expansion.Link);
end;

function TFHIRValueSet2.hasExpansion: boolean;
begin
  result := vs.expansion <> nil;
end;

function TFHIRValueSet2.imports: TArray<String>;
var
  i : integer;
begin
  SetLength(result, vs.compose.importList.count);
  for i := 0 to vs.compose.importList.count - 1 do
    result[i] :=  vs.compose.importList[i].value;
end;

function TFHIRValueSet2.includes: TFslList<TFhirValueSetComposeIncludeW>;
var
  c : TFhirValueSetComposeInclude;
begin
  result := TFslList<TFhirValueSetComposeIncludeW>.create(vs.compose.includeList.Count);
  for c in vs.compose.includeList do
    result.Add(TFhirValueSetComposeInclude2.Create(c.Link));
end;

function TFHIRValueSet2.inlineCS: TFHIRValueSetCodeSystemW;
begin
  if vs.codeSystem = nil then
    result := nil
  else
    result := TFHIRValueSetCodeSystem2.create(vs.codeSystem.link);
end;

function TFHIRValueSet2.name: String;
begin
  result := vs.name;
end;

function TFHIRValueSet2.url: String;
begin
  result := vs.url;
end;

function TFHIRValueSet2.vs: TFhirValueSet;
begin
  result := Resource as TFHIRValueSet;
end;


{ TFhirValueSetComposeInclude2 }

function TFhirValueSetComposeInclude2.concepts: TFslList<TFhirValueSetComposeIncludeConceptW>;
var
  i : TFhirValueSetComposeIncludeConcept;
begin
  result := TFslList<TFhirValueSetComposeIncludeConceptW>.create((Element as TFhirValueSetComposeInclude).ConceptList.Count);
  for i in (Element as TFhirValueSetComposeInclude).ConceptList do
    result.Add(TFhirValueSetComposeIncludeConcept2.Create(i.Link));
end;

function TFhirValueSetComposeInclude2.filters: TFslList<TFhirValueSetComposeIncludeFilterW>;
var
  i : TFhirValueSetComposeIncludeFilter;
begin
  result := TFslList<TFhirValueSetComposeIncludeFilterW>.create((Element as TFhirValueSetComposeInclude).filterList.Count);
  for i in (Element as TFhirValueSetComposeInclude).filterList do
    result.Add(TFhirValueSetComposeIncludeFilter2.Create(i.Link));
end;

function TFhirValueSetComposeInclude2.hasConcepts: boolean;
begin
  result := (Element as TFhirValueSetComposeInclude).conceptList.Count > 0;
end;

function TFhirValueSetComposeInclude2.hasFilters: boolean;
begin
  result := (Element as TFhirValueSetComposeInclude).filterList.Count > 0;
end;

function TFhirValueSetComposeInclude2.system: String;
begin
  result := (Element as TFhirValueSetComposeInclude).system;
end;

function TFhirValueSetComposeInclude2.valueSets: TArray<String>;
begin
  SetLength(result, 0);
end;

function TFhirValueSetComposeInclude2.version: String;
begin
  result := (Element as TFhirValueSetComposeInclude).version;
end;

{ TFhirValueSetComposeIncludeFilter2 }

function TFhirValueSetComposeIncludeFilter2.op: TFilterOperator;
begin
  result := MAP_TFilterOperator[(Element as TFhirValueSetComposeIncludeFilter).op];
end;

function TFhirValueSetComposeIncludeFilter2.prop: String;
begin
  result := (Element as TFhirValueSetComposeIncludeFilter).property_;
end;

function TFhirValueSetComposeIncludeFilter2.value: String;
begin
  result := (Element as TFhirValueSetComposeIncludeFilter).value;
end;

{ TFHIRValueSetCodeSystem2 }

function TFHIRValueSetCodeSystem2.concepts: TFslList<TFHIRCodeSystemConceptW>;
var
  i : TFHIRValueSetCodeSystemConcept;
begin
  result := TFslList<TFHIRCodeSystemConceptW>.create;
  for i in (element as TFHIRValueSetCodeSystem).conceptList do
    result.Add(TFhirCodeSystemConcept2.create(i.Link));
end;

function TFHIRValueSetCodeSystem2.system: String;
begin
  result := (element as TFHIRValueSetCodeSystem ).system;
end;

{ TFhirCodeSystemConcept2 }

function TFhirCodeSystemConcept2.code: String;
begin
  result := (Element as TFhirCodeSystemConcept).code;
end;

function TFhirCodeSystemConcept2.concept(ndx: integer): TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConcept2.create((element as TFhirCodeSystemConcept).conceptList[ndx]);
end;

function TFhirCodeSystemConcept2.conceptCount: integer;
begin
  result := (element as TFhirCodeSystemConcept).conceptList.Count;
end;

function TFhirCodeSystemConcept2.conceptList: TFslList<TFhirCodeSystemConceptW>;
var
  i : TFHIRValueSetCodeSystemConcept;
begin
  if FConceptList = nil then
  begin
    FConceptList := TFslList<TFHIRCodeSystemConceptW>.create;
    for i in (element as TFhirCodeSystemConcept).conceptList do
      FConceptList.Add(TFhirCodeSystemConcept2.create(i.Link));
  end;
  result := FConceptList;
end;

function TFhirCodeSystemConcept2.definition: String;
begin
  result := (Element as TFhirCodeSystemConcept).definition;
end;

function TFhirCodeSystemConcept2.designationCount: integer;
begin
  result := (element as TFhirCodeSystemConcept).designationList.Count;
end;

function TFhirCodeSystemConcept2.designations: TFslList<TFhirCodeSystemConceptDesignationW>;
var
  i : TFhirValueSetCodeSystemConceptDesignation;
begin
  result := TFslList<TFhirCodeSystemConceptDesignationW>.create;
  for i in (element as TFhirCodeSystemConcept).designationList do
    result.Add(TFhirCodeSystemConceptDesignation2.create(i.Link));
end;

function TFhirCodeSystemConcept2.display: String;
begin
  result := (Element as TFhirCodeSystemConcept).display;
end;

function TFhirCodeSystemConcept2.displayTag(tag: String): String;
begin
  result := (element as TFhirCodeSystemConcept).displayElement.Tags[tag];
end;

function TFhirCodeSystemConcept2.hasConcept(c: TFhirCodeSystemConceptW): boolean;
var
  i : TFhirValueSetCodeSystemConcept;
begin
  result := false;
  for i in (element as TFhirCodeSystemConcept).conceptList do
    if i.code = c.code then
      exit(true);
end;

function TFhirCodeSystemConcept2.properties: TFslList<TFhirCodeSystemConceptPropertyW>;
begin
  result := TFslList<TFhirCodeSystemConceptPropertyW>.create;
end;

procedure TFhirCodeSystemConcept2.setDisplayTag(tag, value: String);
begin
  if (element as TFhirCodeSystemConcept).displayElement <> Nil then
    (element as TFhirCodeSystemConcept).displayElement.Tags[tag] := value;
end;

{ TFhirValueSetComposeIncludeConcept2 }

function TFhirValueSetComposeIncludeConcept2.code: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).code;
end;

function TFhirValueSetComposeIncludeConcept2.designations: TFslList<TFhirValueSetComposeIncludeConceptDesignationW>;
begin
  result := TFslList<TFhirValueSetComposeIncludeConceptDesignationW>.create;
end;

function TFhirValueSetComposeIncludeConcept2.display: String;
begin
  result := (Element as TFhirValueSetComposeIncludeConcept).display;
end;

{ TFHIRLookupOpResponse2 }

function TFHIRLookupOpResponse2.addDesignation(system, code, display, value: string): TFHIRLookupOpRespDesignationW;
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
    result := TFHIRLookupOpRespDesignation2.create(p.Link);
  finally
    p.free;
  end;
end;

function TFHIRLookupOpResponse2.addDesignation(lang, value: string): TFHIRLookupOpRespDesignationW;
var
  p : TFHIRLookupOpRespDesignation;
begin
  p := TFHIRLookupOpRespDesignation.create;
  try
    p.language := lang;
    p.value := value;
    (op as TFHIRLookupOpResponse).designationList.Add(p.link as TFHIRLookupOpRespDesignation);
    result := TFHIRLookupOpRespDesignation2.create(p.Link);
  finally
    p.free;
  end;
end;

procedure TFHIRLookupOpResponse2.addExtension(name: String; value: boolean);
begin
  (op as TFHIRLookupOpResponse).addExtension(name, value);
end;

procedure TFHIRLookupOpResponse2.addExtension(name, value: String);
begin
  (op as TFHIRLookupOpResponse).addExtension(name, value);
end;

function TFHIRLookupOpResponse2.addProp(name: string): TFHIRLookupOpRespPropertyW;
begin
  raise Exception.Create('Properties are not supported in R2');
end;

function TFHIRLookupOpResponse2.GetVersion: String;
begin
  result := (op as TFHIRLookupOpResponse).version;
end;

procedure TFHIRLookupOpResponse2.SetVersion(const Value: String);
begin
  (op as TFHIRLookupOpResponse).version := value;
end;

{ TFHIRLookupOpRespDesignation2 }

function TFHIRLookupOpRespDesignation2.GetUse: TFHIRObject;
begin
  result := (op as TFHIRLookupOpRespDesignation).use;
end;

procedure TFHIRLookupOpRespDesignation2.SetUse(const Value: TFHIRObject);
begin
  (op as TFHIRLookupOpRespDesignation).use := value as TFhirCoding;
end;

{ TFhirCodeSystemConceptDesignation2 }

function TFhirCodeSystemConceptDesignation2.language: String;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).language;
end;

function TFhirCodeSystemConceptDesignation2.use: TFHIRObject;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).use;
end;

function TFhirCodeSystemConceptDesignation2.useGen: String;
begin
  result := gen((Element as TFhirCodeSystemConceptDesignation).use);
end;

function TFhirCodeSystemConceptDesignation2.value: String;
begin
  result := (Element as TFhirCodeSystemConceptDesignation).value;
end;

{ TFHIRExtension2 }

function TFHIRExtension2.url: String;
begin
  result := (Element as TFHIRExtension).url;
end;

function TFHIRExtension2.value: TFHIRObject;
begin
  result := (Element as TFHIRExtension).value;
end;

{ TFHIRCoding2 }

function TFHIRCoding2.code: String;
begin
  result := (element as TFHIRCoding).code;
end;

function TFHIRCoding2.display: String;
begin
  result := (element as TFHIRCoding).display;
end;

function TFHIRCoding2.system: String;
begin
  result := (element as TFHIRCoding).system;
end;

function TFHIRCoding2.version: String;
begin
  result := (element as TFHIRCoding).version;
end;

{ TFhirCodeSystem2 }

function TFhirCodeSystem2.concept(ndx: integer): TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConcept2.create(cs.conceptList[ndx].Link);
end;

function TFhirCodeSystem2.conceptCount: integer;
begin
  result := cs.conceptList.Count;
end;

function TFhirCodeSystem2.conceptList: TFslList<TFhirCodeSystemConceptW>;
var
  i : TFHIRCodeSystemConcept;
begin
  if FConceptList = nil then
  begin
    FConceptList := TFslList<TFHIRCodeSystemConceptW>.create;
    for i in (resource as TFhirCodeSystem).conceptList do
      FConceptList.Add(TFhirCodeSystemConcept2.create(i.Link));
  end;
  result := FConceptList;
end;

function TFhirCodeSystem2.copyright: String;
begin
  result := vs.copyright;
end;

function TFhirCodeSystem2.cs: TFHIRValueSetCodeSystem;
begin
  result := vs.codeSystem;
  assert(result <> nil);
end;

function TFhirCodeSystem2.description: String;
begin
  result := vs.description;
end;

function TFhirCodeSystem2.getChildren(c: TFhirCodeSystemConceptW): TFslList<TFhirCodeSystemConceptW>;
var
  list : TFhirCodeSystemConceptList;
  i :  TFhirCodeSystemConcept;
begin
  list := cs.getChildren(c.element as TFhirCodeSystemConcept);
  try
    result := TFslList<TFhirCodeSystemConceptW>.create;
    try
      for i in list do
        result.Add(TFhirCodeSystemConcept2.Create(i.Link));
      result.link;
    finally
      result.Free;
    end;
  finally
    c.free;
  end;
end;

function TFhirCodeSystem2.getParents(c: TFhirCodeSystemConceptW): TFslList<TFhirCodeSystemConceptW>;
var
  list : TFhirCodeSystemConceptList;
  i :  TFhirCodeSystemConcept;
begin
  list := cs.getParents(c.element as TFhirCodeSystemConcept);
  try
    result := TFslList<TFhirCodeSystemConceptW>.create;
    try
      for i in list do
        result.Add(TFhirCodeSystemConcept2.Create(i.Link));
      result.link;
    finally
      result.Free;
    end;
  finally
    c.free;
  end;
end;

function TFhirCodeSystem2.hasConcept(c: TFhirCodeSystemConceptW): boolean;
var
  i : TFHIRCodeSystemConcept;
begin
  result := false;
  for i in (resource as TFhirCodeSystem).conceptList do
      if i.code = c.code then
        exit(true);
end;

function TFhirCodeSystem2.isAbstract(c: TFhirCodeSystemConceptW): boolean;
begin
  result := (c.Element as TFhirCodeSystemConcept).abstract;;
end;

function TFhirCodeSystem2.language: String;
begin
  result := vs.language;
end;

function TFhirCodeSystem2.name: String;
begin
  result := vs.name;
end;

function TFhirCodeSystem2.properties: TFslList<TFhirCodeSystemPropertyW>;
begin
  result := TFslList<TFhirCodeSystemPropertyW>.create;
end;

function TFhirCodeSystem2.url: String;
begin
  result := cs.system;
end;

function TFhirCodeSystem2.version: String;
begin
  result := vs.version;
end;

function TFhirCodeSystem2.vs: TFhirValueSet;
begin
  result := resource as TFHIRValueSet;
end;

{ TFhirValueSetExpansion2 }

procedure TFhirValueSetExpansion2.addContains(item: TFhirValueSetExpansionContainsW);
begin
  exp.containsList.Add(item.Element as TFhirValueSetExpansionContains);
end;

function TFhirValueSetExpansion2.addContains: TFhirValueSetExpansionContainsW;
begin
  result := TFhirValueSetExpansionContains2.Create((element as TFhirValueSetExpansionContains).containsList.Append.Link);
end;

procedure TFhirValueSetExpansion2.addParam(name, value: String);
begin
  exp.AddParam(name, value);
end;

function TFhirValueSetExpansion2.contains: TFslList<TFhirValueSetExpansionContainsW>;
var
  item : TFhirValueSetExpansionContains;
begin
  result := TFslList<TFhirValueSetExpansionContainsW>.create;
  for item in (Element as TFhirValueSetExpansion).containsList do
    result.Add(TFhirValueSetExpansionContains2.Create(item.Link));
end;

procedure TFhirValueSetExpansion2.copyParams(source: TFhirValueSetExpansionW);
var
  param : TFhirValueSetExpansionParameter;
begin
  for param in (source.Element as TFhirValueSetExpansion).parameterList do
    (Element as TFhirValueSetExpansion).parameterList.Add(param.Link);
end;

function TFhirValueSetExpansion2.exp: TFhirValueSetExpansion;
begin
  result := element as TFhirValueSetExpansion;
end;

function TFhirValueSetExpansion2.hasParam(name, value: string): boolean;
var
  param : TFhirValueSetExpansionParameter;
begin
  result := false;
  for param in (Element as TFhirValueSetExpansion).parameterList do
    if (param.name = name) and (param.value.primitiveValue = value) then
      exit(true);
end;

function TFhirValueSetExpansion2.hasParam(name: string): boolean;
var
  param : TFhirValueSetExpansionParameter;
begin
  result := false;
  for param in (Element as TFhirValueSetExpansion).parameterList do
    if (param.name = name) then
      exit(true);
end;

{ TFhirValueSetExpansionContains2 }

function TFhirValueSetExpansionContains2.getcode: String;
begin
  result := (Element as TFhirValueSetExpansionContains).code;
end;

function TFhirValueSetExpansionContains2.contains: TFslList<TFhirValueSetExpansionContainsW>;
var
  item : TFhirValueSetExpansionContains;
begin
  result := TFslList<TFhirValueSetExpansionContainsW>.create;
  for item in (Element as TFhirValueSetExpansionContains).containsList do
    result.Add(TFhirValueSetExpansionContains2.Create(item.Link));
end;

function TFhirValueSetExpansionContains2.getdisplay: String;
begin
  result := (Element as TFhirValueSetExpansionContains).display;
end;

function TFhirValueSetExpansionContains2.getsystem: String;
begin
  result := (Element as TFhirValueSetExpansionContains).system;
end;

procedure TFhirValueSetExpansionContains2.SetCode(const Value: String);
begin
  (Element as TFhirValueSetExpansionContains).code := value;
end;

procedure TFhirValueSetExpansionContains2.SetDisplay(const Value: String);
begin
  (Element as TFhirValueSetExpansionContains).display := value;
end;

procedure TFhirValueSetExpansionContains2.SetSystem(const Value: String);
begin
  (Element as TFhirValueSetExpansionContains).system := value;
end;

end.
