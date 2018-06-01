unit FHIR.Base.Common;

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
  FHIR.Support.Objects, FHIR.Support.Generics, FHIR.Support.DateTime,
  FHIR.Base.Objects;

Type
  TFilterOperator = (foNull, foEqual, foIsA, foDescendentOf, foIsNotA, foRegex, foIn, foNotIn, foGeneralizes, foExists);

const
  CODES_TFhirFilterOperator: Array[TFilterOperator] of String = ('', '=', 'is-a', 'descendent-of', 'is-not-a', 'regex', 'in', 'not-in', 'generalizes', 'exists');

type
  // base wrappers.....
  TFHIRXVersionElementWrapper = class (TFHIRObject)
  protected
    FElement : TFHIRObject;
    function GetFhirObjectVersion: TFHIRVersion; override;
  public
    constructor Create(elem : TFHIRObject);
    destructor Destroy; override;

    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function fhirType : String; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;

    property Element : TFHIRObject read FElement;

    // extensions:
    function hasExtension(url : String) : boolean; override;
    function getExtensionString(url : String) : String; override;
    function extensionCount(url : String) : integer; override;
    function extensions(url : String) : TFslList<TFHIRObject>; override;
    procedure addExtension(url : String; value : TFHIRObject); override;
  end;

  TFHIRXVersionResourceWrapper = class (TFHIRObject)
  protected
    FRes : TFHIRResourceV;
    function GetFhirObjectVersion: TFHIRVersion; override;
  public
    constructor Create(res : TFHIRResourceV);
    destructor Destroy; override;

    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function fhirType : String; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;

    property Resource : TFHIRResourceV read FRes;
  end;

  TFHIRXVersionOperationWrapper = class (TFsLObject)
  protected
    FOp : TFslObject;
  public
    constructor Create(res : TFslObject);
    destructor Destroy; override;

    property Op : TFslObject read FOp;
  end;

  // types....
  TFhirExtensionW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirExtensionW; overload;
    function url : String; virtual; abstract;
    function value : TFHIRObject; virtual; abstract;
  end;

  TFhirCodingW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirCodingW; overload;
    function system : String; virtual; abstract;
    function code : String; virtual; abstract;
    function version : String; virtual; abstract;
    function display : String; virtual; abstract;
  end;

  // types....
  TFhirCodeableConceptW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirCodeableConceptW; overload;
    function codingCount : integer; virtual; abstract;
    function codings : TFslList<TFhirCodingW>; virtual; abstract;
    function summary : String; virtual; abstract;
  end;

  TFhirOperationOutcomeIssueW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirOperationOutcomeIssueW; overload;


    function display : String; virtual; abstract;
    function severity : TIssueSeverity; virtual; abstract;
  end;

  TFhirOperationOutcomeW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFhirOperationOutcomeW; overload;

    function hasText : boolean; virtual; abstract;
    function text : String; virtual; abstract;
    function code :  TExceptionType; virtual; abstract;

    procedure addIssue(issue : TFhirOperationOutcomeIssueW); virtual; abstract;
  end;
  TFhirOperationOutcomeWClass = class of TFhirOperationOutcomeW;


  TFHIRBundleEntrySearchMode = (smUnknown, smMatch, smInclude, smOutcome);

  TFhirBundleEntryW = class (TFHIRXVersionElementWrapper)
  public
    function Link : TFhirBundleEntryW; overload;
    function searchMode : TFHIRBundleEntrySearchMode; virtual; abstract;
    function searchModeE : TFHIRObject; virtual; abstract;
    function searchScoreE : TFHIRObject; virtual; abstract;
    function resource : TFHIRResourceV; virtual; abstract;
  end;

  TFHIRBundleW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFHIRBundleW;
    function next : String; overload;
    function next(bnd : TFHIRResourceV) : String; overload; virtual; abstract;
    procedure addEntries(bnd : TFHIRResourceV); virtual; abstract;
    procedure clearLinks; virtual; abstract;
    function entries : TFslList<TFhirBundleEntryW>; virtual; abstract;
    procedure listLinks(links : TDictionary<String, String>); virtual; abstract;
    function GetLink(rel: String): String; virtual; abstract;
    procedure SetLink(rel: String; const Value: String); virtual; abstract;
    property links[rel : String] : String read GetLink write SetLink;
    function total : TFHIRObject; virtual; abstract;
  end;
  TFHIRBundleWClass = class of TFHIRBundleW;

  TFHIRSearchParamType = (sptNull, sptString, sptToken, sptComposite, sptUri, sptReference, sptNumber, sptDate, sptQuantity);
  TFhirSearchParamTypeList = set of TFhirSearchParamType;
  TFhirSearchXpathUsage = (sxpNull,  sxpNormal, sxpPhonetic, sxpNearby, sxpDistance, sxpOther);

const
  CODES_TFhirSearchParamType : Array[TFhirSearchParamType] of String = ('', 'string', 'token', 'composite', 'uri', 'reference', 'number', 'date', 'quantity');

type
  TFhirSearchParameterW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFhirSearchParameterW;
    function name : String;  virtual; abstract;
    function description : String;  virtual; abstract;
    function type_ : TFHIRSearchParamType;  virtual; abstract;
    function xpathUsage : TFhirSearchXpathUsage;  virtual; abstract;
    function targets : String; virtual; abstract;{var
  targets : TArray<String>;
  i : integer;
  SetLength(targets, sp.targetList.Count);
  for i := 0 to sp.targetList.Count - 1 do
    targets[i] := sp.targetList[i].value;

}
  end;

  TFHIRSearchParamDefinitionW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFHIRSearchParamDefinitionW; overload;

    function name : String; virtual; abstract;
    function documentation : String; virtual; abstract;
    function type_ : TFHIRSearchParamType; virtual; abstract;
  end;

  TFHIRInteraction = (fiRead, fiSearch, fiHistory, fiCreate, fiUpdate, fiDelete, fiPatch);
  TFHIRInteractions = set of TFHIRInteraction;

const
  ALL_INTERACTIONS = [fiRead..fiDelete];

type
  TFHIRCapabilityStatementW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFHIRCapabilityStatementW; overload;

    function hasRest : boolean; virtual; abstract;
    function hasSecurity(system, code : String) : boolean; virtual; abstract;
    procedure readSmartExtension(var authorize, token, register: String); virtual; abstract;
    function hasFormat(fmt : String) : boolean; virtual; abstract;

    function supportsType(name : String; interaction : TFHIRInteraction) : boolean; virtual; abstract;
    procedure listTypes(interactions : TFHIRInteractions; names : TStrings); virtual; abstract;
    procedure listSearchParams(name : String; list : TFslList<TFHIRSearchParamDefinitionW>); virtual; abstract;
  end;

  TFhirParametersParameterW = class (TFHIRXVersionElementWrapper)
  protected
    FList : TFslList<TFhirParametersParameterW>;
    function GetValue: TFHIRObject; virtual; abstract;
    procedure SetValue(const Value: TFHIRObject); virtual; abstract;
    procedure populateList; virtual;
    function GetParameterParameter(name: String): TFhirParametersParameterW;  virtual; abstract;
    function GetResourceParameter(name: String): TFHIRResourceV;  virtual; abstract;
    function GetStringParameter(name: String): String;  virtual; abstract;
  public
    destructor Destroy; override;
    function link : TFhirParametersParameterW; overload;

    function name : String; virtual; abstract;
    function hasValue : boolean;  virtual; abstract;
    property value : TFHIRObject read GetValue write SetValue;

    property res[name : String] : TFHIRResourceV read GetResourceParameter;
    property str[name : String] : String read GetStringParameter;
    property param[name : String] : TFhirParametersParameterW read GetParameterParameter;

    function partList : TFslList<TFhirParametersParameterW>;
    function appendPart(name : String) : TFhirParametersParameterW; virtual; abstract;
  end;

  TFHIRParametersW = class (TFHIRXVersionResourceWrapper)
  protected
    FList : TFslList<TFhirParametersParameterW>;
    procedure populateList; virtual;
  public
    destructor Destroy; override;
    function link : TFHIRParametersW; overload;

    function bool(name : String) : boolean; virtual; abstract;
    function str(name : String) : String; virtual; abstract;

    procedure addParamBool(name : String; value : boolean); virtual; abstract;
    procedure addParamStr(name : String; value : string); virtual; abstract;
    procedure addParamCode(name : String; value : string); virtual; abstract;

    function parameterList : TFslList<TFhirParametersParameterW>;
    function appendParameter(name : String) : TFhirParametersParameterW; virtual; abstract;
  end;

  TFhirCodeSystemConceptPropertyW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirCodeSystemConceptPropertyW; overload;
    function code : String; virtual; abstract;
    function value : TFHIRObject; virtual; abstract;
  end;

  TFhirCodeSystemConceptDesignationW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirCodeSystemConceptDesignationW; overload;
    function language : String; virtual; abstract;
    function useGen : String; virtual; abstract;
    function use : TFHIRObject; virtual; abstract;
    function value : String; virtual; abstract;
  end;

  TFhirCodeSystemConceptW = class (TFHIRXVersionElementWrapper)
  protected
    FConceptList : TFslList<TFhirCodeSystemConceptW>;
  public
    destructor Destroy; override;
  public
    function link : TFhirCodeSystemConceptW; overload;
    function code : String; virtual; abstract;
    function display : String; virtual; abstract;
    function definition : String; virtual; abstract;
    function conceptList : TFslList<TFhirCodeSystemConceptW>; virtual; abstract;
    function concept(ndx : integer) : TFhirCodeSystemConceptW; virtual; abstract;
    function conceptCount : integer; virtual; abstract;
    function hasConcept(c : TFhirCodeSystemConceptW) : boolean; virtual; abstract;
    function designationCount : integer; virtual; abstract;
    function designations : TFslList<TFhirCodeSystemConceptDesignationW>; virtual; abstract;
    function properties : TFslList<TFhirCodeSystemConceptPropertyW>; virtual; abstract;
    function displayTag(tag : String) : String; virtual; abstract;
    procedure setDisplayTag(tag, value : String); virtual; abstract;
  end;

  TFhirCodeSystemPropertyType = (cptNull, cptCode, cptCoding, cptString, cptInteger, cptBoolean, cptDateTime, cptDecimal);
  TFhirCodeSystemPropertyW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirCodeSystemPropertyW; overload;
    function code : String; virtual; abstract;
    function type_ : TFhirCodeSystemPropertyType; virtual; abstract;
  end;

  TFhirCodeSystemW = class (TFHIRXVersionResourceWrapper)
  protected
    FConceptList : TFslList<TFhirCodeSystemConceptW>;
  public
    destructor Destroy; override;
    function link : TFhirCodeSystemW; overload;
    function name : String; virtual; abstract;
    function url : String; virtual; abstract;
    function version : String; virtual; abstract;
    function description : String; virtual; abstract;
    function copyright : String; virtual; abstract;
    function language : String; virtual; abstract;

    function properties : TFslList<TFhirCodeSystemPropertyW>;  virtual; abstract;
    // this is special because it's owned
    function conceptList : TFslList<TFhirCodeSystemConceptW>; virtual; abstract;
    function concept(ndx : integer) : TFhirCodeSystemConceptW; virtual; abstract;
    function conceptCount : integer; virtual; abstract;
    function hasConcept(c : TFhirCodeSystemConceptW) : boolean; virtual; abstract;

    function isAbstract(c : TFhirCodeSystemConceptW) : boolean; virtual; abstract;
    function getParents(c : TFhirCodeSystemConceptW) : TFslList<TFhirCodeSystemConceptW>; virtual; abstract;
    function getChildren(c : TFhirCodeSystemConceptW) : TFslList<TFhirCodeSystemConceptW>; virtual; abstract;
  end;

  TFhirValueSetExpansionContainsW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirValueSetExpansionContainsW; overload;
    function getSystem : String; virtual; abstract;
    function getCode : String; virtual; abstract;
    function getDisplay : String; virtual; abstract;
    procedure SetCode(const Value: String); virtual; abstract;
    procedure SetDisplay(const Value: String); virtual; abstract;
    procedure SetSystem(const Value: String); virtual; abstract;

    property system : String read GetSystem write SetSystem;
    property code : String read GetCode write SetCode;
    property display : String read GetDisplay write SetDisplay;

    function contains : TFslList<TFhirValueSetExpansionContainsW>; virtual; abstract;
  end;

  TFhirValueSetExpansionW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirValueSetExpansionW; overload;
    procedure addParam(name, value : String); virtual; abstract;
    function hasParam(name : string) : boolean; overload; virtual; abstract;
    function hasParam(name, value : string) : boolean; overload; virtual; abstract;
    procedure copyParams(source : TFhirValueSetExpansionW); virtual; abstract;
    procedure addContains(item : TFhirValueSetExpansionContainsW); overload; virtual; abstract;
    function addContains : TFhirValueSetExpansionContainsW; overload; virtual; abstract;
    function contains : TFslList<TFhirValueSetExpansionContainsW>; virtual; abstract;
  end;

  TFhirValueSetComposeIncludeFilterW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirValueSetComposeIncludeFilterW; overload;
    function prop : String; virtual; abstract;
    function op : TFilterOperator; virtual; abstract;
    function value : String; virtual; abstract;
  end;

  TFhirValueSetComposeIncludeConceptDesignationW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirValueSetComposeIncludeConceptDesignationW; overload;
    function language : String; virtual; abstract;
    function value : String; virtual; abstract;
  end;

  TFhirValueSetComposeIncludeConceptW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirValueSetComposeIncludeConceptW; overload;
    function code : String; virtual; abstract;
    function display : String; virtual; abstract;
    function designations : TFslList<TFhirValueSetComposeIncludeConceptDesignationW>; virtual; abstract;
  end;

  TFhirValueSetComposeIncludeW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirValueSetComposeIncludeW; overload;
    function system : String; virtual; abstract;
    function version : String; virtual; abstract;
    function valueSets : TArray<String>; virtual; abstract;
    function hasConcepts : boolean; virtual; abstract;
    function concepts : TFslList<TFhirValueSetComposeIncludeConceptW>; virtual; abstract;
    function hasFilters : boolean; virtual; abstract;
    function filters : TFslList<TFhirValueSetComposeIncludeFilterW>; virtual; abstract;
  end;

  TFHIRValueSetCodeSystemW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFHIRValueSetCodeSystemW; overload;
    function system : String; virtual; abstract;
    function concepts : TFslList<TFHIRCodeSystemConceptW>; virtual; abstract;
  end;

  TFhirValueSetW =  class (TFHIRXVersionResourceWrapper)
  public
    function link : TFhirValueSetW; overload;
    function name : string; virtual; abstract;
    function url : String; virtual; abstract;

    function checkCompose(place, role : String) : boolean; virtual; abstract;
    function imports : TArray<String>; virtual; abstract; // only in R2
    function inlineCS : TFHIRValueSetCodeSystemW; virtual; abstract;
    function includes : TFslList<TFhirValueSetComposeIncludeW>; virtual; abstract;
    function excludes : TFslList<TFhirValueSetComposeIncludeW>; virtual; abstract;

    procedure clearDefinition; virtual; abstract;
    function hasExpansion : boolean; virtual; abstract;
    function expansion : TFhirValueSetExpansionW; virtual; abstract;
    function forceExpansion : TFhirValueSetExpansionW; virtual; abstract;
  end;

  TElementDefinitionBinding = (edbNone, edbRequired, edbExtensible, edbPreferred, edpExample);

  TFHIRElementDefinitionW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFHIRElementDefinitionW; overload;
    function path : String; virtual; abstract;
    function min : integer; virtual; abstract;
    function max : integer; virtual; abstract;
    function defn : String; virtual; abstract;
    function types : String; virtual; abstract;
    function typeList : TArray<String>; virtual; abstract;
    function explicitTypeName : String; virtual; abstract;
    function isSummary : boolean; virtual; abstract;
    function binding : TElementDefinitionBinding; virtual; abstract;
    function valueSet : String; virtual; abstract;
  end;

  TStructureDefinitionKind = (sdkPrimitive, sdkDataType, sdkExtension, sdkResource);

  TFhirStructureDefinitionW =  class (TFHIRXVersionResourceWrapper)
  public
    function kind : TStructureDefinitionKind; virtual; abstract;
    function name : String; virtual; abstract;
    function url : String; virtual; abstract;
    function type_ : String; virtual; abstract;
    function elements : TFslList<TFHIRElementDefinitionW>; virtual; abstract;
  end;

  TFhirPatientW = class (TFHIRXVersionResourceWrapper)
  public
    function Link : TFhirPatientW; overload;
  end;

  TFHIRLookupOpRespPropertyW = class (TFHIRXVersionOperationWrapper)
  public
    function link : TFHIRLookupOpRespPropertyW; overload;
    function GetDescription: string; virtual; abstract;
    procedure SetDescription(const Value: string); virtual; abstract;
    function GetValue: TFHIRObject; virtual; abstract;
    procedure SetValue(const Value: TFHIRObject); virtual; abstract;

    property description : string read GetDescription write SetDescription;
    property value : TFHIRObject read GetValue write SetValue;
  end;

  TFHIRLookupOpRespDesignationW = class (TFHIRXVersionOperationWrapper)
  public
    function link : TFHIRLookupOpRespDesignationW; overload;
    function GetUse: TFHIRObject; virtual; abstract;
    procedure SetUse(const Value: TFHIRObject); virtual; abstract;

    property use : TFHIRObject read GetUse write SetUse;
  end;

  TFHIRLookupOpResponseW = class (TFHIRXVersionOperationWrapper)
  private
  public
    function link : TFHIRLookupOpResponseW; overload;
    function addProp(name : string) : TFHIRLookupOpRespPropertyW; virtual; abstract;
    function addDesignation(system, code, display, value : string) : TFHIRLookupOpRespDesignationW; overload; virtual; abstract;
    function addDesignation(lang, value : string) : TFHIRLookupOpRespDesignationW; overload; virtual; abstract;
    function GetVersion: String; virtual; abstract;
    procedure SetVersion(const Value: String); virtual; abstract;
    procedure addExtension(name, value : String); overload; virtual; abstract;
    procedure addExtension(name : String; value : boolean); overload; virtual; abstract;

    property version : String read GetVersion write SetVersion;
  end;

implementation

{ TFHIRXVersionResourceWrapper }

constructor TFHIRXVersionResourceWrapper.create(res: TFHIRResourceV);
begin
  inherited create;
  if (res = nil) then
    raise Exception.Create('A value must be provided');
  FRes := res;
end;

destructor TFHIRXVersionResourceWrapper.Destroy;
begin
  FRes.Free;
  inherited;
end;

function TFHIRXVersionResourceWrapper.createPropertyValue(propName: string): TFHIRObject;
begin
  result := FRes.createPropertyValue(propName);
end;

function TFHIRXVersionResourceWrapper.fhirType: String;
begin
  result := FRes.fhirType;
end;

function TFHIRXVersionResourceWrapper.getId: String;
begin
  result := FRes.getId;
end;

function TFHIRXVersionResourceWrapper.GetFhirObjectVersion: TFHIRVersion;
begin
  result := FRes.FhirObjectVersion;
end;

function TFHIRXVersionResourceWrapper.makeCodeValue(v: String): TFHIRObject;
begin
  result := FRes.makeCodeValue(v);
end;

function TFHIRXVersionResourceWrapper.makeIntValue(v: String): TFHIRObject;
begin
  result := FRes.makeIntValue(v);
end;

function TFHIRXVersionResourceWrapper.makeStringValue(v: String): TFHIRObject;
begin
  result := FRes.makeStringValue(v);
end;

procedure TFHIRXVersionResourceWrapper.setIdValue(id: String);
begin
  FRes.SetIdValue(id);
end;

procedure TFHIRXVersionResourceWrapper.setProperty(propName: string; propValue: TFHIRObject);
begin
  FRes.setProperty(propName, propValue);
end;


{ TFhirOperationOutcomeW }

function TFhirOperationOutcomeW.link: TFhirOperationOutcomeW;
begin
  result := TFhirOperationOutcomeW(inherited Link);
end;

function TFhirOperationOutcomeIssueW.link: TFhirOperationOutcomeIssueW;
begin
  result := TFhirOperationOutcomeIssueW(inherited link);
end;


{ TFHIRCapabilitiesStatementW }

function TFHIRCapabilityStatementW.link: TFHIRCapabilityStatementW;
begin
  result := TFHIRCapabilityStatementW(inherited Link);
end;

{ TFHIRBundleW }

function TFHIRBundleW.next: String;
begin
  result := next(resource);
end;

function TFHIRBundleW.link: TFHIRBundleW;
begin
  result := TFHIRBundleW(inherited link);
end;

{ TFhirParametersParameterW }

destructor TFhirParametersParameterW.Destroy;
begin
  FList.Free;
  inherited;
end;



function TFhirParametersParameterW.link: TFhirParametersParameterW;
begin
  result := TFhirParametersParameterW(inherited link);
end;

function TFhirParametersParameterW.partList: TFslList<TFhirParametersParameterW>;
begin
  populateList;
  result := FList;
end;

procedure TFhirParametersParameterW.populateList;
begin
  if FList = nil then
    FList := TFslList<TFhirParametersParameterW>.create;
  FList.Clear;
end;

{ TFHIRXVersionElementWrapper }

procedure TFHIRXVersionElementWrapper.addExtension(url: String; value: TFHIRObject);
begin
  FElement.addExtension(url, value);
end;

constructor TFHIRXVersionElementWrapper.Create(elem : TFHIRObject);
begin
  inherited create;
  if (elem = nil) then
    raise Exception.Create('A value must be provided');
  FElement := elem;
end;

destructor TFHIRXVersionElementWrapper.Destroy;
begin
  FElement.Free;
  inherited;
end;

function TFHIRXVersionElementWrapper.extensionCount(url: String): integer;
begin
  result := FElement.extensionCount(url);
end;

function TFHIRXVersionElementWrapper.extensions(url: String): TFslList<TFHIRObject>;
begin
  result := FElement.extensions(url);
end;

function TFHIRXVersionElementWrapper.createPropertyValue(propName: string): TFHIRObject;
begin
  result := FElement.createPropertyValue(propName);
end;

function TFHIRXVersionElementWrapper.fhirType: String;
begin
  result := FElement.fhirType;
end;

function TFHIRXVersionElementWrapper.getExtensionString(url: String): String;
begin
  result := FElement.getExtensionString(url);
end;

function TFHIRXVersionElementWrapper.getId: String;
begin
  result := FElement.getId;
end;

function TFHIRXVersionElementWrapper.GetFhirObjectVersion: TFHIRVersion;
begin
  result := FElement.FhirObjectVersion;
end;

function TFHIRXVersionElementWrapper.hasExtension(url: String): boolean;
begin
  result := FElement.hasExtension(url);
end;

function TFHIRXVersionElementWrapper.makeCodeValue(v: String): TFHIRObject;
begin
  result := FElement.makeCodeValue(v);
end;

function TFHIRXVersionElementWrapper.makeIntValue(v: String): TFHIRObject;
begin
  result := FElement.makeIntValue(v);
end;

function TFHIRXVersionElementWrapper.makeStringValue(v: String): TFHIRObject;
begin
  result := FElement.makeStringValue(v);
end;

procedure TFHIRXVersionElementWrapper.setIdValue(id: String);
begin
  FElement.SetIdValue(id);
end;

procedure TFHIRXVersionElementWrapper.setProperty(propName: string; propValue: TFHIRObject);
begin
  FElement.setProperty(propName, propValue);
end;

{ TFHIRParametersW }

destructor TFHIRParametersW.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFHIRParametersW.link: TFHIRParametersW;
begin
  result := TFHIRParametersW(inherited Link);

end;

function TFHIRParametersW.parameterList: TFslList<TFhirParametersParameterW>;
begin
  populateList;
  result := FList;
end;

procedure TFHIRParametersW.populateList;
begin
  if FList = nil then
    FList := TFslList<TFhirParametersParameterW>.create;
  FList.Clear;
end;

function TFHIRSearchParamDefinitionW.link: TFHIRSearchParamDefinitionW;
begin
  result := TFHIRSearchParamDefinitionW(inherited link);
end;

{ TFHIRElementDefinitionW }

{ TFhirBundleEntryW }

{ TFHIRElementDefinitionW }

function TFHIRElementDefinitionW.link: TFHIRElementDefinitionW;
begin
  result := TFHIRElementDefinitionW(inherited link);
end;

{ TFhirCodingW }

function TFhirCodingW.link: TFhirCodingW;
begin
  result := TFhirCodingW(inherited Link);
end;

{ TFhirCodeableConceptW }

function TFhirCodeableConceptW.link: TFhirCodeableConceptW;
begin
  result := TFhirCodeableConceptW(inherited Link);
end;

{ TFhirValueSetComposeIncludeW }

function TFhirValueSetComposeIncludeW.link: TFhirValueSetComposeIncludeW;
begin
  result := TFhirValueSetComposeIncludeW(inherited link);
end;

{ TFhirValueSetW }

function TFhirValueSetW.link: TFhirValueSetW;
begin
  result := TFhirValueSetW(inherited link);
end;

{ TFhirValueSetComposeIncludeFilterW }

function TFhirValueSetComposeIncludeFilterW.link: TFhirValueSetComposeIncludeFilterW;
begin
  result := TFhirValueSetComposeIncludeFilterW(inherited link);
end;

{ TFhirCodeSystemW }

destructor TFhirCodeSystemW.Destroy;
begin
  FConceptList.Free;
  inherited;
end;

function TFhirCodeSystemW.link: TFhirCodeSystemW;
begin
  result := TFhirCodeSystemW(inherited link);
end;

{ TFhirCodeSystemConceptW }

destructor TFhirCodeSystemConceptW.Destroy;
begin
  FConceptList.Free;
  inherited;
end;

function TFhirCodeSystemConceptW.link: TFhirCodeSystemConceptW;
begin
  result := TFhirCodeSystemConceptW(inherited link);
end;

{ TFHIRValueSetCodeSystemW }

function TFHIRValueSetCodeSystemW.link: TFHIRValueSetCodeSystemW;
begin
  result := TFHIRValueSetCodeSystemW(inherited Link);

end;

{ TFhirValueSetComposeIncludeConceptW }

function TFhirValueSetComposeIncludeConceptW.link: TFhirValueSetComposeIncludeConceptW;
begin
  result := TFhirValueSetComposeIncludeConceptW(inherited link);
end;

{ TFHIRXVersionOperationWrapper }

constructor TFHIRXVersionOperationWrapper.Create(res: TFslObject);
begin
  inherited create;
  if (res = nil) then
    raise Exception.Create('A value must be provided');
  FOp := res;
end;

destructor TFHIRXVersionOperationWrapper.Destroy;
begin
  FOp.Free;
  inherited;
end;

{ TFHIRLookupOpResponseW }

function TFHIRLookupOpResponseW.link: TFHIRLookupOpResponseW;
begin
  result := TFHIRLookupOpResponseW(inherited link);
end;

{ TFHIRLookupOpRespPropertyW }

function TFHIRLookupOpRespPropertyW.link: TFHIRLookupOpRespPropertyW;
begin
  result := TFHIRLookupOpRespPropertyW(inherited link);
end;

{ TFHIRLookupOpRespDesignationW }

function TFHIRLookupOpRespDesignationW.link: TFHIRLookupOpRespDesignationW;
begin
  result := TFHIRLookupOpRespDesignationW(inherited link);
end;

{ TFhirCodeSystemPropertyW }

function TFhirCodeSystemPropertyW.link: TFhirCodeSystemPropertyW;
begin
  result := TFhirCodeSystemPropertyW(inherited link);
end;

{ TFhirCodeSystemConceptDesignationW }

function TFhirCodeSystemConceptDesignationW.link: TFhirCodeSystemConceptDesignationW;
begin
  result := TFhirCodeSystemConceptDesignationW(inherited link);
end;

{ TFhirCodeSystemConceptPropertyW }

function TFhirCodeSystemConceptPropertyW.link: TFhirCodeSystemConceptPropertyW;
begin
  result := TFhirCodeSystemConceptPropertyW(inherited link);
end;

{ TFhirExtensionW }

function TFhirExtensionW.link: TFhirExtensionW;
begin
  result := TFhirExtensionW(inherited link);
end;


{ TFhirValueSetExpansionContainsW }

function TFhirValueSetExpansionContainsW.link: TFhirValueSetExpansionContainsW;
begin
  result := TFhirValueSetExpansionContainsW(inherited link);
end;

{ TFhirValueSetExpansionW }

function TFhirValueSetExpansionW.link: TFhirValueSetExpansionW;
begin
  result := TFhirValueSetExpansionW(inherited Link);
end;

{ TFhirValueSetComposeIncludeConceptDesignationW }

function TFhirValueSetComposeIncludeConceptDesignationW.link: TFhirValueSetComposeIncludeConceptDesignationW;
begin
  result := TFhirValueSetComposeIncludeConceptDesignationW(inherited Link);
end;

{ TFhirPatientW }

function TFhirPatientW.Link: TFhirPatientW;
begin
  result := TFhirPatientW(inherited Link);
end;

{ TFhirBundleEntryW }

function TFhirBundleEntryW.Link: TFhirBundleEntryW;
begin
  result := TFhirBundleEntryW(inherited link);
end;

{ TFhirSearchParameterW }

function TFhirSearchParameterW.link: TFhirSearchParameterW;
begin
  result := TFhirSearchParameterW(inherited link);
end;

end.



