unit ftx_service;

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

{$I fhir.inc}

interface

uses
  SysUtils, Classes, Generics.Collections,
  fsl_utilities, fsl_base, fsl_collections, fsl_fpc, fsl_lang, fsl_logging, fsl_i18n,
  fsl_http,
  fhir_common, fhir_factory, fhir_features, fhir_objects, fhir_utilities,
  fhir_cdshooks;

type
  ETerminologySetup = class (EFslException); // problem in the terminology configuration or loaded terminologies

  { ETerminologyError }

  ETerminologyError = class (EFslException) // problem in terminology operation
  private
    FIssueType : TFhirIssueType;
    FopIssue : TOpIssueCode;
  public
    constructor Create(message : String; issueType : TFhirIssueType);
    constructor Create(message : String; issueType : TFhirIssueType; opIssue : TOpIssueCode);
    property IssueType : TFhirIssueType read FIssueType;
    property OpIssue : TOpIssueCode read FopIssue;
  end;

  ETerminologyTodo = Class(ETerminologyError)
  public
    Constructor Create(place : String);
  End;

const
  ANY_CODE_VS = 'http://hl7.org/fhir/ValueSet/@all';

Type

  TFhirFilterOperator = fhir_common.TFilterOperator;

  { TTxOperationContext }

  TTxOperationContext = class abstract (TFslObject)
  private
    FVersion : TFHIRVersion;
  public
    constructor create(version : TFhirVersion);
    procedure log(note : String); virtual; abstract;
    function deadCheck(var time : integer) : boolean; virtual; abstract;
    property version : TFHIRVersion read FVersion;
  end;

  TCodeSystemProviderContext = class (TFslObject)
  public
    function Link : TCodeSystemProviderContext; overload;
  end;

  { TCodeSystemIteratorContext }

  TCodeSystemIteratorContext = class (TFslObject)
  protected
    FContext: TCodeSystemProviderContext;
    FCount: integer;
    FCurrent: integer;
  public
    constructor Create(context : TCodeSystemProviderContext; count : integer);
    destructor Destroy; override;

    function Link : TCodeSystemIteratorContext; overload;

    property context : TCodeSystemProviderContext read FContext;
    property count : integer read FCount;
    property current : integer read FCurrent;

    function more : boolean; virtual;
    procedure next; // only call from within a provider
    procedure moveCursor(current : integer); // a LOINC hack
  end;

  { TCodeSystemProviderFilterContext }

  TCodeSystemProviderFilterContext = class (TFslObject)
  private
    FSummary: String;
  public
    function Link : TCodeSystemProviderFilterContext; overload;
    property summary : String read FSummary write FSummary;
  end;

  TCodeSystemProviderFilterPreparationContext = class (TFslObject)
  public
    function Link : TCodeSystemProviderFilterPreparationContext; overload;
  end;

  { TConceptDesignation }

  TConceptDesignation = class (TFslObject)
  private
    FBase : boolean;
    FLanguage: TIETFLang;
    FUse: TFHIRCodingW;
    FValue : TFHIRPrimitiveW;
    FExtensions : TFslList<TFHIRExtensionW>;
    function GetExtensions: TFslList<TFHIRExtensionW>;
    procedure SetLanguage(const Value: TIETFLang);
    procedure SetUse(const Value: TFHIRCodingW);
    procedure SetValue(const Value: TFHIRPrimitiveW);
  public
    constructor Create; overload;
    destructor Destroy; override;
    function link : TConceptDesignation; overload;

    property language : TIETFLang read FLanguage write SetLanguage;
    property use : TFHIRCodingW read FUse write SetUse;
    property value : TFHIRPrimitiveW read FValue write SetValue;
    property extensions : TFslList<TFHIRExtensionW> read GetExtensions;
    property base : boolean read FBase write FBase;

    function display : String;
    function present : String;
  end;

  TDisplayCheckingStyle = (dcsExact, dcsCaseInsensitive, dcsNormalised);
  TDisplayDifference = (ddDifferent, ddCase, ddNormalised);
  TLangMatchType = (lmtLiteral, lmtFull, lmtLangRegion, lmtLang);

  TCodeSystemProvider = class;

  { TConceptDesignations }

  TConceptDesignations = class (TFslObject)
  private
    FFactory : TFHIRFactory;
    FBaseLang : TIETFLang;
    FDesignations : TFslList<TConceptDesignation>;
    FLanguages : TIETFLanguageDefinitions;
    FSource : TCodeSystemProvider;
    function GetCount: Integer;
    function langMatches(lang : THTTPLanguageEntry; stated : TIETFLang; matchType: TLangMatchType) : boolean;
    function langsMatch(langList : THTTPLanguageList; stated : TIETFLang; matchType : TLangMatchType; defLang : TIETFLang) : boolean;
    procedure SetSource(AValue: TCodeSystemProvider);
    function stringMatches(source, possible : String; mode : TDisplayCheckingStyle; lang : TIETFLang) : boolean;
    procedure SetBaseLang(value : TIETFLang);
  public
    constructor Create(factory : TFHIRFactory; languages : TIETFLanguageDefinitions);
    destructor Destroy; override;

    function link : TConceptDesignations; overload;
    procedure clear;

    function  addDesignation(base, isDisplay : boolean; lang, display : String) : TConceptDesignation; overload;
    procedure addDesignation(base, isDisplay : boolean; lang : string; displays : TStringList); overload;
    function  addDesignation(base, isDisplay : boolean; lang : String; value : TFHIRPrimitiveW; extensions : TFslList<TFHIRExtensionW> = nil) : TConceptDesignation; overload;
    function  addDesignation(ccd : TFhirCodeSystemConceptDesignationW) : TConceptDesignation; overload;
    function  addDesignation(ccd : TFhirValueSetComposeIncludeConceptDesignationW) : TConceptDesignation; overload;

    function hasDisplay(langList : THTTPLanguageList; defLang : TIETFLang; value : String; mode : TDisplayCheckingStyle; out diff : TDisplayDifference) : boolean;
    function displayCount(langList : THTTPLanguageList; defLang : TIETFLang; displayOnly : boolean) : integer;
    function present(langList : THTTPLanguageList; defLang : TIETFLang; displayOnly : boolean) : String;
    function include(cd : TConceptDesignation; langList : THTTPLanguageList; defLang : TIETFLang) : boolean;
    function preferredDesignation(langList : THTTPLanguageList = nil; defLang : TIETFLang = nil) : TConceptDesignation;
    function preferredDisplay(langList : THTTPLanguageList = nil; defLang : TIETFLang = nil) : String;
    function summary : String;
    function present : String;
    function langCode : String;

    property factory : TFHIRFactory read FFactory;
    property baseLang : TIETFLang read FBaseLang write SetBaseLang;
    property designations : TFslList<TConceptDesignation> read FDesignations;
    property count : Integer read GetCount;
    property source : TCodeSystemProvider read FSource write SetSource;
  end;

  TSearchFilterText = class (TFslObject)
  private
    FFilter: string;
    FStems : TStringList;
    FStemmer : TFslWordStemmer;

    function find(s : String) : boolean;

    procedure process;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(filter : String);  overload;
    destructor Destroy; override;

    function Link : TSearchFilterText; overload;

    function null : Boolean;
    function passes(value : String) : boolean; overload;
    function passes(cds : TConceptDesignations) : boolean; overload;
    function passes(value : String; var rating : double) : boolean; overload;
    function matches(stems : TFslStringList) : double; overload;
    property filter : string read FFilter;
    property stems : TStringList read FStems;
  end;

  { TAlternateCodeOptions }

  TAlternateCodeOptions = class (TFslObject)
  private
    FAll: boolean;
    FUses: TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;
    property all : boolean read FAll write FAll;
    property useTypes : TStringList read FUses;

    procedure seeParam(value : String);
    function passes(prop : TFhirCodeSystemConceptPropertyW) : boolean;
  end;

  { TCodeTranslation }

  TCodeTranslation = class (TFslObject)
  private
    FCode : String;
    FMap: String;
    FUri : String;
    FVersion : String;
    FDisplay : String;
    FMessage : String;
    FEquivalence : TFHIRConceptEquivalence;
  public
    function link : TCodeTranslation; overload;
    property code : String read FCode write FCode;
    property uri : String read FUri write FUri;
    property version : String read FVersion write FVersion;
    property display : String read FDisplay write FDisplay;
    property message : String read FMessage write FMessage;
    property equivalence : TFHIRConceptEquivalence read FEquivalence write FEquivalence;
    property map : String read FMap write FMap;
  end;

  { TCodeSystemProvider }

  TCodeSystemProvider = class abstract (TFslObject)
  private
    FUseCount : cardinal;
    FDefLang : TIETFLang;
  protected
    FLanguages : TIETFLanguageDefinitions;
    FI18n : TI18nSupport;
    procedure setDefLang(value : TIETFLang);
    procedure deadCheck(opContext: TTxOperationContext; place, op: String);
  public
    constructor Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport);
    destructor Destroy; override;

    function Link : TCodeSystemProvider; overload;
    function cloneWithSupplements(supplements : TFslList<TFhirCodeSystemW>) : TCodeSystemProvider; virtual;
    function checkCodeSystem(langs : THTTPLanguageList; var msg : String) : boolean; virtual;

    function contentMode : TFhirCodeSystemContentMode; virtual;
    function expandLimitation : Integer; virtual;
    function description : String; virtual; abstract;
    function sourcePackage : String; virtual;
    function TotalCount : integer; virtual; abstract;
    function getPropertyDefinitions : TFslList<TFhirCodeSystemPropertyW>; virtual;
    function getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; virtual; abstract;
    function getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; virtual; abstract;
    function systemUri() : String; virtual; abstract;
    function version() : String; virtual;
    function defaultToLatest : boolean; virtual;
    function defLang() : TIETFLang; virtual;
    function hasAnyDisplays(disp : THTTPLanguageList) : boolean; virtual;
    function name(context : TCodeSystemProviderContext) : String; virtual;
    function getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String; virtual; abstract;
    function getDefinition(opContext : TTxOperationContext; code : String):String; virtual; abstract;
    function locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions= nil) : TCodeSystemProviderContext; overload; virtual;
    function locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; overload; virtual; abstract;
    function sameContext(opContext : TTxOperationContext; a, b : TCodeSystemProviderContext) : boolean; virtual;
    function locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; virtual; abstract;
    function IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; virtual; abstract;
    function IsInactive(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; overload; virtual;
    function getCodeStatus(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : String; overload; virtual;
    function deprecated(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; overload; virtual;
    function IsInactive(opContext : TTxOperationContext; code : String) : boolean; overload; virtual;
    function Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; virtual; abstract;
    function Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string;  overload; virtual; abstract;
    function Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; lang : TFslList<TIETFLang>) : string; overload;
    function Definition(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; virtual; abstract;
    function itemWeight(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; virtual;
    procedure Designations(opContext : TTxOperationContext; context : TCodeSystemProviderContext; list : TConceptDesignations); overload; virtual; abstract;  // get all displays for all languages
    function getExtensions(opContext : TTxOperationContext; context : TCodeSystemProviderContext)  : TFslList<TFHIRExtensionW>; virtual;
    function getProperties(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TFslList<TFhirCodeSystemConceptPropertyW>; virtual;
    function listCodes(opContext : TTxOperationContext; ctxt : TCodeSystemProviderContext; altOpt : TAlternateCodeOptions) : TStringArray; virtual;
    function parent(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : String; virtual; // return if there is one and only one
    function canParent : boolean; virtual;
    function doesFilter(opContext : TTxOperationContext; prop : String; op : TFhirFilterOperator; value : String) : boolean; virtual;
    function incompleteValidationMessage(context : TCodeSystemProviderContext; langs : THTTPLanguageList) : String; virtual;

    function hasSupplement(opContext : TTxOperationContext; url : String) : boolean; virtual;
    procedure listSupplements(opContext : TTxOperationContext; ts : TStringList); virtual;
    function getPrepContext(opContext : TTxOperationContext) : TCodeSystemProviderFilterPreparationContext; virtual;
    function searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; virtual; abstract;
    function specialFilter(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; virtual;
    function filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; virtual; abstract;
    function prepare(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext) : boolean; virtual; // true if the underlying provider collapsed multiple filters
    function filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; overload; virtual; abstract;
    function filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; overload; virtual;
    function FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean; virtual; abstract;
    function filterSize(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : integer; overload; virtual; abstract;
    function FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; virtual; abstract;
    function InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; virtual; abstract;
    function isNotClosed(opContext : TTxOperationContext; textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; virtual; abstract;
    procedure extendLookup(opContext : TTxOperationContext; factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; langList : THTTPLanguageList; props : TArray<String>; resp : TFHIRLookupOpResponseW); virtual;
    function subsumesTest(opContext : TTxOperationContext; codeA, codeB : String) : String; virtual;

    function SpecialEnumeration : String; virtual;
    procedure defineFeatures(opContext : TTxOperationContext; features : TFslList<TFHIRFeature>); virtual; abstract;
    function versionIsMoreDetailed(v1, v2 : String): boolean; virtual;
    procedure getStatus(out status: TPublicationStatus; out standardsStatus: String; out experimental : boolean); virtual;
    procedure getCDSInfo(opContext : TTxOperationContext; card : TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display : String); virtual;

    procedure registerConceptMaps(list : TFslList<TFHIRConceptMapW>; factory : TFHIRFactory); virtual;
    procedure getTranslations(coding: TFHIRCodingW; target : String; codes : TFslList<TCodeTranslation>); virtual;
    procedure RecordUse(count : integer = 1);
    procedure checkReady; virtual;
    function defToThisVersion(specifiedVersion : String) : boolean; virtual;
    property UseCount : cardinal read FUseCount;
  end;

  { TCodeSystemProviderFactory }

  TCodeSystemProviderFactory = class (TFslObject)
  public
    function link : TCodeSystemProviderFactory; overload;
    function getProvider : TCodeSystemProvider; virtual; abstract;
    function systemUri : String; virtual; abstract;
    function version : String; virtual; abstract;
    function name : String; virtual; abstract;
    function TotalCount : integer; virtual; abstract;
    function versionDesc : String; virtual; abstract;
    function description : String; virtual; abstract;
  end;

const
  CODES_TDisplayCheckingStyle : Array [TDisplayCheckingStyle] of String = ('Exact', 'CaseInsensitive', 'Normalised');

implementation

{ TAlternateCodeOptions }

constructor TAlternateCodeOptions.Create;
begin
  inherited Create;
  FAll := false;
  FUses := TStringList.Create;
end;

destructor TAlternateCodeOptions.Destroy;
begin
  FUses.free;
  inherited Destroy;
end;

procedure TAlternateCodeOptions.seeParam(value: String);
begin
  if (value = 'true') then
    FAll := true
  else if (value = 'false') then
  begin
    FAll := false;
    FUses.clear;
  end
  else
    FUses.add(value);
end;

function TAlternateCodeOptions.passes(prop: TFhirCodeSystemConceptPropertyW): boolean;
var
  ext : TFHIRExtensionW;
  c : TFHIRCodingW;
begin
  result := false;
  if all then
    result := true
  else
  begin
    for ext in prop.getExtensionsW('http://hl7.org/fhir/StructureDefinition/alternate-code-use').forEnum do
    begin
      c := ext.valueAsCoding;
      try
        if FUses.indexOf(c.code) > -1 then
          exit(true);
      finally
        c.free;
      end;
    end;
  end;
end;

{ TCodeTranslation }

function TCodeTranslation.link: TCodeTranslation;
begin
  result := TCodeTranslation(inherited link);
end;


{ TConceptDesignations }

destructor TConceptDesignations.Destroy;
begin
  FLanguages.free;
  FFactory.free;
  FBaselang.free;
  FDesignations.free;
  FSource.free;
  inherited Destroy;
end;

function TConceptDesignations.link: TConceptDesignations;
begin
  result := TConceptDesignations(inherited Link);
end;

procedure TConceptDesignations.clear;
begin
  BaseLang := nil;
  FDesignations.clear;
end;

function TConceptDesignations.addDesignation(base, isDisplay : boolean; lang, display: String): TConceptDesignation;
begin
  //Logging.log('Add designation '+display+' for lang '+lang+' (base = '+booleanToString(base)+', display = '+booleanToString(isDisplay)+')');
  result := TConceptDesignation.Create;
  try
    result.language := FLanguages.parse(lang);
    result.value := FFactory.wrapPrimitive(FFactory.makeString(display));
    result.base := base;
    if isDisplay then
      result.use := factory.wrapCoding(factory.makeCoding('http://terminology.hl7.org/CodeSystem/designation-usage', 'display'));
    FDesignations.add(result.link);
  finally
    result.free;
  end;
end;

procedure TConceptDesignations.addDesignation(base, isDisplay : boolean; lang: string; displays: TStringList);
var
  s : String;
begin
  if (displays <> nil) then
    for s in displays do
      addDesignation(base, isDisplay, lang, s);
end;

function TConceptDesignations.addDesignation(base, isDisplay : boolean; lang: String; value: TFHIRPrimitiveW; extensions : TFslList<TFHIRExtensionW>): TConceptDesignation;
var
  ext : TFHIRExtensionW;
begin
  //Logging.log('Add designation '+value.AsString+' for lang '+lang+' (base = '+booleanToString(base)+', display = '+booleanToString(isDisplay)+')');
  result := TConceptDesignation.Create;
  try
    result.language := FLanguages.parse(lang);
    result.value := value; {no .link}
    if (extensions <> nil) then
      for ext in extensions do
        result.extensions.add(ext.link);  
    result.base := base;
    if isDisplay then
      result.use := factory.wrapCoding(factory.makeCoding('http://terminology.hl7.org/CodeSystem/designation-usage', 'display'));
    FDesignations.add(result.link);
  finally
    result.free;
  end;
end;

function codingToString(code : TFHIRCodingW) : String;
begin
  if (code = nil) then
    result := '--'
  else
    result := code.renderText;
end;

function TConceptDesignations.addDesignation(ccd : TFhirCodeSystemConceptDesignationW) : TConceptDesignation;
var
  list : TFslList<TFHIRExtensionW>;
begin
  //Logging.log('Add designation '+ccd.value+' for lang '+ccd.language+' (use = '+codingToString(ccd.use)+')');
  result := TConceptDesignation.Create;
  try
    result.Language := FLanguages.parse(ccd.language);
    result.Use := ccd.use;
    result.Value := ccd.valueElement; {no .link}   
    if (ccd.hasExtensions) then
    begin
      list := ccd.getExtensionsW('http://hl7.org/fhir/StructureDefinition/coding-sctdescid');
      try
        if (list.count > 0) then
          result.extensions.addAll(list);
      finally
        list.free;
      end;
    end;
    designations.add(result.link);
  finally
    result.free;
  end;
end;

function TConceptDesignations.addDesignation(ccd : TFhirValueSetComposeIncludeConceptDesignationW) : TConceptDesignation;
var
  list : TFslList<TFHIRExtensionW>;
begin
  //Logging.log('Add designation '+ccd.value+' for lang '+ccd.language+' (use = '+codingToString(ccd.use)+')');
  result := TConceptDesignation.Create;
  try
    result.Language := FLanguages.parse(ccd.language);
    result.Use := ccd.use;
    result.Value := ccd.valueElement; {no .link}
    if (ccd.hasExtensions) then
    begin
      list := ccd.getExtensionsW('http://hl7.org/fhir/StructureDefinition/coding-sctdescid');
      try
        if (list.count > 0) then
          result.extensions.addAll(list);
      finally
        list.free;
      end;
    end;
    designations.add(result.link);
  finally
    result.free;
  end;
end;


function isDisplay(cd : TConceptDesignation) : Boolean;
begin
  result := (cd.use = nil) or ((cd.use <> nil) and (cd.use.systemUri = 'http://terminology.hl7.org/CodeSystem/designation-usage') and (cd.use.code = 'display'));
end;

function TConceptDesignations.preferredDesignation(langList : THTTPLanguageList = nil; defLang : TIETFLang = nil) : TConceptDesignation;
var
  cd : TConceptDesignation;
  lang : THTTPLanguageEntry;
begin
  if FDesignations.count = 0 then
    exit(nil);
  if langList = nil then
  begin
    for cd in FDesignations do
      if (cd.base) then
        exit(cd);
    for cd in FDesignations do
      if isDisplay(cd) then
        exit(cd);
    exit(FDesignations[0]);
  end;
  // we have a list of languages in priority order
  // we have a list of designations in no order
  // language exact match is preferred
  // display is always preferred
  result := nil;
  for lang in langList.langs do
  begin
    if (lang.value > 0) then
    begin
      for cd in FDesignations do
        if (cd.base) and langMatches(lang, cd.language, lmtFull) then
        begin
          exit(cd);
        end;
      for cd in FDesignations do
        if isDisplay(cd) and langMatches(lang, cd.language, lmtFull) then
        begin
          exit(cd);
        end;

      for cd in FDesignations do
        if (cd.base) and langMatches(lang, cd.language, lmtLangRegion) then
        begin
          exit(cd);
        end;
      for cd in FDesignations do
        if isDisplay(cd) and langMatches(lang, cd.language, lmtLangRegion) then
        begin
          exit(cd);
        end;


      for cd in FDesignations do
        if (cd.base) and langMatches(lang, cd.language, lmtLang) then
        begin
          exit(cd);
        end;
      for cd in FDesignations do
        if isDisplay(cd) and langMatches(lang, cd.language, lmtLang) then
        begin
          exit(cd);
        end;

      for cd in FDesignations do
        if langMatches(lang, cd.language, lmtFull) then
        begin
          exit(cd);
        end;             
      for cd in FDesignations do
        if langMatches(lang, cd.language, lmtLangRegion) then
        begin
          exit(cd);
        end;
      for cd in FDesignations do
        if langMatches(lang, cd.language, lmtLang) then
        begin
          exit(cd);
        end;
    end;
  end;
end;
           
function TConceptDesignations.preferredDisplay(langList : THTTPLanguageList; defLang : TIETFLang): String;
var
  cd : TConceptDesignation;
begin
  cd := preferredDesignation(langList);
  if (cd = nil) then
    result := ''
  else
    result := cd.display;
end;

function TConceptDesignations.summary: String;
var
  cd : TConceptDesignation;
begin
  result := '';
  for cd in FDesignations do
    CommaAdd(result, cd.present);
end;

function TConceptDesignations.present: String;
begin
  if (FBaseLang = nil) then
    result := 'Lang: ??'
  else
    result := 'Lang: '+FBaseLang.Code;
  if FSource <> nil then
    result := result + '; source: '+FSource.ClassName;
end;

function TConceptDesignations.langCode: String;
begin
  if FBaseLang = nil then
    result := 'en'
  else
    result := FBaseLang.code;
end;


function TConceptDesignations.displayCount(langList : THTTPLanguageList; defLang : TIETFLang; displayOnly : boolean): integer;
var
  cd : TConceptDesignation;
begin
  result := 0;
  for cd in FDesignations do
    if (not displayOnly or cd.base or isDisplay(cd)) and langsMatch(langList, cd.language, lmtFull, defLang) and (cd.value <> nil) then
    begin
      inc(result);
    end;
  if result = 0 then
    for cd in FDesignations do
      if (not displayOnly or cd.base or isDisplay(cd)) and langsMatch(langList, cd.language, lmtLangRegion, defLang) and (cd.value <> nil) then
      begin
        inc(result);
      end;        
  if result = 0 then
    for cd in FDesignations do
      if (not displayOnly or cd.base or isDisplay(cd)) and langsMatch(langList, cd.language, lmtLang, defLang) and (cd.value <> nil) then
      begin
        inc(result);
      end;
end;


function TConceptDesignations.present(langList : THTTPLanguageList; defLang : TIETFLang; displayOnly : boolean): String;
var
  cd : TConceptDesignation;
  b : TCommaSeparatedStringBuilder;
  c : integer;
begin
  b := TCommaSeparatedStringBuilder.create(', ', ' or ');
  try
    c := 0;
    for cd in designations do
    begin
      if (not displayOnly or cd.base or isDisplay(cd)) and (langsMatch(langList, cd.language, lmtLang, nil) and (cd.value <> nil)) then
      begin
        inc(c);
        if (cd.language <> nil) then
          b.append(''''+cd.display+''' ('+cd.language.code+')')
        else
          b.append(''''+cd.display+'''');
      end;
    end;
    if (c = 0) then
    begin
      for cd in designations do
      begin
        if (not displayOnly or cd.base or isDisplay(cd)) and (cd.value <> nil) then
        begin
          inc(c);
          if (cd.language <> nil) then
            b.append(''''+cd.display+''' ('+cd.language.code+')')
          else
            b.append(''''+cd.display+'''');
        end;
      end;
    end;

    result := b.makeString;
  finally
    b.free;
  end;
end;

function TConceptDesignations.include(cd : TConceptDesignation; langList : THTTPLanguageList; defLang : TIETFLang) : boolean;
begin
  result := langsMatch(langList, cd.language, lmtLang, defLang);
end;

function TConceptDesignations.hasDisplay(langList : THTTPLanguageList; defLang : TIETFLang; value: String; mode : TDisplayCheckingStyle; out diff : TDisplayDifference): boolean;
var
  cd : TConceptDesignation;
begin
  //Logging.log('Check for display "'+value+'". Langs = '+langList.source+', def = '+defLang.code+', mode = '+CODES_TDisplayCheckingStyle[mode]);
  //for cd in designations do
  //  Logging.log(' *'+cd.present);

  result := false;
  diff := ddDifferent;

  for cd in designations do
    if (langsMatch(langList, cd.language, lmtLang, defLang) and (cd.value <> nil) and stringMatches(value, cd.value.asString, mode, cd.language)) then
    begin
      //Logging.log('  true');
      exit(true);
    end;
  if mode = dcsExact then
  begin
    for cd in designations do
      if (langsMatch(langList, cd.language, lmtLang, defLang) and (cd.value <> nil) and stringMatches(value, cd.value.asString, dcsCaseInsensitive, cd.language)) then
      begin
        diff := ddCase;
        //Logging.log('  false (case)');
        exit(false);
      end;
  end;
  if mode <> dcsNormalised then
  begin
    for cd in designations do
      if (langsMatch(langList, cd.language, lmtLang, defLang) and (cd.value <> nil) and stringMatches(value, cd.value.asString, dcsNormalised, cd.language)) then
      begin
        diff := ddNormalised;
        //Logging.log('  false (normalised)');
        exit(false);
      end;
  end;
  //Logging.log('  false');
end;

function depthForMatchType(matchType: TLangMatchType) : TIETFLangPartType;
begin
  case matchType of
    lmtLiteral : result := lptExtension; // not that it matters
    lmtFull : result := lptExtension;
    lmtLangRegion : result := lptRegion;
    lmtLang : result := lptLanguage;
  else
    result := lptExtension; // not that it matters
  end;
end;

function TConceptDesignations.langMatches(lang : THTTPLanguageEntry; stated: TIETFLang; matchType: TLangMatchType): boolean;
var
  actual : TIETFLang;
begin
  if (stated = nil) then
    actual := FBaseLang
  else
    actual := stated;
  result := false;
  if (lang.value > 0) then
  begin
    if (lang.lang = '*') and (matchType <> lmtLiteral) then
      exit(true);

    if (actual <> nil) then
    begin
      if (matchType = lmtLiteral) then
        exit(lang.lang = actual.code);

      if (lang.ietf = nil) then
        lang.ietf := FLanguages.parse(lang.lang);
      if (lang.ietf <> nil) and lang.ietf.matches(actual, depthForMatchType(matchType)) then
        exit(true);
    end;
  end;
end;

function TConceptDesignations.GetCount: Integer;
begin
  result := FDesignations.count;
end;

function TConceptDesignations.langsMatch(langList : THTTPLanguageList; stated: TIETFLang; matchType: TLangMatchType; defLang : TIETFLang): boolean;
var
  e : THTTPLanguageEntry;
begin
  if (defLang <> nil) and (stated <> nil) then
  begin
    if stated.matches(defLang) then
      exit(true);
  end;

  if (stated = nil) or (langList = nil) then
    result := true
  else
  begin
    result := false;
    for e in langList.langs do
      if langMatches(e, stated, matchType) then
        exit(true);
  end;
end;

procedure TConceptDesignations.SetSource(AValue: TCodeSystemProvider);
begin
  FSource.free;
  FSource := AValue;
end;

function TConceptDesignations.stringMatches(source, possible: String; mode : TDisplayCheckingStyle; lang : TIETFLang): boolean;
begin
  // we ignore lang at this time
  case mode of
    dcsExact: result := source = possible;
    dcsCaseInsensitive: result := SameText(source, possible);
    dcsNormalised: result := SameText(StringNormalizeWhitespace(source), StringNormalizeWhitespace(possible));
  else
    result := false;
  end;
end;

procedure TConceptDesignations.SetBaseLang(value: TIETFLang);
begin
  FBaselang.free;
  FBaseLang := value;
end;

constructor TConceptDesignations.Create(factory : TFHIRFactory; languages : TIETFLanguageDefinitions);
begin
  inherited Create;
  FFactory := factory;
  FLanguages := languages;
  FDesignations := TFslList<TConceptDesignation>.Create;
end;

{ TConceptDesignation }

destructor TConceptDesignation.Destroy;
begin
  FLanguage.free;
  FUse.free;
  FValue.free;
  FExtensions.free;
  inherited Destroy;
end;

procedure TConceptDesignation.SetLanguage(const Value: TIETFLang);
begin
  FLanguage.free;
  FLanguage := Value;
end;

function TConceptDesignation.GetExtensions: TFslList<TFHIRExtensionW>;
begin
  if FExtensions = nil then
    FExtensions := TFslList<TFHIRExtensionW>.Create;
  result := FExtensions;
end;

procedure TConceptDesignation.SetUse(const Value: TFHIRCodingW);
begin
  FUse.free;
  FUse := value;
end;

procedure TConceptDesignation.SetValue(const Value: TFHIRPrimitiveW);
begin
  FValue.free;
  FValue := Value;
end;

constructor TConceptDesignation.Create;
begin
  inherited Create;
end;

function TConceptDesignation.display: String;
begin
  if (value = nil) then
    result := ''
  else
    result := value.AsString;
end;

function TConceptDesignation.link: TConceptDesignation;
begin
  result := TConceptDesignation(inherited Link);
end;

function TConceptDesignation.present: String;
begin
  if FValue <> nil then
    result := '"'+FValue.asString+'"'
  else
    result := '""';
  if (FLanguage <> nil) or (FUse <> nil) then
  begin
    result := result +' (';
    if (FLanguage <> nil) then
      result := result + FLanguage.code;
    if FUse <> nil then
      result := result + '/'+FUse.renderText;
    result := result +')';
  end;
end;

{ TCodeSystemProvider }

procedure TCodeSystemProvider.setDefLang(value: TIETFLang);
begin
  FDefLang.free;
  FDefLang := value;
end;

procedure TCodeSystemProvider.deadCheck(opContext: TTxOperationContext; place, op: String);
var
  time : integer;
begin
  if opContext.deadCheck(time) then
    raise ETooCostly.create(FI18n.translate('CODESYSTEM_TOO_COSTLY_TIME', nil, [inttostr(time), op, place]));
end;

constructor TCodeSystemProvider.Create(languages: TIETFLanguageDefinitions; i18n : TI18nSupport);
begin
  inherited Create;
  FLanguages := languages;
  FI18n := i18n;
  FDefLang := FLanguages.parse('en');
end;

function TCodeSystemProvider.defToThisVersion(specifiedVersion : String): boolean;
begin
  result := true;
end;

destructor TCodeSystemProvider.Destroy;
begin
  FLanguages.free;
  FI18n.free;
  FDefLang.free;
  inherited;
end;

function TCodeSystemProvider.doesFilter(opContext : TTxOperationContext; prop: String; op: TFhirFilterOperator; value: String): boolean;
var
  ctxt : TCodeSystemProviderFilterContext;
begin
  result := false;
  ctxt := filter(opContext, false, true, prop, op, value, nil);
  try
    result := ctxt <> nil;
  finally
    ctxt.free;
  end;
end;

function TCodeSystemProvider.incompleteValidationMessage(context: TCodeSystemProviderContext; langs: THTTPLanguageList): String;
begin
  result := '';
end;

procedure TCodeSystemProvider.extendLookup(opContext : TTxOperationContext; factory : TFHIRFactory; ctxt: TCodeSystemProviderContext; langList : THTTPLanguageList; props : TArray<String>; resp : TFHIRLookupOpResponseW);
begin
  // nothing here
end;

function TCodeSystemProvider.filterLocate(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext; code: String): TCodeSystemProviderContext;
var
  msg : String;
begin
  result := filterLocate(opContext, ctxt, code, msg);
end;

procedure TCodeSystemProvider.getCDSInfo(opContext : TTxOperationContext; card: TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display: String);
begin
  card.summary := 'No CDSHook Implementation for code system '+systemUri+' for code '+code+' ('+display+')';
end;

procedure TCodeSystemProvider.registerConceptMaps(list: TFslList<TFHIRConceptMapW>; factory : TFHIRFactory);
begin
  // nothing
end;

procedure TCodeSystemProvider.getTranslations(coding: TFHIRCodingW; target: String; codes: TFslList<TCodeTranslation>);
begin
  // no translations
end;

function TCodeSystemProvider.getPrepContext(opContext : TTxOperationContext): TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

function TCodeSystemProvider.hasSupplement(opContext : TTxOperationContext; url: String): boolean;
begin
  result := false;
end;

procedure TCodeSystemProvider.listSupplements(opContext : TTxOperationContext; ts: TStringList);
begin
  // nothing
end;

function TCodeSystemProvider.IsInactive(opContext : TTxOperationContext; code: String): boolean;
var
  ctxt : TCodeSystemProviderContext;
begin
  ctxt := locate(opContext, code);
  try
    result := IsInactive(opContext, ctxt);
  finally
    ctxt.free;
  end;
end;

function TCodeSystemProvider.Display(opContext : TTxOperationContext; context: TCodeSystemProviderContext; lang : TFslList<TIETFLang>): string;
var
  hl : THTTPLanguageList;
  l : TIETFLang;
begin
  hl := THTTPLanguageList.Create(FLanguages.link, '', false);
  try
    for l in lang do
      hl.addCode(l.code);
    result := display(opContext, context, hl);
  finally
    hl.free;
  end;
end;

function TCodeSystemProvider.itemWeight(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TCodeSystemProvider.contentMode: TFhirCodeSystemContentMode;
begin
  result := cscmComplete; // unless specified otherwise
end;

function TCodeSystemProvider.expandLimitation: Integer;
begin
  result := 0; // no limit
end;

function TCodeSystemProvider.sourcePackage: String;
begin
  result := '';
end;

function TCodeSystemProvider.getPropertyDefinitions: TFslList<TFhirCodeSystemPropertyW>;
begin
  result := nil;
end;

function TCodeSystemProvider.IsInactive(opContext : TTxOperationContext; context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function TCodeSystemProvider.getCodeStatus(opContext : TTxOperationContext; context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

function TCodeSystemProvider.deprecated(opContext : TTxOperationContext; context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function TCodeSystemProvider.Link: TCodeSystemProvider;
begin
  result := TCodeSystemProvider(inherited link);
end;

function TCodeSystemProvider.cloneWithSupplements(supplements: TFslList<TFhirCodeSystemW>): TCodeSystemProvider;
var
  s : String;
  cs : TFhirCodeSystemW;
begin
  s := '';
  for cs in supplements do 
    CommaAdd(s, cs.vurl);
  raise ETerminologyError.create('The codesystem '+systemUri()+' does not support supplements: '+s);
end;

function TCodeSystemProvider.checkCodeSystem(langs: THTTPLanguageList; var msg : String) : boolean;
begin
  // nothing here
  result := true;
end;

function TCodeSystemProvider.locate(opContext : TTxOperationContext; code: String; altOpt : TAlternateCodeOptions= nil): TCodeSystemProviderContext;
var
  msg : String;
begin
  result := locate(opContext, code, altOpt, msg);
end;

function TCodeSystemProvider.name(context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

function TCodeSystemProvider.prepare(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  result := false;
end;

procedure TCodeSystemProvider.RecordUse(count : integer = 1);
begin
  FUseCount := FUseCount + count;
end;

procedure TCodeSystemProvider.checkReady;
begin
  // nothing
end;

function TCodeSystemProvider.sameContext(opContext : TTxOperationContext; a, b: TCodeSystemProviderContext): boolean;
begin
  result := a = b;
end;

function TCodeSystemProvider.SpecialEnumeration: String;
begin
  result := '';
end;

function TCodeSystemProvider.versionIsMoreDetailed(v1, v2: String): boolean;
begin
  result := false;
end;

procedure TCodeSystemProvider.getStatus(out status: TPublicationStatus; out standardsStatus: String; out experimental : boolean);
begin
  status := psNull;
  standardsStatus := '';
  experimental := false;
end;

function TCodeSystemProvider.specialFilter(opContext : TTxOperationContext; prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.create('Not implemented for '+ClassName, itException);
end;

function TCodeSystemProvider.subsumesTest(opContext : TTxOperationContext; codeA, codeB: String): String;
begin
  raise ETerminologyError.create('Subsumption Testing is not supported for system '+systemUri, itException);
end;

function TCodeSystemProvider.getExtensions(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TFslList<TFHIRExtensionW>;
begin
  result := nil;
end;

function TCodeSystemProvider.getProperties(opContext : TTxOperationContext; context: TCodeSystemProviderContext): TFslList<TFhirCodeSystemConceptPropertyW>;
begin
  result := nil
end;

function TCodeSystemProvider.listCodes(opContext : TTxOperationContext; ctxt: TCodeSystemProviderContext; altOpt: TAlternateCodeOptions): TStringArray;
begin
  SetLength(result, 1);
  result[0] := code(opContext, ctxt);
end;

function TCodeSystemProvider.parent(opContext : TTxOperationContext; context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

function TCodeSystemProvider.canParent: boolean;
begin
  result := false;
end;

function TCodeSystemProvider.version(): String;
begin
  result := '';
end;

function TCodeSystemProvider.defaultToLatest: boolean;
begin
  result := false;
end;

function TCodeSystemProvider.defLang(): TIETFLang;
begin
  result := FDefLang;
end;

function TCodeSystemProvider.hasAnyDisplays(disp: THTTPLanguageList): boolean;
begin
  result := disp.matches('en', false);
end;

{ TSearchFilterText }

constructor TSearchFilterText.create(filter: String);
begin
  Create;
  FStemmer := TFslWordStemmer.create('english');
  FStems := TStringList.Create;
  FFilter := filter.toLower;
  process;
end;

destructor TSearchFilterText.Destroy;
begin
  FStems.free;
  FStemmer.free;
  inherited;
end;

function TSearchFilterText.find(s: String): boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FStems.Count - 1;
  while not result and (L <= H) do
  begin
    I := (L + H) shr 1;
    C := CompareStr(FStems[I], copy(S, 1, length(FStems[i])));
    if C = 0 then
      Result := True
    else if C < 0 then
      L := I + 1
    else
      H := I - 1;
  end;
end;

function TSearchFilterText.Link: TSearchFilterText;
begin
  result := TSearchFilterText(inherited link);
end;

function TSearchFilterText.null: Boolean;
begin
  result := FStems.Count = 0;
end;

function TSearchFilterText.passes(value: String): boolean;
var
  r : double;
begin
  result := passes(value, r);
end;

function TSearchFilterText.passes(value: String; var rating : double): boolean;
var
  i, j : integer;
begin
  result := Null;
  i := 1;
  rating := 0;
  while (not result) and (i <= length(value)) Do
  begin
    if CharInSet(value[i], ['0'..'9', 'a'..'z', 'A'..'Z']) then
    begin
      j := i;
      while (i <= length(value)) and CharInSet(value[i], ['0'..'9', 'a'..'z', 'A'..'Z']) do
        inc(i);
      result := find(lowercase(FStemmer.Stem(copy(value, j, i-j))));
      if result then
        rating := rating + length(value) / FStems.Count;
    end
    else
      inc(i);
  End;
end;

function TSearchFilterText.matches(stems: TFslStringList): double;
var
  i : integer;
  s : String;
  incomplete, complete : boolean;
begin
  result := 0;
  if FStems.Count = 0 then
    result := 100
  else if stems = nil then
    result := 0
  else
  begin
    for i := 0 to stems.count - 1 do
    begin
      incomplete := false;
      complete := false;
      for s in FStems do
      begin
        if s = stems[i] then
          complete := complete or true
        else
          incomplete := incomplete or stems[i].startsWith(s);
      end;
      if complete then
        result := result + length(stems[i])/Fstems.count
      else if incomplete then
        result := result + length(stems[i])/Fstems.count / 2
      else
    end;
  end;
end;

function TSearchFilterText.passes(cds: TConceptDesignations): boolean;
var
  cd : TConceptDesignation;
begin
  result := false;
  if (cds <> nil) then
  begin
    for cd in cds.designations do
      if (cd.value <> nil) and passes(cd.value.AsString) then
        exit(true);
  end;
end;

procedure TSearchFilterText.process;
var
  i, j : Integer;
begin
  i := 1;
  while i <= length(FFilter) Do
  begin
    if CharInSet(FFilter[i], ['0'..'9', 'a'..'z', 'A'..'Z']) then
    begin
      j := i;
      while (i <= length(FFilter)) and CharInSet(FFilter[i], ['0'..'9', 'a'..'z', 'A'..'Z']) do
        inc(i);
      FStems.Add(lowercase(FStemmer.Stem(copy(FFilter, j, i-j))));
    end
    else
      inc(i);
  End;
  FStems.Sort;
end;

function TSearchFilterText.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (FFilter.length * sizeof(char)) + 12);
  inc(result, FStems.sizeInBytes(magic));
  inc(result, FStemmer.sizeInBytes(magic));
end;

{ TCodeSystemProviderContext }

function TCodeSystemProviderContext.Link: TCodeSystemProviderContext;
begin
  result := TCodeSystemProviderContext(inherited link);
end;

{ TCodeSystemProviderFilterContext }

function TCodeSystemProviderFilterContext.Link: TCodeSystemProviderFilterContext;
begin
  result := TCodeSystemProviderFilterContext(inherited link);
end;

{ TCodeSystemProviderFilterPreparationContext }

function TCodeSystemProviderFilterPreparationContext.Link: TCodeSystemProviderFilterPreparationContext;
begin
  result := TCodeSystemProviderFilterPreparationContext(inherited Link);
end;

{ TCodeSystemIndexBasedIterator }

constructor TCodeSystemIteratorContext.Create(context: TCodeSystemProviderContext; count: integer);
begin
  inherited Create;
  FContext := context;
  FCount := count;
end;

destructor TCodeSystemIteratorContext.Destroy;
begin
  FContext.free;
  inherited;
end;

function TCodeSystemIteratorContext.Link: TCodeSystemIteratorContext;
begin
  result := TCodeSystemIteratorContext(inherited Link);
end;

function TCodeSystemIteratorContext.more: boolean;
begin
  result := FCurrent < FCount;
end;

procedure TCodeSystemIteratorContext.next;
begin
  inc(FCurrent);
end;

procedure TCodeSystemIteratorContext.moveCursor(current: integer);
begin
  FCurrent := current;
end;


{ ETerminologyError }

constructor ETerminologyError.Create(message: String; issueType: TFhirIssueType);
begin
  inherited Create(message);
  FIssueType := issueType;
  FopIssue := oicVoid;
end;

constructor ETerminologyError.Create(message: String; issueType: TFhirIssueType; opIssue: TOpIssueCode);
begin
  inherited Create(message);
  FIssueType := issueType;
  FopIssue := opIssue;
end;

{ ETerminologyTodo }

constructor ETerminologyTodo.Create(place: String);
begin
  inherited Create('Not done yet @ '+place, itException);
end;

{ TTxOperationContext }

constructor TTxOperationContext.create(version: TFhirVersion);
begin
  inherited Create;
  FVersion := version;
end;

{ TCodeSystemProviderFactory }

function TCodeSystemProviderFactory.link: TCodeSystemProviderFactory;
begin
  result := TCodeSystemProviderFactory(inherited link);
end;


end.
