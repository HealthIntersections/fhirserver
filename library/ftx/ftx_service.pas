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
  fsl_utilities, fsl_base, fsl_collections, fsl_fpc, fsl_lang,
  fsl_http,
  fhir_common, fhir_factory, fhir_features, fhir_objects,
  fhir_cdshooks;

type
  ETerminologySetup = class (EFslException); // problem in the terminology configuration or loaded terminologies

  { ETerminologyError }

  ETerminologyError = class (EFslException) // problem in terminology operation
  private
    FIssueType : TFhirIssueType;
  public
    constructor create(message : String; issueType : TFhirIssueType);
    property IssueType : TFhirIssueType read FIssueType;
  end;

  ETerminologyTodo = Class(ETerminologyError)
  public
    Constructor Create(place : String);
  End;

const
  ANY_CODE_VS = 'http://hl7.org/fhir/ValueSet/@all';
  ALL_CODE_CS = 'http://hl7.org/fhir/CodeSystem/@all';

Type

  TFhirFilterOperator = fhir_common.TFilterOperator;

  TCodeSystemProviderContext = class (TFslObject)
  public
    function Link : TCodeSystemProviderContext; overload;
  end;

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
  end;

  TCodeSystemProviderFilterContext = class (TFslObject)
  public
    function Link : TCodeSystemProviderFilterContext; overload;
  end;

  TCodeSystemProviderFilterPreparationContext = class (TFslObject)
  public
    function Link : TCodeSystemProviderFilterPreparationContext; overload;
  end;

  { TConceptDesignation }

  TConceptDesignation = class (TFslObject)
  private
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

    class function build(languages : TIETFLanguageDefinitions; ccd : TFhirCodeSystemConceptDesignationW) : TConceptDesignation;

    property language : TIETFLang read FLanguage write SetLanguage;
    property use : TFHIRCodingW read FUse write SetUse;
    property value : TFHIRPrimitiveW read FValue write SetValue;
    property extensions : TFslList<TFHIRExtensionW> read GetExtensions;

    function present : String;
  end;

  { TConceptDesignations }

  TConceptDesignations = class (TFslObject)
  private
    FFactory : TFHIRFactory;
    FBaseLang : TIETFLang;
    FDisplay : TFHIRPrimitiveW;
    FDesignations : TFslList<TConceptDesignation>;
    FLanguages : TIETFLanguageDefinitions;
    function langMatches(allowedLangs : TFslList<TIETFLang>; stated : TIETFLang) : boolean;
    function stringMatches(source, possible : String; caseSensitive : boolean; lang : TIETFLang) : boolean;
    procedure SetBaseLang(value : TIETFLang);
    procedure SetDisplay(value : TFHIRPrimitiveW);
  public
    constructor Create(factory : TFHIRFactory; languages : TIETFLanguageDefinitions);
    destructor Destroy; override;

    function link : TConceptDesignations; overload;
    procedure clear;

    procedure addBase(lang, display : String);
    function  addDesignation(lang, display : String; beBase : boolean = false) : TConceptDesignation; overload;
    procedure addDesignation(lang : string; displays : TStringList; beBase : boolean = false); overload;
    function  addDesignation(lang : String = ''; value : TFHIRPrimitiveW = nil) : TConceptDesignation; overload;
    function  addDesignation(index : integer; lang : String = ''; value : TFHIRPrimitiveW = nil; extensions : TFslList<TFHIRExtensionW> = nil) : TConceptDesignation; overload;
    function hasDisplay(allowedLangs : TFslList<TIETFLang>; value : String; caseSensitive : boolean = false) : boolean;
    function displayCount(allowedLangs : TFslList<TIETFLang>) : integer;
    function preferredDisplay(allowedLangs : TFslList<TIETFLang>) : String;
    function present(allowedLangs : TFslList<TIETFLang>) : String;
    function findDisplay(allowedLangs : TFslList<TIETFLang>) : TConceptDesignation;

    property factory : TFHIRFactory read FFactory;
    property baseLang : TIETFLang read FBaseLang write SetBaseLang;
    property display : TFHIRPrimitiveW read FDisplay write SetDisplay;
    property designations : TFslList<TConceptDesignation> read FDesignations;
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
    function passes(stems : TFslStringList; var rating : double) : boolean; overload;
    property filter : string read FFilter;
    property stems : TStringList read FStems;
  end;

  { TCodeSystemProvider }

  TCodeSystemProvider = class abstract (TFslObject)
  private
    FUseCount : cardinal;
  protected
    FLanguages : TIETFLanguageDefinitions;
  public
    constructor Create(languages : TIETFLanguageDefinitions);
    destructor Destroy; override;

    function Link : TCodeSystemProvider; overload;

    function contentMode : TFhirCodeSystemContentMode; virtual;
    function description : String;  virtual; abstract;
    function TotalCount : integer;  virtual; abstract;
    function getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; virtual; abstract;
    function getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; virtual; abstract;
    function systemUri(context : TCodeSystemProviderContext) : String; virtual; abstract;
    function version(context : TCodeSystemProviderContext) : String; virtual;
    function name(context : TCodeSystemProviderContext) : String; virtual;
    function getDisplay(code : String; const lang : THTTPLanguages):String; virtual; abstract;
    function getDefinition(code : String):String; virtual; abstract;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; overload; virtual; abstract;
    function locate(code : String) : TCodeSystemProviderContext; overload; virtual;
    function sameContext(a, b : TCodeSystemProviderContext) : boolean; virtual;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; virtual; abstract;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; virtual; abstract;
    function IsInactive(context : TCodeSystemProviderContext) : boolean; overload; virtual;
    function deprecated(context : TCodeSystemProviderContext) : boolean; overload; virtual;
    function IsInactive(code : String) : boolean; overload; virtual;
    function Code(context : TCodeSystemProviderContext) : string; virtual; abstract;
    function Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string;  overload; virtual; abstract;
    function Display(context : TCodeSystemProviderContext; lang : TFslList<TIETFLang>) : string; overload;
    function Definition(context : TCodeSystemProviderContext) : string; virtual; abstract;
    function itemWeight(context : TCodeSystemProviderContext) : string; virtual;
    procedure Designations(context : TCodeSystemProviderContext; list : TConceptDesignations); overload; virtual; abstract;  // get all displays for all languages
    function getExtensions(context : TCodeSystemProviderContext)  : TFslList<TFHIRExtensionW>; virtual;
    function getProperties(context : TCodeSystemProviderContext) : TFslList<TFhirCodeSystemConceptPropertyW>; virtual;
    function parent(context : TCodeSystemProviderContext) : String; virtual; // return if there is one and only one
    function canParent : boolean; virtual;
    function doesFilter(prop : String; op : TFhirFilterOperator; value : String) : boolean; virtual;

    function hasSupplement(url : String) : boolean; virtual;
    function getPrepContext : TCodeSystemProviderFilterPreparationContext; virtual;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; virtual; abstract;
    function specialFilter(prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; virtual;
    function filter(forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; virtual; abstract;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; virtual; // true if the underlying provider collapsed multiple filters
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; overload; virtual; abstract;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; overload; virtual;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; virtual; abstract;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; virtual; abstract;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; virtual; abstract;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; virtual; abstract;
    procedure extendLookup(factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; const lang : THTTPLanguages; props : TArray<String>; resp : TFHIRLookupOpResponseW); virtual;
    function subsumesTest(codeA, codeB : String) : String; virtual;

    function SpecialEnumeration : String; virtual;
    procedure defineFeatures(features : TFslList<TFHIRFeature>); virtual; abstract;
    procedure getCDSInfo(card : TCDSHookCard; const lang : THTTPLanguages; baseURL, code, display : String); virtual;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); overload; virtual;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); overload; virtual; abstract;
    procedure Close(ctxt : TCodeSystemProviderContext); overload; virtual; abstract;

    procedure RecordUse(count : integer = 1);
    function defToThisVersion(specifiedVersion : String) : boolean; virtual;
    property UseCount : cardinal read FUseCount;
  end;

implementation

{ TConceptDesignations }

destructor TConceptDesignations.Destroy;
begin
  FLanguages.Free;
  FDisplay.Free;
  FFactory.Free;
  FBaselang.Free;
  FDesignations.Free;
  inherited Destroy;
end;

function TConceptDesignations.link: TConceptDesignations;
begin
  result := TConceptDesignations(inherited Link);
end;

procedure TConceptDesignations.clear;
begin
  BaseLang := nil;
  Display := nil;
  FDesignations.clear;
end;

procedure TConceptDesignations.addBase(lang, display: String);
begin
  FBaseLang := FLanguages.parse(lang);
  FDisplay := FFactory.wrapPrimitive(FFactory.makeString(display));
end;

function TConceptDesignations.addDesignation(lang, display: String; beBase : boolean = false): TConceptDesignation;
begin
  if (beBase and (FDisplay = nil)) then
    addBase(lang, display)
  else
  begin
    result := TConceptDesignation.create;
    try
      result.language := FLanguages.parse(lang);
      result.value := FFactory.wrapPrimitive(FFactory.makeString(display));
      FDesignations.add(result.link);
    finally
      result.free;
    end;
  end;
end;

procedure TConceptDesignations.addDesignation(lang: string; displays: TStringList; beBase : boolean = false);
var
  s : String;
begin
  if (displays <> nil) then
    for s in displays do
      addDesignation(lang, s, beBase);
end;

function TConceptDesignations.addDesignation(lang: String; value: TFHIRPrimitiveW): TConceptDesignation;
begin
  result := TConceptDesignation.create;
  try
    result.language := FLanguages.parse(lang);
    result.value := value; {no .link}
    FDesignations.add(result.link);
  finally
    result.free;
  end;
end;

function TConceptDesignations.addDesignation(index: integer; lang: String; value: TFHIRPrimitiveW; extensions : TFslList<TFHIRExtensionW>): TConceptDesignation;
var
  ext : TFHIRExtensionW;
begin
  result := TConceptDesignation.create;
  try
    result.language := FLanguages.parse(lang);
    result.value := value; {no .link}
    if (extensions <> nil) then
      for ext in extensions do
        result.extensions.add(ext.link);
    FDesignations.Insert(index, result.link);
  finally
    result.free;
  end;
end;

function TConceptDesignations.hasDisplay(allowedLangs: TFslList<TIETFLang>; value: String; caseSensitive : boolean): boolean;
var
  cd : TConceptDesignation;
begin
  result := false;
  if (langMatches(allowedLangs, FBaseLang) and (FDisplay <> nil) and stringMatches(value, FDisplay.asString, caseSensitive, FBaseLang)) then
    exit(true);
  for cd in designations do
    if (langMatches(allowedLangs, cd.language) and (cd.value <> nil) and stringMatches(value, cd.value.asString, caseSensitive, cd.language)) then
      exit(true);
end;

function TConceptDesignations.displayCount(allowedLangs: TFslList<TIETFLang>): integer;
var
  cd : TConceptDesignation;
begin
  result := 0;
  if (langMatches(allowedLangs, FBaseLang) and (FDisplay <> nil)) then
    inc(result);
  for cd in designations do
    if (langMatches(allowedLangs, cd.language) and (cd.value <> nil)) then
      inc(result);
end;

function TConceptDesignations.preferredDisplay(allowedLangs: TFslList<TIETFLang>): String;
var
  cd : TConceptDesignation;
begin
  result := '';
  if (langMatches(allowedLangs, FBaseLang) and (FDisplay <> nil)) then
    exit(FDisplay.AsString);
  for cd in designations do
    if ((cd.use = nil) and langMatches(allowedLangs, cd.language) and (cd.value <> nil)) then
      exit(cd.value.asString);
  for cd in designations do
    if (langMatches(allowedLangs, cd.language) and (cd.value <> nil)) then
      exit(cd.value.asString);
end;

function TConceptDesignations.present(allowedLangs: TFslList<TIETFLang>): String;
var
  cd : TConceptDesignation;
  b : TCommaSeparatedStringBuilder;
begin
  b := TCommaSeparatedStringBuilder.create(', ', ' or ');
  try
    if (langMatches(allowedLangs, FBaseLang) and (FDisplay <> nil)) then
      b.append(''''+FDisplay.AsString+'''');
    for cd in designations do
      if (langMatches(allowedLangs, cd.language) and (cd.value <> nil)) then
        b.append(''''+cd.present+'''');
    result := b.makeString;
  finally
    b.free;
  end;
end;

function TConceptDesignations.findDisplay(allowedLangs: TFslList<TIETFLang>): TConceptDesignation;
var
  cd : TConceptDesignation;
begin
  result := nil;
  for cd in designations do
    if ((cd.use = nil) and langMatches(allowedLangs, cd.language) and (cd.value <> nil)) then
      exit(cd);
  for cd in designations do
    if (langMatches(allowedLangs, cd.language) and (cd.value <> nil)) then
      exit(cd);
end;

function TConceptDesignations.langMatches(allowedLangs: TFslList<TIETFLang>; stated: TIETFLang): boolean;
var
  lang : TIETFLang;
begin
  if (stated = nil) or (allowedLangs.Empty) then
    result := true
  else
  begin
    result := false;
    for lang in allowedLangs do
      if lang.matches(stated) then
        exit(true);
  end;
end;

function TConceptDesignations.stringMatches(source, possible: String; caseSensitive: boolean; lang: TIETFLang): boolean;
begin
  // we ignore lang at this time
  if (caseSensitive) then
    result := source = possible
  else
    result := SameText(source, possible);
end;

procedure TConceptDesignations.SetBaseLang(value: TIETFLang);
begin
  FBaselang.Free;
  FBaseLang := value;
end;

procedure TConceptDesignations.SetDisplay(value: TFHIRPrimitiveW);
begin
  FDisplay.Free;
  FDisplay := value;
end;

constructor TConceptDesignations.Create(factory : TFHIRFactory; languages : TIETFLanguageDefinitions);
begin
  inherited Create;
  FFactory := factory;
  FLanguages := languages;
  FDesignations := TFslList<TConceptDesignation>.create;
end;

{ TConceptDesignation }

destructor TConceptDesignation.Destroy;
begin
  FLanguage.Free;
  FUse.Free;
  FValue.Free;
  FExtensions.Free;
  inherited Destroy;
end;

procedure TConceptDesignation.SetLanguage(const Value: TIETFLang);
begin
  FLanguage.Free;
  FLanguage := Value;
end;

function TConceptDesignation.GetExtensions: TFslList<TFHIRExtensionW>;
begin
  if FExtensions = nil then
    FExtensions := TFslList<TFHIRExtensionW>.create;
  result := FExtensions;
end;

procedure TConceptDesignation.SetUse(const Value: TFHIRCodingW);
begin
  FUse.Free;
  FUse := value;
end;

procedure TConceptDesignation.SetValue(const Value: TFHIRPrimitiveW);
begin
  FValue.Free;
  FValue := Value;
end;

constructor TConceptDesignation.Create;
begin
  inherited Create;
end;

class function TConceptDesignation.build(languages : TIETFLanguageDefinitions; ccd : TFhirCodeSystemConceptDesignationW) : TConceptDesignation;
var
  list : TFslList<TFHIRExtensionW>;
begin
  result := TConceptDesignation.create;
  try
    result.Language := languages.parse(ccd.language);
    result.Use := ccd.use;
    result.Value := ccd.valueElement; {no .link}
    list := ccd.getExtensionsW('http://hl7.org/fhir/StructureDefinition/coding-sctdescid');
    try
      if (list.count > 0) then
        result.extensions.addAll(list);
    finally
      list.free;
    end;
    result.link;
  finally
    result.free;
  end;
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

procedure TCodeSystemProvider.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  // do nothing
end;

constructor TCodeSystemProvider.Create(languages: TIETFLanguageDefinitions);
begin
  inherited create;
  FLanguages := languages;
end;

function TCodeSystemProvider.defToThisVersion(specifiedVersion : String): boolean;
begin
  result := true;
end;

destructor TCodeSystemProvider.Destroy;
begin
  FLanguages.Free;
  inherited;
end;

function TCodeSystemProvider.doesFilter(prop: String; op: TFhirFilterOperator; value: String): boolean;
var
  ctxt : TCodeSystemProviderFilterContext;
begin
  result := false;
  ctxt := filter(true, prop, op, value, nil);
  try
    result := ctxt <> nil;
  finally
    if result then
      Close(ctxt);
  end;
end;

procedure TCodeSystemProvider.extendLookup(factory : TFHIRFactory; ctxt: TCodeSystemProviderContext; const lang : THTTPLanguages; props : TArray<String>; resp : TFHIRLookupOpResponseW);
begin
  // nothing here
end;

function TCodeSystemProvider.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String): TCodeSystemProviderContext;
var
  msg : String;
begin
  result := filterLocate(ctxt, code, msg);
end;

procedure TCodeSystemProvider.getCDSInfo(card: TCDSHookCard; const lang : THTTPLanguages; baseURL, code, display: String);
begin
  card.summary := 'No CDSHook Implemeentation for code system '+systemUri(nil)+' for code '+code+' ('+display+')';
end;

function TCodeSystemProvider.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

function TCodeSystemProvider.hasSupplement(url: String): boolean;
begin
  result := false;
end;

function TCodeSystemProvider.IsInactive(code: String): boolean;
var
  ctxt : TCodeSystemProviderContext;
begin
  ctxt := locate(code);
  try
    result := IsInactive(ctxt);
  finally
    Close(ctxt);
  end;
end;

function TCodeSystemProvider.Display(context: TCodeSystemProviderContext; lang: TFslList<TIETFLang>): string;
var
  hl : THTTPLanguages;
  l : TIETFLang;
begin
  for l in lang do
    hl.AddCode(l.code);
  result := display(context, hl);
end;

function TCodeSystemProvider.itemWeight(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TCodeSystemProvider.contentMode: TFhirCodeSystemContentMode;
begin
  result := cscmComplete; // unless specified otherwise
end;

function TCodeSystemProvider.IsInactive(context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function TCodeSystemProvider.deprecated(context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function TCodeSystemProvider.Link: TCodeSystemProvider;
begin
  result := TCodeSystemProvider(inherited link);
end;

function TCodeSystemProvider.locate(code: String): TCodeSystemProviderContext;
var
  msg : String;
begin
  result := locate(code, msg);
end;

function TCodeSystemProvider.name(context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

function TCodeSystemProvider.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  result := false;
end;

procedure TCodeSystemProvider.RecordUse(count : integer = 1);
begin
  FUseCount := FUseCount + count;
end;

function TCodeSystemProvider.sameContext(a, b: TCodeSystemProviderContext): boolean;
begin
  result := a = b;
end;

function TCodeSystemProvider.SpecialEnumeration: String;
begin
  result := '';
end;

function TCodeSystemProvider.specialFilter(prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.create('Not implemented for '+ClassName, itException);
end;

function TCodeSystemProvider.subsumesTest(codeA, codeB: String): String;
begin
  raise ETerminologyError.create('Subsumption Testing is not supported for system '+systemUri(nil), itException);
end;

function TCodeSystemProvider.getExtensions(context : TCodeSystemProviderContext) : TFslList<TFHIRExtensionW>;
begin
  result := nil;
end;

function TCodeSystemProvider.getProperties(context: TCodeSystemProviderContext): TFslList<TFhirCodeSystemConceptPropertyW>;
begin
  result := nil
end;

function TCodeSystemProvider.parent(context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

function TCodeSystemProvider.canParent: boolean;
begin
  result := false;
end;

function TCodeSystemProvider.version(context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

{ TSearchFilterText }

constructor TSearchFilterText.create(filter: String);
begin
  Create;
  FStemmer := TFslWordStemmer.create('english');
  FStems := TStringList.Create;
  FFilter := filter;
  process;
end;

destructor TSearchFilterText.destroy;
begin
  FStems.Free;
  FStemmer.Free;
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

function TSearchFilterText.passes(stems: TFslStringList; var rating : double): boolean;
var
  i : integer;
  all, any, this : boolean;
begin
  rating := 0;
  if FStems.Count = 0 then
    result := true
  else
  begin
    all := true;
    any := false;
    for i := 0 to stems.count - 1 do
    begin
      this := find(stems[i]);
      all := all and this;
      any := any or this;
      if this then
        rating := rating + length(stems[i])/Fstems.count;
    end;
    result := any;
  end;
end;

function TSearchFilterText.passes(cds: TConceptDesignations): boolean;
var
  cd : TConceptDesignation;
begin
  if (cds = nil) then
    exit(false);
  if (cds.display = nil) then
    result := false
  else
    result := passes(cds.display.AsString);
  for cd in cds.designations do
    if (cd.value <> nil) and passes(cd.value.AsString) then
      exit(true);
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


{ ETerminologyError }

constructor ETerminologyError.create(message: String; issueType: TFhirIssueType);
begin
  inherited Create(message);
  FIssueType := issueType;
end;

{ ETerminologyTodo }

constructor ETerminologyTodo.Create(place: String);
begin
  inherited create('Not done yet @ '+place, itException);
end;

end.
