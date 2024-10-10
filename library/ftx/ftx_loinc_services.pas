unit ftx_loinc_services;

{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

// Component:property:time:system:scale:method

{$I fhir.inc}

Interface

Uses
  SysUtils, Classes, Generics.Collections, {$IFDEF FPC} LazUTF8,{$ELSE} IOUtils, RegularExpressions, {$ENDIF}
  fsl_base, fsl_utilities, fsl_stream, fsl_collections, fsl_fpc, fsl_lang, fsl_http, fsl_regex, fsl_i18n, fsl_threads, fsl_logging,
  fdb_manager, fdb_sqlite3,
  fhir_objects, fhir_common, fhir_utilities, fhir_factory, fhir_features, fhir_uris,
  fhir_cdshooks,
  ftx_service;

{axes

Component
Property
System
Scale
Method
ClassType/Class
Source


also:
  all names
  order/obs
  v2 & v3 data type
}
const
  LOINC_KEY_CLASS = 3;
  LOINC_KEY_SCALE = 30;
  LOINC_KEY_COMPONENT = 4;
  LOINC_KEY_PROPERTY = 14;
  LOINC_KEY_TIME = 34;
  LOINC_KEY_SYSTEM = 33;
  LOINC_KEY_METHOD = 13;

  KEY_INCREMENT = 10;



type
  TKeyArray = array of cardinal;
  TLoincProviderContextKind = (lpckCode, lpckPart, lpckList, lpckAnswer);

  { TKeySet }

  TKeySet = class (TFslObject)
  private
    FKeys : TKeyArray;
    FCount : integer;
  public
    function addKey(key : integer) : boolean;
    procedure close;
    property Keys : TKeyArray read FKeys;
  end;

  { TDescriptionCacheEntry }
  TDescriptionCacheEntry = class (TFslObject)
  private
    FDisplay : boolean;
    FLang : String;
    FValue : String;
  public
    constructor create(display : boolean; lang: String; value : String);

    property display : boolean read FDisplay;
    property lang : String read FLang;
    property value : String read FValue;
  end;

  { TLoincProviderContext }

  TLoincProviderContext = class (TCodeSystemProviderContext)
  private
    FKey: cardinal;
    FKind : TLoincProviderContextKind;
    FCode : String;
    FDesc : String;
    FDisplays : TFslList<TDescriptionCacheEntry>;
    FChildren : TKeySet;
    procedure addChild(key : Integer);
  public
    constructor Create(key : cardinal; kind : TLoincProviderContextKind; code, desc : String);
    destructor Destroy; override;
    function link : TLoincProviderContext; overload;

    property Key: cardinal read FKey;
    property Kind : TLoincProviderContextKind read FKind;
    property Code : String read FCode;
    property Desc : String read FDesc;
  end;

  { TLoincDisplay }

  TLoincDisplay = class (TFslObject)
  private
    FLanguage : String;
    FValue : String;
  public
    constructor create(lang, v : String);
    property Language : String read FLanguage;
    property Value : String read FValue;
  end;

  { TLoincIteratorContext }

  TLoincIteratorContext = class (TCodeSystemIteratorContext)
  private 
    FKeys : TKeyArray;
  public
    constructor create(ctxt : TLoincProviderContext; keys : TKeyArray);
    function more : boolean; override;
  end;

  { TLoincFilterHolder }

  TLoincFilterHolder = class (TCodeSystemProviderFilterContext)
  private
    FKeys : TKeyArray;                                     
    FCursor : integer;
    lsql : String;
    function HasKey(key : cardinal) : boolean;
  end;

  { TLOINCPrep }

  TLOINCPrep = class (TCodeSystemProviderFilterPreparationContext)
  private
    filters : TFslList<TLoincFilterHolder>;
  public
    constructor Create; Override;
    destructor Destroy; Override;
  end;


  { TLOINCServices }

  TLOINCServices = class (TCodeSystemProvider)
  Private
    FLock : TFslLock;
    FDB : TFDBManager;
    FLangs : TDictionary<String, cardinal>;
    FCodes : TFslMap<TLoincProviderContext>;
    FCodeList : TFslList<TLoincProviderContext>;
    FVersion : String;
    FRoot : String;
    FFirstCodeKey : integer;
    FRelationships : TDictionary<String, String>;
    FProperties : TDictionary<String, String>;
    FStatusKeys : TDictionary<String, String>;
    function commaListOfCodes(source: String): String;
    function filterBySQL(c : TFDBConnection; d, sql, lsql : String; forIteration : boolean) : TCodeSystemProviderFilterContext;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport);
    destructor Destroy; Override;
    Function Link : TLOINCServices; Overload;

    Procedure Load(Const sFilename : String);
    class function checkFile(Const sFilename : String) : String;

    function description : String; override;
    function TotalCount : integer; override;
    function getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    //function findMAConcept(code : String) : Cardinal;
    function systemUri : String; override;
    function version : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; langList : THTTPLanguageList):String; override;
    function locate(code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; override;
    function sameContext(a, b : TCodeSystemProviderContext) : boolean; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string; override;
    procedure Designations(context : TCodeSystemProviderContext; list : TConceptDesignations); override;
    function doesFilter(prop : String; op : TFhirFilterOperator; value : String) : boolean; override;
    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function filter(forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function filterSize(ctxt : TCodeSystemProviderFilterContext) : integer; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; overload; override;
    function buildValueSet(factory : TFHIRFactory; id : String) : TFhirValueSetW;
    function getDefinition(code : String):String; override;
    function Definition(context : TCodeSystemProviderContext) : string; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure getCDSInfo(card : TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display : String); override;
    procedure extendLookup(factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; langList : THTTPLanguageList; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;
    //function subsumes(codeA, codeB : String) : String; override;
    procedure defineFeatures(features : TFslList<TFHIRFeature>); override;


    property DB : TFDBManager read FDB;
    property Langs : TDictionary<String, cardinal> read FLangs;
    property Codes : TFslMap<TLoincProviderContext> read FCodes;
    property CodeList : TFslList<TLoincProviderContext> read FCodelist;
    property VersionStr : String read FVersion;
    property Root : String read FRoot;
    property Relationships : TDictionary<String, String> read FRelationships;
    property Properties : TDictionary<String, String> read FProperties;
  End;

  TLOINCServiceList = class (TFslObjectList)
  Private
    FDefinition: TLOINCServices;
    function GetService(i: integer): TLOINCServices;
    procedure SetDefinition(const Value: TLOINCServices);
    function GetDefinition: TLOINCServices;
  Protected
    Function ItemClass : TFslObjectClass; Override;
    function sizeInBytesV(magic : integer) : cardinal; override;
  Public
    destructor Destroy; Override;

    Function GetServiceByName(sName : String) : TLOINCServices;

    Function HasDefaultService : Boolean;

    Property DefaultService : TLOINCServices Read GetDefinition write SetDefinition;

    Property Service[i : integer] : TLOINCServices read GetService; Default;
  End;

//function utfcopy(s : TBytes; start, length : integer) : String;
//function nolang : TLangArray;
//
function memU8toString(bytes : TBytes; index, chars : integer) : String;
function memU16ToString(const bytes : TBytes; index : cardinal; chars : word) : String;
function pct(i, t : integer) : String;

Implementation

function pct(i, t : integer) : String;
begin
  result := FloatToStrF((i * 100.0) / (t * 1.0), ffFixed, 1, 1)+'%';
end;

// the bytes contain UTF16
function memU16ToString(const bytes : TBytes; index : cardinal; chars : word) : String;
{$IFDEF FPC}
var
  u : UnicodeString;
{$ENDIF}
begin
  if chars = 0 then
    exit('');
  if (length(bytes) < index + chars*2) then
    raise ETerminologyError.create('Read off end of file: '+inttostr(length(bytes))+' / '+inttostr(index)+':'+inttostr(chars*2), itException);
  {$IFDEF FPC}
  setLength(u, chars);
  Move(bytes[index], u[1], chars*2);
  result := UTF16ToUTF8(u);
  {$ELSE}
  setLength(result, chars);
  Move(bytes[index], result[1], chars*2);
  {$ENDIF}
end;

// the bytes contain UTF8
function memU8toString(bytes : TBytes; index, chars : integer) : String;
begin
  if chars = 0 then
    exit('');
  if (length(bytes) < index + chars) then
    raise ETerminologyError.create('Read off end of file: '+inttostr(length(bytes))+' / '+inttostr(index)+':'+inttostr(chars), itException);
  {$IFDEF FPC}
  setLength(result, chars);
  Move(bytes[index], result[1], chars);
  {$ELSE}
  result := TEncoding.UTF8.GetString(bytes, index, chars);
  {$ENDIF}
end;


{ TLOINCServices }

constructor TLOINCServices.Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport);
begin
  inherited;
  FLangs := TDictionary<String, cardinal>.create;
  FCodes := TFslMap<TLoincProviderContext>.create;
  FCodes.DefaultValue := nil;
  FCodeList := TFslList<TLoincProviderContext>.create;
  FRelationships := TDictionary<String, String>.create;
  FProperties := TDictionary<String, String>.create;
  FStatusKeys := TDictionary<String, String>.create;
  FLock := TFslLock.create('LOINC');
end;

destructor TLOINCServices.Destroy;
begin
  FRelationships.free;
  FProperties.free;
  FStatusKeys.free;

  FCodeList.free;
  FCodes.free;
  FLangs.free;
  FLock.free;
  inherited;
end;

function TLOINCServices.Link: TLOINCServices;
begin
  result := TLOINCServices(inherited Link);
end;

procedure TLOINCServices.defineFeatures(features: TFslList<TFHIRFeature>);
begin
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'SCALE_TYP:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'SCALE_TYP:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'CLASS:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'COMPONENT:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'PROPERTY:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'TIME_ASPCT:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'SYSTEM:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'METHOD_TYP:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'ORDER_OBS:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'CLASSTYPE:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'STATUS:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'copyright:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'parent:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'ancestor:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'concept:is-a'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'concept:descends'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'LIST:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'TYPE:equals'));
end;

function TLOINCServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TLOINCServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TLOINCServices.description: String;
begin
  result := 'LOINC';
end;

procedure TLOINCServices.Load(const sFilename: String);
var
  c : TFDBConnection;
  i, k : integer;
  ci : TLoincProviderContext;
  s : String;
begin
  FDB := TFDBSQLiteManager.create(ExtractFileName(sFilename), sFilename, true, false, 10);
  c := FDB.GetConnection('load');
  try
    c.sql := 'Select LanguageKey, Code from Languages';
    c.prepare;
    c.Execute;
    while c.fetchnext do
      FLangs.Add(c.ColStringByName['Code'], c.ColKeyByName['LanguageKey']);
    c.terminate;

    c.sql := 'Select StatusKey, Description from StatusCodes';
    c.prepare;
    c.Execute;
    while c.fetchnext do
      FStatusKeys.Add(c.ColStringByName['Description'], c.ColStringByName['StatusKey']);
    c.terminate;

    c.sql := 'Select RelationshipTypeKey, Description from RelationshipTypes';
    c.prepare;
    c.Execute;
    while c.fetchnext do
    begin
      FRelationships.Add(c.ColStringByName['Description'], c.ColStringByName['RelationshipTypeKey']);
      s := c.ColStringByName['Description'];
      if (s <> c.ColStringByName['Description']) then
        FRelationships.Add(s, c.ColStringByName['RelationshipTypeKey']);
    end;
    c.terminate;

    c.sql := 'Select PropertyTypeKey, Description from PropertyTypes';
    c.prepare;
    c.Execute;
    while c.fetchnext do
      FProperties.Add(c.ColStringByName['Description'], c.ColStringByName['PropertyTypeKey']);
    c.terminate;

    FCodeList.add(nil); // keys start from 1

    c.sql := 'Select CodeKey, Code, Type, Description from Codes order by CodeKey Asc';
    c.prepare;
    c.Execute;
    i := 0;
    while c.fetchnext do
    begin
      inc(i);
      k := c.ColKeyByName['CodeKey'];
      if (i <> k) then
        raise EFslException.create('Error loading LOINC: Primary key has break in sequence at '+inttostr(k));
      ci := TLoincProviderContext.create(c.ColKey[1], TLoincProviderContextKind(c.ColKey[3]-1), c.ColString[2], c.ColString[4]);
      FCodes.Add(c.ColStringByName['Code'], ci);
      FCodeList.add(ci.link);
      if (FFirstCodeKey = 0) and (ci.Kind = lpckCode) then
        FFirstCodeKey := ci.Key;
    end;
    c.terminate;

    c.sql := 'Select Languages.Code as Lang, CodeKey as Key, DescriptionTypes.Description as DType, Value from Descriptions, Languages, DescriptionTypes where Descriptions.DescriptionTypeKey != 4 and Descriptions.DescriptionTypeKey = DescriptionTypes.DescriptionTypeKey and Descriptions.LanguageKey = Languages.LanguageKey';
    c.prepare;
    c.execute;
    while c.fetchNext do
      FCodeList[c.colKey[2]].FDisplays.add(TDescriptionCacheEntry.create('LongCommonName' = c.ColString[3], c.ColString[1], c.ColString[4]));
    c.terminate;

    c.sql := 'select SourceKey, TargetKey from Relationships where RelationshipTypeKey = '+FRelationships['child'];
    c.prepare;
    c.execute;
    while c.fetchNext do
      if (c.colKey[1] <> 0) and (c.colKey[2] <> 0) then
        FCodeList[c.colKey[1]].addChild(c.colKey[2]);
    c.terminate;

    for ci in FCodeList do
      if (ci <> nIl) and (ci.FChildren <> nil) then
        ci.FChildren.close;

    FVersion := c.Lookup('Config', 'ConfigKey', '2', 'Value', '');
    FRoot := c.Lookup('Config', 'ConfigKey', '3', 'Value', '');
    c.Release;
  except
    on e : exception do
    begin
      c.error(e);
      raise;
    end;
  end;
end;

class function TLOINCServices.checkFile(const sFilename: String): String;
var
  db : TFDBSQLiteManager;
  c : TFDBConnection;
begin
  try
    dB := TFDBSQLiteManager.create('db', sFilename, true, false, 10);
    try
      c := db.GetConnection('test');
      try
        if (c.Lookup('Config', 'ConfigKey', '1', 'Value', '') = 'c3c89b66-5930-4aa2-8962-124561a5f8c1') then
           result := 'Ok (version = '+c.Lookup('Config', 'ConfigKey', '2', 'Value', '')+')';
        c.Release;
      except
        on e : Exception do
        begin
          c.Error(e)
        end;
      end;
    finally
      db.free;
    end;
  except
    on e : Exception do
      result := 'Error: '+e.message;
  end;
end;

function TLOINCServices.searchFilter(filter: TSearchFilterText; prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise Exception.create('Not done yet');
end;

function TLOINCServices.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
end;

procedure TLOINCServices.getCDSInfo(card: TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display: String);
begin
  //b := TFslStringBuilder.Create;
  //try
  //  iRefs := nil;
  //  if not CodeList.FindCode(code, iIndex) Then
  //    b.Append('* Error: Code '+code+' not known')
  //  else
  //  Begin
  //    card.addLink('Further Detail', baseURL+'/loinc/doco/?type=loinc&code='+code);
  //
  //    CodeList.GetInformation(iIndex, langs, sCode1, iDescription, iOtherNames, iEntries, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);
  //    b.Append('LOINC Code '+code+' : '+Desc.GetEntry(iDescription, ll)+#13#10#13#10);
  //    if iComponent <> 0 Then
  //      b.Append('* Component: '+GetConceptDesc(iComponent, langs)+#13#10);
  //    if iProperty <> 0 Then
  //      b.Append('* Property: '+GetConceptDesc(iProperty, langs)+#13#10);
  //    if iTimeAspect <> 0 Then
  //      b.Append('* Time Aspect: '+GetConceptDesc(iTimeAspect, langs)+#13#10);
  //    if iSystem <> 0 Then
  //      b.Append('* System: '+GetConceptDesc(iSystem, langs)+#13#10);
  //    if iScale <> 0 Then
  //      b.Append('* Scale: '+GetConceptDesc(iScale, langs)+#13#10);
  //    if iMethod <> 0 Then
  //      b.Append('* Method: '+GetConceptDesc(iMethod, langs)+#13#10);
  //    if iClass <> 0 Then
  //      b.Append('* Class: '+GetConceptDesc(iClass, langs)+#13#10);
  //
  //    b.Append('* Type: ');
  //    if iFlags and FLAGS_CLIN > 0 Then
  //      b.Append('Clinical'+#13#10)
  //    Else if iFlags and FLAGS_ATT > 0 Then
  //      b.Append('Attachment'+#13#10)
  //    Else if iFlags and FLAGS_SURV > 0 Then
  //      b.Append('Survey'+#13#10)
  //    Else
  //      b.Append('Lab'+#13#10);
  //
  //    b.Append('* Status: ');
  //    if iFlags and FLAGS_HOLD > 0 Then
  //      b.Append('Not yet final'+#13#10)
  //    Else
  //      b.Append('Final'+#13#10);
  //
  //    if iFlags and FLAGS_UNITS > 0 Then
  //      b.Append('* Units are required'+#13#10);
  //
  //    b.Append('* Order/Obs Status: ');
  //    if (iFlags and FLAGS_ORDER> 0 ) and (iFlags and FLAGS_OBS> 0 ) Then
  //      b.Append('Both'+#13#10)
  //    Else if iFlags and FLAGS_ORDER > 0 Then
  //      b.Append('Order'+#13#10)
  //    Else if iFlags and FLAGS_OBS > 0 Then
  //      b.Append('Observation'+#13#10)
  //    else
  //      b.Append(#13#10);
  //
  //    if iOtherNames <> 0 Then
  //    begin
  //      first := true;
  //      b.Append('* Other Names: ');
  //      iRefs := Refs.GetRefs(iOtherNames);
  //      for i := Low(iRefs) To High(iRefs) Do
  //        if iRefs[i] <> 0 Then
  //        begin
  //          s := desc.GetEntry(iRefs[i], ll);
  //          ok := false;
  //          for ilang in langs do
  //            if (ilang = ll) then
  //              ok := true;
  //          if ok then
  //          begin
  //            if first then
  //              first := false
  //            else
  //              b.Append(', ');
  //            b.Append(s);
  //          end;
  //        End;
  //      b.Append(#13#10);
  //    End;
  //  End;
  //  b.Append(#13#10+'This LOINC&copy; content is copyright &copy; 1995 Regenstrief Institute, Inc. and the LOINC Committee, and available at no cost under the license at <http://loinc.org/terms-of-use>'#13#10);
  //  card.detail := b.ToString;
  //finally
  //  b.free;
  //end;
end;

function TLOINCServices.buildValueSet(factory : TFHIRFactory; id: String): TFhirValueSetW;
var
  inc : TFhirValueSetComposeIncludeW;
  filt :  TFhirValueSetComposeIncludeFilterW;
  cc : TFhirValueSetComposeIncludeConceptW;
  ci : TLoincProviderContext;
  c : TFDBConnection;
begin
  result := nil;
  if (id = '') then
  begin
    result := factory.wrapValueSet(factory.makeByName('ValueSet') as TFHIRResourceV);
    try
      result.url := id;
      result.status := psActive;
      result.version := version;
      result.name := 'LOINC Value Set - all LOINC codes';
      result.description := 'All LOINC codes';
      result.date := TFslDateTime.makeUTC;
      result.experimental := false;
      inc := result.addInclude;
      try
        inc.systemUri := URI_LOINC;
      finally
        inc.free;
      end;
      result.link;
    finally
      result.free;
    end;
  end
  else if (id.StartsWith('http://loinc.org/vs/') and FCodes.ContainsKey(id.Substring(20))) then
  begin
    ci := FCodes[id.Substring(20)];
    if (ci.kind = lpckPart) then
    begin
      result := factory.wrapValueSet(factory.makeByName('ValueSet') as TFHIRResourceV);
      try
        result.url := id;
        result.status := psActive;
        result.version := version;
        result.name := 'LOINCValueSetFor'+ci.code.replace('-', '_');
        result.description := 'LOINC value set for code '+ci.code+': '+ci.desc;
        result.date := TFslDateTime.makeUTC;
        result.experimental := false;
        inc := result.addInclude;
        try
          inc.systemUri := URI_LOINC;
          filt := inc.addFilter;
          try
            filt.prop := 'ancestor';
            filt.op := foEqual;
            filt.value := id.Substring(20);
          finally
            filt.free;
          end;
        finally
          inc.free;
        end;
        result.link;
      finally
        result.free;
      end;
    end
    else if (ci.kind = lpckList) then
    begin
      result := factory.wrapValueSet(factory.makeByName('ValueSet') as TFHIRResourceV);
      try
        result.url := id;
        result.status := psActive;
        result.version := version;
        result.name := 'LOINCAnswerList'+ci.code.replace('-', '_');
        result.description := 'LOINC Answer list for code '+ci.code+': '+ci.desc;
        result.date := TFslDateTime.makeUTC;
        result.experimental := false;
        inc := result.addInclude;
        try
          inc.systemUri := URI_LOINC;
          c := FDB.GetConnection('buildValueSet');
          try
            c.sql := 'select Code, Description from Relationships, Codes where SourceKey = '+inttostr(ci.key)+' and RelationshipTypeKey = 40 and Relationships.TargetKey = Codes.CodeKey';
            c.prepare;
            c.execute;
            while c.fetchnext do
            begin
              cc := inc.addConcept;
              try
                cc.code := c.colStringByName['Code'];
                cc.display := c.colStringByName['Description'];
              finally
                cc.free;
              end;
            end;
            c.terminate;
            c.release;
          except
            on e : Exception do
            begin
              c.error(e);
              raise;
            end;
          end;
        finally
          inc.free;
        end;
        result.link;
      finally
        result.free;
      end;
    end;
  end;
end;

function TLOINCServices.getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
var
  ctxt : TLoincProviderContext;
  c : TFDBConnection;
  keys : TKeyArray;
  l : integer;
begin
  ctxt := context as TLoincProviderContext;

  if context = nil then
  begin
    result := TCodeSystemIteratorContext.Create(nil, TotalCount);
    result.moveCursor(FFirstCodeKey);
  end
  else if ctxt.kind = lpckPart then
  begin
    if (ctxt.FChildren = nil) then
      result := TCodeSystemIteratorContext.Create(nil, 0)
    else
      result := TLoincIteratorContext.Create(ctxt.link, ctxt.FChildren.FKeys);
  end
  else
    result := TCodeSystemIteratorContext.Create(nil, 0);

end;

function TLOINCServices.getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
var
  ctxt : TLoincIteratorContext;
  i, k : integer;
begin
  if (context.context = nil) then
    result := FCodeList[context.current].link
  else
  begin
    ctxt := context as TLoincIteratorContext;
    i := context.current;
    k := ctxt.FKeys[i];
    result := FCodeList[k].link;
  end;
  context.next;
end;

function TLOINCServices.Code(context: TCodeSystemProviderContext): string;
begin
  result := (context as TLoincProviderContext).code;
end;

function TLOINCServices.Display(context: TCodeSystemProviderContext; langList : THTTPLanguageList): string;
var
  displays : TFslList<TLoincDisplay>;
  c : TFDBConnection;
  ll : THTTPLanguageEntry;
  disp : TLoincDisplay;
begin
  result := (context as TLoincProviderContext).Desc;
  if (langList <> nil) and not (langList.matches('en', true)) then
  begin
    displays := TFslList<TLoincDisplay>.create;
    try
      displays.add(TLoincDisplay.create('en-US', (context as TLoincProviderContext).Desc));

      c := FDB.getConnection('Designations');
      try
        displays.add(TLoincDisplay.create('en-US', (context as TLoincProviderContext).Desc));
        c.sql := 'Select Languages.Code as Lang, Value from Descriptions, Languages where CodeKey = '+inttostr((context as TLoincProviderContext).key)+' and Descriptions.DescriptionTypeKey in (1,2,5) and Descriptions.LanguageKey = Languages.LanguageKey order by DescriptionTypeKey';
        c.prepare;
        c.execute;
        while c.fetchNext do
          displays.add(TLoincDisplay.create(c.ColStringByName['Lang'], c.ColStringByName['Value']));
        c.terminate;
        c.release;
      except
        on e : Exception do
        begin
          c.error(e);
          raise;
        end;
      end;
      // now we have them all - iterate looking for a match
      for ll in langList.langs do
        for disp in displays do
          if ll.matches(disp.language, true) then
            exit(disp.value);
      for ll in langList.langs do
        for disp in displays do
          if ll.matches(disp.language, false) then
            exit(disp.value);
    finally
      displays.free;
    end;
  end;
end;

procedure TLOINCServices.Designations(context: TCodeSystemProviderContext; list: TConceptDesignations);
var
  c : TFDBConnection;
  ctxt : TLoincProviderContext;
  cache : TFslList<TDescriptionCacheEntry>;
  entry : TDescriptionCacheEntry;
begin
  ctxt := context as TLoincProviderContext;
  if (ctxt <> nil) then
  begin
    FLock.lock('Designations');
    try
      cache := ctxt.FDisplays;
    finally
      FLock.unlock;
    end;
    list.addDesignation(true, true, 'en-US', (context as TLoincProviderContext).Desc);
    if (cache = nil) then
    begin
      cache := TFslList<TDescriptionCacheEntry>.create;
      try
        c := FDB.getConnection('Designations');
        try
          c.sql := 'Select Languages.Code as Lang, DescriptionTypes.Description as DType, Value from Descriptions, Languages, DescriptionTypes where CodeKey = '+inttostr((context as TLoincProviderContext).key)+' and Descriptions.DescriptionTypeKey = DescriptionTypes.DescriptionTypeKey and Descriptions.LanguageKey = Languages.LanguageKey';
          c.prepare;
          c.execute;
          while c.fetchNext do
          begin
            list.addDesignation(false, 'LongCommonName' = c.ColStringByName['Type'], c.ColStringByName['Lang'], c.ColStringByName['Value']);
            cache.add(TDescriptionCacheEntry.create('LongCommonName' = c.ColStringByName['Type'], c.ColStringByName['Lang'], c.ColStringByName['Value']));
          end;
          c.terminate;
          c.release;
        except
          on e : Exception do
          begin
            c.error(e);
            raise;
          end;
        end;

        FLock.lock('Designations#2');
        try
          ctxt.FDisplays := cache.link;
        finally
          FLock.unlock;
        end
      finally
        cache.free;
      end;
    end
    else
    begin
      for entry in cache do
        list.addDesignation(false, entry.display, entry.lang, entry.value);
    end;
  end;
end;

function TLOINCServices.doesFilter(prop: String; op: TFhirFilterOperator; value: String): boolean;
var
  ts : TStringList;
  reg : TRegularExpression;
begin
  result := false;
  if (FRelationships.ContainsKey(prop) and (op = foEqual)) then
    if FCodes.ContainsKey(value) then
      result := true
    else
      result := true
  else if (FProperties.ContainsKey(prop) and (op = foEqual)) then
    result := true
  else if (FRelationships.ContainsKey(prop) and (op = foExists)) then
    if FCodes.ContainsKey(value) then
      result := true
    else
      result := true
  else if (FProperties.ContainsKey(prop) and (op = foExists)) then
    result := true
  else if (prop = 'STATUS') and (op = foEqual)and (FStatusKeys.ContainsKey(value)) then
    result := true
  else if (prop = 'LIST') and (op = foEqual) and (FCodes.ContainsKey(value)) then
    result := true
  else if (FRelationships.ContainsKey(prop)) and (op = foRegex) then
    result := true
  else if (FProperties.ContainsKey(prop)) and (op = foRegex) then
    result := true
  else if (prop = 'concept') and (op in [foIsA, foDescendentOf]) then
    result := true
  else if (prop = 'copyright') and (op = foEqual) and (value = 'LOINC') then
    result := true
  else if (prop = 'copyright') and (op = foEqual) and (value = '3rdParty') then
    result := true
  else
    result := false;
end;

function TLOINCServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := TLOINCPrep.Create;
end;

procedure TLOINCServices.extendLookup(factory : TFHIRFactory; ctxt: TCodeSystemProviderContext; langList : THTTPLanguageList; props: TArray<String>; resp: TFHIRLookupOpResponseW);
var
  c : TFDBConnection;
begin
  resp.isAbstract := false;
  c := FDB.getConnection('extendLookup');
  try
    c.sql := 'Select RelationshipTypes.Description as Relationship, Codes.Code, Codes.Description as Value from Relationships, RelationshipTypes, Codes where Relationships.SourceKey = '+inttostr((ctxt as TLoincProviderContext).key)+' and Relationships.RelationshipTypeKey = RelationshipTypes.RelationshipTypeKey and Relationships.TargetKey = Codes.CodeKey';
    c.prepare;
    c.execute;
    while c.fetchNext do
    begin
      resp.AddProp(c.colStringByName['Relationship']).value := Factory.makeCode(c.colStringByName['Code']);
    end;
    c.terminate;

    c.sql := 'Select Description, Value from Properties, PropertyTypes, PropertyValues where CodeKey = '+inttostr((ctxt as TLoincProviderContext).key)+' and Properties.PropertyTypeKey = PropertyTypes.PropertyTypeKey and Properties.PropertyValueKey = PropertyValues.PropertyValueKey';
    c.prepare;
    c.execute;
    while c.fetchNext do
      resp.AddProp(c.colStringByName['Description']).value := Factory.makeString(c.colStringByName['Value']);
    c.terminate;

    c.sql := 'Select StatusCodes.Description from Codes, StatusCodes where CodeKey = '+inttostr((ctxt as TLoincProviderContext).key)+' and Codes.StatusKey != 0 and Codes.StatusKey = StatusCodes.StatusKey';
    c.prepare;
    c.execute;
    while c.fetchNext do
      resp.AddProp('STATUS').value := Factory.makeString(c.colStringByName['Description']);
    c.terminate;

    case (ctxt as TLoincProviderContext).Kind of
      lpckCode: resp.addDesignation('en-US', 'http://loinc.org', 'LONG_COMMON_NAME', 'LONG_COMMON_NAME', (ctxt as TLoincProviderContext).desc);
      lpckPart: resp.addDesignation('en-US', 'http://loinc.org', 'PartDisplayName', 'PartDisplayName', (ctxt as TLoincProviderContext).desc);
      lpckList: resp.addDesignation('en-US', 'http://loinc.org', 'LONG_COMMON_NAME', 'LONG_COMMON_NAME', (ctxt as TLoincProviderContext).desc);
      lpckAnswer: resp.addDesignation('en-US', 'http://loinc.org', 'LONG_COMMON_NAME', 'LONG_COMMON_NAME', (ctxt as TLoincProviderContext).desc);
    end;


    c.sql := 'Select Languages.Code as Lang, DescriptionTypes.Description as DType, Value from Descriptions, Languages, DescriptionTypes where CodeKey = '+inttostr((ctxt as TLoincProviderContext).key)+' and Descriptions.DescriptionTypeKey != 4 and Descriptions.DescriptionTypeKey = DescriptionTypes.DescriptionTypeKey and Descriptions.LanguageKey = Languages.LanguageKey';
    c.prepare;
    c.execute;
    while c.fetchNext do
      resp.addDesignation(c.ColStringByName['Lang'], 'http://loinc.org', c.ColStringByName['DType'], c.ColStringByName['DType'], c.ColStringByName['Value']);
    c.terminate;

    c.release;
  except
    on e : Exception do
    begin
      c.error(e);
      raise;
    end;
  end;
end;


function TLOINCServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TLOINCServices.getDisplay(code: String; langList : THTTPLanguageList): String;
var
  ctxt : TLoincProviderContext;
begin
  ctxt := FCodes[code];
  if (ctxt = nil) then
    result := ''
  else
    result := Display(ctxt, langList);
end;

function TLOINCServices.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  result := false; // loinc don't do abstract
end;

function TLOINCServices.locate(code: String; altOpt : TAlternateCodeOptions; var message: String): TCodeSystemProviderContext;
begin
  result := FCodes[code].link;
end;

function TLOINCServices.sameContext(a, b: TCodeSystemProviderContext): boolean;
begin
  Result:=inherited sameContext(a, b);
end;

function TLOINCServices.systemUri: String;
begin
  result := URI_LOINC;
end;

function TLOINCServices.TotalCount: integer;
begin
  result := FCodes.Count;
end;

function TLOINCServices.version: String;
begin
  result := FVersion;
end;

function TLOINCServices.InFilter(ctxt: TCodeSystemProviderFilterContext; concept: TCodeSystemProviderContext): Boolean;
begin
  result := (ctxt as TLoincFilterHolder).HasKey((concept as TLoincProviderContext).Key);
end;

function TLOINCServices.filterBySQL(c : TFDBConnection; d, sql, lsql: String; forIteration : boolean): TCodeSystemProviderFilterContext;
var
  keys : TKeyArray;
  l : integer;
begin
  l := 0;
  if (forIteration) then
  begin
    SetLength(keys, 1000);
    l := 0;
    c.select(sql);
    while c.fetchnext do
    begin
      if (c.ColKeyByName['Key'] <> 0) then
      begin
        if (l = length(keys)) then
          SetLength(keys, l + 1000);
        keys[l] := c.ColKeyByName['Key'];
        inc(l);
      end;
    end;
    c.terminate;
  end;
  SetLength(keys, l);
  // Logging.log('LOINC filter: '+inttostr(l)+' rows for '+d+' (sql = '+sql+')');
  result := TLoincFilterHolder.create;
  TLoincFilterHolder(result).FKeys := keys;
  TLoincFilterHolder(result).lsql := lsql;
end;


function TLOINCServices.commaListOfCodes(source : String) : String;
var
  s : String;
begin
  result := '';
  for s in source.split([',']) do
    if FCodes.ContainsKey(s) then
      CommaAdd(result, s);
end;

function TLOINCServices.filter(forIteration : boolean; prop: String; op: TFhirFilterOperator; value: String; prep: TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
var
  c : TFDBConnection;
  ts : TStringList;
  reg : TRegularExpression;
  s : string;
  d : String;
begin
  d := prop+' '+CODES_TFhirFilterOperator[op]+' '+value;
  c := FDB.getConnection('filterBySQL');
  try
    if (FRelationships.ContainsKey(prop) and (op = foEqual)) then
      if FCodes.ContainsKey(value) then
        result := FilterBySQL(c, d, 'select SourceKey as Key from Relationships where RelationshipTypeKey = '+FRelationships[prop]+' and TargetKey in (select CodeKey from Codes where Code = '''+sqlwrapString(value)+''') order by SourceKey ASC',
          'select count(SourceKey) from Relationships where RelationshipTypeKey = '+FRelationships[prop]+' and TargetKey in (select CodeKey from Codes where Code = '''+sqlwrapString(value)+''') and SourceKey = ', forIteration)
      else
        result := FilterBySQL(c, d, 'select SourceKey as Key from Relationships where RelationshipTypeKey = '+FRelationships[prop]+' and TargetKey in (select CodeKey from Codes where Description = '''+sqlwrapString(value)+''' COLLATE NOCASE) order by SourceKey ASC',
          'select count(SourceKey) from Relationships where RelationshipTypeKey = '+FRelationships[prop]+' and TargetKey in (select CodeKey from Codes where Description = '''+sqlwrapString(value)+''' COLLATE NOCASE) and SourceKey = ', forIteration)
    else if (FRelationships.ContainsKey(prop) and (op = foIn)) then
    begin
      s := commaListOfCodes(value);
      result := FilterBySQL(c, d, 'select SourceKey as Key from Relationships where RelationshipTypeKey = '+FRelationships[prop]+' and TargetKey in (select CodeKey from Codes where Code in ('''+s+''') order by SourceKey ASC',
        'select count(SourceKey) from Relationships where RelationshipTypeKey = '+FRelationships[prop]+' and TargetKey in (select CodeKey from Codes where Code = '''+sqlwrapString(value)+''') and SourceKey = ', forIteration)
    end
    else if (FProperties.ContainsKey(prop) and (op = foEqual)) then
      result := FilterBySQL(c, d, 'select CodeKey as Key from Properties, PropertyValues where Properties.PropertyTypeKey = '+FProperties[prop]+' and Properties.PropertyValueKey  = PropertyValues.PropertyValueKey and PropertyValues.Value = '''+SQLWrapString(value)+''' COLLATE NOCASE order by CodeKey ASC',
        'select count(CodeKey) from Properties, PropertyValues where Properties.PropertyTypeKey = '+FProperties[prop]+' and Properties.PropertyValueKey  = PropertyValues.PropertyValueKey and PropertyValues.Value = '''+SQLWrapString(value)+''' COLLATE NOCASE and CodeKey = ', forIteration)
    else if (FProperties.ContainsKey(prop) and (op = foIn)) then
    begin    
      s := commaListOfCodes(value);
      result := FilterBySQL(c, d, 'select CodeKey as Key from Properties, PropertyValues where Properties.PropertyTypeKey = '+FProperties[prop]+' and Properties.PropertyValueKey  = PropertyValues.PropertyValueKey and PropertyValues.Value = ('''+SQLWrapString(s)+''') COLLATE NOCASE order by CodeKey ASC',
        'select count(CodeKey) from Properties, PropertyValues where Properties.PropertyTypeKey = '+FProperties[prop]+' and Properties.PropertyValueKey  = PropertyValues.PropertyValueKey and PropertyValues.Value = '''+SQLWrapString(value)+''' COLLATE NOCASE and CodeKey = ', forIteration)
    end
    else if (FRelationships.ContainsKey(prop) and (op = foExists)) then
      if FCodes.ContainsKey(value) then
        result := FilterBySQL(c, d, 'select SourceKey as Key from Relationships where RelationshipTypeKey = '+FRelationships[prop]+' and exists (select CodeKey from Codes where (Code = '''+sqlwrapString(value)+''')) order by SourceKey ASC',
          'select count(SourceKey) from Relationships where RelationshipTypeKey = '+FRelationships[prop]+' and exists (select CodeKey from Codes where (Code = '''+sqlwrapString(value)+''')) and SourceKey = ', forIteration)
      else
        result := FilterBySQL(c, d, 'select SourceKey as Key from Relationships where RelationshipTypeKey = '+FRelationships[prop]+' and exists (select CodeKey from Codes where (Description = '''+sqlwrapString(value)+''' COLLATE NOCASE)) order by SourceKey ASC',
          'select count(SourceKey) from Relationships where RelationshipTypeKey = '+FRelationships[prop]+' and exists (select CodeKey from Codes where (Description = '''+sqlwrapString(value)+''' COLLATE NOCASE)) and SourceKey = ', forIteration)
    else if (FRelationships.ContainsKey(prop) and (op = foIn)) then
    begin
      s := commaListOfCodes(value);
      result := FilterBySQL(c, d, 'select SourceKey as Key from Relationships where RelationshipTypeKey = '+FRelationships[prop]+' and exists (select CodeKey from Codes where (Code in ('''+sqlwrapString(s)+'''))) order by SourceKey ASC',
        'select count(SourceKey) from Relationships where RelationshipTypeKey = '+FRelationships[prop]+' and exists (select CodeKey from Codes where (Code = '''+sqlwrapString(value)+''')) and SourceKey = ', forIteration)
    end
    else if (FProperties.ContainsKey(prop) and (op = foExists)) then
      result := FilterBySQL(c, d, 'select distinct CodeKey as Key from Properties where Properties.PropertyTypeKey = '+FProperties[prop]+' order by CodeKey ASC',
        'select count(CodeKey) from Properties where Properties.PropertyTypeKey = '+FProperties[prop]+' and CodeKey = ', forIteration)
    else if (prop = 'STATUS') and (op = foEqual)and (FStatusKeys.ContainsKey(value)) then
      result := FilterBySQL(c, d, 'select CodeKey as Key from Codes where StatusKey = '+FStatusKeys[value]+' order by CodeKey ASC',
        'select count(CodeKey) from Codes where StatusKey = '+FStatusKeys[value]+' and CodeKey = ', forIteration)
    else if (prop = 'LIST') and (op = foEqual) and (FCodes.ContainsKey(value)) then
    result := FilterBySQL(c, d, 'select TargetKey as Key from Relationships where RelationshipTypeKey = '+FRelationships['Answer']+' and SourceKey in (select CodeKey from Codes where (Code = '''+sqlwrapString(value)+''')) order by SourceKey ASC',
      'select count(TargetKey) from Relationships where RelationshipTypeKey = '+FRelationships['Answer']+' and SourceKey in (select CodeKey from Codes where (Code = '''+sqlwrapString(value)+''')) and TargetKey = ', forIteration)
    else if (FRelationships.ContainsKey(prop)) and (op = foRegex) then
    begin
      reg := TRegularExpression.Create(value);
      try
        ts := TStringList.create;
        try
          c.select('Select CodeKey as Key, Description from Codes where CodeKey in (select TargetKey from Relationships where RelationshipTypeKey = '+FRelationships[prop]+')');
          while c.FetchNext do
            if reg.IsMatch(c.ColStringByName['Description']) then
              ts.add(c.ColStringByName['Key']);
          c.terminate;
          result := FilterBySQL(c, d, 'select SourceKey as Key from Relationships where RelationshipTypeKey = '+FRelationships[prop]+' and TargetKey in ('+ts.CommaText+') order by SourceKey ASC',
            'select count(SourceKey) from Relationships where RelationshipTypeKey = '+FRelationships[prop]+'  and TargetKey in ('+ts.CommaText+') and SourceKey = ', forIteration)
        finally
          ts.free;
        end;
      finally
        reg.free;
      end;
    end
    else if (FProperties.ContainsKey(prop)) and (op = foRegex) then
    begin
      reg := TRegularExpression.Create(value);
      try
        ts := TStringList.create;
        try
          c.select('Select PropertyValueKey, Value from PropertyValues where PropertyValueKey in (select PropertyValueKey from Properties where PropertyTypeKey = '+FProperties[prop]+')');
          while c.FetchNext do
            if reg.IsMatch(c.ColStringByName['Value']) then
              ts.add(c.ColStringByName['PropertyValueKey']);
          c.terminate;
          result := FilterBySQL(c, d, 'select CodeKey as Key from Properties where PropertyTypeKey = '+FProperties[prop]+' and PropertyValueKey in ('+ts.CommaText+') order by CodeKey ASC',
            'select count(CodeKey) from Properties where PropertyTypeKey = '+FProperties[prop]+' and PropertyValueKey in ('+ts.CommaText+') and CodeKey = ', forIteration)
        finally
          ts.free;
        end;
      finally
        reg.free;
      end;
    end
    else if (prop = 'concept') and (op in [foIsA, foDescendentOf]) then
      result := FilterBySQL(c, d, 'select DescendentKey as Key from Closure where AncestorKey in (select CodeKey from Codes where Code = '''+sqlwrapString(value)+''') order by DescendentKey ASC',
        'select count(DescendentKey) from Closure where AncestorKey in (select CodeKey from Codes where Code = '''+sqlwrapString(value)+''') and DescendentKey = ', forIteration)
    else if (prop = 'copyright') and (op = foEqual) and (value = 'LOINC') then
      result := FilterBySQL(c, d, 'select CodeKey as Key from Codes where not CodeKey in (select CodeKey from Properties where PropertyTypeKey = 9) order by CodeKey ASC',
        'select count(CodeKey) from Codes where not CodeKey in (select CodeKey from Properties where PropertyTypeKey = 9) and CodeKey = ', forIteration)
    else if (prop = 'copyright') and (op = foEqual) and (value = '3rdParty') then
      result := FilterBySQL(c, d, 'select CodeKey as Key from Codes where CodeKey in (select CodeKey from Properties where PropertyTypeKey = 9) order by CodeKey ASC',
        'select count(CodeKey) from Codes where CodeKey in (select CodeKey from Properties where PropertyTypeKey = 9) and CodeKey = ', forIteration)
    else
      result := nil;

    c.release;
  except
    on e : Exception do
    begin
      c.error(e);
      raise;
    end;
  end;
end;

function TLOINCServices.FilterConcept(ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
var
  ndx : integer;
  context : TLoincFilterHolder;
begin
  context := ctxt as TLoincFilterHolder;
  ndx := context.FKeys[context.FCursor-1];
  result := FCodeList[ndx].link;
end;

function TLOINCServices.FilterMore(ctxt: TCodeSystemProviderFilterContext): boolean;
var
  context : TLoincFilterHolder;
begin
  context := ctxt as TLoincFilterHolder;
  inc(context.FCursor);
  result := context.FCursor <= Length(context.FKeys);
end;

function TLOINCServices.filterSize(ctxt: TCodeSystemProviderFilterContext): integer;  
var
  context : TLoincFilterHolder;
begin
  context := ctxt as TLoincFilterHolder;
  result := Length(context.FKeys);
end;

function TLOINCServices.locateIsA(code, parent: String; disallowParent : boolean = false): TCodeSystemProviderContext;
begin
  result := nil; // cause loinc don't do subsumption
end;

function TLOINCServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'LOINC';
end;

function TLOINCServices.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String; var message: String): TCodeSystemProviderContext;
var
  ci : TLoincProviderContext;
  fi : TLoincFilterHolder;
  c : TFDBConnection;
begin
  fi := ctxt as TLoincFilterHolder;
  result := nil;

  ci := FCodes[code];
  if (ci = nil) then
    message := 'Not a valid code: '+code
  else if fi.lsql = '' then
    message := 'Filter not understood'
  else
  begin
    c := FDB.getConnection('filterLocate');
    try
      if c.CountSQL(fi.lsql+inttostr(ci.key)) > 0 then
        result := ci.link;
      c.release;
    except
      on e : Exception do
      begin
        c.error(e);
        raise;
      end;
    end;
  end;
end;


{ TLoincFilterHolder }

function TLoincFilterHolder.HasKey(key : cardinal) : boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to Length(FKeys) - 1 do
  begin
    if (FKeys[i] = key) then
      exit(true);
  end;
end;

{ TLOINCPrep }

constructor TLOINCPrep.Create;
begin
  inherited Create;
  filters := TFslList<TLoincFilterHolder>.create;
end;

destructor TLOINCPrep.Destroy;
begin
  filters.free;
  inherited Destroy;
end;

{ TDescriptionCacheEntry }

constructor TDescriptionCacheEntry.create(display: boolean; lang: String; value: String);
begin
  inherited create;
  FDisplay := display;
  FLang := lang;
  FValue := value;
end;


{ TLoincProviderContext }

procedure TLoincProviderContext.addChild(key: Integer);
begin
  if FChildren = nil then
    FChildren := TKeySet.create;
  FChildren.addKey(key);
end;

constructor TLoincProviderContext.Create(key: cardinal;
  kind: TLoincProviderContextKind; code, desc: String);
begin
  inherited Create;
  FKey := key;
  FCode := code;
  FKind := kind;
  FDesc := desc;
  FDisplays := TFslList<TDescriptionCacheEntry>.create;
end;

destructor TLoincProviderContext.Destroy;
begin
  FDisplays.free;
  FChildren.free;
  inherited Destroy;
end;

function TLoincProviderContext.link : TLoincProviderContext;
begin
  result := TLoincProviderContext(inherited link);
end;

{ TLOINCServiceList }

destructor TLOINCServiceList.Destroy;
begin
  FDefinition.free;
  inherited;
end;

function TLOINCServiceList.GetDefinition: TLOINCServices;
begin
  if FDefinition = nil then
    raise ETerminologyError.create('There is no default LOINC service', itException);
  result := FDefinition;
end;

function TLOINCServiceList.GetService(i: integer): TLOINCServices;
begin
  result := TLOINCServices(ObjectByIndex[i]);
end;

function TLOINCServiceList.GetServiceByName(sName: String): TLOINCServices;
var
  i : integer;
begin
  if sName = '' then
    result := DefaultService
  Else
  Begin
    Result := nil;
    i := 0;
    While (i < Count) and (result = nil) do
    Begin
      if SameText(Service[i].FVersion, sName) then
        result := Service[i];
      inc(i);
    End;
  End;end;

function TLOINCServiceList.HasDefaultService: Boolean;
begin
  result := FDefinition <> nil;
end;

function TLOINCServiceList.ItemClass: TFslObjectClass;
begin
  result := TLOINCServices;
end;

procedure TLOINCServiceList.SetDefinition(const Value: TLOINCServices);
begin
  FDefinition := Value;
end;

function TLOINCServiceList.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FDefinition.sizeInBytes(magic));
end;

                                                                    
{ TLoincDisplay }

constructor TLoincDisplay.create(lang, v : String);
begin
  inherited Create;
  FLanguage := lang;
  FValue := v;
end;

{ TLoincIteratorContext }

constructor TLoincIteratorContext.create(ctxt: TLoincProviderContext; keys: TKeyArray);
begin
  inherited create(ctxt, length(keys));
  FKeys := keys;
end;

function TLoincIteratorContext.more: boolean;
begin
  Result := inherited more;
end;

{ TKeySet }

function TKeySet.addKey(key: integer) : boolean;
var
  i : integer;
begin
  for i := low(FKeys) to High(FKeys) do
    if FKeys[i] = key then
      exit(false);
  if (FCount = length(FKeys)) then
    SetLength(FKeys, length(FKeys) + KEY_INCREMENT);
  FKeys[FCount] := key;
  inc(FCount);
  result := true;
end;

procedure TKeySet.close;
begin
  SetLength(FKeys, FCount);
end;

End.

