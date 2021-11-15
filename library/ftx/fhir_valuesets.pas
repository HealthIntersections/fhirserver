unit fhir_valuesets;

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

{$I fhir.inc}

interface

{
todo:
  include designations
  include Inactive

}
uses
  SysUtils, Classes,
  fsl_base, fsl_collections, fsl_utilities, fsl_http, fsl_lang,
  fhir_objects, fhir_common, ftx_service, fhir_factory, fhir_xhtml,
  fhir_codesystem_service;

{  SysUtils, Classes, fsl_utilities, fsl_utilities,
  fsl_utilities,
  fsl_collections, fsl_base,
  fhir_objects,  fhir_common, fhir_factory,
  ftx_service;
  //, ftx_loinc_services, ftx_sct_services, ftx_ucum_services, FHIR.Tx.Server, FHIR.Tx.Manager;}

const
  UPPER_LIMIT_NO_TEXT = 10000;
  UPPER_LIMIT_TEXT = 1000;// won't expand a value set bigger than this - just takes too long, and no one's going to do anything with it anyway

  FHIR_VERSION_CANONICAL_SPLIT_2 = '?version=';
  FHIR_VERSION_CANONICAL_SPLIT_3p = '|';


Type

  TFhirExpansionParamsFixedVersionMode = (fvmDefault, fvmCheck, fvmOverride);
  TValueSetValidationMode = (vsvmAllChecks, vsvmMembershipOnly, vsvmNoMembership);

  TFhirExpansionParamsFixedVersion = class (TFslObject)
  private
    Fsystem : String;
    Fversion : String;
    FMode : TFhirExpansionParamsFixedVersionMode;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(system, version : String); overload;
    constructor Create(system, version : String; mode : TFhirExpansionParamsFixedVersionMode); overload;

    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property mode : TFhirExpansionParamsFixedVersionMode read FMode write FMode;
  end;

  TFHIRExpansionParams = class (TFslObject)
  private
    FFixedVersions : TFslList<TFhirExpansionParamsFixedVersion>;
    FactiveOnly: boolean;
    FdisplayLanguage: THTTPLanguages;
    FexcludeNested: boolean;
    FlimitedExpansion: boolean;
    FexcludeNotForUI: boolean;
    FexcludePostCoordinated: boolean;
    FincludeDesignations: boolean;
    FincludeDefinition: boolean;
    FUid: String;
    FValueSetMode: TValueSetValidationMode;
    FDefaultToLatestVersion : boolean;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRExpansionParams;
    class function defaultProfile : TFHIRExpansionParams;

    property fixedVersions : TFslList<TFhirExpansionParamsFixedVersion> read FFixedVersions;
    property activeOnly : boolean read FactiveOnly write FactiveOnly;
    property displayLanguage : THTTPLanguages read FdisplayLanguage write FdisplayLanguage;
    property includeDefinition : boolean read FincludeDefinition write FincludeDefinition;
    property limitedExpansion : boolean read FlimitedExpansion write FlimitedExpansion;
    property includeDesignations : boolean read FincludeDesignations write FincludeDesignations;
    property excludeNested : boolean read FexcludeNested write FexcludeNested;
    property excludeNotForUI : boolean read FexcludeNotForUI write FexcludeNotForUI;
    property excludePostCoordinated : boolean read FexcludePostCoordinated write FexcludePostCoordinated;
    property valueSetMode : TValueSetValidationMode read FValueSetMode write FValueSetMode;
    property uid : String read FUid write FUid;
    property defaultToLatestVersion : boolean read FDefaultToLatestVersion write FDefaultToLatestVersion;

    function hash : String;
  end;

  TSpecialProviderFilterContextNothing = class (TCodeSystemProviderFilterContext);
  TSpecialProviderFilterContextConcepts = class (TCodeSystemProviderFilterContext)
  private
    FList : TFslList<TCodeSystemProviderContext>;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure add(c : TCodeSystemProviderContext);
  end;

  TFHIRImportedValueSet = class (TFslObject)
  private
    FValueSet: TFHIRValueSetW;
    FMap : TStringList;
    procedure SetValueSet(const Value: TFHIRValueSetW);
    function key(system, code : String) : String;
    procedure addToMap(c : TFhirValueSetExpansionContainsW);
  public
    constructor Create(valueSet : TFHIRValueSetW);
    destructor Destroy; override;

    procedure buildMap;
    function hasCode(system, code : String) : boolean;

    property valueSet : TFHIRValueSetW read FValueSet write SetValueSet;
  end;

  TGetValueSetEvent = function (sender : TObject; url : String) : TFHIRValueSetW of object;
  TGetProviderEvent = function (sender : TObject; url, version : String; params : TFHIRExpansionParams; nullOk : boolean) : TCodeSystemProvider of object;
  TGetExpansionEvent = function (sender : TObject; url, filter : String; params : TFHIRExpansionParams; dependencies : TStringList; additionalResources : TFslMetadataResourceList; limit : integer) : TFHIRValueSetW of object;
  TGetSystemVersionsEvent = procedure (sender : TObject; url : String; list : TStringlist) of object;

  TValueSetWorker = class (TFslObject)
  private
    FFactory : TFHIRFactory;
    FOnGetValueSet : TGetValueSetEvent;
    FOnGetCSProvider : TGetProviderEvent;
    FOnListCodeSystemVersions : TGetSystemVersionsEvent;
    FParams : TFHIRExpansionParams;
    FAdditionalResources : TFslMetadataResourceList;
    FLanguages : TIETFLanguageDefinitions;

    function findValueSet(url : String) : TFHIRValueSetW;
    function findCodeSystem(url, version : String; params : TFHIRExpansionParams; nullOk : boolean) : TCodeSystemProvider;
    function listVersions(url : String) : String;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
    procedure listDisplays(displays : TCodeDisplays; cs : TCodeSystemProvider; c: TCodeSystemProviderContext); overload;
    procedure listDisplays(displays : TCodeDisplays; c: TFhirCodeSystemConceptW); overload;
    procedure listDisplays(displays: TCodeDisplays; c: TFhirValueSetComposeIncludeConceptW); overload;
  public
    constructor Create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions); overload;
    destructor Destroy; override;
  end;

  TValueSetChecker = class (TValueSetWorker)
  private
    FOthers : TFslStringObjectMatch; // checkers or code system providers
    FValueSet : TFHIRValueSetW;
    FId: String;
    FLog : String;

    function determineSystem(code : String) : String;
    function check(system, version, code : String; abstractOk, implySystem : boolean; displays : TCodeDisplays; var message, ver : String; var cause : TFhirIssueType; op : TFhirOperationOutcomeW) : boolean; overload;
    function findCode(cs : TFhirCodeSystemW; code: String; list : TFhirCodeSystemConceptListW; displays : TCodeDisplays; out isabstract : boolean): boolean;
    function checkConceptSet(cs: TCodeSystemProvider; cset : TFhirValueSetComposeIncludeW; code : String; abstractOk : boolean; displays : TCodeDisplays; var message : String) : boolean;
    procedure prepareConceptSet(desc: string; cc: TFhirValueSetComposeIncludeW);
    function getName: String;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; id : String); overload;
    destructor Destroy; override;

    property id : String read FId;
    property name : String read getName;

    procedure prepare(vs : TFHIRValueSetW; params : TFHIRExpansionParams);

    function check(system, version, code : String; abstractOk, implySystem : boolean; op : TFhirOperationOutcomeW) : boolean; overload;
    function check(system, version, code : String; implySystem : boolean) : TFhirParametersW; overload;
    function check(coding : TFhirCodingW; abstractOk, implySystem : boolean): TFhirParametersW; overload;
    function check(code: TFhirCodeableConceptW; abstractOk, implySystem : boolean) : TFhirParametersW; overload;

    property log : String read FLog;
  end;

  TFHIRValueSetExpander = class (TValueSetWorker)
  private
    FCount : integer;
    FOffset : integer;
    FOnGetExpansion : TGetExpansionEvent;
    FLang : String;

    function makeFilterForValueSet(cs : TCodeSystemProvider; vs : TFHIRValueSetW) : TCodeSystemProviderFilterContext;
    procedure processCodeAndDescendants(doDelete : boolean; list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; limitCount : integer; cs : TCodeSystemProvider; context : TCodeSystemProviderContext; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>);

    procedure handleDefine(cs : TFhirCodeSystemW; list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; limitCount : integer; source : TFhirValueSetCodeSystemW; defines : TFhirCodeSystemConceptListW; filter : TSearchFilterText; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>);
    procedure importValueSet(list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; vs : TFHIRValueSetW; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; offset : integer);
    procedure excludeValueSet(list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; vs : TFHIRValueSetW; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; offset : integer);
    procedure processCodes(doDelete : boolean; list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; limitCount : integer; cset : TFhirValueSetComposeIncludeW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; var notClosed : boolean);
    procedure handleCompose(list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; limitCount : integer; source : TFhirValueSetW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; var notClosed : boolean);

    function passesImports(imports : TFslList<TFHIRImportedValueSet>; system, code : String; offset : integer) : boolean;
    function passesImport(import : TFHIRImportedValueSet; system, code : String) : boolean;

    procedure processCode(doDelete : boolean; limitCount : integer; list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; system, version, code : String; display : TCodeDisplays; definition: string; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>);
    procedure addDefinedCode(cs : TFhirCodeSystemW; list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; limitCount : integer; system : string; c : TFhirCodeSystemConceptW; imports : TFslList<TFHIRImportedValueSet>);
    function key(system, code : String): string; overload;
    function key(c : TFhirValueSetExpansionContainsW) : string;  overload;
    function expandValueSet(uri, filter: String; dependencies: TStringList; var notClosed: boolean): TFHIRValueSetW;
    function canonical(system, version: String): String;
    procedure checkSource(cset: TFhirValueSetComposeIncludeW; limitCount : integer; filter : TSearchFilterText);
    procedure checkCanExpandValueset(uri: String);
    function isValidLang(lang: String): boolean;
  public
    constructor Create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; getExpansion : TGetExpansionEvent); overload;

    function expand(source : TFHIRValueSetW; params : TFHIRExpansionParams; textFilter : String; dependencies : TStringList; limit, count, offset : integer) : TFHIRValueSetW;
  end;

const
   CODES_TFhirExpansionParamsFixedVersionMode : array [TFhirExpansionParamsFixedVersionMode] of String = ('Default', 'Check', 'Override');

implementation

{ TSpecialProviderFilterContextConcepts }

constructor TSpecialProviderFilterContextConcepts.Create;
begin
  inherited;
  FList := TFslList<TCodeSystemProviderContext>.create;
end;

destructor TSpecialProviderFilterContextConcepts.Destroy;
begin
  FList.free;
  inherited;
end;

procedure TSpecialProviderFilterContextConcepts.add(c: TCodeSystemProviderContext);
begin
  FList.Add(c);
end;

{ TValueSetWorker }

constructor TValueSetWorker.Create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions);
begin
  Create;
  FFactory := factory;
  FOnGetValueSet := getVS;
  FOnGetCSProvider := getCS;
  FOnListCodeSystemVersions := getVersions;
  FAdditionalResources := txResources;
  FLanguages := languages;
end;

destructor TValueSetWorker.Destroy;
begin
  FLanguages.Free;
  FAdditionalResources.Free;
  FFactory.Free;
  FParams.Free;
  inherited;
end;

function TValueSetWorker.findCodeSystem(url, version: String; params: TFHIRExpansionParams; nullOk: boolean): TCodeSystemProvider;
var
  r : TFHIRMetadataResourceW;
  cs : TFhirCodeSystemW;
begin
  if (url = '') then
    exit(nil);

//  result := nil;

  if FAdditionalResources <> nil then
  begin
    for r in FAdditionalResources do
      if (url <> '') and ((r.url = url) or (r.vurl = url)) then
      begin
        if not (r is TFhirCodeSystemW) then
          raise EFHIRException.Create('Attempt to reference '+url+' as a CodeSystem when it''s a '+r.fhirType);
        cs := r as TFhirCodeSystemW;
        if (cs.content = cscmComplete) then
        begin
          exit(TFhirCodeSystemProvider.Create(FLanguages.link, FFactory.link, TFHIRCodeSystemEntry.Create(cs.link)));
        end;
      end;
  end;
  result := FOnGetCSProvider(self, url, version, FParams, true);

  if (result <> nil) then
    exit(result);

  if FAdditionalResources <> nil then
  begin
    for r in FAdditionalResources do
      if (url <> '') and ((r.url = url) or (r.vurl = url)) then
      begin
        if not (r is TFhirCodeSystemW) then
          raise EFHIRException.Create('Attempt to reference '+url+' as a CodeSystem when it''s a '+r.fhirType);
        cs := r as TFhirCodeSystemW;
        if (cs.content = cscmFragment) then
          exit(TFhirCodeSystemProvider.Create(FLanguages.link, FFactory.link, TFHIRCodeSystemEntry.Create(cs.link)));
      end;
  end;


  if not nullok then
    raise ETerminologySetup.create('Unable to provide support for code system '+url);
end;

function TValueSetWorker.findValueSet(url: String): TFHIRValueSetW;
var
  r : TFHIRMetadataResourceW;
begin
  if (url = '') then
    exit(nil);

  if FAdditionalResources <> nil then
  begin
    for r in FAdditionalResources do
      if (url <> '') and ((r.url = url) or (r.vurl = url)) then
      begin
        if not (r is TFHIRValueSetW) then
          raise EFHIRException.Create('Attempt to reference '+url+' as a ValueSet when it''s a '+r.fhirType);
        exit(r.link as TFHIRValueSetW);
      end;
  end;
  result := FOnGetValueSet(self, url);
end;

function TValueSetWorker.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FFactory.sizeInBytes(magic));
  inc(result, FParams.sizeInBytes(magic));
  inc(result, FAdditionalResources.sizeInBytes(magic));
end;

{ TValueSetChecker }

constructor TValueSetChecker.create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; getVersions : TGetSystemVersionsEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; id : string);
begin
  inherited Create(factory, getVs, getCs, getVersions, txResources, languages);
  FId := id;
  FOthers := TFslStringObjectMatch.create;
  FOthers.PreventDuplicates;
  FOthers.DefaultValue := nil;
  FOthers.Forced := true;
end;

destructor TValueSetChecker.destroy;
begin
  FValueSet.Free;
  FOthers.Free;
  inherited;
end;

function TValueSetChecker.determineSystem(code: String): String;
var
  vsi : TFhirValueSetComposeIncludeW;
  cs : TCodeSystemProvider;
  cc : TFhirValueSetComposeIncludeConceptW;
  match : boolean;
  msg : String;
  loc : TCodeSystemProviderContext;
begin
  result := '';
  for vsi in FValueSet.excludes.forEnum do
    exit('');

  for vsi in FValueSet.includes do
  begin
    if length(vsi.valueSets) > 0 then
      exit('');
    if vsi.systemUri = '' then
      exit('');
    if vsi.hasFilters then
      exit('');
    cs := findCodeSystem(vsi.systemUri, '', nil, true);
    if (cs = nil) then
      exit('');
    try
      if (vsi.hasConcepts) then
      begin
        for cc in vsi.concepts.forEnum do
        begin
          // if cs.casesensitive then
          match := cc.code = code;
          if (match) then
          begin
            if (result = '') then
              result := vsi.systemUri
            else if (result <> vsi.systemUri) then
              exit('');
          end;
        end;
      end
      else
      begin
        loc := cs.locate(code, msg);
        if loc <> nil then
        begin
          cs.Close(loc);
          if (result = '') then
            result := vsi.systemUri
          else if (result <> vsi.systemUri) then
            exit('');
        end;
      end;
    finally
      cs.Free;
    end;
  end;
end;

procedure TValueSetChecker.prepare(vs: TFHIRValueSetW; params : TFHIRExpansionParams);
var
  cc : TFhirValueSetComposeIncludeW;
  other : TFHIRValueSetW;
  checker : TValueSetChecker;
  ics : TFHIRValueSetCodeSystemW;
  s : String;
begin
  FParams := params.Link;

  vs.checkNoImplicitRules('ValueSetChecker.prepare', 'ValueSet');
  FFactory.checkNoModifiers(vs, 'ValueSetChecker.prepare', 'ValueSet');
  if (vs = nil) then
    raise EFslException.Create('Error Error: vs = nil')
  else
  begin
    FValueSet := vs.link;

    // r2:
    ics := FValueSet.inlineCS;
    if ics <> nil then
    begin
      try
        FFactory.checkNoModifiers(ics, 'ValueSetChecker.prepare', 'CodeSystem');
        FOthers.Add(ics.systemUri, TFhirCodeSystemProvider.create(FLanguages.link, ffactory.link, TFHIRCodeSystemEntry.Create(FFactory.wrapCodeSystem(FValueSet.Resource.Link))));
      finally
        ics.Free;
      end;
    end;

    if (FValueSet.checkCompose('ValueSetChecker.prepare', 'ValueSet.compose')) then
    begin
      // not r2:
      for s in FValueSet.imports do
      begin
        other := findValueSet(s);
        try
          if other = nil then
            raise ETerminologyError.create('Unable to find value set '+s);
          checker := TValueSetChecker.create(FFactory.link, FOnGetValueSet, FOnGetCSProvider, FOnListCodeSystemVersions, FAdditionalResources.link, FLanguages.link, other.url);
          try
            checker.prepare(other, params);
            FOthers.Add(s, checker.Link);
          finally
            checker.free;
          end;
        finally
          other.free;
        end;
      end;

      for cc in FValueSet.includes.forEnum do
        prepareConceptSet('include', cc);
      for cc in FValueSet.excludes.forEnum do
        prepareConceptSet('exclude', cc);
    end;
  end;
end;

procedure TValueSetChecker.prepareConceptSet(desc: string; cc: TFhirValueSetComposeIncludeW);
var
  other: TFhirValueSetW;
  checker: TValueSetChecker;
  s : string;
  ccf: TFhirValueSetComposeIncludeFilterW;
  cs: TCodeSystemProvider;
begin
  FFactory.checkNoModifiers(cc, 'ValueSetChecker.prepare', desc);
  for s in cc.valueSets do
  begin
    if not FOthers.ExistsByKey(s) then
    begin
      other := findValueSet(s);
      try
        if other = nil then
          raise ETerminologyError.create('Unable to find value set ' + s);
        checker := TValueSetChecker.create(FFactory.link, FOnGetValueSet, FOnGetCSProvider, FOnListCodeSystemVersions, FAdditionalResources.link, FLanguages.link, other.url);
        try
          checker.prepare(other, FParams);
          FOthers.Add(s, checker.Link);
        finally
          checker.free;
        end;
      finally
        other.free;
      end;
    end;
  end;
  if not FOthers.ExistsByKey(cc.systemUri) then
    FOthers.Add(cc.systemUri, findCodeSystem(cc.systemUri, cc.version, FParams, true));
  cs := FOthers.matches[cc.systemUri] as TCodeSystemProvider;
  if cs <> nil then
  begin
    for ccf in cc.filters.forEnum do
    begin
      FFactory.checkNoModifiers(ccf, 'ValueSetChecker.prepare', desc + '.filter');
      if not (('concept' = ccf.prop) and (ccf.Op in [foIsA, foDescendentOf])) then
        if not cs.doesFilter(ccf.prop, ccf.Op, ccf.value) then
          raise ETerminologyError.create('The filter "' + ccf.prop + ' ' + CODES_TFhirFilterOperator[ccf.Op] + ' ' + ccf.value + '" was not understood in the context of ' + cs.systemUri(nil));
    end;
  end;
end;

function TValueSetChecker.findCode(cs : TFhirCodeSystemW; code: String; list : TFhirCodeSystemConceptListW; displays : TCodeDisplays; out isabstract : boolean): boolean;
var
  i : integer;
  ccl : TFhirCodeSystemConceptListW;
begin
  result := false;
  for i := 0 to list.count - 1 do
  begin
    if (code = list[i].code) then
    begin
      result := true;
      if cs = nil then
        isAbstract := false
      else
        isAbstract := cs.isAbstract(list[i]);
      displays.see(cs.language, list[i].display);
      exit;
    end;
    ccl := list[i].conceptList;
    if findCode(cs, code, ccl, displays, isabstract) then
    begin
      result := true;
      exit;
    end;
  end;
end;

function TValueSetChecker.getName: String;
begin
  if (FValueSet <> nil) then
    result := FValueSet.name
  else
    result := '??';
end;

function TValueSetChecker.check(system, version, code: String; abstractOk, implySystem : boolean; op : TFhirOperationOutcomeW): boolean;
var
  list : TCodeDisplays;
  msg, ver : string;
  it : TFhirIssueType;
begin
  list := TCodeDisplays.Create;
  try
    result := check(system, version, code, abstractOk, implySystem, list, msg, ver, it, op);
  finally
    list.Free;
  end;
end;

function TValueSetChecker.check(system, version, code : String; abstractOk, implySystem : boolean; displays : TCodeDisplays; var message, ver : String; var cause : TFhirIssueType; op : TFhirOperationOutcomeW) : boolean;
var
  cs : TCodeSystemProvider;
  ctxt : TCodeSystemProviderContext;
  cc : TFhirValueSetComposeIncludeW;
  excluded : boolean;
  isabstract : boolean;
  checker : TValueSetChecker;
  s : String;
  ics : TFHIRValueSetCodeSystemW;
  ccl : TFhirCodeSystemConceptListW;
begin
//  result := false;
  FLog := '';
  {special case:}
  s := FValueSet.url;
  if (s = ANY_CODE_VS) then
  begin
    cs := findCodeSystem(system, version, FParams, true);
    try
      if cs = nil then
      begin
        result := false;
        cause := itNotFound;
        FLog := 'Unknown code system';
//        !!!! check if version is valid format then

      end
      else
      begin
        ctxt := cs.locate(code, message);
        if (ctxt = nil) then
        begin
          if cs.isIncomplete then
          begin
            result := true; // we can't say it isn't valid. Need a third status?
            cause := itNotFound;
            FLog := 'Not found in Incomplete Code System';
          end
          else
          begin
            result := false;
            cause := itCodeInvalid;
            FLog := 'Unknown code';
          end;
        end
        else
        begin
          ver := cs.version(nil);
          cause := itNull;
          try
            result := (abstractOk or not cs.IsAbstract(ctxt)) and ((FParams = nil) or not FParams.activeOnly or not cs.isInactive(ctxt));
            if (not result) then
            begin
              FLog := 'Inactive/abstract code';
              cause := itBusinessRule;
            end
            else
              FLog := 'found';
            listDisplays(displays, cs, ctxt);
          finally
            cs.Close(ctxt);
          end;
        end;
      end;
    finally
      cs.Free;
    end;
  end
  else if (FParams.valueSetMode = vsvmNoMembership) then
  begin
    // anyhow, we ignore the value set (at least for now)
    cs := findCodeSystem(system, version, FParams, true);
    try
      if cs = nil then
      begin
        result := false;
        cause := itNotFound;
        FLog := 'Unknown code system';
      end
      else
      begin
        ctxt := cs.locate(code);
        if (ctxt = nil) then
        begin
          result := false;
          cause := itCodeInvalid;
          FLog := 'Unknown code';
        end
        else
        begin
          ver := cs.version(nil);
          cause := itNull;
          try
            result := (abstractOk or not cs.IsAbstract(ctxt)) and ((FParams = nil) or not FParams.activeOnly or not cs.isInactive(ctxt));
            if (not result) then
            begin
              cause := itBusinessRule;
              FLog := 'Inactive/abstract code';
            end
            else
              FLog := 'found';
            listDisplays(displays, cs, ctxt);
          finally
            cs.Close(ctxt);
          end;
        end;
      end;
    finally
      cs.Free;
    end;
  end
  else
  begin
    if (system = '') and implySystem then
      system := determineSystem(code);

    ics := FValueSet.inlineCS; // r2
    if ics <> nil then
    begin
      try
        ver := FValueSet.version;
        if (system = ics.systemUri) or (system = SYSTEM_NOT_APPLICABLE) then
        begin
          ccl := ics.concepts;
          try
            result := FindCode(nil, code, ccl, displays, isabstract);
            if result then
            begin
              result := abstractOk or not isabstract;
              exit;
            end;
          finally
            ccl.Free;
          end;
        end;
      finally
        ics.free;
      end;
    end;

    if (FValueSet.checkCompose('ValueSetChecker.prepare', 'ValueSet.compose')) then
    begin
      result := false;
      for s in FValueSet.imports do
      begin
        if not result then
        begin
          checker := TValueSetChecker(FOthers.matches[s]);
          result := checker.check(system, version, code, abstractOk, implySystem, displays, message, ver, cause, op);
        end;
      end;
      for cc in FValueSet.includes.forEnum do
      begin
        if cc.systemUri = '' then
          result := true
        else if (cc.systemUri = system) or (system = SYSTEM_NOT_APPLICABLE) then
        begin
          cs := TCodeSystemProvider(FOthers.matches[cc.systemUri]);
          if (cs = nil) then
          begin
            message := 'The code system "'+cc.systemUri+'" in the include in "'+FValueSet.url+'" is not known';
            FLog := 'Unknown code system';
            exit(false);
          end;

          if cc.hasExtension('http://hl7.org/fhir/StructureDefinition/valueset-supplement') then
          begin
            s := cc.getExtensionString('http://hl7.org/fhir/StructureDefinition/valueset-supplement');
            if not cs.hasSupplement(s) then
              raise ETerminologyError.create('Value Set Validation depends on supplement '+s+' on '+cs.systemUri(nil)+' that is not known');
          end;

          result := ((system = SYSTEM_NOT_APPLICABLE) or (cs.systemUri(nil) = system)) and checkConceptSet(cs, cc, code, abstractOk, displays, message);
        end
        else
          result := false;
        for s in cc.valueSets do
        begin
          checker := TValueSetChecker(FOthers.matches[s]);
          if checker <> nil then
            result := result and checker.check(system, version, code, abstractOk, implySystem, displays, message, ver, cause, op)
          else
            raise ETerminologyError.Create('No Match for '+s);
        end;
        if result then
          break;
      end;
      if result then
        for cc in FValueSet.excludes.forEnum do
        begin
          if cc.systemUri = '' then
            excluded := true
          else
          begin
            cs := TCodeSystemProvider(FOthers.matches[cc.systemUri]);
            if cc.hasExtension('http://hl7.org/fhir/StructureDefinition/valueset-supplement') then
            begin
              s := cc.getExtensionString('http://hl7.org/fhir/StructureDefinition/valueset-supplement');
              if not cs.hasSupplement(s) then
                raise ETerminologyError.create('Value Set Validation depends on supplement '+s+' on '+cs.systemUri(nil)+' that is not known');
            end;
            excluded := ((system = SYSTEM_NOT_APPLICABLE) or (cs.systemUri(nil) = system)) and checkConceptSet(cs, cc, code, abstractOk, displays, message);
          end;
          for s in cc.valueSets do
          begin
            checker := TValueSetChecker(FOthers.matches[s]);
            excluded := excluded and checker.check(system, version, code, abstractOk, implySystem, displays, message, ver, cause, op);
          end;
          if excluded then
            exit(false);
        end;
    end
    else
      result := true;
  end;
end;


function TValueSetChecker.check(coding: TFhirCodingW; abstractOk, implySystem : boolean) : TFhirParametersW;
var
  list : TCodeDisplays;
  message, ver : String;
  cause : TFhirIssueType;
  op : TFhirOperationOutcomeW;
begin
  result := FFactory.makeParameters;
  try
    op := FFactory.wrapOperationOutcome(FFactory.makeResource('OperationOutcome'));
    try
      list := TCodeDisplays.Create;
      try
        if check(coding.systemUri, coding.version, coding.code, abstractOk, implySystem, list, message, ver, cause, op) then
        begin
          result.AddParamBool('result', true);
          if (coding.display <> '') and (not list.has(coding.display)) then
            result.AddParamStr('message', 'The display "'+coding.display+'" is not a valid display for the code '+coding.code+' - should be one of ['+list.present+']');
          if list.Count > 0 then
            result.AddParamStr('display', list.preferred);
          result.addParamStr('system', coding.systemUri);
          if (ver <> '') then
            result.addParamStr('version', ver);
          result.addParamStr('code', coding.code);
          if cause <> itNull then
            result.AddParamStr('cause', CODES_TFhirIssueType[cause]);
        end
        else
        begin
          result.AddParamBool('result', false);
          if (ver <> '') then
            result.addParamStr('version', ver);
          result.AddParamStr('message', 'The system/code "'+coding.systemUri+'"/"'+coding.code+'" is not in the value set '+FValueSet.name);
          if (message <> '') then
            result.AddParamStr('message', message);
          if cause <> itNull then
            result.AddParamStr('cause', CODES_TFhirIssueType[cause]);
        end;
      finally
        list.Free;
      end;
      if (op.hasIssues) then
        result.addParam('issues').resource := op.Resource.link;
    finally
      op.Free;
    end;
    result.Link;
  finally
    result.free;
  end;
end;

function hasMessage(params : TFhirParametersW; msg : String) : boolean;
var
  p : TFhirParametersParameterW;
begin
  result := false;
  for p in params.parameterList do
    if (p.name = 'message') and (p.value.primitiveValue = msg) then
      exit(true);
end;

function TValueSetChecker.check(code: TFhirCodeableConceptW; abstractOk, implySystem : boolean) : TFhirParametersW;
  function Summary(code: TFhirCodeableConceptW) : String;
  begin
    if (code.codingCount = 1) then
      result := 'The code provided ('+code.summary+') is not '
    else
      result := 'None of the codes provided ('+code.summary+') are ';
  end;
var
  list : TCodeDisplays;
  v : boolean;
  ok, first : boolean;
  cc, codelist, message, mt, ver: String;
  prov, prov2 : TCodeSystemProvider;
  ctxt : TCodeSystemProviderContext;
  c : TFhirCodingW;
  cause : TFhirIssueType;
  op : TFhirOperationOutcomeW;
  procedure msg(s : String);
  begin
    if (s = '') then
      exit;
    if mt = '' then
      mt := s
    else if not mt.Contains(s) then
      mt := mt+'; '+s;
  end;
begin
  cause := itNull;
  if FValueSet = nil then
    raise ETerminologyError.create('Error: cannot validate a CodeableConcept without a nominated valueset');
  result := FFactory.makeParameters;
  try
    op := FFactory.wrapOperationOutcome(FFactory.makeResource('OperationOutcome'));
    try
      list := TCodeDisplays.Create;
      try
        ok := false;
        codelist := '';
        mt := '';
        for c in code.codings.forEnum do
        begin
          list.Clear;
          cc := ',{'+c.systemUri+'}'+c.code;
          codelist := codelist + cc;
          v := check(c.systemUri, c.version, c.code, abstractOk, implySystem, list, message, ver, cause, op);
          if not v and (message <> '') then
            msg(message);
          if not v then
            cause := itInvalid;
          ok := ok or v;
          message := '';

          if (v) then
          begin
            if (c.display <> '') and (not list.has(FLanguages, FParams.displayLanguage, c.display)) then
              msg('The display "'+c.display+'" is not a valid display for the code '+cc.substring(1)+' - should be one of ['+list.present+']');
            if list.Count > 0 then
              result.AddParamStr('display', list.chooseDisplay(FLanguages, FParams.displayLanguage));
            result.addParamStr('system', c.systemUri);
            result.addParamStr('code', c.code);
            if (ver <> '') then
              result.addParamStr('version', ver);
          end
          else
          begin
            prov := findCodeSystem(c.systemUri, c.version, FParams, true);
            try
             if (prov = nil) then
             begin
               prov2 := findCodeSystem(c.systemUri, '', FParams, true);
               try
                 if (prov2 = nil) then
                   msg('The code system '''+c.systemUri+''' is not known (encountered paired with code = '''+c.code+''')')
                 else
                   msg('Version '''+c.version+''' of the code system '''+c.systemUri+''' is not known (encountered paired with code = '''+c.code+'''). ValidVersions = ['+listVersions(c.systemUri)+']')
               finally
                 prov2.free;
               end;
               cause := itNotFound;
             end
             else
             begin
               ctxt := prov.locate(c.code, message);
               try
                 if ctxt = nil then
                 begin
                   msg(message);
                   msg('The code "'+c.code+'" is not valid in the system '+c.systemUri);
                   cause := itInvalid;
                 end
                 else
                 begin
                   listDisplays(list, prov, ctxt);
                   if (c.display <> '') and (not list.has(c.display)) then
                     msg('The display "'+c.display+'" is not a valid display for the code '+cc+' - should be one of ['+list.present+']');
                   result.addParamStr('version', prov.version(nil));
                 end;
               finally
                 prov.Close(ctxt);
               end;
             end;
            finally
              prov.Free;
            end;
          end;
        end;
        result.AddParamBool('result', ok);
        if (not ok) then
        begin
          if FValueSet.name = '' then
            msg(Summary(code) +'valid')
          else
            msg(Summary(code) +'valid in the value set '''+FValueSet.name+'''');
          if cause = itNull then
            cause := itUnknown;
        end;
      finally
        list.Free;
      end;
      if mt <> '' then
        result.AddParamStr('message', mt);
      if not (cause in [itNull]) then
        result.addParamStr('cause', CODES_TFhirIssueType[cause]);
      if (op.hasIssues) then
        result.addParam('issues').resource := op.Resource.link;
    finally
      op.free;
    end;
    result.Link;
  finally
    result.free;
  end;
end;

function TValueSetChecker.check(system, version, code: String; implySystem : boolean): TFhirParametersW;
var
  list : TCodeDisplays;
  message, ver : String;
  cause : TFhirIssueType;
  op : TFhirOperationOutcomeW;
begin
  result := FFactory.makeParameters;
  try
    op := FFactory.wrapOperationOutcome(FFactory.makeResource('OperationOutcome'));
    try
      list := TCodeDisplays.Create;
      try
        if check(system, version, code, true, implySystem, list, message, ver, cause, op) then
        begin
          result.AddParamBool('result', true);
          if list.Count > 0 then
            result.AddParamStr('display', list.preferred);
          result.addParamStr('system', system);
          result.addParamStr('code', code);
          if cause <> itNull then
            result.AddParamStr('cause', CODES_TFhirIssueType[cause]);
        end
        else
        begin
          result.AddParamBool('result', false);
          result.AddParamStr('message', 'The system/code "'+system+'"/"'+code+'" is not in the value set '+FValueSet.name);
          if (message <> '') then
            result.AddParamStr('message', message);
          if cause <> itNull then
            result.AddParamStr('cause', CODES_TFhirIssueType[cause]);
        end;
      finally
        list.Free;
      end;
      if (op.hasIssues) then
        result.addParam('issues').resource := op.Resource.link;
    finally
      op.free;
    end;
    result.Link;
  finally
    result.free;
  end;
end;

Function FreeAsBoolean(cs : TCodeSystemProvider; ctxt : TCodeSystemProviderContext) : boolean; overload;
begin
  result := ctxt <> nil;
  if result then
    cs.Close(ctxt);
end;

Function FreeAsBoolean(cs : TCodeSystemProvider; ctxt : TCodeSystemProviderFilterContext) : boolean; overload;
begin
  result := ctxt <> nil;
  if result then
    cs.Close(ctxt);
end;

function TValueSetChecker.checkConceptSet(cs: TCodeSystemProvider; cset : TFhirValueSetComposeIncludeW; code: String; abstractOk : boolean; displays : TCodeDisplays; var message : String): boolean;
var
  i : integer;
  fc : TFhirValueSetComposeIncludeFilterW;
  ctxt : TCodeSystemProviderFilterContext;
  loc :  TCodeSystemProviderContext;
  prep : TCodeSystemProviderFilterPreparationContext;
  filters : Array of TCodeSystemProviderFilterContext;
  msg : String;
  cc : TFhirValueSetComposeIncludeConceptW;
  cfl : TFslList<TFhirValueSetComposeIncludeFilterW>;
begin
  result := false;
  if (not cset.hasConcepts) and (not cset.hasFilters) then
  begin
    loc := cs.locate(code, message);
    try
      result := (loc <> nil) and (abstractOk or not cs.IsAbstract(loc));
      if result then
      begin
        listDisplays(displays, cs, loc);
        exit;
      end;
    finally
      cs.Close(loc);
    end;
  end;

  for cc in cset.concepts.forEnum do
    if (code = cc.code) then
    begin
      loc := cs.locate(code);
      if Loc <> nil then
      begin
        listDisplays(displays, cs, loc);
        listDisplays(displays, cc);
        result := (abstractOk or not cs.IsAbstract(loc));
        cs.close(loc);
        exit;
      end;
    end;

  if cset.hasFilters then
  begin
    cfl := cset.filters;
    try
      SetLength(filters, cfl.count);
      prep := cs.getPrepContext;
      try
        i := 0;
        for fc in cfl do
        begin
          // gg - why? if ('concept' = fc.property_) and (fc.Op = FilterOperatorIsA) then
          filters[i] := cs.filter(false, fc.prop, fc.Op, fc.value, prep);
          inc(i);
        end;
        if cs.prepare(prep) then // all are together, just query the first filter
        begin
          ctxt := filters[0];
          loc := cs.filterLocate(ctxt, code);
          try
            result := (loc <> nil) and (abstractOk or not cs.IsAbstract(loc));
            if result then
              listDisplays(displays, cs, loc);
          finally
            cs.Close(loc);
          end;
        end
        else
        begin
          result := true;
          i := 0;
          for fc in cfl do
          begin
            if ('concept' = fc.prop) and (fc.Op in [foIsA, foDescendentOf]) then
            begin
              loc := cs.locateIsA(code, fc.value, fc.Op = foDescendentOf);
              try
                result := result and (loc <> nil) and (abstractOk or not cs.IsAbstract(loc));
                if loc <> nil then
                  listDisplays(displays, cs, loc);
              finally
                cs.Close(loc);
              end;
            end
            else if ('concept' = fc.prop) and (fc.Op = foIsNotA) then
            begin
              loc := cs.locateIsA(code, fc.value);
              try
                result := (loc = nil);
                if (result) then
                begin
                  loc := cs.locate(code, msg);
                  if loc <> nil then
                    listDisplays(displays, cs, loc);
                end;
              finally
                cs.Close(loc);
              end;
            end
            else
            begin
              ctxt := filters[i];
              loc := cs.filterLocate(ctxt, code, msg);
              try
                if (loc = nil) and (message = '') then
                  message := msg;
                result := result and (loc <> nil) and (abstractOk or not cs.IsAbstract(loc));
                if loc <> nil then
                  listDisplays(displays, cs, loc);
              finally
                cs.Close(loc);
              end;
            end;
            if not result then
              break;
            inc(i);
          end;
        end;
      finally
        for i := 0 to cfl.count - 1 do
          cs.Close(filters[i]);
        cs.Close(prep);
      end;
    finally
      cfl.free;
    end;
  end;
end;

function TValueSetChecker.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FOthers.sizeInBytes(magic));
  inc(result, FValueSet.sizeInBytes(magic));
  inc(result, (FId.length * sizeof(char)) + 12);
end;

{ TFHIRValueSetExpander }


function TFHIRValueSetExpander.Expand(source: TFHIRValueSetW; params : TFHIRExpansionParams; textFilter : String; dependencies : TStringList; limit, count, offset : integer): TFHIRValueSetW;
var
  list : TFslList<TFhirValueSetExpansionContainsW>;
  map : TFslMap<TFhirValueSetExpansionContainsW>;
  i, t, o : integer;
  c : TFhirValueSetExpansionContainsW;
  //e : TFhirExtension;
  filter : TSearchFilterText;
  notClosed : boolean;
  div_, table : TFhirXHtmlNode;
  tr : TFhirXHtmlNode;
  exp: TFHIRValueSetExpansionW;
  ics : TFHIRValueSetCodeSystemW;
  cl : TFhirCodeSystemConceptListW;
  cs2 : TFhirCodeSystemW;
  limitCount : integer;
begin
  source.checkNoImplicitRules('ValueSetExpander.Expand', 'ValueSet');
  FFactory.checkNoModifiers(source, 'ValueSetExpander.Expand', 'ValueSet');

  FParams := params.Link;

  result := Ffactory.wrapValueSet(source.Resource.Clone as TFHIRResourceV);
  if not FParams.includeDefinition then
  begin
    result.clearDefinition;
    div_ := nil;
    table := nil;
  end
  else
  begin
    div_ := FFactory.resetXhtml(result.Resource);
    table := div_.AddTag('table').setAttribute('class', 'grid');
  end;

  if (result.hasExpansion) then
    exit; // just return the expansion

  if (source.url <> '') then
    dependencies.Add(source.url);

  result.language := FParams.displayLanguage.prefLang;
  FLang := result.language;

  filter := TSearchFilterText.create(textFilter);
  map := TFslMap<TFhirValueSetExpansionContainsW>.create('VS.Expander.map');
  list := TFslList<TFhirValueSetExpansionContainsW>.create;
  try
    if filter.null then
      limitCount := UPPER_LIMIT_NO_TEXT
    else
      limitCount := UPPER_LIMIT_TEXT;

    if (limit < limitCount) then
      limitCount := limit;
    FCount := count;
    FOffset := offset;

    exp := result.forceExpansion;
    if source.id <> '' then
      exp.addParam('expansion-source', 'ValueSet/'+source.id)
    else if source.url <> '' then
      exp.addParam('expansion-source', source.url);

    if FParams.limitedExpansion then
      exp.addParam('limitedExpansion', FParams.limitedExpansion);
    if FParams.displayLanguage.header <> '' then
      exp.addParam('displayLanguage', FParams.displayLanguage.header);
    if FParams.includeDesignations then
      exp.addParam('includeDesignations', FParams.includeDesignations);
    if FParams.includeDefinition then
      exp.addParam('includeDefinition', FParams.includeDefinition);
    if FParams.activeOnly then
      exp.addParam('activeOnly', FParams.activeOnly);
    if FParams.excludeNested then
      exp.addParam('excludeNested', FParams.excludeNested);
    if FParams.excludeNotForUI then
      exp.addParam('excludeNotForUI', FParams.excludeNotForUI);
    if FParams.excludePostCoordinated then
      exp.addParam('excludePostCoordinated', FParams.excludePostCoordinated);

    try
      ics := source.inlineCS;
      try
        if (ics <> nil) then
        begin
          FFactory.checkNoModifiers(ics, 'ValueSetExpander.Expand', 'code system');
          cl := ics.concepts;
          try
            cs2 := FFactory.wrapCodeSystem(source.Resource.link);
            try
              handleDefine(cs2, list, map, limitCount, ics, cl, filter, exp, nil);
            finally
              cs2.Free;
            end;
          finally
            cl.Free;
          end;
        end;
      finally
        ics.Free;
      end;
      notClosed := false;
      if (source.checkCompose('ValueSetExpander.Expand', 'compose')) then
        handleCompose(list, map, limitCount, source, filter, dependencies, exp, notClosed);
    except
      on e : ETooCostly do
      begin
        if FParams.limitedExpansion then
        begin
          exp.addExtension('http://hl7.org/fhir/StructureDefinition/valueset-toocostly', FFactory.makeBoolean(true));
          if (table <> nil) then
            div_.addTag('p').setAttribute('style', 'color: Maroon').addText(e.message);
        end
        else
        begin
          recordStack(e);
          raise;
        end;
      end;
      on e : Exception do
      begin
        recordStack(e);
        raise;
      end;
    end;
    if notClosed then
    begin
      exp.addExtension('http://hl7.org/fhir/StructureDefinition/valueset-unclosed', FFactory.makeBoolean(true));
      if (table <> nil) then
        div_.addTag('p').setAttribute('style', 'color: Navy').addText('Because of the way that this value set is defined, not all the possible codes can be listed in advance');
    end;

    t := 0;
    o := 0;
    for i := 0 to list.count - 1 do
    begin
      c := list[i];
      if map.containsKey(key(c)) then
      begin
        inc(o);
        if (o >= offset) and ((count = 0) or (t < count)) then
        begin
          inc(t);
          exp.addContains(c);
          if (table <> nil) then
          begin
            tr := table.AddChild('tr');
            tr.AddChild('td').AddText(c.systemUri);
            tr.AddChild('td').AddText(c.code);
            tr.AddChild('td').AddText(c.display);
          end;
        end;
      end;
    end;

    result.link;
  finally
    map.free;
    list.free;
    result.free;
    filter.Free;
  end;
end;

function TFHIRValueSetExpander.key(system, code : String): string;
begin
  result := '{'+system+'}'+code;
end;

function TFHIRValueSetExpander.key(c: TFhirValueSetExpansionContainsW): string;
begin
  result := key(c.systemUri, c.Code);
end;

function TFHIRValueSetExpander.makeFilterForValueSet(cs: TCodeSystemProvider; vs: TFHIRValueSetW): TCodeSystemProviderFilterContext;
var
  inc : TFhirValueSetComposeIncludeW;
  cc : TFhirValueSetComposeIncludeConceptW;
  cf : TFhirValueSetComposeIncludeFilterW;
  message : String;
begin
  result := nil;
  for inc in vs.excludes.forEnum do // no short cuts if there's excludes
    exit;
  for inc in vs.includes.forEnum do
  begin
    if inc.systemUri = '' then
      exit; // no short cuts if there's further value set references
    if inc.systemUri = cs.systemUri(nil) then
    begin
      // ok we have a match. Check we can simplify it
      if inc.hasValueSets then
        exit;
      if inc.hasConcepts and inc.hasFilters then
        exit;
      if inc.filterCount > 1 then
        exit;
      if inc.hasFilters then
      begin
        for cf in inc.filters.forEnum do // will only cycle once
        begin
          exit(cs.filter(false, cf.prop, cf.op, cf.value, nil));
        end;
      end
      else
      begin
        result := TSpecialProviderFilterContextConcepts.create;
        for cc in inc.concepts.forEnum do
          TSpecialProviderFilterContextConcepts(result).add(cs.locate(cc.code, message));
        exit;
      end;
    end;
  end;
  // if we get to here, there's nothing left
  result := TSpecialProviderFilterContextNothing.create;
end;

procedure TFHIRValueSetExpander.handleCompose(list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; limitCount : integer; source: TFhirValueSetW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; var notClosed : boolean);
var
  vs : TFHIRValueSetW;
  s : String;
  c : TFhirValueSetComposeIncludeW;
begin
  for s in source.imports do
  begin
    vs := expandValueSet(s, filter.filter, dependencies, notClosed);
    try
      importValueSet(list, map, vs, expansion, nil, 0);
    finally
      vs.free;
    end;
  end;

  for c in source.includes.forEnum do
    checkSource(c, limitCount, filter);
  for c in source.excludes.forEnum do
    checkSource(c, limitCount, filter);

  for c in source.includes.forEnum do
    processCodes(false, list, map, limitCount, c, filter, dependencies, expansion, notClosed);
  for c in source.excludes.forEnum do
    processCodes(true, list, map, limitCount, c, filter, dependencies, expansion, notClosed);
end;

procedure TFHIRValueSetExpander.handleDefine(cs : TFhirCodeSystemW; list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; limitCount : integer; source : TFhirValueSetCodeSystemW; defines : TFhirCodeSystemConceptListW; filter : TSearchFilterText; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>);
var
  cm : TFhirCodeSystemConceptW;
  v : String;
begin
  if (defines.Count > 0) and (expansion <> nil) and (cs.version <> '') then
  begin
    if FFactory.version = fhirVersionRelease2 then
      v := source.systemUri+FHIR_VERSION_CANONICAL_SPLIT_2+cs.version
    else
      v := source.systemUri+FHIR_VERSION_CANONICAL_SPLIT_3p+cs.version;
    if not expansion.hasParam('version', v) then
      expansion.addParam('version', v);
  end;
  for cm in defines do
  begin
    FFactory.checkNoModifiers(cm, 'ValueSetExpander.handleDefine', 'concept');
    if filter.passes(cm.display) or filter.passes(cm.code) then
      addDefinedCode(cs, list, map, limitCount, source.systemUri, cm, imports);
    handleDefine(cs, list, map, limitCount, source, cm.conceptList, filter, nil, imports);
  end;
end;

procedure TValueSetWorker.listDisplays(displays : TCodeDisplays; cs : TCodeSystemProvider; c: TCodeSystemProviderContext);
begin
  // list all known language displays
  cs.Displays(c, displays);
end;

procedure TValueSetWorker.listDisplays(displays : TCodeDisplays; c: TFhirCodeSystemConceptW); // todo: supplements
var
  ccd : TFhirCodeSystemConceptDesignationW;
begin
  // list all known provided displays
  for ccd in c.designations.forEnum do
    displays.see(ccd.language, ccd.value);
end;

procedure TValueSetWorker.listDisplays(displays : TCodeDisplays; c: TFhirValueSetComposeIncludeConceptW);
var
  cd : TFhirValueSetComposeIncludeConceptDesignationW;
  first : boolean;
begin
  first := true;
  for cd in c.designations.forEnum do
  begin
    // see https://chat.fhir.org/#narrow/stream/179202-terminology/topic/ValueSet.20designations.20and.20languages
    if first then
    begin
      displays.Clear;
      first := false;
    end;
    displays.see(cd.language, cd.value, true);
  end;
end;

function TValueSetWorker.listVersions(url: String): String;
var
  ts : TStringList;
begin
  ts := TStringList.Create;
  try
    ts.Sorted := true;
    ts.Duplicates := Classes.dupIgnore;
    FOnListCodeSystemVersions(self, url, ts);
    result := ts.CommaText;
  finally
    ts.Free;
  end;
end;

constructor TFHIRValueSetExpander.Create(factory: TFHIRFactory; getVS: TGetValueSetEvent; getCS: TGetProviderEvent; getVersions : TGetSystemVersionsEvent; txResources : TFslMetadataResourceList; languages : TIETFLanguageDefinitions; getExpansion: TGetExpansionEvent);
begin
  inherited create(factory, getVS, getCS, getVersions, txResources, languages);
  FOnGetExpansion := getExpansion;
end;

procedure TFHIRValueSetExpander.addDefinedCode(cs : TFhirCodeSystemW; list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; limitCount : integer; system: string; c: TFhirCodeSystemConceptW; imports : TFslList<TFHIRImportedValueSet>);
var
  i : integer;
  cds : TCodeDisplays;
begin
  if not FParams.excludeNotForUI or not (cs.isAbstract(c)) then
  begin
    cds := TCodeDisplays.Create;
    try
      listDisplays(cds, c);
      processCode(false, limitCount, list, map, system, '', c.Code, cds, c.definition, nil, imports);
    finally
      cds.Free;
    end;
  end;
  for i := 0 to c.conceptList.count - 1 do
    addDefinedCode(cs, list, map, limitCount, system, c.conceptList[i], imports);
end;

function TFHIRValueSetExpander.canonical(system, version : String) : String;
begin
  if FFactory.version = fhirVersionRelease2 then
    result := system + '?version='+version
  else
    result := system + '|'+version
end;

function TFHIRValueSetExpander.isValidLang(lang : String) : boolean;
var
  s : String;
  l, r : TIETFLang;
begin
  result := true;
  if (lang = '') then
    exit(true);

  l := FLanguages.parse(lang);
  try
    for s in FParams.displayLanguage.codes do
    begin
      result := false;
      if (s = '') then
        exit(true);
      r := FLanguages.parse(s);
      try
        if l.matches(r) then
          exit(true);
      finally
        r.Free;
      end;
    end;
  finally
    l.Free;
  end;
end;


function TFHIRValueSetExpander.passesImport(import: TFHIRImportedValueSet; system, code: String): boolean;
begin
  import.buildMap;
  result := import.hasCode(system, code);
end;

function TFHIRValueSetExpander.passesImports(imports: TFslList<TFHIRImportedValueSet>; system, code: String; offset: integer): boolean;
var
  i : integer;
begin
  result := true;
  if imports <> nil then
  begin
    for i := offset to imports.Count - 1 do
      if not passesImport(imports[i], system, code) then
        exit(false);
  end;
end;

procedure TFHIRValueSetExpander.processCode(doDelete : boolean; limitCount : integer; list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; system, version, code : String; display : TCodeDisplays; definition: string; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>);
var
  n : TFhirValueSetExpansionContainsW;
  s : String;
  cd : TCodeDisplay;
begin
  if not passesImports(imports, system, code, 0) then
    exit;

  if (limitCount > 0) and (map.Count >= limitCount) and not doDelete then
    raise ETooCostly.create('Too many codes to display (>'+inttostr(limitCount)+') (A text filter may reduce the number of codes in the expansion)');

  if (expansion <> nil) and (version <> '') then
  begin
    s := canonical(system, version);
    if not expansion.hasParam('version', s) then
      expansion.addParam('version', s);
  end;

  s := key(system, code);

  if doDelete or not map.containsKey(s) then
  begin
    n := FFactory.makeValueSetContains;
    try
      n.systemUri := system;
      n.Code := code;
      n.Display := display.chooseDisplay(FLanguages, FParams.FdisplayLanguage);
      if FParams.includeDesignations then
      begin
        for cd in display do
        begin
          if ((cd.language <> FLang) or (cd.value <> n.display)) and isValidLang(cd.language) then
            n.addDesignation(cd.language, '', cd.value);
        end;
      end;

      if (dodelete) then
      begin
        if map.ContainsKey(s) then
        begin
          list.Remove(map[s]);
          map.Remove(s);
        end;
      end
      else if not map.ContainsKey(s) then
      begin
        list.add(n.link);
        map.add(s, n.link);
      end;
      if definition <> '' then
        n.addExtension('http://hl7.org/fhir/StructureDefinition/valueset-definition', FFactory.makeString(definition));
    finally
      n.free;
    end;
  end;
end;

procedure TFHIRValueSetExpander.checkCanExpandValueset(uri: String);
var
  vs : TFHIRValueSetW;
begin
  vs := findValueSet(uri);
  try
    if vs = nil then
      raise ETerminologyError.create('Unable to find value set "'+uri+'"');
  finally
    vs.Free;
  end;
end;

function TFHIRValueSetExpander.expandValueSet(uri: String; filter : String; dependencies : TStringList;  var notClosed : boolean) : TFHIRValueSetW;
var
  dep : TStringList;
  exp : TFhirValueSetExpansionW;
begin
  dep := TStringList.Create;
  try
    result := FOnGetExpansion(self, uri, filter, FParams, dep, FAdditionalResources , -1);
    try
      dependencies.AddStrings(dep);
      if (result = nil) then
        raise ETerminologyError.create('unable to find value set '+uri);
      if result.expansion.hasextension('http://hl7.org/fhir/params/questionnaire-extensions#closed') then
        notClosed := true;
      result.Link;
    finally
      result.free;
    end;
  finally
    dep.Free;
  end;
end;

procedure TFHIRValueSetExpander.importValueSet(list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; vs : TFHIRValueSetW; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; offset : integer);
var
  c : TFhirValueSetExpansionContainsW;
  s : String;
begin
  expansion.copyParams(vs.expansion);

  for c in vs.expansion.contains.forEnum do
  begin
    s := key(c);
    if passesImports(imports, c.systemUri, c.code, offset) and not map.containsKey(s) then
    begin
      list.add(c.link);
      map.add(s, c.link);
    end;
  end;
end;

procedure TFHIRValueSetExpander.excludeValueSet(list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; vs : TFHIRValueSetW; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>; offset : integer);
var
  c : TFhirValueSetExpansionContainsW;
  s : String;
begin
  for c in vs.expansion.contains.forEnum do
  begin
    s := key(c);
    if passesImports(imports, c.systemUri, c.code, offset) and map.containsKey(s) then
    begin
      list.Remove(map[s]);
      map.Remove(s);
    end;
  end;
end;

procedure TFHIRValueSetExpander.checkSource(cset: TFhirValueSetComposeIncludeW; limitCount : integer; filter : TSearchFilterText);
var
  cs : TCodeSystemProvider;
  s : string;
  imp : boolean;
begin
  FFactory.checkNoModifiers(cset,'ValueSetExpander.processCodes', 'set');
  imp := false;
  for s in cset.valueSets do
  begin
    checkCanExpandValueset(s);
    imp := true;
  end;

  if cset.systemUri <> '' then
  begin
    cs := findCodeSystem(cset.systemUri, cset.version, FParams, false);
    try
      if cset.hasExtension('http://hl7.org/fhir/StructureDefinition/valueset-supplement') then
        begin
          s := cset.getExtensionString('http://hl7.org/fhir/StructureDefinition/valueset-supplement');
          if not cs.hasSupplement(s) then
            raise ETerminologyError.create('Expansion depends on supplement '+s+' on '+cs.systemUri(nil)+' that is not known');
        end;

      if (not cset.hasConcepts) and (not cset.hasFilters) then
      begin
        if (cs.SpecialEnumeration <> '') and FParams.limitedExpansion then
        begin
          checkCanExpandValueSet(cs.SpecialEnumeration);
        end
        else if filter.Null then // special case - add all the code system
        begin
          if cs.isNotClosed(filter) then
            if cs.SpecialEnumeration <> '' then
              raise ETooCostly.create('The code System "'+cs.systemUri(nil)+'" has a grammar, and cannot be enumerated directly. If an incomplete expansion is requested, a limited enumeration will be returned')
            else  if (cs.systemUri(nil) = ALL_CODE_CS) then
              raise ETooCostly.create('Cannot filter across all code Systems known to the server')
            else
              raise ETooCostly.create('The code System "'+cs.systemUri(nil)+'" has a grammar, and cannot be enumerated directly');

          if not imp and (limitCount > 0) and (cs.TotalCount > limitCount) and not (FParams.limitedExpansion) then
            raise ETooCostly.create('Too many codes to display (>'+inttostr(limitCount)+') (A text filter may reduce the number of codes in the expansion)');
        end
      end;

    finally
      cs.free;
    end;
  end;
end;

procedure TFHIRValueSetExpander.processCodes(doDelete : boolean; list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; limitCount : integer; cset: TFhirValueSetComposeIncludeW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; var notClosed : boolean);
var
  cs : TCodeSystemProvider;
  i, count, offset : integer;
  fc : TFhirValueSetComposeIncludeFilterW;
  fcl : TFslList<TFhirValueSetComposeIncludeFilterW>;
  c : TCodeSystemProviderContext;
  filters : TFslList<TCodeSystemProviderFilterContext>;
  f : TCodeSystemProviderFilterContext;
  ctxt : TCodeSystemProviderFilterContext;
  ok : boolean;
  prep : TCodeSystemProviderFilterPreparationContext;
  inner : boolean;
  s, display : String;
  valueSets : TFslList<TFHIRImportedValueSet>;
  base : TFHIRValueSetW;
  cc : TFhirValueSetComposeIncludeConceptW;
  cctxt : TCodeSystemProviderContext;
  cds : TCodeDisplays;
  iter : TCodeSystemIteratorContext;
  vs : TFHIRValueSetW;
  function passesFilters(c : TCodeSystemProviderContext; offset : integer) : boolean;
  var
    j : integer;
    ok : boolean;
    t : TCodeSystemProviderContext;
  begin
    result := true;
    for j := offset to filters.Count - 1 do
    begin
      f := filters[j];
      if f is TSpecialProviderFilterContextNothing then
        result := false
      else if f is TSpecialProviderFilterContextConcepts then
      begin
        ok := false;
        for t in (f as TSpecialProviderFilterContextConcepts).FList do
          if cs.sameContext(t, c) then
            ok := true;
        result := result and ok;
      end
      else
        result := result and cs.InFilter(f, c);
    end;
  end;
begin
  valueSets := TFslList<TFHIRImportedValueSet>.create;
  try
    FFactory.checkNoModifiers(cset,'ValueSetExpander.processCodes', 'set');

    if cset.systemUri = '' then
    begin
      for s in cset.valueSets do
        valueSets.add(TFHIRImportedValueSet.create(expandValueset(s, filter.filter, dependencies, notClosed)));
      if doDelete then
        excludeValueSet(list, map, valueSets[0].valueSet, expansion, valueSets, 1)
      else
        importValueSet(list, map, valueSets[0].valueSet, expansion, valueSets, 1);
    end
    else
    begin
      filters := TFslList<TCodeSystemProviderFilterContext>.create;
      try
        cs := findCodeSystem(cset.systemUri, cset.version, FParams, false);
        try
          for s in cset.valueSets do
          begin
            f := nil;
            // if we can, we can do a short cut evaluation that means we don't have to do a full expansion of the source value set.
            // this saves lots of overhead we don't need. But it does require simple cases (though they are common). So we have a look
            // at the value set, and see whether we can short cut it. If we can, it's just another filter (though we can't iterate on it)
            vs := FOnGetValueSet(self, s);
            try
              if (vs <> nil) then
                f := makeFilterForValueSet(cs, vs);
              if (f <> nil) then
                filters.add(f)
              else
                valueSets.add(TFHIRImportedValueSet.create(expandValueset(s, filter.filter, dependencies, notClosed)));
            finally
              vs.Free;
            end;
          end;
          if cset.hasExtension('http://hl7.org/fhir/StructureDefinition/valueset-supplement') then
          begin
            s := cset.getExtensionString('http://hl7.org/fhir/StructureDefinition/valueset-supplement');
            if not cs.hasSupplement(s) then
              raise ETerminologyError.create('Expansion depends on supplement '+s+' on '+cs.systemUri(nil)+' that is not known');
          end;

          if (not cset.hasConcepts) and (not cset.hasFilters) then
          begin
            if (cs.SpecialEnumeration <> '') and FParams.limitedExpansion and filters.Empty then
            begin
              base := expandValueSet(cs.SpecialEnumeration, filter.filter, dependencies, notClosed);
              try
                expansion.addExtension('http://hl7.org/fhir/StructureDefinition/valueset-toocostly', FFactory.makeBoolean(true));
                if doDelete then
                  excludeValueSet(list, map, base, expansion, valueSets, 0)
                else
                  importValueSet(list, map, base, expansion, valueSets, 0);
              finally
                base.Free;
              end;
              notClosed := true;
            end
            else if filter.Null then // special case - add all the code system
            begin
              if cs.isNotClosed(filter) then
                if cs.SpecialEnumeration <> '' then
                  raise ETooCostly.create('The code System "'+cs.systemUri(nil)+'" has a grammar, and cannot be enumerated directly. If an incomplete expansion is requested, a limited enumeration will be returned')
                else  if (cs.systemUri(nil) = ALL_CODE_CS) then
                  raise ETooCostly.create('Cannot filter across all code Systems known to the server')
                else
                  raise ETooCostly.create('The code System "'+cs.systemUri(nil)+'" has a grammar, and cannot be enumerated directly');

              iter := cs.getIterator(nil);
              try
                if valueSets.Empty and (limitCount > 0) and (iter.count > limitCount) and not (FParams.limitedExpansion) and not doDelete then
                  raise ETooCostly.create('Too many codes to display (>'+inttostr(limitCount)+') (A text filter may reduce the number of codes in the expansion)');
                while iter.more do
                begin
                  c := cs.getNextContext(iter);
                  if passesFilters(c, 0) then
                    processCodeAndDescendants(doDelete, list, map, limitCount, cs, c, expansion, valueSets);
                end;
              finally
                iter.Free;
              end;
            end
            else
            begin
              if cs.isNotClosed(filter) then
                notClosed := true;
              prep := cs.getPrepContext;
              try
                ctxt := cs.searchFilter(filter, prep, false);
                try
                  cs.prepare(prep);
                  while cs.FilterMore(ctxt) do
                  begin
                    c := cs.FilterConcept(ctxt);
                    if passesFilters(c, 0) then
                    begin
                      cds := TCodeDisplays.Create;
                      try
                        listDisplays(cds, cs, c); // cs.display(c, FParams.displayLanguage)
                        processCode(doDelete, limitCount, list, map, cs.systemUri(c), cs.version(c), cs.code(c), cds, cs.definition(c), expansion, valueSets);
                      finally
                        cds.free;
                      end;
                    end;
                  end;
                finally
                  cs.Close(ctxt);
                end;
              finally
                cs.Close(prep);
              end;
            end;
          end;

          cds := TCodeDisplays.Create;
          try
            for cc in cset.concepts.forEnum do
            begin
              cds.Clear;
              FFactory.checkNoModifiers(cc, 'ValueSetExpander.processCodes', 'set concept reference');
              cctxt := cs.locate(cc.code);
              try
                if (cctxt <> nil) and (not FParams.activeOnly or not cs.IsInactive(cctxt)) and passesFilters(cctxt, 0) then
                begin
                  listDisplays(cds, cs, cctxt);
                  listDisplays(cds, cc);
                  if filter.passes(cds) or filter.passes(cc.code) then
                    processCode(doDelete, limitCount, list, map, cs.systemUri(nil), cs.version(nil), cc.code, cds, cs.Definition(cctxt), expansion, valueSets);
                end;
              finally
                cs.Close(cctxt);
              end;
            end;
          finally
            cds.free;
          end;

          if cset.hasFilters then
          begin
            fcl := cset.filters;
            try
              prep := cs.getPrepContext;
              try
                try
                  offset := 0;
                  if not filter.null then
                  begin
                    filters.Insert(0, cs.searchFilter(filter, prep, true)); // this comes first, because it imposes order
                    inc(offset);
                  end;

                  if cs.specialEnumeration <> '' then
                  begin
                    filters.Insert(offset, cs.specialFilter(prep, true));
                    expansion.addExtension('http://hl7.org/fhir/StructureDefinition/valueset-toocostly', FFactory.makeBoolean(true));
                    notClosed := true;
                  end;
                  for i := 0 to fcl.count - 1 do
                  begin
                    fc := fcl[i];
                    ffactory.checkNoModifiers(fc, 'ValueSetExpander.processCodes', 'filter');
                    f := cs.filter(i = 0, fc.prop, fc.Op, fc.value, prep);
                    if f = nil then
                      raise ETerminologyError.create('The filter "'+fc.prop +' '+ CODES_TFhirFilterOperator[fc.Op]+ ' '+fc.value+'" was not understood in the context of '+cs.systemUri(nil));
                    filters.Insert(offset, f);
                    if cs.isNotClosed(filter, f) then
                      notClosed := true;
                  end;

                  inner := not cs.prepare(prep);
                  count := 0;
                  While cs.FilterMore(filters[0]) and ((FOffset + FCount = 0) or (count < FOffset + FCount)) do
                  begin
                    c := cs.FilterConcept(filters[0]);
                    try
                      ok := (not FParams.activeOnly or not cs.IsInactive(c)) and (inner or passesFilters(c, 1));
                      if ok then
                      begin
                        inc(count);
                        if count > FOffset then
                        begin
                          cds := TCodeDisplays.Create;
                          try
                            if passesImports(valueSets, cs.systemUri(nil), cs.code(c), 0) then
                            begin
                              listDisplays(cds, cs, c);
                              processCode(doDelete, limitCount, list, map, cs.systemUri(nil), cs.version(nil), cs.code(c), cds, cs.definition(c), expansion, nil);
                            end;
                          finally
                            cds.free;
                          end;
                        end;
                      end;
                    finally
                      cs.close(c);
                    end;
                  end;
                finally
                  for f in filters do
                    cs.Close(f.Link);
                end;
              finally
                prep.free;
              end;
            finally
              fcl.Free;
            end;
          end;
        finally
          cs.free;
        end;
      finally
        filters.Free;
      end;
    end;
  finally
    valueSets.Free;
  end;
end;

procedure TFHIRValueSetExpander.processCodeAndDescendants(doDelete : boolean; list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; limitCount : integer; cs: TCodeSystemProvider; context: TCodeSystemProviderContext; expansion : TFhirValueSetExpansionW; imports : TFslList<TFHIRImportedValueSet>);
var
  i : integer;
  vs : String;
  cds : TCodeDisplays;
  iter : TCodeSystemIteratorContext;
begin
  try
    if (cs.version(nil) <> '') and (expansion <> nil) then
    begin
      vs := canonical(cs.systemUri(nil), cs.version(nil));
      if not expansion.hasParam('version', vs) then
        expansion.addParam('version', vs);
    end;
    if not FParams.excludeNotForUI or not cs.IsAbstract(context) then
    begin
      cds := TCodeDisplays.Create;
      try
        listDisplays(cds, cs, context);
        processCode(doDelete, limitCount, list, map, cs.systemUri(context), '', cs.Code(context), cds, cs.definition(context), expansion, imports);
      finally
        cds.Free;
      end;
    end;
    iter := cs.getIterator(context);
    try
      while iter.more do
        processCodeAndDescendants(doDelete, list, map, limitCount, cs, cs.getNextContext(iter), expansion, imports);
    finally
      iter.Free;
    end;
  finally
    cs.Close(context);
  end;
end;

{ TFHIRExpansionParams }

constructor TFHIRExpansionParams.Create;
begin
  inherited;
  FFixedVersions := TFslList<TFhirExpansionParamsFixedVersion>.create;
end;

function TFHIRExpansionParams.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FFixedVersions.sizeInBytes(magic));
  inc(result, FdisplayLanguage.sizeInBytes(magic));
  inc(result, (FUid.length * sizeof(char)) + 12);
end;

class function TFHIRExpansionParams.defaultProfile: TFHIRExpansionParams;
begin
  result := TFHIRExpansionParams.Create;
end;

destructor TFHIRExpansionParams.Destroy;
begin
  FFixedVersions.Free;
  inherited;
end;

function TFHIRExpansionParams.hash: String;
var
  s : String;
  function b(v : boolean):string;
  begin
    if v then
      result := '1'
    else
      result := '0';
  end;
begin
  s := FFixedVersions.ToString +'|' +b(activeOnly)+'|'+displayLanguage.header+ b(includeDefinition) +'|'+   b(limitedExpansion) +'|'+  b(includeDesignations) +'|'+
    b(excludeNested) +'|'+ b(excludeNotForUI) +'|'+ b(excludePostCoordinated) +'|'+uid+'|'+inttostr(ord(FValueSetMode))+'|'+ b(defaultToLatestVersion);
  result := inttostr(HashStringToCode32(s));
end;

function TFHIRExpansionParams.link: TFHIRExpansionParams;
begin
  result := TFHIRExpansionParams(inherited Link);
end;


{ TFhirExpansionParamsFixedVersion }

constructor TFhirExpansionParamsFixedVersion.Create(system, version: String; mode: TFhirExpansionParamsFixedVersionMode);
begin
  inherited Create;
  FSystem := system;
  FVersion := version;
  FMode := mode;
end;

constructor TFhirExpansionParamsFixedVersion.Create(system, version: String);
begin
  inherited Create;
  FSystem := system;
  FVersion := version;
end;

function TFhirExpansionParamsFixedVersion.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (Fsystem.length * sizeof(char)) + 12);
  inc(result, (Fversion.length * sizeof(char)) + 12);
end;

{ TFHIRImportedValueSet }

constructor TFHIRImportedValueSet.Create(valueSet: TFHIRValueSetW);
begin
  inherited Create;
  FValueSet := valueSet;
  FMap := nil;
end;

destructor TFHIRImportedValueSet.Destroy;
begin
  FMap.Free;
  FValueSet.free;
  inherited;
end;

procedure TFHIRImportedValueSet.SetValueSet(const Value: TFHIRValueSetW);
begin
  FValueSet.free;
  FValueSet := Value;
end;

procedure TFHIRImportedValueSet.buildMap;
var
  cc : TFhirValueSetExpansionContainsW;
begin
  if FMap = nil then
  begin
    FMap := TStringList.Create;
    for cc in FValueSet.expansion.contains.forEnum do
      addToMap(cc);
    FMap.Sort;
  end;
end;

procedure TFHIRImportedValueSet.addToMap(c: TFhirValueSetExpansionContainsW);
var
  cc : TFhirValueSetExpansionContainsW;
begin
  if (c.systemUri <> '') and (c.code <> '') then
    FMap.Add(key(c.systemUri, c.code));
  for cc in c.contains.forEnum do
    addToMap(cc);
end;

function TFHIRImportedValueSet.hasCode(system, code: String): boolean;
begin
  result := FMap.IndexOf(key(system, code)) > -1;
end;

function TFHIRImportedValueSet.key(system, code: String): String;
begin
  result := system+#1+code;
end;

end.



