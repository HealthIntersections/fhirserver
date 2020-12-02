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
  fsl_base, fsl_collections, fsl_utilities, fsl_http,
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
    function sizeInBytesV : cardinal; override;
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
  protected
    function sizeInBytesV : cardinal; override;
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

    function hash : String;
  end;

  TGetValueSetEvent = function (sender : TObject; url : String) : TFHIRValueSetW of object;
  TGetProviderEvent = function (sender : TObject; url, version : String; params : TFHIRExpansionParams; nullOk : boolean) : TCodeSystemProvider of object;
  TGetExpansionEvent = function (sender : TObject; url, filter : String; params : TFHIRExpansionParams; dependencies : TStringList; limit : integer) : TFHIRValueSetW of object;
  TCheckCanExpandEvent = procedure (sender : TObject; url : String; params : TFHIRExpansionParams) of object;

  TValueSetWorker = class (TFslObject)
  private
    FFactory : TFHIRFactory;
    FOnGetValueSet : TGetValueSetEvent;
    FOnGetCSProvider : TGetProviderEvent;
    FParams : TFHIRExpansionParams;
    FAdditionalResources : TFslMetadataResourceList;

    function findValueSet(url : String) : TFHIRValueSetW;
    function findCodeSystem(url, version : String; params : TFHIRExpansionParams; nullOk : boolean) : TCodeSystemProvider;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; txResources : TFslMetadataResourceList); overload;
    destructor Destroy; override;
  end;

  TValueSetChecker = class (TValueSetWorker)
  private
    FOthers : TFslStringObjectMatch; // checkers or code system providers
    FValueSet : TFHIRValueSetW;
    FId: String;
    FNoValueSetExpansion : boolean;

    function determineSystem(code : String) : String;
    function check(system, version, code : String; abstractOk, implySystem : boolean; displays : TStringList; var message : String; var cause : TFhirIssueType) : boolean; overload;
    function findCode(cs : TFhirCodeSystemW; code: String; list : TFhirCodeSystemConceptListW; displays : TStringList; out isabstract : boolean): boolean;
    function checkConceptSet(cs: TCodeSystemProvider; cset : TFhirValueSetComposeIncludeW; code : String; abstractOk : boolean; displays : TStringList; var message : String) : boolean;
    procedure prepareConceptSet(desc: string; cc: TFhirValueSetComposeIncludeW);
    function getName: String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; txResources : TFslMetadataResourceList; id : String); overload;
    destructor Destroy; override;

    property id : String read FId;
    property name : String read getName;

    procedure prepare(vs : TFHIRValueSetW; params : TFHIRExpansionParams);

    function check(system, version, code : String; abstractOk, implySystem : boolean) : boolean; overload;
    function check(system, version, code : String; implySystem : boolean) : TFhirParametersW; overload;
    function check(coding : TFhirCodingW; abstractOk, implySystem : boolean): TFhirParametersW; overload;
    function check(code: TFhirCodeableConceptW; abstractOk, implySystem : boolean) : TFhirParametersW; overload;

  end;

  TFHIRValueSetExpander = class (TValueSetWorker)
  private
    FLimit : integer;
    FCount : integer;
    FOffset : integer;
    FOnGetExpansion : TGetExpansionEvent;
    FOnCheckCanExpand : TCheckCanExpandEvent;

    procedure processCodeAndDescendants(doDelete : boolean; list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; cs : TCodeSystemProvider; context : TCodeSystemProviderContext; expansion : TFhirValueSetExpansionW; params : TFHIRExpansionParams; importHash : TStringList);

    procedure handleDefine(cs : TFhirCodeSystemW; list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; source : TFhirValueSetCodeSystemW; defines : TFhirCodeSystemConceptListW; filter : TSearchFilterText; expansion : TFhirValueSetExpansionW; params : TFHIRExpansionParams; importHash : TStringList);
    procedure importValueSet(list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; vs : TFHIRValueSetW; expansion : TFhirValueSetExpansionW; importHash : TStringList);
    procedure processCodes(doDelete : boolean; list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; cset : TFhirValueSetComposeIncludeW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; params : TFHIRExpansionParams; var notClosed : boolean);
    procedure handleCompose(list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; source : TFhirValueSetW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; params : TFHIRExpansionParams; var notClosed : boolean);

    procedure hashImport(hash : TStringList; cc : TFhirValueSetExpansionContainsW);
    function makeImportHash(imports : TFslList<TFHIRValueSetW>; start : integer) : TStringList;
    function passesImportFilter(importHash : TStringList; system, code : string) : boolean;

    procedure processCode(doDelete : boolean; list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; system, version, code, display, definition: string; expansion : TFhirValueSetExpansionW; params : TFHIRExpansionParams; importHash : TStringList);
    procedure addDefinedCode(cs : TFhirCodeSystemW; list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; system : string; c : TFhirCodeSystemConceptW; params : TFHIRExpansionParams; importHash : TStringList);
    function key(system, code, display : String): string; overload;
    function key(c : TFhirValueSetExpansionContainsW) : string;  overload;
    function chooseDisplay(c: TFhirCodeSystemConceptW;  params : TFHIRExpansionParams): String; overload;
    function chooseDisplay(c: TFhirValueSetComposeIncludeConceptW;  params : TFHIRExpansionParams): String; overload;
    function expandValueSet(uri, filter: String; dependencies: TStringList; var notClosed: boolean): TFHIRValueSetW;
    function canonical(system, version: String): String;
    procedure checkSource(cset: TFhirValueSetComposeIncludeW; filter : TSearchFilterText);
    procedure checkCanExpandValueset(uri: String);
  public
    constructor Create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; txResources : TFslMetadataResourceList; getExpansion : TGetExpansionEvent; checkExpand : TCheckCanExpandEvent); overload;

    function expand(source : TFHIRValueSetW; params : TFHIRExpansionParams; textFilter : String; dependencies : TStringList; limit, count, offset : integer) : TFHIRValueSetW;
  end;

const
   CODES_TFhirExpansionParamsFixedVersionMode : array [TFhirExpansionParamsFixedVersionMode] of String = ('Default', 'Check', 'Override');

implementation

{ TValueSetWorker }

constructor TValueSetWorker.Create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; txResources : TFslMetadataResourceList);
begin
  Create;
  FFactory := factory;
  FOnGetValueSet := getVS;
  FOnGetCSProvider := getCS;
  FAdditionalResources := txResources;
end;

destructor TValueSetWorker.Destroy;
begin
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
          exit(TFhirCodeSystemProvider.Create(FFactory.link, TFHIRCodeSystemEntry.Create(cs.link)));
        end;
      end;
  end;
  result := FOnGetCSProvider(self, url, version, params, true);

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
        exit(TFhirCodeSystemProvider.Create(FFactory.link, TFHIRCodeSystemEntry.Create(cs.link)));
      end;
  end;
  result := FOnGetCSProvider(self, url, version, params, true);

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

function TValueSetWorker.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFactory.sizeInBytes);
  inc(result, FParams.sizeInBytes);
  inc(result, FAdditionalResources.sizeInBytes);
end;

{ TValueSetChecker }

constructor TValueSetChecker.create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; txResources : TFslMetadataResourceList; id : string);
begin
  inherited Create(factory, getVs, getCs, txResources);
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
    raise Exception.Create('Error Error: vs = nil')
  else
  begin
    FValueSet := vs.link;

    // r2:
    ics := FValueSet.inlineCS;
    if ics <> nil then
    begin
      try
        FFactory.checkNoModifiers(ics, 'ValueSetChecker.prepare', 'CodeSystem');
        FOthers.Add(ics.systemUri, TFhirCodeSystemProvider.create(ffactory.link, TFHIRCodeSystemEntry.Create(FFactory.wrapCodeSystem(FValueSet.Resource.Link))));
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
          checker := TValueSetChecker.create(FFactory.link, FOnGetValueSet, FOnGetCSProvider, FAdditionalResources.link, other.url);
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
    other := findValueSet(s);
    try
      if other = nil then
        raise ETerminologyError.create('Unable to find value set ' + s);
      checker := TValueSetChecker.create(FFactory.link, FOnGetValueSet, FOnGetCSProvider, FAdditionalResources.link, other.url);
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
  if not FOthers.ExistsByKey(cc.systemUri) then
    FOthers.Add(cc.systemUri, findCodeSystem(cc.systemUri, cc.version, FParams, true));
  cs := TCodeSystemProvider(FOthers.matches[cc.systemUri]);
  FNoValueSetExpansion := cs = nil;
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

function TValueSetChecker.findCode(cs : TFhirCodeSystemW; code: String; list : TFhirCodeSystemConceptListW; displays : TStringList; out isabstract : boolean): boolean;
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
      displays.Add(list[i].display);
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

function TValueSetChecker.check(system, version, code: String; abstractOk, implySystem : boolean): boolean;
var
  list : TStringList;
  msg : string;
  it : TFhirIssueType;
begin
  list := TStringList.Create;
  try
    list.Duplicates := Classes.dupIgnore;
    list.CaseSensitive := false;
    result := check(system, version, code, abstractOk, implySystem, list, msg, it);
  finally
    list.Free;
  end;
end;

function TValueSetChecker.check(system, version, code : String; abstractOk, implySystem : boolean; displays : TStringList; var message : String; var cause : TFhirIssueType) : boolean;
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
      end
      else
      begin
        ctxt := cs.locate(code, message);
        if (ctxt = nil) then
        begin
          result := false;
          cause := itCodeInvalid;
        end
        else
        begin
          cause := itNull;
          try
            result := (abstractOk or not cs.IsAbstract(ctxt)) and ((FParams = nil) or not FParams.activeOnly or not cs.isInactive(ctxt));
            if (not result) then
              cause := itBusinessRule;
            cs.Displays(ctxt, displays, FParams.displayLanguage);
          finally
            cs.Close(ctxt);
          end;
        end;
      end;
    finally
      cs.Free;
    end;
  end
  else if FNoValueSetExpansion or (FParams.valueSetMode = vsvmNoMembership) then
  begin
    // anyhow, we ignore the value set (at least for now)
    cs := findCodeSystem(system, version, FParams, true);
    try
      if cs = nil then
      begin
        result := false;
        cause := itNotFound;
      end
      else
      begin
        ctxt := cs.locate(code);
        if (ctxt = nil) then
        begin
          result := false;
          cause := itCodeInvalid;
        end
        else
        begin
          cause := itNull;
          try
            result := (abstractOk or not cs.IsAbstract(ctxt)) and ((FParams = nil) or not FParams.activeOnly or not cs.isInactive(ctxt));
            if (not result) then
              cause := itBusinessRule;
            cs.Displays(ctxt, displays, FParams.displayLanguage);
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
          result := checker.check(system, version, code, abstractOk, implySystem, displays, message, cause);
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
            result := result and checker.check(system, version, code, abstractOk, implySystem, displays, message, cause)
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
            excluded := excluded and checker.check(system, version, code, abstractOk, implySystem, displays, message, cause);
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
  list : TStringList;
  message : String;
  cause : TFhirIssueType;
begin
  result := FFactory.makeParameters;
  try
    list := TStringList.Create;
    try
      list.Duplicates := Classes.dupIgnore;
      list.CaseSensitive := false;
      if check(coding.systemUri, coding.version, coding.code, abstractOk, implySystem, list, message, cause) then
      begin
        result.AddParamBool('result', true);
        if (coding.display <> '') and (list.IndexOf(coding.display) < 0) then
          result.AddParamStr('message', 'The display "'+coding.display+'" is not a valid display for the code '+coding.code+' - should be one of ['+list.CommaText+']');
        if list.Count > 0 then
          result.AddParamStr('display', list[0]);
        if cause <> itNull then
          result.AddParamStr('cause', CODES_TFhirIssueType[cause]);
      end
      else
      begin
        result.AddParamBool('result', false);
        result.AddParamStr('message', 'The system/code "'+coding.systemUri+'"/"'+coding.code+'" is not in the value set '+FValueSet.name);
        if (message <> '') then
          result.AddParamStr('message', message);
        if cause <> itNull then
          result.AddParamStr('cause', CODES_TFhirIssueType[cause]);
      end;
    finally
      list.Free;
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
  list : TStringList;
  v : boolean;
  ok : boolean;
  cc, codelist, message, mt: String;
  prov : TCodeSystemProvider;
  ctxt : TCodeSystemProviderContext;
  c : TFhirCodingW;
  cause : TFhirIssueType;
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
    list := TStringList.Create;
    try
      list.Duplicates := Classes.dupIgnore;
      list.CaseSensitive := false;
      ok := false;
      codelist := '';
      mt := '';
      for c in code.codings.forEnum do
      begin
        list.Clear;
        cc := ',{'+c.systemUri+'}'+c.code;
        codelist := codelist + cc;
        v := check(c.systemUri, c.version, c.code, abstractOk, implySystem, list, message, cause);
        if not v and (message <> '') then
          msg(message);
        ok := ok or v;
        message := '';

        if (v) then
        begin
          if (c.display <> '') and (list.IndexOf(c.display) < 0) then
            msg('The display "'+c.display+'" is not a valid display for the code '+cc.substring(1)+' - should be one of ['+list.CommaText+']');
          if list.Count > 0 then
            result.AddParamStr('display', list[0]);
        end
        else
        begin
          prov := findCodeSystem(c.systemUri, c.version, FParams, true);
          try
           if (prov = nil) then
           begin
             msg('The code system "'+c.systemUri+'" is not known (encountered paired with code = "'+c.code+'")');
             cause := itNotFound;
           end
           else
           begin
             ctxt := prov.locate(c.code, message);
             try
               if ctxt = nil then
               begin
                 msg(message);
                 if (c.systemUri = 'http://ncimeta.nci.nih.gov') then
                 begin
                   // cause := itInvali;
                 end
                 else
                 begin
                   msg('The code "'+c.code+'" is not valid in the system '+c.systemUri);
                   cause := itInvalid;
                 end;
               end
               else
               begin
                 prov.Displays(ctxt, list, THTTPLanguages.Create('en'));
                 if (c.display <> '') and (list.IndexOf(c.display) = -1) then
                   msg('The display "'+c.display+'" is not a valid display for the code '+cc+' - should be one of ['+list.CommaText+']')
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
    result.Link;
  finally
    result.free;
  end;
end;

function TValueSetChecker.check(system, version, code: String; implySystem : boolean): TFhirParametersW;
var
  list : TStringList;
  message : String;
  cause : TFhirIssueType;
begin
  result := FFactory.makeParameters;
  try
    list := TStringList.Create;
    try
      list.Duplicates := Classes.dupIgnore;
      list.CaseSensitive := false;
      if check(system, version, code, true, implySystem, list, message, cause) then
      begin
        result.AddParamBool('result', true);
        if list.Count > 0 then
          result.AddParamStr('display', list[0]);
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

function TValueSetChecker.checkConceptSet(cs: TCodeSystemProvider; cset : TFhirValueSetComposeIncludeW; code: String; abstractOk : boolean; displays : TStringList; var message : String): boolean;
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
        cs.displays(loc, displays, FParams.displayLanguage);
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
        cs.close(loc);
        cs.displays(code, displays, THTTPLanguages.Create('en'));
        result := (abstractOk or not cs.IsAbstract(loc));
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
          filters[i] := cs.filter(fc.prop, fc.Op, fc.value, prep);
          inc(i);
        end;
        if cs.prepare(prep) then // all are together, just query the first filter
        begin
          ctxt := filters[0];
          loc := cs.filterLocate(ctxt, code);
          try
            result := (loc <> nil) and (abstractOk or not cs.IsAbstract(loc));
            if result then
              cs.displays(loc, displays, FParams.displayLanguage);
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
                  cs.displays(loc, displays, FParams.displayLanguage);
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
                    cs.displays(loc, displays, FParams.displayLanguage);
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
                  cs.displays(loc, displays, FParams.displayLanguage);
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

function TValueSetChecker.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FOthers.sizeInBytes);
  inc(result, FValueSet.sizeInBytes);
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

  filter := TSearchFilterText.create(textFilter);
  map := TFslMap<TFhirValueSetExpansionContainsW>.create('VS.Expander.map');
  list := TFslList<TFhirValueSetExpansionContainsW>.create;
  try
    if filter.null then
      FLimit := UPPER_LIMIT_NO_TEXT
    else
      FLimit := UPPER_LIMIT_TEXT;

    if (limit > 0) and (limit < FLimit) then
      FLimit := limit;
    FCount := count;
    FOffset := offset;

    exp := result.forceExpansion;
    try
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
                handleDefine(cs2, list, map, ics, cl, filter, exp, params, nil);
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
          handleCompose(list, map, source, filter, dependencies, exp, params, notClosed);
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
    finally
      exp.free;
    end;

    result.link;
  finally
    map.free;
    list.free;
    result.free;
    filter.Free;
  end;
end;

function TFHIRValueSetExpander.key(system, code, display : String): string;
begin
  result:= '{'+system+'}'+code+'#1'+display;
end;

function TFHIRValueSetExpander.key(c: TFhirValueSetExpansionContainsW): string;
begin
  result := key(c.systemUri, c.Code, c.display);
end;

procedure TFHIRValueSetExpander.hashImport(hash: TStringList; cc: TFhirValueSetExpansionContainsW);
var
  ccc : TFhirValueSetExpansionContainsW;
begin
  if cc.code <> '' then
    hash.Add(cc.systemUri+#1+cc.code);
  for ccc in cc.contains.forEnum do
    hashImport(hash, ccc);
end;

function TFHIRValueSetExpander.makeImportHash(imports: TFslList<TFHIRValueSetW>; start: integer): TStringList;
var
  i : integer;
  vs : TFHIRValueSetW;
  cc : TFhirValueSetExpansionContainsW;
begin
  if imports.Count <= start then
    exit(nil);

  result := TStringList.Create;
  try
    for i := start to imports.Count - 1 do
    begin
      vs := imports[i];
      for cc in vs.expansion.contains.forEnum do
        hashImport(result, cc);
    end;
    result.Sorted := true;
  except
    result.Free;
    raise;
  end;
end;

procedure TFHIRValueSetExpander.handleCompose(list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; source: TFhirValueSetW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; params : TFHIRExpansionParams; var notClosed : boolean);
var
  vs : TFHIRValueSetW;
  s : String;
  c : TFhirValueSetComposeIncludeW;
begin
  for s in source.imports do
  begin
    vs := expandValueSet(s, filter.filter, dependencies, notClosed);
    try
      importValueSet(list, map, vs, expansion, nil);
    finally
      vs.free;
    end;
  end;

  for c in source.includes.forEnum do
    checkSource(c, filter);
  for c in source.excludes.forEnum do
    checkSource(c, filter);

  for c in source.includes.forEnum do
    processCodes(false, list, map, c, filter, dependencies, expansion, params, notClosed);
  for c in source.excludes.forEnum do
    processCodes(true, list, map, c, filter, dependencies, expansion, params, notClosed);
end;

procedure TFHIRValueSetExpander.handleDefine(cs : TFhirCodeSystemW; list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; source : TFhirValueSetCodeSystemW; defines : TFhirCodeSystemConceptListW; filter : TSearchFilterText; expansion : TFhirValueSetExpansionW; params : TFHIRExpansionParams; importHash : TStringList);
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
      addDefinedCode(cs, list, map, source.systemUri, cm, params, importHash);
    handleDefine(cs, list, map, source, cm.conceptList, filter, nil, params, importHash);
  end;
end;

function TFHIRValueSetExpander.chooseDisplay(c: TFhirCodeSystemConceptW; params : TFHIRExpansionParams) : String;
var
  ccd : TFhirCodeSystemConceptDesignationW;
begin
  result := c.display;
  for ccd in c.designations.forEnum do
    if FParams.displayLanguage.matches(ccd.language) then
      result := ccd.value;
end;

function TFHIRValueSetExpander.chooseDisplay(c: TFhirValueSetComposeIncludeConceptW; params : TFHIRExpansionParams) : String;
var
  ccd : TFhirValueSetComposeIncludeConceptDesignationW;
begin
  result := c.display;
  for ccd in c.designations.forEnum do
    if (FParams.displayLanguage.matches(ccd.language)) then
      result := ccd.value;
end;

constructor TFHIRValueSetExpander.Create(factory: TFHIRFactory; getVS: TGetValueSetEvent; getCS: TGetProviderEvent; txResources : TFslMetadataResourceList; getExpansion: TGetExpansionEvent; checkExpand : TCheckCanExpandEvent);
begin
  inherited create(factory, getVS, getCS, txResources);
  FOnGetExpansion := getExpansion;
  FOnCheckCanExpand := checkExpand;
end;

procedure TFHIRValueSetExpander.addDefinedCode(cs : TFhirCodeSystemW; list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; system: string; c: TFhirCodeSystemConceptW; params : TFHIRExpansionParams; importHash : TStringList);
var
  i : integer;
begin
  if not FParams.excludeNotForUI or not (cs.isAbstract(c)) then
    processCode(false, list, map, system, '', c.Code, chooseDisplay(c, params), c.definition, nil, params, importHash);
  for i := 0 to c.conceptList.count - 1 do
    addDefinedCode(cs, list, map, system, c.conceptList[i], params, importHash);
end;

function TFHIRValueSetExpander.passesImportFilter(importHash: TStringList; system, code: string): boolean;
var
  i : integer;
begin
  if importHash = nil then
    result := true
  else
    result := importHash.find(system+#1+code, i);
end;

function TFHIRValueSetExpander.canonical(system, version : String) : String;
begin
  if FFactory.version = fhirVersionRelease2 then
    result := system + '?version='+version
  else
    result := system + '|'+version
end;

procedure TFHIRValueSetExpander.processCode(doDelete : boolean; list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; system, version, code, display, definition: string; expansion : TFhirValueSetExpansionW; params : TFHIRExpansionParams; importHash : TStringList);
var
  n : TFhirValueSetExpansionContainsW;
  s : String;
begin
  if not passesImportFilter(importHash, system, code) then
    exit;

  if (map.Count >= FLimit) and not doDelete then
    raise ETooCostly.create('Too many codes to display (>'+inttostr(FLimit)+') (A text filter may reduce the number of codes in the expansion)');

  if (expansion <> nil) and (version <> '') then
  begin
    s := canonical(system, version);
    if not expansion.hasParam('version', s) then
      expansion.addParam('version', s);
  end;

  s := key(system, code, display);

  if doDelete or not map.containsKey(s) then
  begin
    n := FFactory.makeValueSetContains;
    try
      n.systemUri := system;
      n.Code := code;
      if (display <> '') then
        n.Display := display
      else
        n.Display := code;
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
begin
  FOnCheckCanExpand(self, uri, FParams);
end;

function TFHIRValueSetExpander.expandValueSet(uri: String; filter : String; dependencies : TStringList; var notClosed : boolean) : TFHIRValueSetW;
var
  dep : TStringList;
begin
  dep := TStringList.Create;
  try
    result := FOnGetExpansion(self, uri, filter, FParams, dep, FLimit);
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

procedure TFHIRValueSetExpander.importValueSet(list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; vs : TFHIRValueSetW; expansion : TFhirValueSetExpansionW; importHash : TStringList);
var
  c : TFhirValueSetExpansionContainsW;
  s : String;
begin
  expansion.copyParams(vs.expansion);

  for c in vs.expansion.contains.forEnum do
  begin
    s := key(c);
    if passesImportFilter(importHash, c.systemUri, c.code) and not map.containsKey(s) then
    begin
      list.add(c.link);
      map.add(s, c.link);
    end;
  end;
end;

procedure TFHIRValueSetExpander.checkSource(cset: TFhirValueSetComposeIncludeW; filter : TSearchFilterText);
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

          if not imp and (cs.TotalCount > FLimit) and not (FParams.limitedExpansion) then
            raise ETooCostly.create('Too many codes to display (>'+inttostr(FLimit)+') (A text filter may reduce the number of codes in the expansion)');
        end
      end;

    finally
      cs.free;
    end;
  end;
end;

procedure TFHIRValueSetExpander.processCodes(doDelete : boolean; list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; cset: TFhirValueSetComposeIncludeW; filter : TSearchFilterText; dependencies : TStringList; expansion : TFhirValueSetExpansionW; params : TFHIRExpansionParams; var notClosed : boolean);
var
  cs : TCodeSystemProvider;
  i, offset, count : integer;
  fc : TFhirValueSetComposeIncludeFilterW;
  fcl : TFslList<TFhirValueSetComposeIncludeFilterW>;
  c : TCodeSystemProviderContext;
  filters : Array of TCodeSystemProviderFilterContext;
  ctxt : TCodeSystemProviderFilterContext;
  ok : boolean;
  prep : TCodeSystemProviderFilterPreparationContext;
  inner : boolean;
  s, display : String;
  imports : TFslList<TFHIRValueSetW>;
  hash : TStringList;
  base : TFHIRValueSetW;
  cc : TFhirValueSetComposeIncludeConceptW;
  cctxt : TCodeSystemProviderContext;
begin
  imports := TFslList<TFHIRValueSetW>.create;
  try
    FFactory.checkNoModifiers(cset,'ValueSetExpander.processCodes', 'set');
    for s in cset.valueSets do
      imports.add(expandValueset(s, filter.filter, dependencies, notClosed));

    if cset.systemUri = '' then
    begin
      base := imports[0];
      hash := makeImportHash(imports, 1);
      try
        importValueSet(list, map, base, expansion, hash);
      finally
        hash.Free;
      end;
    end
    else
    begin
      hash := makeImportHash(imports, 0);
      try
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
              base := expandValueSet(cs.SpecialEnumeration, filter.filter, dependencies, notClosed);
              try
                expansion.addExtension('http://hl7.org/fhir/StructureDefinition/valueset-toocostly', FFactory.makeBoolean(true));
                importValueSet(list, map, base, expansion, hash);
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

              if imports.Empty and (cs.TotalCount > FLimit) and not (FParams.limitedExpansion) then
                raise ETooCostly.create('Too many codes to display (>'+inttostr(FLimit)+') (A text filter may reduce the number of codes in the expansion)');
              for i := 0 to cs.ChildCount(nil) - 1 do
                processCodeAndDescendants(doDelete, list, map, cs, cs.getcontext(nil, i), expansion, params, hash)
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
                    processCode(doDelete, list, map, cs.systemUri(c), cs.version(c), cs.code(c), cs.display(c, FParams.displayLanguage), cs.definition(c), expansion, params, hash);
                  end;
                finally
                  cs.Close(ctxt);
                end;
              finally
                cs.Close(prep);
              end;
            end;
          end;

          for cc in cset.concepts.forEnum do
          begin
            FFactory.checkNoModifiers(cc, 'ValueSetExpander.processCodes', 'set concept reference');
            cctxt := cs.locate(cc.code);
            try
              if (cctxt <> nil) and (not params.activeOnly or not cs.IsInactive(cctxt)) then
              begin
                display := chooseDisplay(cc, params);
                if (display = '') then
                  display := cs.Display(cctxt, FParams.displayLanguage);
                if filter.passes(display) or filter.passes(cc.code) then
                  processCode(doDelete, list, map, cs.systemUri(nil), cs.version(nil), cc.code, display, cs.Definition(cctxt), expansion, params, hash);
              end;
            finally
              cs.Close(cctxt);
            end;
          end;

          if cset.hasFilters then
          begin
            fcl := cset.filters;
            try
              prep := cs.getPrepContext;
              try
                try
                  if filter.null then
                  begin
                    SetLength(filters, fcl.count);
                    offset := 0;
                  end
                  else
                  begin
                    SetLength(filters, fcl.count+1);
                    offset := 1;
                    filters[0] := cs.searchFilter(filter, prep, true); // this comes first, because it imposes order
                  end;

                  if cs.specialEnumeration <> '' then
                  begin
                    SetLength(filters, length(filters)+1);
                    filters[offset] := cs.specialFilter(prep, true);
                    offset := offset + 1;
                    expansion.addExtension('http://hl7.org/fhir/StructureDefinition/valueset-toocostly', FFactory.makeBoolean(true));
                    notClosed := true;
                  end;
                  for i := 0 to fcl.count - 1 do
                  begin
                    fc := fcl[i];
                    ffactory.checkNoModifiers(fc, 'ValueSetExpander.processCodes', 'filter');
                    filters[i+offset] := cs.filter(fc.prop, fc.Op, fc.value, prep);
                    if filters[i+offset] = nil then
                      raise ETerminologyError.create('The filter "'+fc.prop +' '+ CODES_TFhirFilterOperator[fc.Op]+ ' '+fc.value+'" was not understood in the context of '+cs.systemUri(nil));
                    if cs.isNotClosed(filter, filters[i+offset]) then
                      notClosed := true;
                  end;

                  inner := not cs.prepare(prep);
                  count := 0;
                  While cs.FilterMore(filters[0]) and ((FOffset + FCount = 0) or (count < FOffset + FCount)) do
                  begin
                    c := cs.FilterConcept(filters[0]);
                    try
                      ok := not params.activeOnly or not cs.IsInactive(c);
                      if inner then
                        for i := 1 to length(filters) - 1 do
                          ok := ok and cs.InFilter(filters[i], c);
                      if ok then
                      begin
                        inc(count);
                        if count > FOffset then
                          processCode(doDelete, list, map, cs.systemUri(nil), cs.version(nil), cs.code(c), cs.display(c, FParams.displayLanguage), cs.definition(c), expansion, params, hash);
                      end;
                    finally
                      cs.close(c);
                    end;
                  end;
                finally
                  for i := 0 to length(filters) - 1 do
                    if filters[i] <> nil then
                      cs.Close(filters[i]);
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
        hash.Free;
      end;
    end;
  finally
    imports.Free;
  end;
end;

procedure TFHIRValueSetExpander.processCodeAndDescendants(doDelete : boolean; list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; cs: TCodeSystemProvider; context: TCodeSystemProviderContext; expansion : TFhirValueSetExpansionW; params : TFHIRExpansionParams; importHash : TStringList);
var
  i : integer;
  vs : String;
begin
  try
    if (cs.version(nil) <> '') and (expansion <> nil) then
    begin
      vs := canonical(cs.systemUri(nil), cs.version(nil));
      if not expansion.hasParam('version', vs) then
        expansion.addParam('version', vs);
    end;
    if not FParams.excludeNotForUI or not cs.IsAbstract(context) then
      processCode(doDelete, list, map, cs.systemUri(context), '', cs.Code(context), cs.Display(context, FParams.displayLanguage), cs.definition(context), expansion, params, importHash);
    for i := 0 to cs.ChildCount(context) - 1 do
      processCodeAndDescendants(doDelete, list, map, cs, cs.getcontext(context, i), expansion, params, importHash);
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

function TFHIRExpansionParams.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFixedVersions.sizeInBytes);
  inc(result, FdisplayLanguage.sizeInBytes);
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
    b(excludeNested) +'|'+ b(excludeNotForUI) +'|'+ b(excludePostCoordinated) +'|'+uid+'|'+inttostr(ord(FValueSetMode));
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

function TFhirExpansionParamsFixedVersion.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (Fsystem.length * sizeof(char)) + 12);
  inc(result, (Fversion.length * sizeof(char)) + 12);
end;

end.



