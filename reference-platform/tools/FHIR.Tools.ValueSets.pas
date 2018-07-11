unit FHIR.Tools.ValueSets;

{
Copyright (c) 2001-2013, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

interface

{
todo:
  include designations
  include Inactive

}
uses
  SysUtils, Classes,
  FHIR.Support.Base, FHIR.Support.Collections, FHIR.Support.Utilities, 
  FHIR.Base.Objects, FHIR.Base.Common, FHIR.Tx.Service, FHIR.Base.Factory, FHIR.Base.Xhtml, FHIR.Base.Lang,
  FHIR.Tools.CodeSystemProvider;

{  SysUtils, Classes, FHIR.Support.Utilities, FHIR.Support.Utilities,
  FHIR.Support.Utilities,
  FHIR.Support.Collections, FHIR.Support.Base,
  FHIR.Base.Objects, FHIR.Base.Lang, FHIR.Base.Common, FHIR.Base.Factory,
  FHIR.Tx.Service;
  //, FHIR.Loinc.Services, FHIR.Snomed.Services, FHIR.Ucum.Services, FHIR.Tx.Server, FHIR.Tx.Manager;}

const
  UPPER_LIMIT_NO_TEXT = 10000;
  UPPER_LIMIT_TEXT = 1000;// won't expand a value set bigger than this - just takes too long, and no one's going to do anything with it anyway

  FHIR_VERSION_CANONICAL_SPLIT_2 = '?version=';
  FHIR_VERSION_CANONICAL_SPLIT_3p = '|';


Type
  TFhirExpansionParamsFixedVersionMode = (fvmCheck, fvmOverride);
  TFhirExpansionParamsFixedVersion = class (TFslObject)
  private
    Fsystem : String;
    Fversion : String;
    FMode : TFhirExpansionParamsFixedVersionMode;
  public
    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property mode : TFhirExpansionParamsFixedVersionMode read FMode write FMode;
  end;

  TFHIRExpansionParams = class (TFslObject)
  private
    FFixedVersions : TFslList<TFhirExpansionParamsFixedVersion>;
    FactiveOnly: boolean;
    FdisplayLanguage: string;
    FexcludeNested: boolean;
    FlimitedExpansion: boolean;
    FexcludeNotForUI: boolean;
    FexcludePostCoordinated: boolean;
    FincludeDesignations: boolean;
    FincludeDefinition: boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRExpansionParams;
    class function defaultProfile : TFHIRExpansionParams;

    property fixedVersions : TFslList<TFhirExpansionParamsFixedVersion> read FFixedVersions;
    property activeOnly : boolean read FactiveOnly write FactiveOnly;
    property displayLanguage : string read FdisplayLanguage write FdisplayLanguage;
    property includeDefinition : boolean read FincludeDefinition write FincludeDefinition;
    property limitedExpansion : boolean read FlimitedExpansion write FlimitedExpansion;
    property includeDesignations : boolean read FincludeDesignations write FincludeDesignations;
    property excludeNested : boolean read FexcludeNested write FexcludeNested;
    property excludeNotForUI : boolean read FexcludeNotForUI write FexcludeNotForUI;
    property excludePostCoordinated : boolean read FexcludePostCoordinated write FexcludePostCoordinated;

    function hash : String;
  end;

  TGetValueSetEvent = function (sender : TObject; url : String) : TFHIRValueSetW of object;
  TGetProviderEvent = function (sender : TObject; url, version : String; params : TFHIRExpansionParams) : TCodeSystemProvider of object;
  TGetExpansionEvent = function (sender : TObject; url, filter : String; params : TFHIRExpansionParams; dependencies : TStringList; limit : integer) : TFHIRValueSetW of object;

  TValueSetWorker = class (TFslObject)
  private
    FFactory : TFHIRFactory;
    FOnGetValueSet : TGetValueSetEvent;
    FOnGetCSProvider : TGetProviderEvent;
    FParams : TFHIRExpansionParams;

  public
    constructor Create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent); overload;
    destructor Destroy; override;
  end;

  TValueSetChecker = class (TValueSetWorker)
  private
    FOthers : TFslStringObjectMatch; // checkers or code system providers
    FValueSet : TFHIRValueSetW;
    FId: String;

    function check(system, version, code : String; abstractOk : boolean; displays : TStringList; var message : String) : boolean; overload;
    function findCode(cs : TFhirCodeSystemW; code: String; list : TFslList<TFhirCodeSystemConceptW>; displays : TStringList; out isabstract : boolean): boolean;
    function checkConceptSet(cs: TCodeSystemProvider; cset : TFhirValueSetComposeIncludeW; code : String; abstractOk : boolean; displays : TStringList; var message : String) : boolean;
    procedure prepareConceptSet(desc: string; cc: TFhirValueSetComposeIncludeW; var cs: TCodeSystemProvider);
    function getName: String;
  public
    constructor Create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; id : String); overload;
    destructor Destroy; override;

    property id : String read FId;
    property name : String read getName;

    procedure prepare(vs : TFHIRValueSetW; params : TFHIRExpansionParams);

    function check(system, version, code : String; abstractOk : boolean) : boolean; overload;
    function check(system, version, code : String) : TFhirParametersW; overload;
    function check(coding : TFhirCodingW; abstractOk : boolean): TFhirParametersW; overload;
    function check(code: TFhirCodeableConceptW; abstractOk : boolean) : TFhirParametersW; overload;

  end;

  TFHIRValueSetExpander = class (TValueSetWorker)
  private
    FLimit : integer;
    FCount : integer;
    FOffset : integer;
    FOnGetExpansion : TGetExpansionEvent;

    procedure processCodeAndDescendants(doDelete : boolean; list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; cs : TCodeSystemProvider; context : TCodeSystemProviderContext; expansion : TFhirValueSetExpansionW; params : TFHIRExpansionParams; importHash : TStringList);

    procedure handleDefine(cs : TFhirCodeSystemW; list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; source : TFhirValueSetCodeSystemW; defines : TFslList<TFhirCodeSystemConceptW>; filter : TSearchFilterText; expansion : TFhirValueSetExpansionW; params : TFHIRExpansionParams; importHash : TStringList);
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
  public
    constructor Create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; getExpansion : TGetExpansionEvent); overload;

    function expand(source : TFHIRValueSetW; params : TFHIRExpansionParams; textFilter : String; dependencies : TStringList; limit, count, offset : integer) : TFHIRValueSetW;
  end;


implementation

{ TValueSetWorker }

constructor TValueSetWorker.Create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent);
begin
  Create;
  FFactory := factory;
  FOnGetValueSet := getVS;
  FOnGetCSProvider := getCS;
end;

destructor TValueSetWorker.Destroy;
begin
  FFactory.Free;
  FParams.Free;
  inherited;
end;

{ TValueSetChecker }

constructor TValueSetChecker.create(factory : TFHIRFactory; getVS: TGetValueSetEvent; getCS : TGetProviderEvent; id : string);
begin
  inherited Create(factory, getVs, getCs);
  FId := id;
  FOthers := TFslStringObjectMatch.create;
  FOthers.PreventDuplicates;
end;

destructor TValueSetChecker.destroy;
begin
  FValueSet.Free;
  FOthers.Free;
  inherited;
end;

procedure TValueSetChecker.prepare(vs: TFHIRValueSetW; params : TFHIRExpansionParams);
var
  cs : TCodeSystemProvider;
  cc : TFhirValueSetComposeIncludeW;
  i, j : integer;
  other : TFHIRValueSetW;
  checker : TValueSetChecker;
  ics : TFHIRValueSetCodeSystemW;
  s : String;
begin
  FParams := params.Link;

  vs.checkNoImplicitRules('ValueSetChecker.prepare', 'ValueSet');
  FFactory.checkNoModifiers(vs, 'ValueSetChecker.prepare', 'ValueSet');
  if (vs = nil) then

  else
  begin
    FValueSet := vs.link;

    // r2:
    ics := FValueSet.inlineCS;
    if ics <> nil then
    begin
      try
        FFactory.checkNoModifiers(ics, 'ValueSetChecker.prepare', 'CodeSystem');
        FOthers.Add(ics.system, TFhirCodeSystemProvider.create(ffactory.link, TFHIRCodeSystemEntry.Create(FFactory.wrapCodeSystem(FValueSet.Resource.Link))));
      finally
        ics.Free;
      end;
    end;

    if (FValueSet.checkCompose('ValueSetChecker.prepare', 'ValueSet.compose')) then
    begin
      // not r2:
      for s in FValueSet.imports do
      begin
        other := FOnGetValueSet(self, s);
        try
          if other = nil then
            raise ETerminologyError.create('Unable to find value set '+s);
          checker := TValueSetChecker.create(FFactory.link, FOnGetValueSet, FOnGetCSProvider, other.url);
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
        prepareConceptSet('include', cc, cs);
      for cc in FValueSet.excludes.forEnum do
        prepareConceptSet('exclude', cc, cs);
    end;
  end;
end;

procedure TValueSetChecker.prepareConceptSet(desc: string; cc: TFhirValueSetComposeIncludeW; var cs: TCodeSystemProvider);
var
  other: TFhirValueSetW;
  checker: TValueSetChecker;
  s : string;
  ccf: TFhirValueSetComposeIncludeFilterW;
begin
  FFactory.checkNoModifiers(cc, 'ValueSetChecker.prepare', desc);
  for s in cc.valueSets do
  begin
    other := FOnGetValueSet(self, s);
    try
      if other = nil then
        raise ETerminologyError.create('Unable to find value set ' + s);
      checker := TValueSetChecker.create(FFactory.link, FOnGetValueSet, FOnGetCSProvider, other.url);
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
  if not FOthers.ExistsByKey(cc.system) then
    FOthers.Add(cc.system, FOnGetCSProvider(self, cc.system, cc.version, FParams));
  cs := TCodeSystemProvider(FOthers.matches[cc.system]);
  for ccf in cc.filters.forEnum do
  begin
    FFactory.checkNoModifiers(ccf, 'ValueSetChecker.prepare', desc + '.filter');
    if not (('concept' = ccf.prop) and (ccf.Op = foIsA)) then
      if not cs.doesFilter(ccf.prop, ccf.Op, ccf.value) then
        raise ETerminologyError.create('The filter "' + ccf.prop + ' ' + CODES_TFhirFilterOperator[ccf.Op] + ' ' + ccf.value + '" was not understood in the context of ' + cs.system(nil));
  end;
end;

function TValueSetChecker.findCode(cs : TFhirCodeSystemW; code: String; list : TFslList<TFhirCodeSystemConceptW>; displays : TStringList; out isabstract : boolean): boolean;
var
  i : integer;
  ccl : TFslList<TFhirCodeSystemConceptW>;
begin
  result := false;
  for i := 0 to list.count - 1 do
  begin
    if (code = list[i].code) then
    begin
      result := true;
      isabstract := cs.isAbstract(list[i]);
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

function TValueSetChecker.check(system, version, code: String; abstractOk : boolean): boolean;
var
  list : TStringList;
  msg : string;
begin
  list := TStringList.Create;
  try
    list.Duplicates := Classes.dupIgnore;
    list.CaseSensitive := false;
    result := check(system, version, code, abstractOk, list, msg);
  finally
    list.Free;
  end;
end;

function TValueSetChecker.check(system, version, code : String; abstractOk : boolean; displays : TStringList; var message : String) : boolean;
var
  cs : TCodeSystemProvider;
  ctxt : TCodeSystemProviderContext;
  cc : TFhirValueSetComposeIncludeW;
  excluded : boolean;
  i : integer;
  isabstract : boolean;
  checker : TValueSetChecker;
  s : String;
  ics : TFHIRValueSetCodeSystemW;
  ccl : TFslList<TFhirCodeSystemConceptW>;
begin
  result := false;
  {special case:}
  if (FValueSet.url = ANY_CODE_VS) then
  begin
    cs := FOnGetCSProvider(self, system, version, FParams{, true});
    try
      if cs = nil then
        result := false
      else
      begin
        ctxt := cs.locate(code);
        if (ctxt = nil) then
          result := false
        else
          try
            result := (abstractOk or not cs.IsAbstract(ctxt)) and ((FParams = nil) or not FParams.activeOnly or not cs.isInactive(ctxt));
            cs.Displays(ctxt, displays, FParams.displayLanguage);
          finally
            cs.Close(ctxt);
          end;
      end;
    finally
      cs.Free;
    end;
  end
  else
  begin
    ics := FValueSet.inlineCS; // r2
    if ics <> nil then
    begin
      try
        if (system = ics.system) or (system = SYSTEM_NOT_APPLICABLE) then
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
          result := checker.check(system, version, code, abstractOk, displays, message);
        end;
      end;
      for cc in FValueSet.includes.forEnum do
      begin
        if cc.system = '' then
          result := true
        else
        begin
          cs := TCodeSystemProvider(FOthers.matches[cc.system]);
          if cc.hasExtension('http://hl7.org/fhir/StructureDefinition/valueset-supplement') then
          begin
            s := cc.getExtensionString('http://hl7.org/fhir/StructureDefinition/valueset-supplement');
            if not cs.hasSupplement(s) then
              raise ETerminologyError.create('Value Set Validation depends on supplement '+s+' on '+cs.system(nil)+' that is not known');
          end;

          result := ((system = SYSTEM_NOT_APPLICABLE) or (cs.system(nil) = system)) and checkConceptSet(cs, cc, code, abstractOk, displays, message);
        end;
        for s in cc.valueSets do
        begin
          checker := TValueSetChecker(FOthers.matches[s]);
          result := result and checker.check(system, version, code, abstractOk, displays, message);
        end;
        if result then
          break;
      end;
      if result then
        for cc in FValueSet.excludes.forEnum do
        begin
          if cc.system = '' then
            excluded := true
          else
          begin
            cs := TCodeSystemProvider(FOthers.matches[cc.system]);
            if cc.hasExtension('http://hl7.org/fhir/StructureDefinition/valueset-supplement') then
            begin
              s := cc.getExtensionString('http://hl7.org/fhir/StructureDefinition/valueset-supplement');
              if not cs.hasSupplement(s) then
                raise ETerminologyError.create('Value Set Validation depends on supplement '+s+' on '+cs.system(nil)+' that is not known');
            end;
            excluded := ((system = SYSTEM_NOT_APPLICABLE) or (cs.system(nil) = system)) and checkConceptSet(cs, cc, code, abstractOk, displays, message);
          end;
          for s in cc.valueSets do
          begin
            checker := TValueSetChecker(FOthers.matches[s]);
            excluded := excluded and checker.check(system, version, code, abstractOk, displays, message);
          end;
          if excluded then
            exit(false);
        end;
    end;
  end;
end;


function TValueSetChecker.check(coding: TFhirCodingW; abstractOk : boolean) : TFhirParametersW;
var
  list : TStringList;
  message : String;
begin
  result := FFactory.makeParameters;
  try
    list := TStringList.Create;
    try
      list.Duplicates := Classes.dupIgnore;
      list.CaseSensitive := false;
      if check(coding.system, coding.version, coding.code, abstractOk, list, message) then
      begin
        result.AddParamBool('result', true);
        if (coding.display <> '') and (list.IndexOf(coding.display) < 0) then
          result.AddParamStr('message', 'The display "'+coding.display+'" is not a valid display for the code '+coding.code);
        if list.Count > 0 then
          result.AddParamStr('display', list[0]);
      end
      else
      begin
        result.AddParamBool('result', false);
        result.AddParamStr('message', 'The system/code "'+coding.system+'"/"'+coding.code+'" is not in the value set '+FValueSet.name);
        if (message <> '') then
          result.AddParamStr('message', message);
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
  for p in params.parameterList.forEnum do
    if (p.name = 'message') and (p.value.primitiveValue = msg) then
      exit(true);
end;

function TValueSetChecker.check(code: TFhirCodeableConceptW; abstractOk : boolean) : TFhirParametersW;
  function Summary(code: TFhirCodeableConceptW) : String;
  begin
    if (code.codingCount = 1) then
      result := 'The code provided ('+code.summary+') is not '
    else
      result := 'None of the codes provided ('+code.summary+') are ';
  end;
var
  list : TStringList;
  i : integer;
  v : boolean;
  ok : boolean;
  cc, codelist, message : String;
  prov : TCodeSystemProvider;
  ctxt : TCodeSystemProviderContext;
  c : TFhirCodingW;
begin
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
      for c in code.codings.forEnum do
      begin
        list.Clear;
        cc := ',{'+c.system+'}'+c.code;
        codelist := codelist + cc;
        v := check(c.system, c.version, c.code, abstractOk, list, message);
        if not v and (message <> '') then
          result.AddParamStr('message', message);
        ok := ok or v;

        if (v) then
        begin
          message := '';
          if (c.display <> '') and (list.IndexOf(c.display) < 0) then
            result.AddParamStr('message', 'The display "'+c.display+'" is not a valid display for the code '+cc.substring(1));
          if list.Count > 0 then
            result.AddParamStr('display', list[0]);
        end
        else
        begin
          prov := FOnGetCSProvider(nil, c.system, c.version, FParams{, true});
          try
           if (prov = nil) then
           begin
             result.AddParamStr('message', 'The system "'+c.system+'" is not known');
             result.AddParamStr('cause', 'unknown');
           end
           else
           begin
             ctxt := prov.locate(c.code, message);
             try
               if ctxt = nil then
               begin
                 result.AddParamStr('message', 'The code "'+c.code+'" is not valid in the system '+c.system);
                 result.AddParamStr('cause', 'invalid');
                 if (message <> '') and not hasMessage(result, message) then
                   result.AddParamStr('message', message);
               end
               else
               begin
                 prov.Displays(ctxt, list, '');
                 if (c.display <> '') and (list.IndexOf(c.display) = -1) then
                   result.AddParamStr('message', 'The display "'+c.display+'" is not a valid display for the code '+cc)
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
        if FValueSet.name = '' then
          result.AddParamStr('message', Summary(code) +' valid')
        else
          result.AddParamStr('message', Summary(code) +' valid in the value set '+FValueSet.name);
    finally
      list.Free;
    end;
    result.Link;
  finally
    result.free;
  end;
end;

function TValueSetChecker.check(system, version, code: String): TFhirParametersW;
var
  list : TStringList;
  message : String;
begin
  result := FFactory.makeParameters;
  try
    list := TStringList.Create;
    try
      list.Duplicates := Classes.dupIgnore;
      list.CaseSensitive := false;
      if check(system, version, code, true, list, message) then
      begin
        result.AddParamBool('result', true);
        if list.Count > 0 then
          result.AddParamStr('display', list[0]);
      end
      else
      begin
        result.AddParamBool('result', false);
        result.AddParamStr('message', 'The system/code "'+system+'"/"'+code+'" is not in the value set '+FValueSet.name);
        if (message <> '') then
          result.AddParamStr('message', message);
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
      end
      else

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
        cs.displays(code, displays, '');
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
          for fc in cfl do
          begin
            if ('concept' = fc.prop) and (fc.Op = foIsA) then
            begin
              loc := cs.locateIsA(code, fc.value);
              try
                result := (loc <> nil) and (abstractOk or not cs.IsAbstract(loc));
                if loc <> nil then
                  cs.displays(loc, displays, FParams.displayLanguage);
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
                result := (loc <> nil) and (abstractOk or not cs.IsAbstract(loc));
                if loc <> nil then
                  cs.displays(loc, displays, FParams.displayLanguage);
              finally
                cs.Close(loc);
              end;
            end;
            if not result then
              break;
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
  cl : TFslList<TFHIRCodeSystemConceptW>;
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
  map := TFslMap<TFhirValueSetExpansionContainsW>.create;
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
        exp.addParam('limitedExpansion', boolToStr(FParams.limitedExpansion));
      if FParams.displayLanguage <> '' then
        exp.addParam('displayLanguage', FParams.displayLanguage);
      if FParams.includeDesignations then
        exp.addParam('includeDesignations', boolToStr(FParams.includeDesignations));
      if FParams.includeDefinition then
        exp.addParam('includeDefinition', boolToStr(FParams.includeDefinition));
      if FParams.activeOnly then
        exp.addParam('activeOnly', boolToStr(FParams.activeOnly));
      if FParams.excludeNested then
        exp.addParam('excludeNested', boolToStr(FParams.excludeNested));
      if FParams.excludeNotForUI then
        exp.addParam('excludeNotForUI', boolToStr(FParams.excludeNotForUI));
      if FParams.excludePostCoordinated then
        exp.addParam('excludePostCoordinated', boolToStr(FParams.excludePostCoordinated));

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
              tr.AddChild('td').AddText(c.system);
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
  result := key(c.System, c.Code, c.display);
end;

procedure TFHIRValueSetExpander.hashImport(hash: TStringList; cc: TFhirValueSetExpansionContainsW);
var
  ccc : TFhirValueSetExpansionContainsW;
begin
  if cc.code <> '' then
    hash.Add(cc.system+#1+cc.code);
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
  i : integer;
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
    processCodes(false, list, map, c, filter, dependencies, expansion, params, notClosed);
  for c in source.excludes.forEnum do
    processCodes(true, list, map, c, filter, dependencies, expansion, params, notClosed);
end;

procedure TFHIRValueSetExpander.handleDefine(cs : TFhirCodeSystemW; list : TFslList<TFhirValueSetExpansionContainsW>; map : TFslMap<TFhirValueSetExpansionContainsW>; source : TFhirValueSetCodeSystemW; defines : TFslList<TFhirCodeSystemConceptW>; filter : TSearchFilterText; expansion : TFhirValueSetExpansionW; params : TFHIRExpansionParams; importHash : TStringList);
var
  cm : TFhirCodeSystemConceptW;
begin
  if (defines.Count > 0) and (expansion <> nil) and (cs.version <> '') then
  begin
    if FFactory.version = fhirVersionRelease2 then
      expansion.addParam('version', source.system+FHIR_VERSION_CANONICAL_SPLIT_2+cs.version)
    else
     expansion.addParam('version', source.system+FHIR_VERSION_CANONICAL_SPLIT_3p+cs.version);
  end;
  for cm in defines do
  begin
    FFactory.checkNoModifiers(cm, 'ValueSetExpander.handleDefine', 'concept');
    if filter.passes(cm.display) or filter.passes(cm.code) then
      addDefinedCode(cs, list, map, source.system, cm, params, importHash);
    handleDefine(cs, list, map, source, cm.conceptList, filter, nil, params, importHash);
  end;
end;

function TFHIRValueSetExpander.chooseDisplay(c: TFhirCodeSystemConceptW; params : TFHIRExpansionParams) : String;
var
  ccd : TFhirCodeSystemConceptDesignationW;
begin
  result := c.display;
  for ccd in c.designations.forEnum do
    if (FParams.displayLanguage = '') or languageMatches(FParams.displayLanguage, ccd.language) then
      result := ccd.value;
end;

function TFHIRValueSetExpander.chooseDisplay(c: TFhirValueSetComposeIncludeConceptW; params : TFHIRExpansionParams) : String;
var
  ccd : TFhirValueSetComposeIncludeConceptDesignationW;
begin
  result := c.display;
  for ccd in c.designations.forEnum do
    if (FParams.displayLanguage = '') or languageMatches(FParams.displayLanguage, ccd.language) then
      result := ccd.value;
end;

constructor TFHIRValueSetExpander.Create(factory: TFHIRFactory; getVS: TGetValueSetEvent; getCS: TGetProviderEvent; getExpansion: TGetExpansionEvent);
begin
  inherited create(factory, getVS, getCS);
  FOnGetExpansion := getExpansion;
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

procedure TFHIRValueSetExpander.processCode(doDelete : boolean; list: TFslList<TFhirValueSetExpansionContainsW>; map: TFslMap<TFhirValueSetExpansionContainsW>; system, version, code, display, definition: string; expansion : TFhirValueSetExpansionW; params : TFHIRExpansionParams; importHash : TStringList);
var
  n : TFhirValueSetExpansionContainsW;
  s : String;
  f : boolean;
begin
  if not passesImportFilter(importHash, system, code) then
    exit;

  if (map.Count >= FLimit) and not doDelete then
    raise ETooCostly.create('Too many codes to display (>'+inttostr(FLimit)+') (A text filter may reduce the number of codes in the expansion)');

  if (expansion <> nil) and (version <> '') then
  begin
    s := system+'?version='+version;
    if not expansion.hasParam('version', s) then
      expansion.addParam('version', s);
  end;

  n := expansion.addContains;
  try
    n.System := system;
    n.Code := code;
    if (display <> '') then
      n.Display := display
    else
      n.Display := code;
    s := key(n);
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
  i : integer;
  c : TFhirValueSetExpansionContainsW;
  s : String;
begin
  expansion.copyParams(vs.expansion);

  for c in vs.expansion.contains.forEnum do
  begin
    s := key(c);
    if passesImportFilter(importHash, c.system, c.code) and not map.containsKey(s) then
    begin
      list.add(c.link);
      map.add(s, c.link);
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
begin
  imports := TFslList<TFHIRValueSetW>.create;
  try
    FFactory.checkNoModifiers(cset,'ValueSetExpander.processCodes', 'set');
    for s in cset.valueSets do
      imports.add(expandValueset(s, filter.filter, dependencies, notClosed));

    if cset.system = '' then
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
        cs := FOnGetCSProvider(self, cset.system, cset.version, FParams);
        try
          if cset.hasExtension('http://hl7.org/fhir/StructureDefinition/valueset-supplement') then
          begin
            s := cset.getExtensionString('http://hl7.org/fhir/StructureDefinition/valueset-supplement');
            if not cs.hasSupplement(s) then
              raise ETerminologyError.create('Expansion depends on supplement '+s+' on '+cs.system(nil)+' that is not known');
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
                  raise ETooCostly.create('The code System "'+cs.system(nil)+'" has a grammar, and cannot be enumerated directly. If an incomplete expansion is requested, a limited enumeration will be returned')
                else
                  raise ETooCostly.create('The code System "'+cs.system(nil)+'" has a grammar, and cannot be enumerated directly');

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
                    processCode(doDelete, list, map, cs.system(c), cs.version(c), cs.code(c), cs.display(c, FParams.displayLanguage), cs.definition(c), expansion, params, hash);
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
            display := chooseDisplay(cc, params);
            if (display = '') then
              display := cs.getDisplay(cc.code, FParams.displayLanguage);
            if filter.passes(display) or filter.passes(cc.code) then
              processCode(doDelete, list, map, cs.system(nil), cs.version(nil), cc.code, display, cs.getDefinition(cc.code), expansion, params, hash);
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
                      raise ETerminologyError.create('The filter "'+fc.prop +' '+ CODES_TFhirFilterOperator[fc.Op]+ ' '+fc.value+'" was not understood in the context of '+cs.system(nil));
                    if cs.isNotClosed(filter, filters[i+offset]) then
                      notClosed := true;
                  end;

                  inner := not cs.prepare(prep);
                  count := 0;
                  While cs.FilterMore(filters[0]) and ((FOffset + FCount = 0) or (count < FOffset + FCount)) do
                  begin
                    c := cs.FilterConcept(filters[0]);
                    ok := true;
                    if inner then
                      for i := 1 to length(filters) - 1 do
                        ok := ok and cs.InFilter(filters[i], c);
                    if ok then
                    begin
                      inc(count);
                      if count > FOffset then
                        processCode(doDelete, list, map, cs.system(nil), cs.version(nil), cs.code(c), cs.display(c, FParams.displayLanguage), cs.definition(c), expansion, params, hash);
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
begin
  try
    if (cs.version(nil) <> '') and (expansion <> nil) then
      expansion.addParam('version', cs.system(nil)+'?version='+cs.version(nil));

    if not FParams.excludeNotForUI or not cs.IsAbstract(context) then
      processCode(doDelete, list, map, cs.system(context), '', cs.Code(context), cs.Display(context, FParams.displayLanguage), cs.definition(context), expansion, params, importHash);
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
begin
  result := inttostr(HashStringToCode32(FFixedVersions.ToString));
end;

function TFHIRExpansionParams.link: TFHIRExpansionParams;
begin
  result := TFHIRExpansionParams(inherited Link);
end;


end.
