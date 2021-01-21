{
todo:
 - support for descendent-of, ValueSet.compose.inactive

}
unit tx_server;

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
  SysUtils, Classes, IniFiles, Generics.Collections,

  fsl_base, fsl_utilities, fsl_collections, fsl_http, fsl_threads,
  fdb_manager,
  fhir_objects, fhir_common, fhir_cdshooks, fhir_factory,
  fhir_valuesets,
  session,
  ftx_service, ftx_sct_services, ftx_loinc_services, ftx_ucum_services, tx_rxnorm, tx_unii,
  tx_lang, closuremanager, adaptations, utilities, bundlebuilder,
  tx_manager, ftx_sct_expressions;

Type
  TTerminologyServer = class (TTerminologyServerStore)
  private
    FExpansions : TFslStringObjectMatch;
    FDependencies : TFslStringObjectMatch; // object is TFslStringList of identity
    FClosures : TFslMap<TClosureManager>;
    FWebBase : String;

    procedure AddDependency(name, value : String);
//    function getCodeDefinition(c : TFhirCodeSystemConceptW; code : string) : TFhirCodeSystemConceptW; overload;
//    function getCodeDefinition(vs : TFhirCodeSystemW; code : string) : TFhirCodeSystemConceptW; overload;
    function makeAnyValueSet: TFhirValueSetW;

    // database maintenance
    procedure processValueSet(ValueSetKey : integer; URL : String; conn2, conn3 : TFDBConnection);
    procedure processConcept(ConceptKey : integer; URL, version, Code : String; conn2, conn3 : TFDBConnection);
    function isOkTarget(cm: TLoadedConceptMap; vs: TFhirValueSetW): boolean;
    function isOkSource(cm: TLoadedConceptMap; vs: TFhirValueSetW; coding: TFHIRCodingW; out group : TFhirConceptMapGroupW; out match : TFhirConceptMapGroupElementW): boolean; overload;
    function isOkSource(cm: TLoadedConceptMap; coding: TFHIRCodingW; out group : TFhirConceptMapGroupW; out match : TFhirConceptMapGroupElementW): boolean; overload;
    procedure LoadClosures;
    procedure BuildIndexesInternal(prog : boolean; conn1, conn2, conn3 : TFDBConnection);

    function workerGetDefinition(sender : TObject; url : String) : TFHIRValueSetW;
    function workerGetProvider(sender : TObject; url, version : String; params : TFHIRExpansionParams; nullOk : boolean) : TCodeSystemProvider;
    function workerGetExpansion(sender : TObject; url, filter : String; params : TFHIRExpansionParams; dependencies : TStringList; limit : integer) : TFHIRValueSetW;
    procedure workerCanExpand(sender : TObject; url : String; params : TFHIRExpansionParams);
  protected
    procedure invalidateVS(id : String); override;
  public
    constructor Create(db : TFDBManager; factory : TFHIRFactory; common : TCommonTerminologies); override;
    destructor Destroy; override;
    function Link: TTerminologyServer; overload;
    property webBase : String read FWebBase write FWebBase;

    // functional services

    // if this id identifies a value set known to the external resources (if it does, construct it and return it)
    function isKnownValueSet(id : String; out vs : TFhirValueSetW): Boolean;

    // given a value set, expand it
    function expandVS(vs : TFhirValueSetW; cacheId : String; profile : TFHIRExpansionParams; textFilter : String; limit, count, offset : integer; txResources : TFslMetadataResourceList) : TFhirValueSetW; overload;
    function expandVS(uri : String; profile : TFHIRExpansionParams; textFilter : String; limit, count, offset : integer) : TFhirValueSetW; overload;

    // these are internal services - not for use outside the terminology server
    function expandVS(uri: String; profile : TFHIRExpansionParams; textFilter : String; dependencies : TStringList; limit, count, offset : integer) : TFhirValueSetW; overload;
    function expandVS(vs: TFhirValueSetW; cacheId : String; profile : TFHIRExpansionParams; textFilter : String; dependencies : TStringList; limit, count, offset : integer; txResources : TFslMetadataResourceList): TFhirValueSetW; overload;

    procedure lookupCode(coding : TFHIRCodingW; const lang : THTTPLanguages; props : TArray<String>; resp : TFHIRLookupOpResponseW);
    function validate(vs : TFhirValueSetW; coding : TFHIRCodingW; profile : TFHIRExpansionParams; abstractOk, implySystem : boolean; txResources : TFslMetadataResourceList) : TFhirParametersW; overload;
    function validate(vs : TFhirValueSetW; coded : TFhirCodeableConceptW; profile : TFHIRExpansionParams; abstractOk, implySystem: boolean; txResources : TFslMetadataResourceList) : TFhirParametersW; overload;
    function translate(const lang : THTTPLanguages; cm : TLoadedConceptMap; coding : TFHIRCodingW) : TFhirParametersW; overload;
    function translate(const lang : THTTPLanguages; source : TFhirValueSetW; coding : TFHIRCodingW; target : TFhirValueSetW) : TFhirParametersW; overload;
    function translate(const lang : THTTPLanguages; source : TFhirValueSetW; coded : TFhirCodeableConceptW; target : TFhirValueSetW) : TFhirParametersW; overload;
    Function MakeChecker(uri : string; profile : TFHIRExpansionParams) : TValueSetChecker;
    function getDisplayForCode(const lang : THTTPLanguages; system, version, code : String): String;
    function checkCode(op : TFhirOperationOutcomeW; const lang : THTTPLanguages; path : string; code : string; system, version : string; display : string) : boolean;
    function isValidCode(system, code : String) : boolean;
    // procedure composeCode(req : TFHIRComposeOpRequest; resp : TFHIRComposeOpResponse);
    function findCanonicalResources(bundle : TFHIRBundleBuilder; rType : String; url, version : String) : boolean;

    // closures
    function InitClosure(name : String) : String;
    function UseClosure(name : String; out cm : TClosureManager) : boolean;
    function enterIntoClosure(conn : TFDBConnection; name, uri, code : String) : integer;

    procedure getCodeView(const lang : THTTPLanguages; coding : TFHIRCodingW; response : TCDSHookResponse); overload;
    procedure getCodeView(const lang : THTTPLanguages; coding : TFhirCodeableConceptW; response : TCDSHookResponse); overload;

    // database maintenance
    procedure BuildIndexes(prog : boolean);
    function Summary : String;
    function cacheSize : UInt64; override;
    procedure clearCache;
  end;

implementation

uses
  fsl_logging;


{ TTerminologyServer }

constructor TTerminologyServer.Create(db : TFDBManager; factory : TFHIRFactory; common : TCommonTerminologies);
begin
  inherited;
  FExpansions := TFslStringObjectMatch.create;
  FExpansions.PreventDuplicates;
  FDependencies := TFslStringObjectMatch.create;
  FDependencies.PreventDuplicates;
  FClosures := TFslMap<TClosureManager>.create('tx.closure');
  if (DB <> nil) then
    LoadClosures;
end;

destructor TTerminologyServer.Destroy;
begin
  FClosures.Free;
  FDependencies.Free;
  FExpansions.free;
  inherited;
end;

procedure TTerminologyServer.LoadClosures;
var
  conn : TFDBConnection;
begin
  conn := FDB.GetConnection('LoadClosures');
  try
    conn.SQL := 'Select ClosureKey, Name, Version from Closures';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
      FClosures.Add(conn.ColStringByName['name'], TClosureManager.create(conn.ColStringByName['name'], conn.ColIntegerByName['ClosureKey'], conn.ColIntegerByName['Version'], self));
    conn.terminate;
    conn.Release;
  except
    on e : exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
end;

procedure TTerminologyServer.lookupCode(coding : TFHIRCodingW; const lang : THTTPLanguages; props : TArray<String>; resp : TFHIRLookupOpResponseW);
var
  provider : TCodeSystemProvider;
  ctxt : TCodeSystemProviderContext;
  s : String;
  p : TFHIRLookupOpRespPropertyW;

  function hasProp(name : String; def : boolean) : boolean;
  begin
    if (props = nil) or (length(props) = 0) then
      result := def
    else
      result := StringArrayExistsInsensitive(props, name);
  end;
begin
  provider := getProvider(coding.systemUri, coding.version, nil);
  try
    resp.name := provider.name(nil);
    s := provider.version(nil);
    if (s <> '') then
      resp.version := s;
    ctxt := provider.locate(coding.code);
    try
      if ctxt = nil then
        raise ETerminologyError.create('Unable to find code '+coding.code+' in '+coding.systemUri+' version '+s);

      if (hasProp('abstract', true) and provider.IsAbstract(ctxt)) then
      begin
        p := resp.addProp('abstract');
        try
          p.value := Factory.makeBoolean(true);
        finally
          p.Free;
        end;
      end;
      if (hasProp('display', true)) then
        resp.display := provider.Display(ctxt, lang);
      provider.extendLookup(Factory, ctxt, lang, props, resp);
    finally
      provider.Close(ctxt);
    end;
  finally
    provider.Free;
  end;
end;

function TTerminologyServer.Link: TTerminologyServer;
begin
  result := TTerminologyServer(inherited Link);
end;

procedure TTerminologyServer.AddDependency(name, value : String);
var
  ts : TFslStringList;
begin
  // must be in lock
  if FDependencies.ExistsByKey(name) then
    ts := FDependencies.GetValueByKey(name) as TFslStringList
  else
  begin
    ts := TFslStringList.Create;
    FDependencies.Add(name, ts);
  end;
  if not ts.ExistsByValue(value) then
    ts.Add(value);
end;

procedure TTerminologyServer.invalidateVS(id: String);
var
  ts : TFslStringList;
  i : integer;
begin
  // must be in lock
  if FDependencies.ExistsByKey(id) then
  begin
    ts := FDependencies.GetValueByKey(id) as TFslStringList;
    for i := 0 to ts.Count - 1 do
      if FExpansions.ExistsByKey(ts[i]) then
        FExpansions.DeleteByKey(ts[i]);
    FDependencies.DeleteByKey(id);
  end;
  for i := FExpansions.Count - 1 downto 0 do
   if FExpansions.KeyByIndex[i].StartsWith(id+#1) then
     FExpansions.DeleteByIndex(i);
end;

function TTerminologyServer.expandVS(vs: TFhirValueSetW; cacheId : String; profile : TFHIRExpansionParams; textFilter : String; limit, count, offset : integer; txResources : TFslMetadataResourceList): TFhirValueSetW;
var
  ts : TStringList;
begin
  ts := TStringList.create;
  try
    result := expandVS(vs, cacheId, profile, textFilter, ts, limit, count, offset, txResources);
  finally
    ts.free;
  end;
end;

function TTerminologyServer.enterIntoClosure(conn: TFDBConnection; name, uri, code: String): integer;
var
  exists : boolean;
  cm : TClosureManager;
begin
  FLock.Lock;
  try
    exists := FClosures.ContainsKey(name);
    if exists then
      cm := FClosures[name]
    else
    begin
      cm := TClosureManager.create(name, 0, 0, self);
      FClosures.Add(name, cm);
    end;
    if not exists then
      cm.Init(conn);
    result := cm.enterCode(conn, uri, code);
  finally
    FLock.Unlock;
  end;
end;


function TTerminologyServer.expandVS(vs: TFhirValueSetW; cacheId : String; profile : TFHIRExpansionParams; textFilter : String; dependencies : TStringList;
    limit, count, offset : integer; txResources : TFslMetadataResourceList): TFhirValueSetW;
var
  s, d : String;
  p : TArray<String>;
  exp : TFHIRValueSetExpander;
begin
  result := nil;
  if cacheId <> '' then
  begin
    FLock.Lock('expandVS.1');
    try
      if FExpansions.ExistsByKey(cacheId+#1+profile.hash+#1+textFilter+#1+inttostr(limit)+#1+inttostr(count)+#1+inttostr(offset)) then
      begin
        result := (FExpansions.matches[cacheId+#1+profile.hash+#1+textFilter+#1+inttostr(limit)+#1+inttostr(count)+#1+inttostr(offset)] as TFhirValueSetW).link;
        p := result.Tags['cache'].Split([#1]);
        for s in p do
          if (s <> '') then
            dependencies.Add(s);
      end;
    finally
      FLock.Unlock;
    end;
  end;
  if result = nil then
  begin
    exp := TFHIRValueSetExpander.create(Factory.link, workerGetDefinition, workerGetProvider, txResources.link, workerGetExpansion, workerCanExpand);
    try
      result := exp.expand(vs, profile, textFilter, dependencies, limit, count, offset);
      if (dependencies.Count > 0) and (cacheId <> '') then
      begin
        FLock.Lock('expandVS.2');
        try
          if not FExpansions.ExistsByKey(cacheId+#1+profile.hash+#1+textFilter+#1+inttostr(limit)+#1+inttostr(count)+#1+inttostr(offset)) then
          begin
            FExpansions.Add(cacheId+#1+profile.hash+#1+textFilter+#1+inttostr(limit)+#1+inttostr(count)+#1+inttostr(offset), result.Link);
            // in addition, we trace the dependencies so we can expire the cache
            d := '';
            for s in dependencies do
            begin
              AddDependency(s, cacheId);
              d := d + s+#1;
            end;
            result.Tags['cache'] := d;
          end;
        finally
          FLock.Unlock;
        end;
      end;
    finally
      exp.Free;
    end;
  end;
end;


function TTerminologyServer.findCanonicalResources(bundle: TFHIRBundleBuilder; rType: string; url, version: String): boolean;
var
  vs : TFhirValueSetW;
  be : TFHIRBundleEntryW;
begin
  result := false;
  if rType = 'ValueSet' then
  begin
    if isKnownValueSet(url, vs) then
    begin
      be := Factory.wrapBundleEntry(factory.makeByName('Bundle.entry'));
      try
        be.resource := vs.Resource.link;
        be.Url := url;
        bundle.addEntry(be.Link, false);
      finally
        be.Free;
      end;
    end;
  end;
end;

function TTerminologyServer.expandVS(uri: String; profile : TFHIRExpansionParams; textFilter : String; limit, count, offset : integer): TFhirValueSetW;
var
  vs : TFhirValueSetW;
  ts : TStringList;
begin
  ts := TStringList.create;
  try
    vs := getValueSetByUrl(uri);
    try
      result := expandVS(vs, uri, profile, textFilter, ts, limit, count, offset.MaxValue, nil);
    finally
      vs.Free;
    end;
  finally
    ts.Free;
  end;
end;

function TTerminologyServer.expandVS(uri: String; profile : TFHIRExpansionParams; textFilter : String; dependencies: TStringList; limit, count, offset : integer): TFhirValueSetW;
var
  vs : TFhirValueSetW;
begin
  vs := getValueSetByUrl(uri);
  try
    if vs = nil then
      raise ETerminologyError.create('Unable to find value set "'+uri+'"');
    result := expandVS(vs, uri, profile, textFilter, limit, count, offset, nil);
  finally
    vs.Free;
  end;
end;


function TTerminologyServer.makeAnyValueSet: TFhirValueSetW;
var
  inc : TFhirValueSetComposeIncludeW;
begin
  result := Factory.wrapValueSet(factory.makeResource('ValueSet'));
  try
    result.url := ANY_CODE_VS;
    result.name := 'All codes known to the system';
    result.description := 'All codes known to the system';
    result.status := psActive;
    inc := result.addInclude;
    try
      inc.systemUri := ALL_CODE_CS;
    finally
      inc.Free;
    end;
    result.link;
  finally
    result.Free;
  end;
end;


function TTerminologyServer.isKnownValueSet(id: String; out vs: TFhirValueSetW): Boolean;
var
  cs : TFhirCodeSystemW;
  sn : TSnomedServices;
begin
  vs := nil;
  if id.Contains('|') then
    id := id.Substring(0, id.IndexOf('|'));

  if id.StartsWith('http://snomed.info/') then
  begin
    vs := CommonTerminologies.DefSnomed.buildValueSet(Factory, id);
    if (vs = nil) then
    begin
      for sn in CommonTerminologies.Snomed do
      begin
        vs := sn.buildValueSet(Factory, id);
        if (vs <> nil) then
          break;
      end;
    end;
  end
  else if id.StartsWith('http://loinc.org/vs/LP') or id.StartsWith('http://loinc.org/vs/LL') then
    vs := CommonTerminologies.Loinc.buildValueSet(Factory, id)
  else if id = 'http://loinc.org/vs' then
    vs := CommonTerminologies.Loinc.buildValueSet(Factory, '')
  else if id = ANY_CODE_VS then
    vs := makeAnyValueSet
  else
  begin
    cs := getCodeSystemByValueSet(id);
    if (cs <> nil) then
    begin
      try
        vs := cs.buildImplicitValueSet;
      finally
        cs.Free;
      end;
    end;
  end;

  result := vs <> nil;
end;

function TTerminologyServer.MakeChecker(uri: string; profile : TFHIRExpansionParams): TValueSetChecker;
var
  vs : TFhirValueSetW;
begin
  result := TValueSetChecker.create(Factory.link, workerGetDefinition, workerGetProvider, nil, uri);
  try
    vs := getValueSetByUrl(uri);
    try
      result.prepare(vs, profile);
    finally
      vs.Free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TTerminologyServer.validate(vs : TFhirValueSetW; coding : TFHIRCodingW; profile : TFHIRExpansionParams; abstractOk, implySystem : boolean; txResources : TFslMetadataResourceList) : TFhirParametersW;
var
  check : TValueSetChecker;
begin
  if vs = nil then
    vs := makeAnyValueSet
  else
    vs.Link;

  try
    check := TValueSetChecker.create(Factory.link, workerGetDefinition, workerGetProvider, txResources.link, vs.url);
    try
      check.prepare(vs, profile);
      result := check.check(coding, abstractOk, implySystem);
    finally
      check.Free;
    end;
  finally
    vs.Free;
  end;
end;


function TTerminologyServer.validate(vs : TFhirValueSetW; coded : TFhirCodeableConceptW; profile : TFHIRExpansionParams; abstractOk, implySystem : boolean; txResources : TFslMetadataResourceList) : TFhirParametersW;
var
  check : TValueSetChecker;
begin
  if vs = nil then
    vs := makeAnyValueSet
  else
    vs.Link;

  try
    check := TValueSetChecker.create(Factory.link, workerGetDefinition, workerGetProvider, txResources.link, vs.url);
    try
      check.prepare(vs, profile);
      result := check.check(coded, abstractOk, implySystem);
    finally
      check.Free;
   end;
  finally
    vs.Free;
  end;
end;


procedure TTerminologyServer.workerCanExpand(sender: TObject; url: String; params: TFHIRExpansionParams);
var
  vs : TFHIRValueSetW;
begin
  vs := getValueSetByUrl(url);
  try
    if vs = nil then
      raise ETerminologyError.create('Unable to find value set "'+url+'"');
  finally
    vs.Free;
  end;
end;

function TTerminologyServer.workerGetDefinition(sender: TObject; url: String): TFHIRValueSetW;
begin
  result := getValueSetByUrl(url);
end;

function TTerminologyServer.workerGetExpansion(sender: TObject; url, filter: String; params: TFHIRExpansionParams; dependencies: TStringList; limit: integer): TFHIRValueSetW;
begin
  result := expandVS(url, params, filter, dependencies, limit, 0, 0);
end;

function TTerminologyServer.workerGetProvider(sender: TObject; url, version: String; params: TFHIRExpansionParams; nullOk : boolean): TCodeSystemProvider;
begin
  result := getProvider(url, version, params, nullOk);
end;

function TTerminologyServer.cacheSize : UInt64;
begin
  result := inherited cacheSize;
  FLock.Lock;
  try
    result := result + FExpansions.sizeInBytes;
    result := result + FDependencies.sizeInBytes;
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServer.checkCode(op : TFhirOperationOutcomeW; const lang : THTTPLanguages; path : string; code : string; system, version : string; display : string) : boolean;
var
  cs : TFhirCodeSystemW;
  cp : TCodeSystemProvider;
  lct : TCodeSystemProviderContext;
  def : TFhirCodeSystemConceptW;
  d : String;
begin
  result := false;
  if (system = 'http://hl7.org/fhir/sid/icd-10') then
    result := true// nothing for now....
  else if (system = 'http://snomed.info/sct') and (CommonTerminologies.DefSnomed <> nil) then
  begin
    if op.warning('InstanceValidator', itInvalid, path, CommonTerminologies.DefSnomed.IsValidConcept(code), 'The SNOMED-CT term "'+code+'" is unknown') then
    begin
      d := CommonTerminologies.DefSnomed.GetDisplayName(code, '');
      result := op.warning('InstanceValidator', itInvalid, path, (display = '') or (display = d), 'Display for SNOMED-CT term "'+code+'" should be "'+d+'"');
    end;
  end
  else if system.StartsWith('http://loinc.org') and (CommonTerminologies.Loinc <> nil) then
  begin
    d := CommonTerminologies.Loinc.GetDisplayByName(code, CommonTerminologies.Loinc.LangsForLang(lang));
    if op.warning('InstanceValidator', itInvalid, path, d <> '', 'The LOINC code "'+code+'" is unknown') then
      result := op.warning('InstanceValidator', itInvalid, path, (display = '') or (display = d), 'Display for Loinc Code "'+code+'" should be "'+d+'"');
  end
  else if system.StartsWith('http://unitsofmeasure.org') and (CommonTerminologies.Ucum <> nil) then
  begin
    d := CommonTerminologies.Ucum.validate(code);
    result := op.warning('InstanceValidator', itInvalid, path, d = '', 'The UCUM code "'+code+'" is not valid: '+d);
    // we don't make rules about display for UCUM.
  end
  else
  begin
    cp := getProvider(system, version, nil, true);
    if cp <> nil then
    begin
      try
        lct := cp.locate(code);
        try
          if (op.error('InstanceValidator', itInvalid, path, lct <> nil, 'Unknown Code ('+system+'#'+code+')')) then
            result := op.warning('InstanceValidator', itInvalid, path, (display = '') or (display = cp.Display(lct, THTTPLanguages.create(''))),
            'Display for '+system+' code "'+code+'" should be "'+cp.Display(lct, THTTPLanguages.Create(''))+'"');
        finally
          cp.Close(lct);
        end;
      finally
        cp.Free;
      end;
    end
    else
    begin
      cs := getCodeSystem(system);
      try
        if op.warning('InstanceValidator', itInvalid, path, cs <> nil, 'Unknown Code System uri "'+system+'"') then
        begin
          def := cs.getCode(code);
          if (op.error('InstanceValidator', itInvalid, path, def <> nil, 'Unknown Code ('+system+'#'+code+')')) then
            result := op.warning('InstanceValidator', itInvalid, path, (display = '') or (display = def.Display), 'Display for '+system+' code "'+code+'" should be "'+def.Display+'"');
        end;
      finally
        cs.free;
      end;
    end;
  end;
end;

procedure TTerminologyServer.clearCache;
begin
  FLock.Lock;
  try
    FExpansions.Clear;
    FDependencies.Clear;
  finally
    FLock.UnLock;
  end;
end;

//procedure TTerminologyServer.composeCode(req: TFHIRComposeOpRequest; resp: TFHIRComposeOpResponse);
//var
//  prop : TFHIRComposeOpReqProperty_;
//  cs : TCodeSystemProvider;
//  s : string;
//  first : boolean;
//  exp : TSnomedExpression;
//  list : TFslList<TMatchingConcept>;
//  mc : TMatchingConcept;
//  match : TFHIRComposeOpRespMatch;
//  ref : TSnomedRefinementGroup;
//  unmatch : TFHIRComposeOpRespUnmatched;
//  function isValidCode(s : String) : boolean;
//  var
//    ctxt : TCodeSystemProviderContext;
//  begin
//    ctxt := cs.locate(s);
//    result := ctxt <> nil;
//    cs.Close(ctxt);
//  end;
//begin
//  cs := getProvider(req.systemUri, req.version, nil);
//  try
//    s := '';
//    for prop in req.property_List do
//    begin
//      if prop.code = 'focus' then
//        if not isValidCode(prop.value) then
//          raise ETerminologyError.create('invalid snomed value :'+prop.value)
//        else
//          s := prop.value;
//    end;
//    if s = '' then
//      raise ETerminologyError.create('no focus found');
//
//    first := true;
//    for prop in req.property_List do
//    begin
//      if prop.code <> 'focus' then
//      begin
//        if not isValidCode(prop.code) then
//          raise ETerminologyError.create('invalid snomed code :'+prop.code);
//        if not isValidCode(prop.value) then
//          raise ETerminologyError.create('invalid snomed value :'+prop.value);
//        if prop.subpropertyList.Count > 0 then
//          raise ETerminologyError.create('invalid sub-property');
//        if first then
//          s := s + ':'+ prop.code+'='+prop.value
//        else
//          s := s + ','+ prop.code+'='+prop.value;
//        first := false;
//      end;
//    end;
//    if not req.exact then
//      raise ETerminologyError.create('Only ''exact=true'' is supported at present');
//    exp := TSnomedServices(cs).parseExpression(s);
//    try
//      list := TSnomedServices(cs).condenseExpression(exp);
//      try
//        for mc in list do
//        begin
//          match := TFHIRComposeOpRespMatch.Create;
//          resp.matchList.add(match);
//          match.code := TFHIRCodingW.Create('http://snomed.info/sct', mc.matched);
//          match.code.display := cs.getDisplay(mc.matched, '');
//          for ref in mc.Unmatched do
//          begin
//            unmatch := TFHIRComposeOpRespUnmatched.Create;
//            match.unmatchedList.Add(unmatch);
//            if ref.refinements.Count = 1 then
//            begin
//              unmatch.code := ref.refinements[0].name.code;
//              unmatch.value := ref.refinements[0].value.ToString;
//            end
//            else
//            begin
//              raise ETerminologyTodo.create();
//            end;
//          end;
//        end;
//      finally
//        list.free;
//      end;
//    finally
//      exp.free;
//    end;
//  finally
//    cs.free;
//  end;
//end;

(*
function TTerminologyServer.getCodeDefinition(c : TFhirCodeSystemConceptW; code : string) : TFhirCodeSystemConceptW;
var
  i : integer;
  g : TFhirCodeSystemConceptW;
  r : TFhirCodeSystemConceptW;
begin
  result := nil;
  if (code = c.Code) then
    result := c;
  for i := 0 to c.conceptList.Count - 1 do
  begin
    g := c.conceptList[i];
    r := getCodeDefinition(g, code);
    if (r <> nil) then
    begin
      result := r;
      exit;
    end;
  end;
end;

function TTerminologyServer.getCodeDefinition(vs : TFhirCodeSystemW; code : string) : TFhirCodeSystemConceptW;
var
  i : integer;
  c : TFhirCodeSystemConceptW;
  r : TFhirCodeSystemConceptW;
begin
  result := nil;
  for c in vs.conceptList do
  begin
    r := getCodeDefinition(c, code);
    if (r <> nil) then
    begin
      result := r;
      exit;
    end;
  end;
end;
*)

function TTerminologyServer.getDisplayForCode(const lang : THTTPLanguages; system, version, code: String): String;
var
  provider : TCodeSystemProvider;
begin
  provider := getProvider(system, version, nil, true);
  if provider <> nil then
  try
    result := provider.getDisplay(code, lang);
  finally
    provider.Free;
  end;
end;

procedure TTerminologyServer.getCodeView(const lang : THTTPLanguages; coding: TFHIRCodingW; response: TCDSHookResponse);
var
  card : TCDSHookCard;
  cs : TCodeSystemProvider;
begin
  cs := getProvider(coding.systemUri, coding.version, nil, true);
  if cs <> nil then
  begin
    try
      card := response.addCard;
      cs.getCDSInfo(card, lang, webBase, coding.code, coding.display);
    finally
      cs.Free;
    end;
  end;
end;

procedure TTerminologyServer.getCodeView(const lang : THTTPLanguages; coding: TFhirCodeableConceptW; response: TCDSHookResponse);
var
  c : TFHIRCodingW;
begin
  for c in coding.codings.forEnum do
    getCodeView(lang, c, response);
end;

function TTerminologyServer.InitClosure(name: String) : String;
var
  conn : TFDBConnection;
  closure : TClosureManager;
begin
  conn := FDB.GetConnection('InitClosure');
  try
    FLock.Lock;
    try
      if FClosures.ContainsKey(name) then
        closure := FClosures[name]
      else
      begin
        closure := TClosureManager.create(name, 0, 0, self);
        FClosures.Add(name, closure);
      end;
    finally
      FLock.Unlock;
    end;
    closure.Init(conn);
    conn.Release;
  except
    on e : exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
  result := '0';
end;

function TTerminologyServer.isOkSource(cm: TLoadedConceptMap; coding: TFHIRCodingW; out group : TFhirConceptMapGroupW; out match: TFhirConceptMapGroupElementW): boolean;
var
  g : TFhirConceptMapGroupW;
  em : TFhirConceptMapGroupElementW;
begin
  result := false;
  for g in cm.Resource.groups.forEnum do
    for em in g.elements.forEnum do
      if (g.source = coding.systemUri) and (em.code = coding.code) then
      begin
        result := true;
        match := em.link;
        group := g.link;
      end;
end;

function TTerminologyServer.isOkTarget(cm : TLoadedConceptMap; vs : TFhirValueSetW) : boolean;
begin
  if cm.Target <> nil then
    result := cm.Target.url = vs.url
  else
    result := false;
  // todo: or it might be ok to use this value set if it's a subset of the specified one?
end;

function TTerminologyServer.isValidCode(system, code: String): boolean;
var
  cp : TCodeSystemProvider;
  lct : TCodeSystemProviderContext;
begin
  cp := getProvider(system, '', nil, true);
  if cp = nil then
    result := false
  else
  begin
    try
      lct := cp.locate(code);
      try
        result := lct <> nil;
      finally
        cp.Close(lct);
      end;
    finally
      cp.Free;
    end;
  end
end;

function TTerminologyServer.isOkSource(cm : TLoadedConceptMap; vs : TFhirValueSetW; coding : TFHIRCodingW; out group : TFhirConceptMapGroupW; out match : TFhirConceptMapGroupElementW) : boolean;
var
  g : TFhirConceptMapGroupW;
  em : TFhirConceptMapGroupElementW;
begin
  result := false;
  if (vs = nil) or ((cm.source <> nil) and (cm.Source.url = vs.url)) then
  begin
    for g in cm.Resource.groups.forEnum do
      for em in g.elements.forEnum do
        if (g.source = coding.systemUri) and (em.code = coding.code) then
      begin
        result := true;
        match := em.link;
        group := g.link;
      end;
  end;
end;

function TTerminologyServer.translate(const lang : THTTPLanguages; source : TFhirValueSetW; coding : TFHIRCodingW; target : TFhirValueSetW) : TFhirParametersW;
var
  op : TFhirOperationOutcomeW;
  list : TLoadedConceptMapList;
  i : integer;
  cm : TLoadedConceptMap;
  p : TFhirParametersW;
  g : TFhirConceptMapGroupW;
  em : TFhirConceptMapGroupElementW;
  map : TFhirConceptMapGroupElementTargetW;
  outcome : TFHIRCodingW;
begin
  op := Factory.wrapOperationOutcome(factory.makeResource('OperationOutcome'));
  try
    try
      if not checkCode(op, lang, '', coding.code, coding.systemUri, coding.version, coding.display) then
        raise ETerminologyError.create('Code '+coding.code+' in system '+coding.systemUri+' not recognized');

      // check to see whether the coding is already in the target value set, and if so, just return it
      p := validate(target, coding, nil, false, false, nil);
      try
        if p.bool('result') then
        begin
          result := Factory.wrapParams(factory.makeResource('Parameters'));
          result.addParamBool('result', true);
          result.addParam('outcome', coding.Link);
          result.addParamCode('equivalence', 'equal');
          exit;
        end;
      finally
        p.Free;
      end;

      result := Factory.wrapParams(factory.makeResource('Parameters'));
      list := GetConceptMapList;
      try
        for i := 0 to list.Count - 1 do
        begin
          cm := list[i];
          if isOkTarget(cm, target) and isOkSource(cm, source, coding, g, em) then
          try
            if em.targetCount = 0 then
              raise ETerminologyError.create('Concept Map has an element with no map for '+'Code '+coding.code+' in system '+coding.systemUri);
            for map in em.targets.forEnum do
            begin
              if (map.equivalence in [cmeEquivalent, cmeEqual, cmeWider, cmeSubsumes, cmeNarrower, cmeSpecializes, cmeInexact]) then
              begin
                result.addParamBool('result', true);
                outcome := factory.wrapCoding(factory.makeByName('Coding'));
                result.AddParam('outcome', outcome);
                outcome.systemUri := g.target;
                outcome.code := map.code;
                result.addParamCode('equivalence', CODES_TFHIRConceptEquivalence[map.equivalence]);
                if (map.comments <> '') then
                  result.addParamStr('message', map.comments);
                break;
              end
            end;
            exit;
          finally
            em.free;
            g.free;
          end;
        end;
      finally
        list.Free;
      end;

      result.AddParamBool('result', false);
      result.AddParamStr('message', 'no match found');
    except
      on e : exception do
      begin
        result := Factory.wrapParams(factory.makeResource('Parameters'));
        result.AddParamBool('result', false);
        result.AddParamStr('message', e.message);
      end;
    end;
  finally
    op.Free;
  end;
end;

function TTerminologyServer.translate(const lang : THTTPLanguages; source : TFhirValueSetW; coded : TFhirCodeableConceptW; target : TFhirValueSetW) : TFhirParametersW;
var
  c : TFhirCodingW;
begin
  for c in coded.codings.forEnum do
    exit(translate(lang, source, c, target));
  raise ETerminologyTodo.create('TTerminologyServer.translate');
end;

function TTerminologyServer.UseClosure(name: String; out cm: TClosureManager): boolean;
begin
  FLock.Lock;
  try
    result := FClosures.ContainsKey(name);
    if result then
      cm := FClosures[name].Link
  finally
    FLock.Unlock;
  end;
end;

(*function TTerminologyServer.translate(vs: TFhirValueSetW; coding: TFHIRCodingW; dest : String): TFhirParametersW;
var
  isCs, ok: boolean;
  i, j : integer;
  cc : TFhirCodeableConceptW;
  c : TFHIRCodingW;
  maps : TFhirConceptMapConcepttargetList;
  map : TFhirConceptMapConceptMap;
begin
  result := Factory.wrapOperationOutcome(factory.makeResource('OperationOutcome'));
  try
    if checkCode(result, '', coding.code, coding.systemUri, coding.display) then
    begin
      cc := TFhirCodeableConceptW.Create;
      try
        cc.codingList.Add(coding.Link);

        isCs := HasCodeSystem(dest);
        ok := false;

        // iterate the concept maps
        // a concept map is a match if there is a translation from source to dest
        FLock.Lock('translate');
        try
          for i := 0 to FConceptMaps.count - 1 do
          begin
            cm := FConceptMaps.values[i] as TLoadedConceptMap;
            if (cm.source <> nil) and (cm.source.url = vs.url) and
              cm.hasTranslation(coding.systemUri, coding.code, maps) then
            try
              for j := 0 to maps.Count - 1 do
              begin
                map := maps[j];
                if (map.equivalence in [ConceptEquivalenceEqual, ConceptEquivalenceEquivalent, ConceptEquivalenceWider, ConceptEquivalenceInexact]) and
                  (not isCs {if we'rea value set mapping, we'll just run with all the maps) } or (map.codeSystem = dest)) then
                begin
                  ok := true;
                  c := cc.codingList.Append;
                  c.systemUri := map.codeSystem;
                  c.code := map.code;
                  c.display := getDisplayForCode(map.codeSystem, map.code);
                  if map.comments <> '' then
                    result.hint('terminology-server', 'mapping', '', false, 'Mapping from "'+coding.systemUri+'"/"'+coding.code+'" to "'+c.systemUri+'"/"'+c.code+'": '+map.comments)
                end;
              end;
            finally
              maps.free;
            end;
          end;
          result.error('terminology-server', 'mapping', '', ok, 'No Mapping found');
        finally
          FLock.Unlock;
        end;
        if ok then
          result.AddExtension('http://hl7.org/fhir/Profile/general-extensions#translation', cc.Link);
      finally
        cc.free;
      end;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;
*)

procedure TTerminologyServer.BuildIndexesInternal(prog: boolean; conn1, conn2, conn3: TFDBConnection);
var
  i : integer;
  finish : TDateTime;
begin
  finish := now + DATETIME_SECOND_ONE * 30;
  setThreadStatus('BI: counting');
  if conn1.CountSQL('Select Count(*) from ValueSets where NeedsIndexing = 0') = 0 then
    conn1.ExecSQL('Update Concepts set NeedsIndexing = 0'); // we're going to index them all anwyay

  // first, update value set member information
  setThreadStatus('BI: Updating ValueSet Members');
  if (prog) then Logging.start('Updating ValueSet Members');
  conn1.SQL := 'Select ValueSetKey, URL from ValueSets where NeedsIndexing = 1';
  conn1.Prepare;
  conn1.Execute;
  i := 0;

  while conn1.FetchNext do
  begin
    inc(i);
    if (prog and (i mod 10 = 0)) then Logging.continue('.');
    setThreadStatus('BI: Updating ValueSet Members for '+conn1.ColStringByName['ValueSetKey']);
    processValueSet(conn1.ColIntegerByName['ValueSetKey'], conn1.ColStringByName['URL'], conn2, conn3);
    if finish < now then
      break;
  end;
  conn1.Terminate;
  if (prog) then Logging.Finish;
  if finish < now then
    exit;


  // second, for each concept that needs indexing, check it's value set information
  if (prog) then logging.start('Indexing Concepts');
  setThreadStatus('BI: Indexing Concepts');
  conn1.SQL := 'Select ConceptKey, URL, Code from Concepts where NeedsIndexing = 1';
  conn1.Prepare;
  conn1.Execute;
  i := 0;
  while conn1.FetchNext do
  begin
    inc(i);
    if (prog and (i mod 10 = 0)) then Logging.continue('.');
    setThreadStatus('BI: Indexing Concept '+conn1.ColStringByName['ConceptKey']);
    processConcept(conn1.ColIntegerByName['ConceptKey'], conn1.ColStringByName['URL'], '', conn1.ColStringByName['Code'], conn2, conn3);
    if finish < now then
      break;
  end;
  conn1.Terminate;
  if (prog) then Logging.finish;
  if finish < now then
    exit;

  // last, for each entry in the closure entry table that needs closureing, do it
  if (prog) then Logging.start('Generating Closures');
  setThreadStatus('BI: Generating Closures');
  conn1.SQL := 'select ClosureEntryKey, Closures.ClosureKey, SubsumesKey, Name, URL, Code from ClosureEntries, Concepts, Closures '+
     'where Closures.ClosureKey = ClosureEntries.ClosureKey and ClosureEntries.IndexedVersion = 0 and ClosureEntries.SubsumesKey = Concepts.ConceptKey';
  conn1.Prepare;
  conn1.Execute;
  while conn1.FetchNext do
  begin
    inc(i);
    if (prog and (i mod 100 = 0)) then logging.continue('.');
    setThreadStatus('BI: Generating Closures for '+conn1.ColStringByName['Name']);
    FClosures[conn1.ColStringByName['Name']].processEntry(conn2, conn1.ColIntegerByName['ClosureEntryKey'], conn1.ColIntegerByName['SubsumesKey'], conn1.ColStringByName['URL'], conn1.ColStringByName['Code']);
    if finish < now then
      break;
  end;
  conn1.Terminate;
  if (prog) then Logging.finish;
  if finish < now then
    exit;

  if (prog) then Logging.log('Done');
  setThreadStatus('BI: ');
end;

procedure TTerminologyServer.BuildIndexes(prog : boolean);
var
  conn1, conn2, conn3 : TFDBConnection;
begin
  if DB = nil then
    exit;

  conn1 := DB.GetConnection('BuildIndexes');
  try
    conn2 := DB.GetConnection('BuildIndexes');
    try
      conn3 := DB.GetConnection('BuildIndexes');
      try
        BuildIndexesInternal(prog, conn1, conn2, conn3);
        conn3.Release;
      except
        on e : exception do
        begin
          conn3.Error(e);
          raise;
        end;
      end;
      conn2.Release;
    except
      on e : exception do
      begin
        conn2.Error(e);
        raise;
      end;
    end;
    conn1.Release;
  except
    on e : exception do
    begin
      conn1.Error(e);
      raise;
    end;
  end;
end;

procedure TTerminologyServer.processConcept(ConceptKey: integer; URL, version, Code: String; conn2, conn3: TFDBConnection);
var
  vs : TFhirValueSetW;
  val : TValuesetChecker;
  profile : TFHIRExpansionParams;
begin
  conn2.SQL := 'select ValueSetKey, URL from ValueSets';
  conn2.Prepare;
  conn2.Execute;
  while conn2.FetchNext do
  begin
    vs := getValueSetByURL(conn2.ColStringByName['URL']);
    if vs = nil then
      conn3.ExecSQL('Update ValueSets set NeedsIndexing = 0, Error = ''Unable to find definition'' where ValueSetKey = '+conn2.ColStringByName['ValueSetKey'])
    else
      try
        profile := TFHIRExpansionParams.Create;
        try
          try
            val := TValueSetChecker.create(Factory.link, workerGetDefinition, workerGetProvider, nil, vs.url);
            try
              val.prepare(vs, profile);
              if not val.check(URL, version, code, true, false) then
                conn3.ExecSQL('Delete from ValueSetMembers where ValueSetKey = '+conn2.ColStringByName['ValueSetKey']+' and ConceptKey = '+inttostr(ConceptKey))
              else if conn3.CountSQL('select Count(*) from ValueSetMembers where ValueSetKey = '+conn2.ColStringByName['ValueSetKey']+' and ConceptKey = '+inttostr(ConceptKey)) = 0 then
                conn3.ExecSQL('insert into ValueSetMembers (ValueSetMemberKey, ValueSetKey, ConceptKey) values ('+inttostr(NextValueSetMemberKey)+','+conn2.ColStringByName['ValueSetKey']+', '+inttostr(ConceptKey)+')');
            finally
              val.Free;
            end;
          finally
            vs.Free;
          end;
        finally
          profile.free;
        end;
      except
        on e : Exception do
        begin
          conn3.ExecSQL('Update ValueSets set NeedsIndexing = 0, Error = '''+sqlwrapstring(e.Message)+''' where ValueSetKey = '+conn2.ColStringByName['ValueSetKey']);
        end;
      end;
  end;
  Conn2.Terminate;
  conn2.ExecSQL('Update Concepts set NeedsIndexing = 0 where ConceptKey = '+inttostr(ConceptKey));
end;

procedure TTerminologyServer.processValueSet(ValueSetKey: integer; URL: String; conn2, conn3: TFDBConnection);
var
  vs : TFhirValueSetW;
  val : TValuesetChecker;
  system, version, code : String;
  profile : TFHIRExpansionParams;
begin
  vs := getValueSetByURL(URL);
  if vs = nil then
    conn2.ExecSQL('Update ValueSets set NeedsIndexing = 0, Error = ''Unable to find definition'' where ValueSetKey = '+inttostr(valuesetKey))
  else
    try
      profile := TFHIRExpansionParams.defaultProfile;
      try
        try
          val := TValueSetChecker.create(Factory.link, workerGetDefinition, workerGetProvider, nil, vs.url);
          try
            val.prepare(vs, profile);
            conn2.SQL := 'select ConceptKey, URL, Code from Concepts';
            conn2.Prepare;
            try
              conn2.Execute;
              while conn2.FetchNext do
              begin
                system := conn2.ColStringByName['URL'];
                code := conn2.ColStringByName['Code'];
                if not val.check(system, version, code, true, false) then
                  conn3.ExecSQL('Delete from ValueSetMembers where ValueSetKey = '+inttostr(ValueSetKey)+' and ConceptKey = '+conn2.ColStringByName['ConceptKey'])
                else if conn3.CountSQL('select Count(*) from ValueSetMembers where ValueSetKey = '+inttostr(ValueSetKey)+' and ConceptKey = '+conn2.ColStringByName['ConceptKey']) = 0 then
                  conn3.ExecSQL('insert into ValueSetMembers (ValueSetMemberKey, ValueSetKey, ConceptKey) values ('+inttostr(NextValueSetMemberKey)+','+inttostr(ValueSetKey)+', '+conn2.ColStringByName['ConceptKey']+')');
              end;
            finally
              Conn2.Terminate;
            end;
            conn2.ExecSQL('Update ValueSets set NeedsIndexing = 0, Error = null where ValueSetKey = '+inttostr(valuesetKey));
          finally
            val.Free;
          end;
        finally
          vs.Free;
        end;
      finally
        profile.Free;
      end;
    except
      on e : Exception do
      begin
        conn2.ExecSQL('Update ValueSets set NeedsIndexing = 0, Error = '''+sqlwrapstring(e.Message)+''' where ValueSetKey = '+inttostr(valuesetKey));
      end;
    end;
end;


function TTerminologyServer.Summary: String;
var
  b  : TStringBuilder;
begin
  b := TStringBuilder.Create;
  try
    getSummary(b);
    b.append('<li>Cached Expansions : '+inttostr(FExpansions.Count)+'</li>');
    b.append('<li>Closures : '+inttostr(FClosures.Count)+'</li>');
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function TTerminologyServer.translate(const lang : THTTPLanguages; cm: TLoadedConceptMap; coding: TFHIRCodingW): TFhirParametersW;
var
  op : TFhirOperationOutcomeW;
  g : TFhirConceptMapGroupW;
  em : TFhirConceptMapGroupElementW;
  map : TFhirConceptMapGroupElementTargetW;
  outcome : TFHIRCodingW;
  p, pp :  TFhirParametersParameterW;
  prod : TFhirConceptMapGroupElementDependsOnW;
  added : boolean;
begin
  op := Factory.wrapOperationOutcome(factory.makeResource('OperationOutcome'));
  try
    try
      if not checkCode(op, lang, '', coding.code, coding.systemUri, coding.version, coding.display) then
        raise ETerminologyError.create('Code '+coding.code+' in system '+coding.systemUri+' not recognized');

//      // check to see whether the coding is already in the target value set, and if so, just return it
//      p := validate(target, coding, false);
//      try
//        if TFhirBoolean(p.NamedParameter['result']).value then
//        begin
//          result := Factory.wrapParams(factory.makeResource('Parameters'));
//          result.AddParameter('result', true);
//          result.AddParameter('outcome', coding.Link);
//          result.AddParameter('equivalence', TFhirCode.Create('equal'));
//          exit;
//        end;
//      finally
//        p.Free;
//      end;

      result := Factory.wrapParams(factory.makeResource('Parameters'));
      if isOkSource(cm, coding, g, em) then
      try
        if em.targetCount = 0 then
          raise ETerminologyError.create('Concept Map has an element with no map for '+'Code '+coding.code+' in system '+coding.systemUri);
        added := false;
        for map in em.targets.forEnum do
        begin
          if (map.equivalence in [cmeNull, cmeEquivalent, cmeEqual, cmeWider, cmeSubsumes, cmeNarrower, cmeSpecializes, cmeInexact]) then
          begin
            result.AddParamBool('result', true);
            added := true;
            outcome := Factory.wrapCoding(factory.makeByName('Coding'));
            try
              p := result.AddParam('match');
              outcome.systemUri := g.target;
              outcome.code := map.code;
              p.AddParam('match', outcome.Element.Link);
              p.addParamCode('equivalence', CODES_TFHIRConceptEquivalence[map.equivalence]);
              if (map.comments <> '') then
                p.addParamStr('message', map.comments);
              for prod in map.products.forEnum do
              begin
                pp := p.addParam('product');
                pp.addParamStr('element', prod.property_);
                pp.addParam('concept').value := Factory.makeCoding(prod.system_, prod.value);
              end;
            finally
              outcome.free;
            end;
            break;
          end;
        end;
        if not added then
        begin
          result.AddParamBool('result', false);
          result.AddParamStr('message', 'no match found');
        end;
      finally
        em.free;
        g.free;
      end;
    except
        on e : exception do
        begin
          result := Factory.wrapParams(factory.makeResource('Parameters'));
          result.AddParamBool('result', false);
          result.AddParamStr('message', e.message);
        end;
      end;
  finally
    op.Free;
  end;
end;

end.
