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

  fsl_base, fsl_utilities, fsl_collections, fsl_http, fsl_threads, fsl_i18n, fsl_logging,
  fdb_manager,
  fhir_objects, fhir_common, fhir_cdshooks, fhir_factory, fhir_features, fhir_uris,
  fhir_tx, fhir_valuesets, fhir_codesystem_service,
  session,
  ftx_service, ftx_sct_services, ftx_loinc_services, ftx_ucum_services, tx_rxnorm, tx_unii,
  ftx_lang, closuremanager, adaptations, utilities, bundlebuilder, server_stats,
  tx_manager, ftx_sct_expressions, server_constants;

Type

  { TCachedItem }

  TCachedItem = class (TFslObject)
  private
    FExpires : TDateTime;
  public
    constructor Create(expires : TDateTime);
    property expires : TDateTime read FExpires;
  end;

  { TCachedValueSet }

  TCachedValueSet = class (TCachedItem)
  private
    FValueSet : TFhirValueSetW;
  public
    constructor Create(expires : TDateTime; vs : TFhirValueSetW);
    destructor Destroy; override;
    property valueSet : TFhirValueSetW read FValueSet;
  end;

  { TCachedIds }

  TCachedIds = class (TCachedItem)
  private
    FIds : TFslStringList;
  public
    constructor Create(expires : TDateTime; ids : TFslStringList);
    destructor Destroy; override;
    property ids : TFslStringList read FIds;
  end;

  { TTerminologyServer }

  TTerminologyServer = class (TTerminologyServerStore)
  private
    FExpansions : TFslMap<TCachedValueSet>;
    FDependencies : TFslMap<TCachedIds>; // object is TFslStringList of identity
    FClosures : TFslMap<TClosureManager>;
    FOnGetCurrentRequestCount: TGetCurrentRequestCountEvent;
    FWebBase : String;
    FCaching : boolean;
    FCacheDwellTime : TDateTime;

    procedure AddDependency(name, value : String; dt : TDateTime);
//    function getCodeDefinition(c : TFhirCodeSystemConceptW; code : string) : TFhirCodeSystemConceptW; overload;
//    function getCodeDefinition(vs : TFhirCodeSystemW; code : string) : TFhirCodeSystemConceptW; overload;

    // database maintenance
    procedure processValueSet(ValueSetKey : integer; URL : String; conn2, conn3 : TFDBConnection);
    procedure processConcept(ConceptKey : integer; URL, version, Code : String; conn2, conn3 : TFDBConnection);
    procedure LoadClosures;
    procedure BuildIndexesInternal(prog : boolean; conn1, conn2, conn3 : TFDBConnection);

    function workerGetDefinition(sender : TObject; url, version : String) : TFHIRValueSetW;
    function workerGetProvider(sender : TObject; url, version : String; params : TFHIRTxOperationParams; nullOk : boolean) : TCodeSystemProvider;
    function workerGetExpansion(sender : TObject; opContext : TTerminologyOperationContext; url, version, filter : String; params : TFHIRTxOperationParams; dependencies : TStringList; additionalResources : TFslList<TFHIRCachedMetadataResource>; limit : integer; noCacheThisOne : boolean) : TFHIRValueSetW;
    procedure workerGetVersions(sender : TObject; url : String; list : TStringList);
    function handlePrepareException(e : EFHIROperationException; profile : TFHIRTxOperationParams; unknownValueSets : TStringList; url : String) : TFhirParametersW;
    procedure processCoding(coding : TFHIRCodingW; params : TFhirParametersW);
  protected
    procedure invalidateVS(id : String); override;
  public
    constructor Create(db : TFDBManager; factory : TFHIRFactory; common : TCommonTerminologies; i18n : TI18nSupport); override;
    destructor Destroy; override;
    function Link: TTerminologyServer; overload;
    property webBase : String read FWebBase write FWebBase;

    // functional services


    // given a value set, expand it
    function expandVS(vs : TFhirValueSetW; reqId, cacheId : String; profile : TFHIRTxOperationParams; textFilter : String; limit, count, offset : integer; txResources : TFslList<TFHIRCachedMetadataResource>; noCacheThisOne, diagnostics : boolean; tt : TFslTimeTracker) : TFhirValueSetW; overload;
    function expandVS(vs : TFhirValueSetW; reqId, cacheId : String; profile : TFHIRTxOperationParams; opContext : TTerminologyOperationContext; textFilter : String; limit, count, offset : integer; txResources : TFslList<TFHIRCachedMetadataResource>; noCacheThisOne, diagnostics : boolean; tt : TFslTimeTracker) : TFhirValueSetW; overload;
    function expandVS(reqId, uri, version : String; profile : TFHIRTxOperationParams; textFilter : String; limit, count, offset : integer; txResources : TFslList<TFHIRCachedMetadataResource>; noCacheThisOne, diagnostics : boolean; tt : TFslTimeTracker) : TFhirValueSetW; overload;

    // these are internal services - not for use outside the terminology server
    function expandVS(reqId, uri, version: String; profile : TFHIRTxOperationParams; textFilter : String; dependencies : TStringList; limit, count, offset : integer; txResources : TFslList<TFHIRCachedMetadataResource>; noCacheThisOne, diagnostics : boolean; tt : TFslTimeTracker) : TFhirValueSetW; overload;
    function expandVS(reqId, uri, version: String; profile : TFHIRTxOperationParams; opContext : TTerminologyOperationContext; textFilter : String; dependencies : TStringList; limit, count, offset : integer; txResources : TFslList<TFHIRCachedMetadataResource>; noCacheThisOne, diagnostics : boolean; tt : TFslTimeTracker) : TFhirValueSetW; overload;
    function expandVS(vs: TFhirValueSetW; reqId, cacheId : String; profile : TFHIRTxOperationParams; textFilter : String; dependencies : TStringList; limit, count, offset : integer; txResources : TFslList<TFHIRCachedMetadataResource>; noCacheThisOne, diagnostics : boolean; tt : TFslTimeTracker): TFhirValueSetW; overload;
    function expandVS(vs: TFhirValueSetW; reqId, cacheId : String; profile : TFHIRTxOperationParams; opContext : TTerminologyOperationContext; textFilter : String; dependencies : TStringList; limit, count, offset : integer; txResources : TFslList<TFHIRCachedMetadataResource>; noCacheThisOne, diagnostics : boolean; tt : TFslTimeTracker): TFhirValueSetW; overload;

    procedure lookupCode(coding : TFHIRCodingW; reqId : String; profile : TFHIRTxOperationParams; langList : THTTPLanguageList; props : TArray<String>; resp : TFHIRLookupOpResponseW; txResources : TFslList<TFHIRCachedMetadataResource>);
    function validate(reqId : String; vs : TFhirValueSetW; coding : TFHIRCodingW; profile : TFHIRTxOperationParams; abstractOk, inferSystem : boolean; txResources : TFslList<TFHIRCachedMetadataResource>; var summary : string; tt : TFslTimeTracker) : TFhirParametersW; overload;
    function validate(reqId, issuePath : String; vs : TFhirValueSetW; coded : TFhirCodeableConceptW; profile : TFHIRTxOperationParams; abstractOk, inferSystem: boolean; mode : TValidationCheckMode; txResources : TFslList<TFHIRCachedMetadataResource>; var summary : string; tt : TFslTimeTracker) : TFhirParametersW; overload;
    function codeInValueSet(c : TFHIRCodingW; valueSet : String) : boolean;
    function translate(langList : THTTPLanguageList; reqId : String; cml : TFslList<TFHIRConceptMapW>; coding : TFHIRCodingW; target : string; params : TFhirParametersW; txResources : TFslList<TFHIRCachedMetadataResource>; profile : TFhirTxOperationParams): TFhirParametersW; overload;
    function translate(langList : THTTPLanguageList; source : TFhirValueSetW; coding : TFHIRCodingW; target : TFhirValueSetW; params : TFhirParametersW; txResources : TFslList<TFHIRCachedMetadataResource>; profile : TFhirTxOperationParams) : TFhirParametersW; overload;
    function translate(langList : THTTPLanguageList; source : TFhirValueSetW; coded : TFhirCodeableConceptW; target : TFhirValueSetW; params : TFhirParametersW; txResources : TFslList<TFHIRCachedMetadataResource>; profile : TFhirTxOperationParams) : TFhirParametersW; overload;
    Function MakeChecker(reqId, uri, version : string; profile : TFHIRTxOperationParams; tt : TFslTimeTracker) : TValueSetChecker;
    function getDisplayForCode(langList : THTTPLanguageList; system, version, code : String): String;
    function checkCode(op : TFhirOperationOutcomeW; langList : THTTPLanguageList; path : string; code : string; system, version : string; display : string) : boolean;
    function isValidCode(system, code : String) : boolean;
    // procedure composeCode(req : TFHIRComposeOpRequest; resp : TFHIRComposeOpResponse);
    function findCanonicalResources(bundle : TFHIRBundleBuilder; rType : String; url, version : String) : boolean;

    // closures
    function InitClosure(name : String) : String;
    function UseClosure(name : String; out cm : TClosureManager) : boolean;
    function enterIntoClosure(conn : TFDBConnection; name, uri, code : String) : integer;

    procedure getCodeView(langList : THTTPLanguageList; coding : TFHIRCodingW; response : TCDSHookResponse); overload;
    procedure getCodeView(langList : THTTPLanguageList; coding : TFhirCodeableConceptW; response : TCDSHookResponse); overload;

    // database maintenance
    procedure BuildIndexes(prog : boolean);
    function Summary : String;
    procedure Unload; override;
    function cacheSize(magic : integer) : UInt64; override;
    procedure clearCache; override;
    procedure sweep;
    procedure SetCacheStatus(status : boolean); override;
    procedure getCacheInfo(ci: TCacheInformation); override;
    procedure defineFeatures(features : TFslList<TFHIRFeature>); override;
    procedure recordStats(rec : TStatusRecord); override;
    property cacheDwellTime : TDateTime read FCacheDwellTime write FCacheDwellTime;
    property OnGetCurrentRequestCount : TGetCurrentRequestCountEvent read FOnGetCurrentRequestCount write FOnGetCurrentRequestCount;
  end;

implementation

{ TCachedItem }

constructor TCachedItem.Create(expires: TDateTime);
begin
  inherited Create;
  FExpires := expires;
end;

{ TCachedIds }

constructor TCachedIds.Create(expires: TDateTime; ids: TFslStringList);
begin
  inherited Create(expires);
  FIds := ids;
end;

destructor TCachedIds.Destroy;
begin
  FIds.free;
  inherited Destroy;
end;

{ TCachedValueSet }

constructor TCachedValueSet.Create(expires: TDateTime; vs: TFhirValueSetW);
begin
  inherited Create(expires);
  FValueSet := vs;
end;

destructor TCachedValueSet.Destroy;
begin
  FValueSet.free;
  inherited Destroy;
end;


{ TTerminologyServer }

constructor TTerminologyServer.Create(db : TFDBManager; factory : TFHIRFactory; common : TCommonTerminologies; i18n : TI18nSupport);
begin
  inherited;
  FCaching := true;
  FCacheDwellTime := DEFAULT_DWELL_TIME;
  FExpansions := TFslMap<TCachedValueSet>.Create;
  FExpansions.defaultValue := nil;
  FDependencies := TFslMap<TCachedIds>.Create;
  FDependencies.defaultValue := nil;
  FClosures := TFslMap<TClosureManager>.Create('tx.closure');
  if (DB <> nil) then
    LoadClosures;
end;

procedure TTerminologyServer.defineFeatures(features: TFslList<TFHIRFeature>);
begin
  inherited;
end;

procedure TTerminologyServer.recordStats(rec: TStatusRecord);
begin
  inherited recordStats(rec);
  FLock.Lock('recordStats');
  try
    rec.ServerCacheCount := rec.ServerCacheCount + FExpansions.Count + FDependencies.Count;
    rec.ServerCacheSize := rec.ServerCacheSize + FExpansions.sizeInBytes(rec.magic) + FDependencies.sizeInBytes(rec.magic);
  finally
    FLock.Unlock;
  end;
end;

destructor TTerminologyServer.Destroy;
begin
  FClosures.free;
  FDependencies.free;
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
      FClosures.Add(conn.ColStringByName['name'], TClosureManager.Create(conn.ColStringByName['name'], conn.ColIntegerByName['ClosureKey'], conn.ColIntegerByName['Version'], self));
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

procedure TTerminologyServer.lookupCode(coding : TFHIRCodingW; reqId : String; profile : TFHIRTxOperationParams; langList : THTTPLanguageList; props : TArray<String>; resp : TFHIRLookupOpResponseW; txResources : TFslList<TFHIRCachedMetadataResource>);
var
  worker : TFHIRCodeSystemInformationProvider;
  tt : TFslTimeTracker;
begin
  tt := TFslTimeTracker.create;
  try
    worker := TFHIRCodeSystemInformationProvider.Create(Factory.link, TTerminologyOperationContext.Create(I18n.link, reqId, profile.HTTPLanguages.link, LOOKUP_DEAD_TIME_SECS, OnGetCurrentRequestCount, tt.link, Factory.version), workerGetProvider, workerGetVersions, txResources.link, CommonTerminologies.Languages.link, i18n.link);
    try
      worker.lookupCode(nil, coding, profile, props, resp);
    finally
      worker.free;
    end;
  finally
    tt.free;
  end;
end;

function TTerminologyServer.Link: TTerminologyServer;
begin
  result := TTerminologyServer(inherited Link);
end;

procedure TTerminologyServer.AddDependency(name, value : String; dt : TDateTime);
var
  ts : TFslStringList;
begin
  // must be in lock
  if FDependencies.containsKey(name) then
    ts := FDependencies[name].Ids
  else
  begin
    ts := TFslStringList.Create;
    FDependencies.AddOrSetValue(name, TCachedIds.Create(dt, ts));
  end;
  if not ts.ExistsByValue(value) then
    ts.Add(value);
end;

procedure TTerminologyServer.invalidateVS(id: String);
var
  ts : TFslStringList;
  i : integer;
  s : String;
begin
  // must be in lock
  if FDependencies.containsKey(id) then
  begin
    ts := FDependencies[id].Ids;
    for i := 0 to ts.Count - 1 do
      if FExpansions.containsKey(ts[i]) then
        FExpansions.Remove(ts[i]);
    FDependencies.remove(id);
  end;
  ts := TFslStringList.Create;
  try
    for s in FExpansions.Keys do
      if s.startsWith(id+#1) then
        ts.add(s);
    for i := 0 to ts.count - 1 do
     FExpansions.remove(ts[i]);
  finally
    ts.free;
  end;
end;

function TTerminologyServer.expandVS(vs: TFhirValueSetW; reqId, cacheId : String; profile : TFHIRTxOperationParams; textFilter : String; limit, count, offset : integer; txResources : TFslList<TFHIRCachedMetadataResource>; noCacheThisOne, diagnostics : boolean; tt : TFslTimeTracker): TFhirValueSetW;
begin
  result := expandVS(vs, reqId, cacheId, profile, nil, textFilter, limit, count, offset, txResources, noCacheThisOne, diagnostics, tt);
end;

function TTerminologyServer.expandVS(vs: TFhirValueSetW; reqId, cacheId : String; profile : TFHIRTxOperationParams; opContext : TTerminologyOperationContext;  textFilter : String; limit, count, offset : integer; txResources : TFslList<TFHIRCachedMetadataResource>; noCacheThisOne, diagnostics : boolean; tt : TFslTimeTracker): TFhirValueSetW;
var
  ts : TStringList;
begin
  ts := TStringList.Create;
  try
    result := expandVS(vs, reqId, cacheId, profile, opContext, textFilter, ts, limit, count, offset, txResources, noCacheThisOne, diagnostics, tt);
  finally
    ts.free;
  end;
end;

function TTerminologyServer.enterIntoClosure(conn: TFDBConnection; name, uri, code: String): integer;
var
  exists : boolean;
  cm : TClosureManager;
begin
  FLock.Lock('enterIntoClosure');
  try
    exists := FClosures.ContainsKey(name);
    if exists then
      cm := FClosures[name]
    else
    begin
      cm := TClosureManager.Create(name, 0, 0, self);
      FClosures.Add(name, cm);
    end;
    if not exists then
      cm.Init(conn);
    result := cm.enterCode(conn, uri, code);
  finally
    FLock.Unlock;
  end;
end;


function hashTx(list : TFslList<TFHIRCachedMetadataResource>) : String;
var
  t : TFHIRCachedMetadataResource;
  s : String;
begin
  s := '';
  for t in list do
    s := s + t.resource.Url+'|'+t.resource.version+#1;
  result := inttostr(HashStringToCode32(s));
end;

function TTerminologyServer.expandVS(vs: TFhirValueSetW; reqId, cacheId : String; profile : TFHIRTxOperationParams; textFilter : String; dependencies : TStringList;
    limit, count, offset : integer; txResources : TFslList<TFHIRCachedMetadataResource>; noCacheThisOne, diagnostics : boolean; tt : TFslTimeTracker): TFhirValueSetW;
begin
  result := expandVS(vs, reqId, cacheId, profile, nil, textFilter, dependencies, limit, count, offset, txResources, noCacheThisOne, diagnostics, tt) ;
end;

function TTerminologyServer.expandVS(vs: TFhirValueSetW; reqId, cacheId : String; profile : TFHIRTxOperationParams; opContext : TTerminologyOperationContext; textFilter : String; dependencies : TStringList;
    limit, count, offset : integer; txResources : TFslList<TFHIRCachedMetadataResource>; noCacheThisOne, diagnostics : boolean; tt : TFslTimeTracker): TFhirValueSetW;
var
  s, d, key: String;
  p : TArray<String>;
  exp : TFHIRValueSetExpander;
  dt : TDateTime;
begin
  result := nil;
  if (dependencies.Count > 0) and not noCacheThisOne and FCaching then
  begin
    key := cacheId+#1+profile.hash+#1+textFilter+#1+inttostr(limit)+#1+inttostr(count)+#1+inttostr(offset)+#1+inttostr(offset)+#1+hashTx(txResources);
    FLock.Lock('expandVS.1');
    try
      if FExpansions.containsKey(key) then
      begin
        result := FExpansions[key].valueSet.link;
        p := result.Tags['cache'].Split([#1]);
        for s in p do
          if (s <> '') then
            dependencies.Add(s);
      end;
    finally
      FLock.Unlock;
    end;
  end;
  tt.step('not in cache');
  if result = nil then
  begin
    if opContext = nil then
      exp := TFHIRValueSetExpander.Create(Factory.link, TTerminologyOperationContext.Create(I18n.link, reqId, profile.HTTPLanguages.link, EXPANSION_DEAD_TIME_SECS, OnGetCurrentRequestCount, tt.link, Factory.version), workerGetDefinition, workerGetProvider, workerGetVersions, workerGetExpansion, txResources.link, CommonTerminologies.Languages.link, i18n.link)
    else
      exp := TFHIRValueSetExpander.Create(Factory.link, opContext.copy, workerGetDefinition, workerGetProvider, workerGetVersions, workerGetExpansion, txResources.link, CommonTerminologies.Languages.link, i18n.link);
    try
      result := exp.expand(vs, profile, textFilter, dependencies, limit, count, offset, noCacheThisOne);
      if (dependencies.Count > 0) and not noCacheThisOne and FCaching and (vs.TagInt = 0) then
      begin
        if FCaching then
        begin
          tt.step('add to cache');
          FLock.Lock('expandVS.2');
          try
            dt := now + FCacheDwellTime;
            if not FExpansions.ContainsKey(key) then
            begin
              FExpansions.AddOrSetValue(key, TCachedValueSet.Create(dt, result.clone));
              // in addition, we trace the dependencies so we can expire the cache
              d := '';
              for s in dependencies do
              begin
                AddDependency(s, cacheId, dt);
                d := d + s+#1;
              end;
              result.Tags['cache'] := d;
            end;
          finally
            FLock.Unlock;
          end;
        end;
        tt.step('added to cache');
      end;
      if (diagnostics) then
        result.expansion.addParamStr('diagnostics', exp.opContext.diagnostics);
    finally
      exp.free;
    end;
  end;
end;


function TTerminologyServer.findCanonicalResources(bundle: TFHIRBundleBuilder;
  rType: String; url, version: String): boolean;
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
        be.free;
      end;
    end;
  end;
end;

function TTerminologyServer.expandVS(reqId, uri, version: String; profile : TFHIRTxOperationParams; textFilter : String; limit, count, offset : integer; txResources : TFslList<TFHIRCachedMetadataResource>; noCacheThisOne, diagnostics : boolean; tt : TFslTimeTracker): TFhirValueSetW;
var
  vs : TFhirValueSetW;
  ts : TStringList;
begin
  ts := TStringList.Create;
  try
    vs := getValueSetByUrl(uri, version, txResources);
    try
      result := expandVS(vs, reqId, uri, profile, nil, textFilter, ts, limit, count, offset.MaxValue, txResources, noCacheThisOne, diagnostics, tt);
    finally
      vs.free;
    end;
  finally
    ts.free;
  end;
end;

function TTerminologyServer.expandVS(reqId, uri, version: String; profile : TFHIRTxOperationParams; textFilter : String; dependencies: TStringList; limit, count, offset : integer; txResources : TFslList<TFHIRCachedMetadataResource>; noCacheThisOne, diagnostics : boolean; tt : TFslTimeTracker): TFhirValueSetW;
begin
  result := expandVS(reqId, uri, version, profile, nil, textFilter, dependencies, limit, count, offset, txResources, noCacheThisOne, diagnostics, tt);
end;

function TTerminologyServer.expandVS(reqId, uri, version: String; profile : TFHIRTxOperationParams; opContext : TTerminologyOperationContext;  textFilter : String; dependencies: TStringList; limit, count, offset : integer; txResources : TFslList<TFHIRCachedMetadataResource>; noCacheThisOne, diagnostics : boolean; tt : TFslTimeTracker): TFhirValueSetW;
var
  vs : TFhirValueSetW;
begin
  vs := getValueSetByUrl(uri, version, txResources);
  try
    if vs = nil then
      raise ETerminologyError.Create('Unable to find value set "'+uri+'"', itUnknown);
    result := expandVS(vs, reqId, uri, profile, opContext, textFilter, limit, count, offset, txResources, noCacheThisOne, diagnostics, tt);
  finally
    vs.free;
  end;
end;



function TTerminologyServer.MakeChecker(reqId, uri, version: string; profile : TFHIRTxOperationParams; tt : TFslTimeTracker): TValueSetChecker;
var
  vs : TFhirValueSetW;
begin
  result := TValueSetChecker.Create(Factory.link, TTerminologyOperationContext.Create(I18n.link, reqId, profile.HTTPLanguages.link, VALIDATION_DEAD_TIME_SECS, OnGetCurrentRequestCount, tt.link, Factory.version), workerGetDefinition, workerGetProvider, workerGetVersions, workerGetExpansion, nil, CommonTerminologies.Languages.link, uri, i18n.link);
  try
    vs := getValueSetByUrl(uri, version);
    try
      result.prepare(vs, profile, nil);
    finally
      vs.free;
    end;
    result.Link;
  finally
    result.free;
  end;
end;

function TTerminologyServer.handlePrepareException(e : EFHIROperationException; profile : TFHIRTxOperationParams; unknownValueSets : TStringList; url : String) : TFhirParametersW;
var
  op : TFhirOperationOutcomeW;
begin
  result := factory.makeParameters;
  try
    op := Factory.wrapOperationOutcome(Factory.buildOperationOutcome(i18n, profile.HTTPLanguages, e));
    try
      result.addParam('issues').resource := op.Resource.link;
      result.addParamBool('result', false);
      result.addParamStr('message', e.message); // +'; '+msg);
    finally
      op.free;
    end;
    exit(result.link);
  finally
    result.free;
  end;
end;

procedure TTerminologyServer.processCoding(coding : TFHIRCodingW; params : TFhirParametersW);
begin
  if coding.systemUri <> '' then
    params.addParamUri('system', coding.systemUri);
  if coding.version <> '' then
    params.addParamStr('version', coding.version);
  if coding.code <> '' then
    params.addParamCode('code', coding.code);
  if coding.display <> '' then
    params.addParamStr('display', coding.display);
end;

function TTerminologyServer.validate(reqId : String; vs : TFhirValueSetW; coding : TFHIRCodingW; profile : TFHIRTxOperationParams; abstractOk, inferSystem : boolean; txResources : TFslList<TFHIRCachedMetadataResource>; var summary : string; tt : TFslTimeTracker) : TFhirParametersW;
var
  check : TValueSetChecker;
  unknownValueSets : TStringList;
begin
  if vs = nil then
    vs := makeAnyValueSet
  else
    vs.Link;

  try
    unknownValueSets := TStringList.create;
    check := TValueSetChecker.Create(Factory.link, TTerminologyOperationContext.Create(I18n.link, reqId, profile.HTTPLanguages.link, VALIDATION_DEAD_TIME_SECS, OnGetCurrentRequestCount, tt.link, Factory.version), workerGetDefinition, workerGetProvider, workerGetVersions, workerGetExpansion, txResources.link, CommonTerminologies.Languages.link, vs.url, i18n.link);
    try
      unknownValueSets.Sorted := true;
      unknownValueSets.Duplicates := dupIgnore;
      try
        check.prepare(vs, profile, unknownValueSets);
      except
        on e : EFHIROperationException do
        begin
          result := handlePrepareException(e, profile, unknownValueSets, vs.vurl);
          try
            processCoding(coding, result);
            exit(result.link);
          finally
            result.free;
          end;
        end;
        on e : Exception do
          raise;
      end;
      result := check.check('Coding', coding, abstractOk, inferSystem);
      summary := check.log;
    finally
      check.free;
      unknownValueSets.free;
    end;
  finally
    vs.free;
  end;
end;


function TTerminologyServer.validate(reqId, issuePath : String; vs : TFhirValueSetW; coded : TFhirCodeableConceptW; profile : TFHIRTxOperationParams; abstractOk, inferSystem : boolean; mode : TValidationCheckMode; txResources : TFslList<TFHIRCachedMetadataResource>; var summary : string; tt : TFslTimeTracker) : TFhirParametersW;
var
  check : TValueSetChecker;
  coding : TFhirCodingW; 
  unknownValueSets : TStringList;
  op : TFhirOperationOutcomeW;
  msg : String;
begin
  if vs = nil then
    vs := makeAnyValueSet
  else
    vs.Link;

  try
    unknownValueSets := TStringList.create;
    check := TValueSetChecker.Create(Factory.link, TTerminologyOperationContext.Create(I18n.link, reqId, profile.HTTPLanguages.link, VALIDATION_DEAD_TIME_SECS, OnGetCurrentRequestCount, tt.link, Factory.version), workerGetDefinition, workerGetProvider, workerGetVersions, workerGetExpansion, txResources.link, CommonTerminologies.Languages.link, vs.url, i18n.link);
    try
      unknownValueSets.Sorted := true;
      unknownValueSets.Duplicates := dupIgnore;
      try
        check.prepare(vs, profile, unknownValueSets);
      except
        on e : EFHIROperationException do
        begin
          result := handlePrepareException(e, profile, unknownValueSets, vs.vurl);
          try
            if mode = vcmCodeableConcept then
                result.addParam('codeableConcept').value := coded.Element.link
            else
              for coding in coded.codings.forEnum do // there'll only be one, but this handles freeing the object
                processCoding(coding, result);
            exit(result.link);
          finally
            result.free;
          end;
        end;
        on e : Exception do
          raise;
      end;
      result := check.check(issuePath, coded, abstractOk, inferSystem, mode);
      summary := check.log;
      if profile.diagnostics then
        result.addParamStr('diagnostics', check.opContext.diagnostics);
    finally
      check.free;
   end;
  finally
    vs.free;
  end;
end;

function TTerminologyServer.workerGetDefinition(sender: TObject; url, version: String): TFHIRValueSetW;
begin
  result := getValueSetByUrl(url, version);
end;

function TTerminologyServer.workerGetExpansion(sender: TObject; opContext : TTerminologyOperationContext; url, version, filter: String; params: TFHIRTxOperationParams; dependencies: TStringList; additionalResources : TFslList<TFHIRCachedMetadataResource>; limit: integer; noCacheThisOne : boolean): TFHIRValueSetW;
begin
  result := expandVS(opContext.reqId, url, version, params, opContext, filter, dependencies, limit, -1, -1, additionalResources, noCacheThisOne, false, opContext.TimeTracker);
end;

function TTerminologyServer.workerGetProvider(sender: TObject; url, version: String; params: TFHIRTxOperationParams; nullOk : boolean): TCodeSystemProvider;
begin
  result := getProvider(url, version, params, nullOk);
end;

procedure TTerminologyServer.workerGetVersions(sender: TObject; url: String; list: TStringList);
begin
  listVersions(url, list);
end;

function TTerminologyServer.cacheSize(magic : integer) : UInt64;
begin
  result := inherited cacheSize(magic);
  FLock.Lock('cacheSize');
  try
    result := result + FExpansions.sizeInBytes(magic);
    result := result + FDependencies.sizeInBytes(magic);
  finally
    FLock.Unlock;
  end;
end;

function TTerminologyServer.checkCode(op : TFhirOperationOutcomeW; langList : THTTPLanguageList; path : string; code : string; system, version : string; display : string) : boolean;
var
  cs : TFhirCodeSystemEntry;
  cp : TCodeSystemProvider;
  lct : TCodeSystemProviderContext;
  def : TFhirCodeSystemConceptW;
  d : String;
begin
  result := false;
  if (system = URI_ICD10) then
    result := true// nothing for now....
  else if (system = URI_SNOMED) and (CommonTerminologies.DefSnomed <> nil) then
  begin
    if op.warning('InstanceValidator', itInvalid, path, CommonTerminologies.DefSnomed.IsValidConcept(code), 'The SNOMED-CT term "'+code+'" is unknown') then
    begin
      d := CommonTerminologies.DefSnomed.GetDisplayName(code, '');
      result := op.warning('InstanceValidator', itInvalid, path, (display = '') or (display = d), 'Display for SNOMED-CT term "'+code+'" should be "'+d+'"');
    end;
  end
  else if system.StartsWith(URI_LOINC) and (CommonTerminologies.Loinc <> nil) then
  begin
    d := CommonTerminologies.Loinc.getDisplay(nil, code, langList);
    if op.warning('InstanceValidator', itInvalid, path, d <> '', 'The LOINC code "'+code+'" is unknown') then
      result := op.warning('InstanceValidator', itInvalid, path, (display = '') or (display = d), 'Display for Loinc Code "'+code+'" should be "'+d+'"');
  end
  else if system.StartsWith(URI_UCUM) and (CommonTerminologies.Ucum <> nil) then
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
        lct := cp.locate(nil, code);
        try
          if (op.error('InstanceValidator', itInvalid, path, lct <> nil, 'Unknown Code ('+system+'#'+code+')')) then
            result := op.warning('InstanceValidator', itInvalid, path, (display = '') or (display = cp.Display(nil, lct, THTTPLanguageList(nil))),
            'Display for '+system+' code "'+code+'" should be "'+cp.Display(nil, lct, THTTPLanguageList(nil))+'"');
        finally
          lct.free;
        end;
      finally
        cp.free;
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
  inherited ClearCache;
  FLock.Lock('clearCache');
  try
    FExpansions.Clear;
    FDependencies.Clear;
  finally
    FLock.UnLock;
  end;
end;

procedure TTerminologyServer.sweep;
var
  ts : TStringList;
  s : String;
  dt : TDateTime;
begin
  ts := TStringList.Create;
  try
    Flock.Lock('sweep');
    try
      dt := now;
      for s in FExpansions.Keys do
        if FExpansions[s].expires < dt then
          ts.add(s);
      for s in ts do
        FExpansions.remove(s);
      ts.clear;
      for s in FDependencies.Keys do
        if FDependencies[s].expires < dt then
          ts.add(s);
      for s in ts do
        FDependencies.remove(s);
    finally
      FLock.Unlock;
    end;
  finally
    ts.free;
  end;
end;

function TTerminologyServer.codeInValueSet(c : TFHIRCodingW; valueSet: String): boolean;
var
  vs : TFHIRValueSetW;
  p : TFhirParametersW;
  profile : TFHIRTxOperationParams;
  summary : string;
  tt : TFslTimeTracker;
begin
  tt := TFslTimeTracker.create;
  vs := getValueSetByUrl(valueSet, '', nil);
  try
    if (vs = nil) then
      exit(false);
    profile := TFHIRTxOperationParams.Create(CommonTerminologies.Languages.link);
    try
      profile.membershipOnly := true;
      p := validate('', vs, c, profile, true, false, nil, summary, tt);
      try
        result := p.bool('result');
      finally
        p.free;
      end;
    finally
      profile.free;
    end;
  finally
    vs.free;
    tt.free;
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
//          raise ETerminologyError.Create('invalid snomed value :'+prop.value)
//        else
//          s := prop.value;
//    end;
//    if s = '' then
//      raise ETerminologyError.Create('no focus found');
//
//    first := true;
//    for prop in req.property_List do
//    begin
//      if prop.code <> 'focus' then
//      begin
//        if not isValidCode(prop.code) then
//          raise ETerminologyError.Create('invalid snomed code :'+prop.code);
//        if not isValidCode(prop.value) then
//          raise ETerminologyError.Create('invalid snomed value :'+prop.value);
//        if prop.subpropertyList.Count > 0 then
//          raise ETerminologyError.Create('invalid sub-property');
//        if first then
//          s := s + ':'+ prop.code+'='+prop.value
//        else
//          s := s + ','+ prop.code+'='+prop.value;
//        first := false;
//      end;
//    end;
//    if not req.exact then
//      raise ETerminologyError.Create('Only ''exact=true'' is supported at present');
//    exp := TSnomedServices(cs).parseExpression(s);
//    try
//      list := TSnomedServices(cs).condenseExpression(exp);
//      try
//        for mc in list do
//        begin
//          match := TFHIRComposeOpRespMatch.Create;
//          resp.matchList.add(match);
//          match.code := TFHIRCodingW.Create(URI_SNOMED, mc.matched);
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
//              raise ETerminologyTodo.Create();
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

function TTerminologyServer.getDisplayForCode(langList : THTTPLanguageList; system, version, code: String): String;
var
  provider : TCodeSystemProvider;
begin
  provider := getProvider(system, version, nil, true);
  if provider <> nil then
  try
    result := provider.getDisplay(nil, code, langList);
  finally
    provider.free;
  end;
end;

procedure TTerminologyServer.getCodeView(langList : THTTPLanguageList; coding: TFHIRCodingW; response: TCDSHookResponse);
var
  card : TCDSHookCard;
  cs : TCodeSystemProvider;
begin
  cs := getProvider(coding.systemUri, coding.version, nil, true);
  if cs <> nil then
  begin
    try
      card := response.addCard;
      cs.getCDSInfo(nil, card, langList, webBase, coding.code, coding.display);
    finally
      cs.free;
    end;
  end;
end;

procedure TTerminologyServer.getCacheInfo(ci: TCacheInformation);
begin
  inherited;
  ci.Add('Expansions', FExpansions.sizeInBytes(ci.magic));
  ci.Add('Dependencies', FDependencies.sizeInBytes(ci.magic));
end;

procedure TTerminologyServer.getCodeView(langList : THTTPLanguageList; coding: TFhirCodeableConceptW; response: TCDSHookResponse);
var
  c : TFHIRCodingW;
begin
  for c in coding.codings.forEnum do
    getCodeView(langList, c, response);
end;

function TTerminologyServer.InitClosure(name: String) : String;
var
  conn : TFDBConnection;
  closure : TClosureManager;
begin
  conn := FDB.GetConnection('InitClosure');
  try
    FLock.Lock('InitClosure');
    try
      if FClosures.ContainsKey(name) then
        closure := FClosures[name]
      else
      begin
        closure := TClosureManager.Create(name, 0, 0, self);
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
      lct := cp.locate(nil, code);
      try
        result := lct <> nil;
      finally
        lct.free;
      end;
    finally
      cp.free;
    end;
  end
end;

function TTerminologyServer.translate(langList : THTTPLanguageList; source : TFhirValueSetW; coding : TFHIRCodingW; target : TFhirValueSetW; params : TFhirParametersW; txResources : TFslList<TFHIRCachedMetadataResource>; profile : TFhirTxOperationParams) : TFhirParametersW;
//var
//  op : TFhirOperationOutcomeW;
//  list : TLoadedConceptMapList;
//  i : integer;
//  summary : string;
//  cm : TLoadedConceptMap;
//  p : TFhirParametersW;
//  g : TFhirConceptMapGroupW;
//  em : TFhirConceptMapGroupElementW;
//  map : TFhirConceptMapGroupElementTargetW;
//  outcome : TFHIRCodingW;
begin
  raise ETerminologyTodo.Create('TTerminologyServer.translate');
  //op := Factory.wrapOperationOutcome(factory.makeResource('OperationOutcome'));
  //try
  //  try
  //    if not checkCode(op, langList, '', coding.code, coding.systemUri, coding.version, coding.display) then
  //      raise ETerminologyError.Create('Code '+coding.code+' in system '+coding.systemUri+' not recognized', itUnknown);
  //
  //    // check to see whether the coding is already in the target value set, and if so, just return it
  //    p := validate('', target, coding, nil, false, false, nil, summary);
  //    try
  //      if p.bool('result') then
  //      begin
  //        result := Factory.wrapParams(factory.makeResource('Parameters'));
  //        result.addParamBool('result', true);
  //        result.addParam('outcome', coding.Link);
  //        result.addParamCode('equivalence', 'equal');
  //        exit;
  //      end;
  //    finally
  //      p.free;
  //    end;
  //
  //    result := Factory.wrapParams(factory.makeResource('Parameters'));
  //    list := GetConceptMapList;
  //    try
  //      for i := 0 to list.Count - 1 do
  //      begin
  //        cm := list[i];
  //        if isOkTarget(cm, target) and isOkSource(cm, source, coding, g, em) then
  //        try
  //          if em.targetCount = 0 then
  //            raise ETerminologyError.Create('Concept Map has an element with no map for '+'Code '+coding.code+' in system '+coding.systemUri, itInvalid);
  //          for map in em.targets.forEnum do
  //          begin
  //            if (map.equivalence in [cmeEquivalent, cmeEqual, cmeWider, cmeSubsumes, cmeNarrower, cmeSpecializes, cmeInexact]) then
  //            begin
  //              result.addParamBool('result', true);
  //              outcome := factory.wrapCoding(factory.makeByName('Coding'));
  //              result.AddParam('outcome', outcome);
  //              outcome.systemUri := g.target;
  //              outcome.code := map.code;
  //              result.addParamCode('equivalence', CODES_TFHIRConceptEquivalence[map.equivalence]);
  //              if (map.comments <> '') then
  //                result.addParamStr('message', map.comments);
  //              break;
  //            end
  //          end;
  //          exit;
  //        finally
  //          em.free;
  //          g.free;
  //        end;
  //      end;
  //    finally
  //      list.free;
  //    end;
  //
  //    result.AddParamBool('result', false);
  //    result.AddParamStr('message', 'no match found');
  //  except
  //    on e : exception do
  //    begin
  //      result := Factory.wrapParams(factory.makeResource('Parameters'));
  //      result.AddParamBool('result', false);
  //      result.AddParamStr('message', e.message);
  //    end;
  //  end;
  //finally
  //  op.free;
  //end;
end;

function TTerminologyServer.translate(langList : THTTPLanguageList; source : TFhirValueSetW; coded : TFhirCodeableConceptW; target : TFhirValueSetW; params : TFhirParametersW; txResources : TFslList<TFHIRCachedMetadataResource>; profile : TFhirTxOperationParams) : TFhirParametersW;
//var
//  c : TFhirCodingW;
begin
  //for c in coded.codings.forEnum do
  //  exit(translate(langList, source, c, target, params, txResources, profile));
  raise ETerminologyTodo.Create('TTerminologyServer.translate');
end;

function TTerminologyServer.UseClosure(name: String; out cm: TClosureManager): boolean;
begin
  FLock.Lock('UseClosure');
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
    result.free;
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
  profile : TFHIRTxOperationParams;
  tt : TFslTimeTracker;
begin
  conn2.SQL := 'select ValueSetKey, URL from ValueSets';
  conn2.Prepare;
  conn2.Execute;
  while conn2.FetchNext do
  begin
    vs := getValueSetByURL(conn2.ColStringByName['URL'], '');
    if vs = nil then
      conn3.ExecSQL('Update ValueSets set NeedsIndexing = 0, Error = ''Unable to find definition'' where ValueSetKey = '+conn2.ColStringByName['ValueSetKey'])
    else
      try
        tt := TFslTimeTracker.create;
        profile := TFHIRTxOperationParams.Create(CommonTerminologies.Languages.link);
        try
          try
            val := TValueSetChecker.Create(Factory.link, TTerminologyOperationContext.Create(I18n.link, '', profile.HTTPLanguages.link, VALIDATION_DEAD_TIME_SECS, OnGetCurrentRequestCount, tt.link, Factory.version), workerGetDefinition, workerGetProvider, workerGetVersions, workerGetExpansion, nil, CommonTerminologies.Languages.link, vs.url, i18n.link);
            try
              val.prepare(vs, profile, nil);
              if val.check('code', URL, version, code, true, false, nil) <> bTrue then
                conn3.ExecSQL('Delete from ValueSetMembers where ValueSetKey = '+conn2.ColStringByName['ValueSetKey']+' and ConceptKey = '+inttostr(ConceptKey))
              else if conn3.CountSQL('select Count(*) from ValueSetMembers where ValueSetKey = '+conn2.ColStringByName['ValueSetKey']+' and ConceptKey = '+inttostr(ConceptKey)) = 0 then
                conn3.ExecSQL('insert into ValueSetMembers (ValueSetMemberKey, ValueSetKey, ConceptKey) values ('+inttostr(NextValueSetMemberKey)+','+conn2.ColStringByName['ValueSetKey']+', '+inttostr(ConceptKey)+')');
            finally
              val.free;
            end;
          finally
            vs.free;
          end;
        finally
          profile.free;
          tt.free;
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
  profile : TFHIRTxOperationParams;
  tt : TFslTimeTracker;
begin
  vs := getValueSetByURL(URL, '');
  if vs = nil then
    conn2.ExecSQL('Update ValueSets set NeedsIndexing = 0, Error = ''Unable to find definition'' where ValueSetKey = '+inttostr(valuesetKey))
  else
    try
      tt := TFslTimeTracker.create;;
      profile := TFHIRTxOperationParams.defaultProfile(CommonTerminologies.Languages.link);
      try
        try
          val := TValueSetChecker.Create(Factory.link, TTerminologyOperationContext.Create(I18n.link, '', profile.HTTPLanguages.link, VALIDATION_DEAD_TIME_SECS, OnGetCurrentRequestCount, tt.link, Factory.version), workerGetDefinition, workerGetProvider, workerGetVersions, workerGetExpansion, nil, CommonTerminologies.Languages.link, vs.url, i18n.link);
          try
            val.prepare(vs, profile, nil);
            conn2.SQL := 'select ConceptKey, URL, Code from Concepts';
            conn2.Prepare;
            try
              conn2.Execute;
              while conn2.FetchNext do
              begin
                system := conn2.ColStringByName['URL'];
                code := conn2.ColStringByName['Code'];
                if val.check('code', system, version, code, true, false, nil) <> bTrue then
                  conn3.ExecSQL('Delete from ValueSetMembers where ValueSetKey = '+inttostr(ValueSetKey)+' and ConceptKey = '+conn2.ColStringByName['ConceptKey'])
                else if conn3.CountSQL('select Count(*) from ValueSetMembers where ValueSetKey = '+inttostr(ValueSetKey)+' and ConceptKey = '+conn2.ColStringByName['ConceptKey']) = 0 then
                  conn3.ExecSQL('insert into ValueSetMembers (ValueSetMemberKey, ValueSetKey, ConceptKey) values ('+inttostr(NextValueSetMemberKey)+','+inttostr(ValueSetKey)+', '+conn2.ColStringByName['ConceptKey']+')');
              end;
            finally
              Conn2.Terminate;
            end;
            conn2.ExecSQL('Update ValueSets set NeedsIndexing = 0, Error = null where ValueSetKey = '+inttostr(valuesetKey));
          finally
            val.free;
          end;
        finally
          vs.free;
        end;
      finally
        profile.free;
        tt.free;
      end;
    except
      on e : Exception do
      begin
        conn2.ExecSQL('Update ValueSets set NeedsIndexing = 0, Error = '''+sqlwrapstring(e.Message)+''' where ValueSetKey = '+inttostr(valuesetKey));
      end;
    end;
end;


procedure TTerminologyServer.SetCacheStatus(status: boolean);
begin
  inherited;
  FCaching := status;
end;

function TTerminologyServer.Summary: String;
var
  b  : TFslStringBuilder;
begin
  b := TFslStringBuilder.Create;
  try
    getSummary(b);
    b.append('<li>Cached Expansions : '+inttostr(FExpansions.Count)+'</li>');
    b.append('<li>Closures : '+inttostr(FClosures.Count)+'</li>');
    result := b.ToString;
  finally
    b.free;
  end;
end;

procedure TTerminologyServer.Unload;
begin
  inherited Unload;
end;

function TTerminologyServer.translate(langList : THTTPLanguageList; reqId : String; cml : TFslList<TFHIRConceptMapW>; coding: TFHIRCodingW; target : string; params : TFhirParametersW; txResources : TFslList<TFHIRCachedMetadataResource>; profile : TFhirTxOperationParams): TFhirParametersW;
var
  worker : TFHIRConceptMapTranslator;
  tt : TFslTimeTracker;
begin
  tt := TFslTimeTracker.create;
  worker := TFHIRConceptMapTranslator.Create(Factory.link, TTerminologyOperationContext.Create(I18n.link, reqId, profile.HTTPLanguages.link, LOOKUP_DEAD_TIME_SECS, OnGetCurrentRequestCount, tt.link, Factory.version), workerGetProvider, workerGetVersions, txResources.link, CommonTerminologies.Languages.link, i18n.link);
  try
    result := worker.translate(langList, reqId, cml, coding, target, params, profile);
  finally
    worker.free;
    tt.free;
  end;
end;

end.
