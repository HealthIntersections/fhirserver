{
todo:
 - support for descendent-of, ValueSet.compose.inactive

}
unit TerminologyServer;

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
  SysUtils, Classes, IniFiles, Generics.Collections,

  StringSupport, DateSupport,
  AdvObjects, AdvStringObjectMatches, AdvStringLists, AdvGenerics,
  KDBManager,
  FHIRTypes, FHIRResources, FHIRUtilities, CDSHooksUtilities, FHIROperations,
  TerminologyServices, SnomedServices, LoincServices, UcumServices, RxNormServices, UniiServices, ACIRServices, ICD10Services,
  IETFLanguageCodeServices, FHIRValueSetChecker, ClosureManager, ServerAdaptations, ServerUtilities,
  TerminologyServerStore, SnomedExpressions;

Type
  TDataBaseConnectionEvent = function (name : String; max, timeout : integer; driver, server, database, username, password : String; forCreate : boolean) : TKDBManager of object;

  TTerminologyServer = class (TTerminologyServerStore)
  private
    FExpansions : TAdvStringObjectMatch;
    FDependencies : TAdvStringObjectMatch; // object is TAdvStringList of identity
    FClosures : TAdvMap<TClosureManager>;
    FWebBase : String;
    FOnConnectDB : TDataBaseConnectionEvent;

    procedure AddDependency(name, value : String);
    function getCodeDefinition(c : TFhirCodeSystemConcept; code : string) : TFhirCodeSystemConcept; overload;
    function getCodeDefinition(vs : TFhirCodeSystem; code : string) : TFhirCodeSystemConcept; overload;
    function makeAnyValueSet: TFhirValueSet;

    // database maintenance
    procedure processValueSet(ValueSetKey : integer; URL : String; conn2, conn3 : TKDBConnection);
    procedure processConcept(ConceptKey : integer; URL, version, Code : String; conn2, conn3 : TKDBConnection);
    function isOkTarget(cm: TLoadedConceptMap; vs: TFHIRValueSet): boolean;
    function isOkSource(cm: TLoadedConceptMap; vs: TFHIRValueSet; coding: TFHIRCoding; out group : TFhirConceptMapGroup; out match : TFhirConceptMapGroupElement): boolean; overload;
    function isOkSource(cm: TLoadedConceptMap; coding: TFHIRCoding; out group : TFhirConceptMapGroup; out match : TFhirConceptMapGroupElement): boolean; overload;
    procedure LoadClosures;
    procedure BuildIndexesInternal(prog : boolean; conn1, conn2, conn3 : TKDBConnection);
  protected
    procedure invalidateVS(id : String); override;
  public
    constructor Create(db : TKDBManager); override;
    Destructor Destroy; override;
    function Link: TTerminologyServer; overload;
    property webBase : String read FWebBase write FWebBase;

    // load external terminology resources (snomed, Loinc, etc)
    procedure load(ini : TFHIRServerIniFile);
    // functional services

    // if this id identifies a value set known to the external resources (if it does, construct it and return it)
    function isKnownValueSet(id : String; out vs : TFHIRValueSet): Boolean;

    // given a value set, expand it
    function expandVS(vs : TFHIRValueSet; cacheId : String; profile : TFhirExpansionProfile; textFilter : String; limit, count, offset : integer) : TFHIRValueSet; overload;
    function expandVS(uri : String; profile : TFhirExpansionProfile; textFilter : String; limit, count, offset : integer) : TFHIRValueSet; overload;

    // these are internal services - not for use outside the terminology server
    function expandVS(uri: String; profile : TFhirExpansionProfile; textFilter : String; dependencies : TStringList; limit, count, offset : integer) : TFHIRValueSet; overload;
    function expandVS(vs: TFHIRValueSet; cacheId : String; profile : TFhirExpansionProfile; textFilter : String; dependencies : TStringList; limit, count, offset : integer): TFHIRValueSet; overload;

    procedure lookupCode(coding : TFhirCoding; lang : String; props : TList<String>; resp : TFHIRLookupOpResponse);
    function validate(vs : TFHIRValueSet; coding : TFhirCoding; profile : TFhirExpansionProfile; abstractOk : boolean) : TFhirParameters; overload;
    function validate(vs : TFHIRValueSet; coded : TFhirCodeableConcept; profile : TFhirExpansionProfile; abstractOk : boolean) : TFhirParameters; overload;
    function translate(lang : String; cm : TLoadedConceptMap; coding : TFhirCoding) : TFHIRParameters; overload;
    function translate(lang : String; source : TFHIRValueSet; coding : TFhirCoding; target : TFHIRValueSet) : TFHIRParameters; overload;
    function translate(lang : String; source : TFHIRValueSet; coded : TFhirCodeableConcept; target : TFHIRValueSet) : TFHIRParameters; overload;
    Function MakeChecker(uri : string; profile : TFhirExpansionProfile) : TValueSetChecker;
    function getDisplayForCode(lang : String; system, version, code : String): String;
    function checkCode(op : TFhirOperationOutcome; lang, path : string; code : string; system, version : string; display : string) : boolean;
    function isValidCode(system, code : String) : boolean;
    {$IFNDEF FHIR2}
    procedure composeCode(req : TFHIRComposeOpRequest; resp : TFHIRComposeOpResponse);
    {$ENDIF}

    // closures
    function InitClosure(name : String) : String;
    function UseClosure(name : String; out cm : TClosureManager) : boolean;
    function enterIntoClosure(conn : TKDBConnection; name, uri, code : String) : integer;

    procedure getCodeView(lang : String; coding : TFHIRCoding; response : TCDSHookResponse); overload;
    procedure getCodeView(lang : String; coding : TFhirCodeableConcept; response : TCDSHookResponse); overload;

    // database maintenance
    procedure BuildIndexes(prog : boolean);
    function Summary : String;
    property OnConnectDB : TDataBaseConnectionEvent read FOnConnectDB write FOnConnectDB;
  end;

implementation

uses
  FHIRLog,
  FHIRConstants,
  FHIRValueSetExpander;


{ TTerminologyServer }

constructor TTerminologyServer.Create(db : TKDBManager);
begin
  inherited;
  FExpansions := TAdvStringObjectMatch.create;
  FExpansions.PreventDuplicates;
  FDependencies := TAdvStringObjectMatch.create;
  FDependencies.PreventDuplicates;
  FClosures := TAdvMap<TClosureManager>.create;
end;

destructor TTerminologyServer.Destroy;
begin
  FClosures.Free;
  FDependencies.Free;
  FExpansions.free;
  inherited;
end;

procedure TTerminologyServer.load(ini : TFHIRServerIniFile);
var
  fn, s : string;
  p : TCodeSystemProvider;
  sn: TSnomedServices;
  def : boolean;
  icdX: TICD10Provider;
begin
  logt('Load DB Terminologies');
  Unii := TUniiServices.Create(Fdb.Link);
  ACIR := TACIRServices.Create;
  p := TIETFLanguageCodeServices.Create(ini.ReadString(voVersioningNotApplicable, 'lang', 'source', 'C:\work\fhirserver\sql\lang.txt'));
  ProviderClasses.Add(p.system(nil), p);
  ProviderClasses.Add(p.system(nil)+URI_VERSION_BREAK+p.version(nil), p.link);
  logt(' - done');

  if ini.ReadString(voVersioningNotApplicable, 'RxNorm', 'database', '') <> '' then
  begin
    logt('Connect to RxNorm');
    RxNorm := TRxNormServices.Create(FOnConnectDB('rxnorm', 100, 0, Ini.ReadString(voVersioningNotApplicable, 'database', 'driver', ''),
        Ini.ReadString(voVersioningNotApplicable, 'database', 'server', ''), Ini.ReadString(voVersioningNotApplicable, 'RxNorm', 'database', ''),
        Ini.ReadString(voVersioningNotApplicable, 'database', 'username', ''), Ini.ReadString(voVersioningNotApplicable, 'database', 'password', ''), false));
  end;
  if ini.ReadString(voVersioningNotApplicable, 'NciMeta', 'database', '') <> '' then
  begin
    logt('Connect to NciMeta');
    NciMeta := TNciMetaServices.Create(FOnConnectDB('ncimeta', 100, 0, Ini.ReadString(voVersioningNotApplicable, 'database', 'driver', ''),
        Ini.ReadString(voVersioningNotApplicable, 'database', 'server', ''), Ini.ReadString(voVersioningNotApplicable, 'NciMeta', 'database', ''),
        Ini.ReadString(voVersioningNotApplicable, 'database', 'username', ''), Ini.ReadString(voVersioningNotApplicable, 'database', 'password', ''), false));
  end;
  fn := ini.ReadString(voVersioningNotApplicable, 'icd-10', 'source', '');
  if fn <> '' then
  begin
    for s in fn.split([',']) do
    begin
      logt('Load ICD-10 from '+s);
      icdX := TICD10Provider.Create(true, s);
      icd10.Add(icdX);
      ProviderClasses.Add(icdX.system(nil), icdX.Link);
      ProviderClasses.Add(icdX.system(nil)+URI_VERSION_BREAK+icdX.version(nil), icdX.Link);
      logt(' - done');
    end;
  end;
  fn := ini.ReadString(voVersioningNotApplicable, 'snomed', 'cache', '');
  if fn <> '' then
  begin
    def := true;
    for s in fn.split([',']) do
    begin
      logt('Load Snomed from '+s);
      sn := TSnomedServices.Create;
      snomed.Add(sn);
      sn.Load(s.Trim);
      if (def) then
        DefSnomed := sn.Link;
      ProviderClasses.Add(sn.system(nil)+URI_VERSION_BREAK+sn.versionUri, sn.Link);
      ProviderClasses.Add(sn.system(nil)+URI_VERSION_BREAK+sn.editionUri, sn.Link);
      logt(' - done');
      def := false;
    end;
  end;

  fn := ini.ReadString(voVersioningNotApplicable, 'loinc', 'cache', '');
  if fn <> '' then
  begin
    logt('Load Loinc from '+fn);
    Loinc := TLoincServices.Create;
    Loinc.Load(fn);
    logt(' - done');
  end;
  fn := ini.ReadString(voVersioningNotApplicable, 'ucum', 'source', '');
  if fn = '' then
    raise Exception.Create('Unable to start because [ucum] source= is not specified in the ini file');
  logt('Load Ucum from '+fn);
  Ucum := TUcumServices.Create;
  Ucum.Import(fn);
  logt(' - done');
  if (DB <> nil) then
    LoadClosures;
end;

procedure TTerminologyServer.LoadClosures;
var
  conn : TKDBConnection;
begin
  conn := FDB.GetConnection('LoadClosures');
  try
    conn.SQL := 'Select ClosureKey, Name, Version from Closures';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
      FClosures.Add(conn.ColStringByName['name'], TClosureManager.create(conn.ColStringByName['name'], conn.ColIntegerByName['ClosureKey'], conn.ColIntegerByName['Version'], self));
    conn.Release;
  except
    on e : exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
end;

procedure TTerminologyServer.lookupCode(coding : TFhirCoding; lang : String; props : TList<String>; resp : TFHIRLookupOpResponse);
var
  provider : TCodeSystemProvider;
  ctxt : TCodeSystemProviderContext;
  s : String;
  {$IFNDEF FHIR2}
  p : TFHIRLookupOpRespProperty_;
  {$ENDIF}
  function hasProp(name : String; def : boolean) : boolean;
  begin
    if (props = nil) or (props.Count = 0) then
      result := def
    else
      result := props.Contains(name);
  end;
begin
  provider := getProvider(coding.system, coding.version, nil);
  try
    resp.name := provider.name(nil);
    s := provider.version(nil);
    if (s <> '') then
      resp.version := s;
    ctxt := provider.locate(coding.code);
    try
      if ctxt = nil then
        raise Exception.Create('Unable to find code '+coding.code+' in '+coding.system+' version '+s);

      if (hasProp('abstract', true) and provider.IsAbstract(ctxt)) then
      begin
        {$IFNDEF FHIR2}
        p := TFHIRLookupOpRespProperty_.create;
        resp.property_List.add(p);
        p.code := 'abstract';
        p.value := 'true';
        {$ELSE}
        resp.abstract := true;
        {$ENDIF}
      end;
      if (hasProp('display', true)) then
        resp.display := provider.Display(ctxt, lang);
      provider.extendLookup(ctxt, lang, props, resp);
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
  ts : TAdvStringList;
begin
  // must be in lock
  if FDependencies.ExistsByKey(name) then
    ts := FDependencies.GetValueByKey(name) as TAdvStringList
  else
  begin
    ts := TAdvStringList.Create;
    FDependencies.Add(name, ts);
  end;
  if not ts.ExistsByValue(value) then
    ts.Add(value);
end;

procedure TTerminologyServer.invalidateVS(id: String);
var
  ts : TAdvStringList;
  i : integer;
begin
  // must be in lock
  if FDependencies.ExistsByKey(id) then
  begin
    ts := FDependencies.GetValueByKey(id) as TAdvStringList;
    for i := 0 to ts.Count - 1 do
      if FExpansions.ExistsByKey(ts[i]) then
        FExpansions.DeleteByKey(ts[i]);
    FDependencies.DeleteByKey(id);
  end;
  for i := FExpansions.Count - 1 downto 0 do
   if FExpansions.KeyByIndex[i].StartsWith(id+#1) then
     FExpansions.DeleteByIndex(i);
end;

function TTerminologyServer.expandVS(vs: TFHIRValueSet; cacheId : String; profile : TFhirExpansionProfile; textFilter : String; limit, count, offset : integer): TFHIRValueSet;
var
  ts : TStringList;
begin
  ts := TStringList.create;
  try
    result := expandVS(vs, cacheId, profile, textFilter, ts, limit, count, offset);
  finally
    ts.free;
  end;
end;

function TTerminologyServer.enterIntoClosure(conn: TKDBConnection; name, uri, code: String): integer;
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


function TTerminologyServer.expandVS(vs: TFHIRValueSet; cacheId : String; profile : TFhirExpansionProfile; textFilter : String; dependencies : TStringList; limit, count, offset : integer): TFHIRValueSet;
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
        result := (FExpansions.matches[cacheId+#1+profile.hash+#1+textFilter+#1+inttostr(limit)+#1+inttostr(count)+#1+inttostr(offset)] as TFhirValueSet).link;
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
    exp := TFHIRValueSetExpander.create(self.Link);
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



function TTerminologyServer.expandVS(uri: String; profile : TFhirExpansionProfile; textFilter : String; limit, count, offset : integer): TFHIRValueSet;
var
  vs : TFHIRValueSet;
  ts : TStringList;
begin
  ts := TStringList.create;
  try
    vs := getValueSetByUrl(uri);
    try
      result := expandVS(vs, uri, profile, textFilter, ts, limit, count, offset);
    finally
      vs.Free;
    end;
  finally
    ts.Free;
  end;
end;

function TTerminologyServer.expandVS(uri: String; profile : TFhirExpansionProfile; textFilter : String; dependencies: TStringList; limit, count, offset : integer): TFHIRValueSet;
var
  vs : TFHIRValueSet;
begin
  vs := getValueSetByUrl(uri);
  try
    if vs = nil then
      raise Exception.Create('Unable to find value set "'+uri+'"');
    result := expandVS(vs, uri, profile, textFilter, limit, count, offset);
  finally
    vs.Free;
  end;
end;


function TTerminologyServer.makeAnyValueSet: TFhirValueSet;
begin
  result := TFhirValueSet.Create;
  try
    result.url := ANY_CODE_VS;
    result.name := 'All codes known to the system';
    result.description := 'All codes known to the system';
    result.status := PublicationStatusActive;
    result.compose := TFhirValueSetCompose.create;
    result.compose.includeList.Append.system := ANY_CODE_VS;
    result.link;
  finally
    result.Free;
  end;
end;


function TTerminologyServer.isKnownValueSet(id: String; out vs: TFHIRValueSet): Boolean;
var
  cs : TFHIRCodeSystem;
  sn : TSnomedServices;
begin
  vs := nil;
  if id.StartsWith('http://snomed.info/') then
  begin
    vs := DefSnomed.buildValueSet(id);
    if (vs = nil) then
    begin
      for sn in Snomed do
      begin
        vs := sn.buildValueSet(id);
        if (vs <> nil) then
          break;
      end;
    end;
  end
  else if id.StartsWith('http://loinc.org/vs/LP') or id.StartsWith('http://loinc.org/vs/LL') then
    vs := Loinc.buildValueSet(id)
  else if id = 'http://loinc.org/vs' then
    vs := Loinc.buildValueSet('')
  else if id = ANY_CODE_VS then
    vs := makeAnyValueSet
  else
  begin
    cs := getCodeSystemByValueSet(id);
    if (cs <> nil) then
    begin
      try
        vs := {$IFDEF FHIR2}cs.link; {$ELSE} cs.buildImplicitValueSet {$ENDIF};
      finally
        cs.Free;
      end;
    end;
  end;

  result := vs <> nil;
end;

function TTerminologyServer.MakeChecker(uri: string; profile : TFhirExpansionProfile): TValueSetChecker;
var
  vs : TFhirValueSet;
begin
  result := TValueSetChecker.create(self.Link, uri);
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

function TTerminologyServer.validate(vs : TFHIRValueSet; coding : TFhirCoding; profile : TFhirExpansionProfile; abstractOk : boolean) : TFhirParameters;
var
  check : TValueSetChecker;
begin
  if vs = nil then
  begin
    vs := TFhirValueSet.Create;
    vs.url := ANY_CODE_VS;
  end
  else
    vs.Link;

  try
    check := TValueSetChecker.create(self.Link, vs.url);
    try
      check.prepare(vs, profile);
      result := check.check(coding, abstractOk);
    finally
      check.Free;
    end;
  finally
    vs.Free;
  end;
end;


function TTerminologyServer.validate(vs : TFHIRValueSet; coded : TFhirCodeableConcept; profile : TFhirExpansionProfile; abstractOk : boolean) : TFhirParameters;
var
  check : TValueSetChecker;
begin
  if vs = nil then
  begin
    vs := TFhirValueSet.Create;
    vs.url := ANY_CODE_VS;
  end
  else
    vs.Link;

  try
    check := TValueSetChecker.create(self.Link, vs.url);
    try
      check.prepare(vs, profile);
      result := check.check(coded, abstractOk);
    finally
      check.Free;
   end;
  finally
    vs.Free;
  end;
end;


function TTerminologyServer.checkCode(op : TFhirOperationOutcome; lang, path : string; code : string; system, version : string; display : string) : boolean;
var
  cs : TFhirCodeSystem;
  cp : TCodeSystemProvider;
  lct : TCodeSystemProviderContext;
  def : TFhirCodeSystemConcept;
  d : String;
begin
  result := false;
  if (system = 'http://hl7.org/fhir/sid/icd-10') then
    result := true// nothing for now....
  else if (system = 'http://snomed.info/sct') and (DefSnomed <> nil) then
  begin
    if op.warning('InstanceValidator', IssueTypeCodeInvalid, path, DefSnomed.IsValidConcept(code), 'The SNOMED-CT term "'+code+'" is unknown') then
    begin
      d := DefSnomed.GetDisplayName(code, '');
      result := op.warning('InstanceValidator', IssueTypeCodeInvalid, path, (display = '') or (display = d), 'Display for SNOMED-CT term "'+code+'" should be "'+d+'"');
    end;
  end
  else if system.StartsWith('http://loinc.org') and (Loinc <> nil) then
  begin
    d := Loinc.GetDisplayByName(code, Loinc.LangsForLang(lang));
    if op.warning('InstanceValidator', IssueTypeCodeInvalid, path, d <> '', 'The LOINC code "'+code+'" is unknown') then
      result := op.warning('InstanceValidator', IssueTypeCodeInvalid, path, (display = '') or (display = d), 'Display for Loinc Code "'+code+'" should be "'+d+'"');
  end
  else if system.StartsWith('http://unitsofmeasure.org') and (Ucum <> nil) then
  begin
    d := Ucum.validate(code);
    result := op.warning('InstanceValidator', IssueTypeCodeInvalid, path, d = '', 'The UCUM code "'+code+'" is not valid: '+d);
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
          if (op.error('InstanceValidator', IssueTypeCodeInvalid, path, lct <> nil, 'Unknown Code ('+system+'#'+code+')')) then
            result := op.warning('InstanceValidator', IssueTypeCodeInvalid, path, (display = '') or (display = cp.Display(lct, '')),
            'Display for '+system+' code "'+code+'" should be "'+cp.Display(lct, '')+'"');
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
        if op.warning('InstanceValidator', IssueTypeCodeInvalid, path, cs <> nil, 'Unknown Code System '+system) then
        begin
          def := getCodeDefinition(cs, code);
          if (op.error('InstanceValidator', IssueTypeCodeInvalid, path, def <> nil, 'Unknown Code ('+system+'#'+code+')')) then
            result := op.warning('InstanceValidator', IssueTypeCodeInvalid, path, (display = '') or (display = def.Display), 'Display for '+system+' code "'+code+'" should be "'+def.Display+'"');
        end;
      finally
        cs.free;
      end;
    end;
  end;
end;

{$IFNDEF FHIR2}
procedure TTerminologyServer.composeCode(req: TFHIRComposeOpRequest; resp: TFHIRComposeOpResponse);
var
  prop : TFHIRComposeOpReqProperty_;
  cs : TCodeSystemProvider;
  s : string;
  first : boolean;
  exp : TSnomedExpression;
  list : TAdvList<TMatchingConcept>;
  mc : TMatchingConcept;
  match : TFHIRComposeOpRespMatch;
  ref : TSnomedRefinementGroup;
  unmatch : TFHIRComposeOpRespUnmatched;
  function isValidCode(s : String) : boolean;
  var
    ctxt : TCodeSystemProviderContext;
  begin
    ctxt := cs.locate(s);
    result := ctxt <> nil;
    cs.Close(ctxt);
  end;
begin
  cs := getProvider(req.system, req.version, nil);
  try
    s := '';
    for prop in req.property_List do
    begin
      if prop.code = 'focus' then
        if not isValidCode(prop.value) then
          raise Exception.Create('invalid snomed value :'+prop.value)
        else
          s := prop.value;
    end;
    if s = '' then
      raise Exception.Create('no focus found');

    first := true;
    for prop in req.property_List do
    begin
      if prop.code <> 'focus' then
      begin
        if not isValidCode(prop.code) then
          raise Exception.Create('invalid snomed code :'+prop.code);
        if not isValidCode(prop.value) then
          raise Exception.Create('invalid snomed value :'+prop.value);
        if prop.subpropertyList.Count > 0 then
          raise Exception.Create('invalid sub-property');
        if first then
          s := s + ':'+ prop.code+'='+prop.value
        else
          s := s + ','+ prop.code+'='+prop.value;
        first := false;
      end;
    end;
    if not req.exact then
      raise Exception.Create('Only ''exact=true'' is supported at present');
    exp := TSnomedServices(cs).parseExpression(s);
    try
      list := TSnomedServices(cs).condenseExpression(exp);
      try
        for mc in list do
        begin
          match := TFHIRComposeOpRespMatch.Create;
          resp.matchList.add(match);
          match.code := TFhirCoding.Create('http://snomed.info/sct', mc.matched);
          match.code.display := cs.getDisplay(mc.matched, '');
          for ref in mc.Unmatched do
          begin
            unmatch := TFHIRComposeOpRespUnmatched.Create;
            match.unmatchedList.Add(unmatch);
            if ref.refinements.Count = 1 then
            begin
              unmatch.code := ref.refinements[0].name.code;
              unmatch.value := ref.refinements[0].value.ToString;
            end
            else
            begin
              raise Exception.Create('not done yet');
            end;
          end;
        end;
      finally
        list.free;
      end;
    finally
      exp.free;
    end;
  finally
    cs.free;
  end;
end;
{$ENDIF}

function TTerminologyServer.getCodeDefinition(c : TFhirCodeSystemConcept; code : string) : TFhirCodeSystemConcept;
var
  i : integer;
  g : TFhirCodeSystemConcept;
  r : TFhirCodeSystemConcept;
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

function TTerminologyServer.getCodeDefinition(vs : TFhirCodeSystem; code : string) : TFhirCodeSystemConcept;
var
  i : integer;
  c : TFhirCodeSystemConcept;
  r : TFhirCodeSystemConcept;
begin
  result := nil;
  for i := 0 to vs.codeSystem.conceptList.Count - 1 do
  begin
    c := vs.codeSystem.conceptList[i];
    r := getCodeDefinition(c, code);
    if (r <> nil) then
    begin
      result := r;
      exit;
    end;
  end;
end;


function TTerminologyServer.getDisplayForCode(lang : String; system, version, code: String): String;
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

procedure TTerminologyServer.getCodeView(lang : String; coding: TFHIRCoding; response: TCDSHookResponse);
var
  card : TCDSHookCard;
  cs : TCodeSystemProvider;
begin
  cs := getProvider(coding.system, coding.version, nil, true);
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

procedure TTerminologyServer.getCodeView(lang : String; coding: TFhirCodeableConcept; response: TCDSHookResponse);
var
  c : TFhirCoding;
begin
  for c in coding.codingList do
    getCodeView(lang, c, response);
end;

function TTerminologyServer.InitClosure(name: String) : String;
var
  conn : TKDBConnection;
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

function TTerminologyServer.isOkSource(cm: TLoadedConceptMap; coding: TFHIRCoding; out group : TFhirConceptMapGroup; out match: TFhirConceptMapGroupElement): boolean;
var
  g : TFhirConceptMapGroup;
  em : TFhirConceptMapGroupElement;
begin
  result := false;
  {$IFDEF FHIR2}
    for em in cm.resource.elementList do
      if (em.system = coding.system) and (em.code = coding.code) then
  {$ELSE}
  for g in cm.Resource.groupList do
    for em in g.elementList do
      if (g.source = coding.system) and (em.code = coding.code) then
  {$ENDIF}
      begin
        result := true;
        match := em;
        group := g;
      end;
end;

function TTerminologyServer.isOkTarget(cm : TLoadedConceptMap; vs : TFHIRValueSet) : boolean;
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

function TTerminologyServer.isOkSource(cm : TLoadedConceptMap; vs : TFHIRValueSet; coding : TFHIRCoding; out group : TFhirConceptMapGroup; out match : TFhirConceptMapGroupElement) : boolean;
var
  g : TFhirConceptMapGroup;
  em : TFhirConceptMapGroupElement;
begin
  result := false;
  if (vs = nil) or ((cm.source <> nil) and (cm.Source.url = vs.url)) then
  begin
    {$IFDEF FHIR2}
      for em in cm.Resource.elementList do
        if (em.system = coding.system) and (em.code = coding.code) then
    {$ELSE}
    for g in cm.Resource.groupList do
      for em in g.elementList do
        if (g.source = coding.system) and (em.code = coding.code) then
    {$ENDIF}
      begin
        result := true;
        match := em;
        group := g;
      end;
  end;
end;

function TTerminologyServer.translate(lang : String; source : TFHIRValueSet; coding : TFhirCoding; target : TFHIRValueSet) : TFHIRParameters;
var
  op : TFHIROperationOutcome;
  list : TLoadedConceptMapList;
  i : integer;
  cm : TLoadedConceptMap;
  p : TFhirParameters;
  g : TFhirConceptMapGroup;
  em : TFhirConceptMapGroupElement;
  map : TFhirConceptMapGroupElementTarget;
  outcome : TFhirCoding;
  found : boolean;
begin
  result := nil;
  op := TFhirOperationOutcome.Create;
  try
    try
      if not checkCode(op, lang, '', coding.code, coding.system, coding.version, coding.display) then
        raise Exception.Create('Code '+coding.code+' in system '+coding.system+' not recognized');

      // check to see whether the coding is already in the target value set, and if so, just return it
      p := validate(target, coding, nil, false);
      try
        if TFhirBoolean(p.NamedParameter['result']).value then
        begin
          result := TFhirParameters.create;
          result.AddParameter('result', true);
          result.AddParameter('outcome', coding.Link);
          result.AddParameter('equivalence', TFhirCode.Create('equal'));
          exit;
        end;
      finally
        p.Free;
      end;

      found := false;
      result := TFhirParameters.create;
      list := GetConceptMapList;
      try
        for i := 0 to list.Count - 1 do
        begin
          cm := list[i];
          if isOkTarget(cm, target) and isOkSource(cm, source, coding, g, em) then
          begin
            found := true;
            if em.targetList.Count = 0 then
              raise Exception.Create('Concept Map has an element with no map for '+'Code '+coding.code+' in system '+coding.system);
            map := em.targetList[0]; // todo: choose amongst dependencies
            if (map.equivalence in [ConceptMapEquivalenceEquivalent, ConceptMapEquivalenceEqual, ConceptMapEquivalenceWider, ConceptMapEquivalenceSubsumes, ConceptMapEquivalenceNarrower, ConceptMapEquivalenceSpecializes, ConceptMapEquivalenceInexact]) then
            begin
              found := true;
              result.AddParameter('result', true);
              outcome := TFhirCoding.Create;
              result.AddParameter('outcome', outcome);
              {$IFDEF FHIR2}
              outcome.system := map.system;
              {$ELSE}
              outcome.system := g.target;
              {$ENDIF}
              outcome.code := map.code;
            end
            else
            begin
              result.AddParameter('result', false);
            end;
            result.AddParameter('equivalence', map.equivalenceElement.Link);
            if (map.{$IFDEF FHIR2}commentsElement{$ELSE}commentElement{$ENDIF} <> nil) then
              result.AddParameter('message', map.{$IFDEF FHIR2}commentsElement{$ELSE}commentElement{$ENDIF}.Link);
            exit;
          end;
        end;
      finally
        list.Free;
      end;

      if not found then
      begin
        result.AddParameter('result', false);
        result.AddParameter('message', 'no match found');
      end;
    except
      on e : exception do
      begin
        result := TFHIRParameters.create;
        result.AddParameter('result', false);
        result.AddParameter('message', e.message);
      end;
    end;
  finally
    op.Free;
  end;
end;

function TTerminologyServer.translate(lang : String; source : TFHIRValueSet; coded : TFhirCodeableConcept; target : TFHIRValueSet) : TFHIRParameters;
begin
  if coded.codingList.count = 1 then
    result := translate(lang, source, coded.codingList[0], target)
  else
    raise Exception.Create('Not done yet');
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

(*function TTerminologyServer.translate(vs: TFHIRValueSet; coding: TFhirCoding; dest : String): TFHIRParameters;
var
  isCs, ok: boolean;
  i, j : integer;
  cc : TFhirCodeableConcept;
  c : TFHIRCoding;
  maps : TFhirConceptMapConcepttargetList;
  map : TFhirConceptMapConceptMap;
begin
  result := TFhirOperationOutcome.Create;
  try
    if checkCode(result, '', coding.code, coding.system, coding.display) then
    begin
      cc := TFhirCodeableConcept.Create;
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
              cm.hasTranslation(coding.system, coding.code, maps) then
            try
              for j := 0 to maps.Count - 1 do
              begin
                map := maps[j];
                if (map.equivalence in [ConceptEquivalenceEqual, ConceptEquivalenceEquivalent, ConceptEquivalenceWider, ConceptEquivalenceInexact]) and
                  (not isCs {if we'rea value set mapping, we'll just run with all the maps) } or (map.codeSystem = dest)) then
                begin
                  ok := true;
                  c := cc.codingList.Append;
                  c.system := map.codeSystem;
                  c.code := map.code;
                  c.display := getDisplayForCode(map.codeSystem, map.code);
                  if map.comments <> '' then
                    result.hint('terminology-server', 'mapping', '', false, 'Mapping from "'+coding.system+'"/"'+coding.code+'" to "'+c.system+'"/"'+c.code+'": '+map.comments)
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

procedure TTerminologyServer.BuildIndexesInternal(prog: boolean; conn1, conn2, conn3: TKDBConnection);
var
  i : integer;
  finish : TDateTime;
begin
  finish := now + DATETIME_SECOND_ONE * 30;
  MaintenanceThreadStatus := 'BI: counting';
  if conn1.CountSQL('Select Count(*) from ValueSets where NeedsIndexing = 0') = 0 then
    conn1.ExecSQL('Update Concepts set NeedsIndexing = 0'); // we're going to index them all anwyay

  // first, update value set member information
  MaintenanceThreadStatus := 'BI: Updating ValueSet Members';
  if (prog) then logtn('Updating ValueSet Members');
  conn1.SQL := 'Select ValueSetKey, URL from ValueSets where NeedsIndexing = 1';
  conn1.Prepare;
  conn1.Execute;
  i := 0;

  while conn1.FetchNext do
  begin
    inc(i);
    if (prog and (i mod 10 = 0)) then Write('.');
    MaintenanceThreadStatus := 'BI: Updating ValueSet Members for '+conn1.ColStringByName['ValueSetKey'];
    processValueSet(conn1.ColIntegerByName['ValueSetKey'], conn1.ColStringByName['URL'], conn2, conn3);
    if finish < now then
      break;
  end;
  conn1.Terminate;
  if (prog) then Writeln;
  if finish < now then
    exit;


  // second, for each concept that needs indexing, check it's value set information
  if (prog) then logtn('Indexing Concepts');
  MaintenanceThreadStatus := 'BI: Indexing Concepts';
  conn1.SQL := 'Select ConceptKey, URL, Code from Concepts where NeedsIndexing = 1';
  conn1.Prepare;
  conn1.Execute;
  i := 0;
  while conn1.FetchNext do
  begin
    inc(i);
    if (prog and (i mod 10 = 0)) then Write('.');
    MaintenanceThreadStatus := 'BI: Indexing Concept '+conn1.ColStringByName['ConceptKey'];
    processConcept(conn1.ColIntegerByName['ConceptKey'], conn1.ColStringByName['URL'], '', conn1.ColStringByName['Code'], conn2, conn3);
    if finish < now then
      break;
  end;
  conn1.Terminate;
  if (prog) then Writeln;
  if finish < now then
    exit;

  // last, for each entry in the closure entry table that needs closureing, do it
  if (prog) then logtn('Generating Closures');
  MaintenanceThreadStatus := 'BI: Generating Closures';
  conn1.SQL := 'select ClosureEntryKey, Closures.ClosureKey, SubsumesKey, Name, URL, Code from ClosureEntries, Concepts, Closures '+
     'where Closures.ClosureKey = ClosureEntries.ClosureKey and ClosureEntries.IndexedVersion = 0 and ClosureEntries.SubsumesKey = Concepts.ConceptKey';
  conn1.Prepare;
  conn1.Execute;
  while conn1.FetchNext do
  begin
    inc(i);
    if (prog and (i mod 100 = 0)) then Write('.');
    MaintenanceThreadStatus := 'BI: Generating Closures for '+conn1.ColStringByName['Name'];
    FClosures[conn1.ColStringByName['Name']].processEntry(conn2, conn1.ColIntegerByName['ClosureEntryKey'], conn1.ColIntegerByName['SubsumesKey'], conn1.ColStringByName['URL'], conn1.ColStringByName['Code']);
    if finish < now then
      break;
  end;
  conn1.Terminate;
  if (prog) then Writeln;
  if finish < now then
    exit;

  if (prog) then logt('Done');
  MaintenanceThreadStatus := 'BI: ';
end;

procedure TTerminologyServer.BuildIndexes(prog : boolean);
var
  conn1, conn2, conn3 : TKDBConnection;
begin
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

procedure TTerminologyServer.processConcept(ConceptKey: integer; URL, version, Code: String; conn2, conn3: TKDBConnection);
var
  vs : TFhirValueSet;
  val : TValuesetChecker;
  profile : TFhirExpansionProfile;
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
        profile := TFhirExpansionProfile.Create;
        try
          try
            val := TValueSetChecker.create(self.Link, vs.url);
            try
              val.prepare(vs, profile);
              if not val.check(URL, version, code, true) then
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

procedure TTerminologyServer.processValueSet(ValueSetKey: integer; URL: String; conn2, conn3: TKDBConnection);
var
  vs : TFhirValueSet;
  val : TValuesetChecker;
  system, version, code : String;
  profile : TFhirExpansionProfile;
begin
  vs := getValueSetByURL(URL);
  if vs = nil then
    conn2.ExecSQL('Update ValueSets set NeedsIndexing = 0, Error = ''Unable to find definition'' where ValueSetKey = '+inttostr(valuesetKey))
  else
    try
      profile := TFhirExpansionProfile.defaultProfile;
      try
        try
          val := TValueSetChecker.create(self.Link, vs.url);
          try
            val.prepare(vs, profile);
            conn2.SQL := 'select ConceptKey, URL, Code from Concepts';
            conn2.Prepare;
            conn2.Execute;
            while conn2.FetchNext do
            begin
              system := conn2.ColStringByName['URL'];
              code := conn2.ColStringByName['Code'];
              if not val.check(system, version, code, true) then
                conn3.ExecSQL('Delete from ValueSetMembers where ValueSetKey = '+inttostr(ValueSetKey)+' and ConceptKey = '+conn2.ColStringByName['ConceptKey'])
              else if conn3.CountSQL('select Count(*) from ValueSetMembers where ValueSetKey = '+inttostr(ValueSetKey)+' and ConceptKey = '+conn2.ColStringByName['ConceptKey']) = 0 then
                conn3.ExecSQL('insert into ValueSetMembers (ValueSetMemberKey, ValueSetKey, ConceptKey) values ('+inttostr(NextValueSetMemberKey)+','+inttostr(ValueSetKey)+', '+conn2.ColStringByName['ConceptKey']+')');
            end;
            Conn2.Terminate;
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

function TTerminologyServer.translate(lang : String; cm: TLoadedConceptMap; coding: TFhirCoding): TFHIRParameters;
var
  op : TFHIROperationOutcome;
  g : TFhirConceptMapGroup;
  em : TFhirConceptMapGroupElement;
  map : TFhirConceptMapGroupElementTarget;
  outcome : TFhirCoding;
  found : boolean;
begin
  result := nil;
  op := TFhirOperationOutcome.Create;
  try
    try
      if not checkCode(op, lang, '', coding.code, coding.system, coding.version, coding.display) then
        raise Exception.Create('Code '+coding.code+' in system '+coding.system+' not recognized');

//      // check to see whether the coding is already in the target value set, and if so, just return it
//      p := validate(target, coding, false);
//      try
//        if TFhirBoolean(p.NamedParameter['result']).value then
//        begin
//          result := TFhirParameters.create;
//          result.AddParameter('result', true);
//          result.AddParameter('outcome', coding.Link);
//          result.AddParameter('equivalence', TFhirCode.Create('equal'));
//          exit;
//        end;
//      finally
//        p.Free;
//      end;

      found := false;
      result := TFhirParameters.create;
      if isOkSource(cm, coding, g, em) then
      begin
      found := true;
      if em.targetList.Count = 0 then
        raise Exception.Create('Concept Map has an element with no map for '+'Code '+coding.code+' in system '+coding.system);
      map := em.targetList[0]; // todo: choose amongst dependencies
      if (map.equivalence in [ConceptMapEquivalenceNull, ConceptMapEquivalenceEquivalent, ConceptMapEquivalenceEqual, ConceptMapEquivalenceWider, ConceptMapEquivalenceSubsumes, ConceptMapEquivalenceNarrower, ConceptMapEquivalenceSpecializes, ConceptMapEquivalenceInexact]) then
      begin
        found := true;
        result.AddParameter('result', true);
        outcome := TFhirCoding.Create;
        result.AddParameter('outcome', outcome);
        {$IFDEF FHIR2}
        outcome.system := map.system;
        {$ELSE}
        outcome.system := g.target;
        {$ENDIF}
        outcome.code := map.code;
      end
      else
      begin
        result.AddParameter('result', false);
      end;
      if (map.equivalenceElement = nil) then
        result.AddParameter('equivalence', TFHIRCode.Create('equivalent'))
      else
        result.AddParameter('equivalence', TFHIRCode.Create(map.equivalenceElement.value));
      if (map.{$IFDEF FHIR2}commentsElement{$ELSE}commentElement{$ENDIF} <> nil) then
        result.AddParameter('message', map.{$IFDEF FHIR2}commentsElement{$ELSE}commentElement{$ENDIF}
        .Link);
      end;

      if not found then
      begin
        result.AddParameter('result', false);
        result.AddParameter('message', 'no match found');
      end;
    except
      on e : exception do
      begin
        result := TFHIRParameters.create;
        result.AddParameter('result', false);
        result.AddParameter('message', e.message);
      end;
    end;
  finally
    op.Free;
  end;
end;

end.
