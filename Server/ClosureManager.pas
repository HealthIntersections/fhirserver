unit ClosureManager;

interface

Uses
  SysUtils, Generics.Collections,
  StringSupport, kCritSct,
  AdvObjects, AdvGenerics,
  KDBManager,
  FHIRResources, FHIRTypes, FHIRUtilities,
  TerminologyServerStore;

Type
  TSubsumptionMatch = class (TAdvObject)
  private
    FKeySrc: integer;
    FcodeSrc: String;
    FuriSrc: String;
    FKeyTgt: integer;
    FcodeTgt: String;
    FuriTgt: String;
  public
    Constructor Create(keysrc : integer; urisrc, codesrc : String; keytgt : integer; uritgt, codetgt : String);
    property KeySrc : integer read FKeySrc write FKeySrc;
    property uriSrc : String read FuriSrc write FuriSrc;
    property codeSrc : String read FcodeSrc write FcodeSrc;
    property KeyTgt : integer read FKeyTgt write FKeyTgt;
    property uriTgt : String read FuriTgt write FuriTgt;
    property codeTgt : String read FcodeTgt write FcodeTgt;
  end;

  TClosureManager = class (TAdvObject)
  private
    FStore : TTerminologyServerStore; // not linked, as this is the owner
    FName : String;
    FKey : integer;
    FLock : TCriticalSection;
    FVersion : integer;
    function GetConceptKey(conn : TKDBConnection; uri, code : String) : integer;
    procedure processEntryInternal(conn: TKDBConnection; ClosureEntryKey, ConceptKey: integer; uri, code : String; map : TFHIRConceptMap);
  public
    Constructor Create(name : String; key, version : integer; store : TTerminologyServerStore);
    Destructor Destroy; override;

    function Link : TClosureManager; overload;

    Property Version : Integer read FVersion;

    procedure Init(conn : TKDBConnection);

    // this is a split mode implementation, for performance - the subsumption is calculated in the background
    function enterCode(conn: TKDBConnection; uri, code: String): integer;
    procedure processEntry(conn: TKDBConnection; ClosureEntryKey, ConceptKey: integer; uri, code: String);

    // this is the inline variant; subsumption is determined immediately
    procedure processConcepts(conn: TKDBConnection; codings : TAdvList<TFHIRCoding>; map : TFHIRConceptMap);

    procedure reRun(conn: TKDBConnection; map : TFHIRConceptMap; version : integer);
  end;

implementation

{ TClosureManager }

constructor TClosureManager.Create(name: String; key, version : integer; store : TTerminologyServerStore);
begin
  inherited Create;
  FLock := TCriticalSection.Create('Closure Table '+name);
  FName := name;
  FKey := key;
  FVersion := Version;
  FStore := store;
end;

procedure TClosureManager.Init(conn: TKDBConnection);
begin
  if FKey = 0 then
  begin
    Fkey := FStore.nextClosureKey;
    FVersion := 0;
    conn.ExecSQL('insert into Closures (ClosureKey, Name, Version) values ('+inttostr(Fkey)+', '''+FName+''', 0)');
  end
  else
    conn.ExecSQL('delete from ClosureEntries where ClosureKey = '+inttostr(Fkey));
end;

function TClosureManager.Link: TClosureManager;
begin
  result := TClosureManager(inherited Link);
end;

function TClosureManager.GetConceptKey(conn: TKDBConnection; uri, code: String): integer;
begin
  result := conn.CountSQL('select ConceptKey from Concepts where URL = '''+SQLWrapString(uri)+''' and Code = '''+SQLWrapString(code)+'''');
  if result = 0 then
  begin
    result := FStore.NextConceptKey;
    conn.execSQL('insert into Concepts (ConceptKey, URL, Code, NeedsIndexing) values ('+inttostr(result)+', '''+SQLWrapString(uri)+''', '''+SQLWrapString(code)+''', 1)');
  end;
end;

destructor TClosureManager.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TClosureManager.enterCode(conn: TKDBConnection; uri, code: String): integer;
begin
  result := GetConceptKey(conn, uri, code);
  // now, check that it is in the closure
  if conn.CountSQL('select Count(*) from ClosureEntries where ClosureKey = '+inttostr(FKey)+' and SubsumesKey = '+inttostr(result)) = 0 then
  begin
    // enter it into the closure table
    conn.ExecSQL('insert into ClosureEntries (ClosureEntryKey, ClosureKey, SubsumesKey, SubsumedKey, IndexedVersion) values ('+inttostr(FStore.NextClosureEntryKey)+', '+inttostr(FKey)+', '+inttostr(result)+', '+inttostr(result)+', 0)');
  end;
end;

procedure TClosureManager.processEntry(conn: TKDBConnection; ClosureEntryKey, ConceptKey: integer; uri, code : String);
begin
  FLock.Lock;
  try
    inc(FVersion);
    processEntryInternal(conn, ClosureEntryKey, ConceptKey, uri, code, nil);
    conn.ExecSQL('Update ClosureEntries set IndexedVersion = '+inttostr(FVersion)+' where ClosureEntryKey = '+inttostr(ClosureEntryKey));
    conn.ExecSQL('update Closures set Version = '+inttostr(FVersion)+' where ClosureKey = '+inttostr(FKey));
  finally
    FLock.Unlock;
  end;
end;

procedure TClosureManager.processConcepts(conn: TKDBConnection; codings : TAdvList<TFHIRCoding>; map : TFHIRConceptMap);
var
  coding : TFHIRCoding;
  ck, cek : integer;
begin
  FLock.Lock;
  try
    inc(FVersion);
    map.version := inttostr(FVersion);

    for coding in codings do
    begin
      ck := GetConceptKey(conn, coding.system, coding.code);
      if conn.CountSQL('select Count(*) from ClosureEntries where ClosureKey = '+inttostr(FKey)+' and SubsumesKey = '+inttostr(ck)) = 0 then
      begin
        // enter it into the closure table
        cek := FStore.NextClosureEntryKey;
        conn.ExecSQL('insert into ClosureEntries (ClosureEntryKey, ClosureKey, SubsumesKey, SubsumedKey, IndexedVersion) values ('+inttostr(cek)+', '+inttostr(FKey)+', '+inttostr(ck)+', '+inttostr(ck)+', '+inttostr(FVersion)+')');
        //now, check the subsumes....
        processEntryInternal(conn, cek, ck, coding.system, coding.code, map);
      end;
    end;
    conn.ExecSQL('update Closures set Version = '+inttostr(FVersion)+' where ClosureKey = '+inttostr(FKey));
  finally
    FLock.Unlock;
  end;
end;

procedure TClosureManager.processEntryInternal(conn: TKDBConnection; ClosureEntryKey, ConceptKey: integer; uri, code : String; map : TFHIRConceptMap);
var
  version : integer;
  matches : TAdvList<TSubsumptionMatch>;
  match : TSubsumptionMatch;
  element, e : TFhirConceptMapElement;
  target, t : TFhirConceptMapElementTarget;
begin
  matches := TAdvList<TSubsumptionMatch>.create;
  try
    conn.SQL := 'select ConceptKey, URL, Code from Concepts where ConceptKey in (select SubsumesKey from ClosureEntries where ClosureKey = '+inttostr(FKey)+' and ClosureEntryKey <> '+inttostr(ClosureEntryKey)+')';
    conn.prepare;
    conn.execute;
    while conn.fetchnext do
    begin
      try
        if FStore.subsumes(uri, code, conn.ColStringByName['URL'], conn.ColStringByName['Code']) then
          matches.Add(TSubsumptionMatch.Create(ConceptKey, uri, code, conn.colIntegerByName['ConceptKey'], conn.ColStringByName['URL'], conn.ColStringByName['Code']))
        else if FStore.subsumes(conn.ColStringByName['URL'], conn.ColStringByName['Code'], uri, code) then
          matches.Add(TSubsumptionMatch.Create(conn.colIntegerByName['ConceptKey'], conn.ColStringByName['URL'], conn.ColStringByName['Code'], ConceptKey, uri, code));
      except
        // not much we can do but ignore this?
      end;
    end;
    conn.Terminate;

    if matches.Count > 0 then
    begin
      for match in matches do
      begin
        conn.execSQL('Insert into ClosureEntries (ClosureEntryKey, ClosureKey, SubsumesKey, SubsumedKey, IndexedVersion) values '+
                 '('+inttostr(FStore.NextClosureEntryKey)+', '+inttostr(FKey)+', '+inttostr(match.KeySrc)+', '+inttostr(match.KeyTgt)+', '+inttostr(FVersion)+')');
        if (map <> nil) then
        begin
          element := nil;
          for e in map.elementList do
            if (e.codeSystem = match.uriSrc) and (e.code = match.codeSrc) then
              element := e;
          if element = nil then
          begin
            element := map.elementList.Append;
            element.codeSystem := match.uriSrc;
            element.code := match.codeSrc;
          end;
          target := nil;
          for t in element.targetList do
            if (t.codeSystem = match.uriTgt) and (t.code = match.codeTgt) then
              target := t;
          if (target = nil) then
          begin
            target := element.targetList.Append;
            target.codeSystem := match.uritgt;
            target.code := match.codetgt;
            target.equivalence := ConceptMapEquivalenceSpecializes;
          end;
        end;
      end;
    end;
  finally
    matches.Free;
  end;
end;


procedure TClosureManager.reRun(conn: TKDBConnection; map: TFHIRConceptMap; version: integer);
var
  key : String;
  elements : TAdvMap<TFhirConceptMapElement>;
  element : TFhirConceptMapElement;
  target : TFhirConceptMapElementTarget;
begin
  elements := TAdvMap<TFhirConceptMapElement>.create;
  try
    conn.SQL := 'Select ClosureEntryKey, src.URL as UrlSrc, src.Code as CodeSrc, tgt.URL as UrlTgt, tgt.Code as CodeTgt '+
       'from ClosureEntries, Concepts as src, Concepts as tgt '+
       'where ClosureEntries.ClosureKey = '+inttostr(FKey)+' and ClosureEntries.SubsumesKey <> ClosureEntries.SubsumedKey and src.ConceptKey = ClosureEntries.SubsumesKey and tgt.ConceptKey = ClosureEntries.SubsumedKey';
    conn.prepare;
    conn.execute;
    while conn.FetchNext do
    begin
      key := conn.ColStringByName['UrlSrc']+'||'+conn.ColStringByName['CodeSrc'];
      if elements.ContainsKey(key) then
        element := elements[key]
      else
      begin
        element := map.elementList.Append;
        elements.Add(key, element.link);
        element.codeSystem := conn.ColStringByName['UrlSrc'];
        element.code := conn.ColStringByName['CodeSrc'];
        target := element.targetList.Append;
        target.codeSystem := conn.ColStringByName['UrlTgt'];
        target.code := conn.ColStringByName['CodeTgt'];
        target.equivalence := ConceptMapEquivalenceSubsumes;
      end;
    end;
    conn.terminate;
  finally
    elements.Free;
  end;
end;

{ TSubsumptionMatch }

constructor TSubsumptionMatch.Create(keysrc : integer; urisrc, codesrc : String; keytgt : integer; uritgt, codetgt : String);
begin
   inherited create;
   FKeysrc := keysrc;
   FUrisrc := urisrc;
   FCodesrc := codesrc;
   FKeytgt := keytgt;
   FUritgt := uritgt;
   FCodetgt := codetgt;
end;

end.
