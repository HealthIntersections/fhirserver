unit closuremanager;

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

Uses
  SysUtils, Generics.Collections,
  fsl_utilities, fsl_threads,
  fsl_base, 
  fdb_manager,
  fhir_common,
  tx_manager;

Type
  TSubsumptionMatch = class (TFslObject)
  private
    FKeySrc: integer;
    FcodeSrc: String;
    FuriSrc: String;
    FKeyTgt: integer;
    FcodeTgt: String;
    FuriTgt: String;
  public
    constructor Create(keysrc : integer; urisrc, codesrc : String; keytgt : integer; uritgt, codetgt : String);
    property KeySrc : integer read FKeySrc write FKeySrc;
    property uriSrc : String read FuriSrc write FuriSrc;
    property codeSrc : String read FcodeSrc write FcodeSrc;
    property KeyTgt : integer read FKeyTgt write FKeyTgt;
    property uriTgt : String read FuriTgt write FuriTgt;
    property codeTgt : String read FcodeTgt write FcodeTgt;
  end;

  TClosureManager = class (TFslObject)
  private
    FStore : TTerminologyServerStore; // not linked, as this is the owner
    FName : String;
    FKey : integer;
    FLock : TFslLock;
    FVersion : integer;
    function GetConceptKey(conn : TFDBConnection; uri, code : String) : integer;
    procedure processEntryInternal(conn: TFDBConnection; ClosureEntryKey, ConceptKey: integer; uri, code : String; map : TFHIRConceptMapW);
    function getGroup(map: TFHIRConceptMapW; source, target: String): TFHIRConceptMapGroupW;
  public
    constructor Create(name : String; key, version : integer; store : TTerminologyServerStore);
    destructor Destroy; override;

    function Link : TClosureManager; overload;

    Property Version : Integer read FVersion;

    procedure Init(conn : TFDBConnection);

    // this is a split mode implementation, for performance - the subsumption is calculated in the background
    function enterCode(conn: TFDBConnection; uri, code: String): integer;
    procedure processEntry(conn: TFDBConnection; ClosureEntryKey, ConceptKey: integer; uri, code: String);

    // this is the inline variant; subsumption is determined immediately
    procedure processConcepts(conn: TFDBConnection; codings : TFslList<TFHIRCodingW>; map : TFHIRConceptMapW);

    procedure reRun(conn: TFDBConnection; map : TFHIRConceptMapW; version : integer);
  end;

implementation

{ TClosureManager }

constructor TClosureManager.Create(name: String; key, version : integer; store : TTerminologyServerStore);
begin
  inherited Create;
  FLock := TFslLock.Create('Closure Table '+name);
  FName := name;
  FKey := key;
  FVersion := Version;
  FStore := store;
end;

procedure TClosureManager.Init(conn: TFDBConnection);
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

function TClosureManager.GetConceptKey(conn: TFDBConnection; uri, code: String): integer;
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

function TClosureManager.enterCode(conn: TFDBConnection; uri, code: String): integer;
begin
  result := GetConceptKey(conn, uri, code);
  // now, check that it is in the closure
  if conn.CountSQL('select Count(*) from ClosureEntries where ClosureKey = '+inttostr(FKey)+' and SubsumesKey = '+inttostr(result)) = 0 then
  begin
    // enter it into the closure table
    conn.ExecSQL('insert into ClosureEntries (ClosureEntryKey, ClosureKey, SubsumesKey, SubsumedKey, IndexedVersion) values ('+inttostr(FStore.NextClosureEntryKey)+', '+inttostr(FKey)+', '+inttostr(result)+', '+inttostr(result)+', 0)');
  end;
end;

procedure TClosureManager.processEntry(conn: TFDBConnection; ClosureEntryKey, ConceptKey: integer; uri, code : String);
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

procedure TClosureManager.processConcepts(conn: TFDBConnection; codings : TFslList<TFHIRCodingW>; map : TFHIRConceptMapW);
var
  coding : TFHIRCodingW;
  ck, cek : integer;
begin
  FLock.Lock;
  try
    inc(FVersion);
    map.version := inttostr(FVersion);

    for coding in codings do
    begin
      ck := GetConceptKey(conn, coding.systemUri, coding.code);
      if conn.CountSQL('select Count(*) from ClosureEntries where ClosureKey = '+inttostr(FKey)+' and SubsumesKey = '+inttostr(ck)) = 0 then
      begin
        // enter it into the closure table
        cek := FStore.NextClosureEntryKey;
        conn.ExecSQL('insert into ClosureEntries (ClosureEntryKey, ClosureKey, SubsumesKey, SubsumedKey, IndexedVersion) values ('+inttostr(cek)+', '+inttostr(FKey)+', '+inttostr(ck)+', '+inttostr(ck)+', '+inttostr(FVersion)+')');
        //now, check the subsumes....
        processEntryInternal(conn, cek, ck, coding.systemUri, coding.code, map);
      end;
    end;
    conn.ExecSQL('update Closures set Version = '+inttostr(FVersion)+' where ClosureKey = '+inttostr(FKey));
  finally
    FLock.Unlock;
  end;
end;

procedure TClosureManager.processEntryInternal(conn: TFDBConnection; ClosureEntryKey, ConceptKey: integer; uri, code : String; map : TFHIRConceptMapW);
var
  matches : TFslList<TSubsumptionMatch>;
  match : TSubsumptionMatch;
  group, g : TFhirConceptMapGroupW;
  element, e : TFhirConceptMapGroupElementW;
  target, t : TFhirConceptMapGroupElementTargetW;
begin
  group := nil;
  matches := TFslList<TSubsumptionMatch>.create;
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
          for g in map.groups.forEnum do
            for e in g.elements.forEnum do
              if (g.source = match.uriSrc) and (e.code = match.codeSrc) then
              begin
                group := g;
                element := e;
              end;
          target := nil;
          if (element <> nil) then
            for t in element.targets.forEnum do
              if (group.target = match.uriTgt) and (t.code = match.codeTgt) then
                target := t;
          if (target = nil) then
          begin
            if element = nil then
            begin
              group := getGroup(map, match.uriSrc, match.uritgt);
              element := group.addElement(match.codeSrc);
            end;
            {target := }element.addTarget(match.codetgt, cmeSpecializes);
          end;
        end;
      end;
    end;
  finally
    matches.Free;
  end;
end;

function TClosureManager.getGroup(map: TFHIRConceptMapW; source, target: String): TFhirConceptMapGroupW;
var
  g : TFhirConceptMapGroupW;
begin
  for g in map.groups.forEnum do
    if (g.source = source) and (g.target = target) then
      exit(g.link);
  result := map.addGroup(source, target);
end;

procedure TClosureManager.reRun(conn: TFDBConnection; map: TFHIRConceptMapW; version: integer);
var
  key : String;
  elements : TFslMap<TFhirConceptMapGroupElementW>;
  element : TFhirConceptMapGroupElementW;
begin
  elements := TFslMap<TFhirConceptMapGroupElementW>.create('tx.closure.run');
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
        element := getGroup(map, conn.ColStringByName['UrlSrc'], conn.ColStringByName['UrlTgt']).addElement(conn.ColStringByName['CodeSrc']);
      try
        elements.Add(key, element.link);
        element.addTarget(conn.ColStringByName['CodeTgt'], cmeSubsumes).Free;
      finally
        element.Free;
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
