unit ClosureManager;

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
    function getGroup(map: TFHIRConceptMap; source, target: String): TFHIRConceptMapGroup;
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
  matches : TAdvList<TSubsumptionMatch>;
  match : TSubsumptionMatch;
  group, g : TFhirConceptMapGroup;
  element, e : TFhirConceptMapGroupElement;
  target, t : TFhirConceptMapGroupElementTarget;
begin
  group := nil;
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
          {$IFDEF FHIR2}
            for e in g.elementList do
              if (e.system = match.uriSrc) and (e.code = match.codeSrc) then
          {$ELSE}
          for g in map.groupList do
            for e in g.elementList do
              if (g.source = match.uriSrc) and (e.code = match.codeSrc) then
          {$ENDIF}
              begin
                group := g;
                element := e;
              end;
          target := nil;
          if (element <> nil) then
            for t in element.targetList do
              {$IFDEF FHIR2}
              if (t.system = match.uriTgt) and (t.code = match.codeTgt) then
              {$ELSE}
              if (group.target = match.uriTgt) and (t.code = match.codeTgt) then
              {$ENDIF}
                target := t;
          if (target = nil) then
          begin
            if element = nil then
            begin
              group := getGroup(map, match.uriSrc, match.uritgt);
              element := group.elementList.Append;
              {$IFDEF FHIR2}
              element.system := match.uriSrc;
              {$ENDIF}
              element.code := match.codeSrc;
            end;
            target := element.targetList.Append;
            {$IFDEF FHIR2}
            target.system := match.uriTgt;
            {$ENDIF}
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

function TClosureManager.getGroup(map: TFHIRConceptMap; source, target: String): TFHIRConceptMapGroup;
var
  g : TFHIRConceptMapGroup;
begin
  {$IFDEF FHIR2}
    result := map;
  {$ELSE}
  for g in map.groupList do
    if (g.source = source) and (g.target = target) then
      exit(g);
  g := map.groupList.Append;
  g.source := source;
  g.target := target;
  exit(g);
  {$ENDIF}
end;

procedure TClosureManager.reRun(conn: TKDBConnection; map: TFHIRConceptMap; version: integer);
var
  key : String;
  elements : TAdvMap<TFhirConceptMapGroupElement>;
  element : TFhirConceptMapGroupElement;
  target : TFhirConceptMapGroupElementTarget;
begin
  elements := TAdvMap<TFhirConceptMapGroupElement>.create;
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
        element := getGroup(map, conn.ColStringByName['UrlSrc'], conn.ColStringByName['UrlTgt']).elementList.Append;
        {$IFDEF FHIR2}
        element.system := conn.ColStringByName['UrlSrc'];
        {$ENDIF}
        elements.Add(key, element.link);
        element.code := conn.ColStringByName['CodeSrc'];
        target := element.targetList.Append;
        {$IFDEF FHIR2}
        target.system := conn.ColStringByName['UrlTgt'];
        {$ENDIF}
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
