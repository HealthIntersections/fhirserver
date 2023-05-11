unit SnomedCombiner;

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
  Windows, SysUtils, Classes,
  fsl_base, fsl_utilities,
  ftx_sct_services;

Type
  TTabWriter = class (TFslObject)
  private
    FStream : TFileStream;
    FDiv : boolean;
  public
    constructor Create(filename : String);
    destructor Destroy; override;

    procedure field(s : String);
    procedure endRecord;
  end;

  TSnomedCombinedStoreEntry = class (TFslObject)
  private
    id : int64;
  public
    constructor Create(s : String); overload;
    constructor Create(i : int64); overload;
  end;

  TSnomedCombinedItem = class (TFslObject)
  private
    FId : Int64;
  public
    property id : Int64 read FId write FId;
  end;

  TSnomedCombinedConcept = class;

  TSnomedCombinedDescription = class (TSnomedCombinedItem)
  private
    FDate : TSnomedDate;
    FModule : Int64;
    FKind : TSnomedCombinedConcept; // not linked
    FCaps : TSnomedCombinedConcept; // not linked
    FActive : boolean;
    FLang : byte;
    FValue : String;
  public
    function link : TSnomedCombinedDescription; overload;
  end;

  TSnomedCombinedRelationship = class (TSnomedCombinedItem)
  private
    FTarget : TSnomedCombinedConcept;
    FRelType : TSnomedCombinedConcept;
    FModule : Int64;
    FKind : TSnomedCombinedConcept;
    FModifier : TSnomedCombinedConcept;
    FDate : TSnomedDate;
    FActive : boolean;
    source : TSnomedServices;
  public
    constructor Create(source : TSnomedServices);
    function link : TSnomedCombinedRelationship; overload;
    function copy : TSnomedCombinedRelationship;
  end;

  TSnomedCombinedRelationshipGroup = class (TFslObject)
  private
    FRelationships : TFslList<TSnomedCombinedRelationship>;
    FGroup : Integer;
    source : TSnomedServices;
  public
    constructor Create(source : TSnomedServices);
    destructor Destroy; Override;
    function link : TSnomedCombinedRelationshipGroup; overload;
    function copy : TSnomedCombinedRelationshipGroup;
  end;

  TSnomedCombinedConcept = class (TSnomedCombinedItem)
  private
    FDescriptions : TFslList<TSnomedCombinedDescription>;
    FFlags : byte;
    FModule : Int64;
    FDate : TSnomedDate;
    source : TSnomedServices;
    FNoGroup : TSnomedCombinedRelationshipGroup;
    FGroups : TFslList<TSnomedCombinedRelationshipGroup>;
    FChildren : TFslList<TSnomedCombinedConcept>;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function group(svc : TSnomedServices; grp : integer) : TSnomedCombinedRelationshipGroup;
    function link : TSnomedCombinedConcept; overload;
  end;

  TSnomedCombinedReferenceSetEntry = class (TFslObject)
  private
    id : TGuid;
    date : TSnomedDate;
    module : int64;
    FItem : TSnomedCombinedItem;
    FValues : TStringList;
  public
    constructor Create; Override;
    destructor Destroy; Override;
  end;

  TSnomedCombinedReferenceSet = class (TFslObject)
  private
    FName : String;
    FFilename : String;
    FDefinition : TSnomedCombinedConcept;
    FTypes : TStringList;
    FFields : TStringList;
    DefaultLanguage : boolean;
    FMembers : TFslMap<TSnomedCombinedReferenceSetEntry>;
    function getByItem(item : TSnomedCombinedItem; values : TStringList) : TSnomedCombinedReferenceSetEntry;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function abbrev : String;
  end;

  TSnomedCombinedDependency = class (TFslObject)
  private
    id : TGuid;
    FDate : TSnomedDate;
    source, target : Int64;
    sourceName, targetName : String;
    startDate, endDate : TSnomedDate;
    function key : String;
  public
  end;

  TSnomedCombiner = class (TFslObject)
  private
    FInternational: TSnomedServices;
    FOthers: TFslList<TSnomedServices>;

    FStore : TFslMap<TSnomedCombinedStoreEntry>;
    FDependencies : TFslMap<TSnomedCombinedDependency>;
    FConcepts : TFslMap<TSnomedCombinedConcept>;
    FDescriptions : TFslMap<TSnomedCombinedDescription>;
    FRelationships : TFslMap<TSnomedCombinedRelationship>;
    FRefSets : TFslMap<TSnomedCombinedReferenceSet>;

    FCallback: TInstallerCallback;
    FCurrent, FTotal, FPercent, FLast, FPathCount, FDescCount, FRelnCount : cardinal;
    FMessage : String;
    FSummary : TStringList;
    FIssues : TStringList;
    FDestination: String;
    FStoreLocation: String;

    nextRelationship : integer;
    nextDescription : integer;
    FModuleId : int64;
    function generateId(k, t : integer) : int64; // t: 0 = concept, 1 = description, 2 = relationship
    function getDescriptionId(d : String) : int64;
    function getRelationshipId(s, rt, t : int64; grp : integer) : int64;
    procedure determineTotal;

    procedure loadDependency(svc: TSnomedServices; rm : TSnomedReferenceSetMember);
    procedure loadDependencies(svc : TSnomedServices); overload;
    procedure loadDependencies; overload;

    function LoadConcept(svc : TSnomedServices; i : integer) : TSnomedCombinedConcept;
    Procedure UpdateConcept(svc : TSnomedServices; i : integer; concept : TSnomedCombinedConcept);
    procedure loadConcepts;

    function LoadDescription(svc : TSnomedServices; i : integer) : boolean;
    procedure loadDescriptions;

    function LoadRelationship(svc : TSnomedServices; i : integer) : boolean;
    procedure loadRelationships;

    function LoadReferenceSet(svc : TSnomedServices; i : integer) : boolean;
    procedure loadReferenceSets;

    procedure checkForCircles; overload;
    procedure checkForCircles(svc : TSnomedServices); overload;
    procedure checkForCircles(root : UInt64); overload;
    procedure checkForCircles(focus : TSnomedCombinedConcept; parents : Array of TSnomedCombinedConcept); overload;

    function subsumes(this, other : TSnomedCombinedConcept) : boolean;
    function matches(this, other : TSnomedCombinedRelationship) : boolean; overload;

    procedure step(desc : String);
    procedure classify; overload;
    procedure classify(concept : TSnomedCombinedConcept); overload;
    procedure forceRelationship(concept : TSnomedCombinedConcept; group : TSnomedCombinedRelationshipGroup; relationship : TSnomedCombinedRelationship);
    function exists(list  : TFslList<TSnomedCombinedRelationship>; relationship : TSnomedCombinedRelationship) : boolean;

    procedure addToRefSet(rsId, conceptId : UInt64);
    procedure identify;

    procedure updateDependencies;

    procedure saveToRF2;
    procedure SetInternational(const Value: TSnomedServices);
    procedure recordIssue(s : String);
    procedure recordSummary(s : String);

    procedure loadStore;
    procedure saveStore;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Execute;

    property international : TSnomedServices read FInternational write SetInternational;
    property others : TFslList<TSnomedServices> read FOthers;
    property callback : TInstallerCallback read FCallback write FCallback;

    property issues : TStringList read FIssues;
    property summary : TStringList read FSummary;

    property destination : String read FDestination write FDestination;
    property store : String read FStoreLocation write FStoreLocation;
  end;

implementation

{ TTabWriter }

constructor TTabWriter.Create(filename: String);
begin
  inherited create;
  FStream := TFileStream.Create(filename, fmCreate);
  FDiv := false;
end;

destructor TTabWriter.destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TTabWriter.field(s: String);
var
  b : TBytes;
begin
  if FDiv then
  begin
    setLength(b, 1);
    b[0] := 9;
    FStream.Write(b[0], 1);
  end;
  b := TEncoding.UTF8.GetBytes(s);
  FStream.Write(b[0], length(b));
  FDiv := true;
end;

procedure TTabWriter.endRecord;
var
  b : TBytes;
begin
  setLength(b, 2);
  b[0] := 13;
  b[1] := 10;
  FStream.Write(b[0], 2);
  FDiv := false;
end;

{ TSnomedCombinedRelationship }

constructor TSnomedCombinedRelationship.Create(source : TSnomedServices);
begin
  inherited Create;
  self.source := source;
end;

function TSnomedCombinedRelationship.link: TSnomedCombinedRelationship;
begin
  result := TSnomedCombinedRelationship(inherited Link);
end;

function TSnomedCombinedRelationship.copy: TSnomedCombinedRelationship;
begin
  result := TSnomedCombinedRelationship.Create(source);
  result.FId := FId;
  result.FTarget := FTarget.link;
  result.FRelType := FRelType.link;
  result.FModule := FModule;
  result.FKind := FKind.link;
  result.FModifier := FModifier.link;
  result.FDate := FDate;
  result.FActive := FActive;
end;

{ TSnomedCombinedRelationshipGroup }

constructor TSnomedCombinedRelationshipGroup.Create(source : TSnomedServices);
begin
  inherited Create;
  self.source := source;
  FRelationships := TFslList<TSnomedCombinedRelationship>.create;
end;

destructor TSnomedCombinedRelationshipGroup.Destroy;
begin
  FRelationships.Free;
  inherited;
end;

function TSnomedCombinedRelationshipGroup.link: TSnomedCombinedRelationshipGroup;
begin
  result := TSnomedCombinedRelationshipGroup(inherited link);
end;

function TSnomedCombinedRelationshipGroup.copy: TSnomedCombinedRelationshipGroup;
var
  t : TSnomedCombinedRelationship;
begin
  result := TSnomedCombinedRelationshipGroup.create(source);
  for t in FRelationships do
    result.FRelationships.add(t.copy);
  result.FGroup := FGroup;
end;

{ TSnomedCombinedConcept }

constructor TSnomedCombinedConcept.Create;
begin
  inherited;
  FDescriptions := TFslList<TSnomedCombinedDescription>.create;
  FGroups := TFslList<TSnomedCombinedRelationshipGroup>.create;
  FChildren := TFslList<TSnomedCombinedConcept>.create;
end;

destructor TSnomedCombinedConcept.Destroy;
begin
  FChildren.Free;
  FGroups.Free;
  FDescriptions.Free;
  inherited;
end;

function TSnomedCombinedConcept.group(svc : TSnomedServices; grp: integer): TSnomedCombinedRelationshipGroup;
var
  t : TSnomedCombinedRelationshipGroup;
begin
  for t in FGroups do
    if t.FGroup = grp then
      exit(t);
  result := TSnomedCombinedRelationshipGroup.Create(svc);
  result.FGroup := grp;
  FGroups.add(result);
end;

function TSnomedCombinedConcept.link: TSnomedCombinedConcept;
begin
  result := TSnomedCombinedConcept(inherited Link);
end;

{ TSnomedCombiner }

constructor TSnomedCombiner.Create;
begin
  inherited;
  FStore := TFslMap<TSnomedCombinedStoreEntry>.create;
  FOthers := TFslList<TSnomedServices>.create;
  FConcepts := TFslMap<TSnomedCombinedConcept>.create(500000);
  FDependencies := TFslMap<TSnomedCombinedDependency>.Create;
  FDescriptions := TFslMap<TSnomedCombinedDescription>.create(1500000);
  FRelationships := TFslMap<TSnomedCombinedRelationship>.create(3000000);
  FRefSets := TFslMap<TSnomedCombinedReferenceSet>.create(1000);
  FSummary := TStringList.create;
  FIssues := TStringList.create;
end;

destructor TSnomedCombiner.Destroy;
begin
  FStore.Free;
  FRefSets.Free;
  FDescriptions.Free;
  FDependencies.Free;
  FRelationships.Free;
  FSummary.free;
  FIssues.free;
  FConcepts.Free;
  FInternational.free;
  FOthers.free;
  inherited;
end;

procedure TSnomedCombiner.SetInternational(const Value: TSnomedServices);
begin
  FInternational.free;
  FInternational := Value;
end;

procedure TSnomedCombiner.recordIssue(s: String);
begin
  FIssues.Add(s);
end;

procedure TSnomedCombiner.recordSummary(s: String);
begin
  FSummary.Add(s);
end;

procedure TSnomedCombiner.step(desc: String);
var
  pct : integer;
begin
  inc(FCurrent);
  pct := trunc(FCurrent * 100 / FTotal);
  if (pct <> FPercent) or (GetTickCount > FLast + 1000) or (FMessage <> desc) then
  begin
    callback(pct, desc);
    FMessage := desc;
    FPercent := pct;
    FLast := GetTickCount;
  end;
end;

procedure TSnomedCombiner.Execute;
begin
  // 1. merging

  determineTotal;
  loadStore;

  loadDependencies;

  loadConcepts;
  loadDescriptions;
  loadRelationships;
  loadReferenceSets;

  // 2. classifying
  checkForCircles;
  classify;

  identify;
  updateDependencies;

  // 3. save to RF2
  saveStore;
  saveToRF2;
end;


procedure TSnomedCombiner.determineTotal;
var
  svc : TSnomedServices;
begin
  FTotal := (FInternational.Concept.Count * 2) + FInternational.Desc.Count + FInternational.ChildRelationshipCount + FInternational.Rel.Count + FInternational.RefSetCount;
  for svc in others do
    FTotal := FTotal + (svc.Concept.Count * 2) + svc.Desc.Count + svc.ChildRelationshipCount + svc.Rel.Count + svc.RefSetCount;
  FTotal := FTotal + 3; // admin overhead
  FCurrent := 0;
  FPercent := 0;
  FLast := GetTickCount;
end;

procedure TSnomedCombiner.UpdateConcept(svc : TSnomedServices; i : integer; concept : TSnomedCombinedConcept);
var
  Identity : UInt64;
  Flags : Byte;
  effectiveTime : TSnomedDate;
  Parents, Descriptions, Inbounds, outbounds, refsets : Cardinal;
begin
  svc.Concept.GetConcept(i, Identity, Flags, effectiveTime, Parents, Descriptions, Inbounds, outbounds, refsets);
  concept.Fflags := flags or concept.Fflags;
  if (concept.FDate < effectiveTime) then
    concept.FDate := effectiveTime;
end;

procedure TSnomedCombiner.updateDependencies;
var
  refset : TSnomedCombinedReferenceSet;
  fsm : TSnomedCombinedReferenceSetEntry;
  dep : TSnomedCombinedDependency;
begin
  refset := FRefSets['900000000000534007'];
  refset.FMembers.Clear;
  for dep in FDependencies.Values do
  begin
    fsm := TSnomedCombinedReferenceSetEntry.Create;
    fsm.id := dep.id;
    refset.FMembers.add(GUIDToString(fsm.id), fsm);
    fsm.date := dep.FDate;
    fsm.module := dep.source;
    fsm.FItem := FConcepts[inttostr(dep.target)];
    fsm.FValues := TStringList.create;
    fsm.FValues.Add(formatDateTime('yyyymmdd', dep.startDate));
    fsm.FValues.Add(formatDateTime('yyyymmdd', dep.endDate));
  end;
end;

function TSnomedCombiner.LoadConcept(svc: TSnomedServices; i: integer): TSnomedCombinedConcept;
var
  Identity : UInt64;
  Flags : Byte;
  effectiveTime : TSnomedDate;
  Parents, Descriptions, Inbounds, outbounds, refsets : Cardinal;
begin
  svc.Concept.GetConcept(i, Identity, Flags, effectiveTime, Parents, Descriptions, Inbounds, outbounds, refsets);
  result := TSnomedCombinedConcept.Create;
  result.FNoGroup := TSnomedCombinedRelationshipGroup.Create(svc);
  result.FGroups.Add(result.FNoGroup);
  result.FId := Identity;
  result.FFlags := Flags;
  result.FDate := effectiveTime;
  result.FModule := svc.Concept.getConceptId(svc.Concept.GetModuleId(i));
  result.source := svc;
end;

procedure TSnomedCombiner.loadConcepts;
var
  i : integer;
  concept : TSnomedCombinedConcept;
  svc : TSnomedServices;
  id : Int64;
begin
  recordSummary(FInternational.EditionName+' concepts: '+inttostr(FInternational.Concept.Count));
  for i := 0 to FInternational.Concept.Count - 1 do
  begin
    step('Process Concepts for '+FInternational.EditionName);
    concept := LoadConcept(FInternational, i * CONCEPT_SIZE);
    FConcepts.Add(inttostr(concept.id), concept);
  end;
  for svc in FOthers do
  begin
    recordSummary(svc.EditionName+' concepts: '+inttostr(svc.Concept.Count));
    for i := 0 to svc.Concept.Count - 1 do
    begin
      step('Process Concepts for '+svc.EditionName);
      id := svc.Concept.getConceptId(i * CONCEPT_SIZE);
      if FConcepts.TryGetValue(inttostr(id), concept) then
        updateConcept(svc, i * CONCEPT_SIZE, concept)
      else
      begin
        concept := LoadConcept(svc, i * CONCEPT_SIZE);
        FConcepts.Add(inttostr(concept.id), concept);
      end;
    end;
  end;
  recordSummary('Total Concepts: '+inttostr(FConcepts.Count));
end;

procedure TSnomedCombiner.loadDependencies(svc: TSnomedServices);
var
  rm : TSnomedReferenceSetMember;
  rml : TSnomedReferenceSetMemberArray;
begin
  rml := svc.getRefSet(900000000000534007);
  for rm in rml do
    loadDependency(svc, rm);
end;

procedure TSnomedCombiner.loadDependencies;
var
  svc : TSnomedServices;
begin
  step('Checking Dependencies');
  loadDependencies(international);
  for svc in others do
    loadDependencies(svc);
end;

procedure TSnomedCombiner.loadDependency(svc: TSnomedServices; rm: TSnomedReferenceSetMember);
var
  str : TCardinalArray;
  dep, depE : TSnomedCombinedDependency;
begin
  dep := TSnomedCombinedDependency.Create;
  try
    dep.id := rm.id;
    dep.FDate := rm.date;
    dep.source := svc.Concept.getConceptId(rm.module);
    dep.target := svc.Concept.getConceptId(rm.Ref);
    dep.sourceName := svc.GetPNForConcept(rm.module);
    dep.targetName := svc.GetPNForConcept(rm.Ref);
    str := svc.Refs.GetReferences(rm.values);
    dep.startDate := readDate(svc.Strings.GetEntry(str[0]));
    dep.endDate := readDate(svc.Strings.GetEntry(str[2]));
    if (FDependencies.TryGetValue(dep.key, depE)) then
    begin
      if (dep.startDate <> depE.startDate) then
        depE.startDate := dep.startDate;
      if (dep.endDate <> depE.endDate) then
        depE.endDate := dep.endDate;
    end
    else
      FDependencies.Add(dep.key, dep.Link as TSnomedCombinedDependency);
  finally
    dep.Free;
  end;
end;

function TSnomedCombiner.LoadDescription(svc: TSnomedServices; i: integer): boolean;
var
  iDesc : Cardinal;
  id, cid : UInt64;
  date : TSnomedDate;
  concept, module, kind, caps, refsets, valueses : Cardinal;
  active : boolean;
  c : TSnomedCombinedConcept;
  d, t : TSnomedCombinedDescription;
  lang : byte;
begin
  svc.Desc.getDescription(i, iDesc, id, date, concept, module, kind, caps, refsets, valueses, active, lang);
  cid := svc.Concept.getConceptId(concept);
  if not FConcepts.TryGetValue(inttostr(cid), c) then
    raise ETerminologySetup.create('no find concept '+inttostr(cid))
  else
  begin
    d := nil;
    for t in c.Fdescriptions do
      if t.Fid = id then
      begin
        d := t;
        break;
      end;
    result := d = nil;
    if d <> nil then
    begin
      if not SameText(d.FValue, svc.Strings.GetEntry(iDesc)) then
        recordIssue('Module '+svc.EditionName+' changes value of description '+inttostr(d.FId)+' from "'+d.FValue+'" to "'+ svc.Strings.GetEntry(iDesc)+'"');
    end
    else
    begin
      d := TSnomedCombinedDescription.create;
      c.FDescriptions.Add(d);
      FDescriptions.Add(inttostr(id), d.Link);
      d.FId := id;
      d.FDate := date;
      d.FModule := svc.Concept.getConceptId(module);
      d.FKind := FConcepts[inttostr(svc.Concept.getConceptId(kind))];
      d.FCaps := FConcepts[inttostr(svc.Concept.getConceptId(caps))];
      d.FActive := active;
      d.FLang := lang;
      d.FValue := svc.Strings.GetEntry(iDesc);
    end;
  end;
end;

procedure TSnomedCombiner.loadDescriptions;
var
  i, t : integer;
  svc : TSnomedServices;
begin
  t := 0;
  recordSummary(FInternational.EditionName+' descriptions: '+inttostr(FInternational.Desc.Count));
  for i := 0 to FInternational.Desc.Count - 1 do
  begin
    step('Process Descriptions for '+FInternational.EditionName);
    if LoadDescription(FInternational, i*DESC_SIZE) then
      inc(t);
  end;

  for svc in FOthers do
  begin
    recordSummary(svc.EditionName+' descriptions: '+inttostr(svc.Desc.Count));
    for i := 0 to svc.Desc.Count - 1 do
    begin
      step('Process Descriptions for '+svc.EditionName);
      if LoadDescription(svc, i*DESC_SIZE) then
        inc(t);
    end;
  end;
  recordSummary('Total Descriptions: '+inttostr(t));
  FDescCount := t;
end;

function TSnomedCombiner.LoadReferenceSet(svc: TSnomedServices; i: integer): boolean;
var
  definition, members, dummy, types, t, iFilename, name, names : cardinal;
  ui, s, uid : string;
  rs : TSnomedCombinedReferenceSet;
  nl, tl, vl :  TCardinalArray;
  ml : TSnomedReferenceSetMemberArray;
  m : TSnomedReferenceSetMember;
  rm : TSnomedCombinedReferenceSetEntry;
  j : integer;
  new : boolean;
  item : TSnomedCombinedItem;
  guid  : TGuid;
begin
  svc.RefSetIndex.GetReferenceSet(i, name, iFilename, definition, members, dummy, types, names);

  ui := svc.GetConceptId(definition);
  if (ui = '900000000000534007') then
    s := 'test';
  new := not FRefSets.ContainsKey(ui);
  if new then
  begin
    rs := TSnomedCombinedReferenceSet.Create;
    FRefSets.Add(ui, rs);
    rs.FDefinition := FConcepts[ui];
    rs.FName := svc.Strings.GetEntry(name);
    rs.FFilename := svc.Strings.GetEntry(iFilename);
    if rs.FName = '' then
      rs.FName := ui;
  end
  else
    rs := FRefSets[ui];

  if (svc = FInternational) and (definition = FInternational.DefaultLanguageRefSet) then
    rs.DefaultLanguage := true;

  if (types <> 0)  then
  begin
    tl := svc.Refs.GetReferences(types);
    j := 0;
    for t in tl do
    begin
      s := chr(t);
      if new then
        rs.FTypes.Add(s)
      else if rs.FTypes[j] <> s then
        recordIssue('Value set mismatch - type '+inttostr(i)+' for '+rs.FName+' ('+inttostr(rs.FDefinition.id)+') was '+rs.FTypes[j]+', now found '+s);
      inc(j);
    end;
    nl := svc.Refs.GetReferences(names);
    j := 0;
    for t in nl do
    begin
      s := svc.Strings.GetEntry(t);
      if new then
        rs.FFields.Add(s)
      else if rs.FFields[j] <> s then
        recordIssue('Value set mismatch - field '+inttostr(i)+' for '+rs.FName+' ('+inttostr(rs.FDefinition.id)+') was '+rs.FFields[j]+', now found '+s);
      inc(j);
    end;
  end;

  ml := svc.RefSetMembers.GetMembers(members);
  result := false;
  for m in ml do
  begin
    step('Process Reference Sets for '+svc.EditionName);
    guid := m.id;
    if IsNilGUID(guid) then
      guid := CreateGUID;
    uid := GuidToString(guid);
    if new or not rs.FMembers.ContainsKey(uid) then
    begin
      result := true;
      rm := TSnomedCombinedReferenceSetEntry.Create;
      rs.FMembers.Add(uid, rm);
      rm.id := guid;
      rm.date := m.date;
      if (m.module <> 0) then
        rm.module := svc.Concept.getConceptId(m.module);
      case m.kind of
        0: rm.FItem := FConcepts[svc.GetConceptId(m.ref)];
        1: rm.FItem := FDescriptions[svc.GetDescriptionId(m.ref)];
        2: rm.FItem := FRelationships[svc.GetRelationshipId(m.ref)];
      end;
      if (m.values <> 0) then
      begin
        rm.FValues := TStringList.Create;
        vl := svc.Refs.GetReferences(m.values);
        for j := 0 to length(tl) - 1 do
          case vl[j*2+1] of
            1 {concept} : rm.FValues.add(svc.GetConceptId(vl[j*2]));
            2 {desc}    : rm.FValues.add(svc.GetDescriptionId(vl[j*2]));
            3 {rel}     : rm.FValues.add(svc.GetRelationshipId(vl[j*2]));
            4 {integer} : rm.FValues.add(inttostr(vl[j*2]));
            5 {string}  : rm.FValues.add(svc.Strings.GetEntry(vl[j*2]));
          else
            raise ETerminologySetup.create('Unknown Cell Type '+inttostr(vl[j*2+1]));
          end;
      end;
    end
  end;
end;

procedure TSnomedCombiner.loadReferenceSets;
var
  i, t : integer;
  svc : TSnomedServices;
begin
  t := 0;
  recordSummary(FInternational.EditionName+' Reference Sets : '+inttostr(FInternational.Rel.Count));
  for i := 0 to FInternational.RefSetIndex.Count - 1 do
  begin
    step('Process Reference Sets for '+FInternational.EditionName);
    if LoadReferenceSet(FInternational, i) then
      inc(t);
  end;

  for svc in FOthers do
  begin
    recordSummary(svc.EditionName+' reference sets: '+inttostr(svc.Rel.Count));
    for i := 0 to svc.RefSetIndex.Count - 1 do
    begin
      step('Process Reference Sets for '+svc.EditionName);
      if LoadReferenceSet(svc, i) then
        inc(t);
    end;
  end;
  recordSummary('Total Reference Sets: '+inttostr(t));
  FRelnCount := t;

end;

function TSnomedCombiner.LoadRelationship(svc: TSnomedServices; i: integer): boolean;
var
//  iDesc : Cardinal;
  cid : UInt64;
//  date : TSnomedDate;
//  concept, module, kind, refsets, valueses : Cardinal;
//  iFlags : Byte;
  c, ct, cr : TSnomedCombinedConcept;
  r, t : TSnomedCombinedRelationship;
  identity : UInt64;
  Source, Target, RelType, module, kind, modifier : Cardinal;
  date : TSnomedDate;
  Flags : Byte;
  Group : Integer;
  active, defining : boolean;
  g : TSnomedCombinedRelationshipGroup;
begin
  svc.Rel.GetRelationship(i, identity, Source, Target, RelType, module, kind, modifier, date, active, defining, group);

//  if (RelType = svc.Is_a_Index) then
//  // these are handled differently (in reverse, so we can iterate the children)
//  begin
//    if group <> 0 then
//      raise ETerminologySetup.create('is_a in a group - '+inttostr(identity));
//    cid := svc.Concept.getConceptId(target);
//    if not FConcepts.TryGetValue(inttostr(cid), c) then
//      raise ETerminologySetup.create('no find concept '+inttostr(cid));
//    cid := svc.Concept.getConceptId(source);
//    if not FConcepts.TryGetValue(inttostr(cid), ct) then
//      raise ETerminologySetup.create('no find concept '+inttostr(cid));
//    cid := svc.Concept.getConceptId(reltype);
//    if not FConcepts.TryGetValue(inttostr(cid), cr) then
//      raise ETerminologySetup.create('no find concept '+inttostr(cid));
//
//    r := nil;
//    for t in c.FNoGroup.FRelationships do
//      if (t.FTarget = ct) and t.FIsChild then
//      begin
//        r := t;
//        break;
//      end;
//    result := r = nil; // don't count this
//    if r = nil then
//    begin
//      r := TSnomedCombinedRelationship.Create(svc);
//      r.FIsChild := true; // mark that it's the wrong way around
//      c.FNoGroup.FRelationships.Add(r);
//      r.FId := identity;
//      r.FDate := date;
//      r.FRelType := cr;
//      r.Fmodule := svc.Concept.getConceptId(module);
//      r.FTarget := ct;
//      if (kind <> 0) then
//        r.Fkind := FConcepts[inttostr(svc.Concept.getConceptId(kind))];
//      if (modifier <> 0) then
//        r.Fmodifier := FConcepts[inttostr(svc.Concept.getConceptId(modifier))];
//      r.FDate := date;
//      r.FActive := active;
//    end;
//  end
//  else
//  begin
    cid := svc.Concept.getConceptId(source);
    if not FConcepts.TryGetValue(inttostr(cid), c) then
      raise ETerminologySetup.create('no find concept '+inttostr(cid));
    cid := svc.Concept.getConceptId(target);
    if not FConcepts.TryGetValue(inttostr(cid), ct) then
      raise ETerminologySetup.create('no find concept '+inttostr(cid));
    cid := svc.Concept.getConceptId(RelType);
    if not FConcepts.TryGetValue(inttostr(cid), cr) then
      raise ETerminologySetup.create('no find concept '+inttostr(cid));

    if group = 0 then
      g := c.FNoGroup
    else
      g := c.group(svc, group);

    r := nil;
    for t in g.FRelationships do
      if (t.Fid = identity) then
      begin
        r := t;
        break;
      end;
    result := r = nil;
    if r <> nil then
    begin
      if r.FDate < date then
        r.FDate := date;
      if not r.FActive and active then
        r.FActive := active;
      if r.FModule <> svc.Concept.getConceptId(module) then
        recordIssue('Module '+svc.EditionName+' changes module of description '+inttostr(r.FId)+' from '+inttostr(r.FModule) +' to '+ inttostr(svc.Concept.getConceptId(module)));
      if r.FTarget <> FConcepts[inttostr(svc.Concept.getConceptId(target))] then
        recordIssue('Module '+svc.EditionName+' changes Target of relationship '+inttostr(r.FId)+' from '+inttostr(r.FTarget.id) +' to '+ inttostr(FConcepts[inttostr(svc.Concept.getConceptId(target))].id));
      if r.FRelType <> FConcepts[inttostr(svc.Concept.getConceptId(RelType))] then
        recordIssue('Module '+svc.EditionName+' changes RelType of relationship '+inttostr(r.FId)+' from '+inttostr(r.FRelType.id) +' to '+ inttostr(FConcepts[inttostr(svc.Concept.getConceptId(RelType))].id));

      if (r.FKind = nil) and (kind <> 0) then
        recordIssue('Module '+svc.EditionName+' assigns kind to relationship '+inttostr(r.FId)+' (to '+ inttostr(kind)+')')
      else if (r.FKind <> nil) and (kind = 0) then
        recordIssue('Module '+svc.EditionName+' removes kind from relationship '+inttostr(r.FId)+' (is '+ inttostr(r.FKind.id)+')')
      else if (r.FKind <> nil) and (r.FKind <> FConcepts[inttostr(svc.Concept.getConceptId(kind))]) then
        recordIssue('Module '+svc.EditionName+' changes kind of relationship '+inttostr(r.FId)+' from '+inttostr(r.FKind.id) +' to '+ inttostr(FConcepts[inttostr(svc.Concept.getConceptId(kind))].id));

      if (r.Fmodifier = nil) and (modifier <> 0) then
        recordIssue('Module '+svc.EditionName+' assigns modifier to relationship '+inttostr(r.FId)+' (to '+ inttostr(modifier)+')')
      else if (r.Fmodifier <> nil) and (modifier = 0) then
        recordIssue('Module '+svc.EditionName+' removes modifier from relationship '+inttostr(r.FId)+' (is '+ inttostr(r.Fmodifier.id)+')')
      else if (r.Fmodifier <> nil) and (r.Fmodifier <> FConcepts[inttostr(svc.Concept.getConceptId(modifier))]) then
        recordIssue('Module '+svc.EditionName+' changes modifier of relationship '+inttostr(r.FId)+' from '+inttostr(r.Fmodifier.id) +' to '+ inttostr(FConcepts[inttostr(svc.Concept.getConceptId(modifier))].id));
    end
    else
    begin
      r := TSnomedCombinedRelationship.create(svc);
      g.FRelationships.Add(r);
      FRelationships.add(inttostr(identity), r.link);
      r.FId := identity;
      r.FDate := date;
      r.FTarget := ct;
      r.FRelType := cr;
      r.Fmodule := svc.Concept.getConceptId(module);
      if (kind <> 0) then
        r.Fkind := FConcepts[inttostr(svc.Concept.getConceptId(kind))];
      if (modifier <> 0) then
        r.Fmodifier := FConcepts[inttostr(svc.Concept.getConceptId(modifier))];
      r.FDate := date;
      r.FActive := active;
      if (RelType <> svc.Is_a_Index) and active then
        r.FTarget.FChildren.Add(c.link)
    end;
//  end;
end;

procedure TSnomedCombiner.loadRelationships;
var
  i, t : integer;
  svc : TSnomedServices;
begin
  t := 0;
  recordSummary(FInternational.EditionName+' relationships: '+inttostr(FInternational.Rel.Count));
  for i := 0 to FInternational.Rel.Count - 1 do
  begin
    step('Process Relationships for '+FInternational.EditionName);
    if LoadRelationship(FInternational, i*RELATIONSHIP_SIZE) then
      inc(t);
  end;

  for svc in FOthers do
  begin
    recordSummary(svc.EditionName+' relationships: '+inttostr(svc.Rel.Count));
    for i := 0 to svc.Rel.Count - 1 do
    begin
      step('Process Relationships for '+svc.EditionName);
      if LoadRelationship(svc, i*RELATIONSHIP_SIZE) then
        inc(t);
    end;
  end;
  recordSummary('Total Relationships: '+inttostr(t));
  FRelnCount := t;
end;

procedure TSnomedCombiner.loadStore;
var
  s, l, r : String;
  sl : TStringList;
begin
  Step('Loading Persistent Identity Store');
  if FileExists(FStoreLocation) then
  begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(FStoreLocation);
      nextRelationship := StrToInt(sl[0]);
      nextDescription := StrToInt(sl[1]);
      sl.Delete(0);
      sl.Delete(0);
      for s in sl do
      begin
        StringSplit(s, '=', l, r);
        FStore.Add(r, TSnomedCombinedStoreEntry.Create(l));
      end;
    finally
      sl.Free;
    end;
  end;
  FModuleId := generateId(1, 0);
end;

procedure TSnomedCombiner.checkForCircles;
var
  svc : TSnomedServices;
begin
  callback(0, '2nd Pass: Searching for Circular definitions');
  checkForCircles(FInternational);
  for svc in others do
    checkForCircles(svc);
end;

procedure TSnomedCombiner.checkForCircles(svc : TSnomedServices);
var
  i : int64;
begin
  for i in svc.ActiveRoots do
    checkForCircles(i);
  for i in svc.InactiveRoots do
    checkForCircles(i);
end;

procedure TSnomedCombiner.checkForCircles(root : UInt64);
var
  concept : TSnomedCombinedConcept;
  child : TSnomedCombinedConcept;
begin
  concept := FConcepts[inttostr(root)];
  for child in concept.FChildren do
    checkForCircles(child, [concept]);
end;

procedure TSnomedCombiner.addToRefSet(rsId, conceptId: UInt64);
var
  entry : TSnomedCombinedReferenceSetEntry;
begin
  entry := TSnomedCombinedReferenceSetEntry.Create;
  entry.id := CreateGUID;
  FRefSets[inttostr(rsId)].FMembers.add(GuidToString(entry.id), entry);
  entry.FItem := FConcepts[inttostr(conceptId)].link;
end;

procedure TSnomedCombiner.checkForCircles(focus : TSnomedCombinedConcept; parents : Array of TSnomedCombinedConcept);
var
  c : TSnomedCombinedConcept;
  s : String;
  f : boolean;
  np : Array of TSnomedCombinedConcept;
  i : integer;
  child : TSnomedCombinedConcept;
begin
  inc(FPathCount);
  f := false;
  for c in parents do
    if focus = c then
      f := true;
  if f then
  begin
    s := '';
    for c in parents do
      if s = '' then
        s := inttostr(c.FID)
      else
        s := s+','+inttostr(c.FID);
    raise ETerminologySetup.create('Circular Dependency found: SCT concept '+inttostr(focus.FId)+' has itself as a parent ('+s+')');
  end;
  SetLength(np, length(parents)+1);
  for i := 0 to length(parents) - 1 do
    np[i] := parents[i];
  np[length(parents)] := focus;
  for child in focus.FChildren do
    checkForCircles(child, np);
end;

function intCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  i1, i2, i3 : int64;
begin
  i1 := StrToInt64(list[index1]);
  i2 := StrToInt64(list[index2]);
  i3 := i1-i2;
  if (i3 > 0) then
    result := 1
  else if (i3 < 0) then
    result := -1
  else
    result := 0;
end;

function charForBool(bool : boolean) : String;
begin
  if bool then
    result := '1'
  else
    result := '0';
end;

procedure TSnomedCombiner.saveStore;
var
  s : String;
  f : System.Text;
begin
  Step('Saving Persistent Identity Store');
  assignfile(f, FStoreLocation);
  rewrite(f);
  writeln(f, nextRelationship);
  writeln(f, nextDescription);
  for s in FStore.Keys do
    writeln(f, inttostr(FStore[s].id)+'='+s);
  closefile(f);
end;

procedure TSnomedCombiner.saveToRF2;
var
  c,d,r : TTabWriter;
  concept : TSnomedCombinedConcept;
  desc : TSnomedCombinedDescription;
  rel : TSnomedCombinedRelationship;
  relg : TSnomedCombinedRelationshipGroup;
  st : TStringList;
  s : String;
  i : integer;
  rs : TSnomedCombinedReferenceSet;
  rm : TSnomedCombinedReferenceSetEntry;
  filename : String;
begin
  step('Sorting Concepts');
  st := TStringList.Create;
  try
    for s in FConcepts.Keys do
      st.AddObject(s, FConcepts[s]);
    st.CustomSort(intCompare);

    CreateDir(IncludeTrailingBackslash(destination)+'Terminology');
    CreateDir(IncludeTrailingBackslash(destination)+'RefSet');
    CreateDir(IncludeTrailingBackslash(destination)+'RefSet\Language');


    c := TTabWriter.Create(IncludeTrailingBackslash(destination)+'Terminology\concepts.txt');
    d := TTabWriter.Create(IncludeTrailingBackslash(destination)+'Terminology\descriptions.txt');
    r := TTabWriter.Create(IncludeTrailingBackslash(destination)+'Terminology\relationships.txt');
    try
      c.field('id');
      c.field('effectiveTime');
      c.field('active');
      c.field('moduleId');
      c.field('definitionStatusId');
      c.endRecord;

      d.field('id');
      d.field('effectiveTime');
      d.field('active');
      d.field('moduleId');
      d.field('conceptId');
      d.field('languageCode');
      d.field('typeId');
      d.field('term');
      d.field('caseSignificanceId');
      d.endRecord;

      r.field('id');
      r.field('effectiveTime');
      r.field('active');
      r.field('moduleId');
      r.field('sourceId');
      r.field('destinationId');
      r.field('relationshipGroup');
      r.field('typeId');
      r.field('characteristicTypeId');
      r.field('modifierId');
      r.endRecord;

      for i := 0 to st.Count - 1 do
      begin
        step('Saving RF2 format');
        concept := TSnomedCombinedConcept(st.Objects[i]);
        c.field(inttostr(concept.id));
        c.field(formatDateTime('yyyymmdd', concept.Fdate));
        if concept.FFlags and MASK_CONCEPT_STATUS > 0 then
          c.field('0')
        else
          c.field('1');
        c.field(inttostr(concept.FModule));
        if concept.FFlags and MASK_CONCEPT_PRIMITIVE > 0 then
          c.field(inttostr(RF2_MAGIC_PRIMITIVE))
        else
          c.field(inttostr(RF2_MAGIC_DEFINED));
        c.endRecord;
        for desc in concept.FDescriptions do
        begin
          d.field(inttostr(desc.FId));
          d.field(formatDateTime('yyyymmdd', desc.FDate));
          d.field(charForBool(desc.FActive));
          d.field(inttostr(desc.FModule));
          d.field(inttostr(concept.id));
          d.field(codeForLang(desc.FLang));
          d.field(inttostr(desc.FKind.FId));
          d.field(desc.FValue);
          d.field(inttostr(desc.FCaps.FId));
          d.endRecord;
        end;
        for relg in concept.FGroups do
        begin
          for rel in relg.FRelationships do
          begin
            r.field(inttostr(rel.FId));                      // id
            r.field(formatDateTime('yyyymmdd', rel.FDate));  // effectiveTime
            r.field(charForBool(rel.FActive));               // active
            r.field(inttostr(rel.FModule));                  // moduleId
            r.field(inttostr(concept.id));                   // sourceId
            r.field(inttostr(rel.FTarget.FID));              // destinationId
            r.field(inttostr(relg.FGroup));                  // relationshipGroup
            r.field(inttostr(rel.FRelType.FId));             // typeId
            r.field(inttostr(rel.FKind.FId));                // characteristicTypeId
            r.field(inttostr(rel.FModifier.FId));            // modifierId
            r.endRecord;
          end;
        end;
      end;
    finally
      c.free;
      d.Free;
      r.Free;
    end;
  finally
    st.Free;
  end;
  for rs in FRefSets.Values do
  begin
    if rs.DefaultLanguage then
      filename := IncludeTrailingBackslash(destination)+'RefSet\Language\'+rs.FFilename
    else
      filename := IncludeTrailingBackslash(destination)+'RefSet\'+rs.FFilename;
    ForceFolder(ExtractFilePath(filename));
    r := TTabWriter.Create(filename);
    try
      r.field('id');
      r.field('effectiveTime');
      r.field('active');
      r.field('moduleId');
      r.field('refsetId');
      r.field('referencedComponentId');
      for i := 0 to rs.FTypes.Count - 1 do
        r.field(rs.Ffields[i]);
      r.endRecord;
      for rm in rs.FMembers.Values do
      begin
        r.field(NewGuidId);
        if (rm.Date = 0) then
          r.field(formatDateTime('yyyymmdd', rs.FDefinition.FDate))
        else
          r.field(formatDateTime('yyyymmdd', rm.Date));
        r.field('1');
        if (rm.module = 0) then
          r.field(inttostr(rs.FDefinition.FModule))
        else
          r.field(inttostr(rm.module));
        r.field(inttostr(rs.FDefinition.id));
        r.field(inttostr(rm.FItem.FId));
        if (rm.FValues <> nil) then
          for i := 0 to rm.FValues.Count - 1 do
            r.field(rm.FValues[i]);
        r.endRecord;
      end;
    finally
      r.Free;
    end;
  end;
end;



procedure TSnomedCombiner.classify;
var
  concept : TSnomedCombinedConcept;
begin
  for concept in FConcepts.Values do
    classify(concept);
end;

procedure TSnomedCombiner.classify(concept : TSnomedCombinedConcept);
var
  child : TSnomedCombinedConcept;
  rel : TSnomedCombinedRelationship;
  relg : TSnomedCombinedRelationshipGroup;
begin
  step('Classifying');
  for relg in concept.FGroups do
  begin
    for rel in relg.FRelationships do
    begin
      if (rel.FActive) and // ignore things that are not active
         (rel.FRelType.FId <> IS_A_MAGIC) and // ignore is_a - don't classify them
         (rel.source = FInternational) then // anything from internationa is fully classified
      begin
        for child in concept.FChildren do
          forceRelationship(child, relg, rel);
      end;
    end;
  end;
end;

procedure TSnomedCombiner.forceRelationship(concept : TSnomedCombinedConcept; group : TSnomedCombinedRelationshipGroup; relationship : TSnomedCombinedRelationship);
var
  child : TSnomedCombinedConcept;
  g : TSnomedCombinedRelationshipGroup;
  r : TSnomedCombinedRelationship;
begin
  if (concept.source <> international) and (concept.source <> relationship.source) then
  begin
    // ok, the relationship was added by a different edition to the concept. So the concept might not have this
    // relationship on it. ensure that it exists
    g := concept.group(group.source, group.FGroup);
    if not exists(g.FRelationships, relationship) then
    begin
      r := TSnomedCombinedRelationship.Create(relationship.source);
      g.FRelationships.Add(r);
      r.id := getRelationshipId(concept.id, relationship.FRelType.id, relationship.FTarget.id, group.FGroup);
      r.FTarget := relationship.FTarget;
      r.FRelType := relationship.FRelType;
      r.FKind := relationship.FKind;
      r.FModifier := relationship.FModifier;
      r.FDate := relationship.FDate;
      r.FModule := FModuleId;
      r.FActive := relationship.FActive;
    end;
  end;
  for child in concept.FChildren do
    forceRelationship(child, group, relationship);
end;

function TSnomedCombiner.generateId(k, t: integer): int64;
var
  s : String;
begin
  s := inttostr(k)+inttostr(SCT_FHIR_NS)+'1'+inttostr(t);
  s := s + genCheckDigit(s);
  result := StrToInt64(s);
end;

function TSnomedCombiner.getDescriptionId(d: String): int64;
var
  k : TSnomedCombinedStoreEntry;
begin
  if FStore.TryGetValue(d, k) then
    result := k.id
  else
  begin
    inc(nextDescription);
    result := generateId(nextDescription, 1);
    FStore.Add(d, TSnomedCombinedStoreEntry.create(result));
  end;
end;

function TSnomedCombiner.getRelationshipId(s, rt, t: int64; grp : integer): int64;
var
  k : TSnomedCombinedStoreEntry;
  id : String;
begin
  id := inttostr(s)+':'+inttostr(rt)+':'+inttostr(t)+':'+inttostr(grp);
  if FStore.TryGetValue(id, k) then
    result := k.id
  else
  begin
    inc(nextRelationship);
    result := generateId(nextRelationship, 2);
    FStore.Add(id, TSnomedCombinedStoreEntry.create(result));
  end;
end;

procedure TSnomedCombiner.identify;
  function concept(id : int64; fsn, pn : String; parent : int64) : int64;
  var
    concept : TSnomedCombinedConcept;
    relationship : TSnomedCombinedRelationship;
    desc : TSnomedCombinedDescription;
  begin
    concept := TSnomedCombinedConcept.Create;
    FConcepts.Add(inttostr(id), concept);
    concept.FId := id;
    concept.FFlags := MASK_CONCEPT_PRIMITIVE or MASK_CONCEPT_STATUS;
    concept.FDate := trunc(now);
    concept.FModule := FModuleId;

    desc := TSnomedCombinedDescription.Create;
    concept.FDescriptions.add(desc);
    desc.id := getDescriptionId(fsn);
    desc.FDate := trunc(now);
    desc.FModule := FModuleId;
    desc.FKind := FConcepts[inttostr(RF2_MAGIC_FSN)];
    desc.FCaps := FConcepts[inttostr(900000000000448009)];
    desc.FActive := true;
    desc.Flang := 1;
    desc.FValue := FSN;

    desc := TSnomedCombinedDescription.Create;
    concept.FDescriptions.add(desc);
    desc.id := getDescriptionId(pn);
    desc.FDate := trunc(now);
    desc.FModule := FModuleId;
    desc.FKind := FConcepts[inttostr(900000000000013009)];
    desc.FCaps := FConcepts[inttostr(900000000000448009)];
    desc.FActive := true;
    desc.Flang := 1;
    desc.FValue := pn;

    relationship := TSnomedCombinedRelationship.create(nil);
    concept.group(nil, 0).FRelationships.add(relationship);
    relationship.id := getRelationshipId(concept.id, IS_A_MAGIC, parent, 0);
    relationship.FTarget := FConcepts[inttostr(parent)];
    relationship.FRelType := FConcepts[inttostr(IS_A_MAGIC)];
    relationship.FModule := FModuleId;
    relationship.FKind := FConcepts[inttostr(900000000000010007)];
    relationship.FModifier := FConcepts[inttostr(900000000000451002)];
    relationship.FDate := trunc(now);
    relationship.FActive := true;
  end;

  procedure relationship(source, reltype, target : int64);
  var
    concept : TSnomedCombinedConcept;
    relationship : TSnomedCombinedRelationship;
    desc : TSnomedCombinedDescription;
  begin
    concept := FConcepts[inttostr(source)];

    relationship := TSnomedCombinedRelationship.create(nil);
    concept.group(nil, 0).FRelationships.add(relationship);
    relationship.id := getRelationshipId(source, reltype, target, 0);
    relationship.FTarget := FConcepts[inttostr(target)];
    relationship.FRelType := FConcepts[inttostr(reltype)];
    relationship.FModule := FModuleId;
    relationship.FKind := FConcepts[inttostr(900000000000010007)];
    relationship.FModifier := FConcepts[inttostr(900000000000451002)];
    relationship.FDate := trunc(now);
    relationship.FActive := true;
  end;
var
  rid : int64;
  svc : TSnomedServices;
  st : TStringList;
  dep : TSnomedCombinedDependency;
  s : String;
begin
  concept(FModuleId, 'Custom Combined Module (core metadata concept)', 'Custom Combined Module', 900000000000443000);
  rid := generateId(2, 0);
  concept(rid, 'Combines (linkage concept)', 'Combines', 106237007);
  relationship(FModuleId, rid, StrToInt64(international.EditionId));
  for svc in others do
    relationship(FModuleId, rid, StrToInt64(svc.EditionId));
  st := TStringList.Create;
  try
    st.Sorted := true;
    st.Duplicates := dupIgnore;
    for dep in FDependencies.Values do
      st.Add(inttostr(dep.source));
    for s in st do
    begin
      dep := TSnomedCombinedDependency.Create;
      try
        dep.id := CreateGUID;
        dep.FDate := trunc(now);
        dep.source := FModuleId;
        dep.target := StrToInt64(s);
        dep.startDate := trunc(now);
        dep.endDate := trunc(now);
        FDependencies.Add(dep.key, dep.Link as TSnomedCombinedDependency);
      finally
        dep.Free;
      end;
    end;
  finally
    st.Free;
  end;
end;

function TSnomedCombiner.exists(list: TFslList<TSnomedCombinedRelationship>; relationship: TSnomedCombinedRelationship): boolean;
var
  t : TSnomedCombinedRelationship;
begin
  for t in list do
    if matches(relationship, t) then
      exit(true);
  exit(false);
end;

function TSnomedCombiner.matches(this, other: TSnomedCombinedRelationship): boolean;
begin
  result := ((this.FRelType = other.FRelType) or subsumes(other.FRelType, this.FRelType)) and
            ((this.FTarget = other.FTarget) or subsumes(other.FTarget, this.FTarget));
end;


function TSnomedCombiner.subsumes(this, other: TSnomedCombinedConcept): boolean;
var
  st, so: String;
  svc : TSnomedServices;
begin
  if this.FId = other.FId then
    exit(true);

  st := inttostr(this.FId);
  so := inttostr(other.FId);
  result := false;
  if international.Subsumes(st, so) then
    exit(true);
  for svc in others do
    if svc.Subsumes(st, so) then
      exit(true);
  result := false;
end;



{ TSnomedCombinedDescription }

function TSnomedCombinedDescription.link: TSnomedCombinedDescription;
begin
  result := TSnomedCombinedDescription(inherited Link);
end;

{ TSnomedCombinedReferenceSetEntry }

constructor TSnomedCombinedReferenceSetEntry.Create;
begin
  inherited;
end;

destructor TSnomedCombinedReferenceSetEntry.Destroy;
begin
  FValues.Free;
  inherited;
end;

{ TSnomedCombinedReferenceSet }

function TSnomedCombinedReferenceSet.abbrev: String;
var
  s : String;
begin
  result := '';
  for s in FTypes do
    result := result + s;
end;

constructor TSnomedCombinedReferenceSet.Create;
begin
  inherited;
  FTypes := TStringList.create;
  FFields := TStringList.create;
  FMembers := TFslMap<TSnomedCombinedReferenceSetEntry>.create;
end;

destructor TSnomedCombinedReferenceSet.Destroy;
begin
  FTypes.Free;
  FFields.Free;
  FMembers.Free;
  inherited;
end;

function TSnomedCombinedReferenceSet.getByItem(item: TSnomedCombinedItem; values : TStringList): TSnomedCombinedReferenceSetEntry;
var
  t : TSnomedCombinedReferenceSetEntry;
  i : integer;
  ok : boolean;
begin
  for t in FMembers.Values do
    if t.FItem.id = item.id then
    begin
      if (t.FValues = nil) and (values.Count = 0) then
        exit(t);
      if (t.FValues <> nil) and (t.FValues.Count > 0) and (values.Count = t.FValues.Count) then
      begin
        ok := true;
        for i := 0 to values.Count - 1 do
          if values[i] <> t.FValues[i] then
            ok := false;
        if (ok) then
          exit(t);
      end;
    end;
  exit(nil);
end;

{ TSnomedCombinedDependency }

function TSnomedCombinedDependency.key: String;
begin
  result := inttostr(source)+':'+inttostr(target);
end;

{ TSnomedCombinedStoreEntry }

constructor TSnomedCombinedStoreEntry.create(s: String);
begin
  inherited create;
  id := Strtoint64(s);
end;

constructor TSnomedCombinedStoreEntry.create(i: int64);
begin
  inherited create;
  id := i;
end;

end.
