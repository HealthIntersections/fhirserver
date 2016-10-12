unit SnomedCombiner;

interface

uses
  Windows, SysUtils, Classes,
  ThreadSupport,
  AdvGenerics, AdvObjects,
  SnomedServices;

Type
  TTabWriter = class (TAdvObject)
  private
    FStream : TFileStream;
    FDiv : boolean;
  public
    constructor Create(filename : String);
    destructor destroy; override;

    procedure field(s : String);
    procedure endRecord;
  end;

  TSnomedCombinedConcept = class;

  TSnomedCombinedDescription = class (TAdvObject)
  private
    FId : Int64;
    FDate : TSnomedDate;
    FModule : Int64;
    FKind : TSnomedCombinedConcept; // not linked
    FFlags : byte;
    FValue : String;
  public
  end;

  TSnomedCombinedRelationship = class (TAdvObject)
  private
    FId : Int64;
    FSource : TSnomedCombinedConcept;
    FTarget : TSnomedCombinedConcept;
    FRelType : TSnomedCombinedConcept;
    FModule : Int64;
    FKind : TSnomedCombinedConcept;
    FModifier : TSnomedCombinedConcept;
    FDate : TSnomedDate;
    FFlags : byte;
    FGroup : Integer;
    FAdded : boolean;
  public
    function link : TSnomedCombinedRelationship; overload;
    function copy : TSnomedCombinedRelationship;
  end;

  TSnomedCombinedRelationshipGroup = class (TAdvObject)
  private
    FRelationships : TAdvList<TSnomedCombinedRelationship>;
    FGroup : Integer;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    function link : TSnomedCombinedRelationshipGroup; overload;
    function copy : TSnomedCombinedRelationshipGroup;
  end;

  TSnomedCombinedConcept = class (TAdvObject)
  private
    FId : Int64;
    FDescriptions : TAdvList<TSnomedCombinedDescription>;
    FFlags : byte;
    FModule : Int64;
    FDate : TSnomedDate;
    international : boolean;
    FChildren : TAdvList<TSnomedCombinedConcept>;
    FRelationships : TAdvList<TSnomedCombinedRelationship>;
    FRelationshipGroups : TAdvList<TSnomedCombinedRelationshipGroup>;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    function link : TSnomedCombinedConcept; overload;
    property id : Int64 read FId write FId;
  end;
{
// for major.concept
//   add to combination
//   add descriptions
//   add relationships

// for each other
//   for each concept
//     is it in combo?
//     are it's descriptions in combo?
//     are its relationships in combo?

// classify

// write the combo to RF2 format

}
Type
  TSnomedCombiner = class (TAdvObject)
  private
    FModuleId: int64;
    FInternational: TSnomedServices;
    FOthers: TAdvList<TSnomedServices>;

    FConcepts : TAdvMap<TSnomedCombinedConcept>;
    FCallback: TInstallerCallback;
    FCurrent, FTotal, FPercent, FLast, FPathCount, FDescCount, FRelnCount : integer;
    FMessage : String;
    FSummary : TStringList;
    FIssues : TStringList;
    FDestination: String;

    procedure determineTotal;

    function LoadConcept(svc : TSnomedServices; i : integer; international : boolean) : TSnomedCombinedConcept;
    Procedure UpdateConcept(svc : TSnomedServices; i : integer; concept : TSnomedCombinedConcept);
    procedure loadConcepts;

    function LoadDescription(svc : TSnomedServices; i : integer) : boolean;
    procedure loadDescriptions;

    function processChildren(svc : TSnomedServices; i : integer) : integer;
    procedure loadChildren;

    function LoadRelationship(svc : TSnomedServices; i : integer; international : boolean) : boolean;
    procedure loadRelationships;

    procedure determineTotal2;

    procedure checkForCircles; overload;
    procedure checkForCircles(svc : TSnomedServices); overload;
    procedure checkForCircles(root : UInt64); overload;
    procedure checkForCircles(focus : TSnomedCombinedConcept; parents : Array of TSnomedCombinedConcept); overload;

    procedure buildGroups(concept : TSnomedCombinedConcept); overload;
    procedure buildGroups; overload;
    function subsumes(this, other : TSnomedCombinedConcept) : boolean;
    function matches(this, other : TSnomedCombinedRelationship) : boolean; overload;
    procedure merge(this, other : TSnomedCombinedRelationship); overload;
    function matches(this, other : TSnomedCombinedRelationshipGroup) : boolean; overload;
    procedure merge(this, other : TSnomedCombinedRelationshipGroup); overload;
    procedure eliminateDuplicates(concept : TSnomedCombinedConcept); overload;
    procedure eliminateDuplicates; overload;

    procedure step(desc : String);
    procedure classify; overload;
    procedure classify(svc : TSnomedServices); overload;
    procedure classify(concept : TSnomedCombinedConcept); overload;
    function hasAdded(relg : TSnomedCombinedRelationshipGroup) : boolean;
    procedure defineOnChild(concept : TSnomedCombinedConcept; rel : TSnomedCombinedRelationship); overload;
    procedure defineOnChild(concept : TSnomedCombinedConcept; relg : TSnomedCombinedRelationshipGroup); overload;


    procedure saveToRF2;
    procedure SetInternational(const Value: TSnomedServices);
    procedure recordIssue(s : String);
    procedure recordSummary(s : String);
  public
    Constructor Create; override;
    Destructor Destroy; override;

    procedure Execute;

    property international : TSnomedServices read FInternational write SetInternational;
    property others : TAdvList<TSnomedServices> read FOthers;
    property moduleId : int64 read FModuleId write FModuleId;
    property callback : TInstallerCallback read FCallback write FCallback;

    property issues : TStringList read FIssues;
    property summary : TStringList read FSummary;

    property destination : String read FDestination write FDestination;
  end;

implementation

{ TSnomedCombiner }

procedure TSnomedCombiner.checkForCircles;
var
  svc : TSnomedServices;
begin
  callback(0, 'Searching for Circular definitions');
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
  concept, child : TSnomedCombinedConcept;
begin
  concept := FConcepts[inttostr(root)];
  for child in concept.FChildren do
    checkForCircles(child, [concept]);
end;

procedure TSnomedCombiner.buildGroups;
var
  cc : TSnomedCombinedConcept;
begin
  for cc in FConcepts.Values do
  begin
    step('Processing Relationship groups');
    buildGroups(cc);
  end;
end;

procedure TSnomedCombiner.checkForCircles(focus : TSnomedCombinedConcept; parents : Array of TSnomedCombinedConcept);
var
  c : TSnomedCombinedConcept;
  s : String;
  f : boolean;
  np : Array of TSnomedCombinedConcept;
  i : integer;
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
    raise Exception.Create('Circular Dependency found: SCT concept '+inttostr(focus.FId)+' has itself as a parent ('+s+')');
  end;
  SetLength(np, length(parents)+1);
  for i := 0 to length(parents) - 1 do
    np[i] := parents[i];
  np[length(parents)] := focus;
  for c in focus.FChildren do
    checkForCircles(c, np);
end;
procedure TSnomedCombiner.classify(svc: TSnomedServices);
var
  i : int64;
begin
  for i in svc.ActiveRoots do
    classify(FConcepts[inttostr(i)]);
  for i in svc.InactiveRoots do
    classify(FConcepts[inttostr(i)]);
end;

//
//
//end;
//     Property ActiveRoots : UInt64Array read FActiveRoots write FActiveRoots;
//    Property InactiveRoots : UInt64Array read FInActiveRoots write FInActiveRoots;
//
//var
//  c, cc : TSnomedCombinedConcept;
//begin
//
//  for c in FConcepts.Values do
//    for cc in c.FChildren do
//    begin
//      step('Checking for Circles');
//      if (cc = c) then
//      checkHasNoChild(cc, c);
//    end;
//end;

constructor TSnomedCombiner.Create;
begin
  inherited;
  FOthers := TAdvList<TSnomedServices>.create;
  FConcepts := TAdvMap<TSnomedCombinedConcept>.create;
  FSummary := TStringList.create;
  FIssues := TStringList.create;
end;

procedure TSnomedCombiner.defineOnChild(concept: TSnomedCombinedConcept; rel: TSnomedCombinedRelationship);
var
  ok : boolean;
  t : TSnomedCombinedRelationship;
begin
  for t in concept.FRelationships do
    if (matches(t, rel)) then
    begin
      merge(t, rel);
      ok := true;
    end;
  if not ok then
    concept.FRelationships.Add(rel.copy);
end;

procedure TSnomedCombiner.defineOnChild(concept: TSnomedCombinedConcept; relg: TSnomedCombinedRelationshipGroup);
var
  ok : boolean;
  t : TSnomedCombinedRelationshipGroup;
begin
  if concept.FRelationshipGroups = nil then
    concept.FRelationshipGroups := TAdvList<TSnomedCombinedRelationshipGroup>.create;

  for t in concept.FRelationshipGroups do
    if (matches(t, relg)) then
    begin
      merge(t, relg);
      ok := true;
    end;
  if not ok then
    concept.FRelationshipGroups.Add(relg.copy);
end;

destructor TSnomedCombiner.Destroy;
begin
  FSummary.free;
  FIssues.free;
  FConcepts.Free;
  FInternational.free;
  FOthers.free;
  inherited;
end;

procedure TSnomedCombiner.determineTotal;
var
  svc : TSnomedServices;
begin
  FTotal := (FInternational.Concept.Count * 2) + FInternational.Desc.Count + FInternational.Rel.Count;
  for svc in others do
    FTotal := FTotal + (svc.Concept.Count * 2) + svc.Desc.Count;
  FCurrent := 0;
  FPercent := 0;
  FLast := GetTickCount;
end;

procedure TSnomedCombiner.determineTotal2;
begin
  FTotal :=
     FConcepts.Count + // build groups
     FConcepts.Count + // eliminate duplicates
     FPathCount + // classify
     FConcepts.Count; // rf2

  FCurrent := 0;
  FPercent := 0;
  FLast := GetTickCount;
end;

procedure TSnomedCombiner.eliminateDuplicates;
var
  cc : TSnomedCombinedConcept;
begin
  for cc in FConcepts.Values do
  begin
    step('Eliminating Duplicate Relationships');
    eliminateDuplicates(cc);
  end;
end;

procedure TSnomedCombiner.classify;
var
  svc : TSnomedServices;
begin
  exit;
  callback(0, 'Searching for Circular definitions');
  classify(FInternational);
  for svc in others do
    classify(svc);
end;

procedure TSnomedCombiner.Execute;
begin
  // 1. merging
  determineTotal;
  loadConcepts;
  loadDescriptions;
//  loadChildren;
//  loadRelationships;

  // 2. classifying
//  checkForCircles;
//  determineTotal2;
//  buildGroups;
//  eliminateDuplicates;
//  classify;

  // 3. save to RF2
  saveToRF2;
end;

function TSnomedCombiner.hasAdded(relg: TSnomedCombinedRelationshipGroup): boolean;
var
  t : TSnomedCombinedRelationship;
begin
  for t in relg.FRelationships do
    if (t.FAdded) then
      exit(true);
  result := false;
end;

function TSnomedCombiner.processChildren(svc : TSnomedServices; i : integer) : integer;
var
  Identity : UInt64;
  Flags : Byte;
  Group : integer;
  date : TSnomedDate;
  ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsetIndex : Cardinal;
  inbounds : TCardinalArray;
  c : Cardinal;
  cc, ct, tt : TSnomedCombinedConcept;
  iWork, iWork2, iWork3, iWork4, iWork5, iWork6 : Cardinal;
  found : boolean;
begin
  result := 0;
  svc.Concept.GetConcept(i, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsetIndex);
  if not FConcepts.TryGetValue(inttostr(Identity), cc) then
    raise Exception.create('no find concept '+inttostr(Identity));
  inbounds := svc.Refs.GetReferences(InboundIndex);
  for i := 0 to High(Inbounds) Do
  begin
    svc.Rel.GetRelationship(Inbounds[i], Identity, iWork, iWork2, iWork3, iWork4, iWork5, iWork6, date, Flags, Group);
    if (iWork3 = svc.Is_a_Index) and not (flags and MASK_REL_CHARACTERISTIC = VAL_REL_Historical) Then
    begin
      ct := FConcepts[svc.GetConceptId(iWork)];
      found := false;
      for tt in cc.FChildren do
        if tt = ct then
          found := true;
      if not found then
      begin
        cc.FChildren.Add(ct.Link);
        inc(result);
      end;
    end;
  end;
end;

procedure TSnomedCombiner.loadChildren;
var
  i, t : integer;
  concept : TSnomedCombinedConcept;
  svc : TSnomedServices;
  id : Int64;
begin
  t := 0;
  for i := 0 to FInternational.Concept.Count - 1 do
  begin
    step('Process Children for '+FInternational.EditionName);
    inc(t, processChildren(FInternational, i * CONCEPT_SIZE));
  end;
  for svc in FOthers do
  begin
    for i := 0 to svc.Concept.Count - 1 do
    begin
      step('Process Children for '+svc.EditionName);
      inc(t, processChildren(svc, i * CONCEPT_SIZE));
    end;
  end;
  recordSummary('Total Child Relationships: '+inttostr(t));
  FTotal := FTotal -1000000 + t;
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
//  if (concept.FModule <> svc.Concept.getConceptId(svc.Concept.GetModuleId(i))) then
//  begin
//    recordIssue('Module '+svc.EditionName+' changes module of concept '+inttostr(concept.FId)+' from '+inttostr(concept.FModule) +' to '+ inttostr(svc.Concept.getConceptId(svc.Concept.GetModuleId(i))));
//    concept.FModule := svc.Concept.getConceptId(svc.Concept.GetModuleId(i));
//  end;
end;

function TSnomedCombiner.LoadConcept(svc: TSnomedServices; i: integer; international : boolean): TSnomedCombinedConcept;
var
  Identity : UInt64;
  Flags : Byte;
  effectiveTime : TSnomedDate;
  Parents, Descriptions, Inbounds, outbounds, refsets : Cardinal;
begin
  svc.Concept.GetConcept(i, Identity, Flags, effectiveTime, Parents, Descriptions, Inbounds, outbounds, refsets);
  result := TSnomedCombinedConcept.Create;
  result.FId := Identity;
  result.FFlags := Flags;
  result.FDate := effectiveTime;
  result.FModule := svc.Concept.getConceptId(svc.Concept.GetModuleId(i));
  result.international := international;
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
    concept := LoadConcept(FInternational, i * CONCEPT_SIZE, false);
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
        concept := LoadConcept(svc, i * CONCEPT_SIZE, true);
        FConcepts.Add(inttostr(concept.id), concept);
      end;
    end;
  end;
  recordSummary('Total Concepts: '+inttostr(FConcepts.Count));
end;

function TSnomedCombiner.LoadDescription(svc: TSnomedServices; i: integer): boolean;
var
  iDesc : Cardinal;
  id, cid : UInt64;
  date : TSnomedDate;
  concept, module, kind, refsets, valueses : Cardinal;
  iFlags : Byte;
  c : TSnomedCombinedConcept;
  d, t : TSnomedCombinedDescription;
begin
  svc.Desc.getDescription(i, iDesc, id, date, concept, module, kind, refsets, valueses, iFlags);
  cid := svc.Concept.getConceptId(concept);
  if not FConcepts.TryGetValue(inttostr(cid), c) then
    raise Exception.create('no find concept '+inttostr(cid))
  else
  begin
    d := nil;
    for t in c.Fdescriptions do
      if t.Fid = id then
        d := t;
    result := d = nil;
    if d <> nil then
    begin
      if d.FDate < date then
        d.FDate := date;
//      if d.FModule <> svc.Concept.getConceptId(module) then
//        recordIssue('Module '+svc.EditionName+' changes module of description '+inttostr(d.FId)+' from '+inttostr(d.FModule) +' to '+ inttostr(svc.Concept.getConceptId(module)));
      d.FFlags := iFlags or d.FFlags;
      if not SameText(d.FValue, svc.Strings.GetEntry(iDesc)) then
        recordIssue('Module '+svc.EditionName+' changes value of description '+inttostr(d.FId)+' from "'+d.FValue+'" to "'+ svc.Strings.GetEntry(iDesc)+'"');
    end
    else
    begin
      d := TSnomedCombinedDescription.create;
      c.FDescriptions.Add(d);
      d.FId := id;
      d.FDate := date;
      d.FModule := svc.Concept.getConceptId(module);
      d.FKind := FConcepts[inttostr(svc.Concept.getConceptId(kind))];
      d.FFlags := iFlags;
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

function TSnomedCombiner.LoadRelationship(svc: TSnomedServices; i: integer; international : boolean): boolean;
var
//  iDesc : Cardinal;
  cid : UInt64;
//  date : TSnomedDate;
//  concept, module, kind, refsets, valueses : Cardinal;
//  iFlags : Byte;
  c : TSnomedCombinedConcept;
  r, t : TSnomedCombinedRelationship;
  identity : UInt64;
  Source, Target, RelType, module, kind, modifier : Cardinal;
  date : TSnomedDate;
  Flags : Byte;
  Group : Integer;
begin
  svc.Rel.GetRelationship(i, identity, Source, Target, RelType, module, kind, modifier, date, Flags, group);
  if (RelType = svc.Is_a_Index) and not (flags and MASK_REL_CHARACTERISTIC = VAL_REL_Historical) then
    exit(false); // we ignore is_a; they've already been loaded

  cid := svc.Concept.getConceptId(source);
  if not FConcepts.TryGetValue(inttostr(cid), c) then
    raise Exception.create('no find concept '+inttostr(cid))
  else
  begin
    r := nil;
    for t in c.FRelationships do
      if t.Fid = identity then
        r := t;
    result := r = nil;
    if r <> nil then
    begin
      if r.FDate < date then
        r.FDate := date;
      if r.FFlags <> Flags then
        r.FFlags := Flags or r.FFlags;
      if r.FModule <> svc.Concept.getConceptId(module) then
        recordIssue('Module '+svc.EditionName+' changes module of description '+inttostr(r.FId)+' from '+inttostr(r.FModule) +' to '+ inttostr(svc.Concept.getConceptId(module)));
      if r.FGroup <> group then
        recordIssue('Module '+svc.EditionName+' changes Group of relationship '+inttostr(r.FId)+' from '+inttostr(r.FGroup) +' to '+ inttostr(group));
      if r.FSource <> FConcepts[inttostr(svc.Concept.getConceptId(source))] then
        recordIssue('Module '+svc.EditionName+' changes Source of relationship '+inttostr(r.FId)+' from '+inttostr(r.FSource.id) +' to '+ inttostr(FConcepts[inttostr(svc.Concept.getConceptId(source))].id));
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
      r := TSnomedCombinedRelationship.create;
      c.FRelationships.Add(r);
      r.FAdded := international and not c.international;
      r.FId := identity;
      r.FDate := date;
      r.FSource := FConcepts[inttostr(svc.Concept.getConceptId(source))];
      r.FTarget := FConcepts[inttostr(svc.Concept.getConceptId(target))];
      r.FRelType := FConcepts[inttostr(svc.Concept.getConceptId(RelType))];
      r.Fmodule := svc.Concept.getConceptId(module);
      if (kind <> 0) then
        r.Fkind := FConcepts[inttostr(svc.Concept.getConceptId(kind))];
      if (kind <> 0) then
        r.Fmodifier := FConcepts[inttostr(svc.Concept.getConceptId(modifier))];
      r.FDate := date;
      r.FFlags := Flags;
      r.FGroup := group;
    end;
  end;

end;

procedure TSnomedCombiner.loadRelationships;
var
  i, t : integer;
  svc : TSnomedServices;
begin
  t := 0;
  recordSummary(FInternational.EditionName+' relationships: '+inttostr(FInternational.Rel.Count));
  for i := 0 to FInternational.Desc.Count - 1 do
  begin
    step('Process Relationships for '+FInternational.EditionName);
    if LoadRelationship(FInternational, i*RELATIONSHIP_SIZE, true) then
      inc(t);
  end;

  for svc in FOthers do
  begin
    recordSummary(svc.EditionName+' relationships: '+inttostr(svc.Rel.Count));
    for i := 0 to svc.Desc.Count - 1 do
    begin
      step('Process Relationships for '+svc.EditionName);
      if LoadRelationship(svc, i*RELATIONSHIP_SIZE, false) then
        inc(t);
    end;
  end;
  recordSummary('Total Relationships: '+inttostr(t));
  FRelnCount := t;
end;

function TSnomedCombiner.matches(this, other: TSnomedCombinedRelationship): boolean;
begin
  result := (subsumes(this.FRelType, other.FRelType) or subsumes(other.FRelType, this.FRelType)) and
            (subsumes(this.FTarget, other.FTarget) or subsumes(other.FTarget, this.FTarget));
end;

function TSnomedCombiner.matches(this, other: TSnomedCombinedRelationshipGroup): boolean;
var
  i , j : integer;
begin
  for i := 0 to this.FRelationships.Count - 1 do
    for j := i + 1 to this.FRelationships.Count - 1 do
      if matches(this.FRelationships[i], this.FRelationships[j]) then
        exit(true);
  result := false;
end;

procedure TSnomedCombiner.merge(this, other: TSnomedCombinedRelationship);
begin
  if this.FRelType <> other.FRelType then
  begin
    if subsumes(this.FRelType, other.FRelType) then
      this.FRelType := other.FRelType;
  end;
  if (this.FTarget <> other.FTarget) then
  begin
    if subsumes(this.FTarget, other.FTarget) then
      this.FTarget := other.FTarget;
  end;
  // ignore the other stuff
end;

procedure TSnomedCombiner.merge(this, other: TSnomedCombinedRelationshipGroup);
var
  s, t : TSnomedCombinedRelationship;
  b : boolean;
begin
  for s in other.FRelationships do
  begin
    b := false;
    for t in this.FRelationships do
      if matches(t, s) then
      begin
        merge(t, s);
        b := true;
      end;
    if not b then
      this.FRelationships.Add(s.link);
  end;
end;

procedure TSnomedCombiner.recordIssue(s: String);
begin
  FIssues.Add(s);
end;

procedure TSnomedCombiner.recordSummary(s: String);
begin
  FSummary.Add(s);
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

procedure TSnomedCombiner.saveToRF2;
var
  c,d,r : TTabWriter;
  concept, child : TSnomedCombinedConcept;
  desc : TSnomedCombinedDescription;
  st : TStringList;
  s : String;
  i : integer;
begin
  st := TStringList.Create;
  try
    for s in FConcepts.Keys do
      st.AddObject(s, FConcepts[s]);
    st.CustomSort(intCompare);

    c := TTabWriter.Create(IncludeTrailingBackslash(destination)+'concepts.txt');
    d := TTabWriter.Create(IncludeTrailingBackslash(destination)+'descriptions.txt');
    r := TTabWriter.Create(IncludeTrailingBackslash(destination)+'relationships.txt');
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
        c.field(inttostr(concept.Fdate));
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
          d.field(inttostr(desc.FDate));
          d.field(inttostr(desc.FFlags and MASK_DESC_STATUS));
          d.field(inttostr(desc.FModule));
          d.field(inttostr(concept.id));
          d.field('en');
          if desc.FKind = nil then
            d.field('0')
          else
            d.field(inttostr(desc.FKind.FId));
          d.field(desc.FValue);
          case desc.FFlags and MASK_DESC_CAPS_MASK of
            MASK_DESC_CAPS_NONE : d.field('900000000000448009');
            MASK_DESC_CAPS_FIRST : d.field('900000000000020002');
            MASK_DESC_CAPS_ALL : d.field('900000000000017005');
          end;
          d.endRecord;
        end;
        for child in concept.FChildren do
        begin
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
end;

procedure TSnomedCombiner.SetInternational(const Value: TSnomedServices);
begin
  FInternational.free;
  FInternational := Value;
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

procedure TSnomedCombiner.buildGroups(concept : TSnomedCombinedConcept);
var
  rel : TSnomedCombinedRelationship;
  grp, t : TSnomedCombinedRelationshipGroup;
  l : TAdvList<TSnomedCombinedRelationshipGroup>;
  s : String;
  f, b : boolean;
begin
  for rel in concept.FRelationships do
    if rel.FGroup <> 0 then
    begin
      if concept.FRelationshipGroups = nil then
        concept.FRelationshipGroups := TAdvList<TSnomedCombinedRelationshipGroup>.create;
      grp := nil;
      for t in concept.FRelationshipGroups do
        if (t.FGroup = rel.FGroup) then
          grp := t;
      if grp = nil then
      begin
        grp := TSnomedCombinedRelationshipGroup.Create;
        concept.FRelationshipGroups.Add(grp);
        grp.FGroup := rel.FGroup;
      end;
      grp.FRelationships.add(rel.Link);
    end;
  if concept.FRelationshipGroups <> nil then
  begin
    l := TAdvList<TSnomedCombinedRelationshipGroup>.create;
    try
      for t in concept.FRelationshipGroups do
        if t.FRelationships.Count = 1 then
        begin
          l.Add(t.Link);
        end
        else
          for rel in t.FRelationships do
            concept.FRelationships.Remove(rel);
      for t in l do
        concept.FRelationshipGroups.Remove(t);
    finally
      l.free;
    end;
    for t in concept.FRelationshipGroups do
    begin
      f := true;
      for rel in t.FRelationships do
      begin
        if f then
        begin
          f := false;
          b := rel.FAdded
        end
        else if rel.FAdded <> b then
            recordIssue('concept '+inttostr(concept.FId)+' has Mixed group "'+s+'"');
      end;
    end;
  end;
end;

procedure TSnomedCombiner.eliminateDuplicates(concept : TSnomedCombinedConcept);
var
//  list, tl : TAdvList<TSnomedCombinedRelationship>;
//  rel, t : TSnomedCombinedRelationship;
//  s : String;
//  sl : TStringList;
  i, j : integer;
  ri, rj : TSnomedCombinedRelationship;
  gi, gj : TSnomedCombinedRelationshipGroup;
begin
  i := 0;
  while i < concept.FRelationships.Count - 1 do
  begin
    j := i + 1;
    while j < concept.FRelationships.Count - 1  do
    begin
      ri := concept.FRelationships[i];
      rj := concept.FRelationships[j];
      if (matches(ri, rj)) then
      begin
        merge(ri, rj);
        concept.FRelationships.Delete(j)
      end
      else
        inc(j);
    end;
    inc(i);
  end;
  if concept.FRelationshipGroups <> nil then
  begin
    i := 0;
    while i < concept.FRelationshipGroups.Count - 1 do
    begin
      j := i + 1;
      while j < concept.FRelationshipGroups.Count - 1  do
      begin
        gi := concept.FRelationshipGroups[i];
        gj := concept.FRelationshipGroups[j];
        if matches(gi, gj) then
        begin
          merge(gi, gj);
          concept.FRelationshipGroups.Delete(j);
        end
        else
          inc(j);
      end;
      inc(i);
    end;
  end;
end;

procedure TSnomedCombiner.classify(concept : TSnomedCombinedConcept);
var
  child : TSnomedCombinedConcept;
  rel : TSnomedCombinedRelationship;
  relg : TSnomedCombinedRelationshipGroup;
begin
  step('Classifing');
  // actually classify
  for rel in concept.FRelationships do
    if rel.FAdded then
      for child in concept.FChildren do
        defineOnChild(child, rel);

  if concept.FRelationshipGroups <> nil then
    for relg in concept.FRelationshipGroups do
      if hasAdded(relg) then
        for child in concept.FChildren do
          defineOnChild(child, relg);

  for child in concept.FChildren do
    classify(child);
end;

{ TSnomedCombinedConcept }

constructor TSnomedCombinedConcept.Create;
begin
  inherited;
  FDescriptions := TAdvList<TSnomedCombinedDescription>.create;
  FChildren := TAdvList<TSnomedCombinedConcept>.create;
  FRelationships := TAdvList<TSnomedCombinedRelationship>.create;
end;

destructor TSnomedCombinedConcept.Destroy;
begin
  FRelationshipGroups.Free;
  FDescriptions.Free;
  FChildren.Free;
  FRelationships.Free;
  inherited;
end;

function TSnomedCombinedConcept.link: TSnomedCombinedConcept;
begin
  result := TSnomedCombinedConcept(inherited Link);
end;

{ TSnomedCombinedRelationship }

function TSnomedCombinedRelationship.copy: TSnomedCombinedRelationship;
begin
  result := TSnomedCombinedRelationship.Create;
  result.FId := FId;
  result.FSource := FSource.link;
  result.FTarget := FTarget.link;
  result.FRelType := FRelType.link;
  result.FModule := FModule;
  result.FKind := FKind.link;
  result.FModifier := FModifier.link;
  result.FDate := FDate;
  result.FFlags := FFlags;
  result.FGroup := FGroup;
  result.FAdded := FAdded;
end;

function TSnomedCombinedRelationship.link: TSnomedCombinedRelationship;
begin
  result := TSnomedCombinedRelationship(inherited Link);
end;

{ TSnomedCombinedRelationshipGroup }

function TSnomedCombinedRelationshipGroup.copy: TSnomedCombinedRelationshipGroup;
var
  t : TSnomedCombinedRelationship;
begin
  result := TSnomedCombinedRelationshipGroup.create;
  for t in FRelationships do
    result.FRelationships.add(t.copy);
  result.FGroup := FGroup;
end;

constructor TSnomedCombinedRelationshipGroup.Create;
begin
  inherited;
  FRelationships := TAdvList<TSnomedCombinedRelationship>.create;
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

end.
