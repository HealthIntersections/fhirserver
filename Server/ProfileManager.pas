unit ProfileManager;

interface

uses
  SysUtils,
  kCritSct,
  StringSupport,
  AdvObjects, AdvStringMatches, AdvStringObjectMatches,
  FHIRComponents, FHIRResources, FHIRAtomFeed, FHIRUtilities, FHIRConstants;


Type
  TProfileManager = class (TAdvObject)
  private
    lock : TCriticalSection;
    FProfilesByIdentifier : TAdvStringObjectMatch; // all current profiles by identifier (ValueSet.identifier)
    FProfilesByURL : TAdvStringObjectMatch; // all current profiles by their URL
    FProfilesByKey : TAdvStringObjectMatch;
    function GetProfileByUrl(url: String): TFhirProfile;
    function GetProfileByType(aType: TFhirResourceType): TFhirProfile; // all profiles by the key they are known from (mainly to support drop)

  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TProfileManager; overload;

    procedure SeeProfile(url : String; key : Integer; profile : TFHIRProfile);
    procedure DropProfile(key : Integer; url : String; aType : TFhirResourceType);
    procedure loadFromFeed(feed : TFHIRAtomFeed);

    function getExtensionDefn(source : TFhirProfile; url : String; var profile : TFhirProfile; var extension : TFhirProfileExtensionDefn) : boolean;
    function getStructure(source : TFhirProfile; url : String; var profile : TFhirProfile; var Structure : TFhirProfileStructure) : boolean;
    function getLinks(non_structures, non_resources : boolean) : TAdvStringMatch;

    property ProfileByURL[url : String] : TFhirProfile read GetProfileByUrl; default;
    property ProfileByType[aType : TFhirResourceType] : TFhirProfile read GetProfileByType;
  end;

  {$IFNDEF FHIR-DSTU}
  {
  This encapsulates a reference to an element definition within a structure.
  The path may be replace
  }
  TProfileDefinition = class (TAdvObject)
  private
    FProfiles : TProfileManager;
    FProfile : TFhirProfile;
    FStructure : TFhirProfileStructure;
    FElement : TFhirProfileStructureSnapshotElement;
    statedPath : String;
    FType : TFhirProfileStructureSnapshotElementDefinitionType;

    function GetTypes: TFhirProfileStructureSnapshotElementDefinitionTypeList;
    function GetPath: String;
    function GetName: String;
    Property Types : TFhirProfileStructureSnapshotElementDefinitionTypeList read GetTypes;
  public
    Constructor Create(profiles : TProfileManager; profile : TFhirProfile; structure : TFhirProfileStructure); overload;
    Destructor Destroy; override;

    procedure setType(t : TFhirProfileStructureSnapshotElementDefinitionType);
    function statedType : TFhirProfileStructureSnapshotElementDefinitionType;
    function hasTypeChoice : boolean;
    Property path : String read GetPath;
    Property name : String read GetName;
    function getById(id : String) : TProfileDefinition;
  end;
  {$ENDIF}

implementation

{ TProfileManager }

constructor TProfileManager.Create;
begin
  inherited;
  lock := TCriticalSection.Create('profiles');
  FProfilesByIdentifier := TAdvStringObjectMatch.create;
  FProfilesByIdentifier.Forced := true;
  FProfilesByURL := TAdvStringObjectMatch.create;
  FProfilesByURL.Forced := true;
  FProfilesByKey := TAdvStringObjectMatch.create;
  FProfilesByKey.Forced := true;
end;

destructor TProfileManager.Destroy;
begin
  FProfilesByIdentifier.free;
  FProfilesByURL.free;
  FProfilesByKey.free;
  lock.Free;
  inherited;
end;

function TProfileManager.getExtensionDefn(source: TFhirProfile; url: String; var profile: TFhirProfile; var extension : TFhirProfileExtensionDefn): boolean;
var
  id, code : String;
  i : integer;
begin
  result := false;
  if url.StartsWith('#') then
  begin
    profile := source;
    code := url.Substring(1);
  end
  else
  begin
    StringSplit(url, '#', id, code);
    lock.Lock;
    try
      profile := FProfilesByIdentifier.Matches[id] as TFhirProfile;
    finally
      lock.Unlock;
    end;
  end;

  if (profile <> nil) then
  begin
    extension := nil;
    for i := 0 to profile.extensionDefnList.Count - 1 do
      if profile.extensionDefnList[i].codeST = url.Substring(1) then
        extension := profile.extensionDefnList[i];
    result := extension <> nil;
  end;
end;

function TProfileManager.getLinks(non_structures, non_resources : boolean): TAdvStringMatch;
var
  i, j : integer;
  p : TFHIRProfile;
  bs, br : boolean;
  url : String;
begin
  lock.Lock('getLinks');
  try
    result := TAdvStringMatch.Create;
    try
      for i := 0 to FProfilesByURL.Count - 1 do
      begin
        url := FProfilesByURL.KeyByIndex[i];
        if (not url.startsWith('http:')) then
        begin
          p := TFHIRProfile(FProfilesByURL.ValueByIndex[i]);
          bs := non_structures;
          br := non_resources;
          for j := 0 to p.structureList.Count - 1 do
          begin
            bs := bs or p.structureList[j].publishST;
            br := br or StringArrayExistsSensitive(CODES_TFhirResourceType, p.structureList[j].type_ST);
          end;
          if bs and br then
            result.Add(url, p.nameST);
        end;
      end;
      result.Link;
    finally
      result.Free;
    end;
  finally
    lock.Unlock;
  end;
end;

function TProfileManager.GetProfileByType(aType: TFhirResourceType): TFhirProfile;
begin
  result := GetProfileByUrl('http://hl7.org/fhir/Profile/'+CODES_TFHIRResourceType[aType]);
end;

function TProfileManager.GetProfileByUrl(url: String): TFhirProfile;
begin
  result := TFhirProfile(FProfilesByURL.GetValueByKey(url));
end;

function TProfileManager.getStructure(source: TFhirProfile; url: String; var profile: TFhirProfile; var Structure: TFhirProfileStructure): boolean;
var
  id, code : String;
  i : integer;
begin
  result := false;
  if url.StartsWith('#') then
  begin
    profile := source;
    code := url.Substring(1);
  end
  else
  begin
    StringSplit(url, '#', id, code);
    lock.Lock;
    try
      profile := FProfilesByIdentifier.Matches[id] as TFhirProfile;
    finally
      lock.Unlock;
    end;
  end;

  if (profile <> nil) then
  begin
    structure := nil;
    for i := 0 to profile.structureList.Count - 1 do
      if profile.structureList[i].nameST = code then
        structure := profile.structureList[i];
    result := structure <> nil;
  end;
end;

function TProfileManager.Link: TProfileManager;
begin
  result := TProfileManager(inherited Link);
end;

procedure TProfileManager.loadFromFeed(feed: TFHIRAtomFeed);
var
  i : integer;
begin
  for i := 0 to feed.entries.Count - 1 do
    if feed.entries[i].resource is TFHIRProfile then
      SeeProfile(feed.entries[i].id, i, feed.entries[i].resource as TFhirProfile);
end;

procedure TProfileManager.SeeProfile(url: String; key: Integer; profile: TFHIRProfile);
begin
  lock.Lock('SeeProfile');
  try
    FProfilesByIdentifier.Matches[profile.{$IFDEF FHIR-DST}identifierST {$ELSE}urlST{$ENDIF}] := profile.Link;
    FProfilesByURL.Matches[url] := profile.Link;
    FProfilesByKey.Matches[inttostr(key)] := profile.Link;
  finally
    lock.Unlock;
  end;
end;


procedure TProfileManager.DropProfile(key: Integer; url: String; aType: TFhirResourceType);
var
  p, p1 : TFhirProfile;
begin
  lock.Lock('DropProfile');
  try
    p := TFhirProfile(FProfilesByKey.GetValueByKey(inttostr(key)));
    if p <> nil then
    begin
      FProfilesByURL.DeleteByKey(url);
      FProfilesByIdentifier.DeleteByKey(p.{$IFDEF FHIR-DST}identifierST {$ELSE}urlST{$ENDIF});
      FProfilesByKey.DeleteByKey(inttostr(key));
    end;
  finally
    lock.Unlock;
  end;
end;


{$IFNDEF FHIR-DSTU}

{ TProfileDefinition }

constructor TProfileDefinition.Create(profiles: TProfileManager; profile: TFhirProfile; structure: TFhirProfileStructure);
begin
  Create;
  FProfiles := profiles;
  FProfile := profile;
  FStructure := structure;
  FElement := structure.snapshot.elementList[0].link;
end;

destructor TProfileDefinition.Destroy;
begin
  FType.free;
  FProfiles.Free;
  FProfile.Free;
  FStructure.Free;
  FElement.Free;
  inherited;
end;

function TProfileDefinition.getById(id: String): TProfileDefinition;
var
  path : String;
  i : integer;
  profile : TFhirProfile;
  structure : TFhirProfileStructure;
  elements : TFhirProfileStructureSnapshotElementList;
begin
//  if FActualPath = '' then
//    path := id
//  else if not id.StartsWith(FStatedPath) then
//    raise Exception.Create('Bad Path "'+id+'"')
//  else
//   path := FActualPath+ id.Substring(FStatedPath.Length);

  if id.endsWith('/1') then
    id := id.subString(0, id.length-2);

  if (Types.Count = 0) or (Types[0].codeST = 'Resource') then
  begin
    path := id;
    profile := FProfile;
    structure := FStructure;
  end
  else if Types.Count = 1 then
  begin
    profile := FProfiles['http://hl7.org/fhir/Profile/'+Types[0].codeST];
    if (profile = nil) then
      raise Exception.Create('Unable to find profile for '+Types[0].codeST+' @ '+id);
    structure := profile.structureList[0];
    path := Types[0].codeST+id.Substring(statedPath.Length);
  end
  else if FType <> nil then
  begin
    profile := FProfiles['http://hl7.org/fhir/Profile/'+FType.codeST];
    if (profile = nil) then
      raise Exception.Create('Unable to find profile for '+FType.codeST+' @ '+id);
    structure := profile.structureList[0];
    if not id.startsWith(statedPath+'._'+FType.tagValue) then
      raise Exception.Create('Internal logic error');
    path := Types[0].codeST+id.Substring(statedPath.Length+2+FType.tagValue.length);
  end
  else
    raise Exception.Create('not handled - multiple types');
  elements := structure.snapshot.elementList;

  result := nil;
  for i := 0 to elements.Count - 1 do
    if elements[i].pathST = path then
    begin
      result := TProfileDefinition.Create(FProfiles.Link, profile.Link, structure.Link);
      try
        result.FElement := elements[i].Link;
        result.statedPath := id;
        result.link;
      finally
        result.free;
      end;
      break;
    end;

  if result = nil then
    raise Exception.Create('Unable to resolve path "'+id+'"');
end;

function TProfileDefinition.GetName: String;
begin
  result := path.substring(path.lastIndexOf('.')+1);
end;

function TProfileDefinition.GetPath: String;
begin
  result := FElement.pathST;
end;

function TProfileDefinition.GetTypes: TFhirProfileStructureSnapshotElementDefinitionTypeList;
begin
  result := FElement.definition.type_List;
end;
function TProfileDefinition.hasTypeChoice: boolean;
begin
  result := Types.Count > 1;
end;

procedure TProfileDefinition.setType(t: TFhirProfileStructureSnapshotElementDefinitionType);
begin
  FType.Free;
  FType := t;
end;

function TProfileDefinition.statedType: TFhirProfileStructureSnapshotElementDefinitionType;
begin
  if Types.Count = 0 then
    result := nil
  else if Types.Count = 1 then
    result := Types[0]
  else
    raise Exception.Create('Shouldn''t get here');
end;

{$ENDIF}

end.


