unit ProfileManager;

interface

uses
  SysUtils,
  kCritSct,
  StringSupport,
  AdvObjects, AdvStringMatches, AdvStringObjectMatches,
  FHIRComponents, FHIRResources, FHIRUtilities, FHIRConstants, FHIRTypes;


Type
  TProfileManager = class (TAdvObject)
  private
    lock : TCriticalSection;
    FProfilesByIdentifier : TAdvStringObjectMatch; // all current profiles by identifier (ValueSet.identifier)
    FProfilesByURL : TAdvStringObjectMatch; // all current profiles by their URL
    FProfilesByKey : TAdvStringObjectMatch;
    FExtensions : TAdvStringObjectMatch;
    function GetProfileByUrl(url: String): TFHirStructureDefinition;
    function GetProfileByType(aType: TFhirResourceType): TFHirStructureDefinition; // all profiles by the key they are known from (mainly to support drop)

  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TProfileManager; overload;

    procedure SeeProfile(base : String; key : Integer; profile : TFHirStructureDefinition);
    procedure DropProfile(base : String; key : Integer; url : String; aType : TFhirResourceType);
    procedure loadFromFeed(feed : TFHIRBundle);

    function getExtensionDefn(source : TFHirStructureDefinition; url : String; var profile : TFHirStructureDefinition; var extension : TFHirStructureDefinition) : boolean;
    function getProfileStructure(source : TFHirStructureDefinition; url : String; var profile : TFHirStructureDefinition) : boolean;
    function getLinks(non_resources : boolean) : TAdvStringMatch;

    property ProfileByURL[url : String] : TFHirStructureDefinition read GetProfileByUrl; default;
    property ProfileByType[aType : TFhirResourceType] : TFHirStructureDefinition read GetProfileByType;
  end;

  {
  This encapsulates a reference to an element definition within a structure.
  The path may be replace
  }
  TProfileDefinition = class (TAdvObject)
  private
    FProfiles : TProfileManager;
    FProfile : TFHirStructureDefinition;
    FElement : TFhirElementDefinition;
    statedPath : String;
    FType : TFhirElementDefinitionType;

    function GetTypes: TFhirElementDefinitionTypeList;
    function GetPath: String;
    function GetName: String;
    Property Types : TFhirElementDefinitionTypeList read GetTypes;
  public
    Constructor Create(profiles : TProfileManager; profile : TFHirStructureDefinition); overload;
    Destructor Destroy; override;

    procedure setType(t : TFhirElementDefinitionType);
    function statedType : TFhirElementDefinitionType;
    function hasTypeChoice : boolean;
    Property path : String read GetPath;
    Property name : String read GetName;
    function getById(id : String) : TProfileDefinition;
  end;

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

function TProfileManager.getExtensionDefn(source: TFHirStructureDefinition; url: String; var profile: TFHirStructureDefinition; var extension : TFHirStructureDefinition): boolean;
var
  id, code : String;
  i : integer;
begin
{  result := false;
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
      profile := FProfilesByIdentifier.Matches[id] as TFHirStructureDefinition;
    finally
      lock.Unlock;
    end;
  end;

  if (profile <> nil) then
  begin
    extension := nil;
    for i := 0 to profile.extensionDefnList.Count - 1 do
      if profile.extensionDefnList[i].code = url.Substring(1) then
        extension := profile.extensionDefnList[i];
    result := extension <> nil;
  end;}

end;

function TProfileManager.getLinks(non_resources : boolean): TAdvStringMatch;
var
  i, j : integer;
  p : TFHirStructureDefinition;
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
          p := TFHirStructureDefinition(FProfilesByURL.ValueByIndex[i]);
          if non_resources or StringArrayExistsSensitive(CODES_TFhirResourceType, p.snapshot.elementList[0].path) then
            result.Add(url, p.name);
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

function TProfileManager.GetProfileByType(aType: TFhirResourceType): TFHirStructureDefinition;
begin
  result := GetProfileByUrl('http://hl7.org/fhir/Profile/'+CODES_TFHIRResourceType[aType]);
end;

function TProfileManager.GetProfileByUrl(url: String): TFHirStructureDefinition;
begin
  result := TFHirStructureDefinition(FProfilesByURL.GetValueByKey(url));
end;

function TProfileManager.getProfileStructure(source: TFHirStructureDefinition; url: String; var profile: TFHirStructureDefinition): boolean;
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
      profile := FProfilesByIdentifier.Matches[id] as TFHirStructureDefinition;
    finally
      lock.Unlock;
    end;
  end;

{  if (profile <> nil) then
  begin
    structure := nil;
    for i := 0 to profile.structureList.Count - 1 do
      if profile.structureList[i].name = code then
        structure := profile.structureList[i];
    result := structure <> nil;
  end;}
end;

function TProfileManager.Link: TProfileManager;
begin
  result := TProfileManager(inherited Link);
end;

procedure TProfileManager.loadFromFeed(feed: TFHIRBundle);
var
  i : integer;
  base : String;
begin
  for i := 0 to feed.entryList.Count - 1 do
  begin
    if feed.entryList[i].base <> '' then
      base := feed.entryList[i].base
    else
      base := feed.base;

    if feed.entryList[i].resource is TFHirStructureDefinition then
      SeeProfile(base, i, feed.entryList[i].resource as TFHirStructureDefinition);
  end;
end;

procedure TProfileManager.SeeProfile(base : String; key: Integer; profile: TFHirStructureDefinition);
begin
  lock.Lock('SeeProfile');
  try
    FProfilesByIdentifier.Matches[profile.{$IFDEF FHIR-DST}identifier {$ELSE}url{$ENDIF}] := profile.Link;
    FProfilesByURL.Matches[profile.id] := profile.Link;
    FProfilesByKey.Matches[inttostr(key)] := profile.Link;
  finally
    lock.Unlock;
  end;
end;


procedure TProfileManager.DropProfile(base : String; key: Integer; url: String; aType: TFhirResourceType);
var
  p : TFHirStructureDefinition;
begin
  lock.Lock('DropProfile');
  try
    p := TFHirStructureDefinition(FProfilesByKey.GetValueByKey(inttostr(key)));
    if p <> nil then
    begin
      FProfilesByURL.DeleteByKey(url);
      FProfilesByIdentifier.DeleteByKey(fullResourceUri(base, aType, p.id));
      FProfilesByKey.DeleteByKey(inttostr(key));
    end;
  finally
    lock.Unlock;
  end;
end;

{ TProfileDefinition }

constructor TProfileDefinition.Create(profiles: TProfileManager; profile: TFHirStructureDefinition);
begin
  Create;
  FProfiles := profiles;
  FProfile := profile;
  FElement := profile.snapshot.elementList[0].link;
end;

destructor TProfileDefinition.Destroy;
begin
  FType.free;
  FProfiles.Free;
  FProfile.Free;
  FElement.Free;
  inherited;
end;

function TProfileDefinition.getById(id: String): TProfileDefinition;
var
  path : String;
  i : integer;
  profile : TFHirStructureDefinition;
  elements : TFhirElementDefinitionList;
begin
//  if FActualPath = '' then
//    path := id
//  else if not id.StartsWith(FStatedPath) then
//    raise Exception.Create('Bad Path "'+id+'"')
//  else
//   path := FActualPath+ id.Substring(FStatedPath.Length);

  if id.endsWith('/1') then
    id := id.subString(0, id.length-2);

  if (Types.Count = 0) or (Types[0].code = 'Resource') then
  begin
    path := id;
    profile := FProfile;
  end
  else if Types.Count = 1 then
  begin
    profile := FProfiles['http://hl7.org/fhir/Profile/'+Types[0].code];
    if (profile = nil) then
      raise Exception.Create('Unable to find profile for '+Types[0].code+' @ '+id);
    path := Types[0].code+id.Substring(statedPath.Length);
  end
  else if FType <> nil then
  begin
    profile := FProfiles['http://hl7.org/fhir/Profile/'+FType.code];
    if (profile = nil) then
      raise Exception.Create('Unable to find profile for '+FType.code+' @ '+id);
    if not id.startsWith(statedPath+'._'+FType.tags['type']) then
      raise Exception.Create('Internal logic error');
    path := Types[0].code+id.Substring(statedPath.Length+2+FType.tags['type'].length);
  end
  else
    raise Exception.Create('not handled - multiple types');
  elements := profile.snapshot.elementList;

  result := nil;
  for i := 0 to elements.Count - 1 do
    if elements[i].path = path then
    begin
      result := TProfileDefinition.Create(FProfiles.Link, profile.Link);
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
  result := FElement.path;
end;

function TProfileDefinition.GetTypes: TFhirElementDefinitionTypeList;
begin
  result := FElement.type_List;
end;

function TProfileDefinition.hasTypeChoice: boolean;
begin
  result := Types.Count > 1;
end;

procedure TProfileDefinition.setType(t: TFhirElementDefinitionType);
begin
  FType.Free;
  FType := t;
end;

function TProfileDefinition.statedType: TFhirElementDefinitionType;
begin
  if Types.Count = 0 then
    result := nil
  else if Types.Count = 1 then
    result := Types[0]
  else
    raise Exception.Create('Shouldn''t get here');
end;


end.


