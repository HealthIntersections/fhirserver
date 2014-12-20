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
    function getProfileStructure(source : TFhirProfile; url : String; var profile : TFhirProfile; var Structure : TFhirProfileStructure) : boolean;
    function getLinks(non_structures, non_resources : boolean) : TAdvStringMatch;

    property ProfileByURL[url : String] : TFhirProfile read GetProfileByUrl; default;
    property ProfileByType[aType : TFhirResourceType] : TFhirProfile read GetProfileByType;
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
      if profile.extensionDefnList[i].code = url.Substring(1) then
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
            bs := bs or p.structureList[j].publish;
            br := br or StringArrayExistsSensitive(CODES_TFhirResourceType, p.structureList[j].type_);
          end;
          if bs and br then
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

function TProfileManager.GetProfileByType(aType: TFhirResourceType): TFhirProfile;
begin
  result := GetProfileByUrl('http://hl7.org/fhir/Profile/'+CODES_TFHIRResourceType[aType]);
end;

function TProfileManager.GetProfileByUrl(url: String): TFhirProfile;
begin
  result := TFhirProfile(FProfilesByURL.GetValueByKey(url));
end;

function TProfileManager.getProfileStructure(source: TFhirProfile; url: String; var profile: TFhirProfile; var Structure: TFhirProfileStructure): boolean;
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
      if profile.structureList[i].name = code then
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
  for i := 0 to feed.entryList.Count - 1 do
    if feed.entryList[i].resource is TFHIRProfile then
      SeeProfile(feed.entryList[i].id, i, feed.entryList[i].resource as TFhirProfile);
end;

procedure TProfileManager.SeeProfile(url: String; key: Integer; profile: TFHIRProfile);
begin
  lock.Lock('SeeProfile');
  try
    FProfilesByIdentifier.Matches[profile.{$IFDEF FHIR-DST}identifier {$ELSE}url{$ENDIF}] := profile.Link;
    FProfilesByURL.Matches[url] := profile.Link;
    FProfilesByKey.Matches[inttostr(key)] := profile.Link;
  finally
    lock.Unlock;
  end;
end;


procedure TProfileManager.DropProfile(key: Integer; url: String; aType: TFhirResourceType);
var
  p : TFhirProfile;
begin
  lock.Lock('DropProfile');
  try
    p := TFhirProfile(FProfilesByKey.GetValueByKey(inttostr(key)));
    if p <> nil then
    begin
      FProfilesByURL.DeleteByKey(url);
      FProfilesByIdentifier.DeleteByKey(p.identifier);
      FProfilesByKey.DeleteByKey(inttostr(key));
    end;
  finally
    lock.Unlock;
  end;
end;

end.


