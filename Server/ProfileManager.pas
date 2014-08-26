unit ProfileManager;

interface

uses
  SysUtils,
  kCritSct,
  StringSupport,
  AdvObjects, AdvStringMatches, AdvStringObjectMatches,
  FHIRComponents, FHIRResources;


Type
  TProfileManager = class (TAdvObject)
  private
    lock : TCriticalSection;
    FProfilesByIdentifier : TAdvStringObjectMatch; // all current profiles by identifier (ValueSet.identifier)
    FProfilesByURL : TAdvStringObjectMatch; // all current profiles by their URL
    FProfilesByKey : TAdvStringObjectMatch; // all profiles by the key they are known from (mainly to support drop)

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SeeProfile(url : String; key : Integer; profile : TFHIRProfile);
    procedure DropProfile(key : Integer; url : String; aType : TFhirResourceType);

    function getExtensionDefn(source : TFhirProfile; url : String; var profile : TFhirProfile; var extension : TFhirProfileExtensionDefn) : boolean;
    function getStructure(source : TFhirProfile; url : String; var profile : TFhirProfile; var Structure : TFhirProfileStructure) : boolean;
    function getLinks : TAdvStringMatch;
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
      if profile.extensionDefnList[i].codeST = url.Substring(1) then
        extension := profile.extensionDefnList[i];
    result := extension <> nil;
  end;
end;

function TProfileManager.getLinks: TAdvStringMatch;
var
  i : integer;
begin
  lock.Lock('getLinks');
  try
    result := TAdvStringMatch.Create;
    try
      for i := 0 to FProfilesByURL.Count - 1 do
        result.Add(FProfilesByURL.KeyByIndex[i], TFHIRProfile(FProfilesByURL.ValueByIndex[i]).nameST);
      result.Link;
    finally
      result.Free;
    end;
  finally
    lock.Unlock;
  end;
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


end.
