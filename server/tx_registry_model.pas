unit tx_registry_model;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_json, fsl_utilities, fsl_versions;

Type
  TServerSecurity = (ssOpen, ssPassword, ssToken, ssOAuth, ssSmart, ssCert);
  TServerSecuritySet = set of TServerSecurity;

const
  CODES_TServerSecurity : Array[TServerSecurity] of String = ('open', 'password', 'token', 'oauth', 'smart', 'cert');

type
  { TServerVersionInformation }

  TServerVersionInformation = class (TFslObject)
  private
    FError: String;
    FAddress : String;
    FVersion : String;
    FTerminologies : TStringList;
    FLastSuccess : TFslDateTime;
    FSecurity : TServerSecuritySet;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TServerVersionInformation; overload;
    property Version : String read FVersion write FVersion;
    property Address : String read FAddress write FAddress;
    property Security : TServerSecuritySet read FSecurity write FSecurity;
    property Error : String read FError write FError;
    property LastSuccess : TFslDateTime read FLastSuccess write FLastSuccess;
    property Terminologies : TStringList read FTerminologies;     
    procedure update(source : TServerVersionInformation);

    function Details : String;
    function cslist : String;
  end;

  { TServerInformation }

  TServerInformation = class (TFslObject)
  private
    FCode: String;
    FName : string;      
    FAddress : String;
    FAccessInfo : String;
    FAuthlist : TStringList;
    FUsageList : TStringList;
    FVersions : TFslList<TServerVersionInformation>;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TServerInformation; overload;
    property Code : String read FCode write FCode;
    property Name : String read FName write FName; 
    property Address : String read FAddress write FAddress;
    property AccessInfo : String read FAccessInfo write FAccessInfo;
    property AuthList : TStringList read FAuthList;
    property UsageList : TStringList read FUsageList;
    property Versions : TFslList<TServerVersionInformation> read FVersions;
    function version(ver : String) : TServerVersionInformation;
    procedure update(source : TServerInformation);

    function Details : String;
    function isAuth(tx : String) : boolean;  
    function Description : String;
  end;

  { TServerRegistry }

  TServerRegistry = class (TFslObject)
  private
    FCode: String;
    FName : string;
    FAddress : String;
    FAuthority : string;
    FError : String;
    FServers : TFslList<TServerInformation>;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TServerRegistry; overload;
    property Code : String read FCode write FCode;
    property Name : String read FName write FName;
    property Address : String read FAddress write FAddress;
    property Authority : String read FAuthority write FAuthority;
    property Error : String read FError write FError;
    property Servers : TFslList<TServerInformation> read FServers;

    function server(code : String) : TServerInformation;
    procedure update(source : TServerRegistry);
  end;

  { TServerRegistries }

  TServerRegistries = class (TFslObject)
  private
    FAddress : String;
    FDoco: String;
    FLastRun : TFslDateTime;
    FOutcome : String;
    FRegistries: TFslList<TServerRegistry>;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TServerRegistries; overload;
    property Address : String read FAddress write FAddress;
    property doco : String read FDoco write FDoco;
    property LastRun : TFslDateTime read FLastRun write FLastRun;
    property Outcome : String read FOutcome write FOutcome;
    property Registries : TFslList<TServerRegistry> read FRegistries;
                                               
    function registry(code : String) : TServerRegistry;
    procedure update(source : TServerRegistries);
  end;

  { TServerRow }

  TServerRow = class (TFslObject)
  private
    FError: String;
    FLastSuccess: cardinal;
    FRegistryCode: String;
    FRegistryName: String;
    FRegistryUrl: String;
    FSecurity: TServerSecuritySet;
    FServerCode: String;
    FServerName: String;
    FSystems: integer;
    FURL: String;
    FVersion: String;
    FAuthlist : TStringList;
    FAuthoritative : boolean;
  public             
    constructor Create; override;
    destructor Destroy; override;
    function Link : TServerRow; overload;
    property ServerName : String read FServerName write FServerName;
    property ServerCode : String read FServerCode write FServerCode;
    property RegistryName : String read FRegistryName write FRegistryName;
    property RegistryCode : String read FRegistryCode write FRegistryCode;   
    property RegistryUrl : String read FRegistryUrl write FRegistryUrl;    
    property AuthList : TStringList read FAuthList;

    property Version : String read FVersion write FVersion;
    property URL : String read FURL write FURL;
    property Error : String read FError write FError;    
    property Security : TServerSecuritySet read FSecurity write FSecurity;
    property LastSuccess : cardinal read FLastSuccess write FLastSuccess; // ms
    property systems : integer read FSystems write FSystems;
    property Authoritative : boolean read FAuthoritative write FAuthoritative;
  end;

  { TServerRegistryUtilities }

  TServerRegistryUtilities = class (TFslObject)
  private
    class function securitySetToString(sset : TServerSecuritySet) : String;
    class function toJson(v : TServerVersionInformation) : TJsonObject;  overload;
    class function toJson(s : TServerInformation) : TJsonObject;  overload;
    class function toJson(r : TServerRegistry) : TJsonObject;  overload;

    class function stringToSecuritySet(s : String) : TServerSecuritySet;
    class function readVersion(fv : String; json : TJsonObject): TServerVersionInformation;
    class function readServer(fv : String; json : TJsonObject): TServerInformation;
    class function readRegistry(fv : String; json : TJsonObject): TServerRegistry;

    class procedure addRow(rows : TFslList<TServerRow>; reg: TServerRegistry; srvr : TServerInformation; version : TServerVersionInformation; auth : boolean);
    class procedure buildRows(reg: TServerRegistry; srvr : TServerInformation; version, tx : String; rows : TFslList<TServerRow>); overload;
    class procedure buildRows(reg : TServerRegistry; srvrCode, version, tx : String; rows : TFslList<TServerRow>); overload;
    class procedure buildRows(info : TServerRegistries; regCode, srvrCode, version, tx : String; rows : TFslList<TServerRow>); overload;

  public
    class function fromJson(json : TJsonObject) : TServerRegistries;
    class function toJson(reg : TServerRegistries) : TJsonObject; overload;
    class function toJson(row : TServerRow) : TJsonObject; overload;

    class function buildRows(info : TServerRegistries; regCode, srvrCode, version, tx : String) : TFslList<TServerRow>; overload;
    class function hasMatchingCodeSystem(cs : String; list : TStringList; mask : boolean) : boolean;
  end;

implementation

{ TServerRow }

constructor TServerRow.Create;
begin
  inherited Create;
  FAuthlist := TStringList.Create;
end;

destructor TServerRow.Destroy;
begin
  FAuthlist.free;
  inherited Destroy;
end;

function TServerRow.Link: TServerRow;
begin
  result := TServerRow(inherited Link);
end;

{ TServerRegistryUtilities }

class function TServerRegistryUtilities.securitySetToString(sset: TServerSecuritySet): String;
var
  a : TServerSecurity;
begin
  result := '';
  for a := low(TServerSecurity) to High(TServerSecurity) do
    if a in sset then
      CommaAdd(result, CODES_TServerSecurity[a]);
end;


class function TServerRegistryUtilities.stringToSecuritySet(s : String) : TServerSecuritySet;
var
  a : TServerSecurity;
begin
  result := [];
  for a := low(TServerSecurity) to High(TServerSecurity) do
    if s.Contains(CODES_TServerSecurity[a]) then
      result := result + [a];
end;

class function TServerRegistryUtilities.toJson(v: TServerVersionInformation): TJsonObject;
var
  s : String;
begin
  result := TJsonObject.Create;
  try              
    result.str['address'] := v.Address;
    result.str['version'] := v.Version;
    result.str['security'] := securitySetToString(v.Security);
    result.str['error'] := v.Error;
    result.str['last-success'] := v.LastSuccess.toXML;
    for s in v.Terminologies do
      result.forceArr['terminologies'].add(s);
    result.link;
  finally
    result.free;
  end;
end;

class function TServerRegistryUtilities.readVersion(fv : String; json: TJsonObject): TServerVersionInformation;
var
  s : String;
begin
  result := TServerVersionInformation.Create;
  try                             
    result.Address := json.str['address'];
    result.Version := json.str['version'];
    result.Security := stringToSecuritySet(json.str['security']);
    result.Error := json.str['error'];
    result.LastSuccess := TFslDateTime.fromXML(json.str['last-success']);
    json.forceArr['terminologies'].readStrings(result.Terminologies);
    result.link;
  finally
    result.free;
  end;
end;


class function TServerRegistryUtilities.toJson(s: TServerInformation): TJsonObject;
var
  v : TServerVersionInformation;
begin
  result := TJsonObject.Create;
  try
    result.str['code'] := s.Code;
    result.str['name'] := s.Name;
    result.str['address'] := s.Address;
    result.str['access-info'] := s.AccessInfo;
    result.str['authoritative'] := s.AuthList.CommaText;
    for v in s.Versions do
      result.forceArr['versions'].add(toJson(s));
    result.link;
  finally
    result.free;
  end;
end;


class function TServerRegistryUtilities.readServer(fv : String; json: TJsonObject): TServerInformation;
var
  obj : TJsonObject;
begin
  result := TServerInformation.Create;
  try                                
    result.Code := json.str['code'];
    result.Name := json.str['name'];
    result.Address := json.str['address'];
    result.AccessInfo := json.str['access-info'];
    result.AuthList.CommaText := json.str['authoritative'];;
    for obj in json.forceArr['versions'].asObjects.forEnum do
      result.versions.add(readVersion(fv, json));
    result.link;
  finally
    result.free;
  end;
end;


class function TServerRegistryUtilities.toJson(r: TServerRegistry): TJsonObject;
var
  s : TServerInformation;
begin
  result := TJsonObject.Create;
  try                
    result.str['code'] := s.Code;
    result.str['name'] := r.Name;
    result.str['address'] := r.Address;
    result.str['authority'] := r.Authority;
    result.str['error'] := r.Error;
    for s in r.Servers do
      result.forceArr['servers'].add(toJson(s));
    result.link;
  finally
    result.free;
  end;
end;

class function TServerRegistryUtilities.readRegistry(fv : String; json: TJsonObject): TServerRegistry;
var
  obj : TJsonObject;
begin
  result := TServerRegistry.Create;
  try
    result.Code := json.str['code'];
    result.Name := json.str['name'];
    result.Address := json.str['address'];
    result.Authority := json.str['authority'];
    result.Error := json.str['error'];
    for obj in json.forceArr['servers'].asObjects.forEnum do
      result.Servers.add(readServer(fv, json));
    result.link;
  finally
    result.free;
  end;
end;

class procedure TServerRegistryUtilities.addRow(rows: TFslList<TServerRow>; reg: TServerRegistry; srvr: TServerInformation; version: TServerVersionInformation; auth : boolean);
var
  row : TServerRow;
begin
  row := TServerRow.Create;
  try
    row.Authoritative := auth;
    row.ServerName := srvr.Name;
    row.ServerCode := srvr.Code;
    row.RegistryName := reg.Name;
    row.RegistryCode := reg.Code;
    row.RegistryUrl := reg.Address;

    row.URL := version.Address;
    row.Error := version.Error;
    if (version.LastSuccess.null) then
      row.LastSuccess := 0
    else
      row.LastSuccess := trunc(TFslDateTime.makeUTC.difference(version.LastSuccess) * DATETIME_DAY_MILLISECONDS);
    row.security := version.security;
    row.Version := version.Version;
    row.systems := version.Terminologies.Count;
    row.Authlist.assign(srvr.AuthList);

    rows.add(row.link);
  finally
    row.free;
  end;
end;

function passesMask(mask, tx : string) : Boolean;
begin
  if mask.EndsWith('*') then
    result := tx.StartsWith(mask.Substring(0, mask.length-1))
  else
    result := tx = mask;
end;


class function TServerRegistryUtilities.hasMatchingCodeSystem(cs : String; list : TStringList; mask : boolean) : boolean;
var
  s, r : String;
begin
  r := cs;
  if r.contains('|') then
    r := r.subString(0, r.indexOf('|'));
  result := false;
  for s in list do
  begin
    if mask and passesMask(s, cs) then
      exit(true);
    if not mask and ((s = cs) or (r = s)) then
      exit(true);
  end;
end;

class procedure TServerRegistryUtilities.buildRows(reg: TServerRegistry; srvr: TServerInformation; version, tx: String; rows: TFslList<TServerRow>);
var
  ver : TServerVersionInformation;
  auth : boolean;
begin
  auth := hasMatchingCodeSystem(tx, srvr.AuthList, true);
  for ver in srvr.Versions do
    if (version = '') or (TSemanticVersion.matches(version, ver.version, semverAuto)) then
      begin
        if auth or (tx = '') or hasMatchingCodeSystem(tx, ver.Terminologies, false) then
          addRow(rows, reg, srvr, ver, auth);
      end;
end;

class procedure TServerRegistryUtilities.buildRows(reg: TServerRegistry; srvrCode, version, tx: String; rows: TFslList<TServerRow>);
var
  srvr : TServerInformation;
begin
  for srvr in reg.Servers do
    if (srvrCode = '') or (srvr.Code = srvrCode) then
      buildRows(reg, srvr, version, tx, rows);
end;

class procedure TServerRegistryUtilities.buildRows(info: TServerRegistries; regCode, srvrCode, version, tx: String; rows: TFslList<TServerRow>);
var
  reg : TServerRegistry;
begin
  for reg in info.Registries do
    if (regCode = '') or (reg.Code = regCode) then
      buildRows(reg, srvrCode, version, tx, rows);
end;

class function TServerRegistryUtilities.toJson(reg: TServerRegistries): TJsonObject;
var
  sr : TServerRegistry;
begin
  result := TJsonObject.Create;
  try
    result.str['version'] := '1';
    result.str['address'] := reg.Address;
    result.str['last-run'] := reg.LastRun.toXML;
    result.str['outcome'] := reg.Outcome;
    for sr in reg.Registries do
      result.forceArr['registries'].add(toJson(sr));
    result.link;
  finally
    result.free;
  end;
end;

class function TServerRegistryUtilities.toJson(row: TServerRow): TJsonObject;
var
  s : String;
begin
  result := TJsonObject.Create;
  try
    if (row.Authoritative) then
      result.bool['is-authoritative'] := true;
    result.str['server-name'] := row.ServerName;
    result.str['server-code'] := row.ServerCode;

    result.str['registry-name'] := row.RegistryName;
    result.str['registry-code'] := row.RegistryCode;
    result.str['registry-url'] := row.RegistryUrl;

    result.str['url'] := row.URL;
    result.str['version'] := row.Version;
    result.str['error'] := row.Error;
    result.int['last-success'] := row.LastSuccess;
    result.int['systems'] := row.systems;
    for s in row.Authlist do
      result.forceArr['authoritative'].add(s);

    if (ssOpen in row.Security) then result.bool[CODES_TServerSecurity[ssOpen]] := true;
    if (ssPassword in row.Security) then result.bool[CODES_TServerSecurity[ssPassword]] := true;
    if (ssToken in row.Security) then result.bool[CODES_TServerSecurity[ssToken]] := true;
    if (ssOAuth in row.Security) then result.bool[CODES_TServerSecurity[ssOAuth]] := true;
    if (ssSmart in row.Security) then result.bool[CODES_TServerSecurity[ssSmart]] := true;
    if (ssCert in row.Security) then result.bool[CODES_TServerSecurity[ssCert]] := true;

    result.link;
  finally
    result.free;
  end;
end;

class function TServerRegistryUtilities.buildRows(info: TServerRegistries; regCode, srvrCode, version, tx: String): TFslList<TServerRow>;
begin
  result := TFslList<TServerRow>.Create;
  try
    buildRows(info, regCode, srvrCode, version, tx, result);
    result.link;
  finally
    result.free;
  end;
end;

class function TServerRegistryUtilities.fromJson(json: TJsonObject): TServerRegistries;
var
  fv : String;
  obj : TJsonObject;
begin
  fv := json.str['version'];
  if (fv <> '1') then
    raise EFslException.Create('Unsupported version '+fv);

  result := TServerRegistries.Create;
  try
    result.Address := json.str['address'];
    result.LastRun :=  TFslDateTime.fromXML(json.str['last-run']);
    result.Outcome := json.str['outcome'];
    for obj in json.forceArr['registries'].asObjects.forEnum do
      result.Registries.add(readRegistry(fv, json));
    result.link;
  finally
    result.free;
  end;
end;

{ TServerRegistries }

constructor TServerRegistries.Create;
begin
  inherited Create;
  FRegistries := TFslList<TServerRegistry>.Create;
end;

destructor TServerRegistries.Destroy;
begin
  FRegistries.free;
  inherited Destroy;
end;

function TServerRegistries.Link: TServerRegistries;
begin
  result := TServerRegistries(inherited link);
end;

function TServerRegistries.registry(code: String): TServerRegistry;
var
  t : TServerRegistry;
begin
  result := nil;
  for t in FRegistries do
    if (t.code = code) then
      exit(t);
end;

procedure TServerRegistries.update(source: TServerRegistries);
var
  t, sr : TServerRegistry;
begin
  FLastRun := source.FLastRun;
  FOutcome := source.FOutcome;
  FDoco := source.doco;
  for t in source.Registries do
  begin
    sr := registry(t.Code);
    if (sr = nil) then
      FRegistries.add(t.link)
    else
      sr.update(t);
  end;
end;

{ TServerRegistry }

constructor TServerRegistry.Create;
begin
  inherited Create;
  FServers := TFslList<TServerInformation>.Create;
end;

destructor TServerRegistry.Destroy;
begin
  FServers.free;
  inherited Destroy;
end;

function TServerRegistry.Link: TServerRegistry;
begin
  result := TServerRegistry(inherited link);
end;

function TServerRegistry.server(code: String): TServerInformation;
var
  t : TServerInformation;
begin
  result := nil;
  for t in FServers do
    if (t.code = code) then
      exit(t);
end;

procedure TServerRegistry.update(source: TServerRegistry);
var
  t, s : TServerInformation;
begin
  FName := source.FName;
  FAddress := source.FAddress;
  FAuthority := source.FAuthority;
  FError := source.FError;
  for t in source.Servers do
  begin
    s := server(t.Code);
    if (s = nil) then
      FServers.add(t.link)
    else
      s.update(t);
  end;
end;

{ TServerInformation }

constructor TServerInformation.Create;
begin
  inherited Create;  
  FVersions := TFslList<TServerVersionInformation>.Create;
  FAuthlist := TStringList.Create;
  FUsageList := TStringList.create;
end;

destructor TServerInformation.Destroy;
begin
  FUsageList.free;
  FAuthlist.free;
  FVersions.free;
  inherited Destroy;
end;

function TServerInformation.Link: TServerInformation;
begin
  result := TServerInformation(inherited link);
end;

function TServerInformation.version(ver: String): TServerVersionInformation;
var
  t : TServerVersionInformation;
begin
  result := nil;
  for t in FVersions do
    if (t.version = ver) then
      exit(t);
end;

procedure TServerInformation.update(source: TServerInformation);
var
  t, v : TServerVersionInformation;
begin
  FName := source.FName;
  FAddress := source.FAddress;
  FAccessInfo := source.FAccessInfo;
  FAuthlist.Assign(source.FAuthlist);
  FUsagelist.Assign(source.FUsagelist);
  for t in source.Versions do
  begin
    v := version(t.Version);
    if (v = nil) then
      FVersions.add(t.link)
    else
      v.update(t);
  end;
end;

function TServerInformation.Details: String;
begin
  result := FAccessInfo;
end;

function TServerInformation.isAuth(tx: String): boolean;
var
  mask : String;
begin
  result := false;
  for mask in AuthList do
    if passesMask(mask, tx) then
      exit(true);
end;

function TServerInformation.description: String;
var
  s : String;
begin
  result := '';
  if (FusageList.count > 0) then
    result := 'Usage Tags: '+FUsageList.CommaText;
  if (FAuthList.count > 0) then
  begin
    if (result <> '') then
      result := result+'. ';
    result := result + 'Authoritative for: <ul>';
    for s in FAuthlist do
      result := result + '<li>'+FormatTextToHtml(s)+'</li>';
    result := result + '</ul>';
  end;
end;

{ TServerVersionInformation }

constructor TServerVersionInformation.Create;
begin
  inherited Create;   
  FTerminologies := TStringList.Create;
end;

destructor TServerVersionInformation.Destroy;
begin
  FTerminologies.free;
  inherited Destroy;
end;

function TServerVersionInformation.Link: TServerVersionInformation;
begin
  result := TServerVersionInformation(inherited link);
end;

procedure TServerVersionInformation.update(source: TServerVersionInformation);
begin
  FAddress := source.FAddress;
  FError := source.FError;
  if (source.Error = '') then
  begin
    FSecurity := source.FSecurity;
    FLastSuccess := source.FLastSuccess;
    FTerminologies.assign(source.Terminologies);
  end;
end;

function TServerVersionInformation.Details: String;
begin
  if FError = '' then
    result := 'All Ok'
  else
    result := FError;
  result := result + ' (last seen '+LastSuccess.toXML()+')';
end;

function TServerVersionInformation.cslist: String;
var
  s : String;
begin
  result := '<ul>';
  for s in FTerminologies do
    result := result + '<li>'+FormatTextToHtml(s)+'</li>';
  result := result + '</ul>';
end;


end.

