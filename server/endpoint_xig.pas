unit endpoint_xig;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

{$i fhir.inc}

interface

uses
  SysUtils, Classes, ZStream,
  IdContext, IdCustomHTTPServer, IdOpenSSLX509,
  fsl_base, fsl_utilities, fsl_json, fsl_i18n, fsl_http, fsl_html,
  fhir_objects,
  fdb_manager,
  utilities, server_config, tx_manager, time_tracker, kernel_thread,
  web_base, endpoint, server_stats;

type
  TContentMode = (cmAll, cmCodeSystem, cmResProfile, cmDTProfile, cmLogical, cmExtensions, cmValueSet, cmConceptMap);


  { TPackageInformation }

  TPackageInformation = class (TFslObject)
  private
    FCanonical: String;
    FId: String;
    Fkey: String;
    FVid: String;
    FWeb: String;
  public
    constructor create(key, id, vid, web, canonical : String);

    function link : TPackageInformation; overload;

    property key : String read Fkey write FKey;
    property id : String read FId write FId;
    property vid : String read FVid write FVid;
    property web : String read FWeb write FWeb;
    property canonical : String read FCanonical write FCanonical;
  end;

  { TFHIRXIGWebServer }

  TFHIRXIGWebServer = class (TFhirWebServerEndpoint)
  private
    FVersions : TStringList;
    FRealms : TStringList;
    FAuthorities : TStringList;
    FTypes : TStringList;
    FDatabase : TFDBManager;
    FPackages : TFslMap<TPackageInformation>;
    FPackagesById : TFslMap<TPackageInformation>;
    FResourceTypes : TStringList;
    FProfileResources : TStringList;
    FProfileTypes : TStringList;
    FExtensionContexts : TStringList;
    FExtensionTypes : TStringList;
    FTerminologySources : TStringList;

    function authBar(secure: boolean; realm, auth, ver, rtype, rt, text: String): String;
    function realmBar(secure: boolean; realm, auth, ver, rtype, rt, text: String): String;
    function typeBar(secure: boolean; realm, auth, ver, rtype, rt, text: String): String;
    function versionBar(secure: boolean; realm, auth, ver, rtype, rt, text: String): String;
    function makeSelect(rt : String; list : TStringList) : String;

    function contentAll(mode : TContentMode; secure: boolean; realm, auth, ver, rt, text, offset: String): String;
    procedure renderExtension(b : TFslStringBuilder; details : String);
    function hasTerminologySource(s : String): boolean;
    function extLink(url : String) : String;
    function contentRes(pid, rtype, id  : String) : String;

    procedure sendViewHtml(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; secure : boolean; rtype, auth, realm, ver, rt, text, offset : String);
    procedure sendResourceHtml(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; secure : boolean; pid, rtype, id  : String);
    function doRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String; secure: boolean): String;
  public
    Constructor Create(code, path : String; common : TFHIRWebServerCommon); override;
    destructor Destroy; override;
    function link  : TFHIRXIGWebServer; overload;
    function description : String; override;

    function PlainRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String; tt : TTimeTracker) : String; override;
    function SecureRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String; tt : TTimeTracker) : String; override;
    function logId : string; override;
  end;

  { TXIGServerEndPoint }

  TXIGServerEndPoint = class (TFHIRServerEndPoint)
  private
    FXIGServer : TFHIRXIGWebServer;
    procedure loadFromDB;
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; common : TCommonTerminologies; i18n : TI18nSupport);
    destructor Destroy; override;

    function summary : String; override;
    function makeWebEndPoint(common : TFHIRWebServerCommon) : TFhirWebServerEndpoint; override;
    procedure InstallDatabase(params : TCommandLineParameters); override;
    procedure UninstallDatabase; override;
    procedure LoadPackages(installer : boolean; plist : String); override;
    procedure updateAdminPassword(pw : String); override;
    procedure Load; override;
    Procedure Unload; override;
    procedure internalThread(callback : TFhirServerMaintenanceThreadTaskCallBack); override;
    function cacheSize(magic : integer) : UInt64; override;
    procedure clearCache; override;
    procedure SweepCaches; override;
    procedure SetCacheStatus(status : boolean); override;
    procedure getCacheInfo(ci: TCacheInformation); override;
    procedure recordStats(rec : TStatusRecord); override;
  end;

implementation

{ TPackageInformation }

constructor TPackageInformation.create(key, id, vid, web, canonical: String);
begin
  inherited create;
  FKey := key;
  FId := id;
  FVid := vid;
  FWeb := web;
  if canonical = '' then
    FCanonical := '!!!'
  else
    FCanonical  := canonical+'/';
end;

function TPackageInformation.link: TPackageInformation;
begin
  result := TPackageInformation(inherited Link);
end;

{ TXIGServerEndPoint }

constructor TXIGServerEndPoint.Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; common : TCommonTerminologies; i18n : TI18nSupport);
begin
  inherited Create(config, settings, nil, common, nil, i18n);
end;

destructor TXIGServerEndPoint.Destroy;
begin
  FXIGServer.free;

  inherited;
end;

function TXIGServerEndPoint.cacheSize(magic : integer): UInt64;
begin
  result := inherited cacheSize(magic);
end;

procedure TXIGServerEndPoint.clearCache;
begin
  inherited;
end;

procedure TXIGServerEndPoint.SweepCaches;
begin
  inherited SweepCaches;
end;

procedure TXIGServerEndPoint.getCacheInfo(ci: TCacheInformation);
begin
  inherited;
end;

procedure TXIGServerEndPoint.recordStats(rec: TStatusRecord);
begin
  inherited recordStats(rec);
//  rec.
end;

procedure TXIGServerEndPoint.loadFromDB;
var
  conn : TFDBConnection;
  pck : TPackageInformation;
begin
  conn := Database.GetConnection('Load');
  try
    conn.SQL := 'select Code from realms';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
      FXIGServer.FRealms.add(conn.ColStringByName['Code']);
    conn.terminate;
    FXIGServer.FRealms.Sort;

    conn.SQL := 'select Code from Authorities';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
      FXIGServer.FAuthorities.add(conn.ColStringByName['Code']);
    conn.terminate;
    FXIGServer.FAuthorities.sort;


    conn.SQL := 'select PackageKey, Id, PID, Web, Canonical from Packages';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
    begin
      pck := TPackageInformation.create(conn.ColStringByName['PackageKey'], conn.ColStringByName['Id'], conn.ColStringByName['PID'].replace('#', '|'), conn.ColStringByName['Web'], conn.ColStringByName['Canonical']);
      try
        FXIGServer.FPackages.add(pck.key, pck.link);
        FXIGServer.FPackagesById.add(pck.vid, pck.link);
      finally
        pck.free;
      end;
    end;
    conn.terminate;
    FXIGServer.FAuthorities.sort;
                                    
    conn.SQL := 'Select distinct type from Resources where ResourceType = ''StructureDefinition'' and Kind = ''resource''';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
      if conn.ColStringByName['Type'] <> '' then
      FXIGServer.FProfileResources.add(conn.ColStringByName['Type']);
    conn.terminate;
    FXIGServer.FProfileResources.Sort;

    conn.SQL := 'Select distinct type from Resources where ResourceType = ''StructureDefinition'' and (Kind = ''complex-type'' or Kind = ''primitive-type'')';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
      if (conn.ColStringByName['Type'] <> 'Extension') and (conn.ColStringByName['Type'] <> '') then
        FXIGServer.FProfileTypes.add(conn.ColStringByName['Type']);
    conn.terminate;
    FXIGServer.FProfileTypes.Sort;
                     
    conn.SQL := 'Select distinct ResourceType from Resources';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
      if (conn.ColStringByName['ResourceType'] <> '') and (conn.ColStringByName['ResourceType'] <> 'StructureDefinition') and (conn.ColStringByName['ResourceType'] <> 'CodeSystem') and
        (conn.ColStringByName['ResourceType'] <> 'ValueSet') and (conn.ColStringByName['ResourceType'] <> 'ConceptMap') then
        FXIGServer.FResourceTypes.add(conn.ColStringByName['ResourceType']);
    conn.terminate;
    FXIGServer.FResourceTypes.Sort;

    conn.SQL := 'Select distinct Code from Categories where mode = 2';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
      if (conn.ColStringByName['Code'] <> '') then
        FXIGServer.FExtensionContexts.add(conn.ColStringByName['Code']);
    conn.terminate;
    FXIGServer.FExtensionContexts.Sort;

    conn.SQL := 'Select distinct Code from Categories where mode = 3';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
      if (conn.ColStringByName['Code'] <> '') then
        FXIGServer.FExtensionTypes.add(conn.ColStringByName['Code']);
    conn.terminate;
    FXIGServer.FExtensionTypes.Sort;

    conn.SQL := 'Select Code, Display from TxSource';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
      if (conn.ColStringByName['Code'] <> '') then
        FXIGServer.FTerminologySources.add(conn.ColStringByName['Code']+'='+conn.ColStringByName['Display']);
    conn.terminate;
    FXIGServer.FTerminologySources.Sort;


    FXIGServer.FVersions.Add('R2');
    FXIGServer.FVersions.Add('R2B');
    FXIGServer.FVersions.Add('R3');
    FXIGServer.FVersions.Add('R4');
    FXIGServer.FVersions.Add('R4B');
    FXIGServer.FVersions.Add('R5');
    FXIGServer.FVersions.Add('R6');

    FXIGServer.FTypes.add('rp=Resource Profiles');
    FXIGServer.FTypes.add('dp=Datatype Profiles');
    FXIGServer.FTypes.add('ext=Extensions');
    FXIGServer.FTypes.add('lm=Logical Models');
    FXIGServer.FTypes.add('cs=CodeSystems');
    FXIGServer.FTypes.add('vs=ValueSets');
    FXIGServer.FTypes.add('cm=ConceptMaps');

    conn.release;
  except
    on e : Exception do
      conn.Error(e);
  end;
end;

procedure TXIGServerEndPoint.Unload;
begin
end;

procedure TXIGServerEndPoint.InstallDatabase(params: TCommandLineParameters);
begin
end;

procedure TXIGServerEndPoint.UninstallDatabase;
begin
end;

procedure TXIGServerEndPoint.internalThread(callback: TFhirServerMaintenanceThreadTaskCallBack);
begin
  // nothing, for now
  // todo: health check on spider
end;

procedure TXIGServerEndPoint.LoadPackages(installer : boolean; plist: String);
begin
  raise EFslException.Create('This is not applicable to this endpoint');
end;

function TXIGServerEndPoint.makeWebEndPoint(common: TFHIRWebServerCommon): TFhirWebServerEndpoint;
var
  json : TJsonObject;
begin
  inherited makeWebEndPoint(common);
  FXIGServer := TFHIRXIGWebServer.Create(config.name, config['path'].value, common);
  WebEndPoint := FXIGServer;
  loadFromDB;
  FXIGServer.FDatabase := Database.link;
  result := FXIGServer.link;
end;

procedure TXIGServerEndPoint.SetCacheStatus(status: boolean);
begin
  inherited;
end;

function TXIGServerEndPoint.summary: String;
begin
  result := 'XIG Server based on database built ???'; // +dateBuilt();
end;

procedure TXIGServerEndPoint.updateAdminPassword(pw: String);
begin
  raise EFslException.Create('This is not applicable to this endpoint');
end;

procedure TXIGServerEndPoint.Load;
begin
  inherited Load;
end;

{ TFHIRXIGWebServer }

constructor TFHIRXIGWebServer.Create(code, path: String; common: TFHIRWebServerCommon);
begin
  inherited Create(code, path, common);

  FVersions := TStringList.create;
  FRealms := TStringList.create;
  FAuthorities := TStringList.create;
  FTypes := TStringList.create;
  FPackages := TFslMap<TPackageInformation>.create;
  FPackagesById := TFslMap<TPackageInformation>.create;
  FPackagesById.defaultValue := nil;
  FProfileResources := TStringList.create;
  FProfileTypes := TStringList.create;
  FResourceTypes := TStringList.create;
  FExtensionContexts := TStringList.create;
  FExtensionTypes := TStringList.create;
  FTerminologySources := TStringList.create;
end;

destructor TFHIRXIGWebServer.Destroy;
begin
  FPackagesById.free;
  FExtensionContexts.free;
  FExtensionTypes.free;
  FTerminologySources.free;
  FResourceTypes.free;
  FProfileResources.Free;
  FProfileTypes.Free;
  FPackages.Free;
  FDatabase.Free;
  FVersions.Free;
  FRealms.Free;
  FAuthorities.Free;
  FTypes.Free;
  inherited;
end;

function TFHIRXIGWebServer.description: String;
begin
  result := 'XIG Server based on database built ???';
end;

function TFHIRXIGWebServer.realmBar(secure: boolean; realm, auth, ver, rtype, rt, text : String) : String;
var
  p, s : String;
begin
  p := AbsoluteURL(secure)+'/?'; 
  if (rtype <> '') then
    p := p+'&type='+rtype;
  if (auth <> '') then
    p := p+'&auth='+auth;
  if (ver <> '') then
    p := p+'&ver='+ver;  
  if (rt <> '') then
      p := p + '&rt='+rt;
  if (text <> '') then
      p := p + '&text='+encodePercent(text);
  if realm = '' then
    result := '<b>All</b>'
  else
    result := '<a href="'+p+'">All</a>';
  for s in FRealms do
    if (s = realm) then
      result := result + ' | <b>'+s+'</b>'
    else
      result := result + ' | <a href="'+p+'&realm='+s+'">'+s+'</a>';
end;

function TFHIRXIGWebServer.authBar(secure: boolean; realm, auth, ver, rtype, rt, text  : String) : String;
var
  p, s : String;
begin                 
  p := AbsoluteURL(secure)+'/?';
  if (rtype <> '') then
    p := p+'&type='+rtype;
  if (realm <> '') then
    p := p+'&realm='+realm;
  if (ver <> '') then
    p := p+'&ver='+ver;  
  if (rt <> '') then
      p := p + '&rt='+rt;
  if (text <> '') then
      p := p + '&text='+encodePercent(text);
  if auth = '' then
    result := '<b>All</b>'
  else
    result := '<a href="'+p+'">All</a>';
  for s in FAuthorities do
    if (s = auth) then
      result := result + ' | <b>'+s+'</b>'
    else
      result := result + ' | <a href="'+p+'&auth='+s+'">'+s+'</a>';
end;

function TFHIRXIGWebServer.versionBar(secure: boolean; realm, auth, ver, rtype, rt, text  : String) : String;
var
  p, s : String;
begin   
  p := AbsoluteURL(secure)+'/?';
  if (rtype <> '') then
    p := p+'&type='+rtype;
  if (realm <> '') then
    p := p+'&realm='+realm;
  if (auth <> '') then
    p := p+'&auth='+auth;  
  if (rt <> '') then
      p := p + '&rt='+rt;
  if (text <> '') then
      p := p + '&text='+encodePercent(text);
  if ver = '' then
    result := '<b>All</b>'
  else
    result := '<a href="'+p+'">All</a>';
  for s in FVersions do
    if (s = ver) then
      result := result + ' | <b>'+s+'</b>'
    else
      result := result + ' | <a href="'+p+'&ver='+s+'">'+s+'</a>';
end;

function TFHIRXIGWebServer.typeBar(secure: boolean; realm, auth, ver, rtype, rt, text  : String) : String;
var
  p, s, n, v : String;
begin
  p := AbsoluteURL(secure)+'/?';
  if (ver <> '') then
    p := p+'&ver='+ver;
  if (realm <> '') then
    p := p+'&realm='+realm;
  if (auth <> '') then
    p := p+'&auth='+auth;
  if (rt <> '') then
      p := p + '&rt='+rt;    
  if (text <> '') then
      p := p + '&text='+encodePercent(text);
  if rtype = '' then
    result := '<b>All</b>'
  else
    result := '<a href="'+p+'">All</a>';
  for s in FTypes do
  begin
    StringSplit(s, '=', n, v);
    if (n = rtype) then
      result := result + ' | <b>'+v+'</b>'
    else
      result := result + ' | <a href="'+p+'&type='+n+'">'+v+'</a>';
  end;
end;

function showVersion(db : TFDBConnection) : String;
begin
  result := '';
  if (db.ColIntegerByName['R2'] = 1) then
    CommaAdd(result, 'R2');
  if (db.ColIntegerByName['R2B'] = 1) then
    CommaAdd(result, 'R2B');
  if (db.ColIntegerByName['R3'] = 1) then
    CommaAdd(result, 'R3');
  if (db.ColIntegerByName['R4'] = 1) then
    CommaAdd(result, 'R4');
  if (db.ColIntegerByName['R4B'] = 1) then
    CommaAdd(result, 'R4B');
  if (db.ColIntegerByName['R5'] = 1) then
    CommaAdd(result, 'R5');
  if (db.ColIntegerByName['R6'] = 1) then
    CommaAdd(result, 'R6');
end;

function TFHIRXIGWebServer.makeSelect(rt : String; list : TStringList) : String;
var
  b : TFslStringBuilder;
  s : String;
  procedure add(t : String);
  var
    n, v :String;
  begin
    if t.contains('=') then
      StringSplit(t, '=', n, v)
    else
    begin
      n := t;
      v := t;
    end;
    if (rt = n) then
      b.append('<option value="'+n+'" selected="true">'+v+'</option>')
    else
      b.append('<option value="'+n+'">'+v+'</option>')
  end;
begin
  b := TFslStringBuilder.create;
  try
    b.append('<select name="rt" size="1">');
    add('');

    for s in list do
      add(s);

    b.append('</select>');
    result := b.asString;
  finally
    b.free;
  end;
end;

function TFHIRXIGWebServer.contentAll(mode : TContentMode; secure: boolean; realm, auth, ver, rt, text, offset: String): String;
var
  db : TFDBConnection;
  b : TFslStringBuilder;
  filter, s, p, tt, id : String;
  count, c, offs : integer;
  pck : TPackageInformation;
begin
  p := AbsoluteURL(secure)+'/?';
  if (ver <> '') then
    p := p+'&ver='+ver;
  if (realm <> '') then
    p := p+'&realm='+realm;
  if (auth <> '') then
    p := p+'&auth='+auth;         
  if (text <> '') then
    p := p+'&text='+EncodePercent(text);
  if (rt <> '') then
    p := p+'&rt='+rt;
  case mode of
    cmCodeSystem: p := p+'&type=cs';
    cmResProfile: p := p+'&type=rp';
    cmDTProfile: p := p+'&type=dp';
    cmLogical: p := p+'&type=lm';
    cmExtensions : p := p + '&type=ext';
    cmValueSet : p := p + '&type=vs';
    cmConceptMap : p := p + '&type=cm';
  end;

  b := TFslStringBuilder.create;
  try
    case mode of
      cmCodeSystem : b.append('<h2>CodeSystems');
      cmResProfile : b.append('<h2>Resource Profiles');
      cmDTProfile : b.append('<h2>Datatype Profiles');
      cmLogical : b.append('<h2>Logical models');
      cmExtensions : b.append('<h2>Extensions');  
      cmValueSet :b.append('<h2>ValueSets');
      cmConceptMap :b.append('<h2>ConceptMaps');
    else if rt <> '' then
      b.append('<h2>Resources - '+rt)
    else
      b.append('<h2>Resources - All Kinds');
    end;

    if (realm <> '') then
      b.append(', Realm '+realm.ToUpper);
    if (auth <> '') then
      b.append(', Authority '+capitalise(auth));
    if ver <> '' then
      b.append(', Version '+ver);
    b.append('</h2>');
    filter := '';
    if (realm <> '') then
      filter := filter + ' and realm = '''+sqlWrapString(realm)+'''';
    if (auth <> '') then
      filter := filter + ' and authority = '''+sqlWrapString(auth)+'''';
    if ver = 'R2' then
      filter := filter + ' and R2 = 1'
    else if ver = 'R2B' then
      filter := filter + ' and R2B = 1'
    else if ver = 'R3' then
      filter := filter + ' and R3 = 1'
    else if ver = 'R4' then
      filter := filter + ' and R4 = 1'
    else if ver = 'R4B' then
      filter := filter + ' and R4B = 1'
    else if ver = 'R5' then
      filter := filter + ' and R5 = 1'
    else if ver = 'R6' then
      filter := filter + ' and R6 = 1';
    case mode of
      cmCodeSystem : filter := filter + ' and ResourceType = ''CodeSystem''';
      cmResProfile :
        begin
          filter := filter + ' and ResourceType = ''StructureDefinition'' and kind = ''resource''';
          if (rt <> '') and (FProfileResources.IndexOf(rt) > -1)  then
            filter := filter + ' and Type = '''+sqlWrapString(rt)+''''
        end;
      cmDTProfile :
        begin
          filter := filter + ' and ResourceType = ''StructureDefinition'' and (kind = ''complex-type'' or kind = ''primitive-type'')';
          if (rt <> '') and (FProfileTypes.IndexOf(rt) > -1) then
            filter := filter + ' and Type = '''+sqlWrapString(rt)+''''
        end;
      cmLogical: filter := filter + ' and ResourceType = ''StructureDefinition'' and kind = ''logical''';
      cmExtensions :
        begin
          filter := filter + ' and ResourceType = ''StructureDefinition'' and  (Type = ''Extension'')';
          if (rt <> '') and (FExtensionContexts.IndexOf(rt) > -1) then
            filter := filter + ' and ResourceKey  in (Select ResourceKey from  Categories where Mode = 2 and Code = '''+sqlWrapString(rt)+''')'
        end;
      cmValueSet:    
        begin
          filter := filter + ' and ResourceType = ''ValueSet''';
          if (rt <> '') and (hasTerminologySource(rt)) then
            filter := filter + ' and ResourceKey  in (Select ResourceKey from  Categories where Mode = 1 and Code = '''+sqlWrapString(rt)+''')'
        end;
      cmConceptMap:
        begin
          filter := filter + ' and ResourceType = ''ConceptMap''';
          if (rt <> '') and (hasTerminologySource(rt)) then
            filter := filter + ' and ResourceKey  in (Select ResourceKey from  Categories where Mode = 1 and Code = '''+sqlWrapString(rt)+''')'
        end;
    else
    if (rt <> '') and (FResourceTypes.IndexOf(rt) > -1)  then
      filter := filter + ' and ResourceType = '''+sqlWrapString(rt)+''''
    else
      ; // nothing
    end;

    if text <> '' then
      if mode = cmCodeSystem then
        filter := filter + ' and (ResourceKey in (select ResourceKey from ResourceFTS where Description match '''+SQLWrapString(text)+''' or Narrative match '''+SQLWrapString(text)+''') '+
            'or ResourceKey in (select ResourceKey from CodeSystemFTS where Display match '''+SQLWrapString(text)+''' or Definition match '''+SQLWrapString(text)+'''))'
      else
        filter := filter + ' and ResourceKey in (select ResourceKey from ResourceFTS where Description match '''+SQLWrapString(text)+''' or Narrative match '''+SQLWrapString(text)+''')';
    if filter <> '' then
      filter := 'where '+filter.substring(4);

    db := FDatabase.GetConnection('content-all');
    try
      b.append('<p>');
      count := db.CountSQL('select count(*) from Resources '+filter);
      b.append(inttostr(count));
      b.append(' resources</p>');
      b.append('<form method="GET" action="">');
      if (ver <> '') then
        b.append('<input type="hidden" name="ver" value="'+ver+'"/>');
      if (realm <> '') then
        b.append('<input type="hidden" name="realm" value="'+realm+'"/>');
      if (auth <> '') then
        b.append('<input type="hidden" name="auth" value="'+auth+'"/>');
      case mode of
        cmCodeSystem: b.append('<input type="hidden" name="type" value="cs"/>');
        cmResProfile:
          begin
            b.append('<input type="hidden" name="type" value="rp"/>'); 
            b.append('Type: '+makeSelect(rt, FProfileResources)+'<br/>');
          end;
        cmDTProfile:
          begin
            b.append('<input type="hidden" name="type" value="dp"/>');
            b.append('Type: '+makeSelect(rt, FProfileTypes)+'<br/>');
          end;
        cmLogical: b.append('<input type="hidden" name="type" value="lm"/>');   
        cmExtensions:
          begin
            b.append('<input type="hidden" name="type" value="ext"/>');
            b.append('Context: '+makeSelect(rt, FExtensionContexts)+'<br/>');
          end;
        cmValueSet:
          begin
            b.append('<input type="hidden" name="type" value="vs"/>');
            b.append('Source: '+makeSelect(rt, FTerminologySources)+'<br/>');
          end;
        cmConceptMap:
          begin
            b.append('<input type="hidden" name="type" value="cm"/>');
            b.append('Source: '+makeSelect(rt, FTerminologySources)+'<br/>');
          end;
      else
        b.append('Type: '+makeSelect(rt, FResourceTypes)+'<br/>');
      end;
      b.append('Text: <input type="text" name="text" value="'+FormatTextToHtml(text)+'"/>');
      b.append('<input type="submit" value="Search"/>');
      b.append('</form>');

      //if (text = '') then
      //begin
        if ver = '' then
        begin
          b.append('<p>By Version</p><ul style="columns: 4; -webkit-columns: 4; -moz-columns: 4">');
          for s in FVersions do
          begin
            if filter = '' then
              c := db.CountSQL('select count(*) from Resources where '+s+' = 1')
            else
              c := db.CountSQL('select count(*) from Resources '+filter+' and '+s+' = 1');
            b.append('<li>');
            b.append('<a href="'+p+'&ver='+s+'">'+s+'</a>: '+inttostr(c));
            b.append('</li>');
          end;
          b.append('</ul>');
        end;

        if auth = '' then
        begin
          db.sql := 'select Authority, count(*) from Resources '+filter+' group by Authority ';
          db.prepare;
          db.execute;
          b.append('<p>By Authority</p><ul style="columns: 4; -webkit-columns: 4; -moz-columns: 4">');
          while db.FetchNext do
          begin
            b.append('<li>');
            if (db.ColString[1] = '') then
              b.append('none: '+db.colString[2])
            else
              b.append('<a href="'+p+'&auth='+db.ColString[1]+'">'+db.ColString[1]+'</a>: '+db.colString[2]);
            b.append('</li>');
          end;
          db.terminate;
          b.append('</ul>');
        end;

        if realm = '' then
        begin
          db.sql := 'select realm, count(*) from Resources '+filter+' group by realm ';
          db.prepare;
          db.execute;
          b.append('<p>By realm</p><ul style="columns: 4; -webkit-columns: 4; -moz-columns: 4">');
          while db.FetchNext do
          begin
            b.append('<li>');
            if (db.ColString[1] = '') then
              b.append('none: '+db.colString[2])
            else
              b.append('<a href="'+p+'&realm='+db.ColString[1]+'">'+db.ColString[1]+'</a>: '+db.colString[2]);
            b.append('</li>');
          end;
          db.terminate;
          b.append('</ul>');
        end;
      //end;

      offs := StrToIntDef(offset, 0);
      if (count > 200) then
      begin
        b.append('<p>');
        if (offs > 200) then
          b.append('<a href="'+p+'&offset=0">Start</a> ');
        if (offs >= 200) then
          b.append('<a href="'+p+'&offset='+inttostr(offs-200)+'">Prev</a> ');
        b.append('<b>Rows '+inttostr(offs)+' - '+inttostr(offs+200)+'</b>');
        b.append(' <a href="'+p+'&offset='+inttostr(offs+200)+'">Next</a> ');
      end;

      if (count > 0) then
      begin
        b.append('<table class="grid">');
        b.append('<tr>');

        b.append('<th>Package</th>');
        if (ver = '') then
          b.append('<th>Version</th>');
        b.append('<th>Identity</th>');
        b.append('<th>Name/Title</th>');
        b.append('<th>Status</th>');
        if (realm = '') then
          b.append('<th>Realm</th>');
        if (auth = '') then
          b.append('<th>Auth</th>');
        case mode of
          cmCodeSystem: b.append('<th>Content</th>');
          cmResProfile: if (rt = '') then b.append('<th>Resource</th>');
          cmDTProfile: if (rt = '') then b.append('<th>DataType</th>');
          cmExtensions: b.append('<th>Context</th><th>Modifier</th><th>Type</th>');
          cmValueSet : b.append('<th>Source(s)</th>');
          cmConceptMap : b.append('<th>Source(s)</th>');
          cmLogical: b.append('<th>Type</th>');
        end;
        case mode of
          cmCodeSystem: tt := 'CodeSystem/';
          cmResProfile: tt := 'StructureDefinition/';
          cmDTProfile: tt := 'StructureDefinition/';
          cmExtensions: tt := 'StructureDefinition/';
          cmValueSet : tt := 'ValueSet/';
          cmConceptMap : tt := 'ConceptMap/';
          cmLogical: tt := 'StructureDefinition/';
        else
          tt := '';
        end;

        b.append('</tr>');
        if (text <> '') then
          db.sql := 'Select PackageKey, ResourceType, Id, R2, R2B, R3, R4, R4B, R5, R6, Web, Url, Version, Status, Name, Title, Realm, Authority, Content, Supplements, Type, Details from Resources '+filter+' LIMIT '+inttostr(offs)+', '+inttostr(offs+200)+''
        else
          db.sql := 'Select PackageKey, ResourceType, Id, R2, R2B, R3, R4, R4B, R5, R6, Web, Url, Version, Status, Name, Title, Realm, Authority, Content, Supplements, Type, Details from Resources '+filter+' LIMIT '+inttostr(offs)+', '+inttostr(offs+200)+'';
        db.prepare;
        db.execute;
        while db.fetchnext do
        begin          
          b.append('<tr>');
          pck := FPackages[db.ColStringByName['PackageKey']];
          if (pck.web <> '') then
            b.append('<td><a href="'+pck.web+'">'+pck.id+'</a></td>')
          else
            b.append('<td>'+pck.id+'</td>');
          if (ver = '') then
            b.append('<td>'+showVersion(db)+'</td>');
          id := pck.vid+'/'+db.ColStringByName['ResourceType']+'/'+db.ColStringByName['Id'];
          if db.ColStringByName['Url'] <> '' then
            b.append('<td><a href="'+id+'">'+db.ColStringByName['Url'].replace(pck.canonical+tt, '')+extLink(db.ColStringByName['Web'])+'</a></td>')
          else
            b.append('<td><a href="'+id+'">'+(db.ColStringByName['ResourceType']+'/').replace(tt, '')+db.ColStringByName['Id']+extLink(db.ColStringByName['Web'])+'</a></td>');
          if db.ColStringByName['Title'] <> '' then
            b.append('<td>'+db.ColStringByName['Title']+'</td>')
          else
            b.append('<td>'+db.ColStringByName['Name']+'</td>');
          b.append('<td>'+db.ColStringByName['Status']+'</td>');
          if (realm = '') then
            b.append('<td>'+db.ColStringByName['Realm']+'</td>');
          if (auth = '') then
            b.append('<td>'+db.ColStringByName['Authority']+'</td>'); 
          case mode of
            cmCodeSystem: if db.ColStringByName['Supplements'] <> '' then
                b.append('<td>Suppl: '+db.ColStringByName['Supplements']+'</td>')
              else
                b.append('<td>'+db.ColStringByName['Content']+'</td>');
            cmResProfile : if (rt = '') then b.append('<td>'+db.ColStringByName['Type']+'</td>');
            cmDTProfile : if (rt = '') then b.append('<td>'+db.ColStringByName['Type']+'</td>');
            cmValueSet : b.append('<td>'+db.ColStringByName['Details'].replace(',', ' ')+'</td>');
            cmConceptMap : b.append('<td>'+db.ColStringByName['Details'].replace(',', ' ')+'</td>');
            cmLogical : b.append('<td>'+db.ColStringByName['Type'].replace(pck.canonical+'StructureDefinition/', '')+'</td>');
            cmExtensions : renderExtension(b, db.ColStringByName['Details']);
          end;
          b.append('</tr>');
        end;  
        b.append('</table>');
      end;
      db.terminate;
      db.release;
      result := b.AsString;
    except
      on e : Exception do
      begin
        db.Error(e);
        result := '<p style="color: Maroon">Error: '+FormatTextToHTML(e.message)+'</p>';
      end;
    end;
  finally
    b.free;
  end;
end;

procedure TFHIRXIGWebServer.renderExtension(b : TFslStringBuilder; details : String);
var
  p1, p2 : TStringArray;
begin
  p1 := details.Split(['|']);
  b.append('<td>'+p1[0].subString(9).replace(',', ' ')+'</td>');
  if p1[2] = 'Mod:1' then
    b.append('<td><b>M</b></td>')
  else
    b.append('<td></td>');
  if (p1[1].Contains('Meta') and p1[1].Contains('uuid')) then
    b.append('<td><b>(all)</b></td>')
  else
    b.append('<td>'+p1[1].subString(5).replace(',', ' ')+'</td>');
end;

function TFHIRXIGWebServer.hasTerminologySource(s: String): boolean;
var
  t : String;
begin
  result := false;
  for t in FTerminologySources do
    if (t.startsWith(s+'=')) then
      exit(true);
end;

function TFHIRXIGWebServer.extLink(url: String): String;
begin
  if (url = '') then
    result := ''
  else
    result := ' <a href="'+FormatTextToHtml(url)+'"><img src="http://hl7.org/fhir/external.png"/></a>';
end;


procedure DecompressStream(src, dst: TStream);
var
  ds: TDecompressionStream;
  d: dword;
  buff: array[0..1023] of byte;
begin
  ds := TDecompressionStream.Create(src, true);
  try
    repeat
      d := ds.Read(buff, 1024);
      dst.Write(buff, d);
    until
      d = 0;
  finally
    ds.Free;
  end;
end;

function inflate(source:TBytes):TBytes;
var
  ss1, ss2: TStringStream;
begin
  ss1 := TStringStream.Create;
  try
    ss1.write(source[0], length(source));
    ss1.Position := 10; //SKIP GZIP HEADER

    ss2 := TStringStream.Create;
    try
      DecompressStream(ss1, ss2);
      ss2.Position := 0;
      setLength(result, ss2.Size);
      ss2.Read(result[0], length(result));
    finally
      ss2.Free;
    end;
  finally
    ss1.Free;
  end;
end;


function TFHIRXIGWebServer.contentRes(pid, rtype, id: String): String;
var
  db : TFDBConnection;
  b : TFslStringBuilder;
  pck : TPackageInformation;
  rk, s, dv, js: String;
  j : TBytes;
  json, txt : TJsonObject;
begin
  b := TFslStringBuilder.create;
  try
    db := FDatabase.GetConnection('content-res');
    try   
      pck := FPackagesById[pid];
      if (pck = nil) then
        raise exception.create('Unknown Package '+pid.replace('|', '#'));
       db.sql := 'Select * from Resources where PackageKey = '+pck.key+' and ResourceType = '''+SqlWrapString(rtype)+''' and Id = '''+SqlWrapString(id)+'''';
       db.prepare;
       db.Execute;
       if not db.FetchNext then
         raise exception.create('Unknown Resource '+rtype+'/'+id+' in package '+pid);
       rk := db.ColStringByName['ResourceKey'];
       b.append('<table class="grid">');
       b.append('<tr> <td>Package</td> <td><a href="'+pck.web+'">'+pck.id+'</a></td> </tr>');
       b.append('<tr> <td>Type</td> <td>'+rtype+'</td> </tr>');
       b.append('<tr> <td>Id</td> <td>Id</td> </tr>');
       s := showVersion(db);
       if (s.Contains(',')) then
         b.append('<tr> <td>FHIR Versions</td> <td>'+s+'</td> </tr>')
       else
         b.append('<tr> <td>FHIR Version</td> <td>'+s+'</td> </tr>');

       b.append('<tr> <td>Source</td> <td><a href="'+db.ColStringByName['Web']+'">'+db.ColStringByName['Web']+'</a></td> </tr>');
       if (db.ColStringByName['Url'] <> '') then
         b.append('<tr> <td>Url</td> <td>'+db.ColStringByName['Url']+'</td> </tr>');
       if (db.ColStringByName['Version'] <> '') then
         b.append('<tr> <td>Version</td> <td>'+db.ColStringByName['Version']+'</td> </tr>');
       if (db.ColStringByName['Status'] <> '') then
         b.append('<tr> <td>Status</td> <td>'+db.ColStringByName['Status']+'</td> </tr>');
       if (db.ColStringByName['Date'] <> '') then
         b.append('<tr> <td>Date</td> <td>'+db.ColStringByName['Date']+'</td> </tr>');
       if (db.ColStringByName['Name'] <> '') then
         b.append('<tr> <td>Name</td> <td>'+db.ColStringByName['Name']+'</td> </tr>');
       if (db.ColStringByName['Title'] <> '') then
         b.append('<tr> <td>Title</td> <td>'+db.ColStringByName['Title']+'</td> </tr>');
       if (db.ColStringByName['Experimental'] <> '') then
         if (db.ColStringByName['Experimental'] = '1') then
           b.append('<tr> <td>Experimental</td> <td>True</td> </tr>')
         else
           b.append('<tr> <td>Experimental</td> <td>False</td> </tr>');
       if (db.ColStringByName['Realm'] <> '') then
         b.append('<tr> <td>Realm</td> <td>'+db.ColStringByName['Realm']+'</td> </tr>');
       if (db.ColStringByName['Authority'] <> '') then
         b.append('<tr> <td>Authority</td> <td>'+db.ColStringByName['Authority']+'</td> </tr>');
       if (db.ColStringByName['Description'] <> '') then
         b.append('<tr> <td>Description</td> <td>'+db.ColStringByName['Description']+'</td> </tr>');  
       if (db.ColStringByName['Purpose'] <> '') then
         b.append('<tr> <td>Purpose</td> <td>'+db.ColStringByName['Purpose']+'</td> </tr>');
       if (db.ColStringByName['Copyright'] <> '') then
         b.append('<tr> <td>Copyright</td> <td>'+db.ColStringByName['Copyright']+'</td> </tr>');
       if (db.ColStringByName['CopyrightLabel'] <> '') then
         b.append('<tr> <td>Copyright Label</td> <td>'+db.ColStringByName['CopyrightLabel']+'</td> </tr>');
       if (db.ColStringByName['Content'] <> '') then
         b.append('<tr> <td>Content</td> <td>'+db.ColStringByName['Content']+'</td> </tr>');
       if (db.ColStringByName['Type'] <> '') then
         b.append('<tr> <td>Type</td> <td>'+db.ColStringByName['Type']+'</td> </tr>');
       if (db.ColStringByName['Supplements'] <> '') then
         b.append('<tr> <td>Supplements</td> <td>'+db.ColStringByName['Supplements']+'</td> </tr>');
       if (db.ColStringByName['valueSet'] <> '') then
         b.append('<tr> <td>valueSet</td> <td>'+db.ColStringByName['valueSet']+'</td> </tr>');
       if (db.ColStringByName['Kind'] <> '') then
         b.append('<tr> <td>Kind</td> <td>'+db.ColStringByName['Kind']+'</td> </tr>');
       //if (db.ColStringByName['Details'] <> '') then
       //  b.append('<tr> <td>Details</td> <td>'+db.ColStringByName['Details']+'</td> </tr>');
       b.append('</table>');
       db.terminate;

       db.SQL := 'Select * from Contents where ResourceKey = '+rk;
       db.prepare;
       db.Execute;
       db.fetchNext;
       j := inflate(db.ColBlobByName['Json']);  
       db.terminate;
       db.release;

       json := TJsonParser.Parse(j);
       try
         txt := json.forceObj['text'];
         dv := txt.str['div'];
         if (dv <> '') then
         begin
           b.append('<hr/>');
           b.append('<h3>Narrative</h3>');   
           b.append('<p style="color: maroon">Note: This is the original narrative; links are presented as is but will be broken</p>');
           b.append(dv);
         end;
         js := TJSONWriter.writeObjectStr(json, true);
       finally
         json.free;
       end;
       b.append('<hr/>');
       b.append('<h3>Source</h3>');
       b.append('<pre>');
       b.append(js);
       b.append('</pre>');

      result := b.AsString;
    except
      on e : Exception do
      begin
        db.Error(e);
        result := '<p style="color: Maroon">Error: '+FormatTextToHTML(e.message)+'</p>';
      end;
    end;
  finally
    b.free;
  end;
end;

procedure TFHIRXIGWebServer.sendViewHtml(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; secure : boolean; rtype, auth, realm, ver, rt, text, offset : String);
var
  vars : TFslMap<TFHIRObject>;
  ms : int64;
begin
  vars := TFslMap<TFHIRObject>.Create('vars');
  try
    vars.add('realm-bar', TFHIRObjectText.Create(realmBar(secure, realm, auth, ver, rtype, rt, text)));
    vars.add('auth-bar', TFHIRObjectText.Create(authBar(secure, realm, auth, ver, rtype, rt, text)));
    vars.add('version-bar', TFHIRObjectText.Create(versionBar(secure, realm, auth, ver, rtype, rt, text)));
    vars.add('type-bar', TFHIRObjectText.Create(typeBar(secure, realm, auth, ver, rtype, rt, text)));

    ms := getTickCount64;
    if (rtype = '') then
      vars.add('content', TFHIRObjectText.Create(contentAll(cmAll, secure, realm, auth, ver, rt, text, offset)))
    else if (rtype = 'cs') then
      vars.add('content', TFHIRObjectText.Create(contentAll(cmCodeSystem, secure, realm, auth, ver, '', text, offset)))
    else if (rtype = 'rp') then
      vars.add('content', TFHIRObjectText.Create(contentAll(cmResProfile, secure, realm, auth, ver, rt, text, offset)))
    else if (rtype = 'dp') then
      vars.add('content', TFHIRObjectText.Create(contentAll(cmDTProfile, secure, realm, auth, ver, rt, text, offset)))
    else if (rtype = 'lm') then
      vars.add('content', TFHIRObjectText.Create(contentAll(cmLogical, secure, realm, auth, ver, rt, text, offset)))
    else if (rtype = 'ext') then
      vars.add('content', TFHIRObjectText.Create(contentAll(cmExtensions, secure, realm, auth, ver, rt, text, offset)))
    else if (rtype = 'vs') then
      vars.add('content', TFHIRObjectText.Create(contentAll(cmValueset, secure, realm, auth, ver, rt, text, offset)))
    else if (rtype = 'cm') then
      vars.add('content', TFHIRObjectText.Create(contentAll(cmConceptMap, secure, realm, auth, ver, rt, text, offset)))
    else
      vars.add('content', TFHIRObjectText.Create('<p>Not done yet</p>'));
    vars.add('ms', TFHIRObjectText.Create(inttostr(getTickCount64 - ms)));

    //vars.add('matches', TFHIRObjectText.Create(renderJson(json, path, reg, srvr, ver)));
    //vars.add('count', TFHIRObjectText.Create(json.forceArr['results'].Count));
    //vars.add('registry', TFHIRObjectText.Create(reg));
    //vars.add('server', TFHIRObjectText.Create(srvr));
    //vars.add('version', TFHIRObjectText.Create(ver));
    //vars.add('url', TFHIRObjectText.Create(tx));
    //vars.add('status', TFHIRObjectText.Create(status));
    returnFile(request, response, nil, request.Document, 'xig.html', false, vars);
  finally
    vars.free;
  end;
end;

procedure TFHIRXIGWebServer.sendResourceHtml(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; secure : boolean; pid, rtype, id  : String);
var
  vars : TFslMap<TFHIRObject>;
  ms : int64;
begin
  vars := TFslMap<TFHIRObject>.Create('vars');
  try
    vars.add('pid', TFHIRObjectText.Create(pid.replace('|', '#')));
    vars.add('rtype', TFHIRObjectText.Create(rtype));
    vars.add('id', TFHIRObjectText.Create(id));

    ms := getTickCount64;
    vars.add('content', TFHIRObjectText.Create(contentRes(pid, rtype, id)));
    vars.add('ms', TFHIRObjectText.Create(inttostr(getTickCount64 - ms)));
    returnFile(request, response, nil, request.Document, 'xig-res.html', false, vars);
  finally
    vars.free;
  end;
end;

function TFHIRXIGWebServer.link: TFHIRXIGWebServer;
begin
  result := TFHIRXIGWebServer(inherited link);
end;

function TFHIRXIGWebServer.logId: string;
begin
  result := 'xig';
end;

function TFHIRXIGWebServer.PlainRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String; tt : TTimeTracker) : String;
begin
  countRequest;
  result := doRequest(AContext, request, response, id, false);
end;


function TFHIRXIGWebServer.doRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String; secure : boolean) : String;
var
  pm : THTTPParameters;
  rtype, auth, realm, ver, rt,text, offs, s : String;
  //s : TArray<String>;
  //sId : string;
  json : TJsonObject;
  p : TStringArray;
begin
  pm := THTTPParameters.Create(request.UnparsedParams);
  try
    if (request.CommandType <> hcGET) then
      raise EFslException.Create('The operation '+request.Command+' '+request.Document+' is not supported');
    if request.document = PathWithSlash then
    begin
      rtype  := pm.Value['type'];
      auth  := pm.Value['auth'];
      realm := pm.Value['realm'];
      ver   := pm.Value['ver'];
      offs   := pm.Value['offset'];
      text   := pm.Value['text'];
      rt   := pm.Value['rt'];
      result := 'XIG (type='+rtype+', auth='+auth+', realm='+realm+', ver='+ver+', offset='+offs+', rt='+rt+', text='+text+')';
      sendViewHtml(request, response, secure, rtype, auth, realm, ver, rt, text, offs);
    end
    else if request.Document.StartsWith(PathWithSlash) then
    begin
      p := request.Document.subString(PathWithSlash.length).split(['/']);
      if (length(p) <> 3) then
        raise EFslException.Create('The operation '+request.Command+' '+request.Document+' is not supported')
      else
      begin
        result := 'XIG Resource '+p[0]+'/'+p[1]+'/'+p[2];
        sendResourceHtml(request, response, secure, p[0], p[1], p[2]);
      end;
    end
    else
      raise EFslException.Create('The operation '+request.Command+' '+request.Document+' is not supported');
  finally
    pm.free;
  end;
end;

function TFHIRXIGWebServer.SecureRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo;  cert: TIdOpenSSLX509; id: String; tt : TTimeTracker): String;
begin
  countRequest;
  result := doRequest(AContext, request, response, id, true);
end;

end.
