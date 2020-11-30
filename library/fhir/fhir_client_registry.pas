unit fhir_client_registry;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_json, 
  fhir_objects, 
  fhir_oauth, fhir_cdshooks;

type
  TFHIRClientRegistry = class (TFslObject)
  private
    FFilename : String;
    FVersions : TFHIRVersionSet;
    copy :  TJsonObject;

    procedure RegisterKnownServers;
  protected
    json : TJsonObject;
    FShuttingDown: boolean;
    procedure initSettings; virtual;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(filename: String; versions : TFHIRVersionSet);
    destructor Destroy; override;
    procedure Save;

    procedure holdChanges;
    procedure CommitChanges;
    procedure AbandonChanges;
    property ShuttingDown : boolean read FShuttingDown write FShuttingDown;

    property FileName : String read FFilename;

    function default(purpose : String) : TRegisteredFHIRServer; // must be freed
    function defaultAddress(purpose : String) : String; // must be freed
    procedure ListServers(purpose : String; items : TFslList<TRegisteredFHIRServer>); overload;
    procedure ListServers(purpose : String; items : TStrings); overload;
    procedure registerServer(purpose : String; server : TRegisteredFHIRServer);
    procedure updateServerInfo(purpose : String; server : TRegisteredFHIRServer);
    function ServerCount(purpose : String) : integer;
    procedure DeleteServer(purpose : String; server : TRegisteredFHIRServer);
    procedure moveServerBefore(purpose : String; server, dest : TRegisteredFHIRServer);
    procedure moveServerAfter(purpose : String; server, dest : TRegisteredFHIRServer);
  end;

implementation

function srvname(purpose : String): String;
begin
  if purpose = '' then
    result := 'Servers'
  else
    result := 'Servers-'+purpose;
end;


{ TFHIRClientRegistry }

constructor TFHIRClientRegistry.Create(filename: String; versions : TFHIRVersionSet);
var
  f : TFileStream;
begin
  Inherited Create;
  Ffilename := filename;
  FVersions := versions;

  if not FileExists(filename) then
  begin
    json := TJsonObject.Create;
    initSettings;
  end
  else
  begin
    f := TFileStream.Create(filename, fmOpenRead + fmShareDenyWrite);
    try
      json := TJSONParser.Parse(f)
    finally
      f.Free;
    end;
  end;
end;

function TFHIRClientRegistry.default(purpose : String): TRegisteredFHIRServer;
var
  l : TFslList<TRegisteredFHIRServer>;
begin
  l := TFslList<TRegisteredFHIRServer>.create;
  try
    ListServers(purpose, l);
    if l.Count > 0 then
      result := l[0].Link
    else
      raise EFHIRException.create('No default server exists');
  finally
    l.Free;
  end;
end;

function TFHIRClientRegistry.defaultAddress(purpose : String): String;
var
  l : TFslList<TRegisteredFHIRServer>;
begin
  l := TFslList<TRegisteredFHIRServer>.create;
  try
    ListServers(purpose, l);
    if l.Count > 0 then
      result := l[0].fhirEndpoint
    else
      raise EFHIRException.create('No default server exists');
  finally
    l.Free;
  end;
end;

destructor TFHIRClientRegistry.Destroy;
begin
  json.Free;
  inherited;
end;

procedure TFHIRClientRegistry.AbandonChanges;
begin
  json.Free;
  json := copy;
  copy := nil;
end;

procedure TFHIRClientRegistry.CommitChanges;
begin
  copy.Free;
  copy := nil;
  Save;
end;

procedure TFHIRClientRegistry.DeleteServer(purpose : String; server : TRegisteredFHIRServer);
begin
  json.forceArr[srvname(purpose)].remove(server.id);
end;


procedure TFHIRClientRegistry.holdChanges;
var
  f : TFileStream;
begin
  f := TFileStream.Create(filename, fmOpenRead + fmShareDenyWrite);
  try
    copy := TJSONParser.Parse(f)
  finally
    f.Free;
  end;
end;

procedure TFHIRClientRegistry.initSettings;
begin
  RegisterKnownServers;
end;


procedure TFHIRClientRegistry.ListServers(purpose: String; items: TStrings);
var
  servers : TFslList<TRegisteredFHIRServer>;
  server : TRegisteredFHIRServer;
begin
  servers := TFslList<TRegisteredFHIRServer>.create();
  try
    ListServers(purpose, servers);
    for server in servers do
      items.Add(server.fhirEndpoint);
  finally
    servers.Free;
  end;
end;

procedure TFHIRClientRegistry.updateServerInfo(purpose : String; server: TRegisteredFHIRServer);
var
  arr : TJsonArray;
  o : TJsonObject;
  i : integer;
begin
  arr := json.arr[srvname(purpose)];

  for i := 0 to arr.Count - 1 do
    if i <> server.id then
      if (arr.Obj[i].str['Name'] = server.name) then
        raise EFHIRException.create('Duplicate Server Name '+server.name);

  o := arr.Obj[server.id];
  server.writeToJson(o);
  Save;
end;

procedure TFHIRClientRegistry.ListServers(purpose : String; items: TFslList<TRegisteredFHIRServer>);
var
  arr : TJsonArray;
  i : integer;
  srv : TRegisteredFHIRServer;
begin
  items.Clear;
  arr := json.forceArr[srvname(purpose)];
  for i := 0 to arr.Count - 1 do
  begin
    srv := TRegisteredFHIRServer.Create;
    try
      srv.readFromJson(arr.Obj[i]);
      if (srv.version in FVersions) then
        items.Add(srv.Link);
    finally
      srv.Free;
    end;
  end;
end;

procedure TFHIRClientRegistry.moveServerBefore(purpose : String; server, dest: TRegisteredFHIRServer);
var
  arr : TJsonArray;
  delta : integer;
begin
  arr := json.forceArr[srvname(purpose)];
  delta := server.id - dest.id;
  if delta <> 0 then
    arr.move(server.id, delta);
end;

procedure TFHIRClientRegistry.moveServerAfter(purpose : String; server, dest: TRegisteredFHIRServer);
var
  arr : TJsonArray;
  delta : integer;
begin
  arr := json.forceArr[srvname(purpose)];
  delta := server.id - (dest.id-1);
  arr.move(server.id, delta);
end;

procedure TFHIRClientRegistry.RegisterKnownServers;
var
  server : TRegisteredFHIRServer;
begin
  server := TRegisteredFHIRServer.Create;
  try
    server.name := 'Reference Server (R3)';
    server.fhirEndpoint := 'http://test.fhir.org/r3';
    server.version := fhirVersionRelease3;
//    server.addCdsHook('Get Terminology Information', TCDSHooks.codeView);
//    server.addCdsHook('Get Identifier Information', TCDSHooks.identifierView);
//    server.addCdsHook('Fetch Patient Alerts', TCDSHooks.patientView).preFetch.Add('Patient/{{Patient.id}}');
//    server.autoUseHooks := true;
    registerServer('', server);
    server.name := 'Reference Server (R4)';
    server.fhirEndpoint := 'http://test.fhir.org/r4';
    server.version := fhirVersionRelease4;
    registerServer('', server);
    server.name := 'Reference Server (R2)';
    server.fhirEndpoint := 'http://test.fhir.org/r2';
    server.version := fhirVersionRelease2;
    registerServer('', server);

    server.clear;
    server.name := 'Secure Reference Server';
    server.fhirEndpoint := 'https://test.fhir.org/r3';
    server.version := fhirVersionRelease3;
    server.SmartAppLaunchMode := salmOAuthClient;
    server.clientid := '458EA027-CDCC-4E89-B103-997965132D0C';
    server.redirectport := 23145;
    server.tokenEndpoint := 'https://authorize-dstu2.smarthealthit.org/token';
    server.authorizeEndpoint := 'https://authorize-dstu2.smarthealthit.org/authorize';
//    server.addCdsHook('Get Terminology Information', TCDSHooks.codeView);
//    server.addCdsHook('Get Identifier Information', TCDSHooks.identifierView);
//    server.addCdsHook('Fetch Patient Alerts', TCDSHooks.patientView).preFetch.Add('Patient/{{Patient.id}}');
    registerServer('', server);
    server.name := 'Secure Reference Server (R4)';
    server.fhirEndpoint := 'http://test.fhir.org/r4';
    server.version := fhirVersionRelease4;
    registerServer('', server);
    server.name := 'Secure Reference Server (R2)';
    server.fhirEndpoint := 'http://test.fhir.org/r2';
    server.version := fhirVersionRelease2;
    registerServer('', server);
  finally
    server.free;
  end;
end;

procedure TFHIRClientRegistry.registerServer(purpose : String; server : TRegisteredFHIRServer);
var
  arr : TJsonArray;
  i : integer;
  o : TJsonObject;
begin
  arr := json.forceArr[srvname(purpose)];
  for i := 0 to arr.Count - 1 do
    if (arr.Obj[i].str['Name'] = server.name) then
      raise EFHIRException.create('Duplicate Server Name '+server.name);
  o := arr.addObject;
  server.writeToJson(o);
  Save;
end;


procedure TFHIRClientRegistry.Save;
var
  f : TFileStream;
begin
  if copy <> nil then
    exit;

  f := TFileStream.Create(filename, fmCreate);
  try
    TJSONWriter.writeObject(f, json, true);
  finally
    f.Free;
  end;
end;

function TFHIRClientRegistry.ServerCount(purpose : String): integer;
var
  l : TFslList<TRegisteredFHIRServer>;
begin
  l := TFslList<TRegisteredFHIRServer>.create;
  try
    ListServers(purpose, l);
    result := l.Count;
  finally
    l.Free;
  end;
end;


function TFHIRClientRegistry.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FFilename.length * sizeof(char)) + 12);
  inc(result, copy.sizeInBytes);
  inc(result, json.sizeInBytes);
end;

end.

