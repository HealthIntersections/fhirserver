unit endpoint_folder;

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

{
This server exposes snomed and loinc browsers
}
interface

uses
  Sysutils, Classes, {$IFDEF DELPHI}IOUtils, {$ENDIF}
  IdContext, IdCustomHTTPServer, IdOpenSSLX509,
  fsl_base, fsl_utilities, fsl_threads, fsl_logging, fsl_json, fsl_http, fsl_npm, fsl_stream, fsl_htmlgen, fsl_fpc,
  fdb_manager,
  fhir_objects,
  server_config, utilities, server_constants,
  tx_manager, telnet_server, time_tracker,
  web_base, endpoint;

type
  TFolderWebServer = class (TFhirWebServerEndpoint)
  private
    FFolder : String;
    procedure serveFolder(request, path : String; response: TIdHTTPResponseInfo);
    procedure serveFile(path : String; response: TIdHTTPResponseInfo);

    function doRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String; secure: boolean): String;
  public
    destructor Destroy; override;
    function link : TFolderWebServer; overload;
    function description : String; override;
    function logId : string; override;

    function PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String; tt : TTimeTracker) : String; override;
    function SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String; tt : TTimeTracker) : String; override;
  end;

  TFolderWebEndPoint = class (TFHIRServerEndPoint)
  private
    FFolderServer : TFolderWebServer;
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings);
    destructor Destroy; override;

    function summary : String; override;
    function makeWebEndPoint(common : TFHIRWebServerCommon) : TFhirWebServerEndpoint; override;
    function cacheSize(magic : integer) : UInt64; override;
    procedure getCacheInfo(ci: TCacheInformation); override;
  end;

implementation

{ TFolderWebEndPoint }


constructor TFolderWebEndPoint.Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings);
begin
  inherited create(config, settings, nil, nil, nil);
end;

destructor TFolderWebEndPoint.Destroy;
begin
  FFolderServer.free;
  inherited;
end;

function TFolderWebEndPoint.cacheSize(magic : integer): UInt64;
begin
  result := inherited cacheSize(magic);
end;


procedure TFolderWebEndPoint.getCacheInfo(ci: TCacheInformation);
begin
  inherited;
end;

function TFolderWebEndPoint.makeWebEndPoint(common: TFHIRWebServerCommon): TFhirWebServerEndpoint;
begin
  FFolderServer := TFolderWebServer.Create(config.name, config['path'].value, common);
  FFolderServer.FFolder := config['folder'].value;
  WebEndPoint := FFolderServer;
  result := FFolderServer.link;
end;

function TFolderWebEndPoint.summary: String;
begin
  result := 'Folder Server';
end;

{ TFolderWebServer }

function TFolderWebServer.description: String;
begin
  result := 'Folder browser';
end;

destructor TFolderWebServer.Destroy;
begin
  inherited;
end;

function TFolderWebServer.doRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String; secure: boolean): String;
var
  p : String;
begin
  if not request.Document.StartsWith(PathNoSlash) then
    raise EWebException.Create('Illegal path');
  if request.Document.Contains('..') then
    raise EWebException.Create('Illegal path');
  p := FilePath([FFolder, request.Document.Substring(pathNoSlash.Length)]);
  if FolderExists(p) then
    serveFolder(request.Document, p, response)
  else if FileExists(p) then
    serveFile(p, response)
  else
  begin
    response.ResponseNo := 401;
    response.ContentText := request.Document+' not found on this server';
  end;
end;

function TFolderWebServer.link: TFolderWebServer;
begin
  result := TFolderWebServer(inherited link);
end;

function TFolderWebServer.logId: string;
begin
  result := 'FF';
end;

function TFolderWebServer.PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String; tt : TTimeTracker): String;
begin
  result := doRequest(AContext, request, response, id, false);
end;

function TFolderWebServer.SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert: TIdOpenSSLX509; id: String; tt : TTimeTracker): String;
begin
  result := doRequest(AContext, request, response, id, true);
end;

procedure TFolderWebServer.serveFile(path: String; response: TIdHTTPResponseInfo);
begin
  response.ContentType := GetMimeTypeForExt(ExtractFileExt(path));
  response.FreeContentStream := true;
  response.ContentStream := TFileStream.Create(path, fmOpenRead + fmShareDenyWrite);
end;

procedure TFolderWebServer.serveFolder(request, path: String; response: TIdHTTPResponseInfo);
var
  f, fl, s : String;
begin
  if not request.EndsWith('/') then
  begin
    response.Redirect(request+'/');
  end
  else
  begin
    s := '<pre>'+#13#10;
    for f in TDirectory.GetDirectories(path) do
    begin
      fl := ExtractFileName(f);
      s := s + '<a href="'+URLPath([request, fl])+'">'+fl+'</a>'+#13#10;
    end;
    for f in TDirectory.GetFiles(path) do
    begin
      fl := ExtractFileName(f);
      s := s + '<a href="'+URLPath([request, fl])+'">'+fl+'</a> '+DescribeBytes(FileSize(f))+#13#10;
    end;
    s := s + '</pre>'+#13#10;
    response.ContentText := s;
  end;
end;

end.
