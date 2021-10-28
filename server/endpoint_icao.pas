unit endpoint_icao;

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
convert from icao certificate to smart on FHIR certificate
}
interface

uses
  Sysutils, Classes, {$IFDEF DELPHI}IOUtils, {$ENDIF}
  IdContext, IdCustomHTTPServer, IdOpenSSLX509,
  fsl_utilities, fsl_stream, fsl_crypto, fsl_http,
  fhir_objects, fhir_icao,
  fhir4_factory,
  utilities, server_config, time_tracker, storage,
  web_base, endpoint;

type
  TICAOWebServer = class (TFhirWebServerEndpoint)
  private
    FJWK : TJWK;
    FJWKSFile : String;
    function processSHC(card : THealthcareCard; accept : String; htmlLog : String; response: TIdHTTPResponseInfo): String;
    function processCard(stream : TStream; accept : String; response: TIdHTTPResponseInfo): String;
    function processQRCode(stream : TStream; accept : String; response: TIdHTTPResponseInfo): String;
    function render(card: THealthcareCard; log: String): String;
    function processError(message, accept, htmlLog: String;  response: TIdHTTPResponseInfo): String;
    function renderError(message, log: String): String;
    function renderStartPage(path: string): String;
    function extractFile(request: TIdHTTPRequestInfo): TStream;
  public
    destructor Destroy; override;
    function link : TICAOWebServer; overload;
    function description : String; override;
    function logId : string; override;

    function PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String; tt : TTimeTracker) : String; override;
    function SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String; tt : TTimeTracker) : String; override;
  end;

  TICAOWebEndPoint = class (TFHIRServerEndPoint)
  private
    FICAOServer : TICAOWebServer;
    FJWK : TJWK;
    FJWKSFile : String;
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings);
    destructor Destroy; override;

    function summary : String; override;
    function makeWebEndPoint(common : TFHIRWebServerCommon) : TFhirWebServerEndpoint; override;
    function cacheSize(magic : integer) : UInt64; override;
    procedure getCacheInfo(ci: TCacheInformation); override;
  end;

implementation

{ TICAOWebEndPoint }

constructor TICAOWebEndPoint.Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings);
begin
  inherited create(config, settings, nil, nil, nil);
  FJWK :=  TJWK.loadFromFile(Settings.Ini.web['card-key'].value);
  FJWKSFile := Settings.Ini.web['card-jwks'].value;
end;

destructor TICAOWebEndPoint.Destroy;
begin
  FJWK.Free;
  FICAOServer.free;
  inherited;
end;

function TICAOWebEndPoint.cacheSize(magic : integer): UInt64;
begin
  result := inherited cacheSize(magic);
end;

procedure TICAOWebEndPoint.getCacheInfo(ci: TCacheInformation);
begin
  inherited;
end;

function TICAOWebEndPoint.makeWebEndPoint(common: TFHIRWebServerCommon): TFhirWebServerEndpoint;
begin
  FICAOServer := TICAOWebServer.Create(config.name, config['path'].value, common);
  FICAOServer.FJWK := FJWK.link;
  FICAOServer.FJWKSFile := FJWKSFile;
  WebEndPoint := FICAOServer;
  result := FICAOServer.link;
end;

function TICAOWebEndPoint.summary: String;
begin
  result := 'ICAO -> SHC Server';
end;

{ TICAOWebServer }

function TICAOWebServer.description: String;
begin
  result := 'ICAO -> SHC Convertor';
end;

destructor TICAOWebServer.Destroy;
begin
  FJWK.free;
  inherited;
end;

function TICAOWebServer.render(card : THealthcareCard; log : String): String;
begin
  result :=
'<html>'+#13#10+
'  <head>'+#13#10+
'    <title>ICAO VDS Process</title>'+#13#10+
'  </head>'+#13#10+
'  <body>'+#13#10+
'    <h1>Outcome of Processing ICAO VDS</h1>'+#13#10+
'    <h2>Smart Health QR Code</h2>'+#13#10+
'    <p><img src="data:image/png;base64,'+EncodeBase64(card.image)+'"></p>'+#13#10+
'    <h2>Log Details</h2>'+#13#10+
'    <blockquote>'+#13#10+
  log+
'    </blockquote>'+#13#10+
'    <h2>QR Code Source</h2>'+#13#10+
'    <pre>'+#13#10+
card.qrSource(true)+
'    </pre>'+#13#10+
'  </body>'+#13#10+
'</html>'+#13#10;
end;

function TICAOWebServer.renderError(message, log : String): String;
begin
  result :=
'<html>'+#13#10+
'  <head>'+#13#10+
'    <title>ICAO VDS Process Error</title>'+#13#10+
'  </head>'+#13#10+
'  <body>'+#13#10+
'    <h1>Outcome of Processing ICAO VDS</h1>'+#13#10+
'    <p style="color: maroon">'+encodeXml(message)+'</p>'+#13#10+
'    <h2>Log Details</h2>'+#13#10+
'    <blockquote>'+#13#10+
  log+
'    </blockquote>'+#13#10+
'  </body>'+#13#10+
'</html>'+#13#10;
end;

function TICAOWebServer.renderStartPage(path : string): String;
begin
  result :=
'<html>'+#13#10+
'  <head>'+#13#10+
'    <title>ICAO VDS Processor</title>'+#13#10+
'  </head>'+#13#10+
'  <body>'+#13#10+
'    <h1>Convert an ICAO VDS to a Smart Health Card</h1>'+#13#10+
'    <p>Choose your VDS from an image file (png etc)</p>'+#13#10+
'    <form method="POST" enctype="multipart/form-data" action="'+path+'">'+#13#10+
'      <input type="file" id="myFile" name="filename"><hr>'+#13#10+
'      <input type="submit">'+#13#10+
'    </form>'+#13#10+
'    <p>Note: The server does not retain any information from the posted VDS, except for the card id.</p>'+#13#10+
'  </body>'+#13#10+
'</html>'+#13#10;
end;

function TICAOWebServer.processSHC(card : THealthcareCard; accept : String; htmlLog : String; response: TIdHTTPResponseInfo): String;
begin
  result := 'Convert ICAO card "'+card.id+'" to SHC';
  response.ResponseNo := 200;
  response.ResponseText := 'OK';
  if accept = 'image/png' then
  begin
    response.ContentType := 'image/png';
    response.ContentStream := TBytesStream.create(card.image);
    response.FreeContentStream := true;
  end
  else if accept = 'application/jwt' then
  begin
    response.ContentType := 'application/jwt';
    response.ContentText := card.jws;
  end
  else if accept.Contains('text/html') then
  begin
    response.ContentType := 'text/html';
    response.ContentText := render(card, htmlLog);
  end
  else
  begin
    response.ContentType := 'application/smart-health-card';
    response.ContentText := card.qrSource(true);
  end;
end;

function TICAOWebServer.processError(message, accept : String; htmlLog : String; response: TIdHTTPResponseInfo): String;
begin
  result := 'Convert ICAO card to SHC error: '+message;
  response.ResponseNo := 500;
  response.ResponseText := 'Exception';
  if accept.Contains('text/html') then
  begin
    response.ContentType := 'text/html';
    response.ContentText := renderError(message, htmlLog);
  end
  else
  begin
    response.ContentType := 'text/plain';
    response.ContentText := 'Error Processing ICAO VDS: '+message;
  end;
end;

function TICAOWebServer.processCard(stream: TStream; accept : String; response: TIdHTTPResponseInfo): String;
var
  conv : TICAOCardImporter;
  card : THealthcareCard;
begin
  conv := TICAOCardImporter.create;
  try
    conv.factory := TFHIRFactoryR4.create;
    conv.issuer := AbsoluteURL(true);
    conv.jwk := FJWK.link;
    try
      card := conv.import(StreamToString(stream, TEncoding.UTF8));
      try
        processSHC(card, accept, conv.htmlReport, response);
      finally
        card.free;
      end;
    except
      on e : Exception do
        result := processError(e.message, accept, conv.htmlReport, response);
    end;
  finally
    conv.free;
  end;
end;

function TICAOWebServer.processQRCode(stream: TStream; accept : String; response: TIdHTTPResponseInfo): String;
var
  conv : TICAOCardImporter;
  card : THealthcareCard;
begin
  conv := TICAOCardImporter.create;
  try
    conv.factory := TFHIRFactoryR4.create;
    conv.issuer := AbsoluteURL(true);
    conv.jwk := FJWK.link;
    try
      card := conv.import(StreamToBytes(stream));
      try
        processSHC(card, accept, conv.htmlReport, response);
      finally
        card.free;
      end;
    except
      on e : Exception do
        result := processError(e.message, accept, conv.htmlReport, response);
    end;
  finally
    conv.free;
  end;
end;

function TICAOWebServer.link: TICAOWebServer;
begin
  result := TICAOWebServer(inherited link);
end;

function TICAOWebServer.logId: string;
begin
  result := 'SC';
end;

function TICAOWebServer.PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String; tt : TTimeTracker): String;
var
  tgt : String;
begin
  tgt := URLPath([AbsoluteURL(true), request.document.Substring(PathWithSlash.Length)]);
  result := 'Redirect to '+tgt;
  response.redirect(tgt);
end;

function TICAOWebServer.extractFile(request: TIdHTTPRequestInfo) : TStream;
var
  form: TMimeMessage;
  mode: TOperationMode;
  sContentType : String;
begin
  form := loadMultipartForm(request.PostStream, request.contentType, mode);
  try
    result := extractFileData(defLang, form, 'file', sContentType);
  finally
    form.Free;
  end;
end;

function TICAOWebServer.SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert: TIdOpenSSLX509; id: String; tt : TTimeTracker): String;
var
  s : TStream;
begin
  if (request.Command = 'POST') and (request.ContentType = 'application/json') then
    result := processCard(request.PostStream, request.Accept, response)
  else if (request.Command = 'POST') and (request.ContentType.startsWith('image/')) then
    result := processQRCode(request.PostStream, request.Accept, response)
  else if (request.Command = 'POST') and request.ContentType.StartsWith('multipart/') then
  begin
    s := extractFile(request);
    try
      result := processQRCode(s, request.Accept, response)
    finally
       s.Free;
    end;
  end
  else if (request.Command = 'GET') and (request.document = URLPath([PathNoSlash, '.well-known/jwks.json'])) then
  begin
    result := 'Health card JKWS';
    response.ResponseNo := 200;
    response.ResponseText := 'OK';
    response.ContentStream := TFileStream.Create(FJWKSFile, fmOpenRead + fmShareDenyWrite);
    response.Expires := Now + 1;
    response.FreeContentStream := true;
    response.contentType := 'application/json';
  end
  else if (request.Command = 'GET') and (request.Document.Length > PathWithSlash.Length) and request.Accept.contains('text/html') then
  begin
    result := 'Start Page';
    response.ResponseNo := 200;
    response.ResponseText := 'OK';
    response.ContentText := renderStartPage(request.Document);
    response.Expires := Now + 1;
    response.FreeContentStream := true;
    response.contentType := 'text/html';
  end
  else
  begin
    response.ResponseNo := 404;
    response.ContentText := request.Document+' not found on this server';
    result := request.Command+' to '+request.Document;
  end;
end;

end.
