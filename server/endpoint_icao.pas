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
  Sysutils, Classes, Graphics, {$IFDEF DELPHI}IOUtils, {$ENDIF}
  IdContext, IdCustomHTTPServer, IdOpenSSLX509,
  PdfiumCore,
  fsl_utilities, fsl_stream, fsl_crypto, fsl_http, fsl_threads,
  fhir_objects, fhir_icao,
  fhir4_factory,
  utilities, server_config, time_tracker, storage,
  web_base, endpoint, healthcard_generator;

type
  TICAOWebServer = class (TFhirWebServerEndpoint)
  private
    FJWK : TJWK;
    FJWKSFile : String;
    FStore : TX509CertificateStore;
    FPDFLock : TFslLock;
    function readPDF(stream : TStream) : TBitmap;
    function processGenerationForm(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo) : String;
    function processSHC(card : THealthcareCard; accept : String; title, action, htmlLog : String; response: TIdHTTPResponseInfo): String;
    function processCard(stream : TStream; accept : String; response: TIdHTTPResponseInfo): String;
    function processQRCode(stream : TStream; accept : String; response: TIdHTTPResponseInfo): String;
    function processPDF(stream : TStream; accept : String; response: TIdHTTPResponseInfo): String;
    function render(card: THealthcareCard; title, action, log: String): String;
    function processError(message, accept, htmlLog: String;  response: TIdHTTPResponseInfo): String;
    function renderError(message, log: String): String;
    function renderStartPage(path: string): String;
    function extractFile(request: TIdHTTPRequestInfo; var ct : String): TStream;
  public
    constructor Create(code, path : String; common : TFHIRWebServerCommon);
    destructor Destroy; override;
    function link : TICAOWebServer; overload;
    function description : String; override;
    function logId : string; override;

    function PlainRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String; tt : TTimeTracker) : String; override;
    function SecureRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String; tt : TTimeTracker) : String; override;
  end;

  TICAOWebEndPoint = class (TFHIRServerEndPoint)
  private
    FICAOServer : TICAOWebServer;
    FJWK : TJWK;
    FJWKSFile : String;
    FStore : TX509CertificateStore;
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
  FStore := TX509CertificateStore.create;
  if Settings.Ini.web['cert-store'].value <> '' then
    FStore.addFolder(Settings.Ini.web['cert-store'].value);
end;

destructor TICAOWebEndPoint.Destroy;
begin
  FJWK.Free;
  FICAOServer.free;
  FStore.Free;
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
  FICAOServer.FStore := FStore.Link;
  WebEndPoint := FICAOServer;
  result := FICAOServer.link;
end;

function TICAOWebEndPoint.summary: String;
begin
  result := 'ICAO -> SHC Server';
end;

{ TICAOWebServer }

constructor TICAOWebServer.Create(code, path : String; common : TFHIRWebServerCommon);
begin
  inherited;
  FPDFLock := TFslLock.create;
  PdfiumDllFileName := 'libpdf.dll';
end;

destructor TICAOWebServer.Destroy;
begin
  FJWK.free;
  FStore.free;
  FPDFLock.Free;
  inherited;
end;

function TICAOWebServer.description: String;
begin
  result := 'ICAO -> SHC Convertor';
end;

function TICAOWebServer.readPDF(stream : TStream): TBitmap;
var
  pdf : TPdfDocument;
  pg : TPdfPage;
  i, t : integer;
  obj : TPDFObject;
begin
  FPDFLock.Lock; // Pdfium is not thread safe
  try
    pdf := TPdfDocument.Create;
    try
      pdf.LoadFromStream(stream);
      t := 0;
      for i := 0 to pdf.PageCount - 1 do
      begin
        pg := pdf.Pages[i];
        for obj in pg.Objects do
        begin
          if obj.kind = potImage then
            exit(obj.AsBitmap);
          inc(t);
        end;
      end;
      raise EFHIRException.Create('No image found in PDF ('+inttostr(t)+' objects scanned');
    finally
      pdf.free;
    end;
  finally
    FPDFLock.Unlock;
  end;
end;

function TICAOWebServer.render(card : THealthcareCard; title, action, log : String): String;
begin
  result :=
'<html>'+#13#10+
'  <head>'+#13#10+
'    <title>'+title+'</title>'+#13#10+
'  </head>'+#13#10+
'  <body>'+#13#10+
'    <h1>Outcome of '+action+'</h1>'+#13#10+
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
'    <h2>Payload Source</h2>'+#13#10+
'    <pre>'+#13#10+
EncodeXML(card.payloadSource)+
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
'  <body style="font-family: Helvetica, Arial, Sans-Serif">'+#13#10+
'    <h1>Convert an ICAO VDS to a Smart Health Card</h1>'+#13#10+
'    <p style="font-family: Helvetica, Arial, Sans-Serif">Choose the PDF file that is your Australian International Covid Passport, or choose your VDS QR code directly from an image file (png etc).</p>'+#13#10+
'    <form method="POST" enctype="multipart/form-data" action="'+path+'">'+#13#10+
'      <input type="file" id="myFile" name="filename"><hr>'+#13#10+
'      <input type="submit">'+#13#10+
'    </form>'+#13#10+
'    <p>Notes</p>'+#13#10+
'    <ul>'+#13#10+
'     <li>The VDS QR code is a big one, and scanning with ZXing is hit and miss - you might have to try various resolutions</li>'+#13#10+
'     <li>The server does not retain any information from the posted VDS, except for the card id.</li>'+#13#10+
'    </ul>'+#13#10+
'    <hr/>'+#13#10+
'    <p>Alternatively, enter your details directly, and generate any card you want:</p>'+#13#10+
processFile(nil, 'shc_covid_vacc_form.shtml', 'shc_covid_vacc_form.shtml', true, nil)+#13#10+
'  </body>'+#13#10+
'</html>'+#13#10;
end;

function TICAOWebServer.processSHC(card : THealthcareCard; accept : String; title, action, htmlLog : String; response: TIdHTTPResponseInfo): String;
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
    response.ContentText := render(card, title, action, htmlLog);
  end
  else if accept.Contains('text/qrcode') then
  begin
    response.ContentType := 'text/qrcode';
    response.ContentText := card.qrSource(true);
  end
  else
  begin
    response.ContentType := 'application/smart-health-card';
    response.ContentText := '{"verifiableCredential": ["'+card.jws+'"]}';
  end
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


function TICAOWebServer.processGenerationForm(AContext: TIdContext; ip: String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo): String;
var
  vars : THTTPParameters;
  gen : THealthCardFormGenerator;
  card : THealthcareCard;
begin
  gen := THealthCardFormGenerator.create(FJWK.link);
  try
    gen.IssuerURL := AbsoluteURL(true);
    try
      vars := THTTPParameters.Create(request.UnparsedParams);
      try
        if not gen.checkInput(vars) then
          result := processError('Form Validation Failed', request.Accept, '<ul>'+gen.issues.Text+'</ul>', response)
        else
        begin
          card := gen.generateCard;
          try
            result := processSHC(card, request.Accept, 'SHC Generation', 'Processing User Form', gen.issues.Text, response);
          finally
            card.Free;
          end;
        end;
      finally
        vars.Free;
      end;
    except
      on e : Exception do
        result := processError(e.message, request.Accept, gen.issues.Text, response);
    end;
  finally
    gen.Free
  end;
end;

function TICAOWebServer.processPDF(stream: TStream; accept: String; response: TIdHTTPResponseInfo): String;
var
  bmp : TBitmap;
  conv : TICAOCardImporter;
  card : THealthcareCard;
begin
  conv := TICAOCardImporter.create;
  try
    conv.factory := TFHIRFactoryR4.create;
    conv.issuer := AbsoluteURL(true);
    conv.store := FStore.Link;
    conv.mustVerify := true;
    conv.jwk := FJWK.link;
    conv.log('Loading PDF');
    bmp := readPDF(stream);
    try
      try
        card := conv.import(bmp);
        try
          processSHC(card, accept, 'ICAO VDS Process', 'Processing ICAO VDS', conv.htmlReport, response);
        finally
          card.free;
        end;
      except
        on e : Exception do
          result := processError(e.message, accept, conv.htmlReport, response);
      end;
    finally
      bmp.Free;
    end;
  finally
    conv.free;
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
    conv.store := FStore.Link;
    conv.mustVerify := true;
    conv.jwk := FJWK.link;
    try
      card := conv.import(StreamToString(stream, TEncoding.UTF8));
      try
        processSHC(card, accept, 'ICAO VDS Process', 'Processing ICAO VDS', conv.htmlReport, response);
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
    conv.store := FStore.Link;
    conv.mustVerify := true;
    conv.jwk := FJWK.link;
    try
      card := conv.import(StreamToBytes(stream));
      try
        processSHC(card, accept, 'ICAO VDS Process', 'Processing ICAO VDS', conv.htmlReport, response);
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

function TICAOWebServer.PlainRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String; tt : TTimeTracker): String;
var
  tgt : String;
begin
  tgt := URLPath([AbsoluteURL(true), request.document.Substring(PathWithSlash.Length)]);
  result := 'Redirect to '+tgt;
  response.redirect(tgt);
end;

function TICAOWebServer.extractFile(request: TIdHTTPRequestInfo; var ct : String) : TStream;
var
  form: TMimeMessage;
  mode: TOperationMode;
begin
  form := loadMultipartForm(request.PostStream, request.contentType, mode);
  try
    result := extractFileData(defLang, form, 'file', ct);
  finally
    form.Free;
  end;
end;

function TICAOWebServer.SecureRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert: TIdOpenSSLX509; id: String; tt : TTimeTracker): String;
var
  s : TStream;
  ct : String;
begin
  if (request.Command = 'POST') and (request.ContentType = 'application/json') then
    result := processCard(request.PostStream, request.Accept, response)
  else if (request.Command = 'POST') and (request.ContentType.startsWith('image/')) then
    result := processQRCode(request.PostStream, request.Accept, response)
  else if (request.Command = 'POST') and (request.ContentType = 'application/pdf') then
    result := processPDF(request.PostStream, request.Accept, response)
  else if (request.Command = 'POST') and request.ContentType.StartsWith('multipart/') then
  begin
    s := extractFile(request, ct);
    try
      if ct = 'application/pdf' then
        result := processPDF(s, request.Accept, response)
      else
        result := processQRCode(s, request.Accept, response)
    finally
       s.Free;
    end;
  end
  else if (request.Command = 'POST') then
  begin
    result := ProcessGenerationForm(aContext, ip, request, response);
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
