unit endpoint_loinc;

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
  Sysutils, Classes,
  IdContext, IdCustomHTTPServer, IdOpenSSLX509,
  fsl_base, fsl_utilities, fsl_threads, fsl_logging, fsl_json, fsl_http, fsl_npm, fsl_stream, fsl_htmlgen, fsl_i18n,
  fdb_manager,
  ftx_loinc_services, ftx_loinc_publisher,
  fhir_objects,
  server_config, utilities, server_constants,
  tx_manager, telnet_server, time_tracker, server_stats,
  web_base, endpoint;

type
  TLoincWebServer = class (TFhirWebServerEndpoint)
  private
    FTx : TCommonTerminologies;
    function doRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String; secure: boolean): String;

    procedure returnContent(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; path: String; secure : boolean; title, content : String); overload;
  public
    destructor Destroy; override;
    function link : TLoincWebServer; overload;
    function description : String; override;

    function PlainRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String; tt : TTimeTracker) : String; override;
    function SecureRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String; tt : TTimeTracker) : String; override;
    function logId : string; override;
  end;

  { TLoincWebEndPoint }

  TLoincWebEndPoint = class (TFHIRServerEndPoint)
  private
    FLoincServer : TLoincWebServer;
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies; i18n : TI18nSupport);
    destructor Destroy; override;

    function summary : String; override;
    function makeWebEndPoint(common : TFHIRWebServerCommon) : TFhirWebServerEndpoint; override;
    procedure InstallDatabase; override;
    procedure UninstallDatabase; override;
    procedure LoadPackages(plist : String); override;
    procedure updateAdminPassword; override;
    procedure Load; override;
    Procedure Unload; override;
    function cacheSize(magic : integer) : UInt64; override;
    procedure clearCache; override;
    procedure SweepCaches; override;
    procedure SetCacheStatus(status : boolean); override;
    procedure getCacheInfo(ci: TCacheInformation); override;  
    procedure recordStats(var rec : TStatusRecord); override;
  end;

implementation

{ TLoincWebEndPoint }

function TLoincWebEndPoint.cacheSize(magic : integer): UInt64;
begin
  result := inherited cacheSize(magic);
end;

procedure TLoincWebEndPoint.clearCache;
begin
  inherited;
end;

procedure TLoincWebEndPoint.SweepCaches;
begin
  inherited SweepCaches;
end;

constructor TLoincWebEndPoint.Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies; i18n : TI18nSupport);
begin
  inherited create(config, settings, db, common, nil, i18n);
end;

destructor TLoincWebEndPoint.Destroy;
begin
  inherited;
end;

procedure TLoincWebEndPoint.getCacheInfo(ci: TCacheInformation);
begin
  inherited;
end;

procedure TLoincWebEndPoint.recordStats(var rec: TStatusRecord);
begin
  inherited recordStats(rec);
  // nothing
end;

function TLoincWebEndPoint.makeWebEndPoint(common: TFHIRWebServerCommon): TFhirWebServerEndpoint;
begin
  inherited makeWebEndPoint(common);
  FLoincServer := TLoincWebServer.Create(config.name, config['path'].value, common);
  FLoincServer.FTx := Terminologies.Link;
  WebEndPoint := FLoincServer;
  result := FLoincServer;
end;

procedure TLoincWebEndPoint.SetCacheStatus(status: boolean);
begin
  inherited;
end;

function TLoincWebEndPoint.summary: String;
begin
  result := 'Loinc Server';
end;

procedure TLoincWebEndPoint.InstallDatabase;
begin
  raise EFslException.Create('This operation is not supported for this endpoint');
end;

procedure TLoincWebEndPoint.Load;
begin
end;

procedure TLoincWebEndPoint.LoadPackages(plist: String);
begin
  raise EFslException.Create('This operation is not supported for this endpoint');
end;

procedure TLoincWebEndPoint.UninstallDatabase;
begin
  raise EFslException.Create('This operation is not supported for this endpoint');
end;

procedure TLoincWebEndPoint.Unload;
begin
  // nothing
end;

procedure TLoincWebEndPoint.updateAdminPassword;
begin
  raise EFslException.Create('This operation is not supported for this endpoint');
end;

{ TLoincWebServer }

function TLoincWebServer.description: String;
begin
  result := 'LOINC browser';
end;

destructor TLoincWebServer.Destroy;
begin
  FTx.Free;
  inherited;
end;

function TLoincWebServer.doRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String; secure: boolean): String;
var
  code, lang, country : String;
  pub : TLoincPublisher;
  html : THtmlPublisher;
  i : integer;
  st : TStringList;
begin
  FTx.Loinc.RecordUse;
  code := request.UnparsedParams;
  lang := request.Document.Substring(PathWithSlash.Length);
  result := 'Loinc doco '+request.UnparsedParams+' ('+request.Document.Substring(12)+')';
  if ((lang = '') and (code = '')) or ((lang <> '') and not FTX.Loinc.supportsLang(THTTPLanguages.create(lang))) then
  begin
    st := TStringList.create;
    try
      for i := 0 to FTX.Loinc.Lang.count - 1 do
      begin
        FTX.Loinc.Lang.GetEntry(i, lang, country);
        st.add(lang+'-'+country);
      end;
      st.sort;
      html := THtmlPublisher.Create();
      try
        html.Version := SERVER_FULL_VERSION;
        html.BaseURL := '/loinc/doco/';
        html.Lang := THTTPLanguages.create(lang);
        html.Heading(1, 'LOINC Languages');
        html.StartList();
        for i := 0 to st.count - 1 do
        begin
          html.StartListItem;
          html.URL(st[i], st[i]);
          html.EndListItem;
        end;
        html.EndList();
        returnContent(request, response, request.Document, secure, 'LOINC Langauges', html.output);
      finally
        html.free;
      end;
    finally
      st.free;
    end;
  end
  else
  begin
    result := 'Loinc Doco: '+code;
    try
      html := THtmlPublisher.Create();
      pub := TLoincPublisher.create(FTX.Loinc, AbsoluteURL(secure), THTTPLanguages.Create(lang));
      try
        html.Version := SERVER_FULL_VERSION;
        html.BaseURL := PathWithSlash+lang;
        html.Lang := THTTPLanguages.Create(Lang);
        pub.PublishDict(code, PathWithSlash+lang, html);
        returnContent(request, response, request.Document, secure, 'LOINC Content', html.output);
      finally
        html.free;
        pub.free;
      end;
    except
      on e:exception do
      begin
        response.ResponseNo := 500;
        response.ContentText := 'error:'+FormatTextToXml(e.Message, xmlText);
      end;
    end;
  end;
end;

function TLoincWebServer.link: TLoincWebServer;
begin
  result := TLoincWebServer(inherited link);

end;

function TLoincWebServer.logId: string;
begin
  result := 'LN';
end;

function TLoincWebServer.PlainRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String; tt : TTimeTracker): String;
begin
  countRequest;
  result := doRequest(AContext, request, response, id, false);
end;

procedure TLoincWebServer.returnContent(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; path: String; secure: boolean; title, content : String);
var
  vars :  TFslMap<TFHIRObject>;
begin
  vars := TFslMap<TFHIRObject>.create;
  try
    vars.add('title', TFHIRObjectText.Create(title));
    vars.add('content', TFHIRObjectText.Create(content));
    returnFile(request, response, nil, path, 'template-nfhir.html', secure, vars);
  finally
    vars.free;
  end;
end;

function TLoincWebServer.SecureRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert: TIdOpenSSLX509; id: String; tt : TTimeTracker): String;
begin
  countRequest;
  result := doRequest(AContext, request, response, id, true);
end;

end.
