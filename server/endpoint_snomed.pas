unit endpoint_snomed;

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
  SysUtils, Classes,
  IdContext, IdCustomHTTPServer, IdOpenSSLX509,
  fsl_base, fsl_utilities, fsl_threads, fsl_logging, fsl_json, fsl_http, fsl_npm, fsl_stream, fsl_htmlgen, fsl_i18n,
  fdb_manager,
  ftx_service, ftx_sct_services, ftx_sct_publisher, ftx_sct_analysis, ftx_sct_expressions,
  fhir_objects,
  server_config, utilities, server_constants,
  tx_manager, telnet_server, time_tracker, server_stats,
  web_base, endpoint;

type
  TSnomedWebServer = class (TFhirWebServerEndpoint)
  private
    FTx : TCommonTerminologies;
    function doRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String; secure: boolean): String;

    function chooseSnomedRelease() : String;
    function processSnomedForTool(ss : TSnomedServices; code : String) : String;
    procedure returnContent(request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; path: String; secure : boolean; title, content : String); overload;
  public
    destructor Destroy; override;
    function link : TSnomedWebServer; overload;
    function logId : string; override;

    function description : String; override;
    function PlainRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String; tt : TTimeTracker) : String; override;
    function SecureRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String; tt : TTimeTracker) : String; override;
  end;

  { TSnomedWebEndPoint }

  TSnomedWebEndPoint = class (TFHIRServerEndPoint)
  private
    FSnomedServer : TSnomedWebServer;
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
    function cacheSize(magic : integer) : UInt64; override;
    procedure clearCache; override; 
    procedure SweepCaches; override;
    procedure SetCacheStatus(status : boolean); override;
    procedure getCacheInfo(ci: TCacheInformation); override;  
    procedure recordStats(rec : TStatusRecord); override;
  end;


implementation

{ TSnomedWebEndPoint }

function TSnomedWebEndPoint.cacheSize(magic : integer): UInt64;
begin
  result := inherited cacheSize(magic);
end;

procedure TSnomedWebEndPoint.clearCache;
begin
  inherited;
end;

procedure TSnomedWebEndPoint.SweepCaches;
begin
  inherited SweepCaches;
end;

constructor TSnomedWebEndPoint.Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; common : TCommonTerminologies; i18n : TI18nSupport);
begin
  inherited Create(config, settings, nil, common, nil, i18n);
end;

destructor TSnomedWebEndPoint.Destroy;
begin
  inherited;
end;

procedure TSnomedWebEndPoint.getCacheInfo(ci: TCacheInformation);
begin
  inherited;
end;

procedure TSnomedWebEndPoint.recordStats(rec: TStatusRecord);
begin
  inherited recordStats(rec);
  // nothing
end;

function TSnomedWebEndPoint.makeWebEndPoint(common: TFHIRWebServerCommon): TFhirWebServerEndpoint;
begin
  inherited makeWebEndPoint(common);
  FSnomedServer := TSnomedWebServer.Create(config.name, config['path'].value, common);
  FSnomedServer.FTx := Terminologies.Link;
  WebEndPoint := FSnomedServer;
  result := FSnomedServer;
end;

procedure TSnomedWebEndPoint.SetCacheStatus(status: boolean);
begin
  inherited;
end;

function TSnomedWebEndPoint.summary: String;
begin
  result := 'Snomed CT Server'
end;

procedure TSnomedWebEndPoint.InstallDatabase;
begin
  raise EFslException.Create('This operation is not supported for this endpoint');
end;

procedure TSnomedWebEndPoint.Load;
begin
end;

procedure TSnomedWebEndPoint.LoadPackages(installer : boolean; plist: String);
begin
  raise EFslException.Create('This operation is not supported for this endpoint');
end;

procedure TSnomedWebEndPoint.UninstallDatabase;
begin
  raise EFslException.Create('This operation is not supported for this endpoint');
end;

procedure TSnomedWebEndPoint.Unload;
begin
  // nothing
end;

procedure TSnomedWebEndPoint.updateAdminPassword;
begin
  raise EFslException.Create('This operation is not supported for this endpoint');
end;

{ TSnomedWebServer }

function TSnomedWebServer.description: String;
begin
  result := 'Snomed browser';
end;

destructor TSnomedWebServer.Destroy;
begin
  FTx.free;
  inherited;
end;

function TSnomedWebServer.doRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String; secure: boolean): String;
var
  code : String;
  pub : TSnomedPublisher;
  html : THtmlPublisher;
  analysis : TSnomedAnalysis;
  parts : TArray<String>;
  ss, t : TSnomedServices;
  pm : THTTPParameters;
  buf : TFslNameBuffer;
  start : UInt64;
begin
  start := GetTickCount64;
  if request.Document.StartsWith(PathWithSlash+'tool/') then // FHIR build process support
  begin
    parts := request.Document.Split(['/']);
    ss := nil;
    for t in FTx.Snomed do
      if t.EditionId = parts[length(parts)-2] then
        ss := t;
    if ss = nil then
    begin
      response.ResponseNo := 404;
      response.ContentText := 'Document '+request.Document+' not found';
      result := 'miss: '+request.Document;
    end
    else
    begin
      ss.RecordUse;
      result := 'Snomed Tool: '+parts[length(parts)-1];
      response.ContentType := 'text/xml';
      try
        response.ContentText := processSnomedForTool(ss, parts[length(parts)-1]);
        response.ResponseNo := 200;
      except
        on e : Exception do
        begin
          response.ResponseNo := 500;
          response.ContentText := '<snomed version="'+FTx.DefSnomed.VersionDate+'" type="error" message="'+FormatTextToXml(e.Message, xmlAttribute)+'"/>';
        end;
      end;
    end;
  end
  else if request.Document.StartsWith(PathWithSlash+'analysis/')  then
  begin
    result := 'Snomed Analysis';
    FTx.DefSnomed.RecordUse;
    analysis := TSnomedAnalysis.Create(FTx.DefSnomed.Link);
    try
      pm := THTTPParameters.Create(request.UnparsedParams);
      try
        buf := analysis.generate(pm);
        try
          response.ContentType := buf.Name;
          response.ContentStream := TBytesStream.Create(buf.AsBytes);
          response.FreeContentStream := true;
        finally
          buf.free;
        end;
      finally
        pm.free;
      end;
      response.ResponseNo := 200;
    finally
      analysis.free;
    end;
  end
  else if (request.Document = PathWithSlash) then
  begin
    returnContent(request, response, request.Document, secure, 'SNOMED CT Editions', chooseSnomedRelease());
  end
  else if request.Document.StartsWith(PathWithSlash) then
  begin
    parts := request.Document.Split(['/']);
    ss := nil;
    for t in FTx.Snomed do
      if t.EditionId+'-'+t.VersionDate = parts[2] then
      begin
        ss := t;
        break;
      end;
    if ss = nil then
    begin
      returnContent(request, response, request.Document, secure, 'SNOMED CT Browser', 'Document '+request.Document+' not found');
      result := 'Snomed: miss '+request.Document;
    end
    else
    begin
      ss.RecordUse;
      code := request.UnparsedParams;
      result := 'Snomed Doco ('+ss.EditionName+'): '+code;

      try
        html := THtmlPublisher.Create;
        pub := TSnomedPublisher.Create(ss, AbsoluteURL(secure), start);
        try
          html.Version := SERVER_FULL_VERSION;
          html.BaseURL := PathWithSlash+ss.EditionId+'-'+ss.VersionDate+'/';
          html.LangList := THTTPLanguageList.Create(request.AcceptLanguage, true);
          pub.PublishDict(code, PathWithSlash+ss.EditionId+'-'+ss.VersionDate+'/', html);
          returnContent(request, response, request.Document, secure, 'SNOMED CT Browser', html.output);
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
  end
  else
  begin
    response.ResponseNo := 404;
    response.ContentText := 'Document '+request.Document+' not found';
    result := 'Snomed: miss: '+request.Document;
  end;
end;

function TSnomedWebServer.link: TSnomedWebServer;
begin
  result := TSnomedWebServer(inherited link);

end;

function TSnomedWebServer.logId: string;
begin
  result := 'SN';
end;

function TSnomedWebServer.PlainRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String; tt : TTimeTracker): String;
begin
  countRequest;
  result := doRequest(AContext, request, response, id, false);
end;

procedure TSnomedWebServer.returnContent(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; path: String; secure: boolean; title, content : String);
var
  vars :  TFslMap<TFHIRObject>;
begin
  vars := TFslMap<TFHIRObject>.Create;
  try
    vars.add('title', TFHIRObjectText.Create(title));
    vars.add('content', TFHIRObjectText.Create(content));
    returnFile(request, response, nil, path, 'template-nfhir.html', secure, vars);
  finally
    vars.free;
  end;
end;

function TSnomedWebServer.SecureRequest(AContext: TIdContext; ip : String; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert: TIdOpenSSLX509; id: String; tt : TTimeTracker): String;
begin
  countRequest;
  result := doRequest(AContext, request, response, id, true);
end;

function TSnomedWebServer.processSnomedForTool(ss : TSnomedServices; code : String) : String;
var
  sl : TConceptDesignations;
  cd : TConceptDesignation;
  id : UInt64;
  exp : TSnomedExpression;
  index : cardinal;
  s : string;
begin
  if StringIsInteger64(code) then
  begin
    if ss.ConceptExists(code, index) then
    begin
      result := '<snomed version="'+ss.VersionDate+'" type="concept" concept="'+code+
       '" display="'+FormatTextToXml(ss.GetDisplayName(code, ''), xmlAttribute)+
       '" active="'+booleanToString(ss.isActive(index))+'">';
      sl := TConceptDesignations.Create(nil, FTx.Languages.link);
      try
        ss.ListDisplayNames(sl, code, '', ALL_DISPLAY_NAMES);
        for cd in sl.designations do
          result := result + '<display value="'+FormatTextToXml(cd.value.asString, xmlAttribute)+'"/>';
      finally
        sl.free;
      end;
      result := result + '</snomed>';
    end
    else if ss.IsValidDescription(code, id, s) then
    begin
      result := '<snomed version="'+ss.VersionDate+'" type="description" description="'+code+'" concept="'+inttostr(id)+'" display="'+FormatTextToXml(s, xmlAttribute)+'">';
      sl := TConceptDesignations.Create(nil, FTx.Languages.link);
      try
        ss.ListDisplayNames(sl, inttostr(id), '', ALL_DISPLAY_NAMES);
        for cd in sl.designations do
          result := result + '<display value="'+FormatTextToXml(cd.value.asString, xmlAttribute)+'"/>';
      finally
        sl.free;
      end;
      result := result + '</snomed>';
    end
    else
      result := '<snomed version="'+ss.VersionDate+'" description="Snomed ID '+code+' not known"/>';
  end
  else
  begin
    exp := ss.parseExpression(code);
    try
      result := '<snomed version="'+ss.VersionDate+'" type="expression" expression="'+code+'" expressionMinimal="'+FormatTextToXml(ss.renderExpression(exp, sroMinimal), xmlAttribute)+'" expressionMax="'+
      FormatTextToXml(ss.renderExpression(exp, sroReplaceAll), xmlAttribute)+'" display="'+FormatTextToXml(ss.displayExpression(exp), xmlAttribute)+'" ok="true"/>';
    finally
      exp.free;
    end;
  end;
end;

function TSnomedWebServer.chooseSnomedRelease: String;
var
  html : THtmlPublisher;
  ss : TSnomedServices;
begin
  html := THtmlPublisher.Create;
  try
    html.Version := SERVER_FULL_VERSION;
    html.Heading(1, 'Choose SNOMED CT Version');
    html.StartTable(true);
    html.StartTableRow;
    html.AddTableCell('Choose SNOMED Edition');
    html.AddTableCell('Version');
    html.AddTableCell('Date');
    html.AddTableCell('UseCount');
    html.AddTableCell('Last Used');
    html.EndTableRow;
    for ss in FTx.Snomed do
    begin
      html.StartTableRow;
      html.AddTableCellURL(ss.EditionName, '/snomed/'+ss.editionId+'-'+ss.VersionDate);
      html.AddTableCell(ss.VersionUri);
      html.AddTableCell(ss.VersionDate);
      html.AddTableCell(inttostr(ss.UseCount));
      html.AddTableCell(ss.LastUseStatus);
      html.EndTableRow;
    end;
    html.EndTable;
    html.done;
    result := html.output;
  finally
    html.free;
  end;
end;



end.
