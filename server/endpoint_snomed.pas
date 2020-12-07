unit endpoint_snomed;

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  IdContext, IdCustomHTTPServer, IdOpenSSLX509,
  fsl_base, fsl_utilities, fsl_threads, fsl_logging, fsl_json, fsl_http, fsl_npm, fsl_stream, fsl_htmlgen,
  fdb_manager,
  ftx_sct_services, ftx_sct_publisher, ftx_sct_analysis, ftx_sct_expressions,
  fhir_objects,
  server_config, utilities, server_constants,
  tx_manager, telnet_server,
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

    function description : String; override;
    function PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id : String) : String; override;
    function SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert : TIdOpenSSLX509; id : String) : String; override;
  end;

  TSnomedWebEndPoint = class (TFHIRServerEndPoint)
  private
    FSnomedServer : TSnomedWebServer;
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; common : TCommonTerminologies);
    destructor Destroy; override;

    function summary : String; override;
    function makeWebEndPoint(common : TFHIRWebServerCommon) : TFhirWebServerEndpoint; override;
    procedure InstallDatabase; override;
    procedure UninstallDatabase; override;
    procedure LoadPackages(plist : String); override;
    procedure updateAdminPassword; override;
    procedure Load; override;
    Procedure Unload; override;
    procedure internalThread; override;
    function cacheSize : UInt64; override;
    procedure clearCache; override;
  end;


implementation

{ TSnomedWebEndPoint }

function TSnomedWebEndPoint.cacheSize: UInt64;
begin
  result := inherited cacheSize;
end;

procedure TSnomedWebEndPoint.clearCache;
begin
  inherited;
end;

constructor TSnomedWebEndPoint.Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; common : TCommonTerminologies);
begin
  inherited create(config, settings, nil, common);
end;

destructor TSnomedWebEndPoint.Destroy;
begin
  inherited;
end;

function TSnomedWebEndPoint.makeWebEndPoint(common: TFHIRWebServerCommon): TFhirWebServerEndpoint;
begin
  FSnomedServer := TSnomedWebServer.Create(config.name, config['path'].value, common);
  FSnomedServer.FTx := Terminologies.Link;
  WebEndPoint := FSnomedServer;
  result := FSnomedServer.link;
end;

function TSnomedWebEndPoint.summary: String;
begin
  result := 'Snomed CT Server'
end;

procedure TSnomedWebEndPoint.InstallDatabase;
begin
  raise Exception.Create('This operation is not supported for this endpoint');
end;

procedure TSnomedWebEndPoint.internalThread;
begin
  // nothing
end;

procedure TSnomedWebEndPoint.Load;
begin
end;

procedure TSnomedWebEndPoint.LoadPackages(plist: String);
begin
  raise Exception.Create('This operation is not supported for this endpoint');
end;

procedure TSnomedWebEndPoint.UninstallDatabase;
begin
  raise Exception.Create('This operation is not supported for this endpoint');
end;

procedure TSnomedWebEndPoint.Unload;
begin
  // nothing
end;

procedure TSnomedWebEndPoint.updateAdminPassword;
begin
  raise Exception.Create('This operation is not supported for this endpoint');
end;

{ TSnomedWebServer }

function TSnomedWebServer.description: String;
begin
  result := 'Snomed browser';
end;

destructor TSnomedWebServer.Destroy;
begin
  FTx.Free;
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
begin
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
      ss.checkLoaded;
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
    analysis := TSnomedAnalysis.create(FTx.DefSnomed.Link);
    try
      pm := THTTPParameters.create(request.UnparsedParams);
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
        pm.Free;
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
        ss := t;
    if ss = nil then
    begin
      returnContent(request, response, request.Document, secure, 'SNOMED CT Browser', 'Document '+request.Document+' not found');
      result := 'Snomed: miss '+request.Document;
    end
    else
    begin
      ss.RecordUse;
      ss.checkLoaded;
      code := request.UnparsedParams;
      result := 'Snomed Doco ('+ss.EditionName+'): '+code;

      try
        html := THtmlPublisher.Create();
        pub := TSnomedPublisher.create(ss, AbsoluteURL(secure));
        try
          html.Version := SERVER_FULL_VERSION;
          html.BaseURL := PathWithSlash+ss.EditionId+'-'+ss.VersionDate+'/';
          html.Lang := THTTPLanguages.Create(request.AcceptLanguage);
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

function TSnomedWebServer.PlainRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; id: String): String;
begin
  result := doRequest(AContext, request, response, id, false);
end;

procedure TSnomedWebServer.returnContent(request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; path: String; secure: boolean; title, content : String);
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

function TSnomedWebServer.SecureRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; cert: TIdOpenSSLX509; id: String): String;
begin
  result := doRequest(AContext, request, response, id, true);
end;

function TSnomedWebServer.processSnomedForTool(ss : TSnomedServices; code : String) : String;
var
  sl : TStringList;
  s : String;
  id : UInt64;
  exp : TSnomedExpression;
  index : cardinal;
begin
  if StringIsInteger64(code) then
  begin
    if ss.ConceptExists(code, index) then
    begin
      result := '<snomed version="'+ss.VersionDate+'" type="concept" concept="'+code+
       '" display="'+FormatTextToXml(ss.GetDisplayName(code, ''), xmlAttribute)+
       '" active="'+booleanToString(ss.isActive(index))+'">';
      sl := TStringList.Create;
      try
        ss.ListDisplayNames(sl, code, '', ALL_DISPLAY_NAMES);
        for s in sl do
          result := result + '<display value="'+FormatTextToXml(s, xmlAttribute)+'"/>';
      finally
        sl.free;
      end;
      result := result + '</snomed>';
    end
    else if ss.IsValidDescription(code, id, s) then
    begin
      result := '<snomed version="'+ss.VersionDate+'" type="description" description="'+code+'" concept="'+inttostr(id)+'" display="'+FormatTextToXml(s, xmlAttribute)+'">';
      sl := TStringList.Create;
      try
        ss.ListDisplayNames(sl, inttostr(id), '', ALL_DISPLAY_NAMES);
        for s in sl do
          result := result + '<display value="'+FormatTextToXml(s, xmlAttribute)+'"/>';
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
      exp.Free;
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
    html.AddTableCell('Load Status');
    html.EndTableRow;
    for ss in FTx.Snomed do
    begin
      html.StartTableRow;
      html.AddTableCellURL(ss.EditionName, '/snomed/'+ss.editionId+'-'+ss.VersionDate);
      html.AddTableCell(ss.VersionUri);
      html.AddTableCell(ss.VersionDate);
      html.AddTableCell(inttostr(ss.UseCount));
      html.AddTableCell(ss.LastUseStatus);
      html.AddTableCell(ss.LoadStatus);
      html.EndTableRow;
    end;
    html.EndTable;
    html.done;
    result := html.output;
  finally
    html.Free;
  end;
end;



end.
