unit ClientApplicationVerifier;

interface

uses
  SysUtils, Classes,
  IdHTTP, IdSSLOpenSSL,
  TextUtilities,
  AdvObjects,
  AdvJson, JWT,
  FHIRTypes, FHIRResources, FHIRUtilities;

type
  TClientApplicationVerifier = class (TAdvObject)
  private
    FServer: String;
    FCertificate: String;
    FPassword: String;
    procedure getSSLpassword(var Password: String);
    function performVerification(jwt : String) : TJsonObject;
  public
    function Link : TClientApplicationVerifier; overload;
    property Server : String read FServer write FServer;
    property Certificate : String read FCertificate write FCertificate;
    property Password : String read FPassword write FPassword;

    function check(jwt : TJWT; html : TStringBuilder; var summary : String) : boolean; overload;
    function check(jwt : TJWT; params : TFHIRParameters) : boolean; overload;
  end;

implementation

{ TClientApplicationVerifier }

function TClientApplicationVerifier.check(jwt : TJWT; html : TStringBuilder; var summary : String) : boolean;
var
  json, obj : TJsonObject;
  item : TJsonNode;
begin
  json := performVerification(jwt.originalSource);
  try
    result := json.str['status'] = 'approved';
    summary := json.str['message'];
    for item in json.forceArr['endorsements'] do
      if item is TJsonObject then
      begin
        obj := item as TJsonObject;
        html.Append('<li>');
        if obj.str['type'] = 'warning' then
          html.Append('<img src="warning.png"/>')
        else if obj.str['type'] = 'usage-note' then
          html.Append('<img src="usagenote.png"/>')
        else if obj.str['type'] = 'approval' then
          html.Append('<img src="approval.png"/>');
        html.Append(FormatTextToHTML(obj.str['comment']));
        if (obj.obj['endorser'].has('url')) then
          html.Append('<a href="'+obj.obj['endorser'].str['url']+'">');
        html.Append(FormatTextToHTML(obj.obj['endorser'].str['name']));
        if (obj.obj['endorser'].has('url')) then
          html.Append('</a>');
      end;
  finally
    json.Free;
  end;
end;

function TClientApplicationVerifier.check(jwt : TJWT; params : TFHIRParameters) : boolean;
var
  p, p1 : TFhirParametersParameter;
  json, obj : TJsonObject;
  item : TJsonNode;
begin
  json := performVerification(jwt.originalSource);
  try
    result := json.str['status'] = 'approved';
    p := params.parameterList.Append;
    p.name := 'message';
    p.value := TFHIRString.Create(json.str['message']);
    for item in json.forceArr['endorsements'] do
      if item is TJsonObject then
      begin
        obj := item as TJsonObject;
        p := params.parameterList.Append;
        p.name := 'endorsement';
        if (obj.has('type')) then
        begin
          p1 := p.partList.Append;
          p1.name := 'type';
          p1.value := TFHIRCode.Create(obj.str['type']);
        end;
        if (obj.has('comment')) then
        begin
          p1 := p.partList.Append;
          p1.name := 'comment';
          p1.value := TFHIRString.Create(obj.str['comment']);
        end;
        if (obj.has('endorser')) then
        begin
          if (obj.obj['endorser'].has('name')) then
          begin
            p1 := p.partList.Append;
            p1.name := 'endorser';
            p1.value := TFHIRString.Create(obj.obj['endorser'].str['name']);
          end;
          if (obj.obj['endorser'].has('url')) then
          begin
            p1 := p.partList.Append;
            p1.name := 'url';
            p1.value := TFHIRString.Create(obj.obj['endorser'].str['url']);
          end;
        end;
      end;
  finally
    json.Free;
  end;
end;


function TClientApplicationVerifier.Link: TClientApplicationVerifier;
begin
  result := TClientApplicationVerifier(inherited Link);
end;

function TClientApplicationVerifier.performVerification(jwt: String): TJsonObject;
var
  indy : TIdHTTP;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
  src, resp : TMemoryStream;
begin
  ssl := nil;
  indy := TIdHTTP.create(nil);
  try
    indy.HandleRedirects := true;
    if FServer.StartsWith('https:') then
    begin
      ssl := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
      indy.IOHandler := ssl;
      ssl.SSLOptions.Mode := sslmClient;
      ssl.SSLOptions.SSLVersions := [sslvTLSv1_2];
      if Certificate <> '' then
      begin
        ssl.SSLOptions.CertFile := Certificate;
        ssl.SSLOptions.KeyFile := ChangeFileExt(Certificate, '.key');
        ssl.OnGetPassword := getSSLpassword;
      end;
    end;
    indy.Request.ContentType := 'application/x-www-form-urlencoded';
    indy.Request.Accept := 'application/json';

    src := TMemoryStream.Create;
    resp := TMemoryStream.create;
    Try
      StringToStream('jwt='+jwt, src, TEncoding.ASCII);
      src.Position := 0;
      indy.Post(FServer, src, resp);
      if (indy.ResponseCode < 200) or (indy.ResponseCode >= 300) Then
        raise exception.create('unexpected condition');
      resp.Position := 0;
      result := TJSONParser.Parse(resp);
    Finally
      resp.free;
      src.Free;
    End;
  finally
    ssl.Free;
    indy.free;
  end;
end;

procedure TClientApplicationVerifier.getSSLpassword(var Password: String);
begin
  Password := FPassword;
end;


end.
