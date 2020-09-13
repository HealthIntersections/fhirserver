unit FHIR.Client.ClientDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  IdHttp, IdSSLOpenSSL,
  FHIR.Support.Json, FHIR.Support.Certs,
  FHIR.Base.Lang,
  FHIR.Smart.Utilities;

type
  TRegisterClientForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edtName: TEdit;
    edtUrl: TEdit;
    edtLogo: TEdit;
    Bevel1: TBevel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    memRedirects: TMemo;
    Label7: TLabel;
    Panel1: TPanel;
    btnOk: TButton;
    Button2: TButton;
    cbxConfidential: TComboBox;
    procedure btnOkClick(Sender: TObject);
  private
    FMode: TSmartAppLaunchMode;
    FClientId: String;
    FClientSecret: String;
    FSoftwareVersion: String;
    FPort: integer;
    FServer: String;
    FSoftwareId: String;
    function request : string;
  public
    property Mode : TSmartAppLaunchMode read FMode write FMode;
    property Port : integer read FPort write FPort;
    property Server : String read FServer write FServer;
    property SoftwareId : String read FSoftwareId write FSoftwareId;
    property SoftwareVersion : String read FSoftwareVersion write FSoftwareVersion;
    property ClientId : String read FClientId write FClientId;
    property ClientSecret : String read FClientSecret write FClientSecret;
  end;

var
  RegisterClientForm: TRegisterClientForm;

implementation

{$R *.dfm}

procedure TRegisterClientForm.btnOkClick(Sender: TObject);
var
  http: TIdHTTP;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
  post, resp : TBytesStream;
  json : TJSONObject;
  s : String;
begin
  try
    post := TBytesStream.create(TEncoding.UTF8.getBytes(request));
    try
      http := TIdHTTP.Create(nil);
      Try
  //      if server.clientsecret <> '' then
  //      begin
  //        http.Request.BasicAuthentication := True;
  //        http.Request.Username := server.clientid;
  //        http.Request.Password := server.clientsecret;
  //      end;
        ssl := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
        Try
          http.IOHandler := ssl;
          ssl.SSLOptions.Mode := sslmClient;
          ssl.SSLOptions.Method := sslvTLSv1_2;
          http.Request.ContentType := 'application/json';
//          if edtAuth.text <> '' then
//            http.Request.CustomHeaders.values['Authorization'] := edtAuth.text;
          resp := TBytesStream.create;
          try
            try
              http.Post(FServer, post, resp);
              resp.position := 0;
              json := TJSONParser.Parse(resp);
              try
                FClientId := json.str['client_id'];
                FClientSecret := json.str['client_secret'];
              finally
                json.free;
              end;
            except
              on e : EIdHTTPProtocolException do
                raise EFHIRException.create(e.message+' : '+e.ErrorMessage);
              on e:Exception do
                raise;
            end;
          finally
            resp.free;
          end;
        finally
          ssl.free;
        end;
      finally
        http.free;
      end;
    finally
      post.free;
    end;
    ModalResult := mrOk;
  except
    on e : Exception do
      ShowMessage(e.message);
  end;

end;

function TRegisterClientForm.request: string;
var
  json : TJsonObject;
  arr : TJsonArray;
  s : String;
  jwks : TJWKList;
begin
  json := TJsonObject.Create;
  try
    json.str['client_name'] := edtName.text;
    if edtUrl.text <> '' then
      json.str['client_uri'] := edtUrl.text;
    if edtLogo.text <> '' then
      json.str['logo_uri'] := edtLogo.text;
    if FSoftwareId <> '' then
      json.str['software_id'] := FSoftwareId;
    if FSoftwareVersion <> '' then
      json.str['software_version'] := FSoftwareVersion;
    if Mode = salmOAuthClient then
    begin
      arr := json.forceArr['redirect_uris'];
      for s in memRedirects.Lines do
        if s <> '' then
          arr.add(s);
      if cbxConfidential.ItemIndex = 0 then
      begin
        // confidential
        json.str['token_endpoint_auth_method'] := 'client_secret_basic';
        json.forceArr['grant_types'].add('authorization_code');
        json.forceArr['response_types'].add('code');
      end
      else
      begin
        // public
        json.str['token_endpoint_auth_method'] := 'none';
        json.forceArr['grant_types'].add('authorization_code');
        json.forceArr['response_types'].add('code');
      end;
    end
    else
    begin
      jwks := TJWKList.create();
      try
//        jwks.add(TJWTUtils.loadKeyFromRSACert(edtPublicKey.Text));
        jwks.writeToJson(json.forceObj['jwks']);
      finally
        jwks.Free;
      end;
      json.str['token_endpoint_auth_method'] := 'private_key_jwt';
  //    json.str['issuer'] := edtIssuer.Text;
      json.forceArr['grant_types'].add('client_credentials');
      json.forceArr['response_types'].add('token');
    end;
    result := TJSONWriter.writeObjectStr(json, true)
  finally
    json.Free;
  end;
end;

end.
