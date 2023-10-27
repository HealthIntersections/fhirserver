unit FHIR.Client.ClientDialogFMX;

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
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Edit,
  IdHttp, IdSSLOpenSSL,
  fsl_json,
  FHIR.Support.Certs,
  
  fhir_oauth, FMX.Objects, FMX.ListBox;

type
  TRegisterClientForm = class(TForm)
    Panel1: TPanel;
    btnOk: TButton;
    Button2: TButton;
    Label1: TLabel;
    edtName: TEdit;
    Label2: TLabel;
    edtUrl: TEdit;
    Label3: TLabel;
    edtLogo: TEdit;
    edtPublicKey: TEdit;
    Label4: TLabel;
    btnPublicKey: TButton;
    Label5: TLabel;
    memRedirects: TMemo;
    Label6: TLabel;
    odPublicKey: TOpenDialog;
    edtIssuer: TEdit;
    Label7: TLabel;
    Line1: TLine;
    Label8: TLabel;
    Label9: TLabel;
    Line2: TLine;
    Line3: TLine;
    Label10: TLabel;
    edtAuth: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    cbxConfidential: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure edtNameChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnPublicKeyClick(Sender: TObject);
  private
    FMode: TSmartAppLaunchMode;
    FPort: integer;
    FServer: String;
    FSoftwareVersion: String;
    FSoftwareId: String;
    FClientId : String;
    FClientSecret : String;
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

{$R *.fmx}

procedure TRegisterClientForm.btnOkClick(Sender: TObject);
var
  http: TIdHTTP;
  ssl : TIdSSLIOHandlerSocketOpenSSL;
  post, resp : TBytesStream;
  json : TJSONObject;
  s : String;
begin
  try
    post := TBytesStream.Create(TEncoding.UTF8.getBytes(request));
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
          if edtAuth.text <> '' then
            http.Request.CustomHeaders.values['Authorization'] := edtAuth.text;
          resp := TBytesStream.Create;
          try
            try
              http.Post(FServer, post, resp);
              resp.position := 0;
              json := TJSONParser.Parse(resp);
              try
                FClientId := json.str['client_id'];
                FClientSecret := json.str['client_id'];
              finally
                json.free;
              end;
            except
              on e : EIdHTTPProtocolException do
                raise EFHIRException.Create(e.message+' : '+e.ErrorMessage);
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

procedure TRegisterClientForm.btnPublicKeyClick(Sender: TObject);
begin
  if odPublicKey.Execute then
    edtPublicKey.Text := odPublicKey.FileName;
end;

procedure TRegisterClientForm.edtNameChange(Sender: TObject);
begin
  if Mode = salmOAuthClient then
    btnOk.Enabled := (edtName.Text.Trim <> '') and (memRedirects.Text.Trim <> '')
  else
    btnOk.Enabled := (edtName.Text.Trim <> '') and FileExists(edtPublicKey.Text);
end;

procedure TRegisterClientForm.FormShow(Sender: TObject);
begin
  if Mode = salmOAuthClient then
  begin
    edtPublicKey.Enabled := false;
    memRedirects.Enabled := true;
    if Port <> 0 then
      memRedirects.Lines.Add('http://localhost:'+inttostr(Port)+'/done');
  end
  else
  begin
    edtPublicKey.Enabled := true;
    memRedirects.Enabled := false;
  end;
  btnPublicKey.Enabled := edtPublicKey.Enabled;
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
        arr.add(s);
      if cbxConfidential.ItemIndex = 0 then
      begin
        // confidential
        json.str['token_endpoint_auth_method'] := 'client_secret_basic';
        json.str['grant_types'] := 'authorization_code';
        json.str['response_types'] := 'code';
      end
      else
      begin
        // public
        json.str['token_endpoint_auth_method'] := 'none';
        json.str['grant_types'] := 'authorization_code';
        json.str['response_types'] := 'code';
      end;
    end
    else
    begin
      jwks := TJWKList.Create();
      try
        jwks.add(TJWTUtils.loadKeyFromRSACert(edtPublicKey.Text));
        jwks.writeToJson(json.forceObj['jwks']);
      finally
        jwks.free;
      end;
      json.str['token_endpoint_auth_method'] := 'private_key_jwt';
      json.str['issuer'] := edtIssuer.Text;
      json.str['grant_types'] := 'client_credentials';
      json.str['response_types'] := 'token';
    end;
    result := TJSONWriter.writeObjectStr(json, true)
  finally
    json.free;
  end;
end;

end.
