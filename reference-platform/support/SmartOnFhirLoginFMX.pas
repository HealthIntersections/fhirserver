unit SmartOnFhirLoginFMX;

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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  FMX.StdCtrls, FMX.Controls.Presentation,
  IdContext, IdHTTPServer, IdCustomHTTPServer, IdSocketHandle,
  FileSupport, ParseMap, GuidSupport,
  FHIRResources, FHIRClient, SmartOnFHIRUtilities;

type
  TCheckScopesProc = reference to procedure (scopes : TArray<String>; var ok : boolean; var msg : String);

  TSmartOnFhirLoginForm = class(TForm)
    Panel1: TPanel;
    Button2: TButton;
    browser: TWebBrowser;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FLogoPath: String;
    FClient: TFHIRHTTPClient;
    FServer : TRegisteredFHIRServer;
    FScopes: TArray<String>;
    FHandleError: boolean;
    FToken: TSmartOnFhirAccessToken;
    FErrorMessage: String;
    FFinalState : string;
    FAuthCode : String;
    FInitialState : string;

    webserver : TIdHTTPServer;

    procedure SetClient(const Value: TFHIRHTTPClient);
    procedure SetToken(const Value: TSmartOnFhirAccessToken);
    procedure SetServer(const Value: TRegisteredFHIRServer);
//    procedure DoDone(var Msg: TMessage); message UMSG;
    procedure DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  public
    destructor Destroy; override;
    property logoPath : String read FLogoPath write FLogoPath;
    property client : TFHIRHTTPClient read FClient write SetClient;
    property scopes : TArray<String> read FScopes write FScopes;
    property handleError : boolean read FHandleError write FHandleError;
    property Server : TRegisteredFHIRServer read FServer write SetServer;

    // if modalResult = mrok, you'll get a token. otherwise, you'll get an error message
    property ErrorMessage : String read FErrorMessage write FErrorMessage;
    Property Token : TSmartOnFhirAccessToken read FToken write SetToken;
  end;

var
  SmartOnFhirLoginForm: TSmartOnFhirLoginForm;

function doSmartOnFHIRLogin(owner : TComponent; server : TRegisteredFHIRServer; client : TFHIRHTTPClient; cs : TFhirCapabilityStatement; authorize, token : String; scopes : TArray<String>; checkproc : TCheckScopesProc) : boolean;

implementation

{$R *.fmx}

function doSmartOnFHIRLogin(owner : TComponent; server : TRegisteredFHIRServer; client : TFHIRHTTPClient; cs : TFhirCapabilityStatement; authorize, token : String; scopes : TArray<String>; checkproc : TCheckScopesProc) : boolean;
var
  mr : integer;
begin
  SmartOnFhirLoginForm := TSmartOnFhirLoginForm.Create(owner);
  try
    SmartOnFhirLoginForm.logoPath := path([ExtractFilePath(paramstr(0)), 'toolkit.png']);
    SmartOnFhirLoginForm.client := client.Link;
    SmartOnFhirLoginForm.server := server.Link;
    SmartOnFhirLoginForm.scopes := scopes; // 'openid profile user/*.*';
    SmartOnFhirLoginForm.handleError := true;
    SmartOnFhirLoginForm.Caption := 'Login to '+client.address;
    mr := SmartOnFhirLoginForm.ShowModal;
    if mr = mrOK then
    begin
      client.SmartToken := SmartOnFhirLoginForm.Token.Link;
      result := true;
    end
    else if (mr = mrAbort) and (SmartOnFhirLoginForm.ErrorMessage <> '') then
      MessageDlg(SmartOnFhirLoginForm.ErrorMessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbNo], 0);
  finally
    SmartOnFhirLoginForm.Free;
  end;
end;

{ TSmartOnFhirLoginForm }

procedure TSmartOnFhirLoginForm.Button2Click(Sender: TObject);
begin
  ErrorMessage := 'User Cancelled';
end;

destructor TSmartOnFhirLoginForm.Destroy;
begin
  FServer.Free;
  FToken.free;
  FClient.Free;
  inherited;
end;

procedure TSmartOnFhirLoginForm.DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  s : TArray<String>;
  pm : TParseMap;
begin
  if ARequestInfo.Document = '/done' then
  begin
    s := ARequestInfo.RawHTTPCommand.Split([' ']);
    pm := TParseMap.create(s[1].Substring(6));
    try
      FAuthCode := pm.GetVar('code');
      FFinalState := pm.GetVar('state');
    finally
      pm.free;
    end;

    AResponseInfo.ResponseNo := 200;
    AResponseInfo.ResponseText := 'OK';
    AResponseInfo.ContentText := 'Checking Authorization, please wait...';
    //PostMessage(handle, UMSG, 0, 0);
  end
  else
  begin
    AResponseInfo.ResponseNo := 200;
    AResponseInfo.ResponseText := 'OK';
    AResponseInfo.ContentStream := TFileStream.Create(FLogoPath, fmOpenRead + fmShareDenyWrite);
    AResponseInfo.FreeContentStream := true;
  end;
end;

procedure TSmartOnFhirLoginForm.FormHide(Sender: TObject);
begin
  webserver.Active := false;
  webserver.Free;
end;

procedure TSmartOnFhirLoginForm.FormShow(Sender: TObject);
var
  url : String;
  SHandle: TIdSocketHandle;
  sl, s : String;
begin
  inherited;
  webserver := TIdHTTPServer.Create(nil);
  SHandle := webserver.Bindings.Add;
  SHandle.IP := '127.0.0.1';
  SHandle.Port := server.redirectPort;
  webserver.OnCommandGet := DoCommandGet;
  webserver.Active := true;

  FInitialState := NewGuidId;
  sl := 'openid profile';
  for s in scopes do
    sl := sl +' '+s;
  url := buildAuthUrl(server, sl, FInitialState);
  browser.Navigate(url);
end;

procedure TSmartOnFhirLoginForm.SetClient(const Value: TFHIRHTTPClient);
begin
  FClient.Free;
  FClient := Value;
end;

procedure TSmartOnFhirLoginForm.SetServer(const Value: TRegisteredFHIRServer);
begin
  FServer.Free;
  FServer := Value;
end;

procedure TSmartOnFhirLoginForm.SetToken(const Value: TSmartOnFhirAccessToken);
begin
  FToken.free;
  FToken := Value;
end;

end.
