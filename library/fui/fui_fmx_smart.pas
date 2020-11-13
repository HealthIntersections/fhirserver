unit FHIR.Client.SmartLoginFMX;

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
  fsl_utilities,
  FHIR.Version.Resources, FHIR.Version.Client, FHIR.Client.SmartUtilities;

type
  TCheckScopesProc = reference to procedure (scopes : TArray<String>; var ok : boolean; var msg : String);

  TSmartOnFhirLoginForm = class(TForm)
    Panel1: TPanel;
    Button2: TButton;
    WebBrowser1: TWebBrowser;
  private
    FLogoPath: String;
    FClient: TFHIRHTTPClient;
    FScopes: TArray<String>;
    FHandleError: boolean;
    FToken: TClientAccessToken;
    FErrorMessage: String;
    procedure SetClient(const Value: TFHIRHTTPClient);
    procedure SetToken(const Value: TClientAccessToken);
  public
    destructor Destroy; override;
    property logoPath : String read FLogoPath write FLogoPath;
    property client : TFHIRHTTPClient read FClient write SetClient;
    property scopes : TArray<String> read FScopes write FScopes;
    property handleError : boolean read FHandleError write FHandleError;

    // if modalResult = mrok, you'll get a token. otherwise, you'll get an error message
    property ErrorMessage : String read FErrorMessage write FErrorMessage;
    Property Token : TClientAccessToken read FToken write SetToken;
  end;

var
  SmartOnFhirLoginForm: TSmartOnFhirLoginForm;

function doSmartOnFHIRLogin(owner : TComponent; client : TFHIRHTTPClient; cs : TFhirCapabilityStatement; authorize, token : String; scopes : TArray<String>; checkproc : TCheckScopesProc) : boolean;

implementation

{$R *.fmx}

function doSmartOnFHIRLogin(owner : TComponent; client : TFHIRHTTPClient; cs : TFhirCapabilityStatement; authorize, token : String; scopes : TArray<String>; checkproc : TCheckScopesProc) : boolean;
var
  mr : integer;
begin
  SmartOnFhirLoginForm := TSmartOnFhirLoginForm.Create(owner);
  try
    SmartOnFhirLoginForm.logoPath := path([ExtractFilePath(paramstr(0)), 'toolkit.png']);
    SmartOnFhirLoginForm.client := client.Link;
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

destructor TSmartOnFhirLoginForm.Destroy;
begin
  FToken.free;
  FClient.Free;
  inherited;
end;

procedure TSmartOnFhirLoginForm.SetClient(const Value: TFHIRHTTPClient);
begin
  FClient.Free;
  FClient := Value;
end;

procedure TSmartOnFhirLoginForm.SetToken(const Value: TClientAccessToken);
begin
  FToken.free;
  FToken := Value;
end;

end.
