unit ServerLoginDialog;

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

{
Login Dialog.

This dialog prompts the user for 6 pieces of information:
 server:
   the FHIR end point of the server to connect to (e.g. https://test.fhir.org/r2)
   this must be an HTTPS endpoint that supports Smart on FHIR, and this client
   must be registered as an application on the server

 client id:
   the id assigned to the client by the srver when the application was registered

 redirect port:
   The port to listen on for redirects, so the client knows when the OAuth
   login process is completed. This must match the redirect URL when the
   application is registered. See notes further below on the redirect URL

 Web redirector:
   Whether to use a web redirection port during the OAuth redirection process.
   See notes below.

 patient id:
   The patient logical id to look up when the auth process is complete (instead
   of looking up the patient manually, which is not implemented here)

 In process:
   Whether to use an in process browser, or to spawn to a system browser for
   login.


Choosing whether to use an in process web browser
-------------------------------------------------

This login dialog offers the user 2 choices for a browser:
 - spawn to the system default browser and do the OAuth login there
 - run the OAuth browser process in process in a TWebBrowser control

As for a user experience, it's more natural for a user to use an in process
web browser - they choose to login, the server's OAuth process appears, and
then when it's finished, it goes away.

The problem with this is that the user has no assurance that the application
isn't doing something like key-logging the login, and stealing the user
credentials (assuming, that is, that the server does collect some credentials
from the user; mostly, that doesn't happen during the login process). Of course,
on windows, the system browser is susceptible to key logging etc, but there is
nominal security to protect that (not sure how reliable it is, but on enterprise
standard builds, it's probably pretty strong).

So enterprises might prefer an external browser, even though that's less
convenient for the user - they get left with a browser tab that has to be
closed, and then they have to alt-tab back to the application.


About the Redirect Port
-----------------------

At the completion of the OAuth process, the browser that is performing
the OAuth process is redirected to a nominated URL (it must be nominated
by the client at the beginning of the process, and it must match a
URL registered for that client - more than one must be registered).

The application has to catch that redirect. TWebBrowser exports no
internal even that be can be used to capture the redirect, so the options
are the same whether or not we are using a TWebBrowser or an external
browser. There are 2 options:
- put up a server on localhost that is only bind to the loopback adaptor
  and listen here for the redirect (this doesn't require admin privileges
  etc, since it's only bound to the internal TCP host, not external)

- register a custom URL scheme with an associated handler that uses
  some interprocess communication to let the application know that
  OAuth is complete. This is messy, *and it requires administrative access*
  to the OS.

For this reason, this application uses the first approach: it puts up a server
on the local port, and listens for the end of the process. That means that
if the port to listen on is 65000, then the redirect will be http://localhost:65000/done
(the /done bit is arbitrary on the part of the application). So the application is
registered with a redirect URL of http://localhost:[x]/done where x is the port
nominated in this dialog box.

However, we're not done yet; some EHRs don't allow a redirect to localhost :-(.
This is for security reasons, in that some other application can grab the port
and listen for the redirect (which is why this application puts up the server
first, so ensure no other application gets the port). But we can't always
register a local host redirect. We have, therefore, 2 other choices:

* redirect to http://local.[x.y] where x.y is some domain that has mapped
  local.x.y to 127.0.0.1. Health Intersections is one such domain - e.g.
  http://local.healthintersections.com.au always resolves to 127.0.0.1

* redirect to a web redirector that then redirects to http://localhost:[s]/done.
  One such redirect runs at http://www.healthintersections.com.au/redirect?port=x&path=y
  this redirects to http://localhost:[x]/[path]

Either of these works, though the second requires www.healthintersections.com.au
to be up. Of course, both of these things are trivial to set up by any domain.

}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, System.UITypes, IniFiles,
  fsl_utilities, fsl_shell, fsl_http,
  fhir_utilities, fhir_common, fhir_oauth, fui_vcl_smart, fhir_objects, fhir_client_http,
  fhir2_client, fhir2_types, fhir2_resources, fhir2_utilities, fhir2_common,
  ProgressDialog, FHIRDemoLogging;

type
  TFHIRAuthScope = (fasSystem,fasUser);
  TScopesStrArray = TArray<String>;

const
  CLIENT_SECRET = '? you have to fill this in after registration ?';


  KFHIRAuthScopeStr: array[TFHIRAuthScope] of UnicodeString = (
  'system/Patient.read system/MedicationOrder.read system/MedicationStatement.read system/AllergyIntolerance.read',
  'user/Patient.read user/MedicationOrder.read user/MedicationStatement.read user/AllergyIntolerance.read'
  );

  KFHIRAuthScopeAy: array[TFHIRAuthScope] of TScopesStrArray = (
  ['system/Patient.read','system/MedicationOrder.read','system/MedicationStatement.read','system/AllergyIntolerance.read'],
  ['user/Patient.read','user/MedicationOrder.read','user/MedicationStatement.read','user/AllergyIntolerance.read']
  );


type
  TServerLoginForm = class(TForm)
    Panel1: TPanel;
    btnLogin: TButton;
    btnCancel: TButton;
    Panel2: TPanel;
    Label2: TLabel;
    cbxServer: TComboBox;
    Label4: TLabel;
    Label6: TLabel;
    edtRedirectPort: TEdit;
    Label1: TLabel;
    edtPatientId: TEdit;
    chkInProgress: TCheckBox;
    Label3: TLabel;
    cbAuthScope: TComboBox;
    cbClientId: TComboBox;
    Label5: TLabel;
    edtHost: TEdit;
    procedure FormShow(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
  private
    FIni : TIniFile;
    FClient: TFhirClient2;
    FProgressForm : TProgressWindow;
    FLogService: TLoggingService;
    procedure SetClient(const Value: TFhirClient2);
    procedure DoOpenURL(url : UnicodeString);
    procedure DoIdle(out stop : boolean);
  public
    destructor Destroy; override;
    Property Ini : TIniFile read FIni write FIni;
    Property Client : TFhirClient2 read FClient write SetClient;
    property ProgressForm : TProgressWindow read FProgressForm write FProgressForm;
    property LogService : TLoggingService read FLogService write FLogService;
  end;

var
  ServerLoginForm: TServerLoginForm;

implementation

{$R *.dfm}



procedure TServerLoginForm.FormShow(Sender: TObject);
begin
  // these items will be fields in a record in wellsoft, stored in Ctrl-F10
  cbxServer.Text := ini.ReadString('Server', 'URL', 'https://test.fhir.org/r2');
  cbClientId.Text := ini.ReadString('Server', 'ClientId', '(get from server)');
  edtRedirectPort.Text := ini.ReadString('Server', 'Port', '32411');
  // this is the id in Cerner, obtained from... interface message? Not sure
  edtPatientId.Text := ini.ReadString('Server', 'Patient', 'example');
  // this determines whether or not a separate web browser window is opened, or a TWebBrowser window
  chkInProgress.Checked := ini.readBool('Server', 'InProcess', false);
  // this sets the combobox for Authorization Scope to the appropriate value
  cbAuthScope.Text := ini.ReadString('Server','AuthScope','User');
  edtHost.Text := ini.ReadString('Server','Host','localhost');
end;

destructor TServerLoginForm.Destroy;
begin
  FClient.Free;
  inherited;
end;


procedure TServerLoginForm.btnLoginClick(Sender: TObject);
var
  server : TRegisteredFHIRServer;
  conf : TFhirCapabilityStatementW;
  a, t, r : UnicodeString;
  login : TSmartAppLaunchLogin;
  form : TSmartOnFhirLoginForm;
begin
  if not isAbsoluteUrl(cbxServer.Text) then
    raise EFHIRException.create('Invalid Server Address');
  if not StringIsInteger16(edtRedirectPort.Text) then
    raise EFHIRException.create('Invalid Port Number');
  if not IsId(edtPatientId.Text) then
    raise EFHIRException.create('Invalid Patient Id');

  ini.WriteString('Server', 'URL', cbxServer.Text);
  ini.WriteString('Server', 'ClientId', cbClientId.Text);
  ini.WriteString('Server', 'Port', edtRedirectPort.Text);
  ini.WriteString('Server', 'Host', edtHost.Text);
  ini.WriteString('Server', 'Patient', edtPatientId.Text);
  ini.WriteBool('Server', 'InProcess', chkInProgress.Checked);
  ini.WriteString('Server','AuthScope',cbAuthScope.Text);

  FClient := TFhirClient2.Create(nil, THTTPLanguages.create('en'), TFHIRHTTPCommunicator.create(cbxServer.Text));
  FClient.Logger := TDemoHttpLogger.Create;
  server := TRegisteredFHIRServer.create;
  try
    server.fhirEndPoint := cbxServer.Text;
    server.format := ffJson;
    server.clientid := cbClientId.Text;
    if cbAuthScope.Text = 'User' then
    begin
      server.SmartAppLaunchMode := salmOAuthClient;
    end
    else
    begin
      server.SmartAppLaunchMode := salmBackendClient;
      server.clientsecret := CLIENT_SECRET;
    end;
    server.redirectport := StrToInt(edtRedirectPort.Text);
    server.thishost := edtHost.Text;

    FProgressForm.Message := 'Logging in';
    FProgressForm.Show;
    FProgressForm.Update;

    cursor := crHourGlass;
    try
      conf := TFHIRCapabilityStatement2.Create(FClient.conformance(false));
      try
        if usesSmartOnFHIR(conf, a, t, r) then
        begin
          server.authorizeEndpoint := a;
          server.tokenEndpoint := t;
        end
        else
          raise EFHIRException.create('This server does not support Smart on FHIR');
        if chkInProgress.Checked and (cbAuthScope.Text <> 'System') then
        begin
          form := TSmartOnFhirLoginForm.create(self);
          try
            form.logoPath := path([ExtractFilePath(paramstr(0)), ChangeFileExt(ExtractFileName(paramstr(0)), '.png')]);
            form.server := server.link;
            form.scopes := 'openid profile ' + KFHIRAuthScopeStr[TFHIRAuthScope(cbAuthScope.ItemIndex)]; // 'openid profile user/Patient.read user/MedicationOrder.read user/MedicationStatement.read user/AllergyIntolerance.read';
            form.handleError := true;
            form.ShowModal;
            if form.Token <> nil then
            begin
              client.smartToken := form.token.link;
              FLogService.openIdToken := form.token.idToken.link;
              FLogService.Server := server.link;
              FLogService.recordLogin;
              ModalResult := mrOk;
            end
            else
              FProgressForm.Close;
          finally
            form.free;
          end;
        end
        else
        begin
          login := TSmartAppLaunchLogin.create;
          try
            login.server := server.Link;
            if cbAuthScope.Text = 'System' then
              login.scopes := KFHIRAuthScopeAy[fasSystem] // ['openid', 'profile', 'user/Patient.read', 'user/MedicationOrder.read', 'user/MedicationStatement.read', 'user/AllergyIntolerance.read'];
            else
              login.scopes := KFHIRAuthScopeAy[fasUser]; // ['openid', 'profile', 'user/Patient.read', 'user/MedicationOrder.read', 'user/MedicationStatement.read', 'user/AllergyIntolerance.read'];
            login.name := 'VclDemo';
            login.version := '0.01';
            login.OnOpenURL := DoOpenURL;
            login.OnIdle := DoIdle;

            if not login.login then
            begin
              MessageDlg('Unable to login', mtError, [mbok], 0);
              FProgressForm.Close;
            end
            else
            begin
              client.smartToken := login.token.link;
              FLogService.openIdToken := login.token.idToken.link;
              FLogService.Server := server.Link;
              FLogService.recordLogin;
              ModalResult := mrOk;
            end;
          finally
            login.Free;
          end;
        end;
      finally
        conf.Free;
      end;
    finally
      cursor := crDefault;
    end;
  finally
    server.free;
  end;
end;

procedure TServerLoginForm.DoIdle(out stop : boolean);
begin
  Application.ProcessMessages;
  stop := (FProgressForm <> nil) and FProgressForm.Stopped;
end;

procedure TServerLoginForm.DoOpenURL(url: UnicodeString);
begin
  ExecuteOpen(url);
end;

procedure TServerLoginForm.SetClient(const Value: TFhirClient2);
begin
  FClient.Free;
  FClient := Value;
end;

end.
