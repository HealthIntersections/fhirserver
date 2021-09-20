unit frm_oauth;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  LclIntf, StdCtrls, ExtCtrls,

  fhir_client, fhir_oauth;

type

  { TOAuthForm }

  TOAuthForm = class(TForm)
    btnCancel: TButton;
    lblProgress: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Timer1: TTimer;
    procedure btnCancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FLogin : TSmartAppLaunchLogin;
    FWantStop : boolean;

    procedure DoOpenUrl(url : String);
    procedure DoIdle(out stop : boolean);
    procedure DoProgress(msg : String);

    function GetScopes: TArray<String>;
    function GetServer: TRegisteredFHIRServer;
    function GetToken: TClientAccessToken;
    procedure SetScopes(AValue: TArray<String>);
    procedure SetServer(AValue: TRegisteredFHIRServer);
    procedure SetToken(AValue: TClientAccessToken);
  public
    property server : TRegisteredFHIRServer read GetServer write SetServer;
    property scopes : TArray<String> read GetScopes write SetScopes;
    property token : TClientAccessToken read GetToken write SetToken;
  end;

var
  OAuthForm: TOAuthForm;

implementation

{$R *.lfm}

{ TOAuthForm }

procedure TOAuthForm.FormCreate(Sender: TObject);
begin
  FLogin := TSmartAppLaunchLogin.create;
  FLogin.name := 'FHIR Toolkit';
  FLogin.OnOpenURL := DoOpenUrl;
  FLogin.OnIdle := DoIdle;
  FLogin.OnProgress := DoProgress;
end;

procedure TOAuthForm.FormActivate(Sender: TObject);
begin
  Timer1.Enabled := true;
end;

procedure TOAuthForm.btnCancelClick(Sender: TObject);
begin
  FWantStop := true;
end;

procedure TOAuthForm.FormDestroy(Sender: TObject);
begin
  FLogin.Free;
end;

procedure TOAuthForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  FWantStop := false;
  if FLogin.login then
    ModalResult := mrOK
  else
    ModalResult := mrCancel;
end;

procedure TOAuthForm.DoOpenUrl(url: String);
begin
  OpenURL(url);
end;

procedure TOAuthForm.DoIdle(out stop: boolean);
begin
  Stop := FWantStop;
  Application.ProcessMessages;
end;

procedure TOAuthForm.DoProgress(msg: String);
begin
  lblProgress.Caption := msg;
end;

function TOAuthForm.GetScopes: TArray<String>;
begin
  result := FLogin.scopes;
end;

function TOAuthForm.GetServer: TRegisteredFHIRServer;
begin
  result := FLogin.server;
end;

function TOAuthForm.GetToken: TClientAccessToken;
begin
  result := FLogin.token;
end;

procedure TOAuthForm.SetScopes(AValue: TArray<String>);
begin
  FLogin.scopes := AValue;
end;

procedure TOAuthForm.SetServer(AValue: TRegisteredFHIRServer);
begin
  FLogin.server := AValue;
end;

procedure TOAuthForm.SetToken(AValue: TClientAccessToken);
begin
  FLogin.token := AValue;
end;


end.

