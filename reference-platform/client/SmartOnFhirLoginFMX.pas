unit SmartOnFhirLoginFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  FMX.StdCtrls, FMX.Controls.Presentation,
  FileSupport,
  FHIRResources, FHIRClient, SmartonFHIRUtilities;

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
    FToken: TSmartOnFhirAccessToken;
    FErrorMessage: String;
    procedure SetClient(const Value: TFHIRHTTPClient);
    procedure SetToken(const Value: TSmartOnFhirAccessToken);
  public
    destructor Destroy; override;
    property logoPath : String read FLogoPath write FLogoPath;
    property client : TFHIRHTTPClient read FClient write SetClient;
    property scopes : TArray<String> read FScopes write FScopes;
    property handleError : boolean read FHandleError write FHandleError;

    // if modalResult = mrok, you'll get a token. otherwise, you'll get an error message
    property ErrorMessage : String read FErrorMessage write FErrorMessage;
    Property Token : TSmartOnFhirAccessToken read FToken write SetToken;
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

procedure TSmartOnFhirLoginForm.SetToken(const Value: TSmartOnFhirAccessToken);
begin
  FToken.free;
  FToken := Value;
end;

end.
