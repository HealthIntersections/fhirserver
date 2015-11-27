unit ValidationOutcomes;

// todo:
//   look ahead on search parameters on past values by name
//   better date entry

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls, Vcl.ExtCtrls, NppForms,
  SystemSupport,
  FHIRResources, FHIRUtilities, clipbrd, NppPlugin, Vcl.OleCtrls, SHDocVw, ActiveX, TextUtilities;

const
  MIN_COL_WIDTH = 260;
  SEARCH_PANEL_HEIGHT = 26;

type
  TValidationOutcomeForm = class(TNppForm)
    Panel2: TPanel;
    btnCancel: TButton;
    btnOpen: TButton;
    CheckBox1: TCheckBox;
    Panel1: TPanel;
    WebBrowser1: TWebBrowser;
    sd: TSaveDialog;
    procedure btnOpenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    html : String;
    procedure loadHtml(s :  String);
  public
    { Public declarations }
  end;

var
  ValidationOutcomeForm: TValidationOutcomeForm;

function ValidationSummary(owner : TNppPlugin; outcome : TFHIROperationOutcome) : boolean;
function ValidationError(owner : TNppPlugin; message : String) : boolean;

implementation

{$R *.dfm}

uses
  FHIRPluginSettings;

function ValidationSummary(owner : TNppPlugin; outcome : TFHIROperationOutcome) : boolean;
begin
  result := not Settings.NoValidationSummary;
  if result then
  begin
    ValidationOutcomeForm := TValidationOutcomeForm.create(owner);
    try
      ValidationOutcomeForm.loadHtml(outcome.narrativeAsWebPage);
      ValidationOutcomeForm.ShowModal;
    finally
      FreeAndNil(ValidationOutcomeForm);
    end;
  end;
end;

function ValidationError(owner : TNppPlugin; message : String) : boolean;
begin
  result := not Settings.NoValidationSummary;
  if result then
  begin
    ValidationOutcomeForm := TValidationOutcomeForm.create(owner);
    try
      ValidationOutcomeForm.loadHtml('<html><body>Error validating: '+message+'</body></html>');
      ValidationOutcomeForm.ShowModal;
    finally
      FreeAndNil(ValidationOutcomeForm);
    end;
  end;
end;


{ TValidationOutcomeForm }

procedure TValidationOutcomeForm.btnOpenClick(Sender: TObject);
begin
  if sd.Execute then
    StringToFile(html, sd.FileName, TEncoding.UTF8);
end;

procedure TValidationOutcomeForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  Settings.NoValidationSummary := CheckBox1.Checked;
end;

procedure TValidationOutcomeForm.loadHtml(s: String);
var
  fn : String;
begin
  fn := IncludeTrailingBackslash(SystemTemp)+'validation-outcomes-npp-fhir.html';
  StringToFile(s, fn, TEncoding.UTF8);
  WebBrowser1.Navigate('file://'+fn);
  html := s;
end;

end.
