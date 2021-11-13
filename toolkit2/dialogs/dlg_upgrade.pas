unit dlg_upgrade;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, lclintf,
  HtmlView,
  MarkdownProcessor,
  fsl_utilities, fsl_fetcher,
  fui_lcl_utilities;

type

  { TToolkitUpgradeForm }

  TToolkitUpgradeForm = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    btnReset: TButton;
    html: THtmlViewer;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnOkClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMarkdown: String;
    FUrlExe: String;
    FUrlPage: String;
    procedure SetMarkdown(AValue: String);
  public
    property urlPage : String read FUrlPage write FUrlPage;
    property urlExe : String read FUrlExe write FUrlExe;
    property markdown : String read FMarkdown write SetMarkdown;
  end;

var
  ToolkitUpgradeForm: TToolkitUpgradeForm;

function showUpgradeInformation(owner : TComponent; urlPage, urlExe, md : String) : TModalResult;

implementation

{$R *.lfm}

function showUpgradeInformation(owner : TComponent; urlPage, urlExe, md : String) : TModalResult;
begin
  ToolkitUpgradeForm := TToolkitUpgradeForm.create(owner);
  try
    ToolkitUpgradeForm.urlPage := urlPage;
    ToolkitUpgradeForm.urlExe := urlExe;
    ToolkitUpgradeForm.markdown := md;
    result := ToolkitUpgradeForm.ShowModal;
  finally
    ToolkitUpgradeForm.Free;
  end;
end;

{ TToolkitUpgradeForm }

procedure TToolkitUpgradeForm.FormCreate(Sender: TObject);
begin
  setForOs(btnOk, btnCancel);
end;

procedure TToolkitUpgradeForm.btnResetClick(Sender: TObject);
begin
  openURL(FUrlPage);
end;

procedure TToolkitUpgradeForm.btnOkClick(Sender: TObject);
var
  fn : String;
begin
  fn := FilePath([DownloadsFolder, ExtractFileName(FUrlExe)]);
  try
    BytesToFile(TInternetFetcher.fetchUrl(FUrlExe), fn);
    {$IFDEF WINDOWS}
    if OpenDocument(fn) then
      ModalResult := mrOk
    else
      raise Exception.create('Unable to execute download '+fn);
    {$ELSE}
    raise Exception.create('Not implemented yet');
    {$ENDIF}
  except
    on e : Exception do
      MessageDlg('Download', 'Error: '+e.message, mtError, [mbok], 0);
  end;
end;

procedure TToolkitUpgradeForm.SetMarkdown(AValue: String);
var
  proc : TMarkdownProcessor;
begin
  FMarkdown := AValue;

  proc := TMarkdownProcessor.createDialect(mdCommonMark);
  try
    proc.unsafe := false;
    html.LoadFromString(proc.process(FMarkdown));
  finally
    proc.free;
  end;
end;

end.

