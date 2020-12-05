unit frm_edit_changes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls,
  Process,
  fsl_diff, fsl_utilities,
  ftk_context;

type

  { TEditChangeReviewForm }

  TEditChangeReviewForm = class(TForm)
    btnCancel: TButton;
    btnExternalDiff: TButton;
    lblSummary: TLabel;
    mBase: TMemo;
    mCurr: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlLeftSummary: TPanel;
    pnlRightSummary: TPanel;
    pnlTextLeft: TPanel;
    pnlTextRight: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure btnExternalDiffClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabSheet1Resize(Sender: TObject);
  private
    FDiffTool: String;
    FEditor: TToolkitEditor;
    baseBytes, currBytes : TBytes;
    procedure SetEditor(AValue: TToolkitEditor);
    procedure buildTextDiff;
  public
    property editor : TToolkitEditor read FEditor write SetEditor;
    property DiffTool : String read FDiffTool write FDiffTool;
  end;

var
  EditChangeReviewForm: TEditChangeReviewForm;

implementation

{$R *.lfm}

{ TEditChangeReviewForm }

procedure TEditChangeReviewForm.FormDestroy(Sender: TObject);
begin
  FEditor.Free;
end;

procedure TEditChangeReviewForm.btnExternalDiffClick(Sender: TObject);
var
  baseFile, currFile : String;
  p : TProcess;
begin
  if editor.Session.Address.StartsWith('file:') then
    baseFile := editor.Session.Address.Substring(5)
  else
  begin
    baseFile := path(['[tmp]', 'base.'+editor.Store.MakeFilename(editor.session.Address)]);
    BytesToFile(baseBytes, baseFile);
  end;
  currFile := path(['[tmp]', 'current.'+ExtractFileName(baseFile)]);
  BytesToFile(currBytes, currFile);
  p := TProcess.create(nil);
  try
    p.Executable := DiffTool;
    p.Parameters.add(baseFile);
    p.Parameters.add(currFile);
    p.Options := [];
    p.Execute;
  finally
    p.free;
  end;

end;

procedure TEditChangeReviewForm.FormShow(Sender: TObject);
begin
  buildTextDiff;
  btnExternalDiff.enabled := fileExists(FDiffTool);
end;

procedure TEditChangeReviewForm.TabSheet1Resize(Sender: TObject);
begin
  pnlTextLeft.Width := TabSheet1.Width div 2;
end;

procedure TEditChangeReviewForm.SetEditor(AValue: TToolkitEditor);
begin
  FEditor.Free;
  FEditor:=AValue;
end;

procedure TEditChangeReviewForm.buildTextDiff;
var
  base, curr : TStringList;
  diff : TFslTextComparer;
begin
  baseBytes := editor.Store.load(editor.session.address).content;
  currBytes := editor.GetBytes;

  base := TStringList.create;
  curr := TStringList.create;
  diff := TFslTextComparer.create;
  try
    base.text := TEncoding.UTF8.GetAnsiString(baseBytes).trim+#13#10;
    curr.text := TEncoding.UTF8.GetAnsiString(currBytes).trim+#13#10;
    diff.IgnoreCase := false;
    diff.IgnoreSpaces := true;
    diff.Heuristic := 50;
    diff.Source1 := base;
    diff.Source2 := curr;
    diff.Output1 := mBase.lines;
    diff.Output2 := mCurr.lines;
    diff.Execute;

    lblSummary.caption := 'Comparison between Saved and Current version of '+editor.Session.Caption+': '+inttostr(diff.count)+' '+StringPlural('change', diff.count)+' found';
    pnlLeftSummary.caption := inttostr(base.count)+' lines';
    pnlRightSummary.caption := inttostr(curr.count)+' lines';
  finally
    diff.free;
    base.free;
    curr.free;
  end;
end;

end.

