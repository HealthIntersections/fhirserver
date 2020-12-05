unit frm_edit_changes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls,
  fsl_diff,
  ftk_context;

type

  { TEditChangeReviewForm }

  TEditChangeReviewForm = class(TForm)
    btnCancel: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FEditor: TToolkitEditor;
    procedure SetEditor(AValue: TToolkitEditor);
    procedure buildTextDiff;
  public
    property editor : TToolkitEditor read FEditor write SetEditor;

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

procedure TEditChangeReviewForm.FormShow(Sender: TObject);
begin
  buildTextDiff;
end;

procedure TEditChangeReviewForm.SetEditor(AValue: TToolkitEditor);
begin
  FEditor.Free;
  FEditor:=AValue;
end;

procedure TEditChangeReviewForm.buildTextDiff;
var
  base, edit : TStringList;
  diff : TFslTextComparer;
begin
  base := TStringList.create;
  edit := TStringList.create;
  diff := TFslTextComparer.create;
  try
    base.lines.text := editor.GetBytes;


    var
  loaded : TLoadedBytes;
begin
  if FFinishedLoading and Context.hasFocus and Context.focus.hasStore and Context.focus.Store.CheckTimes then
  begin
    loaded := Context.focus.Store.load(Context.focus.session.address);


  finally
    diff.free;
    base.free;
    edit.free;
  end;
end;

end.

