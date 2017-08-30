unit MemoEditorDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit;

type
  TMemoEditorForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MemoEditorForm: TMemoEditorForm;

procedure editMemo(owner : TComponent; caption : String; edt : TEdit);

implementation

{$R *.fmx}

procedure editMemo(owner : TComponent; caption : String; edt : TEdit);
var
  MemoEditorForm: TMemoEditorForm;
begin
  MemoEditorForm := TMemoEditorForm.create(owner);
  try
    MemoEditorForm.caption := caption;
    MemoEditorForm.Memo1.Text := edt.Text;
    if MemoEditorForm.ShowModal = mrOk then
      edt.Text := MemoEditorForm.Memo1.Text;
  finally
    MemoEditorForm.Free;
  end;

end;

end.
