unit frm_format_chooser;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ftk_context;

type

  { TFormatChooserForm }

  TFormatChooserForm = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    lbChoices: TListBox;
    mSource: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Splitter1: TSplitter;
    procedure lbChoicesClick(Sender: TObject);
    procedure lbChoicesDblClick(Sender: TObject);
  private

  public

  end;

var
  FormatChooserForm: TFormatChooserForm;

function ChooseFormat(owner : TForm; fmts : TSourceEditorKindSet; source : String) : TSourceEditorKind;

implementation

{$R *.lfm}

function ChooseFormat(owner : TForm; fmts : TSourceEditorKindSet; source : String) : TSourceEditorKind;
var
  FormatChooserForm: TFormatChooserForm;
  a : TSourceEditorKind;
begin
  FormatChooserForm := TFormatChooserForm.create(owner);
  try
    FormatChooserForm.mSource.Text := source;
    FormatChooserForm.lbChoices.items.clear;
    for a in TSourceEditorKindSet do
      if a in fmts then
        FormatChooserForm.lbChoices.items.AddObject(NAMES_TSourceEditorKind[a], TObject(a));
    if FormatChooserForm.ShowModal = mrOk then
      result := TSourceEditorKind(integer(FormatChooserForm.lbChoices.items.Objects[FormatChooserForm.lbChoices.ItemIndex]))
    else
      result := sekNull;
  finally
    FormatChooserForm.free;
  end;
end;

{ TFormatChooserForm }

procedure TFormatChooserForm.lbChoicesClick(Sender: TObject);
begin
  btnOk.enabled := lbChoices.ItemIndex <> -1;
end;

procedure TFormatChooserForm.lbChoicesDblClick(Sender: TObject);
begin
  if lbChoices.ItemIndex <> -1 then
    ModalResult := mrOk;
end;

end.

