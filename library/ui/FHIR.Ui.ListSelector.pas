unit FHIR.Ui.ListSelector;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls;

type
  TListSelectorForm = class(TForm)
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    btnok: TButton;
    btnCancel: TButton;
    Panel2: TPanel;
    ListBox1: TCheckListBox;
    btnOkAll: TButton;
    cbDontAsk: TCheckBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure ListBox1ClickCheck(Sender: TObject);
    procedure btnOkAllClick(Sender: TObject);
  private
    FOkWithNoneSelected: boolean;
    FVerb: String;
    procedure SetVerb(const Value: String);
  public
    property okWithNoneSelected : boolean read FOkWithNoneSelected write FOkWithNoneSelected;
    property verb : String read FVerb write SetVerb;
  end;

var
  ListSelectorForm: TListSelectorForm;

implementation

{$R *.dfm}

procedure TListSelectorForm.btnOkAllClick(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to ListBox1.Items.Count - 1 do
    ListBox1.Checked[i] := true;
end;

procedure TListSelectorForm.CheckBox1Click(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to ListBox1.Items.Count - 1 do
    ListBox1.Checked[i] := CheckBox1.Checked;
end;

procedure TListSelectorForm.ListBox1ClickCheck(Sender: TObject);
var
  i : integer;
begin
  btnok.Enabled := FOkWithNoneSelected;
  for i := 0 to ListBox1.Items.Count - 1 do
    if ListBox1.Checked[i] then
      btnok.Enabled := true;
end;

procedure TListSelectorForm.SetVerb(const Value: String);
begin
  FVerb := Value;
  btnOk.Caption := FVerb;
  btnOkAll.Caption := 'All + '+FVerb;
  cbDontAsk.Caption := FVerb+' automatically without asking';
end;

end.
