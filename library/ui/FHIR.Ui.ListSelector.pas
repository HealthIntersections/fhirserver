unit FHIR.Ui.ListSelector;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls;

type
  TListSelectorForm = class(TForm)
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    ListBox1: TCheckListBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure ListBox1ClickCheck(Sender: TObject);
  private
    FOkWithNoneSelected: boolean;
  public
    property okWithNoneSelected : boolean read FOkWithNoneSelected write FOkWithNoneSelected;
  end;

var
  ListSelectorForm: TListSelectorForm;

implementation

{$R *.dfm}

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
  Button1.Enabled := FOkWithNoneSelected;
  for i := 0 to ListBox1.Items.Count - 1 do
    if ListBox1.Checked[i] then
      Button1.Enabled := true;
end;

end.
