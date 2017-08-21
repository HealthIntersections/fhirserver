unit ListSelector;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TListSelectorForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    CheckBox1: TCheckBox;
    procedure ListBox1ChangeCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    FOkWithNoneSelected: boolean;
    { Private declarations }
  public
    { Public declarations }
    property okWithNoneSelected : boolean read FOkWithNoneSelected write FOkWithNoneSelected;
  end;

var
  ListSelectorForm: TListSelectorForm;

implementation

{$R *.fmx}

procedure TListSelectorForm.CheckBox1Change(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to ListBox1.Items.Count - 1 do
    ListBox1.ListItems[i].IsChecked := CheckBox1.IsChecked;
end;

procedure TListSelectorForm.FormShow(Sender: TObject);
begin
  CheckBox1.Visible := ListBox1.ShowCheckboxes;
end;

procedure TListSelectorForm.ListBox1ChangeCheck(Sender: TObject);
var
  i : integer;
begin
  if not ListBox1.ShowCheckboxes then
    Button1.Enabled := FOkWithNoneSelected or (ListBox1.ItemIndex > -1)
  else
  begin
    Button1.Enabled := FOkWithNoneSelected;
    for i := 0 to ListBox1.Items.Count - 1 do
      if ListBox1.ListItems[i].IsChecked then
        Button1.Enabled := true;
  end;
end;

procedure TListSelectorForm.ListBox1DblClick(Sender: TObject);
begin
  if not ListBox1.ShowCheckboxes then
   ModalResult := mrOk;
end;

end.
