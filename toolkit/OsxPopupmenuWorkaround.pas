unit OsxPopupmenuWorkaround;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.Menus;

type
  TPopupMenuWorkaroundForm = class(TForm)
    ListBox1: TListBox;
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  private
    FMenu: TPopupMenu;
    FSelected: integer;
  public
    property menu : TPopupMenu read FMenu write FMenu;
    property Selected : integer read FSelected write FSelected;
  end;

var
  PopupMenuWorkaroundForm: TPopupMenuWorkaroundForm;

function runPopupAlternative(owner : TComponent; menu : TPopupMenu; left, top : integer) : integer;

implementation

{$R *.fmx}

function runPopupAlternative(owner : TComponent; menu : TPopupMenu; left, top : integer) : integer;
begin
  PopupMenuWorkaroundForm := TPopupMenuWorkaroundForm.Create(owner);
  try
    PopupMenuWorkaroundForm.Left := left;
    PopupMenuWorkaroundForm.Top := top;
    PopupMenuWorkaroundForm.menu := menu;
    PopupMenuWorkaroundForm.ShowModal;
    result := PopupMenuWorkaroundForm.Selected;
  finally
    PopupMenuWorkaroundForm.Free;
  end;
end;

procedure TPopupMenuWorkaroundForm.FormShow(Sender: TObject);
var
  i : integer;
begin
  ListBox1.Items.Clear;
  for i := 0 to menu.ItemsCount - 1 do
    listBox1.Items.Add(menu.Items[i].Text);
end;

procedure TPopupMenuWorkaroundForm.ListBox1Click(Sender: TObject);
begin
  FSelected := ListBox1.ItemIndex;
  ModalResult := mrOk;
end;

procedure TPopupMenuWorkaroundForm.ListBox1KeyDown(Sender: TObject;
  var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkEscape then
     ModalResult := mrClose;
end;

end.
