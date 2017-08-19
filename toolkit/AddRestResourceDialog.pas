unit AddRestResourceDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TAddRestResourceForm = class(TForm)
    cbRead: TCheckBox;
    cbVRead: TCheckBox;
    cbSearch: TCheckBox;
    cbCreate: TCheckBox;
    cbUpdate: TCheckBox;
    cbPatch: TCheckBox;
    cbDelete: TCheckBox;
    cbHistoryInstance: TCheckBox;
    cbHistoryType: TCheckBox;
    cbUpdateCreate: TCheckBox;
    cbCondCreate: TCheckBox;
    cbCondUpdate: TCheckBox;
    cbCondDelete: TCheckBox;
    cbxVersioning: TComboBox;
    cbxReadCondition: TComboBox;
    Label22: TLabel;
    cbRefLocal: TCheckBox;
    cbRefEnforced: TCheckBox;
    cbRefResolve: TCheckBox;
    cbRefLogical: TCheckBox;
    Label23: TLabel;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    Label21: TLabel;
    Label24: TLabel;
    cbRefLiteral: TCheckBox;
    cbStandardSearch: TCheckBox;
    Panel3: TPanel;
    ListBox1: TListBox;
    procedure ListBox1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AddRestResourceForm: TAddRestResourceForm;

implementation

{$R *.fmx}

procedure TAddRestResourceForm.FormActivate(Sender: TObject);
begin
  ListBox1Click(nil);
end;

procedure TAddRestResourceForm.ListBox1Click(Sender: TObject);
var
  i : integer;
begin
  button1.Enabled := false;
  for i := 0 to ListBox1.Items.Count - 1 do
    if ListBox1.ListItems[i].IsChecked then
      Button1.Enabled := true;
end;

end.
