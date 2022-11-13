unit dlg_txsrvr_props;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  fui_lcl_utilities;

type

  { TTxServerPropertiesDialog }

  TTxServerPropertiesDialog = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    Button1: TButton;
    chkDefault: TCheckBox;
    edtName: TEdit;
    edtAddress: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  TxServerPropertiesDialog: TTxServerPropertiesDialog;

implementation

{$R *.lfm}

{ TTxServerPropertiesDialog }

procedure TTxServerPropertiesDialog.FormCreate(Sender: TObject);
begin
  setForOs(btnOk, btnCancel);
end;

end.

