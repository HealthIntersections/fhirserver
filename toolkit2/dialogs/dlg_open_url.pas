unit dlg_open_url;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  fui_lcl_utilities,
  ftk_store_temp;

type

  { TOpenURLForm }

  TOpenURLForm = class(TForm)
    btnOk: TButton;
    Button1: TButton;
    btnCancel: TButton;
    cbxURL: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    procedure FormCreate(Sender: TObject);
  private
  end;

var
  OpenURLForm: TOpenURLForm;

implementation

{$R *.lfm}

{ TOpenURLForm }

procedure TOpenURLForm.FormCreate(Sender: TObject);
begin
  setForOs(btnOk, btnCancel);
end;

end.

