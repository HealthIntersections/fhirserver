unit console_dest_edit;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons,
  server_ini;

type
  { TEditDestForm }

  TEditDestForm = class(TForm)
    btnSource: TBitBtn;
    btnDBTest1: TBitBtn;
    btnDBTest3: TBitBtn;
    cbxType: TComboBox;
    cbxSource: TComboBox;
    chkDefault: TCheckBox;
    edtIdentity: TEdit;
    edtSource: TEdit;
    edtVersion: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    dlgOpen: TOpenDialog;
    Panel1: TPanel;
    procedure btnSourceClick(Sender: TObject);
    procedure cbxTypeChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbMSSQLClick(Sender: TObject);
  private
    FIni: TFHIRServerIniFile;
    FTx: TFHIRServerIniComplex;
    function isDatabase(type_: String): boolean;
    procedure SetIni(AValue: TFHIRServerIniFile);
    procedure SetTx(AValue: TFHIRServerIniComplex);
  public
    property Tx : TFHIRServerIniComplex read FTx write SetTx;
    property Ini : TFHIRServerIniFile read FIni write SetIni;
    procedure update;
  end;

var
  EditDestForm: TEditDestForm;

implementation

{$R *.lfm}

{ TEditDestForm }

procedure TEditDestForm.FormDestroy(Sender: TObject);
begin
  FIni.Free;
  FTx.Free;
end;

procedure TEditDestForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOk then
  begin
    update;
    CanClose := true;
  end;
end;

function TEditDestForm.isDatabase(type_ : String) : boolean;
begin
  result := (type_ = 'rxnorm') or (type_ = 'ndc') or (type_ = 'unii') or (type_ = 'ndfrt') or (type_ = 'nci');
end;

procedure TEditDestForm.SetIni(AValue: TFHIRServerIniFile);
var
  s : String;
begin
  FIni.Free;
  FIni:=AValue;
  if (FIni <> nil) then
  begin
    cbxSource.items.clear;
    for s in FIni.databases.SortedKeys do
      cbxSource.items.add(s);
  end;
end;

procedure TEditDestForm.cbxTypeChange(Sender: TObject);
begin
  Tx['type'] := cbxType.items[cbxType.ItemIndex];
  if isDatabase(Tx['type']) then
  begin
    edtSource.Visible := false;
    btnSource.Visible := false;
    cbxSource.Visible := true;
    cbxSource.itemIndex := cbxSource.Items.IndexOf(Tx['database']);
  end
  else
  begin
    cbxSource.Visible := false;
    edtSource.Visible := true;
    btnSource.Visible := true;
    edtSource.Text := Tx['source'];
  end;
  edtVersion.enabled := Tx['type'] = 'ndc';
  chkDefault.Enabled := Tx['type'] = 'snomed';
end;

procedure TEditDestForm.btnSourceClick(Sender: TObject);
begin
  dlgOpen.fileName := edtSource.text;
  if dlgOpen.Execute then
    edtSource.text := dlgOpen.fileName;
end;

procedure TEditDestForm.FormResize(Sender: TObject);
begin
end;

procedure TEditDestForm.FormShow(Sender: TObject);
begin
end;

procedure TEditDestForm.rbMSSQLClick(Sender: TObject);
begin
end;

procedure TEditDestForm.SetTx(AValue: TFHIRServerIniComplex);
begin
  FTx.Free;
  FTx:=AValue;

  if FTx <> nil then
  begin
    edtIdentity.text := Tx.name;
    cbxType.itemIndex := cbxType.Items.IndexOf(Tx['type']);
    cbxTypeChange(self);
    edtVersion.text := Tx['version'];
    chkDefault.Checked := Tx['default'] = 'true';
  end;
end;

procedure TEditDestForm.update;
begin
  Tx.name := edtIdentity.text;
  if cbxSource.itemIndex > -1 then
    Tx['database'] := cbxSource.Items[cbxSource.itemIndex]
  else
    Tx['database'] := '';
  Tx['source'] := edtSource.Text;
  Tx['version'] := edtVersion.text;
  if not chkDefault.Enabled then
    Tx['default'] := ''
  else if chkDefault.Checked then
    Tx['default'] := 'true'
  else
    Tx['default'] := 'false'
end;

end.

