unit console_ep_edit;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons,
  server_ini;

type
  { TEditEPForm }

  TEditEPForm = class(TForm)
    btnDBTest1: TBitBtn;
    btnDBTest3: TBitBtn;
    cbxDatabase: TComboBox;
    cbxType: TComboBox;
    cbxMode: TComboBox;
    edtIdentity: TEdit;
    edtPath: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    procedure cbxTypeChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
  private
    FIni: TFHIRServerIniFile;
    FEP: TFHIRServerIniComplex;
    function isDatabase(type_: String): boolean;
    procedure SetIni(AValue: TFHIRServerIniFile);
    procedure SetEP(AValue: TFHIRServerIniComplex);
  public
    property EP : TFHIRServerIniComplex read FEP write SetEP;
    property Ini : TFHIRServerIniFile read FIni write SetIni;
    procedure update;
  end;

var
  EditEPForm: TEditEPForm;

implementation

{$R *.lfm}

{ TEditEPForm }

procedure TEditEPForm.FormDestroy(Sender: TObject);
begin
  FIni.Free;
  FEP.Free;
end;

procedure TEditEPForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOk then
  begin
    update;
    CanClose := true;
  end;
end;

function TEditEPForm.isDatabase(type_ : String) : boolean;
begin
  result := (type_ = 'rxnorm') or (type_ = 'ndc') or (type_ = 'unii') or (type_ = 'ndfrt') or (type_ = 'nci');
end;

procedure TEditEPForm.SetIni(AValue: TFHIRServerIniFile);
var
  s : String;
begin
  FIni.Free;
  FIni:=AValue;
  if (FIni <> nil) then
  begin
    cbxDatabase.items.clear;
    for s in FIni.databases.SortedKeys do
      cbxDatabase.items.add(s);
  end;
end;

procedure TEditEPForm.SetEP(AValue: TFHIRServerIniComplex);
begin
  FEp.Free;
  FEp:=AValue;

  if FEp <> nil then
  begin
    edtIdentity.text := EP['id'];
    cbxType.itemIndex := cbxType.Items.IndexOf(EP['type']);
    cbxMode.itemIndex := cbxMode.Items.IndexOf(EP['mode']);
    edtPath.text := EP['path'];
    cbxDatabase.itemIndex := cbxDatabase.Items.IndexOf(EP['database']);
    cbxTypeChange(self);
  end;
end;


procedure TEditEPForm.cbxTypeChange(Sender: TObject);
begin
  if cbxType.ItemIndex = -1 then
  begin
    cbxMode.Enabled := false;
  end
  else
  if cbxType.items[cbxType.ItemIndex] = 'package' then
  begin
    cbxMode.Enabled := false;
    cbxMode.itemIndex := -1;
  end
  else
  begin
    cbxMode.Enabled := true;
    if cbxMode.itemIndex = -1 then
      cbxMode.itemIndex := cbxMode.Items.IndexOf(EP['mode']);
  end;
end;

procedure TEditEPForm.update;
begin
  EP['id'] := edtIdentity.text;
  if cbxType.ItemIndex > -1 then
    EP['type'] := cbxType.items[cbxType.ItemIndex]
  else
    EP['type'] := '';
  if cbxMode.itemIndex > -1 then
    EP['mode'] := cbxMode.Items[cbxMode.itemIndex]
  else
    EP['mode'] := '';
  EP['path'] := edtPath.text;
  if cbxDatabase.itemIndex > -1 then
    EP['database'] := cbxDatabase.Items[cbxDatabase.itemIndex]
  else
    EP['database'] := '';
end;

end.

