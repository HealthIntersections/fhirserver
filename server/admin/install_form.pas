unit install_form;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  StdCtrls, ComCtrls,
  fsl_base, fsl_npm_client,
  fdb_manager;

type

  { TEndpointInstallForm }

  TEndpointInstallForm = class(TForm)
    btnDBTest1: TBitBtn;
    btnDBTest3: TBitBtn;
    cbxSecurity: TComboBox;
    edtUserName: TEdit;
    edtPassword: TEdit;
    edtAnonymousRights: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblMode: TLabel;
    lvPackages: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FConnection: TFDBConnection;
    FMode: String;
    FPackages : TFslList<TFHIRPackageInfo>;
    FVersion: String;
    function isAutomatic(pi: TFHIRPackageInfo): boolean;
    function matchesVersion(pi, piv: String): boolean;
    procedure SetConnection(AValue: TFDBConnection);
    procedure SetPackages(AValue: TFslList<TFHIRPackageInfo>);
    procedure loadPackages;
  public
    property Packages : TFslList<TFHIRPackageInfo> read FPackages write SetPackages;
    property Connection : TFDBConnection read FConnection write SetConnection;
    property version : String read FVersion write FVersion;
    property mode : String read FMode write FMode;
  end;

var
  EndpointInstallForm: TEndpointInstallForm;

implementation

{$R *.lfm}

{ TEndpointInstallForm }

procedure TEndpointInstallForm.FormDestroy(Sender: TObject);
begin
  FPackages.Free;
  FConnection.Free;
end;

function TEndpointInstallForm.matchesVersion(pi, piv : String):boolean;
begin
  if version = 'r2' then
    result := SameText(pi, 'DSTU2') or SameText(pi, 'STU2') or pi.StartsWith('1.0') or piv.StartsWith('1.0')
  else if version = 'r3' then
    result := SameText(pi, 'STU3') or pi.StartsWith('3.0') or piv.StartsWith('3.0')
  else if version = 'r4' then
    result := SameText(pi, 'R4') or pi.StartsWith('4.0') or piv.StartsWith('4.0')
  else if version = 'r5' then
    result := SameText(pi, 'R5') or pi.StartsWith('4.5') or piv.StartsWith('4.5')
  else
    result := false;
end;

function TEndpointInstallForm.isAutomatic(pi : TFHIRPackageInfo):boolean;
begin
  if pi.id = 'hl7.fhir.core' then
    result := true
  else if pi.id = 'hl7.fhir.'+version+'.core' then
    result := true
  else if pi.id = 'hl7.terminology' then
    result := true
  else
    result := false;
end;

procedure TEndpointInstallForm.FormShow(Sender: TObject);
begin
  lblMode.caption := 'Install '+mode+' for version '+version;
  if mode = 'terminology' then
  begin
    edtUserName.text := '';
    edtUserName.Enabled := true;
    edtPassword.text := '';
    edtPassword.Enabled := true;
    cbxSecurity.itemIndex := 0;
    edtPassword.Enabled := false;
    edtAnonymousRights.text := 'User/*.read';
    edtAnonymousRights.Enabled := false;
    lvPackages.items.clear;
    lvPackages.Enabled := false;
    loadPackages;
  end
  else if mode = 'general' then
  begin
    edtUserName.text := '';
    edtUserName.Enabled := true;
    edtPassword.text := '';
    edtPassword.Enabled := true;
    cbxSecurity.itemIndex := 0;
    edtPassword.Enabled := true;
    edtAnonymousRights.text := 'User/*.*';
    edtAnonymousRights.Enabled := true;
    lvPackages.items.clear;
    lvPackages.Enabled := true;
    loadPackages;
  end
  else
  begin
    edtUserName.text := 'n/a';
    edtUserName.Enabled := false;
    edtPassword.text := 'n/a';
    edtPassword.Enabled := false;
    cbxSecurity.itemIndex := -1;
    edtPassword.Enabled := false;
    edtAnonymousRights.text := 'n/a';
    edtAnonymousRights.Enabled := false;
    lvPackages.items.clear;
    lvPackages.Enabled := false;
  end;
end;

procedure TEndpointInstallForm.SetPackages(AValue: TFslList<TFHIRPackageInfo>);
begin
  FPackages.Free;
  FPackages := AValue;
end;

procedure TEndpointInstallForm.loadPackages;
var
  pi : TFHIRPackageInfo;
  li : TListItem;
begin
  lvPackages.Items.clear;
  for pi in FPackages do
  begin
    if matchesversion(pi.fhirVersion, pi.version) and not isAutomatic(pi) then
    begin
      li := lvPackages.items.Add;
      li.Caption := pi.id+'#'+pi.version;
      li.Checked := false;
    end;
  end;
  lvPackages.ViewStyle := vsIcon;
  lvPackages.ViewStyle := vsList;
end;

procedure TEndpointInstallForm.SetConnection(AValue: TFDBConnection);
begin
  FConnection.Free;
  FConnection := AValue;
end;

end.

