unit ValuesetExpansion;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.StdCtrls, FMX.DateTimeCtrls,
  FMX.Edit, FMX.Controls.Presentation, IniFiles,
  FHIRResources, FHIRUtilities, FHIRClient, FMX.ListBox,
  SettingsDialog;

type
  TValuesetExpansionForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    Go: TButton;
    Label2: TLabel;
    edtFilter: TEdit;
    Label3: TLabel;
    dedDate: TDateEdit;
    Label4: TLabel;
    edtOffset: TEdit;
    Label5: TLabel;
    edtCount: TEdit;
    Label6: TLabel;
    edtLang: TEdit;
    cbDesignations: TCheckBox;
    cbActiveOnly: TCheckBox;
    cbUIOnly: TCheckBox;
    cbNoExpressions: TCheckBox;
    cbAllowSubset: TCheckBox;
    gridContains: TGrid;
    StringColumn10: TStringColumn;
    CheckColumn1: TCheckColumn;
    CheckColumn2: TCheckColumn;
    StringColumn11: TStringColumn;
    StringColumn12: TStringColumn;
    StringColumn13: TStringColumn;
    cbxServer: TComboBox;
    btnSettings: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GoClick(Sender: TObject);
    procedure gridContainsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure btnSettingsClick(Sender: TObject);
  private
    FValueSet: TFHIRValueSet;
    FExpansion : TFHIRValueSet;
    FIni: TIniFile;
    FClient : TFhirClient;
    procedure SetValueSet(const Value: TFHIRValueSet);
    procedure SetExpansion(const Value: TFHIRValueSet);
  public
    destructor Destroy; override;
    property ValueSet : TFHIRValueSet read FValueSet write SetValueSet;
    property Expansion : TFHIRValueSet read FExpansion write SetExpansion;
    property Ini : TIniFile read FIni write FIni;
  end;

var
  ValuesetExpansionForm: TValuesetExpansionForm;

implementation

{$R *.fmx}

Uses
  FHIRToolkitForm;

{ TValuesetExpansionForm }

procedure TValuesetExpansionForm.btnSettingsClick(Sender: TObject);
var
  form : TSettingsForm;
begin
  form := TSettingsForm.create(self);
  try
    form.Ini := FIni;
    form.TabControl1.TabIndex := 1;
    if form.showmodal = mrOk then
    begin
      FIni.ReadSection('Terminology-Servers', cbxServer.Items);
      cbxServer.ItemIndex := 0;
    end;
  finally
    form.free;
  end;
end;

destructor TValuesetExpansionForm.Destroy;
begin
  FExpansion.Free;
  FValueSet.Free;
  FClient.Free;
  inherited;
end;

procedure TValuesetExpansionForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  s : String;
begin
  try
    FIni.WriteInteger('Expansion.Window', 'left', left);
    FIni.WriteInteger('Expansion.Window', 'top', top);
    FIni.WriteInteger('Expansion.Window', 'width', width);
    FIni.WriteInteger('Expansion.Window', 'height', height);

    FIni.WriteString('Expansion', 'Filter', edtFilter.Text);
    FIni.WriteString('Expansion', 'Date', dedDate.Text);
    FIni.WriteString('Expansion', 'Offset', edtOffset.Text);
    FIni.WriteString('Expansion', 'Count', edtCount.Text);
    FIni.WriteString('Expansion', 'Lang', edtLang.Text);
    FIni.WriteBool('Expansion', 'Designations', cbDesignations.IsChecked);
    FIni.WriteBool('Expansion', 'ActiveOnly', cbActiveOnly.IsChecked);
    FIni.WriteBool('Expansion', 'UIOnly', cbUIOnly.IsChecked);
    FIni.WriteBool('Expansion', 'NoExpressions', cbNoExpressions.IsChecked);
    FIni.WriteBool('Expansion', 'AllowSubset', cbAllowSubset.IsChecked);

    FIni.WriteInteger('Expansion', 'width-col-1', trunc(StringColumn10.Width));
    FIni.WriteInteger('Expansion', 'width-col-1', trunc(CheckColumn1.Width));
    FIni.WriteInteger('Expansion', 'width-col-1', trunc(CheckColumn2.Width));
    FIni.WriteInteger('Expansion', 'width-col-1', trunc(StringColumn11.Width));
    FIni.WriteInteger('Expansion', 'width-col-1', trunc(StringColumn12.Width));
    FIni.WriteInteger('Expansion', 'width-col-1', trunc(StringColumn13.Width));
  except
  end;
end;

procedure TValuesetExpansionForm.FormShow(Sender: TObject);
begin
  Left := FIni.ReadInteger('Expansion.Window', 'left', left);
  Top := FIni.ReadInteger('Expansion.Window', 'top', top);
  Width := FIni.ReadInteger('Expansion.Window', 'width', width);
  Height := FIni.ReadInteger('Expansion.Window', 'height', height);

  FIni.ReadSection('Terminology-Servers', cbxServer.Items);
  if cbxServer.Items.Count = 0 then
    cbxServer.Items.Add('http://tx.fhir.org/r3');
  cbxServer.ItemIndex := 0;

  edtFilter.Text := FIni.ReadString('Expansion', 'Filter', '');
  dedDate.Text := FIni.ReadString('Expansion', 'Date', '');
  edtOffset.Text := FIni.ReadString('Expansion', 'Offset', '');
  edtCount.Text := FIni.ReadString('Expansion', 'Count', '');
  edtLang.Text := FIni.ReadString('Expansion', 'Lang', '');
  cbDesignations.IsChecked := FIni.ReadBool('Expansion', 'Designations', false);
  cbActiveOnly.IsChecked := FIni.ReadBool('Expansion', 'ActiveOnly', false);
  cbUIOnly.IsChecked := FIni.ReadBool('Expansion', 'UIOnly', false);
  cbNoExpressions.IsChecked := FIni.ReadBool('Expansion', 'NoExpressions', false);
  cbAllowSubset.IsChecked := FIni.ReadBool('Expansion', 'AllowSubset', false);

  StringColumn10.Width := FIni.ReadInteger('Expansion', 'width-col-1', trunc(StringColumn10.Width));
  CheckColumn1.Width := FIni.ReadInteger('Expansion', 'width-col-1', trunc(CheckColumn1.Width));
  CheckColumn2.Width := FIni.ReadInteger('Expansion', 'width-col-1', trunc(CheckColumn2.Width));
  StringColumn11.Width := FIni.ReadInteger('Expansion', 'width-col-1', trunc(StringColumn11.Width));
  StringColumn12.Width := FIni.ReadInteger('Expansion', 'width-col-1', trunc(StringColumn12.Width));
  StringColumn13.Width := FIni.ReadInteger('Expansion', 'width-col-1', trunc(StringColumn13.Width));
end;

procedure TValuesetExpansionForm.GoClick(Sender: TObject);
var
  params :  TFHIRParameters;
begin
  FExpansion.Free;
  FExpansion := nil;
  gridContains.RowCount := 0;
  button1.Enabled := false;

  if FClient = nil then
    FClient := TFhirThreadedClient.Create(TFhirHTTPClient.Create(nil, cbxServer.items[cbxServer.itemIndex], false, FIni.ReadInteger('HTTP', 'timeout', 5) * 1000, FIni.ReadString('HTTP', 'proxy', '')), MasterToolsForm.threadMonitorProc);

  MasterToolsForm.dowork(self, 'Expanding',
    procedure
    var
      params :  TFHIRParameters;
    begin
      params := TFhirParameters.Create;
      try
        params.AddParameter('valueSet', FValueSet.Link);
        if edtFilter.Text <> '' then
          params.AddParameter('filter', edtFilter.Text);
        if dedDate.Text <> '' then
          params.AddParameter('date', dedDate.Text);
        if edtOffset.Text <> '' then
          params.AddParameter('offset', edtOffset.Text);
        if edtCount.Text <> '' then
          params.AddParameter('count', edtCount.Text);
        if edtLang.Text <> '' then
          params.AddParameter('displayLanguage', edtLang.Text);

        if cbDesignations.IsChecked then
          params.AddParameter('includeDesignations', true);
        if cbActiveOnly.IsChecked then
          params.AddParameter('activeOnly', true);
        if cbUIOnly.IsChecked then
          params.AddParameter('excludeNotForUI', true);
        if cbNoExpressions.IsChecked then
          params.AddParameter('excludePostCoordinated', true);
        if cbAllowSubset.IsChecked then
          params.AddParameter('limitedExpansion', true);

        FExpansion := FClient.operation(frtValueSet, 'expand', params) as TFHIRValueSet;
        gridContains.RowCount := FExpansion.expansion.containsList.Count;
        button1.Enabled := FExpansion.expansion.containsList.Count > 0;
      finally
        params.Free;
      end;
    end);
end;

procedure TValuesetExpansionForm.gridContainsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  contains : TFhirValueSetExpansionContains;
begin
  contains := Expansion.expansion.containsList[ARow];
  case ACol of
    0: value := contains.code;
    1: value := contains.abstract;
    2: value := contains.inactive;
    3: value := contains.display;
    4: value := contains.system;
    5: value := contains.version;
  end;
end;

procedure TValuesetExpansionForm.SetExpansion(const Value: TFHIRValueSet);
begin
  FExpansion.Free;
  FExpansion := Value;
end;

procedure TValuesetExpansionForm.SetValueSet(const Value: TFHIRValueSet);
begin
  FValueSet.Free;
  FValueSet := Value;
end;

end.
