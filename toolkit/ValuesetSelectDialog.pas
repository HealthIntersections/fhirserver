unit ValuesetSelectDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.StdCtrls, FMX.DateTimeCtrls, FMX.ListBox,
  FMX.Edit, FMX.Controls.Presentation, IniFiles, GuidSupport,
  FHIRTypes, FHIRResources, FHIRUtilities, FHIRClient,
  SettingsDialog, FMX.ComboEdit;

type
  TValuesetSelectForm = class(TForm)
    Panel1: TPanel;
    Button2: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    Go: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    edtValue: TEdit;
    gridContains: TGrid;
    cbxServer: TComboBox;
    btnSettings: TButton;
    cbxOperation: TComboBox;
    CheckColumn1: TCheckColumn;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    CheckColumn2: TCheckColumn;
    btnAppend: TButton;
    btnReplace: TButton;
    CheckColumn3: TCheckColumn;
    Label4: TLabel;
    btnAll: TButton;
    btnActive: TButton;
    btnSelectable: TButton;
    btnNone: TButton;
    cbUseDisplays: TCheckBox;
    cbeProperty: TComboEdit;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GoClick(Sender: TObject);
    procedure gridContainsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnAllClick(Sender: TObject);
    procedure btnNoneClick(Sender: TObject);
    procedure btnActiveClick(Sender: TObject);
    procedure btnSelectableClick(Sender: TObject);
    procedure btnAppendClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure gridContainsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
  private
    FExpansion : TFHIRValueSet;
    FIni: TIniFile;
    FClient : TFhirClient;
    FVersion: String;
    FSystem: String;
    FHasConcepts: boolean;
    FReplace: boolean;
    function selectedCount : integer;
    procedure SetExpansion(const Value: TFHIRValueSet);
  public
    destructor Destroy; override;

    property ini : TIniFile read FIni write FIni;
    property system : String read FSystem write FSystem;
    property version : String read FVersion write FVersion;
    property hasConcepts : boolean read FHasConcepts write FHasConcepts;
    property replace : boolean read FReplace write FReplace;
    property expansion : TFHIRValueSet read FExpansion;
  end;

var
  ValuesetSelectForm: TValuesetSelectForm;

implementation

{$R *.fmx}

Uses
  FHIRToolkitForm;

{ TValuesetExpansionForm }

procedure TValuesetSelectForm.btnSelectableClick(Sender: TObject);
var
  contains : TFhirValueSetExpansionContains;
begin
  for contains in FExpansion.expansion.containsList do
    if contains.abstract then
      contains.TagInt := 0
    else
      contains.TagInt := 1;
  gridContains.RowCount := 0;
  gridContains.RowCount := FExpansion.expansion.containsList.Count;
  btnAppend.Enabled := selectedCount > 0;
  btnReplace.Enabled := HasConcepts and (selectedCount > 0);
end;

procedure TValuesetSelectForm.btnSettingsClick(Sender: TObject);
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

procedure TValuesetSelectForm.btnActiveClick(Sender: TObject);
var
  contains : TFhirValueSetExpansionContains;
begin
  for contains in FExpansion.expansion.containsList do
    if contains.inactive then
      contains.TagInt := 0
    else
      contains.TagInt := 1;
  gridContains.RowCount := 0;
  gridContains.RowCount := FExpansion.expansion.containsList.Count;
  btnAppend.Enabled := selectedCount > 0;
  btnReplace.Enabled := HasConcepts and (selectedCount > 0);
end;

procedure TValuesetSelectForm.btnNoneClick(Sender: TObject);
var
  contains : TFhirValueSetExpansionContains;
begin
  for contains in FExpansion.expansion.containsList do
    contains.TagInt := 0;
  gridContains.RowCount := 0;
  gridContains.RowCount := FExpansion.expansion.containsList.Count;
  btnAppend.Enabled := selectedCount > 0;
  btnReplace.Enabled := HasConcepts and (selectedCount > 0);
end;

procedure TValuesetSelectForm.btnReplaceClick(Sender: TObject);
begin
  if selectedCount > 0 then
  begin
    replace := true;
    ModalResult := mrOk;
  end;
end;

procedure TValuesetSelectForm.btnAllClick(Sender: TObject);
var
  contains : TFhirValueSetExpansionContains;
begin
  for contains in FExpansion.expansion.containsList do
    contains.TagInt := 1;
  gridContains.RowCount := 0;
  gridContains.RowCount := FExpansion.expansion.containsList.Count;
  btnAppend.Enabled := selectedCount > 0;
  btnReplace.Enabled := HasConcepts and (selectedCount > 0);
end;

procedure TValuesetSelectForm.btnAppendClick(Sender: TObject);
begin
  if selectedCount > 0 then
  begin
    replace := false;
    ModalResult := mrOk;
  end;
end;

destructor TValuesetSelectForm.Destroy;
begin
  FExpansion.Free;
  FClient.Free;
  inherited;
end;

procedure TValuesetSelectForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  s : String;
begin
  try
    FIni.WriteInteger('ValueSet.Select.Window', 'left', left);
    FIni.WriteInteger('ValueSet.Select.Window', 'top', top);
    FIni.WriteInteger('ValueSet.Select.Window', 'width', width);
    FIni.WriteInteger('ValueSet.Select.Window', 'height', height);

    FIni.WriteString('ValueSet.Select', 'Property', cbeProperty.Text);
    FIni.WriteInteger('ValueSet.Select', 'Operation', cbxOperation.ItemIndex);
    FIni.WriteString('ValueSet.Select', 'Value', edtValue.Text);
  except
  end;
end;

procedure TValuesetSelectForm.FormShow(Sender: TObject);
begin
  Left := FIni.ReadInteger('ValueSet.Select.Window', 'left', left);
  Top := FIni.ReadInteger('ValueSet.Select.Window', 'top', top);
  Width := FIni.ReadInteger('ValueSet.Select.Window', 'width', width);
  Height := FIni.ReadInteger('ValueSet.Select.Window', 'height', height);

  FIni.ReadSection('Terminology-Servers', cbxServer.Items);
  if cbxServer.Items.Count = 0 then
    cbxServer.Items.Add('http://tx.fhir.org/r3');
  cbxServer.ItemIndex := 0;

  edtProperty.Text := FIni.ReadString('ValueSet.Select', 'Property', '');
  cbxOperation.ItemIndex := FIni.ReadInteger('ValueSet.Select', 'Operation', 0);
  edtValue.Text := FIni.ReadString('ValueSet.Select', 'Value', '');
end;

procedure TValuesetSelectForm.GoClick(Sender: TObject);
var
  params :  TFHIRParameters;
  vs : TFHIRValueSet;
  inc : TFhirValueSetComposeInclude;
  filter : TFhirValueSetComposeIncludeFilter;
begin
  vs := TFHIRValueSet.create;
  try
    vs.url := NewGuidURN;
    vs.compose := TFhirValueSetCompose.create;
    inc := vs.compose.includeList.Append;
    inc.system := system;
    inc.version := version;
    filter := inc.filterList.Append;
    filter.property_ := edtProperty.Text;
    filter.op := TFhirFilterOperatorEnum(cbxOperation.ItemIndex);
    filter.value := edtValue.Text;

    FExpansion.Free;
    FExpansion := nil;
    gridContains.RowCount := 0;
    btnAppend.Enabled := false;
    btnReplace.Enabled := false;

    if FClient = nil then
      FClient := TFhirThreadedClient.Create(TFhirHTTPClient.Create(nil, cbxServer.items[cbxServer.itemIndex], false, FIni.ReadInteger('HTTP', 'timeout', 5) * 1000, FIni.ReadString('HTTP', 'proxy', '')), MasterToolsForm.threadMonitorProc);

    MasterToolsForm.dowork(self, 'Searching',
      procedure
      var
        params :  TFHIRParameters;
      begin
        params := TFhirParameters.Create;
        try
          params.AddParameter('valueSet', vs.Link);

          FExpansion := FClient.operation(frtValueSet, 'expand', params) as TFHIRValueSet;
          gridContains.RowCount := FExpansion.expansion.containsList.Count;
          btnAppend.Enabled := selectedCount > 0;
          btnReplace.Enabled := HasConcepts and (selectedCount > 0);
        finally
          params.Free;
        end;
      end);
  finally
    vs.Free;
  end;
end;

procedure TValuesetSelectForm.gridContainsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  contains : TFhirValueSetExpansionContains;
begin
  contains := FExpansion.expansion.containsList[ARow];
  case ACol of
    0: value := contains.TagInt > 0;
    1: value := contains.code;
    2: value := contains.display;
    3: value := not contains.abstract;
    4: value := not contains.inactive;
  end;
end;

procedure TValuesetSelectForm.gridContainsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  contains : TFhirValueSetExpansionContains;
begin
  contains := FExpansion.expansion.containsList[ARow];
  if ACol = 0 then
    if value.AsBoolean then
      contains.TagInt := 1
    else
      contains.TagInt := 0;
  btnAppend.Enabled := selectedCount > 0;
  btnReplace.Enabled := HasConcepts and (selectedCount > 0);
end;

function TValuesetSelectForm.selectedCount: integer;
var
  contains : TFhirValueSetExpansionContains;
begin
  result := 0;
  for contains in FExpansion.expansion.containsList do
    if contains.TagInt > 0 then
      inc(result);
end;

procedure TValuesetSelectForm.SetExpansion(const Value: TFHIRValueSet);
begin
  FExpansion.Free;
  FExpansion := Value;
end;


end.
