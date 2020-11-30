unit ValuesetSelectDialog;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.StdCtrls, FMX.DateTimeCtrls, FMX.ListBox,
  FMX.Edit, FMX.Controls.Presentation, fsl_utilities,
  FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Resources.Base, FHIR.Version.Utilities, FHIR.Version.Client, fhir_oauth,
  fsl_base, FHIR.Ui.Fmx,
  SettingsDialog, FMX.ComboEdit,
  ToolkitSettings;

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
    FSettings: TFHIRToolkitSettings;
    FClient : TFhirClient;
    FVersion: String;
    FSystem: String;
    FHasConcepts: boolean;
    FReplace: boolean;
    FServers : TFslList<TRegisteredFHIRServer>;
    function selectedCount : integer;
    procedure SetExpansion(const Value: TFHIRValueSet);
    procedure SetSettings(const Value: TFHIRToolkitSettings);
    procedure loadServers;
  public
    destructor Destroy; override;

    property Settings : TFHIRToolkitSettings read FSettings write SetSettings;
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
    form.Settings := FSettings.link;
    form.TabControl1.TabIndex := 1;
    if showmodalHack(form) = mrOk then
    begin
      loadServers;
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
  FSettings.Free;
  FExpansion.Free;
  FClient.Free;
  FServers.Free;
  inherited;
end;

procedure TValuesetSelectForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  s : String;
begin
  try
    FSettings.storeValue('ValueSet.Select.Window', 'left', left);
    FSettings.storeValue('ValueSet.Select.Window', 'top', top);
    FSettings.storeValue('ValueSet.Select.Window', 'width', width);
    FSettings.storeValue('ValueSet.Select.Window', 'height', height);

    FSettings.storeValue('ValueSet.Select', 'Property', cbeProperty.Text);
    FSettings.storeValue('ValueSet.Select', 'Operation', cbxOperation.ItemIndex);
    FSettings.storeValue('ValueSet.Select', 'Value', edtValue.Text);
    FSettings.save;
  except
  end;
end;

procedure TValuesetSelectForm.FormShow(Sender: TObject);
begin
  Left := FSettings.getValue('ValueSet.Select.Window', 'left', left);
  Top := FSettings.getValue('ValueSet.Select.Window', 'top', top);
  Width := FSettings.getValue('ValueSet.Select.Window', 'width', width);
  Height := FSettings.getValue('ValueSet.Select.Window', 'height', height);

  loadServers;
  cbxServer.ItemIndex := 0;

  cbeProperty.Text := FSettings.getValue('ValueSet.Select', 'Property', '');
  cbxOperation.ItemIndex := FSettings.getValue('ValueSet.Select', 'Operation', 0);
  edtValue.Text := FSettings.getValue('ValueSet.Select', 'Value', '');
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
    filter.property_ := cbeProperty.Text;
    filter.op := TFhirFilterOperatorEnum(cbxOperation.ItemIndex);
    filter.value := edtValue.Text;

    FExpansion.Free;
    FExpansion := nil;
    gridContains.RowCount := 0;
    btnAppend.Enabled := false;
    btnReplace.Enabled := false;

    if FClient = nil then
      FClient := TFhirClients.makeThreaded(nil, TFhirClients.makeHTTP(nil,
        TRegisteredFHIRServer(cbxServer.ListItems[cbxServer.ItemIndex].Data).fhirEndpoint,
        false, FSettings.timeout * 1000, FSettings.proxy), MasterToolsForm.threadMonitorProc);

    MasterToolsForm.dowork(self, 'Searching', true,
      procedure (context : pointer)
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

procedure TValuesetSelectForm.loadServers;
var
  i : integer;
begin
  if FServers = nil then
    FServers := TFslList<TRegisteredFHIRServer>.create
  else
    FServers.Clear;
  cbxServer.Items.Clear;
  FSettings.ListServers('Terminology', FServers);
  for i := 0 to FServers.Count - 1 do
  begin
    cbxServer.Items.add(FServers[i].name + ': '+FServers[i].fhirEndpoint);
    cbxServer.ListItems[i].Data := FServers[i];
  end;
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


procedure TValuesetSelectForm.SetSettings(const Value: TFHIRToolkitSettings);
begin
  FSettings.Free;
  FSettings := Value;
end;

end.
