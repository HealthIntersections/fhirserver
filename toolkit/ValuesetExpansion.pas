unit ValuesetExpansion;

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
  FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.StdCtrls, FMX.DateTimeCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.ListBox, FMX.Platform,
  fsl_base, fsl_stream, FHIR.Ui.Fmx,
  fhir_objects, 
  FHIR.Version.Resources, FHIR.Version.Resources.Base, FHIR.Version.Utilities, FHIR.Version.Client, fhir_oauth,
  SettingsDialog, BaseFrame, ProcessForm,
  ToolkitSettings;

type
  TValuesetExpansionForm = class(TForm)
    Panel1: TPanel;
    btnUse: TButton;
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
    btnExport: TButton;
    dlgExport: TSaveDialog;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GoClick(Sender: TObject);
    procedure gridContainsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
  private
    FValueSet: TFHIRValueSet;
    FExpansion : TFHIRValueSet;
    FSettings : TFHIRToolkitSettings;
    FClient : TFhirClient;
    FIsStopped : boolean;
    FServers : TFslList<TRegisteredFHIRServer>;
    procedure SetValueSet(const Value: TFHIRValueSet);
    procedure SetExpansion(const Value: TFHIRValueSet);
    procedure SetSettings(const Value: TFHIRToolkitSettings);
    procedure dowork(Sender : TObject; opName : String; canCancel : boolean; proc : TWorkProc);
    function GetStopped(context : pointer): boolean;
    procedure btnStopClick(Sender: TObject);
    procedure loadServers;
  public
    destructor Destroy; override;
    property ValueSet : TFHIRValueSet read FValueSet write SetValueSet;
    property Expansion : TFHIRValueSet read FExpansion write SetExpansion;
    property Settings : TFHIRToolkitSettings read FSettings write SetSettings;
  end;

var
  ValuesetExpansionForm: TValuesetExpansionForm;

implementation

{$R *.fmx}

Uses
  FHIRToolkitForm;



{ TValuesetExpansionForm }

procedure TValuesetExpansionForm.btnExportClick(Sender: TObject);
var
  s : String;
begin
  if dlgExport.Execute then
  begin
    s := ExtractFileExt(dlgExport.FileName);
    if s = '.xml' then
      resourceToFile(FValueSet, dlgExport.FileName, ffXml)
    else if s = '.json' then
      resourceToFile(FValueSet, dlgExport.FileName, ffJson)
    else if s = '.ttl' then
      resourceToFile(FValueSet, dlgExport.FileName, ffJson)
    else if s = '.csv' then
      produceCsv(dlgExport.FileName,
        ['system', 'abstract', 'inactive', 'version', 'code', 'display'],
        procedure (csv : TCSVWriter)
        var
          c : TFhirValueSetExpansionContains;
        begin
          for c in FValueSet.expansion.containsList do
          begin
            csv.cell(c.system);
            csv.cell(c.abstract);
            csv.cell(c.inactive);
            csv.cell(c.version);
            csv.cell(c.code);
            csv.cell(c.display);
            csv.line;
          end;
        end
      )
    else
      raise EFHIRException.create('Unknown format');
  end;
end;

procedure TValuesetExpansionForm.btnSettingsClick(Sender: TObject);
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

procedure TValuesetExpansionForm.btnStopClick(Sender: TObject);
begin
  FIsStopped := true;
end;

destructor TValuesetExpansionForm.Destroy;
begin
  FSettings.Free;
  FExpansion.Free;
  FValueSet.Free;
  FClient.Free;
  FServers.free;
  inherited;
end;

procedure TValuesetExpansionForm.dowork(Sender: TObject; opName: String; canCancel: boolean; proc: TWorkProc);
var
  fcs : IFMXCursorService;
  form : TProcessingForm;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    fcs := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService
  else
    fcs := nil;
  if Assigned(fcs) then
  begin
    Cursor := fcs.GetCursor;
    fcs.SetCursor(crHourGlass);
  end;
  try
    FIsStopped := false;
    if assigned(sender) and (sender is TBaseFrame) then
      TBaseFrame(sender).OnStopped := GetStopped;
    form := TProcessingForm.Create(self);
    try
      form.lblOperation.text := opName;
      form.Button1.enabled := canCancel;
      form.Button1.OnClick := btnStopClick;
      form.proc := proc;
      ShowModalHack(form);
    finally
      form.Free;
    end;
  finally
    if Assigned(fCS) then
      fcs.setCursor(Cursor);
  end;
end;

procedure TValuesetExpansionForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  s : String;
begin
  try
    FSettings.storeValue('Expansion.Window', 'left', left);
    FSettings.storeValue('Expansion.Window', 'top', top);
    FSettings.storeValue('Expansion.Window', 'width', width);
    FSettings.storeValue('Expansion.Window', 'height', height);

    FSettings.storeValue('Expansion', 'Filter', edtFilter.Text);
    FSettings.storeValue('Expansion', 'Date', dedDate.Text);
    FSettings.storeValue('Expansion', 'Offset', edtOffset.Text);
    FSettings.storeValue('Expansion', 'Count', edtCount.Text);
    FSettings.storeValue('Expansion', 'Lang', edtLang.Text);
    FSettings.storeValue('Expansion', 'Designations', cbDesignations.IsChecked);
    FSettings.storeValue('Expansion', 'ActiveOnly', cbActiveOnly.IsChecked);
    FSettings.storeValue('Expansion', 'UIOnly', cbUIOnly.IsChecked);
    FSettings.storeValue('Expansion', 'NoExpressions', cbNoExpressions.IsChecked);
    FSettings.storeValue('Expansion', 'AllowSubset', cbAllowSubset.IsChecked);

    FSettings.storeValue('Expansion', 'width-col-1', trunc(StringColumn10.Width));
    FSettings.storeValue('Expansion', 'width-col-1', trunc(CheckColumn1.Width));
    FSettings.storeValue('Expansion', 'width-col-1', trunc(CheckColumn2.Width));
    FSettings.storeValue('Expansion', 'width-col-1', trunc(StringColumn11.Width));
    FSettings.storeValue('Expansion', 'width-col-1', trunc(StringColumn12.Width));
    FSettings.storeValue('Expansion', 'width-col-1', trunc(StringColumn13.Width));
    FSettings.Save;
  except
  end;
end;

procedure TValuesetExpansionForm.FormShow(Sender: TObject);
begin
  Left := FSettings.getValue('Expansion.Window', 'left', left);
  Top := FSettings.getValue('Expansion.Window', 'top', top);
  Width := FSettings.getValue('Expansion.Window', 'width', width);
  Height := FSettings.getValue('Expansion.Window', 'height', height);

  loadServers;
  cbxServer.ItemIndex := 0;

  edtFilter.Text := FSettings.getValue('Expansion', 'Filter', '');
  dedDate.Text := FSettings.getValue('Expansion', 'Date', '');
  edtOffset.Text := FSettings.getValue('Expansion', 'Offset', '');
  edtCount.Text := FSettings.getValue('Expansion', 'Count', '');
  edtLang.Text := FSettings.getValue('Expansion', 'Lang', '');
  cbDesignations.IsChecked := FSettings.getValue('Expansion', 'Designations', false);
  cbActiveOnly.IsChecked := FSettings.getValue('Expansion', 'ActiveOnly', false);
  cbUIOnly.IsChecked := FSettings.getValue('Expansion', 'UIOnly', false);
  cbNoExpressions.IsChecked := FSettings.getValue('Expansion', 'NoExpressions', false);
  cbAllowSubset.IsChecked := FSettings.getValue('Expansion', 'AllowSubset', false);

  StringColumn10.Width := FSettings.getValue('Expansion', 'width-col-1', trunc(StringColumn10.Width));
  CheckColumn1.Width := FSettings.getValue('Expansion', 'width-col-1', trunc(CheckColumn1.Width));
  CheckColumn2.Width := FSettings.getValue('Expansion', 'width-col-1', trunc(CheckColumn2.Width));
  StringColumn11.Width := FSettings.getValue('Expansion', 'width-col-1', trunc(StringColumn11.Width));
  StringColumn12.Width := FSettings.getValue('Expansion', 'width-col-1', trunc(StringColumn12.Width));
  StringColumn13.Width := FSettings.getValue('Expansion', 'width-col-1', trunc(StringColumn13.Width));
end;

function TValuesetExpansionForm.GetStopped(context : pointer): boolean;
begin

end;

procedure TValuesetExpansionForm.GoClick(Sender: TObject);
var
  params :  TFHIRParameters;
begin
  FExpansion.Free;
  FExpansion := nil;
  gridContains.RowCount := 0;
  btnUse.Enabled := false;
  btnExport.Enabled := false;

  if FClient = nil then
    FClient := TFhirClients.makeThreaded(nil, TFhirClients.makeHTTP(nil,
     TRegisteredFHIRServer(cbxServer.ListItems[cbxServer.ItemIndex].Data).fhirEndpoint,
     false, FSettings.timeout * 1000, FSettings.proxy), MasterToolsForm.threadMonitorProc);

  dowork(self, 'Expanding', true,
    procedure (context : pointer)
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
        btnUse.Enabled := FExpansion.expansion.containsList.Count > 0;
        btnExport.Enabled := FExpansion.expansion.containsList.Count > 0;
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

procedure TValuesetExpansionForm.loadServers;
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

procedure TValuesetExpansionForm.SetExpansion(const Value: TFHIRValueSet);
begin
  FExpansion.Free;
  FExpansion := Value;
end;

procedure TValuesetExpansionForm.SetSettings(const Value: TFHIRToolkitSettings);
begin
  FSettings.Free;
  FSettings := Value;
end;

procedure TValuesetExpansionForm.SetValueSet(const Value: TFHIRValueSet);
begin
  FValueSet.Free;
  FValueSet := Value;
end;

end.
