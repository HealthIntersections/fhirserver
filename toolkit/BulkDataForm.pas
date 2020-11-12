unit BulkDataForm;

{
Copyright (c) 2018+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, FMX.Edit, FMX.ListBox, FMX.Controls.Presentation,
  FMX.Ani,
  fsl_utilities, fsl_threads,
  fhir_objects, FHIR.Version.Constants, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Resources.Base, FHIR.Version.Utilities,
  FHIR.Version.Client, fhir_client_async,
  BaseDialog, ToolkitSettings, FMX.Memo.Types;

type
  TForm = TBaseForm;

  TBulkDataDialog = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    rbPatient: TRadioButton;
    rbGroup: TRadioButton;
    cbxPatients: TComboBox;
    cbxGroups: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    edtFolder: TEdit;
    Button1: TButton;
    Panel4: TPanel;
    Label4: TLabel;
    btnExecute: TButton;
    AniIndicator1: TAniIndicator;
    Button3: TButton;
    mLog: TMemo;
    Button4: TButton;
    FloatAnimation1: TFloatAnimation;
    lblProgress: TLabel;
    Timer1: TTimer;
    btnCancel: TButton;
    FloatAnimation2: TFloatAnimation;
    procedure FormShow(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure edtFolderChangeTracking(Sender: TObject);
  private
    FServer: TFHIRClient;
    FSettings: TFHIRToolkitSettings;
    FAsync : TFHIRClientAsyncTask;

    procedure SetServer(const Value: TFHIRClient);
    procedure SetSettings(const Value: TFHIRToolkitSettings);
    procedure loadLookup(combo: TComboBox; resType : TFhirResourceType; params: String);
    procedure loadLookups;
    procedure updateStatus;
  public
    destructor Destroy; override;

    property Server : TFHIRClient read FServer write SetServer;
    property Settings : TFHIRToolkitSettings read FSettings write SetSettings;
  end;

var
  BulkDataDialog: TBulkDataDialog;

implementation

{$R *.fmx}

{ TForm1 }

procedure TBulkDataDialog.btnExecuteClick(Sender: TObject);
var
  rType : TFhirResourceType;
  id : String;
begin
  Settings.storeValue('bulk-data', 'destination', edtFolder.Text);
  if rbPatient.IsChecked then
  begin
    rType := frtPatient;
    if cbxPatients.ItemIndex = 0 then
      id := ''
    else
    begin
      id := cbxPatients.Items[cbxPatients.ItemIndex];
      id := id.Substring(0, id.IndexOf(':'));
    end;
  end
  else
  begin
    rType := frtGroup;
    id := cbxGroups.Items[cbxGroups.ItemIndex];
    id := id.Substring(0, id.IndexOf(':'));
  end;
  Settings.storeValue('bulk-data', 'type', CODES_TFhirResourceType[rType]);
  Settings.storeValue('bulk-data', 'id', id);

  btnExecute.Enabled := false;
  btnCancel.Enabled := true;
  FAsync := TFHIRClientAsyncTask.create(server.link);
  FAsync.format := ffNDJson;
  if (id = '') then
    FAsync.query := CODES_TFhirResourceType[rType]+'/$export'
  else
    FAsync.query := CODES_TFhirResourceType[rType]+'/'+id+'/$export';
  FAsync.folder := edtFolder.Text;
  AniIndicator1.Enabled := true;
end;

procedure TBulkDataDialog.Button1Click(Sender: TObject);
var
  s : String;
begin
  s := edtFolder.Text;
  if FMX.Dialogs.SelectDirectory('Select output Folder', '', s) then
  begin
    edtFolder.Text := s;
    Settings.storeValue('bulk-data', 'destination', edtFolder.Text);
  end;
  updateStatus;
end;

procedure TBulkDataDialog.Button4Click(Sender: TObject);
begin
  cbxPatients.Items.Clear;
  cbxPatients.Items.Add('All Accessible Patients');
  loadLookup(cbxPatients, frtPatient, '_summary=true');
end;

destructor TBulkDataDialog.Destroy;
begin
  FSettings.free;
  FServer.Free;
  FAsync.Free;
  inherited;
end;

procedure TBulkDataDialog.edtFolderChangeTracking(Sender: TObject);
begin
  updateStatus;
end;

procedure TBulkDataDialog.FormShow(Sender: TObject);
begin
  Caption := 'Bulk Data Download from '+Server.address;
  loadLookups;
  edtFolder.Text := Settings.getValue('bulk-data', 'destination', '');

  if Settings.getValue('bulk-data', 'type', 'Patient') = 'Patient' then
  begin
    rbPatient.IsChecked := true;
    if Settings.getValue('bulk-data', 'id', '') = '' then
      cbxPatients.ItemIndex := 0
    else
    begin
      cbxPatients.ItemIndex := cbxPatients.items.IndexOf(Settings.getValue('bulk-data', 'id', ''));
      if cbxPatients.ItemIndex = -1 then
      begin
        cbxPatients.Items.Add(Settings.getValue('bulk-data', 'id', ''));
        cbxPatients.ItemIndex := cbxPatients.Items.Count - 1;
      end;
    end;
  end
  else
  begin
    rbGroup.IsChecked := true;
    cbxGroups.ItemIndex := cbxGroups.items.IndexOf(Settings.getValue('bulk-data', 'id', ''));
  end;
  updateStatus;
end;

procedure TBulkDataDialog.loadLookup(combo: TComboBox; resType : TFhirResourceType; params: String);
var
  bnd : TFHIRBundle;
  be :  TFhirBundleEntry;
begin
  bnd := FServer.search(resType, true, params);
  try
    for be in bnd.entryList do
      if (be.search = nil) or (be.search.mode = SearchEntryModeMatch) then
        combo.Items.Add(be.resource.id+': '+be.resource.textSummary);
  finally
    bnd.Free;
  end;
end;

procedure TBulkDataDialog.loadLookups;
begin
  cbxPatients.Items.Clear;
  cbxPatients.Items.Add('All Accessible Patients');
  cbxGroups.Items.Clear;
  loadLookup(cbxGroups, frtGroup, '_summary=true');
end;

procedure TBulkDataDialog.SetServer(const Value: TFHIRClient);
begin
  FServer.Free;
  FServer := Value;
end;

procedure TBulkDataDialog.SetSettings(const Value: TFHIRToolkitSettings);
begin
  FSettings.free;
  FSettings := Value;
end;

procedure TBulkDataDialog.Timer1Timer(Sender: TObject);
var
  s : String;
begin
  if FAsync <> nil then
  begin
    try
      FAsync.next;
    except
      on e : exception do
      begin
        mLog.Text := 'Error: '+e.Message +#13#10+FAsync.logText;
        AniIndicator1.Enabled := false;
        FASync.free;
        FASync := nil;
        exit;
      end;
    end;
    if FAsync <> nil then
    begin
      lblProgress.Text := FAsync.status;
      lblProgress.Repaint;
      mLog.Text := FAsync.logText;
      if FAsync.Finished then
      begin
        AniIndicator1.Enabled := false;
        s := FASync.Summary;
        FASync.free;
        FASync := nil;
        Timer1.enabled := false;
        MessageDlg(s, TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbok], 0);
      end;
    end;
  end;
end;

procedure TBulkDataDialog.updateStatus;
begin
  btnExecute.enabled := false;
  if edtFolder.Text = '' then
    lblProgress.Text := 'Output folder required. Should be empty'
  else if not FolderExists(edtFolder.Text) then
    lblProgress.Text := 'Output folder not found. Choose one (preferably empty)'
  else if not TDirectory.IsEmpty(edtFolder.Text) then
  begin
    lblProgress.Text := 'Ready to go (though output folder is not empty)';
    btnExecute.enabled := true;
  end
  else
  begin
    lblProgress.Text := 'Ready to go';
    btnExecute.enabled := true;
  end
end;

end.
