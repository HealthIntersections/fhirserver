unit ValidationFrame;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  BaseFrame, FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.ComboEdit, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.Platform,
  fsl_utilities, FHIR.Tools.ValidationWrapper,
  fsl_npm, fsl_npm_cache, FMX.Menus;

type
  TFrame = TBaseFrame; // re-aliasing the Frame to work around a designer bug

  TValidationEngineFrame = class(TFrame)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    cbxVersion: TComboBox;
    chkNative: TCheckBox;
    Panel5: TPanel;
    Label3: TLabel;
    lbPackages: TListBox;
    Panel6: TPanel;
    Label4: TLabel;
    lbOther: TListBox;
    Splitter1: TSplitter;
    Panel7: TPanel;
    Panel8: TPanel;
    btnAdd: TButton;
    btnDelete: TButton;
    Label5: TLabel;
    cbeSource: TComboEdit;
    btnSource: TButton;
    Panel9: TPanel;
    Panel10: TPanel;
    Label6: TLabel;
    btnCopyCommand: TButton;
    lblCommand: TLabel;
    Button5: TButton;
    Button6: TButton;
    btnValidate: TButton;
    Panel11: TPanel;
    mOutput: TMemo;
    Label8: TLabel;
    Splitter2: TSplitter;
    pmSource: TPopupMenu;
    mnuSourceFile: TMenuItem;
    mnuSourceFolder: TMenuItem;
    mnuSourceUrl: TMenuItem;
    dlgOpenFile: TOpenDialog;
    pmAddAsset: TPopupMenu;
    mnuAssetFile: TMenuItem;
    mnuAssetFolder: TMenuItem;
    mnuAssetUrl: TMenuItem;
    cbeProfile: TComboEdit;
    Label7: TLabel;
    cbxTxServer: TComboBox;
    sdFile: TSaveDialog;
    procedure cbxVersionChange(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure mnuAddAssetFileClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure lbOtherClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure lbPackagesChangeCheck(Sender: TObject);
    procedure chkNativeChange(Sender: TObject);
    procedure lbOtherChangeCheck(Sender: TObject);
    procedure btnSourceClick(Sender: TObject);
    procedure cbeSourceChange(Sender: TObject);
    procedure cbeSourceChangeTracking(Sender: TObject);
    procedure mnuSourceFileClick(Sender: TObject);
    procedure mnuSourceFolderClick(Sender: TObject);
    procedure mnuSourceUrlClick(Sender: TObject);
    procedure btnCopyCommandClick(Sender: TObject);
    procedure btnValidateClick(Sender: TObject);
    procedure cbxTxServerChange(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure cbeProfileChange(Sender: TObject);
    procedure cbeProfileChangeTracking(Sender: TObject);
  private
    FLoading, FDirty : boolean;
    FValidator : TFHIRValidationWrapper;
    procedure saveSettings;
    procedure updateparams;
    procedure validatorOutput(Sender : TObject; line : String);
    procedure loadTx;

  public
    destructor Destroy; override;

    procedure SettingsChanged; override;
    procedure load; override;
    function canSave : boolean; override;
    function canSaveAs : boolean; override;
    function isDirty : boolean; override;
    function save : boolean; override;
    function nameForSaveDialog : String; override;
 end;

implementation

{$R *.fmx}

{ TValidationEngineFrame }

procedure TValidationEngineFrame.btnDeleteClick(Sender: TObject);
begin
  lbOther.Items.Delete(lbOther.ItemIndex);
  FDirty := true;
end;

procedure TValidationEngineFrame.btnSourceClick(Sender: TObject);
var
  pt : TPointF;
begin
  pt.X := 0;
  pt.Y := btnSource.Height;
  pt := btnSource.LocalToAbsolute(pt);
  pt := form.ClientToScreen(pt);
  pmSource.Popup(pt.X, pt.Y);
end;

procedure TValidationEngineFrame.btnAddClick(Sender: TObject);
var
  pt : TPointF;
begin
  pt.X := 0;
  pt.Y := btnAdd.Height;
  pt := btnAdd.LocalToAbsolute(pt);
  pt := form.ClientToScreen(pt);
  pmAddAsset.Popup(pt.X, pt.Y);
end;

procedure TValidationEngineFrame.btnCopyCommandClick(Sender: TObject);
var
  Svc: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
    Svc.SetClipboard(lblCommand.Text);
end;

procedure TValidationEngineFrame.btnValidateClick(Sender: TObject);
begin
  // checks: validation package exists + is recent
  // checks: packages exist
  // ask to move any remote references to local packages
  // check java installed
  updateparams;
  mOutput.lines.Clear;
  FValidator.validate;
end;

procedure TValidationEngineFrame.Button5Click(Sender: TObject);
var
  Svc: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
    Svc.SetClipboard(mOutput.Text);
end;

procedure TValidationEngineFrame.Button6Click(Sender: TObject);
begin
  if sdFile.Execute then
    mOutput.Lines.SaveToFile(sdFile.FileName);
end;

procedure TValidationEngineFrame.cbxTxServerChange(Sender: TObject);
begin
  if cbxTxServer.ItemIndex = -1 then
    FValidator.txServer := ''
  else
    FValidator.txServer := cbxTxServer.Items[cbxTxServer.ItemIndex];
  updateparams;
  FDirty := true;
end;

procedure TValidationEngineFrame.cbxVersionChange(Sender: TObject);
var
  s : String;
  i : integer;
begin
  if cbxVersion.ItemIndex = -1 then
  begin
    lbPackages.Clear;
    exit;
  end;

  s := cbxVersion.Items[cbxVersion.ItemIndex];
  s := s.Substring(0, s.IndexOf(' '));
//  Settings.CacheManager.ListPackagesForFhirVersion([fpkIG], true, s, lbPackages.Items);
//  for i := 0 to lbPackages.Items.Count - 1 do
//    lbPackages.ListItems[i].IsChecked := Settings.getValue('Packages', lbPackages.Items[i], false);
  FDirty := true;
  updateParams;
end;

procedure TValidationEngineFrame.chkNativeChange(Sender: TObject);
begin
  FDirty := true;
  updateparams;
end;

function TValidationEngineFrame.canSave: boolean;
begin
  result := true;
end;

function TValidationEngineFrame.canSaveAs: boolean;
begin
  result := false;
end;

procedure TValidationEngineFrame.cbeProfileChange(Sender: TObject);
var
  i : integer;
begin
  i := cbeProfile.Items.IndexOf(cbeProfile.Text);
  if i > -1 then
    cbeProfile.Items.Delete(i);
  cbeProfile.Items.Insert(0, cbeProfile.Text);
  FDirty := true;
  updateparams;
end;

procedure TValidationEngineFrame.cbeProfileChangeTracking(Sender: TObject);
begin
  updateparams;
  FDirty := true;
end;

procedure TValidationEngineFrame.cbeSourceChange(Sender: TObject);
var
  i : integer;
begin
  i := cbeSource.Items.IndexOf(cbeSource.Text);
  if i > -1 then
    cbeSource.Items.Delete(i);
  cbeSource.Items.Insert(0, cbeSource.Text);
  FDirty := true;
  updateparams;
end;

procedure TValidationEngineFrame.cbeSourceChangeTracking(Sender: TObject);
begin
  updateparams;
  FDirty := true;
end;

destructor TValidationEngineFrame.Destroy;
begin
  FValidator.Free;
  inherited;
end;

function TValidationEngineFrame.isDirty: boolean;
begin
  result := FDirty;
end;

procedure TValidationEngineFrame.lbOtherChangeCheck(Sender: TObject);
begin
  FDirty := true;
  updateparams;
end;

procedure TValidationEngineFrame.lbOtherClick(Sender: TObject);
begin
  btnDelete.Enabled := lbOther.ItemIndex > -1;
end;

procedure TValidationEngineFrame.lbPackagesChangeCheck(Sender: TObject);
begin
  updateparams;
  FDirty := true;
end;

procedure TValidationEngineFrame.load;
var
  i : integer;
  ts : TStringList;
begin
  FValidator := TFHIRValidationWrapper.Create(Settings.CacheManager.Link);
  FValidator.OnOutput := validatorOutput;
//  FValidator.txServer := Settings.

  FLoading := true;
  try
    cbxVersion.Items.Clear;
    if Settings.CacheManager.packageExists('hl7.fhir.core', '1.0.2') then
      cbxVersion.Items.Add('1.0.2 (R2)');
    if Settings.CacheManager.packageExists('hl7.fhir.core', '1.4.0') then
      cbxVersion.Items.Add('1.4.0 (May 2016)');
    if Settings.CacheManager.packageExists('hl7.fhir.core', '3.0.1') then
      cbxVersion.Items.Add('3.0.1 (R3)');
    if Settings.CacheManager.packageExists('hl7.fhir.core', '4.0.0') then
      cbxVersion.Items.Add('4.0.0 (R4)');
    if cbxVersion.Items.Count = 0 then
      raise EFHIRException.create('Invalid installation/system - no FHIR packages found');

    cbxVersion.ItemIndex := cbxVersion.Items.IndexOf(Settings.getValue('Validation', 'version', ''));
    if cbxVersion.ItemIndex = -1 then
      cbxVersion.ItemIndex := 0;
    cbxVersionChange(nil);

    chkNative.IsChecked := Settings.getValue('Validation', 'Native', false);
    Settings.getValues('Validation-OtherAssets', lbOther.Items);
    for i := 0 to lbOther.Items.Count - 1 do
      lbOther.ListItems[i].IsChecked := Settings.getValue('Validation-Others', lbOther.items[i], false);
    btnDelete.Enabled := lbOther.ItemIndex > -1;

    Settings.getValues('Validation-source-history', cbeSource.Items);
    cbeSource.Text := Settings.getValue('Validation', 'source', '');
    Settings.getValues('Validation-profile-history', cbeProfile.Items);
    cbeProfile.Text := Settings.getValue('Validation', 'profile', '');

    loadTx();
  finally
    FLoading := false;
  end;
  FDirty := false;
end;

procedure TValidationEngineFrame.loadTx;
begin
  Settings.ListServers('Terminology', cbxTxServer.items);
  cbxTxServer.items.Insert(0, '');
  if (cbxTxServer.Items.Count > 0) then
  begin
    cbxTxServer.ItemIndex := cbxTxServer.items.IndexOf(Settings.getValue('Validation', 'tx', ''));
    if cbxTxServer.ItemIndex = -1 then
      cbxTxServer.ItemIndex := 0;
  end;
  cbxTxServerChange(self);
end;

procedure TValidationEngineFrame.mnuAddAssetFileClick(Sender: TObject);
begin
  if dlgOpenFile.Execute then
    lbOther.Items.Add(dlgOpenFile.FileName);
end;

procedure TValidationEngineFrame.mnuSourceFileClick(Sender: TObject);
begin
  if dlgOpenFile.Execute then
    cbeSource.text := dlgOpenFile.FileName;
end;

procedure TValidationEngineFrame.mnuSourceFolderClick(Sender: TObject);
var
  dir : String;
begin
  if SelectDirectory('Choose Folder', '', dir) then
    cbeSource.text := dir;
end;

procedure TValidationEngineFrame.mnuSourceUrlClick(Sender: TObject);
var
  url : String;
begin
  url := 'https://';
  if InputQuery('Profiles from the Web', 'Enter URL:', url) then
    cbeSource.text := url;
end;

function TValidationEngineFrame.nameForSaveDialog: String;
begin
  result := 'Validation Configuration';
end;

procedure TValidationEngineFrame.MenuItem2Click(Sender: TObject);
var
  dir : String;
begin
  if SelectDirectory('Choose Folder', '', dir) then
    lbOther.Items.Add(dir);
end;

procedure TValidationEngineFrame.MenuItem3Click(Sender: TObject);
var
  url : String;
begin
  url := 'https://';
  if InputQuery('Profiles from the Web', 'Enter URL:', url) then
    lbOther.Items.Add(url);
end;

function TValidationEngineFrame.save: boolean;
begin
  saveSettings;
  result := true;
end;

procedure TValidationEngineFrame.saveSettings;
var
  i : integer;
begin
  if FLoading then
    exit;
  Settings.storeValue('Validation', 'version', cbxVersion.Items[cbxVersion.ItemIndex]);

  Settings.storeValues('Validation-source-history', cbeSource.Items);
  Settings.storeValue('Validation', 'source', cbeSource.Text);
  Settings.storeValues('Validation-profile-history', cbeProfile.Items);
  Settings.storeValue('Validation', 'profile', cbeProfile.Text);

  if cbxTxServer.ItemIndex = -1 then
    Settings.storeValue('Validation', 'tx', '')
  else
    Settings.storeValue('Validation', 'tx', cbxTxServer.Items[cbxTxServer.ItemIndex]);
  Settings.storeValue('Validation', 'Native', chkNative.IsChecked);

  for i := 0 to lbPackages.Items.Count - 1 do
    Settings.storeValue('Packages', lbPackages.Items[i], lbPackages.ListItems[i].IsChecked);

  Settings.storeValues('Validation-OtherAssets', lbOther.Items);
  for i := 0 to lbOther.Items.Count - 1 do
    Settings.storeValue('Validation-Others', lbOther.Items[i], lbOther.ListItems[i].IsChecked);
  Settings.Save;
  FDirty := false;
end;

procedure TValidationEngineFrame.SettingsChanged;
begin
  loadTx;
end;

procedure TValidationEngineFrame.updateparams;
var
  s : String;
  i : integer;
begin
  s := cbxVersion.Items[cbxVersion.ItemIndex];
  s := s.Substring(0, s.IndexOf(' '));
  FValidator.version := s;

  FValidator.source := cbeSource.Text;
  FValidator.profile := cbeProfile.Text;
  FValidator.native := chkNative.IsChecked;
  FValidator.Packages.Clear;
  FValidator.Others.Clear;

  for i := 0 to lbPackages.Items.Count - 1 do
    if lbPackages.ListItems[i].IsChecked then
      FValidator.Packages.add(lbPackages.Items[i]);

  for i := 0 to lbOther.Items.Count - 1 do
    if lbOther.ListItems[i].IsChecked then
      FValidator.Others.add(lbOther.Items[i]);

  lblCommand.Text := FValidator.validateCmd;
end;


procedure TValidationEngineFrame.validatorOutput(Sender: TObject; line: String);
begin
  mOutput.Lines.Add(line);
  Application.ProcessMessages;
end;

end.
