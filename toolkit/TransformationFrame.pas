unit TransformationFrame;

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
  fsl_npm_cache, FMX.Menus;

type
  TFrame = TBaseFrame; // re-aliasing the Frame to work around a designer bug

  TTransformationEngineFrame = class(TFrame)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
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
    btnTransform: TButton;
    Panel11: TPanel;
    mOutput: TMemo;
    Label8: TLabel;
    Splitter2: TSplitter;
    pmSource: TPopupMenu;
    mnuSourceFile: TMenuItem;
    mnuSourceUrl: TMenuItem;
    dlgOpenFile: TOpenDialog;
    pmAddAsset: TPopupMenu;
    mnuAssetFile: TMenuItem;
    mnuAssetFolder: TMenuItem;
    mnuAssetUrl: TMenuItem;
    cbeMap: TComboEdit;
    Label7: TLabel;
    cbxTxServer: TComboBox;
    sdFile: TSaveDialog;
    Label9: TLabel;
    cbeDest: TComboEdit;
    Button1: TButton;
    sdDest: TSaveDialog;
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
    procedure cbeDestChange(Sender: TObject);
    procedure cbeSourceChange(Sender: TObject);
    procedure mnuSourceFileClick(Sender: TObject);
    procedure mnuSourceFolderClick(Sender: TObject);
    procedure mnuSourceUrlClick(Sender: TObject);
    procedure btnCopyCommandClick(Sender: TObject);
    procedure btnTransformClick(Sender: TObject);
    procedure cbxTxServerChange(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure cbeDestChangeTracking(Sender: TObject);
    procedure cbeSourceChangeTracking(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbeMapChange(Sender: TObject);
    procedure cbeMapChangeTracking(Sender: TObject);
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

procedure TTransformationEngineFrame.btnDeleteClick(Sender: TObject);
begin
  lbOther.Items.Delete(lbOther.ItemIndex);
  FDirty := true;
end;

procedure TTransformationEngineFrame.btnSourceClick(Sender: TObject);
var
  pt : TPointF;
begin
  pt.X := 0;
  pt.Y := btnSource.Height;
  pt := btnSource.LocalToAbsolute(pt);
  pt := form.ClientToScreen(pt);
  pmSource.Popup(pt.X, pt.Y);
end;

procedure TTransformationEngineFrame.btnAddClick(Sender: TObject);
var
  pt : TPointF;
begin
  pt.X := 0;
  pt.Y := btnAdd.Height;
  pt := btnAdd.LocalToAbsolute(pt);
  pt := form.ClientToScreen(pt);
  pmAddAsset.Popup(pt.X, pt.Y);
end;

procedure TTransformationEngineFrame.btnCopyCommandClick(Sender: TObject);
var
  Svc: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
    Svc.SetClipboard(lblCommand.Text);
end;

procedure TTransformationEngineFrame.btnTransformClick(Sender: TObject);
begin
  // checks: validation package exists + is recent
  // checks: packages exist
  // ask to move any remote references to local packages
  // check java installed
  updateparams;
  mOutput.lines.Clear;
  FValidator.transform;
end;

procedure TTransformationEngineFrame.Button1Click(Sender: TObject);
begin
  if sdDest.Execute then
    cbeDest.Text := sdDest.FileName;
end;

procedure TTransformationEngineFrame.Button5Click(Sender: TObject);
var
  Svc: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
    Svc.SetClipboard(mOutput.Text);
end;

procedure TTransformationEngineFrame.Button6Click(Sender: TObject);
begin
  if sdFile.Execute then
    mOutput.Lines.SaveToFile(sdFile.FileName);
end;

procedure TTransformationEngineFrame.cbxTxServerChange(Sender: TObject);
begin
  if cbxTxServer.ItemIndex = -1 then
    FValidator.txServer := ''
  else
    FValidator.txServer := cbxTxServer.Items[cbxTxServer.ItemIndex];
  updateparams;
  FDirty := true;
end;

procedure TTransformationEngineFrame.chkNativeChange(Sender: TObject);
begin
  FDirty := true;
  updateparams;
end;

function TTransformationEngineFrame.canSave: boolean;
begin
  result := true;
end;

function TTransformationEngineFrame.canSaveAs: boolean;
begin
  result := false;
end;

procedure TTransformationEngineFrame.cbeDestChange(Sender: TObject);
var
  i : integer;
begin
  i := cbeDest.Items.IndexOf(cbeDest.Text);
  if i > -1 then
    cbeDest.Items.Delete(i);
  cbeDest.Items.Insert(0, cbeDest.Text);
  FDirty := true;
  updateparams;
end;

procedure TTransformationEngineFrame.cbeDestChangeTracking(Sender: TObject);
begin
  updateparams;
  FDirty := true;
end;

procedure TTransformationEngineFrame.cbeMapChange(Sender: TObject);
var
  i : integer;
begin
  i := cbeMap.Items.IndexOf(cbeMap.Text);
  if i > -1 then
    cbeMap.Items.Delete(i);
  cbeMap.Items.Insert(0, cbeMap.Text);
  FDirty := true;
  updateparams;
end;

procedure TTransformationEngineFrame.cbeMapChangeTracking(Sender: TObject);
begin
  updateparams;
  FDirty := true;
end;

procedure TTransformationEngineFrame.cbeSourceChange(Sender: TObject);
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

procedure TTransformationEngineFrame.cbeSourceChangeTracking(Sender: TObject);
begin
  updateparams;
  FDirty := true;
end;

destructor TTransformationEngineFrame.Destroy;
begin
  FValidator.Free;
  inherited;
end;

function TTransformationEngineFrame.isDirty: boolean;
begin
  result := FDirty;
end;

procedure TTransformationEngineFrame.lbOtherChangeCheck(Sender: TObject);
begin
  FDirty := true;
  updateparams;
end;

procedure TTransformationEngineFrame.lbOtherClick(Sender: TObject);
begin
  btnDelete.Enabled := lbOther.ItemIndex > -1;
end;

procedure TTransformationEngineFrame.lbPackagesChangeCheck(Sender: TObject);
begin
  updateparams;
  FDirty := true;
end;

procedure TTransformationEngineFrame.load;
var
  i : integer;
  ts : TStringList;
begin
  FValidator := TFHIRValidationWrapper.Create(Settings.CacheManager.Link);
  FValidator.OnOutput := validatorOutput;

  FLoading := true;
  try
    Settings.getValues('Transformation-OtherAssets', lbOther.Items);
    for i := 0 to lbOther.Items.Count - 1 do
      lbOther.ListItems[i].IsChecked := Settings.getValue('Transformation-Others', lbOther.items[i], false);
    btnDelete.Enabled := lbOther.ItemIndex > -1;

    Settings.getValues('Transformation-source-history', cbeSource.Items);
    cbeSource.Text := Settings.getValue('Transformation', 'source', '');
    Settings.getValues('Transformation-map-history', cbeMap.Items);
    cbeMap.Text := Settings.getValue('Transformation', 'map', '');
    Settings.getValues('Transformation-dest-history', cbeDest.Items);
    cbeDest.Text := Settings.getValue('Transformation', 'dest', '');

    loadTx();
  finally
    FLoading := false;
  end;
  FDirty := false;
end;

procedure TTransformationEngineFrame.loadTx;
begin
  Settings.ListServers('Terminology', cbxTxServer.items);
  cbxTxServer.items.Insert(0, '');
  if (cbxTxServer.Items.Count > 0) then
  begin
    cbxTxServer.ItemIndex := cbxTxServer.items.IndexOf(Settings.getValue('Transformation', 'tx', ''));
    if cbxTxServer.ItemIndex = -1 then
      cbxTxServer.ItemIndex := 0;
  end;
  cbxTxServerChange(self);
end;

procedure TTransformationEngineFrame.mnuAddAssetFileClick(Sender: TObject);
begin
  if dlgOpenFile.Execute then
    lbOther.Items.Add(dlgOpenFile.FileName);
end;

procedure TTransformationEngineFrame.mnuSourceFileClick(Sender: TObject);
begin
  if dlgOpenFile.Execute then
    cbeSource.text := dlgOpenFile.FileName;
end;

procedure TTransformationEngineFrame.mnuSourceFolderClick(Sender: TObject);
var
  dir : String;
begin
  if SelectDirectory('Choose Folder', '', dir) then
    cbeSource.text := dir;
end;

procedure TTransformationEngineFrame.mnuSourceUrlClick(Sender: TObject);
var
  url : String;
begin
  url := 'https://';
  if InputQuery('Profiles from the Web', 'Enter URL:', url) then
    cbeSource.text := url;
end;

function TTransformationEngineFrame.nameForSaveDialog: String;
begin
  result := 'Transformation Configuration';
end;

procedure TTransformationEngineFrame.MenuItem2Click(Sender: TObject);
var
  dir : String;
begin
  if SelectDirectory('Choose Folder', '', dir) then
    lbOther.Items.Add(dir);
end;

procedure TTransformationEngineFrame.MenuItem3Click(Sender: TObject);
var
  url : String;
begin
  url := 'https://';
  if InputQuery('Profiles from the Web', 'Enter URL:', url) then
    lbOther.Items.Add(url);
end;

function TTransformationEngineFrame.save: boolean;
begin
  saveSettings;
  result := true;
end;

procedure TTransformationEngineFrame.saveSettings;
var
  i : integer;
begin
  if FLoading then
    exit;
  Settings.storeValues('Transformation-source-history', cbeSource.Items);
  Settings.storeValue('Transformation', 'source', cbeSource.Text);
  Settings.storeValues('Transformation-map-history', cbeMap.Items);
  Settings.storeValue('Transformation', 'map', cbeMap.Text);
  Settings.storeValues('Transformation-dest-history', cbeDest.Items);
  Settings.storeValue('Transformation', 'dest', cbeDest.Text);

  if cbxTxServer.ItemIndex = -1 then
    Settings.storeValue('Transformation', 'tx', '')
  else
    Settings.storeValue('Transformation', 'tx', cbxTxServer.Items[cbxTxServer.ItemIndex]);

  for i := 0 to lbPackages.Items.Count - 1 do
    Settings.storeValue('Packages', lbPackages.Items[i], lbPackages.ListItems[i].IsChecked);

  Settings.storeValues('Transformation-OtherAssets', lbOther.Items);
  for i := 0 to lbOther.Items.Count - 1 do
    Settings.storeValue('Transformation-Others', lbOther.Items[i], lbOther.ListItems[i].IsChecked);
  Settings.Save;
  FDirty := false;
end;

procedure TTransformationEngineFrame.SettingsChanged;
begin
  loadTx;
end;

procedure TTransformationEngineFrame.updateparams;
var
  i : integer;
begin
  FValidator.version := '3.4.0';

  FValidator.source := cbeSource.Text;
  FValidator.map := cbeMap.Text;
  FValidator.dest := cbeDest.Text;
  FValidator.Packages.Clear;
  FValidator.Others.Clear;

  for i := 0 to lbPackages.Items.Count - 1 do
    if lbPackages.ListItems[i].IsChecked then
      FValidator.Packages.add(lbPackages.Items[i]);

  for i := 0 to lbOther.Items.Count - 1 do
    if lbOther.ListItems[i].IsChecked then
      FValidator.Others.add(lbOther.Items[i]);

  lblCommand.Text := FValidator.transformCmd;
end;


procedure TTransformationEngineFrame.validatorOutput(Sender: TObject; line: String);
begin
  mOutput.Lines.Add(line);
  Application.ProcessMessages;
end;

end.
