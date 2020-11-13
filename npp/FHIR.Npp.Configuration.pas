unit FHIR.Npp.Configuration;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Buttons,
  Vcl.Dialogs, FHIR.Npp.Base, Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
  VirtualTrees, Vcl.ComCtrls, fhir_oauth, FHIR.Client.ServerDialog,
  fsl_base, fhir_objects, fhir_factory,
  fsl_npm_cache,
  FHIR.Npp.Context, FHIR.Npp.Form;

type
  TSettingForm = class(TNppForm)
    od: TOpenDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    edtServerR2: TEdit;
    GroupBox2: TGroupBox;
    TabSheet2: TTabSheet;
    btnEditAsText: TButton;
    Panel3: TPanel;
    vtServers: TVirtualStringTree;
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    btnUp: TButton;
    btnDown: TButton;
    TabSheet3: TTabSheet;
    Panel4: TPanel;
    GroupBox3: TGroupBox;
    cbPathSummary: TCheckBox;
    GroupBox4: TGroupBox;
    cbValidationSummary: TCheckBox;
    Panel5: TPanel;
    Label5: TLabel;
    cbR2: TCheckBox;
    cbR3: TCheckBox;
    cbR4: TCheckBox;
    Label3: TLabel;
    Bevel1: TBevel;
    Label4: TLabel;
    Button3: TButton;
    lblR2Status: TLabel;
    lblR3Status: TLabel;
    lblR4Status: TLabel;
    GroupBox5: TGroupBox;
    chkWelcome: TCheckBox;
    edtServerR3: TEdit;
    Label6: TLabel;
    edtServerR4: TEdit;
    Label7: TLabel;
    Panel6: TPanel;
    Panel7: TPanel;
    cbValidationAnnotations: TCheckBox;
    cbBackgroundValidation: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnEditAsTextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure vtServersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FServers : TFslList<TRegisteredFHIRServer>;
    FVersions: TFHIRVersionFactories;
    FCache : TFHIRPackageManager;
    FContext : TFHIRNppContext;
    procedure LoadServers;
    procedure SetVersions(const Value: TFHIRVersionFactories);
    procedure SetContext(const Value: TFHIRNppContext);
  public
    destructor Destroy; override;
    property versions : TFHIRVersionFactories read FVersions write SetVersions;
    property Context : TFHIRNppContext read FContext write SetContext;
  end;

var
  SettingForm: TSettingForm;

implementation

{$R *.dfm}

uses
  FHIR.Npp.Settings,
  fhir4_constants,
  FHIR.Npp.Plugin,
  FHIR.Npp.Visualiser,
  FHIR.Npp.Toolbox, FHIR.Npm.Manager;

procedure TSettingForm.btnEditAsTextClick(Sender: TObject);
begin
  FNpp.DoOpen(Settings.Filename);
  showmessage('Note that this file is only read at start up, and changes you make will be overwritten by by the plug-in if you make setting changes');
end;

procedure TSettingForm.Button1Click(Sender: TObject);
var
  b, s : String;
begin
  Settings.TerminologyServerR2 := edtServerR2.Text;
  Settings.TerminologyServerR3 := edtServerR3.Text;
  Settings.TerminologyServerR4 := edtServerR4.Text;
  Settings.loadR2 := cbR2.Checked;
  Settings.loadR3 := cbR3.Checked;
  Settings.loadR4 := cbR4.Checked;

  Settings.NoPathSummary := not cbPathSummary.checked;
  Settings.NoValidationSummary := not cbValidationSummary.checked;
  Settings.BackgroundValidation := cbBackgroundValidation.checked;
  Settings.ValidationAnnotations := cbValidationAnnotations.checked;
  Settings.NoWelcomeForm := not chkWelcome.Checked;
  Settings.CommitChanges;

  if FHIRVisualizer <> nil then
    FHIRVisualizer.reregisterAllCDSServers;
end;

procedure TSettingForm.Button2Click(Sender: TObject);
begin
  inherited;
  Settings.AbandonChanges;
end;

procedure TSettingForm.Button3Click(Sender: TObject);
begin
  PackageCacheForm := TPackageCacheForm.Create(self);
  try
    PackageCacheForm.ShowModal;
    if FCache.packageExists('hl7.fhir.r2.core', '1.0') then
      cbR2.Checked := Settings.loadR2
    else
      cbR2.enabled := false;
    if FCache.packageExists('hl7.fhir.r3.core', '3.0') then
      cbR3.Checked := Settings.loadR3
    else
      cbR3.enabled := false;
    if FCache.packageExists('hl7.fhir.r4.core', '4.0') then
      cbR4.Checked := Settings.loadR4
    else
      cbR4.enabled := false;
  finally
    PackageCacheForm.Free;
  end;

end;

destructor TSettingForm.Destroy;
begin
  FContext.Free;
  FServers.Free;
  FVersions.Free;
  FCache.Free;
  inherited;
end;

procedure TSettingForm.btnUpClick(Sender: TObject);
var
  i, c : integer;
  n : PVirtualNode;
  focus : TRegisteredFHIRServer;
  dest : TRegisteredFHIRServer;
begin
  if (vtServers.GetFirstSelected() <> nil) and (vtServers.GetFirstSelected().Index > 0) then
  begin
    c := vtServers.GetFirstSelected().Index;
    focus := FServers[vtServers.GetFirstSelected().Index];
    dest := FServers[vtServers.GetFirstSelected().Index-1];
    Settings.moveServerBefore('', focus, dest);
    LoadServers;
    n := vtServers.RootNode.FirstChild;
    for i := 1 to c - 1 do
      n := n.NextSibling;
    vtServers.Selected[n] := true;
  end;
end;

procedure TSettingForm.btnDeleteClick(Sender: TObject);
var
  i : integer;
begin
  if (vtServers.GetFirstSelected() <> nil) then
  begin
    i := vtServers.GetFirstSelected().Index;
    Settings.deleteServer('', FServers[i]);
    loadServers;
  end;
end;

procedure TSettingForm.btnDownClick(Sender: TObject);
var
  i, c : integer;
  n : PVirtualNode;
  focus : TRegisteredFHIRServer;
  dest : TRegisteredFHIRServer;
begin
  if (vtServers.GetFirstSelected() <> nil) and (vtServers.GetFirstSelected().Index < Settings.ServerCount('') - 1) then
  begin
    c := vtServers.GetFirstSelected().Index;
    focus := FServers[vtServers.GetFirstSelected().Index];
    dest := FServers[vtServers.GetFirstSelected().Index+1];
    Settings.moveServerAfter('', focus, dest);
    LoadServers;
    n := vtServers.RootNode.FirstChild;
    for i := 1 to c + 1 do
      n := n.NextSibling;
    vtServers.Selected[n] := true;
  end;
end;

procedure TSettingForm.btnEditClick(Sender: TObject);
var
  i : integer;
begin
  if (vtServers.GetFirstSelected() <> nil) then
  begin
    i := vtServers.GetFirstSelected().Index;
    EditRegisteredServerForm := TEditRegisteredServerForm.create(npp);
    try
      EditRegisteredServerForm.versions := FVersions.link;
      EditRegisteredServerForm.Server := FServers[i].Link;
      if EditRegisteredServerForm.ShowModal = mrOk then
      begin
        vtServers.RootNodeCount := Settings.ServerCount('');
        vtServers.Invalidate;
      end;
    finally
      EditRegisteredServerForm.Free;
    end;
  end;
end;

procedure TSettingForm.btnAddClick(Sender: TObject);
begin
  EditRegisteredServerForm := TEditRegisteredServerForm.create(npp);
  try
    EditRegisteredServerForm.versions := FVersions.link;
    EditRegisteredServerForm.load;
    if EditRegisteredServerForm.ShowModal = mrOk then
    begin
      loadServers;
      vtServers.RootNodeCount := Settings.ServerCount('');
      vtServers.Invalidate;
    end;
  finally
    EditRegisteredServerForm.Free;
  end;
end;

procedure TSettingForm.FormCreate(Sender: TObject);
begin
  inherited;
  Settings.holdChanges;
  FCache := TFHIRPackageManager.create(true);
end;

procedure TSettingForm.FormShow(Sender: TObject);
var
  s : String;
begin
  edtServerR2.Text := Settings.TerminologyServerR2;
  edtServerR3.Text := Settings.TerminologyServerR3;
  edtServerR4.Text := Settings.TerminologyServerR4;
  if FCache.packageExists('hl7.fhir.r2.core', '1.0') then
    cbR2.Checked := Settings.loadR2
  else
    cbR2.enabled := false;
  if FCache.packageExists('hl7.fhir.r3.core', '3.0') then
    cbR3.Checked := Settings.loadR3
  else
    cbR3.enabled := false;
  if FCache.packageExists('hl7.fhir.r4.core', '4.0') then
    cbR4.Checked := Settings.loadR4
  else
    cbR4.enabled := false;
  lblR2Status.Caption := CODES_TFHIRVersionLoadingStatus[Context.VersionLoading[fhirVersionRelease2]];
  lblR3Status.Caption := CODES_TFHIRVersionLoadingStatus[Context.VersionLoading[fhirVersionRelease3]];
  lblR4Status.Caption := CODES_TFHIRVersionLoadingStatus[Context.VersionLoading[fhirVersionRelease4]];
  vtServers.RootNodeCount := Settings.ServerCount('');
  cbPathSummary.checked := not Settings.NoPathSummary;
  cbValidationSummary.checked := not Settings.NoValidationSummary;
  cbBackgroundValidation.checked := Settings.BackgroundValidation;
  cbValidationAnnotations.checked := Settings.ValidationAnnotations;
  chkWelcome.Checked := not Settings.NoWelcomeForm;
end;

procedure TSettingForm.LoadServers;
begin
  if FServers = nil then
    FServers := TFslList<TRegisteredFHIRServer>.create
  else
    FServers.Clear;
  Settings.ListServers('', FServers);
  vtServers.ClearSelection;
  vtServers.RootNodeCount := FServers.Count;
  vtServers.Invalidate;
end;

procedure TSettingForm.SetContext(const Value: TFHIRNppContext);
begin
  FContext.Free;
  FContext := Value;
end;

procedure TSettingForm.SetVersions(const Value: TFHIRVersionFactories);
begin
  FVersions.Free;
  FVersions := Value;
end;

procedure TSettingForm.vtServersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  server : TRegisteredFHIRServer;
begin
  if FServers = nil then
    loadServers;
  server := FServers[Node.Index];
  case Column of
  0: CellText := server.name;
  1: CellText := server.fhirEndpoint;
  2: CellText := CODES_TFHIRVersion[server.version];
  3: CellText := CODES_TFHIRFormat[server.format];
  4: CellText := CODES_TSmartAppLaunchMode[server.SmartAppLaunchMode];
  5: CellText := server.cdshookSummary;
  end;
end;

end.

