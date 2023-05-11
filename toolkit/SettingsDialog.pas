unit SettingsDialog;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyrigh, FMX.Layouts,
  FMX.ListBoxt notice, this
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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.EditBox,
  FMX.SpinBox, FMX.Edit, FMX.StdCtrls, FMX.TabControl, FMX.Controls.Presentation,
  IniFiles, System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.Layouts, FMX.ListBox,
  fsl_base, FHIR.Ui.Fmx,
  ToolkitSettings,
  FHIR.Client.ServerDialogFMX, fhir_oauth;

type
  TSettingsForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    Label1: TLabel;
    Label2: TLabel;
    edtProxy: TEdit;
    edtTimeout: TSpinBox;
    Label3: TLabel;
    Label4: TLabel;
    TabItem2: TTabItem;
    btnAdd: TButton;
    btnUp: TButton;
    btnDown: TButton;
    btnDelete: TButton;
    lbServers: TListBox;
    btnEdit: TButton;
    TabItem3: TTabItem;
    cbCheckUpgrades: TCheckBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    edtUsername: TEdit;
    edtPassword: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure lbServersClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
  private
    FSettings :  TFHIRToolkitSettings;
    FServers : TFslList<TRegisteredFHIRServer>;
    procedure SetSettings(const Value: TFHIRToolkitSettings);
    procedure LoadServers;
  public
    destructor Destroy; override;
    Property Settings : TFHIRToolkitSettings read FSettings write SetSettings;
  end;

var
  SettingsForm: TSettingsForm;

implementation

{$R *.fmx}

procedure TSettingsForm.btnAddClick(Sender: TObject);
var
  form : TEditRegisteredServerForm;
begin
  form := TEditRegisteredServerForm.create(self);
  try
    form.Server := TRegisteredFHIRServer.Create;
    form.Versions := Settings.Versions.link;
    if ShowModalHack(form) = mrOk then
    begin
      FSettings.registerServer('Terminology', form.Server);
      LoadServers;
      lbServersClick(nil);
    end;
  finally
    form.Free;
  end;
end;

procedure TSettingsForm.btnDeleteClick(Sender: TObject);
var
  i : integer;
begin
  i := lbServers.ItemIndex;
  FSettings.DeleteServer('Terminology', TRegisteredFHIRServer(lbServers.ListItems[lbServers.ItemIndex].Data));
  lbServers.items.Delete(i);
  if i = lbServers.items.Count then
    dec(i);
  lbServers.ItemIndex := i;
  lbServersClick(nil);
end;

procedure TSettingsForm.btnDownClick(Sender: TObject);
var
  focus : TRegisteredFHIRServer;
  dest : TRegisteredFHIRServer;
begin
  focus := TRegisteredFHIRServer(lbServers.ListItems[lbServers.ItemIndex].Data);
  dest := TRegisteredFHIRServer(lbServers.ListItems[lbServers.ItemIndex+1].Data);
  FSettings.moveServerAfter('Terminology', focus, dest);
  LoadServers;
  lbServers.ItemIndex := lbServers.ItemIndex+1;
  lbServersClick(nil);
end;

procedure TSettingsForm.btnUpClick(Sender: TObject);
var
  focus : TRegisteredFHIRServer;
  dest : TRegisteredFHIRServer;
begin
  focus := TRegisteredFHIRServer(lbServers.ListItems[lbServers.ItemIndex].Data);
  dest := TRegisteredFHIRServer(lbServers.ListItems[lbServers.ItemIndex-1].Data);
  FSettings.moveServerBefore('Terminology', focus, dest);
  LoadServers;
  lbServers.ItemIndex := lbServers.ItemIndex-1;
  lbServersClick(nil);
end;

procedure TSettingsForm.btnEditClick(Sender: TObject);
var
  i : integer;
  form : TEditRegisteredServerForm;
begin
  form := TEditRegisteredServerForm.create(self);
  try
    form.Server := TRegisteredFHIRServer(lbServers.ListItems[lbServers.ItemIndex].Data).Link;
    if ShowModalHack(form) = mrOk then
    begin
      FSettings.updateServerInfo('Terminology', form.Server);
      LoadServers;
      lbServersClick(nil);
    end;
  finally
    form.Free;
  end;
end;

destructor TSettingsForm.Destroy;
begin
  FSettings.Free;
  FServers.Free;
  inherited;
end;

procedure TSettingsForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  s : String;
begin
  if ModalResult = mrOk then
  begin
    FSettings.Proxy := edtProxy.Text;
    FSettings.timeout := trunc(edtTimeout.Value);
    FSettings.CheckForUpgradesOnStart := cbCheckUpgrades.IsChecked;
    FSettings.RegistryUsername := edtUsername.Text;
    FSettings.RegistryPassword := edtPassword.Text;
  end;
end;

procedure TSettingsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := true;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItem1;
  edtProxy.Text := FSettings.Proxy;
  edtTimeout.Value := FSettings.timeout;
  cbCheckUpgrades.IsChecked := FSettings.CheckForUpgradesOnStart;
  LoadServers;
  lbServers.ItemIndex := 0;
  lbServersClick(nil);
  edtUsername.Text := FSettings.RegistryUsername;
  edtPassword.Text := FSettings.RegistryPassword;
end;

procedure TSettingsForm.lbServersClick(Sender: TObject);
begin
  btnEdit.Enabled := lbServers.ItemIndex >= 0;
  btnAdd.enabled := true;
  btnDown.enabled := (lbServers.ItemIndex > -1) and (lbServers.ItemIndex < FSettings.ServerCount('Terminology') - 1);
  btnUp.enabled := lbServers.ItemIndex > 0;
  btnDelete.enabled := (lbServers.ItemIndex >= 0) and (FSettings.ServerCount('Terminology') > 1);
end;

procedure TSettingsForm.LoadServers;
var
  i : integer;
begin
  if FServers = nil then
    FServers := TFslList<TRegisteredFHIRServer>.create
  else
    FServers.Clear;
  lbServers.Items.Clear;
  FSettings.ListServers('Terminology', FServers);
  for i := 0 to FServers.Count - 1 do
  begin
    lbServers.Items.add(FServers[i].name + ': '+FServers[i].fhirEndpoint);
    lbServers.ListItems[i].Data := FServers[i];
  end;
end;

procedure TSettingsForm.SetSettings(const Value: TFHIRToolkitSettings);
begin
  FSettings.Free;
  FSettings := Value;
end;

end.
