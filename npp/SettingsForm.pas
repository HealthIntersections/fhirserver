unit SettingsForm;

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
  Vcl.Dialogs, NppForms, Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
  VirtualTrees, Vcl.ComCtrls, SmartOnFhirUtilities, EditRegisteredServerDialog;

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
    edtServer: TEdit;
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
    GroupBox5: TGroupBox;
    rbR3: TRadioButton;
    rbR2: TRadioButton;
    Panel5: TPanel;
    Label5: TLabel;
    lbAdditional: TListBox;
    btnAddIG: TButton;
    btnDeleteIG: TButton;
    Label3: TLabel;
    Button3: TButton;
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
    procedure btnAddIGClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SettingForm: TSettingForm;

implementation

{$R *.dfm}

uses
  FHIRPluginSettings,
  FHIRConstants,
  FHIRPlugin,
  FHIRVisualiser,
  FHIRToolboxForm;

procedure TSettingForm.btnEditAsTextClick(Sender: TObject);
begin
  FNpp.DoOpen(Settings.Filename);
  showmessage('Note that this file is only read at start up, and changes you make will be overwritten by by the plug-in if you make setting changes');
end;

procedure TSettingForm.Button1Click(Sender: TObject);
var
  b, s : String;
begin
  Settings.TerminologyServer := edtServer.Text;
  if rbR2.Checked then
    Settings.DefinitionsVersion := defV2
  else
    Settings.DefinitionsVersion := defV3;

  b := '';
  for s in lbAdditional.Items do
    if b = '' then
      b := s
    else
      b := b +','+s;
  Settings.AdditionalDefinitions := s;
  Settings.NoPathSummary := not cbPathSummary.checked;
  Settings.NoValidationSummary := not cbValidationSummary.checked;
  Settings.CommitChanges;
  _FuncDisconnect;

  if FHIRToolbox <> nil then
    FHIRToolbox.loadServers;
  if FHIRVisualizer <> nil then
    FHIRVisualizer.reregisterAllCDSServers;
end;

procedure TSettingForm.Button2Click(Sender: TObject);
begin
  inherited;
  Settings.AbandonChanges;
end;

procedure TSettingForm.btnUpClick(Sender: TObject);
var
  i, c : integer;
  n : PVirtualNode;
begin
  if (vtServers.GetFirstSelected() <> nil) and (vtServers.GetFirstSelected().Index > 0) then
  begin
    c := vtServers.GetFirstSelected().Index;
    Settings.moveServer('', c, -1);
    vtServers.Invalidate;
    vtServers.ClearSelection;
    n := vtServers.RootNode.FirstChild;
    for i := 1 to c - 1 do
      n := n.NextSibling;
    vtServers.Selected[n] := true;
  end;
end;

procedure TSettingForm.btnAddIGClick(Sender: TObject);
var
  od : TFileOpenDialog;
begin
  od := TFileOpenDialog.Create(nil);
  try
    if od.Execute then
      lbAdditional.Items.Add(od.FileName);
  finally
    od.Free;
  end;
end;

procedure TSettingForm.btnDeleteClick(Sender: TObject);
var
  i : integer;
begin
  if (vtServers.GetFirstSelected() <> nil) then
  begin
    i := vtServers.GetFirstSelected().Index;
    Settings.deleteServer('', i);
    vtServers.RootNodeCount := Settings.ServerCount('');
    vtServers.Invalidate;
  end;
end;

procedure TSettingForm.btnDownClick(Sender: TObject);
var
  i, c : integer;
  n : PVirtualNode;
begin
  if (vtServers.GetFirstSelected() <> nil) and (vtServers.GetFirstSelected().Index < Settings.ServerCount('') - 1) then
  begin
    c := vtServers.GetFirstSelected().Index;
    Settings.moveServer('', c, +2); // because +1 is immediately after, so no change
    vtServers.Invalidate;
    vtServers.ClearSelection;
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
      EditRegisteredServerForm.loadFrom(i);
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
    if EditRegisteredServerForm.ShowModal = mrOk then
    begin
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
end;

procedure TSettingForm.FormShow(Sender: TObject);
var
  s : String;
begin
  edtServer.Text := Settings.TerminologyServer;
  if settings.DefinitionsVersion = defV2 then
    rbR2.Checked := true
  else
    rbr3.Checked := true;
  vtServers.RootNodeCount := Settings.ServerCount('');
  cbPathSummary.checked := not Settings.NoPathSummary;
  cbValidationSummary.checked := not Settings.NoValidationSummary;
  for s in Settings.AdditionalDefinitions.Split([',']) do
    lbAdditional.Items.Add(s);
end;

procedure TSettingForm.vtServersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  server : TRegisteredFHIRServer;
begin
  server := Settings.serverInfo('', Node.Index);
  try
    case Column of
    0: CellText := server.name;
    1: CellText := server.fhirEndpoint;
    2: CellText := CODES_TSmartAppLaunchMode[server.SmartAppLaunchMode];
    3: CellText := server.cdshookSummary;
    end;
  finally
    server.free;
  end;
end;

end.

