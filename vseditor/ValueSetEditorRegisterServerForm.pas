unit ValueSetEditorRegisterServerForm;

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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, VirtualTrees, ValueSetEditorCore,
  NewServerForm;

type
  TfrmRegisterServer = class(TForm)
    Panel1: TPanel;
    btnOpenFile: TButton;
    Panel2: TPanel;
    tvServers: TVirtualStringTree;
    btnUpdate: TButton;
    Panel3: TPanel;
    Button1: TButton;
    btnDelete: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvServersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure btnUpdateClick(Sender: TObject);
    procedure tvServersClick(Sender: TObject);
    procedure tvServersInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvServersChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure Button1Click(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure tvServersAddToSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure tvServersRemoveFromSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure tvServersDblClick(Sender: TObject);
  private
    FContext : TValueSetEditorContext;
    procedure SetContext(const Value: TValueSetEditorContext);
  public
    { Public declarations }
    property Context : TValueSetEditorContext read FContext write SetContext;
  end;

var
  frmRegisterServer: TfrmRegisterServer;

implementation

uses
  ServerOperationForm;

{$R *.dfm}

procedure TfrmRegisterServer.btnUpdateClick(Sender: TObject);
var
  server : TValueSetEditorServerCache;
begin
  if (tvServers.FocusedNode <> nil) then
  begin
    server := FContext.Servers[tvServers.FocusedNode.Index];
    ServerOperation(server.update, '', 'updating', true);
  end;
end;

procedure TfrmRegisterServer.Button1Click(Sender: TObject);
var
  msg : String;
begin
  frmNewServer.Context := Context.Link;
  if frmNewServer.ShowModal = mrOk then
  begin
    Context.AddServer(frmNewServer.edtName.Text, frmNewServer.edtAddress.Text, frmNewServer.edtUsername.Text, frmNewServer.edtPassword.Text, frmNewServer.DoesSearch);
    frmNewServer.edtName.Text := '';
    frmNewServer.edtAddress.Text := '';
    tvServers.RootNodeCount := 0;
    tvServers.RootNodeCount := Context.Servers.Count;
  end;
end;

procedure TfrmRegisterServer.btnDeleteClick(Sender: TObject);
var
  server : TValueSetEditorServerCache;
begin
  if (tvServers.FocusedNode <> nil) and (Context.Servers.Count > 1) and (Context.Servers[tvServers.FocusedNode.Index] <> Context.WorkingServer) then
  begin
    Context.deleteServer(Context.Servers[tvServers.FocusedNode.Index].Name);
    Context.commit('');
    tvServers.RootNodeCount := 0;
    tvServers.RootNodeCount := Context.Servers.Count;
  end
  else
    MessageBeep(MB_ICONEXCLAMATION);
end;

procedure TfrmRegisterServer.FormDestroy(Sender: TObject);
begin
  FContext.Free;
end;

procedure TfrmRegisterServer.FormShow(Sender: TObject);
begin
  tvServers.RootNodeCount := 0;
  tvServers.RootNodeCount := Context.Servers.Count;
end;

procedure TfrmRegisterServer.SetContext(const Value: TValueSetEditorContext);
begin
  FContext.free;
  FContext := Value;
end;

procedure TfrmRegisterServer.tvServersAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if btnDelete = nil then
    exit;
  btnDelete.Enabled := (tvServers.FocusedNode <> nil) and (Context.Servers.Count > 1) and (Context.Servers[tvServers.FocusedNode.Index] <> Context.WorkingServer);
end;

procedure TfrmRegisterServer.tvServersRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if btnDelete = nil then
    exit;
  btnDelete.Enabled := (tvServers.FocusedNode <> nil) and (Context.Servers.Count > 1) and (Context.Servers[tvServers.FocusedNode.Index] <> Context.WorkingServer);
end;

procedure TfrmRegisterServer.tvServersChecked(Sender: TBaseVirtualTree;  Node: PVirtualNode);
var
  server : TValueSetEditorServerCache;
begin
  server := FContext.Servers[node.Index];
  Context.SetNominatedServer(server.URL);
  tvServers.RootNodeCount := 0;
  tvServers.RootNodeCount := Context.Servers.Count;
end;

procedure TfrmRegisterServer.tvServersClick(Sender: TObject);
begin
  btnUpdate.Enabled := tvServers.FocusedNode <> nil;
end;

procedure TfrmRegisterServer.tvServersDblClick(Sender: TObject);
var
  server : TValueSetEditorServerCache;
begin
  if (tvServers.FocusedNode <> nil) then
  begin
    server := Context.Servers[tvServers.FocusedNode.Index];
    frmNewServer.Context := Context.Link;
    frmNewServer.edtName.Text := server.Name;
    frmNewServer.edtName.Enabled := false;
    frmNewServer.edtAddress.Text := server.URL;
    frmNewServer.edtUsername.Text := server.Username;
    frmNewServer.edtPassword.Text := server.Password;
    if frmNewServer.ShowModal = mrOk then
    begin
      server.URL := frmNewServer.edtAddress.Text;
      server.Username := frmNewServer.edtUsername.Text;
      server.Password := frmNewServer.edtPassword.Text;
      Context.UpdateServer(server);
      tvServers.RootNodeCount := 0;
      tvServers.RootNodeCount := Context.Servers.Count;
    end;
  end
  else
    MessageBeep(MB_ICONEXCLAMATION);
end;

procedure TfrmRegisterServer.tvServersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  server : TValueSetEditorServerCache;
begin
  server := FContext.Servers[node.Index];
  case Column of
    0: CellText := server.URL;
    1: CellText := server.Name;
    2: CellText := inttostr(Server.List.Count);
    3: CellText := inttostr(Server.CodeSystems.Count);
    4: CellText := Server.LastUpdated;
  end
end;

procedure TfrmRegisterServer.tvServersInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  server : TValueSetEditorServerCache;
begin
  server := FContext.Servers[node.Index];
  Node.CheckType := ctCheckBox;
  if (server = Context.WorkingServer) then
    Node.CheckState := csCheckedNormal
  else
    Node.CheckState := csUncheckedNormal;
end;

end.
