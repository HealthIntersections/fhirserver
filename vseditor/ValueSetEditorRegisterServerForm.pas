unit ValueSetEditorRegisterServerForm;

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
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvServersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure btnUpdateClick(Sender: TObject);
    procedure tvServersClick(Sender: TObject);
    procedure tvServersInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvServersChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure Button1Click(Sender: TObject);
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
  if frmNewServer.ShowModal = mrOk then
  begin
    if Context.CheckServer(frmNewServer.edtAddress.Text, msg) then
    begin
      Context.Settings.AddServer(frmNewServer.edtName.Text, frmNewServer.edtAddress.Text);
      frmNewServer.edtName.Text := '';
      frmNewServer.edtAddress.Text := '';
      tvServers.RootNodeCount := Context.Servers.Count;
      tvServers.Invalidate;
    end
  end;
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

procedure TfrmRegisterServer.tvServersChecked(Sender: TBaseVirtualTree;  Node: PVirtualNode);
var
  server : TValueSetEditorServerCache;
begin
  server := FContext.Servers[node.Index];
  Context.SetNominatedServer(server.URL);
end;

procedure TfrmRegisterServer.tvServersClick(Sender: TObject);
begin
  btnUpdate.Enabled := tvServers.FocusedNode <> nil;
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
