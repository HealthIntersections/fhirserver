unit SettingsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Buttons,
  Vcl.Dialogs, NppForms, Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
  VirtualTrees, Vcl.ComCtrls, SmartOnFhirUtilities, NewServerForm;

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
    Label3: TLabel;
    Label4: TLabel;
    SpeedButton1: TSpeedButton;
    edtFile: TEdit;
    TabSheet2: TTabSheet;
    btnEditAsText: TButton;
    Panel3: TPanel;
    vtServers: TVirtualStringTree;
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    btnUp: TButton;
    btnDown: TButton;
    Label5: TLabel;
    TabSheet3: TTabSheet;
    Panel4: TPanel;
    GroupBox3: TGroupBox;
    cbPathSummary: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
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
  FHIRToolboxForm;

procedure TSettingForm.btnEditAsTextClick(Sender: TObject);
begin
  FNpp.DoOpen(Settings.SourceFile);
  showmessage('Note that this file is only read at start up, and changes you make will be overwritten by by the plug-in if you make setting changes');
end;

procedure TSettingForm.Button1Click(Sender: TObject);
begin
  Settings.TerminologyServer := edtServer.Text;
  Settings.DefinitionsSource := edtFile.Text;
  Settings.NoPathSummary := not cbPathSummary.checked;
  Settings.CommitChanges;
  _FuncDisconnect;

  if FHIRToolbox <> nil then
    FHIRToolbox.loadServers;
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
    Settings.moveServer(c, -1);
    vtServers.Invalidate;
    vtServers.ClearSelection;
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
    Settings.deleteServer(i);
    vtServers.RootNodeCount := Settings.ServerCount;
    vtServers.Invalidate;
  end;
end;

procedure TSettingForm.btnDownClick(Sender: TObject);
var
  i, c : integer;
  n : PVirtualNode;
begin
  if (vtServers.GetFirstSelected() <> nil) and (vtServers.GetFirstSelected().Index < Settings.ServerCount - 1) then
  begin
    c := vtServers.GetFirstSelected().Index;
    Settings.moveServer(c, +2); // because +1 is immediately after, so no change
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
    RegisterServerForm := TRegisterServerForm.create(npp);
    try
      RegisterServerForm.loadFrom(i);
      if RegisterServerForm.ShowModal = mrOk then
      begin
        vtServers.RootNodeCount := Settings.ServerCount;
        vtServers.Invalidate;
      end;
    finally
      RegisterServerForm.Free;
    end;
  end;
end;

procedure TSettingForm.btnAddClick(Sender: TObject);
begin
  RegisterServerForm := TRegisterServerForm.create(npp);
  try
    if RegisterServerForm.ShowModal = mrOk then
    begin
      vtServers.RootNodeCount := Settings.ServerCount;
      vtServers.Invalidate;
    end;
  finally
    RegisterServerForm.Free;
  end;
end;

procedure TSettingForm.FormCreate(Sender: TObject);
begin
  inherited;
  Settings.holdChanges;
end;

procedure TSettingForm.FormShow(Sender: TObject);
begin
  edtServer.Text := Settings.TerminologyServer;
  edtFile.Text := Settings.DefinitionsSource;
  vtServers.RootNodeCount := Settings.ServerCount;
  cbPathSummary.checked := not Settings.NoPathSummary;
end;

procedure TSettingForm.SpeedButton1Click(Sender: TObject);
begin
  if od.Execute then
    edtFile.Text := od.FileName;
end;

procedure TSettingForm.vtServersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  rec : TRegisteredServer;
begin
  rec := Settings.serverInfo(Node.Index);
  case Column of
  0: CellText := rec.name;
  1: CellText := rec.fhirEndpoint;
  2: CellText := BoolToStr(rec.SmartOnFHIR, true);
  end;
end;

end.
