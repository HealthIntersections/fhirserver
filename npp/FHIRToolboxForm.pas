unit FHIRToolboxForm;

interface

{
On the toolbox:
  ----
  Change Server
  <server>
  Open by Client
  Put / Post / Transaction (if bundle)
  Validate on Server
  ----
  New from Template
  JSON <-> XML
  Validation/Intellisense on/off
  ----
  <path>
  EvaluatePath
  ----
  Settings
  Close


icons
  0  server
  1  client open
  2  post
  3  put
  4  transaction
  5  validate
  6  new
  7  format change
  8  evaluate path
  9  settings
  10 close

}

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, NppDockingForms, Vcl.StdCtrls, NppPlugin, Vcl.ToolWin,
  Vcl.ComCtrls, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls;

type
  TFHIRToolbox = class(TNppDockingForm)
    Memo1: TMemo;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ComboBox1: TComboBox;
    ToolButton1: TToolButton;
    Panel1: TPanel;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormHide(Sender: TObject);
    procedure FormFloat(Sender: TObject);
    procedure FormDock(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure ToolButton14Click(Sender: TObject);
    procedure ToolButton18Click(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure ToolButton15Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ToolButton13Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton16Click(Sender: TObject);
    procedure ToolButton17Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FHIRToolbox: TFHIRToolbox;

implementation

{$R *.dfm}

Uses
  FHIRPluginSettings,
  FHIRPlugin;

procedure TFHIRToolbox.FormCreate(Sender: TObject);
begin
  self.NppDefaultDockingMask := DWS_DF_FLOATING; // whats the default docking position
  self.KeyPreview := true; // special hack for input forms
  self.OnFloat := self.FormFloat;
  self.OnDock := self.FormDock;
  inherited;
end;

procedure TFHIRToolbox.Button1Click(Sender: TObject);
begin
  inherited;
  self.UpdateDisplayInfo('test');
end;

procedure TFHIRToolbox.Button2Click(Sender: TObject);
begin
  inherited;
  self.Hide;
end;

// special hack for input forms
// This is the best possible hack I could came up for
// memo boxes that don't process enter keys for reasons
// too complicated... Has something to do with Dialog Messages
// I sends a Ctrl+Enter in place of Enter
procedure TFHIRToolbox.FormKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;
  if (Key = #13) and (self.Memo1.Focused) then self.Memo1.Perform(WM_CHAR, 10, 0);
end;

// Docking code calls this when the form is hidden by either "x" or self.Hide
procedure TFHIRToolbox.FormHide(Sender: TObject);
begin
  inherited;
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 0);
  Settings.ToolboxVisible := false;
end;

procedure TFHIRToolbox.FormDock(Sender: TObject);
begin
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 1);
end;

procedure TFHIRToolbox.FormFloat(Sender: TObject);
begin
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 1);
end;

procedure TFHIRToolbox.FormShow(Sender: TObject);
begin
  inherited;
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 1);
  Settings.ToolboxVisible := true;
end;

procedure TFHIRToolbox.ToolButton10Click(Sender: TObject);
begin
  _FuncFormat;
end;

procedure TFHIRToolbox.ToolButton11Click(Sender: TObject);
begin
  _FuncValidate;
end;

procedure TFHIRToolbox.ToolButton13Click(Sender: TObject);
begin
  _FuncPOST;
end;

procedure TFHIRToolbox.ToolButton14Click(Sender: TObject);
begin
  _FuncValidateClear;
end;

procedure TFHIRToolbox.ToolButton15Click(Sender: TObject);
begin
  _FuncPath;
end;

procedure TFHIRToolbox.ToolButton16Click(Sender: TObject);
begin
  _FuncSettings;
end;

procedure TFHIRToolbox.ToolButton17Click(Sender: TObject);
begin
  _FuncToolbox;
end;

procedure TFHIRToolbox.ToolButton18Click(Sender: TObject);
begin
  _FuncAbout;
end;

procedure TFHIRToolbox.ToolButton1Click(Sender: TObject);
begin
  _FuncServers;
end;

procedure TFHIRToolbox.ToolButton2Click(Sender: TObject);
begin
  _FuncConnect;
end;

procedure TFHIRToolbox.ToolButton4Click(Sender: TObject);
begin
  _FuncOpen;
end;

procedure TFHIRToolbox.ToolButton5Click(Sender: TObject);
begin
  _FuncPUT;
end;

procedure TFHIRToolbox.ToolButton6Click(Sender: TObject);
begin
  _FuncTransaction;
end;

procedure TFHIRToolbox.ToolButton7Click(Sender: TObject);
begin
  _FuncServerValidate;
end;

procedure TFHIRToolbox.ToolButton9Click(Sender: TObject);
begin
  _FuncNewResource;
end;

end.
