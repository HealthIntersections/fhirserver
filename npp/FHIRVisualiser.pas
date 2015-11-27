unit FHIRVisualiser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, NppDockingForms, Vcl.StdCtrls, NppPlugin, Vcl.ToolWin, SystemSupport,
  Vcl.ComCtrls, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls, Vcl.Styles, Vcl.Themes,
  FHIRPathDocumentation, Vcl.Buttons, Vcl.OleCtrls, SHDocVw, TextUtilities;

type
  TVisualiserMode = (
    vmNone,  // the visualiser is not showing
    vmNarrative, // show the narrative of the resource
    vmPath, // show the path output
    vmMessages, // show messages about the content
    vmElement // information about the current selected element
  );

  TFHIRVisualizer = class(TNppDockingForm)
    TabControl1: TTabControl;
    pnlContent: TPanel;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormFloat(Sender: TObject);
    procedure FormDock(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
  private
    { Private declarations }
    memo : TMemo;
    browser : TWebBrowser;

    last : String;
    procedure makeBrowser;
    procedure makeMemo;
  public
    { Public declarations }
    procedure setNarrative(s : String);
    procedure setPathOutcomes(s : String);
  end;

var
  FHIRVisualizer: TFHIRVisualizer;
  VisualiserMode : TVisualiserMode = vmNone;

implementation

{$R *.dfm}

Uses
  FHIRPluginSettings,
  FHIRToolboxForm,
  FHIRPlugin;

procedure TFHIRVisualizer.FormKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;
end;

// Docking code calls this when the form is hidden by either "x" or self.Hide
procedure TFHIRVisualizer.FormCreate(Sender: TObject);
begin
  inherited;
  VisualiserMode := TVisualiserMode(TabControl1.TabIndex+1);
end;

procedure TFHIRVisualizer.FormDock(Sender: TObject);
begin
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 1);
end;

procedure TFHIRVisualizer.FormFloat(Sender: TObject);
begin
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 1);
end;

procedure TFHIRVisualizer.FormHide(Sender: TObject);
begin
  inherited;
  Settings.VisualiserVisible := false;
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 0);
  VisualiserMode := vmNone;
end;

procedure TFHIRVisualizer.FormShow(Sender: TObject);
begin
  inherited;
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 1);
  Settings.VisualiserVisible := true;
  last := '';
  TabControl1Change(nil);
end;

procedure TFHIRVisualizer.makeBrowser;
begin
  browser := TWebBrowser.Create(self);
  browser.SetParentComponent(pnlContent);
  browser.Align := alClient;
end;

procedure TFHIRVisualizer.makeMemo;
begin
  memo := TMemo.Create(self);
  memo.Parent := pnlContent;
  memo.Align := alClient;
  memo.Font.Name := Settings.fontName;
  memo.Font.Size := Settings.fontSize;
end;

procedure TFHIRVisualizer.setNarrative(s: String);
var
  fn : String;
begin
  if (s <> last) then
  begin
    fn := IncludeTrailingBackslash(SystemTemp)+'validation-outcomes-npp-fhir.html';
    StringToFile(s, fn, TEncoding.UTF8);
    browser.Navigate('file://'+fn);
    last := s;
  end;
end;

procedure TFHIRVisualizer.setPathOutcomes(s: String);
begin
  if (s <> last) then
  begin
    memo.Text := s;
    last := s;
  end;
end;

procedure TFHIRVisualizer.TabControl1Change(Sender: TObject);
begin
  VisualiserMode := TVisualiserMode(TabControl1.TabIndex+1);
  case VisualiserMode of
    vmNone :
      begin
        FreeAndNil(memo);
        FreeAndNil(browser);
      end;
    vmNarrative :
      begin
        FreeAndNil(memo);
        makeBrowser;
      end;
    vmPath:
      begin
        makeMemo;
        FreeAndNil(browser);
      end;
    vmMessages:
      begin
        FreeAndNil(memo);
        FreeAndNil(browser);
      end;
    vmElement :
      begin
        FreeAndNil(memo);
        FreeAndNil(browser);
      end;
  end;
  FNpp.reset;
  FNpp.DoNppnTextModified;
end;

end.
