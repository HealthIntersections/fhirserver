unit FHIR.Npp.Toolbox;


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
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, FHIR.Npp.DockingForm, Vcl.StdCtrls, FHIR.Npp.Base, Vcl.ToolWin,
  Vcl.ComCtrls, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls, Vcl.Styles, Vcl.Themes,
  fsl_base,
  fhir_objects, fhir_factory, fhir_oauth,
  FHIRPathDocumentation, Vcl.Menus, Vcl.Buttons, FHIR.Ui.ColorSB, FHIR.Npp.Context;

type
  TFHIRVersionStatus = (vsInvalid, vsValid, vsSpecified);
  TFHIRVersionStatuses = array [TFHIRVersion] of TFHIRVersionStatus;

const
  DISPLAY_TFHIRVersionStatus : array [TFHIRVersionStatus] of String = ('Not a valid resource for ', 'Valid resource for ', 'The resource known to be ');

type
  TFHIRToolbox = class(TNppDockingForm)
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    btnServers: TToolButton;
    ToolButton3: TToolButton;
    btnOpen: TToolButton;
    btnSave: TToolButton;
    btnSpecial: TToolButton;
    btnServerValidate: TToolButton;
    ToolButton8: TToolButton;
    btnNew: TToolButton;
    btnFormat: TToolButton;
    btnValidate: TToolButton;
    ToolButton12: TToolButton;
    btnCommitAs: TToolButton;
    btnValidationClear: TToolButton;
    btnPathGo: TToolButton;
    btnSettings: TToolButton;
    btnAbout: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    btnNarrative: TToolButton;
    btnPathDebug: TToolButton;
    btnPathGet: TToolButton;
    btnPatch: TToolButton;
    btnCodeGeneration: TToolButton;
    btnPackages: TToolButton;
    pmFormat: TPopupMenu;
    XML1: TMenuItem;
    Json1: TMenuItem;
    Turtle1: TMenuItem;
    btnR4: TColorSpeedButton;
    btnR3: TColorSpeedButton;
    btnR2: TColorSpeedButton;
    pmNonFormat: TPopupMenu;
    NotaResource1: TMenuItem;
    Timer1: TTimer;
    ImageList24: TImageList;
    ImageList16: TImageList;
    pmVersion: TPopupMenu;
    mnuTreatAsVersion: TMenuItem;
    mnuMarkAsVersion: TMenuItem;
    mnuConvertVersion: TMenuItem;
    pnlEdit: TPanel;
    edtPath: TEdit;
    Panel1: TPanel;
    mnuValidate: TMenuItem;
    tbV2: TToolButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormHide(Sender: TObject);
    procedure FormFloat(Sender: TObject);
    procedure FormDock(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnValidateClick(Sender: TObject);
    procedure btnValidationClearClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
    procedure btnFormatClick(Sender: TObject);
    procedure btnPathGoClick(Sender: TObject);
    procedure btnPathDebugClick(Sender: TObject);
    procedure btnServersClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCommitAsClick(Sender: TObject);
    procedure btnSpecialClick(Sender: TObject);
    procedure btnServerValidateClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure ToolButton17Click(Sender: TObject);
    procedure btnNarrativeClick(Sender: TObject);
    procedure mPathKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mPathEnter(Sender: TObject);
    procedure mPathChange(Sender: TObject);
    procedure btnPathGetClick(Sender: TObject);
    procedure btnPatchClick(Sender: TObject);
    procedure btnCodeGenerationClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPackagesClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure XML1Click(Sender: TObject);
    procedure Json1Click(Sender: TObject);
    procedure Turtle1Click(Sender: TObject);
    procedure btnR2Click(Sender: TObject);
    procedure btnR3Click(Sender: TObject);
    procedure btnR4Click(Sender: TObject);
    procedure mnuTreatAsVersionClick(Sender: TObject);
    procedure mnuMarkAsVersionClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mnuValidateClick(Sender: TObject);
  private
    FMessageShort, FMessageLong : String;
    FFirstPathEdit : boolean;
    FHasValidPath : boolean;
    FServers : TFslList<TRegisteredFHIRServer>;
    FFormat : TFHIRFormat;
    FStatuses : TFHIRVersionStatuses;
    FLoaded : boolean;
    FContext : TFHIRNppContext;
    FUrl : String;
    editleft : integer;
    procedure updateButton(btn : TColorSpeedButton; version: TFHIRVersion);
    procedure SetContext(const Value: TFHIRNppContext);
    procedure updateButtons;
    procedure showVersionMenu(button : TSpeedButton; version : TFHIRVersion; convVersions : TFHIRVersionSet);
  public
    procedure updateStatus(format : TFHIRFormat; versions : TFHIRVersionStatuses; loaded : Boolean; url : String);
    property HasValidPath : boolean read FHasValidPath;
    property Context : TFHIRNppContext read FContext write SetContext;
  end;

var
  FHIRToolbox: TFHIRToolbox;

implementation

{$R *.dfm}

Uses
  fhir4_pathengine,
  FHIR.Client.ServerDialog,
  FHIR.Npp.Settings, FHIR.Npp.Plugin;

procedure TFHIRToolbox.FormCreate(Sender: TObject);
begin
  self.NppDefaultDockingMask := DWS_DF_FLOATING; // whats the default docking position
  self.KeyPreview := true; // special hack for input forms
  self.OnFloat := self.FormFloat;
  self.OnDock := self.FormDock;
  edtPath.font.Name := Settings.FontName;
  edtPath.font.Size := Settings.FontSize;
  if edtPath.Text = Settings.Path then
  begin
    edtPath.Text := 'Path...';
    FFirstPathEdit := true;
  end
  else
    edtPath.Text := Settings.Path;
  inherited;
  if Screen.PixelsPerInch >= 140 then
    ToolBar1.Images := ImageList24;
  editleft := 550 * Screen.PixelsPerInch div 96;
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
  if (Key = #13) and (self.edtPath.Focused) then self.edtPath.Perform(WM_CHAR, 10, 0);
end;

procedure TFHIRToolbox.FormResize(Sender: TObject);
begin
  ToolBar1.Width := editleft;
  pnlEdit.Left := editleft;
  pnlEdit.Width := ClientWidth - pnlEdit.Left;
end;

// Docking code calls this when the form is hidden by either "x" or self.Hide
procedure TFHIRToolbox.FormHide(Sender: TObject);
begin
  inherited;
  SendMessage(self.Npp.NppData.NppHandle, NPPM_SETMENUITEMCHECK, self.CmdID, 0);
  Settings.ToolboxVisible := false;
end;

procedure TFHIRToolbox.FormDestroy(Sender: TObject);
begin
  FContext.Free;
  FServers.Free;
  inherited;
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

procedure TFHIRToolbox.Json1Click(Sender: TObject);
begin
  FNpp.FuncFormat(ffJson);
end;

procedure TFHIRToolbox.mnuMarkAsVersionClick(Sender: TObject);
begin
  Fnpp.MarkAsVersion(TFHIRVersion(mnuTreatAsVersion.tag));
end;

procedure TFHIRToolbox.mnuTreatAsVersionClick(Sender: TObject);
begin
  Fnpp.TreatAsVersion(TFHIRVersion(mnuTreatAsVersion.tag));
end;

procedure TFHIRToolbox.mnuValidateClick(Sender: TObject);
begin
  FNpp.FuncValidate(TFHIRVersion((sender as TMenuItem).tag));
end;

procedure TFHIRToolbox.mPathChange(Sender: TObject);
var
  qry : TFHIRPathEngine;
begin
  Settings.Path := edtPath.Text;
  FNpp.FuncMatchesClear;
  if edtPath.text = '' then
  begin
    FHasValidPath := false;
  end
  else
  begin
    try
      qry := TFHIRPathEngine.create(nil, nil);
      try
        qry.parse(edtPath.Text).free;
        edtPath.Color := clWindow;
        edtPath.Hint := 'FHIR Path Statement';
      finally
        qry.Free;
      end;
      FHasValidPath := true;
      FNpp.reset;
      FNpp.DoNppnTextModified;
    except
      on e: exception do
      begin
        edtPath.Color := $edebfa;
        edtPath.Hint := e.Message;
        FHasValidPath := false;
      end;
    end;
  end;
end;

procedure TFHIRToolbox.mPathEnter(Sender: TObject);
begin
  inherited;
  if FFirstPathEdit then
    edtPath.Clear;
  FFirstPathEdit := false;
end;

procedure TFHIRToolbox.mPathKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if key = VK_F1 then
  begin
    FHIRPathDocumentationForm := TFHIRPathDocumentationForm.Create(self);
    try
      FHIRPathDocumentationForm.ShowModal;
    finally
      FHIRPathDocumentationForm.Free;
    end;
  end;
end;

procedure TFHIRToolbox.SetContext(const Value: TFHIRNppContext);
begin
  FContext.Free;
  FContext := Value;
end;

procedure TFHIRToolbox.showVersionMenu(button: TSpeedButton; version: TFHIRVersion; convVersions: TFHIRVersionSet);
var
  pt : TPoint;
  v : TFHIRVersion;
  ok : boolean;
begin
  mnuTreatAsVersion.Caption := 'Treat as '+CODES_TFHIRVersion[version].ToUpper;
  mnuTreatAsVersion.Enabled := true; // FStatuses[version] = vsValid;
  mnuTreatAsVersion.Tag := ord(version);
  mnuValidate.Caption := 'Validate as '+CODES_TFHIRVersion[version].ToUpper;
  mnuValidate.Enabled := true;
  mnuValidate.Tag := ord(version);
  mnuMarkAsVersion.Caption := 'Mark as '+CODES_TFHIRVersion[version].ToUpper;
  mnuMarkAsVersion.Enabled := FStatuses[version] <> vsInvalid;
  mnuMarkAsVersion.Tag := ord(version);
  mnuConvertVersion.Caption := 'Convert to '+CODES_TFHIRVersion[version].ToUpper;
  ok := false;
  for v in SUPPORTED_VERSIONS do
    ok := ok or (FStatuses[version] <> vsInvalid);
  mnuConvertVersion.Enabled := ok and (FStatuses[version] <> vsSpecified) ;
  mnuConvertVersion.Tag := ord(version);

  pt.X := button.left;
  pt.y := button.top + btnFormat.Height;
  pt := ClientToScreen(pt);
  pmVersion.Popup(pt.X, pt.Y);
end;

procedure TFHIRToolbox.Timer1Timer(Sender: TObject);
begin
  FNpp.checkTrigger;
  updateButtons;
end;

procedure TFHIRToolbox.btnFormatClick(Sender: TObject);
var
  pm : TPopupMenu;
  pt : TPoint;
begin
  if FFormat = ffUnspecified then
    FNpp.refreshStatus;

  pm := pmFormat;
  case FFormat of
    ffXml:
      begin
        XML1.Caption := 'This is Xml';
        XML1.Enabled := false;
        Json1.Caption := 'To &Json';
        Json1.Enabled := true;
        Turtle1.Caption := 'To &Turtle';
        Turtle1.Enabled := true;
      end;
    ffJson:
      begin
        XML1.Caption := 'To &Xml';
        XML1.Enabled := true;
        Json1.Caption := 'This is &Json';
        Json1.Enabled := false;
        Turtle1.Caption := 'To &Turtle';
        Turtle1.Enabled := true;
      end;
    ffTurtle:
      begin
        XML1.Caption := 'To &Xml';
        XML1.Enabled := true;
        Json1.Caption := 'To &Json';
        Json1.Enabled := true;
        Turtle1.Caption := 'This is Turtle';
        Turtle1.Enabled := false;
      end;
    else // ffUnspecified :
    begin
      pm := pmNonFormat;
    end;
  end;
  pt.X := btnFormat.left;
  pt.y := btnFormat.top + btnFormat.Height;
  pt := ClientToScreen(pt);
  pm.Popup(pt.X, pt.Y);
end;

procedure TFHIRToolbox.btnValidateClick(Sender: TObject);
begin
  _FuncValidate;
end;

procedure TFHIRToolbox.btnCommitAsClick(Sender: TObject);
begin
  _FuncPOST;
end;

procedure TFHIRToolbox.btnValidationClearClick(Sender: TObject);
begin
  _FuncValidateClear;
end;

procedure TFHIRToolbox.btnPathGoClick(Sender: TObject);
begin
  FNpp.FuncJumpToPath;
end;

procedure TFHIRToolbox.btnR2Click(Sender: TObject);
begin
  showVersionMenu(btnR2, fhirVersionRelease2, []);
end;

procedure TFHIRToolbox.btnR3Click(Sender: TObject);
begin
  showVersionMenu(btnR3, fhirVersionRelease3, [fhirVersionRelease4]);
end;

procedure TFHIRToolbox.btnR4Click(Sender: TObject);
begin
  showVersionMenu(btnR4, fhirVersionRelease4, [fhirVersionRelease3]);
end;

procedure TFHIRToolbox.btnSettingsClick(Sender: TObject);
begin
  _FuncSettings;
end;

procedure TFHIRToolbox.ToolButton17Click(Sender: TObject);
begin
  _FuncToolbox;
end;

procedure TFHIRToolbox.Turtle1Click(Sender: TObject);
begin
  FNpp.FuncFormat(ffTurtle);
end;

procedure TFHIRToolbox.btnAboutClick(Sender: TObject);
begin
  _FuncAbout;
end;

procedure TFHIRToolbox.btnPathDebugClick(Sender: TObject);
begin
  FNpp.FuncDebugPath;
end;

procedure TFHIRToolbox.btnNarrativeClick(Sender: TObject);
begin
  _FuncNarrative;
end;

procedure TFHIRToolbox.btnPathGetClick(Sender: TObject);
begin
  FNpp.FuncExtractPath;
end;

procedure TFHIRToolbox.btnPatchClick(Sender: TObject);
begin
  _FuncDifference;
end;

procedure TFHIRToolbox.btnCodeGenerationClick(Sender: TObject);
begin
  _FuncGenerateCode;
end;

procedure TFHIRToolbox.btnPackagesClick(Sender: TObject);
begin
  _FuncPackageManager;
end;

procedure TFHIRToolbox.btnServersClick(Sender: TObject);
begin
   FNpp.FuncSettings(true);
end;

procedure TFHIRToolbox.btnOpenClick(Sender: TObject);
begin
  _FuncOpen;
end;

procedure TFHIRToolbox.btnSaveClick(Sender: TObject);
begin
  _FuncPUT;
end;

procedure TFHIRToolbox.btnSpecialClick(Sender: TObject);
begin
  _FuncTransaction;
end;

procedure TFHIRToolbox.btnServerValidateClick(Sender: TObject);
begin
  _FuncServerValidate;
end;

procedure TFHIRToolbox.btnNewClick(Sender: TObject);
begin
  _FuncNewResource;
end;

procedure TFHIRToolbox.updateButton(btn : TColorSpeedButton; version: TFHIRVersion);
begin
  if FContext.VersionLoading[version] <> vlsLoaded then
  begin
    btn.Enabled := false;
    btn.Font.Style := [];
    btn.Font.Color := clMaroon;
    btn.Color := clSilver;
    btn.Hint := CODES_TFHIRVersionLoadingStatus[FContext.VersionLoading[version]];
  end
  else if FFormat = ffUnspecified then
  begin
    btn.Enabled := true;
    btn.Font.Style := [];
    btn.Color := clSilver;
    btn.Hint := 'Not a FHIR Resource';
  end
  else
  begin
    btn.Enabled := true;
    btn.Font.Style := [fsBold];
    btn.Font.Color := clBlack;
    case FStatuses[version] of
      vsInvalid: btn.Color := clRed;
      vsValid: btn.Color := clYellow;
      vsSpecified:
        begin
        btn.Color := clLime;
        btn.Font.Color := clOlive;
        end;
    end;
    btn.Hint := DISPLAY_TFHIRVersionStatus[FStatuses[version]] + UpperCase(CODES_TFHIRVersion[version]);
  end;
end;

procedure TFHIRToolbox.updateStatus(format: TFHIRFormat; versions: TFHIRVersionStatuses; loaded : Boolean; url : String);
begin
  FFormat := format;
  FStatuses := versions;
  FUrl := url;
  FLoaded := loaded;
  UpdateButtons;
end;

procedure TFHIRToolbox.XML1Click(Sender: TObject);
begin
  FNpp.FuncFormat(ffXml);
end;

procedure TFHIRToolbox.updateButtons;
begin
  btnFormat.Enabled := FLoaded;
  updateButton(btnR2, fhirVersionRelease2);
  updateButton(btnR3, fhirVersionRelease3);
  updateButton(btnR4, fhirVersionRelease4);

  btnNarrative.Enabled := FLoaded and (FFormat <> ffUnspecified);
  btnValidate.Enabled := FLoaded and (FFormat <> ffUnspecified);
  btnValidationClear.Enabled := FLoaded and (FFormat <> ffUnspecified);
  btnPatch.Enabled := FLoaded and (FFormat <> ffUnspecified);
  btnCodeGeneration.Enabled := FLoaded and (FFormat <> ffUnspecified);

  btnPathGo.Enabled := FLoaded and (FFormat <> ffUnspecified);
  btnPathDebug.Enabled := FLoaded and (FFormat <> ffUnspecified);
  btnPathGet.Enabled := FLoaded and (FFormat <> ffUnspecified);

  btnNew.Enabled := FLoaded;
  btnOpen.Enabled := FLoaded;
  btnSave.Enabled := FLoaded and (FFormat <> ffUnspecified) and (FUrl <> '');
  btnCommitAs.Enabled := FLoaded and (FFormat <> ffUnspecified);
  btnSpecial.Enabled := FLoaded and (FFormat <> ffUnspecified);
  btnServerValidate.Enabled := FLoaded and (FFormat <> ffUnspecified);

  btnServers.Enabled := FLoaded;
  btnPackages.Enabled := true;
  btnSettings.Enabled := true;
  btnAbout.Enabled := true;
end;

end.
