Unit CDEForm;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$DEFINE SCINTILLA}
{.$.DEFINE ADDICT} // if you have a copy of Addict in the delphi pathway (not open source)

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, StdCtrls,
  Dialogs, ExtCtrls, Menus, IniFiles, Clipbrd,

  ScintInt, ScintEdit, ScintCDA,

  fsl_stream, fsl_utilities, fsl_shell,
  dicom_Dictionary,
  cda_documents, cda_objects,

  fui_vclx_forms, fui_vclx_controls,
  wp_graphics, wp_printing_win, wp_clipboard,

  wp_document, FHIR.WP.Builder, FHIR.WP.Control, FHIR.WP.Toolbar, wp_format, FHIR.WP.Dialogs,
  wp_types, wp_definers, FHIR.WP.Renderer, FHIR.WP.Widgets, FHIR.WP.Snomed, FHIR.WP.Settings,
  {$IFDEF ADDICT} FHIR.WP.Addict, {$ENDIF}
  wp_working, FHIR.WP.V2Ft,

  CDEOptions, OidFetcher, CdaheaderForm;

{
  CDA Header dialog

  Clinical Document
  code
  identifier
  title
  effective time
  setid / version number
  language
  patent document oid / setid / version
  source record identifier

  Patient
  identifiers 0..*
  names       0..*
  address     0..*
  telecom     0..*
  birthdate   0..1
  codes
  - marital
  - religious
  - ethnic
  Provider : org (name | id | address | telecom)

  Author
  device - code / model name
  persion name
  id / code / addr / telecom
  represents : org (name | id | address | telecom)

  Authenticator
  person name / id / addr / telecom + code
  represents : org (name | id | address | telecom)

  Encounter

}

Const
  SHOW_CLIPBOARD = True;
  SHOW_RTF = True;
  SHOW_TESTS = True;
  SHOW_PRINTERS = True;
  PDF_PRINTER_NAME = 'Bullzip PDF Printer'; // 'Microsoft Print to PDF';

Type
  TRecentFileList = class(TStringList)
  private
    function GetSaveText: string;
    procedure SetSaveText(Value: string);
  public
    property SaveText: string read GetSaveText write SetSaveText;
  end;

const
  { Memo marker numbers }
  mmIconHasEntry = 0; { grey dot }
  mmIconEntryProcessed = 1; { green dot }
  mmIconBreakpoint = 2; { stop sign }
  mmIconBreakpointGood = 3; { stop sign + check }
  mmIconBreakpointBad = 4; { stop sign + X }
  mmLineError = 10; { red line highlight }
  mmLineBreakpoint = 11; { red line highlight }
  mmLineBreakpointBad = 12; { ugly olive line highlight }
  mmLineStep = 13; { blue line highlight }

  { Memo indicator numbers (also in ScintStylerInnoSetup) }
  inSquiggly = 0;
  inPendingSquiggly = 1;

type
  TISScintEdit = class(TScintEdit)
  protected
    procedure CreateWnd; override;
  end;

Type
  TWordProcessorForm = Class (TUixForm)
  Private
//    FReportScribe: TKestralPublishScribe;

    FRecentList: TRecentFileList;
    FOpenFormat: TWPFormat;
    IsLoading: boolean;

    pgPages: TUixPageControl;
    pgWP: TUixTabsheet;
    pgClipboard: TUixTabsheet;

    pnlDocument: TUixPanel;
    pnlSource: TUixPanel;
    spltView: TUixSplitter;

    wrdEditor: TWordProcessor; // TWPSpeechMagic;
    wrdToolbar: TWordProcessorToolbar;
    pnlViewSource: TUixPanel;
    pnlSourceHeader: TUixPanel;
    pnlBanner: TUixPanel;
    pnlEditHeader: TUixPanel;
    btnEditHeader: TUixButton;
    edtSource: TISScintEdit;
    sciCDA: TCDAStyler;
    btnSource: TUixButton;
    wrdBanner: TWordProcessor;

    wrdTouchToolbar: TWordProcessorTouchToolbar;
    FDocumentName: String;
    FIsDirty: boolean;

    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuOpen: TMenuItem;
    mnuRecent: TMenuItem;
    mnuSave: TMenuItem;
    mnuSaveAs: TMenuItem;
    mnuNew: TMenuItem;
    mnuClose: TMenuItem;
    mnuExit: TMenuItem;
    mnuPrint: TMenuItem;
    mnuPrintPreview: TMenuItem;
    mnuEdit: TMenuItem;
    mnuUndo: TMenuItem;
    mnuRedo: TMenuItem;
    mnuCopy: TMenuItem;
    mnuCopyAll: TMenuItem;
    mnuCopyFilename: TMenuItem;
    mnuPaste: TMenuItem;
    mnuCut: TMenuItem;
    mnuPasteSpecial: TMenuItem;
    mnuFind: TMenuItem;
    mnuReplace: TMenuItem;
    mnuFormat: TMenuItem;
    mnuFont: TMenuItem;
    mnuParagraph: TMenuItem;
    mnuStyle: TMenuItem;
    mnuTextOnly: TMenuItem;
    mnuChangeCase: TMenuItem;
    mnuSpecial: TMenuItem;
    mnuTools: TMenuItem;
    mnuHelp: TMenuItem;
    odDocument: TWPOpenFileDialog;
    sdDocument: TWPSaveFileDialog;
    sdPDF : TSaveDialog;
    pnlStatus: TUixPanel;
    sbpHotspot: TUixPanel;
    sbpCursor: TUixPanel;
    sbpDirty: TUixPanel;
    sbpFields: TUixPanel;
    sbpComments: TUixPanel;
    sbpZoom: TUixPanel;
    sbpFill: TUixPanel;
    mnuKarismaConnect: TMenuItem;
    mnuImageMode: TMenuItem;
    mnuFormatMode: TMenuItem;
    mnuConsoleMode: TMenuItem;
    mnuNoParagraphMode: TMenuItem;
    mnuReadOnly: TMenuItem;
    Empty1: TMenuItem;
    Sample1: TMenuItem;
    SampleSect: TMenuItem;
    Error1: TMenuItem;
    NoTableLines1: TMenuItem;
    Yield1: TMenuItem;
    LowLight1: TMenuItem;
    FormsMode1: TMenuItem;
    SpellingOption: TMenuItem;
    MnuItmSpelling: TMenuItem;
    MnuItmStyleEditor: TMenuItem;
    MnuItmOids: TMenuItem;
    MnuItmDictEditor: TMenuItem;
    MnuItmOptions: TMenuItem;
    mnuHelpIndex: TMenuItem;
    mnuHelpWebSite: TMenuItem;
    mnuHelpCheckUpgrade: TMenuItem;
    mnuHelpSendSupportCase: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuPasteimport: TMenuItem;

    HL71: TMenuItem;
    MD1: TMenuItem;
    Sample21: TMenuItem;
    Sample31: TMenuItem;
    Sample41: TMenuItem;
    Sample51: TMenuItem;
    Sample61: TMenuItem;
    Sample71: TMenuItem;
    Sample81: TMenuItem;

    clpTimer: TTimer;
    clpList: TListBox;
    clpSource: TUixMemo;

    pgTests: TUixTabsheet;
    // FSnomedBtn : TUixToolButton;

    pgPrinters: TUixTabsheet;
    prnList: TListBox;
    prnDetails: TUixMemo;

    // FKarisma : TKestralStorageManager;
    // FKarismaKey : Int64;
    FFirstBtn: TUixToolButton;
    FLastBtn: TUixToolButton;
    FNextBtn: TUixToolButton;
    FPrevBtn: TUixToolButton;
    FZoomTrack: TTrackBar;
    FZooming: boolean;
    FSourceVisible: boolean;
    FSourceWidth: integer;

    Procedure mnuExitClick(Sender: TObject);
    Procedure mnuNewClick(Sender: TObject);
    Procedure mnuOpenClick(Sender: TObject);
    Procedure mnuSaveAsClick(Sender: TObject);
    Procedure mnuSaveClick(Sender: TObject);
    Procedure mnuCloseClick(Sender: TObject);
    Procedure mnuPrintClick(Sender: TObject);
    Procedure mnuPrintPreviewClick(Sender: TObject);
    Procedure mnuFaxClick(Sender: TObject);
    Procedure mnuPDFClick(Sender: TObject);
    Procedure mnuEmailClick(Sender: TObject);
    Procedure mnuExportBitmapClick(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure mnuFileClick(Sender: TObject);
    Procedure mnuEditClick(Sender: TObject);
    Procedure mnuUndoClick(Sender: TObject);
    Procedure mnuRedoClick(Sender: TObject);
    Procedure mnuCutClick(Sender: TObject);
    Procedure mnuCopyClick(Sender: TObject);
    Procedure mnuCopyAllClick(Sender: TObject);
    Procedure mnuCopyFilenameClick(Sender: TObject);
    Procedure mnuPasteClick(Sender: TObject);
    Procedure mnuPasteSpecialClick(Sender: TObject);
    Procedure mnuFormatClick(Sender: TObject);
    Procedure mnuFontClick(Sender: TObject);
    Procedure mnuParagraphClick(Sender: TObject);
    Procedure mnuStyleClick(Sender: TObject);
    Procedure mnuTextOnlyClick(Sender: TObject);
    Procedure mnuChangeCaseClick(Sender: TObject);
    Procedure mnuImageModeClick(Sender: TObject);
    Procedure mnuFormatModeClick(Sender: TObject);
    Procedure mnuConsoleModeClick(Sender: TObject);
    Procedure mnuNoParagraphModeClick(Sender: TObject);
    Procedure mnuReadOnlyClick(Sender: TObject);
    Procedure Sample1Click(Sender: TObject);
    Procedure SampleSectClick(Sender: TObject);
    Procedure Sample2Click(Sender: TObject);
    Procedure Error1Click(Sender: TObject);
    Procedure NoTableLines1Click(Sender: TObject);
    Procedure Yield1Click(Sender: TObject);
    Procedure LowLight1Click(Sender: TObject);
    Procedure FormsMode1Click(Sender: TObject);
    Procedure SpellingOptionClick(Sender: TObject);
    Procedure Sample3Click(Sender: TObject);
    Procedure Sample4Click(Sender: TObject);
    Procedure Sample5Click(Sender: TObject);
    Procedure Sample6Click(Sender: TObject);
    Procedure Sample7Click(Sender: TObject);
    Procedure Sample8Click(Sender: TObject);
    Procedure mnuKarismaConnectClick(Sender: TObject);
    Procedure HL71Click(Sender: TObject);
    Procedure HL72Click(Sender: TObject);
    Procedure MD1Click(Sender: TObject);
    Procedure mnuFindClick(Sender: TObject);
    Procedure mnuReplaceClick(Sender: TObject);
    Procedure clpLoad(Sender: TObject);
    Procedure clpSelect(Sender: TObject);
    Procedure prnSelect(Sender: TObject);
    Procedure ReplaceFieldClick(Sender: TObject);
    Procedure DocFieldProcessClick(Sender: TObject);
    Procedure LiveFieldProcessClick(Sender: TObject);
    Procedure CopyAllClick(Sender: TObject);
    Procedure SpeechMagicPopulateClick(Sender: TObject);
    // Procedure FirstBtnClicked(Sender: TObject);
    // Procedure PrevBtnClicked(Sender: TObject);
    // Procedure NextBtnClicked(Sender: TObject);
    // Procedure LastBtnClicked(Sender: TObject);
    Procedure CheckFieldsClick(Sender: TObject);
    Procedure SpawnWPEditor(Sender: TObject);
    Procedure SpellingClick(Sender: TObject);
    Procedure StyleEditorClick(Sender: TObject);
    procedure OidUpdateClick(Sender: TObject);
    Procedure DictEditorClick(Sender: TObject);
    Procedure ToolsOptionsClick(Sender: TObject);
    Procedure HelpIndexClick(Sender: TObject);
    Procedure HelpWebSiteClick(Sender: TObject);
    Procedure HelpCheckUpgradeClick(Sender: TObject);
    Procedure HelpSendSupportCaseClick(Sender: TObject);
    Procedure HelpAboutClick(Sender: TObject);
    Procedure mnuPasteImportClick(Sender: TObject);
    Procedure mnuOpenRecentClick(Sender: TObject);
    Procedure SettingsChanged(Sender: TObject);
    Procedure ChangeZoom(Sender: TObject);
    Procedure ToggleSourceView(Sender: TObject);
    Procedure SourceViewResize(Sender: TObject);
    Procedure DoDocumentLoaded(Sender: TObject);
    Procedure LoadCDA(cda: TCDADocument);
    function BuildCDABanner(cda: TCDADocument): TWPDocument;
    procedure SourceChange(Sender: TObject; const Info: TScintEditChangeInfo);

    Procedure ShowPopup(Const aScreenBox: Windows.TRect);
    Function CreateExampleStyle: TWPStyle;
    Function CreateHeadingStyle: TWPStyle;
    Function CreateUppercaseStyle: TWPStyle;
    Function CreateNormalStyle: TWPStyle;
    Procedure ContentChange(oSender: TObject);
    Procedure SelectionChange(oSender: TObject);
    Function CodeCompletion(oSender: TObject; Const sText: String;
      oItems: TWPCompletionItems): boolean;
    Procedure WPNewDoc(oSender: TObject; oDocument: TWPDocument);
    Procedure HotSpotHover(oSender: TWordProcessor; bActive: boolean;
      oInfo: TWPHotspotInformation);
    Procedure HotSpot(oSender: TWordProcessor; oInfo: TWPHotspotInformation);
    Function CheckSave: boolean;
    Procedure SetIsDirty(Const Value: boolean);
    Function DocumentTitle: String;
    procedure LoadRecentList;
    procedure RecordRecentFile(sFilename: String; aFormat: TWPFormat);
//    function FetchPublishingDocument : TKestralPublishDocumentBundle;
    function getPDFPrinter : TFslPrinter;
    procedure saveToPDF(filename : String);
    function printPDFToBuffer : TFslBuffer;

    Procedure LoadForm;
    Procedure LoadFileMenu;
    Procedure LoadEditMenu;
    Procedure LoadFormatMenu;
    Procedure LoadToolsMenu;
    Procedure LoadHelpMenu;
    Procedure LoadSpecialMenu;
    Procedure OnResizeCDASpace(Sender: TObject);
    procedure DoShow(Sender : TObject);

    Procedure LoadDialogs;
    Procedure LoadStatusBar;
    Procedure LoadTests;
    Function NamedTemplate(oSender: TObject; Const iId: integer;
      Const sName: String; Var aFormat: TWPFormat): TFslBuffer;
    Function DoTemplate(oSender: TObject): TFslBuffer;
    Procedure UpdateField(oSender: TObject; aState: TFieldUpdateStatus; oDefinitionProvider: TWPFieldDefinitionProvider; oField: TWPDocumentField; sContent: String; bValid: boolean);
    procedure ApplySourceViewSettings;

    Property IsDirty: boolean Read FIsDirty Write SetIsDirty;
    Function newMenuItem(Var i: TMenuItem; Const sCaption: String;
      index: integer; aAction: TNotifyEvent; aShortCut: TShortCut = 0)
      : TMenuItem;
    Function newSeparator: TMenuItem;
    Function getDefaultPrinter: TFslPrinter;
    Function BuildPageLayoutController: TWPPageLayoutController;
    Procedure LoadClipboard;
    Procedure LoadPrinters;
    Function UnderDevelopment: boolean;
    // Procedure ConnectToKarisma(oProvider : TKestralStorageProvider);
    // Procedure LoadWPFromKarisma(oSQL : TKestralStorageStatement);
    // Procedure SnomedBtnClicked(Sender: TObject);
    Function DescribeCanvas(oCanvas: TFslPrinterCanvas;
      oPrinter: TFslPrinter): String;
    Function LoadDicomDictinoary: TDicomDictionary;
    procedure NewDocumentSource(source: TFslStream);
    procedure EditCDAHeader(Sender: TObject);
  Protected
    Procedure Initialise; Override;
    Procedure Finalise; Override;


    Function NewDocument: boolean; Overload; Virtual;
    Function Open(Const sFilename: String; aFormat: TWPFormat): boolean;
      Overload; Virtual;
    Function Save(Const sFilename: String; aFormat: TWPFormat): boolean;
      Overload; Virtual;

    Function DocRoot: String;
  Public
    Procedure Refresh; Override;
  End;

var
  WordProcessorForm : TWordProcessorForm;

Implementation

Uses
  fsl_collections,
  fui_vclx_Base,
  FHIR.WP.FieldDefiners, FHIR.WP.AnnotationDefiners;


Const
  NEW_DOC_NAME = 'New Document';
  NS_TEST = 'tools\wordprocessor\test';

Type
  TWPTestFieldDefiner = Class(TWPFieldDefinitionProvider)
  Public
    Function GetNamespace: String; Overload; Override;
    Function GetTitle: String; Overload; Override;
    Function CanInsertField: boolean; Overload; Override;
    // Function CanInsertSection : Boolean; Overload; Override;
    Function NewField(oField: TWPDocumentField; bSection: boolean;
      var bAcceptExistingContent: boolean; Var oSection: TWPDocumentSection)
      : boolean; Overload; Override;
    // Function NewSection(oSection : TWPDocumentSection) : Boolean; Overload; Override;
    // Function UserCanEditFieldProperties(oField : TWPDocumentField) : Boolean; Overload; Override;
    // Function UserCanEditFieldSectionProperties(oSection : TWPDocumentSection) : Boolean; Overload; Override;
    // Function EditField(oField : TWPDocumentField) : Boolean; Overload; Override;
    // Function EditSection(oSection : TWPDocumentSection) : Boolean; Overload; Override;
  End;

  TWPTestFieldValuer = Class(TWPFieldValueProvider)
    Function GetNamespace: String; Overload; Override;
    Function GetAction(oField: TWPDocumentField): TFieldAction;
      Overload; Override;
    Procedure Replace(oField: TWPDocumentField; oBuilder: TWPDocumentBuilder);
      Overload; Override;
  End;

{ TWordProcessorFormMemoPopupMenu }

type
  TWordProcessorFormMemoPopupMenu = class(TPopupMenu)
  public
    procedure Popup(X, Y: Integer); override;
  end;

procedure TWordProcessorFormMemoPopupMenu.Popup(X, Y: Integer);
var
  Form: TWordProcessorForm;
begin
  { Show the existing Edit menu }
  Form := Owner as TWordProcessorForm;
  TrackPopupMenu(Form.mnuEdit.Handle, TPM_RIGHTBUTTON, X, Y, 0, Form.Handle, nil);
end;


Procedure TWordProcessorForm.LoadForm;
Begin
  pgPages := TUixPageControl.Create(Self);
  pgPages.parent := Self;
  pgPages.AlignClient;
  pgPages.FullPages := True;
  pgWP := TUixTabsheet.Create(Self);
  pgWP.PageControl := pgPages;
  pgWP.Caption := 'WP';
  pgWP.OnResize := OnResizeCDASpace;

  pnlDocument := TUixPanel.Create(pgWP);
  pnlDocument.parent := pgWP;
  pnlDocument.AlignLeft;

  pnlSource := TUixPanel.Create(pgWP);
  pnlSource.parent := pgWP;
  pnlSource.AlignRight;

  spltView := TUixSplitter.Create(pgWP);
  spltView.parent := pgWP;
  spltView.Color := clGray;
  spltView.Width := 8;

  mnuMain := TMainMenu.Create(Self);
  LoadFileMenu;
  LoadEditMenu;
  LoadFormatMenu;
  LoadToolsMenu;
  LoadHelpMenu;
  if UnderDevelopment then
    LoadSpecialMenu;

  LoadDialogs;
  LoadStatusBar;
  If SHOW_CLIPBOARD Then
    LoadClipboard;
  If SHOW_TESTS Then
    LoadTests;
  If SHOW_PRINTERS Then
    LoadPrinters;

  OnCloseQuery := FormCloseQuery;
  OnShow := DoShow;

  pnlBottom.Top := 399;
  Left := 348;
  Top := 406;
  Caption := 'WordProcessorForm';
  Menu := mnuMain;
  OldCreateOrder := True;
  Position := poDefault;
  PixelsPerInch := 96;
End;

procedure TWordProcessorForm.LoadCDA(cda: TCDADocument);
var
  build: TWPDocumentBuilder;
  doc: TWPDocument;
begin
  pnlBanner.Visible := True;
  doc := BuildCDABanner(cda);
  try
    wrdBanner.DocumentHandler.LoadDocument(doc);
  finally
    doc.free;
  end;
end;

Procedure TWordProcessorForm.LoadClipboard;
Begin
  pgClipboard := TUixTabsheet.Create(Self);
  pgClipboard.PageControl := pgPages;
  pgClipboard.Caption := 'Clipboard';

  clpList := TListBox.Create(Self);
  clpList.parent := pgClipboard;
  clpList.Align := alLeft;
  clpList.OnClick := clpSelect;

  clpTimer := TTimer.Create(Self);
  clpTimer.Interval := 200;
  clpTimer.OnTimer := clpLoad;
  clpTimer.Enabled := True;

  clpSource := TUixMemo.Create(Self);
  clpSource.parent := pgClipboard;
  clpSource.AlignClient;
  clpSource.Font.Name := 'Courier New';
End;

Procedure TWordProcessorForm.LoadPrinters;
Var
  oPrintMgr: TFslPrinterManager;
  oPrinters: TFslPrinterList;
  i: integer;
Begin
  pgPrinters := TUixTabsheet.Create(Self);
  pgPrinters.PageControl := pgPages;
  pgPrinters.Caption := 'Printers';

  prnList := TListBox.Create(Self);
  prnList.parent := pgPrinters;
  prnList.Align := alLeft;
  prnList.OnClick := prnSelect;
  prnList.Width := 300;

  prnDetails := TUixMemo.Create(Self);
  prnDetails.parent := pgPrinters;
  prnDetails.AlignClient;
  prnDetails.Font.Name := 'Courier New';

  oPrinters := TFslPrinterList.Create;
  Try
    oPrintMgr := TFslPrinterManager.Create;
    Try
      oPrintMgr.Open;
      oPrintMgr.CompilePrinterList(oPrinters);
      oPrintMgr.Close;
    Finally
      oPrintMgr.free;
    End;
    For i := 0 To oPrinters.Count - 1 Do
      prnList.Items.Add(oPrinters[i].Definition.Name);
  Finally
    oPrinters.free;
  End;
End;

procedure TWordProcessorForm.LoadRecentList;
var
  config: TIniFile;
  mnu: TMenuItem;
  i: integer;
begin
  config := TIniFile.Create('cde.ini');
  try
    FRecentList.SaveText := config.ReadString('WP', 'Recent', '');
    FSourceVisible := config.ReadBool('WP', 'SourceVisible', True);
    FSourceWidth := config.ReadInteger('WP', 'SourceWidth', 300);
  finally
    config.free;
  end;
  mnuRecent.Clear;
  for i := 0 to FRecentList.Count - 1 do
    if FRecentList[i] <> FDocumentName then
    begin
      mnuRecent.Add(newMenuItem(mnu, FRecentList[i], -1, mnuOpenRecentClick));
      mnu.Tag := i;
    end;
  mnuRecent.Enabled := mnuRecent.Count > 0;
end;

Procedure TWordProcessorForm.LoadStatusBar;
Begin
  pnlStatus := TUixPanel.Create(pnlDocument);
  pnlStatus.parent := pnlDocument;
  pnlStatus.Left := 0;
  pnlStatus.Top := 380;
  pnlStatus.Width := 688;
  pnlStatus.Height := 19;
  pnlStatus.BevelOuterNone;
  // pnlStatus.BevelInnerLowered;
  // pnlStatus.BevelWidth := 2;
  pnlStatus.AlignBottom;

  sbpHotspot := TUixPanel.Create(Self);
  sbpHotspot.parent := pnlStatus;
  sbpHotspot.AlignLeft;
  sbpHotspot.Alignment := taLeftJustify;
  sbpHotspot.BevelOuterLowered;
  sbpHotspot.Hint := 'Link';
  sbpHotspot.Width := 150;

  sbpCursor := TUixPanel.Create(Self);
  sbpCursor.parent := pnlStatus;
  sbpCursor.AlignLeft;
  sbpCursor.Alignment := taLeftJustify;
  sbpCursor.BevelOuterLowered;
  sbpCursor.Hint := 'Selection';
  sbpCursor.Width := 200;

  sbpDirty := TUixPanel.Create(Self);
  sbpDirty.parent := pnlStatus;
  sbpDirty.AlignLeft;
  sbpDirty.Alignment := taLeftJustify;
  sbpDirty.BevelOuterLowered;
  sbpDirty.Hint := 'Document Status';
  sbpDirty.Width := 150;

  sbpFields := TUixPanel.Create(Self);
  sbpFields.parent := pnlStatus;
  sbpFields.AlignLeft;
  sbpFields.Alignment := taLeftJustify;
  sbpFields.BevelOuterLowered;
  sbpFields.Hint := 'Fields';
  sbpFields.Width := 150;

  sbpComments := TUixPanel.Create(Self);
  sbpComments.parent := pnlStatus;
  sbpComments.AlignLeft;
  sbpComments.Alignment := taLeftJustify;
  sbpComments.BevelOuterLowered;
  sbpComments.Hint := 'Annotations';
  sbpComments.Width := 150;

  sbpZoom := TUixPanel.Create(Self);
  sbpZoom.parent := pnlStatus;
  sbpZoom.AlignRight;
  sbpZoom.BevelOuterLowered;
  sbpZoom.Alignment := taLeftJustify;
  sbpZoom.Hint := 'Zoom %';
  sbpZoom.Width := 150;

  sbpFill := TUixPanel.Create(Self);
  sbpFill.parent := pnlStatus;
  sbpFill.AlignClient;
  sbpFill.BevelOuterLowered;
  sbpFill.Alignment := taLeftJustify;

  FZoomTrack := TTrackBar.Create(Self);
  FZoomTrack.parent := sbpZoom;
  FZoomTrack.Align := alRight;
  FZoomTrack.Width := 100;
  FZoomTrack.Min := 1; // 10%
  FZoomTrack.Max := 20; // 400%
  FZoomTrack.OnChange := ChangeZoom;
End;

Procedure TWordProcessorForm.LoadDialogs;
Begin
  odDocument := TWPOpenFileDialog.Create(Self);
  odDocument.AllowNative := True;
  odDocument.AllowSnapshot := UnderDevelopment;
  sdDocument := TWPSaveFileDialog.Create(Self);
  sdDocument.AllowNative := True;
  sdDocument.allowCDA := True;
  sdPDF := TSaveDialog.Create(self);
  sdPDF.DefaultExt := '.pdf';
  sdPDF.Title := 'Save as PDF';
End;

function TWordProcessorForm.LoadDicomDictinoary: TDicomDictionary;
var
  parser: TDicomDictionaryParser;
  reader: TFslZipReader;
  f: TFslFile;
  mem: TFslMemoryStream;
  errors: TFslStringList;
  i: integer;
Begin
  parser := TDicomDictionaryParser.Create;
  Try
    parser.Dictionary := TDicomDictionary.Create;
    if FileExists('c:\ProgramData\HL7Connect\data\dicom.cache') Then
    Begin
      reader := TFslZipReader.Create;
      Try
        f := TFslFile.Create('c:\ProgramData\HL7Connect\data\dicom.cache', fmOpenRead + fmShareDenyWrite);
        Try
          reader.Stream := f.Link;
        Finally
          f.free;
        End;
        reader.ReadZip;
        For i := 0 to reader.Parts.Count - 1 Do
        Begin
          mem := TFslMemoryStream.Create;
          Try
            mem.Buffer := reader.Parts[i].Link;
            parser.source := mem.Link;
            parser.Execute;
          Finally
            mem.free;
          End;
        End;
      Finally
        reader.free;
      End;
    End;
    parser.Dictionary.ResolveReferences(errors);
    Try
{$IFOPT C+}
      if errors.Count > 0 then
        ShowMessage(errors.AsText);
{$ENDIF}
    Finally
      errors.free;
    End;
    result := parser.Dictionary.Link;
  Finally
    parser.free;
  End;
end;

Function TWordProcessorForm.newMenuItem(Var i: TMenuItem;
  Const sCaption: String; index: integer; aAction: TNotifyEvent;
  aShortCut: TShortCut): TMenuItem;
Begin
  i := TMenuItem.Create(Self);
  i.Caption := sCaption;
  i.ImageIndex := index;
  i.OnClick := aAction;
  i.ShortCut := aShortCut;
  result := i;
End;

Procedure TWordProcessorForm.LoadFormatMenu;
Begin
  mnuMain.Items.Add(newMenuItem(mnuFormat, '&Format', -1, mnuFormatClick));
  mnuFormat.Add(newMenuItem(mnuFont, '&Font', 29, mnuFontClick));
  mnuFormat.Add(newMenuItem(mnuParagraph, '&Paragraph', 26, mnuParagraphClick));
  mnuFormat.Add(newSeparator);
  mnuFormat.Add(newMenuItem(mnuChangeCase, 'Change &Case', -1,
    mnuChangeCaseClick));
End;

Procedure TWordProcessorForm.LoadToolsMenu;
begin
  mnuMain.Items.Add(newMenuItem(mnuTools, '&Tools', -1, Nil));
  mnuTools.Add(newMenuItem(MnuItmSpelling, 'Check &Spelling', -1,
    SpellingClick));
  mnuTools.Add(newMenuItem(MnuItmStyleEditor, 'Style &Editor', -1,
    StyleEditorClick));
  mnuTools.Add(newMenuItem(MnuItmOids, 'Get &Oid updates', -1, OidUpdateClick));
  // mnuTools.Add(newMenuItem(MnuItmDictEditor, '&Dictionary Editor', -1, DictEditorClick));
  mnuTools.Add(newMenuItem(MnuItmOptions, '&Options', -1, ToolsOptionsClick));

end;

Procedure TWordProcessorForm.LoadHelpMenu;
begin
  mnuMain.Items.Add(newMenuItem(mnuHelp, '&Help', -1, Nil));
  mnuHelp.Add(newMenuItem(mnuHelpIndex, '&Index', -1, HelpIndexClick));
  mnuHelp.Add(newMenuItem(mnuHelpWebSite, '&Web Site', -1, HelpWebSiteClick));
  mnuHelp.Add(newMenuItem(mnuHelpCheckUpgrade, '&Upgrade Check', -1,
    HelpCheckUpgradeClick));
  mnuHelp.Add(newMenuItem(mnuHelpSendSupportCase, '&Send Support Case', -1,
    HelpSendSupportCaseClick));
  mnuHelp.Add(newMenuItem(mnuHelpAbout, '&About', -1, HelpAboutClick));
end;

Procedure TWordProcessorForm.LoadSpecialMenu;
Begin
  mnuMain.Items.Add(newMenuItem(mnuSpecial, 'Special', -1, Nil));
  mnuSpecial.Add(newMenuItem(Error1, 'Error', -1, Error1Click));
  mnuSpecial.Add(newMenuItem(Yield1, 'Yield', -1, Yield1Click));
  mnuSpecial.Add(newMenuItem(HL71, 'Load HL7', -1, HL71Click));
  mnuSpecial.Add(newMenuItem(HL71, 'Produce HL7', -1, HL72Click));
  mnuSpecial.Add(newMenuItem(MD1, 'Medical Director Export', -1, MD1Click));
  mnuSpecial.Add(newMenuItem(HL71, 'Field Replace', -1, ReplaceFieldClick));
  mnuSpecial.Add(newMenuItem(HL71, 'Doc Field Process', -1,
    DocFieldProcessClick));
  mnuSpecial.Add(newMenuItem(HL71, 'Live Field Process', -1,
    LiveFieldProcessClick));
  mnuSpecial.Add(newMenuItem(HL71, 'Copy all', -1, CopyAllClick));
  mnuSpecial.Add(newMenuItem(HL71, 'SpeechMagic Populate', -1,
    SpeechMagicPopulateClick));
  mnuSpecial.Add(newMenuItem(HL71, 'Check Fields', -1, CheckFieldsClick));
  mnuSpecial.Add(newMenuItem(HL71, 'Spawn WP', -1, SpawnWPEditor,
    StringToShortcut('Ctrl+Alt+S')));
  mnuSpecial.Add(newMenuItem(mnuTextOnly, 'Text Only', -1, mnuTextOnlyClick));
  mnuSpecial.Add(newMenuItem(mnuStyle, '&Style', 26, mnuStyleClick));
  mnuSpecial.Add(newMenuItem(mnuKarismaConnect, '&Connect', -1,
    mnuKarismaConnectClick));
  mnuSpecial.Add(newMenuItem(mnuImageMode, '&Images', -1, mnuImageModeClick));
  mnuSpecial.Add(newMenuItem(mnuFormatMode, '&Formatting', -1,
    mnuFormatModeClick));
  mnuSpecial.Add(newMenuItem(mnuConsoleMode, '&Console Mode', -1,
    mnuConsoleModeClick));
  mnuSpecial.Add(newMenuItem(mnuNoParagraphMode, 'No &Paragraph Mode', -1,
    mnuNoParagraphModeClick));
  mnuSpecial.Add(newMenuItem(mnuReadOnly, 'Read Only', -1, mnuReadOnlyClick));
  mnuSpecial.Add(newMenuItem(NoTableLines1, 'No Table Lines', -1,
    NoTableLines1Click));
  mnuSpecial.Add(newMenuItem(LowLight1, 'Low Light', -1, LowLight1Click));
  mnuSpecial.Add(newMenuItem(FormsMode1, 'Forms Mode', -1, FormsMode1Click));
  mnuSpecial.Add(newMenuItem(SpellingOption, 'Check Spelling', -1,
    SpellingOptionClick));
  SpellingOption.Checked := True;
End;

Function TWordProcessorForm.newSeparator: TMenuItem;
Begin
  result := TMenuItem.Create(Self);
  result.Caption := '-';
End;

Procedure TWordProcessorForm.LoadEditMenu;
Begin
  mnuMain.Items.Add(newMenuItem(mnuEdit, '&Edit', -1, mnuEditClick));

  mnuEdit.Add(newMenuItem(mnuUndo, '&Undo', 7, mnuUndoClick));
  mnuEdit.Add(newMenuItem(mnuRedo, '&Redo', 8, mnuRedoClick));
  mnuEdit.Add(newSeparator);
  mnuEdit.Add(newMenuItem(mnuCut, 'Cu&t', 4, mnuCutClick));
  mnuEdit.Add(newMenuItem(mnuCopy, '&Copy', 5, mnuCopyClick));
  mnuEdit.Add(newMenuItem(mnuPaste, '&Paste', 6, mnuPasteClick));
  mnuEdit.Add(newMenuItem(mnuPasteSpecial, 'Paste / &Import', -1,
    mnuPasteSpecialClick));
  mnuEdit.Add(newSeparator);
  mnuEdit.Add(newMenuItem(mnuFind, '&Find', 10, mnuFindClick));
  mnuEdit.Add(newMenuItem(mnuReplace, '&Replace', -1, mnuReplaceClick));
  mnuEdit.Add(newMenuItem(mnuCopyAll, 'Copy &All', 5, mnuCopyAllClick));
  mnuEdit.Add(newMenuItem(mnuCopyFilename, 'Copy &Filename', 5,
    mnuCopyFilenameClick));
End;

Procedure TWordProcessorForm.LoadFileMenu;
Begin
  mnuMain.Items.Add(newMenuItem(mnuFile, '&File', -1, mnuFileClick));

  mnuFile.Add(newMenuItem(mnuNew, '&New', 0, mnuNewClick));
  mnuFile.Add(newMenuItem(mnuOpen, '&Open', 1, mnuOpenClick));
  mnuFile.Add(newMenuItem(mnuRecent, '&Recent', 1, nil));
  mnuFile.Add(newMenuItem(mnuSave, '&Save', 2, mnuSaveClick));
  mnuFile.Add(newMenuItem(mnuSaveAs, 'Save &As...', -1, mnuSaveAsClick));
  mnuFile.Add(newSeparator);
  mnuFile.Add(newMenuItem(mnuPrint, '&Print', 3, mnuPrintClick));
  mnuFile.Add(newMenuItem(mnuPrintPreview, '&Fax', -1, mnuFaxClick));
  mnuFile.Add(newMenuItem(mnuPrintPreview, 'P&DF', -1, mnuPDFClick));
  mnuFile.Add(newMenuItem(mnuPrintPreview, '&Email', -1, mnuEmailClick));
  mnuFile.Add(newSeparator);
  mnuFile.Add(newMenuItem(mnuExit, 'E&xit', -1, mnuExitClick));
End;

Function readFormatParam(param: string): TWPFormat;
var
  i: integer;
begin
  i := StringArrayIndexOf(WPFORMAT_NAMES, param);
  if i = -1 then
    result := wpfUnknown
  else
    result := TWPFormat(i);
end;

Procedure TWordProcessorForm.Initialise;
Var
  oDefn: TWPModelFieldDefinitionProvider;
  oSnomed: TWPSnomedDefinitionProvider;
Begin
  Inherited;
  FRecentList := TRecentFileList.Create;
  FZooming := false;
  FSourceVisible := True;

  LoadForm;
  LoadRecentList;

{  FReportScribe := TWordProcessorPublishScribe.Create;
  FReportScribe.Pattern := TWordProcessorPublishPattern.Create;
  FReportScribe.Compile;}
  wrdToolbar := TWordProcessorToolbar.Create(pnlDocument);
  wrdToolbar.parent := pnlDocument;
  wrdToolbar.AlignTop;
  wrdToolbar.onNew := mnuNewClick;
  wrdToolbar.onOpen := mnuOpenClick;
  wrdToolbar.onSave := mnuSaveClick;
  wrdToolbar.onSaveAs := mnuSaveAsClick;
  wrdToolbar.onPrint := mnuPrintClick;

  wrdToolbar.VisibleWidgetSet := wrdToolbar.VisibleWidgetSet + [tbwMacro, tbwSpelling, tbwNew, tbwOpen, tbwSave];

  // wrdToolbar.AddSeparator;
  // FSnomedBtn := wrdToolbar.AddButton('Snomed Code', SnomedBtnClicked, WPIconModule.VOICE);

  // wrdEditor := TWordProcessor.Create(Self); // to enable table manipulation

{$IFDEF UNICODE}
  wrdTouchToolbar := TWordProcessorTouchToolbar.Create(pnlDocument);
  wrdTouchToolbar.parent := pnlDocument;
  wrdTouchToolbar.AlignTop;
  wrdTouchToolbar.VisibleWidgetSet := wrdTouchToolbar.VisibleWidgetSet +
    [tbwMacro, tbwSpelling];
  wrdTouchToolbar.onNew := mnuNewClick;
  wrdTouchToolbar.onOpen := mnuOpenClick;
  wrdTouchToolbar.onSave := mnuSaveClick;
  wrdTouchToolbar.onSaveAs := mnuSaveAsClick;
  // wrdTouchToolbar.OnEmail := mnuEmailClick;
  wrdTouchToolbar.OnExit := mnuExitClick;
  wrdTouchToolbar.OnOptions := ToolsOptionsClick;
  wrdTouchToolbar.onPrint := mnuPrintClick;
  wrdTouchToolbar.OnFax := mnuFaxClick;
  wrdTouchToolbar.OnCopyFilename := mnuCopyFilenameClick;
  wrdTouchToolbar.OnHelpIndex := HelpIndexClick;
  wrdTouchToolbar.OnHelpHome := HelpWebSiteClick;
  wrdTouchToolbar.OnHelpVersion := HelpCheckUpgradeClick;
  wrdTouchToolbar.OnHelpSupportCase := HelpSendSupportCaseClick;
{$ENDIF}
  pnlBanner := TUixPanel.Create(pnlDocument);
  pnlBanner.parent := pnlDocument;
  pnlBanner.AlignTop;
  pnlBanner.Height := 80;
  pnlBanner.BevelOuterLowered;
  pnlBanner.BevelInnerRaised;
  pnlEditHeader := TUixPanel.Create(pnlDocument);
  pnlEditHeader.parent := pnlBanner;
  pnlEditHeader.AlignRight;
  pnlEditHeader.Width := 60;
  pnlEditHeader.BevelOuterNone;
  pnlEditHeader.BevelInnerNone;

  btnEditHeader := TUixButton.Create(pnlEditHeader);
  btnEditHeader.parent := pnlEditHeader;
  btnEditHeader.Top := 1;
  btnEditHeader.Left := 1;
  btnEditHeader.Width := pnlEditHeader.Width - 2;
  btnEditHeader.Height := pnlBanner.Height - 7;
  btnEditHeader.Caption := 'Edit Header';
  btnEditHeader.Visible := True;
//tt  btnEditHeader.WordWrap := True;
  btnEditHeader.OnClick := EditCDAHeader;

  wrdBanner := TWordProcessor.Create(pnlDocument);
  wrdBanner.parent := pnlBanner;
  wrdBanner.AlignClient;
  wrdBanner.Color := $D2FAFA;
  wrdBanner.Settings.Margin := 10;
  wrdBanner.Settings.Interactive := false;

  pnlViewSource := TUixPanel.Create(pnlSource);
  pnlViewSource.parent := pnlSource;
  pnlViewSource.BevelInner := bvNone;
  pnlViewSource.BevelOuter := bvNone;
  pnlViewSource.ParentColor := false;
  pnlViewSource.Color := clBtnFace;
  pnlViewSource.BevelWidth := 1;
//tt  pnlViewSource.Margins.Left := 10;
  pnlViewSource.Width := 500;
  pnlViewSource.Align := alClient;
  pnlViewSource.Transparent := false;
  pnlViewSource.OnResize := SourceViewResize;

  pnlSourceHeader := TUixPanel.Create(pgWP);
  pnlSourceHeader.parent := pnlViewSource;
  pnlSourceHeader.BevelInner := bvNone;
  pnlSourceHeader.BevelOuter := bvNone;
  pnlSourceHeader.BevelWidth := 1;
  pnlSourceHeader.ParentColor := false;
  pnlSourceHeader.Color := clBlack;
  pnlSourceHeader.Height := 24;
  pnlSourceHeader.Align := altop;
  pnlSourceHeader.Alignment := taLeftJustify;
  pnlSourceHeader.Caption := '      Source View';
  pnlSourceHeader.Transparent := false;

{$IFDEF SCINTILLA}
  sciCDA := TCDAStyler.Create(Self);

  edtSource := TISScintEdit.Create(Self);
  edtSource.AcceptDroppedFiles := false; // True;
  edtSource.Align := alClient;
  edtSource.AutoCompleteFontName := Font.Name;
  edtSource.AutoCompleteFontSize := Font.Size;
  edtSource.CodePage := CP_UTF8;
  edtSource.Font.Name := 'Courier New';
  edtSource.Font.Size := 10;
  edtSource.ShowHint := True;

  edtSource.PopupMenu := TWordProcessorFormMemoPopupMenu.Create(Self);
  edtSource.OnChange := SourceChange;
  // sciMemo.OnCharAdded := MemoCharAdded;
  // sciMemo.OnDropFiles := MemoDropFiles;
  // sciMemo.OnHintShow := MemoHintShow;
  // sciMemo.OnKeyDown := MemoKeyDown;
  // sciMemo.OnKeyPress := MemoKeyPress;
  // sciMemo.OnMarginClick := MemoMarginClick;
  // sciMemo.OnModifiedChange := MemoModifiedChange;
  // sciMemo.OnUpdateUI := MemoUpdateUI;
  edtSource.parent := pnlViewSource;
  edtSource.LineNumbers := True;
  edtSource.Styler := sciCDA;

{$ELSE}
  synXml := TSynXMLSyn.Create(pgWP);
  edtSource := TSynEdit.Create(pgWP);
  edtSource.parent := pnlViewSource;
  edtSource.Align := alClient;
  edtSource.Gutter.Visible := True;
  edtSource.Gutter.ShowLineNumbers := True;
  edtSource.ActiveLineColor := $EEEEEE;
  edtSource.Highlighter := synXml;
{$ENDIF}
  { btnSource := TUixButton.Create(pnlSourceHeader);
    btnSource.Parent := pnlSourceHeader;
    btnSource.Caption := '>';
    btnSource.Left := 0;
    btnSource.Top := 1;
    btnSource.Width := 20;
    btnSource.Height := 20;
    btnSource.OnClick := ToggleSourceView;

    ApplySourceViewSettings;
  }

  wrdEditor := TWordProcessor.Create(Self);
  wrdEditor.parent := pnlDocument;
  wrdEditor.AlignClient;
  wrdEditor.Top := 40;
  wrdEditor.Left := 40;
  wrdEditor.Width := 672;
  wrdEditor.Height := 600;
  // wrdEditor.Color := clAqua;
  wrdEditor.Settings.AllowPopup := True;
  wrdEditor.onNew := mnuNewClick;
  wrdEditor.onOpen := mnuOpenClick;
  wrdEditor.onSave := mnuSaveClick;
  wrdEditor.onSaveAs := mnuSaveAsClick;
  wrdEditor.onPrint := mnuPrintClick;

  wrdToolbar.WordProcessor := wrdEditor;
{$IFDEF UNICODE}
  wrdTouchToolbar.WordProcessor := wrdEditor;
{$ENDIF}
  wrdEditor.OnContentChanged := ContentChange;
  wrdEditor.OnSelectionChanged := SelectionChange;
  wrdEditor.OnSettingsChanged := SettingsChanged;
  wrdEditor.OnCodeCompletion := CodeCompletion;
  wrdEditor.OnNamedTemplate := NamedTemplate;
  wrdEditor.OnTemplate := DoTemplate;
  wrdEditor.OnHotspot := HotSpot;
  wrdEditor.OnHotspotHover := HotSpotHover;
  wrdEditor.OnUpdateField := UpdateField;
  wrdEditor.OnDocumentLoaded := DoDocumentLoaded;
  wrdEditor.Settings.Margin := 20;
  wrdEditor.Settings.SmartParagraphs := True;
  wrdEditor.ShowHint := True;
  wrdEditor.Settings.DicomDictionary := LoadDicomDictinoary;

  wrdEditor.DefineFieldProvider(TWPInputFieldDefinitionProvider.Create(wrdEditor), false);
  wrdEditor.DefineFieldProvider(TWPHotspotFieldDefinitionProvider.Create(wrdEditor), false);
  wrdEditor.DefineFieldProvider(TWPValueFieldDefinitionProvider.Create(), false);
  // wrdEditor.DefineFieldProvider(TWPTestFieldDefiner.Create, False);
  wrdEditor.DefineAnnotationProvider(TWPCommentDefinitionProvider.Create(wrdEditor));
  oSnomed := TWPSnomedDefinitionProvider.Create(wrdEditor);
  oSnomed.Client := TSnomedClient.Create;

  wrdEditor.DefineAnnotationProvider(oSnomed);

  oDefn := TWPModelFieldDefinitionProvider.Create(wrdEditor);
  Try
    oDefn.Namespace := 'tools/wp/test';
    oDefn.Model.Title := 'Variable';
    oDefn.TouchIconIndex := TOUCH_ICON_MODEL;
    oDefn.Model.Entries.Add(TWPFieldEntry.Create('Simple', 'Simple',
      'Simple Field', false));
    oDefn.Model.Entries.Add(TWPFieldEntry.Create('Complex', 'Complex',
      'Complex Field', True));

    wrdEditor.DefineFieldProvider(oDefn.Link, false);
  Finally
    oDefn.free;
  End;

  wrdEditor.DocumentHandler.OnNewDocument := WPNewDoc;
  // wrdEditor.WorkingWidthLimit := 500;
  // wrdEditor.Styles.Clear;
  If Not wrdEditor.ConfiguredStyles.HasDefaultStyle Then
    wrdEditor.ConfiguredStyles.DefaultStyle := CreateNormalStyle
  Else
  Begin
    // wrdEditor.ConfiguredStyles.DefaultStyle.Font.Name := 'Symbol';
  End;
  wrdEditor.ConfiguredStyles.Add(CreateExampleStyle);
  wrdEditor.ConfiguredStyles.Add(CreateHeadingStyle);
  wrdEditor.ConfiguredStyles.Add(CreateUppercaseStyle);
  // wrdEditor.Scale := 1.5;

  wrdEditor.Settings.Hold;
  Try
    wrdEditor.Settings.Images := True;
    wrdEditor.Settings.TableBorders := false;
    wrdEditor.Settings.SnapshotEmail := 'grahameg@gmail.com';
    wrdEditor.Settings.ShowDocumentInspector := True;
    wrdEditor.Settings.Background := clWhite; // $FFD5CF;
    wrdEditor.Settings.LinkColour := clNavy;
    wrdEditor.Settings.HoverColour := clMaroon;
    wrdEditor.Settings.Pagination := True;
    wrdEditor.Settings.PrintBackgrounds := wpbpInColour;
    wrdEditor.Settings.SpellCheckUppercaseWords := True;
    wrdEditor.Settings.AllowSpecialSymbols := True;
    wrdEditor.Settings.TextWrapWidth := 90;
    wrdEditor.Settings.EditHints := false;
    wrdEditor.Settings.CapitaliseFirstInSentence := True;
    wrdEditor.Settings.InsertTemplates := True;
    wrdEditor.Settings.TouchMode := wptmAlways;
    wrdEditor.Settings.Hotspots := wphmAltKey;
  Finally
    wrdEditor.Settings.Release;
  End;

{$IFDEF ADDICT}
  wrdEditor.Speller := TWPAddictSpeller.Create
    (TWPAddictDictionary.Create('C:\Temp',
    wrdEditor.Settings.SpellCheckUppercaseWords));
{$ENDIF}
  wrdEditor.Printer := getDefaultPrinter;
  wrdEditor.PageLayoutController := BuildPageLayoutController;

  If (ParamStr(1) <> '') And FileExists(ParamStr(1)) Then
    Open(ParamStr(1), readFormatParam(ParamStr(2)))
  Else If ParamStr(1) = '-s1' Then
    Sample1Click(Self)
  Else If ParamStr(1) = '-s:sect' Then
    SampleSectClick(Self)
  Else If ParamStr(1) = '-s2' Then
    Sample2Click(Self)
  Else If ParamStr(1) = '-s3' Then
    Sample3Click(Self)
  Else If ParamStr(1) = '-s4' Then
    Sample4Click(Self)
  Else If ParamStr(1) = '-s5' Then
    Sample5Click(Self)
  Else If ParamStr(1) = '-s6' Then
    Sample6Click(Self)
  Else If ParamStr(1) = '-s7' Then
    Sample7Click(Self)
  Else If ParamStr(1) = '-s8' Then
    Sample8Click(Self)
  Else If ParamStr(1) = '-h1' Then
    HL71Click(Self)
  Else
    wrdEditor.Clear;

  if UnderDevelopment then
  begin
    mnuImageMode.Checked := True;
    mnuFormatMode.Checked := True;
  end;

  pnlBottom.Visible := false;
  pnlStatus.parent := Self;

  IsDirty := false;
  Refresh;

  If ParamStr(1) = '-open' Then
    mnuOpenClick(Self);

  wrdEditor.Settings.ModeWordProcessor;
  TCDEOptionsForm.Initialise(wrdEditor);
  // wrdEditor.Settings.AnnotationWidth := 100;
  // wrdEditor.AlignTop;
  // wrdEditor.height := wrdEditor.DocumentHeight;
  SettingsChanged(Self);
End;

Procedure TWordProcessorForm.Finalise;
var
  config: TIniFile;
Begin
//  FReportScribe.free;
  FRecentList.free;

  { if FKarisma <> Nil Then
    Begin
    FKarisma.Close;
    FKarisma.Free;
    End;
  }
  config := TIniFile.create('cde.ini');
  try
    config.WriteBool('WP', 'SourceVisible', FSourceVisible);
    config.WriteInteger('WP', 'SourceWidth', FSourceWidth);
  finally
    config.free;
  end;
  Inherited;
End;

Function TWordProcessorForm.CreateNormalStyle: TWPStyle;
Begin
  result := TWPStyle.Create;
  result.Name := 'Normal';
  result.Font.Name := 'Arial';
  result.Font.Size := 12;
  result.Font.Bold := tsFalse;
  result.Font.Italic := tsFalse;
  result.Font.Underline := tsFalse;
  result.Font.State := fsNormal;
  result.Font.Foreground := clMaroon;
  result.Font.Background := clAqua;
End;

Function TWordProcessorForm.CreateExampleStyle: TWPStyle;
Begin
  result := TWPStyle.Create;
  result.Name := 'Example';
  result.Font.Name := 'Arial';
  result.Font.Size := 10;
  result.Font.Bold := tsFalse;
  result.Font.Italic := tsTrue;
  result.Font.Underline := tsFalse;
  result.Font.State := fsNormal;
  result.Font.Foreground := clNavy;
  result.Font.Background := clWhite;
  result.Paragraph.LeftIndent := 1;
  result.Paragraph.Align := WordProcessorParagraphAlignmentCentre;
End;

Function TWordProcessorForm.CreateUppercaseStyle: TWPStyle;
Begin
  result := TWPStyle.Create;
  result.Name := 'HeadingU';
  result.ResetOnNewParagraph := True;
  result.Font.Name := 'Arial';
  result.Font.Size := 14;
  result.Font.Bold := tsTrue;
  result.Font.Italic := tsFalse;
  result.Font.Underline := tsFalse;
  result.Font.State := fsNormal;
  result.Font.Capitalization := fcsAllCaps;
  result.Font.Foreground := HTMLColourStringToColour('Steelblue');
  result.Font.Background := clWhite;
  result.Paragraph.LeftIndent := 1;
End;

Function TWordProcessorForm.CreateHeadingStyle: TWPStyle;
Begin
  result := TWPStyle.Create;
  result.Name := 'Heading';
  result.ResetOnNewParagraph := True;
  result.Font.Name := 'Arial';
  result.Font.Size := 14;
  result.Font.Bold := tsTrue;
  result.Font.Italic := tsFalse;
  result.Font.Underline := tsFalse;
  result.Font.State := fsNormal;
  result.Font.Foreground := HTMLColourStringToColour('Steelblue');
  result.Font.Background := clWhite;
  result.Paragraph.LeftIndent := 1;
End;

Procedure TWordProcessorForm.mnuExitClick(Sender: TObject);
Begin
  Close;
End;

Procedure TWordProcessorForm.mnuNewClick(Sender: TObject);
Begin
  NewDocument;
End;

Procedure TWordProcessorForm.mnuOpenClick(Sender: TObject);
Begin
  If odDocument.Execute Then
    if Open(odDocument.Filename, odDocument.Format) Then
      Refresh;
End;

procedure TWordProcessorForm.mnuOpenRecentClick(Sender: TObject);
begin
  Open(FRecentList[(Sender as TComponent).Tag],
    TWPFormat(FRecentList.Objects[(Sender as TComponent).Tag]));
end;

Procedure TWordProcessorForm.mnuSaveAsClick(Sender: TObject);
Begin
  If sdDocument.Execute And Save(sdDocument.Filename, sdDocument.Format) Then
    Refresh;
End;

Procedure TWordProcessorForm.mnuSaveClick(Sender: TObject);
Begin
  If Not FileExists(FDocumentName) Then
    mnuSaveAs.Click
  Else If Save(FDocumentName, FOpenFormat) Then
    Refresh;
End;

Procedure TWordProcessorForm.mnuCloseClick(Sender: TObject);
Begin
  If CheckSave Then
  Begin
    IsDirty := false;
    NewDocument;
  End;
End;

procedure TWordProcessorForm.RecordRecentFile(sFilename: String;
  aFormat: TWPFormat);
var
  i: integer;
  config: TIniFile;
  mnu: TMenuItem;
begin
  repeat
    i := FRecentList.IndexOf(sFilename);
    if i > -1 then
      FRecentList.Delete(i);
  until i = -1;
  FRecentList.InsertObject(0, sFilename, TObject(aFormat));
  if FRecentList.Count > 20 then
    FRecentList.Delete(20);
  config := TIniFile.Create('cde.ini');
  try
    config.WriteString('WP', 'Recent', FRecentList.SaveText);
  finally
    config.free;
  end;
  mnuRecent.Clear;
  for i := 0 to FRecentList.Count - 1 do
    if FRecentList[i] <> FDocumentName then
    begin
      mnuRecent.Add(newMenuItem(mnu, FRecentList[i], -1, mnuOpenRecentClick));
      mnu.Tag := i;
    end;
  mnuRecent.Enabled := mnuRecent.Count > 0;
end;

Procedure TWordProcessorForm.Refresh;
Begin
  Inherited;

{$IFDEF VER130}
  Caption := DocumentTitle + ' - Word Processor'
{$ELSE}
  Caption := DocumentTitle + ' - Clinical Document Editor'
{$ENDIF}
End;

Function TWordProcessorForm.NewDocument: boolean;
Begin
  result := True;
  If FIsDirty Then
    ExecuteOpen(ParamStr(0))
  Else
  Begin
    FDocumentName := '';
    IsLoading := True;
    try
      wrdEditor.Clear;
    finally
      IsLoading := false;
    end;
    Refresh;
  End;
End;

procedure TWordProcessorForm.NewDocumentSource(source: TFslStream);
begin

end;

procedure TWordProcessorForm.OidUpdateClick(Sender: TObject);
begin
  OidFetcherForm := TOidFetcherForm.Create(Self);
  try
    OidFetcherForm.ShowModal;
  finally
    OidFetcherForm.free;
  end;
end;

procedure TWordProcessorForm.OnResizeCDASpace(Sender: TObject);
begin
  pnlDocument.Width := pgWP.Width div 2 - 2;
end;

Function TWordProcessorForm.Open(Const sFilename: String;
  aFormat: TWPFormat): boolean;
Begin
  result := FileExists(sFilename);

  If result Then
    If IsDirty Then
      ExecuteOpen(ParamStr(0), '"' + sFilename + '" ' + WPFORMAT_NAMES[aFormat])
    Else
    Begin
      IsLoading := True;
      try
        if aFormat = wpfUnknown then
          FOpenFormat := wrdEditor.DocumentHandler.LoadByExtension(sFilename)
        else
        begin
          FOpenFormat := aFormat;
          wrdEditor.DocumentHandler.loadByFormat(sFilename, aFormat);
        end;
      finally
        IsLoading := false;
      end;
      FDocumentName := sFilename;
      IsDirty := false;
    End;
  RecordRecentFile(sFilename, FOpenFormat);
  Caption := PathTitle(sFilename) + ' (' + sFilename + ') - CDE';
  Application.Title := Caption;
End;

Function TWordProcessorForm.Save(Const sFilename: String;
  aFormat: TWPFormat): boolean;
Begin
  result := True;

  FDocumentName := sFilename;
  if (aFormat = wpfUnknown) then
    aFormat := wrdEditor.DocumentHandler.SaveByExtension(sFilename)
  else
    wrdEditor.DocumentHandler.SaveByFormat(sFilename, aFormat);
  DoDocumentLoaded(Self);
  IsDirty := false;
  RecordRecentFile(sFilename, aFormat);
  Caption := PathTitle(sFilename) + ' (' + sFilename + ') - CDE';
  Application.Title := Caption;
End;

Procedure TWordProcessorForm.ContentChange(oSender: TObject);
var
  buf: TFslBuffer;
  i: integer;
Begin
  IsDirty := True;
  if not IsLoading then
  begin
    if (wrdEditor.Document.SourceObjectModel <> nil) and
      (wrdEditor.Document.SourceObjectModel is TCDADocument) then
    begin
      for i := 0 to wrdEditor.Document.Pieces.Count - 1 do
        wrdEditor.Document.Pieces[i].ClearSource;

      buf := TFslBuffer.Create;
      try
        wrdEditor.DocumentHandler.SaveByFormat(buf, wpfCDA,
          (wrdEditor.Document.SourceObjectModel as TCDADocument).Title);
        DoDocumentLoaded(Self);
      finally
        buf.free;
      end;
    end;
  end;
End;

Procedure TWordProcessorForm.SelectionChange(oSender: TObject);
var
  s: String;
{$IFDEF SCINTILLA}
{$ELSE}
  b: TBufferCoord;
{$ENDIF}
  line, col: integer;
Begin
  sbpCursor.Caption := '  ' + wrdEditor.SelectionSummary + ' [' +
    wrdEditor.Selection.Summary + ']';
  if not wrdEditor.hasFields then
    sbpFields.Caption := '  ' + ''
  else if wrdEditor.AllFieldsValid(s) then
    sbpFields.Caption := '  ' + 'Fields OK'
  else
    sbpFields.Caption := '  ' + 'Fields not OK (' + s + ')';
  sbpComments.Caption := '  ' +
    inttostr(wrdEditor.Document.AllAnnotations.Count) + ' Comments';
  if wrdEditor.GetSourceLocation(line, col) then
  begin
{$IFDEF SCINTILLA}
    edtSource.CaretLine := line - 1;
    edtSource.CaretColumn := col;
    edtSource.ScrollCaretIntoView;
{$ELSE}
    b.line := line;
    b.Char := 1;
    edtSource.SelStart := edtSource.RowColToCharIndex(b);
{$ENDIF}
  end;
End;

Function TWordProcessorForm.CheckSave: boolean;
Begin
  result := Not IsDirty;
  If Not result Then
    Case MessageDlg('Save ' + DocumentTitle + '?', mtConfirmation,
      mbYesNoCancel, 0) Of
      mrYes:
        If FDocumentName = '' Then
          result := sdDocument.Execute And Save(sdDocument.Filename,
            sdDocument.Format)
        Else
          result := Save(FDocumentName, sdDocument.Format);
      mrNo:
        result := True;
    Else
      result := false;
    End;
End;

Procedure TWordProcessorForm.SetIsDirty(Const Value: boolean);
Begin
  FIsDirty := Value;
  If FIsDirty Then
    sbpDirty.Caption := '  ' + 'Modified'
  Else
    sbpDirty.Caption := '';
End;

function ScaleForZoomTrack(i: integer): real;
begin
  case i of
    1:
      result := 0.1;
    2:
      result := 0.2;
    3:
      result := 0.3;
    4:
      result := 0.4;
    5:
      result := 0.5;
    6:
      result := 0.6;
    7:
      result := 0.7;
    8:
      result := 0.8;
    9:
      result := 0.9;
    10:
      result := 1.0;
    11:
      result := 1.1;
    12:
      result := 1.20;
    13:
      result := 1.30;
    14:
      result := 1.40;
    15:
      result := 1.50;
    16:
      result := 2.00;
    17:
      result := 2.50;
    18:
      result := 3.00;
    19:
      result := 3.50;
    20:
      result := 4.00;
  else
    result := 1;
  end;
end;

function ZoomTrackForScale(r: real): integer;
begin
  if r <= 0.1 then
    result := 1
  else if r <= 0.2 then
    result := 2
  else if r <= 0.3 then
    result := 3
  else if r <= 0.4 then
    result := 4
  else if r <= 0.5 then
    result := 5
  else if r <= 0.6 then
    result := 6
  else if r <= 0.7 then
    result := 7
  else if r <= 0.8 then
    result := 8
  else if r <= 0.9 then
    result := 9
  else if (r >= 0.9) and (r <= 1.1) then
    result := 10
  else if r >= 1.1 then
    result := 11
  else if r >= 1.20 then
    result := 12
  else if r >= 1.30 then
    result := 13
  else if r >= 1.40 then
    result := 14
  else if r >= 1.50 then
    result := 15
  else if r >= 2.00 then
    result := 16
  else if r >= 2.50 then
    result := 17
  else if r >= 3.00 then
    result := 18
  else if r >= 3.50 then
    result := 19
  else // if r >= 4 then
    result := 20;
end;

procedure TWordProcessorForm.SettingsChanged(Sender: TObject);
begin
  if wrdEditor.InTouchMode then
  begin
{$IFDEF UNICODE}
    wrdTouchToolbar.Visible := True;
{$ENDIF}
    wrdToolbar.Visible := false;
    Menu := nil;
  end
  else
  begin
{$IFDEF UNICODE}
    wrdTouchToolbar.Visible := false;
{$ENDIF}
    wrdToolbar.Visible := True;
    Menu := mnuMain;
  end;
  if not FZooming then
    FZoomTrack.Position := ZoomTrackForScale(wrdEditor.Settings.Scale);
  sbpZoom.Caption := ' ' +
    inttostr(trunc(wrdEditor.Settings.Scale * 100)) + '%';
end;

Function TWordProcessorForm.DocumentTitle: String;
Begin
  If FDocumentName <> '' Then
    result := FDocumentName
  Else
    result := NEW_DOC_NAME;
End;

procedure TWordProcessorForm.DoDocumentLoaded(Sender: TObject);
var
  mem: TFslMemoryStream;
  vcl: TVCLStream;
  line, col: integer;
begin
  if wrdEditor.Document.SourceBytes <> nil then
  begin
    mem := TFslMemoryStream.Create;
    try
      mem.Buffer := wrdEditor.Document.SourceBytes.Link;
      vcl := TVCLStream.Create;
      try
        vcl.Stream := mem.Link;
        edtSource.Lines.LoadFromStream(vcl);
      finally
        vcl.free;
      end;

    finally
      mem.free;
    end;
    if wrdEditor.GetSourceLocation(line, col) then
    begin
{$IFDEF SCINTILLA}
      edtSource.CaretLine := line - 1;
      edtSource.CaretColumn := col;
      edtSource.ScrollCaretIntoView;
{$ELSE}
      b.line := line;
      b.Char := 1;
      edtSource.SelStart := edtSource.RowColToCharIndex(b);
{$ENDIF}
    end;
  end
  else
  begin
    edtSource.Lines.Clear;
  end;
  if (wrdEditor.Document.SourceObjectModel <> nil) and
    (wrdEditor.Document.SourceObjectModel is TCDADocument) then
    LoadCDA(wrdEditor.Document.SourceObjectModel as TCDADocument)
  else
    pnlBanner.Visible := false;
end;

Procedure TWordProcessorForm.FormCloseQuery(Sender: TObject;
  Var CanClose: boolean);
Begin
  Inherited;
  CanClose := CheckSave;
End;

Procedure TWordProcessorForm.mnuFileClick(Sender: TObject);
Begin
  Inherited;
  mnuSave.Enabled := FIsDirty;
End;

Procedure TWordProcessorForm.mnuEditClick(Sender: TObject);
var
  srcFocus : Boolean;
Begin
  Inherited;
  mnuCopyFilename.Enabled := true;
  mnuFind.Enabled := true;
  mnuReplace.Enabled := true;
  if edtSource.Focused then
  begin
    mnuUndo.Enabled := edtSource.CanUndo;
    mnuRedo.Enabled := edtSource.CanRedo;
    mnuCut.Enabled := edtSource.SelAvail;
    mnuCopy.Enabled := canWrite In wrdEditor.Capabilities;
    mnuCopyAll.Enabled := True;
    mnuPaste.Enabled := Clipboard.HasText;
    mnuPasteSpecial.Enabled := False;
  end
  else if wrdEditor.Focused then
  begin
    mnuUndo.Enabled := canUndo In wrdEditor.Capabilities;
    mnuRedo.Enabled := canRedo In wrdEditor.Capabilities;
    mnuCut.Enabled := canCut In wrdEditor.Capabilities;
    mnuCopy.Enabled := canWrite In wrdEditor.Capabilities;
    mnuCopyAll.Enabled := True;
    mnuPaste.Enabled := (canInsert In wrdEditor.Capabilities) And Clipboard.CanPaste;
    mnuPasteSpecial.Enabled := (canInsert In wrdEditor.Capabilities) And Clipboard.CanPaste;
  end
  else
  begin
    mnuUndo.Enabled := false;
    mnuRedo.Enabled := false;
    mnuCut.Enabled := false;
    mnuCopy.Enabled := false;
    mnuCopyAll.Enabled := true;
    mnuPaste.Enabled := false;
    mnuPasteSpecial.Enabled := false;
    mnuFind.Enabled := false;
    mnuReplace.Enabled := false;
  end;
End;

Procedure TWordProcessorForm.mnuUndoClick(Sender: TObject);
Begin
  if edtSource.Focused then
    edtSource.Undo
  else
    wrdEditor.Undo;
End;

Procedure TWordProcessorForm.mnuRedoClick(Sender: TObject);
Begin
  if edtSource.Focused then
    edtSource.Redo
  else
    wrdEditor.Redo;
End;

Procedure TWordProcessorForm.mnuCutClick(Sender: TObject);
Begin
  if edtSource.Focused then
    edtSource.CutToClipboard
  else
    wrdEditor.Cut;
End;

Procedure TWordProcessorForm.mnuCopyClick(Sender: TObject);
Begin
  if edtSource.Focused then
    edtSource.CopyToClipboard
  else
    wrdEditor.Copy;
End;

Procedure TWordProcessorForm.mnuCopyAllClick(Sender: TObject);
Begin
  if edtSource.Focused then
  begin
    edtSource.SelectAll;
    edtSource.CopyToClipboard;
  end
  else
    wrdEditor.CopyAllToClipboard;
End;

Procedure TWordProcessorForm.mnuCopyFilenameClick(Sender: TObject);
Begin
  Clipbrd.Clipboard.AsText := odDocument.Filename;
End;

Procedure TWordProcessorForm.mnuPasteClick(Sender: TObject);
Var
  sError: String;
Begin
  if edtSource.Focused then
    edtSource.PasteFromClipboard
  else If Not wrdEditor.Paste(sError) Then
    DialogError(sError);
End;

procedure TWordProcessorForm.mnuPasteImportClick(Sender: TObject);
begin
  Abort;
end;

Procedure TWordProcessorForm.mnuPasteSpecialClick(Sender: TObject);
Begin
  if edtSource.Focused then
    MessageBeep(MB_ICONEXCLAMATION)
  else
    wrdEditor.PasteSpecialDialog;
End;

Procedure TWordProcessorForm.mnuFontClick(Sender: TObject);
Begin
  Inherited;
  wrdEditor.FontDialog;
End;

Function TWordProcessorForm.CodeCompletion(oSender: TObject;
  Const sText: String; oItems: TWPCompletionItems): boolean;
Begin
  result := True;
  oItems.Add('Option 1', 'Content 1');
  oItems.Add('Option 2', '', 'Description 2');
  oItems.Add('Option 3', '');
End;

Procedure TWordProcessorForm.WPNewDoc(oSender: TObject; oDocument: TWPDocument);
Var
  oBuilder: TWPDocumentBuilder;
Begin
  oBuilder := TWPDocumentBuilder.Create(oDocument.Link);
  Try
    oBuilder.Start;
    oBuilder.StartParagraph;
    oBuilder.EndParagraph;
    oBuilder.Stop;
  Finally
    oBuilder.free;
  End;
End;

Procedure TWordProcessorForm.mnuImageModeClick(Sender: TObject);
Begin
  Inherited;
  wrdEditor.Settings.Images := Not wrdEditor.Settings.Images;
  mnuImageMode.Checked := wrdEditor.Settings.Images;
End;

Procedure TWordProcessorForm.mnuFormatModeClick(Sender: TObject);
Begin
  Inherited;
  wrdEditor.Settings.Format := Not wrdEditor.Settings.Format;
  mnuFormatMode.Checked := wrdEditor.Settings.Format;
End;

Procedure TWordProcessorForm.mnuConsoleModeClick(Sender: TObject);
Var
  sMsg: String;
Begin
  Inherited;
  If Not wrdEditor.Settings.ConsoleMode And Not wrdEditor.CheckOkForConsoleMode
    (sMsg) Then
    raise EWPException.create
      ('You cannot enter console mode because the document contains:'#13#10 +
      sMsg);
  wrdEditor.Settings.ConsoleMode := Not wrdEditor.Settings.ConsoleMode;
  mnuConsoleMode.Checked := wrdEditor.Settings.ConsoleMode;
End;

Procedure TWordProcessorForm.mnuNoParagraphModeClick(Sender: TObject);
Begin
  Inherited;
  wrdEditor.Settings.NoParagraphs := Not wrdEditor.Settings.NoParagraphs;
  mnuNoParagraphMode.Checked := wrdEditor.Settings.NoParagraphs;
End;

Procedure TWordProcessorForm.mnuTextOnlyClick(Sender: TObject);
Begin
  If mnuTextOnly.Checked Then
  Begin
    mnuTextOnly.Checked := false;
    wrdEditor.Settings.Format := True;
  End
  Else
  Begin
    mnuTextOnly.Checked := True;
    IsLoading := True;
    try
      wrdEditor.DocumentHandler.AsText := wrdEditor.DocumentHandler.AsText;
    finally
      IsLoading := false;
    end;
    wrdEditor.Settings.Format := false;
  End;
End;

Procedure TWordProcessorForm.mnuReadOnlyClick(Sender: TObject);
Begin
  Inherited;
  If Not wrdEditor.Settings.ReadOnly Then
    wrdEditor.Settings.ModeBrowser
  Else
    wrdEditor.Settings.ModeWordProcessor;
  mnuReadOnly.Checked := wrdEditor.Settings.ReadOnly;
End;

Procedure TWordProcessorForm.SampleSectClick(Sender: TObject);
Var
  oBuilder: TWPDocumentBuilder;
  oBorder: TWPBorder;
  oField: TWPDocumentField;
  oCell: TWPDocumentTableCell;
Begin
  wrdEditor.Settings.ModeBrowser;

  oBuilder := TWPDocumentBuilder.Create(TWPDocument.Create);
  Try
    oBuilder.Start;
    oBuilder.StartSection('Public', 'Report').Display := DisplayTitle;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('test');
    oBuilder.EndParagraph;
    oBuilder.EndSection;

    oBuilder.StartSection('Private', 'Report').Display := DisplayTitle;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('1');
    oBuilder.EndParagraph;
    oBuilder.EndSection;

    oBuilder.Stop;
    IsLoading := True;
    try
      wrdEditor.DocumentHandler.LoadDocument(oBuilder.Document);
    finally
      IsLoading := false;
    end;
  Finally
    oBuilder.free;
  End;
End;

Procedure TWordProcessorForm.Sample1Click(Sender: TObject);
Var
  oBuilder: TWPDocumentBuilder;
  oBorder: TWPBorder;
  oField: TWPDocumentField;
  oCell: TWPDocumentTableCell;
Begin
  wrdEditor.Settings.ModeBrowser;

  oBuilder := TWPDocumentBuilder.Create(TWPDocument.Create);
  Try
    oBuilder.Start;
    oBuilder.StartSection('Public', 'Report').Display := DisplayTitle;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('1');
    oBuilder.EndParagraph;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('2');
    oBuilder.EndParagraph;
    oBuilder.StartParagraph;
    oField := oBuilder.StartField(NS_FIELD_INPUT, '');
    oBuilder.DefineHotspot(DocRoot + '\Test Docs\ApplicationSystem.xml')
      .Key := 'A';
    oBuilder.AddTextPlain('text_in-field');
    oBuilder.EndField;

    // oField.HasHotspot := true;
    // oField.Hotspot.URL := '.clear'; {.ReadOnly := tsFalse};
    // oField.Hotspot.URL := 'popup'; {.ReadOnly := tsFalse};
    oField.DataValue[FIELD_DATA_NAME_LIST] := 'Test1,text2';
    oBuilder.EndParagraph;
    oBuilder.StartTable.BorderPolicy := BorderPolicyGrid;
    { oBuilder.StartTableRow;
      oCell := oBuilder.StartTableCell;
      oCell.Width := 1;
      oCell.Span := 3;
      oBuilder.StartParagraph;
      oBuilder.AddText('header');
      oBuilder.EndParagraph;
      oBuilder.EndTableCell;
      oBuilder.EndTableRow; }
    oBuilder.StartTableRow.ReadOnly := ReadOnlyTrue;
    // oBuilder.DefineHotspot('any_link:row', clTeal, clLime).Key := 'B';
    oCell := oBuilder.StartTableCell;
    oCell.Background := clYellow;
    oCell.VerticalAlignment := VerticalAlignmentCentered;
    oBuilder.StartParagraph.Format.Align :=
      WordProcessorParagraphAlignmentRight;
    oBuilder.AddImage(DocRoot + 'Test Docs\button-up-example.jpg')
      .VerticalAlignment := ImageVerticalAlignmentBottom;

    oBuilder.AddTextPlain('1,1');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;
    oBuilder.StartTableCell { .Width := 0.2 };
    // oBuilder.DefineHotspot('any_link:cell', clAqua, clSilver).Key := 'C';
    oBuilder.StartParagraph.Format.Align :=
      WordProcessorParagraphAlignmentRight;
    oBuilder.AddTextPlain('1,1');
    oBuilder.StartField(NS_FIELD_INPUT, 'test').ReadOnly := ReadOnlyFalse;
    // oBuilder.DefineHotspot('any_link:field', clOlive, clBlack);
    oBuilder.AddTextPlain('sssssssssssssssssssssssssssssssssssssssss');
    oBuilder.EndField;
    oBuilder.EndParagraph;
    oBuilder.StartParagraph.Format.Align :=
      WordProcessorParagraphAlignmentRight;
    oBuilder.AddTextPlain
      ('ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;
    oBorder := oBuilder.StartTableCell.BottomBorder;
    oBorder.Defined := True;
    oBorder.Width := 4;
    oBorder.Colour := clNavy;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('1,3');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;
    oBuilder.EndTableRow;

    oBuilder.StartTableRow { (tsTrue) };
    oBorder := oBuilder.StartTableCell.LeftBorder;
    oBorder.Defined := True;
    oBorder.Width := 4;
    oBorder.Colour := clNavy;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('2,1');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;
    oBuilder.StartTableCell.VerticalAlignment := VerticalAlignmentCentered;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('2,2');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;
    { oBuilder.StartTableCell;
      oBuilder.StartParagraph;
      oBuilder.AddTextPlain('2,3');
      oBuilder.EndParagraph;
      oBuilder.EndTableCell; }
    oBuilder.EndTableRow;

    oBuilder.StartTableRow { (tsTrue) };
    // oBuilder.DefineHotspot('any_link:row2', clTeal, clLime);
    oBuilder.StartTableCell.Span := 2;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('3,1');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;
    oBuilder.StartTableCell;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('3,3');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;
    oBuilder.EndTableRow;

    oBuilder.EndTable;

    { oBuilder.StartParagraph;
      oBuilder.AddTextPlain('2,2');
      oBuilder.EndParagraph; }

    oBuilder.EndSection;
    { oSection := oBuilder.StartSection('Private', 'Private Comments');
      oSection.ReadOnly := ReadOnlyTrue;
      oSection.Display := DisplayTitle;
      oBuilder.StartParagraph;
      oBuilder.AddTextPlain('Testxx');
      oBuilder.EndParagraph;
      oBuilder.EndSection;

      oBuilder.StartParagraph;
      oBuilder.AddTextPlain('Patient has ', False, False);
      oField := oBuilder.StartField;
      oField.HasHotspot := True;
      oField.Hotspot.URL := 'Medical Alerts';
      oBuilder.AddTextPlain('medical alerts', True, False);
      oBuilder.EndField;
      oBuilder.AddTextPlain('.', False, False);
      oBuilder.EndParagraph; }

    oBuilder.Stop;
    IsLoading := True;
    try
      wrdEditor.DocumentHandler.LoadDocument(oBuilder.Document);
    finally
      IsLoading := false;
    end;
  Finally
    oBuilder.free;
  End;
End;

Procedure TWordProcessorForm.Sample2Click(Sender: TObject);
Var
  oBuilder: TWPDocumentBuilder;
  oTable: TWPDocumentTable;
  oRow: TWPDocumentTableRow;
  oRow1: TWPDocumentTableRow;
Begin
  oBuilder := TWPDocumentBuilder.Create(TWPDocument.Create);
  Try
    oBuilder.Start;
    oTable := oBuilder.StartTable;
    oTable.BorderPolicy := BorderPolicyGrid;

    oBuilder.StartTableRow;

    oBuilder.StartTableCell;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('1,1');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;

    oBuilder.StartTableCell;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('1,2');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;

    oBuilder.EndTableRow;

    oRow := oBuilder.StartTableRow;
    oRow.Background := clRed;

    oBuilder.StartTableCell;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('2,1');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;

    oBuilder.StartTableCell;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('2,2');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;

    oBuilder.EndTableRow;

    oRow1 := oBuilder.StartTableRow(oRow);
    oRow1.Background := clRed;

    oBuilder.StartTableCell;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('3,1');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;

    oBuilder.StartTableCell;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('3,2');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;

    oBuilder.EndTableRow;

    oBuilder.StartTableRow(oRow1);

    oBuilder.StartTableCell;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('4,1');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;

    oBuilder.StartTableCell;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('4,2');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;

    oBuilder.EndTableRow;

    oBuilder.StartTableRow(oRow);

    oBuilder.StartTableCell;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('5,1');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;

    oBuilder.StartTableCell;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('5,2');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;

    oBuilder.EndTableRow;

    oBuilder.EndTable;
    oBuilder.Stop;
    IsLoading := True;
    try
      wrdEditor.DocumentHandler.LoadDocument(oBuilder.Document);
    finally
      IsLoading := false;
    end;
  Finally
    oBuilder.free;
  End;
End;

procedure TWordProcessorForm.EditCDAHeader(Sender: TObject);
begin
  CdaHeaderDialog := TCdaHeaderDialog.Create(Self);
  try
    CdaHeaderDialog.CDADocument :=
      (wrdEditor.Document.SourceObjectModel as TCDADocument)
      .root.Clone as TcdaClinicalDocument;
    if CdaHeaderDialog.ShowModal = mrOK then
    begin
      (wrdEditor.Document.SourceObjectModel as TCDADocument).root :=
        CdaHeaderDialog.CDADocument.Link;
      ContentChange(Self);
    end;
  finally
    CdaHeaderDialog.free;
  end;
end;

Procedure TWordProcessorForm.Error1Click(Sender: TObject);
Begin
  wrdEditor.ShowFieldError(NS_FIELD_INPUT, 'test', 'Test', 'Test Message');

End;

Procedure TWordProcessorForm.ShowPopup(Const aScreenBox: Windows.TRect);
Var
  Menu: TPopupMenu;
  item: TMenuItem;
  point: TPoint;
Begin
  Menu := TPopupMenu.Create(Self);
  item := TMenuItem.Create(Self);
  item.Caption := 'Test1';
  Menu.Items.Add(item);
  item := TMenuItem.Create(Self);
  item.Caption := 'Test2';
  Menu.Items.Add(item);
  item := TMenuItem.Create(Self);
  item.Caption := 'Test3';
  Menu.Items.Add(item);
  point.x := aScreenBox.Left;
  point.y := aScreenBox.Bottom;
  point := ClientToScreen(point);
  Menu.popup(point.x, point.y);
End;

Procedure TWordProcessorForm.HotSpotHover(oSender: TWordProcessor;
  bActive: boolean; oInfo: TWPHotspotInformation);
Begin
  If Not bActive Then
    sbpHotspot.Caption := ''
  Else If oInfo.HotSpot <> Nil Then
    sbpHotspot.Caption := '  ' + 'Hotspot: ' + oInfo.HotSpot.URL
  Else
    sbpHotspot.Caption := '  ' + 'Annotation';
End;

Procedure TWordProcessorForm.HotSpot(oSender: TWordProcessor;
  oInfo: TWPHotspotInformation);
Var
  oHakBalloon: TUixBalloon;
  oHakCodeEdit: TUixCodeEdit;
  sURL: String;
  oImage: TWPWorkingDocumentImagePiece;
  oArea: TWPImageMapArea;
Begin
  sURL := oInfo.HotSpot.URL;
  If sURL = '.clear' Then
    wrdEditor.Clear
  Else If sURL = 'popup' Then
    ShowPopup(oInfo.Box)
  Else If FileExists(sURL) Then
    Open(sURL, wpfUnknown)
  Else If wrdEditor.HasImageArea(sURL, oImage, oArea) Then
    wrdEditor.SelectImageArea(oImage, oArea)
  Else
  Begin
    oHakBalloon := TUixBalloon.CreateNew(Self);
    oHakBalloon.Title := 'link was clicked';
    oHakBalloon.AnchorPositionTopLeft;
    oHakBalloon.RenderStyleCustom;
    oHakBalloon.BalloonTypeInformation;
    oHakBalloon.Control := wrdEditor;
    oHakBalloon.ControlPositionStyleAbsolute;
    oHakBalloon.ControlAbsoluteX := oInfo.Box.Left +
      ((oInfo.Box.Right - oInfo.Box.Left) Div 2);
    oHakBalloon.ControlAbsoluteY := oInfo.Box.Bottom;
    oHakBalloon.RenderStyleMessage;
    // oHakBalloon.ControlRelativeHorizontalPositionCenter;
    // oHakBalloon.ControlRelativeVerticalPositionBottom;

    oHakCodeEdit := TUixCodeEdit.Create(oHakBalloon.ClientPanel);
    oHakCodeEdit.AlignClient;

    oHakBalloon.ClientPanel.Height := 25;
    oHakBalloon.ClientPanel.Width := 130;

    oHakBalloon.Message := sURL;
    oHakBalloon.ShowBalloon;
  End;
End;

Procedure TWordProcessorForm.NoTableLines1Click(Sender: TObject);
Begin
  wrdEditor.Settings.TableBorders := Not wrdEditor.Settings.TableBorders;
  NoTableLines1.Checked := wrdEditor.Settings.TableBorders;
End;

Procedure TWordProcessorForm.Yield1Click(Sender: TObject);
Begin
  wrdEditor.Yield;
End;

Procedure TWordProcessorForm.LowLight1Click(Sender: TObject);
Begin
  Inherited;
  wrdEditor.Settings.LowLight := Not wrdEditor.Settings.LowLight;
  LowLight1.Checked := wrdEditor.Settings.LowLight;

End;

Procedure TWordProcessorForm.Sample3Click(Sender: TObject);
Var
  oBuilder: TWPDocumentBuilder;
  // oField : TWPDocumentField;
Begin
  oBuilder := TWPDocumentBuilder.Create(TWPDocument.Create);
  Try
    oBuilder.Start;
    oBuilder.StartParagraph;
    /// oBuilder.AddTextPlain('before ');
    // oField := oBuilder.StartField;
    /// oField.HasHotspot := True;
    // oField.Hotspot.URL := 'http://www.test.org';
    /// oField.Hotspot.LinkColour := clNavy;
    // oField.Hotspot.HoverColour := clFuchsia;
    oBuilder.AddTextPlain
      ('field1234567890qwertyuioplkjghgfdsazxcvbvbnnmmlkjkhgahasgq6dhdagcvandaljsfghsfywwgwreudsfyadstr51234567890sdf')
    { .Font.Foreground := clMaroon };
    // obuilder.EndField;
    // oBuilder.AddTextPlain(' after');
    oBuilder.EndParagraph;
    oBuilder.Stop;
    IsLoading := True;
    try
      wrdEditor.DocumentHandler.LoadDocument(oBuilder.Document);
    finally
      IsLoading := false;
    end;
  Finally
    oBuilder.free;
  End;
  // wrdEditor.Settings.ModeBrowser;
End;

Procedure TWordProcessorForm.Sample4Click(Sender: TObject);
Var
  oBuilder: TWPDocumentBuilder;
  oField: TWPDocumentField;
Begin
  oBuilder := TWPDocumentBuilder.Create(TWPDocument.Create);
  Try
    oBuilder.Start;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('before ');
    oField := oBuilder.StartField;
    oField.HasHotspot := True;
    oField.HotSpot.URL := 'http://www.test.org';
    oField.HotSpot.LinkColour := clOlive;
    oField.HotSpot.HoverColour := clYellow;
    oBuilder.AddTextPlain
      ('fieldsfdkjfhsdkfjhskdfhsldfjhsldjfhlsadjfhsaljdfhsaljdhfljs')
    { .Font.Foreground := clMaroon };
    oBuilder.EndField;
    oBuilder.AddTextPlain(' after');
    oBuilder.EndParagraph;
    oBuilder.Stop;
    IsLoading := True;
    try
      wrdEditor.DocumentHandler.LoadDocument(oBuilder.Document);
    finally
      IsLoading := false;
    end;
  Finally
    oBuilder.free;
  End;
End;

Procedure TWordProcessorForm.Sample5Click(Sender: TObject);
Var
  oBuilder: TWPDocumentBuilder;
  oTable: TWPDocumentTable;
  oImage: TFslBitmapGraphic;
Begin
  oImage := TFslBitmapGraphic.Create;
  Try
    oImage.LoadFromFile
      ('C:\Documents and Settings\Grahame Grieve\My Documents\Test Docs\test_brush.bmp');
    oBuilder := TWPDocumentBuilder.Create(TWPDocument.Create);
    Try
      oBuilder.Start;
      oTable := oBuilder.StartTable;
      oTable.BorderPolicy := BorderPolicyCustom;
      oBuilder.StartTableRow;

      With oBuilder.StartTableCell Do
      Begin
        TopBorder.SimpleFancy;
        TopBorder.BrushImage := oImage.Link;
        TopBorder.OuterColour := clNavy;
        TopBorder.OuterColour2 := clFuchsia;
        LeftBorder.SimpleFancy;
        LeftBorder.BrushImage := oImage.Link;
        LeftBorder.OuterColour := clWhite;
        Background := clYellow;
        TopBorder.LowOuterlimit := 16;
        TopBorder.HighOuterlimit := 36;
      End;
      oBuilder.StartParagraph;
      oBuilder.AddTextPlain('1,1');
      oBuilder.EndParagraph;
      oBuilder.EndTableCell;

      With oBuilder.StartTableCell Do
      Begin
        TopBorder.SimpleFancy;
        TopBorder.BrushImage := oImage.Link;
        TopBorder.OuterColour := clNavy;
        TopBorder.OuterColour2 := clFuchsia;
        RightBorder.SimpleFancy;
        RightBorder.BrushImage := oImage.Link;
        RightBorder.OuterColour := clWhite;
        Background := clYellow;
        TopBorder.LowOuterlimit := 16;
        TopBorder.HighOuterlimit := 36;
      End;
      oBuilder.StartParagraph;
      oBuilder.AddTextPlain('1,2');
      oBuilder.EndParagraph;
      oBuilder.EndTableCell;

      oBuilder.EndTableRow;

      oBuilder.StartTableRow.Background := clRed;

      With oBuilder.StartTableCell Do
      Begin
        BottomBorder.SimpleFancy;
        BottomBorder.BrushImage := oImage.Link;
        BottomBorder.OuterColour := clGreen;
        LeftBorder.SimpleFancy;
        LeftBorder.BrushImage := oImage.Link;
        LeftBorder.OuterColour := clWhite;
        Background := clYellow;
      End;
      oBuilder.StartParagraph;
      oBuilder.AddTextPlain('2,1');
      oBuilder.EndParagraph;
      oBuilder.EndTableCell;

      With oBuilder.StartTableCell Do
      Begin
        BottomBorder.SimpleFancy;
        BottomBorder.BrushImage := oImage.Link;
        BottomBorder.OuterColour := clGreen;
        RightBorder.SimpleFancy;
        RightBorder.BrushImage := oImage.Link;
        RightBorder.OuterColour := clWhite;
        Background := clYellow;
      End;
      oBuilder.StartParagraph;
      oBuilder.AddTextPlain('2,2');
      oBuilder.EndParagraph;
      oBuilder.EndTableCell;

      oBuilder.EndTableRow;

      oBuilder.EndTable;
      oBuilder.Stop;
      IsLoading := True;
      try
        wrdEditor.DocumentHandler.LoadDocument(oBuilder.Document);
      finally
        IsLoading := false;
      end;
    Finally
      oBuilder.free;
    End;
  Finally
    oImage.free;
  End;
End;

Procedure TWordProcessorForm.Sample6Click(Sender: TObject);
Var
  oBuilder: TWPDocumentBuilder;
  oImage: TWPDocumentImage;
  oArea: TWPImageMapArea;
Begin
  oBuilder := TWPDocumentBuilder.Create(TWPDocument.Create);
  Try
    oBuilder.Start;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('spacer');
    oBuilder.EndParagraph;
    oBuilder.StartParagraph;

    oImage := oBuilder.AddImage(DocRoot + 'Test Docs\unhighlighted.png',
      DocRoot + 'Test Docs\highlighted.png');

    // oImage.HasHotspot := True;
    // oImage.Hotspot.URL := 'link:image';
    // oImage.ImageHeight := 333;
    // oImage.ImageWidth := 470;

    oImage.HasMap := True;
    oArea := oImage.Map.Areas.New;
    Try
      oArea.URL := 'image:map:0';
      oArea.HoverColour := clSilver;
      // oArea.LinkColour := clGray;
      oArea.Coordinates.Add(219, 15);
      oArea.Coordinates.Add(219, 57);
      oArea.Coordinates.Add(316, 57);
      oArea.Coordinates.Add(316, 15);
      oArea.Selected := True;
      oImage.Map.Areas.Add(oArea.Link);
    Finally
      oArea.free;
    End;
    oArea := oImage.Map.Areas.New;
    Try
      oArea.URL := 'image:map:1';
      oArea.HoverColour := clSilver;
      // oArea.LinkColour := clGray;
      oArea.Coordinates.Add(319, 15);
      oArea.Coordinates.Add(319, 57);
      oArea.Coordinates.Add(396, 57);
      oArea.Coordinates.Add(396, 15);
      oImage.Map.Areas.Add(oArea.Link);
    Finally
      oArea.free;
    End;

    oBuilder.EndParagraph;
    oBuilder.Stop;
    IsLoading := True;
    try
      wrdEditor.DocumentHandler.LoadDocument(oBuilder.Document);
    finally
      IsLoading := false;
    end;
  Finally
    oBuilder.free;
  End;
  // wrdEditor.ReadOnly := True;
  wrdEditor.Settings.ModeBrowser;
End;

Const
  HL7_MESSAGE_CONTENT = '\H\CT BRAIN AND CT FACIAL BONES CLINICAL HISTORY:\N\' +
    cReturn + cReturn + '\.in2\Fall, \Zfs20\? LOC.  On Aspirin.' + cReturn +
    cReturn + 'TECHNIQUE:\Zpac\ Non contrast axial scans have been performed from the skull base '
    + 'to the vertex.  Fine cuts have been performed through the facial bones with coronal and sagittal '
    + 'reformations. \.br\REPORT:  No acute intracerebral haemorrhage or cerebral contusion seen. There are no '
    + 'extra-axial collections.  The ventricles and subarachnoid spaces are prominent in keeping with the '
    + 'patient''s age. \.ce\No subarachnoid haemorrhage seen.  There is moderate periventricular low density '
    + 'change consistent with small vessel ischaemia.   \Zi+\No acute infarct seen.\Zi+-\   No skull base or vault '
    + 'fracture detected.  \ZfcNavy\The mastoid \Zfs+2\air\Zfs\ cells and paranasal sinuses are clear\Zfc\.  No facial bone '
    + 'fractures seen.  \Zfc#118822\Deviation\Zfc\ of the \ZfbPink\nasal septum\Zfb\ to the left side is normal.  Allowing for metallic '
    + 'artefact from dental fillings, no mandibular fracture is seen.  The TMJs appear within '
    + 'normal limits.' + cReturn + cReturn +
    'COMMENT:  No \Zb+\acute\Zb-\ intracranial pathology identified.  No fracture seen.'
    + cReturn + cReturn +
    '\Zu+\CT ABDOMEN AND PELVIS WITH CONTRAST CLINICAL HISTORY\Zu-\:' + cReturn
    + cReturn +
    '\ZfnCourier New\\Zpl3\MRI shows sepsis.  Abdominal\Zfn\ pain and distension.  ? cause.'
    + cReturn + cReturn +
    'TECHNIQUE:   Axial scans were performed through the abdomen and pelvis following '
    + 'oral and intravenous contrast.  Delayed scans were performed following insertion of '
    + 'a urinary catheter. \.sp0\FINDINGS:\Zpi\\Zptdisc\There is a large fluid filled mass in the central '
    + 'lower abdomen measuring 24 cm AP x 20 cm trans x 25 cm cranio caudal.\Zpi\\Zptdisc\This contains some internal '
    + 'septations and there is a small amount of soft tissue adjacent to the left posterolateral aspect '
    + 'of this mass.\Zpi\\Zptalpha\\Zpndot\There is a rounded calcific area closely related to this mass on the left side '
    + 'measuring 3\Zfosuper\cm\Zfo\.\Zfosuper\ \Zpi\\Zpr6\This mass is closely related to the urinary bladder, causing compression of the '
    + 'bladder. There is no direct contact with the bladder, as confirmed by the scan following urinary '
    + 'catheterisation.  This mass most likely arises from an ovary, ? cyst adenoma, ? cyst adenocarcinoma. '
    + 'Further assessment by ultrasound is recommended.  The uterus appears separate from this mass '
    + 'and is anteverted and is of normal size. \.sk3\  There is a trace of free fluid within the abdomen, located '
    + 'to the right side of this cystic mass.  No free fluid is seen in the pouch of Douglas. There is '
    + 'effacement of the bowel but no evidence of obstruction.  No  collections are seen.  The liver, '
    + 'gall bladder, spleen, adrenals and pancreas are within normal limits. Both kidney are atrophied, '
    + 'with some perinephric stranding, consistent with a history of end stage renal failure.  There '
    + 'is no  lymphadenopathy. There is bibasal collapse/consolidation at the lung bases with small '
    + 'bilateral pleural effusions.  There is degenerative change within the lumbosacral spine.  There '
    + 'is irregularity of the inferior end plate of the L4 vertebral body with associated '
    + 'anterolisthesis of L4 on L5.  While this may relate to degenerative change, given the history '
    + 'of sepsis discitis/osteomyelitis should also be considered.  If there is concern then a '
    + 'bone scan is recommended.';

procedure TWordProcessorForm.HelpAboutClick(Sender: TObject);
begin
  Abort;
end;

procedure TWordProcessorForm.HelpCheckUpgradeClick(Sender: TObject);
begin
  Abort;
end;

procedure TWordProcessorForm.HelpIndexClick(Sender: TObject);
begin
  Abort;
end;

procedure TWordProcessorForm.HelpSendSupportCaseClick(Sender: TObject);
begin
  Abort;
end;

procedure TWordProcessorForm.HelpWebSiteClick(Sender: TObject);
begin
  Abort;
end;

Procedure TWordProcessorForm.HL71Click(Sender: TObject);
Var
  oData: TFslStringStream;
Begin
  oData := TFslStringStream.Create;
  Try
{$IFDEF VER130}
    oData.Data := StringReplace(HL7_MESSAGE_CONTENT, '\', #1);
{$ELSE}
    oData.Bytes := TEncoding.UTF8.GetBytes(StringReplace(HL7_MESSAGE_CONTENT,
      '\', #1));
{$ENDIF}
    IsLoading := True;
    try
      wrdEditor.DocumentHandler.LoadHL7(oData);
    finally
      IsLoading := false;
    end;
  Finally
    oData.free;
  End;
End;

Const
  S_MSG_START1 =
    'MSH|^~\&|Karisma|Qscan|HealthLink|newmktrd|20081105180753+1000||ORU^R01^ORU_R01|1149019|P|2.3.1|||AL|NE|AUS|ASCII|ENG'
    + #13 + 'PID|1||10165^^^Karisma^PI~42711489821^^^Karisma^MedicareNo|10165^^^Karisma^PI|Bonell^Jacqueline^Ann^^Ms^^L||19470216+1000|F|||4/11 Franklin Street^^KELVIN GROVE^QLD^4059^Australia||^^^^^^33523613|^^^^^^38584555|||||4271 14898 2-1|||||||||||N'
    + #13 + 'PV1||O|WI^^^WI^^^^^Qscan Windsor|||||2155989F^Chu^Cedric^^^Dr^^AUSHICPR|2155989F^Chu^Cedric^^^Dr^^AUSHICPR|||||||||||P'
    + #13;
  S_MSG_START2 =
    'ORC|RE||2008W0014276-1||CM|||||GRAA^AMANDA GRACE||2155989F^Chu^Cedric^^^Dr^^AUSHICPR~CHUC^Chu^Cedric^^^Dr^^LOC001|WI||20080710140000+1000||QSCAN||GRAA^AMANDA GRACE'
    + #13 + 'OBR|1||2008W0014276-1|BMD1^Bone Densitometry (prolonged steroid treat 12mths)^L||20080708+1000|200807101400+1000|200807101430+1000||||||||2155989F^Chu^'
    + #13;
  S_MSG_START3 =
    'Cedric^^^Dr^^AUSHICPR~CHUC^Chu^Cedric^^^Dr^^LOC001||WI|BM|DR=2155989F||20081104210708+1000||RAD|F||^^^^^R|||||WONJ2&Wong&Joseph|||WONJ&JOSEPH WONG'
    + #13 + 'OBX|1|FT|REPORT^^LN|1|';

  s_MSG_END =
    '||||||F|||20081104210708+1000||203885FF^Wong^Joseph^^^Dr^^AUSHICPR~WONJ2^Wong^Joseph^^^Dr^^LOC001'
    + #13;

Procedure TWordProcessorForm.MD1Click(Sender: TObject);
Var
  // oFolder : TUixBrowseDialog;
  oLines: TFslStringList;
  oBuffer: TFslBuffer;
  iCount: integer;
  sMsg: String;
Begin
  // oFolder := TUixBrowseDialog.Create(nil);
  // Try
  // oFolder.Folder := 'C:\Program Files\Health Communication Network\Messages\In';
  // if oFolder.Execute Then
  // Begin
  oLines := TFslStringList.Create;
  Try
    oLines.AsText := StringReplace(wrdEditor.DocumentHandler.AsHL7, #1, '\');
    oLines.SaveToText('c:\temp\wp.hl7');
    oBuffer := TFslBuffer.Create;
    Try
      sMsg := '';
      For iCount := 0 To oLines.Count - 1 Do
        sMsg := sMsg + StringReplace(oLines[iCount], '|', '\F\') + '\.br\';
{$IFDEF VER130}
      oBuffer.AsText := S_MSG_START1 + S_MSG_START2 + S_MSG_START3 + sMsg +
        s_MSG_END;
{$ELSE}
      oBuffer.AsBytes := TEncoding.UTF8.GetBytes(S_MSG_START1 + S_MSG_START2 +
        S_MSG_START3 + sMsg + s_MSG_END);
{$ENDIF}
      oBuffer.SaveToFileName
        (IncludeTrailingBackslash
        ('C:\Program Files\Health Communication Network\Messages\In') +
        'hl7_wp_' + FormatDateTime('yyyymmddhhnnss', now) + '.hl7');
    Finally
      oBuffer.free;
    End;
  Finally
    oLines.free;
  End;
  // End;
  // Finally
  // oFolder.Free;
  // End;
End;

Procedure TWordProcessorForm.Sample8Click(Sender: TObject);
Const
  GapColour = $00B78E8D;
  Function ImageMapURL(s: String): String;
  Begin
    result := s;
  End;

Var
  oDocumentBuilder: TWPDocumentBuilder;
  oTable: TWPDocumentTable;
  // oRow : TWPDocumentTableRow;
  oCell: TWPDocumentTableCell;
  oImage: TWPDocumentImage;
  oImageMapArea: TWPImageMapArea;
  oBorderBrushImage: TFslBitmapGraphic;

Begin
  oBorderBrushImage := TFslBitmapGraphic.Create;
  oBorderBrushImage.LoadFromFile
    ('C:\Documents and Settings\Grahame Grieve\My Documents\Test Docs\HomePageBorderBrush.bmp');

  oDocumentBuilder := TWPDocumentBuilder.Create;
  Try
    oDocumentBuilder.Document := TWPDocument.Create;

    oDocumentBuilder.Start;

    oDocumentBuilder.StartParagraph;

    oImage := oDocumentBuilder.AddImage
      ('C:\Documents and Settings\Grahame Grieve\My Documents\Test Docs\TopMenuUnselected.png',
      'C:\Documents and Settings\Grahame Grieve\My Documents\Test Docs\TopMenuSelected.png');

    oImage.VerticalAlignment := ImageVerticalAlignmentBottom;
    oImage.HasMap := True;

    oImageMapArea := oImage.Map.Areas.New;
    Try
      oImageMapArea.URL := ImageMapURL('patients');
      oImageMapArea.HoverColour := clSilver;
      oImageMapArea.Coordinates.Add(219, 1);
      oImageMapArea.Coordinates.Add(219, 57);
      oImageMapArea.Coordinates.Add(316, 57);
      oImageMapArea.Coordinates.Add(316, 1);
      oImage.Map.Areas.Add(oImageMapArea.Link);

    Finally
      oImageMapArea.free;
    End;

    oImageMapArea := oImage.Map.Areas.New;
    Try
      oImageMapArea.URL := ImageMapURL('requests');
      oImageMapArea.HoverColour := clSilver;
      oImageMapArea.Coordinates.Add(317, 1);
      oImageMapArea.Coordinates.Add(317, 57);
      oImageMapArea.Coordinates.Add(396, 57);
      oImageMapArea.Coordinates.Add(396, 1);
      oImage.Map.Areas.Add(oImageMapArea.Link);

    Finally
      oImageMapArea.free;
    End;

    oImageMapArea := oImage.Map.Areas.New;
    Try
      oImageMapArea.URL := ImageMapURL('services');
      oImageMapArea.HoverColour := clSilver;
      oImageMapArea.Coordinates.Add(397, 1);
      oImageMapArea.Coordinates.Add(397, 57);
      oImageMapArea.Coordinates.Add(476, 57);
      oImageMapArea.Coordinates.Add(476, 1);
      oImage.Map.Areas.Add(oImageMapArea.Link);

    Finally
      oImageMapArea.free;
    End;

    oImageMapArea := oImage.Map.Areas.New;
    Try
      oImageMapArea.URL := ImageMapURL('reports');
      oImageMapArea.HoverColour := clSilver;
      oImageMapArea.Coordinates.Add(477, 1);
      oImageMapArea.Coordinates.Add(477, 57);
      oImageMapArea.Coordinates.Add(555, 57);
      oImageMapArea.Coordinates.Add(555, 1);
      oImage.Map.Areas.Add(oImageMapArea.Link);

    Finally
      oImageMapArea.free;
    End;

    oImageMapArea := oImage.Map.Areas.New;
    Try
      oImageMapArea.URL := ImageMapURL('accounts');
      oImageMapArea.HoverColour := clSilver;
      oImageMapArea.Coordinates.Add(556, 1);
      oImageMapArea.Coordinates.Add(556, 57);
      oImageMapArea.Coordinates.Add(652, 57);
      oImageMapArea.Coordinates.Add(652, 1);
      oImage.Map.Areas.Add(oImageMapArea.Link);

    Finally
      oImageMapArea.free;
    End;

    oDocumentBuilder.EndParagraph;

    oTable := oDocumentBuilder.StartTable;

    oTable.BorderPolicy := BorderPolicyCustom;

    // Heading row
    oDocumentBuilder.StartTableRow;

    oCell := oDocumentBuilder.StartTableCell;
    oCell.Span := 3;
    oCell.Background := clWhite;
    oCell.LeftBorder.SimpleFancy;
    oCell.LeftBorder.BrushImage := oBorderBrushImage.Link;
    // oCell.LeftBorder.OuterColour := GapColour;//clWhite;
    // oCell.LeftBorder.LowOuterlimit := 5;
    // oCell.LeftBorder.HighOuterlimit := 5;
    oCell.TopBorder.BrushImage := oBorderBrushImage.Link;
    oCell.TopBorder.SimpleFancy;
    oCell.TopBorder.OuterColour := GapColour;
    // oCell.TopBorder.OuterColour2 := GapColour;
    // oCell.TopBorder.LowOuterlimit := 5;
    // oCell.TopBorder.HighOuterlimit := 5;
    oCell.RightBorder.SimpleFancy;
    oCell.RightBorder.BrushImage := oBorderBrushImage.Link;
    // oCell.RightBorder.OuterColour := GapColour;//clWhite;
    // oCell.RightBorder.LowOuterlimit := 16;
    // oCell.RightBorder.HighOuterlimit := 50;

    oDocumentBuilder.StartParagraph;
    oDocumentBuilder.AddTextPlain('Patient');
    oDocumentBuilder.EndParagraph;

    oDocumentBuilder.EndTableCell;
    oDocumentBuilder.EndTableRow;

    // Content row
    oDocumentBuilder.StartTableRow;

    // TO DO : determine the module to drawn the contents of based on FSelectedModuleName

    oCell := oDocumentBuilder.StartTableCell;
    oCell.Width := 0.35;
    oCell.Background := clWhite;
    oCell.LeftBorder.SimpleFancy;
    oCell.LeftBorder.BrushImage := oBorderBrushImage.Link;
    oCell.BottomBorder.SimpleFancy;
    oCell.BottomBorder.BrushImage := oBorderBrushImage.Link;
    oCell.BottomBorder.OuterColour := GapColour;
    oCell.BottomBorder.OuterColour2 := clWhite;
    oCell.BottomBorder.LowOuterlimit := 87;
    // oCell.BottomBorder.OuterColour2 := GapColour;
    // oCell.BottomBorder.LowOuterlimit := 0;
    // oCell.BottomBorder.HighOuterlimit := 0;

    oDocumentBuilder.StartParagraph;
    oDocumentBuilder.AddTextPlain('test');
    oDocumentBuilder.EndParagraph;
    oDocumentBuilder.EndTableCell;

    oCell := oDocumentBuilder.StartTableCell;
    oCell.Width := 0.35;
    oCell.Background := clWhite;
    oCell.BottomBorder.BrushImage := oBorderBrushImage.Link;
    oCell.BottomBorder.SimpleFancy;
    oCell.BottomBorder.OuterColour := GapColour;
    oCell.BottomBorder.OuterColour2 := clFuchsia;
    oCell.BottomBorder.LowOuterlimit := 16;

    oDocumentBuilder.StartParagraph;
    oDocumentBuilder.AddTextPlain('test');
    oDocumentBuilder.EndParagraph;
    oDocumentBuilder.EndTableCell;

    oCell := oDocumentBuilder.StartTableCell;
    oCell.Width := 0.3;
    oCell.Background := clWhite;
    oCell.BottomBorder.SimpleFancy;
    oCell.BottomBorder.BrushImage := oBorderBrushImage.Link;
    oCell.BottomBorder.OuterColour := GapColour;
    oCell.BottomBorder.OuterColour2 := clWhite;
    oCell.BottomBorder.HighOuterlimit := 20;
    // oCell.BottomBorder.HighOuterlimit := 36;
    oCell.RightBorder.SimpleFancy;
    oCell.RightBorder.BrushImage := oBorderBrushImage.Link;
    // oCell.RightBorder.OuterColour := GapColour;

    oDocumentBuilder.StartParagraph;
    oDocumentBuilder.AddTextPlain('test1');
    oDocumentBuilder.EndParagraph;
    oDocumentBuilder.StartParagraph;
    oDocumentBuilder.AddTextPlain('test2');
    oDocumentBuilder.EndParagraph;
    oDocumentBuilder.StartParagraph;
    oDocumentBuilder.AddTextPlain('test3');
    oDocumentBuilder.EndParagraph;
    oDocumentBuilder.StartParagraph;
    oDocumentBuilder.AddTextPlain('test4');
    oDocumentBuilder.EndParagraph;
    oDocumentBuilder.StartParagraph;
    oDocumentBuilder.AddTextPlain('test5');
    oDocumentBuilder.EndParagraph;
    oDocumentBuilder.StartParagraph;
    oDocumentBuilder.AddTextPlain('test6');
    oDocumentBuilder.EndParagraph;
    oDocumentBuilder.StartParagraph;
    oDocumentBuilder.AddTextPlain('test7');
    oDocumentBuilder.EndParagraph;
    oDocumentBuilder.StartParagraph;
    oDocumentBuilder.AddTextPlain('test8');
    oDocumentBuilder.EndParagraph;
    oDocumentBuilder.StartParagraph;
    oDocumentBuilder.AddTextPlain('test9');
    oDocumentBuilder.EndParagraph;
    oDocumentBuilder.StartParagraph;
    oDocumentBuilder.AddTextPlain('testA');
    oDocumentBuilder.EndParagraph;
    oDocumentBuilder.StartParagraph;
    oDocumentBuilder.AddTextPlain('testB');
    oDocumentBuilder.EndParagraph;
    oDocumentBuilder.StartParagraph;
    oDocumentBuilder.AddTextPlain('testC');
    oDocumentBuilder.EndParagraph;
    oDocumentBuilder.EndTableCell;

    oDocumentBuilder.EndTableRow;
    oDocumentBuilder.EndTable;

    // Bottom menu
    oDocumentBuilder.StartParagraph;

    oImage := oDocumentBuilder.AddImage
      ('C:\Documents and Settings\Grahame Grieve\My Documents\Test Docs\BottomMenuUnselected.png',
      'C:\Documents and Settings\Grahame Grieve\My Documents\Test Docs\BottomMenuSelected.png');

    oImage.VerticalAlignment := ImageVerticalAlignmentTop;
    oImage.HasMap := True;

    oImageMapArea := oImage.Map.Areas.New;
    Try
      oImageMapArea.URL := ImageMapURL('filmbags');
      oImageMapArea.HoverColour := clSilver;
      oImageMapArea.Coordinates.Add(219, 6);
      oImageMapArea.Coordinates.Add(219, 61);
      oImageMapArea.Coordinates.Add(315, 61);
      oImageMapArea.Coordinates.Add(315, 6);
      oImage.Map.Areas.Add(oImageMapArea.Link);

    Finally
      oImageMapArea.free;
    End;

    // oImageMapArea := oImage.Map.Areas.New;
    // Try
    // oImageMapArea.URL := ImageMapURL('unused1');
    // oImageMapArea.HoverColour := clSilver;
    // oImageMapArea.Coordinates.Add(316, 6);
    // oImageMapArea.Coordinates.Add(316, 61);
    // oImageMapArea.Coordinates.Add(395, 61);
    // oImageMapArea.Coordinates.Add(395, 6);
    // oImage.Map.Areas.Add(oImageMapArea.Link);
    //
    // FNamedImageMapAreaMatch.Add('unused1', oImageMapArea.Link);
    // Finally
    // oImageMapArea.Free;
    // End;
    //
    // oImageMapArea := oImage.Map.Areas.New;
    // Try
    // oImageMapArea.URL := ImageMapURL('unused2');
    // oImageMapArea.HoverColour := clSilver;
    // oImageMapArea.Coordinates.Add(397, 6);
    // oImageMapArea.Coordinates.Add(397, 61);
    // oImageMapArea.Coordinates.Add(476, 61);
    // oImageMapArea.Coordinates.Add(476, 6);
    // oImage.Map.Areas.Add(oImageMapArea.Link);
    //
    // FNamedImageMapAreaMatch.Add('unused2', oImageMapArea.Link);
    // Finally
    // oImageMapArea.Free;
    // End;
    //
    // oImageMapArea := oImage.Map.Areas.New;
    // Try
    // oImageMapArea.URL := ImageMapURL('unused3');
    // oImageMapArea.HoverColour := clSilver;
    // oImageMapArea.Coordinates.Add(476, 6);
    // oImageMapArea.Coordinates.Add(476, 61);
    // oImageMapArea.Coordinates.Add(555, 61);
    // oImageMapArea.Coordinates.Add(555, 6);
    // oImage.Map.Areas.Add(oImageMapArea.Link);
    //
    // FNamedImageMapAreaMatch.Add('unused3', oImageMapArea.Link);
    // Finally
    // oImageMapArea.Free;
    // End;

    oImageMapArea := oImage.Map.Areas.New;
    Try
      oImageMapArea.URL := ImageMapURL('management');
      oImageMapArea.HoverColour := clSilver;
      oImageMapArea.Coordinates.Add(557, 6);
      oImageMapArea.Coordinates.Add(557, 61);
      oImageMapArea.Coordinates.Add(652, 61);
      oImageMapArea.Coordinates.Add(652, 6);
      oImage.Map.Areas.Add(oImageMapArea.Link);

    Finally
      oImageMapArea.free;
    End;

    oDocumentBuilder.EndParagraph;

    oDocumentBuilder.Stop;
    IsLoading := True;
    try
      wrdEditor.DocumentHandler.LoadDocument(oDocumentBuilder.Document);
    finally
      IsLoading := false;
    end;
  Finally
    oDocumentBuilder.free;
  End;
  wrdEditor.Settings.ModeBrowser;
  oBorderBrushImage.free;
End;

Procedure TWordProcessorForm.Sample7Click(Sender: TObject);
Var
  oBuilder: TWPDocumentBuilder;
  oCell: TWPDocumentTableCell;
  oImage: TWPDocumentImage;
  oImageMapArea: TWPImageMapArea;
Begin
  oBuilder := TWPDocumentBuilder.Create(TWPDocument.Create);
  Try
    oBuilder.Start;
    oBuilder.StartTable;
    // Top menu row
    oBuilder.StartTableRow;
    oCell := oBuilder.StartTableCell;
    oCell.Span := 3;
    oBuilder.StartParagraph;
    oImage := oBuilder.AddImage
      ('C:\Documents and Settings\Grahame Grieve\My Documents\Test Docs\TopMenuUnselected.png',
      'C:\Documents and Settings\Grahame Grieve\My Documents\Test Docs\TopMenuSelected.png');
    oImage.HasMap := True;

    // TO DO : register each area against a name, which can be used to locate and set the selected area when there is one

    oImageMapArea := oImage.Map.Areas.New;
    Try
      oImageMapArea.URL := 'image:map:patients';
      oImageMapArea.HoverColour := clSilver;
      oImageMapArea.LinkColour := clGray;
      oImageMapArea.Coordinates.Add(219, 00);
      oImageMapArea.Coordinates.Add(219, 57);
      oImageMapArea.Coordinates.Add(316, 57);
      oImageMapArea.Coordinates.Add(316, 0);
      oImageMapArea.Selected := True;
      oImage.Map.Areas.Add(oImageMapArea.Link);
    Finally
      oImageMapArea.free;
    End;

    oImageMapArea := oImage.Map.Areas.New;
    Try
      oImageMapArea.URL := 'image:map:requests';
      oImageMapArea.HoverColour := clSilver;
      oImageMapArea.LinkColour := clGray;
      oImageMapArea.Coordinates.Add(319, 0);
      oImageMapArea.Coordinates.Add(319, 57);
      oImageMapArea.Coordinates.Add(396, 57);
      oImageMapArea.Coordinates.Add(396, 0);
      oImage.Map.Areas.Add(oImageMapArea.Link);
    Finally
      oImageMapArea.free;
    End;

    oBuilder.EndParagraph;
    oBuilder.EndTableCell;
    oBuilder.EndTableRow;

    // Content row
    oBuilder.StartTableRow;

    oBuilder.StartTableCell;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;

    oBuilder.StartTableCell;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;

    oBuilder.StartTableCell;
    oBuilder.StartParagraph;
    oBuilder.AddTextPlain('');
    oBuilder.EndParagraph;
    oBuilder.EndTableCell;

    oBuilder.EndTableRow;

    oBuilder.EndTable;

    oBuilder.Stop;
    IsLoading := True;
    try
      wrdEditor.DocumentHandler.LoadDocument(oBuilder.Document);
    finally
      IsLoading := false;
    end;
  Finally
    oBuilder.free;
  End;
  wrdEditor.Settings.ModeBrowser;
End;

Procedure TWordProcessorForm.mnuFindClick(Sender: TObject);
Begin
  if edtSource.Focused then
  begin
    abort;
  end
  else
    wrdEditor.SearchDialog;
End;

Procedure TWordProcessorForm.mnuReplaceClick(Sender: TObject);
Begin
  if edtSource.Focused then
  begin
    abort;
  end
  else
    wrdEditor.ReplaceDialog;
End;

Procedure TWordProcessorForm.mnuFormatClick(Sender: TObject);
Begin
  Inherited;

  mnuChangeCase.Enabled := (canFormat In wrdEditor.Capabilities) And
    wrdEditor.PrimaryRange.Selection.HasSelection;
End;

Procedure TWordProcessorForm.mnuParagraphClick(Sender: TObject);
Begin
  Inherited;
  wrdEditor.ParaDialog;
End;

Procedure TWordProcessorForm.mnuStyleClick(Sender: TObject);
Begin
  Inherited;
  wrdEditor.StyleDialog;
End;

Function TWordProcessorForm.getDefaultPrinter: TFslPrinter;
Var
  oManager: TFslPrinterManager;
Begin
  oManager := TFslPrinterManager.Create;
  Try
    oManager.Open;
    If oManager.HasDefaultDefinition Then
    Begin
      result := TFslPrinter.Create;
      result.Definition := oManager.DefaultDefinition.Link;
    End
    Else
      result := Nil;
  Finally
    oManager.free;
  End;
End;

Function TWordProcessorForm.BuildPageLayoutController: TWPPageLayoutController;
//Var
//  oController: TKestralPublishWordProcessorPageController;
Begin
//  oController := TKestralPublishWordProcessorPageController.Create;
//  Try
//    oController.Load(FReportScribe.Design);
//    result := oController.Link;
//  Finally
//    oController.free;
//  End;
  result := nil;
End;

Procedure TWordProcessorForm.clpSelect(Sender: TObject);
Begin
  If clpList.ItemIndex = -1 Then
    clpSource.Clear
  Else
  Begin
    Clipboard.Open;
    clpSource.Text := Clipboard.PasteContent
      (TWPClipboardContentType(clpList.Items.Objects[clpList.ItemIndex]));
    Clipboard.Close;
  End;
End;

Procedure TWordProcessorForm.clpLoad(Sender: TObject);
Var
  aType: TWPClipboardContentType;
  sSel: String;
Begin
  If clpList.ItemIndex <> -1 Then
    sSel := clpList.Items[clpList.ItemIndex];
  Clipboard.Open;
  clpList.Items.Clear;
  For aType := Low(TWPClipboardContentType) To High(TWPClipboardContentType) Do
    If Clipboard.HasContentType(aType) And (aType <> wcctUnknown) Then
      clpList.Items.AddObject(WPCLIPBOARDCONTENTTYPE_NAMES[aType],
        TObject(aType));
  Clipboard.Close;
  If clpList.Items.Count = 0 Then
    clpSource.Clear
  Else If (sSel <> '') Then
    clpList.ItemIndex := clpList.Items.IndexOf(sSel);
End;

Procedure TWordProcessorForm.mnuChangeCaseClick(Sender: TObject);
Begin
  Inherited;
  wrdEditor.ChangeCaseDialog;
End;

Procedure TWordProcessorForm.LoadTests;
Begin
End;

Function TWordProcessorForm.DocRoot: String;
Begin
  result := ShellDocumentFolder;
End;

Procedure TWordProcessorForm.ReplaceFieldClick(Sender: TObject);
Var
  oDocument: TWPDocument;
  oBuilder: TWPDocumentBuilder;
Begin
  oDocument := TWPDocument.Create;
  Try
    wrdEditor.DocumentHandler.SaveDocument(oDocument);
    oBuilder := TWPDocumentBuilder.Create;
    Try
      oBuilder.Document := oDocument.Link;
      oBuilder.Home;
      oBuilder.FirstField;
      While oBuilder.More And (oBuilder.CurrentField.Name <> 'test') Do
        oBuilder.NextField;
      If oBuilder.More Then
      Begin
        oBuilder.Start;
        oBuilder.ClearChildren;
        oBuilder.AddTextStyledByContext('inserted text');
        oBuilder.CloneField(', ');
        oBuilder.AddTextStyledByContext('inserted text2');
        oBuilder.Stop;
      End;
      oBuilder.NextField;
      Assert(Not oBuilder.More);
    Finally
      oBuilder.free;
    End;
    IsLoading := True;
    try
      wrdEditor.DocumentHandler.LoadDocument(oDocument);
    finally
      IsLoading := false;
    end;
  Finally
    oDocument.free;
  End;
End;

procedure TWordProcessorForm.DoShow(Sender : TObject);
begin
  pnlDocument.AlignLeft;
  spltView.AlignLeft;
  spltView.ShuffleRight;
  pnlSource.AlignClient;
end;

Procedure TWordProcessorForm.LiveFieldProcessClick(Sender: TObject);
var
  range: TWPVisualRange;
begin
  range := wrdEditor.ProduceRange;
  try
    if not range.SetFieldContents(NS_FIELD_INPUT, 'test', 'New Value',
      wrdEditor.Settings.AnnotationDefinitions[0]) then
      ShowMessage('input field "test" not found');
  finally
    wrdEditor.ConsumeRange(range);
  end;
end;

Procedure TWordProcessorForm.DocFieldProcessClick(Sender: TObject);
Var
  oDocument: TWPDocument;
  oBuilder: TWPDocumentBuilder;
Begin
  oDocument := TWPDocument.Create;
  Try
    wrdEditor.DocumentHandler.SaveDocument(oDocument);
    oBuilder := TWPDocumentBuilder.Create;
    Try
      oBuilder.Document := oDocument.Link;
      oBuilder.FieldValueProviders.Add(TWPTestFieldValuer.Create());
      oBuilder.ProcessFields;
    Finally
      oBuilder.free;
    End;
    IsLoading := True;
    try
      wrdEditor.DocumentHandler.LoadDocument(oDocument);
    finally
      IsLoading := false;
    end;
  Finally
    oDocument.free;
  End;
End;

Procedure TWordProcessorForm.CopyAllClick(Sender: TObject);
Begin
  wrdEditor.CopyAllToClipboard;
End;

Function TWordProcessorForm.NamedTemplate(oSender: TObject; Const iId: integer;
  Const sName: String; Var aFormat: TWPFormat): TFslBuffer;
Begin
  aFormat := wpfHTML;
  result := TFslBuffer.Create;
  Try
    result.AsText := '<html><body><p>This <b>is</b> a test</p></body></html>';
    result.Link;
  Finally
    result.free;
  End;
End;


// Function TWPTestFieldDefiner.NewField(oField : TWPDocumentField):Boolean;
// Begin
// oField.Namespace := NS_FIELD_MANUAL;
// oField.Name1 := 'test';
// oField.Font.Bold := tsTrue;
// oField.HasHotspot := True;
// oField.Hotspot.URL := 'test';
// oField.Contents.Add(TWPDocumentText.Create('test'));
// Result := True;
// End;
//
// Function TWPTestFieldDefiner.UserCanEditFieldProperties(oField : TWPDocumentField) : Boolean;
// Begin
// Result := True;
// End;
//
//
// Function TWPTestFieldDefiner.UserCanEditFieldSectionProperties(oSection : TWPDocumentSection) : Boolean;
// Begin
// Result := True;
// End;

{ TWPTestFieldDefiner }

Function TWPTestFieldDefiner.CanInsertField: boolean;
Begin
  result := True;
End;

Function TWPTestFieldDefiner.GetNamespace: String;
Begin
  result := NS_TEST;
End;

Function TWPTestFieldDefiner.GetTitle: String;
Begin
  result := 'Test';
End;

Function TWPTestFieldDefiner.NewField(oField: TWPDocumentField;
  bSection: boolean; var bAcceptExistingContent: boolean;
  Var oSection: TWPDocumentSection): boolean;
Begin
  bAcceptExistingContent := false;
  oField.Name := 'test';
  oField.Contents.Add(TWPDocumentText.Create('test'));
  oField.FixedFormat := fffFixed;
  result := True;
End;

{ TWPTestFieldValuer }

Function TWPTestFieldValuer.GetAction(oField: TWPDocumentField): TFieldAction;
Begin
  If oField.Name = 'test' Then
    result := FieldActionReplaceAll
  Else
    result := FieldActionDelete;
End;

Function TWPTestFieldValuer.GetNamespace: String;
Begin
  result := NS_TEST;
End;

Procedure TWPTestFieldValuer.Replace(oField: TWPDocumentField;
  oBuilder: TWPDocumentBuilder);
Var
  oPiece: TWPDocumentText;
Begin
  oPiece := oBuilder.AddTextStyledByContext('tested');
  oPiece.Style := oField.Style;
  oPiece.Font.Update(oField.Font);
End;

Procedure TWordProcessorForm.SpeechMagicPopulateClick(Sender: TObject);
//Var
//  oConv: TWPSPTemplateLoader;
//  oBuffer: TFslBuffer;
Begin
//  oBuffer := TFslBuffer.Create;
//  Try
//    wrdEditor.DocumentHandler.SaveNative(oBuffer, false);
//
//    oConv := TWPSPTemplateLoader.Create();
//    Try
//      oConv.PlainText := false;
//      oConv.UIHandle := Self;
//      oConv.SourceBuffer := oBuffer.Link;
//      oConv.Load;
//      If wrdEditor Is TWPSpeechMagic Then
//        TWPSpeechMagic(wrdEditor).DebugLoadRtf(oConv.FinalFilename);
//    Finally
//      oConv.free;
//    End;
//
//  Finally
//    oBuffer.free;
//  End;
End;

Procedure TWordProcessorForm.HL72Click(Sender: TObject);
Var
  oHL7: TWPHL7FTWriter;
  oStream: TFslFile;
Begin
  oStream := TFslFile.Create('C:\temp\hl7.txt', fmCreate);
  Try
    oHL7 := TWPHL7FTWriter.Create;
    Try
      oHL7.Stream := oStream.Link;
      oHL7.Styles := wrdEditor.WorkingStyles.Link;
      oHL7.UseHighlighting := True;
      oHL7.AllOneField := false;
      oHL7.UseFormattedTextEncoding := false;
      oHL7.Width := 82;
      oHL7.DoubleParagraphs := True;
      oHL7.Write(wrdEditor.Document);
    Finally
      oHL7.free;
    End;
  Finally
    oStream.free;
  End;
  ExecuteOpen('C:\temp\hl7.txt');
End;

Procedure TWordProcessorForm.mnuKarismaConnectClick(Sender: TObject);
{ var
  oWizard : TKestralStorageProviderWizard;
  oFile : TFslXMLFile;
  oProvider : TKestralStorageProvider;
}
Begin
  { if FKarisma = nil Then
    Begin
    oWizard := TKestralStorageProviderWizard.Create(self);
    Try
    if FileExists('wp-karisma-provider.xml') Then
    Begin
    oProvider := nil;
    oFile := TFslXMLFile.Create;
    Try
    oFile.Name := 'wp-karisma-provider.xml';
    oFile.OpenRead;
    oFile.Reader['Provider'].DefineObject(oProvider);
    Try
    if oProvider <> Nil Then
    //              oWizard.Provider.AssignProvider(oProvider);
    Finally
    oProvider.Free;
    End;
    Finally
    oFile.Free;
    End;
    End;
    if oWizard.Execute Then
    Begin
    oProvider := oWizard.Provider;
    oFile := TFslXMLFile.Create;
    Try
    oFile.Name := 'wp-karisma-provider.xml';
    oFile.OpenCreate;
    oFile.Writer['Provider'].DefineObject(oProvider);
    Finally
    oFile.Free;
    End;
    ConnectToKarisma(oProvider);
    End;
    Finally
    oWizard.Free;
    End;
    End
    Else
    Begin
    FLastBtn.Free;
    FNextBtn.Free;
    FPrevBtn.Free;
    FFirstBtn.Free;
    FKarisma.Close;
    FKarisma.Free;
    FKarisma := nil;
    mnuKarismaConnect.Caption := '&Connect';
    End;
  }
  raise EWPException.create('commented out');
End;

{
  procedure TWordProcessorForm.ConnectToKarisma(oProvider: TKestralStorageProvider);
  begin
  //  FKarisma := TODBCStorageManager.CreateDirect(oProvider.Username, oProvider.Password, oProvider.Server, oProvider.Database, oProvider.FailoverPartner);
  //  FKarisma.Open;
  //  mnuKarismaConnect.Caption := '&Disconnect';
  (*
  FLastBtn := wrdToolbar.AddButton('Last', LastBtnClicked, WPIconModule.LAST);
  FNextBtn := wrdToolbar.AddButton('Next', NextBtnClicked, WPIconModule.NEXT);
  FPrevBtn := wrdToolbar.AddButton('Prev', PrevBtnClicked, WPIconModule.PREV);
  FFirstBtn := wrdToolbar.AddButton('First', FirstBtnClicked, WPIconModule.FIRST);
  *)
  end;
}
(*
  procedure TWordProcessorForm.FirstBtnClicked(Sender: TObject);
  var
  oConn : TKestralStorageConnection;
  oSql : TKestralStorageStatement;
  begin
  oConn := FKarisma.ProduceConnection;
  Try
  oSql := oConn.ProduceStatement;
  Try
  oSql.SQL := 'Select [Key], Blob from ReportInstanceValues where [Current] = 1 and DataType = 2';
  oSql.Prepare;
  oSql.Execute;
  if oSql.FetchNext Then
  LoadWPFromKarisma(oSQL)
  else
  wrdEditor.Clear;
  oSql.Terminate;
  Finally
  oConn.ConsumeStatement(oSql);
  End;
  Finally
  FKarisma.ConsumeConnection(oConn);
  End;
  end;

  procedure TWordProcessorForm.LastBtnClicked(Sender: TObject);
  var
  oConn : TKestralStorageConnection;
  oSql : TKestralStorageStatement;
  begin
  oConn := FKarisma.ProduceConnection;
  Try
  oSql := oConn.ProduceStatement;
  Try
  oSql.SQL := 'Select [Key], Blob from ReportInstanceValues where [Current] = 1 and DataType = 2 Order By [Key] Desc';
  oSql.Prepare;
  oSql.Execute;
  if oSql.FetchNext Then
  LoadWPFromKarisma(oSQL)
  else
  wrdEditor.Clear;
  oSql.Terminate;
  Finally
  oConn.ConsumeStatement(oSql);
  End;
  Finally
  FKarisma.ConsumeConnection(oConn);
  End;
  end;


  procedure TWordProcessorForm.NextBtnClicked(Sender: TObject);
  var
  oConn : TKestralStorageConnection;
  oSql : TKestralStorageStatement;
  begin
  oConn := FKarisma.ProduceConnection;
  Try
  oSql := oConn.ProduceStatement;
  Try
  oSql.SQL := 'Select [Key], Blob from ReportInstanceValues where [Current] = 1 and DataType = 2 and [Key] > '+IntegerToString(FKarismaKey)+' Order By [Key] Asc';
  oSql.Prepare;
  oSql.Execute;
  if oSql.FetchNext Then
  LoadWPFromKarisma(oSQL)
  Else
  LastBtnClicked(self);
  oSql.Terminate;
  Finally
  oConn.ConsumeStatement(oSql);
  End;
  Finally
  FKarisma.ConsumeConnection(oConn);
  End;
  end;

  procedure TWordProcessorForm.PrevBtnClicked(Sender: TObject);
  var
  oConn : TKestralStorageConnection;
  oSql : TKestralStorageStatement;
  begin
  oConn := FKarisma.ProduceConnection;
  Try
  oSql := oConn.ProduceStatement;
  Try
  oSql.SQL := 'Select [Key], Blob from ReportInstanceValues where [Current] = 1 and DataType = 2 and [Key] < '+IntegerToString(FKarismaKey)+' Order By [Key] Desc';
  oSql.Prepare;
  oSql.Execute;
  if oSql.FetchNext Then
  LoadWPFromKarisma(oSQL)
  Else
  FirstBtnClicked(self);
  oSql.Terminate;
  Finally
  oConn.ConsumeStatement(oSql);
  End;
  Finally
  FKarisma.ConsumeConnection(oConn);
  End;
  end;
*)
(*
  procedure TWordProcessorForm.LoadWPFromKarisma(oSQL: TKestralStorageStatement);
  Var
  oStream : TFslMemoryStream;
  begin
  FKarismaKey := oSQl.ColumnByName['Key'].AsInteger;
  oStream := TFslMemoryStream.Create;
  Try
  oStream.Buffer := oSQl.ColumnByName['Blob'].AsBlob.Link;
  wrdEditor.DocumentHandler.LoadNative(oStream);
  Finally
  oStream.Free;
  End;
  end;
*)

Procedure TWordProcessorForm.prnSelect(Sender: TObject);
Var
  oPrinter: TFslPrinter;
  oPreview: TFslPrinterPreviewJob;
Begin
  oPrinter := TFslPrinter.Create;
  Try
    oPrinter.Definition := TFslPrinterDefinition.Create;
    oPrinter.Definition.Name := prnList.Items[prnList.ItemIndex];
    oPrinter.Open;

    oPreview := TFslPrinterPreviewJob.Create;
    Try
      oPreview.OpenSettings(oPrinter.Settings);
      oPreview.Start;
      oPreview.NewPage;

      prnDetails.Text := DescribeCanvas(oPreview.Canvas, oPrinter);
      oPreview.Close;
    Finally
      oPreview.free;
    End;

    oPrinter.Close;
  Finally
    oPrinter.free;
  End;
End;

Function CapsToString(c, a: integer): String;
Begin
  If c And a > 0 Then
    result := 'True'
  Else
    result := 'False';
End;

Function TWordProcessorForm.DescribeCanvas(oCanvas: TFslPrinterCanvas;
  oPrinter: TFslPrinter): String;
Var
  rc: integer;
Begin
  If (pcColor In oPrinter.CapabilitySet) Then
    result := 'This is a colour printer' + #13#10
  Else
    result := 'This is not a colour printer' + #13#10;

  result := result + #13#10 + 'Y PixelsPerInch : ' +
    inttostr(GetDeviceCaps(oCanvas.DCHandle, LOGPIXELSY)) + #13#10 +
    'X PixelsPerInch : ' + inttostr(GetDeviceCaps(oCanvas.DCHandle, LOGPIXELSX))
    + #13#10 + 'X PixelsPerGraphicMetre : ' +
    inttostr(GetDeviceCaps(oCanvas.DCHandle, LOGPIXELSX)) + #13#10 +
    'X Resolution : ' + inttostr(GetDeviceCaps(oCanvas.DCHandle, HORZRES)) +
    #13#10 + 'Y Resolution : ' + inttostr(GetDeviceCaps(oCanvas.DCHandle,
    VERTRES)) + #13#10 + 'X OffsetPixels : ' +
    inttostr(GetDeviceCaps(oCanvas.DCHandle, PhysicalOffsetX)) + #13#10 +
    'Y OffsetPixels : ' + inttostr(GetDeviceCaps(oCanvas.DCHandle,
    PhysicalOffsetY)) + #13#10 + 'Colour Resolution : ' +
    inttostr(GetDeviceCaps(oCanvas.DCHandle, COLORRES)) + #13#10 + 'Planes : ' +
    inttostr(GetDeviceCaps(oCanvas.DCHandle, PLANES)) + #13#10 +
    'Bits / Pixel : ' + inttostr(GetDeviceCaps(oCanvas.DCHandle, BITSPIXEL)) +
    #13#10 + 'Number of Colours : ' +
    inttostr(GetDeviceCaps(oCanvas.DCHandle, NUMCOLORS)) + #13#10 +
    'Supports Colour : ' + BooleanToString(pcColor In oPrinter.CapabilitySet) +
    #13#10 + 'Use Colour : ' + BooleanToString(oPrinter.Settings.UseColour) +
    #13#10 + 'Raster Capabilities : ' + #13#10;
  rc := GetDeviceCaps(oCanvas.DCHandle, RASTERCAPS);
  result := result + '  Requires banding support : ' +
    CapsToString(rc, RC_BANDING) + #13#10 +
    '  Capable of transferring bitmaps : ' + CapsToString(rc, RC_BITBLT) +
    #13#10 + '  Capable of supporting bitmaps larger than 64K : ' +
    CapsToString(rc, RC_BITMAP64) + #13#10 +
    '  Capable of supporting the SetDIBits and GetDIBits functions : ' +
    CapsToString(rc, RC_DI_BITMAP) + #13#10 +
    '  Capable of supporting the SetDIBitsToDevice function : ' +
    CapsToString(rc, RC_DIBTODEV) + #13#10 +
    '  Capable of performing flood fills : ' + CapsToString(rc, RC_FLOODFILL) +
    #13#10 + '  Capable of supporting features of Windows 2.0 : ' +
    CapsToString(rc, RC_GDI20_OUTPUT) + #13#10 +
    '  Specifies a palette-based device : ' + CapsToString(rc, RC_PALETTE) +
    #13#10 + '  Capable of scaling : ' + CapsToString(rc, RC_SCALING) + #13#10 +
    '  Capable of performing the StretchBlt function : ' +
    CapsToString(rc, RC_STRETCHBLT) + #13#10 +
    '  Capable of performing the StretchDIBits function : ' +
    CapsToString(rc, RC_STRETCHDIB) + #13#10;
End;

procedure TWordProcessorForm.DictEditorClick(Sender: TObject);
begin
  abort;
end;

Function FileToString(AFilename: String;
  AShareMode: Word = fmOpenRead + fmShareDenyWrite): String;
Var
  LFileStream: TFilestream;
Begin
  If FileExists(AFilename) Then
  Begin
    LFileStream := TFilestream.Create(AFilename, AShareMode);
    Try
      SetLength(result, LFileStream.Size);
      If LFileStream.Size > 0 Then
        LFileStream.Read(result[1], LFileStream.Size);
    Finally
      LFileStream.free;
    End;
  End
  Else
    raise EWPException.create('File "' + AFilename + '" not found');
End;

Procedure StringToFile(Const AStr, AFilename: String);
Var
  oFileStreamSimple: TFilestream;
Begin
  oFileStreamSimple := TFilestream.Create(AFilename, fmCreate);
  Try
    If Length(AStr) > 0 Then
      oFileStreamSimple.Write(AStr[1], Length(AStr));
  Finally
    oFileStreamSimple.free;
  End;
End;

Procedure TWordProcessorForm.FormsMode1Click(Sender: TObject);
Begin
  Inherited;
  wrdEditor.Settings.FormsMode := Not wrdEditor.Settings.FormsMode;
  If wrdEditor.Settings.FormsMode Then
    wrdEditor.Settings.FieldWrappers := wpfpHints
  Else
    wrdEditor.Settings.FieldWrappers := wpfpSquareBrackets;
  FormsMode1.Checked := wrdEditor.Settings.FormsMode;
End;

procedure TWordProcessorForm.SpellingClick(Sender: TObject);
begin
  wrdEditor.CheckSpelling;
end;

Procedure TWordProcessorForm.SpellingOptionClick(Sender: TObject);
Begin
  Inherited;
  wrdEditor.Settings.SpellingErrors := Not wrdEditor.Settings.SpellingErrors;
  SpellingOption.Checked := wrdEditor.Settings.SpellingErrors;
End;

procedure TWordProcessorForm.StyleEditorClick(Sender: TObject);
begin
  abort;
end;

procedure TWordProcessorForm.ToggleSourceView(Sender: TObject);
begin
  FSourceVisible := not FSourceVisible;
  ApplySourceViewSettings;
end;

procedure TWordProcessorForm.ApplySourceViewSettings;
begin
  if FSourceVisible then
  begin
    pnlViewSource.Width := FSourceWidth;
    edtSource.Visible := True;
    btnSource.Caption := '>';
    pnlSourceHeader.Caption := '      Source View';;
    spltView.Visible := True;
  end
  else
  begin
    pnlViewSource.Width := 30;
    edtSource.Visible := false;
    btnSource.Caption := '<';
    pnlSourceHeader.Caption := '';
    spltView.Visible := false;
  end;
end;

procedure TWordProcessorForm.ToolsOptionsClick(Sender: TObject);
begin
  CDEOptionsForm := TCDEOptionsForm.Create(Self);
  try
    CDEOptionsForm.WP := wrdEditor;
    CDEOptionsForm.ShowModal;
  finally
    CDEOptionsForm.free;
  end;
end;

procedure TWordProcessorForm.ChangeZoom(Sender: TObject);
begin
  FZooming := True;
  try
    wrdEditor.Settings.Scale := ScaleForZoomTrack(FZoomTrack.Position);
  finally
    FZooming := false;
  end;
end;

Procedure TWordProcessorForm.CheckFieldsClick(Sender: TObject);
Var
  oList: TFslStringList;
Begin
  oList := TFslStringList.Create;
  Try
    If Not wrdEditor.CheckFields(oList) Then
      ShowMessage('Failed: ' + oList.AsText);
  Finally
    oList.free;
  End;
End;

procedure TWordProcessorForm.SourceChange(Sender: TObject;
  const Info: TScintEditChangeInfo);
var
  src: TMemoryStream;
begin
  FIsDirty := True;
  src := TMemoryStream.Create;
  // src := TFileStream.Create('c:\temp\src.bin', fmCreate);
  try
    edtSource.Lines.SaveToStream(src);
    src.Position := 0;
    try
      sciCDA.doc := TCDADocument.Create(src);
    except
      sciCDA.doc := nil;
    end;
    edtSource.Refresh;
  finally
    src.free;
  end;
end;

procedure TWordProcessorForm.SourceViewResize(Sender: TObject);
begin
  if FSourceVisible then
    FSourceWidth := pnlViewSource.Width;
end;

Procedure TWordProcessorForm.SpawnWPEditor(Sender: TObject);
Var
  oEditor: TWordProcessor;
  oForm: TUixForm;
Begin
  oForm := TUixForm.Create(Self);
  oForm.Width := 1000;
  oForm.Height := 600;
  oForm.HasClientPanel := True;

  oEditor := TWordProcessor.Create(oForm);
  oEditor.parent := oForm.ClientPanel;
  oEditor.AlignClient;
  oEditor.ManualDock(oForm.ClientPanel);
  oEditor.Clear;

  oForm.Show;
End;

function TWordProcessorForm.UnderDevelopment: boolean;
begin
  result := FileExists('C:\HL7Connect\v2\development_readme.txt');
end;

procedure TWordProcessorForm.UpdateField(oSender: TObject;
  aState: TFieldUpdateStatus; oDefinitionProvider: TWPFieldDefinitionProvider;
  oField: TWPDocumentField; sContent: String; bValid: boolean);
begin
  // if bValid then
  // ShowMessage('Field '+oField.Namespace+'#'+oField.Name+': '+sContent);
end;

procedure TWordProcessorForm.mnuExportBitmapClick(Sender: TObject);
begin
  raise EWPException.create('not done yet');
end;

Procedure TWordProcessorForm.mnuEmailClick(Sender: TObject);
begin
  ShowMessage('not done yet');
end;


function TWordProcessorForm.DoTemplate(oSender: TObject): TFslBuffer;
var
  dlg: TOpenDialog;
begin
  result := nil;
  dlg := TOpenDialog.Create(Self);
  try
    dlg.InitialDir := 'C:\Users\Grahame\Documents\Test Docs\Templates';
    dlg.Filter := 'Templates|*.kdoc';
    if dlg.Execute then
    begin
      result := TFslBuffer.Create;
      result.LoadFromFileName(dlg.Filename);
    end;
  finally
    dlg.free;
  end;
end;

function TWordProcessorForm.BuildCDABanner(cda: TCDADocument): TWPDocument;
var
  build: TWPDocumentBuilder;
begin
  build := TWPDocumentBuilder.Create;
  try
    build.Document := TWPDocument.Create;
    build.Start;

    build.StartParagraph(); // document
    build.AddText('Document', True, false);
    build.AddTextPlain(': ');
    build.AddTextPlain(cda.Title);
    build.AddTextPlain(', date ');
    build.AddTextPlain(cda.root.effectiveTime.render);
    build.AddTextPlain(' (id = ');
    build.AddText(cda.root.id.render, false, True);
    build.AddTextPlain(')');
    build.EndParagraph;

    build.StartParagraph(); // patient
    build.AddText('Patient', True, false);
    build.AddTextPlain(': ');
    build.AddTextPlain(cda.patientName);
    build.AddTextPlain(', ');
    build.AddTextPlain(cda.patientGender);
    build.AddTextPlain(' DOB: ');
    build.AddTextPlain(cda.patientDOB);
    build.AddTextPlain('. Identifiers = ');
    build.AddText(cda.RenderPatientIdentifiers, false, True);
    build.EndParagraph;

    build.StartParagraph(); // author
    build.AddText('Author', True, false);
    build.AddTextPlain(': ');
    build.AddTextPlain(cda.authorName);
    build.AddTextPlain(', ');
    build.AddTextPlain(cda.authorTelecomSummary);
    build.AddTextPlain('. Identifiers = ');
    build.AddText(cda.RenderPatientIdentifiers, false, True);
    build.EndParagraph;

    build.Stop;
    result := build.Document.Link;
  finally
    build.free;
  end;
end;

{ TRecentFileList }

{ TRecentFileList }

function TRecentFileList.GetSaveText: string;
var
  i: integer;
begin
  result := '';
  for i := 0 to Count - 1 do
    result := result + ';' + Strings[i] + ',' + WPFORMAT_NAMES
      [TWPFormat(Objects[i])];
  result := Copy(result, 2, $FFFF);
end;

procedure TRecentFileList.SetSaveText(Value: string);
var
  s, l, r: String;
begin
  while (Value <> '') do
  begin
    StringSplit(Value, ';', s, Value);
    StringSplit(s, ',', l, r);
    AddObject(l, TObject(readFormatParam(r)));
  end;
end;

{ TISScintEdit }

procedure TISScintEdit.CreateWnd;
const
  PixmapHasEntry: array [0 .. 8] of PAnsiChar = ('5 5 2 1', 'o c #808080',
    '. c #c0c0c0', 'ooooo', 'o...o', 'o...o', 'o...o', 'ooooo', nil);
  PixmapEntryProcessed: array [0 .. 8] of PAnsiChar = ('5 5 2 1', 'o c #008000',
    '. c #00ff00', 'ooooo', 'o...o', 'o...o', 'o...o', 'ooooo', nil);
  PixmapBreakpoint: array [0 .. 14] of PAnsiChar = ('9 10 3 1', '= c none',
    'o c #000000', '. c #ff0000', '=========', '==ooooo==', '=o.....o=',
    'o.......o', 'o.......o', 'o.......o', 'o.......o', 'o.......o',
    '=o.....o=', '==ooooo==', nil);
  PixmapBreakpointGood: array [0 .. 15] of PAnsiChar = ('9 10 4 1', '= c none',
    'o c #000000', '. c #ff0000', '* c #00ff00', '======oo=', '==oooo**o',
    '=o....*o=', 'o....**.o', 'o....*..o', 'o...**..o', 'o**.*...o',
    'o.***...o', '=o.*...o=', '==ooooo==', nil);
  PixmapBreakpointBad: array [0 .. 15] of PAnsiChar = ('9 10 4 1', '= c none',
    'o c #000000', '. c #ff0000', '* c #ffff00', '=========', '==ooooo==',
    '=o.....o=', 'o.*...*.o', 'o.**.**.o', 'o..***..o', 'o.**.**.o',
    'o.*...*.o', '=o.....o=', '==ooooo==', nil);
const
  SC_MARK_BACKFORE = 3030; { new marker type added in my Scintilla build }
begin
  inherited;

  Call(SCI_SETCARETWIDTH, 2, 0);
  Call(SCI_AUTOCSETAUTOHIDE, 0, 0);
  Call(SCI_AUTOCSETCANCELATSTART, 0, 0);
  Call(SCI_AUTOCSETDROPRESTOFWORD, 1, 0);
  Call(SCI_AUTOCSETIGNORECASE, 1, 0);
  Call(SCI_AUTOCSETMAXHEIGHT, 7, 0);
  Call(SCI_SETSTYLEBITS, 8, 0);

  Call(SCI_ASSIGNCMDKEY, Ord('Z') or ((SCMOD_SHIFT or SCMOD_CTRL) shl 16),
    SCI_REDO);

  Call(SCI_SETSCROLLWIDTH, 1024 * CallStr(SCI_TEXTWIDTH, 0, 'X'), 0);

  Call(SCI_INDICSETSTYLE, inSquiggly, INDIC_SQUIGGLE);
  Call(SCI_INDICSETFORE, inSquiggly, clRed);
  Call(SCI_INDICSETSTYLE, inPendingSquiggly, INDIC_HIDDEN);

  Call(SCI_SETMARGINTYPEN, 1, SC_MARGIN_SYMBOL);
  Call(SCI_SETMARGINWIDTHN, 1, 21);
  Call(SCI_SETMARGINSENSITIVEN, 1, 1);
  Call(SCI_SETMARGINCURSORN, 1, SC_CURSORARROW);
  Call(SCI_SETMARGINTYPEN, 2, SC_MARGIN_BACK);
  Call(SCI_SETMARGINMASKN, 2, 0);
  Call(SCI_SETMARGINWIDTHN, 2, 1);
  Call(SCI_SETMARGINTYPEN, 3, SC_MARGIN_SYMBOL);
  Call(SCI_SETMARGINMASKN, 3, 0);
  Call(SCI_SETMARGINWIDTHN, 3, 1);
  Call(SCI_SETMARGINLEFT, 0, 2);

  Call(SCI_MARKERDEFINEPIXMAP, mmIconHasEntry, LPARAM(@PixmapHasEntry));
  Call(SCI_MARKERDEFINEPIXMAP, mmIconEntryProcessed,
    LPARAM(@PixmapEntryProcessed));
  Call(SCI_MARKERDEFINEPIXMAP, mmIconBreakpoint, LPARAM(@PixmapBreakpoint));
  Call(SCI_MARKERDEFINEPIXMAP, mmIconBreakpointGood,
    LPARAM(@PixmapBreakpointGood));
  Call(SCI_MARKERDEFINEPIXMAP, mmIconBreakpointBad,
    LPARAM(@PixmapBreakpointBad));
  Call(SCI_MARKERDEFINE, mmLineError, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineError, clWhite);
  Call(SCI_MARKERSETBACK, mmLineError, clMaroon);
  Call(SCI_MARKERDEFINE, mmLineBreakpoint, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineBreakpoint, clWhite);
  Call(SCI_MARKERSETBACK, mmLineBreakpoint, clRed);
  Call(SCI_MARKERDEFINE, mmLineBreakpointBad, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineBreakpointBad, clLime);
  Call(SCI_MARKERSETBACK, mmLineBreakpointBad, clOlive);
  Call(SCI_MARKERDEFINE, mmLineStep, SC_MARK_BACKFORE);
  Call(SCI_MARKERSETFORE, mmLineStep, clWhite);
  Call(SCI_MARKERSETBACK, mmLineStep, clBlue);
  Call(SCI_SETCARETLINEBACK, clYellow, 0);
  Call(SCI_SETCARETLINEVISIBLE, 1, 0);
  // Call(SCI_SETCARETLINEBACKALPHA, SC_ALPHA_TRANSPARENT, 0);
  Call(SCI_SETCARETLINEVISIBLEALWAYS, MAXINT, 0);

end;

(*
function TWordProcessorForm.FetchPublishingDocument : TKestralPublishDocumentBundle;
var
  oReportDocument: TKestralPublishDocument;
  oWPDocument: TWPDocument;
begin
  oReportDocument := TKestralPublishDocument.Create;
  Try
    oWPDocument := TWPDocument.Create;
    Try
      wrdEditor.DocumentHandler.SaveDocument(oWPDocument);
      FReportScribe.Origin.SpanSource.Atom.Value := oWPDocument.Link;
    Finally
      oWPDocument.free;
    End;
    FReportScribe.Write(oReportDocument);
    result := TKestralPublishDocumentBundle.Create;
    result.Documents.Add(oReportDocument.Link);
  Finally
    oReportDocument.free;
  End;
end;
*)


Procedure TWordProcessorForm.mnuPrintClick(Sender: TObject);
//Var
//  oDialog: TKestralPublishDocumentPrintDialog;
Begin
//  oDialog := TKestralPublishDocumentPrintDialog.Create(Self);
//  Try
//    oDialog.DocumentBundle := FetchPublishingDocument;
//    oDialog.Execute;
//  Finally
//    oDialog.free;
//  End;
  Abort;
End;

Procedure TWordProcessorForm.mnuPrintPreviewClick(Sender: TObject);
//Var
//  oDialog: TKestralPublishDocumentPrintPreviewDialog;
Begin
//  oDialog := TKestralPublishDocumentPrintPreviewDialog.Create(Self);
//  Try
//    oDialog.DocumentBundle := FetchPublishingDocument;
//    oDialog.Execute;
//  Finally
//    oDialog.free;
//  End;
  Abort;
End;


procedure TWordProcessorForm.mnuPDFClick(Sender: TObject);
Begin
  if sdPDF.Execute then
    saveToPDF(sdPDF.FileName);
end;

{ var
  begin
  oDialog := TFaxDialog.Create(Self);
  Try
  oDialog.DocumentBundle := TKestralPublishDocumentBundle.Create;
  oDialog.DocumentBundle.Documents.Add(oReportDocument.Link);

  oDialog.Execute;
  Finally
  oDialog.Free;
  End;
  Finally
  oReportDocument.Free;
  End;
  end;
  end;
}


function TWordProcessorForm.getPDFPrinter: TFslPrinter;
var
  mgr : TFslPrinterManager;
  list : TFslPrinterList;
begin
  mgr := TFslPrinterManager.Create;
  Try
    mgr.Open;
    list := TFslPrinterList.Create;
    Try
      mgr.CompilePrinterList(list);
      result := list.GetByName(PDF_PRINTER_NAME).Link;
    Finally
      list.Free;
    End;
  Finally
    mgr.Free;
  End;
end;

procedure TWordProcessorForm.saveToPDF(filename: String);
Var
  pdfPrinter : TFslPrinter;
//  oEngine : TKestralPublishDocumentPrintEngine;
  oJob : TFslPrinterJob;
begin
  pdfPrinter := getPDFPrinter;
  try
    pdfPrinter.open;
//    oEngine := TKestralPublishDocumentPrintEngine.Create;
//    Try
//      oEngine.DocumentBundle := FetchPublishingDocument;
//      oJob := pdfPrinter.ProduceJob;
//      Try
//        oJob.Title := ExtractFileNameNoExt(fileName);
//        oJob.DestinationFileName := fileName;
//        oEngine.Job := oJob.Link;
//        oEngine.Execute;
//      Finally
//        pdfPrinter.ConsumeJob(oJob);
//      End;
//    Finally
//      oEngine.Free;
//    End;
    Abort;
  finally
    pdfPrinter.Free;
  end;
end;

function FileCanBeRead(fn : String) : boolean;
var
  f : TFileStream;
begin
  try
    f := TFileStream.create(fn, fmOpenRead + fmShareDenyWrite);
    try
      result := true;
    finally
      f.free;
    end;
  except
    result := false;
  end;
end;

function TWordProcessorForm.printPDFToBuffer : TFslBuffer;
var
  fn : String;
  i : integer;
begin
  fn := IncludeTrailingBackslash(SystemTemp)+lowercase(copy(GUIDToString(CreateGUID), 2, 36));
  saveToPDF(fn);
  sleep(1000);
  i := 0;
  while (i < 15) and not FileExists(fn) and not FileCanBeRead(fn) do
  begin
    sleep(1000);
    inc(i);
  end;
  result := TFslBuffer.create;
  try
    result.LoadFromFileName(fn);
    result.link;
  finally
    result.free;
  end;
end;

procedure TWordProcessorForm.mnuFaxClick(Sender: TObject);
//var
//  pdf : TFslBuffer;
//  dlg : TWordProcessorFaxDialog;
//  gofax : TGoFaxGatewayController;
//  sId : String;
//  oJob : TFslFaxJob;
//  oEngine : TKestralPublishDocumentPrintEngine;
//  oFaxContextInfo : TWinFaxContextInfo;
//  oFaxPrinterInfo : TWinFaxPrintInfo;
//  iJobIdentifier : DWORD;
begin
//  sId := '';
//  dlg := TWordProcessorFaxDialog.create(nil);
//  try
//    if dlg.ShowModal = mrOk then
//    begin
//      if dlg.rbGoFax.checked then
//      begin
//        pdf := printPDFToBuffer;
//        try
//          gofax := TGoFaxGatewayController.Create;
//          try
//            gofax.open;
//            gofax.AccessToken := dlg.eToken.text;
//            if not gofax.Ready then
//              raise EWPException.create('Unable to send a fax');
//            gofax.Send(stripSpaces(dlg.eNumber.Text), stripSpaces(dlg.eSource.text), 'filename.pdf', pdf, sId);
//            gofax.close;
//            ShowMessage('send as '+sId);
//          finally
//            gofax.Free;
//          end;
//        finally
//          pdf.free;
//        end;
//      end
//      else
//      begin
//        oEngine := TKestralPublishDocumentPrintEngine.Create;
//        Try
//          oEngine.DocumentBundle := FetchPublishingDocument;
//          oJob := TFslFaxJob.Create;
//          Try
//            oEngine.Job := oJob.Link;
//
//            oJob.Title := ExtractFileNameNoExt(FDocumentName);
//            FillChar(oFaxContextInfo, SizeOf(TWinFaxContextInfo), #0);
//            FillChar(oFaxPrinterInfo, SizeOf(TWinFaxPrintInfo), #0);
//            oFaxContextInfo.SizeOfStruct := SizeOf(TWinFaxContextInfo);
//            oFaxPrinterInfo.SizeOfStruct := SizeOf(TWinFaxPrintInfo);
//            oFaxPrinterInfo.RecipientNumber := StringToPChar(stripSpaces(dlg.eNumber.Text));
//            oFaxPrinterInfo.DocName := StringToPChar(FDocumentName);
//            oFaxPrinterInfo.OutputFileName := StringToPChar('c:\temp\'+FDocumentName);
//            If Not FaxStartPrintJob(Nil, oFaxPrinterInfo, iJobIdentifier,
//              oFaxContextInfo) Then
//              Error('CreateJob', ErrorAsString);
//            oJob.DC := oFaxContextInfo.hDC;
//            oJob.Open;
//
//            oEngine.Execute;
//            oJob.Close;
//          Finally
//            oJob.Free;
//          End;
//        Finally
//          oEngine.Free;
//        End;
//      end;
//    end;
//  finally
//    dlg.Free;
//  end;
  Abort;
end;


End.

