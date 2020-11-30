Unit FHIR.WP.Dialogs;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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
Interface


Uses
  Windows, SysUtils, Graphics, Controls, Classes, Forms, StdCtrls, ExtCtrls, Spin, Math, ComCtrls, Dialogs, Contnrs,
  fsl_utilities, fsl_collections, wp_graphics,
  fui_vclx_Base, fui_vclx_Controls, fui_vclx_Forms, fui_vclx_Images, fui_vclx_Advanced, fui_vclx_Dialogs,

  wp_gdiplus, wp_clipboard,
  wp_types, wp_document, FHIR.WP.Settings, FHIR.WP.Icons, wp_working, FHIR.WP.Engine, FHIR.WP.Control, wp_format;


Type
  TWPFrame = Class (TUixForm)
    Protected
      Procedure Initialise; Override;

      Function AddLabel(oParent : TWinControl; iTop, iLeft : Integer; Const sCaption : String) : TUixLabel;

    Public
      Function DesiredHeight : Integer; Virtual;
      Function DesiredWidth : Integer; Virtual;
  End;

  TWPFrameClass = Class Of TWPFrame;

  TWPDialog = Class (TUixForm)
    Private
      FReadOnly : Boolean;
      FFrame : TWPFrame;

      Function GetFrame : TWPFrame;

      Procedure SetReadOnly(Const Value : Boolean);
    function GetHasFrame: Boolean;

    Protected
      Function FrameClass : TWPFrameClass; Overload; Virtual;
      Function DialogCaption : String; Overload; Virtual;

      Property Frame : TWPFrame Read GetFrame;

      Procedure Initialise; Override;

    Public
      Procedure Restore; Override;
      Procedure Refresh; Override;
      Procedure Accept; Override;

      Property HasFrame : Boolean Read GetHasFrame;

      Property ReadOnly : Boolean Read FReadOnly Write SetReadOnly;
  End;

  TWPHotspotFrame = Class(TWPFrame)
    Private
      FField : TWPDocumentField;

      FKeyEdit : TEdit;
      FURLEdit : TEdit;
      FURLUnderlineCheckBox : TCheckBox;
      FLinkColor : TUixHTMLColourComboBox;
      FHoverColor : TUixHTMLColourComboBox;

      Function GetField : TWPDocumentField;
      Procedure SetField(Const Value: TWPDocumentField);

      Procedure OnKeyEditChanged(oSender: TObject);

    Protected
      Procedure Initialise; Override;
      Procedure Finalise; Override;

    Public
      Function DesiredHeight : Integer; Override;
      Function DesiredWidth : Integer; Override;

      Procedure Accept; Override;

      Property Field : TWPDocumentField Read GetField Write SetField;
      Property URLEdit : TEdit Read FURLEdit;
  End;


Type
  TWPHotspotDialog = Class (TWPDialog)
    Private
      Function GetField: TWPDocumentField;
      Procedure SetField(Const Value: TWPDocumentField);
      Function GetFrame : TWPHotspotFrame;
    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;

      Function CanAccept : Boolean; Override;

    Public
      Procedure Restore; Override;
      Property Frame : TWPHotspotFrame Read GetFrame;
      Property Field : TWPDocumentField Read GetField Write SetField;
  End;


Const
  MAX_ITEM_DISPLAY = 16;
  MAX_WIDTH = 400;
  Min_WIDTH = 40;

Type

  TWPCodeCompletionDialog = Class (TUixForm)
    Private
      FList : TWPCompletionItems;
      FListBox : TListBox;

      Function GetList : TWPCompletionItems;
      Procedure SetList(Const Value: TWPCompletionItems);

      Procedure DoKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
      Procedure ListBoxDoubleClick(Sender: TObject);

    Protected
      Procedure Initialise; Override;
      Procedure Finalise; Override;
    Public
      Procedure SetCodePosition(Const aPoint : TPoint); Overload; Virtual;

      Function Selected : TWPCompletionItem; Overload; Virtual;

      Procedure Restore; Override;

      Property List : TWPCompletionItems Read GetList Write SetList;
  End;

Type
  TWPAdornmentFrame = Class(TWPFrame)
  Private
    FAdornment : TWPDocumentImageAdornment;
    FFontsAllowed: TFslStringList;
    bIsReady : Boolean;

    FgrpPen : TUixGroupBox;
    FspnPenWidth : TSpinEdit;
    FcbxColour : TUixHTMLColourComboBox;
    FcbxStyle : TUixCombobox;

    FgrpCaption : TUixGroupBox;
    FCaption : TUixEdit;
    FcbxFont : TUixCombobox;
    FcbxSize : TUixCombobox;
    FcbxFontColour : TUixHTMLColourComboBox;

    FgrpExample : TUixGroupBox;
    FExample : TUixImage;

    Procedure userAction(oSender : TObject);

    Procedure DrawExample;

    Procedure BuildPen;
    Procedure BuildCaption;
    Procedure BuildExample;
    Procedure BuildForm;

    Procedure DoUpdate;
    Procedure SetFontsAllowed(Const Value: TFslStringList);
    procedure LoadFontNames;

    Function GetAdornment : TWPDocumentImageAdornment;
    Procedure SetAdornment(Const Value : TWPDocumentImageAdornment);

  Protected

    Procedure Initialise; Override;
    Procedure Finalise; override;

  Public
    Function DesiredHeight : Integer; Override;
    Function DesiredWidth : Integer; Override;

    Procedure Restore; Override;
    Procedure Accept; Override;
    Function CanAccept : Boolean; Override;
    Procedure Resize; Override;

    Property FontsAllowed : TFslStringList Read FFontsAllowed Write SetFontsAllowed;
    Property Adornment : TWPDocumentImageAdornment Read GetAdornment Write SetAdornment;
  End;



Type
  TWPAdornmentDialog = Class (TWPDialog)
    Private
      Function GetAdornment : TWPDocumentImageAdornment;
      Procedure SetAdornment(Value : TWPDocumentImageAdornment);
      Function GetFrame : TWPAdornmentFrame;
      function GetFontsAllowed: TFslStringList;
      procedure SetFontsAllowed(const Value: TFslStringList);

    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;
    Public

      Property Frame : TWPAdornmentFrame read GetFrame;
      Property Adornment : TWPDocumentImageAdornment Read GetAdornment Write SetAdornment;
      Property FontsAllowed : TFslStringList read GetFontsAllowed Write SetFontsAllowed;
  End;


Type
  TWPFontFrame = Class(TWPFrame)
  Private
    FFont : TWPSFontDetails;

    FgrpFont : TGroupBox;
    FedtFont : TEdit;
    FlstFont : TListBox;
    FSpnSize : TSpinEdit;
    FlstSize : TListBox;

    FgrpOptions : TGroupBox;
    FchkBold : TCheckBox;
    FchkItalic : TCheckBox;
    FchkUnderline : TCheckbox;
    FchkStrikeout : TCheckBox;
    FcbState : TComboBox;
    FcbCaps : TComboBox;

    FgrpColour : TGroupBox;
    FcbxFore : TUixHTMLColourComboBox;
    FcbxBack : TUixHTMLColourComboBox;

    FgrpSample : TGroupBox;
    FpnlSample : TPanel;
    FpntSample : TPaintBox;

    FUpdating : Boolean;

    FFontSizeMinimum : Integer;
    FFontSizeMaximum : Integer;
    FFontsAllowed: TFslStringList;
    FCOnsoleMode: Boolean;

    Procedure LoadFontNames;
    Procedure ListSizes;
    Procedure BuildNameSize;
    Procedure BuildOptions;
    Procedure BuildColour;
    Procedure BuildSample;
    Procedure BuildForm;

    Procedure SetFont(Const Value: TWPSFontDetails);
    Procedure BindToFont;

    Procedure edtFontChange(oSender : TObject);
    Procedure lstFontClick(oSender : TObject);
    Procedure SpnSizeChange(oSender : TObject);
    Procedure lstSizeClick(oSender : TObject);

    Procedure chkBoldClick(oSender : TObject);
    Procedure chkItalicClick(oSender : TObject);
    Procedure chkUnderlineClick(oSender : TObject);
    Procedure chkStrikeoutClick(oSender : TObject);
{    Procedure chkSuperClick(oSender : TObject);
    Procedure chkSubClick(oSender : TObject);
    Procedure chkCAPSClick(oSender : TObject);
    Procedure chkNcapsClick(oSender : TObject);
}
    Procedure cbStateChange(oSender : TObject);
    Procedure cbCapChange(oSender : TObject);
    Procedure cbxForeChange(oSender : TObject);
    Procedure cbxBackChange(oSender : TObject);

    Procedure pntSamplePaint(oSender : TObject);
    Procedure RefreshSample;
    Procedure SetFontsAllowed(Const Value: TFslStringList);

  Protected
    Procedure Initialise; Override;
    Procedure Finalise; Override;

  Public
    Function DesiredHeight : Integer; Override;
    Function DesiredWidth : Integer; Override;

    Procedure Restore; Override;
    Procedure Okay; Override;

    Function HasFontSizeConstraint : Boolean;

    Property Font : TWPSFontDetails Read FFont Write SetFont;
    Property FontSizeMinimum : Integer Read FFontSizeMinimum Write FFontSizeMinimum;
    Property FontSizeMaximum : Integer Read FFontSizeMaximum Write FFontSizeMaximum;
    Property FontsAllowed : TFslStringList Read FFontsAllowed Write SetFontsAllowed;
    Property ConsoleMode : Boolean Read FCOnsoleMode Write FConsoleMode;
  End;


Const
  FONTSIZES : Array [1..39] Of Integer = (4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,24,26,28,30,32,34,36,38,40,44,48,52,56,60,64,72,80,96,128,172);


Type
  TWPFontDialog = Class (TWPDialog)
    Private
      Function GetFont: TWPSFontDetails;
      Procedure SetFont(Const Value: TWPSFontDetails);
      Function GetFrame : TWPFontFrame;
    Function GetFontsAllowed: TFslStringList;
    Procedure SetFontsAllowed(Const Value: TFslStringList);
    Function GetConsoleMode: Boolean;
    Procedure SetConsoleMode(Const Value: Boolean);

    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;
    Public

      Property Frame : TWPFontFrame Read GetFrame;
      Property Font : TWPSFontDetails Read GetFont Write SetFont;
      Property FontsAllowed : TFslStringList Read GetFontsAllowed Write SetFontsAllowed;
      Property ConsoleMode : Boolean Read GetConsoleMode Write SetConsoleMode;
  End;

Type
  TWPPasteSpecialFrame = Class(TWPFrame)
  Private
    lblDesc : TLabel;
    lbFormats : TListBox;

    Function GetFormat : TWPClipboardContentType;
    Procedure SetFormat(aValue : TWPClipboardContentType);
    Procedure BuildForm;
    Procedure LoadClipboardFormats;
    Procedure lbFormatsDblClick(oSender : TObject);
  Protected
    Procedure Initialise; Override;
    Procedure Finalise; Override;

  Public
    Function DesiredHeight : Integer; Override;
    Function DesiredWidth : Integer; Override;

    Procedure Restore; Override;
    Procedure Refresh; Override;

    Property Format : TWPClipboardContentType Read GetFormat Write SetFormat;
  End;

Type
  TWPPasteSpecialDialog = Class (TWPDialog)
    Private
      Function GetFormat: TWPClipboardContentType;
      Procedure SetFormat(Const Value: TWPClipboardContentType);
      Function GetFrame : TWPPasteSpecialFrame;
    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;
    Public

      Property Frame : TWPPasteSpecialFrame Read GetFrame;
      Property Format : TWPClipboardContentType Read GetFormat Write SetFormat;
  End;

Type
  TWPParagraphFrame = Class(TWPFrame)
    Private
      FParagraph : TWPSParagraphDetails;

      FgrpAlign : TUixGroupBox;
      FbtnLeft : TUixAdvancedButton;
      FbtnCenter : TUixAdvancedButton;
      FbtnRight : TUixAdvancedButton;
      FbtnJustify : TUixAdvancedButton;

      FgrpMargin : TUixGroupBox;
      FspnLeftIndent : TSpinEdit;
      FspnRightIndent : TSpinEdit;
      FspnBottomMargin : TSpinEdit;

      FgrpList : TUixGroupBox;
      FcbxList : TUixComboBox;
      FcbxBullet : TUixComboBox;
      FcbxNumber : TUixComboBox;
      FcbxNumberFormat : TUixComboBox;
      FchkNumber : TUixCheckBox;
      FspnNumber : TSpinEdit;

      Function GetParagraph : TWPSParagraphDetails;
      Procedure SetParagraph(Const Value: TWPSParagraphDetails);

      Procedure BindToParagraph;

      Procedure btnLeftChange(oSender : TObject);
      Procedure btnCenterChange(oSender : TObject);
      Procedure btnRightChange(oSender : TObject);
      Procedure btnJustifyChange(oSender : TObject);
      Procedure spnLeftIndentChange(oSender : TObject);
      Procedure spnRightIndentChange(oSender : TObject);
      Procedure spnBottomMarginChange(oSender : TObject);
      Procedure FcbxListChange(oSender : TObject);
      Procedure cbxBulletChange(oSender : TObject);
      Procedure cbxNumberChange(oSender : TObject);
      Procedure cbxNumberFormatChange(oSender : TObject);
      Procedure chkNumberChange(oSender : TObject);
      Procedure spnNumberChange(oSender : TObject);

    Protected
      Procedure Initialise; Override;
      Procedure Finalise; Override;

      Procedure UpdateAlignment;

    Public
      Function DesiredHeight : Integer; Override;
      Function DesiredWidth : Integer; Override;

      Procedure Restore; Override;
      Procedure Okay; Override;

      Property Paragraph : TWPSParagraphDetails Read GetParagraph Write SetParagraph;
  End;



Type
  TWPParagraphDialog = Class (TWPDialog)
    Private
      Function GetParagraph: TWPSParagraphDetails;
      Procedure SetParagraph(Const Value: TWPSParagraphDetails);

      Function GetFrame : TWPParagraphFrame;

    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;

      Function DialogCaption : String; Overload; Override;

    Public
      Property Frame : TWPParagraphFrame Read GetFrame;
      Property Paragraph : TWPSParagraphDetails Read GetParagraph Write SetParagraph;
  End;

  TWPStyleFrame = Class(TWPFrame)
    Private
      FStyle : TWPStyle;

      FTabs : TUixPageControl;
      FStyleTab : TUixTabsheet;
      FFontTab : TUixTabsheet;
      FFontFrame : TWPFontFrame;
      FParaTab : TUixTabsheet;
      FParaFrame : TWPParagraphFrame;

      FStyleName : TUixEdit;
      FStyleDesc : TUixEdit;
      FDescLabel : TUixLabel;

      FchkHasParagraph : TUixCheckBox;
      FchkEndsWithParagraph : TUixCheckBox;
      FHasDesc: Boolean;

      Procedure HasParagraphChanged(Sender : TObject);
      Procedure EndsWithParagraphChanged(Sender : TObject);
      Procedure StyleNameChanged(Sender : TObject);

      Procedure MakePara;
      Procedure ClearPara;

      Function GetStyle : TWPStyle;
      Procedure SetStyle(Const Value: TWPStyle);

      Procedure BindToStyle;
      Procedure SetHasDesc(Const Value: Boolean);
      Function GetDesc: String;
      Procedure SetDesc(Const Value: String);

    Protected
      Procedure Initialise; Override;
      Procedure Finalise; Override;

    Public
      Function DesiredHeight : Integer; Override;
      Function DesiredWidth : Integer; Override;

      Procedure Restore; Override;
      Procedure Accept; Override;

      Property Style : TWPStyle Read GetStyle Write SetStyle;
      Property HasDesc : Boolean Read FHasDesc Write SetHasDesc;
      Property Desc : String Read GetDesc Write SetDesc;
  End;



Type
  TWPStyleDialog = Class (TWPDialog)
    Private
      Function GetStyle: TWPStyle;
      Procedure SetStyle(Const Value: TWPStyle);

      Function GetFrame : TWPStyleFrame;

    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;

      Function DialogCaption : String; Overload; Override;

    Public
      Procedure Bind; Overload;
      Property Frame : TWPStyleFrame Read GetFrame;
      Property Style : TWPStyle Read GetStyle Write SetStyle;
  End;

  TWPChangeCaseFrame = Class(TWPFrame)
  Private
    FChangeCaseType: TWPChangeCaseType;

    FRadioSentenceCase:   TUixRadioButton;
    FRadioLowerCase:      TUixRadioButton;
    FRadioUpperCase:      TUixRadioButton;
    FRadioTitleCase:      TUixRadioButton;
    FRadioToggleCase:     TUixRadioButton;

    Procedure BuildForm;

    Procedure chkSentenceCaseClick(oSender : TObject);
    Procedure chkLowerCaseClick(oSender : TObject);
    Procedure chkUpperCaseClick(oSender : TObject);
    Procedure chkTitleCaseClick(oSender : TObject);
    Procedure chkToggleCaseClick(oSender : TObject);

  Protected
    Procedure Initialise; Override;
    //Procedure Finalise; Override;

  Public
    Function DesiredHeight : Integer; Override;
    Function DesiredWidth : Integer; Override;

    Property ChangeCaseType: TWPChangeCaseType Read FChangeCaseType;
  End;

Type
  TWPChangeCaseDialog = Class (TWPDialog)
    Private
      Function GetChangeCaseType: TWPChangeCaseType;
      Function GetFrame : TWPChangeCaseFrame;
    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;
    Public

      Property Frame : TWPChangeCaseFrame Read GetFrame;
      Property ChangeCaseType : TWPChangeCaseType Read GetChangeCaseType;
  End;

Type
  TWPImageFrame = Class(TWPFrame)
  Private
    FImage : TWPWorkingDocumentImagePiece;

    FgrpInfo : TGroupBox;
    FedtInfo : TEdit;
    FspnFrame : TSpinEdit;

    FgrpSize : TGroupBox;
    FcbxSize : TCombobox;
    FspnHeight : TSpinEdit;
    FspnWidth : TSpinEdit;
    FtrckSize : TTrackBar;
    FedtPercent : TEdit;

    FgrpBorder : TGroupBox;
    FspnBWidth : TSpinEdit;
    FcbxColour : TUixHTMLColourComboBox;

    FgrpOther : TGroupBox;
    FcbxVerticalAlignment : TUixCombobox;
    FcbTransparent : TUixCheckBox;
    FcbxTransparent : TUixHTMLColourComboBox;

    FChanging : Boolean;

    Function GetImage : TWPWorkingDocumentImagePiece;
    Procedure SetImage(Const Value: TWPWorkingDocumentImagePiece);

    Procedure spnHeightChange(oSender : TObject);
    Procedure spnHeightExit(oSender : TObject);
    Procedure spnWidthChange(oSender : TObject);
    Procedure spnWidthExit(oSender : TObject);
    Procedure spnBWidthExit(oSender : TObject);
    Procedure trckSizeChange(oSender : TObject);
    Procedure TransparentClick(oSender : TObject);
    Procedure AdjustTracker;
    Procedure SizePolicyChange(oSender : TObject);
  Protected
    Procedure Initialise; Override;
    Procedure Finalise; Override;

  Public
    Function DesiredHeight : Integer; Override;
    Function DesiredWidth : Integer; Override;

    Procedure Restore; Override;
    Procedure Accept; Override;

    Property Image : TWPWorkingDocumentImagePiece Read GetImage Write SetImage;
  End;


Type
  TWPImageDialog = Class (TWPDialog)
    Private
      Function GetImage: TWPWorkingDocumentImagePiece;
      Procedure SetImage(Const Value: TWPWorkingDocumentImagePiece);
      Function GetFrame : TWPImageFrame;
    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;
    Public

      Property Frame : TWPImageFrame read GetFrame;
      Property Image : TWPWorkingDocumentImagePiece Read GetImage Write SetImage;
  End;

Type
  TWPImageMapFrame = Class(TWPFrame)
  Private
    FImage : TWPWorkingDocumentImagePiece;
    FBitmap : TBitmap;
    FChanging : Boolean;
    FArea : TWPImageMapArea;
    FCoord : TWPCoordinate;

    FOnMapAreaChanged: TNotifyEvent;

    FEditBox : TGroupBox;

    // Area maps and its buttons
    FMapBox : TGroupBox;
    FMaps : TListBox;
    FAddMap : TButton;
    FRemoveMap : TButton;

    // Editable info. of an area map
    FAreaEditBox : TGroupBox;
    FTitle : TEdit;
    FUrl : TEdit;
    FKey : TEdit;
    FcbLinkColour : TUixCheckBox;
    FcbxLinkColour : TUixHTMLColourComboBox;
    FcbHoverColour : TUixCheckBox;
    FcbxHoverColour : TUixHTMLColourComboBox;

    // Editable coordinates of an area map
    FCoordsBox :TGroupBox;
    FCoords : TListBox;
    FAddCoord : TButton;
    FRemoveCoord : TButton;

    // Review box
    FreviewBox : TGroupBox;
    FReviewStatus : TStatusBar;
    FImageBox : TUixScrollBox;
    FImageView : TUixImage;

    Function GetImage : TWPWorkingDocumentImagePiece;
    Procedure SetImage(Const Value: TWPWorkingDocumentImagePiece);

    Procedure OnMapAreaSelected(Sender: TObject);
    Procedure OnAddAreaClick(Sender: TObject);
    Procedure OnRemoveAreaClick(Sender: TObject);
    Procedure OnCoordSelected(Sender: TObject);
    Procedure OnAddCoordClick(Sender: TObject);
    Procedure OnRemoveCoordClick(Sender: TObject);

    Procedure OnImageBoxResize(Sender: TObject);
    Procedure OnAreaTitleChanged(Sender: TObject);
    Procedure OnAreaUrlChanged(Sender: TObject);
    Procedure OnAreaKeyChanged(Sender: TObject);
    Procedure OnHoverColourChanged(Sender: TObject);
    Procedure OnHoverColourSelected(Sender: TObject);
    Procedure OnLinkColourChanged(Sender: TObject);
    Procedure OnLinkColourSelected(Sender: TObject);

    Procedure OnMouseMoveInImage(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure OnMouseDownInImage(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    Function GetAreaMapAsText(oArea : TWPImageMapArea): String;
    Procedure UpdateCurrentAreaMapText;

    Procedure DrawAreaMapOnImage;
    Procedure DrawCurrentCoord;

  Protected
    Procedure Initialise; Override;
    Procedure Finalise; Override;

  Public
    Function DesiredHeight : Integer; Override;
    Function DesiredWidth : Integer; Override;

    //Procedure Accept; Override;

    Property Image : TWPWorkingDocumentImagePiece Read GetImage Write SetImage;
    Property OnMapAreaChanged : TNotifyEvent Read FOnMapAreaChanged Write FOnMapAreaChanged;
  End;

Const
  V_PADDING = 5;
  H_PADDING = 5;
  BUTTON_HEIGHT = 20;
  EDIT_BOX_WITH = 300;
  COORD_LIST_WIDTH = 120;


Type
  TWPImageMapDialog = Class (TWPDialog)
    Private
      btnExport : TUixButton;

      sdExport : TUixSaveFileDialog;

      Function GetImage: TWPWorkingDocumentImagePiece;
      Procedure SetImage(Const Value: TWPWorkingDocumentImagePiece);
      Function GetFrame : TWPImageMapFrame;

      Procedure OnExportClick(Sender :TObject);
      Procedure OnMapAreaChanged(Sender :TObject);
    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;
      Procedure Initialise; Overload; Override;
    Public

      Property Frame : TWPImageMapFrame Read GetFrame;
      Property Image : TWPWorkingDocumentImagePiece Read GetImage Write SetImage;
  End;

Type
  TWPSymbolFrame = Class(TWPFrame)
  Private
    FSymbolFont : TWPSFontDetails;
    FInsertChar : Char;

    FGrid : TWordProcessor;
    FOnDone: TNotifyEvent;

    Function GetSymbolFont : TWPSFontDetails;
    Procedure SetSymbolFont(Value : TWPSFontDetails);
    Function GetInsertChar: Char;

    Procedure BuildExample;
    Procedure BuildForm;

    Procedure HotSpotHover(oSender : TWordProcessor; bActive : Boolean; oInfo : TWPHotspotInformation);
    Procedure HotSpot(oSender : TWordProcessor; oInfo : TWPHotspotInformation);
  Protected
    Procedure Initialise; Override;
    Procedure Finalise; Override;
  Public
    Procedure Restore; Override;

    Function DesiredHeight : Integer; Override;
    Function DesiredWidth : Integer; Override;

    Property SymbolFont : TWPSFontDetails Read GetSymbolFont Write SetSymbolFont;
    Property InsertChar : Char Read GetInsertChar;
    Property OnDone : TNotifyEvent Read FOnDone Write FOnDone;
  End;

Type
  TWPSymbolDialog = Class (TWPDialog)
    Private
      FWorkingFont : TWPSFontDetails;
      FSpecifiedFont : TWPSFontDetails;
      FFont : TUixComboBox;
      FFontsAllowed : TFslStringList;
      FAllowSpecialSymbols: Boolean;

      Function GetWorkingFont : TWPSFontDetails;
      Procedure SetWorkingFont(Value : TWPSFontDetails);
      Function GetSpecifiedFont : TWPSFontDetails;
      Procedure SetSpecifiedFont(Value : TWPSFontDetails);

      Function GetFrame : TWPSymbolFrame;
      Function GetInsertChar: Char;

      Procedure FontChange(oSender : TObject);
      Procedure Done(oSender : TObject);
    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;
      Procedure Initialise; Override;
    Public
      destructor Destroy; Override;

      Property Frame : TWPSymbolFrame Read GetFrame;
      Property WorkingFont : TWPSFontDetails Read GetWorkingFont Write SetWorkingFont;
      Property SpecifiedFont : TWPSFontDetails Read GetSpecifiedFont Write SetSpecifiedFont;
      Property InsertChar : Char Read GetInsertChar;
      Property FontsAllowed : TFslStringList Read FFontsAllowed;
      Property AllowSpecialSymbols : Boolean Read FAllowSpecialSymbols Write FAllowSpecialSymbols;

      Function DrawnFontName : String;
  End;

  TWPInsertTableFrame = Class(TWPFrame)
  Private

    FgrpSize : TUixGroupBox;
    FedtRows : TUixEdit;
    FedtCols : TUixEdit;

    Procedure BuildForm;

    Function GetRows : Integer;
    Procedure SetRows(Const Value : Integer);
    Function GetColumns : Integer;
    Procedure SetColumns(Const Value : Integer);
  Protected
    Procedure Initialise; Override;
    Procedure Finalise; Override;

  Public
    Function DesiredHeight : Integer; Override;
    Function DesiredWidth : Integer; Override;

    Procedure Restore; Override;
    Function CanAccept : Boolean; Override;

    Property Rows : Integer Read GetRows Write SetRows;
    Property Columns : Integer Read GetColumns Write SetColumns;
  End;

Type
  TWPInsertTableDialog = Class (TWPDialog)
    Private
      Function GetRows : Integer;
      Procedure SetRows(Const Value : Integer);
      Function GetColumns : Integer;
      Procedure SetColumns(Const Value : Integer);
      Function GetFrame : TWPInsertTableFrame;
    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;
    Public

      Property Frame : TWPInsertTableFrame Read GetFrame;
      Property Rows : Integer Read GetRows Write SetRows;
      Property Columns : Integer Read GetColumns Write SetColumns;
  End;

  TWPTablePropertiesFrame = Class(TWPFrame)
  Private
    FTable      : TWPWorkingDocumentTableStartPiece;
    FTableRow   : TWPWorkingDocumentTableRowStartPiece;
    FTableCell  : TWPWorkingDocumentTableCellStartPiece;

    FExSelectionStart   : Integer;
    FExSelectionEnd     : Integer;
    {FExTableStart       : Integer;
    FExTableEnd         : Integer;}

    // GUI controls
    FExample    : TWordProcessor;
    FMainPage   : TPageControl;
    FTableTab   : TTabSheet;
    FRowTab     : TTabSheet;
    FCellTab    : TTabSheet;

    // Table sheet
    FgrpBorders :                       TUixGroupBox;
    FTableBorderLeft:                   TUixButton;
    FTableBorderRight:                  TUixButton;
    FTableBorderTop:                    TUixButton;
    FTableBorderBottom:                 TUixButton;
    FTableBorderCenterHorizontal:       TUixButton;
    FTableBorderCenterVertical:         TUixButton;
    FTableBorderPolicyCombo:            TUixComboBox;

    FgrpTableMisc:             TUixGroupBox;
    FHorizontalMarginSpinEdit:  TUixEdit;
    FVerticalMarginSpinEdit:    TUixEdit;
    FTableBackground:           TUixHTMLColourComboBox;
    FExpandLastColumn :         TUixCheckBox;

    // Row sheet
    FHeaderCheckbox :           TUixCheckBox;
    FBreakBeforeCheckbox :      TUixCheckBox;
    FRowBackground:             TUixHTMLColourComboBox;
    FLowerPaddingSizeEdit :     TUixEdit;
    FLowerPaddingColourEdit :   TUixHTMLColourComboBox;

    // Cell sheet
    FgrpCellBorders:    TUixGroupBox;
    FCellBorderLeft:    TUixButton;
    FCellBorderRight:   TUixButton;
    FCellBorderTop:     TUixButton;
    FCellBorderBottom:  TUixButton;

    FgrpMargin:                 TUixGroupBox;
    FCellMarginLeftEdit :       TUixEdit;
    FCellMarginTopEdit :        TUixEdit;
    FCellMarginRightEdit :      TUixEdit;
    FCellMarginBottomEdit :     TUixEdit;

    FgrpCellMisc:               TUixGroupBox;
    FVerticalAlignmentCombo :   TUixComboBox;
    FCellWidthEdit :            TUixEdit; // percentage with (0-1) of total available size
    FCellBackground:            TUixHTMLColourComboBox;

    Function GetTable : TWPWorkingDocumentTableStartPiece;
    Procedure SetTable(Const Value : TWPWorkingDocumentTableStartPiece);
    Function GetTableRow : TWPWorkingDocumentTableRowStartPiece;
    Procedure SetTableRow(Const Value : TWPWorkingDocumentTableRowStartPiece);
    Function GetTableCell: TWPWorkingDocumentTableCellStartPiece;
    Procedure SetTableCell(Const Value : TWPWorkingDocumentTableCellStartPiece);

    Procedure BuildForm;
    Procedure BuildTableTab;
    Procedure BuildRowTab;
    Procedure BuildCellTab;

    Procedure LoadPolicyList;
    Procedure ChoosePolicy(oSender : TObject);
    Procedure ChangeHorizontal(oSender : TObject);
    Procedure ChangeVertical(oSender : TObject);
    Procedure ChangeBackgroundColour(oSender: TObject);
    Procedure ChangeRowHeader(oSender: TObject);
    Procedure ChangeExpandLastColumn(oSender: TObject);
    Procedure ChangeRowBreakBefore(oSender: TObject);
    Procedure ChangeRowBackgroundColour(oSender: TObject);
    Procedure ChangeLowerPaddingSize(oSender: TObject);
    Procedure ChangeLowerPaddingColor(oSender: TObject);
    Procedure ChangeCellMargin(oSender: TObject);
    Procedure ChangeCellWidth(oSender: TObject);
    Procedure ChangeCellBackground(oSender: TObject);
    Procedure LoadCellVerticalAlignmentList;
    Procedure ChangeCellVerticalAlignment(oSender: TObject);

    Procedure ChangeBorder(oSender: TObject);

    Procedure CalcSelection(oTable : TWPWorkingDocumentTableStartPiece; iRowStart, iRowEnd, iCellStart, iCellEnd: Integer);
    Procedure MirrorStyle(oSrc, oDest: TWPWorkingDocumentTableStartPiece);
  Protected
    Procedure Initialise; Override;
    Procedure Finalise; Override;

  Public
    Procedure BuildExampleTable(oTable : TWPWorkingDocumentTableStartPiece; iRowStart, iRowEnd, iCellStart, iCellEnd  : Integer);
    Procedure ChangeExampleProperty(oTable: TWPWorkingDocumentTableStartPiece; oRow: TWPWorkingDocumentTableRowStartPiece; oCell: TWPWorkingDocumentTableCellStartPiece);

    Function DesiredHeight : Integer; Override;
    Function DesiredWidth : Integer; Override;

    Procedure Restore; Override;
    Procedure Accept; Override;
    Function CanAccept : Boolean; Override;
    Function ValidatePositiveInt(oControl: TWinControl; Const sValue: String; Const sName: String): Boolean;

    Property Table : TWPWorkingDocumentTableStartPiece Read GetTable Write SetTable;
    Property Row   : TWPWorkingDocumentTableRowStartPiece Read GetTableRow Write SetTableRow;
    Property Cell  : TWPWorkingDocumentTableCellStartPiece Read GetTableCell Write SetTableCell;
  End;


Type
  TWPTablePropertiesDialog = Class (TWPDialog)
    Private
      Function GetTable : TWPWorkingDocumentTableStartPiece;
      Function GetTableRow : TWPWorkingDocumentTableRowStartPiece;
      Function GetTableCell: TWPWorkingDocumentTableCellStartPiece;

      Function GetFrame : TWPTablePropertiesFrame;
    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;

      Function CalcCommonRowFormat(oRows : TWPWorkingDocumentTableRowStartPieces): TWPWorkingDocumentTableRowStartPiece; Overload; Virtual;
      Function CalcCommonCellFormat(oCells : TWPWorkingDocumentTableCellStartPieces): TWPWorkingDocumentTableCellStartPiece; Overload; Virtual;
    Public
      Procedure SetRange(oRange : TWPRange); Overload; Virtual;

      Function CanAccept : Boolean; Override;

      Property Table : TWPWorkingDocumentTableStartPiece Read GetTable;
      Property Row   : TWPWorkingDocumentTableRowStartPiece Read GetTableRow;
      Property Cell  : TWPWorkingDocumentTableCellStartPiece Read GetTableCell;
      Property Frame : TWPTablePropertiesFrame read GetFrame;
  End;

  TWPTableBorderFrame = Class(TWPFrame)
    Private
      FBorder: TWPBorder;

      FBorderStyleCombo:        TUixComboBox;
      FgrpStandard:             TUixGroupBox;
      FWidthEdit:               TUixEdit;
      FPenStyleCombo:           TUixComboBox;
      FColourEdit:              TUixHTMLColourComboBox;

      // Custom Fancy
      FgrpFancy:                TUixGroupBox;
      FLowOuterLimitEdit:       TUixEdit;
      FHighOuterLimitEdit:      TUixEdit;
      FOuterColourEdit:         TUixHTMLColourComboBox;
      FOuterColour2Edit:        TUixHTMLColourComboBox;

      // TODO: custom brush

      Function GetBorder: TWPBorder;
      Procedure SetBorder(Const oBorder: TWPBorder);

      Procedure BuildForm;
      Procedure BuildStandardPropertyInputs;
      Procedure BuildFancyPropertyInputs;

      Procedure EnableControlsToStyle(oStyle : TPredefinedBorderStyles);
      Procedure LoadBorderStyle;
      Procedure LoadPenStyle;

      Procedure OnBorderStyleChange(oSender: TObject);
      Procedure OnIndividualBorderChange(oSender: TObject);

    Protected
      Procedure Initialise; Override;
      Procedure Finalise; Override;

    Public
      Function DesiredHeight : Integer; Override;
      Function DesiredWidth : Integer; Override;

      Procedure Restore; Override;
      Procedure Accept; Override;
      Function CanAccept : Boolean; Override;

      Property Border: TWPBorder Read GetBorder Write SetBorder;
    End;


Type
  TWPTableBorderDialog = Class (TWPDialog)
    Private
      Function GetBorder : TWPBorder;
      Procedure SetBorder(Const oBorder: TWPBorder);

      Function GetFrame : TWPTableBorderFrame;
    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;

    Public
      Function CanAccept : Boolean; Override;

      Property Border: TWPBorder Read GetBorder Write SetBorder;
      Property Frame : TWPTableBorderFrame read GetFrame;
  End;

Type
  TWPLineFrame = Class(TWPFrame)
  Private
    FLine : TWPWorkingDocumentBreakPiece;
    bIsReady : Boolean;

    FgrpSize : TUixGroupBox;
    FcbxAlignment : TUixComboBox;
    FtrckWidth : TTrackBar;
    FcWidth : TUixLabel;

    FgrpPen : TUixGroupBox;
    FspnPenWidth : TSpinEdit;
    FcbxColour : TUixHTMLColourComboBox;
    FcbxStyle : TUixCombobox;
    FcbxEndStyle : TUixCombobox;

    FgrpExample : TUixGroupBox;
    FExample : TWordProcessor;

    Function GetLine : TWPWorkingDocumentBreakPiece;
    Procedure SetLine(Const Value : TWPWorkingDocumentBreakPiece);

    Procedure userAction(oSender : TObject);


    Procedure BuildSize;
    Procedure BuildPen;
    Procedure BuildExample;
    Procedure BuildExampleLine;
    Procedure BuildForm;

    Procedure DoUpdate;
  Protected

    Procedure Initialise; Override;
    Procedure Finalise; Override;

  Public
    Function DesiredHeight : Integer; Override;
    Function DesiredWidth : Integer; Override;

    Procedure Restore; Override;
    Procedure Accept; Override;
    Function CanAccept : Boolean; Override;

    Property Line : TWPWorkingDocumentBreakPiece Read GetLine Write SetLine;
  End;


Type
  TWPLineDialog = Class (TWPDialog)
    Private
      Function GetLine : TWPWorkingDocumentBreakPiece;
      Procedure SetLine(Value : TWPWorkingDocumentBreakPiece);

      Function GetFrame : TWPLineFrame;

    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;
    Public

      Property Frame : TWPLineFrame read GetFrame;
      Property Line : TWPWorkingDocumentBreakPiece Read GetLine Write SetLine;
  End;

  TWPSearchFrame = Class(TWPFrame)
  Private
    FSearchDetails : TWPSearchDetails;
    FRange : TWPRange;

    FgrpText : TUixGroupBox;
    FedtText : TUixEdit;
    FpnlOptions : TUixPanel;
    FgrpOptions : TUixGroupBox;
    FcbCaseSensitive : TUixCheckBox;
    FcbWholeWords : TUixCheckBox;

    FgrpDirection : TUixRadioGroup;
    FgrpScope : TUixRadioGroup;

    Procedure BuildText;
    Procedure BuildOptions;
    Procedure BuildDirection;
    Procedure BuildScope;
    Procedure BuildForm;
    Procedure BindToSearch;
    Procedure ReadSearch;

    Function GetSearchDetails : TWPSearchDetails;
    Function GetRange : TWPRange;
    Procedure SetSearchDetails(Const Value : TWPSearchDetails);
    Procedure SetRange(Const Value : TWPRange);
  Protected
    Procedure Initialise; Override;
    Procedure Finalise; Override;

  Public
    Function DesiredHeight : Integer; Override;
    Function DesiredWidth : Integer; Override;

    Function CanAccept : Boolean; Override;
    Procedure Accept; Override;
    Procedure Restore; Override;

    Function HasSearchDetails : Boolean;
    Function HasRange : Boolean;

    Property SearchDetails : TWPSearchDetails Read GetSearchDetails Write SetSearchDetails;
    Property Range : TWPRange Read GetRange Write SetRange;
    Property EditText : TUixEdit Read FedtText;
  End;

Type
  TWPSearchDialog = Class (TWPDialog)
    Private
      Function GetSearchDetails: TWPSearchDetails;
      Procedure SetSearchDetails(Const Value: TWPSearchDetails);
      Function GetRange : TWPRange;
      Procedure SetRange(Const Value : TWPRange);
      Function GetFrame : TWPSearchFrame;
    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;
    Public
      Procedure Refresh; Override;
      Function CanAccept : Boolean; Override;

      Property SearchDetails : TWPSearchDetails Read GetSearchDetails Write SetSearchDetails;
      Property Range : TWPRange Read GetRange Write SetRange;
      Property Frame : TWPSearchFrame Read GetFrame;
  End;

  TWPReplaceFrame = Class(TWPFrame)
  Private
    FReplaceDetails : TWPReplaceDetails;
    FOriginalSelection : TWPSelection;
    FRange : TWPRange;

    FgrpText : TUixGroupBox;
    FedtText : TUixEdit;
    FedtReplace : TUixEdit;
    FpnlOptions : TUixPanel;
    FgrpOptions : TUixGroupBox;
    FcbCaseSensitive : TUixCheckBox;
    FcbWholeWords : TUixCheckBox;
    FcbPrompt : TUixCheckBox;
    FgrpDirection : TUixRadioGroup;
    FgrpScope : TUixRadioGroup;
    FHasReplacedAll : Boolean;

    Procedure BuildText;
    Procedure BuildOptions;
    Procedure BuildDirection;
    Procedure BuildScope;
    Procedure BuildForm;
    Procedure BindToReplace;
    Function ReadReplace : Boolean;

    Function DoReplace : Boolean;

    Function GetReplaceDetails : TWPReplaceDetails;
    Function GetRange : TWPRange;
    Procedure SetReplaceDetails(Const Value : TWPReplaceDetails);
    Procedure SetRange(Const Value : TWPRange);
  Protected

    Procedure Initialise; Override;
    Procedure Finalise; Override;

  Public
    Function DesiredHeight : Integer; Override;
    Function DesiredWidth : Integer; Override;

    Function CanAccept : Boolean; Override;
    Procedure Accept; Override;
    Procedure Restore; Override;

    Function ReplaceAll : Boolean; Overload; Virtual;

    Function HasReplaceDetails : Boolean;
    Function HasRange : Boolean;

    Property ReplaceDetails : TWPReplaceDetails Read GetReplaceDetails Write SetReplaceDetails;
    Property Range : TWPRange Read GetRange Write SetRange;
    Property EditText : TUixEdit Read FedtText;
  End;


Type
  TWPReplaceDialog = Class (TWPDialog)
    Private
      FbtnAll : TUixButton;

      Function GetReplaceDetails: TWPReplaceDetails;
      Procedure SetReplaceDetails(Const Value: TWPReplaceDetails);
      Function GetRange : TWPRange;
      Procedure SetRange(Const Value : TWPRange);
      Function GetFrame : TWPReplaceFrame;

      Procedure ReplaceAll(oSender : TObject);
    Protected
      Procedure Initialise; Override;
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;
    Public
      Procedure Refresh; Override;
      Function CanAccept : Boolean; Override;

      Property Frame : TWPReplaceFrame Read GetFrame;
      Property ReplaceDetails : TWPReplaceDetails Read GetReplaceDetails Write SetReplaceDetails;
      Property Range : TWPRange Read GetRange Write SetRange;
  End;

Const
  SORT_SIZE = 2;        // number of possible sorting order (can be made dynamic if needed)

Type
  TWPSortTableFrame = Class (TWPFrame)
    Private
      FTable : TWPWorkingDocumentTableStartPiece;

      FSortDetails: TWPSortDetailList;

      // UI controls
      FgrpSorts: Array[0..SORT_SIZE] Of TUixGroupBox;
      FColumnCombos: Array[0..SORT_SIZE] Of TUixComboBox;
      FAscendRadios: Array[0..SORT_SIZE] Of TRadioButton;
      FDescendRadios: Array[0..SORT_SIZE] Of TRadioButton;

      Function GetTable : TWPWorkingDocumentTableStartPiece;
      Procedure SetTable(Const oTable: TWPWorkingDocumentTableStartPiece);

      Procedure BuildForm;
      Procedure EnableControls(iIndex: Integer; bEnable: Boolean = True);

      Procedure LoadColumns;

      Procedure OnColumnComboClick(oSender: TObject);

    Protected
      Procedure Initialise; Override;
      Procedure Finalise; Override;

    Public
      Function DesiredHeight : Integer; Override;
      Function DesiredWidth : Integer; Override;

      Procedure Restore; Override;
      Procedure Accept; Override;
      Function CanAccept : Boolean; Override;

      Property Table: TWPWorkingDocumentTableStartPiece Read GetTable Write SetTable;
      Property SortDetails: TWPSortDetailList Read FSortDetails;
    End;

Type
  TWPSortTableDialog = Class (TWPDialog)
    Private
      Function GetTable : TWPWorkingDocumentTableStartPiece;
      Procedure SetTable(Const oTable: TWPWorkingDocumentTableStartPiece);

      Function GetSortDetails: TWPSortDetailList;

      Function GetFrame : TWPSortTableFrame;
    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;

    Public
      Function CanAccept : Boolean; Override;

      Property Table: TWPWorkingDocumentTableStartPiece Read GetTable Write SetTable;
      Property Sorts: TWPSortDetailList Read GetSortDetails;
      Property Frame : TWPSortTableFrame Read GetFrame;
  End;

  TWPAllowedWordsFrame = Class(TWPFrame)
  Private
    FWords : TFslStringList;
    FToDictionary : TFslStringList;

    FBox : TUixScrollBox;
    FComponents : TComponentList;

    Procedure BuildForm;
    procedure SetWords(const Value: TFslStringList);
    procedure ClearEdits;
    procedure ReloadEdits;

    Procedure EditChange(oSender : TObject);
    Procedure ButtonDelete(oSender : TObject);
    Procedure ButtonMove(oSender : TObject);
  Protected

    Procedure Initialise; Override;
    Procedure Finalise; Override;

  Public
    Function DesiredHeight : Integer; Override;
    Function DesiredWidth : Integer; Override;

    Procedure Restore; Override;
    Function CanAccept : Boolean; Override;

    Property Words : TFslStringList Read FWords Write SetWords;
  End;

  TWPAllowedWordsDialog = Class (TWPDialog)
    Private
      Function GetFrame : TWPAllowedWordsFrame;
      function GetWords: TFslStringList;
      procedure SetWords(const Value: TFslStringList);
    function GetToDictionary: TFslStringList;
    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;
    Public

      Property Frame : TWPAllowedWordsFrame read GetFrame;
      Property Words : TFslStringList Read GetWords Write SetWords;
      Property ToDictionary : TFslStringList Read GetToDictionary;
  End;

  TWPOpenFileDialog = Class(TUixOpenFileDialog)
  Private
    FAllowNative : Boolean;
    FAllowSnapshot : Boolean;
    FFilterFormats : Array of TWPFormat;
    Procedure SetAllowNative(Const Value: Boolean);
    Procedure SetAllowSnapshot(Const Value: Boolean);
    Procedure SetOptions;
    function GetFormat: TWPFormat;
    procedure SetFilterFormats(a : Array of TWPFormat);
  Protected
    Procedure Initialise; Overload; Override;
  Public
    Procedure ExecuteWP(oWP : TWordProcessor); Overload; Virtual;
    Property AllowNative : Boolean Read FAllowNative Write SetAllowNative;
    Property AllowSnapshot : Boolean Read FAllowSnapShot Write SetAllowSnapShot;
    Property Format : TWPFormat read GetFormat;
  End;

  TWPSaveFileDialog = Class(TUixSaveFileDialog)
  Private
    FAllowNative : Boolean;
    FAllowCDA: boolean;
    FFilterFormats : Array of TWPFormat;
    Procedure SetAllowNative(Const Value: Boolean);
    Procedure SetOptions;
    procedure SetAllowCDA(const Value: boolean);
    function GetFormat: TWPFormat;
    procedure SetFilterFormats(a : Array of TWPFormat);
  Protected
    Procedure Initialise; Overload; Override;
  Public
    Procedure ExecuteWP(oWP : TWordProcessor); Overload; Virtual;
    Property AllowNative : Boolean Read FAllowNative Write SetAllowNative;
    Property AllowCDA : boolean read FAllowCDA write SetAllowCDA;
    Property Format : TWPFormat read GetFormat;
  End;

Const
  DEFAULT_EXTENSION = 'cde';
  FILTER_ALL_DOCUMENTS = 'Known Documents|*.xml;*.rtf;*.htm;*.cda;*.zip;*.txt;*.odt;*.cde;*.kdoc;*.html;*.mht';
  FILTER_WORD_DOCUMENT = 'Word Document (*.rtf)|*.rtf';
  FILTER_CDA_DOCUMENT = 'CDA Document (*.xml)|*.xml;*.cda;*.zip';
  FILTER_WP_DOCUMENT = 'CDE Private Format (*.cde)|*.cde;*.kdoc';
  FILTER_SNAPSHOT = 'Snapshots (*.xml)|*.xml';
  FILTER_ODT_DOCUMENT = 'OpenDocument (*.odt)|*.odt';
  FILTER_MHT_DOCUMENT = 'Web Archive (*.mht)|*.mht';
  FILTER_CONSOLEDOCUMENT = 'PLS Word Processor Document (*.pls)|*.pls';


Type
  TWPFieldModelFrame = Class(TWPFrame)
    Private
      FModel : TWPFieldModel;
      FTreeView : TUixTreeView;
      FSection: Boolean;
      FEntries : TWPFieldEntryList;
      FIsInitialColumnSetupDone : Boolean;

      Function GetModel: TWPFieldModel;
      Procedure SetModel(Const Value: TWPFieldModel);

      Procedure DoGetNode(oSender: TObject; oNode: TUixTreeViewNode);
      Procedure DoRenderNode(oSender: TObject; oNode: TUixTreeViewNode);

      Function GetSelected : TWPFieldEntry;
      Procedure SetSelected (Const Value: TWPFieldEntry);

      Function GetHasModel: Boolean;

    Protected
      Procedure Initialise; Override;
      Procedure Finalise; Override;

    Public
      Procedure Refresh; Override;

      Function DesiredHeight : Integer; Override;
      Function DesiredWidth : Integer; Override;

      Function HasSelected : Boolean; Overload; Virtual;

      Property Model : TWPFieldModel Read GetModel Write SetModel;
      Property HasModel : Boolean Read GetHasModel;
      Property Selected : TWPFieldEntry Read GetSelected Write SetSelected;
      Property TreeView : TUixTreeView Read FTreeView;
      Property Section : Boolean Read FSection Write FSection;
  End;


Type
  TWPFieldModelDialog = Class(TWPDialog)
    Private
      Function GetFrame: TWPFieldModelFrame;

      Function GetModel: TWPFieldModel;
      Procedure SetModel(Const Value: TWPFieldModel);

      Function GetSelected: TWPFieldEntry;
      Procedure SetSelected(Const Value: TWPFieldEntry);

      Procedure DoFrameTreeViewDoubleClick(oSender : TObject; oNode : TUixTreeViewNode);
      Function GetSection: Boolean;
      Procedure SetSection(Const Value: Boolean);

    Protected
      Procedure Initialise; Override;

      Function FrameClass : TWPFrameClass; Overload; Override;

      Function CanAccept : Boolean; Overload; Override;

    Public
      Procedure Restore; Override;

      Function DialogCaption : String; Overload; Override;

      Property Frame : TWPFieldModelFrame Read GetFrame;
      Property Model : TWPFieldModel Read GetModel Write SetModel;
      Property Section : Boolean Read GetSection Write SetSection;
      Property Selected : TWPFieldEntry Read GetSelected Write SetSelected;
  End;

Type
  TWPInputFieldFrame = Class(TWPFrame)
    Private
      FField : TWPDocumentField;
      FSection : TWPDocumentSection;

      FIdentityBox : TUixGroupBox;
      FNameEdit : TUixEdit;
      FDeletableCheckBox : TUixCheckBox;
      FKeyEdit : TUixEdit;
      FWidthSpin : TSpinEdit;

      FDetailsBox : TUixGroupBox;
      FrbBoolean : TUixRadioButton;
      FrbText : TUixRadioButton;
      FrbList : TUixRadioButton;
      FrbParagraph : TUixRadioButton;
      FrbNumber : TUixRadioButton;
      FrbInteger : TUixRadioButton;
      FrbDate : TUixRadioButton;

      FFormatableCombo : TUixComboBox;
      FForceInputCheckBox : TUixCheckBox;
      FMandatoryCheckBox : TUixCheckBox;

      FedIntMinimumLabel : TUixLabel;
      FedIntMinimum : TSpinEdit;
      FedIntMaximumLabel : TUixLabel;
      FedIntMaximum : TSpinEdit;
      FedtFloatMainLabel : TUixLabel;
      FedtFloatMain : TSpinEdit;
      FedtFloatDecimalLabel : TUixLabel;
      FedtFloatDecimal : TSpinEdit;
      FchkDateTimeLabel : TUixLabel;
      FchkDateTime : TUixCombobox;
      FedtTextRegexLabel : TUixLabel;
      FedtTextRegex : TUixEdit;
      FedtTextDescLabel : TUixLabel;
      FedtTextDesc : TUixEdit;
      FedtTextMaxLabel : TUixLabel;
      FedtTextMax : TSpinEdit;
      FcbxListModeLabel : TUixLabel;
      FcbxListMode : TUixCombobox;
      FmemLstListLabel: TUixLabel;
      FmemLstList: TUixMemo;

      Function GetField : TWPDocumentField;
      Procedure SetField(Const Value: TWPDocumentField);
      Function GetSection : TWPDocumentSection;
      Procedure SetSection(Const Value: TWPDocumentSection);

      Procedure BindField;
      Procedure BindSection;

      Procedure OnKeyEditChanged(oSender: TObject);
      Procedure rbClick(oSender: TObject);
      Procedure CheckForceInputState(oSender: TObject);
      Function MakeTypeRadioButton(iTop : integer; Const sCaption : String) : TUixRadioButton;
    Protected
      Procedure Initialise; Override;
      Procedure Finalise; Override;

    Public

      Function DesiredHeight : Integer; Override;
      Function DesiredWidth : Integer; Override;

      Procedure FieldOnly;
      Procedure SectionOnly;
      Procedure FieldOrSection;
      Function IsSection : Boolean;

      Procedure Accept; Override;

      Property Field : TWPDocumentField Read GetField Write SetField;
      Property Section : TWPDocumentSection Read GetSection Write SetSection;
      Property NameEdit : TUixEdit Read FNameEdit;
  End;


Type
  TWPInputFieldDialog = Class (TWPDialog)
    Private
      Function GetField: TWPDocumentField;
      Procedure SetField(Const Value: TWPDocumentField);
      Function GetSection: TWPDocumentSection;
      Procedure SetSection(Const Value: TWPDocumentSection);
      Function GetFrame : TWPInputFieldFrame;
    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;

      Function CanAccept : Boolean; Override;

    Public
      Procedure Restore; Override;

      Procedure FieldOnly;
      Procedure SectionOnly;
      Procedure FieldOrSection;

      Function IsSection : Boolean;

      Property Frame : TWPInputFieldFrame Read GetFrame;
      Property Field : TWPDocumentField Read GetField Write SetField;
      Property Section : TWPDocumentSection Read GetSection Write SetSection;
  End;

Type
  TWPCommentFrame = Class(TWPFrame)
  Private
    FText : TUixMemo;

    Function GetComment : String;

    Procedure BuildForm;
    procedure SetComment(const Value: String);
  Protected
    Procedure Initialise; Override;
    Procedure Finalise; Override;
  Public
    Procedure Restore; Override;

    Function DesiredHeight : Integer; Override;
    Function DesiredWidth : Integer; Override;

    Property Comment : String Read GetComment Write SetComment;
  End;

Type
  TWPCommentDialog = Class (TWPDialog)
    Private
      FWantDelete: Boolean;
      Function GetFrame : TWPCommentFrame;
      Function GetComment : String;

      procedure SetComment(Value: String);
      procedure DoDelete(sender : TObject);
    Protected
      Function FrameClass : TWPFrameClass; Overload; Override;
      Function DialogCaption : String; Overload; Override;
      Procedure Initialise; Override;
    Public

      Property Frame : TWPCommentFrame read GetFrame;
      Property Comment : String read GetComment Write SetComment;
      Property WantDelete : Boolean read FWantDelete write FWantDelete;
  End;


Implementation

uses
  FHIR.WP.Builder;

Procedure TWPHotspotFrame.Initialise;
Begin
  Inherited;

  AddLabel(ClientPanel, 21, 10, 'URL');
  FURLEdit := TEdit.Create(ClientPanel);
  FURLEdit.Parent := ClientPanel;
  FURLEdit.Top := 19;
  FURLEdit.Left := 70;
  FURLEdit.Width := 194;

  AddLabel(ClientPanel, 47, 10, 'Hot Key');
  FKeyEdit := TEdit.Create(ClientPanel);
  FKeyEdit.Parent := ClientPanel;
  FKeyEdit.CharCase := ecUpperCase;
  FKeyEdit.MaxLength := 1;
  FKeyEdit.AutoSelect := True;
  FKeyEdit.Top := 45;
  FKeyEdit.Left := 70;
  FKeyEdit.Width := 194;
  FKeyEdit.OnChange := OnKeyEditChanged;

  FURLUnderlineCheckBox := TCheckBox.Create(ClientPanel);
  FURLUnderlineCheckBox.Parent := ClientPanel;
  FURLUnderlineCheckBox.Caption := ' Underline URL Link';
  FURLUnderlineCheckBox.Top := 73;
  FURLUnderlineCheckBox.Left := 20;
  FURLUnderlineCheckBox.Width := 194;

  AddLabel(ClientPanel, 99, 10, 'Link');
  FLinkColor := TUixHTMLColourComboBox.Create(ClientPanel);
  FLinkColor.Parent := ClientPanel;
  FLinkColor.Top := 97;
  FLinkColor.Left := 70;
  FLinkColor.Width := 194;

  AddLabel(ClientPanel, 125, 10, 'Hover');
  FHoverColor := TUixHTMLColourComboBox.Create(ClientPanel);
  FHoverColor.Parent := ClientPanel;
  FHoverColor.Top := 123;
  FHoverColor.Left := 70;
  FHoverColor.Width := 194;
End;


Procedure TWPHotspotFrame.Finalise;
Begin
  FField.Free;

  Inherited;
End;


Procedure TWPHotspotFrame.OnKeyEditChanged(oSender: TObject);
Begin
  If FField.HasHotspot Then
  Begin
    If (Length(FKeyEdit.Text) > 0) And Not CharInSet(FKeyEdit.Text[1], ['A'..'Z', '0'..'9']) Then
      FKeyEdit.Text := FField.Hotspot.Key;
  End;
End;

Procedure TWPHotspotFrame.Accept;
Begin
  FField.HasHotspot := True;
  FField.Hotspot.Key := FKeyEdit.Text;
  FField.Hotspot.URL := FURLEdit.Text;
  FField.Hotspot.LinkColour := FLinkColor.Value;
  FField.Hotspot.HoverColour := FHoverColor.Value;
  FField.Hotspot.LinkUnderline := FURLUnderlineCheckBox.Checked;
End;


Function TWPHotspotFrame.DesiredHeight : Integer;
Begin
  Result := 152;
End;


Function TWPHotspotFrame.DesiredWidth : Integer;
Begin
  Result := 277;
End;


Function TWPHotspotFrame.GetField : TWPDocumentField;
Begin
  Assert(Invariants('GetField', FField, TWPDocumentField, 'Field'));

  Result := FField;
End;


Procedure TWPHotspotFrame.SetField(Const Value : TWPDocumentField);
Begin
  FField.Free;
  FField := Value;

  If FField.HasHotspot Then
  Begin
    FKeyEdit.Text := FField.Hotspot.Key;
    FURLEdit.Text := FField.Hotspot.URL;
    FLinkColor.Value := FField.Hotspot.LinkColour;
    FHoverColor.Value := FField.Hotspot.HoverColour;
    FURLUnderlineCheckBox.Checked := FField.Hotspot.LinkUnderline;
  End
  Else
  Begin
    FKeyEdit.Text := '';
    FURLEdit.Text := '';
    FLinkColor.Value := DEF_COLOUR;
    FHoverColor.Value := DEF_COLOUR;
    FURLUnderlineCheckBox.Checked := False;
  End;
End;


Function TWPHotspotDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPHotspotFrame;
End;


Function TWPHotspotDialog.DialogCaption : String;
Begin
  Result := 'Hotspot Details';
End;

Function TWPHotspotDialog.CanAccept: Boolean;
Begin
  Result := Inherited CanAccept;

  If Result Then
  Begin
    Result := Frame.URLEdit.Text <> '';

    If Not Result Then
      Invalid(Frame.URLEdit, 'URL', 'URL must be specified.');
  End;
End;


Function TWPHotspotDialog.GetFrame : TWPHotspotFrame;
Begin
  Result := TWPHotspotFrame(Inherited Frame);
End;


Function TWPHotspotDialog.GetField : TWPDocumentField;
Begin
  Result := Frame.Field;
End;


Procedure TWPHotspotDialog.SetField(Const Value : TWPDocumentField);
Begin
  Frame.Field := Value;
End;

Procedure TWPHotspotDialog.Restore;
Begin
  Inherited;
  ClientHeight := Frame.DesiredHeight + BottomPanel.Height;
End;


Procedure TWPDialog.Initialise;
Begin
  Inherited;

  Assert(Invariants('Initialise', FrameClass, TWPFrame, 'FrameClass'));

  PopupWindowStyle := True;
  HasAutomaticDocking := False;
  HasBottomPanel := True;
  HasClientPanel := True;

  FFrame := FrameClass.Create(ClientPanel);
  FFrame.Parent := ClientPanel;
  FFrame.AlignClient;

  Caption := DialogCaption;
  BorderStyleDialog;

  ClientHeight := FFrame.DesiredHeight + BottomPanel.Height;
  ClientWidth := FFrame.DesiredWidth;
End;


Procedure TWPDialog.SetReadOnly(Const Value : Boolean);
Begin
  FReadOnly := Value;
  OKButton.Enabled := Not FReadOnly;
End;


Function TWPDialog.GetFrame : TWPFrame;
Begin
  Assert(Invariants('GetFrame', FFrame, TWPFrame, 'Frame'));

  Result := FFrame;
End;


Procedure TWPDialog.Restore;
Begin
  Inherited;

  Frame.Restore;
End;


Procedure TWPDialog.Refresh;
Begin
  Inherited;

  Frame.Refresh;
End;


Procedure TWPDialog.Accept;
Begin
  Inherited;

  Frame.Accept;
End;


Function TWPDialog.FrameClass : TWPFrameClass;
Begin
  Result := Nil;
End;


Function TWPDialog.DialogCaption : String;
Begin
  Result := '';
  Error('DialogCaption', 'Must override '+ClassName+'.DialogCaption');
End;


function TWPDialog.GetHasFrame: Boolean;
begin
  Result := Assigned(FFrame);
end;



Procedure TWPFrame.Initialise;
Begin
  Inherited;

  HasBottomPanel := False;
  HasAutomaticDocking := True;
  HasClientPanel := True;
End;


Function TWPFrame.DesiredHeight : Integer;
Begin
  Result := 0;
End;


Function TWPFrame.DesiredWidth : Integer;
Begin
  Result := 0;
End;


Function TWPFrame.AddLabel(oParent: TWinControl; iTop, iLeft: Integer; Const sCaption: String) : TUixLabel;
Begin
  Result := TUixLabel.Create(Self);
  Result.Parent := oParent;
  Result.Top := iTop;
  Result.Left := iLeft;
  Result.Caption := sCaption;
End;

Procedure TWPCodeCompletionDialog.Initialise;
Begin
  Inherited;

  PopupWindowStyle := True;
  HasAutomaticDocking := False;
  HasBottomPanel := False;
  HasClientPanel := False;

  BorderStyleNone;

  ClientHeight := 200;
  ClientWidth := MAX_WIDTH;

  FListBox := TListBox.Create(Self);
  FListBox.Parent := Self;
  FListBox.Align := alClient;
  FListBox.OnDblClick := ListBoxDoubleClick;

  FList := Nil;

  KeyPreview := True;
  OnKeyDown := DoKeyDown;
End;


Procedure TWPCodeCompletionDialog.Finalise;
Begin
  FList.Free;

  Inherited;
End;


Function TWPCodeCompletionDialog.GetList : TWPCompletionItems;
Begin
  Assert(Invariants('GetList', FList, TWPCompletionItems, 'List'));
  Result := FList;
End;


Procedure TWPCodeCompletionDialog.SetList(Const Value: TWPCompletionItems);
Var
  iWidth : Integer;
  iLoop : Integer;
Begin
  FList.Free;
  FList := Value;
  ClientHeight := FListBox.ItemHeight * IntegerMin(MAX_ITEM_DISPLAY, FList.Count);
  Canvas.Font.Assign(FListBox.Font);
  iWidth := 0;
  For iLoop := 0 To FList.Count - 1 Do
    iWidth := IntegerMax(iWidth, Canvas.TextWidth(FList[iLoop].Display)+20);
  ClientWidth := IntegerMin(iWidth, MAX_WIDTH);
End;


Procedure TWPCodeCompletionDialog.Restore;
Var
  iLoop : Integer;
Begin
  Inherited;

  For iLoop := 0 To FList.Count- 1 Do
    FListBox.Items.Add(FList[iLoop].Display);

  FListBox.ItemIndex := 0;
End;


Procedure TWPCodeCompletionDialog.DoKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  If Key = vkEscape Then
    ModalResult := mrCancel
  Else If Key = vkEnter Then
    ModalResult := mrOk;
End;


Function TWPCodeCompletionDialog.Selected: TWPCompletionItem;
Begin
  Result := FList[FListBox.ItemIndex];
End;


Procedure TWPCodeCompletionDialog.SetCodePosition(Const aPoint : TPoint);
Begin
  Top := IntegerMin(aPoint.Y, Screen.DesktopHeight - Height);
  Left := IntegerMin(aPoint.X, Screen.DesktopWidth - Width);
End;


Procedure TWPCodeCompletionDialog.ListBoxDoubleClick(Sender: TObject);
Begin
  ModalResult := mrOk;
End;


{ TWPAdornmentFrame }

Function TWPAdornmentFrame.DesiredHeight: Integer;
Begin
  Result := 390;
End;


Function TWPAdornmentFrame.DesiredWidth: Integer;
Begin
  Result := 240;
End;


Procedure TWPAdornmentFrame.Initialise;
Begin
  Inherited;

  BuildForm;
End;


Procedure TWPAdornmentFrame.BuildPen;
var
  aLoop : TFslPenStyle;
Begin
  FgrpPen := TUixGroupBox.Create(Self);
  FgrpPen.Parent := ClientPanel;
  FgrpPen.align := alTop;
  FgrpPen.Caption := ' Pen ';

  AddLabel(FgrpPen, 24, 20, 'Width');
  FspnPenWidth := TSpinEdit.Create(Self);
  FspnPenWidth.Parent := FgrpPen;
  FspnPenWidth.Left := 70;
  FspnPenWidth.Top := 22;
  FspnPenWidth.Width := 130;
  FspnPenWidth.MinValue := 1;
  FspnPenWidth.MaxValue := 12;
  FspnPenWidth.OnChange := userAction;

  AddLabel(FgrpPen, 50, 20, 'Colour');
  FcbxColour := TUixHTMLColourComboBox.Create(Self);
  FcbxColour.Parent := FgrpPen;
  FcbxColour.Left := 70;
  FcbxColour.Top := 48;
  FcbxColour.Width := 150;
  FcbxColour.OnClick := userAction;

  AddLabel(FgrpPen, 76, 20, 'Style');
  FcbxStyle := TUixComboBox.Create(Self);
  FcbxStyle.Parent := FgrpPen;
  FcbxStyle.Top := 74;
  FcbxStyle.Left := 70;
  FcbxStyle.Width := 150;
  FcbxStyle.Style := csDropDownList;
  For aLoop := apsSolid to apsDot Do
    FcbxStyle.AddValue(ADVPENSTYLE_NAMES[aLoop]);
  FcbxStyle.OnClick := userAction;

  FgrpPen.Height := FcbxStyle.Top + FcbxStyle.Height + 10;
End;


Procedure TWPAdornmentFrame.BuildExample;
Begin
  FgrpExample := TUixGroupBox.Create(Self);
  FgrpExample.Parent := ClientPanel;
  FgrpExample.align := alClient;
  FgrpExample.Caption := ' Sample ';

  FExample := TUixImage.Create(Self);
  FExample.Parent := FgrpExample;
  FExample.Top := 10;
  FExample.Left := 10;
  FExample.Width := 240;
  FExample.Height := 100;

End;



Function EnumFontsProc(Var LogFont: TLogFont; Var TextMetric: TTextMetric; FontType: Integer; Data: Pointer): Integer; Stdcall;
Var
  oList : TStringList;
Begin
  Result := 1;
  oList := TStringlist(Data);
  If FontType And TRUETYPE_FONTTYPE > 0 Then
    oList.Add(LogFont.lfFaceName);
End;


Procedure TWPAdornmentFrame.LoadFontNames;
Var
  oList : TStringList;
  iLoop : Integer;
Begin
  FcbxFont.items.Clear;
  oList := TStringList.Create;
  Try
    EnumFonts(Canvas.Handle,  Nil, @EnumFontsProc, pointer(oList));
    oList.Sort;
    For iLoop := 0 To oList.Count - 1 Do
      If (FFontsAllowed = Nil) Or (FFontsAllowed.Count = 0) Or (FFontsAllowed.ExistsByValue(oList[iLoop])) Then
        FcbxFont.items.Add(oList[iLoop]);
  Finally
    oList.Free;
  End;
End;


Procedure TWPAdornmentFrame.BuildCaption;
Begin
  FgrpCaption := TUixGroupBox.Create(Self);
  FgrpCaption.Parent := ClientPanel;
  FgrpCaption.align := alTop;
  FgrpCaption.Caption := ' Caption ';

  AddLabel(FgrpCaption, 24, 20, 'Text');
  FCaption := TUixEdit.Create(Self);
  FCaption.Parent := FgrpCaption;
  FCaption.Top := 24;
  FCaption.Left := 70;
  FCaption.Width := 150;
  FCaption.OnChange := userAction;

  AddLabel(FgrpCaption, 50, 20, 'Font');
  FcbxFont := TUixComboBox.Create(Self);
  FcbxFont.Parent := FgrpCaption;
  FcbxFont.Top := 48;
  FcbxFont.Left := 70;
  FcbxFont.Width := 150;
  FcbxFont.Style := csDropDownList;
  LoadFontNames;
  FcbxFont.OnClick := userAction;

  AddLabel(FgrpCaption, 76, 20, 'Size');
  FcbxSize := TUixComboBox.Create(Self);
  FcbxSize.Parent := FgrpCaption;
  FcbxSize.Top := 74;
  FcbxSize.Left := 70;
  FcbxSize.Width := 150;
  FcbxSize.Style := csDropDownList;
  FcbxSize.AddValues(['7', '8', '9', '10', '12', '14', '16', '18', '20', '24', '30', '36']);
  FcbxSize.OnClick := userAction;

  AddLabel(FgrpCaption, 102, 20, 'Colour');
  FcbxFontColour := TUixHTMLColourComboBox.Create(Self);
  FcbxFontColour.Parent := FgrpCaption;
  FcbxFontColour.Top := 100;
  FcbxFontColour.Left := 70;
  FcbxFontColour.Width := 150;
  FcbxFontColour.OnClick := userAction;

  FgrpCaption.Height := FcbxFontColour.Top + FcbxFontColour.Height + 10;
End;



Procedure TWPAdornmentFrame.BuildForm;
Begin
  BuildExample;
  BuildCaption;
  BuildPen;
End;


Procedure TWPAdornmentFrame.Restore;
Begin
  Inherited;
  FspnPenWidth.Value := Adornment.PenWidth;
  FcbxColour.Value := Adornment.PenColour;
  FcbxStyle.Value := ord(Adornment.PenStyle);

  FcbxStyle.Visible := Adornment.PenWidth = 1;

  FcbxFont.ItemIndex := FcbxFont.Items.IndexOf(Adornment.Font.Name);
  if Adornment.Font.Size = DEF_WORD Then
    FcbxSize.Text := ''
  Else
    FcbxSize.ItemIndex := FcbxSize.Items.IndexOf(IntToStr(Adornment.Font.Size));

  FcbxFontColour.Value := Adornment.Font.Foreground;
  FCaption.Text := Adornment.Caption;


  DrawExample;
  bIsReady := True;
End;


Procedure TWPAdornmentFrame.DoUpdate;
Begin
  Adornment.PenWidth := FspnPenWidth.Value;
  Adornment.PenColour := FcbxColour.Value;
  Adornment.PenStyle := TFslPenStyle(FcbxStyle.Value);
  Adornment.Font.Name := FcbxFont.Text;
  Adornment.Font.Size := StrToIntDef(FcbxSize.Text, DEF_WORD);
  Adornment.Font.Foreground := FcbxFontColour.Value;
  Adornment.Caption := FCaption.Text;
  FcbxStyle.Visible := Adornment.PenWidth = 1;

  If (Adornment.Caption <> '') And (Adornment.CaptionPoint.X = 0) And (Adornment.CaptionPoint.Y = 0) Then
  Begin
    Adornment.CaptionPoint.X := Adornment.Coordinates[0].X;
    Adornment.CaptionPoint.Y := Adornment.Coordinates[0].Y;
  End;
  DrawExample;
End;



Function TWPAdornmentFrame.CanAccept : Boolean;
Begin
  Result := Inherited CanAccept And ((Adornment.AdornmentType <> iatMark) Or (Adornment.Caption <> ''));
End;


Procedure TWPAdornmentFrame.Accept;
Begin
  Inherited;
  DoUpdate();
End;


Procedure TWPAdornmentFrame.userAction(oSender: TObject);
Begin
  If bIsReady Then
    DoUpdate();
End;

{$R-}

Procedure Gradient(Canvas:TCanvas; Rect:TRect; FromColor, ToColor:TColor);
var
  X:integer;
  dr,dg,db:Extended;
  C1,C2:TColor;
  r1,r2,g1,g2,b1,b2:Byte;
  R,G,B:Byte;
  cnt:integer;
begin
  C1 := FromColor;
  R1 := GetRValue(C1);
  G1 := GetGValue(C1);
  B1 := GetBValue(C1);

  C2 := ToColor;
  R2 := GetRValue(C2);
  G2 := GetGValue(C2);
  B2 := GetBValue(C2);

  dr := (R2-R1) / (Rect.Right-Rect.Left);
  dg := (G2-G1) / (Rect.Right-Rect.Left);
  db := (B2-B1) / (Rect.Right-Rect.Left);

  cnt := 0;
  for X := Rect.Left to Rect.Right-1 do
  begin
    R := R1+Ceil(dr*cnt);
    G := G1+Ceil(dg*cnt);
    B := B1+Ceil(db*cnt);

    Canvas.Pen.Color := RGB(R,G,B);
    Canvas.MoveTo(X,Rect.Top);
    Canvas.LineTo(X,Rect.Bottom);
    inc(cnt);
  end;
end;


procedure TWPAdornmentFrame.DrawExample;
var
  oBmp : TBitmap;
  oCanvas : TCanvas;
  aRect : TRect;
Const
  MARK__IN = 5;
  MARK_OUT = 10;
begin
  If Adornment = nil then
    exit;

  oBmp := TBitmap.Create;
  Try
    oCanvas := oBmp.Canvas;

    oCanvas.Lock;
    Try
      oBmp.Height := FExample.Height;
      oBmp.Width := FExample.Width;
      oCanvas.CopyMode := cmMergeCopy;
      oCanvas.Pen.Style := psSolid;
      oCanvas.Pen.Mode := pmCopy;
      oCanvas.Pen.Width := 1;
      aRect.Top := 5;
      aRect.Left := 50;
      aRect.Bottom := FExample.Height - 5;
      aRect.Right := FExample.Width - 50;
      Gradient(oCanvas, aRect, clWhite, clBlack);

      oCanvas.Brush.Style := bsSolid;
      oCanvas.Brush.Color := clWhite;
      oCanvas.Pen.Color := clWhite;
      oCanvas.Rectangle(20, aRect.Top, 50, aRect.Bottom);
      oCanvas.Brush.Color := clBlack;
      oCanvas.Pen.Color := clBlack;
      oCanvas.Rectangle(FExample.Width - 50, aRect.Top, FExample.Width - 20, aRect.Bottom);

      Case Adornment.AdornmentType of
        iatLine :
        Begin
          oCanvas.Brush.Style := bsClear;
          oCanvas.Pen.Style := ADVPENSTYLE_VCLVALUES[Adornment.PenStyle];
          oCanvas.Pen.Color := Adornment.PenColour;
          oCanvas.Pen.Width := Adornment.PenWidth;
          oCanvas.MoveTo(30,(aRect.Top + aRect.Bottom) div 2);
          oCanvas.LineTo(FExample.Width - 30,(aRect.Top + aRect.Bottom) div 2);

          If Adornment.Caption <> '' Then
          Begin
            oCanvas.Brush.Style := bsClear;
            oCanvas.Font.Color := Adornment.PenColour;
            Adornment.Font.Apply(oCanvas.Font);
            oCanvas.TextOut(30, (aRect.Top + aRect.Bottom) div 2 - 5 - oCanvas.TextHeight(Adornment.Caption), Adornment.Caption);
          End;
        End;
        iatRectangle, iatCircle, iatZoom:
        Begin
          oCanvas.Brush.Style := bsClear;
          oCanvas.Pen.Style := ADVPENSTYLE_VCLVALUES[Adornment.PenStyle];
          oCanvas.Pen.Color := Adornment.PenColour;
          oCanvas.Pen.Width := Adornment.PenWidth;
          if Adornment.AdornmentType = iatCircle Then
            oCanvas.Ellipse(30, 15, FExample.Width - 30, FExample.Height - 15)
          Else
            oCanvas.Rectangle(30, 15, FExample.Width - 30, FExample.Height - 15);

          If Adornment.Caption <> '' Then
          Begin
            oCanvas.Brush.Style := bsClear;
            oCanvas.Font.Color := Adornment.PenColour;
            Adornment.Font.Apply(oCanvas.Font);
            if Adornment.AdornmentType = iatCircle Then
              oCanvas.TextOut((aRect.Left + aRect.Right) div 2 - oCanvas.TextWidth(Adornment.Caption), (aRect.Top + aRect.Bottom) div 2 - oCanvas.TextHeight(Adornment.Caption) div 2, Adornment.Caption)
            Else
              oCanvas.TextOut(40, (aRect.Top + aRect.Bottom) div 2 - 5 - oCanvas.TextHeight(Adornment.Caption), Adornment.Caption);
          End;
        End;
        iatMark :
        Begin
          oCanvas.Brush.Style := bsClear;
          oCanvas.Pen.Style := ADVPENSTYLE_VCLVALUES[Adornment.PenStyle];
          oCanvas.Pen.Color := Adornment.PenColour;
          oCanvas.Pen.Width := Adornment.PenWidth;

          oCanvas.MoveTo(30 - MARK_OUT, 15);
          oCanvas.LineTo(30 - MARK__IN, 15);
          oCanvas.MoveTo(30 + MARK_OUT, 15);
          oCanvas.LineTo(30 + MARK__IN, 15);
          oCanvas.MoveTo(30, 15+MARK_OUT);
          oCanvas.LineTo(30, 15+MARK__IN);
          oCanvas.MoveTo(30, 15-MARK_OUT);
          oCanvas.LineTo(30, 15-MARK__IN);

          oCanvas.MoveTo(30, 15);
          oCanvas.LineTo(60, (aRect.Top + aRect.Bottom) div 2);

          oCanvas.MoveTo(FExample.Width - 30 - MARK_OUT, 15);
          oCanvas.LineTo(FExample.Width - 30 - MARK__IN, 15);
          oCanvas.MoveTo(FExample.Width - 30 + MARK_OUT, 15);
          oCanvas.LineTo(FExample.Width - 30 + MARK__IN, 15);
          oCanvas.MoveTo(FExample.Width - 30, 15+MARK_OUT);
          oCanvas.LineTo(FExample.Width - 30, 15+MARK__IN);
          oCanvas.MoveTo(FExample.Width - 30, 15-MARK_OUT);
          oCanvas.LineTo(FExample.Width - 30, 15-MARK__IN);


          oCanvas.MoveTo(FExample.Width - 30, 15);
          oCanvas.LineTo(FExample.Width - 60, (aRect.Top + aRect.Bottom) div 2 + 30);

          oCanvas.Brush.Style := bsClear;
          oCanvas.Font.Color := Adornment.PenColour;
          Adornment.Font.Apply(oCanvas.Font);
          oCanvas.TextOut(60, (aRect.Top + aRect.Bottom) div 2, Adornment.Caption);
          oCanvas.TextOut(FExample.Width - 60 - oCanvas.TextWidth(Adornment.Caption), (aRect.Top + aRect.Bottom) div 2 + 30, Adornment.Caption);
        End;
      End;

      FExample.Picture.Assign(oBmp);
    Finally
      oCanvas.Unlock;
    End;
  Finally
    oBmp.Free;
  End;
end;

procedure TWPAdornmentFrame.Finalise;
begin
  FAdornment.Free;
  FFontsAllowed.Free;
  inherited;
end;

procedure TWPAdornmentFrame.SetFontsAllowed(const Value: TFslStringList);
begin
  FFontsAllowed.Free;
  FFontsAllowed := Value;
end;

Function TWPAdornmentFrame.GetAdornment : TWPDocumentImageAdornment;
Begin
  Result := FAdornment;
End;


Procedure TWPAdornmentFrame.SetAdornment(Const Value : TWPDocumentImageAdornment);
Begin
  FAdornment.Free;
  FAdornment := Value;
End;





procedure TWPAdornmentFrame.Resize;
begin
  inherited;
  DrawExample;
end;


{ TWPAdornmentDialog }

Function TWPAdornmentDialog.GetFrame : TWPAdornmentFrame;
Begin
  Result := TWPAdornmentFrame(Inherited Frame);
End;


Function TWPAdornmentDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPAdornmentFrame;
End;



Function TWPAdornmentDialog.GetAdornment : TWPDocumentImageAdornment;
Begin
  Result := Frame.Adornment;
End;


Procedure TWPAdornmentDialog.SetAdornment(Value : TWPDocumentImageAdornment);
Begin
  Frame.Adornment := Value;
End;


Function TWPAdornmentDialog.DialogCaption : String;
Begin
  if Adornment = nil Then
    Result := 'Item Details'
  Else
    Result := IMAGE_ADORNMENT_TYPE_CODES[Adornment.AdornmentType]+' Details';
End;

function TWPAdornmentDialog.GetFontsAllowed: TFslStringList;
begin
  Result := GetFrame.FontsAllowed;
end;

procedure TWPAdornmentDialog.SetFontsAllowed(const Value: TFslStringList);
begin
  GetFrame.FontsAllowed := Value;
end;

{ TWPFontDialog }

Function TWPFontDialog.GetFont: TWPSFontDetails;
Begin
  Result := Frame.Font;
End;

Procedure TWPFontDialog.SetFont(Const Value: TWPSFontDetails);
Begin
  Frame.Font := Value;
End;


Function TWPFontDialog.GetFrame : TWPFontFrame;
Begin
  Result := TWPFontFrame(Inherited Frame);
End;


Function TWPFontDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPFontFrame;
End;


Function TWPFontDialog.DialogCaption : String;
Begin
  Result := 'Font Details';
End;

Function TWPFontDialog.GetFontsAllowed: TFslStringList;
Begin
  Result := GetFrame.FontsAllowed;
End;

Procedure TWPFontDialog.SetFontsAllowed(Const Value: TFslStringList);
Begin
  GetFrame.FontsAllowed := Value;
End;




Function TWPFontFrame.DesiredHeight: Integer;
Begin
  Result := 368;
End;


Function TWPFontFrame.DesiredWidth: Integer;
Begin
  Result := 400;
End;

Procedure TWPFontFrame.Initialise;
Begin
  Inherited;
  FFont := TWPSFontDetails.Create;

  FFontSizeMinimum := -1;
  FFontSizeMaximum := -1;

  BuildForm;
End;

Procedure TWPFontFrame.Finalise;
Begin
  FFontsAllowed.Free;
  FFont.Free;
  Inherited;
End;

Procedure TWPFontFrame.BuildNameSize;
Begin
  FgrpFont := TGroupBox.Create(Self);
  FgrpFont.Parent := ClientPanel;
  FgrpFont.align := alTop;
  FgrpFont.Caption := 'Font';

  AddLabel(FgrpFont, 15, 10, 'Name');

  FedtFont := TEdit.Create(Self);
  FedtFont.Parent := FgrpFont;
  FedtFont.Top := 30;
  FedtFont.Left := 10;
  FedtFont.Width := 250;
  FedtFont.OnChange := edtFontChange;

  FlstFont := TListBox.Create(Self);
  FlstFont.Parent := FgrpFont;
  FlstFont.Top := 50;
  FlstFont.Left := 30;
  FlstFont.Width := 230;
  FlstFont.Height := 80;
  FlstFont.OnClick := lstFontClick;

  AddLabel(FgrpFont, 15, 280, 'Size');

  FSpnSize := TSpinEdit.Create(Self);
  FSpnSize.Parent := FgrpFont;
  FSpnSize.Top := 30;
  FSpnSize.Left := 280;
  FSpnSize.Width := 80;
  FSpnSize.OnChange := SpnSizeChange;

  FlstSize := TListBox.Create(Self);
  FlstSize.Parent := FgrpFont;
  FlstSize.Top := 51;
  FlstSize.Left := 300;
  FlstSize.Width := 60;
  FlstSize.Height := 79;
  FlstSize.OnClick := lstSizeClick;

  FgrpFont.Height := FlstFont.Top + FlstFont.Height + 15;
End;

Procedure TWPFontFrame.BuildOptions;
Begin
  FgrpOptions := TGroupBox.Create(Self);
  FgrpOptions.Parent := ClientPanel;
  FgrpOptions.align := alTop;
  FgrpOptions.Caption := 'Options';

  AddLabel(FgrpOptions, 20, 10, 'Position: ');

  FcbState := TComboBox.Create(Self);
  FcbState.Parent := FgrpOptions;
  FcbState.Style := csDropDownList;
  FcbState.Left := 70;
  FcbState.Top := 18;
  FcbState.Width := 112;
  FcbState.OnChange := cbStateChange;

  AddLabel(FgrpOptions, 42, 10, 'Capitals: ');

  FcbCaps := TComboBox.Create(Self);
  FcbCaps.Parent := FgrpOptions;
  FcbCaps.Style := csDropDownList;
  FcbCaps.Left := 70;
  FcbCaps.Top := 40;
  FcbCaps.Width := 112;
  FcbCaps.OnChange := cbCapChange;

  FchkBold := TCheckBox.Create(Self);
  FchkBold.Parent := FgrpOptions;
  FchkBold.AllowGrayed := True;
  FchkBold.Left := 197;
  FchkBold.Top := 20;
  FchkBold.Caption := 'Bold';
  FchkBold.Width := 20 + Canvas.TextWidth(FchkBold.Caption);
  FchkBold.OnClick := chkBoldClick;

  FchkItalic := TCheckBox.Create(Self);
  FchkItalic.Parent := FgrpOptions;
  FchkItalic.AllowGrayed := True;
  FchkItalic.Left := 197;
  FchkItalic.Top := 40;
  FchkItalic.Caption := 'Italics';
  FchkItalic.Width := 20 + Canvas.TextWidth(FchkItalic.Caption);
  FchkItalic.OnClick := chkItalicClick;

  FchkUnderline := TCheckBox.Create(Self);
  FchkUnderline.Parent := FgrpOptions;
  FchkUnderline.AllowGrayed := True;
  FchkUnderline.Left := 275;
  FchkUnderline.Top := 20;
  FchkUnderline.Caption := 'Underline';
  FchkUnderline.Width := 20 + Canvas.TextWidth(FchkUnderline.Caption);
  FchkUnderline.OnClick := chkUnderlineClick;

  FchkStrikeout := TCheckBox.Create(Self);
  FchkStrikeout.Parent := FgrpOptions;
  FchkStrikeout.AllowGrayed := True;
  FchkStrikeout.Left := 275;
  FchkStrikeout.Top := 40;
  FchkStrikeout.Caption := 'Strike Through';
  FchkStrikeout.Width := 20 + Canvas.TextWidth(FchkStrikeout.Caption);
  FchkStrikeout.OnClick := chkStrikeoutClick;

  FgrpOptions.Height := 70;
End;

Procedure TWPFontFrame.BuildColour;
Begin
  FgrpColour := TGroupBox.Create(Self);
  FgrpColour.Parent := ClientPanel;
  FgrpColour.align := alTop;
  FgrpColour.Caption := 'Colour';

  AddLabel(FgrpColour, 20, 10, 'Foreground');

  FcbxFore := TUixHTMLColourComboBox.Create(Self);
  FcbxFore.Parent := FgrpColour;
  FcbxFore.Left := 84;
  FcbxFore.Top := 18;
  FcbxFore.Width := 98;
  FcbxFore.OnChange := cbxForeChange;

  AddLabel(FgrpColour, 20, 194, 'Background');

  FcbxBack := TUixHTMLColourComboBox.Create(Self);
  FcbxBack.Parent := FgrpColour;
  FcbxBack.Left := 276;
  FcbxBack.Top := 18;
  FcbxBack.Width := 98;
  FcbxBack.OnChange := cbxBackChange;

  FgrpColour.Height := FcbxBack.Top + FcbxBack.Height + 10;
End;

Procedure TWPFontFrame.BuildSample;
Begin
  FgrpSample := TGroupBox.Create(Self);
  FgrpSample.Parent := ClientPanel;
  FgrpSample.align := alTop;
  FgrpSample.Caption := 'Sample';

  FpnlSample := TPanel.Create(Self);
  FpnlSample.Parent := FgrpSample;
  FpnlSample.Top := 20;
  FpnlSample.Left := 15;
  FpnlSample.Height := 50;
  FpnlSample.Width := FgrpSample.Width - 30;
  FpnlSample.Anchors := [akLeft, akTop, akRight];
  FpnlSample.BorderStyle := bsSingle;
  FpnlSample.Color := clWhite;

  FpntSample := TPaintBox.Create(Self);
  FpntSample.Parent := FpnlSample;
  FpntSample.Align := alClient;
  FpntSample.OnPaint := pntSamplePaint;

  FgrpSample.Height := FpnlSample.Top + FpnlSample.Height + 15;
End;

Procedure TWPFontFrame.BuildForm;
Begin
  BuildNameSize;
  BuildOptions;
  BuildColour;
  BuildSample;
End;

Procedure TWPFontFrame.Okay;
Begin
End;

Procedure TWPFontFrame.SetFont(Const Value: TWPSFontDetails);
Begin
  FFont.Assign(Value);
  BindToFont;
End;

Procedure TWPFontFrame.ListSizes;
Var
  bHasFontSizeConstraint : Boolean;

  iIndex : Integer;
  iFontSize : Integer;
Begin
  FlstSize.Items.Clear;

  bHasFontSizeConstraint := HasFontSizeConstraint;

  For iIndex := Low(FONTSIZES) To High(FONTSIZES) Do
  Begin
    iFontSize := FONTSIZES[iIndex];

    If bHasFontSizeConstraint Then
    Begin
      If (iFontSize >= FFontSizeMinimum) And (iFontSize <= FFontSizeMaximum) Then
        FlstSize.Items.Add(IntToStr(iFontSize));
    End
    Else
    Begin
      FlstSize.Items.Add(IntToStr(iFontSize));
    End;
  End;
End;


Procedure TWPFontFrame.LoadFontNames;
Var
  oList : TStringList;
  iLoop : Integer;
Begin
  FlstFont.items.Clear;
  oList := TStringList.Create;
  Try
    EnumFonts(Canvas.Handle,  Nil, @EnumFontsProc, pointer(oList));
    oList.Sort;
    For iLoop := 0 To oList.Count - 1 Do
      If (FFontsAllowed = Nil) Or (FFontsAllowed.Count = 0) Or (FFontsAllowed.ExistsByValue(oList[iLoop])) Then
        FlstFont.items.Add(oList[iLoop]);
  Finally
    oList.Free;
  End;
End;

Procedure TWPFontFrame.Restore;
Begin
  Inherited;

  If HasFontSizeConstraint Then
  Begin
    FSpnSize.MinValue := FFontSizeMinimum;
    FSpnSize.MaxValue := FFontSizeMaximum;
  End
  Else
  Begin
    FSpnSize.MinValue := 4;
    FSpnSize.MaxValue := 256;
  End;


  ListSizes;
  LoadFontNames;

  BindToFont;
  If ConsoleMode Then
  Begin
    FedtFont.Enabled := False;
    FlstFont.Enabled := False;
    FSpnSize.Enabled := False;
    FlstSize.Enabled := False;
    FchkStrikeout.Enabled := False;
    FcbState.Enabled := False;
    FcbCaps.Enabled := False;
    FcbxFore.Enabled := False;
    FcbxBack.Enabled := False;
  End;
End;

Procedure WriteCheckbox(oCheck : TCheckBox; aValue : TWPSTriState);
Begin
  If aValue = tsUnknown Then
    oCheck.State := cbGrayed
  Else
    oCheck.Checked := aValue = tsTrue;
End;

Procedure TWPFontFrame.BindToFont;
Var
  oTempState : TWPSFontState;
  oTempCap : TWPSCapsState;
Begin
  FUpdating := True;
  Try
    FedtFont.Text := FFont.Name;
    FlstFont.ItemIndex := FlstFont.Items.indexOf(FFont.Name);
    If FFont.Size <> DEF_WORD Then
      Begin
      FSpnSize.Value := FFont.Size;
      FlstSize.ItemIndex := FlstSize.Items.indexOf(IntToStr(FFont.Size));
      End
    Else
      Begin
      FSpnSize.Text := '';
      FlstSize.ItemIndex := -1;
      End;
    WriteCheckbox(FchkBold, FFont.Bold);
    WriteCheckbox(FchkItalic, FFont.Italic);
    WriteCheckbox(FchkUnderline, FFont.Underline);
    WriteCheckbox(FchkStrikeout, FFont.Strikethrough);

    FcbState.Items.Clear;
    For oTempState := Low(TWPSFontState) To High(TWPSFontState) Do
      FcbState.Items.Add(NAMES_WPSFONTSTATE[oTempState]);
    FcbState.ItemIndex := Ord(FFont.State);

    FcbCaps.Items.Clear;
    For oTempCap := Low(TWPSCapsState) To High(TWPSCapsState) Do
      FcbCaps.Items.Add(NAMES_WPSCAPSSTATE[oTempCap]);
    FcbCaps.ItemIndex := Ord(FFont.Capitalization);

    If FFont.Foreground <> DEF_COLOUR Then
      FcbxFore.Value := FFont.Foreground
    Else
      FcbxFore.ItemIndex := -1;

    If FFont.Background <> DEF_COLOUR Then
      FcbxBack.Value := FFont.Background
    Else
      FcbxBack.ItemIndex := -1;
  Finally
    FUpdating := False;
  End;
End;

Procedure TWPFontFrame.edtFontChange;
Var
  iLoop : Integer;
  bDone : Boolean;
Begin
  iLoop := 0;
  bDone := False;
  FFont.Name := '';
  While Not bDone And (iLoop < FlstFont.Items.Count) Do
  Begin
    If StringEquals(FedtFont.Text, Copy(FlstFont.Items[iLoop], 1, Length(FedtFont.Text))) Then
    Begin
      bDone := True;
      FlstFont.ItemIndex := iLoop;

      If StringEquals(FedtFont.Text, FlstFont.Items[iLoop]) Then
        FFont.Name := FedtFont.Text;
    End;

    Inc(iLoop);
  End;
  RefreshSample;
End;

Procedure TWPFontFrame.lstFontClick;
Begin
  FedtFont.Text := FlstFont.Items[FlstFont.ItemIndex];
  FFont.Name := FedtFont.Text;
  RefreshSample;
End;

Procedure TWPFontFrame.lstSizeClick;
Begin
  FSpnSize.Value := StrToIntDef(FlstSize.Items[FlstSize.ItemIndex], 10);
  FFont.Size := FSpnSize.Value;
  RefreshSample;
End;

Procedure TWPFontFrame.SpnSizeChange;
Begin
  If FSpnSize.Text = '' Then
    Begin
    FlstSize.ItemIndex := -1;
    FFont.Size := DEF_WORD
    End
  Else
    Begin
    FlstSize.ItemIndex := FlstSize.Items.IndexOf(IntegerToString(FSpnSize.Value));
    FFont.Size := FSpnSize.Value;
    End;
  RefreshSample;
End;

Procedure TWPFontFrame.cbxForeChange(oSender: TObject);
Begin
  If FcbxFore.ItemIndex = -1 Then
    FFont.Foreground := DEF_COLOUR
  Else
    FFont.Foreground := FcbxFore.Value;
  RefreshSample;
End;

Procedure TWPFontFrame.cbxBackChange(oSender: TObject);
Begin
  If FcbxBack.ItemIndex = -1 Then
    FFont.Background := DEF_COLOUR
  Else
    FFont.Background := FcbxBack.Value;

  RefreshSample;
End;

Function ReadCheckbox(oCheck : TCheckBox) : TWPSTriState;
Begin
  If oCheck.State = cbGrayed Then
    Result := tsUnknown
  Else If oCheck.Checked Then
    Result := tsTrue
  Else
    Result := tsFalse;
End;


Procedure TWPFontFrame.chkBoldClick(oSender: TObject);
Begin
  FFont.Bold := ReadCheckBox(FchkBold);
  RefreshSample;
End;

Procedure TWPFontFrame.chkItalicClick(oSender: TObject);
Begin
  FFont.Italic := ReadCheckBox(FchkItalic);
  RefreshSample;
End;

Procedure TWPFontFrame.chkUnderlineClick(oSender: TObject);
Begin
  FFont.Underline := ReadCheckBox(FchkUnderline);
  RefreshSample;
End;

Procedure TWPFontFrame.chkStrikeoutClick(oSender: TObject);
Begin
  FFont.Strikethrough := ReadCheckBox(FchkStrikeout);
  RefreshSample;
End;

Procedure TWPFontFrame.cbCapChange(oSender: TObject);
Begin
  If Not FUpdating Then
  Begin
    FUpdating := True;
    Try
      FFont.Capitalization := TWPSCapsState(FcbCaps.ItemIndex);
      RefreshSample;
    Finally
      FUpdating := False;
    End;
  End;
End;

Procedure TWPFontFrame.cbStateChange(oSender: TObject);
Begin
  If Not FUpdating Then
  Begin
    FUpdating := True;
    Try
      FFont.State := TWPSFontState(FcbState.ItemIndex);
      RefreshSample;
    Finally
      FUpdating := False;
    End;
  End;
End;

Procedure TWPFontFrame.RefreshSample;
Begin
  FpnlSample.Invalidate;
End;

Procedure TWPFontFrame.pntSamplePaint(oSender: TObject);
Var
  aRect : TSize;
  aStyle : TFontStyles;
  sText : String;
  iOffset : Integer;
Begin
  If FFont.Name <> '' Then
    FpntSample.Canvas.Font.Name := FFont.Name
  Else
    FpntSample.Canvas.Font.Name := DEFAULT_FONT_NAME;

  If FFont.Size <> DEF_WORD Then
    FpntSample.Canvas.Font.Size := FFont.Size
  Else
    FpntSample.Canvas.Font.Size := DEFAULT_FONT_SIZE;

  aStyle := [];

  If FFont.Bold = tsTrue Then
    include(aStyle, fsBold);
  If FFont.Italic = tsTrue Then
    include(aStyle, fsItalic);
  If FFont.Underline = tsTrue Then
    include(aStyle, fsUnderline);
  If FFont.Strikethrough = tsTrue Then
    include(aStyle, fsStrikeOut);
  FpntSample.Canvas.Font.Style := aStyle;

  sText := FpntSample.Canvas.Font.Name+' '+IntegerToString(FpntSample.Canvas.Font.Size);
  aRect := FpntSample.Canvas.TextExtent(sText);

  iOffset := 0;
  If FFont.State = fsSuperscript Then
    Begin
    iOffset := -(FpntSample.Canvas.Font.Size Div 2);
    FpntSample.Canvas.Font.Size := FpntSample.Canvas.Font.Size - FpntSample.Canvas.Font.Size Div 3;
    End
  Else If FFont.State = fsSubscript Then
    Begin
    iOffset := +(FpntSample.Canvas.Font.Size Div 2);
    FpntSample.Canvas.Font.Size := FpntSample.Canvas.Font.Size - FpntSample.Canvas.Font.Size Div 3;
    End;

  If FFont.Foreground <> DEF_COLOUR Then
    FpntSample.Canvas.Font.Color := FFont.Foreground
  Else
    FpntSample.Canvas.Font.Color := DEFAULT_FOREGROUND;

  If FFont.Background <> DEF_COLOUR Then
    FpntSample.Canvas.Brush.Color := FFont.Background
  Else
    FpntSample.Canvas.Brush.Color := clWhite;

  If (FFont.Capitalization = fcsAllCaps) Or (FFont.Capitalization = fcsSmallCaps) Then
    sText := StringUpper(sText)
  Else If FFont.Capitalization = fcsNoCaps Then
    sText := Lowercase(sText);

  FpntSample.Canvas.TextOut(FpntSample.Width Div 2 - aRect.cx Div 2, iOffset + FpntSample.Height Div 2 - aRect.cy Div 2, sText);
End;


Function TWPFontFrame.HasFontSizeConstraint: Boolean;
Begin
  Result := (FFontSizeMinimum <> -1) And (FFontSizeMaximum <> -1);
End;


Procedure TWPFontFrame.SetFontsAllowed(Const Value: TFslStringList);
Begin
  FFontsAllowed.Free;
  FFontsAllowed := Value;
End;



Function TWPFontDialog.GetConsoleMode: Boolean;
Begin
  Result := Frame.ConsoleMode;
End;

Procedure TWPFontDialog.SetConsoleMode(Const Value: Boolean);
Begin
  Frame.ConsoleMode := Value;
End;

{ TWPPasteSpecialDialog }

Function TWPPasteSpecialDialog.GetFrame : TWPPasteSpecialFrame;
Begin
  Result := TWPPasteSpecialFrame(Inherited Frame);
End;


Function TWPPasteSpecialDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPPasteSpecialFrame;
End;


Function TWPPasteSpecialDialog.DialogCaption : String;
Begin
  Result := 'Paste Special';
End;


Function TWPPasteSpecialDialog.GetFormat: TWPClipboardContentType;
Begin
  Result := Frame.Format;
End;

Procedure TWPPasteSpecialDialog.SetFormat(Const Value: TWPClipboardContentType);
Begin
  Frame.Format := Value;
End;



{ TWPPasteSpecialFrame }

Function TWPPasteSpecialFrame.DesiredHeight: Integer;
Begin
  Result := 200;
End;

Function TWPPasteSpecialFrame.DesiredWidth: Integer;
Begin
  Result := 300;
End;

Procedure TWPPasteSpecialFrame.Initialise;
Begin
  Inherited;

  BuildForm;
  LoadClipboardFormats;
  Format := wcctUnknown;
End;

Procedure TWPPasteSpecialFrame.Finalise;
Begin

  Inherited;
End;

Procedure TWPPasteSpecialFrame.BuildForm;
Begin
  lblDesc := TLabel.Create(ClientPanel);
  lblDesc.Parent := ClientPanel;
  lblDesc.Top := 10;
  lblDesc.Left := 10;
  lblDesc.Caption := 'Select Format to Paste';

  lbFormats := TListBox.Create(ClientPanel);
  lbFormats.Parent := ClientPanel;
  lbFormats.Top := 30;
  lbFormats.Left := 10;
  lbFormats.Width := 266;
  lbFormats.Height := 140;
  lbFormats.OnDblClick := lbFormatsDblClick;
End;

Procedure TWPPasteSpecialFrame.LoadClipboardFormats;
Var
  oClip : TWPClipboard;
  aStyle : TWPClipboardContentType;
  aStyles : TWPClipboardContentTypes;
Begin
  lbFormats.Items.Clear;
  oClip := TWPClipboard.Create;
  Try
    oClip.Open;
    aStyles := oClip.ContentTypes;
    For aStyle := Succ(Low(TWPClipboardContentType)) To High(TWPClipboardContentType) Do
      If aStyle In aStyles Then
        lbFormats.Items.AddObject(WPCLIPBOARDCONTENTTYPE_NAMES[aStyle], TObject(aStyle));
  Finally
    oClip.Free;
  End;
End;

Procedure TWPPasteSpecialFrame.Restore;
Begin
  Inherited;
  // nothing
End;

Function TWPPasteSpecialFrame.GetFormat: TWPClipboardContentType;
Begin
  If lbFormats.ItemIndex = -1 Then
    Result := wcctUnknown
  Else
    Result := TWPClipboardContentType(lbFormats.Items.Objects[lbFormats.ItemIndex]);
End;

Procedure TWPPasteSpecialFrame.SetFormat(aValue: TWPClipboardContentType);
Var
  iLoop : Integer;
Begin
  lbFormats.ItemIndex := -1;
  For iLoop := 0 To lbFormats.Items.Count - 1 Do
    If TWPClipboardContentType(lbFormats.Items.Objects[iLoop]) = aValue Then
      lbFormats.ItemIndex := iLoop;
  OKButton.Enabled := lbFormats.ItemIndex <> -1;
End;

Procedure TWPPasteSpecialFrame.lbFormatsDblClick(oSender: TObject);
Begin
  Okay;
End;

Procedure TWPPasteSpecialFrame.Refresh;
Begin
  Inherited;
  OKButton.Enabled := lbFormats.ItemIndex <> -1;
End;


Function TWPParagraphDialog.GetFrame : TWPParagraphFrame;
Begin
  Result := TWPParagraphFrame(Inherited Frame);
End;


Function TWPParagraphDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPParagraphFrame;
End;


Function TWPParagraphDialog.DialogCaption : String;
Begin
  Result := 'Paragraph Details';
End;


Function TWPParagraphDialog.GetParagraph: TWPSParagraphDetails;
Begin
  Result := Frame.Paragraph;
End;


Procedure TWPParagraphDialog.SetParagraph(Const Value: TWPSParagraphDetails);
Begin
  Frame.Paragraph := Value;
End;


Function TWPParagraphFrame.DesiredHeight: Integer;
Begin
  Result := 406;
End;

Function TWPParagraphFrame.DesiredWidth: Integer;
Begin
  Result := 340;
End;


Procedure TWPParagraphFrame.Initialise;
Var
  oBitmap : TBitmap;
Begin
  Inherited;

  FParagraph := TWPSParagraphDetails.Create;

  // Align.
  FgrpAlign := TUixGroupBox.Create(Self);
  FgrpAlign.Parent := ClientPanel;
  FgrpAlign.AlignTop;
  FgrpAlign.Caption := ' Alignment ';

  FgrpAlign.Height := 100;

  FbtnLeft := TUixAdvancedButton.Create(Self);
  FbtnLeft.Parent := FgrpAlign;
  FbtnLeft.Left := 30;
  FbtnLeft.Top := 30;
  FbtnLeft.Height := 30;
  FbtnLeft.Width := 30;
  FbtnLeft.OnClick := btnLeftChange;
  FbtnLeft.ShapeStyleSquare;
  FbtnLeft.PresentationEntity.HasIconBitmapImage := True;
  oBitmap := TBitmap.Create;
  WPIconModule.Images.GetBitmap(WPIconModule.ALIGN_LEFT, oBitmap);
  FbtnLeft.PresentationEntity.IconBitmapImage.LoadFromVCLBitmap(oBitmap);
  FbtnLeft.PresentationEntity.IconBitmapImage.TransparentColour := argbWhite;
  oBitmap.Free;
  AddLabel(FgrpAlign, 65, 33, 'Left');

  FbtnCenter := TUixAdvancedButton.Create(Self);
  FbtnCenter.Parent := FgrpAlign;
  FbtnCenter.Left := 110;
  FbtnCenter.Top := 30;
  FbtnCenter.Height := 30;
  FbtnCenter.Width := 30;
  FbtnCenter.OnClick := btnCenterChange;
  FbtnCenter.ShapeStyleSquare;
  FbtnCenter.PresentationEntity.HasIconBitmapImage := True;
  oBitmap := TBitmap.Create;
  WPIconModule.Images.GetBitmap(WPIconModule.ALIGN_CENTRE, oBitmap);
  FbtnCenter.PresentationEntity.IconBitmapImage.LoadFromVCLBitmap(oBitmap);
  FbtnCenter.PresentationEntity.IconBitmapImage.TransparentColour := argbWhite;
  oBitmap.Free;
  AddLabel(FgrpAlign, 65, 105, 'Centre');

  FbtnRight := TUixAdvancedButton.Create(Self);
  FbtnRight.Parent := FgrpAlign;
  FbtnRight.Left := 190;
  FbtnRight.Top := 30;
  FbtnRight.Height := 30;
  FbtnRight.Width := 30;
  FbtnRight.OnClick := btnRightChange;
  FbtnRight.ShapeStyleSquare;
  FbtnRight.PresentationEntity.HasIconBitmapImage := True;
  oBitmap := TBitmap.Create;
  WPIconModule.Images.GetBitmap(WPIconModule.ALIGN_RIGHT, oBitmap);
  FbtnRight.PresentationEntity.IconBitmapImage.LoadFromVCLBitmap(oBitmap);
  FbtnRight.PresentationEntity.IconBitmapImage.TransparentColour := argbWhite;
  oBitmap.Free;
  AddLabel(FgrpAlign, 65, 189, 'Right');

  FbtnJustify := TUixAdvancedButton.Create(Self);
  FbtnJustify.Parent := FgrpAlign;
  FbtnJustify.Left := 270;
  FbtnJustify.Top := 30;
  FbtnJustify.Height := 30;
  FbtnJustify.Width := 30;
  FbtnJustify.OnClick := btnJustifyChange;
  FbtnJustify.ShapeStyleSquare;
  FbtnJustify.PresentationEntity.HasIconBitmapImage := True;
  oBitmap := TBitmap.Create;
  WPIconModule.Images.GetBitmap(WPIconModule.ALIGN_JUSTIFY, oBitmap);
  FbtnJustify.PresentationEntity.IconBitmapImage.LoadFromVCLBitmap(oBitmap);
  FbtnJustify.PresentationEntity.IconBitmapImage.TransparentColour := argbWhite;
  oBitmap.Free;
  AddLabel(FgrpAlign, 65, 267, 'Justify');


  // Margins.
  FgrpMargin := TUixGroupBox.Create(Self);
  FgrpMargin.Parent := ClientPanel;
  FgrpMargin.AlignTop;
  FgrpMargin.Caption := ' Margins ';
  FgrpMargin.Height := 110;

  FspnLeftIndent := TSpinEdit.Create(Self);
  FspnLeftIndent.Parent := FgrpMargin;
  FspnLeftIndent.Top := 21;
  FspnLeftIndent.Left := 188;
  FspnLeftIndent.Width := 80;
  FspnLeftIndent.MinValue := 0;
  FspnLeftIndent.MaxValue := 10;
  FspnLeftIndent.OnChange := spnLeftIndentChange;
  AddLabel(FgrpMargin, 25, 20, 'Distance from Left Margin');

  FspnRightIndent := TSpinEdit.Create(Self);
  FspnRightIndent.Parent := FgrpMargin;
  FspnRightIndent.Top := 48;
  FspnRightIndent.Left := 188;
  FspnRightIndent.Width := 80;
  FspnRightIndent.MinValue := 0;
  FspnRightIndent.MaxValue := 10;
  FspnRightIndent.OnChange := spnRightIndentChange;
  AddLabel(FgrpMargin, 52, 20, 'Distance from Right Margin');

  FspnBottomMargin := TSpinEdit.Create(Self);
  FspnBottomMargin.Parent := FgrpMargin;
  FspnBottomMargin.Top := 75;
  FspnBottomMargin.Left := 188;
  FspnBottomMargin.Width := 80;
  FspnBottomMargin.Increment := 6;
  FspnBottomMargin.MinValue := 0;
  FspnBottomMargin.MaxValue := 48;
  FspnBottomMargin.OnChange := spnBottomMarginChange;
  AddLabel(FgrpMargin, 79, 20, 'Bottom Margin');

  // List.
  FgrpList := TUixGroupBox.Create(Self);
  FgrpList.Parent := ClientPanel;
  FgrpList.AlignTop;
  FgrpList.Caption := ' List ';
  FgrpList.Height := 180;

  FcbxList := TUixComboBox.Create(Self);
  FcbxList.Parent := FgrpList;
  FcbxList.Top := 24;
  FcbxList.Left := 115;
  FcbxList.Width := 190;
  FcbxList.Style := csDropDownList;
  FcbxList.AddValues(TITLES_WPSPARAGRAPHLISTTYPE);
  FcbxList.OnClick := FcbxListChange;
  AddLabel(FgrpList, 28, 20, 'List Format');

  FcbxBullet := TUixComboBox.Create(Self);
  FcbxBullet.Parent := FgrpList;
  FcbxBullet.Top := 54;
  FcbxBullet.Left := 115;
  FcbxBullet.Width := 190;
  FcbxBullet.Style := csDropDownList;
  FcbxBullet.AddValues(TITLES_WPSPARAGRAPHBULLETTYPE);
  FcbxBullet.OnClick := cbxBulletChange;
  AddLabel(FgrpList, 58, 20, 'Bullet Type');

  FcbxNumber := TUixComboBox.Create(Self);
  FcbxNumber.Parent := FgrpList;
  FcbxNumber.Top := 84;
  FcbxNumber.Left := 115;
  FcbxNumber.Width := 190;
  FcbxNumber.Style := csDropDownList;
  FcbxNumber.AddValues(TITLES_WPSPARAGRAPHNUMBERTYPE);
  FcbxNumber.OnClick := cbxNumberChange;
  AddLabel(FgrpList, 88, 20, 'Number Type');

  FcbxNumberFormat := TUixComboBox.Create(Self);
  FcbxNumberFormat.Parent := FgrpList;
  FcbxNumberFormat.Top := 114;
  FcbxNumberFormat.Left := 115;
  FcbxNumberFormat.Width := 190;
  FcbxNumberFormat.Style := csDropDownList;
  FcbxNumberFormat.AddValues(TITLES_WPSPARAGRAPHNUMBERFORMAT);
  FcbxNumberFormat.OnClick := cbxNumberFormatChange;
  AddLabel(FgrpList, 118, 20, 'Number Format');

  FchkNumber := TUixCheckBox.Create(Self);
  FchkNumber.Parent := FgrpList;
  FchkNumber.Left := 40;
  FchkNumber.Top := 146;
  FchkNumber.Caption := 'Reset Number to';
  FchkNumber.OnClick := chkNumberChange;
  FchkNumber.Width := Canvas.TextWidth(FchkNumber.Caption) + 20;

  FspnNumber := TSpinEdit.Create(Self);
  FspnNumber.Parent := FgrpList;
  FspnNumber.Top := 144;
  FspnNumber.Left := FchkNumber.Left + FchkNumber.Width + 5;
  FspnNumber.Width := 80;
  FspnNumber.MinValue := 0;
  FspnNumber.MaxValue := 1000;
  FspnNumber.OnChange := spnNumberChange;
End;


Procedure TWPParagraphFrame.Finalise;
Begin
  FParagraph.Free;

  Inherited;
End;


Procedure TWPParagraphFrame.Okay;
Begin
End;


Function TWPParagraphFrame.GetParagraph : TWPSParagraphDetails;
Begin
  Assert(Invariants('GetParagraph', FParagraph, TWPSParagraphDetails, 'Paragraph'));
  Result := FParagraph;
End;


Procedure TWPParagraphFrame.SetParagraph(Const Value: TWPSParagraphDetails);
Begin
  FParagraph.Assign(Value);
  BindToParagraph;
End;


Procedure TWPParagraphFrame.Restore;
Begin
  Inherited;

  BindToParagraph;
End;


Procedure TWPParagraphFrame.BindToParagraph;
Var
  iNum : Integer;
Begin
  UpdateAlignment;

  // Update Paragraph.
  If FParagraph.LeftIndent <> DEF_WORD Then
    FspnLeftIndent.Value := FParagraph.LeftIndent
  Else
    FspnLeftIndent.Text := '0';

  If FParagraph.RightIndent <> DEF_WORD Then
    FspnRightIndent.Value := FParagraph.RightIndent
  Else
    FspnRightIndent.Text := '0';

  If FParagraph.MarginBottom <> DEF_WORD Then
    FspnBottomMargin.Value := FParagraph.MarginBottom
  Else
    FspnBottomMargin.Text := '0';

  // Update List.
  FcbxList.ItemIndex := ord(FParagraph.ListType);
  FcbxListChange(Self);

  FcbxBullet.ItemIndex := ord(FParagraph.BulletType);
  FcbxNumber.ItemIndex := ord(FParagraph.NumberType);
  FcbxNumberFormat.ItemIndex := ord(FParagraph.NumberFormat);

  iNum := FParagraph.FixedNumber;
  FchkNumber.Checked := iNum <> DEF_WORD;
  chkNumberChange(Self);
  If FchkNumber.Checked Then
    FspnNumber.Value := iNum
  Else
    FspnNumber.Text := '0';
End;


Procedure TWPParagraphFrame.UpdateAlignment;
Begin
  FbtnLeft.Checked := False;
  FbtnCenter.Checked := False;
  FbtnRight.Checked := False;
  FbtnJustify.Checked := False;
  Case FParagraph.Align Of
    WordProcessorParagraphAlignmentLeft : FbtnLeft.Checked := True;
    WordProcessorParagraphAlignmentCentre : FbtnCenter.Checked := True;
    WordProcessorParagraphAlignmentRight : FbtnRight.Checked := True;
    WordProcessorParagraphAlignmentJustify : FbtnJustify.Checked := True;
  Else
   // taUnknown,
  End;

  FbtnLeft.Refresh;
  FbtnCenter.Refresh;
  FbtnRight.Refresh;
  FbtnJustify.Refresh;
End;


Procedure TWPParagraphFrame.btnLeftChange(oSender : TObject);
Begin
  If FParagraph.Align = WordProcessorParagraphAlignmentLeft Then
    FParagraph.Align := WordProcessorParagraphAlignmentUnknown
  Else
    FParagraph.Align := WordProcessorParagraphAlignmentLeft;

  UpdateAlignment;
End;


Procedure TWPParagraphFrame.btnCenterChange(oSender : TObject);
Begin
  If FParagraph.Align = WordProcessorParagraphAlignmentCentre Then
    FParagraph.Align := WordProcessorParagraphAlignmentUnknown
  Else
    FParagraph.Align := WordProcessorParagraphAlignmentCentre;

  UpdateAlignment;
End;


Procedure TWPParagraphFrame.btnRightChange(oSender : TObject);
Begin
  If FParagraph.Align = WordProcessorParagraphAlignmentRight Then
    FParagraph.Align := WordProcessorParagraphAlignmentUnknown
  Else
    FParagraph.Align := WordProcessorParagraphAlignmentRight;

  UpdateAlignment;
End;


Procedure TWPParagraphFrame.btnJustifyChange(oSender : TObject);
Begin
  If FParagraph.Align = WordProcessorParagraphAlignmentJustify Then
    FParagraph.Align := WordProcessorParagraphAlignmentUnknown
  Else
    FParagraph.Align := WordProcessorParagraphAlignmentJustify;

  UpdateAlignment;
End;


Procedure TWPParagraphFrame.spnLeftIndentChange(oSender : TObject);
Begin
  If FspnLeftIndent.Text = '' Then
    FParagraph.LeftIndent := DEF_WORD
  Else
    FParagraph.LeftIndent := FspnLeftIndent.Value;
End;

Procedure TWPParagraphFrame.spnRightIndentChange(oSender : TObject);
Begin
  If FspnRightIndent.Text = '' Then
    FParagraph.RightIndent := DEF_WORD
  Else
    FParagraph.RightIndent := FspnRightIndent.Value;
End;


Procedure TWPParagraphFrame.spnBottomMarginChange(oSender : TObject);
Begin
  If FspnBottomMargin.Text = '' Then
    FParagraph.MarginBottom := DEF_WORD
  Else
    FParagraph.MarginBottom := FspnBottomMargin.Value;
End;


Procedure TWPParagraphFrame.FcbxListChange(oSender: TObject);
Begin
  FParagraph.ListType := TWPSParagraphListType(FcbxList.ItemIndex);

  FcbxBullet.Enabled := False;
  FcbxNumber.Enabled := False;
  FcbxNumberFormat.Enabled := False;
  FchkNumber.Enabled := False;
  FspnNumber.Enabled := False;

  Case FcbxList.ItemIndex Of
    2:
    Begin
      FcbxBullet.Enabled := True;
    End;

    3:
    Begin
      FcbxNumber.Enabled := True;
      FcbxNumberFormat.Enabled := True;
      FchkNumber.Enabled := True;
      FspnNumber.Enabled := True;
    End;
  End;
End;


Procedure TWPParagraphFrame.cbxBulletChange(oSender : TObject);
Begin
  FParagraph.BulletType := TWPSParagraphBulletType(FcbxBullet.ItemIndex);
End;


Procedure TWPParagraphFrame.cbxNumberChange(oSender : TObject);
Begin
  FParagraph.NumberType := TWPSParagraphNumberType(FcbxNumber.ItemIndex);
End;


Procedure TWPParagraphFrame.cbxNumberFormatChange(oSender : TObject);
Begin
  FParagraph.NumberFormat := TWPSParagraphNumberFormat(FcbxNumberFormat.ItemIndex);
End;


Procedure TWPParagraphFrame.chkNumberChange(oSender : TObject);
Begin
  If FchkNumber.Checked And (FspnNumber.Text <> '') Then
    FParagraph.FixedNumber := FspnNumber.Value
  Else
    FParagraph.FixedNumber := DEF_WORD;
  FspnNumber.Enabled := FchkNumber.Checked;
End;


Procedure TWPParagraphFrame.spnNumberChange(oSender : TObject);
Begin
  If FchkNumber.Checked And (FspnNumber.Text <> '') Then
    FParagraph.FixedNumber := FspnNumber.Value
  Else
    FParagraph.FixedNumber := DEF_WORD;
End;



Function TWPStyleDialog.GetFrame : TWPStyleFrame;
Begin
  Result := TWPStyleFrame(Inherited Frame);
End;


Function TWPStyleDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPStyleFrame;
End;


Function TWPStyleDialog.DialogCaption : String;
Begin
  Result := 'Style Details';
End;


Function TWPStyleDialog.GetStyle: TWPStyle;
Begin
  Result := Frame.Style;
End;


Procedure TWPStyleDialog.SetStyle(Const Value: TWPStyle);
Begin
  Frame.Style := Value;
End;


Function TWPStyleFrame.DesiredHeight: Integer;
Begin
  Result := 406;
End;

Function TWPStyleFrame.DesiredWidth: Integer;
Begin
  Result := 400;
End;


Procedure TWPStyleFrame.Initialise;
Var
  oPanel : TUixPanel;
  oLabel : TUixLabel;
Begin
  Inherited;

  FStyle := TWPStyle.Create;

  FTabs := TUixPageControl.Create(Self);
  FTabs.Parent := ClientPanel;
  FTabs.AlignClient;

  FStyleTab := TUixTabsheet.Create(Self);
  FStyleTab.Name := 'Style';
  FStyleTab.PageControl := FTabs;

  oPanel := TUixPanel.Create(Self);
  oPanel.Parent := FStyleTab;
  oPanel.AlignClient;
  oPanel.ParentColor := False;
  oPanel.Color := clBtnFace;

  oLabel := TUixLabel.Create(Self);
  oLabel.Parent := oPanel;
  oLabel.Left := 10;
  oLabel.Top := 22;
  oLabel.Caption := 'Name:';

  FStyleName := TUixEdit.Create(Self);
  FStyleName.Parent := oPanel;
  FStyleName.Left := 64;
  FStyleName.Top := 20;
  FStyleName.Width := 250;
  FStyleName.OnChange := StyleNameChanged;

  FDescLabel := TUixLabel.Create(Self);
  FDescLabel.Parent := oPanel;
  FDescLabel.Left := 10;
  FDescLabel.Top := 52;
  FDescLabel.Caption := 'Name:';
  FDescLabel.Visible := HasDesc;

  FStyleDesc := TUixEdit.Create(Self);
  FStyleDesc.Parent := oPanel;
  FStyleDesc.Left := 64;
  FStyleDesc.Top := 50;
  FStyleDesc.Width := 250;
  FStyleDesc.OnChange := StyleNameChanged;
  FStyleDesc.Visible := HasDesc;

  FchkHasParagraph := TUixCheckBox.Create(Self);
  FchkHasParagraph.Parent := oPanel;
  FchkHasParagraph.Left := 40;
  FchkHasParagraph.Top := 100;
  FchkHasParagraph.Width := 250;
  FchkHasParagraph.Caption := 'Has Paragraph Aspect';
  FchkHasParagraph.OnChange := HasParagraphChanged;

  FchkEndsWithParagraph := TUixCheckBox.Create(Self);
  FchkEndsWithParagraph.Parent := oPanel;
  FchkEndsWithParagraph.Left := 40;
  FchkEndsWithParagraph.Top := 130;
  FchkEndsWithParagraph.Width := 250;
  FchkEndsWithParagraph.Caption := 'Style Ends With Paragraph';
  FchkEndsWithParagraph.OnChange := EndsWithParagraphChanged;

  FFontTab := TUixTabsheet.Create(Self);
  FFontTab.Name := 'Font';
  FFontTab.PageControl := FTabs;
  FFontFrame := TWPFontFrame.Create(FFontTab);
  FFontFrame.Parent := FFontTab;
  FFontFrame.AlignClient;

End;

Procedure TWPStyleFrame.MakePara;
Begin
  FParaTab := TUixTabsheet.Create(Self);
  FParaTab.Name := 'Paragraph';
  FParaTab.PageControl := FTabs;
  FParaFrame := TWPParagraphFrame.Create(FParaTab);
  FParaFrame.Parent := FParaTab;
  FParaFrame.AlignClient;
  FParaFrame.Restore;
End;

Procedure TWPStyleFrame.ClearPara;
Begin
  FreeAndNil(FParaFrame);
  FreeAndNil(FParaTab);
End;


Procedure TWPStyleFrame.Finalise;
Begin
  FStyle.Free;

  Inherited;
End;


Procedure TWPStyleFrame.Accept;
Begin
  FStyle.Font.Assign(FFontFrame.Font);
  If FParaFrame <> Nil Then
    FStyle.Paragraph.Assign(FParaFrame.Paragraph);
End;


Function TWPStyleFrame.GetStyle : TWPStyle;
Begin
  Assert(Invariants('GetStyle', FStyle, TWPStyle, 'Style'));
  Result := FStyle;
End;


Procedure TWPStyleFrame.SetStyle(Const Value: TWPStyle);
Begin
  FStyle.Assign(Value);
  BindToStyle;
End;


Procedure TWPStyleFrame.Restore;
Begin
  Inherited;

  FFontFrame.Restore;
  BindToStyle;
End;


Procedure TWPStyleFrame.BindToStyle;
Begin
  FStyleName.Text := FStyle.Name;
  FchkHasParagraph.Checked := FStyle.HasParagraphAspect;
  FchkEndsWithParagraph.Checked := FStyle.ResetOnNewParagraph;

  FFontFrame.Font := FStyle.Font;

  If FStyle.HasParagraphAspect Then
  Begin
    If FParaFrame = Nil Then
      MakePara;
    FParaFrame.Paragraph := FStyle.Paragraph;
  End;
End;

Procedure TWPStyleFrame.HasParagraphChanged(Sender : TObject);
Begin
  If FchkHasParagraph.Checked Then
  Begin
    MakePara;
    FParaFrame.Paragraph := FStyle.Paragraph;
  End
  Else
    ClearPara;
End;

Procedure TWPStyleFrame.EndsWithParagraphChanged(Sender : TObject);
Begin
  FStyle.ResetOnNewParagraph := FchkEndsWithParagraph.checked;
End;

Procedure TWPStyleFrame.StyleNameChanged(Sender : TObject);
Begin
  FStyle.Name := FStyleName.Text;
End;

Procedure TWPStyleDialog.Bind;
Begin
  Frame.BindToStyle;
End;

Procedure TWPStyleFrame.SetHasDesc(Const Value: Boolean);
Begin
  FHasDesc := Value;
  FStyleDesc.Visible := HasDesc;
  FDescLabel.Visible := HasDesc;
End;

Function TWPStyleFrame.GetDesc: String;
Begin
  Result := FStyleDesc.Text;
End;

Procedure TWPStyleFrame.SetDesc(Const Value: String);
Begin
  FStyleDesc.Text := Value;
End;

{ TWPFontDialog }

Function TWPChangeCaseDialog.GetChangeCaseType: TWPChangeCaseType;
Begin
  Result := Frame.ChangeCaseType;
End;


Function TWPChangeCaseDialog.GetFrame : TWPChangeCaseFrame;
Begin
  Result := TWPChangeCaseFrame(Inherited Frame);
End;


Function TWPChangeCaseDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPChangeCaseFrame;
End;


Function TWPChangeCaseDialog.DialogCaption : String;
Begin
  Result := 'Change Cases';
End;

{ TWPChangeCaseFrame }

Procedure TWPChangeCaseFrame.BuildForm;
Begin
  FRadioSentenceCase := TUixRadioButton.Create(Self);
  FRadioSentenceCase.Parent := ClientPanel;
  FRadioSentenceCase.Left := 20;
  FRadioSentenceCase.Top := 40;
  FRadioSentenceCase.Caption := 'Sentence case';
  FRadioSentenceCase.Width := 20 + Canvas.TextWidth(FRadioSentenceCase.Caption);
  FRadioSentenceCase.OnClick := chkSentenceCaseClick;

  FRadioLowerCase := TUixRadioButton.Create(Self);
  FRadioLowerCase.Parent := ClientPanel;
  FRadioLowerCase.Left := 20;
  FRadioLowerCase.Top := 60;
  FRadioLowerCase.Caption := 'lower case';
  FRadioLowerCase.Width := 20 + Canvas.TextWidth(FRadioLowerCase.Caption);
  FRadioLowerCase.OnClick := chkLowerCaseClick;

  FRadioUpperCase := TUixRadioButton.Create(Self);
  FRadioUpperCase.Parent := ClientPanel;
  FRadioUpperCase.Left := 20;
  FRadioUpperCase.Top := 80;
  FRadioUpperCase.Caption := 'UPPER CASE';
  FRadioUpperCase.Width := 20 + Canvas.TextWidth(FRadioUpperCase.Caption);
  FRadioUpperCase.OnClick := chkUpperCaseClick;

  FRadioTitleCase := TUixRadioButton.Create(Self);
  FRadioTitleCase.Parent := ClientPanel;
  FRadioTitleCase.Left := 20;
  FRadioTitleCase.Top := 100;
  FRadioTitleCase.Caption := 'Title Case';
  FRadioTitleCase.Width := 20 + Canvas.TextWidth(FRadioTitleCase.Caption);
  FRadioTitleCase.OnClick := chkTitleCaseClick;

  FRadioToggleCase := TUixRadioButton.Create(Self);
  FRadioToggleCase.Parent := ClientPanel;
  FRadioToggleCase.Left := 20;
  FRadioToggleCase.Top := 120;
  FRadioToggleCase.Caption := 'tOGGLE cASE';
  FRadioToggleCase.Width := 20 + Canvas.TextWidth(FRadioToggleCase.Caption);
  FRadioToggleCase.OnClick := chkToggleCaseClick;
End;

Procedure TWPChangeCaseFrame.chkLowerCaseClick(oSender: TObject);
Begin
  FChangeCaseType := cctLowerCase;
End;

Procedure TWPChangeCaseFrame.chkSentenceCaseClick(oSender: TObject);
Begin
  FChangeCaseType := cctSentenceCase;
End;

Procedure TWPChangeCaseFrame.chkTitleCaseClick(oSender: TObject);
Begin
  FChangeCaseType := cctTitleCase;
End;

Procedure TWPChangeCaseFrame.chkToggleCaseClick(oSender: TObject);
Begin
  FChangeCaseType := cctToggleCase;
End;

Procedure TWPChangeCaseFrame.chkUpperCaseClick(oSender: TObject);
Begin
  FChangeCaseType := cctUpperCase;
End;

Function TWPChangeCaseFrame.DesiredHeight: Integer;
Begin
  Result := 150;
End;

Function TWPChangeCaseFrame.DesiredWidth: Integer;
Begin
  Result := 180;
End;

Procedure TWPChangeCaseFrame.Initialise;
Begin
  Inherited;

  BuildForm;
  FRadioToggleCase.Checked := True;
End;

Function TWPImageDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPImageFrame;
End;

Function TWPImageDialog.DialogCaption : String;
Begin
  Result := 'Image Details';
End;

Function TWPImageDialog.GetFrame : TWPImageFrame;
Begin
  Result := TWPImageFrame(Inherited Frame);
End;

Function TWPImageDialog.GetImage: TWPWorkingDocumentImagePiece;
Begin
  Result := Frame.Image;
End;

Procedure TWPImageDialog.SetImage(Const Value: TWPWorkingDocumentImagePiece);
Begin
  Frame.Image := Value;
End;



Function TWPImageFrame.DesiredHeight: Integer;
Begin
  Result := 356;
End;


Function TWPImageFrame.DesiredWidth: Integer;
Begin
  Result := 300;
End;


Procedure TWPImageFrame.Initialise;
Var
  aLoop : TWPImageVerticalAlignment;
Begin
  Inherited;

  PositionMainForm;

  FgrpInfo := TGroupBox.Create(Self);
  FgrpInfo.Parent := ClientPanel;
  FgrpInfo.align := alTop;
  FgrpInfo.Caption := ' Image ';

  AddLabel(FgrpInfo, 24, 15, 'Image');
  FedtInfo := TEdit.Create(Self);
  FedtInfo.Parent := FgrpInfo;
  FedtInfo.Left := 74;
  FedtInfo.Top := 24;
  FedtInfo.Width := 200;
  FedtInfo.BorderStyle := bsNone;
  FedtInfo.ReadOnly := True;
  FedtInfo.Color := clBtnFace;

  AddLabel(FgrpInfo, 51, 74, 'Frame');
  FspnFrame := TSpinEdit.Create(Self);
  FspnFrame.Parent := FgrpInfo;
  FspnFrame.Left := 120;
  FspnFrame.Top := 47;
  FspnFrame.Width := 50;
  FspnFrame.MinValue := 1;
  FspnFrame.MaxValue := 10000;

  FgrpInfo.Height := FspnFrame.Top + FspnFrame.Height+10;

  // Size.
  FgrpSize := TGroupBox.Create(Self);
  FgrpSize.Parent := ClientPanel;
  FgrpSize.align := alTop;
  FgrpSize.Caption := ' Size ';

  AddLabel(FgrpSize, 24, 15, 'Mode');
  FcbxSize := TCombobox.Create(Self);
  FcbxSize.Parent := FgrpSize;
  FcbxSize.Left := 74;
  FcbxSize.Top := 25;
  FcbxSize.Width := 200;
  FcbxSize.Style := csDropDownList;
  FcbxSize.OnChange := SizePolicyChange;

  AddLabel(FgrpSize, 53, 74, 'Height');
  FspnHeight := TSpinEdit.Create(Self);
  FspnHeight.Parent := FgrpSize;
  FspnHeight.Left := 120;
  FspnHeight.Top := 49;
  FspnHeight.Width := 50;
  FspnHeight.MinValue := 1;
  FspnHeight.MaxValue := 10000;
  FspnHeight.OnChange := spnHeightChange;
  FspnHeight.OnExit := spnHeightExit;

  AddLabel(FgrpSize, 53, 185, 'Width');
  FspnWidth := TSpinEdit.Create(Self);
  FspnWidth.Parent := FgrpSize;
  FspnWidth.Left := 222;
  FspnWidth.Top := 49;
  FspnWidth.Width := 52;
  FspnWidth.MinValue := 1;
  FspnWidth.MaxValue := 10000;
  FspnWidth.OnChange := spnWidthChange;
  FspnWidth.OnExit := spnWidthExit;

  FtrckSize := TTrackBar.Create(Self);
  FtrckSize.Parent := FgrpSize;
  FtrckSize.Left := 67;
  FtrckSize.Top := 74;
  FtrckSize.Width := 190;
  FtrckSize.Min := 1;
  FtrckSize.Max := 200;
  FtrckSize.Frequency := 40;
  FtrckSize.LineSize := 40;
  FtrckSize.OnChange := trckSizeChange;

  FedtPercent := TEdit.Create(Self);
  FedtPercent.Parent := FgrpSize;
  FedtPercent.Left := 254;
  FedtPercent.Top := 74;
  FedtPercent.Width := 40;
  FedtPercent.Height := 12;
  FedtPercent.BorderStyle := bsNone;
  FedtPercent.ReadOnly := True;
  FedtPercent.Color := clBtnFace;


  FgrpSize.Height := FtrckSize.Top + FtrckSize.Height-2;

  // Border.
  FgrpBorder := TGroupBox.Create(Self);
  FgrpBorder.Parent := ClientPanel;
  FgrpBorder.align := alTop;
  FgrpBorder.Caption := ' Border ';

  AddLabel(FgrpBorder, 26, 15, 'Width');
  FspnBWidth := TSpinEdit.Create(Self);
  FspnBWidth.Parent := FgrpBorder;
  FspnBWidth.Left := 74;
  FspnBWidth.Top := 22;
  FspnBWidth.Width := 60;
  FspnBWidth.MinValue := 0;
  FspnBWidth.MaxValue := 50;
  FspnBWidth.OnExit := spnBWidthExit;

  AddLabel(FgrpBorder, 52, 15, 'Colour');
  FcbxColour := TUixHTMLColourComboBox.Create(Self);
  FcbxColour.Parent := FgrpBorder;
  FcbxColour.Left := 74;
  FcbxColour.Top := 48;
  FcbxColour.Width := 200;

  FgrpBorder.Height := FcbxColour.Top + FcbxColour.Height + 10;

  FgrpOther := TGroupBox.Create(Self);
  FgrpOther.Parent := ClientPanel;
  FgrpOther.align := alTop;
  FgrpOther.Caption := ' Miscellaineous ';

  AddLabel(FgrpOther, 26, 15, 'Vertical Alignment');
  FcbxVerticalAlignment := TUixCombobox.Create(Self);
  FcbxVerticalAlignment.Parent := FgrpOther;
  FcbxVerticalAlignment.Left := 128;
  FcbxVerticalAlignment.Top := 22;
  FcbxVerticalAlignment.Width := 146;
  FcbxVerticalAlignment.Style := csDropDownList;
  For aLoop := Low(TWPImageVerticalAlignment) To High(TWPImageVerticalAlignment) Do
    FcbxVerticalAlignment.Items.Add(NAMES_IMAGEVERTICALALIGNMENT[aLoop]);

  FcbTransparent := TUixCheckBox.Create(Self);
  FcbTransparent.Parent := FgrpOther;
  FcbTransparent.Left := 15;
  FcbTransparent.Top := 48;
  FcbTransparent.Width := 90;
  FcbTransparent.Caption := 'Transparent';
  FcbTransparent.OnClick := TransparentClick;

  FcbxTransparent := TUixHTMLColourComboBox.Create(Self);
  FcbxTransparent.Parent := FgrpOther;
  FcbxTransparent.Left := 128;
  FcbxTransparent.Top := 48;
  FcbxTransparent.Width := 146;

  FgrpOther.Height := FcbxTransparent.Top + FcbxTransparent.Height + 10;

End;


procedure TWPImageFrame.Restore;
begin
  inherited;
end;

Procedure TWPImageFrame.Finalise;
Begin
  FImage.Free;

  Inherited;
End;


Function TWPImageFrame.GetImage : TWPWorkingDocumentImagePiece;
Begin
  Assert(Invariants('GetImage', FImage, TWPWorkingDocumentImagePiece, 'Image'));
  Result := FImage;
End;


Procedure TWPImageFrame.SetImage(Const Value: TWPWorkingDocumentImagePiece);
Begin
  FImage.Free;
  FImage := Value;

  FChanging := True;
  Try
    FedtInfo.Text := FImage.ImageTypename + ' ('+IntegerToString(FImage.Image.Height)+'px high, '+ IntegerToString(FImage.Image.Width)+'px wide)';
    FspnFrame.MinValue := 1;
    FspnFrame.MaxValue := FImage.MaxFrameIndex;
    FspnFrame.Value := FImage.FrameIndex + 1;
    FspnFrame.Enabled := FspnFrame.MaxValue > 1;

    FspnHeight.Value := FImage.Height;
    FspnWidth.Value := FImage.Width;
    FspnBWidth.Value := FImage.Border;
    FcbxColour.Value := FImage.BorderColour;
    FcbxVerticalAlignment.ItemIndex := Ord(FImage.VerticalAlignment);
    FcbxSize.Items.Clear;
    FcbxSize.Items.Add('Manual');
    FcbxSize.Items.Add('Page Width');
    FcbxSize.Items.Add('Whole Page');
    FcbxSize.ItemIndex := Ord(FImage.SizePolicy);
    SizePolicyChange(nil);
    FcbTransparent.checked := False;
    If Not (FImage.Image Is TFslBitmapGraphic) And Not (FImage.Image Is TFslIconGraphic)  Then
    Begin
      FcbTransparent.Enabled := False;
      FcbxTransparent.Enabled := False;
      FcbxTransparent.Color := clSilver;
      FcbxTransparent.Value := clSilver;
    End
    Else If (FImage.TransparentColour <> DEF_COLOUR) Then
    Begin
      FcbTransparent.checked := True;
      FcbxTransparent.Value := FImage.TransparentColour;
    End
    Else
    Begin
      FcbxTransparent.Enabled := False;
      FcbxTransparent.Value := clBlack;
    End;
  Finally
    FChanging := False;
  End;

  AdjustTracker;
End;


procedure TWPImageFrame.SizePolicyChange(oSender: TObject);
begin
  if FcbxSize.ItemIndex = 0 then
  begin
    FspnHeight.Enabled  := true;
    FspnWidth.enabled  := true;
    FtrckSize.enabled  := true;
    FedtPercent.enabled  := true;
  end
  else
  begin
    FspnHeight.enabled  := false;
    FspnWidth.enabled  := false;
    FtrckSize.enabled  := false;
    FedtPercent.enabled  := false;
  end;
end;

Procedure TWPImageFrame.AdjustTracker;
Begin
  FChanging := True;
  Try
    If abs((FspnWidth.Value / FImage.Image.Width) - (FspnHeight.Value / FImage.Image.Height)) < 0.02 Then
    Begin
      FtrckSize.Position := Trunc(((FspnWidth.Value / FImage.Image.Width) + (FspnHeight.Value / FImage.Image.Height)) * 50);
      FedtPercent.Text := IntegerToString(FtrckSize.Position)+'%';
    End
    Else
    Begin
      FtrckSize.Position := Trunc(((FspnWidth.Value / FImage.Image.Width) + (FspnHeight.Value / FImage.Image.Height)) * 50);
      FedtPercent.Text := '--';
    End;
  Finally
    FChanging := False;
  End;
End;


Procedure TWPImageFrame.spnHeightChange(oSender: TObject);
Begin
  If Not FChanging And (FspnHeight.Text <> '') Then
    AdjustTracker;
End;


Procedure TWPImageFrame.spnHeightExit(oSender: TObject);
Begin
  If FspnHeight.Text = '' Then
    FspnHeight.Text := '1';
End;


Procedure TWPImageFrame.spnWidthChange(oSender: TObject);
Begin
  If Not FChanging And (FspnWidth.Text <> '') Then
    AdjustTracker;
End;


Procedure TWPImageFrame.spnWidthExit(oSender: TObject);
Begin
  If FspnWidth.Text = '' Then
    FspnWidth.Text := '1';
End;


Procedure TWPImageFrame.spnBWidthExit(oSender: TObject);
Begin
  If FspnBWidth.Text = '' Then
    FspnBWidth.Text := '1';
End;


Procedure TWPImageFrame.trckSizeChange(oSender: TObject);
Begin
  If Not FChanging Then
  Begin
    FChanging := True;
    Try
      If FtrckSize.Position < 10 Then
        FtrckSize.Position := 10;
      FspnHeight.Value := Trunc((FImage.Image.Height/ 100) * FtrckSize.Position);
      FspnWidth.Value := Trunc((FImage.Image.Width/ 100) * FtrckSize.Position);
      FedtPercent.Text := IntegerToString(FtrckSize.Position)+'%';
    Finally
      FChanging := False;
    End;
  End;
End;


Procedure TWPImageFrame.Accept;
Begin
  Inherited;
  FImage.FrameIndex := FspnFrame.Value - 1;
  FImage.Height := FspnHeight.Value;
  FImage.Width := FspnWidth.Value;
  FImage.Border := FspnBWidth.Value;
  FImage.BorderColour := FcbxColour.Value;
  FImage.VerticalAlignment := TWPImageVerticalAlignment(FcbxVerticalAlignment.ItemIndex);
  FImage.SizePolicy := TWordProcessorImageSizePolicy(FcbxSize.ItemIndex);
  If FcbTransparent.Checked Then
    FImage.TransparentColour := FcbxTransparent.Value
  Else
    FImage.TransparentColour := DEF_COLOUR;
End;


Procedure TWPImageFrame.TransparentClick(oSender: TObject);
Begin
  If Not FChanging Then
  Begin
    If FcbTransparent.Checked Then
    Begin
      FcbxTransparent.Enabled := True;
      FcbxTransparent.Color := clWhite;
      FImage.TransparentColour := FcbxTransparent.Value;
    End
    Else
    Begin
      FcbxTransparent.Enabled := FcbTransparent.Checked;
      FcbxTransparent.Color := clSilver;
      FImage.TransparentColour := DEF_COLOUR;
    End;
  End;
End;

Function TWPImageMapDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPImageMapFrame;
End;


Function TWPImageMapDialog.DialogCaption : String;
Begin
  Result := 'Image Map Details';
End;


Function TWPImageMapDialog.GetFrame : TWPImageMapFrame;
Begin
  Result := TWPImageMapFrame(Inherited Frame);
End;


Function TWPImageMapDialog.GetImage: TWPWorkingDocumentImagePiece;
Begin
  Result := Frame.Image;
End;


Procedure TWPImageMapDialog.SetImage(Const Value: TWPWorkingDocumentImagePiece);
Begin
  Frame.Image := Value;
  btnExport.Enabled := Value.HasImageMap And (Value.ImageMap.Areas.Count > 0);
End;


procedure TWPImageMapDialog.Initialise;
begin
  inherited;

  btnExport := TUixButton.Create(self);
  btnExport.Parent := BottomPanel;
  btnExport.Caption := 'Export';
  btnExport.Enabled := False;
  btnExport.Left := 5;
  btnExport.OnClick := OnExportClick;

  sdExport := TUixSaveFileDialog.Create(self);
  sdExport.Filter := 'XML file (*.xml) |*.XML';

  BorderStyleSizeable;
  Frame.OnMapAreaChanged := OnMapAreaChanged;
End;


Procedure TWPImageMapDialog.OnExportClick(Sender :TObject);
var
  oMap : TWPImageMap;
Begin
  If (sdExport.Execute) Then
  Begin
    // Saving to sdExport.FileName
    raise EWPException.create('Not done yet');
  End
End;


Procedure TWPImageMapDialog.OnMapAreaChanged(Sender : TObject);
Begin
  btnExport.Enabled := Image.HasImageMap And (Image.ImageMap.Areas.Count > 0);
End;




Function TWPImageMapFrame.DesiredHeight: Integer;
Begin
  Result := 450;
End;


Function TWPImageMapFrame.DesiredWidth: Integer;
Begin
  Result := 600;
End;


Procedure TWPImageMapFrame.Initialise;
Var
  NextX, NextY : Integer;
  tmpLabel : TUixLabel;
Begin
  Inherited;

  // Main edit
  FEditBox := TGroupBox.Create(Self);
  FEditBox.Parent := ClientPanel;
  FEditBox.Align := alLeft;
  FEditBox.Width := EDIT_BOX_WITH;
  FEditBox.Caption := '';  // To be filled with image info.

  // Map areas edit
  NextX := H_PADDING;
  NextY := V_PADDING * 3; // avoid overwritten the caption
  FMapBox := TGroupBox.Create(Self);
  FMapBox.Parent := FEditBox;
  FMapBox.Top := NextY;
  FMapBox.Left := NextX;
  FMapBox.Width := FEditBox.Width - (2 * FMapBox.Left);
  FMapBox.Height := 170;
  FMapBox.Caption := ' Map Areas ';

  FMaps := TListBox.Create(Self);
  FMaps.Parent := FMapBox;
  FMaps.Top := NextY;
  FMaps.Left := NextX;
  FMaps.Width := FMapBox.Width - (2 * FMaps.Left);
  FMaps.Height := FMapBox.Height - FMaps.Top - BUTTON_HEIGHT - (V_PADDING * 2);
  FMaps.OnClick := OnMapAreaSelected;

  FAddMap := TButton.Create(Self);
  FAddMap.Enabled := True; // Always true
  FAddMap.Parent := FMapBox;
  FAddMap.Caption := ' Append ';
  FAddMap.Height := BUTTON_HEIGHT;
  FAddMap.Width := 70;
  FAddMap.Top := FMaps.Top + FMaps.Height + V_PADDING;
  FAddMap.Left := ((FMapBox.Width - 140) Div 2) - H_Padding;
  FAddMap.OnClick := OnAddAreaClick;

  FRemoveMap := TButton.Create(Self);
  FRemoveMap.Enabled := False;
  FRemoveMap.Parent := FMapBox;
  FRemoveMap.Caption := ' Remove ';
  FRemoveMap.Top := FAddMap.Top;
  FRemoveMap.Left := FAddMap.Left + FAddMap.Width + H_PADDING;
  FRemoveMap.Height := BUTTON_HEIGHT;
  FRemoveMap.Width := 70;
  FRemoveMap.OnClick := OnRemoveAreaClick;


  // Editable area info.
  NextY := FMapBox.Height + FMapBox.Top + V_PADDING;
  NextX := H_PADDING;
  FAreaEditBox := TGroupBox.Create(Self);
  FAreaEditBox.Parent := FEditBox;
  FAreaEditBox.Top := NextY;
  FAreaEditBox.Left := NextX;
  FAreaEditBox.Width := FEditBox.Width - COORD_LIST_WIDTH - FAreaEditBox.Left - (H_PADDING * 2);
  //FAreaEditBox.Height := 250;
  FAreaEditBox.Height := DesiredHeight - FAreaEditBox.Top - V_PADDING;
  FAreaEditBox.Caption := ' Area Info. ';

  NextX := H_PADDING;
  NextY := V_PADDING * 3;
  tmpLabel := AddLabel(FAreaEditBox, NextY, NextX, ' Area Title ');

  NextY := NextY + tmpLabel.Height + V_PADDING;
  FTitle := TEdit.Create(Self);
  FTitle.Enabled := False;
  FTitle.Parent := FAreaEditBox;
  FTitle.Top := NextY;
  FTitle.Left := NextX;
  FTitle.Width := FAreaEditBox.Width - (2 * FTitle.Left);
  FTitle.OnChange := OnAreaTitleChanged;

  NextY := NextY + FTitle.Height + V_PADDING;
  tmpLabel := AddLabel(FAreaEditBox, NextY, NextX, ' Area URL ');

  NextY := NextY + tmpLabel.Height + V_PADDING;
  FUrl := TEdit.Create(Self);
  FUrl.Enabled := False;
  FUrl.Parent := FAreaEditBox;
  FUrl.Top := NextY;
  FUrl.Left := NextX;
  FUrl.Width := FAreaEditBox.Width - (2 * FUrl.Left);
  FUrl.OnChange := OnAreaUrlChanged;

  NextY := NextY + FUrl.Height + V_PADDING;
  tmpLabel := AddLabel(FAreaEditBox, NextY, NextX, ' Area Key ');

  NextY := NextY + tmpLabel.Height + V_PADDING;
  FKey := TEdit.Create(Self);
  FKey.Enabled := False;
  FKey.CharCase := ecUpperCase;
  FKey.MaxLength := 1;
  FKey.AutoSelect := True;
  FKey.Parent := FAreaEditBox;
  FKey.Top := NextY;
  FKey.Left := NextX;
  FKey.Width := FAreaEditBox.Width - (2 * FKey.Left);
  FKey.OnChange := OnAreaKeyChanged;
  NextY := NextY + FKey.Height + V_PADDING;

  FcbLinkColour := TUixCheckBox.Create(Self);
  FcbLinkColour.Parent := FAreaEditBox;
  FcbLinkColour.Caption := ' Area Link Color ';
  FcbLinkColour.Enabled := False;
  FcbLinkColour.Top := NextY;
  FcbLinkColour.Left := NextX;
  FcbLinkColour.Width := FAreaEditBox.Width - (2 * FcbLinkColour.Left);
  FcbLinkColour.OnChange := OnLinkColourChanged;
  NextY := NextY + FcbLinkColour.Height + V_PADDING;

  FcbxLinkColour := TUixHTMLColourComboBox.Create(Self);
  FcbxLinkColour.Enabled := False;
  FcbxLinkColour.Parent := FAreaEditBox;
  FcbxLinkColour.Top := NextY;
  FcbxLinkColour.Left := NextX;
  FcbxLinkColour.Width := FAreaEditBox.Width - (2 * FcbxLinkColour.Left);
  FcbxLinkColour.OnChange := OnLinkColourSelected;
  NextY := NextY + FcbxLinkColour.Height + V_PADDING;

  FcbHoverColour := TUixCheckBox.Create(Self);
  FcbHoverColour.Parent := FAreaEditBox;
  FcbHoverColour.Caption := ' Area Hover Color ';
  FcbHoverColour.Enabled := False;
  FcbHoverColour.Top := NextY;
  FcbHoverColour.Left := NextX;
  FcbHoverColour.Width := FAreaEditBox.Width - (2 * FcbHoverColour.Left);
  FcbHoverColour.OnChange := OnHoverColourChanged;
  NextY := NextY + FcbHoverColour.Height + V_PADDING;

  FcbxHoverColour := TUixHTMLColourComboBox.Create(Self);
  FcbxHoverColour.Enabled := False;
  FcbxHoverColour.Parent := FAreaEditBox;
  FcbxHoverColour.Top := NextY;
  FcbxHoverColour.Left := NextX;
  FcbxHoverColour.Width := FAreaEditBox.Width - (2 * FcbxHoverColour.Left);
  FcbxHoverColour.OnChange := OnHoverColourSelected;

  // Coords
  NextY := FAreaEditBox.Top;
  NextX := FAreaEditBox.Left + FAreaEditBox.Width + H_PADDING;
  FCoordsBox := TGroupBox.Create(Self);
  FCoordsBox.Parent := FEditBox;
  FCoordsBox.Top := NextY;
  FCoordsBox.Left := NextX;
  FCoordsBox.Width := COORD_LIST_WIDTH;
  FCoordsBox.Height := DesiredHeight - FAreaEditBox.Top - V_PADDING;
  FCoordsBox.Caption := ' Coords ';

  NextX := H_PADDING;
  NextY := V_PADDING * 3;
  FCoords := TListBox.Create(Self);
  FCoords.Parent := FCoordsBox;
  FCoords.Top := NextY;
  FCoords.Left := NextX;
  FCoords.Width := FCoordsBox.Width - FCoords.Left - H_PADDING;
  FCoords.Height := FCoordsBox.Height - FCoords.Top - BUTTON_HEIGHT - V_PADDING * 2;
  FCoords.OnClick := OnCoordSelected;

  FAddCoord := TButton.Create(Self);
  FAddCoord.Enabled := False;
  FAddCoord.Parent := FCoordsBox;
  FAddCoord.Caption := ' Add ';
  FAddCoord.Top := FCoords.Top + FCoords.Height + V_PADDING;
  FAddCoord.Left := NextX;
  FAddCoord.Height := BUTTON_HEIGHT;
  //FAddCoord.Width := FCoordsBox.Width div 2 - (H_PADDING * 3);
  FAddCoord.Width := 45;
  FAddCoord.OnClick := OnAddCoordClick;

  FRemoveCoord := TButton.Create(Self);
  FRemoveCoord.Enabled := False;
  FRemoveCoord.Parent := FCoordsBox;
  FRemoveCoord.Caption := ' Remove ';
  FRemoveCoord.Top := FAddCoord.Top;
  FRemoveCoord.Left := FAddCoord.Left + FAddCoord.Width + H_PADDING;
  FRemoveCoord.Height := BUTTON_HEIGHT;
  FRemoveCoord.Width := FCoordsBox.Width - FRemoveCoord.Left - H_PADDING;
  FRemoveCoord.OnClick := OnRemoveCoordClick;


  // Review box
  FreviewBox := TGroupBox.Create(Self);
  FreviewBox.Parent := ClientPanel;
  FreviewBox.Align := alClient;
  FreviewBox.Caption := ' Review ';

  FReviewStatus := TStatusBar.Create(Self);
  FReviewStatus.Parent := FreviewBox;
  FReviewStatus.Align := alBottom;
  // add two panels
  FReviewStatus.Panels.Add;
  FReviewStatus.Panels.Add;

  // TODO: add zoom toolbar

  FImageBox := TUixScrollBox.Create(Self);
  FImageBox.Parent := FreviewBox;
  FImageBox.Align := alClient;
  FImageBox.OnResize := OnImageBoxResize;

  FImageView := TUixImage.Create(FImageBox);
  FImageView.Parent := FImageBox;
  FImageView.OnMouseMove := OnMouseMoveInImage;
  FImageView.OnMouseDown := OnMouseDownInImage;
  FImageView.AutoSize := True;
End;


Procedure TWPImageMapFrame.OnImageBoxResize(Sender : TObject);
Begin
  If (FImageView.Height < FImageBox.Height) Then
    FImageView.Top := (FImageBox.Height - FImageView.Height) Div 2;
  If (FImageView.Width < FImageBox.Width) Then
    FImageView.Left := (FImageBox.Width - FImageView.Width) Div 2;
End;


Procedure TWPImageMapFrame.OnMapAreaSelected(Sender : TObject);
Var
  iLoop : Integer;
Begin
  // replace previous area map with the newly selected area map
  If Assigned(FArea) Then
  Begin
    DrawAreaMapOnImage;
    FArea.Free;
    FArea := Nil;
  End;

  FRemoveMap.Enabled := (FMaps.ItemIndex <> -1);
  If FMaps.ItemIndex <> -1 Then
  Begin
    FArea := FImage.ImageMap.Areas[FMaps.ItemIndex].Link;
    DrawAreaMapOnImage;

    FChanging := True;
    Try
      FTitle.Enabled := True;
      FTitle.Text := FArea.Title;
      FUrl.Enabled := True;
      FUrl.Text := FArea.URL;
      FKey.Enabled := True;
      FKey.Text := FArea.Key;
      FcbLinkColour.Enabled := True;
      FcbLinkColour.Checked := (FArea.LinkColour <> DEF_COLOUR);
      FcbxLinkColour.Value := FArea.LinkColour;
      FcbHoverColour.Enabled := True;
      FcbHoverColour.Checked := (FArea.HoverColour <> DEF_COLOUR);
      FcbxHoverColour.Value := FArea.HoverColour;

      FAddCoord.Enabled := True;
      FRemoveCoord.Enabled := False;
      FCoords.Items.Clear;
      If FArea.Coordinates.Count > 0 Then
      Begin
        For iLoop := 0 To FArea.Coordinates.Count - 1 Do
          FCoords.Items.Add('(' + IntegerToString(FArea.Coordinates[iLoop].X)
                        + ',' + IntegerToString(FArea.Coordinates[iLoop].Y) + ')');

        FCoords.ItemIndex := 0;
      End;
    Finally
      FChanging := False;
    End;
  End
  Else
  Begin
    FChanging := True;
    Try
      FTitle.Enabled := True;
      FTitle.Text := '';
      FUrl.Enabled := False;
      FUrl.Text := '';
      FKey.Enabled := False;
      FKey.Text := '';
      FcbLinkColour.Checked := False;
      FcbLinkColour.Enabled := False;
      FcbxLinkColour.Value := DEF_COLOUR;
      FcbHoverColour.Checked := False;
      FcbHoverColour.Enabled := False;
      FcbxHoverColour.Value := DEF_COLOUR;
      FAddCoord.Enabled := False;
      FCoords.Enabled := False;
      FCoords.Items.Clear;
    Finally
      FChanging := False;
    End;
  End;

  // Propagate to Coord
  FCoords.OnClick(Self);
End;


Procedure TWPImageMapFrame.OnAreaTitleChanged(Sender: TObject);
Begin
  If Not FChanging And Assigned(FArea) Then
  Begin
    FArea.Title := FTitle.Text;
    UpdateCurrentAreaMapText;
  End;
End;


Procedure TWPImageMapFrame.OnAreaUrlChanged(Sender: TObject);
Begin
  If Not FChanging And Assigned(FArea) Then
  Begin
    FArea.URL := FUrl.Text;
    If FArea.Title = '' Then
      UpdateCurrentAreaMapText;
  End;
End;


Procedure TWPImageMapFrame.OnAreaKeyChanged(Sender: TObject);
Begin
  If Not FChanging And Assigned(FArea) Then
  Begin
    If (Length(FKey.Text) > 0) And (Not CharInSet(FKey.Text[1], ['A'..'Z', '0'..'9'])) Then
      FKey.Text := FArea.Key
    Else
      FArea.Key := FKey.Text;
  End;
End;


Procedure TWPImageMapFrame.OnLinkColourChanged(Sender: TObject);
Begin
  FcbxLinkColour.Enabled := FcbLinkColour.Checked;

  If Not FChanging And Assigned(FArea) Then
  Begin
    If Not FcbxLinkColour.Enabled Then
    Begin
        FArea.LinkColour := DEF_COLOUR;
        FcbxLinkColour.Value := clGray;
    End
    Else
      FArea.LinkColour := FcbxLinkColour.Value;
  End;
End;


Procedure TWPImageMapFrame.OnLinkColourSelected(Sender: TObject);
Begin
  If Not FChanging And Assigned(FArea) Then
    FArea.LinkColour := FcbxLinkColour.Value;
End;


Procedure TWPImageMapFrame.OnHoverColourChanged(Sender: TObject);
Begin
  FcbxHoverColour.Enabled := FcbHoverColour.Checked;

  If Not FChanging And Assigned(FArea) Then
  Begin
    If Not FcbxHoverColour.Enabled Then
    Begin
        FArea.HoverColour := DEF_COLOUR;
        FcbxHoverColour.Value := clSilver;
    End
    Else
      FArea.HoverColour := FcbxHoverColour.Value;
  End;
End;


Procedure TWPImageMapFrame.OnHoverColourSelected(Sender: TObject);
Begin
  If Not FChanging And Assigned(FArea) Then
    FArea.HoverColour := FcbxHoverColour.Value;
End;


Procedure TWPImageMapFrame.OnCoordSelected(Sender: TObject);
Begin
  If Assigned(FCoord) Then
  Begin
    DrawCurrentCoord;
    FCoord.Free;
  End;

  If FCoords.ItemIndex <> -1 Then
  Begin
    FCoord := FArea.Coordinates[FCoords.ItemIndex].Link;
    DrawCurrentCoord;
  End
  Else
    FCoord := Nil;

  FCoords.Enabled := (FCoords.Items.Count > 0);
  FRemoveCoord.Enabled := (FCoords.ItemIndex <> -1);
End;


Procedure TWPImageMapFrame.OnAddAreaClick(Sender: TObject);
Var
  oArea : TWPImageMapArea;
Begin
  // Create a new image map if one doesn't exist
  If Not FImage.HasImageMap Then
    FImage.HasImageMap := True;

  oArea := FImage.ImageMap.Areas.New;
  Try
    // init. the new area
    oArea.Title := 'New Map';
    oArea.URL := 'URL';
    oArea.Key := 'Key';
    oArea.Coordinates.Add(0, 0);
    oArea.Coordinates.Add(0, IntegerMin(FImage.Image.Height, 10));
    oArea.Coordinates.Add(IntegerMin(FImage.Image.Width, 10), 0);

    FImage.ImageMap.Areas.Add(oArea.Link);
    FMaps.Items.Add(GetAreaMapAsText(oArea));
    FMaps.ItemIndex := FMaps.Items.Count - 1;
    OnMapAreaSelected(Self);
  Finally
    oArea.Free;
  End;

  If Assigned(FOnMapAreaChanged) Then
    FOnMapAreaChanged(Self);
End;


Procedure TWPImageMapFrame.OnRemoveAreaClick(Sender: TObject);
Var
  iIndex : Integer;
Begin
  If FMaps.ItemIndex <> -1 Then
  Begin
    FImage.ImageMap.Areas.DeleteByIndex(FMaps.ItemIndex);

    // More than 1 item, then selected the next one (or the previous one)
    If FMaps.Items.Count > 1 Then
    Begin
      If (FMaps.ItemIndex = FMaps.Items.Count - 1) Then
        iIndex := FMaps.ItemIndex - 1 // last index, move up after deletion
      Else
        iIndex := FMaps.ItemIndex;
      FMaps.Items.Delete(FMaps.ItemIndex);
      FMaps.ItemIndex := iIndex;
    End
    Else
      FMaps.Items.Delete(FMaps.ItemIndex);
  End;

  // Propagate update
  FMaps.OnClick(Self);
  If Assigned(FOnMapAreaChanged) Then
    FOnMapAreaChanged(Self);
End;


Procedure TWPImageMapFrame.OnAddCoordClick(Sender: TObject);
Var
  iIndex : Integer;
Begin
  If (FCoords.ItemIndex <> -1) Then
    iIndex := FCoords.ItemIndex
  Else If (FCoords.Items.Count > 0) Then
    iIndex := FCoords.Items.Count - 1
  Else
    iIndex := -1;

  If iIndex >= 0 Then
  Begin
    DrawAreaMapOnImage;
    FArea.Coordinates.Insert(iIndex + 1, FArea.Coordinates[iIndex].Clone);
    DrawAreaMapOnImage;
    FCoords.Items.Insert(iIndex + 1, FCoords.Items[iIndex]);
    FCoords.ItemIndex := iIndex + 1;
  End
  Else
  Begin
     FArea.Coordinates.Add(0,0);
     FCoords.Items.Add('(' + IntegerToString(FArea.Coordinates[0].X)
                        + ',' + IntegerToString(FArea.Coordinates[0].Y) + ')');
     FCoords.ItemIndex := 0;
     FCoords.Enabled := True;
   End;
   OnCoordSelected(Self);
End;


Procedure TWPImageMapFrame.OnRemoveCoordClick(Sender: TObject);
Var
  iIndex : Integer;
Begin
  If Assigned(FArea) And (FCoords.ItemIndex <> -1) Then
  Begin
    DrawAreaMapOnImage;
    FArea.Coordinates.DeleteByIndex(FCoords.ItemIndex);
    DrawAreaMapOnImage;

    // More than 1 item, then selected the next one (or the previous one)
    If FCoords.Items.Count > 1 Then
    Begin
      If (FCoords.ItemIndex = FCoords.Items.Count - 1) Then
        iIndex := FCoords.ItemIndex - 1 // last index, move up after deletion
      Else
        iIndex := FCoords.ItemIndex;
      FCoords.Items.Delete(FCoords.ItemIndex);
      FCoords.ItemIndex := iIndex;
    End
    Else
      FCoords.Items.Delete(FCoords.ItemIndex);
    FCoords.OnClick(Self);
  End;
End;


Procedure TWPImageMapFrame.OnMouseDownInImage(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  iIndex : Integer;
Begin
  If FCoords.ItemIndex <> -1 Then
  Begin
    iIndex := FCoords.ItemIndex;
    DrawCurrentCoord;
    DrawAreaMapOnImage;
    FArea.Coordinates[iIndex].X := X;
    FArea.Coordinates[iIndex].Y := Y;
    DrawAreaMapOnImage;
    DrawCurrentCoord;
    FCoords.Items[iIndex] := '(' + IntegerToString(X) + ', ' + IntegerToString(Y) + ')';
  End;
End;


Procedure TWPImageMapFrame.OnMouseMoveInImage(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Begin
  FImageView.Cursor := crHandPoint;
  FReviewStatus.Panels[1].Text := 'x=' + IntegerToString(X) + ': y=' + IntegerToString(Y);
End;


Procedure TWPImageMapFrame.Finalise;
Begin
  FArea.Free;
  FCoord.Free;
  FImage.Free;
  FBitmap.Free;

  Inherited;
End;


Function TWPImageMapFrame.GetImage : TWPWorkingDocumentImagePiece;
Begin
  Assert(Invariants('GetImage', FImage, TWPWorkingDocumentImagePiece, 'Image'));
  Result := FImage;
End;


Procedure TWPImageMapFrame.SetImage(Const Value: TWPWorkingDocumentImagePiece);
Var
  iLoop : Integer;
Begin
  FImage.Free;
  FImage := Value;

  If Assigned(FImage) Then
  Begin
    FChanging := True;
    Try
      // show the image
      FEditBox.Caption := FImage.ImageTypename + ' ('+IntegerToString(FImage.Image.Height)+'px high, '+ IntegerToString(FImage.Image.Width)+'px wide)';
      FBitmap.Free;
      FBitmap := TBitmap.Create;
      FBitmap.Canvas.Lock;
      Try
        FBitmap.Height := FImage.Image.Height;
        FBitmap.Width := FImage.Image.Width;
        if not (FImage.Image is TFslVCLGraphic) then
          raise EWPException.create('Cannot Edit an image Map for this kind of Image');

        FBitmap.Canvas.Draw(0, 0, TFslVCLGraphic(FImage.Image).Handle);
      Finally
        FBitmap.Canvas.Unlock;
      End;

      FImageView.Picture.Bitmap := FBitmap;

      // populate the map-areas list
      FMaps.Clear;
      If FImage.HasImageMap Then
      Begin
        For iLoop := 0 To FImage.ImageMap.Areas.Count - 1 Do
          FMaps.Items.Add(GetAreaMapAsText(FImage.ImageMap.Areas[iLoop]));
      End;
    Finally
      FChanging := False;
    End;

    // Select the first Map area
    FMaps.ItemIndex := 0;
    FMaps.OnClick(Self);
  End;
End;


Function TWPImageMapFrame.GetAreaMapAsText(oArea : TWPImageMapArea) :String;
Begin
  If oArea.Title <> '' Then
    Result := oArea.Title
  Else If oArea.URL <> '' Then
    Result := oArea.URL
  Else
  Begin
    Result := '- Area no: '
        + IntegerToString(FImage.ImageMap.Areas.IndexByReference(oArea)) + '-';
  End;
End;


Procedure TWPImageMapFrame.UpdateCurrentAreaMapText;
Var
  iIndex : Integer;
  sText : String;
Begin
  Assert(Condition(Assigned(FArea), 'UpdateCurrentAreaMapText', 'Current AreaMap Undefined'));

  iIndex := FMaps.ItemIndex;
  sText := GetAreaMapAsText(FArea);
  If sText <> FMaps.Items[iIndex] Then
    FMaps.Items[iIndex] := sText;
End;


Procedure TWPImageMapFrame.DrawAreaMapOnImage;
Var
  iLoop : Integer;
Begin
  Assert(Condition(Assigned(FArea), 'DrawAreaMapOnImage', 'Current AreaMap undefined'));

  If FArea.Coordinates.Count > 2 Then
  Begin
    FImageView.Canvas.Pen.Width := 1;
    FImageView.Canvas.Pen.Color := clSilver;
    FImageView.Canvas.Pen.Style := psSolid;
    FImageView.Canvas.Pen.Mode := pmXor;

    FImageView.Canvas.MoveTo(FArea.Coordinates[0].X, FArea.Coordinates[0].Y);
    For iLoop := 1 To FArea.Coordinates.Count - 1 Do
      FImageView.Canvas.LineTo(FArea.Coordinates[iLoop].X, FArea.Coordinates[iLoop].Y);

    FImageView.Canvas.LineTo(FArea.Coordinates[0].X, FArea.Coordinates[0].Y);
  End;
End;


Procedure TWPImageMapFrame.DrawCurrentCoord;
Var
  lowX, highX, lowY, highY: Integer;
Begin
  Assert(Condition(Assigned(FCoord), 'DrawCurrentCoord', 'Coordinate undefined'));

  FImageView.Canvas.Pen.Width := 3;
  FImageView.Canvas.Pen.Color := clGrayText;
  FImageView.Canvas.Pen.Style := psSolid;
  FImageView.Canvas.Pen.Mode := pmXor;
  lowX := IntegerMax(0, FCoord.X - 6);
  highX := IntegerMin(FBitmap.Width, FCoord.X + 6);
  lowY := IntegerMax(0, FCoord.Y - 6);
  highY := IntegerMin(FBitmap.Height, FCoord.Y + 6);

  FImageView.Canvas.MoveTo(lowX, FCoord.Y);
  FImageView.Canvas.LineTo(highX, FCoord.Y);
  FImageView.Canvas.MoveTo(FCoord.X, lowY);
  FImageView.Canvas.LineTo(FCoord.X, highY);
End;


{ TWPSymbolDialog }

Function TWPSymbolDialog.GetFrame : TWPSymbolFrame;
Begin
  Result := TWPSymbolFrame(Inherited Frame);
End;


Function TWPSymbolDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPSymbolFrame;
End;


Function TWPSymbolDialog.DialogCaption : String;
Begin
  Result := 'Insert Symbol';
End;


Function TWPSymbolDialog.GetSpecifiedFont : TWPSFontDetails;
Begin
  Result := FSpecifiedFont;
End;


Procedure TWPSymbolDialog.SetSpecifiedFont(Value : TWPSFontDetails);
Begin
  FSpecifiedFont.Free;
  FSpecifiedFont := Value;
  WorkingFont := SpecifiedFont.Clone;
  Frame.SymbolFont := WorkingFont.Link;
End;


Function TWPSymbolDialog.GetWorkingFont : TWPSFontDetails;
Begin
  Result := FWorkingFont;
End;


Procedure TWPSymbolDialog.SetWorkingFont(Value : TWPSFontDetails);
Begin
  FWorkingFont.Free;
  FWorkingFont := Value;
End;


Function TWPSymbolDialog.GetInsertChar: Char;
Begin
  Result := Frame.InsertChar;
End;

Function EnumSpecialFontsProc(Var LogFont: TLogFont; Var TextMetric: TTextMetric; FontType: Integer; Data: Pointer): Integer; Stdcall;
Var
  oDlg : TWPSymbolDialog;
Begin
  Result := 1;
  oDlg := TWPSymbolDialog(Data);
  If (FontType And TRUETYPE_FONTTYPE > 0) And (LogFont.lfCharSet = SYMBOL_CHARSET) And (LogFont.lfFaceName <> 'Symbol') And ((oDlg.FFontsAllowed.Count = 0) Or oDlg.FFontsAllowed.ExistsByValue(LogFont.lfFaceName)) Then
    oDlg.FFont.Items.Add(LogFont.lfFaceName);
End;

Procedure TWPSymbolDialog.Initialise;
Var
  oLabel : TUixLabel;
Begin
  Inherited;
  FFontsAllowed := TFslStringList.Create;

  HasOKButton := False;
  Frame.OnDone := Done;

  If AllowSpecialSymbols Then
  Begin
    oLabel := TUixLabel.Create(Self);
    oLabel.Parent := BottomPanel;
    oLabel.Left := 10;
    oLabel.Top := 5;
    oLabel.Caption := 'Font:';

    FFont := TUixComboBox.Create(Self);
    FFont.Parent := BottomPanel;
    FFont.Left := 50;
    FFont.Top := 2;
    FFont.Width := 250;
    FFont.Strict := True;
    FFont.Items.Add('Current Font');
    If (FFontsAllowed.Count = 0) Or FFontsAllowed.ExistsByValue('Symbol') Then
      FFont.Items.Add('Symbol');
    FFont.ItemIndex := 0;
    EnumFonts(Canvas.Handle,  Nil, @EnumSpecialFontsProc, pointer(Self));
    FFont.OnChange := FontChange;
  End;
End;


Procedure TWPSymbolDialog.Done(oSender: TObject);
Begin
  Okay;
End;

Destructor TWPSymbolDialog.Destroy;
Begin
  FFontsAllowed.Free;
  FWorkingFont.Free;
  FSpecifiedFont.Free;
  Inherited;
End;

Procedure TWPSymbolDialog.FontChange(oSender: TObject);
Begin
  WorkingFont := SpecifiedFont.Clone;
  If FFont.ItemIndex > 0 Then
    WorkingFont.Name := FFont.Text;
  Frame.SymbolFont := WorkingFont.Link;
End;

Function TWPSymbolDialog.DrawnFontName: String;
Begin
  If (FFont = Nil) Or (FFont.ItemIndex = 0) Then
    Result := ''
  Else
    Result := FFont.Text;
End;


{ TWPSymbolFrame }

Function TWPSymbolFrame.DesiredHeight: Integer;
Begin
  Result := 570;
End;


Function TWPSymbolFrame.DesiredWidth: Integer;
Begin
  Result := 650;
End;


Function TWPSymbolFrame.GetSymbolFont : TWPSFontDetails;
Begin
  Assert(Invariants('GetSymbolFont', FSymbolFont, TWPSFontDetails, 'SymbolFont'));
  Result := FSymbolFont;
End;


Procedure TWPSymbolFrame.SetSymbolFont(Value : TWPSFontDetails);
Begin
  FSymbolFont.Free;
  FSymbolFont := Value;
  BuildExample;
End;


Procedure TWPSymbolFrame.Initialise;
Begin
  Inherited;

  BuildForm;
End;


Procedure TWPSymbolFrame.Finalise;
Begin
  FSymbolFont.Free;
  Inherited;
End;

Procedure TWPSymbolFrame.BuildExample;
Var
  oBuilder : TWPDocumentBuilder;
  iLoop : Byte;
Begin
  oBuilder := TWPDocumentBuilder.Create;
  Try
    oBuilder.Document := TWPDocument.Create;
    With oBuilder.Document.Styles.Add('Symbol').Font Do
    Begin
      Name := FSymbolFont.Name;
      Size := 20;
    End;

    oBuilder.Start;
    oBuilder.StartTable.BorderPolicy := BorderPolicyGrid;
    oBuilder.StartTableRow;
    For iLoop := 32 To 255 Do
    Begin
      If (iLoop > 32) And (iLoop Mod 16 = 0) Then
      Begin
        oBuilder.EndTableRow;
        oBuilder.StartTableRow;
      End;
      With oBuilder.StartTableCell Do
      Begin
        MarginLeft := 1;
        MarginTop := 1;
        MarginRight := 1;
        MarginBottom := 1;
        Width := 1/17;
      End;
      oBuilder.DefineHotspot(chr(iLoop), DEF_COLOUR, clYellow);
      With oBuilder.StartParagraph Do
      Begin
        Format.AlignCentre;
        Font.Name := FSymbolFont.Name;
      End;
      oBuilder.AddText(chr(iLoop), 'Symbol');
      oBuilder.EndParagraph;
      oBuilder.EndTableCell;
    End;
    oBuilder.EndTableRow;
    oBuilder.EndTable;
    oBuilder.Stop;


    FGrid.DocumentHandler.LoadDocument(oBuilder.Document);
  Finally
    oBuilder.Free;
  End;
End;


Procedure TWPSymbolFrame.BuildForm;
Begin
  BorderWidth := 12;

  FGrid := TWordProcessor.Create(Self);
  FGrid.Parent := Self;
  FGrid.Settings.ModeBrowser;
  FGrid.Settings.Selecting := False;
  FGrid.AlignClient;
  FGrid.Settings.HorizontalMargin := 5;
  FGrid.Settings.VerticalMargin := 5;
  FGrid.OnHotSpotHover := HotspotHover;
  FGrid.OnHotSpot := Hotspot;
End;


Function TWPSymbolFrame.GetInsertChar: Char;
Begin
  Result := FInsertChar;
End;

Procedure TWPSymbolFrame.Restore;
Begin
  Inherited;
  BuildExample;
End;

Procedure TWPSymbolFrame.HotSpot(oSender: TWordProcessor; oInfo : TWPHotspotInformation);
Begin
  FInsertChar := oInfo.Hotspot.URL[1];
  FOnDone(Self);
End;

Procedure TWPSymbolFrame.HotSpotHover(oSender: TWordProcessor; bActive: Boolean; oInfo : TWPHotspotInformation);
Begin
  // nothing for now
End;

{ TWPInsertTableDialog }

Function TWPInsertTableDialog.GetFrame : TWPInsertTableFrame;
Begin
  Result := TWPInsertTableFrame(Inherited Frame);
End;


Function TWPInsertTableDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPInsertTableFrame;
End;


Function TWPInsertTableDialog.DialogCaption : String;
Begin
  Result := 'Insert Table';
End;


Function TWPInsertTableDialog.GetRows : Integer;
Begin
  Result := Frame.Rows;
End;


Procedure TWPInsertTableDialog.SetRows(Const Value : Integer);
Begin
  Frame.Rows := Value;
End;


Function TWPInsertTableDialog.GetColumns : Integer;
Begin
  Result := Frame.Columns;
End;


Procedure TWPInsertTableDialog.SetColumns(Const Value : Integer);
Begin
  Frame.Columns := Value;
End;



{ TWPInsertTableFrame }

Function TWPInsertTableFrame.DesiredHeight: Integer;
Begin
  Result := 90;
End;

Function TWPInsertTableFrame.DesiredWidth: Integer;
Begin
  Result := 180;
End;

Procedure TWPInsertTableFrame.Initialise;
Begin
  Inherited;

  BuildForm;
End;

Procedure TWPInsertTableFrame.Finalise;
Begin
  Inherited;
End;

Procedure TWPInsertTableFrame.BuildForm;
Begin
  FgrpSize := TUixGroupBox.Create(Self);
  FgrpSize.Parent := ClientPanel;
  FgrpSize.align := alTop;
  FgrpSize.Caption := ' Size';

  AddLabel(FgrpSize, 23, 10, 'Rows');
  FedtRows := TUixEdit.Create(Self);
  FedtRows.Parent := FgrpSize;
  FedtRows.Top := 20;
  FedtRows.Left := 70;
  FedtRows.Width := 80;

  AddLabel(FgrpSize, 49, 10, 'Columns');
  FedtCols := TUixEdit.Create(Self);
  FedtCols.Parent := FgrpSize;
  FedtCols.Top := 46;
  FedtCols.Left := 70;
  FedtCols.Width := 80;

  FgrpSize.Height := 110;
End;


Function TWPInsertTableFrame.GetRows : Integer;
Begin
  Result := StrToIntDef(FedtRows.Text, 0);
End;


Procedure TWPInsertTableFrame.SetRows(Const Value : Integer);
Begin
  FedtRows.Text := IntegerToString(Value);
End;


Function TWPInsertTableFrame.GetColumns : Integer;
Begin
  Result := StrToIntDef(FedtCols.Text, 0);
End;


Procedure TWPInsertTableFrame.SetColumns(Const Value : Integer);
Begin
  FedtCols.Text := IntegerToString(Value);
End;


Procedure TWPInsertTableFrame.Restore;
Begin
  Inherited;
  Rows := 2;
  Columns := 2;
End;


Function TWPInsertTableFrame.CanAccept : Boolean;
Begin
  Result := Inherited CanAccept;
  If Not Result Then
    Begin
    Result := (Rows > 0) And (Columns > 0);
    If Not Result Then
      DialogError('Rows and Columns must be positive numbers');
    End;
End;


{ TWPTablePropertiesDialog }

Function TWPTablePropertiesDialog.GetFrame : TWPTablePropertiesFrame;
Begin
  Result := TWPTablePropertiesFrame(Inherited Frame);
End;


Function TWPTablePropertiesDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPTablePropertiesFrame;
End;


Function TWPTablePropertiesDialog.DialogCaption : String;
Begin
  Result := 'Table Properties';
End;


Function TWPTablePropertiesDialog.GetTable : TWPWorkingDocumentTableStartPiece;
Begin
  Result := Frame.Table;
End;

Function TWPTablePropertiesDialog.GetTableRow : TWPWorkingDocumentTableRowStartPiece;
Begin
  Result := Frame.Row;
End;

Function TWPTablePropertiesDialog.GetTableCell: TWPWorkingDocumentTableCellStartPiece;
Begin
  Result := Frame.Cell;
End;

Procedure TWPTablePropertiesDialog.SetRange(oRange: TWPRange);
Var
  iRowStart, iRowEnd, iCellStart, iCellEnd: Integer;
Begin
  If Not oRange.Selection.HasSelection Then
  Begin
    // no selection -> apply to current cell/table/rows
    Frame.Table := oRange.CurrentTableStart.Clone;
    Frame.Row := oRange.CurrentTableRowStart.Clone;
    Frame.Cell := oRange.CurrentTableCellStart.Clone;
    iRowStart := oRange.CurrentTableStart.Rows.IndexByReference(oRange.CurrentTableRowStart);
    iRowEnd := iRowStart;
    iCellStart := oRange.CurrentTableRowStart.Cells.IndexByReference(oRange.CurrentTableCellStart);
    iCellEnd := iCellStart;
    Frame.BuildExampleTable(oRange.CurrentTableStart, iRowStart, iRowEnd, iCellStart, iCellEnd);
  End
  Else
  Begin
    Frame.Table := oRange.SelectedTable.Clone;
    Frame.Row := CalcCommonRowFormat(oRange.SelectedRows);
    Frame.Cell := CalcCommonCellFormat(oRange.SelectedCells);
    iRowStart := oRange.SelectedTable.Rows.IndexByReference(oRange.SelectedRows[0]);
    iRowEnd := oRange.SelectedTable.Rows.IndexByReference(oRange.SelectedRows[oRange.SelectedRows.Count - 1]);
    iCellStart := oRange.SelectedRows[0].Cells.IndexByReference(oRange.SelectedCells[0]);
    iCellEnd := oRange.SelectedRows[oRange.SelectedRows.Count - 1].Cells.IndexByReference(oRange.SelectedCells[oRange.SelectedCells.Count - 1]);
    Frame.BuildExampleTable(oRange.SelectedTable, iRowStart, iRowEnd, iCellStart, iCellEnd);
  End;
End;

Function TWPTablePropertiesDialog.CalcCommonCellFormat(oCells: TWPWorkingDocumentTableCellStartPieces): TWPWorkingDocumentTableCellStartPiece;
Var
  iLoop: Integer;
Begin
  Result := Nil;
  If (oCells.Count > 0) Then
  Begin
    Result := oCells[0].Clone;
    For iLoop := 1 To oCells.Count - 1 Do
      Result.MergeProperties(oCells[iLoop]);
  End;
End;

Function TWPTablePropertiesDialog.CalcCommonRowFormat(oRows: TWPWorkingDocumentTableRowStartPieces): TWPWorkingDocumentTableRowStartPiece;
Var
  iLoop: Integer;
Begin
  Result := Nil;
  If (oRows.Count > 0) Then
  Begin
    Result := oRows[0].Clone;
    If oRows.Count > 1 Then Result.State := tisMiddle;  // not the first/only row if more than one row is selected
    For iLoop := 1 To oRows.Count - 1 Do
      Result.MergeProperties(oRows[iLoop]);
  End;
End;

Function TWPTablePropertiesDialog.CanAccept: Boolean;
Begin
  Result := Frame.CanAccept;
End;


{ TWPTablePropertiesFrame }

Procedure TWPTablePropertiesFrame.BuildForm;
Begin
  FExample := TWordProcessor.Create(Self);
  FExample.Parent := ClientPanel;
  FExample.AlignTop;
  FExample.Height := 120;
  FExample.Settings.Interactive := False;
  FExample.Settings.TableBorders := False;
  FExample.Settings.HorizontalMargin := 10;
  FExample.Settings.VerticalMargin := 10;

  FMainPage := TPageControl.Create(Self);
  FMainPage.Parent := ClientPanel;
  FMainPage.Align := alClient;

  FTableTab := TTabSheet.Create(FMainPage);
  FTableTab.PageControl := FMainPage;
  FTableTab.Name := 'tsTableTab';
  FTableTab.Caption := 'Table';
  FTableTab.Enabled := False;
  BuildTableTab;

  FRowTab := TTabSheet.Create(FMainPage);
  FRowTab.PageControl := FMainPage;
  FRowTab.Name := 'tsRowTab';
  FRowTab.Caption := 'Row';
  FRowTab.Enabled := False;
  BuildRowTab;

  FCellTab := TTabSheet.Create(FMainPage);
  FCellTab.PageControl := FMainPage;
  FCellTab.Name := 'tsCellTab';
  FCellTab.Caption := 'Cell';
  FCellTab.Enabled := False;
  BuildCellTab;
End;

Procedure TWPTablePropertiesFrame.BuildTableTab;
Begin
  FgrpBorders := TUixGroupBox.Create(FTableTab);
  FgrpBorders.Parent := FTableTab;
  FgrpBorders.Align := alTop;
  FgrpBorders.Caption := 'Borders';
  FgrpBorders.Height := 160;

  AddLabel(FgrpBorders, 25, 10, 'Border Policy:');

  FTableBorderPolicyCombo := TUixComboBox.Create(FTableTab);
  FTableBorderPolicyCombo.Parent := FgrpBorders;
  FTableBorderPolicyCombo.Top := 20;
  FTableBorderPolicyCombo.Left := 110;
  FTableBorderPolicyCombo.Width := 270;
  FTableBorderPolicyCombo.Style := csDropDownList;
  LoadPolicyList;
  FTableBorderPolicyCombo.OnClick := ChoosePolicy;

  AddLabel(FgrpBorders, 60, 10, 'Inner');
  AddLabel(FgrpBorders, 75, 10, 'Horizontal:');

  FTableBorderCenterHorizontal := TUixButton.Create(FgrpBorders);
  FTableBorderCenterHorizontal.Parent := FgrpBorders;
  FTableBorderCenterHorizontal.Top := 65;
  FTableBorderCenterHorizontal.Left := 80;
  FTableBorderCenterHorizontal.Width := 120;
  FTableBorderCenterHorizontal.Caption := 'Center Horizontal';
  FTableBorderCenterHorizontal.OnClick := ChangeBorder;

  AddLabel(FgrpBorders, 60, 215, 'Inner');
  AddLabel(FgrpBorders, 75, 215, 'Vertical:');

  FTableBorderCenterVertical := TUixButton.Create(FgrpBorders);
  FTableBorderCenterVertical.Parent := FgrpBorders;
  FTableBorderCenterVertical.Top := 65;
  FTableBorderCenterVertical.Left := 270;
  FTableBorderCenterVertical.Width := 120;
  FTableBorderCenterVertical.Caption := 'Center Vertical';
  FTableBorderCenterVertical.OnClick := ChangeBorder;

  AddLabel(FgrpBorders, 110, 10, 'Left:');

  FTableBorderLeft := TUixButton.Create(FgrpBorders);
  FTableBorderLeft.Parent := FgrpBorders;
  FTableBorderLeft.Top := 105;
  FTableBorderLeft.Left := 80;
  FTableBorderLeft.Width := 120;
  FTableBorderLeft.Caption := 'Left Border';
  FTableBorderLeft.OnClick := ChangeBorder;

  AddLabel(FgrpBorders, 110, 215, 'Right:');

  FTableBorderRight := TUixButton.Create(FgrpBorders);
  FTableBorderRight.Parent := FgrpBorders;
  FTableBorderRight.Top := 105;
  FTableBorderRight.Left := 270;
  FTableBorderRight.Width := 120;
  FTableBorderRight.Caption := 'Right Border';
  FTableBorderRight.OnClick := ChangeBorder;

  AddLabel(FgrpBorders, 140, 10, 'Top:');

  FTableBorderTop := TUixButton.Create(FgrpBorders);
  FTableBorderTop.Parent := FgrpBorders;
  FTableBorderTop.Top := 135;
  FTableBorderTop.Left := 80;
  FTableBorderTop.Width := 120;
  FTableBorderTop.Caption := 'Top Border';
  FTableBorderTop.OnClick := ChangeBorder;

  AddLabel(FgrpBorders, 140, 215, 'Bottom:');

  FTableBorderBottom := TUixButton.Create(FgrpBorders);
  FTableBorderBottom.Parent := FgrpBorders;
  FTableBorderBottom.Top := 135;
  FTableBorderBottom.Left := 270;
  FTableBorderBottom.Width := 120;
  FTableBorderBottom.Caption := 'Bottom Border';
  FTableBorderBottom.OnClick := ChangeBorder;

  FgrpTableMisc := TUixGroupBox.Create(FTableTab);
  FgrpTableMisc.Parent := FTableTab;
  FgrpTableMisc.Align := alClient;
  FgrpTableMisc.Caption := 'Misc.';

  AddLabel(FgrpTableMisc, 23, 10, 'Left and Right');

  FHorizontalMarginSpinEdit := TUixEdit.Create(FTableTab);
  FHorizontalMarginSpinEdit.Parent := FgrpTableMisc;
  FHorizontalMarginSpinEdit.Top := 18;
  FHorizontalMarginSpinEdit.Left := 95;
  FHorizontalMarginSpinEdit.Width := 70;
  FHorizontalMarginSpinEdit.OnChange := ChangeHorizontal;

  AddLabel(FgrpTableMisc, 23, 180, 'Top and Bottom');

  FVerticalMarginSpinEdit := TUixEdit.Create(FTableTab);
  FVerticalMarginSpinEdit.Parent := FgrpTableMisc;
  FVerticalMarginSpinEdit.Top := 18;
  FVerticalMarginSpinEdit.Left := 278;
  FVerticalMarginSpinEdit.Width := 60;
  FVerticalMarginSpinEdit.OnChange := ChangeVertical;

  AddLabel(FgrpTableMisc, 43, 10, 'Table background:');

  FTableBackground := TUixHTMLColourComboBox.Create(FgrpTableMisc);
  FTableBackground.Parent := FgrpTableMisc;
  FTableBackground.Left := 158;
  FTableBackground.Top := 43;
  FTableBackground.Width := 180;
  FTableBackground.OnChange := ChangeBackgroundColour;

  FExpandLastColumn := TUixCheckBox.Create(FgrpTableMisc);
  FExpandLastColumn.Parent := FgrpTableMisc;
  FExpandLastColumn.Left := 10;
  FExpandLastColumn.Top := 65;
  FExpandLastColumn.Width := 260;
  FExpandLastColumn.Caption := 'Expand Last Column To Fill Space';
  FExpandLastColumn.OnChange := ChangeExpandLastColumn;
End;


Procedure TWPTablePropertiesFrame.BuildRowTab;
Begin
  FHeaderCheckbox := TUixCheckBox.Create(FRowTab);
  FHeaderCheckbox.Parent := FRowTab;
  FHeaderCheckbox.Left := 10;
  FHeaderCheckbox.Top := 15;
  FHeaderCheckbox.Width := 200;
  FHeaderCheckbox.Caption := 'Header (at top of every page)';
  FHeaderCheckbox.OnChange := ChangeRowHeader;

  FBreakBeforeCheckbox := TUixCheckBox.Create(FRowTab);
  FBreakBeforeCheckbox.Parent := FRowTab;
  FBreakBeforeCheckbox.Left := 10;
  FBreakBeforeCheckbox.Top := 35;
  FBreakBeforeCheckbox.Width := 290;
  FBreakBeforeCheckbox.Caption := 'BreakBefore (at top of every page)';
  FBreakBeforeCheckbox.OnChange := ChangeRowBreakBefore;

  AddLabel(FRowTab, 60, 10, 'Row background:');

  FRowBackground := TUixHTMLColourComboBox.Create(FRowTab);
  FRowBackground.Parent := FRowTab;
  FRowBackground.Left := 140;
  FRowBackground.Top := 60;
  FRowBackground.Width := 180;
  FRowBackground.OnChange := ChangeRowBackgroundColour;

  AddLabel(FRowTab, 85, 10, 'Lower padding size:');

  FLowerPaddingSizeEdit := TUixEdit.Create(FRowTab);
  FLowerPaddingSizeEdit.Parent := FRowTab;
  FLowerPaddingSizeEdit.Left := 140;
  FLowerPaddingSizeEdit.Top := 85;
  FLowerPaddingSizeEdit.Width := 70;
  FLowerPaddingSizeEdit.OnChange := ChangeLowerPaddingSize;

  AddLabel(FRowTab, 110, 10, 'Lower padding colour:');

  FLowerPaddingColourEdit := TUixHTMLColourComboBox.Create(FRowTab);
  FLowerPaddingColourEdit.Parent := FRowTab;
  FLowerPaddingColourEdit.Left := 140;
  FLowerPaddingColourEdit.Top := 110;
  FLowerPaddingColourEdit.Width := 180;
  FLowerPaddingColourEdit.OnChange := ChangeLowerPaddingColor;
End;


Procedure TWPTablePropertiesFrame.BuildCellTab;
Begin
  FgrpCellBorders := TUixGroupBox.Create(FCellTab);
  FgrpCellBorders.Parent := FCellTab;
  FgrpCellBorders.Align := alTop;
  FgrpCellBorders.Caption := 'Borders';
  FgrpCellBorders.Height := 70;
  FgrpCellBorders.Left := 10;

  AddLabel(FgrpCellBorders, 20, 20, 'Left:');

  FCellBorderLeft := TUixButton.Create(FgrpCellBorders);
  FCellBorderLeft.Parent := FgrpCellBorders;
  FCellBorderLeft.Top := 20;
  FCellBorderLeft.Left := 60;
  FCellBorderLeft.Width := 120;
  FCellBorderLeft.Caption := 'Left Border';
  FCellBorderLeft.OnClick := ChangeBorder;

  AddLabel(FgrpCellBorders, 20, 210, 'Right:');

  FCellBorderRight := TUixButton.Create(FgrpCellBorders);
  FCellBorderRight.Parent := FgrpCellBorders;
  FCellBorderRight.Top := 20;
  FCellBorderRight.Left := 260;
  FCellBorderRight.Width := 120;
  FCellBorderRight.Caption := 'Right Border';
  FCellBorderRight.OnClick := ChangeBorder;

  AddLabel(FgrpCellBorders, 45, 20, 'Top:');

  FCellBorderTop := TUixButton.Create(FgrpCellBorders);
  FCellBorderTop.Parent := FgrpCellBorders;
  FCellBorderTop.Top := 45;
  FCellBorderTop.Left := 60;
  FCellBorderTop.Width := 120;
  FCellBorderTop.Caption := 'Top Border';
  FCellBorderTop.OnClick := ChangeBorder;

  AddLabel(FgrpCellBorders, 45, 210, 'Bottom:');

  FCellBorderBottom := TUixButton.Create(FgrpCellBorders);
  FCellBorderBottom.Parent := FgrpCellBorders;
  FCellBorderBottom.Top := 45;
  FCellBorderBottom.Left := 260;
  FCellBorderBottom.Width := 120;
  FCellBorderBottom.Caption := 'Bottom Border';
  FCellBorderBottom.OnClick := ChangeBorder;

  FgrpMargin := TUixGroupBox.Create(FCellTab);
  FgrpMargin.Parent := FCellTab;
  FgrpMargin.Top := 75;
  FgrpMargin.Left := 10;
  FgrpMargin.Caption := 'Margin';
  FgrpMargin.Height := 70;
  FgrpMargin.Width := FCellTab.Width - 20;

  AddLabel(FgrpMargin, 20, 10, 'Left:');

  FCellMarginLeftEdit := TUixEdit.Create(FgrpMargin);
  FCellMarginLeftEdit.Parent := FgrpMargin;
  FCellMarginLeftEdit.Top := 20;
  FCellMarginLeftEdit.Left := 50;
  FCellMarginLeftEdit.Width := 120;
  FCellMarginLeftEdit.OnChange := ChangeCellMargin;

  AddLabel(FgrpMargin, 20, 200, 'Right:');

  FCellMarginRightEdit := TUixEdit.Create(FgrpMargin);
  FCellMarginRightEdit.Parent := FgrpMargin;
  FCellMarginRightEdit.Top := 20;
  FCellMarginRightEdit.Left := 250;
  FCellMarginRightEdit.Width := 120;
  FCellMarginRightEdit.OnChange := ChangeCellMargin;

  AddLabel(FgrpMargin, 45, 10, 'Top:');

  FCellMarginTopEdit := TUixEdit.Create(FgrpMargin);
  FCellMarginTopEdit.Parent := FgrpMargin;
  FCellMarginTopEdit.Top := 45;
  FCellMarginTopEdit.Left := 50;
  FCellMarginTopEdit.Width := 120;
  FCellMarginTopEdit.OnChange := ChangeCellMargin;

  AddLabel(FgrpMargin, 45, 200, 'Bottom:');

  FCellMarginBottomEdit := TUixEdit.Create(FgrpMargin);
  FCellMarginBottomEdit.Parent := FgrpMargin;
  FCellMarginBottomEdit.Top := 45;
  FCellMarginBottomEdit.Left := 250;
  FCellMarginBottomEdit.Width := 120;
  FCellMarginBottomEdit.OnChange := ChangeCellMargin;

  FgrpCellMisc := TUixGroupBox.Create(FCellTab);
  FgrpCellMisc.Parent := FCellTab;
  FgrpCellMisc.Top := 150;
  FgrpCellMisc.Left := 10;
  FgrpCellMisc.Caption := 'Misc.';
  FgrpCellMisc.Height := 120;
  FgrpCellMisc.Width := FCellTab.Width - 20;

  AddLabel(FgrpCellMisc, 20, 10, 'Vertical Alignment:');

  FVerticalAlignmentCombo := TUixComboBox.Create(FgrpCellMisc);
  FVerticalAlignmentCombo.Parent := FgrpCellMisc;
  FVerticalAlignmentCombo.Top := 20;
  FVerticalAlignmentCombo.Left := 130;
  FVerticalAlignmentCombo.Width := 150;
  FVerticalAlignmentCombo.Style := csDropDownList;
  LoadCellVerticalAlignmentList;
  FVerticalAlignmentCombo.OnChange := ChangeCellVerticalAlignment;

  AddLabel(FgrpCellMisc, 45, 10, 'Cell Width:');

  FCellWidthEdit := TUixEdit.Create(FgrpCellMisc);
  FCellWidthEdit.Parent := FgrpCellMisc;
  FCellWidthEdit.Top := 45;
  FCellWidthEdit.Left := 130;
  FCellWidthEdit.Width := 150;
  FCellWidthEdit.OnChange := ChangeCellWidth;

  AddLabel(FgrpCellMisc, 70, 10, 'Cell Background:');

  FCellBackground := TUixHTMLColourComboBox.Create(FgrpCellMisc);
  FCellBackground.Parent := FgrpCellMisc;
  FCellBackground.Top := 70;
  FCellBackground.Left := 130;
  FCellBackground.Width := 150;
  FCellBackground.OnChange := ChangeCellBackground;
End;


Procedure TWPTablePropertiesFrame.BuildExampleTable(oTable : TWPWorkingDocumentTableStartPiece; iRowStart, iRowEnd, iCellStart, iCellEnd  : Integer);
Var
  oBuilder              : TWPDocumentBuilder;
  iLoop, iLoop2         : Integer;
  oRow                  : TWPWorkingDocumentTableRowStartPiece;
  bInSelectedRows       : Boolean;
Begin
  oBuilder := TWPDocumentBuilder.Create;
  Try
    oBuilder.Document := TWPDocument.Create;
    oBuilder.Start;
    oBuilder.StartTable;
    For iLoop := 0 To oTable.Rows.Count - 1 Do
    Begin
      oRow := oTable.Rows[iLoop];
      bInSelectedRows := (iLoop >= iRowStart) And (iLoop <= iRowEnd);
      oBuilder.StartTableRow;
      For iLoop2 := 0 To oRow.Cells.Count - 1 Do
      Begin
        oBuilder.StartTableCell;
        If bInSelectedRows Then
        Begin
          oBuilder.StartParagraph;
          If (iRowStart = iRowEnd) Then
            Begin
              If (iLoop2 >= iCellStart) And (iLoop2 <= iCellEnd) Then oBuilder.AddTextPlain('(...)');
            End
          Else If iLoop = iRowStart Then
            Begin
              If iLoop2 >= iCellStart Then oBuilder.AddTextPlain('(...)');
            End
          Else If iLoop = iRowEnd Then
            Begin
              If iLoop2 <= iCellEnd Then oBuilder.AddTextPlain('(...)');
            End
          Else oBuilder.AddTextPlain('C');
          oBuilder.EndParagraph;
        End;
        oBuilder.EndTableCell;
      End;
      oBuilder.EndTableRow;
    End;
    oBuilder.EndTable;
    oBuilder.StartParagraph;
    oBuilder.EndParagraph;
    oBuilder.Stop;
    FExample.DocumentHandler.LoadDocument(oBuilder.Document);
  Finally
    oBuilder.Free;
  End;

  // mirror selection and styles
  iLoop := 0;
  While (iLoop < FExample.Document.Pieces.Count - 1) Do
  Begin
    If FExample.Document.Pieces[iLoop].PieceType = ptTableStart Then
    Begin
      MirrorStyle(oTable, TWPWorkingDocumentTableStartPiece(FExample.Document.Pieces[iLoop]));
      CalcSelection(TWPWorkingDocumentTableStartPiece(FExample.Document.Pieces[iLoop]), iRowStart, iRowEnd, iCellStart, iCellEnd);
      FExample.PrimaryRange.SelectRange(FExSelectionStart, FExSelectionEnd);
      Break;
    End;
    Inc(iLoop);
  End;
End;

Procedure TWPTablePropertiesFrame.CalcSelection(oTable : TWPWorkingDocumentTableStartPiece; iRowStart, iRowEnd, iCellStart, iCellEnd: Integer);
Var
  oStartPiece : TWPWorkingDocumentTableCellStartPiece;
  oStopPiece: TWPWorkingDocumentPiece;
Begin
  // selection start
  oStartPiece := oTable.Rows[iRowStart].Cells[iCellStart];
  FExSelectionStart := oStartPiece.Metrics.Position + oStartPiece.Metrics.CharCount;

  // selection end
  oStartPiece := oTable.Rows[iRowEnd].Cells[iCellEnd];
  oStopPiece := FExample.Document.Pieces[FExample.Document.Pieces.IndexByReference(oStartPiece) + 1];
  FExSelectionEnd := oStopPiece.Metrics.Position + oStopPiece.Metrics.CharCount;
End;

Procedure TWPTablePropertiesFrame.MirrorStyle(oSrc, oDest: TWPWorkingDocumentTableStartPiece);
Var
  iLoop, iLoop2         : Integer;
  oSrcRow, oDestRow     : TWPWorkingDocumentTableRowStartPiece;
Begin
  oDest.ApplyProperties(oSrc);
  For iLoop := 0 To oDest.Rows.Count - 1 Do
  Begin
    oSrcRow := oSrc.Rows[iLoop];
    oDestRow := oDest.Rows[iLoop];
    oDestRow.ApplyProperties(oSrcRow);
    For iLoop2 := 0 To oDestRow.Cells.Count - 1 Do
      oDestRow.Cells[iLoop2].ApplyProperties(oSrcRow.Cells[iLoop2]);
  End;
End;

Procedure TWPTablePropertiesFrame.LoadPolicyList;
Var
  aLoop : TWPWorkingDocumentTableBorderPolicy;
Begin
  FTableBorderPolicyCombo.Items.Clear;
  For aLoop := Low(TWPWorkingDocumentTableBorderPolicy) To High(TWPWorkingDocumentTableBorderPolicy) Do
    FTableBorderPolicyCombo.Items.Add(NAMES_TWPWorkingDocumentTableBORDERPOLICY[aLoop]);
End;

Function TWPTablePropertiesFrame.GetTable : TWPWorkingDocumentTableStartPiece;
Begin
  Assert(Invariants('GetTable', FTable, TWPWorkingDocumentTableStartPiece, 'Table'));
  Result := FTable;
End;


Procedure TWPTablePropertiesFrame.SetTable(Const Value : TWPWorkingDocumentTableStartPiece);
Begin
  FTable.Free;
  FTable := Value;
  FTableTab.Enabled := Assigned(FTable);
End;

Function TWPTablePropertiesFrame.GetTableRow : TWPWorkingDocumentTableRowStartPiece;
Begin
  Assert(Invariants('GetTableRow', FTableRow, TWPWorkingDocumentTableRowStartPiece, 'Row'));
  Result := FTableRow;
End;

Procedure TWPTablePropertiesFrame.SetTableRow(Const Value : TWPWorkingDocumentTableRowStartPiece);
Begin
  FTableRow.Free;
  FTableRow := Value;
  FRowTab.Enabled := Assigned(FTableRow);
End;

Function TWPTablePropertiesFrame.GetTableCell: TWPWorkingDocumentTableCellStartPiece;
Begin
  Assert(Invariants('GetTableCell', FTableCell, TWPWorkingDocumentTableCellStartPiece, 'Cell'));
  Result := FTableCell;
End;

Procedure TWPTablePropertiesFrame.SetTableCell(Const Value : TWPWorkingDocumentTableCellStartPiece);
Begin
  FTableCell.Free;
  FTableCell := Value;
  FCellTab.Enabled := Assigned(FTableCell);
End;

Function TWPTablePropertiesFrame.DesiredHeight: Integer;
Begin
  Result := 400;
End;

Function TWPTablePropertiesFrame.DesiredWidth: Integer;
Begin
  Result := 400;
End;

Procedure TWPTablePropertiesFrame.Initialise;
Begin
  Inherited;

  BuildForm;
End;

Procedure TWPTablePropertiesFrame.Finalise;
Begin
  FTable.Free;
  FTableRow.Free;
  FTableCell.Free;

  Inherited;
End;


Procedure TWPTablePropertiesFrame.Restore;
Begin
  Inherited;

  // table properties
  FTableBorderPolicyCombo.ItemIndex := ord(FTable.BorderPolicy);
  FTableBorderLeft.Caption := FTable.LeftBorder.PropertyDescription;
  FTableBorderRight.Caption := FTable.RightBorder.PropertyDescription;
  FTableBorderTop.Caption := FTable.TopBorder.PropertyDescription;
  FTableBorderBottom.Caption := FTable.BottomBorder.PropertyDescription;
  FTableBorderCenterHorizontal.Caption := FTable.CenterHorizontalBorder.PropertyDescription;
  FTableBorderCenterVertical.Caption := FTable.CenterVerticalBorder.PropertyDescription;
  FTableBorderLeft.Tag := Integer(FTable.LeftBorder);
  FTableBorderRight.Tag := Integer(FTable.RightBorder);
  FTableBorderTop.Tag := Integer(FTable.TopBorder);
  FTableBorderBottom.Tag := Integer(FTable.BottomBorder);
  FTableBorderCenterHorizontal.Tag := Integer(FTable.CenterHorizontalBorder);
  FTableBorderCenterVertical.Tag := Integer(FTable.CenterVerticalBorder);
  FHorizontalMarginSpinEdit.Value := WordToText(FTable.HorizontalMargin);
  FVerticalMarginSpinEdit.Value := WordToText(FTable.VerticalMargin);
  FTableBackground.Value := FTable.Background;
  FExpandLastColumn.Value := FTable.ExpandLastColumn;

  // row properties
  FHeaderCheckbox.Enabled := (FTableRow.State In [tisFirst, tisOnly]);
  FHeaderCheckbox.Checked := FTableRow.Header;
  FBreakBeforeCheckbox.Checked := FTableRow.BreakBefore;
  FRowBackground.Value := FTableRow.Background;
  FLowerPaddingSizeEdit.Value := WordToText(FTableRow.LowerPaddingSize);
  FLowerPaddingColourEdit.Value := FTableRow.LowerPaddingColour ;

  // cell properties
  FCellBorderLeft.Caption := FTableCell.LeftBorder.PropertyDescription;
  FCellBorderRight.Caption := FTableCell.RightBorder.PropertyDescription;
  FCellBorderTop.Caption := FTableCell.TopBorder.PropertyDescription;
  FCellBorderBottom.Caption := FTableCell.BottomBorder.PropertyDescription;
  FCellBorderLeft.Tag := Integer(FTableCell.LeftBorder);
  FCellBorderRight.Tag := Integer(FTableCell.RightBorder);
  FCellBorderTop.Tag := Integer(FTableCell.TopBorder);
  FCellBorderBottom.Tag := Integer(FTableCell.BottomBorder);
  FCellWidthEdit.Value := FloatToStr(FTableCell.Width);
  FCellBackground.Value := FTableCell.Background;
  FCellMarginLeftEdit.Value := WordToText(FTableCell.MarginLeft);
  FCellMarginRightEdit.Value := WordToText(FTableCell.MarginRight);
  FCellMarginTopEdit.Value := WordToText(FTableCell.MarginTop);
  FCellMarginBottomEdit.Value := WordToText(FTableCell.MarginBottom);
  FVerticalAlignmentCombo.ItemIndex := ord(FTableCell.VerticalAlignment);
End;


Function TWPTablePropertiesFrame.CanAccept : Boolean;
Var
  iTemp : Real;
Begin
  Result := Inherited CanAccept;

  // table can accept
  Result := Result And ValidatePositiveInt(FHorizontalMarginSpinEdit, FHorizontalMarginSpinEdit.Value, 'Table horizontal margin');
  Result := Result And ValidatePositiveInt(FVerticalMarginSpinEdit, FVerticalMarginSpinEdit.Value, 'Table vertical margin');

  // row can accept
  Result := Result And ValidatePositiveInt(FLowerPaddingSizeEdit, FLowerPaddingSizeEdit.Value, 'Row lower padding');

  // cell can accept
  Result := Result And ValidatePositiveInt(FCellMarginLeftEdit, FCellMarginLeftEdit.Value, 'Cell left margin');
  Result := Result And ValidatePositiveInt(FCellMarginRightEdit, FCellMarginRightEdit.Value, 'Cell right margin');
  Result := Result And ValidatePositiveInt(FCellMarginTopEdit, FCellMarginTopEdit.Value, 'Cell top margin');
  Result := Result And ValidatePositiveInt(FCellMarginBottomEdit, FCellMarginBottomEdit.Value, 'Cell bottom margin');

  If Not StringIsReal(FCellWidthEdit.Value) Then
    Begin
      Self.Invalid(FCellWidthEdit, 'Invalid value', '''' + FCellWidthEdit.Value + ''' is not a real');
      Result := False;
    End
  Else
    Begin
      iTemp := StringToReal(FCellWidthEdit.Value);
      If (iTemp < 0) Or (iTemp > 1) Then
      Begin
        Self.Invalid(FCellWidthEdit, 'Invalid value', 'Cell width must be in the range of [0, 1], not ''' + FCellWidthEdit.Value + '''');
        Result := False;
      End;
    End;
End;


Procedure TWPTablePropertiesFrame.Accept;
Begin
  Inherited;
End;


Procedure TWPTablePropertiesFrame.ChangeExampleProperty(oTable: TWPWorkingDocumentTableStartPiece; oRow: TWPWorkingDocumentTableRowStartPiece; oCell: TWPWorkingDocumentTableCellStartPiece);
Begin
  FExample.PrimaryRange.SetTableProperties(oTable, oRow, oCell);

  // redraw the whole table before reset the selected range
  FExample.PrimaryRange.SelectTable;
  FExample.PrimaryRange.ChangeState;
  FExample.PrimaryRange.SelectRange(FExSelectionStart, FExSelectionEnd);
End;


Procedure TWPTablePropertiesFrame.ChoosePolicy(oSender : TObject);
Begin
  FTable.BorderPolicy := TWPWorkingDocumentTableBorderPolicy(FTableBorderPolicyCombo.ItemIndex);
  FTable.ApplyPolicy;
  ChangeExampleProperty(FTable, Nil, Nil);

  // update border details to reflect the new policy
  FTableBorderLeft.Caption := FTable.LeftBorder.PropertyDescription;
  FTableBorderRight.Caption := FTable.RightBorder.PropertyDescription;
  FTableBorderTop.Caption := FTable.TopBorder.PropertyDescription;
  FTableBorderBottom.Caption := FTable.BottomBorder.PropertyDescription;
  FTableBorderCenterHorizontal.Caption := FTable.CenterHorizontalBorder.PropertyDescription;
  FTableBorderCenterVertical.Caption := FTable.CenterVerticalBorder.PropertyDescription;
End;


Procedure TWPTablePropertiesFrame.ChangeHorizontal(oSender : TObject);
Begin
  FTable.HorizontalMargin := TextToWord(FHorizontalMarginSpinEdit.Text);
  ChangeExampleProperty(FTable, Nil, Nil);
End;


Procedure TWPTablePropertiesFrame.ChangeVertical(oSender : TObject);
Begin
  FTable.VerticalMargin := TextToWord(FVerticalMarginSpinEdit.Text);
  ChangeExampleProperty(FTable, Nil, Nil);
End;

Procedure TWPTablePropertiesFrame.ChangeLowerPaddingColor(oSender: TObject);
Begin
  FTableRow.LowerPaddingColour := FLowerPaddingColourEdit.Value;
  ChangeExampleProperty(Nil, FTableRow, Nil);
End;


Procedure TWPTablePropertiesFrame.ChangeLowerPaddingSize(oSender: TObject);
Begin
  FTableRow.LowerPaddingSize := TextToWord(FLowerPaddingSizeEdit.Value);
  ChangeExampleProperty(Nil, FTableRow, Nil);
End;

Procedure TWPTablePropertiesFrame.ChangeBackgroundColour(oSender: TObject);
Begin
  FTable.Background := FTableBackground.Value;
  ChangeExampleProperty(FTable, Nil, Nil);
End;

Procedure TWPTablePropertiesFrame.ChangeRowBackgroundColour(oSender: TObject);
Begin
  FTableRow.Background := FRowBackground.Value;
  ChangeExampleProperty(Nil, FTableRow, Nil);
End;

Procedure TWPTablePropertiesFrame.ChangeExpandLastColumn(oSender: TObject);
Begin
  FTable.ExpandLastColumn := FExpandLastColumn.Value;
  ChangeExampleProperty(FTable, NIl, Nil);
End;

Procedure TWPTablePropertiesFrame.ChangeRowHeader(oSender: TObject);
Begin
  FTableRow.Header := FHeaderCheckbox.Value;
  ChangeExampleProperty(Nil, FTableRow, Nil);
End;

Procedure TWPTablePropertiesFrame.ChangeRowBreakBefore(oSender: TObject);
Begin
  FTableRow.BreakBefore := FBreakBeforeCheckbox.Value;
  ChangeExampleProperty(Nil, FTableRow, Nil);
End;

Procedure TWPTablePropertiesFrame.ChangeCellBackground(oSender: TObject);
Begin
  FTableCell.Background := FCellBackground.Value;
  ChangeExampleProperty(Nil, Nil, FTableCell);
End;


Procedure TWPTablePropertiesFrame.ChangeCellMargin(oSender: TObject);
Var
  iTemp : Integer;
  oEditSender: TUixEdit;
Begin
  Assert(oSender.ClassNameIs('TUixEdit'), 'Unexpected event source');
  oEditSender := oSender As TUixEdit;
  iTemp := TextToWord(oEditSender.Value);
  If (iTemp >= 0) Then
  Begin
    If oSender = FCellMarginLeftEdit Then
      FTableCell.MarginLeft := iTemp
    Else If oSender = FCellMarginRightEdit Then
      FTableCell.MarginRight := iTemp
    Else If oSender = FCellMarginBottomEdit Then
      FTableCell.MarginBottom := iTemp
    Else
      FTableCell.MarginTop := iTemp;
    ChangeExampleProperty(Nil, Nil, FTableCell);
  End;
End;

Procedure TWPTablePropertiesFrame.ChangeCellVerticalAlignment(oSender: TObject);
Begin
  If FVerticalAlignmentCombo.Text <> '' Then
  Begin
    FTableCell.VerticalAlignment := TWordProcessorVerticalAlignment(FVerticalAlignmentCombo.Items.IndexOf(FVerticalAlignmentCombo.Text));
    ChangeExampleProperty(Nil, Nil, FTableCell);
  End;
End;

Procedure TWPTablePropertiesFrame.ChangeCellWidth(oSender: TObject);
Var
  iTemp: Real;
Begin
  If StringIsReal(FCellWidthEdit.Value) Then
  Begin
    iTemp := StringToReal(FCellWidthEdit.Value);
    If (iTemp >= 0) And (iTemp <= 1) Then
    Begin
      FTableCell.Width :=  iTemp;
      ChangeExampleProperty(Nil, Nil, FTableCell);
    End;
  End;
End;

Function TWPTablePropertiesFrame.ValidatePositiveInt(oControl: TWinControl; Const sValue, sName: String): Boolean;
Begin
  Result := True;
  If (sValue <> '(default)') And (Not StringIsInteger32(sValue))  Then
    Begin
      Self.Invalid(oControl, 'Invalid value', '''' + sValue + ''' is not an integer');
      Result := False;
    End
  Else If (sValue <> '(default)') And (StrToInt(sValue) < 0) Then
    Begin
      Self.Invalid(oControl, 'Invalid value', sName + ' must be 0 or a positive integer, not ''' + sValue + '''');
      Result := False;
    End;
End;

Procedure TWPTablePropertiesFrame.LoadCellVerticalAlignmentList;
Var
  aLoop : TWordProcessorVerticalAlignment;
Begin
  FVerticalAlignmentCombo.Items.Clear;
  For aLoop := Low(TWordProcessorVerticalAlignment) To High(TWordProcessorVerticalAlignment) Do
    FVerticalAlignmentCombo.Items.Add(NAMES_WORDPROCESSORVERTICALALIGNMENT[aLoop]);
End;

Procedure TWPTablePropertiesFrame.ChangeBorder(oSender: TObject);
Var
  oSource: TButton;
  oDialog: TWPTableBorderDialog;
  oBorder: TWPBorder;
Begin
  oSource := oSender As TButton;
  Assert(oSource <> Nil, 'Expected event from a button');
  oBorder := TWPBorder(oSource.Tag);

  oDialog := TWPTableBorderDialog.Create(Self);
  oDialog.Border := oBorder.Clone;
  If oDialog.Execute Then
  Begin
    // change table border policy to custom
    If FTableBorderPolicyCombo.ItemIndex <> ord(tbpCustom) Then
    Begin
      FTableBorderPolicyCombo.ItemIndex := ord(tbpCustom);
      //ChoosePolicy(oSender);
    End;
    FTableBorderPolicyCombo.ItemIndex := ord(tbpCustom);
    ChoosePolicy(oSender);

    // update
    oBorder.Assign(oDialog.Border);
    oSource.Caption := oBorder.PropertyDescription;
    If (oSource = FCellBorderLeft) Or (oSource = FCellBorderRight)
      Or (oSource = FCellBorderTop) Or (oSource = FCellBorderBottom) Then
      ChangeExampleProperty(Nil, Nil, FTableCell)
    Else
      ChangeExampleProperty(FTable, Nil, Nil);
  End;
End;

{ TWPTableBorderDialog }

Function TWPTableBorderDialog.DialogCaption: String;
Begin
  Result := 'Table Border';
End;

Function TWPTableBorderDialog.CanAccept: Boolean;
Begin
  Result := Frame.CanAccept;
End;

Function TWPTableBorderDialog.FrameClass: TWPFrameClass;
Begin
  Result := TWPTableBorderFrame;
End;

Function TWPTableBorderDialog.GetBorder: TWPBorder;
Begin
  Result := Frame.Border;
End;

Procedure TWPTableBorderDialog.SetBorder(const oBorder: TWPBorder);
Begin
  Frame.Border := oBorder;
End;

Function TWPTableBorderDialog.GetFrame: TWPTableBorderFrame;
Begin
  Result := TWPTableBorderFrame(Inherited Frame);
End;


{ TWPTableBorderFrame }

Procedure TWPTableBorderFrame.Initialise;
Begin
  Inherited;

  BuildForm;
End;

Procedure TWPTableBorderFrame.Finalise;
Begin
  FBorder.Free;

  Inherited;
End;

Function TWPTableBorderFrame.GetBorder: TWPBorder;
Begin
  Assert(Invariants('GetBorder', FBorder, TWPBorder, 'Border'));
  Result := FBorder;
End;

Procedure TWPTableBorderFrame.SetBorder(const oBorder: TWPBorder);
Begin
  FBorder.Free;
  FBorder := oBorder;
End;

Procedure TWPTableBorderFrame.Accept;
Begin
  Inherited;

  FBorder.Width := TextToWord(FWidthEdit.Value);
  FBorder.Style := TFslPenStyle(FPenStyleCombo.Value);
  FBorder.LowOuterlimit := TextToWord(FLowOuterLimitEdit.Value);
  FBorder.HighOuterlimit := TextToWord(FHighOuterLimitEdit.Value);
  FBorder.Colour := FColourEdit.Value;
  FBorder.OuterColour := FOuterColourEdit.Value;
  FBorder.OuterColour2 := FOuterColour2Edit.Value;
End;

Function TWPTableBorderFrame.CanAccept: Boolean;
Begin
  Result := Inherited CanAccept;
End;

Procedure TWPTableBorderFrame.Restore;
Var
  oStyle : TPredefinedBorderStyles;
Begin
  Inherited;

  oStyle := FBorder.BorderStyle;
  FWidthEdit.Value := WordToText(FBorder.Width);
  FPenStyleCombo.ItemIndex := ord(FBorder.Style);
  FLowOuterLimitEdit.Value := WordToText(FBorder.LowOuterlimit);
  FHighOuterLimitEdit.Value := WordToText(FBorder.HighOuterLimit);
  FColourEdit.Value := FBorder.Colour;
  FOuterColourEdit.Value := FBorder.OuterColour;
  FOuterColour2Edit.Value := FBorder.OuterColour2;

  FBorderStyleCombo.ItemIndex := ord(oStyle);
  EnableControlsToStyle(oStyle);
End;

Function TWPTableBorderFrame.DesiredHeight: Integer;
Begin
  Result := 350;
End;

Function TWPTableBorderFrame.DesiredWidth: Integer;
Begin
  Result := 300;
End;

Procedure TWPTableBorderFrame.BuildForm;
Begin
  AddLabel(ClientPanel, 20, 10, 'Border Style:');

  FBorderStyleCombo := TUixComboBox.Create(ClientPanel);
  FBorderStyleCombo.Parent := ClientPanel;
  FBorderStyleCombo.Left := 120;
  FBorderStyleCombo.Top := 20;
  FBorderStyleCombo.Width := 170;
  FBorderStyleCombo.Style := csDropDownList;
  LoadBorderStyle;
  FBorderStyleCombo.OnChange := OnBorderStyleChange;

  FgrpStandard := TUixGroupBox.Create(ClientPanel);
  FgrpStandard.Parent := ClientPanel;
  FgrpStandard.Top := 45;
  FgrpStandard.Width := DesiredWidth;
  FgrpStandard.Height := 120;
  FgrpStandard.Caption := 'Standard Properties';
  BuildStandardPropertyInputs;

  FgrpFancy := TUixGroupBox.Create(ClientPanel);
  FgrpFancy.Parent := ClientPanel;
  FgrpFancy.Top := 170;
  FgrpFancy.Width := FgrpStandard.Width;
  FgrpFancy.Height := DesiredHeight - 180;
  FgrpFancy.Caption := 'Fancy Properties';
  BuildFancyPropertyInputs;
End;

Procedure TWPTableBorderFrame.BuildStandardPropertyInputs;
Begin
  AddLabel(FgrpStandard, 30, 10, 'Width:');

  FWidthEdit := TUixEdit.Create(FgrpStandard);
  FWidthEdit.Parent := FgrpStandard;
  FWidthEdit.Left := 115;
  FWidthEdit.Top := 35;
  FWidthEdit.Width := 175;
  FWidthEdit.OnChange := OnIndividualBorderChange;

  AddLabel(FgrpStandard, 60, 10, 'Penstyle:');

  FPenStyleCombo := TUixComboBox.Create(FgrpStandard);
  FPenStyleCombo.Parent := FgrpStandard;
  FPenStyleCombo.Left := 115;
  FPenStyleCombo.Top := 65;
  FPenStyleCombo.Width := 175;
  FPenStyleCombo.Style := csDropDownList;
  LoadPenStyle;
  FPenStyleCombo.OnChange := OnIndividualBorderChange;

  AddLabel(FgrpStandard, 90, 10, 'Colour:');

  FColourEdit := TUixHTMLColourComboBox.Create(FgrpStandard);
  FColourEdit.Parent := FgrpStandard;
  FColourEdit.Left := 115;
  FColourEdit.Top := 95;
  FColourEdit.Width := 175;
  FColourEdit.OnChange := OnIndividualBorderChange;
End;

Procedure TWPTableBorderFrame.BuildFancyPropertyInputs;
Begin
  AddLabel(FgrpFancy, 35, 10, 'Low Outer Limit:');

  FLowOuterLimitEdit := TUixEdit.Create(FgrpFancy);
  FLowOuterLimitEdit.Parent := FgrpFancy;
  FLowOuterLimitEdit.Left := 115;
  FLowOuterLimitEdit.Top := 30;
  FLowOuterLimitEdit.Width := 175;

  AddLabel(FgrpFancy, 65, 10, 'High Outer Limit:');

  FHighOuterLimitEdit := TUixEdit.Create(FgrpFancy);
  FHighOuterLimitEdit.Parent := FgrpFancy;
  FHighOuterLimitEdit.Left := 115;
  FHighOuterLimitEdit.Top := 60;
  FHighOuterLimitEdit.Width := 175;

  AddLabel(FgrpFancy, 95, 10, 'Outer Colour:');

  FOuterColourEdit := TUixHTMLColourComboBox.Create(FgrpFancy);
  FOuterColourEdit.Parent := FgrpFancy;
  FOuterColourEdit.Left := 115;
  FOuterColourEdit.Top := 90;
  FOuterColourEdit.Width := 175;

  AddLabel(FgrpFancy, 125, 10, 'Outer Colour 2:');

  FOuterColour2Edit := TUixHTMLColourComboBox.Create(FgrpFancy);
  FOuterColour2Edit.Parent := FgrpFancy;
  FOuterColour2Edit.Left := 115;
  FOuterColour2Edit.Top := 120;
  FOuterColour2Edit.Width := 175;
End;

Procedure TWPTableBorderFrame.LoadBorderStyle;
Var
  oStyle: TPredefinedBorderStyles;
Begin
  FBorderStyleCombo.ClearValues;
  For oStyle := Low(TPredefinedBorderStyles) To High(TPredefinedBorderStyles) Do
    FBorderStyleCombo.Items.Add(PREDEFINED_BODERSTYLE_NAMES[oStyle]);
End;

Procedure TWPTableBorderFrame.LoadPenStyle;
Var
  oStyle: TFslPenStyle;
Begin
  FPenStyleCombo.ClearValues;
  For oStyle := Low(TFslPenStyle) To High(TFslPenStyle) Do
    FPenStyleCombo.Items.Add(ADVPENSTYLE_NAMES[oStyle]);
End;

Procedure TWPTableBorderFrame.EnableControlsToStyle(oStyle : TPredefinedBorderStyles);
Var
  iLoop: Integer;
Begin
  If oStyle = pbsNone Then
    Begin
    FgrpStandard.Enabled := False;
    For iLoop := 0 To FgrpStandard.ControlCount - 1 Do
      FgrpStandard.Controls[iLoop].Enabled := False;

    FgrpFancy.Enabled := False;
    For iLoop := 0 To FgrpFancy.ControlCount - 1 Do
      FgrpFancy.Controls[iLoop].Enabled := False;
    End
  Else
    Begin
    FgrpStandard.Enabled := True;
    For iLoop := 0 To FgrpStandard.ControlCount - 1 Do
      FgrpStandard.Controls[iLoop].Enabled := True;

    If (oStyle = pbsSimpleFancy) Or (oStyle = pbsCustomFancy) Then
      Begin
      FgrpFancy.Enabled := True;
      For iLoop := 0 To FgrpFancy.ControlCount - 1 Do
        FgrpFancy.Controls[iLoop].Enabled := True;
      End
    Else
      Begin
      FgrpFancy.Enabled := False;
      For iLoop := 0 To FgrpFancy.ControlCount - 1 Do
        FgrpFancy.Controls[iLoop].Enabled := False;
      End;
    End;
End;

Procedure TWPTableBorderFrame.OnBorderStyleChange(oSender: TObject);
Var
  oStyle : TPredefinedBorderStyles;
Begin
  oStyle := TPredefinedBorderStyles(FBorderStyleCombo.ItemIndex);
  Case oStyle Of
    pbsSimpleLine:      FBorder.SimpleLine;
    pbsSimpleDot:       FBorder.SimpleDot;
    pbsSimpleFancy:     FBorder.SimpleFancy;
    pbsCustomFancy:
      Begin
        FBorder.Defined := True;
        FBorder.Fancy := True;
      End;
    pbsCustom:
      Begin
        FBorder.Defined := True;
        FBorder.Fancy := False;
      End;
    pbsNone:
      Begin
        FBorder.Defined := False;
        FBorder.Fancy := False;
      End;
  End;

  Restore;
End;

Procedure TWPTableBorderFrame.OnIndividualBorderChange(oSender: TObject);
Begin
  If (FBorderStyleCombo.ItemIndex <> ord(pbsCustomFancy))
    And (FBorderStyleCombo.ItemIndex <> ord(pbsCustom)) Then
    FBorderStyleCombo.ItemIndex := ord(pbsCustom);
End;


{ TWPLineDialog }

Function TWPLineDialog.GetFrame : TWPLineFrame;
Begin
  Result := TWPLineFrame(Inherited Frame);
End;


Function TWPLineDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPLineFrame;
End;


Function TWPLineDialog.DialogCaption : String;
Begin
  Result := 'Line Details';
End;


Function TWPLineDialog.GetLine : TWPWorkingDocumentBreakPiece;
Begin
  Result := Frame.Line;
End;


Procedure TWPLineDialog.SetLine(Value : TWPWorkingDocumentBreakPiece);
Begin
  Frame.Line := Value;
End;



{ TWPLineFrame }

Function TWPLineFrame.DesiredHeight: Integer;
Begin
  Result := 270;
End;


Function TWPLineFrame.DesiredWidth: Integer;
Begin
  Result := 240;
End;


Function TWPLineFrame.GetLine : TWPWorkingDocumentBreakPiece;
Begin
  Assert(Invariants('GetLine', FLine, TWPWorkingDocumentBreakPiece, 'Line'));
  Result := FLine;
End;


Procedure TWPLineFrame.SetLine(Const Value : TWPWorkingDocumentBreakPiece);
Begin
  FLine.Free;
  FLine := Value;
End;


Procedure TWPLineFrame.Initialise;
Begin
  Inherited;

  BuildForm;
End;


Procedure TWPLineFrame.Finalise;
Begin
  FLine.Free;
  Inherited;
End;


Procedure TWPLineFrame.BuildSize;
Begin
  FgrpSize := TUixGroupBox.Create(Self);
  FgrpSize.Parent := ClientPanel;
  FgrpSize.align := alTop;
  FgrpSize.Caption := ' Size ';

  FcbxAlignment := TUixComboBox.Create(Self);
  FcbxAlignment.Parent := FgrpSize;
  FcbxAlignment.Top := 24;
  FcbxAlignment.Left := 100;
  FcbxAlignment.Width := 100;
  FcbxAlignment.Style := csDropDownList;
  FcbxAlignment.AddValues(NAMES_WPSALIGNMENT);
  FcbxAlignment.OnClick := userAction;
  AddLabel(FgrpSize, 28, 20, 'Alignment');

  FtrckWidth := TTrackBar.Create(Self);
  FtrckWidth.Parent := FgrpSize;
  FtrckWidth.Left := 92;
  FtrckWidth.Top := 50;
  FtrckWidth.Width := 116;
  FtrckWidth.Min := 0;
  FtrckWidth.Max := 100;
  FtrckWidth.Frequency := 25;
  FtrckWidth.LineSize := 10;
  FtrckWidth.OnChange := userAction;
  AddLabel(FgrpSize, 52, 20, 'Width (%)');
  FcWidth := AddLabel(FgrpSize, 52, 208, '100');

  FgrpSize.Height := FtrckWidth.Top + FtrckWidth.Height - 10;
End;


Procedure TWPLineFrame.BuildPen;
Begin
  FgrpPen := TUixGroupBox.Create(Self);
  FgrpPen.Parent := ClientPanel;
  FgrpPen.align := alTop;
  FgrpPen.Caption := ' Pen ';

  AddLabel(FgrpPen, 24, 20, 'Width');
  FspnPenWidth := TSpinEdit.Create(Self);
  FspnPenWidth.Parent := FgrpPen;
  FspnPenWidth.Left := 100;
  FspnPenWidth.Top := 22;
  FspnPenWidth.Width := 100;
  FspnPenWidth.MinValue := 1;
  FspnPenWidth.MaxValue := 12;
  FspnPenWidth.OnChange := userAction;

  AddLabel(FgrpPen, 50, 20, 'Colour');
  FcbxColour := TUixHTMLColourComboBox.Create(Self);
  FcbxColour.Parent := FgrpPen;
  FcbxColour.Left := 100;
  FcbxColour.Top := 48;
  FcbxColour.Width := 100;
  FcbxColour.OnClick := userAction;

  AddLabel(FgrpPen, 76, 20, 'Style');
  FcbxStyle := TUixComboBox.Create(Self);
  FcbxStyle.Parent := FgrpPen;
  FcbxStyle.Top := 76;
  FcbxStyle.Left := 100;
  FcbxStyle.Width := 100;
  FcbxStyle.Style := csDropDownList;
  FcbxStyle.AddValues(ADVPENSTYLE_NAMES);
  FcbxStyle.OnClick := userAction;

  FcbxEndStyle := TUixComboBox.Create(Self);
  FcbxEndStyle.Parent := FgrpPen;
  FcbxEndStyle.Top := 76;
  FcbxEndStyle.Left := 100;
  FcbxEndStyle.Width := 100;
  FcbxEndStyle.Style := csDropDownList;
  FcbxEndStyle.AddValues(ADVPENENDSTYLE_CODES);
  FcbxEndStyle.OnClick := userAction;

  FgrpPen.Height := FcbxStyle.Top + FcbxStyle.Height + 10;
End;


Procedure TWPLineFrame.BuildExample;
Begin
  FgrpExample := TUixGroupBox.Create(Self);
  FgrpExample.Parent := ClientPanel;
  FgrpExample.align := alClient;
  FgrpExample.Caption := ' Sample ';
  FgrpExample.BorderWidth := 12;

  FExample := TWordProcessor.Create(Self);
  FExample.Parent := FgrpExample;
  FExample.Settings.Interactive := False;
  FExample.AlignClient;
  FExample.Settings.HorizontalMargin := 10;
  FExample.Settings.VerticalMargin := 10;
  FExample.Settings.ShowVerticalScrollbar := True;

  BuildExampleLine;
End;

Procedure TWPLineFrame.BuildExampleLine;
Var
  oBuilder : TWPDocumentBuilder;
Begin
  oBuilder := TWPDocumentBuilder.Create;
  Try
    oBuilder.Document := TWPDocument.Create;
    oBuilder.Start;
    oBuilder.AddLine(1);
    oBuilder.StartParagraph;
    oBuilder.EndParagraph;
    oBuilder.Stop;
    FExample.DocumentHandler.LoadDocument(oBuilder.Document);
  Finally
    oBuilder.Free;
  End;
End;


Procedure TWPLineFrame.BuildForm;
Begin
  BuildExample;
  BuildPen;
  BuildSize;
End;


Procedure TWPLineFrame.Restore;
Begin
  Inherited;
  FcbxAlignment.ItemIndex := ord(FLine.Alignment);
  FtrckWidth.Position := IntegerMax(0, IntegerMin(Trunc(FLine.Width * 100), 100));
  FcWidth.Caption := IntegerToString(FtrckWidth.Position);
  FspnPenWidth.Value := FLine.PenWidth;
  FcbxColour.Value := FLine.PenColour;
  FcbxStyle.Value := ord(FLine.PenStyle);
  FcbxEndStyle.Value := ord(FLine.EndStyle);

  FcbxEndStyle.Visible := FLine.PenWidth > 1;
  FcbxStyle.Visible := FLine.PenWidth = 1;

  FExample.PrimaryRange.SetLineProperties(FLine);
  bIsReady := True;
End;


Procedure TWPLineFrame.DoUpdate;
Begin
  FLine.Alignment := TWordProcessorAlignment(FcbxAlignment.ItemIndex);
  FLine.Width := FtrckWidth.Position / 100;
  FcWidth.Caption := IntegerToString(FtrckWidth.Position);
  FLine.PenWidth := FspnPenWidth.Value;
  FLine.PenColour := FcbxColour.Value;
  FLine.PenStyle := TFslPenStyle(FcbxStyle.Value);
  FLine.EndStyle := TFslPenEndStyle(FcbxEndStyle.Value);

  FcbxEndStyle.Visible := FLine.PenWidth > 1;
  FcbxStyle.Visible := FLine.PenWidth = 1;

  FExample.PrimaryRange.SetLineProperties(FLine);
End;



Function TWPLineFrame.CanAccept : Boolean;
Begin
  Result := Inherited CanAccept;
End;


Procedure TWPLineFrame.Accept;
Begin
  Inherited;
  DoUpdate();
End;


Procedure TWPLineFrame.userAction(oSender: TObject);
Begin
  If bIsReady Then
    DoUpdate();
End;




{ TWPSearchDialog }

Function TWPSearchDialog.GetFrame : TWPSearchFrame;
Begin
  Result := TWPSearchFrame(Inherited Frame);
End;


Function TWPSearchDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPSearchFrame;
End;


Function TWPSearchDialog.DialogCaption : String;
Begin
  Result := 'Search';
End;

Function TWPSearchDialog.GetSearchDetails: TWPSearchDetails;
Begin
  Result := Frame.SearchDetails;
End;

Procedure TWPSearchDialog.SetSearchDetails(Const Value: TWPSearchDetails);
Begin
  Frame.SearchDetails := Value;
End;

Function TWPSearchDialog.GetRange: TWPRange;
Begin
  Result := Frame.Range;
End;

Procedure TWPSearchDialog.SetRange(Const Value: TWPRange);
Begin
  Frame.Range := Value;
End;

Procedure TWPSearchDialog.Refresh;
Begin
  Inherited;
  ActiveControl := Frame.EditText;
End;

Function TWPSearchDialog.CanAccept : Boolean;
Begin
  Result := Inherited CanAccept And Frame.CanAccept;
End;


{ TWPSearchFrame }

Function TWPSearchFrame.DesiredHeight: Integer;
Begin
  Result := 155;
End;

Function TWPSearchFrame.DesiredWidth: Integer;
Begin
  Result := 420;
End;

Procedure TWPSearchFrame.Initialise;
Begin
  Inherited;

  FSearchDetails := TWPSearchDetails.Create;
  BuildForm;
End;

Procedure TWPSearchFrame.Finalise;
Begin
  FRange.Free;
  FSearchDetails.Free;
  Inherited;
End;

Procedure TWPSearchFrame.BuildText;
Begin
  FgrpText := TUixGroupBox.Create(Self);
  FgrpText.parent := ClientPanel;
  FgrpText.AlignTop;
  FgrpText.Caption := 'Text';
  FgrpText.Height := 60;

  AddLabel(FGrpText, 23, 20, 'Find');

  FedtText := TUixEdit.Create(Self);
  FedtText.Parent := FgrpText;
  FedtText.Left := 60;
  FedtText.Top := 20;
  FedtText.Width := DesiredWidth - FedtText.Left - 30;
End;


Procedure TWPSearchFrame.BuildOptions;
Begin
  FgrpOptions := TUixGroupBox.Create(Self);
  FgrpOptions.parent := FpnlOptions;
  FgrpOptions.AlignLeft;
  FgrpOptions.Width := 130;
  FgrpOptions.Caption := 'Options';

  FcbCaseSensitive := TUixCheckBox.Create(Self);
  FcbCaseSensitive.Parent := FgrpOptions;
  FcbCaseSensitive.Caption := 'Case &Sensitive';
  FcbCaseSensitive.Left := 20;
  FcbCaseSensitive.Top := 20;
  FcbCaseSensitive.Width := FgrpOptions.Width - FcbCaseSensitive.Left - 5;

  FcbWholeWords := TUixCheckBox.Create(Self);
  FcbWholeWords.Parent := FgrpOptions;
  FcbWholeWords.Caption := '&Whole Words';
  FcbWholeWords.Left := 20;
  FcbWholeWords.Top := 50;
  FcbWholeWords.Width := FgrpOptions.Width - FcbWholeWords.Left - 5;
End;

Procedure TWPSearchFrame.BuildDirection;
Begin
  FgrpDirection := TUixRadioGroup.Create(Self);
  FgrpDirection.parent := FpnlOptions;
  FgrpDirection.AlignLeft;
  FgrpDirection.Width := 120;
  FgrpDirection.Caption := 'Direction';
  FgrpDirection.ItemStringList.Add('&Forwards');
  FgrpDirection.ItemStringList.Add('&Backwards');
End;


Procedure TWPSearchFrame.BuildScope;
Begin
  FgrpScope := TUixRadioGroup.Create(Self);
  FgrpScope.parent := FpnlOptions;
  FgrpScope.AlignClient;
  FgrpScope.Caption := 'Scope';
  FgrpScope.ItemStringList.Add('From &Cursor');
  FgrpScope.ItemStringList.Add('Whole &Document');
End;

Procedure TWPSearchFrame.BuildForm;
Begin
  BuildText;

  FpnlOptions := TUixPanel.Create(Self);
  FpnlOptions.parent := ClientPanel;
  FpnlOptions.AlignClient;

  BuildOptions;
  BuildDirection;
  BuildScope;
End;

Procedure TWPSearchFrame.SetSearchDetails(Const Value: TWPSearchDetails);
Begin
  FSearchDetails.Free;
  FSearchDetails := Value;
  BindToSearch;
End;

Procedure TWPSearchFrame.BindToSearch;
Begin
  FedtText.Text := SearchDetails.Text;
  If SearchDetails.Direction = sdBackwards Then
    FgrpDirection.ItemIndex := 1
  Else
    FgrpDirection.ItemIndex := 0;
  If SearchDetails.WholeDocument Then
    FgrpScope.ItemIndex := 1
  Else
    FgrpScope.ItemIndex := 0;

  FcbCaseSensitive.Checked := SearchDetails.CaseSensitive;
  FcbWholeWords.Checked := SearchDetails.WholeWords;
End;


Procedure TWPSearchFrame.Restore;
Begin
  Inherited;
  BindToSearch;
End;


Procedure TWPSearchFrame.ReadSearch;
Begin
  SearchDetails.Text := FedtText.Text;
  Case FgrpDirection.ItemIndex Of
    0: SearchDetails.Direction := sdForwards;
    1: SearchDetails.Direction := sdBackwards;
  End;
  SearchDetails.WholeDocument := FgrpScope.ItemIndex = 1;
  SearchDetails.CaseSensitive := FcbCaseSensitive.Checked;
  SearchDetails.WholeWords := FcbWholeWords.Checked;
End;


Function TWPSearchFrame.CanAccept : Boolean;
Begin
  Result := Inherited CanAccept;
  If Result Then
  Begin
    ReadSearch;
    Result := SearchDetails.Text <> '';
    If Not Result Then
      Invalid(FedtText, 'Search Text', 'Please enter some text to search for')
    Else
    Begin
      Result := Range.Search(SearchDetails);
      If Not Result Then
      Begin
        If Not SearchDetails.WholeDocument Then
        Begin
          If DialogQuestion('Unable to find '+SearchDetails.Text+', search whole document?') Then
          Begin
          SearchDetails.WholeDocument := True;
          Result := Range.Search(SearchDetails);
          SearchDetails.WholeDocument := False;
          If Not Result Then
            DialogStatement('Unable to find '+SearchDetails.Text);
          End
        End
        Else
          DialogStatement('Unable to find '+SearchDetails.Text);
      End;
    End;
  End;
  GetParentForm(FedtText).activeControl := FedtText;
End;


Procedure TWPSearchFrame.Accept;
Begin
  Inherited;
  ReadSearch;
End;


Function TWPSearchFrame.GetSearchDetails : TWPSearchDetails;
Begin
  Assert(Condition(HasSearchDetails, 'GetSearchDetails', 'No SearchDetails'));
  Result := FSearchDetails;
End;


Function TWPSearchFrame.GetRange : TWPRange;
Begin
  Assert(Condition(HasRange, 'GetRange', 'No Range'));
  Result := FRange;
End;


Procedure TWPSearchFrame.SetRange(Const Value : TWPRange);
Begin
  FRange.Free;
  FRange := Value;
End;


Function TWPSearchFrame.HasSearchDetails : Boolean;
Begin
  Result := Assigned(FSearchDetails);
End;


Function TWPSearchFrame.HasRange : Boolean;
Begin
  Result := Assigned(FRange);
End;


{ TWPReplaceDialog }

Function TWPReplaceDialog.GetFrame : TWPReplaceFrame;
Begin
  Result := TWPReplaceFrame(Inherited Frame);
End;


Function TWPReplaceDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPReplaceFrame;
End;


Function TWPReplaceDialog.DialogCaption : String;
Begin
  Result := 'Replace';
End;


Procedure TWPReplaceDialog.Initialise;
Begin
  Inherited;
  FbtnAll := TUixButton.Create(Self);
  FbtnAll.Parent := BottomPanel;
  FBtnAll.AnchoredRight := True;
  FBtnAll.AnchoredLeft := False;
  FBtnAll.AnchoredTop := True;
  FBtnAll.AnchoredBottom := False;
  FBtnAll.Left := OKButton.Left - 10 - FBtnAll.Width;
  FBtnAll.Top := OKButton.Top;
  FBtnAll.Caption := '&All';
  FBtnAll.Visible := True;
  FBtnAll.OnClick := ReplaceAll;
End;

Function TWPReplaceDialog.GetReplaceDetails: TWPReplaceDetails;
Begin
  Result := Frame.ReplaceDetails;
End;

Procedure TWPReplaceDialog.SetReplaceDetails(Const Value: TWPReplaceDetails);
Begin
  Frame.ReplaceDetails := Value;
End;

Function TWPReplaceDialog.GetRange: TWPRange;
Begin
  Result := Frame.Range;
End;

Procedure TWPReplaceDialog.SetRange(Const Value: TWPRange);
Begin
  Frame.Range := Value;
End;

Procedure TWPReplaceDialog.Refresh;
Begin
  Inherited;
  ActiveControl := Frame.EditText;
End;

Function TWPReplaceDialog.CanAccept : Boolean;
Begin
  Result := Inherited CanAccept And Frame.CanAccept;
End;


Procedure TWPReplaceDialog.ReplaceAll(oSender : TObject);
Var
  iOldLeft : Integer;
Begin
  iOldLeft := Left;
  Left := -10000;
  Try
    If Frame.ReplaceAll Then
      Okay;
  Finally
    Left := iOldLeft;
  End;
End;

{ TWPReplaceFrame }

Function TWPReplaceFrame.DesiredHeight: Integer;
Begin
  Result := 200;
End;

Function TWPReplaceFrame.DesiredWidth: Integer;
Begin
  Result := 430;
End;

Procedure TWPReplaceFrame.Initialise;
Begin
  Inherited;

  FReplaceDetails := TWPReplaceDetails.Create;
  BuildForm;
  FHasReplacedAll := False;
End;

Procedure TWPReplaceFrame.Finalise;
Begin
  FOriginalSelection.Free;
  FRange.Free;
  FReplaceDetails.Free;
  Inherited;
End;

Procedure TWPReplaceFrame.BuildText;
Begin
  FgrpText := TUixGroupBox.Create(Self);
  FgrpText.parent := ClientPanel;
  FgrpText.AlignTop;
  FgrpText.Caption := 'Text';
  FgrpText.Height := 85;

  AddLabel(FGrpText, 23, 15, 'Find');

  FedtText := TUixEdit.Create(Self);
  FedtText.Parent := FgrpText;
  FedtText.Left := 75;
  FedtText.Top := 20;
  FedtText.Width := DesiredWidth - FedtText.Left - 30;

  AddLabel(FGrpText, 53, 15, 'Replace');
  FedtReplace := TUixEdit.Create(Self);
  FedtReplace.Parent := FgrpText;
  FedtReplace.Left := FedtText.Left;
  FedtReplace.Top := FedtText.Top + 30;
  FedtReplace.Width := FedtText.Width;
End;


Procedure TWPReplaceFrame.BuildOptions;
Begin
  FgrpOptions := TUixGroupBox.Create(Self);
  FgrpOptions.parent := FpnlOptions;
  FgrpOptions.AlignLeft;
  FgrpOptions.Width := 150;
  FgrpOptions.Caption := 'Options';

  FcbCaseSensitive := TUixCheckBox.Create(Self);
  FcbCaseSensitive.Parent := FgrpOptions;
  FcbCaseSensitive.Caption := 'Case &Sensitive';
  FcbCaseSensitive.Left := 20;
  FcbCaseSensitive.Top := 20;
  FcbCaseSensitive.Width := FgrpOptions.Width - FcbCaseSensitive.Left - 5;

  FcbWholeWords := TUixCheckBox.Create(Self);
  FcbWholeWords.Parent := FgrpOptions;
  FcbWholeWords.Caption := '&Whole Words';
  FcbWholeWords.Left := 20;
  FcbWholeWords.Top := 46;
  FcbWholeWords.Width := FgrpOptions.Width - FcbWholeWords.Left - 5;

  FcbPrompt := TUixCheckBox.Create(Self);
  FcbPrompt.Parent := FgrpOptions;
  FcbPrompt.Caption := '&Prompt on Replace';
  FcbPrompt.Left := 20;
  FcbPrompt.Top := 72;
  FcbPrompt.Width := FgrpOptions.Width - FcbPrompt.Left - 5;
End;

Procedure TWPReplaceFrame.BuildDirection;
Begin
  FgrpDirection := TUixRadioGroup.Create(Self);
  FgrpDirection.parent := FpnlOptions;
  FgrpDirection.Left := FgrpOptions.Left + FgrpOptions.Width;
  FgrpDirection.Top := FgrpOptions.Top;
  FgrpDirection.Width := 120;
  FgrpDirection.Height := 72;
  FgrpDirection.Caption := 'Direction';
  FgrpDirection.ItemStringList.Add('&Forwards');
  FgrpDirection.ItemStringList.Add('&Backwards');
End;


Procedure TWPReplaceFrame.BuildScope;
Begin
  FgrpScope := TUixRadioGroup.Create(Self);
  FgrpScope.parent := FpnlOptions;
  FgrpScope.Left := FgrpDirection.Left + FgrpDirection.Width;
  FgrpScope.Top := FgrpOptions.Top;
  FgrpScope.Width := 130;
  FgrpScope.Height := 72;
  FgrpScope.Caption := 'Scope';
  FgrpScope.ItemStringList.Add('From &Cursor');
  FgrpScope.ItemStringList.Add('Whole &Document');
End;

Procedure TWPReplaceFrame.BuildForm;
Begin
  BuildText;

  FpnlOptions := TUixPanel.Create(Self);
  FpnlOptions.parent := ClientPanel;
  FpnlOptions.AlignClient;

  BuildOptions;
  BuildDirection;
  BuildScope;
End;

Procedure TWPReplaceFrame.SetReplaceDetails(Const Value: TWPReplaceDetails);
Begin
  FReplaceDetails.Free;
  FReplaceDetails := Value;
  BindToReplace;
End;

Procedure TWPReplaceFrame.BindToReplace;
Begin
  FedtText.Text := ReplaceDetails.Text;
  FedtReplace.Text := ReplaceDetails.Replace;

  If FOriginalSelection.HasSelection Then
  Begin
    FgrpScope.ItemStringList.Add('Selected &Text');
    FgrpScope.Height := 102;
  End;

  If ReplaceDetails.Direction = sdBackwards Then
    FgrpDirection.ItemIndex := 1
  Else
    FgrpDirection.ItemIndex := 0;

  If FOriginalSelection.HasSelection And ReplaceDetails.Selection Then
    FgrpScope.ItemIndex := 2
  Else If ReplaceDetails.WholeDocument Then
    FgrpScope.ItemIndex := 1
  Else
    FgrpScope.ItemIndex := 0;

  FcbCaseSensitive.Checked := ReplaceDetails.CaseSensitive;
  FcbWholeWords.Checked := ReplaceDetails.WholeWords;
  FcbPrompt.Checked := ReplaceDetails.Prompt;
End;


Procedure TWPReplaceFrame.Restore;
Begin
  Inherited;
  BindToReplace;
End;


Function TWPReplaceFrame.ReadReplace : Boolean;
Begin
  ReplaceDetails.Text := FedtText.Text;
  ReplaceDetails.Replace := FedtReplace.Text;

  Case FgrpDirection.ItemIndex Of
    0: ReplaceDetails.Direction := sdForwards;
    1: ReplaceDetails.Direction := sdBackwards;
  End;
  ReplaceDetails.WholeDocument := FgrpScope.ItemIndex = 1;
  ReplaceDetails.Selection := FgrpScope.ItemIndex = 2;

  ReplaceDetails.CaseSensitive := FcbCaseSensitive.Checked;
  ReplaceDetails.WholeWords := FcbWholeWords.Checked;
  ReplaceDetails.Prompt := FcbPrompt.Checked;
  Result := ReplaceDetails.Text <> '';
  If Not Result Then
    Invalid(FedtText, 'Replace Text', 'Please enter some text to Replace for')
End;


Function TWPReplaceFrame.CanAccept : Boolean;
Begin
  Result := Inherited CanAccept;
  If Result And Not FHasReplacedAll Then
  Begin
    Result := ReadReplace;
    If Result Then
    Begin
      // move to the left
      If FReplaceDetails.Selection Then
        Result := Range.Search(ReplaceDetails, FOriginalSelection.SelStart, FOriginalSelection.SelEnd)
      Else
        Result := Range.Search(ReplaceDetails);

      If Not Result Then
      Begin
        If Not ReplaceDetails.WholeDocument And Not FReplaceDetails.Selection Then
        Begin
          If DialogQuestion('Unable to find '+ReplaceDetails.Text+', Search whole document?') Then
          Begin
          ReplaceDetails.WholeDocument := True;
          Result := Range.Search(ReplaceDetails);
          ReplaceDetails.WholeDocument := False;
          If Result Then
            Result := DoReplace
          Else
            DialogStatement('Unable to find '+ReplaceDetails.Text);
          End
        End
        Else
          DialogStatement('Unable to find '+ReplaceDetails.Text);
      End
      Else
        Result := DoReplace;
    End;
  End;
  GetParentForm(FedtText).activeControl := FedtText;
End;


Procedure TWPReplaceFrame.Accept;
Begin
  Inherited;
  ReadReplace;
End;


Function TWPReplaceFrame.GetReplaceDetails : TWPReplaceDetails;
Begin
  Assert(Condition(HasReplaceDetails, 'GetReplaceDetails', 'No ReplaceDetails'));
  Result := FReplaceDetails;
End;


Function TWPReplaceFrame.GetRange : TWPRange;
Begin
  Assert(Condition(HasRange, 'GetRange', 'No Range'));
  Result := FRange;
End;


Procedure TWPReplaceFrame.SetRange(Const Value : TWPRange);
Begin
  FRange.Free;
  FRange := Value;
  FOriginalSelection := FRange.Selection.Clone;
End;


Function TWPReplaceFrame.HasReplaceDetails : Boolean;
Begin
  Result := Assigned(FReplaceDetails);
End;


Function TWPReplaceFrame.HasRange : Boolean;
Begin
  Result := Assigned(FRange);
End;


Function TWPReplaceFrame.DoReplace : Boolean;
Begin
  Result := False;
  If Not ReplaceDetails.Prompt Or DialogQuestion('Replace') Then
    Result := Range.Insert(replaceDetails.Replace);
End;


Function TWPReplaceFrame.ReplaceAll : Boolean;
Var
  bNoPrompt : Boolean;
  bDone : Boolean;
Begin
  Result := ReadReplace;
  If Result Then
  Begin

    If ReplaceDetails.WholeDocument Then
    Begin
      If ReplaceDetails.Direction = sdForwards Then
        Range.MoveTo(0)
      Else
        Range.MoveTo(Range.Operator_.Document.CharCount - 1);
    End
    Else If ReplaceDetails.Selection Then
    Begin
      If ReplaceDetails.Direction = sdForwards Then
        Range.MoveTo(Range.Selection.SelStart)
      Else
        Range.MoveTo(Range.Selection.SelEnd - 1);
    End;

    Result := False;

    bNoPrompt := False;
    bDone := False;
    While Not bDone Do
    Begin
      If ReplaceDetails.Selection Then
        bDone := Not Range.Search(ReplaceDetails, FOriginalSelection.SelStart, FOriginalSelection.SelEnd)
      Else
        bDone := Not Range.Search(ReplaceDetails);

      If Not bDone Then
      Begin
        If bNoPrompt Or Not ReplaceDetails.Prompt Then
          Result := Range.Insert(ReplaceDetails.Replace) Or Result
        Else
        Begin
          Case MessageDlg('Replace?', mtConfirmation, [mbYes, mbNo, mbYesToAll, mbNoToAll, mbCancel], 0) Of
            mrYes : Result := Range.Insert(ReplaceDetails.Replace) Or Result;
            mrNo : ; // nothing
            mrYesToAll :
              Begin
              Result := Range.Insert(ReplaceDetails.Replace) Or Result;
              bNoPrompt := True;
              End;
            mrNoToAll :
              Begin
              Result := True;
              bDone := True;
              End;
            mrCancel :
              Begin
              Result := False;
              bDone := True;
              End;
          End;
        End;
      End;
    End;

    If Result Then
      FHasReplacedAll := True;
  End;
End;

{ TWPSortTableDialogs }

Function TWPSortTableDialog.DialogCaption: String;
Begin
  Result := 'Sorting Table';
End;

Function TWPSortTableDialog.CanAccept: Boolean;
Begin
  Result := Frame.CanAccept;
End;

Function TWPSortTableDialog.FrameClass: TWPFrameClass;
Begin
  Result := TWPSortTableFrame;
End;

Function TWPSortTableDialog.GetFrame: TWPSortTableFrame;
Begin
  Result := TWPSortTableFrame(Inherited Frame);
End;

Function TWPSortTableDialog.GetTable: TWPWorkingDocumentTableStartPiece;
Begin
  Result := Frame.Table;
End;

Procedure TWPSortTableDialog.SetTable(const oTable: TWPWorkingDocumentTableStartPiece);
Begin
  Frame.Table := oTable;
End;

Function TWPSortTableDialog.GetSortDetails: TWPSortDetailList;
Begin
  Result := Frame.SortDetails;
End;

{ TWPSortTableFrames }

Function TWPSortTableFrame.DesiredHeight: Integer;
Begin
  Result := 180;
End;

Function TWPSortTableFrame.DesiredWidth: Integer;
Begin
  Result := 240;
End;

Procedure TWPSortTableFrame.Finalise;
Begin
  Inherited;

  FSortDetails.Free;
End;

Procedure TWPSortTableFrame.Initialise;
Begin
  Inherited;

  FSortDetails := Nil;
  BuildForm;
End;

Procedure TWPSortTableFrame.Accept;
Var
  iLoop: Integer;
  oDetails: TWPSortDetail;
Begin
  Inherited;

  FSortDetails := TWPSortDetailList.Create;
  For iLoop := 0 To SORT_SIZE DO
  Begin
    If (Not FColumnCombos[iLoop].Enabled) Or (FColumnCombos[iLoop].ItemIndex <= 0) Then
      Break;

    oDetails := TWPSortDetail.Create(FColumnCombos[iLoop].ItemIndex - 1, FAscendRadios[iLoop].Checked);
    FSortDetails.Add(oDetails);
  End;
End;

Function TWPSortTableFrame.CanAccept: Boolean;
Var
  iLoop: Integer;
  sIndex: String;
  oColumns: TStringList;
Begin
  Result := Inherited CanAccept;

  // make sure each column has at most one sort order
  oColumns := TStringList.Create;
  Try
    For iLoop := 0 To SORT_SIZE DO
    Begin
      If Not FColumnCombos[iLoop].Enabled Then
        Break;

      sIndex := IntegerToString(FColumnCombos[iLoop].ItemIndex);
      If oColumns.IndexOf(sIndex) > -1 Then
      Begin
        Result := False;
        Break;
      End;
      oColumns.Add(sIndex);
    End;
  Finally
    oColumns.Free;
  End;
End;

Procedure TWPSortTableFrame.Restore;
Begin
  Inherited;

  LoadColumns;
End;

Function TWPSortTableFrame.GetTable: TWPWorkingDocumentTableStartPiece;
Begin
  Assert(Invariants('GetTable', FTable, TWPWorkingDocumentTableStartPiece, 'Table'));
  Result := FTable;
End;

Procedure TWPSortTableFrame.SetTable(Const oTable: TWPWorkingDocumentTableStartPiece);
Begin
  FTable.Free;
  FTable := oTable;
End;

Procedure TWPSortTableFrame.BuildForm;
Var
  iLoop: Integer;
  oGroup: TUixGroupBox;
  oCombo: TUixComboBox;
  oAscRadio: TRadioButton;
  oDesRadio: TRadioButton;
Begin
  For iLoop := 0 to SORT_SIZE Do
  Begin
  oGroup := TUixGroupBox.Create(ClientPanel);
  oGroup.Parent := ClientPanel;
  oGroup.Left := 5;
  oGroup.Top := 5 + (60 * iLoop);
  oGroup.Width := DesiredWidth - 10;
  oGroup.Height := 50;

  oCombo := TUixComboBox.Create(oGroup);
  oCombo.Parent := oGroup;
  oCombo.Align := alLeft;
  oCombo.Width := 150;
  oCombo.Style := csDropDownList;
  oCombo.OnClick := OnColumnComboClick;
  oCombo.Tag := iLoop;

  oAscRadio := TRadioButton.Create(oGroup);
  oAscRadio.Parent := oGroup;
  oAscRadio.Left := 160;
  oAscRadio.Top := 15;
  oAscRadio.Caption := 'Ascend';
  oAscRadio.Checked := True;

  oDesRadio := TRadioButton.Create(oGroup);
  oDesRadio.Parent := oGroup;
  oDesRadio.Left := 160;
  oDesRadio.Top := 35;
  oDesRadio.Caption := 'Descend';

  // put the controls into main array
  FgrpSorts[iLoop] := oGroup;
  FColumnCombos[iLoop] := oCombo;
  FAscendRadios[iLoop] := oAscRadio;
  FDescendRadios[iLoop] := oDesRadio;

  If iLoop = 0 Then
    Begin
    oGroup.Caption := 'First Sort by';
    End
  Else
    Begin
    oGroup.Caption := 'Then by';
    EnableControls(iLoop, False);
    End;
  End
End;


Procedure TWPSortTableFrame.LoadColumns;
Var
  iCount, iLoop: Integer;
Begin
  For iCount := 0 To SORT_SIZE Do
  Begin
  FColumnCombos[iCount].Sorted := True;
  FColumnCombos[iCount].Clear;
  FColumnCombos[iCount].Items.Add('- None -');
  For iLoop := 1 To FTable.ColumnCount Do
    FColumnCombos[iCount].Items.Add('Column ' + IntToStr(iLoop));
  End;
End;

Procedure TWPSortTableFrame.EnableControls(iIndex: Integer; bEnable: Boolean);
Begin
  FgrpSorts[iIndex].Enabled := bEnable;
  FColumnCombos[iIndex].Enabled := bEnable;
  FAscendRadios[iIndex].Enabled := bEnable;
  FDescendRadios[iIndex].Enabled := bEnable;
End;

Procedure TWPSortTableFrame.OnColumnComboClick(oSender: TObject);
Var
  oCombo: TUixComboBox;
  iLoop: Integer;
Begin
  oCombo := TUixComboBox(oSender);
  If oCombo.ItemIndex <= 0 Then
    Begin
      For iLoop := oCombo.Tag + 1 To SORT_SIZE Do
        EnableControls(iLoop, False);
    End
  Else If (FTable.ColumnCount > oCombo.Tag + 1) And (oCombo.Tag < SORT_SIZE) Then
    EnableControls(oCombo.Tag + 1);
End;

{ TWPAllowedWordsDialog }

Function TWPAllowedWordsDialog.GetFrame : TWPAllowedWordsFrame;
Begin
  Result := TWPAllowedWordsFrame(Inherited Frame);
End;


Function TWPAllowedWordsDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPAllowedWordsFrame;
End;


Function TWPAllowedWordsDialog.DialogCaption : String;
Begin
  Result := 'Allowed Words';
End;


function TWPAllowedWordsDialog.GetWords: TFslStringList;
begin
  result := Frame.Words;
end;

procedure TWPAllowedWordsDialog.SetWords(const Value: TFslStringList);
begin
  Frame.Words := Value;
end;

function TWPAllowedWordsDialog.GetToDictionary: TFslStringList;
begin
  result := Frame.FToDictionary;
end;

{ TWPAllowedWordsFrame }

Function TWPAllowedWordsFrame.DesiredHeight: Integer;
Begin
  Result := 390;
End;

Function TWPAllowedWordsFrame.DesiredWidth: Integer;
Begin
  Result := 400;
End;

Procedure TWPAllowedWordsFrame.Initialise;
Begin
  Inherited;

  FWords := TFslStringList.Create;
  FToDictionary := TFslStringList.Create;
  FComponents := TComponentList.Create;
  BuildForm;
End;

Procedure TWPAllowedWordsFrame.Finalise;
Begin
  Inherited;
  FWords.Free;
  FComponents.Free;
  FToDictionary.Free;
End;

Procedure TWPAllowedWordsFrame.BuildForm;
Begin
  FBox := TUixScrollBox.Create(self);
  FBox.Parent := ClientPanel;
  FBox.Align := alClient;
  FBox.Color := clWhite;
End;


Procedure TWPAllowedWordsFrame.Restore;
Begin
  Inherited;
  FToDictionary.Clear;
  ClearEdits;
  ReloadEdits;
End;


Function TWPAllowedWordsFrame.CanAccept : Boolean;
Begin
  Result := Inherited CanAccept;
  If Not Result Then
    Begin

    If Not Result Then
      DialogError('Rows and Columns must be positive numbers');
    End;
End;


procedure TWPAllowedWordsFrame.SetWords(const Value: TFslStringList);
begin
  FWords.Free;
  FWords := Value;
end;

procedure TWPAllowedWordsFrame.ButtonDelete(oSender: TObject);
begin
  FWords.DeleteByIndex(TComponent(oSender).Tag);
  ClearEdits;
  ReloadEdits;
end;

procedure TWPAllowedWordsFrame.ButtonMove(oSender: TObject);
begin
  FToDictionary.Add(FWords[TComponent(oSender).Tag]);
  ButtonDelete(oSender);
end;

procedure TWPAllowedWordsFrame.EditChange(oSender: TObject);
begin
  FWords[TComponent(oSender).Tag] := TUixEdit(oSender).Text;
end;

procedure TWPAllowedWordsFrame.ClearEdits;
var
  i : Integer;
begin
  for i := FComponents.Count - 1 Downto 0 do
    FComponents[i].Free;
end;

procedure TWPAllowedWordsFrame.ReloadEdits;
var
  i : Integer;
  oEdit : TUixEdit;
  oBtn : TUixButton;
Begin
  for i := 0 to FWords.Count - 1 Do
  Begin
    oEdit := TUixEdit.Create(Self);
    oEdit.Parent := FBox;
    oEdit.Top := 10+i*28;
    oEdit.Left := 10;
    oEdit.Width := 200;
    oEdit.Text := FWords[i];
    oEdit.Tag := i;
    oEdit.BorderStyle := bsSingle;
    oEdit.OnChange := EditChange;
    FComponents.Add(oEdit);

    oBtn := TUixButton.Create(self);
    oBtn.Parent := FBox;
    oBtn.Top := 8+i*28;
    oBtn.Left := 220;
    oBtn.Width := 55;
    oBtn.Caption := 'Delete';
    oBtn.Tag := i;
//    oBtn.Flat := True;
    oBtn.Font.Color := clNavy;
    oBtn.Font.Style := [fsUnderline];
    oBtn.OnClick := ButtonDelete;
    FComponents.Add(oBtn);

    oBtn := TUixButton.Create(self);
    oBtn.Parent := FBox;
    oBtn.Top := 8+i*28;
    oBtn.Left := 280;
    oBtn.Width := 105;
    oBtn.Caption := 'To Dictionary';
    oBtn.Tag := i;
//    oBtn.Flat := True;
    oBtn.Font.Color := clNavy;
    oBtn.Font.Style := [fsUnderline];
    oBtn.OnClick := ButtonMove;
    FComponents.Add(oBtn);
  End;
end;

{ TWPOpenFileDialog }

Procedure TWPOpenFileDialog.ExecuteWP(oWP: TWordProcessor);
Begin
  If Execute Then
    if FAllowSnapshot And StringEqualsInsensitive(PathExtension(Filename), '.xml') And (FilterIndex = 2) Then
      oWP.DocumentHandler.LoadSnapshot(Filename)
    Else
      oWP.DocumentHandler.LoadByExtension(FileName);
End;

function TWPOpenFileDialog.GetFormat: TWPFormat;
begin
  result := FFilterFormats[FilterIndex - 1];
end;

Procedure TWPOpenFileDialog.Initialise;
Begin
  Inherited;
  AllowNative := True;
End;

Procedure TWPOpenFileDialog.SetAllowNative(Const Value: Boolean);
Begin
  FAllowNative := Value;
  SetOptions;
End;

Procedure TWPOpenFileDialog.SetAllowSnapshot(Const Value: Boolean);
Begin
  FAllowSnapshot := Value;
  SetOptions;
End;

procedure TWPOpenFileDialog.SetFilterFormats(a: array of TWPFormat);
var
  i : integer;
begin
  SetLength(FFilterFormats, length(a));
  for i := 0 to length(a) - 1 do
    FFilterFormats[i] := a[i];
end;

Procedure TWPOpenFileDialog.SetOptions;
Begin
  Title := 'Open Document';
  If AllowNative Then
  Begin
    DefaultExt := DEFAULT_EXTENSION;
    Filter :=
      FILTER_ALL_DOCUMENTS + '|'+
      FILTER_WORD_DOCUMENT + '|' +
      FILTER_ODT_DOCUMENT + '|' +
      FILTER_CDA_DOCUMENT + '|' +
      FILTER_WEBDOCUMENT + '|' +
      FILTER_TEXTDOCUMENT + '|'  +
      FILTER_WP_DOCUMENT + '|' ;
    if FAllowSnapshot Then
      Filter := Filter + FILTER_SNAPSHOT + '|';
    Filter := Filter +
      FILTER_CONSOLEDOCUMENT + '|' +
      FILTER_ANYFILE;
    if FAllowSnapshot then
      SetFilterFormats([wpfUnknown, wpfRtf, wpfODT, wpfCDA, wpfHTML, wpfText, wpfNative, wpfSnapshot, wpfUnknown])
    else
      SetFilterFormats([wpfUnknown, wpfRtf, wpfODT, wpfCDA, wpfHTML, wpfText, wpfNative, wpfUnknown])

  End
  Else
  Begin
    DefaultExt := DEFAULT_EXTENSION;
    Filter :=
      FILTER_ALL_DOCUMENTS + '|'+
      FILTER_WORD_DOCUMENT + '|' +
      FILTER_ODT_DOCUMENT + '|' +
      FILTER_CDA_DOCUMENT + '|' +
      FILTER_WEBDOCUMENT + '|' +
      FILTER_TEXTDOCUMENT + '|'  +
      FILTER_WP_DOCUMENT + '|' +
      FILTER_ANYFILE;
      SetFilterFormats([wpfUnknown, wpfRtf, wpfODT, wpfCDA, wpfHTML, wpfText, wpfNative, wpfUnknown]);
  End;
  Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
  // ofReadOnly maybe supported in the future - needs to track document readonly in the WP
End;


{ TWPSaveFileDialog }

Procedure TWPSaveFileDialog.ExecuteWP(oWP: TWordProcessor);
Begin
  If Execute Then
    oWP.DocumentHandler.SaveByExtension(FileName);
End;

function TWPSaveFileDialog.GetFormat: TWPFormat;
begin
  result := FFilterFormats[FilterIndex-1];
end;

Procedure TWPSaveFileDialog.Initialise;
Begin
  Inherited;
  AllowNative := True;
End;

procedure TWPSaveFileDialog.SetAllowCDA(const Value: boolean);
begin
  FAllowCDA := Value;
  SetOptions;
end;

Procedure TWPSaveFileDialog.SetAllowNative(Const Value: Boolean);
Begin
  FAllowNative := Value;
  SetOptions;
End;

procedure TWPSaveFileDialog.SetFilterFormats(a: array of TWPFormat);
var
  i : integer;
begin
  SetLength(FFilterFormats, length(a));
  for i := 0 to length(a) - 1 do
    FFilterFormats[i] := a[i];
end;

Procedure TWPSaveFileDialog.SetOptions;
var
  s : String;
Begin
  Title := 'Save Document';
  if FAllowCDA then
    s := FILTER_CDA_DOCUMENT + '|'
  else
    s := '';
  if FAllowCDA then
    DefaultExt := '.xml'
  else if AllowNative then
    DefaultExt := DEFAULT_EXTENSION
  else
    DefaultExt := '.rtf';


  if FAllowCDA then
    if FAllowNative then
      SetFilterFormats([wpfCDA, wpfNative, wpfRtf, wpfODT, wpfHTML, wpfMht, wpfText, wpfUnknown])
    else
      SetFilterFormats([wpfCDA, wpfRtf, wpfODT, wpfHTML, wpfMht, wpfText, wpfUnknown])
  else
    if FAllowNative then
      SetFilterFormats([wpfNative, wpfRtf, wpfODT, wpfHTML, wpfMht, wpfText, wpfUnknown])
    else
      SetFilterFormats([wpfRtf, wpfODT, wpfHTML, wpfMht, wpfText, wpfUnknown]);

  If AllowNative Then
    Filter :=
      s+
      FILTER_WP_DOCUMENT + '|' +
      FILTER_RICHTEXTFILE + '|' +
      FILTER_ODT_DOCUMENT + '|' +
      FILTER_WEBDOCUMENT + '|' +
      FILTER_MHT_DOCUMENT + '|' +
      FILTER_TEXTDOCUMENT + '|' +
      FILTER_CONSOLEDOCUMENT + '|' +
      FILTER_ANYFILE
  Else
    Filter :=
      s +
      FILTER_RICHTEXTFILE + '|' +
      FILTER_ODT_DOCUMENT + '|' +
      FILTER_WEBDOCUMENT + '|' +
      FILTER_MHT_DOCUMENT + '|' +
      FILTER_TEXTDOCUMENT + '|' +
      FILTER_ANYFILE;

  Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
End;


Procedure TWPFieldModelFrame.Initialise;
Begin
  Inherited;

  FIsInitialColumnSetupDone := False;

  FModel := Nil;

  FTreeView := TUixTreeView.Create(ClientPanel);
  FTreeView.AlignClient;
  FTreeView.Columns.Add('Name');
  FTreeView.Columns.Add('Description');
  FTreeView.ShowRoot := False;
  FTreeView.AutoSizeColumn := 1;
  FTreeView.OnGetNode := DoGetNode;
  FTreeView.OnRenderNode := DoRenderNode;
End;


Procedure TWPFieldModelFrame.Finalise;
Begin
  FModel.Free;
  FEntries.Free;

  Inherited;
End;


Function TWPFieldModelFrame.GetModel: TWPFieldModel;
Begin
  Assert(Invariants('GetModel', FModel, TWPFieldModel, 'FModel'));

  Result := FModel;
End;


Procedure TWPFieldModelFrame.SetModel(Const Value: TWPFieldModel);
Var
  iLoop : Integer;
Begin
  Assert(Not Assigned(Value) Or Invariants('SetModel', Value, TWPFieldModel, 'Value'));

  FModel.Free;
  FModel := Value;
  If (Assigned(FModel)) Then
  If FSection Then
    FEntries := FModel.Entries.Link
  Else
  Begin
    FEntries := TWPFieldEntryList.Create;
    For iLoop := 0 To FModel.Entries.Count - 1 Do
      If Not FModel.Entries[iLoop].Section Then
        FEntries.Add(FModel.Entries[iLoop].Link);
  End;
End;


Procedure TWPFieldModelFrame.DoGetNode(oSender: TObject; oNode: TUixTreeViewNode);
Var
  oEntry : TWPFieldEntry;
Begin
  If oNode.IsRoot Then
  Begin
    oNode.Children.Count := FEntries.Count;
  End
  Else If FEntries.ExistsByIndex(oNode.Index) Then
  Begin
    oEntry := FEntries[oNode.Index];

    If (oEntry.Code = '') Then
      oNode.ID := '--'
    Else
      oNode.ID := IntegerToString(oNode.Index);
    oNode.Data := oEntry;
  End;
End;


Procedure TWPFieldModelFrame.DoRenderNode(oSender: TObject; oNode: TUixTreeViewNode);
Var
  oEntry : TWPFieldEntry;
Begin
  oEntry := TWPFieldEntry(oNode.Data);

  Assert(Invariants('DoRenderNode', oEntry, TWPFieldEntry, 'oEntry'));

  oNode.Captions.Add(oEntry.Name);
  oNode.Captions.Add(oEntry.Description);
End;


Function TWPFieldModelFrame.GetSelected: TWPFieldEntry;
Var
  sId : String;
Begin
  sId := FTreeView.SelectedID;
  If sId = '' Then
    Result := Nil
  Else
    Result := FEntries[StringToInteger32(sId)];
End;


Procedure TWPFieldModelFrame.SetSelected(Const Value: TWPFieldEntry);
Begin
  FTreeView.SelectedID := IntegerToString(FEntries.IndexByReference(Value));
End;


Function TWPFieldModelFrame.DesiredHeight: Integer;
Begin
  Result := 135;
End;


Function TWPFieldModelFrame.DesiredWidth: Integer;
Begin
  Result := 400;
End;


Procedure TWPFieldModelFrame.Refresh;
Var
  iIndex : Integer;
  bHasDescriptions : Boolean;
Begin
  Inherited;

  If Not FIsInitialColumnSetupDone Then
  Begin
    bHasDescriptions := False;

    iIndex := 0;
    While Not bHasDescriptions And (iIndex < FEntries.Count) Do
    Begin
      bHasDescriptions := FEntries[iIndex].Description <> '';

      Inc(iIndex);
    End;

    If Not bHasDescriptions Then
    Begin
      FTreeView.HeaderVisible := False;
      FTreeView.AutoSizeColumn := 0;
      FTreeView.Columns.Columns[1].Visible := False;
    End;

    FIsInitialColumnSetupDone := True;
  End;

  FTreeView.Refresh;
End;


Function TWPFieldModelFrame.HasSelected: Boolean;
Begin
  Result := FTreeView.SelectedID <> '';
End;


Function TWPFieldModelFrame.GetHasModel: Boolean;
Begin
  Result := Assigned(FModel);
End;


Procedure TWPFieldModelDialog.Initialise;
Begin
  Inherited;

  PositionOwner;
  BorderStyleSizeable;

  Frame.TreeView.OnDoubleClick := DoFrameTreeViewDoubleClick;
End;


Function TWPFieldModelDialog.FrameClass: TWPFrameClass;
Begin
  Result := TWPFieldModelFrame;
End;


Function TWPFieldModelDialog.GetFrame: TWPFieldModelFrame;
Begin
  Result := TWPFieldModelFrame(Inherited Frame);
End;


Function TWPFieldModelDialog.GetModel: TWPFieldModel;
Begin
  Result := Frame.Model;
End;


Procedure TWPFieldModelDialog.SetModel(Const Value: TWPFieldModel);
Begin
  Frame.Model := Value;
  Caption := DialogCaption;
End;


Function TWPFieldModelDialog.GetSelected : TWPFieldEntry;
Begin
  Result := Frame.Selected;
End;


Procedure TWPFieldModelDialog.SetSelected(Const Value: TWPFieldEntry);
Begin
  Frame.Selected := Value;
End;


Function TWPFieldModelDialog.DialogCaption: String;
Begin
  If Frame.HasModel  Then
    Result := 'Insert '+Model.Title
  Else
    Result := 'Insert';
End;


Procedure TWPFieldModelDialog.DoFrameTreeViewDoubleClick(oSender: TObject; oNode: TUixTreeViewNode);
Begin
  If Frame.HasSelected Then
    OKButton.Click;
End;


Function TWPFieldModelDialog.CanAccept: Boolean;
Begin
  Result := Inherited CanAccept;

  If Result Then
  Begin
    Result := Frame.HasSelected;

    If Not Result Then
      Invalid(Frame.TreeView, 'Fields', 'You must select a '+Model.Title+' to insert into the word processor.');
  End;
End;


Function TWPFieldModelDialog.GetSection: Boolean;
Begin
  Result := Frame.Section;
End;

Procedure TWPFieldModelDialog.SetSection(Const Value: Boolean);
Begin
  Frame.Section := Value;
End;


Procedure TWPFieldModelDialog.Restore;
Var
  oCanvas : TCanvas;
  iMaximumEntryLength : Integer;
  iIndex : Integer;
Begin
  Inherited;

  oCanvas := TCanvas.Create;
  Try
    // very rough metric to help make the dialog a usable size by default
    iMaximumEntryLength := 0;
    For iIndex := 0 To Model.Entries.Count - 1 Do
      iMaximumEntryLength := IntegerMax(iMaximumEntryLength, Canvas.TextWidth(Model.Entries[iIndex].Name));

    Width := IntegerMin(iMaximumEntryLength + 20, 600);
    Height := IntegerMin(600, (Model.Entries.Count * 15) + 30);
  Finally
    oCanvas.Free;
  End;
End;

Procedure TWPInputFieldFrame.Initialise;
Var
  oBevel : TBevel;
Begin
  Inherited;

  FIdentityBox := TUixGroupBox.Create(ClientPanel);
  FIdentityBox.Parent := ClientPanel;
  FIdentityBox.AlignTop;
  FIdentityBox.Caption := ' Input ';
  FIdentityBox.Height := 122;


  AddLabel(FIdentityBox, 22, 20, 'Name:');
  FNameEdit := TUixEdit.Create(FIdentityBox);
  FNameEdit.Parent := FIdentityBox;
  FNameEdit.Top := 20;
  FNameEdit.Left := 70;
  FNameEdit.Width := 294;

  AddLabel(FIdentityBox, 47, 20, 'Hot Key:');
  FKeyEdit := TUixEdit.Create(FIdentityBox);
  FKeyEdit.Parent := FIdentityBox;
  FKeyEdit.CharCase := ecUpperCase;
  FKeyEdit.MaxLength := 1;
  FKeyEdit.AutoSelect := True;
  FKeyEdit.Top := 45;
  FKeyEdit.Left := 70;
  FKeyEdit.Width := 20;
  FKeyEdit.OnChange := OnKeyEditChanged;
  AddLabel(FIdentityBox, 47, 95, '(Alt-Key goes to field)');

  AddLabel(FIdentityBox, 47, 250, 'Width:');
  FWidthSpin := TSpinEdit.Create(FIdentityBox);
  FWidthSpin.Parent := FIdentityBox;
  FWidthSpin.Top := 45;
  FWidthSpin.Left := 284;
  FWidthSpin.Width := 80;
  FWidthSpin.MinValue := 0;
  FWidthSpin.MaxValue := 100;

  FDeletableCheckBox := TUixCheckBox.Create(FIdentityBox);
  FDeletableCheckBox.Parent := FIdentityBox;
  FDeletableCheckBox.Caption := 'User can delete';
  FDeletableCheckBox.Top := 75;
  FDeletableCheckBox.Left := 20;
  FDeletableCheckBox.Width := 114;

  FMandatoryCheckBox := TUixCheckBox.Create(FIdentityBox);
  FMandatoryCheckBox.Parent := FIdentityBox;
  FMandatoryCheckBox.Caption := 'Value required';
  FMandatoryCheckBox.Top := 75;
  FMandatoryCheckBox.Left := 134;
  FMandatoryCheckBox.Width := 108;

  FForceInputCheckBox := TUixCheckBox.Create(FIdentityBox);
  FForceInputCheckBox.Parent := FIdentityBox;
  FForceInputCheckBox.Caption := 'Correct input only';
  FForceInputCheckBox.Top := 75;
  FForceInputCheckBox.Left := 242;
  FForceInputCheckBox.Width := 120;

  AddLabel(FIdentityBox, 99, 20, 'Format Control:');
  FFormatableCombo := TUixComboBox.Create(FIdentityBox);
  FFormatableCombo.Parent := FIdentityBox;
  FFormatableCombo.Top := 97;
  FFormatableCombo.Left := 115;
  FFormatableCombo.Width := 249;
  FFormatableCombo.AddValue('User cannot format field');
  FFormatableCombo.AddValue('User can format field but not contents');
  FFormatableCombo.AddValue('User can format field and contents');

  FDetailsBox := TUixGroupBox.Create(ClientPanel);
  FDetailsBox.Parent := ClientPanel;
  FDetailsBox.AlignClient;
  FDetailsBox.Caption := ' Type ';

  FrbBoolean := MakeTypeRadioButton(18, '&Boolean');
  FrbNumber := MakeTypeRadioButton(40, '&Number');
  FrbInteger := MakeTypeRadioButton(62, '&Integer');
  FrbDate := MakeTypeRadioButton(84, '&Date');
  FrbText := MakeTypeRadioButton(106, '&Text');
  FrbList := MakeTypeRadioButton(128, '&List');
  FrbParagraph := MakeTypeRadioButton(145, '&Paragraph');

  oBevel := TBevel.Create(FDetailsBox);
  oBevel.Parent := FDetailsBox;
  oBevel.Top := 18;
  oBevel.Height := 100;
  oBevel.Left := 100;
  oBevel.Width := 2;
  oBevel.Shape := bsLeftLine;

  FedIntMinimumLabel := AddLabel(FDetailsBox, 20, 110, 'Minimum Value:');
  FedIntMinimum := TSpinEdit.Create(self);
  FedIntMinimum.Parent := FDetailsBox;
  FedIntMinimum.Top := 18;
  FedIntMinimum.Left := 225;
  FedIntMinimum.Width := 145;

  FedIntMaximumLabel := AddLabel(FDetailsBox, 42, 110, 'Maximum Value:');
  FedIntMaximum := TSpinEdit.Create(self);
  FedIntMaximum.Parent := FDetailsBox;
  FedIntMaximum.Top := 42;
  FedIntMaximum.Left := 225;
  FedIntMaximum.Width := 145;

  FedtFloatMainLabel := AddLabel(FDetailsBox, 20, 110, 'Main Digits:');
  FedtFloatMain := TSpinEdit.Create(self);
  FedtFloatMain.Parent := FDetailsBox;
  FedtFloatMain.Top := 18;
  FedtFloatMain.Left := 225;
  FedtFloatMain.Width := 145;
  FedtFloatMain.MinValue := 1;
  FedtFloatMain.MaxValue := 10;

  FedtFloatDecimalLabel := AddLabel(FDetailsBox, 42, 110, 'Decimal Places:');
  FedtFloatDecimal := TSpinEdit.Create(self);
  FedtFloatDecimal.Parent := FDetailsBox;
  FedtFloatDecimal.Top := 42;
  FedtFloatDecimal.Left := 225;
  FedtFloatDecimal.Width := 145;
  FedtFloatDecimal.MinValue := 0;
  FedtFloatDecimal.MaxLength := 10;

  FchkDateTimeLabel := AddLabel(FDetailsBox, 20, 110, 'Time Status:');
  FchkDateTime := TUixComboBox.Create(self);
  FchkDateTime.Parent := FDetailsBox;
  FchkDateTime.Top := 18;
  FchkDateTime.Left := 225;
  FchkDateTime.Width := 145;
  FchkDateTime.Strict := true;
  FchkDateTime.Items.Add(FIELD_DATA_NAME_DATE_TIME_Prohibited);
  FchkDateTime.Items.Add(FIELD_DATA_NAME_DATE_TIME_Allowed);
  FchkDateTime.Items.Add(FIELD_DATA_NAME_DATE_TIME_Required);

  FedtTextRegexLabel := AddLabel(FDetailsBox, 20, 110, 'Regular Expression:');
  FedtTextRegex := TUixEdit.Create(self);
  FedtTextRegex.Parent := FDetailsBox;
  FedtTextRegex.Top := 18;
  FedtTextRegex.Left := 225;
  FedtTextRegex.Width := 145;

  FedtTextDescLabel := AddLabel(FDetailsBox, 42, 110, 'Description:');
  FedtTextDesc := TUixEdit.Create(self);
  FedtTextDesc.Parent := FDetailsBox;
  FedtTextDesc.Top := 42;
  FedtTextDesc.Left := 225;
  FedtTextDesc.Width := 145;

  FedtTextMaxLabel := AddLabel(FDetailsBox, 64, 110, 'Maximum Length:');
  FedtTextMax := TSpinEdit.Create(self);
  FedtTextMax.Parent := FDetailsBox;
  FedtTextMax.Top := 64;
  FedtTextMax.Left := 225;
  FedtTextMax.Width := 145;
  FedtTextMax.MinValue := 0;
  FedtTextMax.MaxLength := 100;

  FcbxListModeLabel := AddLabel(FDetailsBox, 20, 110, 'List Mode:');
  FcbxListMode := TUixCombobox.Create(self);
  FcbxListMode.Parent := FDetailsBox;
  FcbxListMode.Top := 18;
  FcbxListMode.Left := 225;
  FcbxListMode.Width := 145;
  FcbxListMode.Strict := true;
  FcbxListMode.Items.Add(FIELD_DATA_NAME_LIST_MODE_Dropdown);
  FcbxListMode.Items.Add(FIELD_DATA_NAME_LIST_MODE_Optional);
  FcbxListMode.Items.Add(FIELD_DATA_NAME_LIST_MODE_Buttons);

  FmemLstListLabel := AddLabel(FDetailsBox, 42, 110, 'List Values:');
  FmemLstList:= TUixMemo.Create(self);
  FmemLstList.Parent := FDetailsBox;
  FmemLstList.Top := 42;
  FmemLstList.Left := 225;
  FmemLstList.Width := 145;
  FmemLstList.Height := 145;
End;


Procedure TWPInputFieldFrame.Finalise;
Begin
  FField.Free;
  FSection.Free;

  Inherited;
End;


Procedure TWPInputFieldFrame.OnKeyEditChanged(oSender: TObject);
Begin
  If (Length(FKeyEdit.Text) > 0) And (not CharInSet(FKeyEdit.Text[1], ['A'..'Z', '0'..'9'])) Then
  if IsSection Then
    if FSection.HasHotspot Then
        FKeyEdit.Text := FSection.Hotspot.Key
      else
        FKeyEdit.Text := ''
    Else
      if FField.HasHotspot Then
        FKeyEdit.Text := FField.Hotspot.Key
      else
        FKeyEdit.Text := '';
End;

Procedure TWPInputFieldFrame.Accept;
Var
  oList : TFslStringList;
Begin
  If IsSection Then
  Begin
    If (FSection = Nil) Then
      Section := TWPDocumentSection.Create;
    FSection.IsField := True;
    FSection.Name := FNameEdit.Text;
    FSection.Deletable := FDeletableCheckBox.Checked;
    FSection.HasHotspot := FKeyEdit.Text <> '';
    If FKeyEdit.Text <> '' Then
    Begin
      FSection.HasHotspot := true;
      FSection.Hotspot.Key := FKeyEdit.Text;
    End;
  End
  Else
  Begin
    FField.Name := FNameEdit.Text;
    FField.Width := FWidthSpin.Value;
    FField.Deletable := FDeletableCheckBox.Checked;
    FField.FixedFormat := TWPDocumentFieldFixedFormat(FFormatableCombo.value);
//    If Not FEditableCheckBox.Checked Then
//       FField.ReadOnly := ReadOnlyTrue;
    if FrbBoolean.Checked Then
      FField.DataValue[FIELD_DATA_NAME_TYPE] := 'Boolean'
    Else if FrbList.Checked Then
      FField.DataValue[FIELD_DATA_NAME_TYPE] := 'List'
    Else if FrbNumber.Checked Then
      FField.DataValue[FIELD_DATA_NAME_TYPE] := 'Number'
    Else if FrbInteger.Checked Then
      FField.DataValue[FIELD_DATA_NAME_TYPE] := 'Integer'
    Else if FrbDate.Checked Then
      FField.DataValue[FIELD_DATA_NAME_TYPE] := 'Date'
    Else // if FrbText.Checked Then
      FField.DataValue[FIELD_DATA_NAME_TYPE] := 'Text';

    FField.DataValue[FIELD_DATA_NAME_FORCE] := BooleanToString(FForceInputCheckBox.Enabled And FForceInputCheckBox.Checked);
    FField.DataValue[FIELD_DATA_NAME_MAND] := BooleanToString(FMandatoryCheckBox.Enabled And FMandatoryCheckBox.Checked);

    oList := TFslStringList.Create;
    Try
      oList.AsText := FmemLstList.Text;
      FField.DataValue[FIELD_DATA_NAME_LIST] := oList.AsCSV;
    Finally
      oList.Free;
    End;
    If FKeyEdit.Text <> '' Then
    Begin
      FField.HasHotspot := True;
      FField.Hotspot.Key := FKeyEdit.Text;
    End;
    FField.DataValue[FIELD_DATA_NAME_INT_MIN] := FedIntMinimum.Text;
    FField.DataValue[FIELD_DATA_NAME_INT_MAX] := FedIntMaximum.Text;
    FField.DataValue[FIELD_DATA_NAME_FLT_MAIN] := FedtFloatMain.Text;
    FField.DataValue[FIELD_DATA_NAME_FLT_DECIMAL] := FedtFloatDecimal.Text;
    FField.DataValue[FIELD_DATA_NAME_DATE_TIME] := FchkDateTime.Text;
    FField.DataValue[FIELD_DATA_NAME_TEXT_REGEX] := FedtTextRegex.Text;
    FField.DataValue[FIELD_DATA_NAME_TEXT_DESC] := FedtTextDesc.Text;
    FField.DataValue[FIELD_DATA_NAME_TEXT_MAX] := FedtTextMax.Text;
    FField.DataValue[FIELD_DATA_NAME_LIST_MODE] := FcbxListMode.Text;
  End;
End;


Function TWPInputFieldFrame.DesiredHeight : Integer;
Begin
 Result := 322;
End;


Function TWPInputFieldFrame.DesiredWidth : Integer;
Begin
  Result := 380;
End;


Function TWPInputFieldFrame.GetField : TWPDocumentField;
Begin
  Assert(Invariants('GetField', FField, TWPDocumentField, 'Field'));
  Result := FField;
End;


Procedure TWPInputFieldFrame.SetField(Const Value : TWPDocumentField);
Begin
  FField.Free;
  FField := Value;
End;

Function TWPInputFieldFrame.GetSection : TWPDocumentSection;
Begin
  Assert(Invariants('GetSection', FSection, TWPDocumentSection, 'Section'));
  Result := FSection;
End;


Procedure TWPInputFieldFrame.SetSection(Const Value : TWPDocumentSection);
Begin
  FSection.Free;
  FSection := Value;
End;

Procedure TWPInputFieldFrame.FieldOnly;
Begin
  FrbParagraph.Enabled := False;
  BindField;
End;

Procedure TWPInputFieldFrame.FieldOrSection;
Begin
  FDeletableCheckBox.Checked := True;
  BindField;
End;

Function TWPInputFieldFrame.IsSection: Boolean;
Begin
  Result := FrbParagraph.Checked;
End;

Procedure TWPInputFieldFrame.SectionOnly;
Begin
  FrbBoolean.Enabled := False;
  FrbText.Enabled := False;
  FrbList.Enabled := False;
  FrbNumber.Enabled := False;
  FrbInteger.Enabled := False;
  FrbDate.Enabled := False;
  BindSection;
End;

Procedure TWPInputFieldFrame.BindField;
Var
  oList : TFslStringList;
Begin
  FNameEdit.Text := FField.Name;
  FWidthSpin.Value := FField.Width;
  FDeletableCheckBox.Checked := FField.Deletable;
  FFormatableCombo.Value := ord(FField.FixedFormat);
  FForceInputCheckBox.Checked := FField.DataValue[FIELD_DATA_NAME_FORCE] = 'True';
  FMandatoryCheckBox.Checked := FField.DataValue[FIELD_DATA_NAME_MAND] = 'True';

//  FEditableCheckBox.Checked := Not (FField.ReadOnly = ReadOnlyTrue);
  If FField.HasDataValue(FIELD_DATA_NAME_TYPE) Then
    If FField.DataValue[FIELD_DATA_NAME_TYPE] = 'Boolean' Then
      FrbBoolean.Checked := true
    Else If FField.DataValue[FIELD_DATA_NAME_TYPE] = 'List' Then
      FrbList.Checked := true
    Else If FField.DataValue[FIELD_DATA_NAME_TYPE] = 'Number' Then
      FrbNumber.Checked := true
    Else If FField.DataValue[FIELD_DATA_NAME_TYPE] = 'Integer' Then
      FrbInteger.Checked := true
    Else If FField.DataValue[FIELD_DATA_NAME_TYPE] = 'Date' Then
      FrbDate.Checked := true
    Else // If FField.DataValue[FIELD_DATA_NAME_TYPE] = 'Text' Then
      FrbText.Checked := true
  Else if FField.HasDataValue(FIELD_DATA_NAME_LIST) Then
    FrbList.Checked := true
  Else
    FrbText.Checked := true;

  oList := TFslStringList.Create;
  Try
    oList.AsCSV := FField.DataValue[FIELD_DATA_NAME_LIST];
    FmemLstList.Text := oList.AsText;
  Finally
    oList.Free;
  End;

  If FField.HasHotspot Then
    FKeyEdit.Text := FField.Hotspot.Key
  Else
    FKeyEdit.Text := '';


  FedIntMinimum.Text := FField.DataValue[FIELD_DATA_NAME_INT_MIN];
  FedIntMaximum.Text := FField.DataValue[FIELD_DATA_NAME_INT_MAX];
  FedtFloatMain.Text := FField.DataValue[FIELD_DATA_NAME_FLT_MAIN];
  FedtFloatDecimal.Text := FField.DataValue[FIELD_DATA_NAME_FLT_DECIMAL];
  FchkDateTime.Text := FField.DataValue[FIELD_DATA_NAME_DATE_TIME];
  FedtTextRegex.Text := FField.DataValue[FIELD_DATA_NAME_TEXT_REGEX];
  FedtTextDesc.Text := FField.DataValue[FIELD_DATA_NAME_TEXT_DESC];
  FedtTextMax.Text := FField.DataValue[FIELD_DATA_NAME_TEXT_MAX];
  FcbxListMode.ItemIndex := FcbxListMode.items.indexof(FField.DataValue[FIELD_DATA_NAME_LIST_MODE]);

  If FchkDateTime.Text = '' Then
    FchkDateTime.itemIndex := 0;
  If FcbxListMode.Text = '' Then
    FcbxListMode.itemIndex := 0;
  rbClick(self);
End;

Procedure TWPInputFieldFrame.BindSection;
Begin
  FNameEdit.Text := FSection.Name;
  FWidthSpin.Enabled := false;
  FDeletableCheckBox.Checked := FSection.Deletable;
  FFormatableCombo.Value := 2;
  FFormatableCombo.Enabled := false;
  FForceInputCheckBox.Checked := False;
  FMandatoryCheckBox.Checked := False;
  FrbParagraph.Checked := FSection.IsField;
  If FSection.HasHotspot Then
    FKeyEdit.Text := FSection.Hotspot.Key
  Else
    FKeyEdit.Text := '';
End;

Procedure TWPInputFieldFrame.rbClick(oSender: TObject);
Begin
  FedIntMinimumLabel.Visible := false;
  FedIntMinimum.Visible := false;
  FedIntMaximumLabel.Visible := false;
  FedIntMaximum.Visible := false;
  FedtFloatMainLabel.Visible := false;
  FedtFloatMain.Visible := false;
  FedtFloatDecimalLabel.Visible := false;
  FedtFloatDecimal.Visible := false;
  FchkDateTimeLabel.Visible := false;
  FchkDateTime.Visible := false;
  FedtTextRegexLabel.Visible := false;
  FedtTextRegex.Visible := false;
  FedtTextDescLabel.Visible := false;
  FedtTextDesc.Visible := false;
  FedtTextMaxLabel.Visible := false;
  FedtTextMax.Visible := false;
  FcbxListModeLabel.Visible := false;
  FcbxListMode.Visible := false;
  FmemLstListLabel.Visible := false;
  FmemLstList.Visible := false;

  If FrbBoolean.Checked Then
  Begin
  End
  Else If FrbText.Checked Then
  Begin
    FedtTextRegexLabel.Visible := true;
    FedtTextRegex.Visible := true;
    FedtTextDescLabel.Visible := true;
    FedtTextDesc.Visible := true;
    FedtTextMaxLabel.Visible := true;
    FedtTextMax.Visible := true;
  End
  Else If FrbList.Checked Then
  Begin
    FcbxListModeLabel.Visible := true;
    FcbxListMode.Visible := true;
    FmemLstListLabel.Visible := true;
    FmemLstList.Visible := true;
  End
  Else If FrbParagraph.Checked Then
  Begin
  End
  Else If FrbNumber.Checked Then
  Begin
    FedtFloatMainLabel.Visible := true;
    FedtFloatMain.Visible := true;
    FedtFloatDecimalLabel.Visible := true;
    FedtFloatDecimal.Visible := true;
  End
  Else If FrbInteger.Checked Then
  Begin
    FedIntMinimumLabel.Visible := true;
    FedIntMinimum.Visible := true;
    FedIntMaximumLabel.Visible := true;
    FedIntMaximum.Visible := true;
  End
  Else If FrbDate.Checked Then
  Begin
    FchkDateTimeLabel.Visible := true;
    FchkDateTime.Visible := true;
  End;


  CheckForceInputState(Nil);
End;


Procedure TWPInputFieldFrame.CheckForceInputState(oSender: TObject);
Begin
  FFormatableCombo.Enabled := not (FrbBoolean.Checked or FrbParagraph.Checked);
  FForceInputCheckBox.Enabled := not (FrbBoolean.Checked or FrbParagraph.Checked);
  FMandatoryCheckBox.Enabled := not (FrbBoolean.Checked or FrbParagraph.Checked);
End;

function TWPInputFieldFrame.MakeTypeRadioButton(iTop: integer; const sCaption: String): TUixRadioButton;
begin
  result := TUixRadioButton.Create(FDetailsBox);
  result.Parent := FDetailsBox;
  result.Left := 20;
  result.Top := iTop;
  result.Width := 80;
  result.Caption := sCaption;
  result.OnClick := rbClick;
end;




Function TWPInputFieldDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPInputFieldFrame;
End;


Function TWPInputFieldDialog.DialogCaption : String;
Begin
  Result := 'Input Details';
End;


Function TWPInputFieldDialog.CanAccept: Boolean;
Begin
  Result := Inherited CanAccept;

  If Result Then
  Begin
    Result := Frame.NameEdit.Text <> '';

    If Not Result Then
      Invalid(Frame.NameEdit, 'Name', 'Name must be specified.');
  End;
End;


Function TWPInputFieldDialog.GetFrame : TWPInputFieldFrame;
Begin
  Result := TWPInputFieldFrame(Inherited Frame);
End;


Function TWPInputFieldDialog.GetField : TWPDocumentField;
Begin
  Result := Frame.Field;
End;


Procedure TWPInputFieldDialog.SetField(Const Value : TWPDocumentField);
Begin
  Frame.Field := Value;
End;

Function TWPInputFieldDialog.GetSection : TWPDocumentSection;
Begin
  Result := Frame.Section;
End;


Procedure TWPInputFieldDialog.SetSection(Const Value : TWPDocumentSection);
Begin
  Frame.Section := Value;
End;

Procedure TWPInputFieldDialog.Restore;
Begin
  Inherited;
  ClientHeight := Frame.DesiredHeight + BottomPanel.Height;
  DeclarePrimaryControl(Frame.NameEdit);
End;

procedure TWPInputFieldDialog.FieldOnly;
begin
  Frame.FieldOnly;
end;

procedure TWPInputFieldDialog.FieldOrSection;
begin
  Frame.FieldOrSection;
end;

function TWPInputFieldDialog.IsSection: Boolean;
begin
  Result := Frame.IsSection;
end;

procedure TWPInputFieldDialog.SectionOnly;
begin
  Frame.SectionOnly;
end;

{ TWPCommentDialog }

Function TWPCommentDialog.GetFrame : TWPCommentFrame;
Begin
  Result := TWPCommentFrame(Inherited Frame);
End;


Function TWPCommentDialog.FrameClass : TWPFrameClass;
Begin
  Result := TWPCommentFrame;
End;


Function TWPCommentDialog.DialogCaption : String;
Begin
  Result := 'Comment';
End;


Function TWPCommentDialog.GetComment : String;
Begin
  Result := Frame.Comment;
End;


Procedure TWPCommentDialog.SetComment(Value : String);
Begin
  Frame.Comment := Value;
End;


procedure TWPCommentDialog.Initialise;
var
  oButton : TUixButton;
begin
  inherited;
  oButton := TUixButton.Create(self);
  oButton.Parent := BottomPanel;
  oButton.Top := OKButton.Top;
  oButton.Left := 20;
  oButton.Width := OKButton.Width;
  oButton.Anchors := [akLeft, akTop];
  oButton.Caption := 'Delete';
  oButton.Visible := true;
  oButton.OnClick := DoDelete;
end;


procedure TWPCommentDialog.DoDelete(sender: TObject);
begin
  FWantDelete := true;
  Okay;
end;

{ TWPCommentFrame }

Function TWPCommentFrame.DesiredHeight: Integer;
Begin
  Result := 100;
End;


Function TWPCommentFrame.DesiredWidth: Integer;
Begin
  Result := 490;
End;


Function TWPCommentFrame.GetComment : String;
Begin
  Result := FText.Text;
End;


Procedure TWPCommentFrame.Initialise;
Begin
  Inherited;

  BuildForm;
End;


Procedure TWPCommentFrame.Finalise;
Begin
  Inherited;
End;


Procedure TWPCommentFrame.BuildForm;
Begin
  BorderWidth := 12;
  AddLabel(pnlClient, 13, 4, 'Content:');

  FText := TUixMemo.Create(pnlClient);
  FText.Parent := pnlClient;
  FText.Top := 10;
  FText.Left := 60;
  FText.Width := 400;
  FText.HeightInLines := 4;
  FText.TabOrder := 0;
End;


Procedure TWPCommentFrame.Restore;
Begin
  Inherited;
End;


procedure TWPCommentFrame.SetComment(const Value: String);
begin
  FText.Text := Value;
end;


End.
