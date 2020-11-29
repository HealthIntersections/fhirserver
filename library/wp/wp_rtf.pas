Unit wp_rtf;

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
  Windows, SysUtils, Vcl.Graphics,
  fsl_base, fsl_utilities, fsl_collections, fsl_stream,
  wp_graphics, wp_types, wp_document, wp_working, wp_format, wp_imaging;

Type
  TFslRTFExtractor = Class(TFslTextExtractor)
    Private
      FIsBreak : Boolean;
      FControl : String;

      Function ConsumeControlInner : String;
      Procedure ConsumeWhiteSpace(IncludeSpace : boolean);
      Procedure EatEoln;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      Function PeekIsGroupStart : Boolean;
      Function PeekIsGroupClose : Boolean;
      Function PeekIsControl : Boolean;
      Function PeekIsBreak : Boolean;

      Function PeekControl : String;

      Function PeekIsNumericControl(Const sName : String): Boolean;
      Function ConsumeNumericControl(Const sName : String) : Integer;
      Function PeekIsBooleanControl(Const sName : String): Boolean;
      Function ConsumeBooleanControl(Const sName : String) : Boolean;
      Function PeekIsUnicodeChar(): Boolean;
      Function ReadUnicodeChar(): Char;

      Procedure ConsumeGroupStart;
      Procedure ConsumeGroupClose;

      Function ConsumeControl : String;
      Function ConsumeText : String;
      Function ConsumeBreak : String;
  End;

  TFslRTFFormatter = Class(TFslTextFormatter)
    Private
      FWasControl : Boolean;
      FLinelimit : Integer; // 0 = no lines introduced
      FLineCount : Integer;
      Procedure CheckNewLine;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      Procedure StartGroup; Overload; Virtual;
      Procedure CloseGroup; Overload; Virtual;
      Procedure Control(Const sWord : String); Overload; Virtual;
      Procedure Break(Const sWord : String); Overload; Virtual;
      Procedure ControlBoolean(Const sWord : String; Const bOn : Boolean); Overload; Virtual;
      Procedure Text(Const sText : String); Overload; Virtual;

      Property LineLimit : Integer Read FLineLimit Write FLineLimit;
  End;

  TWPRTFContext = Class(TFslObject)
    Private
      FFontName : String;
      FStyle : String;
      FFontSize : Integer;
      FBold : Boolean;
      FItalic : Boolean;
      FUnderline : Boolean;
      FCapitalization : Boolean;
      FStrikethrough : Boolean;
      FState : TWPSFontState;
      FForeground : TColour;
      FBackground : TColour;
      FAlignment: TWordProcessorParagraphAlignment;
      FLeftIndent: Integer;
      FRightIndent: Integer;
    Protected
      Function ErrorClass : EFslExceptionClass; Overload; Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPRTFContext; Overload;
      Function Clone : TWPRTFContext; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Procedure Plain;
      Procedure Pard;

      Property FontName : String Read FFontName Write FFontName;
      Property Style : String Read FStyle Write FStyle;
      Property FontSize : Integer Read FFontSize Write FFontSize;
      Property Bold : Boolean Read FBold Write FBold;
      Property Italic : Boolean Read FItalic Write FItalic;
      Property Underline : Boolean Read FUnderline Write FUnderline;
      Property Strikethrough : Boolean Read FStrikethrough Write FStrikethrough;
      Property Capitalization : Boolean Read FCapitalization Write FCapitalization;
      Property Foreground : TColour Read FForeground Write FForeground;
      Property Background : TColour Read FBackground Write FBackground;
      Property State : TWPSFontState Read FState Write FState;

      Property Alignment : TWordProcessorParagraphAlignment Read FAlignment Write FAlignment;
      Property LeftIndent : Integer Read FLeftIndent Write FLeftIndent;
      Property RightIndent : Integer Read FRightIndent Write FRightIndent;
  End;

  EWPRTFContext = Class(EFslException)
  End;

  TWPRTFContexts = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPRTFContext;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPRTFContext);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPRTFContext; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPRTFContexts; Overload;
      Function Clone : TWPRTFContexts; Overload;

      Function New : TWPRTFContext; Reintroduce; Overload; Virtual;
      Function NewContext(Const sDefaultStyle : String) : TWPRTFContext; Overload; Virtual;
      Function CloseContext: TWPRTFContext; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPRTFContext Read GetElement Write SetElement; Default;
  End;




Const
  NFC_Arabic = 0;
  NFC_Uppercase_Roman_numeral = 1;
  NFC_Lowercase_Roman_numeral = 2;
  NFC_Uppercase_letter = 3;
  NFC_Lowercase_letter = 4;
  NFC_Ordinal_number = 5;
  NFC_Cardinal_text_number = 6;
  NFC_Ordinal_text_number = 7;
  NFC_Kanji_numbering_without_the_digit_character = 10;
  NFC_Kanji_numbering_with_the_digit_character = 11;
  NFC_46_phonetic_katakana_characters_in_aiueo_order = 12;
  NFC_46_phonetic_katakana_characters_in_iroha_order = 13;
  NFC_Double_byte_character = 14;
  NFC_Single_byte_character = 15;
  NFC_Kanji_numbering_3 = 16;
  NFC_Kanji_numbering_4 = 17;
  NFC_Circle_numbering = 18;
  NFC_Double_byte_Arabic_numbering = 19;
  NFC_46_phonetic_double_byte_katakana_characters = 20;
  NFC_46_phonetic_double_byte_katakana_charactersB = 21;
  NFC_Arabic_with_leading_zero = 22;
  NFC_Bullet = 23;
  NFC_Korean_numbering_2 = 24;
  NFC_Korean_numbering_1 = 25;
  NFC_Chinese_numbering_1 = 26;
  NFC_Chinese_numbering_2 = 27;
  NFC_Chinese_numbering_3 = 28;
  NFC_Chinese_numbering_4 = 29;
  NFC_Chinese_Zodiac_numbering_1 = 30;
  NFC_Chinese_Zodiac_numbering_2 = 31;
  NFC_Chinese_Zodiac_numbering_3 = 32;
  NFC_Taiwanese_double_byte_numbering_1 = 33;
  NFC_Taiwanese_double_byte_numbering_2 = 34;
  NFC_Taiwanese_double_byte_numbering_3 = 35;
  NFC_Taiwanese_double_byte_numbering_4 = 36;
  NFC_Chinese_double_byte_numbering_1 = 37;
  NFC_Chinese_double_byte_numbering_2 = 38;
  NFC_Chinese_double_byte_numbering_3 = 39;
  NFC_Chinese_double_byte_numbering_4 = 40;
  NFC_Korean_double_byte_numbering_1 = 41;
  NFC_Korean_double_byte_numbering_2 = 42;
  NFC_Korean_double_byte_numbering_3 = 43;
  NFC_Korean_double_byte_numbering_4 = 44;
  NFC_Hebrew_non_standard_decimal = 45;
  NFC_Arabic_Alif_Ba_Tah = 46;
  NFC_Hebrew_Biblical_standard = 47;
  NFC_Arabic_Abjad_style = 48;
  NFC_No_number = 255;

Type
  TWPRTFListLevelDefinition = Class(TWPRTFContext)
    Private
      FStart : Integer;
      FNumberFormat : Integer;
    Protected

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPRTFListLevelDefinition; Overload;
      Function Clone : TWPRTFListLevelDefinition; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Property Start : Integer Read FStart Write FStart;
      Property NumberFormat : Integer Read FNumberFormat Write FNumberFormat;
  End;

  TWPRTFListLevelDefinitions = Class(TWPRTFContexts)
    Private
      Function GetElement(Const iIndex : Integer) : TWPRTFListLevelDefinition;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPRTFListLevelDefinition);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPRTFListLevelDefinition; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPRTFListLevelDefinitions; Overload;
      Function Clone : TWPRTFListLevelDefinitions; Overload;

      Function New : TWPRTFListLevelDefinition; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPRTFListLevelDefinition Read GetElement Write SetElement; Default;
  End;

Type
  TWPRTFListDefinition = Class(TFslObject)
    Private
      FId : Integer;
      FTemplateId : Integer;
      FName : String;
      FLevels : TWPRTFListLevelDefinitions;
    Protected

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPRTFListDefinition; Overload;
      Function Clone : TWPRTFListDefinition; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Property Id : Integer Read FId Write FId;
      Property TemplateId : Integer Read FTemplateId Write FTemplateId;
      Property Name : String Read FName Write FName;
      Property Levels : TWPRTFListLevelDefinitions Read FLevels;
  End;

  TWPRTFListDefinitions = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPRTFListDefinition;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPRTFListDefinition);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPRTFListDefinition; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPRTFListDefinitions; Overload;
      Function Clone : TWPRTFListDefinitions; Overload;

      Function New : TWPRTFListDefinition; Reintroduce; Overload; Virtual;

      Function GetById(iId: Integer): TWPRTFListDefinition;

      Property Elements[Const iIndex : Integer] : TWPRTFListDefinition Read GetElement Write SetElement; Default;
  End;



Type
  TWPRTFListLevelOverrideDefinition = Class(TWPRTFContext)
    Private
      FStart : Integer;
    Protected

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPRTFListLevelOverrideDefinition; Overload;
      Function Clone : TWPRTFListLevelOverrideDefinition; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Property Start : Integer Read FStart Write FStart;
  End;

  TWPRTFListLevelOverrideDefinitions = Class(TWPRTFContexts)
    Private
      Function GetElement(Const iIndex : Integer) : TWPRTFListLevelOverrideDefinition;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPRTFListLevelOverrideDefinition);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPRTFListLevelOverrideDefinition; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPRTFListLevelOverrideDefinitions; Overload;
      Function Clone : TWPRTFListLevelOverrideDefinitions; Overload;

      Function New : TWPRTFListLevelOverrideDefinition; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPRTFListLevelOverrideDefinition Read GetElement Write SetElement; Default;
  End;


Type
  TWPRTFListOverrideDefinition = Class(TFslObject)
    Private
      FId : Integer;
      FListId : Integer;
      FLevels : TWPRTFListLevelOverrideDefinitions;
    Protected

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPRTFListOverrideDefinition; Overload;
      Function Clone : TWPRTFListOverrideDefinition; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Property Id : Integer Read FId Write FId;
      Property ListId : Integer Read FListId Write FListId;
      Property Levels : TWPRTFListLevelOverrideDefinitions Read FLevels;
  End;

  TWPRTFListOverrideDefinitions = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPRTFListOverrideDefinition;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPRTFListOverrideDefinition);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPRTFListOverrideDefinition; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPRTFListOverrideDefinitions; Overload;
      Function Clone : TWPRTFListOverrideDefinitions; Overload;

      Function New : TWPRTFListOverrideDefinition; Reintroduce; Overload; Virtual;

      Function GetById(iId : Integer): TWPRTFListOverrideDefinition;

      Property Elements[Const iIndex : Integer] : TWPRTFListOverrideDefinition Read GetElement Write SetElement; Default;
  End;

Const
  TWIP = 15;
  NDX_RowTop = 1;
  NDX_RowLeft = 2;
  NDX_RowRight = 3;
  NDX_RowBottom = 4;
  NDX_CellTop = 5;
  NDX_CellLeft = 6;
  NDX_CellRight = 7;
  NDX_CellBottom = 8;

Type
  TWPRTFReaderAllowedItem = (WPRTFReaderAllowedItemSection, WPRTFReaderAllowedItemParagraph,
    WPRTFReaderAllowedItemField, WPRTFReaderAllowedItemText, WPRTFReaderAllowedItemTable,
    WPRTFReaderAllowedItemRow, WPRTFReaderAllowedItemCell, WPRTFReaderAllowedItemLine);

  TWPRTFReaderAllowedItems = Set Of TWPRTFReaderAllowedItem;

  TWPRTFTableBorderInfo = Record
    Pen : TFslPenStyle;
    Width : Integer;
    Colour : TColour;
  End;

  TWPRTFTableInfo = Record
    InTable : Boolean;
    RowNeeded : Boolean;
    CellNeeded : Boolean;
    LastRow : Boolean;
    TablePadding : Integer;
    IsHeader : Boolean;
    Background : TColour;
    LastRowPiece : TWPWorkingDocumentTableRowStartPiece;
    LastCellPiece : TWPWorkingDocumentTableCellStartPiece;
    activeBorder : Integer;
    borders : Array [NDX_RowTop .. NDX_CellBottom] Of TWPRTFTableBorderInfo;
  End;

  TWPRTFReader = Class (TWPReader)
    Private
      FLastWasText : Boolean;
      FFirstColourDefault : Boolean;
      FHasColours : Boolean;
      FHasFonts : Boolean;
      FHasStylesheet : Boolean;
      FColours : TFslIntegerList;
      FFonts : TFslStringIntegerMatch;
      FDefaultFont : Integer;
      FLists : TWPRTFListDefinitions;
      FListOverrides : TWPRTFListOverrideDefinitions;
      FListStyleId : Integer;
      FListTextFontName : String;
      FListText : String;
      FListTextUsed : Boolean;
      FLastListNumber : Integer;
      FTable : TWPRTFTableInfo;
      FLastPara : TWPWorkingDocumentParaPiece;
      FIgnoreBackground: Boolean;

      Function GetHasLists: Boolean;
      Procedure SetHasLists(Const Value: Boolean);
      Function GetHasListOverrides: Boolean;
      Procedure SetHasListOverrides(Const Value: Boolean);

      Procedure Initialise;
      Procedure InitTable;
      Procedure InitRow;
      Procedure InitCell;
      Procedure CheckRTF(bTest : Boolean; Const sMessage : String);
      Procedure CheckHeader(oRTF : TFslRTFExtractor);

      Procedure Bypass(oRTF : TFslRTFExtractor; bConsumeClose : Boolean; Const sWhy : String);

      Function GetColor(iIndex : Integer):TColour;

      Procedure ReadColor(oRTF : TFslRTFExtractor);
      Procedure ReadColorTable(oRTF : TFslRTFExtractor);
      Procedure ReadFont(oRTF : TFslRTFExtractor);
      Procedure ReadFontTable(oRTF : TFslRTFExtractor);
      Procedure ReadStyle(oRTF : TFslRTFExtractor; oContext : TWPRTFContext);
      Procedure ReadStylesheet(oRTF : TFslRTFExtractor; oContext : TWPRTFContext);
      Procedure ReadListLevel(oList : TWPRTFListDefinition; oRTF : TFslRTFExtractor);
      Procedure ReadList(oRTF : TFslRTFExtractor);
      Procedure ReadListTable(oRTF : TFslRTFExtractor);
      Procedure ReadListLevelOverride(oList : TWPRTFListOverrideDefinition; oRTF : TFslRTFExtractor);
      Procedure ReadListOverride(oRTF : TFslRTFExtractor);
      Procedure ReadListOverrideTable(oRTF : TFslRTFExtractor);
      Procedure ReadImage(oDocument : TWPWorkingDocument; oRTF : TFslRTFExtractor);
      Procedure LoadImage(oImage : TWPWorkingDocumentImagePiece; aFormat : TWPImageFormat; oRTF : TFslRTFExtractor); Overload;
      Procedure ReadImageProperties(oImage : TWPWorkingDocumentImagePiece; oRTF : TFslRTFExtractor);

      Procedure ReadListStyle(oPara : TWPWorkingDocumentParaPiece);
      Procedure ReadStyleContext(oPiece : TWPWorkingDocumentPiece; oContext : TWPRTFContext);

      Procedure AddPageBreak(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);

      Procedure AddPara(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
      Procedure AddTextPiece(oDocument : TWPWorkingDocument; Const sText : String; oContext : TWPRTFContext);
      Procedure ReadText(oDocument : TWPWorkingDocument; Const sText : String; oContext : TWPRTFContext);

      Procedure trowd(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
      Procedure cell(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
      Procedure row(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);

      Procedure DoBorder(oBorder : TWPBorder; iCell, iRow : Integer);
      Procedure StartTable(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
      Procedure EndTable(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
      Procedure StartRow(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
      Procedure EndRow(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
      Procedure StartCell(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
      Procedure EndCell(oDocument : TWPWorkingDocument; oContext : TWPRTFContext; bEndRow : Boolean);

      Procedure ProcessListText(oRTF : TFslRTFExtractor; oContext : TWPRTFContext);
      Procedure ReadListText(oPara : TWPWorkingDocumentParaPiece);

      Function ReadStyleControl(oRTF : TFslRTFExtractor; oContext : TWPRTFContext): Boolean;
      Procedure ReadControl(oDocument : TWPWorkingDocument; oRTF : TFslRTFExtractor; oContext : TWPRTFContext; aAllowed : TWPRTFReaderAllowedItems);
      Procedure Read(oDocument : TWPWorkingDocument; oRTF : TFslRTFExtractor; oContext : TWPRTFContext; aAllowed : TWPRTFReaderAllowedItems); Overload;
      Function ReadHexChar(s : String) : String;

      Property HasLists : Boolean Read GetHasLists Write SetHasLists;
      Property HasListOverrides : Boolean Read GetHasListOverrides Write SetHasListOverrides;

      Function DPIX: Integer;
      Function DPIY: Integer;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Property IgnoreBackground : Boolean Read FIgnoreBackground Write FIgnoreBackground;

      Procedure Read(oDocument : TWPWorkingDocument); Overload; Override;
  End;


Type
  TWPRTFWriter = Class (TWPWriter)
    Private
      FFonts : TFslStringList;
      FContexts : TWPRTFContexts;
      FRTF : TFslRTFFormatter;
      FContext : TWPRTFContext;
      FColours : TFslIntegerList;

      // list of override 'list' (i.e. para pieces)
      FLists : TWPWorkingDocumentPieces;
      FListsIndexer: TFslStringList;
      bInList: Boolean;
      FLineLimit: Integer;

      Function GetFonts : TFslStringList;
      Function GetContexts : TWPRTFContexts;
      Function GetRTF : TFslRTFFormatter;
      Function GetContext : TWPRTFContext;
      Function GetColours : TFslIntegerList;

      Property Fonts : TFslStringList Read GetFonts;
      Property Contexts : TWPRTFContexts Read GetContexts;
      Property Context : TWPRTFContext Read GetContext;
      Property Colours : TFslIntegerList Read GetColours;

      Function GetPieceStyle(oPiece : TWPWorkingDocumentPiece) : TWPStyle;

      Procedure EnumerateStyles;
      Procedure WriteFont(oRTF : TFslRTFFormatter; iIndex : Integer; Const sName : String);
      Procedure WriteFontTable(oRTF : TFslRTFFormatter);
      Procedure WriteStyle(oRTF : TFslRTFFormatter; iIndex : Integer; oStyle : TWPStyle);
      Procedure WriteStyleSheet(oRTF : TFslRTFFormatter);
      Procedure WriteColour(oRTF : TFslRTFFormatter; aColour : TColour);
      Procedure WriteColourTable(oRTF : TFslRTFFormatter);
      Procedure WriteListTable(oRTF: TFslRTFFormatter);
      Procedure WriteBulletType(oRTF: TFslRTFFormatter; Const oType: TWPSParagraphBulletType);
      Procedure WriteNumberedType(oRTF: TFslRTFFormatter; Const oFormat: TWPSParagraphDetails);
      Procedure WriteHeader(oRTF : TFslRTFFormatter);
      Procedure SaveRTF;

      Procedure SaveJPEG(oImage: TFslVCLGraphic);
      Procedure SavePNG(oImage: TFslVCLGraphic);
      Procedure WriteImageName(Const sName : String);

      Procedure WriteTableRowFormat(oTableRow : TWPWorkingDocumentTableRowStartPiece);
      Procedure WriteCellBorders(Const oTop, oBottom, oLeft, oRight: TWPBorder);
      Procedure WriteBorder(Const oBorder: TWPBorder);

      Function ColorIndex(Const aColour : TColour) : String;
      Function FontIndex(Const sFont : String) : String;
      Function ListIndex(Const oPara: TWPWorkingDocumentParaPiece): String;
      Function EncodeListFormat(Const oFormat: TWPSParagraphDetails): String;

      Function EncodeURL(Const sURL: String): String;
    Protected
      Procedure Initialise; Override;
      Procedure Finalise; Override;

      Procedure WriteTextContent(oPiece : TWPWorkingDocumentPiece; Const sText : String); Virtual;

      Property RTF : TFslRTFFormatter Read GetRTF;
      Procedure StartPara;
      Procedure StopPara;
      Procedure MakeNewContext();
      Procedure CloseExistingContext();

      Procedure WriteText(oText : TWPWorkingDocumentTextPiece); Override;
      Procedure WriteImage(oImage : TWPWorkingDocumentImagePiece); Override;
      Procedure WriteFieldStart(oField : TWPWorkingDocumentFieldStartPiece); Override;
      Procedure WriteFieldStop(oField : TWPWorkingDocumentFieldStartPiece; oStop : TWPWorkingDocumentFieldStopPiece); Override;
      Procedure IterateParagraph(oIterator : TWPPieceIterator; oSection : TWPWorkingDocumentSectionStartPiece); Override;
      Procedure WriteParagraphStart(oPara : TWPWorkingDocumentParaPiece); Override;
      Procedure WriteParagraphStop(oPara : TWPWorkingDocumentParaPiece; bNextIsSection : Boolean; oSection : TWPWorkingDocumentSectionStartPiece); Override;
      Procedure WriteLineBreak(oBreak : TWPWorkingDocumentLineBreakPiece); Override;
      Procedure WriteBreak(oBreak : TWPWorkingDocumentBreakPiece); Override;
  //    Procedure WriteTableStart(oTable : TWPWorkingDocumentTableStartPiece); Override;
  //    Procedure WriteTableStop(oTable : TWPWorkingDocumentTableStartPiece; oStop : TWPWorkingDocumentStopPiece); Override;
      Procedure WriteTableRowStart(oTableRow : TWPWorkingDocumentTableRowStartPiece); Override;
      Procedure WriteTableRowStop(oTableRow : TWPWorkingDocumentTableRowStartPiece; oStop : TWPWorkingDocumentStopPiece; bIsLast : Boolean); Override;
  //  Procedure WriteTableCellStart(oTableCell : TWPWorkingDocumentTableCellStartPiece); Override;
      Procedure WriteTableCellStop(oTableCell : TWPWorkingDocumentTableCellStartPiece; oStop : TWPWorkingDocumentStopPiece); Override;
      Procedure WriteSectionStart(oSection : TWPWorkingDocumentSectionStartPiece); Override;
      Procedure WriteSectionStop(oSection : TWPWorkingDocumentSectionStartPiece; oStop : TWPWorkingDocumentStopPiece); Override;
  //    Procedure WriteDocumentStart(oDocument : TWPWorkingDocument); Override;
  //    Procedure WriteDocumentStop(oDocument : TWPWorkingDocument); Override;
      Function Styled : Boolean; Virtual;
    function sizeInBytesV : cardinal; override;
    Public
      destructor Destroy; Override;
      Property LineLimit : Integer Read FLineLimit Write FLineLimit;
  End;



Implementation


Constructor TWPRTFReader.Create;
Begin
  Inherited;

  FColours := TFslIntegerList.Create;
  FFonts := TFslStringIntegerMatch.Create;
  FFonts.SortedByValue;
End;


Destructor TWPRTFReader.Destroy;
Begin
  FFonts.Free;
  FColours.Free;
  FLists.Free;
  FListOverrides.Free;
  Inherited;
End;


Procedure TWPRTFReader.Initialise;
Begin
  FLastWasText := True;
  FHasColours := False;
  FHasFonts := False;
  FHasStylesheet := False;
  FColours.Clear;
  FFonts.Clear;
  FDefaultFont := -1;
  FLists := Nil;
  FLastListNumber := 1;
  FListStyleId := -1;
  InitTable;
End;

Procedure TWPRTFReader.InitTable;
Var
  i : Integer;
Begin
  FTable.InTable := False;
  FTable.RowNeeded := False;
  FTable.TablePadding := -1;
  For i := NDX_RowTop To NDX_RowBottom Do
  Begin
    FTable.borders[i].pen := apsNone;
    FTable.borders[i].Width := 0;
    FTable.borders[i].Colour := DEF_COLOUR;
  End;
  InitRow;
End;


Procedure TWPRTFReader.InitRow;
Var
  i : Integer;
Begin
  FTable.CellNeeded := FTable.InTable;
  FTable.LastRow := False;
  FTable.IsHeader := False;
  FTable.Background := DEF_COLOUR;
  FTable.LastRowPiece := Nil;
  For i := NDX_CellTop To NDX_CellBottom Do
  Begin
    FTable.borders[i].pen := apsNone;
    FTable.borders[i].Width := 0;
    FTable.borders[i].Colour := DEF_COLOUR;
  End;
  InitCell;
End;

Procedure TWPRTFReader.InitCell;
Begin
  FTable.LastCellPiece := Nil;
  FTable.activeBorder := -1;
End;

Procedure TWPRTFReader.Read(oDocument : TWPWorkingDocument);
Var
  oRTF : TFslRTFExtractor;
  oContext : TWPRTFContext;
Begin
  Initialise;

  oRTF := TFslRTFExtractor.Create(Stream.Link);
  Try

    CheckHeader(oRTF);

    oContext := TWPRTFContext.Create;
    Try
      Read(oDocument, oRTF, oContext, [WPRTFReaderAllowedItemSection, WPRTFReaderAllowedItemParagraph, WPRTFReaderAllowedItemTable]);
    Finally
      oContext.Free;
    End;
  Finally
    oRTF.Free;
  End;

  If FLastWasText And MustCloseWithPara Then
    oDocument.NewParagraph;

  DoneReading(oDocument);
  If FLastPara <> Nil Then
    FLastPara.SpeechMagicDouble := True;
End;


Procedure TWPRTFReader.CheckRTF(bTest : Boolean; Const sMessage : String);
Begin
  If Not bTest Then
    RaiseError('CheckRTF', 'Error reading RTF: '+sMessage);
End;


Procedure TWPRTFReader.CHeckHeader(oRTF : TFslRTFExtractor);
Begin
  oRTF.ConsumeGroupStart;
  CheckRTF(oRTF.ConsumeControl = 'rtf1', 'Reading RTF Header');
End;


Procedure TWPRTFReader.ReadControl(oDocument : TWPWorkingDocument; oRTF : TFslRTFExtractor; oContext : TWPRTFContext; aAllowed : TWPRTFReaderAllowedItems);
Var
  sControl : String;
Begin

  If Not ReadStyleControl(oRTF, oContext) Then
  if oRTF.PeekIsUnicodeChar then
    ReadText(oDocument, oRTF.ReadUnicodeChar, oContext)
  Else
  Begin
    sControl := oRTF.ConsumeControl;
    If sControl = '~' Then
      ReadText(oDocument, ' ', oContext)
    Else If sControl = '*' Then
    Begin
      sControl := oRTF.ConsumeControl;
      If sControl = 'listtable' Then
        ReadListTable(oRTF)
      Else If sControl = 'listoverridetable' Then
        ReadListOverrideTable(oRTF)
      Else If sControl = 'shppict' Then
        ReadImage(oDocument, oRTF)
      Else
        Bypass(oRTF, False, sControl);
    End
    Else If sControl = 'colortbl' Then
      ReadColorTable(oRTF)
    Else If sControl = 'fonttbl' Then
      ReadFontTable(oRTF)
    Else If sControl = 'stylesheet' Then
      ReadStylesheet(oRTF, oContext)
    Else If sControl = 'listtext' Then
      ProcessListText(oRTF, oContext)
    Else If sControl = 'info' Then
      ByPass(oRTF, False, '..info')
    Else If sControl = 'pict' Then
      ByPass(oRTF, False, '..pict')
    Else If (sControl = 'par') Then
      AddPara(oDocument, oContext)
    Else If ((sControl = 'trowd') Or (sControl = 'intbl')) And (WPRTFReaderAllowedItemTable In aAllowed) Then
      trowd(oDocument, oContext)
    Else If(sControl = 'cell') And (WPRTFReaderAllowedItemTable In aAllowed) Then
      cell(oDocument, oContext)
    Else If(sControl = 'row') And (WPRTFReaderAllowedItemTable In aAllowed) Then
      row(oDocument, oContext)
    Else If(sControl = 'lastrow') And (WPRTFReaderAllowedItemTable In aAllowed) Then
      FTable.LastRow := True
    Else If (sControl = 'pard') Then
    Begin
      oContext.Pard;
      If (FListTextUsed) Then
      Begin
        FListStyleId := -1;
        FListTextFontName := '';
        FListText := '';
      End;
    End
    Else If (sControl = 'page') Then
      AddPageBreak(oDocument, oContext)
    Else If (sControl[1] = '''') Then
      ReadText(oDocument, readHexChar(Copy(sControl, 2, 2)), oContext)
    Else If (sControl = 'lquote') Then
      ReadText(oDocument, readHexChar('91'), oContext)
    Else If (sControl = 'rquote') Then
      ReadText(oDocument, readHexChar('92'), oContext)
    Else If (sControl = 'ldblquote') Then
      ReadText(oDocument, readHexChar('93'), oContext)
    Else If (sControl = 'rdblquote') Then
      ReadText(oDocument, readHexChar('94'), oContext)
    Else If (Length(sControl) = 1) Then
    Begin
      // Accept special escaped characters
      If (sControl = '\') Or (sControl = '{') Or (sControl = '}') Then
        ReadText(oDocument, sControl, oContext);
    End
    Else
      ;
  End;
End;


Procedure TWPRTFReader.trowd(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
Begin
  If (Not FTable.InTable) Then
    StartTable(oDocument, oContext);
End;

Procedure TWPRTFReader.cell(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
Begin
  EndCell(oDocument, oContext, False);
  FTable.CellNeeded := True;
End;

Procedure TWPRTFReader.row(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
Begin
  EndCell(oDocument, oContext, True);
  EndRow(oDocument, oContext);
//  If FTable.LastRow Then
  EndTable(oDocument, oContext);
End;



Procedure TWPRTFReader.StartTable(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
Var
  oTable : TWPWorkingDocumentTableStartPiece;
Begin
  If (oDocument.Pieces.Count > 0) And (oDocument.Pieces.Last().PieceType = ptTableStop) Then
  Begin
    // append to previous table
    oDocument.Pieces.RemoveLast().Free();
  End
  Else
  Begin
    // create new table if can't append to previous table
    oTable := TWPWorkingDocumentTableStartPiece.Create;
    Try
      oDocument.Pieces.Add(oTable.Link);
    Finally
      oTable.Free;
    End;
  End;

  FTable.InTable := True;
  FTable.RowNeeded := True;
  FTable.LastRow := False;
  FTable.CellNeeded := True;
  FTable.TablePadding := -1;
End;

Procedure TWPRTFReader.EndTable(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
Var
  oTable : TWPWorkingDocumentStopPiece;
Begin
  oTable := TWPWorkingDocumentStopPiece.Create;
  oTable.StopType := stTable;
  Try
    oDocument.Pieces.Add(oTable.Link);
  Finally
    oTable.Free;
  End;
  InitTable;
End;

Procedure TWPRTFReader.StartRow(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
Var
  oTableRow : TWPWorkingDocumentTableRowStartPiece;
Begin
  oTableRow := TWPWorkingDocumentTableRowStartPiece.Create;
  Try
    FTable.LastRowPiece := oTableRow;
    oDocument.Pieces.Add(oTableRow.Link);
  Finally
    oTableRow.Free;
  End;
  FTable.RowNeeded := False;
End;

Procedure TWPRTFReader.EndRow(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
Var
  oTableRow : TWPWorkingDocumentStopPiece;
Begin

  FTable.LastRowPiece.Header := FTable.IsHeader;
  FTable.IsHeader := False;
  If FTable.Background <> DEF_COLOUR Then
    FTable.LastRowPiece.Background := FTable.Background;
  FTable.Background := DEF_COLOUR;

  oTableRow := TWPWorkingDocumentStopPiece.Create;
  oTableRow.StopType := stTableRow;
  Try
    oDocument.Pieces.Add(oTableRow.Link);
  Finally
    oTableRow.Free;
  End;
  FTable.RowNeeded := True;
  InitRow;
End;

Procedure TWPRTFReader.StartCell(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
Var
  oTableCell : TWPWorkingDocumentTableCellStartPiece;
Begin
  If FTable.RowNeeded Then
    StartRow(oDocument, oContext);

  oTableCell := TWPWorkingDocumentTableCellStartPiece.Create;
  Try
    FTable.LastCellPiece := oTableCell;
    oDocument.Pieces.Add(oTableCell.Link);
  Finally
    oTableCell.Free;
  End;
  FTable.CellNeeded := False;
  FLastWasText := True;
End;

Procedure TWPRTFReader.DoBorder(oBorder : TWPBorder; iCell, iRow : Integer);
Begin
  oBorder.Clear;
  If FTable.borders[iCell].Pen <> apsNone Then
    oBorder.Style := FTable.borders[iCell].Pen
  Else If FTable.borders[iRow].Pen <> apsNone Then
    oBorder.Style := FTable.borders[iRow].Pen;

  oBorder.Defined := oBorder.Style <> apsNone;

  If FTable.borders[iCell].Width <> 0 Then
    oBorder.Width := FTable.borders[iCell].Width
  Else If FTable.borders[iRow].Width <> 0 Then
    oBorder.Width := FTable.borders[iRow].Width;

  If FTable.borders[iCell].Colour <> 0 Then
    oBorder.Colour := FTable.borders[iCell].Colour
  Else If FTable.borders[iRow].Colour <> DEF_COLOUR Then
    oBorder.Colour := FTable.borders[iRow].Colour;
End;

Procedure TWPRTFReader.EndCell(oDocument : TWPWorkingDocument; oContext : TWPRTFContext; bEndRow : Boolean);
Var
  oTableCell : TWPWorkingDocumentStopPiece;
Begin
  If (Not bEndRow) Or (Not FTable.CellNeeded) Then
  Begin
    If FTable.CellNeeded Then
      StartCell(oDocument, oContext);

    If FLastWasText Then
      AddPara(oDocument, oContext);

    If FTable.LastCellPiece <> Nil Then
    Begin
      If (FTable.TablePadding <> -1) Then
      Begin
        FTable.LastCellPiece.MarginLeft := FTable.TablePadding;
        FTable.LastCellPiece.MarginRight := FTable.TablePadding;
      End;
      doBorder(FTable.LastCellPiece.TopBorder, NDX_CellTop, NDX_RowTop);
      doBorder(FTable.LastCellPiece.LeftBorder, NDX_CellLeft, NDX_RowLeft);
      doBorder(FTable.LastCellPiece.RightBorder, NDX_CellRight, NDX_RowRight);
      doBorder(FTable.LastCellPiece.BottomBorder, NDX_CellBottom, NDX_RowBottom);
    End;
    
    oTableCell := TWPWorkingDocumentStopPiece.Create;
    oTableCell.StopType := stTableCell;
    Try
      oDocument.Pieces.Add(oTableCell.Link);
    Finally
      oTableCell.Free;
    End;
    FTable.CellNeeded := True;
  End;
  InitCell;
End;


Function TWPRTFReader.ReadHexChar(s : String) : String;
Begin
  s := StringStrip(s, setWhitespace);
  Assert(Length(s) = 2);
  Result := Char(StringToInteger32('$'+s));
End;

Procedure TWPRTFReader.Read(oDocument : TWPWorkingDocument; oRTF : TFslRTFExtractor; oContext : TWPRTFContext; aAllowed : TWPRTFReaderAllowedItems);
Var
  oNew : TWPRTFContext;
Begin
  While oRTF.More And Not oRTF.PeekIsGroupClose Do
  Begin
    If oRTF.PeekIsGroupStart Then
    Begin
      oRTF.ConsumeGroupStart;
      oNew := oContext.Clone;
      Try
        Read(oDocument, oRTF, oNew, aAllowed);
      Finally
        oNew.Free;
      End;
    End
    Else If oRTF.PeekIsControl Then
      ReadControl(oDocument, oRTF, oContext, aAllowed)
    Else If oRTF.PeekIsBreak Then
      oRTF.ConsumeBreak
    Else
      ReadText(oDocument, oRTF.ConsumeText, oContext);
  End;
  If oRTF.More Then
    oRTF.ConsumeGroupClose;
End;


Procedure TWPRTFReader.AddPara(oDocument : TWPWorkingDocument; oContext : TWPRTFContext);
Var
  oPara : TWPWorkingDocumentParaPiece;
Begin
  If FTable.CellNeeded Then
    StartCell(oDocument, oContext);

  oPara := TWPWorkingDocumentParaPiece.Create;
  Try
    oPara.SpeechMagicDouble := True;
    ReadStyleContext(oPara, oContext);

    If oContext.Alignment <> WordProcessorParagraphAlignmentUnknown Then
      oPara.Format.Align := oContext.Alignment;
    If oContext.LeftIndent <> DEF_INT Then
      oPara.Format.LeftIndent := oContext.LeftIndent;
    If oContext.RightIndent <> DEF_INT Then
      oPara.Format.RightIndent := oContext.RightIndent;
    If FListStyleId <> -1 Then
      ReadListStyle(oPara);

    oDocument.Pieces.Add(oPara.Link);
    FLastPara := oPara;
  Finally
    oPara.Free;
  End;
  FLastWasText := False;
End;


Procedure TWPRTFReader.AddTextPiece(oDocument : TWPWorkingDocument; Const sText : String; oContext : TWPRTFContext);
Var
  oText : TWPWorkingDocumentTextPiece;
  sWorkingText : String;
Begin
  If FTable.CellNeeded Then
    StartCell(oDocument, oContext);

  sWorkingText := sText;

  While Length(sWorkingText) > MAX_WORD_LENGTH Do
  Begin
    AddTextPiece(oDocument, Copy(sWorkingText, 1, MAX_WORD_LENGTH), oContext);
    sWorkingText := Copy(sWorkingText, MAX_WORD_LENGTH + 1, MAXINT);
  End;

  oText := TWPWorkingDocumentTextPiece.Create;
  Try
    ReadStyleContext(oText, oContext);
    oText.Content := sWorkingText;

//    debug(' '+oText.Content, 0);
    oDocument.Pieces.Add(oText.Link);
  Finally
    oText.Free;
  End;
  FLastWasText := True;
End;


Procedure TWPRTFReader.ReadText(oDocument : TWPWorkingDocument; Const sText : String; oContext : TWPRTFContext);
Begin
  Splitter.Init(StringReplace(StringReplace(sText, cReturn, ''), [#9], ' '));

  While Splitter.More Do
    AddTextPiece(oDocument, Splitter.Next, oContext);
End;


Procedure TWPRTFReader.ReadColor(oRTF : TFslRTFExtractor);
Var
  sControl: String;
  aParts : TColourParts;

  Function ReadShade(Const sControl, sName : String) : Byte;
  Begin
    Result := StrToInt(Copy(sControl, Length(sName)+1, $FF));
  End;

Begin
  While (oRTF.PeekIsControl) Do
  Begin
    sControl := oRTF.ConsumeControl;
    If StringStartsWith(sControl, 'red') Then
        aParts.Red := ReadShade(sControl, 'red')
    Else If StringStartsWith(sControl, 'green') Then
        aParts.Green := ReadShade(sControl, 'green')
    Else If StringStartsWith(sControl, 'blue') Then
        aParts.Blue := ReadShade(sControl, 'blue')
    Else        // skip <themecolor>
        ;
  End;

  // red/green/blue is guarantee to exist even with theme color
  FColours.Add(TColour(aParts));

  oRTF.ConsumeBreak;
End;


Procedure TWPRTFReader.ReadColorTable(oRTF : TFslRTFExtractor);
Begin
  CheckRTF(Not FHasColours, 'Encountered Colour table at inappropriate time');
  FHasColours := True;

  FFirstColourDefault := oRTF.PeekIsBreak;
  If FFirstColourDefault Then
  Begin
    oRTF.ConsumeBreak;
    FColours.Add(0); // just a place holder to keep indexes correct
  End;
  While Not oRTF.PeekIsGroupClose Do
    ReadColor(oRTF);
End;


Procedure TWPRTFReader.ReadFont(oRTF : TFslRTFExtractor);
Var
  iIndex : Integer;
  sName : String;
  bGroup : boolean;
begin
// {\f0\froman\fcharset0\fprq2{\*\panose 02020603050405020304}Times New Roman;}
  iIndex := -1;
  bGroup := oRTF.PeekIsGroupStart;
  if bGroup then
    oRTF.ConsumeGroupStart;
  While Not oRTF.PeekIsGroupClose Do
  Begin
    If oRTF.PeekIsControl Then
    Begin
      If oRTF.PeekIsNumericControl('f') Then
        iIndex := oRTF.ConsumeNumericControl('f')
      Else
        oRTF.ConsumeControl; // ignore
    End
    Else If oRTF.PeekIsGroupStart Then
    Begin
      oRTF.ConsumeGroupStart;
      ByPass(oRTF, True, '{');
    End
    Else If oRTF.PeekIsBreak Then
    Begin
      oRTF.ConsumeBreak;
    End
    Else
      sName := StringReplace(oRTF.ConsumeText, ';', '');
  End;

  If iIndex = -1 Then
    RaiseError('ReadFont', 'Font Index not found for '+sName);
  If (sName = '') Then
    RaiseError('ReadFont', 'Font found without a name');
  FFonts.Add(sName, iIndex);
  if bGroup then
    oRTF.ConsumeGroupClose;
End;


Procedure TWPRTFReader.ReadFontTable(oRTF : TFslRTFExtractor);
Begin
  CheckRTF(Not FHasFonts, 'Encountered Font table at inappropriate time');
  FHasFonts := True;
  While Not oRTF.PeekIsGroupClose Do
    ReadFont(oRTF);
End;

Procedure TWPRTFReader.ReadListLevel(oList : TWPRTFListDefinition; oRTF : TFslRTFExtractor);
Var
  oLevel : TWPRTFListLevelDefinition;
Begin
  oRTF.ConsumeControl;
  oLevel := oList.Levels.New;
  Try
    While Not oRTF.PeekIsGroupClose Do
    Begin
      If (oRTF.PeekIsControl) Then
      Begin
        If Not ReadStyleControl(oRTF, oLevel) Then
        Begin
          If oRTF.PeekIsNumericControl('levelstartat') Then
            oLevel.Start := oRTF.ConsumeNumericControl('levelstartat')
          Else If oRTF.PeekIsNumericControl('levelnfc') And (oLevel.NumberFormat  = -1) Then
            oLevel.NumberFormat := oRTF.ConsumeNumericControl('levelnfc')
          Else If oRTF.PeekIsNumericControl('levelnfcn') Then
            oLevel.NumberFormat := oRTF.ConsumeNumericControl('levelnfcn')
          Else
            oRTF.ConsumeControl
        End
        // ReadStyleControl already consume the control if one exist
//        Else
//          oRTF.ConsumeControl;
      End
      Else If oRTF.PeekIsGroupStart Then
      Begin
        oRTF.ConsumeGroupStart;
        ByPass(oRTF, True, '->in list');
      End;
    End;
    oList.Levels.Add(oLevel.Link);
  Finally
    oLevel.Free;
  End;
  oRTF.ConsumeGroupClose;
End;

Procedure TWPRTFReader.ReadList(oRTF : TFslRTFExtractor);
Var
  oList : TWPRTFListDefinition;
Begin
  oRTF.ConsumeGroupStart;
  oList := FLists.New;
  Try
    While Not oRTF.PeekIsGroupClose Do
    Begin
      If (oRTF.PeekIsControl) Then
      Begin
        If oRTF.PeekIsNumericControl('listtemplateid') Then
          oList.TemplateId := oRTF.ConsumeNumericControl('listtemplateid')
        Else If oRTF.PeekIsNumericControl('listid') Then
          oList.Id := oRTF.ConsumeNumericControl('listid')
        Else
          oRTF.ConsumeControl;
      End
      Else If oRTF.PeekIsGroupStart Then
      Begin
        oRTF.ConsumeGroupStart;
        If oRTF.PeekControl = 'listlevel' Then
          ReadListLevel(oList, oRTF)
        Else If oRTF.PeekControl = 'listname' Then
        Begin
          oList.Name := StringStrip(oRTF.ConsumeText, ';');
          oRTF.ConsumeGroupClose;
        End
        Else
          ByPass(oRTF, True, '->in list');
      End;
    End;
    FLists.Add(oList.Link);
  Finally
    oList.Free;
  End;
  oRTF.ConsumeGroupClose;
End;


Procedure TWPRTFReader.ReadListTable(oRTF : TFslRTFExtractor);
Begin
  CheckRTF(Not HasLists, 'Encountered List table at inappropriate time');
  HasLists := True;
  While Not oRTF.PeekIsGroupClose Do
    ReadList(oRTF);
End;


Procedure TWPRTFReader.ReadListLevelOverride(oList : TWPRTFListOverrideDefinition; oRTF : TFslRTFExtractor);
Var
  oLevelOverride : TWPRTFListLevelOverrideDefinition;
Begin
  oLevelOverride := oList.Levels.New;
  Try
    While Not oRTF.PeekIsGroupClose Do
    Begin
      If (oRTF.PeekIsControl) Then
      Begin
        If Not ReadStyleControl(oRTF, oLevelOverride) Then
        Begin
          If oRTF.PeekIsNumericControl('leveloverridestartat') Then
            oLevelOverride.Start := oRTF.ConsumeNumericControl('levelstartat')
          Else
            oRTF.ConsumeControl;
        End
        Else
          oRTF.ConsumeControl;
      End
      Else If oRTF.PeekIsGroupStart Then
      Begin
        oRTF.ConsumeGroupStart;
        ByPass(oRTF, True, '-> in list override');
      End;
    End;
    oList.Levels.Add(oLevelOverride.Link);
  Finally
    oLevelOverride.Free;
  End;
  oRTF.ConsumeGroupClose;
End;

Procedure TWPRTFReader.ReadListOverride(oRTF : TFslRTFExtractor);
Var
  oListOverride : TWPRTFListOverrideDefinition;
Begin
  oRTF.ConsumeGroupStart;
  oListOverride := FListOverrides.New;
  Try
    While Not oRTF.PeekIsGroupClose Do
    Begin
      If (oRTF.PeekIsControl) Then
      Begin
        If oRTF.PeekIsNumericControl('ls') Then
          oListOverride.Id := oRTF.ConsumeNumericControl('ls')
        Else If oRTF.PeekIsNumericControl('listid') Then
          oListOverride.ListId := oRTF.ConsumeNumericControl('listid')
        Else
          oRTF.ConsumeControl;
      End
      Else If oRTF.PeekIsGroupStart Then
      Begin
        oRTF.ConsumeGroupStart;
        If oRTF.PeekControl = 'listoverridelevel' Then
          ReadListLevelOverride(oListOverride, oRTF)
        Else
          ByPass(oRTF, True, '-> in list override');
      End;
    End;
    FListOverrides.Add(oListOverride.Link);
  Finally
    oListOverride.Free;
  End;
  oRTF.ConsumeGroupClose;
End;


Procedure TWPRTFReader.ReadListOverrideTable(oRTF : TFslRTFExtractor);
Begin
  CheckRTF(Not HasListOverrides, 'Encountered List table at inappropriate time');
  HasListOverrides := True;
  While Not oRTF.PeekIsGroupClose Do
    ReadListOverride(oRTF);
End;


Procedure TWPRTFReader.ReadImageProperties(oImage : TWPWorkingDocumentImagePiece; oRTF : TFslRTFExtractor);
Begin
  oRTF.ConsumeControl;
  If oRTF.PeekControl <> 'picprop' Then
    Exit;
  oRTF.ConsumeControl;
  While (Not oRTF.PeekIsGroupClose) Do
  Begin
    If oRTF.PeekIsGroupStart Then
    Begin
      oRTF.ConsumeGroupStart;
      If oRTF.PeekIsControl And (oRTF.PeekControl = 'sp') Then
      Begin
        While (Not oRTF.PeekIsGroupClose) Do
        Begin
          If oRTF.PeekIsGroupStart Then
          Begin
            oRTF.ConsumeGroupStart;
            If oRTF.PeekIsControl And (oRTF.PeekControl = 'sv') Then
              oImage.Name := oRTF.ConsumeText;
            Bypass(oRTF, False, '...'); // consume any trailing props.
            oRTF.ConsumeGroupClose;
          End;
          Bypass(oRTF, False, '... Unknown properties');
        End;
      End;
      Bypass(oRTF, False, '...');       // consume any trailing props.
      oRTF.ConsumeGroupClose;
    End;
    Bypass(oRTF, False, '... Unknown properties');
  End;
End;


Procedure TWPRTFReader.LoadImage(oImage : TWPWorkingDocumentImagePiece; aFormat : TWPImageFormat; oRTF : TFslRTFExtractor);
Var
  oBuffer : TFslBuffer;
  sString : String;
Begin
  oBuffer := TFslBuffer.Create;
  Try
    // read the image as encoded hexadecimal string (may break into multiple lines)
    sString := '';
    While (Not oRTF.PeekIsGroupStart) And (Not oRTF.PeekIsGroupClose) And (Not oRTF.PeekIsControl) Do
    Begin
      If (oRTF.PeekIsBreak) Then
        oRTF.ConsumeBreak
      Else
        StringAppend(sString, oRTF.ConsumeText);
    End;

    oBuffer.AsBytes := DecodeHexadecimal(sString);
//    oBuffer.SaveToFileName('e:\Temp\image.png');
    LoadImage(oImage, oBuffer, aFormat, False);
  Finally
    oBuffer.Free;
  End;
End;


Procedure TWPRTFReader.ReadImage(oDocument : TWPWorkingDocument; oRTF : TFslRTFExtractor);
Var
  oImage : TWPWorkingDocumentImagePiece;
  sControl : String;
  aFormat : TWPImageFormat;
Begin
  oRTF.ConsumeGroupStart;
  sControl := oRTF.PeekControl;
  If sControl <> 'pict' Then
    Bypass(oRTF, False, '.. non-pict group')
  Else
  Begin
    oImage := TWPWorkingDocumentImagePiece.Create;
    aFormat := ifUnknown;
    Try
      While Not oRTF.PeekIsGroupClose Do
      Begin
        If oRTF.PeekIsControl Then
        Begin
          If oRTF.PeekIsNumericControl('picw') And (oImage.Width <= 0) Then
            oImage.Width := oRTF.ConsumeNumericControl('picw')
          Else If oRTF.PeekIsNumericControl('pich') And (oImage.Height <= 0) Then
            oImage.Height := oRTF.ConsumeNumericControl('pich')
          Else If oRTF.PeekIsNumericControl('picwgoal') Then
            oImage.Width := oRTF.ConsumeNumericControl('picwgoal') * DPIX Div 1440
          Else If oRTF.PeekIsNumericControl('pichgoal') Then
            oImage.Height := oRTF.ConsumeNumericControl('pichgoal') * DPIY Div 1440
          Else
          Begin
            sControl := oRTF.ConsumeControl;
            If sControl = 'jpegblip' Then
              aFormat := ifJPEG
            Else If sControl = 'pngblip' Then
              aFormat := ifPNG
            Else
              ;         // ignore unknown controls
          End;
        End
        Else If oRTF.PeekIsGroupStart Then
        Begin
          // Read image name from image properties
          oRTF.ConsumeGroupStart;
          If oRTF.PeekControl = '*' Then
            ReadImageProperties(oImage, oRTF);
          Bypass(oRTF, False, '.. unknow group');
          oRTF.ConsumeGroupClose;
        End
        Else
        Begin
          If aFormat <> ifUnknown Then
            LoadImage(oImage, aFormat, oRTF);
        End;
      End;

      If oImage.hasImage Then
        oDocument.Pieces.Add(oImage.Link);
    Finally
      oImage.Free;
    End;
  End;
  oRTF.ConsumeGroupClose;
End;


Procedure TWPRTFReader.Bypass(oRTF : TFslRTFExtractor; bConsumeClose : Boolean; Const sWhy : String);
Begin
  While Not oRTF.PeekIsGroupClose Do
  Begin
    If oRTF.PeekIsGroupStart Then
    Begin
      oRTF.ConsumeGroupStart;
      Bypass(oRTF, True, '{');
    End
    Else If oRTF.PeekIsControl Then // check for bin
      oRTF.ConsumeControl
    Else If oRTF.PeekIsBreak Then
      oRTF.ConsumeBreak
    Else
      oRTF.ConsumeText
  End;
  If bConsumeClose Then
    oRTF.ConsumeGroupClose;
End;


Procedure TWPRTFReader.ReadStyle(oRTF : TFslRTFExtractor; oContext : TWPRTFContext);
Var
  sName : String;
Begin
  oRTF.ConsumeGroupStart;
  While Not oRTF.PeekIsGroupClose Do
  Begin
    If oRTF.PeekIsControl Then
    Begin
      If Not ReadStyleControl(oRTF, oContext) Then
        oRTF.ConsumeControl; // ignore
    End
    Else If oRTF.PeekIsGroupStart Then
    Begin
      oRTF.ConsumeGroupStart;
      ByPass(oRTF, True, '-> style');
    End
    Else
      sName := StringReplace(oRTF.ConsumeText, ';', '');
  End;
  // update Style.... FFonts.Add(sName, iIndex);
  oRTF.ConsumeGroupClose;
End;

Procedure TWPRTFReader.ReadStylesheet(oRTF : TFslRTFExtractor; oContext : TWPRTFContext);
Begin
  CheckRTF(Not FHasStylesheet, 'Encountered Stylesheet at inappropriate time');
  FHasStylesheet := True;
  While Not oRTF.PeekIsGroupClose Do
    ReadStyle(oRTF, oContext);
End;


Function TWPRTFReader.ReadStyleControl(oRTF : TFslRTFExtractor; oContext : TWPRTFContext): Boolean;
Var
  i : Integer;
  s : String;
Begin
  Result := True;
  If oRTF.PeekIsBooleanControl('b') Then
    oContext.Bold := oRTF.ConsumeBooleanControl('b')
  Else If oRTF.PeekIsBooleanControl('i') Then
    oContext.Italic := oRTF.ConsumeBooleanControl('i')
  Else If oRTF.PeekIsBooleanControl('ul') Then
    oContext.Underline := oRTF.ConsumeBooleanControl('ul')
  Else If oRTF.PeekIsBooleanControl('caps') Then
    oContext.Capitalization := oRTF.ConsumeBooleanControl('caps')
  Else If oRTF.PeekIsBooleanControl('strike') Then
    oContext.Strikethrough := oRTF.ConsumeBooleanControl('strike')
  Else If oRTF.PeekIsNumericControl('fs') Then
    oContext.FontSize := oRTF.ConsumeNumericControl('fs') Div 2
  Else If oRTF.PeekIsNumericControl('ls') Then
    FListStyleId := oRTF.ConsumeNumericControl('ls')
  Else If oRTF.PeekIsNumericControl('li') Then
    oContext.LeftIndent := oRTF.ConsumeNumericControl('li') Div 100     // not twip, but 'hundredths of a character unit'
  Else If oRTF.PeekIsNumericControl('ri') Then
    oContext.RightIndent := oRTF.ConsumeNumericControl('ri') Div TWIP
  Else If oRTF.PeekIsNumericControl('cf') Then
    oContext.Foreground := GetColor(oRTF.ConsumeNumericControl('cf'))
  Else If oRTF.PeekIsNumericControl('trgaph') Then
    FTable.TablePadding := oRTF.ConsumeNumericControl('trgaph') Div TWIP
  Else If oRTF.PeekIsNumericControl('brdrw15') And (FTable.activeBorder <> -1) Then
    FTable.borders[FTable.activeBorder].Width := oRTF.ConsumeNumericControl('trgaph') Div TWIP
  Else If oRTF.PeekIsNumericControl('brdrcf') And (FTable.activeBorder <> -1) Then
    FTable.borders[FTable.activeBorder].Colour := GetColor(oRTF.ConsumeNumericControl('brdrcf'))
  Else If oRTF.PeekIsNumericControl('trcfpat') Then
    If (FTable.LastRowPiece <> Nil) Then
      FTable.LastRowPiece.Background := GetColor(oRTF.ConsumeNumericControl('trcfpat'))
    Else
      FTable.Background := GetColor(oRTF.ConsumeNumericControl('trcfpat'))
  Else If oRTF.PeekIsBooleanControl('sub') Then
    If oRTF.ConsumeBooleanControl('sub') Then
      oContext.State := fsSubscript
    Else
      oContext.State := fsNormal
  Else If oRTF.PeekIsBooleanControl('super') Then
    If oRTF.ConsumeBooleanControl('super') Then
      oContext.State := fsSuperscript
    Else
      oContext.State := fsNormal
  Else If Not IgnoreBackground And oRTF.PeekIsNumericControl('highlight') Then
    oContext.Background := GetColor(oRTF.ConsumeNumericControl('highlight'))
  Else If oRTF.PeekIsNumericControl('deff') Then
    FDefaultFont := oRTF.ConsumeNumericControl('deff')
  Else If oRTF.PeekIsNumericControl('f') Then
  Begin
    i := oRTF.ConsumeNumericControl('f');
    If FFonts.ExistsByValue(i) Then
      oContext.FontName := FFonts.GetKeyByValue(i)
    Else
      oContext.FontName := '';
  End
  Else
  Begin
    s := oRTF.PeekControl;
    If s = 'plain' Then
    Begin
      oContext.plain;
    End Else If (Length(s) = 2) And (s[1] = 'q') And CharInSet(s[2], ['l', 'r', 'c', 'j']) Then
    Begin
      Case s[2] Of
        'l' : oContext.Alignment := WordProcessorParagraphAlignmentLeft;
        'r' : oContext.Alignment := WordProcessorParagraphAlignmentRight;
        'c' : oContext.Alignment := WordProcessorParagraphAlignmentCentre;
        'j' : oContext.Alignment := WordProcessorParagraphAlignmentJustify;
      End;
    End
    Else If (FTable.activeBorder <> -1) And (StringStartsWith(s, 'brdr')) Then
    Begin
      If (s = 'brdrdash') Or (s = 'brdrdashsm') Then
        FTable.Borders[FTable.ActiveBorder].Pen := apsDash
      Else If (s = 'brdrdashd') Then
        FTable.Borders[FTable.ActiveBorder].Pen := apsDashDot
      Else If (s = 'brdrdashdd') Then
        FTable.Borders[FTable.ActiveBorder].Pen := apsDashDotDot
      Else If (s = 'brdrdot') Then
        FTable.Borders[FTable.ActiveBorder].Pen := apsDot
      Else If (s = 'brdrframe') Then
        FTable.Borders[FTable.ActiveBorder].Pen := apsInsideFrame
      Else If (s = 'brdrtbl') Or (s = 'brdrnil') Or (s = 'brdrnone') Then
        FTable.Borders[FTable.ActiveBorder].Pen := apsNone
      Else
        FTable.Borders[FTable.ActiveBorder].Pen := apsSolid;
    End
    Else If (s = 'trhdr') Then
    Begin
      If (FTable.LastRowPiece <> Nil) Then
        FTable.LastRowPiece.Header := True
      Else
        FTable.IsHeader := True;
    End
    Else If (s = 'trbrdrt') Then
      FTable.activeBorder := NDX_RowTop
    Else If (s = 'trbrdrl') Then
      FTable.activeBorder := NDX_RowLeft
    Else If (s = 'trbrdrr') Then
      FTable.activeBorder := NDX_RowRight
    Else If (s = 'trbrdrb') Then
      FTable.activeBorder := NDX_RowBottom
    Else If (s = 'clbrdrt') Then
      FTable.activeBorder := NDX_CellTop
    Else If (s = 'clbrdrl') Then
      FTable.activeBorder := NDX_CellLeft
    Else If (s = 'clbrdrr') Then
      FTable.activeBorder := NDX_CellRight
    Else If (s = 'clbrdrb') Then
      FTable.activeBorder := NDX_CellBottom
    Else
      Result := False;
    If Result Then
      oRTF.ConsumeControl;
  End;
End;


Procedure TWPRTFReader.ReadStyleContext(oPiece : TWPWorkingDocumentPiece; oContext : TWPRTFContext);
Begin
  If oContext.FontName <> '' Then
    oPiece.Font.Name := oContext.FontName
  Else If (FDefaultFont <> -1) And (FFonts.ExistsByValue(FDefaultFont)) Then
    oPiece.Font.Name := FFonts.GetKeyByValue(FDefaultFont);

  If oContext.FontSize <> 0 Then
    oPiece.Font.Size := oContext.FontSize;
  If oContext.Bold Then
    oPiece.Font.Bold := tsTrue
  Else
    oPiece.Font.Bold := tsFalse;
  If oContext.Underline Then
    oPiece.Font.Underline := tsTrue
  Else
    oPiece.Font.Underline := tsFalse;
  If oContext.Italic Then
    oPiece.Font.Italic := tsTrue
  Else
    oPiece.Font.Italic := tsFalse;
  If oContext.Strikethrough Then
    oPiece.Font.Strikethrough := tsTrue
  Else
    oPiece.Font.Strikethrough := tsFalse;
  If oContext.Capitalization Then
    oPiece.Font.Capitalization := fcsAllCaps
  Else
    oPiece.Font.Capitalization := fcsNormal;

  oPiece.Font.State := oContext.State;
  oPiece.Font.Foreground := oContext.Foreground;
  oPiece.Font.Background := oContext.Background;
End;

Function TWPRTFReader.GetColor(iIndex : Integer) : TColour;
Begin
  If (iIndex = 0) And FFirstColourDefault Then
    Result := DEF_COLOUR
  Else
    Result := FColours[iIndex];
End;

Function TWPRTFReader.GetHasLists: Boolean;
Begin
  Result := FLists <> Nil;
End;

Procedure TWPRTFReader.SetHasLists(Const Value: Boolean);
Begin
  If value Then
  Begin
    If FLists = Nil Then
      FLists := TWPRTFListDefinitions.Create
    Else
      FLists.Clear;
  End
  Else If (FLists <> Nil) Then
  Begin
    FLists.Free;
    FLists := Nil;
  End;
End;

Function TWPRTFReader.GetHasListOverrides: Boolean;
Begin
  Result := FListOverrides <> Nil;
End;

Procedure TWPRTFReader.SetHasListOverrides(Const Value: Boolean);
Begin
  If value Then
  Begin
    If FListOverrides = Nil Then
      FListOverrides := TWPRTFListOverrideDefinitions.Create
    Else
      FListOverrides.Clear;
  End
  Else If (FListOverrides <> Nil) Then
  Begin
    FListOverrides.Free;
    FListOverrides := Nil;
  End;
End;

Function NumberSplit(Const s : String; Out i : Integer; Out ch : Char) : Boolean;
Begin
  If (Length(s) > 1) And IsNumericString(s) Then
  Begin
    Result := True;
    i := StringToInteger32(s);
    ch := #0;
  End
  Else
  Begin
    Result := (Length(s) > 1) And (IsNumericString(Copy(s, 1, Length(s)-1)));
    If Result Then
    Begin
      i := StringToInteger32(Copy(s, 1, Length(s)-1));
      ch := s[Length(s)]
    End
    Else
      Result := False;
  End
End;

Procedure TWPRTFReader.ReadListStyle(oPara : TWPWorkingDocumentParaPiece);
Var
  oListOverride : TWPRTFListOverrideDefinition;
  oList : TWPRTFListDefinition;
  ch : Char;
  i : Integer;
Begin
  oListOverride := FListOverrides.GetById(FListStyleId);
  If Assigned(oListOverride) Then
  Begin
    oList := FLists.GetById(oListOverride.ListId);
    If Assigned(oList) And (oList.Levels.Count > 0) Then
    Begin
      oPara.Format.ListType := WPSParagraphListTypeNumbers;
      Case oList.Levels[0].NumberFormat Of
        NFC_Arabic,
        NFC_Cardinal_text_number,
        NFC_Ordinal_text_number,
        NFC_Ordinal_number:
           oPara.Format.NumberType := tnArabic;
        NFC_Uppercase_Roman_numeral:
          oPara.Format.NumberType := tnUpperRoman;
        NFC_Lowercase_Roman_numeral:
          oPara.Format.NumberType := tnLowerRoman;
        NFC_Uppercase_letter:
          oPara.Format.NumberType := tnUpperAlpha;
        NFC_Lowercase_letter:
          oPara.Format.NumberType := tnLowerAlpha;
        NFC_Bullet, NFC_No_number:
          // we have to inspect the list text to decide what to do
          ReadListText(oPara);
      Else
        // everything else
        oPara.Format.NumberType := tnArabic;
      End;
    End;
  End;
  If (oPara.Format.ListType = WPSParagraphListTypeNumbers) And NumberSplit(FListText, i, ch) Then
  Begin
     Case ch Of
       #0 : oPara.Format.NumberFormat := nwNone;
       '.' : oPara.Format.NumberFormat := nwDot;
       '/' : oPara.Format.NumberFormat := nwSlash;
       ')' : oPara.Format.NumberFormat := nwParenthesis;
       ':' : oPara.Format.NumberFormat := nwColon;
       ';' : oPara.Format.NumberFormat := nwSemiColon;
     Else
       oPara.Format.NumberFormat := nwUnknown;
     End;
     If i <> FLastListNumber Then
       oPara.Format.FixedNumber := i;
     FLastListNumber := i;
  End;
  FListTextUsed := True;
End;

Procedure TWPRTFReader.ReadListText(oPara : TWPWorkingDocumentParaPiece);
Var
  ch : Char;
  i : Integer;
Begin
  If FListText = '' Then
    oPara.Format.ListType := WPSParagraphListTypeBullets
  Else If NumberSplit(FListText, i, ch) Then
     oPara.Format.ListType := WPSParagraphListTypeNumbers
  Else If (FListTextFontName = 'Symbol') And (FListText = #183) Then
  Begin
    oPara.Format.BulletTypeDisc;
    oPara.Format.ListType := WPSParagraphListTypeBullets;
  End
  Else If (FListTextFontName = 'Wingding') And (FListText = #167) Then
  Begin
    oPara.Format.BulletTypeSquare;
    oPara.Format.ListType := WPSParagraphListTypeBullets;
  End
  Else If (FListTextFontName = 'Courier New') And (FListText = 'o') Then
  Begin
    oPara.Format.BulletTypeCircle;
    oPara.Format.ListType := WPSParagraphListTypeBullets;
  End
  Else
    oPara.Format.ListType := WPSParagraphListTypeBullets
End;

Procedure TWPRTFReader.ProcessListText(oRTF: TFslRTFExtractor; oContext: TWPRTFContext);
Var
  i : Integer;
Begin
  While Not oRTF.PeekIsGroupClose Do
  Begin
    If (oRTF.PeekIsControl) Then
    Begin
      If oRTF.PeekIsNumericControl('f') Then
      Begin
        i := oRTF.ConsumeNumericControl('f');
        If FFonts.ExistsByValue(i) Then
          FListTextFontName := FFonts.GetKeyByValue(i)
        Else
          FListTextFontName := '';
      End Else If (oRTF.PeekIsControl) And (oRTF.PeekControl[1] = '''') Then
        FListText := readHexChar(Copy(oRTF.ConsumeControl, 2, MAXINT))
      Else
        oRTF.ConsumeControl;
    End
    Else If oRTF.PeekIsGroupStart Then
    Begin
      oRTF.ConsumeGroupStart;
      ByPass(oRTF, True, '-> list text');
    End
    Else
      FListText := oRTF.ConsumeText;
  End;
  FListTextUsed := False;
End;


Procedure TWPRTFReader.AddPageBreak(oDocument: TWPWorkingDocument; oContext : TWPRTFContext);
Var
  oBreak : TWPWorkingDocumentBreakPiece;
Begin
  // page break should be in its own paragraph
  AddPara(oDocument, oContext);
  
  oBreak := TWPWorkingDocumentBreakPiece.Create;
  Try
    oBreak.BreakType := btPageBreak;
    oBreak.Alignment := WordProcessorAlignmentUnknown;
    oBreak.Width := 1;
    oBreak.PenColour := clBlack;
    oBreak.PenWidth := 1;
    oBreak.PenStyle := apsSolid;
    oBreak.EndStyle := apesRound;
    oDocument.Pieces.Add(oBreak.Link);
  Finally
    oBreak.Free;
  End;
End;

Function TWPRTFReader.DPIX: Integer;
Var
  HDC : THandle;
Begin
  HDC := GetDC(0);
  Result := GetDeviceCaps(HDC, LOGPIXELSX);
  ReleaseDC(0, HDC);
End;

Function TWPRTFReader.DPIY: Integer;
Var
  HDC : THandle;
Begin
  HDC := GetDC(0);
  Result := GetDeviceCaps(HDC, LOGPIXELSY);
  ReleaseDC(0, HDC);
End;


function TWPRTFReader.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FColours.sizeInBytes);
  inc(result, FFonts.sizeInBytes);
  inc(result, FLists.sizeInBytes);
  inc(result, FListOverrides.sizeInBytes);
  inc(result, (FListTextFontName.length * sizeof(char)) + 12);
  inc(result, (FListText.length * sizeof(char)) + 12);
  inc(result, FLastPara.sizeInBytes);
end;

Destructor TWPRTFWriter.Destroy;
Begin
  FFonts.Free;
  FContexts.Free;
  FRTF.Free;
  FColours.Free;
  Inherited;
End;


Function TWPRTFWriter.GetFonts : TFslStringList;
Begin
  Assert(Invariants('GetFonts', FFonts, TFslStringList, 'FFonts'));
  Result := FFonts;
End;


Function TWPRTFWriter.GetColours : TFslIntegerList;
Begin
  Assert(Invariants('GetColours', FColours, TFslIntegerList, 'FColours'));
  Result := FColours;
End;


Function TWPRTFWriter.GetContexts : TWPRTFContexts;
Begin
  Assert(Invariants('GetContexts', FContexts, TWPRTFContexts, 'FContexts'));
  Result := FContexts;
End;


Function TWPRTFWriter.GetRTF : TFslRTFFormatter;
Begin
  Assert(Invariants('GetRTF', FRTF, TFslRTFFormatter, 'FRTF'));
  Result := FRTF;
End;


Function TWPRTFWriter.GetContext : TWPRTFContext;
Begin
  Assert(Invariants('GetContext', FContext, TWPRTFContext, 'FContext'));
  Result := FContext;
End;


Procedure TWPRTFWriter.Initialise;
Begin
  FFonts := TFslStringList.Create;
  FRTF := TFslRTFFormatter.Create;
  FRTF.LineLimit := LineLimit; 
  FRTF.Stream := TFslStringStream.Create;
  FContexts := TWPRTFContexts.Create;
  FColours := TFslIntegerList.Create;

  FLists := TWPWorkingDocumentPieces.Create(False);
  FListsIndexer := TFslStringList.Create;

  EnumerateStyles;
End;


Procedure TWPRTFWriter.Finalise;
Begin
  FContexts.Free;
  FContexts := Nil;

  SaveRTF;

  FColours.Free;
  FColours := Nil;
  FRTF.Free;
  FRTF := Nil;
  FFonts.Free;
  FFonts := Nil;

  FListsIndexer.Free;
  FListsIndexer := Nil;
  FLists.Free;
  FLists := Nil;
End;


Procedure TWPRTFWriter.SaveRTF;
Var
  oRTF: TFslRTFFormatter;
Begin
  oRTF := TFslRTFFormatter.Create;
  Try
    oRTF.Stream := Stream.Link;
    oRTF.LineLimit := LineLimit;
    oRTF.StartGroup;
    WriteHeader(oRTF);
    oRTF.ProduceInline(String(TFslStringStream(RTF.Stream).Data));
    oRTF.CloseGroup;
  Finally
    oRTF.Free;
  End;
End;


Procedure TWPRTFWriter.WriteHeader(oRTF : TFslRTFFormatter);
Begin
  oRTF.Control('rtf1');
  oRTF.Control('ansi');
  oRTF.Control('deff0');
  WriteFontTable(oRTF);
  WriteStyleSheet(oRTF);
  WriteColourTable(oRTF);

  // <filetbl>?
  // <colortbl>?
  // <stylesheet>?
  // <listtables>?
  WriteListTable(oRTF);
  // <revtbl>?
  // <rsidtable>?
  // <generator>?
End;


Procedure TWPRTFWriter.WriteStyle(oRTF : TFslRTFFormatter; iIndex : Integer; oStyle : TWPStyle);
Begin
  oRTF.StartGroup;
  If oStyle <> Styles.DefaultStyle Then
    oRTF.Control('s'+IntegerToString(iIndex));
  oRTF.Control('f'+FontIndex(oStyle.Font.Name));
  oRTF.Control('fs'+IntegerToString(oStyle.Font.Size * 2));
  oRTF.Text(oStyle.Name);
  oRTF.CloseGroup;
End;


Procedure TWPRTFWriter.WriteStyleSheet(oRTF : TFslRTFFormatter);
Var
  iLoop : Integer;
Begin
  oRTF.StartGroup;
  oRTF.Control('stylesheet');
  For iLoop := 0 To Styles.Count - 1 Do
    WriteStyle(oRTF, iLoop, Styles[iLoop]);
  oRTF.CloseGroup;
End;


Procedure TWPRTFWriter.WriteColour(oRTF : TFslRTFFormatter; aColour : TColour);
Begin
  oRTF.Control('red'+IntegerToString(TColourParts(aColour).Red));
  oRTF.Control('green'+IntegerToString(TColourParts(aColour).Green));
  oRTF.Control('blue'+IntegerToString(TColourParts(aColour).Blue));
  oRTF.Break(';');
End;


Procedure TWPRTFWriter.WriteColourTable(oRTF : TFslRTFFormatter);
Var
  iLoop : Integer;
Begin
  oRTF.StartGroup;
  oRTF.Control('colortbl');
  oRTF.Break(';');
  For iLoop := 0 To Colours.Count - 1 Do
    WriteColour(oRTF, Colours[iLoop]);
  oRTF.CloseGroup;
End;

Procedure TWPRTFWriter.WriteFontTable(oRTF : TFslRTFFormatter);
Var
  iLoop : Integer;
Begin
  oRTF.StartGroup;
  oRTF.Control('fonttbl');
  For iLoop := 0 To FFonts.Count - 1 Do
    WriteFont(oRTF, iLoop, FFonts[iLoop]);
  oRTF.CloseGroup;
End;


Procedure TWPRTFWriter.WriteFont(oRTF : TFslRTFFormatter; iIndex : Integer; Const sName : String);
Begin
  oRTF.StartGroup;
  oRTF.Control('f'+IntegerToString(iIndex));
  oRTF.Text(sName);
  oRTF.CloseGroup;
End;


Function TWPRTFWriter.ColorIndex(Const aColour : TColour) : String;
Begin
  If Colours.ExistsByValue(aColour) Then
    Result := IntegerToString(Colours.IndexByValue(aColour)+1)
  Else
    Result := IntegerToString(Colours.Add(aColour)+1);
End;


Function TWPRTFWriter.FontIndex(Const sFont : String) : String;
Begin
  If Fonts.ExistsByValue(sFont) Then
    Result := IntegerToString(Fonts.IndexByValue(sFont))
  Else
    Result := IntegerToString(Fonts.Add(sFont));
End;


Procedure TWPRTFWriter.WriteListTable(oRTF: TFslRTFFormatter);
Var
  iLoop: Integer;
  oPara: TWPWorkingDocumentParaPiece;
Begin
  oRTF.StartGroup;
  oRTF.Control('*');
  oRTF.Control('listtable');
  For iLoop := 0 To FLists.Count - 1 Do
  Begin
    // {\list \listsimple1 {\listlevel {\leveltext\'01\u-3929 ?;}\f2} \listid1}
    oPara := TWPWOrkingDocumentParaPiece(FLists[iLoop]);
    oRTF.StartGroup;
    oRTF.Control('list');
    oRTF.Control('listsimple1');
    oRTF.StartGroup;
    oRTF.Control('listlevel');
    // bullet type or number
    If (oPara.Format.ListType = WPSParagraphListTypeBullets) Then
      WriteBulletType(oRTF, oPara.Format.BulletType)
    Else
      WriteNumberedType(oRTF, oPara.Format);
    oRTF.Closegroup;
    oRTF.Control('listid' + IntegerToString(Integer(oPara)));
    oRTF.CloseGroup;
  End;
  oRTF.CloseGroup;

  oRTF.StartGroup;
  oRTF.Control('*');
  oRTF.Control('listoverridetable');
  For iLoop := 0 To FLists.Count - 1 Do
  Begin
    oPara := TWPWOrkingDocumentParaPiece(FLists[iLoop]);
    // {\listoverride\listid1\listoverridecount0\ls1}
    oRTF.StartGroup;
    oRTF.Control('listoverride');
    oRTF.Control('listid' + IntegerToString(Integer(oPara)));
    oRTF.Control('listoverridecount0');
    oRTF.Control('ls' + IntegerToString(Integer(oPara)));
    oRTF.CloseGroup;
  End;
  oRTF.CloseGroup;
End;


Procedure TWPRTFWriter.WriteNumberedType(oRTF: TFslRTFFormatter; Const oFormat: TWPSParagraphDetails);
Begin
  Case oFormat.NumberType Of
    tnUpperRoman:       oRTF.Control('levelnfcn1');
    tnLowerRoman:       oRTF.Control('levelnfcn2');
    tnUpperAlpha:       oRTF.Control('levelnfcn3');
    tnLowerAlpha:       oRTF.Control('levelnfcn4')
    Else                oRTF.Control('levelnfcn0');      // Assume arabic
  End;

  If (oFormat.FixedNumber <> DEF_WORD) Then
    oRTF.Control('levelstartat' + IntegerToString(oFormat.FixedNumber))
  Else
    oRTF.Control('levelstartat1');

  oRTF.StartGroup;
  Case oFormat.NumberFormat Of
    nwNone:             oRTF.Control('leveltext\''02\''00;');
    nwSlash:            oRTF.Control('leveltext\''02\''00/;');
    nwParenthesis:      oRTF.Control('leveltext\''03(\''00);');
    nwColon:            oRTF.Control('leveltext\''02\''00:;');
    nwSemiColon:        oRTF.Control('leveltext\''02\''00\''3b;');
    Else                oRTF.Control('leveltext\''02\''00.;');
  End;
  oRTF.CloseGroup;

  oRTF.StartGroup;
  If (oFormat.NumberFormat = nwParenthesis) Then
    oRTF.Control('levelnumbers\''02;')
  Else
    oRTF.Control('levelnumbers\''01;');
  oRTF.CloseGroup;
End;


Procedure TWPRTFWriter.WriteBulletType(oRTF: TFslRTFFormatter; Const oType: TWPSParagraphBulletType);
Begin
  // Square: {\leveltext\'01\u-3929 ?;}; Font: {\f2\fnil Wingdings;};
  // Disc: {\leveltext\'01\u-3913 ?;} + FONT: {\f3\froman Symbol;}
  // Circle: {\leveltext\'01o;} + FONT: {\f4 Courier New;}
  oRTF.Control('levelnfcn23');
  oRTF.StartGroup;
  oRTF.Control('leveltext');

  If oType = tbCircle Then
  Begin
    oRTF.Control('''01o;');
    oRTF.CloseGroup;
    oRTF.Control('f' + FontIndex('Courier New'));
  End
  Else If oType = tbSquare Then
  Begin
    oRTF.Control('''01\u-3929 ?;');
    oRTF.CloseGroup;
    oRTF.Control('f' + FontIndex('Wingdings'));
  End
  Else // Disc or Unknown
  Begin
    oRTF.Control('''01\u-3913 ?;');
    oRTF.CloseGroup;
    oRTF.Control('f' + FontIndex('Symbol'));
  End;
End;

Function TWPRTFWriter.ListIndex(Const oPara: TWPWorkingDocumentParaPiece): String;
Var
  sListAsString: String;
  iIndex : Integer;
Begin
  If (oPara <> Nil)
    And ((oPara.Format.ListType = WPSParagraphListTypeBullets) Or (oPara.Format.ListType = WPSParagraphListTypeNumbers)) Then
  Begin
    sListAsString := EncodeListFormat(oPara.Format);
    // new list or reset number list
    If (Not FListsIndexer.ExistsByValue(sListAsString))
      Or ((oPara.Format.ListType = WPSParagraphListTypeNumbers) And (oPara.Format.FixedNumber <> DEF_WORD)) Then
    Begin
      Result := IntegerToString(Integer(oPara));
      FLists.Add(oPara.Link);
      FListsIndexer.Add(sListAsString);

      // Add font to show the bullet
      If (oPara.Format.ListType = WPSParagraphListTypeBullets) Then
      Begin
        If (oPara.Format.BulletType = tbCircle) Then
          FontIndex('Courier New')
        Else If (oPara.Format.BulletType = tbSquare) Then
          FontIndex('Wingdings')
        Else  // default to disc
          FontIndex('Symbol')
      End;
    End
    Else
    Begin
      iIndex := FListsIndexer.IndexByValue(sListAsString);
      Result := IntegerToString(Integer(FLists[iIndex]));
    End;
  End
  Else
    Result := '';
End;


Function TWPRTFWriter.EncodeListFormat(Const oFormat: TWPSParagraphDetails): String;
Begin
  Result := ATTR_NAME_LISTTYPE + '=' + NAMES_WPSPARAGRAPHLISTTYPE[oFormat.ListType] + ';';
  If (oFormat.ListType = WPSParagraphListTypeBullets) Then
  Begin
    Result := Result + ATTR_NAME_BULLETTYPE + '=' + NAMES_WPSPARAGRAPHBULLETTYPE[oFormat.BulletType] + ';';
  End
  Else If (oFormat.ListType = WPSParagraphListTypeNumbers) Then
  Begin
    Result := Result + ATTR_NAME_NUMBERTYPE + '=' + NAMES_WPSPARAGRAPHNUMBERTYPE[oFormat.NumberType] + ';';
    Result := Result + ATTR_NAME_NUMBERFORMAT + '=' + NAMES_WPSPARAGRAPHNUMBERFORMAT[oFormat.NumberFormat] + ';';
  End
  Else
    RaiseError('EncodeListFormat', 'Paragraph is not a list item.');
End;


Procedure TWPRTFWriter.EnumerateStyles;
Var
  iLoop : Integer;
  oStyle : TWPStyle;
Begin
  For iLoop := 0 To Styles.Count - 1 Do
  Begin
    oStyle := Styles[iLoop];
    If (oStyle.Font.Name <> '') And Not FFonts.ExistsByValue(oStyle.Font.Name) Then
      FFonts.Add(oStyle.Font.Name);
    If (oStyle.Font.Foreground <> DEF_COLOUR) And Not FColours.ExistsByValue(oStyle.Font.Foreground) Then
      FColours.Add(oStyle.Font.Foreground);
    If (oStyle.Font.Background <> DEF_COLOUR) And Not FColours.ExistsByValue(oStyle.Font.Background) Then
      FColours.Add(oStyle.Font.Background);
  End;
End;


Function TWPRTFWriter.GetPieceStyle(oPiece : TWPWorkingDocumentPiece) : TWPStyle;
Begin
  Result := Nil;
  If oPiece.Style <> '' Then
    Result := Styles.GetByName(oPiece.Style);
  If Result = Nil Then
    Result := Styles.DefaultStyle;
End;

Procedure TWPRTFWriter.WriteText(oText : TWPWorkingDocumentTextPiece);
Begin
  WriteTextContent(oText, oText.Content);
End;


Procedure TWPRTFWriter.WriteTextContent(oPiece : TWPWorkingDocumentPiece; Const sText : String);
Var
  oStyle : TWPStyle;

  sValue : String;
  iValue : Integer;
  bValue : Boolean;
  aColour : TColour;
Begin
  If Styled Then
  Begin
    oStyle := GetPieceStyle(oPiece);

    sValue := oStyle.Name;
    If Context.Style <> sValue Then
    Begin
      Context.Style := sValue;
      If Context.Style = DEFAULT_STYLE_NAME Then
        RTF.Control('s0')
      Else
        RTF.Control('s'+IntegerToString(Styles.IndexByName(sValue)));
    End;

    sValue := oStyle.WorkingFontName(oPiece.Font);
    If Context.FontName <> sValue Then
    Begin
      Context.FontName := sValue;
      RTF.Control('f'+FontIndex(sValue));
    End;

    iValue := oStyle.WorkingFontSize(oPiece.Font);
    If Context.FontSize <> iValue Then
    Begin
      Context.FontSize := iValue;
      RTF.Control('fs'+IntegerToString(iValue * 2));
    End;

    bValue := oStyle.WorkingFontCapitalization(oPiece.Font) In [fcsAllCaps, fcsSmallCaps];
    If Context.Capitalization <> bValue Then
    Begin
      Context.Capitalization := bValue;
      RTF.ControlBoolean('caps', bValue);
    End;

    bValue := oStyle.WorkingFontBold(oPiece.Font) = tsTrue;
    If Context.Bold <> bValue Then
    Begin
      Context.Bold := bValue;
      RTF.ControlBoolean('b', bValue);
    End;

    bValue := oStyle.WorkingFontItalic(oPiece.Font) = tsTrue;
    If Context.Italic <> bValue Then
    Begin
      Context.Italic := bValue;
      RTF.ControlBoolean('i', bValue);
    End;

    bValue := oStyle.WorkingFontUnderline(oPiece.Font) = tsTrue;
    If Context.Underline <> bValue Then
    Begin
      Context.Underline := bValue;
      RTF.ControlBoolean('ul', bValue);
    End;

    bValue := oStyle.WorkingFontStrikethrough(oPiece.Font) = tsTrue;
    If Context.Strikethrough <> bValue Then
    Begin
      Context.Strikethrough := bValue;
      RTF.ControlBoolean('\strike', bValue);
    End;

    aColour := oStyle.WorkingFontForeground(oPiece.Font);
    If Context.Foreground <> aColour Then
    Begin
      Context.Foreground := aColour;
      If aColour = DEF_COLOUR Then
        RTF.Control('cf0')
      Else
        RTF.Control('cf'+ColorIndex(aColour));
    End;

    aColour := oStyle.WorkingFontBackground(oPiece.Font, DEF_COLOUR);
    If Context.Background <> aColour Then
    Begin
      Context.Background := aColour;
      If aColour = DEF_COLOUR Then
        RTF.Control('highlight0')
      Else
        RTF.Control('highlight'+ColorIndex(aColour));
    End;
  End
  Else
    RTF.Control('f0');

  RTF.Text(sText);
End;


Procedure TWPRTFWriter.IterateParagraph(oIterator : TWPPieceIterator; oSection : TWPWorkingDocumentSectionStartPiece);
Var
  oPara : TWPWorkingDocumentParaPiece;
  sListId: String;
Begin
  oPara := oIterator.CurrentParagraph(True);

  // start a group of list items
  sListId := ListIndex(oPara);
  If (Not bInList) And(sListId <> '') Then
  Begin
    RTF.StartGroup;
    RTF.Control('ls' + sListId);
    bInList := True;
  End;

  WriteParagraphStart(oPara);
  IterateParagraphContents(oIterator);
  WriteParagraphStop(oPara, (oIterator.Peek <> Nil) And (oIterator.Peek.PieceType = ptSectionStop), oSection);

  oIterator.Next;

  // if this is the last paragraph, or paragraph does not match current list, then we close group
  If bInList And
    ((Not oIterator.More) Or (oIterator.CurrentParagraph(True) = Nil)
     Or (sListId <> ListIndex(oIterator.CurrentParagraph(True)))) Then
  Begin
    RTF.CloseGroup;
    bInList := False;
  End;
End;


Procedure TWPRTFWriter.WriteParagraphStart(oPara : TWPWorkingDocumentParaPiece);
Begin
  StartPara;
End;


Procedure TWPRTFWriter.StartPara;
Begin
  FContext := Contexts.NewContext(DEFAULT_STYLE_NAME);
  RTF.StartGroup;
End;


Procedure TWPRTFWriter.WriteParagraphStop(oPara : TWPWorkingDocumentParaPiece; bNextIsSection : Boolean; oSection : TWPWorkingDocumentSectionStartPiece);
Begin
  StopPara;
End;


Procedure TWPRTFWriter.StopPara;
Begin
  RTF.Control('par');
  RTF.CloseGroup;
  FContext := Contexts.CloseContext;
End;


Procedure TWPRTFWriter.WriteLineBreak(oBreak : TWPWorkingDocumentLineBreakPiece);
Begin
  RTF.Control('line');
End;


Procedure TWPRTFWriter.WriteImageName(Const sName : String);
Begin
  RTF.StartGroup;
  RTF.Control('*');
  RTF.Control('picprop');

  RTF.StartGroup;
  RTF.Control('sp');
  RTF.StartGroup;
  RTF.Control('sn');
  RTF.Text('pibname');
  RTF.CloseGroup;
  RTF.StartGroup;
  RTF.Control('sv');
  RTF.Text(sName);
  RTF.CloseGroup;
  RTF.CloseGroup;

  RTF.CloseGroup;
End;


Const WRAP_WIDTH = 128;

Function WrapLines(Const sBase64 : String; iChars : Integer = WRAP_WIDTH) : String;
Var
  iCursor : Integer;
  iPos : Integer;
Begin
  SetLength(Result, Trunc(Length(sBase64) + ((Length(sBase64) Div iChars)*2)));
  iCursor := 1;
  iPos := 1;
  While iCursor < Length(sBase64) Do
  Begin
    Move(sBase64[iCursor], Result[iPos], IntegerMin(iChars, (Length(sBase64) - iCursor) + 1));
    Inc(iPos, IntegerMin(iChars, (Length(sBase64) - iCursor) + 1));
    Inc(iCursor, iChars);
    If iCursor < Length(sBase64) Then
      Move(#13#10, Result[iPos], 2);
    Inc(iPos, 2);
  End;
End;


Procedure TWPRTFWriter.SaveJPEG(oImage: TFslVCLGraphic);
Var
  oStream : TFslStringStream;
Begin
  RTF.Control('jpegblip');

  oStream := TFslStringStream.Create;
  Try
    oImage.SaveToStream(oStream);
  {$IFDEF VER130}
    RTF.Text(WrapLines(EncodeHexadecimal(oStream.Data)));
  {$ELSE}
    RTF.Text(WrapLines(String(EncodeHexadecimal(oStream.Bytes))));
  {$ENDIF}
  Finally
    oStream.Free;
  End;
End;


Procedure TWPRTFWriter.SavePNG(oImage: TFslVCLGraphic);
Var
  oStream : TFslStringStream;
Begin
  RTF.Control('pngblip');
  oStream := TFslStringStream.Create;
  Try
    TFslPortableNetworkGraphic.SavePNGToStream(oImage, oStream);
  {$IFDEF VER130}
    RTF.Text(WrapLines(EncodeHexadecimal(oStream.Data)));
  {$ELSE}
    RTF.Text(WrapLines(String(EncodeHexadecimal(oStream.Bytes))));
  {$ENDIF}
  Finally
    oStream.Free;
  End;
End;


Procedure TWPRTFWriter.WriteImage(oImage : TWPWorkingDocumentImagePiece);
var
  oImg : TFslGraphic;
  oTemp : TFslVCLGraphic;
Begin
  RTF.StartGroup;
  RTF.Control('*');
  RTF.Control('shppict');
  RTF.StartGroup;
  RTF.Control('pict');
  RTF.Control('picw'+IntegerToString(oImage.Width));
  RTF.Control('pich'+IntegerToString(oImage.Height));
  RTF.Control('picscalex58');
  RTF.Control('picscaley58');

  If oImage.Name <> '' Then
    WriteImageName(oImage.Name);

  oImg := oImage.GetWorkingImage(0, 0, False, false);
  if not (oImg is TFslVCLGraphic) then
  begin
    oTemp := oImg.AsBitmap;
    try
      SavePNG(oTemp);
    finally
      otemp.Free;
    end;
  end
  else If TFslVCLGraphic(oImg).Handle Is TJPEGImage Then
    SaveJPEG(TFslVCLGraphic(oImg))
  Else
    SavePNG(TFslVCLGraphic(oImg));

  RTF.CloseGroup;
  RTF.CloseGroup;
End;


Procedure TWPRTFWriter.WriteTableRowFormat(oTableRow : TWPWorkingDocumentTableRowStartPiece);
Var
  iLoop: Integer;
  oCell: TWPWorkingDocumentTableCellStartPiece;
Begin
  RTF.Control('trowd');
  RTF.Control('trftsWidth1');
  RTF.Control('trautofit1');

  For iLoop := 0 To oTableRow.Cells.Count - 1 Do
  Begin
    oCell := TWPWorkingDocumentTableCellStartPiece(oTableRow.Cells[iLoop]);
    WriteCellBorders(oCell.WorkingTopBorder, oCell.WorkingBottomBorder, oCell.WorkingLeftBorder, oCell.WorkingBottomBorder);
    RTF.Control('clvertalc');
    RTF.Control('cltxlrtb');
    RTF.Control('clftsWidth1');
    RTF.Control('cellx' + IntegerToString(iLoop + 1));
  End;
End;


Procedure TWPRTFWriter.WriteTableRowStart(oTableRow: TWPWorkingDocumentTableRowStartPiece);
//Var
//  iLoop: Integer;
//  oCell: TWPWorkingDocumentTableCellStartPiece;
Begin
  // {\trowd \pard\intbl {
  RTF.StartGroup;
  WriteTableRowFormat(oTableRow);

  RTF.Control('pard');
  RTF.Control('intbl');

  RTF.StartGroup;
End;


Procedure TWPRTFWriter.WriteTableRowStop(oTableRow: TWPWorkingDocumentTableRowStartPiece;oStop: TWPWorkingDocumentStopPiece; bIsLast : Boolean);
//Var
//  iLoop: Integer;
//  oCell: TWPWorkingDocumentTableCellStartPiece;
Begin
  // }
  // \trftsWidth1\trautofit1
  // \clvertalt \cltxlrtb\clftsWidth1 \cellx1
  // ...
  // \clvertalt \cltxlrtb\clftsWidth1 \cellx3
  // \par\row}

  RTF.CloseGroup;

  RTF.StartGroup;
  // repeat row format data
  WriteTableRowFormat(oTableRow);

  RTF.Control('par');
  RTF.Control('row');
  RTF.CloseGroup;

  RTF.CloseGroup;
End;

Procedure TWPRTFWriter.WriteTableCellStop(oTableCell: TWPWorkingDocumentTableCellStartPiece; oStop: TWPWorkingDocumentStopPiece);
Begin
  RTF.Control('cell');
End;


Procedure TWPRTFWriter.WriteCellBorders(Const oTop, oBottom, oLeft, oRight: TWPBorder);
Begin
  RTF.Control('clbrdrt');
  WriteBorder(oTop);
  RTF.Control('clbrdrb');
  WriteBorder(oBottom);
  RTF.Control('clbrdrl');
  WriteBorder(oLeft);
  RTF.Control('clbrdrr');
  WriteBorder(oRight);
End;


Procedure TWPRTFWriter.WriteBorder(Const oBorder: TWPBorder);
Begin
  If (oBorder.Defined) Then
  Begin
    Case oBorder.Style Of
      apsDash           : RTF.Control('brdrdash');
      apsDashDot        : RTF.Control('brdrdashd');
      apsDashDotDot     : RTF.Control('brdrdashdd');
      apsDot            : RTF.Control('brdrdot')
      // ignore the rest
    End;

    If (oBorder.Width < 8) Then
    Begin
      RTF.Control('brdrs');
      RTF.Control('brdrw' + IntegerToString(oBorder.Width * 10));
    End
    Else
    Begin
      RTF.Control('brdrth');
      RTF.Control('brdrw' + IntegerToString(oBorder.Width * 5))
    End;

    If oBorder.Colour <> DEF_COLOUR Then
      RTF.Control('brdrcf' + ColorIndex(oBorder.Colour));
  End
  Else
    RTF.Control('brdrnil');
End;


Procedure TWPRTFWriter.WriteFieldStart(oField: TWPWorkingDocumentFieldStartPiece);
Begin
  RTF.StartGroup;
  RTF.Control('field');
  RTF.Control('fldlock');
  RTF.StartGroup;
  RTF.Control('*');
  RTF.Control('fldinst');
  RTF.StartGroup;
  If Assigned(oField.Hotspot) Then
    RTF.Text('HYPERLINK "' + EncodeURL(oField.Hotspot.URL) + '" \o "' + oField.NamePair + '"')
  Else
    RTF.Text('HYPERLINK "http://" \o "' + oField.NamePair + '"');
  RTF.CloseGroup;
  RTF.CloseGroup;

  // start Field content
  RTF.StartGroup;
  RTF.Control('fldrslt');
End;


Procedure TWPRTFWriter.WriteFieldStop(oField: TWPWorkingDocumentFieldStartPiece; oStop: TWPWorkingDocumentFieldStopPiece);
Begin
  RTF.CloseGroup;
  RTF.CloseGroup;
End;


Procedure TWPRTFWriter.MakeNewContext();
Begin
  FContext := Contexts.NewContext(DEFAULT_STYLE_NAME);
End;

Procedure TWPRTFWriter.CloseExistingContext();
Begin
  FContext := Contexts.CloseContext;
End;

Procedure TWPRTFWriter.WriteSectionStart(oSection: TWPWorkingDocumentSectionStartPiece);
Begin
  MakeNewContext;
  RTF.StartGroup;
End;


Procedure TWPRTFWriter.WriteSectionStop(oSection: TWPWorkingDocumentSectionStartPiece; oStop: TWPWorkingDocumentStopPiece);
Begin
  RTF.Control('par');
  RTF.CloseGroup;
  CloseExistingContext;
End;


Function TWPRTFWriter.EncodeURL(Const sURL: String): String;
Begin
  If (StringUpper(StringCopy(sURL, 1, 7)) <> 'HTTP://') Then
    Result := 'http://' + sURL
  Else
    Result := sURL;
End;

Procedure TWPRTFWriter.WriteBreak(oBreak: TWPWorkingDocumentBreakPiece);
Begin
  If oBreak.BreakType = btPageBreak Then
    RTF.Control('page')
  Else
    RTF.Control('line');        // TODO: should do a horizontal line, but too complicated
End;


Function TWPRTFWriter.Styled: Boolean;
Begin
  Result := True;
End;


function TWPRTFWriter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFonts.sizeInBytes);
  inc(result, FContexts.sizeInBytes);
  inc(result, FRTF.sizeInBytes);
  inc(result, FContext.sizeInBytes);
  inc(result, FColours.sizeInBytes);
  inc(result, FLists.sizeInBytes);
  inc(result, FListsIndexer.sizeInBytes);
end;

Constructor TWPRTFContext.Create;
Begin
  Inherited;
  Plain;
  Pard;
End;


Destructor TWPRTFContext.Destroy;
Begin
  Inherited;
End;


Function TWPRTFContext.Link : TWPRTFContext;
Begin
  Result := TWPRTFContext(Inherited Link);
End;


Function TWPRTFContext.Clone : TWPRTFContext;
Begin
  Result := TWPRTFContext(Inherited Clone);
End;


Procedure TWPRTFContext.Assign(oObject : TFslObject);
Begin
  Inherited;

  FontName := TWPRTFContext(oObject).FontName;
  Style := TWPRTFContext(oObject).Style;
  FontSize := TWPRTFContext(oObject).FontSize;
  Bold := TWPRTFContext(oObject).Bold;
  Italic := TWPRTFContext(oObject).Italic;
  Underline := TWPRTFContext(oObject).Underline;
  Strikethrough := TWPRTFContext(oObject).Strikethrough;
  Foreground := TWPRTFContext(oObject).Foreground;
  Background := TWPRTFContext(oObject).Background;
  State := TWPRTFContext(oObject).State;
  Alignment := TWPRTFContext(oObject).Alignment;
  LeftIndent := TWPRTFContext(oObject).LeftIndent;
  RightIndent := TWPRTFContext(oObject).RightIndent;
  Capitalization := TWPRTFContext(oObject).Capitalization;
End;


Function TWPRTFContext.ErrorClass : EFslExceptionClass;
Begin
  Result := EWPRTFContext;
End;


function TWPRTFContext.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FFontName.length * sizeof(char)) + 12);
  inc(result, (FStyle.length * sizeof(char)) + 12);
end;

Function TWPRTFContexts.Link : TWPRTFContexts;
Begin
  Result := TWPRTFContexts(Inherited Link);
End;


Function TWPRTFContexts.Clone : TWPRTFContexts;
Begin
  Result := TWPRTFContexts(Inherited Clone);
End;


Function TWPRTFContexts.New : TWPRTFContext;
Begin
  Result := TWPRTFContext(Inherited New);
End;


Function TWPRTFContexts.NewContext(Const sDefaultStyle : String)  : TWPRTFContext;
Begin
  Result := New;
  Try
    Result.FStyle := sDefaultStyle;
    Add(Result.Link);
  Finally
    Result.Free;
  End;
End;


Function TWPRTFContexts.CloseContext : TWPRTFContext;
Begin
  Assert(CheckCondition(Count > 0, 'CloseContext', 'Attempt to close context when no context is open'));
  DeleteByIndex(Count - 1);
  If Count = 0 Then
    Result := Nil
  Else
    Result := Elements[Count - 1];
End;


Function TWPRTFContexts.ItemClass : TFslObjectClass;
Begin
  Result := TWPRTFContext;
End;


Function TWPRTFContexts.GetElement(Const iIndex : Integer) : TWPRTFContext;
Begin
  Result := TWPRTFContext(ObjectByIndex[iIndex]);
End;


Procedure TWPRTFContexts.SetElement(Const iIndex : Integer; Const oValue : TWPRTFContext);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPRTFContexts.Get(Const aValue : Integer) : TWPRTFContext;
Begin
  Result := TWPRTFContext(Inherited Get(aValue));
End;


Procedure TWPRTFContext.Plain;
Begin
  FFontName := ''; // TWPRTFContextFontName;
  FStyle := ''; // TWPRTFContextStyle;
  FFontSize := 12; // TWPRTFContextFontSize;
  FBold := False; // TWPRTFContextBold;
  FItalic := False; // TWPRTFContextItalic;
  FUnderline := False; // TWPRTFContextUnderline;
  FStrikethrough := False; // TWPRTFContextStrikethrough;
  FForeground := DEF_COLOUR; // TColour;
  FBackground := DEF_COLOUR; // TColour;
  FState := fsNormal; // TWPSFontState;
  Capitalization := False;
End;

Procedure TWPRTFContext.Pard;
Begin
  Alignment := WordProcessorParagraphAlignmentUnknown;
  FLeftIndent := DEF_INT;
  FRightIndent := DEF_INT;
End;


Constructor TWPRTFListDefinition.Create;
Begin
  Inherited;
  FLevels := TWPRTFListLevelDefinitions.Create;
End;


Destructor TWPRTFListDefinition.Destroy;
Begin
  FLevels.Free;
  Inherited;
End;


Function TWPRTFListDefinition.Link : TWPRTFListDefinition;
Begin
  Result := TWPRTFListDefinition(Inherited Link);
End;


Function TWPRTFListDefinition.Clone : TWPRTFListDefinition;
Begin
  Result := TWPRTFListDefinition(Inherited Clone);
End;


Procedure TWPRTFListDefinition.Assign(oObject : TFslObject);
Begin
  Inherited;
  FLevels.Assign(TWPRTFListDefinition(oObject).FLevels);
  FId := TWPRTFListDefinition(oObject).FId;
  FTemplateId := TWPRTFListDefinition(oObject).FTemplateId;
  FName := TWPRTFListDefinition(oObject).FName;
End;


function TWPRTFListDefinition.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FLevels.sizeInBytes);
end;

Function TWPRTFListDefinitions.Link : TWPRTFListDefinitions;
Begin
  Result := TWPRTFListDefinitions(Inherited Link);
End;


Function TWPRTFListDefinitions.Clone : TWPRTFListDefinitions;
Begin
  Result := TWPRTFListDefinitions(Inherited Clone);
End;


Function TWPRTFListDefinitions.New : TWPRTFListDefinition;
Begin
  Result := TWPRTFListDefinition(Inherited New);
End;


Function TWPRTFListDefinitions.ItemClass : TFslObjectClass;
Begin
  Result := TWPRTFListDefinition;
End;


Function TWPRTFListDefinitions.GetElement(Const iIndex : Integer) : TWPRTFListDefinition;
Begin
  Result := TWPRTFListDefinition(ObjectByIndex[iIndex]);
End;


Procedure TWPRTFListDefinitions.SetElement(Const iIndex : Integer; Const oValue : TWPRTFListDefinition);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPRTFListDefinitions.Get(Const aValue : Integer) : TWPRTFListDefinition;
Begin
  Result := TWPRTFListDefinition(Inherited Get(aValue));
End;


Function TWPRTFListDefinitions.GetById(iId: Integer): TWPRTFListDefinition;
Var
  iLoop : Integer;
  oItem : TWPRTFListDefinition;
Begin
  Result := Nil;
  For iLoop := 0 To Count - 1 Do
  Begin
    oItem := Elements[iLoop];
    If (oItem.Id = iId) Then
      Result := oItem;
  End;
End;


Constructor TWPRTFListLevelDefinition.Create;
Begin
  Inherited;
  FNumberFormat := -1;
  FStart := -1;
End;


Destructor TWPRTFListLevelDefinition.Destroy;
Begin
  Inherited;
End;


Function TWPRTFListLevelDefinition.Link : TWPRTFListLevelDefinition;
Begin
  Result := TWPRTFListLevelDefinition(Inherited Link);
End;


Function TWPRTFListLevelDefinition.Clone : TWPRTFListLevelDefinition;
Begin
  Result := TWPRTFListLevelDefinition(Inherited Clone);
End;


Procedure TWPRTFListLevelDefinition.Assign(oObject : TFslObject);
Begin
  Inherited;

  FStart := TWPRTFListLevelDefinition(oObject).FStart;
  FNumberFormat := TWPRTFListLevelDefinition(oObject).FNumberFormat;
End;


function TWPRTFListLevelDefinition.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

Function TWPRTFListLevelDefinitions.Link : TWPRTFListLevelDefinitions;
Begin
  Result := TWPRTFListLevelDefinitions(Inherited Link);
End;


Function TWPRTFListLevelDefinitions.Clone : TWPRTFListLevelDefinitions;
Begin
  Result := TWPRTFListLevelDefinitions(Inherited Clone);
End;


Function TWPRTFListLevelDefinitions.New : TWPRTFListLevelDefinition;
Begin
  Result := TWPRTFListLevelDefinition(Inherited New);
End;


Function TWPRTFListLevelDefinitions.ItemClass : TFslObjectClass;
Begin
  Result := TWPRTFListLevelDefinition;
End;


Function TWPRTFListLevelDefinitions.GetElement(Const iIndex : Integer) : TWPRTFListLevelDefinition;
Begin
  Result := TWPRTFListLevelDefinition(ObjectByIndex[iIndex]);
End;


Procedure TWPRTFListLevelDefinitions.SetElement(Const iIndex : Integer; Const oValue : TWPRTFListLevelDefinition);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPRTFListLevelDefinitions.Get(Const aValue : Integer) : TWPRTFListLevelDefinition;
Begin
  Result := TWPRTFListLevelDefinition(Inherited Get(aValue));
End;



Constructor TWPRTFListLevelOverrideDefinition.Create;
Begin
  Inherited;
  FStart := -1;

End;


Destructor TWPRTFListLevelOverrideDefinition.Destroy;
Begin
  Inherited;
End;


Function TWPRTFListLevelOverrideDefinition.Link : TWPRTFListLevelOverrideDefinition;
Begin
  Result := TWPRTFListLevelOverrideDefinition(Inherited Link);
End;


Function TWPRTFListLevelOverrideDefinition.Clone : TWPRTFListLevelOverrideDefinition;
Begin
  Result := TWPRTFListLevelOverrideDefinition(Inherited Clone);
End;


Procedure TWPRTFListLevelOverrideDefinition.Assign(oObject : TFslObject);
Begin
  Inherited;

  FStart := TWPRTFListLevelOverrideDefinition(oObject).FStart;
End;


function TWPRTFListLevelOverrideDefinition.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

Function TWPRTFListLevelOverrideDefinitions.Link : TWPRTFListLevelOverrideDefinitions;
Begin
  Result := TWPRTFListLevelOverrideDefinitions(Inherited Link);
End;


Function TWPRTFListLevelOverrideDefinitions.Clone : TWPRTFListLevelOverrideDefinitions;
Begin
  Result := TWPRTFListLevelOverrideDefinitions(Inherited Clone);
End;


Function TWPRTFListLevelOverrideDefinitions.New : TWPRTFListLevelOverrideDefinition;
Begin
  Result := TWPRTFListLevelOverrideDefinition(Inherited New);
End;


Function TWPRTFListLevelOverrideDefinitions.ItemClass : TFslObjectClass;
Begin
  Result := TWPRTFListLevelOverrideDefinition;
End;


Function TWPRTFListLevelOverrideDefinitions.GetElement(Const iIndex : Integer) : TWPRTFListLevelOverrideDefinition;
Begin
  Result := TWPRTFListLevelOverrideDefinition(ObjectByIndex[iIndex]);
End;


Procedure TWPRTFListLevelOverrideDefinitions.SetElement(Const iIndex : Integer; Const oValue : TWPRTFListLevelOverrideDefinition);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPRTFListLevelOverrideDefinitions.Get(Const aValue : Integer) : TWPRTFListLevelOverrideDefinition;
Begin
  Result := TWPRTFListLevelOverrideDefinition(Inherited Get(aValue));
End;



Constructor TWPRTFListOverrideDefinition.Create;
Begin
  Inherited;
  FLevels := TWPRTFListLevelOverrideDefinitions.Create;
End;


Destructor TWPRTFListOverrideDefinition.Destroy;
Begin
  FLevels.Free;
  Inherited;
End;


Function TWPRTFListOverrideDefinition.Link : TWPRTFListOverrideDefinition;
Begin
  Result := TWPRTFListOverrideDefinition(Inherited Link);
End;


Function TWPRTFListOverrideDefinition.Clone : TWPRTFListOverrideDefinition;
Begin
  Result := TWPRTFListOverrideDefinition(Inherited Clone);
End;


Procedure TWPRTFListOverrideDefinition.Assign(oObject : TFslObject);
Begin
  Inherited;
  FLevels.Assign(TWPRTFListOverrideDefinition(oObject).FLevels);
  FId := TWPRTFListOverrideDefinition(oObject).FId;
  FListId := TWPRTFListOverrideDefinition(oObject).FListId;
End;


function TWPRTFListOverrideDefinition.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FLevels.sizeInBytes);
end;

Function TWPRTFListOverrideDefinitions.Link : TWPRTFListOverrideDefinitions;
Begin
  Result := TWPRTFListOverrideDefinitions(Inherited Link);
End;


Function TWPRTFListOverrideDefinitions.Clone : TWPRTFListOverrideDefinitions;
Begin
  Result := TWPRTFListOverrideDefinitions(Inherited Clone);
End;


Function TWPRTFListOverrideDefinitions.New : TWPRTFListOverrideDefinition;
Begin
  Result := TWPRTFListOverrideDefinition(Inherited New);
End;


Function TWPRTFListOverrideDefinitions.ItemClass : TFslObjectClass;
Begin
  Result := TWPRTFListOverrideDefinition;
End;


Function TWPRTFListOverrideDefinitions.GetElement(Const iIndex : Integer) : TWPRTFListOverrideDefinition;
Begin
  Result := TWPRTFListOverrideDefinition(ObjectByIndex[iIndex]);
End;


Procedure TWPRTFListOverrideDefinitions.SetElement(Const iIndex : Integer; Const oValue : TWPRTFListOverrideDefinition);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPRTFListOverrideDefinitions.Get(Const aValue : Integer) : TWPRTFListOverrideDefinition;
Begin
  Result := TWPRTFListOverrideDefinition(Inherited Get(aValue));
End;


Function TWPRTFListOverrideDefinitions.GetById(iId: Integer): TWPRTFListOverrideDefinition;
Var
  iLoop : Integer;
  oItem : TWPRTFListOverrideDefinition;
Begin
  Result := Nil;
  For iLoop := 0 To Count - 1 Do
  Begin
    oItem := Elements[iLoop];
    If (oItem.Id = iId) Then
      Result := oItem;
  End;
End;


Function TFslRTFExtractor.PeekIsGroupStart : Boolean;
Begin
  Result := Not FIsBreak And (NextCharacter = '{');
End;


Function TFslRTFExtractor.PeekIsGroupClose : Boolean;
Begin
  Result := Not FIsBreak And (NextCharacter = '}');
End;


Function TFslRTFExtractor.PeekIsControl : Boolean;
Begin
  Result := Not FIsBreak And (NextCharacter = '\');
End;


Function TFslRTFExtractor.PeekIsBreak : Boolean;
Begin
  Result := FIsBreak;
End;

Procedure TFslRTFExtractor.ConsumeGroupStart;
Begin
  Assert(CheckCondition(NextCharacter = '{', 'ConsumeGroupStart', 'Found "'+NextCharacter+'" consuming {'));
  ConsumeCharacter;
  FIsBreak := False;
End;


Procedure TFslRTFExtractor.ConsumeGroupClose;
Begin
  Assert(CheckCondition(NextCharacter = '}', 'ConsumeGroupClose', 'Found "'+NextCharacter+'" consuming }'));
  ConsumeCharacter;
  FIsBreak := More And (NextCharacter = ';');
  ConsumeWhiteSpace(false);
End;


Function TFslRTFExtractor.ConsumeControlInner : String;
Begin
  If FControl <> '' Then
    Result := FControl
  Else
  Begin
    Assert(CheckCondition(NextCharacter = '\', 'ConsumeGroupClose', 'Found "'+NextCharacter+'" consuming \'));
    ConsumeCharacter; // '\'
    Result := ConsumeCharacter;
    // work around an apparent Word Bug: \~ does not have a following space
    If Result <> '~' Then
    Begin
      If Result = '''' Then
        Result := Result + ConsumeCharacterCount(2)
      Else If (Result = '\') Or (Result = '{') Or (Result = '}') Then
        Result := Result     // escape special characters
      Else
      Begin
        Result := Result + ConsumeUntilCharacterSet([' ', '\', '{', '}', ';', #13, #10]);
        ConsumeWhiteSpace(true);
      End;
    End;
    EatEoln;
  End;
  FControl := '';
End;


Function TFslRTFExtractor.ConsumeControl : String;
Begin
  Result := ConsumeControlInner;
  FIsBreak := NextCharacter = ';';
End;


Function TFslRTFExtractor.ConsumeBreak : String;
Begin
  Assert(CheckCondition(FIsBreak And (NextCharacter = ';'), 'ConsumeGroupClose', 'Found "'+NextCharacter+'" consuming ;'));
  ConsumeCharacter;
  FIsBreak := False;
  ConsumeWhiteSpace(true);
End;


Function TFslRTFExtractor.ConsumeText : String;
Begin
  Assert(CheckCondition(Not (FIsBreak Or CharInSet(NextCharacter, ['\', '{', '}'])), 'ConsumeText', 'not at text consuming text'));
  Result := ConsumeUntilCharacterSet(['\', '{', '}', #13, #10]);
  EatEoln;
  FIsBreak := False;
End;


Procedure TFslRTFExtractor.ConsumeWhiteSpace(IncludeSpace : boolean);
Begin
  While More And CharInSet(NextCharacter, setVertical) Do
    ConsumeCharacter;

  If More And IncludeSpace And (NextCharacter = ' ') Then
    ConsumeCharacter;

  While More And CharInSet(NextCharacter, setVertical) Do
    ConsumeCharacter;
End;


Function TFslRTFExtractor.PeekControl : String;
Begin
  Result := ConsumeControlInner;
  FControl := Result;
End;


Function TFslRTFExtractor.PeekIsNumericControl(Const sName : String): Boolean;
Var
  sValue : String;
Begin
  sValue := PeekControl;
  Result := (Copy(sValue, 1, Length(sName)) = sName) And (Length(Copy(sValue, Length(sName) + 1, $FF)) > 0) And (StringIsInteger32(Copy(sValue, Length(sName) + 1, $FF)));
End;

Function TFslRTFExtractor.PeekIsUnicodeChar(): Boolean;
Var
  sValue : String;
Begin
  sValue := PeekControl;
  Result := (Copy(sValue, 1, 1) = 'u') And (Length(Copy(sValue, 2, length(sValue)-2)) > 0) And (StringIsInteger32(Copy(sValue, 2, length(sValue)-2)));
End;


function TFslRTFExtractor.ReadUnicodeChar: Char;
Var
  sValue : String;
Begin
  sValue := ConsumeControl;
  if (Copy(sValue, 1, 1) = 'u') And (Length(Copy(sValue, 2, length(sValue)-2)) > 0) And (StringIsInteger32(Copy(sValue, 2, length(sValue)-2))) then
  {$IFDEF VER130}
    result := sValue[length(sValue)]
  {$ELSE}
    result := Char(StringToInteger32(Copy(sValue, 2, length(sValue)-2)))
  {$ENDIF}
  else
    result := '?';
end;

Function TFslRTFExtractor.ConsumeNumericControl(Const sName : String) : Integer;
Var
  sValue : String;
Begin
  sValue := ConsumeControl;
  Assert(CheckCondition(Copy(sValue, 1, Length(sName)) = sName, 'ConsumeNumericControl', StringFormat('Found "%s" trying to read "%s"', [sValue, sName])));
  Result := StringToInteger32(Copy(sValue, Length(sName)+1, $FF));
End;


Function TFslRTFExtractor.PeekIsBooleanControl(Const sName : String): Boolean;
Var
  sValue : String;
Begin
  sValue := PeekControl;
  Result := (Copy(sValue, 1, Length(sName)) = sName) And ((Length(sValue) = Length(sName)) Or (Copy(sValue, Length(sName)+1, $FF) = '0'));
End;


Function TFslRTFExtractor.ConsumeBooleanControl(Const sName : String) : Boolean;
Var
  sValue : String;
Begin
  sValue := ConsumeControl;
  Assert(CheckCondition(Copy(sValue, 1, Length(sName)) = sName, 'ConsumeNumericControl', StringFormat('Found "%s" trying to read "%s"', [sValue, sName])));
  Result := Length(sName) = Length(sValue)
End;


Procedure TFslRTFExtractor.EatEoln;
Begin
  While More And CharInSet(NextCharacter, setVertical) Do
    ConsumeCharacter;
End;

function TFslRTFExtractor.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FControl.length * sizeof(char)) + 12);
end;

Procedure TFslRTFFormatter.StartGroup;
Begin
  Produce('{');
  Inc(FLineCount);
  FWasControl := True;
End;


Procedure TFslRTFFormatter.CloseGroup;
Begin
  FWasControl := False;
  Produce('}');
  Inc(FLineCount);
  CheckNewLine;
End;


Procedure TFslRTFFormatter.Break(Const sWord : String);
Begin
  FWasControl := True;
  Produce(sWord);
  Inc(FLineCount, Length(sWord));
End;

Procedure TFslRTFFormatter.Control(Const sWord : String);
Begin
  FWasControl := True;
  Produce('\' + sWord);
  Inc(FLineCount, Length(sWord)+1);
End;


Procedure TFslRTFFormatter.ControlBoolean(Const sWord : String; Const bOn : Boolean);
Begin
  If bOn Then
    Control(sWord)
  Else
    Control(sWord + '0');
End;


Procedure TFslRTFFormatter.Text(Const sText : String);
Var
  iLoop : Integer;
  s : String;
Begin
  If sText <> '' Then
  Begin
    If FWasControl Then
    Begin
      Produce(' ');
      Inc(FLineCount, 1);
      CheckNewLine;
    End;

    For iLoop := 1 To Length(sText) Do
    Begin
      If CharInSet(sText[iLoop], ['\', '{', '}']) Then
      Begin
        Produce('\' + sText[iLoop]);
        Inc(FLineCount, 2);
      End
      Else if Ord(sText[iLoop]) > 126 then
      Begin
        s := inttostr(Ord(sText[iLoop]));
        Produce('\u' + s +'?');
        Inc(FLineCount, length(s)+3);
      End
      else
      Begin
        Produce(sText[iLoop]);
        Inc(FLineCount, 1);
      End;
    End;

    FWasControl := False;
  End;
End;


Procedure TFslRTFFormatter.CheckNewLine;
Begin
  If (FLinelimit <> 0) And (FLineCount > FLinelimit) Then
  Begin
    ProduceNewLine;
    FLineCount := 0;
  End;
End;

function TFslRTFFormatter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

End.


