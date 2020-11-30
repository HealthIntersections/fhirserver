Unit wp_odt;

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
  SysUtils, Classes, Vcl.Graphics,
  fsl_base, fsl_stream, fsl_utilities, fsl_xml, fsl_collections,

  wp_graphics, wp_types, wp_document, wp_working, wp_format,
  wp_imaging;

Type

  TWPOpenDocPackagePart = Class(TFslZipPart)
    Private
      FMimeType : String;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      Function Link : TWPOpenDocPackagePart;
      Function Clone : TWPOpenDocPackagePart;

      Procedure Assign(oObject : TFslObject); Override;

      Property MimeType : String Read FMimeType Write FMimeType;
  End;

  TWPOpenDocPackage = Class(TFslZipPartList)
    Private
      Function GetPart(iIndex : Integer) : TWPOpenDocPackagePart;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Function Link : TWPOpenDocPackage;
      Function Clone : TWPOpenDocPackage;

      Function GetByName(Const sName : String) : TWPOpenDocPackagePart;

      Property Part[iIndex : Integer] : TWPOpenDocPackagePart Read GetPart; Default;
  End;

{*****************************************************************************
* Open Document are stored in JAR format, where the one zip file can contain
* either one XML document; or a collection of XML documents, each represents
* part of the full open document (such as styles, metadata, ect).
* This adapter attempts to abstract the above organization, so that reader
* can directly access individual parts of the document without knowing whether
* they come from different files, or merely from different portion of an XML
* document.
*****************************************************************************}


Const
  { Some constant for testing, while we don't have a jar extractor yet }
  MANIFEST_PATH = 'META-INF/manifest.xml';   // path to the manifest file

  // name of sub-document when they are separated from the master XML file
  SUBDOC_CONTENT = 'content.xml';
  SUBDOC_STYLE = 'styles.xml';
  SUBDOC_META = 'meta.xml';
  SUBDOC_SETTINGS = 'settings.xml';


  // Some namespace in Open Document specification
  MANIFEST_NS = 'urn:oasis:names:tc:opendocument:xmlns:manifest:1.0';
  OFFICE_NS = 'urn:oasis:names:tc:opendocument:xmlns:office:1.0';
  META_NS = 'urn:oasis:names:tc:opendocument:xmlns:meta:1.0';
  CONFIG_NS = 'urn:oasis:names:tc:opendocument:xmlns:config:1.0';
  TEXT_NS = 'urn:oasis:names:tc:opendocument:xmlns:text:1.0';
  TABLE_NS = 'urn:oasis:names:tc:opendocument:xmlns:table:1.0';
  DRAWING_NS = 'urn:oasis:names:tc:opendocument:xmlns:drawing:1.0';
  DR3D_NS = 'urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0';
  PRESENTATION_NS = 'urn:oasis:names:tc:opendocument:xmlns:presentation:1.0';
  GRAPHIC3D_NS = 'urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0';
  ANIMATION_NS = 'urn:oasis:names:tc:opendocument:xmlns:animation:1.0';
  CHART_NS = 'urn:oasis:names:tc:opendocument:xmlns:chart:1.0';
  FORM_NS = 'urn:oasis:names:tc:opendocument:xmlns:form:1.0';
  SCRIPT_NS = 'urn:oasis:names:tc:opendocument:xmlns:script:1.0';
  STYLE_NS = 'urn:oasis:names:tc:opendocument:xmlns:style:1.0';
  NUMBER_NS = 'urn:oasis:names:tc:opendocument:xmlns:data style:1.0';

  // some non-ODT namespace
  XLINK_NS = 'http://www.w3.org/1999/xlink';
  FONT_NS = 'urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0';
  SVG_NS = 'urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0';
  SMIL_NS = 'urn:oasis:names:tc:opendocument:xmlns:smil-compatible:1.0';
  MATH_NS = 'http://www.w3.org/1998/Math/MathML';

  // some main element name
  SCRIPT_ELENAME = 'script';
  SCRIPT_GROUPNAME = 'scripts';
  STYLE_GROUPNAME = 'styles';
  AUTOMATIC_STYLE_GROUPNAME = 'automatic-styles';
  FONT_DECLARATION_GROUPNAME = 'font-face-decls';
  FONT_ELENAME = 'font-face';
  BODY_ELENAME = 'body';

  // some known Open Document element
  MANIFEST_ELEMENT = 'manifest';


Type
  TOpenDocumentSubDocType = (OpenDocumentSubDocMeta, OpenDocumentSubDocSettings,
                OpenDocumentSubDocStyle, OpenDocumentSubDocContent );

  TOdtReaderAdapter = Class(TFslObject)
  Private
    // info extracted from the manifest. They include the full path
    // (often be '/'), document version and mediatype.
    FVersion: String;
    FMediaType: String;

    // the raw input stream (jar raw data)
    FPackage : TWPOpenDocPackage;

    Function GetFileByPath(Const sFilePath: String): TFslBuffer;

    Procedure ReadManifest(oExtractor: TFslXMLExtractor);
    Procedure LoadManifest; Virtual;

  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Function Link : TOdtReaderAdapter; Overload;

    Procedure Load(oStream : TFslStream); Overload; Virtual;

    // manifest-related property
    Property Version: String Read FVersion;
    Property MediaType: String Read FMediaType;

    Function GetSubDoc(Const aType: TOpenDocumentSubDocType): TFslXMLExtractor; Virtual;
  End;


  TOdtWriterAdapter = Class(TFslObject)
  Private
    FPackage : TWPOpenDocPackage;

  Protected
    Procedure WriteMIMEType; Virtual;
    Procedure WriteManifest; Virtual;

    Function GetByName(Const sName, sMimeType : String) : TFslBuffer;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Function Link : TOdtWriterAdapter; Overload;

    Procedure Finish(oStream : TFslStream); Virtual;

    Function GetSubDocStream(Const aType: TOpenDocumentSubDocType): TFslBuffer; Virtual;

  End;

{*****************************************************************************
* Each style in OpenDocument (ODT) define both basic formating information, as
* well as a locally unique id, and some hierarchy structure. The main styles
* properties are:
* + Family: One of [paragraph, text, section, table, table-column,
*   table-row, table-cell, table-page, chart, default, drawing-page,
*   graphic, presentation, control, ruby]
* + Name/('display name' if not existed): Unique in a family
* + Parent (inherited ?): must existed, but can't be automatic style
* + Next style: default to current, specify paragraph style after paragraph break
* + Formating Properties, which can be one of:
*   - page layout properties
*   - page header and footer properties
*   - text properties
*   - paragraph properties.
*   - text section properties.
*   - ruby section properties.
*   - list properties.
*   - for table properties.
*   - for table column properties.
*   - table row properties.
*   - table cell properties.
*   - drawing object properties.
* While the formatting properties are opened (custom elements/attributes are
* allowed), each group of formatting properties does have
* some predefined elements/attributes.
******************************************************************************}


Type

  TOdtStyleFamily = (osfParagraph, osfText, osfSection, osfTable,
                osfTableColumn, osfTableRow, osfTableCell,
                osfTablePage, osfChart, osfDefault, osfDrawingPage,
                osfGraphic, osfPresentation, osfControl, osfRuby);

  {********************************************************************
  * + Default: style assigned by default to a family, one each family
  * + Common:  style with name that can be selected by user
  * + Automatic: anonymous (noname) style, normally derived from common
  *              style with some modification (bold, italic, etc)
  ********************************************************************}
  TOdtStyleType = (ostDefault, ostCommon, ostAutomatic);

Const
  NAMES_TODTSTYLEFAMILY : Array [TOdtStyleFamily] Of String = ('paragraph',
                'text', 'section', 'table', 'table-column', 'table-row',
                'table-cell', 'table-page', 'chart', 'default',
                'drawing-page', 'graphic', 'presentation', 'control', 'ruby');

Type

  TOdtFontDeclarations = Class(TFslStringMatch);

  {***************************************************************************
  * This represents only part of the predefined properties of text formatting,
  * mostly those have to do with font formatting. So this is kind of
  * corresponding to our own TWPSFontDetails
  ***************************************************************************}
  TOdtTextFormat = Class(TFslObject)
  Private
    // font name: Used with TOdtFontDeclarations to work out the real font
    FName: String;
    FFamily: String;
    FSize: String;     // can be an absolute, percentage or relative

    // Font color
    FColor: TColour;
    FUseWindowColor: Boolean;
    FBackground: TColour;       // can be transparent (DEF_COLOUR) or color

    FStyle: String;             // Italic: normal, italic, obligue
    FWeight: String;            // Bold: normal, bold, 100 .. 900
    FPosition: String;          // Fontstate: subscript, superscript, percentage

    // underline
    FUnderlineType: String;     // None, single, double
    FUnderlineStyle: String;    // None, solid, dotted, dash .. wave

    // strike through
    FLineThroughType: String;   // None, single, double
    FLineThroughStyle: String;  // None, solid, dotted, dash .. wave

    FTextTransform: String;     // Capitalize: None, lowercase, uppercase, capitalize

  Protected
    // convert between WPStyles and OdtStyles
    Function GetFontSize: Integer; Overload; Virtual;
    Procedure SetFontSize(Const iValue: Integer); Overload; Virtual;

    Function GetForeground: TColour; Overload; Virtual;
    Procedure SetForeground(Const color: TColour); Overload; Virtual;

    Function GetBackground: TColour; Overload; Virtual;
    Procedure SetBackground(Const color: TColour); Overload; Virtual;

    Function GetItalic: TWPSTriState; Overload; Virtual;
    Procedure SetItalic(Const oItalic: TWPSTriState); Overload; Virtual;

    Function GetBold: TWPSTriState; Overload; Virtual;
    Procedure SetBold(Const oBold: TWPSTriState); Overload; Virtual;

    Function GetFontState: TWPSFontState; Overload; Virtual;
    Procedure SetFontState(Const oFontState: TWPSFontState); Overload; Virtual;

    Function GetUnderline: TWPSTriState; Overload; Virtual;
    Procedure SetUnderline(Const oUnderline: TWPSTriState); Overload; Virtual;

    Function GetStrikeThrough: TWPSTriState; Overload; Virtual;
    Procedure SetStrikeThrough(Const oStrikethrough: TWPSTriState); Overload; Virtual;

    Function GetCapitalization: TWPSCapsState; Overload; Virtual;
    Procedure SetCapitalization(Const oCapitalize: TWPSCapsState); Overload; Virtual;

    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Overload; Override;
    constructor Create(Const oParent: TOdtTextFormat); Overload;
    constructor Create(Const oFormat: TWPSFontDetails); Overload;
    destructor Destroy; Override;

    Function Link: TOdtTextFormat;

    Function Matches(Const oFormat: TWPSFontDetails; Const bIsStrict : Boolean = True): Boolean; Overload;

    Function HasName: Boolean; Virtual;
    Function HasFamily: Boolean; Virtual;
    Function HasSize: Boolean; Virtual;
    Function HasColor: Boolean; Virtual;
    Function HasUseWindowColor: Boolean; Virtual;
    Function HasBackground: Boolean; Virtual;
    Function HasStyle: Boolean; Virtual;
    Function HasWeight: Boolean; Virtual;
    Function HasPosition: Boolean; Virtual;
    Function HasUnderlineType: Boolean; Virtual;
    Function HasUnderlineStyle: Boolean; Virtual;
    Function HasLineThroughType: Boolean; Virtual;
    Function HasLineThroughStyle: Boolean; Virtual;
    Function HasTextTransform: Boolean; Virtual;

    // WP Properties
    Property WPFontSize: Integer Read GetFontSize Write SetFontSize;
    Property WPForeGround: TColour Read GetForeground Write SetForeground;
    Property WPBackground: TColour Read GetBackground Write SetBackground;
    Property WPItalic: TWPSTriState Read GetItalic Write SetItalic;
    Property WPBold: TWPSTriState Read GetBold Write SetBold;
    Property WPFontState: TWPSFontState Read GetFontState Write SetFontState;
    Property WPUnderline: TWPSTriState Read GetUnderline Write SetUnderline;
    Property WPStrikeThrough: TWPSTriState Read GetStrikeThrough Write SetStrikeThrough;
    Property WPCapitalization: TWPSCapsState Read GetCapitalization Write SetCapitalization;

    // Raw properties
    Property Name: String Read FName Write FName;
    Property Family: String Read FFamily Write FFamily;
    Property Size: String Read FSize Write FSize;
    Property Color: TColour Read FColor Write FColor;
    Property UseWindowColor: Boolean Read FUseWindowColor Write FUseWindowColor;
    Property Background: TColour Read FBackground Write FBackground;
    Property Style: String Read FStyle Write FStyle;
    Property Weight: String Read FWeight Write FWeight;
    Property Position: String Read FPosition Write FPosition;

    Property UnderlineType: String Read FUnderlineType Write FUnderlineType;
    Property UnderlineStyle: String Read FUnderlineStyle Write FUnderlineStyle;
    Property LineThroughType: String Read FLineThroughType Write FLineThroughType;
    Property LineThroughStyle: String Read FLineThroughStyle Write FLineThroughStyle;

    Property TextTransform: String Read FTextTransform Write FTextTransform;
  End;

  {***************************************************************************
  * Similar to above, this represent TWPSParagraphDetails
  ***************************************************************************}
  TOdtParagraphFormat = Class(TFslObject)
  Private
    FTextAlign: String; // fo:text-align == TWordProcessorParagraphAlignment

    FMarginLeft, FMarginRight, FMarginBottom: String;
    FBreakBefore, FBreakAfter: String;          // mutual exclusive
  Protected
    // convert between WPStyles and OdtStyles
    Function GetAlign: TWordProcessorParagraphAlignment; Overload; Virtual;
    Procedure SetAlign(Const aAlign: TWordProcessorParagraphAlignment); Overload; Virtual;

    Function GetMarginLeft: Integer; Overload; Virtual;
    Procedure SetMarginLeft(Const aValue: Integer); Overload; Virtual;

    Function GetMarginRight: Integer; Overload; Virtual;
    Procedure SetMarginRight(Const aValue: Integer); Overload; Virtual;

    Function GetMarginBottom: Integer; Overload; Virtual;
    Procedure SetMarginBottom(Const aValue: Integer); Overload; Virtual;

    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Overload; Override;
    constructor Create(Const oParent: TOdtParagraphFormat); Overload;
    constructor Create(Const oFormat: TWPSParagraphDetails); Overload;
    destructor Destroy; Override;

    Function Link: TOdtParagraphFormat;

    Function Matches(Const oFormat: TWPSParagraphDetails): Boolean; Overload; Virtual;

    Function IsBreakBefore: Boolean; Virtual;
    Function IsBreakAfter: Boolean; Virtual;

    // WP properties
    Property WPAlignt: TWordProcessorParagraphAlignment Read GetAlign Write SetAlign;
    Property WPLeftIndent: Integer Read GetMarginLeft Write SetMarginLeft;
    Property WPRightIndent: Integer Read GetMarginRight Write SetMarginRight;
    Property WPMarginBottom: Integer Read GetMarginBottom Write SetMarginBottom;

    // raw properties
    Property TextAlign: String Read FTextAlign Write FTextAlign;
    Property MarginLeft: String Read FMarginLeft Write FMarginLeft;
    Property MarginRight: String Read FMarginRight Write FMarginRight;
    Property MarginBottom: String Read FMarginBottom Write FMarginBottom;
    Property BreakBefore: String Read FBreakBefore Write FBreakBefore;
    Property BreakAfter: String Read FBreakAfter Write FBreakAfter;
  End;

  {***************************************************************************
  * Similar to above, this represent the list format in TWPSParagraphDetails
  ***************************************************************************}
  TOdtListLevelFormat = Class(TFslObject)

    FBulletType: TWPSParagraphBulletType;

    FNumberStyle : String;       // TWPSParagraphNumberType;
    FNumberFormat : String;      // TWPSParagraphNumberFormat;
    FStartValue: String;
  Protected
    // convert between WPStyles and OdtStyles
    Function GetListType : TWPSParagraphListType; Virtual;
    Function GetNumberStyle : TWPSParagraphNumberType; Virtual;
    Function GetNumberFormat : TWPSParagraphNumberFormat; Virtual;
    Procedure SetNumberStyle(Const oType: TWPSParagraphNumberType); Virtual;
    Procedure SetNumberFormat(Const oFormat: TWPSParagraphNumberFormat); Virtual;

    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Overload; Override;
    constructor Create(Const oParent: TOdtListLevelFormat); Overload;
    constructor Create(Const oFormat: TWPSParagraphDetails); Overload;
    destructor Destroy; Override;

    Function Link: TOdtListLevelFormat;

    Procedure ApplyStyle(oParaFormat: TWPSParagraphDetails; Const iItemCount: Integer);
    Function Matches(Const oFormat: TWPSParagraphDetails; Const iCount: Integer): Boolean; Overload; Virtual;

    Function GetNumber(Const iIndex: Integer): Integer; Virtual;
    Procedure SetStartNumber(Const iNum: Integer); Virtual;

    // WP Properties
    Property WPListType: TWPSParagraphListType Read GetListType;
    Property WPNumberStyle: TWPSParagraphNumberType Read GetNumberStyle Write SetNumberStyle;
    Property WPNumberFormat: TWPSParagraphNumberFormat Read GetNumberFormat Write SetNumberFormat;

    // raw properties
    Property BulletType : TWPSParagraphBulletType Read FBulletType Write FBulletType;
    Property NumberStyle : String Read FNumberStyle Write FNumberStyle;
    Property NumberFormat : String Read FNumberFormat Write FNumberFormat;
    Property StartValue: String Read FStartValue Write FStartValue;
  End;

  {***************************************************************************
  * A collection of List level format
  ***************************************************************************}
  TOdtListFormat = Class(TFslObjectList)
  Private
    FName : String;

    Function GetLevelFormat(iIndex: Integer): TOdtListLevelFormat;
  Protected
    Function ItemClass : TFslObjectClass; Override;

    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Overload; Override;
    constructor Create(Const oParent: TOdtListFormat); Overload;
    constructor Create(Const sName: String; Const oFormat: TWPSParagraphDetails); Overload;
    destructor Destroy; Override;

    Function Link: TOdtListFormat;

    Procedure ApplyStyle(oParaFormat: TWPSParagraphDetails; Const iLevel, iItemCount: Integer);
    Function Matches(Const oFormat: TWPSParagraphDetails; Const iCount: Integer): Boolean; Overload; Virtual;

    // raw properties
    Property Name : String Read FName Write FName;
    Property LevelFormat[iIndex : Integer] : TOdtListLevelFormat Read GetLevelFormat; Default;
  End;


  {***************************************************************************
  * This represent a table-wide format
  ***************************************************************************}

  TOdtTableFormat = Class(TFslObject)
  Private
    FMarginLeft, FMarginRight: String;
    FMarginTop, FMarginBottom: String;
    FBackgroundColor: TColour;
  Protected
    Function GetMarginLeft: Integer; Overload; Virtual;
    Procedure SetMarginLeft(Const aValue: Integer); Overload; Virtual;

    Function GetMarginRight: Integer; Overload; Virtual;
    Procedure SetMarginRight(Const aValue: Integer); Overload; Virtual;

    Function GetMarginBottom: Integer; Overload; Virtual;
    Procedure SetMarginBottom(Const aValue: Integer); Overload; Virtual;

    Function GetMarginTop: Integer; Overload; Virtual;
    Procedure SetMarginTop(Const aValue: Integer); Overload; Virtual;

    Function GetHorizontalMargin: Integer; Overload; Virtual;
    Procedure SetHorizontalMargin(Const aValue: Integer); Overload; Virtual;

    Function GetVerticalMargin: Integer; Overload; Virtual;
    Procedure SetVerticalMargin(Const aValue: Integer); Overload; Virtual;

    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Overload; Override;
    constructor Create(Const oFormat: TOdtTableFormat); Overload;
    constructor Create(Const oTable: TWPWorkingDocumentTableStartPiece); Overload;
    destructor Destroy; Override;

    Function Link: TOdtTableFormat;

    Procedure ApplyFormat(oTable: TWPWorkingDocumentTableStartPiece); Overload;
    Function Matches(Const oTable: TWPWorkingDocumentTableStartPiece): Boolean; Overload;

    // WP properties
    Property WPHorizontalMargin : Integer Read GetHorizontalMargin Write SetHorizontalMargin;
    Property WPVerticalMargin : Integer Read GetVerticalMargin Write SetVerticalMargin;
    Property WPMarginLeft: Integer Read GetMarginLeft Write SetMarginLeft;
    Property WPMarginRight: Integer Read GetMarginRight Write SetMarginRight;
    Property WPMarginTop: Integer Read GetMarginTop Write SetMarginTop;
    Property WPMarginBottom: Integer Read GetMarginBottom Write SetMarginBottom;

    // raw properties
    Property MarginLeft: String Read FMarginLeft Write FMarginLeft;
    Property MarginRight: String Read FMarginRight Write FMarginRight;
    Property MarginBottom: String Read FMarginBottom Write FMarginBottom;
    Property MarginTop: String Read FMarginTop Write FMarginTop;
    Property BackgroundColor: TColour Read FBackgroundColor Write FBackgroundColor;
  End;

  {***************************************************************************
  * This represent a table row-wide format
  ***************************************************************************}

  TOdtTableRowFormat = Class(TFslObject)
  Private
    FBackgroundColor: TColour;
  Public
    constructor Create; Overload; Override;
    constructor Create(Const oFormat: TOdtTableRowFormat); Overload;
    constructor Create(Const oRow: TWPWorkingDocumentTableRowStartPiece); Overload;
    destructor Destroy; Override;

    Function Link: TOdtTableRowFormat;

    Procedure ApplyFormat(oRow: TWPWorkingDocumentTableRowStartPiece); Overload;
    Function Matches(Const oRow: TWPWorkingDocumentTableRowStartPiece): Boolean; Overload;

    // raw properties
    Property BackgroundColor: TColour Read FBackgroundColor Write FBackgroundColor;
  End;

  {***************************************************************************
  * This represent a table cell-wide format
  ***************************************************************************}

  TOdtTableCellFormat = Class(TFslObject)
  Private
    FBackgroundColor: TColour;

    // See section 15.5.25: fo:border, fo:border-top, fo:border-bottom, fo:border-left and fo:border-right
    FBorderTop, FBorderBottom: String;
    FBorderLeft, FBorderRight: String;
  Protected
    Function GetBorderTop: TWPBorder; Virtual;
    Procedure SetBorderTop(oBorder: TWPBorder); Virtual;

    Function GetBorderBottom: TWPBorder; Virtual;
    Procedure SetBorderBottom(oBorder: TWPBorder); Virtual;

    Function GetBorderLeft: TWPBorder; Virtual;
    Procedure SetBorderLeft(oBorder: TWPBorder); Virtual;

    Function GetBorderRight: TWPBorder; Virtual;
    Procedure SetBorderRight(oBorder: TWPBorder); Virtual;

    // WP properties
    Property WPBorderTop: TWPBorder Read GetBorderTop Write SetBorderTop;
    Property WPBorderBottom: TWPBorder Read GetBorderBottom Write SetBorderBottom;
    Property WPBorderLeft: TWPBorder Read GetBorderLeft Write SetBorderLeft;
    Property WPBorderRight: TWPBorder Read GetBorderRight Write SetBorderRight;

    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Overload; Override;
    constructor Create(Const oFormat: TOdtTableCellFormat); Overload;
    constructor Create(Const oCell: TWPWorkingDocumentTableCellStartPiece); Overload;
    destructor Destroy; Override;

    Function Link: TOdtTableCellFormat;

    Procedure ApplyFormat(oCell: TWPWorkingDocumentTableCellStartPiece); Overload;
    Function Matches(Const oCell: TWPWorkingDocumentTableCellStartPiece): Boolean; Overload;

    // raw properties
    Property BorderTop: String Read FBorderTop Write FBorderTop;
    Property BorderBottom: String Read FBorderBottom Write FBorderBottom;
    Property BorderLeft: String Read FBorderLeft Write FBorderLeft;
    Property BorderRight: String Read FBorderRight Write FBorderRight;
    Property BackgroundColor: TColour Read FBackgroundColor Write FBackgroundColor;
  End;

  {***************************************************************************
  * This represent a style
  ***************************************************************************}
  TOdtStyle = Class(TFslObject)
  Private
    FName: String;
    FFamily: TOdtStyleFamily;
    FStyleType: TOdtStyleType;

    FTextFormat: TOdtTextFormat;
    FParagraphFormat: TOdtParagraphFormat;
    FListFormat: TOdtListFormat;
    FTableFormat: TOdtTableFormat;
    FRowFormat : TOdtTableRowFormat;
    FCellFormat : TOdtTableCellFormat;

    Function GetTextFormat: TOdtTextFormat;
    Procedure SetTextFormat(oFormat: TOdtTextFormat);
    Function GetParaFormat: TOdtParagraphFormat;
    Procedure SetParaFormat(oFormat: TOdtParagraphFormat);
    Function GetListFormat: TOdtListFormat;
    Procedure SetListFormat(oFormat: TOdtListFormat);

    Function GetTableFormat: TOdtTableFormat;
    Procedure SetTableFormat(oFormat: TOdtTableFormat);
    Function GetRowFormat: TOdtTableRowFormat;
    Procedure SetRowFormat(oFormat: TOdtTableRowFormat);
    Function GetCellFormat: TOdtTableCellFormat;
    Procedure SetCellFormat(oFormat: TOdtTableCellFormat);

  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Overload; Override;
    constructor Create(Const oParent: TOdtStyle); Overload;
    destructor Destroy; Override;

    Function Link: TOdtStyle;

    Function MatchesParaFormat(Const oFormat: TWPSParagraphDetails): Boolean; Overload; Virtual;
    Function MatchesListFormat(Const oFormat: TWPSParagraphDetails; Const iCount: Integer): Boolean; Overload; Virtual;
    Function MatchesTextFormat(Const oFormat: TWPSFontDetails; Const bIsStrict : Boolean = True): Boolean; Overload;
    Function MatchesTableFormat(Const oTable: TWPWorkingDocumentTableStartPiece): Boolean; Overload;
    Function MatchesTableRowFormat(Const oRow: TWPWorkingDocumentTableRowStartPiece): Boolean; Overload;
    Function MatchesTableCellFormat(Const oCell: TWPWorkingDocumentTableCellStartPiece): Boolean; Overload;

    Function HasTextFormat: Boolean;
    Function HasParagraphFormat: Boolean;
    Function HasListFormat: Boolean;
    Function HasTableFormat: Boolean;
    Function HasRowFormat: Boolean;
    Function HasCellFormat: Boolean;

    Property Name: String Read FName Write FName;
    Property Family: TOdtStyleFamily Read FFamily Write FFamily;
    Property StyleType: TOdtStyleType Read FStyleType Write FStyleType;

    Property TextFormat: TOdtTextFormat Read GetTextFormat Write SetTextFormat;
    Property ParagraphFormat: TOdtParagraphFormat Read GetParaFormat Write SetParaFormat;
    Property ListFormat: TOdtListFormat Read GetListFormat Write SetListFormat;
    Property TableFormat: TOdtTableFormat Read GetTableFormat Write SetTableFormat;
    Property RowFormat: TOdtTableRowFormat Read GetRowFormat Write SetRowFormat;
    Property CellFormat: TOdtTableCellFormat Read GetCellFormat Write SetCellFormat;
  End;

  {***************************************************************************
  * This represent a collection of styles
  ***************************************************************************}
  TOdtStyleList = Class(TFslObjectList)
  Private
      Function GetStyle(iIndex : Integer) : TOdtStyle;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Function Link: TOdtStyleList;

      Property Styles[iIndex : Integer] : TOdtStyle Read GetStyle; Default;
  End;

  {***************************************************************************
  * This represent a stack (LIFO List) of styles
  ***************************************************************************}
  TOdtStyleStack = Class(TOdtStyleList)
    Public
      Function Empty : Boolean;

      Function Peek : TOdtStyle;

      Procedure Push(oStyle : TOdtStyle);
      Procedure Pop;
  End;


// Support Functions
// TODO: Refactor into a separated units
Function LengthCSS2ToPixel(Const sValue: String; Const iDefault: Integer = 0): Integer;
Function LengthPixelToCSS2(Const iValue: Integer; Const sDefault: String = ''): String;
Function BorderCSS2ToWPBorder(Const sBorder: String): TWPBorder;
Function WPBorderToBorderCSS2(Const oBorder: TWPBorder): String;
Function SimpleTriStateMatches(Const aVal, aVal2 : TWPSTriState): Boolean;
Function SimpleFontStateMatches(Const aVal, aVal2 : TWPSFontState): Boolean;
Function SimpleCapsStateMatches(Const aVal, aVal2 : TWPSCapsState): Boolean;



Type
  {
      <choice>
        <ref name="text-h"/>
        <ref name="text-p"/>
        <ref name="text-list"/>
        <ref name="text-numbered-paragraph"/>
        <ref name="table-table"/>
        <ref name="draw-a"/>
        <ref name="text-section"/>
        <ref name="text-soft-page-break"/>      // we don't care about this
        <ref name="text-table-of-content"/>
        <ref name="text-illustration-index"/>
        <ref name="text-table-index"/>
        <ref name="text-object-index"/>
        <ref name="text-user-index"/>
        <ref name="text-alphabetical-index"/>
        <ref name="text-bibliography"/>
        <ref name="shape"/>
        <ref name="change-marks"/>
      </choice>
  }
  TWPOdtReaderItemType = (TWPOdtReaderHeaderType, TWPOdtReaderParaType,
        TWPOdtReaderNumberedParaType, TWPOdtReaderListType,
        TWPOdtReaderTableType, TWPOdtReaderLinkType);

  TWPOdtReaderItemTypes = Set Of TWPOdtReaderItemType;

Const
  ALL_ODTREADERITEMTYPES = [TWPOdtReaderHeaderType..TWPOdtReaderLinkType];

  NAMES_ODTREADERITEMS : Array [TWPOdtReaderItemType] Of String = ('h', 'p',
        'p', 'list', 'table', 'a');
  NAMESPACE_ODTREADERITEMS : Array [TWPOdtReaderItemType] Of String = (TEXT_NS, TEXT_NS,
        TEXT_NS, TEXT_NS, TABLE_NS, DRAWING_NS);

Type

  TWPOdtReader = Class (TWPReader)
  Private
    FOdtAdapter: TOdtReaderAdapter;

    // raw style information
    FFontDecls: TOdtFontDeclarations;
    FDefaultStyles: TOdtStyleList;
    FStyles: TOdtStyleList;

    FStyleStack: TOdtStyleStack;

    // keeping track of list
    FCurrentListStyle: TOdtStyle;
    FCurrentListLevel: Integer;

    Procedure AddTexts(oDocument: TWPWorkingDocument; oReader: TFslXMLExtractor);
    Procedure AddPageBreak(oDocument: TWPWorkingDocument; oStyle: TOdtStyle);
    Procedure ApplyListStyle(oParaFormat: TWPSParagraphDetails; Const iItemCount: Integer);

    // TODO: Refactor this to put it into WPReaders
    Function ReadEnumeratedAttribute(Const sValue: String; Const aValues: Array Of String; Const aDefault : Byte): Byte;

  Protected
    Procedure ResetReaderData; Overload; Virtual;

    Function GetStyleByName(Const sStyleName: String): TOdtStyle; Overload; Virtual;
    Function GetStyleForFamily(Const aFamily: TOdtStyleFamily; Const sStyleName: String): TOdtStyle; Overload; Virtual;

    // read sub-document
    Procedure ReadSubDocStyles(oAdapter: TOdtReaderAdapter); Virtual;
    Procedure ReadSubDocMeta(oAdapter: TOdtReaderAdapter); Virtual;
    Procedure ReadSubDocSettings(oAdapter: TOdtReaderAdapter); Virtual;
    Procedure ReadSubDocContent(oDocument : TWPWorkingDocument; oAdapter: TOdtReaderAdapter); Virtual;

    // Method to read different data from the document
    Procedure ReadMetadata(oReader: TFslXMLExtractor); Virtual;
    Procedure ReadApplSettings(oReader: TFslXMLExtractor); Virtual;
    Procedure ReadScripts(oReader: TFslXMLExtractor); Virtual;
    Procedure ReadFontDeclarations(oReader: TFslXMLExtractor); Virtual;
    Procedure ReadStyles(oReader: TFslXMLExtractor); Virtual;
    Procedure ReadAutoStyles(oReader: TFslXMLExtractor); Virtual;
    Procedure ReadMasterStyles(oReader: TFslXMLExtractor); Virtual;
    Procedure ReadBody(oDocument : TWPWorkingDocument; oReader: TFslXMLExtractor); Virtual;

    // read text document body
    Procedure ReadTextBody(oDocument : TWPWorkingDocument; oReader: TFslXMLExtractor); Virtual;
    Function ReadNextTextContent(oDocument : TWPWorkingDocument; oReader: TFslXMLExtractor;
                aAllowedTypes: TWPOdtReaderItemTypes; bIsStrict : Boolean = False) : Integer; Virtual;
    // read table
    Function ReadTable(oDocument : TWPWorkingDocument; oReader: TFslXMLExtractor) : Integer; Virtual;
    Procedure ReadTableHeaderRow(oDocument : TWPWorkingDocument; oReader: TFslXMLExtractor); Virtual;
    Function ReadTableRow(oDocument : TWPWorkingDocument; oReader: TFslXMLExtractor): Integer; Virtual;
    Function ReadTableCell(oDocument : TWPWorkingDocument; oReader: TFslXMLExtractor): Integer; Virtual;
    // read text list
    Function ReadList(oDocument : TWPWorkingDocument; oReader: TFslXMLExtractor) : Integer; Virtual;
    Function ReadListItem(oDocument : TWPWorkingDocument; oReader: TFslXMLExtractor; Const iListItemCount: Integer): Integer; Virtual;
    // read text paragraph
    Function ReadParagraph(oDocument : TWPWorkingDocument; oReader: TFslXMLExtractor) : Integer; Virtual;
    Procedure ReadParagraphContent(oDocument : TWPWorkingDocument; oReader: TFslXMLExtractor); Virtual;
    Function ReadLink(oDocument:TWPWorkingDocument; oReader: TFslXMLExtractor) : Integer; Virtual;
    Procedure ReadLineBreak(oDocument: TWPWorkingDocument; oReader: TFslXMLExtractor); Virtual;

    // read styles
    Function ReadOdtListStyle(oReader: TFslXMLExtractor): TOdtStyle; Overload; Virtual;
    Function ReadOdtStyle(oReader: TFslXMLExtractor; bCanHaveParent:Boolean = False): TOdtStyle; Overload; Virtual;
    Procedure ReadOdtStyleTextProperties(oTextFormat: TOdtTextFormat; oReader: TFslXMLExtractor); Overload;
    Procedure ReadOdtStyleParagraphProperties(oParaFormat: TOdtParagraphFormat; oReader: TFslXMLExtractor); Overload;
    Procedure ReadOdtStyleTableProperties(oFormat: TOdtTableFormat; oReader:TFslXMLExtractor); Overload;
    Procedure ReadOdtStyleRowProperties(oFormat: TOdtTableRowFormat; oReader:TFslXMLExtractor); Overload;
    Procedure ReadOdtStyleCellProperties(oFormat: TOdtTableCellFormat; oReader:TFslXMLExtractor); Overload;

    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Procedure Read(oDocument : TWPWorkingDocument); Overload; Override;

  End;


Type

  TWPOdtWriter = Class (TWPWriter)
    Private
      FOdtAdapter: TOdtWriterAdapter;
      FInSpan: Boolean;
      FInList: Boolean;
      FListNumber: Integer;

      // content formatter
      FFormatter : TFslXMLFormatter;

      FFontDecls: TOdtFontDeclarations;
      FDefaultStyles: TOdtStyleList;
      FStyles: TOdtStyleList;

      FStyleStack: TOdtStyleStack;

    Protected
      Procedure Initialise; Override;
      Procedure Finalise; Override;

      Procedure IterateParagraph(oIterator : TWPPieceIterator; oSection : TWPWorkingDocumentSectionStartPiece); Override;

      Procedure WriteText(oText : TWPWorkingDocumentTextPiece); Override;
//      Procedure WriteImage(oImage : TWPWorkingDocumentImagePiece); Override;
      Procedure WriteLineBreak(oBreak : TWPWorkingDocumentLineBreakPiece); Override;
//      Procedure WriteFieldStart(oField : TWPWorkingDocumentFieldStartPiece); Override;
//      Procedure WriteFieldStop(oField : TWPWorkingDocumentFieldStartPiece; oStop : TWPWorkingDocumentFieldStopPiece); Override;
      Procedure WriteParagraphStart(oParagraph : TWPWorkingDocumentParaPiece); Override;
      Procedure WriteParagraphStop(oParagraph : TWPWorkingDocumentParaPiece; bNextIsSection : Boolean; oSection : TWPWorkingDocumentSectionStartPiece); Override;
//      Procedure WriteBreak(oBreak : TWPWorkingDocumentBreakPiece); Override;
      Procedure WriteTableStart(oTable : TWPWorkingDocumentTableStartPiece); Override;
      Procedure WriteTableStop(oTable : TWPWorkingDocumentTableStartPiece; oStop : TWPWorkingDocumentStopPiece); Override; // note that start is passed, not stop, since stop carries nothing
      Procedure WriteTableRowStart(oTableRow : TWPWorkingDocumentTableRowStartPiece); Override;
      Procedure WriteTableRowStop(oTableRow : TWPWorkingDocumentTableRowStartPiece; oStop : TWPWorkingDocumentStopPiece; bIsLast : Boolean); Override; // note that start is passed, not stop, since stop carries nothing
      Procedure WriteTableCellStart(oTableCell : TWPWorkingDocumentTableCellStartPiece); Override;
      Procedure WriteTableCellStop(oTableCell : TWPWorkingDocumentTableCellStartPiece; oStop : TWPWorkingDocumentStopPiece); Override; // note that start is passed, not stop, since stop carries nothing
//      Procedure WriteSectionStart(oSection : TWPWorkingDocumentSectionStartPiece); Override;
//      Procedure WriteSectionStop(oSection : TWPWorkingDocumentSectionStartPiece; oStop : TWPWorkingDocumentStopPiece); Override; // note that start is passed, not stop, since stop carries nothing
      Procedure WriteDocumentStart(oDocument : TWPWorkingDocument); Override;
      Procedure WriteDocumentStop(oDocument : TWPWorkingDocument); Override;

      Procedure OpenSpan(oStyle: TOdtStyle);
      Procedure CloseSpan;
      Procedure OpenList(oStyle: TOdtStyle);
      Procedure CloseList;
      Procedure OpenListItem;
      Procedure CloseListItem;

      Function GetListStyle(Const oPara: TWPWorkingDocumentParaPiece): TOdtStyle;
      Function GetParaStyle(Const oPara: TWPWorkingDocumentParaPiece): TOdtStyle;
      Function GetTextStyle(Const oText: TWPWorkingDocumentTextPiece): TOdtStyle;
      Function GetTableStyle(Const oTable: TWPWorkingDocumentTableStartPiece): TOdtStyle;
      Function GetTableRowStyle(Const oRow: TWPWorkingDocumentTableRowStartPiece): TOdtStyle;
      Function GetTableCellStyle(Const oCell: TWPWorkingDocumentTableCellStartPiece): TOdtStyle;

      Procedure DeclareOdtNamespaces(oFormatter: TFslXMLFormatter);

      Procedure WriteStyleSubDoc(oFormatter: TFslXMLFormatter);
      Procedure WriteFontDeclarations(oFormatter: TFslXMLFormatter);
      Procedure WriteStyles(oFormatter: TFslXMLFormatter);
      Procedure WriteOdtStyle(oFormatter: TFslXMLFormatter; oStyle: TOdtStyle; bIsDefaultStyle: Boolean = False);
      Procedure WriteTextFormat(oFormatter: TFslXMLFormatter; oTextFormat: TOdtTextFormat);
      Procedure WriteParaFormat(oFormatter: TFslXMLFormatter; oParaFormat: TOdtParagraphFormat);
      Procedure WriteListFormat(oFormatter: TFslXMLFormatter; oListFormat: TOdtListFormat);
      Procedure WriteTableFormat(oFormatter: TFslXMLFormatter; oTableFormat: TOdtTableFormat);
      Procedure WriteTableRowFormat(oFormatter: TFslXMLFormatter; oRowFormat: TOdtTableRowFormat);
      Procedure WriteTableCellFormat(oFormatter: TFslXMLFormatter; oCellFormat: TOdtTableCellFormat);

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TWPOdtWriter; Overload;

      Procedure Write(oDocument : TWPWorkingDocument); Override;
  End;



Implementation

{ TWPOdtReader }

Constructor TWPOdtReader.Create;
Begin
  Inherited Create;

  FOdtAdapter := TOdtReaderAdapter.Create;

  FFontDecls := TOdtFontDeclarations.Create;
  FDefaultStyles := TOdtStyleList.Create;
  FStyles := TOdtStyleList.Create;

  FStyleStack := TOdtStyleStack.Create;
End;


Destructor TWPOdtReader.Destroy;
Begin
  FFontDecls.Free;
  FDefaultStyles.Free;
  FStyles.Free;

  FStyleStack.Free;

  FOdtAdapter.Free;

  Inherited;
End;


Procedure TWPOdtReader.ReadSubDocContent(oDocument : TWPWorkingDocument; oAdapter: TOdtReaderAdapter);
Var
  oReader: TFslXMLExtractor;
  sTag, sLocalName: String;
Begin
  oReader := FOdtAdapter.GetSubDoc(OpenDocumentSubDocContent);
  Assert(oReader <> Nil, 'Unable to read data from sub-doc "content".');
  Try
    oReader.Skip([TFslXMLKnownHeaderType, TFslXMLKnownCommentType]);
    If Not oReader.IsNode(OFFICE_NS, 'document-content') Then
      raise EWPException.create('Expect "document-content" in namespace ' + OFFICE_NS);

    If oReader.PeekIsOpen Then
    Begin
      sTag := oReader.ConsumeOpen;
      While Not oReader.PeekIsClose Do
      Begin
        If Not oReader.PeekIsOpen Then
          oReader.SkipNext
        Else
        Begin
          sLocalName := oReader.NodeLocalName;
          If StringStartsWith(sLocalName, SCRIPT_GROUPNAME) Then
            Self.ReadScripts(oReader)
          Else If StringStartsWith(sLocalName, FONT_DECLARATION_GROUPNAME)  Then
            Self.ReadFontDeclarations(oReader)
          Else If StringStartsWith(sLocalName, AUTOMATIC_STYLE_GROUPNAME) Then
            Self.ReadAutoStyles(oReader)
          Else If StringStartsWith(sLocalName, BODY_ELENAME) Then
            Self.ReadBody(oDocument, oReader);
        End;
      End;
      oReader.ConsumeClose(sTag);
    End
    Else
      oReader.SkipNext;
  Finally
    oReader.Free;
  End;
End;


Procedure TWPOdtReader.ReadSubDocMeta(oAdapter: TOdtReaderAdapter);
Var
  oReader: TFslXMLExtractor;
Begin
  oReader := FOdtAdapter.GetSubDoc(OpenDocumentSubDocMeta);
  If oReader <> Nil Then
  Begin
  Try
    oReader.SkipNext;
  Finally
    oReader.Free;
  End;
  End;
End;


Procedure TWPOdtReader.ReadSubDocSettings(oAdapter: TOdtReaderAdapter);
Var
  oReader: TFslXMLExtractor;
Begin
  oReader := FOdtAdapter.GetSubDoc(OpenDocumentSubDocSettings);
  If oReader <> Nil Then
  Begin
  Try
    oReader.SkipNext;
  Finally
    oReader.Free;
  End;
  End;
End;


Procedure TWPOdtReader.ReadSubDocStyles(oAdapter: TOdtReaderAdapter);
Var
  oReader: TFslXMLExtractor;
Begin
  oReader := FOdtAdapter.GetSubDoc(OpenDocumentSubDocStyle);
  If oReader <> Nil Then
  Begin
  Try
    If oReader.PeekIsEmptyNode Then
      oReader.SkipNext   // don't need to do anything
    Else
    Begin
      oReader.Skip([TFslXMLKnownHeaderType, TFslXMLKnownCommentType]);
      While oReader.PeekIsOpen Do
      Begin
        If oReader.IsNode(OFFICE_NS, FONT_DECLARATION_GROUPNAME) Then
          Self.ReadFontDeclarations(oReader)
        Else If oReader.IsNode(OFFICE_NS, STYLE_GROUPNAME) Then
          Self.ReadStyles(oReader)
        Else
          oReader.SkipNext;
      End;
    End;
  Finally
    oReader.Free;
  End;
  End;
End;


Procedure TWPOdtReader.ResetReaderData;
Begin
  FFontDecls.Clear;

  FDefaultStyles.Clear;
  FStyles.Clear;
  FStyleStack.Clear;

  FCurrentListStyle := Nil;
  FCurrentListLevel := -1;
End;


Procedure TWPOdtReader.Read(oDocument : TWPWorkingDocument);
Begin
  FOdtAdapter.Load(Stream);

  ResetReaderData;
  ReadSubDocMeta(FOdtAdapter);
  ReadSubDocSettings(FOdtAdapter);
  ReadSubDocStyles(FOdtAdapter);
  ReadSubDocContent(oDocument, FOdtAdapter);

  // finish reading
  CheckForEmpty(oDocument);
  DoneReading(oDocument);
End;

Procedure TWPOdtReader.ReadMasterStyles(oReader: TFslXMLExtractor);
Begin
  oReader.SkipNext;
End;

Procedure TWPOdtReader.ReadMetadata(oReader: TFslXMLExtractor);
Begin
  oReader.SkipNext;
End;

Procedure TWPOdtReader.ReadScripts(oReader: TFslXMLExtractor);
Begin
  oReader.SkipNext;
End;

Procedure TWPOdtReader.ReadApplSettings(oReader: TFslXMLExtractor);
Begin
  oReader.SkipNext;
End;


Procedure TWPOdtReader.ReadStyles(oReader: TFslXMLExtractor);
Var
  oStyle: TOdtStyle;
  bIsCommonStyle: Boolean;
Begin
  {Include both default styles and common styles}
  If oReader.PeekIsEmptyNode Then
    oReader.SkipNext    // don't need to do anything
  Else
  Begin
    oReader.ConsumeOpen;
    While oReader.PeekIsOpen Do
    Begin
      If oReader.IsNode(STYLE_NS, 'style')
        Or oReader.IsNode(STYLE_NS, 'default-style') Then
      Begin
        bIsCommonStyle := StringEquals(oReader.NodeLocalName, 'style');
        oStyle := ReadOdtStyle(oReader);
        Try
          If bIsCommonStyle Then
          Begin
            oStyle.StyleType := ostCommon;
            FStyles.Add(oStyle.Link);
          End
          Else
          Begin
            oStyle.StyleType := ostDefault;
            FDefaultStyles.Add(oStyle.Link);
          End;
        Finally
          oStyle.Free;
        End;
      End
      Else If oReader.IsNode(TEXT_NS, 'list-style') Then
      Begin
        oStyle := ReadOdtListStyle(oReader);
        Try
          oStyle.StyleType := ostCommon;
          FStyles.Add(oStyle.Link);
        Finally
          oStyle.Free;
        End;
      End
      Else
        oReader.SkipNext;
    End;
    oReader.ConsumeClose;
  End;
End;


Procedure TWPOdtReader.ReadAutoStyles(oReader: TFslXMLExtractor);
Var
  oStyle: TOdtStyle;
Begin
  { Automatic styles are styles that are assigned directly to an text element }
  If oReader.PeekIsEmptyNode Then
    oReader.SkipNext    // don't need to do anything
  Else
  Begin
    oReader.ConsumeOpen;
    While oReader.PeekIsOpen Do
    Begin
      If oReader.IsNode(STYLE_NS, 'style') Then
      Begin
        oStyle := ReadOdtStyle(oReader);
        Try
          oStyle.StyleType := ostAutomatic;
          FStyles.Add(oStyle.Link);
        Finally
          oStyle.Free;
        End;
      End
      Else If oReader.IsNode(TEXT_NS, 'list-style') Then
      Begin
        oStyle := ReadOdtListStyle(oReader);
        Try
          oStyle.StyleType := ostAutomatic;
          FStyles.Add(oStyle.Link);
        Finally
          oStyle.Free;
        End;
      End
      Else
       oReader.SkipNext;
    End;
    oReader.ConsumeClose;
  End;
End;


Procedure TWPOdtReader.ReadBody(oDocument : TWPWorkingDocument; oReader: TFslXMLExtractor);
Begin
  // Loop through the body and read it
  If oReader.PeekIsEmptyNode Then
    oReader.SkipNext    // don't need to do anything
  Else
  Begin
    oReader.ConsumeOpen;
    While oReader.PeekIsOpen Do
    Begin
      // we only want text content for now
      If oReader.IsNode(OFFICE_NS, 'text') Then
        Self.ReadTextBody(oDocument, oReader)
      Else
        oReader.SkipNext;
    End;
    oReader.ConsumeClose;
  End;
End;


Procedure TWPOdtReader.ReadTextBody(oDocument : TWPWorkingDocument; oReader: TFslXMLExtractor);
Begin
  { Read the body of text document }
  If oReader.PeekIsEmptyNode Then
    oReader.SkipNext    // don't need to do anything
  Else
  Begin
    oReader.ConsumeOpen;
    While oReader.PeekIsOpen Do
      ReadNextTextContent(oDocument, oReader, ALL_ODTREADERITEMTYPES);
    oReader.ConsumeClose;
  End;
End;


Function TWPOdtReader.ReadNextTextContent(oDocument: TWPWorkingDocument;
                oReader: TFslXMLExtractor; aAllowedTypes: TWPOdtReaderItemTypes;
                bIsStrict: Boolean) : Integer;
Var
  sTag : String;
  iIndex : Integer;
  aType : TWPOdtReaderItemType;
  bDone : Boolean;
Begin
  Result := -1;
  bDone := False;
  sTag := oReader.NodeLocalName;
  iIndex := StringArrayIndexOfSensitive(NAMES_ODTREADERITEMS, sTag);

  If (iIndex >= 0) Then
  Begin
    aType := TWPOdtReaderItemType(iIndex);

    If (aType In aAllowedTypes) And
      (StringEquals(NAMESPACE_ODTREADERITEMS[aType], oReader.NodeNamespace)) Then
    Begin
      bDone := True;
      Case aType Of
      TWPOdtReaderHeaderType,
      TWPOdtReaderParaType,
      TWPOdtReaderNumberedParaType:     Result := ReadParagraph(oDocument, oReader);
      TWPOdtReaderListType:             Result := ReadList(oDocument, oReader);
      TWPOdtReaderTableType:            Result := ReadTable(oDocument, oReader);
      TWPOdtReaderLinkType:             Result := ReadLink(oDocument, oReader);
      Else
        bDone := False;
      End;
    End;
  End;

  If Not bDone Then
  Begin
    If bIsStrict Then
      RaiseError('ReadContent', 'Unexpected content - found ' + sTag)
    Else
      oReader.SkipNext;  // ignore unknown node
  End;
End;



Function TWPOdtReader.ReadTable(oDocument: TWPWorkingDocument; oReader: TFslXMLExtractor): Integer;
Var
  oStart : TWPWorkingDocumentTableStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
  oStyle: TOdtStyle;
Begin
  { Read the table }
  Result := -1;
  If oReader.PeekIsEmptyNode Then
    oReader.SkipNext    // don't need to do anything
  Else
  Begin
    oStart := TWPWorkingDocumentTableStartPiece.Create;
    Try
      oStyle := GetStyleByName(oReader.GetAttribute(TABLE_NS, 'style-name'));
      If (oStyle <> Nil) And (oStyle.HasTableFormat) Then
        oStyle.TableFormat.ApplyFormat(oStart);
      Result := oDocument.Pieces.Add(oStart.Link);
    Finally
      oStart.Free;
    End;
    oReader.ConsumeOpen;

    // read table rows
    While oReader.PeekIsOpen Do
    Begin
      If oReader.IsNode(TABLE_NS, 'table-row') Then
        ReadTableRow(oDocument, oReader)
      Else If oReader.IsNode(TABLE_NS, 'table-header-rows') Then
        ReadTableHeaderRow(oDocument, oReader)
      Else
        oReader.SkipNext;
    End;

    oStop := TWPWorkingDocumentStopPiece.Create(stTable);
    Try
      oDocument.Pieces.Add(oStop.Link);
    Finally
      oStop.Free;
    End;
    oReader.ConsumeClose;
  End;
End;


Procedure TWPOdtReader.ReadTableHeaderRow(oDocument: TWPWorkingDocument; oReader: TFslXMLExtractor);
Var
  iIndex: Integer;
  oStart : TWPWorkingDocumentTableRowStartPiece;
Begin
  If oReader.PeekIsEmptyNode Then
    oReader.SkipNext    // don't need to do anything
  Else
  Begin
    oReader.ConsumeOpen;
    While oReader.PeekIsOpen Do
    Begin
      If oReader.IsNode(TABLE_NS, 'table-row') Then
      Begin
        iIndex := ReadTableRow(oDocument, oReader);
        If (iIndex >= 0) And (oDocument.Pieces[iIndex] Is TWPWorkingDocumentTableRowStartPiece) Then
        Begin
          oStart := TWPWorkingDocumentTableRowStartPiece(oDocument.Pieces[iIndex]);
          oStart.Header := True;
        End;
      End
      Else
        oReader.SkipNext;
    End;

    oReader.ConsumeClose;
  End;
End;


Function TWPOdtReader.ReadTableRow(oDocument: TWPWorkingDocument; oReader: TFslXMLExtractor): Integer;
Var
  oStart : TWPWorkingDocumentTableRowStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
  oStyle: TOdtStyle;
Begin
  Result := -1;
  { Read the table row }
  If oReader.PeekIsEmptyNode Then
    oReader.SkipNext    // don't need to do anything
  Else
  Begin
    oStart := TWPWorkingDocumentTableRowStartPiece.Create;
    Try
      oStyle := GetStyleByName(oReader.GetAttribute(TABLE_NS, 'style-name'));
      If (oStyle <> Nil) And (oStyle.HasRowFormat) Then
        oStyle.RowFormat.ApplyFormat(oStart);
      Result := oDocument.Pieces.Add(oStart.Link);
    Finally
      oStart.Free;
    End;
    oReader.ConsumeOpen;

    // read table cells
    While oReader.PeekIsOpen Do
    Begin
      If oReader.IsNode(TABLE_NS, 'table-cell') Then
        ReadTableCell(oDocument, oReader)
      Else
        oReader.SkipNext;
    End;

    oStop := TWPWorkingDocumentStopPiece.Create(stTableRow);
    Try
      oDocument.Pieces.Add(oStop.Link);
    Finally
      oStop.Free;
    End;
    oReader.ConsumeClose;
  End;
End;


Function TWPOdtReader.ReadTableCell(oDocument: TWPWorkingDocument; oReader: TFslXMLExtractor): Integer;
Var
  oStart : TWPWorkingDocumentTableCellStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
  oStyle: TOdtStyle;
Begin
  Result := -1;
  { Read the table cell }
  If oReader.PeekIsEmptyNode Then
    oReader.SkipNext    // don't need to do anything
  Else
  Begin
    oStart := TWPWorkingDocumentTableCellStartPiece.Create;
    Try
      oStyle := GetStyleByName(oReader.GetAttribute(TABLE_NS, 'style-name'));
      If (oStyle <> Nil) And (oStyle.HasCellFormat) Then
        oStyle.CellFormat.ApplyFormat(oStart);
      Result := oDocument.Pieces.Add(oStart.Link);
    Finally
      oStart.Free;
    End;

    oReader.ConsumeOpen;
    While oReader.PeekIsOpen Do
      ReadNextTextContent(oDocument, oReader, ALL_ODTREADERITEMTYPES);

    oStop := TWPWorkingDocumentStopPiece.Create(stTableCell);
    Try
      oDocument.Pieces.Add(oStop.Link);
    Finally
      oStop.Free;
    End;
    oReader.ConsumeClose;
  End;
End;

Function TWPOdtReader.ReadList(oDocument: TWPWorkingDocument; oReader: TFslXMLExtractor): Integer;
Var
  iListItemCount : Integer;
Begin
  Result := -1;
  If oReader.PeekIsEmptyNode Then
    oReader.ConsumeOpen
  Else
  Begin
    Inc(FCurrentListLevel);
    If FCurrentListStyle = Nil Then
      FCurrentListStyle := Self.GetStyleForFamily(osfParagraph, oReader.GetAttribute(TEXT_NS, 'style-name'));
    Try
      oReader.ConsumeOpen;
      iListItemCount := 0;

      // read list items
      While oReader.PeekIsOpen Do
      Begin
        If oReader.IsNode(TEXT_NS, 'list-item') Then
        Begin
          Result := ReadListItem(oDocument, oReader, iListItemCount);
          Inc(iListItemCount);
        End
        Else
          oReader.SkipNext;
      End;

      oReader.ConsumeClose;
    Finally
      Dec(FCurrentListLevel);
      If FCurrentListLevel < 0 Then
        FCurrentListStyle := Nil;
    End;
  End;
End;


Function TWPOdtReader.ReadListItem(oDocument: TWPWorkingDocument; oReader: TFslXMLExtractor; Const iListItemCount: Integer): Integer;
Var
  bFirstPara : Boolean;
  iIndex : Integer;
  oParagraph: TWPWorkingDocumentParaPiece;
Begin
  Result := -1;
  If oReader.PeekIsEmptyNode Then
    oReader.ConsumeOpen
  Else
  Begin
    oReader.ConsumeOpen;
    bFirstPara := True;

    While oReader.PeekIsOpen Do
    Begin
      iIndex := ReadNextTextContent(oDocument, oReader, [TWPOdtReaderHeaderType,
                TWPOdtReaderParaType, TWPOdtReaderNumberedParaType, TWPOdtReaderListType]);
      If (bFirstPara) Then
      Begin
        bFirstPara := False;
        Result := iIndex;

        If (oDocument.Pieces[iIndex] Is TWPWorkingDocumentParaPiece) Then
        Begin
          oParagraph := TWPWorkingDocumentParaPiece(oDocument.Pieces[iIndex]);
          // FIXME: bit of a hack, to avoid apply style to list within list
          If oParagraph.Format.ListType In [WPSParagraphListTypeUnknown, WPSParagraphListTypeNone] Then
            ApplyListStyle(oParagraph.Format, iListItemCount);
        End;
      End;
      // FIXME: what do do if first element of item list is not a paragraph ?
    End;

    oReader.ConsumeClose;
  End;
End;


Function TWPOdtReader.ReadParagraph(oDocument : TWPWorkingDocument; oReader: TFslXMLExtractor): Integer;
Var
  oParagraph: TWPWorkingDocumentParaPiece;
  oStyle: TOdtStyle;
  bHasStyle: Boolean;
  oParaFormat: TOdtParagraphFormat;
Begin
  Result := -1;
  { Start a paragraph }
  If oReader.PeekIsEmptyNode Then
    oReader.ConsumeOpen
  Else
  Begin
    oStyle := Self.GetStyleForFamily(osfParagraph, oReader.GetAttribute(TEXT_NS, 'style-name'));
    bHasStyle := oStyle <> Nil;
    If bHasStyle Then
    Begin
      FStyleStack.Push(oStyle.Link);
      If oStyle.HasParagraphFormat And oStyle.ParagraphFormat.IsBreakBefore Then
        AddPageBreak(oDocument, oStyle);
    End;
    oReader.ConsumeOpen;

    oParagraph := TWPWorkingDocumentParaPiece.Create;
    Try
      oParagraph.SpeechMagicDouble := SpeechMagicDouble;
      If (Not FStyleStack.Empty) And FStyleStack.Peek.HasParagraphFormat Then
      Begin
        oParaFormat := FStyleStack.Peek.ParagraphFormat;
        // apply style
        oParagraph.Format.Align := oParaFormat.WPAlignt;
        oParagraph.Format.MarginBottom := oParaFormat.WPMarginBottom;
        oParagraph.Format.LeftIndent := oParaFormat.WPLeftIndent;
        oParagraph.Format.RightIndent := oParaFormat.WPRightIndent;
      End;

      Repeat
        ReadParagraphContent(oDocument, oReader);
      Until oReader.PeekIsClose;

      Result := oDocument.Pieces.Add(oParagraph.Link);
    Finally
      oParagraph.Free;
    End;
    oReader.ConsumeClose;
    If bHasStyle Then
    Begin
      FStyleStack.Pop;
      If oStyle.HasParagraphFormat And oStyle.ParagraphFormat.IsBreakAfter Then
        AddPageBreak(oDocument, oStyle);
    End;
  End;
End;


Procedure TWPOdtReader.ReadParagraphContent(oDocument: TWPWorkingDocument; oReader: TFslXMLExtractor);
Var
  sLocalName: String;
  oStyle: TOdtStyle;
  bHasStyle: Boolean;
  bDone: Boolean;
Begin
  { Read paragraph content (not only styled texts, but also link, etc) }
  bDone := False;
  While (Not bDone) Do
  Begin
    If oReader.PeekIsText Then
      // read text and add them word by word
      AddTexts(oDocument, oReader)
    Else If oReader.PeekIsOpen Then
    Begin
      sLocalName := oReader.NodeLocalName;
      If oReader.IsNode(TEXT_NS, 'span') Then
      Begin
        If oReader.PeekIsEmptyNode Then
          oReader.SkipNext
        Else
        Begin
          oStyle := Self.GetStyleForFamily(osfParagraph, oReader.GetAttribute(TEXT_NS, 'style-name'));
          bHasStyle := oStyle <> Nil;
          If bHasStyle Then
          Begin
            FStyleStack.Push(oStyle.Link);
            If oStyle.HasParagraphFormat And oStyle.ParagraphFormat.IsBreakBefore Then
              AddPageBreak(oDocument, oStyle);
          End;

          // recursive reading
          oReader.ConsumeOpen;
          ReadParagraphContent(oDocument, oReader);
          oReader.ConsumeClose;

          If bHasStyle Then
          Begin
            FStyleStack.Pop;
            If oStyle.HasParagraphFormat And oStyle.ParagraphFormat.IsBreakAfter Then
              AddPageBreak(oDocument, oStyle);
          End;
        End;
      End
      Else If StringEquals(sLocalName, 'line-break') Then
        Self.ReadLineBreak(oDocument, oReader)
      Else
        ReadNextTextContent(oDocument, oReader, [TWPOdtReaderLinkType]);
    End
    Else
      bDone := True;
  End;
End;


Procedure TWPOdtReader.AddTexts(oDocument: TWPWorkingDocument; oReader: TFslXMLExtractor);
Var
  oText : TWPWorkingDocumentTextPiece;
  oFormat : TOdtTextFormat;
Begin
  { Convert text string into WP Text pieces }
  Splitter.Init(oReader.ConsumeBody);
  While Splitter.More Do
  Begin
    oText := TWPWorkingDocumentTextPiece.Create;
    Try
      // Apply style
      If (Not FStyleStack.Empty) And FStyleStack.Peek.HasTextFormat Then
      Begin
        oFormat := FStyleStack.Peek.TextFormat;

        If (oFormat.Name <> '') And FFontDecls.ExistsByKey(oFormat.Name) Then
          oText.Font.Name := FFontDecls.GetValueByKey(oFormat.Name)
        Else If (oFormat.Family <> '') Then
          oText.Font.Name := oFormat.Family;

        oText.Font.Size := oFormat.WPFontSize;
        oText.Font.Foreground := oFormat.WPForeground;
        oText.Font.Background := oFormat.WPBackground;
        oText.Font.Italic := oFormat.WPItalic;
        oText.Font.Bold := oFormat.WPBold;
        oText.Font.State := oFormat.WPFontState;
        oText.Font.Underline := oFormat.WPUnderline;
        oText.Font.Strikethrough := oFormat.WPStrikeThrough;
        oText.Font.Capitalization := oFormat.WPCapitalization;
      End;

      oText.Content := Splitter.Next;
      oDocument.Pieces.Add(oText.Link);
    Finally
      oText.Free;
    End;
  End;
End;


Procedure TWPOdtReader.AddPageBreak(oDocument: TWPWorkingDocument; oStyle: TOdtStyle);
Var
  oPiece: TWPWorkingDocumentBreakPiece;
Begin
  oPiece := TWPWorkingDocumentBreakPiece.Create;
  Try
    oPiece.BreakType := btPageBreak;
    // TODO: apply style to break piece
    oDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;


Function TWPOdtReader.ReadLink(oDocument: TWPWorkingDocument; oReader: TFslXMLExtractor): Integer;
Var
  oStart: TWPWorkingDocumentFieldStartPiece;
  oStop: TWPWorkingDocumentFieldStopPiece;
Begin
  { Read an reference link into a field }
  Result := -1;
  If oReader.PeekIsEmptyNode Then
    oReader.SkipNext
  Else
  Begin
    oStop := TWPWorkingDocumentFieldStopPiece.Create;
    Try
      oStart := TWPWorkingDocumentFieldStartPiece.Create;
      Try
        oReader.ConsumeOpen;
        oStart.HasHotspot := True;
        oStart.Hotspot.URL := oReader.GetAttribute(XLINK_NS, 'href');
        oStart.Hotspot.Title := oReader.GetAttribute(OFFICE_NS, 'name');
        // TODO: convert link color & visit color from style
        Result := oDocument.Pieces.Add(oStart.Link);

        ReadParagraphContent(oDocument, oReader);
        oReader.ConsumeClose;
        oStop.Style := oStart.Style;
        oStop.Font.Assign(oStart.Font);
      Finally
        oStart.Free;
      End;

      oDocument.Pieces.Add(oStop.Link);
    Finally
      oStop.Free;
    End;
  End;
End;


Procedure TWPOdtReader.ReadLineBreak(oDocument: TWPWorkingDocument; oReader: TFslXMLExtractor);
Var
  oBreak : TWPWorkingDocumentLineBreakPiece;
Begin
  oBreak := TWPWorkingDocumentLineBreakPiece.Create;
  Try
    // TODO: Read style
    oDocument.Pieces.Add(oBreak.Link);
  Finally
    oBreak.Free;
  End;

  oReader.SkipNext;
End;


Procedure TWPOdtReader.ReadFontDeclarations(oReader: TFslXMLExtractor);
Var
  sName, sFamily : String;
Begin
  // Loop through the font declarations and read it
  // TODO: Duplicate declaration is possible,
  // as both content.xml and styles.xml can have this declaration
  If oReader.PeekIsEmptyNode Then
    oReader.SkipNext   // don't need to do anything
  Else
  Begin
    oReader.ConsumeOpen;
    While oReader.PeekIsOpen And oReader.IsNode(STYLE_NS, 'font-face') Do
    Begin
      sName := oReader.GetAttribute(STYLE_NS, 'name');
      sFamily := oReader.GetAttribute(SVG_NS, 'font-family');
      If (sName <> '') And (sFamily <> '') Then
      Begin
        // FIXME: small hack to avoid single-quote in font-name with space
        sFamily := DecodeXML(sFamily);
        sFamily := StringReplaceAll(sFamily, '''', '');

        FFontDecls.Add(sName, sFamily);
      End;

      oReader.SkipNext;
    End;
    oReader.ConsumeClose;
  End;
End;


Function TWPOdtReader.ReadOdtListStyle(oReader: TFslXMLExtractor): TOdtStyle;
Var
  oStyle: TOdtStyle;
  bIsEmptyNode: Boolean;
  oLevelFormat: TOdtListLevelFormat;
Begin
  // construct the style object
  oStyle := TOdtStyle.Create();
  // read style info.
  Try
    bIsEmptyNode := oReader.PeekIsEmptyNode;
    // Read style attribute
    oStyle.Name := oReader.GetAttribute(STYLE_NS, 'name');
    oStyle.Family := osfParagraph;      // set to paragraph

    If bIsEmptyNode Then
      oReader.SkipNext
    Else
    Begin
      oReader.ConsumeOpen;
      While oReader.PeekIsOpen Do
      Begin
        If oReader.IsNode(TEXT_NS, 'list-level-style-number')
          Or oReader.IsNode(TEXT_NS, 'list-level-style-bullet') Then
        Begin
          { Read the level format }
          If Not oStyle.HasListFormat Then
            oStyle.ListFormat := TOdtListFormat.Create;

          oLevelFormat := TOdtListLevelFormat.Create;
          Try
            If StringEquals(oReader.NodeLocalName, 'list-level-style-bullet') Then
              oLevelFormat.BulletType := tbCircle //WideString(oReader.GetAttribute(TEXT_NS, 'bullet-char'))[1]
            Else
            Begin
              oLevelFormat.NumberStyle := oReader.GetAttribute(STYLE_NS, 'num-format');
              oLevelFormat.NumberFormat := oReader.GetAttribute(STYLE_NS, 'num-suffix');
              oLevelFormat.StartValue := oReader.GetAttribute(TEXT_NS, 'start-value');
            End;

            oStyle.ListFormat.Add(oLevelFormat.Link);
          Finally
            oLevelFormat.Free;
          End;
        End;

        // always skip
        oReader.SkipNext;
      End;
      oReader.ConsumeClose;
    End;
    Result := oStyle.Link;
  Finally
    oStyle.Free;
  End;
End;


Function TWPOdtReader.ReadOdtStyle(oReader: TFslXMLExtractor; bCanHaveParent:Boolean): TOdtStyle;
Var
  oStyle, oParent: TOdtStyle;
  sLocalName, sParentName: String;
  bIsEmptyNode: Boolean;
Begin
  // construct the style object
  oParent := Nil;
  If bCanHaveParent Then
  Begin
    sParentName := oReader.GetAttribute(STYLE_NS, 'parent-style-name');
    oParent := GetStyleByName(sParentName);
  End;
  oStyle := TOdtStyle.Create(oParent);

  // read style info.
  Try
    bIsEmptyNode := oReader.PeekIsEmptyNode;
    // Read style attribute
    oStyle.Name := oReader.GetAttribute(STYLE_NS, 'name');
    oStyle.Family := TOdtStyleFamily(Self.ReadEnumeratedAttribute(oReader.GetAttribute(STYLE_NS, 'family'), NAMES_TODTSTYLEFAMILY, 0));

    If bIsEmptyNode Then
      oReader.SkipNext
    Else
    Begin
      oReader.ConsumeOpen;
      While oReader.PeekIsOpen Do
      Begin
        If oReader.NodeNamespace <> STYLE_NS Then
          oReader.SkipNext   // don't need to do anything
        Else
        Begin
          sLocalName := oReader.NodeLocalName;
          // TODO: Read other formating properties
          If sLocalName = 'text-properties' Then
          Begin
            oStyle.TextFormat := TOdtTextFormat.Create;
            Self.ReadOdtStyleTextProperties(oStyle.TextFormat, oReader);
          End
          Else If sLocalName = 'paragraph-properties' Then
          Begin
            oStyle.ParagraphFormat := TOdtParagraphFormat.Create;
            Self.ReadOdtStyleParagraphProperties(oStyle.ParagraphFormat, oReader);
          End
          Else If sLocalName = 'table-properties' Then
          Begin
            oStyle.TableFormat := TOdtTableFormat.Create;
            Self.ReadOdtStyleTableProperties(oStyle.TableFormat, oReader);
          End
          Else If sLocalName = 'table-row-properties' Then
          Begin
            oStyle.RowFormat := TOdtTableRowFormat.Create;
            Self.ReadOdtStyleRowProperties(oStyle.RowFormat, oReader);
          End
          Else If sLocalName = 'table-cell-properties' Then
          Begin
            oStyle.CellFormat := TOdtTableCellFormat.Create;
            Self.ReadOdtStyleCellProperties(oStyle.CellFormat, oReader);
          End
          Else
            oReader.SkipNext;     // TODO: process other formating properties
        End;
      End;
      oReader.ConsumeClose;
    End;
    Result := oStyle.Link;
  Finally
    oStyle.Free;
  End;
End;

Function TWPOdtReader.ReadEnumeratedAttribute(Const sValue: String;
  Const aValues: Array Of String; Const aDefault: Byte): Byte;
Var
  iResult : Integer;
Begin
  iResult := StringArrayIndexOf(aValues, sValue);
  If iResult = -1 Then
    Result := aDefault
  Else
    Result := iResult;
End;

Procedure TWPOdtReader.ReadOdtStyleTextProperties(oTextFormat: TOdtTextFormat; oReader: TFslXMLExtractor);
Begin
  oReader.PeekXml; // load attribute

  oTextFormat.Name := oReader.GetAttribute(STYLE_NS, 'font-name');
  oTextFormat.Family := oReader.GetAttribute(FONT_NS, 'font-family');
  oTextFormat.Size := oReader.GetAttribute(FONT_NS, 'font-size');

  oTextFormat.UseWindowColor := StringToBoolean(oReader.GetAttribute(STYLE_NS, 'use-window-font-color'));
  oTextFormat.Color := XMLColourStringToColourOrDefault(oReader.GetAttribute(FONT_NS, 'color'), DEF_COLOUR);
  oTextFormat.Background := XMLColourStringToColourOrDefault(oReader.GetAttribute(FONT_NS, 'background-color'), DEF_COLOUR);

  oTextFormat.Style := oReader.GetAttribute(FONT_NS, 'font-style');
  oTextFormat.Weight := oReader.GetAttribute(FONT_NS, 'font-weight');
  oTextFormat.Position := oReader.GetAttribute(STYLE_NS, 'text-position');

  oTextFormat.UnderlineType := oReader.GetAttribute(STYLE_NS, 'text-underline-type');
  oTextFormat.UnderlineStyle := oReader.GetAttribute(STYLE_NS, 'text-underline-style');
  oTextFormat.LineThroughType := oReader.GetAttribute(STYLE_NS, 'text-line-through-type');
  oTextFormat.LineThroughStyle := oReader.GetAttribute(STYLE_NS, 'text-line-through-style');

  oTextFormat.TextTransform := oReader.GetAttribute(FONT_NS, 'text-transform');

  // consume and ignore any node that we don't known
  oReader.SkipNext;
End;

Procedure TWPOdtReader.ReadOdtStyleParagraphProperties(oParaFormat: TOdtParagraphFormat; oReader: TFslXMLExtractor);
Var
  sTemp : String;
Begin
  oReader.PeekXml; // load attribute

  oParaFormat.TextAlign := oReader.GetAttribute(FONT_NS, 'text-align');

  sTemp := oReader.GetAttribute(FONT_NS, 'margin');
  oParaFormat.MarginLeft := oReader.GetAttribute(FONT_NS, 'margin-left', sTemp);
  oParaFormat.MarginRight := oReader.GetAttribute(FONT_NS, 'margin-right', sTemp);
  oParaFormat.MarginBottom := oReader.GetAttribute(FONT_NS, 'margin-bottom', sTemp);
  oParaFormat.BreakBefore := oReader.GetAttribute(FONT_NS, 'break-before');
  oParaFormat.BreakAfter := oReader.GetAttribute(FONT_NS, 'break-after');

  oReader.SkipNext;     // TODO: To be implemented
End;


Procedure TWPOdtReader.ReadOdtStyleTableProperties(oFormat: TOdtTableFormat; oReader: TFslXMLExtractor);
Var
  sTemp : String;
Begin
  oReader.PeekXml;

  oFormat.BackgroundColor := XMLColourStringToColourOrDefault(oReader.GetAttribute(FONT_NS, 'background-color'), DEF_COLOUR);
  sTemp := oReader.GetAttribute(FONT_NS, 'margin');
  oFormat.MarginLeft := oReader.GetAttribute(FONT_NS, 'margin-left', sTemp);
  oFormat.MarginRight := oReader.GetAttribute(FONT_NS, 'margin-right', sTemp);
  oFormat.MarginTop := oReader.GetAttribute(FONT_NS, 'margin-top', sTemp);
  oFormat.MarginBottom := oReader.GetAttribute(FONT_NS, 'margin-bottom', sTemp);

  oReader.SkipNext;
End;


Procedure TWPOdtReader.ReadOdtStyleRowProperties(oFormat: TOdtTableRowFormat; oReader: TFslXMLExtractor);
Begin
  oReader.PeekXml;

  oFormat.BackgroundColor := XMLColourStringToColourOrDefault(oReader.GetAttribute(FONT_NS, 'background-color'), DEF_COLOUR);

  oReader.SkipNext;
End;


Procedure TWPOdtReader.ReadOdtStyleCellProperties(oFormat: TOdtTableCellFormat; oReader: TFslXMLExtractor);
Var
  sTemp: String;
Begin
  oReader.PeekXml;

  oFormat.BackgroundColor := XMLColourStringToColourOrDefault(oReader.GetAttribute(FONT_NS, 'background-color'), DEF_COLOUR);
  sTemp := oReader.GetAttribute(FONT_NS, 'border');
  If sTemp <> '' Then
  Begin
    oFormat.BorderTop := sTemp;
    oFormat.BorderBottom := sTemp;
    oFormat.BorderLeft := sTemp;
    oFormat.BorderRight := sTemp;
  End
  Else
  Begin
    oFormat.BorderTop := oReader.GetAttribute(FONT_NS, 'border-top');
    oFormat.BorderBottom := oReader.GetAttribute(FONT_NS, 'border-bottom');
    oFormat.BorderLeft := oReader.GetAttribute(FONT_NS, 'border-left');
    oFormat.BorderRight := oReader.GetAttribute(FONT_NS, 'border-right');
  End;

  oReader.SkipNext;
End;


Function TWPOdtReader.GetStyleByName(Const sStyleName: String): TOdtStyle;
Var
  iIndex: Integer;
Begin
  Result := Nil;

  If sStyleName <> '' Then
  Begin
    For iIndex := 0 To FStyles.Count - 1 Do
    Begin
      If StringEquals(sStyleName, FStyles[iIndex].Name) Then
      Begin
        Result := FStyles[iIndex];
        Break;
      End;
    End;
  End;
End;


Function TWPOdtReader.GetStyleForFamily(Const aFamily: TOdtStyleFamily; Const sStyleName: String): TOdtStyle;
Var
  iIndex: Integer;
Begin
  Result := GetStyleByName(sStyleName);

  If Result = Nil Then
  Begin
    For iIndex := 0 To FDefaultStyles.Count - 1 Do
    Begin
      If aFamily = FDefaultStyles[iIndex].Family Then
      Begin
        Result := FDefaultStyles[iIndex];
        Break;
      End;
    End;
  End;
End;


Procedure TWPOdtReader.ApplyListStyle(oParaFormat: TWPSParagraphDetails; Const iItemCount: Integer);
Begin
  Assert((FCurrentListStyle <> Nil) And (FCurrentListStyle.HasListFormat));

  FCurrentListStyle.ListFormat.ApplyStyle(oParaFormat, FCurrentListLevel, iItemCount);
End;

function TWPOdtReader.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FOdtAdapter.sizeInBytes);
  inc(result, FFontDecls.sizeInBytes);
  inc(result, FDefaultStyles.sizeInBytes);
  inc(result, FStyles.sizeInBytes);
  inc(result, FStyleStack.sizeInBytes);
  inc(result, FCurrentListStyle.sizeInBytes);
end;

Constructor TWPOdtWriter.Create;
Begin
  Inherited;

  FOdtAdapter := TOdtWriterAdapter.Create;

  FFontDecls := TOdtFontDeclarations.Create;
  FDefaultStyles := TOdtStyleList.Create;
  FStyles := TOdtStyleList.Create;
  FStyleStack := TOdtStyleStack.Create;
End;


Destructor TWPOdtWriter.Destroy;
Begin
  FOdtAdapter.Free;

  FFontDecls.Free;
  FDefaultStyles.Free;
  FStyles.Free;
  FStyleStack.Free;

  Inherited;
End;


Function TWPOdtWriter.Link: TWPOdtWriter;
Begin
  Result := TWPOdtWriter(Inherited Link);
End;


Procedure TWPOdtWriter.Initialise;
Begin
  Inherited;

  // TODO: clear up styles, prepare the jar file
  FFontDecls.Clear;
  FDefaultStyles.Clear;
  FStyles.Clear;
  FStyleStack.Clear;

  FInSpan := False;
  FInList := False;
  FListNumber := -1;
End;


Procedure TWPOdtWriter.Finalise;
Var
  oMem : TFslMemoryStream;
  oFormatter : TFslXMLFormatter;
Begin
  // Flush out the styles
  oMem := TFslMemoryStream.Create;
  Try
    oMem.Buffer := FOdtAdapter.GetSubDocStream(OpenDocumentSubDocStyle).Link;
    oFormatter := TFslXMLFormatter.Create;
    Try
      oFormatter.Stream := oMem.Link;
      oFormatter.HasWhitespace := True;
      WriteStyleSubDoc(oFormatter);
    Finally
      oFormatter.Free;
    End;
  Finally
    oMem.Free;
  End;

  FOdtAdapter.Finish(Stream);

  Inherited;
End;


Procedure TWPOdtWriter.Write(oDocument: TWPWorkingDocument);
Var
  oMem : TFslMemoryStream;
Begin
  Assert(CheckCondition(FFormatter = Nil, 'Write', 'Invalid state: still not yet finished writing.'));

  oMem := TFslMemoryStream.Create;
  Try
    oMem.Buffer := FOdtAdapter.GetSubDocStream(OpenDocumentSubDocContent);
    FFormatter := TFslXMLFormatter.Create;
    Try
      FFormatter.Stream := oMem.Link;
      FFormatter.HasWhitespace := True;
      DeclareOdtNamespaces(FFormatter);
      FFormatter.ProduceOpen('office:document-content');
      Inherited;
      FFormatter.ProduceClose('office:document-content');
    Finally
      FFormatter.Free;
      FFormatter := Nil;
    End;
  Finally
    oMem.Free;
  End;
End;



Procedure TWPOdtWriter.WriteDocumentStart(oDocument: TWPWorkingDocument);
Begin
  FFormatter.ProduceOpen('office:body');
  FFormatter.ProduceOpen('office:text');
End;


Procedure TWPOdtWriter.WriteDocumentStop(oDocument: TWPWorkingDocument);
Begin
  FFormatter.ProduceClose('office:text');
  FFormatter.ProduceClose('office:body');
End;


Procedure TWPOdtWriter.IterateParagraph(oIterator : TWPPieceIterator; oSection : TWPWorkingDocumentSectionStartPiece);
Var
  oPara : TWPWorkingDocumentParaPiece;
Begin
  oPara := oIterator.CurrentParagraph(True);

  WriteParagraphStart(oPara);

  IterateParagraphContents(oIterator);

  WriteParagraphStop(oPara, (oIterator.Peek <> Nil) And (oIterator.Peek.PieceType = ptSectionStop), oSection);

  oIterator.Next;
End;


Procedure TWPOdtWriter.WriteStyleSubDoc(oFormatter: TFslXMLFormatter);
Begin
  DeclareOdtNamespaces(oFormatter);
  oFormatter.ProduceOpen('office:document-styles');

  // write the body of style files
  WriteFontDeclarations(oFormatter);
  WriteStyles(oFormatter);

  oFormatter.ProduceClose('office:document-styles');
End;


Procedure TWPOdtWriter.WriteFontDeclarations(oFormatter: TFslXMLFormatter);
Var
  iIndex: Integer;
Begin
  oFormatter.ProduceOpen('office:font-face-decls');

  // declare fonts
  For iIndex := 0 To FFontDecls.Count -1 Do
  Begin
    oFormatter.AddAttribute('style:name', FFontDecls.KeyByIndex[iIndex]);
    oFormatter.AddAttribute('svg:font-family', FFontDecls.KeyByIndex[iIndex]);
    oFormatter.ProduceTag('style:font-face');
  End;

  oFormatter.ProduceClose('office:font-face-decls');
End;


Procedure TWPOdtWriter.WriteStyles(oFormatter: TFslXMLFormatter);
Var
  iIndex: Integer;
Begin
  oFormatter.ProduceOpen('office:styles');

  For iIndex := 0 To FStyles.Count - 1 Do
    WriteOdtStyle(oFormatter, FStyles[iIndex], False);

  oFormatter.ProduceClose('office:styles');
End;


Procedure TWPOdtWriter.WriteOdtStyle(oFormatter: TFslXMLFormatter; oStyle: TOdtStyle; bIsDefaultStyle: Boolean);
Var
  sTag : String;
Begin
  If oStyle.HasListFormat Then
    sTag := 'text:list-style'
  Else If bIsDefaultStyle Then
    sTag := 'style:default-style'
  Else
    sTag := 'style:style';

  oFormatter.AddAttribute('style:name', oStyle.Name);
  oFormatter.AddAttribute('style:family', NAMES_TODTSTYLEFAMILY[oStyle.Family]);
  oFormatter.ProduceOpen(sTag);

  If oStyle.HasListFormat Then
    // list format can't be mixed with others
    WriteListFormat(oFormatter, oStyle.ListFormat)
  Else
  Begin
    If oStyle.HasTextFormat Then
      WriteTextFormat(oFormatter, oStyle.TextFormat);
    If oStyle.HasParagraphFormat Then
      WriteParaFormat(oFormatter, oStyle.ParagraphFormat);
    If oStyle.HasTableFormat Then
      WriteTableFormat(oFormatter, oStyle.TableFormat);
    If oStyle.HasRowFormat Then
      WriteTableRowFormat(oFormatter, oStyle.RowFormat);
    If oStyle.HasCellFormat Then
      WriteTableCellFormat(oFormatter, oStyle.CellFormat);
  End;

  oFormatter.ProduceClose(sTag);
End;


Procedure TWPOdtWriter.WriteTextFormat(oFormatter: TFslXMLFormatter;
  oTextFormat: TOdtTextFormat);
Begin
  If oTextFormat.HasName Then
    oFormatter.AddAttribute('style:font-name', oTextFormat.Name);
  If oTextFormat.HasFamily Then
    oFormatter.AddAttribute('fo:font-family', oTextFormat.Family);
  If oTextFormat.HasSize Then
    oFormatter.AddAttribute('fo:font-size', oTextFormat.Size);

  If oTextFormat.HasUseWindowColor Then
    oFormatter.AddAttribute('style:use-window-font-color', BooleanToString(oTextFormat.UseWindowColor));
  If oTextFormat.HasColor Then
    oFormatter.AddAttribute('fo:color', ColourToXMLColourString(oTextFormat.Color));
  If oTextFormat.HasBackground Then
    oFormatter.AddAttribute('fo:background-color', ColourToXMLColourString(oTextFormat.Background));

  If oTextFormat.HasStyle Then
    oFormatter.AddAttribute('fo:font-style', oTextFormat.Style);
  If oTextFormat.HasWeight Then
    oFormatter.AddAttribute('fo:font-weight', oTextFormat.Weight);
  If oTextFormat.HasPosition Then
    oFormatter.AddAttribute('style:text-position', oTextFormat.Position);

  If oTextFormat.HasUnderlineType Then
    oFormatter.AddAttribute('style:text-underline-type', oTextFormat.UnderlineType);
  If oTextFormat.HasUnderlineStyle Then
    oFormatter.AddAttribute('style:text-underline-style', oTextFormat.UnderlineStyle);
  If oTextFormat.HasLineThroughType Then
    oFormatter.AddAttribute('style:text-line-through-type', oTextFormat.LineThroughType);
  If oTextFormat.HasLineThroughStyle Then
    oFormatter.AddAttribute('style:text-line-through-style', oTextFormat.LineThroughStyle);

  If oTextFormat.HasTextTransform Then
    oFormatter.AddAttribute('fo:text-transform', oTextFormat.TextTransform);

  oFormatter.ProduceOpen('style:text-properties');
  // TODO
  oFormatter.ProduceClose('style:text-properties');
End;


Procedure TWPOdtWriter.WriteParaFormat(oFormatter: TFslXMLFormatter;
  oParaFormat: TOdtParagraphFormat);
Begin
  oFormatter.AddAttribute('fo:text-align', oParaFormat.TextAlign);

  oFormatter.AddAttribute('fo:margin-left', oParaFormat.MarginLeft);
  oFormatter.AddAttribute('fo:margin-right', oParaFormat.MarginRight);
  oFormatter.AddAttribute('fo:margin-bottom', oParaFormat.MarginBottom);

  oFormatter.AddAttribute('fo:break-before', oParaFormat.BreakBefore);
  oFormatter.AddAttribute('fo:break-after', oParaFormat.BreakAfter);

  oFormatter.ProduceOpen('style:paragraph-properties');
  // TODO
  oFormatter.ProduceClose('style:paragraph-properties');
End;


Procedure TWPOdtWriter.WriteListFormat(oFormatter: TFslXMLFormatter; oListFormat: TOdtListFormat);
Var
  oLevelFormat: TOdtListLevelFormat;
Begin
  oLevelFormat := oListFormat[0];       // only expected 1 level

  oFormatter.AddAttribute('text:level', '1');
  If oLevelFormat.WPListType = WPSParagraphListTypeBullets Then
  Begin
    Case oLevelFormat.BulletType Of
    tbSquare:     oFormatter.AddAttribute('text:bullet-char', '&#9642;');
    Else       oFormatter.AddAttribute('text:bullet-char', '&#8226;');
    End;

    oFormatter.ProduceTag('text:list-level-style-bullet');
  End
  Else  // assume to be numbered list
  Begin
    oFormatter.AddAttribute('style:num-format', oLevelFormat.NumberStyle);
    oFormatter.AddAttribute('style:num-suffix', oLevelFormat.NumberFormat);
    oFormatter.AddAttribute('text:start-value', oLevelFormat.StartValue);
    oFormatter.ProduceTag('text:list-level-style-number');
  End;

End;


Procedure TWPOdtWriter.WriteTableFormat(oFormatter: TFslXMLFormatter; oTableFormat: TOdtTableFormat);
Begin
  oFormatter.AddAttribute('fo:background-color', ColourToXMLColourString(oTableFormat.BackgroundColor));
  oFormatter.AddAttribute('fo:margin-left', oTableFormat.MarginLeft);
  oFormatter.AddAttribute('fo:margin-right', oTableFormat.MarginRight);
  oFormatter.AddAttribute('fo:margin-top', oTableFormat.MarginTop);
  oFormatter.AddAttribute('fo:margin-bottom', oTableFormat.MarginBottom);
  oFormatter.ProduceTag('style:table-properties ');
End;


Procedure TWPOdtWriter.WriteTableRowFormat(oFormatter: TFslXMLFormatter; oRowFormat: TOdtTableRowFormat);
Begin
  oFormatter.AddAttribute('fo:background-color', ColourToXMLColourString(oRowFormat.BackgroundColor));
  oFormatter.ProduceTag('style:table-row-properties');
End;


Procedure TWPOdtWriter.WriteTableCellFormat(oFormatter: TFslXMLFormatter; oCellFormat: TOdtTableCellFormat);
Begin
  oFormatter.AddAttribute('fo:background-color', ColourToXMLColourString(oCellFormat.BackgroundColor));
  oFormatter.AddAttribute('fo:border-left', oCellFormat.BorderLeft);
  oFormatter.AddAttribute('fo:border-right', oCellFormat.BorderRight);
  oFormatter.AddAttribute('fo:border-top', oCellFormat.BorderTop);
  oFormatter.AddAttribute('fo:border-bottom', oCellFormat.BorderBottom);
  oFormatter.ProduceTag('style:table-cell-properties');
End;


Procedure TWPOdtWriter.DeclareOdtNamespaces(oFormatter: TFslXMLFormatter);
Begin
  { Called before open the root element, to addd namespaces delcaration }
  { Namespaces Used:
xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"
xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0"
xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"
xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0"
xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0"
xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0"
xmlns:xlink="http://www.w3.org/1999/xlink"
xmlns:dc="http://purl.org/dc/elements/1.1/"
xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0"
xmlns:number="urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0"
xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0"
xmlns:chart="urn:oasis:names:tc:opendocument:xmlns:chart:1.0"
xmlns:dr3d="urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0"
xmlns:math="http://www.w3.org/1998/Math/MathML"
xmlns:form="urn:oasis:names:tc:opendocument:xmlns:form:1.0"
xmlns:script="urn:oasis:names:tc:opendocument:xmlns:script:1.0"
xmlns:ooo="http://openoffice.org/2004/office"
xmlns:ooow="http://openoffice.org/2004/writer"
xmlns:oooc="http://openoffice.org/2004/calc"
xmlns:dom="http://www.w3.org/2001/xml-events"
xmlns:xforms="http://www.w3.org/2002/xforms"
xmlns:xsd="http://www.w3.org/2001/XMLSchema"
xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
xmlns:rpt="http://openoffice.org/2005/report"
xmlns:of="urn:oasis:names:tc:opendocument:xmlns:of:1.2"
xmlns:rdfa="http://docs.oasis-open.org/opendocument/meta/rdfa#"}
    oFormatter.AddNamespace('office', OFFICE_NS);
    oFormatter.AddNamespace('style', STYLE_NS);
    oFormatter.AddNamespace('text', TEXT_NS);
    oFormatter.AddNamespace('table', TABLE_NS);
    oFormatter.AddNamespace('draw', DRAWING_NS);
    oFormatter.AddNamespace('fo', FONT_NS);
    oFormatter.AddNamespace('xlink', XLINK_NS);
    oFormatter.AddNamespace('dc', 'http://purl.org/dc/elements/1.1/');
    oFormatter.AddNamespace('meta', META_NS);
    oFormatter.AddNamespace('number', NUMBER_NS);
    oFormatter.AddNamespace('svg', SVG_NS);
    oFormatter.AddNamespace('chart', CHART_NS);
    oFormatter.AddNamespace('dr3d', DR3D_NS);
    oFormatter.AddNamespace('math', MATH_NS);
    oFormatter.AddNamespace('form', FORM_NS);
    oFormatter.AddNamespace('script', SCRIPT_NS);
    oFormatter.AddNamespace('ooo', 'http://openoffice.org/2004/office');
    oFormatter.AddNamespace('ooow', 'http://openoffice.org/2004/writer');
    oFormatter.AddNamespace('oooc', 'http://openoffice.org/2004/calc');
    oFormatter.AddNamespace('dom', 'http://www.w3.org/2001/xml-events');
    oFormatter.AddNamespace('xforms', 'http://www.w3.org/2002/xforms');
    oFormatter.AddNamespace('xsd', 'http://www.w3.org/2001/XMLSchema');
    oFormatter.AddNamespace('xsi', 'http://www.w3.org/2001/XMLSchema-instance');
    oFormatter.AddNamespace('rpt', 'http://openoffice.org/2005/report');
    oFormatter.AddNamespace('of', 'urn:oasis:names:tc:opendocument:xmlns:of:1.2');
    oFormatter.AddNamespace('rdfa', 'http://docs.oasis-open.org/opendocument/meta/rdfa#');

    oFormatter.AddAttribute('office:version', '1.2');

End;


Procedure TWPOdtWriter.WriteParagraphStart(oParagraph: TWPWorkingDocumentParaPiece);
Var
  oStyle: TOdtStyle;
Begin
  If oParagraph.Format.ListType In [WPSParagraphListTypeBullets, WPSParagraphListTypeNumbers] Then
  Begin
    oStyle := GetListStyle(oParagraph);
    If (Not FInList) Or (oStyle <> FStyleStack.Peek) Then
    Begin
      // close existing list before start new one
      If FInList Then
        CloseList;
      // start new List
      OpenList(oStyle);
    End;
    OpenListItem;
  End;

  // add style
  oStyle := GetParaStyle(oParagraph);
  FStyleStack.Push(oStyle.Link);
  FFormatter.AddAttribute('text:style-name', oStyle.Name);

  FFormatter.ProduceOpen('text:p');
End;


Procedure TWPOdtWriter.WriteParagraphStop(oParagraph: TWPWorkingDocumentParaPiece;
        bNextIsSection: Boolean; oSection: TWPWorkingDocumentSectionStartPiece);
Begin
  If FInSpan Then
    CloseSpan;
  FFormatter.ProduceClose('text:p');
  FStyleStack.Pop;
  If FInList Then
  Begin
    CloseListItem;
    // close list if next is not a list (see TWPWriter.IterateDocument)
    If (Not oParagraph.HasNext)
      Or (oParagraph.Next.PieceType In [ptSectionStart, ptTableStart, ptBreak]) Then
      CloseList;
  End;
End;


Procedure TWPOdtWriter.WriteText(oText: TWPWorkingDocumentTextPiece);
Var
  oStyle: TOdtStyle;
Begin
  // TODO: check style, if not the current style then use span
  oStyle := GetTextStyle(oText);
  If oStyle <> FStyleStack.Peek Then
  Begin
    If FInSpan Then
      CloseSpan;
    OpenSpan(oStyle);
  End;
  FFormatter.ProduceText(oText.Content);
End;


Procedure TWPOdtWriter.CloseSpan;
Begin
  FFormatter.ProduceClose('text:span');
  FStyleStack.Pop;
  FInSpan := False;
End;


Procedure TWPOdtWriter.OpenSpan(oStyle: TOdtStyle);
Begin
  FFormatter.AddAttribute('text:style-name', oStyle.Name);
  FFormatter.ProduceOpen('text:span');
  FInSpan := True;
  FStyleStack.Push(oStyle.Link);
End;


Procedure TWPOdtWriter.CloseList;
Begin
  FFormatter.ProduceClose('text:list');
  FStyleStack.Pop;
  FInList := False;
  FListNumber := -1;
End;


Procedure TWPOdtWriter.OpenList(oStyle: TOdtStyle);
Begin
  FFormatter.AddAttribute('text:style-name', oStyle.Name);
  FFormatter.ProduceOpen('text:list');

  If oStyle.ListFormat[0].WPListType = WPSParagraphListTypeNumbers Then
    FListNumber := 0
  Else
    FListNumber := -1;
  FInList := True;
  FStyleStack.Push(oStyle.Link);
End;


Procedure TWPOdtWriter.CloseListItem;
Begin
  FFormatter.ProduceClose('text:list-item');
End;


Procedure TWPOdtWriter.OpenListItem;
Begin
  FFormatter.ProduceOpen('text:list-item');
  If FStyleStack.Peek.ListFormat[0].WPListType = WPSParagraphListTypeNumbers Then
    Inc(FListNumber);
End;


Function TWPOdtWriter.GetListStyle(Const oPara: TWPWorkingDocumentParaPiece): TOdtStyle;
Var
  iIndex: Integer;
  oStyle: TOdtStyle;
Begin
  { Find the style that matches this piece, creating new one if not found }
  Result := Nil;
  iIndex := 0;
  While ((Result = Nil) And (iIndex < FStyles.Count)) Do
  Begin
    If FStyles[iIndex].HasListFormat
      And FStyles[iIndex].MatchesListFormat(oPara.Format, FListNumber) Then
      Result := FStyles[iIndex];
    Inc(iIndex);
  End;

  // not found any => create new one
  If (Result = Nil) Then
  Begin
    oStyle := TOdtStyle.Create;
    Try
      oStyle.Name := 'ListStyle' + IntegerToString(FStyles.Count);
      oStyle.StyleType := ostCommon;
      oStyle.Family := osfParagraph;
      oStyle.ListFormat := TOdtListFormat.Create(oStyle.Name, oPara.Format);
      iIndex := FStyles.Add(oStyle.Link);
      Result := FStyles[iIndex];
    Finally
      oStyle.Free;
    End;
  End;
End;


Function TWPOdtWriter.GetParaStyle(Const oPara: TWPWorkingDocumentParaPiece): TOdtStyle;
Var
  iIndex: Integer;
  oStyle: TOdtStyle;
Begin
  { Find the style that matches this piece, creating new one if not found }
  Result := Nil;
  iIndex := 0;
  While ((Result = Nil) And (iIndex < FStyles.Count)) Do
  Begin
    If FStyles[iIndex].MatchesParaFormat(oPara.Format)
      And FStyles[iIndex].MatchesTextFormat(oPara.Font) Then
      Result := FStyles[iIndex];
    Inc(iIndex);
  End;

  // not found any => create new one
  If (Result = Nil) Then
  Begin
    oStyle := TOdtStyle.Create;
    Try
      oStyle.ParagraphFormat := TOdtParagraphFormat.Create(oPara.Format);
      oStyle.TextFormat := TOdtTextFormat.Create(oPara.Font);
      oStyle.Name := 'ParaStyle' + IntegerToString(FStyles.Count);
      oStyle.StyleType := ostCommon;
      oStyle.Family := osfParagraph;
      iIndex := FStyles.Add(oStyle.Link);
      Result := FStyles[iIndex];
    Finally
      oStyle.Free;
    End;
  End;
End;


Function TWPOdtWriter.GetTextStyle(Const oText: TWPWorkingDocumentTextPiece): TOdtStyle;
Var
  iIndex: Integer;
  oStyle: TOdtStyle;
Begin
  { Find the style that matches this piece, creating new one if not found }
  Result := Nil;
  // search the style stack first
  iIndex := FStyleStack.Count - 1;
  While (Result = Nil) And (iIndex >= 0) Do
  Begin
    If FStyleStack[iIndex].MatchesTextFormat(oText.Font, False) Then
      Result := FStyleStack[iIndex];
    Dec(iIndex);
  End;
  // search the whole collections of available styles
  iIndex := 0;
  While ((Result = Nil) And (iIndex < FStyles.Count)) Do
  Begin
    If FStyles[iIndex].MatchesTextFormat(oText.Font, False) Then
      Result := FStyles[iIndex];
    Inc(iIndex);
  End;

  // not found any => create new one
  If (Result = Nil) Then
  Begin
    oStyle := TOdtStyle.Create;
    Try
      oStyle.TextFormat := TOdtTextFormat.Create(oText.Font);
      oStyle.Name := 'TextStyle' + IntegerToString(FStyles.Count);
      oStyle.StyleType := ostCommon;
      oStyle.Family := osfText;
      iIndex := FStyles.Add(oStyle.Link);
      Result := FStyles[iIndex];
    Finally
      oStyle.Free;
    End;
  End;
End;


Function TWPOdtWriter.GetTableStyle(Const oTable: TWPWorkingDocumentTableStartPiece): TOdtStyle;
Var
  iIndex: Integer;
  oStyle: TOdtStyle;
Begin
  { Find the style that matches this piece, creating new one if not found }
  Result := Nil;
  iIndex := 0;
  While ((Result = Nil) And (iIndex < FStyles.Count)) Do
  Begin
    If FStyles[iIndex].MatchesTableFormat(oTable) Then
      Result := FStyles[iIndex];
    Inc(iIndex);
  End;

  // not found any => create new one
  If (Result = Nil) Then
  Begin
    oStyle := TOdtStyle.Create;
    Try
      oStyle.TableFormat := TOdtTableFormat.Create(oTable);
      oStyle.Name := 'TableStyle' + IntegerToString(FStyles.Count);
      oStyle.StyleType := ostCommon;
      oStyle.Family := osfTable;
      iIndex := FStyles.Add(oStyle.Link);
      Result := FStyles[iIndex];
    Finally
      oStyle.Free;
    End;
  End;
End;


Function TWPOdtWriter.GetTableRowStyle(Const oRow: TWPWorkingDocumentTableRowStartPiece): TOdtStyle;
Var
  iIndex: Integer;
  oStyle: TOdtStyle;
Begin
  { Find the style that matches this piece, creating new one if not found }
  Result := Nil;
  iIndex := 0;
  While ((Result = Nil) And (iIndex < FStyles.Count)) Do
  Begin
    If FStyles[iIndex].MatchesTableRowFormat(oRow) Then
      Result := FStyles[iIndex];
    Inc(iIndex);
  End;

  // not found any => create new one
  If (Result = Nil) Then
  Begin
    oStyle := TOdtStyle.Create;
    Try
      oStyle.RowFormat := TOdtTableRowFormat.Create(oRow);
      oStyle.Name := 'TableRowStyle' + IntegerToString(FStyles.Count);
      oStyle.StyleType := ostCommon;
      oStyle.Family := osfTableRow;
      iIndex := FStyles.Add(oStyle.Link);
      Result := FStyles[iIndex];
    Finally
      oStyle.Free;
    End;
  End;
End;


Function TWPOdtWriter.GetTableCellStyle(Const oCell: TWPWorkingDocumentTableCellStartPiece): TOdtStyle;
Var
  iIndex: Integer;
  oStyle: TOdtStyle;
Begin
  { Find the style that matches this piece, creating new one if not found }
  Result := Nil;
  iIndex := 0;
  While ((Result = Nil) And (iIndex < FStyles.Count)) Do
  Begin
    If FStyles[iIndex].MatchesTableCellFormat(oCell) Then
      Result := FStyles[iIndex];
    Inc(iIndex);
  End;

  // not found any => create new one
  If (Result = Nil) Then
  Begin
    oStyle := TOdtStyle.Create;
    Try
      oStyle.CellFormat := TOdtTableCellFormat.Create(oCell);
      oStyle.Name := 'TableCellStyle' + IntegerToString(FStyles.Count);
      oStyle.StyleType := ostCommon;
      oStyle.Family := osfTableCell;
      iIndex := FStyles.Add(oStyle.Link);
      Result := FStyles[iIndex];
    Finally
      oStyle.Free;
    End;
  End;
End;


Procedure TWPOdtWriter.WriteLineBreak(oBreak: TWPWorkingDocumentLineBreakPiece);
Begin
  FFormatter.ProduceTag('text:line-break');
End;


Procedure TWPOdtWriter.WriteTableStart(oTable: TWPWorkingDocumentTableStartPiece);
Var
  oStyle: TOdtStyle;
Begin
  // add style
  oStyle := GetTableStyle(oTable);
  FStyleStack.Push(oStyle.Link);
  FFormatter.AddAttribute('table:style-name', oStyle.Name);
  FFormatter.ProduceOpen('table:table');

  // preset no. of column
  FFormatter.AddAttribute('table:number-columns-repeated', intToStr(oTable.ColumnCount));
  FFormatter.ProduceTag('table:table-column');
End;

Procedure TWPOdtWriter.WriteTableStop(oTable: TWPWorkingDocumentTableStartPiece;  oStop: TWPWorkingDocumentStopPiece);
Begin
  FFormatter.ProduceClose('table:table');
  FStyleStack.Pop;
End;

Procedure TWPOdtWriter.WriteTableCellStart(oTableCell: TWPWorkingDocumentTableCellStartPiece);
Var
  oStyle: TOdtStyle;
Begin
  // add style
  oStyle := GetTableCellStyle(oTableCell);
  FStyleStack.Push(oStyle.Link);
  FFormatter.AddAttribute('table:style-name', oStyle.Name);
  FFormatter.ProduceOpen('table:table-cell');
End;

Procedure TWPOdtWriter.WriteTableCellStop(oTableCell: TWPWorkingDocumentTableCellStartPiece; oStop: TWPWorkingDocumentStopPiece);
Begin
  FFormatter.ProduceClose('table:table-cell');
  FStyleStack.Pop;
End;

Procedure TWPOdtWriter.WriteTableRowStart(oTableRow: TWPWorkingDocumentTableRowStartPiece);
Var
  oStyle: TOdtStyle;
Begin
  // add style
  oStyle := GetTableRowStyle(oTableRow);
  FStyleStack.Push(oStyle.Link);
  FFormatter.AddAttribute('table:style-name', oStyle.Name);
  FFormatter.ProduceOpen('table:table-row');
End;

Procedure TWPOdtWriter.WriteTableRowStop(oTableRow: TWPWorkingDocumentTableRowStartPiece; oStop: TWPWorkingDocumentStopPiece; bIsLast: Boolean);
Begin
  FFormatter.ProduceClose('table:table-row');
  FStyleStack.Pop;
End;


function TWPOdtWriter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FOdtAdapter.sizeInBytes);
  inc(result, FFormatter.sizeInBytes);
  inc(result, FFontDecls.sizeInBytes);
  inc(result, FDefaultStyles.sizeInBytes);
  inc(result, FStyles.sizeInBytes);
  inc(result, FStyleStack.sizeInBytes);
end;

Procedure TWPOpenDocPackagePart.Assign(oObject : TFslObject);
Begin
  Inherited;
  FMimeType := TWPOpenDocPackagePart(oObject).FMimeType;
End;



Function TWPOpenDocPackagePart.Link : TWPOpenDocPackagePart;
Begin
  Result := TWPOpenDocPackagePart(Inherited Link);
End;


Function TWPOpenDocPackagePart.Clone : TWPOpenDocPackagePart;
Begin
  Result := TWPOpenDocPackagePart(Inherited Clone);
End;


function TWPOpenDocPackagePart.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FMimeType.length * sizeof(char)) + 12);
end;

Function TWPOpenDocPackage.Clone : TWPOpenDocPackage;
Begin
  Result := TWPOpenDocPackage(Inherited Clone);
End;


Function TWPOpenDocPackage.Link : TWPOpenDocPackage;
Begin
  Result := TWPOpenDocPackage(Inherited Link);
End;


Function TWPOpenDocPackage.ItemClass : TFslObjectClass;
Begin
  Result := TWPOpenDocPackagePart;
End;



Function TWPOpenDocPackage.GetPart(iIndex : Integer) : TWPOpenDocPackagePart;
Begin
  Result := TWPOpenDocPackagePart(ObjectByIndex[iIndex]);
End;


Function TWPOpenDocPackage.GetByName(Const sName: String): TWPOpenDocPackagePart;
Begin
  Result := TWPOpenDocPackagePart(Inherited GetByName(sName));
End;


Function NameForType(Const aType: TOpenDocumentSubDocType): String;
Begin
  Case aType Of
    OpenDocumentSubDocContent: Result := SUBDOC_CONTENT;
    OpenDocumentSubDocStyle: Result := SUBDOC_STYLE;
    OpenDocumentSubDocMeta: Result := SUBDOC_META;
    OpenDocumentSubDocSettings: Result := SUBDOC_SETTINGS;
  Else
    Raise EFslException.Create('Unknown type '+IntegerToString(Ord(aType)));
  End;
End;


{ TOdtReaderAdapter }

Constructor TOdtReaderAdapter.Create;
Begin
  Inherited;

  FPackage := TWPOpenDocPackage.Create;
End;


Destructor TOdtReaderAdapter.Destroy;
Begin
  FPackage.Free;

  Inherited;
End;


Function TOdtReaderAdapter.Link: TOdtReaderAdapter;
Begin
  Result := TOdtReaderAdapter(Inherited Link);
End;



Procedure TOdtReaderAdapter.ReadManifest(oExtractor: TFslXMLExtractor);
Var
  sPath, sType: String;
  iCount : Integer;
Begin
  iCount := 0;
  If oExtractor.PeekIsHeader Then
    oExtractor.ConsumeHeader;
  If oExtractor.PeekIsOpen And oExtractor.IsNode(MANIFEST_NS, MANIFEST_ELEMENT) Then
  Begin
    oExtractor.ConsumeOpen;
    While oExtractor.More And oExtractor.PeekIsOpen Do
    Begin
      If oExtractor.NodeLocalName = 'file-entry' Then
      Begin
        sPath := oExtractor.GetAttribute(MANIFEST_NS, 'full-path');
        sType := oExtractor.GetAttribute(MANIFEST_NS, 'media-type');
        If sPath = '/' Then
        Begin
          FVersion := oExtractor.GetAttribute(MANIFEST_NS, 'version');
          FMediaType := sType;
        End
        Else If FPackage.ExistsByName(sPath) Then
        Begin
          Inc(iCount);
          FPackage.GetByName(sPath).MimeType := sType;
        End
        Else If Not StringEndsWith(sPath, '/') Then
          // some directory is implicited
          RaiseError('ReadManifest', 'Resource missing from zip file: ' + sPath + ':' + sType);
        // TODO: we ignore implicit directory for now
        // but may need to re-created it in case we need their information
      End;
      oExtractor.SkipNext; // skip next element
    End;
    oExtractor.ConsumeClose;
  End;
  If (iCount = 0) Or Not (FPackage.ExistsByName('content.xml')) Then
    RaiseError('ReadManifest', 'No OpenDocument Content Found')
  Else If (iCount = 1) Then
    RaiseError('ReadManifest', 'Single OpenDocument Content Model - not handled at this time');
End;


Procedure TOdtReaderAdapter.LoadManifest;
Var
  oManifestStream: TFslMemoryStream;
  oExtractor: TFslXMLExtractor;
Begin
  oManifestStream := TFslMemoryStream.Create;
  Try
    oManifestStream.Buffer := GetFileByPath(MANIFEST_PATH).Link;
    oExtractor := TFslXMLExtractor.Create(oManifestStream.Link);
    Try
      ReadManifest(oExtractor);
    Finally
      oExtractor.Free;
    End;
  Finally
    oManifestStream.Free;
  End;
End;


Procedure TOdtReaderAdapter.Load(oStream : TFslStream);
Var
  oReader : TFslZipReader;
Begin
  FPackage.Clear;
  FVersion := '';
  FMediaType := '';

  oReader := TFslZipReader.Create;
  Try
    oReader.Parts := FPackage.Link;
    oReader.Stream := oStream.Link;
    oReader.ReadZip;
  Finally
    oReader.Free;
  End;

  LoadManifest;
End;


Function TOdtReaderAdapter.GetFileByPath(Const sFilePath: String): TFslBuffer;
Begin
  Result := FPackage.GetByName(sFilePath);
  If (Result = Nil) Then
    RaiseError('GetFileByPath', 'Unable to find "'+sFilePath+'" in ODT Archive');
End;


Function TOdtReaderAdapter.GetSubDoc(Const aType: TOpenDocumentSubDocType): TFslXMLExtractor;
Var
  oResult : TFslXMLExtractor;
  oStream: TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := GetFileByPath(NameForType(aType)).Link;
    oResult := TFslXMLExtractor.Create(oStream.Link);
    Try
      Result := oResult.Link;
    Finally
      oResult.Free;
    End
  Finally
    oStream.Free;
  End;
End;





function TOdtReaderAdapter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, (FMediaType.length * sizeof(char)) + 12);
  inc(result, FPackage.sizeInBytes);
end;

{ TOdtWriterAdapter }

Constructor TOdtWriterAdapter.Create;
Begin
  Inherited;

  FPackage := TWPOpenDocPackage.Create;
End;

Destructor TOdtWriterAdapter.Destroy;
Begin
  FPackage.Free;

  Inherited;
End;


Function TOdtWriterAdapter.Link: TOdtWriterAdapter;
Begin
  Result := TOdtWriterAdapter(Inherited Link);
End;


Procedure TOdtWriterAdapter.Finish(oStream : TFslStream);
Var
  oWriter : TFslZipWriter;
Begin
  WriteManifest;
  WriteMIMEType; // must come after write manifest because it doesn't appear in the manifest

  oWriter := TFslZipWriter.Create;
  Try
    oWriter.Parts := FPackage.Link;
    oWriter.Stream := oStream.Link;
    oWriter.WriteZip;
  Finally
    oWriter.Free;
  End;

  FPackage.Clear;
End;


Function TOdtWriterAdapter.GetByName(Const sName, sMimeType : String) : TFslBuffer;
Var
  oPart : TWPOpenDocPackagePart;
Begin
  If FPackage.ExistsByName(sName) Then
    RaiseError('GetSubDocStream', 'Attempt to create a duplicate resource: "'+sName+'"');

  oPart := TWPOpenDocPackagePart.Create;
  Try
    oPart.Timestamp := now;
    oPart.MimeType := sMimeType;
    oPart.Name := sName;
    FPackage.Add(oPart.Link);
    Result := oPart;
  Finally
    oPart.Free;
  End;
End;


Function TOdtWriterAdapter.GetSubDocStream(Const aType: TOpenDocumentSubDocType): TFslBuffer;
Begin
  Result := GetByName(NameForType(aType), 'text/xml');
End;



Procedure TOdtWriterAdapter.WriteManifest;
Var
  oStream : TFslMemoryStream;
  oFormatter: TFslXMLFormatter;
  iLoop : Integer;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := GetByName('META-INF\manifest.xml', '--');

    // write the manifest content
    oFormatter := TFslXMLFormatter.Create;
    Try
      oFormatter.Stream := oStream.Link;
      oFormatter.Attributes.Add('xmlns:manifest', 'urn:oasis:names:tc:opendocument:xmlns:manifest:1.0');
      oFormatter.ProduceOpen('manifest:manifest');

      oFormatter.Attributes.Add('manifest:media-type', 'application/vnd.oasis.opendocument.text');
      oFormatter.Attributes.Add('manifest:version', '1.2');
      oFormatter.Attributes.Add('manifest:full-path', '/');
      oFormatter.ProduceTag('manifest:file-entry');

      For iLoop := 0 To FPackage.Count - 1 Do
        If FPackage[iLoop].Name <> 'META-INF\manifest.xml' Then // manifest doesn't list itself
        Begin
          oFormatter.Attributes.Add('manifest:media-type', FPackage[iLoop].MimeType);
          oFormatter.Attributes.Add('manifest:full-path', FPackage[iLoop].Name);
          oFormatter.ProduceTag('manifest:file-entry');
        End;
      oFormatter.ProduceClose('manifest:manifest');
    Finally
      oFormatter.Free;
    End;
  Finally
    oStream.Free;
  End;
End;


Procedure TOdtWriterAdapter.WriteMIMEType;
Var
  oStream : TFslMemoryStream;
  sText: String;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := GetByName('mimetype', '--');
    sText := 'application/vnd.oasis.opendocument.text';
    oStream.Write(Pointer(sText)^, Length(sText));
  Finally
    oStream.Free;
  End;
End;


Function LengthCSS2ToPixel(Const sValue: String; Const iDefault: Integer): Integer;
Var
  sUnit: String;
Begin
  Result := iDefault;

  If (Length(sValue) > 2) Then
  Begin
    Try
      sUnit := StringCopy(sValue, Length(sValue) - 1, 2);
      If StringEquals(sUnit, 'cm') Then
        Result := Round(StringToReal(StringCopy(sValue, 0, Length(sValue) - 2)) * 50)
      Else
        Result := Round(StringToReal(StringCopy(sValue, 0, Length(sValue) - 2)));
    Except
      Result := iDefault;
    End;
  End;
End;

Function LengthPixelToCSS2(Const iValue: Integer; Const sDefault: String): String;
Begin
  Try
    Result := IntegerToString(iValue) + 'px';
  Except
    Result := sDefault;
  End;
End;


Function BorderCSS2ToWPBorder(Const sBorder: String): TWPBorder;
Var
  oBorder: TWPBorder;
  sTemp, sWidth, sStyle, sColor: String;
Begin
  oBorder := TWPBorder.Create;
  Try
    If (sBorder = '') Or (StringEquals(sBorder, 'none')) Then
      oBorder.Defined := False
    Else
    Begin
      StringSplit(sBorder, ' ', sWidth, sTemp);
      StringSplit(sTemp, ' ', sStyle, sColor);
      If StringEquals(sWidth, '0')
        Or StringEquals(sStyle, 'none') Or StringEquals(sStyle, 'hidden') Then
        oBorder.Defined := False
      Else
      Begin
        oBorder.Defined := True;
        oBorder.Width := LengthCSS2ToPixel(sWidth, 1);
        oBorder.Colour := XMLColourStringToColourOrDefault(sColor, clBlack);

        If StringEquals(sStyle, 'dotted') Then
          oBorder.Style := TFslPenStyle.apsDot
        Else If StringEquals(sStyle, 'dashed') Then
          oBorder.Style := apsDash
        Else
          oBorder.Style := apsSolid;
      End;
    End;
    Result := oBorder.Link;
  Finally
    oBorder.Free;
  End;
End;


Function WPBorderToBorderCSS2(Const oBorder: TWPBorder): String;
Var
  sWidth, sStyle, sColor: String;
Begin
  If (oBorder = Nil) Or (Not oBorder.Defined) Then
    Result := ''
  Else
  Begin
    sColor := ColourToXMLColourString(oBorder.Colour);
    sWidth := LengthPixelToCSS2(oBorder.Width, '1');
    Case oBorder.Style Of
    apsDot:     sStyle := 'dotted';
    apsDash:    sStyle := 'dashed';
    Else        sStyle := 'solid';
    End;

    Result := sWidth + ' ' + sStyle + ' ' + sColor;
  End;
End;


Function SimpleTriStateMatches(Const aVal, aVal2 : TWPSTriState): Boolean;
Begin
  Result := (aVal = aVal2) Or ((aVal = tsUnknown) And (aVal2 = tsFalse));
End;

Function SimpleFontStateMatches(Const aVal, aVal2 : TWPSFontState): Boolean;
Begin
  Result := (aVal = aVal2) Or ((aVal = fsUnknown) And (aVal2 = fsNormal));
End;

Function SimpleCapsStateMatches(Const aVal, aVal2 : TWPSCapsState): Boolean;
Begin
  Result := (aVal = aVal2) Or ((aVal = fcsUnknown) And (aVal2 = fcsNormal));
End;

function TOdtWriterAdapter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPackage.sizeInBytes);
end;

{ TOdtTextFormat }

Constructor TOdtTextFormat.Create;
Begin
  Inherited;

  FColor := DEF_COLOUR;
  FUseWindowColor := True;;
  FBackground := DEF_COLOUR;
End;

Constructor TOdtTextFormat.Create(Const oParent: TOdtTextFormat);
Begin
  Inherited Create;

  Self.FName := oParent.FName;
  Self.FFamily := oParent.FFamily;
  Self.FSize := oParent.FSize;

  Self.FColor := oParent.FColor;
  Self.FUseWindowColor := oParent.FUseWindowColor;
  Self.FBackground := oParent.FBackground;

  Self.FStyle := oParent.FStyle;
  Self.FWeight := oParent.FWeight;
  Self.FPosition := oParent.FPosition;

    // underline
  Self.FUnderlineType := oParent.FUnderlineType;
  Self.FUnderlineStyle := oParent.FUnderlineStyle;

    // strike through
  Self.FLineThroughType := oParent.FLineThroughType;
  Self.FLineThroughStyle := oParent.FLineThroughStyle;

  Self.FTextTransform := oParent.FTextTransform;
End;


Constructor TOdtTextFormat.Create(Const oFormat: TWPSFontDetails);
Begin
  Inherited Create;

  Self.Name := oFormat.Name;
  Self.Family := '';

  Self.SetFontSize(oFormat.Size);
  Self.SetForeground(oFormat.Foreground);
  Self.SetBackground(oFormat.Background);
  Self.SetItalic(oFormat.Italic);
  Self.SetBold(oFormat.Bold);
  Self.SetFontState(oFormat.State);
  Self.SetUnderline(oFormat.Underline);
  Self.SetStrikeThrough(oFormat.Strikethrough);
  Self.SetCapitalization(oFormat.Capitalization);
End;


Destructor TOdtTextFormat.Destroy;
Begin
  Inherited;
End;


Function TOdtTextFormat.Matches(Const oFormat: TWPSFontDetails; Const bIsStrict : Boolean): Boolean;
Begin
  { Strict means exact match, while non-strict means no conflict }
  If bIsStrict Then
    Result := (oFormat.Size = Self.WPFontSize)
        And (oFormat.Foreground = Self.WPForeground)
        And (oFormat.Background = Self.WPBackground)
        And SimpleTriStateMatches(oFormat.Italic, Self.WPItalic)
        And SimpleTriStateMatches(oFormat.Bold, Self.WPBold)
        And SimpleFontStateMatches(oFormat.State, Self.WPFontState)
        And SimpleTriStateMatches(oFormat.Underline, Self.WPUnderline)
        And SimpleTriStateMatches(oFormat.Strikethrough, Self.WPStrikeThrough)
        And SimpleCapsStateMatches(oFormat.Capitalization, Self.WPCapitalization)
  Else
  Begin
    Result := ((Not oFormat.HasSize) Or (oFormat.Size = Self.WPFontSize))
        And ((Not oFormat.HasForeground) Or (oFormat.Foreground = Self.WPForeground))
        And ((Not oFormat.HasBackground) Or (oFormat.Background = Self.WPBackground))
        And ((Not oFormat.HasItalic) Or SimpleTriStateMatches(oFormat.Italic, Self.WPItalic))
        And ((Not oFormat.HasBold) Or SimpleTriStateMatches(oFormat.Bold, Self.WPBold))
        And ((Not oFormat.HasState) Or SimpleFontStateMatches(oFormat.State, Self.WPFontState))
        And ((Not oFormat.HasUnderline) Or SimpleTriStateMatches(oFormat.Underline, Self.WPUnderline))
        And ((Not oFormat.HasStrikethrough) Or SimpleTriStateMatches(oFormat.Strikethrough, Self.WPStrikeThrough))
        And ((Not oFormat.HasCapitalization) Or SimpleCapsStateMatches(oFormat.Capitalization, Self.WPCapitalization))
  End;
End;


Function TOdtTextFormat.GetBold: TWPSTriState;
Begin
  If StringEquals(FWeight, 'normal') Then
    Result := tsFalse
  Else If StringEquals(FWeight, 'bold') Then
    Result := tsTrue
  Else
    Result := tsUnknown;
End;


Procedure TOdtTextFormat.SetBold(Const oBold: TWPSTriState);
Begin
  If oBold = tsTrue Then
    FWeight := 'bold'
  Else
    FWeight := 'normal';
End;


Function TOdtTextFormat.GetCapitalization: TWPSCapsState;
Begin
  If StringEquals(FTextTransform, 'capitalize') Then
    Result := fcsNormal        { not exactly right, but nearest match }
  Else If StringEquals(FTextTransform, 'lowercase') Then
    Result := fcsNoCaps
  Else If StringEquals(FTextTransform, 'uppercase') Then
    Result := fcsAllCaps
  Else
    Result := fcsUnknown;
End;


Procedure TOdtTextFormat.SetCapitalization(Const oCapitalize: TWPSCapsState);
Begin
  Case oCapitalize Of
    fcsNoCaps:  FTextTransform := 'lowercase';
    fcsAllCaps: FTextTransform := 'uppercase';
    Else        FTextTransform := 'none';
    { fcsNormal is not matched with capitalize }
  End;
End;


Function TOdtTextFormat.GetFontState: TWPSFontState;
Begin
  If StringEquals(FPosition, 'super') Then
    Result := fsSuperscript
  Else If StringEquals(FPosition, 'sub') Then
    Result := fsSubscript
  Else
    Result := fsUnknown;
End;


Procedure TOdtTextFormat.SetFontState(Const oFontstate: TWPSFontState);
Begin
  Case oFontstate Of
  fsSubscript:          FPosition := 'sub';
  fsSuperscript:        FPosition := 'super'
  Else                  FPosition := '';
  End;
End;


Function TOdtTextFormat.GetItalic: TWPSTriState;
Begin
  If StringEquals(FStyle, 'italic') Then
    Result := tsTrue
  Else If StringEquals(FStyle, 'normal') Then
    Result := tsFalse
  Else
    Result := tsUnknown;
End;


Procedure TOdtTextFormat.SetItalic(Const oItalic: TWPSTriState);
Begin
  If oItalic = tsTrue Then
    FStyle := 'italic'
  Else
    FStyle := 'normal';
End;


Function TOdtTextFormat.GetStrikeThrough: TWPSTriState;
Begin
  If (StringEquals(FLineThroughType, 'none') Or (FLineThroughType = ''))
    And (StringEquals(FLineThroughStyle, 'none') Or (FLineThroughStyle = '')) Then
    Result := tsFalse
  Else
    Result := tsTrue;
End;


Procedure TOdtTextFormat.SetStrikeThrough(Const oStrikethrough: TWPSTriState);
Begin
  If oStrikethrough = tsTrue Then
  Begin
    FLineThroughType := 'single';
    FLineThroughStyle := 'solid';
  End
  Else
  Begin
    FLineThroughType := 'none';
    FLineThroughStyle := 'none';
  End;
End;


Function TOdtTextFormat.GetUnderline: TWPSTriState;
Begin
  If (StringEquals(FUnderlineType, 'none') Or (FUnderlineType = ''))
     And (StringEquals(FUnderlineStyle, 'none') Or (FUnderlineStyle = '')) Then
    Result := tsFalse
  Else
    Result := tsTrue;
End;


Procedure TOdtTextFormat.SetUnderline(Const oUnderline: TWPSTriState);
Begin
  If oUnderline = tsTrue Then
  Begin
    FUnderlineType := 'single';
    FUnderlineStyle := 'solid';
  End
  Else
  Begin
    FUnderlineType := 'none';
    FUnderlineStyle := 'none';
  End;
End;

Function TOdtTextFormat.GetBackground: TColour;
Begin
  Result := FBackground;
End;


Procedure TOdtTextFormat.SetBackground(Const color: TColour);
Begin
  FBackground := color;
End;


Function TOdtTextFormat.GetForeground: TColour;
Begin
  If FUseWindowColor Then
    Result := DEF_COLOUR
  Else
    Result := FColor;
End;


Procedure TOdtTextFormat.SetForeground(Const color: TColour);
Begin
  If color = DEF_COLOUR Then
  Begin
    FUseWindowColor := True;
    FColor := DEF_COLOUR;
  End
  Else
  Begin
    FUseWindowColor := False;
    FColor := color;
  End;
End;

Function TOdtTextFormat.Link: TOdtTextFormat;
Begin
  Result := TOdtTextFormat(Inherited Link);
End;


Function TOdtTextFormat.GetFontSize: Integer;
Begin
  If (Length(FSize) > 2) And StringEquals(StringCopy(FSize, Length(FSize) - 1, 2), 'pt') Then
    Result := StringToInteger32(StringCopy(FSize, 0, Length(FSize) - 2))
  Else
    Result := 11; // FIXME: default font size
End;


Procedure TOdtTextFormat.SetFontSize(Const iValue: Integer);
Begin
  FSize := IntegerToString(iValue) + 'pt';
End;

Function TOdtTextFormat.HasBackground: Boolean;
Begin
  Result := FBackground <> DEF_COLOUR;
End;

Function TOdtTextFormat.HasColor: Boolean;
Begin
  Result := FColor <> DEF_COLOUR;
End;

Function TOdtTextFormat.HasFamily: Boolean;
Begin
  Result := FFamily <> '';
End;

Function TOdtTextFormat.HasLineThroughStyle: Boolean;
Begin
  Result := FLineThroughStyle <> '';
End;

Function TOdtTextFormat.HasLineThroughType: Boolean;
Begin
  Result := FLineThroughType <> '';
End;

Function TOdtTextFormat.HasName: Boolean;
Begin
  Result := FName <> '';
End;

Function TOdtTextFormat.HasPosition: Boolean;
Begin
  Result := FPosition <> '';
End;

Function TOdtTextFormat.HasSize: Boolean;
Begin
  Result := FSize <> '';
End;

Function TOdtTextFormat.HasStyle: Boolean;
Begin
  Result := FStyle <> '';
End;

Function TOdtTextFormat.HasTextTransform: Boolean;
Begin
  Result := FTextTransform <> '';
End;

Function TOdtTextFormat.HasUnderlineStyle: Boolean;
Begin
  Result := FUnderlineStyle <> '';
End;

Function TOdtTextFormat.HasUnderlineType: Boolean;
Begin
  Result := FUnderlineType <> '';
End;

Function TOdtTextFormat.HasUseWindowColor: Boolean;
Begin
  Result := True;
End;

Function TOdtTextFormat.HasWeight: Boolean;
Begin
  Result := FWeight <> '';
End;

function TOdtTextFormat.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FFamily.length * sizeof(char)) + 12);
  inc(result, (FSize.length * sizeof(char)) + 12);
  inc(result, (FStyle.length * sizeof(char)) + 12);
  inc(result, (FWeight.length * sizeof(char)) + 12);
  inc(result, (FPosition.length * sizeof(char)) + 12);
  inc(result, (FUnderlineType.length * sizeof(char)) + 12);
  inc(result, (FUnderlineStyle.length * sizeof(char)) + 12);
  inc(result, (FLineThroughType.length * sizeof(char)) + 12);
  inc(result, (FLineThroughStyle.length * sizeof(char)) + 12);
  inc(result, (FTextTransform.length * sizeof(char)) + 12);
end;

{ TOdtParagraphFormat }

Constructor TOdtParagraphFormat.Create;
Begin
  Inherited;

  Self.FTextAlign := '';
  Self.FMarginLeft := '';
  Self.FMarginRight := '';
  Self.FMarginBottom := '';
  Self.FBreakBefore := '';
  Self.FBreakBefore := '';
  Self.FBreakAfter := '';
End;


Constructor TOdtParagraphFormat.Create(Const oParent: TOdtParagraphFormat);
Begin
  Inherited Create;

  Self.FTextAlign := oParent.FTextAlign;

  Self.FMarginLeft := oParent.FMarginLeft;
  Self.FMarginRight := oParent.FMarginRight;
  Self.FMarginBottom := oParent.FMarginBottom;
  Self.FBreakBefore := oParent.FBreakBefore;
  Self.FBreakAfter := oParent.FBreakAfter;
End;


Constructor TOdtParagraphFormat.Create(Const oFormat: TWPSParagraphDetails);
Begin
  Inherited Create;

  Self.SetAlign(oFormat.Align);
  Self.SetMarginLeft(oFormat.LeftIndent);
  Self.SetMarginRight(oFormat.RightIndent);
  Self.SetMarginBottom(oFormat.MarginBottom);
End;


Destructor TOdtParagraphFormat.Destroy;
Begin
  Inherited;
End;


Function TOdtParagraphFormat.Link: TOdtParagraphFormat;
Begin
  Result := TOdtParagraphFormat(Inherited Link);
End;


Function TOdtParagraphFormat.Matches(Const oFormat: TWPSParagraphDetails): Boolean;
Begin
  Result := (oFormat.Align = Self.WPAlignt)
        And (oFormat.LeftIndent = Self.WPLeftIndent)
        And (oFormat.RightIndent = Self.WPRightIndent)
        And (oFormat.MarginBottom = Self.WPMarginBottom);
End;


Function TOdtParagraphFormat.GetAlign: TWordProcessorParagraphAlignment;
Begin
  If StringEquals(FTextAlign, 'center') Then
    Result := WordProcessorParagraphAlignmentCentre
  Else If StringEquals(FTextAlign, 'justify') Then
    Result := WordProcessorParagraphAlignmentJustify
  Else If StringEquals(FTextAlign, 'right') Then
    Result := WordProcessorParagraphAlignmentRight
  Else
    Result := WordProcessorParagraphAlignmentUnknown;
End;


Procedure TOdtParagraphFormat.SetAlign(Const aAlign: TWordProcessorParagraphAlignment);
Begin
  Case aAlign Of
  WordProcessorParagraphAlignmentCentre:        FTextAlign := 'center';
  WordProcessorParagraphAlignmentJustify:       FTextAlign := 'justify';
  WordProcessorParagraphAlignmentRight:         FTextAlign := 'right'
  Else                                          FTextAlign := 'left';
  End;
End;


Function TOdtParagraphFormat.GetMarginBottom: Integer;
Begin
  // FIXME: default indent
  Result := LengthCSS2ToPixel(FMarginBottom, 1);
End;


Procedure TOdtParagraphFormat.SetMarginBottom(Const aValue: Integer);
Begin
  // FIXME: default indent
  FMarginBottom := LengthPixelToCSS2(aValue, '0px');
End;


Function TOdtParagraphFormat.GetMarginLeft: Integer;
Begin
  // FIXME: default indent
  Result := LengthCSS2ToPixel(FMarginLeft, 1);
End;


Procedure TOdtParagraphFormat.SetMarginLeft(Const aValue: Integer);
Begin
  // FIXME: default indent
  FMarginLeft := LengthPixelToCSS2(aValue, '0px');
End;


Function TOdtParagraphFormat.GetMarginRight: Integer;
Begin
  // FIXME: default indent
  Result := LengthCSS2ToPixel(FMarginRight, 1);
End;


Procedure TOdtParagraphFormat.SetMarginRight(Const aValue: Integer);
Begin
  // FIXME: default indent
  FMarginRight := LengthPixelToCSS2(aValue, '0px');
End;


Function TOdtParagraphFormat.IsBreakAfter: Boolean;
Begin
  Result := (FBreakBefore = '') And (FBreakAfter <> '');
End;

Function TOdtParagraphFormat.IsBreakBefore: Boolean;
Begin
  Result := (FBreakBefore <> '') And (FBreakAfter = '');
End;


function TOdtParagraphFormat.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FTextAlign.length * sizeof(char)) + 12);
  inc(result, (FMarginLeft.length * sizeof(char)) + 12);
  inc(result, ( FMarginRight.length * sizeof(char)) + 12);
  inc(result, ( FMarginBottom.length * sizeof(char)) + 12);
  inc(result, (FBreakBefore.length * sizeof(char)) + 12);
  inc(result, ( FBreakAfter.length * sizeof(char)) + 12);
end;

{ TOdtStyle }

Constructor TOdtStyle.Create;
Begin
  Inherited;

  FTextFormat := Nil;
  FParagraphFormat := Nil;
  FListFormat := Nil;
End;

Constructor TOdtStyle.Create(Const oParent: TOdtStyle);
Begin
  Create;

  If oParent <> Nil Then
  Begin

    FFamily := oParent.FFamily;
    FStyleType := oParent.FStyleType;

    If oParent.HasTextFormat Then
      FTextFormat := TOdtTextFormat.Create(oParent.FTextFormat);

    If oParent.HasParagraphFormat Then
      FParagraphFormat := TOdtParagraphFormat.Create(oParent.FParagraphFormat);

    If oParent.HasListFormat Then
      FListFormat := TOdtListFormat.Create(oParent.FListFormat);
  End
End;

Destructor TOdtStyle.Destroy;
Begin
  FTextFormat.Free;
  FParagraphFormat.Free;
  FListFormat.Free;
  FTableFormat.Free;
  FRowFormat.Free;
  FCellFormat.Free;

  Inherited;
End;


Function TOdtStyle.HasListFormat: Boolean;
Begin
  Result := FListFormat <> Nil;
End;

Function TOdtStyle.GetListFormat: TOdtListFormat;
Begin
  If Self = Nil Then
    Result := Nil
  Else
    Result := FListFormat;
End;

Procedure TOdtStyle.SetListFormat(oFormat: TOdtListFormat);
Begin
  FListFormat.Free;
  FListFormat := oFormat;
End;


Function TOdtStyle.HasParagraphFormat: Boolean;
Begin
  Result := FParagraphFormat <> Nil;
End;

Function TOdtStyle.GetParaFormat: TOdtParagraphFormat;
Begin
  If Self = Nil Then
    Result := Nil
  Else
    Result := FParagraphFormat;
End;

Procedure TOdtStyle.SetParaFormat(oFormat: TOdtParagraphFormat);
Begin
  FParagraphFormat.Free;
  FParagraphFormat := oFormat;
End;


Function TOdtStyle.HasTextFormat: Boolean;
Begin
  Result := FTextFormat <> Nil;
End;

Function TOdtStyle.GetTextFormat: TOdtTextFormat;
Begin
  If Self = Nil Then
    Result := Nil
  Else
    Result := FTextFormat;
End;

Procedure TOdtStyle.SetTextFormat(oFormat: TOdtTextFormat);
Begin
  FTextFormat.Free;
  FTextFormat := oFormat;
End;

Function TOdtStyle.Link: TOdtStyle;
Begin
  Result := TOdtStyle(Inherited Link);
End;


Function TOdtStyle.HasCellFormat: Boolean;
Begin
  Result := FCellFormat <> Nil;
End;

Function TOdtStyle.GetCellFormat: TOdtTableCellFormat;
Begin
  If Self = Nil Then
    Result := Nil
  Else
    Result := FCellFormat;
End;

Procedure TOdtStyle.SetCellFormat(oFormat: TOdtTableCellFormat);
Begin
  FCellFormat.Free;
  FCellFormat := oFormat;
End;


Function TOdtStyle.HasRowFormat: Boolean;
Begin
  Result := FRowFormat <> Nil;
End;

Function TOdtStyle.GetRowFormat: TOdtTableRowFormat;
Begin
  If Self = Nil Then
    Result := Nil
  Else
    Result := FRowFormat;
End;

Procedure TOdtStyle.SetRowFormat(oFormat: TOdtTableRowFormat);
Begin
  FRowFormat.Free;
  FRowFormat := oFormat;
End;


Function TOdtStyle.HasTableFormat: Boolean;
Begin
  Result := FTableFormat <> Nil;
End;

Function TOdtStyle.GetTableFormat: TOdtTableFormat;
Begin
  If Self = Nil Then
    Result := Nil
  Else
    Result := FTableFormat;
End;

Procedure TOdtStyle.SetTableFormat(oFormat: TOdtTableFormat);
Begin
  FTableFormat.Free;
  FTableFormat := oFormat;
End;

Function TOdtStyle.MatchesListFormat(Const oFormat: TWPSParagraphDetails;  Const iCount: Integer): Boolean;
Begin
  Result := HasListFormat;
  If Result Then
    Result := Self.ListFormat.Matches(oFormat, iCount);
End;


Function TOdtStyle.MatchesParaFormat(Const oFormat: TWPSParagraphDetails): Boolean;
Begin
  Result := HasParagraphFormat;
  If Result Then
    Result := Self.ParagraphFormat.Matches(oFormat);
End;


Function TOdtStyle.MatchesTextFormat(Const oFormat: TWPSFontDetails; Const bIsStrict : Boolean): Boolean;
Begin

  Result := HasTextFormat;
  If Result Then
    Result := Self.TextFormat.Matches(oFormat, bIsStrict)
  Else If Not bIsStrict Then
    Result := True;
End;


Function TOdtStyle.MatchesTableFormat(Const oTable: TWPWorkingDocumentTableStartPiece): Boolean;
Begin
  Result := HasTableFormat;
  If Result Then
    Result := FTableFormat.Matches(oTable);
End;


Function TOdtStyle.MatchesTableRowFormat(Const oRow: TWPWorkingDocumentTableRowStartPiece): Boolean;
Begin
  Result := HasRowFormat;
  If Result Then
    Result := FRowFormat.Matches(oRow);
End;


Function TOdtStyle.MatchesTableCellFormat(Const oCell: TWPWorkingDocumentTableCellStartPiece): Boolean;
Begin
  Result := HasCellFormat;
  If Result Then
    Result := FCellFormat.Matches(oCell);
End;


function TOdtStyle.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FTextFormat.sizeInBytes);
  inc(result, FParagraphFormat.sizeInBytes);
  inc(result, FListFormat.sizeInBytes);
  inc(result, FTableFormat.sizeInBytes);
  inc(result, FRowFormat.sizeInBytes);
  inc(result, FCellFormat.sizeInBytes);
end;

{ TOdtStyleList }

Function TOdtStyleList.GetStyle(iIndex: Integer): TOdtStyle;
Begin
  Result := TOdtStyle(ObjectByIndex[iIndex]);
End;

Function TOdtStyleList.ItemClass: TFslObjectClass;
Begin
  Result := TOdtStyle;
End;

Function TOdtStyleList.Link: TOdtStyleList;
Begin
  Result := TOdtStyleList(Inherited Link);
End;

{ TOdtStyleStack }

Function TOdtStyleStack.Empty: Boolean;
Begin
  Result := Count = 0;
End;

Function TOdtStyleStack.Peek: TOdtStyle;
Begin
  If count = 0 Then
    Result := Nil
  Else
    Result := Styles[Count - 1];
End;

Procedure TOdtStyleStack.Pop;
Begin
  Assert(CheckCondition(Count > 0, 'Pop', 'Stack is empty'));

  DeleteByIndex(Count - 1);
End;

Procedure TOdtStyleStack.Push(oStyle: TOdtStyle);
Begin
  Add(oStyle);
End;

{ TOdtListLevelFormat }

Constructor TOdtListLevelFormat.Create;
Begin
  Inherited;

  FBulletType := tbUnknown;
  FNumberStyle := '';
  FNumberFormat := '';
  FStartValue := '';
End;

Constructor TOdtListLevelFormat.Create(Const oParent: TOdtListLevelFormat);
Begin
  Inherited Create;

  If oParent <> Nil Then
  Begin
    FBulletType := oParent.FBulletType;
    FNumberStyle := oParent.FNumberStyle;
    FNumberFormat := oParent.FNumberFormat;
    FStartValue := oParent.FStartValue;
  End;
End;


Constructor TOdtListLevelFormat.Create(Const oFormat: TWPSParagraphDetails);
Begin
  Inherited Create;

  FstartValue := IntegerToString(oFormat.FixedNumber);
  Self.FBulletType := oFormat.BulletType;
  Self.SetNumberStyle(oFormat.NumberType);
  Self.SetNumberFormat(oFormat.NumberFormat);
End;


Destructor TOdtListLevelFormat.Destroy;
Begin
  Inherited;
End;


Function TOdtListLevelFormat.Link: TOdtListLevelFormat;
Begin
  Result := TOdtListLevelFormat(Inherited Link);
End;


Function TOdtListLevelFormat.GetListType: TWPSParagraphListType;
Begin
  If FBulletType <> tbUnknown Then
    Result := WPSParagraphListTypeBullets
  Else If (FNumberStyle <> '') Or (FNumberFormat <> '') Then
    Result := WPSParagraphListTypeNumbers
  Else
    Result := WPSParagraphListTypeNone;
End;


Procedure TOdtListLevelFormat.SetStartNumber(Const iNum: Integer);
Begin
  FStartValue := IntegerToString(iNum);
End;


Function TOdtListLevelFormat.GetNumber(Const iIndex: Integer): Integer;
Begin
  Result := StrToIntDef(FStartValue, 1) + iIndex;
End;


Procedure TOdtListLevelFormat.SetNumberFormat(Const oFormat: TWPSParagraphNumberFormat);
Begin
  Case oFormat Of
  nwDot:                FNumberFormat := '.';
  nwSlash:              FNumberFormat := '/';
  nwParenthesis:        FNumberFormat := ')';
  nwColon:              FNumberFormat := ':';
  nwSemiColon:          FNumberFormat := ';';
  Else                  FNumberFormat := '';    // None and Unknown
  End;
End;


Function TOdtListLevelFormat.GetNumberFormat: TWPSParagraphNumberFormat;
Begin
  If FNumberFormat = '.' Then
    Result := nwDot
  Else If FNumberFormat = '/' Then
    Result := nwSlash
  Else If FNumberFormat = ')' Then
    Result := nwParenthesis
  Else If FNumberFormat = ':' Then
    Result := nwColon
  Else If FNumberFormat = ';' Then
    Result := nwSemiColon
  Else
    Result := nwUnknown;
End;


Procedure TOdtListLevelFormat.SetNumberStyle(Const oType: TWPSParagraphNumberType);
Begin
  Case oType Of
  tnLowerAlpha:         FNumberStyle := 'a';
  tnUpperAlpha:         FNumberStyle := 'A';
  tnLowerRoman:         FNumberStyle := 'i';
  tnUpperRoman:         FNumberStyle := 'I';
  Else                  FNumberStyle := '1';
  End;
End;


Function TOdtListLevelFormat.GetNumberStyle: TWPSParagraphNumberType;
Begin
  If FNumberStyle = '1' Then
    Result := tnArabic
  Else If FNumberStyle = 'i' Then
    Result := tnLowerRoman
  Else If FNumberStyle = 'I' Then
    Result := tnUpperRoman
  Else If FNumberStyle = 'a' Then
    Result := tnLowerAlpha
  Else If FNumberStyle = 'A' Then
    Result := tnUpperAlpha
  Else
    Result := tnUnknown;
End;

Procedure TOdtListLevelFormat.ApplyStyle(oParaFormat: TWPSParagraphDetails;
  Const iItemCount: Integer);
Begin
  oParaFormat.ListType := Self.GetListType;
  If oParaFormat.ListType = WPSParagraphListTypeBullets Then
    oParaFormat.BulletType := Self.BulletType
  Else
  Begin
    oParaFormat.NumberType := Self.GetNumberStyle;
    oParaFormat.NumberFormat := Self.GetNumberFormat;
    oParaFormat.FixedNumber := Self.GetNumber(iItemCount);
  End;
End;


Function TOdtListLevelFormat.Matches(Const oFormat: TWPSParagraphDetails; Const iCount: Integer): Boolean;
Begin
  Result := (oFormat.ListType = Self.WPListType);
  If Result Then
  Begin
    If oFormat.ListType = WPSParagraphListTypeBullets Then
      Result := oFormat.BulletType = Self.BulletType
    Else
      Result := (oFormat.NumberType = Self.WPNumberStyle)
                And (oFormat.NumberFormat = Self.WPNumberFormat)
                And (oFormat.FixedNumber = Self.GetNumber(iCount));
  End;
End;


function TOdtListLevelFormat.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FNumberStyle.length * sizeof(char)) + 12);
  inc(result, (FNumberFormat.length * sizeof(char)) + 12);
  inc(result, (FStartValue.length * sizeof(char)) + 12);
end;

{ TOdtListFormat }

Constructor TOdtListFormat.Create;
Begin
  Inherited;
End;

Constructor TOdtListFormat.Create(Const oParent: TOdtListFormat);
Var
  iIndex: Integer;
  oLevelFormat: TOdtListLevelFormat;
Begin
  Inherited Create;

  FName := oParent.FName;

  // copy the level formats
  For iIndex := 1 To oParent.Count - 1 Do
  Begin
    oLevelFormat := TOdtListLevelFormat.Create(oParent[iIndex]);
    Try
      Self.Add(oLevelFormat.Link);
    Finally
      oLevelFormat.Free;
    End;
  End;
End;

Constructor TOdtListFormat.Create(Const sName: String;  Const oFormat: TWPSParagraphDetails);
Var
  oLevelFormat: TOdtListLevelFormat;
Begin
  Inherited Create;

  FName := sName;

  // add the list style as list level 1
  oLevelFormat := TOdtListLevelFormat.Create(oFormat);
  Try
    Self.Add(oLevelFormat.Link);
  Finally
    oLevelFormat.Free;
  End;
End;

Function TOdtListFormat.Link: TOdtListFormat;
Begin
  Result := TOdtListFormat(Inherited Link);
End;

Destructor TOdtListFormat.Destroy;
Begin
  Inherited;
End;

Function TOdtListFormat.GetLevelFormat(iIndex: Integer): TOdtListLevelFormat;
Begin
  Result := TOdtListLevelFormat(ObjectByIndex[iIndex]);
End;

Function TOdtListFormat.ItemClass: TFslObjectClass;
Begin
  Result := TOdtListLevelFormat;
End;


Procedure TOdtListFormat.ApplyStyle(oParaFormat: TWPSParagraphDetails;
  Const iLevel, iItemCount: Integer);
Begin
  Assert(ExistsByIndex(iLevel));
  Self[iLevel].ApplyStyle(oParaFormat, iItemCount);
End;


Function TOdtListFormat.Matches(Const oFormat: TWPSParagraphDetails; Const iCount: Integer): Boolean;
Begin
  // Our WP only have 1 level
  Result := ExistsByIndex(0);
  If Result Then
    Result := Self[0].Matches(oFormat, iCount);
End;

function TOdtListFormat.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
end;

{ TOdtTableFormat }

Constructor TOdtTableFormat.Create;
Begin
  Inherited;

  Self.BackgroundColor := DEF_COLOUR;
End;

Constructor TOdtTableFormat.Create(Const oFormat: TOdtTableFormat);
Begin
  Create;

  Self.FMarginLeft := oFormat.FMarginLeft;
  Self.FMarginRight := oFormat.FMarginRight;
  Self.FMarginTop := oFormat.FMarginTop;
  Self.FMarginBottom := oFormat.FMarginBottom;
  Self.FBackgroundColor := oFormat.FBackgroundColor;
End;

Constructor TOdtTableFormat.Create(Const oTable: TWPWorkingDocumentTableStartPiece);
Begin
  Create;

  FBackgroundColor := oTable.Background;

  SetMarginLeft(oTable.HorizontalMargin);
  SetMarginRight(oTable.HorizontalMargin);
  SetMarginTop(oTable.VerticalMargin);
  SetMarginBottom(oTable.VerticalMargin);
End;


Function TOdtTableFormat.Link: TOdtTableFormat;
Begin
  Result := TOdtTableFormat(Inherited Link);
End;

Destructor TOdtTableFormat.Destroy;
Begin
  Inherited;
End;

Function TOdtTableFormat.GetMarginBottom: Integer;
Begin
  // FIXME: default indent
  Result := LengthCSS2ToPixel(FMarginBottom, 0);
End;

Function TOdtTableFormat.GetMarginLeft: Integer;
Begin
  // FIXME: default indent
  Result := LengthCSS2ToPixel(FMarginLeft, 0);
End;

Function TOdtTableFormat.GetMarginRight: Integer;
Begin
  // FIXME: default indent
  Result := LengthCSS2ToPixel(MarginRight, 0);
End;

Function TOdtTableFormat.GetMarginTop: Integer;
Begin
  // FIXME: default indent
  Result := LengthCSS2ToPixel(FMarginTop, 0);
End;

Procedure TOdtTableFormat.SetMarginBottom(Const aValue: Integer);
Begin
  // FIXME: default indent
  FMarginBottom := LengthPixelToCSS2(aValue, '0px');
End;

Procedure TOdtTableFormat.SetMarginLeft(Const aValue: Integer);
Begin
  // FIXME: default indent
  FMarginLeft := LengthPixelToCSS2(aValue, '0px');
End;

Procedure TOdtTableFormat.SetMarginRight(Const aValue: Integer);
Begin
  // FIXME: default indent
  FMarginRight := LengthPixelToCSS2(aValue, '0px');
End;

Procedure TOdtTableFormat.SetMarginTop(Const aValue: Integer);
Begin
  // FIXME: default indent
  FMarginTop := LengthPixelToCSS2(aValue, '0px');
End;


Procedure TOdtTableFormat.ApplyFormat(oTable: TWPWorkingDocumentTableStartPiece);
Begin
  oTable.HorizontalMargin := Self.WPHorizontalMargin;
  oTable.VerticalMargin := Self.WPVerticalMargin;
  oTable.Background := Self.BackgroundColor;
End;


Function TOdtTableFormat.Matches(Const oTable: TWPWorkingDocumentTableStartPiece): Boolean;
Begin
  Result := (oTable.Background = BackgroundColor)
    And (oTable.HorizontalMargin = WPHorizontalMargin)
    And (oTable.VerticalMargin = WPVerticalMargin);
End;


Function TOdtTableFormat.GetHorizontalMargin: Integer;
Begin
  Result := IntegerMax(WPMarginTop, WPMarginBottom);
End;

Procedure TOdtTableFormat.SetHorizontalMargin(Const aValue: Integer);
Begin
  Self.WPMarginTop := aValue;
  Self.WPMarginBottom := aValue;
End;


Function TOdtTableFormat.GetVerticalMargin: Integer;
Begin
  Result := IntegerMax(WPMarginLeft, WPMarginRight);
End;

Procedure TOdtTableFormat.SetVerticalMargin(Const aValue: Integer);
Begin
  Self.WPMarginLeft := aValue;
  Self.WPMarginRight := aValue;
End;


function TOdtTableFormat.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FMarginLeft.length * sizeof(char)) + 12);
  inc(result, ( FMarginRight.length * sizeof(char)) + 12);
  inc(result, (FMarginTop.length * sizeof(char)) + 12);
  inc(result, ( FMarginBottom.length * sizeof(char)) + 12);
end;

{ TOdtTableRowFormat }

Constructor TOdtTableRowFormat.Create;
Begin
  Inherited;
  FBackgroundColor := DEF_COLOUR;
End;


Constructor TOdtTableRowFormat.Create(Const oFormat: TOdtTableRowFormat);
Begin
  Create;

  FBackgroundColor := oFormat.FBackgroundColor;
End;


Constructor TOdtTableRowFormat.Create(Const oRow: TWPWorkingDocumentTableRowStartPiece);
Begin
  Create;

  BackgroundColor := oRow.Background;
End;


Function TOdtTableRowFormat.Link: TOdtTableRowFormat;
Begin
  Result := TOdtTableRowFormat(Inherited Link);
End;


Destructor TOdtTableRowFormat.Destroy;
Begin
  Inherited;
End;


Procedure TOdtTableRowFormat.ApplyFormat(oRow: TWPWorkingDocumentTableRowStartPiece);
Begin
  oRow.Background := Self.BackgroundColor;
End;


Function TOdtTableRowFormat.Matches(Const oRow: TWPWorkingDocumentTableRowStartPiece): Boolean;
Begin
  Result := (BackgroundColor = oRow.Background);
End;


{ TOdtTableCellFormat }

Constructor TOdtTableCellFormat.Create;
Begin
  Inherited;
End;

Constructor TOdtTableCellFormat.Create(Const oFormat: TOdtTableCellFormat);
Begin
  Create;

  FBackgroundColor := oFormat.FBackgroundColor;
  FBorderTop := oFormat.FBorderTop;
  FBorderBottom := oFormat.FBorderBottom;
  FBorderLeft := oFormat.FBorderLeft;
  FBorderRight := oFormat.FBorderRight;
End;

Constructor TOdtTableCellFormat.Create(Const oCell: TWPWorkingDocumentTableCellStartPiece);
Begin
  Create;

  FBackgroundColor := oCell.Background;
  WPBorderTop := oCell.TopBorder;
  WPBorderBottom := oCell.BottomBorder;
  WPBorderLeft := oCell.LeftBorder;
  WPBorderRight := oCell.RightBorder;
End;


Function TOdtTableCellFormat.Link: TOdtTableCellFormat;
Begin
  Result := TOdtTableCellFormat(Inherited Link);
End;


Destructor TOdtTableCellFormat.Destroy;
Begin
  Inherited;
End;


Function TOdtTableCellFormat.GetBorderBottom: TWPBorder;
Begin
  Result := BorderCSS2ToWPBorder(FBorderBottom);
End;

Procedure TOdtTableCellFormat.SetBorderBottom(oBorder: TWPBorder);
Begin
  FBorderBottom := WPBorderToBorderCSS2(oBorder);
End;


Function TOdtTableCellFormat.GetBorderLeft: TWPBorder;
Begin
  Result := BorderCSS2ToWPBorder(FBorderLeft);
End;

Procedure TOdtTableCellFormat.SetBorderLeft(oBorder: TWPBorder);
Begin
  FBorderLeft := WPBorderToBorderCSS2(oBorder);
End;


Function TOdtTableCellFormat.GetBorderRight: TWPBorder;
Begin
  Result := BorderCSS2ToWPBorder(FBorderRight);
End;

Procedure TOdtTableCellFormat.SetBorderRight(oBorder: TWPBorder);
Begin
  FBorderRight := WPBorderToBorderCSS2(oBorder);
End;


Function TOdtTableCellFormat.GetBorderTop: TWPBorder;
Begin
  Result := BorderCSS2ToWPBorder(FBorderTop);
End;

Procedure TOdtTableCellFormat.SetBorderTop(oBorder: TWPBorder);
Begin
  FBorderTop := WPBorderToBorderCSS2(oBorder);
End;


Procedure TOdtTableCellFormat.ApplyFormat(oCell: TWPWorkingDocumentTableCellStartPiece);
Begin
  oCell.Background := Self.BackgroundColor;
  oCell.TopBorder := Self.WPBorderTop;
  oCell.BottomBorder := Self.WPBorderBottom;
  oCell.LeftBorder := Self.WPBorderLeft;
  oCell.RightBorder := Self.WPBorderRight;
End;


Function TOdtTableCellFormat.Matches(Const oCell: TWPWorkingDocumentTableCellStartPiece): Boolean;
Var
  oBorder: TWPBorder;
  iIndex: Integer;
Begin
  Result := (BackgroundColor = oCell.Background);

  iIndex := 0;
  While Result And (iIndex < 4) Do
  Begin
    Case iIndex Of
    0:  oBorder := WPBorderTop;
    1:  oBorder := WPBorderBottom;
    2:  oBorder := WPBorderLeft;
    Else  oBorder := WPBorderRight;
    End;
    Try
      Result := oCell.TopBorder.IsCompatible(oBorder);
    Finally
      oBorder.Free;
      Inc(iIndex);
    End;
  End;
End;

function TOdtTableCellFormat.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FBorderTop.length * sizeof(char)) + 12);
  inc(result, ( FBorderBottom.length * sizeof(char)) + 12);
  inc(result, (FBorderLeft.length * sizeof(char)) + 12);
  inc(result, ( FBorderRight.length * sizeof(char)) + 12);
end;

End.
