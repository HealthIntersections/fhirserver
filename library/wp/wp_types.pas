Unit wp_types;

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
{$i fhir.inc}

Interface

Uses
  Windows,
  SysUtils, Classes, Graphics,
  fsl_base, fsl_utilities, fsl_collections, fsl_stream,
  wp_graphics;

Const
  TOUCH_ICON_HOTSPOT = 89;
  TOUCH_ICON_MODEL = 90;
  TOUCH_ICON_FIELD = 91;

  FASTDRAW_REDRAW = 1.5; // seconds
  FASTDRAW_LAG = 1; // seconds

  MAX_INTERNAL_WIDTH = 5200;

  MAX_WORD_LENGTH = 1023; // this is limited by a windows API call (or at least, the delphi wrapper thereof. But it improves performance, so we don't worry about it

  PAGINATION_DELAY = 1000; // 1 second, and then pagination kicks in
  MIN_PAGINATION_WIDTH = 6;

  DEFAULT_FONT_NAME = 'Verdana';
  DEFAULT_FONT_SIZE = 10;
  DEFAULT_FOREGROUND = clBlack;
  DEFAULT_BACKGROUND = clWhite;
  DEF_INT = -1;

  DEFAULT_BACKGROUND_FIELD_MASK = clAqua;
  DEFAULT_BACKGROUND_FIELD_VOCAB = clLime;
  DEFAULT_BACKGROUND_READONLY = TColour($EFEFEF);
  SCROLLBAR_WIDTH = 20;

  NESTED_TABLE_INDENT = 10;

  MIN_IMAGE_DRAG = 20;
  IMAGE_CORNER_HINT = 3;

  DEFAULT_SECTION_BOTTOM_MARGIN_POINTS = 0;
  DEF_ANNOTATION_WIDTH = 120;

  DEF_COLOUR_FIELD_ACTIVE = clGray; // ?? TColour($0080FF);
  DEFAULT_LOW_LIGHT_CONTRAST = 0.6;
  DEFAULT_NORMAL_CONTRAST = 1;
  BULLET_SCALE_FACTOR = 0.7;

  FIELD_START_CHAR = '[';
  FIELD_END_CHAR = ']';
  CHAR_PILCROW = #182;
  CHAR_SPACE_HINT = chr(183);
  CHAR_BREAK_HINT = #172;
  CHAR_IMAGE_HINT = #164;

  TAB_CHAR_COUNT = 6;

  DEFAULT_PARAGRAPH_MARGIN_TOP = 0;
  DEFAULT_PARAGRAPH_MARGIN_BOTTOM = 0;

  DEFAULT_CELL_MARGIN_LEFT = 6;
  DEFAULT_CELL_MARGIN_TOP = 4;
  DEFAULT_CELL_MARGIN_RIGHT = 6;
  DEFAULT_CELL_MARGIN_BOTTOM = 4;

  MIN_CELL_SPACE = 10;
  MIN_PRINT_WIDTH = 50;

  // if the word contains any of these letters, it is exempt from spell checking
  // the general concept is that numerical things aren't spell checked
  CHAR_SPELL_EXEMPT = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '''', '"', '`',
                       '_', '+', '=', '*', '$', '%', '@', '&', '#'];

  // these characters break a word, just like a space does
  CHAR_WORD_BREAK = [' ', '-', '/', '\'];

  // these letters are exempt from spell checking but don't exempt the word from being spellchecked
  CHAR_SPELL_EXCLUDE = ['!', ',', '.', ';', ':', '?', '(', ')', '{', '}', '[', ']', '<', '>'];

  ANNOTATION_LEFT_GAP = 15;
  ANNOTATION_LEFT_LINE_WIDTH = 1;
  ANNOTATION_LEFT_PADDING = 3;
  ANNOTATION_LEFT_STUB = 3;

  ANNOTATION_RIGHT_GAP = 6;
  ANNOTATION_RIGHT_LINE_WIDTH = 1;
  ANNOTATION_RIGHT_PADDING = 3;

  ANNOTATION_TOP_LINE_WIDTH = 1;
  ANNOTATION_TOP_PADDING = 3;

  ANNOTATION_BOTTOM_LINE_WIDTH = 1;
  ANNOTATION_BOTTOM_PADDING = 3;

  ANNOTATION_TOP_GAP = 2;
  ANNOTATION_GAP = 4;

  ANNOTATION_LEFT_DEADSPACE = ANNOTATION_LEFT_GAP + ANNOTATION_LEFT_LINE_WIDTH + ANNOTATION_LEFT_PADDING;
  ANNOTATION_RIGHT_DEADSPACE = ANNOTATION_RIGHT_GAP + ANNOTATION_RIGHT_LINE_WIDTH + ANNOTATION_RIGHT_PADDING;
  ANNOTATION_HORIZ_DEADSPACE = ANNOTATION_LEFT_DEADSPACE + ANNOTATION_RIGHT_DEADSPACE;


  // native Word Processor Format
  NAMESPACE_WP_NATIVE = 'urn:kestral.com.au:wp-native';
  VERSION_NATIVE_ONE = '1';
  VERSION_NATIVE_TWO = '2';
  VERSION_NATIVE_THREE = '3';
  TAG_NAME_STREAM = 'WordProcessor';
  TAG_NAME_DOCUMENT = 'Document';
  TAG_NAME_SECTION = 'Section';
  TAG_NAME_PARAGRAPH = 'Paragraph';
  TAG_NAME_FIELD = 'Field';
  TAG_NAME_LINEBREAK = 'Linebreak';
  TAG_NAME_TEXT = 'Text';
  TAG_NAME_CONTENT = 'Content';
  TAG_NAME_STYLE = 'Style';
  TAG_NAME_ANNOTATIONS = 'Annotations';
  TAG_NAME_ANNOTATION = 'Annotation';
  TAG_NAME_ATTACHMENTS = 'Attachments';
  TAG_NAME_ATTACHMENT = 'Attachment';
  TAG_NAME_TABLE = 'Table';
  TAG_NAME_TABLE_ROW = 'Row';
  TAG_NAME_TABLE_CELL = 'Cell';
  TAG_NAME_IMAGE = 'Image';
  TAG_NAME_DATA = 'Data';
  TAG_NAME_SELECTION = 'Selection';
  TAG_NAME_MAP = 'Map';
  TAG_NAME_AREA = 'Area';
  TAG_NAME_COORD = 'C';
  TAG_NAME_LEFTBORDER = 'LeftBorder';
  TAG_NAME_RIGHTBORDER = 'RightBorder';
  TAG_NAME_CENTERHORIZONTALBORDER = 'CenterHorizontalBorder';
  TAG_NAME_CENTERVERTICALBORDER = 'CenterVerticalBorder';
  TAG_NAME_TOPBORDER = 'TopBorder';
  TAG_NAME_BOTTOMBORDER = 'BottomBorder';
  TAG_NAME_BREAK = 'Break';
  TAG_NAME_BRUSH = 'Brush';
  TAG_NAME_ADORNMENT = 'Adornment';

  ATTR_NAME_XMLNS = 'xmlns';
  ATTR_NAME_VERSION = 'version';
  ATTR_NAME_ALLOWED_WORDS = 'allowed-words';
  ATTR_NAME_NAME = 'Name';
  ATTR_NAME_DATA = 'Data';
  ATTR_NAME_VALUE = 'Value';
  ATTR_NAME_ID = 'Id';
  ATTR_NAME_ANNOTATION = 'Annotation';
  ATTR_NAME_NAMESPACE = 'Namespace';
  ATTR_NAME_DISPLAYNAME = 'DisplayName';
  ATTR_NAME_READONLY = 'ReadOnly';
  ATTR_NAME_DELETABLE = 'Deletable';
  ATTR_NAME_ISFIELD = 'IsField';
  ATTR_NAME_MASK = 'Mask';
  ATTR_NAME_FIXEDFORMAT = 'FixedFormat';
  ATTR_NAME_VOCABULARY = 'Vocabulary';
  ATTR_NAME_URL = 'URL';
  ATTR_NAME_KEY = 'Key';
  ATTR_NAME_LINKCOLOUR = 'LinkColour';
  ATTR_NAME_HOVERCOLOUR = 'HoverColour';
  ATTR_NAME_STYLE = 'Style';
  ATTR_NAME_FONTNAME = 'FontName';
  ATTR_NAME_SIZE = 'Size';
  ATTR_NAME_BOLD = 'Bold';
  ATTR_NAME_ITALIC = 'Italic';
  ATTR_NAME_UNDERLINE = 'Underline';
  ATTR_NAME_STRIKETHROUGH = 'Strikethrough';
  ATTR_NAME_FONTSTATE = 'FontState';
  ATTR_NAME_FOREGROUND = 'Foreground';
  ATTR_NAME_BACKGROUND = 'Background';
  ATTR_NAME_ALIGN = 'Align';
  ATTR_NAME_ALIGNMENT = 'Alignment';
  ATTR_NAME_LEFTINDENT = 'LeftIndent';
  ATTR_NAME_RIGHTINDENT = 'RightIndent';
  ATTR_NAME_BULLETTYPE = 'BulletType';
  ATTR_NAME_LISTTYPE = 'ListType';
  ATTR_NAME_NUMBERTYPE = 'NumberType';
  ATTR_NAME_NUMBERFORMAT = 'NumberFormat';
  ATTR_NAME_FIXEDNUMBER = 'FixedNumber';
  ATTR_NAME_DOCUMENT = 'Document';
  ATTR_NAME_IMAGEREF = 'Reference';
  ATTR_NAME_IMAGEREF_SELECTION = 'SelectionReference';
  ATTR_NAME_BORDERWIDTH = 'BorderWidth';
  ATTR_NAME_BORDERCOLOR = 'BorderColor';
  ATTR_NAME_TRANSPARENTCOLOR = 'TransparentColor';
  ATTR_NAME_HEIGHT = 'Height';
  ATTR_NAME_WIDTH = 'Width';
  ATTR_NAME_DEFINED = 'Defined';
  ATTR_NAME_FANCY = 'Fancy';
  ATTR_NAME_COLOUR = 'Colour';
  ATTR_NAME_OUTERCOLOUR = 'OuterColour';
  ATTR_NAME_OUTERCOLOUR2 = 'OuterColour2';
  ATTR_NAME_HEADER = 'Header';
  ATTR_NAME_LOWER_PADDING_SIZE = 'LowerPaddingSize';
  ATTR_NAME_LOWER_PADDING_COLOUR = 'LowerPaddingColour';
  ATTR_NAME_SPAN = 'Span';
  ATTR_NAME_BORDERPOLICY = 'BorderPolicy';
  ATTR_NAME_BREAKTYPE = 'BreakType';
  ATTR_NAME_PENCOLOUR = 'PenColour';
  ATTR_NAME_PENWIDTH = 'PenWidth';
  ATTR_NAME_PENSTYLE = 'PenStyle';
  ATTR_NAME_PENENDSTYLE = 'EndStyle';
  ATTR_NAME_MARGIN_LEFT = 'MarginLeft';
  ATTR_NAME_MARGIN_RIGHT = 'MarginRight';
  ATTR_NAME_MARGIN_TOP = 'MarginTop';
  ATTR_NAME_MARGIN_BOTTOM = 'MarginBottom';
  ATTR_NAME_DISPLAYTYPE = 'DisplayType';
  ATTR_NAME_VERTICALALIGNMENT = 'VerticalAlignment';
  ATTR_NAME_FORMAT = 'Format';
  ATTR_NAME_HORIZONTAL_MARGIN = 'HorizMargin';
  ATTR_NAME_VERTICAL_MARGIN = 'VertMargin';
  ATTR_NAME_TITLE = 'Title';
  ATTR_NAME_X = 'x';
  ATTR_NAME_Y = 'y';
  ATTR_NAME_LOW_OUTER = 'LowOuter';
  ATTR_NAME_HIGH_OUTER = 'HighOuter';
  ATTR_NAME_CAPITALIZATION = 'Capitalization';
  ATTR_NAME_ANNOT_TEXT = 'AnnotationText';
  ATTR_NAME_ANNOT_AUDIT = 'AnnotationAudit';
  ATTR_NAME_ANNOT_COLOUR = 'AnnotationColour';
  ATTR_NAME_BREAK_BEFORE = 'BreakBefore';
  ATTR_NAME_RESET_STYLE = 'Reset';
  ATTR_NAME_DRAWNFONT = 'DrawnFont';
  ATTR_NAME_ADORN_TYPE = 'AdornmentType';
  ATTR_NAME_ROTATION = 'Rotation';
  ATTR_NAME_CAPTION = 'Caption';
  ATTR_NAME_CAPTION_X = 'CaptionX';
  ATTR_NAME_CAPTION_Y = 'CaptionY';
  ATTR_NAME_ROW_BACKGROUND = 'RowBackground';
  ATTR_NAME_TABLE_BACKGROUND = 'TableBackground';
  ATTR_NAME_CELL_BACKGROUND = 'CellBackground';
  ATTR_NAME_INDEX = 'Index';
  ATTR_NAME_EXPAND_LAST = 'ExpandLast';
  ATTR_NAME_SIZEPOLICY = 'SizePolicy';
  ATTR_NAME_FRAMEINDEX = 'FrameIndex';
  ATTR_NAME_MIMETYPE = 'MimeType';
  ATTR_NAME_EXTENSION = 'FileExtension';
  ATTR_NAME_COMPRESSED = 'Compressed';

  // extended CSS attribute name
//  CSS_ATTR_NAME = 'wpx-Name';
//  CSS_ATTR_DISPLAYNAME = 'wpx-DisplayName';
  CSS_ATTR_READONLY = 'wpx-ReadOnly';
  CSS_ATTR_DELETABLE = 'wpx-Deletable';
  CSS_ATTR_ISFIELD = 'wpx-IsField';
  CSS_ATTR_DATA = 'wpx-Data';
  CSS_ATTR_MASK = 'wpx-Mask';
  CSS_ATTR_FIXEDFORMAT = 'wpx-FixedFormat';
  CSS_ATTR_VOCABULARY = 'wpx-Vocabulary';
  CSS_ATTR_URL = 'wpx-URL';
  CSS_ATTR_KEY = 'wpx-Key';
  CSS_ATTR_LINKCOLOUR = 'wpx-LinkColour';
  CSS_ATTR_HOVERCOLOUR = 'wpx-HoverColour';
//  CSS_ATTR_STYLE = 'wpx-Style';
  CSS_ATTR_FONTNAME = 'font-family';
  CSS_ATTR_FONTSIZE = 'font-size';
//  CSS_ATTR_SIZE = 'wpx-Size';
//  CSS_ATTR_BOLD = 'wpx-Bold';
//  CSS_ATTR_ITALIC = 'wpx-Italic';
//  CSS_ATTR_UNDERLINE = 'wpx-Underline';
//  CSS_ATTR_STRIKETHROUGH = 'wpx-Strikethrough';
//  CSS_ATTR_FONTSTATE = 'wpx-FontState';
  CSS_ATTR_FOREGROUND = 'color';
  CSS_ATTR_BACKGROUND = 'background-color';
  CSS_ATTR_ALIGN = 'text-align';
//  CSS_ATTR_ALIGNMENT = 'wpx-Alignment';
//  CSS_ATTR_LEFTINDENT = 'wpx-LeftIndent';
//  CSS_ATTR_RIGHTINDENT = 'wpx-RightIndent';
  CSS_ATTR_LISTTYPE = 'list-style-type';
//  CSS_ATTR_LISTTYPE = 'wpx-ListType';
//  CSS_ATTR_NUMBERTYPE = 'wpx-NumberType';
//  CSS_ATTR_NUMBERFORMAT = 'wpx-NumberFormat';
//  CSS_ATTR_FIXEDNUMBER = 'wpx-FixedNumber';
//  CSS_ATTR_DOCUMENT = 'wpx-Document';
//  CSS_ATTR_IMAGEREF = 'wpx-Reference';
//  CSS_ATTR_IMAGEREF_SELECTION = 'wpx-SelectionReference';
//  CSS_ATTR_BORDERWIDTH = 'wpx-BorderWidth';
//  CSS_ATTR_BORDERCOLOR = 'wpx-BorderColor';
//  CSS_ATTR_TRANSPARENTCOLOR = 'wpx-TransparentColor';
  CSS_ATTR_HEIGHT = 'height';
  CSS_ATTR_WIDTH = 'width';
  CSS_ATTR_BORDERLEFT = 'border-left';
  CSS_ATTR_BORDERLEFT_EXT = 'wpx-border-left';
  CSS_ATTR_BORDERRIGHT = 'border-right';
  CSS_ATTR_BORDERRIGHT_EXT = 'wpx-border-right';
  CSS_ATTR_BORDERTOP = 'border-top';
  CSS_ATTR_BORDERTOP_EXT = 'wpx-border-top';
  CSS_ATTR_BORDERBOTTOM = 'border-bottom';
  CSS_ATTR_BORDERBOTTOM_EXT = 'wpx-border-bottom';
  CSS_ATTR_BORDER_VERTICAL_CENTRE = 'wpx-border-vertical-centre';
  CSS_ATTR_BORDER_VERTICAL_CENTRE_EXT = 'wpx-border-vertical-centre-ext';
  CSS_ATTR_BORDER_HORIZONTAL_CENTRE = 'wpx-border-horizontal-centre';
  CSS_ATTR_BORDER_HORIZONTAL_CENTRE_EXT = 'wpx-border-horizontal-centre-ext';
//  CSS_ATTR_DEFINED = 'wpx-Defined';
//  CSS_ATTR_FANCY = 'wpx-Fancy';
//  CSS_ATTR_COLOUR = 'wpx-Colour';
//  CSS_ATTR_OUTERCOLOUR = 'wpx-OuterColour';
//  CSS_ATTR_OUTERCOLOUR2 = 'wpx-OuterColour2';
  CSS_ATTR_HEADER = 'wpx-Header';
  CSS_ATTR_LOWER_PADDING_SIZE = 'wpx-LowerPaddingSize';
  CSS_ATTR_LOWER_PADDING_COLOUR = 'wpx-LowerPaddingColour';
  CSS_ATTR_SPAN = 'wpx-Span';
  CSS_ATTR_BORDERPOLICY = 'wpx-BorderPolicy';
  CSS_ATTR_BREAKTYPE = 'wpx-BreakType';
  CSS_ATTR_PENSTYLE = 'wpx-PenStyle';
  CSS_ATTR_PENENDSTYLE = 'wpx-EndStyle';
  CSS_ATTR_MARGIN_LEFT = 'margin-left';
  CSS_ATTR_MARGIN_RIGHT = 'margin-right';
  CSS_ATTR_MARGIN_TOP = 'margin-top';
  CSS_ATTR_MARGIN_BOTTOM = 'margin-bottom';
  CSS_ATTR_DISPLAYTYPE = 'wpx-DisplayType';
  CSS_ATTR_VERTICALALIGNMENT = 'wpx-VerticalAlignment';
//  CSS_ATTR_FORMAT = 'wpx-Format';
  CSS_ATTR_HORIZONTAL_MARGIN = 'wpx-HorizMargin';
  CSS_ATTR_VERTICAL_MARGIN = 'wpx-VertMargin';
  CSS_ATTR_TITLE = 'wpx-Title';
  CSS_ATTR_TEXT_TRANSFORM = 'text-transform';
//  CSS_ATTR_X = 'wpx-x';
//  CSS_ATTR_Y = 'wpx-y';
//  CSS_ATTR_LOW_OUTER = 'wpx-LowOuter';
//  CSS_ATTR_HIGH_OUTER = 'wpx-HighOuter';

  FIELD_DATA_NAME_LIST = 'List';
  FIELD_DATA_NAME_FORCE = 'Force';
  FIELD_DATA_NAME_SORT = 'Sort';

  FIELD_DATA_NAME_MAND = 'Mandatory';
  FIELD_DATA_NAME_TYPE = 'Type';
  FIELD_DATA_NAME_INT_MIN = 'IntMin';
  FIELD_DATA_NAME_INT_MAX = 'IntMax';
  FIELD_DATA_NAME_FLT_MAIN = 'FloatMain';
  FIELD_DATA_NAME_FLT_DECIMAL = 'FloatDecimal';
  FIELD_DATA_NAME_DATE_TIME = 'DateTime';
  FIELD_DATA_NAME_TEXT_REGEX = 'TextRegex';
  FIELD_DATA_NAME_TEXT_DESC = 'TextRegexDesc';
  FIELD_DATA_NAME_TEXT_MAX = 'TextMaxLength';
  FIELD_DATA_NAME_LIST_MODE = 'ListMode';

  FIELD_DATA_NAME_DATE_TIME_Prohibited = 'Prohibited';
  FIELD_DATA_NAME_DATE_TIME_Allowed = 'Allowed';
  FIELD_DATA_NAME_DATE_TIME_Required = 'Required';

  FIELD_DATA_NAME_LIST_MODE_Dropdown = 'Dropdown';
  FIELD_DATA_NAME_LIST_MODE_Optional = 'Dropdown / Optional';
  FIELD_DATA_NAME_LIST_MODE_Buttons = 'Buttons';

  // Some standard namespaces
  NAMESPACE_W3C_XSI = 'http://www.w3.org/2001/XMLSchema-instance';
  NAMESPACE_HL7V3 = 'urn:hl7-org:v3';
  NAMEPSACE_HL7V3_VOC = 'urn:hl7-org:v3/voc';

Function IsWordBreak(Const sWord : String) : Boolean;


Type
  EWPException = class (EFslException);

  TWPMouseAction = (
   // Mouse button is up
     msFree,  // mouse is free - "exploring" the document.
     // There may be an operation associated with where the mouse is (hover)
     // when mouse moves, potential actions may set cursor

     msNull,       // mouse is over an area that is not active in any way

   // mouse button is down
     msSelect,     // mouse is currently selecting content as it is moved around (crIBeam)
     msImageTool,  // mouse is over image, and current selected image tool applies
     msHotspot,    // mouse has gone down on a hotspot
     msDragBlock,  // mouse is dragging content to a destination (maybe sourced from outside the document)
     msDragEdge,   // mouse is dragging the edge of something (image/object/table) around
     msButton);    // the mouse went down on some mouse responsive object, and is owned by that object until the mouse comes up

  TAdornmentPart = (apAll, apPrimary, apCaption, apZoom);
  TAdornmentAction = (aaMove, aaDragTop, aaDragBottom, aaDragLeft, aaDragRight, aaDragTopLeft, aaDragTopRight, aaDragBottomLeft, aaDragBottomRight);
  TWPImageFormat = (ifUnknown, ifJPEG, ifPNG, ifBMP, ifICO, ifDicom, ifGIF, ifPDF);


  TWPShiftState = (wssShift, wssAlt, wssCtrl, wssLeft, wssRight, wssMiddle, wssDouble);
  TWPShiftStates = Set Of TWPShiftState;

  TChangeType = (ctListContents, ctTextContents, ctLayout, ctPresentation);

  // null states:
  // tristate = tsUnknown
  // string = ''
  // word = MAXWORD
  // colour = MAXINT
  // enumerations = ord(0)

  {
    Alignment of a break - null, left, centre, or right
  }
  TWordProcessorAlignment = (WordProcessorAlignmentUnknown, WordProcessorAlignmentLeft, WordProcessorAlignmentCentre, WordProcessorAlignmentRight);

  {
    Vertical Alignment of the content in a table cell - null, bottom, centred, or top
  }
  TWordProcessorVerticalAlignment = (VerticalAlignmentBottom, VerticalAlignmentCentered, VerticalAlignmentTop);

  {
    alignment of a paragraph - null, left, centre, right, and justify
  }
  TWordProcessorParagraphAlignment = (WordProcessorParagraphAlignmentUnknown, WordProcessorParagraphAlignmentLeft, WordProcessorParagraphAlignmentCentre,
                                      WordProcessorParagraphAlignmentRight, WordProcessorParagraphAlignmentJustify);

  {
    vertical alignment of an image in a paragraph - null, bottom, centre, top, or aligned to the base line of the characters (normal default)
  }
  TWordProcessorImageVerticalAlignment = (ImageVerticalAlignmentBaseLine, ImageVerticalAlignmentBottom, ImageVerticalAlignmentCentered, ImageVerticalAlignmentTop);

  {
    vertical alignment of an image in a paragraph - null, bottom, centre, top, or aligned to the base line of the characters (normal default)
  }
  TWordProcessorImageSizePolicy = (ImageSizeManual, ImageSizePageWidth, ImageSizeWholePage);

  TWPImageVerticalAlignment = TWordProcessorImageVerticalAlignment;

  {
    list type of a paragraph - null, none, bullets, or numbers
  }
  TWPSParagraphListType = (WPSParagraphListTypeUnknown, WPSParagraphListTypeNone, WPSParagraphListTypeBullets, WPSParagraphListTypeNumbers);

  {
    unknown (whatever default would otherwise apply), true, or false
  }
  TWPSTriState = (tsUnknown, tsTrue, tsFalse);

  {
    whether a font is superscripted/subscripted
  }
  TWPSFontState = (fsUnknown, fsNormal, fsSuperscript, fsSubscript);

  {
    Whether a caps state is applied to the font
  }
  TWPSCapsState = (fcsUnknown, fcsNormal, fcsAllCaps, fcsNoCaps, fcsSmallCaps);

  {
    If the paragraph is a bulleted list, what the bullet is
  }
  TWPSParagraphBulletType = (tbUnknown, tbDisc, tbCircle, tbSquare);

  {
    if the paragraph is in a numbered list, what the number type is
  }
  TWPSParagraphNumberType = (tnUnknown, tnArabic, tnLowerAlpha, tnUpperAlpha, tnLowerRoman, tnUpperRoman);

  {
    if the paragraph is in a numbered list, how the number is shown
  }
  TWPSParagraphNumberFormat = (nwUnknown, nwNone, nwDot, nwSlash, nwParenthesis, nwColon, nwSemiColon);

Const
  DEF_WORD = 65535;
  DEF_ENUM = 0;
  DEF_STRING = '';
  DEF_COLOUR = MaxInt;

  NAMES_WPSTRISTATE : Array [TWPSTriState] Of String = ('unknown', 'true', 'false');
  NAMES_WPSFONTSTATE : Array [TWPSFontState] Of String = ('Unknown', 'Normal', 'Superscript', 'Subscript');
  NAMES_WPSCAPSSTATE : Array [TWPSCapsState] Of String = ('Unknown', 'Normal', 'AllCaps', 'NoCaps', 'SmallCaps');
  NAMES_WPSALIGNMENT : Array [TWordProcessorAlignment] Of String = ('Unknown', 'Left', 'Middle', 'Right');
  NAMES_WPSPARAGRAPHALIGNMENT : Array [TWordProcessorParagraphAlignment] Of String = ('Unknown', 'Left', 'Middle', 'Right', 'Justify');
  NAMES_WPSPARAGRAPHBULLETTYPE : Array [TWPSParagraphBulletType] Of String = ('Unknown', 'Disc', 'Circle', 'Square');
  NAMES_WPSPARAGRAPHNUMBERTYPE : Array [TWPSParagraphNumberType] Of String = ('Unknown', 'Arabic', 'LowerAlpha', 'UpperAlpha', 'LowerRoman', 'UpperRoman');
  NAMES_WPSPARAGRAPHNUMBERFORMAT : Array [TWPSParagraphNumberFormat] Of String = ('Unknown', 'None', 'Dot', 'Slash', 'Parenthesis', 'Colon', 'SemiColon');
  NAMES_WPSPARAGRAPHLISTTYPE : Array [TWPSParagraphListType] Of String = ('Unknown', 'None', 'bullets', 'numbers');

  TITLES_WPSPARAGRAPHBULLETTYPE : Array [TWPSParagraphBulletType] Of String = ('', 'Discs', 'Circles', 'Squares');
  TITLES_WPSPARAGRAPHNUMBERTYPE : Array [TWPSParagraphNumberType] Of String = ('', 'Numbers', 'Letters (a,b,c..)', 'Capital Letters (A,B,C..)', 'Roman Numerals (i,ii,iii...)', 'Capital Roman Numerals (I,II,III...)');
  TITLES_WPSPARAGRAPHNUMBERFORMAT : Array [TWPSParagraphNumberFormat] Of String = ('', 'Plain: N', 'Dot: N.', 'Slash: N/', 'Parenthesis: (N)', 'Colon: N:', 'SemiColon: N;');
  TITLES_WPSPARAGRAPHLISTTYPE : Array [TWPSParagraphListType] Of String = ('', 'No Bullets or Numbers', 'Bullets', 'Numbers');

  NAMES_WORDPROCESSORVERTICALALIGNMENT : Array[TWordProcessorVerticalAlignment] Of String = ('VerticalAlignmentBottom', 'VerticalAlignmentCentered', 'VerticalAlignmentTop');
  NAMES_WORDPROCESSORIMAGEVERTICALALIGNMENT : Array[TWordProcessorImageVerticalAlignment] Of String = ('ImageVerticalAlignmentBaseLine', 'ImageVerticalAlignmentBottom', 'ImageVerticalAlignmentCentered', 'ImageVerticalAlignmentTop');
  NAMES_WORDPROCESSORIMAGESIZEPOLICY  : Array[TWordProcessorImageSizePolicy] Of String = ('ImageSizeManual', 'ImageSizePageWidth', 'ImageSizeWholePage');


Type
  {
    Font Style details
  }
   TWPSFontDetails = Class(TFslObject)
    Private
      FName : String;
      FSize : Integer;
      FForeground : TColour;
      FBackground : TColour;

      FBold : TWPSTriState;
      FState : TWPSFontState;
      FItalic : TWPSTriState;
      FUnderline : TWPSTriState;
      FStrikethrough : TWPSTriState;
      FCapitalization : TWPSCapsState;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;

      Function Link : TWPSFontDetails; Overload;
      Function Clone : TWPSFontDetails; Overload;

      Procedure Assign(oSource : TFslObject); Override;

      Procedure Apply(oFont : TFont; bDefaults : Boolean = true); Overload; Virtual;
      Procedure Copy(oFont : TFont); Overload; Virtual;

      Function Describe : String;

      Procedure Clear;
      Procedure Defaults;

      Procedure DefaultSize;

      Function Matches(oOther : TWPSFontDetails) : Boolean;
      Procedure Merge(oOther : TWPSFontDetails);
      Procedure Update(oOther : TWPSFontDetails);

      Procedure SetBold(Const bValue : Boolean);

      Function HasName : Boolean;
      Function HasSize : Boolean;
      Function HasBold : Boolean;
      Function HasItalic : Boolean;
      Function HasUnderline : Boolean;
      Function HasStrikethrough : Boolean;
      Function HasState : Boolean;
      Function HasForeground : Boolean;
      Function HasBackground  : Boolean;
      Function HasCapitalization : Boolean;

  published
      {
        The font name. null/blank means to use the default font of the context
      }
      Property Name : String Read FName Write FName;
      {
        the size of the font. Value of DEF_WORD (65535) means to use the default font size
      }
      Property Size : Integer Read FSize Write FSize;
      {
        Whether the font is bold or not
      }
      Property Bold : TWPSTriState Read FBold Write FBold;
      {
        whether the font is italic or not
      }
      Property Italic : TWPSTriState Read FItalic Write FItalic;
      {
        whether the font is underlined or not
      }
      Property Underline : TWPSTriState Read FUnderline Write FUnderline;
      {
        whether the font is strikethrough or not
      }
      Property Strikethrough : TWPSTriState Read FStrikethrough Write FStrikethrough;
      {
        whether the font is superscirpt/subscript
      }
      Property State : TWPSFontState Read FState Write FState;
      {
        foreground colour for the font
      }
      Property Foreground : TColour Read FForeground Write FForeground;
      {
        background colour for the font
      }
      Property Background  : TColour Read FBackground  Write FBackground;
      {
        Caps State of the font
      }
      Property Capitalization : TWPSCapsState Read FCapitalization Write FCapitalization;
  End;

  {
    Paragraph Style details
  }
  TWPSParagraphDetails = Class(TFslObject)
    Private
      FAlign : TWordProcessorParagraphAlignment;
      FBulletType : TWPSParagraphBulletType;
      FNumberType : TWPSParagraphNumberType;
      FListType : TWPSParagraphListType;

      FFixedNumber : Word;
      FNumberFormat : TWPSParagraphNumberFormat;

      FLeftIndent : Integer;
      FRightIndent : Integer;
      FMarginBottom : Integer;

    Public
      constructor Create; Override;

      Function Link : TWPSParagraphDetails; Overload;
      Function Clone : TWPSParagraphDetails; Overload;

      Procedure Assign(oSource : TFslObject); Override;

      Procedure Clear;
      Procedure Defaults;

      Function Matches(oOther : TWPSParagraphDetails) : Boolean;
      Procedure Merge(oOther : TWPSParagraphDetails);
      Function Update(oOther : TWPSParagraphDetails) : TChangeType;

      Function HasValues : Boolean;

      Function HasAlign : Boolean;
      Function HasLeftIndent : Boolean;
      Function HasRightIndent : Boolean;
      Function HasMarginBottom : Boolean;
      Function HasBulletType : Boolean;
      Function HasListType : Boolean;
      Function HasNumberType : Boolean;
      Function HasNumberFormat : Boolean;
      Function HasFixedNumber : Boolean;

      Procedure AlignNone;
      Procedure AlignLeft;
      Procedure AlignRight;
      Procedure AlignCentre;
      Procedure AlignJustify;

      Procedure BulletTypeNone;
      Procedure BulletTypeDisc;
      Procedure BulletTypeCircle;
      Procedure BulletTypeSquare;

      Function Describe : String;
    published
      {
        Paragraph alignment of the paragraph
      }
      Property Align : TWordProcessorParagraphAlignment Read FAlign Write FAlign;
      {
        Left indent for the paragraph
      }
      Property LeftIndent : Integer Read FLeftIndent Write FLeftIndent;
      {
        RIght indent for the paragraph
      }
      Property RightIndent : Integer Read FRightIndent Write FRightIndent;
      {
        Empty space gap at the end of the paragraph
      }
      Property MarginBottom : Integer Read FMarginBottom Write FMarginBottom;
      {
        whether the paragraph is in a list (and whether it is numbered or bulleted)
      }
      Property ListType : TWPSParagraphListType Read FListType Write FListType;
      {
        What bullet to use if the paragraph is in a bulleted list
      }
      Property BulletType : TWPSParagraphBulletType Read FBulletType Write FBulletType;
      {
        The number type of the list if in a numbered list
      }
      Property NumberType : TWPSParagraphNumberType Read FNumberType Write FNumberType;
      {
        The number format is in a numbered list
      }
      Property NumberFormat : TWPSParagraphNumberFormat Read FNumberFormat Write FNumberFormat;
      {
        if <> -1, then the list is renumbered at this point to the specified number
      }
      Property FixedNumber : Word Read FFixedNumber Write FFixedNumber;
  End;


  {
    Style details.

    If Paragraph is not null, the paragraph has a paragraph aspect
   }
  TWPStyle = Class(TFslName)
    Private
      FFont : TWPSFontDetails;
      FParagraph : TWPSParagraphDetails;
      FResetOnNewParagraph: Boolean;

      Function GetFont : TWPSFontDetails;
      Procedure SetFont(Const Value : TWPSFontDetails);

      Function GetParagraph : TWPSParagraphDetails;
      Procedure SetParagraph(Const Value : TWPSParagraphDetails);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TWPStyle; Overload;
      Function Clone : TWPStyle; Overload;

      Procedure Assign(oSource : TFslObject); Override;

      Function WorkingFontName(oFont : TWPSFontDetails) : String;
      Function WorkingFontSize(oFont : TWPSFontDetails) : Integer;
      Function WorkingFontBold(oFont : TWPSFontDetails) : TWPSTriState;
      Function WorkingFontItalic(oFont : TWPSFontDetails) : TWPSTriState;
      Function WorkingFontUnderline(oFont : TWPSFontDetails) : TWPSTriState;
      Function WorkingFontStrikethrough(oFont : TWPSFontDetails) : TWPSTriState;
      Function WorkingFontState(oFont : TWPSFontDetails) : TWPSFontState;
      Function WorkingFontForeground(oFont : TWPSFontDetails) : TColour;
      Function WorkingFontBackground (oFont : TWPSFontDetails; aDefault : TColour) : TColour;
      Function WorkingFontCapitalization(oFont : TWPSFontDetails) : TWPSCapsState;

      Function WorkingParagraphAlign(oParagraph : TWPSParagraphDetails) : TWordProcessorParagraphAlignment;
      Function WorkingParagraphLeftIndent(oParagraph : TWPSParagraphDetails) : Integer;
      Function WorkingParagraphRightIndent(oParagraph : TWPSParagraphDetails) : Integer;
      Function WorkingParagraphMarginBottom(oParagraph : TWPSParagraphDetails) : Integer;
      Function WorkingParagraphBulletType(oParagraph : TWPSParagraphDetails) : TWPSParagraphBulletType;
      Function WorkingParagraphListType(oParagraph : TWPSParagraphDetails) : TWPSParagraphListType;
      Function WorkingParagraphNumberType(oParagraph : TWPSParagraphDetails) : TWPSParagraphNumberType;
      Function WorkingParagraphNumberFormat(oParagraph : TWPSParagraphDetails) : TWPSParagraphNumberFormat;

      Function HasParagraphAspect : Boolean; Overload; Virtual;

    published
      {$IFDEF FORDOCO}
      {
        The name of the font
      }
      Property Name : String read GetName write SetName;
      {$ENDIF}
      {
        The font aspects of the style
      }
      Property Font : TWPSFontDetails Read GetFont Write SetFont;
      {
        The paragraph aspects of the style. If this is not null, then the style is applied to the paragraph as a whole
      }
      Property Paragraph : TWPSParagraphDetails Read GetParagraph Write SetParagraph;

      {
        True if the style reverts to normal when the user creates a new paragraph
      }
      Property ResetOnNewParagraph : Boolean read FResetOnNewParagraph Write FResetOnNewParagraph;
  End;


Const
  CHANGE_TYPE_LIST_CONTENTS = 1;
  CHANGE_TYPE_TEXT_CONTENTS = 1;
  CHANGE_TYPE_LAYOUT = 1;
  CHANGE_TYPE_PRESENTATION = 1;

Type

  TWPTrackable = Class;

  TChangeEvent = Procedure(aType : TChangeType; oSource : TWPTrackable) Of Object;

  TWPTrackable = Class(TFslObject)
    Private
      FOnChange: TChangeEvent;
    Public
      destructor Destroy; Override;

      Procedure Change(aType : TChangeType; oSource : TWPTrackable); Overload; Virtual;

      Procedure Hook(aEvent : TChangeEvent);
      Procedure UnHook(aEvent : TChangeEvent);
  End;

  TWPTrackableList = Class (TFslObjectList)
    Private
      FOnChange: TChangeEvent;
      FHooking : Boolean;

      Function GetElement(Const iIndex : Integer) : TWPTrackable;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPTrackable);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPTrackable; Reintroduce; Overload; Virtual;

      Procedure InternalAfterInclude(iIndex : Integer; oObject : TFslObject); Override;
      Procedure InternalBeforeExclude(iIndex : Integer; oObject : TFslObject); Override;

    Public
      constructor Create; Overload; Override;
      constructor Create(bHooking : Boolean); Overload; Virtual;
      destructor Destroy; Override;

      Function Link : TWPTrackableList; Overload;
      Function Clone : TWPTrackableList; Overload;
      Procedure Assign(oSource : TFslObject); Overload; Override;

      Procedure Change(aType : TChangeType; oSource : TWPTrackable); Overload; Virtual;

      Function New : TWPTrackable; Reintroduce; Overload; Virtual;
      Procedure Hook(aEvent : TChangeEvent);
      Procedure UnHook(aEvent : TChangeEvent);

      Property Hooking : Boolean Read FHooking Write FHooking; // must be set on construction
      Property Elements[Const iIndex : Integer] : TWPTrackable Read GetElement Write SetElement; Default;
  End;

  TProcedureEvent = Procedure Of Object;

  TPredefinedBorderStyles = (pbsNone, pbsSimpleLine, pbsSimpleDot, pbsSimpleFancy, pbsCustom, pbsCustomFancy);

  // defined - overrides values set in container.
  // if you don't want a border, ste style to apsNone
  {
    A border in a table

    defined - overrides values set in container. If you don't want a border, ste style to apsNone
  }
  TWPBorder = Class (TWPTrackable)
  Private

    FDefined : Boolean; //  whether there is a border at all
    FColour : TColour; // The colour for the border
    FWidth : Integer; // The width of the border
    FStyle : TFslPenStyle; // The style of pen for simple borders
    FFancy : Boolean; // True if the border is a fancy border (width should be > 6)
    FBrushImage : TFslBitmapGraphic; // replaces colour (only when fancy is true)
    FOuterColour : TColour; // The outer color for blending (fancy only)
    FOuterColour2 : TColour; // The outer color for non-blending areas (outside limits) (fancy only)
    FLowOuterlimit : Word; // if the border is fancy, this is how far from the lower limit to draw the outside.
    FHighOuterLimit : Word;// if the border is fancy, this is how far from the higher limit to draw the outside.

    Procedure SetDefined(Const Value : Boolean);
    Procedure SetFancy(Const Value : Boolean);
    Procedure SetOuterColour(Const Value : TColour);
    Procedure SetOuterColour2(Const Value : TColour);
    Procedure SetColour(Const Value : TColour);
    Procedure SetWidth(Const Value : Integer);
    Procedure SetStyle(Const Value : TFslPenStyle);
    Procedure SetBrushImage(Const Value : TFslBitmapGraphic);
    Procedure SetLowOuterlimit(Const Value : Word);
    Procedure SetHighOuterLimit(Const Value : Word);

    Function MapToPredefinedBorderStyle: TPredefinedBorderStyles;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;

    Function Link : TWPBorder; Overload;
    Function Clone : TWPBorder; Overload;

    Procedure Assign(oSource : TFslObject); Override;

    Function IsCompatible(oBorder: TWPBorder): Boolean; Overload; Virtual;

    Procedure Clear; Overload; Virtual;
    Procedure SimpleLine; Overload; Virtual;
    Procedure SimpleDot; Overload; Virtual;
    Procedure SimpleFancy; Overload; Virtual;

    Function IsSimpleLine: Boolean; Overload; Virtual;
    Function IsSimpleDot: Boolean; Overload; Virtual;
    Function IsSimpleFancy: Boolean; Overload; Virtual;

    Function ActualWidth : Integer;

    Function PropertyDescription: String;

    Property BorderStyle: TPredefinedBorderStyles Read MapToPredefinedBorderStyle;

    Property Fancy : Boolean Read FFancy Write SetFancy;
    Property OuterColour : TColour Read FOuterColour Write SetOuterColour;
    Property OuterColour2 : TColour Read FOuterColour2 Write SetOuterColour2;
    Property BrushImage : TFslBitmapGraphic Read FBrushImage Write SetBrushImage;
    Property LowOuterlimit : Word Read FLowOuterlimit Write SetLowOuterlimit;
    Property HighOuterLimit : Word Read FHighOuterLimit Write SetHighOuterLimit;
  Published
    {
      Whether the border is defined or not. If not defined, the logical border inherits defaults from elsewhere. I.e. the left cell of a row has a default left border of the row left border
    }
    Property Defined : Boolean Read FDefined Write SetDefined;
    {
      the colour of the border
    }
    Property Colour : TColour Read FColour Write SetColour;
    {
      the width of the border
    }
    Property Width : Integer Read FWidth Write SetWidth;
    {
      The style which which the border is drawn
    }
    Property Style : TFslPenStyle Read FStyle Write SetStyle;
  End;

Const
  PREDEFINED_BODERSTYLE_NAMES : Array [TPredefinedBorderStyles] Of String =('- None -', 'Simple Line', 'Simple Dot', 'Simple Fancy', 'Custom', 'Custom Fancy');

Type
  {
    An URL Link
  }
  TWPHotspot = Class(TWPTrackable)
    Private
      FURL : String;
      FKey : String;
      FLinkColour : TColour;
      FHoverColour : TColour;
      FTitle : String;
      FLinkUnderline: Boolean;

      Procedure SetKey(Const Value: String);
      Procedure SetHoverColour(Const Value: TColour);
      Procedure SetLinkColour(Const Value: TColour);
      Procedure SetURL(Const Value: String);
      Procedure SetTitle(Const Value: String);
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      constructor Create(Const sURL : String); Overload; Virtual;
      constructor Create(Const sURL : String; iLinkColour : TColour; iHoverColour : TColour); Overload; Virtual;
      constructor Create(Const sURL, sKey : String; iLinkColour : TColour; iHoverColour : TColour); Overload; Virtual;
      destructor Destroy; Override;

      Procedure AfterConstruction; Override;

      Function Link : TWPHotspot; Overload;
      Function Clone : TWPHotspot; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;
      Procedure Defaults; Overload; Virtual;

      function describe : string;
      Function IsEmpty : Boolean;

      Function WorkingColour(oFocus : TWPHotspot) : TColour;

      Property Key : String Read FKey Write SetKey;
  Published
      {
        The actual URL for the link
      }
      Property URL : String Read FURL Write SetURL;
      {
        The popup title for the link (html only)
      }
      Property Title : String Read FTitle Write SetTitle;
      {
        The colour with which the link is drawn
      }
      Property LinkColour : TColour Read FLinkColour Write SetLinkColour;
      {
        The colour of the link when the user holds the mouse over the link
      }
      Property HoverColour : TColour Read FHoverColour Write SetHoverColour;
      {
        whether the link is underlined or not
      }
      Property LinkUnderline : Boolean Read FLinkUnderline Write FLinkUnderline;
  End;


  TWPHotspotList = Class (TWPTrackableList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPHotspot;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPHotspot);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPHotspot; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPHotspotList; Overload;
      Function Clone : TWPHotspotList; Overload;

      Function New : TWPHotspot; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPHotspot Read GetElement Write SetElement; Default;
  End;

Type
  TWPCoordinate = Class(TWPTrackable)
    Private
      FX : Integer;
      FY : Integer;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      Function Link : TWPCoordinate; Overload;
      Function Clone : TWPCoordinate; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;
      Function AsPoint : TPoint;

      Property X : Integer Read FX Write FX;
      Property Y : Integer Read FY Write FY;

  End;


  TWPCoordinateList = Class (TWPTrackableList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPCoordinate;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPCoordinate);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPCoordinate; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPCoordinateList; Overload;
      Function Clone : TWPCoordinateList; Overload;

      Procedure Add(x, y : Integer); Overload; Virtual;

      Function New : TWPCoordinate; Reintroduce; Overload; Virtual;

      Function Contains(iX, iY : Integer):Boolean;
      Function LastIs(iX, iY : Integer):Boolean;

      Property Elements[Const iIndex : Integer] : TWPCoordinate Read GetElement Write SetElement; Default;
  End;

  TWPDataItemMap = Class (TFslStringMatch)
    Public
      constructor Create; Override;

      Function Link : TWPDataItemMap; Overload;
      Function Clone : TWPDataItemMap; Overload;
  End;

  TWPImageMapArea = Class(TWPHotspot)
    Private
      FCoordinates : TWPCoordinateList;
      FSelected : Boolean;
    Procedure SetSelected(Const Value: Boolean);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPImageMapArea; Overload;
      Function Clone : TWPImageMapArea; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Function Contains(iX, iY : Integer):Boolean;

      Property Coordinates : TWPCoordinateList Read FCoordinates;
      Property Selected : Boolean Read FSelected Write SetSelected;
  End;


  TWPImageMapAreaList = Class (TWPHotspotList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPImageMapArea;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPImageMapArea);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPImageMapArea; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPImageMapAreaList; Overload;
      Function Clone : TWPImageMapAreaList; Overload;

      Function New : TWPImageMapArea; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPImageMapArea Read GetElement Write SetElement; Default;
  End;


  TWPImageMap = Class (TWPTrackable)
    Private
      FAreas : TWPImageMapAreaList;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Override;

      Function Link : TWPImageMap; Overload;
      Function Clone : TWPImageMap; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Function ContainsHotspot(oHotspot : TWPHotspot) : Boolean;
      Function HasSelection : Boolean;
      Function Selection : TWPImageMapArea;
      Function GetAreaByURL(Const sURL : String): TWPImageMapArea;
      Procedure Select(oArea : TWPImageMapArea);

      Property Areas : TWPImageMapAreaList Read FAreas;
  End;


  TWPDocumentImageAdornmentType = (iatLine, iatRectangle, iatCircle, iatZoom, iatMark);

  TWPDocumentImageAdornment = Class (TFslObject)
    private
      FId : String;

      FAdornmentType : TWPDocumentImageAdornmentType;
      FPenColour : TColour;
      FPenWidth : Word;
      FPenStyle : TFslPenStyle;
      FCoordinates : TWPCoordinateList;

      FCaptionPoint : TWPCoordinate;
      FFont : TWPSFontDetails;
      FCaption : String;
  protected
    function sizeInBytesV : cardinal; override;
    public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TWPDocumentImageAdornment; Overload;
      Function Clone : TWPDocumentImageAdornment; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;
      Procedure Move(aPart : TAdornmentPart; aAction : TAdornmentAction; iX, iY : Integer); Overload;
      Property Id : String read FId;

      function Describe : String;

     {
        The type of the adornment: either iatLine, iatShape, iatZoom, or iatMark
      }
      Property AdornmentType : TWPDocumentImageAdornmentType read FAdornmentType write FAdornmentType;

      {
        The font of the paragraph (and it's bullets/numbers if applicable). - can override what is specified in the style
      }
      Property Font : TWPSFontDetails Read FFont;

      {
        the coordinates for the adornment - either a list, or a set amount for the different shapes
      }
      Property Coordinates : TWPCoordinateList read FCoordinates;

      {
        the colour of the break - if drawn
      }
      Property PenColour : TColour Read FPenColour Write FPenColour;
      {
        the vertical height (= pen width) of the break - if drawn
      }
      Property PenWidth : Word Read FPenWidth Write FPenWidth;
      {
        the stle of the break - if drawn
      }
      Property PenStyle : TFslPenStyle Read FPenStyle Write FPenStyle;

      {
        Point at which caption displays
      }
      Property CaptionPoint : TWPCoordinate read FCaptionPoint;

      {
        Optional caption to display with the adornment
      }
      Property Caption : String Read FCaption Write FCaption;
  End;

  TWPDocumentImageAdornments = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentImageAdornment;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentImageAdornment);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentImageAdornment; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentImageAdornments; Overload;
      Function Clone : TWPDocumentImageAdornments; Overload;

      Function New : TWPDocumentImageAdornment; Reintroduce; Overload; Virtual;

      Function GetById(Const sId : String) : TWPDocumentImageAdornment;
      Function IndexById(Const sId : String) : Integer;

      Property Elements[Const iIndex : Integer] : TWPDocumentImageAdornment Read GetElement Write SetElement; Default;
  End;

Const
  IMAGE_ADORNMENT_TYPE_CODES : Array [TWPDocumentImageAdornmentType] Of String = ('Line', 'Rectangle', 'Circle', 'Zoom', 'Mark');

Type
  TEdgeAction = (eaLeft, eaRight, eaTop, eaBottom, eaLeftTop, eaRightTop, eaLeftBottom, eaRightBottom);

  TWPMouseInfo = Class (TFslObject)
    Private
      FOffset : Integer;
      FHotspot : TWPHotspot;
      FProposedAction : TWPMouseAction;
      FEdgeAction : TEdgeAction;
      FSubject: TFslObject;
      FAdornment: TWPDocumentImageAdornment;
      FAdornmentPart: TAdornmentPart;
      FAdornmentAction: TAdornmentAction;
      FHint: String;
      FHintError: Boolean;
      Procedure SetHotspot(Const Value: TWPHotspot);
      Procedure SetSubject(Const Value: TFslObject);
      procedure SetAdornment(const Value: TWPDocumentImageAdornment);
  protected
    function sizeInBytesV : cardinal; override;
    Public
      destructor Destroy; Override;

      Function Link : TWPMouseInfo; Overload;

      Property ProposedAction : TWPMouseAction Read FProposedAction Write FProposedAction;
      Property Offset : Integer Read FOffset Write FOffset;
      Property Hotspot : TWPHotspot Read FHotspot Write SetHotspot;
      Property Subject : TFslObject Read FSubject Write SetSubject;
      Property EdgeAction : TEdgeAction Read FEdgeAction Write FEdgeAction;
      Property HintText : String read FHint write FHint;
      Property HintError : Boolean read FHintError write FHintError;

      Property Adornment : TWPDocumentImageAdornment read FAdornment write SetAdornment;
      Property AdornmentPart : TAdornmentPart read FAdornmentPart write FAdornmentPart;
      Property AdornmentAction : TAdornmentAction read FAdornmentAction write FAdornmentAction;
  End;

Type
  TWPWordIterator = Class (TFslObject)
    Private
      FText : String;
      FCursor : Integer;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      Procedure Init(Const sText : String); Overload; Virtual;
      Function More : Boolean; Overload; Virtual;
      Function Next : String; Overload; Virtual;
  End;


  TWPLineIterator = class (TFslObject)
    Private
      FText : String;
      FCursor : Integer;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      Procedure Init(const sText : String); Overload; Virtual;
      Function More : Boolean; Overload; Virtual;
      Function Next : String; Overload; Virtual;
  End;

  {
    Styles defined for the document
  }
  TWPStyles = Class(TFslNameList)
    Private
      FDefaultStyle : TWPStyle;

      Function GetStyleByIndex(Const iIndex : Integer) : TWPStyle;
      Procedure SetStyleByIndex(Const iIndex : Integer; oStyle : TWPStyle);

      Function GetDefaultStyle : TWPStyle;  Virtual;
      Procedure SetDefaultStyle(oStyle : TWPStyle); Virtual;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TWPStyles; Overload;
      Function Clone : TWPStyles; Overload;


      Function HasDefaultStyle : Boolean; // usually we would have a default style, but it is possible to delete it
      Procedure UseSystemDefaultStyle; // add the default default style

      Function Add(Const sName : String):TWPStyle; Overload; Virtual;

      Property StyleByIndex[Const iIndex : Integer] : TWPStyle Read GetStyleByIndex write SetStyleByIndex; Default;

    {
      Add a Row to the end of the list.
    }
    Function Append : TWPStyle;
    {
      Add an already existing Row to the end of the list.
    }
    Procedure AddItem(value : TWPStyle);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TWPStyle) : Integer;
    {
       Insert Row before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TWPStyle;
    {
       Insert an existing Row before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TWPStyle);
    {
       Get the iIndexth Row. (0 = first item)
    }
    Function Item(iIndex : Integer) : TWPStyle;
    {
       Set the iIndexth Row. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TWPStyle);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;

    {
      Add a style with a name
    }
    Function AddNamed(Const sName : String):TWPStyle; Overload; Virtual;

    {
      Merge the styles from another document with this document
    }
    Procedure MergeStyles(oStyles : TWPStyles; bUpdateExisting : Boolean);

    {
      Get a style with the given name, if one exists
    }
    Function GetByName(Const sName : String) : TWPStyle; Reintroduce;

    {
      Get a style with the given name, if one exists. Otherwise return the default style
    }

    Function GetByNameOrDefault(Const sName : String) : TWPStyle;

    {
      Default style for the document. Do not set to null
    }
    Property DefaultStyle : TWPStyle Read GetDefaultStyle Write SetDefaultStyle;

  End;

  TWPStyleStack = Class(TWPStyles)
    Public
      Function Empty : Boolean;

      Function Peek : TWPStyle;

      Procedure Push(oStyle : TWPStyle);
      Procedure Pop;
  End;

  TWordProcessorStyle = TWPStyle;


Const
  DEFAULT_STYLE_NAME = 'Default';


Function ToTriState(bValue : Boolean):TWPSTriState;


Function WordToText(iValue : Word) : String;
Function TextToWord(Const sValue : String) : Word;


Type
  TWPCompletionItemCode = String;
  TWPCompletionItemDescription = String;
  TWPCompletionItemContent = String;

  TWPCompletionItem = Class(TFslObject)
    Private
      FCode : TWPCompletionItemCode;
      FDescription : TWPCompletionItemDescription;
      FContent : TWPCompletionItemContent;
      FId: Integer;

    Protected
      Function ErrorClass : EFslExceptionClass; Overload; Override;

    Public
      Function Link : TWPCompletionItem; Overload;
      Function Clone : TWPCompletionItem; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Function Display : String; Overload; Virtual;

      // used for internal identification by the host application
      Property Id : Integer Read FId Write FId;

      // code is what the user types and what matching is based on
      Property Code : TWPCompletionItemCode Read FCode Write FCode;

      // description is shown appended to the code in the drop down browser
      Property Description : TWPCompletionItemDescription Read FDescription Write FDescription;

      // plain text content to insert. if this is blank, then the WP will retrieve
      // the content from the host
      Property Content : TWPCompletionItemContent Read FContent Write FContent;
  End;

  EWPCompletionItem = Class(EFslException)
  End;

  TWPCompletionItems = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPCompletionItem;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPCompletionItem);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByCode(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByDescription(pA, pB : Pointer) : Integer; Overload; Virtual;

      Function Get(Const aValue : Integer) : TWPCompletionItem; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPCompletionItems; Overload;
      Function Clone : TWPCompletionItems; Overload;

      Function New : TWPCompletionItem; Reintroduce; Overload; Virtual;

      Function Add(sCode : String; sContent : String; sDescription : String = '') : TWPCompletionItem; Overload; Virtual;

      Function IndexByCode(Const aValue : TWPCompletionItemCode) : Integer; Overload; Virtual;
      Function IndexByDescription(Const aValue : TWPCompletionItemDescription) : Integer; Overload; Virtual;

      Function GetByCode(Const aValue : TWPCompletionItemCode) : TWPCompletionItem; Overload; Virtual;
      Function GetByDescription(Const aValue : TWPCompletionItemDescription) : TWPCompletionItem; Overload; Virtual;

      Function ExistsByCode(Const aValue : TWPCompletionItemCode) : Boolean; Overload; Virtual;
      Function ExistsByDescription(Const aValue : TWPCompletionItemDescription) : Boolean; Overload; Virtual;

      Procedure SortedByCode; Overload; Virtual;
      Procedure SortedByDescription; Overload; Virtual;

      Function IsSortedByCode : Boolean; Overload; Virtual;
      Function IsSortedByDescription : Boolean; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPCompletionItem Read GetElement Write SetElement; Default;
  End;

Type
  TWPObserver = Class (TFslObject)
  Private
    FEvent: TNotifyEvent;
    FId: TObject;
    Function GetId : TObject;
  Public
    Function Link : TWPObserver; Overload;
    Function Clone : TWPObserver; Overload;

    Property Id : TObject Read GetId Write FId;
    Property Event : TNotifyEvent Read FEvent Write FEvent;
  End;

  TWPObservers = Class (TFslObjectList)
  Private
    Function GetObserver(iIndex: Integer): TWPObserver;
  Protected
    Function ItemClass : TFslObjectClass; Override;
    Function CompareByObject(pa, pB: Pointer) : Integer;
  Public
    destructor Destroy; Override;

    Procedure Notify(oSender : TObject); Overload; Virtual;
    Procedure Unregister(oObject : TObject); Overload; Virtual;
    Procedure Register(oObject : TObject; aEvent : TNotifyEvent); Overload; Virtual;

    Function IndexByObject(oObject : TObject): Integer; Overload; Virtual;
    Function ExistsByObject(oObject : TObject): Boolean; Overload; Virtual;
    Function GetByObject(oObject : TObject): TWPObserver; Overload; Virtual;

    Property Observer[iIndex : Integer] : TWPObserver Read GetObserver; Default;
  End;

Const
  PROP_ID_FIELD_FIXED = 1;
  PROP_ID_FIELD_READONLY = 2;
  PROP_ID_FIELD_NAME = 3;
  PROP_ID_FIELD_DATA = 4;
  PROP_ID_FIELD_DELETABLE = 5;
  PROP_ID_SEL_START = 7;
  PROP_ID_SEL_END = 8;
  PROP_ID_SEL_CURSOR = 9;
  PROP_ID_IMAGE_TYPE = 10;
  PROP_ID_IMAGE_NAME = 11;
  PROP_ID_IMAGE_SRC_HEIGHT = 12;
  PROP_ID_IMAGE_SRC_WIDTH = 13;
  PROP_ID_IMAGE_HEIGHT = 14;
  PROP_ID_IMAGE_WIDTH = 15;
  PROP_ID_IMAGE_BORDER = 16;
  PROP_ID_IMAGE_TRANSPARENT = 17;
  PROP_ID_IMAGE_VERTICAL = 18;
  PROP_ID_PARA_ALIGN = 19;
  PROP_ID_PARA_LIST = 20;
  PROP_ID_PARA_BULLET = 21;
  PROP_ID_PARA_NUMBER_TYPE = 22;
  PROP_ID_PARA_NUMBER_FORMAT = 23;
  PROP_ID_PARA_NUMBER_FIXED = 24;
  PROP_ID_PARA_LEFT = 25;
  PROP_ID_PARA_RIGHT = 26;
  PROP_ID_PARA_BOTTOM = 27;
  PROP_ID_CELL_SPAN = 28;
  PROP_ID_CELL_WIDTH = 29;
  PROP_ID_CELL_COLOUR = 30;
  PROP_ID_CELL_MARGIN_LEFT = 31;
  PROP_ID_CELL_MARGIN_RIGHT = 32;
  PROP_ID_CELL_MARGIN_TOP = 33;
  PROP_ID_CELL_MARGIN_BOTTOM = 34;
  PROP_ID_CELL_ALIGN = 35;
  PROP_ID_ROW_COLOUR = 36;
  PROP_ID_ROW_HEADER = 37;
  PROP_ID_ROW_BREAK_BEFORE = 82;

  PROP_ID_ROW_LOWER_PADDING_SIZE = 38;
  PROP_ID_ROW_LOWER_PADDING_COLOUR = 39;
  PROP_ID_TABLE_COLOUR = 40;
  PROP_ID_TABLE_BORDER = 41;
  PROP_ID_TABLE_MARGIN_HORIZ = 42;
  PROP_ID_TABLE_MARGIN_VERT = 43;
  PROP_ID_TABLE_COLCOUNT = 44;
  PROP_ID_TEXT_FONT = 45;
  PROP_ID_TEXT_SIZE = 46;
  PROP_ID_TEXT_FOREGROUND = 47;
  PROP_ID_TEXT_BACKGROUND = 48;
  PROP_ID_TEXT_BOLD = 49;
  PROP_ID_TEXT_ITALIC = 50;
  PROP_ID_TEXT_UNDERLINE = 51;
  PROP_ID_TEXT_STRIKETHROUGH = 52;
  PROP_ID_TEXT_STATE = 53;
  PROP_ID_SECTION_NAME = 54;
  PROP_ID_SECTION_TITLE = 55;
  PROP_ID_SECTION_TYPE = 56;
  PROP_ID_LINE_ALIGNMENT = 57;
  PROP_ID_LINE_WIDTH = 58;
  PROP_ID_LINE_COLOUR = 59;
  PROP_ID_LINE_PENWIDTH = 60;
  PROP_ID_LINE_STYLE = 61;
  PROP_ID_LINE_END = 62;
  PROP_ID_TEXT_STYLE = 63;
  PROP_ID_FIELD_NAMESPACE = 64;
  PROP_ID_SECTION_NAMESPACE = 65;
  PROP_ID_SECTION_DATA = 66;

  PROP_ID_DOC_MODIFIED = 71;
  PROP_ID_DOC_BACKGROUND = 72;
  PROP_ID_DOC_LOWLIGHT = 73;
  PROP_ID_DOC_HORIZMARGIN = 74;
  PROP_ID_DOC_VERTMARGIN = 75;
  PROP_ID_DOC_SCALE = 76;
  PROP_ID_DOC_TABLEBORDERS = 77;
  PROP_ID_DOC_EDITHINTS = 78;
  PROP_ID_DOC_SPELLING = 79;
  PROP_ID_DOC_FIELDWRAPPERS = 80;
  PROP_ID_DOC_ID = 81;
  // 82 is used


  PROP_ID_HOTSPOT_URL = 1;
  PROP_ID_HOTSPOT_KEY = 2;
  PROP_ID_HOTSPOT_TITLE = 3;
  PROP_ID_HOTSPOT_LINK = 4;
  PROP_ID_HOTSPOT_HOVER = 5;
  PROP_ID_HOTSPOT_UNDERLINE = 6;

  PROP_ID_TABLE_BORDER_LEFT = 1;
  PROP_ID_TABLE_BORDER_TOP = 2;
  PROP_ID_TABLE_BORDER_RIGHT = 3;
  PROP_ID_TABLE_BORDER_BOTTOM = 4;
  PROP_ID_TABLE_BORDER_LEFT_WORK = 11;
  PROP_ID_TABLE_BORDER_TOP_WORK = 12;
  PROP_ID_TABLE_BORDER_RIGHT_WORK = 13;
  PROP_ID_TABLE_BORDER_BOTTOM_WORK = 14;

  PROP_ID_FIELD_HOTSPOT = 1000;
  PROP_ID_CELL_HOTSPOT = 1100;
  PROP_ID_ROW_HOTSPOT = 1200;
  PROP_ID_CELL_ITEM = 1300;
  PROP_ID_ROW_ITEM = 1400;
  PROP_ID_TABLE_ITEM = 1500;

Type
  TEnumerationValues = Array Of String;
  TWPPropertyList = Class;

  TWPPropertyKind = (
//   code          meaning                                content of value
     pkGroup,   // contains a group of properties         name of the group
     pkName,    // identifier (alphanumeric)
     pkToken,    // any non-breaking content
     pkString,  // any string value (no eoln)
     pkInteger, // any integer                            string representation of integer
     pkFloat,   // any float                              string representation of float
     pkEnum,    // one of a list of options               a value from the list
     pkColour,  // a colour                               colour as a name or a hex value
     pkComplex);// a string (no eoln), but cannot be edited by user

  TWPProperty = Class (TFslObject)
    Private
      FEditable: Boolean;
      FId: Integer;
      FEnum: TEnumerationValues;
      FValue: String;
      FName: String;
      FKind: TWPPropertyKind;
      FChildren: TWPPropertyList;
      Procedure SetChildren(Const Value: TWPPropertyList);
      Procedure SetKind(Const Value: TWPPropertyKind);
  protected
    function sizeInBytesV : cardinal; override;
    Public
      destructor Destroy; Override;

      Function Link : TWPProperty; Overload;

      Function EnumAsVocab : String;

      Property Id : Integer Read FId Write FId;
      Property Editable : Boolean Read FEditable Write FEditable;
      Property Name : String Read FName Write FName;
      Property Kind : TWPPropertyKind Read FKind Write SetKind;
      Property Enum : TEnumerationValues Read FEnum Write FEnum;
      Property Value : String Read FValue Write FValue;
      Property Children : TWPPropertyList Read FChildren Write SetChildren;
  End;

  TWPPropertyList = Class (TFslObjectList)
  Private
      Function GetProperty(iIndex: Integer): TWPProperty;
    Protected
      Function ItemClass : TFslObjectClass; Override;
    Public
      Procedure Add(Const iId : Integer; Const bEditable : Boolean; Const sName : String; aKind : TWPPropertyKind; Const sValue : String); Overload; Virtual;
      Procedure AddName(Const iId : Integer; Const bEditable : Boolean; Const sName : String; Const sValue : String); Overload; Virtual;
      Procedure AddToken(Const iId : Integer; Const bEditable : Boolean; Const sName : String; Const sValue : String); Overload; Virtual;
      Procedure AddString(Const iId : Integer; Const bEditable : Boolean; Const sName : String; Const sValue : String); Overload; Virtual;
      Procedure AddInteger(Const iId : Integer; Const bEditable : Boolean; Const sName : String; Const iValue : Integer); Overload; Virtual;
      Procedure AddFloat(Const iId : Integer; Const bEditable : Boolean; Const sName : String; Const rValue : Real); Overload; Virtual;
      Procedure AddWord(Const iId : Integer; Const bEditable : Boolean; Const sName : String; Const iValue : Word); Overload; Virtual;
      Procedure AddColour(Const iId : Integer; Const bEditable : Boolean; Const sName : String; Const aValue : TColour); Overload; Virtual;
      Procedure AddEnum(Const iId : Integer; Const bEditable : Boolean; Const sName : String; Const iIndex : Integer; Const aNames : Array Of String); Overload; Virtual;
      Procedure AddEnum(Const iId : Integer; Const bEditable : Boolean; Const sName : String; Const sValue : String; Const aNames : TStringList); Overload; Virtual;
      Procedure AddEnum(Const iId : Integer; Const bEditable : Boolean; Const sName : String; Const sValue : String; Const aNames : TFslNameList); Overload; Virtual;
      Procedure AddComplex(Const iId : Integer; Const sName : String; Const sValue : String); Overload; Virtual;
      Function AddGroup(Const sName : String) : TWPPropertyList; Overload; Virtual;
      Procedure AddBoolean(Const iId : Integer; Const bEditable : Boolean; Const sName : String; Const bValue : Boolean); Overload; Virtual;
      Procedure AddTristate(Const iId : Integer; Const bEditable : Boolean; Const sName : String; Const aValue : TWPSTriState); Overload; Virtual;

      Property Properties[iIndex : Integer] : TWPProperty Read GetProperty; Default;
  End;

Type
  TWPSortDetail = Class(TFslObject)
    Private
      FSortIndex: Integer;
      FIsAscend: Boolean;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create(Const iIndex: Integer; Const bIsAscend: Boolean); Overload;

      Property SortIndex: Integer Read FSortIndex Write FSortIndex;
      Property IsAscend: Boolean Read FIsAscend Write FIsAscend;
  End;

  TWPSortDetailList = Class (TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPSortDetail;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPSortDetail);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPSortDetail; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPSortDetailList; Overload;
      Function Clone : TWPSortDetailList; Overload;
      Procedure Assign(oSource : TFslObject); Overload; Override;

      Property Elements[Const iIndex : Integer] : TWPSortDetail Read GetElement Write SetElement; Default;
  End;

  { Static functions }
  // 0 of equals, 1 if data1 should be 'before' data2, -1 otherwise.
  Function SortTextArray(Const oSorts : TWPSortDetailList; Const oData1: TStringList; Const oData2: TStringList): Integer;
  Procedure SortTextArrays(Const oSorts: TWPSortDetailList; Const aData: Array Of TStringList; Var aSortedPos: Array Of Integer);

type
  TWPRendererTableColumnMetricWidth = Integer;

  TWPRendererTableColumnMetric = Class(TFslObject)
    Private
      FDeadLefts : TFslIntegerList;
      FDeadRights : TFslIntegerList;
      FMinimum : TWPRendererTableColumnMetricWidth;
      FMaximum : TWPRendererTableColumnMetricWidth;
      FSpecified : TWPRendererTableColumnMetricWidth;
      FActual : TWPRendererTableColumnMetricWidth;
      FTotalChars : Integer;
    Protected
      Function ErrorClass : EFslExceptionClass; Overload; Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPRendererTableColumnMetric; Overload;

      Property DeadLefts : TFslIntegerList read FDeadLefts;
      Property DeadRights : TFslIntegerList read FDeadRights;

      Function DeadLeft : Integer; Overload; Virtual;
      Function DeadRight : Integer; Overload; Virtual;

      Property Minimum : TWPRendererTableColumnMetricWidth Read FMinimum Write FMinimum;
      Property Maximum : TWPRendererTableColumnMetricWidth Read FMaximum Write FMaximum;
      Property Specified : TWPRendererTableColumnMetricWidth Read FSpecified Write FSpecified;
      Property Actual : TWPRendererTableColumnMetricWidth Read FActual Write FActual;
      Property TotalChars : Integer Read FTotalChars Write FTotalChars;
  End;

  EWPRendererTableColumnMetric = Class(EFslException)
  End;

  TWPTableColumnMetrics = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPRendererTableColumnMetric;
    Protected
      Function ItemClass : TFslObjectClass; Override;
      Function Get(Const aValue : Integer) : TWPRendererTableColumnMetric; Reintroduce; Overload; Virtual;
    Public
      Function Link : TWPTableColumnMetrics; Overload;
      Function Clone : TWPTableColumnMetrics; Overload;

      Function New : TWPRendererTableColumnMetric; Reintroduce; Overload; Virtual;

      Function SumActual : Integer;

      Property Elements[Const iIndex : Integer] : TWPRendererTableColumnMetric Read GetElement; Default;
  End;

Type
  TWPFieldEntry = Class(TFslName)
    Private
      FCode : String;
      FDescription : String;
      FSection : Boolean;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create(Const sCode : String; Const sName : String; Const sDescription : String; Const bSection : Boolean); Overload; Virtual;

      Property Code : String Read FCode Write FCode;
      Property Description : String Read FDescription Write FDescription;
      Property Section : Boolean Read FSection Write FSection;
  End;

  TWPFieldEntryList = Class(TFslNameList)
    Private
      Function GetFieldName(iIndex: Integer): TWPFieldEntry;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Function Link : TWPFieldEntryList; Overload;
      Property FieldNameByIndex[iIndex : Integer] : TWPFieldEntry Read GetFieldName; Default;
  End;

  TWPFieldModel = Class(TFslObject)
  Private
    FTitle : String;
    FEntries : TWPFieldEntryList;
    Function GetEntries : TWPFieldEntryList;

  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Function Link : TWPFieldModel; Overload;
    Function Clone : TWPFieldModel; Overload;

    Property Entries : TWPFieldEntryList Read GetEntries;
    Property Title : String Read FTitle Write FTitle;
  End;

Implementation


Procedure TWPSFontDetails.Assign(oSource : TFslObject);
Begin
  Inherited;

  State := TWPSFontDetails(oSource).State;
  Bold := TWPSFontDetails(oSource).Bold;
  Italic := TWPSFontDetails(oSource).Italic;
  Strikethrough := TWPSFontDetails(oSource).Strikethrough;
  Underline := TWPSFontDetails(oSource).Underline;
  Name := TWPSFontDetails(oSource).Name;
  Foreground := TWPSFontDetails(oSource).Foreground;
  Background := TWPSFontDetails(oSource).Background;
  Size := TWPSFontDetails(oSource).Size;
  Capitalization := TWPSFontDetails(oSource).Capitalization;
End;


Procedure TWPSFontDetails.Clear;
Begin
  FName := DEF_STRING;
  FForeground := DEF_COLOUR;
  FBackground := DEF_COLOUR;
  FSize := DEF_WORD;
  FState := TWPSFontState(DEF_ENUM);
  FBold := TWPSTriState(DEF_ENUM);
  FItalic := TWPSTriState(DEF_ENUM);
  FStrikethrough := TWPSTriState(DEF_ENUM);
  FUnderline := TWPSTriState(DEF_ENUM);
  FCapitalization := TWPSCapsState(DEF_ENUM);
End;


Function TWPSFontDetails.HasName : Boolean;
Begin
  Result := FName <> DEF_STRING;
End;


Function TWPSFontDetails.HasSize : Boolean;
Begin
  Result := FSize <> DEF_WORD;
End;


Function TWPSFontDetails.HasBold : Boolean;
Begin
  Result := FBold <> TWPSTriState(DEF_ENUM);
End;


Function TWPSFontDetails.HasItalic : Boolean;
Begin
  Result := FItalic <> TWPSTriState(DEF_ENUM);
End;


Function TWPSFontDetails.HasUnderline : Boolean;
Begin
  Result := FUnderline <> TWPSTriState(DEF_ENUM);
End;


Function TWPSFontDetails.HasStrikethrough : Boolean;
Begin
  Result := FStrikethrough <> TWPSTriState(DEF_ENUM);
End;


Function TWPSFontDetails.HasState : Boolean;
Begin
  Result := FState <> TWPSFontState(DEF_ENUM);
End;


Function TWPSFontDetails.HasForeground : Boolean;
Begin
  Result := FForeground <> DEF_COLOUR;
End;


Function TWPSFontDetails.HasBackground  : Boolean;
Begin
  Result := FBackground <> DEF_COLOUR;
End;


Function TWPSFontDetails.Clone: TWPSFontDetails;
Begin
  Result := TWPSFontDetails(Inherited Clone);
End;


Constructor TWPSFontDetails.Create;
Begin
  Inherited;

  Clear;
End;


Function TWPSFontDetails.Link: TWPSFontDetails;
Begin
  Result := TWPSFontDetails(Inherited Link);
End;


Function TWPSFontDetails.Matches(oOther: TWPSFontDetails): Boolean;
Begin
  Result :=
    (Assigned(oOther)) And
    (FState = oOther.FState) And
    (FBold = oOther.FBold) And
    (FItalic = oOther.FItalic) And
    (FStrikethrough = oOther.FStrikethrough) And
    (FUnderline = oOther.FUnderline) And
    (FName = oOther.FName) And
    (FForeground = oOther.FForeground) And
    (FBackground = oOther.FBackground) And
    (FCapitalization = oOther.FCapitalization) And
    (FSize = oOther.FSize);
End;


Procedure TWPSFontDetails.Merge(oOther: TWPSFontDetails);
Begin
  If Assigned(oOther) Then
  Begin
    If FState <> oOther.FState Then
      FState := TWPSFontState(DEF_ENUM);

    If FBold <> oOther.FBold Then
      FBold := TWPSTriState(DEF_ENUM);

    If FItalic <> oOther.FItalic Then
      FItalic := TWPSTriState(DEF_ENUM);

    If FStrikethrough <> oOther.FStrikethrough Then
      FStrikethrough := TWPSTriState(DEF_ENUM);

    If FUnderline <> oOther.FUnderline Then
      FUnderline := TWPSTriState(DEF_ENUM);

    If FName <> oOther.FName Then
      FName := DEF_STRING;

    If FForeground <> oOther.FForeground Then
      FForeground := DEF_COLOUR;

    If FBackground <> oOther.FBackground Then
      FBackground := DEF_COLOUR;

    If FSize <> oOther.FSize Then
      FSize := DEF_WORD;

    If FCapitalization <> oOther.FCapitalization Then
      FCapitalization := TWPSCapsState(DEF_ENUM);
  End;
End;


Procedure TWPSFontDetails.Defaults;
Begin
  FState := fsUnknown;
  FBold := tsUnknown;
  FItalic := tsUnknown;
  FStrikethrough := tsUnknown;
  FUnderline := tsUnknown;
  FName := '';
  FForeground := DEF_COLOUR;
  FBackground := DEF_COLOUR;
  FSize := DEF_WORD;
  FCapitalization := fcsUnknown;
End;


Procedure TWPSFontDetails.DefaultSize;
Begin
  FSize := DEF_WORD;
End;


Procedure TWPSFontDetails.Update(oOther: TWPSFontDetails);
Begin
  If Assigned(oOther) Then
  Begin
    If oOther.FState <> TWPSFontState(DEF_ENUM) Then
      FState := oOther.FState;

    If oOther.FBold <> TWPSTriState(DEF_ENUM) Then
      FBold := oOther.FBold;

    If oOther.FItalic <> TWPSTriState(DEF_ENUM) Then
      FItalic := oOther.FItalic;

    If oOther.FStrikethrough <> TWPSTriState(DEF_ENUM) Then
      FStrikethrough := oOther.FStrikethrough;

    If oOther.FUnderline <> TWPSTriState(DEF_ENUM) Then
      FUnderline := oOther.FUnderline;

    If oOther.FName <> DEF_STRING Then
      FName := oOther.FName;

    If oOther.FForeground <> DEF_COLOUR Then
      FForeground := oOther.FForeground;

    If oOther.FBackground <> DEF_COLOUR Then
      FBackground := oOther.FBackground;

    If oOther.FSize <> DEF_WORD Then
      FSize := oOther.FSize;

    If oOther.FCapitalization <> TWPSCapsState(DEF_ENUM) Then
      FCapitalization := oOther.FCapitalization;
  End;
End;


function TWPSFontDetails.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
end;

Procedure TWPSParagraphDetails.Assign(oSource: TFslObject);
Begin
  Inherited;

  Align := TWPSParagraphDetails(oSource).Align;
  BulletType := TWPSParagraphDetails(oSource).BulletType;
  NumberType := TWPSParagraphDetails(oSource).NumberType;
  ListType := TWPSParagraphDetails(oSource).ListType;
  FMarginBottom := TWPSParagraphDetails(oSource).FMarginBottom;
  RightIndent := TWPSParagraphDetails(oSource).RightIndent;
  LeftIndent := TWPSParagraphDetails(oSource).LeftIndent;
  FixedNumber := TWPSParagraphDetails(oSource).FixedNumber;
  NumberFormat := TWPSParagraphDetails(oSource).NumberFormat;
End;

Procedure TWPSParagraphDetails.Clear;
Begin
  FAlign := TWordProcessorParagraphAlignment(DEF_ENUM);
  FMarginBottom := DEF_WORD;
  FBulletType := TWPSParagraphBulletType(DEF_ENUM);
  FNumberType := TWPSParagraphNumberType(DEF_ENUM);
  FListType := TWPSParagraphListType(DEF_ENUM);
  FNumberFormat := TWPSParagraphNumberFormat(DEF_ENUM);
  FRightIndent := DEF_WORD;
  FLeftIndent := DEF_WORD;
  FFixedNumber := DEF_WORD;
End;


Function TWPSParagraphDetails.HasValues : Boolean;
Begin
  Result :=
     HasAlign Or HasLeftIndent Or HasRightIndent Or HasMarginBottom Or
     HasBulletType Or HasListType Or HasNumberType Or HasNumberFormat Or
     HasFixedNumber;
End;


Function TWPSParagraphDetails.HasAlign : Boolean;
Begin
  Result := FAlign <> TWordProcessorParagraphAlignment(DEF_ENUM);
End;


Function TWPSParagraphDetails.HasLeftIndent : Boolean;
Begin
  Result := FLeftIndent <> DEF_WORD;
End;


Function TWPSParagraphDetails.HasRightIndent : Boolean;
Begin
  Result := FRightIndent <> DEF_WORD;
End;


Function TWPSParagraphDetails.HasMarginBottom : Boolean;
Begin
  Result := FMarginBottom <> DEF_WORD;
End;


Function TWPSParagraphDetails.HasBulletType : Boolean;
Begin
  Result := FBulletType <> TWPSParagraphBulletType(DEF_ENUM);
End;


Function TWPSParagraphDetails.HasListType : Boolean;
Begin
  Result := FListType <> TWPSParagraphListType(DEF_ENUM);
End;


Function TWPSParagraphDetails.HasNumberType : Boolean;
Begin
  Result := FNumberType <> TWPSParagraphNumberType(DEF_ENUM);
End;


Function TWPSParagraphDetails.HasNumberFormat : Boolean;
Begin
  Result := FNumberFormat <> TWPSParagraphNumberFormat(DEF_ENUM);
End;


Function TWPSParagraphDetails.HasFixedNumber : Boolean;
Begin
  Result := FFixedNumber <> DEF_WORD;
End;


Function TWPSParagraphDetails.Clone: TWPSParagraphDetails;
Begin
  Result := TWPSParagraphDetails(Inherited Clone);
End;


Constructor TWPSParagraphDetails.Create;
Begin
  Inherited;

  Defaults;
End;



procedure prop(var result : string; name, value : string);
begin
  if value <> '' then
  begin
    if result <> '' then
      result := result + '; ';
    result := result + name+': '+value;
  end;
end;

function TWPSParagraphDetails.Describe: String;
begin
  if self = nil then
    result := '(nil)'
  else
  begin
    result := '';
    prop(result, 'align',NAMES_WPSPARAGRAPHALIGNMENT[FAlign]);
    prop(result, 'bullet-type',NAMES_WPSPARAGRAPHBULLETTYPE[FBulletType]);
    prop(result, 'number-type',NAMES_WPSPARAGRAPHNUMBERTYPE[FNumberType]);
    prop(result, 'list-type',NAMES_WPSPARAGRAPHLISTTYPE[FListType]);
    prop(result, 'fixed-number', inttostr(FFixedNumber));
    prop(result, 'number-format', NAMES_WPSPARAGRAPHNUMBERFORMAT[FNumberFormat]);
    prop(result, 'left-margin', inttostr(FLeftIndent));
    prop(result, 'right-margin', inttostr(FRightIndent));
    prop(result, 'bottom-margin', inttostr(FMarginBottom));
  end;
end;

Function TWPSParagraphDetails.Link: TWPSParagraphDetails;
Begin
  Result := TWPSParagraphDetails(Inherited Link);
End;


Function TWPSParagraphDetails.Matches(oOther: TWPSParagraphDetails): Boolean;
Begin
  Result :=
    (FAlign = oOther.FAlign) And
    (FMarginBottom = oOther.FMarginBottom) And
    (FBulletType = oOther.FBulletType) And
    (FNumberType = oOther.FNumberType) And
    (FListType = oOther.FListType) And
    (FRightIndent = oOther.FRightIndent) And
    (FLeftIndent = oOther.FLeftIndent) And
    (FFixedNumber = oOther.FFixedNumber) And
    (FNumberFormat = oOther.FNumberFormat);
End;

Procedure TWPSParagraphDetails.Merge(oOther: TWPSParagraphDetails);
Begin
  If FAlign <> oOther.FAlign Then
    FAlign := TWordProcessorParagraphAlignment(DEF_ENUM);

  If FMarginBottom <> oOther.FMarginBottom Then
    FMarginBottom := DEF_WORD;

  If FBulletType <> oOther.FBulletType Then
    FBulletType := TWPSParagraphBulletType(DEF_ENUM);

  If FNumberType <> oOther.FNumberType Then
    FNumberType := TWPSParagraphNumberType(DEF_ENUM);

  If FListType <> oOther.FListType Then
    FListType := TWPSParagraphListType(DEF_ENUM);

  If FNumberFormat <> oOther.FNumberFormat Then
    FNumberFormat := TWPSParagraphNumberFormat(DEF_ENUM);

  If FRightIndent <> oOther.FRightIndent Then
    FRightIndent := DEF_WORD;

  If FLeftIndent <> oOther.FLeftIndent Then
    FLeftIndent := DEF_WORD;

  If FFixedNumber <> oOther.FFixedNumber Then
    FFixedNumber := DEF_WORD;
End;


Procedure TWPSParagraphDetails.Defaults;
Begin
  FAlign := WordProcessorParagraphAlignmentUnknown;
  FMarginBottom := DEF_WORD;
  FBulletType := tbUnknown;
  FNumberType := tnUnknown;
  FListType := WPSParagraphListTypeUnknown;
  FNumberFormat := nwUnknown;
  FRightIndent := DEF_WORD;
  FLeftIndent := DEF_WORD;
  FFixedNumber := DEF_WORD;
End;


Function TWPSParagraphDetails.Update(oOther: TWPSParagraphDetails) : TChangeType;
Begin
  Result := ctLayout;

  If oOther.FAlign <> TWordProcessorParagraphAlignment(DEF_ENUM) Then
    FAlign := oOther.FAlign;

  If oOther.FMarginBottom <> DEF_WORD Then
    FMarginBottom := oOther.FMarginBottom;

  If oOther.FBulletType <> TWPSParagraphBulletType(DEF_ENUM) Then
    FBulletType := oOther.FBulletType;

  If oOther.FNumberType <> TWPSParagraphNumberType(DEF_ENUM) Then
    FNumberType := oOther.FNumberType;

  If oOther.FListType <> TWPSParagraphListType(DEF_ENUM) Then
  Begin
    FListType := oOther.FListType;
    Result := ctTextContents;
  End;

  If oOther.FNumberFormat <> TWPSParagraphNumberFormat(DEF_ENUM) Then
    FNumberFormat := oOther.FNumberFormat;

  If oOther.FRightIndent <> DEF_WORD Then
    FRightIndent := oOther.FRightIndent;

  If oOther.FLeftIndent <> DEF_WORD Then
    FLeftIndent := oOther.FLeftIndent;

  If oOther.FFixedNumber <> DEF_WORD Then
  Begin
    FFixedNumber := oOther.FFixedNumber;
    Result := ctTextContents;
  End;
End;


Procedure TWPSParagraphDetails.AlignCentre;
Begin
  FAlign := WordProcessorParagraphAlignmentCentre;
End;


Procedure TWPSParagraphDetails.AlignJustify;
Begin
  FAlign := WordProcessorParagraphAlignmentJustify;
End;


Procedure TWPSParagraphDetails.AlignLeft;
Begin
  FAlign := WordProcessorParagraphAlignmentLeft;
End;


Procedure TWPSParagraphDetails.AlignNone;
Begin
  FAlign := WordProcessorParagraphAlignmentUnknown;
End;


Procedure TWPSParagraphDetails.AlignRight;
Begin
  FAlign := WordProcessorParagraphAlignmentRight;
End;

Constructor TWPStyle.Create;
Begin
  Inherited;

  FFont := TWPSFontDetails.Create;
  FFont.Clear;

  FParagraph := TWPSParagraphDetails.Create;
  FParagraph.Clear;
End;


Destructor TWPStyle.Destroy;
Begin
  FFont.Free;
  FParagraph.Free;

  Inherited;
End;


Procedure TWPStyle.Assign(oSource: TFslObject);
Begin
  Inherited;

  FFont.Assign(TWPStyle(oSource).FFont);
  FParagraph.Assign(TWPStyle(oSource).FParagraph);
  FResetOnNewParagraph := TWPStyle(oSource).FResetOnNewParagraph;
End;


Function TWPStyle.Clone: TWPStyle;
Begin
  Result := TWPStyle(Inherited Clone);
End;


Function TWPStyle.Link: TWPStyle;
Begin
  Result := TWPStyle(Inherited Link);
End;


Procedure TWPStyle.SetFont(Const Value: TWPSFontDetails);
Begin
  FFont.Free;
  FFont := Value;
End;


Procedure TWPStyle.SetParagraph(Const Value: TWPSParagraphDetails);
Begin
  FParagraph.Free;
  FParagraph := Value;
End;


Function TWPStyle.GetFont: TWPSFontDetails;
Begin
  If Self = Nil Then
    Result := Nil
  Else
    Result := FFont;
End;


Function TWPStyle.GetParagraph: TWPSParagraphDetails;
Begin
  If Self = Nil Then
    Result := Nil
  Else
    Result := FParagraph;
End;


Function TWPStyle.WorkingFontName(oFont : TWPSFontDetails) : String;
Begin
  If oFont.HasName Then
    Result := oFont.Name
  Else
    Result := Font.Name;

  If Result = '' Then
    Result := DEFAULT_FONT_NAME;
End;


Function TWPStyle.WorkingFontSize(oFont : TWPSFontDetails) : Integer;
Begin
  If oFont.HasSize Then
    Result := oFont.Size
  Else
    Result := Font.Size;

  If Result = DEF_WORD Then
    Result := DEFAULT_FONT_SIZE;
End;


Function TWPStyle.WorkingFontBold(oFont : TWPSFontDetails) : TWPSTriState;
Begin
  If oFont.HasBold Then
    Result := oFont.Bold
  Else
    Result := Font.Bold;

  If Result = tsUnknown Then
    Result := tsFalse;
End;


Function TWPStyle.WorkingFontCapitalization(oFont : TWPSFontDetails) : TWPSCapsState;
Begin
  If oFont.HasCapitalization Then
    Result := oFont.Capitalization
  Else
    Result := Font.Capitalization;

  If Result = fcsUnknown Then
    Result := fcsNormal;
End;


Function TWPStyle.WorkingFontItalic(oFont : TWPSFontDetails) : TWPSTriState;
Begin
  If oFont.HasItalic Then
    Result := oFont.Italic
  Else
    Result := Font.Italic;

  If Result = tsUnknown Then
    Result := tsFalse;
End;


Function TWPStyle.WorkingFontUnderline(oFont : TWPSFontDetails) : TWPSTriState;
Begin
  If oFont.HasUnderline Then
    Result := oFont.Underline
  Else
    Result := Font.Underline;

  If Result = tsUnknown Then
    Result := tsFalse;
End;


Function TWPStyle.WorkingFontStrikethrough(oFont : TWPSFontDetails) : TWPSTriState;
Begin
  If oFont.HasStrikethrough Then
    Result := oFont.Strikethrough
  Else
    Result := Font.Strikethrough;

  If Result = tsUnknown Then
    Result := tsFalse;
End;


Function TWPStyle.WorkingFontState(oFont : TWPSFontDetails) : TWPSFontState;
Begin
  If oFont.HasState Then
    Result := oFont.State
  Else
    Result := Font.State;

  If Result = fsUnknown Then
    Result := fsNormal;
End;


Function TWPStyle.WorkingFontForeground(oFont : TWPSFontDetails) : TColour;
Begin
  If oFont.HasForeground Then
    Result := oFont.Foreground
  Else
    Result := Font.Foreground;

  If Result = DEF_COLOUR Then
    Result := DEFAULT_FOREGROUND;
End;


Function TWPStyle.WorkingFontBackground (oFont : TWPSFontDetails; aDefault : TColour) : TColour;
Begin
  If oFont.HasBackground Then
    Result := oFont.Background
  Else
    Result := Font.Background;

  If Result = DEF_COLOUR Then
    Result := aDefault;
End;


Function TWPStyle.WorkingParagraphAlign(oParagraph : TWPSParagraphDetails) : TWordProcessorParagraphAlignment;
Begin
  If oParagraph.HasAlign Then
    Result := oParagraph.Align
  Else
    Result := Paragraph.Align;

  If Result = WordProcessorParagraphAlignmentUnknown Then
    Result := WordProcessorParagraphAlignmentLeft;
End;


Function TWPStyle.WorkingParagraphLeftIndent(oParagraph : TWPSParagraphDetails) : Integer;
Begin
  If oParagraph.HasLeftIndent Then
    Result := oParagraph.LeftIndent
  Else
    Result := Paragraph.LeftIndent;

  If Result = DEF_WORD Then
    Result := 0;
End;


Function TWPStyle.WorkingParagraphRightIndent(oParagraph : TWPSParagraphDetails) : Integer;
Begin
  If oParagraph.HasRightIndent Then
    Result := oParagraph.RightIndent
  Else
    Result := Paragraph.RightIndent;

  If Result = DEF_WORD Then
    Result := 0;
End;


Function TWPStyle.WorkingParagraphMarginBottom(oParagraph : TWPSParagraphDetails) : Integer;
Begin
  If oParagraph.HasMarginBottom Then
    Result := oParagraph.MarginBottom
  Else
    Result := Paragraph.MarginBottom;

  If Result = DEF_WORD Then
    Result := DEFAULT_PARAGRAPH_MARGIN_BOTTOM;
End;


Function TWPStyle.WorkingParagraphBulletType(oParagraph : TWPSParagraphDetails) : TWPSParagraphBulletType;
Begin
  If oParagraph.HasBulletType Then
    Result := oParagraph.BulletType
  Else
    Result := Paragraph.BulletType;

  If Result = tbUnknown Then
    Result := tbDisc;
End;


Function TWPStyle.WorkingParagraphListType(oParagraph : TWPSParagraphDetails) : TWPSParagraphListType;
Begin
  If oParagraph.HasListType Then
    Result := oParagraph.ListType
  Else
    Result := Paragraph.ListType;

  If Result = WPSParagraphListTypeUnknown Then
    Result := WPSParagraphListTypeNone;
End;


Function TWPStyle.WorkingParagraphNumberType(oParagraph : TWPSParagraphDetails) : TWPSParagraphNumberType;
Begin
  If oParagraph.HasNumberType Then
    Result := oParagraph.NumberType
  Else
    Result := Paragraph.NumberType;

  If Result = tnUnknown Then
    Result := tnArabic;
End;


Function TWPStyle.WorkingParagraphNumberFormat(oParagraph : TWPSParagraphDetails) : TWPSParagraphNumberFormat;
Begin
  If oParagraph.HasNumberFormat Then
    Result := oParagraph.NumberFormat
  Else
    Result := Paragraph.NumberFormat;

  If Result = nwUnknown Then
    Result := nwNone;
End;


Function TWPStyle.HasParagraphAspect : Boolean;
Begin
  Result := FParagraph.HasValues;
End;


function TWPStyle.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFont.sizeInBytes);
  inc(result, FParagraph.sizeInBytes);
end;

Constructor TWPStyles.Create;
Begin
  Inherited;

  Clear;
  UseSystemDefaultStyle;
End;

Destructor TWPStyles.Destroy;
Begin
  FDefaultStyle.Free;
  Inherited;
End;

Function TWPStyles.Clone: TWPStyles;
Begin
  Result := TWPStyles(Inherited Clone);
End;


Function TWPStyles.GetStyleByIndex(Const iIndex : Integer) : TWPStyle;
Begin
  Result := TWPStyle(ObjectByIndex[iIndex]);
End;


Function TWPStyles.ItemClass: TFslObjectClass;
Begin
  Result := TWPStyle;
End;


Function TWPStyles.Link: TWPStyles;
Begin
  Result := TWPStyles(Inherited Link);
End;


Function TWPStyles.HasDefaultStyle: Boolean;
Begin
  Result := GetDefaultStyle <> nil;
End;


Function TWPStyles.GetDefaultStyle : TWPStyle;
Begin
  Result := TWPStyle(Inherited GetByName(DEFAULT_STYLE_NAME));
End;


Function TWPStyles.GetByNameOrDefault(Const sName : String) : TWPStyle;
Begin
  If sName <> '' Then
    Result := GetByName(sName)
  Else
    Result := Nil;

  If Not Assigned(Result) Then
    Result := GetDefaultStyle;

  If Not Assigned(Result) Then
    RaiseError('GetByNameOrDefault' ,'No Default style found');
End;


Function TWPStyles.GetByName(Const sName : String) : TWPStyle;
Begin
  If sName = '' Then
    Result := DefaultStyle
  Else
    Result := TWPStyle(Inherited GetByName(sName));
End;


Procedure TWPStyles.SetDefaultStyle(oStyle: TWPStyle);
Begin
  Assert(Invariants('SetDefaultStyle', oStyle, TWPStyle, 'Value'));
  If ExistsByName(DEFAULT_STYLE_NAME) Then
    RemoveByName(DEFAULT_STYLE_NAME);
  oStyle.name := DEFAULT_STYLE_NAME;
  Add(oStyle);
End;


Function ToTriState(bValue : Boolean):TWPSTriState;
Begin
  If bValue Then
    Result := tsTrue
  Else
    Result := tsFalse;
End;


Procedure TWPStyles.UseSystemDefaultStyle;
Var
  oStyle : TWPStyle;
Begin
  oStyle := TWPStyle.Create;
  Try
    oStyle.Name := DEFAULT_STYLE_NAME;
    oStyle.font.Name := 'Verdana';
    oStyle.font.Size := 10;

    SetDefaultStyle(oStyle.Link);
  Finally
    oStyle.Free;
  End;
End;


function TWPStyles.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDefaultStyle.sizeInBytes);
end;

Function TWPStyleStack.Empty: Boolean;
Begin
  Result := Count = 0;
End;


Function TWPStyleStack.Peek: TWPStyle;
Begin
  If count = 0 Then
    Result := Nil
  Else
    Result := StyleByIndex[Count - 1];
End;


Procedure TWPStyleStack.Pop;
Begin
  Assert(CheckCondition(Count > 0, 'Pop', 'Stack is empty'));

  DeleteByIndex(Count - 1);
End;


Procedure TWPStyleStack.Push(oStyle : TWPStyle);
Begin
  Add(oStyle);
End;


Procedure TWPSParagraphDetails.BulletTypeCircle;
Begin
  BulletType := tbCircle;
End;


Procedure TWPSParagraphDetails.BulletTypeDisc;
Begin
  BulletType := tbDisc;
End;


Procedure TWPSParagraphDetails.BulletTypeNone;
Begin
  BulletType := tbUnknown;
End;


Procedure TWPSParagraphDetails.BulletTypeSquare;
Begin
  BulletType := tbSquare;
End;


Function TWPStyles.Add(Const sName: String): TWPStyle;
Begin
  Result := TWPStyle.Create;
  Try
    Result.Name := sName;
    Add(Result.Link);
  Finally
    Result.Free;
  End;
End;


Procedure TWPSFontDetails.SetBold(Const bValue: Boolean);
Begin
  If bValue Then
    Bold := tsTrue
  Else
    Bold := tsFalse;
End;


Function WordToText(iValue : Word) : String;
Begin
  If iValue = DEF_WORD Then
    Result := '(default)'
  Else
    Result := IntegerToString(iValue);
End;


Function TextToWord(Const sValue : String) : Word;
Var
  iValue : Integer;
Begin
  If StringIsInteger32(sValue) Then
  Begin
    iValue := StringToInteger32(sValue);
    If (iValue >= 0) And (iValue < DEF_WORD) Then
      Result := iValue
    Else
      Result := DEF_WORD;
  End
  Else
    Result := DEF_WORD;
End;


Procedure TWPStyles.MergeStyles(oStyles: TWPStyles; bUpdateExisting : Boolean);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oStyles.Count - 1 Do
    If Not ExistsByName(oStyles[iLoop].Name) Then
      Add(oStyles[iLoop].Clone)
    Else If bUpdateExisting Then
    Begin
      GetByName(oStyles[iLoop].Name).Font.Merge(oStyles[iLoop].Font);
      GetByName(oStyles[iLoop].Name).Paragraph.Merge(oStyles[iLoop].Paragraph);
    End;
End;

Function TWPSFontDetails.HasCapitalization: Boolean;
Begin
  Result := FCapitalization <> fcsUnknown;
End;

Procedure TWPSFontDetails.Apply(oFont: TFont; bDefaults : Boolean);
Begin
  If (Name <> '') Then
    oFont.Name := FName
  Else if bDefaults then
    oFont.Name := DEFAULT_FONT_NAME;
  If (FSize <> DEF_WORD) Then
    oFont.Size := FSize
  Else if bDefaults then
    oFont.Size := DEFAULT_FONT_SIZE;
  If (FForeground <> DEF_COLOUR) Then
    oFont.Color := FForeground
  Else if bDefaults then
    oFont.Color := DEFAULT_FOREGROUND;
  oFont.Style := [];
  If FBold = tsTrue Then
    oFont.Style := oFont.Style + [fsBold];
  If FItalic = tsTrue Then
    oFont.Style := oFont.Style + [fsItalic];
  If FUnderline = tsTrue Then
    oFont.Style := oFont.Style + [fsUnderline];
End;


Procedure TWPSFontDetails.Copy(oFont: TFont);
Begin
  FName := oFont.Name;
  FSize := oFont.Size;
  FForeground := oFont.Color;
  If (fsBold In oFont.Style) Then
    FBold := tsTrue
  Else
    FBold := tsFalse;
  If (fsItalic In oFont.Style) Then
    FItalic := tsTrue
  Else
    FItalic := tsFalse;
  If (fsUnderline In oFont.Style) Then
    FUnderline := tsTrue
  Else
    FUnderline := tsFalse;
End;

Function DescribeTriState(aState : TWPSTriState; Const name : String): String;
Begin
  If aState = tsTrue Then
    Result := ' '+name+','
  Else If aState = tsFalse Then
    Result := ' -'+name+'-,'
  Else
    Result := '';
End;


Function TWPSFontDetails.Describe: String;
Begin
  if self = nil then
    result := '(nil)'
  else
  begin
    If (FName = '') Then
      Result := '**'
    Else
      Result := FName;

    If FSize = DEF_WORD Then
      Result := Result + ': **'
    Else
      Result := Result + ': '+IntegerToString(FSize);

    If FForeground <> DEF_COLOUR Then
      Result := Result + ' '+ColourToHTMLColourString(FForeground);

    If FBackground <> DEF_COLOUR Then
      Result := Result + ' /'+ColourToHTMLColourString(FBackground);

    Result := Result + '[';
    Result := Result + DescribeTriState(FBold, 'bold');
    Result := Result + DescribeTriState(FItalic, 'italic');
    Result := Result + DescribeTriState(FUnderline, 'underline');
    Result := Result + DescribeTriState(FStrikethrough, 'strikethrough');
    If FState = fsNormal Then
      Result := Result + ' normal,'
    Else If FState = fsSuperscript Then
      Result := Result + ' superscript,'
    Else If FState = fsSubscript Then
      Result := Result + ' subscript,';

    If FCapitalization = fcsNormal Then
      Result := Result + ' normal-caps,'
    Else If FCapitalization = fcsAllCaps Then
      Result := Result + ' all-caps,'
    Else If FCapitalization = fcsSmallCaps Then
      Result := Result + ' small-caps,'
    Else If FCapitalization = fcsNoCaps Then
      Result := Result + ' no-caps,';

    If (Result[Length(Result)] = ',') Then
      Delete(Result, Length(Result), 1);
    Result := Result + ']';
  end;
End;

Destructor TWPBorder.Destroy;
Begin
  FBrushImage.Free;
  Inherited;
End;


Function TWPBorder.Link : TWPBorder;
Begin
  Result := TWPBorder(Inherited Link);
End;


Function TWPBorder.Clone : TWPBorder;
Begin
  Result := TWPBorder(Inherited Clone);
End;


Procedure TWPBorder.Assign(oSource : TFslObject);
Begin
  Inherited;

  FDefined := TWPBorder(oSource).FDefined;
  FFancy := TWPBorder(oSource).FFancy;
  FOuterColour := TWPBorder(oSource).FOuterColour;
  FOuterColour2 := TWPBorder(oSource).FOuterColour2;
  FColour := TWPBorder(oSource).FColour;
  FWidth := TWPBorder(oSource).FWidth;
  FStyle := TWPBorder(oSource).FStyle;
  BrushImage := TWPBorder(oSource).FBrushImage.Link;
  FLowOuterlimit := TWPBorder(oSource).FLowOuterlimit;
  FHighOuterLimit := TWPBorder(oSource).FHighOuterLimit;
End;



Function TWPBorder.IsCompatible(oBorder: TWPBorder): Boolean;
Begin
  Result := (oBorder <> Nil) And (FDefined = oBorder.Defined);
  If Result Then
  Begin
    Result := (FFancy = oBorder.FFancy)
        And (FColour = oBorder.FColour)
        And (FWidth = oBorder.FWidth)
        And (FStyle = oBorder.FStyle)
        And (BrushImage = oBorder.FBrushImage);
    If Result And FFancy Then
      Result := (FOuterColour = oBorder.FOuterColour)
        And (FOuterColour2 = oBorder.FOuterColour2)
        And (FLowOuterlimit = oBorder.FLowOuterlimit)
        And (FHighOuterLimit = oBorder.FHighOuterLimit);
  End;
End;


Procedure TWPBorder.Clear;
Begin
  FDefined := False;
  FFancy := False;
  FOuterColour := DEF_COLOUR;
  FOuterColour2 := DEF_COLOUR;
  FColour := clBlack;
  FWidth := 1;
  FStyle := apsNone;
  BrushImage := Nil;
  FLowOuterlimit := DEF_WORD;
  FHighOuterLimit := DEF_WORD;
End;


Procedure TWPBorder.SetDefined(Const Value : Boolean);
Begin
  If Value <> FDefined Then
    Begin
    FDefined := Value;
    Change(ctLayout, Self);
    End;
End;


Procedure TWPBorder.SetFancy(Const Value : Boolean);
Begin
  If Value <> FFancy Then
    Begin
    FFancy := Value;
    Change(ctLayout, Self);
    End;
End;


Procedure TWPBorder.SetOuterColour(Const Value : TColour);
Begin
  If Value <> FOuterColour Then
    Begin
    FOuterColour := Value;
    Change(ctLayout, Self);
    End;
End;


Procedure TWPBorder.SetOuterColour2(Const Value : TColour);
Begin
  If Value <> FOuterColour2 Then
    Begin
    FOuterColour2 := Value;
    Change(ctLayout, Self);
    End;
End;


Procedure TWPBorder.SetColour(Const Value : TColour);
Begin
  If Value <> FColour Then
    Begin
    FColour := Value;
    Change(ctLayout, Self);
    End;
End;


Procedure TWPBorder.SetWidth(Const Value : Integer);
Begin
  If Value <> FWidth Then
    Begin
    FWidth := Value;
    Change(ctLayout, Self);
    End;
End;


Procedure TWPBorder.SetStyle(Const Value : TFslPenStyle);
Begin
  If Value <> FStyle Then
    Begin
    FStyle := Value;
    Change(ctLayout, Self);
    End;
End;

Procedure TWPBorder.SetBrushImage(Const Value : TFslBitmapGraphic);
Begin
  If Value <> FBrushImage Then
  Begin
    FBrushImage.Free;
    FBrushImage := Value;
    Change(ctLayout, Self);
  End;
End;


Procedure TWPBorder.SetLowOuterlimit(Const Value : Word);
Begin
  If Value <> FLowOuterlimit Then
  Begin
    FLowOuterlimit := Value;
    Change(ctLayout, Self);
  End;
End;


Procedure TWPBorder.SetHighOuterLimit(Const Value : Word);
Begin
  If Value <> FHighOuterLimit Then
  Begin
    FHighOuterLimit := Value;
    Change(ctLayout, Self);
  End;
End;



Procedure TWPBorder.SimpleLine;
Begin
  Clear;
  Defined := True;
  Fancy := False;
  OuterColour := DEF_COLOUR;
  OuterColour2 := DEF_COLOUR;
  Colour := clBlack;
  Width := 1;
  Style := apsSolid;
End;

Function TWPBorder.IsSimpleLine: Boolean;
Begin
  Result := (Defined = True) And (Fancy = False)
    And (Colour = clBlack) And (Width = 1) And (Style = apsSolid)
    And (OuterColour = DEF_COLOUR) And (OuterColour2 = DEF_COLOUR);
End;

Procedure TWPBorder.SimpleDot;
Begin
  Clear;
  Defined := True;
  Fancy := False;
  OuterColour := DEF_COLOUR;
  OuterColour2 := DEF_COLOUR;
  Colour := clBlack;
  Width := 1;
  Style := apsDot;
End;

Function TWPBorder.IsSimpleDot: Boolean;
Begin
  Result := (Defined = True) And (Fancy = False)
    And (Colour = clBlack) And (Width = 1) And (Style = apsDot)
    And (OuterColour = DEF_COLOUR) And (OuterColour2 = DEF_COLOUR);
End;

Procedure TWPBorder.SimpleFancy;
Begin
  Defined := True;
  Fancy := True;
  OuterColour := DEF_COLOUR;
  OuterColour2 := DEF_COLOUR;
  Colour := clSilver;
  Width := 16;
  Style := apsSolid;
End;

Function TWPBorder.IsSimpleFancy: Boolean;
Begin
  Result := (Defined = True) And (Fancy = True)
    And (Colour = clSilver) And (Width = 16) And (Style = apsSolid)
    And (OuterColour = DEF_COLOUR) And (OuterColour2 = DEF_COLOUR);
End;

Function TWPBorder.ActualWidth : Integer;
Begin
  If Not Defined Then
    Result := 0
  Else
    Result := FWidth;
End;

Function TWPBorder.PropertyDescription: String;
Begin
  If (FDefined) Then
  Begin
    If FFancy Then
      Result := 'Fancy'
    Else
      Result := ColourToHTMLColourTitleString(FColour)+' '+IntegerToString(FWidth)+' px '+ADVPENSTYLE_CODES[FStyle];
  End
  Else
    Result := '--';
End;

Function TWPBorder.MapToPredefinedBorderStyle: TPredefinedBorderStyles;
Begin
  If Not Defined Then
    Result := pbsNone
  Else If Fancy Then
    Begin
      If IsSimpleFancy Then
        Result := pbsSimpleFancy
      Else
        Result := pbsCustomFancy;
    End
  Else
    Begin
      If IsSimpleLine Then
        Result := pbsSimpleLine
      Else If IsSimpleDot Then
        Result := pbsSimpleDot
      Else
        Result := pbsCustom;
    End
End;


function TWPBorder.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FBrushImage.sizeInBytes);
end;

Destructor TWPTrackable.Destroy;
Begin
  If Assigned(FOnChange) Then
    RaiseError('Destroy', 'Must unhook before destroying');
  // not the best solution, but safe.
  // note that you need to unhook when filing as well
  Inherited;
End;


Procedure TWPTrackable.Hook(aEvent: TChangeEvent);
Begin
  If Self <> Nil Then
  Begin
    If Assigned(FOnChange) Then
      RaiseError('Hook', 'Already Hooked');
    FOnChange := aEvent;
  End;
End;


Procedure TWPTrackable.UnHook(aEvent: TChangeEvent);
Begin
  If Self <> Nil Then
  Begin
    If Not Assigned(FOnChange) Then
      RaiseError('Hook', 'not Already Hooked');
    If @FOnChange <> @aEvent Then
      RaiseError('Hook', 'not Hooked to this object');
    FOnChange := Nil;
  End;
End;


Procedure TWPTrackable.Change(aType : TChangeType; oSource : TWPTrackable);
Begin
  If Assigned(FOnChange) Then
    FOnChange(aType, oSource);
End;

Constructor TWPTrackableList.Create;
Begin
  Inherited;
  FHooking := True;
End;


Constructor TWPTrackableList.Create(bHooking : Boolean);
Begin
  Create;
  FHooking := bHooking;
End;


Function TWPTrackableList.Link : TWPTrackableList;
Begin
  Result := TWPTrackableList(Inherited Link);
End;


Function TWPTrackableList.Clone : TWPTrackableList;
Begin
  Result := TWPTrackableList(Inherited Clone);
End;


Function TWPTrackableList.New : TWPTrackable;
Begin
  Result := TWPTrackable(Inherited New);
End;


Function TWPTrackableList.ItemClass : TFslObjectClass;
Begin
  Result := TWPTrackable;
End;


Function TWPTrackableList.GetElement(Const iIndex : Integer) : TWPTrackable;
Begin
  Result := TWPTrackable(ObjectByIndex[iIndex]);
End;


Procedure TWPTrackableList.SetElement(Const iIndex : Integer; Const oValue : TWPTrackable);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPTrackableList.Get(Const aValue : Integer) : TWPTrackable;
Begin
  Result := TWPTrackable(Inherited Get(aValue));
End;

Destructor TWPTrackableList.Destroy;
Begin
  If Assigned(FOnChange) Then
    RaiseError('Destroy', 'Must unhook before destroying');
  // not the best solution, but safe.
  // note that you need to unhook when filing as well
  Inherited;
End;


Procedure TWPTrackableList.Hook(aEvent: TChangeEvent);
Begin
  If Self <> Nil Then
  Begin
    If Assigned(FOnChange) Then
      RaiseError('Hook', 'Already Hooked');
    FOnChange := aEvent;
  End;
End;


Procedure TWPTrackableList.UnHook(aEvent: TChangeEvent);
Begin
  If Self <> Nil Then
  Begin
    If Not Assigned(FOnChange) Then
      RaiseError('Hook', 'not Already Hooked');
    If @FOnChange <> @aEvent Then
      RaiseError('Hook', 'not Hooked to this object');
    FOnChange := Nil;
  End;
End;


Procedure TWPTrackableList.Change(aType : TChangeType; oSource : TWPTrackable);
Begin
  If Assigned(FOnChange) Then
    FOnChange(aType, oSource);
End;


Procedure TWPTrackableList.InternalAfterInclude(iIndex : Integer; oObject : TFslObject);
Var
  oTrackable : TWPTrackable;
Begin
  Inherited InternalAfterInclude(iIndex, oObject);
  If Hooking Then
  Begin
    oTrackable := TWPTrackable(oObject);

    oTrackable.Hook(Change);
    Change(ctListContents, oTrackable);
  End;
End;


Procedure TWPTrackableList.InternalBeforeExclude(iIndex : Integer; oObject : TFslObject);
Var
  oTrackable : TWPTrackable;
Begin
  If Hooking Then
  Begin
    oTrackable := TWPTrackable(oObject);

    Change(ctListContents, oTrackable);
    oTrackable.UnHook(Change);
  End;
  Inherited InternalBeforeExclude(iIndex, oObject);
End;


Procedure TWPTrackableList.Assign(oSource: TFslObject);
Var
  iLoop : Integer;
Begin
  Inherited;
  For iLoop := 0 To Count - 1 Do
    Elements[iLoop].Hook(Change);
End;


Function IsWordBreak(Const sWord : String) : Boolean;
Begin
  Result := (Length(sWord) = 1) And CharInSet(sWord[1], CHAR_WORD_BREAK);
End;

Constructor TWPHotspot.Create;
Begin
  Inherited Create;
End;

Destructor TWPHotspot.Destroy;
Begin
  Inherited;
End;


Constructor TWPHotspot.Create(Const sURL : String);
Begin
  Create;
  FURL := sURL;
End;

Constructor TWPHotspot.Create(Const sURL : String; iLinkColour : TColour; iHoverColour : TColour);
Begin
  Create;
  FURL := sURL;
  FLinkColour := iLinkColour;
  FHoverColour := iHoverColour;
End;


Constructor TWPHotspot.Create(Const sURL, sKey : String; iLinkColour : TColour; iHoverColour : TColour);
Begin
  Create;
  FURL := sURL;
  FKey := sKey;
  FLinkColour := iLinkColour;
  FHoverColour := iHoverColour;
End;


Procedure TWPHotspot.AfterConstruction;
Begin
  Inherited;

  Defaults;
End;


Function TWPHotspot.IsEmpty : Boolean;
Begin
  Result := (FURL = '') And (FKey = '') And (FLinkColour = DEF_COLOUR) And (FHoverColour = DEF_COLOUR) And (FTitle = '');
End;


Procedure TWPHotspot.Defaults;
Begin
  LinkColour := DEF_COLOUR;
  HoverColour := DEF_COLOUR;
  FURL := '';
  FKey := '';
  FTitle := '';
  FLinkUnderline := True;
End;


Function TWPHotspot.Link : TWPHotspot;
Begin
  Result := TWPHotspot(Inherited Link);
End;


Function TWPHotspot.Clone : TWPHotspot;
Begin
  Result := TWPHotspot(Inherited Clone);
End;



Procedure TWPHotspot.Assign(oObject: TFslObject);
Begin
  Inherited;
  URL := TWPHotspot(oObject).URL;
  Key := TWPHotspot(oObject).Key;
  LinkColour := TWPHotspot(oObject).LinkColour;
  HoverColour := TWPHotspot(oObject).HoverColour;
  Title := TWPHotspot(oObject).Title;
  FLinkUnderline := TWPHotspot(oObject).LinkUnderline;
End;



function TWPHotspot.describe: string;
begin
  if self = nil then
    result := '(nil)'
  else
  begin
    prop(result, 'url', FURL);
    prop(result, 'title', FTitle);
  end;
end;

Procedure TWPHotspot.SetKey(Const Value: String);
Begin
  // Make sure key is a 1 character Uppercase,
  FKey := StringUpper(Value);
  If Length(FKey) > 1 Then
    Delete(FKey, 2, Length(FKey) - 1);

  // and can only be one of 'A'..'Z' or '0'..'9'
  If (Length(FKey) > 0) And (Not CharInSet(FKey[1], ['A'..'Z', '0'..'9'])) Then
     Delete(FKey, 1, 1);

  Change(ctPresentation, Self);
End;


Procedure TWPHotspot.SetURL(Const Value: String);
Var
  aType : TChangeType;
Begin
  If (FUrl = '') <> (Value = '') Then
    aType := ctLayout
  Else
    aType := ctPresentation;
  FURl := Value;
  Change(aType, Self);
End;


Procedure TWPHotspot.SetLinkColour(Const Value: TColour);
Begin
  FLinkColour := Value;
  Change(ctPresentation, Self);
End;


Procedure TWPHotspot.SetHoverColour(Const Value: TColour);
Begin
  FHoverColour := Value;
  Change(ctPresentation, Self);
End;


Function TWPHotspot.WorkingColour(oFocus: TWPHotspot): TColour;
Begin
  If (oFocus = Self) And (FHoverColour <> DEF_COLOUR) Then
    Result := FHoverColour
  Else
    Result := FLinkColour;
End;


Procedure TWPHotspot.SetTitle(Const Value: String);
Begin
  FTitle := Value;
  Change(ctPresentation, Self);
End;


function TWPHotspot.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FURL.length * sizeof(char)) + 12);
  inc(result, (FKey.length * sizeof(char)) + 12);
  inc(result, (FTitle.length * sizeof(char)) + 12);
end;

Function TWPHotspotList.Link : TWPHotspotList;
Begin
  Result := TWPHotspotList(Inherited Link);
End;


Function TWPHotspotList.Clone : TWPHotspotList;
Begin
  Result := TWPHotspotList(Inherited Clone);
End;


Function TWPHotspotList.New : TWPHotspot;
Begin
  Result := TWPHotspot(Inherited New);
End;


Function TWPHotspotList.ItemClass : TFslObjectClass;
Begin
  Result := TWPHotspot;
End;


Function TWPHotspotList.GetElement(Const iIndex : Integer) : TWPHotspot;
Begin
  Result := TWPHotspot(ObjectByIndex[iIndex]);
End;


Procedure TWPHotspotList.SetElement(Const iIndex : Integer; Const oValue : TWPHotspot);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPHotspotList.Get(Const aValue : Integer) : TWPHotspot;
Begin
  Result := TWPHotspot(Inherited Get(aValue));
End;


Constructor TWPDataItemMap.Create;
Begin
  Inherited;

  Symbol := '|';
  Separator := '=';
  Forced := True;
End;


Function TWPDataItemMap.Link : TWPDataItemMap;
Begin
  Result := TWPDataItemMap(Inherited Link);
End;


Function TWPDataItemMap.Clone : TWPDataItemMap;
Begin
  Result := TWPDataItemMap(Inherited Clone);
End;

{ TWPImageMap }

Procedure TWPImageMap.Assign(oObject: TFslObject);
Begin
  Inherited;
  Assert(Invariants('Object', oObject, TWPImageMap, 'assign'));
  FAreas.Assign(TWPImageMap(oObject).FAreas);
End;

Function TWPImageMap.Clone: TWPImageMap;
Begin
  Result := TWPImageMap(Inherited Clone);
End;

Constructor TWPImageMap.Create;
Begin
  Inherited;
  FAreas := TWPImageMapAreaList.Create;
  FAreas.Hook(Change);
End;

Destructor TWPImageMap.Destroy;
Begin
  FAreas.UnHook(Change);
  FAreas.Free;
  Inherited;
End;

Function TWPImageMap.Link: TWPImageMap;
Begin
  Result := TWPImageMap(Inherited Link);
End;


Function TWPImageMap.ContainsHotspot(oHotspot : TWPHotspot) : Boolean;
Var
  iLoop : Integer;
Begin
  Result := False;
  For iLoop := 0 To FAreas.Count - 1 Do
    Result := Result Or (FAreas[iLoop] = oHotspot);
End;


Function TWPImageMap.HasSelection: Boolean;
Var
  iLoop : Integer;
Begin
  Result := False;
  For iLoop := 0 To FAreas.Count - 1 Do
    Result := Result Or (FAreas[iLoop].Selected);
End;


Function TWPImageMap.Selection: TWPImageMapArea;
Var
  iLoop : Integer;
Begin
  Result := Nil;
  iLoop := 0;
  While (Result = Nil) And (iLoop < FAreas.Count) Do
  Begin
    If FAreas[iLoop].Selected Then
      Result := FAreas[iLoop];
    Inc(iLoop);
  End;
End;


Function TWPImageMap.GetAreaByURL(Const sURL : String): TWPImageMapArea;
Var
  iLoop : Integer;
Begin
  Result := Nil;
  iLoop := 0;
  While (Result = Nil) And (iLoop < FAreas.Count) Do
  Begin
    If FAreas[iLoop].URL = sURL Then
      Result := FAreas[iLoop];
    Inc(iLoop);
  End;
End;


Procedure TWPImageMap.Select(oArea: TWPImageMapArea);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To FAreas.Count - 1 Do
    FAreas[iLoop].Selected := False;
  oArea.Selected := True;
End;


function TWPImageMap.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FAreas.sizeInBytes);
end;

{ TWPImageMapArea }

Constructor TWPImageMapArea.Create;
Begin
  Inherited;
  FCoordinates := TWPCoordinateList.Create;
  FCoordinates.Hook(Change);
End;

Destructor TWPImageMapArea.Destroy;
Begin
  FCoordinates.UnHook(Change);
  FCoordinates.Free;
  Inherited;
End;

Procedure TWPImageMapArea.Assign(oObject: TFslObject);
Begin
  Inherited;
  FCoordinates.Assign(TWPImageMapArea(oObject).FCoordinates);
  Selected := TWPImageMapArea(oObject).Selected;
End;

Function TWPImageMapArea.Clone: TWPImageMapArea;
Begin
  Result := TWPImageMapArea(Inherited Clone);
End;

Function TWPImageMapArea.Link: TWPImageMapArea;
Begin
  Result := TWPImageMapArea(Inherited Link);
End;



function TWPImageMapArea.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCoordinates.sizeInBytes);
end;

Function TWPImageMapAreaList.Link : TWPImageMapAreaList;
Begin
  Result := TWPImageMapAreaList(Inherited Link);
End;


Function TWPImageMapAreaList.Clone : TWPImageMapAreaList;
Begin
  Result := TWPImageMapAreaList(Inherited Clone);
End;


Function TWPImageMapAreaList.New : TWPImageMapArea;
Begin
  Result := TWPImageMapArea(Inherited New);
End;


Function TWPImageMapAreaList.ItemClass : TFslObjectClass;
Begin
  Result := TWPImageMapArea;
End;


Function TWPImageMapAreaList.GetElement(Const iIndex : Integer) : TWPImageMapArea;
Begin
  Result := TWPImageMapArea(ObjectByIndex[iIndex]);
End;


Procedure TWPImageMapAreaList.SetElement(Const iIndex : Integer; Const oValue : TWPImageMapArea);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPImageMapAreaList.Get(Const aValue : Integer) : TWPImageMapArea;
Begin
  Result := TWPImageMapArea(Inherited Get(aValue));
End;

Function TWPImageMapArea.Contains(iX, iY: Integer): Boolean;
Begin
 Result := FCoordinates.Contains(iX, iY);
End;


Procedure TWPImageMapArea.SetSelected(Const Value: Boolean);
Begin
  If FSelected <> Value Then
  Begin
    FSelected := Value;
    Change(ctPresentation, Self);
  End;
End;

{ TWPCoordinate }

function TWPCoordinate.AsPoint: TPoint;
begin
  result.x := x;
  result.y := y;
end;

Procedure TWPCoordinate.Assign(oObject: TFslObject);
Begin
  Inherited;
  Assert(Invariants('Object', oObject, TWPCoordinate, 'assign'));
  FX := TWPCoordinate(oObject).FX;
  FY := TWPCoordinate(oObject).FY;
End;

Function TWPCoordinate.Clone: TWPCoordinate;
Begin
  Result := TWPCoordinate(Inherited Clone);
End;

Function TWPCoordinate.Link: TWPCoordinate;
Begin
  Result := TWPCoordinate(Inherited Link);
End;



function TWPCoordinate.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

function TWPCoordinateList.LastIs(iX, iY: Integer): Boolean;
Begin
  Result := (Count > 0) And (Elements[Count-1].Y = iY) And (Elements[Count-1].X = iX);
end;


Function TWPCoordinateList.Link : TWPCoordinateList;
Begin
  Result := TWPCoordinateList(Inherited Link);
End;


Function TWPCoordinateList.Clone : TWPCoordinateList;
Begin
  Result := TWPCoordinateList(Inherited Clone);
End;


Function TWPCoordinateList.New : TWPCoordinate;
Begin
  Result := TWPCoordinate(Inherited New);
End;


Function TWPCoordinateList.ItemClass : TFslObjectClass;
Begin
  Result := TWPCoordinate;
End;


Function TWPCoordinateList.GetElement(Const iIndex : Integer) : TWPCoordinate;
Begin
  Result := TWPCoordinate(ObjectByIndex[iIndex]);
End;


Procedure TWPCoordinateList.SetElement(Const iIndex : Integer; Const oValue : TWPCoordinate);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPCoordinateList.Get(Const aValue : Integer) : TWPCoordinate;
Begin
  Result := TWPCoordinate(Inherited Get(aValue));
End;


Procedure TWPCoordinateList.Add(x, y: Integer);
Var
  oCoord : TWPCoordinate;
Begin
  oCoord := New;
  Try
    oCoord.x := x;
    oCoord.y := y;
    Add(oCoord.Link);
  Finally
    oCoord.Free;
  End;
End;

(*
Source for the logic:
  delphi: http://delphi.about.com/cs/adptips2001/a/bltip0601_5.htm
  java:   http://alienryderflex.com/polygon/
*)

Function TWPCoordinateList.Contains(iX, iY: Integer): Boolean;
Var
  K, J : Integer;
Begin
  Result := False;
  J := Self.Count-1;
  For K := 0 To Self.Count-1 Do
  Begin
    If ((Elements[K].Y <= iY) And (iY < Elements[J].Y)) Or
       ((Elements[J].Y <= iY) And (iY < Elements[K].Y)) Then
    Begin
      If (iX < (Elements[j].X - Elements[K].X) * (iY - Elements[K].Y) /
         (Elements[j].Y - Elements[K].Y) + Elements[K].X) Then
        Result := Not Result;
    End;
  J := K;
  End;
End;

{ TWPDocumentImageAdornment }

procedure TWPDocumentImageAdornment.Assign(oObject: TFslObject);
begin
  inherited;
  FId := TWPDocumentImageAdornment(oObject).FId;
  FPenColour := TWPDocumentImageAdornment(oObject).FPenColour;
  FPenWidth := TWPDocumentImageAdornment(oObject).FPenWidth;
  FPenStyle := TWPDocumentImageAdornment(oObject).FPenStyle;
  FCaption := TWPDocumentImageAdornment(oObject).FCaption;
  FAdornmentType := TWPDocumentImageAdornment(oObject).FAdornmentType;

  FCoordinates.Assign(TWPDocumentImageAdornment(oObject).FCoordinates);
  FCaptionPoint.Assign(TWPDocumentImageAdornment(oObject).FCaptionPoint);
  FFont.Assign(TWPDocumentImageAdornment(oObject).FFont);
end;

function TWPDocumentImageAdornment.Clone: TWPDocumentImageAdornment;
begin
  result := TWPDocumentImageAdornment(Inherited Clone);
end;

constructor TWPDocumentImageAdornment.Create;
begin
  inherited;
  FId := GUIDToString(CreateGUID);
  FCoordinates := TWPCoordinateList.Create;
  FFont := TWPSFontDetails.Create;
  FCaptionPoint := TWPCoordinate.Create;
end;


function TWPDocumentImageAdornment.Describe: String;
begin
  if self = nil then
    result := '(nil)'
  else
  begin
    result := IMAGE_ADORNMENT_TYPE_CODES[FAdornmentType];
  end;
end;

destructor TWPDocumentImageAdornment.Destroy;
begin
  FCaptionPoint.Free;
  FCoordinates.Free;
  FFont.Free;
  inherited;
end;

function TWPDocumentImageAdornment.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FId.length * sizeof(char)) + 12);
  inc(result, FCoordinates.sizeInBytes);
  inc(result, FCaptionPoint.sizeInBytes);
  inc(result, FFont.sizeInBytes);
  inc(result, (FCaption.length * sizeof(char)) + 12);
end;

Function TWPDocumentImageAdornments.Link : TWPDocumentImageAdornments;
Begin
  Result := TWPDocumentImageAdornments(Inherited Link);
End;


Function TWPDocumentImageAdornments.Clone : TWPDocumentImageAdornments;
Begin
  Result := TWPDocumentImageAdornments(Inherited Clone);
End;


Function TWPDocumentImageAdornments.New : TWPDocumentImageAdornment;
Begin
  Result := TWPDocumentImageAdornment(Inherited New);
End;


Function TWPDocumentImageAdornments.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentImageAdornment;
End;


Function TWPDocumentImageAdornments.GetElement(Const iIndex : Integer) : TWPDocumentImageAdornment;
Begin
  Result := TWPDocumentImageAdornment(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentImageAdornments.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentImageAdornment);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentImageAdornments.Get(Const aValue : Integer) : TWPDocumentImageAdornment;
Begin
  Result := TWPDocumentImageAdornment(Inherited Get(aValue));
End;

function TWPDocumentImageAdornment.Link: TWPDocumentImageAdornment;
begin
  result := TWPDocumentImageAdornment(Inherited Link);
end;


procedure TWPDocumentImageAdornment.Move(aPart : TAdornmentPart; aAction : TAdornmentAction; iX, iY: Integer);
var
  i : integer;
begin
  if aPart <> apCaption Then
  Begin
    case FAdornmentType of
      iatLine:
        Begin
          if aAction <> aaMove Then
            RaiseError('Move', 'Not supported');
          For i := 0 to Coordinates.Count - 1 Do
          Begin
            Coordinates[i].X := Coordinates[i].X + iX;
            Coordinates[i].Y := Coordinates[i].Y + iY;
          End;
        End;
      iatMark :
        Begin
          if aAction <> aaMove Then
            RaiseError('Move', 'Not supported');
          For i := 0 to Coordinates.Count - 1 Do
          Begin
            Coordinates[i].X := Coordinates[i].X + iX;
            Coordinates[i].Y := Coordinates[i].Y + iY;
          End;
        End;
      iatRectangle, iatCircle:
        Case aAction of
          aaMove :
            Begin
            Coordinates[0].X := Coordinates[0].X + iX;
            Coordinates[0].Y := Coordinates[0].Y + iY;
            Coordinates[1].X := Coordinates[1].X + iX;
            Coordinates[1].Y := Coordinates[1].Y + iY;
            End;
        aaDragTop :
            Coordinates[0].Y := Coordinates[0].Y + iY;
        aaDragBottom :
            Coordinates[1].Y := Coordinates[1].Y + iY;
        aaDragLeft :
            Coordinates[0].X := Coordinates[0].X + iX;
        aaDragRight :
            Coordinates[1].X := Coordinates[1].X + iX;
        aaDragTopLeft :
            Begin
            Coordinates[0].X := Coordinates[0].X + iX;
            Coordinates[0].Y := Coordinates[0].Y + iY;
            End;
        aaDragTopRight :
            Begin
            Coordinates[0].Y := Coordinates[0].Y + iY;
            Coordinates[1].X := Coordinates[1].X + iX;
            End;
        aaDragBottomLeft :
            Begin
            Coordinates[0].X := Coordinates[0].X + iX;
            Coordinates[1].Y := Coordinates[1].Y + iY;
            End;
        aaDragBottomRight :
            Begin
            Coordinates[1].X := Coordinates[1].X + iX;
            Coordinates[1].Y := Coordinates[1].Y + iY;
            End;
        End;
      iatZoom :
        Begin
        Case aAction of
          aaMove :
            Begin
            if aPart in [apAll, apPrimary] Then
            Begin
              Coordinates[0].X := Coordinates[0].X + iX;
              Coordinates[0].Y := Coordinates[0].Y + iY;
              Coordinates[1].X := Coordinates[1].X + iX;
              Coordinates[1].Y := Coordinates[1].Y + iY;
            End;
            if aPart in [apAll, apZoom] Then
            Begin
              Coordinates[2].X := Coordinates[2].X + iX;
              Coordinates[2].Y := Coordinates[2].Y + iY;
              Coordinates[3].X := Coordinates[3].X + iX;
              Coordinates[3].Y := Coordinates[3].Y + iY;
            End;
            End;
        aaDragTop :
            if aPart = apPrimary Then
              Coordinates[0].Y := Coordinates[0].Y + iY
            else
              Coordinates[2].Y := Coordinates[2].Y + iY;
        aaDragBottom :
            if aPart = apPrimary Then
              Coordinates[1].Y := Coordinates[1].Y + iY
            else
              Coordinates[3].Y := Coordinates[3].Y + iY;
        aaDragLeft :
            if aPart = apPrimary Then
              Coordinates[0].X := Coordinates[0].X + iX
            else
              Coordinates[2].X := Coordinates[2].X + iX;
        aaDragRight :
            if aPart = apPrimary Then
              Coordinates[1].X := Coordinates[1].X + iX
            else
              Coordinates[3].X := Coordinates[3].X + iX;
        aaDragTopLeft :
            if aPart = apPrimary Then
            Begin
              Coordinates[0].X := Coordinates[0].X + iX;
              Coordinates[0].Y := Coordinates[0].Y + iY;
            End
            else
            Begin
              Coordinates[2].X := Coordinates[2].X + iX;
              Coordinates[2].Y := Coordinates[2].Y + iY;
            End;
        aaDragTopRight :
            if aPart = apPrimary Then
            Begin
              Coordinates[0].Y := Coordinates[0].Y + iY;
              Coordinates[1].X := Coordinates[1].X + iX;
            End
            else
            Begin
              Coordinates[2].Y := Coordinates[2].Y + iY;
              Coordinates[3].X := Coordinates[3].X + iX;
            End;
        aaDragBottomLeft :
            if aPart = apPrimary Then
            Begin
              Coordinates[0].X := Coordinates[0].X + iX;
              Coordinates[1].Y := Coordinates[1].Y + iY;
            End
            else
            Begin
              Coordinates[2].X := Coordinates[2].X + iX;
              Coordinates[3].Y := Coordinates[3].Y + iY;
            End;
        aaDragBottomRight :
            if aPart = apPrimary Then
            Begin
              Coordinates[1].X := Coordinates[1].X + iX;
              Coordinates[1].Y := Coordinates[1].Y + iY;
            End
            else
            Begin
              Coordinates[3].X := Coordinates[3].X + iX;
              Coordinates[3].Y := Coordinates[3].Y + iY;
            End;
          End;
        End;
    End;
  End;
  if (aPart in [apAll, apCaption]) And (aAction = aaMove) Then
  Begin
    CaptionPoint.X := CaptionPoint.X + iX;
    CaptionPoint.Y := CaptionPoint.Y + iY;
  End;
end;

function TWPDocumentImageAdornments.GetById(const sId: String): TWPDocumentImageAdornment;
var
  i : integer;
begin
  result := nil;
  i := 0;
  While (result = nil) and (i < Count) Do
  Begin
    if Elements[i].Id = sId Then
      result := Elements[i];
    inc(i);
  End;
end;

function TWPDocumentImageAdornments.IndexById(const sId: String): Integer;
var
  i : integer;
begin
  result := -1;
  i := 0;
  While (result = -1) and (i < Count) Do
  Begin
    if Elements[i].Id = sId Then
      result := i;
    inc(i);
  End;
end;

{ TWPStyles }

Function TWPStyles.Append : TWPStyle;
Begin
  Result := TWPStyle.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TWPStyles.AddItem(value : TWPStyle);
Begin
  Add(value.Link);
End;

Function TWPStyles.IndexOf(value : TWPStyle) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TWPStyles.Insert(iIndex : Integer) : TWPStyle;
Begin
  Result := TWPStyle.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TWPStyles.InsertItem(iIndex : Integer; value : TWPStyle);
begin
  Inherited Insert(iIndex, value);
End;

Function TWPStyles.Item(iIndex : Integer) : TWPStyle;
Begin
  Result := StyleByIndex[iIndex];
End;

Procedure TWPStyles.SetItemByIndex(iIndex : Integer; value: TWPStyle);
Begin
  StyleByIndex[iIndex] := value;
End;

Procedure TWPStyles.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TWPStyles.ClearItems;
Begin
  Clear;
End;

function TWPStyles.Count: Integer;
begin
  result := Inherited Count;
end;


procedure TWPStyles.SetStyleByIndex(const iIndex: Integer; oStyle: TWPStyle);
begin
  Inherited ObjectByIndex[iIndex] := oStyle;
end;

Function TWPStyles.AddNamed(Const sName: String): TWPStyle;
Begin
  Result := TWPStyle.Create;
  Try
    Result.Name := sName;
    Add(Result.Link);
  Finally
    Result.Free;
  End;
End;


{ TWPMouseInfo }

Destructor TWPMouseInfo.Destroy;
Begin
  FAdornment.Free;
  FHotspot.Free;
  FSubject.Free;
  Inherited;
End;

Procedure TWPMouseInfo.SetSubject(Const Value: TFslObject);
Begin
  FSubject.Free;
  FSubject := Value;
End;

Procedure TWPMouseInfo.SetHotspot(Const Value: TWPHotspot);
Begin
  FHotspot.Free;
  FHotspot := Value;
End;

procedure TWPMouseInfo.SetAdornment(const Value: TWPDocumentImageAdornment);
begin
  FAdornment.Free;
  FAdornment := Value;
end;

function TWPMouseInfo.Link: TWPMouseInfo;
begin
  result := TWPMouseInfo(Inherited Link);
end;

function TWPMouseInfo.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FHotspot.sizeInBytes);
  inc(result, FSubject.sizeInBytes);
  inc(result, FAdornment.sizeInBytes);
  inc(result, (FHint.length * sizeof(char)) + 12);
end;

Procedure TWPWordIterator.Init(Const sText : String);
Begin
  FText := sText;
  FCursor := 0;
End;

Function TWPWordIterator.More : Boolean;
Begin
  Result := FCursor < Length(FText);
End;


Function TWPWordIterator.Next : String;
Var
  iC : Integer;
Begin
  If Not More Then
    RaiseError('Next', 'Next called when no more exists');

  iC:= FCursor + 1;
  If CharInSet(FText[iC], CHAR_WORD_BREAK) or (FText[iC] = #11) Then
  Begin
    Result := FText[iC];
    Inc(FCursor);
  End
  Else If (FText[iC] = #13) Then
  Begin
    Result := FText[iC];
    Inc(FCursor);
    If (FCursor < Length(FText)) And (FText[FCursor+1] = #10) Then
      Inc(FCursor);
  End
  Else
  Begin
    Repeat
      Inc(iC);
    Until (iC > Length(FText)) Or (iC > FCursor + MAX_WORD_LENGTH) Or CharInSet(FText[iC], CHAR_WORD_BREAK) or (FText[iC] = #11) Or (FText[iC] = #13);

    If iC > Length(FText) Then
    Begin
      Result := Copy(FText, FCursor+1, MaxInt);
      FCursor := Length(FText);
    End
    Else
    Begin
      Result := Copy(FText, FCursor+1, iC - FCursor-1);
      FCursor := iC-1;
    End;
  End;
End;




function TWPWordIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FText.length * sizeof(char)) + 12);
end;

Procedure TWPLineIterator.Init(const sText : String);
Begin
  FText := sText;
  FCursor := 0;
End;

Function TWPLineIterator.More : Boolean;
Begin
  Result := FCursor < length(FText);
End;


Function TWPLineIterator.Next : String;
var
  iC : integer;
Begin
  if not More then
    RaiseError('Next', 'Next called when no more exists');

  iC:= FCursor + 1;
  if CharInSet(FText[iC], [#13, #10]) Then
  Begin
    Result := FText[iC];
    inc(FCursor);
    if (Result = #13) and (iC < length(FText)) and (FText[iC+1] = #10) Then
      inc(FCursor);
  End
  Else
  Begin
    Repeat
      Inc(iC);
    Until (iC > Length(FText)) or CharInSet(FText[iC], [#13, #10]);

    If iC > Length(FText) Then
    Begin
      Result := copy(FText, FCursor+1, MaxInt);
      FCursor := Length(FText);
    End
    Else
    Begin
      Result := copy(FText, FCursor+1, iC - FCursor-1);
      FCursor := iC-1;
    End;
  End;
End;



function TWPLineIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FText.length * sizeof(char)) + 12);
end;

Function TWPCompletionItem.Link : TWPCompletionItem;
Begin
  Result := TWPCompletionItem(Inherited Link);
End;


Function TWPCompletionItem.Clone : TWPCompletionItem;
Begin
  Result := TWPCompletionItem(Inherited Clone);
End;


Procedure TWPCompletionItem.Assign(oObject : TFslObject);
Begin
  Inherited;

  Code := TWPCompletionItem(oObject).Code;
  Description := TWPCompletionItem(oObject).Description;
  Content := TWPCompletionItem(oObject).Content;
  Id := TWPCompletionItem(oObject).Id;
End;



Function TWPCompletionItem.ErrorClass : EFslExceptionClass;
Begin
  Result := EWPCompletionItem;
End;


Function TWPCompletionItems.Link : TWPCompletionItems;
Begin
  Result := TWPCompletionItems(Inherited Link);
End;


Function TWPCompletionItems.Clone : TWPCompletionItems;
Begin
  Result := TWPCompletionItems(Inherited Clone);
End;


Function TWPCompletionItems.New : TWPCompletionItem;
Begin
  Result := TWPCompletionItem(Inherited New);
End;


Function TWPCompletionItems.ItemClass : TFslObjectClass;
Begin
  Result := TWPCompletionItem;
End;


Function TWPCompletionItems.GetElement(Const iIndex : Integer) : TWPCompletionItem;
Begin
  Result := TWPCompletionItem(ObjectByIndex[iIndex]);
End;


Procedure TWPCompletionItems.SetElement(Const iIndex : Integer; Const oValue : TWPCompletionItem);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPCompletionItems.CompareByCode(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(TWPCompletionItem(pA).Code, TWPCompletionItem(pB).Code);
End;


Function TWPCompletionItems.CompareByDescription(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(TWPCompletionItem(pA).Description, TWPCompletionItem(pB).Description);
End;


Function TWPCompletionItems.IndexByCode(Const aValue : TWPCompletionItemCode) : Integer;
Var
  oElement : TWPCompletionItem;
Begin
  oElement := New;
  Try
    oElement.Code := aValue;

    If Not Find(oElement, Result, CompareByCode) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;


Function TWPCompletionItems.IndexByDescription(Const aValue : TWPCompletionItemDescription) : Integer;
Var
  oElement : TWPCompletionItem;
Begin
  oElement := New;
  Try
    oElement.Description := aValue;

    If Not Find(oElement, Result, CompareByDescription) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;


Function TWPCompletionItems.Get(Const aValue : Integer) : TWPCompletionItem;
Begin
  Result := TWPCompletionItem(Inherited Get(aValue));
End;


Function TWPCompletionItems.GetByCode(Const aValue : TWPCompletionItemCode) : TWPCompletionItem;
Begin
  Result := Get(IndexByCode(aValue));
End;


Function TWPCompletionItems.GetByDescription(Const aValue : TWPCompletionItemDescription) : TWPCompletionItem;
Begin
  Result := Get(IndexByDescription(aValue));
End;


Function TWPCompletionItems.ExistsByCode(Const aValue : TWPCompletionItemCode) : Boolean;
Begin
  Result := ExistsByIndex(IndexByCode(aValue));
End;


Function TWPCompletionItems.ExistsByDescription(Const aValue : TWPCompletionItemDescription) : Boolean;
Begin
  Result := ExistsByIndex(IndexByDescription(aValue));
End;


Procedure TWPCompletionItems.SortedByCode;
Begin
  SortedBy(CompareByCode);
End;


Procedure TWPCompletionItems.SortedByDescription;
Begin
  SortedBy(CompareByDescription);
End;


Function TWPCompletionItems.IsSortedByCode : Boolean;
Begin
  Result := IsSortedBy(CompareByCode);
End;


Function TWPCompletionItems.IsSortedByDescription : Boolean;
Begin
  Result := IsSortedBy(CompareByDescription);
End;


Function TWPCompletionItems.Add(sCode, sContent, sDescription: String): TWPCompletionItem;
Begin
  Result := New;
  Try
    Result.Code := sCode;
    Result.Description := sDescription;
    Result.Content := sContent;
    Add(Result.Link);
  Finally
    Result.Free;
  End;
End;

Function TWPCompletionItem.Display: String;
Begin
  If Description <> '' Then
    Result := Code +' - ' +Description
  Else
    Result := Code;
End;


Destructor TWPObservers.Destroy;
Begin
  If Count > 0 Then
    RaiseError('Destroy', 'You must unregister all the observers prior to freeing a Word Processor ('+IntegerToString(Count)+' found)');

  Inherited;
End;

Function TWPObservers.CompareByObject(pa, pB: Pointer): Integer;
Begin
  Result := Integer(TWPObserver(pA).FId) - Integer(TWPObserver(pB).FId);
End;

Function TWPObservers.ExistsByObject(oObject: TObject): Boolean;
Begin
  Result := ExistsByIndex(IndexByObject(oObject));
End;

Function TWPObservers.GetByObject(oObject: TObject): TWPObserver;
Begin
  Result := TWPObserver(Get(IndexByObject(oObject)));
End;

Function TWPObservers.GetObserver(iIndex: Integer): TWPObserver;
Begin
  Result := TWPObserver(ObjectByIndex[iIndex]);
End;

Function TWPObservers.IndexByObject(oObject: TObject): Integer;
Var
  oObserver : TWPObserver;
Begin
  oObserver := TWPObserver.Create;
  Try
    oObserver.Id := oObject;
    If Not Find(oObserver, Result, CompareByObject) Then
      Result := -1;
  Finally
    oObserver.Free;
  End;
End;

Function TWPObservers.ItemClass: TFslObjectClass;
Begin
  Result := TWPObserver;
End;

Procedure TWPObservers.Notify(oSender : TObject);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To Count - 1 Do
    Observer[iLoop].Event(oSender);
End;

Procedure TWPObservers.Register(oObject: TObject; aEvent: TNotifyEvent);
Var
  oObserver : TWPObserver;
Begin
  If ExistsByObject(oObject) Then
    RaiseError('Register', 'You cannot register an object more than once');
  oObserver := TWPObserver.Create;
  Try
    oObserver.Id := oObject;
    oObserver.Event := aEvent;
    Add(oObserver.Link);
  Finally
    oObserver.Free;
  End;
End;

Procedure TWPObservers.Unregister(oObject: TObject);
Var
  iIndex : Integer;
Begin
  iIndex := IndexByObject(oObject);
  If Not ExistsByIndex(iIndex) Then
    RaiseError('Register', 'You cannot unregister an object that has not been registered')
  Else
    DeleteByIndex(iIndex);
End;


Function TWPObserver.Clone: TWPObserver;
Begin
  Result := TWPObserver(Inherited Clone);
End;

Function TWPObserver.Link: TWPObserver;
Begin
  Result := TWPObserver(Inherited Link);
End;


Function TWPObserver.GetId : TObject;
Begin
  Assert(Assigned(FId));
  Result := FId;
End;


{ TWPProperty }

Destructor TWPProperty.Destroy;
Begin
  FChildren.Free;
  Inherited;
End;

Function TWPProperty.EnumAsVocab: String;
Var
  iLoop : Integer;
Begin
  Result := '';
  For iLoop := Low(FEnum) To High(FEnum) Do
  Begin
    If iLoop > 0 Then
      Result := Result + ',';
    Result := Result + FEnum[iLoop];
  End;
End;

Function TWPProperty.Link: TWPProperty;
Begin
  Result := TWPProperty(Inherited Link);
End;

Procedure TWPProperty.SetChildren(Const Value: TWPPropertyList);
Begin
  FChildren.Free;
  FChildren := Value;
End;

Procedure TWPProperty.SetKind(Const Value: TWPPropertyKind);
Begin
  FKind := Value;
  If (FKind = pkGroup) And (FChildren = Nil) Then
    FChildren := TWPPropertyList.Create;
End;

function TWPProperty.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FChildren.sizeInBytes);
end;

{ TWPPropertyList }

Procedure TWPPropertyList.Add(Const iId : Integer; Const bEditable : Boolean; Const sName: String; aKind: TWPPropertyKind; Const sValue: String);
Var
  oProp : TWPProperty;
Begin
  oProp := TWPProperty.Create;
  Try
    oProp.Id := iId;
    oProp.Editable := bEditable;
    oProp.Name := sName;
    oProp.Kind := aKind;
    oProp.Value := sValue;
    Add(oProp.Link);
  Finally
    oProp.Free;
  End;
End;

Procedure TWPPropertyList.AddBoolean(Const iId : Integer; Const bEditable : Boolean; Const sName: String; Const bValue: Boolean);
Const BOOL_NAMES : Array [0..1] Of String = ('yes', 'no'); // not that this is not in the normal order, but in the designer order
Begin
  If bValue Then
    AddEnum(iId, bEditable, sName, 1, BOOL_NAMES)
  Else
    AddEnum(iId, bEditable, sName, 0, BOOL_NAMES);
End;

Procedure TWPPropertyList.AddColour(Const iId : Integer; Const bEditable : Boolean; Const sName: String; Const aValue: TColour);
Var
  oProp : TWPProperty;
Begin
  oProp := TWPProperty.Create;
  Try
    oProp.Id := iId;
    oProp.Editable := bEditable;
    oProp.Name := sName;
    oProp.Kind := pkColour;
    If aValue <> DEF_COLOUR Then
      oProp.Value := ColourToHTMLColourString(aValue);
    Add(oProp.Link);
  Finally
    oProp.Free;
  End;
End;

Procedure TWPPropertyList.AddEnum(Const iId : Integer; Const bEditable : Boolean; Const sName: String; Const iIndex: Integer; Const aNames : Array Of String);
Var
  oProp : TWPProperty;
  aEnum : TEnumerationValues;
  iLoop : Integer;
Begin
  SetLength(aEnum, Length(aNames));
  For iLoop := Low(aNames) To High(aNames) Do
    aEnum[iLoop] := aNames[iLoop];

  oProp := TWPProperty.Create;
  Try
    oProp.Id := iId;
    oProp.Editable := bEditable;
    oProp.Name := sName;
    oProp.Kind := pkEnum;
    oProp.Value := aNames[iIndex];
    oProp.Enum := aEnum;
    Add(oProp.Link);
  Finally
    oProp.Free;
  End;
End;

Procedure TWPPropertyList.AddEnum(Const iId : Integer; Const bEditable : Boolean; Const sName: String; Const sValue : String; Const aNames : TStringList);
Var
  oProp : TWPProperty;
  aEnum : TEnumerationValues;
  iLoop : Integer;
Begin
  SetLength(aEnum, aNames.Count);
  For iLoop := 0 To aNames.Count - 1  Do
    aEnum[iLoop] := aNames[iLoop];

  oProp := TWPProperty.Create;
  Try
    oProp.Id := iId;
    oProp.Editable := bEditable;
    oProp.Name := sName;
    oProp.Kind := pkEnum;
    oProp.Value := sValue;
    oProp.Enum := aEnum;
    Add(oProp.Link);
  Finally
    oProp.Free;
  End;
End;

Procedure TWPPropertyList.AddEnum(Const iId : Integer; Const bEditable : Boolean; Const sName: String; Const sValue : String; Const aNames : TFslNameList);
Var
  oProp : TWPProperty;
  aEnum : TEnumerationValues;
  iLoop : Integer;
Begin
  SetLength(aEnum, aNames.Count);
  For iLoop := 0 To aNames.Count - 1  Do
    aEnum[iLoop] := aNames[iLoop].Name;

  oProp := TWPProperty.Create;
  Try
    oProp.Id := iId;
    oProp.Editable := bEditable;
    oProp.Name := sName;
    oProp.Kind := pkEnum;
    oProp.Value := sValue;
    oProp.Enum := aEnum;
    Add(oProp.Link);
  Finally
    oProp.Free;
  End;
End;

Function TWPPropertyList.AddGroup(Const sName: String): TWPPropertyList;
Var
  oProp : TWPProperty;
Begin
  oProp := TWPProperty.Create;
  Try
    oProp.Editable := False;
    oProp.Name := sName;
    oProp.Kind := pkGroup;
    Add(oProp.Link);
    Result := oProp.Children;
  Finally
    oProp.Free;
  End;
End;

Procedure TWPPropertyList.AddInteger(Const iId : Integer; Const bEditable : Boolean; Const sName: String; Const iValue: Integer);
Var
  oProp : TWPProperty;
Begin
  oProp := TWPProperty.Create;
  Try
    oProp.Id := iId;
    oProp.Editable := bEditable;
    oProp.Name := sName;
    oProp.Kind := pkInteger;
    oProp.Value := IntegerToString(iValue);
    Add(oProp.Link);
  Finally
    oProp.Free;
  End;
End;

Procedure TWPPropertyList.AddWord(Const iId : Integer; Const bEditable : Boolean; Const sName: String; Const iValue: Word);
Var
  oProp : TWPProperty;
Begin
  oProp := TWPProperty.Create;
  Try
    oProp.Id := iId;
    oProp.Editable := bEditable;
    oProp.Name := sName;
    oProp.Kind := pkInteger;
    If (iValue <> DEF_WORD) Then
      oProp.Value := IntegerToString(iValue);
    Add(oProp.Link);
  Finally
    oProp.Free;
  End;
End;

Procedure TWPPropertyList.AddFloat(Const iId : Integer; Const bEditable : Boolean; Const sName : String; Const rValue : Real);
Var
  oProp : TWPProperty;
Begin
  oProp := TWPProperty.Create;
  Try
    oProp.Id := iId;
    oProp.Editable := bEditable;
    oProp.Name := sName;
    oProp.Kind := pkInteger;
    oProp.Value := RealToString(rValue);
    Add(oProp.Link);
  Finally
    oProp.Free;
  End;
End;

Procedure TWPPropertyList.AddName(Const iId : Integer; Const bEditable : Boolean; Const sName, sValue: String);
Var
  oProp : TWPProperty;
Begin
  oProp := TWPProperty.Create;
  Try
    oProp.Id := iId;
    oProp.Editable := bEditable;
    oProp.Name := sName;
    oProp.Kind := pkName;
    oProp.Value := sValue;
    Add(oProp.Link);
  Finally
    oProp.Free;
  End;
End;

Procedure TWPPropertyList.AddString(Const iId : Integer; Const bEditable : Boolean; Const sName, sValue: String);
Var
  oProp : TWPProperty;
Begin
  oProp := TWPProperty.Create;
  Try
    oProp.Id := iId;
    oProp.Editable := bEditable;
    oProp.Name := sName;
    oProp.Kind := pkString;
    oProp.Value := sValue;
    Add(oProp.Link);
  Finally
    oProp.Free;
  End;
End;

Procedure TWPPropertyList.AddComplex(Const iId : Integer; Const sName, sValue: String);
Var
  oProp : TWPProperty;
Begin
  oProp := TWPProperty.Create;
  Try
    oProp.Id := iId;
    oProp.Editable := False;
    oProp.Name := sName;
    oProp.Kind := pkComplex;
    oProp.Value := sValue;
    Add(oProp.Link);
  Finally
    oProp.Free;
  End;
End;

Procedure TWPPropertyList.AddTristate(Const iId: Integer; Const bEditable: Boolean; Const sName: String; Const aValue: TWPSTriState);
Const TRISTATE_NAMES : Array [0..2] Of String = ('yes', 'no', '');  // not that this is not in the normal order, but in the designer order
Begin
  Case aValue Of
    tsUnknown : AddEnum(iId, bEditable, sName, 2, TRISTATE_NAMES);
    tsTrue : AddEnum(iId, bEditable, sName, 0, TRISTATE_NAMES);
    tsFalse : AddEnum(iId, bEditable, sName, 1, TRISTATE_NAMES);
  End;
End;

Procedure TWPPropertyList.AddToken(Const iId : Integer; Const bEditable : Boolean; Const sName, sValue: String);
Var
  oProp : TWPProperty;
Begin
  oProp := TWPProperty.Create;
  Try
    oProp.Id := iId;
    oProp.Editable := bEditable;
    oProp.Name := sName;
    oProp.Kind := pkToken;
    oProp.Value := sValue;
    Add(oProp.Link);
  Finally
    oProp.Free;
  End;
End;

Function TWPPropertyList.GetProperty(iIndex: Integer): TWPProperty;
Begin
  Result := TWPProperty(ObjectByIndex[iIndex]);
End;

Function TWPPropertyList.ItemClass: TFslObjectClass;
Begin
  Result := TWPProperty;
End;

{ TWPSortDetail }

Constructor TWPSortDetail.Create(Const iIndex: Integer; Const bIsAscend: Boolean);
Begin
  Create;

  FSortIndex := iIndex;
  FIsAscend := bIsAscend;
End;

function TWPSortDetail.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TWPSortDetailList }

Function TWPSortDetailList.ItemClass: TFslObjectClass;
Begin
  Result := TWPSortDetail;
End;

Procedure TWPSortDetailList.Assign(oSource: TFslObject);
Begin
  Inherited;
End;

Function TWPSortDetailList.Clone: TWPSortDetailList;
Begin
  Result := TWPSortDetailList(Inherited Clone);
End;

Function TWPSortDetailList.Link: TWPSortDetailList;
Begin
  Result := TWPSortDetailList(Inherited Link);
End;

Function TWPSortDetailList.Get(Const aValue: Integer): TWPSortDetail;
Begin
  Result := TWPSortDetail(Get(aValue));
End;

Function TWPSortDetailList.GetElement(Const iIndex: Integer): TWPSortDetail;
Begin
  Result := TWPSortDetail(ObjectByIndex[iIndex]);
End;

Procedure TWPSortDetailList.SetElement(Const iIndex: Integer; Const oValue: TWPSortDetail);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;

{ Static methods }

Function SortTextArray(Const oSorts : TWPSortDetailList; Const oData1: TStringList; Const oData2: TStringList): Integer;
Var
  iLoop: Integer;
  oSort: TWPSortDetail;
  sData1, sData2: String;
Begin
  Result := 0;
  For iLoop := 0 to oSorts.Count - 1 Do
  Begin
    oSort := oSorts[iLoop];
    sData1 := oData1[oSort.SortIndex];
    sData2 := oData2[oSort.SortIndex];

    If Not (sData1 = sData2) Then
    Begin
      If (oSort.IsAscend And (sData1 > sData2)) Or ((Not oSort.IsAscend) And (sData1 < sData2)) Then
        Result := 1
      Else
        Result := -1;

      Break;    // sort circuit
    End;
  End;
End;


Procedure SortTextArrays(Const oSorts: TWPSortDetailList; Const aData: Array Of TStringList; Var aSortedPos: Array Of Integer);
Var
  iLoop1, iLoop2: Integer;
  iTemp: Integer;
  oTemp: TStringList;
Begin
  Assert(Length(aData) = Length(aSortedPos), 'SortTextArrays: Size of input array must equal to size of output result');

  // init.
  For iLoop1 := 0 To Length(aSortedPos) - 1 Do
    aSortedPos[iLoop1] := iLoop1;

  oTemp := TStringList.Create;
  Try
    For iLoop1 := 0 To Length(aData) - 2 Do
    Begin
      For iLoop2 := iLoop1 + 1 To Length(aData) - 1 Do
        If SortTextArray(oSorts, aData[iLoop1], aData[iLoop2]) < 0 Then
        Begin
          // exchange index position
          iTemp := aSortedPos[iLoop1];
          aSortedPos[iLoop1] := aSortedPos[iLoop2];
          aSortedPos[iLoop2] := iTemp;

          // exchange data
          oTemp.Assign(aData[iLoop1]);
          aData[iLoop1].Assign(aData[iLoop2]);
          aData[iLoop2].Assign(oTemp);
        End;
    End;
  Finally
    oTemp.Free;
  End;
End;



Constructor TWPRendererTableColumnMetric.Create;
Begin
  Inherited;
  FDeadLefts := TFslIntegerList.Create;
  FDeadRights := TFslIntegerList.Create;
End;


Destructor TWPRendererTableColumnMetric.Destroy;
Begin
  FDeadLefts.Free;
  FDeadRights.Free;
  Inherited;
End;


Function TWPRendererTableColumnMetric.Link : TWPRendererTableColumnMetric;
Begin
  Result := TWPRendererTableColumnMetric(Inherited Link);
End;

Function TWPRendererTableColumnMetric.DeadLeft : Integer;
var
  iLoop : Integer;
Begin
  Result := 1;
  For iLoop := 0 to FDeadLefts.Count - 1 do
    result := IntegerMax(Result, FDeadLefts[iLoop]);
End;


Function TWPRendererTableColumnMetric.DeadRight : Integer;
var
  iLoop : Integer;
Begin
  Result := 1;
  For iLoop := 0 to FDeadRights.Count - 1 do
    result := IntegerMax(Result, FDeadRights[iLoop]);
End;


Function TWPRendererTableColumnMetric.ErrorClass : EFslExceptionClass;
Begin
  Result := EWPRendererTableColumnMetric;
End;


function TWPRendererTableColumnMetric.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDeadLefts.sizeInBytes);
  inc(result, FDeadRights.sizeInBytes);
end;

Function TWPTableColumnMetrics.Link : TWPTableColumnMetrics;
Begin
  Result := TWPTableColumnMetrics(Inherited Link);
End;


Function TWPTableColumnMetrics.Clone : TWPTableColumnMetrics;
Begin
  Result := TWPTableColumnMetrics(Inherited Clone);
End;


Function TWPTableColumnMetrics.New : TWPRendererTableColumnMetric;
Begin
  Result := TWPRendererTableColumnMetric(Inherited New);
End;


Function TWPTableColumnMetrics.ItemClass : TFslObjectClass;
Begin
  Result := TWPRendererTableColumnMetric;
End;


Function TWPTableColumnMetrics.GetElement(Const iIndex : Integer) : TWPRendererTableColumnMetric;
Begin
  Result := TWPRendererTableColumnMetric(ObjectByIndex[iIndex]);
End;


Function TWPTableColumnMetrics.Get(Const aValue : Integer) : TWPRendererTableColumnMetric;
Begin
  Result := TWPRendererTableColumnMetric(Inherited Get(aValue));
End;


Function TWPTableColumnMetrics.SumActual : Integer;
Var
  iLoop : Integer;
Begin
  Result := 0;
  For iLoop := 0 To Count - 1 Do
    Result := Result + Elements[iLoop].Actual;
End;


Constructor TWPFieldModel.Create;
Begin
  Inherited;

  FEntries := TWPFieldEntryList.Create;
End;


Destructor TWPFieldModel.Destroy;
Begin
  FEntries.Free;

  Inherited;
End;


Function TWPFieldModel.GetEntries : TWPFieldEntryList;
Begin
  Assert(Invariants('GetEntries', FEntries, TWPFieldEntryList, 'FEntries'));
  Result := FEntries;
End;


Function TWPFieldModel.Clone: TWPFieldModel;
Begin
  Result := TWPFieldModel(Inherited Clone);
End;


Function TWPFieldModel.Link: TWPFieldModel;
Begin
  Result := TWPFieldModel(Inherited Link);
End;


function TWPFieldModel.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FTitle.length * sizeof(char)) + 12);
  inc(result, FEntries.sizeInBytes);
end;

Function TWPFieldEntryList.GetFieldName(iIndex: Integer): TWPFieldEntry;
Begin
  Result := TWPFieldEntry(ObjectByIndex[iIndex]);
End;


Function TWPFieldEntryList.ItemClass: TFslObjectClass;
Begin
  Result := TWPFieldEntry;
End;


{ TWPFieldEntry }

constructor TWPFieldEntry.Create(const sCode, sName, sDescription: String; const bSection: Boolean);
begin
  Create;
  Name := sName;
  FCode := sCode;
  FDescription := sDescription;
  FSection := bSection;
end;

function TWPFieldEntry.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeof(char)) + 12);
  inc(result, (FDescription.length * sizeof(char)) + 12);
end;

function TWPFieldEntryList.Link: TWPFieldEntryList;
begin
  Result := TWPFieldEntryList(Inherited Link);
end;

End.
