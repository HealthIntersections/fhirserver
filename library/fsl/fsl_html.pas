Unit fsl_html;

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

{$i fhir.inc}

Interface

Uses
  SysUtils,
  fsl_base, fsl_utilities, fsl_stream, fsl_collections, fsl_xml;


Type
  TFslHTMLToken = (ahtNull, ahtWhitespace, ahtComment, ahtIdentifier, ahtNumber,
    ahtOpenTag, ahtCloseTag, ahtOpenTagForwardSlash, ahtCloseTagForwardSlash,
    ahtOpenTagExclamation, ahtOpenTagQuery, ahtCloseTagQuery, ahtForwardSlash, ahtBackwardSlash, ahtExclamation,
    ahtQuery, ahtEquals, ahtPeriod, ahtComma, ahtColon, ahtSemiColon, ahtUnderscore, ahtHyphen,
    ahtAmpersand, ahtPercentage, ahtHash, ahtAt, ahtDollar, ahtCaret, ahtAsterisk,
    ahtOpenRound, ahtCloseRound, ahtOpenSquare, ahtCloseSquare, ahtOpenBrace, ahtCloseBrace,
    ahtPlus, ahtTilde, ahtBackQuote, ahtSingleQuote, ahtDoubleQuote, ahtPipe);

  TFslHTMLTokens = Set Of TFslHTMLToken;


Function ToHTMLToken(cChar : Char) : TFslHTMLToken; Overload;


Const
  HTMLTOKEN_STRING : Array[TFslHTMLToken] Of String =
    ('Null', 'Whitespace', 'Comment', 'Identifier', 'Number',
      '<', '>', '</', '/>',
      '<!', '<?', '?>', '/', '\', '!',
      '?', '=', '.', ',', ':', ';', '_', '-',
      '&', '%', '#', '@', '$', '^', '*',
      '(', ')', '[', ']', '{', '}',
      '+', '~', '`', '''', '"', '|');

  HTMLTOKEN_SYMBOL_SET = [ahtForwardSlash, ahtBackwardSlash, ahtExclamation,
    ahtQuery, ahtEquals, ahtPeriod, ahtComma, ahtColon, ahtSemiColon, ahtUnderscore, ahtHyphen,
    ahtAmpersand, ahtPercentage, ahtHash, ahtAt, ahtDollar, ahtCaret, ahtAsterisk,
    ahtOpenRound, ahtCloseRound, ahtOpenSquare, ahtCloseSquare, ahtOpenBrace, ahtCloseBrace,
    ahtPlus, ahtTilde, ahtBackQuote, ahtSingleQuote, ahtDoubleQuote, ahtPipe];

  HTMLTOKEN_BODY_SET = HTMLTOKEN_SYMBOL_SET + [ahtIdentifier, ahtNumber, ahtWhitespace];

Type
  TFslCSSValues =  Class(TFslStringMatch)
    Private
      Function GetMatch(Const sKey: String): String;
      Procedure SetMatch(Const sKey, sValue: String);

    Public
      constructor Create; Override;

      Property Matches[Const sKey : String] : String Read GetMatch Write SetMatch; Default;
  End;

  TFslCSSFragment = Class (TFslObject)
    Private
      FValues : TFslCSSValues;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslCSSFragment;
      Function Clone : TFslCSSFragment;

      Procedure Assign(oSource : TFslObject); Override;

      Property Values : TFslCSSValues Read FValues;
  End;

  TFslCSSFragments = Class(TFslObjectList)
    Private
      Function GetFragment(iIndex: Integer): TFslCSSFragment;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property Fragments[iIndex : Integer] : TFslCSSFragment Read GetFragment; Default;
  End;

  TFslCSSStyle = Class(TFslCSSFragment)
    Private
      FName : String;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      Function Link : TFslCSSStyle;
      Function Clone : TFslCSSStyle;

      Procedure Assign(oSource : TFslObject); Override;

      Property Name : String Read FName Write FName;
  End;

  TFslCSSStyles = Class (TFslCSSFragments)
    Private
      Function GetScript(iIndex: Integer): TFslCSSStyle;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property Styles[iIndex : Integer] : TFslCSSStyle Read GetScript; Default;
  End;

  TFslCSSClasses = Class(TFslStringList)
  End;


Type
  TFslHTMLAttributes = Class(TFslStringMatch)
    Private
      Function GetAttribute(Const sKey: String): String;
      Procedure SetAttribute(Const sKey, sValue: String);

    Public
      constructor Create; Override;

      Property Attribute[Const sKey : String] : String Read GetAttribute Write SetAttribute; Default;
  End;



Type
  TFslHTMLAlign = (haUnspecified, haLeft, haCenter, haRight, haJustify, haTop, haBottom);
  TFslHTMLBreakClearType = (bctNone, bctLeft, bctRight, bctAll);
  TFslHTMLHeadingLevel = (ahlOne, ahlTwo, ahlThree, ahlFour, ahlFive, ahlSix);
  TFslHTMLOrderedListType = (ltArabic {1}, ltLowerAlpha {a}, ltUpperAlpha {A}, ltLowerRoman {i}, ltUpperRoman {I});
  TFslHTMLUnorderedListType = (ltDisc, ltSquare, ltCircle);
  TFslHTMLTableSectionType = (tstBody, tstHead, tstFoot);
  TFslHTMLFormInputType = (fitText, fitPassword, fitCheckbox, fitRadio, fitSubmit, fitReset, fitFile, fitHidden, fitImage, fitButton);
  TFslHTMLFormButtonType = (fbtSubmit, fbtReset, fbtButton);
  TFslHTMLFormMethod = (fmGet, fmPost);

  // support the use of Case Statements
  TFslHTMLElementType = (ahError, ahComment, ahTextFragment, ahEntity, ahImageReference, ahAnchor,
                         ahBreak, ahSection, ahInLineSection, ahBlockSection, {ahLink, to be put back inb - this is a header link not an <a>}
                         ahSpan, ahEmphasis, ahStrong, ahCitation, ahDefinition,
                         ahCode, ahSample, ahKeyboardText, ahVariable, ahAbbrevation,
                         ahAcronym, ahTeleType, ahItalic, ahBold, ahBig,
                         ahSmall, ahStrikeOut, ahUnderline, ahFont, ahQuotation,
                         ahSuperScript, ahSubScript, ahBlockQuote, ahHorizontalRule, ahDiv,
                         ahParagraph, ahPreformatted, ahHeading, ahListItem, ahListElement,
                         ahOrderedList, ahUnOrderedList, ahDefinitionListItem, ahDefinitionList, ahTableCell,
                         ahTableRow, ahTableSection, ahTableCaption, ahTable, ahFormControl,
                         ahInput, ahButton, ahOptionItem, ahOptGroup, ahOption,
                         ahSelect, ahTextArea, ahForm, ahBody);

Const
  ADVHTMLALIGN_NAMES : Array [TFslHTMLAlign] Of String = ('', 'Left', 'Center', 'Right', 'Justify', 'Top', 'Bottom');
  ADVHTMLBREAKCLEARTYPE_NAMES : Array [TFslHTMLBreakClearType] Of String = ('None', 'Left', 'Right', 'All');
  ADVHTMLHEADINGLEVEL_NAMES : Array [TFslHTMLHeadingLevel] Of String = ('H1', 'H2', 'H3', 'H4', 'H5', 'H6');
  ADVHTMLORDEREDLISTTYPE_NAMES : Array [TFslHTMLOrderedListType] Of String = ('Arabic', 'LowerAlpha', 'UpperAlpha', 'LowerRoman', 'UpperRoman');
  ADVHTMLUNORDEREDLISTTYPE_NAMES : Array [TFslHTMLUnorderedListType] Of String = ('Disc', 'Square', 'Circle');
  ADVHTMLTABLESECTIONTYPE_NAMES : Array [TFslHTMLTableSectionType] Of String = ('Body', 'Head', 'Foot');
  ADVHTMLFORMINPUTTYPE_NAMES : Array [TFslHTMLFormInputType] Of String = ('Text', 'Password', 'Checkbox', 'Radio', 'Submit', 'Reset', 'File', 'Hidden', 'Image', 'Button');
  ADVHTMLFORMBUTTONTYPE_NAMES : Array [TFslHTMLFormButtonType] Of String = ('Submit', 'Reset', 'Button');
  ADVHTMLFORMMETHOD_NAMES : Array [TFslHTMLFormMethod] Of String = ('Get', 'Post');


Type
  // all objects in the DOM descend from one of these 3 classes
  TFslHTMLObject = TFslObject;
  TFslHTMLList = TFslObjectList;
  TFslHTMLStrings = TFslStringList;

  TFslHTMLColour = String; // for the moment, but something encoded is preferred later


{------------- HTML DOM Elements ----------------------------------------------}

  // all html elements descend from this class, it supports the title="" attribute
  TFslHTMLElement = Class (TFslHTMLObject)
  Private
    FTitleAttribute : String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Procedure Assign(oSource : TFslObject); Override;

    Procedure Clear; Virtual;

    Property TitleAttribute : String Read FTitleAttribute Write FTitleAttribute; // all elements can have a title, though it often mightn't mean anything
  End;

  TFslHTMLElements = Class(TFslHTMLList)
    Private
      Function GetElement(iIndex: Integer): TFslHTMLElement;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property Element[iIndex : Integer] : TFslHTMLElement Read GetElement; Default;
  End;

  // all html body elements descend from this class. It supports
  // the attributes id=, class=, and style=
  TFslHTMLItem = Class (TFslHTMLElement)
  Private
    FId: String;
    FClasses: TFslCSSClasses;
    FStyle: TFslCSSFragment;
    Procedure SetClasses(Const oValue: TFslCSSClasses);
    Procedure SetStyle(Const oValue: TFslCSSFragment);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TFslHTMLItem;
    Function Clone : TFslHTMLItem;
    Procedure Assign(oSource : TFslObject); Override;
    Function ElementType : TFslHTMLElementType; Virtual;

    Property Classes : TFslCSSClasses Read FClasses Write SetClasses; // though there would usually only be one
    Property Id : String Read FId Write FId;
    Property Style : TFslCSSFragment Read FStyle Write SetStyle;
//    property WorkingStyles : TCSSWorkingStyles read FWorkingStyles; // parser to ignore
  End;

  TFslHTMLItems = Class (TFslHTMLElements)
  Private
    Function GetHTMLItem(iIndex: Integer): TFslHTMLItem;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Property Item[iIndex : Integer] : TFslHTMLItem Read GetHTMLItem; Default;
  End;

{------------- HTML Simple Elements -------------------------------------------}

  // These 3 classes are descendents from TFslHTMLItem for convenience (so they
  // can be members of a TFslHTMLItems, not because they can carry id, style, and classes
  TFslHTMLComment = Class (TFslHTMLItem)
  Private
    FText: String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLComment;
    Function Clone : TFslHTMLComment;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Text : String Read FText Write FText; // may include cReturns, interpret literally
  End;

  // any text
  TFslHTMLTextFragment = Class (TFslHTMLItem)
  Private
    FText: String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLTextFragment;
    Function Clone : TFslHTMLTextFragment;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Text : String Read FText Write FText; // do not collapse whitespace (because of pre)
  End;

  // it's not possible to collapse entities into the raw text stream, some
  // have information processing significance
  // &ent;
  TFslHTMLEntity = Class (TFslHTMLItem)
  Private
    FEntity: String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLEntity;
    Function Clone : TFslHTMLEntity;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Entity : String Read FEntity Write FEntity; // may include cReturns
  End;

  // <img>
  TFslHTMLImage = Class (TFslHTMLItem)
  Private
    FHorizontalSpace: String;
    FAlternative : String;
    FLongDescription: String;
    FHeight: String;
    FBorder: String;
    FSource: String;
    FName: String;
    FVerticalSpace: String;
    FWidth: String;
    FAlign: TFslHTMLAlign;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLImage;
    Function Clone : TFslHTMLImage;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Align : TFslHTMLAlign Read FAlign Write FAlign;  // Top, Middle, Bottom, Left, Right
    Property Alternative : String Read FAlternative Write FAlternative;
    Property Border : String Read FBorder Write FBorder;
    Property Height : String Read FHeight Write FHeight;
    Property HorizontalSpace : String Read FHorizontalSpace Write FHorizontalSpace;
    Property LongDescription : String Read FLongDescription Write FLongDescription;
    Property Name : String Read FName Write FName;
    Property Source : String Read FSource Write FSource;
    Property VerticalSpace : String Read FVerticalSpace Write FVerticalSpace;
    Property Width : String Read FWidth Write FWidth;
  End;

  // <br>
  TFslHTMLBreak = Class (TFslHTMLItem)
  Private
    FClearType: TFslHTMLBreakClearType;
  Public
    Function Link : TFslHTMLBreak;
    Function Clone : TFslHTMLBreak;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property ClearType : TFslHTMLBreakClearType Read FClearType Write FClearType;
  End;


{------------- HTML Sections --------------------------------------------------}

  // base element of recursiveness in html structure. There is a few rules
  // about what Sections can contain other sections, but these are vague and
  // sometimes are makde across several layers (i.e. no forms in forms), so
  // it's rarely possible to represent such constraints in this object model
  TFslHTMLSection = Class (TFslHTMLItem)
  Private
    FItems: TFslHTMLItems;
    Procedure SetItems(Const oValue: TFslHTMLItems);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TFslHTMLSection;
    Function Clone : TFslHTMLSection;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Procedure Clear; Override;

    Property Items : TFslHTMLItems Read FItems Write SetItems;
  End;

  TFslHTMLSections = Class (TFslHTMLItems);

  // TFslHTMLInlineSection is an abstract concept, no direct HTML equivalent
  // Inline sections fit into the text around them in an inline fashion
  TFslHTMLInLineSection = Class (TFslHTMLSection);

  TFslHTMLInLineSections = Class (TFslHTMLSections);

  // TFslHTMLBlockSection is an abstract concept, no direct HTML equivalent
  // Block sections Start a new line
  TFslHTMLBlockSection = Class (TFslHTMLSection);

  TFslHTMLBlockSections = Class (TFslHTMLSections);

{------------- HTML Inline Sections -------------------------------------------}

  // <a name="" href="">
  TFslHTMLAnchor = Class (TFslHTMLInLineSection)
  Private
    FURL: String;
    FName: String;
    FAccessKey: String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLAnchor;
    Function Clone : TFslHTMLAnchor;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property URL : String Read FURL Write FURL;
    Property Name : String Read FName Write FName;
    Property AccessKey : String Read FAccessKey Write FAccessKey;
  End;

  // <span>
  TFslHTMLSpan = Class (TFslHTMLInLineSection)
  Private
    FAlign: TFslHTMLAlign;
  Public
    Function Link : TFslHTMLSpan;
    Function Clone : TFslHTMLSpan;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property align : TFslHTMLAlign Read FAlign Write FAlign;  // Left, Center, Right, Justify
  End;

  // <em> </em> - italics
  TFslHTMLEmphasis = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLEmphasis;
    Function Clone : TFslHTMLEmphasis;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <strong< </strong> - bold
  TFslHTMLStrong = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLStrong;
    Function Clone : TFslHTMLStrong;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <cit></cite>
  TFslHTMLCitation = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLCitation;
    Function Clone : TFslHTMLCitation;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <dfn></dfn>
  TFslHTMLDefinition = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLDefinition;
    Function Clone : TFslHTMLDefinition;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <code></code>
  TFslHTMLCode = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLCode;
    Function Clone : TFslHTMLCode;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <samp></samp>
  TFslHTMLSample = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLSample;
    Function Clone : TFslHTMLSample;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <kdb></kdb>
  TFslHTMLKeyboardText = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLKeyboardText;
    Function Clone : TFslHTMLKeyboardText;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <var></var>
  TFslHTMLVariable = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLVariable;
    Function Clone : TFslHTMLVariable;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <abbr></abbr>
  TFslHTMLAbbrevation = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLAbbrevation;
    Function Clone : TFslHTMLAbbrevation;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <Acronym></Acronym>
  TFslHTMLAcronym = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLAcronym;
    Function Clone : TFslHTMLAcronym;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <tt></tt> -  fixed width
  TFslHTMLTeleType = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLTeleType;
    Function Clone : TFslHTMLTeleType;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <i></i>
  TFslHTMLItalic = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLItalic;
    Function Clone : TFslHTMLItalic;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <b></b>
  TFslHTMLBold = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLBold;
    Function Clone : TFslHTMLBold;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <big></big>
  TFslHTMLBig = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLBig;
    Function Clone : TFslHTMLBig;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <small></small>
  TFslHTMLSmall = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLSmall;
    Function Clone : TFslHTMLSmall;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <s></s> or <strike></strike>
  TFslHTMLStrikeOut = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLStrikeOut;
    Function Clone : TFslHTMLStrikeOut;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <u></u>
  TFslHTMLUnderline = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLUnderline;
    Function Clone : TFslHTMLUnderline;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  TFslHTMLFont = Class (TFslHTMLInLineSection)
  Private
    FSize: String;
    FFace: String;
    FColour: TFslHTMLColour;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLFont;
    Function Clone : TFslHTMLFont;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Color : TFslHTMLColour Read FColour Write FColour;
    Property Face : String Read FFace Write FFace; // leave as comma separated
    Property Size : String Read FSize Write FSize;
  End;

  // <q></q>
  TFslHTMLQuotation = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLQuotation;
    Function Clone : TFslHTMLQuotation;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <super></super>
  TFslHTMLSuperScript = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLSuperScript;
    Function Clone : TFslHTMLSuperScript;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <sub></sub>
  TFslHTMLSubScript = Class (TFslHTMLInLineSection)
  Public
    Function Link : TFslHTMLSubScript;
    Function Clone : TFslHTMLSubScript;
    Function ElementType : TFslHTMLElementType; Override;
  End;

{------------- HTML Block Sections --------------------------------------------}

  // <blockQuote></blockQuote> - usually indented
  TFslHTMLBlockQuote = Class (TFslHTMLBlockSection)
  Public
    Function Link : TFslHTMLBlockQuote;
    Function Clone : TFslHTMLBlockQuote;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <hr>
  TFslHTMLHorizontalRule = Class (TFslHTMLBlockSection)
  Private
    FNoShade: Boolean;
    FSize: String;
    FWidth: String;
    FAlign: TFslHTMLAlign;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLHorizontalRule;
    Function Clone : TFslHTMLHorizontalRule;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Align : TFslHTMLAlign Read FAlign Write FAlign; // Left|middle|right
    Property NoShade : Boolean Read FNoShade Write FNoShade;
    Property Size : String Read FSize Write FSize;
    Property Width : String Read FWidth Write FWidth;
  End;

  // <div>
  TFslHTMLContainer = Class (TFslHTMLBlockSection)
  Private
    FAlign: TFslHTMLAlign;
  Public
    Function Link : TFslHTMLContainer;
    Function Clone : TFslHTMLContainer;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property align : TFslHTMLAlign Read FAlign Write FAlign; // Left, Center, Right, Justify
  End;

  // <p>
  // parser note : Paragraphs cannot contain paragraphs, but they can contain Pre and Div (and div can contain paragraphs....)
  TFslHTMLParagraph = Class (TFslHTMLContainer)
  Public
    Function Link : TFslHTMLParagraph;
    Function Clone : TFslHTMLParagraph;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <pre>
  TFslHTMLPreformatted = Class (TFslHTMLParagraph)
  Public
    Function Link : TFslHTMLPreformatted;
    Function Clone : TFslHTMLPreformatted;
    Function ElementType : TFslHTMLElementType; Override;
  End;

  // <h[N]></h[N]>
  TFslHTMLHeading = Class (TFslHTMLBlockSection)
  Private
    FLevel: TFslHTMLHeadingLevel;
  Public
    Function Link : TFslHTMLHeading;
    Function Clone : TFslHTMLHeading;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Level : TFslHTMLHeadingLevel Read FLevel Write FLevel;
  End;

{------------- HTML Lists -----------------------------------------------------}

  // <li>
  TFslHTMLListItem = Class (TFslHTMLBlockSection)
  Private
    FValue: Integer;
    FHasValue : Boolean;
    Function GetHasValue: Boolean;
    Function GetValue: Integer;
    Procedure SetHasValue(Const Value: Boolean);
    Procedure SetValue(Const Value: Integer);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLListItem;
    Function Clone : TFslHTMLListItem;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Value : Integer Read GetValue Write SetValue; // only has meaning for OL lists.
    Property HasValue : Boolean Read GetHasValue Write SetHasValue; // whether value is defined
  End;

  TFslHTMLListItems = Class (TFslHTMLBlockSections)
  Private
    Function GetListItem(iIndex: Integer): TFslHTMLListItem;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Property item[iIndex : Integer] : TFslHTMLListItem Read GetListItem; Default;
  End;

  // no html equivalent
  TFslHTMLListElement = Class (TFslHTMLBlockSection)
  Private
    FCompact: Boolean;
    FList: TFslHTMLListItems;
    Procedure SetList(Const oValue: TFslHTMLListItems);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TFslHTMLListElement;
    Function Clone : TFslHTMLListElement;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Compact : Boolean Read FCompact Write FCompact;
    Property List : TFslHTMLListItems Read FList Write SetList;
  End;

  // <ol>
  TFslHTMLOrderedList = Class (TFslHTMLListElement)
  Private
    FStart: Integer;
    FListType: TFslHTMLOrderedListType;
  Public
    Function Link : TFslHTMLOrderedList;
    Function Clone : TFslHTMLOrderedList;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property listType : TFslHTMLOrderedListType Read FListType Write FListType;
    Property Start : Integer Read FStart Write FStart;
  End;

  // <ul>
  TFslHTMLUnorderedList = Class (TFslHTMLListElement)
  Private
    FListType: TFslHTMLUnorderedListType;
  Public
    Function Link : TFslHTMLUnorderedList;
    Function Clone : TFslHTMLUnorderedList;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property listType : TFslHTMLUnorderedListType Read FListType Write FListType;
  End;

  // <dt><dd> the <dd> section is the ancestor content
  TFslHTMLDefinitionListItem = Class (TFslHTMLInlineSection)
  Private
    FTerm: TFslHTMLItem;
    Procedure SetTerm(Const oValue: TFslHTMLItem);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TFslHTMLDefinitionListItem;
    Function Clone : TFslHTMLDefinitionListItem;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Term : TFslHTMLItem Read FTerm Write SetTerm;
  End;

  TFslHTMLDefinitionListItems = Class (TFslHTMLInlineSections)
  Private
    Function GetListItem(iIndex: Integer): TFslHTMLDefinitionListItem;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Property item[iIndex : Integer] : TFslHTMLDefinitionListItem Read GetListItem; Default;
  End;

  // <dl>
  TFslHTMLDefinitionList = Class (TFslHTMLBlockSection)
  Private
    FListItems: TFslHTMLDefinitionListItems;
    Procedure SetListItems(Const oValue: TFslHTMLDefinitionListItems);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TFslHTMLDefinitionList;
    Function Clone : TFslHTMLDefinitionList;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property ListItems : TFslHTMLDefinitionListItems Read FListItems Write SetListItems;
  End;

{------------- HTML Tables ----------------------------------------------------}

  //  <td></td>  or <th></th>  ( = isHeader)
  TFslHTMLTableCell = Class (TFslHTMLBlockSection)
  Private
    FNoWrap: Boolean;
    FRowspan: String;
    FColspan: String;
    FWidth: String;
    FHeight: String;
    FVertAlign: TFslHTMLAlign;
    FHorizAlign: TFslHTMLAlign;
    FBgColour: TFslHTMLColour;
    FIsHeader: Boolean;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLTableCell;
    Function Clone : TFslHTMLTableCell;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property BgColor : TFslHTMLColour Read FBgColour Write FBgColour;
    Property ColSpan : String Read FColspan Write FColspan;
    Property Height : String Read FHeight Write FHeight;
    Property HorizAlign : TFslHTMLAlign Read FHorizAlign Write FHorizAlign;
    Property IsHeader : Boolean Read FIsHeader Write FIsHeader;
    Property NoWrap : Boolean Read FNoWrap Write FNoWrap;
    Property RowSpan : String Read FRowspan Write FRowspan;
    Property VertAlign : TFslHTMLAlign Read FVertAlign Write FVertAlign;
    Property Width : String Read FWidth Write FWidth;
  End;

  TFslHTMLTableCells = Class (TFslHTMLBlockSections)
  Private
    Function GetCell(iIndex: Integer): TFslHTMLTableCell;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Property Cell[iIndex : Integer] : TFslHTMLTableCell Read GetCell; Default;
  End;

  // <tr></tr>
  TFslHTMLTableRow = Class (TFslHTMLItem)
  Private
    FVertAlign: TFslHTMLAlign;
    FHorizAlign: TFslHTMLAlign;
    FBgColour: TFslHTMLColour;
    FCells: TFslHTMLTableCells;
    Procedure SetCells(Const oValue: TFslHTMLTableCells);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TFslHTMLTableRow;
    Function Clone : TFslHTMLTableRow;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property BgColor : TFslHTMLColour Read FBgColour Write FBgColour;
    Property Cells : TFslHTMLTableCells Read FCells Write SetCells;
    Property HorizAlign : TFslHTMLAlign Read FHorizAlign Write FHorizAlign;
    Property VertAlign : TFslHTMLAlign Read FVertAlign Write FVertAlign;
  End;

  TFslHTMLTableRows = Class (TFslHTMLItems)
  Private
    Function GetRow(iIndex: Integer): TFslHTMLTableRow;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Property Row[iIndex : Integer] : TFslHTMLTableRow Read GetRow; Default;
  End;

  // <THead>, <TBody>, <TFoot>
  TFslHTMLTableSection = Class (TFslHTMLItem)
  Private
    FRows: TFslHTMLTableRows;
    FSectionType: TFslHTMLTableSectionType;
    Procedure SetRows(Const oValue: TFslHTMLTableRows);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TFslHTMLTableSection;
    Function Clone : TFslHTMLTableSection;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Rows : TFslHTMLTableRows Read FRows Write SetRows;
    Property SectionType : TFslHTMLTableSectionType Read FSectionType Write FSectionType;
  End;

  TFslHTMLTableSections = Class (TFslHTMLItems)
  Private
    Function GetSection(iIndex: Integer): TFslHTMLTableSection;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Property Section[iIndex : Integer] : TFslHTMLTableSection Read GetSection; Default;
  End;

  // <caption></caption>
  TFslHTMLTableCaption = Class (TFslHTMLBlockSection)
  Private
    FAlign: TFslHTMLAlign;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLTableCaption;
    Function Clone : TFslHTMLTableCaption;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Align : TFslHTMLAlign Read FAlign Write FAlign; //Top, Bottom, Left, Right
  End;

  // <table></table>
  TFslHTMLTable = Class (TFslHTMLBlockSection)
  Private
    FSummary: String;
    FWidth: String;
    FCellpadding: String;
    FCellspacing: String;
    FBorder: String;
    FAlign: TFslHTMLAlign;
    FBgColour: TFslHTMLColour;
    FCaption: TFslHTMLTableCaption;
    FRows: TFslHTMLTableRows;
    FSections: TFslHTMLTableSections;
    Procedure SetCaption(Const oValue: TFslHTMLTableCaption);
    Procedure SetRows(Const oValue: TFslHTMLTableRows);
    Procedure SetSections(Const oValue: TFslHTMLTableSections);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TFslHTMLTable;
    Function Clone : TFslHTMLTable;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

  // todo: frames and rules, colGroups, Cols
    Property Align : TFslHTMLAlign Read FAlign Write FAlign;  // Left|middle|right
    Property AllRows : TFslHTMLTableRows Read FRows Write SetRows;
    Property BgColor : TFslHTMLColour Read FBgColour Write FBgColour;
    Property Border : String Read FBorder Write FBorder;
    Property Caption : TFslHTMLTableCaption Read FCaption Write SetCaption;
    Property Cellpadding : String Read FCellpadding Write FCellpadding;
    Property Cellspacing : String Read FCellspacing Write FCellspacing;
    Property Sections : TFslHTMLTableSections Read FSections Write SetSections;
    Property Summary : String Read FSummary Write FSummary;
    Property Width : String Read FWidth Write FWidth;
    Property Rows : TFslHTMLTableRows Read FRows;
  End;

{------------- HTML Forms -----------------------------------------------------}

  TFslHTMLFormControl = Class (TFslHTMLItem)
  Private
    FDisabled: Boolean;
    FTabIndex: String;
    FName: String;
    FValue: String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLFormControl;
    Function Clone : TFslHTMLFormControl;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Disabled : Boolean Read FDisabled Write FDisabled;
    Property Name : String Read FName Write FName;
    Property TabIndex : String Read FTabIndex Write FTabIndex;
    Property Value : String Read FValue Write FValue;
  End;

  // <input>
  TFslHTMLInput = Class (TFslHTMLFormControl)
  Private
    FReadOnly: Boolean;
    FChecked: Boolean;
    FSize: String;
    FAccept: String;
    FSource: String;
    FMaxLength: String;
    FAlternative: String;
    FAlign: TFslHTMLAlign;
    FInputType: TFslHTMLFormInputType;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLInput;
    Function Clone : TFslHTMLInput;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Accept : String Read FAccept Write FAccept;
    Property Align : TFslHTMLAlign Read FAlign Write FAlign;  // Left|middle|right
    Property Alternative : String Read FAlternative Write FAlternative;
    Property Checked : Boolean Read FChecked Write FChecked;
    Property InputType : TFslHTMLFormInputType Read FInputType Write FInputType;
    Property MaxLength : String Read FMaxLength Write FMaxLength;
    Property ReadOnly : Boolean Read FReadOnly Write FReadOnly;
    Property Size : String Read FSize Write FSize;
    Property Source : String Read FSource Write FSource;
  End;

  // <button>
  TFslHTMLButton = Class (TFslHTMLFormControl)
  Private
    FButtonType: TFslHTMLFormButtonType;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLButton;
    Function Clone : TFslHTMLButton;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property ButtonType : TFslHTMLFormButtonType Read FButtonType Write FButtonType;
  End;

  // html 4 does not allow OptGroups to recurse. This structure allows for 0 optgroups or all optgroups
  TFslHTMLOptionItem = Class (TFslHTMLItem) // TFslHTMLItem from DOM/HTML. What style etc might mean in this context, who knows?
  Private
    FDisabled: Boolean;
    FCaption: String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLOptionItem;
    Function Clone : TFslHTMLOptionItem;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Caption : String Read FCaption Write FCaption; // was "label" in html
    Property Disabled : Boolean Read FDisabled Write FDisabled;
  End;

  TFslHTMLOptionItems = Class (TFslHTMLItems)
  Private
    Function GetOption(iIndex: Integer): TFslHTMLOptionItem;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Property Option[iIndex : Integer] : TFslHTMLOptionItem Read GetOption; Default;
  End;

  // <optgroup>
  TFslHTMLOptGroup = Class (TFslHTMLOptionItem)
  Private
    FOptions: TFslHTMLOptionItems;
    Procedure SetOptions(Const oValue: TFslHTMLOptionItems);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TFslHTMLOptGroup;
    Function Clone : TFslHTMLOptGroup;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Options : TFslHTMLOptionItems Read FOptions Write SetOptions;
  End;

  // <option>
  TFslHTMLOption = Class (TFslHTMLOptionItem)
  Private
    FSelected: Boolean;
    FValue: String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLOption;
    Function Clone : TFslHTMLOption;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Selected : Boolean Read FSelected Write FSelected;
    Property Value : String Read FValue Write FValue;
  End;

  TFslHTMLSelect = Class (TFslHTMLFormControl)
  Private
    FMultiple: Boolean;
    FSize: String;
    FAllOptions: TFslHTMLOptionItems;
    FOptions: TFslHTMLOptionItems;
    Procedure SetAllOptions(Const oValue: TFslHTMLOptionItems);
    Procedure SetOptions(Const oValue: TFslHTMLOptionItems);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TFslHTMLSelect;
    Function Clone : TFslHTMLSelect;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property AllOptions : TFslHTMLOptionItems Read FAllOptions Write SetAllOptions;
    Property Multiple : Boolean Read FMultiple Write FMultiple;
    Property Options : TFslHTMLOptionItems Read FOptions Write SetOptions;
    Property Size : String Read FSize Write FSize;
  End;

  // <textarea>
  // still to decide - how do we handle the content?
  TFslHTMLTextArea = Class (TFslHTMLFormControl)
  Private
    FReadOnly: Boolean;
    FRows: Integer;
    FCols: Integer;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLTextArea;
    Function Clone : TFslHTMLTextArea;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Cols : Integer Read FCols Write FCols;
    Property ReadOnly : Boolean Read FReadOnly Write FReadOnly;
    Property Rows : Integer Read FRows Write FRows;
  End;

  TFslHTMLForm = Class (TFslHTMLBlockSection)// forms cannot be contained in other forms
  Private
    FAction: String;
    FEncType: String;
    FName: String;
    FMethod: TFslHTMLFormMethod;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLForm;
    Function Clone : TFslHTMLForm;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property Action : String Read FAction Write FAction;
    Property EncType : String Read FEncType Write FEncType;
    Property Method : TFslHTMLFormMethod Read FMethod Write FMethod;
    Property Name : String Read FName Write FName;

    // controls
  End;

{------------- HTML Document Content ------------------------------------------}

  TFslHTMLBody = Class (TFslHTMLBlockSection)
  Private
    FLinkColor: TFslHTMLColour;
    FVlinkcolor: TFslHTMLColour;
    FAlinkColor: TFslHTMLColour;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLBody;
    Function Clone : TFslHTMLBody;
    Function ElementType : TFslHTMLElementType; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property alinkColor : TFslHTMLColour Read FAlinkColor Write FAlinkColor;
    Property linkColor : TFslHTMLColour Read FLinkColor Write FLinkColor;
    Property vlinkcolor : TFslHTMLColour Read FVlinkcolor Write FVlinkcolor;
  End;

  TFslHTMLMetaEntry = Class (TFslHTMLElement)
  Private
    FHttpEquiv: String;
    FContent: String;
    FName: String;
    FScheme: String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Link : TFslHTMLMetaEntry;
    Function Clone : TFslHTMLMetaEntry;
    Procedure Assign(oSource : TFslObject); Override;

    Property Content : String Read FContent Write FContent;
    Property HttpEquiv : String Read FHttpEquiv Write FHttpEquiv;
    Property Name : String Read FName Write FName;
    Property Scheme : String Read FScheme Write FScheme;
  End;

  TFslHTMLMetaEntries = Class (TFslHTMLElements)
  Private
    Function GetMeta(iIndex: Integer): TFslHTMLMetaEntry;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Property Meta[iIndex : Integer] : TFslHTMLMetaEntry Read GetMeta; Default;
  End;

  TFslHTMLHead = Class (TFslHTMLElement)
  Private
    FMeta: TFslHTMLMetaEntries;
    FStyles: TFslCSSStyles;
    FTitle: String;
    Procedure SetMeta(Const oValue: TFslHTMLMetaEntries); // parser - collapse link and profile someday
    Procedure SetStyles(Const oValue: TFslCSSStyles);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TFslHTMLHead;
    Function Clone : TFslHTMLHead;
    Procedure Assign(oSource : TFslObject); Override;

    Procedure Clear; Override;

    Property Meta : TFslHTMLMetaEntries Read FMeta Write SetMeta;
    Property Styles : TFslCSSStyles Read FStyles Write SetStyles;
    Property Title : String Read FTitle Write FTitle;
  End;

  TFslHtmlDocument = Class (TFslHTMLElement)
  Private
    FDTD: String;
    FVersion: String;
    FStyles: TFslCSSStyles;
    FBody: TFslHTMLBody;
    FHead: TFslHTMLHead;
    Procedure SetBody(Const oValue: TFslHTMLBody);
    Procedure SetHead(Const oValue: TFslHTMLHead);
    Procedure SetStyles(Const oValue: TFslCSSStyles);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TFslHtmlDocument;
    Function Clone : TFslHtmlDocument;
    Procedure Assign(oSource : TFslObject); Override;

    Procedure Clear; Override;

    Property Version : String Read FVersion Write FVersion;
    Property DTD : String Read FDTD Write FDTD;
    Property Head : TFslHTMLHead Read FHead Write SetHead;
    Property Styles : TFslCSSStyles Read FStyles Write SetStyles;
    Property Body : TFslHTMLBody Read FBody Write SetBody;

    // From DOM....
{    property Images : THTMLCollection;
    property Applets : HTMLCollection;
    property Links : HTMLCollection;
    property Forms : HTMLCollection;
    Function ElementType : TFslHTMLElementType; override;
    property Anchors : HTMLCollection;

    Function ElementById(sId : String) : Element;
    Function ElementsByName(sName : String) : NodeList;}
  End;


Type
  TFslHTMLLexer = Class(TFslTextExtractor)
    Private
      FNextToken : TFslHTMLToken;
      FCurrentToken : TFslHTMLToken;
      FCurrentValue : String;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      Function ConsumeIdentifier : TFslHTMLToken;
      Function ConsumeWhitespace : TFslHTMLToken;
      Function ConsumeNumber : TFslHTMLToken;
      Function ConsumeSymbol : TFslHTMLToken;

      Function ConsumeToken : TFslHTMLToken; Overload;
      Function NextToken : TFslHTMLToken;

      Procedure ConsumeToken(aToken : TFslHTMLToken); Overload;

      Property Value : String Read FCurrentValue Write FCurrentValue;
  End;




Type
  TFslHTMLParser = Class(TFslHTMLLexer)
    Private
      FAttributes : TFslHTMLAttributes;
      FNextTagOpen : String;
      FNextTagClose : String;

    Protected
      Procedure ConsumeIdentifier(Const sIdentifier : String);
      Function ConsumeString: String;

      Function NextIsTagOpen : Boolean;
      Function NextIsTagClose : Boolean;
      Function NextIsString : Boolean;

      Function NextTagOpen : String; Overload;
      Function NextTagClose : String; Overload;
      Function ConsumeTagOpen : String; Overload;
      Function ConsumeTagClose : String; Overload;
      Procedure ConsumeTagOpen(Const sTag : String); Overload;
      Procedure ConsumeTagClose(Const sTag : String); Overload;
      Function NextTagOpen(Const sTag : String) : Boolean; Overload;
      Function NextTagClose(Const sTag : String) : Boolean; Overload;
      Function ConsumeTagBody : String;

      Procedure ReadStyles(oStyles: TFslStringMatch; Const sValue: String);
      Procedure ReadItem(oItem : TFslHTMLItem);

      Procedure ConsumeTextFragment(oTextFragment : TFslHTMLTextFragment);
      Procedure ConsumeParagraph(oParagraph : TFslHTMLParagraph);
      Procedure ConsumeHeading(oHeading : TFslHTMLHeading);
      Procedure ConsumeAnchor(oAnchor : TFslHTMLAnchor);
      Procedure ConsumeFont(oFont : TFslHTMLFont);
      Procedure ConsumeSpan(oSpan : TFslHTMLSpan);
      Procedure ConsumeOrderedList(oList : TFslHTMLOrderedList);
      Procedure ConsumeUnorderedList(oList : TFslHTMLUnorderedList);
      Procedure ConsumeListItem(oItem : TFslHTMLListItem);
      Procedure ConsumeItalic(oItalic : TFslHTMLItalic);
      Procedure ConsumeBold(oBold : TFslHTMLBold);
      Procedure ConsumeUnderline(oUnderline : TFslHTMLUnderline);
      Procedure ConsumeBlockQuote(oBlockQuote : TFslHTMLBlockQuote);
      Procedure ConsumeImage(oImage : TFslHTMLImage);
      Procedure ConsumeTable(oTable : TFslHTMLTable);
      Procedure ConsumeTableRow(oTableRow : TFslHTMLTableRow);
      Procedure ConsumeTableCell(oTableCell : TFslHTMLTableCell);
      Procedure ConsumeContainer(oContainer : TFslHTMLContainer);
      Procedure ConsumeSubscript(oSubscript : TFslHTMLSubscript);
      Procedure ConsumeSuperscript(oSuperscript : TFslHTMLSuperscript);
      Procedure ConsumeBreak(oBreak : TFslHTMLBreak);
      Procedure ConsumeHorizontalRule(oHorizontalRule : TFslHTMLHorizontalRule);
      Procedure ConsumeInput(oInput : TFslHTMLInput);
      Procedure ConsumePre(oPre : TFslHTMLPreformatted);
      Procedure ConsumeStyles(oStyles : TFslCSSStyles);
      Procedure ConsumeMetaEntry(oEntries : TFslHTMLMetaEntries);

      Function ConsumeItem : TFslHTMLItem;
      Procedure ConsumeSection(Const sTag : String); Overload;
      Procedure ConsumeSection(Const sTag : String; oSection : TFslHTMLSection); Overload; 
      Procedure ConsumeItems(oItems : TFslHTMLItems); 
      Procedure ConsumeHead(oHead : TFslHTMLHead); 
      Procedure ConsumeBody(oBody : TFslHTMLBody); 
      Procedure ConsumeDocumentType(oDocument : TFslHTMLDocument); 

      Function ReduceToken: TFslHTMLToken; Overload; 
      Function ReduceToken(Const aTokens : TFslHTMLTokens) : TFslHTMLToken; Overload;
      Procedure ReduceToken(aToken : TFslHTMLToken); Overload; 
      Function PeekToken : TFslHTMLToken; 

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure ConsumeDocument(oDocument : TFslHTMLDocument); Overload; Virtual;
      Procedure ConsumeDocumentHead(oHead : TFslHTMLHead); Overload; Virtual;
  End;



Type
  TFslHTMLGreekSymbol = (
      ahsCapsAlpha, ahsCapsBeta, ahsCapsGamma, ahsCapsDelta, ahsCapsEpsilon,
      ahsCapsZeta, ahsCapsEta, ahsCapsTheta, ahsCapsIota, ahsCapsKappa,
      ahsCapsLambda, ahsCapsMu, ahsCapsNu, ahsCapsXi, ahsCapsOmicron,
      ahsCapsPi, ahsCapsRho, ahsCapsSigma, ahsCapsTau, ahsCapsUpsilon,
      ahsCapsPhi, ahsCapsChi, ahsCapsPsi, ahsCapsOmega, ahsAlpha,
      ahsBeta, ahsGamma, ahsDelta, ahsEpsilon, ahsZeta, ahsEta, ahsTheta,
      ahsIota, ahsKappa, ahsLambda, ahsMu, ahsNu, ahsXi, ahsOmicron, ahsPi,
      ahsRho, ahsSigmaf, ahsSigma, ahsTau, ahsUpsilon, ahsPhi, ahsChi, ahsPsi,
      ahsOmega, ahsSpecialTheta, ahsSpecialUpsilonHooked, ahsSpecialPi);
  TFslHTMLGreekSymbols = Set Of TFslHTMLGreekSymbol;

  TFslHTMLMathSymbol = (
      ahsNot, ahsMinus, ahsTimes, ahsPrime, ahsDblPrime, ahsFractionSlash,
      ahsSuper2, ahsSuper3, ahsSuper1, ahsQuarter, ahsHalf, ahsThreeQuarter,
      ahsPlusMinus, ahsForAll, ahsPartialDiff, ahsExist, ahsEmptySet, ahsNabla,
      ahsIsIn, ahsNotIn, ahsContains, ahsProduct, ahsSum, ahsAsterisk, ahsSquareRoot,
      ahsProportional, ahsInfinity, ahsAngle, ahsAnd, ahsOr, ahsIntersection,
      ahsUnion, ahsIntegral, ahsTherefore, ahsSimilarTo, ahsApproxEqual,
      ahsAsymptotic, ahsNotEqual, ahsEquivalent, ahsLessEqual, ahsGreaterEqual,
      ahsSubset, ahsSuperset, ahsNotSubset, ahsSubSetEqual, ahsSupersetEqual,
      ahsCirclePlus, ahsCircleTimes, ahsPerpendicular, ahsDot, ahsAlefSym);
  TFslHTMLMathSymbols = Set Of TFslHTMLMathSymbol;

  TFslHTMLSymbol = (
      ahsQuote, ahsAmpersand, ahsLessThan, ahsGreaterThan, ahsLeftSingleQuote,
      ahsRightSingleQuote,ahsSingleLowQuote, ahsLeftDoubleQuote, ahsRightDoubleQuote,
      ahsDoubleLowQuote, ahsNonBreakingSpace,ahsDegrees, ahsCopyright, ahsRegistered,
      ahsTrademark, ahsCent,ahsPound, ahsCurrency, ahsYen, ahsEuro, ahsLeftArrow, ahsUpArrow,
      ahsRightArrow, ahsDownArrow, ahsleftRightArrow, ahsCarriageReturn, ahsLeftDoubleArrow,
      ahsUpDoubleArrow, ahsRightDoubleArrow, ahsDownDoubleArrow, ahsLeftRightDoubleArrow,
      ahsInvertedExcl, ahsInvertedQuestion, ahsBrokenVertical, ahsSection,
      ahsUmlaut,ahsFeminineOrdinal, ahsLeftGuillemet, ahsRightGuillemet, ahsSoftHyphen,
      ahsMacron,ahsAcute, ahsMicro, ahsParagraph, ahsMiddleDot, ahsCedilla,
      ahsMasculineOrdinal, ahsLeftCeiling, ahsRightCeiling, ahsLeftFloor, ahsRightFloor,
      ahsLeftAngle, ahsRightAngle, ahsLozenge, ahsSpades, ahsClubs,ahsHearts,
      ahsDiams, ahsDagger, ahsDoubleDagger, ahsPerMille,ahsBullet, ahsHorizEllipsis,
      ahsOverline, ahsWeierstrass, ahsImaginary, ahsReal);
  TFslHTMLSymbols = Set Of TFslHTMLSymbol;


Type
  TFslHTMLAdapter = Class(TFslStreamAdapter)
  Private
    FProduceXHTML : Boolean;

  Public
    Procedure Read(Var Buffer; iCount : Integer); Override;
    Procedure Write(Const Buffer; iCount : Integer); Override;

    Property ProduceXHTML : Boolean Read FProduceXHTML Write FProduceXHTML;
  End;

  TFslHTMLFormatter = Class(TFslXMLFormatter)
    Private
      FAdapter : TFslHTMLAdapter;

      Function GetStyle: String;
      Procedure SetStyle(Const Value: String);

      Function GetProduceXHTML: Boolean;
      Procedure SetProduceXHTML(Const Value: Boolean);

    Protected
      Procedure SetStream(oStream : TFslStream); Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslHTMLFormatter;
      Function Clone : TFslHTMLFormatter;

      Procedure ProduceDocumentType(Const sVersion, sDTD : String);

      Procedure ProduceDocumentOpen;
      Procedure ProduceDocumentClose;

      Procedure ProduceHeadOpen;
      Procedure ProduceHeadClose;

      Procedure ProduceStyleOpen;
      Procedure ProduceStyleClose;

      Procedure ProduceStyleFragment(Const sName : String; oProperties : TFslStringMatch);
      Procedure ProduceStyleFragmentOpen(Const sName : String);
      Procedure ProduceStyleFragmentProperty(Const sName, sValue : String);
      Procedure ProduceStyleFragmentClose;

      Procedure ProduceBodyOpen;
      Procedure ProduceBodyClose;

      Procedure ProduceTitle(Const sTitle : String);
      Procedure ProduceStylesheet(Const sFilename : String);

      Procedure ProduceOpen(Const sTag : String; bInline : Boolean = False);
      Procedure ProduceClose(Const sTag : String; bInline : Boolean = False);
      Procedure ProduceInlineTag(Const sTag : String);

      Procedure ProduceInlineDocument(Const sDocument : String);

      Procedure ProduceHeading(Const sText : String; Const iLevel : Integer = 1);

      Procedure ProducePreformatted(Const sData : String);
      Procedure ProduceEscaped(Const sData : String);

      Procedure ProduceAnchor(Const sCaption, sReference : String; Const sBookmark : String = ''; Const sTarget : String = '');
      Procedure ProduceAnchorOpen(Const sReference : String; Const sBookmark : String = ''; Const sTarget : String = '');
      Procedure ProduceAnchorClose;
      Procedure ProduceBookmark(Const sBookmark : String);

      Procedure ProduceImageReference(Const sReference : String; Const sAlternate : String = '');

      Procedure ProduceLineBreak;

      Procedure ProduceTableOpen;
      Procedure ProduceTableClose;
      Procedure ProduceTableHeadOpen;
      Procedure ProduceTableHeadClose;
      Procedure ProduceTableRowOpen;
      Procedure ProduceTableRowClose;
      Procedure ProduceTableCellOpen;
      Procedure ProduceTableCellClose;
      Procedure ProduceTableCellText(Const sText : String);
      Procedure ProduceTableColumnGroupOpen;
      Procedure ProduceTableColumnGroupClose;
      Procedure ProduceTableColumn;

      Procedure ProduceSpan(Const sText : String);
      Procedure ProduceParagraph(Const sText : String);

      Procedure ProduceMathSymbol(Const aSymbol : TFslHTMLMathSymbol);
      Procedure ProduceGreekSymbol(Const aSymbol : TFslHTMLGreekSymbol);
      Procedure ProduceSymbol(Const aSymbol : TFslHTMLSymbol);

      Procedure ApplyAttributeAlignLeft;
      Procedure ApplyAttributeAlignRight;
      Procedure ApplyAttributeAlignCenter;
      Procedure ApplyAttributeWidthPixel(Const iWidthPixel : Integer);
      Procedure ApplyAttributeHeightPixel(Const iHeightPixel : Integer);
      Procedure ApplyAttributeHeightPercentage(Const iHeightPercentage : Integer);

      Property Style : String Read GetStyle Write SetStyle;
      Property ProduceXHTML : Boolean Read GetProduceXHTML Write SetProduceXHTML;
  End;


Const
  HTML_GREEK_SYMBOL_VALUES : Array [TFslHTMLGreekSymbol] Of String = (
      'Alpha', 'Beta', 'Gamma', 'Delta', 'Epsilon', 'Zeta', 'Eta', 'Theta',
      'Iota', 'Kappa', 'Lambda', 'Mu', 'Nu', 'Xi', 'Omicron', 'Pi', 'Rho',
       'Sigma', 'Tau', 'Upsilon', 'Phi', 'Chi', 'Psi', 'Omega', 'alpha',
      'beta', 'gamma', 'delta', 'epsilon', 'zeta', 'eta', 'theta', 'iota', 'kappa',
      'lambda', 'mu', 'nu', 'xi', 'omicron', 'pi', 'rho', 'sigmaf', 'sigma', 'tau',
      'upsilon', 'phi', 'chi', 'psi', 'omega', 'thetasym', 'upsih', 'piv');

  HTML_MATH_SYMBOL_VALUES : Array [TFslHTMLMathSymbol] Of String = (
      'not', 'minus', 'times', 'prime', 'Prime', 'frasl', 'sup2', 'sup3', 'sup1',
      'frac14', 'frac12', 'frac34', 'plusmn', 'forall', 'part', 'exist', 'empty',
      'nabla', 'isin', 'notin', 'ni', 'prod', 'sum', 'lowast', 'radic', 'prop',
      'infin', 'ang', 'and', 'or', 'cap', 'cup', 'int', 'there4', 'sim', 'cong',
      'asymp', 'ne', 'equiv', 'le', 'ge', 'sub', 'sup', 'nsub', 'sube', 'supe',
      'oplus', 'otimes', 'perp', '#8901', '#8501');

  HTML_SYMBOL_VALUES : Array [TFslHTMLSymbol] Of String = (
      'quot', 'amp', 'lt', 'gt', 'lsquo', 'rsquo', 'sbquo', 'ldquo', 'rdquo',
      'bdquo', 'nbsp', 'deg', 'copy', 'reg', 'trade', 'cent', 'pound', 'curren',
      'yen', 'euro', 'larr', 'uarr', 'rarr', 'darr', 'harr', 'crarr', 'lArr',
      'uArr', 'rArr', 'dArr', 'hArr', 'iexcl', 'iquest', 'brvbar', 'sect', 'uml',
      'ordf', 'laquo', 'raquo', 'shy', 'macr', 'acute', 'micro', 'para', 'middot',
      'cedil', 'ordm', 'lceil', 'rceil', 'lfloor', 'rfloor', 'lang', 'rang', 'loz',
      'spades', 'clubs', 'hearts', 'diams', 'dagger', 'Dagger', 'permil', 'bull',
      'hellip', 'oline', 'weierp', 'image', 'real');


Implementation


Constructor TFslHTMLParser.Create;
Begin
  Inherited;

  FAttributes := TFslHTMLAttributes.Create;
  FAttributes.Forced := True;
End;


Destructor TFslHTMLParser.Destroy;
Begin
  FAttributes.Free;

  Inherited;
End;



Function TFslHTMLParser.ReduceToken : TFslHTMLToken;
Begin
  Result := PeekToken;

  ConsumeToken;
End;


Function TFslHTMLParser.ReduceToken(Const aTokens: TFslHTMLTokens): TFslHTMLToken;
Begin
  If Not (PeekToken In aTokens) Then
    RaiseError('Reduce', StringFormat('Unexpected token found ''%s''.', [HTMLTOKEN_STRING[PeekToken]]));

  Result := ReduceToken;
End;


Procedure TFslHTMLParser.ReduceToken(aToken: TFslHTMLToken);
Begin
  If PeekToken <> aToken Then
    RaiseError('ReduceToken', StringFormat('Expected token %s but found token %s.', [HTMLTOKEN_STRING[aToken], HTMLTOKEN_STRING[PeekToken]]));

  ReduceToken;
End;


Function TFslHTMLParser.PeekToken: TFslHTMLToken;
Begin
  Result := NextToken;

  While Result In [ahtWhitespace, ahtComment] Do
  Begin
    ConsumeToken;

    Result := NextToken;
  End;
End;


Function TFslHTMLParser.NextIsTagOpen : Boolean;
Begin
  Result := (FNextTagOpen <> '') Or (PeekToken In [ahtOpenTag, ahtOpenTagExclamation]);
End;


Function TFslHTMLParser.NextIsTagClose : Boolean;
Begin
  Result := (FNextTagClose <> '') Or (PeekToken = ahtOpenTagForwardSlash);
End;


Function TFslHTMLParser.NextIsString: Boolean;
Begin
  Result := PeekToken In [ahtSingleQuote, ahtDoubleQuote];
End;


Function TFslHTMLParser.NextTagOpen : String;
Var
  sKey   : String;
  sValue : String;
Begin
  If FNextTagOpen = '' Then
  Begin
    // <TagOpen> ::= '<' <Identifier> { <Attributes> } { '>' | '/>' }

    ReduceToken(ahtOpenTag);

    // Identifier
    ReduceToken(ahtIdentifier);

    FNextTagOpen := Value;

    If PeekToken = ahtColon Then
    Begin
      ReduceToken(ahtColon);

      ReduceToken(ahtIdentifier);

      FNextTagOpen := FNextTagOpen + ':' + Value;
    End;

    FNextTagOpen := StringUpper(FNextTagOpen);

    // Attributes
    FAttributes.Clear;

    While More And Not (PeekToken In [ahtCloseTag, ahtCloseTagForwardSlash]) Do
    Begin { While }
      ReduceToken(ahtIdentifier);

      sKey := Value;

      // TODO: handle namespaces properly 'xmlns:v="urn:schemas-microsoft-com:vml"'
      If PeekToken = ahtColon Then
      Begin
        ReduceToken(ahtColon);

        ReduceToken(ahtIdentifier);

        sKey := sKey + ':' + Value;
      End;

      ReduceToken(ahtEquals);

      If PeekToken In [ahtSingleQuote, ahtDoubleQuote] Then
      Begin
        sValue := ConsumeString;
      End
      Else
      Begin
        ReduceToken([ahtIdentifier, ahtNumber]);

        sValue := Value;
      End;

      FAttributes.Add(sKey, sValue);
    End;  { While }

    If PeekToken = ahtCloseTagForwardSlash Then
    Begin
      ReduceToken(ahtCloseTagForwardSlash);

      FNextTagClose := FNextTagOpen;
    End
    Else
    Begin
      FNextTagClose := '';

      ReduceToken(ahtCloseTag);
    End;
  End;

  Result := FNextTagOpen;
End;


Function TFslHTMLParser.NextTagOpen(Const sTag: String): Boolean;
Begin
  Assert(CheckCondition(UpperCase(sTag) = sTag, 'NextTagOpen', 'sTag must be uppercase.'));

  Result := (NextIsTagOpen And (NextTagOpen = sTag));
End;


Function TFslHTMLParser.NextTagClose(Const sTag: String): Boolean;
Begin
  Assert(CheckCondition(UpperCase(sTag) = sTag, 'NextTagClose', 'sTag must be uppercase.'));

  Result := (NextIsTagClose And (NextTagClose = sTag));
End;


Function TFslHTMLParser.ConsumeTagOpen : String;
Begin
  Result := NextTagOpen;

  FNextTagOpen := '';
End;


Function TFslHTMLParser.NextTagClose : String;
Begin
  If FNextTagClose = '' Then
  Begin
    // <TagClose> ::= '</' <Identifier> '>'

    ReduceToken(ahtOpenTagForwardSlash);

    // Identifier
    ReduceToken(ahtIdentifier);

    FNextTagClose := Value;

    If PeekToken = ahtColon Then
    Begin
      ReduceToken(ahtColon);

      ReduceToken(ahtIdentifier);

      FNextTagClose := FNextTagClose + ':' + Value;
    End;

    FNextTagClose := StringUpper(FNextTagClose);

    ReduceToken(ahtCloseTag);

    FAttributes.Clear;
  End;

  Result := FNextTagClose;
End;


Function TFslHTMLParser.ConsumeTagClose : String;
Begin
  Result := NextTagClose;

  FNextTagClose := '';
End;


Procedure TFslHTMLParser.ConsumeTagOpen(Const sTag: String);
Var
  sValue : String;
Begin
  sValue := ConsumeTagOpen;

  If Not StringEquals(sValue, sTag) Then
    RaiseError('ConsumeTagOpen', StringFormat('Expected tag <%s> but found tag <%s>', [sTag, sValue]));
End;


Procedure TFslHTMLParser.ConsumeTagClose(Const sTag: String);
Var
  sValue : String;
Begin
  sValue := ConsumeTagClose;

  If Not StringEquals(sValue, sTag) Then
    RaiseError('ConsumeTagClose', StringFormat('Expected tag </%s> but found tag </%s>', [sTag, sValue]));
End;


Function TFslHTMLParser.ConsumeTagBody: String;
Begin
  Result := '';

  While More And (NextToken In HTMLTOKEN_BODY_SET) Do
  Begin
    ConsumeToken;

    StringAppend(Result, Value, '');
  End;
End;


Procedure TFslHTMLParser.ConsumeIdentifier(Const sIdentifier: String);
Begin
  ReduceToken(ahtIdentifier);

  If Not StringEquals(Value, sIdentifier) Then
    RaiseError('ConsumeTagClose', StringFormat('Expected identifier ''%s'' but found identifier ''%s''', [sIdentifier, Value]));
End;


Function TFslHTMLParser.ConsumeString : String;
Var
  aQuote : TFslHTMLToken;
Begin
  Result := '';

  If PeekToken = ahtSingleQuote Then
    aQuote := ahtSingleQuote
  Else
    aQuote := ahtDoubleQuote;

  ReduceToken(aQuote);

  While More And (NextToken <> aQuote) Do
  Begin
    ConsumeToken;

    Result := Result + Value;
  End;

  ReduceToken(aQuote);
End;


Function TFslHTMLParser.ConsumeItem: TFslHTMLItem;
Var
  sTag : String;
Begin
  Result := Nil;
  Try
    // TODO: smarter implementation.

  //If (FNextTagOpen <> '') Or (PeekToken In [ahtOpenTag, ahtOpenTagExclamation]) Then
  //Begin
  //  sTag := FNextTagOpen;
    If NextIsTagOpen Then
    Begin
      sTag := NextTagOpen;

      If sTag = 'P' Then
      Begin
        Result := TFslHTMLParagraph.Create;

        ConsumeParagraph(TFslHTMLParagraph(Result));
      End
      Else If sTag = 'A' Then
      Begin
        Result := TFslHTMLAnchor.Create;

        ConsumeAnchor(TFslHTMLAnchor(Result));
      End
      Else If sTag = 'I' Then
      Begin
        Result := TFslHTMLItalic.Create;

        ConsumeItalic(TFslHTMLItalic(Result));
      End
      Else If sTag = 'B' Then
      Begin
        Result := TFslHTMLBold.Create;

        ConsumeBold(TFslHTMLBold(Result));
      End
      Else If sTag = 'BR' Then
      Begin
        Result := TFslHTMLBreak.Create;

        ConsumeBreak(TFslHTMLBreak(Result));
      End
      Else If sTag = 'HR' Then
      Begin
        Result := TFslHTMLHorizontalRule.Create;

        ConsumeHorizontalRule(TFslHTMLHorizontalRule(Result));
      End
      Else If sTag = 'U' Then
      Begin
        Result := TFslHTMLUnderline.Create;

        ConsumeUnderline(TFslHTMLUnderline(Result));
      End
      Else If sTag = 'UL' Then
      Begin
        Result := TFslHTMLUnorderedList.Create;

        ConsumeUnorderedList(TFslHTMLUnorderedList(Result));
      End
      Else If sTag = 'OL' Then
      Begin
        Result := TFslHTMLOrderedList.Create;

        ConsumeOrderedList(TFslHTMLOrderedList(Result));
      End
      Else If sTag = 'LI' Then
      Begin
        Result := TFslHTMLListItem.Create;

        ConsumeListItem(TFslHTMLListItem(Result));
      End
      Else If sTag = 'SUP' Then
      Begin
        Result := TFslHTMLSuperscript.Create;

        ConsumeSuperscript(TFslHTMLSuperscript(Result));
      End
      Else If sTag = 'SUB' Then
      Begin
        Result := TFslHTMLSubscript.Create;

        ConsumeSubscript(TFslHTMLSubscript(Result));
      End
      Else If sTag = 'DIV' Then
      Begin
        Result := TFslHTMLContainer.Create;

        ConsumeContainer(TFslHTMLContainer(Result));
      End
      Else If sTag = 'IMG' Then
      Begin
        Result := TFslHTMLImage.Create;

        ConsumeImage(TFslHTMLImage(Result));
      End
      Else If sTag = 'FONT' Then
      Begin
        Result := TFslHTMLFont.Create;

        ConsumeFont(TFslHTMLFont(Result));
      End
      Else If sTag = 'SPAN' Then
      Begin
        Result := TFslHTMLSpan.Create;

        ConsumeSpan(TFslHTMLSpan(Result));
      End
      Else If sTag = 'TABLE' Then
      Begin
        Result := TFslHTMLTable.Create;

        ConsumeTable(TFslHTMLTable(Result));
      End
      Else If sTag = 'TR' Then
      Begin
        Result := TFslHTMLTableRow.Create;

        ConsumeTableRow(TFslHTMLTableRow(Result));
      End
      Else If (sTag = 'TD') Or (sTag = 'TH') Then
      Begin
        Result := TFslHTMLTableCell.Create;

        ConsumeTableCell(TFslHTMLTableCell(Result));
      End
      Else If sTag = 'BLOCKQUOTE' Then
      Begin
        Result := TFslHTMLBlockQuote.Create;

        ConsumeBlockQuote(TFslHTMLBlockQuote(Result));
      End
      Else If sTag = 'INPUT' Then
      Begin
        Result := TFslHTMLInput.Create;

        ConsumeInput(TFslHTMLInput(Result));
      End
      Else If (Length(sTag) = 2) And (sTag[1] = 'H') And CharInSet(sTag[2], ['1'..'6']) Then
      Begin
        Result := TFslHTMLHeading.Create;

        ConsumeHeading(TFslHTMLHeading(Result));
      End
      Else If sTag = 'PRE' Then
      Begin
        Result := TFslHTMLPreformatted.Create;

        ConsumePre(TFslHTMLPreformatted(Result));
      End
      Else
      Begin
        // Ignore unknown tags.

        ConsumeSection(sTag);
      End
    End
    Else
    Begin
      Result := TFslHTMLTextFragment.Create;

      ConsumeTextFragment(TFslHTMLTextFragment(Result));
    End;

    Assert(Not Assigned(Result) Or Invariants('ConsumeItem', Result, TFslHTMLItem, 'Result (' + sTag + ')'));

    Result.Link;
  Finally
    Result.Free;
  End;
End;


Procedure TFslHTMLParser.ConsumeSection(Const sTag: String);
Var
  oSection : TFslHTMLSection;
Begin
  // Ignore unhandled section.

  oSection := TFslHTMLSection.Create;
  Try
    ConsumeSection(sTag, oSection);
  Finally
    oSection.Free;
  End;
End;


Procedure TFslHTMLParser.ReadStyles(oStyles : TFslStringMatch; Const sValue: String);
Var
  sTemp   : String;
  sItem   : String;
  iPos    : Integer;
  iSymbol : Integer;
  sLeft   : String;
  sRight  : String;
Begin
  oStyles.Clear;

  sTemp := sValue;
  iPos := Pos(';', sTemp);
  iSymbol := Length(';');

  While (iPos > 0) Do
  Begin
    sItem := System.Copy(sTemp, 1, iPos - 1);

    If StringSplit(sItem, ':', sLeft, sRight) Then
      oStyles.Add(StringTrimWhitespace(sLeft), StringTrimWhitespace(sRight))
    Else
      oStyles.Add(StringTrimWhitespace(sItem), '');

    System.Delete(sTemp, 1, iPos + iSymbol - 1);

    iPos := Pos(';', sTemp);
  End;

  If sTemp <> '' Then
  Begin
    If StringSplit(sTemp, ':', sLeft, sRight) Then
      oStyles.Add(StringTrimWhitespace(sLeft), StringTrimWhitespace(sRight))
    Else
      oStyles.Add(StringTrimWhitespace(sTemp), '');
  End;
End;


Procedure TFslHTMLParser.ReadItem(oItem: TFslHTMLItem);
Begin
  oItem.ID := FAttributes['ID'];
  oItem.TitleAttribute := FAttributes['Title'];

  ReadStyles(oItem.Style.Values, FAttributes['Style']);

  oItem.Classes.AsText := FAttributes['Class'];
End;



Procedure TFslHTMLParser.ConsumeSection(Const sTag: String; oSection: TFslHTMLSection);
Begin
  ConsumeTagOpen(sTag);

  if (sTag[Length(sTag)] <> '/') Then
  Begin
    ReadItem(oSection);

    ConsumeItems(oSection.Items);

    If NextTagClose(sTag) Then
      ConsumeTagClose(sTag);
  End;
End;


Procedure TFslHTMLParser.ConsumeItems(oItems: TFslHTMLItems);
Var
  oItem : TFslHTMLItem;
Begin
  oItems.Clear;

  While More And Not NextIsTagClose Do
  Begin
    oItem := ConsumeItem;

    If Assigned(oItem) Then
      oItems.Add(oItem);
  End;
End;


Procedure TFslHTMLParser.ConsumeImage(oImage: TFslHTMLImage);
Begin
  // <Image> ::= '<IMG' {<ImageAttributes>} '>'
  //
  // <ImageAttributes> ::= { 'src' '=' <URL> } { 'alt' '=' <String> } { 'longdesc' = <String> } { 'align' '=' <Align> }
  //
  // <Align> ::= 'left' | 'top' | 'right' | 'bottom' | 'middle'

  ConsumeTagOpen('IMG');

  oImage.Name := FAttributes['name'];
  oImage.Source := FAttributes['src'];
  oImage.Alternative := FAttributes['alt'];
  oImage.LongDescription := FAttributes['longdesc'];
  oImage.Width := FAttributes['width'];
  oImage.Height := FAttributes['height'];
  oImage.HorizontalSpace := FAttributes['hspace'];
  oImage.VerticalSpace := FAttributes['vspace'];
  oImage.Align := TFslHTMLAlign(IntegerMin(StringArrayIndexOf(ADVHTMLALIGN_NAMES, FAttributes['align']), 0));

  ReadItem(oImage);

  If NextTagClose('IMG') Then
    ConsumeTagClose('IMG');
End;


Procedure TFslHTMLParser.ConsumeTableCell(oTableCell: TFslHTMLTableCell);
Var
  sTag : String;
Begin
  // <TableCell> ::= <TableDataCell> | <TableHeaderCell>
  // <TableDataCell> ::= '<TD>' <TableCell>* '</TD>'
  // <TableHeaderCell> ::= '<TH>' <TableCell>* '</TH>'

  sTag := ConsumeTagOpen;

  oTableCell.IsHeader := sTag = 'TH';
  oTableCell.Width := FAttributes['width'];
  oTableCell.Height := FAttributes['height'];
  ReadItem(oTableCell);

  ConsumeItems(oTableCell.Items);

  ConsumeTagClose(sTag);
End;


Procedure TFslHTMLParser.ConsumeTableRow(oTableRow: TFslHTMLTableRow);
Var
  oCell : TFslHTMLTableCell;
Begin
  // <TableRow> ::= '<TR>' <TableCell>* '</TR>'

  ConsumeTagOpen('TR');
  ReadItem(oTableRow);

  While More And Not NextIsTagClose Do
  Begin
    If NextTagOpen('TD') Or NextTagOpen('TH') Then
    Begin
      oCell := TFslHTMLTableCell.Create;
      Try
        ConsumeTableCell(oCell);

        oTableRow.Cells.Add(oCell.Link);
      Finally
        oCell.Free;
      End;
    End
    Else
    Begin
      // TODO: table row elements.

      ConsumeSection(NextTagOpen);
    End;
  End;

  ConsumeTagClose('TR');
End;


Procedure TFslHTMLParser.ConsumeTable(oTable: TFslHTMLTable);
Var
  oRow : TFslHTMLTableRow;
  sInnerTag : String;
Begin
  // <Table> ::= '<TABLE>' <TableRow>* '</TABLE>'

  // TODO: COLGROUPS, THEAD/TFOOT/TBODY

  ConsumeTagOpen('TABLE');
  ReadItem(oTable);

  sInnerTag := '';
  While More And ((sInnerTag <> '') Or Not NextIsTagClose) Do
  Begin
    If NextTagOpen('TR') Then
    Begin
      oRow := TFslHTMLTableRow.Create;
      Try
        ConsumeTableRow(oRow);

        oTable.Rows.Add(oRow.Link);
      Finally
        oRow.Free;
      End;
    End
    Else If NextIsTagOpen Then
    Begin
      sInnerTag := NextTagOpen();

      If (sInnerTag = 'THEAD') Or (sInnerTag = 'TFOOT') Or (sInnerTag = 'TBODY') Then
        ConsumeTagOpen(sInnerTag)
      Else
        RaiseError('ConsumeTable', 'Unexpected tag encountered inside table ' + sInnerTag);
    End
    Else If (sInnerTag <> '') Then
    Begin
      ConsumeTagClose(sInnerTag);
      sInnerTag := '';
    End;

  End;

  ConsumeTagClose('TABLE');
End;


Procedure TFslHTMLParser.ConsumeHeading(oHeading: TFslHTMLHeading);
Var
  sTag : String;
  iLevel : Integer;
Begin
  // <Heading> ::= '<' <HeadingLevel> '>' <Item>* '</' <HeadingLevel '>'
  //
  // <HeadingLevel> ::= 'H1' | 'H2' | 'H3' | 'H4' | 'H5' | 'H6'

  sTag := ConsumeTagOpen;

  ReadItem(oHeading);

  iLevel := StringToInteger32(Copy(sTag, 2, MaxInt)) - 1;

  If (iLevel >= Integer(Low(TFslHTMLHeadingLevel))) And (iLevel <= Integer(High(TFslHTMLHeadingLevel))) Then
    oHeading.Level := TFslHTMLHeadingLevel(iLevel)
  Else
    oHeading.Level := Low(TFslHTMLHeadingLevel);

  ConsumeItems(oHeading.Items);

  ConsumeTagClose(sTag);
End;


Procedure TFslHTMLParser.ConsumeAnchor(oAnchor : TFslHTMLAnchor);
Begin
  // <Anchor> ::= '<A>' <Item>* '</A>'

  ConsumeTagOpen('A');

  ReadItem(oAnchor);

  oAnchor.URL := FAttributes['href'];
  oAnchor.Name := FAttributes['name'];
  oAnchor.AccessKey := FAttributes['accesskey'];

  ConsumeItems(oAnchor.Items);

  ConsumeTagClose('A');
End;


Procedure TFslHTMLParser.ConsumeFont(oFont: TFslHTMLFont);
Begin
  // <Font> ::= '<FONT>' <Item>* '</FONT>'

  oFont.Color := FAttributes['color'];
  ConsumeSection('FONT', oFont);
End;


Procedure TFslHTMLParser.ConsumeSpan(oSpan: TFslHTMLSpan);
Begin
  // <Span> ::= '<Span>' <Item>* '</Span>'

  ConsumeSection('SPAN', oSpan);
End;


Procedure TFslHTMLParser.ConsumeUnorderedList(oList: TFslHTMLUnorderedList);
Begin
  // <UnorderedList> ::= '<UL>' <Item>* '</UL>'

  ConsumeSection('UL', oList);
End;


Procedure TFslHTMLParser.ConsumeOrderedList(oList: TFslHTMLOrderedList);
Begin
  // <OrderedList> ::= '<OL>' <Item>* '</OL>'

  ConsumeSection('OL', oList);
End;


Procedure TFslHTMLParser.ConsumeListItem(oItem: TFslHTMLListItem);
Begin
  // <ListItem> ::= '<LI value="">' <Item>* '</LI>'

  oItem.Value := StrToIntDef(FAttributes['value'], 0);
  oItem.HasValue := FAttributes['value'] <> '';
  ConsumeSection('LI', oItem);
End;


Procedure TFslHTMLParser.ConsumeSubscript(oSubscript: TFslHTMLSubscript);
Begin
  ConsumeSection('SUB', oSubscript);
End;


Procedure TFslHTMLParser.ConsumeSuperscript(oSuperscript: TFslHTMLSuperscript);
Begin
  ConsumeSection('SUP', oSuperscript);
End;


Procedure TFslHTMLParser.ConsumeBreak(oBreak: TFslHTMLBreak);
Begin
  // <Break> ::= '<BR>' Or '<BR />' Or '<BR></BR>'

  ConsumeTagOpen('BR');
  ReadItem(oBreak);

  If NextTagClose('BR') Then
    ConsumeTagClose('BR');
End;


Procedure TFslHTMLParser.ConsumeHorizontalRule(oHorizontalRule: TFslHTMLHorizontalRule);
Begin
  // <HorizontalRule> ::= '<HR>' Or '<HR />' Or '<HR></HR>'

  ConsumeTagOpen('HR');
  ReadItem(oHorizontalRule);
  
  If NextTagClose('HR') Then
    ConsumeTagClose('HR');
End;


Procedure TFslHTMLParser.ConsumeInput(oInput: TFslHTMLInput);
Begin
  // <Input> ::= '<INPUT' <InputAttributes> '>'

  ConsumeTagOpen('INPUT');
End;


Procedure TFslHTMLParser.ConsumeBold(oBold: TFslHTMLBold);
Begin
  ConsumeSection('B', oBold);
End;


Procedure TFslHTMLParser.ConsumeItalic(oItalic: TFslHTMLItalic);
Begin
  ConsumeSection('I', oItalic);
End;


Procedure TFslHTMLParser.ConsumeUnderline(oUnderline: TFslHTMLUnderline);
Begin
  ConsumeSection('U', oUnderline);
End;


Procedure TFslHTMLParser.ConsumeBlockQuote(oBlockQuote : TFslHTMLBlockQuote);
Begin
  ConsumeSection('BLOCKQUOTE', oBlockQuote);
End;


Procedure TFslHTMLParser.ConsumeParagraph(oParagraph: TFslHTMLParagraph);
Begin
  // <Paragraph> ::= '<P>' <Item>* '</P>'

  ConsumeSection('P', oParagraph);
End;


Procedure TFslHTMLParser.ConsumeContainer(oContainer: TFslHTMLContainer);
Begin
  ConsumeSection('DIV', oContainer);
End;


Procedure TFslHTMLParser.ConsumeTextFragment(oTextFragment : TFslHTMLTextFragment);
Begin
  oTextFragment.Text := ConsumeTagBody;
End;


Procedure TFslHTMLParser.ConsumeBody(oBody: TFslHTMLBody);
Begin
  // <Body> ::= '<BODY>' <Item>* '</BODY>'

  ConsumeSection('BODY', oBody);
End;


Procedure TFslHTMLParser.ConsumeStyles(oStyles: TFslCSSStyles);
Var
  oStyle : TFslCSSStyle;
  sKey   : String;
  sValue : String;
Begin
  // <Styles> ::= '<STYLE>' <Style>* '</STYLE>
  // <Style> ::= <StyleName> '{' ( <StyleKey> ':' <StyleValue> ';' ) * '}'

  ConsumeTagOpen('STYLE');

  While More And Not NextTagClose('STYLE') Do
  Begin
    oStyle := TFslCSSStyle.Create;
    Try
      sValue := '';
      While PeekToken In [ahtIdentifier, ahtColon, ahtPeriod, ahtHash] Do
      Begin
        ReduceToken;

        StringAppend(sValue, Value);
      End;

      oStyle.Name := sValue;

      If PeekToken = ahtOpenBrace Then
      Begin
        ReduceToken(ahtOpenBrace);

        While More And (PeekToken <> ahtCloseBrace) Do
        Begin
          ReduceToken(ahtIdentifier);

          sKey := Value;

          ReduceToken(ahtColon);

          sValue := '';
          While PeekToken In [ahtNumber, ahtIdentifier, ahtHash] Do
          Begin
            ReduceToken;

            StringAppend(sValue, Value);
          End;

          oStyle.Values.Add(sKey, sValue);

          If PeekToken = ahtSemicolon Then
            ReduceToken(ahtSemicolon);
        End;

        ReduceToken(ahtCloseBrace);
      End;

      oStyles.Add(oStyle.Link);
    Finally
      oStyle.Free;
    End;
  End;

  ConsumeTagClose('STYLE');
End;


Procedure TFslHTMLParser.ConsumeMetaEntry(oEntries : TFslHTMLMetaEntries);
Var
  oMetaEntry : TFslHTMLMetaEntry;
Begin
  ConsumeTagOpen('META');

  oMetaEntry := TFslHTMLMetaEntry.Create;
  Try
    oMetaEntry.HttpEquiv := FAttributes['http-equiv'];
    oMetaEntry.Content := FAttributes['content'];
    oMetaEntry.Name := FAttributes['name'];
    oMetaEntry.Scheme := FAttributes['scheme'];

    oEntries.Add(oMetaEntry.Link);
  Finally
    oMetaEntry.Free;
  End;

  If NextTagClose('META') Then
    ConsumeTagClose('META');
End;


Procedure TFslHTMLParser.ConsumeHead(oHead: TFslHTMLHead);
Begin
  // <Head> ::= '<HEAD>' { '<TITLE>' <Body> '</TITLE>' } { '<' 'META' <Attributes> '>' }

  ConsumeTagOpen('HEAD');
  ConsumeWhitespace;

  While More And Not NextTagClose('HEAD') Do
  Begin
    If NextTagOpen('TITLE') Then
    Begin
      ConsumeTagOpen('TITLE');

      oHead.Title := ConsumeTagBody;

      ConsumeTagClose('TITLE');
    End
    Else If NextTagOpen('META') Then
    Begin
      ConsumeMetaEntry(oHead.Meta);
    End
    Else If NextTagOpen('LINK') Then
    Begin
      ConsumeSection('LINK');
    End
    Else If NextTagOpen('SCRIPT') Then
    Begin
      ConsumeSection('SCRIPT');

      // TODO: Scripts
    End
    Else If NextTagOpen('STYLE') Then
    Begin
      ConsumeStyles(oHead.Styles);
    End
    Else
    Begin
      // TODO: Other head items.

      RaiseError('ConsumeHead', StringFormat('Open tag ''%s'' not recognised.', [NextTagOpen]));
    End;
    ConsumeWhitespace;
  End;

  ConsumeTagClose('HEAD');
End;


Procedure TFslHTMLParser.ConsumeDocumentType(oDocument: TFslHTMLDocument);
Begin
  // <DocumentType> ::= { '<' !DOCTYPE' 'HTML' 'PUBLIC' <Version> <DTD> '>' }

  If PeekToken = ahtOpenTagQuery Then
  Begin
    ReduceToken(ahtOpenTagQuery);

    ConsumeIdentifier('XML');

    // Ignore XML all attributes
    While PeekToken <> ahtCloseTagQuery Do
    Begin
      ReduceToken(ahtIdentifier);

      ReduceToken(ahtEquals);

      ConsumeString;
    End;

    ReduceToken(ahtCloseTagQuery);
  End;

  If PeekToken = ahtOpenTagExclamation Then
  Begin
    ReduceToken(ahtOpenTagExclamation);

    ConsumeIdentifier('DOCTYPE');

    ConsumeIdentifier('HTML');

    ConsumeIdentifier('PUBLIC');

    If NextIsString Then
    Begin
      oDocument.Version := ConsumeString;

      If NextIsString Then
        oDocument.DTD := ConsumeString;
    End;

    ReduceToken(ahtCloseTag);
  End;
End;


Procedure TFslHTMLParser.ConsumeDocument(oDocument: TFslHTMLDocument);
Begin
  // <Document> ::= <DocumentType> '<HTML>' { <Head> } { <Body> } | '</HTML>'

  oDocument.Clear;

  ConsumeDocumentType(oDocument);

  If NextTagOpen('HTML') Then
  Begin
    ConsumeTagOpen('HTML');
    ConsumeWhitespace;

    If NextTagOpen('HEAD') Then
      ConsumeHead(oDocument.Head);
    ConsumeWhitespace;

    If NextTagOpen('STYLE') Then
      ConsumeStyles(oDocument.Styles);
    ConsumeWhitespace;

    If NextTagOpen('BODY') Then
      ConsumeBody(oDocument.Body);
    ConsumeWhitespace;

    ConsumeTagClose('HTML');
  End
  Else
  Begin
    ConsumeItems(oDocument.Body.Items);
  End;
End;


Procedure TFslHTMLParser.ConsumeDocumentHead(oHead : TFslHTMLHead);
Var
  oDocument : TFslHTMLDocument;
Begin
  oHead.Clear;

  oDocument := TFslHTMLDocument.Create;
  Try
    ConsumeDocumentType(oDocument);

    If NextTagOpen('HTML') Then
    Begin
      ConsumeTagOpen('HTML');

      If NextTagOpen('HEAD') Then
        ConsumeHead(oHead);
    End;
  Finally
    oDocument.Free;
  End;
End;


procedure TFslHTMLParser.ConsumePre(oPre: TFslHTMLPreformatted);
begin
  ConsumeSection('PRE', oPre);
end;


function TFslHTMLParser.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FAttributes.sizeInBytes);
  inc(result, (FNextTagOpen.length * sizeof(char)) + 12);
  inc(result, (FNextTagClose.length * sizeof(char)) + 12);
end;

Constructor TFslCSSFragment.Create;
Begin
  Inherited;

  FValues := TFslCSSValues.Create;
  FValues.Symbol := ';';
  FValues.Separator := ':';
End;


Destructor TFslCSSFragment.Destroy;
Begin
  FValues.Free;

  Inherited;
End;


Procedure TFslCSSFragment.Assign(oSource: TFslObject);
Begin
  Inherited;

  FValues.Assign(TFslCSSFragment(oSource).Values);
End;


Function TFslCSSFragment.Clone: TFslCSSFragment;
Begin
  Result := TFslCSSFragment(Inherited Clone);
End;


Function TFslCSSFragment.Link: TFslCSSFragment;
Begin
  Result := TFslCSSFragment(Inherited Link);
End;


function TFslCSSFragment.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FValues.sizeInBytes);
end;

Function TFslCSSFragments.GetFragment(iIndex: Integer): TFslCSSFragment;
Begin
  Result := TFslCSSFragment(ObjectByIndex[iIndex]);
End;


Function TFslCSSFragments.ItemClass: TFslObjectClass;
Begin
  Result := TFslCSSFragment;
End;


Procedure TFslCSSStyle.Assign(oSource: TFslObject);
Begin
  Inherited;

  Name := TFslCSSStyle(oSource).Name;
End;


Function TFslCSSStyle.Clone: TFslCSSStyle;
Begin
  Result := TFslCSSStyle(Inherited Clone);
End;


Function TFslCSSStyle.Link: TFslCSSStyle;
Begin
  Result := TFslCSSStyle(Inherited Link);
End;


function TFslCSSStyle.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
end;

Function TFslCSSStyles.GetScript(iIndex: Integer): TFslCSSStyle;
Begin
  Result := TFslCSSStyle(ObjectByIndex[iIndex]);
End;


Function TFslCSSStyles.ItemClass: TFslObjectClass;
Begin
  Result := TFslCSSStyle;
End;


Constructor TFslCSSValues.Create;
Begin
  Inherited;

  Forced := True;
End;


Function TFslCSSValues.GetMatch(Const sKey: String): String;
Begin
  Result := GetValueByKey(sKey);
End;


Procedure TFslCSSValues.SetMatch(Const sKey, sValue: String);
Begin
  SetValueByKey(sKey, sValue);
End;


Constructor TFslHTMLAttributes.Create;
Begin
  Inherited;

  Forced := True;
End;


Function TFslHTMLAttributes.GetAttribute(Const sKey: String): String;
Begin
  Result := GetValueByKey(sKey);
End;


Procedure TFslHTMLAttributes.SetAttribute(Const sKey, sValue: String);
Begin
  SetValueByKey(sKey, sValue);
End;





Procedure TFslHTMLElement.Assign(oSource: TFslObject);
Begin
  Inherited;

  FTitleAttribute := TFslHTMLElement(oSource).FTitleAttribute;
End;


Procedure TFslHTMLElement.Clear;
Begin
End;


function TFslHTMLElement.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FTitleAttribute.length * sizeof(char)) + 12);
end;

Function TFslHTMLElements.ItemClass: TFslObjectClass;
Begin
  Result := TFslHTMLElement;
End;


Function TFslHTMLElements.GetElement(iIndex: Integer): TFslHTMLElement;
Begin
  Result := TFslHTMLElement(ObjectByIndex[iIndex]);
End;


Procedure TFslHTMLItem.Assign(oSource: TFslObject);
Begin
  Inherited;

  FId := TFslHTMLItem(oSource).FId;
  FClasses.Assign(TFslHTMLItem(oSource).FClasses);
  Style := TFslHTMLItem(oSource).FStyle.Link;
End;


Function TFslHTMLItem.Clone: TFslHTMLItem;
Begin
  Result := TFslHTMLItem(Inherited Clone);
End;


Constructor TFslHTMLItem.Create;
Begin
  Inherited;

  FClasses := TFslCSSClasses.Create;
  FStyle := TFslCSSFragment.Create;

  FClasses.Symbol := ' ';
End;


Destructor TFslHTMLItem.Destroy;
Begin
  FClasses.Free;
  FStyle.Free;

  Inherited;
End;


Function TFslHTMLItem.ElementType: TFslHTMLElementType;
Begin
  Result := ahError;
  RaiseError('ElementType', 'Need to override ElementType in '+ClassName);
End;


Function TFslHTMLItem.Link: TFslHTMLItem;
Begin
  Result := TFslHTMLItem(Inherited Link);
End;


Procedure TFslHTMLItem.SetClasses(Const oValue: TFslCSSClasses);
Begin
  FClasses.Free;
  FClasses := oValue;
End;

Procedure TFslHTMLItem.SetStyle(Const oValue: TFslCSSFragment);
Begin
  FStyle.Free;
  FStyle := oValue;
End;


function TFslHTMLItem.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FId.length * sizeof(char)) + 12);
  inc(result, FClasses.sizeInBytes);
  inc(result, FStyle.sizeInBytes);
end;

Function TFslHTMLItems.GetHTMLItem(iIndex: Integer): TFslHTMLItem;
Begin
  Result := TFslHTMLItem(ObjectByIndex[iIndex]);
End;


Function TFslHTMLItems.ItemClass: TFslObjectClass;
Begin
  Result := TFslHTMLItem;
End;


Procedure TFslHTMLComment.Assign(oSource: TFslObject);
Begin
  Inherited;

  FText := TFslHTMLComment(oSource).FText;
End;


Function TFslHTMLComment.Clone: TFslHTMLComment;
Begin
  Result := TFslHTMLComment(Inherited Clone);
End;


Function TFslHTMLComment.ElementType: TFslHTMLElementType;
Begin
  Result := ahComment;
End;


Function TFslHTMLComment.Link: TFslHTMLComment;
Begin
  Result := TFslHTMLComment(Inherited Link);
End;


function TFslHTMLComment.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FText.length * sizeof(char)) + 12);
end;

Procedure TFslHTMLTextFragment.Assign(oSource: TFslObject);
Begin
  Inherited;
  FText := TFslHTMLTextFragment(oSource).FText;
End;

Function TFslHTMLTextFragment.Clone: TFslHTMLTextFragment;
Begin
  Result := TFslHTMLTextFragment(Inherited Clone);
End;

Function TFslHTMLTextFragment.ElementType: TFslHTMLElementType;
Begin
  Result := ahTextFragment;
End;

Function TFslHTMLTextFragment.Link: TFslHTMLTextFragment;
Begin
  Result := TFslHTMLTextFragment(Inherited Link);
End;


function TFslHTMLTextFragment.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FText.length * sizeof(char)) + 12);
end;

Procedure TFslHTMLEntity.Assign(oSource: TFslObject);
Begin
  Inherited;
  FEntity := TFslHTMLEntity(oSource).FEntity;
End;

Function TFslHTMLEntity.Clone: TFslHTMLEntity;
Begin
  Result := TFslHTMLEntity(Inherited Clone);
End;

Function TFslHTMLEntity.ElementType: TFslHTMLElementType;
Begin
  Result := ahEntity;
End;

Function TFslHTMLEntity.Link: TFslHTMLEntity;
Begin
  Result := TFslHTMLEntity(Inherited Link);
End;


function TFslHTMLEntity.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FEntity.length * sizeof(char)) + 12);
end;

Procedure TFslHTMLImage.Assign(oSource: TFslObject);
Begin
  Inherited;
  FHorizontalSpace := TFslHTMLImage(oSource).FHorizontalSpace;
  FAlternative := TFslHTMLImage(oSource).FAlternative;
  FLongDescription := TFslHTMLImage(oSource).FLongDescription;
  FHeight := TFslHTMLImage(oSource).FHeight;
  FBorder := TFslHTMLImage(oSource).FBorder;
  FSource := TFslHTMLImage(oSource).FSource;
  FName := TFslHTMLImage(oSource).FName;
  FVerticalSpace := TFslHTMLImage(oSource).FVerticalSpace;
  FWidth := TFslHTMLImage(oSource).FWidth;
  FAlign := TFslHTMLImage(oSource).FAlign;
End;

Function TFslHTMLImage.Clone: TFslHTMLImage;
Begin
  Result := TFslHTMLImage(Inherited Clone);
End;

Function TFslHTMLImage.ElementType: TFslHTMLElementType;
Begin
  Result := ahImageReference;
End;

Function TFslHTMLImage.Link: TFslHTMLImage;
Begin
  Result := TFslHTMLImage(Inherited Link);
End;


function TFslHTMLImage.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FHorizontalSpace.length * sizeof(char)) + 12);
  inc(result, (FAlternative.length * sizeof(char)) + 12);
  inc(result, (FLongDescription.length * sizeof(char)) + 12);
  inc(result, (FHeight.length * sizeof(char)) + 12);
  inc(result, (FBorder.length * sizeof(char)) + 12);
  inc(result, (FSource.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FVerticalSpace.length * sizeof(char)) + 12);
  inc(result, (FWidth.length * sizeof(char)) + 12);
end;

Procedure TFslHTMLAnchor.Assign(oSource: TFslObject);
Begin
  Inherited;
  FURL := TFslHTMLAnchor(oSource).FURL;
  FName := TFslHTMLAnchor(oSource).FName;
  FAccessKey := TFslHTMLAnchor(oSource).FAccessKey;
End;

Function TFslHTMLAnchor.Clone: TFslHTMLAnchor;
Begin
  Result := TFslHTMLAnchor(Inherited Clone);
End;

Function TFslHTMLAnchor.ElementType: TFslHTMLElementType;
Begin
  Result := ahAnchor;
End;

Function TFslHTMLAnchor.Link: TFslHTMLAnchor;
Begin
  Result := TFslHTMLAnchor(Inherited Link);
End;


function TFslHTMLAnchor.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FURL.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FAccessKey.length * sizeof(char)) + 12);
end;

Procedure TFslHTMLBreak.Assign(oSource: TFslObject);
Begin
  Inherited;
  FClearType := TFslHTMLBreak(oSource).FClearType;
End;

Function TFslHTMLBreak.Clone: TFslHTMLBreak;
Begin
  Result := TFslHTMLBreak(Inherited Clone);
End;

Function TFslHTMLBreak.ElementType: TFslHTMLElementType;
Begin
  Result := ahBreak;
End;

Function TFslHTMLBreak.Link: TFslHTMLBreak;
Begin
  Result := TFslHTMLBreak(Inherited Link);
End;


Procedure TFslHTMLSection.Assign(oSource: TFslObject);
Begin
  Inherited;
  FItems.Assign(TFslHTMLSection(oSource).FItems);
End;

Procedure TFslHTMLSection.Clear;
Begin
  Inherited;

  FItems.Clear;
End;


Function TFslHTMLSection.Clone: TFslHTMLSection;
Begin
  Result := TFslHTMLSection(Inherited Clone);
End;

Constructor TFslHTMLSection.Create;
Begin
  Inherited;
  FItems := TFslHTMLItems.Create;
End;

Destructor TFslHTMLSection.Destroy;
Begin
  FItems.Free;
  Inherited;
End;

Function TFslHTMLSection.ElementType: TFslHTMLElementType;
Begin
  Result := ahSection;
End;

Function TFslHTMLSection.Link: TFslHTMLSection;
Begin
  Result := TFslHTMLSection(Inherited Link);
End;

Procedure TFslHTMLSection.SetItems(Const oValue: TFslHTMLItems);
Begin
  FItems.Free;
  FItems := oValue;
End;


function TFslHTMLSection.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FItems.sizeInBytes);
end;

Procedure TFslHTMLSpan.Assign(oSource: TFslObject);
Begin
  Inherited;
  FAlign := TFslHTMLSpan(oSource).FAlign;
End;

Function TFslHTMLSpan.Clone: TFslHTMLSpan;
Begin
  Result := TFslHTMLSpan(Inherited Clone);
End;

Function TFslHTMLSpan.ElementType: TFslHTMLElementType;
Begin
  Result := ahSpan;
End;

Function TFslHTMLSpan.Link: TFslHTMLSpan;
Begin
  Result := TFslHTMLSpan(Inherited Link);
End;


Function TFslHTMLEmphasis.Clone: TFslHTMLEmphasis;
Begin
  Result := TFslHTMLEmphasis(Inherited Clone);
End;

Function TFslHTMLEmphasis.ElementType: TFslHTMLElementType;
Begin
  Result := ahEmphasis;
End;

Function TFslHTMLEmphasis.Link: TFslHTMLEmphasis;
Begin
  Result := TFslHTMLEmphasis(Inherited Link);
End;


Function TFslHTMLStrong.Clone: TFslHTMLStrong;
Begin
  Result := TFslHTMLStrong(Inherited Clone);
End;

Function TFslHTMLStrong.ElementType: TFslHTMLElementType;
Begin
  Result := ahStrong;
End;

Function TFslHTMLStrong.Link: TFslHTMLStrong;
Begin
  Result := TFslHTMLStrong(Inherited Link);
End;


Function TFslHTMLCitation.Clone: TFslHTMLCitation;
Begin
  Result := TFslHTMLCitation(Inherited Clone);
End;

Function TFslHTMLCitation.ElementType: TFslHTMLElementType;
Begin
  Result := ahCitation;
End;

Function TFslHTMLCitation.Link: TFslHTMLCitation;
Begin
  Result := TFslHTMLCitation(Inherited Link);
End;


Function TFslHTMLDefinition.Clone: TFslHTMLDefinition;
Begin
  Result := TFslHTMLDefinition(Inherited Clone);
End;

Function TFslHTMLDefinition.ElementType: TFslHTMLElementType;
Begin
  Result := ahDefinition;
End;

Function TFslHTMLDefinition.Link: TFslHTMLDefinition;
Begin
  Result := TFslHTMLDefinition(Inherited Link);
End;


Function TFslHTMLCode.Clone: TFslHTMLCode;
Begin
  Result := TFslHTMLCode(Inherited Clone);
End;

Function TFslHTMLCode.ElementType: TFslHTMLElementType;
Begin
  Result := ahCode;
End;

Function TFslHTMLCode.Link: TFslHTMLCode;
Begin
  Result := TFslHTMLCode(Inherited Link);
End;


Function TFslHTMLSample.Clone: TFslHTMLSample;
Begin
  Result := TFslHTMLSample(Inherited Clone);
End;

Function TFslHTMLSample.ElementType: TFslHTMLElementType;
Begin
  Result := ahSample;
End;

Function TFslHTMLSample.Link: TFslHTMLSample;
Begin
  Result := TFslHTMLSample(Inherited Link);
End;


Function TFslHTMLKeyboardText.Clone: TFslHTMLKeyboardText;
Begin
  Result := TFslHTMLKeyboardText(Inherited Clone);
End;

Function TFslHTMLKeyboardText.ElementType: TFslHTMLElementType;
Begin
  Result := ahKeyboardText;
End;

Function TFslHTMLKeyboardText.Link: TFslHTMLKeyboardText;
Begin
  Result := TFslHTMLKeyboardText(Inherited Link);
End;


Function TFslHTMLVariable.Clone: TFslHTMLVariable;
Begin
  Result := TFslHTMLVariable(Inherited Clone);
End;

Function TFslHTMLVariable.ElementType: TFslHTMLElementType;
Begin
  Result := ahVariable;
End;

Function TFslHTMLVariable.Link: TFslHTMLVariable;
Begin
  Result := TFslHTMLVariable(Inherited Link);
End;


Function TFslHTMLAbbrevation.Clone: TFslHTMLAbbrevation;
Begin
  Result := TFslHTMLAbbrevation(Inherited Clone);
End;

Function TFslHTMLAbbrevation.ElementType: TFslHTMLElementType;
Begin
  Result := ahAbbrevation;
End;

Function TFslHTMLAbbrevation.Link: TFslHTMLAbbrevation;
Begin
  Result := TFslHTMLAbbrevation(Inherited Link);
End;


Function TFslHTMLAcronym.Clone: TFslHTMLAcronym;
Begin
  Result := TFslHTMLAcronym(Inherited Clone);
End;

Function TFslHTMLAcronym.ElementType: TFslHTMLElementType;
Begin
  Result := ahAcronym;
End;

Function TFslHTMLAcronym.Link: TFslHTMLAcronym;
Begin
  Result := TFslHTMLAcronym(Inherited Link);
End;


Function TFslHTMLTeleType.Clone: TFslHTMLTeleType;
Begin
  Result := TFslHTMLTeleType(Inherited Clone);
End;

Function TFslHTMLTeleType.ElementType: TFslHTMLElementType;
Begin
  Result := ahTeleType;
End;

Function TFslHTMLTeleType.Link: TFslHTMLTeleType;
Begin
  Result := TFslHTMLTeleType(Inherited Link);
End;


Function TFslHTMLItalic.Clone: TFslHTMLItalic;
Begin
  Result := TFslHTMLItalic(Inherited Clone);
End;

Function TFslHTMLItalic.ElementType: TFslHTMLElementType;
Begin
  Result := ahItalic;
End;

Function TFslHTMLItalic.Link: TFslHTMLItalic;
Begin
  Result := TFslHTMLItalic(Inherited Link);
End;


Function TFslHTMLBold.Clone: TFslHTMLBold;
Begin
  Result := TFslHTMLBold(Inherited Clone);
End;

Function TFslHTMLBold.ElementType: TFslHTMLElementType;
Begin
  Result := ahBold;
End;

Function TFslHTMLBold.Link: TFslHTMLBold;
Begin
  Result := TFslHTMLBold(Inherited Link);
End;


Function TFslHTMLBig.Clone: TFslHTMLBig;
Begin
  Result := TFslHTMLBig(Inherited Clone);
End;

Function TFslHTMLBig.ElementType: TFslHTMLElementType;
Begin
  Result := ahBig;
End;

Function TFslHTMLBig.Link: TFslHTMLBig;
Begin
  Result := TFslHTMLBig(Inherited Link);
End;


Function TFslHTMLSmall.Clone: TFslHTMLSmall;
Begin
  Result := TFslHTMLSmall(Inherited Clone);
End;

Function TFslHTMLSmall.ElementType: TFslHTMLElementType;
Begin
  Result := ahSmall;
End;

Function TFslHTMLSmall.Link: TFslHTMLSmall;
Begin
  Result := TFslHTMLSmall(Inherited Link);
End;


Function TFslHTMLStrikeOut.Clone: TFslHTMLStrikeOut;
Begin
  Result := TFslHTMLStrikeOut(Inherited Clone);
End;

Function TFslHTMLStrikeOut.ElementType: TFslHTMLElementType;
Begin
  Result := ahStrikeOut;
End;

Function TFslHTMLStrikeOut.Link: TFslHTMLStrikeOut;
Begin
  Result := TFslHTMLStrikeOut(Inherited Link);
End;


Function TFslHTMLUnderline.Clone: TFslHTMLUnderline;
Begin
  Result := TFslHTMLUnderline(Inherited Clone);
End;

Function TFslHTMLUnderline.ElementType: TFslHTMLElementType;
Begin
  Result := ahUnderline;
End;

Function TFslHTMLUnderline.Link: TFslHTMLUnderline;
Begin
  Result := TFslHTMLUnderline(Inherited Link);
End;


Procedure TFslHTMLFont.Assign(oSource: TFslObject);
Begin
  Inherited;
  FSize := TFslHTMLFont(oSource).FSize;
  FFace := TFslHTMLFont(oSource).FFace;
  FColour := TFslHTMLFont(oSource).FColour;
End;

Function TFslHTMLFont.Clone: TFslHTMLFont;
Begin
  Result := TFslHTMLFont(Inherited Clone);
End;

Function TFslHTMLFont.ElementType: TFslHTMLElementType;
Begin
  Result := ahFont;
End;

Function TFslHTMLFont.Link: TFslHTMLFont;
Begin
  Result := TFslHTMLFont(Inherited Link);
End;


function TFslHTMLFont.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSize.length * sizeof(char)) + 12);
  inc(result, (FFace.length * sizeof(char)) + 12);
end;

Function TFslHTMLQuotation.Clone: TFslHTMLQuotation;
Begin
  Result := TFslHTMLQuotation(Inherited Clone);
End;

Function TFslHTMLQuotation.ElementType: TFslHTMLElementType;
Begin
  Result := ahQuotation;
End;

Function TFslHTMLQuotation.Link: TFslHTMLQuotation;
Begin
  Result := TFslHTMLQuotation(Inherited Link);
End;


Function TFslHTMLSuperScript.Clone: TFslHTMLSuperScript;
Begin
  Result := TFslHTMLSuperScript(Inherited Clone);
End;

Function TFslHTMLSuperScript.ElementType: TFslHTMLElementType;
Begin
  Result := ahSuperScript;
End;

Function TFslHTMLSuperScript.Link: TFslHTMLSuperScript;
Begin
  Result := TFslHTMLSuperScript(Inherited Link);
End;


Function TFslHTMLSubScript.Clone: TFslHTMLSubScript;
Begin
  Result := TFslHTMLSubScript(Inherited Clone);
End;

Function TFslHTMLSubScript.ElementType: TFslHTMLElementType;
Begin
  Result := ahSubScript;
End;

Function TFslHTMLSubScript.Link: TFslHTMLSubScript;
Begin
  Result := TFslHTMLSubScript(Inherited Link);
End;


Function TFslHTMLBlockQuote.Clone: TFslHTMLBlockQuote;
Begin
  Result := TFslHTMLBlockQuote(Inherited Clone);
End;

Function TFslHTMLBlockQuote.ElementType: TFslHTMLElementType;
Begin
  Result := ahBlockQuote;
End;

Function TFslHTMLBlockQuote.Link: TFslHTMLBlockQuote;
Begin
  Result := TFslHTMLBlockQuote(Inherited Link);
End;


Procedure TFslHTMLHorizontalRule.Assign(oSource: TFslObject);
Begin
  Inherited;
  FNoShade := TFslHTMLHorizontalRule(oSource).FNoShade;
  FSize := TFslHTMLHorizontalRule(oSource).FSize;
  FWidth := TFslHTMLHorizontalRule(oSource).FWidth;
  FAlign := TFslHTMLHorizontalRule(oSource).FAlign;
End;

Function TFslHTMLHorizontalRule.Clone: TFslHTMLHorizontalRule;
Begin
  Result := TFslHTMLHorizontalRule(Inherited Clone);
End;

Function TFslHTMLHorizontalRule.ElementType: TFslHTMLElementType;
Begin
  Result := ahHorizontalRule;
End;

Function TFslHTMLHorizontalRule.Link: TFslHTMLHorizontalRule;
Begin
  Result := TFslHTMLHorizontalRule(Inherited Link);
End;


function TFslHTMLHorizontalRule.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSize.length * sizeof(char)) + 12);
  inc(result, (FWidth.length * sizeof(char)) + 12);
end;

Procedure TFslHTMLContainer.Assign(oSource: TFslObject);
Begin
  Inherited;
  FAlign := TFslHTMLContainer(oSource).FAlign;
End;

Function TFslHTMLContainer.Clone: TFslHTMLContainer;
Begin
  Result := TFslHTMLContainer(Inherited Clone);
End;

Function TFslHTMLContainer.ElementType: TFslHTMLElementType;
Begin
  Result := ahDiv;
End;

Function TFslHTMLContainer.Link: TFslHTMLContainer;
Begin
  Result := TFslHTMLContainer(Inherited Link);
End;

Function TFslHTMLParagraph.Clone: TFslHTMLParagraph;
Begin
  Result := TFslHTMLParagraph(Inherited Clone);
End;

Function TFslHTMLParagraph.ElementType: TFslHTMLElementType;
Begin
  Result := ahParagraph;
End;

Function TFslHTMLParagraph.Link: TFslHTMLParagraph;
Begin
  Result := TFslHTMLParagraph(Inherited Link);
End;


Function TFslHTMLPreformatted.Clone: TFslHTMLPreformatted;
Begin
  Result := TFslHTMLPreformatted(Inherited Clone);
End;

Function TFslHTMLPreformatted.ElementType: TFslHTMLElementType;
Begin
  Result := ahPreformatted;
End;

Function TFslHTMLPreformatted.Link: TFslHTMLPreformatted;
Begin
  Result := TFslHTMLPreformatted(Inherited Link);
End;


Procedure TFslHTMLHeading.Assign(oSource: TFslObject);
Begin
  Inherited;
  FLevel := TFslHTMLHeading(oSource).FLevel;
End;

Function TFslHTMLHeading.Clone: TFslHTMLHeading;
Begin
  Result := TFslHTMLHeading(Inherited Clone);
End;

Function TFslHTMLHeading.ElementType: TFslHTMLElementType;
Begin
  Result := ahHeading;
End;

Function TFslHTMLHeading.Link: TFslHTMLHeading;
Begin
  Result := TFslHTMLHeading(Inherited Link);
End;

Procedure TFslHTMLListItem.Assign(oSource: TFslObject);
Begin
  Inherited;
  FValue := TFslHTMLListItem(oSource).FValue;
  FHasValue := TFslHTMLListItem(oSource).FHasValue;
End;

Function TFslHTMLListItem.Clone: TFslHTMLListItem;
Begin
  Result := TFslHTMLListItem(Inherited Clone);
End;

Function TFslHTMLListItem.ElementType: TFslHTMLElementType;
Begin
  Result := ahListItem;
End;

Function TFslHTMLListItem.GetHasValue: Boolean;
Begin
  Result := FHasValue;
End;

Function TFslHTMLListItem.GetValue: Integer;
Begin
  Assert(CheckCondition(FHasValue, 'GetValue', 'Must have a value'));
  Result := FValue;
End;

Function TFslHTMLListItem.Link: TFslHTMLListItem;
Begin
  Result := TFslHTMLListItem(Inherited Link);
End;


function TFslHTMLListItem.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

Function TFslHTMLListItems.GetListItem(iIndex: Integer): TFslHTMLListItem;
Begin
  Result := TFslHTMLListItem(ObjectByIndex[iIndex]);
End;

Function TFslHTMLListItems.ItemClass: TFslObjectClass;
Begin
  Result := TFslHTMLListItem;
End;


Procedure TFslHTMLListElement.Assign(oSource: TFslObject);
Begin
  Inherited;
  FCompact := TFslHTMLListElement(oSource).FCompact;
  FList.Assign(TFslHTMLListElement(oSource).FList);
End;

Function TFslHTMLListElement.Clone: TFslHTMLListElement;
Begin
  Result := TFslHTMLListElement(Inherited Clone);
End;

Constructor TFslHTMLListElement.Create;
Begin
  Inherited;
  FList := TFslHTMLListItems.Create;
End;

Destructor TFslHTMLListElement.Destroy;
Begin
  FList.Free;
  Inherited;
End;

Function TFslHTMLListElement.ElementType: TFslHTMLElementType;
Begin
  Result := ahListElement;
End;

Function TFslHTMLListElement.Link: TFslHTMLListElement;
Begin
  Result := TFslHTMLListElement(Inherited Link);
End;

Procedure TFslHTMLListElement.SetList(Const oValue: TFslHTMLListItems);
Begin
  FList.Free;
  FList := oValue;
End;


function TFslHTMLListElement.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

Procedure TFslHTMLOrderedList.Assign(oSource: TFslObject);
Begin
  Inherited;
  FStart := TFslHTMLOrderedList(oSource).FStart;
  FListType := TFslHTMLOrderedList(oSource).FListType;
End;

Function TFslHTMLOrderedList.Clone: TFslHTMLOrderedList;
Begin
  Result := TFslHTMLOrderedList(Inherited Clone);
End;

Function TFslHTMLOrderedList.ElementType: TFslHTMLElementType;
Begin
  Result := ahOrderedList;
End;

Function TFslHTMLOrderedList.Link: TFslHTMLOrderedList;
Begin
  Result := TFslHTMLOrderedList(Inherited Link);
End;


Procedure TFslHTMLUnorderedList.Assign(oSource: TFslObject);
Begin
  Inherited;
  FListType := TFslHTMLUnorderedList(oSource).FListType;
End;

Function TFslHTMLUnorderedList.Clone: TFslHTMLUnorderedList;
Begin
  Result := TFslHTMLUnorderedList(Inherited Clone);
End;

Function TFslHTMLUnorderedList.ElementType: TFslHTMLElementType;
Begin
  Result := ahUnOrderedList;
End;

Function TFslHTMLUnorderedList.Link: TFslHTMLUnorderedList;
Begin
  Result := TFslHTMLUnorderedList(Inherited Link);
End;


Procedure TFslHTMLDefinitionListItem.Assign(oSource: TFslObject);
Begin
  Inherited;
  Term := TFslHTMLDefinitionListItem(oSource).FTerm.Link;
End;

Function TFslHTMLDefinitionListItem.Clone: TFslHTMLDefinitionListItem;
Begin
  Result := TFslHTMLDefinitionListItem(Inherited Clone);
End;

Constructor TFslHTMLDefinitionListItem.Create;
Begin
  Inherited;
  FTerm := TFslHTMLItem.Create;
End;

Destructor TFslHTMLDefinitionListItem.Destroy;
Begin
  FTerm.Free;
  Inherited;
End;

Function TFslHTMLDefinitionListItem.ElementType: TFslHTMLElementType;
Begin
  Result := ahDefinitionListItem;
End;

Function TFslHTMLDefinitionListItem.Link: TFslHTMLDefinitionListItem;
Begin
  Result := TFslHTMLDefinitionListItem(Inherited Link);
End;

Procedure TFslHTMLDefinitionListItem.SetTerm(Const oValue: TFslHTMLItem);
Begin
  FTerm.Free;
  FTerm := oValue;
End;


function TFslHTMLDefinitionListItem.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FTerm.sizeInBytes);
end;

Function TFslHTMLDefinitionListItems.GetListItem(iIndex: Integer): TFslHTMLDefinitionListItem;
Begin
  Result := TFslHTMLDefinitionListItem(ObjectByIndex[iIndex]);
End;

Function TFslHTMLDefinitionListItems.ItemClass: TFslObjectClass;
Begin
  Result := TFslHTMLDefinitionListItem;
End;


Procedure TFslHTMLDefinitionList.Assign(oSource: TFslObject);
Begin
  Inherited;
  FListItems.Assign(TFslHTMLDefinitionList(oSource).FListItems);
End;

Function TFslHTMLDefinitionList.Clone: TFslHTMLDefinitionList;
Begin
  Result := TFslHTMLDefinitionList(Inherited Clone);
End;

Constructor TFslHTMLDefinitionList.Create;
Begin
  Inherited;
  FListItems := TFslHTMLDefinitionListItems.Create;
End;

Destructor TFslHTMLDefinitionList.Destroy;
Begin
  FListItems.Free;
  Inherited;
End;

Function TFslHTMLDefinitionList.ElementType: TFslHTMLElementType;
Begin
  Result := ahDefinitionList;
End;

Function TFslHTMLDefinitionList.Link: TFslHTMLDefinitionList;
Begin
  Result := TFslHTMLDefinitionList(Inherited Link);
End;

Procedure TFslHTMLDefinitionList.SetListItems(Const oValue: TFslHTMLDefinitionListItems);
Begin
  FListItems.Free;
  FListItems := oValue;
End;


function TFslHTMLDefinitionList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FListItems.sizeInBytes);
end;

Procedure TFslHTMLTableCell.Assign(oSource: TFslObject);
Begin
  Inherited;
  FNoWrap := TFslHTMLTableCell(oSource).FNoWrap;
  FRowspan := TFslHTMLTableCell(oSource).FRowspan;
  FColspan := TFslHTMLTableCell(oSource).FColspan;
  FWidth := TFslHTMLTableCell(oSource).FWidth;
  FHeight := TFslHTMLTableCell(oSource).FHeight;
  FVertAlign := TFslHTMLTableCell(oSource).FVertAlign;
  FHorizAlign := TFslHTMLTableCell(oSource).FHorizAlign;
  FBgColour := TFslHTMLTableCell(oSource).FBgColour;
  FIsHeader := TFslHTMLTableCell(oSource).FIsHeader;
End;

Function TFslHTMLTableCell.Clone: TFslHTMLTableCell;
Begin
  Result := TFslHTMLTableCell(Inherited Clone);
End;

Function TFslHTMLTableCell.ElementType: TFslHTMLElementType;
Begin
  Result := ahTableCell;
End;

Function TFslHTMLTableCell.Link: TFslHTMLTableCell;
Begin
  Result := TFslHTMLTableCell(Inherited Link);
End;


function TFslHTMLTableCell.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FRowspan.length * sizeof(char)) + 12);
  inc(result, (FColspan.length * sizeof(char)) + 12);
  inc(result, (FWidth.length * sizeof(char)) + 12);
  inc(result, (FHeight.length * sizeof(char)) + 12);
end;

Function TFslHTMLTableCells.GetCell(iIndex: Integer): TFslHTMLTableCell;
Begin
  Result := TFslHTMLTableCell(ObjectByIndex[iIndex]);
End;

Function TFslHTMLTableCells.ItemClass: TFslObjectClass;
Begin
  Result := TFslHTMLTableCell;
End;


Procedure TFslHTMLTableRow.Assign(oSource: TFslObject);
Begin
  Inherited;
  FVertAlign := TFslHTMLTableRow(oSource).FVertAlign;
  FHorizAlign := TFslHTMLTableRow(oSource).FHorizAlign;
  FBgColour := TFslHTMLTableRow(oSource).FBgColour;
  FCells.Assign(TFslHTMLTableRow(oSource).FCells);
End;

Function TFslHTMLTableRow.Clone: TFslHTMLTableRow;
Begin
  Result := TFslHTMLTableRow(Inherited Clone);
End;

Constructor TFslHTMLTableRow.Create;
Begin
  Inherited;
  FCells := TFslHTMLTableCells.Create;
End;

Destructor TFslHTMLTableRow.Destroy;
Begin
  FCells.Free;
  Inherited;
End;

Function TFslHTMLTableRow.ElementType: TFslHTMLElementType;
Begin
  Result := ahTableRow;
End;

Function TFslHTMLTableRow.Link: TFslHTMLTableRow;
Begin
  Result := TFslHTMLTableRow(Inherited Link);
End;

Procedure TFslHTMLTableRow.SetCells(Const oValue: TFslHTMLTableCells);
Begin
  FCells.Free;
  FCells := oValue;
End;


function TFslHTMLTableRow.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCells.sizeInBytes);
end;

Function TFslHTMLTableRows.GetRow(iIndex: Integer): TFslHTMLTableRow;
Begin
  Result := TFslHTMLTableRow(ObjectByIndex[iIndex]);
End;

Function TFslHTMLTableRows.ItemClass: TFslObjectClass;
Begin
  Result := TFslHTMLTableRow;
End;


Procedure TFslHTMLTableSection.Assign(oSource: TFslObject);
Begin
  Inherited;
  FRows.Assign(TFslHTMLTableSection(oSource).FRows);
  FSectionType := TFslHTMLTableSection(oSource).FSectionType;
End;

Function TFslHTMLTableSection.Clone: TFslHTMLTableSection;
Begin
  Result := TFslHTMLTableSection(Inherited Clone);
End;

Constructor TFslHTMLTableSection.Create;
Begin
  Inherited;
  FRows := TFslHTMLTableRows.Create;
End;

Destructor TFslHTMLTableSection.Destroy;
Begin
  FRows.Free;

  Inherited;
End;


Function TFslHTMLTableSection.ElementType: TFslHTMLElementType;
Begin
  Result := ahTableSection;
End;


Function TFslHTMLTableSection.Link: TFslHTMLTableSection;
Begin
  Result := TFslHTMLTableSection(Inherited Link);
End;


Procedure TFslHTMLTableSection.SetRows(Const oValue: TFslHTMLTableRows);
Begin
  FRows.Free;
  FRows := oValue;
End;


function TFslHTMLTableSection.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FRows.sizeInBytes);
end;

Function TFslHTMLTableSections.GetSection(iIndex: Integer): TFslHTMLTableSection;
Begin
  Result := TFslHTMLTableSection(ObjectByIndex[iIndex]);
End;


Function TFslHTMLTableSections.ItemClass: TFslObjectClass;
Begin
  Result := TFslHTMLTableSection;
End;


Procedure TFslHTMLTableCaption.Assign(oSource: TFslObject);
Begin
  Inherited;

  FAlign := TFslHTMLTableCaption(oSource).FAlign;
End;


Function TFslHTMLTableCaption.Clone: TFslHTMLTableCaption;
Begin
  Result := TFslHTMLTableCaption(Inherited Clone);
End;

Function TFslHTMLTableCaption.ElementType: TFslHTMLElementType;
Begin
  Result := ahTableCaption;
End;


Function TFslHTMLTableCaption.Link: TFslHTMLTableCaption;
Begin
  Result := TFslHTMLTableCaption(Inherited Link);
End;


function TFslHTMLTableCaption.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

Procedure TFslHTMLTable.Assign(oSource: TFslObject);
Begin
  Inherited;

  FSummary := TFslHTMLTable(oSource).FSummary;
  FWidth := TFslHTMLTable(oSource).FWidth;
  FCellpadding := TFslHTMLTable(oSource).FCellpadding;
  FCellspacing := TFslHTMLTable(oSource).FCellspacing;
  FBorder := TFslHTMLTable(oSource).FBorder;
  FAlign := TFslHTMLTable(oSource).FAlign;
  FBgColour := TFslHTMLTable(oSource).FBgColour;
  Caption := TFslHTMLTable(oSource).FCaption.Link;
  FRows.Assign(TFslHTMLTable(oSource).FRows);
  FSections.Assign(TFslHTMLTable(oSource).FSections);
End;


Function TFslHTMLTable.Clone: TFslHTMLTable;
Begin
  Result := TFslHTMLTable(Inherited Clone);
End;


Constructor TFslHTMLTable.Create;
Begin
  Inherited;
  FCaption := TFslHTMLTableCaption.Create;
  FRows := TFslHTMLTableRows.Create;
  FSections := TFslHTMLTableSections.Create;
End;



Destructor TFslHTMLTable.Destroy;
Begin
  FCaption.Free;
  FRows.Free;
  FSections.Free;
  Inherited;
End;


Function TFslHTMLTable.ElementType: TFslHTMLElementType;
Begin
  Result := ahTable;
End;


Function TFslHTMLTable.Link: TFslHTMLTable;
Begin
  Result := TFslHTMLTable(Inherited Link);
End;


Procedure TFslHTMLTable.SetCaption(Const oValue: TFslHTMLTableCaption);
Begin
  FCaption.Free;
  FCaption := oValue;
End;


Procedure TFslHTMLTable.SetRows(Const oValue: TFslHTMLTableRows);
Begin
  FRows.Free;
  FRows := oValue;
End;


Procedure TFslHTMLTable.SetSections(Const oValue: TFslHTMLTableSections);
Begin
  FSections.Free;
  FSections := oValue;
End;


function TFslHTMLTable.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSummary.length * sizeof(char)) + 12);
  inc(result, (FWidth.length * sizeof(char)) + 12);
  inc(result, (FCellpadding.length * sizeof(char)) + 12);
  inc(result, (FCellspacing.length * sizeof(char)) + 12);
  inc(result, (FBorder.length * sizeof(char)) + 12);
  inc(result, FCaption.sizeInBytes);
  inc(result, FRows.sizeInBytes);
  inc(result, FSections.sizeInBytes);
end;

Procedure TFslHTMLFormControl.Assign(oSource: TFslObject);
Begin
  Inherited;

  FDisabled := TFslHTMLFormControl(oSource).FDisabled;
  FTabIndex := TFslHTMLFormControl(oSource).FTabIndex;
  FName := TFslHTMLFormControl(oSource).FName;
  FValue := TFslHTMLFormControl(oSource).FValue;
End;


Function TFslHTMLFormControl.Clone: TFslHTMLFormControl;
Begin
  Result := TFslHTMLFormControl(Inherited Clone);
End;



Function TFslHTMLFormControl.ElementType: TFslHTMLElementType;
Begin
  Result := ahFormControl;
End;


Function TFslHTMLFormControl.Link: TFslHTMLFormControl;
Begin
  Result := TFslHTMLFormControl(Inherited Link);
End;


function TFslHTMLFormControl.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FTabIndex.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

Procedure TFslHTMLInput.Assign(oSource: TFslObject);
Begin
  Inherited;

  FReadOnly := TFslHTMLInput(oSource).FReadOnly;
  FChecked := TFslHTMLInput(oSource).FChecked;
  FSize := TFslHTMLInput(oSource).FSize;
  FAccept := TFslHTMLInput(oSource).FAccept;
  FSource := TFslHTMLInput(oSource).FSource;
  FMaxLength := TFslHTMLInput(oSource).FMaxLength;
  FAlternative := TFslHTMLInput(oSource).FAlternative;
  FAlign := TFslHTMLInput(oSource).FAlign;
  FInputType := TFslHTMLInput(oSource).FInputType;
End;


Function TFslHTMLInput.Clone: TFslHTMLInput;
Begin
  Result := TFslHTMLInput(Inherited Clone);
End;



Function TFslHTMLInput.ElementType: TFslHTMLElementType;
Begin
  Result := ahInput;
End;


Function TFslHTMLInput.Link: TFslHTMLInput;
Begin
  Result := TFslHTMLInput(Inherited Link);
End;


function TFslHTMLInput.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSize.length * sizeof(char)) + 12);
  inc(result, (FAccept.length * sizeof(char)) + 12);
  inc(result, (FSource.length * sizeof(char)) + 12);
  inc(result, (FMaxLength.length * sizeof(char)) + 12);
  inc(result, (FAlternative.length * sizeof(char)) + 12);
end;

Procedure TFslHTMLButton.Assign(oSource: TFslObject);
Begin
  Inherited;

  FButtonType := TFslHTMLButton(oSource).FButtonType;
End;


Function TFslHTMLButton.Clone: TFslHTMLButton;
Begin
  Result := TFslHTMLButton(Inherited Clone);
End;



Function TFslHTMLButton.ElementType: TFslHTMLElementType;
Begin
  Result := ahButton;
End;


Function TFslHTMLButton.Link: TFslHTMLButton;
Begin
  Result := TFslHTMLButton(Inherited Link);
End;


function TFslHTMLButton.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

Procedure TFslHTMLOptionItem.Assign(oSource: TFslObject);
Begin
  Inherited;

  FDisabled := TFslHTMLOptionItem(oSource).FDisabled;
  FCaption := TFslHTMLOptionItem(oSource).FCaption;
End;


Function TFslHTMLOptionItem.Clone: TFslHTMLOptionItem;
Begin
  Result := TFslHTMLOptionItem(Inherited Clone);
End;



Function TFslHTMLOptionItem.ElementType: TFslHTMLElementType;
Begin
  Result := ahOptionItem;
End;


Function TFslHTMLOptionItem.Link: TFslHTMLOptionItem;
Begin
  Result := TFslHTMLOptionItem(Inherited Link);
End;


function TFslHTMLOptionItem.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCaption.length * sizeof(char)) + 12);
end;

Function TFslHTMLOptionItems.GetOption(iIndex: Integer): TFslHTMLOptionItem;
Begin
  Result := TFslHTMLOptionItem(ObjectByIndex[iIndex]);
End;


Function TFslHTMLOptionItems.ItemClass: TFslObjectClass;
Begin
  Result := TFslHTMLOptionItem;
End;


Procedure TFslHTMLOptGroup.Assign(oSource: TFslObject);
Begin
  Inherited;

  FOptions.Assign(TFslHTMLOptGroup(oSource).FOptions);
End;


Function TFslHTMLOptGroup.Clone: TFslHTMLOptGroup;
Begin
  Result := TFslHTMLOptGroup(Inherited Clone);
End;


Constructor TFslHTMLOptGroup.Create;
Begin
  Inherited;

  FOptions := TFslHTMLOptionItems.Create;
End;


Destructor TFslHTMLOptGroup.Destroy;
Begin
  FOptions.Free;

  Inherited;
End;


Function TFslHTMLOptGroup.ElementType: TFslHTMLElementType;
Begin
  Result := ahOptGroup;
End;


Function TFslHTMLOptGroup.Link: TFslHTMLOptGroup;
Begin
  Result := TFslHTMLOptGroup(Inherited Link);
End;


Procedure TFslHTMLOptGroup.SetOptions(Const oValue: TFslHTMLOptionItems);
Begin
  FOptions.Free;
  FOptions := oValue;
End;


function TFslHTMLOptGroup.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FOptions.sizeInBytes);
end;

Procedure TFslHTMLOption.Assign(oSource: TFslObject);
Begin
  Inherited;

  FSelected := TFslHTMLOption(oSource).FSelected;
  FValue := TFslHTMLOption(oSource).FValue;
End;


Function TFslHTMLOption.Clone: TFslHTMLOption;
Begin
  Result := TFslHTMLOption(Inherited Clone);
End;


Function TFslHTMLOption.ElementType: TFslHTMLElementType;
Begin
  Result := ahOption;
End;


Function TFslHTMLOption.Link: TFslHTMLOption;
Begin
  Result := TFslHTMLOption(Inherited Link);
End;


function TFslHTMLOption.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

Procedure TFslHTMLSelect.Assign(oSource: TFslObject);
Begin
  Inherited;

  FMultiple := TFslHTMLSelect(oSource).FMultiple;
  FSize := TFslHTMLSelect(oSource).FSize;
  FAllOptions.Assign(TFslHTMLSelect(oSource).FAllOptions);
  FOptions.Assign(TFslHTMLSelect(oSource).FOptions);
End;


Function TFslHTMLSelect.Clone: TFslHTMLSelect;
Begin
  Result := TFslHTMLSelect(Inherited Clone);
End;


Constructor TFslHTMLSelect.Create;
Begin
  Inherited;

  FAllOptions := TFslHTMLOptionItems.Create;
  FOptions := TFslHTMLOptionItems.Create;
End;


Destructor TFslHTMLSelect.Destroy;
Begin
  FAllOptions.Free;
  FOptions.Free;

  Inherited;
End;


Function TFslHTMLSelect.ElementType: TFslHTMLElementType;
Begin
  Result := ahSelect;
End;


Function TFslHTMLSelect.Link: TFslHTMLSelect;
Begin
  Result := TFslHTMLSelect(Inherited Link);
End;


Procedure TFslHTMLSelect.SetAllOptions(Const oValue: TFslHTMLOptionItems);
Begin
  FAllOptions.Free;
  FAllOptions := oValue;
End;


Procedure TFslHTMLSelect.SetOptions(Const oValue: TFslHTMLOptionItems);
Begin
  FOptions.Free;
  FOptions := oValue;
End;


function TFslHTMLSelect.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSize.length * sizeof(char)) + 12);
  inc(result, FAllOptions.sizeInBytes);
  inc(result, FOptions.sizeInBytes);
end;

Procedure TFslHTMLTextArea.Assign(oSource: TFslObject);
Begin
  Inherited;

  FReadOnly := TFslHTMLTextArea(oSource).FReadOnly;
  FRows := TFslHTMLTextArea(oSource).FRows;
  FCols := TFslHTMLTextArea(oSource).FCols;
End;


Function TFslHTMLTextArea.Clone: TFslHTMLTextArea;
Begin
  Result := TFslHTMLTextArea(Inherited Clone);
End;



Function TFslHTMLTextArea.ElementType: TFslHTMLElementType;
Begin
  Result := ahTextArea;
End;


Function TFslHTMLTextArea.Link: TFslHTMLTextArea;
Begin
  Result := TFslHTMLTextArea(Inherited Link);
End;


function TFslHTMLTextArea.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

Procedure TFslHTMLForm.Assign(oSource: TFslObject);
Begin
  Inherited;

  FAction := TFslHTMLForm(oSource).FAction;
  FEncType := TFslHTMLForm(oSource).FEncType;
  FName := TFslHTMLForm(oSource).FName;
  FMethod := TFslHTMLForm(oSource).FMethod;
End;


Function TFslHTMLForm.Clone: TFslHTMLForm;
Begin
  Result := TFslHTMLForm(Inherited Clone);
End;



Function TFslHTMLForm.ElementType: TFslHTMLElementType;
Begin
  Result := ahForm;
End;


Function TFslHTMLForm.Link: TFslHTMLForm;
Begin
  Result := TFslHTMLForm(Inherited Link);
End;


function TFslHTMLForm.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FAction.length * sizeof(char)) + 12);
  inc(result, (FEncType.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
end;

Procedure TFslHTMLBody.Assign(oSource: TFslObject);
Begin
  Inherited;
  FLinkColor := TFslHTMLBody(oSource).FLinkColor;
  FVlinkcolor := TFslHTMLBody(oSource).FVlinkcolor;
  FAlinkColor := TFslHTMLBody(oSource).FAlinkColor;
End;


Function TFslHTMLBody.Clone: TFslHTMLBody;
Begin
  Result := TFslHTMLBody(Inherited Clone);
End;



Function TFslHTMLBody.ElementType: TFslHTMLElementType;
Begin
  Result := ahBody;
End;


Function TFslHTMLBody.Link: TFslHTMLBody;
Begin
  Result := TFslHTMLBody(Inherited Link);
End;


function TFslHTMLBody.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

Procedure TFslHTMLMetaEntry.Assign(oSource: TFslObject);
Begin
  Inherited;

  FHttpEquiv := TFslHTMLMetaEntry(oSource).FHttpEquiv;
  FContent := TFslHTMLMetaEntry(oSource).FContent;
  FName := TFslHTMLMetaEntry(oSource).FName;
  FScheme := TFslHTMLMetaEntry(oSource).FScheme;
End;


Function TFslHTMLMetaEntry.Clone: TFslHTMLMetaEntry;
Begin
  Result := TFslHTMLMetaEntry(Inherited Clone);
End;



Function TFslHTMLMetaEntry.Link: TFslHTMLMetaEntry;
Begin
  Result := TFslHTMLMetaEntry(Inherited Link);
End;


function TFslHTMLMetaEntry.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FHttpEquiv.length * sizeof(char)) + 12);
  inc(result, (FContent.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FScheme.length * sizeof(char)) + 12);
end;

Function TFslHTMLMetaEntries.GetMeta(iIndex: Integer): TFslHTMLMetaEntry;
Begin
  Result := TFslHTMLMetaEntry(ObjectByIndex[iIndex]);
End;


Function TFslHTMLMetaEntries.ItemClass: TFslObjectClass;
Begin
  Result := TFslHTMLMetaEntry;
End;


Procedure TFslHTMLHead.Assign(oSource: TFslObject);
Begin
  Inherited;

  FMeta.Assign(TFslHTMLHead(oSource).FMeta);
  FStyles.Assign(TFslHTMLHead(oSource).FStyles);
  FTitle := TFslHTMLHead(oSource).FTitle;
End;


Procedure TFslHTMLHead.Clear;
Begin
  Inherited;

  FMeta.Clear;
  FStyles.Clear;
End;


function TFslHTMLHead.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMeta.sizeInBytes);
  inc(result, FStyles.sizeInBytes);
  inc(result, (FTitle.length * sizeof(char)) + 12);
end;

Procedure TFslHtmlHead.SetStyles(Const oValue: TFslCSSStyles);
Begin
  FStyles.Free;
  FStyles := oValue;
End;


Function TFslHTMLHead.Clone: TFslHTMLHead;
Begin
  Result := TFslHTMLHead(Inherited Clone);
End;


Constructor TFslHTMLHead.Create;
Begin
  Inherited;

  FMeta := TFslHTMLMetaEntries.Create;
  FStyles := TFslCSSStyles.Create;
End;


Destructor TFslHTMLHead.Destroy;
Begin
  FMeta.Free;
  FStyles.Free;

  Inherited;
End;


Function TFslHTMLHead.Link: TFslHTMLHead;
Begin
  Result := TFslHTMLHead(Inherited Link);
End;


Procedure TFslHTMLHead.SetMeta(Const oValue: TFslHTMLMetaEntries);
Begin
  FMeta.Free;
  FMeta := oValue;
End;


Procedure TFslHtmlDocument.Assign(oSource: TFslObject);
Begin
  Inherited;

  FDTD := TFslHtmlDocument(oSource).FDTD;
  FVersion := TFslHtmlDocument(oSource).FVersion;
  FStyles.Assign(TFslHtmlDocument(oSource).FStyles);
  Body := TFslHtmlDocument(oSource).FBody.Link;
  Head := TFslHtmlDocument(oSource).FHead.Link;
End;


Procedure TFslHtmlDocument.Clear;
Begin
  Inherited;

  FDTD := 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd';
  FVersion := '-//W3C//DTD XHTML 1.0 Strict//EN';

  FBody.Clear;
  FHead.Clear;
  FStyles.Clear;
End;


Function TFslHtmlDocument.Clone: TFslHtmlDocument;
Begin
  Result := TFslHtmlDocument(Inherited Clone);
End;


Constructor TFslHtmlDocument.Create;
Begin
  Inherited;

  FStyles := TFslCSSStyles.Create;
  FBody := TFslHTMLBody.Create;
  FHead := TFslHTMLHead.Create;
End;




Destructor TFslHtmlDocument.Destroy;
Begin
  FStyles.Free;
  FBody.Free;
  FHead.Free;

  Inherited;
End;


Function TFslHtmlDocument.Link: TFslHtmlDocument;
Begin
  Result := TFslHtmlDocument(Inherited Link);
End;


Procedure TFslHtmlDocument.SetBody(Const oValue: TFslHTMLBody);
Begin
  FBody.Free;
  FBody := oValue;
End;


Procedure TFslHtmlDocument.SetHead(Const oValue: TFslHTMLHead);
Begin
  FHead.Free;
  FHead := oValue;
End;


Procedure TFslHtmlDocument.SetStyles(Const oValue: TFslCSSStyles);
Begin
  FStyles.Free;
  FStyles := oValue;
End;


function TFslHtmlDocument.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FDTD.length * sizeof(char)) + 12);
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, FStyles.sizeInBytes);
  inc(result, FBody.sizeInBytes);
  inc(result, FHead.sizeInBytes);
end;

Procedure TFslHTMLListItem.SetHasValue(Const Value: Boolean);
Begin
  FHasValue := Value;
End;

Procedure TFslHTMLListItem.SetValue(Const Value: Integer);
Begin
  FHasValue := True;
  FValue := Value;
End;


Var
  gTable : Array[Char] Of TFslHTMLToken;


Function ToHTMLToken(cChar : Char) : TFslHTMLToken;
Begin
  Result := gTable[cChar];
  if result = ahtNull then
    result := ahtIdentifier;
End;


Function TFslHTMLLexer.ConsumeToken: TFslHTMLToken;
Begin
  If FNextToken <> ahtNull Then
  Begin
    FCurrentToken := FNextToken;
    FNextToken := ahtNull;
  End
  Else
  Begin
    Case NextCharacter Of
      #0..' ' :
      Begin
        FCurrentToken := ConsumeWhitespace;
      End; { Whitespace }

      '0'..'9' :
      Begin
        FCurrentToken := ConsumeNumber;
      End;

      'a'..'z', 'A'..'Z', '_', '.' :
      Begin
        FCurrentToken := ConsumeIdentifier;
      End; { Identifier }

      '&' :
      Begin
        FCurrentToken := ConsumeSymbol;
      End;

      '?' :
      Begin
        FCurrentValue := ConsumeCharacter('?');

        If More And (NextCharacter = '>') Then
        Begin
          FCurrentValue := FCurrentValue + ConsumeCharacter('>');

          FCurrentToken := ahtCloseTagQuery;
        End
        Else
        Begin
          FCurrentToken := ahtQuery;
        End;
      End;

      '<' :
      Begin
        FCurrentValue := ConsumeCharacter('<');

        If More And (NextCharacter = '!') Then
        Begin
          FCurrentValue := FCurrentValue + ConsumeCharacter('!');

          If NextCharacter <> '-' Then
            FCurrentToken := ahtOpenTagExclamation
          Else
          Begin
            FCurrentValue := FCurrentValue + ConsumeCharacter('-');

            If More And (NextCharacter = '-') Then
            Begin
              FCurrentValue := ConsumeUntilString('-->');
              FCurrentToken := ahtComment;

              If More Then
                ConsumeString('-->');
            End
            Else
            Begin
              FCurrentToken := ahtOpenTagExclamation;
              ProduceCharacter('-');
            End;
          End;
        End
        Else If More And (NextCharacter = '/') Then
        Begin
          FCurrentValue := FCurrentValue + ConsumeCharacter('/');
          FCurrentToken := ahtOpenTagForwardSlash;
        End
        Else If More And (NextCharacter = '?') Then
        Begin
          FCurrentValue := FCurrentValue + ConsumeCharacter('?');
          FCurrentToken := ahtOpenTagQuery;
        End
        Else
        Begin
          FCurrentToken := ahtOpenTag;
        End;
      End;

      '>' :
      Begin
        FCurrentValue := ConsumeCharacter('>');
        FCurrentToken := ahtCloseTag;
      End;

      '/' :
      Begin
        FCurrentValue := ConsumeCharacter('/');

        If NextCharacter = '>' Then
        Begin
          FCurrentValue := FCurrentValue + ConsumeCharacter('>');
          FCurrentToken := ahtCloseTagForwardSlash;
        End
        Else
        Begin
          FCurrentToken := ahtForwardSlash;
        End;
      End;
    Else
      FCurrentValue := ConsumeCharacter;
      FCurrentToken := ToHTMLToken(FCurrentValue[1]);
    End;

  {$IFOPT C+}
//    ConsoleOpen;
//    ConsoleWriteLine(FCurrentValue);
  {$ENDIF}
  End;

  Result := FCurrentToken;
End;


Procedure TFslHTMLLexer.ConsumeToken(aToken: TFslHTMLToken);
Begin
  If NextToken <> aToken Then
    RaiseError('ConsumeToken', StringFormat('Expected token %s but found token %s.', [HTMLTOKEN_STRING[aToken], HTMLTOKEN_STRING[NextToken]]));

  ConsumeToken;
End;


Function TFslHTMLLexer.NextToken: TFslHTMLToken;
Begin
  If FNextToken = ahtNull Then
    FNextToken := ConsumeToken;

  Result := FNextToken;
End;


Function TFslHTMLLexer.ConsumeIdentifier : TFslHTMLToken;
Begin
  FCurrentValue := ConsumeWhileCharacterSet(setAlphabet + ['_', '.']);
  FCurrentValue := FCurrentValue + ConsumeWhileCharacterSet(setAlphanumeric + ['_', '-', '.', '/', '\']);

  Result := ahtIdentifier;
End;


Function TFslHTMLLexer.ConsumeWhitespace: TFslHTMLToken;
Begin
  FCurrentValue := '';
  While More And (NextCharacter <= ' ') Do
    FCurrentValue := FCurrentValue + ConsumeCharacter;

  Result := ahtWhitespace;
End;


Function TFslHTMLLexer.ConsumeNumber: TFslHTMLToken;
Begin
  FCurrentValue := '';
  While More And CharInSet(NextCharacter, setNumbers) Do
    FCurrentValue := FCurrentValue + ConsumeCharacter;

  If NextCharacter = '.' Then
  Begin
    ConsumeCharacter('.');

    FCurrentValue := FCurrentValue + '.';
    While More And CharInSet(NextCharacter, setNumbers) Do
      FCurrentValue := FCurrentValue + ConsumeCharacter;
  End;

  If More And (NextCharacter = '%') Then
    FCurrentValue := FCurrentValue + ConsumeCharacter('%');

  Result := ahtNumber;
End;


Function TFslHTMLLexer.ConsumeSymbol: TFslHTMLToken;
Begin
  FCurrentValue := ConsumeCharacter('&');

  If More Then
  Begin
    FCurrentValue := FCurrentValue + ConsumeUntilString(';');

    If More Then
    Begin
      FCurrentValue := FCurrentValue + ConsumeCharacter(';');

      If Lowercase(FCurrentValue) = '&nbsp;' Then
      Begin
        FCurrentValue := ' ';
        Result := ahtIdentifier;
      End
      Else
      Begin
        FCurrentValue := DecodeXML(FCurrentValue);

        If Length(FCurrentValue) = 1 Then
          Result := ToHTMLToken(FCurrentValue[1])
        Else
          Result := ahtIdentifier;
      End;
    End
    Else
    Begin
      Result := ahtIdentifier;
    End;
  End
  Else
  Begin
    Result := ahtAmpersand;
  End;
End;


function TFslHTMLLexer.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCurrentValue.length * sizeof(char)) + 12);
end;

Constructor TFslHTMLFormatter.Create;
Begin
  Inherited;

  FAdapter := TFslHTMLAdapter.Create;
End;


Destructor TFslHTMLFormatter.Destroy;
Begin
  FAdapter.Free;

  Inherited;
End;


Function TFslHTMLFormatter.Clone: TFslHTMLFormatter;
Begin
  Result := TFslHTMLFormatter(Inherited Clone);
End;


Function TFslHTMLFormatter.Link: TFslHTMLFormatter;
Begin
  Result := TFslHTMLFormatter(Inherited Link);
End;


Procedure TFslHTMLFormatter.SetStream(oStream: TFslStream);
Begin
  Inherited;

  FAdapter.Stream := oStream.Link;
End;


Procedure TFslHTMLFormatter.ProduceDocumentType(Const sVersion, sDTD : String);
Begin
  ProduceLine(StringFormat('<!DOCTYPE html PUBLIC "%s" "%s">', [sVersion, sDTD]));
End;


Procedure TFslHTMLFormatter.ProduceDocumentOpen;
Begin
  ProduceOpen('html');
End;


Procedure TFslHTMLFormatter.ProduceDocumentClose;
Begin
  ProduceClose('html');
End;


Procedure TFslHTMLFormatter.ProduceHeadOpen;
Begin
  ProduceOpen('head');
End;


Procedure TFslHTMLFormatter.ProduceHeadClose;
Begin
  ProduceClose('head');
End;


Procedure TFslHTMLFormatter.ProduceStyleOpen;
Begin
  ProduceOpen('style');
End;


Procedure TFslHTMLFormatter.ProduceStyleClose;
Begin
  ProduceClose('style');
End;


Procedure TFslHTMLFormatter.ProduceStyleFragment(Const sName : String; oProperties : TFslStringMatch);
Var
  aValue : TFslStringMatchItem;
  iLoop : Integer;
Begin
  ProduceStyleFragmentOpen(sName);

  For iLoop := 0 To oProperties.Count - 1 Do
  Begin
    aValue := oProperties.MatchByIndex[iLoop];

    ProduceStyleFragmentProperty(aValue.Key, aValue.Value);
  End;
End;


Procedure TFslHTMLFormatter.ProduceStyleFragmentOpen(Const sName : String);
Begin
  ProduceText(sName + ' { ');
  LevelDown;
End;


Procedure TFslHTMLFormatter.ProduceStyleFragmentClose;
Begin
  LevelUp;
  ProduceText('}');
End;


Procedure TFslHTMLFormatter.ProduceStyleFragmentProperty(Const sName, sValue : String);
Begin
  ProduceText(sName + ': ' + sValue + '; ');
End;


Procedure TFslHTMLFormatter.ProduceBodyOpen;
Begin
  ProduceOpen('body');
End;


Procedure TFslHTMLFormatter.ProduceBodyClose;
Begin
  ProduceClose('body');
End;


Procedure TFslHTMLFormatter.ProduceInlineTag(Const sTag : String);
Begin
  ProduceInLine('<' + sTag + UseAttributes + ' />');
End;


Procedure TFslHTMLFormatter.ProduceOpen(Const sTag : String; bInline : Boolean);
Begin
  If bInline Then
    ProduceInline('<' + sTag + UseAttributes + '>')
  Else
  Begin
    ProduceLine('<' + sTag + UseAttributes + '>');
    LevelDown;
  End;
End;


Procedure TFslHTMLFormatter.ProduceClose(Const sTag: String; bInline : Boolean);
Begin
  If bInline Then
    ProduceInline('</' + sTag + '>')
  Else
  Begin
    LevelUp;
    ProduceLine('</' + sTag + '>');
  End;
End;


Procedure TFslHTMLFormatter.ProduceEscaped(Const sData: String);
Begin
  FAdapter.Write(Pointer(sData)^, Length(sData));
End;


Procedure TFslHTMLFormatter.ProducePreformatted(Const sData: String);
Begin
  ProduceLine('<pre' + UseAttributes + '>' + EncodeXML(sData) + '</pre>');
End;


Procedure TFslHTMLFormatter.ProduceAnchor(Const sCaption, sReference, sBookmark, sTarget : String);
Var
  sLink : String;
Begin
  sLink := sReference;
  If sBookmark <> '' Then
    sLink := sLink + '#' + sBookmark;

  Attributes.add('href', sLink);
  If sTarget <> '' Then
    Attributes.add('target', sTarget);

  ProduceLine(StringFormat('<a %s>%s</a>', [UseAttributes, EncodeXML(sCaption)]));
End;


Procedure TFslHTMLFormatter.ProduceAnchorOpen(Const sReference, sBookmark, sTarget : String);
Var
  sLink : String;
Begin
  sLink := sReference;
  If sBookmark <> '' Then
    sLink := sLink + '#' + sBookmark;

  Attributes.add('href', sLink);
  If sTarget <> '' Then
    Attributes.add('target', sTarget);

  ProduceOpen('a');
End;


Procedure TFslHTMLFormatter.ProduceAnchorClose;
Begin
  ProduceClose('a');
End;


Procedure TFslHTMLFormatter.ProduceBookmark(Const sBookmark: String);
Begin
  Attributes.add('name', sBookmark);
  Attributes.add('id', sBookmark);

  ProduceLine('<a' + UseAttributes + '></a>');
End;


Procedure TFslHTMLFormatter.ProduceTitle(Const sTitle: String);
Begin
  ProduceText('title', sTitle);
End;


Procedure TFslHTMLFormatter.ProduceStylesheet(Const sFilename: String);
Begin
  Attributes.Match['rel'] := 'stylesheet';
  Attributes.Match['href'] := sFilename;
  Attributes.Match['type'] := 'text/css';

  ProduceTag('link');
End;


Procedure TFslHTMLFormatter.ProduceSymbol(Const aSymbol: TFslHTMLSymbol);
Begin
  Produce('&' + HTML_SYMBOL_VALUES[aSymbol] + ';');
End;


Procedure TFslHTMLFormatter.ProduceGreekSymbol(Const aSymbol: TFslHTMLGreekSymbol);
Begin
  Produce('&' + HTML_GREEK_SYMBOL_VALUES[aSymbol] + ';');
End;


Procedure TFslHTMLFormatter.ProduceMathSymbol(Const aSymbol: TFslHTMLMathSymbol);
Begin
  Produce('&' + HTML_MATH_SYMBOL_VALUES[aSymbol] + ';');
End;


Function TFslHTMLFormatter.GetStyle: String;
Begin
  Result := Attributes.Match['class'];
End;


Procedure TFslHTMLFormatter.SetStyle(Const Value: String);
Begin
  Attributes.Match['class'] := Value;
End;


Procedure TFslHTMLFormatter.ProduceHeading(Const sText: String; Const iLevel : Integer);
Begin
  ProduceText('h' + IntegerToString(iLevel), sText);
End;


Procedure TFslHTMLFormatter.ProduceLineBreak;
Begin
  ProduceTag('br');
End;


Procedure TFslHTMLFormatter.ProduceTableCellText(Const sText: String);
Begin
  ProduceText('td', sText);
End;


Procedure TFslHTMLFormatter.ProduceTableOpen;
Begin
  ProduceOpen('table');
End;


Procedure TFslHTMLFormatter.ProduceTableClose;
Begin
  ProduceClose('table');
End;


Procedure TFslHTMLFormatter.ProduceTableHeadOpen;
Begin
  ProduceOpen('th');
End;


Procedure TFslHTMLFormatter.ProduceTableHeadClose;
Begin
  ProduceClose('th');
End;


Procedure TFslHTMLFormatter.ProduceTableRowOpen;
Begin
  ProduceOpen('tr');
End;


Procedure TFslHTMLFormatter.ProduceTableRowClose;
Begin
  ProduceClose('tr');
End;


Procedure TFslHTMLFormatter.ProduceTableCellOpen;
Begin
  ProduceOpen('td');
End;


Procedure TFslHTMLFormatter.ProduceTableCellClose;
Begin
  ProduceClose('td');
End;


Procedure TFslHTMLFormatter.ProduceSpan(Const sText: String);
Begin
  ProduceOpen('span');
  ProduceEscaped(sText);
  ProduceClose('span');
End;


Procedure TFslHTMLFormatter.ProduceParagraph(Const sText: String);
Begin
  ProduceOpen('p');
  ProduceEscaped(sText);
  ProduceClose('p');
End;


Procedure TFslHTMLFormatter.ProduceInlineDocument(Const sDocument: String);
Var
  iPos : Integer;
  sContent : String;
Begin
  // Style.
  sContent := sDocument;

  iPos := Pos('<head', sContent);

  If iPos > 0 Then
  Begin
    Delete(sContent, 1, iPos - 1);

    iPos := Pos('</head>', sContent);

    If iPos > 0 Then
    Begin
      Delete(sContent, iPos + Length('</head>'), MaxInt);

      //only the header section should remain in sContent now, so extract the style
      //section from it if it exists
      iPos := Pos('<style', sContent);

      If iPos > 0 Then
      Begin
        Delete(sContent, 1, iPos + Length('<style'));

        iPos := Pos('>', sContent);

        If iPos > 0 Then
        Begin
          Delete(sContent, 1, iPos);

          iPos := Pos('</style>', sContent);

          If iPos > 0 Then
          Begin
            Delete(sContent, iPos, MaxInt);

            ProduceStyleOpen;
            Produce(sContent);
            ProduceStyleClose;
          End;
        End;
      End;
    End;
  End;

  // Body.
  sContent := sDocument;

  iPos := Pos('<body', sContent);

  If iPos > 0 Then
  Begin
    Delete(sContent, 1, iPos);

    iPos := Pos('>', sContent);

    If iPos > 0 Then
    Begin
      Delete(sContent, 1, iPos);

      iPos := Pos('</body>', sContent);

      If iPos > 0 Then
        Delete(sContent, iPos, Length(sContent));
    End;
  End;

  Produce(sContent);
End;


Procedure TFslHTMLFormatter.ProduceTableColumn;
Begin
  ProduceInlineTag('col');
End;


Procedure TFslHTMLFormatter.ProduceTableColumnGroupClose;
Begin
  ProduceClose('colgroup');
End;


Procedure TFslHTMLFormatter.ProduceTableColumnGroupOpen;
Begin
  ProduceOpen('colgroup');
End;


Procedure TFslHTMLFormatter.ApplyAttributeAlignCenter;
Begin
  Attributes.Match['align'] := 'center';
End;


Procedure TFslHTMLFormatter.ApplyAttributeAlignLeft;
Begin
  Attributes.Match['align'] := 'left';
End;


Procedure TFslHTMLFormatter.ApplyAttributeAlignRight;
Begin
  Attributes.Match['align'] := 'right';
End;


Procedure TFslHTMLFormatter.ApplyAttributeWidthPixel(Const iWidthPixel: Integer);
Begin
  Attributes.Match['width'] := IntegerToString(iWidthPixel) + 'px';
End;


Procedure TFslHTMLFormatter.ApplyAttributeHeightPixel(Const iHeightPixel: Integer);
Begin
  Attributes.Match['height'] := IntegerToString(iHeightPixel) + 'px';
End;


Procedure TFslHTMLFormatter.ApplyAttributeHeightPercentage(Const iHeightPercentage: Integer);
Begin
  Attributes.Match['height'] := IntegerToString(iHeightPercentage) + '%';
End;


Procedure TFslHTMLFormatter.ProduceImageReference(Const sReference: String; Const sAlternate : String = '');
Begin
  Attributes.Match['src'] := sReference;
  Attributes.Match['alt'] := sAlternate;

  ProduceInlineTag('img');
End;


Function TFslHTMLFormatter.GetProduceXHTML: Boolean;
Begin
  Result := FAdapter.ProduceXHTML;
End;


Procedure TFslHTMLFormatter.SetProduceXHTML(Const Value: Boolean);
Begin
  FAdapter.ProduceXHTML := True;
End;



function TFslHTMLFormatter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FAdapter.sizeInBytes);
end;

Procedure TFslHTMLAdapter.Read(Var Buffer; iCount: Integer);
Var
  iLoop : Cardinal;
  pBuffer : PChar;
  aType : Array[0..4] Of Char;
  iType : Integer;
Begin
  pBuffer := @Buffer;
  iLoop := 0;

  While (iLoop < iCount) Do
  Begin
    Inherited Read(pBuffer^, 1);

    If (pBuffer^ = '&') Then
    Begin
      iType := 0;

      // Read the next few bytes after the ampersand.
      While Not CharInSet(aType[iType], [';'] + setWhitespace) And (iType <= High(aType)) Do
      Begin
        Inherited Read(aType[iType], 1);

        Inc(iType);
      End;

      aType[iType] := #0;

      If StringEquals(aType, 'lt;', 3) Then
        pBuffer^ := '<'
      Else If StringEquals(aType, 'gt;', 3) Then
        pBuffer^ := '>'
      Else If StringEquals(aType, 'amp;', 4) Then
        pBuffer^ := '&'
      Else
      Begin
        // Preserve the read ampersand and replace all the other characters

        iType := 0;
        While (aType[iType] <> #0) Do
        Begin
          Inc(pBuffer);

          pBuffer^ := aType[iType];

          Inc(iType);
        End;
      End;
    End;

    Inc(pBuffer);
    Inc(iLoop);
  End;
End;


Procedure TFslHTMLAdapter.Write(Const Buffer; iCount: Integer);
Var
  pBuffer, pStart : PChar;
  iLoop           : Integer;
Begin
  pBuffer := @Buffer;
  pStart := pBuffer;

  For iLoop := 0 To Integer(iCount) - 1 Do
  Begin
    Case pBuffer^ Of
{
      #0..#31, #127..#255 :
      Begin
        Inherited Write('&#' + IntegerToString(Byte(pBuffer)^));

        Inc(pBuffer);
        pStart := pBuffer;
      End;
}
      '<' :
      Begin
        Inherited Write(pStart^, Cardinal(pBuffer) - Cardinal(pStart));
        Inherited Write('&lt;', 4);

        Inc(pBuffer);
        pStart := pBuffer;
      End;

      '>' :
      Begin
        Inherited Write(pStart^, Cardinal(pBuffer) - Cardinal(pStart));
        Inherited Write('&gt;', 4);

        Inc(pBuffer);
        pStart := pBuffer;
      End;

      '&' :
      Begin
        Inherited Write(pStart^, Cardinal(pBuffer) - Cardinal(pStart));
        Inherited Write('&amp;', 5);

        Inc(pBuffer);
        pStart := pBuffer;
      End;

      cEnter :
      Begin
        Inherited Write(pStart^, Cardinal(pBuffer) - Cardinal(pStart));
        If FProduceXHTML Then
          Inherited Write('<br />', 6)
        Else
          Inherited Write('<br>', 4);

        Inc(pBuffer);
        pStart := pBuffer;
      End;

      cFeed :
      Begin
        // Ignore Feed characters.

        Inc(pBuffer);
      End;
    Else
      Inc(pBuffer);
    End;
  End;

  Inherited Write(pStart^, Cardinal(pBuffer) - Cardinal(pStart));
End;

Initialization
  FillChar(gTable, SizeOf(gTable), 0);

  gTable['!'] := ahtExclamation;
  gTable['?'] := ahtQuery;
  gTable['='] := ahtEquals;
  gTable['/'] := ahtForwardSlash;
  gTable['\'] := ahtBackwardSlash;
  gTable['.'] := ahtPeriod;
  gTable[','] := ahtComma;
  gTable[':'] := ahtColon;
  gTable[';'] := ahtSemiColon;
  gTable['_'] := ahtUnderscore;
  gTable['-'] := ahtHyphen;

  gTable['`'] := ahtBackQuote;
  gTable[''''] := ahtSingleQuote;
  gTable['"'] := ahtDoubleQuote;

  gTable['~'] := ahtTilde;
  gTable['@'] := ahtAt;
  gTable['#'] := ahtHash;
  gTable['$'] := ahtDollar;
  gTable['%'] := ahtPercentage;
  gTable['^'] := ahtCaret;
  gTable['&'] := ahtAmpersand;
  gTable['*'] := ahtAsterisk;
  gTable['('] := ahtOpenRound;
  gTable[')'] := ahtCloseRound;
  gTable['+'] := ahtPlus;
  gTable['['] := ahtOpenSquare;
  gTable[']'] := ahtCloseSquare;
  gTable['{'] := ahtOpenBrace;
  gTable['}'] := ahtCloseBrace;
  gTable['|'] := ahtPipe;
End.
