Unit cda_narrative;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)

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

{$I fhir.inc}

Interface

uses
  fsl_base, fsl_collections, fsl_utilities,
  cda_base, cda_types;

Type
  {
    Frame - kind of borders - that applies to a table
  }
  TsnFrame = (fnull, fvoid, above, below, hsides, lhs, rhs, vsides, box, border);

  {
    How Rules (lines) apply to table elements
  }
  TsnRules = (rnull, none, groups, rows, cols, all);

  {
    Alignment of paragraph inside a table cell.
  }
  TsnAlign = (anull, left, center, right, justify, achar);

  {
    Vertical alignment of content inside table cells
  }
  TsnVAlign = (vnull, top, middle, bottom, baseline);

  {
    Scope of a table cell
  }
  TsnCellScope = (cnull1, row, col, rowgroup, colgroup);

  {
    Revised status options for a Content element
  }
  TsnRevised = (rvnull, insert, delete);

  {
    Type of list
  }
  TsnListType = (lnull, ordered, unordered);


Const
  CODES_TsnFrame : array [TsnFrame] of String = ('', 'void', 'above', 'below', 'hsides', 'lhs', 'rhs', 'vsides', 'box', 'border');
  CODES_TsnRules : array [TsnRules] of String = ('', 'none', 'groups', 'rows', 'cols', 'all');
  CODES_TsnAlign : array [TsnAlign] of String = ('', 'left', 'center', 'right', 'justify', 'char');
  CODES_TsnVAlign : array [TsnVAlign] of String = ('', 'top', 'middle', 'bottom', 'baseline');
  CODES_TsnCellScope : array [TsnCellScope] of String = ('', 'row', 'col', 'rowgroup', 'colgroup');
  CODES_TsnRevised : array [TsnRevised] of String = ('', 'insert', 'delete');
  CODES_TsnListType : array [TsnListType] of String = ('', 'ordered', 'unordered');

Type
  TsnTable = class;
  TsnCaption = class;
  TsnLinkHtml = class;
  TsnContent = class;
  TsnRenderMultiMedia = class;
  TsnList = class;
  TsnParagraph = class;
  TsnCMTitle = class;
  TsnFootNote = class;
  TsnFootNoteRef = class;

  TsnCMGeneralList = class;
  TsnCMTitleList = class;
  TsnCMFootnotesList = class;
  TsnCMInlineList = class;
  TsnCMContentList = class;
  TsnItemList = class;
  TsnColList = class;
  TsnTRowPartList = class;
  TsnTRowList = class;
  TsnColGroupList = class;
  TsnTRowGroupList = class;

  TNarrativeCollection = class (Tv3BaseList);

  TNarrativeBase = Class (Tv3Base)
  private
    function GetAsText : String;
  protected
    procedure GetText(builder : TFslStringBuilder); virtual;
    Function CDAClassTypeV : TCDAClassType; Override;
  public
    property AsText : String read GetAsText;
  End;

  TsnBase = class (TNarrativeBase)
  private
    Flanguage: String;
    FstyleCode: TFslStringList;
    FID: String;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnBase; Overload;
    Function Clone(parent : Tv3Base) : TsnBase; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    Property ID : String read FID write FID;
    Property language : String read Flanguage write Flanguage;
    Property styleCode: TFslStringList read FstyleCode;
  End;


  TsnText = class (Tv3ANY)
  private
    FmediaType: String;
    FID: String;
    Flanguage: String;
    FstyleCode: TFslStringList;
    Fparts: TsnCMGeneralList;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnText; Overload;
    Function Clone(parent : Tv3Base) : TsnText; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  Published
    {
      Unique Identity of this element within a document - can be used as a URL target
    }
    Property ID : String read FID write FID;
    {
      Language of the element. Within a document, the language applies to all the contained elements unless some other language is specifically identified
    }
    Property language : String read Flanguage write Flanguage;
    {
      If populated, the value of this attribute SHALL be taken from one of these values listed in StyleCode or a valid local extension.

      Local extensions to the styleType enumeration must follow the following convention:
      [x][A-Za-z][A-Za-z0-9]* (first character is "x", second character is an upper or
      lower case A-Z, remaining characters are any combination of upper and lower case
      letters or numbers)
    }
    Property styleCode: TFslStringList read FstyleCode;
    {
      A series of parts that capture the content of the narrative text
    }
    Property parts : TsnCMGeneralList read Fparts;  { anonymous="true" }
    {
      mediaType is fixed to text/x-hl7-text+xml
    }
    Property mediaType : String read FmediaType; // = 'text/x-hl7-text+xml'
  End;

  TsnTitle = class (Tv3ANY)
  private
    Flanguage: String;
    FID: String;
    FmediaType: String;
    FstyleCode: TFslStringList;
    Fparts: TsnCMTitleList;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnTitle; Overload;
    Function Clone(parent : Tv3Base) : TsnTitle; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  Published
    {
      Unique Identity of this element within a document - can be used as a URL target
    }
    Property ID : String read FID write FID;
    {
      Language of the element. Within a document, the language applies to all the contained elements unless some other language is specifically identified
    }
    Property language : String read Flanguage write Flanguage;
    {
      If populated, the value of this attribute SHALL be taken from one of these values listed in StyleCode or a valid local extension.

      Local extensions to the styleType enumeration must follow the following convention:
      [x][A-Za-z][A-Za-z0-9]* (first character is "x", second character is an upper or
      lower case A-Z, remaining characters are any combination of upper and lower case
      letters or numbers)
    }
    Property styleCode: TFslStringList read FstyleCode;
    {
      A series of parts that capture the content of the narrative title
    }
    Property parts : TsnCMTitleList read Fparts;  { anonymous="true" }
    {
      mediaType is fixed to text/x-hl7-text+xml
    }
    Property mediaType : String read FmediaType; // = 'text/x-hl7-text+xml'
  end;

  TsnString = class (TNarrativeBase)
  private
    FText : String;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
  public
    constructor Create; Overload; Override;
    constructor Create(value : String); Overload;
    destructor Destroy; Override;
    function Link : TsnString; Overload;
    Function Clone(parent : Tv3Base) : TsnString; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    {
      Plain text string.
    }
    property value : String read Ftext write Ftext; {xml="text"}
  end;

  TsnBr = class (TNarrativeBase)
  Protected
    procedure GetText(builder : TFslStringBuilder); override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnBr; Overload;
    Function Clone(parent : Tv3Base) : TsnBr; Overload;
    Function RIMClassNameV : String; Override;
  end;


  TsnCMFootnotes = class (TNarrativeBase)
  private
    Ftext: TsnString;
    Fentity : TsnString;
    Ffootnote: TsnFootNote;
    FfootnoteRef: TsnFootNoteRef;
    procedure SetFootnote(const Value: TsnFootNote);
    procedure SetfootnoteRef(const Value: TsnFootNoteRef);
    procedure Setentity(const Value: TsnString);
    procedure Settext(const Value: TsnString);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnCMFootnotes; Overload;
    Function Clone(parent : Tv3Base) : TsnCMFootnotes; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    {
      Plain text string.
    }
    property text : TsnString read Ftext write Settext; {xml="text"}
    {
      contents of an ampersand entity (i.e. "#160" will be encoded as &#160;).
    }
    property entity : TsnString read Fentity write Setentity; {xml="text"}
    property footnote : TsnFootNote read Ffootnote write SetFootnote;
    property footnoteRef : TsnFootNoteRef read FfootnoteRef write SetfootnoteRef;
  end;


  TsnCMInline = class (TsnCMFootnotes)
  private
    Fsub: TsnString;
    Fsup: TsnString;
    FlinkHtml: TsnLinkHtml;
    procedure SetlinkHtml(const Value: TsnLinkHtml);
    procedure Setsub(const Value: TsnString);
    procedure Setsup(const Value: TsnString);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnCMInline; Overload;
    Function Clone(parent : Tv3Base) : TsnCMInline; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    property linkHtml : TsnLinkHtml read FlinkHtml write SetlinkHtml;   // = nil
    property sub : TsnString read Fsub write Setsub;
    property sup : TsnString read Fsup write Setsup;
  end;

  TsnCMTitle = class (TNarrativeBase)
  private
    Fbr: TsnBr;
    Ftext: TsnString;
    FfootnoteRef: TsnString;
    Fsup: TsnString;
    Fsub: TsnString;
    Fcontent: TsnCMTitle;
    Ffootnote: TsnCMTitleList;
    FlinkHtml: TsnLinkHtml;
    Fentity: TsnString;
    procedure Setcontent(const Value: TsnCMTitle);
    procedure SetlinkHtml(const Value: TsnLinkHtml);
    procedure SetfootNote(const Value: TsnCMTitleList);
    procedure SetBr(const Value: TsnBr);
    procedure Setentity(const Value: TsnString);
    procedure Setsub(const Value: TsnString);
    procedure Setsup(const Value: TsnString);
    procedure Settext(const Value: TsnString);
    procedure SetfootnoteRef(const Value: TsnString);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnCMTitle; Overload;
    Function Clone(parent : Tv3Base) : TsnCMTitle; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    {
      Plain text string.
    }
    property text : TsnString read Ftext write Settext;
    {
      contents of an ampersand entity (i.e. "#160" will be encoded as &#160;).
    }
    property entity : TsnString read Fentity write Setentity; {xml="text"}
    property footnote : TsnCMTitleList read Ffootnote write SetfootNote;
    {
      A reference to a footnote
    }
    property footnoteRef : TsnString read FfootnoteRef write SetfootnoteRef;
    {
      A hard line break
    }
    property br : TsnBr read Fbr write SetBr;
    property linkHtml : TsnLinkHtml read FlinkHtml write SetlinkHtml; // = nil
    {
    }
    property sub : TsnString read Fsub write Setsub;
    {
    }
    property sup : TsnString read Fsup write Setsup;
    property content : TsnCMTitle read Fcontent write Setcontent;
  end;

  TsnCMContent = class (TsnCMInline)
  private
    Fbr: TsnBr;
    Fcontent: TsnContent;
    FrenderMultiMedia: TsnRenderMultiMedia;
    procedure Setcontent(const Value: TsnContent);
    procedure SetrenderMultiMedia(const Value: TsnRenderMultiMedia);
    procedure SetBr(const Value: TsnBr);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnCMContent; Overload;
    Function Clone(parent : Tv3Base) : TsnCMContent; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    property content : TsnContent read Fcontent write Setcontent;
    property br : TsnBr read Fbr write SetBr;
    property renderMultiMedia : TsnRenderMultiMedia read FrenderMultiMedia write SetrenderMultiMedia;
  end;

  TsnCMGeneral = class (TsnCMContent)
  private
    Flist: TsnList;
    Fparagraph: TsnParagraph;
    Ftable: TsnTable;
    procedure Setlist(const Value: TsnList);
    procedure Setparagraph(const Value: TsnParagraph);
    procedure Settable(const Value: TsnTable);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnCMGeneral; Overload;
    Function Clone(parent : Tv3Base) : TsnCMGeneral; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    property paragraph : TsnParagraph read Fparagraph write Setparagraph;
    property list : TsnList read Flist write Setlist;
    property table : TsnTable read Ftable write Settable;
  end;


  TsnRenderMultiMedia = class (TsnBase)
  private
    FreferencedObject: TFslStringList;
    Fcaption: TsnCaption;
    procedure Setcaption(const Value: TsnCaption);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnRenderMultiMedia; Overload;
    Function Clone(parent : Tv3Base) : TsnRenderMultiMedia; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    property caption : TsnCaption read Fcaption write Setcaption;
    property referencedObject : TFslStringList {use="required"} read FreferencedObject;
  end;

  TsnLinkHtml = class (TsnBase)
  private
    Ftitle: String;
    Fname: String;
    Frel: String;
    Frev: String;
    Fhref: String;
    Fparts: TsnCMFootnotesList;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnLinkHtml; Overload;
    Function Clone(parent : Tv3Base) : TsnLinkHtml; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    {
      The linkHtml.name attribute is deprecated, because attributes of type XML ID
      provide an alternative and more consistent target for referencing. Following
      the conventions of HTML, an internal link is prefaced with the pound sign, as
      shown in the following example.
    }
    property name : String read Fname write Fname;
    property href : String read Fhref write Fhref;
    property rel : String read Frel write Frel;
    property rev : String read Frev write Frev;
    property title : String read Ftitle write Ftitle;
    {
      A series of parts that capture the content of the Link
    }
    property parts : TsnCMFootnotesList read Fparts; { anonymous="true" }
  end;

  TsnFootnote = class (TsnBase)
  private
    Fparts: TsnCMGeneralList;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnFootnote; Overload;
    Function Clone(parent : Tv3Base) : TsnFootnote; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    {
      A series of parts that capture the content of the Footnote
    }
    property parts : TsnCMGeneralList read Fparts;    { anonymous="true", "Prohibited=footnote,footnoteRef" }
  end;

  TsnTitleFootnote = class (TsnBase)
  private
    Fparts: TsnCMTitleList;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnTitleFootnote; Overload;
    Function Clone(parent : Tv3Base) : TsnTitleFootnote; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    {
      A series of parts that capture the content of the Footnote
    }
    property parts : TsnCMTitleList read Fparts;    { anonymous="true" }
  end;

  TsnFootnoteRef = class (TsnBase)
  private
    FIDREF: String;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnFootnoteRef; Overload;
    Function Clone(parent : Tv3Base) : TsnFootnoteRef; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    property IDREF : String read FIDREF write FIDREF; {use="required"}       // can this be by reference - no, because the scope is not nailed down to this instance. Better to leave it as it is;
  end;

  TsnCaption = class (TsnBase)
  private
    Fparts: TsnCMInlineList;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnCaption; Overload;
    Function Clone(parent : Tv3Base) : TsnCaption; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    {
      A series of parts that capture the content of the Caption
    }
    property parts : TsnCMInlineList read Fparts;    { anonymous="true" }
  end;

  TsnContent = class (TsnBase)
  private
    Fparts: TsnCMContentList;
    Frevised: TsnRevised;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnContent; Overload;
    Function Clone(parent : Tv3Base) : TsnContent; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    property revised : TsnRevised read Frevised write Frevised;
    {
      A series of parts that capture the content of the Content
    }
    property parts : TsnCMContentList read Fparts;    { anonymous="true" }
  end;

  TsnCaptioned = class (TsnBase)
  private
    Fcaption: TsnCaption;
    procedure Setcaption(const Value: TsnCaption);  { Mixed }
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnCaptioned; Overload;
    Function Clone(parent : Tv3Base) : TsnCaptioned; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    property caption : TsnCaption read Fcaption write Setcaption;
  end;

  TsnParagraph = class (TsnCaptioned)
  private
    Fparts: TsnCMContentList;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnParagraph; Overload;
    Function Clone(parent : Tv3Base) : TsnParagraph; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    {
      A series of parts that capture the content of the paragraph
    }
    property parts : TsnCMContentList read Fparts;    { anonymous="true" }
  end;

  TsnItem = class (TsnCaptioned)
  private
    Fparts: TsnCMGeneralList;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnItem; Overload;
    Function Clone(parent : Tv3Base) : TsnItem; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    {
      A series of parts that capture the content of the item
    }
    property parts : TsnCMGeneralList read Fparts;    { anonymous="true" }
  end;

  TsnList = class (TsnCaptioned)
  private
    Fitem: TsnItemList;
    FlistType: TsnListType;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnList; Overload;
    Function Clone(parent : Tv3Base) : TsnList; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    property listType : TsnListType read FlistType write FlistType;
    {
      A series of items that capture the content of the list
    }
    property item : TsnItemList read Fitem;
  end;

  TsnTableItem = class (TsnBase)
  private
    Fchar: String;
    Falign: TsnAlign;
    Fcharoff: String;
    Fvalign: TsnVAlign;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnTableItem; Overload;
    Function Clone(parent : Tv3Base) : TsnTableItem; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    property align : TsnAlign read Falign write Falign;
    property char_ : String read Fchar write Fchar;
    property charoff : String read Fcharoff write Fcharoff;
    property valign : TsnVAlign read Fvalign write Fvalign;
  end;

  TsnColItem = class (TsnTableItem)
  private
    Fspan: Integer;
    Fwidth: String;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnColItem; Overload;
    Function Clone(parent : Tv3Base) : TsnColItem; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    property span : Integer read Fspan write Fspan; // = 1
    property width : String read Fwidth write Fwidth;
  end;

  TsnCol = class (TsnColItem)
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnCol; Overload;
    Function Clone(parent : Tv3Base) : TsnCol; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  end;

  TsnColGroup = class (TsnColItem)
  private
    Fcol: TsnColList;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnColGroup; Overload;
    Function Clone(parent : Tv3Base) : TsnColGroup; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    {
      A series of parts that capture the conten of the ColGroup
    }
    property col : TsnColList read Fcol;
  end;


  TsnTCell = class (TsnTableItem)
  private
    Frowspan: Integer;
    Fcolspan: Integer;
    Fabbr: String;
    Faxis: String;
    Fheaders: TFslStringList;
    Fscope: TsnCellScope;
    Fparts: TsnCMGeneralList;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnTCell; Overload;
    Function Clone(parent : Tv3Base) : TsnTCell; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    property abbr : String read Fabbr write Fabbr;
    property axis : String read Faxis write Faxis;
    property headers : TFslStringList read Fheaders;
    property scope : TsnCellScope read Fscope write Fscope;
    property rowspan : Integer read Frowspan write Frowspan; // = 1;
    property colspan : Integer read Fcolspan write Fcolspan; // = 1;
    {
      A series of parts that capture the content of the cell
    }
    property parts : TsnCMGeneralList read Fparts;    { anonymous="true" }
  end;

  TsnTRowPart {Choice} = class (TNarrativeBase)
  private
    Fth: TsnTCell;
    Ftd: TsnTCell;
    procedure Settd(const Value: TsnTCell);
    procedure Setth(const Value: TsnTCell);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnTRowPart; Overload;
    Function Clone(parent : Tv3Base) : TsnTRowPart; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    property th : TsnTCell read Fth write Setth;
    property td : TsnTCell read Ftd write Settd;
  end;

  TsnTRow = class (TsnTableItem)
  private
    Fparts: TsnTRowPartList;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnTRow; Overload;
    Function Clone(parent : Tv3Base) : TsnTRow; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    {
      A series of parts that capture the content of the row
    }
    property parts : TsnTRowPartList read Fparts;    { anonymous="true" }
  end;


  TsnTRowGroup = class (TsnTableItem)
  private
    Ftr: TsnTRowList;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnTRowGroup; Overload;
    Function Clone(parent : Tv3Base) : TsnTRowGroup; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    property tr : TsnTRowList read Ftr;
  end;

  TsnTable = class (TsnCaptioned)
  private
    Fsummary: String;
    Fframe: TsnFrame;
    Fcellpadding: String;
    Fcellspacing: String;
    Fborder: String;
    Fwidth: String;
    Frules: TsnRules;
    Ftfoot: TsnTRowGroup;
    Fthead: TsnTRowGroup;
    Fcolgroup: TsnColGroupList;
    Fcol: TsnColList;
    Ftbody: TsnTRowGroupList;
    procedure Settfoot(const Value: TsnTRowGroup);
    procedure Setthead(const Value: TsnTRowGroup);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    procedure GetText(builder : TFslStringBuilder); override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TsnTable; Overload;
    Function Clone(parent : Tv3Base) : TsnTable; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
  published
    {
      A series of cols that define columns in the table
    }
    property col: TsnColList read Fcol;
    {
      A series of column groups that define columns in the table
    }
    property colgroup: TsnColGroupList read Fcolgroup;
    {
      The header of the table
    }
    property thead: TsnTRowGroup read Fthead write Setthead;
    {
      The footer of the table
    }
    property tfoot: TsnTRowGroup read Ftfoot write Settfoot;
    {
      The body of the table
    }
    property tbody: TsnTRowGroupList read Ftbody;
    property summary: String read Fsummary write Fsummary;
    property width: String read Fwidth write Fwidth;
    property border : String read Fborder write Fborder;
    property frame : TsnFrame read Fframe write Fframe;
    property rules : TsnRules read Frules write Frules;
    property cellspacing : String read Fcellspacing write Fcellspacing;
    property cellpadding: String read Fcellpadding write Fcellpadding;
  end;

  TsnCMGeneralList = class (TNarrativeCollection)
  Private
    Function GetTsnCMGenerals(iIndex : Integer) : TsnCMGeneral;
    Procedure SetTsnCMGenerals(iIndex : Integer; value : TsnCMGeneral);
  protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TsnCMGeneralList; Overload;
    Function Clone(parent : Tv3Base) : TsnCMGeneralList; Overload;
    Property TsnCMGenerals[iIndex : Integer] : TsnCMGeneral read GetTsnCMGenerals write SetTsnCMGenerals; Default;
    {
      Append a CMGeneral at the end of the list
    }
    Function Append : TsnCMGeneral;
    {
      Add an already existing CMGeneral to the end of the list
    }
    Procedure AddItem(value : TsnCMGeneral);
    {
      See if an item is already in the list. Returns -1 if not in the list
    }
    Function IndexOf(Value : TsnCMGeneral) : Integer;
    {
      Insert a existing CMGeneral before the specified index (first element at 0)
    }
    Procedure InsertItem(iIndex : Integer; value : TsnCMGeneral);
    {
      Insert a existing CMGeneral before the specified index (first element at 0)
    }
    Function Insert(iIndex : Integer) : TsnCMGeneral;
    {
      Get the CMGeneral at iIndex. The first item is index 0)
    }
    Function Item(iIndex : Integer) : TsnCMGeneral;
    {
      Set the CMGeneral at iIndex. The first item is index 0)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TsnCMGeneral);
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
  End;

  TsnCMTitleList = class (TNarrativeCollection)
  Private
    Function GetTsnCMTitles(iIndex : Integer) : TsnCMTitle;
    Procedure SetTsnCMTitles(iIndex : Integer; value : TsnCMTitle);
  protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TsnCMTitleList; Overload;
    Function Clone(parent : Tv3Base) : TsnCMTitleList; Overload;
    Property TsnCMTitles[iIndex : Integer] : TsnCMTitle read GetTsnCMTitles write SetTsnCMTitles; Default;
    {
      Append a CMTitle at the end of the list
    }
    Function Append : TsnCMTitle;
    {
      Add an already existing CMTitle to the end of the list
    }
    Procedure AddItem(value : TsnCMTitle);
    {
      See if an item is already in the list. Returns -1 if not in the list
    }
    Function IndexOf(Value : TsnCMTitle) : Integer;
    {
      Insert a existing CMTitle before the specified index (first element at 0)
    }
    Procedure InsertItem(iIndex : Integer; value : TsnCMTitle);
    {
      Insert a existing CMTitle before the specified index (first element at 0)
    }
    Function Insert(iIndex : Integer) : TsnCMTitle;
    {
      Get the CMTitle at iIndex. The first item is index 0)
    }
    Function Item(iIndex : Integer) : TsnCMTitle;
    {
      Set the CMTitle at iIndex. The first item is index 0)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TsnCMTitle);
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
  End;

  TsnCMFootnotesList = class (TNarrativeCollection)
  Private
    Function GetTsnCMFootnotess(iIndex : Integer) : TsnCMFootnotes;
    Procedure SetTsnCMFootnotess(iIndex : Integer; value : TsnCMFootnotes);
  protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TsnCMFootnotesList; Overload;
    Function Clone(parent : Tv3Base) : TsnCMFootnotesList; Overload;
    Property TsnCMFootnotess[iIndex : Integer] : TsnCMFootnotes read GetTsnCMFootnotess write SetTsnCMFootnotess; Default;
    {
      Append a CMFootnotes at the end of the list
    }
    Function Append : TsnCMFootnotes;
    {
      Add an already existing CMFootnotes to the end of the list
    }
    Procedure AddItem(value : TsnCMFootnotes);
    {
      See if an item is already in the list. Returns -1 if not in the list
    }
    Function IndexOf(Value : TsnCMFootnotes) : Integer;
    {
      Insert a existing CMFootnotes before the specified index (first element at 0)
    }
    Procedure InsertItem(iIndex : Integer; value : TsnCMFootnotes);
    {
      Insert a existing CMFootnotes before the specified index (first element at 0)
    }
    Function Insert(iIndex : Integer) : TsnCMFootnotes;
    {
      Get the CMFootnotes at iIndex. The first item is index 0)
    }
    Function Item(iIndex : Integer) : TsnCMFootnotes;
    {
      Set the CMFootnotes at iIndex. The first item is index 0)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TsnCMFootnotes);
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
  End;

  TsnCMInlineList = class (TNarrativeCollection)
  Private
    Function GetTsnCMInlines(iIndex : Integer) : TsnCMInline;
    Procedure SetTsnCMInlines(iIndex : Integer; value : TsnCMInline);
  protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TsnCMInlineList; Overload;
    Function Clone(parent : Tv3Base) : TsnCMInlineList; Overload;
    Property TsnCMInlines[iIndex : Integer] : TsnCMInline read GetTsnCMInlines write SetTsnCMInlines; Default;
    {
      Append a CMInline at the end of the list
    }
    Function Append : TsnCMInline;
    {
      Add an already existing CMInline to the end of the list
    }
    Procedure AddItem(value : TsnCMInline);
    {
      See if an item is already in the list. Returns -1 if not in the list
    }
    Function IndexOf(Value : TsnCMInline) : Integer;
    {
      Insert a existing CMInline before the specified index (first element at 0)
    }
    Procedure InsertItem(iIndex : Integer; value : TsnCMInline);
    {
      Insert a existing CMInline before the specified index (first element at 0)
    }
    Function Insert(iIndex : Integer) : TsnCMInline;
    {
      Get the CMInline at iIndex. The first item is index 0)
    }
    Function Item(iIndex : Integer) : TsnCMInline;
    {
      Set the CMInline at iIndex. The first item is index 0)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TsnCMInline);
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
  End;

  TsnCMContentList = class (TNarrativeCollection)
  Private
    Function GetTsnCMContents(iIndex : Integer) : TsnCMContent;
    Procedure SetTsnCMContents(iIndex : Integer; value : TsnCMContent);
  protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TsnCMContentList; Overload;
    Function Clone(parent : Tv3Base) : TsnCMContentList; Overload;
    Property TsnCMContents[iIndex : Integer] : TsnCMContent read GetTsnCMContents write SetTsnCMContents; Default;
    {
      Append a CMContent at the end of the list
    }
    Function Append : TsnCMContent;
    {
      Add an already existing CMContent to the end of the list
    }
    Procedure AddItem(value : TsnCMContent);
    {
      See if an item is already in the list. Returns -1 if not in the list
    }
    Function IndexOf(Value : TsnCMContent) : Integer;
    {
      Insert a existing CMContent before the specified index (first element at 0)
    }
    Procedure InsertItem(iIndex : Integer; value : TsnCMContent);
    {
      Insert a existing CMContent before the specified index (first element at 0)
    }
    Function Insert(iIndex : Integer) : TsnCMContent;
    {
      Get the CMContent at iIndex. The first item is index 0)
    }
    Function Item(iIndex : Integer) : TsnCMContent;
    {
      Set the CMContent at iIndex. The first item is index 0)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TsnCMContent);
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
  End;

  TsnItemList = class (TNarrativeCollection)
  Private
    Function GetTsnItems(iIndex : Integer) : TsnItem;
    Procedure SetTsnItems(iIndex : Integer; value : TsnItem);
  protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TsnItemList; Overload;
    Function Clone(parent : Tv3Base) : TsnItemList; Overload;
    Property TsnItems[iIndex : Integer] : TsnItem read GetTsnItems write SetTsnItems; Default;
    {
      Append a Item at the end of the list
    }
    Function Append : TsnItem;
    {
      Add an already existing Item to the end of the list
    }
    Procedure AddItem(value : TsnItem);
    {
      See if an item is already in the list. Returns -1 if not in the list
    }
    Function IndexOf(Value : TsnItem) : Integer;
    {
      Insert a existing Item before the specified index (first element at 0)
    }
    Procedure InsertItem(iIndex : Integer; value : TsnItem);
    {
      Insert a existing Item before the specified index (first element at 0)
    }
    Function Insert(iIndex : Integer) : TsnItem;
    {
      Get the Item at iIndex. The first item is index 0)
    }
    Function Item(iIndex : Integer) : TsnItem;
    {
      Set the Item at iIndex. The first item is index 0)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TsnItem);
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
  End;

  TsnColList = class (TNarrativeCollection)
  Private
    Function GetTsnCols(iIndex : Integer) : TsnCol;
    Procedure SetTsnCols(iIndex : Integer; value : TsnCol);
  protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TsnColList; Overload;
    Function Clone(parent : Tv3Base) : TsnColList; Overload;
    Property TsnCols[iIndex : Integer] : TsnCol read GetTsnCols write SetTsnCols; Default;
    {
      Append a Col at the end of the list
    }
    Function Append : TsnCol;
    {
      Add an already existing Col to the end of the list
    }
    Procedure AddItem(value : TsnCol);
    {
      See if an item is already in the list. Returns -1 if not in the list
    }
    Function IndexOf(Value : TsnCol) : Integer;
    {
      Insert a existing Col before the specified index (first element at 0)
    }
    Procedure InsertItem(iIndex : Integer; value : TsnCol);
    {
      Insert a existing Col before the specified index (first element at 0)
    }
    Function Insert(iIndex : Integer) : TsnCol;
    {
      Get the Col at iIndex. The first item is index 0)
    }
    Function Item(iIndex : Integer) : TsnCol;
    {
      Set the Col at iIndex. The first item is index 0)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TsnCol);
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
  End;

  TsnTRowPartList = class (TNarrativeCollection)
  Private
    Function GetTsnTRowParts(iIndex : Integer) : TsnTRowPart;
    Procedure SetTsnTRowParts(iIndex : Integer; value : TsnTRowPart);
  protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TsnTRowPartList; Overload;
    Function Clone(parent : Tv3Base) : TsnTRowPartList; Overload;
    Property TsnTRowParts[iIndex : Integer] : TsnTRowPart read GetTsnTRowParts write SetTsnTRowParts; Default;
    {
      Append a TRowPart at the end of the list
    }
    Function Append : TsnTRowPart;
    {
      Add an already existing TRowPart to the end of the list
    }
    Procedure AddItem(value : TsnTRowPart);
    {
      See if an item is already in the list. Returns -1 if not in the list
    }
    Function IndexOf(Value : TsnTRowPart) : Integer;
    {
      Insert a existing TRowPart before the specified index (first element at 0)
    }
    Procedure InsertItem(iIndex : Integer; value : TsnTRowPart);
    {
      Insert a existing TRowPart before the specified index (first element at 0)
    }
    Function Insert(iIndex : Integer) : TsnTRowPart;
    {
      Get the TRowPart at iIndex. The first item is index 0)
    }
    Function Item(iIndex : Integer) : TsnTRowPart;
    {
      Set the TRowPart at iIndex. The first item is index 0)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TsnTRowPart);
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
  End;

  TsnTRowList = class (TNarrativeCollection)
  Private
    Function GetTsnTRows(iIndex : Integer) : TsnTRow;
    Procedure SetTsnTRows(iIndex : Integer; value : TsnTRow);
  protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TsnTRowList; Overload;
    Function Clone(parent : Tv3Base) : TsnTRowList; Overload;
    Property TsnTRows[iIndex : Integer] : TsnTRow read GetTsnTRows write SetTsnTRows; Default;
    {
      Append a TRow at the end of the list
    }
    Function Append : TsnTRow;
    {
      Add an already existing TRow to the end of the list
    }
    Procedure AddItem(value : TsnTRow);
    {
      See if an item is already in the list. Returns -1 if not in the list
    }
    Function IndexOf(Value : TsnTRow) : Integer;
    {
      Insert a existing TRow before the specified index (first element at 0)
    }
    Procedure InsertItem(iIndex : Integer; value : TsnTRow);
    {
      Insert a existing TRow before the specified index (first element at 0)
    }
    Function Insert(iIndex : Integer) : TsnTRow;
    {
      Get the TRow at iIndex. The first item is index 0)
    }
    Function Item(iIndex : Integer) : TsnTRow;
    {
      Set the TRow at iIndex. The first item is index 0)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TsnTRow);
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
  End;

  TsnColGroupList = class (TNarrativeCollection)
  Private
    Function GetTsnColGroups(iIndex : Integer) : TsnColGroup;
    Procedure SetTsnColGroups(iIndex : Integer; value : TsnColGroup);
  protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TsnColGroupList; Overload;
    Function Clone(parent : Tv3Base) : TsnColGroupList; Overload;
    Property TsnColGroups[iIndex : Integer] : TsnColGroup read GetTsnColGroups write SetTsnColGroups; Default;
    {
      Append a ColGroup at the end of the list
    }
    Function Append : TsnColGroup;
    {
      Add an already existing ColGroup to the end of the list
    }
    Procedure AddItem(value : TsnColGroup);
    {
      See if an item is already in the list. Returns -1 if not in the list
    }
    Function IndexOf(Value : TsnColGroup) : Integer;
    {
      Insert a existing ColGroup before the specified index (first element at 0)
    }
    Procedure InsertItem(iIndex : Integer; value : TsnColGroup);
    {
      Insert a existing ColGroup before the specified index (first element at 0)
    }
    Function Insert(iIndex : Integer) : TsnColGroup;
    {
      Get the ColGroup at iIndex. The first item is index 0)
    }
    Function Item(iIndex : Integer) : TsnColGroup;
    {
      Set the ColGroup at iIndex. The first item is index 0)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TsnColGroup);
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
  End;

  TsnTRowGroupList = class (TNarrativeCollection)
  Private
    Function GetTsnTRowGroups(iIndex : Integer) : TsnTRowGroup;
    Procedure SetTsnTRowGroups(iIndex : Integer; value : TsnTRowGroup);
  protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TsnTRowGroupList; Overload;
    Function Clone(parent : Tv3Base) : TsnTRowGroupList; Overload;
    Property TsnTRowGroups[iIndex : Integer] : TsnTRowGroup read GetTsnTRowGroups write SetTsnTRowGroups; Default;
    {
      Append a TRowGroup at the end of the list
    }
    Function Append : TsnTRowGroup;
    {
      Add an already existing TRowGroup to the end of the list
    }
    Procedure AddItem(value : TsnTRowGroup);
    {
      See if an item is already in the list. Returns -1 if not in the list
    }
    Function IndexOf(Value : TsnTRowGroup) : Integer;
    {
      Insert a existing TRowGroup before the specified index (first element at 0)
    }
    Procedure InsertItem(iIndex : Integer; value : TsnTRowGroup);
    {
      Insert a existing TRowGroup before the specified index (first element at 0)
    }
    Function Insert(iIndex : Integer) : TsnTRowGroup;
    {
      Get the TRowGroup at iIndex. The first item is index 0)
    }
    Function Item(iIndex : Integer) : TsnTRowGroup;
    {
      Set the TRowGroup at iIndex. The first item is index 0)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TsnTRowGroup);
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
  End;


Implementation


{ TsnBase }

Function TsnBase.RIMClassNameV : String;
Begin
  Result := 'Base';
End;

procedure TsnBase.Assign(oSource: TFslObject);
begin
  inherited;
  ID := TsnBase(oSource).ID;
  language := TsnBase(oSource).language;
  styleCode.Assign(TsnBase(oSource).styleCode);
end;

function TsnBase.Clone(parent : Tv3Base): TsnBase;
begin
  Result := TsnBase(inherited Clone(parent));
end;

constructor TsnBase.Create;
begin
  inherited;
  FstyleCode := TFslStringList.Create;
end;


destructor TsnBase.Destroy;
begin
  FstyleCode.Free; //TFslStringList
  inherited;
end;

function TsnBase.Link: TsnBase;
begin
  Result := TsnBase(Inherited Link);
end;

procedure TsnBase.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'language', Flanguage));
  oList.Add(Tv3PropertyDefinition.CreateStrings(self, true, 'styleCode', FStyleCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'ID', FId));
end;

procedure TsnBase.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'language' Then
    Flanguage := aValue.AsString
  else if aValue.Name = 'styleCode' Then
    styleCode.Assign(aValue.AsType(TFslStringList))
  else if aValue.Name = 'ID' Then
    FID := aValue.AsString
  else
    Inherited;
end;

procedure TsnBase.DoClear;
begin
  inherited;
  language := '';
  styleCode.Clear;
  ID := '';
end;

function TsnBase.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FstyleCode.sizeInBytes);
end;

{ TsnText }

Function TsnText.RIMClassNameV : String;
Begin
  Result := 'SD.TEXT';
End;

procedure TsnText.Assign(oSource: TFslObject);
begin
  inherited;
  ID := TsnText(oSource).ID;
  language := TsnText(oSource).language;
  styleCode.Assign(TsnText(oSource).styleCode); //TFslStringList
  parts.Assign(TsnText(oSource).parts);  //TsnCMGeneralList
End;

procedure TsnText.DoClear;
begin
  inherited;
  ID := '';
  language := '';
  FmediaType := '';
  styleCode.Clear;
  parts.Clear;
end;

procedure TsnText.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'mediaType', FmediaType));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'ID', FId));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'language', Flanguage));
  oList.Add(Tv3PropertyDefinition.CreateStrings(self, true, 'styleCode', FStyleCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'parts', Fparts, rmpctList, 'StrucDoc.CMGeneral')); // TsnCMGeneralList;
end;

procedure TsnText.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'mediaType' Then
    FmediaType := aValue.AsString
  else if aValue.Name = 'ID' Then
    FID := aValue.AsString
  else if aValue.Name = 'language' Then
    Flanguage := aValue.AsString
  else if aValue.Name = 'styleCode' Then
    styleCode.Assign(aValue.AsType(TFslStringList))
  else if aValue.Name = 'parts' Then
    parts.Assign(aValue.AsType(TsnCMGeneralList))
  else
    Inherited;
end;

function TsnText.CDAClassTypeV: TCDAClassType;
begin
  result := etNarrative;
end;

function TsnText.Clone(parent : Tv3Base): TsnText;
begin
  Result := TsnText(inherited Clone(parent));
end;

constructor TsnText.Create;
begin
  inherited;
  FstyleCode := TFslStringList.Create;
  Fparts := TsnCMGeneralList.Create(self);
  FmediaType := 'text/x-hl7-text+xml';
end;

destructor TsnText.Destroy;
begin
  FstyleCode.Free; //TFslStringList
  Fparts.Free; //TsnCMGeneralList
  inherited;
end;

function TsnText.Link: TsnText;
begin
  Result := TsnText(Inherited Link);
end;

function TsnText.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FstyleCode.sizeInBytes);
  inc(result, Fparts.sizeInBytes);
end;

{ TsnTitle }

Function TsnTitle.RIMClassNameV : String;
Begin
  Result := 'SD.TITLE';
End;

procedure TsnTitle.Assign(oSource: TFslObject);
begin
  inherited;
  ID := TsnTitle(oSource).ID;
  language := TsnTitle(oSource).language;
  styleCode.Assign(TsnTitle(oSource).styleCode); //TFslStringList
  parts.Assign(TsnTitle(oSource).parts); //TsnCMTitleList
End;

procedure TsnTitle.DoClear;
begin
  inherited;
  language := '';
  ID := '';
  FmediaType := '';
  styleCode.Clear;
  parts.Clear;
end;

procedure TsnTitle.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'language', Flanguage));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'ID', FId));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'mediaType', FmediaType));
  oList.Add(Tv3PropertyDefinition.CreateStrings(self, true, 'styleCode', FStyleCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'parts', Fparts, rmpctList, 'StrucDoc.CMTitle')); // TsnCMTitleList;
end;

procedure TsnTitle.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'language' Then
    Flanguage := aValue.AsString
  else if aValue.Name = 'ID' Then
    FID := aValue.AsString
  else if aValue.Name = 'mediaType' Then
    FmediaType := aValue.AsString
  else if aValue.Name = 'styleCode' Then
    styleCode.Assign(aValue.AsType(TFslStringList))
  else if aValue.Name = 'parts' Then
    parts.Assign(aValue.AsType(TsnCMTitleList))
  else
    Inherited;
end;

function TsnTitle.CDAClassTypeV: TCDAClassType;
begin
  result := etNarrative;
end;

function TsnTitle.Clone(parent : Tv3Base): TsnTitle;
begin
  Result := TsnTitle(inherited Clone(parent));
end;

constructor TsnTitle.Create;
begin
  inherited;
  FstyleCode := TFslStringList.Create;
  Fparts := TsnCMTitleList.Create(self);
  FmediaType := 'text/x-hl7-text+xml';
end;

destructor TsnTitle.Destroy;
begin
  FstyleCode.Free; //TFslStringList
  Fparts.Free; //TsnCMTitleList
  inherited;
end;

function TsnTitle.Link: TsnTitle;
begin
  Result := TsnTitle(Inherited Link);
end;

function TsnTitle.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FstyleCode.sizeInBytes);
  inc(result, Fparts.sizeInBytes);
end;

{ TsnCMFootnotes }

Function TsnCMFootnotes.RIMClassNameV : String;
Begin
  Result := 'StrucDoc.CMFootnotes';
End;

procedure TsnCMFootnotes.Assign(oSource: TFslObject);
begin
  inherited;
  text := TsnCMFootnotes(oSource).text.Clone(self);
  footnote := TsnCMFootnotes(oSource).footnote.Clone(self);
  footnoteRef := TsnCMFootnotes(oSource).footnoteRef.Clone(self);
end;

function TsnCMFootnotes.Clone(parent : Tv3Base): TsnCMFootnotes;
begin
  Result := TsnCMFootnotes(inherited Clone(parent));
end;

constructor TsnCMFootnotes.Create;
begin
  inherited;
end;


destructor TsnCMFootnotes.Destroy;
begin
  Ftext.Free;
  Fentity.Free;
  Ffootnote.Free;
  inherited;
end;

function TsnCMFootnotes.Link: TsnCMFootnotes;
begin
  Result := TsnCMFootnotes(Inherited Link);
end;

procedure TsnCMFootnotes.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'text', Ftext, 'String'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'entity', Fentity, 'String'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'footnote', Ffootnote, 'FootNote')); // TsnFootNote;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'footnoteRef', FfootnoteRef, 'FootNoteRef')); // TsnFootNoteRef;
end;

procedure TsnCMFootnotes.Setentity(const Value: TsnString);
begin
  Fentity.Free;
  Fentity := Value;
  if Fentity <> nil then  entity.parent := self;

end;

procedure TsnCMFootnotes.SetFootnote(const Value: TsnFootNote);
begin
  Ffootnote.Free;
  Ffootnote := Value;
  if FFootnote <> nil then  Footnote.parent := self;

end;

procedure TsnCMFootnotes.SetfootnoteRef(const Value: TsnFootNoteRef);
begin
  FfootnoteRef := Value;
  FfootnoteRef.Free;
  if FfootnoteRef <> nil then  footnoteRef.parent := self;

end;

procedure TsnCMFootnotes.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'text' Then
    Ftext := TsnString(aValue.AsType(TsnString)).Clone(self)
  else if aValue.Name = 'entity' Then
    Fentity := TsnString(aValue.AsType(TsnString)).Clone(self)
  else if aValue.Name = 'footnote' Then
    footnote := TsnFootNote(aValue.AsType(TsnFootNote)).Clone(self)
  else if aValue.Name = 'footnoteRef' Then
    FfootnoteRef := TsnFootNoteRef(aValue.AsType(TsnFootNoteRef)).Clone(self)
  else
    Inherited;
end;

procedure TsnCMFootnotes.Settext(const Value: TsnString);
begin
  Ftext.Free;
  Ftext := Value;
  if Ftext <> nil then  text.parent := self;

end;

procedure TsnCMFootnotes.DoClear;
begin
  inherited;
  text := nil;
  entity := nil;
  footnote := nil;
  footnoteRef := Nil;
end;

procedure TsnCMFootnotes.GetText(builder: TFslStringBuilder);
begin
  if FText <> nil then
    builder.Append(FText.value);
end;

function TsnCMFootnotes.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Ftext.sizeInBytes);
  inc(result, Fentity.sizeInBytes);
  inc(result, Ffootnote.sizeInBytes);
  inc(result, FfootnoteRef.sizeInBytes);
end;

{ TsnCMInline }

Function TsnCMInline.RIMClassNameV : String;
Begin
  Result := 'StrucDoc.CMInline';
End;

procedure TsnCMInline.Assign(oSource: TFslObject);
begin
  inherited;
  linkHtml := TsnCMInline(oSource).linkHtml.Clone(self); //TsnLinkHtml
  sub := TsnCMInline(oSource).sub.Clone(self);
  sup := TsnCMInline(oSource).sup.Clone(self);
end;

function TsnCMInline.Clone(parent : Tv3Base): TsnCMInline;
begin
  Result := TsnCMInline(inherited Clone(parent));
end;

constructor TsnCMInline.Create;
begin
  inherited;
end;

procedure TsnCMInline.DoClear;
begin
  inherited;
  sub := nil;
  sup := nil;
  linkHtml := nil;
end;



procedure TsnCMInline.GetText(builder: TFslStringBuilder);
begin
  if Fsub <> nil then
    builder.Append(FSub.value)
  else if Fsup <> nil then
    builder.Append(FSup.value)
  else if FlinkHtml <> nil then
    FlinkHtml.GetText(builder)
  else
    inherited;
end;

destructor TsnCMInline.Destroy;
begin
  FSub.Free;
  FSup.Free;
  FlinkHtml.Free; //TsnLinkHtml
  inherited;
end;

function TsnCMInline.Link: TsnCMInline;
begin
  Result := TsnCMInline(Inherited Link);
end;

procedure TsnCMInline.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'sub', Fsub, 'String'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'sup', Fsup, 'String'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'linkHtml', FlinkHtml, 'LinkHtml')); // TsnLinkHtml;

end;

procedure TsnCMInline.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'sub' Then
    Fsub := TsnString(aValue.AsType(TsnString)).Clone(self)
  else if aValue.Name = 'sup' Then
    Fsup := TsnString(aValue.AsType(TsnString)).Clone(self)
  else if aValue.Name = 'linkHtml' Then
    FlinkHtml := TsnLinkHtml(aValue.AsType(TsnLinkHtml)).Clone(self)
  else
    Inherited;
end;

procedure TsnCMInline.Setsub(const Value: TsnString);
begin
  FSub.Free;
  Fsub := Value;
  if Fsub <> nil then  sub.parent := self;

end;

procedure TsnCMInline.Setsup(const Value: TsnString);
begin
  FSup.Free;
  Fsup := Value;
  if Fsup <> nil then  sup.parent := self;

end;

procedure TsnCMInline.SetlinkHtml(const Value: TsnLinkHtml);
begin
  FlinkHtml.Free;
  FlinkHtml := Value;
  if FlinkHtml <> nil then  linkHtml.parent := self;

end;

function TsnCMInline.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fsub.sizeInBytes);
  inc(result, Fsup.sizeInBytes);
  inc(result, FlinkHtml.sizeInBytes);
end;

{ TsnCMTitle }

Function TsnCMTitle.RIMClassNameV : String;
Begin
  Result := 'StrucDoc.CMTitle';
End;

procedure TsnCMTitle.Assign(oSource: TFslObject);
begin
  inherited;
  text := TsnCMTitle(oSource).text.Clone(self);
  footnote := TsnCMTitle(oSource).footnote.Clone(self); //TsnCMTitleList
  footnoteRef := TsnCMTitle(oSource).footnoteRef.Clone(self);
  br := TsnCMTitle(oSource).br.Clone(self);
  linkHtml := TsnCMTitle(oSource).linkHtml.Clone(self); //TsnLinkHtml
  sub := TsnCMTitle(oSource).sub.Clone(self);
  sup := TsnCMTitle(oSource).sup.Clone(self);
  content := TsnCMTitle(oSource).content.Clone(self); //TsnCMTitle
end;

function TsnCMTitle.Clone(parent : Tv3Base): TsnCMTitle;
begin
  Result := TsnCMTitle(inherited Clone(parent));
end;

constructor TsnCMTitle.Create;
begin
  inherited;
  Ffootnote := TsnCMTitleList.Create(self);
end;


destructor TsnCMTitle.Destroy;
begin
  Fbr.Free;
  Ftext.Free;
  FfootnoteRef.free;
  Fsup.free;
  Fsub.free;
  Fcontent.free;
  Ffootnote.free;
  FlinkHtml.free;
  Fentity.free;
  inherited;
end;

function TsnCMTitle.Link: TsnCMTitle;
begin
  Result := TsnCMTitle(Inherited Link);
end;

procedure TsnCMTitle.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'br', Fbr, 'Boolean'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'text', Ftext, 'String'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'entity', Fentity, 'String'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'footnoteRef', FfootnoteRef, 'String'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'sup', Fsup, 'String'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'sub', Fsub, 'String'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'content', Fcontent, 'StrucDoc.CMTitle')); // TsnCMTitle;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'footnote', Ffootnote, rmpctList, 'StrucDoc.CMTitle')); // TsnCMTitleList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'linkHtml', FlinkHtml, 'LinkHtml')); // TsnLinkHtml;

end;

procedure TsnCMTitle.SetBr(const Value: TsnBr);
begin
  FBr.Free;
  Fbr := Value;
  if FBr <> nil then  Br.parent := self;

end;

procedure TsnCMTitle.Setcontent(const Value: TsnCMTitle);
begin
  Fcontent.Free;
  Fcontent := Value;
  if Fcontent <> nil then  content.parent := self;

end;

procedure TsnCMTitle.Setentity(const Value: TsnString);
begin
  FEntity.free;
  Fentity := Value;
  if Fentity <> nil then  entity.parent := self;

end;

procedure TsnCMTitle.SetfootNote(const Value: TsnCMTitleList);
begin
  FfootNote.Free;
  FfootNote := Value;
  if FfootNote <> nil then  footNote.parent := self;

end;

procedure TsnCMTitle.SetfootnoteRef(const Value: TsnString);
begin
  FfootnoteRef.free;
  FfootnoteRef := Value;
  if FfootnoteRef <> nil then  footnoteRef.parent := self;

end;

procedure TsnCMTitle.SetlinkHtml(const Value: TsnLinkHtml);
begin
  FlinkHtml.Free;
  FlinkHtml := Value;
end;

procedure TsnCMTitle.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'br' Then
    Fbr := TsnBr(aValue.AsType(TsnBr)).Clone(self)
  else if aValue.Name = 'text' Then
    Ftext := TsnString(aValue.AsType(TsnString)).Clone(self)
  else if aValue.Name = 'entity' Then
    Fentity := TsnString(aValue.AsType(TsnString)).Clone(self)
  else if aValue.Name = 'footnoteRef' Then
    FfootnoteRef := TsnString(aValue.AsType(TsnString)).Clone(self)
  else if aValue.Name = 'sup' Then
    Fsup := TsnString(aValue.AsType(TsnString)).Clone(self)
  else if aValue.Name = 'sub' Then
    Fsub := TsnString(aValue.AsType(TsnString)).Clone(self)
  else if aValue.Name = 'content' Then
    Fcontent := TsnCMTitle(aValue.AsType(TsnCMTitle)).Clone(self)
  else if aValue.Name = 'footnote' Then
    Ffootnote := TsnCMTitleList(aValue.AsType(TsnCMTitleList)).Clone(self)
  else if aValue.Name = 'linkHtml' Then
    FlinkHtml := TsnLinkHtml(aValue.AsType(TsnLinkHtml)).Clone(self)
  else
    Inherited;
end;

procedure TsnCMTitle.Setsub(const Value: TsnString);
begin
  FSub.Free;
  Fsub := Value;
  if Fsub <> nil then  sub.parent := self;

end;

procedure TsnCMTitle.Setsup(const Value: TsnString);
begin
  FSup.Free;
  Fsup := Value;
  if Fsup <> nil then  sup.parent := self;

end;

procedure TsnCMTitle.Settext(const Value: TsnString);
begin
  FText.Free;
  Ftext := Value;
  if Ftext <> nil then  text.parent := self;

end;

procedure TsnCMTitle.DoClear;
begin
  inherited;
  br := nil;
  text := nil;
  footnoteRef := nil;
  sup := nil;
  sub := nil;
  content := nil;
  footnote := nil;
  linkHtml := Nil;
  entity := nil;
end;

procedure TsnCMTitle.GetText(builder: TFslStringBuilder);
begin
  if Fbr <> nil then
    builder.Append(#13#10)
  else if Ftext <> nil then
    builder.Append(FText.value)
  else if Fsup <> nil then
    builder.Append(Fsup.value)
  else if Fsub <> nil then
    builder.Append(Fsub.value)
  else if Fcontent <> nil then
    Fcontent.GetText(builder)
  else if FlinkHtml <> nil then
    FlinkHtml.GetText(builder)
  else
    inherited;
end;

function TsnCMTitle.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fbr.sizeInBytes);
  inc(result, Ftext.sizeInBytes);
  inc(result, FfootnoteRef.sizeInBytes);
  inc(result, Fsup.sizeInBytes);
  inc(result, Fsub.sizeInBytes);
  inc(result, Fcontent.sizeInBytes);
  inc(result, Ffootnote.sizeInBytes);
  inc(result, FlinkHtml.sizeInBytes);
  inc(result, Fentity.sizeInBytes);
end;

{ TsnCMContent }

Function TsnCMContent.RIMClassNameV : String;
Begin
  Result := 'StrucDoc.CMContent';
End;

procedure TsnCMContent.Assign(oSource: TFslObject);
begin
  inherited;
  content := TsnCMContent(oSource).content.Clone(self); //TsnContent
  br := TsnCMContent(oSource).br.Clone(self);
  renderMultiMedia := TsnCMContent(oSource).renderMultiMedia.Clone(self); //TsnRenderMultiMedia
end;


procedure TsnCMContent.DoClear;
begin
  inherited;
  br := nil;
  content := nil;
end;

procedure TsnCMContent.GetText(builder: TFslStringBuilder);
begin
  if Fbr <> nil then
    builder.Append(#13#10)
  else if Fcontent <> nil then
    Fcontent.GetText(builder)
  else if FrenderMultiMedia <> nil then
    FrenderMultiMedia.GetText(builder)
  else
    inherited;
end;

function TsnCMContent.Clone(parent : Tv3Base): TsnCMContent;
begin
  Result := TsnCMContent(inherited Clone(parent));
end;

constructor TsnCMContent.Create;
begin
  inherited;
end;


destructor TsnCMContent.Destroy;
begin
  FBr.free;
  Fcontent.Free; //TsnContent
  FrenderMultiMedia.Free; //TsnRenderMultiMedia
  inherited;
end;

function TsnCMContent.Link: TsnCMContent;
begin
  Result := TsnCMContent(Inherited Link);
end;

procedure TsnCMContent.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'br', Fbr, 'Br'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'content', Fcontent, 'Content')); // TsnContent;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'renderMultiMedia', FrenderMultiMedia, 'RenderMultiMedia')); // TsnRenderMultiMedia;
end;

procedure TsnCMContent.SetBr(const Value: TsnBr);
begin
  FBr.free;
  Fbr := Value;
  if FBr <> nil then  Br.parent := self;

end;

procedure TsnCMContent.Setcontent(const Value: TsnContent);
begin
  Fcontent.Free;
  Fcontent := Value;
  if Fcontent <> nil then  content.parent := self;

end;

procedure TsnCMContent.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'br' Then
    Fbr := TsnBr(aValue.AsType(TsnBr)).Clone(self)
  else if aValue.Name = 'content' Then
    Fcontent := TsnContent(aValue.AsType(TsnContent)).Clone(self)
  else if aValue.Name = 'renderMultiMedia' Then
    FrenderMultiMedia := TsnRenderMultiMedia(aValue.AsType(TsnRenderMultiMedia)).Clone(self)
  else
    Inherited;
end;

procedure TsnCMContent.SetrenderMultiMedia(const Value: TsnRenderMultiMedia);
begin
  FrenderMultiMedia.Free;
  FrenderMultiMedia := Value;
  if FrenderMultiMedia <> nil then  renderMultiMedia.parent := self;

end;

function TsnCMContent.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fbr.sizeInBytes);
  inc(result, Fcontent.sizeInBytes);
  inc(result, FrenderMultiMedia.sizeInBytes);
end;

{ TsnCMGeneral }

Function TsnCMGeneral.RIMClassNameV : String;
Begin
  Result := 'StrucDoc.CMGeneral';
End;

procedure TsnCMGeneral.Assign(oSource: TFslObject);
begin
  inherited;
  paragraph := TsnCMGeneral(oSource).paragraph.Clone(self); //TsnParagraph
  list := TsnCMGeneral(oSource).list.Clone(self); //TsnList
  table := TsnCMGeneral(oSource).table.Clone(self); //TsnTable
end;

procedure TsnCMGeneral.DoClear;
begin
  inherited;
  list := nil;
  paragraph := nil;
  table := nil;
end;

procedure TsnCMGeneral.GetText(builder: TFslStringBuilder);
begin
  if Flist <> nil then
    Flist.GetText(builder)
  else if Fparagraph <> nil then
    Fparagraph.GetText(builder)
  else if Ftable <> nil then
    Ftable.GetText(builder)
  else
    inherited;
end;

function TsnCMGeneral.Clone(parent : Tv3Base): TsnCMGeneral;
begin
  Result := TsnCMGeneral(inherited Clone(parent));
end;

constructor TsnCMGeneral.Create;
begin
  inherited;
end;


destructor TsnCMGeneral.Destroy;
begin
  Fparagraph.Free; //TsnParagraph
  Flist.Free; //TsnList
  Ftable.Free; //TsnTable
  inherited;
end;

function TsnCMGeneral.Link: TsnCMGeneral;
begin
  Result := TsnCMGeneral(Inherited Link);
end;

procedure TsnCMGeneral.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'list', Flist, 'List')); // TsnList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'paragraph', Fparagraph, 'Paragraph')); // TsnParagraph;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'table', Ftable, 'Table')); // TsnTable;

end;

procedure TsnCMGeneral.Setlist(const Value: TsnList);
begin
  Flist.Free;
  Flist := Value;
  if Flist <> nil then  list.parent := self;

end;

procedure TsnCMGeneral.Setparagraph(const Value: TsnParagraph);
begin
  Fparagraph.Free;
  Fparagraph := Value;
  if Fparagraph <> nil then  paragraph.parent := self;

end;

procedure TsnCMGeneral.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'list' Then
    Flist := TsnList(aValue.AsType(TsnList)).Clone(self)
  else if aValue.Name = 'paragraph' Then
    Fparagraph := TsnParagraph(aValue.AsType(TsnParagraph)).Clone(self)
  else if aValue.Name = 'table' Then
    Ftable := TsnTable(aValue.AsType(TsnTable)).Clone(self)
  else
    Inherited;
end;

procedure TsnCMGeneral.Settable(const Value: TsnTable);
begin
  Ftable.Free;
  Ftable := Value;
  if Ftable <> nil then  table.parent := self;

end;

function TsnCMGeneral.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Flist.sizeInBytes);
  inc(result, Fparagraph.sizeInBytes);
  inc(result, Ftable.sizeInBytes);
end;

{ TsnRenderMultiMedia }

Function TsnRenderMultiMedia.RIMClassNameV : String;
Begin
  Result := 'RenderMultiMedia';
End;

procedure TsnRenderMultiMedia.Assign(oSource: TFslObject);
begin
  inherited;
  caption := TsnRenderMultiMedia(oSource).caption.Clone(self); //TsnCaption
  referencedObject.Assign(TsnRenderMultiMedia(oSource).referencedObject); //TFslStringList {
end;

function TsnRenderMultiMedia.Clone(parent : Tv3Base): TsnRenderMultiMedia;
begin
  Result := TsnRenderMultiMedia(inherited Clone(parent));
end;

constructor TsnRenderMultiMedia.Create;
begin
  inherited;
  FreferencedObject := TFslStringList.Create;
end;


destructor TsnRenderMultiMedia.Destroy;
begin
  Fcaption.Free; //TsnCaption
  FreferencedObject.Free; //TFslStringList
  inherited;
end;

function TsnRenderMultiMedia.Link: TsnRenderMultiMedia;
begin
  Result := TsnRenderMultiMedia(Inherited Link);
end;

procedure TsnRenderMultiMedia.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateStrings(self, false, 'referencedObject', FreferencedObject));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'caption', Fcaption, 'Caption')); // TsnCaption;
end;

procedure TsnRenderMultiMedia.Setcaption(const Value: TsnCaption);
begin
  Fcaption.Free;
  Fcaption := Value;
  if Fcaption <> nil then  caption.parent := self;

end;

procedure TsnRenderMultiMedia.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'referencedObject' Then
    referencedObject.Assign(aValue.AsType(TFslStringList))
  else if aValue.Name = 'caption' Then
    Fcaption := TsnCaption(aValue.AsType(TsnCaption)).Clone(self)
  else
    Inherited;
end;

procedure TsnRenderMultiMedia.DoClear;
begin
  inherited;
  FreferencedObject.Clear;
  Fcaption := nil;
end;

procedure TsnRenderMultiMedia.GetText(builder: TFslStringBuilder);
begin
  if FreferencedObject <> nil then
    builder.Append('['+FreferencedObject.AsCSV+']');
  if Fcaption <> nil then
    Fcaption.GetText(builder);
end;

function TsnRenderMultiMedia.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FreferencedObject.sizeInBytes);
  inc(result, Fcaption.sizeInBytes);
end;

{ TsnLinkHtml }

Function TsnLinkHtml.RIMClassNameV : String;
Begin
  Result := 'LinkHtml';
End;

procedure TsnLinkHtml.Assign(oSource: TFslObject);
begin
  inherited;
  name := TsnLinkHtml(oSource).name;
  href := TsnLinkHtml(oSource).href;
  rel := TsnLinkHtml(oSource).rel;
  rev := TsnLinkHtml(oSource).rev;
  title := TsnLinkHtml(oSource).title;
  parts.Assign(TsnLinkHtml(oSource).parts); //TsnCMFootnotesList
end;

procedure TsnLinkHtml.DoClear;
begin
  inherited;
  title := '';
  name := '';
  rel := '';
  rev := '';
  href := '';
  parts.Clear;
end;


procedure TsnLinkHtml.GetText(builder: TFslStringBuilder);
var
  i : integer;
begin
  builder.Append('[');
  for i := 0 to parts.Count - 1 do
    parts[i].GetText(builder);
  builder.Append(']');
end;

procedure TsnLinkHtml.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'title', Ftitle));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'name', Fname));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'rel', Frel));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'rev', Frev));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'href', Fhref));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'parts', Fparts, rmpctList, 'StrucDoc.CMFootnotes')); // TsnCMFootnotesList;
end;

procedure TsnLinkHtml.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'title' Then
    Ftitle := aValue.AsString
  else if aValue.Name = 'name' Then
    Fname := aValue.AsString
  else if aValue.Name = 'rel' Then
    Frel := aValue.AsString
  else if aValue.Name = 'rev' Then
    Frev := aValue.AsString
  else if aValue.Name = 'href' Then
    Fhref := aValue.AsString
  else if aValue.Name = 'parts' Then
    parts.Assign(aValue.AsType(TsnCMFootnotesList))
  else
    Inherited;
end;


function TsnLinkHtml.Clone(parent : Tv3Base): TsnLinkHtml;
begin
  Result := TsnLinkHtml(inherited Clone(parent));
end;

constructor TsnLinkHtml.Create;
begin
  inherited;
  Fparts := TsnCMFootnotesList.Create(self);
end;


destructor TsnLinkHtml.Destroy;
begin
  Fparts.Free; //TsnCMFootnotesList
  inherited;
end;

function TsnLinkHtml.Link: TsnLinkHtml;
begin
  Result := TsnLinkHtml(Inherited Link);
end;

function TsnLinkHtml.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fparts.sizeInBytes);
end;

{ TsnFootnote }

Function TsnFootnote.RIMClassNameV : String;
Begin
  Result := 'Footnote';
End;

procedure TsnFootnote.Assign(oSource: TFslObject);
begin
  inherited;
  parts.Assign(TsnFootnote(oSource).parts); //TsnCMGeneralList
end;

procedure TsnFootnote.DoClear;
begin
  inherited;
  Fparts.Clear;
end;


procedure TsnFootnote.GetText(builder: TFslStringBuilder);
var
  i : integer;
begin
  for i := 0 to parts.Count - 1 do
    parts[i].GetText(builder);
end;

procedure TsnFootnote.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'parts', Fparts, rmpctList, 'StrucDoc.CMGeneral')); // TsnCMGeneralList;
end;

procedure TsnFootnote.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'parts' Then
    parts.Assign(aValue.AsType(TsnCMGeneralList))
  else
    Inherited;
end;

function TsnFootnote.Clone(parent : Tv3Base): TsnFootnote;
begin
  Result := TsnFootnote(inherited Clone(parent));
end;

constructor TsnFootnote.Create;
begin
  inherited;
  Fparts := TsnCMGeneralList.Create(self);
end;


destructor TsnFootnote.Destroy;
begin
  Fparts.Free; //TsnCMGeneralList
  inherited;
end;

function TsnFootnote.Link: TsnFootnote;
begin
  Result := TsnFootnote(Inherited Link);
end;

function TsnFootnote.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fparts.sizeInBytes);
end;

{ TsnTitleFootnote }

Function TsnTitleFootnote.RIMClassNameV : String;
Begin
  Result := 'TitleFootnote';
End;

procedure TsnTitleFootnote.Assign(oSource: TFslObject);
begin
  inherited;
  parts.Assign(TsnTitleFootnote(oSource).parts); //TsnCMTitleList
end;

procedure TsnTitleFootnote.DoClear;
begin
  inherited;
  Parts.Clear;
end;


procedure TsnTitleFootnote.GetText(builder: TFslStringBuilder);
var
  i : integer;
begin
  for i := 0 to parts.Count - 1 do
    parts[i].GetText(builder);
end;

procedure TsnTitleFootnote.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'parts', Fparts, rmpctList, 'StrucDoc.CMTitle')); // TsnCMTitleList;
end;

procedure TsnTitleFootnote.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'parts' Then
    parts.Assign(aValue.AsType(TsnCMTitleList))
  else
    Inherited;
end;

function TsnTitleFootnote.Clone(parent : Tv3Base): TsnTitleFootnote;
begin
  Result := TsnTitleFootnote(inherited Clone(parent));
end;

constructor TsnTitleFootnote.Create;
begin
  inherited;
  Fparts := TsnCMTitleList.Create(self);
end;


destructor TsnTitleFootnote.Destroy;
begin
  Fparts.Free; //TsnCMTitleList
  inherited;
end;

function TsnTitleFootnote.Link: TsnTitleFootnote;
begin
  Result := TsnTitleFootnote(Inherited Link);
end;

function TsnTitleFootnote.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fparts.sizeInBytes);
end;

{ TsnFootnoteRef }

Function TsnFootnoteRef.RIMClassNameV : String;
Begin
  Result := 'FootnoteRef';
End;

procedure TsnFootnoteRef.Assign(oSource: TFslObject);
begin
  inherited;
  IDREF := TsnFootnoteRef(oSource).IDREF;
end;

function TsnFootnoteRef.Clone(parent : Tv3Base): TsnFootnoteRef;
begin
  Result := TsnFootnoteRef(inherited Clone(parent));
end;

constructor TsnFootnoteRef.Create;
begin
  inherited;
end;


destructor TsnFootnoteRef.Destroy;
begin
  inherited;
end;

function TsnFootnoteRef.Link: TsnFootnoteRef;
begin
  Result := TsnFootnoteRef(Inherited Link);
end;

procedure TsnFootnoteRef.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'IDREF', FIDREF));

end;

procedure TsnFootnoteRef.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'IDREF' Then
    FIDREF := aValue.AsString
  else
    Inherited;
end;

procedure TsnFootnoteRef.DoClear;
begin
  inherited;
  FIDREF := '';
end;


{ TsnCaption }

Function TsnCaption.RIMClassNameV : String;
Begin
  Result := 'Caption';
End;

procedure TsnCaption.Assign(oSource: TFslObject);
begin
  inherited;
  parts.Assign(TsnCaption(oSource).parts); //TsnCMInlineList
end;

function TsnCaption.Clone(parent : Tv3Base): TsnCaption;
begin
  Result := TsnCaption(inherited Clone(parent));
end;

constructor TsnCaption.Create;
begin
  inherited;
  Fparts := TsnCMInlineList.Create(self);
end;


destructor TsnCaption.Destroy;
begin
  Fparts.Free; //TsnCMInlineList
  inherited;
end;

function TsnCaption.Link: TsnCaption;
begin
  Result := TsnCaption(Inherited Link);
end;

function TsnCaption.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fparts.sizeInBytes);
end;

{ TsnContent }

Function TsnContent.RIMClassNameV : String;
Begin
  Result := 'Content';
End;

procedure TsnContent.Assign(oSource: TFslObject);
begin
  inherited;
  revised := TsnContent(oSource).revised; //TsnRevised
  parts.Assign(TsnContent(oSource).parts); //TsnCMContentList
end;

procedure TsnContent.DoClear;
begin
  inherited;
  parts.Clear;
  revised := rvnull;
end;


procedure TsnContent.GetText(builder: TFslStringBuilder);
var
  i : integer;
begin
  for i := 0 to parts.Count - 1 do
    parts[i].GetText(builder);
end;

procedure TsnContent.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'parts', Fparts, rmpctList, 'StrucDoc.CMContent')); // TsnCMContentList;
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'revised', Ord(Frevised), CODES_TsnRevised));
end;

procedure TsnContent.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'parts' Then
    parts.Assign(aValue.AsType(TsnCMContentList))
  else if aValue.Name = 'revised' Then
    Frevised := TsnRevised(aValue.AsEnum)
  else
    Inherited;
end;

function TsnContent.Clone(parent : Tv3Base): TsnContent;
begin
  Result := TsnContent(inherited Clone(parent));
end;

constructor TsnContent.Create;
begin
  inherited;
  Fparts := TsnCMContentList.Create(self);
end;


destructor TsnContent.Destroy;
begin
  Fparts.Free; //TsnCMContentList
  inherited;
end;

function TsnContent.Link: TsnContent;
begin
  Result := TsnContent(Inherited Link);
end;

function TsnContent.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fparts.sizeInBytes);
end;

{ TsnCaptioned }

Function TsnCaptioned.RIMClassNameV : String;
Begin
  Result := 'Captioned';
End;

procedure TsnCaptioned.Assign(oSource: TFslObject);
begin
  inherited;
  caption := TsnCaptioned(oSource).caption.Clone(self); //TsnCaption
End;

procedure TsnCaptioned.DoClear;
begin
  inherited;
  Caption := nil;
end;


procedure TsnCaptioned.GetText(builder: TFslStringBuilder);
begin
  if Fcaption <> nil then
    Fcaption.GetText(builder);
end;

procedure TsnCaptioned.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'caption' Then
    Fcaption := TsnCaption(aValue.AsType(TsnCaption)).Clone(self)
  else
    Inherited;
end;


function TsnCaptioned.Clone(parent : Tv3Base): TsnCaptioned;
begin
  Result := TsnCaptioned(inherited Clone(parent));
end;

constructor TsnCaptioned.Create;
begin
  inherited;
end;


destructor TsnCaptioned.Destroy;
begin
  Fcaption.Free; //TsnCaption
  inherited;
end;

function TsnCaptioned.Link: TsnCaptioned;
begin
  Result := TsnCaptioned(Inherited Link);
end;

procedure TsnCaptioned.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'caption', Fcaption, 'Caption')); // TsnCaption;
end;

procedure TsnCaptioned.Setcaption(const Value: TsnCaption);
begin
  Fcaption.Free;
  Fcaption := Value;
  if Fcaption <> nil then  caption.parent := self;

end;

function TsnCaptioned.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fcaption.sizeInBytes);
end;

procedure TsnCaption.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'parts', Fparts, rmpctList, 'StrucDoc.CMInline')); // TsnCMInlineList;
end;

procedure TsnCaption.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'parts' Then
    parts.Assign(aValue.AsType(TsnCMInlineList))
  else
    Inherited;
end;

procedure TsnCaption.DoClear;
begin
  inherited;
  parts.Clear;
end;

procedure TsnCaption.GetText(builder: TFslStringBuilder);
var
  i : integer;
begin
  for i := 0 to parts.Count - 1 do
    parts[i].GetText(builder);
end;

{ TsnParagraph }

Function TsnParagraph.RIMClassNameV : String;
Begin
  Result := 'Paragraph';
End;

procedure TsnParagraph.Assign(oSource: TFslObject);
begin
  inherited;
  parts.Assign(TsnParagraph(oSource).parts); //TsnCMContentList
end;

procedure TsnParagraph.DoClear;
begin
  inherited;
  Parts.Clear;
end;


procedure TsnParagraph.GetText(builder: TFslStringBuilder);
var
  i : integer;
begin
  for i := 0 to parts.Count - 1 do
    parts[i].GetText(builder);
  builder.Append(#13#10#13#10);
end;

procedure TsnParagraph.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'parts', Fparts, rmpctList, 'StrucDoc.CMContent')); // TsnCMContentList;
end;

procedure TsnParagraph.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'parts' Then
    parts.Assign(aValue.AsType(TsnCMContentList))
  else
    Inherited;
end;

function TsnParagraph.Clone(parent : Tv3Base): TsnParagraph;
begin
  Result := TsnParagraph(inherited Clone(parent));
end;

constructor TsnParagraph.Create;
begin
  inherited;
  Fparts := TsnCMContentList.Create(self);
end;


destructor TsnParagraph.Destroy;
begin
  Fparts.Free; //TsnCMContentList
  inherited;
end;

function TsnParagraph.Link: TsnParagraph;
begin
  Result := TsnParagraph(Inherited Link);
end;

function TsnParagraph.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fparts.sizeInBytes);
end;

{ TsnItem }

Function TsnItem.RIMClassNameV : String;
Begin
  Result := 'Item';
End;

procedure TsnItem.Assign(oSource: TFslObject);
begin
  inherited;
  parts.Assign(TsnItem(oSource).parts); //TsnCMGeneralList
end;

procedure TsnItem.DoClear;
begin
  inherited;
  Parts.Clear;
end;


procedure TsnItem.GetText(builder: TFslStringBuilder);
var
  i : integer;
begin
  for i := 0 to parts.Count - 1 do
  begin
    builder.Append('* ');
    parts[i].GetText(builder);
  end;
end;

procedure TsnItem.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'parts', Fparts, rmpctList, 'StrucDoc.CMGeneral')); // TsnCMGeneralList;
end;

procedure TsnItem.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'parts' Then
    parts.Assign(aValue.AsType(TsnCMGeneralList))
  else
    Inherited;
end;

function TsnItem.Clone(parent : Tv3Base): TsnItem;
begin
  Result := TsnItem(inherited Clone(parent));
end;

constructor TsnItem.Create;
begin
  inherited;
  Fparts := TsnCMGeneralList.Create(self);
end;


destructor TsnItem.Destroy;
begin
  Fparts.Free; //TsnCMGeneralList
  inherited;
end;

function TsnItem.Link: TsnItem;
begin
  Result := TsnItem(Inherited Link);
end;


function TsnItem.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fparts.sizeInBytes);
end;

{ TsnList }

Function TsnList.RIMClassNameV : String;
Begin
  Result := 'List';
End;

procedure TsnList.Assign(oSource: TFslObject);
begin
  inherited;
  listType := TsnList(oSource).listType; //TsnListType
  item.Assign(TsnList(oSource).item); //TsnItemList
end;

procedure TsnList.DoClear;
begin
  inherited;
  item.Clear;
  listType := lnull;
end;


procedure TsnList.GetText(builder: TFslStringBuilder);
var
  i : integer;
begin
  for i := 0 to item.Count - 1 do
    item[i].GetText(builder);
end;

procedure TsnList.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'item', Fitem, rmpctList, 'Item')); // TsnItemList;
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'listType', Ord(FlistType), CODES_TsnListType));
end;

procedure TsnList.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'item' Then
    Fitem := TsnItemList(aValue.AsType(TsnItemList)).Clone(self)
  else if aValue.Name = 'listType' Then
    FlistType := TsnListType(aValue.AsEnum)
  else
    Inherited;
end;

function TsnList.Clone(parent : Tv3Base): TsnList;
begin
  Result := TsnList(inherited Clone(parent));
end;

constructor TsnList.Create;
begin
  inherited;
  Fitem := TsnItemList.Create(self);
end;

destructor TsnList.Destroy;
begin
  Fitem.Free; //TsnItemList
  inherited;
end;

function TsnList.Link: TsnList;
begin
  Result := TsnList(Inherited Link);
end;

function TsnList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fitem.sizeInBytes);
end;

{ TsnTableItem }

Function TsnTableItem.RIMClassNameV : String;
Begin
  Result := 'TableItem';
End;

procedure TsnTableItem.Assign(oSource: TFslObject);
begin
  inherited;
  align := TsnTableItem(oSource).align; //TsnAlign
  char_ := TsnTableItem(oSource).char_;
  charoff := TsnTableItem(oSource).charoff;
  valign := TsnTableItem(oSource).valign; //TsnVAlign
end;

function TsnTableItem.Clone(parent : Tv3Base): TsnTableItem;
begin
  Result := TsnTableItem(inherited Clone(parent));
end;

constructor TsnTableItem.Create;
begin
  inherited;
end;


destructor TsnTableItem.Destroy;
begin
  inherited;
end;

function TsnTableItem.Link: TsnTableItem;
begin
  Result := TsnTableItem(Inherited Link);
end;

procedure TsnTableItem.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'char', Fchar));
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'align', Ord(Falign), CODES_TsnAlign));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'charoff', Fcharoff));
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'valign', Ord(Fvalign), CODES_TsnVAlign));
end;

procedure TsnTableItem.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'char' Then
    Fchar := aValue.AsString
  else if aValue.Name = 'align' Then
    Falign := TsnAlign(aValue.AsEnum)
  else if aValue.Name = 'charoff' Then
    Fcharoff := aValue.AsString
  else if aValue.Name = 'valign' Then
    Fvalign := TsnVAlign(aValue.AsEnum)
  else
    Inherited;
end;

procedure TsnTableItem.DoClear;
begin
  inherited;
  Fchar := '';
  Falign := anull;
  Fcharoff := '';
  Fvalign := vnull;
end;

{ TsnColItem }

Function TsnColItem.RIMClassNameV : String;
Begin
  Result := 'ColItem';
End;

procedure TsnColItem.Assign(oSource: TFslObject);
begin
  inherited;
  span := TsnColItem(oSource).span; //Integer
  width := TsnColItem(oSource).width;
end;

function TsnColItem.Clone(parent : Tv3Base): TsnColItem;
begin
  Result := TsnColItem(inherited Clone(parent));
end;

constructor TsnColItem.Create;
begin
  inherited;
  Fspan := 1;
end;


destructor TsnColItem.Destroy;
begin
  inherited;
end;

function TsnColItem.Link: TsnColItem;
begin
  Result := TsnColItem(Inherited Link);
end;

procedure TsnColItem.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateInteger(self, false, 'span', true, FSpan));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'width', Fwidth));
end;

procedure TsnColItem.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'span' Then
    Fspan := StringToInteger64(aValue.AsString)
  else if aValue.Name = 'width' Then
    Fwidth := aValue.AsString
  else
    Inherited;
end;

procedure TsnColItem.DoClear;
begin
  inherited;
  span := 0;
  width := '';
end;

{ TsnColGroup }

Function TsnColGroup.RIMClassNameV : String;
Begin
  Result := 'ColGroup';
End;

procedure TsnColGroup.Assign(oSource: TFslObject);
begin
  inherited;
  col.Assign(TsnColGroup(oSource).col);  //TsnColList
end;

procedure TsnColGroup.DoClear;
begin
  inherited;
  Col.Clear;
end;


procedure TsnColGroup.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'col', Fcol, rmpctList, 'Col')); // TsnColList;
end;

procedure TsnColGroup.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'col' Then
    Fcol := TsnColList(aValue.AsType(TsnColList)).Clone(self)
  else
    Inherited;
end;

function TsnColGroup.Clone(parent : Tv3Base): TsnColGroup;
begin
  Result := TsnColGroup(inherited Clone(parent));
end;

constructor TsnColGroup.Create;
begin
  inherited;
  Fcol := TsnColList.Create(self);
end;


destructor TsnColGroup.Destroy;
begin
  Fcol.Free; //TsnColList
  inherited;
end;

function TsnColGroup.Link: TsnColGroup;
begin
  Result := TsnColGroup(Inherited Link);
end;

function TsnColGroup.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fcol.sizeInBytes);
end;

{ TsnCol }

Function TsnCol.RIMClassNameV : String;
Begin
  Result := 'Col';
End;

procedure TsnCol.Assign(oSource: TFslObject);
begin
  inherited;
end;

function TsnCol.Clone(parent : Tv3Base): TsnCol;
begin
  Result := TsnCol(inherited Clone(parent));
end;

constructor TsnCol.Create;
begin
  inherited;
end;


destructor TsnCol.Destroy;
begin
  inherited;
end;

function TsnCol.Link: TsnCol;
begin
  Result := TsnCol(Inherited Link);
end;

{ TsnTCell }

Function TsnTCell.RIMClassNameV : String;
Begin
  Result := 'TCell';
End;

procedure TsnTCell.Assign(oSource: TFslObject);
begin
  inherited;
  abbr := TsnTCell(oSource).abbr;
  axis := TsnTCell(oSource).axis;
  headers.Assign(TsnTCell(oSource).headers); //TFslStringList
  scope := TsnTCell(oSource).scope; //TsnCellScope
  rowspan := TsnTCell(oSource).rowspan; //Integer
  colspan := TsnTCell(oSource).colspan; //Integer
  parts.Assign(TsnTCell(oSource).parts); //TsnCMGeneralList
end;

function TsnTCell.Clone(parent : Tv3Base): TsnTCell;
begin
  Result := TsnTCell(inherited Clone(parent));
end;

constructor TsnTCell.Create;
begin
  inherited;
  Fheaders := TFslStringList.Create;
  Frowspan := 1;
  Fcolspan := 1;
  Fparts := TsnCMGeneralList.Create(self);
end;


destructor TsnTCell.Destroy;
begin
  Fheaders.Free; //TFslStringList
  Fparts.Free; //TsnCMGeneralList
  inherited;
end;

function TsnTCell.Link: TsnTCell;
begin
  Result := TsnTCell(Inherited Link);
end;

function TsnTCell.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fheaders.sizeInBytes);
  inc(result, Fparts.sizeInBytes);
end;

{ TsnTRowPart }

Function TsnTRowPart.RIMClassNameV : String;
Begin
  Result := 'TRowPart';
End;

procedure TsnTRowPart.Assign(oSource: TFslObject);
begin
  inherited;
  th := TsnTRowPart(oSource).th.Clone(self); //TsnTCell
  td := TsnTRowPart(oSource).td.Clone(self); //TsnTCell
end;

procedure TsnTRowPart.DoClear;
begin
  inherited;
  th := nil;
  td := nil;
end;


procedure TsnTRowPart.GetText(builder: TFslStringBuilder);
begin
  if Fth <> nil then
    fth.GetText(builder);
  if Ftd <> nil then
    ftd.GetText(builder);
end;

function TsnTRowPart.Clone(parent : Tv3Base): TsnTRowPart;
begin
  Result := TsnTRowPart(inherited Clone(parent));
end;

constructor TsnTRowPart.Create;
begin
  inherited;
end;


destructor TsnTRowPart.Destroy;
begin
  Fth.Free; //TsnTCell
  Ftd.Free; //TsnTCell
  inherited;
end;

function TsnTRowPart.Link: TsnTRowPart;
begin
  Result := TsnTRowPart(Inherited Link);
end;

procedure TsnTRowPart.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'th', Fth, 'TCell')); // TsnTCell;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'td', Ftd, 'TCell')); // TsnTCell;

end;

procedure TsnTRowPart.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'th' Then
    Fth := TsnTCell(aValue.AsType(TsnTCell)).Clone(self)
  else if aValue.Name = 'td' Then
    Ftd := TsnTCell(aValue.AsType(TsnTCell)).Clone(self)
  else
    Inherited;
end;

procedure TsnTRowPart.Settd(const Value: TsnTCell);
begin
  Ftd.Free;
  Ftd := Value;
  if Ftd <> nil then  td.parent := self;

end;

procedure TsnTRowPart.Setth(const Value: TsnTCell);
begin
  Fth.Free;
  Fth := Value;
  if Fth <> nil then  th.parent := self;

end;

procedure TsnTCell.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateInteger(self, false, 'rowspan', true, FrowSpan));
  oList.Add(Tv3PropertyDefinition.CreateInteger(self, false, 'colspan', true, FcolSpan));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'abbr', Fabbr));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'axis', Faxis));
  oList.Add(Tv3PropertyDefinition.CreateStrings(self, false, 'headers', Fheaders));
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'scope', Ord(Fscope), CODES_TsnCellScope));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'parts', Fparts, rmpctList, 'StrucDoc.CMGeneral')); // TsnCMGeneralList;
end;

procedure TsnTCell.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'rowspan' Then
    Frowspan := StringToInteger64(aValue.AsString)
  else if aValue.Name = 'colspan' Then
    Fcolspan := StringToInteger64(aValue.AsString)
  else if aValue.Name = 'abbr' Then
    Fabbr := aValue.AsString
  else if aValue.Name = 'axis' Then
    Faxis := aValue.AsString
  else if aValue.Name = 'headers' Then
    headers.Assign(aValue.AsType(TFslStringList))
  else if aValue.Name = 'scope' Then
    Fscope := TsnCellScope(aValue.AsEnum)
  else if aValue.Name = 'parts' Then
    parts.Assign(aValue.AsType(TsnCMGeneralList))
  else
    Inherited;
end;

procedure TsnTCell.DoClear;
begin
  inherited;
  rowspan := 0;
  colspan := 0;
  abbr := '';
  axis := '';
  headers.Clear;
  scope := cnull1;
  parts.Clear;
end;

procedure TsnTCell.GetText(builder: TFslStringBuilder);
var
  i : integer;
begin
  builder.Append('|-');
  for i := 0 to parts.Count - 1 do
    parts[i].GetText(builder);
  builder.Append('-|');
end;

{ TsnTRow }

Function TsnTRow.RIMClassNameV : String;
Begin
  Result := 'TRow';
End;

procedure TsnTRow.Assign(oSource: TFslObject);
begin
  inherited;
  parts.Assign(TsnTRow(oSource).parts); //TsnTRowPartList
end;

procedure TsnTRow.DoClear;
begin
  inherited;
  parts.Clear;
end;


procedure TsnTRow.GetText(builder: TFslStringBuilder);
var
  i : integer;
begin
  builder.Append('++');
  for i := 0 to parts.Count - 1 do
    parts[i].GetText(builder);
  builder.Append('++');
end;

procedure TsnTRow.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'parts', Fparts, rmpctList, 'TRowPart')); // TsnTRowPartList;
end;

procedure TsnTRow.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'parts' Then
    parts.Assign(aValue.AsType(TsnTRowPartList))
  else
    Inherited;
end;

function TsnTRow.Clone(parent : Tv3Base): TsnTRow;
begin
  Result := TsnTRow(inherited Clone(parent));
end;

constructor TsnTRow.Create;
begin
  inherited;
  Fparts := TsnTRowPartList.Create(self);
end;


destructor TsnTRow.Destroy;
begin
  Fparts.Free; //TsnTRowPartList
  inherited;
end;

function TsnTRow.Link: TsnTRow;
begin
  Result := TsnTRow(Inherited Link);
end;

function TsnTRow.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fparts.sizeInBytes);
end;

{ TsnTRowGroup }

Function TsnTRowGroup.RIMClassNameV : String;
Begin
  Result := 'TRowGroup';
End;

procedure TsnTRowGroup.Assign(oSource: TFslObject);
begin
  inherited;
  tr.Assign(TsnTRowGroup(oSource).tr); //TsnTRowList
end;

procedure TsnTRowGroup.DoClear;
begin
  inherited;
  tr.Clear;
end;


procedure TsnTRowGroup.GetText(builder: TFslStringBuilder);
var
  i : integer;
begin
  for i := 0 to ftr.Count - 1 do
    ftr[i].GetText(builder);
end;

procedure TsnTRowGroup.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'tr', Ftr, rmpctList, 'TRow')); // TsnTRowList;
end;

procedure TsnTRowGroup.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'tr' Then
    Ftr := TsnTRowList(aValue.AsType(TsnTRowList)).Clone(self)
  else
    Inherited;
end;


function TsnTRowGroup.Clone(parent : Tv3Base): TsnTRowGroup;
begin
  Result := TsnTRowGroup(inherited Clone(parent));
end;

constructor TsnTRowGroup.Create;
begin
  inherited;
  Ftr := TsnTRowList.Create(self);
end;


destructor TsnTRowGroup.Destroy;
begin
  Ftr.Free; //TsnTRowList
  inherited;

end;

function TsnTRowGroup.Link: TsnTRowGroup;
begin
  Result := TsnTRowGroup(Inherited Link);
end;

function TsnTRowGroup.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Ftr.sizeInBytes);
end;

{ TsnTable }

Function TsnTable.RIMClassNameV : String;
Begin
  Result := 'Table';
End;

procedure TsnTable.Assign(oSource: TFslObject);
begin
  inherited;
  col.Assign(TsnTable(oSource).col); //TsnColList
  colgroup.Assign(TsnTable(oSource).colgroup); //TsnColGroupList
  thead := TsnTable(oSource).thead.Clone(self); //TsnTRowGroup
  tfoot := TsnTable(oSource).tfoot.Clone(self); //TsnTRowGroup
  tbody.Assign(TsnTable(oSource).tbody); //TsnTRowGroupList
  summary := TsnTable(oSource).summary;
  width := TsnTable(oSource).width;
  border := TsnTable(oSource).border;
  frame := TsnTable(oSource).frame; //TsnFrame
  rules := TsnTable(oSource).rules; //TsnRules
  cellspacing := TsnTable(oSource).cellspacing;
  cellpadding := TsnTable(oSource).cellpadding;
end;

function TsnTable.Clone(parent : Tv3Base): TsnTable;
begin
  Result := TsnTable(inherited Clone(parent));
end;

constructor TsnTable.Create;
begin
  inherited;
  Fcol := TsnColList.Create(self);
  Fcolgroup := TsnColGroupList.Create(self);
  Ftbody := TsnTRowGroupList.Create(self);
end;


destructor TsnTable.Destroy;
begin
  Fcol.Free; //TsnColList
  Fcolgroup.Free; //TsnColGroupList
  Fthead.Free; //TsnTRowGroup
  Ftfoot.Free; //TsnTRowGroup
  Ftbody.Free; //TsnTRowGroupList
  inherited;
end;

procedure TsnTable.DoClear;
begin
  inherited;
  summary := '';
  frame := fnull;
  cellpadding := '';
  cellspacing := '';
  border := '';
  width := '';
  rules := rnull;
  tfoot := nil;
  thead := nil;
  colgroup.Clear;
  col.Clear;
  tbody.Clear;
end;


procedure TsnTable.GetText(builder: TFslStringBuilder);
var
  i : integer;
begin
  Fthead.GetText(builder);
  for i := 0 to Ftbody.Count - 1 do
    Ftbody[i].GetText(builder);
  Ftfoot.GetText(builder);
end;

function TsnTable.Link: TsnTable;
begin
  Result := TsnTable(Inherited Link);
end;

procedure TsnTable.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'summary', Fsummary));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'frame', Codes_TsnFrame[Fframe])); // TsnFrame;
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'cellpadding', Fcellpadding));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'cellspacing', Fcellspacing));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'border', Fborder));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'width', Fwidth));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'rules', Codes_TsnRules[Frules]));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'tfoot', Ftfoot, 'TRowGroup')); // TsnTRowGroup;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'thead', Fthead, 'TRowGroup')); // TsnTRowGroup;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'colgroup', Fcolgroup, rmpctList, 'ColGroup')); // TsnColGroupList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'col', Fcol, rmpctList, 'Col')); // TsnColList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'tbody', Ftbody, rmpctList, 'TRowGroup')); // TsnTRowGroupList;

end;

procedure TsnTable.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'summary' Then
    Fsummary := aValue.AsString
  else if aValue.Name = 'frame' Then
    Fframe := TsnFrame(aValue.AsEnum)
  else if aValue.Name = 'cellpadding' Then
    Fcellpadding := aValue.AsString
  else if aValue.Name = 'cellspacing' Then
    Fcellspacing := aValue.AsString
  else if aValue.Name = 'border' Then
    Fborder := aValue.AsString
  else if aValue.Name = 'width' Then
    Fwidth := aValue.AsString
  else if aValue.Name = 'rules' Then
    Frules := TsnRules(aValue.AsEnum)
  else if aValue.Name = 'tfoot' Then
    Ftfoot := TsnTRowGroup(aValue.AsType(TsnTRowGroup)).Clone(self)
  else if aValue.Name = 'thead' Then
    Fthead := TsnTRowGroup(aValue.AsType(TsnTRowGroup)).Clone(self)
  else if aValue.Name = 'colgroup' Then
    Fcolgroup := TsnColGroupList(aValue.AsType(TsnColGroupList)).Clone(self)
  else if aValue.Name = 'col' Then
    Fcol := TsnColList(aValue.AsType(TsnColList)).Clone(self)
  else if aValue.Name = 'tbody' Then
    Ftbody := TsnTRowGroupList(aValue.AsType(TsnTRowGroupList)).Clone(self)
  else
    Inherited;
end;

procedure TsnTable.Settfoot(const Value: TsnTRowGroup);
begin
  Ftfoot.Free;
  Ftfoot := Value;
  if Ftfoot <> nil then  tfoot.parent := self;

end;

procedure TsnTable.Setthead(const Value: TsnTRowGroup);
begin
  Fthead.Free;
  Fthead := Value;
  if Fthead <> nil then  thead.parent := self;

end;



function TsnTable.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Ftfoot.sizeInBytes);
  inc(result, Fthead.sizeInBytes);
  inc(result, Fcolgroup.sizeInBytes);
  inc(result, Fcol.sizeInBytes);
  inc(result, Ftbody.sizeInBytes);
end;

function TsnTRowGroupList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TsnTRowGroupList.SetTsnTRowGroups(iIndex: Integer; value: TsnTRowGroup);
begin
  ObjectByIndex[iIndex] := value;
end;

function TsnTRowGroupList.ItemClass: TFslObjectClass;
begin
  Result := TsnTRowGroup;
end;

procedure TsnTRowGroupList.AddItem(value: TsnTRowGroup);
begin
  Add(value.Link);
end;

function TsnTRowGroupList.IndexOf(Value: TsnTRowGroup): Integer;
begin
  result := IndexByReference(value);
end;

{ TsnCMGeneralList }

Function TsnCMGeneralList.GetTsnCMGenerals(iIndex : Integer) : TsnCMGeneral;
Begin
  Result := TsnCMGeneral(ObjectByIndex[iIndex]);
End;

Function TsnCMGeneralList.Link : TsnCMGeneralList;
Begin
  Result := TsnCMGeneralList(Inherited Link);
End;

Function TsnCMGeneralList.Clone(parent : TV3Base): TsnCMGeneralList;
Begin
  Result := TsnCMGeneralList(inherited Clone(parent));
End;

Function TsnCMGeneralList.Append : TsnCMGeneral;
Begin
  Result := TsnCMGeneral.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Function TsnCMGeneralList.Insert(iIndex : Integer) : TsnCMGeneral;
Begin
  Result := TsnCMGeneral.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TsnCMGeneralList.InsertItem(iIndex : Integer; value : TsnCMGeneral);
Begin
  Inherited Insert(iIndex, value);
End;

Function TsnCMGeneralList.Item(iIndex : Integer) : TsnCMGeneral;
Begin
  Result := TsnCMGenerals[iIndex];
End;

Procedure TsnCMGeneralList.SetItemByIndex(iIndex : Integer; value: TsnCMGeneral);
Begin
  TsnCMGenerals[iIndex] := value;
End;

Procedure TsnCMGeneralList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TsnCMGeneralList.ClearItems;
Begin
  Clear;
End;


function TsnCMGeneralList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TsnCMGeneralList.SetTsnCMGenerals(iIndex: Integer; value: TsnCMGeneral);
begin
  ObjectByIndex[iIndex] := value;
end;

function TsnCMGeneralList.ItemClass: TFslObjectClass;
begin
  result := TsnCMGeneral;
end;

procedure TsnCMGeneralList.AddItem(value: TsnCMGeneral);
begin
  Add(value.Link);
end;

function TsnCMGeneralList.IndexOf(Value: TsnCMGeneral): Integer;
begin
  result := IndexByReference(value);
end;

{ TsnCMTitleList }

Function TsnCMTitleList.GetTsnCMTitles(iIndex : Integer) : TsnCMTitle;
Begin
  Result := TsnCMTitle(ObjectByIndex[iIndex]);
End;

Function TsnCMTitleList.Link : TsnCMTitleList;
Begin
  Result := TsnCMTitleList(Inherited Link);
End;

Function TsnCMTitleList.Clone(parent : Tv3Base): TsnCMTitleList;
Begin
  Result := TsnCMTitleList(inherited Clone(parent));
End;

function TsnCMTitleList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TsnCMTitleList.SetTsnCMTitles(iIndex: Integer; value: TsnCMTitle);
begin
  ObjectByIndex[iIndex] := value;
end;

function TsnCMTitleList.ItemClass: TFslObjectClass;
begin
  Result := TsnCMTitle;
end;

procedure TsnCMTitleList.AddItem(value: TsnCMTitle);
begin
  Add(value.Link)
end;

function TsnCMTitleList.IndexOf(Value: TsnCMTitle): Integer;
begin
  result := IndexByReference(value);
end;

{ TsnCMFootnotesList }

Function TsnCMFootnotesList.GetTsnCMFootnotess(iIndex : Integer) : TsnCMFootnotes;
Begin
  Result := TsnCMFootnotes(ObjectByIndex[iIndex]);
End;

Function TsnCMTitleList.Append : TsnCMTitle;
Begin
  Result := TsnCMTitle.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Function TsnCMTitleList.Insert(iIndex : Integer) : TsnCMTitle;
Begin
  Result := TsnCMTitle.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TsnCMTitleList.InsertItem(iIndex : Integer; value : TsnCMTitle);
Begin
  Inherited Insert(iIndex, value);
End;

Function TsnCMTitleList.Item(iIndex : Integer) : TsnCMTitle;
Begin
  Result := TsnCMTitles[iIndex];
End;

Procedure TsnCMTitleList.SetItemByIndex(iIndex : Integer; value: TsnCMTitle);
Begin
  TsnCMTitles[iIndex] := value;
End;

Procedure TsnCMTitleList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TsnCMTitleList.ClearItems;
Begin
  Clear;
End;

Function TsnCMFootnotesList.Link : TsnCMFootnotesList;
Begin
  Result := TsnCMFootnotesList(Inherited Link);
End;

Function TsnCMFootnotesList.Clone(parent : Tv3Base): TsnCMFootnotesList;
Begin
  Result := TsnCMFootnotesList(inherited Clone(parent));
End;

function TsnCMFootnotesList.Append: TsnCMFootnotes;
begin
  Result := TsnCMFootnotes.Create;
  try
    add(Result.Link);
  Finally
    result.Free;
  End;
end;

Function TsnCMFootnotesList.Insert(iIndex : Integer) : TsnCMFootnotes;
Begin
  Result := TsnCMFootnotes.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TsnCMFootnotesList.InsertItem(iIndex : Integer; value : TsnCMFootnotes);
Begin
  Inherited Insert(iIndex, value);
End;

Function TsnCMFootnotesList.Item(iIndex : Integer) : TsnCMFootnotes;
Begin
  Result := TsnCMFootnotess[iIndex];
End;

Procedure TsnCMFootnotesList.SetItemByIndex(iIndex : Integer; value: TsnCMFootnotes);
Begin
  TsnCMFootnotess[iIndex] := value;
End;

Procedure TsnCMFootnotesList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TsnCMFootnotesList.ClearItems;
Begin
  Clear;
End;

function TsnCMFootnotesList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TsnCMFootnotesList.SetTsnCMFootnotess(iIndex: Integer; value: TsnCMFootnotes);
begin
  ObjectByIndex[iIndex] := value;
end;

function TsnCMFootnotesList.ItemClass: TFslObjectClass;
begin
  Result := TsnCMFootnotes;
end;

procedure TsnCMFootnotesList.AddItem(value: TsnCMFootnotes);
begin
  Add(value.Link);
end;

function TsnCMFootnotesList.IndexOf(Value: TsnCMFootnotes): Integer;
begin
  result := IndexByReference(value);
end;

{ TsnCMInlineList }

Function TsnCMInlineList.GetTsnCMInlines(iIndex : Integer) : TsnCMInline;
Begin
  Result := TsnCMInline(ObjectByIndex[iIndex]);
End;

Function TsnCMInlineList.Link : TsnCMInlineList;
Begin
  Result := TsnCMInlineList(Inherited Link);
End;

Function TsnCMInlineList.Clone(parent : Tv3Base): TsnCMInlineList;
Begin
  Result := TsnCMInlineList(inherited Clone(parent));
End;

Function TsnCMInlineList.Append : TsnCMInline;
Begin
  Result := TsnCMInline.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Function TsnCMInlineList.Insert(iIndex : Integer) : TsnCMInline;
Begin
  Result := TsnCMInline.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TsnCMInlineList.InsertItem(iIndex : Integer; value : TsnCMInline);
Begin
  Inherited Insert(iIndex, value);
End;

Function TsnCMInlineList.Item(iIndex : Integer) : TsnCMInline;
Begin
  Result := TsnCMInlines[iIndex];
End;

Procedure TsnCMInlineList.SetItemByIndex(iIndex : Integer; value: TsnCMInline);
Begin
  TsnCMInlines[iIndex] := value;
End;

Procedure TsnCMInlineList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TsnCMInlineList.ClearItems;
Begin
  Clear;
End;

function TsnCMInlineList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TsnCMInlineList.SetTsnCMInlines(iIndex: Integer; value: TsnCMInline);
begin
  ObjectByIndex[iIndex] := value;
end;

function TsnCMInlineList.ItemClass: TFslObjectClass;
begin
  Result := TsnCMInline;
end;

procedure TsnCMInlineList.AddItem(value: TsnCMInline);
begin
  Add(value.Link);
end;

function TsnCMInlineList.IndexOf(Value: TsnCMInline): Integer;
begin
  result := IndexByReference(value);
end;

{ TsnCMContentList }

Function TsnCMContentList.GetTsnCMContents(iIndex : Integer) : TsnCMContent;
Begin
  Result := TsnCMContent(ObjectByIndex[iIndex]);
End;

Function TsnCMContentList.Link : TsnCMContentList;
Begin
  Result := TsnCMContentList(Inherited Link);
End;

Function TsnCMContentList.Clone(parent : Tv3Base): TsnCMContentList;
Begin
  Result := TsnCMContentList(inherited Clone(parent));
End;

Function TsnCMContentList.Append : TsnCMContent;
Begin
  Result := TsnCMContent.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Function TsnCMContentList.Insert(iIndex : Integer) : TsnCMContent;
Begin
  Result := TsnCMContent.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TsnCMContentList.InsertItem(iIndex : Integer; value : TsnCMContent);
Begin
  Inherited Insert(iIndex, value);
End;

Function TsnCMContentList.Item(iIndex : Integer) : TsnCMContent;
Begin
  Result := TsnCMContents[iIndex];
End;

Procedure TsnCMContentList.SetItemByIndex(iIndex : Integer; value: TsnCMContent);
Begin
  TsnCMContents[iIndex] := value;
End;

Procedure TsnCMContentList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TsnCMContentList.ClearItems;
Begin
  Clear;
End;

function TsnCMContentList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TsnCMContentList.SetTsnCMContents(iIndex: Integer; value: TsnCMContent);
begin
  ObjectByIndex[iIndex] := value;
end;

function TsnCMContentList.ItemClass: TFslObjectClass;
begin
  Result := TsnCMContent;
end;

procedure TsnCMContentList.AddItem(value: TsnCMContent);
begin
  Add(value.Link);
end;

function TsnCMContentList.IndexOf(Value: TsnCMContent): Integer;
begin
  result := IndexByReference(value);
end;

{ TsnItemList }

Function TsnItemList.GetTsnItems(iIndex : Integer) : TsnItem;
Begin
  Result := TsnItem(ObjectByIndex[iIndex]);
End;

Function TsnItemList.Link : TsnItemList;
Begin
  Result := TsnItemList(Inherited Link);
End;

Function TsnItemList.Clone(parent : Tv3Base): TsnItemList;
Begin
  Result := TsnItemList(inherited Clone(parent));
End;

function TsnItemList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TsnItemList.SetTsnItems(iIndex: Integer;value: TsnItem);
begin
  ObjectByIndex[iIndex] := value;
end;

function TsnItemList.ItemClass: TFslObjectClass;
begin
  Result := TsnItem;
end;

procedure TsnItemList.AddItem(value: TsnItem);
begin
  Add(value.Link);
end;

function TsnItemList.IndexOf(Value: TsnItem): Integer;
begin
  result := IndexByReference(value);
end;

{ TsnColList }

Function TsnColList.GetTsnCols(iIndex : Integer) : TsnCol;
Begin
  Result := TsnCol(ObjectByIndex[iIndex]);
End;

Function TsnItemList.Append : TsnItem;
Begin
  Result := TsnItem.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Function TsnItemList.Insert(iIndex : Integer) : TsnItem;
Begin
  Result := TsnItem.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TsnItemList.InsertItem(iIndex : Integer; value : TsnItem);
Begin
  Inherited Insert(iIndex, value);
End;

Function TsnItemList.Item(iIndex : Integer) : TsnItem;
Begin
  Result := TsnItems[iIndex];
End;

Procedure TsnItemList.SetItemByIndex(iIndex : Integer; value: TsnItem);
Begin
  TsnItems[iIndex] := value;
End;

Procedure TsnItemList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TsnItemList.ClearItems;
Begin
  Clear;
End;

Function TsnColList.Link : TsnColList;
Begin
  Result := TsnColList(Inherited Link);
End;

Function TsnColList.Clone(parent : Tv3Base): TsnColList;
Begin
  Result := TsnColList(inherited Clone(parent));
End;

function TsnColList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TsnColList.SetTsnCols(iIndex: Integer; value: TsnCol);
begin
  ObjectByIndex[iIndex] := value;
end;

function TsnColList.ItemClass: TFslObjectClass;
begin
  Result := TsnCol;
end;

procedure TsnColList.AddItem(value: TsnCol);
begin
  Add(value.Link);
end;

function TsnColList.IndexOf(Value: TsnCol): Integer;
begin
  result := IndexByReference(value);
end;

{ TsnTRowPartList }

Function TsnTRowPartList.GetTsnTRowParts(iIndex : Integer) : TsnTRowPart;
Begin
  Result := TsnTRowPart(ObjectByIndex[iIndex]);
End;

Function TsnColList.Append : TsnCol;
Begin
  Result := TsnCol.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Function TsnColList.Insert(iIndex : Integer) : TsnCol;
Begin
  Result := TsnCol.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TsnColList.InsertItem(iIndex : Integer; value : TsnCol);
Begin
  Inherited Insert(iIndex, value);
End;

Function TsnColList.Item(iIndex : Integer) : TsnCol;
Begin
  Result := TsnCols[iIndex];
End;

Procedure TsnColList.SetItemByIndex(iIndex : Integer; value: TsnCol);
Begin
  TsnCols[iIndex] := value;
End;

Procedure TsnColList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TsnColList.ClearItems;
Begin
  Clear;
End;


Function TsnTRowPartList.Link : TsnTRowPartList;
Begin
  Result := TsnTRowPartList(Inherited Link);
End;

Function TsnTRowPartList.Clone(parent : Tv3Base): TsnTRowPartList;
Begin
  Result := TsnTRowPartList(inherited Clone(parent));
End;

Function TsnTRowPartList.Append : TsnTRowPart;
Begin
  Result := TsnTRowPart.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Function TsnTRowPartList.Insert(iIndex : Integer) : TsnTRowPart;
Begin
  Result := TsnTRowPart.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TsnTRowPartList.InsertItem(iIndex : Integer; value : TsnTRowPart);
Begin
  Inherited Insert(iIndex, value);
End;

Function TsnTRowPartList.Item(iIndex : Integer) : TsnTRowPart;
Begin
  Result := TsnTRowParts[iIndex];
End;

Procedure TsnTRowPartList.SetItemByIndex(iIndex : Integer; value: TsnTRowPart);
Begin
  TsnTRowParts[iIndex] := value;
End;

Procedure TsnTRowPartList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TsnTRowPartList.ClearItems;
Begin
  Clear;
End;


function TsnTRowPartList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TsnTRowPartList.SetTsnTRowParts(iIndex: Integer; value: TsnTRowPart);
begin
  ObjectByIndex[iIndex] := value;
end;

function TsnTRowPartList.ItemClass: TFslObjectClass;
begin
  Result := TsnTRowPart;
end;

procedure TsnTRowPartList.AddItem(value: TsnTRowPart);
begin
  Add(value.Link);
end;

function TsnTRowPartList.IndexOf(Value: TsnTRowPart): Integer;
begin
  result := IndexByReference(value);
end;

{ TsnTRowList }

Function TsnTRowList.GetTsnTRows(iIndex : Integer) : TsnTRow;
Begin
  Result := TsnTRow(ObjectByIndex[iIndex]);
End;

Function TsnTRowList.Link : TsnTRowList;
Begin
  Result := TsnTRowList(Inherited Link);
End;

Function TsnTRowList.Clone(parent : Tv3Base): TsnTRowList;
Begin
  Result := TsnTRowList(inherited Clone(parent));
End;

function TsnTRowList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TsnTRowList.SetTsnTRows(iIndex: Integer; value: TsnTRow);
begin
  ObjectByIndex[iIndex] := value;
end;

function TsnTRowList.ItemClass: TFslObjectClass;
begin
  Result := TsnTRow;
end;

procedure TsnTRowList.AddItem(value: TsnTRow);
begin
  Add(value.Link);
end;

function TsnTRowList.IndexOf(Value: TsnTRow): Integer;
begin
  result := IndexByReference(value);
end;

{ TsnColGroupList }

Function TsnColGroupList.GetTsnColGroups(iIndex : Integer) : TsnColGroup;
Begin
  Result := TsnColGroup(ObjectByIndex[iIndex]);
End;

Function TsnTRowList.Append : TsnTRow;
Begin
  Result := TsnTRow.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Function TsnTRowList.Insert(iIndex : Integer) : TsnTRow;
Begin
  Result := TsnTRow.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TsnTRowList.InsertItem(iIndex : Integer; value : TsnTRow);
Begin
  Inherited Insert(iIndex, value);
End;

Function TsnTRowList.Item(iIndex : Integer) : TsnTRow;
Begin
  Result := TsnTRows[iIndex];
End;

Procedure TsnTRowList.SetItemByIndex(iIndex : Integer; value: TsnTRow);
Begin
  TsnTRows[iIndex] := value;
End;

Procedure TsnTRowList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TsnTRowList.ClearItems;
Begin
  Clear;
End;

Function TsnColGroupList.Link : TsnColGroupList;
Begin
  Result := TsnColGroupList(Inherited Link);
End;

Function TsnColGroupList.Clone(parent : Tv3Base): TsnColGroupList;
Begin
  Result := TsnColGroupList(inherited Clone(parent));
End;

function TsnColGroupList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TsnColGroupList.SetTsnColGroups(iIndex: Integer; value: TsnColGroup);
begin
  ObjectByIndex[iIndex] := value;
end;

function TsnColGroupList.ItemClass: TFslObjectClass;
begin
  Result := TsnColGroup;
end;

procedure TsnColGroupList.AddItem(value: TsnColGroup);
begin
  Add(value.Link);
end;

function TsnColGroupList.IndexOf(Value: TsnColGroup): Integer;
begin
  result := IndexByReference(value);
end;

{ TsnTRowGroupList }

Function TsnTRowGroupList.GetTsnTRowGroups(iIndex : Integer) : TsnTRowGroup;
Begin
  Result := TsnTRowGroup(ObjectByIndex[iIndex]);
End;

Function TsnColGroupList.Append : TsnColGroup;
Begin
  Result := TsnColGroup.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Function TsnColGroupList.Insert(iIndex : Integer) : TsnColGroup;
Begin
  Result := TsnColGroup.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TsnColGroupList.InsertItem(iIndex : Integer; value : TsnColGroup);
Begin
  Inherited Insert(iIndex, value);
End;

Function TsnColGroupList.Item(iIndex : Integer) : TsnColGroup;
Begin
  Result := TsnColGroups[iIndex];
End;

Procedure TsnColGroupList.SetItemByIndex(iIndex : Integer; value: TsnColGroup);
Begin
  TsnColGroups[iIndex] := value;
End;

Procedure TsnColGroupList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TsnColGroupList.ClearItems;
Begin
  Clear;
End;

Function TsnTRowGroupList.Link : TsnTRowGroupList;
Begin
  Result := TsnTRowGroupList(Inherited Link);
End;

Function TsnTRowGroupList.Clone(parent : Tv3Base): TsnTRowGroupList;
Begin
  Result := TsnTRowGroupList(inherited Clone(parent));
End;


Function TsnTRowGroupList.Append : TsnTRowGroup;
Begin
  Result := TsnTRowGroup.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Function TsnTRowGroupList.Insert(iIndex : Integer) : TsnTRowGroup;
Begin
  Result := TsnTRowGroup.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TsnTRowGroupList.InsertItem(iIndex : Integer; value : TsnTRowGroup);
Begin
  Inherited Insert(iIndex, value);
End;

Function TsnTRowGroupList.Item(iIndex : Integer) : TsnTRowGroup;
Begin
  Result := TsnTRowGroups[iIndex];
End;

Procedure TsnTRowGroupList.SetItemByIndex(iIndex : Integer; value: TsnTRowGroup);
Begin
  TsnTRowGroups[iIndex] := value;
End;

Procedure TsnTRowGroupList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TsnTRowGroupList.ClearItems;
Begin
  Clear;
End;


{ TNarrativeBase }

function TNarrativeBase.CDAClassTypeV: TCDAClassType;
begin
  result := etNarrative;
end;

function TNarrativeBase.GetAsText: String;
var
  b : TFslStringBuilder;
begin
  if (self = nil) then
    result := '(--null--)'
  else
  begin
    b := TFslStringBuilder.Create;
    try
      GetText(b);
      result := b.ToString;
    finally
      b.Free;
    end;
  end;
end;


procedure TNarrativeBase.GetText(builder: TFslStringBuilder);
begin
end;

{ TsnString }

procedure TsnString.Assign(oSource: TFslObject);
begin
  inherited;
  FText := TsnString(oSource).FText;
end;

function TsnString.Clone(parent : Tv3Base): TsnString;
begin
  result := TsnString(inherited Clone(parent));
end;

constructor TsnString.Create(value: String);
begin
  Create;
  FText := value;
end;

constructor TsnString.Create;
begin
  inherited;
end;

destructor TsnString.Destroy;
begin
  inherited;
end;

procedure TsnString.DoClear;
begin
  inherited;
  FText := '';
end;

procedure TsnString.GetText(builder: TFslStringBuilder);
begin
  builder.Append(FText);
end;

function TsnString.Link: TsnString;
begin
  result := TsnString(inherited Link);
end;

procedure TsnString.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties: Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'text', Ftext));
end;

function TsnString.RIMClassNameV: String;
begin
  result := 'String';
end;

procedure TsnString.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'text' then
    FText := aValue.AsString
  else
    inherited;
end;

{ TsnBr }

function TsnBr.Clone(parent : Tv3Base): TsnBr;
begin
  result := TsnBr(inherited Clone(parent));
end;

constructor TsnBr.Create;
begin
  inherited;
end;

destructor TsnBr.Destroy;
begin
  inherited;
end;

procedure TsnBr.GetText(builder: TFslStringBuilder);
begin
  builder.Append(#13#10);
end;

function TsnBr.Link: TsnBr;
begin
  result := TsnBr(inherited Link);
end;

function TsnBr.RIMClassNameV: String;
begin
  result := 'Br';
end;

function TsnTRowPart {Choice}.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fth.sizeInBytes);
  inc(result, Ftd.sizeInBytes);
end;

End.

