Unit wp_document;

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

{$I fhir.inc}

Interface


Uses
  Windows, Classes, SysUtils, Graphics,
  fsl_base, fsl_utilities, fsl_stream, fsl_collections,
  wp_graphics, wp_types;

Type

{$IFDEF FORDOCO}
  {
    The style of the pen
  }
  TFslPenStyle = (apsSolid, apsDash, apsDashDot, apsDashDotDot, apsDot, apsInsideFrame, apsNone);

  {
    The style of the end of the pen line
  }
  TFslPenEndStyle = (apesSquare, apesFlat, apesRound);
{$ENDIF}

  TWPDocumentObjectReadOnly = (ReadOnlyDefault, ReadOnlyTrue, ReadOnlyFalse);

  TWPDocumentObjects = Class;

  {
    An object in a document
  }
  TWPDocumentObject = Class(TFslObject)
    Private
      FNamespace: String;
      FName: String;
      FHotspot : TWPHotspot;

      Function GetHasHotSpot: Boolean;
      Procedure SetHasHotspot(Const bValue: Boolean);
      Procedure SetHotspot(Const Value: TWPHotspot);
      Function GetNamePair: String;
      Procedure SetNamePair(Const Value: String);

    Protected

      Function ErrorClass : EFslExceptionClass; Overload; Override;

      Function GetChildren : TWPDocumentObjects; Overload; Virtual;

      Function CanHaveHotspot : Boolean; Overload; Virtual;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure AfterConstruction; Override;

      Procedure Assign(oObject : TFslObject); Override;

      Function Link : TWPDocumentObject;
      Function Clone : TWPDocumentObject;

      Function GetOwnerFor(oObject : TWPDocumentObject):TWPDocumentObject; Overload; Virtual;

      Function AsText : String; Overload; Virtual;
      Procedure Defaults; Overload; Virtual;

      Function ChildCount : Integer; Overload; Virtual;
      Function Child(iIndex : Integer) : TWPDocumentObject; Overload; Virtual;
      Procedure Clear; Overload; Virtual;
      Procedure ClearChildren; Overload; Virtual;

      Property Hotspot : TWPHotspot Read FHotSpot Write SetHotspot;
      Property HasHotspot : Boolean Read GetHasHotSpot Write SetHasHotspot;

    published
      {
        The namespace of the name applied to the element.

        The element names may be used to identify the parts of a document
        from scripts/code, but are only persisted in the native format
      }
      Property Namespace : String Read FNamespace Write FNamespace;
      {
        The name applied to the element.

        The element names may be used to identify the parts of a document
        from scripts/code, but are only persisted in the native format
      }
      Property Name : String Read FName Write FName;
      {
        The name and namespace applied to the element.

        The element names may be used to identify the parts of a document
        from scripts/code, but are only persisted in the native format
      }
      Property NamePair : String Read GetNamePair Write SetNamePair;
  End;

  TWPDocumentObjects = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentObject;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentObject);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentObject; Reintroduce; Overload; Virtual;

      Function ErrorClass : EFslExceptionClass; Overload; Override;

    Public
      Function Link : TWPDocumentObjects; Overload;
      Function Clone : TWPDocumentObjects; Overload;

      Function New : TWPDocumentObject; Reintroduce; Overload; Virtual;

      Function AsText : String; Overload; Virtual;

      Procedure AddAllCloned(oSource : TWPDocumentObjects); Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPDocumentObject Read GetElement Write SetElement; Default;
  End;

  {
    An annotation in a document.

    The owner is the namespace of the annotation service that maintains the annotation.

    Note that annotations are maintained in a separate list, and a referenced by their identity in the list.
    You should never delete annotations from the list, though this is possible because it's just a standard list.
  }
  TWPDocumentAnnotation = Class(TWPDocumentObject)
    Private
      FOwner : String;
      FText : String;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      Function Link : TWPDocumentAnnotation; Overload;
      Function Clone : TWPDocumentAnnotation; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Function Matches(oOther : TWPDocumentAnnotation) : Boolean; Overload; Virtual;
      Property Owner : String Read FOwner Write FOwner;
      Property Text : String Read FText Write FText;
  End;

  {
    An list of annotations in a document.

    Note that annotations are maintained in a separate list, and a referenced by their identity in the list.
    You should never delete annotations from the list, though this is possible because it's just a standard list.
  }
  TWPDocumentAnnotations = Class(TWPDocumentObjects)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentAnnotation;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentAnnotation);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentAnnotation; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentAnnotations; Overload;
      Function Clone : TWPDocumentAnnotations; Overload;

      Function New : TWPDocumentAnnotation; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPDocumentAnnotation Read GetElement Write SetElement; Default;
  End;

  {
    An Attachment in a document.

    The owner is the namespace of the Attachment service that maintains the Attachment.

    Note that Attachments are maintained in a separate list, and a referenced by their identity in the list.
    You should never delete Attachments from the list, though this is possible because it's just a standard list.
  }
  TWPDocumentAttachment = Class(TWPDocumentObject)
    Private
      FId : String;
      FContent : TFslBuffer;
      FMimeType: String;
      FExtension: String;
      procedure SetContent(const Value: TFslBuffer);
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;
      Function Link : TWPDocumentAttachment; Overload;
      Function Clone : TWPDocumentAttachment; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Function Matches(oOther : TWPDocumentAttachment) : Boolean; Overload; Virtual;
      Property Id : String Read FId Write FId;
      Property MimeType : String Read FMimeType Write FMimeType;
      Property Extension : String Read FExtension Write FExtension;
      Property Content : TFslBuffer Read FContent Write SetContent;
  End;

  {
    An list of Attachments in a document.

    Note that Attachments are maintained in a separate list, and a referenced by their identity in the list.
    You should never delete Attachments from the list, though this is possible because it's just a standard list.
  }
  TWPDocumentAttachments = Class(TWPDocumentObjects)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentAttachment;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentAttachment);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentAttachment; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentAttachments; Overload;
      Function Clone : TWPDocumentAttachments; Overload;

      Function New : TWPDocumentAttachment; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPDocumentAttachment Read GetElement Write SetElement; Default;
  End;

  {
    Content in a document - either text, image, line break, or field.
  }
  TWPDocumentContent = Class(TWPDocumentObject)
    Private
      FStyle : String;
      FFont : TWPSFontDetails;
      FAnnotation : Integer;

      Function GetFont : TWPSFontDetails;
      Procedure SetFont(Const Value : TWPSFontDetails);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TWPDocumentContent;
      Function Clone : TWPDocumentContent;

      Procedure Assign(oObject : TFslObject); Override;

      Function HasFont : Boolean;
      Procedure Defaults; Override;

    published
      {
        The name of the style applied to this element - will be ignored if it is not known
      }
      Property Style : String Read FStyle Write FStyle;
      {
        The font attributes applied to this element
      }
      Property Font : TWPSFontDetails Read GetFont Write SetFont;

      {
        The annotation applied to this item, or -1.
        Note that not all items are able to represented with an annotation

        Any given annotation may appear on more than one content element,
        but if they are not contiguous in a single paragraph, they will be broken
        up into multiple different annotations when loaded
      }
      Property Annotation : Integer read FAnnotation write FAnnotation;
  End;

  {
    Contents of a paragraph.
  }
  TWPDocumentContents = Class(TWPDocumentObjects)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentContent;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentContent);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentContent; Reintroduce; Virtual;

    Public
      Function Link : TWPDocumentContents; Overload;
      Function Clone : TWPDocumentContents; Overload;

      Function New : TWPDocumentContent; Reintroduce;

    {
      Add an already existing Content Item to the end of the list.
    }
    Procedure AddItem(value : TWPDocumentContent);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TWPDocumentContent) : Integer;
    {
       Insert an existing Content Item before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TWPDocumentContent);
    {
       Get the iIndexth Content Item. (0 = first item)
    }
    Function Item(iIndex : Integer) : TWPDocumentContent;
    {
       Set the iIndexth Content Item. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TWPDocumentContent);
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
      Property Elements[Const iIndex : Integer] : TWPDocumentContent Read GetElement Write SetElement; Default;
  End;

  {
    Content in a field either text, image, or line break.
  }
  TWPDocumentSimpleContent = Class(TWPDocumentContent)
    Public
      Function Link : TWPDocumentSimpleContent; Overload;
      Function Clone : TWPDocumentSimpleContent; Overload;
  End;

  {
    Contents of a Field.
  }
  TWPDocumentSimpleContents = Class(TWPDocumentContents)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentSimpleContent;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentSimpleContent);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentSimpleContent; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentSimpleContents; Overload;
      Function Clone : TWPDocumentSimpleContents; Overload;

      Function New : TWPDocumentSimpleContent; Reintroduce; Overload; Virtual;

    {
      Add an already existing Simple Content Item to the end of the list.
    }
    Procedure AddItem(value : TWPDocumentSimpleContent);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TWPDocumentSimpleContent) : Integer;
    {
       Insert an existing Simple Content Item before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TWPDocumentSimpleContent);
    {
       Get the iIndexth Simple Content Item. (0 = first item)
    }
    Function Item(iIndex : Integer) : TWPDocumentSimpleContent;
    {
       Set the iIndexth Simple Content Item. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TWPDocumentSimpleContent);
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
    Property Elements[const iIndex : Integer] : TWPDocumentSimpleContent read GetElement write SetElement; default;
  End;

  {
    Text in a content - one or more words with a common styling
  }
  TWPDocumentText = Class(TWPDocumentSimpleContent)
    Private
      FValue : String;
      FDrawnFont: String;

      Procedure SetValue(Const Value : String);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create(Const sValue : String); Overload; Virtual;

      Function Link : TWPDocumentText; Overload;
      Function Clone : TWPDocumentText; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Function AsText : String; Overload; Override;

    Published
      {
        The text value applied to this element (ASCII only at this time)
      }
      Property Value : String Read FValue Write SetValue;

      {
        An alternate font to actually draw the piece with.
        (used for inserted symbols - they claim to be in the font
        that the surrounding run of text has, but they draw in the
        specified font
      }
      Property DrawnFont : String read FDrawnFont write FDrawnFont;
  End;

  TWPDocumentTexts = Class(TWPDocumentSimpleContents)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentText;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentText);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentText; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentTexts; Overload;
      Function Clone : TWPDocumentTexts; Overload;

      Function New : TWPDocumentText; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPDocumentText Read GetElement Write SetElement; Default;
  End;

  {
    Image in a document - persists as either a jpeg or a png (as first encountered)
  }
  TWPDocumentImage = Class(TWPDocumentSimpleContent)
    Private
      FTitle : String;
      FBorderWidth : Word;
      FBorderColour : TColour;
      FImageWidth : Word;
      FImageHeight : Word;
      FVerticalAlignment : TWordProcessorImageVerticalAlignment;
      FTransparentColour : TColour;
      FImage : TFslGraphic;
      FSelectionImage : TFslGraphic;
      FMap : TWPImageMap;
      FAdornments : TWPDocumentImageAdornments;
      FSizePolicy: TWordProcessorImageSizePolicy;
      FFrameIndex : Integer;

      Function GetSelectionImage : TFslGraphic;
      Procedure SetSelectionImage(Const Value : TFslGraphic);
      Function GetImage : TFslGraphic;
      Procedure SetImage(Const Value : TFslGraphic);
      Function GetMap : TWPImageMap;
      Procedure SetMap(Const Value : TWPImageMap);
      Function GetHasMap : Boolean;
      Procedure SetHasMap(bValue : Boolean);
    Protected
      Function CanHaveHotspot : Boolean; Overload; Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPDocumentImage; Overload;
      Function Clone : TWPDocumentImage; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;
      Procedure Clear; Overload; Override;


      Function AsText : String; Overload; Override;
      Procedure Defaults; Overload; Override;
      Function HasImage : Boolean;
      Function HasSelectionImage : Boolean;

      Property Adornments : TWPDocumentImageAdornments Read FAdornments;
      Property Map : TWPImageMap Read GetMap Write SetMap;
      Property HasMap : Boolean Read GetHasMap Write SetHasMap;

      Property Image : TFslGraphic Read GetImage Write SetImage;
      Property SelectionImage : TFslGraphic Read GetSelectionImage Write SetSelectionImage;
      {
        Save the image to the filename provided. The image will be saved as jpg if
        that was it's original form, otherwise it will be saved as PNG
      }
      procedure SaveToFile(sName : String);
      {
        Load the image from a file. The following formats are supported: BMP, JPG, GIF, PNG, TIFF. The format is autodetected based on the contents, not the name
      }
      Procedure LoadFromFile(sName : String);
      {
        Save the image to the stream provided. The image will be saved as jpg if
        that was it's original form, otherwise it will be saved as PNG
      }
      Procedure SaveToStream(oStream : TStream);
      {
        Load the image from a stream. The following formats are supported: BMP, JPG, GIF, PNG, TIFF. The format is autodetected
      }
      Procedure LoadFromStream(oStream : TStream);
    published
      {
        The alternative display title of the image. defaults to the name of the file if the image was loaded from a file
      }
      Property Title : String Read FTitle Write FTitle;
      {
        The border to draw around the image
      }
      Property BorderWidth : Word Read FBorderWidth Write FBorderWidth;
      {
        The colour of the border around the image
      }
      Property BorderColour : TColour Read FBorderColour Write FBorderColour;
      {
        The drawn width of the image (may be scaled). In pixels
      }
      Property ImageWidth : Word Read FImageWidth Write FImageWidth;
      {
        The drawn height of the image (may be scaled). In pixels
      }
      Property ImageHeight : Word Read FImageHeight Write FImageHeight;
      {
        The alignment of the image relative to the text surrounding it
      }
      Property VerticalAlignment : TWordProcessorImageVerticalAlignment Read FVerticalAlignment Write FVerticalAlignment;
      {
        The transparent colour of the image, if appropriate. Will be read from the file if the format specifies one
      }
      Property TransparentColour : TColour Read FTransparentColour Write FTransparentColour;

      {
        How size of the image is determined. If this is not set to manual, then the specified width and height are ignored
      }
      Property SizePolicy : TWordProcessorImageSizePolicy Read FSizePolicy Write FSizePolicy;

      {
        If the image has multiple frames (/pages), then which one will be shown
      }
      Property FrameIndex : Integer Read FFrameIndex Write FFrameIndex;

  End;

  TWPDocumentImages = Class(TWPDocumentSimpleContents)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentImage;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentImage);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentImage; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentImages; Overload;
      Function Clone : TWPDocumentImages; Overload;

      Function New : TWPDocumentImage; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPDocumentImage Read GetElement Write SetElement; Default;
  End;


(*
masks:

There is 6 kinds of masks, each differentiated by the
starting two characters:
r:mask regex. mask isstandard regex
c:code code. where mask as in FslCodeMasks
m:mask maskedit where mask is as documented for TMaskEdit
n:mask numeric where range is D..D and D is a number. Floating point only allowed if either limit includes decimal
e:mask enumerated values. mask is comma separated values. If e is empty, use vocabulary instead.
s:mask special. mask is
   colour or color - content is a colour.
   date - content is a date
   datetime - content is a date time
   ip address
   email address
   pq[xx] - content is a measurement. it is an item followed by a ucum unit. the rest of the mask is the mask for what comes before the unit
  these specials are associated with special content assistance

if the first two characters don't match one of these types, the mask is assumed to be regex

*)


Const
  FIELD_MASK_INTEGER = 'r:[-+]?[0-9]*';
  FIELD_MASK_FLOAT = 'r:[-+]?[0-9]*\.?[0-9]+'; // note this that doesn't allow exponential representations
  FIELD_MASK_NAME = 'r:[a-bA-B_]([0-9a-bA-B_])*';
  FIELD_MASK_TOKEN = 'r:(\d)*';

Type
  TWPDocumentFieldDeletable = Boolean;
  TWPDocumentFieldFixedFormat = (fffFixed, fffWholeField, fffAnyPart);

  {
    A field: named or controlled content in a document
  }
  TWPDocumentField = Class(TWPDocumentContent)
    Private
      FReadOnly : TWPDocumentObjectReadOnly;
      FDeletable : TWPDocumentFieldDeletable;
      FFixedFormat : TWPDocumentFieldFixedFormat;
      FData : TWPDataItemMap;
      FContents : TWPDocumentSimpleContents;
      FWidth: Integer;
      FCheckedIndex: Integer;

      Function GetContents : TWPDocumentSimpleContents;
      Procedure SetContents(Const Value : TWPDocumentSimpleContents);
      Function GetDataValue(Const sKey: String): String;
      Procedure SetDataValue(Const sKey, sValue: String);
      function GetLink: TWPHotspot;
      procedure SetLink(const Value: TWPHotspot);
    Protected
      Function GetChildren : TWPDocumentObjects; Overload; Override;
      Function CanHaveHotspot : Boolean; Overload; Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPDocumentField; Overload;
      Function Clone : TWPDocumentField; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;
      Procedure ClearChildren; Overload; Override;

      Function GetOwnerFor(oObject : TWPDocumentObject): TWPDocumentObject; Overload; Override;

      Function HasContents : Boolean;
      Procedure Defaults; Overload; Override;
      Function AsText : String; Overload; Override;

      Property ReadOnly : TWPDocumentObjectReadOnly Read FReadOnly Write FReadOnly;
      Property Deletable : TWPDocumentFieldDeletable Read FDeletable Write FDeletable;
      Property FixedFormat : TWPDocumentFieldFixedFormat Read FFixedFormat Write FFixedFormat;
      Property CheckedIndex : Integer read FCheckedIndex write FCheckedIndex;
      {
        Mimimum Width of Field in approximate character units (i.e. when Field is empty)
      }
      Property Width : Integer read FWidth write FWidth;

      Property RawData : TWPDataItemMap Read FData; // not for use outside word processor
      {
        It's possible to associated a series of name = value pairs with the field.
        These are only persisted in the native format
      }

      Property DataValue[Const sKey : String] : String Read GetDataValue Write SetDataValue;
      {
        Check whether a data value with the name provided exists
      }
      Function HasDataValue(Const sKey : String) : Boolean;
      {
        Scrub any data value associated with that name
      }

      Procedure DeleteDataValue(Const sKey : String);


    Published
      {
        The contents of the field - text, images, and line breaks
      }
      Property Contents : TWPDocumentSimpleContents Read GetContents Write SetContents;

      {
        a hotlink to associate with this field
      }
      Property UrlLink : TWPHotspot read GetLink write SetLink;
  End;

  TWPDocumentFields = Class(TWPDocumentContents)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentField;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentField);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentField; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentFields; Overload;
      Function Clone : TWPDocumentFields; Overload;

      Function New : TWPDocumentField; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPDocumentField Read GetElement Write SetElement; Default;
  End;

  {
    A line break = continue same paragraph on next line
  }
  TWPDocumentLineBreak = Class(TWPDocumentSimpleContent)
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;
      Function Link : TWPDocumentLineBreak; Overload;
      Function Clone : TWPDocumentLineBreak; Overload;

      Function AsText : String; Overload; Override;

      Procedure Assign(oObject : TFslObject); Overload; Override;
  End;


  TWPDocumentLineBreaks = Class(TWPDocumentSimpleContents)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentLineBreak;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentLineBreak);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentLineBreak; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentLineBreaks; Overload;
      Function Clone : TWPDocumentLineBreaks; Overload;

      Function New : TWPDocumentLineBreak; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPDocumentLineBreak Read GetElement Write SetElement; Default;
  End;

Const
  TWPDOCUMENTOBJECT_READONLY_VALUE : Array[TWPDocumentObjectReadOnly] Of String = ('Unknown', 'True', 'False');
  TWPDOCUMENTOBJECT_FIXED_FORMAT : Array[TWPDocumentFieldFixedFormat] Of String = ('Fixed', 'Whole', 'Any');

Type
  {
    a holder of content that fills a block. i.e. paragrqph, table etc
  }
  TWPDocumentBlock = Class(TWPDocumentObject)
    Private
      FReadOnly : TWPDocumentObjectReadOnly;

    Public
      Function Link : TWPDocumentBlock; Overload;
      Function Clone : TWPDocumentBlock; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;
      Procedure Defaults; Overload; Override;

      Property ReadOnly : TWPDocumentObjectReadOnly Read FReadOnly Write FReadOnly;
  End;

  {
    Contents of a Document or Section.
  }
  TWPDocumentBlocks = Class(TWPDocumentObjects)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentBlock;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentBlock);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentBlock; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentBlocks; Overload;
      Function Clone : TWPDocumentBlocks; Overload;

      Function New : TWPDocumentBlock; Reintroduce; Overload; Virtual;

    {
      Add an already existing Block to the end of the list.
    }
    Procedure AddItem(value : TWPDocumentBlock);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TWPDocumentBlock) : Integer;
    {
       Insert an existing Block before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TWPDocumentBlock);
    {
       Get the iIndexth Specimen. (0 = first item)
    }
    Function Item(iIndex : Integer) : TWPDocumentBlock;
    {
       Set the iIndexth Block. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TWPDocumentBlock);
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
    Property Elements[const iIndex : Integer] : TWPDocumentBlock read GetElement write SetElement; default;
  End;


  {
    a parapraph - contains zero or more or (text, image, line break, or field)

    lists are built using paragraph styles
  }
  TWPDocumentParagraph = Class(TWPDocumentBlock)
    Private
      FStyle : String;
      FFormat : TWPSParagraphDetails;
      FFont : TWPSFontDetails;
      FContents : TWPDocumentContents;

      Function GetFormat : TWPSParagraphDetails;
      Procedure SetFormat(Const Value : TWPSParagraphDetails);

      Function GetFont : TWPSFontDetails;
      Procedure SetFont(Const Value : TWPSFontDetails);

      Function GetContents : TWPDocumentContents;
      Procedure SetContents(Const Value : TWPDocumentContents);

    Protected
      Function GetChildren : TWPDocumentObjects; Overload; Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPDocumentParagraph; Overload;
      Function Clone : TWPDocumentParagraph; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;
      Procedure ClearChildren; Overload; Override;

      Function GetOwnerFor(oObject : TWPDocumentObject): TWPDocumentObject; Overload; Override;

      Function HasFormat : Boolean; Overload; Virtual;
      Function HasFont : Boolean; Overload; Virtual;
      Function HasContents : Boolean; Overload; Virtual;

      Function AsText : String; Overload; Override;
      Procedure Defaults; Overload; Override;

    Published
      {
        The style of the paragraph itself (contents of the paragraph may have different styles applied).
        This is the name of the style. If the style isn't known, it will be ignored
      }
      Property Style : String Read FStyle Write FStyle;
      {
        The paragraph format - can override what is specified in the style
      }
      Property Format : TWPSParagraphDetails Read GetFormat Write SetFormat;
      {
        The font of the paragraph (and it's bullets/numbers if applicable). - can override what is specified in the style
      }
      Property Font : TWPSFontDetails Read GetFont Write SetFont;
      {
        The contents of the paragraph
      }
      Property Contents : TWPDocumentContents Read GetContents Write SetContents;
  End;

  {
    Contents of a Cell in a table
  }
  TWPDocumentParagraphs = Class(TWPDocumentBlocks)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentParagraph;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentParagraph);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentParagraph; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentParagraphs; Overload;
      Function Clone : TWPDocumentParagraphs; Overload;

      Function New : TWPDocumentParagraph; Reintroduce; Overload; Virtual;

    {
      Add a Paragraph to the end of the list.
    }
    Function Append : TWPDocumentParagraph;
    {
      Add an already existing Paragraph to the end of the list.
    }
    Procedure AddItem(value : TWPDocumentParagraph);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TWPDocumentParagraph) : Integer;
    {
       Insert Paragraph before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TWPDocumentParagraph;
    {
       Insert an existing Paragraph before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TWPDocumentParagraph);
    {
       Get the iIndexth Paragraph. (0 = first item)
    }
    Function Item(iIndex : Integer) : TWPDocumentParagraph;
    {
       Set the iIndexth Paragraph. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TWPDocumentParagraph);
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
    Property Elements[const iIndex : Integer] : TWPDocumentParagraph read GetElement write SetElement; default;
  End;


  {
    a block that holds other content - sections, table cells, document
  }
  TWPDocumentContainer = Class(TWPDocumentBlock)
    Private
      FBlocks : TWPDocumentBlocks;

      Function GetBlocks : TWPDocumentBlocks;
      Procedure SetBlocks(Const Value : TWPDocumentBlocks);

    Protected
      Function GetChildren : TWPDocumentObjects; Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TWPDocumentContainer;
      Function Clone : TWPDocumentContainer;

      Procedure Assign(oObject : TFslObject); Override;
      Procedure ClearChildren; Overload; Override;

      Function GetOwnerFor(oObject : TWPDocumentObject): TWPDocumentObject; Overload; Override;

      Function HasBlocks : Boolean;

    published
      {
        The content of this container
      }
      Property Blocks : TWPDocumentBlocks Read GetBlocks Write SetBlocks;
  End;

  TWPDocumentContainers = Class(TWPDocumentBlocks)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentContainer;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentContainer);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentContainer; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentContainers;
      Function Clone : TWPDocumentContainers;

      Function New : TWPDocumentContainer; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPDocumentContainer Read GetElement Write SetElement; Default;
  End;

  {
    The type of the break - a drawn line, or a page break.
  }
  TWPDocumentBreakBreakType = (BreakTypeLine, BreakTypePageBreak);

  {
    a break - either a line drawn across the page, or a page break (if applicable)
  }
  TWPDocumentBreak = Class(TWPDocumentBlock)
    Private
      FBreakType : TWPDocumentBreakBreakType;
      FAlignment : TWordProcessorAlignment;
      FWidth : Real;
      FPenColour : TColour;
      FPenWidth : Word;
      FPenStyle : TFslPenStyle;
      FEndStyle : TFslPenEndStyle;

    Public
      Function Link : TWPDocumentBreak; Overload;
      Function Clone : TWPDocumentBreak; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Procedure Defaults; Overload; Override;
      Function AsText : String; Overload; Override;

    published
      {
        What kind of break - whether just a line drawn on the page, or a page break
      }
      Property BreakType : TWPDocumentBreakBreakType Read FBreakType Write FBreakType;
      {
        the alignment of the break - if drawn
      }
      Property Alignment : TWordProcessorAlignment Read FAlignment Write FAlignment;
      {
        the width of the break on the page - if drawn
      }
      Property Width : Real Read FWidth Write FWidth;
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
        the end style of the break - if drawn
      }
      Property EndStyle : TFslPenEndStyle Read FEndStyle Write FEndStyle;
  End;

  TWPDocumentBreaks = Class(TWPDocumentBlocks)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentBreak;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentBreak);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentBreak; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentBreaks; Overload;
      Function Clone : TWPDocumentBreaks; Overload;

      Function New : TWPDocumentBreak; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPDocumentBreak Read GetElement Write SetElement; Default;
  End;


  {
    How a section displays - no visual hint, a thin line, a line with a title in it
  }
  TWPDocumentSectionDisplay = (DisplayNone, DisplayLine, DisplayTitle);

  {
    a section - contains other blocks.

    The purpose of a section is to allow the document to be broken into
    identifiable named parts
  }
  TWPDocumentSection = Class(TWPDocumentContainer)
    Private
      FTitle : String;
      FDisplay : TWPDocumentSectionDisplay;
      FDeletable : Boolean;
      FIsField : Boolean;
      FKey : String;
      FData : TWPDataItemMap;
    function GetDataValue(const sKey: String): String;
    procedure SetDataValue(const sKey, sValue: String);

    Protected
      Function CanHaveHotspot : Boolean; Override;
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TWPDocumentSection; Overload;
      Function Clone : TWPDocumentSection; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Procedure Defaults; Overload; Override;
      Function AsText : String; Overload; Override;

      Property RawData : TWPDataItemMap read FData; // not for use outside word processor
      Property Deletable : Boolean Read FDeletable Write FDeletable;
      Property IsField : Boolean Read FIsField Write FIsField;
      Property Key : String Read FKey Write FKey;
      {
        It's possible to associated a series of name = value pairs with the section.
        These are only persisted in the native format
      }
      Property DataValue[Const sKey : String] : String Read GetDataValue Write SetDataValue;
      {
        Check whether a data value with the name provided exists
      }
      Function HasDataValue(Const sKey : String) : Boolean;
      {
        Scrub any data value associated with that name
      }
      Procedure DeleteDataValue(Const sKey : String);
    Published
      {
        the title of the section (only really useful if display = DisplayTitle)
      }
      Property Title : String Read FTitle Write FTitle;
      {
        how the section is displayed - not at all, as a thing grey line, or with the title shown
      }
      Property Display : TWPDocumentSectionDisplay Read FDisplay Write FDisplay;
  End;

  TWPDocumentSections = Class(TWPDocumentContainers)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentSection;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentSection);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByName(pA, pB : Pointer) : Integer; Overload; Virtual;

      Function Get(Const aValue : Integer) : TWPDocumentSection; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentSections; Overload;
      Function Clone : TWPDocumentSections; Overload;

      Function New : TWPDocumentSection; Reintroduce; Overload; Virtual;

      Function IndexByName(Const aValue : String) : Integer; Overload; Virtual;
      Function GetByName(Const aValue : String) : TWPDocumentSection; Overload; Virtual;
      Function ExistsByName(Const aValue : String) : Boolean; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPDocumentSection Read GetElement Write SetElement; Default;
  End;

  {
    an item in a table - row or cell
  }
  TWPDocumentTableItem = Class(TWPDocumentBlock)
    Private
      {
        NOTE: for inner cell, the border width represent the desired border width
        of this cell and its neighbour. As such, the real border width contribute
        by the settings in this cell is only half of the settings value
        (i.e. A border width of 6 mean a 3 width border created in the inside of this cell)
      }
      FLeftBorder : TWPBorder;
      FTopBorder : TWPBorder;
      FRightBorder : TWPBorder;
      FBottomBorder : TWPBorder;
      FBackground : TColour;

      Function GetLeftBorder : TWPBorder;
      Procedure SetLeftBorder(Const Value : TWPBorder);
      Function GetTopBorder : TWPBorder;
      Procedure SetTopBorder(Const Value : TWPBorder);
      Function GetRightBorder : TWPBorder;
      Procedure SetRightBorder(Const Value : TWPBorder);
      Function GetBottomBorder : TWPBorder;
      Procedure SetBottomBorder(Const Value : TWPBorder);
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPDocumentTableItem; Overload;
      Function Clone : TWPDocumentTableItem; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Procedure Defaults; Overload; Override;

      Function HasLeftBorder : Boolean;
      Function HasTopBorder : Boolean;
      Function HasRightBorder : Boolean;
      Function HasBottomBorder : Boolean;

    Published
      {
        The style of the left border of this row or cell
      }
      Property LeftBorder : TWPBorder Read GetLeftBorder Write SetLeftBorder;
      {
        The style of the Top border of this row or cell
      }
      Property TopBorder : TWPBorder Read GetTopBorder Write SetTopBorder;
      {
        The style of the Right border of this row or cell
      }
      Property RightBorder : TWPBorder Read GetRightBorder Write SetRightBorder;
      {
        The style of the bottom border of this row or cell
      }
      Property BottomBorder : TWPBorder Read GetBottomBorder Write SetBottomBorder;
      {
        The background colour of the row or cell
      }
      Property Background : TColour Read FBackground Write FBackground;
  End;

  TWPDocumentTableItems = Class(TWPDocumentBlocks)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentTableItem;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentTableItem);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentTableItem; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentTableItems; Overload;
      Function Clone : TWPDocumentTableItems; Overload;

      Function New : TWPDocumentTableItem; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPDocumentTableItem Read GetElement Write SetElement; Default;
  End;

  {
    an cell in a table. may only contain paragraphs
  }
  TWPDocumentTableCell = Class(TWPDocumentTableItem)
    Private
      FSpan : Word;
      FWidth : Real;
      FMarginLeft : Word;
      FMarginTop : Word;
      FMarginRight : Word;
      FMarginBottom : Word;
      FParagraphs : TWPDocumentParagraphs;
      FVerticalAlignment : TWordProcessorVerticalAlignment;

      Function GetParagraphs : TWPDocumentParagraphs;
      Procedure SetParagraphs(Const Value : TWPDocumentParagraphs);

    Protected
      Function GetChildren : TWPDocumentObjects; Override;
      Function CanHaveHotspot : Boolean; Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TWPDocumentTableCell;
      Function Clone : TWPDocumentTableCell;

      Procedure Assign(oObject : TFslObject); Override;
      Procedure ClearChildren; Overload; Override;

      Function GetOwnerFor(oObject : TWPDocumentObject): TWPDocumentObject; Overload; Override;

      Function HasParagraphs : Boolean;
      Procedure Defaults; Override;

      Function AsText : String; Override;


    Published
      {
        How many columns this cell spans
      }
      Property Span : Word Read FSpan Write FSpan;
      {
        the width of the Cell in %. 0 is default - calculate based on content.

        When printing the document directly, the renderer takes this as a guide, not an absolute value
      }
      Property Width : Real Read FWidth Write FWidth;
      {
        The inner left margin on the cell
      }
      Property MarginLeft : Word Read FMarginLeft Write FMarginLeft;
      {
        The inner top margin on the cell
      }
      Property MarginTop : Word Read FMarginTop Write FMarginTop;
      {
        The inner right margin on the cell
      }
      Property MarginRight : Word Read FMarginRight Write FMarginRight;
      {
        The inner bottom margin on the cell
      }
      Property MarginBottom : Word Read FMarginBottom Write FMarginBottom;
      {
        The paragraphs in the cell

        Note that the paragraphs can have list styles, but the cells cannot
        contain other tables
      }
      Property Paragraphs : TWPDocumentParagraphs Read GetParagraphs Write SetParagraphs;
      {
        The vertical alignment of the content in the cell
      }
      Property VerticalAlignment : TWordProcessorVerticalAlignment Read FVerticalAlignment Write FVerticalAlignment;
  End;

  {
    Contents of a row in a table
  }
  TWPDocumentTableCells = Class(TWPDocumentTableItems)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentTableCell;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentTableCell);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentTableCell; Reintroduce;

    Public
      Function Link : TWPDocumentTableCells; Overload;
      Function Clone : TWPDocumentTableCells; Overload;

      Function New : TWPDocumentTableCell; Reintroduce;

    {
      Add a Cell to the end of the list.
    }
    Function Append : TWPDocumentTableCell;
    {
      Add an already existing Cell to the end of the list.
    }
    Procedure AddItem(value : TWPDocumentTableCell);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TWPDocumentTableCell) : Integer;
    {
       Insert Cell before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TWPDocumentTableCell;
    {
       Insert an existing Cell before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TWPDocumentTableCell);
    {
       Get the iIndexth Cell. (0 = first item)
    }
    Function Item(iIndex : Integer) : TWPDocumentTableCell;
    {
       Set the iIndexth Cell. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TWPDocumentTableCell);
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
    Property Elements[Const iIndex : Integer] : TWPDocumentTableCell Read GetElement Write SetElement; Default;
  End;

  TWPDocumentTableRows = Class;

  {
    an row in a table. may only contain cells
  }
  TWPDocumentTableRow = Class(TWPDocumentTableItem)
    Private
      FHeader : Boolean;
      FBreakBefore : Boolean;
      FLowerPaddingSize : Integer;
      FLowerPaddingColour : TColour;

      FCells : TWPDocumentTableCells;
      FRows : TWPDocumentTableRows;

      Function GetCells : TWPDocumentTableCells;
      Procedure SetCells(Const Value : TWPDocumentTableCells);

      Function GetRows : TWPDocumentTableRows;
      Procedure SetRows(Const Value : TWPDocumentTableRows);

    Protected
      Function GetChildren : TWPDocumentObjects; Overload; Override;
      Function CanHaveHotspot : Boolean; Overload; Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPDocumentTableRow; Overload;
      Function Clone : TWPDocumentTableRow; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;
      Procedure Defaults; Overload; Override;
      Procedure ClearChildren; Overload; Override;

      Function GetOwnerFor(oObject : TWPDocumentObject): TWPDocumentObject; Overload; Override;

      Function HasCells : Boolean;
      Function HasRows : Boolean;
      Function AsText : String; Overload; Override;


      Property BreakBefore : Boolean Read FBreakBefore Write FBreakBefore;

      // Nested Rows. Nested Rows are not nested with in the table row,
      // but contained by the table and rendered as normal rows below.
      // Nested Rows are "contained" by this row for the purposes of
      // table indenting and document folding
      Property Rows : TWPDocumentTableRows Read GetRows Write SetRows;
    published
      {
        The list of cells in the row. Rows can contain different number of cells
      }
      Property Cells : TWPDocumentTableCells Read GetCells Write SetCells;
      {
        Whether this row is a header (when printing, repeated on each page) (usually only makes sense on the first row)
      }
      Property Header : Boolean Read FHeader Write FHeader;
      {
        Space to add below all the cells
      }
      Property LowerPaddingSize : Integer Read FLowerPaddingSize Write FLowerPaddingSize;
      {
        Colour of the padding space
      }
      Property LowerPaddingColour : TColour Read FLowerPaddingColour Write FLowerPaddingColour;
  End;

  {
    Contents of a table
  }
  TWPDocumentTableRows = Class(TWPDocumentTableItems)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentTableRow;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentTableRow);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentTableRow; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentTableRows; Overload;
      Function Clone : TWPDocumentTableRows; Overload;

      Function New : TWPDocumentTableRow; Reintroduce; Overload; Virtual;

      Function OwnsRow(oRow : TWPDocumentTableRow) : Boolean;

    {
      Add a Row to the end of the list.
    }
    Function Append : TWPDocumentTableRow;
    {
      Add an already existing Row to the end of the list.
    }
    Procedure AddItem(value : TWPDocumentTableRow);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TWPDocumentTableRow) : Integer;
    {
       Insert Row before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TWPDocumentTableRow;
    {
       Insert an existing Row before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TWPDocumentTableRow);
    {
       Get the iIndexth Row. (0 = first item)
    }
    Function Item(iIndex : Integer) : TWPDocumentTableRow;
    {
       Set the iIndexth Row. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TWPDocumentTableRow);
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
    Property Elements[Const iIndex : Integer] : TWPDocumentTableRow Read GetElement Write SetElement; Default;
  End;

  {
    What kind of border the table has.

    Custom means that each border is specified individually.
    Do not use Fancy
  }
  TWPDocumentTableBorderPolicy = (BorderPolicyNone, BorderPolicyGrid, BorderPolicyLines, BorderPolicyInnerLines, BorderPolicyBox, BorderPolicyBoxLines, BorderPolicyCustom, BorderPolicyFancy, BorderPolicyDots, BorderPolicyInnerDots);

  {
    a table. may only contain rows
  }
  TWPDocumentTable = Class(TWPDocumentTableItem)
    Private
      FCenterHorizontalBorder : TWPBorder;
      FCenterVerticalBorder : TWPBorder;
      FBorderPolicy : TWPDocumentTableBorderPolicy;
      FRows : TWPDocumentTableRows;
      FHorizontalMargin : Word;
      FVerticalMargin : Word;
      FExpandLastColumn : Boolean;

      Function GetCenterHorizontalBorder : TWPBorder;
      Procedure SetCenterHorizontalBorder(Const Value : TWPBorder);
      Function GetCenterVerticalBorder : TWPBorder;
      Procedure SetCenterVerticalBorder(Const Value : TWPBorder);
      Function GetRows : TWPDocumentTableRows;
      Procedure SetRows(Const Value : TWPDocumentTableRows);
    Protected
      Function GetChildren : TWPDocumentObjects; Overload; Override;
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPDocumentTable; Overload;
      Function Clone : TWPDocumentTable; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;
      Procedure ClearChildren; Overload; Override;

      Procedure BorderPolicyGrid;
      Procedure BorderPolicyBox;

      Function GetOwnerFor(oObject : TWPDocumentObject): TWPDocumentObject; Overload; Override;
      Function OwnsRow(oRow : TWPDocumentTableRow) : Boolean;

      Function HasCenterHorizontalBorder : Boolean;
      Function HasCenterVerticalBorder : Boolean;
      Function HasRows : Boolean;
      Function HasNonEmptyRows : Boolean;

      Function AsText : String; Overload; Override;
      Procedure Defaults; Overload; Override;


    Published
      {
        The default border to use between rows
      }
      Property CenterHorizontalBorder : TWPBorder Read GetCenterHorizontalBorder Write SetCenterHorizontalBorder;
      {
        The default border to use between columns
      }
      Property CenterVerticalBorder : TWPBorder Read GetCenterVerticalBorder Write SetCenterVerticalBorder;
      {
        A border policy that applies to the whole table. Setting this will clear any existing border
        settings. If you change any border setings after setting this, they will overrule the setting
        where applied
      }
      Property BorderPolicy : TWPDocumentTableBorderPolicy Read FBorderPolicy Write FBorderPolicy;
      {
        The left and right margin of the table (equivalent to paragraph indenting)
      }
      Property HorizontalMargin : Word Read FHorizontalMargin Write FHorizontalMargin;
      {
        The top and bottom margin of the table
      }
      Property VerticalMargin : Word Read FVerticalMargin Write FVerticalMargin;
      {
        Whether to expan out the last column to fill the available space
      }
      Property ExpandLastColumn : Boolean read FExpandLastColumn write FExpandLastColumn;
      {
        The rows in the table
      }
      Property Rows : TWPDocumentTableRows Read GetRows Write SetRows;
  End;

  TWPDocumentTables = Class(TWPDocumentTableItems)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentTable;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentTable);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentTable; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentTables; Overload;
      Function Clone : TWPDocumentTables; Overload;

      Function New : TWPDocumentTable; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPDocumentTable Read GetElement Write SetElement; Default;
  End;


  {
    an row in a table. may only contain cells
  }
  TWPDocument = Class(TWPDocumentContainer)
    Private
      FStyles : TWPStyles;
      FAllowedWords : TFslStringList;
      FAnnotations: TWPDocumentAnnotations;
      FAttachments: TWPDocumentAttachments;

      Function GetStyles : TWPStyles;
      Procedure SetStyles(Const Value : TWPStyles);
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPDocument; Overload;
      Function Clone : TWPDocument; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Procedure Defaults; Overload; Override;

      Function Empty : Boolean; Overload; Virtual;
      Function AsText : String; Overload; Override;

      Function HasStyles : Boolean;

      {
        Remove any sections out of the document, while keeping all the content
      }
      Procedure StripSections;

      {
        The styles defined for this document
      }
      Property Styles : TWPStyles Read GetStyles Write SetStyles;
      Property AllowedWords : TFslStringList read FAllowedWords;
      Property Annotations : TWPDocumentAnnotations read FAnnotations;
      Property Attachments : TWPDocumentAttachments read FAttachments;
  End;

  TWPDocuments = Class(TWPDocumentContainers)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocument;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocument);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocument; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocuments; Overload;
      Function Clone : TWPDocuments; Overload;

      Function New : TWPDocument; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPDocument Read GetElement Write SetElement; Default;
  End;

Type
  TWordProcessorDocument = TWPDocument;

Const
  NAMES_WPDocumentSectionDISPLAYTYPE : Array [TWPDocumentSectionDisplay] Of String = ('None', 'Line', 'Name');
  WPDocumentBreakTYPE_NAMES : Array [TWPDocumentBreakBreakType] Of String = ('Line', 'PageBreak');
  NAMES_TWPDocumentTableBORDERPOLICY : Array [TWPDocumentTableBorderPolicy] Of String = ('None', 'Grid', 'Horizontal Lines', 'Inner Horizontal Lines', 'Horizontal Dotted Lines', 'Inner Horizontal Dotted Lines', 'Outer Box', 'Outer Box With Horizontal Lines', 'Custom', 'Fancy');
  CODES_TWPDocumentTableBORDERPOLICY : Array [TWPDocumentTableBorderPolicy] Of String = ('None', 'Grid', 'Lines', 'InnerLines', 'Dots', 'InnerDots', 'Box', 'BoxLines', 'Custom', 'Fancy');

Type
  {
    Factory for document parts
  }
  TWPDocumentFactory = class (TFslObject)
  Public
    {
      create a TWPSFontDetails
    }
    function createFontStyle : TWPSFontDetails;
    {
      create a TWPSParagraphDetails
    }
    function createParagraphStyle : TWPSParagraphDetails;
    {
      create a TWPStyle
    }
    function createStyle : TWPStyle;
    {
      create a TWPHotspot
    }
    function createHotspot : TWPHotspot;
    {
      create a TWPDocumentText
    }
    function createText : TWPDocumentText;
    {
      create a TWPDocumentImage
    }
    function createImage : TWPDocumentImage;
    {
      create a TWPDocumentField
    }
    function createField : TWPDocumentField;
    {
      create a TWPDocumentLineBreak
    }
    function createLineBreak : TWPDocumentLineBreak;
    {
      create a TWPDocumentBlock
    }
    function createBlock : TWPDocumentBlock;
    {
      create a TWPDocumentBreak
    }
    function createBreak : TWPDocumentBreak;
    {
      create a TWPDocumentContainer
    }
    function createContainer : TWPDocumentContainer;
    {
      create a TWPDocumentParagraph
    }
    function createParagraph : TWPDocumentParagraph;
    {
      create a TWPDocumentSection
    }
    function createSection : TWPDocumentSection;
    {
      create a TWPBorder
    }
    function createBorder : TWPBorder;
    {
      create a TWPDocumentTableItem
    }
    function createTableItem : TWPDocumentTableItem;
    {
      create a TWPDocumentTableCell
    }
    function createCell : TWPDocumentTableCell;
    {
      create a TWPDocumentTableRow
    }
    function createRow : TWPDocumentTableRow;
    {
      create a TWPDocumentTable
    }
    function createTable : TWPDocumentTable;
    {
      create a TWPDocumentDocument
    }
    function createDocument : TWPDocument;

  End;


Type
  TWPDocumentIteratorContextItem = TWPDocumentObject;
  TWPDocumentIteratorContextIndex = Integer;

  TWPDocumentIteratorContext = Class(TFslObject)
    Private
      FItem : TWPDocumentIteratorContextItem;
      FIndex : TWPDocumentIteratorContextIndex;

      Procedure SetItem(Const Value : TWPDocumentIteratorContextItem);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPDocumentIteratorContext; Overload;
      Function Clone : TWPDocumentIteratorContext; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Property Item : TWPDocumentIteratorContextItem Read FItem Write SetItem;
      Property Index : TWPDocumentIteratorContextIndex Read FIndex Write FIndex;
  End;

  TWPDocumentIteratorContexts = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentIteratorContext;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentIteratorContext);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentIteratorContext; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentIteratorContexts; Overload;
      Function Clone : TWPDocumentIteratorContexts; Overload;

      Function New : TWPDocumentIteratorContext; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPDocumentIteratorContext Read GetElement Write SetElement; Default;
  End;

  TWPDocumentIteratorContextStack = Class(TWPDocumentIteratorContexts)
    Public
      Procedure Push(oObject : TWPDocumentObject); Overload; Virtual;
      Procedure Pop; Overload; Virtual;

      Function HasCurrent : Boolean; Overload; Virtual;
      Function Current : TWPDocumentIteratorContext; Overload; Virtual;
  End;


  TWPDocumentIterator = Class (TFslObject)
    Private
      FDocument : TWPDocument;
      FStack : TWPDocumentIteratorContextStack;

      Function GetDocument : TWPDocument;
      Procedure SetDocument(Const Value : TWPDocument);
    Protected
      Function Included(oObject : TWPDocumentObject) : Boolean; Overload; Virtual;
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure First; Overload; Virtual;
      Procedure Next; Overload; Virtual;

      Function Current : TWPDocumentObject; Overload; Virtual;

      Function More : Boolean; Overload; Virtual;

      Function HasDocument : Boolean; Overload; Virtual;

      Property Document : TWPDocument Read GetDocument Write SetDocument;
  End;

  TWPDocumentValidationProblem = Class(TFslObject)
    Private
      FMessage: String;
      FObject : TWPDocumentObject;
      Function GetObject: TWPDocumentObject;
      Procedure SetObject(Const Value: TWPDocumentObject);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPDocumentValidationProblem; Overload;
      Function Clone : TWPDocumentValidationProblem; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Function Summary : String;
      Property Message : String Read FMessage Write FMessage;
      Property Object_ : TWPDocumentObject Read GetObject Write SetObject;
  End;

  TWPDocumentValidationProblems = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentValidationProblem;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentValidationProblem);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentValidationProblem; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentValidationProblems; Overload;
      Function Clone : TWPDocumentValidationProblems; Overload;

      Function New : TWPDocumentValidationProblem; Reintroduce; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPDocumentValidationProblem Read GetElement Write SetElement; Default;
  End;

  TWPDocumentValidator = Class (TFslObject)
    Private
      FProblems : TWPDocumentValidationProblems;

      Procedure Problem(oObject : TWPDocumentObject; Const sMessage : String);

    Protected
      Procedure ValidateTableRow(oTableRow : TWPDocumentTableRow);
      Procedure ValidateTable(oTable : TWPDocumentTable);
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Procedure Validate(oDocument : TWPDocument); Overload; Virtual;

      Function Summary : String;

      Class Procedure ValidateAssert(oDocument : TWPDocument);

      Property Problems : TWPDocumentValidationProblems Read FProblems;
  End;

  EWPDocumentValidator = Class (EWPException)
    Private
      FProblems : TWPDocumentValidationProblems;
    Public
      destructor Destroy; Overload; Override;

      Property Problems : TWPDocumentValidationProblems Read FProblems;
  End;


Implementation


Constructor TWPDocument.Create;
Begin
  Inherited;
  FAllowedWords := TFslStringList.Create;
  FStyles := TWPStyles.Create;
  FAnnotations := TWPDocumentAnnotations.Create;
  FAttachments := TWPDocumentAttachments.Create;
End;


Destructor TWPDocument.Destroy;
Begin
  FAllowedWords.Free;
  FStyles.Free;
  FAnnotations.Free;
  FAttachments.Free;

  Inherited;
End;


Function TWPDocument.Link : TWPDocument;
Begin
  Result := TWPDocument(Inherited Link);
End;


Function TWPDocument.Clone : TWPDocument;
Begin
  Result := TWPDocument(Inherited Clone);
End;


Procedure TWPDocument.Assign(oObject : TFslObject);
Begin
  Inherited;

  Styles.Assign(TWPDocument(oObject).Styles);
  AllowedWords.Assign(TWPDocument(oObject).AllowedWords);
  Annotations.Assign(TWPDocument(oObject).Annotations);
  Attachments.Assign(TWPDocument(oObject).Attachments);
End;


Function TWPDocument.Empty : Boolean;
Begin
  Result := Blocks.Count = 0;
End;


Function TWPDocument.GetStyles : TWPStyles;
Begin
  Assert(Invariants('GetStyles', FStyles, TWPStyles, 'Styles'));
  Result := FStyles;
End;


Procedure TWPDocument.SetStyles(Const Value : TWPStyles);
Begin
  FStyles.Free;
  FStyles := Value;
End;


Function TWPDocument.HasStyles : Boolean;
Begin
  Result := Assigned(FStyles);
End;


Function TWPDocument.AsText : String;
Begin
  Result := Blocks.AsText;
End;


function TWPDocument.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FStyles.sizeInBytes);
  inc(result, FAllowedWords.sizeInBytes);
  inc(result, FAnnotations.sizeInBytes);
  inc(result, FAttachments.sizeInBytes);
end;

Function TWPDocuments.Link : TWPDocuments;
Begin
  Result := TWPDocuments(Inherited Link);
End;


Function TWPDocuments.Clone : TWPDocuments;
Begin
  Result := TWPDocuments(Inherited Clone);
End;


Function TWPDocuments.New : TWPDocument;
Begin
  Result := TWPDocument(Inherited New);
End;


Function TWPDocuments.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocument;
End;


Function TWPDocuments.GetElement(Const iIndex : Integer) : TWPDocument;
Begin
  Result := TWPDocument(ObjectByIndex[iIndex]);
End;


Procedure TWPDocuments.SetElement(Const iIndex : Integer; Const oValue : TWPDocument);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocuments.Get(Const aValue : Integer) : TWPDocument;
Begin
  Result := TWPDocument(Inherited Get(aValue));
End;


Procedure TWPDocument.Defaults;
Begin
  Inherited;
  AllowedWords.Clear;
  Annotations.Clear;
  Attachments.Clear;
End;

Constructor TWPDocumentContainer.Create;
Begin
  Inherited;

  FBlocks := TWPDocumentBlocks.Create;
End;


Destructor TWPDocumentContainer.Destroy;
Begin
  FBlocks.Free;

  Inherited;
End;


Function TWPDocumentContainer.Link : TWPDocumentContainer;
Begin
  Result := TWPDocumentContainer(Inherited Link);
End;


Function TWPDocumentContainer.Clone : TWPDocumentContainer;
Begin
  Result := TWPDocumentContainer(Inherited Clone);
End;


Procedure TWPDocumentContainer.Assign(oObject : TFslObject);
Begin
  Inherited;

  Blocks.Assign(TWPDocumentContainer(oObject).Blocks);
End;



Function TWPDocumentContainer.GetChildren : TWPDocumentObjects;
Begin
  Result := FBlocks;
End;


Function TWPDocumentContainer.GetBlocks : TWPDocumentBlocks;
Begin
  Assert(Invariants('GetBlocks', FBlocks, TWPDocumentBlocks, 'Blocks'));
  Result := FBlocks;
End;


Procedure TWPDocumentContainer.SetBlocks(Const Value : TWPDocumentBlocks);
Begin
  FBlocks.Free;
  FBlocks := Value;
End;


Function TWPDocumentContainer.HasBlocks : Boolean;
Begin
  Result := Assigned(FBlocks);
End;


function TWPDocumentContainer.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FBlocks.sizeInBytes);
end;

Function TWPDocumentContainers.Link : TWPDocumentContainers;
Begin
  Result := TWPDocumentContainers(Inherited Link);
End;


Function TWPDocumentContainers.Clone : TWPDocumentContainers;
Begin
  Result := TWPDocumentContainers(Inherited Clone);
End;


Function TWPDocumentContainers.New : TWPDocumentContainer;
Begin
  Result := TWPDocumentContainer(Inherited New);
End;


Function TWPDocumentContainers.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentContainer;
End;


Function TWPDocumentContainers.GetElement(Const iIndex : Integer) : TWPDocumentContainer;
Begin
  Result := TWPDocumentContainer(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentContainers.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentContainer);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentContainers.Get(Const aValue : Integer) : TWPDocumentContainer;
Begin
  Result := TWPDocumentContainer(Inherited Get(aValue));
End;


Procedure TWPDocumentContainer.ClearChildren;
Begin
  Inherited;
  FBlocks.Clear;
End;

Function TWPDocumentContainer.GetOwnerFor(oObject: TWPDocumentObject): TWPDocumentObject;
Var
  iLoop : Integer;
Begin
  Result := Nil;
  iLoop := 0;
  While (Result = Nil) And (iLoop < FBlocks.Count) Do
  Begin
    If FBlocks[iLoop] = oObject Then
      Result := Self
    Else
      Result := FBlocks[iLoop].GetOwnerFor(oObject);
    Inc(iLoop);
  End;
End;

Function TWPDocumentBlock.Link : TWPDocumentBlock;
Begin
  Result := TWPDocumentBlock(Inherited Link);
End;


Function TWPDocumentBlock.Clone : TWPDocumentBlock;
Begin
  Result := TWPDocumentBlock(Inherited Clone);
End;


Procedure TWPDocumentBlock.Assign(oObject : TFslObject);
Begin
  Inherited;

  ReadOnly := TWPDocumentBlock(oObject).ReadOnly;
End;

Function TWPDocumentBlocks.Link : TWPDocumentBlocks;
Begin
  Result := TWPDocumentBlocks(Inherited Link);
End;


Function TWPDocumentBlocks.Clone : TWPDocumentBlocks;
Begin
  Result := TWPDocumentBlocks(Inherited Clone);
End;


Function TWPDocumentBlocks.New : TWPDocumentBlock;
Begin
  Result := TWPDocumentBlock(Inherited New);
End;


Function TWPDocumentBlocks.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentBlock;
End;


Function TWPDocumentBlocks.GetElement(Const iIndex : Integer) : TWPDocumentBlock;
Begin
  Result := TWPDocumentBlock(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentBlocks.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentBlock);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentBlocks.Get(Const aValue : Integer) : TWPDocumentBlock;
Begin
  Result := TWPDocumentBlock(Inherited Get(aValue));
End;


Procedure TWPDocumentBlock.Defaults;
Begin
  Inherited;
  FReadOnly := ReadOnlyDefault;
End;


Constructor TWPDocumentObject.Create;
Begin
  Inherited;

  FHotspot := Nil;
End;


Destructor TWPDocumentObject.Destroy;
Begin
  FHotspot.Free;

  Inherited;
End;


Procedure TWPDocumentObject.AfterConstruction;
Begin
  Inherited;

  Defaults;
End;


Procedure TWPDocumentObject.Assign(oObject: TFslObject);
Begin
  Inherited;

  Hotspot := TWPDocumentObject(oObject).FHotspot.Clone;
  Namespace := TWPDocumentObject(oObject).Namespace;
  Name := TWPDocumentObject(oObject).Name;
End;



Procedure TWPDocumentObject.Defaults;
Begin
End;


Function TWPDocumentObject.Link : TWPDocumentObject;
Begin
  Result := TWPDocumentObject(Inherited Link);
End;


Function TWPDocumentObject.Clone : TWPDocumentObject;
Begin
  Result := TWPDocumentObject(Inherited Clone);
End;


Function TWPDocumentObject.CanHaveHotspot: Boolean;
Begin
  Result := False;
End;


Function TWPDocumentObject.GetHasHotSpot: Boolean;
Begin
  Result := Assigned(FHotspot);
End;


Procedure TWPDocumentObject.SetHasHotspot(Const bValue: Boolean);
Begin
  If Not bValue Then
    Hotspot := Nil
  Else If Not Assigned(FHotspot) Then
    Hotspot := TWPHotspot.Create;
End;


Procedure TWPDocumentObject.SetHotspot(Const Value: TWPHotspot);
Begin
  Assert(CheckCondition((value = Nil) Or CanHaveHotspot, 'SetHotspot', 'Document Elements of type '+ClassName+' cannot act as hotspots'));
  FHotspot.Free;
  FHotSpot := Value;
End;


Function TWPDocumentObject.ErrorClass : EFslExceptionClass;
Begin
  Result := EWPException;
End;


Function TWPDocumentObject.AsText : String;
Begin
  Result := '<Error>';
End;


function TWPDocumentObject.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FNamespace.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FHotspot.sizeInBytes);
end;

Function TWPDocumentObjects.Link : TWPDocumentObjects;
Begin
  Result := TWPDocumentObjects(Inherited Link);
End;


Function TWPDocumentObjects.Clone : TWPDocumentObjects;
Begin
  Result := TWPDocumentObjects(Inherited Clone);
End;


Function TWPDocumentObjects.New : TWPDocumentObject;
Begin
  Result := TWPDocumentObject(Inherited New);
End;


Function TWPDocumentObjects.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentObject;
End;


Function TWPDocumentObjects.GetElement(Const iIndex : Integer) : TWPDocumentObject;
Begin
  Result := TWPDocumentObject(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentObjects.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentObject);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentObjects.Get(Const aValue : Integer) : TWPDocumentObject;
Begin
  Result := TWPDocumentObject(Inherited Get(aValue));
End;


Function TWPDocumentObjects.ErrorClass : EFslExceptionClass;
Begin
  Result := EWPException;
End;


Function TWPDocumentObjects.AsText : String;
Var
  iLoop : Integer;
Begin
  // this is a slow way to do it, but it's only for debugging
  Result := '';
  For iLoop := 0 To Count - 1 Do
    Result := Result + Elements[iLoop].AsText;
End;


Function TWPDocumentObject.GetChildren : TWPDocumentObjects;
Begin
  Result := Nil;
End;


Function TWPDocumentObject.ChildCount : Integer;
Var
  FChildren : TWPDocumentObjects;
Begin
  FChildren := GetChildren;
  If Assigned(FChildren) Then
    Result := FChildren.Count
  Else
    Result := 0;
End;


Function TWPDocumentObject.Child(iIndex : Integer) : TWPDocumentObject;
Var
  FChildren : TWPDocumentObjects;
Begin
  FChildren := GetChildren;

  If Assigned(FChildren) Then
  Begin
    Result := FChildren[iIndex];
  End
  Else
  Begin
    Result := Nil;
    RaiseError('Child', 'Error - no children available');
  End;
End;


Procedure TWPDocumentObjects.AddAllCloned(oSource : TWPDocumentObjects);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oSource.Count - 1 Do
    Add(oSource[iLoop].Clone);
End;


Procedure TWPDocumentObject.Clear;
Begin
  Defaults;
  FHotspot.Free;
  FHotspot := Nil;
  ClearChildren;
End;

Procedure TWPDocumentObject.ClearChildren;
Begin
  // nothing;
End;

Function TWPDocumentObject.GetOwnerFor(oObject: TWPDocumentObject): TWPDocumentObject;
Begin
  Result := Nil;
End;

Function TWPDocumentObject.GetNamePair: String;
Begin
  If FNamespace <> '' Then
    Result := FNamespace +'::'+FName
  Else
    Result := FName;
End;

Procedure TWPDocumentObject.SetNamePair(Const Value: String);
Var
  i : Integer;
Begin
  i := StringFind(value, '::');
  If (i = 0) Then
    FName := Value
  Else
  Begin
    FNamespace := Copy(Value, 1, i-1);
    FName := Copy(Value, i+2, MAXINT);
  End;
End;




Constructor TWPDocumentField.Create;
Begin
  Inherited;
  FData := TWPDataItemMap.Create;
  FContents := TWPDocumentSimpleContents.Create;
End;


Destructor TWPDocumentField.Destroy;
Begin
  FContents.Free;
  FData.Free;

  Inherited;
End;


Function TWPDocumentField.Link : TWPDocumentField;
Begin
  Result := TWPDocumentField(Inherited Link);
End;


Function TWPDocumentField.Clone : TWPDocumentField;
Begin
  Result := TWPDocumentField(Inherited Clone);
End;


Procedure TWPDocumentField.Assign(oObject : TFslObject);
Begin
  Inherited;

  ReadOnly := TWPDocumentField(oObject).ReadOnly;
  Width := TWPDocumentField(oObject).Width;
  CheckedIndex := TWPDocumentField(oObject).CheckedIndex;
  Deletable := TWPDocumentField(oObject).Deletable;
  FData.Assign(TWPDocumentField(oObject).FData);
  FixedFormat := TWPDocumentField(oObject).FixedFormat;
  Contents.Assign(TWPDocumentField(oObject).Contents);
End;



Procedure TWPDocumentField.Defaults;
Begin
  Inherited;
  FReadOnly := ReadOnlyDefault;
  FDeletable := True;
  FFixedFormat := fffAnyPart;
  FWidth := 0;
  FCheckedIndex := 0;
  // no change data
End;


Function TWPDocumentField.GetContents : TWPDocumentSimpleContents;
Begin
  Assert(Invariants('GetContents', FContents, TWPDocumentSimpleContents, 'Contents'));
  Result := FContents;
End;


Procedure TWPDocumentField.SetContents(Const Value : TWPDocumentSimpleContents);
Begin
  FContents.Free;
  FContents := Value;
End;


Function TWPDocumentField.HasContents : Boolean;
Begin
  Result := Assigned(FContents);
End;


Function TWPDocumentField.AsText : String;
Begin
  Result := '[('+Name+') '+Contents.AsText+']';
End;


function TWPDocumentField.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FData.sizeInBytes);
  inc(result, FContents.sizeInBytes);
end;

Function TWPDocumentFields.Link : TWPDocumentFields;
Begin
  Result := TWPDocumentFields(Inherited Link);
End;


Function TWPDocumentFields.Clone : TWPDocumentFields;
Begin
  Result := TWPDocumentFields(Inherited Clone);
End;


Function TWPDocumentFields.New : TWPDocumentField;
Begin
  Result := TWPDocumentField(Inherited New);
End;


Function TWPDocumentFields.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentField;
End;


Function TWPDocumentFields.GetElement(Const iIndex : Integer) : TWPDocumentField;
Begin
  Result := TWPDocumentField(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentFields.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentField);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentFields.Get(Const aValue : Integer) : TWPDocumentField;
Begin
  Result := TWPDocumentField(Inherited Get(aValue));
End;


Function TWPDocumentField.GetChildren : TWPDocumentObjects;
Begin
  Result := FContents;
End;


Function TWPDocumentField.CanHaveHotspot: Boolean;
Begin
  Result := True;
End;

Procedure TWPDocumentField.ClearChildren;
Begin
  Inherited;
  FContents.Clear;
End;


Function TWPDocumentField.GetOwnerFor(oObject: TWPDocumentObject): TWPDocumentObject;
Var
  iLoop : Integer;
Begin
  Result := Nil;
  iLoop := 0;
  While (Result = Nil) And (iLoop < FContents.Count) Do
  Begin
    If FContents[iLoop] = oObject Then
      Result := Self
    Else
      Result := FContents[iLoop].GetOwnerFor(oObject);
    Inc(iLoop);
  End;
End;

Function TWPDocumentField.GetDataValue(Const sKey: String): String;
Begin
  Result := FData.GetValueByKey(sKey);
End;

Function TWPDocumentField.HasDataValue(Const sKey: String): Boolean;
Begin
  Result := FData.ExistsByKey(sKey);
End;

Procedure TWPDocumentField.SetDataValue(Const sKey, sValue: String);
Begin
  FData.SetValueByKey(sKey, sValue);
End;

Procedure TWPDocumentField.DeleteDataValue(Const sKey: String);
Begin
  FData.DeleteByKey(sKey);
End;

Function TWPDocumentSimpleContent.Link : TWPDocumentSimpleContent;
Begin
  Result := TWPDocumentSimpleContent(Inherited Link);
End;


Function TWPDocumentSimpleContent.Clone : TWPDocumentSimpleContent;
Begin
  Result := TWPDocumentSimpleContent(Inherited Clone);
End;


Function TWPDocumentSimpleContents.Link : TWPDocumentSimpleContents;
Begin
  Result := TWPDocumentSimpleContents(Inherited Link);
End;


Function TWPDocumentSimpleContents.Clone : TWPDocumentSimpleContents;
Begin
  Result := TWPDocumentSimpleContents(Inherited Clone);
End;


Function TWPDocumentSimpleContents.New : TWPDocumentSimpleContent;
Begin
  Result := TWPDocumentSimpleContent(Inherited New);
End;


Function TWPDocumentSimpleContents.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentSimpleContent;
End;


Function TWPDocumentSimpleContents.GetElement(Const iIndex : Integer) : TWPDocumentSimpleContent;
Begin
  Result := TWPDocumentSimpleContent(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentSimpleContents.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentSimpleContent);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentSimpleContents.Get(Const aValue : Integer) : TWPDocumentSimpleContent;
Begin
  Result := TWPDocumentSimpleContent(Inherited Get(aValue));
End;

Constructor TWPDocumentContent.Create;
Begin
  Inherited;

  Font := TWPSFontDetails.Create;
End;


Destructor TWPDocumentContent.Destroy;
Begin
  FFont.Free;

  Inherited;
End;


Function TWPDocumentContent.Link : TWPDocumentContent;
Begin
  Result := TWPDocumentContent(Inherited Link);
End;


Function TWPDocumentContent.Clone : TWPDocumentContent;
Begin
  Result := TWPDocumentContent(Inherited Clone);
End;


Procedure TWPDocumentContent.Assign(oObject : TFslObject);
Begin
  Inherited;

  Annotation := TWPDocumentContent(oObject).Annotation;
  Style := TWPDocumentContent(oObject).Style;
  Font.Assign(TWPDocumentContent(oObject).Font);

End;



Procedure TWPDocumentContent.Defaults;
Begin
  Inherited;
  FStyle := '';
  FFont.Defaults;
End;


Function TWPDocumentContent.GetFont : TWPSFontDetails;
Begin
  Assert(Invariants('GetFont', FFont, TWPSFontDetails, 'Font'));

  Result := FFont;
End;


Procedure TWPDocumentContent.SetFont(Const Value : TWPSFontDetails);
Begin
  FFont.Free;
  FFont := Value;
End;


Function TWPDocumentContent.HasFont : Boolean;
Begin
  Result := Assigned(FFont);
End;


function TWPDocumentContent.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FStyle.length * sizeof(char)) + 12);
  inc(result, FFont.sizeInBytes);
end;

Function TWPDocumentContents.Link : TWPDocumentContents;
Begin
  Result := TWPDocumentContents(Inherited Link);
End;


Function TWPDocumentContents.Clone : TWPDocumentContents;
Begin
  Result := TWPDocumentContents(Inherited Clone);
End;


Function TWPDocumentContents.New : TWPDocumentContent;
Begin
  Result := TWPDocumentContent(Inherited New);
End;


Function TWPDocumentContents.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentContent;
End;


Function TWPDocumentContents.GetElement(Const iIndex : Integer) : TWPDocumentContent;
Begin
  Result := TWPDocumentContent(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentContents.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentContent);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentContents.Get(Const aValue : Integer) : TWPDocumentContent;
Begin
  Result := TWPDocumentContent(Inherited Get(aValue));
End;


Function TWPDocumentSection.Link : TWPDocumentSection;
Begin
  Result := TWPDocumentSection(Inherited Link);
End;


Function TWPDocumentSection.Clone : TWPDocumentSection;
Begin
  Result := TWPDocumentSection(Inherited Clone);
End;


Procedure TWPDocumentSection.Assign(oObject : TFslObject);
Begin
  Inherited;

  Title := TWPDocumentSection(oObject).Title;
  Display := TWPDocumentSection(oObject).Display;
  Deletable := TWPDocumentSection(oObject).Deletable;
  IsField := TWPDocumentSection(oObject).IsField;
  Key := TWPDocumentSection(oObject).Key;
  FData.Assign(TWPDocumentSection(oObject).FData);
End;


Function TWPDocumentSection.AsText : String;
Begin
  Result := '====' +Name+' : '+Title+' =============================================='+cReturn + Blocks.AsText;
End;


function TWPDocumentSection.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FTitle.length * sizeof(char)) + 12);
  inc(result, (FKey.length * sizeof(char)) + 12);
  inc(result, FData.sizeInBytes);
end;

Function TWPDocumentSections.Link : TWPDocumentSections;
Begin
  Result := TWPDocumentSections(Inherited Link);
End;


Function TWPDocumentSections.Clone : TWPDocumentSections;
Begin
  Result := TWPDocumentSections(Inherited Clone);
End;


Function TWPDocumentSections.New : TWPDocumentSection;
Begin
  Result := TWPDocumentSection(Inherited New);
End;


Function TWPDocumentSections.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentSection;
End;


Function TWPDocumentSections.GetElement(Const iIndex : Integer) : TWPDocumentSection;
Begin
  Result := TWPDocumentSection(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentSections.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentSection);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentSections.CompareByName(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(TWPDocumentSection(pA).Name, TWPDocumentSection(pB).Name);
End;


Function TWPDocumentSections.IndexByName(Const aValue : String) : Integer;
Var
  oElement : TWPDocumentSection;
Begin
  oElement := New;
  Try
    oElement.Name := aValue;

    If Not Find(oElement, Result, CompareByName) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;


Function TWPDocumentSections.Get(Const aValue : Integer) : TWPDocumentSection;
Begin
  Result := TWPDocumentSection(Inherited Get(aValue));
End;


Function TWPDocumentSections.GetByName(Const aValue : String) : TWPDocumentSection;
Begin
  Result := Get(IndexByName(aValue));
End;


Function TWPDocumentSections.ExistsByName(Const aValue : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexByName(aValue));
End;


Procedure TWPDocumentSection.Defaults;
Begin
  Inherited;
  FTitle := '';
  FDisplay := DisplayNone;
End;

Function TWPDocumentSection.CanHaveHotspot: Boolean;
Begin
  Result := IsField;
End;

Constructor TWPDocumentSection.Create;
Begin
  Inherited;
  FData := TWPDataItemMap.Create;
End;

Destructor TWPDocumentSection.Destroy;
Begin
  FData.Free;
  Inherited;
End;

Procedure TWPDocumentSection.DeleteDataValue(Const sKey: String);
Begin
  FData.DeleteByKey(sKey);
End;

Function TWPDocumentSection.GetDataValue(Const sKey: String): String;
Begin
  Result := FData.GetValueByKey(sKey);
End;

Function TWPDocumentSection.HasDataValue(Const sKey: String): Boolean;
Begin
  Result := FData.ExistsByKey(sKey);
End;

Procedure TWPDocumentSection.SetDataValue(Const sKey, sValue: String);
Begin
  FData.SetValueByKey(sKey, sValue);
End;


Constructor TWPDocumentText.Create(Const sValue : String);
Begin
  Create;
  Value := sValue;
End;


Function TWPDocumentText.Link : TWPDocumentText;
Begin
  Result := TWPDocumentText(Inherited Link);
End;


Function TWPDocumentText.Clone : TWPDocumentText;
Begin
  Result := TWPDocumentText(Inherited Clone);
End;


Procedure TWPDocumentText.Assign(oObject : TFslObject);
Begin
  Inherited;

  Value := TWPDocumentText(oObject).Value;
  DrawnFont := TWPDocumentText(oObject).DrawnFont;
End;



Function TWPDocumentText.AsText : String;
Begin
  Result := FValue;
End;


Procedure TWPDocumentText.SetValue(Const Value : String);
Begin
  If StringExists(value, [#13, #10]) Then
    RaiseError('SetValue', 'Illegal character in WP text content - cannot accept CR/LF');

  FValue := StringReplace(Value, #9, StringMultiply(' ', TAB_CHAR_COUNT));
End;


function TWPDocumentText.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
  inc(result, (FDrawnFont.length * sizeof(char)) + 12);
end;

Function TWPDocumentTexts.Link : TWPDocumentTexts;
Begin
  Result := TWPDocumentTexts(Inherited Link);
End;


Function TWPDocumentTexts.Clone : TWPDocumentTexts;
Begin
  Result := TWPDocumentTexts(Inherited Clone);
End;


Function TWPDocumentTexts.New : TWPDocumentText;
Begin
  Result := TWPDocumentText(Inherited New);
End;


Function TWPDocumentTexts.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentText;
End;


Function TWPDocumentTexts.GetElement(Const iIndex : Integer) : TWPDocumentText;
Begin
  Result := TWPDocumentText(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentTexts.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentText);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentTexts.Get(Const aValue : Integer) : TWPDocumentText;
Begin
  Result := TWPDocumentText(Inherited Get(aValue));
End;



Constructor TWPDocumentImage.Create;
Begin
  Inherited;

  FAdornments := TWPDocumentImageAdornments.Create;
  FImage := Nil;
  FSelectionImage := Nil;
  FMap := Nil;
End;


Destructor TWPDocumentImage.Destroy;
Begin
  FImage.Free;
  FSelectionImage.Free;
  FMap.Free;
  FAdornments.Free;

  Inherited;
End;


Function TWPDocumentImage.Link : TWPDocumentImage;
Begin
  Result := TWPDocumentImage(Inherited Link);
End;


Function TWPDocumentImage.Clone : TWPDocumentImage;
Begin
  Result := TWPDocumentImage(Inherited Clone);
End;


Procedure TWPDocumentImage.Assign(oObject : TFslObject);
Begin
  Inherited;

  Name := TWPDocumentImage(oObject).Name;
  BorderWidth := TWPDocumentImage(oObject).BorderWidth;
  BorderColour := TWPDocumentImage(oObject).BorderColour;
  ImageWidth := TWPDocumentImage(oObject).ImageWidth;
  ImageHeight := TWPDocumentImage(oObject).ImageHeight;
  SizePolicy  := TWPDocumentImage(oObject).SizePolicy;
  FrameIndex  := TWPDocumentImage(oObject).FrameIndex;
  VerticalAlignment := TWPDocumentImage(oObject).VerticalAlignment;
  FTransparentColour := TWPDocumentImage(oObject).FTransparentColour;

  If TWPDocumentImage(oObject).HasImage Then
    Image := TWPDocumentImage(oObject).Image.Clone
  Else
    Image := Nil;
  If TWPDocumentImage(oObject).HasSelectionImage Then
    SelectionImage := TWPDocumentImage(oObject).SelectionImage.Clone
  Else
    SelectionImage := Nil;
  FAdornments.Assign(TWPDocumentImage(oObject).FAdornments);
End;



Procedure TWPDocumentImage.Defaults;
Begin
  Inherited;

  BorderColour := DEF_COLOUR;
  TransparentColour := DEF_COLOUR;
  FName := '';
  FBorderWidth := 0;
  FImageWidth := DEF_WORD;
  FImageHeight := DEF_WORD;
  FSizePolicy := ImageSizeManual;
  FVerticalAlignment := ImageVerticalAlignmentBaseLine;
  FFrameIndex := 0;
End;


Function TWPDocumentImage.GetImage : TFslGraphic;
Begin
  Assert(Invariants('GetImage', FImage, TFslGraphic, 'Image'));

  Result := FImage;
End;


Procedure TWPDocumentImage.SetImage(Const Value : TFslGraphic);
Begin
  Assert(Not Assigned(Value) Or Invariants('SetImage', Value, TFslGraphic, 'Value'));

  FImage.Free;
  FImage := Value;
End;


Function TWPDocumentImage.HasImage : Boolean;
Begin
  Result := Assigned(FImage);
End;


Function TWPDocumentImage.GetSelectionImage : TFslGraphic;
Begin
  Assert(Invariants('GetSelectionImage', FSelectionImage, TFslGraphic, 'SelectionImage'));

  Result := FSelectionImage;
End;


Procedure TWPDocumentImage.SetSelectionImage(Const Value : TFslGraphic);
Begin
  Assert(Not Assigned(Value) Or Invariants('SetSelectionImage', Value, TFslGraphic, 'Value'));

  FSelectionImage.Free;
  FSelectionImage := Value;
End;


Function TWPDocumentImage.HasSelectionImage : Boolean;
Begin
  Result := Assigned(FSelectionImage);
End;


Function TWPDocumentImage.GetMap : TWPImageMap;
Begin
  Assert(Invariants('GetMap', FMap, TWPImageMap, 'Map'));

  Result := FMap;
End;


Procedure TWPDocumentImage.SetMap(Const Value : TWPImageMap);
Begin
  Assert(Not Assigned(Value) Or Invariants('SetMap', Value, TWPImageMap, 'Value'));

  FMap.Free;
  FMap := Value;
End;



Function TWPDocumentImage.GetHasMap : Boolean;
Begin
  Result := Assigned(FMap);
End;


Procedure TWPDocumentImage.SetHasMap(bValue : Boolean);
Begin
  If Not bValue Then
    Map := Nil
  Else If Not Assigned(FMap) Then
    FMap := TWPImageMap.Create;
End;


Function TWPDocumentImage.AsText : String;
Begin
  Result := '<img ' +Name + '>';
End;


function TWPDocumentImage.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FTitle.length * sizeof(char)) + 12);
  inc(result, FImage.sizeInBytes);
  inc(result, FSelectionImage.sizeInBytes);
  inc(result, FMap.sizeInBytes);
  inc(result, FAdornments.sizeInBytes);
end;

Function TWPDocumentImages.Link : TWPDocumentImages;
Begin
  Result := TWPDocumentImages(Inherited Link);
End;


Function TWPDocumentImages.Clone : TWPDocumentImages;
Begin
  Result := TWPDocumentImages(Inherited Clone);
End;


Function TWPDocumentImages.New : TWPDocumentImage;
Begin
  Result := TWPDocumentImage(Inherited New);
End;


Function TWPDocumentImages.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentImage;
End;


Function TWPDocumentImages.GetElement(Const iIndex : Integer) : TWPDocumentImage;
Begin
  Result := TWPDocumentImage(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentImages.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentImage);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentImages.Get(Const aValue : Integer) : TWPDocumentImage;
Begin
  Result := TWPDocumentImage(Inherited Get(aValue));
End;


Function TWPDocumentImage.CanHaveHotspot: Boolean;
Begin
  Result := True;
End;

Procedure TWPDocumentImage.Clear;
Begin
  Inherited;

  FImage.Free;
  FImage := Nil;
  FSelectionImage.Free;
  FSelectionImage := Nil;
  FMap.Free;
  FMap := Nil;
  FFrameIndex := 0;
  Defaults;
End;



Function TWPDocumentBreak.Link : TWPDocumentBreak;
Begin
  Result := TWPDocumentBreak(Inherited Link);
End;


Function TWPDocumentBreak.Clone : TWPDocumentBreak;
Begin
  Result := TWPDocumentBreak(Inherited Clone);
End;


Procedure TWPDocumentBreak.Assign(oObject : TFslObject);
Begin
  Inherited;

  Alignment := TWPDocumentBreak(oObject).Alignment;
  BreakType := TWPDocumentBreak(oObject).BreakType;
  Width := TWPDocumentBreak(oObject).Width;
  PenColour := TWPDocumentBreak(oObject).PenColour;
  PenWidth := TWPDocumentBreak(oObject).PenWidth;
  PenStyle := TWPDocumentBreak(oObject).PenStyle;
  EndStyle := TWPDocumentBreak(oObject).EndStyle;
End;



Function TWPDocumentBreak.AsText : String;
Begin
  Result := '<break>';
End;

Function TWPDocumentBreaks.Link : TWPDocumentBreaks;
Begin
  Result := TWPDocumentBreaks(Inherited Link);
End;


Function TWPDocumentBreaks.Clone : TWPDocumentBreaks;
Begin
  Result := TWPDocumentBreaks(Inherited Clone);
End;


Function TWPDocumentBreaks.New : TWPDocumentBreak;
Begin
  Result := TWPDocumentBreak(Inherited New);
End;


Function TWPDocumentBreaks.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentBreak;
End;


Function TWPDocumentBreaks.GetElement(Const iIndex : Integer) : TWPDocumentBreak;
Begin
  Result := TWPDocumentBreak(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentBreaks.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentBreak);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentBreaks.Get(Const aValue : Integer) : TWPDocumentBreak;
Begin
  Result := TWPDocumentBreak(Inherited Get(aValue));
End;



Procedure TWPDocumentBreak.Defaults;
Begin
  Inherited;
  FBreakType := BreakTypePageBreak;
  FAlignment := WordProcessorAlignmentUnknown;
  FWidth := 1;
  FPenColour := DEF_COLOUR;
  FPenWidth := DEF_WORD;
  FPenStyle := apsNone;
  FEndStyle := apesSquare;
End;


Function TWPDocumentAnnotation.Link : TWPDocumentAnnotation;
Begin
  Result := TWPDocumentAnnotation(Inherited Link);
End;


Function TWPDocumentAnnotation.Clone : TWPDocumentAnnotation;
Begin
  Result := TWPDocumentAnnotation(Inherited Clone);
End;


Procedure TWPDocumentAnnotation.Assign(oObject : TFslObject);
Begin
  Inherited;

  FOwner := TWPDocumentAnnotation(oObject).FOwner;
  FText := TWPDocumentAnnotation(oObject).FText;
End;



function TWPDocumentAnnotation.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FOwner.length * sizeof(char)) + 12);
  inc(result, (FText.length * sizeof(char)) + 12);
end;

Function TWPDocumentAnnotations.Link : TWPDocumentAnnotations;
Begin
  Result := TWPDocumentAnnotations(Inherited Link);
End;


Function TWPDocumentAnnotations.Clone : TWPDocumentAnnotations;
Begin
  Result := TWPDocumentAnnotations(Inherited Clone);
End;


Function TWPDocumentAnnotations.New : TWPDocumentAnnotation;
Begin
  Result := TWPDocumentAnnotation(Inherited New);
End;


Function TWPDocumentAnnotations.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentAnnotation;
End;


Function TWPDocumentAnnotations.GetElement(Const iIndex : Integer) : TWPDocumentAnnotation;
Begin
  Result := TWPDocumentAnnotation(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentAnnotations.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentAnnotation);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentAnnotations.Get(Const aValue : Integer) : TWPDocumentAnnotation;
Begin
  Result := TWPDocumentAnnotation(Inherited Get(aValue));
End;


Function TWPDocumentAnnotation.Matches(oOther: TWPDocumentAnnotation): Boolean;
Begin
  Result := (FText = oOther.FText) And (FOwner = oOther.FOwner);
End;

Function TWPDocumentAttachments.Link : TWPDocumentAttachments;
Begin
  Result := TWPDocumentAttachments(Inherited Link);
End;


Function TWPDocumentAttachments.Clone : TWPDocumentAttachments;
Begin
  Result := TWPDocumentAttachments(Inherited Clone);
End;


Function TWPDocumentAttachments.New : TWPDocumentAttachment;
Begin
  Result := TWPDocumentAttachment(Inherited New);
End;


Function TWPDocumentAttachments.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentAttachment;
End;


Function TWPDocumentAttachments.GetElement(Const iIndex : Integer) : TWPDocumentAttachment;
Begin
  Result := TWPDocumentAttachment(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentAttachments.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentAttachment);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentAttachments.Get(Const aValue : Integer) : TWPDocumentAttachment;
Begin
  Result := TWPDocumentAttachment(Inherited Get(aValue));
End;


Function TWPDocumentAttachment.Matches(oOther: TWPDocumentAttachment): Boolean;
Begin
  Result := (FId = oOther.FId) And (FContent.AsText = oOther.FContent.asText);
End;

Function TWPDocumentAttachment.Link : TWPDocumentAttachment;
Begin
  Result := TWPDocumentAttachment(Inherited Link);
End;


Function TWPDocumentAttachment.Clone : TWPDocumentAttachment;
Begin
  Result := TWPDocumentAttachment(Inherited Clone);
End;


Procedure TWPDocumentAttachment.Assign(oObject : TFslObject);
Begin
  Inherited;

  FContent := TWPDocumentAttachment(oObject).Content.Clone;
  FId := TWPDocumentAttachment(oObject).FId;
End;



function TWPDocumentAttachment.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FId.length * sizeof(char)) + 12);
  inc(result, FContent.sizeInBytes);
  inc(result, (FMimeType.length * sizeof(char)) + 12);
  inc(result, (FExtension.length * sizeof(char)) + 12);
end;

Constructor TWPDocumentLineBreak.Create;
Begin
  Inherited;
End;


Destructor TWPDocumentLineBreak.Destroy;
Begin
  Inherited;
End;


Function TWPDocumentLineBreak.Link : TWPDocumentLineBreak;
Begin
  Result := TWPDocumentLineBreak(Inherited Link);
End;


Function TWPDocumentLineBreak.Clone : TWPDocumentLineBreak;
Begin
  Result := TWPDocumentLineBreak(Inherited Clone);
End;


Procedure TWPDocumentLineBreak.Assign(oObject : TFslObject);
Begin
  Inherited;
End;



Function TWPDocumentLineBreak.AsText : String;
Begin
  Result := '<br>';
End;


Function TWPDocumentLineBreaks.Link : TWPDocumentLineBreaks;
Begin
  Result := TWPDocumentLineBreaks(Inherited Link);
End;


Function TWPDocumentLineBreaks.Clone : TWPDocumentLineBreaks;
Begin
  Result := TWPDocumentLineBreaks(Inherited Clone);
End;


Function TWPDocumentLineBreaks.New : TWPDocumentLineBreak;
Begin
  Result := TWPDocumentLineBreak(Inherited New);
End;


Function TWPDocumentLineBreaks.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentLineBreak;
End;


Function TWPDocumentLineBreaks.GetElement(Const iIndex : Integer) : TWPDocumentLineBreak;
Begin
  Result := TWPDocumentLineBreak(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentLineBreaks.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentLineBreak);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentLineBreaks.Get(Const aValue : Integer) : TWPDocumentLineBreak;
Begin
  Result := TWPDocumentLineBreak(Inherited Get(aValue));
End;





Constructor TWPDocumentParagraph.Create;
Begin
  Inherited;

  FFormat := TWPSParagraphDetails.Create;
  FContents := TWPDocumentContents.Create;
  FFont := TWPSFontDetails.Create;
End;


Destructor TWPDocumentParagraph.Destroy;
Begin
  FContents.Free;
  FFormat.Free;
  FFont.Free;

  Inherited;
End;


Function TWPDocumentParagraph.Link : TWPDocumentParagraph;
Begin
  Result := TWPDocumentParagraph(Inherited Link);
End;


Function TWPDocumentParagraph.Clone : TWPDocumentParagraph;
Begin
  Result := TWPDocumentParagraph(Inherited Clone);
End;


Procedure TWPDocumentParagraph.Assign(oObject : TFslObject);
Begin
  Inherited;

  Style := TWPDocumentParagraph(oObject).Style;
  Format.Assign(TWPDocumentParagraph(oObject).Format);
  Font.Assign(TWPDocumentParagraph(oObject).Font);
  Contents.Assign(TWPDocumentParagraph(oObject).Contents);
End;



Procedure TWPDocumentParagraph.Defaults;
Begin
  Inherited;
  FStyle := '';
  Format.Defaults;
  Font.Defaults;
End;


Function TWPDocumentParagraph.GetChildren : TWPDocumentObjects;
Begin
  Result := FContents;
End;

Function TWPDocumentParagraph.GetFont : TWPSFontDetails;
Begin
  Assert(Invariants('GetFont', FFont, TWPSFontDetails, 'Font'));

  Result := FFont;
End;


Procedure TWPDocumentParagraph.SetFont(Const Value : TWPSFontDetails);
Begin
  FFont.Free;
  FFont := Value;
End;


Function TWPDocumentParagraph.HasFont : Boolean;
Begin
  Result := Assigned(FFont);
End;


Function TWPDocumentParagraph.GetFormat : TWPSParagraphDetails;
Begin
  Assert(Invariants('GetFormat', FFormat, TWPSParagraphDetails, 'Format'));

  Result := FFormat;
End;


Procedure TWPDocumentParagraph.SetFormat(Const Value : TWPSParagraphDetails);
Begin
  FFormat.Free;
  FFormat := Value;
End;


Function TWPDocumentParagraph.HasFormat : Boolean;
Begin
  Result := Assigned(FFormat);
End;


Function TWPDocumentParagraph.GetContents : TWPDocumentContents;
Begin
  Assert(Invariants('GetContents', FContents, TWPDocumentContents, 'Contents'));

  Result := FContents;
End;


Procedure TWPDocumentParagraph.SetContents(Const Value : TWPDocumentContents);
Begin
  FContents.Free;
  FContents := Value;
End;


Function TWPDocumentParagraph.HasContents : Boolean;
Begin
  Result := Assigned(FContents);
End;


Function TWPDocumentParagraph.AsText : String;
Begin
  Result := Contents.AsText + cReturn;
End;


function TWPDocumentParagraph.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FStyle.length * sizeof(char)) + 12);
  inc(result, FFormat.sizeInBytes);
  inc(result, FFont.sizeInBytes);
  inc(result, FContents.sizeInBytes);
end;

Function TWPDocumentParagraphs.Link : TWPDocumentParagraphs;
Begin
  Result := TWPDocumentParagraphs(Inherited Link);
End;


Function TWPDocumentParagraphs.Clone : TWPDocumentParagraphs;
Begin
  Result := TWPDocumentParagraphs(Inherited Clone);
End;


Function TWPDocumentParagraphs.New : TWPDocumentParagraph;
Begin
  Result := TWPDocumentParagraph(Inherited New);
End;


Function TWPDocumentParagraphs.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentParagraph;
End;


Function TWPDocumentParagraphs.GetElement(Const iIndex : Integer) : TWPDocumentParagraph;
Begin
  Result := TWPDocumentParagraph(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentParagraphs.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentParagraph);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentParagraphs.Get(Const aValue : Integer) : TWPDocumentParagraph;
Begin
  Result := TWPDocumentParagraph(Inherited Get(aValue));
End;


Procedure TWPDocumentParagraph.ClearChildren;
Begin
  Inherited;
  FContents.Clear;
End;

Function TWPDocumentParagraph.GetOwnerFor(oObject: TWPDocumentObject): TWPDocumentObject;
Var
  iLoop : Integer;
Begin
  Result := Nil;
  iLoop := 0;
  While (Result = Nil) And (iLoop < FContents.Count) Do
  Begin
    If FContents[iLoop] = oObject Then
      Result := Self
    Else
      Result := FContents[iLoop].GetOwnerFor(oObject);
    Inc(iLoop);
  End;
End;

Constructor TWPDocumentTableItem.Create;
Begin
  Inherited;

  FLeftBorder := TWPBorder.Create;
  FTopBorder := TWPBorder.Create;
  FRightBorder := TWPBorder.Create;
  FBottomBorder := TWPBorder.Create;
End;


Destructor TWPDocumentTableItem.Destroy;
Begin
  FLeftBorder.Free;
  FTopBorder.Free;
  FRightBorder.Free;
  FBottomBorder.Free;

  Inherited;
End;


Function TWPDocumentTableItem.Link : TWPDocumentTableItem;
Begin
  Result := TWPDocumentTableItem(Inherited Link);
End;


Function TWPDocumentTableItem.Clone : TWPDocumentTableItem;
Begin
  Result := TWPDocumentTableItem(Inherited Clone);
End;


Procedure TWPDocumentTableItem.Assign(oObject : TFslObject);
Begin
  Inherited;

  LeftBorder.Assign(TWPDocumentTableItem(oObject).LeftBorder);
  TopBorder.Assign(TWPDocumentTableItem(oObject).TopBorder);
  RightBorder.Assign(TWPDocumentTableItem(oObject).RightBorder);
  BottomBorder.Assign(TWPDocumentTableItem(oObject).BottomBorder);
  Background := TWPDocumentTableItem(oObject).Background;
End;



Procedure TWPDocumentTableItem.Defaults;
Begin
  Inherited;
  FLeftBorder.Clear;
  FTopBorder.Clear;
  FRightBorder.Clear;
  FBottomBorder.Clear;
  FBackground := DEF_COLOUR;
End;


Function TWPDocumentTableItem.GetLeftBorder : TWPBorder;
Begin
  Assert(Invariants('GetLeftBorder', FLeftBorder, TWPBorder, 'LeftBorder'));
  Result := FLeftBorder;
End;


Procedure TWPDocumentTableItem.SetLeftBorder(Const Value : TWPBorder);
Begin
  FLeftBorder.Free;
  FLeftBorder := Value;
End;


Function TWPDocumentTableItem.HasLeftBorder : Boolean;
Begin
  Result := Assigned(FLeftBorder);
End;


Function TWPDocumentTableItem.GetTopBorder : TWPBorder;
Begin
  Assert(Invariants('GetTopBorder', FTopBorder, TWPBorder, 'TopBorder'));
  Result := FTopBorder;
End;


Procedure TWPDocumentTableItem.SetTopBorder(Const Value : TWPBorder);
Begin
  FTopBorder.Free;
  FTopBorder := Value;
End;


Function TWPDocumentTableItem.HasTopBorder : Boolean;
Begin
  Result := Assigned(FTopBorder);
End;


Function TWPDocumentTableItem.GetRightBorder : TWPBorder;
Begin
  Assert(Invariants('GetRightBorder', FRightBorder, TWPBorder, 'RightBorder'));
  Result := FRightBorder;
End;


Procedure TWPDocumentTableItem.SetRightBorder(Const Value : TWPBorder);
Begin
  FRightBorder.Free;
  FRightBorder := Value;
End;


Function TWPDocumentTableItem.HasRightBorder : Boolean;
Begin
  Result := Assigned(FRightBorder);
End;


Function TWPDocumentTableItem.GetBottomBorder : TWPBorder;
Begin
  Assert(Invariants('GetBottomBorder', FBottomBorder, TWPBorder, 'BottomBorder'));
  Result := FBottomBorder;
End;


Procedure TWPDocumentTableItem.SetBottomBorder(Const Value : TWPBorder);
Begin
  FBottomBorder.Free;
  FBottomBorder := Value;
End;


Function TWPDocumentTableItem.HasBottomBorder : Boolean;
Begin
  Result := Assigned(FBottomBorder);
End;


function TWPDocumentTableItem.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FLeftBorder.sizeInBytes);
  inc(result, FTopBorder.sizeInBytes);
  inc(result, FRightBorder.sizeInBytes);
  inc(result, FBottomBorder.sizeInBytes);
end;

Function TWPDocumentTableItems.Link : TWPDocumentTableItems;
Begin
  Result := TWPDocumentTableItems(Inherited Link);
End;


Function TWPDocumentTableItems.Clone : TWPDocumentTableItems;
Begin
  Result := TWPDocumentTableItems(Inherited Clone);
End;


Function TWPDocumentTableItems.New : TWPDocumentTableItem;
Begin
  Result := TWPDocumentTableItem(Inherited New);
End;


Function TWPDocumentTableItems.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentTableItem;
End;


Function TWPDocumentTableItems.GetElement(Const iIndex : Integer) : TWPDocumentTableItem;
Begin
  Result := TWPDocumentTableItem(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentTableItems.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentTableItem);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentTableItems.Get(Const aValue : Integer) : TWPDocumentTableItem;
Begin
  Result := TWPDocumentTableItem(Inherited Get(aValue));
End;


Constructor TWPDocumentTableCell.Create;
Begin
  Inherited;

  FParagraphs := TWPDocumentParagraphs.Create;
End;


Destructor TWPDocumentTableCell.Destroy;
Begin
  FParagraphs.Free;

  Inherited;
End;


Function TWPDocumentTableCell.Link : TWPDocumentTableCell;
Begin
  Result := TWPDocumentTableCell(Inherited Link);
End;


Function TWPDocumentTableCell.Clone : TWPDocumentTableCell;
Begin
  Result := TWPDocumentTableCell(Inherited Clone);
End;


Procedure TWPDocumentTableCell.Assign(oObject : TFslObject);
Begin
  Inherited;

  Span := TWPDocumentTableCell(oObject).Span;
  Width := TWPDocumentTableCell(oObject).Width;
  Background := TWPDocumentTableCell(oObject).Background;
  MarginLeft := TWPDocumentTableCell(oObject).MarginLeft;
  MarginTop := TWPDocumentTableCell(oObject).MarginTop;
  MarginRight := TWPDocumentTableCell(oObject).MarginRight;
  MarginBottom := TWPDocumentTableCell(oObject).MarginBottom;
  VerticalAlignment := TWPDocumentTableCell(oObject).VerticalAlignment;
  Paragraphs.Assign(TWPDocumentTableCell(oObject).Paragraphs);
End;



Procedure TWPDocumentTableCell.Defaults;
Begin
  Inherited;

  FSpan := 1;
  FWidth := 0;
  FMarginLeft := DEF_WORD;
  FMarginTop := DEF_WORD;
  FMarginRight := DEF_WORD;
  FMarginBottom := DEF_WORD;
  FVerticalAlignment := VerticalAlignmentTop;
End;


Function TWPDocumentTableCell.GetChildren : TWPDocumentObjects;
Begin
  Result := FParagraphs;
End;


Function TWPDocumentTableCell.AsText : String;
Begin
  Result := Paragraphs.AsText;
End;


Function TWPDocumentTableCell.GetParagraphs : TWPDocumentParagraphs;
Begin
  Assert(Invariants('GetParagraphs', FParagraphs, TWPDocumentParagraphs, 'Paragraphs'));

  Result := FParagraphs;
End;


Procedure TWPDocumentTableCell.SetParagraphs(Const Value : TWPDocumentParagraphs);
Begin
  FParagraphs.Free;
  FParagraphs := Value;
End;


Function TWPDocumentTableCell.HasParagraphs : Boolean;
Begin
  Result := Assigned(FParagraphs);
End;


function TWPDocumentTableCell.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FParagraphs.sizeInBytes);
end;

Function TWPDocumentTableCells.Link : TWPDocumentTableCells;
Begin
  Result := TWPDocumentTableCells(Inherited Link);
End;


Function TWPDocumentTableCells.Clone : TWPDocumentTableCells;
Begin
  Result := TWPDocumentTableCells(Inherited Clone);
End;


Function TWPDocumentTableCells.New : TWPDocumentTableCell;
Begin
  Result := TWPDocumentTableCell(Inherited New);
End;


Function TWPDocumentTableCells.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentTableCell;
End;


Function TWPDocumentTableCells.GetElement(Const iIndex : Integer) : TWPDocumentTableCell;
Begin
  Result := TWPDocumentTableCell(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentTableCells.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentTableCell);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentTableCells.Get(Const aValue : Integer) : TWPDocumentTableCell;
Begin
  Result := TWPDocumentTableCell(Inherited Get(aValue));
End;


Function TWPDocumentTableCell.CanHaveHotspot: Boolean;
Begin
  Result := True;
End;

Procedure TWPDocumentTableCell.ClearChildren;
Begin
  Inherited;
  FParagraphs.Clear;
End;

Function TWPDocumentTableCell.GetOwnerFor(oObject: TWPDocumentObject): TWPDocumentObject;
Var
  iLoop : Integer;
Begin
  Result := Nil;
  iLoop := 0;
  While (Result = Nil) And (iLoop < FParagraphs.Count) Do
  Begin
    If FParagraphs[iLoop] = oObject Then
      Result := Self
    Else
      Result := FParagraphs[iLoop].GetOwnerFor(oObject);
    Inc(iLoop);
  End;
End;

Constructor TWPDocumentTableRow.Create;
Begin
  Inherited;

  FLowerPaddingSize := 0;
  FLowerPaddingColour := DEF_COLOUR;
  FCells := TWPDocumentTableCells.Create;
  FRows := TWPDocumentTableRows.Create;
End;


Destructor TWPDocumentTableRow.Destroy;
Begin
  FRows.Free;
  FCells.Free;

  Inherited;
End;


Function TWPDocumentTableRow.Link : TWPDocumentTableRow;
Begin
  Result := TWPDocumentTableRow(Inherited Link);
End;


Function TWPDocumentTableRow.Clone : TWPDocumentTableRow;
Begin
  Result := TWPDocumentTableRow(Inherited Clone);
End;


Procedure TWPDocumentTableRow.Assign(oObject : TFslObject);
Begin
  Inherited;

  Header := TWPDocumentTableRow(oObject).Header;
  BreakBefore := TWPDocumentTableRow(oObject).BreakBefore;
  Background := TWPDocumentTableRow(oObject).Background;
  LowerPaddingSize := TWPDocumentTableRow(oObject).LowerPaddingSize;
  LowerPaddingColour := TWPDocumentTableRow(oObject).LowerPaddingColour;
  Cells.Assign(TWPDocumentTableRow(oObject).Cells);
  Rows.Assign(TWPDocumentTableRow(oObject).Rows);
End;



Function TWPDocumentTableRow.GetChildren : TWPDocumentObjects;
Begin
  Result := FCells;
End;


Function TWPDocumentTableRow.GetCells : TWPDocumentTableCells;
Begin
  Assert(Invariants('GetCells', FCells, TWPDocumentTableCells, 'Cells'));
  Result := FCells;
End;


Procedure TWPDocumentTableRow.SetCells(Const Value : TWPDocumentTableCells);
Begin
  FCells.Free;
  FCells := Value;
End;


Function TWPDocumentTableRow.HasCells : Boolean;
Begin
  Result := Assigned(FCells);
End;


Function TWPDocumentTableRow.GetRows : TWPDocumentTableRows;
Begin
  Assert(Invariants('GetRows', FRows, TWPDocumentTableRows, 'Rows'));
  Result := FRows;
End;


Procedure TWPDocumentTableRow.SetRows(Const Value : TWPDocumentTableRows);
Begin
  FRows.Free;
  FRows := Value;
End;


Function TWPDocumentTableRow.HasRows : Boolean;
Begin
  Result := Assigned(FRows);
End;


Function TWPDocumentTableRow.AsText : String;
Var
  iLoop : Integer;
Begin
  Result := '+'+Cells.AsText+'+';
  For iLoop := 0 To FRows.Count - 1 Do
    Result := Result + FRows[iLoop].asText;
End;

function TWPDocumentTableRow.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCells.sizeInBytes);
  inc(result, FRows.sizeInBytes);
end;

Function TWPDocumentTableRows.Link : TWPDocumentTableRows;
Begin
  Result := TWPDocumentTableRows(Inherited Link);
End;


Function TWPDocumentTableRows.Clone : TWPDocumentTableRows;
Begin
  Result := TWPDocumentTableRows(Inherited Clone);
End;


Function TWPDocumentTableRows.New : TWPDocumentTableRow;
Begin
  Result := TWPDocumentTableRow(Inherited New);
End;


Function TWPDocumentTableRows.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentTableRow;
End;


Function TWPDocumentTableRows.GetElement(Const iIndex : Integer) : TWPDocumentTableRow;
Begin
  Result := TWPDocumentTableRow(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentTableRows.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentTableRow);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentTableRows.Get(Const aValue : Integer) : TWPDocumentTableRow;
Begin
  Result := TWPDocumentTableRow(Inherited Get(aValue));
End;


Function TWPDocumentTableRow.CanHaveHotspot: Boolean;
Begin
  Result := True;
End;


Function TWPDocumentTableRows.OwnsRow(oRow : TWPDocumentTableRow) : Boolean;
Var
  iTableRowIndex : Integer;
  oTableRow : TWPDocumentTableRow;
Begin
  Result := False;
  iTableRowIndex := 0;

  While Not Result And (iTableRowIndex < Count) Do
  Begin
    oTableRow := Elements[iTableRowIndex];

    Result := (oTableRow = oRow) Or oTableRow.Rows.OwnsRow(oRow);

    Inc(iTableRowIndex);
  End;
End;


Procedure TWPDocumentTableRow.ClearChildren;
Begin
  Inherited;
  FCells.Clear;
  FRows.Clear;
End;


Procedure TWPDocumentTableRow.Defaults;
Begin
  Inherited;
  FHeader := False;
  BreakBefore := False;
  FLowerPaddingSize := 0;
  FLowerPaddingColour := DEF_COLOUR;
End;

Function TWPDocumentTableRow.GetOwnerFor(oObject: TWPDocumentObject): TWPDocumentObject;
Var
  iLoop : Integer;
Begin
  Result := Nil;
  iLoop := 0;
  While (Result = Nil) And (iLoop < FCells.Count) Do
  Begin
    If FCells[iLoop] = oObject Then
      Result := Self
    Else
      Result := FCells[iLoop].GetOwnerFor(oObject);
    Inc(iLoop);
  End;

  iLoop := 0;
  While (Result = Nil) And (iLoop < FRows.Count) Do
  Begin
    If FRows[iLoop] = oObject Then
      Result := Self
    Else
      Result := FRows[iLoop].GetOwnerFor(oObject);
    Inc(iLoop);
  End;
End;

Constructor TWPDocumentTable.Create;
Begin
  Inherited;

  FCenterHorizontalBorder := TWPBorder.Create;
  FCenterVerticalBorder := TWPBorder.Create;
  FRows := TWPDocumentTableRows.Create;
End;


Destructor TWPDocumentTable.Destroy;
Begin
  FCenterHorizontalBorder.Free;
  FCenterVerticalBorder.Free;
  FRows.Free;

  Inherited;
End;


Function TWPDocumentTable.Link : TWPDocumentTable;
Begin
  Result := TWPDocumentTable(Inherited Link);
End;


Function TWPDocumentTable.Clone : TWPDocumentTable;
Begin
  Result := TWPDocumentTable(Inherited Clone);
End;


Procedure TWPDocumentTable.Assign(oObject : TFslObject);
Begin
  Inherited;

  CenterHorizontalBorder.Assign(TWPDocumentTable(oObject).CenterHorizontalBorder);
  CenterVerticalBorder.Assign(TWPDocumentTable(oObject).CenterVerticalBorder);
  BorderPolicy := TWPDocumentTable(oObject).BorderPolicy;
  HorizontalMargin := TWPDocumentTable(oObject).HorizontalMargin;
  VerticalMargin := TWPDocumentTable(oObject).VerticalMargin;
  ExpandLastColumn := TWPDocumentTable(oObject).ExpandLastColumn;
  Rows.Assign(TWPDocumentTable(oObject).Rows);
End;



Procedure TWPDocumentTable.Defaults;
Begin
  Inherited;
  FCenterHorizontalBorder.Clear;
  FCenterVerticalBorder.Clear;
  FHorizontalMargin := DEF_WORD;
  FVerticalMargin := DEF_WORD;
  FBorderPolicy := BorderPolicyNone;
  FExpandLastColumn := false;
End;


Function TWPDocumentTable.GetChildren : TWPDocumentObjects;
Begin
  Result := FRows;
End;


Function TWPDocumentTable.GetCenterHorizontalBorder : TWPBorder;
Begin
  Assert(Invariants('GetCenterHorizontalBorder', FCenterHorizontalBorder, TWPBorder, 'CenterHorizontalBorder'));
  Result := FCenterHorizontalBorder;
End;


Procedure TWPDocumentTable.SetCenterHorizontalBorder(Const Value : TWPBorder);
Begin
  FCenterHorizontalBorder.Free;
  FCenterHorizontalBorder := Value;
End;


Function TWPDocumentTable.HasCenterHorizontalBorder : Boolean;
Begin
  Result := Assigned(FCenterHorizontalBorder);
End;


Function TWPDocumentTable.GetCenterVerticalBorder : TWPBorder;
Begin
  Assert(Invariants('GetCenterVerticalBorder', FCenterVerticalBorder, TWPBorder, 'CenterVerticalBorder'));
  Result := FCenterVerticalBorder;
End;


Procedure TWPDocumentTable.SetCenterVerticalBorder(Const Value : TWPBorder);
Begin
  FCenterVerticalBorder.Free;
  FCenterVerticalBorder := Value;
End;


Function TWPDocumentTable.HasCenterVerticalBorder : Boolean;
Begin
  Result := Assigned(FCenterVerticalBorder);
End;


Function TWPDocumentTable.GetRows : TWPDocumentTableRows;
Begin
  Assert(Invariants('GetRows', FRows, TWPDocumentTableRows, 'Rows'));
  Result := FRows;
End;


Procedure TWPDocumentTable.SetRows(Const Value : TWPDocumentTableRows);
Begin
  FRows.Free;
  FRows := Value;
End;


Function TWPDocumentTable.HasRows : Boolean;
Begin
  Result := Assigned(FRows);
End;


Function TWPDocumentTable.AsText : String;
Begin
  Result := '#'+Rows.AsText+'#';
End;


function TWPDocumentTable.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCenterHorizontalBorder.sizeInBytes);
  inc(result, FCenterVerticalBorder.sizeInBytes);
  inc(result, FRows.sizeInBytes);
end;

Function TWPDocumentTables.Link : TWPDocumentTables;
Begin
  Result := TWPDocumentTables(Inherited Link);
End;


Function TWPDocumentTables.Clone : TWPDocumentTables;
Begin
  Result := TWPDocumentTables(Inherited Clone);
End;


Function TWPDocumentTables.New : TWPDocumentTable;
Begin
  Result := TWPDocumentTable(Inherited New);
End;


Function TWPDocumentTables.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentTable;
End;


Function TWPDocumentTables.GetElement(Const iIndex : Integer) : TWPDocumentTable;
Begin
  Result := TWPDocumentTable(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentTables.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentTable);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentTables.Get(Const aValue : Integer) : TWPDocumentTable;
Begin
  Result := TWPDocumentTable(Inherited Get(aValue));
End;


Function TWPDocumentTable.OwnsRow(oRow: TWPDocumentTableRow): Boolean;
Begin
  Result := FRows.OwnsRow(oRow);
End;


Procedure TWPDocumentTable.ClearChildren;
Begin
  Inherited;
  FRows.Clear;
End;

Function TWPDocumentTable.GetOwnerFor(oObject: TWPDocumentObject): TWPDocumentObject;
Var
  iLoop : Integer;
Begin
  Result := Nil;
  iLoop := 0;
  While (Result = Nil) And (iLoop < FRows.Count) Do
  Begin
    If FRows[iLoop] = oObject Then
      Result := Self
    Else
      Result := FRows[iLoop].GetOwnerFor(oObject);
    Inc(iLoop);
  End;
End;


Procedure TWPDocumentTable.BorderPolicyBox;
Begin
  BorderPolicy := wp_document.BorderPolicyBox;
End;


Procedure TWPDocumentTable.BorderPolicyGrid;
Begin
  BorderPolicy := wp_document.BorderPolicyGrid;
End;


Function TWPDocumentTable.HasNonEmptyRows: Boolean;
Var
  iLoop : Integer;
Begin
  Result := False;
  If HasRows Then
    For iLoop := 0 To FRows.Count -1 Do
      If FRows[iLoop].HasCells And (FRows[iLoop].Cells.Count > 0) Then
       Result := True;
  // deliberate choice not to check children rows - wouldn't know what to do with them anyway
End;

{ TWPDocumentFactory }

function TWPDocumentFactory.createBlock: TWPDocumentBlock;
begin
  result := TWPDocumentBlock.Create;
end;

function TWPDocumentFactory.createBorder: TWPBorder;
begin
  result := TWPBorder.Create;
end;

function TWPDocumentFactory.createBreak: TWPDocumentBreak;
begin
  result := TWPDocumentBreak.Create;
end;

function TWPDocumentFactory.createCell: TWPDocumentTableCell;
begin
  result := TWPDocumentTableCell.Create;
end;

function TWPDocumentFactory.createContainer: TWPDocumentContainer;
begin
  result := TWPDocumentContainer.Create;
end;

function TWPDocumentFactory.createDocument: TWPDocument;
begin
  result := TWPDocument.Create;
end;

function TWPDocumentFactory.createField: TWPDocumentField;
begin
  result := TWPDocumentField.Create;
end;

function TWPDocumentFactory.createFontStyle: TWPSFontDetails;
begin
  result := TWPSFontDetails.Create;
end;

function TWPDocumentFactory.createHotspot: TWPHotspot;
begin
  result := TWPHotspot.Create;
end;

function TWPDocumentFactory.createImage: TWPDocumentImage;
begin
  result := TWPDocumentImage.Create;
end;

function TWPDocumentFactory.createLineBreak: TWPDocumentLineBreak;
begin
  result := TWPDocumentLineBreak.Create;
end;

function TWPDocumentFactory.createParagraph: TWPDocumentParagraph;
begin
  result := TWPDocumentParagraph.Create;
end;

function TWPDocumentFactory.createParagraphStyle: TWPSParagraphDetails;
begin
  result := TWPSParagraphDetails.Create;
end;

function TWPDocumentFactory.createRow: TWPDocumentTableRow;
begin
  result := TWPDocumentTableRow.Create;
end;

function TWPDocumentFactory.createSection: TWPDocumentSection;
begin
  result := TWPDocumentSection.Create;
end;

function TWPDocumentFactory.createStyle: TWPStyle;
begin
  result := TWPStyle.Create;
end;

function TWPDocumentFactory.createTable: TWPDocumentTable;
begin
  result := TWPDocumentTable.Create;
end;

function TWPDocumentFactory.createTableItem: TWPDocumentTableItem;
begin
  result := TWPDocumentTableItem.Create;
end;

function TWPDocumentFactory.createText: TWPDocumentText;
begin
  result := TWPDocumentText.Create;
end;


{ TWPDocumentContents }

Procedure TWPDocumentContents.AddItem(value : TWPDocumentContent);
Begin
  Add(value.Link);
End;

Function TWPDocumentContents.IndexOf(value : TWPDocumentContent) : Integer;
Begin
  result := IndexByReference(value);
End;

Procedure TWPDocumentContents.InsertItem(iIndex : Integer; value : TWPDocumentContent);
begin
  Inherited Insert(iIndex, value);
End;

Function TWPDocumentContents.Item(iIndex : Integer) : TWPDocumentContent;
Begin
  Result := Elements[iIndex];
End;

Procedure TWPDocumentContents.SetItemByIndex(iIndex : Integer; value: TWPDocumentContent);
Begin
  Elements[iIndex] := value;
End;

Procedure TWPDocumentContents.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TWPDocumentContents.ClearItems;
Begin
  Clear;
End;

function TWPDocumentContents.Count: Integer;
begin
  result := Inherited Count;
end;


{ TWPDocumentSimpleContents }

Procedure TWPDocumentSimpleContents.AddItem(value : TWPDocumentSimpleContent);
Begin
  Add(value.Link);
End;

Function TWPDocumentSimpleContents.IndexOf(value : TWPDocumentSimpleContent) : Integer;
Begin
  result := IndexByReference(value);
End;

Procedure TWPDocumentSimpleContents.InsertItem(iIndex : Integer; value : TWPDocumentSimpleContent);
begin
  Inherited Insert(iIndex, value);
End;

Function TWPDocumentSimpleContents.Item(iIndex : Integer) : TWPDocumentSimpleContent;
Begin
  Result := Elements[iIndex];
End;

Procedure TWPDocumentSimpleContents.SetItemByIndex(iIndex : Integer; value: TWPDocumentSimpleContent);
Begin
  Elements[iIndex] := value;
End;

Procedure TWPDocumentSimpleContents.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TWPDocumentSimpleContents.ClearItems;
Begin
  Clear;
End;

function TWPDocumentSimpleContents.Count: Integer;
begin
  result := Inherited Count;
end;


{ TWPDocumentBlocks }

Procedure TWPDocumentBlocks.AddItem(value : TWPDocumentBlock);
Begin
  Add(value.Link);
End;

Function TWPDocumentBlocks.IndexOf(value : TWPDocumentBlock) : Integer;
Begin
  result := IndexByReference(value);
End;

Procedure TWPDocumentBlocks.InsertItem(iIndex : Integer; value : TWPDocumentBlock);
begin
  Inherited Insert(iIndex, value);
End;

Function TWPDocumentBlocks.Item(iIndex : Integer) : TWPDocumentBlock;
Begin
  Result := Elements[iIndex];
End;

Procedure TWPDocumentBlocks.SetItemByIndex(iIndex : Integer; value: TWPDocumentBlock);
Begin
  Elements[iIndex] := value;
End;

Procedure TWPDocumentBlocks.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TWPDocumentBlocks.ClearItems;
Begin
  Clear;
End;

function TWPDocumentBlocks.Count: Integer;
begin
  result := Inherited Count;
end;

{ TWPDocumentParagraphs }

Function TWPDocumentParagraphs.Append : TWPDocumentParagraph;
Begin
  Result := TWPDocumentParagraph.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TWPDocumentParagraphs.AddItem(value : TWPDocumentParagraph);
Begin
  Add(value.Link);
End;

Function TWPDocumentParagraphs.IndexOf(value : TWPDocumentParagraph) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TWPDocumentParagraphs.Insert(iIndex : Integer) : TWPDocumentParagraph;
Begin
  Result := TWPDocumentParagraph.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TWPDocumentParagraphs.InsertItem(iIndex : Integer; value : TWPDocumentParagraph);
begin
  Inherited Insert(iIndex, value);
End;

Function TWPDocumentParagraphs.Item(iIndex : Integer) : TWPDocumentParagraph;
Begin
  Result := Elements[iIndex];
End;

Procedure TWPDocumentParagraphs.SetItemByIndex(iIndex : Integer; value: TWPDocumentParagraph);
Begin
  Elements[iIndex] := value;
End;

Procedure TWPDocumentParagraphs.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TWPDocumentParagraphs.ClearItems;
Begin
  Clear;
End;

function TWPDocumentParagraphs.Count: Integer;
begin
  result := Inherited Count;
end;

{ TWPDocumentTableCells }

Function TWPDocumentTableCells.Append : TWPDocumentTableCell;
Begin
  Result := TWPDocumentTableCell.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TWPDocumentTableCells.AddItem(value : TWPDocumentTableCell);
Begin
  Add(value.Link);
End;

Function TWPDocumentTableCells.IndexOf(value : TWPDocumentTableCell) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TWPDocumentTableCells.Insert(iIndex : Integer) : TWPDocumentTableCell;
Begin
  Result := TWPDocumentTableCell.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TWPDocumentTableCells.InsertItem(iIndex : Integer; value : TWPDocumentTableCell);
begin
  Inherited Insert(iIndex, value);
End;

Function TWPDocumentTableCells.Item(iIndex : Integer) : TWPDocumentTableCell;
Begin
  Result := Elements[iIndex];
End;

Procedure TWPDocumentTableCells.SetItemByIndex(iIndex : Integer; value: TWPDocumentTableCell);
Begin
  Elements[iIndex] := value;
End;

Procedure TWPDocumentTableCells.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TWPDocumentTableCells.ClearItems;
Begin
  Clear;
End;

function TWPDocumentTableCells.Count: Integer;
begin
  result := Inherited Count;
end;

{ TWPDocumentTableRows }

Function TWPDocumentTableRows.Append : TWPDocumentTableRow;
Begin
  Result := TWPDocumentTableRow.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TWPDocumentTableRows.AddItem(value : TWPDocumentTableRow);
Begin
  Add(value.Link);
End;

Function TWPDocumentTableRows.IndexOf(value : TWPDocumentTableRow) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TWPDocumentTableRows.Insert(iIndex : Integer) : TWPDocumentTableRow;
Begin
  Result := TWPDocumentTableRow.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TWPDocumentTableRows.InsertItem(iIndex : Integer; value : TWPDocumentTableRow);
begin
  Inherited Insert(iIndex, value);
End;

Function TWPDocumentTableRows.Item(iIndex : Integer) : TWPDocumentTableRow;
Begin
  Result := Elements[iIndex];
End;

Procedure TWPDocumentTableRows.SetItemByIndex(iIndex : Integer; value: TWPDocumentTableRow);
Begin
  Elements[iIndex] := value;
End;

Procedure TWPDocumentTableRows.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TWPDocumentTableRows.ClearItems;
Begin
  Clear;
End;

function TWPDocumentTableRows.Count: Integer;
begin
  result := Inherited Count;
end;


procedure TWPDocumentImage.LoadFromFile(sName: String);
begin
  raise EWPException.create('not done yet');
end;

procedure TWPDocumentImage.LoadFromStream(oStream: TStream);
begin
  raise EWPException.create('not done yet');
end;

procedure TWPDocumentImage.SaveToFile(sName: String);
begin
  raise EWPException.create('not done yet');
end;

procedure TWPDocumentImage.SaveToStream(oStream: TStream);
begin
  raise EWPException.create('not done yet');
end;

procedure TWPDocument.StripSections;
begin
  // todo
end;

function TWPDocumentField.GetLink: TWPHotspot;
begin
  result := Hotspot;
end;

procedure TWPDocumentField.SetLink(const Value: TWPHotspot);
begin
  Hotspot := Value;
end;
procedure TWPDocumentAttachment.SetContent(const Value: TFslBuffer);
begin
  FContent.Free;
  FContent := Value;
end;

constructor TWPDocumentAttachment.Create;
begin
  inherited;
  FContent := TFslBuffer.create;
end;

destructor TWPDocumentAttachment.Destroy;
begin
  FContent.Free;
  inherited;
end;



Constructor TWPDocumentIteratorContext.Create;
Begin
  Inherited;

  FItem := Nil;
End;


Destructor TWPDocumentIteratorContext.Destroy;
Begin
  FItem.Free;

  Inherited;
End;


Function TWPDocumentIteratorContext.Link : TWPDocumentIteratorContext;
Begin
  Result := TWPDocumentIteratorContext(Inherited Link);
End;


Function TWPDocumentIteratorContext.Clone : TWPDocumentIteratorContext;
Begin
  Result := TWPDocumentIteratorContext(Inherited Clone);
End;


Procedure TWPDocumentIteratorContext.Assign(oObject : TFslObject);
Begin
  Inherited;

  Item.Assign(TWPDocumentIteratorContext(oObject).Item);
  Index := TWPDocumentIteratorContext(oObject).Index;
End;



Procedure TWPDocumentIteratorContext.SetItem(Const Value : TWPDocumentIteratorContextItem);
Begin
  FItem.Free;
  FItem := Value;
End;


function TWPDocumentIteratorContext.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FItem.sizeInBytes);
end;

Function TWPDocumentIteratorContexts.Link : TWPDocumentIteratorContexts;
Begin
  Result := TWPDocumentIteratorContexts(Inherited Link);
End;


Function TWPDocumentIteratorContexts.Clone : TWPDocumentIteratorContexts;
Begin
  Result := TWPDocumentIteratorContexts(Inherited Clone);
End;


Function TWPDocumentIteratorContexts.New : TWPDocumentIteratorContext;
Begin
  Result := TWPDocumentIteratorContext(Inherited New);
End;


Function TWPDocumentIteratorContexts.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentIteratorContext;
End;


Function TWPDocumentIteratorContexts.GetElement(Const iIndex : Integer) : TWPDocumentIteratorContext;
Begin
  Result := TWPDocumentIteratorContext(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentIteratorContexts.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentIteratorContext);
Begin
  ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentIteratorContexts.Get(Const aValue : Integer) : TWPDocumentIteratorContext;
Begin
  Result := TWPDocumentIteratorContext(Inherited Get(aValue));
End;


Procedure TWPDocumentIteratorContextStack.Push(oObject : TWPDocumentObject);
Var
  oContext : TWPDocumentIteratorContext;
Begin
  oContext := TWPDocumentIteratorContext.Create;
  Try
    oContext.Item := oObject.Link;
    oContext.Index := -1;

    Add(oContext.Link);
  Finally
    oContext.Free;
  End;
End;


Procedure TWPDocumentIteratorContextStack.Pop;
Begin
  Assert(CheckCondition(HasCurrent, 'Pop', 'Nothing to Pop'));
  DeleteByIndex(Count - 1);
End;


Function TWPDocumentIteratorContextStack.HasCurrent : Boolean;
Begin
  Result := Count > 0;
End;


Function TWPDocumentIteratorContextStack.Current : TWPDocumentIteratorContext;
Begin
  Result := Get(Count - 1);
End;

Constructor TWPDocumentIterator.Create;
Begin
  Inherited;
  FStack := TWPDocumentIteratorContextStack.Create;
End;


Destructor TWPDocumentIterator.Destroy;
Begin
  FStack.Free;
  FDocument.Free;
  Inherited;
End;


Function TWPDocumentIterator.GetDocument : TWPDocument;
Begin
  Assert(Invariants('GetDocument', FDocument, TWPDocument, 'Document'));
  Result := FDocument;
End;

Procedure TWPDocumentIterator.SetDocument(Const Value : TWPDocument);
Begin
  Assert((Value = Nil) Or Invariants('GetDocument', Value, TWPDocument, 'Document'));
  FDocument.Free;
  FDocument := Value;
End;


Function TWPDocumentIterator.HasDocument : Boolean;
Begin
  Result := Assigned(FDocument);
End;


Function TWPDocumentIterator.Included(oObject : TWPDocumentObject) : Boolean;
Begin
  Result := False;
End;


Procedure TWPDocumentIterator.First;
Begin
  FStack.Clear;
  FStack.Push(Document);
  If Not Included(Document) Then
    Next;
End;


Procedure TWPDocumentIterator.Next;
Var
  bDone : Boolean;
Begin
  Repeat
    bDone := False;
    While More And Not bDone Do
    Begin
      FStack.Current.Index := FStack.Current.Index + 1;
      If FStack.Current.Index < FStack.Current.Item.ChildCount Then
      Begin
        bDone := True;
        FStack.Push(FStack.Current.Item.Child(FStack.Current.Index));
      End
        Else FStack.Pop;
    End;
  Until Not More Or Included(Current);
End;


Function TWPDocumentIterator.Current : TWPDocumentObject;
Begin
  Assert(CheckCondition(More, 'Current', 'No Current Object available'));
  Result := FStack.Current.Item;
End;


Function TWPDocumentIterator.More : Boolean;
Begin
  Result := FStack.HasCurrent;
End;


function TWPDocumentIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDocument.sizeInBytes);
  inc(result, FStack.sizeInBytes);
end;

Constructor TWPDocumentValidator.Create;
Begin
  Inherited;
  FProblems := TWPDocumentValidationProblems.Create;
End;

Destructor TWPDocumentValidator.Destroy;
Begin
  FProblems.Free;
  Inherited;
End;

Function TWPDocumentValidator.Summary: String;
Var
  sSummary : String;
  iCount : Integer;
Begin
  sSummary := '';
  For iCount := 0 To FProblems.Count - 1 Do
    sSummary := sSummary + FProblems[iCount].Summary + cReturn;

  Result := sSummary;
End;

Procedure TWPDocumentValidator.Validate(oDocument: TWPDocument);
//Var
//  iCount : Integer;
Begin
  // normally we might just clear, but users may have made their own
  // links to the problem list.
  FProblems.Free;
  FProblems := TWPDocumentValidationProblems.Create;

  // for now, the only validation is checking that tables are not empty
// disabled until tested
//  For iCount := 0 to oDocument.Blocks.Count - 1 Do
//    if (oDocument.Blocks[iCount] is TWPDocumentTable) Then
//      ValidateTable(TWPDocumentTable(oDocument.Blocks[iCount]));
//other rules:
//  sections can't be empty

End;


function TWPDocumentValidator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FProblems.sizeInBytes);
end;

Class Procedure TWPDocumentValidator.ValidateAssert(oDocument : TWPDocument);
Var
  oInstance : TWPDocumentValidator;
  Exception : EWPDocumentValidator;
Begin
  oInstance := TWPDocumentValidator.Create;
  Try
    oInstance.Validate(oDocument);
    If oInstance.Problems.Count > 0 Then
    Begin
      Exception := EWPDocumentValidator.Create(oInstance.Summary);
      Exception.FProblems := oInstance.Problems.Link;
      Raise Exception;
    End;
  Finally
    oInstance.Free;
  End;
End;

Procedure TWPDocumentValidator.Problem(oObject : TWPDocumentObject; Const sMessage : String);
Var
  oProblem : TWPDocumentValidationProblem;
Begin
  oProblem := FProblems.New;
  Try
    oProblem.Object_ := oObject.Link;
    oProblem.Message := sMessage;
    FProblems.Add(oProblem.Link);
  Finally
    oProblem.Free;
  End;
End;

Procedure TWPDocumentValidator.ValidateTable(oTable: TWPDocumentTable);
Var
  iCount : Integer;
Begin
  If oTable.Rows.Count = 0 Then
    Problem(oTable, 'Tables must have rows')
  Else
    For iCount := 0 To oTable.Rows.Count - 1 Do
      ValidateTableRow(oTable.Rows[iCount]);
End;

Procedure TWPDocumentValidator.ValidateTableRow(oTableRow : TWPDocumentTableRow);
Var
  iCount : Integer;
Begin
  If oTableRow.Cells.Count = 0 Then
    Problem(oTableRow, 'Rows must have cells');
  For iCount := 0 To oTableRow.Rows.Count - 1 Do
    ValidateTableRow(oTableRow.Rows[iCount]);
End;

{ EWPDocumentValidator }

Destructor EWPDocumentValidator.Destroy;
Begin
  FProblems.Free;
  Inherited;
End;



Constructor TWPDocumentValidationProblem.Create;
Begin
  Inherited;
End;


Destructor TWPDocumentValidationProblem.Destroy;
Begin
  FObject.Free;
  Inherited;
End;


Function TWPDocumentValidationProblem.Link : TWPDocumentValidationProblem;
Begin
  Result := TWPDocumentValidationProblem(Inherited Link);
End;


Function TWPDocumentValidationProblem.Clone : TWPDocumentValidationProblem;
Begin
  Result := TWPDocumentValidationProblem(Inherited Clone);
End;


Procedure TWPDocumentValidationProblem.Assign(oObject : TFslObject);
Begin
  Inherited;
  FMessage := TWPDocumentValidationProblem(oObject).Message;
  FObject := TWPDocumentValidationProblem(oObject).FObject.Link;
End;


function TWPDocumentValidationProblem.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FMessage.length * sizeof(char)) + 12);
  inc(result, FObject.sizeInBytes);
end;

Function TWPDocumentValidationProblems.Link : TWPDocumentValidationProblems;
Begin
  Result := TWPDocumentValidationProblems(Inherited Link);
End;


Function TWPDocumentValidationProblems.Clone : TWPDocumentValidationProblems;
Begin
  Result := TWPDocumentValidationProblems(Inherited Clone);
End;


Function TWPDocumentValidationProblems.New : TWPDocumentValidationProblem;
Begin
  Result := TWPDocumentValidationProblem(Inherited New);
End;


Function TWPDocumentValidationProblems.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentValidationProblem;
End;


Function TWPDocumentValidationProblems.GetElement(Const iIndex : Integer) : TWPDocumentValidationProblem;
Begin
  Result := TWPDocumentValidationProblem(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentValidationProblems.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentValidationProblem);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentValidationProblems.Get(Const aValue : Integer) : TWPDocumentValidationProblem;
Begin
  Result := TWPDocumentValidationProblem(Inherited Get(aValue));
End;



Function TWPDocumentValidationProblem.GetObject: TWPDocumentObject;
Begin
  Assert(Invariants('GetObject', FObject, TWPDocumentObject, 'Object'));
  Result := FObject;
End;

Procedure TWPDocumentValidationProblem.SetObject(Const Value: TWPDocumentObject);
Begin
  Assert(Invariants('GetObject', Value, TWPDocumentObject, 'Value'));
  FObject.Free;
  FObject := Value;
End;

Function TWPDocumentValidationProblem.Summary: String;
Begin
  If FObject.Name <> '' Then
    Result := '['+FObject.NamePair+'] '+Message
  Else
    Result := Message;
End;


End.

















