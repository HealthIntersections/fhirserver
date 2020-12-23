Unit dicom_objects;

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
  SysUtils,
  fsl_base, fsl_utilities, fsl_stream, fsl_collections,
  dicom_dictionary;

Type
  TWords = Array of Word;
  TSingles = Array of Single;

  TDicomPN = Record
    FamilyName : String;
    GivenName : String;
    MiddleName : String;
    NamePrefix : String;
    NameSuffix : String;
  End;

  TDicomAT = Record
    GroupId: Word;
    ElementId: Word;
  End;

  {
  A DICOM value
  }
  TDicomValue = class (TFslObject)
  Private
    FBytes : TBytes;
    FKnownType : TDicomVRType;
    FPossibleTypes : TDicomVRTypes;
    FDictionary : TDicomDictionary;
    FOffsetEnd: Cardinal;
    FOffsetStart: Cardinal;

    Procedure MoveCheck(sType : String; iOffset : Integer; var Dest; ilength : Integer; bExact : Boolean);
    Function AsBytes(Const Src; ilength : integer) : TBytes;

    Function GetAsAE: String;
    Function GetAsAS: String;
    Function GetAsAT: TDicomAT;
    Function GetAsCS: String;
    Function GetAsDA: TDateTime;
    Function GetAsDS: String;
    Function GetAsDT: TDateTime;
    Function GetAsFD: Double;
    Function GetAsFL: Single;
    Function GetAsIS: String;
    Function GetAsLO: String;
    Function GetAsLT: String;
    Function GetAsOF: TSingles;
    Function GetAsOW: TWords;
    Function GetAsPN: TDicomPN;
    Function GetAsSH: String;
    Function GetAsSL: Integer;
    Function GetAsSS: ShortInt;
    Function GetAsST: String;
    Function GetAsTM: TDateTime;
    Function GetAsUI: String;
    Function GetAsUL: Cardinal;
    Function GetAsUN: TBytes;
    Function GetAsUS: Word;
    Function GetAsUT: String;
    Procedure SetAsAE(const Value: String);
    Procedure SetAsAS(const Value: String);
    Procedure SetAsAT(Value: TDicomAT);
    Procedure SetAsCS(const Value: String);
    Procedure SetAsDA(const Value: TDateTime);
    Procedure SetAsDS(const Value: String);
    Procedure SetAsDT(const Value: TDateTime);
    Procedure SetAsFD(const Value: Double);
    Procedure SetAsFL(const Value: Single);
    Procedure SetAsIS(const Value: String);
    Procedure SetAsLO(const Value: String);
    Procedure SetAsLT(const Value: String);
    Procedure SetAsOF(const Value: TSingles);
    Procedure SetAsOW(const Value: TWords);
    Procedure SetAsPN(const Value: TDicomPN);
    Procedure SetAsSH(const Value: String);
    Procedure SetAsSL(const Value: Integer);
    Procedure SetAsSS(const Value: ShortInt);
    Procedure SetAsST(const Value: String);
    Procedure SetAsTM(const Value: TDateTime);
    Procedure SetAsUI(const Value: String);
    Procedure SetAsUL(const Value: Cardinal);
    Procedure SetAsUN(const Value: TBytes);
    Procedure SetAsUS(const Value: Word);
    Procedure SetAsUT(const Value: String);

    Procedure SetDictionary(const Value: TDicomDictionary);
    Function GetAsString: String;
    Procedure SetAsString(const s: String);
    Procedure SetAsWords(const s: String);
  Public
    constructor Create(oDictionary : TDicomDictionary; aKnownType : TDicomVRType; aPossibleTypes : TDicomVRTypes; aBytes : TBytes = nil); overload;
    destructor Destroy; Override;

    Function Clone : TDicomValue; Overload;
    Function Link : TDicomValue; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Property OffsetStart : Cardinal read FOffsetStart write FOffsetStart;
    Property OffsetEnd : Cardinal read FOffsetEnd write FOffsetEnd;
    Procedure ClearOffsets;
    Property KnownType : TDicomVRType read FKnownType;

  Published
    {
    The dictionary in used
    }
    Property Dictionary : TDicomDictionary read FDictionary write SetDictionary;

    {
    The value as a string
    }
    Property AsString : String read GetAsString write SetAsString;

    {
    The value as 'Code String',
    A string of characters with leading or trailing spaces being nonsignificant
    }
    Property AsCS : String read GetAsCS write SetAsCS;

    {
    The value as 'Short String',
    A short character string. Example: telephone numbers, IDs
    }
    Property AsSH : String read GetAsSH write SetAsSH;

    {
    The value as 'Long String',
    A character string that may be padded with leading and/or trailing spaces
    }
    Property AsLO : String read GetAsLO write SetAsLO;

    {
    The value as 'Short Text',
    A character string that may contain one or more paragraphs.
    }
    Property AsST : String read GetAsST write SetAsST;

    {
    The value as 'Long Text',
    A character string that may contain one or more paragraphs, the same as LO, but can be much longer
    }
    Property AsLT : String read GetAsLT write SetAsLT;

    {
    The value as 'Unlimited Text',
    A character string that may contain one or more paragraphs, similar to LT.
    }
    Property AsUT : String read GetAsUT write SetAsUT;

    {
    The value as 'Application Entity',
    A string of characters that identifies a device name with leading and trailing spaces being nonsignificant.
    }
    Property AsAE : String read GetAsAE write SetAsAE;

    {
    The value as 'Unique Identifier (UID)',
    A character string containing a UID that is used to uniquely identify a wide variety of items
    }
    Property AsUI : String read GetAsUI write SetAsUI;

    {
    The value as 'Date',
    A string of characters of the format YYYYMMDD; where YYYY shall contain year, MM shall contain the month, and DD shall contain the day
    }
    Property AsDA : TDateTime read GetAsDA write SetAsDA;

    {
    The value as 'Time',
    A string of characters of the format HHMMSS.FRAC;
    where HH contains hours (range 00  23),
    MM contains minutes (range 00  59),
    '+'SS contains seconds (range 00 59),
    and FRAC contains a fractional part of a second as small as one millionth of a second.

    Example: 183200.00 stands for 6:32 PM.
    }
    Property AsTM : TDateTime read GetAsTM write SetAsTM;

    {
    The value as 'Date Time',
    Concatenated datetime string in the format: YYYYMMDDHHMMSS.FFFFFF
    The components of this string, from left to right, are YYYY = Year,
    '+'MM = Month, DD = Day, HH = Hour, MM = Minute, SS = Second, FFFFFF = Fractional Second.

    Example: 20050812183000.00 stands for 6:30 PM, August 12, 2005');
    }
    Property AsDT : TDateTime read GetAsDT write SetAsDT;

    {
    The value as 'Age String',
    A string of characters with one of the following formats: nnnD, nnnW, nnnM, nnnY;
    where nnn contains the number of days for D, weeks for W, months for M, or years for Y.
    Example: "018M" would represent an age of 18 months.
    }
    Property AsAS : String read GetAsAS write SetAsAS;

    {
    The value as 'Integer String',
    A string of characters representing an integer. Example: -1234567.
    }
    Property AsIS : String read GetAsIS write SetAsIS;

    {
    The value as 'Decimal String',
    A string of characters representing either a fixed point number or a floating point number
    }
    Property AsDS : String read GetAsDS write SetAsDS;

    {
    The value as 'Signed Short',
    Signed binary integer 16 bits long.
    }
    Property AsSS : ShortInt read GetAsSS write SetAsSS;

    {
    The value as 'Unsigned Short',
    Unsigned binary integer 16 bits long.
    }
    Property AsUS : Word read GetAsUS write SetAsUS;

    {
    The value as 'Signed Long',
    Signed binary integer
    }
    Property AsSL : Integer read GetAsSL write SetAsSL;

    {
    The value as 'Unsigned Long',
    Unsigned binary integer 32 bits long.
    }
    Property AsUL : Cardinal read GetAsUL write SetAsUL;

    {
    The value as 'Floating Point Single',
    Single precision binary floating point number.
    }
    Property AsFL : Single read GetAsFL write SetAsFL;

    {
    The value as 'Floating Point Double',
    Double precision binary floating point number
    }
    Property AsFD : Double read GetAsFD write SetAsFD;

  Public

// TODO: Hide as not sure how to convert records into IDL types
    {
    The value as 'Person Name',
    Persons name, with a caret character (^) used as a name delimiter
    }
    Property AsPN : TDicomPN read GetAsPN write SetAsPN;

    {
    The value as 'Attribute Tag',
    Ordered pair of 16-bit (2-byte) unsigned integers that is the value of a Data Element Tag.
    }
    Property AsAT : TDicomAT read GetAsAT write SetAsAT;

// TODO: Hide as not sure how to convert TBytes/TWords/TSingles into IDL types
    {
    The value as 'Other Byte String',
    A string of bytes (other means not defined in any other VR).
    }
    Property AsOB : TBytes read GetAsUN write SetAsUN;

    {
    The value as 'Other Word String',
    A string of 16-bit (2-byte) words.
    }
    Property AsOW : TWords read GetAsOW write SetAsOW;

    {
    The value as 'Other Float String',
    A string of 32-bit (4-byte) floating point words
    }
    Property AsOF : TSingles read GetAsOF write SetAsOF;

    {
    The value as a string of bytes where the encoding of the contents is unknown
    }
    Property AsUN : TBytes read GetAsUN write SetAsUN;
  End;


  {
  }
  TDicomValueList = class (TFslObjectList)
  private
    FDictionary : TDicomDictionary;
    FKnownType : TDicomVRType;
    FPossibleTypes : TDicomVRTypes;
    Function GetValues(iIndex: integer): TDicomValue;
    Procedure SetDictionary(const Value: TDicomDictionary);
  Protected
    Function ItemClass : TFslObjectClass; Override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(oDictionary : TDicomDictionary; aKnownType : TDicomVRType; aPossibleTypes : TDicomVRTypes);
    destructor Destroy; Override;

    Function Link : TDicomValueList; Overload;
    Function Clone : TDicomValueList; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Function Add(aBytes : TBytes) : TDicomValue; overload;

    {
    Facade to access PossibleTypes in script,
    which is a list of possible types for these values
    }
    Function GetPossibleTypes: TFslStringList;

    {
      Add a TDicomValue to the End of the list.
    }
    Function Append : TDicomValue;

    {
      Add an already existing TDicomValue to the End of the list.
    }
    Procedure AddItem(value : TDicomValue);

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomValue) : Integer;

    {
       Insert TDicomValue before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TDicomValue;

    {
       Insert an existing TDicomValue before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TDicomValue);

    {
       Get the iIndexth TDicomValue. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomValue;

    {
       Set the iIndexth TDicomValue. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TDicomValue);

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

  Published
    {
    The known type for these value
    }
    Property KnownType : TDicomVRType read FKnownType;

    {
    List of possible types for these values
    }
    Property PossibleTypes : TDicomVRTypes read FPossibleTypes;

    {
    The dictionary in used
    }
    Property Dictionary : TDicomDictionary read FDictionary write SetDictionary;

  Public
    {
    The value at the given index
    }
    Property Values[iIndex : integer] : TDicomValue read GetValues; default;

  End;

Function ParsePN(pn : String):TDicomPN;

type
  // an element has an identity (group/element), a definition (optional), and either
  //  * [Simp[e] a value with a specified type (or more than one, for some types)
  //  * [Complex] a series of DicomObjects
  TDicomObjectList = class;

  {
  A simple DICOM string
  }
  TDicomString = class (TFslObject)
  Private
    FOffsetEnd: Cardinal;
    FOffsetStart: Cardinal;
    FValue: String;
    function GetValue: String;
    function GetValueA: AnsiString;
    procedure SetValueA(const Value: AnsiString);
  Public
    constructor Create(sValue : String); Overload;
    constructor Create(iValue : Cardinal); Overload;
    Function Clone : TDicomString; Overload;
    Function Link : TDicomString; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    {
      Return the string as cardinal number, or 0 if not a cardinal number
    }
    Function AsCardinal : Cardinal;

    Property OffsetStart : Cardinal read FOffsetStart write FOffsetStart;

    Property OffsetEnd : Cardinal read FOffsetEnd write FOffsetEnd;

  Published
    {
    The actual string value
    }
    Property Value : String read GetValue write FValue;
    Property ValueA : AnsiString read GetValueA write SetValueA;
  End;

  {
  A list of Dicom string
  }
  TDicomStringList = Class(TFslObjectList)
  private
    function GetChildren(iIndex: integer): TDicomString;
  Protected
    Function ItemClass : TFslObjectClass; Override;

  Public
    Function Link : TDicomStringList; Overload;
    Function Clone : TDicomStringList; Overload;


    {
      Add a TDicomString to the End of the list.
    }
    Function Append : TDicomString;

    {
      Add an already existing TDicomString to the End of the list.
    }
    Procedure AddItem(value : TDicomString);

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomString) : Integer;

    {
       Insert TDicomString before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TDicomString;

    {
       Insert an existing TDicomString before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TDicomString);

    {
       Get the iIndexth TDicomString. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomString;

    {
       Set the iIndexth TDicomString. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TDicomString);

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

    Property Children[iIndex : integer] : TDicomString read GetChildren; default;
  End;

  {
  A data element, which is a unit of information in an object definition
  
  A Data Set (i.e. DicomObject) represents an instance of a real world Information Object. 
  A Data Set is constructed of Data Elements. Data Elements contain
  the encoded Values of Attributes of that object. The specific content 
  and semantics of these Attributes are specified in Information Object Definitions (see PS 3.3). 
  
  A Data Element is uniquely identified by a Data Element Tag. The Data Elements 
  in a Data Set shall be ordered by increasing Data Element Tag Number 
  and shall occur at most once in a Data Set.
  
  NOTE: Different to DictElement, which represent a data element definition,
  DataElement is an actual data object (i.e. with value(s).)
  }
  TDicomDataElement = class (TFslObject)
  private
    FIsSimple : Boolean;
    FGroupId: Word;
    FElementId: Word;
    FElementDefinition: TDicomDictionaryElement;
    FObjects: TDicomObjectList;
    FValues: TDicomValueList;
    FDictionary: TDicomDictionary;
    FOffsetEnd: Cardinal;
    FOffsetStart: Cardinal;
    procedure SetElementDefinition(const Value: TDicomDictionaryElement);
    procedure SetDictionary(const Value: TDicomDictionary);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(oDictionary : TDicomDictionary; aKnownType : TDicomVRType; aPossibleTypes : TDicomVRTypes);
    constructor CreateComplex(oDictionary : TDicomDictionary);
    destructor Destroy; Override;

    Function Clone : TDicomDataElement; Overload;
    Function Link : TDicomDataElement; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Property OffsetStart : Cardinal read FOffsetStart write FOffsetStart;
    Property OffsetEnd : Cardinal read FOffsetEnd write FOffsetEnd;

    Function HasValues : Boolean;
    Function HasObjects : Boolean;
    
    Function Tag : String;
    Function AsString : String;

    Function SingleValue : TDicomValue;

    {
      The size of the element when parsed.

      The size will be 0 if the element is being built instead of parsed.

      The size of the element may be wrong if script has modified it's contents after parsing
    }
    Function Size : Cardinal;
  Published
    {
    The group Id of the element tag
    }
    Property GroupId : Word read FGroupId write FGroupId;
    
  {
    The element id of the element tag
    }
    Property ElementId : Word read FElementId write FElementId;
    
  {
    The dictionary used to decode this element
    }
    Property Dictionary : TDicomDictionary read FDictionary write SetDictionary;
    
  {
    The object model definition of this element
    }
    Property ElementDefinition : TDicomDictionaryElement read FElementDefinition write SetElementDefinition;
    
  {
    True if this data element is not a sequence
    }
    Property IsSimple : Boolean read FIsSimple;
    
  {
    List of data values contained in this element
    }
    Property Values : TDicomValueList read FValues;
    
  {
    If this is a sequence, it would contain a list of DICOM objects
    }
    Property Objects : TDicomObjectList read FObjects;

  End;


  {
  A list of Dicom data element instance
  }
  TDicomDataElementList = Class(TFslObjectList)
  private
    FDictionary: TDicomDictionary;

    function GetElements(iIndex: integer): TDicomDataElement;
  Protected
    Function ItemClass : TFslObjectClass; Override;

    Function CompareByTags(pA, pB: Pointer): Integer; Virtual;

    Procedure DefaultCompare(Out aEvent : TFslItemListCompare); Override;

    Function FindByTags(Const group, element: Word; Out iIndex: Integer): Boolean; Overload;

    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(oDictionary: TDicomDictionary); Overload;
    destructor Destroy; Override;

    Function Link : TDicomDataElementList; Overload;
    Function Clone : TDicomDataElementList; Overload;

    {
      Sorted data elements by their tags (i.e. GroupId first, then ElementId)
    }
    Procedure SortedByTags;

    {
      True if elements are sorted by their tags
    }
    Function IsSortedByTags : Boolean;

    {
      Index of item with maching group id/element Id, -1 if not found
    }
    Function IndexByTags(Const group, element: Word) : Integer; Overload;

    {
      True if existed item with matching group id/element id
    }
    Function ExistsByTags(Const group, element: Word) : Boolean; Overload;

    {
      True if existed item with matching group id/element id in format Group,Element, with hex numbers, like this: 0800,1600, or NULL if not found
    }
    Function ExistsByTag(tag : String) : Boolean; Overload;

    {
      Return item with matching group id/element id, or NULL if not found

      Note that the standard DICOM tags are hex numbers, so you have to prepend the numerical values with 0x for javascript or &h for vbscript
    }
    Function GetByTags(Const group, element: Word) : TDicomDataElement; Overload;

    {
      Return item with matching group id/element id in format Group,Element, with hex numbers, like this: 0800,1600, or NULL if not found
    }
    Function GetByTag(tag : String) : TDicomDataElement; Overload;

    Procedure RemoveByTags(Const group, element: Word);

    {
      Add an already existing TDicomDataElement to the End of the list.
    }
    Procedure AddItem(value : TDicomDataElement);

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomDataElement) : Integer;

    {
       Insert an existing TDicomDataElement before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TDicomDataElement);

    {
       Get the iIndexth TDicomDataElement. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomDataElement;

    {
       Set the iIndexth TDicomDataElement. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TDicomDataElement);

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
      A string summary of the elements in the list, with tags separated by semi-colons and values separated by commas.
    }
    Function AsString(includeTags : Boolean) : String;

    Property Elements[iIndex : integer] : TDicomDataElement Read GetElements; Default;
  Published
    {
    The dictionary used in this list
    }
    Property Dictionary : TDicomDictionary Read FDictionary;
  End;

  {
  A simple DICOM object
  }
  TDicomObject = class (TFslObject)
  Private
    FDictionary: TDicomDictionary;
    FElements : TDicomDataElementList;
    FOffsetEnd: Cardinal;
    FOffsetStart: Cardinal;
    Function MakeElement(sTag : String; sValue : String) : TDicomDataElement; Overload;
    Function MakeElement(sTag : String) : TDicomDataElement; Overload;
    Procedure ExecuteQuery(contexts : TDicomObjectList; results : TDicomDataElementList; path : String; Cursor : Integer; bDescendents : Boolean);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Overload; Override;
    constructor Create(oDictionary: TDicomDictionary); Overload;
    destructor Destroy; Override;
    Function Clone : TDicomObject; Overload;
    Function Link : TDicomObject; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Property OffsetStart : Cardinal read FOffsetStart write FOffsetStart;
    Property OffsetEnd : Cardinal read FOffsetEnd write FOffsetEnd;
    Function AsString : String;


    {
      The size of the object when parsed.

      The size will be 0 if the object is being built instead of parsed.

      The size of the object may be wrong if script has modified it's contents after parsing
    }
    Function Size : Cardinal;


    {
      Query a Dicom Object for one or more contained elements. Whether more than one
      element can be returned will depend on the query string.

      The format for query string is a repeating series of tag[filter] specifiers,
      with a seperator. This format is loosely based on XPath.

      "tag" is any dicom tag, in hex, such as 0002,0010 (which is File Transfer
      Syntax UID). Wild cards such as 1000,xxx0 are allowed. Also the tag value '*'
      is allowed, which matches any tag (and is synonymous with xxxx,xxxx)

      The seperator "/" means to match only immediate children of the existing
      set of matching node. The seperator "//" means to match any descendent element.
      While the query may start with a seperator, the query is always executed from
      the context of the current object (Unlike XPath. a starting "/" is redundant and ignored,
      but a starting "//" means "any descendent of the current object"

      The filter is a formula. The outcome from the formula can either be a
      boolean, a numerical value, or a list of elements. If the outcome of the
      formula is a boolean (e.g 0018,6011[0018,6012 = 1], select any US regions
      that have a spacial format of 1) then an element will be included in
      the match list if the formula evaluates to true for it. If the formula
      returns a list of data elements, then an element will be included in
      the match list if any of the elements in the filter formula have a value
      (e.g. 0018,6011[0018,6012], which selects any US region that has a specified
      spacial format. Finally, if the return value is a numerical value, then
      the value is treated as an index in the sibling list of matching elements
      (e.g 0018,6011[2] means the second US region).

      The formula can use the following operators: =, !=, <>, >, <, <=, >=, +, -, \, *.
      In addition, & means string concatenation, ++ means integer addition (i.e. null
      if there's an error), and == means case-insensitive comparison. OIDs are
      treated as strings.

      The following formulas are defined:
        position() -  the index of the element in tag match list (the list of elements that match the tag specification, irrespective of the filter)
        last() - the index number of the last element in the tag match list (count() - 1)
        count() - the number of tag matches
        contains()
        substring(value, start, end)
        substring-before(value, offset)
        substring-after(value, offset)
        starts-with(value, possible-start-value)
        ends-with(value, possible-start-value)
        string-length(value)
        upper-case(value)
        lower-case(value)
        if(test,value-if-true,value-if-false)

      All the values can be a nested query, a string or numerical constant, an OID, or another formula.

      You can combine queries using the | character.
    }
    Function Query(path : String) : TDicomDataElementList;

    {
      Tag the object: ensure that the object has an element corresponding to the tag
      (add it if required) and set it's value to to the provided value

      value format:
        PN: use Dicom.PN(family, given, middle, prefix, suffix)
        AT: use tag 0000,0000
        UN, OB, OW: mime encoded binary
        other: as expected
    }
    Procedure Tag(sTag : String; sValue : String);

    {
      Tag the object: ensure that the object has an element corresponding to the tag
      (add it if required) and then add the provided value to it's values

      value format:
        PN: use Dicom.PN(family, given, middle, prefix, suffix)
        AT: use tag 0000,0000
        UN, OB, OW: mime encoded binary
        other: as expected
    }
    Procedure TagN(sTag : String; sValue : String);

    {
      ensure that the object has an element corresponding to the tag (add it
      if required) and then add an object to it's sequence, and return the
      object for it's values to be filled out
    }
    Function Sequence(sTag : String) : TDicomObject;

    {
      A short cut method to get a simple tag as a plain string.

      The method will return a null string if
        * the element doesn't exist
        * the element is empty
        * elements that are sequences (type SQ)

      If the element has multiple values, they will be returned separated by " | ".

      The element will be returned as a single string, for DICOM types PN and AT, this will be in their native DICOM format.
      For PN, see DICOM.PNPart.

    }
    Function ElementAsString(tagId : String): String;

    {
      A short cut method to get a simple tag as a plain string
      (same as "ElementAsString" but just a shorter name, due
      to the ubiquity of it's use)

      The method will return a null string if
        * the element doesn't exist
        * the element is empty
        * elements that are sequences (type SQ)

      If the element has multiple values, they will be returned separated by " | ".

      The element will be returned as a single string, for DICOM types PN and AT, this will be in their native DICOM format.
      For PN, see DICOM.PNPart.

    }
    Function v(tagId : String): String;
  Published


    {
    List of DICOM data elements made up this DICOM object
    }
    Property Elements : TDicomDataElementList read FElements;
  End;

  {
  A list of simple DICOM objects
  }
  TDicomObjectList = Class(TFslObjectList)
  private
    FDictionary: TDicomDictionary;
    function GetChildren(iIndex: integer): TDicomObject;
  Protected
    Function ItemClass : TFslObjectClass; Override;

    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(oDictionary: TDicomDictionary); Overload;
    destructor Destroy; Override;

    Function Link : TDicomObjectList; Overload;
    Function Clone : TDicomObjectList; Overload;

    {
      Add a TDicomObject to the End of the list.
    }
    Function Append : TDicomObject;

    {
      Add an already existing TDicomObject to the End of the list.
    }
    Procedure AddItem(value : TDicomObject);

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomObject) : Integer;

    {
       Insert TDicomObject before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TDicomObject;

    {
       Insert an existing TDicomObject before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TDicomObject);

    {
       Get the iIndexth TDicomObject. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomObject;

    {
       Set the iIndexth TDicomObject. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TDicomObject);

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

    Property Children[iIndex : integer] : TDicomObject read GetChildren; default;
  Published
    {
    The dictionary used in this list
    }
    Property Dictionary : TDicomDictionary Read FDictionary;
  End;

  {
  List of possible PDU types
  (Associate request/accept/reject, data, abort, release request/response)
  }
  TDicomPDUType = (pduAssociateRequest, pduAssociateAccept, pduAssociateReject, pduData, pduAbort, pduReleaseRequest, pduReleaseResponse);

  {
  A list of simple DICOM objects
  }
  TDicomPDU = class (TFslObject)
  private
    FPacketType: TDicomPDUType;
    FVersion: Word;
    FOffsetEnd: Cardinal;
    FOffsetStart: Cardinal;
    FDictionary: TDicomDictionary;
    procedure SetDictionary(const Value: TDicomDictionary);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TDicomPDU; Overload;
    Function Clone : TDicomPDU; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Property OffsetStart : Cardinal read FOffsetStart write FOffsetStart;
    Property OffsetEnd : Cardinal read FOffsetEnd write FOffsetEnd;

  Published
    {
    The PDU type
    }
    Property PacketType : TDicomPDUType read FPacketType;

  {
    }
    Property Version : Word read FVersion;


  {
    The Dictionary used with this PDU
    }
    Property Dictionary : TDicomDictionary read FDictionary write SetDictionary;
  End;

  {
  A Presentation Context defines the presentation of the data on an Association. 
  It provides a lower level of negotiation and one or more Presentation Contexts 
  can be offered and accepted per Association. 
  
  A Presentation Context consists of three components, a Presentation Context ID, 
  an Abstract Syntax Name, and a list of one or more Transfer Syntax Names. 
  
  Only one Abstract Syntax shall be offered per Presentation Context. However, 
  multiple Transfer Syntaxes may be offered per Presentation Context, 
  but only one shall be accepted.
  }
  TDicomPresentationContextInfo = class (TFslObject)
  private
    FId: Byte;
    FAbstractSyntax: TDicomString;
    FTransferSyntaxes: TDicomStringList;
    FOffsetEnd: Cardinal;
    FOffsetStart: Cardinal;
    procedure SetAbstractSyntax(const Value: TDicomString);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Function Link : TDicomPresentationContextInfo; Overload;
    Function Clone : TDicomPresentationContextInfo; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Property OffsetStart : Cardinal read FOffsetStart write FOffsetStart;
    Property OffsetEnd : Cardinal read FOffsetEnd write FOffsetEnd;

  Published

    {
    The unique id (within this communication) of this presentation context
    }
    Property Id : Byte read FId write FId;

  {
    The abstract syntax UID that's supported by this presentation context
    }
    Property AbstractSyntax : TDicomString read FAbstractSyntax write SetAbstractSyntax;

  {
    The list of transfer syntax that're associated with this presentation context
    }
    Property TransferSyntaxes : TDicomStringList read FTransferSyntaxes;

  End;

  {
  A list of Presentation contexts
  }
  TDicomPresentationContextInfoList = class (TFslObjectList)
  Private
    function GetElements(iIndex: Integer): TDicomPresentationContextInfo;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    
    {
      Add a TDicomPresentationContextInfo to the End of the list.
    }
    Function Append : TDicomPresentationContextInfo;

    {
      Add an already existing TDicomPresentationContextInfo to the End of the list.
    }
    Procedure AddItem(value : TDicomPresentationContextInfo);

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomPresentationContextInfo) : Integer;

    {
       Insert TDicomPresentationContextInfo before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TDicomPresentationContextInfo;

    {
       Insert an existing TDicomPresentationContextInfo before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TDicomPresentationContextInfo);

    {
       Get the iIndexth TDicomPresentationContextInfo. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomPresentationContextInfo;

    {
       Set the iIndexth TDicomPresentationContextInfo. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TDicomPresentationContextInfo);

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

    property Elements[iIndex : Integer] : TDicomPresentationContextInfo read GetElements; default;
  End;

  {
  List of possible presentation results 
  (Accept, User rejection, Provider Reject, Abstract syntax or Transfer Syntax)
  }
  TPresentationAcceptResult = (parAccept, parUserRejection, parProviderRejection, parAbstractSyntax, parTransferSyntax);

  {
  A presentation context info response
  }
  TDicomPresentationAcceptContextInfo = class (TFslObject)
  private
    FId: Byte;
    FTransferSyntax: TDicomString;
    FOffsetEnd: Cardinal;
    FOffsetStart: Cardinal;
    FResult: TPresentationAcceptResult;
    procedure SetTransferSyntax(const Value: TDicomString);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;
    Function Link : TDicomPresentationAcceptContextInfo; Overload;
    Function Clone : TDicomPresentationAcceptContextInfo; Overload;
    Procedure Assign(oSource : TFslObject); Override;

  Property OffsetStart : Cardinal read FOffsetStart write FOffsetStart;
    Property OffsetEnd : Cardinal read FOffsetEnd write FOffsetEnd;

  Published

    {
    The unique id (within this communication) of this presentation context
    }
    Property Id : Byte read FId write FId;

  {
    The select transfer syntax that is deemed suitable for both communication parties
    }
    Property TransferSyntax : TDicomString read FTransferSyntax write SetTransferSyntax;

  {
    The presentation context result
    }
    Property Result : TPresentationAcceptResult read FResult write FResult;

  End;

  {
  A list of Presentation contexts result
  }
  TDicomPresentationAcceptContextInfoList = class (TFslObjectList)
  Private
    function GetElements(iIndex: Integer): TDicomPresentationAcceptContextInfo;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public

    {
      Add a TDicomPresentationAcceptContextInfo to the End of the list.
    }
    Function Append : TDicomPresentationAcceptContextInfo;

    {
      Add an already existing TDicomPresentationAcceptContextInfo to the End of the list.
    }
    Procedure AddItem(value : TDicomPresentationAcceptContextInfo);

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomPresentationAcceptContextInfo) : Integer;

    {
       Insert TDicomPresentationAcceptContextInfo before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TDicomPresentationAcceptContextInfo;

    {
       Insert an existing TDicomPresentationAcceptContextInfo before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TDicomPresentationAcceptContextInfo);

    {
       Get the iIndexth TDicomPresentationAcceptContextInfo. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomPresentationAcceptContextInfo;

    {
       Set the iIndexth TDicomPresentationAcceptContextInfo. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TDicomPresentationAcceptContextInfo);

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

    Property Elements[iIndex : Integer] : TDicomPresentationAcceptContextInfo read GetElements;default;
  End;

  {
  The user information parameter, of the A-ASSOCIATE primitive, can be extended 
  to support the negotiation needs of DICOM Application Entities using the
  Upper Layer (UL) Service. This will result in the definition of specific 
  user information sub-items. These sub-items shall be assigned unique 
  item-type values registered in PS 3.7
  }
  TDicomUserData = class (TDicomString)
  Private
    FId: Word;
  Public
    Function Link : TDicomUserData; Overload;
    Function Clone : TDicomUserData; Overload;
    Procedure Assign(oSource : TFslObject); Override;

  Published
    {
      The Id of this user data
    }
    Property Id : Word Read FId Write FId;
  End;

  {
  A list of Presentation contexts result
  }
  TDicomUserDataList = class (TFslObjectList)
  Private
    function GetElements(iIndex: Integer): TDicomUserData;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Procedure Add(iId : integer; Const oValue : TDicomString; iOffsetStart, iOffsetEnd : Cardinal); Overload;

    {
      Add a TDicomUserData to the End of the list.
    }
    Function Append : TDicomUserData;

    {
      Add an already existing TDicomUserData to the End of the list.
    }
    Procedure AddItem(value : TDicomUserData);

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomUserData) : Integer;

    {
       Insert TDicomUserData before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TDicomUserData;

    {
       Insert an existing TDicomUserData before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TDicomUserData);

    {
       Get the iIndexth TDicomUserData. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomUserData;

    {
       Set the iIndexth TDicomUserData. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TDicomUserData);

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

    property Elements[iIndex : Integer] : TDicomUserData read GetElements;default;
  End;

  {
  A request for the establishment of an association between two Application
  Entities (AEs)
  }
  TDicomAssociateRequestPDU = Class (TDicomPDU)
  Private
    FCalledEntity: TDicomString;
    FCallingEntity: TDicomString;
    FApplicationContext: TDicomString;
    FPresentationContexts: TDicomPresentationContextInfoList;
    FMaxLength: Cardinal;
    FUserData: TDicomUserDataList;
    Procedure SetApplicationContext(Const Value: TDicomString);
    Procedure SetCalledEntity(Const Value: TDicomString);
    Procedure SetCallingEntity(Const Value: TDicomString);
  
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TDicomAssociateRequestPDU; Overload;
    Function Clone : TDicomAssociateRequestPDU; Overload;
    Procedure Assign(oSource : TFslObject); Override;

  Published
  {
    This parameter identifies the Application Entity  that shall contain the 
  intended acceptor of the A-ASSOCIATE service. It is based on the 
  Destination DICOM Application Name. 
  The Called AE title may or may not be the same as the Receiver Address 
  present in DICOM Messages exchanged over the association. 
    }
    Property CalledEntity : TDicomString Read FCalledEntity Write SetCalledEntity;
    
  {
    This parameter identifies the Application Entity (AE) that shall contain 
  the requestor of the A-ASSOCIATE service. It is based on the Source DICOM 
  Application Name.
  The Calling AE title may or may not be the same as the Initiator Address 
  present in DICOM Messages exchanged over the association.
    }
    Property CallingEntity : TDicomString Read FCallingEntity Write SetCallingEntity;
    
  {
    This parameter identifies the application context proposed by the requestor. 
  The acceptor shall return either the same or a different name. 
  The returned name shall specify the application context to be used 
  for this association
    }
    Property ApplicationContext : TDicomString Read FApplicationContext Write SetApplicationContext;
    
  {
    This parameter used in an A-ASSOCIATE request or indication shall consist 
  of a list containing one or more presentation contexts. Each item shall  
  contain three components,  a presentation context identification, 
  an Abstract Syntax Name, and a list of one or more Transfer Syntax Names. 
    }
    Property PresentationContexts : TDicomPresentationContextInfoList Read FPresentationContexts;
    
  {
    This negotiation allows the receivers to limit the size of the 
  Presentation Data Values List parameters of each P-DATA Indication. 
  The association-requestor shall specify in the user information parameter
  of the A-ASSOCIATE request primitive the maximum length in bytes for 
  the PDV list parameter it is ready to receive in each P-DATA indication. 
  The association-acceptor shall ensure in its fragmentation of the DICOM 
  Messages that the list of PDVs included in each P-DATA request does not 
  exceed this maximum length.
  
  Different maximum lengths can be specified for each direction of data flow 
  on the association. 
    }
    Property MaxLength : Cardinal Read FMaxLength Write FMaxLength;
    
  {
    This parameter shall be used by the requestor and the acceptor of 
  the association to include DICOM Application Entity user information. 
  Its meaning shall depend on the application context that accompanies
  the primitive
    }
    Property UserData : TDicomUserDataList Read FUserData;
  End;

  {
  An association accept response for the establishment request of an association 
  between two Application Entities (AEs)
  }
  TDicomAssociateAcceptPDU = class (TDicomPDU)
  private
    FCalledEntity: TDicomString;
    FCallingEntity: TDicomString;
    FApplicationContext: TDicomString;
    FPresentationContexts: TDicomPresentationAcceptContextInfoList;
    FMaxLength: Cardinal;
    FUserData: TDicomUserDataList;
    procedure SetApplicationContext(const Value: TDicomString);
    procedure SetCalledEntity(const Value: TDicomString);
    procedure SetCallingEntity(const Value: TDicomString);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TDicomAssociateAcceptPDU; Overload;
    Function Clone : TDicomAssociateAcceptPDU; Overload;
    Procedure Assign(oSource : TFslObject); Override;
  
  Published    
  {
    This parameter identifies the Application Entity  that shall contain the 
  intended acceptor of the A-ASSOCIATE service. It is based on the 
  Destination DICOM Application Name. 
  The Called AE title may or may not be the same as the Receiver Address 
  present in DICOM Messages exchanged over the association. 
    }
    Property CalledEntity : TDicomString Read FCalledEntity Write SetCalledEntity;
    
  {
    This parameter identifies the Application Entity (AE) that shall contain
  the requestor of the A-ASSOCIATE service. It is based on the Source DICOM 
  Application Name. 
  The Calling AE title may or may not be the same as the Initiator Address 
  present in DICOM Messages exchanged over the association.
    }
    Property CallingEntity : TDicomString Read FCallingEntity Write SetCallingEntity;

  {
    This parameter identifies the application context proposed by the requestor. 
  The acceptor shall return either the same or a different name. 
  The returned name shall specify the application context to be used 
  for this association
    }
    Property ApplicationContext : TDicomString Read FApplicationContext Write SetApplicationContext;
  
  {
    This parameter used in the A-ASSOCIATE Response and Confirmation indicates 
  the acceptance or rejection of each of the presentation context definitions 
  proposed in the presentation context definition list parameter 
  (Section 7.1.1.13). 
  The Presentation Context Definition Result List parameter shall take the 
  form of a list of result values. There is a one to one correspondence 
  between each one of these result values and each of the presentation 
  contexts proposed in the Presentation Context Definition List parameter. 
  Each result value represents either "acceptance", "user-rejection", or 
  "provider-rejection". The values of the results are assigned by the Upper 
  Layer (UL) user on the response service primitive. The result values may be 
  sent in any order. 
    }
  Property PresentationContexts : TDicomPresentationAcceptContextInfoList Read FPresentationContexts;
    
  {
    the association-acceptor can specify in the user information parameter of 
  A-ASSOCIATE response primitive the maximum length in bytes for the PDV 
  list parameter it is ready to receive in each P-DATA indication. 
  The association-requestor shall ensure in its fragmentation of the DICOM 
  Messages that the list of PDVs included in each P-DATA request does not 
  exceed this maximum length.
  
  Different maximum lengths can be specified for each direction of data flow 
  on the association.
    }
  Property MaxLength : Cardinal Read FMaxLength Write FMaxLength;
    
  {
    This parameter shall be used by the requestor and the acceptor of 
  the association to include DICOM Application Entity user information. 
  Its meaning shall depend on the application context that accompanies 
  the primitive
    }
    Property UserData : TDicomUserDataList Read FUserData;
  End;

  {
  List of possible association reject types (permantent or transient)
  }
  TDicomAssociateRejectResult = (RejectedPermanent, RejectedTransient);
  
  {
  List of possible association reject source (user, acse or presentation)
  }
  TDicomAssociateRejectSource = (RejectedUser, RejectedACSE, RejectedPresentation);
  
  {
  List of possible association reject reasons
  (No reason, not support, calling AE Not known, Protocol version not support,
  temporary congestion or local limit exceeded)
  }
  TDicomAssociateRejectReason = (RejectNoReason, AppContextNotSupported, CallingAENotKnown, CalledAENotKnown, ProtocolVersionNotSupported, TemporaryCongestion, LocalLimitExceeded);

  {
  An association reject response for the establishment request of an association 
  between two Application Entities (AEs)
  }
  TDicomAssociateRejectPDU = class (TDicomPDU)
  private
    FReason: TDicomAssociateRejectReason;
    FResult: TDicomAssociateRejectResult;
    FSource: TDicomAssociateRejectSource;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    Function Link : TDicomAssociateRejectPDU; Overload;
    Function Clone : TDicomAssociateRejectPDU; Overload;
    Procedure Assign(oSource : TFslObject); Override;
  
  Published
    
  {
    This parameter shall be provided either by the acceptor of the A-ASSOCIATE 
  request, or by the Upper Layer (UL) service-provider. It shall indicate 
  the result of using the A-ASSOCIATE service.
    }
    Property Result : TDicomAssociateRejectResult Read FResult Write FResult;
    
  {
    The value of the parameter is supplied by the Upper Layer (UL) 
  service-provider. It identifies the creating source of the Result parameter 
  and the Diagnostic parameter, if present.
    }
    Property Source : TDicomAssociateRejectSource Read FSource Write FSource;
    
  {
    It shall be used to provide diagnostic information about the result of the 
  A-ASSOCIATE service (i.e. reason to reject)
    }
    Property Reason : TDicomAssociateRejectReason Read FReason Write FReason;
  End;

  {
  DICOM Messages are encapsulated in P-DATA request primitives as the user data 
  of Presentation Data Values (PDV). A DICOM Message is fragmented in
  Command Fragments and Data Fragments, each placed in a PDV. The same
  presentation context shall be used for every fragment of the same message
  (i.e. same Presentation Context ID for the user data of the PDVs containing
  the fragments of a same message)
  }
  TDicomPresentationDataValue = class (TFslObject)
  Private
    FCommand: Boolean;
    FLast: Boolean;
    FBytes: TBytes;
    FOffsetEnd: Cardinal;
    FOffsetStart: Cardinal;
  Public
    Function Link : TDicomPresentationDataValue; Overload;
    Function Clone : TDicomPresentationDataValue; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Property OffsetStart : Cardinal Read FOffsetStart Write FOffsetStart;
    Property OffsetEnd : Cardinal Read FOffsetEnd Write FOffsetEnd;

// TODO:
  {
    The message fragment
    }
    Property Bytes : TBytes Read FBytes Write FBytes;
  Published

  {
    Whether the fragment is of the Command or Data type
    }
    Property Command : Boolean Read FCommand Write FCommand;

  {
    Whether the fragment is or is not the last fragment of a
  Command/Data Stream of a DICOM Message
    }
    Property Last : Boolean Read FLast Write FLast;


  End;

  {
  A list of Presentation Data Values (PDV)
  }
  TDicomPresentationDataValueList = class (TFslObjectList)
  Private
    function GetElements(iIndex: Integer): TDicomPresentationDataValue;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public

    Procedure AddFromBuffer(bCommand : Boolean; oBuffer : TFslBuffer);

    {
      Add a TDicomPresentationDataValue to the End of the list.
    }
    Function Append : TDicomPresentationDataValue;

    {
      Add an already existing TDicomPresentationDataValue to the End of the list.
    }
    Procedure AddItem(value : TDicomPresentationDataValue);

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomPresentationDataValue) : Integer;

    {
       Insert TDicomPresentationDataValue before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TDicomPresentationDataValue;

    {
       Insert an existing TDicomPresentationDataValue before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TDicomPresentationDataValue);

    {
       Get the iIndexth TDicomPresentationDataValue. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomPresentationDataValue;

    {
       Set the iIndexth TDicomPresentationDataValue. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TDicomPresentationDataValue);

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

    property Elements[iIndex : Integer] : TDicomPresentationDataValue Read GetElements;default;
  End;

  {
  This Presentation P-DATA Service shall be used by either AE to cause
  the exchange of application information (i.e. DICOM Messages). DICOM Messages
  shall be exchanged as defined in PS 3.7. An association provides a
  simultaneous bi-directional exchange of P-DATA request/indication primitives.
  }
  TDicomDataPDU = class (TDicomPDU)
  private
    FContextId: Byte;
    FDataValues : TDicomPresentationDataValueList;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Function Link : TDicomDataPDU; Overload;
    Function Clone : TDicomDataPDU; Overload;
    Procedure Assign(oSource : TFslObject); Override;

  Published
    {
    The same presentation context id that is applied to all
  Presentation Data Values (PDV) in this PDU
    }
    Property ContextId : Byte Read FContextId Write FContextId;

  {
    The list of Presentation Data Values (PDV) that contains the message
  fragments
    }
    Property DataValues : TDicomPresentationDataValueList Read FDataValues;
  End;

  {
  The graceful release of an association between two AEs shall be performed
  through ACSE A-RELEASE request/response primitives.
  This is the request.
  }
  TDicomReleaseRequestPDU = class (TDicomPDU)
  Public
    constructor Create; Override;
    Function Link : TDicomReleaseRequestPDU; Overload;
    Function Clone : TDicomReleaseRequestPDU; Overload;
  End;

  {
  The graceful release of an association between two AEs shall be performed 
  through ACSE A-RELEASE request/response primitives. 
  This is the response.
  }
  TDicomReleaseResponsePDU = class (TDicomPDU)
  Public
    constructor Create; Override;
    Function Link : TDicomReleaseResponsePDU; Overload;
    Function Clone : TDicomReleaseResponsePDU; Overload;
  End;

  {
  List of possible PDU types (by User or Provider)
  }
  TDicomAbortSource = (abortUser, abortProvider);

  {
  List of possible abort reasons
  (Not specified, unrecognised PDU, unexpected PDU, unregconised parameter,
  unexpected parameter, invalid parameter)
  }
  TDicomAbortReason = (abortNotSpecified, abortUnrecognisedPDU, abortUnexpectedPDU, abortUnrecognisedParameter, abortUnexpectedParameter, abortInvalidParameter);

  EDicomAbort = class (EDicomException)
  private
    FSource : TDicomAbortSource;// = (abortUser, abortProvider);
    FReason : TDicomAbortReason;// = (abortNotSpecified, abortUnrecognisedPDU, abortUnexpectedPDU, abortUnrecognisedParameter, abortUnexpectedParameter, abortInvalidParameter);
  Public
    constructor Create(aSource : TDicomAbortSource; aReason : TDicomAbortReason);
    Property Source : TDicomAbortSource read FSource;
    Property Reason : TDicomAbortReason read FReason;
  End;


  {
  The ACSE A-ABORT service shall be used by a requestor in either of the AEs 
  to cause the abnormal release of the association. It shall be a non-confirmed 
  service. However, because of the possibility of an A-ABORT service procedure 
  collision, the delivery of the indication primitive is not guaranteed. Should 
  such a collision occur, both AEs are aware that the association has been terminated.
  }
  TDicomAbortPDU = class (TDicomPDU)
  private
    FReason: TDicomAbortReason;
    FSource: TDicomAbortSource;
  Public
    constructor Create; Override;
    Function Link : TDicomAbortPDU; Overload;
    Function Clone : TDicomAbortPDU; Overload;
    Procedure Assign(oSource : TFslObject); Override;
  
  Published

  {
    This parameter indicates the initiating source of  this abort. It shall
  take one of the following symbolic values: 
  a)  UL service-user 
  b)  UL service-provider (ACSE related) 
    }
    Property Source : TDicomAbortSource Read FSource Write FSource;
    
  {
    Provide a diagnostic reason for the abort
    }
    Property Reason : TDicomAbortReason Read FReason Write FReason;
  End;

  {
  The DICOM File Format provides a means to encapsulate in a file the 
  Data Set representing a SOP Instance related to a DICOM IOD. The byte stream 
  of the Data Set is placed into the file after the DICOM File Meta Information.  
  Each file contains a single SOP Instance. 
  }
  TDicomFile = class (TFslObject)
  private
    FName: String;
    FContent: TDicomObject;
    FHeader: TDicomObject;
    FPrelude: TBytes;

    Function GetHeaderValue(group, element : Word): TDicomValue;
    procedure SetContent(const Value: TDicomObject);
    procedure SetHeader(const Value: TDicomObject);

  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;

    Function Link : TDicomFile; Overload;
    Function Clone : TDicomFile; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    {
      This is a 2-byte field where each bit identifies
      a version of this File Meta Information hea-
      der. In current DICOM version, the first byte
      value is 00H and the second byte value is 01H
      Media Storage
    }
    Function FileMetaInformation : Word; // (0002,0001) OB

    {
    Uniquely identifies the SOP Class associated
    with the DICOM data object. The value of
    this element is based on the image modality
    type; in other words, it should contain the C-
    Store SOP for image modality (see 7.3)
    }
    Function SOPClassUID : String; // (0002,0002) UI

    {
    Uniquely identifies the SOP Instance associated with the DICOM data object placed in
    the file and following the File Meta Information. This value comes from the Image SOP
    }
    Function MediaStorageSOPInstanceUID : String; // (0002,0003) UI

    {
      UI Uniquely identifies the Transfer Syntax used
      to encode the following data object. This
      Transfer Syntax does not apply to the File
      Meta Information
    }
    Function TransferSyntaxUID : String; // (0002,0010)

    {
      Uniquely identifies the implementation that
      wrote the file and its content. It provides an
      unambiguous identification of the type of
      implementation that last wrote the file in the
      event of interchange problems
      Same value as used in Implementation iden-
      tification Item at association establishment
    }
    Function ImplementationClassUID : String; // (0002,0012)

    {
      Identifies a version for an Implementation Class UID using up to 16
      characters. Same value as used in Implementation identification Item
      at association establishment
    }
    Function ImplementationVersionName : String; // (0002,0013)

    {
      The DICOM AET of the AE that wrote this
      files content (or last updated it). If used, it
      allows the source of errors to be traced in the
      event of media interchange problems
      Same value as used in Calling AET at association establishment
    }
    Function SourceAET : String; // (0002,0016)

// TODO
  {
    The 128 byte File Preamble is available for use as defined by Application
  Profiles or specific implementations. This Part of the DICOM Standard
  does not require any structure for this fixed size Preamble.  It is not
  required to be structured as a DICOM Data Element with a Tag and a Length.
  It is intended to facilitate access to the images and other data in the
  DICOM file by providing compatibility with a number of commonly used
  computer image file formats.  Whether or not the File Preamble contains
  information, the DICOM File content shall conform to the requirements of
  this Part and the Data Set shall conform to the SOP Class specified in the
  File Meta Information.
    }
    Property Prelude : TBytes Read FPrelude Write FPrelude;

  Published

    {
    The file name, may be null if this is not an actual file on storage disk
    }
    Property Name : String Read FName Write FName;


  {
    A set of DICOM Meta Elements with Tags and Lengths as defined in PS3.10,
  Table 7.1-1
    }
    Property Header : TDicomObject Read FHeader Write SetHeader;

  {
    The contained Data Set, representing a single SOP Instance related to a
  single SOP Class (and corresponding IOD).
    }
    Property Content : TDicomObject Read FContent Write SetContent;
  End;

  {
  A dicom message, with a command a data object (A SOP pair)
  }
  TDicomMessage = class (TFslObject)
  Private
    FData: TDicomObject;
    FCommand: TDicomObject;
    FAbstractSyntax : String;
    FTransferSyntax : String;
    procedure SetCommand(const Value: TDicomObject);
    procedure SetData(const Value: TDicomObject);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;

    Function Clone : TDicomMessage; Overload;
    Function Link : TDicomMessage; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Procedure Clear;

    {
      read the element that carries the message id in the command
    }
    function ReadMessageId : String;

    {
      read the element that carries the patient id in the data
    }
    Function ReadPatientId : String;

    {
      read the element that carries the command id (type of command) in the command.
    }
    Function ReadCommandId : Integer;

    {
      read the SOP Class as specified in the command
    }
    function ReadSOPClass : String;

    {
      read the SOP Instance as specified in the command
    }
    function ReadSOPInstance : String;

    {
      read the message status as specified in the command
    }
    function ReadStatus : Integer;

    {
      The size of the object when parsed.

      The size will be 0 if the object is being built instead of parsed.

      The size of the object may be wrong if script has modified it's contents after parsing
    }
    Function Size : Cardinal;
  Published

    {
      The command part of the message
    }
    Property Command : TDicomObject read FCommand write SetCommand;

    {
      The data part of the message
    }
    Property Data : TDicomObject read FData write SetData;


    {
      The AbstractSyntax (SOP) of the message.

      Note that changing this does not change the contents of the message
    }
    Property AbstractSyntax : String read FAbstractSyntax write FAbstractSyntax;

    {
      The TransferSyntax (SOP) of the message.

      Note that changing this does not change the contents of the message. To change
      the actual transfer syntax of the message, use the DicomFactory.
    }
    Property TransferSyntax : String read FTransferSyntax write FTransferSyntax;
  End;


  {
  A list of Presentation Data Values (PDV)
  }
  TDicomMessageList = class (TFslObjectList)
  Private
    function GetElements(iIndex: Integer): TDicomMessage;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : TDicomMessageList; overload;

    {
      Add a TDicomMessage to the End of the list.
    }
    Function Append : TDicomMessage;

    {
      Add an already existing TDicomMessage to the End of the list.
    }
    Procedure AddItem(value : TDicomMessage);

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomMessage) : Integer;

    {
       Insert TDicomMessage before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TDicomMessage;

    {
       Insert an existing TDicomMessage before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TDicomMessage);

    {
       Get the iIndexth TDicomMessage. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomMessage;

    {
       Set the iIndexth TDicomMessage. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TDicomMessage);

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

    property Elements[iIndex : Integer] : TDicomMessage Read GetElements;default;
  End;


  {
  List of DICOM instance object type
  }
  TDicomInstanceType = (ditNull, ditSimpleObject, ditMessage, ditFileObject, ditAssociateRequestPDU, ditAssociateAcceptPDU, ditAssociateRejectPDU, ditDataPDU, ditAbortPDU, ditReleaseRequestPDU, ditReleaseResponsePDU);

  {
  A dicom object instance.

  This acts as a wrapper, which could be a file object, a straight data object
  or one of the PDU.
  }
  TDicomInstance = class (TFslObject)
  private
    FSimpleObject : TDicomObject;
    FFileObject : TDicomFile;
    FAssociateRequest : TDicomAssociateRequestPDU;
    FAssociateAccept : TDicomAssociateAcceptPDU;
    FAssociateReject : TDicomAssociateRejectPDU;
    FData : TDicomDataPDU;
    FAbort : TDicomAbortPDU;
    FReleaseRequest : TDicomReleaseRequestPDU;
    FReleaseResponse : TDicomReleaseResponsePDU;
    FMessage: TDicomMessage;

    procedure SetSimpleObject(const Value: TDicomObject);
    procedure SetFileObject(const Value: TDicomFile);
    procedure SetAbort(const Value: TDicomAbortPDU);
    procedure SetAssociateAccept(const Value: TDicomAssociateAcceptPDU);
    procedure SetAssociateReject(const Value: TDicomAssociateRejectPDU);
    procedure SetAssociateRequest(const Value: TDicomAssociateRequestPDU);
    procedure SetData(const Value: TDicomDataPDU);
    procedure SetReleaseRequest(const Value: TDicomReleaseRequestPDU);
    procedure SetReleaseResponse(const Value: TDicomReleaseResponsePDU);
    function GetInstanceType: TDicomInstanceType;
    procedure SetMessage(const Value: TDicomMessage);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;

    Function Clone : TDicomInstance; Overload;
    Function Link : TDicomInstance; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    Procedure Clear;

  Published

    {
    The DICOM object type
    }
    Property InstanceType : TDicomInstanceType Read GetInstanceType;

  {
    The wrapped simple DICOM object
    }
    Property SimpleObject : TDicomObject Read FSimpleObject Write SetSimpleObject;


    {
    The wrapped message (Command + Data)
    }
    Property Message : TDicomMessage Read FMessage write SetMessage;
  {
    The wrapped file object
    }
    Property FileObject : TDicomFile Read FFileObject Write SetFileObject;

  {
    The wrapped associate request
    }
    Property AssociateRequest : TDicomAssociateRequestPDU Read FAssociateRequest Write SetAssociateRequest;

  {
    The wrapped associate accept
    }
    Property AssociateAccept : TDicomAssociateAcceptPDU Read FAssociateAccept Write SetAssociateAccept;

  {
    The wrapped associatie reject
    }
    Property AssociateReject : TDicomAssociateRejectPDU Read FAssociateReject Write SetAssociateReject;

  {
    The wrapped data PDU
    }
    Property Data : TDicomDataPDU Read FData Write SetData;

  {
    The wrapped abort PDU
    }
    Property Abort : TDicomAbortPDU Read FAbort Write SetAbort;

  {
    The wrapped release request
    }
    Property ReleaseRequest : TDicomReleaseRequestPDU Read FReleaseRequest Write SetReleaseRequest;

  {
    The wrapped release response
    }
    Property ReleaseResponse : TDicomReleaseResponsePDU Read FReleaseResponse Write SetReleaseResponse;
  End;

Const
  BYTE_VALUES_PresentationAcceptResult : array [TPresentationAcceptResult] of Byte = (0, 1, 2, 3, 4);

  BYTE_VALUES_DicomAssociateRejectResult : array [TDicomAssociateRejectResult] of Byte = (1, 2);
  BYTE_VALUES_DicomAssociateRejectSource : array [TDicomAssociateRejectSource] of Byte = (1, 2, 3);
  BYTE_VALUES_DicomAssociateRejectReason_Souce_User : array [TDicomAssociateRejectReason] of Byte = (1, 2, 3, 7, 0, 0, 0);
  BYTE_VALUES_DicomAssociateRejectReason_Souce_ACSE : array [TDicomAssociateRejectReason] of Byte = (1, 0, 0, 0, 2, 0, 0);
  BYTE_VALUES_DicomAssociateRejectReason_Souce_Presentation : array [TDicomAssociateRejectReason] of Byte = (1, 0, 0, 0, 0, 1, 2);
  BYTE_VALUES_DicomAbortSource : array [TDicomAbortSource] of Byte = (0, 2);
  BYTE_VALUES_DicomAbortReason : array [TDicomAbortReason] of Byte = (0, 1, 2, 4, 5, 6);

  NAMES_PresentationAcceptResult : array [TPresentationAcceptResult] of String = ('Accept', 'UserRejection', 'ProviderRejection', 'AbstractSyntax', 'TransferSyntax');
  NAMES_DicomAssociateRejectResult : array [TDicomAssociateRejectResult] of String = ('Permanent', 'Transient');
  NAMES_DicomAssociateRejectSource : array [TDicomAssociateRejectSource] of String = ('User', 'ACSE', 'Presentation');
  NAMES_DicomAssociateRejectReason : array [TDicomAssociateRejectReason] of String = ('None', 'AppContextNotSupported', 'CallingAENotKnown', 'CalledAENotKnown', 'ProtocolVersionNotSupported', 'TemporaryCongestion', 'LocalLimitExceeded');
  NAMES_DicomAbortSource : array [TDicomAbortSource] of String = ('User', 'Provider');
  NAMES_DicomAbortReason : array [TDicomAbortReason] of String = ('NotSpecified', 'UnrecognisedPDU', 'UnexpectedPDU', 'UnrecognisedParameter', 'UnexpectedParameter', 'InvalidParameter');

  NAMES_TDicomInstanceType : array [TDicomInstanceType] of String = ('Null', 'Simple Object', 'Message', 'File Object', 'Associate Request', 'Associate Accept', 'Associate Reject', 'Data', 'Abort', 'Release Request', 'Release Response');
  NAMES_TDicomPDUType : array [TDicomPDUType] of String = ('Associate Request', 'Associate Accept', 'Associate Reject', 'Data', 'Abort', 'Release Request', 'Release Response');

Implementation


{ TDicomDataElement }

Constructor TDicomDataElement.Create(oDictionary : TDicomDictionary; aKnownType : TDicomVRType; aPossibleTypes : TDicomVRTypes);
Begin
  Inherited Create;
  FIsSimple := True;
  FDictionary := oDictionary;
  FValues := TDicomValueList.Create(FDictionary.Link, aKnownType, aPossibleTypes);
End;

Constructor TDicomDataElement.CreateComplex(oDictionary : TDicomDictionary);
Begin
  Inherited Create;
  FIsSimple := False;
  FDictionary := oDictionary;
  FObjects := TDicomObjectList.Create;
End;

destructor TDicomDataElement.Destroy;
Begin
  FDictionary.Free;
  FElementDefinition.Free;
  FObjects.Free;
  FValues.Free;
  inherited;
End;

Procedure TDicomDataElement.Assign(oSource: TFslObject);
Begin
  inherited;
  FIsSimple := TDicomDataElement(oSource).FIsSimple;
  FGroupId := TDicomDataElement(oSource).FGroupId;
  FElementId := TDicomDataElement(oSource).FElementId;
  FValues := TDicomDataElement(oSource).FValues.Clone;
  FObjects := TDicomDataElement(oSource).FObjects.Clone;
End;

Function TDicomDataElement.Clone: TDicomDataElement;
Begin
  result := TDicomDataElement(Inherited Clone);
End;

Function TDicomDataElement.Link: TDicomDataElement;
Begin
  result := TDicomDataElement(Inherited Link);
End;

Procedure TDicomDataElement.SetElementDefinition(const Value: TDicomDictionaryElement);
Begin
  FElementDefinition.Free;
  FElementDefinition := Value;
End;

Procedure TDicomDataElement.SetDictionary(const Value: TDicomDictionary);
Begin
  FDictionary.Free;
  FDictionary := Value;
End;

function TDicomDataElement.Size: Cardinal;
begin
  result := FOffsetEnd - FOffsetStart;
end;

function TDicomDataElement.SingleValue: TDicomValue;
begin
  result := nil;
  if self <> nil then
    if not FIsSimple Then
      RaiseError('SingleValue', 'not simple')
    else if FValues.Count > 1 then
      RaiseError('SingleValue', 'multi valued')
    else if FValues.Count = 1 then
      result := FValues[0];

end;

function TDicomDataElement.Tag: String;
begin
  result := FormatTagValues(FGroupId, FElementId);
end;

function TDicomDataElement.AsString: String;
var
  oBuilder : TFslStringBuilder;
  i : Integer;
begin
  if not HasValues And not HasObjects Then
    result := ''
  Else if HasValues and (Values.Count = 1) Then
    result := Values[0].AsString
  Else
  Begin
    oBuilder := TFslStringBuilder.Create;
    Try
      if HasValues Then
      Begin
        oBuilder.Append(FValues[0].AsString);
        For i := 1 to FValues.Count - 1 Do
        Begin
          oBuilder.Append(' | ');
          oBuilder.Append(FValues[i].AsString)
        End;
      End
      Else
      Begin
        oBuilder.Append(FObjects[0].AsString);
        For i := 1 to FValues.Count - 1 Do
        Begin
          oBuilder.Append(' | ');
          oBuilder.Append(FObjects[i].AsString)
        End;
      End;
      result := oBuilder.AsString;
    Finally
      oBuilder.Free;
    End;
  End;
end;

function TDicomDataElement.HasObjects: Boolean;
begin
  result := (FObjects <> nil) and (FObjects.Count > 0);
end;

function TDicomDataElement.HasValues: Boolean;
begin
  result := (FValues <> nil) and (FValues.Count > 0);
end;

function TDicomDataElement.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FElementDefinition.sizeInBytes);
  inc(result, FObjects.sizeInBytes);
  inc(result, FValues.sizeInBytes);
  inc(result, FDictionary.sizeInBytes);
end;

{ TDicomDataElementList }

Procedure TDicomDataElementList.AddItem(value: TDicomDataElement);
Begin
  Add(value);
End;

Procedure TDicomDataElementList.ClearItems;
Begin
  Inherited InternalClear;
End;

Function TDicomDataElementList.Clone: TDicomDataElementList;
Begin
  result := TDicomDataElementList(Inherited Clone);
End;

Function TDicomDataElementList.CompareByTags(pA, pB: Pointer): Integer;
Begin
  Result := IntegerCompare(TDicomDataElement(pA).FGroupId, TDicomDataElement(pB).FGroupId);
  if result = 0 Then
    Result := IntegerCompare(TDicomDataElement(pA).FElementId, TDicomDataElement(pB).FElementId);
End;

Function TDicomDataElementList.Count: Integer;
Begin
  Result := Inherited Count;
End;

Constructor TDicomDataElementList.Create(oDictionary: TDicomDictionary);
Begin
  Inherited Create;

  FDictionary := oDictionary;
  SortedByTags;
End;

Procedure TDicomDataElementList.DefaultCompare(out aEvent: TFslItemListCompare);
Begin
  aEvent := CompareByTags;
End;

Destructor TDicomDataElementList.Destroy;
Begin
  FDictionary.Free;

  Inherited;
End;

function TDicomDataElementList.ExistsByTag(tag: String): Boolean;
var
  group, element : Word;
begin
  ReadTagValues('GetByTag', tag, group, element);
  result := ExistsByTags(group, element);
end;

Function TDicomDataElementList.ExistsByTags(const group, element: Word): Boolean;
Begin
  Result := ExistsByIndex(IndexByTags(group, element));
End;

Function TDicomDataElementList.FindByTags(const group, element: Word; out iIndex: Integer): Boolean;
Var
  oElement : TDicomDataElement;
Begin
  oElement := TDicomDataElement(ItemNew);
  Try
    oElement.FGroupId := group;
    oElement.FElementId := element;

    Result := Find(oElement, iIndex, CompareByTags);
  Finally
    oElement.Free;
  End;
End;

function TDicomDataElementList.GetByTag(tag: String): TDicomDataElement;
var
  group, element : Word;
begin
  ReadTagValues('GetByTag', tag, group, element);
  result := GetByTags(group, element);
end;

Function TDicomDataElementList.GetByTags(const group, element: Word): TDicomDataElement;
Var
  iIndex : Integer;
Begin
  If FindByTags(group, element, iIndex) Then
    Result := TDicomDataElement(ObjectByIndex[iIndex])
  Else
    Result := Nil;
End;

Function TDicomDataElementList.GetElements(iIndex: integer): TDicomDataElement;
Begin
  result := TDicomDataElement(ObjectByIndex[iIndex]);
End;

Function TDicomDataElementList.IndexByTags(const group, element: Word): Integer;
Begin
  If Not FindByTags(group, element, Result) Then
    Result := -1;
End;

Function TDicomDataElementList.IndexOf(value: TDicomDataElement): Integer;
Begin
  Result := IndexByReference(value);
End;

Procedure TDicomDataElementList.InsertItem(iIndex: Integer; value: TDicomDataElement);
Begin
  Inherited Insert(iIndex, value);
End;

Function TDicomDataElementList.IsSortedByTags: Boolean;
Begin
  Result := IsSortedBy(CompareByTags);
End;

Function TDicomDataElementList.Item(iIndex: Integer): TDicomDataElement;
Begin
  Result := Elements[iIndex];
End;

Function TDicomDataElementList.ItemClass: TFslObjectClass;
Begin
  result := TDicomDataElement;
End;

Function TDicomDataElementList.Link: TDicomDataElementList;
Begin
  result := TDicomDataElementList(Inherited Link);
End;

Procedure TDicomDataElementList.Remove(iIndex: Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TDicomDataElementList.RemoveByTags(const group, element: Word);
Var
  iIndex : Integer;
Begin
  If Not FindByTags(group, element, iIndex) Then
    RaiseError('RemoveByTags', StringFormat('Object ''%d'', ''%d'' not found in list.', [group, element]));

  DeleteByIndex(iIndex);
End;

Procedure TDicomDataElementList.SetItemByIndex(iIndex: Integer; value: TDicomDataElement);
Begin
  ObjectByIndex[iIndex] := value;
End;

Procedure TDicomDataElementList.SortedByTags;
Begin
  SortedBy(CompareByTags);
End;

function TDicomDataElementList.AsString(includeTags: Boolean): String;
var
  oBuilder : TFslStringBuilder;
  oElem : TDicomDataElement;
  i, j : integer;
begin
  oBuilder := TFslStringBuilder.Create;
  Try
    for i := 0 to Count - 1 Do
    Begin
      oElem := Elements[i];
      if i > 0 Then
        oBuilder.Append(';');
      if includeTags Then
        oBuilder.Append(oElem.Tag+'=');
      for j := 0 to oElem.Values.Count - 1 Do
      Begin
        if j > 0 Then
          oBuilder.Append(',');
        oBuilder.Append(oElem.Values[j].AsString);
      End;
    End;
    result := oBuilder.AsString;
  Finally
    oBuilder.Free;
  End;
end;

function TDicomDataElementList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDictionary.sizeInBytes);
end;

{ TDicomObject }

Procedure TDicomObject.Assign(oSource: TFslObject);
Begin
  inherited;
  FElements.Assign(TDicomObject(oSource).FElements);
End;

Function TDicomObject.Clone: TDicomObject;
Begin
  result := TDicomObject(inherited Clone);
End;

Constructor TDicomObject.Create(oDictionary: TDicomDictionary);
Begin
  FDictionary := oDictionary;
  Create;
End;

destructor TDicomObject.Destroy;
Begin
  FDictionary.Free;
  FElements.Free;
  inherited;
End;

function TDicomObject.ElementAsString(tagId: String): String;
var
  oElem : TDicomDataElement;
begin
  oElem := FElements.GetByTag(tagId);
  if oElem = nil then
    result := ''
  Else
    result := oElem.AsString;
end;

Function TDicomObject.Link: TDicomObject;
Begin
  result := TDicomObject(inherited Link);
End;

function TDicomObject.MakeElement(sTag : String; sValue : String): TDicomDataElement;
  Procedure Add(s : String);
  var
    oVR : TDicomValue;
  Begin
    oVr := TDicomValue.Create(Elements.FDictionary.Link, Result.values.KnownType, Result.values.PossibleTypes);
    Try
      oVr.AsString := s;
      result.values.Add(oVr.Link);
    Finally
      oVr.Free;
    End;
  End;
var
  iGroupId, iElementId : Word;
  oDefn : TDicomDictionaryElement;
  oVRDefn : TDicomDictionaryVR;
  s : String;
Begin
  ReadTagValues('Factory', sTag, iGroupId, iElementId);
  oDefn := Elements.FDictionary.FindMatchingElement(iGroupId, iElementId, true);
  result := TDicomDataElement.Create(Elements.FDictionary.Link, PreferredVRType(oDefn.VRTypes), oDefn.VRTypes);
  Try
    result.GroupId := iGroupId;
    result.ElementId := iElementId;
    result.ElementDefinition := oDefn.Link;
    oVRdefn := Elements.FDictionary.VRSet[Result.values.KnownType];
    if oVRDefn.Repeatable And (sValue <> '') Then
    Begin
      while sValue <> '' Do
      Begin
        StringSplit(sValue, REP_CHAR, s, sValue);
        Add(s);
      End;
    End
    Else
      Add(sValue);
    result.Link;
  Finally
    result.Free;
  End;
end;

function TDicomObject.MakeElement(sTag : String): TDicomDataElement;
var
  iGroupId, iElementId : Word;
  oDefn : TDicomDictionaryElement;
Begin
  ReadTagValues('Factory', sTag, iGroupId, iElementId);
  oDefn := Elements.FDictionary.FindMatchingElement(iGroupId, iElementId, true);
  result := TDicomDataElement.Create(Elements.FDictionary.Link, PreferredVRType(oDefn.VRTypes), oDefn.VRTypes);
  Try
    result.GroupId := iGroupId;
    result.ElementId := iElementId;
    result.ElementDefinition := oDefn.Link;
    result.Link;
  Finally
    result.Free;
  End;
end;

Procedure TDicomObject.ExecuteQuery(contexts : TDicomObjectList; results : TDicomDataElementList; path : String; Cursor : Integer; bDescendents : Boolean);
var
  work : String;
  tail : String;
  i : integer;
  tag : String;
 // filter : String;
  local : TDicomDataElementList;
  oElem : TDicomDataElement;
Begin
  work := copy(path, cursor, length(path)-(cursor-1));
  i := pos('/', work);
  if i > 0 Then
  Begin
    tail := copy(work, i, length(work)-i);
    work := copy(work, 1, i-1);
  End;
  if pos('[', work) > 0 Then
    raise EDicomException.create('filters are not yet supported')
  Else
    tag := work;
  local := TDicomDataElementList.Create(Elements.Dictionary.Link);
  Try
    for i := 0 to contexts.Count - 1 Do
    Begin
      oElem := contexts[i].Elements.GetByTag(tag);
      if oElem <> nil Then
        local.Add(oElem.Link);
    End;

    if tail = '' Then
    Begin
      For i := 0 to local.Count - 1 Do
        results.Add(local[i].Link);
    End
    Else
      raise EDicomException.create('Tail not supported yet');
  Finally
    local.Free;
  End;
End;


function TDicomObject.Query(path: String): TDicomDataElementList;
var
  results : TDicomDataElementList;
  context : TDicomObjectList;
begin
  results := TDicomDataElementList.Create(Elements.Dictionary.Link);
  context := TDicomObjectList.Create;
  Try
    context.Add(self.Link);
    if StringStartsWith(path, '//') Then
      ExecuteQuery(context, results, path, 3, true)
    Else if StringStartsWith(path, '/') Then
      ExecuteQuery(context, results, path, 2, false)
    Else
      ExecuteQuery(context, results, path, 1, false);
    result := results.Link;
  Finally
    results.Free;
    context.Free;
  End;
end;

Function TDicomObject.Sequence(sTag: String) : TDicomObject;
var
  iGroup, iElement : Word;
  iIndex : integer;
  oElement : TDicomDataElement;
begin
  ReadTagValues('DicomFactory.SetElement', sTag, iGroup, iElement);
  iIndex := Elements.IndexByTags(iGroup, iElement);
  if iIndex = -1 Then
  Begin
    oElement := MakeElement(sTag);
    Elements.Add(oElement);
  End
  Else
    oElement := Elements[iIndex];
  result := TDicomObject.Create(Elements.Dictionary.Link);
  oElement.Objects.Add(result);
end;

function TDicomObject.Size: Cardinal;
begin
  result := FOffsetEnd - FOffsetStart;
end;

procedure TDicomObject.Tag(sTag: String; sValue: String);
var
  iGroup, iElement : Word;
  iIndex : integer;
begin
  ReadTagValues('DicomFactory.SetElement', sTag, iGroup, iElement);
  iIndex := Elements.IndexByTags(iGroup, iElement);
  if iIndex > 0 Then
    Elements.DeleteByIndex(iIndex);
  Elements.add(MakeElement(sTag, sValue));
end;

procedure TDicomObject.TagN(sTag: String; sValue: String);
var
  iGroup, iElement : Word;
  iIndex : integer;
  oElement : TDicomDataElement;
begin
  ReadTagValues('DicomFactory.SetElement', sTag, iGroup, iElement);
  iIndex := Elements.IndexByTags(iGroup, iElement);
  if iIndex = -1 Then
  Begin
    oElement := MakeElement(sTag, sValue);
    Elements.Add(oElement);
  End
  Else
    Elements[iIndex].Values.Append.AsString := sValue;
end;

function TDicomObject.AsString: String;
begin
  result := '{object}';
end;

function TDicomObject.v(tagId: String): String;
begin
  result := ElementAsString(tagId);
end;

constructor TDicomObject.Create;
begin
  inherited;
  FElements := TDicomDataElementList.Create(FDictionary.Link);
end;

function TDicomObject.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDictionary.sizeInBytes);
  inc(result, FElements.sizeInBytes);
end;

{ TDicomObjectList }

Constructor TDicomObjectList.Create(oDictionary: TDicomDictionary);
Begin
  Inherited Create;

  FDictionary := oDictionary;
End;

Destructor TDicomObjectList.Destroy;
Begin
  FDictionary.Free;

  Inherited;
End;

Procedure TDicomObjectList.AddItem(value: TDicomObject);
Begin
  Add(value);
End;

Function TDicomObjectList.Append: TDicomObject;
Begin
  Result := TDicomObject.Create(FDictionary.Link);
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TDicomObjectList.ClearItems;
Begin
  Inherited InternalClear;
End;

Function TDicomObjectList.Clone: TDicomObjectList;
Begin
  result := TDicomObjectList(inherited Clone);
End;

Function TDicomObjectList.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomObjectList.GetChildren(iIndex: integer): TDicomObject;
Begin
  result := TDicomObject(ObjectByIndex[iIndex]);
End;

Function TDicomObjectList.IndexOf(value: TDicomObject): Integer;
Begin
  Result := IndexByReference(value);
End;

Function TDicomObjectList.Insert(iIndex: Integer): TDicomObject;
Begin
  Result := TDicomObject.Create(FDictionary.Link);
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TDicomObjectList.InsertItem(iIndex: Integer; value: TDicomObject);
Begin
  Inherited Insert(iIndex, value);
End;

Function TDicomObjectList.Item(iIndex: Integer): TDicomObject;
Begin
  Result := Children[iIndex]; 
End;

Function TDicomObjectList.ItemClass: TFslObjectClass;
Begin
  result := TDicomObject;
End;

Function TDicomObjectList.Link: TDicomObjectList;
Begin
  result := TDicomObjectList(inherited Link);
End;

Procedure TDicomObjectList.Remove(iIndex: Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TDicomObjectList.SetItemByIndex(iIndex: Integer; value: TDicomObject);
Begin
  ObjectByIndex[iIndex] := value;
End;

function TDicomObjectList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDictionary.sizeInBytes);
end;

{ TDicomInstance }

Procedure TDicomInstance.Assign(oSource: TFslObject);
Begin
  inherited;
  SimpleObject := TDicomInstance(oSource).FSimpleObject.Clone;
  FileObject := TDicomInstance(oSource).FFileObject.Clone;
  AssociateRequest := TDicomInstance(oSource).FAssociateRequest.Clone;
  AssociateAccept := TDicomInstance(oSource).FAssociateAccept.Clone;
  AssociateReject := TDicomInstance(oSource).FAssociateReject.Clone;
  Data := TDicomInstance(oSource).FData.Clone;
  Abort := TDicomInstance(oSource).FAbort.Clone;
  ReleaseRequest := TDicomInstance(oSource).FReleaseRequest.Clone;
  ReleaseResponse := TDicomInstance(oSource).FReleaseResponse.Clone;
  Message := TDicomInstance(oSource).FMessage.Clone;
End;

Procedure TDicomInstance.Clear;
Begin
  FMessage.Free;
  FMessage := nil;
  FSimpleObject.Free;
  FSimpleObject := nil;
  FFileObject.Free;
  FFileObject := nil;
  FAssociateRequest.Free;
  FAssociateRequest := nil;
  FAssociateAccept.Free;
  FAssociateAccept := nil;
  FAssociateReject.Free;
  FAssociateReject := nil;
  FData.Free;
  FData := nil;
  FAbort.Free;
  FAbort := nil;
  FReleaseRequest.Free;
  FReleaseRequest := nil;
  FReleaseResponse.Free;
  FReleaseResponse := nil;
End;

Function TDicomInstance.Clone: TDicomInstance;
Begin
  result := TDicomInstance(Inherited Clone);
End;

destructor TDicomInstance.Destroy;
Begin
  Clear;
  inherited;
End;


Function TDicomInstance.GetInstanceType: TDicomInstanceType;
Begin
  if FSimpleObject <> nil Then
    result := ditSimpleObject
  else if FMessage <> nil Then
    result := ditMessage
  else if FFileObject <> nil Then
    result := ditFileObject
  else if FAssociateRequest <> nil Then
    result := ditAssociateRequestPDU
  else if FAssociateAccept <> nil Then
    result := ditAssociateAcceptPDU
  else if FAssociateReject <> nil Then
    result := ditAssociateRejectPDU
  else if FData <> nil Then
    result := ditDataPDU
  else if FAbort <> nil Then
    result := ditAbortPDU
  else if FReleaseRequest <> nil Then
    result := ditReleaseRequestPDU
  else if FReleaseResponse <> nil Then
    result := ditReleaseResponsePDU
  else
    result := ditNull;
End;

Function TDicomInstance.Link: TDicomInstance;
Begin
  result := TDicomInstance(Inherited Link);
End;

Procedure TDicomInstance.SetAbort(const Value: TDicomAbortPDU);
Begin
  Clear;
  FAbort := Value;
End;

Procedure TDicomInstance.SetAssociateAccept(const Value: TDicomAssociateAcceptPDU);
Begin
  Clear;
  FAssociateAccept := Value;
End;

Procedure TDicomInstance.SetAssociateReject(const Value: TDicomAssociateRejectPDU);
Begin
  Clear;
  FAssociateReject := Value;
End;

Procedure TDicomInstance.SetAssociateRequest(const Value: TDicomAssociateRequestPDU);
Begin
  Clear;
  FAssociateRequest := Value;
End;

Procedure TDicomInstance.SetData(const Value: TDicomDataPDU);
Begin
  Clear;
  FData := Value;
End;

Procedure TDicomInstance.SetFileObject(const Value: TDicomFile);
Begin
  Clear;
  FFileObject := Value;
End;

procedure TDicomInstance.SetMessage(const Value: TDicomMessage);
begin
  FMessage.Free;
  FMessage := Value;
end;

Procedure TDicomInstance.SetReleaseRequest(const Value: TDicomReleaseRequestPDU);
Begin
  Clear;
  FReleaseRequest := Value;
End;

Procedure TDicomInstance.SetReleaseResponse(const Value: TDicomReleaseResponsePDU);
Begin
  Clear;
  FReleaseResponse := Value;
End;

Procedure TDicomInstance.SetSimpleObject(const Value: TDicomObject);
Begin
  Clear;
  FSimpleObject := Value;
End;

function TDicomInstance.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSimpleObject.sizeInBytes);
  inc(result, FFileObject.sizeInBytes);
  inc(result, FAssociateRequest.sizeInBytes);
  inc(result, FAssociateAccept.sizeInBytes);
  inc(result, FAssociateReject.sizeInBytes);
  inc(result, FData.sizeInBytes);
  inc(result, FAbort.sizeInBytes);
  inc(result, FReleaseRequest.sizeInBytes);
  inc(result, FReleaseResponse.sizeInBytes);
  inc(result, FMessage.sizeInBytes);
end;

{ TDicomPDU }

Procedure TDicomPDU.Assign(oSource: TFslObject);
Begin
  inherited;
  Dictionary := TDicomPDU(oSource).FDictionary.Link;
End;

Function TDicomPDU.Clone: TDicomPDU;
Begin
  result := TDicomPDU(inherited Clone);
End;

Constructor TDicomPDU.Create;
Begin
  inherited;
  FVersion := 1;
End;


destructor TDicomPDU.Destroy;
Begin
  FDictionary.Free;
  inherited;
End;

Function TDicomPDU.Link: TDicomPDU;
Begin
  result := TDicomPDU(inherited Link);
End;

Procedure TDicomPDU.SetDictionary(const Value: TDicomDictionary);
Begin
  FDictionary.Free;
  FDictionary := Value;
End;

function TDicomPDU.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDictionary.sizeInBytes);
end;

{ TDicomPresentationContextInfo }

Procedure TDicomPresentationContextInfo.Assign(oSource: TFslObject);
Begin
  inherited;
  FId := TDicomPresentationContextInfo(oSource).FId;
  FAbstractSyntax := TDicomPresentationContextInfo(oSource).FAbstractSyntax.Link;
  FTransferSyntaxes.Assign(TDicomPresentationContextInfo(oSource).FTransferSyntaxes);
End;

Function TDicomPresentationContextInfo.Clone: TDicomPresentationContextInfo;
Begin
  result := TDicomPresentationContextInfo(Inherited Clone);
End;

Constructor TDicomPresentationContextInfo.Create;
Begin
  inherited;
  FTransferSyntaxes := TDicomStringList.Create;
End;

destructor TDicomPresentationContextInfo.Destroy;
Begin
  FAbstractSyntax.Free;
  FTransferSyntaxes.Free;
  inherited;
End;

Function TDicomPresentationContextInfo.Link: TDicomPresentationContextInfo;
Begin
  result := TDicomPresentationContextInfo(Inherited Link);
End;


Procedure TDicomPresentationContextInfo.SetAbstractSyntax(const Value: TDicomString);
Begin
  FAbstractSyntax.Free;
  FAbstractSyntax := Value;
End;

function TDicomPresentationContextInfo.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FAbstractSyntax.sizeInBytes);
  inc(result, FTransferSyntaxes.sizeInBytes);
end;

{ TDicomPresentationAcceptContextInfo }

Procedure TDicomPresentationAcceptContextInfo.Assign(oSource: TFslObject);
Begin
  inherited;
  FResult := TDicomPresentationAcceptContextInfo(oSource).FResult;
  FId := TDicomPresentationAcceptContextInfo(oSource).FId;
  FTransferSyntax := TDicomPresentationAcceptContextInfo(oSource).FTransferSyntax.Link;
End;

Function TDicomPresentationAcceptContextInfo.Clone: TDicomPresentationAcceptContextInfo;
Begin
  result := TDicomPresentationAcceptContextInfo(Inherited Clone);
End;

destructor TDicomPresentationAcceptContextInfo.Destroy;
Begin
  FTransferSyntax.Free;
  inherited;
End;

Function TDicomPresentationAcceptContextInfo.Link: TDicomPresentationAcceptContextInfo;
Begin
  result := TDicomPresentationAcceptContextInfo(Inherited Link);
End;

Procedure TDicomPresentationAcceptContextInfo.SetTransferSyntax(const Value: TDicomString);
Begin
  FTransferSyntax.Free;
  FTransferSyntax := Value;
End;

function TDicomPresentationAcceptContextInfo.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FTransferSyntax.sizeInBytes);
end;

{ TDicomAssociateRequestPDU }

Procedure TDicomAssociateRequestPDU.Assign(oSource: TFslObject);
Begin
  inherited;
  FCalledEntity := TDicomAssociateRequestPDU(oSource).FCalledEntity.Link;
  FCallingEntity := TDicomAssociateRequestPDU(oSource).FCallingEntity.Link;
  FApplicationContext := TDicomAssociateRequestPDU(oSource).FApplicationContext.Link;
  FMaxLength := TDicomAssociateRequestPDU(oSource).FMaxLength;
  FPresentationContexts.Assign(TDicomAssociateRequestPDU(oSource).FPresentationContexts);
  FUserData.Assign(TDicomAssociateRequestPDU(oSource).FUserData);
End;

Function TDicomAssociateRequestPDU.Clone: TDicomAssociateRequestPDU;
Begin
  result := TDicomAssociateRequestPDU(Inherited Clone);
End;

Constructor TDicomAssociateRequestPDU.Create;
Begin
  inherited;
  FPacketType := pduAssociateRequest;
  FPresentationContexts := TDicomPresentationContextInfoList.Create;
  FUserData := TDicomUserDataList.Create;
End;

destructor TDicomAssociateRequestPDU.Destroy;
Begin
  FPresentationContexts.Free;
  FUserData.Free;
  FCalledEntity.Free;
  FCallingEntity.Free;
  FApplicationContext.Free;
  inherited;
End;

Function TDicomAssociateRequestPDU.Link: TDicomAssociateRequestPDU;
Begin
  result := TDicomAssociateRequestPDU(Inherited Link);
End;

Procedure TDicomAssociateRequestPDU.SetApplicationContext(const Value: TDicomString);
Begin
  FApplicationContext.Free;
  FApplicationContext := Value;
End;

Procedure TDicomAssociateRequestPDU.SetCalledEntity(const Value: TDicomString);
Begin
  FCalledEntity.Free;
  FCalledEntity := Value;
End;

Procedure TDicomAssociateRequestPDU.SetCallingEntity(const Value: TDicomString);
Begin
  FCallingEntity.Free;
  FCallingEntity := Value;
End;

function TDicomAssociateRequestPDU.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCalledEntity.sizeInBytes);
  inc(result, FCallingEntity.sizeInBytes);
  inc(result, FApplicationContext.sizeInBytes);
  inc(result, FPresentationContexts.sizeInBytes);
  inc(result, FUserData.sizeInBytes);
end;

{ TDicomPresentationContextInfoList }

Procedure TDicomPresentationContextInfoList.AddItem(value: TDicomPresentationContextInfo);
Begin
  Add(value);
End;

Function TDicomPresentationContextInfoList.Append: TDicomPresentationContextInfo;
Begin
  Result := TDicomPresentationContextInfo.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TDicomPresentationContextInfoList.ClearItems;
Begin
  Inherited InternalClear;
End;

Function TDicomPresentationContextInfoList.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomPresentationContextInfoList.IndexOf(value: TDicomPresentationContextInfo): Integer;
Begin
  Result := IndexByReference(value);
End;

Function TDicomPresentationContextInfoList.Insert(iIndex: Integer): TDicomPresentationContextInfo;
Begin
  Result := TDicomPresentationContextInfo.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TDicomPresentationContextInfoList.InsertItem(iIndex: Integer; value: TDicomPresentationContextInfo);
Begin
   Inherited Insert(iIndex, value);
End;

Function TDicomPresentationContextInfoList.Item(iIndex: Integer): TDicomPresentationContextInfo;
Begin
  Result := Elements[iIndex];
End;

Procedure TDicomPresentationContextInfoList.Remove(iIndex: Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TDicomPresentationContextInfoList.SetItemByIndex(iIndex: Integer; value: TDicomPresentationContextInfo);
Begin
  ObjectByIndex[iIndex] := value;
End;

Function TDicomPresentationContextInfoList.GetElements(iIndex: Integer): TDicomPresentationContextInfo;
Begin
  result := TDicomPresentationContextInfo(ObjectByIndex[iIndex]);
End;

Function TDicomPresentationContextInfoList.ItemClass: TFslObjectClass;
Begin
  result := TDicomPresentationContextInfo;
End;

{ TDicomPresentationAcceptContextInfoList }

Procedure TDicomPresentationAcceptContextInfoList.AddItem(value: TDicomPresentationAcceptContextInfo);
Begin
  Add(value);
End;

Function TDicomPresentationAcceptContextInfoList.Append: TDicomPresentationAcceptContextInfo;
Begin
  Result := TDicomPresentationAcceptContextInfo.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TDicomPresentationAcceptContextInfoList.ClearItems;
Begin
  Inherited InternalClear;
End;

Function TDicomPresentationAcceptContextInfoList.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomPresentationAcceptContextInfoList.GetElements(iIndex: Integer): TDicomPresentationAcceptContextInfo;
Begin
  Result := TDicomPresentationAcceptContextInfo(ObjectByIndex[iIndex]);
End;

Function TDicomPresentationAcceptContextInfoList.IndexOf(value: TDicomPresentationAcceptContextInfo): Integer;
Begin
  Result := IndexByReference(value);
End;

Function TDicomPresentationAcceptContextInfoList.Insert(iIndex: Integer): TDicomPresentationAcceptContextInfo;
Begin
  Result := TDicomPresentationAcceptContextInfo.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TDicomPresentationAcceptContextInfoList.InsertItem(iIndex: Integer; value: TDicomPresentationAcceptContextInfo);
Begin
  Inherited Insert(iIndex, value);
End;

Function TDicomPresentationAcceptContextInfoList.Item(iIndex: Integer): TDicomPresentationAcceptContextInfo;
Begin
  Result := Elements[iIndex];
End;

Function TDicomPresentationAcceptContextInfoList.ItemClass: TFslObjectClass;
Begin
  Result := TDicomPresentationAcceptContextInfo;
End;

Procedure TDicomPresentationAcceptContextInfoList.Remove(iIndex: Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TDicomPresentationAcceptContextInfoList.SetItemByIndex(iIndex: Integer; value: TDicomPresentationAcceptContextInfo);
Begin
  ObjectByIndex[iIndex] := value;
End;

{ TDicomAssociateAcceptPDU }

Procedure TDicomAssociateAcceptPDU.Assign(oSource: TFslObject);
Begin
  inherited;
  FCalledEntity := TDicomAssociateAcceptPDU(oSource).FCalledEntity.Link;
  FCallingEntity := TDicomAssociateAcceptPDU(oSource).FCallingEntity.Link;
  FApplicationContext := TDicomAssociateAcceptPDU(oSource).FApplicationContext.Link;
  FMaxLength := TDicomAssociateAcceptPDU(oSource).FMaxLength;
  FPresentationContexts.Assign(TDicomAssociateAcceptPDU(oSource).FPresentationContexts);
  FUserData.Assign(TDicomAssociateAcceptPDU(oSource).FUserData);
End;

Function TDicomAssociateAcceptPDU.Clone: TDicomAssociateAcceptPDU;
Begin
  result := TDicomAssociateAcceptPDU(Inherited Clone);
End;

Constructor TDicomAssociateAcceptPDU.Create;
Begin
  inherited;
  FPacketType := pduAssociateAccept;
  FPresentationContexts := TDicomPresentationAcceptContextInfoList.Create;
  FUserData := TDicomUserDataList.Create;
End;

destructor TDicomAssociateAcceptPDU.Destroy;
Begin
  FPresentationContexts.Free;
  FUserData.Free;
  FCalledEntity.Free;
  FCallingEntity.Free;
  FApplicationContext.Free;
  inherited;
End;

Function TDicomAssociateAcceptPDU.Link: TDicomAssociateAcceptPDU;
Begin
  result := TDicomAssociateAcceptPDU(Inherited Link);
End;

Procedure TDicomAssociateAcceptPDU.SetApplicationContext(const Value: TDicomString);
Begin
  FApplicationContext.Free;
  FApplicationContext := Value;
End;

Procedure TDicomAssociateAcceptPDU.SetCalledEntity(const Value: TDicomString);
Begin
  FCalledEntity.Free;
  FCalledEntity := Value;
End;

Procedure TDicomAssociateAcceptPDU.SetCallingEntity(const Value: TDicomString);
Begin
  FCallingEntity.Free;
  FCallingEntity := Value;
End;

function TDicomAssociateAcceptPDU.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCalledEntity.sizeInBytes);
  inc(result, FCallingEntity.sizeInBytes);
  inc(result, FApplicationContext.sizeInBytes);
  inc(result, FPresentationContexts.sizeInBytes);
  inc(result, FUserData.sizeInBytes);
end;

{ TDicomAssociateRejectPDU }

Procedure TDicomAssociateRejectPDU.Assign(oSource: TFslObject);
Begin
  inherited;
  FReason := TDicomAssociateRejectPDU(oSource).FReason;
  FResult := TDicomAssociateRejectPDU(oSource).FResult;
  FSource := TDicomAssociateRejectPDU(oSource).FSource;
End;

Function TDicomAssociateRejectPDU.Clone: TDicomAssociateRejectPDU;
Begin
  result := TDicomAssociateRejectPDU(inherited Clone);
End;

Constructor TDicomAssociateRejectPDU.Create;
Begin
  inherited;
  FPacketType := pduAssociateReject;
End;

Function TDicomAssociateRejectPDU.Link: TDicomAssociateRejectPDU;
Begin
  result := TDicomAssociateRejectPDU(inherited Link);
End;

function TDicomAssociateRejectPDU.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TDicomDataPDU }

Procedure TDicomDataPDU.Assign(oSource: TFslObject);
Begin
  inherited;
  FContextId := TDicomDataPDU(oSource).FContextId;
End;

Function TDicomDataPDU.Clone: TDicomDataPDU;
Begin
  result := TDicomDataPDU(Inherited Clone);
End;

Constructor TDicomDataPDU.Create;
Begin
  inherited;
  FPacketType := pduData;
  FDataValues := TDicomPresentationDataValueList.Create;
End;

destructor TDicomDataPDU.Destroy;
Begin
  FDataValues.Free;
  inherited;
End;

Function TDicomDataPDU.Link: TDicomDataPDU;
Begin
  result := TDicomDataPDU(Inherited Link);
End;


function TDicomDataPDU.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDataValues.sizeInBytes);
end;

{ TDicomAbortPDU }

Procedure TDicomAbortPDU.Assign(oSource: TFslObject);
Begin
  inherited;
  FReason := TDicomAbortPDU(oSource).FReason;
  FSource := TDicomAbortPDU(oSource).FSource;
End;

Function TDicomAbortPDU.Clone: TDicomAbortPDU;
Begin
  result := TDicomAbortPDU(inherited Clone);
End;

Constructor TDicomAbortPDU.Create;
Begin
  inherited;
  FPacketType := pduAbort;
End;

Function TDicomAbortPDU.Link: TDicomAbortPDU;
Begin
  result := TDicomAbortPDU(inherited Link);
End;

{ TDicomReleaseRequestPDU }

Function TDicomReleaseRequestPDU.Clone: TDicomReleaseRequestPDU;
Begin
  result := TDicomReleaseRequestPDU(inherited Clone);
End;

Constructor TDicomReleaseRequestPDU.Create;
Begin
  inherited;
  FPacketType := pduReleaseRequest;
End;

Function TDicomReleaseRequestPDU.Link: TDicomReleaseRequestPDU;
Begin
  result := TDicomReleaseRequestPDU(inherited Link);
End;

{ TDicomReleaseResponsePDU }

Function TDicomReleaseResponsePDU.Clone: TDicomReleaseResponsePDU;
Begin
  result := TDicomReleaseResponsePDU(inherited Clone);
End;

Constructor TDicomReleaseResponsePDU.Create;
Begin
  inherited;
  FPacketType := pduReleaseResponse;
End;

Function TDicomReleaseResponsePDU.Link: TDicomReleaseResponsePDU;
Begin
  result := TDicomReleaseResponsePDU(inherited Link);
End;

{ TDicomUserData }

Procedure TDicomUserData.Assign(oSource: TFslObject);
Begin
  inherited;
  FId := TDicomUserData(oSource).FId;
End;

Function TDicomUserData.Clone: TDicomUserData;
Begin
  result := TDicomUserData(Inherited Clone);
End;

Function TDicomUserData.Link: TDicomUserData;
Begin
  result := TDicomUserData(Inherited Link);
End;

{ TDicomUserDataList }

Procedure TDicomUserDataList.Add(iId: integer; const oValue: TDicomString; iOffsetStart, iOffsetEnd : Cardinal);
var
  oData : TDicomUserData;
Begin
  oData := TDicomUserData.Create;
  try
    oData.Id := iId;
    oData.FValue := oValue.Value;
    oValue.Free;
    oData.OffsetStart := iOffsetStart;
    oData.OffsetEnd := iOffsetEnd;
    add(oData.Link);
  Finally
    oData.Free;
  End;
End;

Function TDicomUserDataList.GetElements(iIndex: Integer): TDicomUserData;
Begin
  result := TDicomUserData(ObjectByIndex[iIndex]);
End;

Function TDicomUserDataList.ItemClass: TFslObjectClass;
Begin
  result := TDicomUserData;
End;

Procedure TDicomUserDataList.AddItem(value: TDicomUserData);
Begin
  Add(value);
End;

Function TDicomUserDataList.Append: TDicomUserData;
Begin
  Result := TDicomUserData.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TDicomUserDataList.ClearItems;
Begin
  Inherited InternalClear;
End;

Function TDicomUserDataList.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomUserDataList.IndexOf(value: TDicomUserData): Integer;
Begin
  Result := IndexByReference(value);
End;

Function TDicomUserDataList.Insert(iIndex: Integer): TDicomUserData;
Begin
  Result := TDicomUserData.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TDicomUserDataList.InsertItem(iIndex: Integer; value: TDicomUserData);
Begin
   Inherited Insert(iIndex, value);
End;

Function TDicomUserDataList.Item(iIndex: Integer): TDicomUserData;
Begin
  Result := Elements[iIndex];
End;

Procedure TDicomUserDataList.Remove(iIndex: Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TDicomUserDataList.SetItemByIndex(iIndex: Integer; value: TDicomUserData);
Begin
  ObjectByIndex[iIndex] := value;
End;

{ TDicomPresentationDataValue }

Procedure TDicomPresentationDataValue.Assign(oSource: TFslObject);
Begin
  inherited;
  FCommand := TDicomPresentationDataValue(oSource).FCommand;
  FLast := TDicomPresentationDataValue(oSource).FLast;
  FBytes := TDicomPresentationDataValue(oSource).FBytes;
End;

Function TDicomPresentationDataValue.Clone: TDicomPresentationDataValue;
Begin
  result := TDicomPresentationDataValue(Inherited Clone);
End;

Function TDicomPresentationDataValue.Link: TDicomPresentationDataValue;
Begin
  result := TDicomPresentationDataValue(Inherited Link);
End;

{ TDicomPresentationDataValueList }

Procedure TDicomPresentationDataValueList.AddItem(value: TDicomPresentationDataValue);
Begin
  Add(value);
End;

Function TDicomPresentationDataValueList.Append: TDicomPresentationDataValue;
Begin
  Result := TDicomPresentationDataValue.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TDicomPresentationDataValueList.ClearItems;
Begin
  Inherited InternalClear;
End;

Function TDicomPresentationDataValueList.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomPresentationDataValueList.IndexOf(value: TDicomPresentationDataValue): Integer;
Begin
  Result := IndexByReference(value);
End;

Function TDicomPresentationDataValueList.Insert(iIndex: Integer): TDicomPresentationDataValue;
Begin
  Result := TDicomPresentationDataValue.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TDicomPresentationDataValueList.InsertItem(iIndex: Integer; value: TDicomPresentationDataValue);
Begin
   Inherited Insert(iIndex, value);
End;

Function TDicomPresentationDataValueList.Item(iIndex: Integer): TDicomPresentationDataValue;
Begin
  Result := Elements[iIndex];
End;

Procedure TDicomPresentationDataValueList.Remove(iIndex: Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TDicomPresentationDataValueList.SetItemByIndex(iIndex: Integer; value: TDicomPresentationDataValue);
Begin
  ObjectByIndex[iIndex] := value;
End;

Procedure TDicomPresentationDataValueList.AddFromBuffer(bCommand: Boolean; oBuffer: TFslBuffer);
var
  oPV : TDicomPresentationDataValue;
Begin
  oPV := TDicomPresentationDataValue.Create;
  Try
    oPV.FCommand := bCommand;
    oPV.FLast := True;
    oPV.FBytes := oBuffer.AsBytes;
    Add(oPV.Link);
  Finally
    oPV.Free;
  End;
End;

Function TDicomPresentationDataValueList.GetElements(iIndex: Integer): TDicomPresentationDataValue;
Begin
  result := TDicomPresentationDataValue(ObjectByIndex[iIndex]);
End;

Function TDicomPresentationDataValueList.ItemClass: TFslObjectClass;
Begin
  result := TDicomPresentationDataValue;
End;

{ TDicomFile }

Procedure TDicomFile.Assign(oSource: TFslObject);
Begin
  inherited;
  FName := TDicomFile(oSource).FName;
  FContent := TDicomFile(oSource).FContent.Clone;
  FHeader := TDicomFile(oSource).FHeader.Clone;
End;

Function TDicomFile.Clone: TDicomFile;
Begin
  result := TDicomFile(Inherited Clone);
End;

destructor TDicomFile.Destroy;
Begin
  FContent.Free;
  FHeader.Free;
  inherited;
End;

Function TDicomFile.Link: TDicomFile;
Begin
  result := TDicomFile(Inherited Link);
End;

Procedure TDicomFile.SetContent(const Value: TDicomObject);
Begin
  FContent.Free;
  FContent := Value;
End;

Procedure TDicomFile.SetHeader(const Value: TDicomObject);
Begin
  FHeader.Free;
  FHeader := Value;
End;


Function TDicomFile.FileMetaInformation : Word;
Begin
  result := GetHeaderValue($0002,$0001).AsUS;
End;

Function TDicomFile.SOPClassUID : String;
Begin
  result := GetHeaderValue($0002,$0002).AsUI;
End;

Function TDicomFile.MediaStorageSOPInstanceUID : String;
Begin
  result := GetHeaderValue($0002,$0003).AsUI;
End;

Function TDicomFile.TransferSyntaxUID : String;
Begin
  result := GetHeaderValue($0002,$0010).AsUI;
End;

Function TDicomFile.ImplementationClassUID : String;
Begin
  result := GetHeaderValue($0002,$0012).AsUI;
End;

Function TDicomFile.ImplementationVersionName : String;
Begin
  result := GetHeaderValue($0002,$0013).AsSH;
End;

Function TDicomFile.SourceAET : String;
Begin
  result := GetHeaderValue($0002,$0016).AsAE;
End;



Function TDicomFile.GetHeaderValue(group, element: Word): TDicomValue;
Begin
  result := FHeader.FElements.GetByTags(group, element).SingleValue;
End;

function TDicomFile.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FContent.sizeInBytes);
  inc(result, FHeader.sizeInBytes);
end;

{ TDicomString }

Function TDicomString.AsCardinal: Cardinal;
Begin
  if Length(FValue) = 4 then
    move(FValue[1], result, 4)
  else
    result := 0;
End;

Procedure TDicomString.Assign(oSource: TFslObject);
Begin
  inherited;
  FValue := TDicomString(oSource).FValue;
End;

Function TDicomString.Clone: TDicomString;
Begin
  result := TDicomString(inherited Clone);
End;

Constructor TDicomString.Create(sValue: String);
Begin
  Create;
  FValue := sValue;
End;

Constructor TDicomString.Create(iValue: Cardinal);
var
  s : ShortString;
Begin
  Create;
  s := '    ';
  Move(iValue, s[1], 4);
  FValue := String(s);
End;

Function TDicomString.GetValue: String;
Begin
  if self = nil then
    result := ''
  Else
    Result := FValue;
End;

function TDicomString.GetValueA: AnsiString;
begin
  result := AnsiString(Value);
end;

Function TDicomString.Link: TDicomString;
Begin
  result := TDicomString(inherited Link);
End;

procedure TDicomString.SetValueA(const Value: AnsiString);
begin
  FValue := String(value);
end;

{ TDicomStringList }

Function TDicomStringList.Clone: TDicomStringList;
Begin
  result := TDicomStringList(Inherited Clone);
End;

Function TDicomStringList.GetChildren(iIndex: integer): TDicomString;
Begin
  result := TDicomString(ObjectByIndex[iIndex]);
End;

Function TDicomStringList.ItemClass: TFslObjectClass;
Begin
  result := TDicomString;
End;

Function TDicomStringList.Link: TDicomStringList;
Begin
  result := TDicomStringList(Inherited Link);
End;

Procedure TDicomStringList.AddItem(value: TDicomString);
Begin
  Add(value);
End;

Function TDicomStringList.Append: TDicomString;
Begin
  Result := TDicomString.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TDicomStringList.ClearItems;
Begin
  Inherited InternalClear;
End;

Function TDicomStringList.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomStringList.IndexOf(value: TDicomString): Integer;
Begin
  Result := IndexByReference(value);
End;

Function TDicomStringList.Insert(iIndex: Integer): TDicomString;
Begin
  Result := TDicomString.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TDicomStringList.InsertItem(iIndex: Integer; value: TDicomString);
Begin
   Inherited Insert(iIndex, value);
End;

Function TDicomStringList.Item(iIndex: Integer): TDicomString;
Begin
  Result := Children[iIndex];
End;

Procedure TDicomStringList.Remove(iIndex: Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TDicomStringList.SetItemByIndex(iIndex: Integer; value: TDicomString);
Begin
  ObjectByIndex[iIndex] := value;
End;

{ TDicomMessage }

destructor TDicomMessage.Destroy;
Begin
  FData.Free;
  FCommand.Free;
  inherited;
End;

Procedure TDicomMessage.Assign(oSource: TFslObject);
Begin
  inherited;
  FAbstractSyntax := TDicomMessage(oSource).FAbstractSyntax;
  FTransferSyntax := TDicomMessage(oSource).FTransferSyntax;
  Data := TDicomMessage(oSource).Data.Clone;
  Command := TDicomMessage(oSource).Command.Clone;
End;

Procedure TDicomMessage.Clear;
Begin
  FAbstractSyntax := '';
  FTransferSyntax := '';
  Data := nil;
  Command := nil;
End;

Function TDicomMessage.Clone: TDicomMessage;
Begin
  result := TDicomMessage(Inherited Clone);
End;

Function TDicomMessage.Link: TDicomMessage;
Begin
  result := TDicomMessage(Inherited Link);
End;

Procedure TDicomMessage.SetCommand(const Value: TDicomObject);
Begin
  FCommand.Free;
  FCommand := Value;
End;

Procedure TDicomMessage.SetData(const Value: TDicomObject);
Begin
  FData.Free;
  FData := Value;
End;

function TDicomMessage.ReadMessageId: String;
begin
  result := FCommand.Elements.GetByTag('0000,0110').SingleValue.AsString;
end;

function TDicomMessage.ReadPatientId: String;
begin
  result := FData.Elements.GetByTag('0010,0020').SingleValue.AsString;
end;

function TDicomMessage.ReadCommandId: Integer;
begin
  result := FCommand.Elements.GetByTag('0000,0100').SingleValue.AsUS;
end;

function TDicomMessage.Size: Cardinal;
begin
  result := 0;
  if FCommand <> nil then
    inc(result, FCommand.Size);
  if FData <> nil then
    inc(result, FData.Size);
end;

function TDicomMessage.ReadSOPClass: String;
begin
  result := FCommand.Elements.GetByTag('0000,0002').SingleValue.AsString;
end;

function TDicomMessage.ReadSOPInstance: String;
begin
  result := FCommand.Elements.GetByTag('0000,1000').SingleValue.AsString;
end;

function TDicomMessage.ReadStatus: Integer;
begin
  result := FCommand.Elements.GetByTag('0000,0900').SingleValue.AsUS;
end;

function TDicomMessage.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FData.sizeInBytes);
  inc(result, FCommand.sizeInBytes);
  inc(result, (FAbstractSyntax.length * sizeof(char)) + 12);
  inc(result, (FTransferSyntax.length * sizeof(char)) + 12);
end;

{ TDicomMessageList }

Procedure TDicomMessageList.AddItem(value: TDicomMessage);
Begin
  Add(value);
End;

Function TDicomMessageList.Append: TDicomMessage;
Begin
  Result := TDicomMessage.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TDicomMessageList.ClearItems;
Begin
  Inherited InternalClear;
End;

Function TDicomMessageList.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomMessageList.IndexOf(value: TDicomMessage): Integer;
Begin
  Result := IndexByReference(value);
End;

Function TDicomMessageList.Insert(iIndex: Integer): TDicomMessage;
Begin
  Result := TDicomMessage.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TDicomMessageList.InsertItem(iIndex: Integer; value: TDicomMessage);
Begin
   Inherited Insert(iIndex, value);
End;

Function TDicomMessageList.Item(iIndex: Integer): TDicomMessage;
Begin
  Result := Elements[iIndex];
End;

Procedure TDicomMessageList.Remove(iIndex: Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TDicomMessageList.SetItemByIndex(iIndex: Integer; value: TDicomMessage);
Begin
  ObjectByIndex[iIndex] := value;
End;

Function TDicomMessageList.GetElements(iIndex: Integer): TDicomMessage;
Begin
  result := TDicomMessage(ObjectByIndex[iIndex]);
End;

Function TDicomMessageList.ItemClass: TFslObjectClass;
Begin
  result := TDicomMessage;
End;


Constructor EDicomAbort.Create(aSource : TDicomAbortSource; aReason : TDicomAbortReason);
Begin
  Inherited Create('Abort: '+NAMES_DicomAbortSource[aSource]+' / '+NAMES_DicomAbortReason[aReason]);
  FSource := aSource;
  FReason := aReason;
End;

function TDicomMessageList.Link: TDicomMessageList;
begin
  result := TDicomMessageList(Inherited Link);
end;


{ TDicomValueList }

Procedure TDicomValueList.Assign(oSource: TFslObject);
Begin
  inherited;
  FKnownType := TDicomValueList(oSource).FKnownType;
  FPossibleTypes := TDicomValueList(oSource).FPossibleTypes;
  FDictionary := TDicomValueList(oSource).FDictionary.Link;
End;

Function TDicomValueList.Clone: TDicomValueList;
Begin
  result := TDicomValueList(inherited Clone);
End;

constructor TDicomValueList.Create(oDictionary : TDicomDictionary; aKnownType : TDicomVRType; aPossibleTypes : TDicomVRTypes);
Begin
  Inherited Create;
  FKnownType := aKnownType;
  FPossibleTypes := aPossibleTypes;
  FDictionary := oDictionary;
End;

Function TDicomValueList.GetValues(iIndex: integer): TDicomValue;
Begin
  result := TDicomValue(ObjectByIndex[iIndex]);
End;

Function TDicomValueList.ItemClass: TFslObjectClass;
Begin
  result := TDicomValue;
End;

Function TDicomValueList.Link: TDicomValueList;
Begin
  result := TDicomValueList(inherited Link);
End;


Procedure TDicomValueList.SetDictionary(const Value: TDicomDictionary);
Begin
  FDictionary.Free;
  FDictionary := Value;
End;


Function TDicomValueList.Add(aBytes: TBytes) : TDicomValue;
Begin
  result := TDicomValue.Create(FDictionary.Link, FKnownType, FPossibleTypes, aBytes);
  Add(result);
End;


destructor TDicomValueList.Destroy;
Begin
  FDictionary.Free;
  inherited;
End;

Function TDicomValueList.GetPossibleTypes: TFslStringList;
Var
  oTypes: TFslStringList;
  aLoop : TDicomVRType;
Begin
  oTypes := TFslStringList.Create;
  Try
    For aLoop := Low(TDicomVRType) To High(TDicomVRType) Do
    Begin
      If aLoop In PossibleTypes then
        oTypes.Add(DICOM_VR_TYPE_NAMES_S[aLoop]);
    End;

    Result := oTypes.Link;
  Finally
    oTypes.Free;
  End;
End;

Procedure TDicomValueList.AddItem(value: TDicomValue);
Begin
  Add(value.Link);
End;

Function TDicomValueList.Append: TDicomValue;
Begin
  Result := TDicomValue.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TDicomValueList.ClearItems;
Begin
  Inherited InternalClear;
End;

Function TDicomValueList.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomValueList.IndexOf(value: TDicomValue): Integer;
Begin
  Result := IndexByReference(value);
End;

Function TDicomValueList.Insert(iIndex: Integer): TDicomValue;
Begin
  Result := TDicomValue.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TDicomValueList.InsertItem(iIndex: Integer; value: TDicomValue);
Begin
  Inherited Insert(iIndex, value);
End;

Function TDicomValueList.Item(iIndex: Integer): TDicomValue;
Begin
  Result := Values[iIndex];
End;

Procedure TDicomValueList.Remove(iIndex: Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TDicomValueList.SetItemByIndex(iIndex: Integer; value: TDicomValue);
Begin
  ObjectByIndex[iIndex] := value;
End;

function TDicomValueList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDictionary.sizeInBytes);
end;

{ TDicomValue }

Constructor TDicomValue.Create(oDictionary : TDicomDictionary; aKnownType : TDicomVRType; aPossibleTypes : TDicomVRTypes; aBytes : TBytes);
Begin
  Create;
  FDictionary := oDictionary;
  FKnownType := aKnownType;
  FPossibleTypes := aPossibleTypes;
  FBytes := aBytes;
End;

destructor TDicomValue.Destroy;
Begin
  FDictionary.Free;
  inherited;
End;

Function TDicomValue.Clone: TDicomValue;
Begin
  result := TDicomValue(inherited Clone);
End;

Function TDicomValue.Link: TDicomValue;
Begin
  result := TDicomValue(Inherited Link);
End;

Procedure TDicomValue.Assign(oSource: TFslObject);
Begin
  inherited;
  FBytes := TDicomValue(oSource).FBytes;
  FKnownType := TDicomValue(oSource).FKnownType;
  FPossibleTypes := TDicomValue(oSource).FPossibleTypes;
  FDictionary := TDicomValue(oSource).FDictionary.Link;
End;

Procedure TDicomValue.SetDictionary(const Value: TDicomDictionary);
Begin
  FDictionary.Free;
  FDictionary := Value;
End;

Procedure TDicomValue.MoveCheck(sType : String; iOffset : Integer; var Dest; ilength : Integer; bExact : Boolean);
Begin
  if bExact Then
  Begin
    If length(FBytes) <> iOffset + ilength Then
      raise EDicomException.create('Cannot read VR as '+sType+' as it is '+DICOM_VR_TYPE_NAMES_S[FKnownType]+' (length is wrong)');
  End
  Else
    if length(FBytes) < iOffset + ilength Then
      raise EDicomException.create('Cannot read VR as '+sType);
  Move(FBytes[iOffset], Dest, iLength);
End;

Function TDicomValue.AsBytes(Const Src; ilength : integer) : TBytes;
Begin
  SetLength(result, iLength);
  Move(src, result[0], ilength);
End;

Function TDicomValue.GetAsAE: String;
Begin
  if self = nil then
    result := ''
  else
    result := BytesAsString(FBytes);
End;

Function TDicomValue.GetAsAS: String;
Begin
  if self = nil then
    result := ''
  else
    result := BytesAsString(FBytes);
End;

Function TDicomValue.GetAsAT: TDicomAT;
Begin
  if self = nil then
    FillChar(result, sizeof(result), #0)
  else
    MoveCheck('AT', 0, result, 4, true);
End;

Function TDicomValue.GetAsCS: String;
Begin
  if self = nil then
    result := ''
  else
    result := BytesAsString(FBytes);
End;

Function TDicomValue.GetAsDA: TDateTime;
Begin
  if self = nil then
    result := 0
  else
    result := TFslDateTime.fromHL7(BytesAsString(FBytes)).DateTime;
End;

Function TDicomValue.GetAsDS: String;
Begin
  if self = nil then
    result := ''
  else
    result := BytesAsString(FBytes);
End;

Function TDicomValue.GetAsDT: TDateTime;
Begin
  if self = nil then
    result := 0
  else
    result := TFslDateTime.fromHL7(BytesAsString(FBytes)).DateTime;
End;

Function TDicomValue.GetAsFD: Double;
Begin
  if self = nil then
    result := 0
  else
    MoveCheck('FD', 0, result, 8, true);
End;

Function TDicomValue.GetAsFL: Single;
Begin
  if self = nil then
    result := 0
  else
    MoveCheck('FL', 0, result, 4, true);
End;

Function TDicomValue.GetAsIS: String;
Begin
  if self = nil then
    result := ''
  else
    result := BytesAsString(FBytes);
End;

Function TDicomValue.GetAsLO: String;
Begin
  if self = nil then
    result := ''
  else
    result := BytesAsString(FBytes);
End;

Function TDicomValue.GetAsLT: String;
Begin
  if self = nil then
    result := ''
  else
    result := BytesAsString(FBytes);
End;

Function TDicomValue.GetAsOF: TSingles;
Var
  i : integer;
Begin
  if self = nil then
    SetLength(result, 0)
  else
  Begin
    if (length(FBytes) mod 4 <> 0) Then
      raise EDicomException.create('cannot read data type as OF');
    SetLength(Result, length(FBytes) div 4);
    for i := 0 to Length(result) - 1 Do
      MoveCheck('OF', i*4, result[i], 4, false);
  End;
End;

Function TDicomValue.GetAsOW: TWords;
Var
  i : integer;
Begin
  if self = nil then
    SetLength(result, 0)
  else
  Begin
    if (length(FBytes) mod 2 <> 0) Then
      raise EDicomException.create('cannot read data type as OW');
    SetLength(Result, length(FBytes) div 2);
    for i := 0 to Length(result) - 1 Do
      MoveCheck('OW', i*2, result[i], 2, false);
  End;
End;

Function TDicomValue.GetAsPN: TDicomPN;
Begin
  if self = nil then
    FillChar(result, sizeof(result), 0)
  else
    result := ParsePN(BytesAsString(FBytes));
End;

Function TDicomValue.GetAsSH: String;
Begin
  if self = nil then
    result := ''
  else
    result := BytesAsString(FBytes);
End;

Function TDicomValue.GetAsSL: Integer;
Begin
  if self = nil then
    result := 0
  else
    MoveCheck('SL', 0, result, 4, true);
End;

Function TDicomValue.GetAsSS: ShortInt;
Begin
  if self = nil then
    result := 0
  else
    MoveCheck('SS', 0, result, 2, true);
End;

Function TDicomValue.GetAsST: String;
Begin
  if self = nil then
    result := ''
  else
    result := BytesAsString(FBytes);
End;

Function TDicomValue.GetAsTM: TDateTime;
Begin
  if self = nil then
    result := 0
  else
    result := TFslDateTime.fromHL7(BytesAsString(FBytes)).DateTime;
End;

Function TDicomValue.GetAsUI: String;
Begin
  if self = nil then
    result := ''
  else
    result := BytesAsString(FBytes);
End;

Function TDicomValue.GetAsUL: Cardinal;
Begin
  if self = nil then
    result := 0
  else
    MoveCheck('UL', 0, result, 4, true);
End;

Function TDicomValue.GetAsUN: TBytes;
Begin
  if self = nil then
    SetLength(result, 0)
  else
    result := FBytes;
End;

Function TDicomValue.GetAsUS: Word;
Begin
  if self = nil then
    result := 0
  else
    MoveCheck('US', 0, result, 2, true);
End;

Function TDicomValue.GetAsUT: String;
Begin
  if self = nil then
    result := ''
  else
    result := BytesAsString(FBytes);
End;

Procedure TDicomValue.SetAsAE(const Value: String);
Begin
  FBytes := StringAsBytes(Value);
End;

Procedure TDicomValue.SetAsAS(const Value: String);
Begin
  FBytes := StringAsBytes(Value);
End;

Procedure TDicomValue.SetAsAT(Value: TDicomAT);
Begin
  FBytes := AsBytes(Value, 4);
End;

Procedure TDicomValue.SetAsCS(const Value: String);
Begin
  FBytes := StringAsBytes(Value);
End;

Procedure TDicomValue.SetAsDA(const Value: TDateTime);
Begin
  FBytes := StringAsBytes(TFslDateTime.make(value, dttzUnknown).toHL7);
End;

Procedure TDicomValue.SetAsDS(const Value: String);
Begin
  FBytes := StringAsBytes(Value);
End;

Procedure TDicomValue.SetAsDT(const Value: TDateTime);
Begin
  FBytes := StringAsBytes(TFslDateTime.make(value, dttzUnknown).toHL7);
End;

Procedure TDicomValue.SetAsFD(const Value: Double);
Begin
  FBytes := AsBytes(Value, 8);
End;

Procedure TDicomValue.SetAsFL(const Value: Single);
Begin
  FBytes := AsBytes(Value, 4);
End;

Procedure TDicomValue.SetAsIS(const Value: String);
Begin
  FBytes := StringAsBytes(Value);
End;

Procedure TDicomValue.SetAsLO(const Value: String);
Begin
  FBytes := StringAsBytes(Value);
End;

Procedure TDicomValue.SetAsLT(const Value: String);
Begin
  FBytes := StringAsBytes(Value);
End;

Procedure TDicomValue.SetAsOF(const Value: TSingles);
Var
  i : Integer;
Begin
  SetLength(FBytes, Length(Value)*4);
  For i := 0 to Length(Value) - 1 do
    Move(Value[i], FBytes[i*4], 4);
End;

Procedure TDicomValue.SetAsOW(const Value: TWords);
Var
  i : Integer;
Begin
  SetLength(FBytes, Length(Value)*2);
  For i := 0 to Length(Value) - 1 do
    Move(Value[i], FBytes[(i*2)+1], 2);
End;

Procedure TDicomValue.SetAsPN(const Value: TDicomPN);
Var
  s : String;
Begin
  s := Value.FamilyName+'^'+Value.GivenName+'^'+Value.MiddleName+'^'+Value.NamePrefix+'^'+Value.NameSuffix;
  while (s[length(s)] = '^') do
    delete(s, length(s), 1);
  FBytes := StringAsBytes(s);
End;

Procedure TDicomValue.SetAsSH(const Value: String);
Begin
  FBytes := StringAsBytes(Value);
End;

Procedure TDicomValue.SetAsSL(const Value: Integer);
Begin
  FBytes := AsBytes(Value, 4);
End;

Procedure TDicomValue.SetAsSS(const Value: ShortInt);
Begin
  FBytes := AsBytes(Value, 2);
End;

Procedure TDicomValue.SetAsST(const Value: String);
Begin
  FBytes := StringAsBytes(Value);
End;

Procedure TDicomValue.SetAsTM(const Value: TDateTime);
Begin
  FBytes := StringAsBytes(TFslDateTime.make(value, dttzUnknown).toHL7);
End;

Procedure TDicomValue.SetAsUI(const Value: String);
Begin
  FBytes := StringAsBytes(Value);
End;

Procedure TDicomValue.SetAsUL(const Value: Cardinal);
Begin
  FBytes := AsBytes(Value, 4);
End;

Procedure TDicomValue.SetAsUN(const Value: TBytes);
Begin
  FBytes := Value;
End;

Procedure TDicomValue.SetAsUS(const Value: Word);
Begin
  FBytes := AsBytes(Value, 2);
End;

Procedure TDicomValue.SetAsUT(const Value: String);
Begin
  FBytes := StringAsBytes(Value);
End;


Function TDicomValue.GetAsString: String;
Var
  oBuilder : TFslStringBuilder;
  aWords : TWords;
  aSingles : TSingles;
  i : Integer;
Begin
  if self = nil then
  begin
    result := '';
    exit;
  end;

  SetLength(aWords, 0);
  SetLength(aSingles, 0);
  case FKnownType of
    dvtCS : result := AsCS;
    dvtSH : result := AsSH;
    dvtLO : result := AsLO;
    dvtST : result := AsST;
    dvtLT : result := AsLT;
    dvtUT : result := AsUT;
    dvtAE : result := AsAE;
    dvtPN : result := BytesAsString(AsUN);
    dvtUI : result := AsUI;
    dvtDA : result := BytesAsString(AsUN);
    dvtTM : result := BytesAsString(AsUN);
    dvtDT : result := BytesAsString(AsUN);
    dvtAS : result := AsAS;
    dvtIS : result := AsIS;
    dvtDS : result := AsDS;
    dvtSS : result := inttostr(AsSS);
    dvtUS : result := inttostr(AsUS);
    dvtSL : result := inttostr(AsSL);
    dvtUL : result := inttostr(AsUL);
    dvtAT : result := FormatTagValues(AsAT.GroupId, AsAT.ElementId);
    dvtFL : result := floatToStr(AsFL);
    dvtFD : result := floatToStr(AsFD);
    dvtOB : result := BytesAsString(AsOB);
    dvtOW :
      Begin
      aWords := AsOW;
      oBuilder := TFslStringBuilder.Create;
      Try
        oBuilder.Append('[');
        if Length(aWords) > 0 Then
        Begin
          oBuilder.Append(inttostr(aWords[0]));
          For i := 0 to High(aWords) do
            oBuilder.Append(',' + inttostr(aWords[i]));
        End;
        result := oBuilder.AsString;
      Finally
        oBuilder.Free;
      End;
      End;
    dvtOF :
      Begin
      aSingles := AsOF;
      oBuilder := TFslStringBuilder.Create;
      Try
        oBuilder.Append('[');
        if Length(aSingles) > 0 Then
        Begin
          oBuilder.Append(floattostr(aSingles[0]));
          For i := 1 to Length(aSingles) do
            oBuilder.Append(',' + floattostr(aSingles[i]));
        End;
        result := oBuilder.AsString;
      Finally
        oBuilder.Free;
      End;
      End;
    dvtUN : result := EncodePercent(BytesAsString(AsUN));
  End;
End;

Procedure TDicomValue.SetAsString(const s: String);
Var
  aAT : TDicomAT;
Begin
 // todo: validate
  case FKnownType Of
    dvtCS : AsCS := s;
    dvtSH : AsSH := s;
    dvtLO : AsLO := s;
    dvtST : AsST := s;
    dvtLT : AsLT := s;
    dvtUT : AsUT := s;
    dvtAE : AsAE := s;
    dvtPN : AsUN := StringAsBytes(s); // direct
    dvtUI : AsUI := s;
    dvtDA : AsDA := TFslDateTime.fromHL7(s).DateTime;
    dvtTM : AsTM := TFslDateTime.fromHL7(s).DateTime;
    dvtDT : AsDT := TFslDateTime.fromHL7(s).DateTime;
    dvtAS : AsAS := s;
    dvtIS : AsIS := s;
    dvtDS : AsDS := s;
    dvtSS : AsSS := StrToInt(s);
    dvtUS : AsUS := StrToInt(s);
    dvtSL : AsSL := StrToInt(s);
    dvtUL : AsUL := StrToInt(s);
    dvtAT :
      Begin
      ReadTagValues('VR string value', s, aAt.GroupId, aAt.ElementId);
      AsAT := aAt;
      End;
    dvtFL : AsFL := StrToFloat(s);
    dvtFD : AsFD := StrToFloat(s);
    dvtOB : AsUN := StringAsBytes(DecodePercent(s));
    dvtOW : SetAsWords(s);
    dvtOF : raise EDicomException.create('todo'); // AsOF := s;
    dvtUN : AsUN := StringAsBytes(DecodePercent(s));
  End;
End;

Procedure TDicomValue.SetAsWords(const s: String);
Var
  i, j, t : Integer;
  aWords : TWords;
Begin
  i := 0;
  t := 0;
  SetLength(aWords, 1000);
  while (i < length(s)) do
  Begin
    inc(i);
    j := i;
    while (i < length(s)) and (s[i] <> ',') do
      inc(i);
    if t >= length(aWords) Then
      SetLength(aWords, t + 1000);
    aWords[t] := StrToInt(copy(s, j, i-j));
    inc(t);
  End;
  SetLength(aWords, t);
  AsOW := aWords;
End;

Procedure TDicomValue.ClearOffsets;
Begin
  FOffsetEnd := 0;
  FOffsetStart := 0;
End;

Function ParsePN(pn : String):TDicomPN;
  Function ReadPiece(var s : String):String;
  var
    i : integer;
  Begin
    i := pos('^', s);
    if i > 0 Then
    Begin
      result := copy(s, 1, i-1);
      s := copy(s, i+1, $FF);
    End
    else if s <> '' Then
    Begin
      result := s;
      s := '';
    End
    else
      result := ''
  End;
begin
  result.FamilyName := ReadPiece(pn);
  result.GivenName := ReadPiece(pn);
  result.MiddleName := ReadPiece(pn);
  result.NamePrefix := ReadPiece(pn);
  result.NameSuffix := ReadPiece(pn);
End;

End.
