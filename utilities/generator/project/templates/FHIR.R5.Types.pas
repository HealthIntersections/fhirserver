unit fhir5_types;

{$I fhir5.inc}

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

interface

{{mark}}

uses
  Classes, SysUtils, EncdDecd, 
  fsl_base, fsl_utilities, FHIR.Support.Signatures, fsl_stream, 
  fhir_objects, fhir_xhtml,  
  fhir5_base, fhir5_enums;

type
{{type.abstract.fwds}}
  TFhirEnum = class;
  TFhirEnumList = class;
  TFhirDate = class;
  TFhirDateList = class;
  TFhirDateTime = class;
  TFhirDateTimeList = class;
  TFhirString = class;
  TFhirStringList = class;
  TFhirInteger = class;
  TFhirIntegerList = class;
  TFhirUri = class;
  TFhirUriList = class;
  TFhirInstant = class;
  TFhirInstantList = class;
  TFhirXhtml = class;
  TFhirXhtmlList = class;
  TFhirBoolean = class;
  TFhirBooleanList = class;
  TFhirBase64Binary = class;
  TFhirBase64BinaryList = class;
  TFhirTime = class;
  TFhirTimeList = class;
  TFhirDecimal = class;
  TFhirDecimalList = class;
  TFhirCode = class;
  TFhirCodeList = class;
  TFhirCanonical = class;
  TFhirCanonicalList = class;
  TFhirOid = class;
  TFhirOidList = class;
  TFhirUuid = class;
  TFhirUuidList = class;
  TFhirUrl = class;
  TFhirUrlList = class;
  TFhirMarkdown = class;
  TFhirMarkdownList = class;
  TFhirUnsignedInt = class;
  TFhirUnsignedIntList = class;
  TFhirId = class;
  TFhirIdList = class;
  TFhirPositiveInt = class;
  TFhirPositiveIntList = class;
  TFhirInteger64 = class;
  TFhirInteger64List = class;

{{type.concrete.fwds}}

{{type.abstract.intf}}

  // a complex Enum - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Enum, and extensions
  TFhirEnum = class (TFhirPrimitiveType)
  private
    FValue: String;
    FSystem: String;
    procedure setValue(value: String);
  protected
    procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function AsStringValue : String; override;
    procedure SetStringValue(value : String); override;
  public
    constructor Create(system : String; value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirEnum; overload;
    function Clone : TFhirEnum; overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function isEnum : boolean; override;
    function fhirType : string; override;
  published
    // The actual value of the enum
    property value : String read FValue write SetValue;
    property system : String read FSystem write FSystem;
  End;    


  TFhirEnumListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirEnumList;
    function GetCurrent : TFhirEnum;
  public
    constructor Create(list : TFhirEnumList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirEnum read GetCurrent;
  end;


  TFhirEnumList = class (TFHIRObjectList)
  private

    FSystems : Array Of String;

    FCodes : Array Of String;

    function GetItemN(index : Integer) : TFhirEnum;
    procedure SetItemN(index : Integer; value : TFhirEnum);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    constructor Create(Systems, Codes : Array Of String);

    function Link : TFhirEnumList; overload;
    function Clone : TFhirEnumList; overload;
    function GetEnumerator : TFhirEnumListEnumerator;
    

    //  Add a FhirEnum to the end of the list.
    function Append : TFhirEnum;

    
    // Add an already existing FhirEnum to the end of the list.
    procedure AddItem(value : TFhirEnum); overload;

    
    // Add an already existing FhirEnum to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirEnum) : Integer;
    

    // Insert FhirEnum before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirEnum;
    

    // Insert an existing FhirEnum before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirEnum);
    
    // Get the iIndexth FhirEnum. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirEnum);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirEnum;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirEnums[index : Integer] : TFhirEnum read GetItemN write SetItemN; default;
  End;


  // a complex Date - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Date, and extensions
  TFhirDate = class (TFhirPrimitiveType)
  private
    FValue: TFslDateTime;
    procedure setValue(value: TFslDateTime);
  protected
    procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function AsStringValue : String; override;
    procedure SetStringValue(value : String); override;
  public
    constructor Create(value : TFslDateTime); overload;
    destructor Destroy; override;
    
    function Link : TFhirDate; overload;
    function Clone : TFhirDate; overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
    function dateValue : TFslDateTime; override;
  published
    // The actual value of the date
    property value : TFslDateTime read FValue write SetValue;
  End;    


  TFhirDateListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirDateList;
    function GetCurrent : TFhirDate;
  public
    constructor Create(list : TFhirDateList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirDate read GetCurrent;
  end;


  TFhirDateList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirDate;
    procedure SetItemN(index : Integer; value : TFhirDate);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirDateList; overload;
    function Clone : TFhirDateList; overload;
    function GetEnumerator : TFhirDateListEnumerator;
    

    //  Add a FhirDate to the end of the list.
    function Append : TFhirDate;

    
    // Add an already existing FhirDate to the end of the list.
    procedure AddItem(value : TFhirDate); overload;

    
    // Add an already existing FhirDate to the end of the list.
    procedure AddItem(value : TFslDateTime); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirDate) : Integer;
    

    // Insert FhirDate before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirDate;
    

    // Insert an existing FhirDate before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirDate);
    
    // Get the iIndexth FhirDate. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirDate);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirDate;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirDates[index : Integer] : TFhirDate read GetItemN write SetItemN; default;
  End;


  // a complex DateTime - has an Id attribute, and extensions.
  //  Used where a FHIR element is a DateTime, and extensions
  TFhirDateTime = class (TFhirPrimitiveType)
  private
    FValue: TFslDateTime;
    procedure setValue(value: TFslDateTime);
  protected
    procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function AsStringValue : String; override;
    procedure SetStringValue(value : String); override;
  public
    constructor Create(value : TFslDateTime); overload;
    destructor Destroy; override;
    
    function Link : TFhirDateTime; overload;
    function Clone : TFhirDateTime; overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
    function dateValue : TFslDateTime; override;
  published
    // The actual value of the dateTime
    property value : TFslDateTime read FValue write SetValue;
  End;    


  TFhirDateTimeListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirDateTimeList;
    function GetCurrent : TFhirDateTime;
  public
    constructor Create(list : TFhirDateTimeList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirDateTime read GetCurrent;
  end;


  TFhirDateTimeList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirDateTime;
    procedure SetItemN(index : Integer; value : TFhirDateTime);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirDateTimeList; overload;
    function Clone : TFhirDateTimeList; overload;
    function GetEnumerator : TFhirDateTimeListEnumerator;
    

    //  Add a FhirDateTime to the end of the list.
    function Append : TFhirDateTime;

    
    // Add an already existing FhirDateTime to the end of the list.
    procedure AddItem(value : TFhirDateTime); overload;

    
    // Add an already existing FhirDateTime to the end of the list.
    procedure AddItem(value : TFslDateTime); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirDateTime) : Integer;
    

    // Insert FhirDateTime before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirDateTime;
    

    // Insert an existing FhirDateTime before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirDateTime);
    
    // Get the iIndexth FhirDateTime. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirDateTime);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirDateTime;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirDateTimes[index : Integer] : TFhirDateTime read GetItemN write SetItemN; default;
  End;


  // a complex String - has an Id attribute, and extensions.
  //  Used where a FHIR element is a String, and extensions
  TFhirString = class (TFhirPrimitiveType)
  private
    FValue: String;
    procedure setValue(value: String);
  protected
    procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function AsStringValue : String; override;
    procedure SetStringValue(value : String); override;
  public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirString; overload;
    function Clone : TFhirString; overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
  published
    // The actual value of the string
    property value : String read FValue write SetValue;
  End;    


  TFhirStringListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirStringList;
    function GetCurrent : TFhirString;
  public
    constructor Create(list : TFhirStringList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirString read GetCurrent;
  end;


  TFhirStringList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirString;
    procedure SetItemN(index : Integer; value : TFhirString);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirStringList; overload;
    function Clone : TFhirStringList; overload;
    function GetEnumerator : TFhirStringListEnumerator;
    

    //  Add a FhirString to the end of the list.
    function Append : TFhirString;

    
    // Add an already existing FhirString to the end of the list.
    procedure AddItem(value : TFhirString); overload;

    
    // Add an already existing FhirString to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirString) : Integer;
    

    // Insert FhirString before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirString;
    

    // Insert an existing FhirString before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirString);
    
    // Get the iIndexth FhirString. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirString);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirString;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirStrings[index : Integer] : TFhirString read GetItemN write SetItemN; default;
  End;


  // a complex Integer - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Integer, and extensions
  TFhirInteger = class (TFhirPrimitiveType)
  private
    FValue: String;
    procedure setValue(value: String);
  protected
    procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function AsStringValue : String; override;
    procedure SetStringValue(value : String); override;
  public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirInteger; overload;
    function Clone : TFhirInteger; overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
  published
    // The actual value of the integer
    property value : String read FValue write SetValue;
  End;    


  TFhirIntegerListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirIntegerList;
    function GetCurrent : TFhirInteger;
  public
    constructor Create(list : TFhirIntegerList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirInteger read GetCurrent;
  end;


  TFhirIntegerList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirInteger;
    procedure SetItemN(index : Integer; value : TFhirInteger);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirIntegerList; overload;
    function Clone : TFhirIntegerList; overload;
    function GetEnumerator : TFhirIntegerListEnumerator;
    

    //  Add a FhirInteger to the end of the list.
    function Append : TFhirInteger;

    
    // Add an already existing FhirInteger to the end of the list.
    procedure AddItem(value : TFhirInteger); overload;

    
    // Add an already existing FhirInteger to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirInteger) : Integer;
    

    // Insert FhirInteger before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirInteger;
    

    // Insert an existing FhirInteger before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirInteger);
    
    // Get the iIndexth FhirInteger. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirInteger);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirInteger;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirIntegers[index : Integer] : TFhirInteger read GetItemN write SetItemN; default;
  End;


  // a complex Uri - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Uri, and extensions
  TFhirUri = class (TFhirPrimitiveType)
  private
    FValue: String;
    procedure setValue(value: String);
  protected
    procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function AsStringValue : String; override;
    procedure SetStringValue(value : String); override;
  public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirUri; overload;
    function Clone : TFhirUri; overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
  published
    // The actual value of the uri
    property value : String read FValue write SetValue;
  End;    


  TFhirUriListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirUriList;
    function GetCurrent : TFhirUri;
  public
    constructor Create(list : TFhirUriList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirUri read GetCurrent;
  end;


  TFhirUriList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirUri;
    procedure SetItemN(index : Integer; value : TFhirUri);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirUriList; overload;
    function Clone : TFhirUriList; overload;
    function GetEnumerator : TFhirUriListEnumerator;
    

    //  Add a FhirUri to the end of the list.
    function Append : TFhirUri;

    
    // Add an already existing FhirUri to the end of the list.
    procedure AddItem(value : TFhirUri); overload;

    
    // Add an already existing FhirUri to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirUri) : Integer;
    

    // Insert FhirUri before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirUri;
    

    // Insert an existing FhirUri before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirUri);
    
    // Get the iIndexth FhirUri. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirUri);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirUri;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirUris[index : Integer] : TFhirUri read GetItemN write SetItemN; default;
  End;


  // a complex Instant - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Instant, and extensions
  TFhirInstant = class (TFhirPrimitiveType)
  private
    FValue: TFslDateTime;
    procedure setValue(value: TFslDateTime);
  protected
    procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function AsStringValue : String; override;
    procedure SetStringValue(value : String); override;
  public
    constructor Create(value : TFslDateTime); overload;
    destructor Destroy; override;
    
    function Link : TFhirInstant; overload;
    function Clone : TFhirInstant; overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
    function dateValue : TFslDateTime; override;
  published
    // The actual value of the instant
    property value : TFslDateTime read FValue write SetValue;
  End;    


  TFhirInstantListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirInstantList;
    function GetCurrent : TFhirInstant;
  public
    constructor Create(list : TFhirInstantList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirInstant read GetCurrent;
  end;


  TFhirInstantList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirInstant;
    procedure SetItemN(index : Integer; value : TFhirInstant);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirInstantList; overload;
    function Clone : TFhirInstantList; overload;
    function GetEnumerator : TFhirInstantListEnumerator;
    

    //  Add a FhirInstant to the end of the list.
    function Append : TFhirInstant;

    
    // Add an already existing FhirInstant to the end of the list.
    procedure AddItem(value : TFhirInstant); overload;

    
    // Add an already existing FhirInstant to the end of the list.
    procedure AddItem(value : TFslDateTime); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirInstant) : Integer;
    

    // Insert FhirInstant before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirInstant;
    

    // Insert an existing FhirInstant before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirInstant);
    
    // Get the iIndexth FhirInstant. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirInstant);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirInstant;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirInstants[index : Integer] : TFhirInstant read GetItemN write SetItemN; default;
  End;


  // a complex Xhtml - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Xhtml, and extensions
  TFhirXhtml = class (TFhirPrimitiveType)
  private
    FValue: String;
    procedure setValue(value: String);
  protected
    procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function AsStringValue : String; override;
    procedure SetStringValue(value : String); override;
  public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirXhtml; overload;
    function Clone : TFhirXhtml; overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
  published
    // The actual value of the xhtml
    property value : String read FValue write SetValue;
  End;    


  TFhirXhtmlListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirXhtmlList;
    function GetCurrent : TFhirXhtml;
  public
    constructor Create(list : TFhirXhtmlList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirXhtml read GetCurrent;
  end;


  TFhirXhtmlList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirXhtml;
    procedure SetItemN(index : Integer; value : TFhirXhtml);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirXhtmlList; overload;
    function Clone : TFhirXhtmlList; overload;
    function GetEnumerator : TFhirXhtmlListEnumerator;
    

    //  Add a FhirXhtml to the end of the list.
    function Append : TFhirXhtml;

    
    // Add an already existing FhirXhtml to the end of the list.
    procedure AddItem(value : TFhirXhtml); overload;

    
    // Add an already existing FhirXhtml to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirXhtml) : Integer;
    

    // Insert FhirXhtml before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirXhtml;
    

    // Insert an existing FhirXhtml before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirXhtml);
    
    // Get the iIndexth FhirXhtml. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirXhtml);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirXhtml;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirXhtmls[index : Integer] : TFhirXhtml read GetItemN write SetItemN; default;
  End;


  // a complex Boolean - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Boolean, and extensions
  TFhirBoolean = class (TFhirPrimitiveType)
  private
    FValue: Boolean;
    procedure setValue(value: Boolean);
  protected
    procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function AsStringValue : String; override;
    procedure SetStringValue(value : String); override;
  public
    constructor Create(value : Boolean); overload;
    destructor Destroy; override;
    
    function Link : TFhirBoolean; overload;
    function Clone : TFhirBoolean; overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
  published
    // The actual value of the boolean
    property value : Boolean read FValue write SetValue;
  End;    


  TFhirBooleanListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirBooleanList;
    function GetCurrent : TFhirBoolean;
  public
    constructor Create(list : TFhirBooleanList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirBoolean read GetCurrent;
  end;


  TFhirBooleanList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirBoolean;
    procedure SetItemN(index : Integer; value : TFhirBoolean);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirBooleanList; overload;
    function Clone : TFhirBooleanList; overload;
    function GetEnumerator : TFhirBooleanListEnumerator;
    

    //  Add a FhirBoolean to the end of the list.
    function Append : TFhirBoolean;

    
    // Add an already existing FhirBoolean to the end of the list.
    procedure AddItem(value : TFhirBoolean); overload;

    
    // Add an already existing FhirBoolean to the end of the list.
    procedure AddItem(value : Boolean); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirBoolean) : Integer;
    

    // Insert FhirBoolean before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirBoolean;
    

    // Insert an existing FhirBoolean before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirBoolean);
    
    // Get the iIndexth FhirBoolean. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirBoolean);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirBoolean;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirBooleans[index : Integer] : TFhirBoolean read GetItemN write SetItemN; default;
  End;


  // a complex Base64Binary - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Base64Binary, and extensions
  TFhirBase64Binary = class (TFhirPrimitiveType)
  private
    FValue: TBytes;
    procedure setValue(value: TBytes);
  protected
    procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function AsStringValue : String; override;
    procedure SetStringValue(value : String); override;
  public
    constructor Create(value : TBytes); overload;
    destructor Destroy; override;
    
    function Link : TFhirBase64Binary; overload;
    function Clone : TFhirBase64Binary; overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
  published
    // The actual value of the base64Binary
    property value : TBytes read FValue write SetValue;
  End;    


  TFhirBase64BinaryListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirBase64BinaryList;
    function GetCurrent : TFhirBase64Binary;
  public
    constructor Create(list : TFhirBase64BinaryList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirBase64Binary read GetCurrent;
  end;


  TFhirBase64BinaryList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirBase64Binary;
    procedure SetItemN(index : Integer; value : TFhirBase64Binary);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirBase64BinaryList; overload;
    function Clone : TFhirBase64BinaryList; overload;
    function GetEnumerator : TFhirBase64BinaryListEnumerator;
    

    //  Add a FhirBase64Binary to the end of the list.
    function Append : TFhirBase64Binary;

    
    // Add an already existing FhirBase64Binary to the end of the list.
    procedure AddItem(value : TFhirBase64Binary); overload;

    
    // Add an already existing FhirBase64Binary to the end of the list.
    procedure AddItem(value : TBytes); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirBase64Binary) : Integer;
    

    // Insert FhirBase64Binary before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirBase64Binary;
    

    // Insert an existing FhirBase64Binary before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirBase64Binary);
    
    // Get the iIndexth FhirBase64Binary. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirBase64Binary);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirBase64Binary;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirBase64Binaries[index : Integer] : TFhirBase64Binary read GetItemN write SetItemN; default;
  End;


  // a complex Time - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Time, and extensions
  TFhirTime = class (TFhirPrimitiveType)
  private
    FValue: String;
    procedure setValue(value: String);
  protected
    procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function AsStringValue : String; override;
    procedure SetStringValue(value : String); override;
  public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirTime; overload;
    function Clone : TFhirTime; overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
  published
    // The actual value of the time
    property value : String read FValue write SetValue;
  End;    


  TFhirTimeListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirTimeList;
    function GetCurrent : TFhirTime;
  public
    constructor Create(list : TFhirTimeList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirTime read GetCurrent;
  end;


  TFhirTimeList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirTime;
    procedure SetItemN(index : Integer; value : TFhirTime);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirTimeList; overload;
    function Clone : TFhirTimeList; overload;
    function GetEnumerator : TFhirTimeListEnumerator;
    

    //  Add a FhirTime to the end of the list.
    function Append : TFhirTime;

    
    // Add an already existing FhirTime to the end of the list.
    procedure AddItem(value : TFhirTime); overload;

    
    // Add an already existing FhirTime to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirTime) : Integer;
    

    // Insert FhirTime before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirTime;
    

    // Insert an existing FhirTime before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirTime);
    
    // Get the iIndexth FhirTime. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirTime);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirTime;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirTimes[index : Integer] : TFhirTime read GetItemN write SetItemN; default;
  End;


  // a complex Decimal - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Decimal, and extensions
  TFhirDecimal = class (TFhirPrimitiveType)
  private
    FValue: String;
    procedure setValue(value: String);
  protected
    procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function AsStringValue : String; override;
    procedure SetStringValue(value : String); override;
  public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirDecimal; overload;
    function Clone : TFhirDecimal; overload;
    procedure Assign(oSource : TFslObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
    function fhirType : string; override;
  published
    // The actual value of the decimal
    property value : String read FValue write SetValue;
  End;    


  TFhirDecimalListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirDecimalList;
    function GetCurrent : TFhirDecimal;
  public
    constructor Create(list : TFhirDecimalList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirDecimal read GetCurrent;
  end;


  TFhirDecimalList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirDecimal;
    procedure SetItemN(index : Integer; value : TFhirDecimal);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirDecimalList; overload;
    function Clone : TFhirDecimalList; overload;
    function GetEnumerator : TFhirDecimalListEnumerator;
    

    //  Add a FhirDecimal to the end of the list.
    function Append : TFhirDecimal;

    
    // Add an already existing FhirDecimal to the end of the list.
    procedure AddItem(value : TFhirDecimal); overload;

    
    // Add an already existing FhirDecimal to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirDecimal) : Integer;
    

    // Insert FhirDecimal before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirDecimal;
    

    // Insert an existing FhirDecimal before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirDecimal);
    
    // Get the iIndexth FhirDecimal. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirDecimal);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirDecimal;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirDecimals[index : Integer] : TFhirDecimal read GetItemN write SetItemN; default;
  End;


  // a complex Code - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Code, and extensions
  TFhirCode = class (TFhirString)
  private
  public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirCode; overload;
    function Clone : TFhirCode; overload;
    function fhirType : string; override;
  End;    


  TFhirCodeListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirCodeList;
    function GetCurrent : TFhirCode;
  public
    constructor Create(list : TFhirCodeList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirCode read GetCurrent;
  end;


  TFhirCodeList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirCode;
    procedure SetItemN(index : Integer; value : TFhirCode);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirCodeList; overload;
    function Clone : TFhirCodeList; overload;
    function GetEnumerator : TFhirCodeListEnumerator;
    

    //  Add a FhirCode to the end of the list.
    function Append : TFhirCode;

    
    // Add an already existing FhirCode to the end of the list.
    procedure AddItem(value : TFhirCode); overload;

    
    // Add an already existing FhirCode to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirCode) : Integer;
    

    // Insert FhirCode before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirCode;
    

    // Insert an existing FhirCode before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirCode);
    
    // Get the iIndexth FhirCode. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirCode);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirCode;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirCodes[index : Integer] : TFhirCode read GetItemN write SetItemN; default;
  End;


  // a complex Canonical - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Canonical, and extensions
  TFhirCanonical = class (TFhirUri)
  private
  public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirCanonical; overload;
    function Clone : TFhirCanonical; overload;
    function fhirType : string; override;
  End;    


  TFhirCanonicalListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirCanonicalList;
    function GetCurrent : TFhirCanonical;
  public
    constructor Create(list : TFhirCanonicalList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirCanonical read GetCurrent;
  end;


  TFhirCanonicalList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirCanonical;
    procedure SetItemN(index : Integer; value : TFhirCanonical);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirCanonicalList; overload;
    function Clone : TFhirCanonicalList; overload;
    function GetEnumerator : TFhirCanonicalListEnumerator;
    

    //  Add a FhirCanonical to the end of the list.
    function Append : TFhirCanonical;

    
    // Add an already existing FhirCanonical to the end of the list.
    procedure AddItem(value : TFhirCanonical); overload;

    
    // Add an already existing FhirCanonical to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirCanonical) : Integer;
    

    // Insert FhirCanonical before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirCanonical;
    

    // Insert an existing FhirCanonical before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirCanonical);
    
    // Get the iIndexth FhirCanonical. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirCanonical);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirCanonical;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirCanonicals[index : Integer] : TFhirCanonical read GetItemN write SetItemN; default;
  End;


  // a complex Oid - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Oid, and extensions
  TFhirOid = class (TFhirUri)
  private
  public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirOid; overload;
    function Clone : TFhirOid; overload;
    function fhirType : string; override;
  End;    


  TFhirOidListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirOidList;
    function GetCurrent : TFhirOid;
  public
    constructor Create(list : TFhirOidList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirOid read GetCurrent;
  end;


  TFhirOidList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirOid;
    procedure SetItemN(index : Integer; value : TFhirOid);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirOidList; overload;
    function Clone : TFhirOidList; overload;
    function GetEnumerator : TFhirOidListEnumerator;
    

    //  Add a FhirOid to the end of the list.
    function Append : TFhirOid;

    
    // Add an already existing FhirOid to the end of the list.
    procedure AddItem(value : TFhirOid); overload;

    
    // Add an already existing FhirOid to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirOid) : Integer;
    

    // Insert FhirOid before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirOid;
    

    // Insert an existing FhirOid before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirOid);
    
    // Get the iIndexth FhirOid. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirOid);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirOid;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirOids[index : Integer] : TFhirOid read GetItemN write SetItemN; default;
  End;


  // a complex Uuid - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Uuid, and extensions
  TFhirUuid = class (TFhirUri)
  private
  public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirUuid; overload;
    function Clone : TFhirUuid; overload;
    function fhirType : string; override;
  End;    


  TFhirUuidListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirUuidList;
    function GetCurrent : TFhirUuid;
  public
    constructor Create(list : TFhirUuidList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirUuid read GetCurrent;
  end;


  TFhirUuidList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirUuid;
    procedure SetItemN(index : Integer; value : TFhirUuid);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirUuidList; overload;
    function Clone : TFhirUuidList; overload;
    function GetEnumerator : TFhirUuidListEnumerator;
    

    //  Add a FhirUuid to the end of the list.
    function Append : TFhirUuid;

    
    // Add an already existing FhirUuid to the end of the list.
    procedure AddItem(value : TFhirUuid); overload;

    
    // Add an already existing FhirUuid to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirUuid) : Integer;
    

    // Insert FhirUuid before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirUuid;
    

    // Insert an existing FhirUuid before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirUuid);
    
    // Get the iIndexth FhirUuid. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirUuid);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirUuid;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirUuids[index : Integer] : TFhirUuid read GetItemN write SetItemN; default;
  End;


  // a complex Url - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Url, and extensions
  TFhirUrl = class (TFhirUri)
  private
  public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirUrl; overload;
    function Clone : TFhirUrl; overload;
    function fhirType : string; override;
  End;    


  TFhirUrlListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirUrlList;
    function GetCurrent : TFhirUrl;
  public
    constructor Create(list : TFhirUrlList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirUrl read GetCurrent;
  end;


  TFhirUrlList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirUrl;
    procedure SetItemN(index : Integer; value : TFhirUrl);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirUrlList; overload;
    function Clone : TFhirUrlList; overload;
    function GetEnumerator : TFhirUrlListEnumerator;
    

    //  Add a FhirUrl to the end of the list.
    function Append : TFhirUrl;

    
    // Add an already existing FhirUrl to the end of the list.
    procedure AddItem(value : TFhirUrl); overload;

    
    // Add an already existing FhirUrl to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirUrl) : Integer;
    

    // Insert FhirUrl before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirUrl;
    

    // Insert an existing FhirUrl before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirUrl);
    
    // Get the iIndexth FhirUrl. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirUrl);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirUrl;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirUrls[index : Integer] : TFhirUrl read GetItemN write SetItemN; default;
  End;


  // a complex Markdown - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Markdown, and extensions
  TFhirMarkdown = class (TFhirString)
  private
  public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirMarkdown; overload;
    function Clone : TFhirMarkdown; overload;
    function fhirType : string; override;
  End;    


  TFhirMarkdownListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirMarkdownList;
    function GetCurrent : TFhirMarkdown;
  public
    constructor Create(list : TFhirMarkdownList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirMarkdown read GetCurrent;
  end;


  TFhirMarkdownList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirMarkdown;
    procedure SetItemN(index : Integer; value : TFhirMarkdown);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirMarkdownList; overload;
    function Clone : TFhirMarkdownList; overload;
    function GetEnumerator : TFhirMarkdownListEnumerator;
    

    //  Add a FhirMarkdown to the end of the list.
    function Append : TFhirMarkdown;

    
    // Add an already existing FhirMarkdown to the end of the list.
    procedure AddItem(value : TFhirMarkdown); overload;

    
    // Add an already existing FhirMarkdown to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirMarkdown) : Integer;
    

    // Insert FhirMarkdown before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirMarkdown;
    

    // Insert an existing FhirMarkdown before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirMarkdown);
    
    // Get the iIndexth FhirMarkdown. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirMarkdown);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirMarkdown;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirMarkdowns[index : Integer] : TFhirMarkdown read GetItemN write SetItemN; default;
  End;


  // a complex UnsignedInt - has an Id attribute, and extensions.
  //  Used where a FHIR element is a UnsignedInt, and extensions
  TFhirUnsignedInt = class (TFhirInteger)
  private
  public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirUnsignedInt; overload;
    function Clone : TFhirUnsignedInt; overload;
    function fhirType : string; override;
  End;    


  TFhirUnsignedIntListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirUnsignedIntList;
    function GetCurrent : TFhirUnsignedInt;
  public
    constructor Create(list : TFhirUnsignedIntList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirUnsignedInt read GetCurrent;
  end;


  TFhirUnsignedIntList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirUnsignedInt;
    procedure SetItemN(index : Integer; value : TFhirUnsignedInt);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirUnsignedIntList; overload;
    function Clone : TFhirUnsignedIntList; overload;
    function GetEnumerator : TFhirUnsignedIntListEnumerator;
    

    //  Add a FhirUnsignedInt to the end of the list.
    function Append : TFhirUnsignedInt;

    
    // Add an already existing FhirUnsignedInt to the end of the list.
    procedure AddItem(value : TFhirUnsignedInt); overload;

    
    // Add an already existing FhirUnsignedInt to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirUnsignedInt) : Integer;
    

    // Insert FhirUnsignedInt before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirUnsignedInt;
    

    // Insert an existing FhirUnsignedInt before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirUnsignedInt);
    
    // Get the iIndexth FhirUnsignedInt. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirUnsignedInt);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirUnsignedInt;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirUnsignedInts[index : Integer] : TFhirUnsignedInt read GetItemN write SetItemN; default;
  End;


  // a complex Id - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Id, and extensions
  TFhirId = class (TFhirString)
  private
  public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirId; overload;
    function Clone : TFhirId; overload;
    function fhirType : string; override;
  End;    


  TFhirIdListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirIdList;
    function GetCurrent : TFhirId;
  public
    constructor Create(list : TFhirIdList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirId read GetCurrent;
  end;


  TFhirIdList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirId;
    procedure SetItemN(index : Integer; value : TFhirId);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirIdList; overload;
    function Clone : TFhirIdList; overload;
    function GetEnumerator : TFhirIdListEnumerator;
    

    //  Add a FhirId to the end of the list.
    function Append : TFhirId;

    
    // Add an already existing FhirId to the end of the list.
    procedure AddItem(value : TFhirId); overload;

    
    // Add an already existing FhirId to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirId) : Integer;
    

    // Insert FhirId before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirId;
    

    // Insert an existing FhirId before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirId);
    
    // Get the iIndexth FhirId. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirId);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirId;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirIds[index : Integer] : TFhirId read GetItemN write SetItemN; default;
  End;


  // a complex PositiveInt - has an Id attribute, and extensions.
  //  Used where a FHIR element is a PositiveInt, and extensions
  TFhirPositiveInt = class (TFhirInteger)
  private
  public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirPositiveInt; overload;
    function Clone : TFhirPositiveInt; overload;
    function fhirType : string; override;
  End;    


  TFhirPositiveIntListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirPositiveIntList;
    function GetCurrent : TFhirPositiveInt;
  public
    constructor Create(list : TFhirPositiveIntList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirPositiveInt read GetCurrent;
  end;


  TFhirPositiveIntList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirPositiveInt;
    procedure SetItemN(index : Integer; value : TFhirPositiveInt);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirPositiveIntList; overload;
    function Clone : TFhirPositiveIntList; overload;
    function GetEnumerator : TFhirPositiveIntListEnumerator;
    

    //  Add a FhirPositiveInt to the end of the list.
    function Append : TFhirPositiveInt;

    
    // Add an already existing FhirPositiveInt to the end of the list.
    procedure AddItem(value : TFhirPositiveInt); overload;

    
    // Add an already existing FhirPositiveInt to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirPositiveInt) : Integer;
    

    // Insert FhirPositiveInt before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirPositiveInt;
    

    // Insert an existing FhirPositiveInt before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirPositiveInt);
    
    // Get the iIndexth FhirPositiveInt. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirPositiveInt);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirPositiveInt;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirPositiveInts[index : Integer] : TFhirPositiveInt read GetItemN write SetItemN; default;
  End;

  // a complex Integer64 - has an Id attribute, and extensions.
  //  Used where a FHIR element is a Integer64, and extensions
  TFhirInteger64 = class (TFhirInteger)
  private
  public
    constructor Create(value : String); overload;
    destructor Destroy; override;
    
    function Link : TFhirInteger64; overload;
    function Clone : TFhirInteger64; overload;
    function fhirType : string; override;
  End;    


  TFhirInteger64ListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirInteger64List;
    function GetCurrent : TFhirInteger64;
  public
    constructor Create(list : TFhirInteger64List);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirInteger64 read GetCurrent;
  end;


  TFhirInteger64List = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirInteger64;
    procedure SetItemN(index : Integer; value : TFhirInteger64);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirInteger64List; overload;
    function Clone : TFhirInteger64List; overload;
    function GetEnumerator : TFhirInteger64ListEnumerator;
    

    //  Add a FhirInteger64 to the end of the list.
    function Append : TFhirInteger64;

    
    // Add an already existing FhirInteger64 to the end of the list.
    procedure AddItem(value : TFhirInteger64); overload;

    
    // Add an already existing FhirInteger64 to the end of the list.
    procedure AddItem(value : String); overload;

    
    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirInteger64) : Integer;
    

    // Insert FhirInteger64 before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirInteger64;
    

    // Insert an existing FhirInteger64 before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirInteger64);
    
    // Get the iIndexth FhirInteger64. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirInteger64);
    
    // The number of items in the collection
    function Item(index : Integer) : TFhirInteger64;
    
    // The number of items in the collection
    function Count : Integer; overload;
    
    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);
    
    // Remove All Items from the list
    procedure ClearItems;
    
    property FhirInteger64s[index : Integer] : TFhirInteger64 read GetItemN write SetItemN; default;
  End;

{{type.concrete.intf}}

function asEnum(systems, values: array of String; obj : TFHIRObject) : TFhirEnum;
function asDate(obj : TFHIRObject) : TFhirDate;
function asDateTime(obj : TFHIRObject) : TFhirDateTime;
function asString(obj : TFHIRObject) : TFhirString;
function asInteger(obj : TFHIRObject) : TFhirInteger;
function asUri(obj : TFHIRObject) : TFhirUri;
function asInstant(obj : TFHIRObject) : TFhirInstant;
function asXhtml(obj : TFHIRObject) : TFhirXhtml;
function asBoolean(obj : TFHIRObject) : TFhirBoolean;
function asBase64Binary(obj : TFHIRObject) : TFhirBase64Binary;
function asTime(obj : TFHIRObject) : TFhirTime;
function asDecimal(obj : TFHIRObject) : TFhirDecimal;
function asCode(obj : TFHIRObject) : TFhirCode;
function asCanonical(obj : TFHIRObject) : TFhirCanonical;
function asOid(obj : TFHIRObject) : TFhirOid;
function asUuid(obj : TFHIRObject) : TFhirUuid;
function asUrl(obj : TFHIRObject) : TFhirUrl;
function asMarkdown(obj : TFHIRObject) : TFhirMarkdown;
function asUnsignedInt(obj : TFHIRObject) : TFhirUnsignedInt;
function asId(obj : TFHIRObject) : TFhirId;
function asPositiveInt(obj : TFHIRObject) : TFhirPositiveInt;
function asInteger64(obj : TFHIRObject) : TFhirInteger64;

implementation

uses
  fhir5_elementmodel, fhir5_utilities;

{{type.abstract.impl}}

{ TFhirEnum }

Constructor TFhirEnum.Create(system : String; value : String);
begin
  Create;
  FSystem := system;
  FValue := value;
end;

destructor TFhirEnum.Destroy;
begin
  inherited;
end;

function TFhirEnum.fhirType : string;
begin
  result := 'code';
end;

function TFHIREnum.isEnum : boolean;
begin
  result := true;
end;

procedure TFhirEnum.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirEnum.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'enum', false, nil, FValue));
end;

procedure TFhirEnum.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirEnum(oSource).Value;
end;

function TFhirEnum.AsStringValue : string;
begin
  result := FValue;
end;

procedure TFhirEnum.SetStringValue(value : string);
begin
  FValue := value;
end;

function TFhirEnum.equals(other : TObject) : boolean; 
var
  o : TFhirEnum;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirEnum)) then
    result := false
  else
  begin
    o := TFhirEnum(other);
    result := o.value = value;
  end;
end;

function TFhirEnum.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue = '');
end;

function TFhirEnum.Link : TFhirEnum;
begin
  result := TFhirEnum(inherited Link);
end;

function TFhirEnum.Clone : TFhirEnum;
begin
  result := TFhirEnum(inherited Clone);
end;

procedure TFhirEnum.setValue(value : String);
begin
  FValue := value;
end;


{ TFhirEnumListEnumerator }

Constructor TFhirEnumListEnumerator.Create(list : TFhirEnumList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirEnumListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirEnumListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirEnumListEnumerator.GetCurrent : TFhirEnum;
begin
  Result := FList[FIndex];
end;


{ TFhirEnumList }
procedure TFhirEnumList.AddItem(value: TFhirEnum);
begin
  assert(value.ClassName = 'TFhirEnum', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirEnum');
  add(value);
end;


constructor TFhirEnumList.Create(Systems, Codes : Array Of String);
var
  i : integer;
begin
  inherited create;
  SetLength(FSystems, length(systems));
  SetLength(FCodes, length(codes));
  for i := 0 to length(systems) - 1 do
  begin
    FSystems[i] := systems[i];
    FCodes[i] := codes[i];
  end;
end;

procedure TFhirEnumList.AddItem(value: String);
begin
  add(TFhirEnum.create(FSystems[StringArrayIndexOf(FCodes, value)], value));
end;


function TFhirEnumList.Append: TFhirEnum;
begin
  result := TFhirEnum.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirEnumList.ClearItems;
begin
  Clear;
end;

function TFhirEnumList.GetEnumerator : TFhirEnumListEnumerator;
begin
  result := TFhirEnumListEnumerator.Create(self.link);
end;

function TFhirEnumList.Clone: TFhirEnumList;
begin
  result := TFhirEnumList(inherited Clone);
end;

function TFhirEnumList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirEnumList.GetItemN(index: Integer): TFhirEnum;
begin
  result := TFhirEnum(ObjectByIndex[index]);
end;

function TFhirEnumList.ItemClass: TFslObjectClass;
begin
  result := TFhirEnum;
end;
function TFhirEnumList.IndexOf(value: TFhirEnum): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirEnumList.Insert(index: Integer): TFhirEnum;
begin
  result := TFhirEnum.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirEnumList.InsertItem(index: Integer; value: TFhirEnum);
begin
  assert(value is TFhirEnum);
  Inherited Insert(index, value);
end;

function TFhirEnumList.Item(index: Integer): TFhirEnum;
begin
  result := TFhirEnum(ObjectByIndex[index]);
end;

function TFhirEnumList.Link: TFhirEnumList;
begin
  result := TFhirEnumList(inherited Link);
end;

procedure TFhirEnumList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirEnumList.SetItemByIndex(index: Integer; value: TFhirEnum);
begin
  assert(value is TFhirEnum);
  FhirEnums[index] := value;
end;

procedure TFhirEnumList.SetItemN(index: Integer; value: TFhirEnum);
begin
  assert(value is TFhirEnum);
  ObjectByIndex[index] := value;
end;

{ TFhirDate }

Constructor TFhirDate.Create(value : TFslDateTime);
begin
  Create;
  FValue := value.fixPrecision(dtpDay);
end;

destructor TFhirDate.Destroy;
begin
  inherited;
end;

function TFhirDate.fhirType : string;
begin
  result := 'date';
end;

function TFhirDate.dateValue : TFslDateTime;
begin
  result := FValue;
end;

procedure TFhirDate.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirDate.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    if (FValue.notNull) then
      oList.add(TFHIRProperty.create(self, 'value', 'date', false, nil, FValue.ToString));
end;

procedure TFhirDate.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirDate(oSource).Value;
end;

function TFhirDate.AsStringValue : string;
begin
  if (FValue.null) then
    result := ''
  else
    result := FValue.toXml;
end;

procedure TFhirDate.SetStringValue(value : string);
begin
  if (value = '') then
    FValue := TFslDateTime.makeNull
  else
    FValue := TFslDateTime.fromXml(value);
end;

function TFhirDate.equals(other : TObject) : boolean; 
var
  o : TFhirDate;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirDate)) then
    result := false
  else
  begin
    o := TFhirDate(other);
    result := o.value.equal(value);
  end;
end;

function TFhirDate.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue.null);
end;

function TFhirDate.Link : TFhirDate;
begin
  result := TFhirDate(inherited Link);
end;

function TFhirDate.Clone : TFhirDate;
begin
  result := TFhirDate(inherited Clone);
end;

procedure TFhirDate.setValue(value : TFslDateTime);
begin
  FValue := value;
end;


{ TFhirDateListEnumerator }

Constructor TFhirDateListEnumerator.Create(list : TFhirDateList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirDateListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirDateListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirDateListEnumerator.GetCurrent : TFhirDate;
begin
  Result := FList[FIndex];
end;


{ TFhirDateList }
procedure TFhirDateList.AddItem(value: TFhirDate);
begin
  assert(value.ClassName = 'TFhirDate', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirDate');
  add(value);
end;


procedure TFhirDateList.AddItem(value: TFslDateTime);
begin
  add(TFhirDate.create(value));
end;


function TFhirDateList.Append: TFhirDate;
begin
  result := TFhirDate.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirDateList.ClearItems;
begin
  Clear;
end;

function TFhirDateList.GetEnumerator : TFhirDateListEnumerator;
begin
  result := TFhirDateListEnumerator.Create(self.link);
end;

function TFhirDateList.Clone: TFhirDateList;
begin
  result := TFhirDateList(inherited Clone);
end;

function TFhirDateList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirDateList.GetItemN(index: Integer): TFhirDate;
begin
  result := TFhirDate(ObjectByIndex[index]);
end;

function TFhirDateList.ItemClass: TFslObjectClass;
begin
  result := TFhirDate;
end;
function TFhirDateList.IndexOf(value: TFhirDate): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirDateList.Insert(index: Integer): TFhirDate;
begin
  result := TFhirDate.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirDateList.InsertItem(index: Integer; value: TFhirDate);
begin
  assert(value is TFhirDate);
  Inherited Insert(index, value);
end;

function TFhirDateList.Item(index: Integer): TFhirDate;
begin
  result := TFhirDate(ObjectByIndex[index]);
end;

function TFhirDateList.Link: TFhirDateList;
begin
  result := TFhirDateList(inherited Link);
end;

procedure TFhirDateList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirDateList.SetItemByIndex(index: Integer; value: TFhirDate);
begin
  assert(value is TFhirDate);
  FhirDates[index] := value;
end;

procedure TFhirDateList.SetItemN(index: Integer; value: TFhirDate);
begin
  assert(value is TFhirDate);
  ObjectByIndex[index] := value;
end;

{ TFhirDateTime }

Constructor TFhirDateTime.Create(value : TFslDateTime);
begin
  Create;
  FValue := value;
end;

destructor TFhirDateTime.Destroy;
begin
  inherited;
end;

function TFhirDateTime.fhirType : string;
begin
  result := 'dateTime';
end;

function TFhirDateTime.dateValue : TFslDateTime;
begin
  result := FValue;
end;

procedure TFhirDateTime.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirDateTime.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    if (FValue.notNull) then
      oList.add(TFHIRProperty.create(self, 'value', 'dateTime', false, nil, FValue.ToString));
end;

procedure TFhirDateTime.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirDateTime(oSource).Value;
end;

function TFhirDateTime.AsStringValue : string;
begin
  if (FValue.null) then
    result := ''
  else
    result := FValue.toXml;
end;

procedure TFhirDateTime.SetStringValue(value : string);
begin
  if (value = '') then
    FValue := TFslDateTime.makeNull
  else
    FValue := TFslDateTime.fromXml(value);
end;

function TFhirDateTime.equals(other : TObject) : boolean; 
var
  o : TFhirDateTime;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirDateTime)) then
    result := false
  else
  begin
    o := TFhirDateTime(other);
    result := o.value.equal(value);
  end;
end;

function TFhirDateTime.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue.null);
end;

function TFhirDateTime.Link : TFhirDateTime;
begin
  result := TFhirDateTime(inherited Link);
end;

function TFhirDateTime.Clone : TFhirDateTime;
begin
  result := TFhirDateTime(inherited Clone);
end;

procedure TFhirDateTime.setValue(value : TFslDateTime);
begin
  FValue := value;
end;


{ TFhirDateTimeListEnumerator }

Constructor TFhirDateTimeListEnumerator.Create(list : TFhirDateTimeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirDateTimeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirDateTimeListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirDateTimeListEnumerator.GetCurrent : TFhirDateTime;
begin
  Result := FList[FIndex];
end;


{ TFhirDateTimeList }
procedure TFhirDateTimeList.AddItem(value: TFhirDateTime);
begin
  assert(value.ClassName = 'TFhirDateTime', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirDateTime');
  add(value);
end;


procedure TFhirDateTimeList.AddItem(value: TFslDateTime);
begin
  add(TFhirDateTime.create(value));
end;


function TFhirDateTimeList.Append: TFhirDateTime;
begin
  result := TFhirDateTime.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirDateTimeList.ClearItems;
begin
  Clear;
end;

function TFhirDateTimeList.GetEnumerator : TFhirDateTimeListEnumerator;
begin
  result := TFhirDateTimeListEnumerator.Create(self.link);
end;

function TFhirDateTimeList.Clone: TFhirDateTimeList;
begin
  result := TFhirDateTimeList(inherited Clone);
end;

function TFhirDateTimeList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirDateTimeList.GetItemN(index: Integer): TFhirDateTime;
begin
  result := TFhirDateTime(ObjectByIndex[index]);
end;

function TFhirDateTimeList.ItemClass: TFslObjectClass;
begin
  result := TFhirDateTime;
end;
function TFhirDateTimeList.IndexOf(value: TFhirDateTime): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirDateTimeList.Insert(index: Integer): TFhirDateTime;
begin
  result := TFhirDateTime.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirDateTimeList.InsertItem(index: Integer; value: TFhirDateTime);
begin
  assert(value is TFhirDateTime);
  Inherited Insert(index, value);
end;

function TFhirDateTimeList.Item(index: Integer): TFhirDateTime;
begin
  result := TFhirDateTime(ObjectByIndex[index]);
end;

function TFhirDateTimeList.Link: TFhirDateTimeList;
begin
  result := TFhirDateTimeList(inherited Link);
end;

procedure TFhirDateTimeList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirDateTimeList.SetItemByIndex(index: Integer; value: TFhirDateTime);
begin
  assert(value is TFhirDateTime);
  FhirDateTimes[index] := value;
end;

procedure TFhirDateTimeList.SetItemN(index: Integer; value: TFhirDateTime);
begin
  assert(value is TFhirDateTime);
  ObjectByIndex[index] := value;
end;

{ TFhirString }

Constructor TFhirString.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirString.Destroy;
begin
  inherited;
end;

function TFhirString.fhirType : string;
begin
  result := 'string';
end;

procedure TFhirString.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirString.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'string', false, nil, FValue));
end;

procedure TFhirString.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirString(oSource).Value;
end;

function TFhirString.AsStringValue : string;
begin
  result := FValue;
end;

procedure TFhirString.SetStringValue(value : string);
begin
  FValue := value;
end;

function TFhirString.equals(other : TObject) : boolean; 
var
  o : TFhirString;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirString)) then
    result := false
  else
  begin
    o := TFhirString(other);
    result := o.value = value;
  end;
end;

function TFhirString.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue = '');
end;

function TFhirString.Link : TFhirString;
begin
  result := TFhirString(inherited Link);
end;

function TFhirString.Clone : TFhirString;
begin
  result := TFhirString(inherited Clone);
end;

procedure TFhirString.setValue(value : String);
begin
  FValue := value;
end;


{ TFhirStringListEnumerator }

Constructor TFhirStringListEnumerator.Create(list : TFhirStringList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirStringListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirStringListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirStringListEnumerator.GetCurrent : TFhirString;
begin
  Result := FList[FIndex];
end;


{ TFhirStringList }
procedure TFhirStringList.AddItem(value: TFhirString);
begin
  assert(value.ClassName = 'TFhirString', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirString');
  add(value);
end;


procedure TFhirStringList.AddItem(value: String);
begin
  add(TFhirString.create(value));
end;


function TFhirStringList.Append: TFhirString;
begin
  result := TFhirString.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirStringList.ClearItems;
begin
  Clear;
end;

function TFhirStringList.GetEnumerator : TFhirStringListEnumerator;
begin
  result := TFhirStringListEnumerator.Create(self.link);
end;

function TFhirStringList.Clone: TFhirStringList;
begin
  result := TFhirStringList(inherited Clone);
end;

function TFhirStringList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirStringList.GetItemN(index: Integer): TFhirString;
begin
  result := TFhirString(ObjectByIndex[index]);
end;

function TFhirStringList.ItemClass: TFslObjectClass;
begin
  result := TFhirString;
end;
function TFhirStringList.IndexOf(value: TFhirString): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirStringList.Insert(index: Integer): TFhirString;
begin
  result := TFhirString.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirStringList.InsertItem(index: Integer; value: TFhirString);
begin
  assert(value is TFhirString);
  Inherited Insert(index, value);
end;

function TFhirStringList.Item(index: Integer): TFhirString;
begin
  result := TFhirString(ObjectByIndex[index]);
end;

function TFhirStringList.Link: TFhirStringList;
begin
  result := TFhirStringList(inherited Link);
end;

procedure TFhirStringList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirStringList.SetItemByIndex(index: Integer; value: TFhirString);
begin
  assert(value is TFhirString);
  FhirStrings[index] := value;
end;

procedure TFhirStringList.SetItemN(index: Integer; value: TFhirString);
begin
  assert(value is TFhirString);
  ObjectByIndex[index] := value;
end;

{ TFhirInteger }

Constructor TFhirInteger.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirInteger.Destroy;
begin
  inherited;
end;

function TFhirInteger.fhirType : string;
begin
  result := 'integer';
end;

procedure TFhirInteger.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirInteger.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'integer', false, nil, FValue));
end;

procedure TFhirInteger.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirInteger(oSource).Value;
end;

function TFhirInteger.AsStringValue : string;
begin
  result := FValue;
end;

procedure TFhirInteger.SetStringValue(value : string);
begin
  FValue := value;
end;

function TFhirInteger.equals(other : TObject) : boolean; 
var
  o : TFhirInteger;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirInteger)) then
    result := false
  else
  begin
    o := TFhirInteger(other);
    result := o.value = value;
  end;
end;

function TFhirInteger.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue = '');
end;

function TFhirInteger.Link : TFhirInteger;
begin
  result := TFhirInteger(inherited Link);
end;

function TFhirInteger.Clone : TFhirInteger;
begin
  result := TFhirInteger(inherited Clone);
end;

procedure TFhirInteger.setValue(value : String);
begin
  FValue := value;
end;


{ TFhirIntegerListEnumerator }

Constructor TFhirIntegerListEnumerator.Create(list : TFhirIntegerList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirIntegerListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirIntegerListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirIntegerListEnumerator.GetCurrent : TFhirInteger;
begin
  Result := FList[FIndex];
end;


{ TFhirIntegerList }
procedure TFhirIntegerList.AddItem(value: TFhirInteger);
begin
  assert(value.ClassName = 'TFhirInteger', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirInteger');
  add(value);
end;


procedure TFhirIntegerList.AddItem(value: String);
begin
  add(TFhirInteger.create(value));
end;


function TFhirIntegerList.Append: TFhirInteger;
begin
  result := TFhirInteger.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirIntegerList.ClearItems;
begin
  Clear;
end;

function TFhirIntegerList.GetEnumerator : TFhirIntegerListEnumerator;
begin
  result := TFhirIntegerListEnumerator.Create(self.link);
end;

function TFhirIntegerList.Clone: TFhirIntegerList;
begin
  result := TFhirIntegerList(inherited Clone);
end;

function TFhirIntegerList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirIntegerList.GetItemN(index: Integer): TFhirInteger;
begin
  result := TFhirInteger(ObjectByIndex[index]);
end;

function TFhirIntegerList.ItemClass: TFslObjectClass;
begin
  result := TFhirInteger;
end;
function TFhirIntegerList.IndexOf(value: TFhirInteger): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirIntegerList.Insert(index: Integer): TFhirInteger;
begin
  result := TFhirInteger.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirIntegerList.InsertItem(index: Integer; value: TFhirInteger);
begin
  assert(value is TFhirInteger);
  Inherited Insert(index, value);
end;

function TFhirIntegerList.Item(index: Integer): TFhirInteger;
begin
  result := TFhirInteger(ObjectByIndex[index]);
end;

function TFhirIntegerList.Link: TFhirIntegerList;
begin
  result := TFhirIntegerList(inherited Link);
end;

procedure TFhirIntegerList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirIntegerList.SetItemByIndex(index: Integer; value: TFhirInteger);
begin
  assert(value is TFhirInteger);
  FhirIntegers[index] := value;
end;

procedure TFhirIntegerList.SetItemN(index: Integer; value: TFhirInteger);
begin
  assert(value is TFhirInteger);
  ObjectByIndex[index] := value;
end;

{ TFhirUri }

Constructor TFhirUri.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirUri.Destroy;
begin
  inherited;
end;

function TFhirUri.fhirType : string;
begin
  result := 'uri';
end;

procedure TFhirUri.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirUri.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'uri', false, nil, FValue));
end;

procedure TFhirUri.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirUri(oSource).Value;
end;

function TFhirUri.AsStringValue : string;
begin
  result := FValue;
end;

procedure TFhirUri.SetStringValue(value : string);
begin
  FValue := value;
end;

function TFhirUri.equals(other : TObject) : boolean; 
var
  o : TFhirUri;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirUri)) then
    result := false
  else
  begin
    o := TFhirUri(other);
    result := o.value = value;
  end;
end;

function TFhirUri.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue = '');
end;

function TFhirUri.Link : TFhirUri;
begin
  result := TFhirUri(inherited Link);
end;

function TFhirUri.Clone : TFhirUri;
begin
  result := TFhirUri(inherited Clone);
end;

procedure TFhirUri.setValue(value : String);
begin
  FValue := value;
end;


{ TFhirUriListEnumerator }

Constructor TFhirUriListEnumerator.Create(list : TFhirUriList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirUriListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirUriListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirUriListEnumerator.GetCurrent : TFhirUri;
begin
  Result := FList[FIndex];
end;


{ TFhirUriList }
procedure TFhirUriList.AddItem(value: TFhirUri);
begin
  assert(value.ClassName = 'TFhirUri', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirUri');
  add(value);
end;


procedure TFhirUriList.AddItem(value: String);
begin
  add(TFhirUri.create(value));
end;


function TFhirUriList.Append: TFhirUri;
begin
  result := TFhirUri.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirUriList.ClearItems;
begin
  Clear;
end;

function TFhirUriList.GetEnumerator : TFhirUriListEnumerator;
begin
  result := TFhirUriListEnumerator.Create(self.link);
end;

function TFhirUriList.Clone: TFhirUriList;
begin
  result := TFhirUriList(inherited Clone);
end;

function TFhirUriList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirUriList.GetItemN(index: Integer): TFhirUri;
begin
  result := TFhirUri(ObjectByIndex[index]);
end;

function TFhirUriList.ItemClass: TFslObjectClass;
begin
  result := TFhirUri;
end;
function TFhirUriList.IndexOf(value: TFhirUri): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirUriList.Insert(index: Integer): TFhirUri;
begin
  result := TFhirUri.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirUriList.InsertItem(index: Integer; value: TFhirUri);
begin
  assert(value is TFhirUri);
  Inherited Insert(index, value);
end;

function TFhirUriList.Item(index: Integer): TFhirUri;
begin
  result := TFhirUri(ObjectByIndex[index]);
end;

function TFhirUriList.Link: TFhirUriList;
begin
  result := TFhirUriList(inherited Link);
end;

procedure TFhirUriList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirUriList.SetItemByIndex(index: Integer; value: TFhirUri);
begin
  assert(value is TFhirUri);
  FhirUris[index] := value;
end;

procedure TFhirUriList.SetItemN(index: Integer; value: TFhirUri);
begin
  assert(value is TFhirUri);
  ObjectByIndex[index] := value;
end;

{ TFhirInstant }

Constructor TFhirInstant.Create(value : TFslDateTime);
begin
  Create;
  FValue := value;
end;

destructor TFhirInstant.Destroy;
begin
  inherited;
end;

function TFhirInstant.fhirType : string;
begin
  result := 'instant';
end;

function TFhirInstant.dateValue : TFslDateTime;
begin
  result := FValue;
end;

procedure TFhirInstant.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirInstant.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    if (FValue.notNull) then
      oList.add(TFHIRProperty.create(self, 'value', 'instant', false, nil, FValue.ToString));
end;

procedure TFhirInstant.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirInstant(oSource).Value;
end;

function TFhirInstant.AsStringValue : string;
begin
  if (FValue.null) then
    result := ''
  else
    result := FValue.toXml;
end;

procedure TFhirInstant.SetStringValue(value : string);
begin
  if (value = '') then
    FValue := TFslDateTime.makeNull
  else
    FValue := TFslDateTime.fromXml(value);
end;

function TFhirInstant.equals(other : TObject) : boolean; 
var
  o : TFhirInstant;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirInstant)) then
    result := false
  else
  begin
    o := TFhirInstant(other);
    result := o.value.equal(value);
  end;
end;

function TFhirInstant.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue.null);
end;

function TFhirInstant.Link : TFhirInstant;
begin
  result := TFhirInstant(inherited Link);
end;

function TFhirInstant.Clone : TFhirInstant;
begin
  result := TFhirInstant(inherited Clone);
end;

procedure TFhirInstant.setValue(value : TFslDateTime);
begin
  FValue := value;
end;


{ TFhirInstantListEnumerator }

Constructor TFhirInstantListEnumerator.Create(list : TFhirInstantList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirInstantListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirInstantListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirInstantListEnumerator.GetCurrent : TFhirInstant;
begin
  Result := FList[FIndex];
end;


{ TFhirInstantList }
procedure TFhirInstantList.AddItem(value: TFhirInstant);
begin
  assert(value.ClassName = 'TFhirInstant', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirInstant');
  add(value);
end;


procedure TFhirInstantList.AddItem(value: TFslDateTime);
begin
  add(TFhirInstant.create(value));
end;


function TFhirInstantList.Append: TFhirInstant;
begin
  result := TFhirInstant.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirInstantList.ClearItems;
begin
  Clear;
end;

function TFhirInstantList.GetEnumerator : TFhirInstantListEnumerator;
begin
  result := TFhirInstantListEnumerator.Create(self.link);
end;

function TFhirInstantList.Clone: TFhirInstantList;
begin
  result := TFhirInstantList(inherited Clone);
end;

function TFhirInstantList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirInstantList.GetItemN(index: Integer): TFhirInstant;
begin
  result := TFhirInstant(ObjectByIndex[index]);
end;

function TFhirInstantList.ItemClass: TFslObjectClass;
begin
  result := TFhirInstant;
end;
function TFhirInstantList.IndexOf(value: TFhirInstant): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirInstantList.Insert(index: Integer): TFhirInstant;
begin
  result := TFhirInstant.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirInstantList.InsertItem(index: Integer; value: TFhirInstant);
begin
  assert(value is TFhirInstant);
  Inherited Insert(index, value);
end;

function TFhirInstantList.Item(index: Integer): TFhirInstant;
begin
  result := TFhirInstant(ObjectByIndex[index]);
end;

function TFhirInstantList.Link: TFhirInstantList;
begin
  result := TFhirInstantList(inherited Link);
end;

procedure TFhirInstantList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirInstantList.SetItemByIndex(index: Integer; value: TFhirInstant);
begin
  assert(value is TFhirInstant);
  FhirInstants[index] := value;
end;

procedure TFhirInstantList.SetItemN(index: Integer; value: TFhirInstant);
begin
  assert(value is TFhirInstant);
  ObjectByIndex[index] := value;
end;

{ TFhirXhtml }

Constructor TFhirXhtml.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirXhtml.Destroy;
begin
  inherited;
end;

function TFhirXhtml.fhirType : string;
begin
  result := 'xhtml';
end;

procedure TFhirXhtml.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirXhtml.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'xhtml', false, nil, FValue));
end;

procedure TFhirXhtml.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirXhtml(oSource).Value;
end;

function TFhirXhtml.AsStringValue : string;
begin
  result := FValue;
end;

procedure TFhirXhtml.SetStringValue(value : string);
begin
  FValue := value;
end;

function TFhirXhtml.equals(other : TObject) : boolean; 
var
  o : TFhirXhtml;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirXhtml)) then
    result := false
  else
  begin
    o := TFhirXhtml(other);
    result := o.value = value;
  end;
end;

function TFhirXhtml.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue = '');
end;

function TFhirXhtml.Link : TFhirXhtml;
begin
  result := TFhirXhtml(inherited Link);
end;

function TFhirXhtml.Clone : TFhirXhtml;
begin
  result := TFhirXhtml(inherited Clone);
end;

procedure TFhirXhtml.setValue(value : String);
begin
  FValue := value;
end;


{ TFhirXhtmlListEnumerator }

Constructor TFhirXhtmlListEnumerator.Create(list : TFhirXhtmlList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirXhtmlListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirXhtmlListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirXhtmlListEnumerator.GetCurrent : TFhirXhtml;
begin
  Result := FList[FIndex];
end;


{ TFhirXhtmlList }
procedure TFhirXhtmlList.AddItem(value: TFhirXhtml);
begin
  assert(value.ClassName = 'TFhirXhtml', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirXhtml');
  add(value);
end;


procedure TFhirXhtmlList.AddItem(value: String);
begin
  add(TFhirXhtml.create(value));
end;


function TFhirXhtmlList.Append: TFhirXhtml;
begin
  result := TFhirXhtml.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirXhtmlList.ClearItems;
begin
  Clear;
end;

function TFhirXhtmlList.GetEnumerator : TFhirXhtmlListEnumerator;
begin
  result := TFhirXhtmlListEnumerator.Create(self.link);
end;

function TFhirXhtmlList.Clone: TFhirXhtmlList;
begin
  result := TFhirXhtmlList(inherited Clone);
end;

function TFhirXhtmlList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirXhtmlList.GetItemN(index: Integer): TFhirXhtml;
begin
  result := TFhirXhtml(ObjectByIndex[index]);
end;

function TFhirXhtmlList.ItemClass: TFslObjectClass;
begin
  result := TFhirXhtml;
end;
function TFhirXhtmlList.IndexOf(value: TFhirXhtml): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirXhtmlList.Insert(index: Integer): TFhirXhtml;
begin
  result := TFhirXhtml.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirXhtmlList.InsertItem(index: Integer; value: TFhirXhtml);
begin
  assert(value is TFhirXhtml);
  Inherited Insert(index, value);
end;

function TFhirXhtmlList.Item(index: Integer): TFhirXhtml;
begin
  result := TFhirXhtml(ObjectByIndex[index]);
end;

function TFhirXhtmlList.Link: TFhirXhtmlList;
begin
  result := TFhirXhtmlList(inherited Link);
end;

procedure TFhirXhtmlList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirXhtmlList.SetItemByIndex(index: Integer; value: TFhirXhtml);
begin
  assert(value is TFhirXhtml);
  FhirXhtmls[index] := value;
end;

procedure TFhirXhtmlList.SetItemN(index: Integer; value: TFhirXhtml);
begin
  assert(value is TFhirXhtml);
  ObjectByIndex[index] := value;
end;

{ TFhirBoolean }

Constructor TFhirBoolean.Create(value : Boolean);
begin
  Create;
  FValue := value;
end;

destructor TFhirBoolean.Destroy;
begin
  inherited;
end;

function TFhirBoolean.fhirType : string;
begin
  result := 'boolean';
end;

procedure TFhirBoolean.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirBoolean.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'boolean', false, nil, LCBooleanToString(FValue)));
end;

procedure TFhirBoolean.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirBoolean(oSource).Value;
end;

function TFhirBoolean.AsStringValue : string;
begin
  result := LCBooleanToString(FValue);
end;

procedure TFhirBoolean.SetStringValue(value : string);
begin
  FValue := StringToBoolean(value);
end;

function TFhirBoolean.equals(other : TObject) : boolean; 
var
  o : TFhirBoolean;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirBoolean)) then
    result := false
  else
  begin
    o := TFhirBoolean(other);
    result := o.value = value;
  end;
end;

function TFhirBoolean.isEmpty : boolean;
begin
  result := false;
end;

function TFhirBoolean.Link : TFhirBoolean;
begin
  result := TFhirBoolean(inherited Link);
end;

function TFhirBoolean.Clone : TFhirBoolean;
begin
  result := TFhirBoolean(inherited Clone);
end;

procedure TFhirBoolean.setValue(value : Boolean);
begin
  FValue := value;
end;


{ TFhirBooleanListEnumerator }

Constructor TFhirBooleanListEnumerator.Create(list : TFhirBooleanList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirBooleanListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirBooleanListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirBooleanListEnumerator.GetCurrent : TFhirBoolean;
begin
  Result := FList[FIndex];
end;


{ TFhirBooleanList }
procedure TFhirBooleanList.AddItem(value: TFhirBoolean);
begin
  assert(value.ClassName = 'TFhirBoolean', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirBoolean');
  add(value);
end;


procedure TFhirBooleanList.AddItem(value: Boolean);
begin
  add(TFhirBoolean.create(value));
end;


function TFhirBooleanList.Append: TFhirBoolean;
begin
  result := TFhirBoolean.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirBooleanList.ClearItems;
begin
  Clear;
end;

function TFhirBooleanList.GetEnumerator : TFhirBooleanListEnumerator;
begin
  result := TFhirBooleanListEnumerator.Create(self.link);
end;

function TFhirBooleanList.Clone: TFhirBooleanList;
begin
  result := TFhirBooleanList(inherited Clone);
end;

function TFhirBooleanList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirBooleanList.GetItemN(index: Integer): TFhirBoolean;
begin
  result := TFhirBoolean(ObjectByIndex[index]);
end;

function TFhirBooleanList.ItemClass: TFslObjectClass;
begin
  result := TFhirBoolean;
end;
function TFhirBooleanList.IndexOf(value: TFhirBoolean): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirBooleanList.Insert(index: Integer): TFhirBoolean;
begin
  result := TFhirBoolean.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirBooleanList.InsertItem(index: Integer; value: TFhirBoolean);
begin
  assert(value is TFhirBoolean);
  Inherited Insert(index, value);
end;

function TFhirBooleanList.Item(index: Integer): TFhirBoolean;
begin
  result := TFhirBoolean(ObjectByIndex[index]);
end;

function TFhirBooleanList.Link: TFhirBooleanList;
begin
  result := TFhirBooleanList(inherited Link);
end;

procedure TFhirBooleanList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirBooleanList.SetItemByIndex(index: Integer; value: TFhirBoolean);
begin
  assert(value is TFhirBoolean);
  FhirBooleans[index] := value;
end;

procedure TFhirBooleanList.SetItemN(index: Integer; value: TFhirBoolean);
begin
  assert(value is TFhirBoolean);
  ObjectByIndex[index] := value;
end;

{ TFhirBase64Binary }

Constructor TFhirBase64Binary.Create(value : TBytes);
begin
  Create;
  FValue := value;
end;

destructor TFhirBase64Binary.Destroy;
begin
  inherited;
end;

function TFhirBase64Binary.fhirType : string;
begin
  result := 'base64Binary';
end;

procedure TFhirBase64Binary.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirBase64Binary.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'base64Binary', false, nil, FValue));
end;

procedure TFhirBase64Binary.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirBase64Binary(oSource).Value;
end;

function TFhirBase64Binary.AsStringValue : string;
begin
  if (length(FValue) = 0) then result := '' else result := string(EncodeBase64(FValue));
end;

procedure TFhirBase64Binary.SetStringValue(value : string);
begin
  if (length(value) = 0) then SetLength(FValue, 0) else FValue := DecodeBase64(wideString(value));
end;

function TFhirBase64Binary.equals(other : TObject) : boolean; 
var
  o : TFhirBase64Binary;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirBase64Binary)) then
    result := false
  else
  begin
    o := TFhirBase64Binary(other);
    result := o.value = value;
  end;
end;

function TFhirBase64Binary.isEmpty : boolean;
begin
  result := inherited isEmpty and (length(FValue) = 0);
end;

function TFhirBase64Binary.Link : TFhirBase64Binary;
begin
  result := TFhirBase64Binary(inherited Link);
end;

function TFhirBase64Binary.Clone : TFhirBase64Binary;
begin
  result := TFhirBase64Binary(inherited Clone);
end;

procedure TFhirBase64Binary.setValue(value : TBytes);
begin
  FValue := value;
end;


{ TFhirBase64BinaryListEnumerator }

Constructor TFhirBase64BinaryListEnumerator.Create(list : TFhirBase64BinaryList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirBase64BinaryListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirBase64BinaryListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirBase64BinaryListEnumerator.GetCurrent : TFhirBase64Binary;
begin
  Result := FList[FIndex];
end;


{ TFhirBase64BinaryList }
procedure TFhirBase64BinaryList.AddItem(value: TFhirBase64Binary);
begin
  assert(value.ClassName = 'TFhirBase64Binary', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirBase64Binary');
  add(value);
end;


procedure TFhirBase64BinaryList.AddItem(value: TBytes);
begin
  add(TFhirBase64Binary.create(value));
end;


function TFhirBase64BinaryList.Append: TFhirBase64Binary;
begin
  result := TFhirBase64Binary.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirBase64BinaryList.ClearItems;
begin
  Clear;
end;

function TFhirBase64BinaryList.GetEnumerator : TFhirBase64BinaryListEnumerator;
begin
  result := TFhirBase64BinaryListEnumerator.Create(self.link);
end;

function TFhirBase64BinaryList.Clone: TFhirBase64BinaryList;
begin
  result := TFhirBase64BinaryList(inherited Clone);
end;

function TFhirBase64BinaryList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirBase64BinaryList.GetItemN(index: Integer): TFhirBase64Binary;
begin
  result := TFhirBase64Binary(ObjectByIndex[index]);
end;

function TFhirBase64BinaryList.ItemClass: TFslObjectClass;
begin
  result := TFhirBase64Binary;
end;
function TFhirBase64BinaryList.IndexOf(value: TFhirBase64Binary): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirBase64BinaryList.Insert(index: Integer): TFhirBase64Binary;
begin
  result := TFhirBase64Binary.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirBase64BinaryList.InsertItem(index: Integer; value: TFhirBase64Binary);
begin
  assert(value is TFhirBase64Binary);
  Inherited Insert(index, value);
end;

function TFhirBase64BinaryList.Item(index: Integer): TFhirBase64Binary;
begin
  result := TFhirBase64Binary(ObjectByIndex[index]);
end;

function TFhirBase64BinaryList.Link: TFhirBase64BinaryList;
begin
  result := TFhirBase64BinaryList(inherited Link);
end;

procedure TFhirBase64BinaryList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirBase64BinaryList.SetItemByIndex(index: Integer; value: TFhirBase64Binary);
begin
  assert(value is TFhirBase64Binary);
  FhirBase64Binaries[index] := value;
end;

procedure TFhirBase64BinaryList.SetItemN(index: Integer; value: TFhirBase64Binary);
begin
  assert(value is TFhirBase64Binary);
  ObjectByIndex[index] := value;
end;

{ TFhirTime }

Constructor TFhirTime.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirTime.Destroy;
begin
  inherited;
end;

function TFhirTime.fhirType : string;
begin
  result := 'time';
end;

procedure TFhirTime.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirTime.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'time', false, nil, FValue));
end;

procedure TFhirTime.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirTime(oSource).Value;
end;

function TFhirTime.AsStringValue : string;
begin
  result := FValue;
end;

procedure TFhirTime.SetStringValue(value : string);
begin
  FValue := value;
end;

function TFhirTime.equals(other : TObject) : boolean; 
var
  o : TFhirTime;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirTime)) then
    result := false
  else
  begin
    o := TFhirTime(other);
    result := o.value = value;
  end;
end;

function TFhirTime.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue = '');
end;

function TFhirTime.Link : TFhirTime;
begin
  result := TFhirTime(inherited Link);
end;

function TFhirTime.Clone : TFhirTime;
begin
  result := TFhirTime(inherited Clone);
end;

procedure TFhirTime.setValue(value : String);
begin
  FValue := value;
end;


{ TFhirTimeListEnumerator }

Constructor TFhirTimeListEnumerator.Create(list : TFhirTimeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirTimeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirTimeListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirTimeListEnumerator.GetCurrent : TFhirTime;
begin
  Result := FList[FIndex];
end;


{ TFhirTimeList }
procedure TFhirTimeList.AddItem(value: TFhirTime);
begin
  assert(value.ClassName = 'TFhirTime', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirTime');
  add(value);
end;


procedure TFhirTimeList.AddItem(value: String);
begin
  add(TFhirTime.create(value));
end;


function TFhirTimeList.Append: TFhirTime;
begin
  result := TFhirTime.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirTimeList.ClearItems;
begin
  Clear;
end;

function TFhirTimeList.GetEnumerator : TFhirTimeListEnumerator;
begin
  result := TFhirTimeListEnumerator.Create(self.link);
end;

function TFhirTimeList.Clone: TFhirTimeList;
begin
  result := TFhirTimeList(inherited Clone);
end;

function TFhirTimeList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirTimeList.GetItemN(index: Integer): TFhirTime;
begin
  result := TFhirTime(ObjectByIndex[index]);
end;

function TFhirTimeList.ItemClass: TFslObjectClass;
begin
  result := TFhirTime;
end;
function TFhirTimeList.IndexOf(value: TFhirTime): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirTimeList.Insert(index: Integer): TFhirTime;
begin
  result := TFhirTime.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirTimeList.InsertItem(index: Integer; value: TFhirTime);
begin
  assert(value is TFhirTime);
  Inherited Insert(index, value);
end;

function TFhirTimeList.Item(index: Integer): TFhirTime;
begin
  result := TFhirTime(ObjectByIndex[index]);
end;

function TFhirTimeList.Link: TFhirTimeList;
begin
  result := TFhirTimeList(inherited Link);
end;

procedure TFhirTimeList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirTimeList.SetItemByIndex(index: Integer; value: TFhirTime);
begin
  assert(value is TFhirTime);
  FhirTimes[index] := value;
end;

procedure TFhirTimeList.SetItemN(index: Integer; value: TFhirTime);
begin
  assert(value is TFhirTime);
  ObjectByIndex[index] := value;
end;

{ TFhirDecimal }

Constructor TFhirDecimal.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirDecimal.Destroy;
begin
  inherited;
end;

function TFhirDecimal.fhirType : string;
begin
  result := 'decimal';
end;

procedure TFhirDecimal.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if child_name = 'value' then
    list.add(self.link, 'value', TFHIRObjectText.create(value));
end;

procedure TFhirDecimal.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  if (bPrimitiveValues) then
    oList.add(TFHIRProperty.create(self, 'value', 'decimal', false, nil, FValue));
end;

procedure TFhirDecimal.Assign(oSource : TFslObject);
begin
  inherited;
  FValue := TFhirDecimal(oSource).Value;
end;

function TFhirDecimal.AsStringValue : string;
begin
  result := FValue;
end;

procedure TFhirDecimal.SetStringValue(value : string);
begin
  FValue := value;
end;

function TFhirDecimal.equals(other : TObject) : boolean; 
var
  o : TFhirDecimal;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirDecimal)) then
    result := false
  else
  begin
    o := TFhirDecimal(other);
    result := o.value = value;
  end;
end;

function TFhirDecimal.isEmpty : boolean;
begin
  result := inherited isEmpty and (FValue = '');
end;

function TFhirDecimal.Link : TFhirDecimal;
begin
  result := TFhirDecimal(inherited Link);
end;

function TFhirDecimal.Clone : TFhirDecimal;
begin
  result := TFhirDecimal(inherited Clone);
end;

procedure TFhirDecimal.setValue(value : String);
begin
  FValue := value;
end;


{ TFhirDecimalListEnumerator }

Constructor TFhirDecimalListEnumerator.Create(list : TFhirDecimalList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirDecimalListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirDecimalListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirDecimalListEnumerator.GetCurrent : TFhirDecimal;
begin
  Result := FList[FIndex];
end;


{ TFhirDecimalList }
procedure TFhirDecimalList.AddItem(value: TFhirDecimal);
begin
  assert(value.ClassName = 'TFhirDecimal', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirDecimal');
  add(value);
end;


procedure TFhirDecimalList.AddItem(value: String);
begin
  add(TFhirDecimal.create(value));
end;


function TFhirDecimalList.Append: TFhirDecimal;
begin
  result := TFhirDecimal.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirDecimalList.ClearItems;
begin
  Clear;
end;

function TFhirDecimalList.GetEnumerator : TFhirDecimalListEnumerator;
begin
  result := TFhirDecimalListEnumerator.Create(self.link);
end;

function TFhirDecimalList.Clone: TFhirDecimalList;
begin
  result := TFhirDecimalList(inherited Clone);
end;

function TFhirDecimalList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirDecimalList.GetItemN(index: Integer): TFhirDecimal;
begin
  result := TFhirDecimal(ObjectByIndex[index]);
end;

function TFhirDecimalList.ItemClass: TFslObjectClass;
begin
  result := TFhirDecimal;
end;
function TFhirDecimalList.IndexOf(value: TFhirDecimal): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirDecimalList.Insert(index: Integer): TFhirDecimal;
begin
  result := TFhirDecimal.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirDecimalList.InsertItem(index: Integer; value: TFhirDecimal);
begin
  assert(value is TFhirDecimal);
  Inherited Insert(index, value);
end;

function TFhirDecimalList.Item(index: Integer): TFhirDecimal;
begin
  result := TFhirDecimal(ObjectByIndex[index]);
end;

function TFhirDecimalList.Link: TFhirDecimalList;
begin
  result := TFhirDecimalList(inherited Link);
end;

procedure TFhirDecimalList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirDecimalList.SetItemByIndex(index: Integer; value: TFhirDecimal);
begin
  assert(value is TFhirDecimal);
  FhirDecimals[index] := value;
end;

procedure TFhirDecimalList.SetItemN(index: Integer; value: TFhirDecimal);
begin
  assert(value is TFhirDecimal);
  ObjectByIndex[index] := value;
end;

{ TFhirCode }

Constructor TFhirCode.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirCode.Destroy;
begin
  inherited;
end;

function TFhirCode.fhirType : string;
begin
  result := 'code';
end;

function TFhirCode.Link : TFhirCode;
begin
  result := TFhirCode(inherited Link);
end;

function TFhirCode.Clone : TFhirCode;
begin
  result := TFhirCode(inherited Clone);
end;


{ TFhirCodeListEnumerator }

Constructor TFhirCodeListEnumerator.Create(list : TFhirCodeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirCodeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirCodeListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirCodeListEnumerator.GetCurrent : TFhirCode;
begin
  Result := FList[FIndex];
end;


{ TFhirCodeList }
procedure TFhirCodeList.AddItem(value: TFhirCode);
begin
  assert(value.ClassName = 'TFhirCode', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirCode');
  add(value);
end;


procedure TFhirCodeList.AddItem(value: String);
begin
  add(TFhirCode.create(value));
end;


function TFhirCodeList.Append: TFhirCode;
begin
  result := TFhirCode.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirCodeList.ClearItems;
begin
  Clear;
end;

function TFhirCodeList.GetEnumerator : TFhirCodeListEnumerator;
begin
  result := TFhirCodeListEnumerator.Create(self.link);
end;

function TFhirCodeList.Clone: TFhirCodeList;
begin
  result := TFhirCodeList(inherited Clone);
end;

function TFhirCodeList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirCodeList.GetItemN(index: Integer): TFhirCode;
begin
  result := TFhirCode(ObjectByIndex[index]);
end;

function TFhirCodeList.ItemClass: TFslObjectClass;
begin
  result := TFhirCode;
end;
function TFhirCodeList.IndexOf(value: TFhirCode): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirCodeList.Insert(index: Integer): TFhirCode;
begin
  result := TFhirCode.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirCodeList.InsertItem(index: Integer; value: TFhirCode);
begin
  assert(value is TFhirCode);
  Inherited Insert(index, value);
end;

function TFhirCodeList.Item(index: Integer): TFhirCode;
begin
  result := TFhirCode(ObjectByIndex[index]);
end;

function TFhirCodeList.Link: TFhirCodeList;
begin
  result := TFhirCodeList(inherited Link);
end;

procedure TFhirCodeList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirCodeList.SetItemByIndex(index: Integer; value: TFhirCode);
begin
  assert(value is TFhirCode);
  FhirCodes[index] := value;
end;

procedure TFhirCodeList.SetItemN(index: Integer; value: TFhirCode);
begin
  assert(value is TFhirCode);
  ObjectByIndex[index] := value;
end;

{ TFhirCanonical }

Constructor TFhirCanonical.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirCanonical.Destroy;
begin
  inherited;
end;

function TFhirCanonical.fhirType : string;
begin
  result := 'canonical';
end;

function TFhirCanonical.Link : TFhirCanonical;
begin
  result := TFhirCanonical(inherited Link);
end;

function TFhirCanonical.Clone : TFhirCanonical;
begin
  result := TFhirCanonical(inherited Clone);
end;


{ TFhirCanonicalListEnumerator }

Constructor TFhirCanonicalListEnumerator.Create(list : TFhirCanonicalList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirCanonicalListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirCanonicalListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirCanonicalListEnumerator.GetCurrent : TFhirCanonical;
begin
  Result := FList[FIndex];
end;


{ TFhirCanonicalList }
procedure TFhirCanonicalList.AddItem(value: TFhirCanonical);
begin
  assert(value.ClassName = 'TFhirCanonical', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirCanonical');
  add(value);
end;


procedure TFhirCanonicalList.AddItem(value: String);
begin
  add(TFhirCanonical.create(value));
end;


function TFhirCanonicalList.Append: TFhirCanonical;
begin
  result := TFhirCanonical.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirCanonicalList.ClearItems;
begin
  Clear;
end;

function TFhirCanonicalList.GetEnumerator : TFhirCanonicalListEnumerator;
begin
  result := TFhirCanonicalListEnumerator.Create(self.link);
end;

function TFhirCanonicalList.Clone: TFhirCanonicalList;
begin
  result := TFhirCanonicalList(inherited Clone);
end;

function TFhirCanonicalList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirCanonicalList.GetItemN(index: Integer): TFhirCanonical;
begin
  result := TFhirCanonical(ObjectByIndex[index]);
end;

function TFhirCanonicalList.ItemClass: TFslObjectClass;
begin
  result := TFhirCanonical;
end;
function TFhirCanonicalList.IndexOf(value: TFhirCanonical): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirCanonicalList.Insert(index: Integer): TFhirCanonical;
begin
  result := TFhirCanonical.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirCanonicalList.InsertItem(index: Integer; value: TFhirCanonical);
begin
  assert(value is TFhirCanonical);
  Inherited Insert(index, value);
end;

function TFhirCanonicalList.Item(index: Integer): TFhirCanonical;
begin
  result := TFhirCanonical(ObjectByIndex[index]);
end;

function TFhirCanonicalList.Link: TFhirCanonicalList;
begin
  result := TFhirCanonicalList(inherited Link);
end;

procedure TFhirCanonicalList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirCanonicalList.SetItemByIndex(index: Integer; value: TFhirCanonical);
begin
  assert(value is TFhirCanonical);
  FhirCanonicals[index] := value;
end;

procedure TFhirCanonicalList.SetItemN(index: Integer; value: TFhirCanonical);
begin
  assert(value is TFhirCanonical);
  ObjectByIndex[index] := value;
end;

{ TFhirOid }

Constructor TFhirOid.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirOid.Destroy;
begin
  inherited;
end;

function TFhirOid.fhirType : string;
begin
  result := 'oid';
end;

function TFhirOid.Link : TFhirOid;
begin
  result := TFhirOid(inherited Link);
end;

function TFhirOid.Clone : TFhirOid;
begin
  result := TFhirOid(inherited Clone);
end;


{ TFhirOidListEnumerator }

Constructor TFhirOidListEnumerator.Create(list : TFhirOidList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirOidListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirOidListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirOidListEnumerator.GetCurrent : TFhirOid;
begin
  Result := FList[FIndex];
end;


{ TFhirOidList }
procedure TFhirOidList.AddItem(value: TFhirOid);
begin
  assert(value.ClassName = 'TFhirOid', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirOid');
  add(value);
end;


procedure TFhirOidList.AddItem(value: String);
begin
  add(TFhirOid.create(value));
end;


function TFhirOidList.Append: TFhirOid;
begin
  result := TFhirOid.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirOidList.ClearItems;
begin
  Clear;
end;

function TFhirOidList.GetEnumerator : TFhirOidListEnumerator;
begin
  result := TFhirOidListEnumerator.Create(self.link);
end;

function TFhirOidList.Clone: TFhirOidList;
begin
  result := TFhirOidList(inherited Clone);
end;

function TFhirOidList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirOidList.GetItemN(index: Integer): TFhirOid;
begin
  result := TFhirOid(ObjectByIndex[index]);
end;

function TFhirOidList.ItemClass: TFslObjectClass;
begin
  result := TFhirOid;
end;
function TFhirOidList.IndexOf(value: TFhirOid): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirOidList.Insert(index: Integer): TFhirOid;
begin
  result := TFhirOid.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirOidList.InsertItem(index: Integer; value: TFhirOid);
begin
  assert(value is TFhirOid);
  Inherited Insert(index, value);
end;

function TFhirOidList.Item(index: Integer): TFhirOid;
begin
  result := TFhirOid(ObjectByIndex[index]);
end;

function TFhirOidList.Link: TFhirOidList;
begin
  result := TFhirOidList(inherited Link);
end;

procedure TFhirOidList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirOidList.SetItemByIndex(index: Integer; value: TFhirOid);
begin
  assert(value is TFhirOid);
  FhirOids[index] := value;
end;

procedure TFhirOidList.SetItemN(index: Integer; value: TFhirOid);
begin
  assert(value is TFhirOid);
  ObjectByIndex[index] := value;
end;

{ TFhirUuid }

Constructor TFhirUuid.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirUuid.Destroy;
begin
  inherited;
end;

function TFhirUuid.fhirType : string;
begin
  result := 'uuid';
end;

function TFhirUuid.Link : TFhirUuid;
begin
  result := TFhirUuid(inherited Link);
end;

function TFhirUuid.Clone : TFhirUuid;
begin
  result := TFhirUuid(inherited Clone);
end;


{ TFhirUuidListEnumerator }

Constructor TFhirUuidListEnumerator.Create(list : TFhirUuidList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirUuidListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirUuidListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirUuidListEnumerator.GetCurrent : TFhirUuid;
begin
  Result := FList[FIndex];
end;


{ TFhirUuidList }
procedure TFhirUuidList.AddItem(value: TFhirUuid);
begin
  assert(value.ClassName = 'TFhirUuid', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirUuid');
  add(value);
end;


procedure TFhirUuidList.AddItem(value: String);
begin
  add(TFhirUuid.create(value));
end;


function TFhirUuidList.Append: TFhirUuid;
begin
  result := TFhirUuid.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirUuidList.ClearItems;
begin
  Clear;
end;

function TFhirUuidList.GetEnumerator : TFhirUuidListEnumerator;
begin
  result := TFhirUuidListEnumerator.Create(self.link);
end;

function TFhirUuidList.Clone: TFhirUuidList;
begin
  result := TFhirUuidList(inherited Clone);
end;

function TFhirUuidList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirUuidList.GetItemN(index: Integer): TFhirUuid;
begin
  result := TFhirUuid(ObjectByIndex[index]);
end;

function TFhirUuidList.ItemClass: TFslObjectClass;
begin
  result := TFhirUuid;
end;
function TFhirUuidList.IndexOf(value: TFhirUuid): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirUuidList.Insert(index: Integer): TFhirUuid;
begin
  result := TFhirUuid.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirUuidList.InsertItem(index: Integer; value: TFhirUuid);
begin
  assert(value is TFhirUuid);
  Inherited Insert(index, value);
end;

function TFhirUuidList.Item(index: Integer): TFhirUuid;
begin
  result := TFhirUuid(ObjectByIndex[index]);
end;

function TFhirUuidList.Link: TFhirUuidList;
begin
  result := TFhirUuidList(inherited Link);
end;

procedure TFhirUuidList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirUuidList.SetItemByIndex(index: Integer; value: TFhirUuid);
begin
  assert(value is TFhirUuid);
  FhirUuids[index] := value;
end;

procedure TFhirUuidList.SetItemN(index: Integer; value: TFhirUuid);
begin
  assert(value is TFhirUuid);
  ObjectByIndex[index] := value;
end;

{ TFhirUrl }

Constructor TFhirUrl.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirUrl.Destroy;
begin
  inherited;
end;

function TFhirUrl.fhirType : string;
begin
  result := 'url';
end;

function TFhirUrl.Link : TFhirUrl;
begin
  result := TFhirUrl(inherited Link);
end;

function TFhirUrl.Clone : TFhirUrl;
begin
  result := TFhirUrl(inherited Clone);
end;


{ TFhirUrlListEnumerator }

Constructor TFhirUrlListEnumerator.Create(list : TFhirUrlList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirUrlListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirUrlListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirUrlListEnumerator.GetCurrent : TFhirUrl;
begin
  Result := FList[FIndex];
end;


{ TFhirUrlList }
procedure TFhirUrlList.AddItem(value: TFhirUrl);
begin
  assert(value.ClassName = 'TFhirUrl', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirUrl');
  add(value);
end;


procedure TFhirUrlList.AddItem(value: String);
begin
  add(TFhirUrl.create(value));
end;


function TFhirUrlList.Append: TFhirUrl;
begin
  result := TFhirUrl.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirUrlList.ClearItems;
begin
  Clear;
end;

function TFhirUrlList.GetEnumerator : TFhirUrlListEnumerator;
begin
  result := TFhirUrlListEnumerator.Create(self.link);
end;

function TFhirUrlList.Clone: TFhirUrlList;
begin
  result := TFhirUrlList(inherited Clone);
end;

function TFhirUrlList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirUrlList.GetItemN(index: Integer): TFhirUrl;
begin
  result := TFhirUrl(ObjectByIndex[index]);
end;

function TFhirUrlList.ItemClass: TFslObjectClass;
begin
  result := TFhirUrl;
end;
function TFhirUrlList.IndexOf(value: TFhirUrl): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirUrlList.Insert(index: Integer): TFhirUrl;
begin
  result := TFhirUrl.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirUrlList.InsertItem(index: Integer; value: TFhirUrl);
begin
  assert(value is TFhirUrl);
  Inherited Insert(index, value);
end;

function TFhirUrlList.Item(index: Integer): TFhirUrl;
begin
  result := TFhirUrl(ObjectByIndex[index]);
end;

function TFhirUrlList.Link: TFhirUrlList;
begin
  result := TFhirUrlList(inherited Link);
end;

procedure TFhirUrlList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirUrlList.SetItemByIndex(index: Integer; value: TFhirUrl);
begin
  assert(value is TFhirUrl);
  FhirUrls[index] := value;
end;

procedure TFhirUrlList.SetItemN(index: Integer; value: TFhirUrl);
begin
  assert(value is TFhirUrl);
  ObjectByIndex[index] := value;
end;

{ TFhirMarkdown }

Constructor TFhirMarkdown.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirMarkdown.Destroy;
begin
  inherited;
end;

function TFhirMarkdown.fhirType : string;
begin
  result := 'markdown';
end;

function TFhirMarkdown.Link : TFhirMarkdown;
begin
  result := TFhirMarkdown(inherited Link);
end;

function TFhirMarkdown.Clone : TFhirMarkdown;
begin
  result := TFhirMarkdown(inherited Clone);
end;


{ TFhirMarkdownListEnumerator }

Constructor TFhirMarkdownListEnumerator.Create(list : TFhirMarkdownList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirMarkdownListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirMarkdownListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirMarkdownListEnumerator.GetCurrent : TFhirMarkdown;
begin
  Result := FList[FIndex];
end;


{ TFhirMarkdownList }
procedure TFhirMarkdownList.AddItem(value: TFhirMarkdown);
begin
  assert(value.ClassName = 'TFhirMarkdown', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirMarkdown');
  add(value);
end;


procedure TFhirMarkdownList.AddItem(value: String);
begin
  add(TFhirMarkdown.create(value));
end;


function TFhirMarkdownList.Append: TFhirMarkdown;
begin
  result := TFhirMarkdown.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirMarkdownList.ClearItems;
begin
  Clear;
end;

function TFhirMarkdownList.GetEnumerator : TFhirMarkdownListEnumerator;
begin
  result := TFhirMarkdownListEnumerator.Create(self.link);
end;

function TFhirMarkdownList.Clone: TFhirMarkdownList;
begin
  result := TFhirMarkdownList(inherited Clone);
end;

function TFhirMarkdownList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirMarkdownList.GetItemN(index: Integer): TFhirMarkdown;
begin
  result := TFhirMarkdown(ObjectByIndex[index]);
end;

function TFhirMarkdownList.ItemClass: TFslObjectClass;
begin
  result := TFhirMarkdown;
end;
function TFhirMarkdownList.IndexOf(value: TFhirMarkdown): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirMarkdownList.Insert(index: Integer): TFhirMarkdown;
begin
  result := TFhirMarkdown.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirMarkdownList.InsertItem(index: Integer; value: TFhirMarkdown);
begin
  assert(value is TFhirMarkdown);
  Inherited Insert(index, value);
end;

function TFhirMarkdownList.Item(index: Integer): TFhirMarkdown;
begin
  result := TFhirMarkdown(ObjectByIndex[index]);
end;

function TFhirMarkdownList.Link: TFhirMarkdownList;
begin
  result := TFhirMarkdownList(inherited Link);
end;

procedure TFhirMarkdownList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirMarkdownList.SetItemByIndex(index: Integer; value: TFhirMarkdown);
begin
  assert(value is TFhirMarkdown);
  FhirMarkdowns[index] := value;
end;

procedure TFhirMarkdownList.SetItemN(index: Integer; value: TFhirMarkdown);
begin
  assert(value is TFhirMarkdown);
  ObjectByIndex[index] := value;
end;

{ TFhirUnsignedInt }

Constructor TFhirUnsignedInt.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirUnsignedInt.Destroy;
begin
  inherited;
end;

function TFhirUnsignedInt.fhirType : string;
begin
  result := 'unsignedInt';
end;

function TFhirUnsignedInt.Link : TFhirUnsignedInt;
begin
  result := TFhirUnsignedInt(inherited Link);
end;

function TFhirUnsignedInt.Clone : TFhirUnsignedInt;
begin
  result := TFhirUnsignedInt(inherited Clone);
end;


{ TFhirUnsignedIntListEnumerator }

Constructor TFhirUnsignedIntListEnumerator.Create(list : TFhirUnsignedIntList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirUnsignedIntListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirUnsignedIntListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirUnsignedIntListEnumerator.GetCurrent : TFhirUnsignedInt;
begin
  Result := FList[FIndex];
end;


{ TFhirUnsignedIntList }
procedure TFhirUnsignedIntList.AddItem(value: TFhirUnsignedInt);
begin
  assert(value.ClassName = 'TFhirUnsignedInt', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirUnsignedInt');
  add(value);
end;


procedure TFhirUnsignedIntList.AddItem(value: String);
begin
  add(TFhirUnsignedInt.create(value));
end;


function TFhirUnsignedIntList.Append: TFhirUnsignedInt;
begin
  result := TFhirUnsignedInt.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirUnsignedIntList.ClearItems;
begin
  Clear;
end;

function TFhirUnsignedIntList.GetEnumerator : TFhirUnsignedIntListEnumerator;
begin
  result := TFhirUnsignedIntListEnumerator.Create(self.link);
end;

function TFhirUnsignedIntList.Clone: TFhirUnsignedIntList;
begin
  result := TFhirUnsignedIntList(inherited Clone);
end;

function TFhirUnsignedIntList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirUnsignedIntList.GetItemN(index: Integer): TFhirUnsignedInt;
begin
  result := TFhirUnsignedInt(ObjectByIndex[index]);
end;

function TFhirUnsignedIntList.ItemClass: TFslObjectClass;
begin
  result := TFhirUnsignedInt;
end;
function TFhirUnsignedIntList.IndexOf(value: TFhirUnsignedInt): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirUnsignedIntList.Insert(index: Integer): TFhirUnsignedInt;
begin
  result := TFhirUnsignedInt.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirUnsignedIntList.InsertItem(index: Integer; value: TFhirUnsignedInt);
begin
  assert(value is TFhirUnsignedInt);
  Inherited Insert(index, value);
end;

function TFhirUnsignedIntList.Item(index: Integer): TFhirUnsignedInt;
begin
  result := TFhirUnsignedInt(ObjectByIndex[index]);
end;

function TFhirUnsignedIntList.Link: TFhirUnsignedIntList;
begin
  result := TFhirUnsignedIntList(inherited Link);
end;

procedure TFhirUnsignedIntList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirUnsignedIntList.SetItemByIndex(index: Integer; value: TFhirUnsignedInt);
begin
  assert(value is TFhirUnsignedInt);
  FhirUnsignedInts[index] := value;
end;

procedure TFhirUnsignedIntList.SetItemN(index: Integer; value: TFhirUnsignedInt);
begin
  assert(value is TFhirUnsignedInt);
  ObjectByIndex[index] := value;
end;

{ TFhirId }

Constructor TFhirId.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirId.Destroy;
begin
  inherited;
end;

function TFhirId.fhirType : string;
begin
  result := 'id';
end;

function TFhirId.Link : TFhirId;
begin
  result := TFhirId(inherited Link);
end;

function TFhirId.Clone : TFhirId;
begin
  result := TFhirId(inherited Clone);
end;


{ TFhirIdListEnumerator }

Constructor TFhirIdListEnumerator.Create(list : TFhirIdList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirIdListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirIdListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirIdListEnumerator.GetCurrent : TFhirId;
begin
  Result := FList[FIndex];
end;


{ TFhirIdList }
procedure TFhirIdList.AddItem(value: TFhirId);
begin
  assert(value.ClassName = 'TFhirId', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirId');
  add(value);
end;


procedure TFhirIdList.AddItem(value: String);
begin
  add(TFhirId.create(value));
end;


function TFhirIdList.Append: TFhirId;
begin
  result := TFhirId.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirIdList.ClearItems;
begin
  Clear;
end;

function TFhirIdList.GetEnumerator : TFhirIdListEnumerator;
begin
  result := TFhirIdListEnumerator.Create(self.link);
end;

function TFhirIdList.Clone: TFhirIdList;
begin
  result := TFhirIdList(inherited Clone);
end;

function TFhirIdList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirIdList.GetItemN(index: Integer): TFhirId;
begin
  result := TFhirId(ObjectByIndex[index]);
end;

function TFhirIdList.ItemClass: TFslObjectClass;
begin
  result := TFhirId;
end;
function TFhirIdList.IndexOf(value: TFhirId): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirIdList.Insert(index: Integer): TFhirId;
begin
  result := TFhirId.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirIdList.InsertItem(index: Integer; value: TFhirId);
begin
  assert(value is TFhirId);
  Inherited Insert(index, value);
end;

function TFhirIdList.Item(index: Integer): TFhirId;
begin
  result := TFhirId(ObjectByIndex[index]);
end;

function TFhirIdList.Link: TFhirIdList;
begin
  result := TFhirIdList(inherited Link);
end;

procedure TFhirIdList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirIdList.SetItemByIndex(index: Integer; value: TFhirId);
begin
  assert(value is TFhirId);
  FhirIds[index] := value;
end;

procedure TFhirIdList.SetItemN(index: Integer; value: TFhirId);
begin
  assert(value is TFhirId);
  ObjectByIndex[index] := value;
end;

{ TFhirPositiveInt }

Constructor TFhirPositiveInt.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirPositiveInt.Destroy;
begin
  inherited;
end;

function TFhirPositiveInt.fhirType : string;
begin
  result := 'positiveInt';
end;

function TFhirPositiveInt.Link : TFhirPositiveInt;
begin
  result := TFhirPositiveInt(inherited Link);
end;

function TFhirPositiveInt.Clone : TFhirPositiveInt;
begin
  result := TFhirPositiveInt(inherited Clone);
end;


{ TFhirPositiveIntListEnumerator }

Constructor TFhirPositiveIntListEnumerator.Create(list : TFhirPositiveIntList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirPositiveIntListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirPositiveIntListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirPositiveIntListEnumerator.GetCurrent : TFhirPositiveInt;
begin
  Result := FList[FIndex];
end;


{ TFhirPositiveIntList }
procedure TFhirPositiveIntList.AddItem(value: TFhirPositiveInt);
begin
  assert(value.ClassName = 'TFhirPositiveInt', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirPositiveInt');
  add(value);
end;


procedure TFhirPositiveIntList.AddItem(value: String);
begin
  add(TFhirPositiveInt.create(value));
end;


function TFhirPositiveIntList.Append: TFhirPositiveInt;
begin
  result := TFhirPositiveInt.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirPositiveIntList.ClearItems;
begin
  Clear;
end;

function TFhirPositiveIntList.GetEnumerator : TFhirPositiveIntListEnumerator;
begin
  result := TFhirPositiveIntListEnumerator.Create(self.link);
end;

function TFhirPositiveIntList.Clone: TFhirPositiveIntList;
begin
  result := TFhirPositiveIntList(inherited Clone);
end;

function TFhirPositiveIntList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirPositiveIntList.GetItemN(index: Integer): TFhirPositiveInt;
begin
  result := TFhirPositiveInt(ObjectByIndex[index]);
end;

function TFhirPositiveIntList.ItemClass: TFslObjectClass;
begin
  result := TFhirPositiveInt;
end;
function TFhirPositiveIntList.IndexOf(value: TFhirPositiveInt): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirPositiveIntList.Insert(index: Integer): TFhirPositiveInt;
begin
  result := TFhirPositiveInt.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirPositiveIntList.InsertItem(index: Integer; value: TFhirPositiveInt);
begin
  assert(value is TFhirPositiveInt);
  Inherited Insert(index, value);
end;

function TFhirPositiveIntList.Item(index: Integer): TFhirPositiveInt;
begin
  result := TFhirPositiveInt(ObjectByIndex[index]);
end;

function TFhirPositiveIntList.Link: TFhirPositiveIntList;
begin
  result := TFhirPositiveIntList(inherited Link);
end;

procedure TFhirPositiveIntList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirPositiveIntList.SetItemByIndex(index: Integer; value: TFhirPositiveInt);
begin
  assert(value is TFhirPositiveInt);
  FhirPositiveInts[index] := value;
end;

procedure TFhirPositiveIntList.SetItemN(index: Integer; value: TFhirPositiveInt);
begin
  assert(value is TFhirPositiveInt);
  ObjectByIndex[index] := value;
end;

{ TFhirInteger64 }

Constructor TFhirInteger64.Create(value : String);
begin
  Create;
  FValue := value;
end;

destructor TFhirInteger64.Destroy;
begin
  inherited;
end;

function TFhirInteger64.fhirType : string;
begin
  result := 'Integer64';
end;

function TFhirInteger64.Link : TFhirInteger64;
begin
  result := TFhirInteger64(inherited Link);
end;

function TFhirInteger64.Clone : TFhirInteger64;
begin
  result := TFhirInteger64(inherited Clone);
end;


{ TFhirInteger64ListEnumerator }

Constructor TFhirInteger64ListEnumerator.Create(list : TFhirInteger64List);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirInteger64ListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirInteger64ListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirInteger64ListEnumerator.GetCurrent : TFhirInteger64;
begin
  Result := FList[FIndex];
end;


{ TFhirInteger64List }
procedure TFhirInteger64List.AddItem(value: TFhirInteger64);
begin
  assert(value.ClassName = 'TFhirInteger64', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirInteger64');
  add(value);
end;


procedure TFhirInteger64List.AddItem(value: String);
begin
  add(TFhirInteger64.create(value));
end;


function TFhirInteger64List.Append: TFhirInteger64;
begin
  result := TFhirInteger64.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirInteger64List.ClearItems;
begin
  Clear;
end;

function TFhirInteger64List.GetEnumerator : TFhirInteger64ListEnumerator;
begin
  result := TFhirInteger64ListEnumerator.Create(self.link);
end;

function TFhirInteger64List.Clone: TFhirInteger64List;
begin
  result := TFhirInteger64List(inherited Clone);
end;

function TFhirInteger64List.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirInteger64List.GetItemN(index: Integer): TFhirInteger64;
begin
  result := TFhirInteger64(ObjectByIndex[index]);
end;

function TFhirInteger64List.ItemClass: TFslObjectClass;
begin
  result := TFhirInteger64;
end;
function TFhirInteger64List.IndexOf(value: TFhirInteger64): Integer;
begin
  result := IndexByReference(value);
end;


function TFhirInteger64List.Insert(index: Integer): TFhirInteger64;
begin
  result := TFhirInteger64.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;


procedure TFhirInteger64List.InsertItem(index: Integer; value: TFhirInteger64);
begin
  assert(value is TFhirInteger64);
  Inherited Insert(index, value);
end;

function TFhirInteger64List.Item(index: Integer): TFhirInteger64;
begin
  result := TFhirInteger64(ObjectByIndex[index]);
end;

function TFhirInteger64List.Link: TFhirInteger64List;
begin
  result := TFhirInteger64List(inherited Link);
end;

procedure TFhirInteger64List.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirInteger64List.SetItemByIndex(index: Integer; value: TFhirInteger64);
begin
  assert(value is TFhirInteger64);
  FhirInteger64s[index] := value;
end;

procedure TFhirInteger64List.SetItemN(index: Integer; value: TFhirInteger64);
begin
  assert(value is TFhirInteger64);
  ObjectByIndex[index] := value;
end;

{{type.concrete.impl}}

function asEnum(systems, values: array of String; obj : TFHIRObject) : TFHIREnum;
begin
  if obj is TFHIREnum then
    result := obj as TFHIREnum
  else if obj is TFHIRCode then
  begin
    result := TFHIREnum.create(systems[StringArrayIndexOf(values, TFHIRCode(obj).value)], TFHIRCode(obj).value);
    obj.Free;
  end
  else if obj is TFHIRString then
  begin
    result := TFHIREnum.create(systems[StringArrayIndexOf(values, TFHIRString(obj).value)], TFHIRString(obj).value);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRCode"')
  end;
end;

function asDate(obj : TFHIRObject) : TFHIRDate;
begin
  if obj is TFHIRDate then
    result := obj as TFHIRDate
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRDate.create(TFslDateTime.fromXml(TFHIRMMElement(obj).value));
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRDate.create(TFslDateTime.fromXml(TFHIRObject(obj).primitiveValue));
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRDate"')
  end;
end;

function asDateTime(obj : TFHIRObject) : TFHIRDateTime;
begin
  if obj is TFHIRDateTime then
    result := obj as TFHIRDateTime
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRDateTime.create(TFslDateTime.fromXml(TFHIRMMElement(obj).value));
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRDateTime.create(TFslDateTime.fromXml(TFHIRObject(obj).primitiveValue));
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRDateTime"')
  end;
end;

function asString(obj : TFHIRObject) : TFHIRString;
begin
  if obj is TFHIRString then
    result := obj as TFHIRString
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRString.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRString.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRString"')
  end;
end;

function asInteger(obj : TFHIRObject) : TFHIRInteger;
begin
  if obj is TFHIRInteger then
    result := obj as TFHIRInteger
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRInteger.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRInteger.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRInteger"')
  end;
end;

function asUri(obj : TFHIRObject) : TFHIRUri;
begin
  if obj is TFHIRUri then
    result := obj as TFHIRUri
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRUri.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRUri.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRUri"')
  end;
end;

function asInstant(obj : TFHIRObject) : TFHIRInstant;
begin
  if obj is TFHIRInstant then
    result := obj as TFHIRInstant
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRInstant.create(TFslDateTime.fromXml(TFHIRMMElement(obj).value));
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRInstant.create(TFslDateTime.fromXml(TFHIRObject(obj).primitiveValue));
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRInstant"')
  end;
end;

function asXhtml(obj : TFHIRObject) : TFHIRXhtml;
begin
  if obj is TFHIRXhtml then
    result := obj as TFHIRXhtml
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRXhtml.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRXhtml.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRXhtml"')
  end;
end;

function asBoolean(obj : TFHIRObject) : TFHIRBoolean;
begin
  if obj is TFHIRBoolean then
    result := obj as TFHIRBoolean
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRBoolean.create(TFHIRMMElement(obj).value = 'true');
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRBoolean.create(TFHIRObject(obj).primitiveValue = 'true');
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRBoolean"')
  end;
end;

function asBase64Binary(obj : TFHIRObject) : TFHIRBase64Binary;
begin
  if obj is TFHIRBase64Binary then
    result := obj as TFHIRBase64Binary
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRBase64Binary.create(DecodeBase64(TFHIRMMElement(obj).value));
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRBase64Binary.create(DecodeBase64(TFHIRObject(obj).primitiveValue));
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRBase64Binary"')
  end;
end;

function asTime(obj : TFHIRObject) : TFHIRTime;
begin
  if obj is TFHIRTime then
    result := obj as TFHIRTime
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRTime.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRTime.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRTime"')
  end;
end;

function asDecimal(obj : TFHIRObject) : TFHIRDecimal;
begin
  if obj is TFHIRDecimal then
    result := obj as TFHIRDecimal
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRDecimal.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRDecimal.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRDecimal"')
  end;
end;

function asCode(obj : TFHIRObject) : TFHIRCode;
begin
  if obj is TFHIRCode then
    result := obj as TFHIRCode
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRCode.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRCode.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRCode"')
  end;
end;

function asCanonical(obj : TFHIRObject) : TFHIRCanonical;
begin
  if obj is TFHIRCanonical then
    result := obj as TFHIRCanonical
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRCanonical.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRCanonical.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRCanonical"')
  end;
end;

function asOid(obj : TFHIRObject) : TFHIROid;
begin
  if obj is TFHIROid then
    result := obj as TFHIROid
  else if obj is TFHIRMMElement then
  begin
    result := TFHIROid.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIROid.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIROid"')
  end;
end;

function asUuid(obj : TFHIRObject) : TFHIRUuid;
begin
  if obj is TFHIRUuid then
    result := obj as TFHIRUuid
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRUuid.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRUuid.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRUuid"')
  end;
end;

function asUrl(obj : TFHIRObject) : TFHIRUrl;
begin
  if obj is TFHIRUrl then
    result := obj as TFHIRUrl
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRUrl.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRUrl.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRUrl"')
  end;
end;

function asMarkdown(obj : TFHIRObject) : TFHIRMarkdown;
begin
  if obj is TFHIRMarkdown then
    result := obj as TFHIRMarkdown
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRMarkdown.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRMarkdown.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRMarkdown"')
  end;
end;

function asUnsignedInt(obj : TFHIRObject) : TFHIRUnsignedInt;
begin
  if obj is TFHIRUnsignedInt then
    result := obj as TFHIRUnsignedInt
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRUnsignedInt.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRUnsignedInt.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRUnsignedInt"')
  end;
end;

function asId(obj : TFHIRObject) : TFHIRId;
begin
  if obj is TFHIRId then
    result := obj as TFHIRId
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRId.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRId.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRId"')
  end;
end;

function asPositiveInt(obj : TFHIRObject) : TFHIRPositiveInt;
begin
  if obj is TFHIRPositiveInt then
    result := obj as TFHIRPositiveInt
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRPositiveInt.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRPositiveInt.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRPositiveInt"')
  end;
end;


function asInteger64(obj : TFHIRObject) : TFHIRInteger64;
begin
  if obj is TFHIRInteger64 then
    result := obj as TFHIRInteger64
  else if obj is TFHIRMMElement then
  begin
    result := TFHIRInteger64.create(TFHIRMMElement(obj).value);
    obj.Free;
  end
  else if (obj is TFHIRObject) and (TFHIRObject(obj).isPrimitive) then
  begin
    result := TFHIRInteger64.create(TFHIRObject(obj).primitiveValue);
    obj.Free;
  end
  else
  begin
    obj.Free;
    raise EFhirException.Create('Type mismatch: cannot convert from "'+obj.className+'" to "TFHIRInteger64"')
  end;
end;

end.

