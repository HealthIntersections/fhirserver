unit cda_base;

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

Uses
  SysUtils,
  fsl_base, fsl_utilities, fsl_xml, fsl_stream, fsl_collections;

Type
  ECDAException = class (EXmlException);

  TRMPropertyDefinitionType = (
      rmpdtBoolean,
      rmpdtBinary,
      rmpdtInt,
      rmpdtString,
      rmpdtDecimal,
      rmpdtDateTime,
      rmpdtEnum,
      rmpdtClass);

  TRMPropertyCollectionType = (
      rmpctNone,
      rmpctSet,
      rmpctList,
      rmpctBag);

  Tv3Base = class;
  Tv3BaseList = class;


  Tv3PropertyDefinition = class (TFslObject)
  Private
    FName : String;
    FOwner : Tv3Base;
    FValueType : TRMPropertyDefinitionType;
    FCollectionState : TRMPropertyCollectionType;
    FPossibles : TFslStringList;
    FClassName : String;
    FIsStructural : Boolean;

    FValueBase : Tv3Base;
    FValueCollection : Tv3BaseList;
    FValueString : WideString;
    FValueStrings : TFslStringList;

    function GetAsEnum: Integer;
    function GetAsString: String;
    procedure SetAsEnum(const iValue: Integer);
    procedure SetAsString(const sValue: String);
  Public
    constructor CreateClass(oOwner : Tv3Base; bIsStructural : Boolean; Const sName : String; oObject : Tv3Base; sClass : String); Overload;
    constructor CreateClass(oOwner : Tv3Base; bIsStructural : Boolean; Const sName : String; oList : Tv3BaseList; aCollectionState : TRMPropertyCollectionType; sClass : String); Overload;
    constructor CreateString(oOwner : Tv3Base; bIsStructural : Boolean; Const sName, sValue : String);
    constructor CreateStrings(oOwner : Tv3Base; bIsStructural : Boolean; Const sName : String; oList : TFslStringList);
    constructor CreateBoolean(oOwner : Tv3Base; bIsStructural : Boolean; Const sName : String; bHasValue, bValue : Boolean);
    constructor CreateBinary(oOwner : Tv3Base; bIsStructural : Boolean; Const sName : String; oBuffer : TFslBuffer);
    constructor CreateInteger(oOwner : Tv3Base; bIsStructural : Boolean; Const sName : String; bHasValue : Boolean; iValue : int64);
    constructor CreateDecimal(oOwner : Tv3Base; bIsStructural : Boolean; Const sName : String; aValue : TFslDecimal);
    constructor CreateEnum(oOwner : Tv3Base; bIsStructural : Boolean; Const sName : String; Const iValue : Integer; Const aPossibles : Array of String);
    constructor CreateSet(oOwner : Tv3Base; bIsStructural : Boolean; Const sName : String; var aSet; Const aPossibles : Array of String);

    destructor Destroy; Override;
    Property Name : String read FName;
    Property PropertyType : TRMPropertyDefinitionType Read FValueType;
    Property CollectionState : TRMPropertyCollectionType Read FCollectionState;
    Property IsStructural : Boolean read FIsStructural;
    Property PossibleValues : TFslStringList read FPossibles;

    Function RIMClassName(bAsCollection : Boolean) : String;
    Function CDAClassName(bAsCollection : Boolean) : String;

    Function HasValue : Boolean;

    Function AsType(aType : TFslObjectClass) : TFslObject;
    Procedure AsBool(var bHasValue, bValue : Boolean);
    Procedure AsInt(var bHasValue : Boolean; var iValue : int64);
    Property AsString : String Read GetAsString Write SetAsString;
    Property AsEnum : Integer Read GetAsEnum Write SetAsEnum;

    Procedure SetType(oObject : Tv3Base); Overload;
    Procedure SetType(oList : Tv3BaseList); Overload;
    Procedure SetSet(var aSet);
    Procedure AsSet(var aSet);
  End;

  Tv3PropertyDefinitionList = class (TFslObjectList)
  private
    Function GetProperty(iIndex : Integer) : Tv3PropertyDefinition;
  public
    Property Properties[iIndex : Integer] : Tv3PropertyDefinition read GetProperty; default;
  End;

  Tv3DataTypePropertyIterator = class (TFslObject)
  private
    FFocus : Tv3Base;
    FProperties : Tv3PropertyDefinitionList;
    FCursor : Integer;
    Function GetCurrent : Tv3PropertyDefinition;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(oFocus : Tv3Base; bInheritedProperties : Boolean);
    destructor Destroy; Override;
    Procedure Next;
    Procedure Reset;
    Function More : Boolean;
    Property Current : Tv3PropertyDefinition read GetCurrent;
  End;

  Tv3ExtensionList = class;



  {
    Enumeration of the general type that a CDA class can have
  }
  TCDAClassType = (
    itNull, etAct, etSpacer, etParticipation, etRole, etEntity, etActRel, etDatatype, etNarrative, etExtension
  );

  {
    base class for all CDA types
  }
  Tv3Base = class (TFslObject)
  Private
    Fcomments : TFslStringList;
    FElement : TMXmlElement;
    FPath : String;
    FExtensions : Tv3ExtensionList;
    FTag : TFslObject;
    FTagId: string;
    FParent : Tv3Base; // Not linked
    function Getcomments: TFslStringList;
    function GetExtensions : Tv3ExtensionList;
    procedure SetTag(const Value: TFslObject);
  Protected
{    Function BuildSet(var aSet; Const aCodes : Array of String) : String;
    Procedure ReadSet(var aSet; Const sValues : TWideStringList; Const aCodes : Array of String);
    function ReadEnum(const sValue: String; const aNames: array of String): Integer;
    procedure ReadBoolean(const sValue: String; var bHasValue, bValue : Boolean);
    procedure ReadInteger(const sValue: String; var bHasValue : Boolean; var bValue : Int64);
    Function CheckType(aValue : Tv3Base; aType : Tv3Datatype): Tv3Base; Overload;
    Function CheckType(aValue : Tv3BaseCollection; aType : Tv3BaseCollectionType): Tv3BaseCollection; Overload;
}
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Virtual;
    Procedure SetPropertyValue(Const aValue :Tv3PropertyDefinition); Virtual;
    Procedure DoClear; Virtual;
    Function RIMClassNameV: String; Virtual;
    Function CDAClassNameV: String; Virtual;
    Function CDAClassTypeV: TCDAClassType; Virtual;
    function sizeInBytesV : cardinal; override;
  public

    sourcelocation : TSourceLocation;
    sourcelocationEnd : TSourceLocation;

    constructor Create; Override;
    destructor Destroy; Override;

    function Link : Tv3Base; Overload;
    function Clone(parent : Tv3Base) : Tv3Base; Overload;
    procedure Assign(oSource : TFslObject); Override;

    Property Element : TMXmlElement read FElement write FElement;
    Function EqualsV(oOther : Tv3Base) : Boolean; Virtual;
    Property Path : String read FPath write FPath;


    {
      The name of the type in the RIM. For data types and structured types, RIMClassName = CDAClassName
    }
    Function RIMClassName: String;
    {
      The name of the type in the CDA specification
    }
    Function CDAClassName: String;
    {
      The name of the type in the CDA specification
    }
    Function CDAClassType: TCDAClassType;

    function createIterator(bInheritedProperties : Boolean) : Tv3DataTypePropertyIterator;
    Function HasComments : boolean;

    property tag : TFslObject read FTag write SetTag;
    property tagId : string read FTagId write FTagId;

    {
      XML comments associated with this element
    }
    Property comments : TFslStringList read Getcomments;


    {
      additional extensions that are children of this item
    }
    Property extensions : Tv3ExtensionList read GetExtensions;

    {
      if this is equal to other according the CDA/Data types Specification.
      CDA (RIM) classes are only equal if they are the same instance.
      Narrative is equal if their content model is the same
      Data type equality is evaluated according to the rules in ISO 21090.
    }
    Function Equals(oOther : TObject) : Boolean; override;

    {
      Reset all the properties to their default values
    }
    Procedure Clear;

    {
      Link to the parent; not always populated
    }
    Property Parent : Tv3Base read FParent write FParent;
  End;

  Tv3BaseClass = class of Tv3Base;

  {
    base class for all CDA list types
  }
  Tv3BaseList = class (TFslObjectList)
  private
    FParent: Tv3Base;
    Function GetBase(iIndex : Integer) : Tv3Base;
    procedure SetBases(iIndex: Integer; const Value: Tv3Base);
  protected
    Procedure InternalAfterInclude(iIndex : Integer; oObject : TFslObject); Override;
  public
    constructor Create(Parent : Tv3Base);

    Function Link : Tv3BaseList; Overload;
    Function Clone(parent : Tv3Base) : Tv3BaseList; Overload;

    Property Base[iIndex : Integer] : Tv3Base read GetBase write SetBases; default;

    {
      Add a ANY to the end of the list
    }
    function Append : Tv3Base;
    {
      Insert a existing ANY before the specified iten (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : Tv3Base);
    {
      Insert a ANY before the specified iten (0 = first item)
    }
    Function Insert(iIndex : Integer) : Tv3Base; overload;
    {
      Add an existing ED to the list
    }
    Procedure AddItem(value : Tv3Base);
    {
      Get the iIndexth ANY (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3Base;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3Base) : Integer;
    {
      Set the iIndexth ANY (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3Base);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;



    {
      Link to the parent; not always populated
    }
    Property Parent : Tv3Base read FParent write FParent;

  End;


  {
    Extensions on v3 classes
  }
  Tv3Extension = class (Tv3Base)
  Private
    FNamespace : String;
    FName : String;
    FText : String;
    FOffset: Integer;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    Function RIMClassNameV: String; Override;
    Function CDAClassNameV: String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function Link : Tv3Extension; Overload;
    function Clone(parent : Tv3Base) : Tv3Extension; Overload;
    procedure Assign(oSource : TFslObject); Override;



    Property offset : Integer read FOffset write FOffset;
    Property namespace : String read FNamespace write FNamespace;
    Property name : String read FName write FName;
    Property text : String read FText write FText;
  End;


  {
    a list of extensions
  }
  Tv3ExtensionList = class (Tv3BaseList)
  private
    Function GetExtension(iIndex : Integer) : Tv3Extension;
    procedure SetExtensions(iIndex: Integer; const Value: Tv3Extension);
  public

    Function Link : Tv3ExtensionList; Overload;
    Function Clone(parent : Tv3Base) : Tv3ExtensionList; Overload;


    {
      Add a ANY to the end of the list
    }
    function Append : Tv3Extension;
    {
      Insert a existing ANY before the specified iten (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : Tv3Extension);
    {
      Insert a ANY before the specified iten (0 = first item)
    }
    Function Insert(iIndex : Integer) : Tv3Extension;
    {
      Add an existing ED to the list
    }
    Procedure AddItem(value : Tv3Extension);
    {
      Get the iIndexth ANY (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3Extension;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3Extension) : Integer;
    {
      Set the iIndexth ANY (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3Extension);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;

    Property Extension[iIndex : Integer] : Tv3Extension read GetExtension write SetExtensions; default;
  End;

Implementation

(*

{ Tv3PropertyValue }

function Tv3PropertyValue.CheckType(aType: Tv3PropertyValueType): Tv3PropertyValue;
begin
  if not (self is aType) Then
    Error('CheckType', 'Type is not right');
  Result := self;
end;

constructor Tv3PropertyValue.Create(aType: TRMPropertyDefinitionType; bIsCollection: Boolean);
begin
  Inherited Create;
  FType := aType;
  FIsCollection := bIsCollection;
end;

{ Tv3PropertyValueString }

constructor Tv3PropertyValueString.Create(aType : TRMPropertyDefinitionType; sValue : WideString);
begin
  Inherited Create(aType, False);
  FValue := sValue;
end;


constructor Tv3PropertyValueString.Create(aType: TRMPropertyDefinitionType; Const sValue: String; Const aPossibles: Array of String);
var
  iLoop : Integer;
begin
  Inherited Create(aType, False);
  FValue := sValue;
  SetLength(FPossibles, Length(aPossibles));
  For iLoop := Low(aPossibles) To High(aPossibles) Do
    FPossibles[iLoop] := aPossibles[iLoop];
End;

{ Tv3PropertyValueStringCollection }

constructor Tv3PropertyValueStringCollection.Create(aType: TRMPropertyDefinitionType; oValues : TWideStringList);
begin
  Inherited Create(aType, True);
  FValue := TWideStringList.Create;
  if oValues <> Nil Then
    FValue.Assign(oValues);
end;

constructor Tv3PropertyValueStringCollection.Create(aType: TRMPropertyDefinitionType; const sValues : String; const aPossibles: array of String);
var
  iLoop : Integer;
begin
  FValue := TWideStringList.Create;
  if sValues <> '' Then
    FValue.Text := sValues;
  SetLength(FPossibles, Length(aPossibles));
  For iLoop := Low(aPossibles) To High(aPossibles) Do
    FPossibles[iLoop] := aPossibles[iLoop];
end;

destructor Tv3PropertyValueStringCollection.Destroy;
begin
  FValue.Free;
  inherited;
end;

*)

{ Tv3PropertyDefinitionList }

function Tv3PropertyDefinitionList.GetProperty(iIndex: Integer): Tv3PropertyDefinition;
begin
  result := Tv3PropertyDefinition(ObjectByIndex[iIndex]);
end;


{ Tv3DataTypePropertyIterator }

constructor Tv3DataTypePropertyIterator.Create(oFocus : Tv3Base; bInheritedProperties : Boolean);
begin
  Inherited Create;
  FFocus := oFocus;
  FProperties := Tv3PropertyDefinitionList.Create;
  if FFocus <> nil Then
    FFocus.ListProperties(FProperties, bInheritedProperties);
end;

function Tv3DataTypePropertyIterator.GetCurrent: Tv3PropertyDefinition;
begin
  Result := FProperties[FCursor];
end;

destructor Tv3DataTypePropertyIterator.Destroy;
begin
  FProperties.Free;
  inherited;
end;

function Tv3DataTypePropertyIterator.More: Boolean;
begin
  result := FCursor < FProperties.Count;
end;

procedure Tv3DataTypePropertyIterator.Next;
begin
  inc(FCursor);
end;

procedure Tv3DataTypePropertyIterator.Reset;
begin
  FCursor := 0;
end;

function Tv3DataTypePropertyIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFocus.sizeInBytes);
  inc(result, FProperties.sizeInBytes);
end;

{ Tv3Base }

function Tv3Base.createIterator(bInheritedProperties : Boolean): Tv3DataTypePropertyIterator;
begin
  Result := Tv3DataTypePropertyIterator.create(self, bInheritedProperties);
end;

procedure Tv3Base.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  oList.Add(Tv3PropertyDefinition.CreateStrings(self, true, 'comments', FComments));
end;


procedure Tv3Base.SetPropertyValue(const aValue: Tv3PropertyDefinition);
begin
  If aValue.Name = 'comments' Then
    comments.Assign(aValue.AsType(TFslStringList))
  Else
    RaiseError('SetPropertyValue', 'The property '+aValue.Name+' is unknown');
end;

procedure Tv3Base.SetTag(const Value: TFslObject);
begin
  FTag.Free;
  FTag := Value;
end;

function Tv3Base.HasComments: boolean;
begin
  result := (Fcomments <> nil) And (Fcomments.Count > 0);
end;


procedure Tv3Base.Assign(oSource: TFslObject);
begin
  inherited;
  FreeAndNil(Fcomments);
  if Tv3Base(oSource).HasComments Then
    GetComments.assign(Tv3Base(oSource).comments);
  Path := Tv3Base(oSource).Path;
  Tag := Tv3Base(oSource).Tag.link;
  TagId := Tv3Base(oSource).TagId;
end;

function Tv3Base.Clone(parent : Tv3Base): Tv3Base;
begin
  Result := Tv3Base(Inherited Clone);
  result.Parent := parent;
end;

function Tv3Base.Getcomments: TFslStringList;
begin
  if Fcomments = nil Then
    FComments := TFslStringList.Create;
  Result := Fcomments;
end;

function Tv3Base.Link: Tv3Base;
begin
  Result := Tv3Base(Inherited Link);
end;

destructor Tv3Base.Destroy;
begin
  FTag.free;
  Fcomments.Free;
  FExtensions.free;
  inherited;
end;

function Tv3Base.RIMClassName: String;
begin
  result := RimClassNameV;
end;

function Tv3Base.RIMClassNameV: String;
begin
  result := 'OclAny';
end;

constructor Tv3Base.Create;
begin
  inherited;
  FComments := nil;
  FExtensions := nil;
end;

function Tv3Base.Equals(oOther: TObject): Boolean;
begin
  if self = nil Then
    result := oOther = nil
  Else if oOther = nil then
    result := false
  Else if not (oOther is Tv3Base) then
    result := false
  Else
    result := equalsV(oOther as Tv3Base);
end;

function Tv3Base.EqualsV(oOther: Tv3Base): Boolean;
begin
  result := Self = oOther;
end;

function Tv3Base.CDAClassName: String;
begin
  result := CDAClassNameV;
end;

function Tv3Base.CDAClassNameV: String;
begin
  result := RIMClassNameV;
end;

function Tv3Base.CDAClassType: TCDAClassType;
begin
  result := CDAClassTypeV;
end;

function Tv3Base.CDAClassTypeV: TCDAClassType;
begin
  result := itNull;
end;

procedure Tv3Base.Clear;
begin
  if self <> nil then
    DoClear;
end;

procedure Tv3Base.DoClear;
begin
  if assigned(FComments) then
    FComments.Clear;
  if assigned(FExtensions) then
    FExtensions.Clear;
end;

function Tv3Base.GetExtensions: Tv3ExtensionList;
begin
  if FExtensions = nil Then
    FExtensions := Tv3ExtensionList.Create(self);
  Result := FExtensions;
end;

function Tv3Base.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fcomments.sizeInBytes);
  inc(result, FElement.sizeInBytes);
  inc(result, (FPath.length * sizeof(char)) + 12);
  inc(result, FExtensions.sizeInBytes);
  inc(result, FTag.sizeInBytes);
  inc(result, (FTagId.length * sizeof(char)) + 12);
end;

{ Tv3PropertyDefinition }

Constructor Tv3PropertyDefinition.CreateString(oOwner : Tv3Base; bIsStructural : Boolean; Const sName, sValue : String);
Begin
  Inherited Create;
  FName := sName;
  FOwner := oOwner;
  FValueType := rmpdtString;
  FCollectionState := rmpctNone;
  FIsStructural := bIsStructural;

  FValueString := sValue;
End;

Constructor Tv3PropertyDefinition.CreateStrings(oOwner : Tv3Base; bIsStructural : Boolean; Const sName : String; oList : TFslStringList);
Begin
  Inherited Create;
  FName := sName;
  FOwner := oOwner;
  FValueType := rmpdtString;
  FCollectionState := rmpctList;
  FIsStructural := bIsStructural;
  FValueStrings := TFslStringList.Create;
  if oList <> Nil Then
    FValueStrings.Assign(oList);
End;

Constructor Tv3PropertyDefinition.CreateDecimal(oOwner : Tv3Base; bIsStructural : Boolean; Const sName : String; aValue : TFslDecimal);
Begin
  Inherited Create;
  FName := sName;
  FOwner := oOwner;
  FValueType := rmpdtDecimal;
  FCollectionState := rmpctNone;
  FIsStructural := bIsStructural;
  FValueString := aValue.AsString;
End;

constructor Tv3PropertyDefinition.CreateBoolean(oOwner: Tv3Base; bIsStructural : Boolean; const sName: String; bHasValue, bValue: Boolean);
begin
  Inherited Create;
  FName := sName;
  FOwner := oOwner;
  FValueType := rmpdtBoolean;
  FCollectionState := rmpctNone;
  FIsStructural := bIsStructural;
  if bHasValue Then
    FValueString := BoolToStr(bValue);
end;


constructor Tv3PropertyDefinition.CreateClass(oOwner: Tv3Base; bIsStructural : Boolean; const sName: String; oObject: Tv3Base; sClass : String);
begin
  Inherited Create;
  FName := sName;
  FOwner := oOwner;
  FValueType := rmpdtClass;
  FClassname := sClass;
  FCollectionState := rmpctNone;
  FIsStructural := bIsStructural;
  FValueBase := oObject.Link;
end;


constructor Tv3PropertyDefinition.CreateClass(oOwner: Tv3Base; bIsStructural : Boolean; const sName: String; oList: Tv3BaseList; aCollectionState : TRMPropertyCollectionType; sClass : String);
begin
  Inherited Create;
  FName := sName;
  FOwner := oOwner;
  FValueType := rmpdtClass;
  FClassname := sClass;
  FCollectionState := aCollectionState;
  FIsStructural := bIsStructural;
  FValueCollection := Tv3BaseList(oList.Link);
end;


constructor Tv3PropertyDefinition.CreateEnum(oOwner: Tv3Base; bIsStructural : Boolean; const sName: String; Const iValue : Integer; const aPossibles: array of String);
var
  iLoop : Integer;
begin
  Inherited Create;
  FName := sName;
  FOwner := oOwner;
  FValueType := rmpdtEnum;
  FCollectionState := rmpctNone;
  FIsStructural := bIsStructural;
  FValueString := aPossibles[iValue];
  FPossibles := TFslStringList.Create;
  For iLoop := Low(aPossibles) To High(aPossibles) Do
    FPossibles.Add(aPossibles[iLoop]);
end;

constructor Tv3PropertyDefinition.CreateInteger(oOwner: Tv3Base; bIsStructural : Boolean; const sName: String; bHasValue : Boolean; iValue: int64);
begin
  Inherited Create;
  FName := sName;
  FOwner := oOwner;
  FValueType := rmpdtInt;
  FCollectionState := rmpctNone;
  FIsStructural := bIsStructural;
  if bHasValue Then
    FValueString := IntToStr(iValue);
end;

constructor Tv3PropertyDefinition.CreateSet(oOwner: Tv3Base; bIsStructural : Boolean; const sName : String; var aSet; const aPossibles: array of String);
var
  iLoop : Integer;
begin
  Inherited Create;
  FName := sName;
  FOwner := oOwner;
  FValueType := rmpdtEnum;
  FCollectionState := rmpctSet;
  FIsStructural := bIsStructural;
  FValueStrings := TFslStringList.Create;
  FPossibles := TFslStringList.Create;
  For iLoop := Low(aPossibles) To High(aPossibles) Do
    FPossibles.Add(aPossibles[iLoop]);
  SetSet(aSet);
end;

constructor Tv3PropertyDefinition.CreateBinary(oOwner: Tv3Base; bIsStructural : Boolean; const sName: String; oBuffer: TFslBuffer);
begin
  Inherited Create;
  FName := sName;
  FOwner := oOwner;
  FValueType := rmpdtBinary;
  FCollectionState := rmpctNone;
  FIsStructural := bIsStructural;
  if (oBuffer <> nil) Then
    FValueString := oBuffer.AsText;
end;


destructor Tv3PropertyDefinition.Destroy;
begin
  FPossibles.Free;
  FValueBase.Free;
  FValueCollection.Free;
  FValueString := '';
  FValueStrings.Free;
  inherited;
end;


function Tv3PropertyDefinition.AsType(aType: TFslObjectClass): TFslObject;
begin
  if (FValueBase <> Nil) Then
  Begin
    if Not (FValueBase is aType) Then
      RaiseError('asType', 'Wrong Type');
    Result := FValueBase;
  End
  Else if FValueCollection <> Nil Then
  Begin
    if Not (FValueCollection is aType) Then
      RaiseError('asType', 'Wrong Type');
    Result := FValueCollection;
  End
  Else if FValueStrings <> Nil Then
  Begin
    if Not (FValueStrings is aType) Then
      RaiseError('asType', 'Wrong Type');
    Result := FValueStrings;
  End
  Else
    Result := Nil;
end;

function Tv3PropertyDefinition.GetAsEnum: Integer;
begin
  Result := 0;
  if FPossibles <> Nil Then
    Result := FPossibles.IndexByValue(FValueString);
  If Result = -1 Then
    Result := 0;
end;

function Tv3PropertyDefinition.GetAsString: String;
begin
  if (FValueBase <> Nil) Or (FValueCollection <> Nil) or (FValueStrings <> Nil) Then
    RaiseError('GetAsString', 'Wrong Type');
  Result := FValueString;
end;

procedure Tv3PropertyDefinition.SetAsEnum(const iValue: Integer);
begin
  FValueString := FPossibles[iValue];
  FOwner.SetPropertyValue(self);
end;

procedure Tv3PropertyDefinition.SetAsString(const sValue: String);
begin
  FValueString := sValue;
  FOwner.SetPropertyValue(self);
end;

procedure Tv3PropertyDefinition.SetSet(var aSet);
var
  oSet : TFslOrdinalSet;
  oIterator : TFslOrdinalSetIterator;
begin
  FValueStrings.Clear;

  oSet := TFslOrdinalSet.Create;
  Try
    oSet.Hook(aSet, FPossibles.Count);
    oIterator := TFslOrdinalSetIterator(oSet.Iterator);
    Try
      oIterator.First;
      While oIterator.More Do
      Begin
        If oIterator.Checked Then
          FValueStrings.Add(FPossibles[oIterator.Index]);
        oIterator.Next;
      End;
    Finally
      oIterator.Free;
    End;
  Finally
    oSet.Free;
  End;
end;

procedure Tv3PropertyDefinition.SetType(oObject: Tv3Base);
begin
  If Not (oObject is Tv3Base) Or (FValueType <> rmpdtClass) Or (CollectionState <> rmpctNone) Then
    RaiseError('SetType', 'Wrong Type');
  FValueBase := oObject.Link;
end;

procedure Tv3PropertyDefinition.SetType(oList: Tv3BaseList);
begin
  If Not (oList is Tv3BaseList) Or (FValueType <> rmpdtClass) Or (CollectionState = rmpctNone) Then
    RaiseError('SetType', 'Wrong Type');
  FValueCollection := Tv3BaseList(oList.Link);
end;

(*
function Tv3PropertyDefinition.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FOwner.sizeInBytes);
  inc(result, FValueType.sizeInBytes);
  inc(result, FCollectionState.sizeInBytes);
  inc(result, FPossibles.sizeInBytes);
  inc(result, (FClassName.length * sizeof(char)) + 12);
  inc(result, FValueBase.sizeInBytes);
  inc(result, FValueCollection.sizeInBytes);
  inc(result, FValueString.sizeInBytes);
  inc(result, FValueStrings.sizeInBytes);
end;

function Tv3Base.CheckType(aValue: Tv3BaseCollection; aType: Tv3BaseCollectionType): Tv3BaseCollection;
begin
  if Not (aValue is aType) then
    RaiseError('CheckType', 'Wrong Type');
  Result := aValue;
end;

function Tv3Base.CheckType(aValue: Tv3Base; aType: Tv3Datatype): Tv3Base;
begin
  if Not (aValue is aType) then
    RaiseError('CheckType', 'Wrong Type');
  Result := aValue;
end;

procedure Tv3Base.ReadBoolean(const sValue: String; var bHasValue, bValue: Boolean);
begin
  if sValue = '' Then
    bHasValue := false
  Else if (sValue = 'false') Then
  Begin
    bHasValue := true;
    bValue := false
  End
  Else if (sValue = 'true') Then
  Begin
    bHasValue := true;
    bValue := true
  End
  Else
    RaiseError('ReadBoolean', 'Value '+sValue+' is not an acceptable boolean value');
end;

Function Tv3Base.ReadEnum(Const sValue : String; Const aNames : Array of String) : Integer;
Begin
  if sValue = '' Then
    result := 0
  Else
    result := StringArrayIndexOfSensitive(aNames, sValue);
  If result = -1 then
    RaiseError('ReadEnum', 'Value '+sValue+' is not an acceptable value');
End;

function Tv3Base.BuildSet(var aSet; const aCodes: array of String): String;
var
  oSet : TFslOrdinalSet;
  oIterator : TFslOrdinalSetIterator;
  oBuilder : TFslStringBuilder;
begin
  oSet := TFslOrdinalSet.Create;
  Try
    oSet.Hook(aSet, Length(aCodes));
    oIterator := TFslOrdinalSetIterator(oSet.Iterator);
    Try
      oBuilder := TFslStringBuilder.Create;
      Try
        oIterator.First;
        While oIterator.More Do
        Begin
          If oIterator.Checked Then
            oBuilder.Append(aCodes[oIterator.Index]+ crlf);
          oIterator.Next;
        End;

        Result := oBuilder.AsString;
      Finally
        oBuilder.Free;
      End;
    Finally
      oIterator.Free;
    End;
  Finally
    oSet.Free;
  End;
end;

procedure Tv3Base.ReadSet(var aSet; Const sValues : TWideStringList; const aCodes: array of String);
var
  oSet : TFslOrdinalSet;
  iLoop : Integer;
  iIndex : Integer;
begin
  oSet := TFslOrdinalSet.Create;
  Try
    oSet.Hook(aSet, Length(aCodes));
    oSet.UncheckAll;
    For iLoop := 0 to sValues.Count - 1 Do
    Begin
      iIndex := StringArrayIndexOf(aCodes, sValues[iLoop]);
      If iIndex >= 0 Then
        oSet.Check(iIndex);
    End;
    oSet.Unhook;
  Finally
    oSet.Free;
  End;
end;

procedure Tv3Base.ReadInteger(const sValue: String; var bHasValue: Boolean; var bValue: Int64);
begin
  if sValue = '' Then
    bHasValue := false
  Else if StringIsInteger64(sValue) Then
  Begin
    bHasValue := true;
    bValue := StringToInteger64(sValue);
  End
  Else
    RaiseError('ReadInteger', 'Value '+sValue+' is not an acceptable integer value');
end;

    *)

procedure Tv3PropertyDefinition.AsBool(var bHasValue, bValue: Boolean);
begin
  bHasValue := FValueString <> '';
  if bHasValue Then
    bValue := StrToBool(FValueString);
end;

procedure Tv3PropertyDefinition.AsSet(var aSet);
var
  oSet : TFslOrdinalSet;
  iLoop : Integer;
  iIndex : Integer;
begin
  oSet := TFslOrdinalSet.Create;
  Try
    oSet.Hook(aSet, FPossibles.Count);
    oSet.UncheckAll;
    For iLoop := 0 to FValueStrings.Count - 1 Do
    Begin
      iIndex := FPossibles.IndexByValue(FValueStrings[iLoop]);
      If iIndex >= 0 Then
        oSet.Check(iIndex);
    End;
    oSet.Unhook;
  Finally
    oSet.Free;
  End;
end;

procedure Tv3PropertyDefinition.AsInt(var bHasValue: Boolean; var iValue: int64);
begin
  bHasValue := FValueString <> '';
  if bHasValue Then
    iValue := StrToInt(FValueString);
end;

function Tv3PropertyDefinition.RIMClassName(bAsCollection : Boolean): String;
begin
  Case FValueType Of
    rmpdtBoolean : Result := 'Boolean';
    rmpdtBinary : Result := 'Binary';
    rmpdtInt : Result := 'Int';
    rmpdtString : Result := 'String';
    rmpdtDecimal : Result := 'Decimal';
    rmpdtDateTime : Result := 'DateTime';
    rmpdtEnum : Result := 'Enum';
    rmpdtClass :
      Begin
      if (CollectionState = rmpctNone) And (FValueBase <> Nil) Then
        result := FValueBase.RIMClassName
      Else
        Result := FClassName;
      End;
  End;

  if bAsCollection Then
    Case CollectionState of
      rmpctSet: Result := 'Set<'+Result+'>';
      rmpctList: Result := 'List<'+Result+'>';
      rmpctBag: Result := 'Bag<'+Result+'>';
    End;
end;

function Tv3PropertyDefinition.HasValue: Boolean;
begin
  if (FValueBase <> Nil) Then
    Result := True
  Else if FValueCollection <> Nil Then
    Result := FValueCollection.Count > 0
  Else if FValueStrings <> Nil Then
    Result := FValueStrings.Count > 0
  Else
    Result := FValueString <> '';
end;

function Tv3PropertyDefinition.CDAClassName(bAsCollection: Boolean): String;
begin
  Case FValueType Of
    rmpdtBoolean : Result := 'Boolean';
    rmpdtBinary : Result := 'Binary';
    rmpdtInt : Result := 'Int';
    rmpdtString : Result := 'String';
    rmpdtDecimal : Result := 'Decimal';
    rmpdtDateTime : Result := 'DateTime';
    rmpdtEnum : Result := 'Enum';
    rmpdtClass :
      Begin
      if (CollectionState = rmpctNone) And (FValueBase <> Nil) Then
        result := FValueBase.CDAClassName
      Else
        Result := FClassName;
      End;
  End;

  if bAsCollection Then
    Case CollectionState of
      rmpctSet: Result := 'Set<'+Result+'>';
      rmpctList: Result := 'List<'+Result+'>';
      rmpctBag: Result := 'Bag<'+Result+'>';
    End;
end;

{ Tv3BaseList }

function Tv3BaseList.Clone(parent : Tv3Base): Tv3BaseList;
begin
  result := Tv3BaseList(Inherited Clone);
  result.Parent := parent;
end;

function Tv3BaseList.GetBase(iIndex: Integer): Tv3Base;
begin
  result := Tv3Base(Inherited ObjectByIndex[iIndex]);
end;

function Tv3BaseList.Link: Tv3BaseList;
begin
  result := Tv3BaseList(Inherited Link);
end;


function Tv3BaseList.Append: Tv3Base;
begin
  Result := Tv3Base.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3BaseList.Insert(iIndex: Integer): Tv3Base;
begin
  Result := Tv3Base.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
end;


Procedure Tv3BaseList.InsertItem(iIndex: Integer; value: Tv3Base);
begin
  Inherited Insert(iIndex, value);
end;

procedure Tv3BaseList.InternalAfterInclude(iIndex: Integer; oObject : TFslObject);
begin
  inherited;
  if Parent <> nil then
    Tv3Base(Objects[iIndex]).Parent := Parent;
end;

function Tv3BaseList.Item(iIndex: Integer): Tv3Base;
begin
  Result := Base[iIndex];
end;

function Tv3BaseList.IndexOf(value : Tv3Base): Integer;
Begin
  result := IndexByReference(value);
End;


Procedure Tv3BaseList.AddItem(value : Tv3Base);
begin
  Add(value);
end;

Procedure Tv3BaseList.SetItemByIndex(iIndex: Integer; value: Tv3Base);
begin
  Base[iIndex] := value;
end;

procedure Tv3BaseList.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3BaseList.ClearItems;
begin
  Clear;
end;

function Tv3BaseList.Count: Integer;
begin
  result := Inherited Count;

end;

constructor Tv3BaseList.Create(Parent: Tv3Base);
begin
  Inherited Create;
  Self.Parent := Parent;
end;

procedure Tv3BaseList.SetBases(iIndex: Integer; const Value: Tv3Base);
begin
  ObjectByIndex[iIndex] := Value;
end;


{ Tv3Extension }

procedure Tv3Extension.Assign(oSource: TFslObject);
begin
  inherited;
  Name := Tv3Extension(oSource).Name;
  Namespace := Tv3Extension(oSource).Namespace;
  Text := Tv3Extension(oSource).Text;
  offset := Tv3Extension(oSource).offset;
end;

function Tv3Extension.CDAClassNameV: String;
begin
  result := '??';
end;

function Tv3Extension.CDAClassTypeV: TCDAClassType;
begin
  result := etExtension;
end;

function Tv3Extension.Clone(parent : Tv3Base): Tv3Extension;
begin
  result := Tv3Extension(Inherited Clone(parent));
end;

constructor Tv3Extension.Create;
begin
  inherited;
end;

destructor Tv3Extension.Destroy;
begin
  inherited;
end;

procedure Tv3Extension.DoClear;
begin
  inherited;
  Namespace := '';
  Name := '';
  Text := '';
end;


function Tv3Extension.Link: Tv3Extension;
begin
  result := Tv3Extension(Inherited Link);
end;

procedure Tv3Extension.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties: Boolean);
begin
  inherited;
end;

function Tv3Extension.RIMClassNameV: String;
begin
  result := '??';
end;

procedure Tv3Extension.SetPropertyValue(const aValue: Tv3PropertyDefinition);
begin
  inherited;
end;

function Tv3Extension.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FNamespace.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FText.length * sizeof(char)) + 12);
end;

{ Tv3ExtensionList }

function Tv3ExtensionList.Clone(parent : Tv3Base): Tv3ExtensionList;
begin
  result := Tv3ExtensionList(Inherited Clone(parent));
end;

function Tv3ExtensionList.GetExtension(iIndex: Integer): Tv3Extension;
begin
  result := Tv3Extension(Inherited ObjectByIndex[iIndex]);
end;

function Tv3ExtensionList.Link: Tv3ExtensionList;
begin
  result := Tv3ExtensionList(Inherited Link);
end;


function Tv3ExtensionList.Append: Tv3Extension;
begin
  Result := Tv3Extension.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3ExtensionList.Insert(iIndex: Integer): Tv3Extension;
begin
  Result := Tv3Extension.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
end;

Procedure Tv3ExtensionList.InsertItem(iIndex: Integer; value: Tv3Extension);
begin
  Inherited Insert(iIndex, value);
end;

function Tv3ExtensionList.Item(iIndex: Integer): Tv3Extension;
begin
  Result := Extension[iIndex];
end;

function Tv3ExtensionList.IndexOf(value : Tv3Extension): Integer;
Begin
  result := IndexByReference(value);
End;

Procedure Tv3ExtensionList.AddItem(value : Tv3Extension);
begin
  Add(value);
end;

Procedure Tv3ExtensionList.SetItemByIndex(iIndex: Integer; value: Tv3Extension);
begin
  Extension[iIndex] := value;
end;

procedure Tv3ExtensionList.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3ExtensionList.ClearItems;
begin
  Clear;
end;

function Tv3ExtensionList.Count: Integer;
begin
  result := Inherited Count;

end;

procedure Tv3ExtensionList.SetExtensions(iIndex: Integer; const Value: Tv3Extension);
begin
  ObjectByIndex[iIndex] := Value;
end;


End.

