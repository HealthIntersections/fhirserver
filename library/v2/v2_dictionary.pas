unit v2_dictionary;

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

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_collections, fsl_stream,
  fsl_threads, fsl_xml,
  v2_base;

const
  XML_NS_SCHEMA = 'http://www.w3.org/2001/XMLSchema';

Type
  THL7V2ModelDataTypeName = String;
  THL7V2ModelDataTypeDescription = String;
  THL7V2ModelDataTypeLength = Integer;

  THL7V2ModelDataType = Class(TFslObject)
    Private
      FName : THL7V2ModelDataTypeName;
      FDescription : THL7V2ModelDataTypeDescription;
      FLength : THL7V2ModelDataTypeLength;

    Protected

    function sizeInBytesV : cardinal; override;
    Public
      Constructor Create; Overload; Override;
      Destructor Destroy; Overload; Override;

      Function Link : THL7V2ModelDataType; Overload;
      Function Clone : THL7V2ModelDataType; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Function Escapable : Boolean;

      Property Name : THL7V2ModelDataTypeName Read FName Write FName;
      Property Description : THL7V2ModelDataTypeDescription Read FDescription Write FDescription;
      Property Length : THL7V2ModelDataTypeLength Read FLength Write FLength;
  End;

  THL7V2ModelDataTypes = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : THL7V2ModelDataType;
      Procedure SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelDataType);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByName(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByDescription(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByLength(pA, pB : Pointer) : Integer; Overload; Virtual;

      Function Get(Const aValue : Integer) : THL7V2ModelDataType; Reintroduce; Overload; Virtual;

    Public
      Function Link : THL7V2ModelDataTypes; Overload;
      Function Clone : THL7V2ModelDataTypes; Overload;

      Function New : THL7V2ModelDataType; Reintroduce; Overload; Virtual;

      Function IndexByName(Const aValue : THL7V2ModelDataTypeName) : Integer; Overload; Virtual;
      Function IndexByDescription(Const aValue : THL7V2ModelDataTypeDescription) : Integer; Overload; Virtual;

      Function GetByName(Const aValue : THL7V2ModelDataTypeName) : THL7V2ModelDataType; Overload; Virtual;
      Function GetByDescription(Const aValue : THL7V2ModelDataTypeDescription) : THL7V2ModelDataType; Overload; Virtual;

      Function ExistsByName(Const aValue : THL7V2ModelDataTypeName) : Boolean; Overload; Virtual;
      Function ExistsByDescription(Const aValue : THL7V2ModelDataTypeDescription) : Boolean; Overload; Virtual;

      Function Add(const sName, sDesc : String; iLength : Integer) : THL7V2ModelDataType; Overload;

      Procedure SortedByName; Overload; Virtual;
      Procedure SortedByDescription; Overload; Virtual;

      Function IsSortedByName : Boolean; Overload; Virtual;
      Function IsSortedByDescription : Boolean; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : THL7V2ModelDataType Read GetElement Write SetElement; Default;
  End;

Const
  THL7V2MODELDATATYPE_NAME_FIELD = 'Name';
  THL7V2MODELDATATYPE_DESCRIPTION_FIELD = 'Description';
  THL7V2MODELDATATYPE_LENGTH_FIELD = 'Length';

Type
  THL7V2ModelTableItemID = Integer;
  THL7V2ModelTableItemCode = String;
  THL7V2ModelTableItemDescription = String;

  THL7V2ModelTableItem = Class(TFslObject)
    Private
      FID : THL7V2ModelTableItemID;
      FCode : THL7V2ModelTableItemCode;
      FDescription : THL7V2ModelTableItemDescription;

    Protected

    function sizeInBytesV : cardinal; override;
    Public
      Constructor Create; Overload; Override;
      Destructor Destroy; Overload; Override;

      Function Link : THL7V2ModelTableItem; Overload;
      Function Clone : THL7V2ModelTableItem; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Property ID : THL7V2ModelTableItemID Read FID Write FID;
      Property Code : THL7V2ModelTableItemCode Read FCode Write FCode;
      Property Description : THL7V2ModelTableItemDescription Read FDescription Write FDescription;
  End;

  THL7V2ModelTableItems = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : THL7V2ModelTableItem;
      Procedure SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelTableItem);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByID(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByCode(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByDescription(pA, pB : Pointer) : Integer; Overload; Virtual;

      Function Get(Const aValue : Integer) : THL7V2ModelTableItem; Reintroduce; Overload; Virtual;

    Public
      Function Link : THL7V2ModelTableItems; Overload;
      Function Clone : THL7V2ModelTableItems; Overload;

      Function New : THL7V2ModelTableItem; Reintroduce; Overload; Virtual;

      Function IndexByID(Const aValue : THL7V2ModelTableItemID) : Integer; Overload; Virtual;
      Function IndexByCode(Const aValue : THL7V2ModelTableItemCode) : Integer; Overload; Virtual;
      Function IndexByDescription(Const aValue: THL7V2ModelTableItemDescription): Integer; Overload; Virtual;

      Function GetByID(Const aValue : THL7V2ModelTableItemID) : THL7V2ModelTableItem; Overload; Virtual;
      Function GetByCode(Const aValue : THL7V2ModelTableItemCode) : THL7V2ModelTableItem; Overload; Virtual;
      Function GetByDescription(Const aValue: THL7V2ModelTableItemDescription): THL7V2ModelTableItem; Overload; Virtual;

      Function ExistsByID(Const aValue : THL7V2ModelTableItemID) : Boolean; Overload; Virtual;
      Function ExistsByCode(Const aValue : THL7V2ModelTableItemCode) : Boolean; Overload; Virtual;
      Function ExistsByDescription(Const aValue: THL7V2ModelTableItemDescription): Boolean; Overload; Virtual;

      Function Add(iId : Integer; Const sCode, sDesc : String) : THL7V2ModelTableItem; Overload;

      Procedure SortedByID; Overload; Virtual;
      Procedure SortedByCode; Overload; Virtual;
      Procedure SortedByDescription; Overload; Virtual;

      Function IsSortedByID : Boolean; Overload; Virtual;
      Function IsSortedByCode : Boolean; Overload; Virtual;
      Function IsSortedByDescription: Boolean; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : THL7V2ModelTableItem Read GetElement Write SetElement; Default;
  End;

Const
  THL7V2MODELTABLEITEM_ID_FIELD = 'ID';
  THL7V2MODELTABLEITEM_CODE_FIELD = 'Code';
  THL7V2MODELTABLEITEM_DESCRIPTION_FIELD = 'Description';

Type
  THL7V2ModelTableID = Integer;
  THL7V2ModelTableDescription = String;

  THL7V2ModelTable = Class(TFslObject)
    Private
      FID : THL7V2ModelTableID;
      FDescription : THL7V2ModelTableDescription;
      FItems : THL7V2ModelTableItems;
      Procedure SetItems(Const Value: THL7V2ModelTableItems);

    Protected

    function sizeInBytesV : cardinal; override;
    Public
      Constructor Create; Overload; Override;
      Destructor Destroy; Overload; Override;

      Function Link : THL7V2ModelTable; Overload;
      Function Clone : THL7V2ModelTable; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Function CodesAsString : String;

      Property ID : THL7V2ModelTableID Read FID Write FID;
      Property Description : THL7V2ModelTableDescription Read FDescription Write FDescription;
      Property Items : THL7V2ModelTableItems Read FItems Write SetItems;

  End;

  THL7V2ModelTables = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : THL7V2ModelTable;
      Procedure SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelTable);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByID(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByDescription(pA, pB: Pointer): Integer;Overload; Virtual;

      Function Get(Const aValue : Integer) : THL7V2ModelTable; Reintroduce; Overload; Virtual;

    Public
      Function Link : THL7V2ModelTables; Overload;
      Function Clone : THL7V2ModelTables; Overload;

      Function New : THL7V2ModelTable; Reintroduce; Overload; Virtual;

      Function IndexByID(Const aValue : THL7V2ModelTableID) : Integer; Overload; Virtual;
      Function IndexByDescription(Const aValue: THL7V2ModelTableDescription): Integer; Overload; Virtual;

      Function GetByID(Const aValue : THL7V2ModelTableID) : THL7V2ModelTable; Overload; Virtual;
      Function GetByDescription(Const aValue: THL7V2ModelTableDescription): THL7V2ModelTable; Overload; Virtual;

      Function ExistsByID(Const aValue : THL7V2ModelTableID) : Boolean; Overload; Virtual;
      Function ExistsByDescription(Const aValue: THL7V2ModelTableDescription): Boolean; Overload; Virtual;

      Function Add(iId : Integer; Const sDesc : String) : THL7V2ModelTable; Overload;
      Procedure SortedByDescription; Overload; Virtual;
      Procedure SortedByID; Overload; Virtual;

      Function IsSortedByID : Boolean; Overload; Virtual;
      Function IsSortedByDescription: Boolean; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : THL7V2ModelTable Read GetElement Write SetElement; Default;
  End;

Const
  THL7V2MODELTABLE_ID_FIELD = 'ID';
  THL7V2MODELTABLE_DESCRIPTION_FIELD = 'Description';
  THL7V2MODELTABLE_DESCRIPTION_ITEMS = 'Items';

Type
  THL7V2ModelComponentName = String;
  THL7V2ModelComponentDataType = String;
  THL7V2ModelComponentTable = Integer;
  THL7V2ModelComponentNumber = Integer;

  THL7V2ModelComponent = Class(TFslObject)
    Private
      FName : THL7V2ModelComponentName;
      FDataType : THL7V2ModelComponentDataType;
      FTable : THL7V2ModelComponentTable;
      FNumber: THL7V2ModelComponentNumber;
      FRefTable: THL7V2ModelTable;
      FRefDataType: THL7V2ModelDataType;
      FRefStructure: TFslObject;
      Procedure SetRefTable(Const Value: THL7V2ModelTable);
      Procedure SetRefDataType(Const Value: THL7V2ModelDataType);

    Protected

    function sizeInBytesV : cardinal; override;
    Public
      Constructor Create; Overload; Override;
      Destructor Destroy; Overload; Override;

      Function Link : THL7V2ModelComponent; Overload;
      Function Clone : THL7V2ModelComponent; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Property Name : THL7V2ModelComponentName Read FName Write FName;
      Property DataType : THL7V2ModelComponentDataType Read FDataType Write FDataType;
      Property Table : THL7V2ModelComponentTable Read FTable Write FTable;
      Property Number : THL7V2ModelComponentNumber Read FNumber Write FNUmber;

      Property RefTable : THL7V2ModelTable Read FRefTable Write SetRefTable;
      Property RefDataType : THL7V2ModelDataType Read FRefDataType Write SetRefDataType;

      // this is circular - components are contained by structures, and point at structures.
      // Can't even aggregate, but that's ok, they are loaded and unloaded as a block
      // note, in v2.5, this is directly circular - a simple data type is a structure that has itself as a first component (!)
      Property RefStructure : TFslObject Read FRefStructure Write FRefStructure;
  End;

  THL7V2ModelComponents = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : THL7V2ModelComponent;
      Procedure SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelComponent);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByName(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByNumber(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByDataType(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByTable(pA, pB : Pointer) : Integer; Overload; Virtual;

      Function Get(Const aValue : Integer) : THL7V2ModelComponent; Reintroduce; Overload; Virtual;

    Public
      Function Link : THL7V2ModelComponents; Overload;
      Function Clone : THL7V2ModelComponents; Overload;

      Function New : THL7V2ModelComponent; Reintroduce; Overload; Virtual;

      Function IndexByName(Const aValue : THL7V2ModelComponentName) : Integer; Overload; Virtual;
      Function IndexByNumber(Const aValue : THL7V2ModelComponentNumber) : Integer; Overload; Virtual;
      Function IndexByDataType(Const aValue : THL7V2ModelComponentDataType) : Integer; Overload; Virtual;
      Function IndexByTable(Const aValue : THL7V2ModelComponentTable) : Integer; Overload; Virtual;

      Function GetByName(Const aValue : THL7V2ModelComponentName) : THL7V2ModelComponent; Overload; Virtual;
      Function GetByNumber(Const aValue : THL7V2ModelComponentNumber) : THL7V2ModelComponent; Overload; Virtual;
      Function GetByDataType(Const aValue : THL7V2ModelComponentDataType) : THL7V2ModelComponent; Overload; Virtual;
      Function GetByTable(Const aValue : THL7V2ModelComponentTable) : THL7V2ModelComponent; Overload; Virtual;

      Function ExistsByName(Const aValue : THL7V2ModelComponentName) : Boolean; Overload; Virtual;
      Function ExistsByNumber(Const aValue : THL7V2ModelComponentNumber) : Boolean; Overload; Virtual;
      Function ExistsByDataType(Const aValue : THL7V2ModelComponentDataType) : Boolean; Overload; Virtual;
      Function ExistsByTable(Const aValue : THL7V2ModelComponentTable) : Boolean; Overload; Virtual;

      Function Add(Const sName, sDataType : String; iTable, iNumber : Integer) : THL7V2ModelComponent; Overload;
      Procedure SortedByName; Overload; Virtual;
      Procedure SortedByNumber; Overload; Virtual;
      Procedure SortedByDataType; Overload; Virtual;
      Procedure SortedByTable; Overload; Virtual;

      Function IsSortedByName : Boolean; Overload; Virtual;
      Function IsSortedByNumber : Boolean; Overload; Virtual;
      Function IsSortedByDataType : Boolean; Overload; Virtual;
      Function IsSortedByTable : Boolean; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : THL7V2ModelComponent Read GetElement Write SetElement; Default;
  End;

Const
  THL7V2MODELCOMPONENT_NAME_FIELD = 'Name';
  THL7V2MODELCOMPONENT_DataType_FIELD = 'DataType';
  THL7V2MODELCOMPONENT_TABLE_FIELD = 'Table';
  THL7V2MODELCOMPONENT_NUMBER_FIELD = 'Number';

Type
  THL7V2ModelStructureName = String;
  THL7V2ModelStructureDescription = String;
  THL7V2ModelStructureDataType = String;
  THL7V2ModelStructureID = Integer;

  THL7V2ModelStructure = Class(TFslObject)
    Private
      FName : THL7V2ModelStructureName;
      FDescription : THL7V2ModelStructureDescription;
      FDataType : THL7V2ModelStructureDataType;
      FID : THL7V2ModelStructureID;
      FRefDataType: THL7V2ModelDataType;
      FComponents: THL7V2ModelComponents;
      Procedure SetRefDataType(Const Value: THL7V2ModelDataType);
      Procedure SetComponents(Const Value: THL7V2ModelComponents);

    Protected

    function sizeInBytesV : cardinal; override;
    Public
      Constructor Create; Overload; Override;
      Destructor Destroy; Overload; Override;

      Function Link : THL7V2ModelStructure; Overload;
      Function Clone : THL7V2ModelStructure; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Property Name : THL7V2ModelStructureName Read FName Write FName;
      Property Description : THL7V2ModelStructureDescription Read FDescription Write FDescription;
      Property DataType : THL7V2ModelStructureDataType Read FDataType Write FDataType;
      Property ID : THL7V2ModelStructureID Read FID Write FID;
      Property Components : THL7V2ModelComponents Read FComponents Write SetComponents;

      Property RefDataType : THL7V2ModelDataType Read FRefDataType Write SetRefDataType;
  End;

  THL7V2ModelStructures = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : THL7V2ModelStructure;
      Procedure SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelStructure);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByName(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByDataType(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByID(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByDescription(pA, pB: Pointer): Integer; Overload; Virtual;

      Function Get(Const aValue : Integer) : THL7V2ModelStructure; Reintroduce; Overload; Virtual;

    Public
      Function Link : THL7V2ModelStructures; Overload;
      Function Clone : THL7V2ModelStructures; Overload;

      Function New : THL7V2ModelStructure; Reintroduce; Overload; Virtual;

      Function IndexByName(Const aValue : THL7V2ModelStructureName) : Integer; Overload; Virtual;
      Function IndexByDataType(Const aValue : THL7V2ModelStructureDataType) : Integer; Overload; Virtual;
      Function IndexByID(Const aValue : THL7V2ModelStructureID) : Integer; Overload; Virtual;
      Function IndexByDescription(Const aValue: THL7V2ModelStructureDescription): Integer; Overload; Virtual;

      Function GetByName(Const aValue : THL7V2ModelStructureName) : THL7V2ModelStructure; Overload; Virtual;
      Function GetByDataType(Const aValue : THL7V2ModelStructureDataType) : THL7V2ModelStructure; Overload; Virtual;
      Function GetByID(Const aValue : THL7V2ModelStructureID) : THL7V2ModelStructure; Overload; Virtual;
      Function GetByDescription(Const aValue: THL7V2ModelStructureDescription): THL7V2ModelStructure; Overload; Virtual;

      Function ExistsByName(Const aValue : THL7V2ModelStructureName) : Boolean; Overload; Virtual;
      Function ExistsByDataType(Const aValue : THL7V2ModelStructureDataType) : Boolean; Overload; Virtual;
      Function ExistsByID(Const aValue : THL7V2ModelStructureID) : Boolean; Overload; Virtual;
      Function ExistsByDescription(Const aValue: THL7V2ModelStructureDescription): Boolean; Overload; Virtual;

      Function Add(const sName, sDesc, sDatatype : String; iId : Integer) : THL7V2ModelStructure; Overload;

      Procedure SortedByName; Overload; Virtual;
      Procedure SortedByDescription;  Overload; Virtual;
      Procedure SortedByDataType; Overload; Virtual;
      Procedure SortedByID; Overload; Virtual;

      Function IsSortedByName : Boolean; Overload; Virtual;
      Function IsSortedByDataType : Boolean; Overload; Virtual;
      Function IsSortedByID : Boolean; Overload; Virtual;
      Function IsSortedByDescription: Boolean; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : THL7V2ModelStructure Read GetElement Write SetElement; Default;
  End;

Const
  THL7V2MODELSTRUCTURE_NAME_FIELD = 'Name';
  THL7V2MODELSTRUCTURE_DESCRIPTION_FIELD = 'Description';
  THL7V2MODELSTRUCTURE_DATATYPE_FIELD = 'DataType';
  THL7V2MODELSTRUCTURE_ID_FIELD = 'ID';
  THL7V2MODELSTRUCTURE_ID_COMPONENTS = 'Components';

Type
  THL7V2ModelDataElementDescription = String;
  THL7V2ModelDataElementId = Integer;
  THL7V2ModelDataElementStructure = String;
  THL7V2ModelDataElementTable = Integer;

  THL7V2ModelDataElement = Class(TFslObject)
    Private
      FDescription : THL7V2ModelDataElementDescription;
      FId : THL7V2ModelDataElementId;
      FStructure : THL7V2ModelDataElementStructure;
      FLength_Old : Integer;
      FLength_Min : Integer;
      FLength_Max : Integer;
      FLength_Conf : String;
      FTable : THL7V2ModelDataElementTable;
      FRefTable: THL7V2ModelTable;
      FRefStructure: THL7V2ModelStructure;
      Procedure SetRefTable(Const Value: THL7V2ModelTable);
      Procedure SetRefStructure(Const Value: THL7V2ModelStructure);
      function getLengthDesc: String;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      Constructor Create; Overload; Override;
      Destructor Destroy; Overload; Override;

      Function Link : THL7V2ModelDataElement; Overload;
      Function Clone : THL7V2ModelDataElement; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Property Description : THL7V2ModelDataElementDescription Read FDescription Write FDescription;
      Property Id : THL7V2ModelDataElementId Read FId Write FId;
      Property Structure : THL7V2ModelDataElementStructure Read FStructure Write FStructure;
      Property Length_Old : Integer Read FLength_Old Write FLength_Old;
      Property Length_Min : Integer Read FLength_Min Write FLength_Min;
      Property Length_Max : Integer Read FLength_Max Write FLength_Max;
      Property Length_Conf : String Read FLength_Conf Write FLength_Conf;
      Property LengthDesc : String read getLengthDesc;
      function HasLength : Boolean;
      Property Table : THL7V2ModelDataElementTable Read FTable Write FTable;

      Property RefTable : THL7V2ModelTable Read FRefTable Write SetRefTable;
      Property RefStructure : THL7V2ModelStructure Read FRefStructure Write SetRefStructure;
  End;

  THL7V2ModelDataElements = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : THL7V2ModelDataElement;
      Procedure SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelDataElement);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByDescription(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareById(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByStructure(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByLength(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByTable(pA, pB : Pointer) : Integer; Overload; Virtual;

      Function Get(Const aValue : Integer) : THL7V2ModelDataElement; Reintroduce; Overload; Virtual;

    Public
      Function Link : THL7V2ModelDataElements; Overload;
      Function Clone : THL7V2ModelDataElements; Overload;

      Function New : THL7V2ModelDataElement; Reintroduce; Overload; Virtual;

      Function IndexByDescription(Const aValue : THL7V2ModelDataElementDescription) : Integer; Overload; Virtual;
      Function IndexById(Const aValue : THL7V2ModelDataElementId) : Integer; Overload; Virtual;
      Function IndexByStructure(Const aValue : THL7V2ModelDataElementStructure) : Integer; Overload; Virtual;
      Function IndexByLength(Const aValue : Integer) : Integer; Overload; Virtual;
      Function IndexByTable(Const aValue : THL7V2ModelDataElementTable) : Integer; Overload; Virtual;

      Function GetByDescription(Const aValue : THL7V2ModelDataElementDescription) : THL7V2ModelDataElement; Overload; Virtual;
      Function GetById(Const aValue : THL7V2ModelDataElementId) : THL7V2ModelDataElement; Overload; Virtual;
      Function GetByStructure(Const aValue : THL7V2ModelDataElementStructure) : THL7V2ModelDataElement; Overload; Virtual;
      Function GetByLength(Const aValue : Integer) : THL7V2ModelDataElement; Overload; Virtual;
      Function GetByTable(Const aValue : THL7V2ModelDataElementTable) : THL7V2ModelDataElement; Overload; Virtual;

      Function ExistsByDescription(Const aValue : THL7V2ModelDataElementDescription) : Boolean; Overload; Virtual;
      Function ExistsById(Const aValue : THL7V2ModelDataElementId) : Boolean; Overload; Virtual;
      Function ExistsByStructure(Const aValue : THL7V2ModelDataElementStructure) : Boolean; Overload; Virtual;
      Function ExistsByLength(Const aValue : Integer) : Boolean; Overload; Virtual;
      Function ExistsByTable(Const aValue : THL7V2ModelDataElementTable) : Boolean; Overload; Virtual;

      Function Add(const sDesc : String; iId : Integer; Const sStructure : String; iLength, iTable : Integer) : THL7V2ModelDataElement; Overload;
      Function Add(const sDesc : String; iId : Integer; Const sStructure : String; iLengthMin, iLengthMax : integer; sLengthConf : String; iTable : Integer) : THL7V2ModelDataElement; Overload;

      Procedure SortedByDescription; Overload; Virtual;
      Procedure SortedById; Overload; Virtual;
      Procedure SortedByStructure; Overload; Virtual;
      Procedure SortedByLength; Overload; Virtual;
      Procedure SortedByTable; Overload; Virtual;

      Function IsSortedByDescription : Boolean; Overload; Virtual;
      Function IsSortedById : Boolean; Overload; Virtual;
      Function IsSortedByStructure : Boolean; Overload; Virtual;
      Function IsSortedByLength : Boolean; Overload; Virtual;
      Function IsSortedByTable : Boolean; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : THL7V2ModelDataElement Read GetElement Write SetElement; Default;
  End;

Const
  THL7V2MODELDATAELEMENT_DESCRIPTION_FIELD = 'Description';
  THL7V2MODELDATAELEMENT_ID_FIELD = 'Id';
  THL7V2MODELDATAELEMENT_STRUCTURE_FIELD = 'Structure';
  THL7V2MODELDATAELEMENT_LENGTH_FIELD = 'Length';
  THL7V2MODELDATAELEMENT_LENGTH_FIELD_OLD = 'Length_Old';
  THL7V2MODELDATAELEMENT_LENGTH_FIELD_MIN = 'Length_Min';
  THL7V2MODELDATAELEMENT_LENGTH_FIELD_MAX = 'Length_Max';
  THL7V2MODELDATAELEMENT_LENGTH_FIELD_CONF = 'Length_Conf';
  THL7V2MODELDATAELEMENT_TABLE_FIELD = 'Table';

Type
  THL7V2ModelFieldDataElement = Integer;
  THL7V2ModelFieldRepeatable = Boolean;
  THL7V2ModelFieldRepeatCount = Integer;
  THL7V2ModelFieldRequired = Boolean;
  THL7V2ModelFieldFieldNumber = Integer;

  THL7V2ModelField = Class(TFslObject)
    Private
      FDataElement : THL7V2ModelFieldDataElement;
      FRepeatable : THL7V2ModelFieldRepeatable;
      FRepeatCount : THL7V2ModelFieldRepeatCount;
      FRequired : THL7V2ModelFieldRequired;
      FFieldNumber : THL7V2ModelFieldFieldNumber;
      FRefDataElement: THL7V2ModelDataElement;
      Procedure SetRefDataElement(Const Value: THL7V2ModelDataElement);

    Public
      Constructor Create; Overload; Override;
      Destructor Destroy; Overload; Override;

      Function Link : THL7V2ModelField; Overload;
      Function Clone : THL7V2ModelField; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Property DataElement : THL7V2ModelFieldDataElement Read FDataElement Write FDataElement;
      Property Repeatable : THL7V2ModelFieldRepeatable Read FRepeatable Write FRepeatable;
      Property RepeatCount : THL7V2ModelFieldRepeatCount Read FRepeatCount Write FRepeatCount;
      Property Required : THL7V2ModelFieldRequired Read FRequired Write FRequired;
      Property FieldNumber : THL7V2ModelFieldFieldNumber Read FFieldNumber Write FFieldNumber;

      Property RefDataElement : THL7V2ModelDataElement Read FRefDataElement Write SetRefDataElement;
  End;

  THL7V2ModelFields = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : THL7V2ModelField;
      Procedure SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelField);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByDataElement(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByRepeatable(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByRepeatCount(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByRequired(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByFieldNumber(pA, pB : Pointer) : Integer; Overload; Virtual;

      Function Get(Const aValue : Integer) : THL7V2ModelField; Reintroduce; Overload; Virtual;

    Public
      Function Link : THL7V2ModelFields; Overload;
      Function Clone : THL7V2ModelFields; Overload;

      Function New : THL7V2ModelField; Reintroduce; Overload; Virtual;

      Function IndexByDataElement(Const aValue : THL7V2ModelFieldDataElement) : Integer; Overload; Virtual;
      Function IndexByFieldNumber(Const aValue : THL7V2ModelFieldFieldNumber) : Integer; Overload; Virtual;

      Function GetByDataElement(Const aValue : THL7V2ModelFieldDataElement) : THL7V2ModelField; Overload; Virtual;
      Function GetByFieldNumber(Const aValue : THL7V2ModelFieldFieldNumber) : THL7V2ModelField; Overload; Virtual;

      Function ExistsByDataElement(Const aValue : THL7V2ModelFieldDataElement) : Boolean; Overload; Virtual;
      Function ExistsByFieldNumber(Const aValue : THL7V2ModelFieldFieldNumber) : Boolean; Overload; Virtual;

      Function Add(iDataElement : THL7V2ModelFieldDataElement; bRepeatable : THL7V2ModelFieldRepeatable; iRepeatCount : THL7V2ModelFieldRepeatCount;
                   bRequired : THL7V2ModelFieldRequired; iFieldNumber : THL7V2ModelFieldFieldNumber) : THL7V2ModelField; Overload;

      Procedure SortedByDataElement; Overload; Virtual;
      Procedure SortedByFieldNumber; Overload; Virtual;

      Function IsSortedByDataElement : Boolean; Overload; Virtual;
      Function IsSortedByFieldNumber : Boolean; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : THL7V2ModelField Read GetElement Write SetElement; Default;
  End;

Const
  THL7V2MODELField_DataElement_Field = 'DataElement';
  THL7V2MODELField_REPEATABLE_Field = 'Repeatable';
  THL7V2MODELField_REPEATCOUNT_Field = 'RepeatCount';
  THL7V2MODELField_REQUIRED_Field = 'Required';
  THL7V2MODELField_FieldNUMBER_Field = 'FieldNumber';

Type
  THL7V2ModelSegmentCode = String;
  THL7V2ModelSegmentDescription = String;

  THL7V2ModelSegment = Class(TFslObject)
    Private
      FCode : THL7V2ModelSegmentCode;
      FDescription : THL7V2ModelSegmentDescription;
      FFields: THL7V2ModelFields;
      Procedure SetFields(Const Value: THL7V2ModelFields);

    Protected

    function sizeInBytesV : cardinal; override;
    Public
      Constructor Create; Overload; Override;
      Destructor Destroy; Overload; Override;

      Function Link : THL7V2ModelSegment; Overload;
      Function Clone : THL7V2ModelSegment; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Property Code : THL7V2ModelSegmentCode Read FCode Write FCode;
      Property Description : THL7V2ModelSegmentDescription Read FDescription Write FDescription;
      Property Fields : THL7V2ModelFields Read FFields Write SetFields;
  End;

  THL7V2ModelSegments = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : THL7V2ModelSegment;
      Procedure SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelSegment);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByCode(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByDescription(pA, pB : Pointer) : Integer; Overload; Virtual;

      Function Get(Const aValue : Integer) : THL7V2ModelSegment; Reintroduce; Overload; Virtual;

    Public
      Function Link : THL7V2ModelSegments; Overload;
      Function Clone : THL7V2ModelSegments; Overload;

      Function New : THL7V2ModelSegment; Reintroduce; Overload; Virtual;

      Function IndexByCode(Const aValue : THL7V2ModelSegmentCode) : Integer; Overload; Virtual;
      Function IndexByDescription(Const aValue: THL7V2ModelSegmentDescription): Integer; Overload; Virtual;

      Function GetByCode(Const aValue : THL7V2ModelSegmentCode) : THL7V2ModelSegment; Overload; Virtual;
      Function GetByDescription(Const aValue: THL7V2ModelSegmentDescription): THL7V2ModelSegment; Overload; Virtual;

      Function ExistsByCode(Const aValue : THL7V2ModelSegmentCode) : Boolean; Overload; Virtual;
      Function ExistsByDescription(Const aValue: THL7V2ModelSegmentDescription): Boolean; Overload; Virtual;

      Function Add(Const sCode, sDesc : String) : THL7V2ModelSegment; Overload;
      Procedure SortedByCode; Overload; Virtual;
      Procedure SortedByDescription; Overload; Virtual;

      Function IsSortedByCode : Boolean; Overload; Virtual;
      Function IsSortedByDescription: Boolean; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : THL7V2ModelSegment Read GetElement Write SetElement; Default;
  End;

Const
  THL7V2MODELSEGMENT_CODE_FIELD = 'Code';
  THL7V2MODELSEGMENT_DESCRIPTION_FIELD = 'Description';
  THL7V2MODELSEGMENT_FIELDS_FIELD = 'Description';

Type
  THL7V2ModelSegmentGroupCode = String;
  THL7V2ModelSegmentGroupOptional = Boolean;
  THL7V2ModelSegmentGroupRepeating = Boolean;
  THL7V2ModelSegmentGroupType = (gtSingle, gtGroup, gtChoice);

  THL7V2ModelSegmentGroups = Class;

  THL7V2ModelSegmentGroup = Class(TFslObject)
    Private
      FCode : THL7V2ModelSegmentGroupCode;
      FOptional : THL7V2ModelSegmentGroupOptional;
      FRepeating : THL7V2ModelSegmentGroupRepeating;
      FGroupType : THL7V2ModelSegmentGroupType;
      FChildren: THL7V2ModelSegmentGroups;
      Procedure SetChildren(Const Value: THL7V2ModelSegmentGroups);

    Protected

    function sizeInBytesV : cardinal; override;
    Public
      Constructor Create; Overload; Override;
      Constructor Create(Const sCode : String; bOptional, bRepeating : Boolean; aType : THL7V2ModelSegmentGroupType); Overload;
      Destructor Destroy; Overload; Override;

      Function Link : THL7V2ModelSegmentGroup; Overload;
      Function Clone : THL7V2ModelSegmentGroup; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Function UsesSegment(oSegment : THL7V2ModelSegment) : Boolean;

      Property Code : THL7V2ModelSegmentGroupCode Read FCode Write FCode;
      Property Optional : THL7V2ModelSegmentGroupOptional Read FOptional Write FOptional;
      Property Repeating : THL7V2ModelSegmentGroupRepeating Read FRepeating Write FRepeating;
      Property GroupType : THL7V2ModelSegmentGroupType Read FGroupType Write FGroupType;
      Property Children : THL7V2ModelSegmentGroups Read FChildren Write SetChildren;
  End;

  THL7V2ModelSegmentGroups = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : THL7V2ModelSegmentGroup;
      Procedure SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelSegmentGroup);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByCode(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByOptional(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByRepeating(pA, pB : Pointer) : Integer; Overload; Virtual;

      Function Get(Const aValue : Integer) : THL7V2ModelSegmentGroup; Reintroduce; Overload; Virtual;

    Public
      Function Link : THL7V2ModelSegmentGroups; Overload;
      Function Clone : THL7V2ModelSegmentGroups; Overload;

      Function New : THL7V2ModelSegmentGroup; Reintroduce; Overload; Virtual;

      Function IndexByCode(Const aValue : THL7V2ModelSegmentGroupCode) : Integer; Overload; Virtual;

      Function GetByCode(Const aValue : THL7V2ModelSegmentGroupCode) : THL7V2ModelSegmentGroup; Overload; Virtual;

      Function ExistsByCode(Const aValue : THL7V2ModelSegmentGroupCode) : Boolean; Overload; Virtual;

      Function Add(Const sCode : String; bOptional, bRepeating : Boolean; aGroupType : THL7V2ModelSegmentGroupType) : THL7V2ModelSegmentGroup; Overload; Virtual;

      Procedure SortedByCode; Overload; Virtual;

      Function IsSortedByCode : Boolean; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : THL7V2ModelSegmentGroup Read GetElement Write SetElement; Default;
  End;

Type
  THL7V2ModelMessageStructureName = String;
  THL7V2ModelMessageStructureDescription = String;
  THL7V2ModelMessageStructureExampleEvent = String;
  THL7V2ModelMessageStructureExampleMsgType = String;
  THL7V2ModelMessageStructureAction = String;

  THL7V2ModelMessageStructure = Class(TFslObject)
    Private
      FName : THL7V2ModelMessageStructureName;
      FDescription : THL7V2ModelMessageStructureDescription;
      FExampleEvent : THL7V2ModelMessageStructureExampleEvent;
      FExampleMsgType : THL7V2ModelMessageStructureExampleMsgType;
      FAction : THL7V2ModelMessageStructureAction;
      FSegmentMap: THL7V2ModelSegmentGroup;
      FXMLMap: THL7V2ModelSegmentGroup;
      Procedure SetSegmentMap(Const Value: THL7V2ModelSegmentGroup);
      Procedure SetXMLMap(Const Value: THL7V2ModelSegmentGroup);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      Constructor Create; Overload; Override;
      Destructor Destroy; Overload; Override;

      Function Link : THL7V2ModelMessageStructure; Overload;
      Function Clone : THL7V2ModelMessageStructure; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Function UsesSegment(oSegment : THL7V2ModelSegment) : Boolean;

      Property Name : THL7V2ModelMessageStructureName Read FName Write FName;
      Property Description : THL7V2ModelMessageStructureDescription Read FDescription Write FDescription;
      Property ExampleEvent : THL7V2ModelMessageStructureExampleEvent Read FExampleEvent Write FExampleEvent;
      Property ExampleMsgType : THL7V2ModelMessageStructureExampleMsgType Read FExampleMsgType Write FExampleMsgType;
      Property Action : THL7V2ModelMessageStructureAction Read FAction Write FAction;

      Property SegmentMap : THL7V2ModelSegmentGroup Read FSegmentMap Write SetSegmentMap;
      Property XMLMap : THL7V2ModelSegmentGroup Read FXMLMap Write SetXMLMap;
  End;

  THL7V2ModelMessageStructures = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : THL7V2ModelMessageStructure;
      Procedure SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelMessageStructure);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByName(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByDescription(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByExampleEvent(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByExampleMsgType(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByAction(pA, pB : Pointer) : Integer; Overload; Virtual;

      Function Get(Const aValue : Integer) : THL7V2ModelMessageStructure; Reintroduce; Overload; Virtual;

    Public
      Function Link : THL7V2ModelMessageStructures; Overload;
      Function Clone : THL7V2ModelMessageStructures; Overload;

      Function New : THL7V2ModelMessageStructure; Reintroduce; Overload; Virtual;

      Function IndexByName(Const aValue : THL7V2ModelMessageStructureName) : Integer; Overload; Virtual;
      Function IndexByDescription(Const aValue : THL7V2ModelMessageStructureDescription) : Integer; Overload; Virtual;
      Function IndexByExampleEvent(Const aValue : THL7V2ModelMessageStructureExampleEvent) : Integer; Overload; Virtual;
      Function IndexByExampleMsgType(Const aValue : THL7V2ModelMessageStructureExampleMsgType) : Integer; Overload; Virtual;
      Function IndexByAction(Const aValue : THL7V2ModelMessageStructureAction) : Integer; Overload; Virtual;

      Function GetByName(Const aValue : THL7V2ModelMessageStructureName) : THL7V2ModelMessageStructure; Overload; Virtual;
      Function GetByDescription(Const aValue : THL7V2ModelMessageStructureDescription) : THL7V2ModelMessageStructure; Overload; Virtual;
      Function GetByExampleEvent(Const aValue : THL7V2ModelMessageStructureExampleEvent) : THL7V2ModelMessageStructure; Overload; Virtual;
      Function GetByExampleMsgType(Const aValue : THL7V2ModelMessageStructureExampleMsgType) : THL7V2ModelMessageStructure; Overload; Virtual;
      Function GetByAction(Const aValue : THL7V2ModelMessageStructureAction) : THL7V2ModelMessageStructure; Overload; Virtual;

      Function ExistsByName(Const aValue : THL7V2ModelMessageStructureName) : Boolean; Overload; Virtual;
      Function ExistsByDescription(Const aValue : THL7V2ModelMessageStructureDescription) : Boolean; Overload; Virtual;
      Function ExistsByExampleEvent(Const aValue : THL7V2ModelMessageStructureExampleEvent) : Boolean; Overload; Virtual;
      Function ExistsByExampleMsgType(Const aValue : THL7V2ModelMessageStructureExampleMsgType) : Boolean; Overload; Virtual;
      Function ExistsByAction(Const aValue : THL7V2ModelMessageStructureAction) : Boolean; Overload; Virtual;

      Function Add(Const sName, sDescription, sExampleEvent, sExampleMsgType, sAction : String) : THL7V2ModelMessageStructure;  Overload; Virtual;

      Procedure SortedByName; Overload; Virtual;
      Procedure SortedByDescription; Overload; Virtual;
      Procedure SortedByExampleEvent; Overload; Virtual;
      Procedure SortedByExampleMsgType; Overload; Virtual;
      Procedure SortedByAction; Overload; Virtual;

      Function IsSortedByName : Boolean; Overload; Virtual;
      Function IsSortedByDescription : Boolean; Overload; Virtual;
      Function IsSortedByExampleEvent : Boolean; Overload; Virtual;
      Function IsSortedByExampleMsgType : Boolean; Overload; Virtual;
      Function IsSortedByAction : Boolean; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : THL7V2ModelMessageStructure Read GetElement Write SetElement; Default;
  End;

Const
  THL7V2MODELMESSAGESTRUCTURE_NAME_FIELD = 'Name';
  THL7V2MODELMESSAGESTRUCTURE_DESCRIPTION_FIELD = 'Description';
  THL7V2MODELMESSAGESTRUCTURE_EXAMPLEEVENT_FIELD = 'ExampleEvent';
  THL7V2MODELMESSAGESTRUCTURE_EXAMPLEMSGTYPE_FIELD = 'ExampleMsgType';
  THL7V2MODELMESSAGESTRUCTURE_ACTION_FIELD = 'Action';
  THL7V2MODELMESSAGESTRUCTURE_SEGMENTMAP_FIELD = 'SegmentMap';
  THL7V2MODELMESSAGESTRUCTURE_XMLMAP_FIELD = 'XMLMap';

Type
  THL7V2ModelEventMessageMessage = String;
  THL7V2ModelEventMessageStructure = String;
  THL7V2ModelEventMessageReply = String;
  THL7V2ModelEventMessageReplyStructure = String;

  THL7V2ModelEventMessage = Class(TFslObject)
    Private
      FMessage : THL7V2ModelEventMessageMessage;
      FStructure : THL7V2ModelEventMessageStructure;
      FReply : THL7V2ModelEventMessageReply;
      FReplyStructure : THL7V2ModelEventMessageReplyStructure;
      FRefReplyStructure: THL7V2ModelMessageStructure;
      FRefStructure: THL7V2ModelMessageStructure;
      Procedure SetRefReplyStructure(Const Value: THL7V2ModelMessageStructure);
      Procedure SetRefStructure(Const Value: THL7V2ModelMessageStructure);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      Constructor Create; Overload; Override;
      Destructor Destroy; Overload; Override;

      Function Link : THL7V2ModelEventMessage; Overload;
      Function Clone : THL7V2ModelEventMessage; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Property Message : THL7V2ModelEventMessageMessage Read FMessage Write FMessage;
      Property Structure : THL7V2ModelEventMessageStructure Read FStructure Write FStructure;
      Property Reply : THL7V2ModelEventMessageReply Read FReply Write FReply;
      Property ReplyStructure : THL7V2ModelEventMessageReplyStructure Read FReplyStructure Write FReplyStructure;

      Property RefStructure : THL7V2ModelMessageStructure Read FRefStructure Write SetRefStructure;
      Property RefReplyStructure : THL7V2ModelMessageStructure Read FRefReplyStructure Write SetRefReplyStructure;
  End;

  THL7V2ModelEventMessages = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : THL7V2ModelEventMessage;
      Procedure SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelEventMessage);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByMessage(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByStructure(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByReply(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByReplyStructure(pA, pB : Pointer) : Integer; Overload; Virtual;

      Function Get(Const aValue : Integer) : THL7V2ModelEventMessage; Reintroduce; Overload; Virtual;

    Public
      Function Link : THL7V2ModelEventMessages; Overload;
      Function Clone : THL7V2ModelEventMessages; Overload;

      Function New : THL7V2ModelEventMessage; Reintroduce; Overload; Virtual;

      Function IndexByMessage(Const aValue : THL7V2ModelEventMessageMessage) : Integer; Overload; Virtual;
      Function IndexByStructure(Const aValue : THL7V2ModelEventMessageStructure) : Integer; Overload; Virtual;
      Function IndexByReply(Const aValue : THL7V2ModelEventMessageReply) : Integer; Overload; Virtual;
      Function IndexByReplyStructure(Const aValue : THL7V2ModelEventMessageReplyStructure) : Integer; Overload; Virtual;

      Function GetByMessage(Const aValue : THL7V2ModelEventMessageMessage) : THL7V2ModelEventMessage; Overload; Virtual;
      Function GetByStructure(Const aValue : THL7V2ModelEventMessageStructure) : THL7V2ModelEventMessage; Overload; Virtual;
      Function GetByReply(Const aValue : THL7V2ModelEventMessageReply) : THL7V2ModelEventMessage; Overload; Virtual;
      Function GetByReplyStructure(Const aValue : THL7V2ModelEventMessageReplyStructure) : THL7V2ModelEventMessage; Overload; Virtual;

      Function ExistsByMessage(Const aValue : THL7V2ModelEventMessageMessage) : Boolean; Overload; Virtual;
      Function ExistsByStructure(Const aValue : THL7V2ModelEventMessageStructure) : Boolean; Overload; Virtual;
      Function ExistsByReply(Const aValue : THL7V2ModelEventMessageReply) : Boolean; Overload; Virtual;
      Function ExistsByReplyStructure(Const aValue : THL7V2ModelEventMessageReplyStructure) : Boolean; Overload; Virtual;

      Function Add(const sMessage, sStructure, sReply, sReplyStructure : String) : THL7V2ModelEventMessage; Overload;

      Procedure SortedByMessage; Overload; Virtual;
      Procedure SortedByStructure; Overload; Virtual;
      Procedure SortedByReply; Overload; Virtual;
      Procedure SortedByReplyStructure; Overload; Virtual;

      Function IsSortedByMessage : Boolean; Overload; Virtual;
      Function IsSortedByStructure : Boolean; Overload; Virtual;
      Function IsSortedByReply : Boolean; Overload; Virtual;
      Function IsSortedByReplyStructure : Boolean; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : THL7V2ModelEventMessage Read GetElement Write SetElement; Default;
  End;

Const
  THL7V2MODELEVENTMESSAGE_MESSAGE_FIELD = 'Message';
  THL7V2MODELEVENTMESSAGE_STRUCTURE_FIELD = 'Structure';
  THL7V2MODELEVENTMESSAGE_REPLY_FIELD = 'Reply';
  THL7V2MODELEVENTMESSAGE_REPLYSTRUCTURE_FIELD = 'ReplyStructure';

Type
  THL7V2ModelEventName = String;
  THL7V2ModelEventDescription = String;

  THL7V2ModelEvent = Class(TFslObject)
    Private
      FName : THL7V2ModelEventName;
      FDescription : THL7V2ModelEventDescription;
      FMessages: THL7V2ModelEventMessages;
      Procedure SetMessages(Const Value: THL7V2ModelEventMessages);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      Constructor Create; Overload; Override;
      Destructor Destroy; Overload; Override;

      Function Link : THL7V2ModelEvent; Overload;
      Function Clone : THL7V2ModelEvent; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Function UsesStructure(oStructure : THL7V2ModelMessageStructure) : Boolean;

      Property Name : THL7V2ModelEventName Read FName Write FName;
      Property Description : THL7V2ModelEventDescription Read FDescription Write FDescription;
      Property Messages : THL7V2ModelEventMessages Read FMessages Write SetMessages;
  End;

  THL7V2ModelEvents = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : THL7V2ModelEvent;
      Procedure SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelEvent);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByName(pA, pB : Pointer) : Integer; Overload; Virtual;
      Function CompareByDescription(pA, pB : Pointer) : Integer; Overload; Virtual;

      Function Get(Const aValue : Integer) : THL7V2ModelEvent; Reintroduce; Overload; Virtual;

    Public
      Function Link : THL7V2ModelEvents; Overload;
      Function Clone : THL7V2ModelEvents; Overload;

      Function New : THL7V2ModelEvent; Reintroduce; Overload; Virtual;

      Function IndexByName(Const aValue : THL7V2ModelEventName) : Integer; Overload; Virtual;
      Function IndexByDescription(Const aValue : THL7V2ModelEventDescription) : Integer; Overload; Virtual;

      Function GetByName(Const aValue : THL7V2ModelEventName) : THL7V2ModelEvent; Overload; Virtual;
      Function GetByDescription(Const aValue : THL7V2ModelEventDescription) : THL7V2ModelEvent; Overload; Virtual;

      Function ExistsByName(Const aValue : THL7V2ModelEventName) : Boolean; Overload; Virtual;
      Function ExistsByDescription(Const aValue : THL7V2ModelEventDescription) : Boolean; Overload; Virtual;

      Function Add(const sName, sDesc : String) : THL7V2ModelEvent; Overload;

      Procedure SortedByName; Overload; Virtual;
      Procedure SortedByDescription; Overload; Virtual;

      Function IsSortedByName : Boolean; Overload; Virtual;
      Function IsSortedByDescription : Boolean; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : THL7V2ModelEvent Read GetElement Write SetElement; Default;
  End;

Const
  THL7V2MODELEVENT_NAME_FIELD = 'Name';
  THL7V2MODELEVENT_DESCRIPTION_FIELD = 'Description';
  THL7V2MODELEVENT_MESSAGES_FIELD = 'Messages';

Const
  THL7V2MODELSEGMENTGROUP_CODE_FIELD = 'Code';
  THL7V2MODELSEGMENTGROUP_OPTIONAL_FIELD = 'Optional';
  THL7V2MODELSEGMENTGROUP_REPEATING_FIELD = 'Repeating';
  THL7V2MODELSEGMENTGROUP_GROUPTYPE_FIELD = 'GroupType';
  THL7V2MODELSEGMENTGROUP_CHILDREN_FIELD = 'Children';

  NAMES_THL7V2MODELSEGMENTGROUPTYPE : Array [THL7V2ModelSegmentGroupType] Of String = ('Single', 'Group', 'Choice');
  LITERALS_THL7V2MODELSEGMENTGROUPTYPE : Array [THL7V2ModelSegmentGroupType] Of String = ('gtSingle', 'gtGroup', 'gtChoice');

type
  THL7V2Model = class (TFslObject)
  private
    FVersion: THL7V2Version;
    FTables: THL7V2ModelTables;
    FComponents: THL7V2ModelComponents;
    FDataElements: THL7V2ModelDataElements;
    FSegments: THL7V2ModelSegments;
    FDataTypes: THL7V2ModelDataTypes;
    FStructures: THL7V2ModelStructures;
    FEvents: THL7V2ModelEvents;
    FMessageStructures: THL7V2ModelMessageStructures;

    procedure SetTables(const Value: THL7V2ModelTables);
    procedure SetComponents(const Value: THL7V2ModelComponents);
    procedure SetDataElements(const Value: THL7V2ModelDataElements);
    procedure SetSegments(const Value: THL7V2ModelSegments);
    procedure SetDataTypes(const Value: THL7V2ModelDataTypes);
    procedure SetStructures(const Value: THL7V2ModelStructures);
    procedure SetEvents(const Value: THL7V2ModelEvents);
    procedure SetMessageStructures(const Value: THL7V2ModelMessageStructures);
    Function isVariableTypeName(Const sName : String) : Boolean;
  protected
    function sizeInBytesV : cardinal; override;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Function Link : THL7V2Model; Overload;
    Function Clone : THL7V2Model; Overload;
    Procedure Assign(oObject : TFslObject); Overload; Override;

    procedure CrossLink;

    property Version : THL7V2Version read FVersion write FVersion;
    property Tables : THL7V2ModelTables read FTables write SetTables;
    property Components : THL7V2ModelComponents read FComponents write SetComponents;
    property DataElements : THL7V2ModelDataElements read FDataElements write SetDataElements;
    property Segments : THL7V2ModelSegments read FSegments write SetSegments;
    property DataTypes : THL7V2ModelDataTypes read FDataTypes write SetDataTypes;
    property Structures : THL7V2ModelStructures read FStructures write SetStructures;
    property Events : THL7V2ModelEvents read FEvents write SetEvents;
    property MessageStructures : THL7V2ModelMessageStructures read FMessageStructures write SetMessageStructures;
  end;

Type
  TOnSchemaTransferProgress = Procedure (Sender: TObject; Const sVersion, sName: String; iCount, iTotal: Integer; Var bAbort: Boolean) Of Object;

  THL7V2SchemaStoreCacheEntry = class (TFslName)
  private
    FVersion : THL7V2Version;
    FDate : TDateTime;
    FMap : THL7V2ModelSegmentGroup;
    procedure SetMap(const Value: THL7V2ModelSegmentGroup);
  protected
    function sizeInBytesV : cardinal; override;
  public
    Destructor Destroy; override;

    property Version : THL7V2Version read FVersion write FVersion;
    property Date : TDateTime read FDate write FDate;
    property Map : THL7V2ModelSegmentGroup read FMap write SetMap;
  end;

  THL7V2SchemaStoreCacheEntries = class (TFslNameList)
  private
    function GetEntry(iIndex: Integer): THL7V2SchemaStoreCacheEntry;
  protected
    function ItemClass : TFslObjectClass; override;
    function CompareByVersionAndStructure(pA, pB: Pointer): Integer; Overload; Virtual;
    function Get(iIndex : integer):THL7V2SchemaStoreCacheEntry; Reintroduce; overload; virtual;
  public
    function ExistsByVersionAndStructure(aVersion : THL7V2Version; const sStruct: String) : Boolean; Overload; Virtual;
    function IndexByVersionAndStructure(aVersion : THL7V2Version; const sStruct: String) : Integer; Overload; Virtual;
    function GetByVersionAndStructure(aVersion : THL7V2Version; const sStruct: String) : THL7V2SchemaStoreCacheEntry; Overload; Virtual;

    procedure SortedByVersionAndStructure;

    property Entry[iIndex : Integer] : THL7V2SchemaStoreCacheEntry read GetEntry; default;
  end;

  THL7V2SchemaStore = Class(THL7V2WorkerObject)
  Private
    FLock : TFslLock;
    FFilename: String;
    FFolder: String;
    FCache : THL7V2SchemaStoreCacheEntries;

    Procedure SetFileName(Const sValue: String);

    Function GetSchemaFileName(aVersion : THL7V2Version; Const sStruct: String) : String;
    Function CheckFileExists(aVersion : THL7V2Version; Const sStruct: String) : TDateTime;

    Function GetChildByAttributeValue(oElement: TMXmlElement; Const sName, sValue: String; bCase: Boolean = False): TMXmlElement;
    Procedure ReadSchemaGroup(oDom: TMXmlDocument; oGroup: THL7V2ModelSegmentGroup; Const sFilename : String);
    Function LoadSchemaFromXML(aVersion : THL7V2Version; Const sStruct: String): THL7V2ModelSegmentGroup;

    Procedure ReadSegmentGroup(oReader : TReader; oGroup : THL7V2ModelSegmentGroup);
    Procedure ReadSchemaFromStream(aVersion : THL7V2Version; Const sStruct: String; oStream : TStream);
    Procedure LoadSchemasFromStore;

    Procedure ReadVersionSchemas(oWriter : TWriter; aVersion : THL7V2Version; sFolder : String; aProgress : TOnSchemaTransferProgress);
    Procedure WriteSegmentGroup(oWriter : TWriter; oGroup : THL7V2ModelSegmentGroup);
    Function SaveSegmentMap(oMap : THL7V2ModelSegmentGroup) : String;
    Procedure SaveSchemaMap(oWriter : TWriter; aVersion : THL7V2Version; sName : String; oMap : THL7V2ModelSegmentGroup);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create; Override;
    Constructor Create(Const sFileName, sFolder : String); Overload;
    Destructor Destroy; Override;

    Function ProduceSchemaMap(aVersion : THL7V2Version; Const sStruct: String): THL7V2ModelSegmentGroup; // acquires a reference, must be freed

    Procedure BuildFromSchemas(AProgress : TOnSchemaTransferProgress);

    Property FileName : String Read FFilename Write SetFileName;
    Property Folder : String Read FFolder Write FFolder;
  End;

type
  TOnDictTransferProgress = procedure (Sender: TObject; const sVersion, sTable: String; iCount, iTotal: Integer; var bAbort: Boolean) of object;

  THL7V2Dictionary = class (THL7V2WorkerObject)
  Private
    FVersions : THL7V2Versions;
    FVersionsLoaded : Boolean;
    FLock : TFslLock;
    FModels : Array [THL7V2Version] of THL7V2Model;
    FInitialised : Boolean;

    FOnTransferProgress: TOnDictTransferProgress;
    FSchemaStore: THL7V2SchemaStore;
    Function Progress(aVersion : THL7V2Version; const sTable: String; iCount, iTotal: Integer) : Boolean;
    function GetModel(aVersion: THL7V2Version): THL7V2Model;
    Function GetVersions : THL7V2Versions;

    procedure LoadModel(oModel : THL7V2Model);
    procedure SaveModel(oModel : THL7V2Model; const sDesc : String);
    procedure LoadVersions;
    procedure SetSchemaStore(const Value: THL7V2SchemaStore);

  Protected
    procedure PrepareForLoad(bWipe: Boolean); Overload; Virtual;
    procedure DoneLoading(aTransferEvent: TOnDictTransferProgress); Overload; Virtual;

    Function  ListVersions : THL7V2Versions; Overload; Virtual;
    function  VersionDefined(aVersion : THL7V2Version; var sDesc: String): Boolean; Overload; Virtual;
    procedure AddVersion(aVersion : THL7V2Version; const sDescription: String); Overload; Virtual;

    procedure LoadTables(aVersion : THL7V2Version; oTables : THL7V2ModelTables); Overload; Virtual;
    procedure LoadComponents(aVersion : THL7V2Version; oComponents : THL7V2ModelComponents); Overload; Virtual;
    procedure LoadDataElements(aVersion : THL7V2Version; oDataElements : THL7V2ModelDataElements); Overload; Virtual;
    procedure LoadSegments(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments); Overload; Virtual;
    procedure LoadSegmentFields(aVersion : THL7V2Version; oSegment : THL7V2ModelSegment); Overload; Virtual;
    procedure LoadSegmentFields(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments); Overload; Virtual;
    procedure LoadDataTypes(aVersion : THL7V2Version; oDataTypes : THL7V2ModelDataTypes); Overload; Virtual;
    procedure LoadStructures(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures); Overload; Virtual;
    procedure LoadStructureComponents(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures; oComponents : THL7V2ModelComponents); Overload; Virtual;
    procedure LoadEvents(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents); Overload; Virtual;
    procedure LoadEventMessages(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents); Overload; Virtual;
    procedure LoadMessageStructures(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures); Overload; Virtual;
    procedure LoadSegmentMaps(aVersion : THL7V2Version; oStructures : THL7V2ModelMessageStructures); Overload; Virtual;

    procedure AddTables(aVersion : THL7V2Version; oTables : THL7V2ModelTables); Overload; Virtual;
    procedure AddComponents(aVersion : THL7V2Version; oComponents : THL7V2ModelComponents); Overload; Virtual;
    procedure AddDataElements(aVersion : THL7V2Version; oDataElements : THL7V2ModelDataElements); Overload; Virtual;
    procedure AddSegments(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments); Overload; Virtual;
    procedure AddSegmentFields(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments); Overload; Virtual;
    procedure AddDataTypes(aVersion : THL7V2Version; oDataTypes : THL7V2ModelDataTypes); Overload; Virtual;
    procedure AddStructures(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures); Overload; Virtual;
    procedure AddStructureComponents(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures); Overload; Virtual;
    procedure AddEvents(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents); Overload; Virtual;
    procedure AddEventMessages(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents); Overload; Virtual;
    procedure AddMessageStructures(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures); Overload; Virtual;
    procedure AddSegmentMaps(aVersion : THL7V2Version; oStructures : THL7V2ModelMessageStructures); Overload; Virtual;
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create; Override;
    Destructor Destroy; override;
    Function Link : THL7V2Dictionary; Overload;
    Function Clone : THL7V2Dictionary; Overload;

    function HasVersion(aVer : THL7V2Version) : Boolean;
    function SourceDescription(bFulldetails: Boolean): String; Overload; Virtual;
    function VersionLoaded(aVer : THL7V2Version) : Boolean;
    procedure Initialise; overload; virtual;
    procedure TransferDatabase(oDest: THL7V2Dictionary; bWipe: Boolean);

    property Model[aVersion : THL7V2Version] : THL7V2Model read GetModel; default;
    Property SchemaStore : THL7V2SchemaStore read FSchemaStore write SetSchemaStore;
    property Versions : THL7V2Versions read GetVersions;
    property OnTransferProgress: TOnDictTransferProgress Read FOnTransferProgress Write FOnTransferProgress;
  end;

type
  THL7V2BinaryDictionaryContext = class (TFslName)
  private
    FStream: TMemoryStream;
    FReader : TReader;
    FWriter: TWriter;
  public
    Destructor Destroy; override;

    procedure Read(oBUffer : TFslBuffer);
    procedure Write;
    function CloseWriting : String;

    property Reader : TReader read FReader;
    property Writer: TWriter read FWriter;
    property Stream : TMemoryStream read FStream;
  end;

  THL7V2BinaryDictionaryContexts = class (TFslNameList)
  private
    function GetStream(iIndex : Integer): THL7V2BinaryDictionaryContext;
  protected
    Function ItemClass : TFslObjectClass; override;
  public
    function GetByName(const sName : String) : THL7V2BinaryDictionaryContext; reintroduce; overload; virtual;
    property Stream[iIndex : Integer] : THL7V2BinaryDictionaryContext read GetStream; default;
  end;

type

  THL7V2BinaryDictionary = class (THL7V2Dictionary)
  Private
    FBuffers : TFslNameBufferList;
    FContexts : THL7V2BinaryDictionaryContexts;
    FIsWriting: Boolean;
    FLicenced: Boolean;
    procedure LoadDictionary;
    procedure SaveToStream(oStream : TMemoryStream);
    function  GetReadingContext(const sName: String): THL7V2BinaryDictionaryContext;
    function GetWritingContext(const sName: String): THL7V2BinaryDictionaryContext;
    procedure ReadSegmentGroup(oContext : THL7V2BinaryDictionaryContext; oSegmentGroup : THL7V2ModelSegmentGroup);
    procedure WriteSegmentGroup(oContext : THL7V2BinaryDictionaryContext; oSegmentGroup : THL7V2ModelSegmentGroup);
  Protected
    function  VersionDefined(aVersion : THL7V2Version; var sDesc: String): Boolean; Override;
    Function ListVersions : THL7V2Versions; Override;
    procedure AddVersion(aVersion : THL7V2Version; const sDescription: String); Override;

    procedure LoadTables(aVersion : THL7V2Version; oTables : THL7V2ModelTables); Override;
    procedure LoadComponents(aVersion : THL7V2Version; oComponents : THL7V2ModelComponents); Override;
    procedure LoadDataElements(aVersion : THL7V2Version; oDataElements : THL7V2ModelDataElements); Override;
    procedure LoadSegments(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments); Override;
    procedure LoadSegmentFields(aVersion : THL7V2Version; oSegment : THL7V2ModelSegment); Override;
    procedure LoadDataTypes(aVersion : THL7V2Version; oDataTypes : THL7V2ModelDataTypes); Override;
    procedure LoadStructures(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures); Override;
    procedure LoadStructureComponents(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures; oComponents : THL7V2ModelComponents); Override;
    procedure LoadEvents(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents); Override;
    procedure LoadEventMessages(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents); Override;
    procedure LoadMessageStructures(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures); Override;
    procedure LoadSegmentMaps(aVersion : THL7V2Version; oStructures : THL7V2ModelMessageStructures); Override;

    procedure AddTables(aVersion : THL7V2Version; oTables : THL7V2ModelTables); Override;
    procedure AddComponents(aVersion : THL7V2Version; oComponents : THL7V2ModelComponents); Override;
    procedure AddDataElements(aVersion : THL7V2Version; oDataElements : THL7V2ModelDataElements); Override;
    procedure AddSegments(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments); Override;
    procedure AddSegmentFields(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments); Override;
    procedure AddDataTypes(aVersion : THL7V2Version; oDataTypes : THL7V2ModelDataTypes); Override;
    procedure AddStructures(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures); Override;
    procedure AddStructureComponents(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures); Override;
    procedure AddEvents(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents); Override;
    procedure AddEventMessages(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents); Override;
    procedure AddMessageStructures(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures); Override;
    procedure AddSegmentMaps(aVersion : THL7V2Version; oStructures : THL7V2ModelMessageStructures); Override;

    procedure PrepareForLoad(bWipe: Boolean); Override;
    procedure DoneLoading(aTransferEvent: TOnDictTransferProgress); Override;

    function GetContents(bForWriting : Boolean) : TFslAccessStream; overload; virtual;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; overload; override;
    destructor Destroy; Override;

    procedure Initialise; override;
  end;

  THL7V2FileDictionary = class (THL7V2BinaryDictionary)
  Private
    FFileName: String;
    FErrorsOk : Boolean;
  protected
    function GetContents(bForWriting : Boolean) : TFslAccessStream; override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(const sFilename : String; bSuppressException: Boolean = False); overload;

    function  SourceDescription(bFullDetails: Boolean): String; Override;

    property ErrorsOk : Boolean read FErrorsOk write FErrorsOk;
    property FileName: String read FFileName write FFileName;
  end;

  THL7V2DictionaryProvider = Class (THL7V2WorkerObject)
  Private
    FDictionary : THL7V2Dictionary;
    Procedure SetDictionary(Const Value: THL7V2Dictionary);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create(oDictionary : THL7V2Dictionary); Overload;
    Constructor Create(oProvider : THL7V2DictionaryProvider); Overload; // copy providers setup regarding dictionary
    Destructor Destroy; Override;

    Property Dictionary : THL7V2Dictionary Read FDictionary Write SetDictionary;
  End;

implementation

Constructor THL7V2ModelDataType.Create;
Begin
  Inherited;
End;

Destructor THL7V2ModelDataType.Destroy;
Begin
  Inherited;
End;

Function THL7V2ModelDataType.Link : THL7V2ModelDataType;
Begin
  Result := THL7V2ModelDataType(Inherited Link);
End;

Function THL7V2ModelDataType.Clone : THL7V2ModelDataType;
Begin
  Result := THL7V2ModelDataType(Inherited Clone);
End;

Procedure THL7V2ModelDataType.Assign(oObject : TFslObject);
Begin
  Inherited;

  Name := THL7V2ModelDataType(oObject).Name;
  Description := THL7V2ModelDataType(oObject).Description;
  Length := THL7V2ModelDataType(oObject).Length;
End;

function THL7V2ModelDataType.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeOf(char)) + 12);
  inc(result, (FDescription.length * sizeOf(char)) + 12);
end;

Function THL7V2ModelDataTypes.Link : THL7V2ModelDataTypes;
Begin
  Result := THL7V2ModelDataTypes(Inherited Link);
End;

Function THL7V2ModelDataTypes.Clone : THL7V2ModelDataTypes;
Begin
  Result := THL7V2ModelDataTypes(Inherited Clone);
End;

Function THL7V2ModelDataTypes.New : THL7V2ModelDataType;
Begin
  Result := THL7V2ModelDataType(Inherited New);
End;

Function THL7V2ModelDataTypes.ItemClass : TFslObjectClass;
Begin
  Result := THL7V2ModelDataType;
End;

Function THL7V2ModelDataTypes.GetElement(Const iIndex : Integer) : THL7V2ModelDataType;
Begin
  Result := THL7V2ModelDataType(ObjectByIndex[iIndex]);
End;

Procedure THL7V2ModelDataTypes.SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelDataType);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;

Function THL7V2ModelDataTypes.CompareByName(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelDataType(pA).Name, THL7V2ModelDataType(pB).Name);
End;

Function THL7V2ModelDataTypes.CompareByDescription(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelDataType(pA).Description, THL7V2ModelDataType(pB).Description);
End;

Function THL7V2ModelDataTypes.CompareByLength(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.IntegerCompare(THL7V2ModelDataType(pA).Length, THL7V2ModelDataType(pB).Length);
End;

Function THL7V2ModelDataTypes.IndexByName(Const aValue : THL7V2ModelDataTypeName) : Integer;
Var
  oElement : THL7V2ModelDataType;
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

Function THL7V2ModelDataTypes.IndexByDescription(Const aValue : THL7V2ModelDataTypeDescription) : Integer;
Var
  oElement : THL7V2ModelDataType;
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

Function THL7V2ModelDataTypes.Get(Const aValue : Integer) : THL7V2ModelDataType;
Begin
  Result := THL7V2ModelDataType(Inherited Get(aValue));
End;

Function THL7V2ModelDataTypes.GetByName(Const aValue : THL7V2ModelDataTypeName) : THL7V2ModelDataType;
Begin
  Result := Get(IndexByName(aValue));
End;

Function THL7V2ModelDataTypes.GetByDescription(Const aValue : THL7V2ModelDataTypeDescription) : THL7V2ModelDataType;
Begin
  Result := Get(IndexByDescription(aValue));
End;

Function THL7V2ModelDataTypes.ExistsByName(Const aValue : THL7V2ModelDataTypeName) : Boolean;
Begin
  Result := ExistsByIndex(IndexByName(aValue));
End;

Function THL7V2ModelDataTypes.ExistsByDescription(Const aValue : THL7V2ModelDataTypeDescription) : Boolean;
Begin
  Result := ExistsByIndex(IndexByDescription(aValue));
End;

Procedure THL7V2ModelDataTypes.SortedByName;
Begin
  SortedBy(CompareByName);
End;

Procedure THL7V2ModelDataTypes.SortedByDescription;
Begin
  SortedBy(CompareByDescription);
End;

Function THL7V2ModelDataTypes.IsSortedByName : Boolean;
Begin
  Result := IsSortedBy(CompareByName);
End;

Function THL7V2ModelDataTypes.IsSortedByDescription : Boolean;
Begin
  Result := IsSortedBy(CompareByDescription);
End;

Function THL7V2ModelDataType.Escapable: Boolean;
Begin
  Result := StringArrayExistsInsensitive(['ST', 'TX', 'FT', 'CF'], Name);
End;

function THL7V2ModelDataTypes.Add(const sName, sDesc: String; iLength: Integer): THL7V2ModelDataType;
begin
  Result := New;
  Try
    Result.Name := sName;
    Result.Description := sDesc;
    Result.Length := iLength;
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

Constructor THL7V2ModelTableItem.Create;
Begin
  Inherited;
End;

Destructor THL7V2ModelTableItem.Destroy;
Begin
  Inherited;
End;

Function THL7V2ModelTableItem.Link : THL7V2ModelTableItem;
Begin
  Result := THL7V2ModelTableItem(Inherited Link);
End;

Function THL7V2ModelTableItem.Clone : THL7V2ModelTableItem;
Begin
  Result := THL7V2ModelTableItem(Inherited Clone);
End;

Procedure THL7V2ModelTableItem.Assign(oObject : TFslObject);
Begin
  Inherited;

  ID := THL7V2ModelTableItem(oObject).ID;
  Code := THL7V2ModelTableItem(oObject).Code;
  Description := THL7V2ModelTableItem(oObject).Description;
End;

function THL7V2ModelTableItem.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeOf(char)) + 12);
  inc(result, (FDescription.length * sizeOf(char)) + 12);
end;

Function THL7V2ModelTableItems.Link : THL7V2ModelTableItems;
Begin
  Result := THL7V2ModelTableItems(Inherited Link);
End;

Function THL7V2ModelTableItems.Clone : THL7V2ModelTableItems;
Begin
  Result := THL7V2ModelTableItems(Inherited Clone);
End;

Function THL7V2ModelTableItems.New : THL7V2ModelTableItem;
Begin
  Result := THL7V2ModelTableItem(Inherited New);
End;

Function THL7V2ModelTableItems.ItemClass : TFslObjectClass;
Begin
  Result := THL7V2ModelTableItem;
End;

Function THL7V2ModelTableItems.GetElement(Const iIndex : Integer) : THL7V2ModelTableItem;
Begin
  Result := THL7V2ModelTableItem(ObjectByIndex[iIndex]);
End;

Procedure THL7V2ModelTableItems.SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelTableItem);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;

Function THL7V2ModelTableItems.CompareByID(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.IntegerCompare(THL7V2ModelTableItem(pA).ID, THL7V2ModelTableItem(pB).ID);
End;

Function THL7V2ModelTableItems.CompareByCode(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelTableItem(pA).Code, THL7V2ModelTableItem(pB).Code);
End;

Function THL7V2ModelTableItems.CompareByDescription(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelTableItem(pA).Description, THL7V2ModelTableItem(pB).Description);
End;

Function THL7V2ModelTableItems.IndexByID(Const aValue : THL7V2ModelTableItemID) : Integer;
Var
  oElement : THL7V2ModelTableItem;
Begin
  oElement := New;
  Try
    oElement.ID := aValue;

    If Not Find(oElement, Result, CompareByID) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelTableItems.IndexByCode(Const aValue : THL7V2ModelTableItemCode) : Integer;
Var
  oElement : THL7V2ModelTableItem;
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

Function THL7V2ModelTableItems.IndexByDescription(Const aValue : THL7V2ModelTableItemDescription) : Integer;
Var
  oElement : THL7V2ModelTableItem;
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

Function THL7V2ModelTableItems.Get(Const aValue : Integer) : THL7V2ModelTableItem;
Begin
  Result := THL7V2ModelTableItem(Inherited Get(aValue));
End;

Function THL7V2ModelTableItems.GetByID(Const aValue : THL7V2ModelTableItemID) : THL7V2ModelTableItem;
Begin
  Result := Get(IndexByID(aValue));
End;

Function THL7V2ModelTableItems.GetByCode(Const aValue : THL7V2ModelTableItemCode) : THL7V2ModelTableItem;
Begin
  Result := Get(IndexByCode(aValue));
End;

Function THL7V2ModelTableItems.GetByDescription(Const aValue : THL7V2ModelTableItemDescription) : THL7V2ModelTableItem;
Begin
  Result := Get(IndexByDescription(aValue));
End;

Function THL7V2ModelTableItems.ExistsByID(Const aValue : THL7V2ModelTableItemID) : Boolean;
Begin
  Result := ExistsByIndex(IndexByID(aValue));
End;

Function THL7V2ModelTableItems.ExistsByCode(Const aValue : THL7V2ModelTableItemCode) : Boolean;
Begin
  Result := ExistsByIndex(IndexByCode(aValue));
End;

Function THL7V2ModelTableItems.ExistsByDescription(Const aValue : THL7V2ModelTableItemDescription) : Boolean;
Begin
  Result := ExistsByIndex(IndexByDescription(aValue));
End;

Procedure THL7V2ModelTableItems.SortedByID;
Begin
  SortedBy(CompareByID);
End;

Procedure THL7V2ModelTableItems.SortedByCode;
Begin
  SortedBy(CompareByCode);
End;

Procedure THL7V2ModelTableItems.SortedByDescription;
Begin
  SortedBy(CompareByDescription);
End;

Function THL7V2ModelTableItems.IsSortedByID : Boolean;
Begin
  Result := IsSortedBy(CompareByID);
End;

Function THL7V2ModelTableItems.IsSortedByCode : Boolean;
Begin
  Result := IsSortedBy(CompareByCode);
End;

Function THL7V2ModelTableItems.IsSortedByDescription : Boolean;
Begin
  Result := IsSortedBy(CompareByDescription);
End;

function THL7V2ModelTableItems.Add(iId: Integer; const sCode, sDesc: String): THL7V2ModelTableItem;
begin
  Result := New;
  Try
    Result.ID := iId;
    Result.Code := sCode;
    Result.Description := sDesc;
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

Constructor THL7V2ModelTable.Create;
Begin
  Inherited;
  FItems := THL7V2ModelTableItems.Create;
End;

Destructor THL7V2ModelTable.Destroy;
Begin
  FItems.Free;
  Inherited;
End;

Function THL7V2ModelTable.Link : THL7V2ModelTable;
Begin
  Result := THL7V2ModelTable(Inherited Link);
End;

Function THL7V2ModelTable.Clone : THL7V2ModelTable;
Begin
  Result := THL7V2ModelTable(Inherited Clone);
End;

Procedure THL7V2ModelTable.Assign(oObject : TFslObject);
Begin
  Inherited;

  ID := THL7V2ModelTable(oObject).ID;
  Description := THL7V2ModelTable(oObject).Description;
  Items.Assign(THL7V2ModelTable(oObject).Items);
End;

function THL7V2ModelTable.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FDescription.length * sizeOf(char)) + 12);
  inc(result, FItems.sizeInBytes);
end;

Function THL7V2ModelTables.Link : THL7V2ModelTables;
Begin
  Result := THL7V2ModelTables(Inherited Link);
End;

Function THL7V2ModelTables.Clone : THL7V2ModelTables;
Begin
  Result := THL7V2ModelTables(Inherited Clone);
End;

Function THL7V2ModelTables.New : THL7V2ModelTable;
Begin
  Result := THL7V2ModelTable(Inherited New);
End;

Function THL7V2ModelTables.ItemClass : TFslObjectClass;
Begin
  Result := THL7V2ModelTable;
End;

Function THL7V2ModelTables.GetElement(Const iIndex : Integer) : THL7V2ModelTable;
Begin
  Result := THL7V2ModelTable(ObjectByIndex[iIndex]);
End;

Procedure THL7V2ModelTables.SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelTable);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;

Function THL7V2ModelTables.CompareByID(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.IntegerCompare(THL7V2ModelTable(pA).ID, THL7V2ModelTable(pB).ID);
End;

Function THL7V2ModelTables.CompareByDescription(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelTable(pA).Description, THL7V2ModelTable(pB).Description);
End;

Function THL7V2ModelTables.IndexByID(Const aValue : THL7V2ModelTableID) : Integer;
Var
  oElement : THL7V2ModelTable;
Begin
  oElement := New;
  Try
    oElement.ID := aValue;

    If Not Find(oElement, Result, CompareByID) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelTables.IndexByDescription(Const aValue : THL7V2ModelTableDescription) : Integer;
Var
  oElement : THL7V2ModelTable;
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

Function THL7V2ModelTables.Get(Const aValue : Integer) : THL7V2ModelTable;
Begin
  Result := THL7V2ModelTable(Inherited Get(aValue));
End;

Function THL7V2ModelTables.GetByID(Const aValue : THL7V2ModelTableID) : THL7V2ModelTable;
Begin
  Result := Get(IndexByID(aValue));
End;

Function THL7V2ModelTables.GetByDescription(Const aValue : THL7V2ModelTableDescription) : THL7V2ModelTable;
Begin
  Result := Get(IndexByDescription(aValue));
End;

Function THL7V2ModelTables.ExistsByID(Const aValue : THL7V2ModelTableID) : Boolean;
Begin
  Result := ExistsByIndex(IndexByID(aValue));
End;

Function THL7V2ModelTables.ExistsByDescription(Const aValue : THL7V2ModelTableDescription) : Boolean;
Begin
  Result := ExistsByIndex(IndexByDescription(aValue));
End;

Procedure THL7V2ModelTables.SortedByID;
Begin
  SortedBy(CompareByID);
End;

Procedure THL7V2ModelTables.SortedByDescription;
Begin
  SortedBy(CompareByDescription);
End;

Function THL7V2ModelTables.IsSortedByID : Boolean;
Begin
  Result := IsSortedBy(CompareByID);
End;

Function THL7V2ModelTables.IsSortedByDescription : Boolean;
Begin
  Result := IsSortedBy(CompareByDescription);
End;

Procedure THL7V2ModelTable.SetItems(Const Value: THL7V2ModelTableItems);
Begin
  FItems.Free;
  FItems := Value;
End;

function THL7V2ModelTables.Add(iId: Integer; const sDesc: String): THL7V2ModelTable;
begin
  Result := New;
  Try
    Result.ID := iId;
    Result.Description := sDesc;
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function THL7V2ModelTable.CodesAsString: String;
var
  oBuilder : TFslStringBuilder;
  iLoop : Integer;
begin
  oBuilder  := TFslStringBuilder.Create;
  Try
    if FItems.Count = 0 Then
      Result := 'Table '+IntToStr(FID)+': (none)'
    Else
    Begin
      oBuilder.Append('Table '+IntToStr(FID)+': '+FItems[0].Code);
      For iLoop := 1 to FItems.count - 1 Do
        oBuilder.Append(', '+FItems[iLoop].Code);
      Result := oBuilder.AsString;
    End;
  Finally
    oBuilder.Free;
  End;
end;

Constructor THL7V2ModelComponent.Create;
Begin
  Inherited;
End;

Destructor THL7V2ModelComponent.Destroy;
Begin
  FRefTable.Free;
  FRefDataType.Free;
  Inherited;
End;

Function THL7V2ModelComponent.Link : THL7V2ModelComponent;
Begin
  Result := THL7V2ModelComponent(Inherited Link);
End;

Function THL7V2ModelComponent.Clone : THL7V2ModelComponent;
Begin
  Result := THL7V2ModelComponent(Inherited Clone);
End;

Procedure THL7V2ModelComponent.Assign(oObject : TFslObject);
Begin
  Inherited;

  Name := THL7V2ModelComponent(oObject).Name;
  DataType := THL7V2ModelComponent(oObject).DataType;
  Table := THL7V2ModelComponent(oObject).Table;
  Number := THL7V2ModelComponent(oObject).Number;
End;

function THL7V2ModelComponent.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeOf(char)) + 12);
  inc(result, (FDataType.length * sizeOf(char)) + 12);
  inc(result, FRefTable.sizeInBytes);
  inc(result, FRefDataType.sizeInBytes);
  inc(result, FRefStructure.sizeInBytes);
end;

Function THL7V2ModelComponents.Link : THL7V2ModelComponents;
Begin
  Result := THL7V2ModelComponents(Inherited Link);
End;

Function THL7V2ModelComponents.Clone : THL7V2ModelComponents;
Begin
  Result := THL7V2ModelComponents(Inherited Clone);
End;

Function THL7V2ModelComponents.New : THL7V2ModelComponent;
Begin
  Result := THL7V2ModelComponent(Inherited New);
End;

Function THL7V2ModelComponents.ItemClass : TFslObjectClass;
Begin
  Result := THL7V2ModelComponent;
End;

Function THL7V2ModelComponents.GetElement(Const iIndex : Integer) : THL7V2ModelComponent;
Begin
  Result := THL7V2ModelComponent(ObjectByIndex[iIndex]);
End;

Procedure THL7V2ModelComponents.SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelComponent);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;

Function THL7V2ModelComponents.CompareByName(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelComponent(pA).Name, THL7V2ModelComponent(pB).Name);
End;

Function THL7V2ModelComponents.CompareByNumber(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.IntegerCompare(THL7V2ModelComponent(pA).Number, THL7V2ModelComponent(pB).Number);
End;

Function THL7V2ModelComponents.CompareByDataType(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelComponent(pA).DataType, THL7V2ModelComponent(pB).DataType);
End;

Function THL7V2ModelComponents.CompareByTable(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.IntegerCompare(THL7V2ModelComponent(pA).Table, THL7V2ModelComponent(pB).Table);
End;

Function THL7V2ModelComponents.IndexByName(Const aValue : THL7V2ModelComponentName) : Integer;
Var
  oElement : THL7V2ModelComponent;
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

Function THL7V2ModelComponents.IndexByNumber(Const aValue : THL7V2ModelComponentNumber) : Integer;
Var
  oElement : THL7V2ModelComponent;
Begin
  oElement := New;
  Try
    oElement.Number := aValue;

    If Not Find(oElement, Result, CompareByNumber) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelComponents.IndexByDataType(Const aValue : THL7V2ModelComponentDataType) : Integer;
Var
  oElement : THL7V2ModelComponent;
Begin
  oElement := New;
  Try
    oElement.DataType := aValue;

    If Not Find(oElement, Result, CompareByDataType) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelComponents.IndexByTable(Const aValue : THL7V2ModelComponentTable) : Integer;
Var
  oElement : THL7V2ModelComponent;
Begin
  oElement := New;
  Try
    oElement.Table := aValue;

    If Not Find(oElement, Result, CompareByTable) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelComponents.Get(Const aValue : Integer) : THL7V2ModelComponent;
Begin
  Result := THL7V2ModelComponent(Inherited Get(aValue));
End;

Function THL7V2ModelComponents.GetByName(Const aValue : THL7V2ModelComponentName) : THL7V2ModelComponent;
Begin
  Result := Get(IndexByName(aValue));
End;

Function THL7V2ModelComponents.GetByNumber(Const aValue : THL7V2ModelComponentNumber) : THL7V2ModelComponent;
Begin
  Result := Get(IndexByNumber(aValue));
End;

Function THL7V2ModelComponents.GetByDataType(Const aValue : THL7V2ModelComponentDataType) : THL7V2ModelComponent;
Begin
  Result := Get(IndexByDataType(aValue));
End;

Function THL7V2ModelComponents.GetByTable(Const aValue : THL7V2ModelComponentTable) : THL7V2ModelComponent;
Begin
  Result := Get(IndexByTable(aValue));
End;

Function THL7V2ModelComponents.ExistsByName(Const aValue : THL7V2ModelComponentName) : Boolean;
Begin
  Result := ExistsByIndex(IndexByName(aValue));
End;

Function THL7V2ModelComponents.ExistsByNumber(Const aValue : THL7V2ModelComponentNumber) : Boolean;
Begin
  Result := ExistsByIndex(IndexByNumber(aValue));
End;

Function THL7V2ModelComponents.ExistsByDataType(Const aValue : THL7V2ModelComponentDataType) : Boolean;
Begin
  Result := ExistsByIndex(IndexByDataType(aValue));
End;

Function THL7V2ModelComponents.ExistsByTable(Const aValue : THL7V2ModelComponentTable) : Boolean;
Begin
  Result := ExistsByIndex(IndexByTable(aValue));
End;

Procedure THL7V2ModelComponents.SortedByName;
Begin
  SortedBy(CompareByName);
End;

Procedure THL7V2ModelComponents.SortedByNumber;
Begin
  SortedBy(CompareByNumber);
End;

Procedure THL7V2ModelComponents.SortedByDataType;
Begin
  SortedBy(CompareByDataType);
End;

Procedure THL7V2ModelComponents.SortedByTable;
Begin
  SortedBy(CompareByTable);
End;

Function THL7V2ModelComponents.IsSortedByName : Boolean;
Begin
  Result := IsSortedBy(CompareByName);
End;

Function THL7V2ModelComponents.IsSortedByNumber : Boolean;
Begin
  Result := IsSortedBy(CompareByNumber);
End;

Function THL7V2ModelComponents.IsSortedByDataType : Boolean;
Begin
  Result := IsSortedBy(CompareByDataType);
End;

Function THL7V2ModelComponents.IsSortedByTable : Boolean;
Begin
  Result := IsSortedBy(CompareByTable);
End;

Procedure THL7V2ModelComponent.SetRefTable(Const Value: THL7V2ModelTable);
Begin
  FRefTable.Free;
  FRefTable := Value;
End;

Procedure THL7V2ModelComponent.SetRefDataType(Const Value: THL7V2ModelDataType);
Begin
  FRefDataType.Free;
  FRefDataType := Value;
End;

function THL7V2ModelComponents.Add(const sName, sDataType: String; iTable, iNumber: Integer): THL7V2ModelComponent;
begin
  Result := New;
  Try
    Result.Name := sName;
    Result.Datatype := sDatatype;
    Result.Table := iTable;
    Result.Number := iNumber;
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

Constructor THL7V2ModelStructure.Create;
Begin
  Inherited;
  FComponents := THL7V2ModelComponents.Create;
End;

Destructor THL7V2ModelStructure.Destroy;
Begin
  FComponents.Free;
  FRefDataType.Free;
  Inherited;
End;

Function THL7V2ModelStructure.Link : THL7V2ModelStructure;
Begin
  Result := THL7V2ModelStructure(Inherited Link);
End;

Function THL7V2ModelStructure.Clone : THL7V2ModelStructure;
Begin
  Result := THL7V2ModelStructure(Inherited Clone);
End;

Procedure THL7V2ModelStructure.Assign(oObject : TFslObject);
Begin
  Inherited;

  Name := THL7V2ModelStructure(oObject).Name;
  Description := THL7V2ModelStructure(oObject).Description;
  DataType := THL7V2ModelStructure(oObject).DataType;
  ID := THL7V2ModelStructure(oObject).ID;
  Components.Assign(THL7V2ModelStructure(oObject).Components);
End;

function THL7V2ModelStructure.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeOf(char)) + 12);
  inc(result, (FDescription.length * sizeOf(char)) + 12);
  inc(result, (FDataType.length * sizeOf(char)) + 12);
  inc(result, FRefDataType.sizeInBytes);
  inc(result, FComponents.sizeInBytes);
end;

Function THL7V2ModelStructures.Link : THL7V2ModelStructures;
Begin
  Result := THL7V2ModelStructures(Inherited Link);
End;

Function THL7V2ModelStructures.Clone : THL7V2ModelStructures;
Begin
  Result := THL7V2ModelStructures(Inherited Clone);
End;

Function THL7V2ModelStructures.New : THL7V2ModelStructure;
Begin
  Result := THL7V2ModelStructure(Inherited New);
End;

Function THL7V2ModelStructures.ItemClass : TFslObjectClass;
Begin
  Result := THL7V2ModelStructure;
End;

Function THL7V2ModelStructures.GetElement(Const iIndex : Integer) : THL7V2ModelStructure;
Begin
  Result := THL7V2ModelStructure(ObjectByIndex[iIndex]);
End;

Procedure THL7V2ModelStructures.SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelStructure);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;

Function THL7V2ModelStructures.CompareByName(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelStructure(pA).Name, THL7V2ModelStructure(pB).Name);
End;

Function THL7V2ModelStructures.CompareByDescription(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelStructure(pA).Description, THL7V2ModelStructure(pB).Description);
End;

Function THL7V2ModelStructures.CompareByDataType(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelStructure(pA).DataType, THL7V2ModelStructure(pB).DataType);
End;

Function THL7V2ModelStructures.CompareByID(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.IntegerCompare(THL7V2ModelStructure(pA).ID, THL7V2ModelStructure(pB).ID);
End;

Function THL7V2ModelStructures.IndexByName(Const aValue : THL7V2ModelStructureName) : Integer;
Var
  oElement : THL7V2ModelStructure;
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

Function THL7V2ModelStructures.IndexByDescription(Const aValue : THL7V2ModelStructureDescription) : Integer;
Var
  oElement : THL7V2ModelStructure;
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

Function THL7V2ModelStructures.IndexByDataType(Const aValue : THL7V2ModelStructureDataType) : Integer;
Var
  oElement : THL7V2ModelStructure;
Begin
  oElement := New;
  Try
    oElement.DataType := aValue;

    If Not Find(oElement, Result, CompareByDataType) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelStructures.IndexByID(Const aValue : THL7V2ModelStructureID) : Integer;
Var
  oElement : THL7V2ModelStructure;
Begin
  oElement := New;
  Try
    oElement.ID := aValue;

    If Not Find(oElement, Result, CompareByID) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelStructures.Get(Const aValue : Integer) : THL7V2ModelStructure;
Begin
  Result := THL7V2ModelStructure(Inherited Get(aValue));
End;

Function THL7V2ModelStructures.GetByName(Const aValue : THL7V2ModelStructureName) : THL7V2ModelStructure;
Begin
  Result := Get(IndexByName(aValue));
End;

Function THL7V2ModelStructures.GetByDescription(Const aValue : THL7V2ModelStructureDescription) : THL7V2ModelStructure;
Begin
  Result := Get(IndexByDescription(aValue));
End;

Function THL7V2ModelStructures.GetByDataType(Const aValue : THL7V2ModelStructureDataType) : THL7V2ModelStructure;
Begin
  Result := Get(IndexByDataType(aValue));
End;

Function THL7V2ModelStructures.GetByID(Const aValue : THL7V2ModelStructureID) : THL7V2ModelStructure;
Begin
  Result := Get(IndexByID(aValue));
End;

Function THL7V2ModelStructures.ExistsByName(Const aValue : THL7V2ModelStructureName) : Boolean;
Begin
  Result := ExistsByIndex(IndexByName(aValue));
End;

Function THL7V2ModelStructures.ExistsByDescription(Const aValue : THL7V2ModelStructureDescription) : Boolean;
Begin
  Result := ExistsByIndex(IndexByDescription(aValue));
End;

Function THL7V2ModelStructures.ExistsByDataType(Const aValue : THL7V2ModelStructureDataType) : Boolean;
Begin
  Result := ExistsByIndex(IndexByDataType(aValue));
End;

Function THL7V2ModelStructures.ExistsByID(Const aValue : THL7V2ModelStructureID) : Boolean;
Begin
  Result := ExistsByIndex(IndexByID(aValue));
End;

Procedure THL7V2ModelStructures.SortedByName;
Begin
  SortedBy(CompareByName);
End;

Procedure THL7V2ModelStructures.SortedByDescription;
Begin
  SortedBy(CompareByDescription);
End;

Procedure THL7V2ModelStructures.SortedByDataType;
Begin
  SortedBy(CompareByDataType);
End;

Procedure THL7V2ModelStructures.SortedByID;
Begin
  SortedBy(CompareByID);
End;

Function THL7V2ModelStructures.IsSortedByName : Boolean;
Begin
  Result := IsSortedBy(CompareByName);
End;

Function THL7V2ModelStructures.IsSortedByDescription : Boolean;
Begin
  Result := IsSortedBy(CompareByDescription);
End;

Function THL7V2ModelStructures.IsSortedByDataType : Boolean;
Begin
  Result := IsSortedBy(CompareByDataType);
End;

Function THL7V2ModelStructures.IsSortedByID : Boolean;
Begin
  Result := IsSortedBy(CompareByID);
End;

Procedure THL7V2ModelStructure.SetRefDataType(Const Value: THL7V2ModelDataType);
Begin
  FRefDataType.Free;
  FRefDataType := Value;

End;

Procedure THL7V2ModelStructure.SetComponents(Const Value: THL7V2ModelComponents);
Begin
  FComponents.Free;
  FComponents := Value;
End;

function THL7V2ModelStructures.Add(const sName, sDesc, sDatatype: String;
  iId: Integer): THL7V2ModelStructure;
begin
  Result := New;
  Try
    Result.Name := sName;
    Result.Description := sDesc;
    Result.Datatype := sDatatype;
    Result.Id := iId;
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

Constructor THL7V2ModelDataElement.Create;
Begin
  Inherited;
End;

Destructor THL7V2ModelDataElement.Destroy;
Begin
  FRefTable.Free;
  FRefStructure.Free;
  Inherited;
End;

Function THL7V2ModelDataElement.Link : THL7V2ModelDataElement;
Begin
  Result := THL7V2ModelDataElement(Inherited Link);
End;

Function THL7V2ModelDataElement.Clone : THL7V2ModelDataElement;
Begin
  Result := THL7V2ModelDataElement(Inherited Clone);
End;

Procedure THL7V2ModelDataElement.Assign(oObject : TFslObject);
Begin
  Inherited;

  Description := THL7V2ModelDataElement(oObject).Description;
  Id := THL7V2ModelDataElement(oObject).Id;
  Structure := THL7V2ModelDataElement(oObject).Structure;
  Length_Old := THL7V2ModelDataElement(oObject).Length_Old;
  Length_Min := THL7V2ModelDataElement(oObject).Length_Min;
  Length_Max := THL7V2ModelDataElement(oObject).Length_Max;
  Length_Conf := THL7V2ModelDataElement(oObject).Length_Conf;
  Table := THL7V2ModelDataElement(oObject).Table;
End;

function THL7V2ModelDataElement.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FDescription.length * sizeOf(char)) + 12);
  inc(result, (FStructure.length * sizeOf(char)) + 12);
  inc(result, (FLength_Conf.length * sizeof(char)) + 12);
  inc(result, FRefTable.sizeInBytes);
  inc(result, FRefStructure.sizeInBytes);
end;

Function THL7V2ModelDataElements.Link : THL7V2ModelDataElements;
Begin
  Result := THL7V2ModelDataElements(Inherited Link);
End;

Function THL7V2ModelDataElements.Clone : THL7V2ModelDataElements;
Begin
  Result := THL7V2ModelDataElements(Inherited Clone);
End;

Function THL7V2ModelDataElements.New : THL7V2ModelDataElement;
Begin
  Result := THL7V2ModelDataElement(Inherited New);
End;

Function THL7V2ModelDataElements.ItemClass : TFslObjectClass;
Begin
  Result := THL7V2ModelDataElement;
End;

Function THL7V2ModelDataElements.GetElement(Const iIndex : Integer) : THL7V2ModelDataElement;
Begin
  Result := THL7V2ModelDataElement(ObjectByIndex[iIndex]);
End;

Procedure THL7V2ModelDataElements.SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelDataElement);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;

Function THL7V2ModelDataElements.CompareByDescription(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelDataElement(pA).Description, THL7V2ModelDataElement(pB).Description);
End;

Function THL7V2ModelDataElements.CompareById(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.IntegerCompare(THL7V2ModelDataElement(pA).Id, THL7V2ModelDataElement(pB).Id);
End;

Function THL7V2ModelDataElements.CompareByStructure(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelDataElement(pA).Structure, THL7V2ModelDataElement(pB).Structure);
End;

Function THL7V2ModelDataElements.CompareByLength(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.IntegerCompare(THL7V2ModelDataElement(pA).Length_Old, THL7V2ModelDataElement(pB).Length_Old);
End;

Function THL7V2ModelDataElements.CompareByTable(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.IntegerCompare(THL7V2ModelDataElement(pA).Table, THL7V2ModelDataElement(pB).Table);
End;

Function THL7V2ModelDataElements.IndexByDescription(Const aValue : THL7V2ModelDataElementDescription) : Integer;
Var
  oElement : THL7V2ModelDataElement;
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

Function THL7V2ModelDataElements.IndexById(Const aValue : THL7V2ModelDataElementId) : Integer;
Var
  oElement : THL7V2ModelDataElement;
Begin
  oElement := New;
  Try
    oElement.Id := aValue;

    If Not Find(oElement, Result, CompareById) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelDataElements.IndexByStructure(Const aValue : THL7V2ModelDataElementStructure) : Integer;
Var
  oElement : THL7V2ModelDataElement;
Begin
  oElement := New;
  Try
    oElement.Structure := aValue;

    If Not Find(oElement, Result, CompareByStructure) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelDataElements.IndexByLength(Const aValue : Integer) : Integer;
Var
  oElement : THL7V2ModelDataElement;
Begin
  oElement := New;
  Try
    oElement.Length_Old := aValue;

    If Not Find(oElement, Result, CompareByLength) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelDataElements.IndexByTable(Const aValue : THL7V2ModelDataElementTable) : Integer;
Var
  oElement : THL7V2ModelDataElement;
Begin
  oElement := New;
  Try
    oElement.Table := aValue;

    If Not Find(oElement, Result, CompareByTable) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelDataElements.Get(Const aValue : Integer) : THL7V2ModelDataElement;
Begin
  Result := THL7V2ModelDataElement(Inherited Get(aValue));
End;

Function THL7V2ModelDataElements.GetByDescription(Const aValue : THL7V2ModelDataElementDescription) : THL7V2ModelDataElement;
Begin
  Result := Get(IndexByDescription(aValue));
End;

Function THL7V2ModelDataElements.GetById(Const aValue : THL7V2ModelDataElementId) : THL7V2ModelDataElement;
Begin
  Result := Get(IndexById(aValue));
End;

Function THL7V2ModelDataElements.GetByStructure(Const aValue : THL7V2ModelDataElementStructure) : THL7V2ModelDataElement;
Begin
  Result := Get(IndexByStructure(aValue));
End;

Function THL7V2ModelDataElements.GetByLength(Const aValue : Integer) : THL7V2ModelDataElement;
Begin
  Result := Get(IndexByLength(aValue));
End;

Function THL7V2ModelDataElements.GetByTable(Const aValue : THL7V2ModelDataElementTable) : THL7V2ModelDataElement;
Begin
  Result := Get(IndexByTable(aValue));
End;

Function THL7V2ModelDataElements.ExistsByDescription(Const aValue : THL7V2ModelDataElementDescription) : Boolean;
Begin
  Result := ExistsByIndex(IndexByDescription(aValue));
End;

Function THL7V2ModelDataElements.ExistsById(Const aValue : THL7V2ModelDataElementId) : Boolean;
Begin
  Result := ExistsByIndex(IndexById(aValue));
End;

Function THL7V2ModelDataElements.ExistsByStructure(Const aValue : THL7V2ModelDataElementStructure) : Boolean;
Begin
  Result := ExistsByIndex(IndexByStructure(aValue));
End;

Function THL7V2ModelDataElements.ExistsByLength(Const aValue : Integer) : Boolean;
Begin
  Result := ExistsByIndex(IndexByLength(aValue));
End;

Function THL7V2ModelDataElements.ExistsByTable(Const aValue : THL7V2ModelDataElementTable) : Boolean;
Begin
  Result := ExistsByIndex(IndexByTable(aValue));
End;

Procedure THL7V2ModelDataElements.SortedByDescription;
Begin
  SortedBy(CompareByDescription);
End;

Procedure THL7V2ModelDataElements.SortedById;
Begin
  SortedBy(CompareById);
End;

Procedure THL7V2ModelDataElements.SortedByStructure;
Begin
  SortedBy(CompareByStructure);
End;

Procedure THL7V2ModelDataElements.SortedByLength;
Begin
  SortedBy(CompareByLength);
End;

Procedure THL7V2ModelDataElements.SortedByTable;
Begin
  SortedBy(CompareByTable);
End;

Function THL7V2ModelDataElements.IsSortedByDescription : Boolean;
Begin
  Result := IsSortedBy(CompareByDescription);
End;

Function THL7V2ModelDataElements.IsSortedById : Boolean;
Begin
  Result := IsSortedBy(CompareById);
End;

Function THL7V2ModelDataElements.IsSortedByStructure : Boolean;
Begin
  Result := IsSortedBy(CompareByStructure);
End;

Function THL7V2ModelDataElements.IsSortedByLength : Boolean;
Begin
  Result := IsSortedBy(CompareByLength);
End;

Function THL7V2ModelDataElements.IsSortedByTable : Boolean;
Begin
  Result := IsSortedBy(CompareByTable);
End;

Procedure THL7V2ModelDataElement.SetRefTable(Const Value: THL7V2ModelTable);
Begin
  FRefTable.Free;
  FRefTable := Value;
End;

Procedure THL7V2ModelDataElement.SetRefStructure(Const Value: THL7V2ModelStructure);
Begin
  FRefStructure.Free;
  FRefStructure := Value;
End;

function THL7V2ModelDataElements.Add(const sDesc: String; iId: Integer; const sStructure: String; iLength, iTable: Integer): THL7V2ModelDataElement;
begin
  Result := New;
  Try
    Result.Description := sDesc;
    Result.Id := iId;
    Result.Structure := sStructure;
    Result.Length_Old := iLength;
    Result.Table := iTable;
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

Function THL7V2ModelDataElements.Add(const sDesc : String; iId : Integer; Const sStructure : String; iLengthMin, iLengthMax : integer; sLengthConf : String; iTable : Integer) : THL7V2ModelDataElement;
begin
  Result := New;
  Try
    Result.Description := sDesc;
    Result.Id := iId;
    Result.Structure := sStructure;
    Result.Length_Min := iLengthMin;
    Result.Length_Max := iLengthMax;
    Result.Length_Conf := sLengthConf;
    Result.Table := iTable;
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function THL7V2ModelDataElement.getLengthDesc: String;
begin
  if FLength_Old > 0 then
    result := inttostr(FLength_Old)
  else if FLength_Max > 0 then
    result := inttostr(FLength_Min)+'-'+inttostr(FLength_Max)
  else
    result := '';
  if FLength_Conf > '' then
    result := result + FLength_Conf;
end;

function THL7V2ModelDataElement.HasLength: Boolean;
begin
  result := (FLength_Old > 0) or (FLength_Max > 0) or (FLength_Conf > '');
end;

Constructor THL7V2ModelField.Create;
Begin
  Inherited;
End;

Destructor THL7V2ModelField.Destroy;
Begin
  FRefDataElement.Free;
  Inherited;
End;

Function THL7V2ModelField.Link : THL7V2ModelField;
Begin
  Result := THL7V2ModelField(Inherited Link);
End;

Function THL7V2ModelField.Clone : THL7V2ModelField;
Begin
  Result := THL7V2ModelField(Inherited Clone);
End;

Procedure THL7V2ModelField.Assign(oObject : TFslObject);
Begin
  Inherited;

  DataElement := THL7V2ModelField(oObject).DataElement;
  Repeatable := THL7V2ModelField(oObject).Repeatable;
  RepeatCount := THL7V2ModelField(oObject).RepeatCount;
  Required := THL7V2ModelField(oObject).Required;
  FieldNumber := THL7V2ModelField(oObject).FieldNumber;
End;

Function THL7V2ModelFields.Link : THL7V2ModelFields;
Begin
  Result := THL7V2ModelFields(Inherited Link);
End;

Function THL7V2ModelFields.Clone : THL7V2ModelFields;
Begin
  Result := THL7V2ModelFields(Inherited Clone);
End;

Function THL7V2ModelFields.New : THL7V2ModelField;
Begin
  Result := THL7V2ModelField(Inherited New);
End;

Function THL7V2ModelFields.ItemClass : TFslObjectClass;
Begin
  Result := THL7V2ModelField;
End;

Function THL7V2ModelFields.GetElement(Const iIndex : Integer) : THL7V2ModelField;
Begin
  Result := THL7V2ModelField(ObjectByIndex[iIndex]);
End;

Procedure THL7V2ModelFields.SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelField);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;

Function THL7V2ModelFields.CompareByDataElement(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.IntegerCompare(THL7V2ModelField(pA).DataElement, THL7V2ModelField(pB).DataElement);
End;

Function THL7V2ModelFields.CompareByRepeatable(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.BooleanCompare(THL7V2ModelField(pA).Repeatable, THL7V2ModelField(pB).Repeatable);
End;

Function THL7V2ModelFields.CompareByRepeatCount(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.IntegerCompare(THL7V2ModelField(pA).RepeatCount, THL7V2ModelField(pB).RepeatCount);
End;

Function THL7V2ModelFields.CompareByRequired(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.BooleanCompare(THL7V2ModelField(pA).Required, THL7V2ModelField(pB).Required);
End;

Function THL7V2ModelFields.CompareByFieldNumber(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.IntegerCompare(THL7V2ModelField(pA).FieldNumber, THL7V2ModelField(pB).FieldNumber);
End;

Function THL7V2ModelFields.IndexByDataElement(Const aValue : THL7V2ModelFieldDataElement) : Integer;
Var
  oElement : THL7V2ModelField;
Begin
  oElement := New;
  Try
    oElement.DataElement := aValue;

    If Not Find(oElement, Result, CompareByDataElement) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelFields.IndexByFieldNumber(Const aValue : THL7V2ModelFieldFieldNumber) : Integer;
Var
  oElement : THL7V2ModelField;
Begin
  oElement := New;
  Try
    oElement.FieldNumber := aValue;

    If Not Find(oElement, Result, CompareByFieldNumber) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelFields.Get(Const aValue : Integer) : THL7V2ModelField;
Begin
  Result := THL7V2ModelField(Inherited Get(aValue));
End;

Function THL7V2ModelFields.GetByDataElement(Const aValue : THL7V2ModelFieldDataElement) : THL7V2ModelField;
Begin
  Result := Get(IndexByDataElement(aValue));
End;

Function THL7V2ModelFields.GetByFieldNumber(Const aValue : THL7V2ModelFieldFieldNumber) : THL7V2ModelField;
Begin
  Result := Get(IndexByFieldNumber(aValue));
End;

Function THL7V2ModelFields.ExistsByDataElement(Const aValue : THL7V2ModelFieldDataElement) : Boolean;
Begin
  Result := ExistsByIndex(IndexByDataElement(aValue));
End;

Function THL7V2ModelFields.ExistsByFieldNumber(Const aValue : THL7V2ModelFieldFieldNumber) : Boolean;
Begin
  Result := ExistsByIndex(IndexByFieldNumber(aValue));
End;

Procedure THL7V2ModelFields.SortedByDataElement;
Begin
  SortedBy(CompareByDataElement);
End;

Procedure THL7V2ModelFields.SortedByFieldNumber;
Begin
  SortedBy(CompareByFieldNumber);
End;

Function THL7V2ModelFields.IsSortedByDataElement : Boolean;
Begin
  Result := IsSortedBy(CompareByDataElement);
End;

Function THL7V2ModelFields.IsSortedByFieldNumber : Boolean;
Begin
  Result := IsSortedBy(CompareByFieldNumber);
End;

Procedure THL7V2ModelField.SetRefDataElement(Const Value: THL7V2ModelDataElement);
Begin
  FRefDataElement.Free;
  FRefDataElement := Value;
End;

function THL7V2ModelFields.Add(iDataElement: THL7V2ModelFieldDataElement;
  bRepeatable: THL7V2ModelFieldRepeatable;
  iRepeatCount: THL7V2ModelFieldRepeatCount;
  bRequired: THL7V2ModelFieldRequired;
  iFieldNumber: THL7V2ModelFieldFieldNumber): THL7V2ModelField;
begin
  Result := New;
  try
    Result.DataElement := iDataElement;
    Result.Repeatable := bRepeatable;
    Result.RepeatCount := iRepeatCount;
    Result.Required := bRequired;
    Result.FieldNumber := iFieldNumber;
    Add(Result.Link);
  Finally
    result.Free;
  End;
end;

Constructor THL7V2ModelSegment.Create;
Begin
  Inherited;
  FFields := THL7V2ModelFields.Create;
End;

Destructor THL7V2ModelSegment.Destroy;
Begin
  FFields.Free;
  Inherited;
End;

Function THL7V2ModelSegment.Link : THL7V2ModelSegment;
Begin
  Result := THL7V2ModelSegment(Inherited Link);
End;

Function THL7V2ModelSegment.Clone : THL7V2ModelSegment;
Begin
  Result := THL7V2ModelSegment(Inherited Clone);
End;

Procedure THL7V2ModelSegment.Assign(oObject : TFslObject);
Begin
  Inherited;

  Code := THL7V2ModelSegment(oObject).Code;
  Description := THL7V2ModelSegment(oObject).Description;
  Fields.Assign(THL7V2ModelSegment(oObject).FFields);
End;

function THL7V2ModelSegment.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeOf(char)) + 12);
  inc(result, (FDescription.length * sizeOf(char)) + 12);
  inc(result, FFields.sizeInBytes);
end;

Function THL7V2ModelSegments.Link : THL7V2ModelSegments;
Begin
  Result := THL7V2ModelSegments(Inherited Link);
End;

Function THL7V2ModelSegments.Clone : THL7V2ModelSegments;
Begin
  Result := THL7V2ModelSegments(Inherited Clone);
End;

Function THL7V2ModelSegments.New : THL7V2ModelSegment;
Begin
  Result := THL7V2ModelSegment(Inherited New);
End;

Function THL7V2ModelSegments.ItemClass : TFslObjectClass;
Begin
  Result := THL7V2ModelSegment;
End;

Function THL7V2ModelSegments.GetElement(Const iIndex : Integer) : THL7V2ModelSegment;
Begin
  Result := THL7V2ModelSegment(ObjectByIndex[iIndex]);
End;

Procedure THL7V2ModelSegments.SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelSegment);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;

Function THL7V2ModelSegments.CompareByCode(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelSegment(pA).Code, THL7V2ModelSegment(pB).Code);
End;

Function THL7V2ModelSegments.CompareByDescription(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelSegment(pA).Description, THL7V2ModelSegment(pB).Description);
End;

Function THL7V2ModelSegments.IndexByCode(Const aValue : THL7V2ModelSegmentCode) : Integer;
Var
  oElement : THL7V2ModelSegment;
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

Function THL7V2ModelSegments.IndexByDescription(Const aValue : THL7V2ModelSegmentDescription) : Integer;
Var
  oElement : THL7V2ModelSegment;
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

Function THL7V2ModelSegments.Get(Const aValue : Integer) : THL7V2ModelSegment;
Begin
  Result := THL7V2ModelSegment(Inherited Get(aValue));
End;

Function THL7V2ModelSegments.GetByCode(Const aValue : THL7V2ModelSegmentCode) : THL7V2ModelSegment;
Begin
  Result := Get(IndexByCode(aValue));
End;

Function THL7V2ModelSegments.GetByDescription(Const aValue : THL7V2ModelSegmentDescription) : THL7V2ModelSegment;
Begin
  Result := Get(IndexByDescription(aValue));
End;

Function THL7V2ModelSegments.ExistsByCode(Const aValue : THL7V2ModelSegmentCode) : Boolean;
Begin
  Result := ExistsByIndex(IndexByCode(aValue));
End;

Function THL7V2ModelSegments.ExistsByDescription(Const aValue : THL7V2ModelSegmentDescription) : Boolean;
Begin
  Result := ExistsByIndex(IndexByDescription(aValue));
End;

Procedure THL7V2ModelSegments.SortedByCode;
Begin
  SortedBy(CompareByCode);
End;

Procedure THL7V2ModelSegments.SortedByDescription;
Begin
  SortedBy(CompareByDescription);
End;

Function THL7V2ModelSegments.IsSortedByCode : Boolean;
Begin
  Result := IsSortedBy(CompareByCode);
End;

Function THL7V2ModelSegments.IsSortedByDescription : Boolean;
Begin
  Result := IsSortedBy(CompareByDescription);
End;

Procedure THL7V2ModelSegment.SetFields(Const Value: THL7V2ModelFields);
Begin
  FFields.Free;
  FFields := Value;
End;

function THL7V2ModelSegments.Add(const sCode, sDesc: String): THL7V2ModelSegment;
begin
  Result := New;
  try
    Result.Code := sCode;
    Result.Description := sDesc;
    Add(Result.Link);
  Finally
    result.Free;
  End;
end;

Constructor THL7V2ModelSegmentGroup.Create;
Begin
  Inherited;
  FChildren := THL7V2ModelSegmentGroups.Create;
End;

Constructor THL7V2ModelSegmentGroup.Create(Const sCode : String; bOptional, bRepeating : Boolean; aType : THL7V2ModelSegmentGroupType);
Begin
  Create;
  Code := sCode;
  Optional := bOptional;
  Repeating := bRepeating;
  GroupType := aType;
End;

Destructor THL7V2ModelSegmentGroup.Destroy;
Begin
  FChildren.Free;
  Inherited;
End;

Function THL7V2ModelSegmentGroup.Link : THL7V2ModelSegmentGroup;
Begin
  Result := THL7V2ModelSegmentGroup(Inherited Link);
End;

Function THL7V2ModelSegmentGroup.Clone : THL7V2ModelSegmentGroup;
Begin
  Result := THL7V2ModelSegmentGroup(Inherited Clone);
End;

Procedure THL7V2ModelSegmentGroup.Assign(oObject : TFslObject);
Begin
  Inherited;

  Code := THL7V2ModelSegmentGroup(oObject).Code;
  Optional := THL7V2ModelSegmentGroup(oObject).Optional;
  Repeating := THL7V2ModelSegmentGroup(oObject).Repeating;
  GroupType := THL7V2ModelSegmentGroup(oObject).GroupType;
  Children := THL7V2ModelSegmentGroup(oObject).Children.Clone;
End;

function THL7V2ModelSegmentGroup.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeOf(char)) + 12);
  inc(result, FChildren.sizeInBytes);
end;

Function THL7V2ModelSegmentGroups.Link : THL7V2ModelSegmentGroups;
Begin
  Result := THL7V2ModelSegmentGroups(Inherited Link);
End;

Function THL7V2ModelSegmentGroups.Clone : THL7V2ModelSegmentGroups;
Begin
  Result := THL7V2ModelSegmentGroups(Inherited Clone);
End;

Function THL7V2ModelSegmentGroups.New : THL7V2ModelSegmentGroup;
Begin
  Result := THL7V2ModelSegmentGroup(Inherited New);
End;

Function THL7V2ModelSegmentGroups.ItemClass : TFslObjectClass;
Begin
  Result := THL7V2ModelSegmentGroup;
End;

Function THL7V2ModelSegmentGroups.GetElement(Const iIndex : Integer) : THL7V2ModelSegmentGroup;
Begin
  Result := THL7V2ModelSegmentGroup(ObjectByIndex[iIndex]);
End;

Procedure THL7V2ModelSegmentGroups.SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelSegmentGroup);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;

Function THL7V2ModelSegmentGroups.CompareByCode(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelSegmentGroup(pA).Code, THL7V2ModelSegmentGroup(pB).Code);
End;

Function THL7V2ModelSegmentGroups.CompareByOptional(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.BooleanCompare(THL7V2ModelSegmentGroup(pA).Optional, THL7V2ModelSegmentGroup(pB).Optional);
End;

Function THL7V2ModelSegmentGroups.CompareByRepeating(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.BooleanCompare(THL7V2ModelSegmentGroup(pA).Repeating, THL7V2ModelSegmentGroup(pB).Repeating);
End;

Function THL7V2ModelSegmentGroups.IndexByCode(Const aValue : THL7V2ModelSegmentGroupCode) : Integer;
Var
  oElement : THL7V2ModelSegmentGroup;
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

Function THL7V2ModelSegmentGroups.Get(Const aValue : Integer) : THL7V2ModelSegmentGroup;
Begin
  Result := THL7V2ModelSegmentGroup(Inherited Get(aValue));
End;

Function THL7V2ModelSegmentGroups.GetByCode(Const aValue : THL7V2ModelSegmentGroupCode) : THL7V2ModelSegmentGroup;
Begin
  Result := Get(IndexByCode(aValue));
End;

Function THL7V2ModelSegmentGroups.ExistsByCode(Const aValue : THL7V2ModelSegmentGroupCode) : Boolean;
Begin
  Result := ExistsByIndex(IndexByCode(aValue));
End;

Procedure THL7V2ModelSegmentGroups.SortedByCode;
Begin
  SortedBy(CompareByCode);
End;

Function THL7V2ModelSegmentGroups.IsSortedByCode : Boolean;
Begin
  Result := IsSortedBy(CompareByCode);
End;

Procedure THL7V2ModelSegmentGroup.SetChildren(Const Value: THL7V2ModelSegmentGroups);
Begin
  FChildren.Free;
  FChildren := Value;
End;

Function THL7V2ModelSegmentGroup.UsesSegment(oSegment: THL7V2ModelSegment): Boolean;
Var
  iLoop : Integer;
Begin
  If GroupType = gtSingle Then
    Result := FCode = oSegment.code
  Else
    Begin
    Result := False;
    For iLoop := 0 To Children.Count - 1 Do
      Result := Result Or Children[iLoop].UsesSegment(oSegment);
    End;
End;

function THL7V2ModelSegmentGroups.Add(const sCode: String; bOptional, bRepeating: Boolean; aGroupType: THL7V2ModelSegmentGroupType): THL7V2ModelSegmentGroup;
begin
  Result := New;
  Try
    Result.Code := sCode;
    Result.Optional := bOptional;
    Result.Repeating := bRepeating;
    Result.GroupType := aGroupType;
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

Constructor THL7V2ModelMessageStructure.Create;
Begin
  Inherited;
End;

Destructor THL7V2ModelMessageStructure.Destroy;
Begin
  FSegmentMap.Free;
  FXMLMap.Free;
  Inherited;
End;

Function THL7V2ModelMessageStructure.Link : THL7V2ModelMessageStructure;
Begin
  Result := THL7V2ModelMessageStructure(Inherited Link);
End;

Function THL7V2ModelMessageStructure.Clone : THL7V2ModelMessageStructure;
Begin
  Result := THL7V2ModelMessageStructure(Inherited Clone);
End;

Procedure THL7V2ModelMessageStructure.Assign(oObject : TFslObject);
Begin
  Inherited;

  Name := THL7V2ModelMessageStructure(oObject).Name;
  Description := THL7V2ModelMessageStructure(oObject).Description;
  ExampleEvent := THL7V2ModelMessageStructure(oObject).ExampleEvent;
  ExampleMsgType := THL7V2ModelMessageStructure(oObject).ExampleMsgType;
  Action := THL7V2ModelMessageStructure(oObject).Action;
  SegmentMap := THL7V2ModelMessageStructure(oObject).SegmentMap.Clone;
  XMLMap :=  THL7V2ModelMessageStructure(oObject).XMLMap.Clone;
End;

function THL7V2ModelMessageStructure.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeOf(char)) + 12);
  inc(result, (FDescription.length * sizeOf(char)) + 12);
  inc(result, (FExampleEvent.length * sizeOf(char)) + 12);
  inc(result, (FExampleMsgType.length * sizeOf(char)) + 12);
  inc(result, (FAction.length * sizeOf(char)) + 12);
  inc(result, FSegmentMap.sizeInBytes);
  inc(result, FXMLMap.sizeInBytes);
end;

Function THL7V2ModelMessageStructures.Link : THL7V2ModelMessageStructures;
Begin
  Result := THL7V2ModelMessageStructures(Inherited Link);
End;

Function THL7V2ModelMessageStructures.Clone : THL7V2ModelMessageStructures;
Begin
  Result := THL7V2ModelMessageStructures(Inherited Clone);
End;

Function THL7V2ModelMessageStructures.New : THL7V2ModelMessageStructure;
Begin
  Result := THL7V2ModelMessageStructure(Inherited New);
End;

Function THL7V2ModelMessageStructures.ItemClass : TFslObjectClass;
Begin
  Result := THL7V2ModelMessageStructure;
End;

Function THL7V2ModelMessageStructures.GetElement(Const iIndex : Integer) : THL7V2ModelMessageStructure;
Begin
  Result := THL7V2ModelMessageStructure(ObjectByIndex[iIndex]);
End;

Procedure THL7V2ModelMessageStructures.SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelMessageStructure);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;

Function THL7V2ModelMessageStructures.CompareByName(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelMessageStructure(pA).Name, THL7V2ModelMessageStructure(pB).Name);
End;

Function THL7V2ModelMessageStructures.CompareByDescription(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelMessageStructure(pA).Description, THL7V2ModelMessageStructure(pB).Description);
End;

Function THL7V2ModelMessageStructures.CompareByExampleEvent(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelMessageStructure(pA).ExampleEvent, THL7V2ModelMessageStructure(pB).ExampleEvent);
End;

Function THL7V2ModelMessageStructures.CompareByExampleMsgType(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelMessageStructure(pA).ExampleMsgType, THL7V2ModelMessageStructure(pB).ExampleMsgType);
End;

Function THL7V2ModelMessageStructures.CompareByAction(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelMessageStructure(pA).Action, THL7V2ModelMessageStructure(pB).Action);
End;

Function THL7V2ModelMessageStructures.IndexByName(Const aValue : THL7V2ModelMessageStructureName) : Integer;
Var
  oElement : THL7V2ModelMessageStructure;
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

Function THL7V2ModelMessageStructures.IndexByDescription(Const aValue : THL7V2ModelMessageStructureDescription) : Integer;
Var
  oElement : THL7V2ModelMessageStructure;
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

Function THL7V2ModelMessageStructures.IndexByExampleEvent(Const aValue : THL7V2ModelMessageStructureExampleEvent) : Integer;
Var
  oElement : THL7V2ModelMessageStructure;
Begin
  oElement := New;
  Try
    oElement.ExampleEvent := aValue;

    If Not Find(oElement, Result, CompareByExampleEvent) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelMessageStructures.IndexByExampleMsgType(Const aValue : THL7V2ModelMessageStructureExampleMsgType) : Integer;
Var
  oElement : THL7V2ModelMessageStructure;
Begin
  oElement := New;
  Try
    oElement.ExampleMsgType := aValue;

    If Not Find(oElement, Result, CompareByExampleMsgType) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelMessageStructures.IndexByAction(Const aValue : THL7V2ModelMessageStructureAction) : Integer;
Var
  oElement : THL7V2ModelMessageStructure;
Begin
  oElement := New;
  Try
    oElement.Action := aValue;

    If Not Find(oElement, Result, CompareByAction) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelMessageStructures.Get(Const aValue : Integer) : THL7V2ModelMessageStructure;
Begin
  Result := THL7V2ModelMessageStructure(Inherited Get(aValue));
End;

Function THL7V2ModelMessageStructures.GetByName(Const aValue : THL7V2ModelMessageStructureName) : THL7V2ModelMessageStructure;
Begin
  Result := Get(IndexByName(aValue));
End;

Function THL7V2ModelMessageStructures.GetByDescription(Const aValue : THL7V2ModelMessageStructureDescription) : THL7V2ModelMessageStructure;
Begin
  Result := Get(IndexByDescription(aValue));
End;

Function THL7V2ModelMessageStructures.GetByExampleEvent(Const aValue : THL7V2ModelMessageStructureExampleEvent) : THL7V2ModelMessageStructure;
Begin
  Result := Get(IndexByExampleEvent(aValue));
End;

Function THL7V2ModelMessageStructures.GetByExampleMsgType(Const aValue : THL7V2ModelMessageStructureExampleMsgType) : THL7V2ModelMessageStructure;
Begin
  Result := Get(IndexByExampleMsgType(aValue));
End;

Function THL7V2ModelMessageStructures.GetByAction(Const aValue : THL7V2ModelMessageStructureAction) : THL7V2ModelMessageStructure;
Begin
  Result := Get(IndexByAction(aValue));
End;

Function THL7V2ModelMessageStructures.ExistsByName(Const aValue : THL7V2ModelMessageStructureName) : Boolean;
Begin
  Result := ExistsByIndex(IndexByName(aValue));
End;

Function THL7V2ModelMessageStructures.ExistsByDescription(Const aValue : THL7V2ModelMessageStructureDescription) : Boolean;
Begin
  Result := ExistsByIndex(IndexByDescription(aValue));
End;

Function THL7V2ModelMessageStructures.ExistsByExampleEvent(Const aValue : THL7V2ModelMessageStructureExampleEvent) : Boolean;
Begin
  Result := ExistsByIndex(IndexByExampleEvent(aValue));
End;

Function THL7V2ModelMessageStructures.ExistsByExampleMsgType(Const aValue : THL7V2ModelMessageStructureExampleMsgType) : Boolean;
Begin
  Result := ExistsByIndex(IndexByExampleMsgType(aValue));
End;

Function THL7V2ModelMessageStructures.ExistsByAction(Const aValue : THL7V2ModelMessageStructureAction) : Boolean;
Begin
  Result := ExistsByIndex(IndexByAction(aValue));
End;

Procedure THL7V2ModelMessageStructures.SortedByName;
Begin
  SortedBy(CompareByName);
End;

Procedure THL7V2ModelMessageStructures.SortedByDescription;
Begin
  SortedBy(CompareByDescription);
End;

Procedure THL7V2ModelMessageStructures.SortedByExampleEvent;
Begin
  SortedBy(CompareByExampleEvent);
End;

Procedure THL7V2ModelMessageStructures.SortedByExampleMsgType;
Begin
  SortedBy(CompareByExampleMsgType);
End;

Procedure THL7V2ModelMessageStructures.SortedByAction;
Begin
  SortedBy(CompareByAction);
End;

Function THL7V2ModelMessageStructures.IsSortedByName : Boolean;
Begin
  Result := IsSortedBy(CompareByName);
End;

Function THL7V2ModelMessageStructures.IsSortedByDescription : Boolean;
Begin
  Result := IsSortedBy(CompareByDescription);
End;

Function THL7V2ModelMessageStructures.IsSortedByExampleEvent : Boolean;
Begin
  Result := IsSortedBy(CompareByExampleEvent);
End;

Function THL7V2ModelMessageStructures.IsSortedByExampleMsgType : Boolean;
Begin
  Result := IsSortedBy(CompareByExampleMsgType);
End;

Function THL7V2ModelMessageStructures.IsSortedByAction : Boolean;
Begin
  Result := IsSortedBy(CompareByAction);
End;

Procedure THL7V2ModelMessageStructure.SetSegmentMap(Const Value: THL7V2ModelSegmentGroup);
Begin
  FSegmentMap.Free;
  FSegmentMap := Value;
End;

Procedure THL7V2ModelMessageStructure.SetXMLMap(Const Value: THL7V2ModelSegmentGroup);
Begin
  FXMLMap.Free;
  FXMLMap := Value;
End;

Function THL7V2ModelMessageStructure.UsesSegment(oSegment: THL7V2ModelSegment): Boolean;
Begin
  Result := Assigned(FSegmentMap) And FSegmentMap.UsesSegment(oSegment);
End;

function THL7V2ModelMessageStructures.Add(const sName, sDescription, sExampleEvent, sExampleMsgType, sAction: String): THL7V2ModelMessageStructure;
begin
  Result := New;
  Try
    Result.Name := sName;
    Result.Description := sDescription;
    Result.ExampleEvent := sExampleEvent;
    Result.ExampleMsgType := sExampleMsgType;
    Result.Action := sAction;
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

Constructor THL7V2ModelEventMessage.Create;
Begin
  Inherited;
End;

Destructor THL7V2ModelEventMessage.Destroy;
Begin
  FRefStructure.Free;
  FRefReplyStructure.Free;
  Inherited;
End;

Function THL7V2ModelEventMessage.Link : THL7V2ModelEventMessage;
Begin
  Result := THL7V2ModelEventMessage(Inherited Link);
End;

Function THL7V2ModelEventMessage.Clone : THL7V2ModelEventMessage;
Begin
  Result := THL7V2ModelEventMessage(Inherited Clone);
End;

Procedure THL7V2ModelEventMessage.Assign(oObject : TFslObject);
Begin
  Inherited;

  Message := THL7V2ModelEventMessage(oObject).Message;
  Structure := THL7V2ModelEventMessage(oObject).Structure;
  Reply := THL7V2ModelEventMessage(oObject).Reply;
  ReplyStructure := THL7V2ModelEventMessage(oObject).ReplyStructure;
End;

function THL7V2ModelEventMessage.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FMessage.length * sizeOf(char)) + 12);
  inc(result, (FStructure.length * sizeOf(char)) + 12);
  inc(result, (FReply.length * sizeOf(char)) + 12);
  inc(result, (FReplyStructure.length * sizeOf(char)) + 12);
  inc(result, FRefReplyStructure.sizeInBytes);
  inc(result, FRefStructure.sizeInBytes);
end;

Function THL7V2ModelEventMessages.Link : THL7V2ModelEventMessages;
Begin
  Result := THL7V2ModelEventMessages(Inherited Link);
End;

Function THL7V2ModelEventMessages.Clone : THL7V2ModelEventMessages;
Begin
  Result := THL7V2ModelEventMessages(Inherited Clone);
End;

Function THL7V2ModelEventMessages.New : THL7V2ModelEventMessage;
Begin
  Result := THL7V2ModelEventMessage(Inherited New);
End;

Function THL7V2ModelEventMessages.ItemClass : TFslObjectClass;
Begin
  Result := THL7V2ModelEventMessage;
End;

Function THL7V2ModelEventMessages.GetElement(Const iIndex : Integer) : THL7V2ModelEventMessage;
Begin
  Result := THL7V2ModelEventMessage(ObjectByIndex[iIndex]);
End;

Procedure THL7V2ModelEventMessages.SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelEventMessage);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;

Function THL7V2ModelEventMessages.CompareByMessage(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelEventMessage(pA).Message, THL7V2ModelEventMessage(pB).Message);
End;

Function THL7V2ModelEventMessages.CompareByStructure(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelEventMessage(pA).Structure, THL7V2ModelEventMessage(pB).Structure);
End;

Function THL7V2ModelEventMessages.CompareByReply(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelEventMessage(pA).Reply, THL7V2ModelEventMessage(pB).Reply);
End;

Function THL7V2ModelEventMessages.CompareByReplyStructure(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelEventMessage(pA).ReplyStructure, THL7V2ModelEventMessage(pB).ReplyStructure);
End;

Function THL7V2ModelEventMessages.IndexByMessage(Const aValue : THL7V2ModelEventMessageMessage) : Integer;
Var
  oElement : THL7V2ModelEventMessage;
Begin
  oElement := New;
  Try
    oElement.Message := aValue;

    If Not Find(oElement, Result, CompareByMessage) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelEventMessages.IndexByStructure(Const aValue : THL7V2ModelEventMessageStructure) : Integer;
Var
  oElement : THL7V2ModelEventMessage;
Begin
  oElement := New;
  Try
    oElement.Structure := aValue;

    If Not Find(oElement, Result, CompareByStructure) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelEventMessages.IndexByReply(Const aValue : THL7V2ModelEventMessageReply) : Integer;
Var
  oElement : THL7V2ModelEventMessage;
Begin
  oElement := New;
  Try
    oElement.Reply := aValue;

    If Not Find(oElement, Result, CompareByReply) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelEventMessages.IndexByReplyStructure(Const aValue : THL7V2ModelEventMessageReplyStructure) : Integer;
Var
  oElement : THL7V2ModelEventMessage;
Begin
  oElement := New;
  Try
    oElement.ReplyStructure := aValue;

    If Not Find(oElement, Result, CompareByReplyStructure) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ModelEventMessages.Get(Const aValue : Integer) : THL7V2ModelEventMessage;
Begin
  Result := THL7V2ModelEventMessage(Inherited Get(aValue));
End;

Function THL7V2ModelEventMessages.GetByMessage(Const aValue : THL7V2ModelEventMessageMessage) : THL7V2ModelEventMessage;
Begin
  Result := Get(IndexByMessage(aValue));
End;

Function THL7V2ModelEventMessages.GetByStructure(Const aValue : THL7V2ModelEventMessageStructure) : THL7V2ModelEventMessage;
Begin
  Result := Get(IndexByStructure(aValue));
End;

Function THL7V2ModelEventMessages.GetByReply(Const aValue : THL7V2ModelEventMessageReply) : THL7V2ModelEventMessage;
Begin
  Result := Get(IndexByReply(aValue));
End;

Function THL7V2ModelEventMessages.GetByReplyStructure(Const aValue : THL7V2ModelEventMessageReplyStructure) : THL7V2ModelEventMessage;
Begin
  Result := Get(IndexByReplyStructure(aValue));
End;

Function THL7V2ModelEventMessages.ExistsByMessage(Const aValue : THL7V2ModelEventMessageMessage) : Boolean;
Begin
  Result := ExistsByIndex(IndexByMessage(aValue));
End;

Function THL7V2ModelEventMessages.ExistsByStructure(Const aValue : THL7V2ModelEventMessageStructure) : Boolean;
Begin
  Result := ExistsByIndex(IndexByStructure(aValue));
End;

Function THL7V2ModelEventMessages.ExistsByReply(Const aValue : THL7V2ModelEventMessageReply) : Boolean;
Begin
  Result := ExistsByIndex(IndexByReply(aValue));
End;

Function THL7V2ModelEventMessages.ExistsByReplyStructure(Const aValue : THL7V2ModelEventMessageReplyStructure) : Boolean;
Begin
  Result := ExistsByIndex(IndexByReplyStructure(aValue));
End;

Procedure THL7V2ModelEventMessages.SortedByMessage;
Begin
  SortedBy(CompareByMessage);
End;

Procedure THL7V2ModelEventMessages.SortedByStructure;
Begin
  SortedBy(CompareByStructure);
End;

Procedure THL7V2ModelEventMessages.SortedByReply;
Begin
  SortedBy(CompareByReply);
End;

Procedure THL7V2ModelEventMessages.SortedByReplyStructure;
Begin
  SortedBy(CompareByReplyStructure);
End;

Function THL7V2ModelEventMessages.IsSortedByMessage : Boolean;
Begin
  Result := IsSortedBy(CompareByMessage);
End;

Function THL7V2ModelEventMessages.IsSortedByStructure : Boolean;
Begin
  Result := IsSortedBy(CompareByStructure);
End;

Function THL7V2ModelEventMessages.IsSortedByReply : Boolean;
Begin
  Result := IsSortedBy(CompareByReply);
End;

Function THL7V2ModelEventMessages.IsSortedByReplyStructure : Boolean;
Begin
  Result := IsSortedBy(CompareByReplyStructure);
End;

Procedure THL7V2ModelEventMessage.SetRefReplyStructure(Const Value: THL7V2ModelMessageStructure);
Begin
  FRefReplyStructure.Free;
  FRefReplyStructure := Value;
End;

Procedure THL7V2ModelEventMessage.SetRefStructure(Const Value: THL7V2ModelMessageStructure);
Begin
  FRefStructure.Free;
  FRefStructure := Value;
End;

function THL7V2ModelEventMessages.Add(const sMessage, sStructure, sReply, sReplyStructure: String): THL7V2ModelEventMessage;
begin
  Result := New;
  Try
    Result.Message := sMessage;
    Result.Structure := sStructure;
    Result.Reply := sReply;
    Result.ReplyStructure := sReplyStructure;
    Add(Result.Link);
  Finally
    Result.Free;
  End;

end;

Constructor THL7V2ModelEvent.Create;
Begin
  Inherited;
  FMessages := THL7V2ModelEventMessages.Create;
End;

Destructor THL7V2ModelEvent.Destroy;
Begin
  FMessages.Free;
  Inherited;
End;

Function THL7V2ModelEvent.Link : THL7V2ModelEvent;
Begin
  Result := THL7V2ModelEvent(Inherited Link);
End;

Function THL7V2ModelEvent.Clone : THL7V2ModelEvent;
Begin
  Result := THL7V2ModelEvent(Inherited Clone);
End;

Procedure THL7V2ModelEvent.Assign(oObject : TFslObject);
Begin
  Inherited;

  Name := THL7V2ModelEvent(oObject).Name;
  Description := THL7V2ModelEvent(oObject).Description;
  Messages.Assign(THL7V2ModelEvent(oObject).Messages)
End;

function THL7V2ModelEvent.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeOf(char)) + 12);
  inc(result, (FDescription.length * sizeOf(char)) + 12);
  inc(result, FMessages.sizeInBytes);
end;

Function THL7V2ModelEvents.Link : THL7V2ModelEvents;
Begin
  Result := THL7V2ModelEvents(Inherited Link);
End;

Function THL7V2ModelEvents.Clone : THL7V2ModelEvents;
Begin
  Result := THL7V2ModelEvents(Inherited Clone);
End;

Function THL7V2ModelEvents.New : THL7V2ModelEvent;
Begin
  Result := THL7V2ModelEvent(Inherited New);
End;

Function THL7V2ModelEvents.ItemClass : TFslObjectClass;
Begin
  Result := THL7V2ModelEvent;
End;

Function THL7V2ModelEvents.GetElement(Const iIndex : Integer) : THL7V2ModelEvent;
Begin
  Result := THL7V2ModelEvent(ObjectByIndex[iIndex]);
End;

Procedure THL7V2ModelEvents.SetElement(Const iIndex : Integer; Const oValue : THL7V2ModelEvent);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;

Function THL7V2ModelEvents.CompareByName(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelEvent(pA).Name, THL7V2ModelEvent(pB).Name);
End;

Function THL7V2ModelEvents.CompareByDescription(pA, pB : Pointer) : Integer;
Begin
  Result := fsl_utilities.StringCompare(THL7V2ModelEvent(pA).Description, THL7V2ModelEvent(pB).Description);
End;

Function THL7V2ModelEvents.IndexByName(Const aValue : THL7V2ModelEventName) : Integer;
Var
  oElement : THL7V2ModelEvent;
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

Function THL7V2ModelEvents.IndexByDescription(Const aValue : THL7V2ModelEventDescription) : Integer;
Var
  oElement : THL7V2ModelEvent;
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

Function THL7V2ModelEvents.Get(Const aValue : Integer) : THL7V2ModelEvent;
Begin
  Result := THL7V2ModelEvent(Inherited Get(aValue));
End;

Function THL7V2ModelEvents.GetByName(Const aValue : THL7V2ModelEventName) : THL7V2ModelEvent;
Begin
  Result := Get(IndexByName(aValue));
End;

Function THL7V2ModelEvents.GetByDescription(Const aValue : THL7V2ModelEventDescription) : THL7V2ModelEvent;
Begin
  Result := Get(IndexByDescription(aValue));
End;

Function THL7V2ModelEvents.ExistsByName(Const aValue : THL7V2ModelEventName) : Boolean;
Begin
  Result := ExistsByIndex(IndexByName(aValue));
End;

Function THL7V2ModelEvents.ExistsByDescription(Const aValue : THL7V2ModelEventDescription) : Boolean;
Begin
  Result := ExistsByIndex(IndexByDescription(aValue));
End;

Procedure THL7V2ModelEvents.SortedByName;
Begin
  SortedBy(CompareByName);
End;

Procedure THL7V2ModelEvents.SortedByDescription;
Begin
  SortedBy(CompareByDescription);
End;

Function THL7V2ModelEvents.IsSortedByName : Boolean;
Begin
  Result := IsSortedBy(CompareByName);
End;

Function THL7V2ModelEvents.IsSortedByDescription : Boolean;
Begin
  Result := IsSortedBy(CompareByDescription);
End;

Procedure THL7V2ModelEvent.SetMessages(Const Value: THL7V2ModelEventMessages);
Begin
  FMessages.Free;
  FMessages := Value;
End;

Function THL7V2ModelEvent.UsesStructure(oStructure: THL7V2ModelMessageStructure): Boolean;
Var
  iLoop : Integer;
  oMessage : THL7V2ModelEventMessage;
Begin
  Result := False;
  For iLoop := 0 To Messages.count - 1 Do
    Begin
    oMessage := Messages[iLoop];
    Result := Result And (oMessage.RefStructure = oStructure) Or (oMessage.RefReplyStructure = oStructure);
    End;
End;

function THL7V2ModelEvents.Add(const sName, sDesc: String): THL7V2ModelEvent;
begin
  Result := New;
  Try
    Result.Name := sName;
    Result.Description := sDesc;
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

{ THL7V2Model }

constructor THL7V2Model.Create;
begin
  inherited;
  FTables := THL7V2ModelTables.create;
  FTables.SortedByID;
  FComponents := THL7V2ModelComponents.create;
  FComponents.SortedByNumber;
  FDataElements := THL7V2ModelDataElements.create;
  FDataElements.SortedById;
  FSegments := THL7V2ModelSegments.create;
  FSegments.SortedByCode;
  FDataTypes := THL7V2ModelDataTypes.create;
  FDataTypes.SortedByName;
  FStructures := THL7V2ModelStructures.create;
  FStructures.SortedByID;
  FEvents := THL7V2ModelEvents.create;
  FEvents.SortedByName;
  FMessageStructures := THL7V2ModelMessageStructures.create;
  FMessageStructures.SortedByName;
end;

destructor THL7V2Model.Destroy;
begin
  FTables.Free;
  FComponents.Free;
  FDataElements.Free;
  FSegments.Free;
  FDataTypes.Free;
  FStructures.Free;
  FEvents.Free;
  FMessageStructures.Free;
  inherited;
end;

function THL7V2Model.Link: THL7V2Model;
begin
  result := THL7V2Model(inherited Link);
end;

function THL7V2Model.Clone: THL7V2Model;
begin
  result := THL7V2Model(inherited Clone);
end;

procedure THL7V2Model.Assign(oObject: TFslObject);
begin
  inherited;
  Version := THL7V2Model(oObject).Version;
  FTables.Assign(THL7V2Model(oObject).FTables);
  FComponents.Assign(THL7V2Model(oObject).FComponents);
  FDataElements.Assign(THL7V2Model(oObject).FDataElements);
  FSegments.Assign(THL7V2Model(oObject).FSegments);
  FDataTypes.Assign(THL7V2Model(oObject).FDataTypes);
  FStructures.Assign(THL7V2Model(oObject).FStructures);
  FEvents.Assign(THL7V2Model(oObject).FEvents);
  FMessageStructures.Assign(THL7V2Model(oObject).FMessageStructures);
end;

procedure THL7V2Model.SetTables(const Value: THL7V2ModelTables);
begin
  FTables.Free;
  FTables := Value;
end;

procedure THL7V2Model.SetComponents(const Value: THL7V2ModelComponents);
begin
  FComponents.Free;
  FComponents := Value;
end;

procedure THL7V2Model.SetDataElements(const Value: THL7V2ModelDataElements);
begin
  FDataElements.Free;
  FDataElements := Value;
end;

procedure THL7V2Model.SetSegments(const Value: THL7V2ModelSegments);
begin
  FSegments.Free;
  FSegments := Value;
end;

procedure THL7V2Model.SetDataTypes(const Value: THL7V2ModelDataTypes);
begin
  FDataTypes.Free;
  FDataTypes := Value;
end;

procedure THL7V2Model.SetStructures(const Value: THL7V2ModelStructures);
begin
  FStructures.Free;
  FStructures := Value;
end;

procedure THL7V2Model.SetEvents(const Value: THL7V2ModelEvents);
begin
  FEvents.Free;
  FEvents := Value;
end;

procedure THL7V2Model.SetMessageStructures(const Value: THL7V2ModelMessageStructures);
begin
  FMessageStructures.Free;
  FMessageStructures := Value;
end;

Function THL7V2Model.isVariableTypeName(Const sName : String) : Boolean;
Begin
  Result := (StringCompareInsensitive(sName, 'varies') = 0) Or
            (StringCompareInsensitive(sName, 'var') = 0) Or
            (StringCompareInsensitive(sName, '*') = 0);
End;

procedure THL7V2Model.CrossLink;
var
  iLoop : Integer;
  iInner : Integer;
  oComponent : THL7V2ModelComponent;
  oDataElement : THL7V2ModelDataElement;
  oField : THL7V2ModelField;
  oStructure : THL7V2ModelStructure;
  oMessage : THL7V2ModelEventMessage;
begin
  for iLoop := 0 to Components.count - 1 do
    begin
    oComponent := Components[iLoop];
    If oComponent.Table <> 0 Then
      oComponent.RefTable := Tables.GetByID(oComponent.Table).Link;
    oComponent.RefDataType := DataTypes.GetByName(oComponent.DataType).Link;
    oComponent.RefStructure := Structures.GetByName(oComponent.DataType);
    end;

  for iLoop := 0 to DataElements.count - 1 do
    begin
    oDataElement := DataElements[iLoop];
    if (oDataElement.Table <> 0) Then
      oDataElement.RefTable := Tables.GetByID(oDataElement.Table).Link;
    oDataElement.RefStructure := Structures.GetByName(oDataElement.Structure).Link;
    end;

  for iLoop := 0 to Segments.Count - 1 do
    for iInner := 0 to Segments[iLoop].Fields.Count - 1 do
      begin
      oField := Segments[iLoop].Fields[iInner];
      oField.RefDataElement := DataElements.GetByID(oField.DataElement).Link;
      end;

  for iLoop := 0 to Structures.count - 1 do
    begin
    oStructure := Structures[iLoop];
    oStructure.RefDataType := DataTypes.GetByName(oStructure.DataType).Link;
    if (oStructure.Components.Count = 1) And ((oStructure.Components[0].DataType = oStructure.Name)
         Or (oStructure.Components[0].DataType = 'NUL') Or (isVariableTypeName(oStructure.Name))) Then
      oStructure.Components.DeleteByIndex(0);
    end;

  for iLoop := 0 to Events.Count - 1 do
    for iInner := 0 to Events[iLoop].Messages.Count - 1 do
      begin
      oMessage := Events[iLoop].Messages[iInner];
      oMessage.RefStructure := MessageStructures.GetByName(oMessage.Structure).Link;
      oMessage.RefReplyStructure := MessageStructures.GetByName(oMessage.ReplyStructure).Link;
      end;
end;

function THL7V2Model.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FTables.sizeInBytes);
  inc(result, FComponents.sizeInBytes);
  inc(result, FDataElements.sizeInBytes);
  inc(result, FSegments.sizeInBytes);
  inc(result, FDataTypes.sizeInBytes);
  inc(result, FStructures.sizeInBytes);
  inc(result, FEvents.sizeInBytes);
  inc(result, FMessageStructures.sizeInBytes);
end;

{ THL7V2SchemaStoreCacheEntries }

function THL7V2SchemaStoreCacheEntries.CompareByVersionAndStructure(pA, pB: Pointer): Integer;
begin
  Result := StringCompare(THL7V2SchemaStoreCacheEntry(pA).Name, THL7V2SchemaStoreCacheEntry(pB).Name);
  if result = 0 then
    Result := ord(THL7V2SchemaStoreCacheEntry(pA).FVersion) - ord(THL7V2SchemaStoreCacheEntry(pB).FVersion);
end;

function THL7V2SchemaStoreCacheEntries.ExistsByVersionAndStructure(aVersion: THL7V2Version; const sStruct: String): Boolean;
begin
  result := ExistsByIndex(IndexByVersionAndStructure(aVersion, sStruct));
end;

function THL7V2SchemaStoreCacheEntries.Get(iIndex: integer): THL7V2SchemaStoreCacheEntry;
begin
  result := THL7V2SchemaStoreCacheEntry(inherited Get(iIndex));
end;

function THL7V2SchemaStoreCacheEntries.GetByVersionAndStructure(aVersion: THL7V2Version; const sStruct: String): THL7V2SchemaStoreCacheEntry;
begin
  result := Get(IndexByVersionAndStructure(aVersion, sStruct));
end;

function THL7V2SchemaStoreCacheEntries.GetEntry(iIndex: Integer): THL7V2SchemaStoreCacheEntry;
begin
  result := THL7V2SchemaStoreCacheEntry(ObjectByIndex[iIndex]);
end;

function THL7V2SchemaStoreCacheEntries.IndexByVersionAndStructure(aVersion: THL7V2Version; const sStruct: String): Integer;
var
  oItem : THL7V2SchemaStoreCacheEntry;
begin
  oItem := THL7V2SchemaStoreCacheEntry.create;
  try
    oItem.Version := aVersion;
    oItem.Name := sStruct;
    if not find(oItem, result, CompareByVersionAndStructure) then
      result := -1;
  finally
    oItem.Free;
  end;
end;

function THL7V2SchemaStoreCacheEntries.ItemClass: TFslObjectClass;
begin
  result := THL7V2SchemaStoreCacheEntry;
end;

procedure THL7V2SchemaStoreCacheEntries.SortedByVersionAndStructure;
begin
  SortedBy(CompareByVersionAndStructure);
end;

{ THL7V2SchemaStoreCacheEntry }

destructor THL7V2SchemaStoreCacheEntry.Destroy;
begin
  FMap.Free;
  inherited;
end;

procedure THL7V2SchemaStoreCacheEntry.SetMap(const Value: THL7V2ModelSegmentGroup);
begin
  FMap.Free;
  FMap := Value;
end;

function THL7V2SchemaStoreCacheEntry.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMap.sizeInBytes);
end;

Constructor THL7V2SchemaStore.Create;
Begin
  Inherited;
  FLock := TFslLock.Create;
  FCache := THL7V2SchemaStoreCacheEntries.Create;
End;

Constructor THL7V2SchemaStore.Create(Const sFileName, sFolder : String);
Begin
  Create;
  FileName := sFileName;
  Folder := sFolder;
End;

Destructor THL7V2SchemaStore.Destroy;
Begin
  FCache.Free;
  FLock.Free;
  Inherited;
End;

Function THL7V2SchemaStore.ProduceSchemaMap(aVersion : THL7V2Version; Const sStruct: String): THL7V2ModelSegmentGroup;
Var
  aDate : TDateTime;
  oEntry : THL7V2SchemaStoreCacheEntry;
  bOk : Boolean;
  iIndex : Integer;
Begin
  Result := Nil;
  aDate := CheckFileExists(aVersion, sStruct);
  FLock.Lock;
  Try
    oEntry := FCache.GetByVersionAndStructure(aVersion, sStruct);
    bOk := Assigned(oEntry) And (aDate < oEntry.Date);
    If bOk Then
      Result := oEntry.Map.Link
    Else If Assigned(oEntry) Then
      FCache.DeleteByReference(oEntry);
  Finally
    FLock.UnLock;
  End;
  If Not bOK Then
    If aDate = 0 Then
      ErrorXML('ProduceSchemaMap', 'No schema found for '+sStruct+' from v'+NAMES_HL7V2_VERSION[aVersion]+' in Store ('+FFilename+') or Path('+FFolder+')')
    Else
      Begin
      Result := LoadSchemaFromXML(aVersion, sStruct);
      FLock.Lock;
      Try
        iIndex := FCache.IndexByVersionAndStructure(aVersion, sStruct);
        If iIndex > -1 Then
          FCache.DeleteByIndex(iIndex);
        FCache.Add(Result.Link);
      Finally
        FLock.UnLock;
      End;
      End;
End;

Procedure THL7V2SchemaStore.SetFileName(Const sValue: String);
Begin
  If sValue <> FFileName Then
    Begin
    FFilename := sValue;
    FLock.Lock;
    Try
      FCache.Clear;
      LoadSchemasFromStore;
    Finally
      FLock.Unlock;
    End;
    End;
End;

Function THL7V2SchemaStore.CheckFileExists(aVersion: THL7V2Version; Const sStruct: String): TDateTime;
Begin
  Result := FileGetModified(GetSchemaFileName(aVersion, sStruct));
End;

Function THL7V2SchemaStore.GetSchemaFileName(aVersion: THL7V2Version; Const sStruct: String): String;
Begin
  Result := IncludeTrailingBackslash(FFolder) + 'v' + NAMES_HL7V2_VERSION[aVersion] + '\' + sStruct + '.xsd';
End;

Procedure THL7V2SchemaStore.LoadSchemasFromStore;
Var
  oReader: TReader;
  oFile: TFileStream;
  aVer : THL7V2Version;
  sStruct : String;
  oStream: TStringStream;
Begin
  If FileExists(FFilename) Then
    Begin
    oFile := TFileStream.Create(FFilename, fmOpenRead Or fmShareDenyWrite);
    Try
      oReader := TReader.Create(oFile, 8192);
      Try
        oReader.ReadListBegin;
        While Not oReader.EndOfList Do
          Begin
          aVer := THL7V2Version(oReader.ReadInteger);
          sStruct := oReader.ReadString;
          oStream := TStringStream.Create(oReader.ReadString);
          Try
            ReadSchemaFromStream(aVer, sStruct, oStream);
          Finally
            oStream.Free;
          End;
          End;
        oReader.ReadListEnd;
      Finally
        oReader.Free;
      End;
    Finally
      oFile.Free;
    End;
    End;
End;

Procedure THL7V2SchemaStore.WriteSegmentGroup(oWriter : TWriter; oGroup : THL7V2ModelSegmentGroup);
Var
  iLoop : Integer;
Begin
  oWriter.WriteString(oGroup.Code);
  oWriter.WriteBoolean(oGroup.Optional);
  oWriter.WriteBoolean(oGroup.Repeating);
  oWriter.WriteInteger(Integer(oGroup.GroupType));
  oWriter.WriteInteger(oGroup.Children.Count);
  For iLoop := 0 To oGroup.Children.Count - 1 Do
    WriteSegmentGroup(oWriter, oGroup.Children[iLoop]);
End;

Function THL7V2SchemaStore.SaveSegmentMap(oMap : THL7V2ModelSegmentGroup) : String;
Var
  oStream : TStringStream;
  oWriter : TWriter;
Begin
  oStream := TStringStream.Create('');
  Try
    oWriter := TWriter.Create(oStream, 8192);
    Try
      WriteSegmentGroup(oWriter, oMap);
    Finally
      oWriter.Free;
    End;
    Result := oStream.DataString;
  Finally
    oStream.Free;
  End;
End;

Procedure THL7V2SchemaStore.SaveSchemaMap(oWriter : TWriter; aVersion : THL7V2Version; sName : String; oMap : THL7V2ModelSegmentGroup);
Begin
  oWriter.WriteInteger(Integer(aVersion));
  oWriter.WriteString(sName);
  oWriter.WriteString(SaveSegmentMap(oMap));
End;

Procedure THL7V2SchemaStore.ReadSegmentGroup(oReader : TReader; oGroup : THL7V2ModelSegmentGroup);
Var
  iLoop : Integer;
  iTotal : Integer;
  oChild : THL7V2ModelSegmentGroup;
Begin
  oGroup.Code := oReader.ReadString;
  oGroup.Optional := oReader.ReadBoolean;
  oGroup.Repeating := oReader.ReadBoolean;
  oGroup.GroupType := THL7V2ModelSegmentGroupType(oReader.ReadInteger);
  iTotal := oReader.ReadInteger;
  For iLoop := 0 To iTotal - 1 Do
    Begin
    oChild := THL7V2ModelSegmentGroup.Create;
    Try
      ReadSegmentGroup(oReader, oChild);
      oGroup.Children.Add(oChild.Link);
    Finally
      oChild.Free;
    End;
    End;
End;

Procedure THL7V2SchemaStore.ReadSchemaFromStream(aVersion: THL7V2Version; Const sStruct: String; oStream: TStream);
Var
  oReader : TReader;
  oMap : THL7V2ModelSegmentGroup;
  oEntry : THL7V2SchemaStoreCacheEntry;
Begin
  oReader := TReader.Create(oStream, 8192);
  Try
    oMap := THL7V2ModelSegmentGroup.Create;
    Try
      ReadSegmentGroup(oReader, oMap);
      oEntry := THL7V2SchemaStoreCacheEntry.Create;
      Try
        oEntry.Date := now;
        oEntry.Version := aVersion;
        oEntry.Name := sStruct;
        oEntry.Map := oMap.Link;
        FCache.Add(oEntry.Link);
      Finally
        oEntry.Free;
      End;
    Finally
      oMap.Free;
    End;
  Finally
    oReader.Free;
  End;
End;

Function THL7V2SchemaStore.GetChildByAttributeValue(oElement: TMXmlElement; Const sName, sValue: String; bCase: Boolean = False): TMXmlElement;
Var
  sVal : String;
Begin
  Result := oElement.firstElement;
  While Assigned(Result) Do
    Begin
    sVal := Result.attributeNS[XML_NS_SCHEMA, sName];
    If (Not bCase And StringEquals(sVal, sValue)) Or (sVal = sValue) Then
      Exit;
    Result := Result.nextElement;
    End;
End;

Procedure THL7V2SchemaStore.ReadSchemaGroup(oDom: TMXmlDocument; oGroup: THL7V2ModelSegmentGroup; Const sFilename : String);
Var
  oSchema: TMXmlElement;
  oElem: TMXmlElement;
  oElem2: TMXmlElement;
  oNode: TMXmlElement;
  sName: String;
  oChild : THL7V2ModelSegmentGroup;
Begin
  oSchema := oDom.docElement;
  If (oSchema.Name <> 'schema') Or (oSchema.NamespaceURI <> XML_NS_SCHEMA) Then
    oSchema := oSchema.elementNS(XML_NS_SCHEMA, 'schema');
  If Not Assigned(oSchema) Then
    ErrorXML('ReadSchemaGroup', 'schema root not found in '+sFilename);

  oElem := GetChildByAttributeValue(oSchema, 'name', oGroup.Code);
  If Not Assigned(oElem) Then
    ErrorXML('ReadSchemaGroup', 'schema definition for "'+oGroup.Code+'" not found in '+sFilename);

  oElem2 := oElem.elementNS(XML_NS_SCHEMA, 'complexType');
  If Not Assigned(oElem2) Then
    If oElem.attributeNS[XML_NS_SCHEMA, 'type'] <> '' Then
      oElem2 := GetChildByAttributeValue(oSchema, 'name', oElem.attributeNS[XML_NS_SCHEMA, 'type'])
    Else
      ErrorXML('ReadSchemaGroup', 'No type information or reference found for group "' + oGroup.Code + '" in '+sFilename);
  If Not Assigned(oElem2) Then
    ErrorXML('ReadSchemaGroup', 'Complex Type node not found for the group "' + oGroup.Code + '" in '+sFilename);

  If Assigned(oElem2.elementNS(XML_NS_SCHEMA, 'sequence')) Then
    oElem2 := oElem2.elementNS(XML_NS_SCHEMA, 'sequence')
  Else
    Begin
    oGroup.GroupType := gtChoice;
    oElem2 := oElem2.elementNS(XML_NS_SCHEMA, 'choice');
    End;
  If Not Assigned(oElem2) Then
    ErrorXML('ReadSchemaGroup', 'Sequence or Choice node not found for the group "' + oGroup.Code + '" could be found in '+sFilename);

  oNode := oElem2.firstElement;
  While Assigned(oNode) Do
    Begin
    If (oNode.Name = 'element') Then
      Begin
      sName := oNode.attributeNS[XML_NS_SCHEMA, 'ref'];
      oChild := THL7V2ModelSegmentGroup.Create;
      Try
        oChild.GroupType := gtSingle;
        If Length(sName) = 3 Then
          oChild.Code := sName
        Else If (sName[1] = '(') And (Length(sName) = 5) Then
          oChild.Code := Copy(sName, 2, 3)
        Else If (sName = 'anyZSegment') Then
          oChild.Code := 'Z*'
        Else If (sName = 'anyHL7Segment') Then
          oChild.Code := '*'
        Else
          Begin
          oChild.GroupType := gtGroup;
          oChild.Code := sName;
          ReadSchemaGroup(oDom, oChild, sFilename);
          End;
        oChild.Optional := oNode.attributeNS[XML_NS_SCHEMA, 'minOccurs'] = '0';
        oChild.Repeating := oNode.attributeNS[XML_NS_SCHEMA, 'maxOccurs'] = 'unbounded';
        oGroup.Children.Add(oChild.Link);
      Finally
        oChild.Free;
      End;
      End;
    oNode := oNode.nextElement;
    End;
End;

Function THL7V2SchemaStore.LoadSchemaFromXML(aVersion: THL7V2Version; Const sStruct: String): THL7V2ModelSegmentGroup;
Var
  sFilename : String;
  doc : TMXmlDocument;
  f : TFileStream;
Begin
  sFileName := GetSchemaFileName(aVersion, sStruct);
  Result := THL7V2ModelSegmentGroup.Create;
  Try
    Result.Code := sStruct;
    Result.GroupType := gtGroup;
    f := TFileStream.Create(sFilename, fmOpenRead + fmShareDenyWrite);
    try
      doc := TMXmlParser.parse(f, [xpResolveNamespaces, xpDropWhitespace, xpDropComments]);
      try
        ReadSchemaGroup(doc, Result, sFilename);
      Finally
        doc.Free;
      end;
    finally
      f.Free;
    End;
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Procedure THL7V2SchemaStore.BuildFromSchemas(AProgress: TOnSchemaTransferProgress);
Var
  oWriter: TWriter;
  oFile: TFileStream;
Begin
  oFile := TFileStream.Create(FFilename, fmCreate);
  Try
    oWriter := TWriter.Create(oFile, 8192);
    Try
      oWriter.WriteListBegin;

      ReadVersionSchemas(oWriter, hv231, IncludeTrailingBackslash(FFolder) + 'v2.3.1', AProgress);
      ReadVersionSchemas(oWriter, hv24,  IncludeTrailingBackslash(FFolder) + 'v2.4',   AProgress);
      ReadVersionSchemas(oWriter, hv25,  IncludeTrailingBackslash(FFolder) + 'v2.5',   AProgress);

      oWriter.WriteListEnd;
    Finally
      oWriter.Free;
    End;
  Finally
    oFile.Free;
  End;
End;

Procedure THL7V2SchemaStore.ReadVersionSchemas(oWriter: TWriter; aVersion: THL7V2Version; sFolder: String; aProgress: TOnSchemaTransferProgress);
Var
  iCount, iTotal: Integer;
  oMap: THL7V2ModelSegmentGroup;
  sName: String;
  aSearchRec: TSearchRec;
  iFound: Integer;
  bAbort : Boolean;
Begin
  iTotal := 0;
  iFound := FindFirst(IncludeTrailingBackslash(sFolder) + '*.xsd', faAnyfile, aSearchRec);
  While iFound = 0 Do
    Begin
    Inc(iTotal);
    iFound := FindNext(aSearchRec);
    End;

  iCount := 0;
  iFound := FindFirst(IncludeTrailingBackslash(sFolder) + '*.xsd', faAnyfile, aSearchRec);
  While iFound = 0 Do
    Begin
    sName := PathTitle(aSearchRec.Name);
    If Assigned(aProgress) Then
      Begin
      bAbort := False;
      aProgress(Self, NAMES_HL7V2_VERSION[aVersion], sName, iCount, iTotal, bAbort);
      If bAbort Then
        Abort;
      End;

    If Not StringArrayExistsInsensitive(['batch', 'datatypes', 'fields', 'messages', 'segments'], sName) Then
      Begin
      oMap := LoadSchemaFromXML(AVersion, sName);
      Try
        SaveSchemaMap(oWriter, aVersion, sName, oMap);
      Finally
        oMap.Free;
      End;
      End;
    iFound := FindNext(aSearchRec);
    Inc(iCount);
    End;
End;

function THL7V2SchemaStore.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FFilename.length * sizeof(char)) + 12);
  inc(result, (FFolder.length * sizeof(char)) + 12);
  inc(result, FCache.sizeInBytes);
end;

constructor THL7V2Dictionary.Create;
begin
  inherited;
  FLock := TFslLock.Create;
end;

destructor THL7V2Dictionary.Destroy;
var
  aVersion : THL7V2Version;
begin
  FSchemaStore.Free;
  for aVersion := Low(THL7V2Version) to High(THL7V2Version) do
    FModels[aVersion].free;
  FLock.Free;
  inherited;
end;

procedure THL7V2Dictionary.PrepareForLoad(bWipe: Boolean);
begin
  Raise EHL7V2Exception.Create('Need to override PrepareForLoad in '+ClassName);
end;

procedure THL7V2Dictionary.DoneLoading(aTransferEvent: TOnDictTransferProgress);
begin
  Raise EHL7V2Exception.Create('Need to override DoneLoading in '+ClassName);
end;

function  THL7V2Dictionary.VersionDefined(aVersion : THL7V2Version; var sDesc: String): Boolean;
begin
  Raise EHL7V2Exception.Create('Need to override VersionDefined in '+ClassName);
end;

Function  THL7V2Dictionary.ListVersions : THL7V2Versions;
begin
  result := [];
  Raise EHL7V2Exception.Create('Need to override ListVersions : in '+ClassName);
end;

procedure THL7V2Dictionary.AddVersion(aVersion : THL7V2Version; const sDescription: String);
begin
  Raise EHL7V2Exception.Create('Need to override AddVersion in '+ClassName);
end;

function  THL7V2Dictionary.SourceDescription(bFulldetails: Boolean): String;
begin
  result := '';
  Raise EHL7V2Exception.Create('Need to override SourceDescription in '+ClassName);
end;

procedure THL7V2Dictionary.TransferDatabase(oDest: THL7V2Dictionary; bWipe: Boolean);
var
  aVersions : THL7V2Versions;
  aVersion : THL7V2Version;
  oModel : THL7V2Model;
  sDesc : String;
begin
  oDest.OnTransferProgress := OnTransferProgress;
  if not oDest.FInitialised then
    begin
    oDest.FInitialised := true;
    oDest.Initialise;
    end;
  oDest.PrepareForLoad(bWipe);

  aVersions := Versions;
  for aVersion := Low(THL7V2Version) to High(THL7V2Version) do
    if aVersion in aVersions then
      begin
      oModel := THL7V2Model.create;
      try
        oModel.Version := aVersion;
        VersionDefined(aVersion, sDesc);
        LoadModel(oModel);
        oModel.CrossLink;
        oDest.SaveModel(oModel, sDesc);
      finally
        oModel.Free;
      end;
      end;
  oDest.DoneLoading(FOnTransferProgress);
end;

function THL7V2Dictionary.Clone: THL7V2Dictionary;
begin
  result := THL7V2Dictionary(inherited Clone);
end;

function THL7V2Dictionary.Link: THL7V2Dictionary;
begin
  result := THL7V2Dictionary(inherited Link);
end;

procedure THL7V2Dictionary.AddTables(aVersion : THL7V2Version; oTables: THL7V2ModelTables);
begin
  Raise EHL7V2Exception.Create('Need to override AddTables in '+ClassName);
end;

procedure THL7V2Dictionary.LoadTables(aVersion : THL7V2Version; oTables: THL7V2ModelTables);
begin
  Raise EHL7V2Exception.Create('Need to override LoadTables in '+ClassName);
end;

procedure THL7V2Dictionary.AddComponents(aVersion : THL7V2Version; oComponents: THL7V2ModelComponents);
begin
  Raise EHL7V2Exception.Create('Need to override AddComponents in '+ClassName);
end;

procedure THL7V2Dictionary.LoadComponents(aVersion : THL7V2Version; oComponents: THL7V2ModelComponents);
begin
  Raise EHL7V2Exception.Create('Need to override LoadComponents in '+ClassName);
end;

procedure THL7V2Dictionary.AddDataElements(aVersion : THL7V2Version; oDataElements: THL7V2ModelDataElements);
begin
  Raise EHL7V2Exception.Create('Need to override AddDataElements in '+ClassName);
end;

procedure THL7V2Dictionary.LoadDataElements(aVersion : THL7V2Version; oDataElements: THL7V2ModelDataElements);
begin
  Raise EHL7V2Exception.Create('Need to override LoadDataElements in '+ClassName);
end;

procedure THL7V2Dictionary.AddSegments(aVersion : THL7V2Version; oSegments: THL7V2ModelSegments);
begin
  Raise EHL7V2Exception.Create('Need to override AddSegments in '+ClassName);
end;

procedure THL7V2Dictionary.LoadSegments(aVersion : THL7V2Version; oSegments: THL7V2ModelSegments);
begin
  Raise EHL7V2Exception.Create('Need to override LoadSegments in '+ClassName);
end;

procedure THL7V2Dictionary.AddDataTypes(aVersion : THL7V2Version; oDataTypes: THL7V2ModelDataTypes);
begin
  Raise EHL7V2Exception.Create('Need to override AddDataTypes in '+ClassName);
end;

procedure THL7V2Dictionary.LoadDataTypes(aVersion : THL7V2Version; oDataTypes: THL7V2ModelDataTypes);
begin
  Raise EHL7V2Exception.Create('Need to override LoadDataTypes in '+ClassName);
end;

procedure THL7V2Dictionary.AddStructures(aVersion : THL7V2Version; oStructures: THL7V2ModelStructures);
begin
  Raise EHL7V2Exception.Create('Need to override AddStructures in '+ClassName);
end;

procedure THL7V2Dictionary.LoadStructures(aVersion : THL7V2Version; oStructures: THL7V2ModelStructures);
begin
  Raise EHL7V2Exception.Create('Need to override LoadStructures in '+ClassName);
end;

procedure THL7V2Dictionary.AddEvents(aVersion : THL7V2Version; oEvents: THL7V2ModelEvents);
begin
  Raise EHL7V2Exception.Create('Need to override AddEvents in '+ClassName);
end;

procedure THL7V2Dictionary.LoadEvents(aVersion : THL7V2Version; oEvents: THL7V2ModelEvents);
begin
  Raise EHL7V2Exception.Create('Need to override LoadEvents in '+ClassName);
end;

procedure THL7V2Dictionary.AddMessageStructures(aVersion : THL7V2Version; oMessageStructures: THL7V2ModelMessageStructures);
begin
  Raise EHL7V2Exception.Create('Need to override AddMessageStructures in '+ClassName);
end;

procedure THL7V2Dictionary.LoadMessageStructures(aVersion : THL7V2Version; oMessageStructures: THL7V2ModelMessageStructures);
begin
  Raise EHL7V2Exception.Create('Need to override LoadMessageStructures in '+ClassName);
end;

procedure THL7V2Dictionary.AddStructureComponents(aVersion: THL7V2Version; oStructures: THL7V2ModelStructures);
begin
  Raise EHL7V2Exception.Create('Need to override AddStructureComponents in '+ClassName);
end;

procedure THL7V2Dictionary.LoadStructureComponents(aVersion: THL7V2Version; oStructures: THL7V2ModelStructures; oComponents: THL7V2ModelComponents);
begin
  Raise EHL7V2Exception.Create('Need to override LoadStructureComponents in '+ClassName);
end;

procedure THL7V2Dictionary.AddSegmentFields(aVersion: THL7V2Version; oSegments: THL7V2ModelSegments);
begin
  Raise EHL7V2Exception.Create('Need to override AddSegmentFields in '+ClassName);
end;

procedure THL7V2Dictionary.LoadSegmentFields(aVersion: THL7V2Version; oSegment: THL7V2ModelSegment);
begin
  Raise EHL7V2Exception.Create('Need to override LoadSegmentFields in '+ClassName);
end;

procedure THL7V2Dictionary.AddEventMessages(aVersion: THL7V2Version; oEvents: THL7V2ModelEvents);
begin
  Raise EHL7V2Exception.Create('Need to override AddEventMessages in '+ClassName);
end;

procedure THL7V2Dictionary.LoadEventMessages(aVersion: THL7V2Version; oEvents: THL7V2ModelEvents);
begin
  Raise EHL7V2Exception.Create('Need to override LoadEventMessages in '+ClassName);
end;

procedure THL7V2Dictionary.LoadSegmentMaps(aVersion : THL7V2Version; oStructures : THL7V2ModelMessageStructures);
begin
  Raise EHL7V2Exception.Create('Need to override LoadSegmentMaps in '+ClassName);
end;

procedure THL7V2Dictionary.AddSegmentMaps(aVersion : THL7V2Version; oStructures : THL7V2ModelMessageStructures);
begin
  Raise EHL7V2Exception.Create('Need to override AddSegmentMaps in '+ClassName);
end;

procedure THL7V2Dictionary.LoadSegmentFields(aVersion: THL7V2Version; oSegments: THL7V2ModelSegments);
var
  iLoop : integer;
begin
  for iLoop := 0 to oSegments.count - 1 do
    LoadSegmentFields(aVersion, oSegments[iLoop]);
end;

function THL7V2Dictionary.GetModel(aVersion: THL7V2Version): THL7V2Model;
begin
  FLock.Lock;
  Try
    result := FModels[aVersion];
    if result = nil then
      begin
      result := THL7V2Model.create;
      try
        result.Version := aVersion;
        LoadModel(result);
        result.CrossLink;
        FModels[aVersion] := result.Link;
      finally
        result.Free;
      end;
      end;
  Finally
    FLock.Unlock;
  End;
end;

procedure THL7V2Dictionary.LoadModel(oModel: THL7V2Model);
const
  STEP_COUNT = 12;
begin
  if not FInitialised then
    begin
    FInitialised := true;
    Initialise;
    end;

  if Progress(oModel.Version, 'Loading Tables', 0, STEP_COUNT) then
    Abort;
  LoadTables(oModel.Version, oModel.Tables);
  if Progress(oModel.Version, 'Loading DataTypes', 1, STEP_COUNT) then
    abort;
  LoadDataTypes(oModel.Version, oModel.DataTypes);
  if Progress(oModel.Version, 'Loading Components', 2, STEP_COUNT) then
    abort;
  LoadComponents(oModel.Version, oModel.Components);
  if Progress(oModel.Version, 'Loading Data Structures', 3, STEP_COUNT) then
    abort;
  LoadStructures(oModel.Version, oModel.Structures);
  if Progress(oModel.Version, 'Loading StructureComponents', 4, STEP_COUNT) then
    abort;
  LoadStructureComponents(oModel.Version, oModel.Structures, oModel.Components);
  if Progress(oModel.Version, 'Loading DataElements', 5, STEP_COUNT) then
    abort;
  LoadDataElements(oModel.Version, oModel.DataElements);

  if Progress(oModel.Version, 'Loading Segments', 6, STEP_COUNT) then
    abort;
  LoadSegments(oModel.Version, oModel.Segments);

  if Progress(oModel.Version, 'Loading Message Structures', 8, STEP_COUNT) then
    abort;
  LoadMessageStructures(oModel.Version, oModel.MessageStructures);

  if Progress(oModel.Version, 'Loading Segment Structures', 9, STEP_COUNT) then
    abort;
  LoadSegmentMaps(oModel.Version, oModel.MessageStructures);

  if Progress(oModel.Version, 'Loading Events', 10, STEP_COUNT) then
    abort;
  LoadEvents(oModel.Version, oModel.Events);
  if Progress(oModel.Version, 'Loading Event Messages', 11, STEP_COUNT) then
    abort;
  LoadEventMessages(oModel.Version, oModel.Events);
  if Progress(oModel.Version, 'Loaded', 12, STEP_COUNT) then
    abort;
end;

procedure THL7V2Dictionary.SaveModel(oModel: THL7V2Model; const sDesc : String);
const
  STEP_COUNT = 12;
begin
  AddVersion(oModel.Version, sDesc);
  if Progress(oModel.Version, 'Saving Tables', 0, STEP_COUNT) then
    Abort;
  AddTables(oModel.Version, oModel.Tables);
  if Progress(oModel.Version, 'Saving DataTypes', 1, STEP_COUNT) then
    abort;
  AddDataTypes(oModel.Version, oModel.DataTypes);
  if Progress(oModel.Version, 'Saving Components', 2, STEP_COUNT) then
    abort;
  AddComponents(oModel.Version, oModel.Components);
  if Progress(oModel.Version, 'Saving Data Structures', 3, STEP_COUNT) then
    abort;
  AddStructures(oModel.Version, oModel.Structures);
  if Progress(oModel.Version, 'Saving StructureComponents', 4, STEP_COUNT) then
    abort;
  AddStructureComponents(oModel.Version, oModel.Structures);
  if Progress(oModel.Version, 'Saving DataElements', 5, STEP_COUNT) then
    abort;
  AddDataElements(oModel.Version, oModel.DataElements);
  if Progress(oModel.Version, 'Saving Segments', 6, STEP_COUNT) then
    abort;
  AddSegments(oModel.Version, oModel.Segments);
  if Progress(oModel.Version, 'Saving Fields', 7, STEP_COUNT) then
    abort;
  AddSegmentFields(oModel.Version, oModel.Segments);
  if Progress(oModel.Version, 'Saving Message Structures', 8, STEP_COUNT) then
    abort;
  AddMessageStructures(oModel.Version, oModel.MessageStructures);
  if Progress(oModel.Version, 'Saving Message Structures', 9, STEP_COUNT) then
    abort;
  AddSegmentMaps(oModel.Version, oModel.MessageStructures);
  if Progress(oModel.Version, 'Saving Events', 10, STEP_COUNT) then
    abort;
  AddEvents(oModel.Version, oModel.Events);
  if Progress(oModel.Version, 'Saving Event Messages', 11, STEP_COUNT) then
    abort;
  AddEventMessages(oModel.Version, oModel.Events);
  if Progress(oModel.Version, 'Complete', 12, STEP_COUNT) then
    abort;
end;

function THL7V2Dictionary.GetVersions: THL7V2Versions;
begin
  if not FVersionsLoaded then
    LoadVersions;
  result := FVersions;
end;

procedure THL7V2Dictionary.LoadVersions;
var
  aVersion : THL7V2Version;
  sDesc : String;
begin
  if not FInitialised then
    Initialise;
  FVersions := [];
  for aVersion := Low(THL7V2Version) to High(THL7V2Version) do
    if VersionDefined(aVersion, sDesc) then
      include(FVersions, aVersion);
  FVersionsLoaded := true;
end;

function THL7V2Dictionary.Progress(aVersion: THL7V2Version; const sTable: String; iCount, iTotal: Integer): Boolean;
begin
  result := False;
  if assigned(FOnTransferProgress) then
    FOnTransferProgress(self, NAMES_HL7V2_VERSION[aVersion], sTable, iCount, iTotal, result);
end;

procedure THL7V2Dictionary.Initialise;
begin
end;

function THL7V2Dictionary.HasVersion(aVer: THL7V2Version): Boolean;
begin
  result := aVer in Versions;
end;

function THL7V2Dictionary.VersionLoaded(aVer: THL7V2Version): Boolean;
begin
  FLock.Lock;
  Try
    result := assigned(FModels[aVer]);
  Finally
    FLock.Unlock;
  End;
end;

procedure THL7V2Dictionary.SetSchemaStore(const Value: THL7V2SchemaStore);
begin
  FSchemaStore.Free;
  FSchemaStore := Value;
end;

function THL7V2Dictionary.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSchemaStore.sizeInBytes);
end;

{ THL7V2BinaryDictionaryContext }

destructor THL7V2BinaryDictionaryContext.Destroy;
begin
  FReader.Free;
  FWriter.Free;
  FStream.Free;
  inherited;
end;

procedure THL7V2BinaryDictionaryContext.Read(oBUffer : TFslBuffer);
begin
  assert(FStream = nil, 'the context is already open');
  FStream := TMemoryStream.create;
  if oBUffer.AsText <> '' then
    FStream.Write(oBUffer.AsText[1], length(oBUffer.AsText));
  FStream.Position := 0;
  FReader := TReader.create(FStream, 8192);
  FReader.ReadListBegin;
end;

procedure THL7V2BinaryDictionaryContext.Write;
begin
  assert(FStream = nil, 'the context is already open');
  FStream := TMemoryStream.create;
  FWriter := TWriter.create(FStream, 8192);
  FWriter.WriteListBegin;
end;

function THL7V2BinaryDictionaryContext.CloseWriting: String;
begin
  FWriter.WriteListEnd;
  FWriter.Free;
  FWriter := nil;
  FStream.position := 0;
  SetLength(Result, FStream.size);
  FStream.Read(Result[1], FStream.size);
end;

{ THL7V2BinaryDictionaryContexts }

function THL7V2BinaryDictionaryContexts.GetByName(const sName: String): THL7V2BinaryDictionaryContext;
begin
  result := THL7V2BinaryDictionaryContext(inherited GetByName(sName));
end;

function THL7V2BinaryDictionaryContexts.GetStream(iIndex : Integer): THL7V2BinaryDictionaryContext;
begin
  result := THL7V2BinaryDictionaryContext(ObjectByIndex[iIndex]);
end;

function THL7V2BinaryDictionaryContexts.ItemClass: TFslObjectClass;
begin
  result := THL7V2BinaryDictionaryContext;
end;

{ THL7V2BinaryDictionary }

constructor THL7V2BinaryDictionary.Create;
begin
  inherited;
  FContexts := THL7V2BinaryDictionaryContexts.create;
  FBuffers := TFslNameBufferList.create;
end;

destructor THL7V2BinaryDictionary.Destroy;
begin
  FContexts.Free;
  FBuffers.Free;
  inherited;
end;

procedure THL7V2BinaryDictionary.LoadDictionary;
var
  oContents : TFslAccessStream;
  oMem : TMemoryStream;
  r: TReader;
  oBuffer : TFslNameBuffer;
begin
  FBUffers.Clear;
  FIsWriting := False;
  oContents := GetContents(false);
  if assigned(oContents) then
    try
      oMem := TMemoryStream.create;
      try
        oMem.SetSize(oContents.Size);
        oContents.Read(oMem.Memory^, oContents.Size);
        oMem.Position := 0;
        r := TReader.Create(oMem, 65536);
        try
          FLicenced := r.ReadBoolean;
          r.ReadListBegin;
          while not r.EndOfList do
            begin
            oBuffer := TFslNameBuffer.create;
            try
              oBuffer.Name := r.readString;
              oBuffer.AsText := r.readstring;
              FBuffers.Add(oBuffer.Link);
            finally
              oBuffer.Free;
            end;
            end;
          r.ReadListEnd;
        finally
          r.Free;
          end;
      finally
        oMem.Free;
        end;
    finally
      oContents.Free;
    end;
end;

{------------------------------------------------------------------------------
  Helper function
 ------------------------------------------------------------------------------}

function THL7V2BinaryDictionary.GetReadingContext(const sName: String): THL7V2BinaryDictionaryContext;
var
  oBuffer : TFslNameBuffer;
//  s: TMemoryStream;
//  r: TReader;
begin
  oBuffer := FBuffers.GetByName(sName);
  if not assigned(oBuffer) then
    RaiseError('GetReadingContext', 'No Context for '+sName+' found in '+IntegerToString(FBUffers.Count)+' entries');
  result := THL7V2BinaryDictionaryContext.create;
  result.Read(oBuffer);
end;

function THL7V2BinaryDictionary.GetWritingContext(const sName: String): THL7V2BinaryDictionaryContext;
begin
  result := FContexts.GetByName(sName);
  if not FIsWriting or (assigned(result) and (result.Writer = nil)) then
    RaiseError('GetWritingContext', 'Call to add a record without first calling PrepareForLoad');

  if not assigned(result) then
    begin
    result := THL7V2BinaryDictionaryContext.create;
    result.Name := sName;
    FContexts.add(result);
    result.write;
    end;
end;

procedure THL7V2BinaryDictionary.PrepareForLoad(bWipe: Boolean);
begin
  FContexts.Clear;
  FisWriting := True;
end;

procedure THL7V2BinaryDictionary.SaveToStream(oStream : TMemoryStream);
var
  w: TWriter;
  iLoop : integer;
  oContext : THL7V2BinaryDictionaryContext;
begin
  w := TWriter.Create(oStream, 65536);
  try
    w.WriteBoolean(True);
    w.WriteListBegin;
    for iLoop := 0 to FContexts.Count - 1 do
      begin
      oContext := FContexts[iLoop];
      w.WriteString(oContext.Name);
      w.WriteString(oContext.CloseWriting);
      end;
    w.WriteListEnd;
  finally
    w.Free;
  end;
end;

procedure THL7V2BinaryDictionary.DoneLoading(aTransferEvent: TOnDictTransferProgress);
var
  oStream : TMemoryStream;
  bDummy: Boolean;
  oContents : TFslAccessStream;
begin
  aTransferEvent(self, 'All', 'Closing Contexts', 0, 3, bDummy);
  oStream := TMemoryStream.create;
  try
    SaveToStream(oStream);

    aTransferEvent(self, 'All', 'Saving', 2, 3, bDummy);
    oStream.Position := 0;
    oContents := GetContents(true);
    try
      oContents.Write(oStream.Memory^, oStream.Size);
    finally
      oContents.Free;
    end;
  finally
    oStream.Free;
  end;
  aTransferEvent(self, 'All', 'Saving', 3, 3, bDummy);
  LoadDictionary;
end;

{------------------------------------------------------------------------------
  Versions
 ------------------------------------------------------------------------------}

function THL7V2BinaryDictionary.VersionDefined(aVersion : THL7V2Version; var sDesc: String): Boolean;
var
  oContext : THL7V2BinaryDictionaryContext;
begin
  Result := False;
  oContext := THL7V2BinaryDictionaryContext(GetReadingContext('Versions'));
  try
    while not oContext.Reader.EndOfList and not result do
      begin
      Result := (NAMES_HL7V2_VERSION[aVersion] = oContext.Reader.ReadString);
      if Result then
        sDesc := oContext.Reader.readString
      else
        oContext.Reader.ReadString;
      end;
  finally
    oContext.Free;
    end;
end;

Function THL7V2BinaryDictionary.ListVersions : THL7V2Versions;
var
  oContext : THL7V2BinaryDictionaryContext;
begin
  result := [];
  oContext := THL7V2BinaryDictionaryContext(GetReadingContext('Versions'));
  try
    while not oContext.Reader.EndOfList do
      begin
      include(result, THL7V2Version(StringArrayIndexOfInsensitive(NAMES_HL7V2_VERSION, oContext.Reader.ReadString)));
      oContext.Reader.ReadString;
      end;
  finally
    oContext.Free;
    end;
end;

procedure THL7V2BinaryDictionary.AddVersion(aVersion : THL7V2Version; const sDescription: String);
begin
  with GetWritingContext('Versions').Writer do
    begin
    writestring(NAMES_HL7V2_VERSION[aVersion]);
    WriteString(sDescription);
    end;
end;

{------------------------------------------------------------------------------
  Fields
 ------------------------------------------------------------------------------}
procedure THL7V2BinaryDictionary.LoadSegmentFields(aVersion : THL7V2Version; oSegment : THL7V2ModelSegment);
var
  oContext : THL7V2BinaryDictionaryContext;
  oField : THL7V2ModelField;
  sRep : String;
begin
  if length(oSegment.Code) = 3 then
    begin
    oContext := GetReadingContext('Fields-' + NAMES_HL7V2_VERSION[aVersion] + '-' + oSegment.Code);
    try
      while not oContext.Reader.EndOfList do
        begin
        oField := THL7V2ModelField.create;
        try
          oContext.Reader.ReadString; // ignore segment code
          oField.DataElement := oContext.Reader.ReadInteger;
          sRep := oContext.Reader.ReadString;
          oField.Required := (oContext.Reader.ReadString = 'R');
          oField.RepeatCount := oContext.Reader.ReadInteger;
          oField.Repeatable := (oField.RepeatCount > 0) or (sRep = 'Y');
          oField.FieldNumber := oContext.Reader.ReadInteger;
          oSegment.Fields.Add(oField.Link);
        finally
          oField.Free;
        end;
        end;
    finally
      oContext.Free;
    end;
    end;
end;

procedure THL7V2BinaryDictionary.AddSegmentFields(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments);
var
  iSegment : integer;
  iField : Integer;
  oSegment : THL7V2ModelSegment;
  oField : THL7V2ModelField;
begin
  for iSegment := 0 to oSegments.Count - 1 do
    begin
    oSegment := oSegments[iSegment];
    with GetWritingContext('Fields-' + NAMES_HL7V2_VERSION[aVersion] + '-' + oSegment.Code).Writer do
      begin
      for iField := 0 to oSegment.Fields.Count - 1 do
        begin
        oField := oSegment.Fields[iField];
        WriteString(oSegment.Code);
        WriteInteger(oField.DataElement);
        if oField.Repeatable then
          WriteString('Y')
        else
          WriteString('');
        if oField.Required then
          WriteString('R')
        else
          WriteString('');
        WriteInteger(oField.RepeatCount);
        WriteInteger(oField.FieldNumber);
        end;
      end;
    end;
end;

procedure THL7V2BinaryDictionary.LoadComponents(aVersion : THL7V2Version; oComponents : THL7V2ModelComponents);
var
  oContext : THL7V2BinaryDictionaryContext;
  oComponent : THL7V2ModelComponent;
begin
  oContext := GetReadingContext('Components-' + NAMES_HL7V2_VERSION[aVersion]);
  try
    while not oContext.Reader.EndOfList do
      begin
      oComponent := THL7V2ModelComponent.create;
      try
        oComponent.Name := oContext.Reader.ReadString;
        oComponent.Table := oContext.Reader.ReadInteger;
        oComponent.DataType:= oContext.Reader.ReadString;
        oComponent.Number := oContext.Reader.ReadInteger;
        oComponents.Add(oComponent.Link);
      finally
        oComponent.Free;
      end;
      end;
  finally
    oContext.Free;
  end;
end;

procedure THL7V2BinaryDictionary.AddComponents(aVersion : THL7V2Version; oComponents : THL7V2ModelComponents);
var
  iLoop : integer;
  oComponent : THL7V2ModelComponent;
begin
  with GetWritingContext('Components-' + NAMES_HL7V2_VERSION[aVersion]).Writer do
    for iLoop := 0 to oComponents.Count - 1 do
      begin
      oComponent := oComponents[iLoop];
      WriteString(oComponent.Name);
      WriteInteger(oComponent.Table);
      WriteString(oComponent.DataType);
      WriteInteger(oComponent.Number);
      end;
end;

procedure THL7V2BinaryDictionary.LoadDataElements(aVersion : THL7V2Version; oDataElements : THL7V2ModelDataElements);
var
  oContext : THL7V2BinaryDictionaryContext;
  oDataElement : THL7V2ModelDataElement;
begin
  oContext := GetReadingContext('DataElements-' + NAMES_HL7V2_VERSION[aVersion]);
  try
    while not oContext.Reader.EndOfList do
      begin
      oDataElement := THL7V2ModelDataElement.create;
      try
        oDataElement.Description := oContext.Reader.ReadString;
        oDataElement.Structure := oContext.Reader.ReadString;
        if aVersion >= hv27 then
        begin
          oDataElement.Length_Min := oContext.Reader.ReadInteger;
          oDataElement.Length_Max := oContext.Reader.ReadInteger;
          oDataElement.Length_Conf := oContext.Reader.ReadString;
        end
        else
          oDataElement.Length_Old := oContext.Reader.ReadInteger;
        oDataElement.Table := oContext.Reader.ReadInteger;
        oDataElement.Id := oContext.Reader.ReadInteger;
        oDataElements.Add(oDataElement.Link);
      finally
        oDataElement.Free;
      end;
      end;
  finally
    oContext.Free;
  end;
end;

procedure THL7V2BinaryDictionary.AddDataElements(aVersion : THL7V2Version; oDataElements : THL7V2ModelDataElements);
var
  iLoop : integer;
  oDataElement : THL7V2ModelDataElement;
begin
  with GetWritingContext('DataElements-' + NAMES_HL7V2_VERSION[aVersion]).Writer do
    for iLoop := 0 to oDataElements.Count - 1 do
      begin
      oDataElement := oDataElements[iLoop];
      WriteString(oDataElement.Description);
      WriteString(oDataElement.Structure);
      if aVersion >= hv27 then
      begin
        WriteInteger(oDataElement.Length_Min);
        WriteInteger(oDataElement.Length_Max);
        WriteString(oDataElement.Length_Conf);
      end
      else
        WriteInteger(oDataElement.Length_Old);
      WriteInteger(oDataElement.Table);
      WriteInteger(oDataElement.Id);
      end;
end;

procedure THL7V2BinaryDictionary.LoadDataTypes(aVersion : THL7V2Version; oDataTypes : THL7V2ModelDataTypes);
var
  oContext : THL7V2BinaryDictionaryContext;
  oDataType : THL7V2ModelDataType;
begin
  oContext := GetReadingContext('DataTypes-' + NAMES_HL7V2_VERSION[aVersion]);
  try
    while not oContext.Reader.EndOfList do
      begin
      oDataType := THL7V2ModelDataType.create;
      try
        oDataType.Name := oContext.Reader.ReadString;
        oDataType.Description := oContext.Reader.ReadString;
        oDataType.Length := oContext.Reader.ReadInteger;
        oDataTypes.Add(oDataType.Link);
      finally
        oDataType.Free;
      end;
      end;
  finally
    oContext.Free;
  end;
end;

procedure THL7V2BinaryDictionary.AddDataTypes(aVersion : THL7V2Version; oDataTypes : THL7V2ModelDataTypes);
var
  iLoop : integer;
  oDataType : THL7V2ModelDataType;
begin
  with GetWritingContext('DataTypes-' + NAMES_HL7V2_VERSION[aVersion]).Writer do
    for iLoop := 0 to oDataTypes.Count - 1 do
      begin
      oDataType := oDataTypes[iLoop];
      WriteString(oDataType.Name);
      WriteString(oDataType.Description);
      WriteInteger(oDataType.Length);
      end;
end;

procedure THL7V2BinaryDictionary.LoadSegments(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments);
var
  oContext : THL7V2BinaryDictionaryContext;
  oSegment : THL7V2ModelSegment;
begin
  oContext := GetReadingContext('Segments-' + NAMES_HL7V2_VERSION[aVersion]);
  try
    while not oContext.Reader.EndOfList do
      begin
      oSegment := THL7V2ModelSegment.create;
      try
        oSegment.Code := oContext.Reader.ReadString;
        oSegment.Description := oContext.Reader.ReadString;
        oSegments.Add(oSegment.Link);
      finally
        oSegment.Free;
      end;
      end;
  finally
    oContext.Free;
  end;
  LoadSegmentFields(aVersion, oSegments);
end;

procedure THL7V2BinaryDictionary.AddSegments(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments);
var
  iLoop : integer;
  oSegment : THL7V2ModelSegment;
begin
  for iLoop := 0 to oSegments.Count - 1 do
    begin
    oSegment := oSegments[iLoop];
    with GetWritingContext('Segments-' + NAMES_HL7V2_VERSION[aVersion]).Writer do
      begin
      WriteString(oSegment.Code);
      WriteString(oSegment.Description);
      end;
    end;
end;

procedure THL7V2BinaryDictionary.LoadStructures(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures);
var
  oContext : THL7V2BinaryDictionaryContext;
  oStructure : THL7V2ModelStructure;
begin
  oContext := GetReadingContext('Structures-' + NAMES_HL7V2_VERSION[aVersion]);
  try
    while not oContext.Reader.EndOfList do
      begin
      oStructure := THL7V2ModelStructure.create;
      try
        oStructure.Name := oContext.Reader.ReadString;
        oStructure.Description := oContext.Reader.ReadString;
        oStructure.DataType := oContext.Reader.ReadString;
        oStructure.ID := oContext.Reader.ReadInteger;
        oStructures.Add(oStructure.Link);
      finally
        oStructure.Free;
      end;
      end;
  finally
    oContext.Free;
  end;
end;

procedure THL7V2BinaryDictionary.AddStructures(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures);
var
  iLoop : integer;
  oStructure : THL7V2ModelStructure;
begin
  with GetWritingContext('Structures-' + NAMES_HL7V2_VERSION[aVersion]).Writer do
    for iLoop := 0 to oStructures.Count - 1 do
      begin
      oStructure := oStructures[iLoop];
      WriteString(oStructure.Name);
      WriteString(oStructure.Description);
      WriteString(oStructure.DataType);
      WriteInteger(oStructure.ID);
      end;
end;

procedure THL7V2BinaryDictionary.LoadStructureComponents(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures; oComponents : THL7V2ModelComponents);
var
  oContext : THL7V2BinaryDictionaryContext;
  oStructure : THL7V2ModelStructure;
  oComp : THL7V2ModelComponent;
  sStruc : String;
  sLast : String;
begin
  oStructure := nil;
  oContext := GetReadingContext('StructureComps-' + NAMES_HL7V2_VERSION[aVersion]);
  try
    sLast := '';
    while not oContext.Reader.EndOfList do
      begin
      sStruc := oContext.Reader.ReadString;
      if sStruc <> sLast then
        begin
        sLast := sStruc;
        oStructure := oStructures.GetByName(sLast);
        end;
      oContext.Reader.ReadInteger; // ignore this - FNum - not used
      oComp  := oComponents.GetByNumber(oContext.Reader.ReadInteger);
      oStructure.Components.add(oComp.Link);
      end;
  finally
    oContext.Free;
  end;
End;

procedure THL7V2BinaryDictionary.AddStructureComponents(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures);
var
  iStruc : Integer;
  oStructure : THL7V2ModelStructure;
  iComp : Integer;
begin
  with GetWritingContext('StructureComps-' + NAMES_HL7V2_VERSION[aVersion]).Writer do
    for iStruc := 0 to oStructures.Count - 1 do
      begin
      oStructure := oStructures[iStruc];
      for iComp := 0 to oStructure.Components.Count - 1 do
        begin
        WriteString(oStructure.Name);
        WriteInteger(0); // not used anymore?
        WriteInteger(oStructure.Components[iComp].Number);
        end;
      end;
end;

procedure THL7V2BinaryDictionary.LoadTables(aVersion : THL7V2Version; oTables : THL7V2ModelTables);
var
  oContext : THL7V2BinaryDictionaryContext;
  oTable : THL7V2ModelTable;
  oTableItem : THL7V2ModelTableItem;
  iId : integer;
begin
  oContext := GetReadingContext('Tables-' + NAMES_HL7V2_VERSION[aVersion]);
  try
    while not oContext.Reader.EndOfList do
      begin
      oTable := THL7V2ModelTable.create;
      try
        oTable.Description := oContext.Reader.ReadString;
        oTable.ID := oContext.Reader.ReadInteger;
        oTables.Add(oTable.Link);
      finally
        oTable.Free;
      end;
      end;
  finally
    oContext.Free;
  end;
  oContext := GetReadingContext('tableitems-' + NAMES_HL7V2_VERSION[aVersion]);
  try
    while not oContext.Reader.EndOfList do
      begin
      oTableItem := THL7V2ModelTableItem.create;
      try
        iId := oContext.Reader.ReadInteger;
        oTableItem.ID := oContext.Reader.ReadInteger;
        oTableItem.Code := oContext.Reader.ReadString;
        oTableItem.Description := oContext.Reader.ReadString;
        oTables.GetByID(iId).Items.Add(oTableItem.Link);
      finally
        oTableItem.Free;
      end;
      end;
  finally
    oContext.Free;
  end;
end;

procedure THL7V2BinaryDictionary.AddTables(aVersion : THL7V2Version; oTables : THL7V2ModelTables);
var
  iLoop : integer;
  iInner : Integer;
  oTable : THL7V2ModelTable;
  oItem : THL7V2ModelTableItem;
begin
  with GetWritingContext('Tables-' + NAMES_HL7V2_VERSION[aVersion]).Writer do
    for iLoop := 0 to oTables.Count - 1 do
      begin
      oTable := oTables[iLoop];
      WriteString(oTable.Description);
      WriteInteger(oTable.ID);
      end;
  with GetWritingContext('tableitems-' + NAMES_HL7V2_VERSION[aVersion]).Writer do
    for iLoop := 0 to oTables.Count - 1 do
      begin
      oTable := oTables[iLoop];
      for iInner := 0 to oTable.Items.count - 1 do
        begin
        oItem := oTable.Items[iInner];
        WriteInteger(oTable.ID);
        WriteInteger(oItem.ID);
        WriteString(oItem.Code);
        WriteString(oItem.Description);
        end;
      end;
end;

procedure THL7V2BinaryDictionary.LoadEvents(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents);
var
  oContext : THL7V2BinaryDictionaryContext;
  oEvent : THL7V2ModelEvent;
begin
  oContext := GetReadingContext('Events-' + NAMES_HL7V2_VERSION[aVersion]);
  try
    while not oContext.Reader.EndOfList do
      begin
      oEvent := THL7V2ModelEvent.create;
      try
        oEvent.Name := oContext.Reader.ReadString;
        oEvent.Description := oContext.Reader.ReadString;
        oEvents.Add(oEvent.Link);
      finally
        oEvent.Free;
      end;
      end;
  finally
    oContext.Free;
  end;
end;

procedure THL7V2BinaryDictionary.AddEvents(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents);
var
  iLoop : integer;
  oEvent : THL7V2ModelEvent;
begin
  with GetWritingContext('Events-' + NAMES_HL7V2_VERSION[aVersion]).Writer do
    for iLoop := 0 to oEvents.Count - 1 do
      begin
      oEvent := oEvents[iLoop];
      WriteString(oEvent.Name);
      WriteString(oEvent.Description);
      end;
end;

procedure THL7V2BinaryDictionary.LoadEventMessages(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents);
var
  oContext : THL7V2BinaryDictionaryContext;
  oEvent : THL7V2ModelEvent;
  oMessage : THL7V2ModelEventMessage;
begin
  oContext := GetReadingContext('EventMsgType-' + NAMES_HL7V2_VERSION[aVersion]);
  try
    while not oContext.Reader.EndOfList do
      begin
      oEvent := oEVents.GetByName(oContext.Reader.ReadString);
      oMessage := THL7V2ModelEventMessage.create;
      try
        oMessage.Message := oContext.Reader.ReadString;
        oMessage.Structure := oContext.Reader.ReadString;
        oMessage.Reply := oContext.Reader.ReadString;
        oMessage.ReplyStructure := oContext.Reader.ReadString;
        oEvent.Messages.add(oMessage.Link);
      finally
        oMessage.Free;
      end;
      end;
  finally
    oContext.Free;
  end;
end;

procedure THL7V2BinaryDictionary.AddEventMessages(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents);
var
  iEvent : Integer;
  iMessage : integer;
  oEvent : THL7V2ModelEvent;
  oMessage : THL7V2ModelEventMessage;
begin
  with GetWritingContext('EventMsgType-' + NAMES_HL7V2_VERSION[aVersion]).Writer do
    for iEvent := 0 to oEvents.Count - 1 do
      begin
      oEvent := oEvents[iEvent];
      for iMessage := 0 to oEvent.Messages.Count - 1 do
        begin
        oMessage := oEvent.Messages[iMessage];
        WriteString(oEvent.Name);
        WriteString(oMessage.Message);
        WriteString(oMessage.Structure);
        WriteString(oMessage.Reply);
        WriteString(oMessage.ReplyStructure);
        end;
      end;
end;

procedure THL7V2BinaryDictionary.LoadMessageStructures(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures);
var
  oContext : THL7V2BinaryDictionaryContext;
  oMessageStructure : THL7V2ModelMessageStructure;
begin
  oContext := GetReadingContext('MsgStructs-' + NAMES_HL7V2_VERSION[aVersion]);
  try
    while not oContext.Reader.EndOfList do
      begin
      oMessageStructure := THL7V2ModelMessageStructure.create;
      try
        oMessageStructure.Name := oContext.Reader.ReadString;
        oMessageStructure.Description := oContext.Reader.ReadString;
        oMessageStructure.ExampleEvent := oContext.Reader.ReadString;
        oMessageStructure.ExampleMsgType := oContext.Reader.ReadString;
        oMessageStructure.Action := oContext.Reader.ReadString;
        oMessageStructures.Add(oMessageStructure.Link);
      finally
        oMessageStructure.Free;
      end;
      end;
  finally
    oContext.Free;
  end;
end;

procedure THL7V2BinaryDictionary.AddMessageStructures(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures);
var
  iLoop : integer;
  oMessageStructure : THL7V2ModelMessageStructure;
begin
  with GetWritingContext('MsgStructs-' + NAMES_HL7V2_VERSION[aVersion]).Writer do
    for iLoop := 0 to oMessageStructures.Count - 1 do
      begin
      oMessageStructure := oMessageStructures[iLoop];
      WriteString(oMessageStructure.Name);
      WriteString(oMessageStructure.Description);
      WriteString(oMessageStructure.ExampleEvent);
      WriteString(oMessageStructure.ExampleMsgType);
      WriteString(oMessageStructure.Action);
      end;
end;

procedure THL7V2BinaryDictionary.ReadSegmentGroup(oContext : THL7V2BinaryDictionaryContext; oSegmentGroup : THL7V2ModelSegmentGroup);
var
  iTotal : integer;
  iLoop : integer;
  oChild : THL7V2ModelSegmentGroup;
begin
  oSegmentGroup.Code := oContext.Reader.ReadString;
  oSegmentGroup.Optional := oContext.Reader.ReadBoolean;
  oSegmentGroup.Repeating := oContext.Reader.ReadBoolean;
  oSegmentGroup.GroupType := THL7V2ModelSegmentGroupType(oContext.Reader.ReadInteger);
  iTotal := oContext.Reader.ReadInteger;
  for iLoop := 1 to iTotal do
    begin
    oChild := THL7V2ModelSegmentGroup.create;
    try
      ReadSegmentGroup(oContext, oChild);
      oSegmentGroup.Children.add(oChild.Link);
    finally
      oChild.Free;
    end;
    end;
end;

procedure THL7V2BinaryDictionary.LoadSegmentMaps(aVersion : THL7V2Version; oStructures : THL7V2ModelMessageStructures);
var
  oContext : THL7V2BinaryDictionaryContext;
  iLoop : integer;
  oStructure : THL7V2ModelMessageStructure;
  oSegmentGroup : THL7V2ModelSegmentGroup;
begin
  for iLoop := 0 to oStructures.Count - 1 do
    begin
    oStructure := oStructures[iLoop];
    if FBuffers.GetByName('MsgStruct-' + NAMES_HL7V2_VERSION[aVersion]+'-'+oStructure.Name) <> nil then
      begin
      oContext := GetReadingContext('MsgStruct-' + NAMES_HL7V2_VERSION[aVersion]+'-'+oStructure.Name);
      try
        if oContext.Reader.ReadBoolean then
          begin
          oSegmentGroup := THL7V2ModelSegmentGroup.create;
            try
            ReadSegmentGroup(oContext, oSegmentGroup);
            oStructure.SegmentMap := oSegmentGroup.Link;
          finally
            oSegmentGroup.Free;
          end;
          end;
      finally
        oContext.Free;
      end;
      end;
    end;
end;

procedure THL7V2BinaryDictionary.WriteSegmentGroup(oContext : THL7V2BinaryDictionaryContext; oSegmentGroup : THL7V2ModelSegmentGroup);
var
  iLoop : integer;
begin
  oContext.Writer.WriteString(oSegmentGroup.Code);
  oContext.Writer.WriteBoolean(oSegmentGroup.Optional);
  oContext.Writer.WriteBoolean(oSegmentGroup.Repeating);
  oContext.Writer.WriteInteger(integer(oSegmentGroup.GroupType));
  oContext.Writer.WriteInteger(oSegmentGroup.Children.count);
  for iLoop := 0 to oSegmentGroup.Children.count - 1 do
    WriteSegmentGroup(oContext, oSegmentGroup.Children[iLoop]);
end;

procedure THL7V2BinaryDictionary.AddSegmentMaps(aVersion : THL7V2Version; oStructures : THL7V2ModelMessageStructures);
var
  oContext : THL7V2BinaryDictionaryContext;
  iLoop : integer;
  oStructure : THL7V2ModelMessageStructure;
begin
  for iLoop := 0 to oStructures.Count - 1 do
    begin
    oStructure := oStructures[iLoop];
    oContext := GetwritingContext('MsgStruct-' + NAMES_HL7V2_VERSION[aVersion]+'-'+oStructure.Name);
    oContext.Writer.WriteBoolean(assigned(oStructure.SegmentMap));
    if assigned(oStructure.SegmentMap) then
      writeSegmentGroup(oContext, oStructure.SegmentMap);
    end;
end;

function THL7V2BinaryDictionary.GetContents(bForWriting: Boolean): TFslAccessStream;
begin
  result := nil;
  RaiseError('GetContents', 'Need to override GetContents in '+ClassName);

end;

procedure THL7V2BinaryDictionary.Initialise;
begin
  inherited;
  LoadDictionary;
end;

function THL7V2BinaryDictionary.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FBuffers.sizeInBytes);
  inc(result, FContexts.sizeInBytes);
end;

{ THL7V2FileDictionary }

constructor THL7V2FileDictionary.Create(const sFilename : String; bSuppressException: Boolean = False);
begin
  Create;

  FFileName := sFilename;
  FErrorsOk := bSuppressException;
end;

function THL7V2FileDictionary.GetContents(bForWriting : Boolean) : TFslAccessStream;
begin
  if bForWriting then
    result := TFslFile.Create(FFilename, fmCreate)
  else
  begin
    result := nil;
    if FileExists(FFilename) then
      result := TFslFile.Create(FFilename, fmOpenRead + fmShareDenyWrite)
    else if not FErrorsOk then
      RaiseError('GetContents', 'Unable to find file ['+FFilename+']');
  end;
end;

function THL7V2FileDictionary.SourceDescription(bFullDetails: Boolean): String;
begin
  if bFullDetails then
    Result := 'File:' + FFileName
  else
    Result := 'File';
end;

function THL7V2FileDictionary.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FFileName.length * sizeof(char)) + 12);
end;

{ THL7V2DictionaryProvider }

constructor THL7V2DictionaryProvider.Create(oDictionary: THL7V2Dictionary);
begin
  Create;
  FDictionary := oDictionary;
end;

constructor THL7V2DictionaryProvider.Create(oProvider: THL7V2DictionaryProvider);
begin
  Create;
  FDictionary := oProvider.Dictionary.Link;
end;

destructor THL7V2DictionaryProvider.Destroy;
begin
  FDictionary.Free;
  inherited;
end;

procedure THL7V2DictionaryProvider.SetDictionary(const Value: THL7V2Dictionary);
begin
  FDictionary.Free;
  FDictionary := Value;
end;

function THL7V2DictionaryProvider.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDictionary.sizeInBytes);
end;

end.
