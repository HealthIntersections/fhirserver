Unit Ucum;

{
Copyright (c) 2001-2013, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  Sysutils,
  DecimalSupport,
  AdvObjects,
  AdvNames,
  AdvStringLists,
  AdvPersistentLists;

Const Ucum_CACHE_VERSION = 3;

type
  TConceptKind  = (UcumNull, UcumPREFIX, UcumBASEUNIT, UcumUNIT);

  TUcumProperty = class (TAdvName)
  private
    FCommonUnits : TAdvStringList;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    function Link : TUcumProperty; Overload;
    Procedure Define(oFiler : TAdvFiler); Override;

    Property CommonUnits : TAdvStringlist read FCommonUnits;
  end;

  TUcumPropertyList = class (TAdvNameList)
  Private
    function GetUcumProperty(iIndex : integer): TUcumProperty;
  Protected
    Function ItemClass : TAdvObjectClass; override;
  Public
    Property UcumProperty[iIndex : integer] : TUcumProperty read GetUcumProperty; default;
  End;

  TUcumConcept = class (TAdvPersistent)
  private
    Fkind : TConceptKind;
    Fcode : String;
    FcodeUC : String;
    FprintSymbol : String;
    Fnames : TAdvStringList;
    FText: String;
  protected
    Function GetKind : TConceptKind; virtual;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    function Link : TUcumConcept; Overload;
    Procedure Define(oFiler : TAdvFiler); Override;

    Property kind : TConceptKind read Getkind;
    Property code : String read Fcode write FCode;    // case sensitive code for this concept
    Property codeUC : String read FcodeUC write FcodeUC;    // case insensitive code for this concept
    Property printSymbol : String read FprintSymbol write FPrintSymbol;    // print symbol for this code
    Property names : TAdvStringList read Fnames;    // names for the concept
    Property Text : String read FText write FText;
  End;

  TUcumConceptList = class (TAdvPersistentList)
  Private
    function GetUcumItem(iIndex : integer): TUcumConcept;
  Protected
    Function ItemClass : TAdvObjectClass; override;
  Public
    Property UcumItem[iIndex : integer] : TUcumConcept read GetUcumItem; default;
  End;

  TUcumPrefix = class (TUcumConcept)
  private
    Fvalue : TSmartDecimal;
  protected
    Function GetKind : TConceptKind; Override;
  public

    function Link : TUcumPrefix; Overload;
    Procedure Define(oFiler : TAdvFiler); Override;
    Property value : TSmartDecimal read Fvalue write FValue;  //value for the prefix - 1^-24 through to 1^24

    procedure SetPrecision(i : integer);
  End;

  TUcumPrefixList = class (TAdvPersistentList)
  Private
    function GetUcumItem(iIndex : integer): TUcumPrefix;
  Protected
    Function ItemClass : TAdvObjectClass; override;
  Public
    Property UcumItem[iIndex : integer] : TUcumPrefix read GetUcumItem; default;
  End;

  TUcumUnit = class (TUcumConcept)
  private
    FProperty : Integer;
  Public
    function Link : TUcumUnit; Overload;
    Procedure Define(oFiler : TAdvFiler); Override;
    Property PropertyType : Integer read FProperty write FProperty; // the kind of thing this represents
  End;

  TUcumBaseUnit = class (TUcumUnit)
  private
    Fdim : Char;
  protected
    Function GetKind : TConceptKind; Override;
  public
    function Link : TUcumBaseUnit; Overload;
    Procedure Define(oFiler : TAdvFiler); Override;
    Property dim : Char read FDim write FDim;
  End;

  TUcumBaseUnitList = class (TAdvPersistentList)
  Private
    function GetUcumItem(iIndex : integer): TUcumBaseUnit;
  Protected
    Function ItemClass : TAdvObjectClass; override;
  Public
    Function ExistsByCode(const sCode : String) : Boolean;
    Function GetByCode(const sCode : String) : TUcumBaseUnit;

    Property UcumItem[iIndex : integer] : TUcumBaseUnit read GetUcumItem; default;
  End;

  TUcumValue = class (TAdvPersistent)
  private
    Funit : String;
    FunitUC : String;
    Fvalue : TSmartDecimal;
    Ftext : String;
  public
    function Link : TUcumValue; Overload;
    Procedure Define(oFiler : TAdvFiler); Override;
    Property unit_ : String read Funit write FUnit;
    Property unitUC : String read FunitUC write FUnitUC;
    Property value : TSmartDecimal read FValue write FValue;
    Property text : String read Ftext write FText;

    procedure SetPrecision(i : integer);
  End;

  TUcumDefinedUnit = class (TUcumUnit)
  private
    Fmetric : boolean;
    FisSpecial : boolean;
    Fclass_ : String;
    Fvalue : TUcumValue;
  protected
    Function GetKind : TConceptKind; Override;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    function Link : TUcumDefinedUnit; Overload;
    Procedure Define(oFiler : TAdvFiler); Override;
    Property metric : boolean read Fmetric write FMetric; // whether this is a metric unit or not
    Property isSpecial : boolean read FisSpecial write FIsSpecial; // special means?
    Property class_ : String read Fclass_ write FClass_; // The class of this unit
    Property value : TUcumValue read Fvalue; // Value details
  End;

  TUcumDefinedUnitList = class (TAdvPersistentList)
  Private
    function GetUcumItem(iIndex : integer): TUcumDefinedUnit;
  Protected
    Function ItemClass : TAdvObjectClass; override;
  Public
    Function ExistsByCode(const sCode : String) : Boolean;
    Function GetByCode(const sCode : String) : TUcumDefinedUnit;

    Property UcumItem[iIndex : integer] : TUcumDefinedUnit read GetUcumItem; default;
  End;


  TUcumModel = class (TAdvPersistent)
  private
    FProperties : TUcumPropertyList;
    Fprefixes : TUcumPrefixList;
    FbaseUnits : TUcumBaseUnitList;
    FdefinedUnits : TUcumDefinedUnitList;
    FVersion : String;
    FRevisionDate : String;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    function Link : TUcumModel; Overload;
    Procedure Define(oFiler : TAdvFiler); Override;

    Procedure clear;

    Function GetUnit(sCode : String) : TUcumUnit;

    Property prefixes : TUcumPrefixList read Fprefixes;
    Property baseUnits : TUcumBaseUnitList read FbaseUnits;
    Property definedUnits : TUcumDefinedUnitList read FdefinedUnits;
    Property Version : String read FVersion write FVersion;
    Property RevisionDate : String read FRevisionDate write FRevisionDate;
    Property Properties : TUcumPropertyList read FProperties;
  End;

const
  CODES_CONCEPT_KIND : array [TConceptKind] of string = ('null', 'PREFIX', 'BASEUNIT', 'UNIT');

implementation

{ TUcumConcept }

constructor TUcumConcept.Create;
begin
  inherited;
  Fnames := TAdvStringList.Create;
end;

procedure TUcumConcept.Define(oFiler: TAdvFiler);
begin
  inherited;
  oFiler['kind'].DefineEnumerated(FKind, CODES_CONCEPT_KIND);
  oFiler['code'].DefineString(Fcode);
  oFiler['codeUC'].DefineString(FcodeUC);
  oFiler['printSymbol'].DefineString(FprintSymbol);
  oFiler['names'].DefineObject(FNames);
  oFiler['Text'].DefineString(FText);
end;

destructor TUcumConcept.Destroy;
begin
  FNames.Free;
  inherited;
end;

function TUcumConcept.GetKind: TConceptKind;
begin
  result := UcumNull;
end;

function TUcumConcept.Link: TUcumConcept;
begin
  result := TUcumConcept(Inherited Link);
end;


{ TUcumPrefix }

procedure TUcumPrefix.Define(oFiler: TAdvFiler);
begin
  inherited;
  oFiler['value'].DefineObject(FValue);
end;

function TUcumPrefix.GetKind: TConceptKind;
begin
  result := UcumPREFIX;
end;

function TUcumPrefix.Link: TUcumPrefix;
begin
  result := TUcumPrefix(Inherited Link);
end;


procedure TUcumPrefix.SetPrecision(i: integer);
begin
  FValue.Precision := i;
end;

{ TUcumUnit }

procedure TUcumUnit.Define(oFiler: TAdvFiler);
begin
  inherited;
  oFiler['Property'].DefineInteger(FProperty);
end;

function TUcumUnit.Link: TUcumUnit;
begin
  result := TUcumUnit(Inherited Link);
end;

{ TUcumBaseUnit }

procedure TUcumBaseUnit.Define(oFiler: TAdvFiler);
begin
  inherited;
  oFiler['dim'].DefineChar(Fdim);

end;

function TUcumBaseUnit.GetKind: TConceptKind;
begin
  result := UcumBASEUNIT;
end;

function TUcumBaseUnit.Link: TUcumBaseUnit;
begin
  result := TUcumBaseUnit(Inherited Link);

end;

{ TUcumValue }

procedure TUcumValue.Define(oFiler: TAdvFiler);
begin
  inherited;
  oFiler['unit'].DefineString(Funit);
  oFiler['unitUC'].DefineString(FunitUC);
  oFiler['text'].DefineString(Ftext);
  oFiler['value'].DefineObject(Fvalue);
end;


function TUcumValue.Link: TUcumValue;
begin
  result := TUcumValue(Inherited Link);
end;


procedure TUcumValue.SetPrecision(i: integer);
begin
  FValue.Precision  := i;
end;

{ TUcumDefinedUnit }

constructor TUcumDefinedUnit.Create;
begin
  inherited;
  Fvalue := TUcumValue.Create;
end;

procedure TUcumDefinedUnit.Define(oFiler: TAdvFiler);
begin
  inherited;
  oFiler['metric'].Defineboolean(Fmetric);
  oFiler['isSpecial'].Defineboolean(FisSpecial);
  oFiler['class'].DefineString(Fclass_);
  oFiler['value'].DefineObject(Fvalue);

end;

destructor TUcumDefinedUnit.Destroy;
begin
  Fvalue.Free;
  inherited;
end;

function TUcumDefinedUnit.GetKind: TConceptKind;
begin
  result := UcumUNIT;
end;

function TUcumDefinedUnit.Link: TUcumDefinedUnit;
begin
  result := TUcumDefinedUnit(Inherited Link);
end;

{ TUcumPrefixList }

function TUcumPrefixList.GetUcumItem(iIndex: integer): TUcumPrefix;
begin
  result := TUcumPrefix(inherited ObjectByIndex[iIndex]);
end;

function TUcumPrefixList.ItemClass: TAdvObjectClass;
begin
  result := TUcumPrefix;
end;

{ TUcumBaseUnitList }

function TUcumBaseUnitList.ExistsByCode(const sCode: String): Boolean;
begin
  result := GetByCode(sCode) <> nil;
end;

function TUcumBaseUnitList.GetByCode(const sCode: String): TUcumBaseUnit;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count -1 do
    if UcumItem[i].code = sCode then
    begin
      result := UcumItem[i];
      exit;
    End;
end;

function TUcumBaseUnitList.GetUcumItem(iIndex: integer): TUcumBaseUnit;
begin
  result := TUcumBaseUnit(inherited ObjectByIndex[iIndex]);
end;

function TUcumBaseUnitList.ItemClass: TAdvObjectClass;
begin
  result := TUcumBaseUnit;
end;

{ TUcumDefinedUnitList }

function TUcumDefinedUnitList.ExistsByCode(const sCode: String): Boolean;
begin
  result := GetByCode(sCode) <> nil;
end;

function TUcumDefinedUnitList.GetByCode(const sCode: String): TUcumDefinedUnit;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count -1 do
    if UcumItem[i].code = sCode then
    begin
      result := UcumItem[i];
      exit;
    End;
end;

function TUcumDefinedUnitList.GetUcumItem(iIndex: integer): TUcumDefinedUnit;
begin
  result := TUcumDefinedUnit(inherited ObjectByIndex[iIndex]);
end;

function TUcumDefinedUnitList.ItemClass: TAdvObjectClass;
begin
  result := TUcumDefinedUnit;
end;

{ TUcumModel }

procedure TUcumModel.clear;
begin
  Fprefixes.Clear;
  FbaseUnits.Clear;
  FdefinedUnits.Clear;
  FVersion := '';
  FRevisionDate := '';

end;

constructor TUcumModel.Create;
begin
  inherited;
  Fprefixes := TUcumPrefixList.Create;
  FbaseUnits := TUcumBaseUnitList.Create;
  FdefinedUnits := TUcumDefinedUnitList.Create;
  FProperties := TUcumPropertyList.Create;
end;

procedure TUcumModel.Define(oFiler: TAdvFiler);
var
  i : integer;
begin
  inherited;
  i := Ucum_CACHE_VERSION;
  oFiler['streamVersion'].DefineInteger(i);
  if i <> Ucum_CACHE_VERSION Then
    raise exception.create('the UCUM cache must be rebuilt using the ''Import UCUM'' operation in the manager application.');

  oFiler['Version'].DefineString(FVersion);
  oFiler['RevisionDate'].DefineString(FRevisionDate);
  oFiler['prefixes'].DefineObject(Fprefixes);
  oFiler['baseUnits'].DefineObject(FbaseUnits);
  oFiler['definedUnits'].DefineObject(FdefinedUnits);
  oFiler['properties'].DefineObject(FProperties);
end;

destructor TUcumModel.Destroy;
begin
  Fprefixes.Free;
  FbaseUnits.Free;
  FdefinedUnits.Free;
  FProperties.Free;
  inherited;
end;

function TUcumModel.GetUnit(sCode: String): TUcumUnit;
begin
  result := FbaseUnits.GetByCode(sCode);
  if result = nil Then
    result := FdefinedUnits.GetByCode(sCode);
end;

function TUcumModel.Link: TUcumModel;
begin
  result := TUcumModel(inherited Link);
end;

{ TUcumConceptList }

function TUcumConceptList.GetUcumItem(iIndex: integer): TUcumConcept;
begin
  result := TUcumConcept(inherited ObjectByIndex[iIndex]);
end;

function TUcumConceptList.ItemClass: TAdvObjectClass;
begin
  result := TUcumConcept;
end;

{ TUcumPropertyList }

function TUcumPropertyList.GetUcumProperty(iIndex: integer): TUcumProperty;
begin
  result := TUcumProperty(Inherited ObjectByIndex[iIndex]);
end;

function TUcumPropertyList.ItemClass: TAdvObjectClass;
begin
  result := TUcumProperty;
end;

{ TUcumProperty }

constructor TUcumProperty.Create;
begin
  inherited;
  FCommonUnits := TAdvStringList.Create;
end;

procedure TUcumProperty.Define(oFiler: TAdvFiler);
begin
  inherited;
  oFiler['CommonUnits'].DefineObject(FCommonUnits);
end;

destructor TUcumProperty.Destroy;
begin
  FCommonUnits.Free;
  inherited;
end;

function TUcumProperty.Link: TUcumProperty;
begin
  result := TUcumProperty(Inherited Link);
end;

End.
