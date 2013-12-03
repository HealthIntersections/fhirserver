unit UcumServices;

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
  SysUtils,
  Classes,
  AdvBinaryFilers,
  MathSupport,
  FileSupport,
  AdvFiles,
  AdvFactories,
  DecimalSupport,
  AdvPersistents,
  AdvPersistentLists,
  AdvStringLists,
  AdvObjectLists,
  AdvObjects,
  UcumHandlers,
  UcumValidators,
  UcumExpressions,
  Ucum;

Type
  TUcumPair = class (TAdvObject)
  private
    FUnitCode: String;
    FValue: TSmartDecimal;
    procedure SetValue(const Value: TSmartDecimal);
  Public
    Constructor Create(oValue : TSmartDecimal; sUnitCode : String); Overload;
    Destructor Destroy; Override;

    Property Value : TSmartDecimal read FValue write SetValue;
    Property UnitCode : String read FUnitCode write FUnitCode;
  End;

  TUcumServices = class (TadvObject)
  Private
    FModel : TUcumModel;
    FHandlers : TUcumRegistry;
    FLoaded: Boolean;
    FKey: Integer;
    FName: String;
    FPath: String;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function Link : TUcumServices; Overload;

    Property Model : TUcumModel read FModel;
    Property Key : Integer read FKey write FKey;
    Property Name : String read FName write FName;
    Property Path : String read FPath write FPath;

    Function Version : String;

    Procedure Validate(oErrors : TAdvStringList); Overload;

    (**
     * Search through the UCUM concepts for any concept containing matching text.
     * Search will be limited to the kind of concept defined by kind, or all if kind
     * is null
     *
     * @param kind - can be null. scope of search
     * @param text - required
     * @param isRegex
     * @return
     *)
    Function search(kind : TConceptKind; text : String; isRegex : Boolean) : TUcumConceptList;

    (*
     * return a list of the defined types of units in this UCUM version
     *
     * @return
     *)
    Procedure getProperties(oList : TAdvStringList);

    (**
     * for a given property, return the commonly used units
     *
     * @param code
     * @return
     * @throws OHFException
     *)
    Procedure getCommonUnits(propertyName : String; oList : TAdvStringList);

    (**
     * validate whether a unit code are valid UCUM units
     *
     * @param units - the unit code to check
     * @return nil if valid, or an error message describing the problem
     *)
    Function validate(code : String) : String; Overload;

    (**
     * given a unit, return a formal description of what the units stand for using
     * full names
     * @param units the unit code
     * @return formal description
     * @throws OHFException
     *)
    Function analyse(code : String) : String ;

    (**
     * validate whether a units are valid UCUM units and additionally require that the
     * units from a particular property
     *
     * @param units - the unit code to check
     * @return nil if valid, or an error message describing the problem
     *)
    Function validateInProperty(code, propertyType : String) : String ;

    (**
     * validate whether a units are valid UCUM units and additionally require that the
     * units match a particular base canonical unit
     *
     * @param units - the unit code to check
     * @return nil if valid, or an error message describing the problem
     *)
    Function validateCanonicalUnits(code, canonical : String) : String;

    (**
     * given a set of units, return their canonical form
     * @param unit
     * @return the canonical form
     * @throws OHFException
     *)
    Function getCanonicalUnits(code : String) : String ;

    (**
     * for a given canonical unit, return all the defined units that have the
     * same canonical unit.
     *
     * @param code
     * @return
     * @throws OHFException
     *)
    Function getDefinedForms(code : String) : TUcumDefinedUnitList;

    (**
     * given a value/unit pair, return the canonical form as a value/unit pair
     *
     * 1 mm -> 1e-3 m
     * @param value
     * @return
     * @throws OHFException
     *)
    function getCanonicalForm(value : TUcumPair) : TUcumPair;

    (**
     * given a value and source unit, return the value in the given dest unit
     * an exception is thrown if the conversion is not possible
     *
     * @param value
     * @param sourceUnit
     * @param destUnit
     * @return the value if a conversion is possible
     * @throws OHFException
     *)
    Function convert(value : TSmartDecimal;  sourceUnit, destUnit : String) : TSmartDecimal;

    (**
     * multiply two value/units pairs together and return the result in canonical units
     *
     * Note: since the units returned are canonical,
     * @param o1
     * @param o2
     * @return
     *)
    Function multiply(o1, o2 : TUcumPair) : TUcumPair;

    Procedure Load(Const sFilename : String);
    Procedure Save(Const sFilename : String);

    Property Loaded : Boolean read FLoaded write FLoaded;
  End;

  TUcumServiceList = class (TAdvObjectList)
  Private
    FDefinition: TUcumServices;
    function GetDefinition(iIndex: Integer): TUcumServices;
    procedure SetDefinition(const Value: TUcumServices);
  Protected
    Function ItemClass : TAdvObjectClass; Override;
  Public
    Destructor Destroy; Override;

    Function GetByKey(sKey : String) : TUcumServices;
    Function GetDefinitionByName(sName : String) : TUcumServices;

    Property DefaultDefinition : TUcumServices Read FDefinition write SetDefinition;
    Property Definition[iIndex : Integer] : TUcumServices read GetDefinition; Default;
  End;

var
  GUcums : TUcumServiceList;

Implementation

Uses
 UcumSearch;
 
{ TUcumServices }

function TUcumServices.analyse(code: String): String;
var
  oTerm : TUcumTerm;
begin
  if code = '' Then
    result := '(unity)'
  else
  Begin
    oTerm := TUcumExpressionParser.Parse(Fmodel, code);
    Try
      result := TUcumFormalStructureComposer.Compose(oTerm);
    Finally
      oTerm.Free;
    End;
  End;
end;

function TUcumServices.convert(value: TSmartDecimal; sourceUnit, destUnit: String): TSmartDecimal;
var
  oConv : TUcumConverter;
  src : TUcumCanonical;
  dst : TUcumCanonical;
  term : TUcumTerm;
  s, d : String;
  t : TSmartDecimal;
begin
  if value = nil then
    Error('Convert', 'A value is required');
  if sourceUnit = '' Then
    Error('Convert', 'Source units are required');
  if destUnit = '' Then
    Error('Convert', 'destination units are required');
  if (sourceUnit = destUnit) Then
    result := value.Link
  else
  Begin
    term := nil;
    src := nil;
    dst := nil;
    oConv := TUcumConverter.Create(FModel.Link, FHandlers.Link);
    try
      result := oConv.Context.Value(Value);
      Term := TUcumExpressionParser.Parse(FModel, sourceUnit);
      src := oConv.convert(term);
      term.Free;
      term := TUcumExpressionParser.Parse(FModel, destUnit);
      dst := oConv.convert(term);
      s := TUcumExpressionComposer.compose(src.Unit_);
      d := TUcumExpressionComposer.compose(dst.Unit_);
      if s <> d then
        raise Exception.Create('Unable to convert between units '+sourceUnit+' and '+destUnit+' as they do not have matching canonical forms ('+s+' and '+d+' respectively)');
      t := result.Multiply(src.Value);
      result := t.Divide(dst.Value);
    Finally
      term.Free;
      src.Free;
      dst.Free;
      oConv.Free;
    End;
  End;
  result.Link;
end;

constructor TUcumServices.Create;
begin
  inherited;
  FModel := TUcumModel.Create;
  FHandlers := TUcumRegistry.Create;
  FHandlers.Register;
end;

destructor TUcumServices.Destroy;
begin
  FHandlers.Free;
  FModel.Free;
  inherited;
end;

function TUcumServices.getCanonicalForm(value: TUcumPair): TUcumPair;
var
  t : TUcumTerm;
  conv : TUcumConverter;
  c : TUcumCanonical;
begin
  if value = nil then
    Error('Convert', 'A value is required');
  if value.UnitCode = '' then
    Error('Convert', 'A value unit is required');
  t := TUcumExpressionParser.Parse(FModel, value.UnitCode);
  Try
    conv := TUcumConverter.Create(FModel.Link, FHandlers.Link);
    Try
      c := conv.convert(t);
      Try
        if value.Value = nil then
          result := TUcumPair.Create(nil, TUcumExpressionComposer.Compose(c.Unit_))
        else
          result := TUcumPair.Create(value.Value.Multiply(c.Value), TUcumExpressionComposer.Compose(c.Unit_))
      Finally
        c.Free;
      End;
    Finally
      conv.free;
    End;
  Finally
    t.Free;
  End;
end;

function TUcumServices.getCanonicalUnits(code: String): String;
var
  t : TUcumTerm;
  conv : TUcumConverter;
  c : TUcumCanonical;
begin
  if Code = '' then
    Error('Convert', 'A unit is required');
  t := TUcumExpressionParser.Parse(FModel, Code);
  Try
    conv := TUcumConverter.Create(FModel.Link, FHandlers.Link);
    Try
      c := conv.convert(t);
      Try
        result := TUcumExpressionComposer.Compose(c.Unit_);
      Finally
        c.Free;
      End;
    Finally
      conv.free;
    End;
  Finally
    t.Free;
  End;
end;

function TUcumServices.getDefinedForms(code: String): TUcumDefinedUnitList;
var
  base : TUcumBaseUnit;
  i : integer;
begin
  if Code = '' then
    Error('Convert', 'A unit is required');
  result := TUcumDefinedUnitList.Create;
  Try
    base := FModel.baseUnits.GetByCode(code);
    if assigned(base) then
      for i := 0 to FModel.definedUnits.count - 1 do
        if not FModel.definedUnits[i].isSpecial And (getCanonicalUnits(FModel.definedUnits[i].code) = code) Then
          result.Add(FModel.definedUnits[i].Link);
    result.link;
  Finally
    result.free;
  End;
end;

procedure TUcumServices.getCommonUnits(propertyName: String; oList : TAdvStringList);
var
  i : integer;
begin
  oList.Clear;
  oList.SortAscending;
  oList.Sorted;
  i := Model.Properties.IndexByName(propertyName);
  if i > -1 Then
    oList.Assign(Model.Properties[i].CommonUnits);
end;

procedure TUcumServices.getProperties(oList: TAdvStringList);
var
  i : integer;
begin
  oList.Clear;
  oList.SortAscending;
  oList.Sorted;
  for i := 0 to Model.Properties.Count - 1 do
    oList.Add(Model.Properties[i].Name);
end;

procedure TUcumServices.Load(const sFilename: String);
var
  oFile : TAdvFile;
  oFiler : TAdvBinaryReader;
begin
  oFile := TAdvFile.Create;
  Try
    oFile.Name := sFilename;
    oFile.OpenRead;
    oFiler := TAdvBinaryReader.Create;
    Try
      oFiler.Stream := oFile.Link;
      oFiler['Mode'].DefineObject(FModel);
    Finally
      oFiler.Free;
    End;
  Finally
    oFile.Free;
  End;
  Loaded := true;
end;

function TUcumServices.multiply(o1, o2: TUcumPair): TUcumPair;
var
  res : TUcumPair;
begin
  res := TUcumPair.Create(nil, '');
  Try
    res.value := o1.value.Multiply(o2.Value);
    res.FUnitCode := o1.FUnitCode +'.'+o2.UnitCode;
    result := getCanonicalForm(res);
  Finally
    res.Free;
  End;

end;

procedure TUcumServices.Save(const sFilename: String);
var
  oFile : TAdvFile;
  oFiler : TAdvBinaryWriter;
begin
  if FileExists(sFilename) Then
  begin
    FileSetReadOnlyAttribute(sFilename, False);
    DeleteFile(sFilename);
  End;
  oFile := TAdvFile.Create;
  Try
    oFile.Name := sFilename;
    oFile.OpenCreate;
    oFiler := TAdvBinaryWriter.Create;
    Try
      oFiler.Stream := oFile.Link;
      oFiler['Mode'].DefineObject(FModel);
    Finally
      oFiler.Free;
    End;
  Finally
    oFile.Free;
  End;
end;

function TUcumServices.search(kind: TConceptKind; text: String; isRegex: Boolean): TUcumConceptList;
var
  oSearch : TUcumSearch;
begin
  if text = '' Then
    raise exception.Create('A text to search for is required');
  oSearch := TUcumSearch.Create;
  Try
    result := oSearch.DoSearch(model, kind, text, isRegex);
  Finally
    oSearch.Free;
  End;
end;

function TUcumServices.Validate(code: String): String;
begin
  if (code <> '') Then
  Try
    TUcumExpressionParser.parse(Fmodel, code).Free;
    result := '';
  Except
    on E:Exception do
      result := e.Message;
  End;
end;

procedure TUcumServices.Validate(oErrors: TAdvStringList);
var
  oValidator : TUcumValidator;
begin
  oValidator := TUcumValidator.Create(FModel.Link, FHandlers.Link);
  Try
    oValidator.validate(oErrors);
  Finally
    oValidator.Free;
  End;
end;

function TUcumServices.validateCanonicalUnits(code, canonical: String): String;
var
  t : TUcumTerm;
  conv : TUcumConverter;
  c : TUcumCanonical;
  cu : String;
begin
  if Code = '' then
    Error('Convert', 'A unit is required');
  if canonical = '' then
    Error('Convert', 'A canonical unit is required');

  result := '';
  Try
    t := TUcumExpressionParser.Parse(FModel, Code);
    Try
      conv := TUcumConverter.Create(FModel.Link, FHandlers.Link);
      Try
        c := conv.convert(t);
        Try
          cu := TUcumExpressionComposer.Compose(c.Unit_);
          if cu <> canonical then
            result := 'unit '+code+' has the canonical units '+cu+', not '+canonical+' as required.';
        Finally
          c.Free;
        End;
      Finally
        conv.free;
      End;
    Finally
      t.Free;
    End;
  Except
    on e:exception do
      result := e.message;
  End;
end;

function TUcumServices.validateInProperty(code, propertyType: String): String;
var
  t : TUcumTerm;
  conv : TUcumConverter;
  c : TUcumCanonical;
  cu : String;
  term : TUcumTerm;
  sym : TUcumSymbol;
  b : TUcumBaseUnit;
begin
  if Code = '' then
    Error('Convert', 'A unit is required');
  if propertyType = '' then
    Error('Convert', 'A property is required');

  result := '';
  Try
    t := TUcumExpressionParser.Parse(FModel, Code);
    Try
      conv := TUcumConverter.Create(FModel.Link, FHandlers.Link);
      Try
        c := conv.convert(t);
        Try
          term := c.Unit_;
          cu := TUcumExpressionComposer.Compose(term);
          if (term <> nil) and (term.Component <> nil) and (term.Operator = NOOP) And (term.Component is TUcumSymbol) Then
          Begin
            sym := TUcumSymbol(term.Component);
            if (sym.Exponent = 1) and (sym.Unit_ is TUcumBaseUnit) Then
            begin
              b := TUcumBaseUnit(sym.Unit_);
              if not (propertyType = FModel.Properties[b.PropertyType].Name) then
                result := 'unit '+code+' is of the property type '+FModel.Properties[b.PropertyType].Name+' ('+cu+'), not '+propertyType+' as required.';
              Exit;
            End;
          End;
          if (propertyType = 'concentration') and (cu = 'm-3') then
           // exit
          else
            result := 'unit '+code+' has the base units '+cu+', and is not from the property '+propertyType+' as required.';
        Finally
          c.Free;
        End;
      Finally
        conv.free;
      End;
    Finally
      t.Free;
    End;
  Except
    on e:exception do
      result := e.message;
  End;
end;

function TUcumServices.Version: String;
begin
  result := FModel.Version;
end;

function TUcumServices.Link: TUcumServices;
begin
  result := TUcumServices(Inherited Link);
end;

{ TUcumPair }

constructor TUcumPair.Create(oValue: TSmartDecimal; sUnitCode: String);
begin
  Create;
  Value := oValue;
  UnitCode := sUnitCode;
end;

destructor TUcumPair.Destroy;
begin
  FValue.Free;
  inherited;
end;

procedure TUcumPair.SetValue(const Value: TSmartDecimal);
begin
  FValue.Free;
  FValue := Value;
end;

{ TUcumServiceList }

destructor TUcumServiceList.Destroy;
begin
  FDefinition.Free;
  inherited;
end;

function TUcumServiceList.GetByKey(sKey: String): TUcumServices;
var
  i, k : integer;
begin
  Result := nil;
  i := 0;
  k := StrToIntDef(sKey, 0);
  While (i < Count) and (result = nil) do
  Begin
    if Definition[i].Key = k then
      result := Definition[i];
    inc(i);
  End;
  if result = nil then
    result := DefaultDefinition;
end;

function TUcumServiceList.GetDefinition(iIndex: Integer): TUcumServices;
begin
  result := TUcumServices(ObjectByIndex[iIndex]);
end;

function TUcumServiceList.GetDefinitionByName(sName: String): TUcumServices;
var
  i : integer;
begin
  if sName = '' then
    result := DefaultDefinition
  Else
  Begin
    Result := nil;
    i := 0;
    While (i < Count) and (result = nil) do
    Begin
      if SameText(Definition[i].Version, sName) then
        result := Definition[i];
      inc(i);
    End;
  End;
end;

function TUcumServiceList.ItemClass: TAdvObjectClass;
begin
  result := TUcumServices;
end;

procedure TUcumServiceList.SetDefinition(const Value: TUcumServices);
begin
  FDefinition.Free;
  FDefinition := Value;
end;


End.


