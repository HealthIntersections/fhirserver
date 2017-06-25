
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
  SysUtils, Classes,
  MathSupport, FileSupport,
  AdvFiles, AdvPersistents, AdvPersistentLists, AdvStringLists, AdvObjectLists, AdvObjects,
  DecimalSupport, ParserSupport, MXML,
  UcumHandlers, UcumValidators, UcumExpressions, Ucum,
  FHIRResources, FHIRTypes, FHIRUtilities, FHIRParser, CDSHooksUtilities,
  TerminologyServices;

Type
  EUCUMServices = class (Exception);

  TUcumPair = class (TAdvObject)
  private
    FUnitCode: String;
    FValue: TSmartDecimal;
  Public
    Constructor Create(oValue : TSmartDecimal; sUnitCode : String); Overload;

    Property Value : TSmartDecimal read FValue write FValue;
    Property UnitCode : String read FUnitCode write FUnitCode;
  End;

{ TUCUMCodeHolder }

Type
  TUCUMContext  = class (TCodeSystemProviderContext)
  private
    FConcept: TFhirValueSetComposeIncludeConcept;
    procedure SetConcept(const Value: TFhirValueSetComposeIncludeConcept);
  public
    Constructor Create(concept : TFhirValueSetComposeIncludeConcept); overload;
    Constructor Create(code : String); overload;
    Destructor Destroy; override;
    property concept : TFhirValueSetComposeIncludeConcept read FConcept write SetConcept;
  end;

  TUcumFilterContext = class (TCodeSystemProviderFilterContext)
  private
    FCursor : integer; // used on the first
    FCanonical: String;
  public
    Constructor Create(canonical : String);
    property canonical : String read FCanonical write FCanonical;
  end;

  TUcumServices = class (TCodeSystemProvider)
  Private
    FModel : TUcumModel;
    FHandlers : TUcumRegistry;
    FLoaded: Boolean;
    FKey: Integer;
    FName: String;
    FPath: String;
    FCommonUnits : TFhirValueSet;

    Function ParseDecimal(S,s1 : String):TSmartDecimal;
    Function ParsePrefix(oElem : TMXmlElement):TUcumPrefix;
    Function ParseBaseUnit(oElem : TMXmlElement):TUcumBaseUnit;
    Function ParseUnit(oElem : TMXmlElement):TUcumDefinedUnit;
    Function GetPropertyIndex(const sName : String):Integer;

  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function Link : TUcumServices; Overload;

    Procedure SetCommonUnits(vs : TFHIRValueSet);

    Property Model : TUcumModel read FModel;
    Property Key : Integer read FKey write FKey;
    Property Title : String read FName write FName;
    Property Path : String read FPath write FPath;

    Function UcumVersion : String;

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
    Function search(kind : TConceptKind; text : String; isRegex : Boolean) : TUcumConceptList; overload;

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

    // load from ucum-essence.xml
    Procedure Import(Const sFilename : String);

    Property Loaded : Boolean read FLoaded write FLoaded;

    function TotalCount : integer; override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system(context : TCodeSystemProviderContext) : String; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; lang : String):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; lang : String) : string; override;
    procedure Displays(code : String; list : TStringList; lang : String); override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList; lang : String); override;
    function filter(prop : String; op : TFhirFilterOperatorEnum; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function specialFilter(prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; overload; override;
    function getDefinition(code : String):String; override;
    function Definition(context : TCodeSystemProviderContext) : string; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    function SpecialEnumeration : String; override;
    procedure getCDSInfo(card : TCDSHookCard; slang, baseURL, code, display : String); override;
    //function subsumes(codeA, codeB : String) : String; override;
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
  if sourceUnit = '' Then
    RaiseError('Convert', 'Source units are required');
  if destUnit = '' Then
    RaiseError('Convert', 'destination units are required');
  if (sourceUnit = destUnit) Then
    result := value
  else
  Begin
    term := nil;
    src := nil;
    dst := nil;
    oConv := TUcumConverter.Create(FModel.Link, FHandlers.Link);
    try
      Term := TUcumExpressionParser.Parse(FModel, sourceUnit);
      src := oConv.convert(term);
      term.Free;
      term := TUcumExpressionParser.Parse(FModel, destUnit);
      dst := oConv.convert(term);
      s := TUcumExpressionComposer.compose(src.Unit_);
      d := TUcumExpressionComposer.compose(dst.Unit_);
      if s <> d then
        raise EUCUMServices.Create('Unable to convert between units '+sourceUnit+' and '+destUnit+' as they do not have matching canonical forms ('+s+' and '+d+' respectively)');
      t := value.Multiply(src.Value);
      result := t.Divide(dst.Value);
    Finally
      term.Free;
      src.Free;
      dst.Free;
      oConv.Free;
    End;
  End;
end;

constructor TUcumServices.Create;
begin
  inherited;
  FModel := TUcumModel.Create;
  FHandlers := TUcumRegistry.Create;
  FHandlers.Register;
end;

function TUcumServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

destructor TUcumServices.Destroy;
begin
  FCommonUnits.Free;
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
    RaiseError('Convert', 'A value is required');
  if value.UnitCode = '' then
    RaiseError('Convert', 'A value unit is required');
  t := TUcumExpressionParser.Parse(FModel, value.UnitCode);
  Try
    conv := TUcumConverter.Create(FModel.Link, FHandlers.Link);
    Try
      c := conv.convert(t);
      Try
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
    RaiseError('Convert', 'A unit is required');
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

procedure TUcumServices.getCDSInfo(card: TCDSHookCard; slang, baseURL, code, display: String);
var
  s : String;
  b : TStringBuilder;
begin
  b := TStringBuilder.Create;
  try
    b.Append('* Analysis: '+analyse(code)+#13#10);
    b.Append('* Canonical Form: '+getCanonicalUnits(code)+#13#10);
    s := validate(code);
    if s <> '' then
      b.Append('* Error: '+s+#13#10);
    b.Append(#13#10+'Copyright: UCUM is Copyright &copy; 1999-2013 Regenstrief Institute, Inc. and The UCUM Organization, Indianapolis, IN. See [Terms Of Use](http://unitsofmeasure.org/trac//wiki/TermsOfUse)'+#13#10);
    card.detail := b.ToString;
  finally
    b.Free;
  end;
end;

function TUcumServices.getDefinedForms(code: String): TUcumDefinedUnitList;
var
  base : TUcumBaseUnit;
  i : integer;
begin
  if Code = '' then
    RaiseError('Convert', 'A unit is required');
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

function TUcumServices.getDefinition(code: String): String;
begin
  result := '';
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

function TUcumServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
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

function TUcumServices.multiply(o1, o2: TUcumPair): TUcumPair;
var
  res : TUcumPair;
begin
  res := TUcumPair.Create(TSmartDecimal.One, '');
  Try
    res.value := o1.value.Multiply(o2.Value);
    res.FUnitCode := o1.FUnitCode +'.'+o2.UnitCode;
    result := getCanonicalForm(res);
  Finally
    res.Free;
  End;
end;

function TUcumServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'UCUM';
end;

function TUcumServices.search(kind: TConceptKind; text: String; isRegex: Boolean): TUcumConceptList;
var
  oSearch : TUcumSearch;
begin
  if text = '' Then
    raise EUCUMServices.Create('A text to search for is required');
  oSearch := TUcumSearch.Create;
  Try
    result := oSearch.DoSearch(model, kind, text, isRegex);
  Finally
    oSearch.Free;
  End;
end;

function TUcumServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean): TCodeSystemProviderFilterContext;
begin
  result := nil;
  raise EUCUMServices.Create('to do');
end;

procedure TUcumServices.SetCommonUnits(vs: TFHIRValueSet);
//var
//  i : integer;
//  xml : TFHIRXmlComposer;
//  f : TFileStream;
//  c, d : String;
begin
  FCommonUnits.Free;
  FCommonUnits := vs;

//  for i := 0 to vs.compose.includeList[0].codeList.count - 1 do
//    if not vs.compose.includeList[0].codeList[i].HasExtension('http://hl7.org/fhir/Profile/tools-extensions#display') then
//    begin
//      c := vs.compose.includeList[0].codeList[i].value;
//      d := analyse(c);
//      writeln(c+' -> '+d);
//      vs.compose.includeList[0].codeList[i].setExtensionString('http://hl7.org/fhir/Profile/tools-extensions#display', d);
//    end;
//
//  f := TFileStream.Create('C:\work\org.hl7.fhir\build\source\valueset\valueset-ucum-common.xml', fmCreate);
//  try
//    xml := TFHIRXmlComposer.Create('en');
//    try
//      xml.Compose(f, '', '', '', vs, true, nil);
//    finally
//      xml.Free;
//    end;
//  finally
//    f.free;
//  end;
end;

function TUcumServices.SpecialEnumeration: String;
begin
  if FCommonUnits <> nil then
    result := FCommonUnits.url
  else
    result := '';
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
    RaiseError('Convert', 'A unit is required');
  if canonical = '' then
    RaiseError('Convert', 'A canonical unit is required');

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
    RaiseError('Convert', 'A unit is required');
  if propertyType = '' then
    RaiseError('Convert', 'A property is required');

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

function TUcumServices.Version(context: TCodeSystemProviderContext): String;
begin
  result := UcumVersion;
end;

function TUcumServices.UcumVersion: String;
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
      if SameText(Definition[i].UcumVersion, sName) then
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


function TUcumServices.ChildCount(context: TCodeSystemProviderContext): integer;
begin
  result := 0;
end;

procedure TUcumServices.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.Free;
end;

function TUcumServices.Code(context: TCodeSystemProviderContext): string;
begin
  if context = nil then
    result := ''
  else
    result := TUCUMContext(context).concept.code;
end;

function TUcumServices.getcontext(context: TCodeSystemProviderContext; ndx: integer): TCodeSystemProviderContext;
begin
  result := nil;
end;

function TUcumServices.Display(context: TCodeSystemProviderContext; lang : String): string;
begin
  if context = nil then
    result := ''
  else
    result := getDisplay(TUCUMContext(context).concept.code, lang);
end;

procedure TUcumServices.Displays(context: TCodeSystemProviderContext; list: TStringList; lang : String);
begin
  list.Add(Code(context));
end;

procedure TUcumServices.Displays(code: String; list: TStringList; lang : String);
begin
  list.Add(getDisplay(code, lang));
end;

function TUcumServices.getDisplay(code: String; lang : String): String;
var
  cc : TFhirValueSetComposeIncludeConcept;
begin
  result := analyse(code);
  if FCommonUnits <> nil then
    for cc in FCommonUnits.compose.includeList[0].conceptList do
        if (cc.code = code) and (cc.display <> '') then
          result := cc.display;
end;

procedure TUcumServices.Import(const sFilename: String);
var
  oXml : TMXmlDocument;
  oElem : TMXmlElement;
  oErrors : TAdvStringList;
begin
  oXml := TMXmlParser.parseFile(sFilename, [xpDropWhitespace]);
  try
    if oXml.document.Name <> 'root' Then
      raise EUCUMServices.create('Invalid ucum essence file');
    FModel.Clear;
    FModel.Version := oXml.document.attribute['version'];
    FModel.RevisionDate := oXml.document.Attribute['revision-date'];
    FModel.RevisionDate := copy(FModel.RevisionDate, 8, length(FModel.RevisionDate)-9);
    oElem := oXml.document.firstElement;
    while (oElem <> nil) Do
    Begin
     if oElem.Name = 'prefix' Then
       FModel.prefixes.Add(ParsePrefix(oElem))
     Else if oElem.Name = 'base-unit' Then
       FModel.baseUnits.Add(ParseBaseUnit(oElem))
     Else if oElem.Name = 'unit' Then
       FModel.definedUnits.Add(ParseUnit(oElem))
     else
       raise EUCUMServices.create('unrecognised element '+oElem.Name);
      oElem := oElem.nextElement;
    End;
    oErrors := TAdvStringList.Create;
    Try
      Validate(oErrors);
      if oErrors.Count > 0 then
        raise EUCUMServices.create(oErrors.asText);
    Finally
      oErrors.Free;
    End;
  finally
    oXml.Free;
  end;
end;

function TUcumServices.InFilter(ctxt: TCodeSystemProviderFilterContext; concept: TCodeSystemProviderContext): Boolean;
var
  code : String;
  context : TUcumFilterContext;
begin
  context := TUcumFilterContext(ctxt);
  code := TUcumContext(concept).concept.code;
  result := validateCanonicalUnits(code, context.canonical) = '';
end;

function TUcumServices.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function TUcumServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := true;
end;

function TUcumServices.locate(code: String; var message : String): TCodeSystemProviderContext;
var
  s : String;
begin
  s:= validate(code);
  if s = '' then
    result := TUCUMContext.Create(code)
  else
  begin
    result := nil;
    message := s;
  end;
end;

function TUcumServices.specialFilter(prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  result := TUcumFilterContext.create('')
end;

function TUcumServices.system(context : TCodeSystemProviderContext): String;
begin
  result := 'http://unitsofmeasure.org';
end;

function TUcumServices.TotalCount: integer;
begin
  // this is not true, but this is not too big to expand (the primary purpose of this function)
  result := 0;
end;

procedure TUcumServices.Close(ctxt: TCodeSystemProviderFilterContext);
begin
  ctxt.Free;
end;

function TUcumServices.filter(prop: String; op: TFhirFilterOperatorEnum; value: String; prep : TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
begin
  if (prop = 'canonical') and (op in [FilterOperatorEqual]) then
    result := TUcumFilterContext.create(value)
  else
    result := nil;
end;

function TUcumServices.FilterConcept(ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
var
  context : TUcumFilterContext;
begin
  context := TUcumFilterContext(ctxt);
  result := TUCUMContext.create(FCommonUnits.compose.includeList[0].conceptList[context.FCursor].link);
end;

function TUcumServices.FilterMore(ctxt: TCodeSystemProviderFilterContext): boolean;
var
  context : TUcumFilterContext;
begin
  context := TUcumFilterContext(ctxt);
  inc(context.FCursor);
  result := context.FCursor < FCommonUnits.compose.includeList[0].conceptList.count;
end;

function TUcumServices.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String; var message : String): TCodeSystemProviderContext;
begin
  result := nil;
end;

function TUcumServices.locateIsA(code, parent: String): TCodeSystemProviderContext;
begin
  result := nil;
end;

function TUcumServices.ParseDecimal(s, s1: String): TSmartDecimal;
begin
  if s = '' then
    result := TSmartDecimal.One
  Else
    result := TSmartDecimal.ValueOf(s);
end;

Function TUcumServices.ParsePrefix(oElem : TMXmlElement):TUcumPrefix;
var
  oChild : TMXmlElement;
  s : String;
Begin
  result := TUcumPrefix.Create;
  try
    result.code := oElem.attribute['Code'];
    result.codeUC := oElem.attribute['CODE'];
    oChild := oElem.firstElement;
    while oChild <> nil do
    Begin
      if oChild.Name = 'name' Then
        result.names.Add(oChild.allText)
      else if oChild.Name = 'printSymbol' Then
        result.printSymbol := oChild.allText
      else if oChild.Name = 'value' Then
      begin
        s := oChild.attribute['value'];
        result.value := ParseDecimal(s, result.Code);
        result.SetPrecision(24); // arbitrarily high. even when an integer, these numbers are precise
        if s[2] = 'e' Then
          result.Text := '10^'+Copy(s, 3, $FF)
        else
          result.Text := s;
      End
      else
        raise EUCUMServices.Create('unknown element in prefix: '+oChild.Name);
      oChild := oChild.nextElement;
    End;
    result.Link;
  Finally
    result.Free;
  End;
End;

Function TUcumServices.ParseBaseUnit(oElem : TMXmlElement):TUcumBaseUnit;
var
  oChild : TMXmlElement;
  s : String;
Begin
  result := TUcumBaseUnit.Create;
  try
    result.code := oElem.attribute['Code'];
    result.codeUC := oElem.attribute['CODE'];
    s := oElem.attribute['dim'];
    if s <> '' Then
      result.dim := s[1];
    oChild := oElem.firstElement;
    while oChild <> nil do
    Begin
      if oChild.Name = 'name' Then
        result.names.Add(oChild.allText)
      else if oChild.Name = 'printSymbol' Then
        result.printSymbol := oChild.allText
      else if oChild.Name = 'property' Then
        result.PropertyType := GetPropertyIndex(oChild.allText)
      else
        raise EUCUMServices.Create('unknown element in base unit: '+oChild.Name);
      oChild := oChild.nextElement;
    End;
    result.Link;
  Finally
    result.Free;
  End;
End;

Function TUcumServices.ParseUnit(oElem : TMXmlElement):TUcumDefinedUnit;
var
  oChild : TMXmlElement;
Begin
  result := TUcumDefinedUnit.Create;
  try
    result.code := oElem.attribute['Code'];
    result.codeUC := oElem.attribute['CODE'];
    result.metric := oElem.attribute['isMetric'] = 'yes';
    result.isSpecial := oElem.attribute['isSpecial'] = 'yes';
    result.class_ := oElem.attribute['class'];
    oChild := oElem.firstElement;
    while oChild <> nil do
    Begin
      if oChild.Name = 'name' Then
        result.names.Add(oChild.allText)
      else if oChild.Name = 'printSymbol' Then
        result.printSymbol := oChild.allText
      else if oChild.Name = 'property' Then
        result.PropertyType := GetPropertyIndex(oChild.allText)
      else if oChild.Name = 'value' Then
      begin
        result.value.unit_ := oChild.attribute['Unit'];
        result.value.unitUC := oChild.attribute['UNIT'];
        result.value.value := ParseDecimal(oChild.attribute['value'], result.value.unit_);
        if result.value.value.IsWholeNumber then
          result.value.SetPrecision(24);
        result.value.text := oChild.allText
      End
      else
        raise EUCUMServices.Create('unknown element in unit: '+oChild.Name);
      oChild := oChild.nextElement;
    End;
    result.Link;
  Finally
    result.Free;
  End;
End;


function TUcumServices.GetPropertyIndex(const sName: String): Integer;
begin
  result :=  FModel.Properties.IndexByName(sName);
  if result = -1 then
    result := FModel.Properties.AddByName(sName);
end;




{ TUcumFilterContext }

constructor TUcumFilterContext.Create(canonical: String);
begin
  inherited Create;
  FCursor := -1;
  FCanonical := canonical;
end;

{ TUCUMContext }

constructor TUCUMContext.Create(concept: TFhirValueSetComposeIncludeConcept);
begin
  inherited Create;
  FConcept := concept;
end;

constructor TUCUMContext.Create(code: String);
begin
  inherited Create;
  FConcept := TFhirValueSetComposeIncludeConcept.Create;
  FConcept.code := code;
end;

destructor TUCUMContext.Destroy;
begin
  FConcept.Free;
  inherited;
end;

procedure TUCUMContext.SetConcept(const Value: TFhirValueSetComposeIncludeConcept);
begin
  FConcept.Free;
  FConcept := Value;
end;

End.


