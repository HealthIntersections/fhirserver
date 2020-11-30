unit ftx_ucum_services;

{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_collections, fsl_stream, fsl_xml, fsl_ucum,
  fsl_http,
  ftx_ucum_handlers, ftx_ucum_validators, ftx_ucum_expressions, ftx_ucum_base,
  fhir_common,
  fhir_cdshooks,
  ftx_service;

Type
  TUcumPair = fsl_ucum.TUcumPair;

  TUCUMContext = class (TCodeSystemProviderContext)
  private
    FConcept: TFhirValueSetComposeIncludeConceptW;
    procedure SetConcept(Value: TFhirValueSetComposeIncludeConceptW);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(concept : TFhirValueSetComposeIncludeConceptW); overload;
    constructor Create(code : String); overload;
    destructor Destroy; override;
    property concept : TFhirValueSetComposeIncludeConceptW read FConcept write SetConcept;
  end;

  TUcumFilterContext = class (TCodeSystemProviderFilterContext)
  private
    FCursor : integer; // used on the first
    FCanonical: String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(canonical : String);
    property canonical : String read FCanonical write FCanonical;
  end;

  { TUcumServices }

  TUcumServices = class (TCodeSystemProvider)
  Private
    FModel : TUcumModel;
    FHandlers : TUcumRegistry;
    FLoaded: Boolean;
    FKey: Integer;
    FName: String;
    FPath: String;
    FCommonUnits : TFhirValueSetW;
    FCommonUnitList : TFslList<TFhirValueSetComposeIncludeConceptW>;

    Function ParseDecimal(S,s1 : String):TFslDecimal;
    Function ParsePrefix(oElem : TMXmlElement):TUcumPrefix;
    Function ParseBaseUnit(oElem : TMXmlElement):TUcumBaseUnit;
    Function ParseUnit(oElem : TMXmlElement):TUcumDefinedUnit;

  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TUcumServices; Overload;

    Procedure SetCommonUnits(vs : TFHIRValueSetW);

    Property Model : TUcumModel read FModel;
    Property Key : Integer read FKey write FKey;
    Property Title : String read FName write FName;
    Property Path : String read FPath write FPath;

    Function UcumVersion : String;

    Procedure Validate(oErrors : TFslStringList); Overload;

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
    Function search(kind : TConceptKind; text : String; isRegex : Boolean) : TFslList<TUcumConcept>; overload;

    (*
     * return a list of the defined types of units in this UCUM version
     *
     * @return
     *)
    Procedure getProperties(oList : TFslStringList);

    (**
     * for a given property, return the commonly used units
     *
     * @param code
     * @return
     * @throws OHFException
     *)
    Procedure getCommonUnits(propertyName : String; oList : TFslStringList);

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
    Function getDefinedForms(code : String) : TFslMap<TUcumDefinedUnit>;

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
    Function convert(value : TFslDecimal;  sourceUnit, destUnit : String) : TFslDecimal;

    (**
     * multiply two value/units pairs together and return the result in canonical units
     *
     * Note: the units returned are canonical
     * @param o1
     * @param o2
     * @return
     *)
    Function multiply(o1, o2 : TUcumPair) : TUcumPair;

    (**
     * divide two value/units pairs together and return the result in canonical units
     *
     * Note: the units returned are canonical
     * @param o1
     * @param o2
     * @return
     *)
    Function divideBy(o1, o2 : TUcumPair) : TUcumPair;


    // load from ucum-essence.xml
    Procedure Import(sFilename : String);
    class function checkFile(sFilename : String) : String;

    Property Loaded : Boolean read FLoaded write FLoaded;

    function description : String; override;
    function TotalCount : integer; override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function systemUri(context : TCodeSystemProviderContext) : String; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; const lang : THTTPLanguages):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string; override;
    procedure Displays(code : String; list : TStringList; const lang : THTTPLanguages); override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList; const lang : THTTPLanguages); override;
    function filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function specialFilter(prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; overload; override;
    function getDefinition(code : String):String; override;
    function Definition(context : TCodeSystemProviderContext) : string; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    function SpecialEnumeration : String; override;
    procedure getCDSInfo(card : TCDSHookCard; const slang : THTTPLanguages; baseURL, code, display : String); override;
    //function subsumes(codeA, codeB : String) : String; override;
  End;

  TUcumServiceList = class (TFslObjectList)
  Private
    FDefinition: TUcumServices;
    function GetDefinition(iIndex: Integer): TUcumServices;
    procedure SetDefinition(Value: TUcumServices);
  Protected
    Function ItemClass : TFslObjectClass; Override;
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;

    Function GetByKey(sKey : String) : TUcumServices;
    Function GetDefinitionByName(sName : String) : TUcumServices;

    Property DefaultDefinition : TUcumServices Read FDefinition write SetDefinition;
    Property Definition[iIndex : Integer] : TUcumServices read GetDefinition; Default;
  End;

  TUcumServiceImplementation = class (TUcumServiceInterface)
  private
    FSvc : TUcumServices;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(svc : TUcumServices);
    destructor Destroy; override;
    Function multiply(o1, o2 : TUcumPair) : TUcumPair; override;
    Function divideBy(o1, o2 : TUcumPair) : TUcumPair; override;
    function getCanonicalForm(value : TUcumPair) : TUcumPair; override;
    Function isConfigured : boolean; override;
  end;

Implementation

Uses
  ftx_ucum_search, fhir_objects;

type
  TFhirValueSetComposeIncludeConceptLocal = class (TFhirValueSetComposeIncludeConceptW)
  private
    FCode : String;
  protected
    function getCode : String; override;
    function getDisplay : String; override;
    procedure setCode(Value: String); override;
    procedure setDisplay(Value: String); override;
    function designations : TFslList<TFhirValueSetComposeIncludeConceptDesignationW>; override;
    function sizeInBytesV : cardinal; override;
  end;

{ TFhirValueSetComposeIncludeConceptLocal }

function TFhirValueSetComposeIncludeConceptLocal.designations: TFslList<TFhirValueSetComposeIncludeConceptDesignationW>;
begin
  result := nil;
end;

function TFhirValueSetComposeIncludeConceptLocal.getCode: String;
begin
  result := FCode;
end;

function TFhirValueSetComposeIncludeConceptLocal.getDisplay: String;
begin
  result := '';
end;

procedure TFhirValueSetComposeIncludeConceptLocal.SetCode(Value: String);
begin
  FCode := value;
end;

procedure TFhirValueSetComposeIncludeConceptLocal.SetDisplay(Value: String);
begin
end;

function TFhirValueSetComposeIncludeConceptLocal.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeof(char)) + 12);
end;

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

function TUcumServices.convert(value: TFslDecimal; sourceUnit, destUnit: String): TFslDecimal;
var
  oConv : TUcumConverter;
  src : TUcumCanonical;
  dst : TUcumCanonical;
  term : TUcumTerm;
  s, d : String;
  t : TFslDecimal;
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
      s := TUcumExpressionComposer.compose(src, false);
      d := TUcumExpressionComposer.compose(dst, false);
      if s <> d then
        raise ETerminologyError.Create('Unable to convert between units '+sourceUnit+' and '+destUnit+' as they do not have matching canonical forms ('+s+' and '+d+' respectively)');
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

function TUcumServices.description: String;
begin
  result := 'UCUM';
end;

destructor TUcumServices.Destroy;
begin
  FCommonUnits.Free;
  FCommonUnitList.Free;
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
        result := TUcumPair.Create(value.Value.Multiply(c.Value), TUcumExpressionComposer.Compose(c, false))
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
        result := TUcumExpressionComposer.Compose(c, false);
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

procedure TUcumServices.getCDSInfo(card: TCDSHookCard; const slang : THTTPLanguages; baseURL, code, display: String);
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
    b.Append(#13#10+'Copyright: UCUM is Copyright &copy; 1999+ Regenstrief Institute, Inc. and The UCUM Organization, Indianapolis, IN. See [Terms Of Use](http://unitsofmeasure.org/trac//wiki/TermsOfUse)'+#13#10);
    card.detail := b.ToString;
  finally
    b.Free;
  end;
end;

function TUcumServices.getDefinedForms(code: String): TFslMap<TUcumDefinedUnit>;
var
  base : TUcumBaseUnit;
  du : TUcumDefinedUnit;
begin
  if Code = '' then
    RaiseError('Convert', 'A unit is required');
  result := TFslMap<TUcumDefinedUnit>.Create('Ucum.res');
  Try
    base := FModel.baseUnits[code];
    if assigned(base) then
      for du in FModel.definedUnits.Values do
        if not du.isSpecial And (getCanonicalUnits(du.code) = code) Then
          result.Add(du.code, du.Link);
    result.link;
  Finally
    result.free;
  End;
end;

function TUcumServices.getDefinition(code: String): String;
begin
  result := '';
end;

procedure TUcumServices.getCommonUnits(propertyName: String; oList : TFslStringList);
var
  p : TUcumProperty;
begin
  oList.Clear;
  oList.SortAscending;
  oList.Sorted;
  if Model.Properties.TryGetValue(propertyName, p) then
    oList.Assign(p.CommonUnits);
end;

function TUcumServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

procedure TUcumServices.getProperties(oList: TFslStringList);
var
  p : TUcumProperty;
begin
  oList.Clear;
  oList.SortAscending;
  oList.Sorted;
  for p in Model.Properties.Values do
    oList.Add(p.Name);
end;

function TUcumServices.multiply(o1, o2: TUcumPair): TUcumPair;
var
  res : TUcumPair;
begin
  res := TUcumPair.Create(TFslDecimal.makeOne, '');
  Try
    res.value := o1.value.Multiply(o2.Value);
    res.UnitCode := o1.UnitCode +'.'+o2.UnitCode;
    result := getCanonicalForm(res);
  Finally
    res.Free;
  End;
end;

function TUcumServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'UCUM';
end;

function TUcumServices.search(kind: TConceptKind; text: String; isRegex: Boolean): TFslList<TUcumConcept>;
var
  oSearch : TUcumSearch;
begin
  if text = '' Then
    raise ETerminologyError.Create('A text to search for is required');
  oSearch := TUcumSearch.Create;
  Try
    result := oSearch.DoSearch(model, kind, text, isRegex);
  Finally
    oSearch.Free;
  End;
end;

function TUcumServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.Create('to do');
end;

procedure TUcumServices.SetCommonUnits(vs: TFHIRValueSetW);
var
  inc : TFhirValueSetComposeIncludeW;
begin
  FCommonUnits.Free;
  FCommonUnits := vs;
  FCommonUnitList.Free;
  FCommonUnitList := nil;
  for inc in FCommonUnits.includes.forEnum do
    if FCommonUnitList = nil then
      FCommonUnitList := inc.concepts;
end;

function TUcumServices.SpecialEnumeration: String;
begin
  if FCommonUnits <> nil then
    result := FCommonUnits.url
  else
    result := '';
end;

function TUcumServices.validate(code: String): String;
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

procedure TUcumServices.Validate(oErrors: TFslStringList);
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
          cu := TUcumExpressionComposer.Compose(c, false);
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
          cu := TUcumExpressionComposer.Compose(c, false);
          if (c.Units.Count = 1) then
          begin
            if (propertyType = model.Properties[c.Units[0].Base.PropertyType].Name) then
              exit('')
            else
              exit('unit '+code+' is of the property type '+model.Properties[c.Units[0].Base.PropertyType].Name+' ('+cu+'), not '+propertyType+' as required.');
          end;

          // defined special case
          if (propertyType = 'concentration') and (cu = 'm-3') then
            exit('');
           exit('unit '+code+' has the base units '+cu+', and is not from the property '+propertyType+' as required.');
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

function TUcumServices.version(context: TCodeSystemProviderContext): String;
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



function TUcumServices.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FModel.sizeInBytes);
  inc(result, FHandlers.sizeInBytes);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FPath.length * sizeof(char)) + 12);
  inc(result, FCommonUnits.sizeInBytes);
  inc(result, FCommonUnitList.sizeInBytes);
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

function TUcumServiceList.ItemClass: TFslObjectClass;
begin
  result := TUcumServices;
end;

procedure TUcumServiceList.SetDefinition(Value: TUcumServices);
begin
  FDefinition.Free;
  FDefinition := Value;
end;


function TUcumServiceList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDefinition.sizeInBytes);
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

function TUcumServices.Display(context: TCodeSystemProviderContext; const lang : THTTPLanguages): string;
begin
  if context = nil then
    result := ''
  else
    result := getDisplay(TUCUMContext(context).concept.code, lang);
end;

procedure TUcumServices.Displays(context: TCodeSystemProviderContext; list: TStringList; const lang : THTTPLanguages);
begin
  list.Add(Code(context).Trim);
end;

function TUcumServices.divideBy(o1, o2: TUcumPair): TUcumPair;
var
  res : TUcumPair;
  s : String;
begin
  res := TUcumPair.Create(TFslDecimal.makeOne, '');
  Try
    res.value := o1.value / o2.Value;
    if o1.UnitCode.Contains('/') or o1.UnitCode.contains('*') then
      s := '('+ o1.UnitCode +')'
    else
      s := o1.UnitCode;
    if (o2.UnitCode.contains('/') or o2.UnitCode.contains('*')) then
      s := s + '/' + '('+ o2.UnitCode+')'
    else
      s := s + '/' + o2.UnitCode;
    res.UnitCode := s;
    result := getCanonicalForm(res);
  Finally
    res.Free;
  End;
end;

procedure TUcumServices.Displays(code: String; list: TStringList; const lang : THTTPLanguages);
begin
  list.Add(getDisplay(code, lang));
end;

function TUcumServices.getDisplay(code: String; const lang : THTTPLanguages): String;
var
  inc : TFhirValueSetComposeIncludeW;
  cc : TFhirValueSetComposeIncludeConceptW;
begin
  result := analyse(code);
  if FCommonUnits <> nil then
    for inc in FCommonUnits.includes.forEnum do
      for cc in inc.concepts.forEnum do
        if (cc.code = code) and (cc.display <> '') then
          result := cc.display.Trim;
end;

procedure TUcumServices.Import(sFilename: String);
var
  oXml : TMXmlDocument;
  oElem : TMXmlElement;
  oErrors : TFslStringList;
  bu : TUcumBaseUnit;
  du :  TUcumDefinedUnit;
begin
  oXml := TMXmlParser.parseFile(sFilename, [xpDropWhitespace]);
  try
    if oXml.document.Name <> 'root' Then
      raise ETerminologySetup.create('Invalid ucum essence file');
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
     begin
       bu := ParseBaseUnit(oElem);
       FModel.baseUnits.Add(bu.code, bu)
     end
     Else if oElem.Name = 'unit' Then
     begin
       du := ParseUnit(oElem);
       FModel.definedUnits.Add(du.code, du);
     end
     else
       raise ETerminologySetup.create('unrecognised element '+oElem.Name);
      oElem := oElem.nextElement;
    End;
    oErrors := TFslStringList.Create;
    Try
      Validate(oErrors);
      if oErrors.Count > 0 then
        raise ETerminologySetup.create(oErrors.asText);
    Finally
      oErrors.Free;
    End;
  finally
    oXml.Free;
  end;
end;

class function TUcumServices.checkFile(sFilename: String): String;
var
  oXml : TMXmlDocument;
  rd : String;
begin
  try
    oXml := TMXmlParser.parseFile(sFilename, [xpDropWhitespace]);
    try
      if oXml.document.Name <> 'root' Then
        raise ETerminologySetup.create('Invalid ucum essence file - "root" element not found')
      else
      begin
        rd := oXml.document.Attribute['revision-date'];
        rd := copy(rd, 8, length(rd)-9);
        result := 'Ok (version = '+oXml.document.attribute['version']+', date = '+rd+')';
      end;
    finally
      oXml.Free;
    end;
  except
    on e : Exception do
      result := 'Error: '+e.message;
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

function TUcumServices.systemUri(context : TCodeSystemProviderContext): String;
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

function TUcumServices.filter(prop: String; op: TFhirFilterOperator; value: String; prep : TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
begin
  if (prop = 'canonical') and (op in [foEqual]) then
    result := TUcumFilterContext.create(value)
  else
    result := nil;
end;

function TUcumServices.FilterConcept(ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
var
  context : TUcumFilterContext;
begin
  context := TUcumFilterContext(ctxt);
  result := TUCUMContext.create(FCommonUnitList[context.FCursor].link);
end;

function TUcumServices.FilterMore(ctxt: TCodeSystemProviderFilterContext): boolean;
var
  context : TUcumFilterContext;
begin
  context := TUcumFilterContext(ctxt);
  inc(context.FCursor);
  result := context.FCursor < FCommonUnitList.count;
end;

function TUcumServices.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String; var message : String): TCodeSystemProviderContext;
begin
  result := nil;
end;

function TUcumServices.locateIsA(code, parent: String; disallowParent : boolean = false): TCodeSystemProviderContext;
begin
  result := nil;
end;

function TUcumServices.ParseDecimal(S, s1: String): TFslDecimal;
begin
  if s = '' then
    result := TFslDecimal.makeOne
  Else
    result := TFslDecimal.ValueOf(s);
end;

function TUcumServices.ParsePrefix(oElem: TMXmlElement): TUcumPrefix;
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
        raise ETerminologySetup.Create('unknown element in prefix: '+oChild.Name);
      oChild := oChild.nextElement;
    End;
    result.Link;
  Finally
    result.Free;
  End;
End;

function TUcumServices.ParseBaseUnit(oElem: TMXmlElement): TUcumBaseUnit;
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
        result.PropertyType := oChild.allText
      else
        raise ETerminologyError.Create('unknown element in base unit: '+oChild.Name);
      oChild := oChild.nextElement;
    End;
    result.Link;
  Finally
    result.Free;
  End;
End;

function TUcumServices.ParseUnit(oElem: TMXmlElement): TUcumDefinedUnit;
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
        result.PropertyType := oChild.allText
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
        raise ETerminologySetup.Create('unknown element in unit: '+oChild.Name);
      oChild := oChild.nextElement;
    End;
    result.Link;
  Finally
    result.Free;
  End;
End;

{ TUcumFilterContext }

constructor TUcumFilterContext.Create(canonical: String);
begin
  inherited Create;
  FCursor := -1;
  FCanonical := canonical;
end;

function TUcumFilterContext.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCanonical.length * sizeof(char)) + 12);
end;

{ TUCUMContext }

constructor TUCUMContext.Create(concept: TFhirValueSetComposeIncludeConceptW);
begin
  inherited Create;
  FConcept := concept;
end;

constructor TUCUMContext.Create(code: String);
begin
  inherited Create;
  FConcept := TFhirValueSetComposeIncludeConceptLocal.Create(TFHIRNullObject.create);
  FConcept.code := code;
end;

destructor TUCUMContext.Destroy;
begin
  FConcept.Free;
  inherited;
end;

procedure TUCUMContext.SetConcept(Value: TFhirValueSetComposeIncludeConceptW);
begin
  FConcept.Free;
  FConcept := Value;
end;

function TUCUMContext.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FConcept.sizeInBytes);
end;

{ TUcumServiceImplementation }

constructor TUcumServiceImplementation.Create(svc: TUcumServices);
begin
  inherited create;
  FSvc := svc;
end;

destructor TUcumServiceImplementation.Destroy;
begin
  FSvc.Free;
  inherited;
end;

function TUcumServiceImplementation.divideBy(o1, o2: TUcumPair): TUcumPair;
begin
  result := FSvc.divideBy(o1, o2);
end;

function TUcumServiceImplementation.getCanonicalForm(value: TUcumPair): TUcumPair;
begin
  result := FSvc.getCanonicalForm(value);
end;

function TUcumServiceImplementation.isConfigured: boolean;
begin
  result :=  FSvc <> nil;
end;

function TUcumServiceImplementation.multiply(o1, o2: TUcumPair): TUcumPair;
begin
  result := FSvc.multiply(o1, o2);
end;

function TUcumServiceImplementation.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSvc.sizeInBytes);
end;

End.


