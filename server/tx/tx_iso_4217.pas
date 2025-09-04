unit tx_iso_4217;

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
  fsl_utilities, fsl_base, fsl_stream, fsl_lang, fsl_http, fsl_i18n, fsl_threads,
  fhir_objects, fhir_common, fhir_features,
  ftx_service;

type
  TIso4217Concept = class (TCodeSystemProviderContext)
  private
    FCurrency : TIso4217Currency;
    function GetCode: String;
    function GetDecimals: integer;
    function GetDisplay: String;
    function GetSymbol: String;
  public
    constructor Create(o : TIso4217Currency);
    destructor Destroy; override;
    function link : TIso4217Concept; overload;

    property code : String read GetCode;
    property display : String read GetDisplay;
    property decimals : integer read GetDecimals;
    property symbol : String read GetSymbol;
  end;

  TIso4217ConceptFilter = class (TCodeSystemProviderFilterContext)
  private
    FList : TFslList<TIso4217Concept>;
    FCursor : integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TIso4217ConceptFilter; overload;
  end;

  { TIso4217Services }

  TIso4217Services = class (TCodeSystemProvider)
  private
    FCurrencies : TIso4217CurrencySet;
  public
    constructor Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport);
    destructor Destroy; Override;
    Function Link : TIso4217Services; overload;

    function description : String; override;
    function TotalCount : integer;  override;
    function getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function systemUri : String; override;
    function defaultToLatest : boolean; override;
    function getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String; override;
    function getDefinition(opContext : TTxOperationContext; code : String):String; override;
    function locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; override;
    function Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;
    function Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string; override;
    procedure Designations(opContext : TTxOperationContext; context : TCodeSystemProviderContext; list : TConceptDesignations); override;
    function Definition(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;

    function getPrepContext(opContext : TTxOperationContext) : TCodeSystemProviderFilterPreparationContext; override;
    function prepare(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;

    function searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function filterSize(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : integer; override;
    function FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(opContext : TTxOperationContext; textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    function subsumesTest(opContext : TTxOperationContext; codeA, codeB : String) : String; override;

    procedure defineFeatures(opContext : TTxOperationContext; features : TFslList<TFHIRFeature>); override;
  end;


implementation

{ TIso4217Services }

constructor TIso4217Services.Create(languages: TIETFLanguageDefinitions; i18n : TI18nSupport);
begin
  inherited;
  FCurrencies := TIso4217CurrencySet.Create;
end;


procedure TIso4217Services.defineFeatures(opContext : TTxOperationContext; features: TFslList<TFHIRFeature>);
begin
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'decimals:equals'));
end;

function TIso4217Services.TotalCount : integer;
begin
  result := FCurrencies.Codes.Count;
end;


function TIso4217Services.systemUri : String;
begin
  result := 'urn:iso:std:iso:4217';
end;

function TIso4217Services.defaultToLatest: boolean;
begin
  Result := true;
end;

function TIso4217Services.getDefinition(opContext : TTxOperationContext; code: String): String;
begin
  result := '';
end;

function TIso4217Services.getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String;
begin
  result := FCurrencies.Map[code].display.Trim;
end;

function TIso4217Services.getPrepContext(opContext : TTxOperationContext): TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

function TIso4217Services.locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext;
var
  c : TIso4217Currency;
begin
  c := FCurrencies.Map[code];
  if (c = nil) then
    result := nil
  else
    result := TIso4217Concept.Create(c.link);
end;

function TIso4217Services.Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string;
begin
  result := TIso4217Concept(context).code;
end;

function TIso4217Services.Definition(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TIso4217Services.description: String;
begin
  result := 'Currencies';
end;

destructor TIso4217Services.Destroy;
begin
  FCurrencies.free;
  inherited;
end;

function TIso4217Services.Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string;
begin
  result := TIso4217Concept(context).display.Trim;
end;

procedure TIso4217Services.Designations(opContext : TTxOperationContext; context: TCodeSystemProviderContext; list: TConceptDesignations);
begin
  list.addDesignation(true, true, '', '', Display(opContext, context, nil));
end;

function TIso4217Services.IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // 4217 doesn't do abstract
end;

function TIso4217Services.isNotClosed(opContext : TTxOperationContext; textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TIso4217Services.Link: TIso4217Services;
begin
  result := TIso4217Services(Inherited Link);
end;

function TIso4217Services.getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
begin
  if (context = nil) then
    result := TCodeSystemIteratorContext.Create(nil, TotalCount)
  else
    result := TCodeSystemIteratorContext.Create(nil, 0); // no children
end;

function TIso4217Services.getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
begin
  result := TIso4217Concept.Create(FCurrencies.Codes[context.current].link);
  context.next;
end;

function TIso4217Services.locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.Create('locateIsA not supported by Iso4217', itNotSupported); // Iso4217 doesn't have formal subsumption property, so this is not used
end;


function TIso4217Services.prepare(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  // nothing
  result := false;
end;

function TIso4217Services.searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.Create('TIso4217Services.searchFilter');
end;

function TIso4217Services.subsumesTest(opContext : TTxOperationContext; codeA, codeB: String): String;
begin
  result := 'not-subsumed';
end;

function TIso4217Services.filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
var
  res : TIso4217ConceptFilter;
  c : TIso4217Currency;
begin
  SetThreadStatus(ClassName+'.filter('+prop+CODES_TFhirFilterOperator[op]+value+')');
  if (prop = 'decimals') and (op = foEqual) then
  begin
    res := TIso4217ConceptFilter.Create;
    try
      for c in FCurrencies.Codes do
        if inttostr(c.decimals) = value then
          res.flist.Add(TIso4217Concept.Create(c.link));
        result := res.link;
    finally
      res.free;
    end;
  end
  else
    raise ETerminologyError.Create('the filter '+prop+' '+CODES_TFhirFilterOperator[op]+' = '+value+' is not supported for '+systemUri, itNotSupported);
end;

function TIso4217Services.filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('TIso4217Services.filterLocate');
end;

function TIso4217Services.FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  TIso4217ConceptFilter(ctxt).FCursor := TIso4217ConceptFilter(ctxt).FCursor + 1;
  result := TIso4217ConceptFilter(ctxt).FCursor < TIso4217ConceptFilter(ctxt).FList.Count;
end;

function TIso4217Services.filterSize(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): integer;
begin
  result := TIso4217ConceptFilter(ctxt).FList.Count;
end;

function TIso4217Services.FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  result := TIso4217ConceptFilter(ctxt).FList[TIso4217ConceptFilter(ctxt).FCursor].link;
end;

function TIso4217Services.InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyTodo.Create('TIso4217Services.InFilter');
end;

{ TIso4217ConceptFilter }

constructor TIso4217ConceptFilter.Create;
begin
  inherited;
  FList := TFslList<TIso4217Concept>.Create;
  FCursor := -1;
end;

destructor TIso4217ConceptFilter.Destroy;
begin
  FList.free;
  inherited;
end;

function TIso4217ConceptFilter.link: TIso4217ConceptFilter;
begin
  result := TIso4217ConceptFilter(inherited Link);
end;

{ TIso4217Concept }

constructor TIso4217Concept.Create(o: TIso4217Currency);
begin
  Inherited Create;
  FCurrency := o;
end;

destructor TIso4217Concept.Destroy;
begin
  FCurrency.free;
  inherited;
end;

function TIso4217Concept.GetCode: String;
begin
  result := FCurrency.code;
end;

function TIso4217Concept.GetDecimals: integer;
begin
  result := FCurrency.decimals;
end;

function TIso4217Concept.GetDisplay: String;
begin
  result := FCurrency.display;
end;

function TIso4217Concept.GetSymbol: String;
begin
  result := FCurrency.symbol;
end;

function TIso4217Concept.link: TIso4217Concept;
begin
  result := TIso4217Concept(inherited link);
end;

end.
