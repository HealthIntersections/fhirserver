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
  fsl_utilities, fsl_base, fsl_stream, fsl_lang, fsl_http,
  fhir_common,
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

  TIso4217Services = class (TCodeSystemProvider)
  private
    FCurrencies : TIso4217CurrencySet;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TIso4217Services; overload;

    function description : String; override;
    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function systemUri(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; const lang : THTTPLanguages):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string; override;
    procedure Displays(code : String; list : TStringList; const lang : THTTPLanguages); override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList; const lang : THTTPLanguages); override;
    function Definition(context : TCodeSystemProviderContext) : string; override;

    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;

    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    function subsumesTest(codeA, codeB : String) : String; override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
  end;


implementation

{ TIso4217Services }

Constructor TIso4217Services.create();
begin
  inherited Create;
  FCurrencies := TIso4217CurrencySet.Create;
end;


function TIso4217Services.TotalCount : integer;
begin
  result := FCurrencies.Codes.Count;
end;


function TIso4217Services.systemUri(context : TCodeSystemProviderContext) : String;
begin
  result := 'urn:iso:std:iso:4217';
end;

function TIso4217Services.getDefinition(code: String): String;
begin
  result := '';
end;

function TIso4217Services.getDisplay(code : String; const lang : THTTPLanguages):String;
begin
  result := FCurrencies.Map[code].display.Trim;
end;

function TIso4217Services.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

procedure TIso4217Services.Displays(code : String; list : TStringList; const lang : THTTPLanguages);
begin
  list.Add(getDisplay(code, lang));
end;


function TIso4217Services.locate(code : String; var message : String) : TCodeSystemProviderContext;
begin
  result := TIso4217Concept.Create(FCurrencies.Map[code].link);
end;

function TIso4217Services.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TIso4217Concept(context).code;
end;

function TIso4217Services.Definition(context: TCodeSystemProviderContext): string;
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

function TIso4217Services.Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string;
begin
  result := TIso4217Concept(context).display.Trim;
end;

procedure TIso4217Services.Displays(context: TCodeSystemProviderContext; list: TStringList; const lang : THTTPLanguages);
begin
  list.Add(Display(context, lang));
end;

function TIso4217Services.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // 4217 doesn't do abstract
end;

function TIso4217Services.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TIso4217Services.Link: TIso4217Services;
begin
  result := TIso4217Services(Inherited Link);
end;

function TIso4217Services.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  if (context = nil) then
    result := TotalCount
  else
    result := 0; // no children
end;

function TIso4217Services.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  result := TIso4217Concept.Create(FCurrencies.Codes[ndx].link);
end;

function TIso4217Services.locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('locateIsA not supported by Iso4217'); // Iso4217 doesn't have formal subsumption property, so this is not used
end;


function TIso4217Services.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  // nothing
  result := true;
end;

function TIso4217Services.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.create('TIso4217Services.searchFilter');
end;

function TIso4217Services.subsumesTest(codeA, codeB: String): String;
begin
  result := 'not-subsumed';
end;

function TIso4217Services.filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
var
  res : TIso4217ConceptFilter;
  c : TIso4217Currency;
begin
  if (prop = 'decimals') and (op = foEqual) then
  begin
    res := TIso4217ConceptFilter.create;
    try
      for c in FCurrencies.Codes do
        if inttostr(c.decimals) = value then
          res.flist.Add(TIso4217Concept.create(c.link));
        result := res.link;
    finally
      res.Free;
    end;
  end
  else
    raise ETerminologyError.create('the filter '+prop+' '+CODES_TFhirFilterOperator[op]+' = '+value+' is not support for '+systemUri(nil));
end;

function TIso4217Services.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.create('TIso4217Services.filterLocate');
end;

function TIso4217Services.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  TIso4217ConceptFilter(ctxt).FCursor := TIso4217ConceptFilter(ctxt).FCursor + 1;
  result := TIso4217ConceptFilter(ctxt).FCursor < TIso4217ConceptFilter(ctxt).FList.Count;
end;

function TIso4217Services.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  result := TIso4217ConceptFilter(ctxt).FList[TIso4217ConceptFilter(ctxt).FCursor];
end;

function TIso4217Services.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyTodo.create('TIso4217Services.InFilter');
end;

procedure TIso4217Services.Close(ctxt: TCodeSystemProviderContext);
begin
//  ctxt.free;
end;

procedure TIso4217Services.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;

procedure TIso4217Services.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  raise ETerminologyTodo.create('TIso4217Services.Close');
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
  FList.Free;
  inherited;
end;

function TIso4217ConceptFilter.link: TIso4217ConceptFilter;
begin
  result := TIso4217ConceptFilter(inherited Link);
end;

{ TIso4217Concept }

constructor TIso4217Concept.create(o: TIso4217Currency);
begin
  Inherited Create;
  FCurrency := o;
end;

destructor TIso4217Concept.Destroy;
begin
  FCurrency.Free;
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
