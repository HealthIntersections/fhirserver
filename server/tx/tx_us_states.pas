unit tx_us_states;

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
  fsl_utilities, fsl_base, fsl_stream, fsl_http, fsl_lang, fsl_i18n,
  fhir_objects, fhir_common, fhir_features,
  ftx_service;

type
  TUSStateConcept = class (TCodeSystemProviderContext)
  private
    FDisplay: String;
    FCode: String;
  public
    function link : TUSStateConcept; overload;

    property code : String read FCode write FCode;
    property display : String read FDisplay write FDisplay;
  end;

  TUSStateConceptFilter = class (TCodeSystemProviderFilterContext)
  private
    FList : TFslList<TUSStateConcept>;
    FCursor : integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TUSStateConceptFilter; overload;
  end;

  { TUSStateServices }

  TUSStateServices = class (TCodeSystemProvider)
  private
    FCodes : TFslList<TUSStateConcept>;
    FMap : TFslMap<TUSStateConcept>;

    procedure load;
  public
    constructor Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport);
    destructor Destroy; Override;
    Function Link : TUSStateServices; overload;

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

{ TUSStateServices }

constructor TUSStateServices.Create(languages: TIETFLanguageDefinitions; i18n : TI18nSupport);
begin
  inherited;
  FCodes := TFslList<TUSStateConcept>.Create;
  FMap := TFslMap<TUSStateConcept>.Create('tx.usstate');
  FMap.defaultValue := nil;
  Load;
end;


procedure TUSStateServices.defineFeatures(opContext : TTxOperationContext; features: TFslList<TFHIRFeature>);
begin
end;

function TUSStateServices.TotalCount : integer;
begin
  result := FCodes.Count;
end;


function TUSStateServices.systemUri : String;
begin
  result := 'https://www.usps.com/';
end;

function TUSStateServices.defaultToLatest: boolean;
begin
  Result := true;
end;

function TUSStateServices.getDefinition(opContext : TTxOperationContext; code: String): String;
begin
  result := '';
end;

function TUSStateServices.getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String;
var
  v : TUSStateConcept;
begin
  if Fmap.TryGetValue(code, v) then
    result := v.display.Trim
  else
    result := '';
end;

function TUSStateServices.getPrepContext(opContext : TTxOperationContext): TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;


procedure TUSStateServices.load;
  procedure doLoad(code, display : String);
  var
    c : TUSStateConcept;
  begin
    c := TUSStateConcept.Create;
    try
      c.code := code;
      c.display := display;
      FCodes.Add(c.Link);
      FMap.Add(code, c.Link);
    finally
      c.free;
    end;
  end;
begin
  doLoad('AL', 'Alabama');
  doLoad('AK', 'Alaska');
  doLoad('AS', 'American Samoa');
  doLoad('AZ', 'Arizona');
  doLoad('AR', 'Arkansas');
  doLoad('CA', 'California');
  doLoad('CO', 'Colorado');
  doLoad('CT', 'Connecticut');
  doLoad('DE', 'Delaware');
  doLoad('DC', 'District of Columbia');
  doLoad('FM', 'Federated States of Micronesia');
  doLoad('FL', 'Florida');
  doLoad('GA', 'Georgia');
  doLoad('GU', 'Guam');
  doLoad('HI', 'Hawaii');
  doLoad('ID', 'Idaho');
  doLoad('IL', 'Illinois');
  doLoad('IN', 'Indiana');
  doLoad('IA', 'Iowa');
  doLoad('KS', 'Kansas');
  doLoad('KY', 'Kentucky');
  doLoad('LA', 'Louisiana');
  doLoad('ME', 'Maine');
  doLoad('MH', 'Marshall Islands');
  doLoad('MD', 'Maryland');
  doLoad('MA', 'Massachusetts');
  doLoad('MI', 'Michigan');
  doLoad('MN', 'Minnesota');
  doLoad('MS', 'Mississippi');
  doLoad('MO', 'Missouri');
  doLoad('MT', 'Montana');
  doLoad('NE', 'Nebraska');
  doLoad('NV', 'Nevada');
  doLoad('NH', 'New Hampshire');
  doLoad('NJ', 'New Jersey');
  doLoad('NM', 'New Mexico');
  doLoad('NY', 'New York');
  doLoad('NC', 'North Carolina');
  doLoad('ND', 'North Dakota');
  doLoad('MP', 'Northern Mariana Islands');
  doLoad('OH', 'Ohio');
  doLoad('OK', 'Oklahoma');
  doLoad('OR', 'Oregon');
  doLoad('PW', 'Palau');
  doLoad('PA', 'Pennsylvania');
  doLoad('PR', 'Puerto Rico');
  doLoad('RI', 'Rhode Island');
  doLoad('SC', 'South Carolina');
  doLoad('SD', 'South Dakota');
  doLoad('TN', 'Tennessee');
  doLoad('TX', 'Texas');
  doLoad('UT', 'Utah');
  doLoad('VT', 'Vermont');
  doLoad('VI', 'Virgin Islands');
  doLoad('VA', 'Virginia');
  doLoad('WA', 'Washington');
  doLoad('WV', 'West Virginia');
  doLoad('WI', 'Wisconsin');
  doLoad('WY', 'Wyoming');
  doLoad('AE', 'Armed Forces Europe, the Middle East, and Canada');
  doLoad('AP', 'Armed Forces Pacific');
  doLoad('AA', 'Armed Forces Americas (except Canada)');
end;

function TUSStateServices.locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext;
begin
  result := FMap[code].link;
end;


function TUSStateServices.Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string;
begin
  result := TUSStateConcept(context).code;
end;

function TUSStateServices.Definition(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TUSStateServices.description: String;
begin
  result := 'US State Codes';
end;

destructor TUSStateServices.Destroy;
begin
  FMap.free;
  FCodes.free;
  inherited;
end;

function TUSStateServices.Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string;
begin
  result := TUSStateConcept(context).display.Trim;
end;

procedure TUSStateServices.Designations(opContext : TTxOperationContext; context: TCodeSystemProviderContext; list: TConceptDesignations);
begin
  list.addDesignation(true, true, '', '', Display(opContext, context, nil));
end;

function TUSStateServices.IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // USState doesn't do abstract
end;

function TUSStateServices.isNotClosed(opContext : TTxOperationContext; textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TUSStateServices.Link: TUSStateServices;
begin
  result := TUSStateServices(Inherited Link);
end;

function TUSStateServices.getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
begin
  if (context = nil) then
    result := TCodeSystemIteratorContext.Create(nil, TotalCount)
  else
    result := TCodeSystemIteratorContext.Create(nil, 0); // no children
end;

function TUSStateServices.getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
begin
  result := FCodes[context.current].link;
  context.next;
end;

function TUSStateServices.locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.Create('locateIsA not supported by USState', itNotSupported); // USState doesn't have formal subsumption property, so this is not used
end;


function TUSStateServices.prepare(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  // nothing
  result := false;
end;

function TUSStateServices.searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.Create('TUSStateServices.searchFilter');
end;

function TUSStateServices.subsumesTest(opContext : TTxOperationContext; codeA, codeB: String): String;
begin
  result := 'not-subsumed';
end;

function TUSStateServices.filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.Create('the filter '+prop+' '+CODES_TFhirFilterOperator[op]+' = '+value+' is not supported for '+systemUri, itNotSupported);
end;

function TUSStateServices.filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('TUSStateServices.filterLocate');
end;

function TUSStateServices.FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  TUSStateConceptFilter(ctxt).FCursor := TUSStateConceptFilter(ctxt).FCursor + 1;
  result := TUSStateConceptFilter(ctxt).FCursor < TUSStateConceptFilter(ctxt).FList.Count;
end;

function TUSStateServices.filterSize(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): integer;
begin
  result := TUSStateConceptFilter(ctxt).FList.Count;
end;

function TUSStateServices.FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  result := TUSStateConceptFilter(ctxt).FList[TUSStateConceptFilter(ctxt).FCursor].link;
end;

function TUSStateServices.InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyTodo.Create('TUSStateServices.InFilter');
end;

{ TUSStateConcept }

function TUSStateConcept.link: TUSStateConcept;
begin
  result := TUSStateConcept(inherited Link);
end;

{ TUSStateConceptFilter }

constructor TUSStateConceptFilter.Create;
begin
  inherited;
  FList := TFslList<TUSStateConcept>.Create;
  FCursor := -1;
end;

destructor TUSStateConceptFilter.Destroy;
begin
  FList.free;
  inherited;
end;

function TUSStateConceptFilter.link: TUSStateConceptFilter;
begin
  result := TUSStateConceptFilter(inherited Link);
end;

end.

