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
  fsl_utilities, fsl_base, fsl_stream, fsl_http,
  fhir_common,
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

  TUSStateServices = class (TCodeSystemProvider)
  private
    FCodes : TFslList<TUSStateConcept>;
    FMap : TFslMap<TUSStateConcept>;

    procedure load;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TUSStateServices; overload;

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

{ TUSStateServices }

Constructor TUSStateServices.create();
begin
  inherited Create;
  FCodes := TFslList<TUSStateConcept>.create;
  FMap := TFslMap<TUSStateConcept>.create('tx.usstate');
  FMap.defaultValue := nil;
  Load;
end;


function TUSStateServices.TotalCount : integer;
begin
  result := FCodes.Count;
end;


function TUSStateServices.systemUri(context : TCodeSystemProviderContext) : String;
begin
  result := 'https://www.usps.com/';
end;

function TUSStateServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TUSStateServices.getDisplay(code : String; const lang : THTTPLanguages):String;
var
  v : TUSStateConcept;
begin
  if Fmap.TryGetValue(code, v) then
    result := v.display.Trim
  else
    result := '';
end;

function TUSStateServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

procedure TUSStateServices.Displays(code : String; list : TStringList; const lang : THTTPLanguages);
begin
  list.Add(getDisplay(code, lang));
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
      c.Free;
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

function TUSStateServices.locate(code : String; var message : String) : TCodeSystemProviderContext;
begin
  result := FMap[code];
end;


function TUSStateServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TUSStateConcept(context).code;
end;

function TUSStateServices.Definition(context: TCodeSystemProviderContext): string;
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
  FCodes.Free;
  inherited;
end;

function TUSStateServices.Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string;
begin
  result := TUSStateConcept(context).display.Trim;
end;

procedure TUSStateServices.Displays(context: TCodeSystemProviderContext; list: TStringList; const lang : THTTPLanguages);
begin
  list.Add(Display(context, lang));
end;

function TUSStateServices.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // USState doesn't do abstract
end;

function TUSStateServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TUSStateServices.Link: TUSStateServices;
begin
  result := TUSStateServices(Inherited Link);
end;

function TUSStateServices.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  if (context = nil) then
    result := TotalCount
  else
    result := 0; // no children
end;

function TUSStateServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  result := FCodes[ndx];
end;

function TUSStateServices.locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('locateIsA not supported by USState'); // USState doesn't have formal subsumption property, so this is not used
end;


function TUSStateServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  // nothing
  result := true;
end;

function TUSStateServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.create('TUSStateServices.searchFilter');
end;

function TUSStateServices.subsumesTest(codeA, codeB: String): String;
begin
  result := 'not-subsumed';
end;

function TUSStateServices.filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.create('the filter '+prop+' '+CODES_TFhirFilterOperator[op]+' = '+value+' is not support for '+systemUri(nil));
end;

function TUSStateServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.create('TUSStateServices.filterLocate');
end;

function TUSStateServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  TUSStateConceptFilter(ctxt).FCursor := TUSStateConceptFilter(ctxt).FCursor + 1;
  result := TUSStateConceptFilter(ctxt).FCursor < TUSStateConceptFilter(ctxt).FList.Count;
end;

function TUSStateServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  result := TUSStateConceptFilter(ctxt).FList[TUSStateConceptFilter(ctxt).FCursor];
end;

function TUSStateServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyTodo.create('TUSStateServices.InFilter');
end;

procedure TUSStateServices.Close(ctxt: TCodeSystemProviderContext);
begin
//  ctxt.free;
end;

procedure TUSStateServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;

procedure TUSStateServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  raise ETerminologyTodo.create('TUSStateServices.Close');
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
  FList.Free;
  inherited;
end;

function TUSStateConceptFilter.link: TUSStateConceptFilter;
begin
  result := TUSStateConceptFilter(inherited Link);
end;

end.

