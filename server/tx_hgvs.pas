unit tx_hgvs;

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
  SysUtils, Classes, Generics.Collections,
  fsl_base, fsl_utilities, fsl_http, fsl_json, fsl_fetcher,
  fhir_objects, fhir_common, fhir_factory,
  fhir_cdshooks,
  ftx_service;

type
 THGVSCode = class (TCodeSystemProviderContext)
 private
   FCode : String;
 public
   property code : String read FCode write FCode;
 end;

 THGVSProvider = class (TCodeSystemProvider)
  private
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : THGVSProvider; overload;

    function description : String; override;
    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function systemUri(context : TCodeSystemProviderContext) : String; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; const lang : THTTPLanguages):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; overload; override;
    function locate(code : String) : TCodeSystemProviderContext; overload; override;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function IsInactive(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string; override;
    function Definition(context : TCodeSystemProviderContext) : string; override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList; const lang : THTTPLanguages); overload; override;
    procedure Displays(code : String; list : TStringList; const lang : THTTPLanguages); overload; override;
    function doesFilter(prop : String; op : TFhirFilterOperator; value : String) : boolean; override;

    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function specialFilter(prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; override; // true if the underlying provider collapsed multiple filters
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; overload; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; overload; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure extendLookup(factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; const lang : THTTPLanguages; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;
    function subsumesTest(codeA, codeB : String) : String; override;

    function SpecialEnumeration : String; override;
    procedure getCDSInfo(card : TCDSHookCard; const lang : THTTPLanguages; baseURL, code, display : String); override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); overload; override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); overload; override;
    procedure Close(ctxt : TCodeSystemProviderContext); overload; override;
    function defToThisVersion(specifiedVersion : String) : boolean; override;
  end;

implementation

{ THGVSProvider }

constructor THGVSProvider.Create;
begin
  inherited Create;
end;

destructor THGVSProvider.Destroy;
begin
  inherited;
end;

function THGVSProvider.ChildCount(context: TCodeSystemProviderContext): integer;
begin
  result := 0;
end;

procedure THGVSProvider.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  ctxt.Free;
end;

procedure THGVSProvider.Close(ctxt: TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;

procedure THGVSProvider.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.Free;
end;

function THGVSProvider.Code(context: TCodeSystemProviderContext): string;
begin
  result := (Context as THGVSCode).code;
end;

function THGVSProvider.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function THGVSProvider.defToThisVersion(specifiedVersion: String): boolean;
begin
  result := true;
end;

function THGVSProvider.description: String;
begin
  result := 'HGVS codes';
end;

function THGVSProvider.Display(context: TCodeSystemProviderContext; const lang: THTTPLanguages): string;
begin
  result := Code(Context);
end;

procedure THGVSProvider.Displays(context: TCodeSystemProviderContext; list: TStringList; const lang: THTTPLanguages);
begin
  list.Add(code(context));
end;

procedure THGVSProvider.Displays(code: String; list: TStringList; const lang: THTTPLanguages);
begin
  list.Add(code);
end;

function THGVSProvider.doesFilter(prop: String; op: TFhirFilterOperator; value: String): boolean;
begin
  result := false;
end;

procedure THGVSProvider.extendLookup(factory: TFHIRFactory; ctxt: TCodeSystemProviderContext; const lang: THTTPLanguages; props: TArray<String>; resp: TFHIRLookupOpResponseW);
begin
  // nothing
end;

function THGVSProvider.filter(prop: String; op: TFhirFilterOperator; value: String; prep: TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS');
end;

function THGVSProvider.FilterConcept(ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS');
end;

function THGVSProvider.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String): TCodeSystemProviderContext;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS');
end;

function THGVSProvider.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String; var message: String): TCodeSystemProviderContext;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS');
end;

function THGVSProvider.FilterMore(ctxt: TCodeSystemProviderFilterContext): boolean;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS');
end;

procedure THGVSProvider.getCDSInfo(card: TCDSHookCard; const lang: THTTPLanguages; baseURL, code, display: String);
begin
  // nothing
end;

function THGVSProvider.getcontext(context: TCodeSystemProviderContext; ndx: integer): TCodeSystemProviderContext;
begin
  result := nil;
end;

function THGVSProvider.getDefinition(code: String): String;
begin
  result := '';
end;

function THGVSProvider.getDisplay(code: String; const lang: THTTPLanguages): String;
begin
  result := code;
end;

function THGVSProvider.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

function THGVSProvider.InFilter(ctxt: TCodeSystemProviderFilterContext; concept: TCodeSystemProviderContext): Boolean;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS');
end;

function THGVSProvider.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function THGVSProvider.IsInactive(context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function THGVSProvider.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function THGVSProvider.link: THGVSProvider;
begin
  result := THGVSProvider(inherited link);
end;

function THGVSProvider.locate(code: String; var message: String): TCodeSystemProviderContext;
var
  json, o : TJsonObject;
begin
  json := TInternetFetcher.fetchJson('https://mutalyzer.nl/json/checkSyntax?variant='+code, 5000);
  try
    if json.bool['valid'] then
    begin
      result := THGVSCode.Create;
      THGVSCode(result).code := code;
    end
    else
    begin
      result := nil;
      message := '';
      for o in json.forceArr['messages'].asObjects.forEnum do
        CommaAdd(message, o.str['message']);
    end;
  finally
    json.Free;
  end;
end;

function THGVSProvider.locate(code: String): TCodeSystemProviderContext;
var
  msg : String;
begin
  result := locate(code, msg);
end;

function THGVSProvider.locateIsA(code, parent: String; disallowParent : boolean = false): TCodeSystemProviderContext;
begin
  result := nil;
end;

function THGVSProvider.name(context: TCodeSystemProviderContext): String;
begin
  result := 'HGVS';
end;

function THGVSProvider.prepare(prep: TCodeSystemProviderFilterPreparationContext): boolean;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS');
end;

function THGVSProvider.searchFilter(filter: TSearchFilterText; prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS');
end;

function THGVSProvider.SpecialEnumeration: String;
begin
  result := '';
end;

function THGVSProvider.specialFilter(prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS');
end;

function THGVSProvider.subsumesTest(codeA, codeB: String): String;
begin
  raise ETerminologyError.Create('Subsumption is not supported for HGVS');
end;

function THGVSProvider.systemUri(context: TCodeSystemProviderContext): String;
begin
  result := 'http://varnomen.hgvs.org';
end;

function THGVSProvider.TotalCount: integer;
begin
  result := 0;
end;

function THGVSProvider.version(context: TCodeSystemProviderContext): String;
begin
  result := '2.0';
end;

end.
