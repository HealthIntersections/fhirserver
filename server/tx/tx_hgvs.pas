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

// https://lhcforms.nlm.nih.gov/fhir/hgvs-validator/

{$I fhir.inc}

interface

uses
  SysUtils, Classes, Generics.Collections,
  fsl_base, fsl_utilities, fsl_http, fsl_json, fsl_fetcher,
  fhir_objects, fhir_common, fhir_factory, fhir_features,
  fhir_cdshooks,
  ftx_service;

type
 THGVSCode = class (TCodeSystemProviderContext)
 private
   FCode : String;
 public
   property code : String read FCode write FCode;
 end;

 { THGVSProvider }

 THGVSProvider = class (TCodeSystemProvider)
  private
  public
    function link : THGVSProvider; overload;

    function description : String; override;
    function TotalCount : integer;  override;
    function getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function systemUri : String; override;
    function version : String; override;
    function defaultToLatest : boolean; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String; override;
    function getDefinition(opContext : TTxOperationContext; code : String):String; override;
    function locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; overload; override;
    function locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; override;
    function IsInactive(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; override;
    function Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;
    function Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string; override;
    function Definition(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;
    procedure Designations(opContext : TTxOperationContext; context : TCodeSystemProviderContext; list : TConceptDesignations); overload; override;
    function doesFilter(opContext : TTxOperationContext; prop : String; op : TFhirFilterOperator; value : String) : boolean; override;

    function getPrepContext(opContext : TTxOperationContext) : TCodeSystemProviderFilterPreparationContext; override;
    function searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function specialFilter(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function prepare(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext) : boolean; override; // true if the underlying provider collapsed multiple filters
    function filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; overload; override;
    function filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; overload; override;
    function FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function filterSize(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : integer; override;
    function FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(opContext : TTxOperationContext; textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure extendLookup(opContext : TTxOperationContext; factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; langList : THTTPLanguageList; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;
    function subsumesTest(opContext : TTxOperationContext; codeA, codeB : String) : String; override;

    function SpecialEnumeration : String; override;
    procedure getCDSInfo(opContext : TTxOperationContext; card : TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display : String); override;

    function defToThisVersion(specifiedVersion : String) : boolean; override;
     procedure defineFeatures(opContext : TTxOperationContext; features : TFslList<TFHIRFeature>); override;
 end;

implementation

{ THGVSProvider }


procedure THGVSProvider.defineFeatures(opContext : TTxOperationContext; features: TFslList<TFHIRFeature>);
begin
end;

function THGVSProvider.getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
begin
  result := TCodeSystemIteratorContext.Create(nil, 0);
end;

function THGVSProvider.Code(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  result := (Context as THGVSCode).code;
end;

function THGVSProvider.Definition(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
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

function THGVSProvider.Display(opContext : TTxOperationContext; context: TCodeSystemProviderContext; langList : THTTPLanguageList): string;
begin
  result := Code(opContext, Context);
end;

procedure THGVSProvider.Designations(opContext : TTxOperationContext; context: TCodeSystemProviderContext; list: TConceptDesignations);
begin
  list.addDesignation(true, true, '', '', code(opContext, context));
end;

function THGVSProvider.doesFilter(opContext : TTxOperationContext; prop: String; op: TFhirFilterOperator; value: String): boolean;
begin
  result := false;
end;

procedure THGVSProvider.extendLookup(opContext : TTxOperationContext; factory: TFHIRFactory; ctxt: TCodeSystemProviderContext; langList : THTTPLanguageList; props: TArray<String>; resp: TFHIRLookupOpResponseW);
begin
  // nothing
end;

function THGVSProvider.filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop: String; op: TFhirFilterOperator; value: String; prep: TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS', itNotSupported);
end;

function THGVSProvider.FilterConcept(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS', itNotSupported);
end;

function THGVSProvider.filterLocate(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext; code: String): TCodeSystemProviderContext;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS', itNotSupported);
end;

function THGVSProvider.filterLocate(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext; code: String; var message: String): TCodeSystemProviderContext;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS', itNotSupported);
end;

function THGVSProvider.FilterMore(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): boolean;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS', itNotSupported);
end;

function THGVSProvider.filterSize(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): integer;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS', itNotSupported);
end;

procedure THGVSProvider.getCDSInfo(opContext : TTxOperationContext; card: TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display: String);
begin
  // nothing
end;

function THGVSProvider.getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
begin
  result := nil;
  context.next;
end;

function THGVSProvider.getDefinition(opContext : TTxOperationContext; code: String): String;
begin
  result := '';
end;

function THGVSProvider.getDisplay(opContext : TTxOperationContext; code: String; langList : THTTPLanguageList): String;
begin
  result := code;
end;

function THGVSProvider.getPrepContext(opContext : TTxOperationContext): TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

function THGVSProvider.InFilter(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext; concept: TCodeSystemProviderContext): Boolean;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS', itNotSupported);
end;

function THGVSProvider.IsAbstract(opContext : TTxOperationContext; context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function THGVSProvider.IsInactive(opContext : TTxOperationContext; context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function THGVSProvider.isNotClosed(opContext : TTxOperationContext; textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function THGVSProvider.link: THGVSProvider;
begin
  result := THGVSProvider(inherited link);
end;

function THGVSProvider.locate(opContext : TTxOperationContext; code: String; altOpt : TAlternateCodeOptions; var message: String): TCodeSystemProviderContext;
var
  json, o : TJsonObject;
begin
  result := nil;
  message := '';
  try
    json := TInternetFetcher.fetchJson('https://clinicaltables.nlm.nih.gov/fhir/R4/CodeSystem/hgvs/$validate-code?code='+code, 5000);
    try
      for o in json.forceArr['parameter'].asObjects.forEnum do
      begin
        if (o.str['name'] = 'result') and (o.bool['valueBoolean']) then
        begin
          result := THGVSCode.Create;
          THGVSCode(result).code := code;
        end
        else if (o.str['name'] = 'message') then
          CommaAdd(message, o.str['message']);
      end;
    finally
      json.free;
    end;
  except
    on e : Exception do
      raise EFHIRException.Create('Error parsing HGVS response: '+e.message);
  end;
end;

function THGVSProvider.locateIsA(opContext : TTxOperationContext; code, parent: String; disallowParent : boolean = false): TCodeSystemProviderContext;
begin
  result := nil;
end;

function THGVSProvider.name(context: TCodeSystemProviderContext): String;
begin
  result := 'HGVS';
end;

function THGVSProvider.prepare(opContext : TTxOperationContext; prep: TCodeSystemProviderFilterPreparationContext): boolean;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS', itNotSupported);
end;

function THGVSProvider.searchFilter(opContext : TTxOperationContext; filter: TSearchFilterText; prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS', itNotSupported);
end;

function THGVSProvider.SpecialEnumeration: String;
begin
  result := '';
end;

function THGVSProvider.specialFilter(opContext : TTxOperationContext; prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.Create('Filters are not supported for HGVS', itNotSupported);
end;

function THGVSProvider.subsumesTest(opContext : TTxOperationContext; codeA, codeB: String): String;
begin
  raise ETerminologyError.Create('Subsumption is not supported for HGVS', itNotSupported);
end;

function THGVSProvider.systemUri: String;
begin
  result := 'http://varnomen.hgvs.org';
end;

function THGVSProvider.TotalCount: integer;
begin
  result := 0;
end;

function THGVSProvider.version: String;
begin
  result := '2.0';
end;

function THGVSProvider.defaultToLatest: boolean;
begin
  Result := true;
end;

end.
