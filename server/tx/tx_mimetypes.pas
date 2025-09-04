unit tx_mimetypes;

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
  fsl_utilities, fsl_stream, fsl_base, fsl_http,
  fhir_objects, fhir_common, fhir_features, fhir_uris,
  ftx_service;

type
  TMTCodeSystemProviderContext = class (TCodeSystemProviderContext)
  private
    FMt : TMimeContentType;
    procedure SetMt(const Value: TMimeContentType);
  public
    constructor Create(mt : TMimeContentType);
    destructor Destroy; override;

    property mt : TMimeContentType read FMt write SetMt;
  end;


  { TMimeTypeCodeServices }

  TMimeTypeCodeServices = class (TCodeSystemProvider)
  public
    destructor Destroy; Override;
    Function Link : TMimeTypeCodeServices; overload;

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

    procedure defineFeatures(opContext : TTxOperationContext; features : TFslList<TFHIRFeature>); override;
  end;

implementation

{ TMimeTypeCodeServices }


procedure TMimeTypeCodeServices.defineFeatures(opContext : TTxOperationContext; features: TFslList<TFHIRFeature>);
begin
end;

function TMimeTypeCodeServices.TotalCount : integer;
begin
  result := -1;   // not bounded
end;


function TMimeTypeCodeServices.version: String;
begin
  result := '';
end;

function TMimeTypeCodeServices.defaultToLatest: boolean;
begin
  Result := true;
end;

function TMimeTypeCodeServices.systemUri : String;
begin
  result := URI_BCP13;
end;

function TMimeTypeCodeServices.getDefinition(opContext : TTxOperationContext; code: String): String;
begin
  result := '';
end;

function TMimeTypeCodeServices.getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String;
begin
  result := code.Trim;
end;

function TMimeTypeCodeServices.getPrepContext(opContext : TTxOperationContext): TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

function TMimeTypeCodeServices.locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext;
var
  mt : TMimeContentType;
begin
  mt := TMimeContentType.parseSingle(code, false);
  try
    if mt.isValid and (mt.sub <> '') then
      result := TMTCodeSystemProviderContext.Create(mt.Link)
    else
      result := nil;
  finally
    mt.free;
  end;
end;

function TMimeTypeCodeServices.Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string;
begin
  result := TMTCodeSystemProviderContext(context).mt.source;
end;

function TMimeTypeCodeServices.Definition(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TMimeTypeCodeServices.description: String;
begin
  result := 'Mime Types';
end;

destructor TMimeTypeCodeServices.Destroy;
begin
  inherited;
end;

function TMimeTypeCodeServices.Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string;
begin
  result := getDisplay(opContext, TMTCodeSystemProviderContext(context).mt.source, langList);
end;

procedure TMimeTypeCodeServices.Designations(opContext : TTxOperationContext; context: TCodeSystemProviderContext; list: TConceptDesignations);
begin
  list.addDesignation(true, true, '', '', Display(opContext, context, nil));
end;

function TMimeTypeCodeServices.IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // MimeTypeCode doesn't do abstract
end;

function TMimeTypeCodeServices.isNotClosed(opContext : TTxOperationContext; textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := true;
end;

function TMimeTypeCodeServices.Link: TMimeTypeCodeServices;
begin
  result := TMimeTypeCodeServices(Inherited Link);
end;

function TMimeTypeCodeServices.getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
begin
  result := TCodeSystemIteratorContext.Create(nil, 0);
end;

function TMimeTypeCodeServices.getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('TMimeTypeCodeServices.getcontext');
end;

function TMimeTypeCodeServices.locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  result := nil; // no subsumption
end;


function TMimeTypeCodeServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'IETF langauge';
end;

function TMimeTypeCodeServices.prepare(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  result := false;
end;

function TMimeTypeCodeServices.searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.Create('TMimeTypeCodeServices.searchFilter');
end;

function TMimeTypeCodeServices.filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.Create('Not a supported filter', itNotSupported);
end;

function TMimeTypeCodeServices.filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
begin
  result := nil;
end;

function TMimeTypeCodeServices.FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  raise ETerminologyTodo.Create('TMimeTypeCodeServices.FilterMore');
end;

function TMimeTypeCodeServices.filterSize(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): integer;
begin
  raise ETerminologyTodo.Create('TMimeTypeCodeServices.FilterSize');
end;

function TMimeTypeCodeServices.FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('TMimeTypeCodeServices.FilterConcept');
end;

function TMimeTypeCodeServices.InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyTodo.Create('TMimeTypeCodeServices.InFilter');
end;

{ TMTCodeSystemProviderContext }

constructor TMTCodeSystemProviderContext.Create(mt: TMimeContentType);
begin
  inherited Create;
  FMt := mt;
end;

destructor TMTCodeSystemProviderContext.Destroy;
begin
  FMt.free;
  inherited;
end;

procedure TMTCodeSystemProviderContext.SetMt(const Value: TMimeContentType);
begin
  FMt.free;
  FMt := Value;
end;

end.
