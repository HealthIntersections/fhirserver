unit tx_uri;

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
  fdb_manager,
  fhir_objects, fhir_features, fhir_uris,
  ftx_service;

type
  TUriHolder = class (TCodeSystemProviderContext)
  private
    url : String;
  public
    constructor Create(url : String);
  end;

  { TUriServices }

  TUriServices = class (TCodeSystemProvider)
  public
    Function Link : TUriServices; overload;

    function description : String; override;

    function TotalCount : integer;  override;
    function getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function systemUri : String; override;
    function version : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; langList : THTTPLanguageList):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string; override;
    procedure Designations(context : TCodeSystemProviderContext; list : TConceptDesignations); override;
    function Definition(context : TCodeSystemProviderContext) : string; override;

    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;

    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function filterSize(ctxt : TCodeSystemProviderFilterContext) : integer; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;

    procedure defineFeatures(features : TFslList<TFHIRFeature>); override;
  end;

implementation

{ TUriServices }

function TUriServices.TotalCount : integer;
begin
  result := -1;
end;


function TUriServices.version: String;
begin
  result := 'n/a';
end;

function TUriServices.systemUri : String;
begin
  result := URI_URIs;
end;

function TUriServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TUriServices.getDisplay(code : String; langList : THTTPLanguageList):String;
begin
  result := '';
end;

function TUriServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  raise ETerminologyTodo.Create('TUriServices.getPrepContext');
end;

function TUriServices.locate(code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext;
begin
  result := TUriHolder.Create(code);
end;


function TUriServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TUriHolder(context).url;
end;

procedure TUriServices.defineFeatures(features: TFslList<TFHIRFeature>);
begin
end;

function TUriServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TUriServices.description: String;
begin
  result := 'URIs';
end;

function TUriServices.Display(context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string;
begin
  result := '';
end;

procedure TUriServices.Designations(context: TCodeSystemProviderContext; list: TConceptDesignations);
begin
end;

function TUriServices.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // Uri doesn't do abstract
end;

function TUriServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := true;
end;

function TUriServices.Link: TUriServices;
begin
  result := TUriServices(Inherited Link);
end;

function TUriServices.getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
begin
  result := TCodeSystemIteratorContext.Create(nil, 0); // no children
end;

function TUriServices.getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('TUriServices.getcontext');
end;

function TUriServices.locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.Create('locateIsA not supported by Uri', itNotSupported); // Uri doesn't have formal subsumption property, so this is not used
end;


function TUriServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'Internal URI services';
end;

function TUriServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  raise ETerminologyTodo.Create('TUriServices.prepare');
end;

function TUriServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.Create('TUriServices.searchFilter');
end;

function TUriServices.filter(forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.Create('TUriServices.filter');
end;

function TUriServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('TUriServices.filterLocate');
end;

function TUriServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  raise ETerminologyTodo.Create('TUriServices.FilterMore');
end;

function TUriServices.filterSize(ctxt: TCodeSystemProviderFilterContext): integer;
begin
  raise ETerminologyTodo.Create('TUriServices.FilterSize');
end;

function TUriServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('TUriServices.FilterConcept');
end;

function TUriServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyTodo.Create('TUriServices.InFilter');
end;

{ TUriHolder }

constructor TUriHolder.Create(url: String);
begin
  inherited Create;
  self.url := url;
end;

end.
