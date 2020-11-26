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
  ftx_service;

type
  TUriHolder = class (TCodeSystemProviderContext)
  private
    url : String;
  public
    constructor Create(url : String);
  end;

  TUriServices = class (TCodeSystemProvider)
  public
    Function Link : TUriServices; overload;

    function description : String; override;

    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function systemUri(context : TCodeSystemProviderContext) : String; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
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

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
  end;

implementation

{ TUriServices }

function TUriServices.TotalCount : integer;
begin
  result := -1;
end;


function TUriServices.version(context: TCodeSystemProviderContext): String;
begin
  result := 'n/a';
end;

function TUriServices.systemUri(context : TCodeSystemProviderContext) : String;
begin
  result := 'urn:ietf:rfc:3986';
end;

function TUriServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TUriServices.getDisplay(code : String; const lang : THTTPLanguages):String;
begin
  result := '';
end;

function TUriServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  raise ETerminologyTodo.create('TUriServices.getPrepContext');
end;

procedure TUriServices.Displays(code : String; list : TStringList; const lang : THTTPLanguages);
begin
end;

function TUriServices.locate(code : String; var message : String) : TCodeSystemProviderContext;
begin
  result := TUriHolder.create(code);
end;


function TUriServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TUriHolder(context).url;
end;

function TUriServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TUriServices.description: String;
begin
  result := 'URIs';
end;

function TUriServices.Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string;
begin
  result := '';
end;

procedure TUriServices.Displays(context: TCodeSystemProviderContext; list: TStringList; const lang : THTTPLanguages);
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

function TUriServices.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  result := 0; // no children
end;

function TUriServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.create('TUriServices.getcontext');
end;

function TUriServices.locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('locateIsA not supported by Uri'); // Uri doesn't have formal subsumption property, so this is not used
end;


function TUriServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'Internal URI services';
end;

function TUriServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  raise ETerminologyTodo.create('TUriServices.prepare');
end;

function TUriServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.create('TUriServices.searchFilter');
end;

function TUriServices.filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.create('TUriServices.filter');
end;

function TUriServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.create('TUriServices.filterLocate');
end;

function TUriServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  raise ETerminologyTodo.create('TUriServices.FilterMore');
end;

function TUriServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.create('TUriServices.FilterConcept');
end;

function TUriServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyTodo.create('TUriServices.InFilter');
end;

procedure TUriServices.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.free;
end;

procedure TUriServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;

procedure TUriServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  raise ETerminologyTodo.create('TUriServices.Close');
end;

{ TUriHolder }

constructor TUriHolder.create(url: String);
begin
  inherited create;
  self.url := url;
end;

end.
