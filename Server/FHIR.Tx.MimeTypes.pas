unit FHIR.Tx.MimeTypes;

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


interface

uses
  SysUtils, Classes, System.Generics.Collections,
   FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.Base, FHIR.Web.Parsers,
  FHIR.Base.Common,
  FHIR.Version.Types, FHIR.Version.Resources, FHIR.Tx.Service;

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


  TMimeTypeCodeServices = class (TCodeSystemProvider)
  public
    Constructor Create; override;
    Destructor Destroy; Override;
    Function Link : TMimeTypeCodeServices; overload;

    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system(context : TCodeSystemProviderContext) : String; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; lang : String):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; lang : String) : string; override;
    procedure Displays(code : String; list : TStringList; lang : String); override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList; lang : String); override;
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

{ TMimeTypeCodeServices }

Constructor TMimeTypeCodeServices.create;
begin
  inherited Create;
end;


function TMimeTypeCodeServices.TotalCount : integer;
begin
  result := -1;   // not bounded
end;


function TMimeTypeCodeServices.version(context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

function TMimeTypeCodeServices.system(context : TCodeSystemProviderContext) : String;
begin
  result := 'urn:ietf:bcp:13';
end;

function TMimeTypeCodeServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TMimeTypeCodeServices.getDisplay(code : String; lang : String):String;
begin
  result := code;
end;

function TMimeTypeCodeServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

procedure TMimeTypeCodeServices.Displays(code : String; list : TStringList; lang : String);
begin
  list.Add(getDisplay(code, lang));
end;


function TMimeTypeCodeServices.locate(code : String; var message : String) : TCodeSystemProviderContext;
var
  mt : TMimeContentType;
begin
  mt := TMimeContentType.parseSingle(code);
  try
    if mt.isValid and (mt.sub <> '') then
      result := TMTCodeSystemProviderContext.Create(mt.Link)
    else
      result := nil;
  finally
    mt.Free;
  end;
end;

function TMimeTypeCodeServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TMimeContentType(context).source;
end;

function TMimeTypeCodeServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

destructor TMimeTypeCodeServices.Destroy;
begin
  inherited;
end;

function TMimeTypeCodeServices.Display(context : TCodeSystemProviderContext; lang : String) : string;
begin
  result := getDisplay(TMTCodeSystemProviderContext(context).mt.source, lang);
end;

procedure TMimeTypeCodeServices.Displays(context: TCodeSystemProviderContext; list: TStringList; lang : String);
begin
  list.Add(Display(context, lang));
end;

function TMimeTypeCodeServices.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // MimeTypeCode doesn't do abstract
end;

function TMimeTypeCodeServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := true;
end;

function TMimeTypeCodeServices.Link: TMimeTypeCodeServices;
begin
  result := TMimeTypeCodeServices(Inherited Link);
end;

function TMimeTypeCodeServices.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  result := -1;
end;

function TMimeTypeCodeServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('not done yet');
end;

function TMimeTypeCodeServices.locateIsA(code, parent : String) : TCodeSystemProviderContext;
begin
  result := nil; // no subsumption
end;


function TMimeTypeCodeServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'IETF langauge';
end;

function TMimeTypeCodeServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  result := false;
end;

function TMimeTypeCodeServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.create('not done yet');
end;

function TMimeTypeCodeServices.filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.Create('Not a supported filter');
end;

function TMimeTypeCodeServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
begin
  result := nil;
end;

function TMimeTypeCodeServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  raise ETerminologyError.create('not done yet');
end;

function TMimeTypeCodeServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('not done yet');
end;

function TMimeTypeCodeServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyError.create('not done yet');
end;

procedure TMimeTypeCodeServices.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.free;
end;

procedure TMimeTypeCodeServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;

procedure TMimeTypeCodeServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  ctxt.free;
end;


{ TMTCodeSystemProviderContext }

constructor TMTCodeSystemProviderContext.Create(mt: TMimeContentType);
begin
  inherited Create;
  FMt := mt;
end;

destructor TMTCodeSystemProviderContext.Destroy;
begin
  FMt.Free;
  inherited;
end;

procedure TMTCodeSystemProviderContext.SetMt(const Value: TMimeContentType);
begin
  FMt.Free;
  FMt := Value;
end;

end.
