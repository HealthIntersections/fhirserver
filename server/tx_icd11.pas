unit tx_icd11;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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
  fsl_base, fsl_http, fsl_lang, fsl_utilities,
  fhir_objects, fhir_common, fhir_factory, fhir_features, fhir_cdshooks,
  ftx_service;


type
{ TICD11Provider }

  TICD11Provider = class (TCodeSystemProvider)
  private
//    FUrl : String;
//    FVersion : String;
//    FIsDefault : boolean;
//    FRoots : TFslList<TICD11Node>;
//    FCodes : TFslList<TICD11Node>;
//    FLanguage : String;
//    FTitle : String;
//    FStack : Array[0..5] of TICD11Node;
//    procedure readHeader(s : String);
//    procedure readLine(s : String);
//    procedure load(filename : String);
//    procedure countDescendents(node : TICD11Node);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(languages : TIETFLanguageDefinitions);
    destructor Destroy; override;
    function link : TICD11Provider; overload;

//    Property Title : String read FTitle;

    function description : String; override;
    function TotalCount : integer;  override;
    function getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
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
    procedure Displays(context : TCodeSystemProviderContext; list : TCodeDisplays); overload; override;
    function doesFilter(prop : String; op : TFhirFilterOperator; value : String) : boolean; override;

    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function specialFilter(prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
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
    procedure defineFeatures(features : TFslList<TFHIRFeature>); override;
  end;

implementation

{ TICD11Provider }

constructor TICD11Provider.Create(languages: TIETFLanguageDefinitions);
begin
  inherited create(languages);
end;

destructor TICD11Provider.Destroy;
begin

  inherited;
end;

procedure TICD11Provider.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.free;
end;

procedure TICD11Provider.Close(ctxt: TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;

procedure TICD11Provider.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  ctxt.free;
end;

function TICD11Provider.Code(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

procedure TICD11Provider.defineFeatures(features: TFslList<TFHIRFeature>);
begin
  // nothing
end;

function TICD11Provider.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TICD11Provider.defToThisVersion(specifiedVersion: String): boolean;
begin
  result := false;
end;

function TICD11Provider.description: String;
begin
  result := '';
end;

function TICD11Provider.Display(context: TCodeSystemProviderContext; const lang: THTTPLanguages): string;
begin
  result := '';
end;

procedure TICD11Provider.Displays(context: TCodeSystemProviderContext; list: TCodeDisplays);
begin
end;

function TICD11Provider.doesFilter(prop: String; op: TFhirFilterOperator; value: String): boolean;
begin
  result := false;
end;

procedure TICD11Provider.extendLookup(factory: TFHIRFactory; ctxt: TCodeSystemProviderContext; const lang: THTTPLanguages; props: TArray<String>; resp: TFHIRLookupOpResponseW);
begin
end;

function TICD11Provider.filter(forIteration: boolean; prop: String; op: TFhirFilterOperator; value: String; prep: TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
begin
  result := nil;
end;

function TICD11Provider.FilterConcept(ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  result := nil;
end;

function TICD11Provider.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String; var message: String): TCodeSystemProviderContext;
begin
  result := nil;
end;

function TICD11Provider.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String): TCodeSystemProviderContext;
begin
  result := nil;
end;

function TICD11Provider.FilterMore(ctxt: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

procedure TICD11Provider.getCDSInfo(card: TCDSHookCard; const lang: THTTPLanguages; baseURL, code, display: String);
begin
end;

function TICD11Provider.getDefinition(code: String): String;
begin
  result := '';
end;

function TICD11Provider.getDisplay(code: String; const lang: THTTPLanguages): String;
begin
  result := '';
end;

function TICD11Provider.getIterator(context: TCodeSystemProviderContext): TCodeSystemIteratorContext;
begin
  result := nil;
end;

function TICD11Provider.getNextContext(context: TCodeSystemIteratorContext): TCodeSystemProviderContext;
begin
  result := nil;
end;

function TICD11Provider.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

function TICD11Provider.InFilter(ctxt: TCodeSystemProviderFilterContext; concept: TCodeSystemProviderContext): Boolean;
begin
  result := false;
end;

function TICD11Provider.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function TICD11Provider.IsInactive(context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function TICD11Provider.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TICD11Provider.link: TICD11Provider;
begin
  result := TICD11Provider(inherited Link);
end;

function TICD11Provider.locate(code: String): TCodeSystemProviderContext;
begin
  result := nil;
end;

function TICD11Provider.locate(code: String; var message: String): TCodeSystemProviderContext;
begin
  result := nil;
end;

function TICD11Provider.locateIsA(code, parent: String; disallowParent: boolean): TCodeSystemProviderContext;
begin
  result := nil;
end;

function TICD11Provider.name(context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

function TICD11Provider.prepare(prep: TCodeSystemProviderFilterPreparationContext): boolean;
begin
  result := false;
end;

function TICD11Provider.searchFilter(filter: TSearchFilterText; prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  result := nil;
end;

function TICD11Provider.sizeInBytesV(magic: integer): cardinal;
begin
  result := inherited sizeInBytesV(magic);
end;

function TICD11Provider.SpecialEnumeration: String;
begin
  result := '';
end;

function TICD11Provider.specialFilter(prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  result := nil;
end;

function TICD11Provider.subsumesTest(codeA, codeB: String): String;
begin
  result := '';
end;

function TICD11Provider.systemUri(context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

function TICD11Provider.TotalCount: integer;
begin
  result := 0;
end;

function TICD11Provider.version(context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

end.
