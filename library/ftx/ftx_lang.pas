unit ftx_lang;

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
  fsl_utilities, fsl_stream, fsl_base, fsl_http, fsl_lang,
  fhir_objects, fhir_common, fhir_features, fhir_uris,
  ftx_service;

type
  TIETFLanguageCodeConcept = class (TCodeSystemProviderContext)
  private
    FInfo : TIETFLang;
  public
    constructor Create(info : TIETFLang);
    destructor Destroy; override;

    function Link : TIETFLanguageCodeConcept; overload;
  end;


  { TIETFLanguageDefinitions }

  TIETFLanguageComponent = (languageComponentLang, languageComponentExtLang, languageComponentScript, languageComponentRegion, languageComponentVariant, languageComponentExtension, languageComponentPrivateUse);

  TIETFLanguageCodeFilter = class (TCodeSystemProviderFilterContext)
  private
    FComponent : TIETFLanguageComponent;
    FStatus : boolean;
  public
    constructor Create(component : TIETFLanguageComponent; status : boolean);
    property component : TIETFLanguageComponent read FComponent write FComponent;
    property status : boolean read FStatus write FStatus;
  end;

  TIETFLanguageCodePrep = class (TCodeSystemProviderFilterPreparationContext)
  end;

  { TIETFLanguageCodeServices }

  TIETFLanguageCodeServices = class (TCodeSystemProvider)
  public
    Function Link : TIETFLanguageCodeServices; overload;

    class function checkFile(sourceFile : String) : String;
    function description : String; override;

    function TotalCount : integer;  override;
    function getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function systemUri(context : TCodeSystemProviderContext) : String; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; const lang : THTTPLanguages):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string; override;
    procedure Designations(context : TCodeSystemProviderContext; list : TConceptDesignations); override;
    function Definition(context : TCodeSystemProviderContext) : string; override;

    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;

    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
    procedure defineFeatures(features : TFslList<TFHIRFeature>); override;
  end;

const
  CODES_TIETFLanguageComponent : array [TIETFLanguageComponent] of String = ('language', 'ext-lang', 'script', 'region', 'variant', 'extension', 'private-use');

implementation


{ TIETFLanguageCodeServices }

procedure TIETFLanguageCodeServices.defineFeatures(features: TFslList<TFHIRFeature>);
var
  s : string;
begin
  for s in CODES_TIETFLanguageComponent do
    features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri(nil)+'.filter', s+':exists'));
end;
function TIETFLanguageCodeServices.TotalCount : integer;
begin
  result := -1;   // not bounded
end;


function TIETFLanguageCodeServices.version(context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

function TIETFLanguageCodeServices.systemUri(context : TCodeSystemProviderContext) : String;
begin
  result := URI_BCP47;
end;

function TIETFLanguageCodeServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TIETFLanguageCodeServices.getDisplay(code : String; const lang : THTTPLanguages):String;
var
  c : TIETFLang;
  msg : String;
begin
  if (code = '') then
    result := '??'
  else
  begin
    c := FLanguages.parse(code, msg);
    try
      if c <> nil then
        result := FLanguages.present(c).Trim
      else
        result := '??';
    finally
      c.Free;
    end;
  end;
end;

function TIETFLanguageCodeServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

procedure TIETFLanguageCodeServices.Designations(context: TCodeSystemProviderContext; list: TConceptDesignations);
var
  c : TIETFLanguageCodeConcept;
  msg : String;
begin
  if (context <> nil) then
  begin
    c := context as TIETFLanguageCodeConcept;
    list.addBase('', FLanguages.present(c.FInfo).Trim);
    if c.FInfo.isLangRegion then
      list.addDesignation('', FLanguages.present(c.FInfo, '{{lang}} ({{region}})').Trim);
  end;
end;


function TIETFLanguageCodeServices.locate(code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext;
begin
  result := TIETFLanguageCodeConcept.Create(FLanguages.parse(code, message));
end;


function TIETFLanguageCodeServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TIETFLanguageCodeConcept(context).FInfo.code;
end;

function TIETFLanguageCodeServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TIETFLanguageCodeServices.description: String;
begin
  result := 'IETF language codes';
end;
function TIETFLanguageCodeServices.Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string;
begin
  result := getDisplay(TIETFLanguageCodeConcept(context).FInfo.code, lang);
end;

function TIETFLanguageCodeServices.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // IETFLanguageCode doesn't do abstract
end;

function TIETFLanguageCodeServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := true;
end;

function TIETFLanguageCodeServices.Link: TIETFLanguageCodeServices;
begin
  result := TIETFLanguageCodeServices(Inherited Link);
end;

class function TIETFLanguageCodeServices.checkFile(sourceFile: String): String;
begin
  try
    result := TIETFLanguageDefinitions.checkSource(FileToString(sourceFile, TEncoding.ASCII));
  except
    on e : Exception do
      result := 'Error: '+e.message;
  end;
end;

function TIETFLanguageCodeServices.getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
begin
  result := TCodeSystemIteratorContext.Create(nil, 0);
end;

function TIETFLanguageCodeServices.getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.create('TIETFLanguageCodeServices.getcontext');
end;

function TIETFLanguageCodeServices.locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  result := nil; // no subsumption
end;


function TIETFLanguageCodeServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'IETF langauge';
end;

function TIETFLanguageCodeServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  result := false;
end;

function TIETFLanguageCodeServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.create('TIETFLanguageCodeServices.searchFilter');
end;

function TIETFLanguageCodeServices.filter(forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
var
  i : integer;
begin
  i := StringArrayIndexOfSensitive(CODES_TIETFLanguageComponent, prop);
  if (i >= 0) and (op = foExists) and ((value = 'true') or (value = 'false')) then
    result := TIETFLanguageCodeFilter.Create(TIETFLanguageComponent(i), value = 'true')
  else
    raise ETerminologyError.Create('Not a supported filter', itInvalid);
end;

function TIETFLanguageCodeServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
var
  cc : TIETFLanguageCodeConcept;
  filter : TIETFLanguageCodeFilter;
  ok : boolean;
begin
  result := nil;
  cc := TIETFLanguageCodeConcept.Create(FLanguages.parse(code, message));
  try
    filter := TIETFLanguageCodeFilter(ctxt);
    ok := false;
    if cc <> nil then
    begin
      case filter.component of
        languageComponentLang: ok := filter.status = (cc.FInfo.language <> '');
        languageComponentExtLang: ok := filter.status = (length(cc.FInfo.extLang) > 0);
        languageComponentScript: ok := filter.status = (cc.FInfo.script <> '');
        languageComponentRegion: ok := filter.status = (cc.FInfo.region <> '');
        languageComponentVariant: ok := filter.status = (cc.FInfo.variant <> '');
        languageComponentExtension: ok := filter.status = (cc.FInfo.extension <> '');
        languageComponentPrivateUse: ok := filter.status = (length(cc.FInfo.privateUse) > 0);
      end;
    end;
    if ok then
      result := cc.Link
    else if filter.status then
      message := 'The language code '+code+' does not contain a '+CODES_TIETFLanguageComponent[filter.component]+', and it is required to'
    else
      message := 'The language code '+code+' contains a '+CODES_TIETFLanguageComponent[filter.component]+', and it is not allowed to';
  finally
    cc.free;
  end;
end;

function TIETFLanguageCodeServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  raise ETerminologyError.create('Language valuesets cannot be expanded as they are based on a grammar', itNotSupported);
end;

function TIETFLanguageCodeServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.create('TIETFLanguageCodeServices.FilterConcept');
end;

function TIETFLanguageCodeServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyTodo.create('TIETFLanguageCodeServices.InFilter');
end;

procedure TIETFLanguageCodeServices.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.free;
end;

procedure TIETFLanguageCodeServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;

procedure TIETFLanguageCodeServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  ctxt.free;
end;

{ TIETFLanguageCodeFilter }

constructor TIETFLanguageCodeFilter.create(component: TIETFLanguageComponent; status: boolean);
begin
  inherited create;
  FComponent := component;
  FStatus := status;
end;


{ TIETFLanguageCodeConcept }

constructor TIETFLanguageCodeConcept.Create(info: TIETFLang);
begin
  inherited create;
  FInfo := info;
end;

destructor TIETFLanguageCodeConcept.Destroy;
begin
  FInfo.Free;
  inherited;
end;

function TIETFLanguageCodeConcept.Link: TIETFLanguageCodeConcept;
begin
  result := TIETFLanguageCodeConcept(inherited link);
end;

end.
