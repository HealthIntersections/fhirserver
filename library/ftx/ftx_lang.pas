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
  fsl_utilities, fsl_stream, fsl_base, fsl_http, fsl_lang, fsl_threads,
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
  private
    FSupplements : TFslList<TFhirCodeSystemW>;
  public
    destructor Destroy; override;

    Function Link : TIETFLanguageCodeServices; overload;

    class function checkFile(sourceFile : String) : String;
    function description : String; override;

    function cloneWithSupplements(supplements : TFslList<TFhirCodeSystemW>) : TCodeSystemProvider; override;
    function hasSupplement(opContext : TTxOperationContext; url : String) : boolean; override;

    function TotalCount : integer;  override;
    function getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function systemUri : String; override;
    function version : String; override;
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

const
  CODES_TIETFLanguageComponent : array [TIETFLanguageComponent] of String = ('language', 'ext-lang', 'script', 'region', 'variant', 'extension', 'private-use');

implementation


{ TIETFLanguageCodeServices }

procedure TIETFLanguageCodeServices.defineFeatures(opContext : TTxOperationContext; features: TFslList<TFHIRFeature>);
var
  s : string;
begin
  for s in CODES_TIETFLanguageComponent do
    features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', s+':exists'));
end;
function TIETFLanguageCodeServices.TotalCount : integer;
begin
  result := -1;   // not bounded
end;


function TIETFLanguageCodeServices.version: String;
begin
  result := '';
end;

function TIETFLanguageCodeServices.systemUri : String;
begin
  result := URI_BCP47;
end;

function TIETFLanguageCodeServices.getDefinition(opContext : TTxOperationContext; code: String): String;
begin
  result := '';
end;

function TIETFLanguageCodeServices.getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String;
var
  c : TIETFLang;
  msg : String;
  supp : TFhirCodeSystemW;
  defn : TFhirCodeSystemConceptW;
  ccd : TFhirCodeSystemConceptDesignationW;
  function useDisp(use : TFHIRCodingW; lang  : String) : boolean;
  begin
    result := true;
    if (use <> nil) then
    begin
      result := true;
      use.free;
    end;
    if (result) then
      result := langList.matches(lang, false);
  end;
begin
  if (code = '') then
    result := '??'
  else
  begin
    if (FSupplements <> nil) then
    begin
      for supp in FSupplements do
      begin
        defn := supp.getCode(code);
        if (defn <> nil) then
          if (useDisp(nil, supp.language)) then
            exit(defn.display);
          for ccd in defn.designations.forEnum do
            if (useDisp(ccd.use, ccd.language)) then
              exit(ccd.value);    
          for ccd in defn.designations.forEnum do
            if (useDisp(nil, ccd.language)) then
              exit(ccd.value);
      end;
    end;
    c := FLanguages.parse(code, msg);
    try
      if c <> nil then
        result := FLanguages.present(c).Trim
      else
        result := '??';
    finally
      c.free;
    end;
  end;
end;

function TIETFLanguageCodeServices.getPrepContext(opContext : TTxOperationContext): TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

procedure TIETFLanguageCodeServices.Designations(opContext : TTxOperationContext; context: TCodeSystemProviderContext; list: TConceptDesignations);
var
  c : TIETFLanguageCodeConcept;
  msg : String;
  i : integer;        
  supp : TFhirCodeSystemW;  
  defn : TFhirCodeSystemConceptW;
  ccd : TFhirCodeSystemConceptDesignationW;
begin
  if (context <> nil) then
  begin
    c := context as TIETFLanguageCodeConcept;
    if (c.FInfo <> nil) then
    begin
      list.addDesignation(true, true, '', '', FLanguages.present(c.FInfo).Trim);
      if (c.FInfo.isLangRegion) then
      begin
        list.addDesignation(false, true, '', '', FLanguages.present(c.FInfo, 0, '{{lang}} ({{region}})').Trim);
        list.addDesignation(false, true, '', '', FLanguages.present(c.FInfo, 0, '{{lang}} (Region={{region}})').Trim);
      end;
      for i := 0 to FLanguages.displayCount(c.FInfo) - 1 do
      begin
        list.addDesignation(false, true, '', '', FLanguages.present(c.FInfo, i).Trim);
        if (c.FInfo.isLangRegion) then
        begin
          list.addDesignation(false, true, '', '', FLanguages.present(c.FInfo, i, '{{lang}} ({{region}})').Trim);
          list.addDesignation(false, true, '', '', FLanguages.present(c.FInfo, i, '{{lang}} (Region={{region}})').Trim);
        end;
      end;
    end;
    if (FSupplements <> nil) then
    begin
      for supp in FSupplements do
      begin
        defn := supp.getCode(c.FInfo.code);
        if (defn <> nil) then
          for ccd in defn.designations.forEnum do
            list.addDesignation(ccd);
      end;
    end;
  end;
end;


function TIETFLanguageCodeServices.locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext;
var
  info : TIETFLang;
begin
  info := FLanguages.parse(code, message);
  if info = nil then
    result := nil
  else
    result := TIETFLanguageCodeConcept.Create(info);
end;


function TIETFLanguageCodeServices.Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string;
begin
  result := TIETFLanguageCodeConcept(context).FInfo.code;
end;

function TIETFLanguageCodeServices.Definition(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TIETFLanguageCodeServices.description: String;
begin
  result := 'IETF language codes';
end;

function TIETFLanguageCodeServices.cloneWithSupplements(supplements: TFslList<TFhirCodeSystemW>): TCodeSystemProvider;
begin
  Result := TIETFLanguageCodeServices.create(FLanguages.link, FI18n.link);
  (result as TIETFLanguageCodeServices).FSupplements := supplements.link;
end;

function TIETFLanguageCodeServices.hasSupplement(opContext: TTxOperationContext; url: String): boolean;
var
  cs : TFHIRCodeSystemW;
begin
  result := false;
  for cs in FSupplements do
    if (cs.vurl = url) or (cs.url = url) then
      exit(true);
end;


function TIETFLanguageCodeServices.Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string;
var
  ctxt : TIETFLanguageCodeConcept;
begin
  ctxt := TIETFLanguageCodeConcept(context);
  if (ctxt.FInfo = nil) then
    result := ''
  else
    result := getDisplay(opContext, ctxt.FInfo.code, langList);
end;

function TIETFLanguageCodeServices.IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // IETFLanguageCode doesn't do abstract
end;

function TIETFLanguageCodeServices.isNotClosed(opContext : TTxOperationContext; textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := true;
end;

destructor TIETFLanguageCodeServices.Destroy;
begin
  FSupplements.free;
  inherited Destroy;
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

function TIETFLanguageCodeServices.getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
begin
  result := TCodeSystemIteratorContext.Create(nil, 0);
end;

function TIETFLanguageCodeServices.getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.create('TIETFLanguageCodeServices.getcontext');
end;

function TIETFLanguageCodeServices.locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  result := nil; // no subsumption
end;


function TIETFLanguageCodeServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'IETF langauge';
end;

function TIETFLanguageCodeServices.prepare(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  result := false;
end;

function TIETFLanguageCodeServices.searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.create('TIETFLanguageCodeServices.searchFilter');
end;

function TIETFLanguageCodeServices.filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
var
  i : integer;
begin
  SetThreadStatus(ClassName+'.filter('+prop+CODES_TFhirFilterOperator[op]+value+')');
  i := StringArrayIndexOfSensitive(CODES_TIETFLanguageComponent, prop);
  if (i >= 0) and (op = foExists) and ((value = 'true') or (value = 'false')) then
    result := TIETFLanguageCodeFilter.Create(TIETFLanguageComponent(i), value = 'true')
  else
    raise ETerminologyError.Create('Not a supported filter', itInvalid);
end;

function TIETFLanguageCodeServices.filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
var
  cc : TIETFLanguageCodeConcept;
  filter : TIETFLanguageCodeFilter;
  ok : boolean;
  l : TIETFLang;
begin
  result := nil;
  l := FLanguages.parse(code, message);
  if (l <> nil) then
  begin
    try
      filter := TIETFLanguageCodeFilter(ctxt);
      case filter.component of
        languageComponentLang: ok := filter.status = (l.language <> '');
        languageComponentExtLang: ok := filter.status = (length(l.extLang) > 0);
        languageComponentScript: ok := filter.status = (l.script <> '');
        languageComponentRegion: ok := filter.status = (l.region <> '');
        languageComponentVariant: ok := filter.status = (l.variant <> '');
        languageComponentExtension: ok := filter.status = (l.extension <> '');
        languageComponentPrivateUse: ok := filter.status = (length(l.privateUse) > 0);
      else
        ok := false;
      end;
      if ok then
        result := TIETFLanguageCodeConcept.Create(l.link)
      else if filter.status then
        message := 'The language code '+code+' does not contain a '+CODES_TIETFLanguageComponent[filter.component]+', and it is required to'
      else
        message := 'The language code '+code+' contains a '+CODES_TIETFLanguageComponent[filter.component]+', and it is not allowed to';
    finally
      l.free;
    end;
  end;
end;

function TIETFLanguageCodeServices.FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  raise ETerminologyError.create('Language valuesets cannot be expanded as they are based on a grammar', itNotSupported);
end;

function TIETFLanguageCodeServices.filterSize(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): integer;
begin
  raise ETerminologyError.create('Language valuesets cannot be expanded as they are based on a grammar', itNotSupported);
end;

function TIETFLanguageCodeServices.FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.create('TIETFLanguageCodeServices.FilterConcept');
end;

function TIETFLanguageCodeServices.InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyTodo.create('TIETFLanguageCodeServices.InFilter');
end;

{ TIETFLanguageCodeFilter }

constructor TIETFLanguageCodeFilter.create(component: TIETFLanguageComponent; status: boolean);
begin
  inherited Create;
  FComponent := component;
  FStatus := status;
end;


{ TIETFLanguageCodeConcept }

constructor TIETFLanguageCodeConcept.Create(info: TIETFLang);
begin
  inherited Create;
  FInfo := info;
end;

destructor TIETFLanguageCodeConcept.Destroy;
begin
  FInfo.free;
  inherited;
end;

function TIETFLanguageCodeConcept.Link: TIETFLanguageCodeConcept;
begin
  result := TIETFLanguageCodeConcept(inherited link);
end;

end.
