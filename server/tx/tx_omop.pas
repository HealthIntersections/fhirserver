unit tx_omop;

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

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_utilities, fsl_http, fsl_threads, fsl_lang, fsl_i18n,
  fdb_manager, fdb_dialects,
  fhir_objects, fhir_common, fhir_factory, fhir_utilities, fhir_features, fhir_uris,
  fhir_cdshooks,
  ftx_service;

type

  { TOMOPConcept }

  TOMOPConcept = class (TCodeSystemProviderContext)
  private
    FCode : String;
    FDisplay : String;
    FDomain: String;
  public
    property Code : String read FCode write FCode;
    property Display : String read FDisplay write FDisplay;
    property domain : String read FDomain write FDomain;
  end;

  { TOMOPFilter }

  TOMOPFilter = class (TCodeSystemProviderFilterContext)
  private
    FConn : TFDBConnection;
    procedure SetConn(AValue: TFDBConnection);
  public
    destructor Destroy; override;

    property conn : TFDBConnection read FConn write SetConn;
  end;

  { TOMOPServices }

  TOMOPServices = class (TCodeSystemProvider)
  private
    db : TFDBManager;
    FVersion : String;
    procedure loadVersion;
  public
    constructor Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport; db : TFDBManager);
    destructor Destroy; Override;
    Function Link : TOMOPServices; overload;

    class function checkDB(conn : TFDBConnection) : String;

    function systemUri : String; override;
    function version : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function description : String; override;
    function TotalCount : integer;  override;
    function getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String; override;
    function getDefinition(opContext : TTxOperationContext; code : String):String; override;
    function locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function sameContext(opContext : TTxOperationContext; a, b : TCodeSystemProviderContext) : boolean; override;
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
    procedure getCDSInfo(opContext : TTxOperationContext; card : TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display : String); override;
    procedure extendLookup(opContext : TTxOperationContext; factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; langList : THTTPLanguageList; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;
    //function subsumes(codeA, codeB : String) : String; override;

    procedure defineFeatures(opContext : TTxOperationContext; features : TFslList<TFHIRFeature>); override;
  end;


implementation

{ TOMOPFilter }

procedure TOMOPFilter.SetConn(AValue: TFDBConnection);
begin
  FConn.free;
  FConn:=AValue;
end;

destructor TOMOPFilter.Destroy;
begin
  FConn.terminate;
  FConn.release;
  inherited Destroy;
end;

{ TOMOPServices }

constructor TOMOPServices.Create(languages: TIETFLanguageDefinitions; i18n : TI18nSupport; db: TFDBManager);
begin
  inherited Create(languages, i18n);
  self.db := db;
  loadVersion;
end;

destructor TOMOPServices.Destroy;
begin
  db.free;
  inherited Destroy;
end;

function TOMOPServices.Link: TOMOPServices;
begin
  result := TOMOPServices(inherited link);
end;

class function TOMOPServices.checkDB(conn: TFDBConnection): String;
var
  meta : TFDBMetaData;
begin
  meta := conn.FetchMetaData;
  try
    if not meta.HasTable('Relationships') or not meta.HasTable('Domains') or not meta.HasTable('ConceptClasses') or not meta.HasTable('Vocabularies')
        or not meta.HasTable('Concepts') or not meta.HasTable('ConceptSynonyms') or not meta.HasTable('ConceptRelationships') then
      result := 'Missing Tables - needs re-importing (by java)'
    else
      result := 'OK ('+inttostr(conn.countSql('Select count(*) from Concepts'))+' Concepts)';
  finally
    meta.free;
  end;
end;

procedure TOMOPServices.loadVersion;
var
  ver : String;
  conn : TFDBConnection;
begin
  conn := db.GetConnection('Version');
  try
    ver := conn.Lookup('Vocabularies', 'vocabulary_id', 'OMOP Extension', 'vocabulary_version', '');
    FVersion := ver.Substring(ver.LastIndexOf(' ')+1);
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
    end;
  end;

end;

function TOMOPServices.systemUri: String;
begin
  result := 'http://fhir.ohdsi.org/CodeSystem/concepts';
end;

function TOMOPServices.version: String;
begin
  Result := FVersion;
end;

function TOMOPServices.name(context: TCodeSystemProviderContext): String;
begin
  Result := 'OMOP Concepts';
end;

function TOMOPServices.description: String;
begin
  Result := 'OMOP Concepts, release '+FVersion;
end;

function TOMOPServices.TotalCount: integer;
begin
  result := db.countSql('Select count(*) from Concepts', 'TotalCount');
end;

function TOMOPServices.getDisplay(opContext : TTxOperationContext; code: String; langList : THTTPLanguageList): String;
var
  c : TOMOPConcept;
  msg : String;
begin
  c := locate(opContext, code, nil, msg) as TOMOPConcept;
  try
    if c <> nil then
      result := c.Display
    else
      result := '';
  finally
    c.free;
  end;
end;

function TOMOPServices.getDefinition(opContext : TTxOperationContext; code: String): String;
begin
  result := '';
end;

function TOMOPServices.locate(opContext : TTxOperationContext; code: String; altOpt : TAlternateCodeOptions; var message: String): TCodeSystemProviderContext;
var
  conn : TFDBConnection;
  c : TOMOPConcept;
begin
  conn := db.GetConnection('locate');
  try
    conn.sql := 'Select concept_name, domain_id from Concepts where concept_id = '''+SQLWrapString(code)+'''';
    conn.Prepare;
    conn.Execute;
    if conn.FetchNext then
    begin
      c := TOMOPConcept.Create;
      try
        c.code := code;
        c.display := conn.ColStringByName['concept_name'];
        c.domain := conn.ColStringByName['domain_id'];
        result := c.link;
      finally
        c.free;
      end;
    end
    else
      result := nil;
    conn.terminate;
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      raise
    end;
  end;
end;

function TOMOPServices.locateIsA(opContext : TTxOperationContext; code, parent: String; disallowParent: boolean): TCodeSystemProviderContext;
begin
  result := nil; // none
end;

function TOMOPServices.sameContext(opContext : TTxOperationContext; a, b: TCodeSystemProviderContext): boolean;
begin
  result := (a is TOMOPConcept) and (b is TOMOPConcept) and ((a as TOMOPConcept).code = (b as TOMOPConcept).code);
end;

function TOMOPServices.getIterator(opContext : TTxOperationContext; context: TCodeSystemProviderContext): TCodeSystemIteratorContext;
var
  qry : TFDBConnection;
begin
  qry := db.GetConnection('getIterator');
  try
    result := TCodeSystemIteratorContext.Create(nil, qry.CountSQL('Select count(concept_id) from Concepts'));
    qry.Release;
  except
    on e : Exception do
    begin
      qry.Error(e);
      recordStack(e);
      raise;
    end;
  end;
end;

function TOMOPServices.getNextContext(opContext : TTxOperationContext; context: TCodeSystemIteratorContext): TCodeSystemProviderContext;
begin
  raise ETerminologyError.Create('getNextContext not supported by RXNorm', itException); // only used when iterating the entire code system. and RxNorm is too big
end;

function TOMOPServices.IsAbstract(opContext : TTxOperationContext; context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function TOMOPServices.Code(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  if (context is TOMOPConcept) then
    result := (context as TOMOPConcept).code
  else
    result := '';
end;

function TOMOPServices.Display(opContext : TTxOperationContext; context: TCodeSystemProviderContext; langList : THTTPLanguageList): string;
begin
  if (context is TOMOPConcept) then
    result := (context as TOMOPConcept).display
  else
    result := '';
end;

procedure TOMOPServices.Designations(opContext : TTxOperationContext; context: TCodeSystemProviderContext; list: TConceptDesignations);
var
  conn : TFDBConnection;
begin
  if (context is TOMOPConcept) then
  begin
    list.addDesignation(true, true, 'en', (context as TOMOPConcept).Display);
    conn := db.GetConnection('display');
    try
      conn.sql := 'Select concept_synonym_name from ConceptSynonyms where concept_id = '''+SQLWrapString((context as TOMOPConcept).code)+'''';
      conn.Prepare;
      conn.Execute;
      while conn.FetchNext do
        list.addDesignation(false, false, 'en', conn.ColStringByName['concept_synonym_name']);
      conn.terminate;
      conn.Release;
    except
      on e : Exception do
      begin
        conn.Error(e);
        raise
      end;
    end;
  end;
end;

function TOMOPServices.Definition(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TOMOPServices.getPrepContext(opContext : TTxOperationContext): TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

function TOMOPServices.prepare(opContext : TTxOperationContext; prep: TCodeSystemProviderFilterPreparationContext): boolean;
begin
  result := false;
end;

function TOMOPServices.searchFilter(opContext : TTxOperationContext; filter: TSearchFilterText; prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.Create('not done yet: searchFilter', itBusinessRule);
end;

function TOMOPServices.filter(opContext : TTxOperationContext; forExpansion, forIteration: boolean; prop: String; op: TFhirFilterOperator; value: String; prep: TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
var
  f : TOMOPFilter;
begin
  if (prop = 'domain') and (op = foEqual) then
  begin
    f := TOMOPFilter.Create;
    try
      f.conn := db.GetConnection('filter');
      f.conn.sql := 'Select concept_id, concept_name, domain_id from Concepts where domain_id = '''+SQLWrapString(value)+'''';
      f.conn.Prepare;
      f.conn.Execute;
      result := f.link;
    finally
      f.free;
    end;
  end
  else
    raise ETerminologyError.Create('filter "'+prop+' '+CODES_TFhirFilterOperator[op]+' '+value+'" not understood for OMOP', itBusinessRule);
end;

function TOMOPServices.filterLocate(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext; code: String; var message: String): TCodeSystemProviderContext;
begin
  raise ETerminologyError.Create('not done yet: filterLocate', itBusinessRule);
end;

function TOMOPServices.FilterMore(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): boolean;
begin
  result := (ctxt as TOMOPFilter).Conn.FetchNext;
end;

function TOMOPServices.filterSize(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): integer;
begin
  result := (ctxt as TOMOPFilter).Conn.RowsAffected;
end;

function TOMOPServices.FilterConcept(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
var
  conn : TFDBConnection;
  c : TOMOPConcept;
begin
  conn := (ctxt as TOMOPFilter).Conn;
  c := TOMOPConcept.Create;
  try
    c.code := conn.ColStringByName['concept_id'];
    c.display := conn.ColStringByName['concept_name'];
    c.domain := conn.ColStringByName['domain_id'];
    result := c.link;
  finally
    c.free;
  end;
end;

function TOMOPServices.InFilter(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext; concept: TCodeSystemProviderContext): Boolean;
begin
  raise ETerminologyError.Create('not done yet: InFilter', itBusinessRule);
end;

function TOMOPServices.isNotClosed(opContext : TTxOperationContext; textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

procedure TOMOPServices.getCDSInfo(opContext : TTxOperationContext; card: TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display: String);
begin
  raise ETerminologyError.Create('not done yet: getCDSInfo', itBusinessRule);
end;

procedure TOMOPServices.extendLookup(opContext : TTxOperationContext; factory: TFHIRFactory; ctxt: TCodeSystemProviderContext; langList : THTTPLanguageList; props: TArray<String>; resp: TFHIRLookupOpResponseW);
var
  conn : TFDBConnection;
begin
  if hasProp(props, 'domain', true) then
    resp.addProp('domain').value := factory.makeCode((ctxt as TOMOPConcept).domain);
  conn := db.GetConnection('display');
  try
    conn.sql := 'Select concept_synonym_name from ConceptSynonyms where concept_id = '''+SQLWrapString((ctxt as TOMOPConcept).code)+'''';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do    
     resp.addDesignation('en', '', '', '', conn.ColStringByName['concept_synonym_name']);
    conn.terminate;
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      raise
    end;
  end;
end;

procedure TOMOPServices.defineFeatures(opContext : TTxOperationContext; features: TFslList<TFHIRFeature>);
begin
  raise ETerminologyError.Create('not done yet: defineFeatures', itBusinessRule);
end;

end.

