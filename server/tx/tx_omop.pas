unit tx_omop;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_utilities, fsl_http, fsl_threads, fsl_lang,
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
    constructor Create(languages : TIETFLanguageDefinitions; db : TFDBManager);
    destructor Destroy; Override;
    Function Link : TOMOPServices; overload;

    class function checkDB(conn : TFDBConnection) : String;

    function systemUri(context : TCodeSystemProviderContext) : String; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function description : String; override;
    function TotalCount : integer;  override;
    function getIterator(context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function getDisplay(code : String; langList : THTTPLanguageList):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function sameContext(a, b : TCodeSystemProviderContext) : boolean; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string; override;
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
    procedure getCDSInfo(card : TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display : String); override;
    procedure extendLookup(factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; langList : THTTPLanguageList; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;
    //function subsumes(codeA, codeB : String) : String; override;

    procedure defineFeatures(features : TFslList<TFHIRFeature>); override;
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

constructor TOMOPServices.Create(languages: TIETFLanguageDefinitions; db: TFDBManager);
begin
  inherited Create(languages);
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

function TOMOPServices.systemUri(context: TCodeSystemProviderContext): String;
begin
  result := 'http://fhir.ohdsi.org/CodeSystem/concepts';
end;

function TOMOPServices.version(context: TCodeSystemProviderContext): String;
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

function TOMOPServices.getDisplay(code: String; langList : THTTPLanguageList): String;
var
  c : TOMOPConcept;
  msg : String;
begin
  c := locate(code, nil, msg) as TOMOPConcept;
  try
    if c <> nil then
      result := c.Display
    else
      result := '';
  finally
    c.free;
  end;
end;

function TOMOPServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TOMOPServices.locate(code: String; altOpt : TAlternateCodeOptions; var message: String): TCodeSystemProviderContext;
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

function TOMOPServices.locateIsA(code, parent: String; disallowParent: boolean): TCodeSystemProviderContext;
begin
  result := nil; // none
end;

function TOMOPServices.sameContext(a, b: TCodeSystemProviderContext): boolean;
begin
  result := (a is TOMOPConcept) and (b is TOMOPConcept) and ((a as TOMOPConcept).code = (b as TOMOPConcept).code);
end;

function TOMOPServices.getIterator(context: TCodeSystemProviderContext): TCodeSystemIteratorContext;
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

function TOMOPServices.getNextContext(context: TCodeSystemIteratorContext): TCodeSystemProviderContext;
begin
  raise ETerminologyError.Create('getNextContext not supported by RXNorm', itException); // only used when iterating the entire code system. and RxNorm is too big
end;

function TOMOPServices.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function TOMOPServices.Code(context: TCodeSystemProviderContext): string;
begin
  if (context is TOMOPConcept) then
    result := (context as TOMOPConcept).code
  else
    result := '';
end;

function TOMOPServices.Display(context: TCodeSystemProviderContext; langList : THTTPLanguageList): string;
begin
  if (context is TOMOPConcept) then
    result := (context as TOMOPConcept).display
  else
    result := '';
end;

procedure TOMOPServices.Designations(context: TCodeSystemProviderContext; list: TConceptDesignations);
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

function TOMOPServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TOMOPServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

function TOMOPServices.prepare(prep: TCodeSystemProviderFilterPreparationContext): boolean;
begin
  result := false;
end;

function TOMOPServices.searchFilter(filter: TSearchFilterText; prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.Create('not done yet: searchFilter');
end;

function TOMOPServices.filter(forIteration: boolean; prop: String; op: TFhirFilterOperator; value: String; prep: TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
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
    raise ETerminologyError.Create('filter "'+prop+' '+CODES_TFhirFilterOperator[op]+' '+value+'" not understood for OMOP');
end;

function TOMOPServices.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String; var message: String): TCodeSystemProviderContext;
begin
  raise ETerminologyError.Create('not done yet: filterLocate');
end;

function TOMOPServices.FilterMore(ctxt: TCodeSystemProviderFilterContext): boolean;
begin
  result := (ctxt as TOMOPFilter).Conn.FetchNext;
end;

function TOMOPServices.FilterConcept(ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
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

function TOMOPServices.InFilter(ctxt: TCodeSystemProviderFilterContext; concept: TCodeSystemProviderContext): Boolean;
begin
  raise ETerminologyError.Create('not done yet: InFilter');
end;

function TOMOPServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

procedure TOMOPServices.getCDSInfo(card: TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display: String);
begin
  raise ETerminologyError.Create('not done yet: getCDSInfo');
end;

procedure TOMOPServices.extendLookup(factory: TFHIRFactory; ctxt: TCodeSystemProviderContext; langList : THTTPLanguageList; props: TArray<String>; resp: TFHIRLookupOpResponseW);
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

procedure TOMOPServices.defineFeatures(features: TFslList<TFHIRFeature>);
begin
  raise ETerminologyError.Create('not done yet: defineFeatures');
end;

end.

