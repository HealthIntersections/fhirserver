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
    function getDisplay(code : String; const lang : THTTPLanguages):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function sameContext(a, b : TCodeSystemProviderContext) : boolean; override;
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
    procedure getCDSInfo(card : TCDSHookCard; const lang : THTTPLanguages; baseURL, code, display : String); override;
    procedure extendLookup(factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; const lang : THTTPLanguages; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;
    //function subsumes(codeA, codeB : String) : String; override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
    procedure defineFeatures(features : TFslList<TFHIRFeature>); override;
  end;


implementation

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
  result := db.countSql('TotalCount', 'Select count(*) from Concepts');
end;

function TOMOPServices.getIterator(context: TCodeSystemProviderContext): TCodeSystemIteratorContext;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.getNextContext(context: TCodeSystemIteratorContext): TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.getDisplay(code: String; const lang: THTTPLanguages): String;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.getDefinition(code: String): String;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.locate(code: String; altOpt : TAlternateCodeOptions; var message: String): TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.locateIsA(code, parent: String; disallowParent: boolean): TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.sameContext(a, b: TCodeSystemProviderContext): boolean;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.Code(context: TCodeSystemProviderContext): string;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.Display(context: TCodeSystemProviderContext; const lang: THTTPLanguages): string;
begin
  raise ETerminologyError.create('not done yet');
end;

procedure TOMOPServices.Designations(context: TCodeSystemProviderContext; list: TConceptDesignations);
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.Definition(context: TCodeSystemProviderContext): string;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.prepare(prep: TCodeSystemProviderFilterPreparationContext): boolean;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.searchFilter(filter: TSearchFilterText; prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.filter(forIteration: boolean; prop: String; op: TFhirFilterOperator; value: String; prep: TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String; var message: String): TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.FilterMore(ctxt: TCodeSystemProviderFilterContext): boolean;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.FilterConcept(ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.InFilter(ctxt: TCodeSystemProviderFilterContext; concept: TCodeSystemProviderContext): Boolean;
begin
  raise ETerminologyError.create('not done yet');
end;

function TOMOPServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  raise ETerminologyError.create('not done yet');
end;

procedure TOMOPServices.getCDSInfo(card: TCDSHookCard; const lang: THTTPLanguages; baseURL, code, display: String);
begin
  raise ETerminologyError.create('not done yet');
end;

procedure TOMOPServices.extendLookup(factory: TFHIRFactory; ctxt: TCodeSystemProviderContext; const lang: THTTPLanguages; props: TArray<String>; resp: TFHIRLookupOpResponseW);
begin
  raise ETerminologyError.create('not done yet');
end;

procedure TOMOPServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  raise ETerminologyError.create('not done yet');
end;

procedure TOMOPServices.Close(ctxt: TCodeSystemProviderContext);
begin
  raise ETerminologyError.create('not done yet');
end;

procedure TOMOPServices.Close(ctxt: TCodeSystemProviderFilterContext);
begin
  raise ETerminologyError.create('not done yet');
end;

procedure TOMOPServices.defineFeatures(features: TFslList<TFHIRFeature>);
begin
  raise ETerminologyError.create('not done yet');
end;

end.

