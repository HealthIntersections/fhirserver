unit USStateCodeServices;

interface

uses
  SysUtils, Classes,
  StringSupport,
  AdvObjects, AdvObjectLists, AdvFiles, AdvTextExtractors, AdvStringIntegerMatches, AdvExceptions,
  KDBManager,
  FHIRTypes, FHIRResources, TerminologyServices, DateAndTime;

type
  TUSStateCodeConcept = class (TCodeSystemProviderContext)
  private
    FCode : string;
    FDisplay : String;
  end;

  TUSStateCodeFilter = class (TCodeSystemProviderFilterContext)
  end;

  TUSStateCodePrep = class (TCodeSystemProviderFilterPreparationContext)
  end;

  TUSStateCodeServices = class (TCodeSystemProvider)
  public
    db : TKDBManager;

    Constructor Create(db : TKDBManager);
    Destructor Destroy; Override;
    Function Link : TUSStateCodeServices; overload;

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
    function filter(prop : String; op : TFhirFilterOperatorEnum; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    function subsumesTest(codeA, codeB : String) : String; override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
  end;

implementation

{ TUSStateCodeServices }

Constructor TUSStateCodeServices.create(db : TKDBManager);
begin
  inherited Create;

  self.db := db;
end;


function TUSStateCodeServices.TotalCount : integer;
var
  qry : TKDBConnection;
begin
  qry := db.GetConnection('USStateCode.Count');
  try
    qry.SQL := 'Select Count(*) from USStateCodes';
    qry.prepare;
    qry.execute;
    qry.FetchNext;
    result := qry.ColInteger[1];
    qry.Terminate;
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


function TUSStateCodeServices.version(context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

function TUSStateCodeServices.system(context : TCodeSystemProviderContext) : String;
begin
  result := 'https://www.usps.com/';
end;

function TUSStateCodeServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TUSStateCodeServices.getDisplay(code : String; lang : String):String;
var
  qry : TKDBConnection;
begin
  qry := db.GetConnection('USStateCode.display');
  try
    qry.SQL := 'Select Display from USStateCodes where Code = '''+sqlwrapString(code)+'''';
    qry.prepare;
    qry.execute;
    qry.FetchNext;
    result := qry.colString[1];
    qry.Terminate;
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

function TUSStateCodeServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  raise Exception.Create('not done yet');
end;

procedure TUSStateCodeServices.Displays(code : String; list : TStringList; lang : String);
begin
  list.Add(getDisplay(code, lang));
end;


function TUSStateCodeServices.locate(code : String; var message : String) : TCodeSystemProviderContext;
var
  qry : TKDBConnection;
  res : TUSStateCodeConcept;
begin
  qry := db.GetConnection('USStateCode.locate');
  try
    qry.SQL := 'Select USStateCodeKey, Display from USStateCodes where code = :code';
    qry.prepare;
    qry.bindString('code', code);
    qry.execute;
    if not qry.FetchNext then
      result := nil
    else
    begin
      res := TUSStateCodeConcept.Create;
      try
        res.FCode := code;
        res.FDisplay := qry.ColStringByName['Display'];
        result := res.Link;
      finally
        res.Free;
      end;
    end;
    qry.Terminate;
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


function TUSStateCodeServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TUSStateCodeConcept(context).FCode;
end;

function TUSStateCodeServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

destructor TUSStateCodeServices.Destroy;
begin
  db.Free;
  inherited;
end;

function TUSStateCodeServices.Display(context : TCodeSystemProviderContext; lang : String) : string;
begin
  result := TUSStateCodeConcept(context).FDisplay;
end;

procedure TUSStateCodeServices.Displays(context: TCodeSystemProviderContext; list: TStringList; lang : String);
begin
  list.Add(Display(context, lang));
end;

function TUSStateCodeServices.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // USStateCode doesn't do abstract
end;

function TUSStateCodeServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TUSStateCodeServices.Link: TUSStateCodeServices;
begin
  result := TUSStateCodeServices(Inherited Link);
end;

function TUSStateCodeServices.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  result := 0; // no children
end;

function TUSStateCodeServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  raise Exception.Create('not done yet');
end;

function TUSStateCodeServices.locateIsA(code, parent : String) : TCodeSystemProviderContext;
begin
  raise Exception.Create('locateIsA not supported by USStateCode'); // USStateCode doesn't have formal subsumption property, so this is not used
end;


function TUSStateCodeServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'United States Postal Codes';
end;

function TUSStateCodeServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  raise Exception.Create('not done yet');
end;

function TUSStateCodeServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise Exception.Create('not done yet');
end;

function TUSStateCodeServices.subsumesTest(codeA, codeB: String): String;
begin
  result := 'not-subsumed';
end;

function TUSStateCodeServices.filter(prop : String; op : TFhirFilterOperatorEnum; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise Exception.Create('not done yet');
end;

function TUSStateCodeServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
begin
  raise Exception.Create('not done yet');
end;

function TUSStateCodeServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  raise Exception.Create('not done yet');
end;

function TUSStateCodeServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise Exception.Create('not done yet');
end;

function TUSStateCodeServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise Exception.Create('not done yet');
end;

procedure TUSStateCodeServices.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.free;
end;

procedure TUSStateCodeServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;

procedure TUSStateCodeServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  raise Exception.Create('not done yet');
end;


end.
