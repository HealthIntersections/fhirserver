unit CountryCodeServices;

interface

uses
  SysUtils, Classes,
  StringSupport,
  AdvObjects, AdvObjectLists, AdvFiles, AdvTextExtractors, AdvStringIntegerMatches,
  KDBManager,
  FHIRTypes, FHIRComponents, FHIRResources, TerminologyServices, DateAndTime;

type
  TCountryCodeConcept = class (TCodeSystemProviderContext)
  private
    FCode : string;
    FDisplay : String;
  end;

  TCountryCodeFilter = class (TCodeSystemProviderFilterContext)
  end;

  TCountryCodePrep = class (TCodeSystemProviderFilterPreparationContext)
  end;

  TCountryCodeServices = class (TCodeSystemProvider)
  public
    db : TKDBManager;

    Constructor Create(db : TKDBManager);
    Destructor Destroy; Override;
    Function Link : TCountryCodeServices; overload;

    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext) : string; override;
    procedure Displays(code : String; list : TStringList); override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList); override;
    function Definition(context : TCodeSystemProviderContext) : string; override;

    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;

    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
  end;

implementation

{ TCountryCodeServices }

Constructor TCountryCodeServices.create(db : TKDBManager);
begin
  inherited Create;

  self.db := db;
end;


function TCountryCodeServices.TotalCount : integer;
var
  qry : TKDBConnection;
begin
  qry := db.GetConnection('CountryCode.Count');
  try
    qry.SQL := 'Select Count(*) from CountryCode';
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
      raise;
    end;
  end;
end;


function TCountryCodeServices.system(context : TCodeSystemProviderContext) : String;
begin
  result := 'urn:iso:std:iso:3166';
end;

function TCountryCodeServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TCountryCodeServices.getDisplay(code : String):String;
var
  qry : TKDBConnection;
begin
  qry := db.GetConnection('CountryCode.display');
  try
    qry.SQL := 'Select Display from CountryCode where Code = :code';
    qry.prepare;
    qry.BindString('code', code);
    qry.execute;
    qry.FetchNext;
    result := qry.colString[1];
    qry.Terminate;
    qry.Release;
  except
    on e : Exception do
    begin
      qry.Error(e);
      raise;
    end;
  end;
end;

function TCountryCodeServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  raise Exception.Create('not done yet');
end;

procedure TCountryCodeServices.Displays(code : String; list : TStringList);
begin
  list.Add(getDisplay(code));
end;


function TCountryCodeServices.locate(code : String) : TCodeSystemProviderContext;
var
  qry : TKDBConnection;
  res : TCountryCodeConcept;
begin
  qry := db.GetConnection('CountryCode.locate');
  try
    qry.SQL := 'Select CountryCodeKey, Display from CountryCode where code = :code';
    qry.prepare;
    qry.bindString('code', code);
    qry.execute;
    if not qry.FetchNext then
      result := nil
    else
    begin
      res := TCountryCodeConcept.Create;
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
      raise;
    end;
  end;
end;


function TCountryCodeServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TCountryCodeConcept(context).FCode;
end;

function TCountryCodeServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

destructor TCountryCodeServices.Destroy;
begin
  db.Free;
  inherited;
end;

function TCountryCodeServices.Display(context : TCodeSystemProviderContext) : string;
begin
  result := TCountryCodeConcept(context).FDisplay;
end;

procedure TCountryCodeServices.Displays(context: TCodeSystemProviderContext; list: TStringList);
begin
  list.Add(Display(context));
end;

function TCountryCodeServices.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // CountryCode doesn't do abstract
end;

function TCountryCodeServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TCountryCodeServices.Link: TCountryCodeServices;
begin
  result := TCountryCodeServices(Inherited Link);
end;

function TCountryCodeServices.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  result := 0; // no children
end;

function TCountryCodeServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  raise Exception.Create('not done yet');
end;

function TCountryCodeServices.locateIsA(code, parent : String) : TCodeSystemProviderContext;
begin
  raise Exception.Create('locateIsA not supported by CountryCode'); // CountryCode doesn't have formal subsumption property, so this is not used
end;


function TCountryCodeServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  raise Exception.Create('not done yet');
end;

function TCountryCodeServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise Exception.Create('not done yet');
end;

function TCountryCodeServices.filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise Exception.Create('not done yet');
end;

function TCountryCodeServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext;
begin
  raise Exception.Create('not done yet');
end;

function TCountryCodeServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  raise Exception.Create('not done yet');
end;

function TCountryCodeServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise Exception.Create('not done yet');
end;

function TCountryCodeServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise Exception.Create('not done yet');
end;

procedure TCountryCodeServices.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.free;
end;

procedure TCountryCodeServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;

procedure TCountryCodeServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  raise Exception.Create('not done yet');
end;


end.
