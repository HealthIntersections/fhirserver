unit CountryCodeServices;

interface

uses
  SysUtils, Classes,
  StringSupport,
  AdvObjects, AdvObjectLists, AdvFiles, AdvTextExtractors, AdvStringMatches,
  KDBManager,
  FHIRTypes, FHIRResources, TerminologyServices, DateAndTime;

type
  TCountryCodeConcept = class (TCodeSystemProviderContext);

  TCountryCodeServices = class (TCodeSystemProvider)
  public
    FCodes : TAdvStringMatch;

    Constructor Create(db : TKDBManager);
    Destructor Destroy; Override;
    Function Link : TCountryCodeServices; overload;

    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; lang : String):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String) : TCodeSystemProviderContext; override;
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
var
  qry : TKDBConnection;
begin
  inherited Create;
  FCodes := TAdvStringMatch.Create;

  qry := db.GetConnection('CountryCodes');
  try
    qry.SQL := 'Select Code, Display from CountryCodes';
    qry.prepare;
    qry.execute;
    while qry.FetchNext do
      Fcodes.Add(qry.ColStringByName['Code'], qry.ColStringByName['Display']);
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


function TCountryCodeServices.TotalCount : integer;
begin
  result := FCodes.Count;
end;


function TCountryCodeServices.system(context : TCodeSystemProviderContext) : String;
begin
  result := 'urn:iso:std:iso:3166';
end;

function TCountryCodeServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TCountryCodeServices.getDisplay(code : String; lang : String):String;
begin
  result := FCodes[code];
end;

function TCountryCodeServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  raise Exception.Create('not done yet');
end;

procedure TCountryCodeServices.Displays(code : String; list : TStringList; lang : String);
begin
  list.Add(getDisplay(code, lang));
end;


function TCountryCodeServices.locate(code : String) : TCodeSystemProviderContext;
begin
  result := TCodeSystemProviderContext(FCodes.IndexByKey(code));
end;


function TCountryCodeServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := FCodes.KeyByIndex[integer(context)-1];
end;

function TCountryCodeServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := Display(context,'');
end;

destructor TCountryCodeServices.Destroy;
begin
  FCodes.Free;
  inherited;
end;

function TCountryCodeServices.Display(context : TCodeSystemProviderContext; lang : String) : string;
begin
  result := FCodes.ValueByIndex[integer(context)-1];
end;

procedure TCountryCodeServices.Displays(context: TCodeSystemProviderContext; list: TStringList; lang : String);
begin
  list.Add(Display(context, ''));
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
  if (context = nil) then
    result := TotalCount
  else
    result := 0; // no children
end;

function TCountryCodeServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  result := TCountryCodeConcept(ndx+1);
end;

function TCountryCodeServices.locateIsA(code, parent : String) : TCodeSystemProviderContext;
begin
  result := nil; // CountryCode doesn't have formal subsumption property, so this is not used
end;


function TCountryCodeServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  raise Exception.Create('not done yet');
end;

function TCountryCodeServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise Exception.Create('not done yet');
end;

function TCountryCodeServices.filter(prop : String; op : TFhirFilterOperatorEnum; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
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
  // nothing
end;

procedure TCountryCodeServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  // nothing
end;

procedure TCountryCodeServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  raise Exception.Create('not done yet');
end;


end.
