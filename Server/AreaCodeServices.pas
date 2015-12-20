unit AreaCodeServices;

interface

uses
  SysUtils, Classes,
  StringSupport,
  AdvObjects, AdvObjectLists, AdvFiles, AdvTextExtractors, AdvStringMatches, AdvExceptions,
  KDBManager,
  FHIRTypes, FHIRResources, TerminologyServices, DateAndTime;

type
  TAreaCodeConcept = class (TCodeSystemProviderContext);

  TAreaCodeServices = class (TCodeSystemProvider)
  public
    FCodes : TAdvStringMatch;

    Constructor Create(db : TKDBManager);
    Destructor Destroy; Override;
    Function Link : TAreaCodeServices; overload;

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

{ TAreaCodeServices }

Constructor TAreaCodeServices.create(db : TKDBManager);
var
  qry : TKDBConnection;
begin
  inherited Create;
  FCodes := TAdvStringMatch.Create;

  qry := db.GetConnection('AreaCodes');
  try
    qry.SQL := 'Select Code, Display from AreaCodes';
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
      recordStack(e);
      raise;
    end;
  end;
end;


function TAreaCodeServices.TotalCount : integer;
begin
  result := FCodes.Count;
end;


function TAreaCodeServices.system(context : TCodeSystemProviderContext) : String;
begin
  result := 'http://unstats.un.org/unsd/methods/m49/m49.htm';
end;

function TAreaCodeServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TAreaCodeServices.getDisplay(code : String):String;
begin
  result := FCodes[code];
end;

function TAreaCodeServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  raise Exception.Create('not done yet');
end;

procedure TAreaCodeServices.Displays(code : String; list : TStringList);
begin
  list.Add(getDisplay(code));
end;


function TAreaCodeServices.locate(code : String) : TCodeSystemProviderContext;
begin
  result := TCodeSystemProviderContext(FCodes.IndexByKey(code));
end;


function TAreaCodeServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := FCodes.KeyByIndex[integer(context)-1];
end;

function TAreaCodeServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := Display(context);
end;

destructor TAreaCodeServices.Destroy;
begin
  FCodes.Free;
  inherited;
end;

function TAreaCodeServices.Display(context : TCodeSystemProviderContext) : string;
begin
  result := FCodes.ValueByIndex[integer(context)-1];
end;

procedure TAreaCodeServices.Displays(context: TCodeSystemProviderContext; list: TStringList);
begin
  list.Add(Display(context));
end;

function TAreaCodeServices.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // AreaCode doesn't do abstract
end;

function TAreaCodeServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TAreaCodeServices.Link: TAreaCodeServices;
begin
  result := TAreaCodeServices(Inherited Link);
end;

function TAreaCodeServices.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  if (context = nil) then
    result := TotalCount
  else
    result := 0; // no children
end;

function TAreaCodeServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  result := TAreaCodeConcept(ndx+1);
end;

function TAreaCodeServices.locateIsA(code, parent : String) : TCodeSystemProviderContext;
begin
  raise Exception.Create('locateIsA not supported by AreaCode'); // AreaCode doesn't have formal subsumption property, so this is not used
end;


function TAreaCodeServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  raise Exception.Create('not done yet');
end;

function TAreaCodeServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise Exception.Create('not done yet');
end;

function TAreaCodeServices.filter(prop : String; op : TFhirFilterOperatorEnum; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise Exception.Create('not done yet');
end;

function TAreaCodeServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext;
begin
  raise Exception.Create('not done yet');
end;

function TAreaCodeServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  raise Exception.Create('not done yet');
end;

function TAreaCodeServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise Exception.Create('not done yet');
end;

function TAreaCodeServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise Exception.Create('not done yet');
end;

procedure TAreaCodeServices.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.free;
end;

procedure TAreaCodeServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;

procedure TAreaCodeServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  raise Exception.Create('not done yet');
end;


end.
