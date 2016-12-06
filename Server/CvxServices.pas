unit CvxServices;

interface

// based on a table by importing the excel spreadsheet directly
uses
  SysUtils, Classes,
  StringSupport,
  AdvObjects, AdvObjectLists, AdvFiles, AdvTextExtractors, AdvStringIntegerMatches,  AdvExceptions,
  KDBManager,
  FHIRTypes, FHIRResources, TerminologyServices, DateAndTime;

type
  TCvxConcept = class (TCodeSystemProviderContext)
  private
    FCode : string;
    FDisplay : String;
    FOthers : TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TCvxFilter = class (TCodeSystemProviderFilterContext)
  private
  public
    Destructor Destroy; Override;
  end;

  TCvxPrep = class (TCodeSystemProviderFilterPreparationContext)
//  private
//    filters : TAdvObjectList;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  end;

  TCvxServices = class (TCodeSystemProvider)
  private
    db : TKDBManager;

  public
    Constructor Create(db : TKDBManager);
    Destructor Destroy; Override;
    Function Link : TCvxServices; overload;

    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system(context : TCodeSystemProviderContext) : String; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
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

    function subsumesTest(codeA, codeB : String) : String; override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
  end;

implementation

{ TCvxServices }

Constructor TCvxServices.create(db : TKDBManager);
begin
  inherited Create;

  self.db := db;
end;


function TCvxServices.TotalCount : integer;
var
  qry : TKDBConnection;
begin
  qry := db.GetConnection('Cvx.Count');
  try
    qry.SQL := 'Select Count(*) from Cvx';
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


function TCvxServices.version(context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

function TCvxServices.system(context : TCodeSystemProviderContext) : String;
begin
  result := 'http://hl7.org/fhir/sid/cvx';
end;

function TCvxServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TCvxServices.getDisplay(code : String; lang : String):String;
var
  qry : TKDBConnection;
begin
  qry := db.GetConnection('Cvx.display');
  try
    qry.SQL := 'Select [CVX Short Description] from Cvx where [CVX Code] = :code';
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
      recordStack(e);
      raise;
    end;
  end;
end;

function TCvxServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  raise ETerminologySetup.Create('not done yet');
end;

procedure TCvxServices.Displays(code : String; list : TStringList; lang : String);
begin
  list.Add(getDisplay(code, lang));
end;

function TCvxServices.locate(code : String) : TCodeSystemProviderContext;
var
  qry : TKDBConnection;
  res : TCvxConcept;
begin
  qry := db.GetConnection('Cvx.locate');
  try
    qry.SQL := 'Select [CVX Short Description], [Full Vaccine Name] from Cvx where [CVX Code] = :code';
    qry.prepare;
    qry.bindString('code', code);
    qry.execute;
    if not qry.FetchNext then
      result := nil
    else
    begin
      res := TCvxConcept.Create;
      try
        res.FCode := code;
        res.FDisplay := qry.ColStringByName['CVX Short Description'];
        res.FOthers.Add(qry.ColStringByName['Full Vaccine Name']);
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


function TCvxServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TCvxConcept(context).FCode;
end;

function TCvxServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

destructor TCvxServices.Destroy;
begin
  DB.Free;
  inherited;
end;

function TCvxServices.Display(context : TCodeSystemProviderContext; lang : String) : string;
begin
  result := TCvxConcept(context).FDisplay;
end;

procedure TCvxServices.Displays(context: TCodeSystemProviderContext; list: TStringList; lang : String);
begin
  list.Add(Display(context, lang));
  list.AddStrings(TCvxConcept(context).FOthers);
end;

function TCvxServices.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // Cvx doesn't do abstract
end;

function TCvxServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TCvxServices.Link: TCvxServices;
begin
  result := TCvxServices(Inherited Link);
end;

function TCvxServices.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  result := 0; // no children
end;

function TCvxServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  raise ETerminologySetup.Create('not done yet');
end;

function TCvxServices.locateIsA(code, parent : String) : TCodeSystemProviderContext;
begin
  raise ETerminologySetup.Create('locateIsA not supported by Cvx'); // Cvx doesn't have formal subsumption property, so this is not used
end;


function TCvxServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'CVX';
end;

function TCvxServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  raise ETerminologySetup.Create('not done yet');
end;

function TCvxServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologySetup.Create('not done yet');
end;

function TCvxServices.subsumesTest(codeA, codeB: String): String;
begin
  result := 'not-subsumed';
end;

function TCvxServices.filter(prop : String; op : TFhirFilterOperatorEnum; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologySetup.Create('not done yet');
end;

function TCvxServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext;
begin
  raise ETerminologySetup.Create('not done yet');
end;

function TCvxServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  raise ETerminologySetup.Create('not done yet');
end;

function TCvxServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise ETerminologySetup.Create('not done yet');
end;

function TCvxServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologySetup.Create('not done yet');
end;

procedure TCvxServices.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.free;
end;

procedure TCvxServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;

procedure TCvxServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  raise ETerminologySetup.Create('not done yet');
end;

{ TCvxPrep }

constructor TCvxPrep.Create;
begin
  inherited;
end;

destructor TCvxPrep.Destroy;
begin
  inherited;
end;

{ TCvxFilter }

destructor TCvxFilter.Destroy;
begin
  inherited;
end;

{ TCvxConcept }

constructor TCvxConcept.create;
begin
  inherited;
  FOthers := TStringList.Create;
end;

destructor TCvxConcept.destroy;
begin
  FOthers.free;
  inherited;
end;

end.
