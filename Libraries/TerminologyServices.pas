unit TerminologyServices;

interface

uses
  SysUtils, Classes,
  StringSupport,
  AdvObjects, AdvStringLists,
  FHIRTypes, FHIRComponents, FHIRResources,
  YuStemmer;

Type
  TFhirFilterOperator = FHIRTypes.TFhirFilterOperator;

  TCodeSystemProviderContext = TAdvObject;
  TCodeSystemProviderFilterContext = TAdvObject;

  TSearchFilterText = class (TAdvObject)
  private
    FFilter: string;
    FStems : TStringList;
    FStemmer : TYuStemmer_8;

    procedure process;
  public
    constructor Create(filter : String);  overload;
    destructor Destroy; override;

    function null : Boolean;
    function passes(value : String) : boolean; overload;
    function passes(stems : TAdvStringList) : boolean; overload;
    property filter : string read FFilter;
  end;

  TCodeSystemProvider = {abstract} class (TAdvObject)
  public
    function TotalCount : integer;  virtual; abstract;
    function ChildCount(context : TCodeSystemProviderContext) : integer; virtual; abstract;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; virtual; abstract;
    function system : String; virtual; abstract;
    function getDisplay(code : String):String; virtual; abstract;
    function locate(code : String) : TCodeSystemProviderContext; virtual; abstract;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; virtual; abstract;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; virtual; abstract;
    function Code(context : TCodeSystemProviderContext) : string; virtual; abstract;
    function Display(context : TCodeSystemProviderContext) : string; virtual; abstract;
    procedure Displays(code : String; list : TStringList); virtual; abstract;
    function doesFilter(prop : String; op : TFhirFilterOperator; value : String) : boolean;

    function searchFilter(filter : TSearchFilterText) : TCodeSystemProviderFilterContext; virtual; abstract;
    function filter(prop : String; op : TFhirFilterOperator; value : String) : TCodeSystemProviderFilterContext; virtual; abstract;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; virtual; abstract;
    function FilterCount(ctxt : TCodeSystemProviderFilterContext) : integer; virtual; abstract;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext; ndx : integer): TCodeSystemProviderContext; virtual; abstract;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; virtual; abstract;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); virtual; abstract;
  end;

implementation

{ TCodeSystemProvider }

function TCodeSystemProvider.doesFilter(prop: String; op: TFhirFilterOperator; value: String): boolean;
var
  ctxt : TCodeSystemProviderFilterContext;
begin
  ctxt := filter(prop, op, value);
  result := ctxt <> nil;
  if result then
    Close(ctxt);
end;


{ TSearchFilterText }

constructor TSearchFilterText.create(filter: String);
begin
  Create;
  FStemmer := GetStemmer_8('english');
  FStems := TStringList.Create;
  FFilter := filter;
  process;
end;

destructor TSearchFilterText.destroy;
begin
  FStems.Free;
  FStemmer.Free;
  inherited;
end;

function TSearchFilterText.null: Boolean;
begin
  result := FStems.Count = 0;
end;

function TSearchFilterText.passes(value: String): boolean;
var
  s : String;
  i : integer;
begin
  result := Null;
  while not result and (value <> '') Do
  begin
    StringSplit(value, [',', ' ', ':', '.', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '{', '}', '[', ']', '|', '\', ';', '"', '<', '>', '?', '/', '~', '`', '-', '_', '+', '='], s, value);
    if (s <> '') Then
      result := FStems.Find(lowercase(FStemmer.stem(s)), i);
  End;
end;

function TSearchFilterText.passes(stems: TAdvStringList): boolean;
var
  i, j : integer;
begin
  result := Null;
  for i := 0 to stems.count - 1 do
    result := result or FStems.find(stems[i], j);
end;

procedure TSearchFilterText.process;
var
  s, t : String;
begin
  t := FFilter;
  while (t <> '') Do
  begin
    StringSplit(t, [',', ' ', ':', '.', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '{', '}', '[', ']', '|', '\', ';', '"', '<', '>', '?', '/', '~', '`', '-', '_', '+', '='], s, t);
    if (s <> '') Then
      FStems.Add(lowercase(FStemmer.stem(s)));
  End;
  FStems.Sort;
end;

end.
