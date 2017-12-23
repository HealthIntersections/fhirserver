unit ICD10Services;

interface

uses
  SysUtils, Classes, Generics.Collections,
  AdvObjects, AdvGenerics,
  FHIRTypes, FHIRResources, FHIROperations,
  CDSHooksUtilities,
  TerminologyServices;

type
  TICD10Node = class (TCodeSystemProviderContext)
  private
    FCode : String;
    FDisplay : String;
    FLocalDisplay : String;
    FDescendentCount : integer;

    FChildren : TAdvList<TICD10Node>;
    function hasChildren : boolean;
    procedure addChild(child : TICD10Node);
  public
    constructor Create(code : String);
    destructor Destroy; override;
    function Link : TICD10Node; overload;
  end;

  TICD10Provider = class (TCodeSystemProvider)
  private
    FUrl : String;
    FVersion : String;
    FIsDefault : boolean;
    FRoots : TAdvList<TICD10Node>;
    FCodes : TAdvList<TICD10Node>;
    FLanguage : String;
    FTitle : String;
    FStack : Array[0..5] of TICD10Node;
    procedure readHeader(s : String);
    procedure readLine(s : String);
    procedure load(filename : String);
    procedure countDescendents(node : TICD10Node);
  public
    constructor Create(isDefault : boolean; filename : String);
    destructor Destroy; override;

    Property Title : String read FTitle;

    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system(context : TCodeSystemProviderContext) : String; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; lang : String):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; overload; override;
    function locate(code : String) : TCodeSystemProviderContext; overload; override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function IsInactive(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; lang : String) : string; override;
    function Definition(context : TCodeSystemProviderContext) : string; override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList; lang : String); overload; override;
    procedure Displays(code : String; list : TStringList; lang : String); overload; override;
    function doesFilter(prop : String; op : TFhirFilterOperatorEnum; value : String) : boolean; override;

    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function specialFilter(prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(prop : String; op : TFhirFilterOperatorEnum; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; override; // true if the underlying provider collapsed multiple filters
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; overload; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; overload; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure extendLookup(ctxt : TCodeSystemProviderContext; lang : String; props : TList<String>; resp : TFHIRLookupOpResponse); override;
    function subsumesTest(codeA, codeB : String) : String; override;

    function SpecialEnumeration : String; override;
    procedure getCDSInfo(card : TCDSHookCard; lang, baseURL, code, display : String); override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); overload; override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); overload; override;
    procedure Close(ctxt : TCodeSystemProviderContext); overload; override;
    function defToThisVersion(specifiedVersion : String) : boolean; override;

  end;

implementation

{ TICD10Node }

constructor TICD10Node.Create(code: String);
begin
  inherited Create;
  FCode := code;
end;

destructor TICD10Node.Destroy;
begin
  FChildren.Free;
  inherited;
end;

procedure TICD10Node.addChild(child: TICD10Node);
begin
  if FChildren = nil then
    FChildren := TAdvList<TICD10Node>.create;
  FChildren.add(child.link);
end;

function TICD10Node.hasChildren: boolean;
begin
  result := FChildren <> nil;
end;

function TICD10Node.Link: TICD10Node;
begin
  result := TICD10Node(inherited link);
end;

{ TICD10Provider }

constructor TICD10Provider.Create(isDefault : boolean; filename: String);
begin
  inherited create;
  FRoots := TAdvList<TICD10Node>.create;
  FCodes := TAdvList<TICD10Node>.create;
  FIsDefault := isDefault;
  load(filename);
end;

destructor TICD10Provider.Destroy;
begin
  FRoots.Free;
  FCodes.Free;
  inherited;
end;

function TICD10Provider.ChildCount(context: TCodeSystemProviderContext): integer;
begin
  if context = nil then
    result := TotalCount
  else if not TICD10Node(context).hasChildren then
    result := 0
  else
    result := TICD10Node(context).FChildren.Count;
end;

procedure TICD10Provider.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  // nothing
end;

procedure TICD10Provider.Close(ctxt: TCodeSystemProviderFilterContext);
begin
  // nothing
end;

procedure TICD10Provider.Close(ctxt: TCodeSystemProviderContext);
begin
  // nothing
end;

function TICD10Provider.Code(context: TCodeSystemProviderContext): string;
begin
  result := TICD10Node(context).FCode;
end;

procedure TICD10Provider.countDescendents(node: TICD10Node);
var
  n : TICD10Node;
  t : integer;
begin
  if node.hasChildren then
  begin
    t := node.FChildren.Count;
    for n in node.FChildren do
    begin
      countDescendents(n);
      inc(t, n.FDescendentCount);
    end
  end
  else
    t := 0;
  node.FDescendentCount := t;
end;

function TICD10Provider.Definition(context: TCodeSystemProviderContext): string;
begin
  result := TICD10Node(context).FDisplay;
end;

function TICD10Provider.defToThisVersion(specifiedVersion: String): boolean;
begin
  result := FIsDefault;
end;

function TICD10Provider.Display(context: TCodeSystemProviderContext; lang: String): string;
begin
  if (lang = '') or lang.startsWith(FLanguage) then
    result := TICD10Node(context).FLocalDisplay
  else
    result := TICD10Node(context).FDisplay
end;

procedure TICD10Provider.Displays(code: String; list: TStringList; lang: String);
var
  context : TCodeSystemProviderContext;
begin
  context := locate(code);
  if context <> nil then
    Displays(context, list, lang);
end;

procedure TICD10Provider.Displays(context: TCodeSystemProviderContext; list: TStringList; lang: String);
begin
  if (lang = '') or lang.startsWith(FLanguage) then
    list.add(TICD10Node(context).FLocalDisplay)
  else
    list.add(TICD10Node(context).FDisplay)
end;

function TICD10Provider.doesFilter(prop: String; op: TFhirFilterOperatorEnum; value: String): boolean;
begin
  result := false;
end;

procedure TICD10Provider.extendLookup(ctxt: TCodeSystemProviderContext; lang: String; props: TList<String>; resp: TFHIRLookupOpResponse);
{$IFNDEF FHIR2}
var
  p : TFHIRLookupOpRespProperty_;
{$ENDIF}
begin
{$IFNDEF FHIR2}
  resp.version := FVersion;
  if TICD10Node(ctxt).FDisplay <> '' then
  begin
    resp.designationList.Add(TFHIRLookupOpRespDesignation.Create);
    resp.designationList[0].language := 'en';
    resp.designationList[0].value := TICD10Node(ctxt).FDisplay;
  end;
  p := TFHIRLookupOpRespProperty_.Create;
  resp.property_List.Add(p);
  p.code := 'descendents';
  p.value := inttostr(TICD10Node(ctxt).FDescendentCount);
{$ENDIF}
end;

function TICD10Provider.filter(prop: String; op: TFhirFilterOperatorEnum; value: String; prep: TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
begin
  raise Exception.Create('Not implemented');
end;

function TICD10Provider.FilterConcept(ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise Exception.Create('Not implemented');
end;

function TICD10Provider.filterLocate(ctxt: TCodeSystemProviderFilterContext;code: String): TCodeSystemProviderContext;
begin
  raise Exception.Create('Not implemented');
end;

function TICD10Provider.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String; var message: String): TCodeSystemProviderContext;
begin
  raise Exception.Create('Not implemented');
end;

function TICD10Provider.FilterMore(ctxt: TCodeSystemProviderFilterContext): boolean;
begin
  raise Exception.Create('Not implemented');
end;

procedure TICD10Provider.getCDSInfo(card: TCDSHookCard; lang, baseURL, code, display: String);
begin
  raise Exception.Create('Not implemented');
end;

function TICD10Provider.getcontext(context: TCodeSystemProviderContext; ndx: integer): TCodeSystemProviderContext;
begin
  if context = nil then
    result := FRoots[ndx]
  else
    result := TICD10Node(context).FChildren[ndx];
end;

function TICD10Provider.getDefinition(code: String): String;
begin
  result := getDisplay(code, '');
end;

function TICD10Provider.getDisplay(code, lang: String): String;
var
  context : TCodeSystemProviderContext;
begin
  context := locate(code);
  if context <> nil then
    result := Display(context, lang);
end;

function TICD10Provider.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

function TICD10Provider.InFilter(ctxt: TCodeSystemProviderFilterContext; concept: TCodeSystemProviderContext): Boolean;
begin
  raise Exception.Create('Not implemented');
end;

function TICD10Provider.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  result := TICD10Node(context).hasChildren;
end;

function TICD10Provider.IsInactive(context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function TICD10Provider.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  raise Exception.Create('Not implemented');
end;

function TICD10Provider.locate(code: String; var message: String): TCodeSystemProviderContext;
var
  c : TICD10Node;
begin
  result := nil;
  for c in FCodes do
    if c.FCode = code then
      exit(c);
  message := 'Code "'+code+'" not found';
end;

procedure TICD10Provider.load(filename: String);
var
  st : TStringList;
  i : integer;
  node : TICD10Node;
begin
  for i := 0 to 4 do
    FStack[i] := nil;

  st := TStringList.Create;
  try
    st.LoadFromFile(filename, TEncoding.UTF8);
    readHeader(st[0]);
    for i := 1 to st.Count - 1 do
      readLine(st[i]);
  finally
    st.free;
  end;
  for node in FRoots do
    countDescendents(node);
end;

function TICD10Provider.locate(code: String): TCodeSystemProviderContext;
var
  msg : string;
begin
  result := locate(code, msg);
end;

function TICD10Provider.locateIsA(code,parent: String): TCodeSystemProviderContext;
begin
  raise Exception.Create('Not implemented');
end;

function TICD10Provider.name(context: TCodeSystemProviderContext): String;
begin
  result := FTitle;
end;

function TICD10Provider.prepare(prep: TCodeSystemProviderFilterPreparationContext): boolean;
begin
  raise Exception.Create('Not implemented');
end;

procedure TICD10Provider.readHeader(s: String);
var
  cells : TArray<String>;
begin
  cells := s.Split([#9]);
  if cells[0] <> 'icd-10' then
    raise Exception.Create('Invalid file - should start with icd-10');
  FUrl := cells[1];
  FLanguage := cells[2];
  FTitle := 'ICD-10 '+cells[3];
end;

procedure TICD10Provider.readLine(s: String);
var
  i : integer;
  cells : TArray<String>;
  node : TICD10Node;
begin
  i := 0;
  while s[i+1] = ' ' do
    inc(i);
  cells := s.Substring(i).Split([#9]);
  node := TICD10Node.Create(cells[0]);
  try
    FCodes.Add(node.Link);
    if i = 0 then
      FRoots.add(node.link)
    else
      FStack[i-1].addChild(node.Link);
    node.FLocalDisplay := cells[1];
    if length(cells) > 2 then
      node.FDisplay := cells[2]
    else
      node.FDisplay := node.FLocalDisplay;
    FStack[i] := node;
  finally
    node.free;
  end;
end;

function TICD10Provider.searchFilter(filter: TSearchFilterText; prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise Exception.Create('Not implemented');
end;

function TICD10Provider.SpecialEnumeration: String;
begin
  result := '';
end;

function TICD10Provider.specialFilter(prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise Exception.Create('Not implemented');
end;

function TICD10Provider.subsumesTest(codeA, codeB: String): String;
begin
  raise Exception.Create('Not implemented');
end;

function TICD10Provider.system(context: TCodeSystemProviderContext): String;
begin
  result := FUrl;
end;

function TICD10Provider.TotalCount: integer;
begin
  result := FCodes.Count;
end;

function TICD10Provider.version(context: TCodeSystemProviderContext): String;
begin
  result := FVersion;
end;

end.
