unit tx_icd10;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

{$I fhir.inc}

interface

uses
  SysUtils, Classes, Generics.Collections,
  fsl_base, fsl_http,
  fhir_objects, fhir_common, fhir_factory,
  fhir_cdshooks,
  ftx_service;

type
  TICD10Node = class (TCodeSystemProviderContext)
  private
    FCode : String;
    FDisplay : String;
    FLocalDisplay : String;
    FDescendentCount : integer;

    FChildren : TFslList<TICD10Node>;
    function hasChildren : boolean;
    procedure addChild(child : TICD10Node);
  public
    constructor Create(code : String);
    destructor Destroy; override;
    function Link : TICD10Node; overload;
  end;

  { TICD10Provider }

  TICD10Provider = class (TCodeSystemProvider)
  private
    FUrl : String;
    FVersion : String;
    FIsDefault : boolean;
    FRoots : TFslList<TICD10Node>;
    FCodes : TFslList<TICD10Node>;
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
    function link : TICD10Provider; overload;

    class function checkFile(sourceFile : String) : String;
    Property Title : String read FTitle;

    function description : String; override;
    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function systemUri(context : TCodeSystemProviderContext) : String; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; const lang : THTTPLanguages):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; overload; override;
    function locate(code : String) : TCodeSystemProviderContext; overload; override;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function IsInactive(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string; override;
    function Definition(context : TCodeSystemProviderContext) : string; override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList; const lang : THTTPLanguages); overload; override;
    procedure Displays(code : String; list : TStringList; const lang : THTTPLanguages); overload; override;
    function doesFilter(prop : String; op : TFhirFilterOperator; value : String) : boolean; override;

    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function specialFilter(prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; override; // true if the underlying provider collapsed multiple filters
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; overload; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; overload; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure extendLookup(factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; const lang : THTTPLanguages; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;
    function subsumesTest(codeA, codeB : String) : String; override;

    function SpecialEnumeration : String; override;
    procedure getCDSInfo(card : TCDSHookCard; const lang : THTTPLanguages; baseURL, code, display : String); override;

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
    FChildren := TFslList<TICD10Node>.create;
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
  FRoots := TFslList<TICD10Node>.create;
  FCodes := TFslList<TICD10Node>.create;
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
    result := FRoots.count
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

function TICD10Provider.description: String;
begin
  result := FTitle;
end;

function TICD10Provider.Display(context: TCodeSystemProviderContext; const lang : THTTPLanguages): string;
begin
  if lang.matches(FLanguage) then
    result := TICD10Node(context).FLocalDisplay
  else
    result := TICD10Node(context).FDisplay
end;

procedure TICD10Provider.Displays(code: String; list: TStringList; const lang : THTTPLanguages);
var
  context : TCodeSystemProviderContext;
begin
  context := locate(code);
  if context <> nil then
    Displays(context, list, lang);
end;

procedure TICD10Provider.Displays(context: TCodeSystemProviderContext; list: TStringList; const lang : THTTPLanguages);
begin
  if (lang.matches(FLanguage)) then
    list.add(TICD10Node(context).FLocalDisplay)
  else
    list.add(TICD10Node(context).FDisplay)
end;

function TICD10Provider.doesFilter(prop: String; op: TFhirFilterOperator; value: String): boolean;
begin
  result := false;
end;

procedure TICD10Provider.extendLookup(factory : TFHIRFactory; ctxt: TCodeSystemProviderContext; const lang : THTTPLanguages; props : TArray<String>; resp: TFHIRLookupOpResponseW);
var
  p : TFHIRLookupOpRespPropertyW;
begin
  if factory.version <> fhirVersionRelease2 then
  begin
    resp.version := FVersion;
    if TICD10Node(ctxt).FDisplay <> '' then
    begin
      resp.addDesignation('en', TICD10Node(ctxt).FDisplay);
    end;
    p := resp.addProp('descendents');
    p.value := factory.makeInteger(inttostr(TICD10Node(ctxt).FDescendentCount));
  end;
end;

function TICD10Provider.filter(prop: String; op: TFhirFilterOperator; value: String; prep: TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.create('Not implemented: TICD10Provider.filter');
end;

function TICD10Provider.FilterConcept(ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('Not implemented: TICD10Provider.FilterConcept');
end;

function TICD10Provider.filterLocate(ctxt: TCodeSystemProviderFilterContext;code: String): TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('Not implemented: TICD10Provider.filterLocate');
end;

function TICD10Provider.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String; var message: String): TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('Not implemented: TICD10Provider.filterLocate');
end;

function TICD10Provider.FilterMore(ctxt: TCodeSystemProviderFilterContext): boolean;
begin
  raise ETerminologyError.create('Not implemented: TICD10Provider.FilterMore');
end;

procedure TICD10Provider.getCDSInfo(card: TCDSHookCard; const lang : THTTPLanguages; baseURL, code, display: String);
begin
  raise ETerminologyError.create('Not implemented: TICD10Provider.getCDSInfo');
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
  result := getDisplay(code, THTTPLanguages.create('en'));
end;

function TICD10Provider.getDisplay(code : String; const lang : THTTPLanguages): String;
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
  raise ETerminologyError.create('Not implemented: TICD10Provider.InFilter');
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
  result := false;
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

function TICD10Provider.link: TICD10Provider;
begin
  result := TICD10Provider(Inherited Link);
end;

class function TICD10Provider.checkFile(sourceFile: String): String;
var
  cells : TArray<String>;
  ts : TStringList;
begin
  try
    ts := TStringList.create;
    try
      ts.loadFromFile(sourceFile);
      cells := ts[0].Split([#9]);
      if cells[0] <> 'icd-10' then
        result := 'Not an ICD-10 File'
      else
        result := 'Ok (country = '+cells[3]+', lang  '+cells[2]+')';
    finally
      ts.free;
    end;
  except
    on e : Exception do
      result := 'Error: '+e.message;
  end;
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

function TICD10Provider.locateIsA(code,parent: String; disallowParent : boolean = false): TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('Not implemented: TICD10Provider.locateIsA');
end;

function TICD10Provider.name(context: TCodeSystemProviderContext): String;
begin
  result := FTitle;
end;

function TICD10Provider.prepare(prep: TCodeSystemProviderFilterPreparationContext): boolean;
begin
  raise ETerminologyError.create('Not implemented: TICD10Provider.prepare');
end;

procedure TICD10Provider.readHeader(s: String);
var
  cells : TArray<String>;
begin
  cells := s.Split([#9]);
  if cells[0] <> 'icd-10' then
    raise ETerminologyError.create('Invalid file - should start with icd-10');
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
      FStack[i-1].addChild(node);
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
  raise ETerminologyError.create('Not implemented: TICD10Provider.searchFilter');
end;

function TICD10Provider.SpecialEnumeration: String;
begin
  result := '';
end;

function TICD10Provider.specialFilter(prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.create('Not implemented: TICD10Provider.specialFilter');
end;

function TICD10Provider.subsumesTest(codeA, codeB: String): String;
begin
  raise ETerminologyError.create('Not implemented: TICD10Provider.subsumesTest');
end;

function TICD10Provider.systemUri(context: TCodeSystemProviderContext): String;
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
