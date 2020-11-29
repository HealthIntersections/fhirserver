unit ftx_service;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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
  fsl_utilities, fsl_base, fsl_collections, fsl_fpc,
  fsl_http,
  fhir_common, fhir_factory,
  fhir_cdshooks;

const
  ANY_CODE_VS = 'http://hl7.org/fhir/ValueSet/@all';
  ALL_CODE_CS = 'http://hl7.org/fhir/CodeSystem/@all';

Type

  TFhirFilterOperator = fhir_common.TFilterOperator;

  TCodeSystemProviderContext = class (TFslObject)
  public
    function Link : TCodeSystemProviderContext; overload;
  end;

  TCodeSystemProviderFilterContext = class (TFslObject)
  public
    function Link : TCodeSystemProviderFilterContext; overload;
  end;

  TCodeSystemProviderFilterPreparationContext = class (TFslObject)
  public
    function Link : TCodeSystemProviderFilterPreparationContext; overload;
  end;

  TSearchFilterText = class (TFslObject)
  private
    FFilter: string;
    FStems : TStringList;
    FStemmer : TFslWordStemmer;

    function find(s : String) : boolean;

    procedure process;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(filter : String);  overload;
    destructor Destroy; override;

    function Link : TSearchFilterText; overload;

    function null : Boolean;
    function passes(value : String) : boolean; overload;
    function passes(value : String; var rating : double) : boolean; overload;
    function passes(stems : TFslStringList; var rating : double) : boolean; overload;
    property filter : string read FFilter;
    property stems : TStringList read FStems;
  end;

  TCodeSystemProvider = class abstract (TFslObject)
  private
    FUseCount : cardinal;
  public
    function Link : TCodeSystemProvider; overload;

    function description : String;  virtual; abstract;
    function TotalCount : integer;  virtual; abstract;
    function ChildCount(context : TCodeSystemProviderContext) : integer; virtual; abstract;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; virtual; abstract;
    function systemUri(context : TCodeSystemProviderContext) : String; virtual; abstract;
    function version(context : TCodeSystemProviderContext) : String; virtual;
    function name(context : TCodeSystemProviderContext) : String; virtual;
    function getDisplay(code : String; const lang : THTTPLanguages):String; virtual; abstract;
    function getDefinition(code : String):String; virtual; abstract;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; overload; virtual; abstract;
    function locate(code : String) : TCodeSystemProviderContext; overload; virtual;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; virtual; abstract;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; virtual; abstract;
    function IsInactive(context : TCodeSystemProviderContext) : boolean; overload; virtual;
    function IsInactive(code : String) : boolean; overload; virtual;
    function Code(context : TCodeSystemProviderContext) : string; virtual; abstract;
    function Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string; virtual; abstract;
    function Definition(context : TCodeSystemProviderContext) : string; virtual; abstract;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList; const lang : THTTPLanguages); overload; virtual; abstract;
    procedure Displays(code : String; list : TStringList; const lang : THTTPLanguages); overload; virtual; abstract;
    function doesFilter(prop : String; op : TFhirFilterOperator; value : String) : boolean; virtual;

    function hasSupplement(url : String) : boolean; virtual;
    function getPrepContext : TCodeSystemProviderFilterPreparationContext; virtual;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; virtual; abstract;
    function specialFilter(prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; virtual;
    function filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; virtual; abstract;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; virtual; // true if the underlying provider collapsed multiple filters
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; overload; virtual; abstract;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; overload; virtual;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; virtual; abstract;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; virtual; abstract;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; virtual; abstract;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; virtual; abstract;
    procedure extendLookup(factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; const lang : THTTPLanguages; props : TArray<String>; resp : TFHIRLookupOpResponseW); virtual;
    function subsumesTest(codeA, codeB : String) : String; virtual;

    function SpecialEnumeration : String; virtual;
    procedure getCDSInfo(card : TCDSHookCard; const lang : THTTPLanguages; baseURL, code, display : String); virtual;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); overload; virtual;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); overload; virtual; abstract;
    procedure Close(ctxt : TCodeSystemProviderContext); overload; virtual; abstract;

    procedure RecordUse;
    function defToThisVersion(specifiedVersion : String) : boolean; virtual;
    property UseCount : cardinal read FUseCount;
  end;

implementation

{ TCodeSystemProvider }

procedure TCodeSystemProvider.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  // do nothing
end;

function TCodeSystemProvider.defToThisVersion(specifiedVersion : String): boolean;
begin
  result := true;
end;

function TCodeSystemProvider.doesFilter(prop: String; op: TFhirFilterOperator; value: String): boolean;
var
  ctxt : TCodeSystemProviderFilterContext;
begin
  ctxt := filter(prop, op, value, nil);
  result := ctxt <> nil;
  if result then
    Close(ctxt);
end;



procedure TCodeSystemProvider.extendLookup(factory : TFHIRFactory; ctxt: TCodeSystemProviderContext; const lang : THTTPLanguages; props : TArray<String>; resp : TFHIRLookupOpResponseW);
begin
  // nothing here
end;

function TCodeSystemProvider.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String): TCodeSystemProviderContext;
var
  msg : String;
begin
  result := filterLocate(ctxt, code, msg);
end;

procedure TCodeSystemProvider.getCDSInfo(card: TCDSHookCard; const lang : THTTPLanguages; baseURL, code, display: String);
begin
  card.summary := 'No CDSHook Implemeentation for code system '+systemUri(nil)+' for code '+code+' ('+display+')';
end;

function TCodeSystemProvider.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := nil;
end;

function TCodeSystemProvider.hasSupplement(url: String): boolean;
begin
  result := false;
end;

function TCodeSystemProvider.IsInactive(code: String): boolean;
var
  ctxt : TCodeSystemProviderContext;
begin
  ctxt := locate(code);
  try
    result := IsInactive(ctxt);
  finally
    Close(ctxt);
  end;
end;

function TCodeSystemProvider.IsInactive(context: TCodeSystemProviderContext): boolean;
begin
  result := false;
end;

function TCodeSystemProvider.Link: TCodeSystemProvider;
begin
  result := TCodeSystemProvider(inherited link);
end;

function TCodeSystemProvider.locate(code: String): TCodeSystemProviderContext;
var
  msg : String;
begin
  result := locate(code, msg);
end;

function TCodeSystemProvider.name(context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

function TCodeSystemProvider.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  result := false;
end;

procedure TCodeSystemProvider.RecordUse;
begin
  inc(FUseCount);
end;

function TCodeSystemProvider.SpecialEnumeration: String;
begin
  result := '';
end;

function TCodeSystemProvider.specialFilter(prep: TCodeSystemProviderFilterPreparationContext; sort: boolean): TCodeSystemProviderFilterContext;
begin
  raise ETerminologyError.create('Not implemented for '+ClassName);
end;

function TCodeSystemProvider.subsumesTest(codeA, codeB: String): String;
begin
  raise ETerminologyError.create('Subsumption Testing is not supported for system '+systemUri(nil));
end;

function TCodeSystemProvider.version(context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

{ TSearchFilterText }

constructor TSearchFilterText.create(filter: String);
begin
  Create;
  FStemmer := TFslWordStemmer.create('english');
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

function TSearchFilterText.find(s: String): boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FStems.Count - 1;
  while not result and (L <= H) do
  begin
    I := (L + H) shr 1;
    C := CompareStr(FStems[I], copy(S, 1, length(FStems[i])));
    if C = 0 then
      Result := True
    else if C < 0 then
      L := I + 1
    else
      H := I - 1;
  end;
end;

function TSearchFilterText.Link: TSearchFilterText;
begin
  result := TSearchFilterText(inherited link);
end;

function TSearchFilterText.null: Boolean;
begin
  result := FStems.Count = 0;
end;

function TSearchFilterText.passes(value: String): boolean;
var
  r : double;
begin
  result := passes(value, r);
end;

function TSearchFilterText.passes(value: String; var rating : double): boolean;
var
  i, j : integer;
begin
  result := Null;
  i := 1;
  rating := 0;
  while (not result) and (i <= length(value)) Do
  begin
    if CharInSet(value[i], ['0'..'9', 'a'..'z', 'A'..'Z']) then
    begin
      j := i;
      while (i <= length(value)) and CharInSet(value[i], ['0'..'9', 'a'..'z', 'A'..'Z']) do
        inc(i);
      result := find(lowercase(FStemmer.Stem(copy(value, j, i-j))));
      if result then
        rating := rating + length(value) / FStems.Count;
    end
    else
      inc(i);
  End;
end;

function TSearchFilterText.passes(stems: TFslStringList; var rating : double): boolean;
var
  i : integer;
  all, any, this : boolean;
begin
  rating := 0;
  if FStems.Count = 0 then
    result := true
  else
  begin
    all := true;
    any := false;
    for i := 0 to stems.count - 1 do
    begin
      this := find(stems[i]);
      all := all and this;
      any := any or this;
      if this then
        rating := rating + length(stems[i])/Fstems.count;
    end;
    result := any;
  end;
end;

procedure TSearchFilterText.process;
var
  i, j : Integer;
begin
  i := 1;
  while i <= length(FFilter) Do
  begin
    if CharInSet(FFilter[i], ['0'..'9', 'a'..'z', 'A'..'Z']) then
    begin
      j := i;
      while (i <= length(FFilter)) and CharInSet(FFilter[i], ['0'..'9', 'a'..'z', 'A'..'Z']) do
        inc(i);
      FStems.Add(lowercase(FStemmer.Stem(copy(FFilter, j, i-j))));
    end
    else
      inc(i);
  End;
  FStems.Sort;
end;

function TSearchFilterText.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FFilter.length * sizeof(char)) + 12);
  inc(result, FStems.sizeInBytes);
  inc(result, FStemmer.sizeInBytes);
end;

{ TCodeSystemProviderContext }

function TCodeSystemProviderContext.Link: TCodeSystemProviderContext;
begin
  result := TCodeSystemProviderContext(inherited link);
end;

{ TCodeSystemProviderFilterContext }

function TCodeSystemProviderFilterContext.Link: TCodeSystemProviderFilterContext;
begin
  result := TCodeSystemProviderFilterContext(inherited link);
end;

{ TCodeSystemProviderFilterPreparationContext }

function TCodeSystemProviderFilterPreparationContext.Link: TCodeSystemProviderFilterPreparationContext;
begin
  result := TCodeSystemProviderFilterPreparationContext(inherited Link);
end;

end.
