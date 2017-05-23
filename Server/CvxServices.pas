unit CvxServices;

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


interface

// based on a table by importing the excel spreadsheet directly
uses
  SysUtils, Classes,
  StringSupport,
  AdvObjects, AdvGenerics, AdvFiles, AdvTextExtractors, AdvStringIntegerMatches,  AdvExceptions,
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
    function link : TCvxConcept; overload;
    property code : String read FCode;
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
    FList : TAdvList<TCvxConcept>;
    FMap : TAdvMap<TCvxConcept>;

    procedure load;
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

{ TCvxServices }

Constructor TCvxServices.create(db : TKDBManager);
begin
  inherited Create;

  self.db := db;
  FList := TAdvList<TCvxConcept>.create;
  FMap := TAdvMap<TCvxConcept>.create;
  Load;
end;


function TCvxServices.TotalCount : integer;
begin
  result := FList.Count;
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
begin
  result := FMap[code].FDisplay;
end;

function TCvxServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  raise ETerminologySetup.Create('not done yet');
end;

procedure TCvxServices.Displays(code : String; list : TStringList; lang : String);
begin
  list.Add(getDisplay(code, lang));
end;

procedure TCvxServices.load;
var
  qry : TKDBConnection;
  res : TCvxConcept;
begin
  qry := db.GetConnection('Cvx.locate');
  try
    qry.SQL := 'Select [CVX Code], [CVX Short Description], [Full Vaccine Name] from Cvx';
    qry.prepare;
    qry.execute;
    while qry.FetchNext do
    begin
      res := TCvxConcept.Create;
      try
        res.FCode := qry.ColStringByName['CVX Code'];
        res.FDisplay := qry.ColStringByName['CVX Short Description'];
        res.FOthers.Add(qry.ColStringByName['Full Vaccine Name']);
        Flist.Add(res.Link);
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
  for res in FList do
    FMap.AddOrSetValue(res.code, res.link);
end;

function TCvxServices.locate(code : String; var message : String) : TCodeSystemProviderContext;
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
  FList.Free;
  FMap.Free;
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
  if context = nil then
    result := totalcount
  else
    result := 0; // no children
end;

function TCvxServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  result := FList[ndx];
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

function TCvxServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
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

function TCvxConcept.link: TCvxConcept;
begin
  result := TCvxConcept(inherited link);
end;

end.
