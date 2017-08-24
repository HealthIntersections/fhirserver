unit UniiServices;

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

uses
  SysUtils, Classes,
  StringSupport,
  AdvObjects, AdvObjectLists, AdvFiles, AdvTextExtractors, AdvStringIntegerMatches, AdvExceptions,
  KDBManager,
  FHIRTypes, FHIRResources, TerminologyServices;

type
  TUniiConcept = class (TCodeSystemProviderContext)
  private
    FCode : string;
    FDisplay : String;
    FOthers : TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TUniiFilter = class (TCodeSystemProviderFilterContext)
  private
  public
    Destructor Destroy; Override;
  end;

  TUniiPrep = class (TCodeSystemProviderFilterPreparationContext)
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  end;

  TUniiServices = class (TCodeSystemProvider)
  public
    db : TKDBManager;

    Constructor Create(db : TKDBManager);
    Destructor Destroy; Override;
    Function Link : TUniiServices; overload;

    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function system(context : TCodeSystemProviderContext) : String; override;
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

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
  end;

Procedure ImportUnii(filename : String; dbm : TKDBManager);

implementation

uses
  FHIRLog;

{ TUniiServices }

Constructor TUniiServices.create(db : TKDBManager);
begin
  inherited Create;

  self.db := db;
end;


function TUniiServices.TotalCount : integer;
var
  qry : TKDBConnection;
begin
  if db = nil then
    exit(0);

  qry := db.GetConnection('Unii.Count');
  try
    qry.SQL := 'Select Count(*) from Unii';
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


function TUniiServices.version(context: TCodeSystemProviderContext): String;
begin
  result := '';
end;

function TUniiServices.system(context : TCodeSystemProviderContext) : String;
begin
  result := 'http://fdasis.nlm.nih.gov';
end;

function TUniiServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TUniiServices.getDisplay(code : String; lang : String):String;
var
  qry : TKDBConnection;
begin
  qry := db.GetConnection('Unii.display');
  try
    qry.SQL := 'Select Display from Unii where Code = :code';
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

function TUniiServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  raise Exception.Create('not done yet');
end;

procedure TUniiServices.Displays(code : String; list : TStringList; lang : String);
begin
  list.Add(getDisplay(code, lang));
end;

Procedure ImportUnii(filename : String; dbm : TKDBManager);
var
  tab : TAdvTextExtractor;
  f : TAdvFile;
  s : String;
  cols : TArray<String>;
  map : TAdvStringIntegerMatch;
  key, last, lastDesc : integer;
  db : TKDBConnection;
begin
  logt('Inport UNII from '+filename);
  db := dbm.GetConnection('unii');
  try
    last := 0;
    lastDesc := 0;
    db.ExecSQL('Delete from UNIIDesc');
    db.ExecSQL('Delete from UNII');

    map := TAdvStringIntegerMatch.create;
    try
      map.forced := true;
      f := TAdvFile.Create(filename, fmOpenRead);
      try
        tab := TAdvTextExtractor.Create(f.Link, TEncoding.UTF8);
        try
          s := tab.ConsumeLine;
          while tab.More do
          begin
            s := tab.ConsumeLine;
            cols := s.Split([#9]);
            key := map[cols[2]];
            if key = 0 then
            begin
              inc(last);
              if last mod 1000 = 0 then
                write('.');
              key := last;
              db.ExecSQL('insert into UNII (UniiKey, Code)  values ('+inttostr(key)+', '''+SQLWrapString(cols[2])+''')');
              map[cols[2]] := key;
            end;
            if (cols[1]= 'PT') and (length(cols) >= 4) then
              db.ExecSQL('update UNII set Display = '''+SQLWrapString(cols[3])+'''  where UniiKey= '+inttostr(key))
            else
            begin
              inc(lastDesc);
              db.ExecSQL('insert into UNIIDesc (UniiDescKey, UniiKey, Type, Display)  values ('+inttostr(lastDesc)+', '+inttostr(key)+', '''+SQLWrapString(cols[1])+''', '''+SQLWrapString(cols[0])+''')');
            end;
          end;
        finally
          tab.Free;
        end;
      finally
        f.Free;
      end;
    finally
      map.free;
    end;
  finally
    db.Release;
  end;
end;

function TUniiServices.locate(code : String; var message : String) : TCodeSystemProviderContext;
var
  qry : TKDBConnection;
  res : TUniiConcept;
  key : integer;
  s : String;
begin
  qry := db.GetConnection('Unii.locate');
  try
    qry.SQL := 'Select UniiKey, Display from Unii where code = :code';
    qry.prepare;
    qry.bindString('code', code);
    qry.execute;
    if not qry.FetchNext then
      result := nil
    else
    begin
      res := TUniiConcept.Create;
      try
        res.FCode := code;
        res.FDisplay := qry.ColStringByName['Display'];
        key := qry.ColIntegerByName['UniiKey'];
        qry.Terminate;
        qry.SQL := 'Select Display from UniiDesc where UniiKey = '+inttostr(key);
        qry.prepare;
        qry.execute;
        while qry.FetchNext do
        begin
          s := qry.ColStringByName['Display'];
          if res.FOthers.IndexOf(s) = -1 then
            res.FOthers.Add(s);
        end;
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


function TUniiServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TUniiConcept(context).FCode;
end;

function TUniiServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

destructor TUniiServices.Destroy;
begin
  db.Free;
  inherited;
end;

function TUniiServices.Display(context : TCodeSystemProviderContext; lang : String) : string;
begin
  result := TUniiConcept(context).FDisplay;
end;

procedure TUniiServices.Displays(context: TCodeSystemProviderContext; list: TStringList; lang : String);
begin
  list.Add(Display(context, lang));
  list.AddStrings(TUniiConcept(context).FOthers);
end;

function TUniiServices.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // Unii doesn't do abstract
end;

function TUniiServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TUniiServices.Link: TUniiServices;
begin
  result := TUniiServices(Inherited Link);
end;

function TUniiServices.ChildCount(context : TCodeSystemProviderContext) : integer;
begin
  result := 0; // no children
end;

function TUniiServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  raise Exception.Create('not done yet');
end;

function TUniiServices.locateIsA(code, parent : String) : TCodeSystemProviderContext;
begin
  raise Exception.Create('locateIsA not supported by Unii'); // Unii doesn't have formal subsumption property, so this is not used
end;


function TUniiServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'UNII';
end;

function TUniiServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  raise Exception.Create('not done yet');
end;

function TUniiServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise Exception.Create('not done yet');
end;

function TUniiServices.filter(prop : String; op : TFhirFilterOperatorEnum; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise Exception.Create('not done yet');
end;

function TUniiServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
begin
  raise Exception.Create('not done yet');
end;

function TUniiServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  raise Exception.Create('not done yet');
end;

function TUniiServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise Exception.Create('not done yet');
end;

function TUniiServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise Exception.Create('not done yet');
end;

procedure TUniiServices.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.free;
end;

procedure TUniiServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;

procedure TUniiServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
begin
  // raise Exception.Create('not done yet');
end;

{ TUniiPrep }

constructor TUniiPrep.Create;
begin
  inherited;
end;

destructor TUniiPrep.Destroy;
begin
  inherited;
end;

{ TUniiFilter }

destructor TUniiFilter.Destroy;
begin
  inherited;
end;

{ TUniiConcept }

constructor TUniiConcept.create;
begin
  inherited;
  FOthers := TStringList.Create;
end;

destructor TUniiConcept.destroy;
begin
  FOthers.free;
  inherited;
end;

end.
