unit tx_unii;

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
  SysUtils, Classes,
  fsl_utilities, fsl_base, fsl_collections, fsl_stream, fsl_http, fsl_lang, fsl_threads, fsl_i18n,
  fdb_manager,
  fhir_objects, fhir_features, fhir_uris,
  ftx_service;

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
    destructor Destroy; Override;
  end;

  TUniiPrep = class (TCodeSystemProviderFilterPreparationContext)
  public
    constructor Create; Override;
    destructor Destroy; Override;
  end;

  { TUniiServices }

  TUniiServices = class (TCodeSystemProvider)
  private
    db : TFDBManager;
    FVersion : String;
  public
    constructor Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport; db : TFDBManager);
    destructor Destroy; Override;
    Function Link : TUniiServices; overload;

    class function checkDB(conn : TFDBConnection) : String;

    function TotalCount : integer;  override;
    function getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function version : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function systemUri : String; override;
    function getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String; override;
    function getDefinition(opContext : TTxOperationContext; code : String):String; override;
    function locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; override;
    function Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;
    function Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string; override;
    procedure Designations(opContext : TTxOperationContext; context : TCodeSystemProviderContext; list : TConceptDesignations); override;
    function Definition(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;

    function getPrepContext(opContext : TTxOperationContext) : TCodeSystemProviderFilterPreparationContext; override;
    function prepare(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;

    function description : String; override;
    function searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function filterSize(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : integer; override;
    function FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(opContext : TTxOperationContext; textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;

    procedure defineFeatures(opContext : TTxOperationContext; features : TFslList<TFHIRFeature>); override;
  end;

Procedure ImportUnii(filename, version : String; dbm : TFDBManager; callback : TWorkProgressEvent);

implementation

uses
  fsl_logging;

{ TUniiServices }

constructor TUniiServices.Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport; db: TFDBManager);
var
  conn : TFDBConnection;
begin
  inherited Create(languages, i18n);

  self.db := db;
  conn := db.GetConnection('version');
  try
    conn.SQL := 'Select Version from UniiVersion';
    conn.prepare;
    conn.execute;
    conn.fetchnext;
    FVersion := conn.colStringByname['Version'];
    conn.terminate;

    conn.release;
  except
    on e : exception do
      conn.Error(e);
  end;
end;


procedure TUniiServices.defineFeatures(opContext : TTxOperationContext; features: TFslList<TFHIRFeature>);
begin
end;

function TUniiServices.TotalCount : integer;
var
  qry : TFDBConnection;
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


function TUniiServices.version: String;
begin
  result := FVersion;
end;

function TUniiServices.systemUri : String;
begin
  result := URI_UNII;
end;

function TUniiServices.getDefinition(opContext : TTxOperationContext; code: String): String;
begin
  result := '';
end;

function TUniiServices.getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String;
var
  qry : TFDBConnection;
begin
  qry := db.GetConnection('Unii.display');
  try
    qry.SQL := 'Select Display from Unii where Code = :code';
    qry.prepare;
    qry.BindString('code', code);
    qry.execute;
    qry.FetchNext;
    result := qry.colString[1].Trim;
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

function TUniiServices.getPrepContext(opContext : TTxOperationContext): TCodeSystemProviderFilterPreparationContext;
begin
  raise ETerminologyTodo.Create('TUniiServices.getPrepContext');
end;

Procedure ImportUnii(filename, version : String; dbm : TFDBManager; callback : TWorkProgressEvent);
var
  tab : TFslTextExtractor;
  f : TFslFile;
  s : String;
  cols : TArray<String>;
  map : TFslStringIntegerMatch;
  key, last, lastDesc, size, t : integer;
  db : TFDBConnection;
  meta : TFDBMetaData;

begin
  callback(nil, 0, false, 'Preparing');
  db := dbm.GetConnection('unii');
  try
    callback(nil, 0, false, 'Cleaning up');
    meta := db.FetchMetaData;
    try
      if meta.hasTable('UniiDesc', true) then
        db.DropTable('UniiDesc');
      if meta.hasTable('Unii', true) then
        db.DropTable('Unii');
      if meta.hasTable('UniiVersion', true) then
        db.DropTable('UniiVersion');
    finally
      meta.free;
    end;
    db.ExecSQL('CREATE TABLE Unii (UniiKey int NOT NULL, Code nchar(20) NOT NULL, Display nchar(255) NULL, CONSTRAINT PK_Unii PRIMARY KEY ( UniiKey ASC))');
    db.ExecSQL('CREATE TABLE UniiDesc ( UniiDescKey int NOT NULL, UniiKey int NOT NULL, Type nchar(20) NOT NULL, Display nchar(255) NULL, CONSTRAINT PK_UniiDesc PRIMARY KEY (UniiDescKey ASC))');
    db.ExecSQL('CREATE TABLE UniiVersion ( Version nchar(20) NOT NULL)');
    db.ExecSQL('Insert into UniiVersion (Version) values ('''+SQLWrapString(version)+''')');

    last := 0;
    lastDesc := 0;
    map := TFslStringIntegerMatch.Create;
    try
      map.forced := true;
      f := TFslFile.Create(filename, fmOpenRead + fmShareDenyWrite);
      try
        size := f.Size;
        t := 0;
        callback(nil, 0, false, 'Loading');
        tab := TFslTextExtractor.Create(f.Link, TEncoding.UTF8);
        try
          s := tab.ConsumeLine;
          inc(t, length(s) + 2); // ascii so works ok
          while tab.More do
          begin
            callback(nil, trunc((t / size) * 100), false, 'Loading');
            s := tab.ConsumeLine;
            inc(t, length(s) + 2); // ascii so works ok
            cols := s.Split([#9]);
            key := map[cols[2]];
            if key = 0 then
            begin
              inc(last);
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
          callback(nil, 100, true, 'Loading');
        finally
          tab.free;
        end;
      finally
        f.free;
      end;
    finally
      map.free;
    end;
  finally
    db.Release;
  end;
end;

function TUniiServices.locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext;
var
  qry : TFDBConnection;
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
            res.FOthers.Add(s.trim);
        end;
        result := res.Link;
      finally
        res.free;
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


function TUniiServices.Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string;
begin
  result := TUniiConcept(context).FCode;
end;

function TUniiServices.Definition(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TUniiServices.description: String;
begin
  result := 'UNII Codes';
end;

destructor TUniiServices.Destroy;
begin
  db.free;
  inherited;
end;

function TUniiServices.Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string;
begin
  result := TUniiConcept(context).FDisplay.trim;
end;

procedure TUniiServices.Designations(opContext : TTxOperationContext; context: TCodeSystemProviderContext; list: TConceptDesignations);
begin
  list.addDesignation(true, true, '', '', Display(opContext, context, nil));
  list.addDesignation(false, true, '', '', TUniiConcept(context).FOthers);
end;

function TUniiServices.IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // Unii doesn't do abstract
end;

function TUniiServices.isNotClosed(opContext : TTxOperationContext; textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TUniiServices.Link: TUniiServices;
begin
  result := TUniiServices(Inherited Link);
end;

class function TUniiServices.checkDB(conn: TFDBConnection): String;
var
  meta : TFDBMetaData;
begin
  meta := conn.FetchMetaData;
  try
    if not meta.HasTable('Unii') or not meta.HasTable('UniiDesc') then
      result := 'Missing Tables - needs re-importing'
    else
      result := 'OK ('+inttostr(conn.countSql('Select count(*) from Unii'))+' Concepts)';
  finally
    meta.free;
  end;
end;

function TUniiServices.getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
begin
  result := TCodeSystemIteratorContext.Create(nil, 0); // no children
end;

function TUniiServices.getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('TUniiServices.getcontext');
end;

function TUniiServices.locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.Create('locateIsA not supported by Unii', itNotSupported); // Unii doesn't have formal subsumption property, so this is not used
end;


function TUniiServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'UNII';
end;

function TUniiServices.prepare(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext) : boolean;
begin
  raise ETerminologyTodo.Create('TUniiServices.prepare');
end;

function TUniiServices.searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.Create('TUniiServices.searchFilter');
end;

function TUniiServices.filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
begin
  raise ETerminologyTodo.Create('TUniiServices.filter');
end;

function TUniiServices.filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('TUniiServices.filterLocate');
end;

function TUniiServices.FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean;
begin
  raise ETerminologyTodo.Create('TUniiServices.FilterMore');
end;

function TUniiServices.filterSize(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): integer;
begin
  raise ETerminologyTodo.Create('TUniiServices.FilterMore');
end;

function TUniiServices.FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  raise ETerminologyTodo.Create('TUniiServices.FilterConcept');
end;

function TUniiServices.InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyTodo.Create('TUniiServices.InFilter');
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

constructor TUniiConcept.Create;
begin
  inherited;
  FOthers := TStringList.Create;
end;

destructor TUniiConcept.Destroy;
begin
  FOthers.free;
  inherited;
end;

end.
