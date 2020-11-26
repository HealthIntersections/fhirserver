unit tx_rxnorm;

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
  fsl_base, fsl_utilities, fsl_http, fsl_threads,
  fdb_manager,
  fhir_objects, fhir_common, fhir_factory, fhir_utilities,
  fhir_cdshooks,
  ftx_service;

type
  TUMLSConcept = class (TCodeSystemProviderContext)
  private
    FCode : string;
    FDisplay : String;
    FOthers : TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TUMLSFilter = class (TCodeSystemProviderFilterContext)
  private
    sql : String;
    text : boolean;
    qry : TFDBConnection;
  public
    destructor Destroy; Override;
    function Link : TUMLSFilter; overload;
  end;

  TUMLSPrep = class (TCodeSystemProviderFilterPreparationContext)
  private
    filters : TFslList<TUMLSFilter>;
  public
    constructor Create; Override;
    destructor Destroy; Override;
  end;

  { TUMLSServices }

  TUMLSServices = class (TCodeSystemProvider)
  private
    nci : boolean;
    dbprefix : string;
    db : TFDBManager;
    rels : TStringList;
    reltypes : TStringList;

    procedure load(list : TStringList; sql : String);
  protected
    class function getSAB : String; virtual;
    function getCodeField : String; virtual;
  public
    constructor Create(nci : boolean; db : TFDBManager);
    destructor Destroy; Override;
    Function Link : TUMLSServices; overload;

    class function checkDB(conn : TFDBConnection) : String;

    function TotalCount : integer;  override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
//    function systemUri(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; const lang : THTTPLanguages):String; override;
    function getDefinition(code : String):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string; override;
    procedure Displays(code : String; list : TStringList; const lang : THTTPLanguages); override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList; const lang : THTTPLanguages); override;
    function Definition(context : TCodeSystemProviderContext) : string; override;

    function getPrepContext : TCodeSystemProviderFilterPreparationContext; override;
    function prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;

    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure getCDSInfo(card : TCDSHookCard; const lang : THTTPLanguages; baseURL, code, display : String); override;
    procedure extendLookup(factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; const lang : THTTPLanguages; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;
    //function subsumes(codeA, codeB : String) : String; override;

    procedure Close(ctxt : TCodeSystemProviderFilterPreparationContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
  end;

  TRxNormServices = class (TUMLSServices)
  public
    constructor Create(db : TFDBManager);
    function systemUri(context : TCodeSystemProviderContext) : String; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function description : String; override;
  end;

  TNDFRTServices = class (TUMLSServices)
  protected
    class function getSAB : String; override;
    function getCodeField : String; override;
  public
    constructor Create(db : TFDBManager);
    function systemUri(context : TCodeSystemProviderContext) : String; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function description : String; override;
  end;

  TNciMetaServices = class (TUMLSServices)
  public
    constructor Create(db : TFDBManager);
    function systemUri(context : TCodeSystemProviderContext) : String; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function description : String; override;
  end;

  { TUMLSImporter }

  TUMLSImporter = class (TFslObject)
  private
    FFolder : String;
    FConn : TFDBConnection;
    procedure CheckFiles;
    procedure MakeStems(stemmer : TFslWordStemmer; stems : TStringList; desc : String; cui : string); overload;
    procedure makeStems(callback: TWorkProgressEvent); overload;
    procedure CreateTables(callback : TWorkProgressEvent);
    procedure loadRXNCONSO(callback : TWorkProgressEvent);
    procedure loadRXNRel(callback : TWorkProgressEvent);
    procedure loadRXNSty(callback : TWorkProgressEvent);
  public
    constructor Create(folder : String; conn : TFDBConnection);
    destructor Destroy; override;

    procedure Doinstall(sender : TObject; callback : TWorkProgressEvent);
  end;


implementation
{ TUMLSImporter }

constructor TUMLSImporter.Create(folder: String; conn: TFDBConnection);
begin
  inherited create;
  FFolder := folder;
  FCOnn := conn;
end;

destructor TUMLSImporter.Destroy;
begin
  FConn.Free;
  inherited Destroy;
end;

procedure TUMLSImporter.CheckFiles;
begin
  if not FileExists(path([FFolder, 'RXNCONSO.RRF'])) then
    raise Exception.create('File not found: RXNCONSO.RRF');
  if not FileExists(path([FFolder, 'RXNREL.RRF'])) then
    raise Exception.create('File not found: RXNREL.RRF');
  if not FileExists(path([FFolder, 'RXNSTY.RRF'])) then
    raise Exception.create('File not found: RXNSTY.RRF');
end;

procedure TUMLSImporter.CreateTables(callback : TWorkProgressEvent);
var
  meta : TFDBMetaData;
begin
  meta := FConn.FetchMetaData;
  try
    if meta.HasTable('RXNCONSO') then  FConn.ExecSQL('Drop table RXNCONSO');
    if meta.HasTable('RXNREL') then  FConn.ExecSQL('Drop table RXNREL');
    if meta.HasTable('RXNSTY') then  FConn.ExecSQL('Drop table RXNSTY');
    if meta.HasTable('RXNSTEMS') then  FConn.ExecSQL('Drop table RXNSTEMS');
  finally
    meta.free;
  end;

  callback(self, 0, false, 'Create table RXNCONSO (Step 1 of 5)');
  FConn.ExecSQL('CREATE TABLE RXNCONSO ( '+
                '  RXCUI             varchar(8) NOT NULL, '+
                '  RXAUI             varchar(8) NOT NULL, '+
                '  SAB               varchar (20) NOT NULL, '+
                '  TTY               varchar (20) NOT NULL, '+
                '  CODE              varchar (50) NOT NULL, '+
                '  STR               varchar (3000) NOT NULL, '+
                '  SUPPRESS          varchar (1))');
  FConn.ExecSQL('CREATE INDEX X_RXNCONSO_1 ON RXNCONSO(RXCUI)');
  FConn.ExecSQL('CREATE INDEX X_RXNCONSO_2 ON RXNCONSO(SAB, TTY)');
  FConn.ExecSQL('CREATE INDEX X_RXNCONSO_3 ON RXNCONSO(CODE, SAB, TTY)');
  FConn.ExecSQL('CREATE INDEX X_RXNCONSO_4 ON RXNCONSO(TTY, SAB)');
  FConn.ExecSQL('CREATE INDEX X_RXNCONSO_6 ON RXNCONSO(RXAUI)');

  callback(self, 25, false, 'Create table RXNREL (Step 1 of 5)');
  FConn.ExecSQL('CREATE TABLE RXNREL ( '+
                '  RXCUI1    varchar(8) , '+
                '  RXAUI1    varchar(8), '+
                '  REL       varchar(4) , '+
                '  RXCUI2    varchar(8) , '+
                '  RXAUI2    varchar(8), '+
                '  RELA      varchar(100) , '+
                '  SAB       varchar(20) NOT NULL '+
                ')');
  FConn.ExecSQL('CREATE INDEX X_RXNREL_2 ON RXNREL(REL, RXAUI1)');
  FConn.ExecSQL('CREATE INDEX X_RXNREL_3 ON RXNREL(REL, RXCUI1)');
  FConn.ExecSQL('CREATE INDEX X_RXNREL_4 ON RXNREL(RELA, RXAUI2)');
  FConn.ExecSQL('CREATE INDEX X_RXNREL_5 ON RXNREL(RELA, RXCUI2)');

  callback(self, 50, false, 'Create table RXNSTY (Step 1 of 5)');
  FConn.ExecSQL('CREATE TABLE RXNSTY ( '+
                '  RXCUI          varchar(8) NOT NULL, '+
                '  TUI            varchar (4) '+
                ') ');
  FConn.ExecSQL('CREATE INDEX X_RXNSTY_2 ON RXNSTY(TUI)');

  callback(self, 75, false, 'Create table RXNSTEMS (Step 1 of 5)');
  FConn.ExecSQL('CREATE TABLE RXNSTEMS ( '+
                ' stem CHAR(20) NOT NULL, '+
                ' CUI VARCHAR(8) NOT NULL, '+
                'PRIMARY KEY (stem, CUI))');
  callback(self, 100, false, 'Created Tables (Step 1 of 5)');
end;

procedure TUMLSImporter.loadRXNCONSO(callback: TWorkProgressEvent);
var
  ts : TStringList;
  s : TArray<string>;
  i : integer;
begin
  callback(self, 0, false, 'Load RXNCONSO (Step 2 of 5)');
  ts := TStringList.create;
  try
    ts.LoadFromFile(path([FFolder, 'RXNCONSO.RRF']));
    FConn.sql := 'insert into RXNCONSO (RXCUI, RXAUI, SAB, TTY, CODE, STR, SUPPRESS) values (:RXCUI, :RXAUI, :SAB, :TTY, :CODE, :STR, :SUPPRESS)';
    FConn.Prepare;
    for i := 0 to ts.count - 1 do
    begin
      if (i mod 135 = 0) then
        callback(self, trunc((i / ts.count) * 100), false, 'Load RXNCONSO line '+inttostr(i)+' (Step 2 of 5)');

      s := ts[i].split(['|']);
      FConn.BindString('RXCUI', s[0]);
      FConn.BindString('RXAUI', s[7]);
      FConn.BindString('SAB', s[11]);
      FConn.BindString('TTY', s[12]);
      FConn.BindString('CODE', s[13]);
      FConn.BindString('STR', s[14]);
      FConn.BindString('SUPPRESS', s[16]);
      FConn.Execute;
    end;
    FConn.Terminate;
  finally
    ts.free;
  end;
  callback(self, 100, false, 'RXNCONSO Loaded (Step 2 of 5)');
end;

procedure TUMLSImporter.loadRXNRel(callback: TWorkProgressEvent);
var
  ts : TStringList;
  s : TArray<string>;
  i : integer;
begin
  callback(self, 0, false, 'Load RXNREL (Step 3 of 5)');
  ts := TStringList.create;
  try
    ts.LoadFromFile(path([FFolder, 'RXNREL.RRF']));
    FConn.sql := 'insert into RXNREL (RXCUI1, RXAUI1, REL, RXCUI2, RXAUI2, RELA, SAB) values (:RXCUI1, :RXAUI1, :REL, :RXCUI2, :RXAUI2, :RELA, :SAB)';
    FConn.Prepare;
    for i := 0 to ts.count - 1 do
    begin
      if (i mod 135 = 0) then
        callback(self, trunc((i / ts.count) * 100), false, 'Load RXNREL line '+inttostr(i)+' (Step 3 of 5)');

      s := ts[i].split(['|']);
      FConn.BindString('RXCUI1', s[0]);
      FConn.BindString('RXAUI1', s[1]);
      FConn.BindString('REL', s[3]);
      FConn.BindString('RXCUI2', s[4]);
      FConn.BindString('RXAUI2', s[5]);
      FConn.BindString('RELA', s[7]);
      FConn.BindString('SAB', s[10]);
      FConn.Execute;
    end;
    FConn.Terminate;
  finally
    ts.free;
  end;
  callback(self, 100, false, 'RXNREL Loaded (Step 3 of 5)');
end;

procedure TUMLSImporter.loadRXNSty(callback: TWorkProgressEvent);
var
  ts : TStringList;
  s : TArray<string>;
  i : integer;
begin
  callback(self, 0, false, 'Load RXNSTY (Step 4 of 5)');
  ts := TStringList.create;
  try
    ts.LoadFromFile(path([FFolder, 'RXNSTY.RRF']));
    FConn.sql := 'insert into RXNSTY (RXCUI, TUI) values (:RXCUI, :TUI)';
    FConn.Prepare;
    for i := 0 to ts.count - 1 do
    begin
      if (i mod 37 = 0) then
        callback(self, trunc((i / ts.count) * 100), false, 'Load RXNSTY line '+inttostr(i)+' (Step 4 of 5)');

      s := ts[i].split(['|']);
      FConn.BindString('RXCUI', s[0]);
      FConn.BindString('TUI', s[1]);
      FConn.Execute;
    end;
    FConn.Terminate;
  finally
    ts.free;
  end;
  callback(self, 100, false, 'RXNSTY Loaded (Step 4 of 5)');
end;

procedure TUMLSImporter.makeStems(callback: TWorkProgressEvent);
var
  stems, list : TStringList;
  stemmer : TFslWordStemmer;
  i, j, t : integer;
begin
  callback(self, 100, false, 'Generate Word Index (Step 5 of 5)');

  stemmer := TFslWordStemmer.create('english');
  stems := TStringList.Create;
  try
    stems.Sorted := true;
    FConn.ExecSQL('delete from rxnstems');
    t := FConn.CountSQL('Select count(*) from rxnconso where SAB = ''RXNORM''');
    FConn.SQL := 'select RXCUI, STR from rxnconso where SAB = ''RXNORM'''; // allow SY
    FConn.Prepare;
    FConn.Execute;
    i := 0;
    while FConn.FetchNext do
    begin
      makeStems(stemmer, stems, FConn.ColString[2], FConn.ColString[1]);
      inc(i);
      if (i mod 137 = 0) then
        callback(self, trunc(i * 10 / t), false, 'Generate Word Index (Step 5 of 5)')
    end;
    FConn.Terminate;

    FConn.SQL := 'insert into rxnstems (stem, cui) values (:stem, :cui)';
    FConn.Prepare;
    callback(self, 10, false, 'Store Word Index (Step 5 of 5)');
    for i := 0 to stems.count - 1 do
    begin
      list := stems.objects[i] as TStringList;
      for j := 0 to list.count-1 do
      begin
        FConn.BindString('stem', copy(stems[i], 1, 20));
        FConn.BindString('cui', list[j]);
        FConn.Execute;
      end;
      if (i mod 137 = 0) then
        callback(self, 10 + trunc(i * 90 / stems.Count), false, 'Store Word Index (Step 5 of 5)');
    end;
    callback(self, 100, true, 'Finished building Word Index (Step 5 of 5)');
    FConn.Terminate;
    FConn.Release;
  finally
    for i := 0 to stems.Count - 1 do
      stems.Objects[i].free;
    stems.Free;
    stemmer.Free;
  end;
end;

procedure TUMLSImporter.MakeStems(stemmer : TFslWordStemmer; stems : TStringList; desc : String; cui : string);
var
  s : String;
  i : integer;
  list : TStringList;
begin
  while (desc <> '') do
  begin
    StringSplit(desc, [' ', '.', ',', '-', ')', '(', '#', '/', '%', '[', ']', '{', '}', ':', '@'], s, desc);
    s := StringTrimSet(s, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']).ToLower;
    if (s <> '') and not StringIsInteger16(s) and (s[1] >= 'a') then
    begin
      s := stemmer.Stem(s);
      if stems.Find(s, i) then
        TStringList(stems.Objects[i]).add(cui)
      else
      begin
        list := TStringList.Create;
        list.Sorted := true;
        list.Add(cui);
        stems.AddObject(s, list);
      end;
    end;
  end;
end;

procedure TUMLSImporter.Doinstall(sender: TObject; callback: TWorkProgressEvent);
begin
  callback(self, 1, false, 'Checking');
  CheckFiles;
  callback(self, 1, false, 'Creating Tables');
  CreateTables(callback);
  loadRXNCONSO(callback);
  loadRXNRel(callback);
  loadRXNSty(callback);
  makeStems(callback);
  callback(self, 1, true, 'Finished importing');
end;

{ TUMLSServices }

constructor TUMLSServices.Create(nci: boolean; db: TFDBManager);
begin
  inherited Create;

  self.nci := nci;
  if (nci) then
    dbprefix := 'NciMeta'
  else
    dbprefix := 'RxNorm';
  self.db := db;
  rels := TStringList.create;
  reltypes := TStringList.create;

  if (TotalCount = 0) then
    raise EDBException.create('Error Connecting to RxNorm');
  load(rels, 'select distinct REL from RXNREL');
  load(reltypes, 'select distinct RELA from RXNREL');
end;



function TUMLSServices.TotalCount : integer;
var
  qry : TFDBConnection;
begin
  qry := db.GetConnection(dbprefix+'.Count');
  try
    qry.SQL := 'Select Count(*) from rxnconso where SAB = '''+getSAB+''' and TTY <> ''SY''';
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


function TUMLSServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TUMLSServices.getDisplay(code : String; const lang : THTTPLanguages):String;
var
  qry : TFDBConnection;
begin
  qry := db.GetConnection(dbprefix+'.display');
  try
    qry.SQL := 'Select STR from rxnconso where RXCUI = :code and SAB = '''+getSAB+''' and TTY <> ''SY''';
    qry.prepare;
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

function TUMLSServices.getPrepContext: TCodeSystemProviderFilterPreparationContext;
begin
  result := TUMLSPrep.Create;
end;

class function TUMLSServices.getSAB: String;
begin
  result := 'RXNORM';
end;

procedure TUMLSServices.Displays(code : String; list : TStringList; const lang : THTTPLanguages);
begin
  list.Add(getDisplay(code, lang));
end;


{
 TCodeSystemProviderContext methods

 a TCodeSystemProviderContext is a reference to a code (A CUI in RXNrom) that is
 used to get information about the code
}

procedure TUMLSServices.load(list: TStringList; sql: String);
var
  qry : TFDBConnection;
begin
  qry := db.GetConnection(dbprefix+'.display');
  try
    qry.SQL := sql;
    qry.prepare;
    qry.Execute;
    while qry.FetchNext do
      list.add(qry.ColString[1]);
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

function TUMLSServices.locate(code : String; var message : String) : TCodeSystemProviderContext;
var
  qry : TFDBConnection;
  res : TUMLSConcept;
begin
  qry := db.GetConnection(dbprefix+'.display');
  try
    qry.SQL := 'Select STR, TTY from rxnconso where '+getCodeField+' = :code and SAB = '''+getSAB+'''';
    qry.prepare;
    qry.bindString('code', code);
    qry.execute;
    if not qry.FetchNext then
      result := nil
    else
    begin
      res := TUMLSConcept.Create;
      try
        res.FCode := code;
        repeat
          if qry.ColString[2] = 'SY' then
            res.FOthers.Add(qry.ColString[1].trim)
          else
            res.FDisplay := qry.ColString[1].trim;
        until (not qry.FetchNext);
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


function TUMLSServices.Code(context : TCodeSystemProviderContext) : string;
begin
  result := TUMLSConcept(context).FCode;
end;

function TUMLSServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

destructor TUMLSServices.Destroy;
begin
  DB.Free;
  rels.free;
  reltypes.free;
  inherited;
end;

function TUMLSServices.Display(context : TCodeSystemProviderContext; const lang : THTTPLanguages) : string;
begin
  result := TUMLSConcept(context).FDisplay.Trim;
end;

procedure TUMLSServices.Displays(context: TCodeSystemProviderContext; list: TStringList; const lang : THTTPLanguages);
begin
  list.Add(Display(context, lang));
  list.AddStrings(TUMLSConcept(context).FOthers);
end;

procedure TUMLSServices.extendLookup(factory : TFHIRFactory; ctxt: TCodeSystemProviderContext; const lang : THTTPLanguages; props: TArray<String>; resp: TFHIRLookupOpResponseW);
var
  qry : TFDBConnection;
  b : boolean;
  p : TFHIRLookupOpRespPropertyW;
begin
  if hasProp(props, 'inactive', true) then
  begin
    qry := db.GetConnection(dbprefix+'.extendLookup');
    try
      qry.SQL := 'Select suppress from rxnconso where '+getCodeField+' = :code and SAB = '''+getSAB+''' and TTY <> ''SY''';
      qry.prepare;
      qry.BindString('code', TUMLSConcept(ctxt).FCode);
      qry.execute;
      qry.FetchNext;
      b := qry.colinteger[1] = 1;
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

    if factory.version <> fhirVersionRelease2 then
    begin
      p := resp.addProp('inactive');
      p.value := factory.makeBoolean(b);
    end
    else
      resp.addExtension('inactive', b);
  end;
end;

function TUMLSServices.IsAbstract(context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // RxNorm doesn't do abstract?
end;

function TUMLSServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TUMLSServices.Link: TUMLSServices;
begin
  result := TUMLSServices(Inherited Link);
end;

class function TUMLSServices.checkDB(conn: TFDBConnection): String;
var
  meta : TFDBMetaData;
begin
  meta := conn.FetchMetaData;
  try
    if not meta.HasTable('RXNConso') or not meta.HasTable('RXNREL') or not meta.HasTable('RXNSTY') or not meta.HasTable('RXNSTEMS') then
      result := 'Missing Tables - needs re-importing'
    else
      result := 'OK ('+inttostr(conn.countSql('Select count(*) from RXNConso where SAB = '''+getSAB+''' and TTY <> ''SY'''))+' Concepts)';
  finally
    meta.free;
  end;
end;

function TUMLSServices.ChildCount(context : TCodeSystemProviderContext) : integer;
var
  qry : TFDBConnection;
begin
  qry := db.GetConnection(dbprefix+'.display');
  try
    result := qry.CountSQL('Select count(cui1) from RXNCONSO');
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

procedure TUMLSServices.getCDSInfo(card: TCDSHookCard; const lang : THTTPLanguages; baseURL, code, display: String);
begin
//    b.Append(#13#10+'This term definition is derived from SNOMED CT, which is copyright ) 2002+ International Health Terminology Standards Development Organisation (IHTSDO)'#13#10);
  card.detail := 'Not done yet';
end;

function TUMLSServices.getCodeField: String;
begin
  result := 'RXCUI';
end;

function TUMLSServices.getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.create('getcontext not supported by RXNorm'); // only used when iterating the entire code system. and RxNorm is too big
end;

function TUMLSServices.locateIsA(code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  result := nil; // todo: no sumbsumption?
end;


function TUMLSServices.prepare(prep : TCodeSystemProviderFilterPreparationContext) : boolean;
var
  sql1 : string;
  sql2 : String;
  i : integer;
  filter : TUMLSFilter;
begin
  result := false;
  if TUMLSPrep(prep).filters.Count = 0 then
    exit; // not being used

  sql1 := '';
  sql2 := 'from rxnconso';

  for i := 0 to TUMLSPrep(prep).filters.Count - 1 do
    if not TUMLSFilter(TUMLSPrep(prep).filters[i]).text then
      sql1 := sql1 + ' '+TUMLSFilter(TUMLSPrep(prep).filters[i]).sql;
  for i := 0 to TUMLSPrep(prep).filters.Count - 1 do
  begin
    if TUMLSFilter(TUMLSPrep(prep).filters[i]).text then
    begin
      sql2 := sql2 + ', rxnstems as s'+inttostr(i);
      sql1 := sql1 + ' '+TUMLSFilter(TUMLSPrep(prep).filters[i]).sql.replace('%%', inttostr(i));
    end;
  end;

  filter := TUMLSFilter(TUMLSPrep(prep).filters[0]);
  filter.sql := sql1;
  result := true;
  filter.qry := db.GetConnection(dbprefix+'.prepare');
  filter.qry.SQL := 'Select '+getCodeField+', STR '+sql2+' where SAB = '''+getSAB+''' and TTY <> ''SY'' '+filter.sql;
  filter.qry.Prepare;
  filter.qry.Execute;
end;

function TUMLSServices.searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
var
  s : String;
  i : integer;
  res : TUMLSFilter;
begin
  if prep = nil then
  begin
    // in this case, a search through the entire rxnorm. nothing to do to speed it up, and it won't be prepped
    s := '';
    for i := 0 to filter.stems.Count - 1 do
      s := s +' and RXCUI in (select CUI from rxnstems where stem like '''+SQLWrapString(filter.stems[i])+'%'')';

    res := TUMLSFilter.Create;
    try
      res.sql := s;
      result := res.link;
    finally
      res.Free;
    end;
  end
  else
  begin
    result := nil;
    for i := 0 to filter.stems.Count - 1 do
    begin
      res := TUMLSFilter.Create;
      try
        res.sql := ' and (RXCUI = s%%.CUI and s%%.stem like '''+SQLWrapString(filter.stems[i])+'%'')';
        res.text := true;
        if result = nil then
          result := res.link;
        TUMLSPrep(prep).filters.Add(res.Link);
      finally
        res.Free;
      end;
    end;
  end;
end;

function TUMLSServices.filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
var
  res : TUMLSFilter;
  ok : boolean;
begin
  res := TUMLSFilter.Create;
  try
    ok := true;
    if (op = foIn) and (prop = 'TTY') then
      res.sql := 'and TTY in ('+SQLWrapStrings(value)+')'
    else if (op <> foEqual) then
      ok := false
    else if prop = 'STY' then
      res.sql := 'and RXCUI in (select RXCUI from rxnsty where TUI = '''+SQLWrapString(value)+''')'
    else if prop = 'SAB' then
      res.sql := 'and RXCUI in (select RXCUI from rxnconso where SAB = '''+SQLWrapString(value)+'''))'
    else if prop = 'TTY' then
      res.sql := 'and TTY =  '''+SQLWrapString(value)+''''
    else if (rels.indexof(prop) > -1) and value.StartsWith('CUI:') then
      res.sql := 'and (RXCUI in (select RXCUI from rxnconso where RXCUI in (select RXCUI1 from rxnrel where '+
      'REL = '''+SQLWrapString(prop)+''' and RXCUI2 = '''+SQLWrapString(value.Substring(4))+'''))'
    else if (rels.indexof(prop) > -1) and value.StartsWith('AUI:') then
      res.sql := 'and (RXCUI in (select RXCUI from rxnconso where '+
      'RXAUI in (select RXAUI1 from rxnrel where REL = '''+SQLWrapString(prop)+''' and RXAUI2 = '''+SQLWrapString(value.Substring(4))+'''))'
    else if (reltypes.indexof(prop) > -1) and value.StartsWith('CUI:') then
      res.sql := 'and (RXCUI in (select RXCUI from rxnconso where '+
      'RXCUI in (select RXCUI1 from rxnrel where '+
      'RELA = '''+SQLWrapString(prop)+''' and RXCUI2 = '''+SQLWrapString(value.Substring(4))+'''))'
    else if (reltypes.indexof(prop) > -1) and value.StartsWith('AUI:') then
      res.sql := 'and (RXCUI in (select RXCUI from rxnconso where '+
      'RXAUI in (select RXAUI1 from rxnrel where RELA = '''+SQLWrapString(prop)+''' and RXAUI2 = '''+SQLWrapString(value.Substring(4))+'''))'
    else
      ok := false;
    if ok then
    begin
      result := res.link;
      if prep <> nil then
        TUMLSPrep(prep).filters.Add(res.Link);
    end
    else
      raise ETerminologyError.create('Unknown filter "'+prop+' '+CODES_TFhirFilterOperator[op]+' '+value+'"');
  finally
    res.Free;
  end;
end;

function TUMLSServices.filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
var
  qry : TFDBConnection;
  res : TUMLSConcept;
begin
  qry := db.GetConnection(dbprefix+'.locate');
  try
    qry.SQL := 'Select RXCUI, STR from rxnconso where SAB = '''+getSAB+'''  and TTY <> ''SY'' and RXCUI = :code '+TUMLSFilter(ctxt).sql;
    qry.prepare;
    qry.BindString('code', code);
    qry.execute;
    if not qry.FetchNext then
      result := nil
    else
    begin
      res := TUMLSConcept.Create;
      try
        res.FCode := code;
        res.FDisplay := qry.ColString[2];
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

function TUMLSServices.FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean;
var
  filter : TUMLSFilter;
begin
  filter := TUMLSFilter(ctxt);
  if (filter.qry = nil) then
  begin
    // search on full rxnorm
    filter.qry := db.GetConnection(dbprefix+'.filter');
    filter.qry.SQL := 'Select RXCUI, STR from rxnconso where SAB = '''+getSAB+''' and TTY <> ''SY'' '+filter.sql;
    filter.qry.prepare;
    filter.qry.Execute;
  end;
  result := filter.qry.FetchNext;
end;

function TUMLSServices.FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
var
  filter : TUMLSFilter;
  res : TUMLSConcept;
begin
  filter := TUMLSFilter(ctxt);
  res := TUMLSConcept.Create;
  try
    res.FCode := filter.qry.ColString[1];
    res.FDisplay := filter.qry.ColString[2];
    result := res.Link;
  finally
    res.Free;
  end;
end;

function TUMLSServices.InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyError.create('Error in internal logic - filter not prepped?');
end;

procedure TUMLSServices.Close(ctxt: TCodeSystemProviderContext);
begin
  ctxt.free;
end;

procedure TUMLSServices.Close(ctxt : TCodeSystemProviderFilterContext);
begin
  ctxt.free;
end;



procedure TUMLSServices.Close(ctxt: TCodeSystemProviderFilterPreparationContext);
var
  filter : TUMLSFilter;
begin
  for filter in TUMLSPrep(ctxt).filters do
  begin
    if filter.qry <> nil then
    begin
      filter.qry.terminate;
      filter.qry.release;
    end;
  end;
  ctxt.free;
end;

{ TUMLSPrep }

constructor TUMLSPrep.Create;
begin
  inherited;
  filters := TFslList<TUMLSFilter>.Create;
end;

destructor TUMLSPrep.Destroy;
begin
  filters.Free;
  inherited;
end;

{ TUMLSFilter }

destructor TUMLSFilter.Destroy;
begin
  if (qry <> nil) then
  begin
    qry.terminate;
    qry.Release;
  end;
  inherited;
end;

function TUMLSFilter.Link: TUMLSFilter;
begin
  result := TUMLSFilter(inherited Link);
end;

{ TUMLSConcept }

constructor TUMLSConcept.create;
begin
  inherited;
  FOthers := TStringList.Create;
end;

destructor TUMLSConcept.destroy;
begin
  FOthers.free;
  inherited;
end;

{ TRxNormServices }

constructor TRxNormServices.Create(db: TFDBManager);
begin
  inherited create(false, db);
end;

function TRxNormServices.description: String;
begin
  result := 'RxNorm';
end;

function TRxNormServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'RxNorm';
end;

function TRxNormServices.systemUri(context: TCodeSystemProviderContext): String;
begin
  result := 'http://www.nlm.nih.gov/research/umls/rxnorm';
end;

function TRxNormServices.version(context: TCodeSystemProviderContext): String;
begin
  result := '??';
end;

{ TNciMetaServices }

constructor TNciMetaServices.Create(db: TFDBManager);
begin
  inherited create(true, db);
end;

function TNciMetaServices.description: String;
begin
  result := 'NCI Metathesaurus';
end;

function TNciMetaServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'NCI Metathesaurus';
end;

function TNciMetaServices.systemUri(context: TCodeSystemProviderContext): String;
begin
  result := 'http://ncimeta.nci.nih.gov';
end;

function TNciMetaServices.version(context: TCodeSystemProviderContext): String;
begin
  result := '??';
end;

{ TNDFRTServices }

constructor TNDFRTServices.Create(db: TFDBManager);
begin
  inherited create(false, db);
end;

function TNDFRTServices.description: String;
begin
  result := 'NDFRT';
end;

function TNDFRTServices.getCodeField: String;
begin
  result := 'SCUI';
end;

class function TNDFRTServices.getSAB: String;
begin
  result := 'NDFRT';
end;

function TNDFRTServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'NDFRT';
end;

function TNDFRTServices.systemUri(context: TCodeSystemProviderContext): String;
begin
  result := 'http://hl7.org/fhir/ndfrt';
end;

function TNDFRTServices.version(context: TCodeSystemProviderContext): String;
begin
  result := '??';
end;

end.
