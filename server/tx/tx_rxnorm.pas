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
  fsl_base, fsl_utilities, fsl_http, fsl_threads, fsl_lang, fsl_logging, fsl_i18n,
  fdb_manager, fdb_dialects,
  fhir_objects, fhir_common, fhir_factory, fhir_utilities, fhir_features, fhir_uris,
  fhir_cdshooks,
  ftx_service;

type
  TUMLSConcept = class (TCodeSystemProviderContext)
  private
    FArchived : boolean;
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
    FVersion : String;

    procedure load(list : TStringList; sql : String);
  protected
    class function getSAB : String; virtual;
    function getCodeField : String; virtual;
  public
    constructor Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport; nci : boolean; db : TFDBManager);
    destructor Destroy; Override;
    Function Link : TUMLSServices; overload;

    class function checkDB(conn : TFDBConnection) : String;

    function TotalCount : integer;  override;
    function getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext; override;
    function getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext; override;
    function getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String; override;
    function getDefinition(opContext : TTxOperationContext; code : String):String; override;
    function locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext; override;
    function locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext; override;
    function sameContext(opContext : TTxOperationContext; a, b : TCodeSystemProviderContext) : boolean; override;
    function IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; override;
    function Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;
    function Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string; override;
    procedure Designations(opContext : TTxOperationContext; context : TCodeSystemProviderContext; list : TConceptDesignations); override;
    function Definition(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string; override;
    function version : String; override;
    function IsInactive(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean; override;

    function getPrepContext(opContext : TTxOperationContext) : TCodeSystemProviderFilterPreparationContext; override;
    function prepare(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext) : boolean; override;

    function searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; override;
    function filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function filterSize(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : integer; override;
    function FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    function InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function isNotClosed(opContext : TTxOperationContext; textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure getCDSInfo(opContext : TTxOperationContext; card : TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display : String); override;
    procedure extendLookup(opContext : TTxOperationContext; factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; langList : THTTPLanguageList; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;
    //function subsumes(codeA, codeB : String) : String; override;

    procedure defineFeatures(opContext : TTxOperationContext; features : TFslList<TFHIRFeature>); override;
  end;

  TRxNormServices = class (TUMLSServices)
  public
    constructor Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport; db : TFDBManager);
    function systemUri : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function description : String; override;
  end;

  TNDFRTServices = class (TUMLSServices)
  protected
    class function getSAB : String; override;
    function getCodeField : String; override;
  public
    constructor Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport; db : TFDBManager);
    function systemUri : String; override;
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
    procedure loadRXNSAB(callback : TWorkProgressEvent);
    procedure loadRXNCUI(callback : TWorkProgressEvent);
    procedure loadRXNArchive(callback : TWorkProgressEvent);
    procedure loadRXNRel(callback : TWorkProgressEvent);
    procedure loadRXNSty(callback : TWorkProgressEvent);
  public
    constructor Create(folder : String; conn : TFDBConnection);
    destructor Destroy; override;

    procedure Doinstall(sender : TObject; context: TObject; callback : TWorkProgressEvent);
  end;


implementation
{ TUMLSImporter }

constructor TUMLSImporter.Create(folder: String; conn: TFDBConnection);
begin
  inherited Create;
  FFolder := folder;
  FCOnn := conn;
end;

destructor TUMLSImporter.Destroy;
begin
  FConn.free;
  inherited Destroy;
end;

procedure TUMLSImporter.CheckFiles;
begin
  if not FileExists(path([FFolder, 'RXNCONSO.RRF'])) then
    raise EFslException.Create('File not found: RXNCONSO.RRF');
  if not FileExists(path([FFolder, 'RXNREL.RRF'])) then
    raise EFslException.Create('File not found: RXNREL.RRF');
  if not FileExists(path([FFolder, 'RXNSTY.RRF'])) then
    raise EFslException.Create('File not found: RXNSTY.RRF');
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
    if meta.HasTable('RXNATOMARCHIVE') then  FConn.ExecSQL('Drop table RXNATOMARCHIVE');
    if meta.HasTable('RXNCUI') then  FConn.ExecSQL('Drop table RXNCUI');
    if meta.HasTable('RXNSAB') then  FConn.ExecSQL('Drop table RXNSAB');
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
  FConn.ExecSQL('CREATE INDEX idx_rxnconso_sab_tty_rxcui ON RXNCONSO(SAB, TTY, RXCUI)');

  callback(self, 10, false, 'Create table RXNREL (Step 1 of 5)');
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
  FConn.ExecSQL('CREATE INDEX idx_rxnrel_rel ON RXNREL(REL)');
  FConn.ExecSQL('CREATE INDEX idx_rxnrel_rela ON RXNREL(RELA)');


  callback(self, 20, false, 'Create table RXNSTY (Step 1 of 5)');
  FConn.ExecSQL('CREATE TABLE RXNSTY ( '+
                '  RXCUI          varchar(8) NOT NULL, '+
                '  TUI            varchar (4) '+
                ') ');
  FConn.ExecSQL('CREATE INDEX X_RXNSTY_2 ON RXNSTY(TUI)');


  FConn.ExecSQL('CREATE TABLE RXNATOMARCHIVE   '+
'(  '+
'   RXAUI             varchar(8) NOT NULL, '+
'   AUI               varchar(10), '+
'   STR               varchar(4000) NOT NULL, '+
'   ARCHIVE_TIMESTAMP varchar(280) NOT NULL, '+
'   CREATED_TIMESTAMP varchar(280) NOT NULL, '+
'   UPDATED_TIMESTAMP varchar(280) NOT NULL, '+
'   CODE              varchar(50), '+
'   IS_BRAND          varchar(1), '+
'   LAT               varchar(3), '+
'   LAST_RELEASED     varchar(30), '+
'   SAUI              varchar(50), '+
'   VSAB              varchar(40), '+
'   RXCUI             varchar(8), '+
'   SAB               varchar(20), '+
'   TTY               varchar(20), '+
'   MERGED_TO_RXCUI   varchar(8) , '+
                'PRIMARY KEY (RXAUI))' );
  callback(self, 37, false, 'Create table RXNSTEMS (Step 1 of 5)');
  FConn.ExecSQL('CREATE TABLE RXNSTEMS ( '+
                ' stem CHAR(20) NOT NULL, '+
                ' CUI VARCHAR(8) NOT NULL, '+
                'PRIMARY KEY (stem, CUI))');
  callback(self, 54, false, 'Created Tables (Step 1 of 5)');


  callback(self, 71, false, 'Created Tables (Step 1 of 5)');

  FConn.ExecSQL('CREATE TABLE RXNSAB  '+
  '( '+
  '   VCUI           varchar (8), '+
  '   RCUI           varchar (8), '+
  '   VSAB           varchar (40), '+
  '   RSAB           varchar (20) NOT NULL, '+
  '   SON            varchar (3000), '+
  '   SF             varchar (20), '+
  '   SVER           varchar (20), '+
  '   VSTART         varchar (10), '+
  '   VEND           varchar (10), '+
  '   IMETA          varchar (10), '+
  '   RMETA          varchar (10), '+
  '   SLC            varchar (1000), '+
  '   SCC            varchar (1000), '+
  '   SRL            integer, '+
  '   TFR            integer, '+
  '   CFR            integer, '+
  '   CXTY           varchar (50), '+
  '   TTYL           varchar (300), '+
  '   ATNL           varchar (1000), '+
  '   LAT            varchar (3), '+
  '   CENC           varchar (20), '+
  '   CURVER         varchar (1), '+
  '   SABIN          varchar (1), '+
  '   SSN            varchar (3000), '+
  '   SCIT           varchar (4000) , '+
                'PRIMARY KEY (VCUI))');     
  callback(self, 88, false, 'Created Tables (Step 1 of 5)');

 FConn.ExecSQL('CREATE TABLE RXNCUI ( '+
 'cui1 VARCHAR(8), '+
 'ver_start VARCHAR(40), '+
 'ver_end   VARCHAR(40), '+
 'cardinality VARCHAR(8), '+
 'cui2       VARCHAR(8) , '+
                'PRIMARY KEY (cui1))');    
  callback(self, 100, false, 'Created Tables (Step 1 of 5)');
end;

procedure TUMLSImporter.loadRXNCONSO(callback: TWorkProgressEvent);
var
  ts : TStringList;
  s : TArray<string>;
  i : integer;
begin
  callback(self, 0, false, 'Load RXNCONSO (Step 2 of 5)');
  ts := TStringList.Create;
  try
    ts.LoadFromFile(path([FFolder, 'RXNCONSO.RRF']));
    if FConn.Owner.Platform = kdbSQLite then
      FConn.StartTransact;
    try
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
      if FConn.Owner.Platform = kdbSQLite then
        FConn.Commit;
    end;
  finally
    ts.free;
  end;
  callback(self, 100, false, 'RXNCONSO Loaded (Step 2 of 5)');
end;

procedure TUMLSImporter.loadRXNSAB(callback: TWorkProgressEvent);
var
  ts : TStringList;
  s : TArray<string>;
  i : integer;
begin
  callback(self, 0, false, 'Load RXNSAB (Step 2 of 5)');
  ts := TStringList.Create;
  try
    ts.LoadFromFile(path([FFolder, 'RXNSAB.RRF']));
    if FConn.Owner.Platform = kdbSQLite then
      FConn.StartTransact;
    try
      FConn.sql := 'insert into RXNSAB (VCUI, RCUI, VSAB, RSAB, SON, SF, SVER, VSTART, VEND, IMETA, RMETA, SLC, SCC, SRL, TFR, CFR, CXTY, TTYL, ATNL, LAT, CENC, CURVER, SABIN, SSN, SCIT)'+
         ' values (:VCUI, :RCUI, :VSAB, :RSAB, :SON, :SF, :SVER, :VSTART, :VEND, :IMETA, :RMETA, :SLC, :SCC, :SRL, :TFR, :CFR, :CXTY, :TTYL, :ATNL, :LAT, :CENC, :CURVER, :SABIN, :SSN, :SCIT)';
      FConn.Prepare;
      for i := 0 to ts.count - 1 do
      begin
        if (i mod 135 = 0) then
          callback(self, trunc((i / ts.count) * 100), false, 'Load RXNSAB line '+inttostr(i)+' (Step 2 of 5)');

        s := ts[i].split(['|']);
        FConn.BindString('VCUI', s[0]);
        FConn.BindString('RCUI', s[1]);
        FConn.BindString('VSAB', s[2]);
        FConn.BindString('RSAB', s[3]);
        FConn.BindString('SON', s[4]);
        FConn.BindString('SF', s[5]);
        FConn.BindString('SVER', s[6]);
        FConn.BindString('VSTART', s[7]);
        FConn.BindString('VEND', s[8]);
        FConn.BindString('IMETA', s[9]);
        FConn.BindString('RMETA', s[10]);
        FConn.BindString('SLC', s[11]);
        FConn.BindString('SCC', s[12]);
        FConn.BindString('SRL', s[13]);
        FConn.BindString('TFR', s[14]);
        FConn.BindString('CFR', s[15]);
        FConn.BindString('CXTY', s[16]);
        FConn.BindString('TTYL', s[17]);
        FConn.BindString('ATNL', s[18]);
        FConn.BindString('LAT', s[19]);
        FConn.BindString('CENC', s[20]);
        FConn.BindString('CURVER', s[21]);
        FConn.BindString('SABIN', s[22]);
        FConn.BindString('SSN', s[23]);
        FConn.BindString('SCIT', s[24]);
        FConn.Execute;
      end;
      FConn.Terminate;
    finally
      if FConn.Owner.Platform = kdbSQLite then
        FConn.Commit;
    end;
  finally
    ts.free;
  end;
  callback(self, 100, false, 'RXNSAB Loaded (Step 2 of 5)');
end;

procedure TUMLSImporter.loadRXNCUI(callback: TWorkProgressEvent);
var
  ts : TStringList;
  s : TArray<string>;
  i : integer;
begin
  callback(self, 0, false, 'Load RXNCUI (Step 2 of 5)');
  ts := TStringList.Create;
  try
    ts.LoadFromFile(path([FFolder, 'RXNCUI.RRF']));
    if FConn.Owner.Platform = kdbSQLite then
      FConn.StartTransact;
    try
      FConn.sql := 'insert into RXNCUI (cui1, ver_start, ver_end, cardinality, cui2) values (:cui1, :ver_start, :ver_end, :cardinality, :cui2)';
      FConn.Prepare;
      for i := 0 to ts.count - 1 do
      begin
        if (i mod 135 = 0) then
          callback(self, trunc((i / ts.count) * 100), false, 'Load RXNCUI line '+inttostr(i)+' (Step 2 of 5)');

        s := ts[i].split(['|']);
        FConn.BindString('cui1', s[0]);
        FConn.BindString('ver_start', s[1]);
        FConn.BindString('ver_end', s[2]);
        FConn.BindString('cardinality', s[3]);
        FConn.BindString('cui2', s[4]);
        FConn.Execute;
      end;
      FConn.Terminate;
    finally
      if FConn.Owner.Platform = kdbSQLite then
        FConn.Commit;
    end;
  finally
    ts.free;
  end;
  callback(self, 100, false, 'RXNCUI Loaded (Step 2 of 5)');
end;

procedure TUMLSImporter.loadRXNArchive(callback: TWorkProgressEvent);
var
  ts : TStringList;
  s : TArray<string>;
  i : integer;
begin
  callback(self, 0, false, 'Load RXNATOMARCHIVE (Step 2 of 5)');
  ts := TStringList.Create;
  try
    ts.LoadFromFile(path([FFolder, 'RXNATOMARCHIVE.RRF']));
    if FConn.Owner.Platform = kdbSQLite then
      FConn.StartTransact;
    try

      FConn.sql := 'insert into RXNATOMARCHIVE (RXAUI, AUI, STR, ARCHIVE_TIMESTAMP, CREATED_TIMESTAMP, UPDATED_TIMESTAMP, CODE, IS_BRAND, LAT, LAST_RELEASED, SAUI, VSAB, RXCUI, SAB, TTY, MERGED_TO_RXCUI) '+
                              ' values (:RXAUI, :AUI, :STR, :ARCHIVE_TIMESTAMP, :CREATED_TIMESTAMP, :UPDATED_TIMESTAMP, :CODE, :IS_BRAND, :LAT, :LAST_RELEASED, :SAUI, :VSAB, :RXCUI, :SAB, :TTY, :MERGED_TO_RXCUI)';
      FConn.Prepare;
      for i := 0 to ts.count - 1 do
      begin
        if (i mod 135 = 0) then
          callback(self, trunc((i / ts.count) * 100), false, 'Load RXNATOMARCHIVE line '+inttostr(i)+' (Step 2 of 5)');

        s := ts[i].split(['|']);
        FConn.BindString('RXAUI', s[0]);
        FConn.BindString('AUI', s[1]);
        FConn.BindString('STR', s[2]);
        FConn.BindString('ARCHIVE_TIMESTAMP', s[3]);
        FConn.BindString('CREATED_TIMESTAMP', s[4]);
        FConn.BindString('UPDATED_TIMESTAMP', s[5]);
        FConn.BindString('CODE', s[6]);
        FConn.BindString('IS_BRAND', s[7]);
        FConn.BindString('LAT', s[8]);
        FConn.BindString('LAST_RELEASED', s[9]);
        FConn.BindString('SAUI', s[10]);
        FConn.BindString('VSAB', s[11]);
        FConn.BindString('RXCUI', s[12]);
        FConn.BindString('SAB', s[13]);
        FConn.BindString('TTY', s[14]);
        FConn.BindString('MERGED_TO_RXCUI', s[15]);
        FConn.Execute;
      end;
      FConn.Terminate;
    finally
      if FConn.Owner.Platform = kdbSQLite then
        FConn.Commit;
    end;
  finally
    ts.free;
  end;
  callback(self, 100, false, 'RXNATOMARCHIVE Loaded (Step 2 of 5)');
end;

procedure TUMLSImporter.loadRXNRel(callback: TWorkProgressEvent);
var
  ts : TStringList;
  s : TArray<string>;
  i : integer;
begin
  callback(self, 0, false, 'Load RXNREL (Step 3 of 5)');
  ts := TStringList.Create;
  try
    ts.LoadFromFile(path([FFolder, 'RXNREL.RRF']));
    if FConn.Owner.Platform = kdbSQLite then
      FConn.StartTransact;
    try
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
      if FConn.Owner.Platform = kdbSQLite then
        FConn.Commit;
    end;
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
  ts := TStringList.Create;
  try
    ts.LoadFromFile(path([FFolder, 'RXNSTY.RRF']));
    if FConn.Owner.Platform = kdbSQLite then
      FConn.StartTransact;
    try
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
      if FConn.Owner.Platform = kdbSQLite then
        FConn.Commit;
    end;
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

  stemmer := TFslWordStemmer.Create('english');
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

    if FConn.Owner.Platform = kdbSQLite then
      FConn.StartTransact;
    try
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
    finally
      if FConn.Owner.Platform = kdbSQLite then
        FConn.Commit;
    end;
  finally
    for i := 0 to stems.Count - 1 do
      stems.Objects[i].free;
    stems.free;
    stemmer.free;
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

procedure TUMLSImporter.Doinstall(sender: TObject; context: TObject; callback: TWorkProgressEvent);
begin
  callback(self, 1, false, 'Checking');
  CheckFiles;
  callback(self, 1, false, 'Creating Tables');
  CreateTables(callback);
  loadRXNSAB(callback);
  loadRXNArchive(callback);
  loadRXNCUI(callback);
  loadRXNCONSO(callback);
  loadRXNRel(callback);
  loadRXNSty(callback);
  makeStems(callback);
  callback(self, 1, true, 'Finished importing');
end;

function readVersion(db : TFDBManager) : String;
var
  d : String;
begin
  d := db.DBDetails;
  if (d.Contains('.db')) then
  begin
    d := d.Substring(0, d.IndexOf('.db'));
    if (d.Contains('_')) then
      d := d.Substring(d.LastIndexOf('_')+1);
  end;
  if (IsNumericString(d)) then
    result := d
  else
    result := '??';
end;

{ TUMLSServices }

constructor TUMLSServices.Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport; nci: boolean; db: TFDBManager);
begin
  inherited Create(Languages, i18n);

  self.nci := nci;
  if (nci) then
    dbprefix := 'NciMeta'
  else
    dbprefix := 'RxNorm';
  self.db := db;
  try
    FVersion := inttostr(db.CountSQL('select version from RXNVer', 'Version'));
  except
    FVersion := readVersion(db);
  end;
  rels := TStringList.Create;
  reltypes := TStringList.Create;

  Logging.log('Load RxNorm metadata #1');
  if (TotalCount = 0) then
    raise EDBException.Create('Error Connecting to RxNorm');
  Logging.log('Load RxNorm metadata #2');
  load(rels, 'select distinct REL from RXNREL');
  Logging.log('Load RxNorm metadata #3');
  load(reltypes, 'select distinct RELA from RXNREL');
  Logging.log('Load RxNorm metadata #4');
end;

procedure TUMLSServices.defineFeatures(opContext : TTxOperationContext; features: TFslList<TFHIRFeature>);
begin
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'TTY:in'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'STY:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'SAB:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'TTY:equals'));
  features.Add(TFHIRFeature.fromString('rest.Codesystem:'+systemUri+'.filter', 'CUI:equals'));
end;

function TUMLSServices.TotalCount : integer;
var
  qry : TFDBConnection;
begin
  qry := db.GetConnection(dbprefix+'.Count');
  try
    qry.SQL := 'Select Count(RXCUI) from rxnconso where SAB = '''+getSAB+''' and TTY <> ''SY''';
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


function TUMLSServices.getDefinition(opContext : TTxOperationContext; code: String): String;
begin
  result := '';
end;

function TUMLSServices.getDisplay(opContext : TTxOperationContext; code : String; langList : THTTPLanguageList):String;
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

function TUMLSServices.getPrepContext(opContext : TTxOperationContext): TCodeSystemProviderFilterPreparationContext;
begin
  result := TUMLSPrep.Create;
end;

class function TUMLSServices.getSAB: String;
begin
  result := 'RXNORM';
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

function TUMLSServices.locate(opContext : TTxOperationContext; code : String; altOpt : TAlternateCodeOptions; var message : String) : TCodeSystemProviderContext;
var
  qry : TFDBConnection;
  res : TUMLSConcept;
  found : boolean;
  archive : boolean;
begin

  result := nil;
  archive := false;
  qry := db.GetConnection(dbprefix+'.display');
  try
    qry.SQL := 'Select STR, TTY from rxnconso where '+getCodeField+' = :code and SAB = '''+getSAB+'''';
    qry.prepare;
    qry.bindString('code', code);
    qry.execute;
    found := qry.FetchNext;
    if not found then
    begin
      qry.Terminate;  qry.SQL := 'Select STR, TTY from RXNATOMARCHIVE where '+getCodeField+' = :code and SAB = '''+getSAB+'''';
      qry.prepare;
      qry.bindString('code', code);
      qry.execute;
      found := qry.FetchNext;
      archive := true;
    end;
    if found then
    begin
      res := TUMLSConcept.Create;
      try
        res.FArchived := archive;
        res.FCode := code;
        repeat
          if (qry.ColString[2] = 'SY') or (res.FDisplay <> '') then
            res.FOthers.Add(qry.ColString[1].trim)
          else
            res.FDisplay := qry.ColString[1].trim;
        until (not qry.FetchNext);
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


function TUMLSServices.Code(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : string;
begin
  result := TUMLSConcept(context).FCode;
end;

function TUMLSServices.Definition(opContext : TTxOperationContext; context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

function TUMLSServices.version: String;
begin
  Result := FVersion;
end;

function TUMLSServices.IsInactive(opContext: TTxOperationContext; context: TCodeSystemProviderContext): boolean;
var
  qry : TFDBConnection;
begin
  if TUMLSConcept(context).FArchived then
    result := true
  else
  begin
    qry := db.GetConnection(dbprefix+'.extendLookup');
    try
      qry.SQL := 'Select suppress from rxnconso where '+getCodeField+' = :code and SAB = '''+getSAB+''' and TTY <> ''SY''';
      qry.prepare;
      qry.BindString('code', TUMLSConcept(context).FCode);
      qry.execute;
      qry.FetchNext;
      result := qry.colinteger[1] = 1;
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
end;

destructor TUMLSServices.Destroy;
begin
  DB.free;
  rels.free;
  reltypes.free;
  inherited;
end;

function TUMLSServices.Display(opContext : TTxOperationContext; context : TCodeSystemProviderContext; langList : THTTPLanguageList) : string;
begin
  result := TUMLSConcept(context).FDisplay.Trim;
end;

procedure TUMLSServices.Designations(opContext : TTxOperationContext; context: TCodeSystemProviderContext; list: TConceptDesignations);
begin
  list.addDesignation(true, true, '', '', Display(opContext, context, nil));
  list.addDesignation(false, true, '', '', TUMLSConcept(context).FOthers);
end;

procedure TUMLSServices.extendLookup(opContext : TTxOperationContext; factory : TFHIRFactory; ctxt: TCodeSystemProviderContext; langList : THTTPLanguageList; props: TArray<String>; resp: TFHIRLookupOpResponseW);
var
  qry : TFDBConnection;
  b : boolean;
  p : TFHIRLookupOpRespPropertyW;
  list: TConceptDesignations;
  cd : TConceptDesignation;
begin
  list := TConceptDesignations.Create(Factory.link, FLanguages.link);
  try
    Designations(opContext, ctxt, list);
    for cd in list.designations do
    begin
      p := resp.addProp('other.display');
      p.value := factory.makeString(cd.value.AsString);
    end;
  finally
    list.free;
  end;
end;

function TUMLSServices.IsAbstract(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : boolean;
begin
  result := false;  // RxNorm doesn't do abstract?
end;

function TUMLSServices.isNotClosed(opContext : TTxOperationContext; textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
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

function TUMLSServices.getIterator(opContext : TTxOperationContext; context : TCodeSystemProviderContext) : TCodeSystemIteratorContext;
var
  qry : TFDBConnection;
begin
  qry := db.GetConnection(dbprefix+'.getIterator');
  try
    result := TCodeSystemIteratorContext.Create(nil, qry.CountSQL('Select count(cui1) from RXNCONSO'));
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

procedure TUMLSServices.getCDSInfo(opContext : TTxOperationContext; card: TCDSHookCard; langList : THTTPLanguageList; baseURL, code, display: String);
begin
//    b.Append(#13#10+'This term definition is derived from SNOMED CT, which is copyright ) 2002+ International Health Terminology Standards Development Organisation (IHTSDO)'#13#10);
  card.detail := 'Not done yet';
end;

function TUMLSServices.getCodeField: String;
begin
  result := 'RXCUI';
end;

function TUMLSServices.getNextContext(opContext : TTxOperationContext; context : TCodeSystemIteratorContext) : TCodeSystemProviderContext;
begin
  raise ETerminologyError.Create('getNextContext not supported by RXNorm', itException); // only used when iterating the entire code system. and RxNorm is too big
end;

function TUMLSServices.locateIsA(opContext : TTxOperationContext; code, parent : String; disallowParent : boolean = false) : TCodeSystemProviderContext;
begin
  result := nil; // todo: no sumbsumption?
end;


function TUMLSServices.prepare(opContext : TTxOperationContext; prep : TCodeSystemProviderFilterPreparationContext) : boolean;
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

function TUMLSServices.sameContext(opContext : TTxOperationContext; a, b: TCodeSystemProviderContext): boolean;
begin
  result := (a is TUMLSConcept) and (b is TUMLSConcept) and ((a as TUMLSConcept).FCode = (b as TUMLSConcept).FCode);
end;

function TUMLSServices.searchFilter(opContext : TTxOperationContext; filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext;
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
      res.free;
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
        res.free;
      end;
    end;
  end;
end;

function TUMLSServices.filter(opContext : TTxOperationContext; forExpansion, forIteration : boolean; prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext;
var
  res : TUMLSFilter;
  ok : boolean;
begin
  SetThreadStatus(ClassName+'.filter('+prop+CODES_TFhirFilterOperator[op]+value+')');
  prop := prop.toUpper;
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
      raise ETerminologyError.Create('Unknown filter "'+prop+' '+CODES_TFhirFilterOperator[op]+' '+value+'"', itInvalid);
  finally
    res.free;
  end;
end;

function TUMLSServices.filterLocate(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext;
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

function TUMLSServices.FilterMore(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext) : boolean;
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

function TUMLSServices.filterSize(opContext : TTxOperationContext; ctxt: TCodeSystemProviderFilterContext): integer;
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
  result := filter.qry.RowsAffected; // todo: check this
end;

function TUMLSServices.FilterConcept(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
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
    res.free;
  end;
end;

function TUMLSServices.InFilter(opContext : TTxOperationContext; ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean;
begin
  raise ETerminologyError.Create('Error in internal logic - filter not prepped?', itException);
end;


{ TUMLSPrep }

constructor TUMLSPrep.Create;
begin
  inherited;
  filters := TFslList<TUMLSFilter>.Create;
end;

destructor TUMLSPrep.Destroy;
var
  filter : TUMLSFilter;
begin
  for filter in filters do
  begin
    if filter.qry <> nil then
    begin
      filter.qry.terminate;
      filter.qry.release;
      filter.qry := nil;
    end;
  end;
  filters.free;
  inherited;
end;

{ TUMLSFilter }

destructor TUMLSFilter.Destroy;
begin
  if (qry <> nil) then
  begin
    qry.terminate;
    qry.Release;
    qry := nil;
  end;
  inherited;
end;

function TUMLSFilter.Link: TUMLSFilter;
begin
  result := TUMLSFilter(inherited Link);
end;

{ TUMLSConcept }

constructor TUMLSConcept.Create;
begin
  inherited;
  FOthers := TStringList.Create;
end;

destructor TUMLSConcept.Destroy;
begin
  FOthers.free;
  inherited;
end;

{ TRxNormServices }

constructor TRxNormServices.Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport; db: TFDBManager);
begin
  inherited Create(languages, i18n, false, db);
end;

function TRxNormServices.description: String;
begin
  result := 'RxNorm';
end;

function TRxNormServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'RxNorm';
end;

function TRxNormServices.systemUri: String;
begin
  result := URI_RXNORM;
end;

{ TNDFRTServices }

constructor TNDFRTServices.Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport; db: TFDBManager);
begin
  inherited Create(languages, i18n, false, db);
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

function TNDFRTServices.systemUri: String;
begin
  result := URI_NDFRT;
end;

end.
