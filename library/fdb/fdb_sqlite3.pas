unit fdb_sqlite3;

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
  SysUtils, Classes, Contnrs, IniFiles,
  fsl_base, fsl_utilities,
  fdb_dialects, fdb_manager,
  fdb_sqlite3_objects, fdb_sqlite3_wrapper;

type
  TFDBSQLiteConnection = class (TFDBConnection)
  private
    FConnection : TSQLite3Database;
    FStatement : TSQLite3Statement;
    FColNames : TStringList;
    function ColNames : TStringList;
    function readColumn(field: String): TFDBColumn;
  Protected
    procedure StartTransactV; Override;
    procedure CommitV; Override;
    procedure RollbackV; Override;
    function FetchMetaDataV : TFDBMetaData; Override;
    function GetColCountV: Integer; Override;
    function GetColStringV(ACol: Word): String; Override;
    function GetColIntegerV(ACol: Word): Integer; Override;
    function GetColInt64V(ACol: Word): Int64; Override;
    function GetColDoubleV(ACol: Word): Double; Override;
    function GetColBlobV(ACol: Word): TBytes; Override;
    function GetColNullV(ACol: Word): Boolean; Override;
    function GetColTimestampV(ACol: Word): TTimestamp; Override;
    function GetColDateTimeExV(ACol: Word): TFslDateTime; Override;
    function GetColTypeV(ACol: Word): TFDBColumnType; Override;
    function GetColKeyV(ACol: Word): Integer; Override;
    function GetRowsAffectedV: Integer; Override;
    procedure RenameTableV(AOldTableName, ANewTableName: String); Override;
    procedure RenameColumnV(ATableName, AOldColumnName, ANewColumnName: String; AColumnDetails: String = ''); Override;
    procedure DropTableV(ATableName : String); Override;
    procedure DropColumnV(ATableName, AColumnName : String); override;
    procedure ListTablesV(AList : TStrings); override;
    procedure ClearDatabaseV; Override;
    procedure PrepareV; Override;
    procedure ExecuteV; Override;
    procedure TerminateV; Override;
    function FetchNextV: Boolean; Override;
    function ColByNameV(AColName: String): Integer; Override;
    function ColNameV(ACol: Integer): String; Override;
    procedure BindInt64V(AParamName: String; AParamValue: Int64); Override;
    procedure BindIntegerV(AParamName: String; AParamValue: Integer); Override;
    procedure BindKeyV(AParamName: String; AParamValue: Integer); Override;
    procedure BindDoubleV(AParamName: String; AParamValue: Double); Override;
    procedure BindStringV(AParamName: String; AParamValue: String); Override;
    procedure BindTimeStampV(AParamName: String; AParamValue: TTimeStamp); Override;
    procedure BindDateTimeExV(AParamName: String; AParamValue: TFslDateTime); Override;
    procedure BindBlobV(AParamName: String; AParamValue: TBytes); Override;
    procedure BindNullV(AParamName: String); Override;
    function DatabaseSizeV : int64; Override;
    Function TableSizeV(sName : String):int64; Override;
    function SupportsSizingV : Boolean; Override;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(AOwner : TFDBManager; Filename : String; autoCreate : boolean);
    destructor Destroy; override;
  end;

  TFDBSQLiteManager = class (TFDBManager)
  private
    FFilename : String;
    FAutoCreate : boolean;
  Protected
    function GetDBProvider: TFDBProvider; Override;
    function ConnectionFactory: TFDBConnection; Override;
    function GetDBPlatform: TFDBPlatform; Override;
    function GetDBDetails: String; Override;
    function GetDriver: String; Override;
    procedure init; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(AName : String; Filename : String; autoCreate : boolean; maxConn : integer = 100); overload;
    destructor Destroy; override;
    class function IsSupportAvailable(APlatform : TFDBPlatform; Var VMsg : String):Boolean; override;
  end;

implementation

{ TFDBSQLiteManager }

constructor TFDBSQLiteManager.create(AName: String; Filename : String; autoCreate : boolean; maxConn : integer = 100);
begin
  FFilename := filename;
  FAutoCreate := autoCreate;
  Inherited Create(aName, maxConn);
end;

function TFDBSQLiteManager.ConnectionFactory: TFDBConnection;
begin
  result := TFDBSQLiteConnection.Create(self, FFilename, FAutoCreate);
end;

destructor TFDBSQLiteManager.Destroy;
begin
  inherited;
end;

function TFDBSQLiteManager.GetDBDetails: String;
begin
  result := 'SQLite: '+FFIlename;
end;

function TFDBSQLiteManager.GetDBPlatform: TFDBPlatform;
begin
  result := TFDBPlatform.kdbSQLite;
end;

function TFDBSQLiteManager.GetDBProvider: TFDBProvider;
begin
  result := kdbpSQLite;
end;

function TFDBSQLiteManager.GetDriver: String;
begin
  result := 'SQLite';
end;

procedure TFDBSQLiteManager.init;
begin
  loadSQLite;
  assert(sqlite3_threadsafe>0, 'SQLite library is not threadsafe');
  if not FAutoCreate then
    if not FileExists(FFIlename) then
      raise EDBException.create('SQLite Database '+FFIlename+' not found');
end;

function TFDBSQLiteManager.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FFilename.length * sizeof(char)) + 12);
end;

class function TFDBSQLiteManager.IsSupportAvailable(APlatform: TFDBPlatform; var VMsg: String): Boolean;
begin
  result := false;
  VMsg := 'develop this bit';
end;

{ TFDBSQLiteConnection }

constructor TFDBSQLiteConnection.create(AOwner: TFDBManager; Filename : String; autoCreate : boolean);
begin
  inherited create(AOwner);
  FConnection := TSQLite3Database.Create;
  FConnection.Delay := 2000;
  if autoCreate then
    FConnection.Open(Filename, SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  else
    FConnection.Open(Filename, SQLITE_OPEN_READWRITE);
end;

destructor TFDBSQLiteConnection.Destroy;
begin
  FConnection.Free;
  FStatement.Free;
  FColNames.Free;
  inherited;
end;

procedure TFDBSQLiteConnection.BindBlobV(AParamName: String; AParamValue: TBytes);
begin
  FStatement.BindBlob(':'+AParamName, @AParamValue[0], length(AParamValue));
end;

procedure TFDBSQLiteConnection.BindDateTimeExV(AParamName: String; AParamValue: TFslDateTime);
begin
  FStatement.BindText(':'+AParamName, AParamValue.UTC.toDB);
end;

procedure TFDBSQLiteConnection.BindDoubleV(AParamName: String; AParamValue: Double);
begin
  FStatement.BindDouble(':'+AParamName, AParamValue);
end;

procedure TFDBSQLiteConnection.BindInt64V(AParamName: String; AParamValue: Int64);
begin
  FStatement.BindInt64(':'+AParamName, AParamValue);
end;

procedure TFDBSQLiteConnection.BindIntegerV(AParamName: String; AParamValue: Integer);
begin
  FStatement.BindInt(':'+AParamName, AParamValue);
end;

procedure TFDBSQLiteConnection.BindKeyV(AParamName: String; AParamValue: Integer);
begin
  FStatement.BindInt(':'+AParamName, AParamValue);
end;

procedure TFDBSQLiteConnection.BindNullV(AParamName: String);
begin
  FStatement.BindNull(':'+AParamName);
end;

procedure TFDBSQLiteConnection.BindStringV(AParamName, AParamValue: String);
begin
  FStatement.BindText(':'+AParamName, AParamValue);
end;

procedure TFDBSQLiteConnection.BindTimeStampV(AParamName: String; AParamValue: TTimeStamp);
begin
  BindDateTimeExV(AParamName, TFslDateTime.fromTS(aParamValue, dttzUnknown));
end;

procedure TFDBSQLiteConnection.ClearDatabaseV;
begin
  raise EDBTodo.create('TFDBSQLiteConnection.ClearDatabaseV');
end;

function TFDBSQLiteConnection.ColByNameV(AColName: String): Integer;
begin
  result := ColNames.indexOf(aColName)+1;
end;

function TFDBSQLiteConnection.ColNames: TStringList;
var
  i : integer;
begin
  if FColNames = nil then
  begin
    FColnames := TStringList.Create;
    for i := 0 to FStatement.ColumnCount - 1 do
      FColnames.Add(FStatement.ColumnName(i));
  end;
  result := FColNames;
end;

function TFDBSQLiteConnection.ColNameV(ACol: Integer): String;
begin
  result := ColNames[aCol];
end;

procedure TFDBSQLiteConnection.CommitV;
begin
  FConnection.Commit;
end;

function TFDBSQLiteConnection.DatabaseSizeV: int64;
begin
  result := FileSize(TFDBSQLiteManager(Owner).FFilename);
end;

procedure TFDBSQLiteConnection.DropColumnV(ATableName, AColumnName: String);
begin
  raise EDBTodo.create('TFDBSQLiteConnection.DropColumnV');
end;

procedure TFDBSQLiteConnection.DropTableV(ATableName: String);
begin
  ExecSQL('Drop table '+ATableName);
end;

procedure TFDBSQLiteConnection.ExecuteV;
begin
  if not SQL.StartsWith('Select') then
    FStatement.StepAndReset;
end;

function TFDBSQLiteConnection.readColumn(field : String): TFDBColumn;
var
  s : String;
begin
  StringSplit(field, ' ', s, field);
  result := TFDBColumn.Create(s);
  try
    StringSplit(field, ' ', s, field);
    s := s.ToUpper;
    if (s.Contains('INT')) then
      result.DataType := ctInteger
    else if (s.Contains('DATETEXT')) then
      result.DataType := ctDateTime
    else if (s.Contains('CHAR')) or (s.Contains('CLOB')) or (s.Contains('TEXT')) then
    begin
      result.DataType := ctChar;
      if (s.Contains('(')) and (s.Contains(')'))  then
      begin
        s := s.Substring(s.IndexOf('(')+1);
        s := s.Substring(0, s.IndexOf(')'));
        result.Length := StrToIntDef(s, 0);
      end;
    end
    else if (s.Contains('BLOB')) or (s = '') then
      result.DataType := ctBlob
    else if (s.Contains('REAL')) or (s.Contains('FLOA')) or (s.Contains('DOUB')) then
      result.DataType := ctFloat
    else
      result.DataType := ctNumeric;
    s := field.Trim.ToLower;
    result.Nullable := s <> 'not null';
    result.Link;
  finally
    result.Free;
  end;
end;

function TFDBSQLiteConnection.FetchMetaDataV: TFDBMetaData;
var
  tbl : TFDBTable;
  s : String;
  a : TArray<String>;
begin
  result := TFDBMetaData.Create;
  try
    sql := 'SELECT name, sql FROM sqlite_master WHERE type=''table''';
    prepare;
    execute;
    while fetchnext do
    begin
      tbl := TFDBTable.Create;
      try
        tbl.Name := ColStringByName['name'];
        s := ColStringByName['sql'];
        s := s.subString(s.indexof('(')+1);
        s := s.subString(0, s.LastIndexOf(')'));
        s := s.Replace(#13, ' ').Replace(#10, ' ').Replace(#9, ' ').Replace('  ', ' ').trim;
        a := s.split([',']);
        for s in a do
          if not s.Trim.StartsWith('CONSTRAINT') then
            tbl.Columns.Add(readColumn(s.trim));
        result.Tables.Add(tbl.Link);
      finally
        tbl.Free;
      end;
    end;
    terminate;
    // todo: indexes... :SELECT sql FROM SQLite_master WHERE type = 'index'
    result.Link;
  finally
    result.Free;
  end;
end;

function TFDBSQLiteConnection.FetchNextV: Boolean;
begin
  result := FStatement.Step = SQLITE_ROW;
end;

function TFDBSQLiteConnection.GetColBlobV(ACol: Word): TBytes;
begin
  SetLength(result, FStatement.ColumnBytes(ACol-1));
  if length(result) > 0 then
    move(FStatement.ColumnBlob(aCol-1)^, result[0], length(result));
end;

function TFDBSQLiteConnection.GetColCountV: Integer;
begin
  result := FStatement.ColumnCount;
end;

function TFDBSQLiteConnection.GetColDateTimeExV(ACol: Word): TFslDateTime;
begin
  result := TFslDateTime.fromDB(getColStringV(ACol), dttzUTC);
end;

function TFDBSQLiteConnection.GetColDoubleV(ACol: Word): Double;
begin
  result := FStatement.ColumnDouble(ACol-1);
end;

function TFDBSQLiteConnection.GetColInt64V(ACol: Word): Int64;
begin
  result := FStatement.ColumnInt64(ACol-1);
end;

function TFDBSQLiteConnection.GetColIntegerV(ACol: Word): Integer;
begin
  result := FStatement.ColumnInt(ACol-1);
end;

function TFDBSQLiteConnection.GetColKeyV(ACol: Word): Integer;
begin
  result := FStatement.ColumnInt(ACol-1);
end;

function TFDBSQLiteConnection.GetColNullV(ACol: Word): Boolean;
begin
  result := FStatement.ColumnNull(ACol-1);
end;

function TFDBSQLiteConnection.GetColStringV(ACol: Word): String;
begin
  result := FStatement.ColumnText(ACol-1);
end;

function TFDBSQLiteConnection.GetColTimestampV(ACol: Word): TTimestamp;
begin
  result := GetColDateTimeExV(ACol).Local.TimeStamp;
end;

function TFDBSQLiteConnection.GetColTypeV(ACol: Word): TFDBColumnType;
begin
  raise EDBTodo.create('TFDBSQLiteConnection.GetColTypeV');
end;

function TFDBSQLiteConnection.GetRowsAffectedV: Integer;
begin
  result := FConnection.RowsAffected;
end;

procedure TFDBSQLiteConnection.ListTablesV(AList: TStrings);
begin
  raise EDBTodo.create('TFDBSQLiteConnection.ListTablesV');
end;

procedure TFDBSQLiteConnection.PrepareV;
begin
  FStatement := FConnection.Prepare(SQL);
end;

procedure TFDBSQLiteConnection.RenameColumnV(ATableName, AOldColumnName, ANewColumnName, AColumnDetails: String);
begin
  raise EDBTodo.create('TFDBSQLiteConnection.RenameColumnV');
end;

procedure TFDBSQLiteConnection.RenameTableV(AOldTableName, ANewTableName: String);
begin
  raise EDBTodo.create('TFDBSQLiteConnection.RenameTableV');
end;

procedure TFDBSQLiteConnection.RollbackV;
begin
  FConnection.Rollback;
end;

procedure TFDBSQLiteConnection.StartTransactV;
begin
  FConnection.BeginTransaction;
end;

function TFDBSQLiteConnection.SupportsSizingV: Boolean;
begin
  result := true;
end;

function TFDBSQLiteConnection.TableSizeV(sName: String): int64;
begin
  raise EDBTodo.create('TFDBSQLiteConnection.TableSizeV');
end;

procedure TFDBSQLiteConnection.TerminateV;
begin
  FreeAndNil(FStatement);
  FreeAndNil(FColNames);
end;

function TFDBSQLiteConnection.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FColNames.sizeInBytes);
end;

end.

