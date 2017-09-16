unit KDBSQLite;

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
  SysUtils, Classes, Contnrs, IniFiles,
  FileSupport,
  AdvObjects, StringSupport, AdvExceptions, AdvGenerics,
  DateSupport, KDBDialects, KDBManager, KSettings,
  SQLite3, SQLite3Wrap;

type
  TKDBSQLiteConnection = class (TKDBConnection)
  private
    FConnection : TSQLite3Database;
    FStatement : TSQLite3Statement;
    FColNames : TStringList;
    function ColNames : TStringList;
    function readColumn(field: String): TKDBColumn;
  Protected
    procedure StartTransactV; Override;
    procedure CommitV; Override;
    procedure RollbackV; Override;
    function FetchMetaDataV : TKDBMetaData; Override;
    function GetColCountV: Integer; Override;
    function GetColStringV(ACol: Word): String; Override;
    function GetColIntegerV(ACol: Word): Integer; Override;
    function GetColInt64V(ACol: Word): Int64; Override;
    function GetColDoubleV(ACol: Word): Double; Override;
    function GetColBlobV(ACol: Word): TBytes; Override;
    function GetColNullV(ACol: Word): Boolean; Override;
    function GetColTimestampV(ACol: Word): TTimestamp; Override;
    function GetColDateTimeExV(ACol: Word): TDateTimeEx; Override;
    function GetColTypeV(ACol: Word): TKDBColumnType; Override;
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
    procedure BindDateTimeExV(AParamName: String; AParamValue: TDateTimeEx); Override;
    procedure BindBlobV(AParamName: String; AParamValue: TBytes); Override;
    procedure BindNullV(AParamName: String); Override;
    function DatabaseSizeV : int64; Override;
    Function TableSizeV(sName : String):int64; Override;
    function SupportsSizingV : Boolean; Override;
  Public
    constructor create(AOwner : TKDBManager; Filename : String; autoCreate : boolean);
    destructor Destroy; override;
  end;

  TKDBSQLiteManager = class (TKDBManager)
  private
    FFilename : String;
    FAutoCreate : boolean;
  Protected
    function GetDBProvider: TKDBProvider; Override;
    function ConnectionFactory: TKDBConnection; Override;
    function GetDBPlatform: TKDBPlatform; Override;
    function GetDBDetails: String; Override;
    function GetDriver: String; Override;
    procedure init; override;
  public
    constructor create(AName : String; Filename : String; autoCreate : boolean; maxConn : integer = 100); overload;
    constructor create(AName : String; ASettings : TSettingsAdapter; AIdent : String = ''); overload; override;
    destructor Destroy; override;
    procedure SaveSettings(ASettings : TSettingsAdapter); override;
    class function IsSupportAvailable(APlatform : TKDBPlatform; Var VMsg : String):Boolean; override;
  end;

implementation

{ TKDBSQLiteManager }

constructor TKDBSQLiteManager.create(AName: String; Filename : String; autoCreate : boolean; maxConn : integer = 100);
begin
  FFilename := filename;
  FAutoCreate := autoCreate;
  Inherited Create(aName, maxConn);
end;

constructor TKDBSQLiteManager.create(AName: String; ASettings: TSettingsAdapter; AIdent: String);
begin
  create(AName, ASettings.ReadString('Filename', ''), ASettings.ReadBool('AutoCreate', false));
end;

function TKDBSQLiteManager.ConnectionFactory: TKDBConnection;
begin
  result := TKDBSQLiteConnection.Create(self, FFilename, FAutoCreate);
end;

destructor TKDBSQLiteManager.Destroy;
begin
  inherited;
end;

function TKDBSQLiteManager.GetDBDetails: String;
begin
  result := 'SQLite: '+FFIlename;
end;

function TKDBSQLiteManager.GetDBPlatform: TKDBPlatform;
begin
  result := TKDBPlatform.kdbSQLite;
end;

function TKDBSQLiteManager.GetDBProvider: TKDBProvider;
begin
  result := kdbpSQLite;
end;

function TKDBSQLiteManager.GetDriver: String;
begin
  result := 'SQLite';
end;

procedure TKDBSQLiteManager.init;
begin
  loadSQLite;
  assert(sqlite3_threadsafe>0, 'SQLite library is not threadsafe');
  if not FAutoCreate then
    if not FileExists(FFIlename) then
      raise Exception.Create('SQLite Database '+FFIlename+' not found');
end;

class function TKDBSQLiteManager.IsSupportAvailable(APlatform: TKDBPlatform; var VMsg: String): Boolean;
begin
  result := false;
  VMsg := 'develop this bit';
end;

procedure TKDBSQLiteManager.SaveSettings(ASettings: TSettingsAdapter);
begin
  raise Exception.Create('Not done yet');
end;

{ TKDBSQLiteConnection }

constructor TKDBSQLiteConnection.create(AOwner: TKDBManager; Filename : String; autoCreate : boolean);
begin
  inherited create(AOwner);
  FConnection := TSQLite3Database.Create;
  FConnection.Delay := 2000;
  if autoCreate then
    FConnection.Open(Filename, SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  else
    FConnection.Open(Filename, SQLITE_OPEN_READWRITE);
end;

destructor TKDBSQLiteConnection.Destroy;
begin
  FConnection.Free;
  FStatement.Free;
  FColNames.Free;
  inherited;
end;

procedure TKDBSQLiteConnection.BindBlobV(AParamName: String; AParamValue: TBytes);
begin
  FStatement.BindBlob(':'+AParamName, @AParamValue[0], length(AParamValue));
end;

procedure TKDBSQLiteConnection.BindDateTimeExV(AParamName: String; AParamValue: TDateTimeEx);
begin
  FStatement.BindText(':'+AParamName, AParamValue.UTC.toDB);
end;

procedure TKDBSQLiteConnection.BindDoubleV(AParamName: String; AParamValue: Double);
begin
  FStatement.BindDouble(':'+AParamName, AParamValue);
end;

procedure TKDBSQLiteConnection.BindInt64V(AParamName: String; AParamValue: Int64);
begin
  FStatement.BindInt64(':'+AParamName, AParamValue);
end;

procedure TKDBSQLiteConnection.BindIntegerV(AParamName: String; AParamValue: Integer);
begin
  FStatement.BindInt(':'+AParamName, AParamValue);
end;

procedure TKDBSQLiteConnection.BindKeyV(AParamName: String; AParamValue: Integer);
begin
  FStatement.BindInt(':'+AParamName, AParamValue);
end;

procedure TKDBSQLiteConnection.BindNullV(AParamName: String);
begin
  FStatement.BindNull(':'+AParamName);
end;

procedure TKDBSQLiteConnection.BindStringV(AParamName, AParamValue: String);
begin
  FStatement.BindText(':'+AParamName, AParamValue);
end;

procedure TKDBSQLiteConnection.BindTimeStampV(AParamName: String; AParamValue: TTimeStamp);
begin
  BindDateTimeExV(AParamName, TDateTimeEx.fromTS(aParamValue, dttzUnknown));
end;

procedure TKDBSQLiteConnection.ClearDatabaseV;
begin
  raise Exception.Create('Not done yet');
end;

function TKDBSQLiteConnection.ColByNameV(AColName: String): Integer;
begin
  result := ColNames.indexOf(aColName)+1;
end;

function TKDBSQLiteConnection.ColNames: TStringList;
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

function TKDBSQLiteConnection.ColNameV(ACol: Integer): String;
begin
  result := ColNames[aCol];
end;

procedure TKDBSQLiteConnection.CommitV;
begin
  FConnection.Commit;
end;

function TKDBSQLiteConnection.DatabaseSizeV: int64;
begin
  result := FileSize(TKDBSQLiteManager(Owner).FFilename);
end;

procedure TKDBSQLiteConnection.DropColumnV(ATableName, AColumnName: String);
begin
  raise Exception.Create('Not done yet');
end;

procedure TKDBSQLiteConnection.DropTableV(ATableName: String);
begin
  ExecSQL('Drop table '+ATableName);
end;

procedure TKDBSQLiteConnection.ExecuteV;
begin
  if not SQL.StartsWith('Select') then
    FStatement.StepAndReset;
end;

function TKDBSQLiteConnection.readColumn(field : String): TKDBColumn;
var
  s : String;
begin
  StringSplit(field, ' ', s, field);
  result := TKDBColumn.Create(s);
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

function TKDBSQLiteConnection.FetchMetaDataV: TKDBMetaData;
var
  tbl : TKDBTable;
  col : TKDBColumn;
  s : String;
  a : TArray<String>;
begin
  result := TKDBMetaData.Create;
  try
    sql := 'SELECT name, sql FROM sqlite_master WHERE type=''table''';
    prepare;
    execute;
    while fetchnext do
    begin
      tbl := TKDBTable.Create;
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

function TKDBSQLiteConnection.FetchNextV: Boolean;
begin
  result := FStatement.Step = SQLITE_ROW;
end;

function TKDBSQLiteConnection.GetColBlobV(ACol: Word): TBytes;
begin
  SetLength(result, FStatement.ColumnBytes(ACol-1));
  if length(result) > 0 then
    move(FStatement.ColumnBlob(aCol-1)^, result[0], length(result));
end;

function TKDBSQLiteConnection.GetColCountV: Integer;
begin
  result := FStatement.ColumnCount;
end;

function TKDBSQLiteConnection.GetColDateTimeExV(ACol: Word): TDateTimeEx;
begin
  result := TDateTimeEx.fromDB(getColStringV(ACol), dttzUTC);
end;

function TKDBSQLiteConnection.GetColDoubleV(ACol: Word): Double;
begin
  result := FStatement.ColumnDouble(ACol-1);
end;

function TKDBSQLiteConnection.GetColInt64V(ACol: Word): Int64;
begin
  result := FStatement.ColumnInt64(ACol-1);
end;

function TKDBSQLiteConnection.GetColIntegerV(ACol: Word): Integer;
begin
  result := FStatement.ColumnInt(ACol-1);
end;

function TKDBSQLiteConnection.GetColKeyV(ACol: Word): Integer;
begin
  result := FStatement.ColumnInt(ACol-1);
end;

function TKDBSQLiteConnection.GetColNullV(ACol: Word): Boolean;
begin
  result := FStatement.ColumnNull(ACol-1);
end;

function TKDBSQLiteConnection.GetColStringV(ACol: Word): String;
begin
  result := FStatement.ColumnText(ACol-1);
end;

function TKDBSQLiteConnection.GetColTimestampV(ACol: Word): TTimestamp;
begin
  result := GetColDateTimeExV(ACol).Local.TimeStamp;
end;

function TKDBSQLiteConnection.GetColTypeV(ACol: Word): TKDBColumnType;
begin
  raise Exception.Create('Not done yet');
end;

function TKDBSQLiteConnection.GetRowsAffectedV: Integer;
begin
  result := FConnection.RowsAffected;
end;

procedure TKDBSQLiteConnection.ListTablesV(AList: TStrings);
begin
  raise Exception.Create('Not done yet');
end;

procedure TKDBSQLiteConnection.PrepareV;
begin
  FStatement := FConnection.Prepare(SQL);
end;

procedure TKDBSQLiteConnection.RenameColumnV(ATableName, AOldColumnName, ANewColumnName, AColumnDetails: String);
begin
  raise Exception.Create('Not done yet');
end;

procedure TKDBSQLiteConnection.RenameTableV(AOldTableName, ANewTableName: String);
begin
  raise Exception.Create('Not done yet');
end;

procedure TKDBSQLiteConnection.RollbackV;
begin
  FConnection.Rollback;
end;

procedure TKDBSQLiteConnection.StartTransactV;
begin
  FConnection.BeginTransaction;
end;

function TKDBSQLiteConnection.SupportsSizingV: Boolean;
begin
  result := true;
end;

function TKDBSQLiteConnection.TableSizeV(sName: String): int64;
begin
  raise Exception.Create('Not done yet');
end;

procedure TKDBSQLiteConnection.TerminateV;
begin
  FreeAndNil(FStatement);
  FreeAndNil(FColNames);
end;

end.

