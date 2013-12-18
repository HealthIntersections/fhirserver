{! 1 !}
{0.00-023  02 Sep 04 12:50  [21370]  User: Grahame Grieve    fix transient leak}

unit KDBOdbcExpress;

interface

uses
  Classes, Contnrs, IniFiles,
  AdvObjects, StringSupport,
  KDate, KDBDialects, KDBManager, KSettings,
  OdbcExtras, OdbcHeaders, OdbcCore;

type
  TOdbcConnection = class (TKDBConnection)
  private
    FHdbc : THdbc;
    FStmt : THstmt;
    FASAMode : Integer;
    procedure SetASALevel(iLevel : Integer);
    function FetchColumnMetaData(ASrc : TCatalogColumn) : TKDBColumn;
    function FetchIndexMetaData(ACat: TOECatalog; AName : String) : TKDBIndex;
    Function FetchRelationshipMetaData(aCat : TOECatalog; aDetails : String) : TKDBRelationship;
    function FetchTableMetaData(ACat: TOECatalog; ASrc : TCatalogTable) : TKDBTable;
    function DatabaseSizeMSSQL : int64;
    function TableSizeMSSQL(sName : String) : int64;
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
    function GetColMemoryV(ACol: Word): TMemoryStream; Override;
    function GetColNullV(ACol: Word): Boolean; Override;
    function GetColTimestampV(ACol: Word): TTimestamp; Override;
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
    procedure BindBlobV(AParamName: String; AParamValue: TMemoryStream); Override;
    procedure BindNullV(AParamName: String); Override;
    function CheckConnection : Integer; Override;
    function DatabaseSizeV : int64; Override;
    Function TableSizeV(sName : String):int64; Override;
    function SupportsSizingV : Boolean; Override;
  Public
    constructor create(AOwner : TKDBManager; AHdbc : THdbc; AStmt : THstmt);
    destructor destroy; override;
  end;

  TOdbcExpressConnManBase = {Abstract} class (TKDBManager)
  private
    FPlatform : TKDBPlatform;
    FDsn : String;
    FDriver : String;
    FServer : String;
    FDatabase : String;
    FUsername : String;
    FPassword : String;
    FAttributes : TStringList;
  Protected
    function ConnectionFactory: TKDBConnection; Override;
    function GetDBPlatform: TKDBPlatform; Override;
    function GetDBDetails: String; Override;
  public
    destructor destroy; override;
    class function IsSupportAvailable(APlatform : TKDBPlatform; Var VMsg : String):Boolean; override;
    property Dsn : String read FDsn;
    property Driver : String read FDriver;
    property Server : String read FServer;
    property Database : String read FDatabase;
    property Username : String read FUsername;
    property Password : String read FPassword;
  end;

  TKDBOdbcDSN = class (TOdbcExpressConnManBase)
  protected
    function GetDBProvider: TKDBProvider; Override;
  public
    constructor create(AName : String; AMaxConnCount: Integer; ADSN, AUsername, APassword: String); overload;
    constructor create(AName : String; ASettings : TSettingsAdapter; AIdent : String = ''); overload; override;
    procedure SaveSettings(ASettings : TSettingsAdapter); override;
  end;

  TKDBOdbcDirect = class (TOdbcExpressConnManBase)
  protected
    function GetDBProvider: TKDBProvider; Override;
  public
    constructor create(AName : String; AMaxConnCount: Integer; ADriver, AServer, ADatabase, AUsername, APassword: String); overload;
    constructor create(AName : String; ASettings : TSettingsAdapter; AIdent : String = ''); overload; override;
    procedure SaveSettings(ASettings : TSettingsAdapter); override;
  end;

{  TKDBOdbcExpress = class (TOdbcExpressConnManBase)
  public
    constructor create(AName : String; AMaxConnCount: Integer; ADSN, ADriver, AServer, ADatabase, AUsername, APassword: String); overload;
    constructor create(AName : String; AIniFile : TIniFile; ASection : String; AIdent : String = ''); overload; override;
  end;
}

function RecogniseDSNDriver(ADSN: String): TKDBPlatform;
function StandardODBCDriverName(APlatform: TKDBPlatform): String;

implementation

uses
  IdSoapClasses,
  IdSoapUtilities,
  SysUtils;

const
  ASSERT_UNIT = 'KDBOdbcExpress';

constructor TOdbcConnection.create(AOwner : TKDBManager; AHdbc : THdbc; AStmt : THstmt);
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.create';
begin
  inherited create(AOwner);
  FHdbc := AHdbc;
  FStmt := AStmt;
  FASAMode := -1; // transaction isolation level unknown
end;

destructor TOdbcConnection.destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.destroy;';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  FStmt.free;
  FHdbc.free;
  inherited;
end;

procedure TOdbcConnection.SetASALevel(iLevel : Integer);
begin
  if FASAMode <> iLevel then
    begin
    FStmt.Sql := 'SET temporary OPTION ISOLATION_LEVEL = '+inttostr(iLevel);
    FStmt.Prepare;
    FStmt.Execute;
    FStmt.Terminate;
    FASAMode := iLevel;
    end;
end;

function TOdbcConnection.GetColCountV: Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.GetColCount';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  Result := FStmt.ColCount;
end;

function TOdbcConnection.GetColStringV(ACol: Word): String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.GetColString';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  Result := FStmt.ColString[ACol];
end;

function TOdbcConnection.GetColIntegerV(ACol: Word): Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.GetColInteger';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  Result := FStmt.ColInteger[ACol];
end;

function TOdbcConnection.GetColInt64V(ACol: Word): Int64;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.GetColInt64';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  Result := FStmt.ColInt64[ACol];
end;

function TOdbcConnection.GetColDoubleV(ACol: Word): Double;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.GetColDouble';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  Result := FStmt.ColDouble[ACol];
end;

function TOdbcConnection.GetColMemoryV(ACol: Word): TMemoryStream;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.GetColMemory';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  Result := FStmt.ColMemory[ACol];
end;

function TOdbcConnection.GetColNullV(ACol: Word): Boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.GetColNull';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  Result := FStmt.ColNull[ACol];
end;

function TOdbcConnection.GetColTimestampV(ACol: Word): KDate.TTimestamp;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.GetColTimestamp';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  Result := FStmt.ColTimestamp[ACol];
end;

function ConvertColType(ct: SmallInt): TKDBColumnType;
begin
  case ct of
    SQL_C_GUID,
    SQL_C_DEFAULT:
      Result := ctUnknown;

    SQL_C_VARCHAR,
    SQL_C_CHAR:
      Result := ctChar;

    SQL_UNICODE_VARCHAR,
    SQL_UNICODE_LONGVARCHAR,
    SQL_UNICODE_CHAR:
      Result := ctUnicode;

    SQL_C_LONG,
    SQL_C_SLONG,
    SQL_C_SHORT:
      Result := ctInteger;

    SQL_FLOAT,       {What a mess - the ODBC drivers are deriving values from 2 different sets of constants}
    SQL_C_FLOAT,
    SQL_C_DOUBLE:
      Result := ctFloat;

    SQL_C_TYPE_DATE,
    SQL_C_TYPE_TIME,
    SQL_C_TYPE_TIMESTAMP,
    SQL_C_INTERVAL_YEAR,
    SQL_C_INTERVAL_MONTH,
    SQL_C_INTERVAL_DAY,
    SQL_C_INTERVAL_HOUR,
    SQL_C_INTERVAL_MINUTE,
    SQL_C_INTERVAL_SECOND,
    SQL_C_INTERVAL_YEAR_TO_MONTH,
    SQL_C_INTERVAL_DAY_TO_HOUR,
    SQL_C_INTERVAL_DAY_TO_MINUTE,
    SQL_C_INTERVAL_DAY_TO_SECOND,
    SQL_C_INTERVAL_HOUR_TO_MINUTE,
    SQL_C_INTERVAL_HOUR_TO_SECOND,
    SQL_C_INTERVAL_MINUTE_TO_SECOND:
      Result := ctDateTime;

    SQL_LONGVARBINARY,
    SQL_C_BINARY:
      Result := ctBlob;

    SQL_C_BIT:
      Result := ctBoolean;

    SQL_BIGINT,
    SQL_C_SBIGINT,
    SQL_C_UBIGINT:
      Result := ctInt64;

    SQL_C_NUMERIC:
      Result := ctNumeric;

    SQL_C_TINYINT,
    SQL_C_SSHORT,
    SQL_C_STINYINT,
    SQL_C_ULONG,
    SQL_C_USHORT,
    SQL_C_UTINYINT:
      Result := ctNumeric;
  else
    Result := ctUnknown
  end;
end;

function TOdbcConnection.GetColTypeV(ACol: Word): TKDBColumnType;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.GetColType';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  Result := ConvertColType(FStmt.ColType[ACol])
end;

function TOdbcConnection.GetColKeyV(ACol: Word): Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.GetColKey';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  Result := GetColInteger(ACol);
end;

function TOdbcConnection.GetRowsAffectedV: Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.GetRowsAffected';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  Result := FStmt.RowsAffected;
end;

procedure TOdbcConnection.RenameTableV(AOldTableName, ANewTableName: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.RenameTable';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  if Owner.Platform = kdbASA then
    FStmt.SQL := 'ALTER TABLE ' + AOldTableName + ' RENAME ' + ANewTableName
  else if Owner.Platform in [kdbDB2, kdbCtree] then
    FStmt.SQL := 'RENAME TABLE ' + AOldTableName + ' TO ' + ANewTableName
  else
    FStmt.SQL := 'sp_rename ' + AOldTableName + ', ' + ANewTableName;
  FStmt.Prepare;
  FStmt.Execute;
  FStmt.Terminate;
end;

procedure TOdbcConnection.DropTableV(ATableName : String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.RenameColumn';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  SQL := 'Drop Table ' + ATableName;
  Prepare;
  Execute;
  terminate;
end;

procedure TOdbcConnection.DropColumnV(ATableName, aColumnName : String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.RenameColumn';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  SQL := 'ALTER TABLE ' + ATableName+' DROP COLUMN  ' + aColumnName;
  Prepare;
  Execute;
  terminate;
end;

procedure TOdbcConnection.RenameColumnV(ATableName, AOldColumnName, ANewColumnName: String; AColumnDetails: String = '');
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.RenameColumn';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  if Owner.Platform = kdbASA then
    FStmt.SQL := 'ALTER TABLE ' + ATableName + ' RENAME ' + AOldColumnName+' TO '+ANewColumnName
  else
    FStmt.SQL := 'sp_rename ''' + ATableName + '.' + AOldColumnName + ''', ''' + ANewColumnName + '''';
  FStmt.Prepare;
  FStmt.Execute;
  FStmt.Terminate;
end;

procedure TOdbcConnection.ListTablesV(AList : TStrings);
var
  LCat: TOECatalog;
begin
  LCat := TOECatalog.Create(NIL);
  try
    LCat.hDbc := FHdbc;
    LCat.TableType := [ttTable];
    AList.Assign(LCat.TableNames);
  finally
    LCat.Free;
    end;
end;

procedure TOdbcConnection.ClearDatabaseV;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.ClearDatabase;';
var
  LTables: TStringList;
  i: Integer;
  iErrs : Integer;
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');

  LTables := TStringList.Create;
  try
    Repeat
      LTables.Clear;
      iErrs := 0;
      ListTables(LTables);
      for i := 0 to LTables.Count - 1 do
        if SameText(copy(LTables[i], 1, 3), 'sys') or (Pos('_xe_', LTables[i]) > 0) then
          inc(iErrs)
        else
          begin
          try
            SQL := 'Drop Table ' + LTables[i];
            Prepare;
            try
              Execute;
            finally
              terminate;
            end;
        Except
          inc(iErrs);
        End;
      end;
    Until (LTables.Count = iErrs);
  finally
    LTables.free;
    end;
end;

procedure TOdbcConnection.PrepareV;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.Prepare;';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  if Owner.Platform = kdbASA then
    begin
    if SQLHasResultSet(SQL) then
      SetASALevel(0)
    else
      SetASALevel(1);
    end;
  FStmt.sql := SQL;
  FStmt.Prepare;
end;

procedure TOdbcConnection.ExecuteV;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.Execute;';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  FStmt.Execute;
end;

procedure TOdbcConnection.TerminateV;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.Terminate;';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  FStmt.Terminate;
end;

procedure TOdbcConnection.StartTransactV;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.StartTransact;';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  FHdbc.StartTransact;
end;

procedure TOdbcConnection.CommitV;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.Commit;';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  FHdbc.Commit;
  FHdbc.EndTransact;
end;

procedure TOdbcConnection.RollbackV;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.Rollback;';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  FHdbc.rollback;
  FHdbc.EndTransact;
end;

function TOdbcConnection.FetchNextV: Boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.FetchNext';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  Result := FStmt.FetchNext;
end;

function TOdbcConnection.ColByNameV(AColName: String): Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.ColByName';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  Result := FStmt.ColByName(AColName);
end;

function TOdbcConnection.ColNameV(ACol: Integer): String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.ColByName';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  Result := FStmt.ColNames[ACol-1];
end;

type
  TOdbcBoundParam = class (TAdvObject);

  TOdbcBoundString = class (TOdbcBoundParam)
  private
    FString: String;
  end;

  TOdbcBoundInt = class (TOdbcBoundParam)
  private
    FInt: Integer;
  end;

  TOdbcBoundInt64 = class (TOdbcBoundParam)
  private
    FInt64: Int64;
  end;

  TOdbcBoundDate = class (TOdbcBoundParam)
  private
    FDate: KDate.TTimeStamp
  end;

  TOdbcBoundDouble = class (TOdbcBoundParam)
  private
    FDouble: Double
  end;

procedure TOdbcConnection.BindInt64V(AParamName: String; AParamValue: Int64);
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.BindInt64';
var
  LBind: TOdbcBoundInt64;
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  assert(AParamName <> '', ASSERT_LOCATION+': ParamName is not valid');
  LBind := TOdbcBoundInt64.Create;
  LBind.FInt64 := AParamValue;
  FStmt.BindInt64ByName(AParamName, LBind.FInt64);
  KeepBoundObj(aParamName, LBind);
end;

procedure TOdbcConnection.BindIntegerV(AParamName: String; AParamValue: Integer);
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.BindInteger';
var
  LBind: TOdbcBoundInt;
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  assert(AParamName <> '', ASSERT_LOCATION+': ParamName is not valid');
  LBind := TOdbcBoundInt.Create;
  LBind.FInt := AParamValue;
  FStmt.BindIntegerByName(AParamName, LBind.FInt);
  KeepBoundObj(aParamName, LBind);
end;

procedure TOdbcConnection.BindKeyV(AParamName: String; AParamValue: Integer);
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.BindKey';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  assert(AParamName <> '', ASSERT_LOCATION+': ParamName is not valid');
  BindInteger(AParamName, AParamValue);
end;

procedure TOdbcConnection.BindDoubleV(AParamName: String; AParamValue: Double);
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.BindDouble';
var
  LBind: TOdbcBoundDouble;
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  assert(AParamName <> '', ASSERT_LOCATION+': ParamName is not valid');
  LBind := TOdbcBoundDouble.Create;
  LBind.FDouble := AParamValue;
  FStmt.BindDoubleByName(AParamName, LBind.FDouble);
  KeepBoundObj(aParamName, LBind);
end;

procedure TOdbcConnection.BindStringV(AParamName: String; AParamValue: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.BindString';
var
  LBind: TOdbcBoundString;
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  assert(AParamName <> '', ASSERT_LOCATION+': ParamName is not valid');
  LBind := TOdbcBoundString.Create;
  LBind.FString := AParamValue;
  FStmt.BindStringByName(AParamName, LBind.FString);
  KeepBoundObj(aParamName, LBind);
end;

procedure TOdbcConnection.BindTimeStampV(AParamName: String; AParamValue: KDate.TTimeStamp);
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.BindTimeStamp';
var
  LBind: TOdbcBoundDate;
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  assert(AParamName <> '', ASSERT_LOCATION+': ParamName is not valid');
  if Owner.Platform = kdbSybase12 then
    begin
    AParamValue.Fraction := 0;
    end;
  LBind := TOdbcBoundDate.Create;
  LBind.FDate := AParamValue;
  FStmt.BindTimeStampByName(AParamName, LBind.FDate);
  KeepBoundObj(aParamName, LBind);
end;

procedure TOdbcConnection.BindBlobV(AParamName: String; AParamValue: TMemoryStream);
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.BindBinary';
var
  LBind: TMemoryStream;
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  assert(AParamName <> '', ASSERT_LOCATION+': ParamName is not valid');
  LBind := TIdMemoryStream.Create;
  LBind.CopyFrom(AParamValue, 0);
  LBind.Position := 0;
  FStmt.BindBinaryByName(AParamName, LBind);
  KeepBoundObj(aParamName, LBind);
end;

procedure TOdbcConnection.BindNullV(AParamName: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.BindNull';
begin
  assert(self.TestValid(TOdbcConnection), ASSERT_LOCATION+': self is not valid');
  assert(AParamName <> '', ASSERT_LOCATION+': ParamName is not valid');
  FStmt.BindNullByName(AParamName);
end;

function TOdbcConnection.CheckConnection : Integer;
Begin
  FHdbc.DisConnect;
  FHdbc.Connect;
  Result := CONNECTION_OK;
End;

Function ReadBytes(value : String):Int64;
var
  v , u : String;
Begin
  StringSplit(value, ' ', v, u);
  if sameText(u, 'KB') Then
    result := trunc(strToFloat(v)*1024)
  else if sameText(u, 'MB') Then
    result := trunc(strToFloat(v)*1024*1024)
  else if sameText(u, 'GB') Then
    result := trunc(strToFloat(v)*1024*1024*1024)
  else raise exception.create('unknown unit ' +u);
End;



function TOdbcConnection.DatabaseSizeMSSQL : int64;
Begin
  SQL := 'sp_spaceused';
  Prepare;
  try
    Execute;
    FetchNext;
    result := ReadBytes(ColStringByName['database_size']);
  Finally
    Terminate;
  End;
End;

function TOdbcConnection.TableSizeMSSQL(sName : String) : int64;
Begin
  result := 0;
  SQL := 'sp_spaceused '+sName;
  Prepare;
  try
    Execute;
    if not FetchNext Then
      raise exception.create('Table "'+sName+'" not found checking size');
    result := ReadBytes(ColStringByName['reserved']);
  Finally
    Terminate;
  End;
End;


function TOdbcConnection.DatabaseSizeV : int64;
Begin
  case Owner.Platform of
    kdbSQLServer  :
      result := DatabaseSizeMSSQL;
  else // kdbUnknown, kdbSybase11, kdbCtree, kdbAccess, kdbDBIsam, kdbInterbase, kdbDB2, kdbGenericODBC, kdbOracle8, kdbMySQL, kdbASA, kdbSybase12
    raise exception.create('This operation (database size) is not supported on this platform ('+DescribePlatform(Owner.Platform) +')');
  End;
End;


Function TOdbcConnection.TableSizeV(sName : String):int64;
Begin
  case Owner.Platform of
    kdbSQLServer  :
      result := TableSizeMSSQL(sName);
  else // kdbUnknown, kdbSybase11, kdbCtree, kdbAccess, kdbDBIsam, kdbInterbase, kdbDB2, kdbGenericODBC, kdbOracle8, kdbMySQL, kdbASA, kdbSybase12
    raise exception.create('This operation (table size) is not supported on this platform ('+DescribePlatform(Owner.Platform) +')');
  End;
End;

function TOdbcConnection.SupportsSizingV : Boolean;
Begin
  result := Owner.Platform = kdbSQLServer;
End;


function TOdbcConnection.FetchColumnMetaData(ASrc : TCatalogColumn) : TKDBColumn;
begin
  result := TKDBColumn.create;
  try
    result.Name := ASrc.ColumnName;
    result.DataType := ConvertColType(ASrc.DataType);
    result.Length := ASrc.Precision;
    result.Nullable := Asrc.Nullable = SQL_NULLABLE;
  except
    result.free;
    raise;
  end;
end;

function TOdbcConnection.FetchIndexMetaData(ACat: TOECatalog; AName : String) : TKDBIndex;
var
  LFields : TStringList;
  LName : String;
  LIndexUnique : Boolean;
  i : Integer;
begin
  result := TKDBIndex.create;
  try
    LFields := TStringList.create;
    try
      ACat.ParseIndex(AName, LName, LIndexUnique, LFields);
      result.Name := LName;
      result.Unique := LIndexUnique;
      for i := 0 to LFields.Count - 1 do
        begin
        result.Columns.add(LFields[i]);
        end;
    finally
      LFields.Free;
    end;
  except
    result.free;
    raise;
  end;
end;

function TOdbcConnection.FetchRelationshipMetaData(ACat: TOECatalog; aDetails : String) : TKDBRelationship;
var
  ColumnName, ForeignOwner, ForeignTable, ForeignColumn : String;
begin
  result := TKDBRelationship.create;
  try
    ACat.ParseForeignKey(aDetails, ColumnName, ForeignOwner, ForeignTable, ForeignColumn);
    result.Column := columnName;
    result.DestTable := ForeignTable;
    result.DestColumn := ForeignColumn;
  except
    result.free;
    raise;
  end;
end;

function TOdbcConnection.FetchTableMetaData(ACat: TOECatalog; ASrc : TCatalogTable) : TKDBTable;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcConnection.FetchTableMetaData';
var
  i : integer;
begin
  result := TKDBTable.create;
  try
    result.Name := ASrc.TableName;
    result.Owner := ASrc.TableOwner;
    result.Description := ASrc.Description;
    result.TableType := TKDBTableType(ASrc.TableType);
    for i := 0 to ASrc.Columns.itemcount - 1  do
      begin
      result.Columns.add(FetchColumnMetadata(ASrc.Columns[i]));
      end;
    for i := 0 to ASrc.Indexes.Count - 1  do
      begin
      result.Indexes.add(FetchIndexMetadata(ACat, ASrc.Indexes[i]));
      end;
    for i := 0 to ASrc.ForeignKeys.Count - 1 Do
       result.Relationships.Add(FetchRelationshipMetaData(aCat, ASrc.ForeignKeys[i]));

  except
    result.free;
    raise;
  end;
end;

function TOdbcConnection.FetchMetaDataV : TKDBMetaData;
var
  LCat: TOECatalog;
  i : integer;
  LRes : TKDBMetaData;
begin
  LCat := TOECatalog.Create(NIL);
  try
    LCat.hDbc := FHdbc;
    LRes := TKDBMetaData.create;
    try
      for i := 0 to LCat.Tables.ItemCount - 1 do
        begin
        LRes.Tables.Add(FetchTableMetaData(LCat, LCat.Tables[i]))
        end;
      try
        LRes.SupportsProcedures := true;
        for i := 0 to LCat.Procedures.ItemCount - 1 do
          begin
          LRes.Procedures.Add(LCat.Procedures[i].ProcedureName);
          end;
      except
        LRes.SupportsProcedures := False;
      end;
      result := LRes;
      LRes := nil;
    finally
      LRes.Free;
    end;
  finally
    LCat.Free;
    end;
end;

{ TKDBOdbcDSN }

constructor TKDBOdbcDSN.create(AName : String; AMaxConnCount: Integer; ADSN, AUsername, APassword: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBOdbcDSN.create';
begin
  inherited create(AName, AMaxConnCount);
  FAttributes := TIdStringList.create;
  FDsn := ADSN;
  FUsername := AUsername;
  FPassword := APassword;
  FPlatform := RecogniseDSNDriver(FDsn);
end;

constructor TKDBOdbcDSN.create(AName : String; ASettings : TSettingsAdapter; AIdent : String = '');
begin
  create(AName, ASettings.ReadInteger('MaxConnections', 20), ASettings.ReadString('DSN', ''),
         ASettings.ReadString('Username', ''), ASettings.ReadEncryptedString('Password', ''));
end;

procedure TKDBOdbcDSN.SaveSettings(ASettings : TSettingsAdapter);
begin
  ASettings.WriteString('Platform', IdEnumToString(TypeInfo(TKDBPlatform), ord(GetDBPlatform)));
  ASettings.WriteString('Provider', IdEnumToString(TypeInfo(TKDBProvider), ord(kdbpDSN)));
  ASettings.WriteInteger('MaxConnections', MaxConnCount);
  ASettings.WriteString('DSN', FDsn);
  ASettings.WriteString('Username', FUsername);
  ASettings.WriteEncryptedString('Password', FPassword);
end;

function TKDBOdbcDSN.GetDBProvider: TKDBProvider;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBOdbcDSN.GetDBPlrovider';
begin
  assert(self.TestValid(TOdbcExpressConnManBase), ASSERT_LOCATION+': self is not valid');
  result := kdbpDSN;
end;

{ TKDBOdbcDirect }

constructor TKDBOdbcDirect.create(AName : String; ASettings : TSettingsAdapter; AIdent : String = '');
begin
  create(AName, ASettings.ReadInteger('MaxConnections', 20), ASettings.ReadString('ODBCDriver', ''),
                ASettings.ReadString('Server', ''),  ASettings.ReadString('Database', ''),
                ASettings.ReadString('Username', ''), ASettings.ReadEncryptedString('Password', ''));
end;

procedure TKDBOdbcDirect.SaveSettings(ASettings : TSettingsAdapter);
begin
  ASettings.WriteString('Platform', IdEnumToString(TypeInfo(TKDBPlatform), ord(GetDBPlatform)));
  ASettings.WriteString('Provider', IdEnumToString(TypeInfo(TKDBProvider), ord(kdbpODBC)));
  ASettings.WriteInteger('MaxConnections', MaxConnCount);
  ASettings.WriteString('Driver', FDriver);
  ASettings.WriteString('Server', FServer);
  ASettings.WriteString('Database', FDatabase);
  ASettings.WriteString('Username', FUsername);
  ASettings.WriteEncryptedString('Password', FPassword);
end;

constructor TKDBOdbcDirect.create(AName : String; AMaxConnCount: Integer; ADriver, AServer, ADatabase, AUsername, APassword: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBOdbcDirect.create';
begin
  inherited create(Aname, AMaxConnCount);
  FAttributes := TIdStringList.create;
  FDriver := ADriver;
  FServer := AServer;
  FDatabase := ADatabase;
  FUsername := AUsername;
  FPassword := APassword;
  FPlatform := RecogniseDriver(ADriver);

  if (FPlatform = kdbAccess) or ((FPlatform = kdbUnknown) and (pos('excel', lowercase(ADriver))> 0)) then
    begin
    FAttributes.add('DBQ=' + FDatabase)
    end
  else if Platform = kdbDB2 then
    begin
    FAttributes.add('DBAlias=' + FDatabase);
    end
  else
    begin
    if FPlatform = kdbCtree then
      begin
      FAttributes.add('Host=' + FServer)
      end
    else if (FPlatform = kdbSybase11) or (FPlatform = kdbSybase12) then
      begin
      FAttributes.add('ServerName=' + FServer)
      end
    else
      begin
      FAttributes.add('Server=' + FServer);
      end;
    FAttributes.add('Database=' + FDatabase);
    end;
end;

function TKDBOdbcDirect.GetDBProvider: TKDBProvider;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBOdbcDirect.GetDBPlrovider';
begin
  assert(self.TestValid(TOdbcExpressConnManBase), ASSERT_LOCATION+': self is not valid');
  result := kdbpODBC;
end;

{ TOdbcExpressConnManBase }

destructor TOdbcExpressConnManBase.Destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcExpressConnManBase.Destroy';
begin
  assert(self.TestValid(TOdbcExpressConnManBase), ASSERT_LOCATION+': self is not valid');
  FAttributes.free;
  inherited;
end;

function TOdbcExpressConnManBase.ConnectionFactory: TKDBConnection;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcExpressConnManBase.ConnectionFactory:';
var
  LHdbc : THdbc;
  LStmt : THStmt;
  i : integer;
begin
  assert(self.TestValid(TOdbcExpressConnManBase), ASSERT_LOCATION+': self is not valid');
  LStmt := nil;
  LHdbc := THdbc.Create(NIL);
  try
    LHdbc.DataSource := FDsn;
    LHdbc.Driver := FDriver;
    LHdbc.UserName := FUsername;
    LHdbc.Password := FPassword;
    for i := 0 to FAttributes.Count - 1 do
      begin
      LHdbc.Attributes.Add(FAttributes[i]);
      end;
    If (FUsername = '') Then
    Begin
      LHdbc.Attributes.Add('Integrated Security=SSPI');
      LHdbc.Attributes.Add('Trusted_Connection=Yes');
    End;
    LHdbc.InfoPrompt := SQL_DRIVER_NOPROMPT;
    LHdbc.IsolationLevel := SQL_TXN_READ_COMMITTED;
    LHdbc.CursorLib := SQL_CUR_USE_DRIVER;
    LHdbc.Connect;
    LStmt := THstmt.Create(NIL);
    LStmt.hDbc := LHdbc;
    LStmt.ConcurrencyType := SQL_CONCUR_READ_ONLY;
    LStmt.CursorType := SQL_CURSOR_FORWARD_ONLY;
    LStmt.RowSetSize := 1;
    LStmt.BindBookMarks := False;
    result := TOdbcConnection.create(self, LHdbc, LStmt);
  except
    on e:exception do
      begin
      e.Message := 'Error Connecting to "'+DBDetails+'": '+e.Message;
      LStmt.free;
      LHdbc.free;
      raise;
      end;
  end;
end;

function TOdbcExpressConnManBase.GetDBPlatform: TKDBPlatform;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcExpressConnManBase.GetDBPlatform:';
begin
  assert(self.TestValid(TOdbcExpressConnManBase), ASSERT_LOCATION+': self is not valid');
  result := FPlatform;
end;

function TOdbcExpressConnManBase.GetDBDetails: String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TOdbcExpressConnManBase.GetDBDetails:';
begin
  assert(self.TestValid(TOdbcExpressConnManBase), ASSERT_LOCATION+': self is not valid');
  if FDsn = '' then
    Result := '\\' + FDriver + '\' + FServer + '\' + FDatabase + ' [' + FUsername + ']'
  else
    Result := '\\DSN\' + FDsn + ' [' + FUsername + ']';
end;

class function TOdbcExpressConnManBase.IsSupportAvailable(APlatform : TKDBPlatform; Var VMsg : String):Boolean;
begin
  result := false;
  VMsg := 'develop this bit';
end;

{ ODBC Utils }

function RecogniseDSNDriver(ADSN: String): TKDBPlatform;
var
  LDbc: TOEAdministrator;
begin
  LDbc := TOEAdministrator.Create(NIL);
  try
    LDbc.DataSource := ADSN;
    Result := RecogniseDriver(Ldbc.DataSourceDriver(Ldbc.DataSource));
  finally
    LDbc.free;
  end;
end;

function StandardODBCDriverName(APlatform: TKDBPlatform): String;
begin
  case APlatform of
    kdbSQLServer: Result := 'SQL Server';
    kdbSybase11: Result := 'Sybase System 11';
    kdbSybase12: Result := 'Sybase ASE ODBC Driver';
    kdbCtree: Result := 'Faircom Ctree';
    kdbAccess: Result := 'Microsoft Access Driver (*.mdb)';
    kdbDBIsam: Result := '---';
    kdbInterbase: Result := 'Intersolv Interbase ODBC Driver (*.gdb)'; // not that we would actually ever use this
    kdbDB2: Result := 'IBM DB2 ODBC DRIVER';
    kdbOracle8: Result := 'Oracle ODBC Driver';
  else
    Result := 'Unknown Platform ' + inttostr(ord(APlatform));
  end;
end;

{ TKDBOdbcExpress }
                           {
constructor TKDBOdbcExpress.create(AName : String; AIniFile : TIniFile; ASection : String; AIdent : String = '');
begin
  Create(AName, AIniFile.ReadInteger(ASection, 'MaxConnections', 20), AIniFile.ReadString(ASection, 'DSN', ''),
      AIniFile.ReadString(ASection, 'ODBCDriver', ''), AIniFile.ReadString(ASection, 'Server', ''),
      AIniFile.ReadString(ASection, 'Database', ''),  AIniFile.ReadString(ASection, 'Username', ''),
      AIniFile.ReadString(ASection, 'Password', ''));
end;

constructor TKDBOdbcExpress.create(AName : String; AMaxConnCount: Integer; ADSN, ADriver, AServer, ADatabase, AUsername, APassword: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBOdbcExpress.create';
begin
  inherited create(Aname, AMaxConnCount);
  FAttributes := TIdStringList.create;
  FDsn := ADSN;
  FDriver := ADriver;
  FServer := AServer;
  FDatabase := ADatabase;
  FUsername := AUsername;
  FPassword := APassword;
  FPlatform := RecogniseDriver(ADriver);

  if FPlatform = kdbAccess then
    begin
    FAttributes.add('DBQ=' + FDatabase)
    end
  else
    begin
    if FPlatform = kdbSybase11 then
      begin
      FAttributes.add('ServerName=' + FServer)
      end
    else
      begin
      FAttributes.add('Server=' + FServer);
      end;
    FAttributes.add('Database=' + FDatabase);
    end;
end;
                            }

end.

