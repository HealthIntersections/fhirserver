{ File: OVCL.Pas                                                     }
{ Description: ODBC Visual Component Library                         }
{ Author: Pieter A. Myburgh                                          }
{ Copyright: Korbitec (Pty) Ltd                                      }
{                                                                    }

unit OdbcExtras;

{!!}
{0.00-000  10 Jul 03 21:45  []       User: Grahame Grieve    File First added to CodeVault}

{.$.ObjExportAll On}

interface

uses
  Classes,
  SysUtils,
  DB,
  OdbcHeaders,
  OdbcImplementation,
  odbcCore;

type
  { TDataSourceType }
  TDataSourceType = (dsDefault, dsUser, dsSystem);

  { TTableType }
  TTableType = (ttTable, ttView, ttSystem);
  TTableTypeSet = set of TTableType;

const
  DefDataSourceType = dsDefault;

  DefUnique = False;
  DefScriptExt = '.SQL';
  DefExecMarker = 'go';
  DefNameConstraints = False;
  DefPrompt = False;
  DefSystem = False;
  DefReadOnly = False;
  DefTableType = [ttTable, ttView];
  DefMaxRows = 0;
  DefCommitCount = 1;

  cfNotNull = 1;
  cfUnique = 2;
  cfPrimaryKey = 4;

  TableStr = 'TABLE';
  ViewStr = 'VIEW';
  SystemStr = 'SYSTEM TABLE';

type
  { Non-Visual Components }
  TOESchema = class;
  TOEAdministrator = class;
  TOECatalog = class;


  { TStepEvent }
  TStepEvent = procedure (Sender: TObject;
                          Info: String) of object;

  { TSchemaColumn }
  TSchemaColumn = class
  private
    { Private declarations }
    FColumnName: String;
    FDataType: SQLSMALLINT;
    FPrecision: Integer;
    FFlags: Integer;
    FDefault: String;
    FForeignTable: String;
    FForeignColumn: String;

    procedure SetPrecision(APrecision: Integer);
    procedure SetForeignTable(AForeignTable: String);
    procedure SetForeignColumn(AForeignColumn: String);
  protected
    { Protected declarations }
  public
    { Public declarations }
    property ColumnName: String read FColumnName write FColumnName;
    property DataType: SQLSMALLINT read FDataType write FDataType
      default SQL_CHAR;
    property Precision: Integer read FPrecision write SetPrecision
      default 0;
    property Flags: Integer read FFlags write FFlags
      default 0;
    property Default: String read FDefault write FDefault;
    property ForeignTable: String read FForeignTable write SetForeignTable;
    property ForeignColumn: String read FForeignColumn write SetForeignColumn;

    constructor Create;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

  { TSchemaColumns }
  TSchemaColumns = class
  private
    { Private declarations }
    FList: TList;

    function GetItem(Index: Integer): TSchemaColumn;
    procedure SetItem(Index: Integer;
                      AColumn: TSchemaColumn);
    function GetCount: Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    property Items[Index: Integer]: TSchemaColumn read GetItem write SetItem; default;
    property ItemCount: Integer read GetCount;

    constructor Create;
    destructor Destroy; override;
    procedure AddItem;
    procedure InsertItem(Index: Integer);
    procedure DeleteItem(Index: Integer);
    procedure MoveItem(Index, NewIndex: Integer);
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Clear;
  published
    { Published declarations }
  end;

  TSchemaIndex = class;
  TSchemaIndexes = class;
  TSchemaTable = class;
  TSchemaTables = class;
  TSchemaView = class;
  TSchemaViews = class;

  { TSchemaIndex }
  TSchemaIndex = class
  private
    { Private declarations }
    FIndexes: TSchemaIndexes;
    FIndexName: String;
    FUnique: Boolean;
    FColumns: TStrings;

    procedure SetColumns(AColumns: TStrings);
  protected
    { Protected declarations }
  public
    { Public declarations }
    property IndexName: String read FIndexName write FIndexName;
    property Unique: Boolean read FUnique write FUnique
      default DefUnique;
    property Columns: TStrings read FColumns write SetColumns;

    constructor Create;
    destructor Destroy; override;
    procedure LoadIndex;
    procedure DropIndex;
  published
    { Published declarations }
  end;

  { TSchemaIndexes }
  TSchemaIndexes = class
  private
    { Private declarations }
    FTable: TSchemaTable;
    FList: TList;

    function GetItem(Index: Integer): TSchemaIndex;
    procedure SetItem(Index: Integer;
                      AIndex: TSchemaIndex);
    function GetCount: Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    property Items[Index: Integer]: TSchemaIndex read GetItem write SetItem; default;
    property ItemCount: Integer read GetCount;

    constructor Create;
    destructor Destroy; override;
    procedure AddItem;
    procedure InsertItem(Index: Integer);
    procedure DeleteItem(Index: Integer);
    procedure MoveItem(Index, NewIndex: Integer);
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Clear;
  published
    { Published declarations }
  end;

  { TSchemaTable }
  TSchemaTable = class
  private
    { Private declarations }
    FTables: TSchemaTables;
    FTableOwner: String;
    FTableName: String;
    FColumns: TSchemaColumns;
    FIndexes: TSchemaIndexes;

    procedure SetColumns(AColumns: TSchemaColumns);
    procedure SetIndexes(AIndexes: TSchemaIndexes);
  protected
    { Protected declarations }
  public
    { Public declarations }
    property TableOwner: String read FTableOwner write FTableOwner;
    property TableName: String read FTableName write FTableName;
    property Columns: TSchemaColumns read FColumns write SetColumns;
    property Indexes: TSchemaIndexes read FIndexes write SetIndexes;

    constructor Create;
    destructor Destroy; override;
    procedure LoadTable;
    procedure DropTable;
    procedure LoadIndexes;
    procedure DropIndexes(IgnoreErrors: Boolean);
  published
    { Published declarations }
  end;

  { TSchemaTables }
  TSchemaTables = class(TPersistent)
  private
    { Private declarations }
    FSchema: TOESchema;
    FList: TList;

    function GetItem(Index: Integer): TSchemaTable;
    procedure SetItem(Index: Integer;
                      ATable: TSchemaTable);
    function GetCount: Integer;
    procedure ReadProperties(Reader: TReader);
    procedure WriteProperties(Writer: TWriter);
  protected
    { Protected declarations }
    procedure DefineProperties(Filer: TFiler); override;
  public
    { Public declarations }
    property Items[Index: Integer]: TSchemaTable read GetItem write SetItem; default;
    property ItemCount: Integer read GetCount;

    constructor Create;
    destructor Destroy; override;
    procedure AddItem;
    procedure InsertItem(Index: Integer);
    procedure DeleteItem(Index: Integer);
    procedure MoveItem(Index, NewIndex: Integer);
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Clear;
  published
    { Published declarations }
  end;

  { TSchemaView }
  TSchemaView = class
  private
    { Private declarations }
    FViews: TSchemaViews;
    FViewOwner: String;
    FViewName: String;
    FColumns: TStrings;
    FSelectSQL: String;

    procedure SetColumns(AColumns: TStrings);
  protected
    { Protected declarations }
  public
    { Public declarations }
    property ViewOwner: String read FViewOwner write FViewOwner;
    property ViewName: String read FViewName write FViewName;
    property Columns: TStrings read FColumns write SetColumns;
    property SelectSQL: String read FSelectSQL write FSelectSQL;

    constructor Create;
    destructor Destroy; override;
    procedure LoadView;
    procedure DropView;
  published
    { Published declarations }
  end;

  { TSchemaViews }
  TSchemaViews = class(TPersistent)
  private
    { Private declarations }
    FSchema: TOESchema;
    FList: TList;

    function GetItem(Index: Integer): TSchemaView;
    procedure SetItem(Index: Integer;
                      AView: TSchemaView);
    function GetCount: Integer;
    procedure ReadProperties(Reader: TReader);
    procedure WriteProperties(Writer: TWriter);
  protected
    { Protected declarations }
    procedure DefineProperties(Filer: TFiler); override;
  public
    { Public declarations }
    property Items[Index: Integer]: TSchemaView read GetItem write SetItem; default;
    property ItemCount: Integer read GetCount;

    constructor Create;
    destructor Destroy; override;
    procedure AddItem;
    procedure InsertItem(Index: Integer);
    procedure DeleteItem(Index: Integer);
    procedure MoveItem(Index, NewIndex: Integer);
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Clear;
  published
    { Published declarations }
  end;

  { TOESchema }
  TOESchema = class(TComponent)
  private
    { Private declarations }
    FHdbc: THdbc;
    FHstmt: THstmt;
    FTables: TSchemaTables;
    FViews: TSchemaViews;
    FToFile: Boolean;
    FText: Text;
    FExecMarker: String;
    FNameConstraints: Boolean;
    FAborted: Boolean;
    FOnProgress: TStepEvent;

    procedure SetHdbc(AHdbc: THdbc);
    procedure SetTables(ATables: TSchemaTables);
    procedure SetViews(AViews: TSchemaViews);
    procedure SetExecMarker(AExecMarker: String);
    function Prefix(AOwner, ATable: String): String;
    procedure WriteLine(ctSQL: String);
    function ReadLine: String;
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
    procedure SplitColumn(AIndexColumn: string;
                          var AColumn: String;
                          var AType: String);
    procedure DoProgress(Info: String); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadTables;
    procedure DropTables(IgnoreErrors: Boolean);
    procedure LoadViews;
    procedure DropViews(IgnoreErrors: Boolean);
    procedure GenScript(FileName: String);
    procedure RunScript(FileName: String);
    procedure Clear;
    procedure Terminate;
    procedure Abort;

    property Aborted: Boolean read FAborted;
  published
    { Published declarations }
    property hDbc: THdbc read FHdbc write SetHdbc
      default nil;
    property Tables: TSchemaTables read FTables write SetTables;
    property Views: TSchemaViews read FViews write SetViews;
    property ExecMarker: String read FExecMarker write SetExecMarker;
    property NameConstraints: Boolean read FNameConstraints write FNameConstraints;
    property OnProgress: TStepEvent read FOnProgress write FOnProgress;
  end;

  { TOEAdministrator }
  TOEAdministrator = class(TComponent)
  private
    { Private declarations }
    FPrompt: Boolean;
    FDataSourceType: TDataSourceType;
    FDriver: String;
    FDataSource: String;
    FUserName: String;
    FPassword: String;
    FAttributes: TStrings;
    FDataSourceNames: TStrings;
    FDataSourceDrivers: TStrings;
    FDriverNames: TStrings;
    FGetDataSources: Boolean;
    FGetDrivers: Boolean;

    procedure RetrieveDataSources;
    procedure RetrieveDrivers;

    procedure SetDataSourceType(ADataSourceType: TDataSourceType);
    procedure SetDataSource(ADataSource: String);
    procedure SetAttributes(AAttributes: TStrings);
    function Configure(Request: SQLUSMALLINT): Boolean;
    function GetDataSources: TStrings;
    function GetDrivers: TStrings;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Add: Boolean;
    function Modify: Boolean;
    function Remove: Boolean;
    function Valid(ADataSource: String): Boolean;
    procedure Refresh;
    function DataSourceDriver(ADataSource: String): String;

    property DataSources: TStrings read GetDataSources;
    property Drivers: TStrings read GetDrivers;
  published
    { Published declarations }
    property Prompt: Boolean read FPrompt write FPrompt
      default DefPrompt;
    property DataSourceType: TDataSourceType read FDataSourceType write SetDataSourceType
      default DefDataSourceType;
    property Driver: String read FDriver write FDriver;
    property DataSource: String read FDataSource write SetDataSource;
    property UserName: String read FUserName write FUserName;
    property Password: String read FPassword write FPassword;
    property Attributes: TStrings read FAttributes write SetAttributes;
  end;

  { TCatalogColumn }
  TCatalogColumn = class
  private
    { Private declarations }
    FColumnName: String;
    FColumnType: SQLSMALLINT;
    FDataType: SQLSMALLINT;
    FDataTypeName: String;
    FPrecision: SQLINTEGER;
    FScale: SQLSMALLINT;
    FRadix: SQLSMALLINT;
    FNullable: SQLSMALLINT;
    FDefault: String;
    FDescription: String;
  protected
    { Protected declarations }
  public
    { Public declarations }
    property ColumnName: String read FColumnName;
    property ColumnType: SQLSMALLINT read FColumnType;
    property DataType: SQLSMALLINT read FDataType;
    property DataTypeName: String read FDataTypeName;
    property Precision: SQLINTEGER read FPrecision;
    property Scale: SQLSMALLINT read FScale;
    property Radix: SQLSMALLINT read FRadix;
    property Nullable: SQLSMALLINT read FNullable;
    property Default: String read FDefault;
    property Description: String read FDescription;

    constructor Create;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

  { TCatalogColumns }
  TCatalogColumns = class
  private
    { Private declarations }
    FList: TList;

    procedure FreeItems;
    function AddItem: TCatalogColumn;
    function GetItem(Index: Integer): TCatalogColumn;
    procedure SetItem(Index: Integer;
                      AColumn: TCatalogColumn);
    function GetCount: Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    property Items[Index: Integer]: TCatalogColumn read GetItem write SetItem; default;
    property ItemCount: Integer read GetCount;

    constructor Create;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

  { TCatalogTable }
  TCatalogTable = class
  private
    { Private declarations }
    FCatalog: TOECatalog;
    FTableOwner: String;
    FTableName: String;
    FTableType: TTableType;
    FDescription: String;
    FColumns: TCatalogColumns;
    FColumnNames: TStrings;
    FPrimaryKeys: TStrings;
    FForeignKeys: TStrings;
    FForeignReferences: TStrings;
    FIndexes: TStrings;
    FUniqueKey: String;
    FGetColumns: Boolean;
    FGetPrimaryKeys: Boolean;
    FGetForeignKeys: Boolean;
    FGetForeignReferences: Boolean;
    FGetIndexes: Boolean;
    FGetUniqueKey: Boolean;

    procedure RetrieveColumns;
    procedure RetrievePrimaryKeys;
    procedure RetrieveForeignKeys;
    procedure RetrieveForeignReferences;
    procedure RetrieveIndexes;
    procedure RetrieveUniqueKey;

    function GetColumns: TCatalogColumns;
    function GetColumnNames: TStrings;
    function GetPrimaryKeys: TStrings;
    function GetForeignKeys: TStrings;
    function GetForeignReferences: TStrings;
    function GetIndexes: TStrings;
    function GetUniqueKey: String;
  protected
    { Protected declarations }
  public
    { Public declarations }
    property TableOwner: String read FTableOwner;
    property TableName: String read FTableName;
    property TableType: TTableType read FTableType;
    property Description: String read FDescription;
    property Columns: TCatalogColumns read GetColumns;
    property ColumnNames: TStrings read GetColumnNames;
    property PrimaryKeys: TStrings read GetPrimaryKeys;
    property ForeignKeys: TStrings read GetForeignKeys;
    property ForeignReferences: TStrings read GetForeignReferences;
    property Indexes: TStrings read GetIndexes;
    property UniqueKey: String read GetUniqueKey;

    constructor Create;
    destructor Destroy; override;
    function ColumnByName(AColumnName: String): TCatalogColumn;
    procedure Refresh;
  published
    { Published declarations }
  end;

  { TCatalogTables }
  TCatalogTables = class
  private
    { Private declarations }
    FCatalog: TOECatalog;
    FList: TList;

    procedure FreeItems;
    function AddItem: TCatalogTable;
    function GetItem(Index: Integer): TCatalogTable;
    procedure SetItem(Index: Integer;
                      ATable: TCatalogTable);
    function GetCount: Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    property Items[Index: Integer]: TCatalogTable read GetItem write SetItem; default;
    property ItemCount: Integer read GetCount;

    constructor Create;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

  { TCatalogProcedure }
  TCatalogProcedure = class
  private
    { Private declarations }
    FCatalog: TOECatalog;
    FProcedureOwner: String;
    FProcedureName: String;
    FProcedureType: SQLSMALLINT;
    FDescription: String;
    FColumns: TCatalogColumns;
    FColumnNames: TStrings;
    FGetColumns: Boolean;

    procedure RetrieveColumns;

    function GetColumns: TCatalogColumns;
    function GetColumnNames: TStrings;
  protected
    { Protected declarations }
  public
    { Public declarations }
    property ProcedureOwner: String read FProcedureOwner;
    property ProcedureName: String read FProcedureName;
    property ProcedureType: SQLSMALLINT read FProcedureType;
    property Description: String read FDescription;
    property Columns: TCatalogColumns read GetColumns;
    property ColumnNames: TStrings read GetColumnNames;

    constructor Create;
    destructor Destroy; override;
    function ColumnByName(AColumnName: String): TCatalogColumn;
    procedure Refresh;
  published
    { Published declarations }
  end;

  { TCatalogProcedures }
  TCatalogProcedures = class
  private
    { Private declarations }
    FCatalog: TOECatalog;
    FList: TList;

    procedure FreeItems;
    function AddItem: TCatalogProcedure;
    function GetItem(Index: Integer): TCatalogProcedure;
    procedure SetItem(Index: Integer;
                      AProcedure: TCatalogProcedure);
    function GetCount: Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    property Items[Index: Integer]: TCatalogProcedure read GetItem write SetItem; default;
    property ItemCount: Integer read GetCount;

    constructor Create;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

  { TOECatalog }
  TOECatalog = class(TComponent)
  private
    { Private declarations }
    FHdbc: THdbc;
    FHstmt: THstmt;
    FTables: TCatalogTables;
    FProcedures: TCatalogProcedures;
    FTableNames: TStrings;
    FProcedureNames: TStrings;
    FTableOwner: String;
    FTableName: String;
    FTableType: TTableTypeSet;
    FProcedureOwner: String;
    FProcedureName: String;
    FGetTables: Boolean;
    FGetProcedures: Boolean;

    procedure RetrieveTables;
    procedure RetrieveProcedures;

    function GetTables: TCatalogTables;
    function GetProcedures: TCatalogProcedures;
    function GetTableNames: TStrings;
    function GetProcedureNames: TStrings;
    procedure SetHdbc(AHdbc: THdbc);
    procedure SetTableOwner(ATableOwner: String);
    procedure SetTableName(ATableName: String);
    procedure SetTableType(ATableType: TTableTypeSet);
    procedure SetProcedureOwner(AProcedureOwner: String);
    procedure SetProcedureName(AProcedureName: String);
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function TableByName(ATableOwner, ATableName: String): TCatalogTable;
    function ProcedureByName(AProcedureOwner, AProcedureName: String): TCatalogProcedure;
    procedure Refresh;
    procedure Terminate;
    procedure ParseForeignKey(ForeignKey: String;
                              var ColumnName, ForeignOwner, ForeignTable, ForeignColumn: String);
    procedure ParseIndex(Index: String;
                         var IndexName: String;
                         var Unique: Boolean;
                         ColumnNames: TStrings);
    procedure ParseUniqueKey(UniqueKey: String;
                             ColumnNames: TStrings);

    property Tables: TCatalogTables read GetTables;
    property Procedures: TCatalogProcedures read GetProcedures;
    property TableNames: TStrings read GetTableNames;
    property ProcedureNames: TStrings read GetProcedureNames;
  published
    { Published declarations }
    property hDbc: THdbc read FHdbc write SetHdbc
      default nil;
    property TableOwner: String read FTableOwner write SetTableOwner;
    property TableName: String read FTableName write SetTableName;
    property TableType: TTableTypeSet read FTableType write SetTableType
      default DefTableType;
    property ProcedureOwner: String read FProcedureOwner write SetProcedureOwner;
    property ProcedureName: String read FProcedureName write SetProcedureName;
  end;

  { TBindEvent }
  TBindEvent = procedure (Sender: TObject;
                          Table: String;
                          Hstmt: THstmt) of object;

  { TOEBulkCopy }
  TOEBulkCopy = class(TComponent)
  private
    { Private declarations }
    FHdbcSource, FHdbcTarget: THdbc;
    FHstmtSource, FHstmtTarget: THstmt;
    FSQLSource, FSQLTarget: String;
    FAborted: Boolean;
    FExecMarker: String;
    FRowSetSize: SQLUINTEGER;
    FRowsAffected: SQLINTEGER;
    FMaxRows: SQLUINTEGER;
    FCommitCount: SQLUINTEGER;
    FOnProgress: TStepEvent;
    FOnBindParams: TBindEvent;

    function GetRowsAffected: SQLINTEGER;
    procedure SetExecMarker(AExecMarker: String);
    procedure SetRowSetSize(ARowSetSize: SQLUINTEGER);
    procedure ExecuteCopy(ASQLSource, ASQLTarget: String);
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
    procedure DoProgress(Info: String); virtual;
    procedure DoBindParams(Table: String;
                           Hstmt: THstmt); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Execute;
    procedure RunScript(FileName: String);
    procedure Terminate;
    procedure Abort;

    property RowsAffected: SQLINTEGER read GetRowsAffected;
    property Aborted: Boolean read FAborted;
  published
    { Published declarations }
    property hDbcSource: THdbc read FHdbcSource write FHdbcSource
      default nil;
    property hDbcTarget: THdbc read FHdbcTarget write FHdbcTarget
      default nil;
    property SQLSource: String read FSQLSource write FSQLSource;
    property SQLTarget: String read FSQLTarget write FSQLTarget;
    property ExecMarker: String read FExecMarker write SetExecMarker;
    property RowSetSize: SQLUINTEGER read FRowSetSize write SetRowSetSize
      default DefRowSetSize;
    property MaxRows: SQLUINTEGER read FMaxRows write FMaxRows
      default DefMaxRows;
    property CommitCount: SQLUINTEGER read FCommitCount write FCommitCount
      default DefCommitCount;
    property OnProgress: TStepEvent read FOnProgress write FOnProgress;
    property OnBindParams: TBindEvent read FOnBindParams write FOnBindParams;
  end;

{ Private Utilities }
function TableTypeToString(TableType: TTableType): String;
function StringToTableType(S: String): TTableType;

implementation

{ Private Utilities }

function TableTypeToString(TableType: TTableType): String;
begin
  case TableType of
    ttTable:
      Result:= TableStr;
    ttView:
      Result:= ViewStr;
    ttSystem:
      Result:= SystemStr;
  end;
end;

function StringToTableType(S: String): TTableType;
begin
  S:= UpperCase(Trim(S));
  if S = TableStr then
    Result:= ttTable
  else if S = ViewStr then
    Result:= ttView
  else if S = SystemStr then
    Result:= ttSystem
  else
    Result:= ttTable;
end;

function ExtractTable(SQL, Locator: String): String;
var
  Loc: Integer;
begin
  SQL:= StringReplace(SQL, EnterString, ' ', [rfReplaceAll, rfIgnoreCase]);
  Locator:= ' '+Locator+' ';
  Loc:= Pos(Locator, UpperCase(SQL));
  if Loc > 0 then
  begin
    Result:= Trim(Copy(SQL, Loc+Length(Locator), Length(SQL)-Loc-Length(Locator)+1));
    Loc:= Pos(' ', Result);
    if Loc > 0 then
      Result:= Copy(Result, 1, Loc-1);  //can include table owner
  end
  else
    Result:= '';
end;

{ TSchemaColumn }

procedure TSchemaColumn.SetPrecision(APrecision: Integer);
begin
  if (APrecision <> FPrecision) and (APrecision >= 0) then
    FPrecision:= APrecision;
end;

procedure TSchemaColumn.SetForeignTable(AForeignTable: String);
begin
  if AForeignTable <> FForeignTable then
  begin
    FForeignTable:= AForeignTable;
    FForeignColumn:= '';
  end;
end;

procedure TSchemaColumn.SetForeignColumn(AForeignColumn: String);
begin
  if AForeignColumn <> FForeignColumn then
  begin
    if FForeignTable <> '' then
      FForeignColumn:= AForeignColumn;
  end;
end;

constructor TSchemaColumn.Create;
begin
  FColumnName:= '';
  FDataType:= SQL_CHAR;
  FPrecision:= 0;
  FFlags:= 0;
  FDefault:= '';
  FForeignTable:= '';
  FForeignColumn:= '';
end;

destructor TSchemaColumn.Destroy;
begin

  inherited Destroy;
end;

{ TSchemaColumns }

function TSchemaColumns.GetItem(Index: Integer): TSchemaColumn;
begin
  Result:= TSchemaColumn(FList[Index]);
end;

procedure TSchemaColumns.SetItem(Index: Integer;
                           AColumn: TSchemaColumn);
begin
end;

function TSchemaColumns.GetCount: Integer;
begin
  Result:= FList.Count;
end;

constructor TSchemaColumns.Create;
begin
  FList:= TList.Create;
  FList.Clear;
end;

destructor TSchemaColumns.Destroy;
begin
  Clear;
  FList.Free;

  inherited Destroy;
end;

procedure TSchemaColumns.AddItem;
var
  temp: TSchemaColumn;
begin
  temp:= TSchemaColumn.Create;
  FList.Add(temp);
end;

procedure TSchemaColumns.InsertItem(Index: Integer);
var
  temp: TSchemaColumn;
begin
  temp:= TSchemaColumn.Create;
  FList.Insert(Index, temp);
end;

procedure TSchemaColumns.DeleteItem(Index: Integer);
begin
  TSchemaColumn(FList[Index]).Free;
  FList.Delete(Index);
end;

procedure TSchemaColumns.MoveItem(Index, NewIndex: Integer);
begin
  FList.Move(Index, NewIndex);
end;

procedure TSchemaColumns.ExchangeItems(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

procedure TSchemaColumns.Clear;
var
  i: Integer;
begin
  for i:= 0 to FList.Count-1 do
    TSchemaColumn(FList[i]).Free;
  FList.Clear;
end;

{ TSchemaIndex }

procedure TSchemaIndex.SetColumns(AColumns: TStrings);
begin
end;

constructor TSchemaIndex.Create;
begin
  FIndexName:= '';
  FUnique:= DefUnique;
  FColumns:= TStringList.Create;
end;

destructor TSchemaIndex.Destroy;
begin
  FColumns.Free;

  inherited Destroy;
end;

procedure TSchemaIndex.LoadIndex;
var
  k: Integer;
  ctSQL: String;
begin
  if (Trim(FIndexes.FTable.TableName) = '') or (Trim(IndexName) = '') then
    Exit;

  if Unique then
    ctSQL:= 'CREATE UNIQUE INDEX '+Quoted(IndexName)+' ON '
  else
    ctSQL:= 'CREATE INDEX '+Quoted(IndexName)+ ' ON ';
  with FIndexes.FTable do
    ctSQL:= ctSQL+FTables.FSchema.Prefix(TableOwner, TableName)+' (';

  for k:= 0 to Columns.Count-1 do
    ctSQL:= ctSQL+Quoted(Columns[k])+', ';
  SetLength(ctSQL, Length(ctSQL)-2);
  ctSQL:= ctSQL+')';

  if FIndexes.FTable.FTables.FSchema.FToFile then
    FIndexes.FTable.FTables.FSchema.WriteLine(ctSQL)
  else
    with FIndexes.FTable.FTables.FSchema.FHstmt do
    begin
      SQL:= ctSQL;
      Prepare;
      Execute;
    end;
end;

procedure TSchemaIndex.DropIndex;
var
  ctSQL: String;
begin
  if IndexName <> '' then
  begin
    with FIndexes.FTable do
      ctSQL:= 'DROP INDEX '+FTables.FSchema.Prefix(TableOwner, TableName)+'.'+Quoted(IndexName);
    if FIndexes.FTable.FTables.FSchema.FToFile then
      FIndexes.FTable.FTables.FSchema.WriteLine(ctSQL)
    else
      with FIndexes.FTable.FTables.FSchema.FHstmt do
      begin
        SQL:= ctSQL;
        Prepare;
        Execute;
      end;
  end;
end;

{ TSchemaIndexes }

function TSchemaIndexes.GetItem(Index: Integer): TSchemaIndex;
begin
  Result:= TSchemaIndex(FList[Index]);
end;

procedure TSchemaIndexes.SetItem(Index: Integer;
                           AIndex: TSchemaIndex);
begin
end;

function TSchemaIndexes.GetCount: Integer;
begin
  Result:= FList.Count;
end;

constructor TSchemaIndexes.Create;
begin
  FList:= TList.Create;
  FList.Clear;
end;

destructor TSchemaIndexes.Destroy;
begin
  Clear;
  FList.Free;

  inherited Destroy;
end;

procedure TSchemaIndexes.AddItem;
var
  temp: TSchemaIndex;
begin
  temp:= TSchemaIndex.Create;
  temp.FIndexes:= Self;
  FList.Add(temp);
end;

procedure TSchemaIndexes.InsertItem(Index: Integer);
var
  temp: TSchemaIndex;
begin
  temp:= TSchemaIndex.Create;
  temp.FIndexes:= Self;
  FList.Insert(Index, temp);
end;

procedure TSchemaIndexes.DeleteItem(Index: Integer);
begin
  TSchemaIndex(FList[Index]).Free;
  FList.Delete(Index);
end;

procedure TSchemaIndexes.MoveItem(Index, NewIndex: Integer);
begin
  FList.Move(Index, NewIndex);
end;

procedure TSchemaIndexes.ExchangeItems(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

procedure TSchemaIndexes.Clear;
var
  i: Integer;
begin
  for i:= 0 to FList.Count-1 do
    TSchemaIndex(FList[i]).Free;
  FList.Clear;
end;

{ TSchemaTable }

procedure TSchemaTable.SetColumns(AColumns: TSchemaColumns);
begin
end;

procedure TSchemaTable.SetIndexes(AIndexes: TSchemaIndexes);
begin
end;

constructor TSchemaTable.Create;
begin
  FTableOwner:= '';
  FTableName:= '';
  FColumns:= TSchemaColumns.Create;
  FIndexes:= TSchemaIndexes.Create;
  FIndexes.FTable:= Self;
end;

destructor TSchemaTable.Destroy;
begin
  FColumns.Free;
  FIndexes.Free;

  inherited Destroy;
end;

procedure TSchemaTable.LoadTable;
var
  j: Integer;
  ctSQL: String;
  Found: Boolean;
  ColTypeString: String;
  Constraint: String;
  ConstraintCount: Integer;

  procedure NameConstraint;
  begin
    if FTables.FSchema.FNameConstraints then
    begin
      Inc(ConstraintCount);
      ctSQL:= ctSQL+' CONSTRAINT '+Constraint+IntToStr(ConstraintCount);
    end;
  end;

begin
  if (Trim(TableName) = '') or (Columns.ItemCount = 0) then
    Exit;

  Constraint:= StringReplace(TableName, ' ', '_', [rfReplaceAll, rfIgnoreCase])+'Constraint';
  ConstraintCount:= 0;

  ctSQL:= 'CREATE TABLE '+FTables.FSchema.Prefix(TableOwner, TableName)+' (';

  for j:= 0 to Columns.ItemCount-1 do
    with Columns[j] do
    begin
      ColTypeString:= FTables.FSchema.FHstmt.TypeString(DataType, Precision);
      if Trim(ColTypeString) = '' then
        raise EODBCExpress.Create('Data type for column "'+ColumnName+'" in table "'+TableName+'" not supported.');
      FTables.FSchema.FHstmt.Close;

      ctSQL:= ctSQL+Quoted(ColumnName)+' '+ColTypeString;
      if Trim(Default) <> '' then
        ctSQL:= ctSQL+' DEFAULT '+Default;
      if (Flags and cfNotNull) = cfNotNull then
      begin
        NameConstraint;
        ctSQL:= ctSQL+' NOT NULL';
      end;
      if (Flags and cfUnique) = cfUnique then
      begin
        NameConstraint;
        ctSQL:= ctSQL+' UNIQUE';
      end;
      ctSQL:= ctSQL+', ';
    end;

  Found:= False;
  for j:= 0 to Columns.ItemCount-1 do
    with Columns[j] do
      if (Flags and cfPrimaryKey) = cfPrimaryKey then
      begin
        if Found then
          ctSQL:= ctSQL+', '+Quoted(ColumnName)
        else
        begin
          NameConstraint;
          ctSQL:= ctSQL+' PRIMARY KEY ('+Quoted(ColumnName);
          Found:= True;
        end;
      end;
  if Found then
    ctSQL:= ctSQL+'), ';

  for j:= 0 to Columns.ItemCount-1 do
    with Columns[j] do
      if ForeignTable <> '' then
      begin
        NameConstraint;
        ctSQL:= ctSQL+' FOREIGN KEY ('+Quoted(ColumnName)+') REFERENCES '+
          ForeignTable+' ('+Quoted(ForeignColumn)+'), ';
      end;

  SetLength(ctSQL, Length(ctSQL)-2);
  ctSQL:= ctSQL+')';

  if FTables.FSchema.FToFile then
    FTables.FSchema.WriteLine(ctSQL)
  else
    with FTables.FSchema.FHstmt do
    begin
      SQL:= ctSQL;
      Prepare;
      Execute;
    end;
end;

procedure TSchemaTable.DropTable;
var
  ctSQL: String;
begin
  if TableName <> '' then
  begin
    ctSQL:= 'DROP TABLE '+FTables.FSchema.Prefix(TableOwner, TableName);
    if FTables.FSchema.FToFile then
      FTables.FSchema.WriteLine(ctSQL)
    else
      with FTables.FSchema.FHstmt do
      begin
        SQL:= ctSQL;
        Prepare;
        Execute;
      end;
  end;
end;

procedure TSchemaTable.LoadIndexes;
var
  i: Integer;
begin
  for i:= 0 to Indexes.ItemCount-1 do
    Indexes[i].LoadIndex;
end;

procedure TSchemaTable.DropIndexes(IgnoreErrors: Boolean);
var
  i: Integer;
begin
  for i:= Indexes.ItemCount-1 downto 0 do
  try
    Indexes[i].DropIndex;
  except
    if not IgnoreErrors then
      raise;
  end;
end;

{ TSchemaTables }

function TSchemaTables.GetItem(Index: Integer): TSchemaTable;
begin
  Result:= TSchemaTable(FList[Index]);
end;

procedure TSchemaTables.SetItem(Index: Integer;
                          ATable: TSchemaTable);
begin
end;

function TSchemaTables.GetCount: Integer;
begin
  Result:= FList.Count;
end;

procedure TSchemaTables.ReadProperties(Reader: TReader);
var
  //CurVer: Boolean;
  temp: String;
begin
  Reader.ReadListBegin;
  temp:= Reader.ReadString;
  //CurVer:= temp = 'OE'+OEVER;

  Reader.ReadListBegin;
  while not Reader.EndOfList do
  begin
    AddItem;
    with Items[ItemCount-1] do
    begin
      TableOwner:= Reader.ReadString;
      TableName:= Reader.ReadString;
      Reader.ReadListBegin;
      while not Reader.EndOfList do
      begin
        Columns.AddItem;
        with Columns[Columns.ItemCount-1] do
        begin
          ColumnName:= Reader.ReadString;
          DataType:= Reader.ReadInteger;
          Precision:= Reader.ReadInteger;
          Flags:= Reader.ReadInteger;
          Default:= Reader.ReadString;
          ForeignTable:= Reader.ReadString;
          ForeignColumn:= Reader.ReadString;
        end;
      end;
      Reader.ReadListEnd;
      Reader.ReadListBegin;
      while not Reader.EndOfList do
      begin
        Indexes.AddItem;
        Indexes[Indexes.ItemCount-1].IndexName:= Reader.ReadString;
        Indexes[Indexes.ItemCount-1].Unique:= Reader.ReadBoolean;
        Reader.ReadListBegin;
        Indexes[Indexes.ItemCount-1].Columns.Clear;
        while not Reader.EndOfList do
          Indexes[Indexes.ItemCount-1].Columns.Add(Reader.ReadString);
        Reader.ReadListEnd;
      end;
      Reader.ReadListEnd;
    end;
  end;
  Reader.ReadListEnd;

  Reader.ReadListEnd;
end;

procedure TSchemaTables.WriteProperties(Writer: TWriter);
var
  i, j, k: Integer;
begin
  Writer.WriteListBegin;
  Writer.WriteString('OE'+OEVER);

  Writer.WriteListBegin;
  for i:= 0 to ItemCount-1 do
    with Items[i] do
    begin
      Writer.WriteString(TableOwner);
      Writer.WriteString(TableName);
      Writer.WriteListBegin;
      for j:= 0 to Columns.ItemCount-1 do
        with Columns[j] do
        begin
          Writer.WriteString(ColumnName);
          Writer.WriteInteger(DataType);
          Writer.WriteInteger(Precision);
          Writer.WriteInteger(Flags);
          Writer.WriteString(Default);
          Writer.WriteString(ForeignTable);
          Writer.WriteString(ForeignColumn);
        end;
      Writer.WriteListEnd;
      Writer.WriteListBegin;
      for j:= 0 to Indexes.ItemCount-1 do
      begin
        Writer.WriteString(Indexes[j].IndexName);
        Writer.WriteBoolean(Indexes[j].Unique);
        Writer.WriteListBegin;
        for k:= 0 to Indexes[j].Columns.Count-1 do
          Writer.WriteString(Indexes[j].Columns[k]);
        Writer.WriteListEnd;
      end;
      Writer.WriteListEnd;
    end;
  Writer.WriteListEnd;

  Writer.WriteListEnd;
end;

procedure TSchemaTables.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Tables', ReadProperties, WriteProperties, True);
end;

constructor TSchemaTables.Create;
begin
  FList:= TList.Create;
  FList.Clear;
end;

destructor TSchemaTables.Destroy;
begin
  Clear;
  FList.Free;

  inherited Destroy;
end;

procedure TSchemaTables.AddItem;
var
  temp: TSchemaTable;
begin
  temp:= TSchemaTable.Create;
  temp.FTables:= Self;
  FList.Add(temp);
end;

procedure TSchemaTables.InsertItem(Index: Integer);
var
  temp: TSchemaTable;
begin
  temp:= TSchemaTable.Create;
  temp.FTables:= Self;
  FList.Insert(Index, temp);
end;

procedure TSchemaTables.DeleteItem(Index: Integer);
begin
  TSchemaTable(FList[Index]).Free;
  FList.Delete(Index);
end;

procedure TSchemaTables.MoveItem(Index, NewIndex: Integer);
begin
  FList.Move(Index, NewIndex);
end;

procedure TSchemaTables.ExchangeItems(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

procedure TSchemaTables.Clear;
var
  i: Integer;
begin
  for i:= 0 to FList.Count-1 do
    TSchemaTable(FList[i]).Free;
  FList.Clear;
end;

{ TSchemaView }

procedure TSchemaView.SetColumns(AColumns: TStrings);
begin
end;

constructor TSchemaView.Create;
begin
  FViewOwner:= '';
  FViewName:= '';
  FColumns:= TStringList.Create;
  FSelectSQL:= '';
end;

destructor TSchemaView.Destroy;
begin
  FColumns.Free;

  inherited Destroy;
end;

procedure TSchemaView.LoadView;
var
  j: Integer;
  ctSQL: String;
begin
  if Trim(ViewName) = '' then
    Exit;

  ctSQL:= 'CREATE VIEW '+FViews.FSchema.Prefix(ViewOwner, ViewName);

  if (Columns.Count > 0) and (Trim(Columns[0]) <> '') then
  begin
    ctSQL:= ctSQL+' (';

    for j:= 0 to Columns.Count-1 do
      ctSQL:= ctSQL+Quoted(Columns[j])+', ';
    SetLength(ctSQL, Length(ctSQL)-2);
    ctSQL:= ctSQL+')';
  end;

  ctSQl:= ctSQL+' AS '+SelectSQL;

  if FViews.FSchema.FToFile then
    FViews.FSchema.WriteLine(ctSQL)
  else
    with FViews.FSchema.FHstmt do
    begin
      SQL:= ctSQL;
      Prepare;
      Execute;
    end;
end;

procedure TSchemaView.DropView;
var
  ctSQL: String;
begin
  if Trim(ViewName) <> '' then
  begin
    ctSQL:= 'DROP VIEW '+FViews.FSchema.Prefix(ViewOwner, ViewName);
    if FViews.FSchema.FToFile then
      FViews.FSchema.WriteLine(ctSQL)
    else
      with FViews.FSchema.FHstmt do
      begin
        SQL:= ctSQL;
        Prepare;
        Execute;
      end;
  end;
end;

{ TSchemaViews }

function TSchemaViews.GetItem(Index: Integer): TSchemaView;
begin
  Result:= TSchemaView(FList[Index]);
end;

procedure TSchemaViews.SetItem(Index: Integer;
                         AView: TSchemaView);
begin
end;

function TSchemaViews.GetCount: Integer;
begin
  Result:= FList.Count;
end;

procedure TSchemaViews.ReadProperties(Reader: TReader);
var
  //CurVer: Boolean;
  temp: String;
begin
  Reader.ReadListBegin;
  temp:= Reader.ReadString;
  //CurVer:= temp = 'OE'+OEVER;

  Reader.ReadListBegin;
  while not Reader.EndOfList do
  begin
    AddItem;
    with Items[ItemCount-1] do
    begin
      ViewOwner:= Reader.ReadString;
      ViewName:= Reader.ReadString;
      Reader.ReadListBegin;
      Columns.Clear;
      while not Reader.EndOfList do
        Columns.Add(Reader.ReadString);
      Reader.ReadListEnd;
      SelectSQL:= Reader.ReadString;
    end;
  end;
  Reader.ReadListEnd;

  Reader.ReadListEnd;
end;

procedure TSchemaViews.WriteProperties(Writer: TWriter);
var
  i, j: Integer;
begin
  Writer.WriteListBegin;
  Writer.WriteString('OE'+OEVER);

  Writer.WriteListBegin;
  for i:= 0 to ItemCount-1 do
    with Items[i] do
    begin
      Writer.WriteString(ViewOwner);
      Writer.WriteString(ViewName);
      Writer.WriteListBegin;
      for j:= 0 to Columns.Count-1 do
        Writer.WriteString(Columns[j]);
      Writer.WriteListEnd;
      Writer.WriteString(SelectSQL);
    end;
  Writer.WriteListEnd;

  Writer.WriteListEnd;
end;

procedure TSchemaViews.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Views', ReadProperties, WriteProperties, True);
end;

constructor TSchemaViews.Create;
begin
  FList:= TList.Create;
  FList.Clear;
end;

destructor TSchemaViews.Destroy;
begin
  Clear;
  FList.Free;

  inherited Destroy;
end;

procedure TSchemaViews.AddItem;
var
  temp: TSchemaView;
begin
  temp:= TSchemaView.Create;
  temp.FViews:= Self;
  FList.Add(temp);
end;

procedure TSchemaViews.InsertItem(Index: Integer);
var
  temp: TSchemaView;
begin
  temp:= TSchemaView.Create;
  temp.FViews:= Self;
  FList.Insert(Index, temp);
end;

procedure TSchemaViews.DeleteItem(Index: Integer);
begin
  TSchemaView(FList[Index]).Free;
  FList.Delete(Index);
end;

procedure TSchemaViews.MoveItem(Index, NewIndex: Integer);
begin
  FList.Move(Index, NewIndex);
end;

procedure TSchemaViews.ExchangeItems(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

procedure TSchemaViews.Clear;
var
  i: Integer;
begin
  for i:= 0 to FList.Count-1 do
    TSchemaView(FList[i]).Free;
  FList.Clear;    
end;

{ TOESchema }

procedure TOESchema.SetHdbc(AHdbc: THdbc);
begin
  FHdbc:= AHdbc;
  FHstmt.Hdbc:= FHdbc;
end;

procedure TOESchema.SetTables(ATables: TSchemaTables);
begin
end;

procedure TOESchema.SetViews(AViews: TSchemaViews);
begin
end;

procedure TOESchema.SetExecMarker(AExecMarker: String);
begin
  if (AExecMarker <> FExecMarker) and (Trim(AExecmarker) <> '') then
    FExecMarker:= Trim(AExecMarker);
end;

function TOESchema.Prefix(AOwner, ATable: String): String;
begin
  AOwner:= Quoted(AOwner);
  ATable:= Quoted(ATable);

  if AOwner = '' then
    Result:= ATable
  else
    Result:= AOwner+'.'+ATable;
end;

procedure TOESchema.Notification(AComponent: TComponent;
                                 Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opInsert) and (AComponent is THdbc) and (FHdbc = nil) then
    Hdbc:= THdbc(AComponent);

  if (Operation = opRemove) and (AComponent = FHdbc) then
    Hdbc:= nil;
end;

constructor TOESchema.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);

  FHdbc:= nil;
  if AOwner <> nil then
    for i:= 0 to AOwner.ComponentCount-1 do
      if AOwner.Components[i] is THdbc then
      begin
        FHdbc:= THdbc(AOwner.Components[i]);
        Break;
      end;

  FHstmt:= THstmt.Create(nil);
  FHstmt.Hdbc:= FHdbc;
  FTables:= TSchemaTables.Create;
  FTables.FSchema:= Self;
  FViews:= TSchemaViews.Create;
  FViews.FSchema:= Self;

  FToFile:= False;
  FExecMarker:= DefExecMarker;
  FNameConstraints:= DefNameConstraints;
  FAborted:= False;
end;

destructor TOESchema.Destroy;
begin
  FHstmt.Free;
  FTables.Free;
  FViews.Free;

  inherited Destroy;
end;

procedure TOESchema.SplitColumn(AIndexColumn: string;
                                var AColumn: String;
                                var AType: String);
begin
  AColumn:= TrimRight(AIndexColumn);
  AType:= '0';

  if (Length(AIndexColumn) > 4) and (Copy(AIndexColumn, Length(AIndexColumn)-3, 4) = ' ASC') then
  begin
    AColumn:= TrimRight(Copy(AIndexColumn, 1, Length(AIndexColumn)-4));
    AType:= '1';
  end
  else if (Length(AIndexColumn) > 5) and (Copy(AIndexColumn, Length(AIndexColumn)-4, 5) = ' DESC') then
  begin
    AColumn:= TrimRight(Copy(AIndexColumn, 1, Length(AIndexColumn)-5));
    AType:= '2';
  end;
end;

procedure TOESchema.DoProgress(Info: String);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Info);
end;

procedure TOESchema.LoadTables;
var
  i: Integer;
begin
  FAborted:= False;
  for i:= 0 to Tables.ItemCount-1 do
  begin
    if FAborted then
      Break;

    Tables[i].LoadTable;
    Tables[i].LoadIndexes;
    DoProgress(FTables.FSchema.Prefix(Tables[i].TableOwner, Tables[i].TableName));
  end;
end;

procedure TOESchema.DropTables(IgnoreErrors: Boolean);
var
  i: Integer;
begin
  FAborted:= False;
  for i:= Tables.ItemCount-1 downto 0 do
  begin
    if FAborted then
      Break;

    Tables[i].DropIndexes(IgnoreErrors);

    try
      Tables[i].DropTable;
    except
      if not IgnoreErrors then
        raise;
    end;

    DoProgress(FTables.FSchema.Prefix(Tables[i].TableOwner, Tables[i].TableName));
  end;
end;

procedure TOESchema.LoadViews;
var
  i: Integer;
begin
  FAborted:= False;
  for i:= 0 to Views.ItemCount-1 do
  begin
    if FAborted then
      Break;

    Views[i].LoadView;
    DoProgress(FViews.FSchema.Prefix(Views[i].ViewOwner, Views[i].ViewName));
  end;
end;

procedure TOESchema.DropViews(IgnoreErrors: Boolean);
var
  i: Integer;
begin
  FAborted:= False;
  for i:= Views.ItemCount-1 downto 0 do
  begin
    if FAborted then
      Break;

    try
      Views[i].DropView;
    except
      if not IgnoreErrors then
        raise;
    end;

    DoProgress(FViews.FSchema.Prefix(Views[i].ViewOwner, Views[i].ViewName));
  end;
end;

procedure TOESchema.WriteLine(ctSQL: String);
begin
  Writeln(FText, ctSQL);
  Writeln(FText, ExecMarker);
end;

function TOESchema.ReadLine: String;
var
  ctSQL: String;
begin
  Readln(FText, ctSQL);
  if UpperCase(Trim(ctSQL)) = UpperCase(ExecMarker) then
    Result:= DefExecMarker
  else
    Result:= ctSQL;
end;

procedure TOESchema.GenScript(FileName: String);
begin
  FToFile:= True;
  if FileName = '' then
    FileName:= 'SCRIPT'+DefScriptExt;

  AssignFile(FText, FileName);
  Rewrite(FText);

  try
    LoadTables;
    if not FAborted then
      LoadViews;
    if not FAborted then
      DropViews(False);
    if not FAborted then
      DropTables(False);
  finally
    FToFile:= False;
    CloseFile(FText);
  end;
end;

procedure TOESchema.RunScript(FileName: String);
var
  ctSQL, Info: String;
  Line: String;

  function ExtractInfo: String;
  var
    Loc: Integer;
  begin
    Loc:= Pos(' DROP ', UpperCase(' '+ctSQL));
    if Loc > 0 then
    begin
      Result:= ExtractTable(ctSQL, 'TABLE');
      if Result <> '' then
      begin
        Result:= 'DROP TABLE '+Result;
        Exit;
      end;
      Result:= ExtractTable(ctSQL, 'VIEW');
      if Result <> '' then
      begin
        Result:= 'DROP VIEW '+Result;
        Exit;
      end;
      Exit;
    end;

    Loc:= Pos(' CREATE ', UpperCase(' '+ctSQL));
    if Loc > 0 then
    begin
      Result:= ExtractTable(ctSQL, 'TABLE');
      if Result <> '' then
      begin
        Result:= 'CREATE TABLE '+Result;
        Exit;
      end;
      Result:= ExtractTable(ctSQL, 'VIEW');
      if Result <> '' then
      begin
        Result:= 'CREATE VIEW '+Result;
        Exit;
      end;
      Exit;
    end;

    Result:= '';
  end;

begin
  FAborted:= False;
  if FileName = '' then
    FileName:= 'SCRIPT'+DefScriptExt;

  AssignFile(FText, FileName);
  Reset(FText);

  try

  while not SeekEOF(FText) do
  begin
    if FAborted then
      Break;

    ctSQL:= '';
    Line:= ReadLine;
    while Line <> DefExecMarker do
    begin
      ctSQL:= ctSQL+Line+' ';
      if SeekEOF(FText) then
        Break;
      Line:= ReadLine;
    end;

    if Trim(ctSQL) <> '' then
      with FHstmt do
      begin
        Info:= ExtractInfo;
        SQL:= ctSQL;
        Prepare;
        Execute;
        if Info <> '' then
          DoProgress(Info);
      end;

  end;

  finally
    CloseFile(FText);
  end;
end;

procedure TOESchema.Clear;
begin
  Tables.Clear;
  Views.Clear;
end;

procedure TOESchema.Terminate;
begin
  FHstmt.Terminate;
end;

procedure TOESchema.Abort;
begin
  FAborted:= True;
end;

{ TOEAdministrator }

procedure TOEAdministrator.RetrieveDataSources;
var
  Direction: SQLUSMALLINT;
  DSName, DSDriver: NullString;
  StringLength1, StringLength2: SQLSMALLINT;
begin
  { Retrieve DataSources }
  FDataSourceNames.Clear;
  FDataSourceDrivers.Clear;

  case FDataSourceType of
    dsUser:
      Direction:= SQL_FETCH_FIRST_USER;
    dsSystem:
      Direction:= SQL_FETCH_FIRST_SYSTEM;
    else
      Direction:= SQL_FETCH_FIRST;
  end;

  while GlobalHenv.Error.Success(SQLDataSources(GlobalHenv.Handle, Direction,
                                                @DSName, SizeOf(DSName), @StringLength1,
                                                @DSDriver, SizeOf(DSDriver), @StringLength2)) do
  begin
    FDataSourceNames.Add(DSName);
    FDataSourceDrivers.Add(DSDriver);
    Direction:= SQL_FETCH_NEXT;
  end;
end;

procedure TOEAdministrator.RetrieveDrivers;
var
  Direction: SQLUSMALLINT;
  DName, DAttr: NullString;
  StringLength1, StringLength2: SQLSMALLINT;
begin
  { Retrieve Drivers }
  FDriverNames.Clear;

  Direction:= SQL_FETCH_FIRST;

  while GlobalHenv.Error.Success(SQLDrivers(GlobalHenv.Handle, Direction,
                                            @DName, SizeOf(DName), @StringLength1,
                                            @DAttr, SizeOf(DAttr), @StringLength2)) do
  begin
    FDriverNames.Add(DName);
    Direction:= SQL_FETCH_NEXT;
  end;
end;

procedure TOEAdministrator.SetDataSourceType(ADataSourceType: TDataSourceType);
begin
  FDataSourceType:= ADataSourceType;
  Refresh;
end;

procedure TOEAdministrator.SetDataSource(ADataSource: String);
begin
  ADataSource:= Trim(ADataSource);
  if (ADataSource = '') or Valid(ADataSource) then
    FDataSource:= ADataSource;
end;

procedure TOEAdministrator.SetAttributes(AAttributes: TStrings);
begin
  FAttributes.Assign(AAttributes);
end;

function TOEAdministrator.Configure(Request: SQLUSMALLINT): Boolean;
var
  i: Integer;
  hwndParent: SQLHWND;
  LAttributes: String;
begin
  if FDriver = '' then
    raise EODBCExpress.Create('No Driver Value Specified');

  hwndParent:= 0;

  LAttributes:= '';
  if FDataSource <> '' then
    LAttributes:= LAttributes+'DSN='+FDataSource+#1;
  if FUserName <> '' then
    LAttributes:= LAttributes+'UID='+FUserName+#1;
  if FPassword <> '' then
    LAttributes:= LAttributes+'PWD='+FPassword+#1;
  for i:= 0 to FAttributes.Count-1 do
    LAttributes:= LAttributes+FAttributes[i]+#1;

  InsertNulls(LAttributes);

  Result:= SQLConfigDataSource(hwndParent, Request, PChar(FDriver), PChar(LAttributes));
end;

function TOEAdministrator.GetDataSources: TStrings;
begin
  if FGetDataSources then
    RetrieveDataSources;
  FGetDataSources:= False;
  Result:= FDataSourceNames;
end;

function TOEAdministrator.GetDrivers: TStrings;
begin
  if FGetDrivers then
    RetrieveDrivers;
  FGetDrivers:= False;
  Result:= FDriverNames;
end;

constructor TOEAdministrator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPrompt:= DefPrompt;
  FDataSourceType:= DefDataSourceType;
  FDriver:= '';
  FDataSource:= '';
  FUserName:= '';
  FPassword:= '';
  FAttributes:= TStringList.Create;
  FDataSourceNames:= TStringList.Create;
  FDataSourceDrivers:= TStringList.Create;
  FDriverNames:= TStringList.Create;
  FGetDataSources:= True;
  FGetDrivers:= True;
end;

destructor TOEAdministrator.Destroy;
begin
  FAttributes.Free;
  FDataSourceNames.Free;
  FDataSourceDrivers.Free;
  FDriverNames.Free;

  inherited Destroy;
end;

function TOEAdministrator.Add: Boolean;
begin
  if FDataSourceType = dsSystem then
    Result:= Configure(ODBC_ADD_SYS_DSN)
  else
    Result:= Configure(ODBC_ADD_DSN);
end;

function TOEAdministrator.Modify: Boolean;
begin
  if FDataSourceType = dsSystem then
    Result:= Configure(ODBC_CONFIG_SYS_DSN)
  else
    Result:= Configure(ODBC_CONFIG_DSN);
end;

function TOEAdministrator.Remove: Boolean;
begin
  Result:= False;
  case FDataSourceType of
    dsDefault:
      Result:= Configure(ODBC_REMOVE_DEFAULT_DSN);
    dsUser:
      Result:= Configure(ODBC_REMOVE_DSN);
    dsSystem:
      Result:= Configure(ODBC_REMOVE_SYS_DSN)
  end;
end;

function TOEAdministrator.Valid(ADataSource: String): Boolean;
begin
  Result:= SQLValidDSN(PChar(ADataSource));
end;

procedure TOEAdministrator.Refresh;
begin
  FGetDataSources:= True;
  FGetDrivers:= True;
end;

function TOEAdministrator.DataSourceDriver(ADataSource: String): String;
var
  i: Integer;
begin
  Result:= '';
  ADataSource:= UpperCase(ADataSource);
  for i:= 0 to DataSources.Count-1 do
    if UpperCase(DataSources[i]) = ADataSource then
    begin
      Result:= FDataSourceDrivers[i];
      Break;
    end;
end;

{ TCatalogColumn }

constructor TCatalogColumn.Create;
begin
  FColumnName:= '';
  FColumnType:= 0;
  FDataType:= 0;
  FDataTypeName:= '';
  FPrecision:= 0;
  FScale:= 0;
  FRadix:= 0;
  FNullable:= 0;
  FDefault:= '';
  FDescription:= '';
end;

destructor TCatalogColumn.Destroy;
begin

  inherited Destroy;
end;

{ TCatalogColumns }

procedure TCatalogColumns.FreeItems;
var
  i: Integer;
begin
  for i:= 0 to FList.Count-1 do
    TCatalogColumn(FList[i]).Free;
  FList.Clear;  
end;

function TCatalogColumns.AddItem: TCatalogColumn;
begin
  Result:= TCatalogColumn.Create;
  FList.Add(Result);
end;

function TCatalogColumns.GetItem(Index: Integer): TCatalogColumn;
begin
  Result:= TCatalogColumn(FList[Index]);
end;

procedure TCatalogColumns.SetItem(Index: Integer;
                                  AColumn: TCatalogColumn);
begin
end;

function TCatalogColumns.GetCount: Integer;
begin
  Result:= FList.Count;
end;

constructor TCatalogColumns.Create;
begin
  FList:= TList.Create;
  FList.Clear;
end;

destructor TCatalogColumns.Destroy;
begin
  FreeItems;
  FList.Free;

  inherited Destroy;
end;

{ TCatalogTable }

procedure TCatalogTable.RetrieveColumns;
var
  RetCode: SQLRETURN;
  ATableOwner: Pointer;
  temp: TCatalogColumn;
begin
  if FTableOwner = '' then
    ATableOwner:= nil
  else
    ATableOwner:= PChar(FTableOwner);

  FCatalog.FHstmt.Terminate;
  RetCode:= SQLColumns(FCatalog.FHstmt.Handle, nil, 0, ATableOwner, Length(FTableOwner),
    Pointer(PChar(FTableName)), Length(FTableName), nil, 0);
  if not GlobalHenv.Error.Success(RetCode) then
    GlobalHenv.Error.RaiseError(FCatalog.FHstmt, RetCode);

  FColumns.FreeItems;
  while FCatalog.FHstmt.FetchNext do
  begin
    temp:= FColumns.AddItem;

    temp.FColumnName:= FCatalog.FHstmt.ColString[4];
    temp.FColumnType:= SQL_RESULT_COL;
    temp.FDataType:= FCatalog.FHstmt.ColSmallint[5];
    temp.FDataTypeName:= FCatalog.FHstmt.ColString[6];
    temp.FPrecision:= FCatalog.FHstmt.ColInteger[7];
    temp.FScale:= FCatalog.FHstmt.ColSmallint[9];
    temp.FRadix:= FCatalog.FHstmt.ColSmallint[10];
    temp.FNullable:= FCatalog.FHstmt.ColSmallint[11];
    temp.FDefault:= FCatalog.FHstmt.ColString[13];
    temp.FDescription:= FCatalog.FHstmt.ColString[12];
  end;
end;

procedure TCatalogTable.RetrievePrimaryKeys;
var
  RetCode: SQLRETURN;
  ATableOwner: Pointer;
begin
  if FTableOwner = '' then
    ATableOwner:= nil
  else
    ATableOwner:= PChar(FTableOwner);

  FCatalog.FHstmt.Terminate;
  RetCode:= SQLPrimaryKeys(FCatalog.FHstmt.Handle, nil, 0, ATableOwner, Length(FTableOwner),
    Pointer(PChar(FTableName)), Length(FTableName));
  if not GlobalHenv.Error.Success(RetCode) then
    GlobalHenv.Error.RaiseError(FCatalog.FHstmt, RetCode);

  FPrimaryKeys.Clear;
  while FCatalog.FHstmt.FetchNext do
    FPrimaryKeys.Add(FCatalog.FHstmt.ColString[4]);
end;

procedure TCatalogTable.RetrieveForeignKeys;
var
  RetCode: SQLRETURN;
  ATableOwner: Pointer;

  function Prefix(AOwner, ATable: String): String;
  begin
    if AOwner = '' then
      Result:= ATable
    else
      Result:= AOwner+'.'+ATable;
  end;

begin
  if FTableOwner = '' then
    ATableOwner:= nil
  else
    ATableOwner:= PChar(FTableOwner);

  FCatalog.FHstmt.Terminate;
  RetCode:= SQLForeignKeys(FCatalog.FHstmt.Handle, nil, 0, nil, 0, nil, 0,
                                                   nil, 0, ATableOwner, Length(FTableOwner), Pointer(PChar(FTableName)), Length(FTableName));
  if not GlobalHenv.Error.Success(RetCode) then
    GlobalHenv.Error.RaiseError(FCatalog.FHstmt, RetCode);

  FForeignKeys.Clear;
  with FCatalog.FHstmt do
    while FetchNext do
      //<COLUMN NAME>;[<FOREIGN OWNER>.]<FOREIGN TABLE>;<FOREIGN COLUMN>
      FForeignKeys.Add(ColString[8]+';'+Prefix(ColString[2], ColString[3])+';'+ColString[4]);
end;

procedure TCatalogTable.RetrieveForeignReferences;
var
  RetCode: SQLRETURN;
  ATableOwner: Pointer;

  function Prefix(AOwner, ATable: String): String;
  begin
    if AOwner = '' then
      Result:= ATable
    else
      Result:= AOwner+'.'+ATable;
  end;

begin
  if FTableOwner = '' then
    ATableOwner:= nil
  else
    ATableOwner:= PChar(FTableOwner);

  FCatalog.FHstmt.Terminate;
  RetCode:= SQLForeignKeys(FCatalog.FHstmt.Handle, nil, 0, ATableOwner, Length(FTableOwner), Pointer(PChar(FTableName)), Length(FTableName),
                                                   nil, 0, nil, 0, nil, 0);
  if not GlobalHenv.Error.Success(RetCode) then
    GlobalHenv.Error.RaiseError(FCatalog.FHstmt, RetCode);

  FForeignReferences.Clear;
  with FCatalog.FHstmt do
    while FetchNext do
      //<COLUMN NAME>;[<FOREIGN OWNER>.]<FOREIGN TABLE>;<FOREIGN COLUMN>
      FForeignReferences.Add(ColString[4]+';'+Prefix(ColString[6], ColString[7])+';'+ColString[8]);
end;

procedure TCatalogTable.RetrieveIndexes;
var
  RetCode: SQLRETURN;
  ATableOwner: Pointer;
  Fetched: Boolean;
  AIndex, ACols: String;
  AUnique: Boolean;
begin
  if FTableOwner = '' then
    ATableOwner:= nil
  else
    ATableOwner:= PChar(FTableOwner);

  FCatalog.FHstmt.Terminate;
  RetCode:= SQLStatistics(FCatalog.FHstmt.Handle, nil, 0, ATableOwner, Length(FTableOwner),
    Pointer(PChar(FTableName)), Length(FTableName), SQL_INDEX_ALL, SQL_ENSURE);
  if not GlobalHenv.Error.Success(RetCode) then
    GlobalHenv.Error.RaiseError(FCatalog.FHstmt, RetCode);

  FIndexes.Clear;
  AIndex:= '';
  ACols:= '';
  AUnique:= False;

  with FCatalog.FHstmt do
  repeat

    Fetched:= FetchNext;

    if (AIndex <> '') and
       ((not Fetched) or (ColSmallint[7] = SQL_TABLE_STAT) or (ColSmallint[8] = 1)) then
    begin
      //<INDEX NAME>;U|N;<COLUMN NAME>[,<COLUMN NAME>]...
      if AUnique then
        AIndex:= AIndex+';U;'+ACols
      else
        AIndex:= AIndex+';N;'+ACols;
      FIndexes.Add(AIndex);

      AIndex:= '';
    end;

    if ColSmallint[8] = 1 then
    begin
      AIndex:= ColString[6];
      AUnique:= ColSmallint[4] = SQL_FALSE;
      ACols:= ColString[9];
    end
    else
      ACols:= ACols+','+ColString[9];

  until not Fetched;
end;

procedure TCatalogTable.RetrieveUniqueKey;
var
  RetCode: SQLRETURN;
  ATableOwner: Pointer;
begin
  if FTableOwner = '' then
    ATableOwner:= nil
  else
    ATableOwner:= PChar(FTableOwner);

  FCatalog.FHstmt.Terminate;
  RetCode:= SQLSpecialColumns(FCatalog.FHstmt.Handle, SQL_BEST_ROWID, nil, 0, ATableOwner, Length(FTableOwner),
    Pointer(PChar(FTableName)), Length(FTableName), SQL_SCOPE_CURROW, SQL_NULLABLE);
  if not GlobalHenv.Error.Success(RetCode) then
    GlobalHenv.Error.RaiseError(FCatalog.FHstmt, RetCode);

  FUniqueKey:= '';
  while FCatalog.FHstmt.FetchNext do
    if FCatalog.FHstmt.ColSmallint[8] <> SQL_PC_PSEUDO then
      //<COLUMN NAME>[,<COLUMN NAME>]...
      FUniqueKey:= FUniqueKey+FCatalog.FHstmt.ColString[2]+',';
  if Length(FUniqueKey) > 0 then
    SetLength(FUniqueKey, Length(FUniqueKey)-1);
end;

function TCatalogTable.GetColumns: TCatalogColumns;
begin
  if FGetColumns then
    RetrieveColumns;
  FGetColumns:= False;
  Result:= FColumns;
end;

function TCatalogTable.GetColumnNames: TStrings;
var
  i: Integer;
begin
  FColumnNames.Clear;
  for i:= 0 to Columns.ItemCount-1 do
    FColumnNames.Add(Columns[i].ColumnName);
  Result:= FColumnNames;
end;

function TCatalogTable.GetPrimaryKeys: TStrings;
begin
  if FGetPrimaryKeys then
    RetrievePrimaryKeys;
  FGetPrimaryKeys:= False;
  Result:= FPrimaryKeys;
end;

function TCatalogTable.GetForeignKeys: TStrings;
begin
  if FGetForeignKeys then
    RetrieveForeignKeys;
  FGetForeignKeys:= False;
  Result:= FForeignKeys;
end;

function TCatalogTable.GetForeignReferences: TStrings;
begin
  if FGetForeignReferences then
    RetrieveForeignReferences;
  FGetForeignReferences:= False;
  Result:= FForeignReferences;
end;

function TCatalogTable.GetIndexes: TStrings;
begin
  if FGetIndexes then
    RetrieveIndexes;
  FGetIndexes:= False;
  Result:= FIndexes;
end;

function TCatalogTable.GetUniqueKey: String;
begin
  if FGetUniqueKey then
    RetrieveUniqueKey;
  FGetUniqueKey:= False;
  Result:= FUniqueKey;
end;

constructor TCatalogTable.Create;
begin
  FTableOwner:= '';
  FTableName:= '';
  FTableType:= ttTable;
  FDescription:= '';
  FColumns:= TCatalogColumns.Create;
  FColumnNames:= TStringList.Create;
  FPrimaryKeys:= TStringList.Create;
  FForeignKeys:= TStringList.Create;
  FForeignReferences:= TStringList.Create;
  FIndexes:= TStringList.Create;
  FUniqueKey:= '';
  FGetColumns:= True;
  FGetPrimaryKeys:= True;
  FGetForeignKeys:= True;
  FGetForeignReferences:= True;
  FGetIndexes:= True;
  FGetUniqueKey:= True;  
end;

destructor TCatalogTable.Destroy;
begin
  FColumns.Free;
  FColumnNames.Free;
  FPrimaryKeys.Free;
  FForeignKeys.Free;
  FForeignReferences.Free;
  FIndexes.Free;  

  inherited Destroy;
end;

procedure TCatalogTable.Refresh;
begin
  FGetColumns:= True;
  FGetPrimaryKeys:= True;
  FGetForeignKeys:= True;
  FGetForeignReferences:= True;
  FGetIndexes:= True;
  FGetUniqueKey:= True;
end;

function TCatalogTable.ColumnByName(AColumnName: String): TCatalogColumn;
var
  i: Integer;
begin
  Result:= nil;
  AColumnName:= UpperCase(Trim(AColumnName));
  for i:= 0 to Columns.ItemCount-1 do
    if UpperCase(Columns[i].ColumnName) = AColumnName then
    begin
      Result:= Columns[i];
      Break;
    end;
end;

{ TCatalogTables }

procedure TCatalogTables.FreeItems;
var
  i: Integer;
begin
  for i:= 0 to FList.Count-1 do
    TCatalogTable(FList[i]).Free;
  FList.Clear;
end;

function TCatalogTables.AddItem: TCatalogTable;
begin
  Result:= TCatalogTable.Create;
  Result.FCatalog:= FCatalog;
  FList.Add(Result);
end;

function TCatalogTables.GetItem(Index: Integer): TCatalogTable;
begin
  Result:= TCatalogTable(FList[Index]);
end;

procedure TCatalogTables.SetItem(Index: Integer;
                                 ATable: TCatalogTable);
begin
end;

function TCatalogTables.GetCount: Integer;
begin
  Result:= FList.Count;
end;

constructor TCatalogTables.Create;
begin
  FList:= TList.Create;
  FList.Clear;
end;

destructor TCatalogTables.Destroy;
begin
  FreeItems;
  FList.Free;

  inherited Destroy;
end;

{ TCatalogProcedure }

procedure TCatalogProcedure.RetrieveColumns;
var
  RetCode: SQLRETURN;
  AProcedureOwner: Pointer;
  temp: TCatalogColumn;
begin
  if FProcedureOwner = '' then
    AProcedureOwner:= nil
  else
    AProcedureOwner:= PChar(FProcedureOwner);

  FCatalog.FHstmt.Terminate;
  RetCode:= SQLProcedureColumns(FCatalog.FHstmt.Handle, nil, 0, AProcedureOwner, Length(FProcedureOwner),
    Pointer(PChar(FProcedureName)), Length(FProcedureName), nil, 0);
  if not GlobalHenv.Error.Success(RetCode) then
    GlobalHenv.Error.RaiseError(FCatalog.FHstmt, RetCode);

  FColumns.FreeItems;
  while FCatalog.FHstmt.FetchNext do
  begin
    temp:= FColumns.AddItem;

    temp.FColumnName:= FCatalog.FHstmt.ColString[4];
    temp.FColumnType:= FCatalog.FHstmt.ColSmallint[5];
    temp.FDataType:= FCatalog.FHstmt.ColSmallint[6];
    temp.FDataTypeName:= FCatalog.FHstmt.ColString[7];
    temp.FPrecision:= FCatalog.FHstmt.ColInteger[8];
    temp.FScale:= FCatalog.FHstmt.ColSmallint[10];
    temp.FRadix:= FCatalog.FHstmt.ColSmallint[11];
    temp.FNullable:= FCatalog.FHstmt.ColSmallint[12];
    temp.FDefault:= FCatalog.FHstmt.ColString[14];
    temp.FDescription:= FCatalog.FHstmt.ColString[13];
  end;
end;

function TCatalogProcedure.GetColumns: TCatalogColumns;
begin
  if FGetColumns then
    RetrieveColumns;
  FGetColumns:= False;
  Result:= FColumns;
end;

function TCatalogProcedure.GetColumnNames: TStrings;
var
  i: Integer;
begin
  FColumnNames.Clear;
  for i:= 0 to Columns.ItemCount-1 do
    FColumnNames.Add(Columns[i].ColumnName);
  Result:= FColumnNames;
end;

constructor TCatalogProcedure.Create;
begin
  FProcedureOwner:= '';
  FProcedureName:= '';
  FProcedureType:= 0;
  FDescription:= '';
  FColumns:= TCatalogColumns.Create;
  FColumnNames:= TStringList.Create;
  FGetColumns:= True;
end;

destructor TCatalogProcedure.Destroy;
begin
  FColumns.Free;
  FColumnNames.Free;

  inherited Destroy;
end;

procedure TCatalogProcedure.Refresh;
begin
  FGetColumns:= True;
end;

function TCatalogProcedure.ColumnByName(AColumnName: String): TCatalogColumn;
var
  i: Integer;
begin
  Result:= nil;
  AColumnName:= UpperCase(Trim(AColumnName));
  for i:= 0 to Columns.ItemCount-1 do
    if UpperCase(Columns[i].ColumnName) = AColumnName then
    begin
      Result:= Columns[i];
      Break;
    end;
end;

{ TCatalogProcedures }

procedure TCatalogProcedures.FreeItems;
var
  i: Integer;
begin
  for i:= 0 to FList.Count-1 do
    TCatalogProcedure(FList[i]).Free;
  FList.Clear;
end;

function TCatalogProcedures.AddItem: TCatalogProcedure;
begin
  Result:= TCatalogProcedure.Create;
  Result.FCatalog:= FCatalog;
  FList.Add(Result);
end;

function TCatalogProcedures.GetItem(Index: Integer): TCatalogProcedure;
begin
  Result:= TCatalogProcedure(FList[Index]);
end;

procedure TCatalogProcedures.SetItem(Index: Integer;
                                     AProcedure: TCatalogProcedure);
begin
end;

function TCatalogProcedures.GetCount: Integer;
begin
  Result:= FList.Count;
end;

constructor TCatalogProcedures.Create;
begin
  FList:= TList.Create;
  FList.Clear;
end;

destructor TCatalogProcedures.Destroy;
begin
  FreeItems;
  FList.Free;

  inherited Destroy;
end;

{ TOECatalog }

procedure TOECatalog.RetrieveTables;
var
  RetCode: SQLRETURN;
  ATableOwner, ATableName: Pointer;
  ATableType: String;
  temp: TCatalogTable;

  procedure CheckTableTypes;
  var
    Empty: String;
    temp: TTableTypeSet;
  begin
    Empty:= '';
    ATableType:= SQL_ALL_TABLE_TYPES;
    FHstmt.Terminate;
    RetCode:= SQLTables(FHstmt.Handle, Pointer(PChar(Empty)), 0, Pointer(PChar(Empty)), 0, Pointer(PChar(Empty)), 0,
      Pointer(PChar(ATableType)), Length(ATableType));
    temp:= [];
    if GlobalHenv.Error.Success(RetCode) then
      while FHstmt.FetchNext do
      begin
        if (FHstmt.ColString[4] = TableStr) and (ttTable in FTableType) then
          temp:= temp+[ttTable];
        if (FHstmt.ColString[4] = ViewStr) and (ttView in FTableType) then
          temp:= temp+[ttView];
        if (FHstmt.ColString[4] = SystemStr) and (ttSystem in FTableType) then
          temp:= temp+[ttSystem];
      end;

    FTableType:= temp;
  end;

begin
  if FHdbc = nil then
    raise EODBCExpress.Create('Property Hdbc of '+Name+' not set.');

  FTableOwner:= Trim(FTableOwner);
  if FTableOwner = '' then
    ATableOwner:= nil
  else
    ATableOwner:= PChar(FTableOwner);

  FTableName:= Trim(FTableName);
  if FTableName = '' then
    ATableName:= nil
  else
    ATableName:= PChar(FTableName);

  //CheckTableTypes;
  FHdbc.Connect;
  if FHdbc.IsDriver(['Excel;odbcjt32.dll']) then
    FTableType:= [];

  ATableType:= '';
  if (ttTable in FTableType) then
    ATableType:= ''''+TableStr+'''';
  if (ttView in FTableType) then
  begin
    if ATableType <> '' then
      ATableType:= ATableType+', ';
    ATableType:= ATableType+''''+ViewStr+'''';
  end;
  if (ttSystem in FTableType) then
  begin
    if ATableType <> '' then
      ATableType:= ATableType+', ';
    ATableType:= ATableType+''''+SystemStr+'''';
  end;

  FHstmt.Terminate;
  if ATableType = '' then
    RetCode:= SQLTables(FHstmt.Handle, nil, 0, ATableOwner, Length(FTableOwner),
      ATableName, Length(FTableName), nil, 0)
  else
    RetCode:= SQLTables(FHstmt.Handle, nil, 0, ATableOwner, Length(FTableOwner),
      ATableName, Length(FTableName), Pointer(PChar(ATableType)), Length(ATableType));
  if not GlobalHenv.Error.Success(RetCode) then
    GlobalHenv.Error.RaiseError(FHstmt, RetCode);

  FTables.FreeItems;
  while FHstmt.FetchNext do
  begin
    temp:= FTables.AddItem;

    temp.FTableOwner:= FHstmt.ColString[2];
    temp.FTableName:= FHstmt.ColString[3];
    temp.FTableType:= StringToTableType(FHstmt.ColString[4]);
    temp.FDescription:= FHstmt.ColString[5];
  end;
end;

procedure TOECatalog.RetrieveProcedures;
var
  RetCode: SQLRETURN;
  AProcedureOwner, AProcedureName: Pointer;
  temp: TCatalogProcedure;

  function ProcName(AProcName: String): String;
  var
    Loc: Integer;
  begin
    Loc:= Pos(';', AProcName);
    if Loc > 0 then
      AProcName:= Copy(AProcName, 1, Loc-1);

    Result:= Trim(AProcName);
  end;

begin
  if FHdbc = nil then
    raise EODBCExpress.Create('Property Hdbc of '+Name+' not set.');

  FProcedureOwner:= Trim(FProcedureOwner);
  if FProcedureOwner = '' then
    AProcedureOwner:= nil
  else
    AProcedureOwner:= PChar(FProcedureOwner);

  FProcedureName:= Trim(FProcedureName);
  if FProcedureName = '' then
    AProcedureName:= nil
  else
    AProcedureName:= PChar(FProcedureName);

  FHstmt.Terminate;
  RetCode:= SQLProcedures(FHstmt.Handle, nil, 0, AProcedureOwner, Length(FProcedureOwner),
    AProcedureName, Length(FProcedureName));
  if not GlobalHenv.Error.Success(RetCode) then
    GlobalHenv.Error.RaiseError(FHstmt, RetCode);

  FProcedures.FreeItems;
  while FHstmt.FetchNext do
  begin
    temp:= FProcedures.AddItem;

    temp.FProcedureOwner:= FHstmt.ColString[2];
    temp.FProcedureName:= ProcName(FHstmt.ColString[3]);
    temp.FProcedureType:= FHstmt.ColSmallint[8];
    temp.FDescription:= FHstmt.COlString[7];
  end;
end;

function TOECatalog.GetTables: TCatalogTables;
begin
  if FGetTables then
    RetrieveTables;
  FGetTables:= False;
  Result:= FTables;
end;

function TOECatalog.GetProcedures: TCatalogProcedures;
begin
  if FGetProcedures then
    RetrieveProcedures;
  FGetProcedures:= False;
  Result:= FProcedures;
end;

function TOECatalog.GetTableNames: TStrings;
var
  i: Integer;
begin
  FTableNames.Clear;
  for i:= 0 to Tables.ItemCount-1 do
    FTableNames.Add(Tables[i].TableName);
  Result:= FTableNames;
end;

function TOECatalog.GetProcedureNames: TStrings;
var
  i: Integer;
begin
  FProcedureNames.Clear;
  for i:= 0 to Procedures.ItemCount-1 do
    FProcedureNames.Add(Procedures[i].ProcedureName);
  Result:= FProcedureNames;
end;

procedure TOECatalog.SetHdbc(AHdbc: THdbc);
begin
  FHdbc:= AHdbc;
  FHstmt.Hdbc:= FHdbc;

  Refresh;
end;

procedure TOECatalog.SetTableOwner(ATableOwner: String);
begin
  if ATableOwner <> FTableOwner then
  begin
    FTableOwner:= ATableOwner;
    FGetTables:= True;
  end;
end;

procedure TOECatalog.SetTableName(ATableName: String);
begin
  if ATableName <> FTableName then
  begin
    FTableName:= ATableName;
    FGetTables:= True;
  end;
end;

procedure TOECatalog.SetTableType(ATableType: TTableTypeSet);
begin
  if ATableType <> FTableType then
  begin
    FTableType:= ATableType;
    FGetTables:= True;
  end;
end;

procedure TOECatalog.SetProcedureOwner(AProcedureOwner: String);
begin
  if AProcedureOwner <> FProcedureOwner then
  begin
    FProcedureOwner:= AProcedureOwner;
    FGetProcedures:= True;
  end;
end;

procedure TOECatalog.SetProcedureName(AProcedureName: String);
begin
  if AProcedureName <> FProcedureName then
  begin
    FProcedureName:= AProcedureName;
    FGetProcedures:= True;
  end;
end;

procedure TOECatalog.Notification(AComponent: TComponent;
                                  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opInsert) and (AComponent is THdbc) and (FHdbc = nil) then
    Hdbc:= THdbc(AComponent);

  if (Operation = opRemove) and (AComponent = FHdbc) then
    Hdbc:= nil;
end;

constructor TOECatalog.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);

  FHdbc:= nil;
  if AOwner <> nil then
    for i:= 0 to AOwner.ComponentCount-1 do
      if AOwner.Components[i] is THdbc then
      begin
        FHdbc:= THdbc(AOwner.Components[i]);
        Break;
      end;

  FHstmt:= THstmt.Create(nil);
  FHstmt.Hdbc:= FHdbc;

  FTables:= TCatalogTables.Create;
  FTables.FCatalog:= Self;
  FProcedures:= TCatalogProcedures.Create;
  FProcedures.FCatalog:= Self;

  FTableNames:= TStringList.Create;
  FProcedureNames:= TStringList.Create;

  FTableOwner:= '';
  FTableName:= '%';
  FTableType:= DefTableType;

  FProcedureOwner:= '';
  FProcedureName:= '%';

  FGetTables:= True;
  FGetProcedures:= True;
end;

destructor TOECatalog.Destroy;
begin
  FHstmt.Free;
  FTables.Free;
  FProcedures.Free;
  FTableNames.Free;
  FProcedureNames.Free;

  inherited Destroy;
end;

function TOECatalog.TableByName(ATableOwner, ATableName: String): TCatalogTable;
var
  i: Integer;
begin
  Result:= nil;
  ATableOwner:= UpperCase(Trim(ATableOwner));
  ATableName:= UpperCase(Trim(ATableName));
  for i:= 0 to Tables.ItemCount-1 do
    if (UpperCase(Tables[i].TableName) = ATableName) and
       ((ATableOwner = '') or (UpperCase(Tables[i].TableOwner) = ATableOwner)) then
    begin
      Result:= Tables[i];
      Break;
    end;
end;

function TOECatalog.ProcedureByName(AProcedureOwner, AProcedureName: String): TCatalogProcedure;
var
  i: Integer;
begin
  Result:= nil;
  AProcedureOwner:= UpperCase(Trim(AProcedureOwner));
  AProcedureName:= UpperCase(Trim(AProcedureName));
  for i:= 0 to Procedures.ItemCount-1 do
    if (UpperCase(Procedures[i].ProcedureName) = AProcedureName) and
       ((AProcedureOwner = '') or (UpperCase(Procedures[i].ProcedureOwner) = AProcedureOwner)) then
    begin
      Result:= Procedures[i];
      Break;
    end;
end;

procedure TOECatalog.Refresh;
begin
  FGetTables:= True;
  FGetProcedures:= True;
end;

procedure TOECatalog.Terminate;
begin
  FHstmt.Terminate;
end;

procedure TOECatalog.ParseForeignKey(ForeignKey: String;
                                     var ColumnName, ForeignOwner, ForeignTable, ForeignColumn: String);
var
  Loc: Integer;
begin
  Loc:= Pos(';', ForeignKey);
  if Loc <= 0 then
    raise EODBCExpress.Create('Invalid foreign key format.');

  ColumnName:= Trim(Copy(ForeignKey, 1, Loc-1));
  Delete(ForeignKey, 1, Loc);

  Loc:= Pos(';', ForeignKey);
  if Loc <= 0 then
    raise EODBCExpress.Create('Invalid foreign key format.');

  ForeignTable:= Trim(Copy(ForeignKey, 1, Loc-1));
  Delete(ForeignKey, 1, Loc);

  Loc:= Pos(';', ForeignKey);
  if Loc > 0 then
    raise EODBCExpress.Create('Invalid foreign key format.');

  ForeignColumn:= Trim(ForeignKey);

  Loc:= Pos('.', ForeignTable);
  if Loc > 0 then
  begin
    ForeignOwner:= Trim(Copy(ForeignTable, 1, Loc-1));
    Delete(ForeignTable, 1, Loc);
  end
  else
    ForeignOwner:= '';
end;

procedure TOECatalog.ParseIndex(Index: String;
                                var IndexName: String;
                                var Unique: Boolean;
                                ColumnNames: TStrings);
var
  Loc: Integer;
begin
  Loc:= Pos(';', Index);
  if Loc <= 0 then
    raise EODBCExpress.Create('Invalid index format.');

  IndexName:= Trim(Copy(Index, 1, Loc-1));
  Delete(Index, 1, Loc);

  Loc:= Pos(';', Index);
  if Loc <= 0 then
    raise EODBCExpress.Create('Invalid index format.');

  Unique:= Trim(Copy(Index, 1, Loc-1)) = 'U';
  Delete(Index, 1, Loc);

  Loc:= Pos(';', Index);
  if Loc > 0 then
    raise EODBCExpress.Create('Invalid index format.');

  SplitList(Trim(Index), ',', ColumnNames);
end;

procedure TOECatalog.ParseUniqueKey(UniqueKey: String;
                                    ColumnNames: TStrings);
begin
  SplitList(Trim(UniqueKey), ',', ColumnNames);
end;

{ TOEBulkCopy }

procedure TOEBulkCopy.Notification(AComponent: TComponent;
                                   Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opInsert) and (AComponent is THdbc) then
  begin
    if HdbcSource = nil then
      HdbcSource:= THdbc(AComponent)
    else if HdbcTarget = nil then
      HdbcTarget:= THdbc(AComponent);
  end;

  if Operation = opRemove then
  begin
    if AComponent = HdbcSource then
      HdbcSource:= nil
    else if AComponent = HdbcTarget then
      HdbcTarget:= nil;
  end;
end;

procedure TOEBulkCopy.DoProgress(Info: String);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Info);
end;

procedure TOEBulkCopy.DoBindParams(Table: String;
                                   Hstmt: THstmt);
begin
  if Assigned(FOnBindParams) then
    FOnBindParams(Self, Table, Hstmt);
end;

constructor TOEBulkCopy.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);

  FHdbcSource:= nil;
  FHdbcTarget:= nil;
  FHstmtSource:= THstmt.Create(nil);
  FHstmtTarget:= THstmt.Create(nil);
  FSQLSource:= '';
  FSQLTarget:= '';
  FAborted:= False;
  FExecMarker:= DefExecMarker;
  FRowSetSize:= DefRowSetSize;
  FRowsAffected:= 0;
  FMaxRows:= DefMaxRows;
  FCommitCount:= DefCommitCount;

  if AOwner <> nil then
    for i:= 0 to AOwner.ComponentCount-1 do
      if AOwner.Components[i] is THdbc then
      begin
        FHdbcSource:= THdbc(AOwner.Components[i]);
        Break;
      end;
end;

destructor TOEBulkCopy.Destroy;
begin
  FHstmtSource.Free;
  FHstmtTarget.Free;

  inherited Destroy;
end;

procedure TOEBulkCopy.Execute;
begin
  ExecuteCopy(FSQLSource, FSQLTarget);
end;

procedure TOEBulkCopy.ExecuteCopy(ASQLSource, ASQLTarget: String);
var
  j, k, LastCommitted: Integer;
  Info: String;
begin
  if FHdbcSource = nil then
    raise Exception.Create('Source not set.');
  if FHdbcTarget = nil then
    raise Exception.Create('Target not set.');

  FHstmtSource.Hdbc:= FHdbcSource;
  FHstmtSource.RowSetSize:= FRowSetSize;
  FHstmtSource.BulkData:= FRowSetSize > 1;
  if FMaxRows > 0 then
    FHstmtSource.MaxRows:= FMaxRows;
  FHstmtTarget.Hdbc:= FHdbcTarget;
  FHstmtTarget.BulkData:= FRowSetSize > 1;

  FAborted:= False;
  Info:= '';

  if FCommitCount <> 1 then
    FHdbcTarget.StartTransact;

  try

    with FHstmtSource do
    begin
      //SELECT
      Info:= ExtractTable(ASQLSource, 'FROM');
      SQL:= ASQLSource;
      if Assigned(FOnBindParams) then
      begin
        Prepare;
        DoBindParams(Info, FHstmtSource);
      end;
      Execute;
    end;

    with FHstmtTarget do
    begin
      //INSERT
      FRowsAffected:= 0;
      LastCommitted:= 0;
      Info:= ExtractTable(ASQLTarget, 'INTO');
      SQL:= ASQLTarget;
      Prepare;
      for j:= 1 to FHstmtSource.ColCount do
        if FHstmtSource.RowSetSize = 1 then
        begin
          if not FHstmtSource.BlobCol[j] then
            FHstmtTarget.BindParam(j, FHstmtSource.ColType[j], FHstmtSource.ColValue[j], FHstmtSource.SqlType[j]);
        end
        else
        begin
          if FHstmtSource.BlobCol[j] then
            FHstmtTarget.BindNulls(j)
          else
            FHstmtTarget.BindParams(j, FHstmtSource.ColType[j], FHstmtSource.ColValue[j], FHstmtSource.RowSetSize);
        end;
      while FHstmtSource.FetchNext do
      begin
        if FAborted then
          Break;
        FHstmtTarget.BulkSize:= FHstmtSource.RowsFetched;
        Inc(FRowsAffected, FHstmtSource.RowsFetched);
        Inc(LastCommitted, FHstmtSource.RowsFetched);
        DoProgress(Info);
        for j:= 1 to FHstmtSource.ColCount do
          if FHstmtSource.RowSetSize = 1 then
          begin
            if FHstmtSource.BlobCol[j] then
              FHstmtTarget.BindParam(j, FHstmtSource.ColType[j], FHstmtSource.ColValue[j], FHstmtSource.SqlType[j]);
            FHstmtTarget.ParamSize[j]:= FHstmtSource.ColSize[j];
          end
          else
          begin
            for k:= 1 to FHstmtSource.RowsFetched do
              if not FHstmtSource.BlobCol[j] then
                FHstmtTarget.BulkParamSize[j,k]:= FHstmtSource.CellSize[j,k];
          end;
        FHstmtTarget.Execute;
        if (FCommitCount > 1) and (LastCommitted >= Integer(FCommitCount)) then
        begin
          FHdbcTarget.Commit;
          LastCommitted:= 0;
        end;
        if (FMaxRows > 0) and (FRowsAffected >= SQLINTEGER(FMaxRows)) then
          Break;
      end;
      DoProgress(Info);
    end;

  finally
    if FCommitCount <> 1 then
      FHdbcTarget.EndTransact;
    FHstmtSource.Close;
  end;
end;

procedure TOEBulkCopy.RunScript(FileName: String);
var
  i, Count: Integer;
  F: TStringList;
  ASQL, ASQLSource, ASQLTarget: String;
begin
  FAborted:= False;

  F:= TStringList.Create;

  try

    F.LoadFromFile(FileName);
    Count:= 0;
    ASQL:= '';
    for i:= 0 to F.Count-1 do
    begin
      if FAborted then
        Break;
      if UpperCase(Trim(F.Strings[i])) = UpperCase(FExecMarker) then
      begin
        Inc(Count);

        if (Count mod 2) = 1 then
          ASQLSource:= ASQL
        else
        begin
          ASQLTarget:= ASQL;
          ExecuteCopy(ASQLSource, ASQLTarget);
        end;
        ASQL:= '';
      end
      else
        ASQL:= ASQL+' '+F[i];
    end;

  finally
    F.Free;
  end;
end;

procedure TOEBulkCopy.Terminate;
begin
  FHstmtSource.Terminate;
  FHstmtTarget.Terminate;
end;

procedure TOEBulkCopy.Abort;
begin
  FAborted:= True;
end;

function TOEBulkCopy.GetRowsAffected: SQLINTEGER;
begin
  Result:= FRowsAffected;
end;

procedure TOEBulkCopy.SetExecMarker(AExecMarker: String);
begin
  if (AExecMarker <> FExecMarker) and (Trim(AExecmarker) <> '') then
    FExecMarker:= Trim(AExecMarker);
end;

procedure TOEBulkCopy.SetRowSetSize(ARowSetSize: SQLUINTEGER);
begin
  if ARowSetSize > 0 then
    FRowSetSize:= ARowSetSize;
end;

end.

