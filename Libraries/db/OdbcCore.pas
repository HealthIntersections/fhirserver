
{ File: OCL.Pas                                                      }
{ Description: ODBC Class Library                                    }
{ Author: Pieter A. Myburgh                                          }
{ Copyright: Korbitec (Pty) Ltd                                      }
{                                                                    }

Unit OdbcCore;

{! 29 !}

{.$.ObjExportAll On}
{$IFDEF WIN64}
{$O-}
{$ENDIF}

Interface

Uses
  StringSupport,
  MathSupport,
  AdvDispatchers,
  AdvEvents,
  AdvIntegerMatches,
  AdvIntegerLists,
  AdvItems,
  KDBDialects,
  Classes,
  SysUtils,
  DateSupport,
  OdbcHeaders,
  OdbcImplementation;

Type
  TDate = record
    year: Smallint;
    month: Word;
    day: Word;
  end;
  TTime = record
    hour: Word;
    minute: Word;
    second: Word;
  end;

  { TConnectionPooling }
  TConnectionPooling = (cpDefault, cpOff, cpOnePerDriver, cpOnePerEnv);

  { TFormatStyle }
  TFormatStyle = (fsNone, fsFloat, fsDateTime, fsCustom);

  { TBlobPlacement }
  TBlobPlacement = (bpDetect, bpByParts, bpByExec);

  { TEmptyToNull }
  TEmptyToNull = (enNever, enAlways, enIfNullable);

  { TStringTrimming }
  TStringTrimming = (stTrimNone, stTrimTrailing, stTrimLeading, stTrimBoth);

  { TRowCountMethod }
  TRowCountMethod = (rcFunction, rcSelect, rcTraverse, rcCustom);

  { TNoRowsAffected }
  TNoRowsAffected = Set Of (nrByInsert, nrByUpdate, nrByDelete, nrByRefresh);

Const
  NullData = '';

  DefRaiseSoftErrors = False;
  DefConnectionPooling = cpDefault;
  DefIsolationLevel = SQL_TXN_READ_COMMITTED;
  DefConnected = False;
  DefInfoPrompt = SQL_DRIVER_NOPROMPT;
  DefCursorLib = SQL_CUR_USE_DRIVER;
  DefTracing = False;
  DefPrepared = False;
  DefExecuted = False;
  DefParamType = SQL_PARAM_INPUT;
  DefBulkData = False;
  DefSQLParsing = True;
  DefConcurrencyType = SQL_CONCUR_READ_ONLY;
  DefCursorType = SQL_CURSOR_FORWARD_ONLY;
  DefRowSetSize = 1;
  DefBlobSize = 32768;
  DefBlobDeferral = False;
  DefBlobPlacement = bpDetect;
  DefExecAsync = False;
  DefEmptyToNull = enNever;
  DefStringTrimming = stTrimTrailing;
  DefBindByName = False;
  DefRowCountMethod = rcFunction;
  DefNoRowsAffected = [nrByUpdate];
  DefFormatStyle = fsNone;
  DefFormatMask = '';
  DefSkipByMethod = False;
  DefBindBookmarks = False;
  DefBookmarkSize = SizeOf(SQLINTEGER);
  DefPrimary = False;
  DefIgnore = False;
  DefCore = False;

  rfNone = 0;
  rfUpdate = 1;
  rfDelete = 2;
  rfRefresh = 4;
  rfInsert = 8;
  rfOps = rfUpdate Or rfDelete Or rfRefresh Or rfInsert;

  //SQL_ROW_SUCCESS=0;
  //SQL_ROW_DELETED=1;
  //SQL_ROW_UPDATED=2;
  //SQL_ROW_NOROW=3;
  //SQL_ROW_ADDED=4;
  //SQL_ROW_ERROR=5;
  //SQL_ROW_SUCCESS_WITH_INFO=6;

  DefPosOpts = SQL_POS_ADD Or SQL_POS_UPDATE Or
               SQL_POS_DELETE Or SQL_POS_REFRESH;
  DefPosStmts = SQL_PS_POSITIONED_UPDATE Or
                SQL_PS_POSITIONED_DELETE;

  ParamCharSet = ['A'..'Z', 'a'..'z', '0'..'9', '_', '@'];

Type
  { Type Pointers }
  NullStringPtr = ^NullString;
  StringPtr = ^String;

  SinglePtr = ^Single;
  DoublePtr = ^Double;

  ShortintPtr = ^ShortInt;
  BytePtr = ^Byte;
  SmallintPtr = ^SmallInt;
  WordPtr = ^Word;
  IntegerPtr = ^Integer;
  CardinalPtr = ^Cardinal;
  LongintPtr = ^LongInt;
  LongwordPtr = ^LongWord;
  Int64Ptr = ^Int64;

  TDatePtr = ^TDate;
  TTimePtr = ^TTime;
  TTimeStampPtr = ^TTimeStamp;

  { Non-Visual Components }
  THenv = Class;
  THdbc = Class;
  THstmt = Class;

  { TErrorPtr }
  TErrorPtr = ^TErrorRec;
  TErrorRec = Record
    FState: String;
    FNative: SQLINTEGER;
    FMessage: String;
  End;

  TODBCObject = class (TObject)

  end;

  { EODBC }
  EODBC = Class(EODBCExpress)
  Private                            
    { Private declarations }
    FOwner: TODBCObject;
    FRetCode: SQLRETURN;
    FCursor: Integer;
    FErrors: TList;

    Function GetState: String;
    Function GetNative: SQLINTEGER;
    Function GetMessage: String;
    Procedure SetMessage(AMessage: String);
  Public
    { Public declarations }
    Property Owner: TODBCObject Read FOwner;
    Property RetCode: SQLRETURN Read FRetCode;
    Property State: String Read GetState;
    Property Native: SQLINTEGER Read GetNative;
    Property Message: String Read GetMessage Write SetMessage;

    Constructor Create(AOwner: TODBCObject;
                       ARetCode: SQLRETURN;
                       AErrors: TList);
    Destructor Destroy; Override;
    Procedure First;
    Procedure Last;
    Function Next: Boolean;
    Function Prev: Boolean;
  End;

  { TODBCErrorHandler }

  TODBCErrorHandler = Class
  Private
    Function Errors(ARetCode: SQLRETURN; aHandleType: SQLSMALLINT; aHandle: SQLHANDLE): TList;
  Public
    { Public declarations }
    Procedure RaiseError(AOwner: TODBCObject; ARetCode: SQLRETURN);
    Function Success(RetCode: SQLRETURN): Boolean;
    Function SuccessOnly(RetCode: SQLRETURN): Boolean;
  End;

  { TChildPtr }
  TChildPtr = ^TChildRec;
  TChildRec = Record
    Child: TObject;
    Next: TChildPtr;
  End;

  { THenv }
  THenv = Class(TODBCObject)
  Private
    { Private declarations }
    FError: TODBCErrorHandler;
    FHenv: SQLHENV;
    FActive: Boolean;
    FRetCode: SQLRETURN;
    FConnectionPooling: TConnectionPooling;

    Function TerminateHandle: Boolean;
    Procedure SetConnectionPooling(AConnectionPooling: TConnectionPooling);
  Protected
    { Protected declarations }
    Function Init: Boolean;
  Public
    { Public declarations }
    Property Handle: SQLHENV Read FHenv;
    Property Active: Boolean Read FActive;
    Property Error: TODBCErrorHandler Read FError Write FError;
    Property ConnectionPooling: TConnectionPooling Read FConnectionPooling Write SetConnectionPooling
      Default DefConnectionPooling;

    Constructor Create;
    Destructor Destroy; Override;
  End;

  { TODBCContext }
  TODBCContext = Class (TODBCObject)
  Protected
    FEnv : THenv;
  Public
    Constructor Create(Env : THEnv); Virtual;
  End;

  { TDriverPtr }
  TDriverPtr = ^TDriverRec;
  TDriverRec = Record
    Desc: String;
    PS_SQL_CHAR,
    PS_SQL_VARCHAR,
    PS_SQL_LONGVARCHAR,
    PS_SQL_BINARY,
    PS_SQL_VARBINARY,
    PS_SQL_LONGVARBINARY,
    PS_SQL_DECIMAL,
    PS_SQL_NUMERIC,
    PS_SQL_TYPE_TIMESTAMP: SQLUINTEGER;
    DD_SQL_DECIMAL,
    DD_SQL_NUMERIC,
    DD_SQL_TYPE_TIMESTAMP: SQLSMALLINT;
  End;

  { THdbc }
  THdbc = Class (TODBCContext)
  Private
    { Private declarations }
    FHdbc: SQLHDBC;
    FActive: Boolean;
    FRetCode: SQLRETURN;
    FConnectionPooling: TConnectionPooling;
    FConnected, FStreamedConnected: Boolean;
    FDriver: String;
    FDataSource: String;
    FUserName: String;
    FPassword: String;
    FForceEmptyPasswordsIntoConnectionString : Boolean;
    FAttributes: TStrings;
    FIsolationLevel: SQLUINTEGER;
    FInfoPrompt: SQLUSMALLINT;
    FCursorLib: SQLUINTEGER;
    FTracing: Boolean;
    FCore: Boolean;
    FDrivers: TList;
    FPlatform : TKDBPlatform;

    { Published Events }
    FBeforeConnect, FAfterConnect: TNotifyEvent;
    FBeforeDisconnect, FAfterDisconnect: TNotifyEvent;

    Function GetHandle: SQLHDBC;
    Function GetCore: Boolean;
    Procedure SetCore(ACore: Boolean);
    Function GetLoginTimeOut: SQLUINTEGER;
    Procedure SetLoginTimeOut(ALoginTimeOut: SQLUINTEGER);
    Procedure SetCursorLib(ACursorLib: SQLUINTEGER);
    Procedure SetTracing(ATracing: Boolean);
    Function GetInTransaction: Boolean;
    Function GetConnectionPooling: TConnectionPooling;
    Procedure SetConnectionPooling(AConnectionPooling: TConnectionPooling);
    Procedure SetDriver(ADriver: String);
    Procedure SetDataSource(ADataSource: String);
    Procedure SetUserName(AUserName: String);
    Procedure SetPassword(APassword: String);
    Procedure SetAttributes(AAttributes: TStrings);
    Procedure SetIsolationLevel(AIsolationLevel: SQLUINTEGER);
    Function GetConnected: Boolean;
    Procedure SetConnected(AConnected: Boolean);
    Function GetVersion: String;
    Procedure SetVersion(AVersion: String);
    procedure TerminateHandle;
  Protected
    { Protected declarations }
    Function Init: Boolean;
    Procedure DoBeforeConnect; Virtual;
    Procedure DoAfterConnect; Virtual;
    Procedure DoBeforeDisconnect; Virtual;
    Procedure DoAfterDisconnect; Virtual;
  Public
    { Public declarations }
    Property Handle: SQLHDBC Read GetHandle;
    Property Active: Boolean Read FActive;
    Property Core: Boolean Read GetCore Write SetCore
      Default DefCore;
    Property LoginTimeOut: SQLUINTEGER Read GetLoginTimeOut Write SetLoginTimeOut;
    Property Tracing: Boolean Read FTracing Write SetTracing
      Default DefTracing;
    Property InTransaction: Boolean Read GetInTransaction;

    Constructor Create(Env : THEnv); Override;
    Destructor Destroy; Override;
//    Function Terminate: Boolean; Override;
//    Procedure Resolve;
    Procedure Connect;
    Procedure Disconnect;
    Procedure StartTransact;
    Procedure EndTransact;
    Procedure Commit;
    Procedure Rollback;
    Function GetFunction(FunctionID: SQLUSMALLINT): Boolean;
    Function GetInfoString(InfoType: SQLUSMALLINT): String;
    Function GetInfoSmallint(InfoType: SQLUSMALLINT): SQLUSMALLINT;
    Function GetInfoInteger(InfoType: SQLUSMALLINT): SQLUINTEGER;

    Procedure RefreshDrivers;
    Procedure ClearDrivers;
    Function AddDriver(ADriver: String): TDriverPtr;
    Procedure RemoveDriver(ADriver: String);
    Function GetDriver(ADriver: String): TDriverPtr;
    Function CurrentDriver: String;
    Function IsDriver(Const ADrivers: Array Of String): Boolean;

    Procedure SetSpecialSQLConnectionAttribute(AAttribute: SQLINTEGER; AValue: SQLPOINTER; AStringLength: SQLINTEGER);
  public
    { Published declarations }
    Property Connected: Boolean Read GetConnected Write SetConnected
      Default DefConnected;
    Property Driver: String Read FDriver Write SetDriver;
    Property DataSource: String Read FDataSource Write SetDataSource;
    Property UserName: String Read FUserName Write SetUserName;
    Property Password: String Read FPassword Write SetPassword;
    Property Attributes: TStrings Read FAttributes Write SetAttributes;
    Property ForceEmptyPasswordsIntoConnectionString : Boolean Read FForceEmptyPasswordsIntoConnectionString Write FForceEmptyPasswordsIntoConnectionString;
    Property IsolationLevel: SQLUINTEGER Read FIsolationLevel Write SetIsolationLevel
      Default DefIsolationLevel;
    Property InfoPrompt: SQLUSMALLINT Read FInfoPrompt Write FInfoPrompt
      Default DefInfoPrompt;
    Property CursorLib: SQLUINTEGER Read FCursorLib Write SetCursorLib
      Default DefCursorLib;
    Property ConnectionPooling: TConnectionPooling Read GetConnectionPooling Write SetConnectionPooling;
    Property Version: String Read GetVersion Write SetVersion;
    Property BeforeConnect: TNotifyEvent Read FBeforeConnect Write FBeforeConnect;
    Property AfterConnect: TNotifyEvent Read FAfterConnect Write FAfterConnect;
    Property BeforeDisconnect: TNotifyEvent Read FBeforeDisconnect Write FBeforeDisconnect;
    Property AfterDisconnect: TNotifyEvent Read FAfterDisconnect Write FAfterDisconnect;
  End;

  { TCommonPtr }
  TCommonPtr = ^TCommonRec;
  TCommonRec = Record
    Next: TCommonPtr;
    FValue: SQLPOINTER;
    FSize: SQLLENPtr;
  End;

  { TParamPtr }
  TParamPtr = ^TParamRec;
  TParamRec = Record
    Next: TParamPtr;
    FValue: SQLPOINTER;
    FSize: SQLLENPtr;
    FType: SQLSMALLINT;
    FSql: SQLSMALLINT;

    FParam: SQLUSMALLINT;
    FCount: SQLUINTEGER;

    FParameterSize: SQLUINTEGER;
    FDecimalDigits: SQLSMALLINT;
    FNullable: SQLSMALLINT;
  End;

  { TColBindPtr }
  TColBindPtr = ^TColBindRec;
  TColBindRec = Record
    FCol: SQLUSMALLINT;
    FSql: SQLSMALLINT;

    Next: TColBindPtr;
  End;

  { TColPtr }
  TColPtr = ^TColRec;
  TColRec = Record
    Next: TColPtr;
    FValue: SQLPOINTER;
    FSize: SQLLENPtr;
    FType: SQLSMALLINT;
    FSql: SQLSMALLINT;

    FMemory: TMemoryStream;
    FBlob, FBlobFetched: Boolean;
    FFormatStyle: TFormatStyle;
    FFormatMask: String;
    FPrimary: Boolean;
    FIgnore: Boolean;

    FColumnSize: SQLUINTEGER;
    FDecimalDigits: SQLSMALLINT;
    FNullable: SQLSMALLINT;
  End;

  { TRowPtr }
  TRowPtr = ^TRowRec;
  TRowRec = Record
    FType: SQLSMALLINT;
    FValue: SQLPOINTER;
    FSize: SQLINTEGERPtr;
    FBlob: Boolean;
  End;

  { TConfirmEvent }
  TConfirmEvent = Function (Sender: TObject;
                            DefaultMsg: String): Boolean Of Object;

  { TStatementEvent }
  TStatementEvent = Procedure (Sender: TObject;
                               Operation: Word;
                               Var SQL: String) Of Object;

  { TRowCountEvent }
  TRowCountEvent = Function (Sender: TObject): Integer Of Object;

  { THstmt }
  THstmt = Class (TODBCContext)
  Private
    { Private declarations }
    FHdbc: THdbc;
    FHstmt: SQLHSTMT;
    FHdesc: SQLHDESC;
    FActive: Boolean;
    FRetCode: SQLRETURN;

    //Linearly searched but never used
    FColBinds: TColBindPtr;

    FCols: TColPtr;
    FColIndexes: Array of TColPtr;

    FParams: TParamPtr;
    FParamIndexes: Array of TParamPtr;

    FRowStatus: TColPtr;
    FRowFlags: TColPtr;
    FRowBookmark: TColPtr;
    FNumCols: SQLSMALLINT;    //#cols in result set
    FNumRows: SQLUINTEGER;    //#values per col
    FNumParams: SQLUINTEGER;  //#values per param
    FBulkData: Boolean;
    FBlobs: Boolean;
    FColumnsBound: Boolean;
    FBindBookmarks: Boolean;
    FBookmarkSize: SQLLEN;
    FPrepared: Boolean;
    FExecuted: Boolean;
    FParamType: SQLSMALLINT;
    FTargetTable: String;
    FTableOwner, FTableName: String;
    FSkipByPosition: Boolean;
    FSkipByCursor: Boolean;
    FParamNames: TStringList;
    FColNames: TStringList;
    FSQL: String;
    FSQLParsing: Boolean;
    FCursorAttr: SQLUINTEGER;
    FConcurrencyType: SQLUINTEGER;
    FCursorType: SQLUINTEGER;
    FRowSetSize: SQLUINTEGER; { Set Before Prepare }
    FBlobSize: LongInt;
    FBlobDeferral: Boolean;
    FBlobPlacement: TBlobPlacement;
    FExecAsync, FAborted, FAsyncEnabled: Boolean;
    FEmptyToNull: TEmptyToNull;
    FStringTrimming: TStringTrimming;
    FBindByName: Boolean;
    FRowCountMethod: TRowCountMethod;
    FNoRowsAffected: TNoRowsAffected;
    FHstmtInsert, FHstmtUpdate, FHstmtDelete, FHstmtRefresh: THstmt;

    { Published Events }
    FBeforePrepare, FAfterPrepare: TNotifyEvent;
    FBeforeExecute, FAfterExecute: TNotifyEvent;
    FBeforeFetch, FAfterFetch: TNotifyEvent;
    FOnInsert: TConfirmEvent;
    FOnUpdate: TConfirmEvent;
    FOnDelete: TConfirmEvent;
    FOnRefresh: TConfirmEvent;
    FOnStatement: TStatementEvent;
    FOnRowCount: TRowCountEvent;

    Procedure FreeParams;
    Procedure FreeCols;
    Procedure FreeColBinds;
    Procedure InsertHead(FParam: SQLUSMALLINT;
                         FType: SQLSMALLINT;
                         FSql: SQLSMALLINT;
                         FValue: SQLPOINTER);
    Procedure InsertTail(Var FTail: TColPtr;
                         FType: SQLSMALLINT;
                         FSql: SQLSMALLINT;
                         FValue: SQLPOINTER);
    Procedure InsertColBind(FCol: SQLUSMALLINT;
                            FSql: SQLSMALLINT);
    Function ParamRec(Param: SQLUSMALLINT): TParamPtr;
    Function ColRec(Col: SQLUSMALLINT): TColPtr;
    Function RowRec(Col, Row: SQLUSMALLINT): TRowPtr;
    Function RowRecEx(Col, Row: SQLUSMALLINT; Out VrRowRec : TRowRec): Boolean;
    Function ColBindRec(Col: SQLUSMALLINT): TColBindPtr;
    Function RowFlags(Row: SQLUSMALLINT): SQLUSMALLINTPtr;
    Function BindCore: Boolean;
    Procedure BindParamMain(Param: SQLUSMALLINT;
                            ParamType: SQLSMALLINT;
                            ParamValue: SQLPOINTER;
                            SqlType: SQLSMALLINT;
                            ParameterSize: SQLULEN;
                            DecimalDigits: SQLSMALLINT;
                            Nullable: SQLSMALLINT;
                            Bulk: Integer);
    Procedure BindCols;
    Procedure BindBlobCols(Bind: Boolean);
    Function Fetch(FetchType: SQLSMALLINT;
                   Row: SQLINTEGER): Boolean;
    Function FetchCol(Col: SQLUSMALLINT;
                      ColType: SQLSMALLINT;
                      ColStream: TStream): SQLINTEGER;
    Function FetchCell(Col, Row: SQLUSMALLINT;
                       ColType: SQLSMALLINT;
                       ColStream: TStream): SQLINTEGER;
    Procedure AsyncEnable(Enabled: Boolean);
    Procedure DataAtExecution(FList: TCommonPtr);
    Function GetPosOpts: SQLINTEGER;
    Function GetPosStmts: SQLINTEGER;
    Function CursorName: String;
    Procedure DetermineTargetTable;
    Procedure DeterminePrimaryCols;
    Procedure DetermineIgnoreCols;
    Function PrimaryClause: String;
    Procedure BindClause(Var Param: SQLUSMALLINT;
                         AHstmt: THstmt;
                         PrimaryClause: Boolean;
                         AssignOnly: Boolean);
    Procedure InsertFields;
    Procedure UpdateFields(WhereClause: String;
                           BindWhere: Boolean);
    Procedure DeleteFields(WhereClause: String;
                           BindWhere: Boolean);
    Procedure RefreshFields(WhereClause: String);
    Function ParseSQL: String;

    Procedure InsertRow(Row: SQLUSMALLINT);
    Procedure UpdateRow(Row: SQLUSMALLINT);
    Procedure DeleteRow(Row: SQLUSMALLINT);
    Procedure RefreshRow(Row: SQLUSMALLINT);
    Procedure InsertRows;
    Procedure UpdateRows;
    Procedure DeleteRows;
    Procedure RefreshRows;

    { Get/Set Methods }
    Function GetHandle: SQLHSTMT;
    Procedure SetHdbc(AHdbc: THdbc);
    Procedure SetSQL(ASQL: String);
    Procedure SetPrepared(APrepared: Boolean);
    Procedure SetExecuted(AExecuted: Boolean);
    Function GetParamSize(Param: SQLUSMALLINT): SQLINTEGER;
    Procedure SetParamSize(Param: SQLUSMALLINT;
                           AParamSize: SQLINTEGER);
    Procedure SetParamType(AParamType: SQLSMALLINT);
    Procedure SetBulkSize(ABulkSize: SQLUINTEGER);
    Function GetBulkParamSize(Param: SQLUSMALLINT;
                              Row: SQLUINTEGER): SQLINTEGER;
    Procedure SetBulkParamSize(Param: SQLUSMALLINT;
                               Row: SQLUINTEGER;
                               AParamSize: SQLINTEGER);
    Procedure SetTargetTable(ATargetTable: String);
    Function GetColValue(Col: SQLUSMALLINT): SQLPOINTER;
    Function GetCellValue(Col, Row: SQLUSMALLINT): SQLPOINTER;
    Function GetColType(Col: SQLUSMALLINT): SQLSMALLINT;
    Function GetSqlType(Col: SQLUSMALLINT): SQLSMALLINT;
    Function GetBlobCol(Col: SQLUSMALLINT): Boolean;
    Function GetColSize(Col: SQLUSMALLINT): SQLINTEGER;
    Procedure SetColSize(Col: SQLUSMALLINT;
                         AColSize: SQLINTEGER);
    Function GetCellSize(Col, Row: SQLUSMALLINT): SQLINTEGER;
    Procedure SetCellSize(Col, Row: SQLUSMALLINT;
                          AColSize: SQLINTEGER);
    Function GetColNull(Col: SQLUSMALLINT): Boolean;
    Procedure SetColNull(Col: SQLUSMALLINT;
                         AColNull: Boolean);
    Function GetCellNull(Col, Row: SQLUSMALLINT): Boolean;
    Procedure SetCellNull(Col, Row: SQLUSMALLINT;
                          ACellNull: Boolean);
    Function GetColIgnore(Col: SQLUSMALLINT): Boolean;
    Procedure SetColIgnore(Col: SQLUSMALLINT;
                           AColIgnore: Boolean);
    Function GetCellIgnore(Col, Row: SQLUSMALLINT): Boolean;
    Procedure SetCellIgnore(Col, Row: SQLUSMALLINT;
                            ACellIgnore: Boolean);
    Function GetFormatStyle(Col: SQLUSMALLINT): TFormatStyle;
    Procedure SetFormatStyle(Col: SQLUSMALLINT;
                             AFormatStyle: TFormatStyle);
    Function GetFormatMask(Col: SQLUSMALLINT): String;
    Procedure SetFormatMask(Col: SQLUSMALLINT;
                            AFormatMask: String);
    Function GetColPrimary(Col: SQLUSMALLINT): Boolean;
    Procedure SetColPrimary(Col: SQLUSMALLINT;
                            AColPrimary: Boolean);
    Function GetColPrecision(Col: SQLUSMALLINT): SQLUINTEGER;
    Function GetColScale(Col: SQLUSMALLINT): SQLSMALLINT;
    Function GetColNullable(Col: SQLUSMALLINT): SQLSMALLINT;
    Function GetParamPrecision(Param: SQLUSMALLINT): SQLUINTEGER;
    Function GetParamScale(Param: SQLUSMALLINT): SQLSMALLINT;
    Function GetParamNullable(Param: SQLUSMALLINT): SQLSMALLINT;
    Function GetRowStatus(Row: SQLUSMALLINT): SQLUSMALLINT;
    Function GetRowFlag(Row: SQLUSMALLINT): SQLUSMALLINT;
    Procedure SetRowFlag(Row: SQLUSMALLINT;
                         ARowFlag: SQLUSMALLINT);
    Function GetRowValid(Row: SQLUSMALLINT): Boolean;
    Function GetParamNames: TStringList;
    Function GetColNames: TStringList;
    Function GetBookmark: SQLPOINTER;
    Procedure SetBookmark(ABookmark: SQLPOINTER);
    Function GetQueryTimeOut: SQLUINTEGER;
    Procedure SetQueryTimeOut(AQueryTimeOut: SQLUINTEGER);
    Function GetMaxRows: SQLUINTEGER;
    Procedure SetMaxRows(AMaxRows: SQLUINTEGER);
    Function GetColCount: SQLSMALLINT;
    Function GetRowCount: SQLINTEGER;
    Function GetRowsFetched: SQLUINTEGER;
    Function GetRowsAffected: SQLULEN;
    Procedure SetConcurrencyType(AConcurrencyType: SQLUINTEGER);
    Procedure SetCursorType(ACursorType: SQLUINTEGER);
    Procedure SetRowSetSize(ARowSetSize: SQLUINTEGER);
    Procedure SetSkipByCursor(ASkipByCursor: Boolean);
    Procedure SetSkipByPosition(ASkipByPosition: Boolean);
    Procedure SetBlobSize(ABlobSize: LongInt);
    Function GetTableOwner: String;
    Function GetTableName: String;
    Function GetPrimaryColNames: String;
    Function GetIgnoreColNames: String;

    Function GetColString(Col: SQLUSMALLINT): String;
    Function GetColSingle(Col: SQLUSMALLINT): Single;
    Function GetColDouble(Col: SQLUSMALLINT): Double;
    Function GetColBoolean(Col: SQLUSMALLINT): Boolean;
    Function GetColShortint(Col: SQLUSMALLINT): ShortInt;
    Function GetColByte(Col: SQLUSMALLINT): Byte;
    Function GetColSmallint(Col: SQLUSMALLINT): SmallInt;
    Function GetColWord(Col: SQLUSMALLINT): Word;
    Function GetColInteger(Col: SQLUSMALLINT): Integer;
    Function GetColCardinal(Col: SQLUSMALLINT): Cardinal;
    Function GetColLongint(Col: SQLUSMALLINT): LongInt;
    Function GetColLongword(Col: SQLUSMALLINT): LongWord;
    Function GetColInt64(Col: SQLUSMALLINT): Int64;
    Function GetColDate(Col: SQLUSMALLINT): TDate;
    Function GetColTime(Col: SQLUSMALLINT): TTime;
    Function GetColTimeStamp(Col: SQLUSMALLINT): DateSupport.TTimeStamp;
    Function GetColMemory(Col: SQLUSMALLINT): TMemoryStream;
    Function GetColVariant(Col: SQLUSMALLINT): Variant;

    Procedure SetColString(Col: SQLUSMALLINT;
                           AValue: String);
    Procedure SetColSingle(Col: SQLUSMALLINT;
                           AValue: Single);
    Procedure SetColDouble(Col: SQLUSMALLINT;
                           AValue: Double);
    Procedure SetColBoolean(Col: SQLUSMALLINT;
                            AValue: Boolean);
    Procedure SetColShortint(Col: SQLUSMALLINT;
                             AValue: ShortInt);
    Procedure SetColByte(Col: SQLUSMALLINT;
                         AValue: Byte);
    Procedure SetColSmallint(Col: SQLUSMALLINT;
                             AValue: SmallInt);
    Procedure SetColWord(Col: SQLUSMALLINT;
                         AValue: Word);
    Procedure SetColInteger(Col: SQLUSMALLINT;
                            AValue: Integer);
    Procedure SetColCardinal(Col: SQLUSMALLINT;
                             AValue: Cardinal);
    Procedure SetColLongint(Col: SQLUSMALLINT;
                            AValue: LongInt);
    Procedure SetColLongword(Col: SQLUSMALLINT;
                             AValue: LongWord);
    Procedure SetColInt64(Col: SQLUSMALLINT;
                          AValue: Int64);
    Procedure SetColDate(Col: SQLUSMALLINT;
                         AValue: TDate);
    Procedure SetColTime(Col: SQLUSMALLINT;
                         AValue: TTime);
    Procedure SetColTimeStamp(Col: SQLUSMALLINT;
                              AValue: DateSupport.TTimeStamp);
    Procedure SetColMemory(Col: SQLUSMALLINT;
                           AValue: TMemoryStream);
    Procedure SetColVariant(Col: SQLUSMALLINT;
                            AValue: Variant);

    Function GetColStringByName(ColName: String): String;
    Function GetColSingleByName(ColName: String): Single;
    Function GetColDoubleByName(ColName: String): Double;
    Function GetColBooleanByName(ColName: String): Boolean;
    Function GetColShortintByName(ColName: String): ShortInt;
    Function GetColByteByName(ColName: String): Byte;
    Function GetColSmallintByName(ColName: String): SmallInt;
    Function GetColWordByName(ColName: String): Word;
    Function GetColIntegerByName(ColName: String): Integer;
    Function GetColCardinalByName(ColName: String): Cardinal;
    Function GetColLongintByName(ColName: String): LongInt;
    Function GetColLongwordByName(ColName: String): LongWord;
    Function GetColInt64ByName(ColName: String): Int64;    
    Function GetColDateByName(ColName: String): TDate;
    Function GetColTimeByName(ColName: String): TTime;
    Function GetColTimeStampByName(ColName: String): DateSupport.TTimeStamp;
    Function GetColMemoryByName(ColName: String): TMemoryStream;
    Function GetColVariantByName(ColName: String): Variant;

    Procedure SetColStringByName(ColName: String;
                                 AValue: String);
    Procedure SetColSingleByName(ColName: String;
                                 AValue: Single);
    Procedure SetColDoubleByName(ColName: String;
                                 AValue: Double);
    Procedure SetColBooleanByName(ColName: String;
                                  AValue: Boolean);
    Procedure SetColShortintByName(ColName: String;
                                   AValue: ShortInt);
    Procedure SetColByteByName(ColName: String;
                               AValue: Byte);
    Procedure SetColSmallintByName(ColName: String;
                                   AValue: SmallInt);
    Procedure SetColWordByName(ColName: String;
                               AValue: Word);
    Procedure SetColIntegerByName(ColName: String;
                                  AValue: Integer);
    Procedure SetColCardinalByName(ColName: String;
                                   AValue: Cardinal);
    Procedure SetColLongintByName(ColName: String;
                                  AValue: LongInt);
    Procedure SetColLongwordByName(ColName: String;
                                   AValue: LongWord);
    Procedure SetColInt64ByName(ColName: String;
                                AValue: Int64);
    Procedure SetColDateByName(ColName: String;
                               AValue: TDate);
    Procedure SetColTimeByName(ColName: String;
                               AValue: TTime);
    Procedure SetColTimeStampByName(ColName: String;
                                    AValue: DateSupport.TTimeStamp);
    Procedure SetColMemoryByName(ColName: String;
                                 AValue: TMemoryStream);
    Procedure SetColVariantByName(ColName: String;
                                  AValue: Variant);

    Function GetCellString(Col, Row: SQLUSMALLINT): String;
    Function GetCellSingle(Col, Row: SQLUSMALLINT): Single;
    Function GetCellDouble(Col, Row: SQLUSMALLINT): Double;
    Function GetCellBoolean(Col, Row: SQLUSMALLINT): Boolean;
    Function GetCellShortint(Col, Row: SQLUSMALLINT): ShortInt;
    Function GetCellByte(Col, Row: SQLUSMALLINT): Byte;
    Function GetCellSmallint(Col, Row: SQLUSMALLINT): SmallInt;
    Function GetCellWord(Col, Row: SQLUSMALLINT): Word;
    Function GetCellInteger(Col, Row: SQLUSMALLINT): Integer;
    Function GetCellCardinal(Col, Row: SQLUSMALLINT): Cardinal;
    Function GetCellLongint(Col, Row: SQLUSMALLINT): LongInt;
    Function GetCellLongword(Col, Row: SQLUSMALLINT): LongWord;
    Function GetCellInt64(Col, Row: SQLUSMALLINT): Int64;
    Function GetCellDate(Col, Row: SQLUSMALLINT): TDate;
    Function GetCellTime(Col, Row: SQLUSMALLINT): TTime;
    Function GetCellTimeStamp(Col, Row: SQLUSMALLINT): DateSupport.TTimeStamp;
    Function GetCellMemory(Col, Row: SQLUSMALLINT): TMemoryStream;
    Function GetCellVariant(Col, Row: SQLUSMALLINT): Variant;

    Procedure SetCellString(Col, Row: SQLUSMALLINT;
                            AValue: String);
    Procedure SetCellSingle(Col, Row: SQLUSMALLINT;
                            AValue: Single);
    Procedure SetCellDouble(Col, Row: SQLUSMALLINT;
                            AValue: Double);
    Procedure SetCellBoolean(Col, Row: SQLUSMALLINT;
                             AValue: Boolean);
    Procedure SetCellShortint(Col, Row: SQLUSMALLINT;
                              AValue: ShortInt);
    Procedure SetCellByte(Col, Row: SQLUSMALLINT;
                          AValue: Byte);
    Procedure SetCellSmallint(Col, Row: SQLUSMALLINT;
                              AValue: SmallInt);
    Procedure SetCellWord(Col, Row: SQLUSMALLINT;
                          AValue: Word);
    Procedure SetCellInteger(Col, Row: SQLUSMALLINT;
                             AValue: Integer);
    Procedure SetCellCardinal(Col, Row: SQLUSMALLINT;
                              AValue: Cardinal);
    Procedure SetCellLongint(Col, Row: SQLUSMALLINT;
                             AValue: LongInt);
    Procedure SetCellLongword(Col, Row: SQLUSMALLINT;
                              AValue: LongWord);
    Procedure SetCellInt64(Col, Row: SQLUSMALLINT;
                           AValue: Int64);
    Procedure SetCellDate(Col, Row: SQLUSMALLINT;
                          AValue: TDate);
    Procedure SetCellTime(Col, Row: SQLUSMALLINT;
                          AValue: TTime);
    Procedure SetCellTimeStamp(Col, Row: SQLUSMALLINT;
                               AValue: DateSupport.TTimeStamp);
    Procedure SetCellMemory(Col, Row: SQLUSMALLINT;
                            AValue: TMemoryStream);
    Procedure SetCellVariant(Col, Row: SQLUSMALLINT;
                             AValue: Variant);

    Function GetCellStringByName(ColName: String;
                                 Row: SQLUSMALLINT): String;
    Function GetCellSingleByName(ColName: String;
                                 Row: SQLUSMALLINT): Single;
    Function GetCellDoubleByName(ColName: String;
                                 Row: SQLUSMALLINT): Double;
    Function GetCellBooleanByName(ColName: String;
                                  Row: SQLUSMALLINT): Boolean;
    Function GetCellShortintByName(ColName: String;
                                   Row: SQLUSMALLINT): ShortInt;
    Function GetCellByteByName(ColName: String;
                               Row: SQLUSMALLINT): Byte;
    Function GetCellSmallintByName(ColName: String;
                                   Row: SQLUSMALLINT): SmallInt;
    Function GetCellWordByName(ColName: String;
                               Row: SQLUSMALLINT): Word;
    Function GetCellIntegerByName(ColName: String;
                                  Row: SQLUSMALLINT): Integer;
    Function GetCellCardinalByName(ColName: String;
                                   Row: SQLUSMALLINT): Cardinal;
    Function GetCellLongintByName(ColName: String;
                                  Row: SQLUSMALLINT): LongInt;
    Function GetCellLongwordByName(ColName: String;
                                   Row: SQLUSMALLINT): LongWord;
    Function GetCellInt64ByName(ColName: String;
                                Row: SQLUSMALLINT): Int64;
    Function GetCellDateByName(ColName: String;
                               Row: SQLUSMALLINT): TDate;
    Function GetCellTimeByName(ColName: String;
                               Row: SQLUSMALLINT): TTime;
    Function GetCellTimeStampByName(ColName: String;
                                    Row: SQLUSMALLINT): DateSupport.TTimeStamp;
    Function GetCellMemoryByName(ColName: String;
                                 Row: SQLUSMALLINT): TMemoryStream;
    Function GetCellVariantByName(ColName: String;
                                  Row: SQLUSMALLINT): Variant;

    Procedure SetCellStringByName(ColName: String;
                                  Row: SQLUSMALLINT;
                                  AValue: String);
    Procedure SetCellSingleByName(ColName: String;
                                  Row: SQLUSMALLINT;
                                  AValue: Single);
    Procedure SetCellDoubleByName(ColName: String;
                                  Row: SQLUSMALLINT;
                                  AValue: Double);
    Procedure SetCellBooleanByName(ColName: String;
                                   Row: SQLUSMALLINT;
                                   AValue: Boolean);
    Procedure SetCellShortintByName(ColName: String;
                                    Row: SQLUSMALLINT;
                                    AValue: ShortInt);
    Procedure SetCellByteByName(ColName: String;
                                Row: SQLUSMALLINT;
                                AValue: Byte);
    Procedure SetCellSmallintByName(ColName: String;
                                    Row: SQLUSMALLINT;
                                    AValue: SmallInt);
    Procedure SetCellWordByName(ColName: String;
                                Row: SQLUSMALLINT;
                                AValue: Word);
    Procedure SetCellIntegerByName(ColName: String;
                                   Row: SQLUSMALLINT;
                                   AValue: Integer);
    Procedure SetCellCardinalByName(ColName: String;
                                    Row: SQLUSMALLINT;
                                    AValue: Cardinal);
    Procedure SetCellLongintByName(ColName: String;
                                   Row: SQLUSMALLINT;
                                   AValue: LongInt);
    Procedure SetCellLongwordByName(ColName: String;
                                    Row: SQLUSMALLINT;
                                    AValue: LongWord);
    Procedure SetCellInt64ByName(ColName: String;
                                 Row: SQLUSMALLINT;
                                 AValue: Int64);
    Procedure SetCellDateByName(ColName: String;
                                Row: SQLUSMALLINT;
                                AValue: TDate);
    Procedure SetCellTimeByName(ColName: String;
                                Row: SQLUSMALLINT;
                                AValue: TTime);
    Procedure SetCellTimeStampByName(ColName: String;
                                     Row: SQLUSMALLINT;
                                     AValue: DateSupport.TTimeStamp);
    Procedure SetCellMemoryByName(ColName: String;
                                  Row: SQLUSMALLINT;
                                  AValue: TMemoryStream);
    Procedure SetCellVariantByName(ColName: String;
                                   Row: SQLUSMALLINT;
                                   AValue: Variant);

  Protected
    { Protected declarations }
    Function Init: Boolean;
    Procedure DoBeforePrepare; Virtual;
    Procedure DoAfterPrepare; Virtual;
    Procedure DoBeforeExecute; Virtual;
    Procedure DoAfterExecute; Virtual;
    Procedure DoBeforeFetch; Virtual;
    Procedure DoAfterFetch; Virtual;
    Function DoRowCount: Integer; Virtual;
    Procedure DescribeParam(Param: SQLUSMALLINT;
                            Var SqlType: SQLSMALLINT;
                            Var ParameterSize: SQLULEN;
                            Var DecimalDigits: SQLSMALLINT;
                            Var Nullable: SQLSMALLINT;
                            Core: Boolean);
    Function ColAttrString(Col: SQLUSMALLINT;
                           FieldIdentifier: SQLUSMALLINT): String;
    Function ColAttrInteger(Col: SQLUSMALLINT;
                            FieldIdentifier: SQLUSMALLINT): SQLINTEGER;
    Procedure UnPrepareHstmts;

    Property TableOwner: String Read GetTableOwner;
    Property TableName: String Read GetTableName;
    Property PrimaryColNames: String Read GetPrimaryColNames;
    Property IgnoreColNames: String Read GetIgnoreColNames;
  Public
    { Public declarations }
    Constructor Create(Env : THEnv; dbc : THdbc); reintroduce; virtual;
    Destructor Destroy; Override;
    Function Terminate: Boolean;
    Procedure Close; Virtual;
    Procedure CloseCursor; Virtual;
    Function ParamByName(ParamName: String): SQLUSMALLINT;
    Function ColByName(ColName: String): SQLUSMALLINT;
    Procedure NullCols(Const Cols: Array Of String);
    Procedure IgnoreCols(Const Cols: Array Of String);
    Procedure PrimaryCols(Const Cols: Array Of String);
    Procedure FreeBookmark(ABookmark: SQLPOINTER);

    Procedure Prepare;
    Procedure BindParam(Param: SQLUSMALLINT;
                        ParamType: SQLSMALLINT;
                        ParamValue: SQLPOINTER); Overload;
    Procedure BindParam(Param: SQLUSMALLINT;
                        ParamType: SQLSMALLINT;
                        ParamValue: SQLPOINTER;
                        SqlType: SQLSMALLINT); Overload;
    Procedure BindParamCore(Param: SQLUSMALLINT;
                            ParamType: SQLSMALLINT;
                            ParamValue: SQLPOINTER;
                            SqlType: SQLSMALLINT);
    Procedure BindParams(Param: SQLUSMALLINT;
                         ParamType: SQLSMALLINT;
                         ParamValue: SQLPOINTER;
                         Bulk: Integer);
    Procedure Execute;
    Procedure BindCol(Col: SQLUSMALLINT; SqlType: SQLSMALLINT);
    Procedure DynamicallySetRowSetSize(ARowSetSize: SQLUINTEGER);
    Function FetchFirst: Boolean;
    Function FetchNext: Boolean;
    Function FetchLast: Boolean;
    Function FetchPrev: Boolean;
    Function FetchAbsolute(Row: SQLINTEGER): Boolean;
    Function FetchRelative(Row: SQLINTEGER): Boolean;
    Function FetchBookmark(Bookmark: SQLPOINTER): Boolean;
    Function MoreResults: Boolean;
    Procedure AbortQuery;
    Procedure ColStream(Col: SQLUSMALLINT;
                        Stream: TStream);
    Procedure CellStream(Col, Row: SQLUSMALLINT;
                         Stream: TStream);

    Procedure DoInsert; Virtual;
    Procedure DoUpdate; Virtual;
    Procedure DoDelete; Virtual;
    Procedure DoRefresh; Virtual;

    { Utility Functions }
    Function TypeString(SqlType: SQLSMALLINT;
                        SqlSize: LongInt): String;

    Procedure BindNull(Param: SQLUSMALLINT);
    Procedure BindNullString(Param: SQLUSMALLINT;
                             Var ParamValue: NullString);
    Procedure BindString(Param: SQLUSMALLINT;
                         Var ParamValue: String);
    Procedure BindSingle(Param: SQLUSMALLINT;
                         Var ParamValue: Single);
    Procedure BindDouble(Param: SQLUSMALLINT;
                         Var ParamValue: Double);
    Procedure BindShortint(Param: SQLUSMALLINT;
                           Var ParamValue: ShortInt);
    Procedure BindByte(Param: SQLUSMALLINT;
                       Var ParamValue: Byte);
    Procedure BindSmallint(Param: SQLUSMALLINT;
                           Var ParamValue: SmallInt);
    Procedure BindWord(Param: SQLUSMALLINT;
                       Var ParamValue: Word);
    Procedure BindInteger(Param: SQLUSMALLINT;
                          Var ParamValue: Integer);
    Procedure BindCardinal(Param: SQLUSMALLINT;
                           Var ParamValue: Cardinal);
    Procedure BindLongint(Param: SQLUSMALLINT;
                          Var ParamValue: LongInt);
    Procedure BindLongword(Param: SQLUSMALLINT;
                           Var ParamValue: LongWord);
    Procedure BindInt64(Param: SQLUSMALLINT;
                        Var ParamValue: Int64);
    Procedure BindDate(Param: SQLUSMALLINT;
                       Var ParamValue: TDate);
    Procedure BindTime(Param: SQLUSMALLINT;
                       Var ParamValue: TTime);
    Procedure BindTimeStamp(Param: SQLUSMALLINT;
                            Var ParamValue: DateSupport.TTimeStamp);
    Procedure BindMemory(Param: SQLUSMALLINT;
                         Var ParamValue: TMemoryStream;
                         Binary: Boolean);
    Procedure BindBinary(Param: SQLUSMALLINT;
                         Var ParamValue: TMemoryStream);
    Procedure BindText(Param: SQLUSMALLINT;
                       Var ParamValue: TMemoryStream);

    Procedure BindNullByName(ParamName: String);
    Procedure BindNullStringByName(ParamName: String;
                                   Var ParamValue: NullString);
    Procedure BindStringByName(ParamName: String;
                               Var ParamValue: String);
    Procedure BindSingleByName(ParamName: String;
                               Var ParamValue: Single);
    Procedure BindDoubleByName(ParamName: String;
                               Var ParamValue: Double);
    Procedure BindShortintByName(ParamName: String;
                                 Var ParamValue: ShortInt);
    Procedure BindByteByName(ParamName: String;
                             Var ParamValue: Byte);
    Procedure BindSmallintByName(ParamName: String;
                                 Var ParamValue: SmallInt);
    Procedure BindWordByName(ParamName: String;
                             Var ParamValue: Word);
    Procedure BindIntegerByName(ParamName: String;
                                Var ParamValue: Integer);
    Procedure BindCardinalByName(ParamName: String;
                                 Var ParamValue: Cardinal);
    Procedure BindLongintByName(ParamName: String;
                                Var ParamValue: LongInt);
    Procedure BindLongwordByName(ParamName: String;
                                 Var ParamValue: LongWord);
    Procedure BindInt64ByName(ParamName: String;
                              Var ParamValue: Int64);
    Procedure BindDateByName(ParamName: String;
                             Var ParamValue: TDate);
    Procedure BindTimeByName(ParamName: String;
                             Var ParamValue: TTime);
    Procedure BindTimeStampByName(ParamName: String;
                                  Var ParamValue: DateSupport.TTimeStamp);
    Procedure BindMemoryByName(ParamName: String;
                               Var ParamValue: TMemoryStream;
                               Binary: Boolean);
    Procedure BindBinaryByName(ParamName: String;
                               Var ParamValue: TMemoryStream);
    Procedure BindTextByName(ParamName: String;
                             Var ParamValue: TMemoryStream);

    Procedure BindNulls(Param: SQLUSMALLINT);                             
    Procedure BindNullStrings(Param: SQLUSMALLINT;
                              Var ParamValue: Array Of NullString);
    Procedure BindSingles(Param: SQLUSMALLINT;
                          Var ParamValue: Array Of Single);
    Procedure BindDoubles(Param: SQLUSMALLINT;
                          Var ParamValue: Array Of Double);
    Procedure BindShortints(Param: SQLUSMALLINT;
                            Var ParamValue: Array Of ShortInt);
    Procedure BindBytes(Param: SQLUSMALLINT;
                        Var ParamValue: Array Of Byte);
    Procedure BindSmallints(Param: SQLUSMALLINT;
                            Var ParamValue: Array Of SmallInt);
    Procedure BindWords(Param: SQLUSMALLINT;
                        Var ParamValue: Array Of Word);
    Procedure BindIntegers(Param: SQLUSMALLINT;
                           Var ParamValue: Array Of Integer);
    Procedure BindCardinals(Param: SQLUSMALLINT;
                            Var ParamValue: Array Of Cardinal);
    Procedure BindLongints(Param: SQLUSMALLINT;
                           Var ParamValue: Array Of LongInt);
    Procedure BindLongwords(Param: SQLUSMALLINT;
                            Var ParamValue: Array Of LongWord);
    Procedure BindInt64s(Param: SQLUSMALLINT;
                         Var ParamValue: Array Of Int64);
    Procedure BindDates(Param: SQLUSMALLINT;
                        Var ParamValue: Array Of TDate);
    Procedure BindTimes(Param: SQLUSMALLINT;
                        Var ParamValue: Array Of TTime);
    Procedure BindTimeStamps(Param: SQLUSMALLINT;
                             Var ParamValue: Array Of DateSupport.TTimeStamp);

    Procedure BindNullsByName(ParamName: String);
    Procedure BindNullStringsByName(ParamName: String;
                                    Var ParamValue: Array Of NullString);
    Procedure BindSinglesByName(ParamName: String;
                                Var ParamValue: Array Of Single);
    Procedure BindDoublesByName(ParamName: String;
                               Var ParamValue: Array Of Double);
    Procedure BindShortintsByName(ParamName: String;
                                  Var ParamValue: Array Of ShortInt);
    Procedure BindBytesByName(ParamName: String;
                              Var ParamValue: Array Of Byte);
    Procedure BindSmallintsByName(ParamName: String;
                                  Var ParamValue: Array Of SmallInt);
    Procedure BindWordsByName(ParamName: String;
                              Var ParamValue: Array Of Word);
    Procedure BindIntegersByName(ParamName: String;
                                 Var ParamValue: Array Of Integer);
    Procedure BindCardinalsByName(ParamName: String;
                                  Var ParamValue: Array Of Cardinal);
    Procedure BindLongintsByName(ParamName: String;
                                 Var ParamValue: Array Of LongInt);
    Procedure BindLongwordsByName(ParamName: String;
                                  Var ParamValue: Array Of LongWord);
    Procedure BindInt64sByName(ParamName: String;
                               Var ParamValue: Array Of Int64);
    Procedure BindDatesByName(ParamName: String;
                              Var ParamValue: Array Of TDate);
    Procedure BindTimesByName(ParamName: String;
                              Var ParamValue: Array Of TTime);
    Procedure BindTimeStampsByName(ParamName: String;
                                   Var ParamValue: Array Of DateSupport.TTimeStamp);

    Property Handle: SQLHSTMT Read GetHandle;
    Property Active: Boolean Read FActive;
    Property Prepared: Boolean Read FPrepared Write SetPrepared
      Default DefPrepared;
    Property Executed: Boolean Read FExecuted Write SetExecuted
      Default DefExecuted;
    Property ParamSize[Param: SQLUSMALLINT]: SQLINTEGER Read GetParamSize Write SetParamSize;
    Property ParamType: SQLSMALLINT Read FParamType Write SetParamType
      Default DefParamType;
    Property BulkSize: SQLUINTEGER Read FNumParams Write SetBulkSize;
    Property BulkParamSize[Param: SQLUSMALLINT;
                           Row: SQLUINTEGER]: SQLINTEGER Read GetBulkParamSize Write SetBulkParamSize;
    Property BulkData: Boolean Read FBulkData Write FBulkData
      Default DefBulkData;
    Property TargetTable: String Read FTargetTable Write SetTargetTable;
    Property ColValue[Col: SQLUSMALLINT]: SQLPOINTER Read GetColValue;
    Property CellValue[Col, Row: SQLUSMALLINT]: SQLPOINTER Read GetCellValue;
    Property ColType[Col: SQLUSMALLINT]: SQLSMALLINT Read GetColType;
    Property SqlType[Col: SQLUSMALLINT]: SQLSMALLINT Read GetSqlType;
    Property BlobCol[Col: SQLUSMALLINT]: Boolean Read GetBlobCol;
    Property ColSize[Col: SQLUSMALLINT]: SQLINTEGER Read GetColSize Write SetColSize;
    Property CellSize[Col, Row: SQLUSMALLINT]: SQLINTEGER Read GetCellSize Write SetCellSize;
    Property ColNull[Col: SQLUSMALLINT]: Boolean Read GetColNull Write SetColNull;
    Property CellNull[Col, Row: SQLUSMALLINT]: Boolean Read GetCellNull Write SetCellNull;
    Property ColIgnore[Col: SQLUSMALLINT]: Boolean Read GetColIgnore Write SetColIgnore;
    Property CellIgnore[Col, Row: SQLUSMALLINT]: Boolean Read GetCellIgnore Write SetCellIgnore;
    Property ColFormatStyle[Col: SQLUSMALLINT]: TFormatStyle Read GetFormatStyle Write SetFormatStyle;
    Property ColFormatMask[Col: SQLUSMALLINT]: String Read GetFormatMask Write SetFormatMask;
    Property ColPrimary[Col: SQLUSMALLINT]: Boolean Read GetColPrimary Write SetColPrimary;
    Property ColPrecision[Col: SQLUSMALLINT]: SQLUINTEGER Read GetColPrecision;
    Property ColScale[Col: SQLUSMALLINT]: SQLSMALLINT Read GetColScale;
    Property ColNullable[Col: SQLUSMALLINT]: SQLSMALLINT Read GetColNullable;
    Property ParamPrecision[Param: SQLUSMALLINT]: SQLUINTEGER Read GetParamPrecision;
    Property ParamScale[Param: SQLUSMALLINT]: SQLSMALLINT Read GetParamScale;
    Property ParamNullable[Param: SQLUSMALLINT]: SQLSMALLINT Read GetParamNullable;
    Property RowStatus[Row: SQLUSMALLINT]: SQLUSMALLINT Read GetRowStatus;
    Property RowFlag[Row: SQLUSMALLINT]: SQLUSMALLINT Read GetRowFlag Write SetRowFlag;
    Property RowValid[Row: SQLUSMALLINT]: Boolean Read GetRowValid;
    Property ParamNames: TStringList Read GetParamNames;
    Property ColNames: TStringList Read GetColNames;
    Property Bookmark: SQLPOINTER Read GetBookmark Write SetBookmark;
    Property BookmarkSize: SQLLEN Read FBookmarkSize;
    Property QueryTimeOut: SQLUINTEGER Read GetQueryTimeOut Write SetQueryTimeOut;
    Property MaxRows: SQLUINTEGER Read GetMaxRows Write SetMaxRows;
    Property ColCount: SQLSMALLINT Read GetColCount;
    Property RowCount: SQLINTEGER Read GetRowCount;
    Property RowsFetched: SQLUINTEGER Read GetRowsFetched;
    Property RowsAffected: SQLULEN Read GetRowsAffected;

    Property ColString[Col: SQLUSMALLINT]: String Read GetColString Write SetColString;
    Property ColSingle[Col: SQLUSMALLINT]: Single Read GetColSingle Write SetColSingle;
    Property ColDouble[Col: SQLUSMALLINT]: Double Read GetColDouble Write SetColDouble;
    Property ColBoolean[Col: SQLUSMALLINT]: Boolean Read GetColBoolean Write SetColBoolean;
    Property ColShortint[Col: SQLUSMALLINT]: ShortInt Read GetColShortint Write SetColShortint;
    Property ColByte[Col: SQLUSMALLINT]: Byte Read GetColByte Write SetColByte;
    Property ColSmallint[Col: SQLUSMALLINT]: SmallInt Read GetColSmallint Write SetColSmallint;
    Property ColWord[Col: SQLUSMALLINT]: Word Read GetColWord Write SetColWord;
    Property ColInteger[Col: SQLUSMALLINT]: Integer Read GetColInteger Write SetColInteger;
    Property ColCardinal[Col: SQLUSMALLINT]: Cardinal Read GetColCardinal Write SetColCardinal;
    Property ColLongint[Col: SQLUSMALLINT]: LongInt Read GetColLongint Write SetColLongint;
    Property ColLongword[Col: SQLUSMALLINT]: LongWord Read GetColLongword Write SetColLongword;
    Property ColInt64[Col: SQLUSMALLINT]: Int64 Read GetColInt64 Write SetColInt64;    
    Property ColDate[Col: SQLUSMALLINT]: TDate Read GetColDate Write SetColDate;
    Property ColTime[Col: SQLUSMALLINT]: TTime Read GetColTime Write SetColTime;
    Property ColTimeStamp[Col: SQLUSMALLINT]: DateSupport.TTimeStamp Read GetColTimeStamp Write SetColTimeStamp;
    Property ColMemory[Col: SQLUSMALLINT]: TMemoryStream Read GetColMemory Write SetColMemory;
    Property ColVariant[Col: SQLUSMALLINT]: Variant Read GetColVariant Write SetColVariant;

    Property ColStringByName[ColName: String]: String Read GetColStringByName Write SetColStringByName;
    Property ColSingleByName[ColName: String]: Single Read GetColSingleByName Write SetColSingleByName;
    Property ColDoubleByName[ColName: String]: Double Read GetColDoubleByName Write SetColDoubleByName;
    Property ColBooleanByName[ColName: String]: Boolean Read GetColBooleanByName Write SetColBooleanByName;
    Property ColShortintByName[ColName: String]: ShortInt Read GetColShortintByName Write SetColShortintByName;
    Property ColByteByName[ColName: String]: Byte Read GetColByteByName Write SetColByteByName;
    Property ColSmallintByName[ColName: String]: SmallInt Read GetColSmallintByName Write SetColSmallintByName;
    Property ColWordByName[ColName: String]: Word Read GetColWordByName Write SetColWordByName;
    Property ColIntegerByName[ColName: String]: Integer Read GetColIntegerByName Write SetColIntegerByName;
    Property ColCardinalByName[ColName: String]: Cardinal Read GetColCardinalByName Write SetColCardinalByName;
    Property ColLongintByName[ColName: String]: LongInt Read GetColLongintByName Write SetColLongintByName;
    Property ColLongwordByName[ColName: String]: LongWord Read GetColLongwordByName Write SetColLongwordByName;
    Property ColInt64ByName[ColName: String]: Int64 Read GetColInt64ByName Write SetColInt64ByName;    
    Property ColDateByName[ColName: String]: TDate Read GetColDateByName Write SetColDateByName;
    Property ColTimeByName[ColName: String]: TTime Read GetColTimeByName Write SetColTimeByName;
    Property ColTimeStampByName[ColName: String]: DateSupport.TTimeStamp Read GetColTimeStampByName Write SetColTimeStampByName;
    Property ColMemoryByName[ColName: String]: TMemoryStream Read GetColMemoryByName Write SetColMemoryByName;
    Property ColVariantByName[ColName: String]: Variant Read GetColVariantByName Write SetColVariantByName;

    Property CellString[Col, Row: SQLUSMALLINT]: String Read GetCellString Write SetCellString;
    Property CellSingle[Col, Row: SQLUSMALLINT]: Single Read GetCellSingle Write SetCellSingle;
    Property CellDouble[Col, Row: SQLUSMALLINT]: Double Read GetCellDouble Write SetCellDouble;
    Property CellBoolean[Col, Row: SQLUSMALLINT]: Boolean Read GetCellBoolean Write SetCellBoolean;
    Property CellShortint[Col, Row: SQLUSMALLINT]: ShortInt Read GetCellShortint Write SetCellShortint;
    Property CellByte[Col, Row: SQLUSMALLINT]: Byte Read GetCellByte Write SetCellByte;
    Property CellSmallint[Col, Row: SQLUSMALLINT]: SmallInt Read GetCellSmallint Write SetCellSmallint;
    Property CellWord[Col, Row: SQLUSMALLINT]: Word Read GetCellWord Write SetCellWord;
    Property CellInteger[Col, Row: SQLUSMALLINT]: Integer Read GetCellInteger Write SetCellInteger;
    Property CellCardinal[Col, Row: SQLUSMALLINT]: Cardinal Read GetCellCardinal Write SetCellCardinal;
    Property CellLongint[Col, Row: SQLUSMALLINT]: LongInt Read GetCellLongint Write SetCellLongint;
    Property CellLongword[Col, Row: SQLUSMALLINT]: LongWord Read GetCellLongword Write SetCellLongword;
    Property CellInt64[Col, Row: SQLUSMALLINT]: Int64 Read GetCellInt64 Write SetCellInt64;
    Property CellDate[Col, Row: SQLUSMALLINT]: TDate Read GetCellDate Write SetCellDate;
    Property CellTime[Col, Row: SQLUSMALLINT]: TTime Read GetCellTime Write SetCellTime;
    Property CellTimeStamp[Col, Row: SQLUSMALLINT]: DateSupport.TTimeStamp Read GetCellTimeStamp Write SetCellTimeStamp;
    Property CellMemory[Col, Row: SQLUSMALLINT]: TMemoryStream Read GetCellMemory Write SetCellMemory;
    Property CellVariant[Col, Row: SQLUSMALLINT]: Variant Read GetCellVariant Write SetCellVariant;

    Property CellStringByName[ColName: String;
                              Row: SQLUSMALLINT]: String Read GetCellStringByName Write SetCellStringByName;
    Property CellSingleByName[ColName: String;
                              Row: SQLUSMALLINT]: Single Read GetCellSingleByName Write SetCellSingleByName;
    Property CellDoubleByName[ColName: String;
                              Row: SQLUSMALLINT]: Double Read GetCellDoubleByName Write SetCellDoubleByName;
    Property CellBooleanByName[ColName: String;
                               Row: SQLUSMALLINT]: Boolean Read GetCellBooleanByName Write SetCellBooleanByName;
    Property CellShortintByName[ColName: String;
                                Row: SQLUSMALLINT]: ShortInt Read GetCellShortintByName Write SetCellShortintByName;
    Property CellByteByName[ColName: String;
                            Row: SQLUSMALLINT]: Byte Read GetCellByteByName Write SetCellByteByName;
    Property CellSmallintByName[ColName: String;
                                Row: SQLUSMALLINT]: SmallInt Read GetCellSmallintByName Write SetCellSmallintByName;
    Property CellWordByName[ColName: String;
                            Row: SQLUSMALLINT]: Word Read GetCellWordByName Write SetCellWordByName;
    Property CellIntegerByName[ColName: String;
                               Row: SQLUSMALLINT]: Integer Read GetCellIntegerByName Write SetCellIntegerByName;
    Property CellCardinalByName[ColName: String;
                                Row: SQLUSMALLINT]: Cardinal Read GetCellCardinalByName Write SetCellCardinalByName;
    Property CellLongintByName[ColName: String;
                               Row: SQLUSMALLINT]: LongInt Read GetCellLongintByName Write SetCellLongintByName;
    Property CellLongwordByName[ColName: String;
                                Row: SQLUSMALLINT]: LongWord Read GetCellLongwordByName Write SetCellLongwordByName;
    Property CellInt64ByName[ColName: String;
                             Row: SQLUSMALLINT]: Int64 Read GetCellInt64ByName Write SetCellInt64ByName;
    Property CellDateByName[ColName: String;
                            Row: SQLUSMALLINT]: TDate Read GetCellDateByName Write SetCellDateByName;
    Property CellTimeByName[ColName: String;
                            Row: SQLUSMALLINT]: TTime Read GetCellTimeByName Write SetCellTimeByName;
    Property CellTimeStampByName[ColName: String;
                                 Row: SQLUSMALLINT]: DateSupport.TTimeStamp Read GetCellTimeStampByName Write SetCellTimeStampByName;
    Property CellMemoryByName[ColName: String;
                              Row: SQLUSMALLINT]: TMemoryStream Read GetCellMemoryByName Write SetCellMemoryByName;
    Property CellVariantByName[ColName: String;
                               Row: SQLUSMALLINT]: Variant Read GetCellVariantByName Write SetCellVariantByName;

    Procedure SetSpecialSQLStatementAttribute(AAttribute: SQLINTEGER; AValue: SQLPOINTER; AStringLength: SQLINTEGER);
  public
    { Published declarations }
    Property hDbc: THdbc Read FHdbc Write SetHdbc
      Default Nil;
    Property SQL: String Read FSQL Write SetSQL;
    Property SQLParsing: Boolean Read FSQLParsing Write FSQLParsing
      Default DefSQLParsing;
    Property ConcurrencyType: SQLUINTEGER Read FConcurrencyType Write SetConcurrencyType
      Default DefConcurrencyType;
    Property CursorType: SQLUINTEGER Read FCursorType Write SetCursorType
      Default DefCursorType;
    Property RowSetSize: SQLUINTEGER Read FRowSetSize Write SetRowSetSize
      Default DefRowSetSize;
    Property SkipByPosition: Boolean Read FSkipByPosition Write SetSkipByPosition
      Default DefSkipByMethod;
    Property SkipByCursor: Boolean Read FSkipByCursor Write SetSkipByCursor
      Default DefSkipByMethod;
    Property BindBookmarks: Boolean Read FBindBookmarks Write FBindBookmarks
      Default DefBindBookmarks;
    Property BlobSize: LongInt Read FBlobSize Write SetBlobSize
      Default DefBlobSize;
    Property BlobDeferral: Boolean Read FBlobDeferral Write FBlobDeferral
      Default DefBlobDeferral;
    Property BlobPlacement: TBlobPlacement Read FBlobPlacement Write FBlobPlacement
      Default DefBlobPlacement;
    Property ExecAsync: Boolean Read FExecAsync Write FExecAsync
      Default DefExecAsync;
    Property EmptyStringToNull: TEmptyToNull Read FEmptyToNull Write FEmptyToNull
      Default DefEmptyToNull;
    Property StringTrimming: TStringTrimming Read FStringTrimming Write FStringTrimming
      Default DefStringTrimming;
    Property BindByName: Boolean Read FBindByName Write FBindByName
      Default DefBindByName;
    Property RowCountMethod: TRowCountMethod Read FRowCountMethod Write FRowCountMethod
      Default DefRowCountMethod;
    Property NoRowsAffected: TNoRowsAffected Read FNoRowsAffected Write FNoRowsAffected
      Default DefNoRowsAffected;

    { Published Events }
    Property BeforePrepare: TNotifyEvent Read FBeforePrepare Write FBeforePrepare;
    Property AfterPrepare: TNotifyEvent Read FAfterPrepare Write FAfterPrepare;
    Property BeforeExecute: TNotifyEvent Read FBeforeExecute Write FBeforeExecute;
    Property AfterExecute: TNotifyEvent Read FAfterExecute Write FAfterExecute;
    Property BeforeFetch: TNotifyEvent Read FBeforeFetch Write FBeforeFetch;
    Property AfterFetch: TNotifyEvent Read FAfterFetch Write FAfterFetch;
    Property OnInsert: TConfirmEvent Read FOnInsert Write FOnInsert;
    Property OnUpdate: TConfirmEvent Read FOnUpdate Write FOnUpdate;
    Property OnDelete: TConfirmEvent Read FOnDelete Write FOnDelete;
    Property OnRefresh: TConfirmEvent Read FOnRefresh Write FOnRefresh;
    Property OnStatement: TStatementEvent Read FOnStatement Write FOnStatement;
    Property OnRowCount: TRowCountEvent Read FOnRowCount Write FOnRowCount;
  End;

{ Public Utilities }
Function PhysSize(CType: SQLSMALLINT): Word;
Function SqlTypeToColType(SqlType: SQLSMALLINT): SQLSMALLINT;
Function ColTypeToSqlType(ColType: SQLSMALLINT): SQLSMALLINT;
Function ToValue(SValue: String;
                 CValue: SQLPOINTER;
                 CType: SQLSMALLINT;
                 StringTrimming: TStringTrimming): Boolean;
Function ToString(CValue: SQLPOINTER;
                  CType: SQLSMALLINT;
                  StringTrimming: TStringTrimming): String;
Function ToDouble(CValue: SQLPOINTER;
                  CType: SQLSMALLINT): Double;
Function ToInteger(CValue: SQLPOINTER;
                   CType: SQLSMALLINT): Integer;
Function ToInt64(CValue: SQLPOINTER;
                 CType: SQLSMALLINT): Int64;
Function ToTimeStamp(CValue: SQLPOINTER;
                     CType: SQLSMALLINT): DateSupport.TTimeStamp;
function NullTS: TTimeStamp;

{ Private Utilities }
Function OffsetPointer(P: Pointer;
                       Ofs: LongInt): Pointer;
Function TrimString(S: String;
                    StringTrimming: TStringTrimming): String;

Implementation

{$IFNDEF VER130}
 Uses
   Variants;
{$ENDIF}


Const
  MinBlobSize = 1024;
  Fractional = 9;  //yyyy-mm-dd hh:mm:ss.fffffffff = 20+Fractional  

{ Public Utilities }

function DateAsTS(D: TDate): TTimeStamp;
begin
  Result := NullTS;
  with Result do
    begin
    Year := D.Year;
    Month := D.Month;
    Day := D.Day;
    end;
end;

function TSAsTime(TS: TTimeStamp): TTime;
begin
  with Result do
    begin
    Hour := TS.Hour;
    Minute := TS.Minute;
    Second := TS.Second;
    end;
end;

function TimeAsTS(T: TTime): TTimeStamp;
begin
  Result := NullTS;
  with Result do
    begin
    Hour := T.Hour;
    Minute := T.Minute;
    Second := T.Second;
    end;
end;


Function PhysSize(CType: SQLSMALLINT): Word;
Begin
  Case CType Of
    SQL_C_CHAR:
      Result:= SizeOf(NullString);

    SQL_C_BINARY:
      Result:= SizeOf(NullString);

    SQL_C_FLOAT:
      Result:= SizeOf(Single);
    SQL_C_DOUBLE:
      Result:= SizeOf(Double);

    SQL_C_BIT:
      Result:= SizeOf(Byte);
    SQL_C_STINYINT:
      Result:= SizeOf(ShortInt);
    SQL_C_UTINYINT:
      Result:= SizeOf(Byte);
    SQL_C_SSHORT:
      Result:= SizeOf(SmallInt);
    SQL_C_USHORT:
      Result:= SizeOf(Word);
    SQL_C_SLONG:
      Result:= SizeOf(NativeInt);
    SQL_C_ULONG:
      Result:= SizeOf(NativeUInt);
    SQL_C_SBIGINT,
    SQL_C_UBIGINT:
      Result:= SizeOf(Int64);

    SQL_C_TYPE_DATE:
      Result:= SizeOf(TDate);
    SQL_C_TYPE_TIME:
      Result:= SizeOf(TTime);
    SQL_C_TYPE_TIMESTAMP:
      Result:= SizeOf(TTimeStamp);
    Else
      Result:= 0;
  End;
End;

Function SqlTypeToColType(SqlType: SQLSMALLINT): SQLSMALLINT;
Begin
  Case SqlType Of
    SQL_CHAR,
    SQL_VARCHAR,
    SQL_LONGVARCHAR:
      Result:= SQL_C_CHAR;

    SQL_BINARY,
    SQL_VARBINARY,
    SQL_LONGVARBINARY:
      Result:= SQL_C_BINARY;

    SQL_REAL:
      Result:= SQL_C_FLOAT;
    SQL_DOUBLE,
    SQL_FLOAT,
    SQL_DECIMAL,
    SQL_NUMERIC:
      Result:= SQL_C_DOUBLE;

    SQL_BIT:
      Result:= SQL_C_BIT;
    SQL_TINYINT:
      Result:= SQL_C_STINYINT;
    SQL_SMALLINT:
      Result:= SQL_C_SSHORT;
    SQL_INTEGER:
      Result:= SQL_C_SLONG;
    SQL_BIGINT:
      Result:= SQL_C_SBIGINT;

    SQL_TYPE_DATE:
      Result:= SQL_C_TYPE_DATE;
    SQL_TYPE_TIME:
      Result:= SQL_C_TYPE_TIME;
    SQL_TYPE_TIMESTAMP:
      Result:= SQL_C_TYPE_TIMESTAMP;
    Else
      Result:= SQL_C_DEFAULT;
  End;
End;

Function ColTypeToSqlType(ColType: SQLSMALLINT): SQLSMALLINT;
Begin
  Case ColType Of
    SQL_C_CHAR:
      Result:= SQL_CHAR;

    SQL_C_WCHAR:
      Result:= SQL_WCHAR;

    SQL_C_BINARY:
      Result:= SQL_BINARY;

    SQL_C_FLOAT:
      Result:= SQL_REAL;
    SQL_C_DOUBLE:
      Result:= SQL_DOUBLE;

    SQL_C_BIT:
      Result:= SQL_BIT;
    SQL_C_STINYINT,
    SQL_C_UTINYINT:
      Result:= SQL_TINYINT;
    SQL_C_SSHORT,
    SQL_C_USHORT:
      Result:= SQL_SMALLINT;
    SQL_C_SLONG,
    SQL_C_ULONG:
      Result:= SQL_INTEGER;
    SQL_C_SBIGINT,
    SQL_C_UBIGINT:
      Result:= SQL_BIGINT;

    SQL_C_TYPE_DATE:
      Result:= SQL_TYPE_DATE;
    SQL_C_TYPE_TIME:
      Result:= SQL_TYPE_TIME;
    SQL_C_TYPE_TIMESTAMP:
      Result:= SQL_TYPE_TIMESTAMP;
    Else
      Result:= SQL_DEFAULT;
  End;
End;

Function ToValue(SValue: String;
                 CValue: SQLPOINTER;
                 CType: SQLSMALLINT;
                 StringTrimming: TStringTrimming): Boolean;
Var
  Code: Integer;
Begin
  SValue:= TrimString(SValue, StringTrimming);

  Case CType Of
    SQL_C_CHAR:
    Begin
      Move(PChar(SValue)^, CValue^, Length(SValue)+1);
      Result:= True;
    End;

    SQL_C_BINARY:
    Begin
      Move(PChar(SValue)^, CValue^, Length(SValue));
      Result:= True;
    End;

    SQL_C_FLOAT:
    Begin
      Val(SValue, Single(CValue^), Code);
      Result:= Code = 0;
    End;
    SQL_C_DOUBLE:
    Begin
      Val(SValue, Double(CValue^), Code);
      Result:= Code = 0;
    End;

    SQL_C_BIT:
    Begin
      Val(SValue, Byte(CValue^), Code);
      Result:= Code = 0;
    End;
    SQL_C_STINYINT:
    Begin
      Val(SValue, ShortInt(CValue^), Code);
      Result:= Code = 0;
    End;
    SQL_C_UTINYINT:
    Begin
      Val(SValue, Byte(CValue^), Code);
      Result:= Code = 0;
    End;
    SQL_C_SSHORT:
    Begin
      Val(SValue, SmallInt(CValue^), Code);
      Result:= Code = 0;
    End;
    SQL_C_USHORT:
    Begin
      Val(SValue, Word(CValue^), Code);
      Result:= Code = 0;
    End;
    SQL_C_SLONG:
    Begin
      Val(SValue, Integer(CValue^), Code);
      Result:= Code = 0;
    End;
    SQL_C_ULONG:
    Begin
      Val(SValue, Cardinal(CValue^), Code);
      Result:= Code = 0;
    End;
    SQL_C_SBIGINT,
    SQL_C_UBIGINT:
    Begin
      Val(SValue, Int64(CValue^), Code);
      Result:= Code = 0;
    End;

    SQL_C_TYPE_DATE:
    Begin
      Result:= False;
      If Length(SValue) >= 10 Then
        With TDate(CValue^) Do
        Begin
          Val(SValue[1]+SValue[2]+SValue[3]+SValue[4], Year, Code);
          If Code = 0 Then
          Begin
            Val(SValue[6]+SValue[7], Month, Code);
            If Code = 0 Then
            Begin
              Val(SValue[9]+SValue[10], Day, Code);
              Result:= Code = 0;
            End;
          End;
        End;
    End;
    SQL_C_TYPE_TIME:
    Begin
      Result:= False;
      If Length(SValue) >= 8 Then
        With TTime(CValue^) Do
        Begin
          Val(SValue[1]+SValue[2], Hour, Code);
          If Code = 0 Then
          Begin
            Val(SValue[4]+SValue[5], Minute, Code);
            If Code = 0 Then
            Begin
              Val(SValue[7]+SValue[8], Second, Code);
              Result:= Code = 0;
            End;
          End;
        End;
    End;
    SQL_C_TYPE_TIMESTAMP:
    Begin
      Result:= False;
      If Length(SValue) >= 21 Then
        With DateSupport.TTimeStamp(CValue^) Do
        Begin
          Val(SValue[1]+SValue[2]+SValue[3]+SValue[4], Year, Code);
          If Code = 0 Then
          Begin
            Val(SValue[6]+SValue[7], Month, Code);
            If Code = 0 Then
            Begin
              Val(SValue[9]+SValue[10], Day, Code);
              If Code = 0 Then
              Begin
                Val(SValue[12]+SValue[13], Hour, Code);
                If Code = 0 Then
                Begin
                  Val(SValue[15]+SValue[16], Minute, Code);
                  If Code = 0 Then
                  Begin
                    Val(SValue[18]+SValue[19], Second, Code);
                    If Code = 0 Then
                    Begin
                      SValue:= Copy(SValue, 21, Length(SValue)-20);
                      Val(SValue, Fraction, Code);
                      Result:= Code = 0;
                    End;
                  End;
                End;
              End;
            End;
          End;
        End;
    End;
    Else
      Result:= False;
  End;
End;

{$WARN SYMBOL_DEPRECATED OFF}
{$WARN IMPLICIT_STRING_CAST OFF}
Function ToString(CValue: SQLPOINTER;
                  CType: SQLSMALLINT;
                  StringTrimming: TStringTrimming): String;
Begin
  Case CType Of
    SQL_C_CHAR:
      Result:= StrPas(PAnsiChar(CValue));
    SQL_C_WCHAR:
      Result:= StrPas(PWideChar(CValue));

    SQL_C_BINARY:
      Result:= '';

    SQL_C_FLOAT:
      Str(Single(CValue^), Result);
    SQL_C_DOUBLE:
      Str(Double(CValue^), Result);

    SQL_C_BIT:
      Str(Byte(CValue^), Result);
    SQL_C_STINYINT:
      Str(ShortInt(CValue^), Result);
    SQL_C_UTINYINT:
      Str(Byte(CValue^), Result);
    SQL_C_SSHORT:
      Str(SmallInt(CValue^), Result);
    SQL_C_USHORT:
      Str(Word(CValue^), Result);
    SQL_C_SLONG:
      Str(Integer(CValue^), Result);
    SQL_C_ULONG:
      Str(Cardinal(CValue^), Result);
    SQL_C_SBIGINT,
    SQL_C_UBIGINT:
      Str(Int64(CValue^), Result);

    SQL_C_TYPE_DATE:
      With TDate(CValue^) Do
        Result:= LeadingZeros(Year, 1)+'-'+LeadingZeros(Month, 1)+'-'+LeadingZeros(Day, 1);
    SQL_C_TYPE_TIME:
      With TTime(CValue^) Do
        Result:= LeadingZeros(Hour, 1)+':'+LeadingZeros(Minute, 1)+':'+LeadingZeros(Second, 1);
    SQL_C_TYPE_TIMESTAMP:
      With DateSupport.TTimeStamp(CValue^) Do
        Result:= LeadingZeros(Year, 1)+'-'+LeadingZeros(Month, 1)+'-'+LeadingZeros(Day, 1)+' '+
          LeadingZeros(Hour, 1)+':'+LeadingZeros(Minute, 1)+':'+LeadingZeros(Second, 1)+'.'+
          LeadingZeros(Fraction, Fractional-1);
    Else
      Result:= '';
  End;

  Result:= TrimString(Result, StringTrimming);
End;

Function ToDouble(CValue: SQLPOINTER;
                  CType: SQLSMALLINT): Double;
Var
  Code: Integer;
Begin
  Case CType Of
    SQL_C_CHAR:
    Begin
      Val(ToString(CValue, CType, stTrimBoth), Result, Code);
      If Code <> 0 Then
        Result:= 0;
    End;

    SQL_C_BINARY:
      Result:= 0;

    SQL_C_FLOAT:
      Result:= Single(CValue^);
    SQL_C_DOUBLE:
      Result:= Double(CValue^);

    SQL_C_BIT:
      Result:= Byte(CValue^);
    SQL_C_STINYINT:
      Result:= ShortInt(CValue^);
    SQL_C_UTINYINT:
      Result:= Byte(CValue^);
    SQL_C_SSHORT:
      Result:= SmallInt(CValue^);
    SQL_C_USHORT:
      Result:= Word(CValue^);
    SQL_C_SLONG:
      Result:= Integer(CValue^);
    SQL_C_ULONG:
      Result:= Cardinal(CValue^);
    SQL_C_SBIGINT,
    SQL_C_UBIGINT:
      Result:= Int64(CValue^);

    SQL_C_TYPE_DATE,
    SQL_C_TYPE_TIME,
    SQL_C_TYPE_TIMESTAMP:
      Result:= 0;
    Else
      Result:= 0;
  End;
End;

Function ToInteger(CValue: SQLPOINTER;
                   CType: SQLSMALLINT): Integer;
Var
  Code: Integer;
Begin
  Case CType Of
    SQL_C_CHAR:
    Begin
      Val(ToString(CValue, CType, stTrimBoth), Result, Code);
      If Code <> 0 Then
        Result:= 0;
    End;

    SQL_C_BINARY:
      Result:= 0;

    SQL_C_FLOAT:
      Result:= Round(Single(CValue^));
    SQL_C_DOUBLE:
      Result:= Round(Double(CValue^));

    SQL_C_BIT:
      Result:= Byte(CValue^);
    SQL_C_STINYINT:
      Result:= ShortInt(CValue^);
    SQL_C_UTINYINT:
      Result:= Byte(CValue^);
    SQL_C_SSHORT:
      Result:= SmallInt(CValue^);
    SQL_C_USHORT:
      Result:= Word(CValue^);
    SQL_C_SLONG:
      Result:= Integer(CValue^);
    SQL_C_ULONG:
      Result:= Cardinal(CValue^);
    SQL_C_SBIGINT,
    SQL_C_UBIGINT:
      Result:= Int64(CValue^);

    SQL_C_TYPE_DATE,
    SQL_C_TYPE_TIME,
    SQL_C_TYPE_TIMESTAMP:
      Result:= 0;
    Else
      Result:= 0;
  End;
End;

Function ToInt64(CValue: SQLPOINTER;
                 CType: SQLSMALLINT): Int64;
Begin
  Case CType Of
    SQL_C_SBIGINT,
    SQL_C_UBIGINT:
      Result:= Int64(CValue^);
    Else
      Result:= ToInteger(CValue, CType);
  End;
End;

Function ToTimeStamp(CValue: SQLPOINTER;
                     CType: SQLSMALLINT): DateSupport.TTimeStamp;
Begin
  Result:= NullTS;
  Case CType Of
    SQL_C_TYPE_DATE:
    Begin
      Result.Year:= TDate(CValue^).Year;
      Result.Month:= TDate(CValue^).Month;
      Result.Day:= TDate(CValue^).Day;
    End;
    SQL_C_TYPE_TIME:
    Begin
      Result.Hour:= TTime(CValue^).Hour;
      Result.Minute:= TTime(CValue^).Minute;
      Result.Second:= TTime(CValue^).Second;
    End;
    SQL_C_TYPE_TIMESTAMP:
    Begin
      Result:= DateSupport.TTimeStamp(CValue^);
    End;
  End;
End;

{ Private Utilities }

Function OffsetPointer(P: Pointer;
                       Ofs: LongInt): Pointer;
Begin
  Result:= Pointer(NativeUInt(P)+Ofs);
End;

Function OffsetRow(P: Pointer;
                   Row: SQLUSMALLINT;
                   Size: Word): Pointer;
Begin
  Result:= OffsetPointer(P, (Row-1)*Size);
End;

Function TrimString(S: String;
                    StringTrimming: TStringTrimming): String;
Begin
  Case StringTrimming Of
    stTrimNone:
      Result:= S;
    stTrimTrailing:
      Result:= TrimRight(S);
    stTrimLeading:
      Result:= TrimLeft(S);
    stTrimBoth:
      Result:= Trim(S);
  End;
End;

{ EODBC }

Constructor EODBC.Create(AOwner: TODBCObject;
                         ARetCode: SQLRETURN;
                         AErrors: TList);
Begin
  FOwner:= AOwner;
  FRetCode:= ARetCode;
  FCursor:= 0;
  FErrors:= AErrors;

  Inherited Create(Message);
End;

Destructor EODBC.Destroy;
Var
  i: Integer;
Begin
  For i:= 0 To FErrors.Count-1 Do
    Dispose(TErrorPtr(FErrors[i]));
  FErrors.Free;

  Inherited Destroy;
End;


Procedure EODBC.First;
Begin
  FCursor:= 0;
End;

Procedure EODBC.Last;
Begin
  FCursor:= FErrors.Count-1;
End;

Function EODBC.Next: Boolean;
Begin
  Result:= False;
  If FCursor < FErrors.Count Then
  Begin
    Inc(FCursor);
    Result:= FCursor < FErrors.Count;
  End;
End;

Function EODBC.Prev: Boolean;
Begin
  Result:= False;
  If FCursor > -1 Then
  Begin
    Dec(FCursor);
    Result:= FCursor > -1;
  End;
End;

Function EODBC.GetState: String;
Begin
  Result:= TErrorPtr(FErrors[FCursor]).FState;
End;

Function EODBC.GetNative: SQLINTEGER;
Begin
  Result:= TErrorPtr(FErrors[FCursor]).FNative;
End;

Function EODBC.GetMessage: String;
Begin
  Result:= TErrorPtr(FErrors[FCursor]).FMessage;
End;

Procedure EODBC.SetMessage(AMessage: String);
Begin
  Inherited Message:= AMessage;
End;


{ TODBCErrorHandler }

Function TODBCErrorHandler.Errors(ARetCode: SQLRETURN; aHandleType: SQLSMALLINT; aHandle: SQLHANDLE): TList;
Var
  ErrorNum: Integer;
  ErrorPtr: TErrorPtr;

  RetCode: SQLRETURN;
  State: NullString;
  Native: SQLINTEGER;
  Message: NullString;
  StringLength: SQLSMALLINT;
Begin
  Result:= TList.Create;
  Result.Clear;

  Case ARetCode Of
    SQL_ERROR,
    SQL_SUCCESS_WITH_INFO:
    Begin
      ErrorNum:= 0;
      Repeat
        Inc(ErrorNum);

        //depreciated RetCode:= _SQLError(FErrHenv, FErrHdbc, FErrHstmt, SqlState, NativeError, ErrorMsg);
        RetCode:= SQLGetDiagRec(aHandleType, aHandle, ErrorNum, @State, @Native, @Message, SizeOf(Message), @StringLength);

        If Success(RetCode) Then
        Begin
          New(ErrorPtr);
          ErrorPtr.FState:= State;
          ErrorPtr.FNative:= Native;
          ErrorPtr.FMessage:= Message;
          Result.Add(ErrorPtr);
        End;
      Until Not Success(RetCode);
      If (Result.Count = 0) Or (RetCode <> SQL_NO_DATA) Then
      Begin
        New(ErrorPtr);
        ErrorPtr.FState:= '';
        ErrorPtr.FNative:= 0;
        ErrorPtr.FMessage:= 'Unable to Retrieve ODBC Error';
        Result.Add(ErrorPtr);
      End;
    End;
    Else
    Begin
      New(ErrorPtr);
      ErrorPtr.FState:= '';
      ErrorPtr.FNative:= 0;
      Case ARetCode Of
        SQL_INVALID_HANDLE:
          ErrorPtr.FMessage:= 'Invalid ODBC Handle';
        SQL_NO_DATA:
          ErrorPtr.FMessage:= 'No Data Found';
        Else
          ErrorPtr.FMessage:= 'ODBC Return Code '+IntToStr(ARetCode);
      End;
      Result.Add(ErrorPtr);
    End;
  End;
End;

Procedure TODBCErrorHandler.RaiseError(AOwner: TODBCObject; ARetCode: SQLRETURN);
Var
  aHandleType: SQLSMALLINT;
  aHandle: SQLHANDLE;
Begin
  If AOwner Is THenv Then
  Begin
    aHandleType := SQL_HANDLE_ENV;
    aHandle:= THenv(AOwner).Handle
  End
  Else If AOwner Is THdbc Then
  Begin
    aHandleType:= SQL_HANDLE_DBC;
    aHandle:= THdbc(AOwner).Handle
    End
  Else // If AOwner Is THstmt Then
    Begin
    aHandleType:= SQL_HANDLE_STMT;
    aHandle:= THstmt(AOwner).Handle;
  end;

  raise EODBC.Create(AOwner, ARetCode, Errors(ARetCode, aHandleType, aHandle));
End;


Function TODBCErrorHandler.Success(RetCode: SQLRETURN): Boolean;
Begin
    Result:= (RetCode = SQL_SUCCESS) Or (RetCode = SQL_SUCCESS_WITH_INFO);
End;

Function TODBCErrorHandler.SuccessOnly(RetCode: SQLRETURN): Boolean;
Begin
  Result:= RetCode = SQL_SUCCESS;
End;


{ THenv }

Procedure THenv.SetConnectionPooling(AConnectionPooling: TConnectionPooling);
Begin
  If AConnectionPooling <> FConnectionPooling Then
  Begin
    TerminateHandle;
    FConnectionPooling:= AConnectionPooling;
    Init;
  End;
End;

Constructor THenv.Create;
Begin
  Inherited Create;

  { Create Error Object }
  FError := TODBCErrorHandler.Create;

  FActive:= False;
  FConnectionPooling:= DefConnectionPooling;

  Init;
End;

Destructor THenv.Destroy;
Begin
  Inherited Destroy;

  { Terminate Self }
  TerminateHandle;

  FError.Free;
End;

Function THenv.Init: Boolean;
Var
  LConnectionPooling, AConnectionPooling: SQLUINTEGER;
Begin
  Log(1, 'THenv.Init');

  If FActive Then
  Begin
    Init:= FActive;
    Exit;
  End;

  If FConnectionPooling <> cpDefault Then
  Begin
    { Set Connection Pooling }
    AConnectionPooling:= SQL_CP_OFF;
    Case FConnectionPooling Of
      cpOff:
        AConnectionPooling:= SQL_CP_OFF;
      cpOnePerDriver:
        AConnectionPooling:= SQL_CP_ONE_PER_DRIVER;
      cpOnePerEnv:
        AConnectionPooling:= SQL_CP_ONE_PER_HENV;
    End;

    FRetCode:= SQLGetEnvAttr(Pointer(SQL_NULL_HANDLE), SQL_ATTR_CONNECTION_POOLING, @LConnectionPooling, 0, Nil);
    If FError.Success(FRetCode) And (LConnectionPooling <> AConnectionPooling) Then
    Begin
      FRetCode:= SQLSetEnvAttr(Pointer(SQL_NULL_HANDLE), SQL_ATTR_CONNECTION_POOLING, Pointer(AConnectionPooling), 0);
      If Not FError.Success(FRetCode) Then
        FError.RaiseError(Self, FRetCode);
    End;
End;

  { Create Handle }
  FRetCode:= SQLAllocHandle(SQL_HANDLE_ENV, Pointer(SQL_NULL_HANDLE), @FHenv);
  If Not FError.Success(FRetCode) Then
Begin
    FHenv := Nil;
    FError.RaiseError(Self, FRetCode);
  End;

  { Set ODBC Version }
  FRetCode:= SQLSetEnvAttr(FHenv, SQL_ATTR_ODBC_VERSION, Pointer(SQL_OV_ODBC3), 0);
  If Not FError.Success(FRetCode) Then
    FError.RaiseError(Self, FRetCode);

  { Set Active Field }
  FActive:= True;

  Result:= FActive;
End;

Function THenv.TerminateHandle: Boolean;
Begin
  { Terminate Self }
  If FActive Then
  Begin
    { Free Handle }
    FRetCode:= SQLFreeHandle(SQL_HANDLE_ENV, FHenv);
    If Not FError.Success(FRetCode) Then
      FError.RaiseError(Self, FRetCode);
    FHenv := Nil;

    { Set Active Field }
    FActive:= False;
  End;

  Result:= Not FActive;
End;

{ THdbc }

Function THdbc.GetCore: Boolean;
Begin
  Connect;

  Result:= FCore;
End;

Procedure THdbc.SetCore(ACore: Boolean);
Begin
  Connect;

  FCore:= ACore;
End;

Function THdbc.GetLoginTimeOut: SQLUINTEGER;
Begin
  Init;

  FRetCode:= SQLGetConnectAttr(FHdbc, SQL_ATTR_LOGIN_TIMEOUT, @Result, SizeOf(Result), Nil);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure THdbc.SetSpecialSQLConnectionAttribute(AAttribute: SQLINTEGER; AValue: SQLPOINTER; AStringLength: SQLINTEGER);
Begin
  Init;

  FRetCode := SQLSetConnectAttr(FHdbc, AAttribute, AValue, AStringLength);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure THdbc.SetLoginTimeOut(ALoginTimeOut: SQLUINTEGER);
Begin
  Init;

  FRetCode:= SQLSetConnectAttr(FHdbc, SQL_ATTR_LOGIN_TIMEOUT, Pointer(ALoginTimeOut), SizeOf(ALoginTimeOut));
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure THdbc.SetCursorLib(ACursorLib: SQLUINTEGER);
Var
  LCursorLib: SQLULEN;
Begin
  Log(1, 'THdbc.SetCursorLib');

  If FConnected Then
    Disconnect;

  { Set Cursor Library }
  FCursorLib:= ACursorLib;

  If FActive Then
  Begin
    FRetCode:= SQLGetConnectAttr(FHdbc, SQL_ATTR_ODBC_CURSORS, @LCursorLib, SizeOf(LCursorLib), Nil);
    If FEnv.Error.Success(FRetCode) And (LCursorLib <> FCursorLib) Then
    Begin
      FRetCode:= SQLSetConnectAttr(FHdbc, SQL_ATTR_ODBC_CURSORS, Pointer(FCursorLib), SizeOf(FCursorLib));
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
    End;
  End;
End;

Procedure THdbc.SetTracing(ATracing: Boolean);
Var
  TraceFile: String;
Begin
  Init;

  If ATracing <> FTracing Then
  Begin
    If ATracing Then
    Begin
      FRetCode:= SQLSetConnectAttr(FHdbc, SQL_ATTR_TRACE, Pointer(SQL_OPT_TRACE_ON), SizeOf(SQLUINTEGER));
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);

      TraceFile:= 'TRACE.OE';
      FRetCode:= SQLSetConnectAttr(FHdbc, SQL_ATTR_TRACEFILE, Pointer(PChar(TraceFile)), SQL_NTS);
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
    End
    Else
    Begin
      FRetCode:= SQLSetConnectAttr(FHdbc, SQL_ATTR_TRACE, Pointer(SQL_OPT_TRACE_OFF), SizeOf(SQLUINTEGER));
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
    End;
    FTracing:= ATracing;
  End;
End;

Function THdbc.GetInTransaction: Boolean;
Var
  LInTransaction: SQLUINTEGER;
Begin
  FRetCode:= SQLGetConnectAttr(FHdbc, SQL_ATTR_AUTOCOMMIT, @LInTransaction, SizeOf(LInTransaction), Nil);
  If Not FEnv.Error.Success(FRetCode) Then
    Result:= False
  Else
    Result:= LInTransaction = SQL_AUTOCOMMIT_OFF;
End;

Function THdbc.GetConnectionPooling: TConnectionPooling;
Begin
  Result:= FEnv.ConnectionPooling;
End;

Procedure THdbc.SetConnectionPooling(AConnectionPooling: TConnectionPooling);
Begin
    FConnectionPooling:= AConnectionPooling;
End;

Procedure THdbc.SetDriver(ADriver: String);
Begin

  FDriver:= ADriver;
End;

Procedure THdbc.SetDataSource(ADataSource: String);
Begin

  FDataSource:= ADataSource;
End;

Procedure THdbc.SetUserName(AUserName: String);
Begin

  FUserName:= AUserName;
End;

Procedure THdbc.SetPassword(APassword: String);
Begin

  FPassword:= APassword;
End;

Procedure THdbc.SetAttributes(AAttributes: TStrings);
Begin

  FAttributes.Assign(AAttributes);
End;

Procedure THdbc.SetIsolationLevel(AIsolationLevel: SQLUINTEGER);
Var
  LIsolationLevel: SQLUINTEGER;
Begin
  Log(1, 'THdbc.SetIsolationLevel');

  { Set Isolation Level }
  FIsolationLevel:= AIsolationLevel;

  If FActive Then
  Begin
    FRetCode:= SQLGetConnectAttr(FHdbc, SQL_ATTR_TXN_ISOLATION, @LIsolationLevel, SizeOf(LIsolationLevel), Nil);
    If FEnv.Error.Success(FRetCode) And (LIsolationLevel <> FIsolationLevel) Then
    Begin
      FRetCode:= SQLSetConnectAttr(FHdbc, SQL_ATTR_TXN_ISOLATION, Pointer(FIsolationLevel), 0);
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
End;
End;
End;


Procedure THdbc.DoBeforeConnect;
Begin
  If Assigned(FBeforeConnect) Then
    FBeforeConnect(Self);
End;

Procedure THdbc.DoAfterConnect;
Begin
  If Assigned(FAfterConnect) Then
    FAfterConnect(Self);
End;

Procedure THdbc.DoBeforeDisconnect;
Begin
  If Assigned(FBeforeDisconnect) Then
    FBeforeDisconnect(Self);
End;

Procedure THdbc.DoAfterDisconnect;
Begin
  If Assigned(FAfterDisconnect) Then
    FAfterDisconnect(Self);
End;

Constructor THdbc.Create(Env : THEnv);
Begin
  Inherited Create(Env);


  { Set Defaults }
  FActive:= False;
  FConnectionPooling:= DefConnectionPooling;
  FConnected:= DefConnected;
  FStreamedConnected:= DefConnected;
  FDriver:= '';
  FDataSource:= '';
  FUserName:= '';
  FPassword:= '';
  ForceEmptyPasswordsIntoConnectionString := True;
  FAttributes:= TStringList.Create;
  FIsolationLevel:= DefIsolationLevel;
  FInfoPrompt:= DefInfoPrompt;
  FCursorLib:= DefCursorLib;
  FTracing:= DefTracing;
  FCore:= DefCore;
  FDrivers:= TList.Create;
  RefreshDrivers;

  Init;
End;

Destructor THdbc.Destroy;
Begin
  Inherited Destroy;

  { Terminate Self }
  FAttributes.Free;
  ClearDrivers;
  FDrivers.Free;
  TerminateHandle;
End;

Function THdbc.Init: Boolean;
Begin
  Log(1, 'THdbc.Init');

  If FActive Then
  Begin
    Init:= FActive;
    Exit;
  End;

  { Create Handle }
  FRetCode:= SQLAllocHandle(SQL_HANDLE_DBC, FEnv.Handle, @FHdbc);
  If Not FEnv.Error.Success(FRetCode) Then
  Begin
    FHdbc := Nil;
    FEnv.Error.RaiseError(Self, FRetCode);
  End;

  { Set Active Field }
  FActive:= True;

  { Set ODBC Properties }
  CursorLib:= FCursorLib;

  Result:= FActive;

End;

Function THdbc.GetHandle: SQLHDBC;
Begin
  Init;

  { Retrieve Handle }
  Result:= FHdbc;
End;

Function THdbc.GetConnected: Boolean;
Begin
  Result:= FConnected;
End;

Procedure THdbc.SetConnected(AConnected: Boolean);
Begin
    If AConnected Then
      Connect
    Else
      Disconnect;
  End;

Procedure THdbc.Connect;
Var
  ConnectStrIn: String;
  ConnectStrOut: NullString;
  StringLength: SQLSMALLINT;

  Function ConnectStr: String;
  Var
    i: Integer;
  Begin
    Result:= '';
    If FDataSource <> '' Then
      Result:= Result+'DSN='+FDataSource+';';
    If FUserName <> '' Then
      Result:= Result+'UID='+FUserName+';';
    If ForceEmptyPasswordsIntoConnectionString Or (FPassword <> '')  Then
      Result:= Result+'PWD='+FPassword+';';
    If FDriver <> '' Then
      Result:= Result+'DRIVER='+FDriver+';';
    For i:= 0 To FAttributes.Count-1 Do
      Result:= Result+FAttributes[i]+';';
  End;

Begin
  Log(1, 'THdbc.Connect');

  Init;

  If Not FConnected Then
  Begin
    DoBeforeConnect;

    { Set ODBC Properties }
    CursorLib:= FCursorLib;

    { Establish Connection }
    If (FInfoPrompt = SQL_DRIVER_NOPROMPT) And (FDriver = '') And (FAttributes.Count = 0) Then
      FRetCode:= SQLConnect(FHdbc, Pointer(PChar(FDataSource)), SQL_NTS,
                                   Pointer(PChar(FUserName)), SQL_NTS,
                                   Pointer(PChar(FPassword)), SQL_NTS)
    Else
    Begin
      ConnectStrIn:= ConnectStr;
      FRetCode:= SQLDriverConnect(FHdbc, 0, Pointer(PChar(ConnectStrIn)), SQL_NTS,
                                                               @ConnectStrOut, SizeOf(ConnectStrOut), @StringLength,
                                                               FInfoPrompt);
    End;
    If Not FEnv.Error.Success(FRetCode) Then
      FEnv.Error.RaiseError(Self, FRetCode);

    { Set Connected Field }
    FConnected:= True;

    { Set ODBC Properties }
    IsolationLevel:= FIsolationLevel;

    { Set Core Level }
    If FConnected Then
      FCore:= Not GetFunction(SQL_API_SQLDESCRIBEPARAM);

    If FConnected Then
      DoAfterConnect;
    FPlatform := RecogniseDriver(Driver);
  End;
End;

Procedure THdbc.Disconnect;
Begin
  Log(1, 'THdbc.Disconnect');

  If FConnected Then
  Begin
    DoBeforeDisconnect;

    { Remove Connection }
    FRetCode:= SQLDisconnect(FHdbc);
    If Not FEnv.Error.Success(FRetCode) Then
      FEnv.Error.RaiseError(Self, FRetCode);

    { Set Connected Field }
    FConnected:= False;

    If Not FConnected Then
      DoAfterDisconnect;
  End;
End;

Procedure THdbc.StartTransact;
Begin
  Connect;

  FRetCode:= SQLSetConnectAttr(FHdbc, SQL_ATTR_AUTOCOMMIT, Pointer(SQL_AUTOCOMMIT_OFF), SizeOf(SQLUINTEGER));
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

procedure THdbc.TerminateHandle;
begin
  FRetCode:= SQLFreeHandle(SQL_HANDLE_DBC, FHdbc);
  If Not FEnv.FError.Success(FRetCode) Then
    FEnv.FError.RaiseError(Self, FRetCode);
  FHdbc := Nil;
end;

Procedure THdbc.EndTransact;
Begin
  Commit;

  FRetCode:= SQLSetConnectAttr(FHdbc, SQL_ATTR_AUTOCOMMIT, Pointer(SQL_AUTOCOMMIT_ON), SizeOf(SQLUINTEGER));
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure THdbc.Commit;
Begin
  Connect;

  FRetCode:= SQLEndTran(SQL_HANDLE_DBC, FHdbc, SQL_COMMIT);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure THdbc.Rollback;
Begin
  Connect;

  FRetCode:= SQLEndTran(SQL_HANDLE_DBC, FHdbc, SQL_ROLLBACK);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Function THdbc.GetFunction(FunctionID: SQLUSMALLINT): Boolean;
Var
  Supported: SQLUSMALLINT;
Begin
  If (FunctionID = SQL_API_ALL_FUNCTIONS) Or
     (FunctionID = SQL_API_ODBC3_ALL_FUNCTIONS) Then
    Raise EODBCExpress.Create('Cannot return information for more than one function at a time.');

  Connect;

  FRetCode:= SQLGetFunctions(FHdbc, FunctionID, @Supported);
  If Not FEnv.Error.Success(FRetCode) Then
    Supported:= 0;

  Result:= Supported <> 0;
End;

Function THdbc.GetInfoString(InfoType: SQLUSMALLINT): String;
Var
  Supported: NullString;
  StringLength: SQLSMALLINT;
Begin
  Connect;

  FRetCode:= SQLGetInfo(FHdbc, InfoType, @Supported, SizeOf(Supported), @StringLength);
  If Not FEnv.Error.Success(FRetCode) Then
    Result:= ''
  Else
    Result:= StrPas(Supported);
End;

Function THdbc.GetInfoSmallint(InfoType: SQLUSMALLINT): SQLUSMALLINT;
Var
  Supported: SQLUSMALLINT;
Begin
  Connect;

  FRetCode:= SQLGetInfo(FHdbc, InfoType, @Supported, SizeOf(Supported), Nil);
  If Not FEnv.Error.Success(FRetCode) Then
    Result:= 0
  Else
    Result:= Supported;
End;

Function THdbc.GetInfoInteger(InfoType: SQLUSMALLINT): SQLUINTEGER;
Var
  Supported: SQLUINTEGER;
Begin
  Connect;

  FRetCode:= SQLGetInfo(FHdbc, InfoType, @Supported, SizeOf(Supported), Nil);
  If Not FEnv.Error.Success(FRetCode) Then
    Result:= 0
  Else
    Result:= Supported;
End;

Function THdbc.GetVersion: String;
Begin
  Result:= OEVER;
End;

Procedure THdbc.SetVersion(AVersion: String);
Begin
End;

Procedure THdbc.RefreshDrivers;
Begin
  ClearDrivers;

  AddDriver('Default');

  With AddDriver('Microsoft SQL Server;sqlsrv32.dll')^ Do
  Begin
    //not core
    PS_SQL_TYPE_TIMESTAMP:= 23;
    DD_SQL_TYPE_TIMESTAMP:= 3;
  End;

  With AddDriver('Microsoft SQL Server;sqlncli.dll')^ Do
  Begin
    //not core
    PS_SQL_TYPE_TIMESTAMP:= 23;
    DD_SQL_TYPE_TIMESTAMP:= 3;
  End;

  With AddDriver('Microsoft SQL Server;sqlncli10.dll')^ Do
  Begin
    //not core
    PS_SQL_TYPE_TIMESTAMP:= 23;
    DD_SQL_TYPE_TIMESTAMP:= 3;
  End;

  With AddDriver('Oracle;msorcl32.dll')^ Do
  Begin
    //not core
    PS_SQL_VARCHAR:= 2000;
  End;

  With AddDriver('Oracle8;sqoci32.dll')^ Do
  Begin
    //core
    PS_SQL_VARCHAR:= 2000;
  End;

  With AddDriver('SQL Server;sysybnt.dll')^ Do
  Begin
    //core
    PS_SQL_VARCHAR:= 30;
    PS_SQL_VARBINARY:= 8;
    PS_SQL_TYPE_TIMESTAMP:= 23;
    DD_SQL_TYPE_TIMESTAMP:= 3;
  End;

  With AddDriver('Adaptive Server Anywhere;dbodbc6w.dll')^ Do
  Begin
    //not core
    PS_SQL_CHAR:= 32767;
    PS_SQL_VARCHAR:= 32767;
    PS_SQL_BINARY:= 32767;
    PS_SQL_VARBINARY:= 32767;
    PS_SQL_TYPE_TIMESTAMP:= 26;
    DD_SQL_TYPE_TIMESTAMP:= 6;
  End;

  With AddDriver('Informix;iclit09a.dll')^ Do
  Begin
    //core
    PS_SQL_CHAR:= 32767;
    PS_SQL_TYPE_TIMESTAMP:= 25;
    DD_SQL_TYPE_TIMESTAMP:= 5;
  End;

  With AddDriver('InterBase;iscdrv32.dll')^ Do
  Begin
    //core
    PS_SQL_CHAR:= 32767;
    PS_SQL_VARCHAR:= 32765;
  End;

  With AddDriver('Access;odbcjt32.dll')^ Do
  Begin
    //core
    PS_SQL_LONGVARBINARY:= MaxLongint Div 2;
  End;

  With AddDriver('dBase;odbcjt32.dll')^ Do
  Begin
    //core
    PS_SQL_CHAR:= 254;
    PS_SQL_VARCHAR:= 254;
    PS_SQL_LONGVARCHAR:= MaxLongint Div 2;
    PS_SQL_BINARY:= 254;
    PS_SQL_VARBINARY:= 254;
    PS_SQL_LONGVARBINARY:= MaxLongint Div 2;
  End;

  With AddDriver('Paradox;odbcjt32.dll')^ Do
  Begin
    //core
    PS_SQL_LONGVARCHAR:= MaxLongint Div 2;
    PS_SQL_BINARY:= 254;
    PS_SQL_VARBINARY:= 254;
    PS_SQL_LONGVARBINARY:= MaxLongint Div 2;
  End;

  With AddDriver('Visual FoxPro;vfpodbc.dll')^ Do
  Begin
    //core
    PS_SQL_CHAR:= 254;
    PS_SQL_VARCHAR:= 254;
    PS_SQL_BINARY:= 254;
    PS_SQL_VARBINARY:= 254;
  End;

  With AddDriver('FoxPro;odbcjt32.dll')^ Do
  Begin
    //core
    PS_SQL_CHAR:= 254;
    PS_SQL_VARCHAR:= 254;
    PS_SQL_BINARY:= 254;
    PS_SQL_VARBINARY:= 254;
  End;
End;

Procedure THdbc.ClearDrivers;
Var
  i: Integer;
Begin
  For i:= 0 To FDrivers.Count-1 Do
    Dispose(TDriverPtr(FDrivers[i]));
  FDrivers.Clear;
End;

Function THdbc.AddDriver(ADriver: String): TDriverPtr;
Begin
  New(Result);
  FDrivers.Add(Result);

  With Result^ Do
  Begin
    Desc:= ADriver;
    PS_SQL_CHAR:= MaxNullString;
    PS_SQL_VARCHAR:= MaxNullString;
    PS_SQL_LONGVARCHAR:= MaxLongint;
    PS_SQL_BINARY:= MaxNullString;
    PS_SQL_VARBINARY:= MaxNullString;
    PS_SQL_LONGVARBINARY:= MaxLongint;
    PS_SQL_DECIMAL:= 15;
    PS_SQL_NUMERIC:= 15;
    PS_SQL_TYPE_TIMESTAMP:= 19;
    DD_SQL_DECIMAL:= 15;
    DD_SQL_NUMERIC:= 15;
    DD_SQL_TYPE_TIMESTAMP:= 0;
  End;
End;

Procedure THdbc.RemoveDriver(ADriver: String);
Var
  i: Integer;
Begin
  ADriver:= UpperCase(ADriver);
  For i:= 0 To FDrivers.Count-1 Do
    If UpperCase(TDriverPtr(FDrivers[i]).Desc) = ADriver Then
    Begin
      Dispose(TDriverPtr(FDrivers[i]));
      FDrivers.Delete(i);
      Break;
    End;
End;

Function THdbc.GetDriver(ADriver: String): TDriverPtr;
Var
  i: Integer;
Begin
  Result:= Nil;
  ADriver:= UpperCase(ADriver);
  For i:= 0 To FDrivers.Count-1 Do
    If UpperCase(TDriverPtr(FDrivers[i]).Desc) = ADriver Then
    Begin
      Result:= FDrivers[i];
      Break;
    End;
End;

Function THdbc.CurrentDriver: String;
Begin
  Result:= GetInfoString(SQL_DBMS_NAME)+';'+GetInfoString(SQL_DRIVER_NAME);
End;

Function THdbc.IsDriver(Const ADrivers: Array Of String): Boolean;
Var
  i: Integer;
  ADriver: String;
Begin
  ADriver:= UpperCase(CurrentDriver);

  Result:= False;
  For i:= Low(ADrivers) To High(ADrivers) Do
    If UpperCase(ADrivers[i]) = ADriver Then
    Begin
      Result:= True;
      Break;
    End;
End;

{ THstmt }

Procedure THstmt.FreeParams;
Var
  temp: TParamPtr;
Begin
  While FParams <> Nil Do
  Begin
    temp:= FParams^.Next;
    FreeMem(FParams^.FSize, FParams^.FCount*SizeOf(SQLINTEGER));
    Dispose(FParams);
    FParams:= temp;
  End;
  FParams:= Nil;
  SetLength(FParamIndexes, 0);
  FNumParams:= 0;
  FHdesc:= Nil;

  FParamNames.Clear;

  { Release Parameter Buffers }
  If FActive Then
  Begin
    FRetCode:= SQLFreeStmt(FHstmt, SQL_RESET_PARAMS);
    If Not FEnv.Error.Success(FRetCode) Then
      FEnv.Error.RaiseError(Self, FRetCode);
  End;
End;

Procedure THstmt.FreeCols;
Var
  temp: TColPtr;
Begin
  While FCols <> Nil Do
  Begin
    temp:= FCols^.Next;

    If FCols^.FBlob Then
      Begin
      FCols^.FMemory.Free;
      FCols^.FMemory:= Nil;
      End
    Else
    Begin
      If FCols^.FMemory <> Nil Then
        Begin
        FCols^.FMemory.Free;
        FCols^.FMemory:= Nil;
        End;

      If (FCols^.FSql = SQL_CHAR) Or (FCols^.FSql = SQL_VARCHAR) Then
        FreeMem(FCols^.FValue, FRowSetSize*(FCols^.FColumnSize+1))
      Else If (FCols^.FSql = SQL_BINARY) Or (FCols^.FSql = SQL_VARBINARY) Then
        FreeMem(FCols^.FValue, FRowSetSize*FCols^.FColumnSize)
      Else
        FreeMem(FCols^.FValue, FRowSetSize*PhysSize(FCols^.FType));
    End;
    FreeMem(FCols^.FSize, FRowSetSize*PhysSize(SQL_C_SLONG));
    Dispose(FCols);
    FCols:= temp;
  End;
  FCols:= Nil;
  SetLength(FColIndexes, 0);
  FNumCols:= 0;
  FNumRows:= 0;

  FColNames.Clear;

  { Release Column Buffers }
  If FActive Then
  Begin
    FRetCode:= SQLFreeStmt(FHstmt, SQL_UNBIND);
    If Not FEnv.Error.Success(FRetCode) Then
      FEnv.Error.RaiseError(Self, FRetCode);
  End;

  FreeColBinds;
  FBlobs:= False;
  FColumnsBound:= False;
End;

Procedure THstmt.FreeColBinds;
Var
  temp: TColBindPtr;
Begin
  While FColBinds <> Nil Do
  Begin
    temp:= FColBinds^.Next;
    Dispose(FColBinds);
    FColBinds:= temp;
  End;
  FColBinds:= Nil;
End;

Procedure THstmt.UnPrepareHstmts;
Begin
  If FHstmtInsert <> Nil Then
    FHstmtInsert.Prepared:= False;
  If FHstmtUpdate <> Nil Then
    FHstmtUpdate.Prepared:= False;
  If FHstmtDelete <>  Nil Then
    FHstmtDelete.Prepared:= False;
  If FHstmtRefresh <> Nil Then
    FHstmtRefresh.Prepared:= False;
End;

Procedure THstmt.InsertHead(FParam: SQLUSMALLINT;
                            FType: SQLSMALLINT;
                            FSql: SQLSMALLINT;
                            FValue: SQLPOINTER);
Var
  temp: TParamPtr;
Begin
  New(temp);
  temp^.FParam:= FParam;
  temp^.FType:= FType;
  temp^.FSql:= FSql;
  temp^.FValue:= FValue;
  temp^.FSize:= Nil;
  temp^.FCount:= 0;

  temp^.FParameterSize:= 0;
  temp^.FDecimalDigits:= 0;
  temp^.FNullable:= SQL_NULLABLE_UNKNOWN;

  temp^.Next:= FParams;
  FParams:= temp;
  if (FParam >= length(FParamIndexes)) then
    SetLength(FParamIndexes, FParam+1);

  FParamIndexes[FParam] := temp;
End;

Procedure THstmt.InsertTail(Var FTail: TColPtr;
                            FType: SQLSMALLINT;
                            FSql: SQLSMALLINT;
                            FValue: SQLPOINTER);
Var
  temp: TColPtr;
Begin
  New(temp);
  temp^.FType:= FType;
  temp^.FSql:= FSql;
  temp^.FValue:= FValue;
  GetMem(temp^.FSize, FRowSetSize*PhysSize(SQL_C_SLONG));
  temp^.FSize^:= SQL_NULL_DATA;
  temp^.FBlob:= (FSql = SQL_LONGVARCHAR) Or (FSql = SQL_LONGVARBINARY) Or (FSql = SQL_VARBINARY);
  temp^.FBlobFetched:= False;
  temp^.FMemory:= Nil;
  If temp^.FBlob Then
    temp^.FMemory:= TMemoryStream.Create;


  temp^.FFormatStyle:= DefFormatStyle;
  temp^.FFormatMask:= DefFormatMask;
  temp^.FPrimary:= DefPrimary;
  temp^.FIgnore:= DefIgnore;

  temp^.FColumnSize:= 0;
  temp^.FDecimalDigits:= 0;
  temp^.FNullable:= SQL_NULLABLE_UNKNOWN;

  temp^.Next:= Nil;
  If FTail = Nil Then
    FCols:= temp
  Else
    FTail^.Next:= temp;
  FTail:= temp;
  SetLength(FColIndexes, Length(FColIndexes)+1);
  FColIndexes[Length(FColIndexes)-1] := temp;
End;

Procedure THstmt.InsertColBind(FCol: SQLUSMALLINT;
                               FSql: SQLSMALLINT);
Var
  temp: TColBindPtr;
Begin
  New(temp);
  temp^.FCol:= FCol;
  temp^.FSql:= FSql;

  temp^.Next:= FColBinds;
  FColBinds:= temp;
End;

Procedure THstmt.SetHdbc(AHdbc: THdbc);
Begin
  Log(1, 'THstmt.SetHdbc');

  Terminate;

  FHdbc:= AHdbc;
End;

Procedure THstmt.SetSQL(ASQL: String);
Begin
  Log(1, 'THstmt.SetSQL');

  { Reset Hstmt }
  If FActive Then
    Close;

  FPrepared:= False;
  FExecuted:= False;

  FSQL:= ASQL;
End;

Procedure THstmt.SetPrepared(APrepared: Boolean);
Begin
  If APrepared Then
    Prepare
  Else
    FPrepared:= False;
End;

Procedure THstmt.SetExecuted(AExecuted: Boolean);
Begin
  If AExecuted Then
    Execute
  Else
    FExecuted:= False;
End;

Function THstmt.GetParamSize(Param: SQLUSMALLINT): SQLINTEGER;
Var
  temp: TParamPtr;
Begin
  Log(1, 'THstmt.GetParamSize');

  temp:= ParamRec(Param);
  If temp = Nil Then
    Result:= 0
  Else
  Begin
    If temp^.FSize^ <= SQL_LEN_DATA_AT_EXEC_OFFSET Then
      Result:= SQL_LEN_DATA_AT_EXEC_OFFSET-temp^.FSize^
    Else
      Result:= temp^.FSize^;
  End;
End;

Procedure THstmt.SetParamSize(Param: SQLUSMALLINT;
                              AParamSize: SQLINTEGER);
Var
  temp: TParamPtr;
Begin
  Log(1, 'THstmt.SetParamSize');

  temp:= ParamRec(Param);
  If temp <> Nil Then
  Begin
    If (temp^.FSize^ <= SQL_LEN_DATA_AT_EXEC_OFFSET) And (AParamSize >= 0) Then
      temp^.FSize^:= SQL_LEN_DATA_AT_EXEC_OFFSET-AParamSize
    Else
      temp^.FSize^:= AParamSize;
  End;
End;

Procedure THstmt.SetParamType(AParamType: SQLSMALLINT);
Begin
  Log(1, 'THstmt.SetParamType');

  If AParamType In [SQL_PARAM_INPUT, SQL_PARAM_OUTPUT, SQL_PARAM_INPUT_OUTPUT] Then
    FParamType:= AParamType;
End;

Procedure THstmt.SetBulkSize(ABulkSize: SQLUINTEGER);
Begin
  Log(1, 'THstmt.SetBulkSize');

  If ABulkSize > 0 Then
    FNumParams:= ABulkSize;
End;

Function THstmt.GetBulkParamSize(Param: SQLUSMALLINT;
                                 Row: SQLUINTEGER): SQLINTEGER;
Var
  temp: TParamPtr;
Begin
  temp:= ParamRec(Param);

  If (temp <> Nil) And (Row > 0) And (Row <= temp^.FCount) Then
    Result:= SQLINTEGER(OffsetPointer(temp^.FSize, (Row-1)*SizeOf(SQLINTEGER))^)
  Else
    Result:= 0;
End;

Procedure THstmt.SetBulkParamSize(Param: SQLUSMALLINT;
                                  Row: SQLUINTEGER;
                                  AParamSize: SQLINTEGER);
Var
  temp: TParamPtr;
Begin
  temp:= ParamRec(Param);

  If (temp <> Nil) And (Row > 0) And (Row <= temp^.FCount) Then
    SQLINTEGER(OffsetPointer(temp^.FSize, (Row-1)*SizeOf(SQLINTEGER))^):= AParamSize;
End;

Procedure THstmt.SetTargetTable(ATargetTable: String);
Begin
  Log(1, 'THstmt.SetTargetTable');

  FTargetTable:= ATargetTable;
End;

Function THstmt.GetColSize(Col: SQLUSMALLINT): SQLINTEGER;
Begin
  Result:= GetCellSize(Col, 1);
End;

Procedure THstmt.SetColSize(Col: SQLUSMALLINT;
                            AColSize: SQLINTEGER);
Begin
  SetCellSize(Col, 1, AColSize);
End;

Function THstmt.GetCellSize(Col, Row: SQLUSMALLINT): SQLINTEGER;
Var
  tempCol: TColPtr;
//  temp: TRowPtr;
//  LrRowRec : TRowRec;
Begin
  tempCol:= ColRec(Col);

  If (tempCol <> Nil) And
     (Row > 0) And (Row <= FRowSetSize) Then
    Begin
    If tempCol^.FBlob Then
      Begin
      If tempCol^.FSize^ < 0 Then
        Begin
        Result:= tempCol^.FSize^;
        End
      Else
        Begin
        Result:= tempCol^.FMemory.Size;
        End;
      End
    Else
      Begin
      Result := SQLINTEGERPtr(OffsetRow(tempCol^.FSize, Row, SizeOf(Integer)))^;
      End;
    End
  Else
    Begin
    Result:= 0;
    End;

{
Alternative Semi-Optimal implementation

  if RowRecEx(Col, Row, LrRowRec) then
    begin
    if LrRowRec.FBlob then
      begin
      tempCol:= ColRec(Col);  //if blob deferral then don't fetch blob

      if tempCol^.FSize^ < 0 then
        begin
        Result:= tempCol^.FSize^
        end
      else
        begin
        Result:= tempCol^.FMemory.Size;
        end;
      end
    else
      begin
      Result:= LrRowRec.FSize^;
      end;
    end
  else
    begin
    Result:= 0
    end;
}

{
Previous Non-Optimal implementation

  temp:= RowRec(Col, Row);

  if temp = nil then
    Result:= 0
  else
  begin
    if temp^.FBlob then
    begin
      tempCol:= ColRec(Col);  //if blob deferral then don't fetch blob

      if tempCol^.FSize^ < 0 then
        Result:= tempCol^.FSize^
      else
        Result:= tempCol^.FMemory.Size;
    end
    else
      Result:= temp^.FSize^;
    Dispose(temp);
  end;
}
End;

Procedure THstmt.SetCellSize(Col, Row: SQLUSMALLINT;
                             AColSize: SQLINTEGER);
Var
  tempCol: TColPtr;
  temp: TRowPtr;
Begin
  temp:= RowRec(Col, Row);

  If temp <> Nil Then
  Begin
    If temp^.FBlob Then
    Begin
      tempCol:= ColRec(Col);
      If AColSize >= 0 Then
        tempCol^.FMemory.SetSize(AColSize);
      tempCol^.FSize^:= AColSize;
    End
    Else
      temp^.FSize^:= AColSize;
    Dispose(temp);
  End;
End;

Function THstmt.GetColNull(Col: SQLUSMALLINT): Boolean;
Begin
  Result:= ColSize[Col] = SQL_NULL_DATA;
End;

Procedure THstmt.SetColNull(Col: SQLUSMALLINT;
                            AColNull: Boolean);
Begin
  If AColNull Then
    ColSize[Col]:= SQL_NULL_DATA;
End;

Function THstmt.GetCellNull(Col, Row: SQLUSMALLINT): Boolean;
Begin
  Result:= CellSize[Col,Row] = SQL_NULL_DATA;
End;

Procedure THstmt.SetCellNull(Col, Row: SQLUSMALLINT;
                             ACellNull: Boolean);
Begin
  If ACellNull Then
    CellSize[Col,Row]:= SQL_NULL_DATA;
End;

Function THstmt.GetColIgnore(Col: SQLUSMALLINT): Boolean;
Begin
  Result:= ColSize[Col] = SQL_IGNORE;
End;

Procedure THstmt.SetColIgnore(Col: SQLUSMALLINT;
                              AColIgnore: Boolean);
Var
  temp: TColPtr;
Begin
  If AColIgnore Then
    ColSize[Col]:= SQL_IGNORE;

  { For ColIgnoreSync }
  temp:= ColRec(Col);
  If temp <> Nil Then
    temp^.FIgnore:= AColIgnore;
End;

Function THstmt.GetCellIgnore(Col, Row: SQLUSMALLINT): Boolean;
Begin
  Result:= CellSize[Col,Row] = SQL_IGNORE;
End;

Procedure THstmt.SetCellIgnore(Col, Row: SQLUSMALLINT;
                               ACellIgnore: Boolean);
Begin
  If ACellIgnore Then
    CellSize[Col,Row]:= SQL_IGNORE;
End;

Function THstmt.GetFormatStyle(Col: SQLUSMALLINT): TFormatStyle;
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp = Nil Then
    Result:= fsNone
  Else
    Result:= temp^.FFormatStyle;
End;

Procedure THstmt.SetFormatStyle(Col: SQLUSMALLINT;
                                AFormatStyle: TFormatStyle);
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp <> Nil Then
    temp^.FFormatStyle:= AFormatStyle;
End;

Function THstmt.GetFormatMask(Col: SQLUSMALLINT): String;
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp = Nil Then
    Result:= ''
  Else
    Result:= temp^.FFormatMask;
End;

Procedure THstmt.SetFormatMask(Col: SQLUSMALLINT;
                               AFormatMask: String);
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp <> Nil Then
    temp^.FFormatMask:= AFormatMask;
End;

Function THstmt.GetColPrimary(Col: SQLUSMALLINT): Boolean;
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp = Nil Then
    Result:= False
  Else
    Result:= temp^.FPrimary;
End;

Procedure THstmt.SetColPrimary(Col: SQLUSMALLINT;
                               AColPrimary: Boolean);
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp <> Nil Then
    temp^.FPrimary:= AColPrimary;
End;

Function THstmt.GetColPrecision(Col: SQLUSMALLINT): SQLUINTEGER;
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp = Nil Then
    Result:= 0
  Else
    Result:= temp^.FColumnSize;
End;

Function THstmt.GetColScale(Col: SQLUSMALLINT): SQLSMALLINT;
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp = Nil Then
    Result:= 0
  Else
    Result:= temp^.FDecimalDigits;
End;

Function THstmt.GetColNullable(Col: SQLUSMALLINT): SQLSMALLINT;
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp = Nil Then
    Result:= SQL_NULLABLE_UNKNOWN
  Else
    Result:= temp^.FNullable;
End;

Function THstmt.GetParamPrecision(Param: SQLUSMALLINT): SQLUINTEGER;
Var
  temp: TParamPtr;
Begin
  temp:= ParamRec(Param);

  If temp = Nil Then
    Result:= 0
  Else
    Result:= temp^.FParameterSize;
End;

Function THstmt.GetParamScale(Param: SQLUSMALLINT): SQLSMALLINT;
Var
  temp: TParamPtr;
Begin
  temp:= ParamRec(Param);

  If temp = Nil Then
    Result:= 0
  Else
    Result:= temp^.FDecimalDigits;
End;

Function THstmt.GetParamNullable(Param: SQLUSMALLINT): SQLSMALLINT;
Var
  temp: TParamPtr;
Begin
  temp:= ParamRec(Param);

  If temp = Nil Then
    Result:= SQL_NULLABLE_UNKNOWN
  Else
    Result:= temp^.FNullable;
End;

Function THstmt.GetRowStatus(Row: SQLUSMALLINT): SQLUSMALLINT;
Begin
  If (Row > 0) And (Row <= FRowSetSize) Then
    Result:= SQLUSMALLINT(OffsetRow(FRowStatus^.FValue, Row, PhysSize(SQL_C_USHORT))^)
  Else
    Result:= SQL_ROW_NOROW;
End;

Function THstmt.GetRowFlag(Row: SQLUSMALLINT): SQLUSMALLINT;
Var
  Flags: SQLUSMALLINTPtr;
Begin
  Flags:= RowFlags(Row);
  If Flags = Nil Then
    Result:= rfNone
  Else
    Result:= Flags^ And rfOps;
End;

Procedure THstmt.SetRowFlag(Row: SQLUSMALLINT;
                            ARowFlag: SQLUSMALLINT);
Var
  Flags: SQLUSMALLINTPtr;
Begin
  Flags:= RowFlags(Row);
  If Flags <> Nil Then
  Begin
    If (rfOps And ARowFlag) = rfNone Then
      Flags^:= Flags^ And (Not rfOps)
    Else
      Flags^:= (Flags^ And (Not rfOps)) Or ARowFlag;
  End;
End;

Function THstmt.GetParamNames: TStringList;
Begin
  If Not FPrepared Then
    Begin
    ParseSQL;
    End;

  Result:= FParamNames;
End;

Function THstmt.GetColNames: TStringList;
Begin
  Result:= FColNames;
End;

Function THstmt.GetBookmark: SQLPOINTER;
Var
  StringLength: SQLLEN;
Begin
  Log(1, 'THstmt.GetBookmark');

  Result:= Nil;
  If ((FCursorAttr And SQL_CA1_BOOKMARK) = SQL_CA1_BOOKMARK) And RowValid[1] Then
  Begin
    GetMem(Result, BookmarkSize);
    If FBindBookmarks Then
      Move(FRowBookmark^.FValue^, Result^, BookmarkSize)
    Else
    Begin
      FRetCode:= SQLSetPos(FHstmt, 1, SQL_POSITION, SQL_LOCK_NO_CHANGE);
      If Not FEnv.Error.Success(FRetCode) Then
      Begin
        FreeMem(Result, BookmarkSize);
        Result:= Nil;
        Exit;
      End;

      //depreciated FRetCode:= SQLGetStmtOption(FHstmt, SQL_GET_BOOKMARK, @Bookmark);
      FRetCode:= SQLGetData(FHstmt, 0, SQL_C_VARBOOKMARK, Result, BookmarkSize, @StringLength);
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
    End;
  End;
End;

Procedure THstmt.SetBookmark(ABookmark: SQLPOINTER);
Begin
  FetchBookmark(ABookmark);
End;

Procedure THstmt.FreeBookmark(ABookmark: SQLPOINTER);
Begin
  If ABookmark <> Nil Then
    FreeMem(ABookmark, BookmarkSize);
End;

Function THstmt.GetRowValid(Row: SQLUSMALLINT): Boolean;
Begin
  Result:= RowStatus[Row] In [SQL_ROW_SUCCESS, SQL_ROW_SUCCESS_WITH_INFO,
                              SQL_ROW_ADDED, SQL_ROW_UPDATED];
End;

Function THstmt.GetQueryTimeOut: SQLUINTEGER;
Begin
  Init;

  FRetCode:= SQLGetStmtAttr(FHstmt, SQL_ATTR_QUERY_TIMEOUT, @Result, SizeOf(Result), Nil);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure THstmt.SetQueryTimeOut(AQueryTimeOut: SQLUINTEGER);
Begin
  Init;

  FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_QUERY_TIMEOUT, Pointer(AQueryTimeOut), SizeOf(AQueryTimeOut));
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure THstmt.SetSpecialSQLStatementAttribute(AAttribute: SQLINTEGER; AValue: SQLPOINTER; AStringLength: SQLINTEGER);
Begin
  Init;

  FRetCode:= SQLSetStmtAttr(FHstmt, AAttribute, AValue, AStringLength);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Function THstmt.GetMaxRows: SQLUINTEGER;
Begin
  Init;

  FRetCode:= SQLGetStmtAttr(FHstmt, SQL_ATTR_MAX_ROWS, @Result, SizeOf(Result), Nil);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure THstmt.SetMaxRows(AMaxRows: SQLUINTEGER);
Begin
  Init;

  FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_MAX_ROWS, Pointer(AMaxRows), SizeOf(AMaxRows));
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure THstmt.SetConcurrencyType(AConcurrencyType: SQLUINTEGER);
Var
  LConcurrencyType: SQLUINTEGER;
Begin
  Log(1, 'THstmt.SetConcurrencyType');

  FConcurrencyType:= AConcurrencyType;

  If FActive Then
  Begin
    Close;

    FRetCode:= SQLGetStmtAttr(FHstmt, SQL_ATTR_CONCURRENCY, @LConcurrencyType, SizeOf(LConcurrencyType), Nil);
    If FEnv.Error.Success(FRetCode) And (LConcurrencyType <> FConcurrencyType) Then
    Begin
      FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_CONCURRENCY, Pointer(FConcurrencyType), SizeOf(FConcurrencyType));
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
    End;
  End;
End;

Procedure THstmt.SetCursorType(ACursorType: SQLUINTEGER);
Var
  LCursorType: SQLUINTEGER;

  Procedure GetCursorAttr;
  Begin
    //depreciated GetInfo(SQL_POS_OPERATIONS);
    //depreciated GetInfo(SQL_POSITIONED_STATEMENTS);
    Case FCursorType Of
      SQL_CURSOR_FORWARD_ONLY:
        FCursorAttr:= Hdbc.GetInfoInteger(SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1);
      SQL_CURSOR_STATIC:
        FCursorAttr:= Hdbc.GetInfoInteger(SQL_STATIC_CURSOR_ATTRIBUTES1);
      SQL_CURSOR_KEYSET_DRIVEN:
        FCursorAttr:= Hdbc.GetInfoInteger(SQL_KEYSET_CURSOR_ATTRIBUTES1);
      SQL_CURSOR_DYNAMIC:
        FCursorAttr:= Hdbc.GetInfoInteger(SQL_DYNAMIC_CURSOR_ATTRIBUTES1);
    End;
  End;

Begin
  Log(1, 'THstmt.SetCursorType');

  FCursorType:= ACursorType;

  If FActive Then
  Begin
    Close;
    GetCursorAttr;

    FRetCode:= SQLGetStmtAttr(FHstmt, SQL_ATTR_CURSOR_TYPE, @LCursorType, SizeOf(LCursorType), Nil);
    If FEnv.Error.Success(FRetCode) And (LCursorType <> FCursorType) Then
    Begin
      FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_CURSOR_TYPE, Pointer(FCursorType), SizeOf(FCursorType));
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
    End;
  End;
End;

Procedure THstmt.SetRowSetSize(ARowSetSize: SQLUINTEGER);
Var
  LRowSetSize: SQLUINTEGER;
Begin
  Log(1, 'THstmt.SetRowSetSize');

  If ARowSetSize > 0 Then
  Begin
    FRowSetSize:= ARowSetSize;

    If FActive Then
    Begin
      { Dispose Data Before Changing RowSetSize }
      Close;

      FRetCode:= SQLGetStmtAttr(FHstmt, SQL_ATTR_ROW_ARRAY_SIZE, @LRowSetSize, SizeOf(LRowSetSize), Nil);
      If FEnv.Error.Success(FRetCode) And (LRowSetSize <> FRowSetSize) Then
      Begin
        FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_ROW_ARRAY_SIZE, Pointer(FRowSetSize), SizeOf(FRowSetSize));
        If Not FEnv.Error.Success(FRetCode) Then
          FEnv.Error.RaiseError(Self, FRetCode);
      End;
    End;
  End;
End;

Procedure THstmt.DynamicallySetRowSetSize(ARowSetSize: SQLUINTEGER);
Begin
  If (ARowSetSize > 0) And FActive And (FRowSetSize <> ARowSetSize) Then
    Begin
    FRowSetSize:= ARowSetSize;

    //Storage buffers need to be Rebound
    FreeCols;
    Try
      //Set the row array size to the new value
      FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_ROW_ARRAY_SIZE, Pointer(FRowSetSize), SizeOf(FRowSetSize));
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
    Finally
      BindCols;
      End;
    End;
End;

Procedure THstmt.SetBlobSize(ABlobSize: LongInt);
Begin
  Log(1, 'THstmt.SetBlobSize');

  If (ABlobSize <> FBlobSize) And (ABlobSize > MinBlobSize) Then
    FBlobSize:= ABlobSize;
End;

Constructor THstmt.Create(Env : THEnv; dbc : THdbc);
Begin
  Inherited Create(Env);

  { Set Defaults }
  FHdbc:= dbc;
  FHdesc:= Nil;
  FActive:= False;
  FRetCode:= SQL_SUCCESS;

  FColBinds:= Nil;

  FCols:= Nil;

  FParams:= Nil;
  
  FRowStatus:= Nil;
  FRowFlags:= Nil;
  FRowBookmark:= Nil;
  FNumCols:= 0;
  FNumRows:= 0;
  FNumParams:= 0;
  FBulkData:= DefBulkData;
  FBlobs:= False;
  FColumnsBound:= False;
  FBindBookmarks:= DefBindBookmarks;
  FBookmarkSize:= DefBookmarkSize;

  FPrepared:= DefPrepared;
  FExecuted:= DefExecuted;
  FParamType:= DefParamType;
  FTargetTable:= '';
  FTableOwner:= '';
  FTableName:= '';
  FSkipByPosition:= DefSkipByMethod;
  FSkipByCursor:= DefSkipByMethod;
  FParamNames:= TStringList.Create;
  FColNames:= TStringList.Create;
  FHstmtInsert:= Nil;
  FHstmtUpdate:= Nil;
  FHstmtDelete:=  Nil;
  FHstmtRefresh:= Nil;

  FSQL:= '';
  FSQLParsing:= DefSQLParsing;
  FCursorAttr:= 0;
  FConcurrencyType:= DefConcurrencyType;
  FCursorType:= DefCursorType;
  FRowSetSize:= DefRowSetSize;
  FBlobSize:= DefBlobSize;
  FBlobDeferral:= DefBlobDeferral;
  FBlobPlacement:= DefBlobPlacement;
  FExecAsync:= DefExecAsync;
  FEmptyToNull:= DefEmptyToNull;
  FStringTrimming:= DefStringTrimming;
  FBindByName:= DefBindByName;
  FRowCountMethod:= DefRowCountMethod;
  FNoRowsAffected:= DefNoRowsAffected;
  FAborted:= False;
  FAsyncEnabled:= False;
End;

Destructor THstmt.Destroy;
Begin
  Inherited Destroy;

  { Terminate Self }
  Terminate;

  FParamNames.Free;
  SetLength(FParamIndexes, 0);
  
  FColNames.Free;
  SetLength(FColIndexes, 0);

  If FHstmtInsert <> Nil Then
    FHstmtInsert.Free;
  If FHstmtUpdate <> Nil Then
    FHstmtUpdate.Free;
  If FHstmtDelete <>  Nil Then
    FHstmtDelete.Free;
  If FHstmtRefresh <> Nil Then
    FHstmtRefresh.Free;
End;

Function THstmt.Init: Boolean;
Begin
  Log(1, 'THstmt.Init');

  If FActive Then
  Begin
    Init:= FActive;
    Exit;
  End;

  Hdbc.Connect;

  { Create Handle }
  FRetCode:= SQLAllocHandle(SQL_HANDLE_STMT, Hdbc.Handle, @FHstmt);
  If Not FEnv.Error.Success(FRetCode) Then
  Begin
    FHstmt := Nil;
    FEnv.Error.RaiseError(Self, FRetCode);
  End;

  { Set Active Field }
  FActive:= True;

  { Set ODBC Properties }
  ConcurrencyType:= FConcurrencyType;
  CursorType:= FCursorType;
  RowSetSize:= FRowSetSize;

  Init;  //call before enabling bookmarks to avoid bookmarksize retrieval problem

  If (FCursorAttr And SQL_CA1_BOOKMARK) = SQL_CA1_BOOKMARK Then
  Begin
    FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_USE_BOOKMARKS, Pointer(SQL_UB_VARIABLE), SizeOf(SQLUINTEGER));
    If FBindBookmarks Then
      FBindBookmarks:= FEnv.Error.Success(FRetCode);
  End
  Else
    FBindBookmarks:= False;

  Result:= FActive;
End;

Function THstmt.Terminate: Boolean;
Begin
  Log(1, 'THstmt.Terminate');

  //Inherited Terminate;

  If FActive Then
  Begin

    { Dispose Storage }
    Close;

    { Free Handle }
    FRetCode:= SQLFreeHandle(SQL_HANDLE_STMT, FHstmt);
    If Not FEnv.Error.Success(FRetCode) Then
      FEnv.Error.RaiseError(Self, FRetCode);
    FHstmt := Nil;

    { Set Active Field }
    FActive:= False;
  End;

  Result:= Not FActive;
End;

Procedure THstmt.CloseCursor;
Begin
  Init;

  { Reset Statement Handle }
  FRetCode:= SQLFreeStmt(FHstmt, SQL_CLOSE);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure THstmt.Close;
Begin
  Log(1, 'THstmt.Close');

  CloseCursor;

  { Dispose of Storage }
  FreeParams;
  FreeCols;
  
  UnPrepareHstmts;

  FPrepared:= False;
  FExecuted:= False;
End;

Function THstmt.GetHandle: SQLHSTMT;
Begin
  Init;

  { Retrieve Handle }
  Result:= FHstmt;
End;

Function THstmt.GetColCount: SQLSMALLINT;
Begin
  Log(1, 'THstmt.NumCols');

  Init;

  { Bind Columns }
  BindCols;

  Result:= FNumCols;
End;

Function THstmt.GetRowCount: SQLINTEGER;
Var
  tempHstmt: THstmt;
  FromLoc, Loc: Integer;
  temp: TParamPtr;
  ASQL, UpperSQL: String;
Begin
  If FRowCountMethod = rcFunction Then
  Begin
    FRetCode:= SQLGetDiagField(SQL_HANDLE_STMT, FHstmt, 0, SQL_DIAG_CURSOR_ROW_COUNT, @Result, SizeOf(Result), Nil);
    If FEnv.Error.Success(FRetCode) And (Result >= 0) Then
      Exit;
  End;

  If FRowCountMethod = rcCustom Then
  Begin
    Result:= DoRowCount;
    If Result >= 0 Then
      Exit;
  End;

  Result:= 0;

  tempHstmt:= THstmt.Create(FEnv, FHdbc);
  tempHstmt.MaxRows:= MaxRows;

  Try

    If FRowCountMethod <> rcTraverse Then
    Begin
      ASQL:= StringReplace(FSQL, EnterString, ' ', [rfReplaceAll, rfIgnoreCase]);
      UpperSQL:= UpperCase(ASQL);
      UpperSQL:= StringReplace(UpperSQL, ',', ' ', [rfReplaceAll, rfIgnoreCase]);
      UpperSQL:= StringReplace(UpperSQL, '(', ' ', [rfReplaceAll, rfIgnoreCase]);
      UpperSQL:= StringReplace(UpperSQL, ')', ' ', [rfReplaceAll, rfIgnoreCase]);
      UpperSQL:= StringSupport.StringReplace(UpperSQL, StringSupport.setControls, ' ');

      FromLoc:= Pos(' FROM ', UpperSQL);
      If (FromLoc > 0) And
         (Pos(' DISTINCT ', UpperSQL) = 0) And
         (Pos(' HAVING ', UpperSQL) = 0) And
         (Pos(' UNION ', UpperSQL) = 0) Then
      Begin
        Loc:= Pos(' GROUP ', UpperSQL);
        If Loc > 0 Then
          ASQL:= Copy(ASQL, 1, Loc-1);
        Loc:= Pos(' ORDER ', UpperSQL);
        If Loc > 0 Then
          ASQL:= Copy(ASQL, 1, Loc-1);

        tempHstmt.SQL:= 'SELECT Count(*)'+Copy(ASQL, FromLoc, Length(ASQL)-FromLoc+1);
        tempHstmt.Prepare;

        temp:= FParams;
        While temp <> Nil Do
        Begin
          tempHstmt.BindParam(temp^.FParam, temp^.FType, temp^.FValue, temp^.FSql);

          //Need to equalify paramsize
          If TempHstmt.ParamSize[temp^.FParam] <> temp^.FSize^ Then
            Begin
            TempHstmt.ParamSize[temp^.FParam] := temp^.FSize^;
            End;

          temp:= temp^.Next;
        End;

        Try

          tempHstmt.Execute;
          If tempHstmt.FetchNext Then
            Result:= tempHstmt.ColCardinal[1];
          Exit;

        Except
          tempHstmt.Terminate;
          //try following code
        End;
      End;
    End;

    tempHstmt.SQL:= FSQL;
    tempHstmt.Prepare;

    temp:= FParams;
    While temp <> Nil Do
    Begin
      tempHstmt.BindParam(temp^.FParam, temp^.FType, temp^.FValue, temp^.FSql);

      //Need to equalify paramsize
      If TempHstmt.ParamSize[temp^.FParam] <> temp^.FSize^ Then
        Begin
        TempHstmt.ParamSize[temp^.FParam] := temp^.FSize^;
        End;

      temp:= temp^.Next;
    End;

    tempHstmt.Execute;
    While tempHstmt.FetchNext Do
      Inc(Result);

  Finally
    tempHstmt.Free;
  End;
End;

Function THstmt.GetRowsFetched: SQLUINTEGER;
Begin
  Log(1, 'THstmt.NumRowsFetched');

  Result:= FNumRows;
End;

Function THstmt.GetRowsAffected: SQLULEN;
Begin
  Log(1, 'THstmt.NumRowsAffected');

  FRetCode:= SQLRowCount(FHstmt, @Result);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
  {$IFDEF WIN64}
  if result = $FFFFFFFFFFFFFFFF then
    result := 0;
  {$ENDIF}
End;

Function THstmt.ParseSQL: String;
Var
  Token: String;
  Loc: Integer;
  Literal: Boolean;

  LbParamIsEscaped  : Boolean;
  LiSubEscapeCount  : Integer;
  LcOpenEscapeChar  : Char;
  LcCloseEscapeChar : Char;

  LsResult       : String;
  LiResultLength : Integer;

  Procedure AppendToLocalResult(AcChar : Char);
  Begin
    Inc(LiResultLength);
    LsResult[LiResultLength] := AcChar;
  End;
Begin
  If Not FSQLParsing Then
  Begin
    Result:= FSQL;
    Exit;
  End;

  FParamNames.Clear;

  Literal:= False;

  LiResultLength:= 0;
  SetLength(LsResult,Length(FSQL));

  Loc:= 1;
  While Loc <= Length(FSQL) Do
  Begin
    If FSQL[Loc] = '''' Then
    Begin
      Inc(Loc);
      If (Loc <= Length(FSQL)) And (FSQL[Loc] = '''') Then
      Begin
        AppendToLocalResult('''');
        Inc(Loc);
      End
      Else
        Literal:= Not Literal;
      AppendToLocalResult('''');
    End
    Else If (Not Literal) And (FSQL[Loc] = ':') Then
    Begin
      Inc(Loc);

      If Loc > Length(FSQL) Then
        Raise EODBCExpress.Create('Invalid parameter token in SQL statement.');

      AppendToLocalResult('?');

      Token:= '';

      If FSQL[Loc] = '[' Then
        Begin
        //This parameter is wrapped in the '[' literal escape
        LbParamIsEscaped  := True;
        LcOpenEscapeChar  := '[';
        LcCloseEscapeChar := ']';
        End
      Else If FSQL[Loc] = '"' Then
        Begin
        //This parameter is wrapped in the '"' literal escape
        LbParamIsEscaped  := True;
        LcOpenEscapeChar  := '"';
        LcCloseEscapeChar := '"';
        End
      Else
        Begin
        LbParamIsEscaped  := False;
        LcOpenEscapeChar  := '"'; //Added to remove warning
        LcCloseEscapeChar := '"'; //Added to remove warning
        End;

      If LbParamIsEscaped Then
        Begin
        //This parameter has been escaped
        //  - token is composed of entire escaped block (including the escape chars)

        Token:= FSQL[Loc];
        Inc(Loc);

        LiSubEscapeCount := 0;
        While (Loc <= Length(FSQL)) And (LiSubEscapeCount >= 0) Do
          Begin
          Token:= Token+FSQL[Loc];

          If FSQL[Loc] = LcOpenEscapeChar Then
            Begin
            Inc(LiSubEscapeCount);
            End
          Else If FSQL[Loc] = LcCloseEscapeChar Then
            Begin
            Dec(LiSubEscapeCount);
            End;

          Inc(Loc);
          End;

        If (Length(Token) < 2) Or (Token[Length(Token)] <> LcCloseEscapeChar) Then
          Begin
          Raise EODBCExpress.Create('Invalid escaped parameter token in SQL statement. Missing (' + LcCloseEscapeChar + ')');
          End;
        End
      Else
        Begin
        While (Loc <= Length(FSQL)) And CharInSet(FSQL[Loc], ParamCharSet) Do
          Begin
          Token:= Token+FSQL[Loc];
          Inc(Loc);
          End;
        End;

      If Token = '' Then
        Begin
        Raise EODBCExpress.Create('Invalid parameter token in SQL statement.');
        End;

//This check requires a full parse of the SQL
//      if (Loc <= Length(FSQL)) and (NOT (FSQL[Loc] in [#8, #9, #10, #13, #27, #32,')'])) then
//        begin
//        raise EODBCExpress.Create('Invalid parameter token in SQL statement. Parameter must be proceed with white space (' + FSQL + ')');
//        end;

      FParamNames.Add(Token);
    End
    Else
    Begin
      AppendToLocalResult(FSQL[Loc]);
      Inc(Loc);
    End;
  End;

  SetLength(LsResult,LiResultLength);
  Result := LsResult;
End;

Procedure THstmt.Prepare;
Var
  ParsedSQL: String;
Begin
  Log(1, 'THstmt.Prepare');

  { Reset Hstmt }
  Close;

  DoBeforePrepare;

  { Parse Parameter SQL }
  ParsedSQL:= ParseSQL;

  { Prepare SQL Statement }
  FRetCode:= SQLPrepare(FHstmt, Pointer(PChar(ParsedSQL)), SQL_NTS);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);

  DoAfterPrepare;

  FPrepared:= True;
End;

Function THstmt.BindCore: Boolean;
Begin
  Result:= Hdbc.Core Or (Not FPrepared) Or FBindByName;
End;

Procedure THstmt.BindParamMain(Param: SQLUSMALLINT;
                               ParamType: SQLSMALLINT;
                               ParamValue: SQLPOINTER;
                               SqlType: SQLSMALLINT;
                               ParameterSize: SQLULEN;
                               DecimalDigits: SQLSMALLINT;
                               Nullable: SQLSMALLINT;
                               Bulk: Integer);
Var
  i: Integer;
  temp: TParamPtr;
  BufferLength: SQLLEN;
  ParamName: String;

  Function BlobPlacementByParts: Boolean;
  Var
    LongDataLen: String;
  Begin
    Case FBlobPlacement Of
      bpByParts:
        Result:= True;
      bpByExec:
        Result:= False;
      Else
      Begin
        LongDataLen:= Hdbc.GetInfoString(SQL_NEED_LONG_DATA_LEN);
        Result:= (LongDataLen <> '') And (LongDataLen[1] = 'Y');
      End;
    End;
  End;

Begin
  Log(1, 'THstmt.BindParamMain');

  temp:= ParamRec(Param);
  If temp = Nil Then
  Begin
    InsertHead(Param, ParamType, SqlType, ParamValue);
    temp:= FParams;
    temp.FParameterSize:= ParameterSize;
    temp.FDecimalDigits:= DecimalDigits;
    temp.FNullable:= Nullable;

    If Bulk = 0 Then
    Begin
      temp^.FCount:= 1;
      New(temp^.FSize);
    End
    Else
    Begin
      temp^.FCount:= Bulk;
      GetMem(temp^.FSize, temp^.FCount*SizeOf(SQLINTEGER));
    End;
  End
  Else
  Begin
    temp^.FType:= ParamType;
    temp^.FSql:= SqlType;
    temp^.FValue:= ParamValue;
  End;

  If FNumParams < temp^.FCount Then
    FNumParams:= temp^.FCount;

  BufferLength:= 0;
  temp^.FSize^:= 0;

  { Create Storage }
  Case SqlType Of
    SQL_CHAR, SQL_WCHAR:
    Begin
      If Bulk = 0 Then
        BufferLength:= ParameterSize+1
      Else
        BufferLength:= MaxNullString+1;
      temp^.FSize^:= SQL_NTS;
    End;
    SQL_VARCHAR:
    Begin
      If Bulk = 0 Then
        BufferLength:= ParameterSize+1
      Else
        BufferLength:= MaxNullString+1;
      temp^.FSize^:= SQL_NTS;
    End;
    SQL_LONGVARCHAR:
    Begin
      If (Not FBulkData) And (Bulk <> 0) Then
        Raise EODBCExpress.Create('Cannot insert bulk blobs.');

      BufferLength:= MaxLongint;
      If BlobPlacementByParts Then
        temp^.FSize^:= SQL_LEN_DATA_AT_EXEC_OFFSET
      Else
        temp^.FSize^:= 0;
    End;

    SQL_BINARY:
    Begin
      If Bulk = 0 Then
        BufferLength:= ParameterSize
      Else
        BufferLength:= MaxNullString+1;
    End;
    SQL_VARBINARY:
    Begin
      //we will treat varbinary like a try variable length blob

      If (Not FBulkData) And (Bulk <> 0) Then
        Raise EODBCExpress.Create('Cannot insert bulk blobs.');

      BufferLength:= MaxLongint;
      If BlobPlacementByParts Then
        temp^.FSize^:= SQL_LEN_DATA_AT_EXEC_OFFSET
      Else
        temp^.FSize^:= 0;

    End;
    SQL_LONGVARBINARY:
    Begin
      If (Not FBulkData) And (Bulk <> 0) Then
        Raise EODBCExpress.Create('Cannot insert bulk blobs.');

      BufferLength:= MaxLongint;
      If BlobPlacementByParts Then
        temp^.FSize^:= SQL_LEN_DATA_AT_EXEC_OFFSET
      Else
        temp^.FSize^:= 0;
    End;
  End;

  { Null Data }
  If ParamValue = Nil Then
    temp^.FSize^:= SQL_NULL_DATA;

  If (Bulk <> 0) Then
    For i:= 1 To temp^.FCount-1 Do
      SQLINTEGER(OffsetPointer(temp^.FSize, i*SizeOf(SQLINTEGER))^):= temp^.FSize^;

  { Bind Parameter }
  FRetCode:= SQLBindParameter(FHstmt, Param, FParamType, ParamType, SqlType,
    ParameterSize, DecimalDigits, ParamValue, BufferLength, temp^.FSize);
  FParamType:= DefParamType;
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);

  If FBindByName And (Param <= FParamNames.Count) Then
  Begin
    If FHdesc = Nil Then
    Begin
      FRetCode:= SQLGetStmtAttr(FHstmt, SQL_ATTR_IMP_PARAM_DESC, @FHdesc, SizeOf(FHdesc), Nil);
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
    End;
    ParamName:= FParamNames[Param-1];
    SQLSetDescField(FHdesc, Param, SQL_DESC_NAME, Pointer(PChar(ParamName)), SQL_NTS);
    SQLSetDescField(FHdesc, Param, SQL_DESC_UNNAMED, Pointer(SQL_NAMED), 0);
  End;
End;

Procedure THstmt.DescribeParam(Param: SQLUSMALLINT;
                               Var SqlType: SQLSMALLINT;
                               Var ParameterSize: SQLULEN;
                               Var DecimalDigits: SQLSMALLINT;
                               Var Nullable: SQLSMALLINT;
                               Core: Boolean);
Var
  DriverPtr: TDriverPtr;
Begin
  If Core Then
  Begin
    ParameterSize:= 0;
    DecimalDigits:= 0;
    Nullable:= SQL_NULLABLE_UNKNOWN;

    DriverPtr:= Hdbc.GetDriver(Hdbc.CurrentDriver);
    If DriverPtr = Nil Then
      DriverPtr:= Hdbc.GetDriver('Default');

    Case SqlType Of
      SQL_CHAR:
        ParameterSize:= DriverPtr.PS_SQL_CHAR;
      SQL_VARCHAR:
        ParameterSize:= DriverPtr.PS_SQL_VARCHAR;
      SQL_LONGVARCHAR:
        ParameterSize:= DriverPtr.PS_SQL_LONGVARCHAR;

      SQL_BINARY:
        ParameterSize:= DriverPtr.PS_SQL_BINARY;
      SQL_VARBINARY:
        ParameterSize:= DriverPtr.PS_SQL_VARBINARY;
      SQL_LONGVARBINARY:
        ParameterSize:= DriverPtr.PS_SQL_LONGVARBINARY;

      SQL_DECIMAL:
      Begin
        ParameterSize:= DriverPtr.PS_SQL_DECIMAL;
        DecimalDigits:= DriverPtr.DD_SQL_DECIMAL;
      End;
      SQL_NUMERIC:
      Begin
        ParameterSize:= DriverPtr.PS_SQL_NUMERIC;
        DecimalDigits:= DriverPtr.DD_SQL_NUMERIC;
      End;

      SQL_TYPE_TIMESTAMP:
      Begin
        ParameterSize:= DriverPtr.PS_SQL_TYPE_TIMESTAMP;
        DecimalDigits:= DriverPtr.DD_SQL_TYPE_TIMESTAMP;
      End;
    End;
  End
  Else
  Begin
    FRetCode:= SQLDescribeParam(FHstmt, Param, @SqlType, @ParameterSize, @DecimalDigits, @Nullable);
    If Not FEnv.Error.Success(FRetCode) Then
      DescribeParam(Param, SqlType, ParameterSize, DecimalDigits, Nullable, True);
  End;
End;

Procedure THstmt.BindParam(Param: SQLUSMALLINT;
                           ParamType: SQLSMALLINT;
                           ParamValue: SQLPOINTER);
Var
  SqlType: SQLSMALLINT;
  ParameterSize: SQLULEN;
  DecimalDigits: SQLSMALLINT;
  Nullable: SQLSMALLINT;
Begin
  Init;

  SqlType:= SQL_CHAR;
  DescribeParam(Param, SqlType, ParameterSize, DecimalDigits, Nullable, False);

  BindParamMain(Param, ParamType, ParamValue, SqlType, ParameterSize, DecimalDigits, Nullable, 0);
End;

Procedure THstmt.BindParam(Param: SQLUSMALLINT;
                           ParamType: SQLSMALLINT;
                           ParamValue: SQLPOINTER;
                           SqlType: SQLSMALLINT);
Var
  ParameterSize: SQLULEN;
  DecimalDigits: SQLSMALLINT;
  Nullable: SQLSMALLINT;
Begin
  Init;

  DescribeParam(Param, SqlType, ParameterSize, DecimalDigits, Nullable, BindCore);

  BindParamMain(Param, ParamType, ParamValue, SqlType, ParameterSize, DecimalDigits, Nullable, 0);
End;

Procedure THstmt.BindParamCore(Param: SQLUSMALLINT;
                               ParamType: SQLSMALLINT;
                               ParamValue: SQLPOINTER;
                               SqlType: SQLSMALLINT);
Var
  ParameterSize: SQLULEN;
  DecimalDigits: SQLSMALLINT;
  Nullable: SQLSMALLINT;
Begin
  Init;

  DescribeParam(Param, SqlType, ParameterSize, DecimalDigits, Nullable, True);

  BindParamMain(Param, ParamType, ParamValue, SqlType, ParameterSize, DecimalDigits, Nullable, 0);
End;

Procedure THstmt.BindParams(Param: SQLUSMALLINT;
                            ParamType: SQLSMALLINT;
                            ParamValue: SQLPOINTER;
                            Bulk: Integer);
Var
  SqlType: SQLSMALLINT;
  ParameterSize: SQLULEN;
  DecimalDigits: SQLSMALLINT;
  Nullable: SQLSMALLINT;
Begin
  Init;

  DescribeParam(Param, SqlType, ParameterSize, DecimalDigits, Nullable, False);

  BindParamMain(Param, ParamType, ParamValue, SqlType, ParameterSize, DecimalDigits, Nullable, Bulk);
End;

Procedure THstmt.BindNull(Param: SQLUSMALLINT);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_DEFAULT;
  BindParam(Param, ParamType, Nil, SQL_CHAR);
End;

Procedure THstmt.BindNullString(Param: SQLUSMALLINT;
                                Var ParamValue: NullString);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_CHAR;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure THstmt.BindString(Param: SQLUSMALLINT;
                            Var ParamValue: String);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_WCHAR;
  BindParam(Param, ParamType, PChar(ParamValue), ColTypeToSqlType(ParamType));
  ParamSize[Param]:= Length(ParamValue) * 2;  //to allow binding memos, but causes re-exec problems
End;

Procedure THstmt.BindSingle(Param: SQLUSMALLINT;
                            Var ParamValue: Single);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_FLOAT;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure THstmt.BindDouble(Param: SQLUSMALLINT;
                            Var ParamValue: Double);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_DOUBLE;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure THstmt.BindShortint(Param: SQLUSMALLINT;
                              Var ParamValue: ShortInt);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_STINYINT;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure THstmt.BindByte(Param: SQLUSMALLINT;
                          Var ParamValue: Byte);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_UTINYINT;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure THstmt.BindSmallint(Param: SQLUSMALLINT;
                              Var ParamValue: SmallInt);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_SSHORT;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure THstmt.BindWord(Param: SQLUSMALLINT;
                          Var ParamValue: Word);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_USHORT;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure THstmt.BindInteger(Param: SQLUSMALLINT;
                             Var ParamValue: Integer);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_SLONG;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure THstmt.BindCardinal(Param: SQLUSMALLINT;
                              Var ParamValue: Cardinal);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_ULONG;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure THstmt.BindLongint(Param: SQLUSMALLINT;
                             Var ParamValue: LongInt);
Begin
  BindInteger(Param, Integer(ParamValue));
End;

Procedure THstmt.BindLongword(Param: SQLUSMALLINT;
                              Var ParamValue: LongWord);
Begin
  BindCardinal(Param, Cardinal(ParamValue));
End;

Procedure THstmt.BindInt64(Param: SQLUSMALLINT;
                           Var ParamValue: Int64);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_SBIGINT;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure THstmt.BindDate(Param: SQLUSMALLINT;
                          Var ParamValue: TDate);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_TYPE_DATE;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure THstmt.BindTime(Param: SQLUSMALLINT;
                          Var ParamValue: TTime);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_TYPE_TIME;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure THstmt.BindTimeStamp(Param: SQLUSMALLINT;
                               Var ParamValue: DateSupport.TTimeStamp);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_TYPE_TIMESTAMP;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure THstmt.BindMemory(Param: SQLUSMALLINT;
                            Var ParamValue: TMemoryStream;
                            Binary: Boolean);
Begin
  If Binary Then
    BindParam(Param, SQL_C_BINARY, ParamValue.Memory, SQL_LONGVARBINARY)
  Else
    BindParam(Param, SQL_C_CHAR, ParamValue.Memory, SQL_LONGVARCHAR);
  If ParamValue.Size = 0 Then  //ParamValue.Memory should be nil
    ParamSize[Param]:= SQL_NULL_DATA
  Else
    ParamSize[Param]:= ParamValue.Size;
End;

Procedure THstmt.BindBinary(Param: SQLUSMALLINT;
                            Var ParamValue: TMemoryStream);
Begin
  BindMemory(Param, ParamValue, True);
End;

Procedure THstmt.BindText(Param: SQLUSMALLINT;
                          Var ParamValue: TMemoryStream);
Begin
  BindMemory(Param, ParamValue, False);
End;

Procedure THstmt.BindNullByName(ParamName: String);
Begin
  BindNull(ParamByName(ParamName));
End;

Procedure THstmt.BindNullStringByName(ParamName: String;
                                      Var ParamValue: NullString);
Begin
  BindNullString(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindStringByName(ParamName: String;
                                  Var ParamValue: String);
Begin
  BindString(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindSingleByName(ParamName: String;
                                  Var ParamValue: Single);
Begin
  BindSingle(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindDoubleByName(ParamName: String;
                                  Var ParamValue: Double);
Begin
  BindDouble(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindShortintByName(ParamName: String;
                                    Var ParamValue: ShortInt);
Begin
  BindShortint(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindByteByName(ParamName: String;
                                Var ParamValue: Byte);
Begin
  BindByte(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindSmallintByName(ParamName: String;
                                    Var ParamValue: SmallInt);
Begin
  BindSmallint(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindWordByName(ParamName: String;
                                Var ParamValue: Word);
Begin
  BindWord(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindIntegerByName(ParamName: String;
                                   Var ParamValue: Integer);
Begin
  BindInteger(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindCardinalByName(ParamName: String;
                                    Var ParamValue: Cardinal);
Begin
  BindCardinal(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindLongintByName(ParamName: String;
                                   Var ParamValue: LongInt);
Begin
  BindLongint(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindLongwordByName(ParamName: String;
                                    Var ParamValue: LongWord);
Begin
  BindLongword(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindInt64ByName(ParamName: String;
                                 Var ParamValue: Int64);
Begin
  BindInt64(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindDateByName(ParamName: String;
                                Var ParamValue: TDate);
Begin
  BindDate(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindTimeByName(ParamName: String;
                                Var ParamValue: TTime);
Begin
  BindTime(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindTimeStampByName(ParamName: String;
                                     Var ParamValue: DateSupport.TTimeStamp);
Begin
  BindTimeStamp(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindMemoryByName(ParamName: String;
                                  Var ParamValue: TMemoryStream;
                                  Binary: Boolean);
Begin
  BindMemory(ParamByName(ParamName), ParamValue, Binary);
End;

Procedure THstmt.BindBinaryByName(ParamName: String;
                                  Var ParamValue: TMemoryStream);
Begin
  BindBinary(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindTextByName(ParamName: String;
                                Var ParamValue: TMemoryStream);
Begin
  BindText(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindNulls(Param: SQLUSMALLINT);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_DEFAULT;
  BindParams(Param, ParamType, Nil, FNumParams);
End;

Procedure THstmt.BindNullStrings(Param: SQLUSMALLINT;
                                 Var ParamValue: Array Of NullString);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_CHAR;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure THstmt.BindSingles(Param: SQLUSMALLINT;
                             Var ParamValue: Array Of Single);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_FLOAT;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure THstmt.BindDoubles(Param: SQLUSMALLINT;
                             Var ParamValue: Array Of Double);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_DOUBLE;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure THstmt.BindShortints(Param: SQLUSMALLINT;
                               Var ParamValue: Array Of ShortInt);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_STINYINT;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure THstmt.BindBytes(Param: SQLUSMALLINT;
                           Var ParamValue: Array Of Byte);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_UTINYINT;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure THstmt.BindSmallints(Param: SQLUSMALLINT;
                               Var ParamValue: Array Of SmallInt);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_SSHORT;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure THstmt.BindWords(Param: SQLUSMALLINT;
                           Var ParamValue: Array Of Word);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_USHORT;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure THstmt.BindIntegers(Param: SQLUSMALLINT;
                              Var ParamValue: Array Of Integer);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_SLONG;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure THstmt.BindCardinals(Param: SQLUSMALLINT;
                               Var ParamValue: Array Of Cardinal);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_ULONG;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure THstmt.BindLongints(Param: SQLUSMALLINT;
                              Var ParamValue: Array Of LongInt);
Var
  ParamType: SQLSMALLINT;
Begin
  //BindIntegers(Param, ParamValue);
  ParamType:= SQL_C_SLONG;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure THstmt.BindLongwords(Param: SQLUSMALLINT;
                               Var ParamValue: Array Of LongWord);
Var
  ParamType: SQLSMALLINT;
Begin
  //BindCardinals(Param, ParamValue);
  ParamType:= SQL_C_ULONG;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure THstmt.BindInt64s(Param: SQLUSMALLINT;
                            Var ParamValue: Array Of Int64);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_SBIGINT;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure THstmt.BindDates(Param: SQLUSMALLINT;
                           Var ParamValue: Array Of TDate);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_TYPE_DATE;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure THstmt.BindTimes(Param: SQLUSMALLINT;
                           Var ParamValue: Array Of TTime);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_TYPE_TIME;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure THstmt.BindTimeStamps(Param: SQLUSMALLINT;
                                Var ParamValue: Array Of DateSupport.TTimeStamp);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_TYPE_TIMESTAMP;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure THstmt.BindNullsByName(ParamName: String);
Begin
  BindNulls(ParamByName(ParamName));
End;

Procedure THstmt.BindNullStringsByName(ParamName: String;
                                       Var ParamValue: Array Of NullString);
Begin
  BindNullStrings(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindSinglesByName(ParamName: String;
                                   Var ParamValue: Array Of Single);
Begin
  BindSingles(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindDoublesByName(ParamName: String;
                                   Var ParamValue: Array Of Double);
Begin
  BindDoubles(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindShortintsByName(ParamName: String;
                                     Var ParamValue: Array Of ShortInt);
Begin
  BindShortints(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindBytesByName(ParamName: String;
                                 Var ParamValue: Array Of Byte);
Begin
  BindBytes(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindSmallintsByName(ParamName: String;
                                     Var ParamValue: Array Of SmallInt);
Begin
  BindSmallints(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindWordsByName(ParamName: String;
                                 Var ParamValue: Array Of Word);
Begin
  BindWords(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindIntegersByName(ParamName: String;
                                    Var ParamValue: Array Of Integer);
Begin
  BindIntegers(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindCardinalsByName(ParamName: String;
                                     Var ParamValue: Array Of Cardinal);
Begin
  BindCardinals(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindLongintsByName(ParamName: String;
                                    Var ParamValue: Array Of LongInt);
Begin
  BindLongints(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindLongwordsByName(ParamName: String;
                                     Var ParamValue: Array Of LongWord);
Begin
  BindLongwords(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindInt64sByName(ParamName: String;
                                  Var ParamValue: Array Of Int64);
Begin
  BindInt64s(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindDatesByName(ParamName: String;
                                 Var ParamValue: Array Of TDate);
Begin
  BindDates(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindTimesByName(ParamName: String;
                                 Var ParamValue: Array Of TTime);
Begin
  BindTimes(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.BindTimeStampsByName(ParamName: String;
                                      Var ParamValue: Array Of DateSupport.TTimeStamp);
Begin
  BindTimeStamps(ParamByName(ParamName), ParamValue);
End;

Procedure THstmt.AsyncEnable(Enabled: Boolean);
Var
  LAsyncEnable: SQLUINTEGER;
Begin
  If Enabled Then
    LAsyncEnable:= SQL_ASYNC_ENABLE_ON
  Else
    LAsyncEnable:= SQL_ASYNC_ENABLE_OFF;

  { Start/End Asynchronous Functionality }
  FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_ASYNC_ENABLE, Pointer(LAsyncEnable), SizeOf(LAsyncEnable));
  If (Not FEnv.Error.Success(FRetCode)) And (Not Enabled) Then
    FEnv.Error.RaiseError(Self, FRetCode);
  FAsyncEnabled:= Enabled;
End;

Procedure THstmt.Execute;
Var
  ParsedSQL: String;
  RetCode: SQLRETURN;
Begin
  Log(1, 'THstmt.Execute');

  Init;

  DoBeforeExecute;

  { Set Bulk Size }
  //depreciated FRetCode:= SQLParamOptions(FHstmt, FNumParams, @irow);
  FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_PARAMSET_SIZE, Pointer(FNumParams), SizeOf(FNumParams));
  If (Not FEnv.Error.Success(FRetCode)) And (FNumParams > 1) Then
    FEnv.Error.RaiseError(Self, FRetCode);

  FExecuted:= False;
  FAborted:= False;
  FAsyncEnabled:= False;
  If FPrepared Then
  Begin
    { Reset Statement Handle }
    If Hdbc.CursorLib = SQL_CUR_USE_DRIVER Then
      CloseCursor;

    { Execute SQL Statement }
    If FExecAsync Then
    Begin
      AsyncEnable(True);
      RetCode:= SQL_STILL_EXECUTING;
      While RetCode = SQL_STILL_EXECUTING Do
      Begin
        RetCode:= SQLExecute(FHstmt);
//        Application.ProcessMessages;
      End;
      FRetCode:= RetCode;
    End
    Else
      FRetCode:= SQLExecute(FHstmt);
  End
  Else
  Begin
    { Parse Parameter SQL }
    ParsedSQL:= ParseSQL;

    { Execute SQL Statement }
    If FExecAsync Then
    Begin
      AsyncEnable(True);
      RetCode:= SQL_STILL_EXECUTING;
      While RetCode = SQL_STILL_EXECUTING Do
      Begin
        RetCode:= SQLExecDirect(FHstmt, Pointer(PChar(ParsedSQL)), SQL_NTS);
//        Application.ProcessMessages;
      End;
      FRetCode:= RetCode;
    End
    Else
      FRetCode:= SQLExecDirect(FHstmt, Pointer(PChar(ParsedSQL)), SQL_NTS);
  End;
  If FAborted Then
  Begin
    AsyncEnable(False);
    FExecuted:= True;
    Abort;
  End;
  If (FRetCode <> SQL_NEED_DATA) And
     (FRetCode <> SQL_NO_DATA) And  //no rows affected by operation
     (Not FEnv.Error.Success(FRetCode)) Then
    FEnv.Error.RaiseError(Self, FRetCode);
  If FExecAsync Then
    AsyncEnable(False);

  { Handle Data-At-Execution Parameters }
  If FRetCode = SQL_NEED_DATA Then
    DataAtExecution(TCommonPtr(FParams));

  If FEnv.Error.Success(FRetCode) Then
    DoAfterExecute;

  { Bind NumRowsFetched }
  FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_ROWS_FETCHED_PTR, @FNumRows, SizeOf(FNumRows));

  FExecuted:= True;
End;

Function THstmt.ColAttrString(Col: SQLUSMALLINT;
                              FieldIdentifier: SQLUSMALLINT): String;
Var
  CharAttr: NullString;
  NumAttr: SQLLEN;
  StringLength: SQLSMALLINT;
Begin
  FRetCode:= SQLColAttribute(FHstmt, Col, FieldIdentifier, @CharAttr, SizeOf(CharAttr), @StringLength, @NumAttr);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);

  Result:= CharAttr;
End;

Function THstmt.ColAttrInteger(Col: SQLUSMALLINT;
                               FieldIdentifier: SQLUSMALLINT): SQLINTEGER;
Var
  CharAttr: NullString;
  NumAttr: SQLLEN;
  StringLength: SQLSMALLINT;
Begin
  FRetCode:= SQLColAttribute(FHstmt, Col, FieldIdentifier, @CharAttr, SizeOf(NumAttr), @StringLength, @NumAttr);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);

  Result:= NumAttr;
End;

Procedure THstmt.BindCol(Col: SQLUSMALLINT;
                         SqlType: SQLSMALLINT);
Var
  temp: TColBindPtr;
Begin
  temp:= ColBindRec(Col);

  If temp = Nil Then
    InsertColBind(Col, SqlType)
  Else
    temp^.FSql:= SqlType;
End;

Procedure THstmt.BindCols;
Var
  FTail: TColPtr;
  temp: TColBindPtr;
  icol: SQLUSMALLINT;
  CType: SQLSMALLINT;
  SqlValue: SQLPOINTER;
  BufferLength: SQLLEN;

  ColumnName: NullString;
  NameLength: SQLSMALLINT;
  SqlType: SQLSMALLINT;
  ColumnSize: SQLULEN;
  DecimalDigits: SQLSMALLINT;
  Nullable: SQLSMALLINT;
Begin
  Log(1, 'THstmt.BindCols');

  If Not FColumnsBound Then
  Begin
    { Get Number of Columns in Result Set }
    if FHdbc.FPlatform = kdbMySQL then
      FNumCols:= ColAttrInteger(1, SQL_DESC_COUNT) // weird requirement?
    else
      FNumCols:= ColAttrInteger(0, SQL_DESC_COUNT);

    { Create Data Structure and Bind Columns }
    FTail:= Nil;
    For icol:= 1 To FNumCols Do
    Begin
      FRetCode:= SQLDescribeCol(FHstmt, icol, @ColumnName, SizeOf(ColumnName), @NameLength,
                                @SqlType, @ColumnSize, @DecimalDigits, @Nullable);
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);

      { Set Column Name }
      FColNames.Add(ColumnName);

      { Set Column Type }
      temp:= ColBindRec(icol);
      If temp <> Nil Then
        SqlType:= temp^.FSql;

      { Driver Translation Done }
      Case SqlType Of
        SQL_WCHAR:
          SqlType:= SQL_CHAR;
        SQL_WVARCHAR:
          SqlType:= SQL_VARCHAR;
        SQL_WLONGVARCHAR:
          SqlType:= SQL_LONGVARCHAR;
        SQL_GUID:
          SqlType:= SQL_BINARY;
        { GDG 22/7/2001 IBM DB2 support }
        SQL_DB2BLOB:
          SqlType:= SQL_LONGVARCHAR;
      End;

      BufferLength:= 0;

      { Create Storage }
      Case SqlType Of
        SQL_CHAR:
        Begin
          CType:= SQL_C_CHAR;
          If FBulkData Then
            BufferLength:= MaxNullString+1
          Else
            BufferLength:= ColumnSize+1;
          GetMem(SqlValue, Integer(FRowSetSize)*BufferLength);
        End;
        SQL_VARCHAR:
        Begin
          CType:= SQL_C_CHAR;
          If FBulkData Or (ColumnSize = 0) Then
            BufferLength:= MaxNullString+1
          Else
            BufferLength:= ColumnSize+1;
          GetMem(SqlValue, Integer(FRowSetSize)*BufferLength);
        End;
        SQL_LONGVARCHAR:
        Begin
          CType:= SQL_C_CHAR;
          SqlValue:= Nil;
        End;

        SQL_BINARY:
        Begin
          CType:= SQL_C_BINARY;
          BufferLength:= ColumnSize;
          GetMem(SqlValue, Integer(FRowSetSize)*BufferLength);
        End;
        SQL_VARBINARY:
        Begin
          CType:= SQL_C_BINARY;
          SqlValue:= Nil;

          //BufferLength:= ColumnSize;
          //GetMem(SqlValue, Integer(FRowSetSize)*BufferLength);
        End;
        SQL_LONGVARBINARY:
        Begin
          CType:= SQL_C_BINARY;
          SqlValue:= Nil;
        End;

        SQL_REAL:
        Begin
          CType:= SQL_C_FLOAT;
          GetMem(SqlValue, FRowSetSize*PhysSize(CType));
        End;
        SQL_DOUBLE:
        Begin
          CType:= SQL_C_DOUBLE;
          GetMem(SqlValue, FRowSetSize*PhysSize(CType));
        End;
        SQL_FLOAT:
        Begin
          CType:= SQL_C_DOUBLE;
          GetMem(SqlValue, FRowSetSize*PhysSize(CType));
        End;
        SQL_DECIMAL:
        Begin
          CType:= SQL_C_DOUBLE;
          BufferLength := 8; // (GM) work around bug in sql 2008 driver 2007.100.1600.22
          GetMem(SqlValue, FRowSetSize*PhysSize(CType));
        End;
        SQL_NUMERIC:
        Begin
          CType:= SQL_C_DOUBLE;
          GetMem(SqlValue, FRowSetSize*PhysSize(CType));
        End;

        SQL_BIT:
        Begin
          CType:= SQL_C_BIT;
          GetMem(SqlValue, FRowSetSize*PhysSize(CType));
        End;
        SQL_TINYINT:
        Begin
          If ColAttrInteger(icol, SQL_DESC_UNSIGNED) = SQL_FALSE Then
            CType:= SQL_C_STINYINT
          Else
            CType:= SQL_C_UTINYINT;
          GetMem(SqlValue, FRowSetSize*PhysSize(CType));
        End;
        SQL_SMALLINT:
        Begin
          If ColAttrInteger(icol, SQL_DESC_UNSIGNED) = SQL_FALSE Then
            CType:= SQL_C_SSHORT
          Else
            CType:= SQL_C_USHORT;
          GetMem(SqlValue, FRowSetSize*PhysSize(CType));
        End;
        SQL_INTEGER:
        Begin
          If ColAttrInteger(icol, SQL_DESC_UNSIGNED) = SQL_FALSE Then
            CType:= SQL_C_SLONG
          Else
            CType:= SQL_C_ULONG;
          GetMem(SqlValue, FRowSetSize*PhysSize(CType));
        End;
        SQL_BIGINT:
        Begin
          If ColAttrInteger(icol, SQL_DESC_UNSIGNED) = SQL_FALSE Then
            CType:= SQL_C_SBIGINT
          Else
            CType:= SQL_C_UBIGINT;
          GetMem(SqlValue, FRowSetSize*PhysSize(CType));
        End;

        SQL_TYPE_DATE:
        Begin
          CType:= SQL_C_TYPE_DATE;
          GetMem(SqlValue, FRowSetSize*PhysSize(CType));
        End;
        SQL_TYPE_TIME:
        Begin
          CType:= SQL_C_TYPE_TIME;
          GetMem(SqlValue, FRowSetSize*PhysSize(CType));
        End;
        SQL_TYPE_TIMESTAMP:
        Begin
          CType:= SQL_C_TYPE_TIMESTAMP;
          GetMem(SqlValue, FRowSetSize*PhysSize(CType));
        End;
        Else
        Begin
          CType:= SQL_C_BINARY;
          SqlValue:= Nil;
        End;
      End;

      { Update Structure }
      InsertTail(FTail, CType, SqlType, SqlValue);
      FTail.FColumnSize:= ColumnSize;
      FTail.FDecimalDigits:= DecimalDigits;
      FTail.FNullable:= Nullable;

      { Bind Column }
      If (SqlType = SQL_LONGVARCHAR) Or (SqlType = SQL_LONGVARBINARY) Or (SqlType = SQL_VARBINARY) Then
        FBlobs:= True
      Else
      Begin
        FRetCode:= SQLBindCol(FHstmt, icol, CType, SqlValue, BufferLength, FTail^.FSize);
        If Not FEnv.Error.Success(FRetCode) Then
          FEnv.Error.RaiseError(Self, FRetCode);
      End;
    End;

    { Add RowStatus Column }
    CType:= SQL_C_USHORT;
    GetMem(SqlValue, FRowSetSize*PhysSize(CType));
    InsertTail(FTail, CType, ColTypeToSqlType(CType), SqlValue);
    SQLUSMALLINT(SqlValue^):= SQL_ROW_SUCCESS;
    FRowStatus:= FTail;

    { Add RowFlags Column }
    CType:= SQL_C_USHORT;
    GetMem(SqlValue, FRowSetSize*PhysSize(CType));
    InsertTail(FTail, CType, ColTypeToSqlType(CType), SqlValue);
    FillChar(SqlValue^, FRowSetSize*PhysSize(CType), rfNone);
    FRowFlags:= FTail;

    { Determine Bookmark Size }
    If (FCursorAttr And SQL_CA1_BOOKMARK) = SQL_CA1_BOOKMARK Then
      FBookmarkSize:= ColAttrInteger(0, SQL_DESC_OCTET_LENGTH);

    { Add Bookmarks Column }
    If FBindBookmarks Then
    Begin
      CType:= SQL_C_VARBOOKMARK;
      BufferLength:= BookmarkSize;
      GetMem(SqlValue, Integer(FRowSetSize)*BufferLength);
      InsertTail(FTail, CType, ColTypeToSqlType(CType), SqlValue);
      FRowBookmark:= FTail;

      FRetCode:= SQLBindCol(FHstmt, 0, CType, SqlValue, BufferLength, FTail^.FSize);
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
    End
    Else
      FRowBookmark:= Nil;

    { Verify FBlobs }
    If FBlobs And (FRowSetSize > 1) Then
      FBlobs:= False;

    { Bind RowStatus }
    //depreciated FRetCode:= SQLExtendedFetch(FHstmt, FetchType, Row, @FNumRows, FRowStatus^.FValue);
    FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_ROW_STATUS_PTR, FRowStatus^.FValue, 0);
  End;

  FColumnsBound:= True;
End;

Procedure THstmt.BindBlobCols(Bind: Boolean);
Var
  icol: SQLUSMALLINT;
  tempCol: TColPtr;
  LongDataLen: String;
  BufferLength: SQLLEN;
Begin
  tempCol:= ColRec(1);
  For icol:= 1 To ColCount Do
  Begin
    If tempCol^.FBlob Then
    Begin
      If Bind Then
        tempCol^.FSize^:= ColSize[icol];  //synchronize sizes

      BufferLength:= 0;
      LongDataLen:= Hdbc.GetInfoString(SQL_NEED_LONG_DATA_LEN);
      If (LongDataLen <> '') And (LongDataLen[1] = 'Y') Then
        tempCol^.FSize^:= SQL_LEN_DATA_AT_EXEC_OFFSET-tempCol^.FSize^
      Else If Bind Then
      Begin
        If tempCol^.FType = SQL_C_BINARY Then
          BufferLength:= tempCol^.FSize^
        Else
          BufferLength:= tempCol^.FSize^+1;
      End;

      If Bind Then
        tempCol^.FValue:= tempCol^.FMemory.Memory
      Else
        tempCol^.FValue:= Nil;
      FRetCode:= SQLBindCol(FHstmt, icol, tempCol^.FType, tempCol^.FValue, BufferLength, tempCol^.FSize);
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
    End;

    tempCol:= tempCol^.Next;
  End;
End;

Function THstmt.FetchCol(Col: SQLUSMALLINT;
                         ColType: SQLSMALLINT;
                         ColStream: TStream): SQLINTEGER;
Var
  ColSize: SQLLEN;
  Buffer: Pointer;
  BSize: LongInt;
  TotalSize: SQLLEN;
  GCursor: LongInt;
  temp: SQLLEN;
Begin
  GetMem(Buffer, FBlobSize+1);

  Try

  { Determine Total Size }
  BSize:= FBlobSize;
  If ColType = SQL_CHAR Then
    TotalSize:= BSize+1
  Else
    TotalSize:= BSize;

  FRetCode:= SQLGetData(FHstmt, Col, ColType, Buffer, TotalSize, @ColSize);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
  If ColStream Is TMemoryStream Then
    TMemoryStream(ColStream).Clear;
  If ColSize < BSize Then
    ColStream.Write(Buffer^, ColSize)
  Else
    ColStream.Write(Buffer^, BSize);

  If ColSize = SQL_NO_TOTAL Then
    Raise EODBCExpress.Create('Unable to retrieve blob.  You may have to increase the BlobSize property.');

  If ColSize <> SQL_NULL_DATA Then
  Begin
    { Retrieve Blob }
    If FRetCode = SQL_SUCCESS_WITH_INFO Then
    Begin
      GCursor:= BSize;
      While FRetCode = SQL_SUCCESS_WITH_INFO Do
      Begin
        If (ColSize - GCursor) > FBlobSize Then
          BSize:= FBlobSize
        Else
          BSize:= ColSize - GCursor;

        { Determine Total Size }
        If ColType = SQL_CHAR Then
          TotalSize:= BSize+1
        Else
          TotalSize:= BSize;

        FRetCode:= SQLGetData(FHstmt, Col, ColType, Buffer, TotalSize, @temp);
        ColStream.Write(Buffer^, BSize);
        GCursor:= GCursor + BSize;
      End;
    End;
    If Not FEnv.Error.Success(FRetCode) Then
      FEnv.Error.RaiseError(Self, FRetCode);
  End;

  Finally
    FreeMem(Buffer, FBlobSize+1);
  End;

  Result:= ColSize;
End;

Function THstmt.FetchCell(Col, Row: SQLUSMALLINT;
                          ColType: SQLSMALLINT;
                          ColStream: TStream): SQLINTEGER;
Var
  temp: TColPtr;
Begin
  Result:= CellSize[Col,Row];
  If FRowSetSize > 1 Then
  Begin
    If (FCursorAttr And SQL_CA1_POS_POSITION) = SQL_CA1_POS_POSITION Then
    Begin
      FRetCode:= SQLSetPos(FHstmt, Row, SQL_POSITION, SQL_LOCK_NO_CHANGE);
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
    End
    Else
      Exit;

    If Not RowValid[Row] Then
    Begin
      Result:= SQL_NULL_DATA;
      Exit;
    End;

    Try
      Result:= FetchCol(Col, ColType, ColStream);
    Except
      On E: EODBC Do  //skip "already fetched" exception
        If EODBC(E).RetCode <> SQL_NO_DATA Then
          Raise;
    End;
  End
  Else
  Begin
    If Not RowValid[1] Then
    Begin
      Result:= SQL_NULL_DATA;
      Exit;
    End;

    temp:= ColRec(Col);
    If (temp <> Nil) And (Not temp^.FBlobFetched) Then
    Begin
      Result:= FetchCol(Col, ColType, ColStream);
      temp^.FBlobFetched:= True;
    End;
  End;
End;

Procedure THstmt.DataAtExecution(FList: TCommonPtr);
Var
  BSize: SQLLEN;
  temp: TCommonPtr;
  PSize: LongInt;
  PValue: Pointer;
  PCursor: LongInt;
  OffsetPtr: Pointer;
Begin
  FRetCode:= SQLParamData(FHstmt, @PValue);
  If (FRetCode <> SQL_NEED_DATA) And (Not FEnv.Error.Success(FRetCode)) Then
    FEnv.Error.RaiseError(Self, FRetCode);

  Repeat
    { Retrieve Blob Information }
    temp:= FList;
    While temp^.FValue <> PValue Do
      temp:= temp^.Next;
    PSize:= SQL_LEN_DATA_AT_EXEC_OFFSET-temp^.FSize^;

    { Write Blob }
    PCursor:= 0;
    Repeat
      If (PSize - PCursor) > FBlobSize Then
        BSize:= FBlobSize
      Else
        BSize:= PSize - PCursor;
      OffsetPtr:= OffsetPointer(PValue, PCursor);
      FRetCode:= SQLPutData(FHstmt, OffsetPtr, BSize);
      PCursor:= PCursor + BSize;
    Until (PCursor = PSize) Or (Not FEnv.Error.Success(FRetCode));
    If Not FEnv.Error.Success(FRetCode) Then
      FEnv.Error.RaiseError(Self, FRetCode);

    FRetCode:= SQLParamData(FHstmt, @PValue);
    If (FRetCode <> SQL_NEED_DATA) And
       (FRetCode <> SQL_NO_DATA)   And  //no rows affected by operation
       (Not FEnv.Error.Success(FRetCode)) Then
      FEnv.Error.RaiseError(Self, FRetCode);
  Until FRetCode <> SQL_NEED_DATA;
End;

Function THstmt.Fetch(FetchType: SQLSMALLINT;
                      Row: SQLINTEGER): Boolean;
Var
  { Global Column }
  icol: SQLUSMALLINT;
  temp: TColPtr;
Begin
  Log(1, 'THstmt.Fetch');

  Init;

  DoBeforeFetch;

  { Bind Columns }
  BindCols;

  { Fetch Next Row in Result Set }
  If (FetchType = SQL_FETCH_NEXT) And (Hdbc.CursorLib = SQL_CUR_USE_DRIVER) Then
    FRetCode:= SQLFetch(FHstmt)
  Else
    FRetCode:= SQLFetchScroll(FHstmt, FetchType, Row);

  FillChar(FRowFlags^.FValue^, FRowSetSize*PhysSize(SQL_C_USHORT), rfNone);
  If FRetCode = SQL_NO_DATA Then
  Begin
    Result:= False;
    FNumRows:= 0;

    Exit;
  End;
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);

  { Fetch Unbound Columns in Result Set }
  If FBlobs Then
  Begin
    icol:= 0;
    temp:= ColRec(1);
    While temp <> Nil Do
    Begin
      Inc(icol);
      If temp^.FBlob Then
      Begin
        temp^.FBlobFetched:= False;
        If FBlobDeferral Then
          temp^.FSize^:= SQL_NULL_DATA
        Else
          temp^.FSize^:= FetchCell(icol, 1, temp^.FType, temp^.FMemory);
      End;
      temp:= temp^.Next;
    End;
  End;

  DoAfterFetch;

  Result:= True;
End;

Function THstmt.FetchFirst: Boolean;
Begin
  Result:= Fetch(SQL_FETCH_FIRST, 0);
End;

Function THstmt.FetchNext: Boolean;
Begin
  Result:= Fetch(SQL_FETCH_NEXT, 0);
End;

Function THstmt.FetchLast: Boolean;
Begin
  Result:= Fetch(SQL_FETCH_LAST, 0);
End;

Function THstmt.FetchPrev: Boolean;
Begin
  Result:= Fetch(SQL_FETCH_PRIOR, 0);
End;

Function THstmt.FetchAbsolute(Row: SQLINTEGER): Boolean;
Begin
  Result:= Fetch(SQL_FETCH_ABSOLUTE, Row);
End;

Function THstmt.FetchRelative(Row: SQLINTEGER): Boolean;
Begin
  Result:= Fetch(SQL_FETCH_RELATIVE, Row);
End;

Function THstmt.FetchBookmark(Bookmark: SQLPOINTER): Boolean;
Begin
  FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_FETCH_BOOKMARK_PTR, Bookmark, BookmarkSize);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);

  Result:= Fetch(SQL_FETCH_BOOKMARK, 0);
End;

Function THstmt.MoreResults: Boolean;
Begin
  FRetCode:= SQLMoreResults(FHstmt);
  If (Not FEnv.Error.Success(FRetCode)) And (FRetCode <> SQL_NO_DATA) Then
    FEnv.Error.RaiseError(Self, FRetCode);

  Result:= FRetCode <> SQL_NO_DATA;
  If Result Then
    FreeCols;
End;

Procedure THstmt.AbortQuery;
Begin
  If Not FAborted Then
  Begin
    FAborted:= True;
    SQLCancel(FHstmt);  //errors ignored
  End;
End;

Procedure THstmt.ColStream(Col: SQLUSMALLINT;
                           Stream: TStream);
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);
  If (temp <> Nil) And temp^.FBlob Then
  Begin
    If FBlobDeferral Then
      temp^.FSize^:= FetchCell(Col, 1, temp^.FType, Stream)
    Else
      temp^.FMemory.SaveToStream(Stream);
  End;
End;

Procedure THstmt.CellStream(Col, Row: SQLUSMALLINT;
                            Stream: TStream);
Var
  tempCol: TColPtr;
  //temp: TRowPtr;
  LrRowRec : TRowRec;
Begin
  //Alternative Semi-Optimal implementation
  
  If RowRecEx(Col, Row, LrRowRec) Then
    Begin
    If LrRowRec.FBlob Then
      Begin
      tempCol:= ColRec(Col);
      tempCol^.FSize^:= FetchCell(Col, Row, LrRowRec.FType, Stream);
      End;
    End;

{
Previous Non-Optimal implementation
  temp:= RowRec(Col,Row);
  if temp <> nil then
  begin
    if temp^.FBlob then
    begin
      tempCol:= ColRec(Col);
      tempCol^.FSize^:= FetchCell(Col, Row, temp^.FType, Stream);
    end;
    Dispose(temp);
  end;
}
End;

Procedure THstmt.DoBeforePrepare;
Begin
  If Assigned(FBeforePrepare) Then
    FBeforePrepare(Self);
End;

Procedure THstmt.DoAfterPrepare;
Begin
  If Assigned(FAfterPrepare) Then
    FAfterPrepare(Self);
End;

Procedure THstmt.DoBeforeExecute;
Begin
  If Assigned(FBeforeExecute) Then
    FBeforeExecute(Self);
End;

Procedure THstmt.DoAfterExecute;
Begin
  If Assigned(FAfterExecute) Then
    FAfterExecute(Self);
End;

Procedure THstmt.DoBeforeFetch;
Begin
  If Assigned(FBeforeFetch) Then
    FBeforeFetch(Self);
End;

Procedure THstmt.DoAfterFetch;
Begin
  If Assigned(FAfterFetch) Then
    FAfterFetch(Self);
End;

Function THstmt.DoRowCount: Integer;
Begin
  Result:= -1;
  If Assigned(FOnRowCount) Then
    Result:= FOnRowCount(Self);
End;

Function THstmt.GetPosOpts: SQLINTEGER;
Begin
  If SkipByPosition Then
    Result:= 0
  Else
    //depreciated Result:= Hdbc.FPosOpts;
    Result:= FCursorAttr;
End;

Function THstmt.GetPosStmts: SQLINTEGER;
Begin
  If SkipByCursor Then
    Result:= 0
  Else
    //depreciated Result:= Hdbc.FPosStmts;
    Result:= FCursorAttr;
End;

Procedure THstmt.DetermineTargetTable;
Var
  CharAttr: NullString;
  NumAttr: SQLLEN;
  StringLength: SQLSMALLINT;
  Loc: Integer;
Begin
  { Determine Target Table }
  If Trim(FTargetTable) = '' Then
  Begin
    FRetCode:= SQLColAttribute(FHstmt, 1, SQL_DESC_SCHEMA_NAME, @CharAttr, SizeOf(CharAttr), @StringLength, @NumAttr);
    FTableOwner:= Trim(CharAttr);
    If Not FEnv.Error.Success(FRetCode) Then
      FTableOwner:= '';

    FRetCode:= SQLColAttribute(FHstmt, 1, SQL_DESC_TABLE_NAME, @CharAttr, SizeOf(CharAttr), @StringLength, @NumAttr);
    FTableName:= Trim(CharAttr);
    If FTableName = '' Then
    Begin
      FTableName:= StringReplace(FSQL, EnterString, ' ', [rfReplaceAll, rfIgnoreCase]);
      Loc:= Pos(' FROM ', UpperCase(FTableName));
      If Loc > 0 Then
      Begin
        FTableName:= Trim(Copy(FTableName, Loc+6, Length(FTableName)-Loc-5));
        Loc:= Pos(' ', FTableName);
        If Loc > 0 Then
          FTableName:= Copy(FTableName, 1, Loc-1);  //can include table owner
      End
      Else
        FTableName:= '';
    End;
    If (Not FEnv.Error.Success(FRetCode)) Or (FTableName = '') Then
      Raise EODBCExpress.Create('Unable to determine table name:  set TargetTable property.');
  End
  Else
  Begin
    FTableOwner:= '';
    FTableName:= Trim(FTargetTable);
  End;
End;

Procedure THstmt.DeterminePrimaryCols;
Var
  icol: SQLUSMALLINT;
  tempCol: TColPtr;
  tempHstmt: THstmt;
  ATableOwner, ATableName: Pointer;
  Found: Boolean;
Begin
  { Determine Primary Columns }
  tempCol:= ColRec(1);
  For icol:= 1 To ColCount Do
  Begin
    If tempCol^.FPrimary Then
      Exit;

    tempCol:= tempCol^.Next;
  End;

  DetermineTargetTable;

  tempHstmt:= THstmt.Create(FEnv, FHdbc);

  Try

    If FTableOwner = '' Then
      ATableOwner:= Nil
    Else
      ATableOwner:= PChar(FTableOwner);

    If FTableName = '' Then
      ATableName:= Nil
    Else
      ATableName:= PChar(FTableName);

    FRetCode:= SQLSpecialColumns(tempHstmt.Handle, SQL_BEST_ROWID, Nil, 0, ATableOwner, Length(FTableOwner),
      ATableName, Length(FTableName), SQL_SCOPE_CURROW, SQL_NULLABLE);
    If Not FEnv.Error.Success(FRetCode) Then
      FEnv.Error.RaiseError(tempHstmt, FRetCode);

    Found:= False;
    While tempHstmt.FetchNext Do
      If tempHstmt.ColSmallint[8] <> SQL_PC_PSEUDO Then
      Begin
        ColPrimary[ColByName(tempHstmt.ColString[2])]:= True;
        Found:= True;
      End;

    If Not Found Then
    Begin
      tempHstmt.Close;

      FRetCode:= SQLPrimaryKeys(tempHstmt.Handle, Nil, 0, ATableOwner, Length(FTableOwner),
        ATableName, Length(FTableName));
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(tempHstmt, FRetCode);

      While tempHstmt.FetchNext Do
        ColPrimary[ColByName(tempHstmt.ColString[4])]:= True;
    End;

  Finally
    tempHstmt.Free;
  End;
End;

Procedure THstmt.DetermineIgnoreCols;
Var
  icol: SQLUSMALLINT;
  tempCol: TColPtr;
Begin
  { Determine Ignore Columns }
  tempCol:= ColRec(1);
  For icol:= 1 To ColCount Do
  Begin
    If ColAttrInteger(icol, SQL_DESC_UPDATABLE) = SQL_ATTR_READONLY Then
      tempCol^.FSize^:= SQL_IGNORE;

    tempCol:= tempCol^.Next;
  End;
End;

Function THstmt.GetTableOwner: String;
Begin
  DetermineTargetTable;

  Result:= FTableOwner;
End;

Function THstmt.GetTableName: String;
Begin
  DetermineTargetTable;

  Result:= FTableName;
End;

Function THstmt.GetPrimaryColNames: String;
Var
  icol: SQLUSMALLINT;
  tempCol: TColPtr;
Begin
  DeterminePrimaryCols;

  Result:= '';
  tempCol:= ColRec(1);
  For icol:= 1 To ColCount Do
  Begin
    If tempCol^.FPrimary Then
      Result:= Result+ColNames[icol-1]+';';

    tempCol:= tempCol^.Next;
  End;
End;

Function THstmt.GetIgnoreColNames: String;
Var
  icol: SQLUSMALLINT;
  tempCol: TColPtr;
Begin
  DetermineIgnoreCols;

  Result:= '';
  tempCol:= ColRec(1);
  For icol:= 1 To ColCount Do
  Begin
    If tempCol^.FSize^ = SQL_IGNORE Then
      Result:= Result+ColNames[icol-1]+';';

    tempCol:= tempCol^.Next;
  End;
End;

Function THstmt.CursorName: String;
Var
  CurName: NullString;
  StringLength: SQLSMALLINT;
Begin
  { Determine Cursor Name }
  FRetCode:= SQLGetCursorName(FHstmt, @CurName, SizeOf(CurName), @StringLength);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);

  Result:= CurName;
End;

Function THstmt.PrimaryClause: String;
Var
  icol: SQLUSMALLINT;
  tempCol: TColPtr;
Begin
  DeterminePrimaryCols;

  Result:= '';
  tempCol:= ColRec(1);
  For icol:= 1 To ColCount Do
  Begin
    If tempCol^.FPrimary Then
      Result:= Result+Quoted(FColNames[icol-1])+' = ? AND ';

    tempCol:= tempCol^.Next;
  End;

  If Result = '' Then
    Raise EODBCExpress.Create('Unable to determine primary columns:  set PrimaryCols property.')
  Else
    SetLength(Result, Length(Result)-5);
End;

Procedure THstmt.BindClause(Var Param: SQLUSMALLINT;
                            AHstmt: THstmt;
                            PrimaryClause: Boolean;
                            AssignOnly: Boolean);
Var
  icol: SQLUSMALLINT;
  tempCol: TColPtr;

  Function PrimaryParamSize: SQLINTEGER;
  Begin
    //to avoid "invalid string or buffer length" error caused by SQL_IGNORE
    If tempCol^.FSize^ >= 0 Then
      Result:= tempCol^.FSize^
    Else If tempCol^.FSize^ = SQL_NULL_DATA Then
      Result:= SQL_NULL_DATA
    Else If tempCol^.FType = SQL_C_CHAR Then
      Result:= SQL_NTS
    Else If tempCol^.FType = SQL_C_BINARY Then
      Result:= 0  //unable to determine actual size
    Else
      Result:= 0;
  End;

Begin
  tempCol:= ColRec(1);
  For icol:= 1 To ColCount Do
  Begin
    If (PrimaryClause And tempCol^.FPrimary) Or
       ((Not PrimaryClause) And (tempCol^.FSize^ <> SQL_IGNORE)) Then
    Begin
      Inc(Param);

      If tempCol^.FBlob Then
        AHstmt.BindMemory(Param, tempCol^.FMemory, tempCol^.FType = SQL_C_BINARY)
      Else
      Begin
        If Not AssignOnly Then
          AHstmt.BindParam(Param, tempCol^.FType, tempCol^.FValue, tempCol^.FSql);
        If PrimaryClause Then
          AHstmt.ParamSize[Param]:= PrimaryParamSize
        Else
          AHstmt.ParamSize[Param]:= tempCol^.FSize^;
      End;

      If tempCol^.FSize^ = SQL_NULL_DATA Then
        AHstmt.ParamSize[Param]:= SQL_NULL_DATA;
    End;

    tempCol:= tempCol^.Next;
  End;
End;

Procedure THstmt.InsertFields;
Var
  icol: SQLUSMALLINT;
  tempCol: TColPtr;
  ASQL, temp: String;
  ParamNum: SQLUSMALLINT;
Begin
  If FHstmtInsert = Nil Then
  Begin
    FHstmtInsert:= THstmt.Create(FEnv, FHdbc);
    FHstmtInsert.BlobSize:= BlobSize;
  End;

  If Not FHstmtInsert.Prepared Then
  Begin
    ASQL:= '';
    temp:= '';
    tempCol:= ColRec(1);
    For icol:= 1 To ColCount Do
    Begin
      If tempCol^.FSize^ <> SQL_IGNORE Then
      Begin
        ASQL:= ASQL+Quoted(FColNames[icol-1])+', ';
        temp:= temp+'?, ';
      End;

      tempCol:= tempCol^.Next;
    End;

    If ASQL <> '' Then
    Begin
      SetLength(ASQL, Length(ASQL)-2);
      SetLength(temp, Length(temp)-2);
      ASQL:= 'INSERT INTO '+Quoted(TableName)+' ('+ASQL+') VALUES ('+temp+')';

      If Assigned(FOnStatement) Then
        FOnStatement(Self, rfInsert, ASQL);

      FHstmtInsert.SQL:= ASQL;
      FHstmtInsert.Prepare;

      ParamNum:= 0;
      BindClause(ParamNum, FHstmtInsert, False, False);
    End;
  End
  Else
  Begin
    ParamNum:= 0;
    BindClause(ParamNum, FHstmtInsert, False, True);
  End;

  FHstmtInsert.Execute;

  If (FHstmtInsert.RowsAffected = 0) And (nrByInsert In NoRowsAffected) Then
    FEnv.Error.RaiseError(Self, SQL_NO_DATA);
End;

Procedure THstmt.UpdateFields(WhereClause: String;
                              BindWhere: Boolean);
Var
  icol: SQLUSMALLINT;
  tempCol: TColPtr;
  ASQL: String;
  ParamNum: SQLUSMALLINT;
Begin
  If FHstmtUpdate = Nil Then
  Begin
    FHstmtUpdate:= THstmt.Create(FEnv, FHdbc);
    FHstmtUpdate.BlobSize:= BlobSize;
  End;

  If Not FHstmtUpdate.Prepared Then
  Begin
    ASQL:= '';
    tempCol:= ColRec(1);
    For icol:= 1 To ColCount Do
    Begin
      If tempCol^.FSize^ <> SQL_IGNORE Then
        ASQL:= ASQL+Quoted(FColNames[icol-1])+' = ?, ';

      tempCol:= tempCol^.Next;
    End;

    If ASQL <> '' Then
    Begin
      SetLength(ASQL, Length(ASQL)-2);
      ASQL:= 'UPDATE '+Quoted(TableName)+' SET '+ASQL+' '+WhereClause;

      If Assigned(FOnStatement) Then
        FOnStatement(Self, rfUpdate, ASQL);

      FHstmtUpdate.SQL:= ASQL;
      FHstmtUpdate.Prepare;

      ParamNum:= 0;
      BindClause(ParamNum, FHstmtUpdate, False, False);
      If BindWhere Then
        BindClause(ParamNum, FHstmtUpdate, True, False);
    End;
  End
  Else
  Begin
    ParamNum:= 0;
    BindClause(ParamNum, FHstmtUpdate, False, True);
    If BindWhere Then
      BindClause(ParamNum, FHstmtUpdate, True, True);
  End;

  FHstmtUpdate.Execute;

  If (FHstmtUpdate.RowsAffected = 0) And (nrByUpdate In NoRowsAffected) Then
    FEnv.Error.RaiseError(Self, SQL_NO_DATA);
End;

Procedure THstmt.DeleteFields(WhereClause: String;
                              BindWhere: Boolean);
Var
  ASQL: String;
  ParamNum: SQLUSMALLINT;
Begin
  If FHstmtDelete = Nil Then
  Begin
    FHstmtDelete:= THstmt.Create(FEnv, FHdbc);
    FHstmtDelete.BlobSize:= BlobSize;
  End;

  If Not FHstmtDelete.Prepared Then
  Begin
    ASQL:= 'DELETE FROM '+Quoted(TableName)+' '+WhereClause;

    If Assigned(FOnStatement) Then
      FOnStatement(Self, rfDelete, ASQL);

    FHstmtDelete.SQL:= ASQL;
    FHstmtDelete.Prepare;

    ParamNum:= 0;
    If BindWhere Then
      BindClause(ParamNum, FHstmtDelete, True, False);
  End
  Else
  Begin
    ParamNum:= 0;
    If BindWhere Then
      BindClause(ParamNum, FHstmtDelete, True, True);
  End;

  FHstmtDelete.Execute;

  If (FHstmtDelete.RowsAffected = 0) And (nrByDelete In NoRowsAffected) Then
    FEnv.Error.RaiseError(Self, SQL_NO_DATA);
End;

Procedure THstmt.RefreshFields(WhereClause: String);
Var
  icol: SQLUSMALLINT;
  tempCol: TColPtr;
  ASQL: String;
  ParamNum: SQLUSMALLINT;
  Size: Word;
Begin
  If FHstmtRefresh = Nil Then
  Begin
    FHstmtRefresh:= THstmt.Create(FEnv, FHdbc);
    FHstmtRefresh.BlobSize:= BlobSize;
  End;

  If Not FHstmtRefresh.Prepared Then
  Begin
    ASQL:= '';
    tempCol:= ColRec(1);
    For icol:= 1 To ColCount Do
    Begin
      ASQL:= ASQL+Quoted(FColNames[icol-1])+', ';

      tempCol:= tempCol^.Next;
    End;

    If ASQL <> '' Then
    Begin
      SetLength(ASQL, Length(ASQL)-2);
      ASQL:= 'SELECT '+ASQL+' FROM '+Quoted(TableName)+' '+WhereClause;

      If Assigned(FOnStatement) Then
        FOnStatement(Self, rfRefresh, ASQL);

      FHstmtRefresh.SQL:= ASQL;
      FHstmtRefresh.Prepare;

      ParamNum:= 0;
      BindClause(ParamNum, FHstmtRefresh, True, False);
    End;
  End
  Else
  Begin
    ParamNum:= 0;
    BindClause(ParamNum, FHstmtRefresh, True, True);
  End;

  FHstmtRefresh.Execute;

  If FHstmtRefresh.FetchNext Then
  Begin
    tempCol:= ColRec(1);
    For icol:= 1 To ColCount Do
    Begin
      If tempCol^.FBlob Then
        tempCol^.FMemory.LoadFromStream(FHstmtRefresh.ColMemory[icol])
      Else
      Begin
        If (tempCol^.FSql = SQL_CHAR) Or (tempCol^.FSql = SQL_VARCHAR) Then
          Size:= tempCol^.FColumnSize+1
        Else If (tempCol^.FSql = SQL_BINARY) Or (tempCol^.FSql = SQL_VARBINARY) Then
          Size:= tempCol^.FColumnSize
        Else
          Size:= PhysSize(tempCol^.FType);

        Move(FHstmtRefresh.ColValue[icol]^, tempCol^.FValue^, Size);
      End;

      tempCol^.FSize^:= FHstmtRefresh.ColSize[icol];

      tempCol:= tempCol^.Next;
    End;
  End
  Else If nrByRefresh In NoRowsAffected Then
    FEnv.Error.RaiseError(Self, SQL_NO_DATA);
End;

{ ActionRow }

Procedure THstmt.InsertRow(Row: SQLUSMALLINT);
Begin
  If (GetPosOpts And SQL_CA1_BULK_ADD) = SQL_CA1_BULK_ADD Then
  Begin
    BindBlobCols(True);

    //depreciated FRetCode:= SQLSetPos(FHstmt, Row, SQL_ADD, SQL_LOCK_NO_CHANGE);
    FRetCode:= SQLBulkOperations(FHstmt, SQL_ADD);
    If (Not FEnv.Error.Success(FRetCode)) And
       (FRetCode <> SQL_NEED_DATA) And
       ((FRetCode <> SQL_NO_DATA) Or ((FRetCode = SQL_NO_DATA) And (nrByInsert In NoRowsAffected))) Then
      FEnv.Error.RaiseError(Self, FRetCode);

    { Handle Data-At-Execution Parameters }
    If FRetCode = SQL_NEED_DATA Then
      DataAtExecution(TCommonPtr(FCols));

    BindBlobCols(False);
  End
  Else
    InsertFields;
End;

Procedure THstmt.UpdateRow(Row: SQLUSMALLINT);
Var
  BindWhere: Boolean;

  Function WhereClause: String;
  Begin
    BindWhere:= Not ((GetPosStmts And SQL_CA1_POSITIONED_UPDATE) = SQL_CA1_POSITIONED_UPDATE);
    If BindWhere Then
      Result:= 'WHERE '+PrimaryClause
    Else
      Result:= 'WHERE CURRENT OF '+CursorName;
  End;

Begin
  DetermineIgnoreCols;  //non-updatable columns

  If (GetPosOpts And SQL_CA1_POS_UPDATE) = SQL_CA1_POS_UPDATE Then
  Begin
    BindBlobCols(True);

    FRetCode:= SQLSetPos(FHstmt, Row, SQL_UPDATE, SQL_LOCK_NO_CHANGE);
    If (Not FEnv.Error.Success(FRetCode)) And
       (FRetCode <> SQL_NEED_DATA) And
       ((FRetCode <> SQL_NO_DATA) Or ((FRetCode = SQL_NO_DATA) And (nrByUpdate In NoRowsAffected))) Then
      FEnv.Error.RaiseError(Self, FRetCode);

    { Handle Data-At-Execution Parameters }
    If FRetCode = SQL_NEED_DATA Then
      DataAtExecution(TCommonPtr(FCols));

    BindBlobCols(False);
  End
  Else
    UpdateFields(WhereClause, BindWhere);
End;

Procedure THstmt.DeleteRow(Row: SQLUSMALLINT);
Var
  BindWhere: Boolean;

  Function WhereClause: String;
  Begin
    BindWhere:= Not ((GetPosStmts And SQL_CA1_POSITIONED_DELETE) = SQL_CA1_POSITIONED_DELETE);
    If BindWhere Then
      Result:= 'WHERE '+PrimaryClause
    Else
      Result:= 'WHERE CURRENT OF '+CursorName;
  End;

Begin
  If (GetPosOpts And SQL_CA1_POS_DELETE) = SQL_CA1_POS_DELETE Then
  Begin
    BindBlobCols(True);

    FRetCode:= SQLSetPos(FHstmt, Row, SQL_DELETE, SQL_LOCK_NO_CHANGE);
    If (Not FEnv.Error.Success(FRetCode)) And
       (FRetCode <> SQL_NEED_DATA) And
       ((FRetCode <> SQL_NO_DATA) Or ((FRetCode = SQL_NO_DATA) And (nrByDelete In NoRowsAffected))) Then
      FEnv.Error.RaiseError(Self, FRetCode);

    { Handle Data-At-Execution Parameters }
    If FRetCode = SQL_NEED_DATA Then
      DataAtExecution(TCommonPtr(FCols));

    BindBlobCols(False);
  End
  Else
    DeleteFields(WhereClause, BindWhere);
End;

Procedure THstmt.RefreshRow(Row: SQLUSMALLINT);

  Function WhereClause: String;
  Begin
    Result:= 'WHERE '+PrimaryClause;
  End;

Begin
  If (GetPosOpts And SQL_CA1_POS_REFRESH) = SQL_CA1_POS_REFRESH Then
  Begin
    FRetCode:= SQLSetPos(FHstmt, Row, SQL_REFRESH, SQL_LOCK_NO_CHANGE);
    If (Not FEnv.Error.Success(FRetCode)) And
       ((FRetCode <> SQL_NO_DATA) Or ((FRetCode = SQL_NO_DATA) And (nrByRefresh In NoRowsAffected))) Then
      FEnv.Error.RaiseError(Self, FRetCode);
  End
  Else
    RefreshFields(WhereClause);
End;

{ ActionRows }

Procedure THstmt.InsertRows;
Var
  i: Integer;
Begin
  For i:= 1 To FNumRows Do
    If RowFlag[i] = rfInsert Then
      InsertRow(i);
End;

Procedure THstmt.UpdateRows;
Var
  i: Integer;
Begin
  For i:= 1 To FNumRows Do
    If RowFlag[i] = rfUpdate Then
      UpdateRow(i);
End;

Procedure THstmt.DeleteRows;
Var
  i: Integer;
Begin
  For i:= 1 To FNumRows Do
    If RowFlag[i] = rfDelete Then
      DeleteRow(i);
End;

Procedure THstmt.RefreshRows;
Var
  i: Integer;
Begin
  For i:= 1 To FNumRows Do
    If RowFlag[i] = rfRefresh Then
      RefreshRow(i);
End;

{ DoAction }

Procedure THstmt.DoInsert;
Begin
  Init;

  If (Not Assigned(FOnInsert)) Or
     (Assigned(FOnInsert) And FOnInsert(Self, 'Insert row(s)?')) Then
    Begin
      If (RowSetSize > 1) And (FNumRows > 0) Then
        InsertRows
      Else
        InsertRow(1);
    End;
End;

Procedure THstmt.DoUpdate;
Begin
  Init;

  If (Not Assigned(FOnUpdate)) Or
     (Assigned(FOnUpdate) And FOnUpdate(Self, 'Update row(s)?')) Then
    Begin
      If (RowSetSize > 1) And (FNumRows > 0) Then
        UpdateRows
      Else
        UpdateRow(1);
    End;
End;

Procedure THstmt.DoDelete;
Begin
  Init;

  If (Not Assigned(FOnDelete)) Or
     (Assigned(FOnDelete) And FOnDelete(Self, 'Delete row(s)?')) Then
    Begin
      If (RowSetSize > 1) And (FNumRows > 0) Then
        DeleteRows
      Else
        DeleteRow(1);
    End;
End;

Procedure THstmt.DoRefresh;
Begin
  Init;

  If (Not Assigned(FOnRefresh)) Or
     (Assigned(FOnRefresh) And FOnRefresh(Self, 'Refresh row(s)?')) Then
    Begin
      If (RowSetSize > 1) And (FNumRows > 0) Then
        RefreshRows
      Else
        RefreshRow(1);
    End;
End;

Function THstmt.GetColValue(Col: SQLUSMALLINT): SQLPOINTER;
Begin
  Result:= GetCellValue(Col, 1);
End;

Function THstmt.GetCellValue(Col, Row: SQLUSMALLINT): SQLPOINTER;
Var
  //temp: TRowPtr;
  LrRowRec : TRowRec;
Begin
  //Alternative Semi-Optimal implementation
  
  If RowRecEx(Col, Row, LrRowRec) Then
    Begin
    If LrRowRec.FBlob Then
      Begin
      Result:= GetCellMemory(Col, Row).Memory
      End
    Else
      Begin
      Result:= LrRowRec.FValue;
      End;
    End
  Else
    Begin
    Result:= Nil;
    End;

{
Previous Non-Optimal implementation
  temp:= RowRec(Col, Row);
  if temp = nil then
    Result:= nil
  else
  begin
    if temp^.FBlob then
      Result:= GetCellMemory(Col, Row).Memory
    else
      Result:= temp^.FValue;
    Dispose(temp);
  end;
}
End;

Function THstmt.GetColType(Col: SQLUSMALLINT): SQLSMALLINT;
Var
  temp: TColPtr;
Begin
  { Retrieve Type }
  temp:= ColRec(Col);

  If temp = Nil Then
    Result:= 0
  Else
    Result:= temp^.FType;
End;

Function THstmt.GetSqlType(Col: SQLUSMALLINT): SQLSMALLINT;
Var
  temp: TColPtr;
Begin
  { Retrieve Type }
  temp:= ColRec(Col);

  If temp = Nil Then
    Result:= 0
  Else
    Result:= temp^.FSql;
End;

Function THstmt.GetBlobCol(Col: SQLUSMALLINT): Boolean;
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);
  Result:= (temp <> Nil) And temp^.FBlob;
End;

Function THstmt.ParamRec(Param: SQLUSMALLINT): TParamPtr;
Begin
  if param >= Length(FParamIndexes) then
    Result := nil
  else
    Result := FParamIndexes[Param];
End;

Function THstmt.ColBindRec(Col: SQLUSMALLINT): TColBindPtr;
Begin

  Result:= FColBinds;

  While Result <> Nil Do
  Begin
    If Result^.FCol = Col Then
      Break;
    Result:= Result^.Next;
  End;
End;

Function THstmt.ColRec(Col: SQLUSMALLINT): TColPtr;
Begin
  Init;

  { Bind Columns }
  BindCols;

  If (Col > 0) And (Col <= SQLUSMALLINT(FNumCols)) Then
    Begin
    Result := FColIndexes[Pred(col)];
    End
  Else
    Begin
    Result := Nil;
    End;

{
//Obsolete linear search

  Result:= nil;
  if (Col > 0) and (Col <= SQLUSMALLINT(FNumCols)) then
  begin
    Result:= FCols;
    for i:= 2 to Col do
      Result:= Result^.Next;
  end;
}
End;

Function THstmt.RowRec(Col, Row: SQLUSMALLINT): TRowPtr;
Var
  tempCol: TColPtr;
  tempRow: TRowPtr;
  Size: Word;
Begin
  tempCol:= ColRec(Col);

  tempRow:= Nil;
  If (tempCol <> Nil) And
     (Row > 0) And (Row <= FRowSetSize) Then
  Begin
    New(tempRow);
    tempRow^.FType:= tempCol^.FType;
    tempRow^.FBlob:= tempCol^.FBlob;
    If tempCol^.FBlob Then
      tempRow^.FValue:= Nil
    Else
    Begin
      If (tempCol^.FSql = SQL_CHAR) Or (tempCol^.FSql = SQL_VARCHAR) Then
        Size:= tempCol^.FColumnSize+1
      Else If (tempCol^.FSql = SQL_BINARY) Or (tempCol^.FSql = SQL_VARBINARY) Then
        Size:= tempCol^.FColumnSize
      Else
        Size:= PhysSize(tempCol^.FType);
      tempRow^.FValue:= OffsetRow(tempCol^.FValue, Row, Size);
    End;
    tempRow^.FSize:= OffsetRow(tempCol^.FSize, Row, PhysSize(SQL_C_SLONG));
  End;

  Result:= tempRow;
End;

Function THstmt.RowRecEx(Col, Row: SQLUSMALLINT; Out VrRowRec : TRowRec): Boolean;
Var
  Size: Word;
  tempCol: TColPtr;
Begin
  tempCol:= ColRec(Col);

  If (tempCol <> Nil) And
     (Row > 0) And (Row <= FRowSetSize) Then
    Begin
    VrRowRec.FType:= tempCol^.FType;

    VrRowRec.FBlob:= tempCol^.FBlob;

    If tempCol^.FBlob Then
      Begin
      VrRowRec.FValue:= Nil
      End
    Else
      Begin
      If (tempCol^.FSql = SQL_CHAR) Or (tempCol^.FSql = SQL_VARCHAR) Then
        Begin
        Size:= tempCol^.FColumnSize+1;
        End
      Else If (tempCol^.FSql = SQL_BINARY) Or (tempCol^.FSql = SQL_VARBINARY) Then
        Begin
        Size:= tempCol^.FColumnSize;
        End
      Else
        Begin
        Size:= PhysSize(tempCol^.FType);
        End;

      VrRowRec.FValue:= OffsetRow(tempCol^.FValue, Row, Size);
      End;

    VrRowRec.FSize:= OffsetRow(tempCol^.FSize, Row, SizeOf(Integer));

    Result := True;
    End
  Else
    Begin
    Result := False;
    End;
End;

Function THstmt.RowFlags(Row: SQLUSMALLINT): SQLUSMALLINTPtr;
Begin
  Log(1, 'THstmt.RowFlags');

  If (Row > 0) And (Row <= FRowSetSize) Then
    Result:= SQLUSMALLINTPtr(OffsetRow(FRowFlags^.FValue, Row, PhysSize(SQL_C_USHORT)))
  Else
    Result:= Nil;
End;

Function THstmt.GetColString(Col: SQLUSMALLINT): String;
Begin
  Result:= GetCellString(Col, 1);
End;

Function THstmt.GetColSingle(Col: SQLUSMALLINT): Single;
Begin
  Result:= GetCellDouble(Col, 1);
End;

Function THstmt.GetColDouble(Col: SQLUSMALLINT): Double;
Begin
  Result:= GetCellDouble(Col, 1);
End;

Function THstmt.GetColBoolean(Col: SQLUSMALLINT): Boolean;
Begin
  Result:= GetCellBoolean(Col, 1);
End;

Function THstmt.GetColShortint(Col: SQLUSMALLINT): ShortInt;
Begin
  Result:= GetCellInteger(Col, 1);
End;

Function THstmt.GetColByte(Col: SQLUSMALLINT): Byte;
Begin
  Result:= GetCellInteger(Col, 1);
End;

Function THstmt.GetColSmallint(Col: SQLUSMALLINT): SmallInt;
Begin
  Result:= GetCellInteger(Col, 1);
End;

Function THstmt.GetColWord(Col: SQLUSMALLINT): Word;
Begin
  Result:= GetCellInteger(Col, 1);
End;

Function THstmt.GetColInteger(Col: SQLUSMALLINT): Integer;
Begin
  Result:= GetCellInteger(Col, 1);
End;

Function THstmt.GetColCardinal(Col: SQLUSMALLINT): Cardinal;
Begin
  Result:= GetCellInteger(Col, 1);
End;

Function THstmt.GetColLongint(Col: SQLUSMALLINT): LongInt;
Begin
  Result:= GetCellInteger(Col, 1);
End;

Function THstmt.GetColLongword(Col: SQLUSMALLINT): LongWord;
Begin
  Result:= GetCellInteger(Col, 1);
End;

Function THstmt.GetColInt64(Col: SQLUSMALLINT): Int64;
Begin
  Result:= GetCellInt64(Col, 1);
End;

Function THstmt.GetColDate(Col: SQLUSMALLINT): TDate;
Begin
  Result:= GetCellDate(Col, 1);
End;

Function THstmt.GetColTime(Col: SQLUSMALLINT): TTime;
Begin
  Result:= GetCellTime(Col, 1);
End;

Function THstmt.GetColTimeStamp(Col: SQLUSMALLINT): DateSupport.TTimeStamp;
Begin
  Result:= GetCellTimeStamp(Col, 1);
End;

Function THstmt.GetColMemory(Col: SQLUSMALLINT): TMemoryStream;
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);
  If temp = Nil Then
    Result:= Nil
  Else
  Begin
    If temp^.FBlob Then
    Begin
      If FBlobDeferral Then
        temp^.FSize^:= FetchCell(Col, 1, temp^.FType, temp^.FMemory);
      Result:= temp^.FMemory
    End
    Else
      Result:= Nil;
  End;
End;

Function THstmt.GetColVariant(Col: SQLUSMALLINT): Variant;
Begin
  Result:= GetCellVariant(Col, 1);
End;

Procedure THstmt.SetColString(Col: SQLUSMALLINT;
                              AValue: String);
Begin
  SetCellString(Col, 1, AValue);
End;

Procedure THstmt.SetColSingle(Col: SQLUSMALLINT;
                              AValue: Single);
Begin
  SetCellDouble(Col, 1, AValue);
End;

Procedure THstmt.SetColDouble(Col: SQLUSMALLINT;
                              AValue: Double);
Begin
  SetCellDouble(Col, 1, AValue);
End;

Procedure THstmt.SetColBoolean(Col: SQLUSMALLINT;
                               AValue: Boolean);
Begin
  SetCellBoolean(Col, 1, AValue);
End;

Procedure THstmt.SetColShortint(Col: SQLUSMALLINT;
                                AValue: ShortInt);
Begin
  SetCellInteger(Col, 1, AValue);
End;

Procedure THstmt.SetColByte(Col: SQLUSMALLINT;
                            AValue: Byte);
Begin
  SetCellInteger(Col, 1, AValue);
End;

Procedure THstmt.SetColSmallint(Col: SQLUSMALLINT;
                                AValue: SmallInt);
Begin
  SetCellInteger(Col, 1, AValue);
End;

Procedure THstmt.SetColWord(Col: SQLUSMALLINT;
                            AValue: Word);
Begin
  SetCellInteger(Col, 1, AValue);
End;

Procedure THstmt.SetColInteger(Col: SQLUSMALLINT;
                               AValue: Integer);
Begin
  SetCellInteger(Col, 1, AValue);
End;

Procedure THstmt.SetColCardinal(Col: SQLUSMALLINT;
                                AValue: Cardinal);
Begin
  SetCellInteger(Col, 1, AValue);
End;

Procedure THstmt.SetColLongint(Col: SQLUSMALLINT;
                               AValue: LongInt);
Begin
  SetCellInteger(Col, 1, AValue);
End;

Procedure THstmt.SetColLongword(Col: SQLUSMALLINT;
                                AValue: LongWord);
Begin
  SetCellInteger(Col, 1, AValue);
End;

Procedure THstmt.SetColInt64(Col: SQLUSMALLINT;
                             AValue: Int64);
Begin
  SetCellInt64(Col, 1, AValue);
End;

Procedure THstmt.SetColDate(Col: SQLUSMALLINT;
                            AValue: TDate);
Begin
  SetCellDate(Col, 1, AValue);
End;

Procedure THstmt.SetColTime(Col: SQLUSMALLINT;
                            AValue: TTime);
Begin
  SetCellTime(Col, 1, AValue);
End;

Procedure THstmt.SetColTimeStamp(Col: SQLUSMALLINT;
                                 AValue: DateSupport.TTimeStamp);
Begin
  SetCellTimeStamp(Col, 1, AValue);
End;

Procedure THstmt.SetColMemory(Col: SQLUSMALLINT;
                              AValue: TMemoryStream);
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);
  If (temp <> Nil) And temp^.FBlob Then
  Begin
    temp^.FMemory.Clear;
    temp^.FMemory.LoadFromStream(AValue);
    temp^.FSize^:= temp^.FMemory.Size;
  End;
End;

Procedure THstmt.SetColVariant(Col: SQLUSMALLINT;
                               AValue: Variant);
Begin
  SetCellVariant(Col, 1, AValue);
End;

Function THstmt.GetColStringByName(ColName: String): String;
Begin
  Result:= GetColString(ColByName(ColName));
End;

Function THstmt.GetColSingleByName(ColName: String): Single;
Begin
  Result:= GetColSingle(ColByName(ColName));
End;

Function THstmt.GetColDoubleByName(ColName: String): Double;
Begin
  Result:= GetColDouble(ColByName(ColName));
End;

Function THstmt.GetColBooleanByName(ColName: String): Boolean;
Begin
  Result:= GetColBoolean(ColByName(ColName));
End;

Function THstmt.GetColShortintByName(ColName: String): ShortInt;
Begin
  Result:= GetColShortint(ColByName(ColName));
End;

Function THstmt.GetColByteByName(ColName: String): Byte;
Begin
  Result:= GetColByte(ColByName(ColName));
End;

Function THstmt.GetColSmallintByName(ColName: String): SmallInt;
Begin
  Result:= GetColSmallint(ColByName(ColName));
End;

Function THstmt.GetColWordByName(ColName: String): Word;
Begin
  Result:= GetColWord(ColByName(ColName));
End;

Function THstmt.GetColIntegerByName(ColName: String): Integer;
Begin
  Result:= GetColInteger(ColByName(ColName));
End;

Function THstmt.GetColCardinalByName(ColName: String): Cardinal;
Begin
  Result:= GetColCardinal(ColByName(ColName));
End;

Function THstmt.GetColLongintByName(ColName: String): LongInt;
Begin
  Result:= GetColLongint(ColByName(ColName));
End;

Function THstmt.GetColLongwordByName(ColName: String): LongWord;
Begin
  Result:= GetColLongword(ColByName(ColName));
End;

Function THstmt.GetColInt64ByName(ColName: String): Int64;
Begin
  Result:= GetColInt64(ColByName(ColName));
End;

Function THstmt.GetColDateByName(ColName: String): TDate;
Begin
  Result:= GetColDate(ColByName(ColName));
End;

Function THstmt.GetColTimeByName(ColName: String): TTime;
Begin
  Result:= GetColTime(ColByName(ColName));
End;

Function THstmt.GetColTimeStampByName(ColName: String): DateSupport.TTimeStamp;
Begin
  Result:= GetColTimeStamp(ColByName(ColName));
End;

Function THstmt.GetColMemoryByName(ColName: String): TMemoryStream;
Begin
  Result:= GetColMemory(ColByName(ColName));
End;

Function THstmt.GetColVariantByName(ColName: String): Variant;
Begin
  Result:= GetColVariant(ColByName(ColName));
End;

Procedure THstmt.SetColStringByName(ColName: String;
                                    AValue: String);
Begin
  SetColString(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColSingleByName(ColName: String;
                                    AValue: Single);
Begin
  SetColSingle(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColDoubleByName(ColName: String;
                                    AValue: Double);
Begin
  SetColDouble(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColBooleanByName(ColName: String;
                                     AValue: Boolean);
Begin
  SetColBoolean(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColShortintByName(ColName: String;
                                      AValue: ShortInt);
Begin
  SetColShortint(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColByteByName(ColName: String;
                                  AValue: Byte);
Begin
  SetColByte(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColSmallintByName(ColName: String;
                                      AValue: SmallInt);
Begin
  SetColSmallint(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColWordByName(ColName: String;
                                  AValue: Word);
Begin
  SetColWord(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColIntegerByName(ColName: String;
                                     AValue: Integer);
Begin
  SetColInteger(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColCardinalByName(ColName: String;
                                      AValue: Cardinal);
Begin
  SetColCardinal(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColLongintByName(ColName: String;
                                     AValue: LongInt);
Begin
  SetColLongint(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColLongwordByName(ColName: String;
                                      AValue: LongWord);
Begin
  SetColLongword(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColInt64ByName(ColName: String;
                                   AValue: Int64);
Begin
  SetColInt64(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColDateByName(ColName: String;
                                  AValue: TDate);
Begin
  SetColDate(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColTimeByName(ColName: String;
                                  AValue: TTime);
Begin
  SetColTime(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColTimeStampByName(ColName: String;
                                       AValue: DateSupport.TTimeStamp);
Begin
  SetColTimeStamp(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColMemoryByName(ColName: String;
                                    AValue: TMemoryStream);
Begin
  SetColMemory(ColByName(ColName), AValue);
End;

Procedure THstmt.SetColVariantByName(ColName: String;
                                     AValue: Variant);
Begin
  SetColVariant(ColByName(ColName), AValue);
End;

Function THstmt.GetCellString(Col, Row: SQLUSMALLINT): String;
Var
  //temp: TRowPtr;
  LrRowRec : TRowRec;

  Function FormatString(CValue: SQLPOINTER;
                        CType: SQLSMALLINT): String;
  Var
    fm: String;
  Begin
    Result:= OdbcCore.ToString(CValue, CType, StringTrimming);
    fm:= ColFormatMask[Col];

    Case ColFormatStyle[Col] Of
      fsFloat:
      Begin
        Case CType Of
          SQL_C_FLOAT,
          SQL_C_DOUBLE:
            Result:= FormatFloat(fm, CellDouble[Col, Row]);

          SQL_C_BIT,
          SQL_C_STINYINT,
          SQL_C_UTINYINT,
          SQL_C_SSHORT,
          SQL_C_USHORT,
          SQL_C_SLONG,
          SQL_C_ULONG:
            Result:= FormatFloat(fm, CellInteger[Col, Row]);
        End;
      End;
      fsDateTime:
      Begin
        Case CType Of
          SQL_C_TYPE_DATE,
          SQL_C_TYPE_TIME,
          SQL_C_TYPE_TIMESTAMP:
            Result:= FormatDateTime(fm, TSToDateTime(CellTimeStamp[Col, Row]));
        End;
      End;
      fsCustom:
      Begin
        Case CType Of
          SQL_C_FLOAT,
          SQL_C_DOUBLE:
            Result:= Format(fm, [CellDouble[Col, Row]]);

          SQL_C_BIT,
          SQL_C_STINYINT,
          SQL_C_UTINYINT,
          SQL_C_SSHORT,
          SQL_C_USHORT,
          SQL_C_SLONG,
          SQL_C_ULONG:
            Result:= Format(fm, [CellInteger[Col, Row]]);
          Else
            Result:= Format(fm, [Result]);
        End;
      End;
    End;
  End;

var res: ansistring;
Begin
  //Alternative Semi-Optimal implementation

  If RowRecEx(Col, Row, LrRowRec) Then
    Begin
    If LrRowRec.FBlob Then
      Begin
      Result:= MemoryStreamToString(GetCellMemory(Col, Row));
      End;
    If LrRowRec.FSize^ = SQL_NULL_DATA Then
      Begin
      Result:= NullData;
      End
    Else If (Not LrRowRec.FBlob) And (LrRowRec.FSize^ > 0) And ((LrRowRec.FType = SQL_C_CHAR) Or (LrRowRec.FType = SQL_C_BINARY)) Then
      Begin
      //Kiap:
      //Made temp changes to support reading strings with 0#
      //What are the consequences of this
      //TODO: The toString functions needs to be overhauled to take into account the size
      SetLength(Res, LrRowRec.FSize^);
      Move(LrRowRec.FValue^, Res[1], LrRowRec.FSize^);
      Result:= TrimString(Res, StringTrimming);
      End
    Else If Not LrRowRec.FBlob Then
      Begin
      Result:= FormatString(LrRowRec.FValue, LrRowRec.FType);
      End;
    End
  Else
    Begin
    Result:= '';
    End;

{
Previous Non-Optimal implementation

  temp:= RowRec(Col, Row);
  if temp = nil then
    Result:= ''
  else
  begin
    if temp^.FBlob then
      Result:= MemoryStreamToString(GetCellMemory(Col, Row));
    if temp^.FSize^ = SQL_NULL_DATA then
      Result:= NullData
    else if (NOT temp^.FBlob) and (temp^.FSize^ > 0) and ((temp^.FType = SQL_C_CHAR) or (temp^.FType = SQL_C_BINARY)) then
      begin
      //Kiap:
      //Made temp changes to support reading strings with 0#
      //What are the consequences of this
      //TODO: The toString functions needs to be overhauled to take into account the size
      SetLength(Result, temp^.FSize^);
      Move(temp^.FValue^, Result[1], temp^.FSize^);
      Result:= TrimString(Result, StringTrimming);
      end
    else if NOT temp^.FBlob then
      Result:= FormatString(temp^.FValue, temp^.FType);
    Dispose(temp);
  end;
}
End;

Function THstmt.GetCellSingle(Col, Row: SQLUSMALLINT): Single;
Begin
  Result:= GetCellDouble(Col, Row);
End;

Function THstmt.GetCellDouble(Col, Row: SQLUSMALLINT): Double;
Var
  //temp: TRowPtr;
  LrRowRec : TRowRec;
Begin
  //Alternative Semi-Optimal implementation
  
  If RowRecEx(Col, Row, LrRowRec) Then
    Begin
    If (LrRowRec.FSize^ = SQL_NULL_DATA) Or LrRowRec.FBlob Then
      Begin
      Result:= 0
      End
    Else
      Begin
      Result:= ToDouble(LrRowRec.FValue, LrRowRec.FType);
      End;
    End
  Else
    Begin
    Result:= 0;
    End;

{
Previous Non-Optimal implementation

  temp:= RowRec(Col, Row);
  if temp = nil then
    Result:= 0
  else
  begin
    if (temp^.FSize^ = SQL_NULL_DATA) or temp^.FBlob then
      Result:= 0
    else
      Result:= ToDouble(temp^.FValue, temp^.FType);
    Dispose(temp);
  end;
}
End;

Function THstmt.GetCellBoolean(Col, Row: SQLUSMALLINT): Boolean;
Begin
  Result:= GetCellInteger(Col, Row) <> 0;
End;

Function THstmt.GetCellShortint(Col, Row: SQLUSMALLINT): ShortInt;
Begin
  Result:= GetCellInteger(Col, Row);
End;

Function THstmt.GetCellByte(Col, Row: SQLUSMALLINT): Byte;
Begin
  Result:= GetCellInteger(Col, Row);
End;

Function THstmt.GetCellSmallint(Col, Row: SQLUSMALLINT): SmallInt;
Begin
  Result:= GetCellInteger(Col, Row);
End;

Function THstmt.GetCellWord(Col, Row: SQLUSMALLINT): Word;
Begin
  Result:= GetCellInteger(Col, Row);
End;

Function THstmt.GetCellInteger(Col, Row: SQLUSMALLINT): Integer;
Var
  //temp: TRowPtr;
  LrRowRec : TRowRec;
Begin
  //Alternative Semi-Optimal implementation

  If RowRecEx(Col, Row, LrRowRec) Then
    Begin
    If (LrRowRec.FSize^ = SQL_NULL_DATA) Or LrRowRec.FBlob Then
      Result:= 0
    Else
      Result:= ToInteger(LrRowRec.FValue, LrRowRec.FType);
    End
  Else
    Begin
    Result:= 0
    End;

{
Previous Non-Optimal implementation

  temp:= RowRec(Col, Row);
  if temp = nil then
    Result:= 0
  else
  begin
    if (temp^.FSize^ = SQL_NULL_DATA) or temp^.FBlob then
      Result:= 0
    else
      Result:= ToInteger(temp^.FValue, temp^.FType);
    Dispose(temp);
  end;
}
End;

Function THstmt.GetCellCardinal(Col, Row: SQLUSMALLINT): Cardinal;
Begin
  Result:= GetCellInteger(Col, Row);
End;

Function THstmt.GetCellLongint(Col, Row: SQLUSMALLINT): LongInt;
Begin
  Result:= GetCellInteger(Col, Row);
End;

Function THstmt.GetCellLongword(Col, Row: SQLUSMALLINT): LongWord;
Begin
  Result:= GetCellInteger(Col, Row);
End;

Function THstmt.GetCellInt64(Col, Row: SQLUSMALLINT): Int64;
Var
  //temp: TRowPtr;
  LrRowRec : TRowRec;
Begin
  //Alternative Semi-Optimal implementation
  
  If RowRecEx(Col, Row, LrRowRec) Then
    Begin
    If (LrRowRec.FSize^ = SQL_NULL_DATA) Or LrRowRec.FBlob Then
      Begin
      Result:= 0;
      End
    Else
      Begin
      Result:= ToInt64(LrRowRec.FValue, LrRowRec.FType);
      End;
    End
  Else
    Begin
    Result:= 0;
    End;

{
Previous Non-Optimal implementation

  temp:= RowRec(Col, Row);
  if temp = nil then
    Result:= 0
  else
  begin
    if (temp^.FSize^ = SQL_NULL_DATA) or temp^.FBlob then
      Result:= 0
    else
      Result:= ToInt64(temp^.FValue, temp^.FType);
    Dispose(temp);
  end;
}
End;

Function THstmt.GetCellDate(Col, Row: SQLUSMALLINT): TDate;
Var
  TS: DateSupport.TTimeStamp;
Begin
  TS:= GetCellTimeStamp(Col, Row);
  With Result Do
  Begin
    Year:= TS.Year;
    Month:= TS.Month;
    Day:= TS.Day;
  End;
End;

Function THstmt.GetCellTime(Col, Row: SQLUSMALLINT): TTime;
Var
  TS: DateSupport.TTimeStamp;
Begin
  TS:= GetCellTimeStamp(Col, Row);
  With Result Do
  Begin
    Hour:= TS.Hour;
    Minute:= TS.Minute;
    Second:= TS.Second;
  End;
End;

Function THstmt.GetCellTimeStamp(Col, Row: SQLUSMALLINT): DateSupport.TTimeStamp;
Var
  //temp: TRowPtr;
  LrRowRec : TRowRec;
Begin
  //Alternative Semi-Optimal implementation
  
  If RowRecEx(Col, Row, LrRowRec) Then
    Begin
    If (LrRowRec.FSize^ = SQL_NULL_DATA) Or LrRowRec.FBlob Then
      Begin
      Result:= NullTS;
      End
    Else
      Begin
      Result:= ToTimeStamp(LrRowRec.FValue, LrRowRec.FType);
      End;
    End
  Else
    Begin
    Result:= NullTS;
    End;
    
{
Previous Non-Optimal implementation

  temp:= RowRec(Col, Row);
  if temp = nil then
    Result:= NullTS
  else
  begin
    if (temp^.FSize^ = SQL_NULL_DATA) or temp^.FBlob then
      Result:= NullTS
    else
      Result:= ToTimeStamp(temp^.FValue, temp^.FType);
    Dispose(temp);
  end;
}
End;

Function THstmt.GetCellMemory(Col, Row: SQLUSMALLINT): TMemoryStream;
Var
  tempCol: TColPtr;
  temp: TRowPtr;
Begin
  temp:= RowRec(Col,Row);
  If temp = Nil Then
    Result:= Nil
  Else
  Begin
    If temp^.FBlob Then
    Begin
      tempCol:= ColRec(Col);
      tempCol^.FSize^:= FetchCell(Col, Row, temp^.FType, tempCol^.FMemory);
      Result:= tempCol^.FMemory;
    End
    Else
      Begin
      //Kiap
      //Need to be able to read other datatypes as memory aswell
      If temp^.FSize^ = SQL_NULL_DATA Then
        Begin
        Result:= Nil;
        End
      Else
        Begin
        tempCol:= ColRec(Col);

        If tempCol^.FMemory = Nil Then
          Begin
          tempCol^.FMemory := TMemoryStream.Create;
          End;

        tempCol^.FMemory.Size     := temp^.FSize^;
        tempCol^.FMemory.Position := 0;

        If (temp^.FSize^ > 0) Then
          Begin
          tempCol^.FMemory.Write(temp^.FValue^, temp^.FSize^);
          End;

        Result:= tempCol^.FMemory;
        End;
      End;

    Dispose(temp);
  End;
End;

Function THstmt.GetCellVariant(Col, Row: SQLUSMALLINT): Variant;
Begin
  If CellNull[Col, Row] Then
    Result:= Unassigned
  Else
    Case ColType[Col] Of
      SQL_C_CHAR:
        Result:= CellString[Col, Row];

      SQL_C_BINARY:
        Result:= Unassigned;

      SQL_C_FLOAT:
        Result:= CellSingle[Col, Row];
      SQL_C_DOUBLE:
        Result:= CellDouble[Col, Row];

      SQL_C_BIT:
        Result:= CellBoolean[Col, Row];
      SQL_C_STINYINT:
        Result:= CellShortint[Col, Row];
      SQL_C_UTINYINT:
        Result:= CellByte[Col, Row];
      SQL_C_SSHORT:
        Result:= CellSmallint[Col, Row];
      SQL_C_USHORT:
        Result:= CellWord[Col, Row];
      SQL_C_SLONG:
        Result:= CellInteger[Col, Row];
      SQL_C_ULONG:
        Result:= CellInteger[Col, Row];
      SQL_C_SBIGINT,
      SQL_C_UBIGINT:
        Result:= CellInteger[Col, Row];

      SQL_C_TYPE_DATE:
        Result:= TSToDateTime(DateAsTS(CellDate[Col, Row]));
      SQL_C_TYPE_TIME:
        Result:= TSToDateTime(TimeAsTS(CellTime[Col, Row]));
      SQL_C_TYPE_TIMESTAMP:
        Result:= TSToDateTime(CellTimeStamp[Col, Row]);
      Else
        Result:= Unassigned;
    End;
End;

Procedure THstmt.SetCellString(Col, Row: SQLUSMALLINT;
                               AValue: String);
Var
  tempCol: TColPtr;
  temp: TRowPtr;
  s: String;

  Function EmptyToNull: Boolean;
  Begin
    Result:= False;
    Case FEmptyToNull Of
      enNever:
        Result:= False;
      enAlways:
        Result:= True;
      enIfNullable:
        Result:= ColNullable[Col] = SQL_NULLABLE;
    End;
  End;

Begin
  temp:= RowRec(Col, Row);
  If temp <> Nil Then
  Begin
    s:= AValue;
    If temp^.FBlob Then
    Begin
      tempCol:= ColRec(Col);
      tempCol^.FMemory.SetSize(Length(s)+1);
      tempCol^.FSize^:= Length(s);
      ToValue(s, tempCol^.FMemory.Memory, tempCol^.FType, StringTrimming);
    End
    Else
    Begin
      ToValue(s, temp^.FValue, temp^.FType, StringTrimming);
      If (temp^.FType = SQL_C_CHAR) Or (temp^.FType = SQL_C_BINARY) Then
        temp^.FSize^:= Length(s)
      Else
        temp^.FSize^:= PhysSize(temp^.FType);
    End;

    If (AValue = '') And EmptyToNull Then
      temp^.FSize^:= SQL_NULL_DATA;

    Dispose(temp);
  End;
End;

Procedure THstmt.SetCellSingle(Col, Row: SQLUSMALLINT;
                               AValue: Single);
Begin
  SetCellDouble(Col, Row, AValue);
End;

Procedure THstmt.SetCellDouble(Col, Row: SQLUSMALLINT;
                               AValue: Double);
Var
  temp: TRowPtr;
  s: String;
Begin
  temp:= RowRec(Col, Row);
  If temp <> Nil Then
  Begin
    If Not temp^.FBlob Then
    Begin
      Str(AValue, s);
      ToValue(s, temp^.FValue, temp^.FType, StringTrimming);
      If temp^.FType = SQL_C_CHAR Then
        temp^.FSize^:= Length(s)
      Else
        temp^.FSize^:= PhysSize(temp^.FType);
    End;

    Dispose(temp);
  End;
End;

Procedure THstmt.SetCellBoolean(Col, Row: SQLUSMALLINT;
                                AValue: Boolean);
Begin
  If AValue Then
    SetCellInteger(Col, Row, 1)
  Else
    SetCellInteger(Col, Row, 0);
End;

Procedure THstmt.SetCellShortint(Col, Row: SQLUSMALLINT;
                                 AValue: ShortInt);
Begin
  SetCellInteger(Col, Row, AValue);
End;

Procedure THstmt.SetCellByte(Col, Row: SQLUSMALLINT;
                             AValue: Byte);
Begin
  SetCellInteger(Col, Row, AValue);
End;

Procedure THstmt.SetCellSmallint(Col, Row: SQLUSMALLINT;
                                 AValue: SmallInt);
Begin
  SetCellInteger(Col, Row, AValue);
End;

Procedure THstmt.SetCellWord(Col, Row: SQLUSMALLINT;
                             AValue: Word);
Begin
  SetCellInteger(Col, Row, AValue);
End;

Procedure THstmt.SetCellInteger(Col, Row: SQLUSMALLINT;
                                AValue: Integer);
Var
  temp: TRowPtr;
  s: String;
Begin
  temp:= RowRec(Col, Row);
  If temp <> Nil Then
  Begin
    If Not temp^.FBlob Then
    Begin
      Str(AValue, s);
      ToValue(s, temp^.FValue, temp^.FType, StringTrimming);
      If temp^.FType = SQL_C_CHAR Then
        temp^.FSize^:= Length(s)
      Else
        temp^.FSize^:= PhysSize(temp^.FType);
    End;

    Dispose(temp);
  End;
End;

Procedure THstmt.SetCellCardinal(Col, Row: SQLUSMALLINT;
                                 AValue: Cardinal);
Begin
  SetCellInteger(Col, Row, AValue);
End;

Procedure THstmt.SetCellLongint(Col, Row: SQLUSMALLINT;
                                AValue: LongInt);
Begin
  SetCellInteger(Col, Row, AValue);
End;

Procedure THstmt.SetCellLongword(Col, Row: SQLUSMALLINT;
                                 AValue: LongWord);
Begin
  SetCellInteger(Col, Row, AValue);
End;

Procedure THstmt.SetCellInt64(Col, Row: SQLUSMALLINT;
                              AValue: Int64);
Var
  temp: TRowPtr;
  s: String;
Begin
  temp:= RowRec(Col, Row);
  If temp <> Nil Then
  Begin
    If Not temp^.FBlob Then
    Begin
      Str(AValue, s);
      ToValue(s, temp^.FValue, temp^.FType, StringTrimming);
      If temp^.FType = SQL_C_CHAR Then
        temp^.FSize^:= Length(s)
      Else
        temp^.FSize^:= PhysSize(temp^.FType);
    End;

    Dispose(temp);
  End;
End;

Procedure THstmt.SetCellDate(Col, Row: SQLUSMALLINT;
                             AValue: TDate);
Var
  temp: TRowPtr;
  s: String;
  ts: DateSupport.TTimeStamp;
Begin
  temp:= RowRec(Col, Row);
  If temp <> Nil Then
  Begin
    If Not temp^.FBlob Then
    Begin
      If temp^.FType = SQL_C_TYPE_TIMESTAMP Then
      Begin
        With AValue Do
        Begin
          ts:= NullTS;
          ts.Year:= Year;
          ts.Month:= Month;
          ts.Day:= Day;
          SetCellTimeStamp(Col, Row, ts);
        End;
      End
      Else
      Begin
        s:= OdbcCore.ToString(@AValue, SQL_C_TYPE_DATE, stTrimBoth);
        ToValue(s, temp^.FValue, temp^.FType, StringTrimming);
        If temp^.FType = SQL_C_CHAR Then
          temp^.FSize^:= Length(s)
        Else
          temp^.FSize^:= PhysSize(temp^.FType);
      End;
    End;

    Dispose(temp);
  End;
End;

Procedure THstmt.SetCellTime(Col, Row: SQLUSMALLINT;
                             AValue: TTime);
Var
  temp: TRowPtr;
  s: String;
  ts: DateSupport.TTimeStamp;
Begin
  temp:= RowRec(Col, Row);
  If temp <> Nil Then
  Begin
    If Not temp^.FBlob Then
    Begin
      If temp^.FType = SQL_C_TYPE_TIMESTAMP Then
      Begin
        With AValue Do
        Begin
          ts:= NullTS;
          ts.Hour:= Hour;
          ts.Minute:= Minute;
          ts.Second:= Second;
          SetCellTimeStamp(Col, Row, ts);
        End;
      End
      Else
      Begin
        s:= OdbcCore.ToString(@AValue, SQL_C_TYPE_TIME, stTrimBoth);
        ToValue(s, temp^.FValue, temp^.FType, StringTrimming);
        If temp^.FType = SQL_C_CHAR Then
          temp^.FSize^:= Length(s)
        Else
          temp^.FSize^:= PhysSize(temp^.FType);
      End;
    End;

    Dispose(temp);
  End;
End;

Procedure THstmt.SetCellTimeStamp(Col, Row: SQLUSMALLINT;
                                  AValue: DateSupport.TTimeStamp);
Var
  temp: TRowPtr;
  s: String;
  Date: TDate;
  Time: TTime;
Begin
  temp:= RowRec(Col, Row);
  If temp <> Nil Then
  Begin
    If Not temp^.FBlob Then
    Begin
      Case temp^.FType Of
        SQL_C_TYPE_DATE:
        Begin
          With AValue Do
          Begin
            Date.Year:= Year;
            Date.Month:= Month;
            Date.Day:= Day;
          End;
          s:= OdbcCore.ToString(@Date, SQL_C_TYPE_DATE, stTrimBoth);
        End;
        SQL_C_TYPE_TIME:
        Begin
          With AValue Do
          Begin
            Time.Hour:= Hour;
            Time.Minute:= Minute;
            Time.Second:= Second;
          End;
          s:= OdbcCore.ToString(@Time, SQL_C_TYPE_TIME, stTrimBoth);
        End;
        Else
          s:= OdbcCore.ToString(@AValue, SQL_C_TYPE_TIMESTAMP, stTrimBoth);
      End;

      ToValue(s, temp^.FValue, temp^.FType, StringTrimming);
      If temp^.FType = SQL_C_CHAR Then
        temp^.FSize^:= Length(s)
      Else
        temp^.FSize^:= PhysSize(temp^.FType);
    End;

    Dispose(temp);
  End;
End;

Procedure THstmt.SetCellMemory(Col, Row: SQLUSMALLINT;
                               AValue: TMemoryStream);
Begin
  SetColMemory(Col, AValue);
End;

Procedure THstmt.SetCellVariant(Col, Row: SQLUSMALLINT;
                                AValue: Variant);
Begin
  Case VarType(AValue) Of
    varEmpty, varNull:
      CellNull[Col, Row]:= True;
    varSmallint:
      CellSmallint[Col, Row]:= AValue;
    varInteger:
      CellInteger[Col, Row]:= AValue;
    varSingle:
      CellSingle[Col, Row]:= AValue;
    varDouble,
    varCurrency:
      CellDouble[Col, Row]:= AValue;
    varDate:
      CellTimeStamp[Col, Row]:= DateTimeToTS(AValue);
    varOleStr, varString:
      CellString[Col, Row]:= AValue;
    varBoolean, varByte:
      CellByte[Col, Row]:= AValue;
    Else
      CellNull[Col, Row]:= True;
  End;
End;

Function THstmt.GetCellStringByName(ColName: String;
                                    Row: SQLUSMALLINT): String;
Begin
  Result:= GetCellString(ColByName(ColName), Row);
End;

Function THstmt.GetCellSingleByName(ColName: String;
                                    Row: SQLUSMALLINT): Single;
Begin
  Result:= GetCellSingle(ColByName(ColName), Row);
End;

Function THstmt.GetCellDoubleByName(ColName: String;
                                    Row: SQLUSMALLINT): Double;
Begin
  Result:= GetCellDouble(ColByName(ColName), Row);
End;

Function THstmt.GetCellBooleanByName(ColName: String;
                                     Row: SQLUSMALLINT): Boolean;
Begin
  Result:= GetCellBoolean(ColByName(ColName), Row);
End;

Function THstmt.GetCellShortintByName(ColName: String;
                                      Row: SQLUSMALLINT): ShortInt;
Begin
  Result:= GetCellShortint(ColByName(ColName), Row);
End;

Function THstmt.GetCellByteByName(ColName: String;
                                  Row: SQLUSMALLINT): Byte;
Begin
  Result:= GetCellByte(ColByName(ColName), Row);
End;

Function THstmt.GetCellSmallintByName(ColName: String;
                                      Row: SQLUSMALLINT): SmallInt;
Begin
  Result:= GetCellSmallint(ColByName(ColName), Row);
End;

Function THstmt.GetCellWordByName(ColName: String;
                                  Row: SQLUSMALLINT): Word;
Begin
  Result:= GetCellWord(ColByName(ColName), Row);
End;

Function THstmt.GetCellIntegerByName(ColName: String;
                                     Row: SQLUSMALLINT): Integer;
Begin
  Result:= GetCellInteger(ColByName(ColName), Row);
End;

Function THstmt.GetCellCardinalByName(ColName: String;
                                      Row: SQLUSMALLINT): Cardinal;
Begin
  Result:= GetCellCardinal(ColByName(ColName), Row);
End;

Function THstmt.GetCellLongintByName(ColName: String;
                                     Row: SQLUSMALLINT): LongInt;
Begin
  Result:= GetCellLongint(ColByName(ColName), Row);
End;

Function THstmt.GetCellLongwordByName(ColName: String;
                                      Row: SQLUSMALLINT): LongWord;
Begin
  Result:= GetCellLongword(ColByName(ColName), Row);
End;

Function THstmt.GetCellInt64ByName(ColName: String;
                                   Row: SQLUSMALLINT): Int64;
Begin
  Result:= GetCellInt64(ColByName(ColName), Row);
End;

Function THstmt.GetCellDateByName(ColName: String;
                                  Row: SQLUSMALLINT): TDate;
Begin
  Result:= GetCellDate(ColByName(ColName), Row);
End;

Function THstmt.GetCellTimeByName(ColName: String;
                                  Row: SQLUSMALLINT): TTime;
Begin
  Result:= GetCellTime(ColByName(ColName), Row);
End;

Function THstmt.GetCellTimeStampByName(ColName: String;
                                       Row: SQLUSMALLINT): DateSupport.TTimeStamp;
Begin
  Result:= GetCellTimeStamp(ColByName(ColName), Row);
End;

Function THstmt.GetCellMemoryByName(ColName: String;
                                    Row: SQLUSMALLINT): TMemoryStream;
Begin
  Result:= GetCellMemory(ColByName(ColName), Row);
End;

Function THstmt.GetCellVariantByName(ColName: String;
                                     Row: SQLUSMALLINT): Variant;
Begin
  Result:= GetCellVariant(ColByName(ColName), Row);
End;

Procedure THstmt.SetCellStringByName(ColName: String;
                                     Row: SQLUSMALLINT;
                                     AValue: String);
Begin
  SetCellString(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellSingleByName(ColName: String;
                                     Row: SQLUSMALLINT;
                                     AValue: Single);
Begin
  SetCellSingle(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellDoubleByName(ColName: String;
                                     Row: SQLUSMALLINT;
                                     AValue: Double);
Begin
  SetCellDouble(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellBooleanByName(ColName: String;
                                      Row: SQLUSMALLINT;
                                      AValue: Boolean);
Begin
  SetCellBoolean(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellShortintByName(ColName: String;
                                       Row: SQLUSMALLINT;
                                       AValue: ShortInt);
Begin
  SetCellShortint(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellByteByName(ColName: String;
                                   Row: SQLUSMALLINT;
                                   AValue: Byte);
Begin
  SetCellByte(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellSmallintByName(ColName: String;
                                       Row: SQLUSMALLINT;
                                       AValue: SmallInt);
Begin
  SetCellSmallint(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellWordByName(ColName: String;
                                   Row: SQLUSMALLINT;
                                   AValue: Word);
Begin
  SetCellWord(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellIntegerByName(ColName: String;
                                      Row: SQLUSMALLINT;
                                      AValue: Integer);
Begin
  SetCellInteger(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellCardinalByName(ColName: String;
                                       Row: SQLUSMALLINT;
                                       AValue: Cardinal);
Begin
  SetCellCardinal(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellLongintByName(ColName: String;
                                       Row: SQLUSMALLINT;
                                       AValue: LongInt);
Begin
  SetCellLongint(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellLongwordByName(ColName: String;
                                       Row: SQLUSMALLINT;
                                       AValue: LongWord);
Begin
  SetCellLongword(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellInt64ByName(ColName: String;
                                    Row: SQLUSMALLINT;
                                    AValue: Int64);
Begin
  SetCellInt64(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellDateByName(ColName: String;
                                   Row: SQLUSMALLINT;
                                   AValue: TDate);
Begin
  SetCellDate(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellTimeByName(ColName: String;
                                   Row: SQLUSMALLINT;
                                   AValue: TTime);
Begin
  SetCellTime(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellTimeStampByName(ColName: String;
                                        Row: SQLUSMALLINT;
                                        AValue: DateSupport.TTimeStamp);
Begin
  SetCellTimeStamp(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellMemoryByName(ColName: String;
                                     Row: SQLUSMALLINT;
                                     AValue: TMemoryStream);
Begin
  SetCellMemory(ColByName(ColName), Row, AValue);
End;

Procedure THstmt.SetCellVariantByName(ColName: String;
                                      Row: SQLUSMALLINT;
                                      AValue: Variant);
Begin
  SetCellVariant(ColByName(ColName), Row, AValue);
End;

Function THstmt.ParamByName(ParamName: String): SQLUSMALLINT;
Var
  Count: Integer;
Begin
  Log(1, 'THstmt.ParamByName');

  ParamName:= UpperCase(ParamName);
  Count:= 0;
  While (Count < FParamNames.Count) And (UpperCase(FParamNames[Count]) <> ParamName) Do
    Inc(Count);

  If Count < FParamNames.Count Then
    Result:= Count+1
  Else
    Raise EODBCExpress.Create('Token '+ParamName+' not found in SQL statement.');
End;

Function THstmt.ColByName(ColName: String): SQLUSMALLINT;
Var
  Index: Integer;
Begin
  Log(1, 'THstmt.ColByName');

  { Bind Columns }
  BindCols;

  Index:= IndexOf(ColName, FColNames);
  If Index > -1 Then
    Result:= Index+1
  Else
    Raise EODBCExpress.Create('Identifier '+ColName+' not found in result set.');
End;

Procedure THstmt.NullCols(Const Cols: Array Of String);
Var
  i: Integer;
Begin
  For i:= 0 To High(Cols) Do
    ColNull[ColByName(Cols[i])]:= True;
End;

Procedure THstmt.IgnoreCols(Const Cols: Array Of String);
Var
  i: Integer;
Begin
  For i:= 0 To High(Cols) Do
    ColIgnore[ColByName(Cols[i])]:= True;
End;

Procedure THstmt.PrimaryCols(Const Cols: Array Of String);
Var
  i: Integer;
Begin
  For i:= 0 To High(Cols) Do
    ColPrimary[ColByName(Cols[i])]:= True;
End;

Function THstmt.TypeString(SqlType: SQLSMALLINT;
                           SqlSize: LongInt): String;
Var
  Loc: Integer;
Begin
  Close;

  FRetCode:= SQLGetTypeInfo(FHstmt, SqlType);
  If FEnv.Error.Success(FRetCode) And FetchNext Then
  Begin
    Result:= ColString[1];
    Log(0, ColString[3]+' '+ColString[15]);  //max precision and scale
  End
  Else
    Result:= '';

  If (SqlSize > 0) And (Result <> '') And
     ((SqlType = SQL_CHAR) Or (SqlType = SQL_VARCHAR) Or
      (SqlType = SQL_BINARY) Or (SqlType = SQL_VARBINARY)) Then
  Begin
    Loc:= Pos('(', Result);
    If Loc > 0 Then
      Insert(IntToStr(SqlSize), Result, Loc+1)
    Else
      Result:= Result+'('+IntToStr(SqlSize)+')';
  End;
End;

Procedure THstmt.SetSkipByCursor(ASkipByCursor: Boolean);
Begin
  FSkipByCursor:= ASkipByCursor;

  UnPrepareHstmts;
End;

Procedure THstmt.SetSkipByPosition(ASkipByPosition: Boolean);
Begin
  FSkipByPosition:= ASkipByPosition;

  UnPrepareHstmts;
End;

{ TODBCContext }

Constructor TODBCContext.Create(Env : THEnv);
begin
  inherited Create;
  FEnv := env;
end;

function NullTS: TTimeStamp;
begin
  Result := DateTimeToTS(0);
end;

End.

