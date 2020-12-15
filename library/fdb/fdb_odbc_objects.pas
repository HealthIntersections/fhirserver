unit fdb_odbc_objects;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

Uses
  SysUtils, Classes, {$IFDEF FPC} fdb_odbc_fpc {$ELSE} fdb_odbc_headers {$ENDIF},
  fsl_utilities, fsl_fpc,
  fdb_dialects;

const
  EnterString = #0#0;

{$IFDEF FPC}
  SQL_API_ALL_FUNCTIONS = 0;
  SQL_API_ODBC3_ALL_FUNCTIONS = 999;

  //SQL_CP_OFF = 0;
  //SQL_CP_ONE_PER_DRIVER = 1;
  //SQL_CP_ONE_PER_HENV = 2;
  //SQL_CP_DEFAULT = SQL_CP_OFF;
  SQL_IGNORE = (-6);
  SQL_ATTR_QUERY_TIMEOUT = SQL_QUERY_TIMEOUT;
  SQL_LEN_DATA_AT_EXEC_OFFSET = (-100);
  SQL_DIAG_CURSOR_ROW_COUNT = (-1249);
  SQL_NEED_LONG_DATA_LEN = 111;
  SQL_NAMED = 0;
  SQL_UNNAMED = 1;

  SQL_ATTR_PARAMSET_SIZE = 22;
  SQL_API_SQLPARAMDATA = 48;
  SQL_PC_UNKNOWN = 0;
  SQL_PC_NON_PSEUDO = 1;
  SQL_PC_PSEUDO = 2;
  SQL_ALL_TABLE_TYPES = '%';
{$ELSE}
type
  PtrUInt = NativeUInt; // SQLUInteger;
{$ENDIF}

Type
  EODBCExpress = Class(Exception);

  SQLINTEGERPtr=^SQLINTEGER;
  SQLUSMALLINTPtr=^SQLUSMALLINT;

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
  // on OSX, the iODBC interface is UTF-32 not UTF-16.
  DefaultStringSize = 255 * {$IFDEF OSX} 4 {$ELSE} 2 {$ENDIF};
  NullData = '';

  DefRaiseSoftErrors = False;
  DefIsolationLevel = SQL_TXN_READ_COMMITTED;
  DefConnected = False;
  DefInfoPrompt = SQL_DRIVER_NOPROMPT;
  DefCursorLib = SQL_CUR_USE_DRIVER;
  DefPrepared = False;
  DefExecuted = False;
  DefParamType = SQL_PARAM_INPUT;
  DefBulkData = False;
  DefSQLParsing = True;
  DefConcurrencyType = SQL_CONCUR_READ_ONLY;
  DefCursorType = SQL_CURSOR_FORWARD_ONLY;
  DefBlobSize = 32768;
  DefBlobDeferral = False;
  DefBlobPlacement = bpDetect;
  DefEmptyToNull = enNever;
  DefStringTrimming = stTrimTrailing;
  DefBindByName = False;
  DefRowCountMethod = rcFunction;
  DefNoRowsAffected = [nrByUpdate];
  DefFormatStyle = fsNone;
  DefFormatMask = '';
  DefSkipByMethod = False;
  DefPrimary = False;
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

  ParamCharSet = ['A'..'Z', 'a'..'z', '0'..'9', '_', '@'];

Type
  { Type Pointers }
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
  TOdbcEnv = Class;
  TOdbcConnection = Class;
  TOdbcStatement = Class;

  { TErrorPtr }
  TErrorPtr = ^TErrorRec;
  TErrorRec = Record
    FState: String;
    FNative: SQLINTEGER;
    FMessage: String;
  End;

  TManagedMemoryStream = class (TMemoryStream)
  public
    property Capacity;
  end;

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

    constructor Create(AOwner: TODBCObject;
                       ARetCode: SQLRETURN;
                       AErrors: TList);
    destructor Destroy; Override;
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

  { TOdbcEnv }
  TOdbcEnv = Class(TODBCObject)
  Private
    { Private declarations }
    FError: TODBCErrorHandler;
    FHenv: SQLHENV;
    FActive: Boolean;
    FRetCode: SQLRETURN;

    Function TerminateHandle: Boolean;
  Protected
    { Protected declarations }
    Function Init: Boolean;
  Public
    { Public declarations }
    Property Handle: SQLHENV Read FHenv;
    Property Active: Boolean Read FActive;
    Property Error: TODBCErrorHandler Read FError Write FError;

    constructor Create;
    destructor Destroy; Override;
  End;

  { TODBCContext }
  TODBCContext = Class (TODBCObject)
  Protected
    FEnv : TOdbcEnv;
  Public
    constructor Create(Env : TOdbcEnv); Virtual;
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

  { TOdbcConnection }
  TOdbcConnection = Class (TODBCContext)
  Private
    { Private declarations }
    FHdbc: SQLHDBC;
    FActive: Boolean;
    FRetCode: SQLRETURN;
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
    FCore: Boolean;
    FDrivers: TList;
    FPlatform : TFDBPlatform;

    { Published Events }
    FBeforeConnect, FAfterConnect: TNotifyEvent;
    FBeforeDisconnect, FAfterDisconnect: TNotifyEvent;

    Function GetHandle: SQLHDBC;
    Function GetCore: Boolean;
    Procedure SetCore(ACore: Boolean);
    Function GetLoginTimeOut: SQLUINTEGER;
    Procedure SetLoginTimeOut(ALoginTimeOut: SQLUINTEGER);
    Procedure SetCursorLib(ACursorLib: SQLUINTEGER);
    Function GetInTransaction: Boolean;
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
    Property InTransaction: Boolean Read GetInTransaction;

    constructor Create(Env : TOdbcEnv); Override;
    destructor Destroy; Override;
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
    FSize: PSQLLEN;
  End;

  { TParamPtr }
  TParamPtr = ^TParamRec;
  TParamRec = Record
    Next: TParamPtr;
    FValue: SQLPOINTER;
    FSize: PSQLLEN;
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
    FSize: PSQLLEN;
    FType: SQLSMALLINT;
    FSql: SQLSMALLINT;

    FMemory: TManagedMemoryStream;
    FBlob, FBlobFetched: Boolean;
    FFormatStyle: TFormatStyle;
    FFormatMask: String;
    FPrimary: Boolean;
    
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

  { TOdbcStatement }
  TOdbcStatement = Class (TODBCContext)
  Private
    { Private declarations }
    FHdbc: TOdbcConnection;
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
    FNumCols: SQLSMALLINT;    //#cols in result set
    FNumRows: SQLUINTEGER;    //#values per col
    FNumParams: SQLUINTEGER;  //#values per param
    FBulkData: Boolean;
    FBlobs: Boolean;
    FColumnsBound: Boolean;
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
    FBlobSize: LongInt;
    FBlobDeferral: Boolean;
    FBlobPlacement: TBlobPlacement;
    FAborted:  Boolean;
    FEmptyToNull: TEmptyToNull;
    FStringTrimming: TStringTrimming;
    FBindByName: Boolean;
    FRowCountMethod: TRowCountMethod;
    FNoRowsAffected: TNoRowsAffected;
    FHstmtInsert, FHstmtUpdate, FHstmtDelete, FHstmtRefresh: TOdbcStatement;

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
    Procedure DataAtExecution(FList: TCommonPtr);
    Function GetPosOpts: SQLINTEGER;
    Function GetPosStmts: SQLINTEGER;
    Function CursorName: String;
    Procedure DetermineTargetTable;
    Procedure DeterminePrimaryCols;
    Function PrimaryClause: String;
    Procedure BindClause(Var Param: SQLUSMALLINT;
                         AHstmt: TOdbcStatement;
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

    { Get/Set Methods }
    Function GetHandle: SQLHSTMT;
    Procedure SeTOdbcConnection(AHdbc: TOdbcConnection);
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
    Function GetQueryTimeOut: SQLUINTEGER;
    Procedure SetQueryTimeOut(AQueryTimeOut: SQLUINTEGER);
    Function GetMaxRows: SQLUINTEGER;
    Procedure SetMaxRows(AMaxRows: SQLUINTEGER);
    Function GetColCount: SQLSMALLINT;
    Function GetRowCount: SQLINTEGER;
    Function GetRowsFetched: SQLUINTEGER;
    Function GetRowsAffected: SQLLEN;
    Procedure SetConcurrencyType(AConcurrencyType: SQLUINTEGER);
    Procedure SetCursorType(ACursorType: SQLUINTEGER);
    Procedure SetSkipByCursor(ASkipByCursor: Boolean);
    Procedure SetSkipByPosition(ASkipByPosition: Boolean);
    Procedure SetBlobSize(ABlobSize: LongInt);
    Function GetTableOwner: String;
    Function GetTableName: String;
    Function GetPrimaryColNames: String;
    
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
    Function GetColTimeStamp(Col: SQLUSMALLINT): fsl_utilities.TTimeStamp;
    Function GetColMemory(Col: SQLUSMALLINT): TManagedMemoryStream;
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
                              AValue: fsl_utilities.TTimeStamp);
    Procedure SetColMemory(Col: SQLUSMALLINT;
                           AValue: TManagedMemoryStream);
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
    Function GetColTimeStampByName(ColName: String): fsl_utilities.TTimeStamp;
    Function GetColMemoryByName(ColName: String): TManagedMemoryStream;
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
                                    AValue: fsl_utilities.TTimeStamp);
    Procedure SetColMemoryByName(ColName: String;
                                 AValue: TManagedMemoryStream);
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
    Function GetCellTimeStamp(Col, Row: SQLUSMALLINT): fsl_utilities.TTimeStamp;
    Function GetCellMemory(Col, Row: SQLUSMALLINT): TManagedMemoryStream;
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
                               AValue: fsl_utilities.TTimeStamp);
    Procedure SetCellMemory(Col, Row: SQLUSMALLINT;
                            AValue: TManagedMemoryStream);
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
                                    Row: SQLUSMALLINT): fsl_utilities.TTimeStamp;
    Function GetCellMemoryByName(ColName: String;
                                 Row: SQLUSMALLINT): TManagedMemoryStream;
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
                                     AValue: fsl_utilities.TTimeStamp);
    Procedure SetCellMemoryByName(ColName: String;
                                  Row: SQLUSMALLINT;
                                  AValue: TManagedMemoryStream);
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
  Public
    { Public declarations }
    constructor Create(Env : TOdbcEnv; dbc : TOdbcConnection); reintroduce; virtual;
    destructor Destroy; Override;
    Function Terminate: Boolean;
    Procedure Close; Virtual;
    Procedure CloseCursor; Virtual;
    Function ParamByName(ParamName: String): SQLUSMALLINT;
    Function ColByName(ColName: String): SQLUSMALLINT;
    Procedure NullCols(Const Cols: Array Of String);
    Procedure PrimaryCols(Const Cols: Array Of String);

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
    Function FetchFirst: Boolean;
    Function FetchNext: Boolean;
    Function FetchLast: Boolean;
    Function FetchPrev: Boolean;
    Function FetchAbsolute(Row: SQLINTEGER): Boolean;
    Function FetchRelative(Row: SQLINTEGER): Boolean;
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
                            Var ParamValue: fsl_utilities.TTimeStamp);
    Procedure BindMemory(Param: SQLUSMALLINT;
                         Var ParamValue: TManagedMemoryStream;
                         Binary: Boolean);
    Procedure BindBinary(Param: SQLUSMALLINT;
                         Var ParamValue: TManagedMemoryStream);
    Procedure BindText(Param: SQLUSMALLINT;
                       Var ParamValue: TManagedMemoryStream);

    Procedure BindNullByName(ParamName: String);
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
                                  Var ParamValue: fsl_utilities.TTimeStamp);
    Procedure BindMemoryByName(ParamName: String;
                               Var ParamValue: TManagedMemoryStream;
                               Binary: Boolean);
    Procedure BindBinaryByName(ParamName: String;
                               Var ParamValue: TManagedMemoryStream);
    Procedure BindTextByName(ParamName: String;
                             Var ParamValue: TManagedMemoryStream);

    Procedure BindNulls(Param: SQLUSMALLINT);
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
                             Var ParamValue: Array Of fsl_utilities.TTimeStamp);

    Procedure BindNullsByName(ParamName: String);
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
                                   Var ParamValue: Array Of fsl_utilities.TTimeStamp);

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
    Property QueryTimeOut: SQLUINTEGER Read GetQueryTimeOut Write SetQueryTimeOut;
    Property MaxRows: SQLUINTEGER Read GetMaxRows Write SetMaxRows;
    Property ColCount: SQLSMALLINT Read GetColCount;
    Property RowCount: SQLINTEGER Read GetRowCount;
    Property RowsFetched: SQLUINTEGER Read GetRowsFetched;
    Property RowsAffected: SQLLEN Read GetRowsAffected;

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
    Property ColTimeStamp[Col: SQLUSMALLINT]: fsl_utilities.TTimeStamp Read GetColTimeStamp Write SetColTimeStamp;
    Property ColMemory[Col: SQLUSMALLINT]: TManagedMemoryStream Read GetColMemory Write SetColMemory;
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
    Property ColTimeStampByName[ColName: String]: fsl_utilities.TTimeStamp Read GetColTimeStampByName Write SetColTimeStampByName;
    Property ColMemoryByName[ColName: String]: TManagedMemoryStream Read GetColMemoryByName Write SetColMemoryByName;
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
    Property CellTimeStamp[Col, Row: SQLUSMALLINT]: fsl_utilities.TTimeStamp Read GetCellTimeStamp Write SetCellTimeStamp;
    Property CellMemory[Col, Row: SQLUSMALLINT]: TManagedMemoryStream Read GetCellMemory Write SetCellMemory;
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
                                 Row: SQLUSMALLINT]: fsl_utilities.TTimeStamp Read GetCellTimeStampByName Write SetCellTimeStampByName;
    Property CellMemoryByName[ColName: String;
                              Row: SQLUSMALLINT]: TManagedMemoryStream Read GetCellMemoryByName Write SetCellMemoryByName;
    Property CellVariantByName[ColName: String;
                               Row: SQLUSMALLINT]: Variant Read GetCellVariantByName Write SetCellVariantByName;

    Procedure SetSpecialSQLStatementAttribute(AAttribute: SQLINTEGER; AValue: SQLPOINTER; AStringLength: SQLINTEGER);
  public
    { Published declarations }
    Property hDbc: TOdbcConnection Read FHdbc Write SeTOdbcConnection
      Default Nil;
    Property SQL: String Read FSQL Write SetSQL;
    Property SQLParsing: Boolean Read FSQLParsing Write FSQLParsing
      Default DefSQLParsing;
    Property ConcurrencyType: SQLUINTEGER Read FConcurrencyType Write SetConcurrencyType
      Default DefConcurrencyType;
    Property CursorType: SQLUINTEGER Read FCursorType Write SetCursorType
      Default DefCursorType;
    Property SkipByPosition: Boolean Read FSkipByPosition Write SetSkipByPosition
      Default DefSkipByMethod;
    Property SkipByCursor: Boolean Read FSkipByCursor Write SetSkipByCursor
      Default DefSkipByMethod;
    Property BlobSize: LongInt Read FBlobSize Write SetBlobSize
      Default DefBlobSize;
    Property BlobDeferral: Boolean Read FBlobDeferral Write FBlobDeferral
      Default DefBlobDeferral;
    Property BlobPlacement: TBlobPlacement Read FBlobPlacement Write FBlobPlacement
      Default DefBlobPlacement;
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
  TOdbcSchema = class;
  TOdbcAdministrator = class;
  TOdbcCatalog = class;


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
  public
    { public declarations }
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
  public
    { public declarations }
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
  public
    { public declarations }
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
  public
    { public declarations }
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
  public
    { public declarations }
  end;

  { TSchemaTables }
  TSchemaTables = class(TPersistent)
  private
    { Private declarations }
    FSchema: TOdbcSchema;
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
  public
    { public declarations }
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
  public
    { public declarations }
  end;

  { TSchemaViews }
  TSchemaViews = class(TPersistent)
  private
    { Private declarations }
    FSchema: TOdbcSchema;
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
  public
    { public declarations }
  end;

  { TOdbcSchema }
  TOdbcSchema = class(TODBCContext)
  private
    { Private declarations }
    FHdbc: TOdbcConnection;
    FHstmt: TOdbcStatement;
    FTables: TSchemaTables;
    FViews: TSchemaViews;
    FToFile: Boolean;
    FText: Text;
    FExecMarker: String;
    FNameConstraints: Boolean;
    FAborted: Boolean;
    FOnProgress: TStepEvent;

    procedure SeTOdbcConnection(AHdbc: TOdbcConnection);
    procedure SetTables(ATables: TSchemaTables);
    procedure SetViews(AViews: TSchemaViews);
    procedure SetExecMarker(AExecMarker: String);
    function Prefix(AOwner, ATable: String): String;
    procedure WriteLine(ctSQL: String);
    function ReadLine: String;
  protected
    { Protected declarations }
    procedure SplitColumn(AIndexColumn: string;
                          var AColumn: String;
                          var AType: String);
    procedure DoProgress(Info: String); virtual;
  public
    { Public declarations }
    constructor Create(Env : TOdbcEnv); override;
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
  public
    { public declarations }
    property hDbc: TOdbcConnection read FHdbc write SeTOdbcConnection
      default nil;
    property Tables: TSchemaTables read FTables write SetTables;
    property Views: TSchemaViews read FViews write SetViews;
    property ExecMarker: String read FExecMarker write SetExecMarker;
    property NameConstraints: Boolean read FNameConstraints write FNameConstraints;
    property OnProgress: TStepEvent read FOnProgress write FOnProgress;
  end;

  { TOdbcAdministrator }
  TOdbcAdministrator = class(TODBCContext)
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
//    procedure SetDataSource(ADataSource: String);
    procedure SetAttributes(AAttributes: TStrings);
//    function Configure(Request: SQLUSMALLINT): Boolean;
    function GetDataSources: TStrings;
    function GetDrivers: TStrings;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(Env : TOdbcEnv); override;
    destructor Destroy; override;

//    function Add: Boolean;
//    function Modify: Boolean;
//    function Remove: Boolean;
//    function Valid(ADataSource: String): Boolean;
    procedure Refresh;
    function DataSourceDriver(ADataSource: String): String;

    property DataSources: TStrings read GetDataSources;
    property Drivers: TStrings read GetDrivers;
  public
    { public declarations }
    property Prompt: Boolean read FPrompt write FPrompt
      default DefPrompt;
    property DataSourceType: TDataSourceType read FDataSourceType write SetDataSourceType
      default DefDataSourceType;
    property Driver: String read FDriver write FDriver;
//    property DataSource: String read FDataSource write SetDataSource;
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
  public
    { public declarations }
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
  public
    { public declarations }
  end;

  { TCatalogTable }
  TCatalogTable = class (TODBCContext)
  private
    { Private declarations }
    FCatalog: TOdbcCatalog;
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

    constructor Create(Env : TOdbcEnv); override;
    destructor Destroy; override;
    function ColumnByName(AColumnName: String): TCatalogColumn;
    procedure Refresh;
  public
    { public declarations }
  end;

  { TCatalogTables }
  TCatalogTables = class (TODBCContext)
  private
    { Private declarations }
    FCatalog: TOdbcCatalog;
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

    constructor Create(Env : TOdbcEnv); override;
    destructor Destroy; override;
  public
    { public declarations }
  end;

  { TCatalogProcedure }
  TCatalogProcedure = class (TODBCContext)
  private
    { Private declarations }
    FCatalog: TOdbcCatalog;
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

    constructor Create(Env : TOdbcEnv);  override;
    destructor Destroy; override;
    function ColumnByName(AColumnName: String): TCatalogColumn;
    procedure Refresh;
  public
    { public declarations }
  end;

  { TCatalogProcedures }
  TCatalogProcedures = class (TODBCContext)
  private
    { Private declarations }
    FCatalog: TOdbcCatalog;
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

    constructor Create(Env : TOdbcEnv); override;
    destructor Destroy; override;
  public
    { public declarations }
  end;

  { TOdbcCatalog }
  TOdbcCatalog = class(TODBCContext)
  private
    { Private declarations }
    FHdbc: TOdbcConnection;
    FHstmt: TOdbcStatement;
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
    procedure SeTOdbcConnection(AHdbc: TOdbcConnection);
    procedure SetTableOwner(ATableOwner: String);
    procedure SetTableName(ATableName: String);
    procedure SetTableType(ATableType: TTableTypeSet);
    procedure SetProcedureOwner(AProcedureOwner: String);
    procedure SetProcedureName(AProcedureName: String);
  protected
  public
    { Public declarations }
    constructor Create(Env : TOdbcEnv; dbc : TOdbcConnection); reintroduce; virtual;
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
  public
    { public declarations }
    property hDbc: TOdbcConnection read FHdbc write SeTOdbcConnection
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
                          Hstmt: TOdbcStatement) of object;



{ Public Utilities }

function TableTypeToString(TableType: TTableType): String;
function StringToTableType(S: String): TTableType;
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
                     CType: SQLSMALLINT): fsl_utilities.TTimeStamp;
function NullTS: TTimeStamp;

{ Private Utilities }
Function OffsetPointer(P: Pointer;
                       Ofs: LongInt): Pointer;
Function TrimString(S: String;
                    StringTrimming: TStringTrimming): String;

Function LeadingZeros(Num: Integer;
                      Zeros: Integer): String;
Function IndexOf(S: String;
                 List: TStrings): Integer;
Function Compare(S1, S2: String): Boolean;
Procedure InsertNulls(Var S: String);
Function Quoted(S: String): String;
Function MemoryStreamToString(M: TManagedMemoryStream): String;
Procedure SplitList(S, Separator: String;
                Var Strings: TStrings);
Procedure Log(ErrorNum: Integer;
              Message: String);

Implementation

{$IFNDEF VER130}
 Uses
   Variants;
{$ENDIF}

Const
  MinBlobSize = 1024;
  Fractional = 9;  //yyyy-mm-dd hh:mm:ss.fffffffff = 20+Fractional

{ Public Utilities }

Function LeadingZeros(Num: Integer;
                      Zeros: Integer): String;
Var
  i: Integer;
Begin
  Result:= IntToStr(Num);
  For i:= Length(Result) To Zeros Do
    Result:= '0'+Result;
End;

Function IndexOf(S: String;
                 List: TStrings): Integer;
Var
  i: Integer;
Begin
  S:= UpperCase(S);

  Result:= -1;
  For i:= 0 To List.Count-1 Do
    If S = UpperCase(List[i]) Then
    Begin
      Result:= i;
      Break;
    End;
End;

Function Compare(S1, S2: String): Boolean;
Var
  V1, V2: Double;
  ErrCode: Integer;
Begin
  Val(S1, V1, ErrCode);
  If ErrCode = 0 Then
  Begin
    Val(S2, V2, ErrCode);
    If ErrCode = 0 Then
    Begin
      Result:= V1 = V2;
      Exit;
    End;
  End;

  S1:= UpperCase(TrimRight(S1));
  S2:= UpperCase(TrimRight(S2));
  Result:= S1 = S2;
End;

Procedure InsertNulls(Var S: String);
Var
  i, Count: Integer;
Begin
  Count:= Length(S);
  For i:= 1 To Count Do
    If S[i] = #1 Then
      S[i]:= #0;
End;

Function Quoted(S: String): String;
Begin
  If (Pos(' AS ', UpperCase(S)) > 0) Or  //already renamed
     (Pos('{FN ', UpperCase(S)) > 0) Or  //function call
     (Pos('"', S) > 0) Then              //already quoted
    Result:= S
  Else If (Pos(' ', S) > 0) Or   //spaced table name
          (Pos('$', S) > 0) Then  //for excel
    Result:= '"'+S+'"'
  Else
    Result:= S;
End;

Function MemoryStreamToString(M: TManagedMemoryStream): String;
Var
  NewCapacity: {$IFDEF FPC} {$IFDEF CPU64} Int64 {$ELSE} Longint {$ENDIF} {$ELSE} LongInt {$ENDIF};
Begin
  If (M.Size = 0) Or (M.Memory = Nil) Then
    Result:= ''
  Else
  Begin
    If M.Capacity = M.Size Then
    Begin
      NewCapacity:= M.Size+1;
      M.Realloc(NewCapacity);
    End;
    pchar(M.Memory)[M.Size]:= #0;
    {$IFNDEF UNICODE}
    Result:= StrPas(M.Memory);
    {$ENDIF}
  End;
End;

Procedure SplitList(S, Separator: String;
                Var Strings: TStrings);
Var
  Loc: Integer;
Begin
  Strings.Clear;
  Loc:= Pos(Separator, S);
  While Loc > 0 Do
  Begin
    Strings.Add(Copy(S, 1, Loc-1));
    Delete(S, 1, Loc);

    Loc:= Pos(Separator, S);
  End;
  Strings.Add(S);
End;

Procedure Log(ErrorNum: Integer;
              Message: String);
Begin
  { Logging Procedure }
End;

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
      Result:= DefaultStringSize; // SizeOf(NullString);

    SQL_C_BINARY:
      Result:= DefaultStringSize; // SizeOf(NullString);

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
        With fsl_utilities.TTimeStamp(CValue^) Do
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
      With fsl_utilities.TTimeStamp(CValue^) Do
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
                     CType: SQLSMALLINT): fsl_utilities.TTimeStamp;
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
      Result:= fsl_utilities.TTimeStamp(CValue^);
    End;
  End;
End;

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


Function OffsetPointer(P: Pointer;
                       Ofs: LongInt): Pointer;
Begin
  Result:= Pointer(NativeUInt(P)+NativeUInt(Ofs));
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

function fromOdbcPChar(p : PChar; length : integer) : String;
  {$IFDEF OSX}
var
  i : integer;
  {$ENDIF}
begin
  {$IFDEF OSX}
  SetLength(result, length);
  for i := 1 to length do
    result[i] := p[(i-1)*2];
  {$ELSE}
  result := p;
  {$ENDIF}
end;

function odbcPChar(s : String) : PChar; overload;
begin
  if s = '' then
    result := nil
  else
  begin
    {$IFDEF FPC}
    getMem(result, (length(s)+1));
    fillChar(result^, (length(s)+1), 0);
    move(s[1], result^, length(s));
    {$ELSE}
    {$IFDEF OSX}
    getMem(result, (length(s)+1) * 4);
    fillChar(result^, (length(s)+1)*4, 0);
    {$ELSE}
    getMem(result, (length(s)+1) * 2);
    fillChar(result^, (length(s)+1)*2, 0);
    move(s[1], result^, length(s) * 2);
    {$ENDIF}
    {$ENDIF}
  end;
end;

function odbcPChar(count : integer) : PChar; overload;
begin
  {$IFDEF OSX}
  getMem(result, count);
  {$ELSE}
  getMem(result, count);
  {$ENDIF}
end;

{ TODBCErrorHandler }

Function TODBCErrorHandler.Errors(ARetCode: SQLRETURN; aHandleType: SQLSMALLINT; aHandle: SQLHANDLE): TList;
Var
  ErrorNum: Integer;
  ErrorPtr: TErrorPtr;

  RetCode: SQLRETURN;
  State: PChar;
  Native: SQLINTEGER;
  Message: PChar;
  StringLength: SQLSMALLINT;
Begin
  State := odbcPChar(DefaultStringSize);
  Message := odbcPChar(DefaultStringSize);
  try
    Result:= TList.Create;
    Result.Clear;

    Case ARetCode Of
      SQL_ERROR, -24238,
      SQL_SUCCESS_WITH_INFO:
      Begin
        ErrorNum:= 0;
        Repeat
          Inc(ErrorNum);

          //depreciated RetCode:= _SQLError(FErrHenv, FErrHdbc, FErrHstmt, SqlState, NativeError, ErrorMsg);
          RetCode:= SQLGetDiagRec(aHandleType, aHandle, ErrorNum, State, Native, Message, DefaultStringSize, StringLength);

          If Success(RetCode) Then
          Begin
            New(ErrorPtr);
            ErrorPtr.FState:= fromOdbcPChar(State, 5);
            ErrorPtr.FNative:= Native;
            ErrorPtr.FMessage:= fromOdbcPChar(Message, StringLength);
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
  finally
    freemem(state);
    freemem(message);
  end;
End;

Procedure TODBCErrorHandler.RaiseError(AOwner: TODBCObject; ARetCode: SQLRETURN);
Var
  aHandleType: SQLSMALLINT;
  aHandle: SQLHANDLE;
Begin
  If AOwner Is TOdbcEnv Then
  Begin
    aHandleType := SQL_HANDLE_ENV;
    aHandle:= TOdbcEnv(AOwner).Handle
  End
  Else If AOwner Is TOdbcConnection Then
  Begin
    aHandleType:= SQL_HANDLE_DBC;
    aHandle:= TOdbcConnection(AOwner).Handle
    End
  Else // If AOwner Is TOdbcStatement Then
    Begin
    aHandleType:= SQL_HANDLE_STMT;
    aHandle:= TOdbcStatement(AOwner).Handle;
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


{ TOdbcEnv }

Constructor TOdbcEnv.Create;
Begin
  Inherited Create;

  { Create Error Object }
  FError := TODBCErrorHandler.Create;

  FActive:= False;

  Init;
End;

Destructor TOdbcEnv.Destroy;
Begin
  Inherited Destroy;

  { Terminate Self }
  TerminateHandle;

  FError.Free;
End;

Function TOdbcEnv.Init: Boolean;
Begin
  Log(1, 'TOdbcEnv.Init');

  If FActive Then
  Begin
    Init:= FActive;
    Exit;
  End;

  { Create Handle }
  FRetCode:= SQLAllocHandle(SQL_HANDLE_ENV, Pointer(SQL_NULL_HANDLE), FHenv);
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

Function TOdbcEnv.TerminateHandle: Boolean;
Begin
  { Terminate Self }
  If FActive Then
  Begin
    { Free Handle }
    FRetCode:= SQLFreeHandle(SQL_HANDLE_ENV, FHenv);
    try
      If Not FError.Success(FRetCode) Then
        FError.RaiseError(Self, FRetCode);
    except
      // cause at this point, we really don't want to know about it
    end;
    FHenv := Nil;

    { Set Active Field }
    FActive:= False;
  End;

  Result:= Not FActive;
End;

{ TOdbcConnection }

Function TOdbcConnection.GetCore: Boolean;
Begin
  Connect;

  Result:= FCore;
End;

Procedure TOdbcConnection.SetCore(ACore: Boolean);
Begin
  Connect;

  FCore:= ACore;
End;

Function TOdbcConnection.GetLoginTimeOut: SQLUINTEGER;
var
  len : integer;
Begin
  Init;

  FRetCode:= SQLGetConnectAttr(FHdbc, SQL_ATTR_LOGIN_TIMEOUT, @Result, SizeOf(Result), {$IFDEF FPC}@{$ENDIF}len);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure TOdbcConnection.SetLoginTimeOut(ALoginTimeOut: SQLUINTEGER);
Begin
  Init;

  FRetCode:= SQLSetConnectAttr(FHdbc, SQL_ATTR_LOGIN_TIMEOUT, Pointer(ALoginTimeOut), SizeOf(ALoginTimeOut));
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure TOdbcConnection.SetCursorLib(ACursorLib: SQLUINTEGER);
Var
  LCursorLib: SQLULEN;
  len : integer;
Begin
  Log(1, 'TOdbcConnection.SetCursorLib');

  If FConnected Then
    Disconnect;

  { Set Cursor Library }
  FCursorLib:= ACursorLib;

  If FActive Then
  Begin
    FRetCode:= SQLGetConnectAttr(FHdbc, SQL_ATTR_ODBC_CURSORS, @LCursorLib, SizeOf(LCursorLib), {$IFDEF FPC}@{$ENDIF}len);
    If FEnv.Error.Success(FRetCode) And (LCursorLib <> FCursorLib) Then
    Begin
      FRetCode:= SQLSetConnectAttr(FHdbc, SQL_ATTR_ODBC_CURSORS, Pointer(FCursorLib), SizeOf(FCursorLib));
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
    End;
  End;
End;

Function TOdbcConnection.GetInTransaction: Boolean;
Var
  LInTransaction: SQLUINTEGER;
  len : integer;
Begin
  FRetCode:= SQLGetConnectAttr(FHdbc, SQL_ATTR_AUTOCOMMIT, @LInTransaction, SizeOf(LInTransaction), {$IFDEF FPC}@{$ENDIF}len);
  If Not FEnv.Error.Success(FRetCode) Then
    Result:= False
  Else
    Result:= LInTransaction = SQL_AUTOCOMMIT_OFF;
End;

Procedure TOdbcConnection.SetDriver(ADriver: String);
Begin

  FDriver:= ADriver;
End;

Procedure TOdbcConnection.SetDataSource(ADataSource: String);
Begin

  FDataSource:= ADataSource;
End;

Procedure TOdbcConnection.SetUserName(AUserName: String);
Begin

  FUserName:= AUserName;
End;

Procedure TOdbcConnection.SetPassword(APassword: String);
Begin

  FPassword:= APassword;
End;

Procedure TOdbcConnection.SetAttributes(AAttributes: TStrings);
Begin

  FAttributes.Assign(AAttributes);
End;

Procedure TOdbcConnection.SetIsolationLevel(AIsolationLevel: SQLUINTEGER);
Var
  LIsolationLevel: SQLUINTEGER;
  len : integer;
Begin
  Log(1, 'TOdbcConnection.SetIsolationLevel');

  { Set Isolation Level }
  FIsolationLevel:= AIsolationLevel;

  If FActive Then
  Begin
    FRetCode:= SQLGetConnectAttr(FHdbc, SQL_ATTR_TXN_ISOLATION, @LIsolationLevel, SizeOf(LIsolationLevel), {$IFDEF FPC}@{$ENDIF}len);
    If FEnv.Error.Success(FRetCode) And (LIsolationLevel <> FIsolationLevel) Then
    Begin
      FRetCode:= SQLSetConnectAttr(FHdbc, SQL_ATTR_TXN_ISOLATION, Pointer(FIsolationLevel), 0);
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
    End;
  End;
End;


Procedure TOdbcConnection.DoBeforeConnect;
Begin
  If Assigned(FBeforeConnect) Then
    FBeforeConnect(Self);
End;

Procedure TOdbcConnection.DoAfterConnect;
Begin
  If Assigned(FAfterConnect) Then
    FAfterConnect(Self);
End;

Procedure TOdbcConnection.DoBeforeDisconnect;
Begin
  If Assigned(FBeforeDisconnect) Then
    FBeforeDisconnect(Self);
End;

Procedure TOdbcConnection.DoAfterDisconnect;
Begin
  If Assigned(FAfterDisconnect) Then
    FAfterDisconnect(Self);
End;

Constructor TOdbcConnection.Create(Env : TOdbcEnv);
Begin
  Inherited Create(Env);


  { Set Defaults }
  FActive:= False;
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
  FCore:= DefCore;
  FDrivers:= TList.Create;
  RefreshDrivers;

  Init;
End;

Destructor TOdbcConnection.Destroy;
Begin
  Inherited Destroy;

  { Terminate Self }
  FAttributes.Free;
  ClearDrivers;
  FDrivers.Free;
  TerminateHandle;
End;

Function TOdbcConnection.Init: Boolean;
Begin
  Log(1, 'TOdbcConnection.Init');

  If FActive Then
  Begin
    Init:= FActive;
    Exit;
  End;

  { Create Handle }
  FRetCode:= SQLAllocHandle(SQL_HANDLE_DBC, FEnv.Handle, FHdbc);
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

Function TOdbcConnection.GetHandle: SQLHDBC;
Begin
  Init;

  { Retrieve Handle }
  Result:= FHdbc;
End;

Function TOdbcConnection.GetConnected: Boolean;
Begin
  Result:= FConnected;
End;

Procedure TOdbcConnection.SetConnected(AConnected: Boolean);
Begin
    If AConnected Then
      Connect
    Else
      Disconnect;
  End;

Procedure TOdbcConnection.Connect;
Var
  ConnectStrIn: PChar;
  ConnectStrOut: PChar;
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
  Log(1, 'TOdbcConnection.Connect');

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
      ConnectStrOut := odbcPChar(DefaultStringSize);
      ConnectStrIn := odbcPChar(ConnectStr);
      try
        FRetCode:= SQLDriverConnect(FHdbc, 0, ConnectStrIn, SQL_NTS,
                                                                 ConnectStrOut, DefaultStringSize, StringLength,
                                                                 FInfoPrompt);
      finally
        freemem(ConnectStrOut);
        FreeMem(ConnectStrIn);
      end;
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

Procedure TOdbcConnection.Disconnect;
Begin
  Log(1, 'TOdbcConnection.Disconnect');

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

Procedure TOdbcConnection.StartTransact;
Begin
  Connect;

  FRetCode:= SQLSetConnectAttr(FHdbc, SQL_ATTR_AUTOCOMMIT, Pointer(SQL_AUTOCOMMIT_OFF), SizeOf(SQLUINTEGER));
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

procedure TOdbcConnection.TerminateHandle;
begin
  FRetCode:= SQLFreeHandle(SQL_HANDLE_DBC, FHdbc);
  If Not FEnv.FError.Success(FRetCode) Then
    FEnv.FError.RaiseError(Self, FRetCode);
  FHdbc := Nil;
end;

Procedure TOdbcConnection.EndTransact;
Begin
  Commit;

  FRetCode:= SQLSetConnectAttr(FHdbc, SQL_ATTR_AUTOCOMMIT, Pointer(SQL_AUTOCOMMIT_ON), SizeOf(SQLUINTEGER));
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure TOdbcConnection.Commit;
Begin
  Connect;

  FRetCode:= SQLEndTran(SQL_HANDLE_DBC, FHdbc, SQL_COMMIT);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure TOdbcConnection.Rollback;
Begin
  Connect;

  FRetCode:= SQLEndTran(SQL_HANDLE_DBC, FHdbc, SQL_ROLLBACK);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Function TOdbcConnection.GetFunction(FunctionID: SQLUSMALLINT): Boolean;
Var
  Supported: SQLUSMALLINT;
Begin
  If (FunctionID = SQL_API_ALL_FUNCTIONS) Or
     (FunctionID = SQL_API_ODBC3_ALL_FUNCTIONS) Then
    Raise EODBCExpress.Create('Cannot return information for more than one function at a time.');

  Connect;

  FRetCode:= SQLGetFunctions(FHdbc, FunctionID, {$IFDEF FPC}@{$ENDIF}Supported);
  If Not FEnv.Error.Success(FRetCode) Then
    Supported:= 0;

  Result := Supported <> 0;
End;

Function TOdbcConnection.GetInfoString(InfoType: SQLUSMALLINT): String;
Var
  Supported: PChar;
  StringLength: SQLSMALLINT;
Begin
  Connect;

  Supported := odbcPChar(DefaultStringSize);
  try
    FRetCode:= SQLGetInfo(FHdbc, InfoType, Supported, DefaultStringSize, {$IFDEF FPC}@{$ENDIF}StringLength);
    If Not FEnv.Error.Success(FRetCode) Then
      Result:= ''
    Else
      Result:= fromOdbcPChar(Supported, StringLength);
  finally
    FreeMem(supported);
  end;
End;

Function TOdbcConnection.GetInfoSmallint(InfoType: SQLUSMALLINT): SQLUSMALLINT;
Var
  Supported: SQLUSMALLINT;
  len : SmallInt;
Begin
  Connect;

  FRetCode:= SQLGetInfo(FHdbc, InfoType, @Supported, SizeOf(Supported), {$IFDEF FPC}@{$ENDIF}len);
  If Not FEnv.Error.Success(FRetCode) Then
    Result:= 0
  Else
    Result:= Supported;
End;

Function TOdbcConnection.GetInfoInteger(InfoType: SQLUSMALLINT): SQLUINTEGER;
Var
  Supported: SQLUINTEGER;
  len : SmallInt;
Begin
  Connect;

  FRetCode:= SQLGetInfo(FHdbc, InfoType, @Supported, SizeOf(Supported), {$IFDEF FPC}@{$ENDIF}len);
  If Not FEnv.Error.Success(FRetCode) Then
    Result:= 0
  Else
    Result:= Supported;
End;

Function TOdbcConnection.GetVersion: String;
Begin
  Result:= '1.0';
End;

Procedure TOdbcConnection.SetVersion(AVersion: String);
Begin
End;

Procedure TOdbcConnection.RefreshDrivers;
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

Procedure TOdbcConnection.ClearDrivers;
Var
  i: Integer;
Begin
  For i:= 0 To FDrivers.Count-1 Do
    Dispose(TDriverPtr(FDrivers[i]));
  FDrivers.Clear;
End;

Function TOdbcConnection.AddDriver(ADriver: String): TDriverPtr;
Begin
  New(Result);
  FDrivers.Add(Result);

  With Result^ Do
  Begin
    Desc:= ADriver;
    PS_SQL_CHAR:= DefaultStringSize;
    PS_SQL_VARCHAR:= DefaultStringSize;
    PS_SQL_LONGVARCHAR:= MaxLongint;
    PS_SQL_BINARY:= DefaultStringSize;
    PS_SQL_VARBINARY:= DefaultStringSize;
    PS_SQL_LONGVARBINARY:= MaxLongint;
    PS_SQL_DECIMAL:= 15;
    PS_SQL_NUMERIC:= 15;
    PS_SQL_TYPE_TIMESTAMP:= 19;
    DD_SQL_DECIMAL:= 15;
    DD_SQL_NUMERIC:= 15;
    DD_SQL_TYPE_TIMESTAMP:= 0;
  End;
End;

Procedure TOdbcConnection.RemoveDriver(ADriver: String);
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

Function TOdbcConnection.GetDriver(ADriver: String): TDriverPtr;
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

Function TOdbcConnection.CurrentDriver: String;
Begin
  Result:= GetInfoString(SQL_DBMS_NAME)+';'+GetInfoString(SQL_DRIVER_NAME);
End;

Function TOdbcConnection.IsDriver(Const ADrivers: Array Of String): Boolean;
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

{ TOdbcStatement }

Procedure TOdbcStatement.FreeParams;
Var
  temp: TParamPtr;
Begin
  While FParams <> Nil Do
  Begin
    temp:= FParams^.Next;
    FreeMem(FParams^.FSize); // , FParams^.FCount*SizeOf(SQLINTEGER));
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

Procedure TOdbcStatement.FreeCols;
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
      begin
        if (FCols^.FColumnSize = 0) then
          FreeMem(FCols^.FValue, (DefaultStringSize+1))
        else
          FreeMem(FCols^.FValue, (FCols^.FColumnSize+1))
      end
      Else If (FCols^.FSql = SQL_BINARY) Or (FCols^.FSql = SQL_VARBINARY) Then
        FreeMem(FCols^.FValue, FCols^.FColumnSize)
      Else
        FreeMem(FCols^.FValue, PhysSize(FCols^.FType));
    End;
    FreeMem(FCols^.FSize, PhysSize(SQL_C_SLONG));
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

Procedure TOdbcStatement.FreeColBinds;
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

Procedure TOdbcStatement.UnPrepareHstmts;
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

Procedure TOdbcStatement.InsertHead(FParam: SQLUSMALLINT;
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

Procedure TOdbcStatement.InsertTail(Var FTail: TColPtr;
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
  GetMem(temp^.FSize, PhysSize(SQL_C_SLONG));
  temp^.FSize^:= SQL_NULL_DATA;
  temp^.FBlob:= (FSql = SQL_LONGVARCHAR) Or (FSql = SQL_LONGVARBINARY) Or (FSql = SQL_VARBINARY);
  temp^.FBlobFetched:= False;
  temp^.FMemory:= Nil;
  If temp^.FBlob Then
    temp^.FMemory:= TManagedMemoryStream.Create;


  temp^.FFormatStyle:= DefFormatStyle;
  temp^.FFormatMask:= DefFormatMask;
  temp^.FPrimary:= DefPrimary;
  
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

Procedure TOdbcStatement.InsertColBind(FCol: SQLUSMALLINT;
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

Procedure TOdbcStatement.SeTOdbcConnection(AHdbc: TOdbcConnection);
Begin
  Log(1, 'TOdbcStatement.SeTOdbcConnection');

  Terminate;

  FHdbc:= AHdbc;
End;

Procedure TOdbcStatement.SetSQL(ASQL: String);
Begin
  Log(1, 'TOdbcStatement.SetSQL');

  { Reset Hstmt }
  If FActive Then
    Close;

  FPrepared:= False;
  FExecuted:= False;

  FSQL:= ASQL;
End;

Procedure TOdbcStatement.SetPrepared(APrepared: Boolean);
Begin
  If APrepared Then
    Prepare
  Else
    FPrepared:= False;
End;

Procedure TOdbcStatement.SetExecuted(AExecuted: Boolean);
Begin
  If AExecuted Then
    Execute
  Else
    FExecuted:= False;
End;

Function TOdbcStatement.GetParamSize(Param: SQLUSMALLINT): SQLINTEGER;
Var
  temp: TParamPtr;
Begin
  Log(1, 'TOdbcStatement.GetParamSize');

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

Procedure TOdbcStatement.SetParamSize(Param: SQLUSMALLINT;
                              AParamSize: SQLINTEGER);
Var
  temp: TParamPtr;
Begin
  Log(1, 'TOdbcStatement.SetParamSize');

  temp:= ParamRec(Param);
  If temp <> Nil Then
  Begin
    If (temp^.FSize^ <= SQL_LEN_DATA_AT_EXEC_OFFSET) And (AParamSize >= 0) Then
      temp^.FSize^:= SQL_LEN_DATA_AT_EXEC_OFFSET-AParamSize
    Else
      temp^.FSize^:= AParamSize;
  End;
End;

Procedure TOdbcStatement.SetParamType(AParamType: SQLSMALLINT);
Begin
  Log(1, 'TOdbcStatement.SetParamType');

  If AParamType In [SQL_PARAM_INPUT, SQL_PARAM_OUTPUT, SQL_PARAM_INPUT_OUTPUT] Then
    FParamType:= AParamType;
End;

Procedure TOdbcStatement.SetBulkSize(ABulkSize: SQLUINTEGER);
Begin
  Log(1, 'TOdbcStatement.SetBulkSize');

  If ABulkSize > 0 Then
    FNumParams:= ABulkSize;
End;

Function TOdbcStatement.GetBulkParamSize(Param: SQLUSMALLINT;
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

Procedure TOdbcStatement.SetBulkParamSize(Param: SQLUSMALLINT;
                                  Row: SQLUINTEGER;
                                  AParamSize: SQLINTEGER);
Var
  temp: TParamPtr;
Begin
  temp:= ParamRec(Param);

  If (temp <> Nil) And (Row > 0) And (Row <= temp^.FCount) Then
    SQLINTEGER(OffsetPointer(temp^.FSize, (Row-1)*SizeOf(SQLINTEGER))^):= AParamSize;
End;

Procedure TOdbcStatement.SetTargetTable(ATargetTable: String);
Begin
  Log(1, 'TOdbcStatement.SetTargetTable');

  FTargetTable:= ATargetTable;
End;

Function TOdbcStatement.GetColSize(Col: SQLUSMALLINT): SQLINTEGER;
Begin
  Result:= GetCellSize(Col, 1);
End;

Procedure TOdbcStatement.SetColSize(Col: SQLUSMALLINT;
                            AColSize: SQLINTEGER);
Begin
  SetCellSize(Col, 1, AColSize);
End;

Function TOdbcStatement.GetCellSize(Col, Row: SQLUSMALLINT): SQLINTEGER;
Var
  tempCol: TColPtr;
//  temp: TRowPtr;
//  LrRowRec : TRowRec;
Begin
  tempCol:= ColRec(Col);

  If (tempCol <> Nil) And
     (Row > 0) And (Row <= 1) Then
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

Procedure TOdbcStatement.SetCellSize(Col, Row: SQLUSMALLINT;
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

Function TOdbcStatement.GetColNull(Col: SQLUSMALLINT): Boolean;
Begin
  Result:= ColSize[Col] = SQL_NULL_DATA;
End;

Procedure TOdbcStatement.SetColNull(Col: SQLUSMALLINT;
                            AColNull: Boolean);
Begin
  If AColNull Then
    ColSize[Col]:= SQL_NULL_DATA;
End;

Function TOdbcStatement.GetCellNull(Col, Row: SQLUSMALLINT): Boolean;
Begin
  Result:= CellSize[Col,Row] = SQL_NULL_DATA;
End;

Procedure TOdbcStatement.SetCellNull(Col, Row: SQLUSMALLINT;
                             ACellNull: Boolean);
Begin
  If ACellNull Then
    CellSize[Col,Row]:= SQL_NULL_DATA;
End;

Function TOdbcStatement.GetFormatStyle(Col: SQLUSMALLINT): TFormatStyle;
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp = Nil Then
    Result:= fsNone
  Else
    Result:= temp^.FFormatStyle;
End;

Procedure TOdbcStatement.SetFormatStyle(Col: SQLUSMALLINT;
                                AFormatStyle: TFormatStyle);
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp <> Nil Then
    temp^.FFormatStyle:= AFormatStyle;
End;

Function TOdbcStatement.GetFormatMask(Col: SQLUSMALLINT): String;
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp = Nil Then
    Result:= ''
  Else
    Result:= temp^.FFormatMask;
End;

Procedure TOdbcStatement.SetFormatMask(Col: SQLUSMALLINT;
                               AFormatMask: String);
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp <> Nil Then
    temp^.FFormatMask:= AFormatMask;
End;

Function TOdbcStatement.GetColPrimary(Col: SQLUSMALLINT): Boolean;
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp = Nil Then
    Result:= False
  Else
    Result:= temp^.FPrimary;
End;

Procedure TOdbcStatement.SetColPrimary(Col: SQLUSMALLINT;
                               AColPrimary: Boolean);
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp <> Nil Then
    temp^.FPrimary:= AColPrimary;
End;

Function TOdbcStatement.GetColPrecision(Col: SQLUSMALLINT): SQLUINTEGER;
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp = Nil Then
    Result:= 0
  Else
    Result:= temp^.FColumnSize;
End;

Function TOdbcStatement.GetColScale(Col: SQLUSMALLINT): SQLSMALLINT;
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp = Nil Then
    Result:= 0
  Else
    Result:= temp^.FDecimalDigits;
End;

Function TOdbcStatement.GetColNullable(Col: SQLUSMALLINT): SQLSMALLINT;
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);

  If temp = Nil Then
    Result:= SQL_NULLABLE_UNKNOWN
  Else
    Result:= temp^.FNullable;
End;

Function TOdbcStatement.GetParamPrecision(Param: SQLUSMALLINT): SQLUINTEGER;
Var
  temp: TParamPtr;
Begin
  temp:= ParamRec(Param);

  If temp = Nil Then
    Result:= 0
  Else
    Result:= temp^.FParameterSize;
End;

Function TOdbcStatement.GetParamScale(Param: SQLUSMALLINT): SQLSMALLINT;
Var
  temp: TParamPtr;
Begin
  temp:= ParamRec(Param);

  If temp = Nil Then
    Result:= 0
  Else
    Result:= temp^.FDecimalDigits;
End;

Function TOdbcStatement.GetParamNullable(Param: SQLUSMALLINT): SQLSMALLINT;
Var
  temp: TParamPtr;
Begin
  temp:= ParamRec(Param);

  If temp = Nil Then
    Result:= SQL_NULLABLE_UNKNOWN
  Else
    Result:= temp^.FNullable;
End;

Function TOdbcStatement.GetRowStatus(Row: SQLUSMALLINT): SQLUSMALLINT;
Begin
  If (Row > 0) And (Row <= 1) Then
    Result:= SQLUSMALLINT(OffsetRow(FRowStatus^.FValue, Row, PhysSize(SQL_C_USHORT))^)
  Else
    Result:= SQL_ROW_NOROW;
End;

Function TOdbcStatement.GetRowFlag(Row: SQLUSMALLINT): SQLUSMALLINT;
Var
  Flags: SQLUSMALLINTPtr;
Begin
  Flags:= RowFlags(Row);
  If Flags = Nil Then
    Result:= rfNone
  Else
    Result:= Flags^ And rfOps;
End;

Procedure TOdbcStatement.SetRowFlag(Row: SQLUSMALLINT;
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

Function TOdbcStatement.GetParamNames: TStringList;
Begin
  If Not FPrepared Then
    Begin
    ParseSQL;
    End;

  Result:= FParamNames;
End;

Function TOdbcStatement.GetColNames: TStringList;
Begin
  Result:= FColNames;
End;


Function TOdbcStatement.GetRowValid(Row: SQLUSMALLINT): Boolean;
Begin
  Result:= RowStatus[Row] In [SQL_ROW_SUCCESS, SQL_ROW_SUCCESS_WITH_INFO,
                              SQL_ROW_ADDED, SQL_ROW_UPDATED];
End;

Function TOdbcStatement.GetQueryTimeOut: SQLUINTEGER;
var
  len : integer;
  res : PtrUInt;
Begin
  Init;

  FRetCode:= SQLGetStmtAttr(FHstmt, SQL_ATTR_QUERY_TIMEOUT, @res, SizeOf(res), {$IFDEF FPC}@{$ENDIF}len);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
  result := res;
End;

Procedure TOdbcStatement.SetQueryTimeOut(AQueryTimeOut: SQLUINTEGER);
Begin
  Init;

  FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_QUERY_TIMEOUT, Pointer(AQueryTimeOut), SizeOf(AQueryTimeOut));
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure TOdbcStatement.SetSpecialSQLStatementAttribute(AAttribute: SQLINTEGER; AValue: SQLPOINTER; AStringLength: SQLINTEGER);
Begin
  Init;

  FRetCode:= SQLSetStmtAttr(FHstmt, AAttribute, AValue, AStringLength);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Function TOdbcStatement.GetMaxRows: SQLUINTEGER;
var
  len : integer;
  res : PtrUInt;
Begin
  Init;

  FRetCode:= SQLGetStmtAttr(FHstmt, SQL_ATTR_MAX_ROWS, @Res, SizeOf(Res), {$IFDEF FPC}@{$ENDIF}len);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
  result := res;
End;

Procedure TOdbcStatement.SetMaxRows(AMaxRows: SQLUINTEGER);
Begin
  Init;

  FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_MAX_ROWS, Pointer(AMaxRows), SizeOf(AMaxRows));
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure TOdbcStatement.SetConcurrencyType(AConcurrencyType: SQLUINTEGER);
Var
  LConcurrencyType: PtrUInt;
  len : integer;
Begin
  Log(1, 'TOdbcStatement.SetConcurrencyType');

  FConcurrencyType:= AConcurrencyType;

  If FActive Then
  Begin
    Close;

    FRetCode:= SQLGetStmtAttr(FHstmt, SQL_ATTR_CONCURRENCY, @LConcurrencyType, SizeOf(LConcurrencyType), {$IFDEF FPC}@{$ENDIF}len);
    If FEnv.Error.Success(FRetCode) And (LConcurrencyType <> FConcurrencyType) Then
    Begin
      FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_CONCURRENCY, pointer(FConcurrencyType), SizeOf(FConcurrencyType));
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
    End;
  End;
End;

Procedure TOdbcStatement.SetCursorType(ACursorType: SQLUINTEGER);
Var
  LCursorType: PtrUInt;

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
var
  len : integer;
Begin
  Log(1, 'TOdbcStatement.SetCursorType');

  FCursorType:= ACursorType;

  If FActive Then
  Begin
    Close;
    GetCursorAttr;

    FRetCode:= SQLGetStmtAttr(FHstmt, SQL_ATTR_CURSOR_TYPE, @LCursorType, SizeOf(LCursorType), {$IFDEF FPC}@{$ENDIF}len);
    If FEnv.Error.Success(FRetCode) And (LCursorType <> FCursorType) Then
    Begin
      FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_CURSOR_TYPE, Pointer(FCursorType), SizeOf(FCursorType));
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
    End;
  End;
End;

Procedure TOdbcStatement.SetBlobSize(ABlobSize: LongInt);
Begin
  Log(1, 'TOdbcStatement.SetBlobSize');

  If (ABlobSize <> FBlobSize) And (ABlobSize > MinBlobSize) Then
    FBlobSize:= ABlobSize;
End;

Constructor TOdbcStatement.Create(Env : TOdbcEnv; dbc : TOdbcConnection);
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
  FNumCols:= 0;
  FNumRows:= 0;
  FNumParams:= 0;
  FBulkData:= DefBulkData;
  FBlobs:= False;
  FColumnsBound:= False;

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
  FBlobSize:= DefBlobSize;
  FBlobDeferral:= DefBlobDeferral;
  FBlobPlacement:= DefBlobPlacement;
  FEmptyToNull:= DefEmptyToNull;
  FStringTrimming:= DefStringTrimming;
  FBindByName:= DefBindByName;
  FRowCountMethod:= DefRowCountMethod;
  FNoRowsAffected:= DefNoRowsAffected;
  FAborted:= False;
End;

Destructor TOdbcStatement.Destroy;
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

Function TOdbcStatement.Init: Boolean;
Begin
  Log(1, 'TOdbcStatement.Init');

  If FActive Then
  Begin
    Init:= FActive;
    Exit;
  End;

  Hdbc.Connect;

  { Create Handle }
  FRetCode:= SQLAllocHandle(SQL_HANDLE_STMT, Hdbc.Handle, FHstmt);
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

  Result:= FActive;
End;

Function TOdbcStatement.Terminate: Boolean;
Begin
  Log(1, 'TOdbcStatement.Terminate');

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

Procedure TOdbcStatement.CloseCursor;
Begin
  Init;

  { Reset Statement Handle }
  FRetCode:= SQLFreeStmt(FHstmt, SQL_CLOSE);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
End;

Procedure TOdbcStatement.Close;
Begin
  Log(1, 'TOdbcStatement.Close');

  CloseCursor;

  { Dispose of Storage }
  FreeParams;
  FreeCols;

  UnPrepareHstmts;

  FPrepared:= False;
  FExecuted:= False;
End;

Function TOdbcStatement.GetHandle: SQLHSTMT;
Begin
  Init;

  { Retrieve Handle }
  Result:= FHstmt;
End;

Function TOdbcStatement.GetColCount: SQLSMALLINT;
Begin
  Log(1, 'TOdbcStatement.NumCols');

  Init;

  { Bind Columns }
  BindCols;

  Result:= FNumCols;
End;

Function TOdbcStatement.GetRowCount: SQLINTEGER;
Var
  tempHstmt: TOdbcStatement;
  FromLoc, Loc: Integer;
  temp: TParamPtr;
  ASQL, UpperSQL: String;
  len : SmallInt;
Begin
  If FRowCountMethod = rcFunction Then
  Begin
    FRetCode:= SQLGetDiagField(SQL_HANDLE_STMT, FHstmt, 0, SQL_DIAG_CURSOR_ROW_COUNT, @Result, SizeOf(Result), len);
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

  tempHstmt:= TOdbcStatement.Create(FEnv, FHdbc);
  tempHstmt.MaxRows:= MaxRows;

  Try

    If FRowCountMethod <> rcTraverse Then
    Begin
      ASQL:= StringReplace(FSQL, EnterString, ' ', [rfReplaceAll, rfIgnoreCase]);
      UpperSQL:= UpperCase(ASQL);
      UpperSQL:= StringReplace(UpperSQL, ',', ' ', [rfReplaceAll, rfIgnoreCase]);
      UpperSQL:= StringReplace(UpperSQL, '(', ' ', [rfReplaceAll, rfIgnoreCase]);
      UpperSQL:= StringReplace(UpperSQL, ')', ' ', [rfReplaceAll, rfIgnoreCase]);
      UpperSQL:= fsl_utilities.StringReplace(UpperSQL, fsl_utilities.setControls, ' ');

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

Function TOdbcStatement.GetRowsFetched: SQLUINTEGER;
Begin
  Log(1, 'TOdbcStatement.NumRowsFetched');

  Result:= FNumRows;
End;

Function TOdbcStatement.GetRowsAffected: SQLLEN;
Begin
  Log(1, 'TOdbcStatement.NumRowsAffected');

  FRetCode:= SQLRowCount(FHstmt, Result);
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);
  {$IFDEF WIN64}
  if result = $FFFFFFFFFFFFFFFF then
    result := 0;
  {$ENDIF}
End;

Function TOdbcStatement.ParseSQL: String;
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

Procedure TOdbcStatement.Prepare;
Var
  ParsedSQL: PChar;
Begin
  Log(1, 'TOdbcStatement.Prepare');

  { Reset Hstmt }
  Close;

  DoBeforePrepare;

  { Parse Parameter SQL }
  ParsedSQL := odbcPChar(ParseSQL);
  try
    { Prepare SQL Statement }
    FRetCode:= SQLPrepare(FHstmt, ParsedSQL, SQL_NTS);
  finally
    freeMem(ParsedSQL);
  end;
  If Not FEnv.Error.Success(FRetCode) Then
    FEnv.Error.RaiseError(Self, FRetCode);

  DoAfterPrepare;

  FPrepared:= True;
End;

Function TOdbcStatement.BindCore: Boolean;
Begin
  Result:= Hdbc.Core Or (Not FPrepared) Or FBindByName;
End;

Procedure TOdbcStatement.BindParamMain(Param: SQLUSMALLINT;
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
  len : integer;

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
  Log(1, 'TOdbcStatement.BindParamMain');

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
        BufferLength:= DefaultStringSize+1;
      temp^.FSize^:= SQL_NTS;
    End;
    SQL_VARCHAR:
    Begin
      If Bulk = 0 Then
        BufferLength:= ParameterSize+1
      Else
        BufferLength:= DefaultStringSize+1;
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
        BufferLength:= DefaultStringSize+1;
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
      FRetCode:= SQLGetStmtAttr(FHstmt, SQL_ATTR_IMP_PARAM_DESC, @FHdesc, SizeOf(FHdesc), {$IFDEF FPC}@{$ENDIF}len);
      If Not FEnv.Error.Success(FRetCode) Then
        FEnv.Error.RaiseError(Self, FRetCode);
    End;
    ParamName:= FParamNames[Param-1];
    SQLSetDescField(FHdesc, Param, SQL_DESC_NAME, Pointer(PChar(ParamName)), SQL_NTS);
    SQLSetDescField(FHdesc, Param, SQL_DESC_UNNAMED, Pointer(SQL_NAMED), 0);
  End;
End;

Procedure TOdbcStatement.DescribeParam(Param: SQLUSMALLINT;
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
    FRetCode:= SQLDescribeParam(FHstmt, Param, SqlType, ParameterSize, DecimalDigits, Nullable);
    If Not FEnv.Error.Success(FRetCode) Then
      DescribeParam(Param, SqlType, ParameterSize, DecimalDigits, Nullable, True);
  End;
End;

Procedure TOdbcStatement.BindParam(Param: SQLUSMALLINT;
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

Procedure TOdbcStatement.BindParam(Param: SQLUSMALLINT;
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

Procedure TOdbcStatement.BindParamCore(Param: SQLUSMALLINT;
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

Procedure TOdbcStatement.BindParams(Param: SQLUSMALLINT;
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

Procedure TOdbcStatement.BindNull(Param: SQLUSMALLINT);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_DEFAULT;
  BindParam(Param, ParamType, Nil, SQL_CHAR);
End;

Procedure TOdbcStatement.BindString(Param: SQLUSMALLINT;
                            Var ParamValue: String);
Var
  ParamType: SQLSMALLINT;
Begin
  {$IFDEF FPC}
  ParamType:= SQL_C_CHAR;
  {$ELSE}
  ParamType:= SQL_C_WCHAR;
  {$ENDIF}
  BindParam(Param, ParamType, PChar(ParamValue), ColTypeToSqlType(ParamType));
  ParamSize[Param]:= Length(ParamValue){$IFNDEF FPC} * 2{$ENDIF};  //to allow binding memos, but causes re-exec problems
End;

Procedure TOdbcStatement.BindSingle(Param: SQLUSMALLINT;
                            Var ParamValue: Single);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_FLOAT;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure TOdbcStatement.BindDouble(Param: SQLUSMALLINT;
                            Var ParamValue: Double);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_DOUBLE;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure TOdbcStatement.BindShortint(Param: SQLUSMALLINT;
                              Var ParamValue: ShortInt);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_STINYINT;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure TOdbcStatement.BindByte(Param: SQLUSMALLINT;
                          Var ParamValue: Byte);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_UTINYINT;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure TOdbcStatement.BindSmallint(Param: SQLUSMALLINT;
                              Var ParamValue: SmallInt);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_SSHORT;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure TOdbcStatement.BindWord(Param: SQLUSMALLINT;
                          Var ParamValue: Word);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_USHORT;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure TOdbcStatement.BindInteger(Param: SQLUSMALLINT;
                             Var ParamValue: Integer);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_SLONG;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure TOdbcStatement.BindCardinal(Param: SQLUSMALLINT;
                              Var ParamValue: Cardinal);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_ULONG;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure TOdbcStatement.BindLongint(Param: SQLUSMALLINT;
                             Var ParamValue: LongInt);
Begin
  BindInteger(Param, Integer(ParamValue));
End;

Procedure TOdbcStatement.BindLongword(Param: SQLUSMALLINT;
                              Var ParamValue: LongWord);
Begin
  BindCardinal(Param, Cardinal(ParamValue));
End;

Procedure TOdbcStatement.BindInt64(Param: SQLUSMALLINT;
                           Var ParamValue: Int64);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_SBIGINT;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure TOdbcStatement.BindDate(Param: SQLUSMALLINT;
                          Var ParamValue: TDate);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_TYPE_DATE;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure TOdbcStatement.BindTime(Param: SQLUSMALLINT;
                          Var ParamValue: TTime);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_TYPE_TIME;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure TOdbcStatement.BindTimeStamp(Param: SQLUSMALLINT;
                               Var ParamValue: fsl_utilities.TTimeStamp);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_TYPE_TIMESTAMP;
  BindParam(Param, ParamType, @ParamValue, ColTypeToSqlType(ParamType));
End;

Procedure TOdbcStatement.BindMemory(Param: SQLUSMALLINT;
                            Var ParamValue: TManagedMemoryStream;
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

Procedure TOdbcStatement.BindBinary(Param: SQLUSMALLINT;
                            Var ParamValue: TManagedMemoryStream);
Begin
  BindMemory(Param, ParamValue, True);
End;

Procedure TOdbcStatement.BindText(Param: SQLUSMALLINT;
                          Var ParamValue: TManagedMemoryStream);
Begin
  BindMemory(Param, ParamValue, False);
End;

Procedure TOdbcStatement.BindNullByName(ParamName: String);
Begin
  BindNull(ParamByName(ParamName));
End;

Procedure TOdbcStatement.BindStringByName(ParamName: String;
                                  Var ParamValue: String);
Begin
  BindString(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindSingleByName(ParamName: String;
                                  Var ParamValue: Single);
Begin
  BindSingle(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindDoubleByName(ParamName: String;
                                  Var ParamValue: Double);
Begin
  BindDouble(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindShortintByName(ParamName: String;
                                    Var ParamValue: ShortInt);
Begin
  BindShortint(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindByteByName(ParamName: String;
                                Var ParamValue: Byte);
Begin
  BindByte(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindSmallintByName(ParamName: String;
                                    Var ParamValue: SmallInt);
Begin
  BindSmallint(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindWordByName(ParamName: String;
                                Var ParamValue: Word);
Begin
  BindWord(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindIntegerByName(ParamName: String;
                                   Var ParamValue: Integer);
Begin
  BindInteger(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindCardinalByName(ParamName: String;
                                    Var ParamValue: Cardinal);
Begin
  BindCardinal(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindLongintByName(ParamName: String;
                                   Var ParamValue: LongInt);
Begin
  BindLongint(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindLongwordByName(ParamName: String;
                                    Var ParamValue: LongWord);
Begin
  BindLongword(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindInt64ByName(ParamName: String;
                                 Var ParamValue: Int64);
Begin
  BindInt64(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindDateByName(ParamName: String;
                                Var ParamValue: TDate);
Begin
  BindDate(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindTimeByName(ParamName: String;
                                Var ParamValue: TTime);
Begin
  BindTime(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindTimeStampByName(ParamName: String;
                                     Var ParamValue: fsl_utilities.TTimeStamp);
Begin
  BindTimeStamp(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindMemoryByName(ParamName: String;
                                  Var ParamValue: TManagedMemoryStream;
                                  Binary: Boolean);
Begin
  BindMemory(ParamByName(ParamName), ParamValue, Binary);
End;

Procedure TOdbcStatement.BindBinaryByName(ParamName: String;
                                  Var ParamValue: TManagedMemoryStream);
Begin
  BindBinary(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindTextByName(ParamName: String;
                                Var ParamValue: TManagedMemoryStream);
Begin
  BindText(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindNulls(Param: SQLUSMALLINT);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_DEFAULT;
  BindParams(Param, ParamType, Nil, FNumParams);
End;

Procedure TOdbcStatement.BindSingles(Param: SQLUSMALLINT;
                             Var ParamValue: Array Of Single);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_FLOAT;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure TOdbcStatement.BindDoubles(Param: SQLUSMALLINT;
                             Var ParamValue: Array Of Double);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_DOUBLE;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure TOdbcStatement.BindShortints(Param: SQLUSMALLINT;
                               Var ParamValue: Array Of ShortInt);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_STINYINT;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure TOdbcStatement.BindBytes(Param: SQLUSMALLINT;
                           Var ParamValue: Array Of Byte);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_UTINYINT;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure TOdbcStatement.BindSmallints(Param: SQLUSMALLINT;
                               Var ParamValue: Array Of SmallInt);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_SSHORT;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure TOdbcStatement.BindWords(Param: SQLUSMALLINT;
                           Var ParamValue: Array Of Word);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_USHORT;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure TOdbcStatement.BindIntegers(Param: SQLUSMALLINT;
                              Var ParamValue: Array Of Integer);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_SLONG;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure TOdbcStatement.BindCardinals(Param: SQLUSMALLINT;
                               Var ParamValue: Array Of Cardinal);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_ULONG;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure TOdbcStatement.BindLongints(Param: SQLUSMALLINT;
                              Var ParamValue: Array Of LongInt);
Var
  ParamType: SQLSMALLINT;
Begin
  //BindIntegers(Param, ParamValue);
  ParamType:= SQL_C_SLONG;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure TOdbcStatement.BindLongwords(Param: SQLUSMALLINT;
                               Var ParamValue: Array Of LongWord);
Var
  ParamType: SQLSMALLINT;
Begin
  //BindCardinals(Param, ParamValue);
  ParamType:= SQL_C_ULONG;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure TOdbcStatement.BindInt64s(Param: SQLUSMALLINT;
                            Var ParamValue: Array Of Int64);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_SBIGINT;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure TOdbcStatement.BindDates(Param: SQLUSMALLINT;
                           Var ParamValue: Array Of TDate);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_TYPE_DATE;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure TOdbcStatement.BindTimes(Param: SQLUSMALLINT;
                           Var ParamValue: Array Of TTime);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_TYPE_TIME;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure TOdbcStatement.BindTimeStamps(Param: SQLUSMALLINT;
                                Var ParamValue: Array Of fsl_utilities.TTimeStamp);
Var
  ParamType: SQLSMALLINT;
Begin
  ParamType:= SQL_C_TYPE_TIMESTAMP;
  BindParams(Param, ParamType, @ParamValue, High(ParamValue)+1);
End;

Procedure TOdbcStatement.BindNullsByName(ParamName: String);
Begin
  BindNulls(ParamByName(ParamName));
End;

Procedure TOdbcStatement.BindSinglesByName(ParamName: String;
                                   Var ParamValue: Array Of Single);
Begin
  BindSingles(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindDoublesByName(ParamName: String;
                                   Var ParamValue: Array Of Double);
Begin
  BindDoubles(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindShortintsByName(ParamName: String;
                                     Var ParamValue: Array Of ShortInt);
Begin
  BindShortints(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindBytesByName(ParamName: String;
                                 Var ParamValue: Array Of Byte);
Begin
  BindBytes(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindSmallintsByName(ParamName: String;
                                     Var ParamValue: Array Of SmallInt);
Begin
  BindSmallints(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindWordsByName(ParamName: String;
                                 Var ParamValue: Array Of Word);
Begin
  BindWords(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindIntegersByName(ParamName: String;
                                    Var ParamValue: Array Of Integer);
Begin
  BindIntegers(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindCardinalsByName(ParamName: String;
                                     Var ParamValue: Array Of Cardinal);
Begin
  BindCardinals(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindLongintsByName(ParamName: String;
                                    Var ParamValue: Array Of LongInt);
Begin
  BindLongints(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindLongwordsByName(ParamName: String;
                                     Var ParamValue: Array Of LongWord);
Begin
  BindLongwords(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindInt64sByName(ParamName: String;
                                  Var ParamValue: Array Of Int64);
Begin
  BindInt64s(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindDatesByName(ParamName: String;
                                 Var ParamValue: Array Of TDate);
Begin
  BindDates(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindTimesByName(ParamName: String;
                                 Var ParamValue: Array Of TTime);
Begin
  BindTimes(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.BindTimeStampsByName(ParamName: String;
                                      Var ParamValue: Array Of fsl_utilities.TTimeStamp);
Begin
  BindTimeStamps(ParamByName(ParamName), ParamValue);
End;

Procedure TOdbcStatement.Execute;
Var
  ParsedSQL: PChar;
Begin
  Log(1, 'TOdbcStatement.Execute');

  Init;

  DoBeforeExecute;

  { Set Bulk Size }
  //deprecated FRetCode:= SQLParamOptions(FHstmt, FNumParams, @irow);
  FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_PARAMSET_SIZE, Pointer(FNumParams), SizeOf(FNumParams));
  If (Not FEnv.Error.Success(FRetCode)) And (FNumParams > 1) Then
    FEnv.Error.RaiseError(Self, FRetCode);

  FExecuted:= False;
  FAborted:= False;
  If not FPrepared Then
  Begin
    { Execute SQL Statement }
    FRetCode:= SQLExecute(FHstmt);
  End
  Else
  Begin
    { Reset Statement Handle }
    If Hdbc.CursorLib = SQL_CUR_USE_DRIVER Then
      CloseCursor;

    { Parse Parameter SQL }
    ParsedSQL := odbcPChar(ParseSQL);
    try
      { Execute SQL Statement }
      FRetCode:= SQLExecDirect(FHstmt, ParsedSQL, SQL_NTS);
    finally
      FreeMem(ParsedSQL);
    end;
  End;
  If FAborted Then
  Begin
    FExecuted:= True;
    Abort;
  End;
  If (FRetCode <> SQL_NEED_DATA) And
     (FRetCode <> SQL_NO_DATA) And  //no rows affected by operation
     (Not FEnv.Error.Success(FRetCode)) Then
    FEnv.Error.RaiseError(Self, FRetCode);

  { Handle Data-At-Execution Parameters }
  If FRetCode = SQL_NEED_DATA Then
    DataAtExecution(TCommonPtr(FParams));

  If FEnv.Error.Success(FRetCode) Then
    DoAfterExecute;

  { Bind NumRowsFetched }
  FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_ROWS_FETCHED_PTR, @FNumRows, SizeOf(FNumRows));

  FExecuted:= True;
End;

Function TOdbcStatement.ColAttrString(Col: SQLUSMALLINT;
                              FieldIdentifier: SQLUSMALLINT): String;
Var
  CharAttr: PChar;
  NumAttr: SQLLEN;
  StringLength: SQLSMALLINT;
Begin
  CharAttr := odbcPChar(DefaultStringSize);
  try
    FRetCode:= SQLColAttribute(FHstmt, Col, FieldIdentifier, CharAttr, DefaultStringSize, {$IFDEF FPC}@{$ENDIF}StringLength, {$IFDEF FPC}@{$ENDIF}NumAttr);
    If Not FEnv.Error.Success(FRetCode) Then
      FEnv.Error.RaiseError(Self, FRetCode);

    Result := fromOdbcPChar(CharAttr, StringLength);
  finally
    freemem(CharAttr);
  end;
End;

Function TOdbcStatement.ColAttrInteger(Col: SQLUSMALLINT;
                               FieldIdentifier: SQLUSMALLINT): SQLINTEGER;
Var
  CharAttr: PChar;
  NumAttr: SQLLEN;
  StringLength: SQLSMALLINT;
Begin
  CharAttr := odbcPChar(DefaultStringSize);
  try
    FRetCode:= SQLColAttribute(FHstmt, Col, FieldIdentifier, CharAttr, DefaultStringSize, {$IFDEF FPC}@{$ENDIF}StringLength, {$IFDEF FPC}@{$ENDIF}NumAttr);
    If Not FEnv.Error.Success(FRetCode) Then
      FEnv.Error.RaiseError(Self, FRetCode);

    Result:= NumAttr;
  finally
    freemem(CharAttr);
  end;
End;

Procedure TOdbcStatement.BindCol(Col: SQLUSMALLINT;
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

Procedure TOdbcStatement.BindCols;
Var
  FTail: TColPtr;
  temp: TColBindPtr;
  icol: SQLUSMALLINT;
  CType: SQLSMALLINT;
  SqlValue: SQLPOINTER;
  BufferLength: SQLLEN;

  ColumnName: PChar;
  NameLength: SQLSMALLINT;
  SqlType: SQLSMALLINT;
  ColumnSize: SQLULEN;
  DecimalDigits: SQLSMALLINT;
  Nullable: SQLSMALLINT;
Begin
  Log(1, 'TOdbcStatement.BindCols');

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
      ColumnName := odbcPChar(DefaultStringSize);
      try
        FRetCode:= SQLDescribeCol(FHstmt, icol, ColumnName, DefaultStringSize, NameLength, SqlType, ColumnSize, DecimalDigits, Nullable);
        If Not FEnv.Error.Success(FRetCode) Then
          FEnv.Error.RaiseError(Self, FRetCode);

        { Set Column Name }
        FColNames.Add(fromOdbcPChar(ColumnName, NameLength));
      finally
        freeMem(columnName);
      end;

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
      End;

      BufferLength:= 0;

      { Create Storage }
      Case SqlType Of
        SQL_CHAR:
        Begin
          CType:= SQL_C_CHAR;
          If FBulkData Then
            BufferLength:= DefaultStringSize+1
          Else
            BufferLength:= ColumnSize+1;
          GetMem(SqlValue, BufferLength);
        End;
        SQL_VARCHAR:
        Begin
          CType:= SQL_C_CHAR;
          If FBulkData Or (ColumnSize = 0) Then
            BufferLength:= DefaultStringSize+1
          Else
            BufferLength:= ColumnSize+1;
          GetMem(SqlValue, BufferLength);
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
          GetMem(SqlValue, BufferLength);
        End;
        SQL_VARBINARY:
        Begin
          CType:= SQL_C_BINARY;
          SqlValue:= Nil;

          //BufferLength:= ColumnSize;
          //GetMem(SqlValue, BufferLength);
        End;
        SQL_LONGVARBINARY:
        Begin
          CType:= SQL_C_BINARY;
          SqlValue:= Nil;
        End;

        SQL_REAL:
        Begin
          CType:= SQL_C_FLOAT;
          GetMem(SqlValue, PhysSize(CType));
        End;
        SQL_DOUBLE:
        Begin
          CType:= SQL_C_DOUBLE;
          GetMem(SqlValue, PhysSize(CType));
        End;
        SQL_FLOAT:
        Begin
          CType:= SQL_C_DOUBLE;
          GetMem(SqlValue, PhysSize(CType));
        End;
        SQL_DECIMAL:
        Begin
          CType:= SQL_C_DOUBLE;
          BufferLength := 8; // (GM) work around bug in sql 2008 driver 2007.100.1600.22
          GetMem(SqlValue, PhysSize(CType));
        End;
        SQL_NUMERIC:
        Begin
          CType:= SQL_C_DOUBLE;
          GetMem(SqlValue, PhysSize(CType));
        End;

        SQL_BIT:
        Begin
          CType:= SQL_C_BIT;
          GetMem(SqlValue, PhysSize(CType));
        End;
        SQL_TINYINT:
        Begin
          If ColAttrInteger(icol, SQL_DESC_UNSIGNED) = SQL_FALSE Then
            CType:= SQL_C_STINYINT
          Else
            CType:= SQL_C_UTINYINT;
          GetMem(SqlValue, PhysSize(CType));
        End;
        SQL_SMALLINT:
        Begin
          If ColAttrInteger(icol, SQL_DESC_UNSIGNED) = SQL_FALSE Then
            CType:= SQL_C_SSHORT
          Else
            CType:= SQL_C_USHORT;
          GetMem(SqlValue, PhysSize(CType));
        End;
        SQL_INTEGER:
        Begin
          If ColAttrInteger(icol, SQL_DESC_UNSIGNED) = SQL_FALSE Then
            CType:= SQL_C_SLONG
          Else
            CType:= SQL_C_ULONG;
          GetMem(SqlValue, PhysSize(CType));
        End;
        SQL_BIGINT:
        Begin
          If ColAttrInteger(icol, SQL_DESC_UNSIGNED) = SQL_FALSE Then
            CType:= SQL_C_SBIGINT
          Else
            CType:= SQL_C_UBIGINT;
          GetMem(SqlValue, PhysSize(CType));
        End;

        SQL_TYPE_DATE:
        Begin
          CType:= SQL_C_TYPE_DATE;
          GetMem(SqlValue, PhysSize(CType));
        End;
        SQL_TYPE_TIME:
        Begin
          CType:= SQL_C_TYPE_TIME;
          GetMem(SqlValue, PhysSize(CType));
        End;
        SQL_TYPE_TIMESTAMP:
        Begin
          CType:= SQL_C_TYPE_TIMESTAMP;
          GetMem(SqlValue, PhysSize(CType));
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
    GetMem(SqlValue, PhysSize(CType));
    InsertTail(FTail, CType, ColTypeToSqlType(CType), SqlValue);
    SQLUSMALLINT(SqlValue^):= SQL_ROW_SUCCESS;
    FRowStatus:= FTail;

    { Add RowFlags Column }
    CType:= SQL_C_USHORT;
    GetMem(SqlValue, PhysSize(CType));
    InsertTail(FTail, CType, ColTypeToSqlType(CType), SqlValue);
    FillChar(SqlValue^, PhysSize(CType), rfNone);
    FRowFlags:= FTail;

    { Bind RowStatus }
    //depreciated FRetCode:= SQLExtendedFetch(FHstmt, FetchType, Row, @FNumRows, FRowStatus^.FValue);
    FRetCode:= SQLSetStmtAttr(FHstmt, SQL_ATTR_ROW_STATUS_PTR, FRowStatus^.FValue, 0);
  End;

  FColumnsBound:= True;
End;

Procedure TOdbcStatement.BindBlobCols(Bind: Boolean);
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

Function TOdbcStatement.FetchCol(Col: SQLUSMALLINT;
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
  If ColStream Is TManagedMemoryStream Then
    TManagedMemoryStream(ColStream).Clear;
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

Function TOdbcStatement.FetchCell(Col, Row: SQLUSMALLINT;
                          ColType: SQLSMALLINT;
                          ColStream: TStream): SQLINTEGER;
Var
  temp: TColPtr;
Begin
  Result:= CellSize[Col,Row];
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

Procedure TOdbcStatement.DataAtExecution(FList: TCommonPtr);
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

Function TOdbcStatement.Fetch(FetchType: SQLSMALLINT;
                      Row: SQLINTEGER): Boolean;
Var
  { Global Column }
  icol: SQLUSMALLINT;
  temp: TColPtr;
Begin
  Log(1, 'TOdbcStatement.Fetch');

  Init;

  DoBeforeFetch;

  { Bind Columns }
  BindCols;

  { Fetch Next Row in Result Set }
  If (FetchType = SQL_FETCH_NEXT) And (Hdbc.CursorLib = SQL_CUR_USE_DRIVER) Then
    FRetCode:= SQLFetch(FHstmt)
  Else
    FRetCode:= SQLFetchScroll(FHstmt, FetchType, Row);

  FillChar(FRowFlags^.FValue^, PhysSize(SQL_C_USHORT), rfNone);
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

Function TOdbcStatement.FetchFirst: Boolean;
Begin
  Result:= Fetch(SQL_FETCH_FIRST, 0);
End;

Function TOdbcStatement.FetchNext: Boolean;
Begin
  Result:= Fetch(SQL_FETCH_NEXT, 0);
End;

Function TOdbcStatement.FetchLast: Boolean;
Begin
  Result:= Fetch(SQL_FETCH_LAST, 0);
End;

Function TOdbcStatement.FetchPrev: Boolean;
Begin
  Result:= Fetch(SQL_FETCH_PRIOR, 0);
End;

Function TOdbcStatement.FetchAbsolute(Row: SQLINTEGER): Boolean;
Begin
  Result:= Fetch(SQL_FETCH_ABSOLUTE, Row);
End;

Function TOdbcStatement.FetchRelative(Row: SQLINTEGER): Boolean;
Begin
  Result:= Fetch(SQL_FETCH_RELATIVE, Row);
End;

Procedure TOdbcStatement.ColStream(Col: SQLUSMALLINT;
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

Procedure TOdbcStatement.CellStream(Col, Row: SQLUSMALLINT;
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

Procedure TOdbcStatement.DoBeforePrepare;
Begin
  If Assigned(FBeforePrepare) Then
    FBeforePrepare(Self);
End;

Procedure TOdbcStatement.DoAfterPrepare;
Begin
  If Assigned(FAfterPrepare) Then
    FAfterPrepare(Self);
End;

Procedure TOdbcStatement.DoBeforeExecute;
Begin
  If Assigned(FBeforeExecute) Then
    FBeforeExecute(Self);
End;

Procedure TOdbcStatement.DoAfterExecute;
Begin
  If Assigned(FAfterExecute) Then
    FAfterExecute(Self);
End;

Procedure TOdbcStatement.DoBeforeFetch;
Begin
  If Assigned(FBeforeFetch) Then
    FBeforeFetch(Self);
End;

Procedure TOdbcStatement.DoAfterFetch;
Begin
  If Assigned(FAfterFetch) Then
    FAfterFetch(Self);
End;

Function TOdbcStatement.DoRowCount: Integer;
Begin
  Result:= -1;
  If Assigned(FOnRowCount) Then
    Result:= FOnRowCount(Self);
End;

Function TOdbcStatement.GetPosOpts: SQLINTEGER;
Begin
  If SkipByPosition Then
    Result:= 0
  Else
    //depreciated Result:= Hdbc.FPosOpts;
    Result:= FCursorAttr;
End;

Function TOdbcStatement.GetPosStmts: SQLINTEGER;
Begin
  If SkipByCursor Then
    Result:= 0
  Else
    //depreciated Result:= Hdbc.FPosStmts;
    Result:= FCursorAttr;
End;

Procedure TOdbcStatement.DetermineTargetTable;
Var
  CharAttr: PChar;
  NumAttr: SQLLEN;
  StringLength: SQLSMALLINT;
  Loc: Integer;
Begin
  CharAttr := odbcPChar(DefaultStringSize);
  try
    { Determine Target Table }
    If Trim(FTargetTable) = '' Then
    Begin
      FRetCode:= SQLColAttribute(FHstmt, 1, SQL_DESC_SCHEMA_NAME, CharAttr, DefaultStringSize, {$IFDEF FPC}@{$ENDIF}StringLength, {$IFDEF FPC}@{$ENDIF}NumAttr);
      FTableOwner:= Trim(fromOdbcPChar(CharAttr, StringLength));
      If Not FEnv.Error.Success(FRetCode) Then
        FTableOwner:= '';

      FRetCode:= SQLColAttribute(FHstmt, 1, SQL_DESC_TABLE_NAME, CharAttr, DefaultStringSize, {$IFDEF FPC}@{$ENDIF}StringLength, {$IFDEF FPC}@{$ENDIF}NumAttr);
      FTableName:= Trim(fromOdbcPChar(CharAttr, StringLength));
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
  finally
    freemem(CharAttr);
  end;
End;

Procedure TOdbcStatement.DeterminePrimaryCols;
Var
  icol: SQLUSMALLINT;
  tempCol: TColPtr;
  tempHstmt: TOdbcStatement;
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

  tempHstmt:= TOdbcStatement.Create(FEnv, FHdbc);

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

Function TOdbcStatement.GetTableOwner: String;
Begin
  DetermineTargetTable;

  Result:= FTableOwner;
End;

Function TOdbcStatement.GetTableName: String;
Begin
  DetermineTargetTable;

  Result:= FTableName;
End;

Function TOdbcStatement.GetPrimaryColNames: String;
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


Function TOdbcStatement.CursorName: String;
Var
  CurName: PChar;
  StringLength: SQLSMALLINT;
Begin
  { Determine Cursor Name }
  CurName := odbcPChar(DefaultStringSize);
  try
    FRetCode:= SQLGetCursorName(FHstmt, CurName, DefaultStringSize, {$IFDEF FPC}@{$ENDIF}StringLength);
    If Not FEnv.Error.Success(FRetCode) Then
      FEnv.Error.RaiseError(Self, FRetCode);

    Result := fromOdbcPChar(CurName, StringLength);
  finally
    freemem(CurName);
  end;
End;

Function TOdbcStatement.PrimaryClause: String;
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

Procedure TOdbcStatement.BindClause(Var Param: SQLUSMALLINT;
                            AHstmt: TOdbcStatement;
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

Procedure TOdbcStatement.InsertFields;
Var
  icol: SQLUSMALLINT;
  tempCol: TColPtr;
  ASQL, temp: String;
  ParamNum: SQLUSMALLINT;
Begin
  If FHstmtInsert = Nil Then
  Begin
    FHstmtInsert:= TOdbcStatement.Create(FEnv, FHdbc);
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

Procedure TOdbcStatement.UpdateFields(WhereClause: String;
                              BindWhere: Boolean);
Var
  icol: SQLUSMALLINT;
  tempCol: TColPtr;
  ASQL: String;
  ParamNum: SQLUSMALLINT;
Begin
  If FHstmtUpdate = Nil Then
  Begin
    FHstmtUpdate:= TOdbcStatement.Create(FEnv, FHdbc);
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

Procedure TOdbcStatement.DeleteFields(WhereClause: String;
                              BindWhere: Boolean);
Var
  ASQL: String;
  ParamNum: SQLUSMALLINT;
Begin
  If FHstmtDelete = Nil Then
  Begin
    FHstmtDelete:= TOdbcStatement.Create(FEnv, FHdbc);
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

Procedure TOdbcStatement.RefreshFields(WhereClause: String);
Var
  icol: SQLUSMALLINT;
  tempCol: TColPtr;
  ASQL: String;
  ParamNum: SQLUSMALLINT;
  Size: Word;
Begin
  If FHstmtRefresh = Nil Then
  Begin
    FHstmtRefresh:= TOdbcStatement.Create(FEnv, FHdbc);
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

Procedure TOdbcStatement.InsertRow(Row: SQLUSMALLINT);
Begin
  If (GetPosOpts And SQL_CA1_BULK_ADD) = SQL_CA1_BULK_ADD Then
  Begin
    BindBlobCols(True);

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

Procedure TOdbcStatement.UpdateRow(Row: SQLUSMALLINT);
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
  UpdateFields(WhereClause, BindWhere);
End;

Procedure TOdbcStatement.DeleteRow(Row: SQLUSMALLINT);
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
  DeleteFields(WhereClause, BindWhere);
End;

Procedure TOdbcStatement.RefreshRow(Row: SQLUSMALLINT);

  Function WhereClause: String;
  Begin
    Result:= 'WHERE '+PrimaryClause;
  End;

Begin
  RefreshFields(WhereClause);
End;

{ DoAction }

Procedure TOdbcStatement.DoInsert;
Begin
  Init;

  If (Not Assigned(FOnInsert)) Or
     (Assigned(FOnInsert) And FOnInsert(Self, 'Insert row(s)?')) Then
    Begin
      InsertRow(1);
    End;
End;

Procedure TOdbcStatement.DoUpdate;
Begin
  Init;

  If (Not Assigned(FOnUpdate)) Or
     (Assigned(FOnUpdate) And FOnUpdate(Self, 'Update row(s)?')) Then
    Begin
      UpdateRow(1);
    End;
End;

Procedure TOdbcStatement.DoDelete;
Begin
  Init;

  If (Not Assigned(FOnDelete)) Or
     (Assigned(FOnDelete) And FOnDelete(Self, 'Delete row(s)?')) Then
    Begin
      DeleteRow(1);
    End;
End;

Procedure TOdbcStatement.DoRefresh;
Begin
  Init;

  If (Not Assigned(FOnRefresh)) Or
     (Assigned(FOnRefresh) And FOnRefresh(Self, 'Refresh row(s)?')) Then
    Begin
      RefreshRow(1);
    End;
End;

Function TOdbcStatement.GetColValue(Col: SQLUSMALLINT): SQLPOINTER;
Begin
  Result:= GetCellValue(Col, 1);
End;

Function TOdbcStatement.GetCellValue(Col, Row: SQLUSMALLINT): SQLPOINTER;
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

Function TOdbcStatement.GetColType(Col: SQLUSMALLINT): SQLSMALLINT;
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

Function TOdbcStatement.GetSqlType(Col: SQLUSMALLINT): SQLSMALLINT;
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

Function TOdbcStatement.GetBlobCol(Col: SQLUSMALLINT): Boolean;
Var
  temp: TColPtr;
Begin
  temp:= ColRec(Col);
  Result:= (temp <> Nil) And temp^.FBlob;
End;

Function TOdbcStatement.ParamRec(Param: SQLUSMALLINT): TParamPtr;
Begin
  if param >= Length(FParamIndexes) then
    Result := nil
  else
    Result := FParamIndexes[Param];
End;

Function TOdbcStatement.ColBindRec(Col: SQLUSMALLINT): TColBindPtr;
Begin

  Result:= FColBinds;

  While Result <> Nil Do
  Begin
    If Result^.FCol = Col Then
      Break;
    Result:= Result^.Next;
  End;
End;

Function TOdbcStatement.ColRec(Col: SQLUSMALLINT): TColPtr;
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

Function TOdbcStatement.RowRec(Col, Row: SQLUSMALLINT): TRowPtr;
Var
  tempCol: TColPtr;
  tempRow: TRowPtr;
  Size: Word;
Begin
  tempCol:= ColRec(Col);

  tempRow:= Nil;
  If (tempCol <> Nil) And
     (Row > 0) And (Row <= 1) Then
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

Function TOdbcStatement.RowRecEx(Col, Row: SQLUSMALLINT; Out VrRowRec : TRowRec): Boolean;
Var
  Size: Word;
  tempCol: TColPtr;
Begin
  tempCol:= ColRec(Col);

  If (tempCol <> Nil) And
     (Row > 0) And (Row <= 1) Then
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

Function TOdbcStatement.RowFlags(Row: SQLUSMALLINT): SQLUSMALLINTPtr;
Begin
  Log(1, 'TOdbcStatement.RowFlags');

  If (Row > 0) And (Row <= 1) Then
    Result:= SQLUSMALLINTPtr(OffsetRow(FRowFlags^.FValue, Row, PhysSize(SQL_C_USHORT)))
  Else
    Result:= Nil;
End;

Function TOdbcStatement.GetColString(Col: SQLUSMALLINT): String;
Begin
  Result:= GetCellString(Col, 1);
End;

Function TOdbcStatement.GetColSingle(Col: SQLUSMALLINT): Single;
Begin
  Result:= GetCellDouble(Col, 1);
End;

Function TOdbcStatement.GetColDouble(Col: SQLUSMALLINT): Double;
Begin
  Result:= GetCellDouble(Col, 1);
End;

Function TOdbcStatement.GetColBoolean(Col: SQLUSMALLINT): Boolean;
Begin
  Result:= GetCellBoolean(Col, 1);
End;

Function TOdbcStatement.GetColShortint(Col: SQLUSMALLINT): ShortInt;
Begin
  Result:= GetCellInteger(Col, 1);
End;

Function TOdbcStatement.GetColByte(Col: SQLUSMALLINT): Byte;
Begin
  Result:= GetCellInteger(Col, 1);
End;

Function TOdbcStatement.GetColSmallint(Col: SQLUSMALLINT): SmallInt;
Begin
  Result:= GetCellInteger(Col, 1);
End;

Function TOdbcStatement.GetColWord(Col: SQLUSMALLINT): Word;
Begin
  Result:= GetCellInteger(Col, 1);
End;

Function TOdbcStatement.GetColInteger(Col: SQLUSMALLINT): Integer;
Begin
  Result:= GetCellInteger(Col, 1);
End;

Function TOdbcStatement.GetColCardinal(Col: SQLUSMALLINT): Cardinal;
Begin
  Result:= GetCellInteger(Col, 1);
End;

Function TOdbcStatement.GetColLongint(Col: SQLUSMALLINT): LongInt;
Begin
  Result:= GetCellInteger(Col, 1);
End;

Function TOdbcStatement.GetColLongword(Col: SQLUSMALLINT): LongWord;
Begin
  Result:= GetCellInteger(Col, 1);
End;

Function TOdbcStatement.GetColInt64(Col: SQLUSMALLINT): Int64;
Begin
  Result:= GetCellInt64(Col, 1);
End;

Function TOdbcStatement.GetColDate(Col: SQLUSMALLINT): TDate;
Begin
  Result:= GetCellDate(Col, 1);
End;

Function TOdbcStatement.GetColTime(Col: SQLUSMALLINT): TTime;
Begin
  Result:= GetCellTime(Col, 1);
End;

Function TOdbcStatement.GetColTimeStamp(Col: SQLUSMALLINT): fsl_utilities.TTimeStamp;
Begin
  Result:= GetCellTimeStamp(Col, 1);
End;

Function TOdbcStatement.GetColMemory(Col: SQLUSMALLINT): TManagedMemoryStream;
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

Function TOdbcStatement.GetColVariant(Col: SQLUSMALLINT): Variant;
Begin
  Result:= GetCellVariant(Col, 1);
End;

Procedure TOdbcStatement.SetColString(Col: SQLUSMALLINT;
                              AValue: String);
Begin
  SetCellString(Col, 1, AValue);
End;

Procedure TOdbcStatement.SetColSingle(Col: SQLUSMALLINT;
                              AValue: Single);
Begin
  SetCellDouble(Col, 1, AValue);
End;

Procedure TOdbcStatement.SetColDouble(Col: SQLUSMALLINT;
                              AValue: Double);
Begin
  SetCellDouble(Col, 1, AValue);
End;

Procedure TOdbcStatement.SetColBoolean(Col: SQLUSMALLINT;
                               AValue: Boolean);
Begin
  SetCellBoolean(Col, 1, AValue);
End;

Procedure TOdbcStatement.SetColShortint(Col: SQLUSMALLINT;
                                AValue: ShortInt);
Begin
  SetCellInteger(Col, 1, AValue);
End;

Procedure TOdbcStatement.SetColByte(Col: SQLUSMALLINT;
                            AValue: Byte);
Begin
  SetCellInteger(Col, 1, AValue);
End;

Procedure TOdbcStatement.SetColSmallint(Col: SQLUSMALLINT;
                                AValue: SmallInt);
Begin
  SetCellInteger(Col, 1, AValue);
End;

Procedure TOdbcStatement.SetColWord(Col: SQLUSMALLINT;
                            AValue: Word);
Begin
  SetCellInteger(Col, 1, AValue);
End;

Procedure TOdbcStatement.SetColInteger(Col: SQLUSMALLINT;
                               AValue: Integer);
Begin
  SetCellInteger(Col, 1, AValue);
End;

Procedure TOdbcStatement.SetColCardinal(Col: SQLUSMALLINT;
                                AValue: Cardinal);
Begin
  SetCellInteger(Col, 1, AValue);
End;

Procedure TOdbcStatement.SetColLongint(Col: SQLUSMALLINT;
                               AValue: LongInt);
Begin
  SetCellInteger(Col, 1, AValue);
End;

Procedure TOdbcStatement.SetColLongword(Col: SQLUSMALLINT;
                                AValue: LongWord);
Begin
  SetCellInteger(Col, 1, AValue);
End;

Procedure TOdbcStatement.SetColInt64(Col: SQLUSMALLINT;
                             AValue: Int64);
Begin
  SetCellInt64(Col, 1, AValue);
End;

Procedure TOdbcStatement.SetColDate(Col: SQLUSMALLINT;
                            AValue: TDate);
Begin
  SetCellDate(Col, 1, AValue);
End;

Procedure TOdbcStatement.SetColTime(Col: SQLUSMALLINT;
                            AValue: TTime);
Begin
  SetCellTime(Col, 1, AValue);
End;

Procedure TOdbcStatement.SetColTimeStamp(Col: SQLUSMALLINT;
                                 AValue: fsl_utilities.TTimeStamp);
Begin
  SetCellTimeStamp(Col, 1, AValue);
End;

Procedure TOdbcStatement.SetColMemory(Col: SQLUSMALLINT;
                              AValue: TManagedMemoryStream);
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

Procedure TOdbcStatement.SetColVariant(Col: SQLUSMALLINT;
                               AValue: Variant);
Begin
  SetCellVariant(Col, 1, AValue);
End;

Function TOdbcStatement.GetColStringByName(ColName: String): String;
Begin
  Result:= GetColString(ColByName(ColName));
End;

Function TOdbcStatement.GetColSingleByName(ColName: String): Single;
Begin
  Result:= GetColSingle(ColByName(ColName));
End;

Function TOdbcStatement.GetColDoubleByName(ColName: String): Double;
Begin
  Result:= GetColDouble(ColByName(ColName));
End;

Function TOdbcStatement.GetColBooleanByName(ColName: String): Boolean;
Begin
  Result:= GetColBoolean(ColByName(ColName));
End;

Function TOdbcStatement.GetColShortintByName(ColName: String): ShortInt;
Begin
  Result:= GetColShortint(ColByName(ColName));
End;

Function TOdbcStatement.GetColByteByName(ColName: String): Byte;
Begin
  Result:= GetColByte(ColByName(ColName));
End;

Function TOdbcStatement.GetColSmallintByName(ColName: String): SmallInt;
Begin
  Result:= GetColSmallint(ColByName(ColName));
End;

Function TOdbcStatement.GetColWordByName(ColName: String): Word;
Begin
  Result:= GetColWord(ColByName(ColName));
End;

Function TOdbcStatement.GetColIntegerByName(ColName: String): Integer;
Begin
  Result:= GetColInteger(ColByName(ColName));
End;

Function TOdbcStatement.GetColCardinalByName(ColName: String): Cardinal;
Begin
  Result:= GetColCardinal(ColByName(ColName));
End;

Function TOdbcStatement.GetColLongintByName(ColName: String): LongInt;
Begin
  Result:= GetColLongint(ColByName(ColName));
End;

Function TOdbcStatement.GetColLongwordByName(ColName: String): LongWord;
Begin
  Result:= GetColLongword(ColByName(ColName));
End;

Function TOdbcStatement.GetColInt64ByName(ColName: String): Int64;
Begin
  Result:= GetColInt64(ColByName(ColName));
End;

Function TOdbcStatement.GetColDateByName(ColName: String): TDate;
Begin
  Result:= GetColDate(ColByName(ColName));
End;

Function TOdbcStatement.GetColTimeByName(ColName: String): TTime;
Begin
  Result:= GetColTime(ColByName(ColName));
End;

Function TOdbcStatement.GetColTimeStampByName(ColName: String): fsl_utilities.TTimeStamp;
Begin
  Result:= GetColTimeStamp(ColByName(ColName));
End;

Function TOdbcStatement.GetColMemoryByName(ColName: String): TManagedMemoryStream;
Begin
  Result:= GetColMemory(ColByName(ColName));
End;

Function TOdbcStatement.GetColVariantByName(ColName: String): Variant;
Begin
  Result:= GetColVariant(ColByName(ColName));
End;

Procedure TOdbcStatement.SetColStringByName(ColName: String;
                                    AValue: String);
Begin
  SetColString(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColSingleByName(ColName: String;
                                    AValue: Single);
Begin
  SetColSingle(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColDoubleByName(ColName: String;
                                    AValue: Double);
Begin
  SetColDouble(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColBooleanByName(ColName: String;
                                     AValue: Boolean);
Begin
  SetColBoolean(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColShortintByName(ColName: String;
                                      AValue: ShortInt);
Begin
  SetColShortint(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColByteByName(ColName: String;
                                  AValue: Byte);
Begin
  SetColByte(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColSmallintByName(ColName: String;
                                      AValue: SmallInt);
Begin
  SetColSmallint(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColWordByName(ColName: String;
                                  AValue: Word);
Begin
  SetColWord(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColIntegerByName(ColName: String;
                                     AValue: Integer);
Begin
  SetColInteger(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColCardinalByName(ColName: String;
                                      AValue: Cardinal);
Begin
  SetColCardinal(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColLongintByName(ColName: String;
                                     AValue: LongInt);
Begin
  SetColLongint(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColLongwordByName(ColName: String;
                                      AValue: LongWord);
Begin
  SetColLongword(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColInt64ByName(ColName: String;
                                   AValue: Int64);
Begin
  SetColInt64(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColDateByName(ColName: String;
                                  AValue: TDate);
Begin
  SetColDate(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColTimeByName(ColName: String;
                                  AValue: TTime);
Begin
  SetColTime(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColTimeStampByName(ColName: String;
                                       AValue: fsl_utilities.TTimeStamp);
Begin
  SetColTimeStamp(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColMemoryByName(ColName: String;
                                    AValue: TManagedMemoryStream);
Begin
  SetColMemory(ColByName(ColName), AValue);
End;

Procedure TOdbcStatement.SetColVariantByName(ColName: String;
                                     AValue: Variant);
Begin
  SetColVariant(ColByName(ColName), AValue);
End;

Function TOdbcStatement.GetCellString(Col, Row: SQLUSMALLINT): String;
Var
  //temp: TRowPtr;
  LrRowRec : TRowRec;

  Function FormatString(CValue: SQLPOINTER;
                        CType: SQLSMALLINT): String;
  Var
    fm: String;
  Begin
    Result:= fdb_odbc_objects.ToString(CValue, CType, StringTrimming);
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

var
  res: ansistring;
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
End;

Function TOdbcStatement.GetCellSingle(Col, Row: SQLUSMALLINT): Single;
Begin
  Result:= GetCellDouble(Col, Row);
End;

Function TOdbcStatement.GetCellDouble(Col, Row: SQLUSMALLINT): Double;
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

Function TOdbcStatement.GetCellBoolean(Col, Row: SQLUSMALLINT): Boolean;
Begin
  Result:= GetCellInteger(Col, Row) <> 0;
End;

Function TOdbcStatement.GetCellShortint(Col, Row: SQLUSMALLINT): ShortInt;
Begin
  Result:= GetCellInteger(Col, Row);
End;

Function TOdbcStatement.GetCellByte(Col, Row: SQLUSMALLINT): Byte;
Begin
  Result:= GetCellInteger(Col, Row);
End;

Function TOdbcStatement.GetCellSmallint(Col, Row: SQLUSMALLINT): SmallInt;
Begin
  Result:= GetCellInteger(Col, Row);
End;

Function TOdbcStatement.GetCellWord(Col, Row: SQLUSMALLINT): Word;
Begin
  Result:= GetCellInteger(Col, Row);
End;

Function TOdbcStatement.GetCellInteger(Col, Row: SQLUSMALLINT): Integer;
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

Function TOdbcStatement.GetCellCardinal(Col, Row: SQLUSMALLINT): Cardinal;
Begin
  Result:= GetCellInteger(Col, Row);
End;

Function TOdbcStatement.GetCellLongint(Col, Row: SQLUSMALLINT): LongInt;
Begin
  Result:= GetCellInteger(Col, Row);
End;

Function TOdbcStatement.GetCellLongword(Col, Row: SQLUSMALLINT): LongWord;
Begin
  Result:= GetCellInteger(Col, Row);
End;

Function TOdbcStatement.GetCellInt64(Col, Row: SQLUSMALLINT): Int64;
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

Function TOdbcStatement.GetCellDate(Col, Row: SQLUSMALLINT): TDate;
Var
  TS: fsl_utilities.TTimeStamp;
Begin
  TS:= GetCellTimeStamp(Col, Row);
  With Result Do
  Begin
    Year:= TS.Year;
    Month:= TS.Month;
    Day:= TS.Day;
  End;
End;

Function TOdbcStatement.GetCellTime(Col, Row: SQLUSMALLINT): TTime;
Var
  TS: fsl_utilities.TTimeStamp;
Begin
  TS:= GetCellTimeStamp(Col, Row);
  With Result Do
  Begin
    Hour:= TS.Hour;
    Minute:= TS.Minute;
    Second:= TS.Second;
  End;
End;

Function TOdbcStatement.GetCellTimeStamp(Col, Row: SQLUSMALLINT): fsl_utilities.TTimeStamp;
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

Function TOdbcStatement.GetCellMemory(Col, Row: SQLUSMALLINT): TManagedMemoryStream;
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
          tempCol^.FMemory := TManagedMemoryStream.Create;
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

Function TOdbcStatement.GetCellVariant(Col, Row: SQLUSMALLINT): Variant;
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

Procedure TOdbcStatement.SetCellString(Col, Row: SQLUSMALLINT;
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

Procedure TOdbcStatement.SetCellSingle(Col, Row: SQLUSMALLINT;
                               AValue: Single);
Begin
  SetCellDouble(Col, Row, AValue);
End;

Procedure TOdbcStatement.SetCellDouble(Col, Row: SQLUSMALLINT;
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

Procedure TOdbcStatement.SetCellBoolean(Col, Row: SQLUSMALLINT;
                                AValue: Boolean);
Begin
  If AValue Then
    SetCellInteger(Col, Row, 1)
  Else
    SetCellInteger(Col, Row, 0);
End;

Procedure TOdbcStatement.SetCellShortint(Col, Row: SQLUSMALLINT;
                                 AValue: ShortInt);
Begin
  SetCellInteger(Col, Row, AValue);
End;

Procedure TOdbcStatement.SetCellByte(Col, Row: SQLUSMALLINT;
                             AValue: Byte);
Begin
  SetCellInteger(Col, Row, AValue);
End;

Procedure TOdbcStatement.SetCellSmallint(Col, Row: SQLUSMALLINT;
                                 AValue: SmallInt);
Begin
  SetCellInteger(Col, Row, AValue);
End;

Procedure TOdbcStatement.SetCellWord(Col, Row: SQLUSMALLINT;
                             AValue: Word);
Begin
  SetCellInteger(Col, Row, AValue);
End;

Procedure TOdbcStatement.SetCellInteger(Col, Row: SQLUSMALLINT;
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

Procedure TOdbcStatement.SetCellCardinal(Col, Row: SQLUSMALLINT;
                                 AValue: Cardinal);
Begin
  SetCellInteger(Col, Row, AValue);
End;

Procedure TOdbcStatement.SetCellLongint(Col, Row: SQLUSMALLINT;
                                AValue: LongInt);
Begin
  SetCellInteger(Col, Row, AValue);
End;

Procedure TOdbcStatement.SetCellLongword(Col, Row: SQLUSMALLINT;
                                 AValue: LongWord);
Begin
  SetCellInteger(Col, Row, AValue);
End;

Procedure TOdbcStatement.SetCellInt64(Col, Row: SQLUSMALLINT;
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

Procedure TOdbcStatement.SetCellDate(Col, Row: SQLUSMALLINT;
                             AValue: TDate);
Var
  temp: TRowPtr;
  s: String;
  ts: fsl_utilities.TTimeStamp;
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
        s:= fdb_odbc_objects.ToString(@AValue, SQL_C_TYPE_DATE, stTrimBoth);
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

Procedure TOdbcStatement.SetCellTime(Col, Row: SQLUSMALLINT;
                             AValue: TTime);
Var
  temp: TRowPtr;
  s: String;
  ts: fsl_utilities.TTimeStamp;
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
        s:= fdb_odbc_objects.ToString(@AValue, SQL_C_TYPE_TIME, stTrimBoth);
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

Procedure TOdbcStatement.SetCellTimeStamp(Col, Row: SQLUSMALLINT;
                                  AValue: fsl_utilities.TTimeStamp);
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
          s:= fdb_odbc_objects.ToString(@Date, SQL_C_TYPE_DATE, stTrimBoth);
        End;
        SQL_C_TYPE_TIME:
        Begin
          With AValue Do
          Begin
            Time.Hour:= Hour;
            Time.Minute:= Minute;
            Time.Second:= Second;
          End;
          s:= fdb_odbc_objects.ToString(@Time, SQL_C_TYPE_TIME, stTrimBoth);
        End;
        Else
          s:= fdb_odbc_objects.ToString(@AValue, SQL_C_TYPE_TIMESTAMP, stTrimBoth);
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

Procedure TOdbcStatement.SetCellMemory(Col, Row: SQLUSMALLINT;
                               AValue: TManagedMemoryStream);
Begin
  SetColMemory(Col, AValue);
End;

Procedure TOdbcStatement.SetCellVariant(Col, Row: SQLUSMALLINT;
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

Function TOdbcStatement.GetCellStringByName(ColName: String;
                                    Row: SQLUSMALLINT): String;
Begin
  Result:= GetCellString(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellSingleByName(ColName: String;
                                    Row: SQLUSMALLINT): Single;
Begin
  Result:= GetCellSingle(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellDoubleByName(ColName: String;
                                    Row: SQLUSMALLINT): Double;
Begin
  Result:= GetCellDouble(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellBooleanByName(ColName: String;
                                     Row: SQLUSMALLINT): Boolean;
Begin
  Result:= GetCellBoolean(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellShortintByName(ColName: String;
                                      Row: SQLUSMALLINT): ShortInt;
Begin
  Result:= GetCellShortint(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellByteByName(ColName: String;
                                  Row: SQLUSMALLINT): Byte;
Begin
  Result:= GetCellByte(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellSmallintByName(ColName: String;
                                      Row: SQLUSMALLINT): SmallInt;
Begin
  Result:= GetCellSmallint(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellWordByName(ColName: String;
                                  Row: SQLUSMALLINT): Word;
Begin
  Result:= GetCellWord(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellIntegerByName(ColName: String;
                                     Row: SQLUSMALLINT): Integer;
Begin
  Result:= GetCellInteger(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellCardinalByName(ColName: String;
                                      Row: SQLUSMALLINT): Cardinal;
Begin
  Result:= GetCellCardinal(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellLongintByName(ColName: String;
                                     Row: SQLUSMALLINT): LongInt;
Begin
  Result:= GetCellLongint(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellLongwordByName(ColName: String;
                                      Row: SQLUSMALLINT): LongWord;
Begin
  Result:= GetCellLongword(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellInt64ByName(ColName: String;
                                   Row: SQLUSMALLINT): Int64;
Begin
  Result:= GetCellInt64(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellDateByName(ColName: String;
                                  Row: SQLUSMALLINT): TDate;
Begin
  Result:= GetCellDate(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellTimeByName(ColName: String;
                                  Row: SQLUSMALLINT): TTime;
Begin
  Result:= GetCellTime(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellTimeStampByName(ColName: String;
                                       Row: SQLUSMALLINT): fsl_utilities.TTimeStamp;
Begin
  Result:= GetCellTimeStamp(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellMemoryByName(ColName: String;
                                    Row: SQLUSMALLINT): TManagedMemoryStream;
Begin
  Result:= GetCellMemory(ColByName(ColName), Row);
End;

Function TOdbcStatement.GetCellVariantByName(ColName: String;
                                     Row: SQLUSMALLINT): Variant;
Begin
  Result:= GetCellVariant(ColByName(ColName), Row);
End;

Procedure TOdbcStatement.SetCellStringByName(ColName: String;
                                     Row: SQLUSMALLINT;
                                     AValue: String);
Begin
  SetCellString(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellSingleByName(ColName: String;
                                     Row: SQLUSMALLINT;
                                     AValue: Single);
Begin
  SetCellSingle(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellDoubleByName(ColName: String;
                                     Row: SQLUSMALLINT;
                                     AValue: Double);
Begin
  SetCellDouble(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellBooleanByName(ColName: String;
                                      Row: SQLUSMALLINT;
                                      AValue: Boolean);
Begin
  SetCellBoolean(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellShortintByName(ColName: String;
                                       Row: SQLUSMALLINT;
                                       AValue: ShortInt);
Begin
  SetCellShortint(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellByteByName(ColName: String;
                                   Row: SQLUSMALLINT;
                                   AValue: Byte);
Begin
  SetCellByte(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellSmallintByName(ColName: String;
                                       Row: SQLUSMALLINT;
                                       AValue: SmallInt);
Begin
  SetCellSmallint(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellWordByName(ColName: String;
                                   Row: SQLUSMALLINT;
                                   AValue: Word);
Begin
  SetCellWord(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellIntegerByName(ColName: String;
                                      Row: SQLUSMALLINT;
                                      AValue: Integer);
Begin
  SetCellInteger(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellCardinalByName(ColName: String;
                                       Row: SQLUSMALLINT;
                                       AValue: Cardinal);
Begin
  SetCellCardinal(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellLongintByName(ColName: String;
                                       Row: SQLUSMALLINT;
                                       AValue: LongInt);
Begin
  SetCellLongint(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellLongwordByName(ColName: String;
                                       Row: SQLUSMALLINT;
                                       AValue: LongWord);
Begin
  SetCellLongword(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellInt64ByName(ColName: String;
                                    Row: SQLUSMALLINT;
                                    AValue: Int64);
Begin
  SetCellInt64(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellDateByName(ColName: String;
                                   Row: SQLUSMALLINT;
                                   AValue: TDate);
Begin
  SetCellDate(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellTimeByName(ColName: String;
                                   Row: SQLUSMALLINT;
                                   AValue: TTime);
Begin
  SetCellTime(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellTimeStampByName(ColName: String;
                                        Row: SQLUSMALLINT;
                                        AValue: fsl_utilities.TTimeStamp);
Begin
  SetCellTimeStamp(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellMemoryByName(ColName: String;
                                     Row: SQLUSMALLINT;
                                     AValue: TManagedMemoryStream);
Begin
  SetCellMemory(ColByName(ColName), Row, AValue);
End;

Procedure TOdbcStatement.SetCellVariantByName(ColName: String;
                                      Row: SQLUSMALLINT;
                                      AValue: Variant);
Begin
  SetCellVariant(ColByName(ColName), Row, AValue);
End;

Function TOdbcStatement.ParamByName(ParamName: String): SQLUSMALLINT;
Var
  Count: Integer;
Begin
  Log(1, 'TOdbcStatement.ParamByName');

  ParamName:= UpperCase(ParamName);
  Count:= 0;
  While (Count < FParamNames.Count) And (UpperCase(FParamNames[Count]) <> ParamName) Do
    Inc(Count);

  If Count < FParamNames.Count Then
    Result:= Count+1
  Else
    Raise EODBCExpress.Create('Token '+ParamName+' not found in SQL statement.');
End;

Function TOdbcStatement.ColByName(ColName: String): SQLUSMALLINT;
Var
  Index: Integer;
Begin
  Log(1, 'TOdbcStatement.ColByName');

  { Bind Columns }
  BindCols;

  Index:= IndexOf(ColName, FColNames);
  If Index > -1 Then
    Result:= Index+1
  Else
    Raise EODBCExpress.Create('Identifier '+ColName+' not found in result set.');
End;

Procedure TOdbcStatement.NullCols(Const Cols: Array Of String);
Var
  i: Integer;
Begin
  For i:= 0 To High(Cols) Do
    ColNull[ColByName(Cols[i])]:= True;
End;

Procedure TOdbcStatement.PrimaryCols(Const Cols: Array Of String);
Var
  i: Integer;
Begin
  For i:= 0 To High(Cols) Do
    ColPrimary[ColByName(Cols[i])]:= True;
End;

Function TOdbcStatement.TypeString(SqlType: SQLSMALLINT;
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

Procedure TOdbcStatement.SetSkipByCursor(ASkipByCursor: Boolean);
Begin
  FSkipByCursor:= ASkipByCursor;

  UnPrepareHstmts;
End;

Procedure TOdbcStatement.SetSkipByPosition(ASkipByPosition: Boolean);
Begin
  FSkipByPosition:= ASkipByPosition;

  UnPrepareHstmts;
End;

{ TODBCContext }

Constructor TODBCContext.Create(Env : TOdbcEnv);
begin
  inherited Create;
  FEnv := env;
end;

function NullTS: TTimeStamp;
begin
  Result := DateTimeToTS(0);
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
  Writer.WriteString('OO1.0');

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
  Writer.WriteString('OO1.0');

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

{ TOdbcSchema }

procedure TOdbcSchema.SeTOdbcConnection(AHdbc: TOdbcConnection);
begin
  FHdbc:= AHdbc;
  FHstmt.Hdbc:= FHdbc;
end;

procedure TOdbcSchema.SetTables(ATables: TSchemaTables);
begin
end;

procedure TOdbcSchema.SetViews(AViews: TSchemaViews);
begin
end;

procedure TOdbcSchema.SetExecMarker(AExecMarker: String);
begin
  if (AExecMarker <> FExecMarker) and (Trim(AExecmarker) <> '') then
    FExecMarker:= Trim(AExecMarker);
end;

function TOdbcSchema.Prefix(AOwner, ATable: String): String;
begin
  AOwner:= Quoted(AOwner);
  ATable:= Quoted(ATable);

  if AOwner = '' then
    Result:= ATable
  else
    Result:= AOwner+'.'+ATable;
end;

constructor TOdbcSchema.Create(Env : TOdbcEnv);
begin
  inherited Create(Env);

  FHdbc:= nil;

  FHstmt:= TOdbcStatement.Create(FEnv, FHdbc);
  FTables:= TSchemaTables.Create;
  FTables.FSchema:= Self;
  FViews:= TSchemaViews.Create;
  FViews.FSchema:= Self;

  FToFile:= False;
  FExecMarker:= DefExecMarker;
  FNameConstraints:= DefNameConstraints;
  FAborted:= False;
end;

destructor TOdbcSchema.Destroy;
begin
  FHstmt.Free;
  FTables.Free;
  FViews.Free;

  inherited Destroy;
end;

procedure TOdbcSchema.SplitColumn(AIndexColumn: string;
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

procedure TOdbcSchema.DoProgress(Info: String);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Info);
end;

procedure TOdbcSchema.LoadTables;
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

procedure TOdbcSchema.DropTables(IgnoreErrors: Boolean);
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

procedure TOdbcSchema.LoadViews;
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

procedure TOdbcSchema.DropViews(IgnoreErrors: Boolean);
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

procedure TOdbcSchema.WriteLine(ctSQL: String);
begin
  Writeln(FText, ctSQL);
  Writeln(FText, ExecMarker);
end;

function TOdbcSchema.ReadLine: String;
var
  ctSQL: String;
begin
  Readln(FText, ctSQL);
  if UpperCase(Trim(ctSQL)) = UpperCase(ExecMarker) then
    Result:= DefExecMarker
  else
    Result:= ctSQL;
end;

procedure TOdbcSchema.GenScript(FileName: String);
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

procedure TOdbcSchema.RunScript(FileName: String);
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

procedure TOdbcSchema.Clear;
begin
  Tables.Clear;
  Views.Clear;
end;

procedure TOdbcSchema.Terminate;
begin
  FHstmt.Terminate;
end;

procedure TOdbcSchema.Abort;
begin
  FAborted:= True;
end;

{ TOdbcAdministrator }

procedure TOdbcAdministrator.RetrieveDataSources;
var
  Direction: SQLUSMALLINT;
  DSName, DSDriver: PChar;
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

  DSName := odbcPChar(DefaultStringSize);
  DSDriver := odbcPChar(DefaultStringSize);
  try
    while FEnv.Error.Success(SQLDataSources(FEnv.Handle, Direction, DSName, DefaultStringSize, {$IFDEF FPC}@{$ENDIF}StringLength1, DSDriver, DefaultStringSize, {$IFDEF FPC}@{$ENDIF}StringLength2)) do
    begin
      FDataSourceNames.Add(fromOdbcPChar(DSName, StringLength1));
      FDataSourceDrivers.Add(fromOdbcPChar(DSDriver, StringLength2));
      Direction:= SQL_FETCH_NEXT;
    end;
  finally
    freeMem(DSName);
    freeMem(DSDriver);
  end;
end;

procedure TOdbcAdministrator.RetrieveDrivers;
var
  Direction: SQLUSMALLINT;
  DName, DAttr: PChar;
  StringLength1, StringLength2: SQLSMALLINT;
begin
  { Retrieve Drivers }
  FDriverNames.Clear;

  Direction:= SQL_FETCH_FIRST;

  DName := odbcPChar(DefaultStringSize);
  DAttr := odbcPChar(DefaultStringSize);
  try
    while FEnv.Error.Success(SQLDrivers(FEnv.Handle, Direction, DName, DefaultStringSize, {$IFDEF FPC}@{$ENDIF}StringLength1, DAttr, DefaultStringSize, {$IFDEF FPC}@{$ENDIF}StringLength2)) do
    begin
      FDriverNames.Add(fromOdbcPChar(DName, StringLength1));
      Direction:= SQL_FETCH_NEXT;
    end;
  finally
    freeMem(DName);
    freeMem(DAttr);
  end;
end;

procedure TOdbcAdministrator.SetDataSourceType(ADataSourceType: TDataSourceType);
begin
  FDataSourceType:= ADataSourceType;
  Refresh;
end;

//procedure TOdbcAdministrator.SetDataSource(ADataSource: String);
//begin
//  ADataSource:= Trim(ADataSource);
//  if (ADataSource = '') or Valid(ADataSource) then
//    FDataSource:= ADataSource;
//end;
//
procedure TOdbcAdministrator.SetAttributes(AAttributes: TStrings);
begin
  FAttributes.Assign(AAttributes);
end;

//function TOdbcAdministrator.Configure(Request: SQLUSMALLINT): Boolean;
//var
//  i: Integer;
//  hwndParent: SQLHWND;
//  LAttributes: String;
//begin
//  if FDriver = '' then
//    raise EODBCExpress.Create('No Driver Value Specified');
//
//  hwndParent:= 0;
//
//  LAttributes:= '';
//  if FDataSource <> '' then
//    LAttributes:= LAttributes+'DSN='+FDataSource+#1;
//  if FUserName <> '' then
//    LAttributes:= LAttributes+'UID='+FUserName+#1;
//  if FPassword <> '' then
//    LAttributes:= LAttributes+'PWD='+FPassword+#1;
//  for i:= 0 to FAttributes.Count-1 do
//    LAttributes:= LAttributes+FAttributes[i]+#1;
//
//  InsertNulls(LAttributes);
//
//  Result:= SQLConfigDataSource(hwndParent, Request, PChar(FDriver), PChar(LAttributes));
//end;

function TOdbcAdministrator.GetDataSources: TStrings;
begin
  if FGetDataSources then
    RetrieveDataSources;
  FGetDataSources:= False;
  Result:= FDataSourceNames;
end;

function TOdbcAdministrator.GetDrivers: TStrings;
begin
  if FGetDrivers then
    RetrieveDrivers;
  FGetDrivers:= False;
  Result:= FDriverNames;
end;

constructor TOdbcAdministrator.Create(Env : TOdbcEnv);
begin
  inherited Create(Env);

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

destructor TOdbcAdministrator.Destroy;
begin
  FAttributes.Free;
  FDataSourceNames.Free;
  FDataSourceDrivers.Free;
  FDriverNames.Free;

  inherited Destroy;
end;

//function TOdbcAdministrator.Add: Boolean;
//begin
//  if FDataSourceType = dsSystem then
//    Result:= Configure(ODBC_ADD_SYS_DSN)
//  else
//    Result:= Configure(ODBC_ADD_DSN);
//end;
//
//function TOdbcAdministrator.Modify: Boolean;
//begin
//  if FDataSourceType = dsSystem then
//    Result:= Configure(ODBC_CONFIG_SYS_DSN)
//  else
//    Result:= Configure(ODBC_CONFIG_DSN);
//end;
//
//function TOdbcAdministrator.Remove: Boolean;
//begin
//  Result:= False;
//  case FDataSourceType of
//    dsDefault:
//      Result:= Configure(ODBC_REMOVE_DEFAULT_DSN);
//    dsUser:
//      Result:= Configure(ODBC_REMOVE_DSN);
//    dsSystem:
//      Result:= Configure(ODBC_REMOVE_SYS_DSN)
//  end;
//end;

//function TOdbcAdministrator.Valid(ADataSource: String): Boolean;
//begin
//  Result:= SQLValidDSN(PChar(ADataSource));
//end;

procedure TOdbcAdministrator.Refresh;
begin
  FGetDataSources:= True;
  FGetDrivers:= True;
end;

function TOdbcAdministrator.DataSourceDriver(ADataSource: String): String;
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
  ATableOwner: Pointer;
  temp: TCatalogColumn;
begin
  if FTableOwner = '' then
    ATableOwner:= nil
  else
    ATableOwner:= PChar(FTableOwner);

  FCatalog.FHstmt.Terminate;
  SQLColumns(FCatalog.FHstmt.Handle, nil, 0, ATableOwner, Length(FTableOwner),
    Pointer(PChar(FTableName)), Length(FTableName), nil, 0);

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
  if not FEnv.Error.Success(RetCode) then
    FEnv.Error.RaiseError(FCatalog.FHstmt, RetCode);

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
  if not FEnv.Error.Success(RetCode) then
    FEnv.Error.RaiseError(FCatalog.FHstmt, RetCode);

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
  if not FEnv.Error.Success(RetCode) then
    FEnv.Error.RaiseError(FCatalog.FHstmt, RetCode);

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
  if not FEnv.Error.Success(RetCode) then
    FEnv.Error.RaiseError(FCatalog.FHstmt, RetCode);

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
  if not FEnv.Error.Success(RetCode) then
    FEnv.Error.RaiseError(FCatalog.FHstmt, RetCode);

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
  inherited Create(Env);
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
  Result:= TCatalogTable.Create(FEnv);
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
  inherited Create(Env);
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
  if not FEnv.Error.Success(RetCode) then
    FEnv.Error.RaiseError(FCatalog.FHstmt, RetCode);

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
  inherited Create(Env);
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
  Result:= TCatalogProcedure.Create(FEnv);
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
  inherited Create(Env);
  FList:= TList.Create;
  FList.Clear;
end;

destructor TCatalogProcedures.Destroy;
begin
  FreeItems;
  FList.Free;

  inherited Destroy;
end;

{ TOdbcCatalog }

procedure TOdbcCatalog.RetrieveTables;
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
    if FEnv.Error.Success(RetCode) then
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
  if not FEnv.Error.Success(RetCode) then
    FEnv.Error.RaiseError(FHstmt, RetCode);

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

procedure TOdbcCatalog.RetrieveProcedures;
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
  if not FEnv.Error.Success(RetCode) then
    FEnv.Error.RaiseError(FHstmt, RetCode);

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

function TOdbcCatalog.GetTables: TCatalogTables;
begin
  if FGetTables then
    RetrieveTables;
  FGetTables:= False;
  Result:= FTables;
end;

function TOdbcCatalog.GetProcedures: TCatalogProcedures;
begin
  if FGetProcedures then
    RetrieveProcedures;
  FGetProcedures:= False;
  Result:= FProcedures;
end;

function TOdbcCatalog.GetTableNames: TStrings;
var
  i: Integer;
begin
  FTableNames.Clear;
  for i:= 0 to Tables.ItemCount-1 do
    FTableNames.Add(Tables[i].TableName);
  Result:= FTableNames;
end;

function TOdbcCatalog.GetProcedureNames: TStrings;
var
  i: Integer;
begin
  FProcedureNames.Clear;
  for i:= 0 to Procedures.ItemCount-1 do
    FProcedureNames.Add(Procedures[i].ProcedureName);
  Result:= FProcedureNames;
end;

procedure TOdbcCatalog.SeTOdbcConnection(AHdbc: TOdbcConnection);
begin
  FHdbc:= AHdbc;
  FHstmt.Hdbc:= FHdbc;

  Refresh;
end;

procedure TOdbcCatalog.SetTableOwner(ATableOwner: String);
begin
  if ATableOwner <> FTableOwner then
  begin
    FTableOwner:= ATableOwner;
    FGetTables:= True;
  end;
end;

procedure TOdbcCatalog.SetTableName(ATableName: String);
begin
  if ATableName <> FTableName then
  begin
    FTableName:= ATableName;
    FGetTables:= True;
  end;
end;

procedure TOdbcCatalog.SetTableType(ATableType: TTableTypeSet);
begin
  if ATableType <> FTableType then
  begin
    FTableType:= ATableType;
    FGetTables:= True;
  end;
end;

procedure TOdbcCatalog.SetProcedureOwner(AProcedureOwner: String);
begin
  if AProcedureOwner <> FProcedureOwner then
  begin
    FProcedureOwner:= AProcedureOwner;
    FGetProcedures:= True;
  end;
end;

procedure TOdbcCatalog.SetProcedureName(AProcedureName: String);
begin
  if AProcedureName <> FProcedureName then
  begin
    FProcedureName:= AProcedureName;
    FGetProcedures:= True;
  end;
end;


constructor TOdbcCatalog.Create(Env : TOdbcEnv; dbc : TOdbcConnection);
begin
  inherited Create(Env);

  FHdbc:= dbc;

  FHstmt:= TOdbcStatement.Create(env, dbc);

  FTables:= TCatalogTables.Create(env);
  FTables.FCatalog:= Self;
  FProcedures:= TCatalogProcedures.Create(env);
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

destructor TOdbcCatalog.Destroy;
begin
  FHstmt.Free;
  FTables.Free;
  FProcedures.Free;
  FTableNames.Free;
  FProcedureNames.Free;

  inherited Destroy;
end;

function TOdbcCatalog.TableByName(ATableOwner, ATableName: String): TCatalogTable;
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

function TOdbcCatalog.ProcedureByName(AProcedureOwner, AProcedureName: String): TCatalogProcedure;
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

procedure TOdbcCatalog.Refresh;
begin
  FGetTables:= True;
  FGetProcedures:= True;
end;

procedure TOdbcCatalog.Terminate;
begin
  FHstmt.Terminate;
end;

procedure TOdbcCatalog.ParseForeignKey(ForeignKey: String;
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

procedure TOdbcCatalog.ParseIndex(Index: String;
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

procedure TOdbcCatalog.ParseUniqueKey(UniqueKey: String;
                                    ColumnNames: TStrings);
begin
  SplitList(Trim(UniqueKey), ',', ColumnNames);
end;


End.


