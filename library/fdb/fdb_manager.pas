unit fdb_manager;

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
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  SysUtils, SyncObjs, Classes, Contnrs, IniFiles, Generics.Collections,
  fsl_base, fsl_utilities, fsl_threads,  fsl_fpc,
  fdb_logging, fdb_dialects;

const
  DEFAULT_CONNECTION_WAIT_LENGTH = 1000;
  DEFAULT_CONNECTION_REFRESH_PERIOD = 200;
  CONNECTION_UNKNOWN = 0;
  CONNECTION_OK = 1;
  CONNECTION_FAIL = 2;

type


  // these are all the known providers. Just because the enumerations are defined doesn't
  // mean that the provider is supported by all 3 of compiler, application, and system
  // access is odbc but settings are done differently
  TFDBProvider = (kdbpUnknown,    kdbpDSN,        kdbpODBC,     kdbpFirebird,    kdbpDBIsam,
                  kdbpDBXpress,   kdbpSoapClient, kdbpMySQL,    kdbpAccess,      kdbpSQLite);

  TFDBProviderSet = set of TFDBProvider;

const
  FSLDB_ALL_PROVIDERS = [Low(TFDBProvider) .. High(TFDBProvider)];

type


  {
    Lists possible database Column types
  }
  TFDBColumnType = (ctUnknown, ctBoolean, ctInteger, ctNumeric, ctFloat, ctChar, ctDateTime, ctBlob, ctInt64, ctUnicode);

  // Meta data
  TFDBTableType = (kdbUser, kdbView, kdbSystem);

  TFDBColumn = class (TFslObject)
  private
    FName: String;
    FLength: Integer;
    FDataType: TFDBColumnType;
    FNullable: Boolean;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(name : String); overload;
    function Link : TFDBColumn; overload;
    property Name : String read FName write FName;
    property DataType : TFDBColumnType read FDataType write FDataType;
    property Length : Integer read FLength write FLength;
    property Nullable : Boolean read FNullable write FNullable;
    function Describe : String;
  end;

  TFDBIndex = class (TFslObject)
  private
    FUnique: Boolean;
    FName: String;
    FColumns: TFslList<TFDBColumn>;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Name : String read FName write FName;
    property Unique : Boolean read FUnique write FUnique;
    property Columns : TFslList<TFDBColumn> read FColumns;
    function Describe : String;
  end;

  TFDBRelationship = class (TFslObject)
  private
    FColumn: String;
    FDestTable : String;
    FDestColumn : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    Property Column : String read FColumn write FColumn;
    Property DestTable : String read FDestTable write FDestTable;
    Property DestColumn : String read FDestColumn write FDestColumn;
    function Describe : String;
  end;

  TFDBTable = class (TFslObject)
  private
    FName: String;
    FColumns: TFslList<TFDBColumn>;
    FIndexes: TFslList<TFDBIndex>;
    FRelationships : TFslList<TFDBRelationship>;
    FTableType: TFDBTableType;
    FOwner: String;
    FDescription: String;
    FOrderMatters : Boolean;

  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TFDBTable; overload;
    property Columns : TFslList<TFDBColumn> read FColumns;
    property Indexes : TFslList<TFDBIndex> read FIndexes;
    Property Relationships : TFslList<TFDBRelationship> read FRelationships;
    property Name : String read FName write FName;
    property TableType : TFDBTableType read FTableType write FTableType;
    property Owner : String read FOwner write FOwner;
    property Description : String read FDescription write FDescription;
    Property OrderMatters : Boolean read FOrderMatters write FOrderMatters;

    function hasColumn(name : String) : boolean;
  end;

  TFDBMetaData = class (TFslObject)
  private
    FTables: TFslList<TFDBTable>;
    FProcedures : TStringList;
    FSupportsProcedures : Boolean;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Tables : TFslList<TFDBTable> read FTables;
    property Procedures : TStringList read FProcedures;
    property SupportsProcedures : Boolean read FSupportsProcedures write FSupportsProcedures;

    function HasTable(name : String; caseSensitive : boolean = false) : boolean;
    function GetTable(name : String) : TFDBTable;
  end;

  TFDBManager = class;
  TOnChangeConnectionCount = procedure (oSender : TFDBManager) of Object;
  TFDBBoundParam = class (TFslObject);


  {
    Database connection that exposes a SQL based interface to the appropriate database.
    These cannot be created directly - you must use a TDBConnPool.GetConnection call
    to get a connection. The connection must always be returned using
    TDBConnPool.YieldConnection otherwise the connection will leak.
  }
  TFDBConnection = class (TFslObject)
  Private
    FOwner: TFDBManager;
    FNoFree : Boolean;
    FBoundItems : TFslMap<TFDBBoundParam>;
    FUsage : String;
    FUsed : TDateTime;
    FTables : TStringList;
    FRowCount : integer;
    FPrepareCount : integer;
    FInTransaction : Boolean;

    // local storage for applications using the connection
    FHolder: TObject;
    FTag: Integer;

    // execution
    FSQL : string;
    FTerminated: Boolean;
    FPrepared : boolean;
    FTransactionId: String;
    function GetTables : TStrings;
    function LookupInternal(ATableName, AKeyField, AKeyValue, AValueField, ADefault: String; bAsString: Boolean): String;
    function GetColBlobAsString(ACol: Integer): String;
    function GetColBlobAsStringByName(AName: String): String;
  Protected
    // caching for blobs, for use by concrete implementations
    procedure KeepBoundObj(sName : String; AObj : TFDBBoundParam);

    // worker routines for descendants to override
    procedure StartTransactV; virtual; abstract;
    procedure CommitV; virtual; abstract;
    procedure RollbackV; virtual; abstract;
    procedure RenameTableV(AOldTableName, ANewTableName: String); virtual; abstract;
    procedure RenameColumnV(ATableName, AOldColumnName, ANewColumnName: String; AColumnDetails: String); virtual; abstract;
    procedure DropTableV(ATableName : String); virtual; abstract;
    procedure DropColumnV(ATableName, AColumnName : String); virtual; abstract;
    procedure ClearDatabaseV; virtual; abstract;
    procedure PrepareV; virtual; abstract;
    procedure ExecuteV; virtual; abstract;
    procedure TerminateV; virtual; abstract;
    function  FetchNextV: Boolean; virtual; abstract;
    function ColByNameV(AColName: String): Integer; virtual; abstract;
    function ColNameV(ACol: Integer): String; virtual; abstract;
    procedure BindInt64V(AParamName: String; AParamValue: Int64); virtual; abstract;
    procedure BindIntegerV(AParamName: String; AParamValue: Integer); virtual; abstract;
    procedure BindKeyV(AParamName: String; AParamValue: Integer); virtual; abstract;
    procedure BindDoubleV(AParamName: String; AParamValue: Double); virtual; abstract;
    procedure BindStringV(AParamName: String; AParamValue: String); virtual; abstract;
    procedure BindTimeStampV(AParamName: String; AParamValue: fsl_utilities.TTimeStamp); virtual; abstract;
    procedure BindDateTimeExV(AParamName: String; AParamValue: TFslDateTime); virtual; abstract;
    procedure BindBlobV(AParamName: String; AParamValue: TBytes); virtual; abstract;
    procedure BindNullV(AParamName: String); virtual; abstract;
    function GetColCountV: Integer; Virtual; Abstract;
    function GetColStringV(ACol: Word): String; Virtual; Abstract;
    function GetColIntegerV(ACol: Word): Integer; Virtual; Abstract;
    function GetColInt64V(ACol: Word): Int64; Virtual; Abstract;
    function GetColDoubleV(ACol: Word): Double; Virtual; Abstract;
    function GetColBlobV(ACol: Word): TBytes; Virtual; Abstract;
    function GetColNullV(ACol: Word): Boolean; Virtual; Abstract;
    function GetColTimestampV(ACol: Word): fsl_utilities.TTimestamp; Virtual; Abstract;
    function GetColDateTimeExV(ACol: Word): TFslDateTime; Virtual; Abstract;
    function GetColTypeV(ACol: Word): TFDBColumnType; Virtual; Abstract;
    function GetColKeyV(ACol: Word): Integer; Virtual; Abstract;
    function GetRowsAffectedV: Integer; Virtual; Abstract;
    function FetchMetaDataV : TFDBMetaData; Virtual; Abstract;
    procedure ListTablesV(AList : TStrings); virtual; abstract;
    function DatabaseSizeV : int64; virtual; abstract;
    Function TableSizeV(sName : String):int64; virtual; abstract;
    function SupportsSizingV : Boolean; virtual; abstract;

    procedure CheckRelease;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(AOwner: TFDBManager);
    destructor Destroy; Override;

    function link : TFDBConnection; overload;

    { internal use only  - platform specific initialization }
    procedure Initialise; virtual;

    {
      After setting the SQL content, prepare the statement so Parameter
      Binding and execution can be done. You must call prepare before
      binding and execution.

      If you are using Interbase, you must call terminate after calling
      prepare - use a try finally construct
    }
    procedure Prepare;

    {
      Execute the SQL Statement. Will raise an exception if there is a problem
    }
    procedure Execute;

    {
      Clean up. You should call terminate before returning the
      connection to the pool or using it again
    }
    procedure Terminate;


    property Usage: String Read FUsage Write FUsage;
    property UseStarted : TDateTime read FUsed;
    property Holder: TObject Read FHolder Write FHolder;
    property Tag: Integer Read FTag Write FTag;
    property Owner: TFDBManager Read FOwner;
    property Prepared : boolean read FPrepared;

    // when the application finishes with the connection, it should use one of these to free the connection
    procedure Release;
    procedure Error(AException: Exception; AErrMsg : string = '');

    // run a group of sql statements as a group
    // note: if you avoid binding timestamps and binaries in order to use this
    // routine, the NDM will remove it and make you clean up
    // not supported in scripting
    procedure ExecSQLBatch(ASql: array of String);

    function FetchMetaData : TFDBMetaData;
  
    // public for scripting engine - usually would be private
    function GetColCount: Integer;
    function GetColString(ACol: Integer): String;
    function GetColInteger(ACol: Integer): Integer;
    function GetColInt64(ACol: Integer): Int64;
    function GetColDouble(ACol: Integer): Double;
    function GetColBlob(ACol: Integer): TBytes;
    function GetColNull(ACol: Integer): Boolean;
    function GetColTimestamp(ACol: Integer): fsl_utilities.TTimestamp;
    function GetColDateTimeEx(ACol: Integer): TFslDateTime;
    function GetColType(ACol: Integer): TFDBColumnType;
    function GetRowsAffected: Integer;

    function GetColStringByName(AName: String): String;
    function GetColBlobByName(AName: String): TBytes;
    function GetColIntegerByName(AName: String): Integer;
    function GetColInt64ByName(AName: String): Int64;
    function GetColDoubleByName(AName: String): Double;
    function GetColTimeStampByName(AName: String): fsl_utilities.TTimestamp;
    function GetColDateTimeExByName(AName: String): TFslDateTime;
    function GetColTypeByName(AName: String): TFDBColumnType;
    function GetColNullByName(AName: String): Boolean;

      function GetColKey(ACol: Integer): Integer;
    function GetColKeyByName(AName: String): Integer;
  
    {
     Execute a single SQL statement (update, insert, etc. Returns the number of rows affected). You can't use this meaningfully with select statements
    }
    function ExecSQL(ASql: String) : integer; overload;
    function ExecSQL(ASql: String; rows : integer) : integer; overload;

    {
      Get the size of the database
    }
    function DatabaseSize : int64;

    {
      Get the size of the specified table
    }
    Function TableSize(sName : String):int64;

    {
      True if the underlying connection supports determining database and table size
    }
    function SupportsSizing : Boolean;

    {
     Execute a Count SQL Statement and return the value returned.
     Use an SQL Statement of the following form:

       select count(KeyField) from Table where conditions

     The first column of the first set of data is returned as an integer.
     It's also possible to use sql such as

       Select max(KeyField) from Table

    }
    function CountSQL(ASql: String): Cardinal;

    {
      Quickly check whether table sTableName has a record where sKeyField has value iKey. assumes
      that keyField has an appropriate index.
    }
    Function ExistsByKey(Const sTableName, sKeyField : String; ikey : Integer) : Boolean;

    {
      Given a tablename, return the value of ValueField where KeyField
      is the same as keyvalue. the keyfield can be a number or a string.
      if there is no match, Default will be returned as the value
    }
    function Lookup(ATableName, AKeyField, AKeyValue, AValueField, ADefault: String): String;

    {
      Given a tablename, return the value of ValueField where KeyField
      is the same as keyvalue. the keyfield can be a number or a string.
      if there is no match, Default will be returned as the value

      The SQL will always be generated as a string - avoid cast errors from SQL
    }
    function LookupString(ATableName, AKeyField, AKeyValue, AValueField, ADefault: String): String;

    {
      Rename a table in the database. Each platform generally provides a
      specific mechanism to do this, but the implementation varies widely.
      this works on all supported platforms.

      Renaming may fail to due to relational or other constraints. In this
      case an exception will usually be raised (provider platform dependent)
    }
    procedure RenameTable(AOldTableName, ANewTableName: String);

    {
      Drop a table from the database. on most platforms this equates to
      executing the SQL statement Drop Table "tablename"
    }
    procedure DropTable(ATableName : String);

    {
      Drop a column from a table in the database.
    }
    procedure DropColumn(ATableName, AColumnName: String);

    {
      Rename a Column in a table. Each platform generally provides a
      specific mechanism to do this, but the implementation varies widely.
      this works on all supported platforms.

      Renaming may fail to due to relational or other constraints. In this
      case an exception will usually be raised (provider platform dependent)
    }
    procedure RenameColumn(ATableName, AOldColumnName, ANewColumnName: String; AColumnDetails: String = '');

      procedure ListTables(AList : TStrings);
  
    {
      A List of all the tables in the database
    }
    property Tables : TStrings Read GetTables;

    {
      Completely drop non system content in database. For obvious reasons, this
      needs to be used with considerable care. Also it should be used before
      anything else has been done with the database.
    }
    procedure ClearDatabase;

    {
      Start a transaction. Close with Commit or RollBack.
    }
    procedure StartTransact;

    {
      Close a transaction and commit changes to the database.
      (note: Multi-stage commits & Rollbacks  are not supported)
    }
    procedure Commit;

    {
      Abandon transaction (no changes to the database).
      (note: Multi-stage commits & Rollbacks are not supported)
    }
    procedure Rollback;

    property InTransaction : Boolean read FInTransaction;

    {
       Iterate through the recordset after execute has been called.
       You must call Fetchnext before the the first record. Fetchnext
       will return false once there is no more records to retrieve.

       Forward cursors only
    }
    function FetchNext: Boolean;

    {
      Determine the index of a column by it's name
    }
    function ColByName(AColName: String): Integer;

    {
      Determine the Name of a column by it's index
    }
    function ColName(ACol: Integer): String;


    {
      Bind an Int64 value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:i)
      this will bind the value i to the parameter.
    }
    procedure BindInt64(AParamName: String; AParamValue: Int64);

    {
      Bind an Integer value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:i)
      this will bind the value i to the parameter.
    }
    procedure BindInteger(AParamName: String; AParamValue: Integer);
      procedure BindKey(AParamName: String; AParamValue: Integer);
  
    {
      Bind a Double (Float) value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:d)
    }
    procedure BindDouble(AParamName: String; AParamValue: Double);

    {
      Bind a String value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:s)
    }
    procedure BindString(AParamName: String; AParamValue: String);

    {
      Bind a String value to a named parameter, or null
    }
    procedure BindStringOrNull(AParamName: String; AParamValue: String);

    {
      Bind a TTimeStamp value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:t)
    }
    procedure BindTimeStamp(AParamName: String; AParamValue: fsl_utilities.TTimeStamp);

    {
      Bind a DateTime value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:t)
    }
    procedure BindDateTimeEx(AParamName: String; AParamValue: TFslDateTime);

      {
      Bind a Binary value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:t)
      use this for Blob fields

      Note that BindBinary works differently to the other bind calls.
      It is the caller's responsibility to make sure that the memory
      pointed to in the binary does not change until after execute is
      called.
    }
    procedure BindBlob(AParamName: String; AParamValue: TBytes);
  
    {
      Bind a Binary value to a named parameter. But present a string for binding
      You can call this after using an SQL statement like this:
        insert into table (field) values (:t)
      use this for Blob fields
    }
    procedure BindBlobFromString(AParamName: String; AParamValue: String);

    {
      Bind an integer from a boolean value. Database value will be 1 if true
    }
    procedure BindIntegerFromBoolean(AParamName: String; AParamValue: Boolean);

    {
      Bind the Null value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:t)
    }
    procedure BindNull(AParamName: String);

      property ColKey       [ACol: Integer]: Integer Read GetColKey;
  
    {
     Get Column Col Field Type
    }
    property ColType      [ACol: Integer]: TFDBColumnType Read GetColType;
    {
     True if Column ACol(index) Value is Null
    }
    property ColNull      [ACol: Integer]: Boolean Read GetColNull;
    {
     Get true if Column ACol(index) is null
    }
    property ColString    [ACol: Integer]: String Read GetColString;
    {
    Get Column ACol(index) as an Integer
    }
    property ColInteger   [ACol: Integer]: Integer Read GetColInteger;
    {
    Get Column ACol(index) as a Int64
    }
    property ColInt64     [ACol: Integer]: Int64 Read GetColInt64;
    {
    Get Column ACol(index) as a Double (Float)
    }
    property ColDouble    [ACol: Integer]: Double Read GetColDouble;

    {
    Get Column ACol(index) as a blob
    }
    property ColBlob    [ACol: Integer]: TBytes Read GetColBlob;
    property ColBlobAsString [ACol: Integer]: String Read GetColBlobAsString;
    {
    Get Column ACol(index) as a TTimestamp
    }
    property ColTimestamp [ACol: Integer]: fsl_utilities.TTimestamp Read GetColTimestamp;

    {
    Get Column ACol(index) as a DateAndTime
    }
    property ColDateTimeEx [ACol: Integer]: TFslDateTime Read GetColDateTimeEx;

      property ColKeyByName       [AName: String]: Integer Read GetColKeyByName;
  
    {
      Get Column "AName" Field Type}
    property ColTypeByName      [AName: String]: TFDBColumnType Read GetColTypeByName;
    {
      true if Column "AName" value is Null}
    property ColNullByName      [AName: String]: Boolean Read GetColNullByName;
    {
      Get Column "AName" as a String}
    property ColStringByName    [AName: String]: String Read GetColStringByName;
    {
      Get Column "AName" as an integer}
    property ColIntegerByName   [AName: String]: Integer Read GetColIntegerByName;
    {
      Get Column "AName" as a Int64}
    property ColInt64ByName     [AName: String]: Int64 Read GetColInt64ByName;
    {
      Get Column "AName" as a Double (Float)}
    property ColDoubleByName    [AName: String]: Double Read GetColDoubleByName;
    {
      Get Column "AName" as a Blob}
    property ColBlobByName    [AName: String]: TBytes Read GetColBlobByName;
    property ColBlobAsStringByName[AName: String]: String Read GetColBlobAsStringByName;
    {
      Get Column "AName" as a TTimeStamp}
    property ColTimeStampByName [AName: String]: fsl_utilities.TTimeStamp Read GetColTimeStampByName;
    {
      Get Column "AName" as a TFslDateTime}
    property ColDateTimeExByName [AName: String]: TFslDateTime Read GetColDateTimeExByName;
    {
      Number of columns in current result set
    }
    property ColCount: Integer Read GetColCount;

    {
      The number of rows affected by an insert, update or delete.
      not valid after a select
    }
    property RowsAffected: Integer Read GetRowsAffected;

    property transactionId : String read FTransactionId write FTransactionId;
  published
    {
      The SQL string to execute
    }
    property SQL: String Read FSql Write FSql;
  end;

  {$IFNDEF FPC}
  TFDBConnectionProc = reference to Procedure (conn : TFDBConnection);
  {$ENDIF}

  { TFDBManager }

  TFDBManager = class(TFslObject)
  Private
    FSemaphore : TSemaphore;
    FWaitCreate : boolean;
    FConnections : TFslList<TFDBConnection>;
    FAvail: TFslList<TFDBConnection>;
    FInUse : TFslList<TFDBConnection>;
    FDBLogger : TFDBLogger;
    FClosing : boolean;
    FOnChangeConnectionCount : TOnChangeConnectionCount;
    FServerIsAvailable : Boolean;
    FLastServerGood : TDateTime;
    FLastServerError : String;
    FThreadWaitCount : integer;

    FMaxConnCount : Integer;
    FName : string;
    FTag : integer;
    function PopAvail : TFDBConnection;
    function GetCurrentCount: Integer;
    procedure Release(AConn : TFDBConnection);
    procedure Error(AConn : TFDBConnection; AException: Exception; AErrMsg : string);
    function GetCurrentUse: Integer;
    procedure SetMaxConnCount(const Value: Integer);
    procedure CheckWait;
  Protected
    FLock : TFslLock;

    function ConnectionFactory: TFDBConnection; Virtual; Abstract;
    function GetDBPlatform: TFDBPlatform; Virtual; Abstract;
    function GetDBProvider: TFDBProvider; Virtual; Abstract;
    function GetDBDetails: String; Virtual; Abstract;
    function GetDriver: String; Virtual; Abstract;
    procedure init; virtual;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(AName : String; AMaxConnCount: Integer); overload;
    destructor Destroy; Override;

    function Link : TFDBManager; overload;

    procedure ExecSQL(ASql, AName : String);
    function CountSQL(ASql, AName : String) : integer;
    function GetConnection(const AUsage: String): TFDBConnection;
    {$IFNDEF FPC}
    procedure connection(usage : String; proc : TFDBConnectionProc);
    {$ENDIF}

    procedure checkConnection;

    property MaxConnCount : Integer Read FMaxConnCount write SetMaxConnCount;
    property CurrConnCount: Integer Read GetCurrentCount;
    property CurrUseCount : Integer read GetCurrentUse;

    property Logger : TFDBLogger read FDBLogger;
    property Platform: TFDBPlatform read GetDBPlatform;
    property Provider : TFDBProvider read GetDBProvider;
    property DBDetails: String read GetDBDetails;
    Property Driver : String read GetDriver;
    function GetConnSummary : String;
    property Tag : integer read FTag write FTag;

    Property ServerIsAvailable : Boolean read FServerIsAvailable;
    Function ServerErrorStatus : String;
    property Name : string read FName;

    property OnChangeConnectionCount : TOnChangeConnectionCount Read FOnChangeConnectionCount Write FOnChangeConnectionCount;
    class function IsSupportAvailable(APlatform : TFDBPlatform; Var VMsg : String):Boolean; virtual; abstract;
  end;

  TFDBManagerClass = class of TFDBManager;

  TFDBManagerEvent = procedure (AConnMan : TFDBManager; ABeingCreated : Boolean) of object;

  TFDBHook = class (TFslObject)
  private
    FHook : TFDBManagerEvent;
    FName : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(Name : String; Hook : TFDBManagerEvent);
  end;

  TFDBManagerList = class (TFslObject)
  private
    FLock : TFslLock;
    FHooks : TFslList<TFDBHook>;
    FList : TList<TFDBManager>; // this can't own because then entries are never freed
    procedure AddConnMan(AConnMan : TFDBManager);
    procedure RemoveConnMan(AConnMan : TFDBManager);
    function GetConnMan(i : Integer):TFDBManager;
    function GetConnManByName(s : String):TFDBManager;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
    property ConnMan[i : Integer]:TFDBManager read GetConnMan;
    property ConnManByName[s : String]:TFDBManager read GetConnManByName; default;
    function HasConnManByName(s : String) : Boolean;
    procedure RegisterHook(AName : String; AHook : TFDBManagerEvent);
    procedure UnRegisterHook(AName : String);
    function dump : String;
  end;

{
   Get a string Description of a given column type
}
function DescribeType(AColType: TFDBColumnType): String;

function DBManagers : TFDBManagerList;

implementation

const
  ASSERT_UNIT = 'fdb_manager';

  KDB_COLUMN_TYPE_NAMES : Array [TFDBColumnType] of String =
            ('ctUnknown', 'ctBoolean', 'ctInteger', 'ctNumeric', 'ctFloat', 'ctChar', 'ctDateTime', 'ctBlob', 'ctInt64', 'ctUnicode');

  KDB_TABLE_TYPE_NAMES : Array [TFDBTableType] of String =
            ('kdbUser', 'kdbView', 'kdbSystem');


var
  GManagers : TFDBManagerList = nil;

{ TFDBConnection }

constructor TFDBConnection.Create(AOwner: TFDBManager);
begin
  inherited create;
  FNoFree := false;
  FOwner := AOwner;
  FUsage := '';
  FHolder := nil;
  FTag := 0;
  FSQL := '';
  FTerminated := true;
  FInTransaction := false;
  FBoundItems := TFslMap<TFDBBoundParam>.create('KDB.Parameters');
  FTables := TStringList.create;
end;

destructor TFDBConnection.Destroy;
begin
  FBoundItems.free;
  FTables.free;
  inherited;
end;

procedure TFDBConnection.BindBlob(AParamName: String; AParamValue: TBytes);
begin
  if (aParamValue = nil) then
    BindNullV(AParamName)
  else
    BindBlobV(AParamName, AParamValue);
end;

procedure TFDBConnection.BindBlobFromString(AParamName, AParamValue: String);
var
  b : TBytes;
begin
  b := TEncoding.UTF8.GetBytes(AParamValue);
  BindBlob(AParamName, b);
end;

procedure TFDBConnection.BindIntegerFromBoolean(AParamName: String; AParamValue: Boolean);
begin
  if AParamValue then
    begin
    BindInteger(AParamName, 1);
    end
  else
    begin
    BindInteger(AParamName, 0);
    end;
end;

function TFDBConnection.CountSQL(ASql: String): Cardinal;
begin

  FSQL := ASql;
  Prepare;
  try
    Execute;
    if not FetchNext then
      result := 0
    else
      Result := ColInteger[1];
  finally
    Terminate;
    end;
end;

procedure TFDBConnection.Error(AException: Exception; AErrMsg : string ='');
begin
  FOwner.Error(self, AException, AErrMsg);
end;

function TFDBConnection.ExecSQL(ASql: String; rows : integer) : integer;
begin

  FSQL := ASql;
  Prepare;
  try
    Execute;
    result := GetRowsAffected;
    if (result <> rows) then
      raise EDBException.create('Error running sql - wrong row count (expected '+inttostr(rows)+', affected '+inttostr(result)+' for sql '+asql+')');
  finally
    Terminate;
    end;
end;

Function TFDBConnection.ExecSQL(ASql: String) : integer;
begin
  if asql = '' then
    exit(0);

  FSQL := ASql;
  Prepare;
  try
    Execute;
    result := GetRowsAffected;
  finally
    Terminate;
    end;
end;

procedure TFDBConnection.ExecSQLBatch(ASql: array of String);
var
  i: Integer;
begin

  if InTransaction then
    begin
    for i := low(ASql) to high(ASql) do
      ExecSQL(ASql[i]);
    end
  else
    begin
    StartTransact;
    try
      for i := low(ASql) to high(ASql) do
        ExecSQL(ASql[i]);
      Commit;
    except
      on e : exception do
      begin
        Rollback;
        recordStack(e);
        raise;
      end;
    end;
  end;
end;

function TFDBConnection.GetColDoubleByName(AName: String): Double;
begin
  result := GetColDouble(ColByName(AName));
end;

function TFDBConnection.GetColInt64ByName(AName: String): Int64;
begin
  result := GetColInt64(ColByName(AName));
end;

function TFDBConnection.GetColIntegerByName(AName: String): Integer;
begin
  result := GetColInteger(ColByName(AName));
end;

function TFDBConnection.GetColKeyByName(AName: String): Integer;
begin
  result := GetColKey(ColByName(AName));
end;

function TFDBConnection.GetColNullByName(AName: String): Boolean;
begin
  result := GetColNull(ColByName(AName));
end;

function TFDBConnection.GetColStringByName(AName: String): String;
begin
  result := GetColString(ColByName(AName));
end;

function TFDBConnection.GetColTimeStampByName(AName: String): fsl_utilities.TTimestamp;
begin
  result := GetColTimestamp(ColByName(AName));
end;

function TFDBConnection.GetColDateTimeExByName(AName: String): TFslDateTime;
begin
  result := TFslDateTime.fromTS(GetColTimestamp(ColByName(AName)));
end;

function TFDBConnection.GetColTypeByName(AName: String): TFDBColumnType;
begin
  result := GetColType(ColByName(AName));
end;

procedure TFDBConnection.Release;
begin
  CheckRelease;
  FOwner.Release(self);
end;


function TFDBConnection.Lookup(ATableName, AKeyField, AKeyValue, AValueField, ADefault: String): String;
begin
  result := LookupInternal(ATableName, AKeyField, AKeyValue, AValueField, ADefault, False);
end;

function TFDBConnection.LookupString(ATableName, AKeyField, AKeyValue, AValueField, ADefault: String): String;
begin
  result := LookupInternal(ATableName, AKeyField, AKeyValue, AValueField, ADefault, True);
end;

function TFDBConnection.LookupInternal(ATableName, AKeyField, AKeyValue, AValueField, ADefault: String; bAsString : Boolean): String;
var
  FVal : Double;
begin

  if StringIsInteger32(AKeyValue) and not bAsString then
    FSQL := 'Select ' + AValueField + ' from ' + ATableName + ' where ' + AKeyField + ' = ' + AKeyValue
  else
    FSQL := 'Select ' + AValueField + ' from ' + ATableName + ' where ' + AKeyField + ' = ''' + SQLWrapString(AKeyValue) + '''';
  Prepare;
  try
    Execute;
    if FetchNext then
      begin
      case ColTypeByName[AValueField] of
        ctUnknown: raise EDBException.Create('Field type UNKNOWN not supported in a macro lookup');
        ctBoolean: raise EDBException.Create('Field type Boolean not supported in a macro lookup');
        ctBlob: raise EDBException.Create('Field type Blob not supported in a macro lookup');
        ctInteger, ctInt64, ctNumeric:
          begin
          Result := IntToStr(ColIntegerByName[AValueField]);
          end;
        ctFloat:
          begin
          // ok, well, we will try to get it as an integer if we can.
          FVal := ColDoubleByName[AValueField];
          if abs(FVal - trunc(FVal)) < 0.00001 then // allow for float uncertainty
            result := inttostr(round(FVal))
          else
            result := FloatToStrF(FVal, ffGeneral, 4, 4);
          end;
        ctChar:
          begin
          Result := ColStringByName[AValueField];
          end;
        ctDateTime:
          begin
          Result := FormatDateTime('c', TsToDateTime(ColTimeStampByName[AValuefield]));
          end;
        else
          begin
          raise EDBException.Create('Field type unknown in a macro lookup');
          end;
        end;
      end
    else
      begin
      Result := ADefault;
      end;
  finally
    Terminate;
    end;
end;

procedure TFDBConnection.Prepare;
begin
  {$IFOPT C+}
  if FPrepared then
    raise EDBException.Create('Attempt to reuse ODBC connection "'+FUsage+'" while it is in use (sql = "'+FSQL+'")');
  {$ENDIF}
  FTerminated := false;
  FBoundItems.Clear;
  inc(FPrepareCount);
  PrepareV;
  FPrepared := true;
end;

function TFDBConnection.FetchNext: Boolean;
begin
  inc(FRowCount);
  result := FetchNextV;
end;

procedure TFDBConnection.Terminate;
begin
  if not FTerminated then
  begin
    FPrepared := false;
    FTerminated := true;
    TerminateV;
  end;
end;

procedure TFDBConnection.StartTransact;
begin
  StartTransactV;
  FInTransaction := true;
  FTransactionId := NewGuidId;
end;

procedure TFDBConnection.Commit;
begin
  // order here is important.
  // if the commit fails, then a rollback is required, so we are still in the transaction
  CommitV;
  FInTransaction := False;
end;

procedure TFDBConnection.Rollback;
begin
  FInTransaction := False;
  RollbackV;
end;

procedure TFDBConnection.KeepBoundObj(sName : String; AObj : TFDBBoundParam);
begin
  FBoundItems.AddOrSetValue(sName, aObj);
end;

function TFDBConnection.link: TFDBConnection;
begin
  result := TFDBConnection(inherited link);
end;

function TFDBConnection.GetTables : TStrings;
begin
  FTables.Clear;
  ListTables(FTables);
  result := FTables;
end;


procedure TFDBConnection.Initialise;
begin
  //nothing
end;

procedure TFDBConnection.BindDouble(AParamName: String; AParamValue: Double);
begin
  BindDoubleV(AParamName, AParamValue);
end;

procedure TFDBConnection.BindInt64(AParamName: String; AParamValue: Int64);
begin
  BindInt64V(AParamName, AParamValue);
end;

procedure TFDBConnection.BindInteger(AParamName: String; AParamValue: Integer);
begin
  BindIntegerV(AParamName, AParamValue);
end;

procedure TFDBConnection.BindKey(AParamName: String; AParamValue: Integer);
begin
  BindKeyV(AParamName, AParamValue);
end;

procedure TFDBConnection.BindNull(AParamName: String);
begin
  BindNullV(AParamName);
end;

procedure TFDBConnection.BindString(AParamName, AParamValue: String);
begin
  BindStringV(AParamName, AParamValue);
end;

procedure TFDBConnection.BindStringOrNull(AParamName, AParamValue: String);
begin
  if AParamValue = '' then
    BindNull(aParamName)
  else
    BindString(aParamName, AParamValue);
end;

procedure TFDBConnection.BindTimeStamp(AParamName: String; AParamValue: TTimeStamp);
begin
  BindTimeStampV(AParamName, AParamValue);
end;

procedure TFDBConnection.CheckRelease;
begin
  {$IFOPT C+}
  if FPrepared then
    raise EDBException.Create('Attempt to release ODBC connection "'+FUsage+'" before it is terminated');
  {$ENDIF}
end;

procedure TFDBConnection.ClearDatabase;
begin
  ClearDatabaseV;
end;

function TFDBConnection.ColByName(AColName: String): Integer;
begin
  result := ColByNameV(AColName);
end;

function TFDBConnection.ColName(ACol: Integer): String;
begin
  result := ColNameV(ACol);
end;

procedure TFDBConnection.Execute;
begin
  ExecuteV;
end;

procedure TFDBConnection.RenameColumn(ATableName, AOldColumnName, ANewColumnName, AColumnDetails: String);
begin
  RenameColumnV(ATableName, AOldColumnName, ANewColumnName, AColumnDetails);
end;

procedure TFDBConnection.RenameTable(AOldTableName, ANewTableName: String);
begin
  RenameTableV(AoldTableName, ANewTableName);
end;

function TFDBConnection.FetchMetaData: TFDBMetaData;
begin
  result := FetchMetaDataV;
end;

function TFDBConnection.GetColBlob(ACol: Integer): TBytes;
begin
  result := GetColBlobV(ACol);
end;

function TFDBConnection.GetColBlobAsString(ACol: Integer): String;
begin
  result := TEncoding.UTF8.GetString(ColBlob[aCol]);
end;

function TFDBConnection.GetColBlobAsStringByName(AName: String): String;
begin
  result := TEncoding.UTF8.GetString(ColBlobByName[AName]);
end;

function TFDBConnection.GetColBlobByName(AName: String): TBytes;
begin
  result := GetColBlob(ColByName(AName));
end;

function TFDBConnection.GetColCount: Integer;
begin
  result := GetColCountV;
end;

function TFDBConnection.GetColDouble(ACol: Integer): Double;
begin
  result := GetColDoubleV(ACol);
end;

function TFDBConnection.GetColInt64(ACol: Integer): Int64;
begin
  result := GetColInt64V(ACol);
end;

function TFDBConnection.GetColInteger(ACol: Integer): Integer;
begin
  result := GetColIntegerV(ACol);
end;

function TFDBConnection.GetColKey(ACol: Integer): Integer;
begin
  result := GetColKeyV(ACol);
end;


function TFDBConnection.GetColNull(ACol: Integer): Boolean;
begin
  result := GetColNullV(ACol);
end;

function TFDBConnection.GetColString(ACol: Integer): String;
begin
  result := GetColStringV(ACol);
end;

function TFDBConnection.GetColTimestamp(ACol: Integer): TTimestamp;
begin
  result := GetColTimestampV(ACol);
end;

function TFDBConnection.GetColDateTimeEx(ACol: Integer): TFslDateTime;
begin
  result := GetColDateTimeExV(ACol);
end;

function TFDBConnection.GetColType(ACol: Integer): TFDBColumnType;
begin
  result := GetColTypeV(ACol);
end;

function TFDBConnection.GetRowsAffected: Integer;
begin
  result := GetRowsAffectedV;
end;

procedure TFDBConnection.ListTables(AList: TStrings);
begin
  ListTablesV(AList);
end;

procedure TFDBConnection.DropTable(ATableName: String);
begin
  DropTableV(ATableName);
end;

procedure TFDBConnection.DropColumn(ATableName, AColumnName: String);
begin
  DropColumnV(ATableName, AColumnName);
end;

function TFDBConnection.ExistsByKey(const sTableName, sKeyField: String; ikey: Integer): Boolean;
begin
  result := CountSQL('Select '+sKeyField+' from '+sTableName+' where '+sKeyField+' = '+inttostr(iKey)) > 0;
end;

procedure TFDBConnection.BindDateTimeEx(AParamName: String; AParamValue: TFslDateTime);
begin
  BindDateTimeExV(aParamName, AParamValue);
end;

function TFDBConnection.DatabaseSize : int64;
Begin
  result := DatabaseSizeV;
End;

Function TFDBConnection.TableSize(sName : String):int64;
Begin
  result := TableSizeV(sName);
End;

function TFDBConnection.SupportsSizing : Boolean;
Begin
  result := SupportsSizingV;
End;


function TFDBConnection.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FOwner.sizeInBytes);
  inc(result, FBoundItems.sizeInBytes);
  inc(result, (FUsage.length * sizeof(char)) + 12);
  inc(result, FTables.sizeInBytes);
  inc(result, (FSQL.length * sizeof(char)) + 12);
  inc(result, (FTransactionId.length * sizeof(char)) + 12);
end;

{ TFDBManager }

constructor TFDBManager.Create(AName : String; AMaxConnCount: Integer);
begin
  inherited create;

  FName := AName;
  FMaxConnCount := AMaxConnCount;

  FLock := TFslLock.create;
  FDBLogger := TFDBLogger.create;
  FSemaphore := TSemaphore.Create(nil, 0, $FFFF, '');
  FWaitCreate := false;

  FConnections := TFslList<TFDBConnection>.create;
  FAvail := TFslList<TFDBConnection>.create;
  FInUse := TFslList<TFDBConnection>.create;

  FClosing := false;
  GManagers.AddConnMan(self);
  init;
end;

procedure TFDBManager.init;
begin
end;

destructor TFDBManager.Destroy;
begin
  FClosing := true;
  if GManagers <> nil then
    GManagers.RemoveConnMan(self);

  FAvail.free;
  FInUse.free;
  FConnections.Free;
  FSemaphore.free;
  FDBLogger.free;
  FLock.Free;
  inherited;
end;

function TFDBManager.GetCurrentCount: Integer;
begin
  FLock.Enter;
  try
    result := FConnections.Count;
  finally
    FLock.Leave;
  end;
end;

procedure TFDBManager.CheckWait;
begin
  if FWaitCreate then
  begin
    FLock.Lock;
    try
      inc(FThreadWaitCount);
    finally
      FLock.Unlock;
    end;
    try
      if FSemaphore.WaitFor(DEFAULT_CONNECTION_WAIT_LENGTH) = wrError then
        raise EDBException.Create('['+Name+'] fdb_manager Wait Failed' {$IFDEF WINDOWS}+' - '+ ErrorAsString(GetLastError){$ENDIF});
    finally
      FLock.Lock;
      try
        dec(FThreadWaitCount);
      finally
        FLock.Unlock;
      end;
    end;
  end;
end;

function TFDBManager.GetConnection(const AUsage: String): TFDBConnection;
var
  LCreateNew: Boolean;
begin
  CheckWait;
  result := PopAvail;
  if not assigned(result) then
    begin
    // there's a small chance that we will get more than the max number of exceptions
    // with this locking strategy. This is a better outcome that other performance options
    LCreateNew := (CurrConnCount < MaxConnCount) or (MaxConnCount = 0);
    if LCreateNew then
      begin
      try
        result := ConnectionFactory;
      except
        on e:exception do
        begin
          FLock.Enter;
          Try
            FServerIsAvailable := False;
            FLastServerError := e.message;
          Finally
            FLock.Leave;
          End;
          recordStack(e);
          raise;
        end;
      end;
      FLock.Enter;
      Try
        FConnections.Add(result);
        FServerIsAvailable := true;
        FLastServerError := '';
      Finally
        FLock.Leave;
      End;

      result.FNoFree := true;
      FLock.Enter;
      try
        FWaitCreate := (FMaxConnCount > 0) and (FConnections.Count = FMaxConnCount);
      finally
        FLock.Leave;
      end;
      if Assigned(FOnChangeConnectionCount) Then
        FOnChangeConnectionCount(self);
      end
    else
      begin
      raise EDBException.create('No Database Connections Available for "'+AUsage+'" (used: '+GetConnSummary+')');
      end;
    end;
  FLock.Enter; // lock this because of debugger
  try
    result.FUsage := AUsage;
    result.FUsed := now;
    result.FRowCount := 0;
    result.FPrepareCount := 0;
    result.Sql := '';
    FInUse.Add(result.Link);
  finally
    FLock.Leave;
  end;
end;

procedure TFDBManager.checkConnection;
var
  conn : TFDBConnection;
begin
  conn := GetConnection('check');
  try
    conn.FetchMetaData.Free;
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;

end;

procedure TFDBManager.Release(AConn : TFDBConnection);
var
  LDispose : boolean;
  LIndex : integer;
  s : String;
begin

  FDBLogger.RecordUsage(AConn.Usage, AConn.FUsed, AConn.FRowCount, AConn.FPrepareCount, nil, '');
  FLock.Enter; // must lock because of the debugger
  try
    LDispose := (FConnections.count > FMaxConnCount) and (FMaxConnCount > 0);
    LIndex := FInUse.IndexOf(AConn);
    FInUse.Delete(LIndex);
    FLastServerGood := now;
    FServerIsAvailable := true;
    s := AConn.FUsage;
    AConn.FUsage := '';
    AConn.FUsed := 0;
    if LDispose then
      begin
      FConnections.Remove(AConn);
      FWaitCreate := (FMaxConnCount > 0) and (FConnections.count = FMaxConnCount);
      end
    else
      begin
      FAvail.Add(AConn.Link);
      try
        if FThreadWaitCount > 0 then
          FSemaphore.Release;
      except
        on e : exception do
          if DebugConsoleMessages then Writeln('Error releasing semaphore for '+s+': '+e.message+' for '+s);
      end;
      end;
  finally
    FLock.Leave;
  end;

  if LDispose then
    begin
    AConn.FNoFree := false;
    try
      AConn.free;
    except
    end;
    if Assigned(FOnChangeConnectionCount) Then
      FOnChangeConnectionCount(self);
    end;
end;

procedure TFDBManager.Error(AConn : TFDBConnection; AException: Exception; AErrMsg : string);
var
  LIndex : integer;
begin
  FDBLogger.RecordUsage(AConn.Usage, AConn.FUsed, AConn.FRowCount, AConn.FPrepareCount, AException, AErrMsg);

  FLock.Enter; // must lock because of the debugger
  try
    LIndex := FInUse.IndexOf(AConn);
    if LIndex > -1 then
      begin
      FInUse.Delete(LIndex);
      end;
    FConnections.Remove(AConn);
    FWaitCreate := (FMaxConnCount > 0) and (FConnections.count > FMaxConnCount div 2);
    if FAvail.Count = 0 then
    begin
      FServerIsAvailable := false;
      FLastServerError := AException.message;
    End;
    try
      if FThreadWaitCount > 0 then
        FSemaphore.Release;
    except
      on e : exception do
        raise EDBException.create('Error (2) releasing semaphore for '+AConn.Usage+': '+e.message+'. please report this error to grahameg@gmail.com (original error = "'+AException.Message+'"');
    end;
  finally
    FLock.Leave;
  end;
  if Assigned(FOnChangeConnectionCount) Then
    FOnChangeConnectionCount(self);
end;

function TFDBManager.GetConnSummary: String;
var
  i : integer;
begin
  result := '';
  FLock.Enter;
  try
    for i := 0 to FInUse.Count - 1 do
      begin
      StringAppend(result, (FInUse[i] as TFDBConnection).FUsage+' '+DescribePeriod(now - (FInUse[i] as TFDBConnection).FUsed)+' ('+(FInUse[i] as TFDBConnection).SQL+')', #13#10);
      end;
    if result <> '' then
      begin
      result := 'InUse '+inttostr(FInUse.Count)+' of '+inttostr(FConnections.count)+': '+result;
      end
    else
      begin
      result := inttostr(FConnections.count)+' Connections Resting';
      end;
  finally
    FLock.Leave;
  end;
end;

function TFDBManager.GetCurrentUse: Integer;
begin
  FLock.Enter;
  try
    result := FInUse.Count;
  finally
    FLock.Leave;
  end;
end;

function TFDBManager.Link: TFDBManager;
begin
  result := TFDBManager(inherited link);
end;

function TFDBManager.PopAvail: TFDBConnection;
begin
  FLock.Enter;
  try
    if FAvail.Count > 0 then
      begin
      result := FAvail[FAvail.count - 1];
      FAvail.Delete(FAvail.count - 1);
      end
    else
      begin
      result := nil;
      end;
  finally
    FLock.Leave;
  end;
end;

procedure TFDBManager.ExecSQL(ASql, AName : String);
var
  LConn : TFDBConnection;
begin
  LConn := GetConnection(AName);
  try
    LConn.ExecSQL(ASql);
    LConn.Release;
  except
    on e:exception do
      begin
      LConn.Error(e);
      recordStack(e);
      raise;
      end;
  end;
end;


function DBManagers : TFDBManagerList;
begin
  result := GManagers;
end;

function DescribeType(AColType: TFDBColumnType): String;
begin
  case AColType of
    ctUnknown:
      begin
      Result := 'Unknown';
      end;
    ctBoolean:
      begin
      Result := 'Boolean';
      end;
    ctInteger:
      begin
      Result := 'Integer';
      end;
    ctNumeric:
      begin
      Result := 'Numeric';
      end;
    ctFloat:
      begin
      Result := 'Float';
      end;
    ctChar:
      begin
      Result := 'Char';
      end;
    ctDateTime:
      begin
      Result := 'DateTime';
      end;
    ctBlob:
      begin
      Result := 'Blob';
      end;
    ctInt64:
      begin
      Result := 'Int64';
      end;
    else
      begin
      Result := ('Unknown columntype?');
      end;
    end;
end;

procedure TFDBManager.SetMaxConnCount(const Value: Integer);
begin
  FLock.Enter;
  try
    FMaxConnCount := Value;
  finally
    FLock.Leave;
  end;
end;

{$IFNDEF FPC}
procedure TFDBManager.connection(usage: String; proc: TFDBConnectionProc);
var
  conn : TFDBConnection;
begin
  conn := GetConnection(usage);
  try
    proc(conn);
    conn.Release;
  except
    on e : exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
end;
{$ENDIF}

function TFDBManager.CountSQL(ASql, AName: String): integer;
var
  LConn : TFDBConnection;
begin
  LConn := GetConnection(AName);
  try
    result := LConn.CountSQL(ASql);
    LConn.Release;
  except
    on e:exception do
      begin
      LConn.Error(e);
      recordStack(e);
      raise;
      end;
  end;
end;

function TFDBManager.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FConnections.sizeInBytes);
  inc(result, FAvail.sizeInBytes);
  inc(result, FInUse.sizeInBytes);
  inc(result, FDBLogger.sizeInBytes);
  inc(result, (FLastServerError.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
end;

{ TFDBManagerList }

constructor TFDBManagerList.create;
begin
  inherited create;
  FLock := TFslLock.create;
  FHooks := TFslList<TFDBHook>.create;
  FList := TList<TFDBManager>.create;
end;

destructor TFDBManagerList.destroy;
begin
  FLock.free;
  FHooks.free;
  FList.Free;
  inherited;
end;

function TFDBManagerList.dump: String;
var
  i : integer;
begin
  result := '';
  for i := 0 to FList.Count - 1 do
    result := result + FList[i].FName+' : '+ FList[i].GetConnSummary+#13#10;
end;

procedure TFDBManagerList.AddConnMan(AConnMan : TFDBManager);
var
  i : integer;
begin
  Lock;
  try
    FList.Add(AConnMan);
    for i := 0 to FHooks.count -1 do
      FHooks[i].FHook(AConnMan, true);
  finally
    Unlock;
  end;
end;

procedure TFDBManagerList.RemoveConnMan(AConnMan : TFDBManager);
var
  i : integer;
begin
  Lock;
  try
    for i := 0 to FHooks.count -1 do
      FHooks[i].FHook(AConnMan, false);
    FList.Remove(AConnMan);
  finally
    Unlock;
  end;
end;

function TFDBManagerList.GetConnMan(i : Integer):TFDBManager;
begin
  result := FList[i];
end;

function TFDBManagerList.HasConnManByName(s : String) : Boolean;
begin
  result := GetConnManByName(s) <> nil;
End;

function TFDBManagerList.GetConnManByName(s : String):TFDBManager;
var
  k : TFDBManager;
begin
  result := nil;
  for k in FList do
    if k.Name = s then
    begin
      result := k;
      exit;
    end;
end;

procedure TFDBManagerList.Lock;
begin
  FLock.Enter;
end;

procedure TFDBManagerList.UnLock;
begin
  FLock.Leave;
end;

procedure TFDBManagerList.RegisterHook(AName : String; AHook : TFDBManagerEvent);
begin
  FHooks.Add(TFDBHook.create(AName, AHook));
end;

procedure TFDBManagerList.UnRegisterHook(AName : String);
var
  i : integer;
begin
  for i := FHooks.Count - 1 downto 0 do
    if FHooks[i].FName = AName then
      FHooks.Delete(i);
end;

function TFDBManagerList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FHooks.sizeInBytes);
end;

{ TFDBHook }

constructor TFDBHook.create(Name : String; Hook : TFDBManagerEvent);
begin
  inherited create;
  FName := name;
  FHook := Hook;
end;

function TFDBHook.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
end;

{ TFDBColumn }

constructor TFDBColumn.Create(name: String);
begin
  inherited create;
  self.Name := name;
end;

function TFDBColumn.Describe : String;
begin
  case FDataType of
    ctBoolean  : result := FName + ' : bool';
    ctInteger  : result := FName + ' : int';
    ctNumeric  : result := FName + ' : numeric';
    ctFloat    : result := FName + ' : float';
    ctChar     : result := FName + ' : char('+inttostr(FLength)+')';
    ctDateTime : result := FName + ' : dateTime';
    ctBlob     : result := FName + ' : Binary';
    ctInt64    : result := FName + ' : int64';
    ctUnicode  : result := FName + ' : nChar('+inttostr(FLength)+')';
  else
    {ctUnknown:} result := FName + ' : unknown';
  end;
  if not FNullable then
    begin
    result := result +' [not Null]';
    end;
end;

function TFDBColumn.Link: TFDBColumn;
begin
  result := TFDBColumn(Inherited Link);
end;

function CommaText(list : TFslList<TFDBColumn>) : String;
var
  s : TStringBuilder;
  b : boolean;
  c : TFDBColumn;
begin
  b := false;
  s := TStringBuilder.Create;
  try
    for C in list do
    begin
      if (b) then
        s.Append(',')
      else
        b := true;
      s.Append(C.Name);
    end;
  finally
    s.Free;
  end;
end;

function TFDBColumn.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
end;

{ TFDBIndex }

constructor TFDBIndex.create;
begin
  inherited;
  FColumns := TFslList<TFDBColumn>.create;
end;

destructor TFDBIndex.destroy;
begin
  FColumns.Free;
  inherited;
end;

function TFDBIndex.Describe : String;
begin
  if FUnique then
    begin
    Result := 'UNIQUE ';
    end
  else
    begin
    Result := '';
    end;


  Result := Result + 'INDEX ' + FName + ' ON (' + CommaText(FColumns) + ')';
end;

function TFDBIndex.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FColumns.sizeInBytes);
end;

function TFDBRelationship.Describe : String;
Begin
  result := FColumn + ' -> '+FDestTable+'.'+FDestColumn;
End;

function TFDBRelationship.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FColumn.length * sizeof(char)) + 12);
  inc(result, (FDestTable.length * sizeof(char)) + 12);
  inc(result, (FDestColumn.length * sizeof(char)) + 12);
end;

{ TFDBTable }

constructor TFDBTable.create;
begin
  inherited;
  FColumns := TFslList<TFDBColumn>.CREATE;
  FIndexes := TFslList<TFDBIndex>.create;
  FRelationships := TFslList<TFDBRelationship>.create;
end;

destructor TFDBTable.destroy;
begin
  FRelationships.Free;
  FColumns.Free;
  FIndexes.Free;
  inherited;
end;

function TFDBTable.hasColumn(name: String): boolean;
var
  c : TFDBColumn;
begin
  result := false;
  for c in FColumns do
    result := result or (c.Name = name);
end;

function TFDBTable.Link: TFDBTable;
begin
  result := TFDBTable(inherited link);
end;

function TFDBTable.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FColumns.sizeInBytes);
  inc(result, FIndexes.sizeInBytes);
  inc(result, FRelationships.sizeInBytes);
  inc(result, (FOwner.length * sizeof(char)) + 12);
  inc(result, (FDescription.length * sizeof(char)) + 12);
end;

{ TFDBMetaData }

constructor TFDBMetaData.create;
begin
  inherited;
  FTables := TFslList<TFDBTable>.create;
  FProcedures := TStringList.create;
end;

destructor TFDBMetaData.destroy;
begin
  FTables.Free;
  FProcedures.Free;
  inherited;
end;


function TFDBMetaData.GetTable(name: String): TFDBTable;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Tables.Count - 1 do
    if Tables[i].Name = name then
      result := Tables[i];
end;

function TFDBMetaData.HasTable(name: String; caseSensitive : boolean = false): boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to Tables.Count - 1 do
    if (caseSensitive and (Tables[i].Name = name)) or (not caseSensitive and (SameText(Tables[i].Name, name))) then
      result := true;
end;


function TFDBMetaData.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FTables.sizeInBytes);
  inc(result, FProcedures.sizeInBytes);
end;

function TFDBManager.ServerErrorStatus: String;
Begin
  FLock.Enter;
  try
    if ServerIsAvailable then
      result := ''
    else
      result := 'Database '+DBDetails+' has been unavailable for '+DescribePeriod(now - FLastServerGood)+'. Last Error = '+FLastServerError;
  Finally
    FLock.Leave;
  End;
End;

procedure CloseUPGManagers;
var
  m : TFDBManagerList;
begin
  m := GManagers;
  GManagers := nil;
  m.free;
end;

initialization
  GManagers := TFDBManagerList.create;
finalization
  CloseUPGManagers;
end.
