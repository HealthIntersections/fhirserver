{! 1 !}
{0.00-041  27 Oct 04 11:15  [21370]  User: Grahame Grieve    relax assertions}

{ Cross platform Database implementation }

{ To implement a Database interface, subclass TBaseKDBConnection overriding
  all the virtual methods, and subclass the TKDBConnectionManager and implement
  functionality as required to set up and manage connections, and a connection
  factories.

  When creating databases, you have the choice of creating the appropriate
  class directly, or using the factory provided in this unit.

  Known implementations:

    Unit         ManagerClass             Description
    odbcconn     TODBCConnectionManager   Standard ODBC Connection class
    ibconn       TIBConnectionManager     Direct Interbase interface
    dbisam       TDBISAMConnManager       DBIsam with auto-recovery
}

unit KDBManager;

// This section used by scripting system

{!Wrapper uses KClasses,Classes,KProcs,KDate,AdvFilers,AdvObjects,AdvIterators, MSSEWrap, MSSEWrap_Wrapper}

// script access at this level is strictly controlled.
// until further notice, connection managers cannot be created directly in scripts.
// instead, you must expose the ability to create and manage connections elsewhere
// portions of this functionality will not be available in the scripting layer

{!ignore TKDBConnection.ExecSQLBatch} // not really supportable in script engine, and not important
{!ignore TConnectionState}
{!ignore TKDBManagerList}
{!ignore TKDBManager}
{!ignore @.KDBManagers}
{!ignore KDB_ALL_PROVIDERS}


interface

uses
  StringSupport,
  kCritSct,
  KSettings,
  KDBLogging,
  Classes,
  Contnrs,
  IdSoapClasses,
  IdSoapDebug,
  IniFiles,
  KDBDialects,
  SysUtils,
  DateAndTime,
  KDate;

{!Script Hide}
const
  DEFAULT_CONNECTION_WAIT_LENGTH = 1000;
  DEFAULT_CONNECTION_REFRESH_PERIOD = 200;
  CONNECTION_UNKNOWN = 0;
  CONNECTION_OK = 1;
  CONNECTION_FAIL = 2;
{!Script Show}

type
  EKDBException = class (Exception);

  {!Script Hide}

  // these are all the known providers. Just because the enumerations are defined doesn't
  // mean that the provider is supported by all 3 of compiler, application, and system
  // access is odbc but settings are done differently
  TKDBProvider = (kdbpUnknown,    kdbpDSN,        kdbpODBC,     kdbpFirebird,    kdbpDBIsam,
                  kdbpDBXpress,   kdbpSoapClient, kdbpMySQL,    kdbpAccess);
                  
  TKDBProviderSet = set of TKDBProvider;

const
  KDB_ALL_PROVIDERS = [Low(TKDBProvider) .. High(TKDBProvider)];

type

  {!Script Show}

  {@enum TKDBColumnType
    Lists possible database Column types
  }
  TKDBColumnType = (ctUnknown, ctBoolean, ctInteger, ctNumeric, ctFloat, ctChar, ctDateTime, ctBlob, ctInt64, ctUnicode);

  {!Script Hide}
  // Meta data
  TKDBTableType = (kdbUser, kdbView, kdbSystem);

  TKDBObject = class (TIdBaseObject);
  TKDBObjectList = class (TIdObjectList)
  private
    function GetObject(Index: integer): TObject;
  public
    property Objects[Index : integer] : TObject read GetObject;
    function Valid(AIndex : Integer) : Boolean;
  end;

  TKDBColumn = class (TKDBObject)
  private
    FName: String;
    FLength: Integer;
    FDataType: TKDBColumnType;
    FNullable: Boolean;
  public
    property Name : String read FName write FName;
    property DataType : TKDBColumnType read FDataType write FDataType;
    property Length : Integer read FLength write FLength;
    property Nullable : Boolean read FNullable write FNullable;
    function Describe : String;
  end;

  TKDBColumns = class (TKDBObjectList)
  private
    function GetColumn(AName : string):TKDBColumn;
  public
    property Column[AName : String]:TKDBColumn read GetColumn;
    function GetByIndex(iIndex : Integer):TKDBColumn;
  end;

  TKDBIndex = class (TKDBObject)
  private
    FUnique: Boolean;
    FName: String;
    FColumns: TStringList;
  public
    constructor create;
    destructor Destroy; override;
    property Name : String read FName write FName;
    property Unique : Boolean read FUnique write FUnique;
    property Columns : TStringList read FColumns;
    function Describe : String;
  end;

  TKDBIndexes = class (TKDBObjectList)
  private
    function GetIndex(AName : string):TKDBIndex;
  public
    property IIndex[AName : String]:TKDBIndex read GetIndex; //II because Index is a reserved word
  end;

  TKDBRelationship = class (TKDBObject)
  private
    FColumn: String;
    FDestTable : String;
    FDestColumn : String;
  public
    Property Column : String read FColumn write FColumn;
    Property DestTable : String read FDestTable write FDestTable;
    Property DestColumn : String read FDestColumn write FDestColumn;
    function Describe : String;
  end;

  TKDBRelationships = class (TKDBObjectList)
  private
    function GetIndex(AName : string):TKDBRelationship;
  public
    property Relationship[AName : String]:TKDBRelationship read GetIndex;
  end;

  TKDBTable = class (TKDBObject)
  private
    FName: String;
    FColumns: TKDBColumns;
    FIndexes: TKDBIndexes;
    FRelationships : TKDBRelationships;
    FTableType: TKDBTableType;
    FOwner: String;
    FDescription: String;
    FOrderMatters : Boolean;

  public
    constructor create;
    destructor Destroy; override;
    property Columns : TKDBColumns read FColumns;
    property Indexes : TKDBIndexes read FIndexes;
    Property Relationships : TKDBRelationships read FRelationships;
    property Name : String read FName write FName;
    property TableType : TKDBTableType read FTableType write FTableType;
    property Owner : String read FOwner write FOwner;
    property Description : String read FDescription write FDescription;
    Property OrderMatters : Boolean read FOrderMatters write FOrderMatters;
  end;

  TKDBTables = class (TKDBObjectList)
  private
    function GetTable(AName : string):TKDBTable;
  public
    property Table[AName : String]:TKDBTable read GetTable; default;
    function GetByIndex(iIndex : Integer):TKDBTable;
  end;

  TKDBMetaData = class (TKDBObject)
  private
    FTables: TKDBTables;
    FProcedures : TStringList;
    FSupportsProcedures : Boolean;
  public
    constructor create;
    destructor Destroy; override;

    property Tables : TKDBTables read FTables;
    property Procedures : TStringList read FProcedures;
    property SupportsProcedures : Boolean read FSupportsProcedures write FSupportsProcedures;

    function HasTable(name : String) : boolean;
  end;

  TKDBManager = class;
  TOnChangeConnectionCount = procedure (oSender : TKDBManager) of Object;

  {!Script Show}

  {@Class TKDBConnection
    Database connection that exposes a SQL based interface to the appropriate database.
    These cannot be created directly - you must use a TDBConnPool.GetConnection call
    to get a connection. The connection must always be returned using
    TDBConnPool.YieldConnection otherwise the connection will leak.
  }
  TKDBConnection = class (TKDBObject)
  Private
    FOwner: TKDBManager;
    FNoFree : Boolean;
    FCachedObjects : array of TObject;
    FBoundItems : TStringList;
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
    function GetTables : TStringList;
    procedure ClearCache; // call this on fetchnext and terminate
    function LookupInternal(ATableName, AKeyField, AKeyValue, AValueField, ADefault: String; bAsString: Boolean): String;
  Protected
    // caching for blobs, for use by concrete implementations
    function GetBlob(ACol: Integer; var VBlob: TMemoryStream): Boolean;
    procedure CacheBlob(ACol: Integer; ABlob: TMemoryStream);
    procedure KeepBoundObj(sName : String; AObj : TObject);

    // worker routines for descendents to override
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
    procedure BindTimeStampV(AParamName: String; AParamValue: KDate.TTimeStamp); virtual; abstract;
    procedure BindBlobV(AParamName: String; AParamValue: TMemoryStream); virtual; abstract;
    procedure BindNullV(AParamName: String); virtual; abstract;
    function GetColCountV: Integer; Virtual; Abstract;
    function GetColStringV(ACol: Word): String; Virtual; Abstract;
    function GetColIntegerV(ACol: Word): Integer; Virtual; Abstract;
    function GetColInt64V(ACol: Word): Int64; Virtual; Abstract;
    function GetColDoubleV(ACol: Word): Double; Virtual; Abstract;
    function GetColMemoryV(ACol: Word): TMemoryStream; Virtual; Abstract;
    function GetColBlobV(ACol: Word): TBytes; Virtual; Abstract;
    function GetColNullV(ACol: Word): Boolean; Virtual; Abstract;
    function GetColTimestampV(ACol: Word): KDate.TTimestamp; Virtual; Abstract;
    function GetColTypeV(ACol: Word): TKDBColumnType; Virtual; Abstract;
    function GetColKeyV(ACol: Word): Integer; Virtual; Abstract;
    function GetRowsAffectedV: Integer; Virtual; Abstract;
    function FetchMetaDataV : TKDBMetaData; Virtual; Abstract;
    procedure ListTablesV(AList : TStrings); virtual; abstract;
    function CheckConnection : Integer; virtual; abstract;
    function DatabaseSizeV : int64; virtual; abstract;
    Function TableSizeV(sName : String):int64; virtual; abstract;
    function SupportsSizingV : Boolean; virtual; abstract;

  Public
    constructor Create(AOwner: TKDBManager);
    destructor Destroy; Override;

    {@member Prepare
      After setting the SQL content, prepare the statement so Parameter
      Binding and execution can be done. You must call prepare before
      binding and execution.

      If you are using Interbase, you must call terminate after calling
      prepare - use a try finally construct
    }
    procedure Prepare;

    {@member Execute
      Execute the SQL Statement. Will raise an exception if there is a problem
    }
    procedure Execute;

    {@member Terminate
      Clean up. You should call terminate before returning the
      connection to the pool or using it again
    }
    procedure Terminate;


    {!Script Hide}
    property Usage: String Read FUsage Write FUsage;
    property UseStarted : TDateTime read FUsed;
    property Holder: TObject Read FHolder Write FHolder;
    property Tag: Integer Read FTag Write FTag;
    property Owner: TKDBManager Read FOwner;

    // when the application finishes with the connection, it should use one of these to free the connection
    procedure Release;
    procedure Error(AException: Exception; AErrMsg : string = '');

    // run a group of sql statements as a group
    // note: if you avoid binding timestamps and binaries in order to use this
    // routine, the NDM will remove it and make you clean up
    // not supported in scripting
    procedure ExecSQLBatch(ASql: array of String);

    function FetchMetaData : TKDBMetaData;
    {!Script Show}

    // public for scripting engine - usually would be private
    function GetColCount: Integer;
    function GetColString(ACol: Integer): String;
    function GetColInteger(ACol: Integer): Integer;
    function GetColInt64(ACol: Integer): Int64;
    function GetColDouble(ACol: Integer): Double;
    function GetColMemory(ACol: Integer): TMemoryStream;
    function GetColBlob(ACol: Integer): TBytes;
    function GetColNull(ACol: Integer): Boolean;
    function GetColTimestamp(ACol: Integer): KDate.TTimestamp;
    function GetColDateAndTime(ACol: Integer): TDateAndTime;
    function GetColType(ACol: Integer): TKDBColumnType;
    function GetRowsAffected: Integer;

    function GetColStringByName(AName: String): String;
    function GetColMemoryByName(AName: String): TMemoryStream;
    function GetColBlobByName(AName: String): TBytes;
    function GetColIntegerByName(AName: String): Integer;
    function GetColInt64ByName(AName: String): Int64;
    function GetColDoubleByName(AName: String): Double;
    function GetColTimeStampByName(AName: String): KDate.TTimestamp;
    function GetColDateAndTimeByName(AName: String): TDateAndTime;
    function GetColTypeByName(AName: String): TKDBColumnType;
    function GetColNullByName(AName: String): Boolean;

    {!Script Hide}
    function GetColKey(ACol: Integer): Integer;
    function GetColKeyByName(AName: String): Integer;
    {!Script Show}

    {@member ExecSQL
     Execute a single SQL statement (update, insert, etc. Returns the number of rows affected). You can't use this meaningfully with select statements
    }
    function ExecSQL(ASql: String) : integer; overload;
    function ExecSQL(ASql: String; rows : integer) : integer; overload;

    {@member DatabaseSize
      Get the size of the database
    }
    function DatabaseSize : int64;

    {@member TableSize
      Get the size of the specified table
    }
    Function TableSize(sName : String):int64;

    {@member SupportsSizing
      True if the underlying connection supports determining database and table size
    }
    function SupportsSizing : Boolean;

    {@member CountSQL
     Execute a Count SQL Statement and return the value returned.
     Use an SQL Statement of the following form:

       select count(KeyField) from Table where conditions

     The first column of the first set of data is returned as an integer.
     It's also possible to use sql such as

       Select max(KeyField) from Table

    }
    function CountSQL(ASql: String): Cardinal;

    {@member ExistsByKey
      Quickly check whether table sTableName has a record where sKeyField has value iKey. assumes
      that keyField has an appropriate index.
    }
    Function ExistsByKey(Const sTableName, sKeyField : String; ikey : Integer) : Boolean;

    {@member Lookup
      Given a tablename, return the value of ValueField where KeyField
      is the same as keyvalue. the keyfield can be a number or a string.
      if there is no match, Default will be returned as the value
    }
    function Lookup(ATableName, AKeyField, AKeyValue, AValueField, ADefault: String): String;

    {@member LookupString
      Given a tablename, return the value of ValueField where KeyField
      is the same as keyvalue. the keyfield can be a number or a string.
      if there is no match, Default will be returned as the value

      The SQL will always be generated as a string - avoid cast errors from SQL
    }
    function LookupString(ATableName, AKeyField, AKeyValue, AValueField, ADefault: String): String;

    {@member RenameTable
      Rename a table in the database. Each platform generally provides a
      specific mechanism to do this, but the implementation varies widely.
      this works on all supported platforms.

      Renaming may fail to due to relational or other constraints. In this
      case an exception will usually be raised (provider platform dependent)
    }
    procedure RenameTable(AOldTableName, ANewTableName: String);

    {@member DropTable
      Drop a table from the database. on most platforms this equates to
      executing the SQL statement Drop Table "tablename"
    }
    procedure DropTable(ATableName : String);

    {@member DropColumn
      Drop a column from a table in the database.
    }
    procedure DropColumn(ATableName, AColumnName: String);

    {@member RenameColumn
      Rename a Column in a table. Each platform generally provides a
      specific mechanism to do this, but the implementation varies widely.
      this works on all supported platforms.

      Renaming may fail to due to relational or other constraints. In this
      case an exception will usually be raised (provider platform dependent)
    }
    procedure RenameColumn(ATableName, AOldColumnName, ANewColumnName: String; AColumnDetails: String = '');

    {!Script hide}
    procedure ListTables(AList : TStrings);
    {!Script Show}

    {@member Tables
      A List of all the tables in the database
    }
    property Tables : TStringList Read GetTables;

    {@member ClearDatabase
      Completely drop non system content in database. For obvious reasons, this
      needs to be used with considerable care. Also it should be used before
      anything else has been done with the database.
    }
    procedure ClearDatabase;

    {@member StartTransact
      Start a transaction. Close with Commit or RollBack.
    }
    procedure StartTransact;

    {@member Commit
      Close a transaction and commit changes to the database.
      (note: Multi-stage commits & Rollbacks  are not supported)
    }
    procedure Commit;

    {@member Rollback
      Abandon transaction (no changes to the database).
      (note: Multi-stage commits & Rollbacks are not supported)
    }
    procedure Rollback;

    property InTransaction : Boolean read FInTransaction;

    {@member Fetchnext
       Iterate through the recordset after execute has been called.
       You must call Fetchnext before the the first record. Fetchnext
       will return false once there is no more records to retrieve.

       Forward cursors only
    }
    function FetchNext: Boolean;

    {@member ColByName
      Determine the index of a column by it's name
    }
    function ColByName(AColName: String): Integer;

    {@member ColName
      Determine the Name of a column by it's index
    }
    function ColName(ACol: Integer): String;


    {@member BindInt64
      Bind an Int64 value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:i)
      this will bind the value i to the parameter.
    }
    procedure BindInt64(AParamName: String; AParamValue: Int64);

    {@member BindInteger
      Bind an Integer value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:i)
      this will bind the value i to the parameter.
    }
    procedure BindInteger(AParamName: String; AParamValue: Integer);
    {!Script Hide}
    procedure BindKey(AParamName: String; AParamValue: Integer);
    {!Script Show}

    {@member BindDouble
      Bind a Double (Float) value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:d)
    }
    procedure BindDouble(AParamName: String; AParamValue: Double);

    {@member BindString
      Bind a String value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:s)
    }
    procedure BindString(AParamName: String; AParamValue: String);

    {@member BindTimeStamp
      Bind a TTimeStamp value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:t)
    }
    procedure BindTimeStamp(AParamName: String; AParamValue: KDate.TTimeStamp);

    {@member BindDateAndTime
      Bind a DateTime value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:t)
    }
    procedure BindDateAndTime(AParamName: String; AParamValue: TDateAndTime);

    {!Script Hide}
    {@member BindBlob
      Bind a Binary value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:t)
      use this for Blob fields

      Note that BindBinary works differently to the other bind calls.
      It is the caller's responsibility to make sure that the memory
      pointed to in the binary does not change until after execute is
      called.
    }
    procedure BindBlob(AParamName: String; AParamValue: TMemoryStream);
    {!Script Show}

    {@member BindBlobFromString
      Bind a Binary value to a named parameter. But present a string for binding
      You can call this after using an SQL statement like this:
        insert into table (field) values (:t)
      use this for Blob fields
    }
    procedure BindBlobFromString(AParamName: String; AParamValue: String);

    {@member BindBlobFromBytes
      Bind a Binary value to a named parameter. But present a bytes for binding
      You can call this after using an SQL statement like this:
        insert into table (field) values (:t)
      use this for Blob fields
    }
    procedure BindBlobFromBytes(AParamName: String; AParamValue: TBytes);

    {@member BindIntegerFromBoolean
      Bind an integer from a boolean value. Database value will be 1 if true
    }
    procedure BindIntegerFromBoolean(AParamName: String; AParamValue: Boolean);

    {@member BindNull
      Bind the Null value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:t)
    }
    procedure BindNull(AParamName: String);

    {!Script Hide}
    property ColKey       [ACol: Integer]: Integer Read GetColKey;
    {!Script Show}

    {@member ColType
     Get Column Col Field Type
    }
    property ColType      [ACol: Integer]: TKDBColumnType Read GetColType;
    {@member ColNull
     True if Column ACol(index) Value is Null
    }
    property ColNull      [ACol: Integer]: Boolean Read GetColNull;
    {@member ColString
     Get true if Column ACol(index) is null
    }
    property ColString    [ACol: Integer]: String Read GetColString;
    {@member ColInteger
    Get Column ACol(index) as an Integer
    }
    property ColInteger   [ACol: Integer]: Integer Read GetColInteger;
    {@member ColInt64
    Get Column ACol(index) as a Int64
    }
    property ColInt64     [ACol: Integer]: Int64 Read GetColInt64;
    {@member ColDouble
    Get Column ACol(index) as a Double (Float)
    }
    property ColDouble    [ACol: Integer]: Double Read GetColDouble;
    {@member ColMemory
    Get Column ACol(index) as a blob
    }
    property ColMemory    [ACol: Integer]: TMemoryStream Read GetColMemory;
    {@member ColMemory
    Get Column ACol(index) as a blob
    }
    property ColBlob    [ACol: Integer]: TBytes Read GetColBlob;
    {@member ColTimestamp
    Get Column ACol(index) as a TTimestamp
    }
    property ColTimestamp [ACol: Integer]: KDate.TTimestamp Read GetColTimestamp;

    {@member ColDateAndTime
    Get Column ACol(index) as a DateAndTime
    }
    property ColDateAndTime [ACol: Integer]: TDateAndTime Read GetColDateAndTime;

    {!Script Hide}
    property ColKeyByName       [AName: String]: Integer Read GetColKeyByName;
    {!Script Show}

    {@member ColTypeByName
      Get Column "AName" Field Type}
    property ColTypeByName      [AName: String]: TKDBColumnType Read GetColTypeByName;
    {@member ColNullByName
      true if Column "AName" value is Null}
    property ColNullByName      [AName: String]: Boolean Read GetColNullByName;
    {@member ColStringByName
      Get Column "AName" as a String}
    property ColStringByName    [AName: String]: String Read GetColStringByName;
    {@member ColIntegerByName
      Get Column "AName" as an integer}
    property ColIntegerByName   [AName: String]: Integer Read GetColIntegerByName;
    {@member ColInt64ByName
      Get Column "AName" as a Int64}
    property ColInt64ByName     [AName: String]: Int64 Read GetColInt64ByName;
    {@member ColDoubleByName
      Get Column "AName" as a Double (Float)}
    property ColDoubleByName    [AName: String]: Double Read GetColDoubleByName;
    {@member ColMemoryByName
      Get Column "AName" as a Blob}
    property ColMemoryByName    [AName: String]: TMemoryStream Read GetColMemoryByName;
    {@member ColMemoryByName
      Get Column "AName" as a Blob}
    property ColBlobByName    [AName: String]: TBytes Read GetColBlobByName;
    {@member ColTimeStampByName
      Get Column "AName" as a TTimeStamp}
    property ColTimeStampByName [AName: String]: KDate.TTimeStamp Read GetColTimeStampByName;
    {@member ColDateAndTimeByName
      Get Column "AName" as a TDateAndTime}
    property ColDateAndTimeByName [AName: String]: TDateAndTime Read GetColDateAndTimeByName;
    {@member ColCount
      Number of columns in current result set
    }
    property ColCount: Integer Read GetColCount;

    {@member RowsAffected
      The number of rows affected by an insert, update or delete.
      not valid after a select
    }
    property RowsAffected: Integer Read GetRowsAffected;
  published
    {@member SQL
      The SQL string to execute
    }
    property SQL: String Read FSQl Write FSql;

  end;

  TKDBManager = class(TKDBObject)
  Private
    FSemaphore: TSemaphore;
    FWaitCreate : boolean;
    FAvail: TObjectList;
    FInUse : TObjectList;
    FDBLogger : TKDBLogger;
    FClosing : boolean;
    FOnChangeConnectionCount : TOnChangeConnectionCount;
    FServerIsAvailable : Boolean;
    FLastServerGood : TDateTime;
    FLastServerError : String;

    FMaxConnCount : Integer;
    FCurrConnCount : Integer;
    FName : string;
    FTag : integer;
    function PopAvail : TKDBConnection;
    function GetCurrentCount: Integer;
    procedure Release(AConn : TKDBConnection);
    procedure Error(AConn : TKDBConnection; AException: Exception; AErrMsg : string);
    function GetCurrentUse: Integer;
    procedure SetMaxConnCount(const Value: Integer);
  Protected
    FLock : TIdCriticalSection;

    function ConnectionFactory: TKDBConnection; Virtual; Abstract;
    function GetDBPlatform: TKDBPlatform; Virtual; Abstract;
    function GetDBProvider: TKDBProvider; Virtual; Abstract;
    function GetDBDetails: String; Virtual; Abstract;
  Public
    constructor Create(AName : String; AMaxConnCount: Integer); overload;
    constructor create(AName : String; ASettings : TSettingsAdapter; AIdent : String = ''); overload; virtual; abstract;
    destructor Destroy; Override;

    procedure ExecSQL(ASql, AName : String);
    function GetConnection(const AUsage: String): TKDBConnection;
    procedure SaveSettings(ASettings : TSettingsAdapter); virtual; abstract;
    function CheckConnection : Integer;

    property MaxConnCount : Integer Read FMaxConnCount write SetMaxConnCount;
    property CurrConnCount: Integer Read GetCurrentCount;
    property CurrUseCount : Integer read GetCurrentUse;

    property Logger : TKDBLogger read FDBLogger;
    property Platform: TKDBPlatform read GetDBPlatform;
    property Provider : TKDBProvider read GetDBProvider;
    property DBDetails: String read GetDBDetails;
    function GetConnSummary : String;
    property Tag : integer read FTag write FTag;

    Property ServerIsAvailable : Boolean read FServerIsAvailable;
    Function ServerErrorStatus : String;
    property Name : string read FName;

    property OnChangeConnectionCount : TOnChangeConnectionCount Read FOnChangeConnectionCount Write FOnChangeConnectionCount;
    class function IsSupportAvailable(APlatform : TKDBPlatform; Var VMsg : String):Boolean; virtual; abstract;
  end;

  TKDBManagerClass = class of TKDBManager;

  TKDBManagerEvent = procedure (AConnMan : TKDBManager; ABeingCreated : Boolean) of object;

  TKDBManagerList = class (TIdStringList)
  private
    FLock : TIdCriticalSection;
    FHooks : TStringList;
    procedure AddConnMan(AConnMan : TKDBManager);
    procedure RemoveConnMan(AConnMan : TKDBManager);
    function GetConnMan(i : Integer):TKDBManager;
    function GetConnManByName(s : String):TKDBManager;
  public
    constructor create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
    property ConnMan[i : Integer]:TKDBManager read GetConnMan;
    property ConnManByName[s : String]:TKDBManager read GetConnManByName; default;
    function HasConnManByName(s : String) : Boolean;
    procedure RegisterHook(AName : String; AHook : TKDBManagerEvent);
    procedure UnRegisterHook(AName : String);
    function dump : String;
  end;

{@routine DescribeType
   Get a string Description of a given column type
}
function DescribeType(AColType: TKDBColumnType): String;

function KDBManagers : TKDBManagerList;

implementation

uses
  AdvFactories,
  Windows,
  ThreadSupport,
  ErrorSupport;

const
  ASSERT_UNIT = 'KDBManager';

  KDB_COLUMN_TYPE_NAMES : Array [TKDBColumnType] of String =
            ('ctUnknown', 'ctBoolean', 'ctInteger', 'ctNumeric', 'ctFloat', 'ctChar', 'ctDateTime', 'ctBlob', 'ctInt64', 'ctUnicode');

  KDB_TABLE_TYPE_NAMES : Array [TKDBTableType] of String =
            ('kdbUser', 'kdbView', 'kdbSystem');


var
  GManagers : TKDBManagerList = nil;

{ TKDBConnection }

constructor TKDBConnection.Create(AOwner: TKDBManager);
var
  i : integer;
begin
  inherited create;
  FNoFree := false;
  FOwner := AOwner;
  FUsage := '';
  FHolder := nil;
  FTag := 0;
  FSQL := '';
  FTerminated := true;
  SetLength(FCachedObjects, 20); //small but rearely exceeded
  for i := Low(FCachedObjects) to High(FCachedObjects) do
    begin
    FCachedObjects[i] := nil;
    end;
  FInTransaction := false;
  FBoundItems := TIdStringList.create(true);
  FBoundItems.sorted := true;
  FBoundItems.Duplicates := dupError;
  FTables := TIdStringList.create(false);
end;

destructor TKDBConnection.Destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.Destroy';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  assert(not FNoFree or FOwner.FClosing, ASSERT_LOCATION+': invalid state - must destroy through Connection Manager');
  ClearCache;
  FBoundItems.free;
  FTables.free;
  inherited;
end;

procedure TKDBConnection.BindBlobFromString(AParamName, AParamValue: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.BindBinaryFromString';
var
  LMem: TMemoryStream;
  b : TBytes;
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  assert(AParamName <> '', ASSERT_LOCATION+': name is not valid');

  b := TEncoding.UTF8.GetBytes(AParamValue);
  LMem := TIdMemoryStream.Create;
  KeepBoundObj(AParamName, lMem);
  if AParamValue <> '' then
    begin
    LMem.Write(b[0], length(b));
    LMem.position := 0;
    end;
  BindBlob(AParamName, LMem);
end;

procedure TKDBConnection.BindBlobFromBytes(AParamName : String; AParamValue: TBytes);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.BindBinaryFromString';
var
  LMem: TMemoryStream;
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  assert(AParamName <> '', ASSERT_LOCATION+': name is not valid');

  LMem := TIdMemoryStream.Create;
  KeepBoundObj(AParamName, lMem);
  if Length(AParamValue) > 0 then
    begin
    LMem.Write(AParamValue[0], length(AParamValue));
    LMem.position := 0;
    end;
  BindBlob(AParamName, LMem);
end;

procedure TKDBConnection.BindIntegerFromBoolean(AParamName: String; AParamValue: Boolean);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.BindIntegerFromBoolean';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  assert(AParamName <> '', ASSERT_LOCATION+': name is not valid');

  if AParamValue then
    begin
    BindInteger(AParamName, 1);
    end
  else
    begin
    BindInteger(AParamName, 0);
    end;
end;

procedure TKDBConnection.CacheBlob(ACol: Integer; ABlob: TMemoryStream);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.CacheBlob';
var
  i, j: integer;
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  assert(ACol > 0, ASSERT_LOCATION+': Col "'+inttostr(ACol)+'" is not valid');
  assert(Assigned(ABlob), ASSERT_LOCATION+': Blob is not valid');
  if ACol >= length(FCachedObjects) then
    begin
    i := length(FCachedObjects);
    SetLength(FCachedObjects, ACol+4);
    for j := i to High(FCachedObjects) do
      begin
      FCachedObjects[j] := nil;
      end;
    end;
  if FCachedObjects[ACol] <> nil then
    begin
    FCachedObjects[ACol].free;
    end;
  FCachedObjects[ACol] := ABlob;
end;

function TKDBConnection.CountSQL(ASql: String): Cardinal;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.CountSQL';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  assert(ASql <> '', ASSERT_LOCATION+': self is not valid');

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

procedure TKDBConnection.Error(AException: Exception; AErrMsg : string ='');
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.Error';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  assert(FOwner.TestValid(TKDBManager), ASSERT_LOCATION+': owner is not valid');
  FOwner.Error(self, AException, AErrMsg);
end;

function TKDBConnection.ExecSQL(ASql: String; rows : integer) : integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.ExecSQL.1';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  assert(ASql <> '', ASSERT_LOCATION+': sql statement is missing');
  assert(not SQLHasResultSet(ASql), ASSERT_LOCATION+': sql would have results');

  FSQL := ASql;
  Prepare;
  try
    Execute;
    result := GetRowsAffected;
    if (result <> rows) then
      raise Exception.Create('Error running sql - wrong row count (expected '+inttostr(rows)+', affected '+inttostr(result)+' for sql '+asql+')');
  finally
    Terminate;
    end;
end;

Function TKDBConnection.ExecSQL(ASql: String) : integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.ExecSQL';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  assert(ASql <> '', ASSERT_LOCATION+': sql statement is missing');
  assert(not SQLHasResultSet(ASql), ASSERT_LOCATION+': sql would have results');

  FSQL := ASql;
  Prepare;
  try
    Execute;
    result := GetRowsAffected;
  finally
    Terminate;
    end;
end;

procedure TKDBConnection.ExecSQLBatch(ASql: array of String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.ExecSQLBatch';
var
  i: Integer;
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');

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
      Rollback;
      raise;
    end;
    end;
end;

function TKDBConnection.GetBlob(ACol: Integer; var VBlob: TMemoryStream): Boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.GetBlob';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  assert(ACol > 0, 'ACol < 1 in TBaseKDBConnManager.');
  Result := (ACol < Length(FCachedObjects)) and assigned(FCachedObjects[ACol]);
  if Result then
    begin
    VBlob := FCachedObjects[ACol] as TMemoryStream;
    end;
end;

function TKDBConnection.GetColDoubleByName(AName: String): Double;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.GetColDoubleByName';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  result := GetColDouble(ColByName(AName));
end;

function TKDBConnection.GetColInt64ByName(AName: String): Int64;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.GetColInt64ByName';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  result := GetColInt64(ColByName(AName));
end;

function TKDBConnection.GetColIntegerByName(AName: String): Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.GetColIntegerByName';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  result := GetColInteger(ColByName(AName));
end;

function TKDBConnection.GetColKeyByName(AName: String): Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.GetColKeyByName';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  result := GetColKey(ColByName(AName));
end;

function TKDBConnection.GetColMemoryByName(AName: String): TMemoryStream;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.GetColMemoryByName';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  result := GetColMemory(ColByName(AName));
end;

function TKDBConnection.GetColNullByName(AName: String): Boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.GetColNullByName';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  result := GetColNull(ColByName(AName));
end;

function TKDBConnection.GetColStringByName(AName: String): String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.GetColStringByName';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  result := GetColString(ColByName(AName));
end;

function TKDBConnection.GetColTimeStampByName(AName: String): KDate.TTimestamp;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.GetColTimeStampByName';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  result := GetColTimestamp(ColByName(AName));
end;

function TKDBConnection.GetColDateAndTimeByName(AName: String): TDateAndTime;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.GetColTimeStampByName';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  result := TDateAndTime.Create;
  Result.Timestamp := GetColTimestamp(ColByName(AName));
end;

function TKDBConnection.GetColTypeByName(AName: String): TKDBColumnType;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.GetColTypeByName';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  result := GetColType(ColByName(AName));
end;

procedure TKDBConnection.Release;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.Release';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  assert(FOwner.TestValid(TKDBManager), ASSERT_LOCATION+': owner is not valid');
  FOwner.Release(self);
end;

procedure TKDBConnection.ClearCache;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.ClearCache';
var
  i : integer;
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  for i := low(FCachedObjects) to High(FCachedObjects) do
    begin
    FCachedObjects[i].free;
    FCachedObjects[i] := nil;
    end;
end;

function TKDBConnection.Lookup(ATableName, AKeyField, AKeyValue, AValueField, ADefault: String): String;
begin
  result := LookupInternal(ATableName, AKeyField, AKeyValue, AValueField, ADefault, False);
end;

function TKDBConnection.LookupString(ATableName, AKeyField, AKeyValue, AValueField, ADefault: String): String;
begin
  result := LookupInternal(ATableName, AKeyField, AKeyValue, AValueField, ADefault, True);
end;

function TKDBConnection.LookupInternal(ATableName, AKeyField, AKeyValue, AValueField, ADefault: String; bAsString : Boolean): String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.Lookup';
var
  FVal : Double;
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  assert(ATableName <> '', ASSERT_LOCATION+': Table Name is not valid');
  assert(AValueField <> '', ASSERT_LOCATION+': Value Field is not valid');

  if StringIsInteger32(AKeyValue) and not bAsString then
    FSQL := 'Select ' + AValueField + ' from ' + ATableName + ' where ' + AKeyField + ' = ' + AKeyValue
  else
    FSQL := 'Select ' + AValueField + ' from ' + ATableName + ' where ' + AKeyField + ' = ''' + AKeyValue + '''';
  Prepare;
  try
    Execute;
    if FetchNext then
      begin
      case ColTypeByName[AValueField] of
        ctUnknown: raise EKDBException.Create('Field type UNKNOWN not supported in a macro lookup');
        ctBoolean: raise EKDBException.Create('Field type Boolean not supported in a macro lookup');
        ctBlob: raise EKDBException.Create('Field type Blob not supported in a macro lookup');
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
          raise EKDBException.Create('Field type unknown in a macro lookup');
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

procedure TKDBConnection.Prepare;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.Prepare';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  FTerminated := false;
  FBoundItems.Clear;
  ClearCache;
  inc(FPrepareCount);
  PrepareV;
end;

function TKDBConnection.FetchNext: Boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.FetchNext';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  inc(FRowCount);
  ClearCache;
  result := FetchNextV;
end;

procedure TKDBConnection.Terminate;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.Terminate';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  FTerminated := true;
  TerminateV;
end;

procedure TKDBConnection.StartTransact;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.StartTransact';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  Assert(not FInTransaction, ASSERT_LOCATION+': Reentrancy of transactions is not supported');
  StartTransactV;
  FInTransaction := true;
end;

procedure TKDBConnection.Commit;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.Commit';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  Assert(FInTransaction, ASSERT_LOCATION+': Commit but not in transaction');
  // order here is important.
  // if the commit fails, then a rollback is required, so we are still in the transaction
  CommitV;
  FInTransaction := False;
end;

procedure TKDBConnection.Rollback;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.Rollback';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  Assert(FInTransaction, ASSERT_LOCATION+': Rollback but not in transaction');
  FInTransaction := False;
  RollbackV;
end;

procedure TKDBConnection.KeepBoundObj(sName : String; AObj : TObject);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.KeepBoundObj';
var
  i : integer;
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  i := FBoundItems.indexof(sName);
  if i <> -1 then
    begin
    FBoundItems.Objects[i].Free;
    FBoundItems.Objects[i] := aObj;
    end
  else
    FBoundItems.AddObject(sName, AObj);
end;

function TKDBConnection.GetTables : TStringList;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.GetTables';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  FTables.Clear;
  ListTables(FTables);
  result := FTables;
end;

procedure TKDBConnection.BindBlob(AParamName: String; AParamValue: TMemoryStream);
begin
  BindBlobV(AParamName, AParamValue);
end;

procedure TKDBConnection.BindDouble(AParamName: String; AParamValue: Double);
begin
  BindDoubleV(AParamName, AParamValue);
end;

procedure TKDBConnection.BindInt64(AParamName: String; AParamValue: Int64);
begin
  BindInt64V(AParamName, AParamValue);
end;

procedure TKDBConnection.BindInteger(AParamName: String; AParamValue: Integer);
begin
  BindIntegerV(AParamName, AParamValue);
end;

procedure TKDBConnection.BindKey(AParamName: String; AParamValue: Integer);
begin
  BindKeyV(AParamName, AParamValue);
end;

procedure TKDBConnection.BindNull(AParamName: String);
begin
  BindNullV(AParamName);
end;

procedure TKDBConnection.BindString(AParamName, AParamValue: String);
begin
  BindStringV(AParamName, AParamValue);
end;

procedure TKDBConnection.BindTimeStamp(AParamName: String; AParamValue: TTimeStamp);
begin
  BindTimeStampV(AParamName, AParamValue);
end;

procedure TKDBConnection.ClearDatabase;
begin
  ClearDatabaseV;
end;

function TKDBConnection.ColByName(AColName: String): Integer;
begin
  result := ColByNameV(AColName);
end;

function TKDBConnection.ColName(ACol: Integer): String;
begin
  result := ColNameV(ACol);
end;

procedure TKDBConnection.Execute;
begin
  ExecuteV;
end;

procedure TKDBConnection.RenameColumn(ATableName, AOldColumnName, ANewColumnName, AColumnDetails: String);
begin
  RenameColumnV(ATableName, AOldColumnName, ANewColumnName, AColumnDetails);
end;

procedure TKDBConnection.RenameTable(AOldTableName, ANewTableName: String);
begin
  RenameTableV(AoldTableName, ANewTableName);
end;

function TKDBConnection.FetchMetaData: TKDBMetaData;
begin
  result := FetchMetaDataV;
end;

function TKDBConnection.GetColBlob(ACol: Integer): TBytes;
begin
  result := GetColBlobV(ACol);
end;

function TKDBConnection.GetColBlobByName(AName: String): TBytes;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBConnection.GetColBlobByName';
begin
  assert(self.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  result := GetColBlob(ColByName(AName));
end;

function TKDBConnection.GetColCount: Integer;
begin
  result := GetColCountV;
end;

function TKDBConnection.GetColDouble(ACol: Integer): Double;
begin
  result := GetColDoubleV(ACol);
end;

function TKDBConnection.GetColInt64(ACol: Integer): Int64;
begin
  result := GetColInt64V(ACol);
end;

function TKDBConnection.GetColInteger(ACol: Integer): Integer;
begin
  result := GetColIntegerV(ACol);
end;

function TKDBConnection.GetColKey(ACol: Integer): Integer;
begin
  result := GetColKeyV(ACol);
end;

function TKDBConnection.GetColMemory(ACol: Integer): TMemoryStream;
begin
  result := GetColMemoryV(ACol);
end;

function TKDBConnection.GetColNull(ACol: Integer): Boolean;
begin
  result := GetColNullV(ACol);
end;

function TKDBConnection.GetColString(ACol: Integer): String;
begin
  result := GetColStringV(ACol);
end;

function TKDBConnection.GetColTimestamp(ACol: Integer): TTimestamp;
begin
  result := GetColTimestampV(ACol);
end;

function TKDBConnection.GetColDateAndTime(ACol: Integer): TDateAndTime;
begin
  result := TDateAndTime.Create;
  result.TimeStamp := GetColTimestampV(ACol);
end;

function TKDBConnection.GetColType(ACol: Integer): TKDBColumnType;
begin
  result := GetColTypeV(ACol);
end;

function TKDBConnection.GetRowsAffected: Integer;
begin
  result := GetRowsAffectedV;
end;

procedure TKDBConnection.ListTables(AList: TStrings);
begin
  ListTablesV(AList);
end;

procedure TKDBConnection.DropTable(ATableName: String);
begin
  DropTableV(ATableName);
end;

procedure TKDBConnection.DropColumn(ATableName, AColumnName: String);
begin
  DropColumnV(ATableName, AColumnName);
end;

function TKDBConnection.ExistsByKey(const sTableName, sKeyField: String; ikey: Integer): Boolean;
begin
  result := CountSQL('Select '+sKeyField+' from '+sTableName+' where '+sKeyField+' = '+inttostr(iKey)) > 0;

end;

procedure TKDBConnection.BindDateAndTime(AParamName: String; AParamValue: TDateAndTime);
begin
  BindTimeStampV(aParamName, AParamValue.TimeStamp);
end;

function TKDBConnection.DatabaseSize : int64;
Begin
  result := DatabaseSizeV;
End;

Function TKDBConnection.TableSize(sName : String):int64;
Begin
  result := TableSizeV(sName);
End;

function TKDBConnection.SupportsSizing : Boolean;
Begin
  result := SupportsSizingV;
End;


{ TKDBManager }

constructor TKDBManager.Create(AName : String; AMaxConnCount: Integer);
begin
  inherited create;

  FName := AName;
  FMaxConnCount := AMaxConnCount;
  FCurrConnCount := 0;

  FLock := TIdCriticalSection.create;
  FDBLogger := TKDBLogger.create;
  FSemaphore := TSemaphore.Create(FCurrConnCount);
  FWaitCreate := false;

  FAvail := TObjectList.create(false);
  FInUse := TObjectList.create(false);

  FClosing := false;
  GManagers.AddConnMan(self);
end;

destructor TKDBManager.Destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManager.Destroy';
begin
  assert(self.TestValid(TKDBManager), ASSERT_LOCATION+': self is not valid');

  FClosing := true;
  GManagers.RemoveConnMan(self);

  Assert(FInUse.Count = 0, 'Connections are still in use in Database Pool '+FName+': '+GetConnSummary);

  // too bad for anyone still trying to use us...
  FAvail.OwnsObjects := true;
  FInUse.OwnsObjects := true;
  FAvail.free;
  FInUse.free;
  FSemaphore.free;
  FDBLogger.free;
  FLock.Free;
  inherited;
end;

function TKDBManager.GetCurrentCount: Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManager.GetCurrentCount';
begin
  assert(self.TestValid(TKDBManager), ASSERT_LOCATION+': self is not valid');
  FLock.Enter;
  try
    result := FCurrConnCount;
  finally
    FLock.Leave;
  end;
end;

function TKDBManager.GetConnection(const AUsage: String): TKDBConnection;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManager.GetConnection';
var
  LCreateNew: Boolean;
begin
  assert(self.TestValid(TKDBManager), ASSERT_LOCATION+': self is not valid');
  assert(AUsage <> '', ASSERT_LOCATION+': Usage is not valid');
  if FWaitCreate and (FSemaphore.Wait(DEFAULT_CONNECTION_WAIT_LENGTH) = wrError) then
    begin
    raise EKDBException.Create('['+Name+'] KDBManager Wait Failed - ' + ErrorAsString(GetLastError));
    end;
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
          raise;
        end;
      end;
      FLock.Enter;
      Try
        FServerIsAvailable := true;
        FLastServerError := '';
      Finally
        FLock.Leave;
      End;

      result.FNoFree := true;
      FLock.Enter;
      try
        inc(FCurrConnCount);
        FWaitCreate := (FMaxConnCount > 0) and (FCurrConnCount > FMaxConnCount div 2);
      finally
        FLock.Leave;
      end;
      if Assigned(FOnChangeConnectionCount) Then
        FOnChangeConnectionCount(self);
      end
    else
      begin
      raise EKDBException.create('No Database Connections Available for "'+AUsage+'" (used: '+GetConnSummary+')');
      end;
    end;
  FLock.Enter; // lock this because of debugger
  try
    result.FUsage := AUsage;
    result.FUsed := now;
    result.FRowCount := 0;
    result.FPrepareCount := 0;
    result.Sql := '';
    FInUse.Add(result);
  finally
    FLock.Leave;
  end;
end;

procedure TKDBManager.Release(AConn : TKDBConnection);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManager.Release';
var
  LDispose : boolean;
  LIndex : integer;
begin
  assert(self.TestValid(TKDBManager), ASSERT_LOCATION+': self is not valid');
  assert(AConn.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');
  assert(AConn.FTerminated, ASSERT_LOCATION+': Connection has not been terminated');

  FDBLogger.RecordUsage(AConn.Usage, AConn.FUsed, AConn.FRowCount, AConn.FPrepareCount, nil, '');
  FLock.Enter; // must lock because of the debugger
  try
    LDispose := (FCurrConnCount > FMaxConnCount) and (FMaxConnCount > 0);
    LIndex := FInUse.IndexOf(AConn);
    assert(LIndex > -1, ASSERT_LOCATION+': return Conn "'+AConn.FUsage+'" not recorded as in use');
    FInUse.Delete(LIndex);
    FLastServerGood := now;
    FServerIsAvailable := true;
    AConn.FUsage := '';
    AConn.FUsed := 0;
    if LDispose then
      begin
      dec(FCurrConnCount);
      FWaitCreate := (FMaxConnCount > 0) and (FCurrConnCount > FMaxConnCount div 2);
      end
    else
      begin
      FAvail.Add(AConn);
      FSemaphore.Release;
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

procedure TKDBManager.Error(AConn : TKDBConnection; AException: Exception; AErrMsg : string);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManager.Error';
var
  LIndex : integer;
begin
  assert(self.TestValid(TKDBManager), ASSERT_LOCATION+': self is not valid');
  assert(AConn.TestValid(TKDBConnection), ASSERT_LOCATION+': self is not valid');

  FDBLogger.RecordUsage(AConn.Usage, AConn.FUsed, AConn.FRowCount, AConn.FPrepareCount, AException, AErrMsg);

  FLock.Enter; // must lock because of the debugger
  try
    LIndex := FInUse.IndexOf(AConn);
    if LIndex > -1 then
      begin
      FInUse.Delete(LIndex);
      end;
    dec(FCurrConnCount);
    FWaitCreate := (FMaxConnCount > 0) and (FCurrConnCount > FMaxConnCount div 2);
    FSemaphore.Release;
    if FAvail.Count = 0 then
    begin
      FServerIsAvailable := false;
      FLastServerError := AException.message;
    End;
  finally
    FLock.Leave;
  end;

  try
    AConn.FNoFree := false;
    AConn.free;
  except
    // it was already in error, and we are in an error handling block - just suppress anything
  end;
  if Assigned(FOnChangeConnectionCount) Then
    FOnChangeConnectionCount(self);
end;

function TKDBManager.GetConnSummary: String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManager.GetConnSummary:';
var
  i : integer;
begin
  assert(self.TestValid(TKDBManager), ASSERT_LOCATION+': self is not valid');
  result := '';
  FLock.Enter;
  try
    for i := 0 to FInUse.Count - 1 do
      begin
      StringAppend(result, (FInUse[i] as TKDBConnection).FUsage+' '+DescribePeriod(now - (FInUse[i] as TKDBConnection).FUsed)+' ('+(FInUse[i] as TKDBConnection).SQL+')', ',');
      end;
    if result <> '' then
      begin
      result := 'InUse '+inttostr(FInUse.Count)+' of '+inttostr(FCurrConnCount)+': '+result;
      end
    else
      begin
      result := inttostr(FCurrConnCount)+' Connections Resting';
      end;
  finally
    FLock.Leave;
  end;
end;

function TKDBManager.GetCurrentUse: Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManager.GetCurrentUse:';
begin
  assert(self.TestValid(TKDBManager), ASSERT_LOCATION+': self is not valid');
  FLock.Enter;
  try
    result := FInUse.Count;
  finally
    FLock.Leave;
  end;
end;

function TKDBManager.PopAvail: TKDBConnection;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManager.PopAvail:';
begin
  assert(self.TestValid(TKDBManager), ASSERT_LOCATION+': self is not valid');
  FLock.Enter;
  try
    if FAvail.Count > 0 then
      begin
      result := FAvail[FAvail.count - 1] as TKDBConnection;
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

procedure TKDBManager.ExecSQL(ASql, AName : String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManager.ExecSQL';
var
  LConn : TKDBConnection;
begin
  assert(self.TestValid(TKDBManager), ASSERT_LOCATION+': self is not valid');
  assert(ASql <> '', ASSERT_LOCATION+': sql is not valid');
  assert(AName <> '', ASSERT_LOCATION+': Name is not valid');
  LConn := GetConnection(AName);
  try
    LConn.ExecSQL(ASql);
    LConn.Release;
  except
    on e:exception do
      begin
      LConn.Error(e);
      raise;
      end;
  end;
end;

Function TKDBManager.CheckConnection : Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManager.ExecSQL';
var
  LConn : TKDBConnection;
begin
  assert(self.TestValid(TKDBManager), ASSERT_LOCATION+': self is not valid');
  LConn := GetConnection('CheckConnection');
  try
    Result := LConn.CheckConnection;
    LConn.Release;
  except
    on e:exception do
      begin
      LConn.Error(e);
      Result := CONNECTION_FAIL;
      end;
  end;
end;


function KDBManagers : TKDBManagerList;
begin
  result := GManagers;
end;

function DescribeType(AColType: TKDBColumnType): String;
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

Type
  TKDBHook = class (TKDBObject)
  private
    FHook : TKDBManagerEvent;
  public
    constructor create(AHook : TKDBManagerEvent);
  end;

procedure TKDBManager.SetMaxConnCount(const Value: Integer);
begin
  FLock.Enter;
  try
    FMaxConnCount := Value;
  finally
    FLock.Leave;
  end;
end;

{ TKDBManagerList }

constructor TKDBManagerList.create;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManagerList.create';
begin
  inherited create(false);
  FLock := TIdCriticalSection.create;
  FHooks := TIdStringList.create(true);
end;

destructor TKDBManagerList.destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManagerList.destroy';
begin
  assert(self.TestValid(TKDBManagerList), ASSERT_LOCATION+': self is not valid');
  FLock.free;
  FHooks.free;
  inherited;
end;

function TKDBManagerList.dump: String;
var
  i : integer;
begin
  result := '';
  for i := 0 to Count - 1 do
    result := result + Strings[i] + ': ' + TKDBManager(Objects[i]).GetConnSummary+#13#10;
end;

procedure TKDBManagerList.AddConnMan(AConnMan : TKDBManager);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManagerList.AddConnMan';
var
  i : integer;
begin
  assert(self.TestValid(TKDBManagerList), ASSERT_LOCATION+': self is not valid');
  Lock;
  try
    assert(indexOf(AConnMan.Name) = -1, ASSERT_LOCATION+': attempt to add a duplicate ConnMan "'+AConnMan.Name+'"');
    AddObject(AConnMan.Name, AConnMan);
    for i := 0 to FHooks.count -1 do
      begin
      TKDBHook(FHooks.Objects[i]).FHook(AConnMan, true);
      end;
  finally
    Unlock;
  end;
end;

procedure TKDBManagerList.RemoveConnMan(AConnMan : TKDBManager);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManagerList.RemoveConnMan';
var
  i : integer;
begin
  assert(self.TestValid(TKDBManagerList), ASSERT_LOCATION+': self is not valid');
  Lock;
  try
    for i := 0 to FHooks.count -1 do
      begin
      TKDBHook(FHooks.Objects[i]).FHook(AConnMan, false);
      end;
    i := indexOf(AConnMan.Name);
    assert(i > -1, ASSERT_LOCATION+': attempt to add a duplicate ConnMan "'+AConnMan.Name+'"');
    Delete(i);
  finally
    Unlock;
  end;
end;

function TKDBManagerList.GetConnMan(i : Integer):TKDBManager;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManagerList.GetConnMan';
begin
  assert(self.TestValid(TKDBManagerList), ASSERT_LOCATION+': self is not valid');
  assert((i >= 0) and (i < Count), ASSERT_LOCATION+': i is not valid ('+inttostr(i)+'/'+inttostr(Count)+')');
  result := objects[i] as TKDBManager;
end;

function TKDBManagerList.HasConnManByName(s : String) : Boolean;
begin
  result := IndexOf(s) > -1;
End;

function TKDBManagerList.GetConnManByName(s : String):TKDBManager;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManagerList.GetConnManByName';
var
  i : integer;
begin
  assert(self.TestValid(TKDBManagerList), ASSERT_LOCATION+': self is not valid');
  i := IndexOf(s);
  if i = -1 then
    raise exception.create('Unable to find database connection "'+s+'" in '+CommaText);
  result := GetConnMan(i);
end;

procedure TKDBManagerList.Lock;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManagerList.Lock';
begin
  assert(self.TestValid(TKDBManagerList), ASSERT_LOCATION+': self is not valid');
  FLock.Enter;
end;

procedure TKDBManagerList.UnLock;
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManagerList.UnLock';
begin
  assert(self.TestValid(TKDBManagerList), ASSERT_LOCATION+': self is not valid');
  assert(FLock.LockedToMe, ASSERT_LOCATION+': lock is not locked');
  FLock.Leave;
end;

procedure TKDBManagerList.RegisterHook(AName : String; AHook : TKDBManagerEvent);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManagerList.RegisterHook';
begin
  assert(self.TestValid(TKDBManagerList), ASSERT_LOCATION+': self is not valid');
  assert(assigned(AHook), ASSERT_LOCATION+': Hook "'+AName+'" is not assigned');
  assert(FHooks.indexOf(AName) = -1, ASSERT_LOCATION+': attempt to register duplicate hook "'+AName+'"');
  FHooks.AddObject(AName, TKDBHook.create(AHook));
end;

procedure TKDBManagerList.UnRegisterHook(AName : String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TKDBManagerList.UnRegisterHook';
var
  i : integer;
begin
  assert(self.TestValid(TKDBManagerList), ASSERT_LOCATION+': self is not valid');
  i := FHooks.indexOf(AName);
  assert(i > -1, ASSERT_LOCATION+': attempt to unregister unknown hook "'+AName+'"');
  FHooks.Delete(i);
end;

{ TKDBHook }

constructor TKDBHook.create(AHook : TKDBManagerEvent);
begin
  inherited create;
  FHook := AHook;
end;

{ TKDBColumn }

function TKDBColumn.Describe : String;
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

{ TKDBColumns }

function TKDBColumns.GetByIndex(iIndex: Integer): TKDBColumn;
begin
  result := Objects[iIndex] as TKDBColumn;
end;

function TKDBColumns.GetColumn(AName : string):TKDBColumn;
var
  i :  Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if SameText((items[i] as TKDBColumn).Name, AName) then
      begin
      result := items[i] as TKDBColumn;
      exit;
      end;
end;

{ TKDBIndex }

constructor TKDBIndex.create;
begin
  inherited;
  FColumns := TStringList.create;
end;

destructor TKDBIndex.destroy;
begin
  FColumns.Free;
  inherited;
end;

function TKDBIndex.Describe : String;
begin
  if FUnique then
    begin
    Result := 'UNIQUE ';
    end
  else
    begin
    Result := '';
    end;

  Result := Result + 'INDEX ' + FName + ' ON (' + FColumns.CommaText + ')';
end;

function TKDBRelationship.Describe : String;
Begin
  result := FColumn + ' -> '+FDestTable+'.'+FDestColumn;
End;

{ TKDBIndexes }

function TKDBIndexes.GetIndex(AName : string):TKDBIndex;
var
  i :  Integer;
begin
  result := nil;
  for i := 0 to count - 1 do
    if SameText((Items[i] as TKDBIndex).Name, AName) then
      begin
      result := Items[i] as TKDBIndex;
      exit;
      end;
end;

{ TKDBTable }

constructor TKDBTable.create;
begin
  inherited;
  FColumns := TKDBColumns.create(True);
  FIndexes := TKDBIndexes.create(True);
  FRelationships := TKDBRelationships.create(True);
end;

destructor TKDBTable.destroy;
begin
  FRelationships.Free;
  FColumns.Free;
  FIndexes.Free;
  inherited;
end;

{ TKDBTables }

function TKDBTables.GetByIndex(iIndex: Integer): TKDBTable;
begin
  result := Objects[iIndex] as TKDBTable;
end;

function TKDBTables.GetTable(AName : string):TKDBTable;
var
  i :  Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if SameText((Items[i] as TKDBTable).Name, AName) then
      begin
      result := Items[i] as TKDBTable;
      exit;
      end;
end;

{ TKDBMetaData }

constructor TKDBMetaData.create;
begin
  inherited;
  FTables := TKDBTables.create(True);
  FProcedures := TStringList.create;
end;

destructor TKDBMetaData.destroy;
begin
  FTables.Free;
  FProcedures.Free;
  inherited;
end;


function TKDBMetaData.HasTable(name: String): boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to Tables.Count - 1 do
    if Tables.GetByIndex(i).Name = name then
      result := true;
end;

{ TKDBObjectList }

function TKDBObjectList.GetObject(Index: integer): TObject;
begin
  result := items[Index];
end;

function TKDBObjectList.Valid(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) And (AIndex < Count);
end;

function TKDBRelationships.GetIndex(AName : string):TKDBRelationship;
var
  i :  Integer;
begin
  result := nil;
  for i := 0 to count - 1 do
    if SameText((Items[i] as TKDBRelationship).Column, AName) then
      begin
      result := Items[i] as TKDBRelationship;
      exit;
      end;
End;

Function TKDBManager.ServerErrorStatus : String;
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

initialization
  GManagers := TKDBManagerList.create;
finalization
  GManagers.free;
  GManagers := nil;
end.
