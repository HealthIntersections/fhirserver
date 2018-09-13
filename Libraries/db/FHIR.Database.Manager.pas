
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

unit FHIR.Database.Manager;


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
  SysUtils, SyncObjs, Classes, Contnrs, IniFiles, Generics.Collections,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Threads,
  FHIR.Database.Settings,
  FHIR.Database.Logging, FHIR.Database.Dialects;

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
  TKDBProvider = (kdbpUnknown,    kdbpDSN,        kdbpODBC,     kdbpFirebird,    kdbpDBIsam,
                  kdbpDBXpress,   kdbpSoapClient, kdbpMySQL,    kdbpAccess,      kdbpSQLite);

  TKDBProviderSet = set of TKDBProvider;

const
  KDB_ALL_PROVIDERS = [Low(TKDBProvider) .. High(TKDBProvider)];

type


  {
    Lists possible database Column types
  }
  TKDBColumnType = (ctUnknown, ctBoolean, ctInteger, ctNumeric, ctFloat, ctChar, ctDateTime, ctBlob, ctInt64, ctUnicode);

  // Meta data
  TKDBTableType = (kdbUser, kdbView, kdbSystem);

  TKDBColumn = class (TFslObject)
  private
    FName: String;
    FLength: Integer;
    FDataType: TKDBColumnType;
    FNullable: Boolean;
  public
    constructor Create(name : String); overload;
    function Link : TKDBColumn; overload;
    property Name : String read FName write FName;
    property DataType : TKDBColumnType read FDataType write FDataType;
    property Length : Integer read FLength write FLength;
    property Nullable : Boolean read FNullable write FNullable;
    function Describe : String;
  end;

  TKDBIndex = class (TFslObject)
  private
    FUnique: Boolean;
    FName: String;
    FColumns: TFslList<TKDBColumn>;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Name : String read FName write FName;
    property Unique : Boolean read FUnique write FUnique;
    property Columns : TFslList<TKDBColumn> read FColumns;
    function Describe : String;
  end;

  TKDBRelationship = class (TFslObject)
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

  TKDBTable = class (TFslObject)
  private
    FName: String;
    FColumns: TFslList<TKDBColumn>;
    FIndexes: TFslList<TKDBIndex>;
    FRelationships : TFslList<TKDBRelationship>;
    FTableType: TKDBTableType;
    FOwner: String;
    FDescription: String;
    FOrderMatters : Boolean;

  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TKDBTable; overload;
    property Columns : TFslList<TKDBColumn> read FColumns;
    property Indexes : TFslList<TKDBIndex> read FIndexes;
    Property Relationships : TFslList<TKDBRelationship> read FRelationships;
    property Name : String read FName write FName;
    property TableType : TKDBTableType read FTableType write FTableType;
    property Owner : String read FOwner write FOwner;
    property Description : String read FDescription write FDescription;
    Property OrderMatters : Boolean read FOrderMatters write FOrderMatters;

    function hasColumn(name : String) : boolean;
  end;

  TKDBMetaData = class (TFslObject)
  private
    FTables: TFslList<TKDBTable>;
    FProcedures : TStringList;
    FSupportsProcedures : Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Tables : TFslList<TKDBTable> read FTables;
    property Procedures : TStringList read FProcedures;
    property SupportsProcedures : Boolean read FSupportsProcedures write FSupportsProcedures;

    function HasTable(name : String) : boolean;
    function GetTable(name : String) : TKDBTable;
  end;

  TKDBManager = class;
  TOnChangeConnectionCount = procedure (oSender : TKDBManager) of Object;
  TKDBBoundParam = class (TFslObject);


  {
    Database connection that exposes a SQL based interface to the appropriate database.
    These cannot be created directly - you must use a TDBConnPool.GetConnection call
    to get a connection. The connection must always be returned using
    TDBConnPool.YieldConnection otherwise the connection will leak.
  }
  TKDBConnection = class (TFslObject)
  Private
    FOwner: TKDBManager;
    FNoFree : Boolean;
    FBoundItems : TFslMap<TKDBBoundParam>;
    FUsage : String;
    FUsed : TDateTime;
    FTables : TStrings;
    FRowCount : integer;
    FPrepareCount : integer;
    FInTransaction : Boolean;

    // local storage for applications using the connection
    FHolder: TObject;
    FTag: Integer;

    // execution
    FSQL : string;
    FTerminated: Boolean;
    FTransactionId: String;
    function GetTables : TStrings;
    function LookupInternal(ATableName, AKeyField, AKeyValue, AValueField, ADefault: String; bAsString: Boolean): String;
    function GetColBlobAsString(ACol: Integer): String;
    function GetColBlobAsStringByName(AName: String): String;
  Protected
    // caching for blobs, for use by concrete implementations
    procedure KeepBoundObj(sName : String; AObj : TKDBBoundParam);

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
    procedure BindTimeStampV(AParamName: String; AParamValue: FHIR.Support.Utilities.TTimeStamp); virtual; abstract;
    procedure BindDateTimeExV(AParamName: String; AParamValue: TDateTimeEx); virtual; abstract;
    procedure BindBlobV(AParamName: String; AParamValue: TBytes); virtual; abstract;
    procedure BindNullV(AParamName: String); virtual; abstract;
    function GetColCountV: Integer; Virtual; Abstract;
    function GetColStringV(ACol: Word): String; Virtual; Abstract;
    function GetColIntegerV(ACol: Word): Integer; Virtual; Abstract;
    function GetColInt64V(ACol: Word): Int64; Virtual; Abstract;
    function GetColDoubleV(ACol: Word): Double; Virtual; Abstract;
    function GetColBlobV(ACol: Word): TBytes; Virtual; Abstract;
    function GetColNullV(ACol: Word): Boolean; Virtual; Abstract;
    function GetColTimestampV(ACol: Word): FHIR.Support.Utilities.TTimestamp; Virtual; Abstract;
    function GetColDateTimeExV(ACol: Word): TDateTimeEx; Virtual; Abstract;
    function GetColTypeV(ACol: Word): TKDBColumnType; Virtual; Abstract;
    function GetColKeyV(ACol: Word): Integer; Virtual; Abstract;
    function GetRowsAffectedV: Integer; Virtual; Abstract;
    function FetchMetaDataV : TKDBMetaData; Virtual; Abstract;
    procedure ListTablesV(AList : TStrings); virtual; abstract;
    function DatabaseSizeV : int64; virtual; abstract;
    Function TableSizeV(sName : String):int64; virtual; abstract;
    function SupportsSizingV : Boolean; virtual; abstract;

  Public
    constructor Create(AOwner: TKDBManager);
    destructor Destroy; Override;

    function link : TKDBConnection; overload;

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
  
    // public for scripting engine - usually would be private
    function GetColCount: Integer;
    function GetColString(ACol: Integer): String;
    function GetColInteger(ACol: Integer): Integer;
    function GetColInt64(ACol: Integer): Int64;
    function GetColDouble(ACol: Integer): Double;
    function GetColBlob(ACol: Integer): TBytes;
    function GetColNull(ACol: Integer): Boolean;
    function GetColTimestamp(ACol: Integer): FHIR.Support.Utilities.TTimestamp;
    function GetColDateTimeEx(ACol: Integer): TDateTimeEx;
    function GetColType(ACol: Integer): TKDBColumnType;
    function GetRowsAffected: Integer;

    function GetColStringByName(AName: String): String;
    function GetColBlobByName(AName: String): TBytes;
    function GetColIntegerByName(AName: String): Integer;
    function GetColInt64ByName(AName: String): Int64;
    function GetColDoubleByName(AName: String): Double;
    function GetColTimeStampByName(AName: String): FHIR.Support.Utilities.TTimestamp;
    function GetColDateTimeExByName(AName: String): TDateTimeEx;
    function GetColTypeByName(AName: String): TKDBColumnType;
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
    procedure BindTimeStamp(AParamName: String; AParamValue: FHIR.Support.Utilities.TTimeStamp);

    {
      Bind a DateTime value to a named parameter. You can call this
      after using an SQL statement like this:
        insert into table (field) values (:t)
    }
    procedure BindDateTimeEx(AParamName: String; AParamValue: TDateTimeEx);

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
    property ColType      [ACol: Integer]: TKDBColumnType Read GetColType;
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
    property ColTimestamp [ACol: Integer]: FHIR.Support.Utilities.TTimestamp Read GetColTimestamp;

    {
    Get Column ACol(index) as a DateAndTime
    }
    property ColDateTimeEx [ACol: Integer]: TDateTimeEx Read GetColDateTimeEx;

      property ColKeyByName       [AName: String]: Integer Read GetColKeyByName;
  
    {
      Get Column "AName" Field Type}
    property ColTypeByName      [AName: String]: TKDBColumnType Read GetColTypeByName;
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
    property ColTimeStampByName [AName: String]: FHIR.Support.Utilities.TTimeStamp Read GetColTimeStampByName;
    {
      Get Column "AName" as a TDateTimeEx}
    property ColDateTimeExByName [AName: String]: TDateTimeEx Read GetColDateTimeExByName;
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

  TKDBConnectionProc = reference to Procedure (conn : TKDBConnection);

  TKDBManager = class(TFslObject)
  Private
    FSemaphore : TSemaphore;
    FWaitCreate : boolean;
    FConnections : TFslList<TKDBConnection>;
    FAvail: TFslList<TKDBConnection>;
    FInUse : TFslList<TKDBConnection>;
    FDBLogger : TKDBLogger;
    FClosing : boolean;
    FOnChangeConnectionCount : TOnChangeConnectionCount;
    FServerIsAvailable : Boolean;
    FLastServerGood : TDateTime;
    FLastServerError : String;
    FThreadWaitCount : integer;

    FMaxConnCount : Integer;
    FName : string;
    FTag : integer;
    function PopAvail : TKDBConnection;
    function GetCurrentCount: Integer;
    procedure Release(AConn : TKDBConnection);
    procedure Error(AConn : TKDBConnection; AException: Exception; AErrMsg : string);
    function GetCurrentUse: Integer;
    procedure SetMaxConnCount(const Value: Integer);
    procedure CheckWait;
  Protected
    FLock : TFslLock;

    function ConnectionFactory: TKDBConnection; Virtual; Abstract;
    function GetDBPlatform: TKDBPlatform; Virtual; Abstract;
    function GetDBProvider: TKDBProvider; Virtual; Abstract;
    function GetDBDetails: String; Virtual; Abstract;
    function GetDriver: String; Virtual; Abstract;
    procedure init; virtual;
  Public
    constructor Create(AName : String; AMaxConnCount: Integer); overload;
    constructor Create(AName : String; ASettings : TSettingsAdapter; AIdent : String = ''); overload; virtual; abstract;
    destructor Destroy; Override;

    function Link : TKDBManager; overload;

    procedure ExecSQL(ASql, AName : String);
    function GetConnection(const AUsage: String): TKDBConnection;
    procedure connection(usage : String; proc : TKDBConnectionProc);
    procedure SaveSettings(ASettings : TSettingsAdapter); virtual; abstract;

    property MaxConnCount : Integer Read FMaxConnCount write SetMaxConnCount;
    property CurrConnCount: Integer Read GetCurrentCount;
    property CurrUseCount : Integer read GetCurrentUse;

    property Logger : TKDBLogger read FDBLogger;
    property Platform: TKDBPlatform read GetDBPlatform;
    property Provider : TKDBProvider read GetDBProvider;
    property DBDetails: String read GetDBDetails;
    Property Driver : String read GetDriver;
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

  TKDBHook = class (TFslObject)
  private
    FHook : TKDBManagerEvent;
    FName : String;
  public
    constructor Create(Name : String; Hook : TKDBManagerEvent);
  end;

  TKDBManagerList = class (TFslObject)
  private
    FLock : TFslLock;
    FHooks : TFslList<TKDBHook>;
    FList : TList<TKDBManager>;
    procedure AddConnMan(AConnMan : TKDBManager);
    procedure RemoveConnMan(AConnMan : TKDBManager);
    function GetConnMan(i : Integer):TKDBManager;
    function GetConnManByName(s : String):TKDBManager;
  public
    constructor Create; override;
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

{
   Get a string Description of a given column type
}
function DescribeType(AColType: TKDBColumnType): String;

function KDBManagers : TKDBManagerList;

implementation

const
  ASSERT_UNIT = 'FHIR.Database.Manager';

  KDB_COLUMN_TYPE_NAMES : Array [TKDBColumnType] of String =
            ('ctUnknown', 'ctBoolean', 'ctInteger', 'ctNumeric', 'ctFloat', 'ctChar', 'ctDateTime', 'ctBlob', 'ctInt64', 'ctUnicode');

  KDB_TABLE_TYPE_NAMES : Array [TKDBTableType] of String =
            ('kdbUser', 'kdbView', 'kdbSystem');


var
  GManagers : TKDBManagerList = nil;

{ TKDBConnection }

constructor TKDBConnection.Create(AOwner: TKDBManager);
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
  FBoundItems := TFslMap<TKDBBoundParam>.create;
  FTables := TStringList.create;
end;

destructor TKDBConnection.Destroy;
begin
  FBoundItems.free;
  FTables.free;
  inherited;
end;

procedure TKDBConnection.BindBlob(AParamName: String; AParamValue: TBytes);
begin
  BindBlobV(AParamName, AParamValue);
end;

procedure TKDBConnection.BindBlobFromString(AParamName, AParamValue: String);
var
  b : TBytes;
begin
  b := TEncoding.UTF8.GetBytes(AParamValue);
  BindBlob(AParamName, b);
end;

procedure TKDBConnection.BindIntegerFromBoolean(AParamName: String; AParamValue: Boolean);
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

function TKDBConnection.CountSQL(ASql: String): Cardinal;
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

procedure TKDBConnection.Error(AException: Exception; AErrMsg : string ='');
begin
  FOwner.Error(self, AException, AErrMsg);
end;

function TKDBConnection.ExecSQL(ASql: String; rows : integer) : integer;
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

Function TKDBConnection.ExecSQL(ASql: String) : integer;
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

procedure TKDBConnection.ExecSQLBatch(ASql: array of String);
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

function TKDBConnection.GetColDoubleByName(AName: String): Double;
begin
  result := GetColDouble(ColByName(AName));
end;

function TKDBConnection.GetColInt64ByName(AName: String): Int64;
begin
  result := GetColInt64(ColByName(AName));
end;

function TKDBConnection.GetColIntegerByName(AName: String): Integer;
begin
  result := GetColInteger(ColByName(AName));
end;

function TKDBConnection.GetColKeyByName(AName: String): Integer;
begin
  result := GetColKey(ColByName(AName));
end;

function TKDBConnection.GetColNullByName(AName: String): Boolean;
begin
  result := GetColNull(ColByName(AName));
end;

function TKDBConnection.GetColStringByName(AName: String): String;
begin
  result := GetColString(ColByName(AName));
end;

function TKDBConnection.GetColTimeStampByName(AName: String): FHIR.Support.Utilities.TTimestamp;
begin
  result := GetColTimestamp(ColByName(AName));
end;

function TKDBConnection.GetColDateTimeExByName(AName: String): TDateTimeEx;
begin
  result := TDateTimeEx.fromTS(GetColTimestamp(ColByName(AName)));
end;

function TKDBConnection.GetColTypeByName(AName: String): TKDBColumnType;
begin
  result := GetColType(ColByName(AName));
end;

procedure TKDBConnection.Release;
begin
  FOwner.Release(self);
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
var
  FVal : Double;
begin

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

procedure TKDBConnection.Prepare;
begin
  FTerminated := false;
  FBoundItems.Clear;
  inc(FPrepareCount);
  PrepareV;
end;

function TKDBConnection.FetchNext: Boolean;
begin
  inc(FRowCount);
  result := FetchNextV;
end;

procedure TKDBConnection.Terminate;
begin
  FTerminated := true;
  TerminateV;
end;

procedure TKDBConnection.StartTransact;
begin
  StartTransactV;
  FInTransaction := true;
  FTransactionId := NewGuidId;
end;

procedure TKDBConnection.Commit;
begin
  // order here is important.
  // if the commit fails, then a rollback is required, so we are still in the transaction
  CommitV;
  FInTransaction := False;
end;

procedure TKDBConnection.Rollback;
begin
  FInTransaction := False;
  RollbackV;
end;

procedure TKDBConnection.KeepBoundObj(sName : String; AObj : TKDBBoundParam);
begin
  FBoundItems.AddOrSetValue(sName, aObj);
end;

function TKDBConnection.link: TKDBConnection;
begin
  result := TKDBConnection(inherited link);
end;

function TKDBConnection.GetTables : TStrings;
begin
  FTables.Clear;
  ListTables(FTables);
  result := FTables;
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

procedure TKDBConnection.BindStringOrNull(AParamName, AParamValue: String);
begin
  if AParamValue = '' then
    BindNull(aParamName)
  else
    BindString(aParamName, AParamValue);
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

function TKDBConnection.GetColBlobAsString(ACol: Integer): String;
begin
  result := TEncoding.UTF8.GetString(ColBlob[aCol]);
end;

function TKDBConnection.GetColBlobAsStringByName(AName: String): String;
begin
  result := TEncoding.UTF8.GetString(ColBlobByName[AName]);
end;

function TKDBConnection.GetColBlobByName(AName: String): TBytes;
begin
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

function TKDBConnection.GetColDateTimeEx(ACol: Integer): TDateTimeEx;
begin
  result := GetColDateTimeExV(ACol);
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

procedure TKDBConnection.BindDateTimeEx(AParamName: String; AParamValue: TDateTimeEx);
begin
  BindDateTimeExV(aParamName, AParamValue);
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

  FLock := TFslLock.create;
  FDBLogger := TKDBLogger.create;
  FSemaphore := TSemaphore.Create(nil, 0, 4{ $FFFF}, '');
  FWaitCreate := false;

  FConnections := TFslList<TKDBConnection>.create;
  FAvail := TFslList<TKDBConnection>.create;
  FInUse := TFslList<TKDBConnection>.create;

  FClosing := false;
  GManagers.AddConnMan(self);
  init;
end;

procedure TKDBManager.init;
begin
end;

destructor TKDBManager.Destroy;
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

function TKDBManager.GetCurrentCount: Integer;
begin
  FLock.Enter;
  try
    result := FConnections.Count;
  finally
    FLock.Leave;
  end;
end;

procedure TKDBManager.CheckWait;
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
        raise EDBException.Create('['+Name+'] FHIR.Database.Manager Wait Failed - ' + ErrorAsString(GetLastError));
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

function TKDBManager.GetConnection(const AUsage: String): TKDBConnection;
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

procedure TKDBManager.Release(AConn : TKDBConnection);
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

procedure TKDBManager.Error(AConn : TKDBConnection; AException: Exception; AErrMsg : string);
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

function TKDBManager.GetConnSummary: String;
var
  i : integer;
begin
  result := '';
  FLock.Enter;
  try
    for i := 0 to FInUse.Count - 1 do
      begin
      StringAppend(result, (FInUse[i] as TKDBConnection).FUsage+' '+DescribePeriod(now - (FInUse[i] as TKDBConnection).FUsed)+' ('+(FInUse[i] as TKDBConnection).SQL+')', #13#10);
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

function TKDBManager.GetCurrentUse: Integer;
begin
  FLock.Enter;
  try
    result := FInUse.Count;
  finally
    FLock.Leave;
  end;
end;

function TKDBManager.Link: TKDBManager;
begin
  result := TKDBManager(inherited link);
end;

function TKDBManager.PopAvail: TKDBConnection;
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

procedure TKDBManager.ExecSQL(ASql, AName : String);
var
  LConn : TKDBConnection;
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

procedure TKDBManager.SetMaxConnCount(const Value: Integer);
begin
  FLock.Enter;
  try
    FMaxConnCount := Value;
  finally
    FLock.Leave;
  end;
end;

procedure TKDBManager.connection(usage: String; proc: TKDBConnectionProc);
var
  conn : TKDBConnection;
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

{ TKDBManagerList }

constructor TKDBManagerList.create;
begin
  inherited create;
  FLock := TFslLock.create;
  FHooks := TFslList<TKDBHook>.create;
  FList := TList<TKDBManager>.create;
end;

destructor TKDBManagerList.destroy;
begin
  FLock.free;
  FHooks.free;
  FList.Free;
  inherited;
end;

function TKDBManagerList.dump: String;
var
  i : integer;
begin
  result := '';
  for i := 0 to FList.Count - 1 do
    result := result + FList[i].FName+' : '+ FList[i].GetConnSummary+#13#10;
end;

procedure TKDBManagerList.AddConnMan(AConnMan : TKDBManager);
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

procedure TKDBManagerList.RemoveConnMan(AConnMan : TKDBManager);
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

function TKDBManagerList.GetConnMan(i : Integer):TKDBManager;
begin
  result := FList[i];
end;

function TKDBManagerList.HasConnManByName(s : String) : Boolean;
begin
  result := GetConnManByName(s) <> nil;
End;

function TKDBManagerList.GetConnManByName(s : String):TKDBManager;
var
  k : TKDBManager;
begin
  result := nil;
  for k in FList do
    if k.Name = s then
    begin
      result := k;
      exit;
    end;
end;

procedure TKDBManagerList.Lock;
begin
  FLock.Enter;
end;

procedure TKDBManagerList.UnLock;
begin
  FLock.Leave;
end;

procedure TKDBManagerList.RegisterHook(AName : String; AHook : TKDBManagerEvent);
begin
  FHooks.Add(TKDBHook.create(AName, AHook));
end;

procedure TKDBManagerList.UnRegisterHook(AName : String);
var
  i : integer;
begin
  for i := FHooks.Count - 1 downto 0 do
    if FHooks[i].FName = AName then
      FHooks.Delete(i);
end;

{ TKDBHook }

constructor TKDBHook.create(Name : String; Hook : TKDBManagerEvent);
begin
  inherited create;
  FName := name;
  FHook := Hook;
end;

{ TKDBColumn }

constructor TKDBColumn.Create(name: String);
begin
  inherited create;
  self.Name := name;
end;

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

function TKDBColumn.Link: TKDBColumn;
begin
  result := TKDBColumn(Inherited Link);
end;

function CommaText(list : TFslList<TKDBColumn>) : String;
var
  s : TStringBuilder;
  b : boolean;
  c : TKDBColumn;
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

{ TKDBIndex }

constructor TKDBIndex.create;
begin
  inherited;
  FColumns := TFslList<TKDBColumn>.create;
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


  Result := Result + 'INDEX ' + FName + ' ON (' + CommaText(FColumns) + ')';
end;

function TKDBRelationship.Describe : String;
Begin
  result := FColumn + ' -> '+FDestTable+'.'+FDestColumn;
End;

{ TKDBTable }

constructor TKDBTable.create;
begin
  inherited;
  FColumns := TFslList<TKDBColumn>.CREATE;
  FIndexes := TFslList<TKDBIndex>.create;
  FRelationships := TFslList<TKDBRelationship>.create;
end;

destructor TKDBTable.destroy;
begin
  FRelationships.Free;
  FColumns.Free;
  FIndexes.Free;
  inherited;
end;

function TKDBTable.hasColumn(name: String): boolean;
var
  c : TKDBColumn;
begin
  result := false;
  for c in FColumns do
    result := result or (c.Name = name);
end;

function TKDBTable.Link: TKDBTable;
begin
  result := TKDBTable(inherited link);
end;

{ TKDBMetaData }

constructor TKDBMetaData.create;
begin
  inherited;
  FTables := TFslList<TKDBTable>.create;
  FProcedures := TStringList.create;
end;

destructor TKDBMetaData.destroy;
begin
  FTables.Free;
  FProcedures.Free;
  inherited;
end;


function TKDBMetaData.GetTable(name: String): TKDBTable;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Tables.Count - 1 do
    if Tables[i].Name = name then
      result := Tables[i];
end;

function TKDBMetaData.HasTable(name: String): boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to Tables.Count - 1 do
    if Tables[i].Name = name then
      result := true;
end;


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

procedure CloseUPGManagers;
var
  m : TKDBManagerList;
begin
  m := GManagers;
  GManagers := nil;
  m.free;
end;

initialization
  GManagers := TKDBManagerList.create;
finalization
  CloseUPGManagers;
end.
