unit fdb_sqlite3_objects;

{*
 * SQLite for Delphi and FreePascal/Lazarus
 *
 * This unit contains complete fdb_sqlite3_objects API translation
 * Version of SQLite: 3.6.22
 *
 * Copyright 2010+ Yury Plashenkov
 * http://plashenkov.github.io/sqlite/
 *
 * The MIT License (MIT)
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom
 * the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *}

// modified by Grahame to load dynamically

{$I fhir.inc}

{ $DEFINE SQLITE_DEPRECATED}              // Enable deprecated functions
{ $DEFINE SQLITE_EXPERIMENTAL}            // Enable experimental functions

{$DEFINE SQLITE_ENABLE_COLUMN_METADATA}   // Enable functions to work with
                                          // column metadata:
                                          // table name, DB name, etc.

{$DEFINE SQLITE_ENABLE_UNLOCK_NOTIFY}     // Enable sqlite3_unlock_notify()
                                          // function to receive DB unlock
                                          // notification

{ $DEFINE SQLITE_DEBUG}                   // Enable sqlite3_mutex_held() and
                                          // sqlite3_mutex_notheld() functions

interface

uses
  fsl_fpc;

type
  PPAnsiCharArray = ^TPAnsiCharArray;
  TPAnsiCharArray = array[0..MaxInt div SizeOf(PAnsiChar) - 1] of PAnsiChar;

const
{$IFDEF WINDOWS}
  sqlite3_lib = 'sqlite3.dll';
{$ENDIF}
{$IFDEF LINUX}
  sqlite3_lib = 'libsqlite3.so';
{$ENDIF}
{$IFDEF OSX}
  sqlite3_lib = 'libsqlite3.dylib';
{$ENDIF}

//var sqlite3_version: PAnsiChar;
Type Tsqlite3_libversion = function : PAnsiChar; cdecl;
var sqlite3_libversion : Tsqlite3_libversion;
Type Tsqlite3_sourceid = function : PAnsiChar; cdecl;
var sqlite3_sourceid : Tsqlite3_sourceid;
Type Tsqlite3_libversion_number = function : Integer; cdecl;
var sqlite3_libversion_number : Tsqlite3_libversion_number;

Type Tsqlite3_threadsafe = function : Integer; cdecl;
var sqlite3_threadsafe : Tsqlite3_threadsafe;

type
  PSQLite3 = type Pointer;

Type Tsqlite3_close = function (db: PSQLite3): Integer; cdecl;
var sqlite3_close : Tsqlite3_close;

type
  TSQLite3Callback = function(pArg: Pointer; nCol: Integer; argv: PPAnsiCharArray; colv: PPAnsiCharArray): Integer; cdecl;

Type Tsqlite3_exec = function (db: PSQLite3; const sql: PAnsiChar; callback: TSQLite3Callback; pArg: Pointer; errmsg: PPAnsiChar): Integer; cdecl;
var sqlite3_exec : Tsqlite3_exec;

const
  SQLITE_OK         = 0;
  SQLITE_ERROR      = 1;
  SQLITE_INTERNAL   = 2;
  SQLITE_PERM       = 3;
  SQLITE_ABORT      = 4;
  SQLITE_BUSY       = 5;
  SQLITE_LOCKED     = 6;
  SQLITE_NOMEM      = 7;
  SQLITE_READONLY   = 8;
  SQLITE_INTERRUPT  = 9;
  SQLITE_IOERR      = 10;
  SQLITE_CORRUPT    = 11;
  SQLITE_NOTFOUND   = 12;
  SQLITE_FULL       = 13;
  SQLITE_CANTOPEN   = 14;
  SQLITE_PROTOCOL   = 15;
  SQLITE_EMPTY      = 16;
  SQLITE_SCHEMA     = 17;
  SQLITE_TOOBIG     = 18;
  SQLITE_CONSTRAINT = 19;
  SQLITE_MISMATCH   = 20;
  SQLITE_MISUSE     = 21;
  SQLITE_NOLFS      = 22;
  SQLITE_AUTH       = 23;
  SQLITE_FORMAT     = 24;
  SQLITE_RANGE      = 25;
  SQLITE_NOTADB     = 26;
  SQLITE_ROW        = 100;
  SQLITE_DONE       = 101;

const
  SQLITE_IOERR_READ              = SQLITE_IOERR or (1 shl 8);
  SQLITE_IOERR_SHORT_READ        = SQLITE_IOERR or (2 shl 8);
  SQLITE_IOERR_WRITE             = SQLITE_IOERR or (3 shl 8);
  SQLITE_IOERR_FSYNC             = SQLITE_IOERR or (4 shl 8);
  SQLITE_IOERR_DIR_FSYNC         = SQLITE_IOERR or (5 shl 8);
  SQLITE_IOERR_TRUNCATE          = SQLITE_IOERR or (6 shl 8);
  SQLITE_IOERR_FSTAT             = SQLITE_IOERR or (7 shl 8);
  SQLITE_IOERR_UNLOCK            = SQLITE_IOERR or (8 shl 8);
  SQLITE_IOERR_RDLOCK            = SQLITE_IOERR or (9 shl 8);
  SQLITE_IOERR_DELETE            = SQLITE_IOERR or (10 shl 8);
  SQLITE_IOERR_BLOCKED           = SQLITE_IOERR or (11 shl 8);
  SQLITE_IOERR_NOMEM             = SQLITE_IOERR or (12 shl 8);
  SQLITE_IOERR_ACCESS            = SQLITE_IOERR or (13 shl 8);
  SQLITE_IOERR_CHECKRESERVEDLOCK = SQLITE_IOERR or (14 shl 8);
  SQLITE_IOERR_LOCK              = SQLITE_IOERR or (15 shl 8);
  SQLITE_IOERR_CLOSE             = SQLITE_IOERR or (16 shl 8);
  SQLITE_IOERR_DIR_CLOSE         = SQLITE_IOERR or (17 shl 8);
  SQLITE_LOCKED_SHAREDCACHE      = SQLITE_LOCKED or (1 shl 8);

const
  SQLITE_OPEN_READONLY       = $00000001;
  SQLITE_OPEN_READWRITE      = $00000002;
  SQLITE_OPEN_CREATE         = $00000004;
  SQLITE_OPEN_DELETEONCLOSE  = $00000008;
  SQLITE_OPEN_EXCLUSIVE      = $00000010;
  SQLITE_OPEN_MAIN_DB        = $00000100;
  SQLITE_OPEN_TEMP_DB        = $00000200;
  SQLITE_OPEN_TRANSIENT_DB   = $00000400;
  SQLITE_OPEN_MAIN_JOURNAL   = $00000800;
  SQLITE_OPEN_TEMP_JOURNAL   = $00001000;
  SQLITE_OPEN_SUBJOURNAL     = $00002000;
  SQLITE_OPEN_MASTER_JOURNAL = $00004000;
  SQLITE_OPEN_NOMUTEX        = $00008000;
  SQLITE_OPEN_FULLMUTEX      = $00010000;
  SQLITE_OPEN_SHAREDCACHE    = $00020000;
  SQLITE_OPEN_PRIVATECACHE   = $00040000;

const
  SQLITE_IOCAP_ATOMIC      = $00000001;
  SQLITE_IOCAP_ATOMIC512   = $00000002;
  SQLITE_IOCAP_ATOMIC1K    = $00000004;
  SQLITE_IOCAP_ATOMIC2K    = $00000008;
  SQLITE_IOCAP_ATOMIC4K    = $00000010;
  SQLITE_IOCAP_ATOMIC8K    = $00000020;
  SQLITE_IOCAP_ATOMIC16K   = $00000040;
  SQLITE_IOCAP_ATOMIC32K   = $00000080;
  SQLITE_IOCAP_ATOMIC64K   = $00000100;
  SQLITE_IOCAP_SAFE_APPEND = $00000200;
  SQLITE_IOCAP_SEQUENTIAL  = $00000400;

const
  SQLITE_LOCK_NONE      = 0;
  SQLITE_LOCK_SHARED    = 1;
  SQLITE_LOCK_RESERVED  = 2;
  SQLITE_LOCK_PENDING   = 3;
  SQLITE_LOCK_EXCLUSIVE = 4;

const
  SQLITE_SYNC_NORMAL   = $00002;
  SQLITE_SYNC_FULL     = $00003;
  SQLITE_SYNC_DATAONLY = $00010;

type
  PSQLite3File = ^TSQLite3File;
  PSQLite3IOMethods = ^TSQLite3IOMethods;

  sqlite3_file = record
    pMethods: PSQLite3IOMethods;
  end;
  TSQLite3File = sqlite3_file;

  sqlite3_io_methods = record
    iVersion: Integer;
    xClose: function(id: PSQLite3File): Integer; cdecl;
    xRead: function(id: PSQLite3File; pBuf: Pointer; iAmt: Integer; iOfst: Int64): Integer; cdecl;
    xWrite: function(id: PSQLite3File; const pBuf: Pointer; iAmt: Integer; iOfst: Int64): Integer; cdecl;
    xTruncate: function(id: PSQLite3File; size: Int64): Integer; cdecl;
    xSync: function(id: PSQLite3File; flags: Integer): Integer; cdecl;
    xFileSize: function(id: PSQLite3File; var pSize: Int64): Integer; cdecl;
    xLock: function(id: PSQLite3File; locktype: Integer): Integer; cdecl;
    xUnlock: function(id: PSQLite3File; locktype: Integer): Integer; cdecl;
    xCheckReservedLock: function(f: PSQLite3File; var pResOut: Integer): Integer; cdecl;
    xFileControl: function(id: PSQLite3File; op: Integer; pArg: Pointer): Integer; cdecl;
    xSectorSize: function(id: PSQLite3File): Integer; cdecl;
    xDeviceCharacteristics: function(id: PSQLite3File): Integer; cdecl;
  end;
  TSQLite3IOMethods = sqlite3_io_methods;

const
  SQLITE_FCNTL_LOCKSTATE   = 1;
  SQLITE_GET_LOCKPROXYFILE = 2;
  SQLITE_SET_LOCKPROXYFILE = 3;
  SQLITE_LAST_ERRNO        = 4;

type
  PSQLite3Mutex = type Pointer;

type
  PSQLite3VFS = ^TSQLite3VFS;
  sqlite3_vfs = record
    iVersion: Integer;
    szOsFile: Integer;
    mxPathname: Integer;
    pNext: PSQLite3VFS;
    zName: PAnsiChar;
    pAppData: Pointer;
    xOpen: function(pVfs: PSQLite3VFS; const zName: PAnsiChar; id: PSQLite3File; flags: Integer; pOutFlags: PInteger): Integer; cdecl;
    xDelete: function(pVfs: PSQLite3VFS; const zName: PAnsiChar; syncDir: Integer): Integer; cdecl;
    xAccess: function(pVfs: PSQLite3VFS; const zName: PAnsiChar; flags: Integer; var pResOut: Integer): Integer; cdecl;
    xFullPathname: function(pVfs: PSQLite3VFS; const zName: PAnsiChar; nOut: Integer; zOut: PAnsiChar): Integer; cdecl;
    xDlOpen: function(pVfs: PSQLite3VFS; const zFilename: PAnsiChar): Pointer; cdecl;
    xDlError: procedure(pVfs: PSQLite3VFS; nByte: Integer; zErrMsg: PAnsiChar); cdecl;
    xDlSym: function(pVfs: PSQLite3VFS; pHandle: Pointer; const zSymbol: PAnsiChar): Pointer; cdecl;
    xDlClose: procedure(pVfs: PSQLite3VFS; pHandle: Pointer); cdecl;
    xRandomness: function(pVfs: PSQLite3VFS; nByte: Integer; zOut: PAnsiChar): Integer; cdecl;
    xSleep: function(pVfs: PSQLite3VFS; microseconds: Integer): Integer; cdecl;
    xCurrentTime: function(pVfs: PSQLite3VFS; var prNow: Double): Integer; cdecl;
    xGetLastError: function(pVfs: PSQLite3VFS; nBuf: Integer; zBuf: PAnsiChar): Integer; cdecl;
  end;
  TSQLite3VFS = sqlite3_vfs;

const
  SQLITE_ACCESS_EXISTS    = 0;
  SQLITE_ACCESS_READWRITE = 1;
  SQLITE_ACCESS_READ      = 2;

Type Tsqlite3_initialize = function : Integer; cdecl;
var sqlite3_initialize : Tsqlite3_initialize;
Type Tsqlite3_shutdown = function : Integer; cdecl;
var sqlite3_shutdown : Tsqlite3_shutdown;
Type Tsqlite3_os_init = function : Integer; cdecl;
var sqlite3_os_init : Tsqlite3_os_init;
Type Tsqlite3_os_end = function : Integer; cdecl;
var sqlite3_os_end : Tsqlite3_os_end;

{$IFDEF SQLITE_EXPERIMENTAL}
Type Tsqlite3_config = function (op: Integer{; ...}): Integer; cdecl;
var sqlite3_config : Tsqlite3_config;

Type Tsqlite3_db_config = function (db: PSQLite3; op: Integer{; ...}): Integer; cdecl;
var sqlite3_db_config : Tsqlite3_db_config;

type
  sqlite3_mem_methods = record
    xMalloc: function(nByte: Integer): Pointer; cdecl;
    xFree: procedure(pPrior: Pointer); cdecl;
    xRealloc: function(pPrior: Pointer; nByte: Integer): Pointer; cdecl;
    xSize: function(pPrior: Pointer): Integer; cdecl;
    xRoundup: function(n: Integer): Integer; cdecl;
    xInit: function(NotUsed: Pointer): Integer; cdecl;
    xShutdown: procedure(NotUsed: Pointer); cdecl;
    pAppData: Pointer;
  end;
  TSQLite3MemMethods = sqlite3_mem_methods;

const
  SQLITE_CONFIG_SINGLETHREAD = 1;
  SQLITE_CONFIG_MULTITHREAD  = 2;
  SQLITE_CONFIG_SERIALIZED   = 3;
  SQLITE_CONFIG_MALLOC       = 4;
  SQLITE_CONFIG_GETMALLOC    = 5;
  SQLITE_CONFIG_SCRATCH      = 6;
  SQLITE_CONFIG_PAGECACHE    = 7;
  SQLITE_CONFIG_HEAP         = 8;
  SQLITE_CONFIG_MEMSTATUS    = 9;
  SQLITE_CONFIG_MUTEX        = 10;
  SQLITE_CONFIG_GETMUTEX     = 11;
  //SQLITE_CONFIG_CHUNKALLOC   = 12;
  SQLITE_CONFIG_LOOKASIDE    = 13;
  SQLITE_CONFIG_PCACHE       = 14;
  SQLITE_CONFIG_GETPCACHE    = 15;

const
  SQLITE_DBCONFIG_LOOKASIDE  = 1001;
{$ENDIF}

Type Tsqlite3_extended_result_codes = function (db: PSQLite3; onoff: Integer): Integer; cdecl;
var sqlite3_extended_result_codes : Tsqlite3_extended_result_codes;

Type Tsqlite3_last_insert_rowid = function (db: PSQLite3): Int64; cdecl;
var sqlite3_last_insert_rowid : Tsqlite3_last_insert_rowid;

Type Tsqlite3_changes = function (db: PSQLite3): Integer; cdecl;
var sqlite3_changes : Tsqlite3_changes;

Type Tsqlite3_total_changes = function (db: PSQLite3): Integer; cdecl;
var sqlite3_total_changes : Tsqlite3_total_changes;

Type Tsqlite3_interrupt = procedure (db: PSQLite3); cdecl;
var sqlite3_interrupt : Tsqlite3_interrupt;

Type Tsqlite3_complete = function (const sql: PAnsiChar): Integer; cdecl;
var sqlite3_complete : Tsqlite3_complete;
Type Tsqlite3_complete16 = function (const sql: PWideChar): Integer; cdecl;
var sqlite3_complete16 : Tsqlite3_complete16;

type
  TSQLite3BusyCallback = function(ptr: Pointer; count: Integer): Integer; cdecl;

Type Tsqlite3_busy_handler = function (db: PSQLite3; xBusy: TSQLite3BusyCallback; pArg: Pointer): Integer; cdecl;
var sqlite3_busy_handler : Tsqlite3_busy_handler;

Type Tsqlite3_busy_timeout = function (db: PSQLite3; ms: Integer): Integer; cdecl;
var sqlite3_busy_timeout : Tsqlite3_busy_timeout;

Type Tsqlite3_get_table = function (db: PSQLite3; const zSql: PAnsiChar; var pazResult: PPAnsiCharArray; pnRow: PInteger; pnColumn: PInteger; pzErrmsg: PPAnsiChar): Integer; cdecl;
var sqlite3_get_table : Tsqlite3_get_table;
Type Tsqlite3_free_table = procedure (result: PPAnsiCharArray); cdecl;
var sqlite3_free_table : Tsqlite3_free_table;

Type Tsqlite3_mprintf = function (const zFormat: PAnsiChar{; ...}): PAnsiChar; cdecl;
var sqlite3_mprintf : Tsqlite3_mprintf;
Type Tsqlite3_vmprintf = function (const zFormat: PAnsiChar; ap: Pointer{va_list}): PAnsiChar; cdecl;
var sqlite3_vmprintf : Tsqlite3_vmprintf;
Type Tsqlite3_snprintf = function (n: Integer; zBuf: PAnsiChar; const zFormat: PAnsiChar{; ...}): PAnsiChar; cdecl;
var sqlite3_snprintf : Tsqlite3_snprintf;

Type Tsqlite3_malloc = function (n: Integer): Pointer; cdecl;
var sqlite3_malloc : Tsqlite3_malloc;
Type Tsqlite3_realloc = function (pOld: Pointer; n: Integer): Pointer; cdecl;
var sqlite3_realloc : Tsqlite3_realloc;
Type Tsqlite3_free = procedure (p: Pointer); cdecl;
var sqlite3_free : Tsqlite3_free;

Type Tsqlite3_memory_used = function : Int64; cdecl;
var sqlite3_memory_used : Tsqlite3_memory_used;
Type Tsqlite3_memory_highwater = function (resetFlag: Integer): Int64; cdecl;
var sqlite3_memory_highwater : Tsqlite3_memory_highwater;

Type Tsqlite3_randomness = procedure (N: Integer; P: Pointer); cdecl;
var sqlite3_randomness : Tsqlite3_randomness;

type
  TSQLite3AuthorizerCallback = function(pAuthArg: Pointer; code: Integer; const zTab: PAnsiChar; const zCol: PAnsiChar; const zDb: PAnsiChar; const zAuthContext: PAnsiChar): Integer; cdecl;

Type Tsqlite3_set_authorizer = function (db: PSQLite3; xAuth: TSQLite3AuthorizerCallback; pUserData: Pointer): Integer; cdecl;
var sqlite3_set_authorizer : Tsqlite3_set_authorizer;

const
  SQLITE_DENY   = 1;
  SQLITE_IGNORE = 2;

const
  SQLITE_CREATE_INDEX        = 1;
  SQLITE_CREATE_TABLE        = 2;
  SQLITE_CREATE_TEMP_INDEX   = 3;
  SQLITE_CREATE_TEMP_TABLE   = 4;
  SQLITE_CREATE_TEMP_TRIGGER = 5;
  SQLITE_CREATE_TEMP_VIEW    = 6;
  SQLITE_CREATE_TRIGGER      = 7;
  SQLITE_CREATE_VIEW         = 8;
  SQLITE_DELETE              = 9;
  SQLITE_DROP_INDEX          = 10;
  SQLITE_DROP_TABLE          = 11;
  SQLITE_DROP_TEMP_INDEX     = 12;
  SQLITE_DROP_TEMP_TABLE     = 13;
  SQLITE_DROP_TEMP_TRIGGER   = 14;
  SQLITE_DROP_TEMP_VIEW      = 15;
  SQLITE_DROP_TRIGGER        = 16;
  SQLITE_DROP_VIEW           = 17;
  SQLITE_INSERT              = 18;
  SQLITE_PRAGMA              = 19;
  SQLITE_READ                = 20;
  SQLITE_SELECT              = 21;
  SQLITE_TRANSACTION         = 22;
  SQLITE_UPDATE              = 23;
  SQLITE_ATTACH              = 24;
  SQLITE_DETACH              = 25;
  SQLITE_ALTER_TABLE         = 26;
  SQLITE_REINDEX             = 27;
  SQLITE_ANALYZE             = 28;
  SQLITE_CREATE_VTABLE       = 29;
  SQLITE_DROP_VTABLE         = 30;
  SQLITE_FUNCTION            = 31;
  SQLITE_SAVEPOINT           = 32;
  SQLITE_COPY                = 0;

{$IFDEF SQLITE_EXPERIMENTAL}
type
  TSQLite3TraceCallback = procedure(pTraceArg: Pointer; const zTrace: PAnsiChar); cdecl;
  TSQLite3ProfileCallback = procedure(pProfileArg: Pointer; const zSql: PAnsiChar; elapseTime: UInt64); cdecl;

Type Tsqlite3_trace = function (db: PSQLite3; xTrace: TSQLite3TraceCallback; pArg: Pointer): Pointer; cdecl;
var sqlite3_trace : Tsqlite3_trace;
Type Tsqlite3_profile = function (db: PSQLite3; xProfile: TSQLite3ProfileCallback; pArg: Pointer): Pointer; cdecl;
var sqlite3_profile : Tsqlite3_profile;
{$ENDIF}

type
  TSQLite3ProgressCallback = function(pProgressArg: Pointer): Integer; cdecl;

Type Tsqlite3_progress_handler = procedure (db: PSQLite3; nOps: Integer; xProgress: TSQLite3ProgressCallback; pArg: Pointer); cdecl;
var sqlite3_progress_handler : Tsqlite3_progress_handler;

Type Tsqlite3_open = function (const filename: PAnsiChar; var ppDb: PSQLite3): Integer; cdecl;
var sqlite3_open : Tsqlite3_open;
Type Tsqlite3_open16 = function (const filename: PWideChar; var ppDb: PSQLite3): Integer; cdecl;
var sqlite3_open16 : Tsqlite3_open16;
Type Tsqlite3_open_v2 = function (const filename: PAnsiChar; var ppDb: PSQLite3; flags: Integer; const zVfs: PAnsiChar): Integer; cdecl;
var sqlite3_open_v2 : Tsqlite3_open_v2;

Type Tsqlite3_errcode = function (db: PSQLite3): Integer; cdecl;
var sqlite3_errcode : Tsqlite3_errcode;
Type Tsqlite3_extended_errcode = function (db: PSQLite3): Integer; cdecl;
var sqlite3_extended_errcode : Tsqlite3_extended_errcode;
Type Tsqlite3_errmsg = function (db: PSQLite3): PAnsiChar; cdecl;
var sqlite3_errmsg : Tsqlite3_errmsg;
Type Tsqlite3_errmsg16 = function (db: PSQLite3): PWideChar; cdecl;
var sqlite3_errmsg16 : Tsqlite3_errmsg16;

type
  PSQLite3Stmt = type Pointer;

Type Tsqlite3_limit = function (db: PSQLite3; limitId: Integer; newLimit: Integer): Integer; cdecl;
var sqlite3_limit : Tsqlite3_limit;

const
  SQLITE_LIMIT_LENGTH              = 0;
  SQLITE_LIMIT_SQL_LENGTH          = 1;
  SQLITE_LIMIT_COLUMN              = 2;
  SQLITE_LIMIT_EXPR_DEPTH          = 3;
  SQLITE_LIMIT_COMPOUND_SELECT     = 4;
  SQLITE_LIMIT_VDBE_OP             = 5;
  SQLITE_LIMIT_FUNCTION_ARG        = 6;
  SQLITE_LIMIT_ATTACHED            = 7;
  SQLITE_LIMIT_LIKE_PATTERN_LENGTH = 8;
  SQLITE_LIMIT_VARIABLE_NUMBER     = 9;
  SQLITE_LIMIT_TRIGGER_DEPTH       = 10;

Type Tsqlite3_prepare = function (db: PSQLite3; const zSql: PAnsiChar; nByte: Integer; var ppStmt: PSQLite3Stmt; const pzTail: PPAnsiChar): Integer; cdecl;
var sqlite3_prepare : Tsqlite3_prepare;
Type Tsqlite3_prepare_v2 = function (db: PSQLite3; const zSql: PAnsiChar; nByte: Integer; var ppStmt: PSQLite3Stmt; const pzTail: PPAnsiChar): Integer; cdecl;
var sqlite3_prepare_v2 : Tsqlite3_prepare_v2;
Type Tsqlite3_prepare16 = function (db: PSQLite3; const zSql: PWideChar; nByte: Integer; var ppStmt: PSQLite3Stmt; const pzTail: PPWideChar): Integer; cdecl;
var sqlite3_prepare16 : Tsqlite3_prepare16;
Type Tsqlite3_prepare16_v2 = function (db: PSQLite3; const zSql: PWideChar; nByte: Integer; var ppStmt: PSQLite3Stmt; const pzTail: PPWideChar): Integer; cdecl;
var sqlite3_prepare16_v2 : Tsqlite3_prepare16_v2;

Type Tsqlite3_sql = function (pStmt: PSQLite3Stmt): PAnsiChar; cdecl;
var sqlite3_sql : Tsqlite3_sql;

type
  PSQLite3Value = ^TSQLite3Value;
  sqlite3_value = type Pointer;
  TSQLite3Value = sqlite3_value;

  PPSQLite3ValueArray = ^TPSQLite3ValueArray;
  TPSQLite3ValueArray = array[0..MaxInt div SizeOf(PSQLite3Value) - 1] of PSQLite3Value;

type
  PSQLite3Context = type Pointer;

type
  TSQLite3DestructorType = procedure(p: Pointer); cdecl;

const
  SQLITE_STATIC    = Pointer(0);
  SQLITE_TRANSIENT = Pointer(-1);

Type Tsqlite3_bind_blob = function (pStmt: PSQLite3Stmt; i: Integer; const zData: Pointer; n: Integer; xDel: TSQLite3DestructorType): Integer; cdecl;
var sqlite3_bind_blob : Tsqlite3_bind_blob;
Type Tsqlite3_bind_double = function (pStmt: PSQLite3Stmt; i: Integer; rValue: Double): Integer; cdecl;
var sqlite3_bind_double : Tsqlite3_bind_double;
Type Tsqlite3_bind_int = function (p: PSQLite3Stmt; i: Integer; iValue: Integer): Integer; cdecl;
var sqlite3_bind_int : Tsqlite3_bind_int;
Type Tsqlite3_bind_int64 = function (pStmt: PSQLite3Stmt; i: Integer; iValue: Int64): Integer; cdecl;
var sqlite3_bind_int64 : Tsqlite3_bind_int64;
Type Tsqlite3_bind_null = function (pStmt: PSQLite3Stmt; i: Integer): Integer; cdecl;
var sqlite3_bind_null : Tsqlite3_bind_null;
Type Tsqlite3_bind_text = function (pStmt: PSQLite3Stmt; i: Integer; const zData: PAnsiChar; n: Integer; xDel: TSQLite3DestructorType): Integer; cdecl;
var sqlite3_bind_text : Tsqlite3_bind_text;
Type Tsqlite3_bind_text16 = function (pStmt: PSQLite3Stmt; i: Integer; const zData: PWideChar; nData: Integer; xDel: TSQLite3DestructorType): Integer; cdecl;
var sqlite3_bind_text16 : Tsqlite3_bind_text16;
Type Tsqlite3_bind_value = function (pStmt: PSQLite3Stmt; i: Integer; const pValue: PSQLite3Value): Integer; cdecl;
var sqlite3_bind_value : Tsqlite3_bind_value;
Type Tsqlite3_bind_zeroblob = function (pStmt: PSQLite3Stmt; i: Integer; n: Integer): Integer; cdecl;
var sqlite3_bind_zeroblob : Tsqlite3_bind_zeroblob;

Type Tsqlite3_bind_parameter_count = function (pStmt: PSQLite3Stmt): Integer; cdecl;
var sqlite3_bind_parameter_count : Tsqlite3_bind_parameter_count;

Type Tsqlite3_bind_parameter_name = function (pStmt: PSQLite3Stmt; i: Integer): PAnsiChar; cdecl;
var sqlite3_bind_parameter_name : Tsqlite3_bind_parameter_name;

Type Tsqlite3_bind_parameter_index = function (pStmt: PSQLite3Stmt; const zName: PAnsiChar): Integer; cdecl;
var sqlite3_bind_parameter_index : Tsqlite3_bind_parameter_index;

Type Tsqlite3_clear_bindings = function (pStmt: PSQLite3Stmt): Integer; cdecl;
var sqlite3_clear_bindings : Tsqlite3_clear_bindings;

Type Tsqlite3_column_count = function (pStmt: PSQLite3Stmt): Integer; cdecl;
var sqlite3_column_count : Tsqlite3_column_count;

Type Tsqlite3_column_name = function (pStmt: PSQLite3Stmt; N: Integer): PAnsiChar; cdecl;
var sqlite3_column_name : Tsqlite3_column_name;
Type Tsqlite3_column_name16 = function (pStmt: PSQLite3Stmt; N: Integer): PWideChar; cdecl;
var sqlite3_column_name16 : Tsqlite3_column_name16;

{$IFDEF SQLITE_ENABLE_COLUMN_METADATA}
Type Tsqlite3_column_database_name = function (pStmt: PSQLite3Stmt; N: Integer): PAnsiChar; cdecl;
var sqlite3_column_database_name : Tsqlite3_column_database_name;
Type Tsqlite3_column_database_name16 = function (pStmt: PSQLite3Stmt; N: Integer): PWideChar; cdecl;
var sqlite3_column_database_name16 : Tsqlite3_column_database_name16;
Type Tsqlite3_column_table_name = function (pStmt: PSQLite3Stmt; N: Integer): PAnsiChar; cdecl;
var sqlite3_column_table_name : Tsqlite3_column_table_name;
Type Tsqlite3_column_table_name16 = function (pStmt: PSQLite3Stmt; N: Integer): PWideChar; cdecl;
var sqlite3_column_table_name16 : Tsqlite3_column_table_name16;
Type Tsqlite3_column_origin_name = function (pStmt: PSQLite3Stmt; N: Integer): PAnsiChar; cdecl;
var sqlite3_column_origin_name : Tsqlite3_column_origin_name;
Type Tsqlite3_column_origin_name16 = function (pStmt: PSQLite3Stmt; N: Integer): PWideChar; cdecl;
var sqlite3_column_origin_name16 : Tsqlite3_column_origin_name16;
{$ENDIF}

Type Tsqlite3_column_decltype = function (pStmt: PSQLite3Stmt; N: Integer): PAnsiChar; cdecl;
var sqlite3_column_decltype : Tsqlite3_column_decltype;
Type Tsqlite3_column_decltype16 = function (pStmt: PSQLite3Stmt; N: Integer): PWideChar; cdecl;
var sqlite3_column_decltype16 : Tsqlite3_column_decltype16;

Type Tsqlite3_step = function (pStmt: PSQLite3Stmt): Integer; cdecl;
var sqlite3_step : Tsqlite3_step;

Type Tsqlite3_data_count = function (pStmt: PSQLite3Stmt): Integer; cdecl;
var sqlite3_data_count : Tsqlite3_data_count;

const
  SQLITE_INTEGER = 1;
  SQLITE_FLOAT   = 2;
  SQLITE_BLOB    = 4;
  SQLITE_NULL    = 5;
  SQLITE_TEXT    = 3;
  SQLITE3_TEXT   = 3;

Type Tsqlite3_column_blob = function (pStmt: PSQLite3Stmt; iCol: Integer): Pointer; cdecl;
var sqlite3_column_blob : Tsqlite3_column_blob;
Type Tsqlite3_column_bytes = function (pStmt: PSQLite3Stmt; iCol: Integer): Integer; cdecl;
var sqlite3_column_bytes : Tsqlite3_column_bytes;
Type Tsqlite3_column_bytes16 = function (pStmt: PSQLite3Stmt; iCol: Integer): Integer; cdecl;
var sqlite3_column_bytes16 : Tsqlite3_column_bytes16;
Type Tsqlite3_column_double = function (pStmt: PSQLite3Stmt; iCol: Integer): Double; cdecl;
var sqlite3_column_double : Tsqlite3_column_double;
Type Tsqlite3_column_int = function (pStmt: PSQLite3Stmt; iCol: Integer): Integer; cdecl;
var sqlite3_column_int : Tsqlite3_column_int;
Type Tsqlite3_column_int64 = function (pStmt: PSQLite3Stmt; iCol: Integer): Int64; cdecl;
var sqlite3_column_int64 : Tsqlite3_column_int64;
Type Tsqlite3_column_text = function (pStmt: PSQLite3Stmt; iCol: Integer): PAnsiChar; cdecl;
var sqlite3_column_text : Tsqlite3_column_text;
Type Tsqlite3_column_text16 = function (pStmt: PSQLite3Stmt; iCol: Integer): PWideChar; cdecl;
var sqlite3_column_text16 : Tsqlite3_column_text16;
Type Tsqlite3_column_type = function (pStmt: PSQLite3Stmt; iCol: Integer): Integer; cdecl;
var sqlite3_column_type : Tsqlite3_column_type;
Type Tsqlite3_column_value = function (pStmt: PSQLite3Stmt; iCol: Integer): PSQLite3Value; cdecl;
var sqlite3_column_value : Tsqlite3_column_value;

Type Tsqlite3_finalize = function (pStmt: PSQLite3Stmt): Integer; cdecl;
var sqlite3_finalize : Tsqlite3_finalize;

Type Tsqlite3_reset = function (pStmt: PSQLite3Stmt): Integer; cdecl;
var sqlite3_reset : Tsqlite3_reset;

type
  TSQLite3RegularFunction = procedure(ctx: PSQLite3Context; n: Integer; apVal: PPSQLite3ValueArray); cdecl;
  TSQLite3AggregateStep = procedure(ctx: PSQLite3Context; n: Integer; apVal: PPSQLite3ValueArray); cdecl;
  TSQLite3AggregateFinalize = procedure(ctx: PSQLite3Context); cdecl;

Type Tsqlite3_create_function = function (db: PSQLite3; const zFunctionName: PAnsiChar; nArg: Integer; eTextRep: Integer; pApp: Pointer; xFunc: TSQLite3RegularFunction; xStep: TSQLite3AggregateStep; xFinal: TSQLite3AggregateFinalize): Integer; cdecl;
var sqlite3_create_function : Tsqlite3_create_function;
Type Tsqlite3_create_function16 = function (db: PSQLite3; const zFunctionName: PWideChar; nArg: Integer; eTextRep: Integer; pApp: Pointer; xFunc: TSQLite3RegularFunction; xStep: TSQLite3AggregateStep; xFinal: TSQLite3AggregateFinalize): Integer; cdecl;
var sqlite3_create_function16 : Tsqlite3_create_function16;

const
  SQLITE_UTF8          = 1;
  SQLITE_UTF16LE       = 2;
  SQLITE_UTF16BE       = 3;
  SQLITE_UTF16         = 4;
  SQLITE_ANY           = 5;
  SQLITE_UTF16_ALIGNED = 8;

{$IFDEF SQLITE_DEPRECATED}
type
  TSQLite3MemoryAlarmCallback = procedure(pArg: Pointer; used: Int64; N: Integer); cdecl;

Type Tsqlite3_aggregate_count = function (p: PSQLite3Context): Integer; cdecl;
var  : sqlite3_aggregate_count;
Type Tsqlite3_expired = function (pStmt: PSQLite3Stmt): Integer; cdecl;
var sqlite3_expired : Tsqlite3_expired;
Type Tsqlite3_transfer_bindings = function (pFromStmt: PSQLite3Stmt; pToStmt: PSQLite3Stmt): Integer; cdecl;
var sqlite3_transfer_bindings : Tsqlite3_transfer_bindings;
Type Tsqlite3_global_recover = function : Integer; cdecl;
var sqlite3_global_recover: : ;
Type Tsqlite3_thread_cleanup; = procedure  cdecl;
var sqlite3_thread_cleanup; : Tsqlite3_thread_cleanup;;
Type Tsqlite3_memory_alarm = function (xCallback: TSQLite3MemoryAlarmCallback; pArg: Pointer; iThreshold: Int64): Integer; cdecl;
var sqlite3_memory_alarm : Tsqlite3_memory_alarm;
{$ENDIF}

Type Tsqlite3_value_blob = function (pVal: PSQLite3Value): Pointer; cdecl;
var sqlite3_value_blob : Tsqlite3_value_blob;
Type Tsqlite3_value_bytes = function (pVal: PSQLite3Value): Integer; cdecl;
var sqlite3_value_bytes : Tsqlite3_value_bytes;
Type Tsqlite3_value_bytes16 = function (pVal: PSQLite3Value): Integer; cdecl;
var sqlite3_value_bytes16 : Tsqlite3_value_bytes16;
Type Tsqlite3_value_double = function (pVal: PSQLite3Value): Double; cdecl;
var sqlite3_value_double : Tsqlite3_value_double;
Type Tsqlite3_value_int = function (pVal: PSQLite3Value): Integer; cdecl;
var sqlite3_value_int : Tsqlite3_value_int;
Type Tsqlite3_value_int64 = function (pVal: PSQLite3Value): Int64; cdecl;
var sqlite3_value_int64 : Tsqlite3_value_int64;
Type Tsqlite3_value_text = function (pVal: PSQLite3Value): PAnsiChar; cdecl;
var sqlite3_value_text : Tsqlite3_value_text;
Type Tsqlite3_value_text16 = function (pVal: PSQLite3Value): PWideChar; cdecl;
var sqlite3_value_text16 : Tsqlite3_value_text16;
Type Tsqlite3_value_text16le = function (pVal: PSQLite3Value): Pointer; cdecl;
var sqlite3_value_text16le : Tsqlite3_value_text16le;
Type Tsqlite3_value_text16be = function (pVal: PSQLite3Value): Pointer; cdecl;
var sqlite3_value_text16be : Tsqlite3_value_text16be;
Type Tsqlite3_value_type = function (pVal: PSQLite3Value): Integer; cdecl;
var sqlite3_value_type : Tsqlite3_value_type;
Type Tsqlite3_value_numeric_type = function (pVal: PSQLite3Value): Integer; cdecl;
var sqlite3_value_numeric_type : Tsqlite3_value_numeric_type;

Type Tsqlite3_aggregate_context = function (p: PSQLite3Context; nBytes: Integer): Pointer; cdecl;
var sqlite3_aggregate_context : Tsqlite3_aggregate_context;

Type Tsqlite3_user_data = function (p: PSQLite3Context): Pointer; cdecl;
var sqlite3_user_data : Tsqlite3_user_data;

Type Tsqlite3_context_db_handle = function (p: PSQLite3Context): PSQLite3; cdecl;
var sqlite3_context_db_handle : Tsqlite3_context_db_handle;

type
  TSQLite3AuxDataDestructor = procedure(pAux: Pointer); cdecl;

Type Tsqlite3_get_auxdata = function (pCtx: PSQLite3Context; N: Integer): Pointer; cdecl;
var sqlite3_get_auxdata : Tsqlite3_get_auxdata;
Type Tsqlite3_set_auxdata = procedure (pCtx: PSQLite3Context; N: Integer; pAux: Pointer; xDelete: TSQLite3AuxDataDestructor); cdecl;
var sqlite3_set_auxdata : Tsqlite3_set_auxdata;

Type Tsqlite3_result_blob = procedure (pCtx: PSQLite3Context; const z: Pointer; n: Integer; xDel: TSQLite3DestructorType); cdecl;
var sqlite3_result_blob : Tsqlite3_result_blob;
Type Tsqlite3_result_double = procedure (pCtx: PSQLite3Context; rVal: Double); cdecl;
var sqlite3_result_double : Tsqlite3_result_double;
Type Tsqlite3_result_error = procedure (pCtx: PSQLite3Context; const z: PAnsiChar; n: Integer); cdecl;
var sqlite3_result_error : Tsqlite3_result_error;
Type Tsqlite3_result_error16 = procedure (pCtx: PSQLite3Context; const z: PWideChar; n: Integer); cdecl;
var sqlite3_result_error16 : Tsqlite3_result_error16;
Type Tsqlite3_result_error_toobig = procedure (pCtx: PSQLite3Context); cdecl;
var sqlite3_result_error_toobig : Tsqlite3_result_error_toobig;
Type Tsqlite3_result_error_nomem = procedure (pCtx: PSQLite3Context); cdecl;
var sqlite3_result_error_nomem : Tsqlite3_result_error_nomem;
Type Tsqlite3_result_error_code = procedure (pCtx: PSQLite3Context; errCode: Integer); cdecl;
var sqlite3_result_error_code : Tsqlite3_result_error_code;
Type Tsqlite3_result_int = procedure (pCtx: PSQLite3Context; iVal: Integer); cdecl;
var sqlite3_result_int : Tsqlite3_result_int;
Type Tsqlite3_result_int64 = procedure (pCtx: PSQLite3Context; iVal: Int64); cdecl;
var sqlite3_result_int64 : Tsqlite3_result_int64;
Type Tsqlite3_result_null = procedure (pCtx: PSQLite3Context); cdecl;
var sqlite3_result_null : Tsqlite3_result_null;
Type Tsqlite3_result_text = procedure (pCtx: PSQLite3Context; const z: PAnsiChar; n: Integer; xDel: TSQLite3DestructorType); cdecl;
var sqlite3_result_text : Tsqlite3_result_text;
Type Tsqlite3_result_text16 = procedure (pCtx: PSQLite3Context; const z: PWideChar; n: Integer; xDel: TSQLite3DestructorType); cdecl;
var sqlite3_result_text16 : Tsqlite3_result_text16;
Type Tsqlite3_result_text16le = procedure (pCtx: PSQLite3Context; const z: Pointer; n: Integer; xDel: TSQLite3DestructorType); cdecl;
var sqlite3_result_text16le : Tsqlite3_result_text16le;
Type Tsqlite3_result_text16be = procedure (pCtx: PSQLite3Context; const z: Pointer; n: Integer; xDel: TSQLite3DestructorType); cdecl;
var sqlite3_result_text16be : Tsqlite3_result_text16be;
Type Tsqlite3_result_value = procedure (pCtx: PSQLite3Context; pValue: PSQLite3Value); cdecl;
var sqlite3_result_value : Tsqlite3_result_value;
Type Tsqlite3_result_zeroblob = procedure (pCtx: PSQLite3Context; n: Integer); cdecl;
var sqlite3_result_zeroblob : Tsqlite3_result_zeroblob;

type
  TSQLite3CollationCompare = function(pUser: Pointer; n1: Integer; const z1: Pointer; n2: Integer; const z2: Pointer): Integer; cdecl;
  TSQLite3CollationDestructor = procedure(pUser: Pointer); cdecl;

Type Tsqlite3_create_collation = function (db: PSQLite3; const zName: PAnsiChar; eTextRep: Integer; pUser: Pointer; xCompare: TSQLite3CollationCompare): Integer; cdecl;
var sqlite3_create_collation : Tsqlite3_create_collation;
Type Tsqlite3_create_collation_v2 = function (db: PSQLite3; const zName: PAnsiChar; eTextRep: Integer; pUser: Pointer; xCompare: TSQLite3CollationCompare; xDestroy: TSQLite3CollationDestructor): Integer; cdecl;
var sqlite3_create_collation_v2 : Tsqlite3_create_collation_v2;
Type Tsqlite3_create_collation16 = function (db: PSQLite3; const zName: PWideChar; eTextRep: Integer; pUser: Pointer; xCompare: TSQLite3CollationCompare): Integer; cdecl;
var sqlite3_create_collation16 : Tsqlite3_create_collation16;

type
  TSQLite3CollationNeededCallback = procedure(pCollNeededArg: Pointer; db: PSQLite3; eTextRep: Integer; const zExternal: PAnsiChar); cdecl;
  TSQLite3CollationNeededCallback16 = procedure(pCollNeededArg: Pointer; db: PSQLite3; eTextRep: Integer; const zExternal: PWideChar); cdecl;

Type Tsqlite3_collation_needed = function (db: PSQLite3; pCollNeededArg: Pointer; xCollNeeded: TSQLite3CollationNeededCallback): Integer; cdecl;
var sqlite3_collation_needed : Tsqlite3_collation_needed;
Type Tsqlite3_collation_needed16 = function (db: PSQLite3; pCollNeededArg: Pointer; xCollNeeded16: TSQLite3CollationNeededCallback16): Integer; cdecl;
var sqlite3_collation_needed16 : Tsqlite3_collation_needed16;

// Type Tsqlite3_key = function (db: PSQLite3; const pKey: Pointer; nKey: Integer): Integer; cdecl;

// Type Tsqlite3_rekey = function (db: PSQLite3; const pKey: Pointer; nKey: Integer): Integer; cdecl;

Type Tsqlite3_sleep = function (ms: Integer): Integer; cdecl;
var sqlite3_sleep : Tsqlite3_sleep;

//var sqlite3_temp_directory: PAnsiChar;

Type Tsqlite3_get_autocommit = function (db: PSQLite3): Integer; cdecl;
var sqlite3_get_autocommit : Tsqlite3_get_autocommit;

Type Tsqlite3_db_handle = function (pStmt: PSQLite3Stmt): PSQLite3; cdecl;
var sqlite3_db_handle : Tsqlite3_db_handle;

Type Tsqlite3_next_stmt = function (pDb: PSQLite3; pStmt: PSQLite3Stmt): PSQLite3Stmt; cdecl;
var sqlite3_next_stmt : Tsqlite3_next_stmt;

type
  TSQLite3CommitCallback = function(pCommitArg: Pointer): Integer; cdecl;
  TSQLite3RollbackCallback = procedure(pRollbackArg: Pointer); cdecl;

Type Tsqlite3_commit_hook = function (db: PSQLite3; xCallback: TSQLite3CommitCallback; pArg: Pointer): Pointer; cdecl;
var sqlite3_commit_hook : Tsqlite3_commit_hook;
Type Tsqlite3_rollback_hook = function (db: PSQLite3; xCallback: TSQLite3RollbackCallback; pArg: Pointer): Pointer; cdecl;
var sqlite3_rollback_hook : Tsqlite3_rollback_hook;

type
  TSQLite3UpdateCallback = procedure(pUpdateArg: Pointer; op: Integer; const zDb: PAnsiChar; const zTbl: PAnsiChar; iKey: Int64); cdecl;

Type Tsqlite3_update_hook = function (db: PSQLite3; xCallback: TSQLite3UpdateCallback; pArg: Pointer): Pointer; cdecl;
var sqlite3_update_hook : Tsqlite3_update_hook;

Type Tsqlite3_enable_shared_cache = function (enable: Integer): Integer; cdecl;
var sqlite3_enable_shared_cache : Tsqlite3_enable_shared_cache;

Type Tsqlite3_release_memory = function (n: Integer): Integer; cdecl;
var sqlite3_release_memory : Tsqlite3_release_memory;

Type Tsqlite3_soft_heap_limit = procedure (n: Integer); cdecl;
var sqlite3_soft_heap_limit : Tsqlite3_soft_heap_limit;

{$IFDEF SQLITE_ENABLE_COLUMN_METADATA}
Type Tsqlite3_table_column_metadata = function (db: PSQLite3; const zDbName: PAnsiChar; const zTableName: PAnsiChar; const zColumnName: PAnsiChar; const pzDataType: PPAnsiChar; const pzCollSeq: PPAnsiChar; pNotNull: PInteger; pPrimaryKey: PInteger; pAutoinc: PInteger): Integer; cdecl;
var sqlite3_table_column_metadata : Tsqlite3_table_column_metadata;
{$ENDIF}

Type Tsqlite3_load_extension = function (db: PSQLite3; const zFile: PAnsiChar; const zProc: PAnsiChar; pzErrMsg: PPAnsiChar): Integer; cdecl;
var sqlite3_load_extension : Tsqlite3_load_extension;

Type Tsqlite3_enable_load_extension = function (db: PSQLite3; onoff: Integer): Integer; cdecl;
var sqlite3_enable_load_extension : Tsqlite3_enable_load_extension;

type
  TSQLiteAutoExtensionEntryPoint = procedure; cdecl;

Type Tsqlite3_auto_extension = function (xEntryPoint: TSQLiteAutoExtensionEntryPoint): Integer; cdecl;
var sqlite3_auto_extension : Tsqlite3_auto_extension;

Type Tsqlite3_reset_auto_extension = procedure ; cdecl;
var sqlite3_reset_auto_extension : Tsqlite3_reset_auto_extension;

{$IFDEF SQLITE_EXPERIMENTAL}
type
  TSQLite3FTS3Func = procedure(pContext: PSQLite3Context; argc: Integer; argv: PPSQLite3ValueArray); cdecl;

type
  PSQLite3VTab = ^TSQLite3VTab;
  PSQLite3IndexInfo = ^TSQLite3IndexInfo;
  PSQLite3VTabCursor = ^TSQLite3VTabCursor;
  PSQLite3Module = ^TSQLite3Module;

  sqlite3_module = record
    iVersion: Integer;
    xCreate: function(db: PSQLite3; pAux: Pointer; argc: Integer; const argv: PPAnsiCharArray; var ppVTab: PSQLite3VTab; var pzErr: PAnsiChar): Integer; cdecl;
    xConnect: function(db: PSQLite3; pAux: Pointer; argc: Integer; const argv: PPAnsiCharArray; var ppVTab: PSQLite3VTab; var pzErr: PAnsiChar): Integer; cdecl;
    xBestIndex: function(pVTab: PSQLite3VTab; pInfo: PSQLite3IndexInfo): Integer; cdecl;
    xDisconnect: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xDestroy: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xOpen: function(pVTab: PSQLite3VTab; var ppCursor: PSQLite3VTabCursor): Integer; cdecl;
    xClose: function(pVtabCursor: PSQLite3VTabCursor): Integer; cdecl;
    xFilter: function(pVtabCursor: PSQLite3VTabCursor; idxNum: Integer; const idxStr: PAnsiChar; argc: Integer; argv: PPSQLite3ValueArray): Integer; cdecl;
    xNext: function(pVtabCursor: PSQLite3VTabCursor): Integer; cdecl;
    xEof: function(pVtabCursor: PSQLite3VTabCursor): Integer; cdecl;
    xColumn: function(pVtabCursor: PSQLite3VTabCursor; sContext: PSQLite3Context; p2: Integer): Integer; cdecl;
    xRowid: function(pVtabCursor: PSQLite3VTabCursor; var pRowid: Int64): Integer; cdecl;
    xUpdate: function(pVtab: PSQLite3VTab; nArg: Integer; ppArg: PPSQLite3ValueArray; var pRowid: Int64): Integer; cdecl;
    xBegin: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xSync: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xCommit: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xRollback: function(pVTab: PSQLite3VTab): Integer; cdecl;
    xFindFunction: function(pVtab: PSQLite3VTab; nArg: Integer; const zName: PAnsiChar; var pxFunc: TSQLite3FTS3Func; var ppArg: Pointer): Integer; cdecl;
    xRename: function(pVtab: PSQLite3VTab; const zNew: PAnsiChar): Integer; cdecl;
  end;
  TSQLite3Module = sqlite3_module;

  sqlite3_index_constraint = record
    iColumn: Integer;
    op: Byte;
    usable: Byte;
    iTermOffset: Integer;
  end;
  TSQLite3IndexConstraint = sqlite3_index_constraint;

  PSQLite3IndexConstraintArray = ^TSQLite3IndexConstraintArray;
  TSQLite3IndexConstraintArray = array[0..MaxInt div SizeOf(TSQLite3IndexConstraint) - 1] of TSQLite3IndexConstraint;

  sqlite3_index_orderby = record
    iColumn: Integer;
    desc: Byte;
  end;
  TSQLite3IndexOrderBy = sqlite3_index_orderby;

  PSQLite3IndexOrderByArray = ^TSQLite3IndexOrderByArray;
  TSQLite3IndexOrderByArray = array[0..MaxInt div SizeOf(TSQLite3IndexOrderBy) - 1] of TSQLite3IndexOrderBy;

  sqlite3_index_constraint_usage = record
    argvIndex: Integer;
    omit: Byte;
  end;
  TSQLite3IndexConstraintUsage = sqlite3_index_constraint_usage;

  PSQLite3IndexConstraintUsageArray = ^TSQLite3IndexConstraintUsageArray;
  TSQLite3IndexConstraintUsageArray = array[0..MaxInt div SizeOf(TSQLite3IndexConstraintUsage) - 1] of TSQLite3IndexConstraintUsage;

  sqlite3_index_info = record
    nConstraint: Integer;
    aConstraint: PSQLite3IndexConstraintArray;
    nOrderBy: Integer;
    aOrderBy: PSQLite3IndexOrderByArray;
    aConstraintUsage: PSQLite3IndexConstraintUsageArray;
    idxNum: Integer;
    idxStr: PAnsiChar;
    needToFreeIdxStr: Integer;
    orderByConsumed: Integer;
    estimatedCost: Double;
  end;
  TSQLite3IndexInfo = sqlite3_index_info;

  sqlite3_vtab = record
    pModule: PSQLite3Module;
    nRef: Integer;
    zErrMsg: PAnsiChar;
  end;
  TSQLite3VTab = sqlite3_vtab;

  sqlite3_vtab_cursor = record
    pVtab: PSQLite3VTab;
  end;
  TSQLite3VTabCursor = sqlite3_vtab_cursor;

const
  SQLITE_INDEX_CONSTRAINT_EQ    = 2;
  SQLITE_INDEX_CONSTRAINT_GT    = 4;
  SQLITE_INDEX_CONSTRAINT_LE    = 8;
  SQLITE_INDEX_CONSTRAINT_LT    = 16;
  SQLITE_INDEX_CONSTRAINT_GE    = 32;
  SQLITE_INDEX_CONSTRAINT_MATCH = 64;

Type Tsqlite3_create_module = function (db: PSQLite3; const zName: PAnsiChar; const p: PSQLite3Module; pClientData: Pointer): Integer; cdecl;
var sqlite3_create_module : Tsqlite3_create_module;

type
  TSQLite3ModuleDestructor = procedure(pAux: Pointer); cdecl;

Type Tsqlite3_create_module_v2 = function (db: PSQLite3; const zName: PAnsiChar; const p: PSQLite3Module; pClientData: Pointer; xDestroy: TSQLite3ModuleDestructor): Integer; cdecl;
var sqlite3_create_module_v2 : Tsqlite3_create_module_v2;

Type Tsqlite3_declare_vtab = function (db: PSQLite3; const zSQL: PAnsiChar): Integer; cdecl;
var sqlite3_declare_vtab : Tsqlite3_declare_vtab;

Type Tsqlite3_overload_function = function (db: PSQLite3; const zFuncName: PAnsiChar; nArg: Integer): Integer; cdecl;
var sqlite3_overload_function : Tsqlite3_overload_function;
{$ENDIF}

type
  PSQLite3Blob = type Pointer;

Type Tsqlite3_blob_open = function (db: PSQLite3; const zDb: PAnsiChar; const zTable: PAnsiChar; const zColumn: PAnsiChar; iRow: Int64; flags: Integer; var ppBlob: PSQLite3Blob): Integer; cdecl;
var sqlite3_blob_open : Tsqlite3_blob_open;

Type Tsqlite3_blob_close = function (pBlob: PSQLite3Blob): Integer; cdecl;
var sqlite3_blob_close : Tsqlite3_blob_close;

Type Tsqlite3_blob_bytes = function (pBlob: PSQLite3Blob): Integer; cdecl;
var sqlite3_blob_bytes : Tsqlite3_blob_bytes;

Type Tsqlite3_blob_read = function (pBlob: PSQLite3Blob; Z: Pointer; N: Integer; iOffset: Integer): Integer; cdecl;
var sqlite3_blob_read : Tsqlite3_blob_read;

Type Tsqlite3_blob_write = function (pBlob: PSQLite3Blob; const z: Pointer; n: Integer; iOffset: Integer): Integer; cdecl;
var sqlite3_blob_write : Tsqlite3_blob_write;

Type Tsqlite3_vfs_find = function (const zVfsName: PAnsiChar): PSQLite3VFS; cdecl;
var sqlite3_vfs_find : Tsqlite3_vfs_find;
Type Tsqlite3_vfs_register = function (pVfs: PSQLite3VFS; makeDflt: Integer): Integer; cdecl;
var sqlite3_vfs_register : Tsqlite3_vfs_register;
Type Tsqlite3_vfs_unregister = function (pVfs: PSQLite3VFS): Integer; cdecl;
var sqlite3_vfs_unregister : Tsqlite3_vfs_unregister;

Type Tsqlite3_mutex_alloc = function (id: Integer): PSQLite3Mutex; cdecl;
var sqlite3_mutex_alloc : Tsqlite3_mutex_alloc;
Type Tsqlite3_mutex_free = procedure (p: PSQLite3Mutex); cdecl;
var sqlite3_mutex_free : Tsqlite3_mutex_free;
Type Tsqlite3_mutex_enter = procedure (p: PSQLite3Mutex); cdecl;
var sqlite3_mutex_enter : Tsqlite3_mutex_enter;
Type Tsqlite3_mutex_try = function (p: PSQLite3Mutex): Integer; cdecl;
var sqlite3_mutex_try : Tsqlite3_mutex_try;
Type Tsqlite3_mutex_leave = procedure (p: PSQLite3Mutex); cdecl;
var sqlite3_mutex_leave : Tsqlite3_mutex_leave;

{$IFDEF SQLITE_EXPERIMENTAL}
type
  sqlite3_mutex_methods = record
    xMutexInit: function: Integer; cdecl;
    xMutexEnd: function: Integer; cdecl;
    xMutexAlloc: function(id: Integer): PSQLite3Mutex; cdecl;
    xMutexFree: procedure(p: PSQLite3Mutex); cdecl;
    xMutexEnter: procedure(p: PSQLite3Mutex); cdecl;
    xMutexTry: function(p: PSQLite3Mutex): Integer; cdecl;
    xMutexLeave: procedure(p: PSQLite3Mutex); cdecl;
    xMutexHeld: function(p: PSQLite3Mutex): Integer; cdecl;
    xMutexNotheld: function(p: PSQLite3Mutex): Integer; cdecl;
  end;
  TSQLite3MutexMethods = sqlite3_mutex_methods;
{$ENDIF}

{$IFDEF SQLITE_DEBUG}
Type Tsqlite3_mutex_held = function (p: PSQLite3Mutex): Integer; cdecl;
var sqlite3_mutex_held : Tsqlite3_mutex_held;
Type Tsqlite3_mutex_notheld = function (p: PSQLite3Mutex): Integer; cdecl;
var sqlite3_mutex_notheld : Tsqlite3_mutex_notheld;
{$ENDIF}

const
  SQLITE_MUTEX_FAST          = 0;
  SQLITE_MUTEX_RECURSIVE     = 1;
  SQLITE_MUTEX_STATIC_MASTER = 2;
  SQLITE_MUTEX_STATIC_MEM    = 3;
  SQLITE_MUTEX_STATIC_MEM2   = 4;
  SQLITE_MUTEX_STATIC_OPEN   = 4;
  SQLITE_MUTEX_STATIC_PRNG   = 5;
  SQLITE_MUTEX_STATIC_LRU    = 6;
  SQLITE_MUTEX_STATIC_LRU2   = 7;

Type Tsqlite3_db_mutex = function (db: PSQLite3): PSQLite3Mutex; cdecl;
var sqlite3_db_mutex : Tsqlite3_db_mutex;

Type Tsqlite3_file_control = function (db: PSQLite3; const zDbName: PAnsiChar; op: Integer; pArg: Pointer): Integer; cdecl;
var sqlite3_file_control : Tsqlite3_file_control;

Type Tsqlite3_test_control = function (op: Integer{; ...}): Integer; cdecl;
var sqlite3_test_control : Tsqlite3_test_control;

const
  SQLITE_TESTCTRL_FIRST               = 5;
  SQLITE_TESTCTRL_PRNG_SAVE           = 5;
  SQLITE_TESTCTRL_PRNG_RESTORE        = 6;
  SQLITE_TESTCTRL_PRNG_RESET          = 7;
  SQLITE_TESTCTRL_BITVEC_TEST         = 8;
  SQLITE_TESTCTRL_FAULT_INSTALL       = 9;
  SQLITE_TESTCTRL_BENIGN_MALLOC_HOOKS = 10;
  SQLITE_TESTCTRL_PENDING_BYTE        = 11;
  SQLITE_TESTCTRL_ASSERT              = 12;
  SQLITE_TESTCTRL_ALWAYS              = 13;
  SQLITE_TESTCTRL_RESERVE             = 14;
  SQLITE_TESTCTRL_OPTIMIZATIONS       = 15;
  SQLITE_TESTCTRL_ISKEYWORD           = 16;
  SQLITE_TESTCTRL_LAST                = 16;

{$IFDEF SQLITE_EXPERIMENTAL}
Type Tsqlite3_status = function (op: Integer; var pCurrent: Integer; var pHighwater: Integer; resetFlag: Integer): Integer; cdecl;
var sqlite3_status : Tsqlite3_status;

const
  SQLITE_STATUS_MEMORY_USED        = 0;
  SQLITE_STATUS_PAGECACHE_USED     = 1;
  SQLITE_STATUS_PAGECACHE_OVERFLOW = 2;
  SQLITE_STATUS_SCRATCH_USED       = 3;
  SQLITE_STATUS_SCRATCH_OVERFLOW   = 4;
  SQLITE_STATUS_MALLOC_SIZE        = 5;
  SQLITE_STATUS_PARSER_STACK       = 6;
  SQLITE_STATUS_PAGECACHE_SIZE     = 7;
  SQLITE_STATUS_SCRATCH_SIZE       = 8;

Type Tsqlite3_db_status = function (db: PSQLite3; op: Integer; var pCur: Integer; var pHiwtr: Integer; resetFlg: Integer): Integer; cdecl;
var sqlite3_db_status : Tsqlite3_db_status;

const
  SQLITE_DBSTATUS_LOOKASIDE_USED = 0;

Type Tsqlite3_stmt_status = function (pStmt: PSQLite3Stmt; op: Integer; resetFlg: Integer): Integer; cdecl;
var sqlite3_stmt_status : ;

const
  SQLITE_STMTSTATUS_FULLSCAN_STEP = 1;
  SQLITE_STMTSTATUS_SORT          = 2;

type
  PSQLite3PCache = type Pointer;

type
  sqlite3_pcache_methods = record
    pArg: Pointer;
    xInit: function(pArg: Pointer): Integer; cdecl;
    xShutdown: procedure(pArg: Pointer); cdecl;
    xCreate: function(szPage: Integer; bPurgeable: Integer): PSQLite3PCache; cdecl;
    xCachesize: procedure(pCache: PSQLite3PCache; nCachesize: Integer); cdecl;
    xPagecount: function(pCache: PSQLite3PCache): Integer; cdecl;
    xFetch: function(pCache: PSQLite3PCache; key: Cardinal; createFlag: Integer): Pointer; cdecl;
    xUnpin: procedure(pCache: PSQLite3PCache; pPg: Pointer; discard: Integer); cdecl;
    xRekey: procedure(pCache: PSQLite3PCache; pPg: Pointer; oldKey: Cardinal; newKey: Cardinal); cdecl;
    xTruncate: procedure(pCache: PSQLite3PCache; iLimit: Cardinal); cdecl;
    xDestroy: procedure(pCache: PSQLite3PCache); cdecl;
  end;
  TSQLite3PCacheMethods = sqlite3_pcache_methods;

type
  PSQLite3Backup = type Pointer;

Type Tsqlite3_backup_init = function (pDest: PSQLite3; const zDestName: PAnsiChar; pSource: PSQLite3; const zSourceName: PAnsiChar): PSQLite3Backup; cdecl;
var sqlite3_backup_init : Tsqlite3_backup_init;
Type Tsqlite3_backup_step = function (p: PSQLite3Backup; nPage: Integer): Integer; cdecl;
var sqlite3_backup_step : Tsqlite3_backup_step;
Type Tsqlite3_backup_finish = function (p: PSQLite3Backup): Integer; cdecl;
var sqlite3_backup_finish : Tsqlite3_backup_finish;
Type Tsqlite3_backup_remaining = function (p: PSQLite3Backup): Integer; cdecl;
var sqlite3_backup_remaining : Tsqlite3_backup_remaining;
Type Tsqlite3_backup_pagecount = function (p: PSQLite3Backup): Integer; cdecl;
var sqlite3_backup_pagecount : Tsqlite3_backup_pagecount;

{$IFDEF SQLITE_ENABLE_UNLOCK_NOTIFY}
type
  TSQLite3UnlockNotifyCallback = procedure(apArg: PPointerArray; nArg: Integer); cdecl;

Type Tsqlite3_unlock_notify = function (pBlocked: PSQLite3; xNotify: TSQLite3UnlockNotifyCallback; pNotifyArg: Pointer): Integer; cdecl;
var sqlite3_unlock_notify : Tsqlite3_unlock_notify;
{$ENDIF}

Type Tsqlite3_strnicmp = function (const zLeft: PAnsiChar; const zRight: PAnsiChar; N: Integer): Integer; cdecl;
var sqlite3_strnicmp : Tsqlite3_strnicmp;
{$ENDIF}

// Type Tsqlite3_win32_mbcs_to_utf8 = function(const S: PAnsiChar): PAnsiChar; cdecl;

procedure loadSQLite;

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF OSX}
  CoreFoundation,
  {$ENDIF}
  SysUtils;

procedure loadSQLite;
var
  lib : TLibHandle;
begin
  lib := loadLibrary(sqlite3_lib);
  if lib = 0 then
    RaiseLastOSError;

  @sqlite3_libversion := GetProcAddress(lib, 'sqlite3_libversion');
  @sqlite3_sourceid := GetProcAddress(lib, 'sqlite3_sourceid');
  @sqlite3_libversion_number := GetProcAddress(lib, 'sqlite3_libversion_number');
  @sqlite3_threadsafe := GetProcAddress(lib, 'sqlite3_threadsafe');
  @sqlite3_close := GetProcAddress(lib, 'sqlite3_close');
  @sqlite3_exec := GetProcAddress(lib, 'sqlite3_exec');
  @sqlite3_initialize := GetProcAddress(lib, 'sqlite3_initialize');
  @sqlite3_shutdown := GetProcAddress(lib, 'sqlite3_shutdown');
  @sqlite3_os_init := GetProcAddress(lib, 'sqlite3_os_init');
  @sqlite3_os_end := GetProcAddress(lib, 'sqlite3_os_end');
  {$IFDEF SQLITE_EXPERIMENTAL}
  @sqlite3_config := GetProcAddress(lib, 'sqlite3_config');
  @sqlite3_db_config := GetProcAddress(lib, 'sqlite3_db_config');
  {$ENDIF}
  @sqlite3_extended_result_codes := GetProcAddress(lib, 'sqlite3_extended_result_codes');
  @sqlite3_last_insert_rowid := GetProcAddress(lib, 'sqlite3_last_insert_rowid');
  @sqlite3_changes := GetProcAddress(lib, 'sqlite3_changes');
  @sqlite3_total_changes := GetProcAddress(lib, 'sqlite3_total_changes');
  @sqlite3_interrupt := GetProcAddress(lib, 'sqlite3_interrupt');
  @sqlite3_complete := GetProcAddress(lib, 'sqlite3_complete');
  @sqlite3_complete16 := GetProcAddress(lib, 'sqlite3_complete16');
  @sqlite3_busy_handler := GetProcAddress(lib, 'sqlite3_busy_handler');
  @sqlite3_busy_timeout := GetProcAddress(lib, 'sqlite3_busy_timeout');
  @sqlite3_get_table := GetProcAddress(lib, 'sqlite3_get_table');
  @sqlite3_free_table := GetProcAddress(lib, 'sqlite3_free_table');
  @sqlite3_mprintf := GetProcAddress(lib, 'sqlite3_mprintf');
  @sqlite3_vmprintf := GetProcAddress(lib, 'sqlite3_vmprintf');
  @sqlite3_snprintf := GetProcAddress(lib, 'sqlite3_snprintf');
  @sqlite3_malloc := GetProcAddress(lib, 'sqlite3_malloc');
  @sqlite3_realloc := GetProcAddress(lib, 'sqlite3_realloc');
  @sqlite3_free := GetProcAddress(lib, 'sqlite3_free');
  @sqlite3_memory_used := GetProcAddress(lib, 'sqlite3_memory_used');
  @sqlite3_memory_highwater := GetProcAddress(lib, 'sqlite3_memory_highwater');
  @sqlite3_randomness := GetProcAddress(lib, 'sqlite3_randomness');
  @sqlite3_set_authorizer := GetProcAddress(lib, 'sqlite3_set_authorizer');
  {$IFDEF SQLITE_EXPERIMENTAL}
  @sqlite3_trace := GetProcAddress(lib, 'sqlite3_trace');
  @sqlite3_profile := GetProcAddress(lib, 'sqlite3_profile');
  {$ENDIF}
  @sqlite3_progress_handler := GetProcAddress(lib, 'sqlite3_progress_handler');
  @sqlite3_open := GetProcAddress(lib, 'sqlite3_open');
  @sqlite3_open16 := GetProcAddress(lib, 'sqlite3_open16');
  @sqlite3_open_v2 := GetProcAddress(lib, 'sqlite3_open_v2');
  @sqlite3_errcode := GetProcAddress(lib, 'sqlite3_errcode');
  @sqlite3_extended_errcode := GetProcAddress(lib, 'sqlite3_extended_errcode');
  @sqlite3_errmsg := GetProcAddress(lib, 'sqlite3_errmsg');
  @sqlite3_errmsg16 := GetProcAddress(lib, 'sqlite3_errmsg16');
  @sqlite3_limit := GetProcAddress(lib, 'sqlite3_limit');
  @sqlite3_prepare := GetProcAddress(lib, 'sqlite3_prepare');
  @sqlite3_prepare_v2 := GetProcAddress(lib, 'sqlite3_prepare_v2');
  @sqlite3_prepare16 := GetProcAddress(lib, 'sqlite3_prepare16');
  @sqlite3_prepare16_v2 := GetProcAddress(lib, 'sqlite3_prepare16_v2');
  @sqlite3_sql := GetProcAddress(lib, 'sqlite3_sql');
  @sqlite3_bind_blob := GetProcAddress(lib, 'sqlite3_bind_blob');
  @sqlite3_bind_double := GetProcAddress(lib, 'sqlite3_bind_double');
  @sqlite3_bind_int := GetProcAddress(lib, 'sqlite3_bind_int');
  @sqlite3_bind_int64 := GetProcAddress(lib, 'sqlite3_bind_int64');
  @sqlite3_bind_null := GetProcAddress(lib, 'sqlite3_bind_null');
  @sqlite3_bind_text := GetProcAddress(lib, 'sqlite3_bind_text');
  @sqlite3_bind_text16 := GetProcAddress(lib, 'sqlite3_bind_text16');
  @sqlite3_bind_value := GetProcAddress(lib, 'sqlite3_bind_value');
  @sqlite3_bind_zeroblob := GetProcAddress(lib, 'sqlite3_bind_zeroblob');
  @sqlite3_bind_parameter_count := GetProcAddress(lib, 'sqlite3_bind_parameter_count');
  @sqlite3_bind_parameter_name := GetProcAddress(lib, 'sqlite3_bind_parameter_name');
  @sqlite3_bind_parameter_index := GetProcAddress(lib, 'sqlite3_bind_parameter_index');
  @sqlite3_clear_bindings := GetProcAddress(lib, 'sqlite3_clear_bindings');
  @sqlite3_column_count := GetProcAddress(lib, 'sqlite3_column_count');
  @sqlite3_column_name := GetProcAddress(lib, 'sqlite3_column_name');
  @sqlite3_column_name16 := GetProcAddress(lib, 'sqlite3_column_name16');
  @sqlite3_column_database_name := GetProcAddress(lib, 'sqlite3_column_database_name');
  @sqlite3_column_database_name16 := GetProcAddress(lib, 'sqlite3_column_database_name16');
  @sqlite3_column_table_name := GetProcAddress(lib, 'sqlite3_column_table_name');
  @sqlite3_column_table_name16 := GetProcAddress(lib, 'sqlite3_column_table_name16');
  @sqlite3_column_origin_name := GetProcAddress(lib, 'sqlite3_column_origin_name');
  @sqlite3_column_origin_name16 := GetProcAddress(lib, 'sqlite3_column_origin_name16');
  @sqlite3_column_decltype := GetProcAddress(lib, 'sqlite3_column_decltype');
  @sqlite3_column_decltype16 := GetProcAddress(lib, 'sqlite3_column_decltype16');
  @sqlite3_step := GetProcAddress(lib, 'sqlite3_step');
  @sqlite3_data_count := GetProcAddress(lib, 'sqlite3_data_count');
  @sqlite3_column_blob := GetProcAddress(lib, 'sqlite3_column_blob');
  @sqlite3_column_bytes := GetProcAddress(lib, 'sqlite3_column_bytes');
  @sqlite3_column_bytes16 := GetProcAddress(lib, 'sqlite3_column_bytes16');
  @sqlite3_column_double := GetProcAddress(lib, 'sqlite3_column_double');
  @sqlite3_column_int := GetProcAddress(lib, 'sqlite3_column_int');
  @sqlite3_column_int64 := GetProcAddress(lib, 'sqlite3_column_int64');
  @sqlite3_column_text := GetProcAddress(lib, 'sqlite3_column_text');
  @sqlite3_column_text16 := GetProcAddress(lib, 'sqlite3_column_text16');
  @sqlite3_column_type := GetProcAddress(lib, 'sqlite3_column_type');
  @sqlite3_column_value := GetProcAddress(lib, 'sqlite3_column_value');
  @sqlite3_finalize := GetProcAddress(lib, 'sqlite3_finalize');
  @sqlite3_reset := GetProcAddress(lib, 'sqlite3_reset');
  @sqlite3_create_function := GetProcAddress(lib, 'sqlite3_create_function');
  @sqlite3_create_function16 := GetProcAddress(lib, 'sqlite3_create_function16');
  {$IFDEF SQLITE_DEPRECATED}
  @sqlite3_aggregate_count := GetProcAddress(lib, 'sqlite3_aggregate_count');
  @sqlite3_expired := GetProcAddress(lib, 'sqlite3_expired');
  @sqlite3_transfer_bindings := GetProcAddress(lib, 'sqlite3_transfer_bindings');
  @sqlite3_global_recover := GetProcAddress(lib, 'sqlite3_global_recover');
  @sqlite3_thread_cleanup := GetProcAddress(lib, 'sqlite3_thread_cleanup');
  @sqlite3_memory_alarm := GetProcAddress(lib, 'sqlite3_memory_alarm');
  {$ENDIF}
  @sqlite3_value_blob := GetProcAddress(lib, 'sqlite3_value_blob');
  @sqlite3_value_bytes := GetProcAddress(lib, 'sqlite3_value_bytes');
  @sqlite3_value_bytes16 := GetProcAddress(lib, 'sqlite3_value_bytes16');
  @sqlite3_value_double := GetProcAddress(lib, 'sqlite3_value_double');
  @sqlite3_value_int := GetProcAddress(lib, 'sqlite3_value_int');
  @sqlite3_value_int64 := GetProcAddress(lib, 'sqlite3_value_int64');
  @sqlite3_value_text := GetProcAddress(lib, 'sqlite3_value_text');
  @sqlite3_value_text16 := GetProcAddress(lib, 'sqlite3_value_text16');
  @sqlite3_value_text16le := GetProcAddress(lib, 'sqlite3_value_text16le');
  @sqlite3_value_text16be := GetProcAddress(lib, 'sqlite3_value_text16be');
  @sqlite3_value_type := GetProcAddress(lib, 'sqlite3_value_type');
  @sqlite3_value_numeric_type := GetProcAddress(lib, 'sqlite3_value_numeric_type');
  @sqlite3_aggregate_context := GetProcAddress(lib, 'sqlite3_aggregate_context');
  @sqlite3_user_data := GetProcAddress(lib, 'sqlite3_user_data');
  @sqlite3_context_db_handle := GetProcAddress(lib, 'sqlite3_context_db_handle');
  @sqlite3_get_auxdata := GetProcAddress(lib, 'sqlite3_get_auxdata');
  @sqlite3_set_auxdata := GetProcAddress(lib, 'sqlite3_set_auxdata');
  @sqlite3_result_blob := GetProcAddress(lib, 'sqlite3_result_blob');
  @sqlite3_result_double := GetProcAddress(lib, 'sqlite3_result_double');
  @sqlite3_result_error := GetProcAddress(lib, 'sqlite3_result_error');
  @sqlite3_result_error16 := GetProcAddress(lib, 'sqlite3_result_error16');
  @sqlite3_result_error_toobig := GetProcAddress(lib, 'sqlite3_result_error_toobig');
  @sqlite3_result_error_nomem := GetProcAddress(lib, 'sqlite3_result_error_nomem');
  @sqlite3_result_error_code := GetProcAddress(lib, 'sqlite3_result_error_code');
  @sqlite3_result_int := GetProcAddress(lib, 'sqlite3_result_int');
  @sqlite3_result_int64 := GetProcAddress(lib, 'sqlite3_result_int64');
  @sqlite3_result_null := GetProcAddress(lib, 'sqlite3_result_null');
  @sqlite3_result_text := GetProcAddress(lib, 'sqlite3_result_text');
  @sqlite3_result_text16 := GetProcAddress(lib, 'sqlite3_result_text16');
  @sqlite3_result_text16le := GetProcAddress(lib, 'sqlite3_result_text16le');
  @sqlite3_result_text16be := GetProcAddress(lib, 'sqlite3_result_text16be');
  @sqlite3_result_value := GetProcAddress(lib, 'sqlite3_result_value');
  @sqlite3_result_zeroblob := GetProcAddress(lib, 'sqlite3_result_zeroblob');
  @sqlite3_create_collation := GetProcAddress(lib, 'sqlite3_create_collation');
  @sqlite3_create_collation_v2 := GetProcAddress(lib, 'sqlite3_create_collation_v2');
  @sqlite3_create_collation16 := GetProcAddress(lib, 'sqlite3_create_collation16');
  @sqlite3_collation_needed := GetProcAddress(lib, 'sqlite3_collation_needed');
  @sqlite3_collation_needed16 := GetProcAddress(lib, 'sqlite3_collation_needed16');
  @sqlite3_sleep := GetProcAddress(lib, 'sqlite3_sleep');
  @sqlite3_get_autocommit := GetProcAddress(lib, 'sqlite3_get_autocommit');
  @sqlite3_db_handle := GetProcAddress(lib, 'sqlite3_db_handle');
  @sqlite3_next_stmt := GetProcAddress(lib, 'sqlite3_next_stmt');
  @sqlite3_commit_hook := GetProcAddress(lib, 'sqlite3_commit_hook');
  @sqlite3_rollback_hook := GetProcAddress(lib, 'sqlite3_rollback_hook');
  @sqlite3_update_hook := GetProcAddress(lib, 'sqlite3_update_hook');
  @sqlite3_enable_shared_cache := GetProcAddress(lib, 'sqlite3_enable_shared_cache');
  @sqlite3_release_memory := GetProcAddress(lib, 'sqlite3_release_memory');
  @sqlite3_soft_heap_limit := GetProcAddress(lib, 'sqlite3_soft_heap_limit');
  @sqlite3_table_column_metadata := GetProcAddress(lib, 'sqlite3_table_column_metadata');
  @sqlite3_load_extension := GetProcAddress(lib, 'sqlite3_load_extension');
  @sqlite3_enable_load_extension := GetProcAddress(lib, 'sqlite3_enable_load_extension');
  @sqlite3_auto_extension := GetProcAddress(lib, 'sqlite3_auto_extension');
  @sqlite3_reset_auto_extension := GetProcAddress(lib, 'sqlite3_reset_auto_extension');
  {$IFDEF SQLITE_EXPERIMENTAL}
  @sqlite3_create_module := GetProcAddress(lib, 'sqlite3_create_module');
  @sqlite3_create_module_v2 := GetProcAddress(lib, 'sqlite3_create_module_v2');
  @sqlite3_declare_vtab := GetProcAddress(lib, 'sqlite3_declare_vtab');
  @sqlite3_overload_function := GetProcAddress(lib, 'sqlite3_overload_function');
  {$ENDIF}
  @sqlite3_blob_open := GetProcAddress(lib, 'sqlite3_blob_open');
  @sqlite3_blob_close := GetProcAddress(lib, 'sqlite3_blob_close');
  @sqlite3_blob_bytes := GetProcAddress(lib, 'sqlite3_blob_bytes');
  @sqlite3_blob_read := GetProcAddress(lib, 'sqlite3_blob_read');
  @sqlite3_blob_write := GetProcAddress(lib, 'sqlite3_blob_write');
  @sqlite3_vfs_find := GetProcAddress(lib, 'sqlite3_vfs_find');
  @sqlite3_vfs_register := GetProcAddress(lib, 'sqlite3_vfs_register');
  @sqlite3_vfs_unregister := GetProcAddress(lib, 'sqlite3_vfs_unregister');
  @sqlite3_mutex_alloc := GetProcAddress(lib, 'sqlite3_mutex_alloc');
  @sqlite3_mutex_free := GetProcAddress(lib, 'sqlite3_mutex_free');
  @sqlite3_mutex_enter := GetProcAddress(lib, 'sqlite3_mutex_enter');
  @sqlite3_mutex_try := GetProcAddress(lib, 'sqlite3_mutex_try');
  @sqlite3_mutex_leave := GetProcAddress(lib, 'sqlite3_mutex_leave');
  {$IFDEF SQLITE_DEBUG}
  @sqlite3_mutex_held := GetProcAddress(lib, 'sqlite3_mutex_held');
  @sqlite3_mutex_notheld := GetProcAddress(lib, 'sqlite3_mutex_notheld');
  {$ENDIF}
  @sqlite3_db_mutex := GetProcAddress(lib, 'sqlite3_db_mutex');
  @sqlite3_file_control := GetProcAddress(lib, 'sqlite3_file_control');
  @sqlite3_test_control := GetProcAddress(lib, 'sqlite3_test_control');
  {$IFDEF SQLITE_EXPERIMENTAL}
  @sqlite3_status := GetProcAddress(lib, 'sqlite3_status');
  @sqlite3_db_status := GetProcAddress(lib, 'sqlite3_db_status');
  @sqlite3_stmt_status := GetProcAddress(lib, 'sqlite3_stmt_status');
  @sqlite3_backup_init := GetProcAddress(lib, 'sqlite3_backup_init');
  @sqlite3_backup_step := GetProcAddress(lib, 'sqlite3_backup_step');
  @sqlite3_backup_finish := GetProcAddress(lib, 'sqlite3_backup_finish');
  @sqlite3_backup_remaining := GetProcAddress(lib, 'sqlite3_backup_remaining');
  @sqlite3_backup_pagecount := GetProcAddress(lib, 'sqlite3_backup_pagecount');
  @sqlite3_unlock_notify := GetProcAddress(lib, 'sqlite3_unlock_notify');
  @sqlite3_strnicmp := GetProcAddress(lib, 'sqlite3_strnicmp');
  {$ENDIF}
end;

end.
