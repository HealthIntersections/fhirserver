unit fdb_tests;

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

Uses
  Sysutils, Classes,
  fsl_testing, fsl_logging, fsl_base, fsl_utilities, fsl_stream,
  fdb_dialects,
  fdb_manager, fdb_odbc, {$IFDEF FPC}fdb_fpc, {$ENDIF} fdb_sqlite3, fdb_sqlite3_objects, fdb_sqlite3_wrapper;

Type

  { TFDBTests }

  TFDBTests = Class (TFslTestCase)
  private
    conn4: TFDBConnection;
    procedure TestThread(context : TObject);
    procedure test(manager: TFDBManager);
  Published
    procedure TestSemaphore;
//    procedure TestODBC;

    {$IFDEF DELPHI}
    procedure TestMSSQL;
    {$ENDIF}

    procedure TestMySQL;
    // procedure TestMySQLMaria;
    procedure TestSQLite;
  End;

procedure registerTests;

implementation

{$IFDEF DELPHI}
uses
  fdb_odbc_headers, fdb_odbc_objects;
{$ENDIF}
const
  Name_405 = 'asdasd askjhf asdjfh sif hksdfh skdjfh sdf askjhas dak akdh ajksdh akjsdh askjd hakjsdh aksdh aksjdh aksjdh asdajksdh askd ajksdha askd ajksdh askjdh aksjdh aksjdh asjkdh askjd haskjdh askdhj asskajhd aksjdhaksjd '+'aksdh askjdh kajsdh aksjhd askjdh akjsdh kajsdh akjshdak jshd akjsdh aksjdh akjshdkajsdh akjsdhk ajshd akjsdhaj kshd akjshd asjkdhasjk d akjdh askjdh askjdh askjdh akjsdhakjsdh akjsdh aksjdh';

type
  ETestException = class(EFslException);

function BlobIsSame(l, r: TBytes): boolean;
var
  i: integer;
begin
  if length(l) <> length(r) then
    exit(false);
  result := true;
  for i := 0 to length(l) - 1 do
    if l[i] <> r[i] then
      exit(false);
end;

function isSame(l, r: Double): boolean;
var
  d: Double;
begin
  d := abs(l - r);
  result := d < 0.00001;
end;

{ TFDBTests }

procedure TFDBTests.test(manager: TFDBManager);
var
  conn: TFDBConnection;
  d, od, dn: TFslDateTime;
  b: TBytes;
  i64: Int64;
  md: TFDBMetaData;
  fn : string;
begin
  d := TFslDateTime.makeLocal(dtpSec);
  fn := TestSettings.serverTestFile(['library', 'fdb', 'tests', 'fdb_tests.pas']);
  b := FileToBytes(fn);
  i64 := MaxInt;
  i64 := i64 + 2;

  conn := manager.GetConnection('test');
  try
    conn.ExecSQL('CREATE TABLE TestTable ( ' + #13#10 + ' TestKey ' + DBKeyType(conn.owner.platform) + ' ' + ColCanBeNull(conn.owner.platform, false) + ', ' +
      #13#10 + ' Name nchar(255) ' + ColCanBeNull(conn.owner.platform, false) + ', ' + #13#10 + ' Number int ' + ColCanBeNull(conn.owner.platform, true) + ', '
      + #13#10 + ' BigNumber bigint ' + ColCanBeNull(conn.owner.platform, true) + ', ' + #13#10 + ' FloatNumber float ' + ColCanBeNull(conn.owner.platform,
      true) + ', ' + #13#10 + ' Instant ' + DBDateTimeType(conn.owner.platform) + ' ' + ColCanBeNull(conn.owner.platform, false) + ', ' + #13#10 + ' BigString text ' + ColCanBeNull(conn.owner.platform, false) + ', ' + #13#10 + ' Content ' +
      DBBlobType(conn.owner.platform) + ' ' + ColCanBeNull(conn.owner.platform, true) + ', ' + #13#10 + PrimaryKeyType(conn.owner.platform, 'PK_TestTable',
      'TestKey') + ') ' + CreateTableInfo(conn.owner.platform));
    conn.ExecSQL('Create Unique INDEX SK_TestTable_Index ON TestTable (Name, Number)');

    try
      if {$IFDEF LINUX} conn.Owner.Platform <> kdbMySQL {$ELSE} true {$ENDIF} then // blows up with function sequence error with mysql on linux?
      begin
        md := conn.FetchMetaData;
        try
          assertTrue(md.HasTable('TestTable'))
        finally
          md.free;
        end;
      end;

      assertTrue(conn.CountSQL('Select count(*) from TestTable') = 0, 'dbt.0');

       conn.ExecSQL('Insert into TestTable (TestKey, Name, BigString, Number, BigNumber, FloatNumber, Instant) values (1, ''a name'', '''', 2, ' + IntToStr(i64) + ', 3.2, ' +
        DBGetDate(manager.platform) + ')');
      conn.sql := 'Insert into TestTable (TestKey, Name, BigString, Number, BigNumber, FloatNumber, Instant, Content) values (:k, :n, :bs, :i, :bi, :d, :ts, :c)';
      conn.Prepare;
      conn.BindKey('k', 2);
      conn.BindString('n', 'Name 2');
      conn.BindString('bs', Name_405);
      conn.BindInteger('i', 3);
      conn.BindInt64('bi', -i64);
      conn.BindDouble('d', 3.4);
      conn.BindDateTimeEx('ts', d);
      conn.BindBlob('c', b);
      conn.Execute;
      conn.Terminate;

      assertTrue(conn.CountSQL('Select count(*) from TestTable where  TestKey = 1') = 1, 'dbt.1');
      assertTrue(conn.CountSQL('Select count(*) from TestTable where  TestKey = 0') = 0, 'dbt.2');

      dn := TFslDateTime.makeLocal;

      conn.sql := 'Select * from TestTable';
      conn.Prepare;
      conn.Execute;
      conn.FetchNext;
      assertTrue(conn.ColIntegerByName['TestKey'] = 1, 'dbt.3');
      assertTrue(conn.ColStringByName['Name'] = 'a name', 'dbt.4');
      assertTrue(conn.ColBlobAsStringByName['BigString'] = '', 'dbt.5');
      assertTrue(conn.ColIntegerByName['Number'] = 2, 'dbt.6');
      assertTrue(conn.ColInt64ByName['BigNumber'] = i64, 'dbt.7');
      assertTrue(isSame(conn.ColDoubleByName['FloatNumber'], 3.2), 'dbt.8');
      assertTrue(TSToDateTime(conn.ColTimestampByName['Instant']) < dn.DateTime, 'dbt.9');
      assertTrue(TSToDateTime(conn.ColTimestampByName['Instant']) > dn.DateTime - DATETIME_MINUTE_ONE,  'dbt.10: '+TSToString(conn.ColTimestampByName['Instant'])+' vs '+FormatDateTime('yyyy-mm-dd''T''hh:nn:ss.zzz', now - DATETIME_MINUTE_ONE));
      od := conn.ColDateTimeExByName['Instant'];
      assertTrue(length(conn.ColBlobByName['Content']) = 0, 'dbt.11');
      assertTrue(conn.ColNullByName['Content'], 'dbt.12');

      conn.FetchNext;
      assertTrue(conn.ColIntegerByName['TestKey'] = 2, 'dbt.13');
      assertTrue(conn.ColStringByName['Name'] = 'Name 2', 'dbt.14');
      assertTrue(conn.ColBlobAsStringByName['BigString'] = Name_405, 'dbt.15');
      assertTrue(conn.ColIntegerByName['Number'] = 3, 'dbt.16');
      assertTrue(conn.ColInt64ByName['BigNumber'] = -i64, 'dbt.17');
      assertTrue(isSame(conn.ColDoubleByName['FloatNumber'], 3.4), 'dbt.18');
      assertTrue(conn.ColDateTimeExByName['Instant'].equal(d, dtpSec), 'dbt.19');
      assertTrue(BlobIsSame(conn.ColBlobByName['Content'], b), 'dbt.20');
      conn.Terminate;

      sleep(1000);

      conn.ExecSQL('Update TestTable set Name = ''3rd Name'', Number = 3, BigNumber = ' + IntToStr(-i64) + ', FloatNumber = 3.1, Instant = ' +
        DBGetDate(manager.platform) + ' where TestKey = 1');
      conn.sql := 'Update TestTable set Name = :n, Number = :i, BigNumber = :bi, FloatNumber = :d, Instant = :ts, Content = :c where TestKey = :k';
      conn.Prepare;
      conn.BindKey('k', 2);
      conn.BindString('n', 'Name 4');
      conn.BindInteger('i', -4);
      conn.BindInt64('bi', i64);
      conn.BindDouble('d', 3.01);
      conn.BindDateTimeEx('ts', od);
      conn.BindNull('c');
      conn.Execute;
      conn.Terminate;

      conn.sql := 'Select * from TestTable';
      conn.Prepare;
      conn.Execute;
      conn.FetchNext;
      assertTrue(conn.ColIntegerByName['TestKey'] = 1, 'dbt.21');
      assertTrue(conn.ColStringByName['Name'] = '3rd Name', 'dbt.22');
      assertTrue(conn.ColIntegerByName['Number'] = 3, 'dbt.23');
      assertTrue(conn.ColInt64ByName['BigNumber'] = -i64, 'dbt.24');
      assertTrue(isSame(conn.ColDoubleByName['FloatNumber'], 3.1), 'dbt.25');
      assertTrue(conn.ColDateTimeExByName['Instant'].after(od, false), 'dbt.26');
      assertTrue(length(conn.ColBlobByName['Content']) = 0, 'dbt.27');
      assertTrue(conn.ColNullByName['Content'], 'dbt.28');

      conn.FetchNext;
      assertTrue(conn.ColIntegerByName['TestKey'] = 2, 'dbt.29');
      assertTrue(conn.ColStringByName['Name'] = 'Name 4', 'dbt.30');
      assertTrue(conn.ColIntegerByName['Number'] = -4, 'dbt.31');
      assertTrue(conn.ColInt64ByName['BigNumber'] = i64, 'dbt.32');
      assertTrue(isSame(conn.ColDoubleByName['FloatNumber'], 3.01), 'dbt.33');
      assertTrue(conn.ColDateTimeExByName['Instant'].equal(od), 'dbt.34');
      assertTrue(length(conn.ColBlobByName['Content']) = 0, 'dbt.35');
      assertTrue(conn.ColNullByName['Content'], 'dbt.36');
      conn.Terminate;

      conn.ExecSQL('Delete from TestTable where TestKey = 1');
      conn.sql := 'Delete from TestTable where TestKey = :k';
      conn.Prepare;
      conn.BindKey('k', 2);
      conn.Execute;
      conn.Terminate;

      assertTrue(conn.CountSQL('Select count(*) from TestTable') = 0, 'dbt.37');

    finally
      conn.terminate;
      conn.DropTable('TestTable');
    end;
    if {$IFDEF LINUX} conn.Owner.Platform <> kdbMySQL {$ELSE} true {$ENDIF} then
    begin
      md := conn.FetchMetaData;
      try
        assertFalse(md.HasTable('TestTable'), 'dbt.38')
      finally
        md.free;
      end;
    end;

    conn.Release;
  except
    on e: Exception do
    begin
      conn.Error(e);
      assertTrue(false, e.message);
    end;
  end;
end;

// docker run -d --name mssql-server --platform linux/arm64/v8 -e ACCEPT_EULA=Y -e SA_PASSWORD={pwd} -p 1433:1433 mcr.microsoft.com/azure-sql-edge

{$IFDEF DELPHI}
procedure TFDBTests.TestMSSQL;
var
  db: TFDBManager;
  settings : TFslStringMap;
begin
  if not TestSettings.hasSection('mssql') then
  begin
//    Logging.log('ignore mssql test - no settings');
    assertNotTested('MSSQL not configured');
  end
  else
  begin
    settings := TestSettings.section('mssql');
    try
//      Logging.log('test mssql: '+settings['server']+'/'+settings['database']+'@'+settings['username']+':'+StringPadLeft('', 'X', settings['password'].length));
      db := TFDBOdbcManager.Create('test', kdbSqlServer, 8, 200, settings);
      try
        test(db);
      finally
        db.free;
      end;
    finally
      settings.free;
    end;
  end;
end;
{$ENDIF}

procedure TFDBTests.TestMySQL;
var
  db: TFDBManager;
  settings : TFslStringMap;
begin
  if not TestSettings.hasSection('mysql') then
  begin
//    Logging.log('ignore mysql test - no settings');
    assertNotTested('MySQL not configured');
  end
  else
  begin
    settings := TestSettings.section('mysql');
    try
//      Logging.log('test mysql: '+settings['server']+'/'+settings['database']+'@'+settings['username']+':'+StringPadLeft('', 'X', settings['password'].length));
      {$IFDEF FPC}
      db := TFDBOdbcManager.Create('test', kdbMySql, 8, 200, settings['driver'], settings['server'], settings['database'], settings['username'], settings['password']);
//      db := TFDBSQLDBManager.Create('test', kdbMySQL, settings['server'], settings['database'], settings['username'], settings['password'], 100);
      {$ELSE}
      db := TFDBOdbcManager.Create('test', kdbMySql, 8, 200, settings);
      {$ENDIF}
      try
        test(db);
      finally
        db.free;
      end;
    finally
      settings.free;
    end;
  end;
end;

// procedure TFDBTests.testSQLite;
// var
// DB: TSQLite3Database;
// Stmt: TSQLite3Statement;
// IDs: array[1..6] of Integer;
// begin
// // Delete database if it already exists
// DeleteFile('artists.db');
//
// // Create database and fill it with example data
// DB := TSQLite3Database.Create;
// try
// DB.Open('artists.db');
//
// // Create table "artists"
// DB.Execute('CREATE TABLE artists (name TEXT, born REAL, died REAL)');
//
// // Fill the table with artists
// Stmt := DB.Prepare('INSERT INTO artists (name, born, died) VALUES (:p1, :p2, :p3)');
// try
// Stmt.BindText  (':p1', 'Leonardo da Vinci');
// Stmt.BindDouble(':p2', EncodeDate(1452, 4, 15));
// Stmt.BindDouble(':p3', EncodeDate(1519, 5, 2));
//
// Stmt.StepAndReset; // StepAndReset executes a prepared statement
// // and resets it so we can reuse it again
//
// IDs[1] := DB.LastInsertRowID; // Save newly added artist's ID to use it
// // when filling "paintings" table below
//
// Stmt.BindText  (1, 'Raphael');
// Stmt.BindDouble(2, EncodeDate(1483, 3, 28));
// Stmt.BindDouble(3, EncodeDate(1520, 4, 6));
// Stmt.StepAndReset;
// IDs[2] := DB.LastInsertRowID;
//
// Stmt.BindText  (1, 'Arkhip Kuindzhi');
// Stmt.BindDouble(2, EncodeDate(1842, 1, 27));
// Stmt.BindDouble(3, EncodeDate(1898, 7, 24));
// Stmt.StepAndReset;
// IDs[3] := DB.LastInsertRowID;
//
// Stmt.BindText  (1, 'Nicholas Roerich');
// Stmt.BindDouble(2, EncodeDate(1874, 10, 9));
// Stmt.BindDouble(3, EncodeDate(1947, 12, 13));
// Stmt.StepAndReset;
// IDs[4] := DB.LastInsertRowID;
//
// Stmt.BindText  (1, 'Ivan Aivazovsky');
// Stmt.BindDouble(2, EncodeDate(1817, 7, 29));
// Stmt.BindDouble(3, EncodeDate(1900, 5, 5));
// Stmt.StepAndReset;
// IDs[5] := DB.LastInsertRowID;
//
// Stmt.BindText  (1, 'Ivan Shishkin');
// Stmt.BindDouble(2, EncodeDate(1832, 1, 25));
// Stmt.BindDouble(3, EncodeDate(1898, 3, 20));
// Stmt.StepAndReset;
// IDs[6] := DB.LastInsertRowID;
// finally
// Stmt.free;
// end;
//
// // Create table "paintings"
// DB.Execute('CREATE TABLE paintings (title TEXT, year INTEGER, artist INTEGER)');
//
// // Fill the table with paintings info
// Stmt := DB.Prepare('INSERT INTO paintings (title, year, artist) VALUES (?, ?, ?)');
// try
// // Leonardo da Vinci
// Stmt.BindText(1, 'The Virgin and Child with St. Anne');
// Stmt.BindInt (2, 1508);
// Stmt.BindInt (3, IDs[1]);
// Stmt.StepAndReset;
//
// Stmt.BindText(1, 'Mona Lisa');
// Stmt.BindInt (2, 1519);
// Stmt.BindInt (3, IDs[1]);
// Stmt.StepAndReset;
//
// // Raphael
// Stmt.BindText(1, 'Sistine Madonna');
// Stmt.BindInt (2, 1514);
// Stmt.BindInt (3, IDs[2]);
// Stmt.StepAndReset;
//
// Stmt.BindText(1, 'Transfiguration');
// Stmt.BindInt (2, 1520);
// Stmt.BindInt (3, IDs[2]);
// Stmt.StepAndReset;
//
// // Arkhip Kuindzhi
// Stmt.BindText(1, 'After a rain');
// Stmt.BindInt (2, 1879);
// Stmt.BindInt (3, IDs[3]);
// Stmt.StepAndReset;
//
// Stmt.BindText(1, 'Elbrus');
// Stmt.BindInt (2, 1895);
// Stmt.BindInt (3, IDs[3]);
// Stmt.StepAndReset;
//
// // Nicholas Roerich
// Stmt.BindText(1, 'To Kailas. Lahul');
// Stmt.BindInt (2, 1932);
// Stmt.BindInt (3, IDs[4]);
// Stmt.StepAndReset;
//
// Stmt.BindText(1, 'Krishna');
// Stmt.BindInt (2, 1929);
// Stmt.BindInt (3, IDs[4]);
// Stmt.StepAndReset;
//
// // Ivan Aivazovsky
// Stmt.BindText(1, 'The Mary Caught in a Storm');
// Stmt.BindInt (2, 1892);
// Stmt.BindInt (3, IDs[5]);
// Stmt.StepAndReset;
//
// Stmt.BindText(1, 'Brig "Mercury" Attacked by Two Turkish Ships');
// Stmt.BindInt (2, 1892);
// Stmt.BindInt (3, IDs[5]);
// Stmt.StepAndReset;
//
// // Ivan Shishkin
// Stmt.BindText(1, 'Morning in a Pine Forest');
// Stmt.BindInt (2, 1889);
// Stmt.BindInt (3, IDs[6]);
// Stmt.StepAndReset;
//
// Stmt.BindText(1, 'Wood Distances');
// Stmt.BindInt (2, 1884);
// Stmt.BindInt (3, IDs[6]);
// Stmt.StepAndReset;
// finally
// Stmt.free;
// end;
//
// finally
// DB.free;
// end;

// end;
//

procedure TFDBTests.TestThread(context : TObject);
begin
  sleep(500);
  conn4.Release;
end;

procedure TFDBTests.TestSemaphore;
var
  db: TFDBManager;
  conn1: TFDBConnection;
  conn2: TFDBConnection;
  conn3: TFDBConnection;
  conn5: TFDBConnection;
  fn  : String;
begin
  fn := filePath(['[tmp]', 'sql.db']);
  if FileExists(fn) then
  //begin
  //  Logging.log('SQLite DB @ '+fn+': delete');
    DeleteFile(fn);
  //end
  //else
  //  Logging.log('SQLite DB @ '+fn);
  db := TFDBSQLiteManager.Create('test', fn, false, true, 4);
  try
    assertTrue(db.CurrConnCount = 0);
    conn1 := db.GetConnection('test1');
    try
      assertTrue(db.CurrConnCount = 1);
      conn2 := db.GetConnection('test2');
      try
        assertTrue(db.CurrConnCount = 2);
        conn3 := db.GetConnection('test3');
        try
          assertTrue(db.CurrConnCount = 3);
          assertTrue(db.CurrUseCount = 3);
          conn4 := db.GetConnection('test4');
          try
            try
              db.GetConnection('test');
              assertTrue(false);
            except
              assertTrue(true);
            end;
            conn4.Release;
          except
            on e: Exception do
            begin
              conn4.Error(e);
              raise;
            end;
          end;
          conn4 := db.GetConnection('test4');
          thread(testThread, nil);
          conn5 := db.GetConnection('test');
          try
            assertTrue(db.CurrConnCount = 4);
            assertTrue(db.CurrUseCount = 4);
            conn5.Release;
          except
            on e: Exception do
            begin
              conn5.Error(e);
              raise;
            end;
          end;
          conn3.Release;
        except
          on e: Exception do
          begin
            conn3.Error(e);
            raise;
          end;
        end;
        conn2.Release;
      except
        on e: Exception do
        begin
          conn2.Error(e);
          raise;
        end;
      end;
      conn1.Release;
    except
      on e: Exception do
      begin
        conn1.Error(e);
        raise;
      end;
    end;
    assertTrue(db.CurrConnCount = 4);
  finally
    db.free;
  end;
end;

const
  DefaultStringSize = 255;

function makePChar(len : integer) : pchar;
begin
  {$IFDEF FPC}
  result := Getmem(len);
  {$ELSE}
  Getmem(result, len);
  {$ENDIF}
end;

function fromPChar(p : PChar; length : integer) : String;
begin
  result := p;
end;


{$IFNDEF FPC}
Function odbcError(ARetCode: SQLRETURN; aHandleType: SQLSMALLINT; aHandle: SQLHANDLE): String;
Var
  ErrorNum: Integer;

  RetCode: SQLRETURN;
  State: PChar;
  Native: SQLINTEGER;
  Message: PChar;
  StringLength: SQLSMALLINT;
Begin
  State := makePChar(DefaultStringSize);
  Message := makePChar(DefaultStringSize);
  try
    Result := '';

    Case ARetCode Of
      SQL_ERROR, -24238,
      SQL_SUCCESS_WITH_INFO:
      Begin
        ErrorNum := 0;
        Repeat
          Inc(ErrorNum);

          RetCode := SQLGetDiagRec(aHandleType, aHandle, ErrorNum, State, Native, Message, DefaultStringSize, StringLength);
          If RetCode = SQL_SUCCESS Then
            CommaAdd(result, fromPChar(State, 5)+': '+fromPChar(Message, StringLength));
        Until RetCode <> SQL_SUCCESS;
        If (Result = '') Or (RetCode <> SQL_NO_DATA) Then
          result := 'Unable to Retrieve ODBC Error';
      End;
      Else
      Begin
        Case ARetCode Of
          SQL_INVALID_HANDLE:
            result := 'Invalid ODBC Handle';
          SQL_NO_DATA:
            result := 'No Data Found';
          Else
            result := 'ODBC Return Code '+IntToStr(ARetCode);
        End;
      End;
    End;
  finally
    freemem(state);
    freemem(message);
  end;
End;

//procedure TFDBTests.TestODBC;
//  procedure check(retValue : integer; op : String; aHandleType: SQLSMALLINT; aHandle: SQLHANDLE);
//  begin
//    if (retValue <> 0) then
//      raise ELibraryException.Create('return value from '+op+' = '+inttostr(retValue)+': '+odbcError(retValue, aHandleType, aHandle));
//  end;
//var
//  env : SQLHENV;
//  dbc : SQLHDBC;
//  stmt : SQLHSTMT;
//  cs, sql : String;
//  co : pchar;
//  l : smallint;
//  np : SQLUINTEGER;
//  srvr, uid, db, pwd, drvr : String;
//begin
//  drvr := TestSettings['mysql', 'driver'];
//  srvr := TestSettings['mysql', 'server'];
//  db := TestSettings['mysql', 'database'];
//  uid := TestSettings['mysql', 'username'];
//  pwd := TestSettings['mysql', 'password'];
//
//  {$IFDEF FPC}
//  if not isODBCLoaded then
//    InitialiseODBC;
//  {$ENDIF}
//
//  check(SQLAllocHandle(SQL_HANDLE_ENV, Pointer(SQL_NULL_HANDLE), env), 'SQLAllocHandle', SQL_HANDLE_ENV, env);
//  check(SQLSetEnvAttr(env, SQL_ATTR_ODBC_VERSION, Pointer(SQL_OV_ODBC3), 0), 'SQLSetEnvAttr', SQL_HANDLE_ENV, env);
//  check(SQLAllocHandle(SQL_HANDLE_DBC, env, dbc), 'SQLSetEnvAttr', SQL_HANDLE_DBC, dbc);
//  cs := 'UID='+uid+';PWD='+pwd+';DRIVER='+drvr+';Server='+srvr+';Database='+db+';';
//  co := makePChar(DefaultStringSize);
//  try
//    check(SQLDriverConnect(dbc, 0, pchar(cs), SQL_NTS, co, DefaultStringSize, l, SQL_DRIVER_NOPROMPT), 'SQLDriverConnect', SQL_HANDLE_DBC, dbc);
//  finally
//    freemem(co);
//  end;
//  check(SQLAllocHandle(SQL_HANDLE_STMT, dbc, stmt), 'SQLAllocHandle', SQL_HANDLE_DBC, dbc);
//  sql := 'SET time_zone = ''+11:00''';
//  check(SQLPrepare(stmt, pchar(sql), SQL_NTS), 'SQLPrepare', SQL_HANDLE_STMT, stmt);
//  // this line bloews up mysql
//  //np := 0;
//  //check(SQLSetStmtAttr(stmt, SQL_ATTR_PARAMSET_SIZE, pointer(np), sizeof(np)), 'SQLPrepare', SQL_HANDLE_STMT, stmt);
//  check(SQLExecDirect(stmt, pchar(sql), SQL_NTS), 'SQLExecDirect', SQL_HANDLE_STMT, stmt);
//  check(SQLFreeHandle(SQL_HANDLE_STMT, stmt), 'SQLFreeHandle', SQL_HANDLE_STMT, stmt);
//  check(SQLDisconnect(dbc), 'SQLDisconnect', SQL_HANDLE_STMT, stmt);
//  check(SQLFreeHandle(SQL_HANDLE_DBC, dbc), 'SQLFreeHandle', SQL_HANDLE_DBC, dbc);
//  check(SQLFreeHandle(SQL_HANDLE_ENV, env), 'SQLFreeHandle', SQL_HANDLE_ENV, env);
//  assertTrue(true); // get to here, success
//end;
{$ENDIF}

procedure TFDBTests.TestSQLite;
var
  db : TFDBManager;
  fn : String;
begin
  fn := filePath(['[tmp]', 'sql.db']);
  if FileExists(fn) then
  //begin
  //  Logging.log('SQLite DB @ '+fn+': delete');
    DeleteFile(fn);
  //end
  //else
  //  Logging.log('SQLite DB @ '+fn);
  db := TFDBSQLiteManager.Create('test', fn, false, true, 4);
  try
    test(db);
  finally
    db.free;
  end;
end;

// procedure TFDBTests.TestMySQLMaria;
// var
// db : TFDBManager;
// begin
// db := TFDBOdbcManager.Create('test', 8, 0, 'MariaDB ODBC 3.0 Driver', 'localhost', 'test', 'root', 'h_Erp3ChJ![~C7~oL|61');
// try
// test(db);
// finally
// db.free;
// end;
// end;
//

procedure registerTests;
// don't use initialization - give other code time to set up directories etc
begin
  RegisterTest('Database.Manager', TFDBTests.Suite);
end;

end.
