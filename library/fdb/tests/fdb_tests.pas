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
  fsl_testing,

  fsl_base, fsl_utilities, fsl_stream,
  fdb_dialects,
  fdb_manager, fdb_odbc, fdb_sqlite3, fdb_sqlite3_objects, fdb_sqlite3_wrapper;

Type
  TFDBTests = Class (TFslTestCase)
  private
    conn4: TFDBConnection;
    procedure TestThread;
    procedure test(manager: TFDBManager);
  Published
    procedure TestSemaphore;
    procedure TestMSSQL;
    procedure TestMySQL;
    // procedure TestMySQLMaria;
    procedure TestSQLite;
  End;

procedure registerTests;

implementation

const
  Name_405 = 'asdasd askjhf asdjfh sif hksdfh skdjfh sdf askjhas dak akdh ajksdh akjsdh askjd hakjsdh aksdh aksjdh aksjdh asdajksdh askd ajksdha askd ajksdh askjdh aksjdh aksjdh asjkdh askjd haskjdh askdhj asskajhd aksjdhaksjd '+'aksdh askjdh kajsdh aksjhd askjdh akjsdh kajsdh akjshdak jshd akjsdh aksjdh akjshdkajsdh akjsdhk ajshd akjsdhaj kshd akjshd asjkdhasjk d akjdh askjdh askjdh askjdh akjsdhakjsdh akjsdh aksjdh';

type
  ETestException = class(Exception);

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
    md := conn.FetchMetaData;
    try
      if md.HasTable('TestTable') then
        conn.DropTable('TestTable');
    finally
      md.Free;
    end;

    conn.ExecSQL('CREATE TABLE TestTable ( ' + #13#10 + ' TestKey ' + DBKeyType(conn.owner.platform) + ' ' + ColCanBeNull(conn.owner.platform, false) + ', ' +
      #13#10 + ' Name nchar(255) ' + ColCanBeNull(conn.owner.platform, false) + ', ' + #13#10 + ' Number int ' + ColCanBeNull(conn.owner.platform, true) + ', '
      + #13#10 + ' BigNumber bigint ' + ColCanBeNull(conn.owner.platform, true) + ', ' + #13#10 + ' FloatNumber float ' + ColCanBeNull(conn.owner.platform,
      true) + ', ' + #13#10 + ' Instant ' + DBDateTimeType(conn.owner.platform) + ' ' + ColCanBeNull(conn.owner.platform, false) + ', ' + #13#10 + ' BigString text ' + ColCanBeNull(conn.owner.platform, false) + ', ' + #13#10 + ' Content ' +
      DBBlobType(conn.owner.platform) + ' ' + ColCanBeNull(conn.owner.platform, true) + ', ' + #13#10 + PrimaryKeyType(conn.owner.platform, 'PK_TestTable',
      'TestKey') + ') ' + CreateTableInfo(conn.owner.platform));
    conn.ExecSQL('Create Unique INDEX SK_TestTable_Index ON TestTable (Name, Number)');

    try
      md := conn.FetchMetaData;
      try
        assertTrue(md.HasTable('TestTable'))
      finally
        md.Free;
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
    md := conn.FetchMetaData;
    try
      assertFalse(md.HasTable('TestTable'), 'dbt.38')
    finally
      md.Free;
    end;

    conn.Release;
  except
    on e: Exception do
    begin
      assertTrue(false, e.message);
      conn.Error(e);
    end;
  end;
end;

procedure TFDBTests.TestMSSQL;
var
  db: TFDBManager;
  settings : TFslStringMap;
begin
  if not TestSettings.hasSection('mssql') then
    assertNotTested
  else
  begin
    settings := TestSettings.section('mssql');
    try
      db := TFDBOdbcManager.create('test', kdbSqlServer, 8, 200, settings);
      try
        test(db);
      finally
        db.Free;
      end;
    finally
      settings.Free;
    end;
  end;
end;

procedure TFDBTests.TestMySQL;
var
  db: TFDBManager;
  settings : TFslStringMap;
begin
  if not TestSettings.hasSection('mysql') then
    assertNotTested
  else
  begin
    settings := TestSettings.section('mysql');
    try
      db := TFDBOdbcManager.create('test', kdbMySql, 8, 200, settings);
      try
        test(db);
      finally
        db.Free;
      end;
    finally
      settings.Free;
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
// Stmt.Free;
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
// Stmt.Free;
// end;
//
// finally
// DB.Free;
// end;

// end;
//

procedure TFDBTests.TestThread;
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
begin
  DeleteFile('c:\temp\sql.db');
  db := TFDBSQLiteManager.create('test', 'c:\temp\sql.db', true, 4);
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
          thread(testThread);
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
    db.Free;
  end;
end;

procedure TFDBTests.TestSQLite;
var
  db: TFDBManager;
begin
  DeleteFile('c:\temp\sql.db');
  db := TFDBSQLiteManager.create('test', 'c:\temp\sql.db', true, 4);
  try
    test(db);
  finally
    db.Free;
  end;
end;

// procedure TFDBTests.TestMySQLMaria;
// var
// db : TFDBManager;
// begin
// db := TFDBOdbcManager.create('test', 8, 0, 'MariaDB ODBC 3.0 Driver', 'localhost', 'test', 'root', 'h_Erp3ChJ![~C7~oL|61');
// try
// test(db);
// finally
// db.Free;
// end;
// end;
//

procedure registerTests;
// don't use initialization - give other code time to set up directories etc
begin
  RegisterTest('Database.Manager', TFDBTests.Suite);
end;

end.
