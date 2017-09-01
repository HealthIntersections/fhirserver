unit KDBTests;

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


{$DEFINE DIFF}

interface

Uses
  SysUtils, Classes,
  DateSupport, TextUtilities,
  KDBDialects,
  KDBManager, KDBOdbc,
  DUnitX.TestFramework;

Type
  [TextFixture]
  TKDBTests = Class (TObject)
  private
    procedure test(manager : TKDBManager);
  Published
    {[TestCase] }procedure TestMSSQL;
    [TestCase] procedure TestMySQL;
    {[TestCase] }procedure TestMySQLMaria;
  End;

implementation

function BlobIsSame(l, r : TBytes) : boolean;
var
  i : integer;
begin
  if length(l) <> length(r) then
    exit(false);
  result := true;
  for i := 0 to length(l) - 1 do
    if l[i] <> r[i] then
      exit(false);
end;


function isSame(l, r : Double) : boolean;
var
  d : Double;
begin
  d := abs(l-r);
  result := d < 0.00001;
end;

{ TKDBTests }

procedure TKDBTests.test(manager: TKDBManager);
var
  conn : TKDBConnection;
  d, od : TDateTimeEx;
  b : TBytes;
  i64 : Int64;
  md : TKDBMetaData;
begin
  d := TDateTimeEx.makeLocal(dtpSec);
  b := FileToBytes('C:\work\fhirserver\Libraries\db\KDBTests.pas');
  i64 := MaxInt;
  i64 := i64+2;

  conn := manager.GetConnection('test');
  try
    md := conn.FetchMetaData;
    try
      if md.HasTable('TestTable') then
        conn.DropTable('TestTable');
    finally
      md.Free;
    end;

    conn.ExecSQL('CREATE TABLE TestTable ( '+#13#10+
         ' TestKey '+DBKeyType(conn.owner.platform)+' '+ColCanBeNull(conn.owner.platform, False)+', '+#13#10+
         ' Name nchar(255) '+ColCanBeNull(conn.owner.platform, False)+', '+#13#10+
         ' Number int '+ColCanBeNull(conn.owner.platform, True)+', '+#13#10+
         ' BigNumber bigint '+ColCanBeNull(conn.owner.platform, True)+', '+#13#10+
         ' FloatNumber float '+ColCanBeNull(conn.owner.platform, True)+', '+#13#10+
         ' Instant '+DBDateTimeType(conn.owner.platform)+' '+ColCanBeNull(conn.owner.platform, False)+', '+#13#10+
         ' Content '+DBBlobType(conn.owner.platform)+' '+ColCanBeNull(conn.owner.platform, true)+', '+#13#10+
         PrimaryKeyType(conn.owner.Platform, 'PK_TestTable', 'TestKey')+') '+CreateTableInfo(conn.owner.platform));
    conn.ExecSQL('Create Unique INDEX SK_TestTable_Index ON TestTable (Name, Number)');

    try
      md := conn.FetchMetaData;
      try
        Assert.IsTrue(md.HasTable('TestTable'))
      finally
        md.Free;
      end;
      Assert.IsTrue(conn.CountSQL('Select count(*) from TestTable') = 0);

      conn.ExecSQL('Insert into TestTable (TestKey, Name, Number, BigNumber, FloatNumber, Instant) values (1, ''a name'', 2, '+IntToStr(i64)+', 3.2, '+DBGetDate(manager.Platform)+')');
      conn.sql := 'Insert into TestTable (TestKey, Name, Number, BigNumber, FloatNumber, Instant, Content) values (:k, :n, :i, :bi, :d, :ts, :c)';
      conn.Prepare;
      conn.BindKey('k', 2);
      conn.BindString('n', 'Name 2');
      conn.BindInteger('i', 3);
      conn.BindInt64('bi', -i64);
      conn.BindDouble('d', 3.4);
      conn.BindDateAndTime('ts', d);
      conn.BindBlob('c', b);
      conn.Execute;
      conn.Terminate;

      Assert.IsTrue(conn.CountSQL('Select count(*) from TestTable where  TestKey = 1') = 1);
      Assert.IsTrue(conn.CountSQL('Select count(*) from TestTable where  TestKey = 0') = 0);

      conn.sql := 'Select * from TestTable';
      conn.Prepare;
      conn.Execute;
      conn.FetchNext;
      assert.IsTrue(Conn.ColIntegerByName['TestKey'] = 1);
      assert.IsTrue(Conn.ColStringByName['Name'] = 'a name');
      assert.IsTrue(Conn.ColIntegerByName['Number'] = 2);
      assert.IsTrue(Conn.ColInt64ByName['BigNumber'] = i64);
      assert.IsTrue(isSame(Conn.ColDoubleByName['FloatNumber'], 3.2));
      assert.IsTrue(TSToDateTime(Conn.ColTimestampByName['Instant']) < now);
      assert.IsTrue(TSToDateTime(Conn.ColTimestampByName['Instant']) > now - DATETIME_MINUTE_ONE);
      od := Conn.ColDateAndTimeByName['Instant'];
      assert.IsTrue(length(Conn.ColBlobByName['Content']) = 0);
      assert.IsTrue(Conn.ColNullByName['Content']);

      conn.FetchNext;
      assert.IsTrue(Conn.ColIntegerByName['TestKey'] = 2);
      assert.IsTrue(Conn.ColStringByName['Name'] = 'Name 2');
      assert.IsTrue(Conn.ColIntegerByName['Number'] = 3);
      assert.IsTrue(Conn.ColInt64ByName['BigNumber'] = -i64);
      assert.IsTrue(isSame(Conn.ColDoubleByName['FloatNumber'], 3.4));
      assert.IsTrue(Conn.ColDateAndTimeByName['Instant'].fixPrecision(dtpSec).equal(d));
      assert.IsTrue(BlobIsSame(Conn.ColBlobByName['Content'], b));
      conn.Terminate;

      sleep(1000);

      conn.ExecSQL('Update TestTable set Name = ''3rd Name'', Number = 3, BigNumber = '+IntToStr(-i64)+', FloatNumber = 3.1, Instant = '+DBGetDate(manager.Platform)+' where TestKey = 1');
      conn.sql := 'Update TestTable set Name = :n, Number = :i, BigNumber = :bi, FloatNumber = :d, Instant = :ts, Content = :c where TestKey = :k';
      conn.Prepare;
      conn.BindKey('k', 2);
      conn.BindString('n', 'Name 4');
      conn.BindInteger('i', -4);
      conn.BindInt64('bi', i64);
      conn.BindDouble('d', 3.01);
      conn.BindDateAndTime('ts', od);
      conn.BindNull('c');
      conn.Execute;
      conn.Terminate;

      conn.sql := 'Select * from TestTable';
      conn.Prepare;
      conn.Execute;
      conn.FetchNext;
      assert.IsTrue(Conn.ColIntegerByName['TestKey'] = 1);
      assert.IsTrue(Conn.ColStringByName['Name'] = '3rd Name');
      assert.IsTrue(Conn.ColIntegerByName['Number'] = 3);
      assert.IsTrue(Conn.ColInt64ByName['BigNumber'] = -i64);
      assert.IsTrue(isSame(Conn.ColDoubleByName['FloatNumber'], 3.1));
      assert.IsTrue(Conn.ColDateAndTimeByName['Instant'].after(od, false));
      assert.IsTrue(length(Conn.ColBlobByName['Content']) = 0);
      assert.IsTrue(Conn.ColNullByName['Content']);

      conn.FetchNext;
      assert.IsTrue(Conn.ColIntegerByName['TestKey'] = 2);
      assert.IsTrue(Conn.ColStringByName['Name'] = 'Name 4');
      assert.IsTrue(Conn.ColIntegerByName['Number'] = -4);
      assert.IsTrue(Conn.ColInt64ByName['BigNumber'] = i64);
      assert.IsTrue(isSame(Conn.ColDoubleByName['FloatNumber'], 3.01));
      assert.IsTrue(Conn.ColDateAndTimeByName['Instant'].equal(od));
      assert.IsTrue(length(Conn.ColBlobByName['Content']) = 0);
      assert.IsTrue(Conn.ColNullByName['Content']);
      conn.Terminate;

      conn.ExecSQL('Delete from TestTable where TestKey = 1');
      conn.sql := 'Delete from TestTable where TestKey = :k';
      conn.Prepare;
      conn.BindKey('k', 2);
      conn.Execute;
      conn.Terminate;

      Assert.IsTrue(conn.CountSQL('Select count(*) from TestTable') = 0);

    finally
      conn.DropTable('TestTable');
    end;
    md := conn.FetchMetaData;
    try
      Assert.Isfalse(md.HasTable('TestTable'))
    finally
      md.Free;
    end;

    conn.Release;
  except
    on e:exception do
    begin
      Assert.IsTrue(false, e.message);
      conn.Error(e);
    end;
  end;
end;

procedure TKDBTests.TestMSSQL;
var
  db : TKDBManager;
begin
  db := TKDBOdbcManager.create('test', 8, 200, 'SQL Server', 'localhost', 'test', '', '');
  try
    test(db);
  finally
    db.Free;
  end;
end;

procedure TKDBTests.TestMySQL;
var
  db : TKDBManager;
begin
  db := TKDBOdbcManager.create('test', 8, 0, 'MySQL ODBC 5.3 Unicode Driver', 'localhost', 'test', 'test', 'test');
  try
    test(db);
  finally
    db.Free;
  end;
end;

procedure TKDBTests.TestMySQLMaria;
var
  db : TKDBManager;
begin
  db := TKDBOdbcManager.create('test', 8, 0, 'MariaDB ODBC 3.0 Driver', 'localhost', 'test', 'root', 'h_Erp3ChJ![~C7~oL|61');
  try
    test(db);
  finally
    db.Free;
  end;
end;




initialization
  TDUnitX.RegisterTestFixture(TKDBTests);
end.
