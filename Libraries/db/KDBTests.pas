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
  KDBDialects,
  KDBManager,
  KDBOdbcExpress,
  DUnitX.TestFramework;

Type
  [TextFixture]
  TKDBTests = Class (TObject)
  private
    procedure test(manager : TKDBManager);
  Published
    [TestCase] procedure TestMSSQL;
    [TestCase] procedure TestMySQL;
  End;

implementation

{ TKDBTests }


{ TKDBTests }

procedure TKDBTests.test(manager: TKDBManager);
var
  conn : TKDBConnection;
begin
  conn := manager.GetConnection('test');
  try
    conn.ExecSQL('CREATE TABLE TestTable ( '+#13#10+
         ' TestKey '+DBKeyType(conn.owner.platform)+' '+ColCanBeNull(conn.owner.platform, False)+', '+#13#10+
         ' Name nchar(255) '+ColCanBeNull(conn.owner.platform, False)+', '+#13#10+
         ' Number int '+ColCanBeNull(conn.owner.platform, True)+', '+#13#10+
         ' FloatNumber float '+ColCanBeNull(conn.owner.platform, True)+', '+#13#10+
         ' Flag int '+ColCanBeNull(conn.owner.platform, False)+', '+#13#10+
         ' Instant '+DBDateTimeType(conn.owner.platform)+' '+ColCanBeNull(conn.owner.platform, False)+', '+#13#10+
         ' Content '+DBBlobType(conn.owner.platform)+' '+ColCanBeNull(conn.owner.platform, False)+', '+#13#10+
         PrimaryKeyType(conn.owner.Platform, 'PK_TestTable', 'TestKey')+') '+CreateTableInfo(conn.owner.platform));
    conn.ExecSQL('Create Unique INDEX SK_TestTable_Index ON TestTable (Name, Number)');


    conn.DropTable('TestTable');
    Assert.IsTrue(true);
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
  db := TKDBOdbcDirect.create('test', 8, 200, ODBCDriverNameEx(kdbSQLServer), 'localhost', 'test', '', '');
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
  db := TKDBOdbcDirect.create('test', 8, 0, ODBCDriverNameEx(kdbMySQL), 'localhost', 'test', 'root', 'h_Erp3ChJ![~C7~oL|61');
  try
    test(db);
  finally
    db.Free;
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TKDBTests);
end.
