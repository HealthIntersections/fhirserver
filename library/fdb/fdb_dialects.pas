unit fdb_dialects;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
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
  fsl_base;

{------------------------------------------------------------------------------
   Cross Platform SQL Support
 ------------------------------------------------------------------------------}
Type
  // never insert into this - order is important

  TFDBPlatform = (kdbUnknown, kdbSQLServer, kdbSybase11, kdbCtree,
                  kdbAccess,  kdbDBIsam,    kdbInterbase, kdbDB2,  kdbGenericODBC,
                  kdbOracle8, kdbMySQL,     kdbASA,       kdbSybase12, kdbSQLite);

  TFDBPlatforms = set of TFDBPlatform;
  TSQLSet = array of String;

const
  KDB_ALL_PLATFORMS = [Low(TFDBPlatform) .. High(TFDBPlatform)];
  EOL_WINDOWS = #13#10;
  EOL_PLATFORM = EOL_WINDOWS;

// platform Recognition
function RecogniseDriver(ADriverDesc: String): TFDBPlatform;
function ShortDriverDesc(ADriverDesc: String): String;
function DescribePlatform(ADBPlatform: TFDBPlatform): String;
function ShowPlatformDescription(ADBPlatform: TFDBPlatform): String;
function DriverDescription(ADBPlatform: TFDBPlatform): String;
function ODBCDriverNameEx(ADBPlatform: TFDBPlatform): String;
function ODBCDriverRegExp(ADBPlatform: TFDBPlatform): String;

// platform specific SQL
function PlatformAutoIncrementSQL(ADBPlatform: TFDBPlatform): String;
function GetPlatformOtherAutoIncSQL(ADBPlatform: TFDBPlatform; ATableName, AFieldName: String; var ASQL1, ASQL2: String): Boolean;
function PrimaryKeyType(ADbasePlatform: TFDBPlatform; AConstraintName: String; APrimaryKeyName: String): String;
function ColCanBeNull(ADBPlatform: TFDBPlatform; ABeNull: Boolean): String;
function DBKeyType(ADBPlatform: TFDBPlatform): String;
function DBUnicodeType(ADBPlatform: TFDBPlatform; ASize : Integer): String;
function DBFloatType(ADBPlatform: TFDBPlatform): String;
function DBBlobType(ADBPlatform: TFDBPlatform): String;
function DBTextBlobType(ADBPlatform: TFDBPlatform): String;
function DBBlobStorageType(ADBPlatform: TFDBPlatform; ADBColumnName: String): String;
function DBDateTimeType(ADBPlatform: TFDBPlatform): String;
function DBGetDate(ADBPlatform: TFDBPlatform): String;
function DBBooleanType(ADBPlatform: TFDBPlatform): String;
function DBCharType(ADBPlatform: TFDBPlatform): String;
function DBInt64Type(ADBPlatform: TFDBPlatform): String;
function RestrictToNRows(ADBPlatform: TFDBPlatform; ASQL: String; AValue: Integer): String; // MSSQL Versions 2000, ? only
function CreateTableInfo(ADBPlatform: TFDBPlatform):String;
function DBIntType(ADBPlatform: TFDBPlatform):String;
function replaceColumnWrappingChars(const sql : String; ADBPlatform: TFDBPlatform): String;

// utilities
procedure ConvertCharacter(ATableName, AVersion: String; var VSQLValue: String);
function SQLHasResultSet(AStr: String): Boolean;
function CreateTableIndex(ADBIndexType: String; ADBIndexName: String; ADBTableName: String; ADBIndexField: String): String;

implementation

uses
  fsl_utilities,
  SysUtils;

function RecogniseDriver(ADriverDesc: String): TFDBPlatform;
var
  i: Integer;
begin
  Result := kdbUnknown;

  { code added here to bring function in line with RecogniseDriver
  previously duplicated in scriptSQL }
  i := pos(';', ADriverDesc);
  if i <> 0 then
    begin
    ADriverDesc := LowerCase(Copy(ADriverDesc, 1, i - 1));
    end
  else
    begin
    ADriverDesc := LowerCase(ADriverDesc);
    end;

  // recognise standard ODBC driver descriptions
  if ADriverDesc = 'sql server' then
    begin
    Result := kdbSQLServer;
    end
  else if ADriverDesc = 'sql native client' then
    begin
    Result := kdbSQLServer;
    end
  else if ADriverDesc = 'sql server native client 10.0' then
    begin
    Result := kdbSQLServer;
    end
  else if ADriverDesc = 'sql server native client 11.0' then
    begin
    Result := kdbSQLServer;
    end
  else if ADriverDesc = 'odbc driver 17 for sql server' then
    begin
    Result := kdbSQLServer;
    end
  else if ADriverDesc = 'faircom 32bit odbc driver' then
    begin
    Result := kdbCtree
    end
  else if ADriverDesc = 'c-treesql odbc driver' then
    begin
    Result := kdbCtree
    end
  else if ADriverDesc = 'microsoft access driver' then
    begin
    Result := kdbAccess
    end
  else if pos('adaptive', lowercase(ADriverDesc)) > 0 then
    begin
    Result := kdbASA
    end
  else if ADriverDesc = 'microsoft access driver (*.mdb)' then
    begin
    Result := kdbAccess
    end
  else if ADriverDesc = 'sybase system 11' then
    begin
    Result := kdbSybase11
    end
  else if ADriverDesc = 'sybase ase odbc driver' then
    begin
    Result := kdbSybase12;
    end
  else if ADriverDesc = 'Interbase' then
    begin
    Result := kdbInterbase
    end
  else if ADriverDesc = 'IBM DB2 ODBC DRIVER' then
    begin
    Result := kdbDB2
    end
  else if ADriverDesc = 'oracle odbc driver' then
    begin
    Result := kdbOracle8
    end
  else if ADriverDesc = 'MySQL' then
    begin
    Result := kdbMySQL
    end
  else if ADriverDesc = 'mariadb odbc 3.0 driver' then
    begin
    Result := kdbMySQL
    end
  else
    begin
    // standard Kestral names
    if pos('mssql', ADriverDesc) <> 0 then
      begin
      Result := kdbSQLServer
      end
    else if pos('mssql2005', ADriverDesc) <> 0 then
      begin
      Result := kdbSQLServer
      end
    else if pos('mssql2008', ADriverDesc) <> 0 then
      begin
      Result := kdbSQLServer
      end
    else if pos('mssql2012', ADriverDesc) <> 0 then
      begin
      Result := kdbSQLServer
      end
    else if pos('ctree', ADriverDesc) <> 0 then
      begin
      Result := kdbCtree
      end
    else if pos('access', ADriverDesc) <> 0 then
      begin
      Result := kdbAccess
      end
    else if pos('dbisam', ADriverDesc) <> 0 then
      begin
      Result := kdbDBIsam
      end
    else if pos('internal', ADriverDesc) <> 0 then
      begin
      Result := kdbDBIsam
      end
    else if pos('interbase', ADriverDesc) <> 0 then
      begin
      Result := kdbInterbase
      end
    else if pos('db2', ADriverDesc) <> 0 then
      begin
      Result := kdbDB2
      end
    else if pos('sybase', ADriverDesc) <> 0 then
      begin
      Result := kdbSybase11
      end
    else if pos('ase', ADriverDesc) <> 0 then
      begin
      Result := kdbSybase12;
      end
    else if pos('oracle', ADriverDesc) <> 0 then
      begin
      Result := kdbOracle8;
      end
    else if pos('asa', ADriverDesc) <> 0 then
      begin
      Result := kdbASA;
      end
    else if pos('mysql', ADriverDesc) <> 0 then
      begin
      Result := kdbMySQL;
      end;
    end;
end;

function ShortDriverDesc(ADriverDesc: String): String;
begin
  case RecogniseDriver(ADriverDesc) of
    kdbSQLServer: Result := 'mssql';
    kdbSybase11: Result := 'sybase';
    kdbCtree: Result := 'ctree';
    kdbAccess: Result := 'access';
    kdbDBIsam: Result := 'internal';
    kdbInterbase: Result := 'interbase';
    kdbDB2: Result := 'IBM DB2';
    kdbOracle8: Result := 'oracle';
    kdbMySQL: Result := 'mysql';
    kdbASA: Result := 'asa';
    kdbSybase12:    Result := 'ase';
  else
    begin
    Result := 'Unknown';
    end;
  end;
end;

function ShowPlatformDescription(ADBPlatform: TFDBPlatform): String;
begin
  case ADBPlatform of
    kdbSQLServer: Result := 'Microsoft SQLServer';
    kdbSybase11: Result := 'Sybase Enterprise';
    kdbCtree: Result := 'Faircom CTreeSQL';
    kdbAccess: Result := 'Microsoft Access';
    kdbDBIsam: Result := 'DBISAM (Internal)';
    kdbInterbase: Result := 'Firebird (or Interbase v6)';
    kdbDB2: Result := 'IBM DB2';
    kdbOracle8: Result := 'Oracle Corporation';
    kdbMySQL: Result := 'MySQL';
    kdbASA: Result := 'Sybase - ASA';
    kdbSybase12: Result := 'Sybase ASE ODBC Driver';
    kdbSQLite: result := 'SQLite';
  else
    begin
    Result := 'Unknown ' + IntToStr(Integer(ADBPlatform));
    end;
  end;
end;

function DescribePlatform(ADBPlatform: TFDBPlatform): String;
begin
  case ADBPlatform of
    kdbSQLServer: Result := 'MSSQLServer';
    kdbSybase11: Result := 'SybaseSystem11';
    kdbCtree: Result := 'CTreeSQL';
    kdbAccess: Result := 'MSAccess';
    kdbDBIsam: Result := 'DBISAM';
    kdbInterbase: Result := 'Interbase';
    kdbDB2: Result := 'IBM DB2';
    kdbOracle8: Result := 'OracleCorporation';
    kdbMySQL: Result := 'MySQL';
    kdbASA: Result := 'ASA';
    kdbSybase12: Result := 'ASE';
    kdbSQLite: result := 'SQLite';
  else
    begin
    Result := 'Unknown ' + IntToStr(Integer(ADBPlatform));
    end;
  end;
end;

function ODBCDriverNameEx(ADBPlatform: TFDBPlatform): String;
begin
  case ADBPlatform of
    kdbUnknown: Result := '';
    kdbSQLServer: Result := 'SQL Server';
    kdbSybase11: Result := '';
    kdbCtree: Result := 'c-treeSQL ODBC Driver';
    kdbAccess: Result := 'Microsoft Access Driver (*.mdb)';
    kdbDBIsam: Result := '';
    kdbInterbase: Result := '';
    kdbDB2: Result := '';
    kdbOracle8: Result := '';
    kdbMySQL: Result := 'MySQL ODBC 5.3 ANSI Driver'; // 'MariaDB ODBC 3.0 Driver';
    kdbASA: result := 'Adaptive Server Anywhere';
    kdbSybase12: Result := 'Sybase ASE ODBC Driver';
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;

// these regular expressions are used by the database wizard
// to determine which drivers on the system service a particular
// platform. See RegExpe.hlp for regular expression sysntax
function ODBCDriverRegExp(ADBPlatform: TFDBPlatform): String;
begin
  case ADBPlatform of
    kdbUnknown, kdbGenericODBC: Result := '^.*$';
    kdbSQLServer: Result := '^.*sql.*$';
    kdbSybase11: Result := '^.*sybase.*server.*$';
    kdbCtree: Result := '^.*c*tree.*$';
    kdbAccess: Result := '^.*microsoft.*access.*$';
    kdbDBIsam: Result := '^no match$';
    kdbInterbase: Result := '^no match$';
    kdbDB2: Result := '^.*db2.*$';
    kdbOracle8: Result := '^.*oracle.*$';
    kdbMySQL: Result := '^no match$';
    kdbASA: result := '^.*anywhere.*$';
    kdbSybase12: Result := '^.*ase.*$';
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;


function DriverDescription(ADBPlatform: TFDBPlatform): String;
// show driver description from TFDBPlatform database platform
begin
  // not sure quite what this is supposed to be. GDG 17/8/2003
  case ADBPlatform of
    kdbUnknown: Result := '';
    kdbSQLServer: Result := 'sql server';
    kdbSybase11: Result := 'sybase system 11';
    kdbCtree: Result := 'c-treeSQL ODBC Driver';
    kdbAccess: Result := 'microsoft access driver';
    kdbDBIsam: Result := '';
    kdbInterbase: Result := 'Interbase';
    kdbDB2: Result := 'ibm db2 odbc driver';
    kdbOracle8: Result := 'oracle odbc driver';
    kdbMySQL: Result := 'mysql';
    kdbASA: Result := 'Sybase ASA';
    kdbSybase12: Result := 'sybase ase odbc driver';
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;

function PlatformAutoIncrementSQL(ADBPlatform: TFDBPlatform): String;
begin
  case ADBPlatform of
    kdbUnknown: raise EDBException.create('Internal Error in Database Configuration, Database Platform not recognised');
    kdbSQLServer : Result := 'int IDENTITY(1,1) Not Null';
    kdbSybase11, kdbASA, kdbSybase12: Result := 'numeric(12, 0) IDENTITY Not Null';
    kdbCtree: raise EDBException.create('You are using Ctree!');
    kdbAccess: raise EDBException.create('You cannot create tables for MSAccess using SQL');
    kdbDBIsam: Result := 'autoinc Not Null';
    kdbInterbase: Result := 'int Not null';
    kdbDB2: Result := 'INT NOT NULL GENERATED ALWAYS AS IDENTITY (START WITH 1, INCREMENT BY 1)';
    kdbOracle8: Result := 'number(12, 0)';
    kdbMySQL: Result := 'int NOT NULL auto_increment';  //only works for a (primary) key field
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;

function GetPlatformOtherAutoIncSQL(ADBPlatform: TFDBPlatform; ATableName, AFieldName: String; var ASQL1, ASQL2: String): Boolean;
begin
  case ADBPlatform of
    kdbUnknown: raise EDBException.create('Internal Error in Database Configuration, Database Platform not recognised');
    kdbSQLServer : Result := False;
    kdbSybase11, kdbASA, kdbSybase12: Result := False;
    kdbCtree: result := false;
    kdbAccess: raise EDBException.create('You cannot create tables for MSAccess using SQL');
    kdbDBIsam: Result := False;
    kdbDB2: Result := False;
    kdbInterbase:
      begin
      Result := True;
      ASQL1 := 'CREATE GENERATOR GEN_' + ATableName;
      ASQL2 := 'CREATE TRIGGER TRIG_' + Copy(ATableName, Length(ATableName) - 10, 10) + '_' + Copy(AFieldName, Length(AFieldName) - 10, 10) + ' FOR ' + ATableName + EOL_WINDOWS +
        'BEFORE INSERT' + EOL_WINDOWS +
        'POSITION 0' + EOL_WINDOWS +
        'AS BEGIN' + EOL_WINDOWS +
        'NEW.' + AFieldName + ' = GEN_ID(GEN_' + ATableName + ', 1);' + EOL_WINDOWS +
        'END';
      end;
    kdbOracle8:
      begin
      Result := True;
      ASQL1 := 'CREATE SEQUENCE SEQ_' + ATableName + ' INCREMENT BY 1 START WITH 1';
      ASQL2 := 'CREATE TRIGGER TRIG_' + Copy(ATableName, Length(ATableName) - 10, 10) + '_' + Copy(AFieldName, Length(AFieldName) - 10, 10) + EOL_WINDOWS +
        ' BEFORE INSERT' + EOL_WINDOWS +
        ' ON ' + ATableName + EOL_WINDOWS +
        ' FOR EACH ROW ' + EOL_WINDOWS +
        ' begin' +
        ' select SEQ_' + ATableName + '.nextval into :new.' + AFieldName + ' from dual;' +
        ' end;';
      end;
    kdbMySQL: Result := False;
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;

function PrimaryKeyType(ADbasePlatform: TFDBPlatform; AConstraintName: String; APrimaryKeyName: String): String;
begin
  case ADbasePlatform of
    kdbUnknown: raise EDBException.create('Unknown Database Platform!');
    kdbSQLServer : Result := 'CONSTRAINT ' + AConstraintName + ' PRIMARY KEY CLUSTERED (' + APrimaryKeyName + ')';
    kdbSybase11, kdbASA, kdbSybase12: Result := 'PRIMARY KEY CLUSTERED (' + APrimaryKeyName + ')';
    kdbCtree: Result := 'PRIMARY KEY (' + APrimaryKeyName + ')';
    kdbAccess: raise EDBException.create('You are using MSACESS!');
    kdbDBIsam: Result := 'CONSTRAINT ' + AConstraintName + ' PRIMARY KEY (' + APrimaryKeyName + ')';
    kdbInterbase: Result := 'PRIMARY KEY (' + APrimaryKeyName + ')';
    kdbDB2: Result := 'PRIMARY KEY (' + APrimaryKeyName + ')';
    kdbOracle8: Result := 'CONSTRAINT ' + AConstraintName + ' PRIMARY KEY (' + APrimaryKeyName + ')';
    kdbMySQL: Result := 'CONSTRAINT ' + AConstraintName + ' PRIMARY KEY (' + APrimaryKeyName + ')';
    kdbSQLite: Result := 'CONSTRAINT ' + AConstraintName + ' PRIMARY KEY (' + APrimaryKeyName + ')';
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;

function CreateTableIndex(ADBIndexType: String; ADBIndexName: String; ADBTableName: String; ADBIndexField: String): String;
begin
  Result := 'CREATE ' + ADBIndexType + ' INDEX ' + ADBIndexName + ' ON ' + ADBTableName + '(' + ADBIndexField + ')'
end;

function DBUnicodeType(ADBPlatform: TFDBPlatform; ASize : Integer): String;
begin
  case ADBPlatform of
    kdbUnknown: raise EDBException.create('Internal Error in Database Configuration, Database Platform not recognised');
    kdbSQLServer : Result := 'nchar('+inttostr(ASize)+')';
    kdbSybase11: raise EDBException.create('Sybase field field for Unicode not yet resolved');
    kdbSybase12: Result := 'unichar('+inttostr(ASize)+')';
    kdbASA : result := 'char('+inttostr(ASize*2)+')';
    kdbCtree: result := 'char('+inttostr(ASize*2)+')';
    kdbAccess: raise EDBException.create('You cannot create tables for MSAccess using SQL');
    kdbDBIsam: Result := 'char('+inttostr(ASize*2)+')';
    kdbInterbase: Result := 'char('+inttostr(ASize*2)+')';
    kdbDB2: result := 'char('+inttostr(ASize*2)+')';
    kdbOracle8: raise EDBException.create('Oracle field field for Unicode not yet resolved');
    kdbMySQL: result := 'VARCHAR('+inttostr(ASize)+') CHARACTER SET utf8mb4';
    kdbSQLite: result := 'VARCHAR('+inttostr(ASize)+') CHARACTER SET utf8';
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;

function DBFloatType(ADBPlatform: TFDBPlatform): String;
begin
  case ADBPlatform of
    kdbUnknown: raise EDBException.create('Internal Error in Database Configuration, Database Platform not recognised');
    kdbSQLServer : Result := 'float';
    kdbSybase11, kdbSybase12: Result := 'float';
    kdbASA : result := 'FLOAT';
    kdbCtree: result := 'FLOAT';
    kdbAccess: raise EDBException.create('You cannot create tables for MSAccess using SQL');
    kdbDBIsam: result := 'float';
    kdbInterbase: result := 'float';
    kdbDB2: result := 'float';
    kdbOracle8: raise EDBException.create('Oracle field field for Float not yet resolved');
    kdbMySQL: result := 'DOUBLE';
    kdbSQLite: result := 'REAL';
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;

function DBKeyType(ADBPlatform: TFDBPlatform): String;
begin
  case ADBPlatform of
    kdbUnknown: raise EDBException.create('Internal Error in Database Configuration, Database Platform not recognised');
    kdbSQLServer : Result := 'int';
    kdbSybase11, kdbASA, kdbSybase12: Result := 'numeric(12, 0)';
    kdbCtree: Result := 'int';
    kdbAccess: raise EDBException.create('You cannot create tables for MSAccess using SQL');
    kdbDBIsam: Result := 'int';
    kdbInterbase: Result := 'int';
    kdbDB2: Result := 'int';
    kdbOracle8: Result := 'number(12,0)';
    kdbMySQL: Result := 'int';
    kdbSQLite: Result := 'Integer';
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;

function DBBlobType(ADBPlatform: TFDBPlatform): String;
begin
  case ADBPlatform of
    kdbUnknown: raise EDBException.create('Internal Error in Database Configuration, Database Platform not recognised');
    kdbSQLServer : Result := 'image';
    kdbSybase11, kdbASA, kdbSybase12: Result := 'image';
    kdbCtree: result := 'LVARBINARY';
    kdbAccess: raise EDBException.create('You cannot create tables for MSAccess using SQL');
    kdbDBIsam: Result := 'blob(1,1)';
    kdbInterbase: Result := 'blob';
    kdbDB2: Result := 'blob';
    kdbOracle8: Result := 'blob default empty_blob()';
    kdbMySQL: Result := 'LongBlob';
    kdbSQLite: Result := 'BLOB';
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;


function DBTextBlobType(ADBPlatform: TFDBPlatform): String;
begin
  case ADBPlatform of
    kdbUnknown: raise EDBException.create('Internal Error in Database Configuration, Database Platform not recognised');
    kdbSQLServer : Result := 'image';
    kdbSybase11, kdbASA, kdbSybase12: Result := 'image';
    kdbCtree: result := 'LVARBINARY';
    kdbAccess: raise EDBException.create('You cannot create tables for MSAccess using SQL');
    kdbDBIsam: Result := 'blob(1,1)';
    kdbInterbase: Result := 'blob';
    kdbDB2: Result := 'blob';
    kdbOracle8: Result := 'blob default empty_blob()';
    kdbMySQL: Result := 'LONGTEXT';
    kdbSQLite: Result := 'BLOB';
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;




function DBBlobStorageType(ADBPlatform: TFDBPlatform; ADBColumnName: String): String;
begin
  case ADBPlatform of
    kdbUnknown: raise EDBException.create('Internal Error in Database Configuration, Database Platform not recognised');
    kdbSQLServer : Result := '';
    kdbSybase11, kdbASA, kdbSybase12: Result := '';
    kdbCtree: raise EDBException.create('You are using Ctree!');
    kdbAccess: raise EDBException.create('You cannot create tables for MSAccess using SQL');
    kdbDBIsam: Result := '';
    kdbInterbase: Result := '';
    kdbDB2: Result := '';
    kdbOracle8:
      Result := 'lob (' + ADBColumnName + ') STORE AS (CHUNK 4096 ' + EOL_WINDOWS +
        'PCTVERSION 5 ' + EOL_WINDOWS +
        'NOCACHE LOGGING ' + EOL_WINDOWS +
        'STORAGE (MAXEXTENTS 5) ' + EOL_WINDOWS +
        ')';
    kdbMySQL: Result := '';
    kdbSQLite: Result := '';
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;

function DBDateTimeType(ADBPlatform: TFDBPlatform): String;
begin
  case ADBPlatform of
    kdbUnknown: raise EDBException.create('Internal Error in Database Configuration, Database Platform not recognised');
    kdbSQLServer : Result := 'datetime';
    kdbSybase11, kdbASA, kdbSybase12: Result := 'datetime';
    kdbCtree: Result := 'timestamp';
    kdbAccess: raise EDBException.create('You cannot create tables for MSAccess using SQL');
    kdbDBIsam: Result := 'timestamp';
    kdbInterbase: Result := 'timestamp';
    kdbDB2: Result := 'timestamp';
    kdbOracle8: Result := 'date';
    kdbMySQL: Result := 'datetime';
    kdbSQLite: Result := 'datetext(24)';
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;

function DBGetDate(ADBPlatform: TFDBPlatform): String;
begin
  case ADBPlatform of
    kdbUnknown: raise EDBException.create('Internal Error in Database Configuration, Database Platform not recognised');
    kdbSQLServer : Result := 'getdate()';
    kdbSybase11, kdbASA, kdbSybase12: Result := 'getdate()';
    kdbCtree: result := 'CURDATE()';
    kdbAccess: Result := '(Date() + Time())';
    kdbDBIsam: Result := 'CURRENT_TIMESTAMP';
    kdbInterbase: Result := 'CURRENT_TIMESTAMP';
    kdbDB2: Result := 'CURRENT TIMESTAMP';
    kdbOracle8: Result := 'sysdate';
    kdbMySQL: Result := 'Now()';
    kdbSQLite: Result := 'CURRENT_TIMESTAMP';
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;


function ColCanBeNull(ADBPlatform: TFDBPlatform; ABeNull: Boolean): String;
begin
  case ADBPlatform of
    kdbUnknown: raise EDBException.create('Internal Error in Database Configuration, Database Platform not recognised');
    kdbSQLServer:
      if ABeNull then
        begin
        Result := 'Null'
        end
      else
        begin
        Result := 'Not Null';
        end;
    kdbSybase11, kdbASA, kdbSybase12:
      if ABeNull then
        begin
        Result := 'Null'
        end
      else
        begin
        Result := 'Not Null';
        end;
    kdbCtree:
      if ABeNull then
        begin
        Result := ''
        end
      else
        begin
        Result := 'Not Null';
        end;
    kdbAccess:
      if ABeNull then
        begin
        Result := 'Null'
        end
      else
        begin
        Result := 'Not Null';
        end;
    kdbDBIsam:
      if ABeNull then
        begin
        Result := ''
        end
      else
        begin
        Result := 'Not Null';
        end;
    kdbInterbase:
      if ABeNull then
        begin
        Result := ''
        end
      else
        begin
        Result := 'Not Null';
        end;
    kdbDB2:
      if ABeNull then
        begin
        Result := ''
        end
      else
        begin
        Result := 'Not Null';
        end;
    kdbOracle8:
      if ABeNull then
        begin
        Result := 'Null'
        end
      else
        begin
        Result := 'Not Null';
        end;
    kdbMySQL:
      if ABeNull then
        begin
        Result := 'Null'
        end
      else
        begin
        Result := 'Not Null';
        end;
    kdbSQLite:
      if ABeNull then
        begin
        Result := 'Null'
        end
      else
        begin
        Result := 'Not Null';
        end;
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;

function DBBooleanType(ADBPlatform: TFDBPlatform): String;
begin
  case ADBPlatform of
    kdbUnknown: raise EDBException.create('Internal Error in Database Configuration, Database Platform not recognised');
    kdbSQLServer : Result := 'bit';
    kdbSybase11, kdbASA, kdbSybase12: Result := 'bit';
    kdbCtree: Result := 'bit';
    kdbAccess: Result := 'DB_BOOLEAN';
    kdbDBIsam: Result := 'boolean';
    kdbInterbase: Result := 'char(1)';
    kdbDB2: Result := 'char(1)';
    kdbOracle8: Result := 'number(1)';
    kdbMySQL: Result := 'bool';
    kdbSQLite: Result := 'Integer';
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;

function DBCharType(ADBPlatform: TFDBPlatform): String;
begin
  case ADBPlatform of
    kdbUnknown: raise EDBException.create('Internal Error in Database Configuration, Database Platform not recognised');
    kdbSQLServer: Result := 'char';
    kdbSybase11, kdbASA, kdbSybase12: Result := 'char';
    kdbCtree: Result := 'char';
    kdbAccess: Result := 'char';
    kdbDBIsam: Result := 'char';
    kdbInterbase: Result := 'char';
    kdbDB2: Result := 'char';
    kdbOracle8: Result := 'varchar';
    kdbMySQL: Result := 'char';
    kdbSQLite: Result := 'text';
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;

function DBInt64Type(ADBPlatform: TFDBPlatform): String;
begin
  case ADBPlatform of
    kdbUnknown: raise EDBException.create('Internal Error in Database Configuration, Database Platform not recognised');
    kdbSQLServer : Result := 'bigint';
    kdbASA : result := 'BIGINT';
    kdbSybase11, kdbSybase12: raise EDBException.create('Sybase does not support Int64 values properly');
    // Result := 'numeric(18, 0)'; If Sybase ever gets ther act together then we can use this....
    kdbCtree: Result := 'bigint';
    kdbAccess: raise EDBException.create('You cannot create tables for MSAccess using SQL');
    kdbDBIsam: Result := 'largeint';
    kdbInterbase: Result := 'bigint';
    kdbDB2: Result := 'bigint';
    kdbOracle8: Result := 'number';
    kdbMySQL: Result := 'bigint';
    kdbSQLite: Result := 'Integer';
  else
    begin
    raise EDBException.create('Internal Error in Database Configuration, Database Platform in Error');
    end;
  end;
end;

function RestrictToNRows(ADBPlatform: TFDBPlatform; ASQL: String; AValue: Integer): String;
var
  LIndex: Integer;
  LSelectSQLPrefix : String;
  LSelectSQLSuffix : String;
begin
  if ADBPlatform in [kdbmysql, kdbSQLite] Then
  Begin
    result := aSQL + ' limit '+inttostr(aValue);
  End
  Else
  Begin
    LSelectSQLPrefix := '';
    LSelectSQLSuffix := StringTrimWhitespace(ASQL);

    //This function will insert "TOP AValue" after "SELECT [ALL | DISTINCT]" in the sql statement
    //NOTE: the keyword in standard SQL3 but may not be supported by all database servers

    LIndex := Pos('select ', LowerCase(LSelectSQLSuffix));
    if LIndex = 1 then
      begin
      LSelectSQLPrefix := copy(LSelectSQLSuffix, 1, 7);
      LSelectSQLSuffix := StringTrimWhitespace(copy(LSelectSQLSuffix, 8, high(Integer)));

      LIndex := Pos('distinct ', LowerCase(LSelectSQLSuffix));
      if LIndex = 1 then
        begin
        LSelectSQLPrefix := LSelectSQLPrefix + copy(LSelectSQLSuffix, 1, 9);
        LSelectSQLSuffix := copy(LSelectSQLSuffix, 10, high(Integer));
        end
      else
        begin
        LIndex := Pos('all ', LowerCase(LSelectSQLSuffix));
        if LIndex = 1 then
          begin
          LSelectSQLPrefix := LSelectSQLPrefix + copy(LSelectSQLSuffix, 1, 4);
          LSelectSQLSuffix := copy(LSelectSQLSuffix, 5, high(Integer));
          end;
        end;

      LSelectSQLPrefix := LSelectSQLPrefix + 'TOP ' + IntToStr(AValue) + ' ';
      end;

    Result := LSelectSQLPrefix + LSelectSQLSuffix;
  End;
end;

function DBIntType(ADBPlatform: TFDBPlatform):String;
begin
  case ADBPlatform of
    kdbSQLServer : result := 'int';
    kdbMySQL : result := 'UNSIGNED';
    kdbSQLite : result := 'Integer';
  else raise EDBTodo.create('DBIntType');
  end;
end;

function CreateTableInfo(ADBPlatform: TFDBPlatform):String;
begin
  if ADBPlatform = kdbMySQL then
    begin
    result := ' ENGINE=INNODB';
    end
  else
    begin
    result := '';
    end;
end;

procedure ConvertCharacter(ATableName, AVersion: String; var VSQLValue: String);
var
  i, j, k: Integer;
  LTmpStr, LTmpStr1: String;
begin
  for i := 1 to Length(VSQLValue) do
    begin
    if VSQLValue[i] = #145 then  // we found  (Left Single Quotation Mark)
      begin
      VSQLValue[i] := '''';
      end
    else if VSQLValue[i] = #146 then // we found  (Right Single Quotation Mark)
      begin
      VSQLValue[i] := '''';
      end
    else if VSQLValue[i] = #147 then // we found  (Left Double Quotation Mark)
      begin
      VSQLValue[i] := '"';
      end
    else if VSQLValue[i] = #148 then // we found  (Right Double Quotation Mark)
      begin
      VSQLValue[i] := '"';
      end
    else if VSQLValue[i] = #150 then // we found  (En Dash)
      begin
      VSQLValue[i] := '-';
      end
    else if VSQLValue[i] = #151 then // we found  (Em Dash)
      begin
      VSQLValue[i] := '-';
      end
    else if VSQLValue[i] = #180 then // we found 4 (Acute Accent)
      begin
      VSQLValue[i] := ''''
      end
    else if VSQLValue[i] = #189 then // we found = (Vulgar Fraction One Half)
      begin
      LTmpStr := Copy(VSQLValue, 1, i - 1);
      LTmpStr1 := LTmpStr;
      LTmpStr1[i] := '.';
      LTmpStr1[i + 1] := '5';
      k := i + 1;
      for j := k to Length(VSQLValue) do
        begin
        LTmpStr1[j] := VSQLValue[i];
        end;
      VSQLValue := LTmpStr1;
      Break;
      end
    else if VSQLValue[i] = #196 then // we found D (Latin Capital Letter A with Diaeresis)
      begin
      VSQLValue[i] := 'A';
      end
    else if VSQLValue[i] = #220 then // we found \ (Latin Capital Letter U with Diaeresis)
      begin
      VSQLValue[i] := 'U';
      end
    else if VSQLValue[i] = #223 then // we found _ (Latin Small Letter Sharp S)
      begin
      VSQLValue[i] := 's';
      end
    else if VSQLValue[i] = #228 then // we found d (Latin Small Letter A with Diaeresis)
      begin
      VSQLValue[i] := 'a';
      end
    else if VSQLValue[i] = #246 then // we found v (Latin Small Letter O with Diaeresis)
      begin
      VSQLValue[i] := 'o';
      end
    else if VSQLValue[i] = #252 then // we found | (Latin Small Letter U with Diaeresis)
      begin
      VSQLValue[i] := 'u'
      end;
    end;
end;

function SQLHasResultSet(AStr: String): Boolean;
begin
  AStr := StringTrimSet(AStr, [#10, #13, #9, ' ']);
  Result := SameText('SELECT', Copy(AStr, 1, 6)) or SameText('SHOW ', Copy(AStr, 1, 5));
end;

function replaceColumnWrappingChars(const sql : String; ADBPlatform: TFDBPlatform): String;
begin
  if ADBPlatform = kdbMySQL then
    result := sql.Replace('[', '`').Replace(']', '`')
  else
    result := sql;
end;

end.
