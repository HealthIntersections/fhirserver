unit fdb_fpc;

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

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  DB, SQLDB,
  mysql80conn,
  fsl_base, fsl_utilities, fsl_logging,
  fdb_dialects, fdb_manager;

type
  TFDBSQDBConnection = class (TFDBConnection)
  private
    FConn : TSQLConnection;
    FTransact : TSQLTransaction;
    FQuery : TSQLQuery;
    FFirst : boolean;
  Protected
    procedure StartTransactV; Override;
    procedure CommitV; Override;
    procedure RollbackV; Override;
    function FetchMetaDataV : TFDBMetaData; Override;
    function GetColCountV: Integer; Override;
    function GetColStringV(ACol: Word): String; Override;
    function GetColIntegerV(ACol: Word): Integer; Override;
    function GetColInt64V(ACol: Word): Int64; Override;
    function GetColDoubleV(ACol: Word): Double; Override;
    function GetColBlobV(ACol: Word): TBytes; Override;
    function GetColNullV(ACol: Word): Boolean; Override;
    function GetColTimestampV(ACol: Word): TTimestamp; Override;
    function GetColDateTimeExV(ACol: Word): TFslDateTime; Override;
    function GetColTypeV(ACol: Word): TFDBColumnType; Override;
    function GetColKeyV(ACol: Word): Integer; Override;
    function GetRowsAffectedV: Integer; Override;
    procedure RenameTableV(AOldTableName, ANewTableName: String); Override;
    procedure RenameColumnV(ATableName, AOldColumnName, ANewColumnName: String; AColumnDetails: String = ''); Override;
    procedure DropTableV(ATableName : String); Override;
    procedure DropColumnV(ATableName, AColumnName : String); override;
    procedure ListTablesV(AList : TStrings); override;
    procedure ClearDatabaseV; Override;
    procedure PrepareV; Override;
    procedure ExecuteV; Override;
    procedure TerminateV; Override;
    function FetchNextV: Boolean; Override;
    function ColByNameV(AColName: String): Integer; Override;
    function ColNameV(ACol: Integer): String; Override;
    procedure BindInt64V(AParamName: String; AParamValue: Int64); Override;
    procedure BindIntegerV(AParamName: String; AParamValue: Integer); Override;
    procedure BindKeyV(AParamName: String; AParamValue: Integer); Override;
    procedure BindDoubleV(AParamName: String; AParamValue: Double); Override;
    procedure BindStringV(AParamName: String; AParamValue: String); Override;
    procedure BindTimeStampV(AParamName: String; AParamValue: TTimeStamp); Override;
    procedure BindDateTimeExV(AParamName: String; AParamValue: TFslDateTime); Override;
    procedure BindBlobV(AParamName: String; AParamValue: TBytes); Override;
    procedure BindNullV(AParamName: String); Override;
    function DatabaseSizeV : int64; Override;
    Function TableSizeV(sName : String):int64; Override;
    function SupportsSizingV : Boolean; Override;
    function sizeInBytesV(magic : integer) : cardinal; override;
  Public
    constructor Create(AOwner : TFDBManager; conn : TSQLConnection);
    destructor Destroy; override;
  end;

  { TFDBSQLDBManager }

  TFDBSQLDBManager = class (TFDBManager)
  private
    FPlatform : TFDBPlatform;
    FServer, FDatabase, FUsername, FPassword : String;
    function makeMySqlConnection: TMySQL80Connection;
  Protected
    function GetDBProvider: TFDBProvider; Override;
    function ConnectionFactory: TFDBConnection; Override;
    function GetDBPlatform: TFDBPlatform; Override;
    function GetDBDetails: String; Override;
    function GetDriver: String; Override;
    procedure init; override;
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(AName : String; platform : TFDBPlatform; server, database, username, password : String; maxConn : integer = 100); overload;
    destructor Destroy; override;
  end;

implementation

{ TFDBSQDBConnection }

constructor TFDBSQDBConnection.Create(AOwner : TFDBManager; conn : TSQLConnection);
begin
  inherited Create(AOwner);
  FConn := conn;
  FTransact := TSQLTransaction.create(nil);
  FTransact.SQLConnection := FConn;
  FQuery := TSQLQuery.create(nil);
  FQuery.Transaction := FTransact;
  FQuery.SQLConnection := FConn;
end;

destructor TFDBSQDBConnection.Destroy;
begin
  FTransact.Free;
  FQuery.Free;
  FConn.Free;
  inherited Destroy;
end;

procedure TFDBSQDBConnection.StartTransactV;
begin
  FTransact.StartTransaction;
end;

procedure TFDBSQDBConnection.CommitV;
begin
  FTransact.Commit;
end;

procedure TFDBSQDBConnection.RollbackV;
begin
  FTransact.Rollback;
end;

procedure TFDBSQDBConnection.PrepareV;
begin
  FQuery.sql.text := sql;
end;

procedure TFDBSQDBConnection.BindInt64V(AParamName: String; AParamValue: Int64);
begin
  FQuery.Params.ParamByName(AParamName).AsLargeInt := AParamValue;
end;

procedure TFDBSQDBConnection.BindIntegerV(AParamName: String; AParamValue: Integer);
begin
  FQuery.Params.ParamByName(AParamName).AsInteger := AParamValue;
end;

procedure TFDBSQDBConnection.BindKeyV(AParamName: String; AParamValue: Integer);
begin
  FQuery.Params.ParamByName(AParamName).AsInteger := AParamValue;
end;

procedure TFDBSQDBConnection.BindDoubleV(AParamName: String; AParamValue: Double);
begin
  FQuery.Params.ParamByName(AParamName).AsFloat := AParamValue;
end;

procedure TFDBSQDBConnection.BindStringV(AParamName: String; AParamValue: String);
begin
  FQuery.Params.ParamByName(AParamName).AsString := AParamValue;
end;

procedure TFDBSQDBConnection.BindTimeStampV(AParamName: String; AParamValue: TTimeStamp);
begin
  FQuery.Params.ParamByName(AParamName).AsDateTime := TSToDateTime(AParamValue);
end;

procedure TFDBSQDBConnection.BindDateTimeExV(AParamName: String; AParamValue: TFslDateTime);
begin
  FQuery.Params.ParamByName(AParamName).AsDateTime := AParamValue.DateTime;
end;

procedure TFDBSQDBConnection.BindBlobV(AParamName: String; AParamValue: TBytes);
begin
  FQuery.Params.ParamByName(AParamName).AsBytes := AParamValue;
end;

procedure TFDBSQDBConnection.BindNullV(AParamName: String);
begin
  FQuery.Params.ParamByName(AParamName).Clear;
end;

procedure TFDBSQDBConnection.ExecuteV;
begin
  FQuery.Open;
  FFirst := true;
end;

procedure TFDBSQDBConnection.TerminateV;
begin
  FQuery.Close;
end;

function TFDBSQDBConnection.FetchNextV: Boolean;
begin
  if FFirst then
    FFirst := false
  else
    FQuery.Next;
  result := not FQuery.Eof;
end;

function TFDBSQDBConnection.GetColCountV: Integer;
begin
  result := FQuery.FieldCount;
end;

function TFDBSQDBConnection.GetColStringV(ACol: Word): String;
begin
  result := FQuery.Fields[ACol].AsString;
end;

function TFDBSQDBConnection.GetColIntegerV(ACol: Word): Integer;
begin
  result := FQuery.Fields[ACol].AsInteger;
end;

function TFDBSQDBConnection.GetColInt64V(ACol: Word): Int64;
begin
  result := FQuery.Fields[ACol].AsLargeInt;
end;

function TFDBSQDBConnection.GetColDoubleV(ACol: Word): Double;
begin
  result := FQuery.Fields[ACol].AsFloat;
end;

function TFDBSQDBConnection.GetColBlobV(ACol: Word): TBytes;
begin
  result := FQuery.Fields[ACol].AsBytes;
end;

function TFDBSQDBConnection.GetColNullV(ACol: Word): Boolean;
begin
  result := FQuery.Fields[ACol].IsNull;
end;

function TFDBSQDBConnection.GetColTimestampV(ACol: Word): TTimestamp;
begin
  result := DateTimeToTs(FQuery.Fields[ACol].AsDateTime);
end;

function TFDBSQDBConnection.GetColDateTimeExV(ACol: Word): TFslDateTime;
begin
  result := TFslDateTime.makeUTC(FQuery.Fields[ACol].AsDateTime);
end;

function TFDBSQDBConnection.GetColTypeV(ACol: Word): TFDBColumnType;
begin
  result := ctUnknown;
  case FQuery.Fields[ACol].DataType of
    ftString, ftFixedChar, ftWideString, ftFixedWideChar, ftWideMemo :
      result := ctUnicode;

    ftBoolean :
      result := ctBoolean;

    ftSmallint, ftInteger, ftWord, ftAutoInc:
      result := ctInteger;

    ftLargeint :
      result := ctInt64;

    ftFloat, ftCurrency, ftBCD:
      result := ctFloat;

    ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftTypedBinary:
      result := ctBlob;

    ftDate, ftTime, ftDateTime, ftTimeStamp:
      result := ctDateTime;
  end;
end;

function TFDBSQDBConnection.GetColKeyV(ACol: Word): Integer;
begin
  result := FQuery.Fields[ACol].AsInteger;
end;

function TFDBSQDBConnection.GetRowsAffectedV: Integer;
begin
  result := FQuery.RowsAffected;
end;

procedure TFDBSQDBConnection.RenameTableV(AOldTableName, ANewTableName: String);
begin
  raise EDBException.create('Not done yet');
end;

procedure TFDBSQDBConnection.RenameColumnV(ATableName, AOldColumnName, ANewColumnName: String; AColumnDetails: String = '');
begin
  raise EDBException.create('Not done yet');
end;

procedure TFDBSQDBConnection.DropTableV(ATableName : String);
begin
  raise EDBException.create('Not done yet');
end;

procedure TFDBSQDBConnection.DropColumnV(ATableName, AColumnName : String);
begin
  raise EDBException.create('Not done yet');
end;

procedure TFDBSQDBConnection.ListTablesV(AList : TStrings);
begin
  raise EDBException.create('Not done yet');
end;

procedure TFDBSQDBConnection.ClearDatabaseV;
begin
  raise EDBException.create('Not done yet');
end;

function TFDBSQDBConnection.ColByNameV(AColName: String): Integer;
begin
  result := FQuery.FieldByName(AColName).Index;
end;

function TFDBSQDBConnection.ColNameV(ACol: Integer): String;
begin
  result := FQuery.Fields[ACol].Name;
end;

function TFDBSQDBConnection.DatabaseSizeV : int64;
begin
  raise EDBException.create('Not done yet');
end;

Function TFDBSQDBConnection.TableSizeV(sName : String):int64;
begin
  raise EDBException.create('Not done yet');
end;

function TFDBSQDBConnection.SupportsSizingV : Boolean;
begin
  raise EDBException.create('Not done yet');
end;

function TFDBSQDBConnection.sizeInBytesV(magic : integer) : cardinal;
begin
  raise EDBException.create('Not done yet');
end;

function TFDBSQDBConnection.FetchMetaDataV : TFDBMetaData;
begin
  raise EDBException.create('Not done yet');
end;


{ TFDBSQLDBManager }

constructor TFDBSQLDBManager.Create(AName : String; platform : TFDBPlatform; server, database, username, password : String; maxConn : integer = 100);
begin
  inherited Create(AName, maxConn);
  FPlatform := platform;
  FServer := server;
  FDatabase := database;
  FUsername := username;
  FPassword := password;
end;

destructor TFDBSQLDBManager.Destroy;
begin
  // nothing
  inherited Destroy;
end;

function TFDBSQLDBManager.GetDBProvider: TFDBProvider;
begin
  result := kdbpSQLDB;
end;

function TFDBSQLDBManager.GetDBPlatform: TFDBPlatform;
begin
  result := FPlatform;
end;

function TFDBSQLDBManager.GetDBDetails: String;
begin
  result := CODES_TFDBPlatform[FPlatform]+'://'+FServer+'/'+FDatabase+'@'+FUsername;
end;

function TFDBSQLDBManager.GetDriver: String;
begin
  result := 'SQLDB';
end;

procedure TFDBSQLDBManager.init;
begin
  // nothing?
end;

function TFDBSQLDBManager.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, (FServer.length * sizeof(char)) + 12);
  inc(result, (FDatabase.length * sizeof(char)) + 12);
  inc(result, (FUsername.length * sizeof(char)) + 12);
  inc(result, (FPassword.length * sizeof(char)) + 12);
end;

function TFDBSQLDBManager.makeMySqlConnection : TMySQL80Connection;
begin
  result := TMySQL80Connection.create(nil);
  result.hostName := FServer;
  result.DatabaseName := FDatabase;
  result.UserName := FUsername;
  result.Password := FPassword;
end;

function TFDBSQLDBManager.ConnectionFactory: TFDBConnection;
begin
  if (FPlatform = kdbMySQL) then
    result := TFDBSQDBConnection.create(self, makeMySqlConnection)
  else if (FPlatform = kdbSQLServer) then
    raise EDBException.create('not supported yet')
  else
    raise EDBException.create('not supported');
end;

end.

