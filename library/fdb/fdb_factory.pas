unit fdb_factory;

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
  fdb_dialects,
  fdb_manager,
  fdb_settings;

type
  EKDBFactoryException = class(EKDBException);

procedure KDBConvertSettings(ASettings : TSettingsAdapter);

function KDBDescribeDB(ASettings : TSettingsAdapter; APlatforms : TFDBPlatforms; var VDescription : String) : Boolean;
function KDBManagerFactory(AProvider : TFDBProvider) : TFDBManagerClass; overload;
function KDBManagerFactory(AProvider : String) : TFDBManagerClass; overload;
function KDBManagerFactory(AName : String; ASettings : TSettingsAdapter; AIdent : String = '') : TFDBManager; overload;
function DecryptPassword(sPassword : String) : String;

implementation

uses
  IdSoapUtilities,
  KProcs,
  {$IFDEF VER140}
  KDBDBExpress,
  {$ENDIF}
  {$IFNDEF UNICODE}
  KDBFirebird,
  KDBOdbcExpress,
  {$ENDIF}
  KDBMySQLConn,
  KDBSoapClient,
  SysUtils;

const
  ASSERT_UNIT = 'fdb_factory';

procedure KDBConvertSettings(ASettings : TSettingsAdapter);
var
  LPlatform : TFDBPlatform;
  s : String;
begin
// convert from old style format to new style format
  if (ASettings.ReadString('Factory') <> 'new') and ((ASettings.ReadString('SymbolicName') <> '') or (ASettings.ReadString('Name') <> '')) then
    begin
    ASettings.WriteString('Name', ASettings.ReadString('SymbolicName'));
    if ASettings.ReadInteger('Platform', -1) <> -1 then
      begin
      LPlatForm := TFDBPlatform(ASettings.ReadInteger('Platform'));
      end
    else
      begin
      s := ASettings.ReadString('Platform');
      if StringStartsWith(s, 'kdbSQLServer', true) then
        LPlatForm := kdbSQLServer
      else
        LPlatForm := TFDBPlatform(IdStringToEnum(TypeInfo(TFDBPlatform), s));
      end;
    case LPlatForm of
      kdbDBIsam :
         begin
         ASettings.WriteString('Provider', IdEnumToString(TypeInfo(TFDBProvider), ord(kdbpDBIsam)));
         ASettings.WriteString('Directory', ASettings.ReadString('DatabaseDirectory'));
         end;
      kdbInterbase :
         begin
         ASettings.WriteString('Provider', IdEnumToString(TypeInfo(TFDBProvider), ord(kdbpFirebird)));
         // other settings OK
         end;
    else
      // odbc
      if ASettings.ReadString('DSN') <> '' then
        begin
        ASettings.WriteString('Provider', IdEnumToString(TypeInfo(TFDBProvider), ord(kdbpDSN)));
        end
      else
        begin
        ASettings.WriteString('Provider', IdEnumToString(TypeInfo(TFDBProvider), ord(kdbpODBC)));
        ASettings.WriteString('ODBCDriver', ASettings.ReadString('Driver'));
        end;
    end;
    ASettings.WriteString('Factory', 'new');
    end;
end;

function KDBDescribeDB(ASettings : TSettingsAdapter; APlatforms : TFDBPlatforms; var VDescription : String) : Boolean;
var
  LProv : TFDBProvider;
  LPlat : TFDBPlatform;
  s : string;
begin
  try
    if ASettings.ReadString('Provider', '') = '' then
      begin
      raise EKDBFactoryException.create('Database access not configured');
      end;
    LProv := TFDBProvider(IdStringToEnum(TypeInfo(TFDBProvider), ASettings.ReadString('Provider', '')));
    if ASettings.ReadInteger('Platform', -1) <> -1 then
      begin
      LPlat := TFDBPlatform(ASettings.ReadInteger('Platform', 0));
      end
    else
      begin
      s := ASettings.ReadString('Platform', '');
      if StringStartsWith(s, 'kdbSQLServer', true) then
        LPlat := kdbSQLServer
      else
        LPlat := TFDBPlatform(IdStringToEnum(TypeInfo(TFDBPlatform), s));
      end;
    if not (LPlat in APlatforms) then
      begin
      raise EKDBFactoryException.create('Platform '+ASettings.ReadString('Platform', '')+' is not supported');
      end;
    case LProv of
      kdbpUnknown : raise EKDBFactoryException.create('unknown provider');
      kdbpDSN : VDescription := ASettings.ReadString('DSN')+' ['+ASettings.ReadString('Username')+']';
      kdbpODBC : VDescription := '\\'+ASettings.ReadString('Server')+'\'+ASettings.ReadString('Database')+' ['+ASettings.ReadString('Username')+']';
      kdbpFirebird : VDescription := '\\'+ASettings.ReadString('Server')+'\'+ASettings.ReadString('Database')+' ['+ASettings.ReadString('Username')+']';
      kdbpDBIsam : VDescription := ASettings.ReadString('Directory');
      kdbpDBXpress : raise EKDBFactoryException.create('DBXpress not handled yet');
      kdbpSoapClient : VDescription := ASettings.ReadString('URL');
      kdbpMySQL : VDescription := '\\'+ASettings.ReadString('Server')+'\'+ASettings.ReadString('Database')+' ['+ASettings.ReadString('Username')+']';
      kdbpAccess : VDescription := ASettings.ReadString('Database');
    else
      raise EKDBFactoryException.create('Unexpected Provider');
    end;
    VDescription := DescribePlatform(LPlat)+': '+VDescription;
    result := true;
  except
    on e:exception do
      begin
      result := false;
      VDescription := 'Error: '+e.Message;
      end;
  end;
end;

function KDBManagerFactory(AProvider : TFDBProvider) : TFDBManagerClass;
const ASSERT_LOCATION = ASSERT_UNIT+'.KDBManagerFactory1';
begin
  case AProvider of
  {$IFNDEF LINUX}
    kdbpDSN : result := TFDBOdbcDSN;
    kdbpODBC : result := TFDBOdbcDirect;
    kdbpFirebird : result := TFireBirdConnMan;
    kdbpSoapClient : result := TFDBSoapConnMan;
    kdbpAccess : result := TFDBOdbcDirect;
    kdbpMySQL : result := TMySQLConnMan;
  {$ENDIF}
  {$IFDEF VER140}
    kdbpDBXpress : result := TDBExpressConnMan;
  {$ENDIF}
  else
    // kdbpUnknown,
    raise EKDBFactoryException.create(ASSERT_LOCATION+': the provider '+IdEnumToString(TypeInfo(TFDBProvider), ord(AProvider))+' is not known or not supported by this system');
  end;
end;

function KDBManagerFactory(AProvider : String) : TFDBManagerClass;
const ASSERT_LOCATION = ASSERT_UNIT+'.KDBManagerFactory2';
begin
  result := KDBManagerFactory(TFDBProvider(IdStringToEnum(TypeInfo(TFDBProvider), AProvider)));
end;

function KDBManagerFactory(AName : String; ASettings : TSettingsAdapter; AIdent : String = '') : TFDBManager;
const ASSERT_LOCATION = ASSERT_UNIT+'.KDBManagerFactory3';
begin
  if AName = '' then
    begin
    AName := ASettings.ReadString('Name');
    end;
  result := KDBManagerFactory(ASettings.ReadString('Provider', '')).Create(AName, ASettings, AIdent);
end;

function DecryptPassword(sPassword : String) : String;
begin
  Result := strDecrypt(sPassword, 19278);
end;

end.


