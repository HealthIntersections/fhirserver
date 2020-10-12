{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

Library InstallHelper;

uses
  Windows,
  SysUtils,
  Dialogs,
  RegularExpressions,
  JclSysUtils,
  FHIR.Support.Base in '..\..\library\Support\FHIR.Support.Base.pas',
  FHIR.Support.Fpc in '..\..\library\Support\FHIR.Support.Fpc.pas',
  FHIR.Support.Utilities in '..\..\library\Support\FHIR.Support.Utilities.pas',
  FHIR.Server.Ini in '..\..\server\FHIR.Server.Ini.pas',
  FHIR.Database.ODBC.Objects in '..\..\library\database\FHIR.Database.ODBC.Objects.pas',
  FHIR.Database.Dialects in '..\..\library\database\FHIR.Database.Dialects.pas',
  FHIR.Database.ODBC.Headers in '..\..\library\database\FHIR.Database.ODBC.Headers.pas',
  FHIR.Database.ODBC in '..\..\library\database\FHIR.Database.ODBC.pas',
  FHIR.Database.Manager in '..\..\library\database\FHIR.Database.Manager.pas',
  FHIR.Support.Threads in '..\..\library\support\FHIR.Support.Threads.pas',
  FHIR.Database.Settings in '..\..\library\database\FHIR.Database.Settings.pas',
  FHIR.Database.Logging in '..\..\library\database\FHIR.Database.Logging.pas',
  FHIR.Npm.Cache in '..\..\library\npm\FHIR.Npm.Cache.pas',
  FHIR.Support.Json in '..\..\library\support\FHIR.Support.Json.pas',
  FHIR.Support.Collections in '..\..\library\support\FHIR.Support.Collections.pas',
  FHIR.Support.Stream in '..\..\library\support\FHIR.Support.Stream.pas',
  FHIR.Web.Fetcher in '..\..\library\web\FHIR.Web.Fetcher.pas',
  FHIR.Server.Version in '..\..\server\FHIR.Server.Version.pas',
  FHIR.Base.Lang in '..\..\library\base\FHIR.Base.Lang.pas',
  FHIR.Base.Objects in '..\..\library\base\FHIR.Base.Objects.pas',
  FHIR.Support.MXml in '..\..\library\support\FHIR.Support.MXml.pas',
  MarkdownHTMLEntities in '..\..\..\markdown\source\MarkdownHTMLEntities.pas',
  FHIR.Base.Utilities in '..\..\library\base\FHIR.Base.Utilities.pas',
  FHIR.Web.Parsers in '..\..\library\web\FHIR.Web.Parsers.pas',
  FHIR.Npm.Package in '..\..\library\npm\FHIR.Npm.Package.pas',
  FHIR.Npm.Client in '..\..\library\npm\FHIR.Npm.Client.pas';

function StrToPChar(AStr: AnsiString): PAnsiChar;
begin
  if AStr = '' then
    Result := NIL
  else
    begin
    AStr := AStr + #0;
    GetMem(Result, Length(AStr) + 1);
    Move(AStr[1], Result^, Length(AStr));
    end;
end;

procedure CommaAdd(var AStr: AnsiString; AStrToAdd: AnsiString);
begin
  if AStr = '' then
    AStr := AStrToAdd
  else
    AStr := AStr + ', ' + AStrToAdd;
end;

function GetString(name : AnsiString):AnsiString;
var
  odbc: TOdbcAdministrator;
  rex : TRegex;
  i : integer;
Begin
  if name = 'dbcfg-drivers' then // for mssql
  begin
    result := '';
    odbc := TOdbcAdministrator.create(TOdbcEnv.create);
    try
      odbc.DataSourceType := dsSystem;
      rex := TRegEx.create('^.*sql.*$', [roCompiled]);
      for i := 0 to odbc.Drivers.count - 1  do
        if rex.isMatch(lowercase(odbc.Drivers[i])) then
          CommaAdd(result, ansiString(odbc.Drivers[i]));
//      showmessage('drivers = '+result);
    finally
      odbc.free;
    end;
  end
  else if name = 'dbcfg-drivers-mysql' then
  begin
    result := '';
    odbc := TOdbcAdministrator.create(TOdbcEnv.create);
    try
      odbc.DataSourceType := dsSystem;
      rex := TRegEx.create('^.*mysql.*$', [roCompiled]);
      for i := 0 to odbc.Drivers.count -1  do
        if rex.isMatch(lowercase(odbc.Drivers[i])) then
          CommaAdd(result, ansiString(odbc.Drivers[i]));
//      showmessage('drivers = '+result);
    finally
      odbc.free;
    end;
  end
  else
    result := 'Unknown ('+name+')';
end;

Function MyDllGetIniValue(iniFile : PAnsiChar; name : PAnsiChar) : PAnsiChar; stdcall;
var
  ini : TFHIRServerIniFile;
  cmp : TFHIRServerIniComplex;
  n, r : String;
begin
  r := '';
  ini := TFHIRServerIniFile.Create(iniFile);
  try
    n := name;
    if n.StartsWith('db.') then
    begin
      if n.StartsWith('db.r2.') then      cmp := ini.databases['dbr2']
      else if n.StartsWith('db.r3.') then cmp := ini.databases['dbr3']
      else if n.StartsWith('db.r4.') then cmp := ini.databases['dbr4']
      else  cmp := nil;

      if (cmp <> nil) then
      begin
        if n.EndsWith('.server') then        r := cmp.value['server']
        else if n.EndsWith('.database') then r := cmp.value['database']
        else if n.EndsWith('.type') then     r := cmp.value['type']
        else if n.EndsWith('.driver') then   r := cmp.value['driver']
        else if n.EndsWith('.username') then r := cmp.value['username']
        else if n.EndsWith('.password') then r := cmp.value['password'];
      end;
    end
    else if n = 'web.host' then r := ini.web['host']
    else if n = 'web.http' then r := ini.web['http']
    else if n = 'web.https' then r := ini.web['https']
    else if n = 'web.certname' then r := ini.web['certname']
    else if n = 'web.cacertname' then r := ini.web['cacertname']
    else if n = 'admin.email' then r := ini.admin['email']
    else if n = 'admin.username' then r := ini.admin['username'];
  finally
    ini.Free;
  end;
  result := strToPchar(r);
end;

Procedure MyDllSetIniValue(iniFile : PAnsiChar; name, value : PAnsiChar); stdcall;
var
  ini : TFHIRServerIniFile;
  cmp : TFHIRServerIniComplex;
  n, v : String;
begin
  ini := TFHIRServerIniFile.Create(iniFile);
  try
    n := name;
    v := value;
    if n.StartsWith('db.') then
    begin
      if n.StartsWith('db.r2.') then
      begin
        if not ini.databases.ContainsKey('dbr2') then
          ini.databases.Add('dbr2', TFHIRServerIniComplex.create(''));
        cmp := ini.databases['dbr2'];
      end
      else if n.StartsWith('db.r3.') then
      begin
        if not ini.databases.ContainsKey('dbr3') then
          ini.databases.Add('dbr3', TFHIRServerIniComplex.create(''));
        cmp := ini.databases['dbr3']
      end
      else if n.StartsWith('db.r4.') then
      begin
        if not ini.databases.ContainsKey('dbr4') then
          ini.databases.Add('dbr4', TFHIRServerIniComplex.create(''));
        cmp := ini.databases['dbr4']
      end
      else
        cmp := nil;

      if (cmp <> nil) then
      begin
        if n.EndsWith('.server') then        cmp.value['server'] := v
        else if n.EndsWith('.database') then cmp.value['database'] := v
        else if n.EndsWith('.type') then     cmp.value['type'] := v
        else if n.EndsWith('.driver') then   cmp.value['driver'] := v
        else if n.EndsWith('.username') then cmp.value['username'] := v
        else if n.EndsWith('.password') then cmp.value['password'] := v;
      end;
    end
    else if n = 'endpoint.r2' then ini.endpoints.AddOrSetValue('r2', TFHIRServerIniComplex.create(v))
    else if n = 'endpoint.r3' then ini.endpoints.AddOrSetValue('r3', TFHIRServerIniComplex.create(v))
    else if n = 'endpoint.r4' then ini.endpoints.AddOrSetValue('r4', TFHIRServerIniComplex.create(v))
    else if n = 'web.host' then ini.web['host'] := v
    else if n = 'web.http' then ini.web['http'] := v
    else if n = 'web.https' then ini.web['https'] := v
    else if n = 'web.certname' then ini.web['certname'] := v
    else if n = 'web.cacertname' then ini.web['cacertname'] := v
    else if n = 'admin.email' then ini.admin['email'] := v
    else if n = 'admin.username' then ini.admin['username'] := v
    else if n = 'endpoint' then ini.admin['username'] := v
    else if n = 'tx.loinc' then ini.terminologies.AddOrSetValue('loinc', TFHIRServerIniComplex.create(v))
    else if n = 'tx.ucum' then ini.terminologies.AddOrSetValue('ucum', TFHIRServerIniComplex.create(v))
    else if n = 'tx.lang' then ini.terminologies.AddOrSetValue('lang', TFHIRServerIniComplex.create(v))
    else if (n = 'scim.salt') and (ini.admin['scim-salt'] = '') then ini.admin['scim-salt'] := NewGuidId;
    ini.save;
  finally
    ini.Free;
  end;
end;

Function MyDllGetString(name : PAnsiChar) : PAnsiChar; stdcall;
begin
  result := StrToPChar(GetString(name));
end;

Function MyDllCreateDatabase(DBDriver, Server, Database, Username, Password, Version : PAnsiChar) : PAnsiChar; stdcall;
begin
{
Work In progress, not sure how to achieve this.
Procedure is to connect to the DB (with which user?) and run script/qeries below

//// MySQL queries - tested:
CREATE DATABASE IF NOT EXISTS fhirDB CHARACTER SET utf8 COLLATE utf8_unicode_ci;
CREATE USER 'sa'@'%' IDENTIFIED BY 'DBroot123';
GRANT ALL PRIVILEGES ON *.* TO 'sa'@'%' IDENTIFIED BY 'DBroot123' WITH GRANT OPTION;


MySQL commmand lines could be executed in the bin directory of mysql :
mysql -u root -p -e "CREATE DATABASE fhirDB CHARACTER SET utf8 COLLATE utf8_unicode_ci;"
mysql -u root -p -e "CREATE USER 'sa'@'%' IDENTIFIED BY 'DBroot123';"
mysql -u root -p -e "GRANT ALL PRIVILEGES ON *.* TO 'sa'@'%' IDENTIFIED BY 'DBroot123' WITH GRANT OPTION;"

Could be:
ShellExecute(0, 'open', 'mysql.exe', '-u root -p -e "CREATE DATABASE fhirDB CHARACTER SET utf8 COLLATE utf8_unicode_ci;"', 'path_to_bin_folder', SW_HIDE);
...


//// MS SQL Server queries - not yet tested:
CREATE DATABASE IF NOT EXISTS fhirDB
USE fhirDB;
exec sp_configure 'contained database authentication', 1;
reconfigure;
alter database fhirDB
set containment = partial
CREATE USER sa WITH PASSWORD = 'DBroot123';
EXEC sp_addrolemember N'db_owner', N'sa'
}
end;


Function MyDllCheckDatabase(DBDriver, Server, Database, Username, Password, Version : PAnsiChar) : PAnsiChar; stdcall;
var
  state : integer;
  conn : TFslDBManager;
  db : TFslDBConnection;
  meta : TFslDBMetaData;
  fver : String;
  dver : integer;
begin
  state := 1;
  try
    conn := TFslDBOdbcManager.Create('config', RecogniseDriver(DBDriver), 1, 0, dbDriver, server, database, username, password);
    try
      db := conn.GetConnection('test');
      try
        meta := db.FetchMetaData;
        try
          state := 2;
          if meta.HasTable('Config') and meta.HasTable('OAuthLogins') and meta.HasTable('AuthorizationSessions') then
          begin
            try
              dver := StrToIntDef(db.Lookup('Config', 'ConfigKey', '5', 'Value', ''), 0);
              fver := db.Lookup('Config', 'ConfigKey', '8', 'Value', '');
              if (dver = 0) then
                result := StrToPchar('2No Version Marker found in database')
              else if (dver < ServerDBVersionEarliestSupported) or (dver > ServerDBVersion) then
                result := StrToPchar('3Unsupported database version '+inttostr(dver))
              else if (fver <> version) then
                result := StrToPchar('3Wrong FHIR version found ('+fver+')')
              else
                result := StrToPchar('4Database ready for use');
            except
              on e : exception do
                result := StrToPchar(inttostr(state)+e.message);
            end;
          end
          else
            result := '2Database exists but has not previously been installed';
        finally
          meta.free;
        end;
        db.Release;
      except
        on e : exception do
        begin
          db.Error(e);
          raise;
        end;
      end;
    finally
      conn.free;
    end;
  except
    on e:exception do
      result := StrToPchar(inttostr(state)+e.Message);
  end;
end;

Type
  TInstallerCallback = procedure(IntParam: Integer; StrParam: WideString) of object;

  TCommandCapturer = class (TFslObject)
  private
    carry : String;
    callback : TInstallerCallback;
    procedure process(dBuffer: array of Char);
    procedure processLine(s : String);
  public
    procedure Execute(CommandLine, WorkDir : string);
  end;

procedure msg(s : String);
begin
  MessageBox(0, PWideChar(s), 'installer', MB_OK);
end;

procedure TCommandCapturer.Execute(CommandLine, WorkDir: string);
const
  CReadBuffer = 2400;
var
  saSecurity: TSecurityAttributes;
  hRead: THandle;
  hWrite: THandle;
  suiStartup: TStartupInfo;
  piProcess: TProcessInformation;
  pBuffer: array [0 .. CReadBuffer] of AnsiChar;
  dBuffer: array [0 .. CReadBuffer] of Char;
  dRead: DWORD;
  dRunning: DWORD;
  dAvailable: DWORD;
begin
  saSecurity.nLength := SizeOf(TSecurityAttributes);
  saSecurity.bInheritHandle := true;
  saSecurity.lpSecurityDescriptor := nil;
  if CreatePipe(hRead, hWrite, @saSecurity, 0) then
    try
      FillChar(suiStartup, SizeOf(TStartupInfo), #0);
      suiStartup.cb := SizeOf(TStartupInfo);
      suiStartup.hStdInput := hRead;
      suiStartup.hStdOutput := hWrite;
      suiStartup.hStdError := hWrite;
      suiStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      suiStartup.wShowWindow := SW_HIDE;
      if CreateProcess(nil, PChar(CommandLine), @saSecurity, @saSecurity, true, NORMAL_PRIORITY_CLASS, nil, pchar(workdir), suiStartup,
        piProcess) then
        try
          msg('executed');
          repeat
            dRunning := WaitForSingleObject(piProcess.hProcess, 100);
            PeekNamedPipe(hRead, nil, 0, nil, @dAvailable, nil);
            if (dAvailable > 0) then
              repeat
                dRead := 0;
                ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
                pBuffer[dRead] := #0;
                OemToCharW(pBuffer, dBuffer);
                try
                  Process(dBuffer);
                except
                  TerminateProcess(piProcess.hProcess, 0);
                  raise;
                end;
              until (dRead < CReadBuffer);
              sleep(50);
          until (dRunning = WAIT_TIMEOUT);
        finally
          CloseHandle(piProcess.hProcess);
          CloseHandle(piProcess.hThread);
        end
      else
        RaiseLastWin32Error;
    finally
      CloseHandle(hRead);
      CloseHandle(hWrite);
    end;
end;

procedure TCommandCapturer.process(dBuffer: array of Char);
var
  s : String;
  lines : TArray<String>;
  i : integer;
begin
  s := carry+dBuffer;
  lines := s.split(['♪◙']);
  for i := 0 to length(lines) - 2 do
    processLine(lines[i]);
  carry := lines[length(lines) - 1];
end;

procedure TCommandCapturer.processLine(s: String);
var
  l, r : String;
begin
  if (s <> '') then
  begin
    msg(s);
    if (s.StartsWith('##> ')) then // ignore the rest
    begin
      StringSplit(s.Substring(4), ' ', l, r);
      if SameText(l, 'Exception') then
        raise ELibraryException.create(r)
      else
        CallBack(StrToIntDef(l, 0), r);
    end;
  end;
end;

function isUser(p : pAnsiChar) : boolean;
begin
  result := (p = 'u') or (p = 'user');
end;

function normaliseVersion(v : String): String;
begin
  if (v = 'STU3') then
    result := '3.0'
  else if (v = 'DSTU3') then
    result := '1.0'
  else if (v = 'R3') then
    result := '3.0'
  else if (v = 'R2') then
    result := '1.0'
  else if (v = 'R4') then
    result := '4.0'
  else if (v.Length > 3) then
    result := v.Substring(0, 3)
  else
    result := v;
end;

function versionMatches(v1, v2 : String): boolean;
begin
  v1 := normaliseVersion(v1);
  v2 := normaliseVersion(v2);
  result := v1 = v2;
end;

procedure listPackages(server : String; packages : TFslList<TFHIRPackageInfo>);
var
  pc : TFHIRPackageClient;
  l : TFslList<TFHIRPackageInfo>;
  p,t  : TFHIRPackageInfo;
  ok : boolean;
begin
  pc := TFHIRPackageClient.Create(server);
  try
    l := pc.search('', '', '', false);
    try
      for t in l do
      begin
        ok := true;
        for p in packages do
        begin
          if (p.id = t.id) and (p.version = t.version) then
            ok := false;
        end;
        if ok then
          packages.Add(t.link);
      end;
    finally
      l.Free;
    end;
  finally
    pc.Free;
  end;
end;

Function MyDllListPackages(mode : PAnsiChar; Version : PAnsiChar) : PAnsiChar; stdcall;
var
  packages, l : TFslList<TFHIRPackageInfo>;
  p : TFHIRPackageInfo;
  b : TFslStringBuilder;
  pcm : TFHIRPackageManager;
begin
  try
    b := TFslStringBuilder.create;
    try
      packages := TFslList<TFHIRPackageInfo>.create;
      try
        listPackages(PACKAGE_SERVER_PRIMARY, packages);
        listPackages(PACKAGE_SERVER_BACKUP, packages);
        packages.SortF(function (const l, r : TFHIRPackageInfo) : Integer
          begin
            result := CompareText(l.id, r.id);
          end);
        pcm := TFHIRPackageManager.Create(false);
        try
          for p in packages do
          begin
            if versionMatches(p.FHIRVersion, version) then
            begin
              if b.Length > 0 then
                b.Append(#13#10);
              if pcm.packageExists(p.Id, p.Version) then
                b.Append(p.Url+'|1|'+p.Id+'#'+p.Version+'|'+p.Description.replace('|', ':').replace(#13, '').replace(#10, ' '))
              else
                b.Append(p.Url+'|0|'+p.Id+'#'+p.Version+'|'+p.Description.replace('|', ':').replace(#13, '').replace(#10, ' '));
            end;
          end;
        finally
          pcm.Free;
        end;
      finally
        packages.Free;
      end;
      result := StrToPchar(b.AsString);
    finally
      b.Free;
    end;
  except
    on e : exception do
      result := StrToPchar('Error: '+e.Message);
  end;
end;

type
  TPackageFetcher = class (TFslObject)
  private
    FCallback : TInstallerCallback;
    FUrls : TArray<String>;
    FIndex : integer;
    FCurrent : String;
    FUserMode : boolean;

    function pct(p : integer) : integer;
    procedure fetchProgress(sender : TObject; progress : integer);
    procedure exec;
    function check(sender : TObject; msg : String) : boolean;
    function getUrl(packages : TFslList<TPackageDefinition>; pi, pv : String) : String;
  end;

function TPackageFetcher.check(sender: TObject; msg: String): boolean;
begin
  result := true;
end;

procedure TPackageFetcher.exec;
var
  pcm : TFHIRPackageManager;
  fetch : TInternetFetcher;
  ok : boolean;
  aborted : boolean;
  s : String;
  i : integer;
  pi, pv, url : String;
  packages : TFslList<TPackageDefinition>;
begin
  packages := TFslList<TPackageDefinition>.create;
  try
    pcm := TFHIRPackageManager.Create(FUserMode);
    try
      pcm.OnCheck := check;
      for i := 0 to length(FUrls) - 1 do // first will be empty
      begin
        FIndex := i;
        pi := FUrls[i];
        FCurrent := pi;
        StringSplit(pi, '#', pi, pv);
        if not pcm.packageExists(pi, pv) then
        begin
          url := getUrl(packages, pi, pv);
          fetch := TInternetFetcher.Create;
          try
            fetch.onProgress := fetchProgress;
            fetch.Buffer := TFslBuffer.create;
            s := '';
            ok := false;
            try
              fetch.URL := URLPath([url, 'package.tgz']);
              fetch.Fetch;
              ok := (fetch.ContentType = 'application/x-compressed') or (fetch.ContentType = 'application/octet-stream') or (fetch.ContentType = 'application/x-tar');
            except
              on e : exception do
              begin
                s := e.Message;
              end;
            end;
            if not ok then
            begin
              try
                fetch.URL := url;
                fetch.Fetch;
                ok := (fetch.ContentType = 'application/x-compressed') or (fetch.ContentType = 'application/octet-stream');
              except
                on e : exception do
                begin
                  s := e.Message;
                end;
              end;
            end;
            if not ok then
              raise EIOException.create('Unable to find package for '+pi+'#'+pv+' at '+url+': '+s);
            if ok then
            begin
              FCallback(pct(100), 'Installing '+FCurrent);
              pcm.Import(fetch.Buffer.AsBytes);
            end;
          finally
            fetch.Free;
          end;
        end;
        FCallback(pct(i+1), 'Installed '+FCurrent);
      end;
      FCallback(100, 'Done...');
    finally
      pcm.Free;
    end;
  finally
    packages.Free;
  end;
end;

procedure TPackageFetcher.fetchProgress(sender: TObject; progress: integer);
begin
  FCallback(pct(progress), 'Downloading '+FCurrent);
end;


function TPackageFetcher.getUrl(packages: TFslList<TPackageDefinition>; pi, pv: String): String;
var
  p : TPackageDefinition;
begin
  result := '';
  for p in packages do
    if (p.id = pi) and (p.Version = pv) then
      exit(p.Url);
end;

function TPackageFetcher.pct(p: integer): integer;
var
  d : double;
begin
  d := p / (100 * length(FUrls)) + FIndex / length(FUrls);
  result := trunc(d * 100);
end;

Function MyDllDownloadPackages(mode : PAnsiChar; urls : PAnsiChar; Callback: TInstallerCallback) : PAnsiChar; stdcall;
var
  fetcher : TPackageFetcher;
  s : String;
begin
  try
    fetcher := TPackageFetcher.Create;
    try
      fetcher.FUserMode := isUser(mode);
      fetcher.FCallback := Callback;
      s := urls;
      s := s.Substring(1);
      fetcher.FUrls := s.Split([',']);
      fetcher.exec;
    finally
      fetcher.free;
    end;
    result := nil;
  except
    on e : exception do
      result := StrToPchar(e.Message);
  end;
end;

Function MyDllInstallSnomed(ExeName, Source, Dest, Version : PAnsiChar; Callback: TInstallerCallback) : PAnsiChar; stdcall;
var
  cmd : TCommandCapturer;
begin
  try
    cmd := TCommandCapturer.Create;
    try
      cmd.callback := Callback;
      cmd.Execute('"'+exename+'" -installer -snomed-rf2 "'+Source+'" -sver "'+Version+'" -sdest "'+dest+'"', ExtractFilePath(exeName));
    finally
      cmd.Free;
    end;
    result := nil;
  except
    on e:exception do
      result := StrToPchar(e.Message);
  end;
end;

type
  THandlingObject = class (TObject)
  private
    Callback: TInstallerCallback;
    procedure handler(const s: string);
  end;

Function MyDllInstallDatabase(ExeName, IniFile, Password, version, packages, mode : PAnsiChar; Callback: TInstallerCallback) : PAnsiChar; stdcall;
var
  s : String;
  o : THandlingObject;
begin
  try
    o := THandlingObject.create;
    try
      o.Callback := Callback;
      s := '"'+exename+'" -cmd remount -installer -password "'+Password+'" -ini "'+IniFile+'" -packages '+packages+' -endpoint '+version+' -mode '+mode;
//      showmessage(s);
      Execute(s, o.handler);
      result := nil;
    finally
      o.Free;
    end;
  except
    on e : Exception do
      result := StrToPchar(e.message);
  end;
end;
{var
  cmd : TCommandCapturer;
  s : String;
begin
  try         !
    cmd := TCommandCapturer.Create;
    try
      cmd.callback := Callback;
      s := '"'+exename+'" -cmd remount -installer -password "'+Password+'" -ini "'+IniFile+'" -packages '+packages+' -endpoint '+version+'';
      windows.MessageBox(0, pchar(s), 'install cmd', MB_SYSTEMMODAL);
      cmd.Execute('"'+exename+'" -cmd remount -installer -password "'+Password+'" -ini "'+IniFile+'" -packages '+packages+' -endpoint '+version, ExtractFilePath(exeName));
    finally
      cmd.Free;
    end;
    result := nil;
  except
    on e:exception do
      result := StrToPchar(e.Message);
  end;
end;
}

exports MyDllGetString;
exports MyDllGetIniValue;
exports MyDllSetIniValue;
exports MyDllCheckDatabase;
//exports MyDllInstallSnomed;
exports MyDllInstallDatabase;
exports MyDllListPackages;
exports MyDllDownloadPackages;

{ THandlingObject }

procedure THandlingObject.handler(const s : string);
var
  l, r : String;
begin
  if (s.StartsWith('##> ')) then // ignore the rest
  begin
    StringSplit(s.Substring(4), ' ', l, r);
    if SameText(l, 'Exception') then
      raise ELibraryException.create(r)
    else
      CallBack(StrToIntDef(l, 0), r);
  end;
end;

End.

