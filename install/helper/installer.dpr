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
  FastMM4 in '..\..\Libraries\FMM\FastMM4.pas',
  WIndows,
  SysUtils,
  RegularExpressions,
  FastMM4Messages in '..\..\Libraries\FMM\FastMM4Messages.pas',
  FHIR.Support.Strings in '..\..\reference-platform\Support\FHIR.Support.Strings.pas',
  FHIR.Support.Math in '..\..\reference-platform\Support\FHIR.Support.Math.pas',
  FHIR.Support.DateTime in '..\..\reference-platform\Support\FHIR.Support.DateTime.pas',
  FHIR.Support.Binary in '..\..\reference-platform\Support\FHIR.Support.Binary.pas',
  FHIR.Support.Objects in '..\..\reference-platform\Support\FHIR.Support.Objects.pas',
  FHIR.Support.Exceptions in '..\..\reference-platform\Support\FHIR.Support.Exceptions.pas',
  FHIR.Support.System in '..\..\reference-platform\Support\FHIR.Support.System.pas',
  FHIR.Support.Collections in '..\..\reference-platform\Support\FHIR.Support.Collections.pas',
  FHIR.Support.Stream in '..\..\reference-platform\Support\FHIR.Support.Stream.pas',
  FHIR.Database.Manager in '..\..\Libraries\db\FHIR.Database.Manager.pas',
  FHIR.Support.Lock in '..\..\reference-platform\support\FHIR.Support.Lock.pas',
  FHIR.Database.Settings in '..\..\Libraries\db\FHIR.Database.Settings.pas',
  FHIR.Database.Logging in '..\..\Libraries\db\FHIR.Database.Logging.pas',
  FHIR.Database.Dialects in '..\..\reference-platform\support\FHIR.Database.Dialects.pas',
  FHIR.Support.Decimal in '..\..\reference-platform\support\FHIR.Support.Decimal.pas',
  FHIR.Support.Generics in '..\..\reference-platform\support\FHIR.Support.Generics.pas',
  FHIR.Database.ODBC.Objects in '..\..\Libraries\db\FHIR.Database.ODBC.Objects.pas',
  FHIR.Database.ODBC in '..\..\Libraries\db\FHIR.Database.ODBC.pas',
  FHIR.Database.ODBC.Headers in '..\..\Libraries\db\FHIR.Database.ODBC.Headers.pas',
  fhir.support.fpc in '..\..\reference-platform\support\fhir.support.fpc.pas',
  FHIR.Support.Text in '..\..\reference-platform\support\FHIR.Support.Text.pas',
  FHIR.Cache.PackageManager in '..\..\reference-platform\cache\FHIR.Cache.PackageManager.pas',
  FHIR.Support.Json in '..\..\reference-platform\support\FHIR.Support.Json.pas',
  FHIR.Support.Tarball in '..\..\reference-platform\support\FHIR.Support.Tarball.pas',
  FHIR.Web.Fetcher in '..\..\reference-platform\support\FHIR.Web.Fetcher.pas';

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
  conn : TKDBManager;
  db : TKDBConnection;
  meta : TKDBMetaData;
  ver : String;
begin
  try
    conn := TKDBOdbcManager.Create('config', 1, 0, dbDriver, server, database, username, password);
    try
      db := conn.GetConnection('test');
      try
        meta := db.FetchMetaData;
        try
          try
            ver := db.Lookup('Config', 'ConfigKey', '8', 'Value', '');
            if (ver <> Version) then
              result := StrToPchar(ver)
            else
              result := nil;
          except
            result := StrToPchar('dbi');
          end;
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
      result := StrToPchar(e.Message);
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
//  MessageBox(0, PWideChar(s), 'installer', MB_OK);
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
  msg(commandLine);
  saSecurity.nLength := SizeOf(TSecurityAttributes);
  saSecurity.bInheritHandle := true;
  saSecurity.lpSecurityDescriptor := nil;
  if CreatePipe(hRead, hWrite, @saSecurity, 0) then
    try
      msg('piped');
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
          until (dRunning <> WAIT_TIMEOUT);
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

Function MyDllListPackages(Version : PAnsiChar) : PAnsiChar; stdcall;
var
  pcm : TFHIRPackageManager;
  packages : TFslList<TPackageDefinition>;
  p : TPackageDefinition;
  b : TFslStringBuilder;
begin
  try
    b := TFslStringBuilder.create;
    try
      pcm := TFHIRPackageManager.Create(false);
      try
        packages := TFslList<TPackageDefinition>.create;
        try
          TPackageDefinition.addStandardPackages(packages);
          TPackageDefinition.addPackagesFromBuild(packages);
          for p in packages do
          begin
            if p.FHIRVersion = version then
            begin
              if b.Length > 0 then
                b.Append(#13#10);
              if pcm.packageExists(p.Id, p.Version) then
                b.Append(p.Url+'|1|'+p.Id+'-'+p.Version+'|'+p.Description)
              else
                b.Append(p.Url+'|0|'+p.Id+'-'+p.Version+'|'+p.Description);
            end;
          end;
        finally
          packages.Free;
        end;
      finally
        pcm.Free;
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
    function pct(p : integer) : integer;
    procedure fetchProgress(sender : TObject; progress : integer);
    procedure exec;
    function check(sender : TObject; msg : String) : boolean;
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
begin
  pcm := TFHIRPackageManager.Create(false);
  try
    pcm.OnCheck := check;
    for i := 0 to length(FUrls) - 1 do // first will be empty
    begin
      FIndex := i;
      StringSplit(FUrls[i], ':', pi, url);
      FCurrent := pi;
      StringSplit(pi, '-', pi, pv);
      if not pcm.packageExists(pi, pv) then
      begin
        fetch := TInternetFetcher.Create;
        try
          fetch.onProgress := fetchProgress;
          fetch.Buffer := TFslBuffer.create;
          s := '';
          ok := false;
          try
            fetch.URL := URLPath([url, 'package.tgz']);
            fetch.Fetch;
            ok := (fetch.ContentType = 'application/x-compressed') or (fetch.ContentType = 'application/octet-stream');
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
            raise EIOException.create('Unable to find package for '+pi+'-'+pv+'at '+url+': '+s);
          if ok then
          begin
            FCallback(pct(100), 'Installing '+FCurrent);
            pcm.Import(fetch.Buffer.AsBytes);
          end;
        finally
          fetch.Free;
        end;
      end;
      FCallback(pct(100), 'Installed '+FCurrent);
    end;
    FCallback(100, 'Done...');
  finally
    pcm.Free;
  end;
end;

procedure TPackageFetcher.fetchProgress(sender: TObject; progress: integer);
begin
  FCallback(pct(progress), 'Downloading '+FCurrent);
end;


function TPackageFetcher.pct(p: integer): integer;
var
  d : double;
begin
  d := p / (100 * length(FUrls)) + FIndex / length(FUrls);
  result := trunc(d * 100);
end;

Function MyDllDownloadPackages(urls : PAnsiChar; Callback: TInstallerCallback) : PAnsiChar; stdcall;
var
  fetcher : TPackageFetcher;
  s : String;
begin
  try
    fetcher := TPackageFetcher.Create;
    try
      fetcher.FCallback := Callback;
      s := urls;
      s := s.Substring(1);
      fetcher.FUrls := s.Split(['|']);
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

Function MyDllInstallDatabase(ExeName, IniFile, Password : PAnsiChar; load, packages : PAnsiChar; Callback: TInstallerCallback) : PAnsiChar; stdcall;
var
  cmd : TCommandCapturer;
  s : String;
begin
  try
    cmd := TCommandCapturer.Create;
    try
      cmd.callback := Callback;
      if (load = nil) then
      begin
        s := '"'+exename+'" -installer -remount -password "'+Password+'" -ini "'+IniFile+'"';
        windows.MessageBox(0, pchar(s), 'install', MB_SYSTEMMODAL);
        cmd.Execute('"'+exename+'" -installer -remount -password "'+Password+'" -ini "'+IniFile+'+"', ExtractFilePath(exeName))
      end
      else
      begin
        s := '"'+exename+'" -installer -remount -password "'+Password+'" -ini "'+IniFile+'" -packages '+packages+' -load "'+load+'"';
        windows.MessageBox(0, pchar(s), 'install', MB_SYSTEMMODAL);
        cmd.Execute('"'+exename+'" -installer -remount -password "'+Password+'" -ini "'+IniFile+'" -packages '+packages+' -load "'+load+'"', ExtractFilePath(exeName));
      end;
    finally
      cmd.Free;
    end;
    result := nil;
  except
    on e:exception do
      result := StrToPchar(e.Message);
  end;
end;

exports MyDllGetString;
exports MyDllCheckDatabase;
exports MyDllInstallSnomed;
exports MyDllInstallDatabase;
exports MyDllListPackages;
exports MyDllDownloadPackages;

End.

