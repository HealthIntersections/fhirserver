program InstallerTester;

{$APPTYPE CONSOLE}

uses
  Windows,
  System.SysUtils;

Type
  TInstallerCallback = procedure(IntParam: Integer; StrParam: WideString) of object;
  TInstallSnomedFunction = Function (DLLName, Source, Dest, Version : PAnsiChar; Callback: TInstallerCallback) : PAnsiChar; stdcall;
  TInstallDatabaseFunction = Function (DLLName, IniFile, Password, Load : PAnsiChar; Callback: TInstallerCallback) : PAnsiChar; stdcall;

  TInstallerCallbackHandler = class (TObject)
  private
    procedure Callback(IntParam: Integer; StrParam: WideString);
  end;

{ TInstallerCallbackHandler }

procedure TInstallerCallbackHandler.Callback(IntParam: Integer; StrParam: WideString);
begin
  writeln(strParam, ' ', intParam);
end;

var
  dll : THandle;
  funcSCT : TInstallSnomedFunction;
  funcDB : TInstallDatabaseFunction;
  cb : TInstallerCallbackHandler;
  msg : PAnsiChar;
begin
  try
    Writeln('Installation tester.');
    Writeln('');
    cb := TInstallerCallbackHandler.create;
    dll := LoadLibraryA('C:\work\fhirserver\install\installer.dll');
    try
      @funcSCT := GetProcAddress(dll, 'MyDllInstallSnomed');
      @funcDB := GetProcAddress(dll, 'MyDllInstallDatabase');
      msg := funcSCT('C:\work\fhirserver\Server\win64_3\Debug\fhirserver3.exe', 'C:\data\terminologies\sct-au\20160430', 'C:\Program Files\FHIRServer\snomed_32506021000036107_20160531.cache', 'http://snomed.info/sct/32506021000036107/version/20160531', cb.Callback);
      if (msg <> nil) then
        Writeln('Error: ', msg);
      msg := funcDB('C:\work\fhirserver\Server\win64_3\Debug\fhirserver3.exe', 'C:\Program Files\FhirServer\fhirserver.ini', 'g', 'C:\Program Files\FhirServer\load\load.ini', cb.Callback);
      if (msg <> nil) then
        Writeln('Error: ', msg);
    finally
      cb.Free;
      FreeLibrary(dll);
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
