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
