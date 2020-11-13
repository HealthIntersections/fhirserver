unit fhir_validation_wrapper;

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

This code is not functional at this time. It may be brought back to life in the future. 

{$i fhir.inc}



interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  SysUtils, Classes,
  fsl_base, fsl_utilities,
  fsl_npm_cache,
  FHIR.Base.Lang;

type
  TOutputLineEvent = procedure (Sender : TObject; line : String) of Object;
  TArg<T> = reference to procedure(const Arg: T);

  TFHIRValidationWrapper = class (TFslObject)
  private
    // context
    FCache : TFHIRPackageManager;
    FJarPath : String;

    // settings
    FSource: String;
    FNative: boolean;
    FPackages: TStringList;
    FOthers: TStringList;
    FVersion: String;
    FOnOutput: TOutputLineEvent;
    FTxServer : String;
    Fmap: String;
    FDest: String;
    FProfile: String;

    procedure executeCommand(cmd : String; CallBack: TArg<PAnsiChar>);
  public
    constructor Create(Cache : TFHIRPackageManager);
    destructor Destroy; override;
    function link : TFHIRValidationWrapper;

    function validateCmd: string;
    function transformCmd: string;

    procedure validate;
    procedure transform;

    property version : String read FVersion write FVersion;
    property Packages : TStringList read FPackages;
    property Others : TStringList read FOthers;
    property native : boolean read FNative write FNative;
    property source : String read FSource write FSource;
    property map : String read Fmap write FMap;
    property profile : String read FProfile write FProfile;
    property dest : String read FDest write FDest;
    Property txServer : String read FTxServer write FTxServer;

    property OnOutput : TOutputLineEvent read FOnOutput write FOnOutput;
  end;

implementation

{ TFHIRValidationWrapper }

constructor TFHIRValidationWrapper.Create(Cache: TFHIRPackageManager);
var
  ts : TStringList;
begin
  inherited create;
  FCache := cache;
  FPackages := TStringList.create;
  FOthers := TStringList.create;

end;

destructor TFHIRValidationWrapper.Destroy;
begin
  FPackages.Free;
  FOthers.Free;
  FCache.free;
  inherited;
end;

  // OSX: see http://www.fmxexpress.com/read-and-interact-with-a-command-line-pipe-in-delphi-xe7-firemonkey-on-mac-osx/ ?
procedure TFHIRValidationWrapper.validate;
begin
  executeCommand(validateCmd,
    procedure(const Line: PAnsiChar)
    begin
      FOnOutput(self, line);
    end);
end;

{$IFDEF WINDOWS}
procedure TFHIRValidationWrapper.executeCommand(cmd : String; CallBack: TArg<PAnsiChar>);
const
  CReadBuffer = 2400;
var
  saSecurity: TSecurityAttributes;
  hRead: THandle;
  hWrite: THandle;
  suiStartup: TStartupInfo;
  piProcess: TProcessInformation;
  pBuffer: array [0 .. CReadBuffer] of AnsiChar;
  dBuffer: array [0 .. CReadBuffer] of AnsiChar;
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
      if CreateProcess(nil, PChar(cmd), @saSecurity, @saSecurity, true, NORMAL_PRIORITY_CLASS, nil, nil, suiStartup,
        piProcess) then
        try
          repeat
            dRunning := WaitForSingleObject(piProcess.hProcess, 100);
            PeekNamedPipe(hRead, nil, 0, nil, @dAvailable, nil);
            if (dAvailable > 0) then
              repeat
                dRead := 0;
                ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
                pBuffer[dRead] := #0;
                OemToCharA(pBuffer, dBuffer);
                CallBack(dBuffer);
              until (dRead < CReadBuffer);
          until (dRunning <> WAIT_TIMEOUT);
        finally
          CloseHandle(piProcess.hProcess);
          CloseHandle(piProcess.hThread);
        end;
    finally
      CloseHandle(hRead);
      CloseHandle(hWrite);
    end;
end;
{$ELSE}
procedure TFHIRValidationWrapper.executeCommand(cmd : String; CallBack: TArg<PAnsiChar>);
begin
  raise EFHIRTodo.create('TFHIRValidationWrapper.executeCommand');
end;
{$ENDIF}


function TFHIRValidationWrapper.link: TFHIRValidationWrapper;
begin
  result := TFHIRValidationWrapper(inherited link);
end;

procedure TFHIRValidationWrapper.transform;
begin
  executeCommand(validateCmd,
    procedure(const Line: PAnsiChar)
    begin
      FOnOutput(self, line);
    end);
end;

function TFHIRValidationWrapper.validateCmd: string;
var
  i : integer;
  s : String;
begin
  if FJarPath = '' then
    exit('Error: validator not installed');

  result := 'java -jar "'+FJarPath+'"';
  if FSource <> '' then
    result := result+' '+source;
  result := result + ' -defn hl7.fhir.core-'+FVersion;

  if FProfile <> '' then
    result := result+' -profile '+FProfile;

  if FNative then
    result := result +' -native';

  if FTxServer <> '' then
    result := result + ' -tx '+FtxServer;

  for s in FPackages do
    result := result +' -ig '+s;

  for s in FOthers do
    result := result +' -ig '+s;

  if FDest <> '' then
    result := result+' -output '+FDest;
end;

function TFHIRValidationWrapper.transformCmd: string;
var
  i : integer;
  s : String;
begin
  if FJarPath = '' then
    exit('Error: validator not installed');
  if FMap = '' then
    exit('Error: map not specified');

  result := 'java -jar "'+FJarPath+'"';
  if FSource <> '' then
    result := result+' '+source;

  if FMap <> '' then
    result := result+' -transform '+FMap+' -defn hl7.fhir.core-'+FVersion;

  if FTxServer <> '' then
    result := result + ' -tx '+FtxServer;

  for s in FPackages do
    result := result +' -ig '+s;

  for s in FOthers do
    result := result +' -ig '+s;

  if FDest <> '' then
    result := result+' -output '+FDest;
end;

end.
