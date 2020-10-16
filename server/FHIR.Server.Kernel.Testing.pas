unit FHIR.Server.Kernel.Testing;

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
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  SysUtils, Classes, Forms,
  {$IFNDEF FPC}
  TestInsight.DUnitX, DUnitX.Loggers.Console, DUnitX.Loggers.GUI.VCL, DUnitX.Loggers.Xml.NUnit, DUnitX.TestFramework,
  {$ENDIF}
  {$IFDEF FPC}
  GuiTestRunner, FHIR.Support.Fpc.ConsoleTester,
  {$ENDIF}
  FHIR.Support.Utilities,
  FHIR.Server.Ini,
  FHIR.Server.TestRegistry;

function isTestInsight : boolean;
procedure runTestInsight(ini : TFHIRServerIniFile);
procedure runTests(ini : TFHIRServerIniFile);

implementation

function isTestInsight : boolean;
begin
  {$IFDEF FPC}
  result := false;
  {$ELSE}
  result := FileExists(partnerFile('TestInsightSettings.ini'));
  {$ENDIF}
end;

procedure runTestInsight;
begin
  FHIR.Server.TestRegistry.registerTests;
  {$IFDEF FPC}
  raise Exception.create('This is not supported in FPC');
  {$ELSE}
  TestInsight.DUnitX.RunRegisteredTests;
  {$ENDIF}
end;

procedure RunTestGui(ini : TFHIRServerIniFile);
begin
  {$IFDEF WINDOWS}
  FreeConsole;
  {$ENDIF}
  {$IFDEF FPC}
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
  {$ELSE}
  Application.Initialize;
  Application.Title := 'FHIRServer Tests';
  Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
  Application.Run;
  {$ENDIF}
end;

procedure RunTestConsole(ini : TFHIRServerIniFile);
{$IFDEF FPC}
var
  app : TFHIRTestRunner;
begin
  app := TFHIRTestRunner.Create(nil);
  app.Initialize;
  app.Title := 'FPCUnit Console test runner';
  app.showProgress := true;
  app.Run;
  app.Free;
end;
{$ELSE}
var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
  s : String;
begin
  runner := TDUnitX.CreateRunner;
  runner.UseRTTI := True;
  logger := TDUnitXConsoleLogger.Create(false);
  runner.AddLogger(logger);
  nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
  runner.AddLogger(nunitLogger);
  runner.FailsOnNoAsserts := True;
  results := runner.Execute;
  if not results.AllPassed then
    System.ExitCode := EXIT_ERRORS;
  if not hasCommandLineParam('-ci') then
  begin
    System.Write('Done.. press <Enter> key to quit.');
    System.Readln;
  end;
end;
{$ENDIF}

procedure runTests(ini : TFHIRServerIniFile);
begin
  FHIR.Server.TestRegistry.registerTests;
  if hasCommandLineParam('gui') then
    RunTestGui(ini)
  else
    RunTestConsole(ini);
end;

end.
