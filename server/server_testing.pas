unit server_testing;

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
  TestInsight.DUnit, DUnitTestRunner, GUITestRunner,
  {$ENDIF}
  {$IFDEF FPC}
  idetester_form, idetester_runtime, idetester_console,
  {$ENDIF}
  fsl_base, fsl_utilities, fsl_testing, fsl_logging,
  server_config,
  test_registry;

function isTestInsight : boolean;
procedure runTestInsight(ini : TFHIRServerConfigFile);
procedure runTests(ini : TFHIRServerConfigFile);

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
  Logging.Log('Run Tests (TestInsight)');
  test_registry.registerTests;
  {$IFDEF FPC}
  raise Exception.create('This is not supported in FPC');
  {$ELSE}
  FreeConsole;
  TestInsight.DUnit.RunRegisteredTests;
  {$ENDIF}
end;

procedure RunTestGui(ini : TFHIRServerConfigFile);
begin
  Logging.Log('Run Tests (GUI)');
  {$IFDEF WINDOWS}
  FreeConsole;
  {$ENDIF}
  {$IFDEF FPC}
  Application.Initialize;
  Application.CreateForm(TIdeTesterForm, IdeTesterForm);
//  TestRunner.FileName := TestSettings.serverTestFile(['tests.ini']);
  Application.Run;
  {$ELSE}
  TGUITestRunner.runRegisteredTests;
  {$ENDIF}
end;

procedure RunTestConsole(ini : TFHIRServerConfigFile);
{$IFDEF FPC}
var
  app : TIdeTesterConsoleRunner;
begin
  Logging.Log('Run Tests (Console)');
  app := TIdeTesterConsoleRunner.Create(nil);
  app.Initialize;
  app.Title := 'FPCUnit Console test runner';
  app.showProgress := true;
  app.Run;
  app.Free;
end;
{$ELSE}
begin
  DUnitTestRunner.RunRegisteredTests;
  if not hasCommandLineParam('-ci') then
  begin
    System.Write('Done.. press <Enter> key to quit.');
    System.Readln;
  end;
end;
{$ENDIF}

procedure runTests(ini : TFHIRServerConfigFile);
begin
  test_registry.registerTests;
  if hasCommandLineParam('gui') then
    RunTestGui(ini)
  {$IFDEF FPC}
  else if IsRunningIDETests then
  begin
    ShowObjectLeaks := false;
    RunIDETests;
  end
  {$ENDIF}
  else
    RunTestConsole(ini);
end;

end.
