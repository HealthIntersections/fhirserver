unit fsl_testing;

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

Uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  SysUtils, Classes, {$IFNDEF FPC}Soap.EncdDecd, System.NetEncoding, {$ENDIF} SyncObjs, IniFiles,
  {$IFDEF FPC} FPCUnit, TestRegistry, RegExpr, {$ELSE} TestFramework, {$ENDIF}
  IdGlobalProtocols,
  CommonTestBase,
  fsl_base, fsl_utilities;

// *** General Testing Infrastructure ******************************************

type
  {$IFNDEF FPC}
  TRunMethod = TTestMethod;
  {$ENDIF}
  TFslTestThread = class;

  {
    TFslTestCase - works with DUnit or FPCUnit
  }
  {$M+}
  TFslTestCase = class (TTestCase)
  protected
    procedure Status(const Msg: string);
    procedure assertNotTested;
    procedure assertPass;
    procedure assertFail(message : String);
    procedure assertTrue(test : boolean; message : String); overload;
    procedure assertTrue(test : boolean); overload;
    procedure assertFalse(test : boolean; message : String); overload;
    procedure assertFalse(test : boolean); overload;
    procedure assertEqual(left, right : String; message : String); overload;
    procedure assertEqual(left, right : String); overload;
    procedure assertEqual(left, right : integer; message : String); overload;
    procedure assertEqual(left, right : integer); overload;
    procedure assertWillRaise(AMethod: TRunMethod; AExceptionClass: ExceptClass; AExceptionMessage : String);
    procedure thread(proc : TRunMethod);
  public
  end;

  TFslTestSuiteCase = class (TFslTestCase)
  protected
    FName : String;
    FFolder : String;
    {$IFDEF FPC}
    function GetTestName: string; override;
    {$ENDIF}
    function filename : String;
  public
    constructor Create(name : String); {$IFNDEF FPC} reintroduce; {$ENDIF}
    constructor CreateInFolder(name, folder : String);

    {$IFNDEF FPC}
    function GetName: string; override;
    {$ENDIF}
    procedure TestCase(name : String); virtual;
  published
    {$IFDEF FPC}
    procedure Test;
    {$ELSE}
    procedure Run;
    {$ENDIF}
  end;
  TFslTestSuiteCaseClass = class of TFslTestSuiteCase;

  TFslTestSuite = class (TTestSuite)
  private
  public
    constructor Create; overload; {$IFDEF FPC} override; {$ELSE} virtual; {$ENDIF}
    constructor Create(folder, filter : String; count : integer; clss : TFslTestSuiteCaseClass); overload;
  end;


  TFslTestThread = class (TThread)
  private
    FProc : TRunMethod;
  protected
    procedure Execute; override;
  public
    constructor Create(proc : TRunMethod);
  end;

  TFslTestSettings = class (TFslObject)
  private
    Fini : TIniFile;
    FFilename : String;
    FServerTestsRoot : String;
    FFHIRTestsRoot : String;
    function testFile(root : String; parts : array of String) : String;
  public
    constructor Create(filename : String);
    destructor Destroy; override;
    property filename : String read FFilename;

    function serverTestFile(parts : array of String) : String;
    function fhirTestFile(parts : array of String) : String;

    function section(name : String): TFslStringMap;
    function hasSection(name : String): boolean;

    function SMTPUsername : String;
    function SMTPPassword : String;
    function SMTPDestination : String;

    function SSLCertFile : String;
    function SSLKeyFile : String;
    function SSLPassword : String;
    function SSLCAFile : String;
  end;

var
  TestSettings : TFslTestSettings;

{$IFDEF FPC}
procedure RegisterTest(ASuitePath: String; ATestClass: TTestCaseClass); overload;
procedure RegisterTest(ASuitePath: String; ATest: TTest); overload;
{$ELSE}
procedure RegisterTest(SuitePath: string; test: ITest);
{$ENDIF}

implementation

{$IFDEF FPC}
procedure RegisterTest(ASuitePath: String; ATestClass: TTestCaseClass); overload;
begin
  TestRegistry.RegisterTest(ASuitePath, ATestClass);
end;

procedure RegisterTest(ASuitePath: String; ATest: TTest); overload;
begin
  TestRegistry.RegisterTest(ASuitePath, ATest);
end;
{$ELSE}
procedure RegisterTest(SuitePath: string; test: ITest);
begin
  TestFramework.RegisterTest(SuitePath, test);
end;
{$ENDIF}

{ TFslTestCase }

procedure TFslTestCase.assertPass;
begin
  {$IFDEF FPC}
  // nothing
  {$ELSE}
  check(true);
  {$ENDIF}
end;

procedure TFslTestCase.assertFail(message: String);
begin
  {$IFDEF FPC}
  TAssert.Fail(message);
  {$ELSE}
  Fail(message);
  {$ENDIF}
end;

procedure TFslTestCase.assertTrue(test: boolean; message: String);
begin
  {$IFDEF FPC}
  TAssert.AssertTrue(message, test);
  {$ELSE}
  check(test, message);
  {$ENDIF}
end;

procedure TFslTestCase.assertTrue(test: boolean);
begin
  {$IFDEF FPC}
  TAssert.AssertTrue(test);
  {$ELSE}
  check(test);
  {$ENDIF}
end;

procedure TFslTestCase.assertFalse(test: boolean; message: String);
begin
  {$IFDEF FPC}
  TAssert.AssertFalse(message, test);
  {$ELSE}
  checkFalse(test, message);
  {$ENDIF}
end;

procedure TFslTestCase.assertFalse(test: boolean);
begin
  {$IFDEF FPC}
  TAssert.AssertFalse(test);
  {$ELSE}
  checkFalse(test);
  {$ENDIF}
end;

procedure TFslTestCase.assertNotTested;
begin
  {$IFDEF FPC}
  TAssert.Fail('Not Tested');
  {$ELSE}
  Fail('Not Tested');
  {$ENDIF}
end;

procedure TFslTestCase.assertEqual(left, right, message: String);
begin
  {$IFDEF FPC}
  TAssert.AssertEquals(message, left, right);
  {$ELSE}
  checkEquals(left, right, message);
  {$ENDIF}
end;

procedure TFslTestCase.assertEqual(left, right: String);
begin
  {$IFDEF FPC}
  TAssert.AssertEquals(left, right);
  {$ELSE}
  checkEquals(left, right);
  {$ENDIF}
end;

procedure TFslTestCase.assertEqual(left, right : integer; message: String);
begin
  {$IFDEF FPC}
  TAssert.AssertEquals(message, left, right);
  {$ELSE}
  checkEquals(left, right, message);
  {$ENDIF}
end;

procedure TFslTestCase.assertEqual(left, right: integer);
begin
  {$IFDEF FPC}
  TAssert.AssertEquals(left, right);
  {$ELSE}
  checkEquals(left, right);
  {$ENDIF}
end;

procedure TFslTestCase.assertWillRaise(AMethod: TRunMethod; AExceptionClass: ExceptClass; AExceptionMessage : String);
begin
  {$IFDEF FPC}
  TAssert.AssertException(AExceptionMessage, AExceptionClass, AMethod);
  {$ELSE}
  try
    AMethod;
    if (AExceptionMessage = '') then
      fail('Expected '+AExceptionClass.ClassName+', but it did not occur')
    else
      fail('Expected '+AExceptionClass.ClassName+' with message "'+AExceptionMessage+'", but it did not occur')
  except
    on e : Exception do
    begin
      assertTrue(e.ClassType = AExceptionClass, 'Method raised an exception, but not of the right type ('+e.ClassName+' vs '+AExceptionClass.ClassName);
      if AExceptionMessage <> '' then
        assertEqual(AExceptionMessage, e.Message);
    end;
  end;
  {$ENDIF}
end;

procedure TFslTestCase.Status(const Msg: string);
begin
 // nothing, for now
end;

procedure TFslTestCase.thread(proc: TRunMethod);
begin
  TFSLTestThread.Create(proc);
end;

{ TFslTestSuiteCase }

constructor TFslTestSuiteCase.Create(name : String);
begin
  {$IFDEF FPC}
  inherited CreateWith('Test', name);
  {$ELSE}
  inherited Create('Run');
  {$ENDIF}
  FName := name;
end;

constructor TFslTestSuiteCase.CreateInFolder(name, folder : String);
begin
  Create(name);
  FFolder := folder;
end;

function TFslTestSuiteCase.filename : String;
begin
  result := IncludeTrailingPathDelimiter(FFolder) + FName;
end;

{$IFDEF FPC}
function TFslTestSuiteCase.GetTestName: string;
begin
  Result := FName;
end;

procedure TFslTestSuiteCase.Test;
begin
  TestCase(FName);
end;

{$ELSE}

function TFslTestSuiteCase.GetName: string;
begin
  Result := FName;
end;

procedure TFslTestSuiteCase.Run;
begin
  TestCase(FName);
end;

{$ENDIF}

procedure TFslTestSuiteCase.TestCase(name: String);
begin
  // nothing - override this
end;

{ TFslTestSuite }

constructor TFslTestSuite.Create;
begin
  inherited Create;
end;

constructor TFslTestSuite.Create(folder, filter : String; count : integer; clss : TFslTestSuiteCaseClass);
var
  sr : TSearchRec;
  s : String;
  c : integer;
begin
  inherited Create;

  c := 0;
  if FindFirst(IncludeTrailingPathDelimiter(folder)+'*.*', faAnyFile, SR) = 0 then
  repeat
    s := sr.Name;
    if ((filter = '') or s.endsWith(filter)) and ((count = 0) or (c < count)) then
    begin
      AddTest(clss.CreateInFolder(s, folder));
      inc(c);
    end;
  until FindNext(SR) <> 0;
end;



{ TFslTestThread }

constructor TFslTestThread.Create(proc: TRunMethod);
begin
  FProc := proc;
  FreeOnTerminate := true;
  inherited Create(false);
end;

procedure TFslTestThread.execute;
begin
  Fproc;
end;

{ TFslTestSettings }

const
  psc = {$IFDEF WINDOWS} '\' {$ELSE} '/' {$ENDIF};

constructor TFslTestSettings.Create(filename: String);
begin
  inherited create;
  FFilename := filename;
  FIni := TIniFile.create(filename);
  FServerTestsRoot := FIni.ReadString('locations', 'fhirserver', '');
  FFHIRTestsRoot := FIni.ReadString('locations', 'fhir-test-cases', '');
  MDTestRoot := FIni.ReadString('locations', 'markdown', '');
end;

destructor TFslTestSettings.Destroy;
begin
  FIni.Free;
  inherited;
end;

function TFslTestSettings.fhirTestFile(parts: array of String): String;
begin
  result := testFile(FFHIRTestsRoot, parts);
end;

function TFslTestSettings.serverTestFile(parts: array of String): String;
begin
  result := testFile(FServerTestsRoot, parts);
end;

function TFslTestSettings.SMTPDestination: String;
begin
  result := Fini.ReadString('email', 'destination', '');
end;

function TFslTestSettings.SMTPPassword: String;
begin
  result := Fini.ReadString('email', 'password', '');
end;

function TFslTestSettings.SMTPUsername: String;
begin
  result := Fini.ReadString('email', 'sender', '');
end;

function TFslTestSettings.SSLCAFile: String;
begin
  result := Fini.ReadString('ssl', 'cacert', '');
end;

function TFslTestSettings.SSLCertFile: String;
begin
  result := Fini.ReadString('ssl', 'cert', '');
end;

function TFslTestSettings.SSLKeyFile: String;
begin
  result := Fini.ReadString('ssl', 'key', '');
end;

function TFslTestSettings.SSLPassword: String;
begin
  result := Fini.ReadString('ssl', 'password', '');
end;

function TFslTestSettings.testFile(root: String; parts: array of String): String;
var
  part : String;
  s : String;
begin
  result := root;
  for part in parts do
  begin
    s := part.Replace('/', psc).Replace('\', psc);
    if result = '' then
      result := s
    else if not result.EndsWith(psc) and not s.startsWith(psc) then
      result := result + psc + s
    else if not result.EndsWith(psc) or not s.startsWith(psc) then
      result := result + s
    else
      result := result + s.substring(1);
  end;
end;

function TFslTestSettings.section(name: String): TFslStringMap;
var
  list : TStringList;
  s : String;
begin
  result := TFslStringMap.Create;
  try
    list := TStringList.Create;
    try
      FIni.ReadSection(name, list);
      for s in list do
        result.Items[s] := FIni.ReadString(name, s, '');
    finally
      list.Free;
    end;
    result.link;
  finally
    result.Free;
  end;
end;

function TFslTestSettings.hasSection(name: String): boolean;
begin
  result := FIni.SectionExists(name);
end;

initialization
  TestSettings := TFslTestSettings.Create('fhir-tests.ini');
finalization
  TestSettings.Free;
end.



