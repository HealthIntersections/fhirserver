unit FHIR.Support.Testing;

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
  IdGlobalProtocols, IdSSLOpenSSLHeaders,
  CommonTestBase,
  FHIR.Support.Base, FHIR.Support.Utilities;

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
    {$IFDEF FPC}
    function GetTestName: string; override;
    {$ENDIF}
  public
    constructor Create(name : String); {$IFNDEF FPC} reintroduce;
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

  TFslTestSuite = class (TTestSuite)
  private
  public
    constructor Create; {$IFDEF FPC} override; {$ELSE} virtual; {$ENDIF}
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
  end;

var
  TestSettings : TFslTestSettings;

implementation

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
        assertEqual(e.Message, AExceptionMessage);
    end;
  end;
  {$ENDIF}
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



