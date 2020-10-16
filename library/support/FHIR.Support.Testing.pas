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
  Windows, SysUtils, Classes, {$IFNDEF FPC}Soap.EncdDecd, System.NetEncoding, {$ENDIF} SyncObjs,
  {$IFDEF FPC} FPCUnit, TestRegistry, RegExpr, {$ELSE} DUnitX.TestFramework, {$ENDIF}
  IdGlobalProtocols, IdSSLOpenSSLHeaders,
  FHIR.Support.Base, FHIR.Support.Utilities;

// *** General Testing Infrastructure ******************************************

var
  ServerTestsRoot : String;
  FHIRTestsRoot : String;

function serverTestFile(parts : array of String) : String;
function fhirTestFile(parts : array of String) : String;

type
  {$IFNDEF FPC}
  TRunMethod = TTestMethod;
  {$ENDIF}
  TFslTestThread = class;

  {
    TFslTestCase

    Base test case for all tests.

    For DUnitX, this doesn't do much directly, but it does define common assertions mechanism for FPCUnit and DUnitX.

    For FPC, it also makes it easy to register tests with alternative names

  }
  {$M+}
  TFslTestCase = class {$IFDEF FPC} (TTestCase) {$ENDIF}
  protected
    procedure assertPass;
    procedure assertFail(message : String);
    procedure assertTrue(test : boolean; message : String); overload;
    procedure assertTrue(test : boolean); overload;
    procedure assertFalse(test : boolean; message : String); overload;
    procedure assertFalse(test : boolean); overload;
    procedure assertEqual(left, right : String; message : String); overload;
    procedure assertEqual(left, right : String); overload;
    procedure assertWillRaise(AMethod: TRunMethod; AExceptionClass: ExceptClass; AExceptionMessage : String);
    procedure thread(proc : TRunMethod);
  public
    {$IFNDEF FPC}
    procedure setup; virtual;
    procedure tearDown; virtual;
    {$ENDIF}
  end;

  TFslTestSuite = class (TFslTestCase)
  protected
    {$IFDEF FPC}
    FName : String;
    function GetTestName: string; override;
    {$ENDIF}
  public
    {$IFDEF FPC}
    constructor Create(name : String);
    {$ENDIF}
    procedure TestCase(name : String); virtual;
  published
    {$IFDEF FPC}
    procedure Test;
    {$ENDIF}
  end;

  TFslTestThread = class (TThread)
  private
    FProc : TRunMethod;
  protected
    procedure Execute; override;
  public
    constructor Create(proc : TRunMethod);
  end;

implementation

const
  psc = {$IFDEF WINDOWS} '\' {$ELSE} '/' {$ENDIF};

function testFile(root : String; parts : array of String) : String;
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

function serverTestFile(parts : array of String) : String;
begin
  result := testFile(ServerTestsRoot, parts);
end;

function fhirTestFile(parts : array of String) : String;
begin
  result := testFile(FHIRTestsRoot, parts);
end;

{ TFslTestCase }

procedure TFslTestCase.assertPass;
begin
  {$IFDEF FPC}
  // nothing
  {$ELSE}
  Assert.Pass;
  {$ENDIF}
end;

procedure TFslTestCase.assertFail(message: String);
begin
  {$IFDEF FPC}
  TAssert.Fail(message);
  {$ELSE}
  Assert.Fail(message);
  {$ENDIF}
end;

procedure TFslTestCase.assertTrue(test: boolean; message: String);
begin
  {$IFDEF FPC}
  TAssert.AssertTrue(message, test);
  {$ELSE}
  Assert.IsTrue(test, message);
  {$ENDIF}
end;

procedure TFslTestCase.assertTrue(test: boolean);
begin
  {$IFDEF FPC}
  TAssert.AssertTrue(test);
  {$ELSE}
  Assert.IsTrue(test);
  {$ENDIF}
end;

procedure TFslTestCase.assertFalse(test: boolean; message: String);
begin
  {$IFDEF FPC}
  TAssert.AssertFalse(message, test);
  {$ELSE}
  Assert.IsFalse(test, message);
  {$ENDIF}
end;

procedure TFslTestCase.assertFalse(test: boolean);
begin
  {$IFDEF FPC}
  TAssert.AssertFalse(test);
  {$ELSE}
  Assert.IsFalse(test);
  {$ENDIF}
end;

procedure TFslTestCase.assertEqual(left, right, message: String);
begin
  {$IFDEF FPC}
  TAssert.AssertEquals(message, left, right);
  {$ELSE}
  Assert.AreEqual(left, right, message);
  {$ENDIF}
end;

procedure TFslTestCase.assertEqual(left, right: String);
begin
  {$IFDEF FPC}
  TAssert.AssertEquals(left, right);
  {$ELSE}
  Assert.AreEqual(left, right);
  {$ENDIF}
end;

procedure TFslTestCase.assertWillRaise(AMethod: TRunMethod; AExceptionClass: ExceptClass; AExceptionMessage : String);
begin
  {$IFDEF FPC}
  TAssert.AssertException(AExceptionMessage, AExceptionClass, AMethod);
  {$ELSE}
  Assert.WillRaise(AMethod, AExceptionClass, AExceptionMessage);
  {$ENDIF}
end;

{$IFNDEF FPC}
procedure TFslTestCase.setup;
begin

end;

procedure TFslTestCase.tearDown;
begin

end;
{$ENDIF}

procedure TFslTestCase.thread(proc: TRunMethod);
begin
  TFSLTestThread.Create(proc);
end;

{ TFslTestSuite }

{$IFDEF FPC}
constructor TFslTestSuite.Create(name : String);
begin
  inherited CreateWith('Test', name);
  FName := name;
end;

function TFslTestSuite.GetTestName: string;
begin
  Result := FName;
end;

procedure TFslTestSuite.Test;
begin
  TestCase(FName);
end;

{$ENDIF}

procedure TFslTestSuite.TestCase(name: String);
begin
  // nothing - override this
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


end.



