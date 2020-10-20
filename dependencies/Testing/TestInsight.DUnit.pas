{******************************************************************************}
{                                                                              }
{           TestInsight                                                        }
{                                                                              }
{           Copyright (c) 2014-2015 Stefan Glienke - All rights reserved       }
{                                                                              }
{           http://www.dsharp.org                                              }
{                                                                              }
{******************************************************************************}

unit TestInsight.DUnit;

interface

uses
  Generics.Collections,
  TestFramework,
  TestInsight.Client;

type
  TTestInsightListener = class(TInterfacedObject,
    IStatusListener, ITestListener, ITestListenerX)
  private
    fClient: ITestInsightClient;
    fSelectTest: Boolean;
    fSelectedTests: TArray<string>;
    fTestCount: Cardinal;
    fLastError: ITest;
    procedure Status(test: ITest; const msg: string);

    procedure TestingStarts;
    procedure StartTest(test: ITest);

    procedure AddSuccess(test: ITest);
    procedure AddError(error: TTestFailure);
    procedure AddFailure(failure: TTestFailure);
    procedure AddSkipped(test: ITest);

    procedure EndTest(test: ITest);
    procedure TestingEnds(testResult: TTestResult);

    function ShouldRunTest(test: ITest): Boolean;

    procedure StartSuite(suite: ITest);
    procedure EndSuite(suite: ITest);
    function PrepareTestFailure(failure: TTestFailure): TTestInsightResult;
  public
    constructor Create(const baseUrl: string; testCount: Cardinal);
  end;

procedure RunRegisteredTests(const baseUrl: string = DefaultUrl);

implementation

uses
  DUnitConsts,
  StrUtils,
  SysUtils;

type
  TAbstractTestAccess = class(TAbstractTest);

function GetFullQualifiedName(const test: ITest): string;
begin
  Result := (test as TObject).ClassName + '.' + test.Name;
end;

function GetMethodName(const test: ITest): string;
begin
  if test is TAbstractTest then
    Result := TAbstractTestAccess(test as TAbstractTest).FTestName
  else
    Result := SplitString(test.Name, '(')[0];
end;

function GetUnitName(const test: ITest): string;
begin
  Result := (test as TObject).UnitName;
end;

procedure RunRegisteredTests(const baseUrl: string);
var
  suite: ITestSuite;
  result: TTestResult;
  listener: ITestListener;
begin
  suite := RegisteredTests;
  if not Assigned(suite) then Exit;
  result := TTestResult.Create;
  result.FailsIfNoChecksExecuted := True;
  result.FailsIfMemoryLeaked := True;
  listener := TTestInsightListener.Create(baseUrl, suite.CountEnabledTestCases);
  result.AddListener(listener);
  try
    suite.Run(result);
  finally
    result.Free;
  end;
end;

{ TTestInsightTestListener }

constructor TTestInsightListener.Create(const baseUrl: string; testCount: Cardinal);
begin
  inherited Create;
  fClient := TTestInsightRestClient.Create(baseUrl);
  fSelectedTests := fClient.GetTests;
{$IFDEF ANDROID}
  if fClient.HasError then
  begin
    fClient := TTestInsightLogcatClient.Create;
    fSelectedTests := fClient.GetTests;
  end;
{$ENDIF}
  fSelectTest := Length(fSelectedTests) > 0;
  fTestCount := testCount;
end;

procedure TTestInsightListener.AddError(error: TTestFailure);
var
  testResult: TTestInsightResult;
begin
  testResult := PrepareTestFailure(error);
  testResult.ResultType := TResultType.Error;
  testResult.ExceptionMessage := Format('%s with message ''%s''', [
    error.ThrownExceptionName, error.ThrownExceptionMessage]);
  fClient.PostResult(testResult);
  fLastError := error.FailedTest;
end;

procedure TTestInsightListener.AddFailure(failure: TTestFailure);
var
  testResult: TTestInsightResult;
begin
  testResult := PrepareTestFailure(failure);
  if failure.ThrownExceptionMessage = sNoChecksExecuted then
    testResult.ResultType := TResultType.Warning
  else
    testResult.ResultType := TResultType.Failed;
  fClient.PostResult(testResult);
end;

procedure TTestInsightListener.AddSkipped(test: ITest);
var
  testResult: TTestInsightResult;
begin
  testResult := TTestInsightResult.Create(
    TResultType.Skipped, GetFullQualifiedName(test));
  testResult.UnitName := GetUnitName(test);
  testResult.ClassName := (test as TObject).ClassName;
  testResult.MethodName := GetMethodName(test);
  fClient.PostResult(testResult);
end;

procedure TTestInsightListener.AddSuccess(test: ITest);
var
  testResult: TTestInsightResult;
begin
  if IsTestMethod(test) and (fLastError <> test) then
  begin
    testResult := TTestInsightResult.Create(
      TResultType.Passed, GetFullQualifiedName(test));
    testResult.Duration := test.ElapsedTestTime;
    testResult.UnitName := GetUnitName(test);
    testResult.ClassName := (test as TObject).ClassName;
    testResult.MethodName := GetMethodName(test);
    testResult.Status := test.Status;
    fClient.PostResult(testResult);
  end;
end;

procedure TTestInsightListener.EndSuite(suite: ITest);
begin
end;

procedure TTestInsightListener.EndTest(test: ITest);
begin
end;

function TTestInsightListener.PrepareTestFailure(failure: TTestFailure): TTestInsightResult;
var
  elapsedTime: Cardinal;
begin
  elapsedTime := failure.FailedTest.ElapsedTestTime; // get this before looking up anything

  Result := TTestInsightResult.Create(
    TResultType.Failed, GetFullQualifiedName(failure.FailedTest));
  Result.Duration := elapsedTime;
  Result.ExceptionMessage := failure.ThrownExceptionMessage;
  Result.UnitName := GetUnitName(failure.FailedTest);
  Result.ClassName := (failure.FailedTest as TObject).ClassName;
  Result.MethodName := GetMethodName(failure.FailedTest);
  Result.Status := failure.FailedTest.Status;
  GetExtendedDetails(failure.ThrownExceptionAddress, Result);
end;

function TTestInsightListener.ShouldRunTest(test: ITest): Boolean;

  function Matches(const test: ITest; const testMethod: string): Boolean;
  var
    testName: string;
    cls: TClass;
  begin
    cls := (test as TObject).ClassParent;
    while (cls <> TTestCase) and (cls <> TObject) do
    begin
      testName := cls.UnitName + '.' + cls.ClassName + '.' + test.Name;
      if SameText(testMethod, testName)
        or StartsText(testMethod + '(', testName) then
        Exit(True);
      cls := cls.ClassParent;
    end;
    Result := False;
  end;

var
  testMethod, testName: string;
begin
  if IsTestMethod(test) and (fSelectTest or not fClient.Options.ExecuteTests) then
  begin
    testName := GetUnitName(test) + '.' + GetFullQualifiedName(test);
    for testMethod in fSelectedTests do
      if SameText(testMethod, testName)
        or StartsText(testMethod + '(', testName) then
        Exit(True);

    for testMethod in fSelectedTests do
      if Matches(test, testMethod) then
        Exit(True);

    Result := False;
    AddSkipped(test);
  end
  else
    Result := test.Enabled;
end;

procedure TTestInsightListener.StartSuite(suite: ITest);
begin
end;

procedure TTestInsightListener.StartTest(test: ITest);
var
  testResult: TTestInsightResult;
begin
  if IsTestMethod(test) and fClient.Options.ShowProgress then
  begin
    testResult := TTestInsightResult.Create(
      TResultType.Running, GetFullQualifiedName(test));
    testResult.UnitName := GetUnitName(test);
    testResult.ClassName := (test as TObject).ClassName;
    testResult.MethodName := GetMethodName(test);
    fClient.PostResult(testResult);
  end;
end;

procedure TTestInsightListener.Status(test: ITest; const msg: string);
begin
end;

procedure TTestInsightListener.TestingEnds(testResult: TTestResult);
begin
  fClient.FinishedTesting;
end;

procedure TTestInsightListener.TestingStarts;
begin
  fClient.StartedTesting(fTestCount);
end;

end.
