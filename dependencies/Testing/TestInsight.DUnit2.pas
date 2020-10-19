{******************************************************************************}
{                                                                              }
{           TestInsight                                                        }
{                                                                              }
{           Copyright (c) 2014-2015 Stefan Glienke - All rights reserved       }
{                                                                              }
{           http://www.dsharp.org                                              }
{                                                                              }
{******************************************************************************}

unit TestInsight.DUnit2;

interface

uses
  TestFrameworkProxyIfaces,
  TestInsight.Client;

type
  TTestInsightListener = class(TInterfacedObject,
    IStatusListener, ITestListener, ITestListenerX)
  private
    fClient: ITestInsightClient;
    fSelectTest: Boolean;
    fSelectedTests: TArray<string>;
    fTestCount: Cardinal;
    procedure Status(const test: ITestProxy; msg: string);

    procedure TestingStarts;
    procedure StartTest(test: ITestProxy);

    procedure AddSuccess(test: ITestProxy);
    procedure AddError(error: TTestFailure);
    procedure AddFailure(failure: TTestFailure);
    procedure AddWarning(warning: TTestFailure);
    procedure AddSkipped(test: ITestProxy);

    procedure EndTest(test: ITestProxy);
    procedure TestingEnds(testResult: TTestResult);

    function ShouldRunTest(const test: ITestProxy): Boolean;

    procedure StartSuite(suite: ITestProxy);
    procedure EndSuite(suite: ITestProxy);

    function PrepareTestFailure(failure: TTestFailure): TTestInsightResult;
  public
    constructor Create(const baseUrl: string; testCount: Cardinal);
    destructor Destroy; override;
  end;

procedure RunRegisteredTests(const baseUrl: string = DefaultUrl);

implementation

uses
  TestFrameworkIfaces,
  TestFrameworkProxy,
  StrUtils,
  SysUtils;

type
  // hack to access the underlying ITest to get the data needed
  TTestProxyAccess = class(TInterfacedObject, IInterface)
  private
    FITest: ITest;
  end;

function GetTest(const testProxy: ITestProxy): ITest;
var
  proxy: TTestProxyAccess;
begin
  proxy := TTestProxyAccess(testProxy as TObject);
  Result := proxy.FITest;
end;

function GetClassName(const test: ITestProxy): string;
begin
  Result := (GetTest(test).ParentTestCase as TObject).ClassName;
end;

function GetFullQualifiedName(const test: ITestProxy): string;
begin
  Result := GetTest(test).ParentTestCase.DisplayedName + '.' + test.GetName;
end;

function GetUnitName(const test: ITestProxy): string;
begin
  Result := (GetTest(test).ParentTestCase as TObject).UnitName;
end;

procedure RunRegisteredTests(const baseUrl: string);
var
  suite: ITestSuiteProxy;
  result: TTestResult;
  listener: ITestListener;
begin
  suite := RegisteredTests;
  if not Assigned(suite) then Exit;
  listener := TTestInsightListener.Create(baseUrl, suite.CountEnabledTestCases);
  try
    result := RunTest(Suite, [listener]);
  finally
    result.ReleaseListeners;
  end;
end;

{ TTestInsightTestListener }

constructor TTestInsightListener.Create(const baseUrl: string; testCount: Cardinal);
begin
  inherited Create;
  fClient := TTestInsightRestClient.Create(baseUrl);
  fSelectedTests := fClient.GetTests;
  fSelectTest := Length(fSelectedTests) > 0;
  fTestCount := testCount;
end;

destructor TTestInsightListener.Destroy;
begin
  fClient.FinishedTesting;
  inherited;
end;

procedure TTestInsightListener.AddError(error: TTestFailure);
var
  testResult: TTestInsightResult;
begin
  testResult := PrepareTestFailure(error);
  testResult.ExceptionMessage := Format('%s with message ''%s''', [
    error.ThrownExceptionName, error.ThrownExceptionMessage]);
  testResult.ResultType := TResultType.Error;
  fClient.PostResult(testResult);
end;

procedure TTestInsightListener.AddFailure(failure: TTestFailure);
var
  testResult: TTestInsightResult;
begin
  testResult := PrepareTestFailure(failure);
  testResult.ResultType := TResultType.Failed;
  fClient.PostResult(testResult);
end;

procedure TTestInsightListener.AddSkipped(test: ITestProxy);
var
  testResult: TTestInsightResult;
begin
  testResult := TTestInsightResult.Create(
    TResultType.Skipped, GetFullQualifiedName(test));
  testResult.UnitName := GetUnitName(test);
  testResult.ClassName := GetClassName(test);
  testResult.MethodName := test.Name;
  fClient.PostResult(testResult);
end;

procedure TTestInsightListener.AddSuccess(test: ITestProxy);
var
  testResult: TTestInsightResult;
begin
  if IsTestMethod(test) then
  begin
    testResult := TTestInsightResult.Create(
      TResultType.Passed, GetFullQualifiedName(test));
    testResult.Duration := test.ElapsedTestTime;
    testResult.UnitName := GetUnitName(test);
    testResult.ClassName := GetClassName(test);
    testResult.MethodName := SplitString(test.Name, '(')[0];
    fClient.PostResult(testResult);
  end;
end;

procedure TTestInsightListener.AddWarning(warning: TTestFailure);
var
  testResult: TTestInsightResult;
begin
  testResult := PrepareTestFailure(warning);
  testResult.ResultType := TResultType.Warning;
  fClient.PostResult(testResult);
end;

procedure TTestInsightListener.EndSuite(suite: ITestProxy);
begin
end;

procedure TTestInsightListener.EndTest(test: ITestProxy);
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
  Result.ClassName := GetClassName(failure.FailedTest);
  Result.MethodName := failure.FailedTest.Name;
  GetExtendedDetails(Pointer(failure.ThrownExceptionAddress), Result);
end;

function TTestInsightListener.ShouldRunTest(const test: ITestProxy): Boolean;
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
    Result := False;
    AddSkipped(test);
  end
  else
    Result := test.Enabled;
end;

procedure TTestInsightListener.StartSuite(suite: ITestProxy);
begin
end;

procedure TTestInsightListener.StartTest(test: ITestProxy);
var
  testResult: TTestInsightResult;
begin
  if IsTestMethod(test) and fClient.Options.ShowProgress then
  begin
    testResult := TTestInsightResult.Create(
      TResultType.Running, GetFullQualifiedName(test));
    testResult.UnitName := GetUnitName(test);
    testResult.ClassName := GetClassName(test);
    testResult.MethodName := test.Name;
    fClient.PostResult(testResult);
  end;
end;

procedure TTestInsightListener.Status(const test: ITestProxy; msg: string);
begin
end;

procedure TTestInsightListener.TestingEnds(testResult: TTestResult);
begin
  // DUnit2 does not call this reliable so this will be done in the dtor
//  fClient.FinishedTesting;
end;

procedure TTestInsightListener.TestingStarts;
begin
  fClient.StartedTesting(fTestCount);
end;

end.
