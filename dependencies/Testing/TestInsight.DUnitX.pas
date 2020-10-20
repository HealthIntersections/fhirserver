{******************************************************************************}
{                                                                              }
{           TestInsight                                                        }
{                                                                              }
{           Copyright (c) 2015 Stefan Glienke - All rights reserved            }
{                                                                              }
{           http://www.dsharp.org                                              }
{                                                                              }
{******************************************************************************}

unit TestInsight.DUnitX;

interface

uses
  DUnitX.TestFramework,
  TestInsight.Client;

type
  TTestInsightLogger = class(TInterfacedObject, ITestLogger)
  private
    fClient: ITestInsightClient;
    fSelectTest: Boolean;
    fSelectedTests: TArray<string>;
    procedure PostTestResult(resultType: TResultType; const test: ITestResult);
  public
    constructor Create(const client: ITestInsightClient);

    procedure OnBeginTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnEndSetupFixture(const threadId: TThreadID;
      const fixture: ITestFixtureInfo);
    procedure OnEndSetupTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnEndTearDownFixture(const threadId: TThreadID;
      const fixture: ITestFixtureInfo);
    procedure OnEndTeardownTest(const threadId: TThreadID;
      const Test: ITestInfo);
    procedure OnEndTest(const threadId: TThreadID; const Test: ITestResult);
    procedure OnEndTestFixture(const threadId: TThreadID;
      const results: IFixtureResult);
    procedure OnExecuteTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnLog(const logType: TLogLevel; const msg: string);
    procedure OnSetupFixture(const threadId: TThreadID;
      const fixture: ITestFixtureInfo);
    procedure OnSetupTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnStartTestFixture(const threadId: TThreadID;
      const fixture: ITestFixtureInfo);
    procedure OnTearDownFixture(const threadId: TThreadID;
      const fixture: ITestFixtureInfo);
    procedure OnTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnTestError(const threadId: TThreadID; const Error: ITestError);
    procedure OnTestFailure(const threadId: TThreadID;
      const Failure: ITestError);
    procedure OnTestIgnored(const threadId: TThreadID;
      const AIgnored: ITestResult);
    procedure OnTestMemoryLeak(const threadId: TThreadID;
      const Test: ITestResult);
    procedure OnTestSuccess(const threadId: TThreadID; const Test: ITestResult);
    procedure OnTestingEnds(const RunResults: IRunResults);
    procedure OnTestingStarts(const threadId: TThreadID;
      testCount, testActiveCount: Cardinal);

    property SelectedTests: TArray<string> read fSelectedTests;
  end;

procedure RunRegisteredTests(const baseUrl: string = DefaultUrl);

implementation

uses
  DUnitX.Extensibility,
  DUnitX.Filters,
  DUnitX.TestRunner,
  StrUtils,
  SysUtils;

type
  TSelectedFilter = class(TInterfacedObject, ITestFilter)
  private
    fClient: ITestInsightClient;
    fSelectedTests: TArray<string>;
    fSelectTest: Boolean;
    procedure AddSkipped(const test: ITest);
  public
    constructor Create(const client: ITestInsightClient);
    function Match(const test: ITest): Boolean;
    function IsEmpty: Boolean;
  end;

{ TSelectedFilter }

procedure TSelectedFilter.AddSkipped(const test: ITest);
var
  testResult: TTestInsightResult;
begin
  testResult := TTestInsightResult.Create(TResultType.Skipped,
    test.Fixture.Name + '.' + test.Name);
  testResult.UnitName := test.Fixture.UnitName;
  testResult.ClassName := test.Fixture.TestClass.ClassName;
  testResult.MethodName := test.MethodName;
    fClient.PostResult(testResult);
end;

constructor TSelectedFilter.Create(const client: ITestInsightClient);
begin
  inherited Create;
  fClient := client;
  fSelectedTests := fClient.GetTests;
  fSelectTest := Length(fSelectedTests) > 0;
end;

function TSelectedFilter.IsEmpty: Boolean;
begin
  Result := False;
end;

function TSelectedFilter.Match(const test: ITest): Boolean;
var
  s: string;
begin
  if fSelectTest or not fClient.Options.ExecuteTests then
  begin
    for s in fSelectedTests do
    begin
      if s = test.Fixture.UnitName + '.' + test.Fixture.Name + '.' + test.Name then
        Exit(True);
      if s = test.Fixture.UnitName + '.' + test.Fixture.FixtureInstance.ClassName + '.' + test.MethodName then
        Exit(True);
    end;
    Result := False;
    AddSkipped(test);
  end
  else
    Result := True;
end;

procedure RunRegisteredTests(const baseUrl: string);
var
  client: ITestInsightClient;
  logger: TTestInsightLogger;
  runner: ITestRunner;
  results: IRunResults;
begin
  client := TTestInsightRestClient.Create(baseUrl);
  logger := TTestInsightLogger.Create(client);
  TDUnitX.Filter := TSelectedFilter.Create(client);
  runner := TDUnitXTestRunner.Create(logger);
  runner.FailsOnNoAsserts := True;
  runner.UseRTTI := True;
  results := runner.Execute;
  TDUnitX.Filter := nil;
end;

{ TTestInsightLogger }

constructor TTestInsightLogger.Create(const client: ITestInsightClient);
begin
  fClient := client;
  fSelectedTests := fClient.GetTests;
  fSelectTest := Length(fSelectedTests) <> 0;
end;

procedure TTestInsightLogger.OnBeginTest(const threadId: TThreadID;
  const Test: ITestInfo);
var
  activeTest: TTestInsightResult;
begin
  activeTest := TTestInsightResult.Create(
    TResultType.Running, test.Fixture.Name + '.' + test.Name);
  activeTest.Duration := 0;
  activeTest.UnitName := test.Fixture.UnitName;
  activeTest.ClassName := test.Fixture.TestClass.ClassName;
  activeTest.MethodName := test.MethodName;
  fClient.PostResult(activeTest);
end;

procedure TTestInsightLogger.OnEndSetupFixture(const threadId: TThreadID;
  const fixture: ITestFixtureInfo);
begin
end;

procedure TTestInsightLogger.OnEndSetupTest(const threadId: TThreadID;
  const Test: ITestInfo);
begin
end;

procedure TTestInsightLogger.OnEndTearDownFixture(const threadId: TThreadID;
  const fixture: ITestFixtureInfo);
begin
end;

procedure TTestInsightLogger.OnEndTeardownTest(const threadId: TThreadID;
  const Test: ITestInfo);
begin
end;

procedure TTestInsightLogger.OnEndTest(const threadId: TThreadID;
  const Test: ITestResult);
begin
end;

procedure TTestInsightLogger.OnEndTestFixture(const threadId: TThreadID;
  const results: IFixtureResult);
begin
end;

procedure TTestInsightLogger.OnExecuteTest(const threadId: TThreadID;
  const Test: ITestInfo);
begin
end;

procedure TTestInsightLogger.OnLog(const logType: TLogLevel; const msg: string);
begin
end;

procedure TTestInsightLogger.OnSetupFixture(const threadId: TThreadID;
  const fixture: ITestFixtureInfo);
begin
end;

procedure TTestInsightLogger.OnSetupTest(const threadId: TThreadID;
  const Test: ITestInfo);
begin
end;

procedure TTestInsightLogger.OnStartTestFixture(const threadId: TThreadID;
  const fixture: ITestFixtureInfo);
begin
end;

procedure TTestInsightLogger.OnTearDownFixture(const threadId: TThreadID;
  const fixture: ITestFixtureInfo);
begin
end;

procedure TTestInsightLogger.OnTeardownTest(const threadId: TThreadID;
  const Test: ITestInfo);
begin
end;

procedure TTestInsightLogger.OnTestError(const threadId: TThreadID;
  const Error: ITestError);
begin
  PostTestResult(TResultType.Error, Error);
end;

procedure TTestInsightLogger.OnTestFailure(const threadId: TThreadID;
  const Failure: ITestError);
const
  SNoAssertions = 'No assertions were made during the test';
begin
  // TODO: ask for special exception type or event from DUnitX in the future
  if Failure.ExceptionMessage = SNoAssertions then
    PostTestResult(TResultType.Warning, Failure)
  else
    PostTestResult(TResultType.Failed, Failure);
end;

procedure TTestInsightLogger.OnTestIgnored(const threadId: TThreadID;
  const AIgnored: ITestResult);
begin
  PostTestResult(TResultType.Skipped, AIgnored);
end;

procedure TTestInsightLogger.OnTestingEnds(const RunResults: IRunResults);
begin
  fClient.FinishedTesting;
end;

procedure TTestInsightLogger.OnTestingStarts(const threadId: TThreadId;
  testCount, testActiveCount: Cardinal);
begin
  fClient.StartedTesting(testCount);
end;

procedure TTestInsightLogger.OnTestMemoryLeak(const threadId: TThreadID;
  const Test: ITestResult);
begin
  PostTestResult(TResultType.Warning, test);
end;

procedure TTestInsightLogger.OnTestSuccess(const threadId: TThreadID;
  const Test: ITestResult);
begin
  PostTestResult(TResultType.Passed, test);
end;

procedure TTestInsightLogger.PostTestResult(resultType: TResultType;
  const test: ITestResult);
var
  error: ITestError;
  testResult: TTestInsightResult;
begin
  testResult := TTestInsightResult.Create(resultType, test.Test.Fixture.Name + '.' + test.Test.Name);
  testResult.Duration := Trunc(test.Duration.TotalMilliseconds);
  testResult.UnitName := test.Test.Fixture.UnitName;
  testResult.ClassName := test.Test.Fixture.TestClass.ClassName;
  testResult.MethodName := test.Test.MethodName;
  testResult.ExceptionMessage := test.Message;
  if test.QueryInterface(ITestError, error) = 0 then
  begin
    if resultType = TResultType.Error then
      testResult.ExceptionMessage := Format('%s with message ''%s''', [
        error.ExceptionClass.ClassName, error.ExceptionMessage]);
    GetExtendedDetails(error.ExceptionAddress, testResult);
  end;
  fClient.PostResult(testResult);
end;

end.
