{******************************************************************************}
{                                                                              }
{           TestInsight                                                        }
{                                                                              }
{           Copyright (c) 2015 Stefan Glienke - All rights reserved            }
{                                                                              }
{           http://www.dsharp.org                                              }
{                                                                              }
{******************************************************************************}

unit TestInsight.Client;

interface

{.$DEFINE USE_JCLDEBUG}
{.$DEFINE USE_MADEXCEPT}
{$IFDEF USE_MADEXCEPT}
  {.$DEFINE USE_ANSISTRING} // madExcept 3 used AnsiString
{$ENDIF}

{$IF CompilerVersion >= 25} // XE4 and higher
  {$ZEROBASEDSTRINGS OFF}
  {$LEGACYIFEND ON}
{$IFEND}

uses
  Classes,
  Generics.Collections,
  IniFiles,
  IdHttp;

const
  DefaultUrl = 'http://localhost:8102/';

{$SCOPEDENUMS ON}

type
  TResultType = (Passed, Failed, Error, Warning, Skipped, Running);

  TTestInsightResult = record
    ResultType: TResultType;
    TestName: string;
    Duration: Cardinal;
    ExceptionMessage: string;
    Status: string;
    UnitName: string;
    ClassName: string;
    MethodName: string;
    LineNumber: Integer;
    constructor Create(const resultType: TResultType; const testName: string);
    function ToJson: string;
  end;

  TTestInsightOptions = record
    ExecuteTests: Boolean;
    ShowProgress: Boolean;
  end;

  ITestInsightClient = interface
    ['{14BE2648-7815-4026-A3D1-D62E9B0D8E43}']
    procedure ClearTests;
    function GetHasError: Boolean;
    function GetTests: TArray<string>;
    function GetOptions: TTestInsightOptions;
    procedure SetOptions(const value: TTestInsightOptions);

    procedure StartedTesting(const totalCount: Integer);
    procedure FinishedTesting;
    procedure PostResult(const testResult: TTestInsightResult;
      sendImmediately: Boolean = False);
    procedure PostResults(const testResults: array of TTestInsightResult;
      sendImmediately: Boolean = False);

    property HasError: Boolean read GetHasError;
    property Options: TTestInsightOptions read GetOptions write SetOptions;
  end;

  TTestInsightClientBase = class abstract(TInterfacedObject)
  private
    fOptions: TTestInsightOptions;
    fTestResults: TList<TTestInsightResult>;
    function GetOptions: TTestInsightOptions;
    procedure SetOptions(const value: TTestInsightOptions);
  protected
    function OpenIni: TCustomIniFile;
    function ParseOptions(const response: string): TTestInsightOptions;
    function ParseTests(const response: string): TArray<string>;
    procedure Post(url: string; const args: array of const;
      const content: string = ''); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PostResult(const testResult: TTestInsightResult;
      sendImmediately: Boolean);
    procedure PostResults(const testResults: array of TTestInsightResult;
      sendImmediately: Boolean);
    procedure StartedTesting(const totalCount: Integer);
    procedure FinishedTesting;

    property Options: TTestInsightOptions read GetOptions write SetOptions;
  end;

  TTestInsightRestClient = class(TTestInsightClientBase, ITestInsightClient)
  private
    fHttp: TIdHTTP;
    fRequest: TStrings;
    fBaseUrl: string;
    fHasError: Boolean;
  protected
    procedure Post(url: string; const args: array of const;
      const content: string); override;
  public
    constructor Create(baseUrl: string = DefaultUrl);
    destructor Destroy; override;

    procedure ClearTests;
    function GetHasError: Boolean;
    function GetTests: TArray<string>;
  end;

{$IFDEF ANDROID}
  TTestInsightLogcatClient = class(TTestInsightClientBase, ITestInsightClient)
  strict private
    const
      TAG = 'testinsight';
    var
      fSelectedTests: TArray<string>;
  protected
    procedure Post(url: string; const args: array of const;
      const content: string = ''); override;
  public
    constructor Create;

    procedure ClearTests;
    function GetHasError: Boolean;
    function GetTests: TArray<string>;
  end;
{$ENDIF}

procedure GetExtendedDetails(address: Pointer; var testResult: TTestInsightResult);

implementation

uses
{$IFDEF ANDROID}
  Androidapi.Log,
{$ENDIF}
{$IF CompilerVersion >= 27} // XE6 and higher
  JSON,
{$ELSE}
  DBXJSON,
{$IFEND}
{$IFDEF USE_JCLDEBUG}
  JclDebug,
{$ENDIF}
{$IFDEF USE_MADEXCEPT}
  madMapFile,
{$ENDIF}
  StrUtils,
  SysUtils;

procedure GetExtendedDetails(address: Pointer; var testResult: TTestInsightResult);
{$IF Defined(USE_JCLDEBUG)}
var
  info: TJclLocationInfo;
begin
  info := GetLocationInfo(address);
  testResult.UnitName := info.UnitName;
  testResult.LineNumber := info.LineNumber;
end;
{$ELSEIF Defined(USE_MADEXCEPT)}
var
  moduleName: {$IFDEF USE_ANSISTRING}AnsiString;{$ELSE}string;{$ENDIF}
  unitName: {$IFDEF USE_ANSISTRING}AnsiString;{$ELSE}string;{$ENDIF}
  publicName: {$IFDEF USE_ANSISTRING}AnsiString;{$ELSE}string;{$ENDIF}
  publicAddr: Pointer;
  line: Integer;
begin
  if GetMapFileInfos(address, moduleName, unitName, publicName, publicAddr, line) then
  begin
    testResult.UnitName := string(unitName);
    testResult.LineNumber := line;
  end;
end;
{$ELSE}
begin
end;
{$IFEND}

{$IF CompilerVersion <= 27} // XE6 and lower
type
  TJSONValueHelper = class helper for TJSONValue
    function ToJSON: string;
  end;

function TJSONValueHelper.ToJSON: string;
var
  LBytes: TBytes;
begin
  SetLength(LBytes, Length(ToString) * 6);
  SetLength(LBytes, ToBytes(LBytes, 0));
  Result := TEncoding.Default.GetString(LBytes);
end;
{$IFEND}

{$IF CompilerVersion <= 26} // XE5 and lower
type
  TJSONObjectHelper = class helper for TJSONObject
  public
    function GetPair(const I: Integer): TJSONPair; inline;
    function GetValue(const Name: string): TJSONValue;
    property Pairs[const Index: Integer]: TJSONPair read GetPair;
    property Values[const Name: string]: TJSONValue read GetValue;
  end;

  TJSONArrayHelper = class helper for TJSONArray
  public
    function GetCount: Integer; inline;
    function GetValue(const Index: Integer): TJSONValue; inline;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TJSONValue read GetValue;
  end;

function TJSONObjectHelper.GetPair(const I: Integer): TJSONPair;
begin
  Result := Get(I);
end;

function TJSONObjectHelper.GetValue(const Name: string): TJSONValue;
var
  LPair: TJSONPair;
begin
  LPair := Get(Name);
  if LPair <> nil then
    Result := LPair.JSONValue
  else
    Result := nil;
end;

function TJSONArrayHelper.GetCount: Integer;
begin
  Result := Size;
end;

function TJSONArrayHelper.GetValue(const Index: Integer): TJSONValue;
begin
  Result := Get(Index);
end;
{$IFEND}

{ TTestInsightResult }

constructor TTestInsightResult.Create(const resultType: TResultType;
  const testName: string);
begin
  Self.ResultType := resultType;
  Self.TestName := testName;
  Self.Duration := 0;
  Self.LineNumber := 0;
end;

function TTestInsightResult.ToJson: string;
const
  ResultTypeStrings: array[TResultType] of string = (
    'Passed', 'Failed', 'Error', 'Warning', 'Skipped', 'Running');
var
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  try
    obj.AddPair('resulttype', ResultTypeStrings[ResultType]);
    obj.AddPair('testname', TestName);
    obj.AddPair('duration', TJSONNumber.Create(Duration));
    obj.AddPair('exceptionmessage', ExceptionMessage);
    obj.AddPair('unitname', UnitName);
    obj.AddPair('classname', ClassName);
    obj.AddPair('methodname', MethodName);
    obj.AddPair('linenumber', TJSONNumber.Create(LineNumber));
    obj.AddPair('status', Status); // Keep it last as it can have very long size
    Result := obj.ToJSON;
  finally
    obj.Free;
  end;
end;

{ TTestInsightClientBase }

constructor TTestInsightClientBase.Create;
begin
  inherited Create;
  fTestResults := TList<TTestInsightResult>.Create;
end;

destructor TTestInsightClientBase.Destroy;
begin
  fTestResults.Free;
  inherited;
end;

procedure TTestInsightClientBase.FinishedTesting;
begin
  if fTestResults.Count > 0 then
    PostResults(fTestResults.ToArray, True);
  Post('tests/finished', []);
end;

function TTestInsightClientBase.GetOptions: TTestInsightOptions;
begin
  Result := fOptions;
end;

function TTestInsightClientBase.OpenIni: TCustomIniFile;
var
  fileName: string;
begin
  Result := nil;
{$IFDEF ANDROID}
  fileName := '/storage/emulated/0/';
{$ELSE}
  fileName := ExtractFilePath(ParamStr(0));
{$ENDIF}
  fileName := fileName + 'TestInsightSettings.ini';
  if FileExists(fileName) then
  begin
    try
      // Use TMemIniFile it does not save the content on destroy
      Result := TMemIniFile.Create(fileName);
    except
      on EFOpenError do
        Exit; // Enable Read External Storage permission in project options
      else
        raise;
    end;
  end;
end;

function TTestInsightClientBase.ParseOptions(
  const response: string): TTestInsightOptions;
var
  obj: TJSONObject;
begin
  obj := TJSONObject.ParseJSONValue(response) as TJSONObject;
  try
    Result := Default(TTestInsightOptions);
    if Assigned(obj) then
    begin
      Result.ExecuteTests := obj.Values['ExecuteTests'] is TJSONTrue;
      Result.ShowProgress := obj.Values['ShowProgress'] is TJSONTrue;
    end;
  finally
    obj.Free;
  end;
end;

function TTestInsightClientBase.ParseTests(const response: string): TArray<string>;
var
  obj: TJSONObject;
  arr: TJSONArray;
  i: Integer;
begin
  obj := TJSONObject.ParseJSONValue(response) as TJSONObject;
  try
    arr := TJSONArray(obj.Pairs[0].JsonValue);
    SetLength(Result, arr.Count);
    for i := 0 to arr.Count - 1 do
      Result[i] := TJSONString(arr.Items[i]).Value;
  finally
    obj.Free;
  end;
end;

procedure TTestInsightClientBase.PostResult(
  const testResult: TTestInsightResult; sendImmediately: Boolean);
begin
  PostResults([testResult], sendImmediately);
end;

procedure TTestInsightClientBase.PostResults(
  const testResults: array of TTestInsightResult; sendImmediately: Boolean);
var
  testResult: TTestInsightResult;
  content: string;
begin
  if Length(testResults) = 0 then
    Exit;
  if sendImmediately or (fOptions.ShowProgress and fOptions.ExecuteTests) then
  begin
    content := '[';
    for testResult in testResults do
      content := content + testResult.ToJson + ',';
    content[Length(content)] := ']';
    Post('tests/results', [], content);
  end
  else
    fTestResults.AddRange(testResults);
end;

procedure TTestInsightClientBase.SetOptions(const value: TTestInsightOptions);
begin
  fOptions := value;
  if fOptions.ShowProgress then
    PostResults(fTestResults.ToArray, True);
end;

procedure TTestInsightClientBase.StartedTesting(const totalCount: Integer);
begin
  Post('tests/started?totalcount=%d', [totalCount]);
end;

{ TTestInsightRestClient }

constructor TTestInsightRestClient.Create(baseUrl: string);
var
  iniFile: TCustomIniFile;
begin
  inherited Create;
  fHttp := TIdHttp.Create(nil);
  fHttp.HTTPOptions := fHttp.HTTPOptions + [hoKeepOrigProtocol] - [hoForceEncodeParams];
  fHttp.Request.ContentType := 'application/json';
  fHttp.ReadTimeout := 5000;
  fHttp.UseNagle := False;
  fRequest := TStringList.Create;

  iniFile := OpenIni;
  if Assigned(iniFile) then
  try
    baseUrl := iniFile.ReadString('Config', 'BaseUrl', baseUrl);
  finally
    iniFile.Free;
  end;

  if baseUrl[Length(baseUrl)] <> '/' then
    baseUrl := baseUrl + '/';
  fBaseUrl := baseUrl;

  try
    fOptions := ParseOptions(fHttp.Get(fBaseUrl + 'options'));
  except
    fHasError := True;
  end;
end;

destructor TTestInsightRestClient.Destroy;
begin
  fTestResults.Free;
  fRequest.Free;
{$IFNDEF AUTOREFCOUNT}
  fHttp.Free;
{$ELSE}
  fHttp.DisposeOf; // Fixes Indy leak bugs
{$ENDIF}
end;

procedure TTestInsightRestClient.ClearTests;
begin
  if not fHasError then
  try
    fHttp.Delete(fBaseUrl + 'tests');
  except
    fHasError := True;
  end;
end;

function TTestInsightRestClient.GetHasError: Boolean;
begin
  Result := fHasError;
end;

function TTestInsightRestClient.GetTests: TArray<string>;
begin
  if not fHasError then
  try
    Result := ParseTests(fHttp.Get(fBaseUrl + 'tests'));
  except
    fHasError := True;
  end;
end;

procedure TTestInsightRestClient.Post(url: string;
  const args: array of const; const content: string);
begin
  if not fHasError then
  try
    url := Format(url, args);
    fRequest.Text := content;
    try
      fHttp.Post(fBaseUrl + url, fRequest);
    finally
      fRequest.Clear;
    end;
  except
    fHasError := True;
  end;
end;

{$IFDEF ANDROID}
{ TTestInsightLogcatClient }

procedure TTestInsightLogcatClient.ClearTests;
begin
  __android_log_write(ANDROID_LOG_DEBUG, TAG, 'tests/delete');
end;

constructor TTestInsightLogcatClient.Create;
var
  iniFile: TCustomIniFile;
begin
  inherited;

  iniFile := OpenIni;
  if Assigned(iniFile) then
  try
    if iniFile.ValueExists('Config', 'SelectedTests') then
    begin
      fSelectedTests := ParseTests(iniFile.ReadString('Config',
        'SelectedTests', ''));
    end;
  finally
    iniFile.Free;
  end;
end;

function TTestInsightLogcatClient.GetHasError: Boolean;
begin
  Result := False;
end;

function TTestInsightLogcatClient.GetTests: TArray<string>;
begin
  Result := fSelectedTests;
end;

procedure TTestInsightLogcatClient.Post(url: string;
  const args: array of const; const content: string = '');
const
  MAX_PAYLOAD_SIZE = (4 * 1024) - 32 {~sizeof(logger_entry)}; // Hardcoded in android
  MESSAGE_SPLIT = MAX_PAYLOAD_SIZE - 100; // Keep some reserve

  procedure SendData(const data: string); //noinline - let the marshaller to free data ASAP
  var
    m: TMarshaller;
  begin
    __android_log_write(ANDROID_LOG_DEBUG, TAG, m.AsAnsi(data).ToPointer);
  end;

  procedure SendChunked(const data: string);
  var
    i: Integer;
  begin
    i := 1;
    SendData('chunk:start');
    while i <= Length(data) do
    begin
      SendData(Format('chunk:%d|', [i]) + Copy(data, i, MESSAGE_SPLIT));
      Inc(i, MESSAGE_SPLIT);
    end;
    SendData('chunk:end');
  end;

begin
  if Length(args) > 0 then
    url := Format(url, args) + content
  else if content <> '' then
    url := url + '?' + content;

  if (Length(url) > MAX_PAYLOAD_SIZE) then
    SendChunked(url)
  else
    SendData(url);
end;

{$ENDIF}

end.
