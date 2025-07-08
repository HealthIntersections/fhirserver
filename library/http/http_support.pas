unit http_support;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, fgl, Generics.Collections;

type
  THttpMethod = (hmGET, hmPOST, hmPUT, hmDELETE, hmHEAD, hmOPTIONS);

  { THttpHeaders - case-insensitive header management }
  THttpHeaders = class
  private
    FHeaders: TStringList;
    function NormalizeHeaderName(const Name: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    
    function GetHeader(const Name: string): string;
    procedure SetHeader(const Name, Value: string);
    procedure AddHeader(const Name, Value: string);
    function HasHeader(const Name: string): boolean;
    procedure Clear;
    function Count: Integer;
    function GetName(Index: Integer): string;
    function GetValue(Index: Integer): string;
    
    // For building HTTP response headers
    function AsHeaderString: string;
  end;

  { THttpParameters - handles multiple values per parameter name }
  THttpParameters = class
  private
    FParams: TStringList; // Stores name=value pairs, allowing duplicates
  public
    constructor Create;
    destructor Destroy; override;
    
    // Add a parameter (allows multiple values for same name)
    procedure Add(const Name, Value: string);
    
    // Get first value for a parameter name
    function GetValue(const Name: string): string; overload;
    function GetValue(const Name: string; const Default: string): string; overload;
    
    // Get all values for a parameter name
    function GetValues(const Name: string): TStringArray;
    
    // Check if parameter exists
    function HasParameter(const Name: string): boolean;
    
    // Get count of values for a specific parameter
    function GetValueCount(const Name: string): Integer;
    
    // Clear all parameters
    procedure Clear;
    
    // Get all parameter names (unique)
    function GetParameterNames: TStringArray;
    
    // For debugging
    function AsString: string;
  end;

  { THttpRequest }
  THttpRequest = class
  private
    FMethod: THttpMethod;
    FPath: string;
    FQuery: string;
    FVersion: string;
    FHeaders: THttpHeaders;
    FBody: TBytes;
    FParameters: THttpParameters;
    FClientIP: string;
    FTimestamp: TDateTime;
    
    procedure ParseQuery(const QueryString: string);
    function URLDecode(const Encoded: string): string;
    function ExtractEncodingFromContentType(const ContentType: string): TEncoding;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Parse a raw HTTP request line
    function ParseRequestLine(const RequestLine: string): Boolean;
    
    // Body management
    procedure SetBody(const Data: TBytes);
    procedure SetBodyFromString(const Text: string; Encoding: TEncoding = nil);
    procedure AppendToBody(const Data: TBytes);
    procedure ClearBody;
    
    // Properties
    property Method: THttpMethod read FMethod write FMethod;
    property Path: string read FPath write FPath;
    property Query: string read FQuery write FQuery;
    property Version: string read FVersion write FVersion;
    property Headers: THttpHeaders read FHeaders;
    property Body: TBytes read FBody;
    property Parameters: THttpParameters read FParameters;
    property ClientIP: string read FClientIP write FClientIP;
    property Timestamp: TDateTime read FTimestamp write FTimestamp;
    
    // Convenience methods
    function GetParameter(const Name: string): string; overload;
    function GetParameter(const Name: string; const Default: string): string; overload;
    function GetParameterValues(const Name: string): TStringArray;
    function HasParameter(const Name: string): boolean;
    
    function GetHeader(const Name: string): string;
    function HasHeader(const Name: string): boolean;
    
    // Body content helpers
    function GetBodyAsString: string;
    function GetContentLength: Integer;
    function GetContentType: string;
    function HasBody: Boolean;
    
    // Debug info
    function AsString: string;
  end;

  { THttpResponse }
  THttpResponse = class
  private
    FStatusCode: Integer;
    FStatusMessage: string;
    FHeaders: THttpHeaders;
    FBody: TBytes;
    FTimestamp: TDateTime;
    FSent: Boolean;              
    function GetContentLength: Integer;  
    function GetContentType: string;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Status management
    procedure SetStatus(Code: Integer; const Message: string = '');
    
    // Content management
    procedure SetTextContent(const Text: string; const ContentType: string = 'text/plain; charset=utf-8');
    procedure SetJSONContent(const JSON: string);
    procedure SetHTMLContent(const HTML: string);
    procedure SetBinaryContent(const Data: TBytes; const ContentType: string);
    procedure SetFileContent(const FileName: string; const ContentType: string = '');
    procedure AppendContent(const Data: string);
    procedure AppendBinaryContent(const Data: TBytes);
    procedure ClearBody;
    
    // Header management
    procedure SetHeader(const Name, Value: string);
    procedure AddHeader(const Name, Value: string);
    function GetHeader(const Name: string): string;
    
    // Common headers
    procedure SetContentType(const ContentType: string);
    procedure SetCacheControl(const CacheControl: string);
    procedure SetCookie(const Name, Value: string; const Path: string = '/'; 
      const Domain: string = ''; MaxAge: Integer = -1; Secure: Boolean = False; 
      HttpOnly: Boolean = False);
    
    // Redirect
    procedure Redirect(const URL: string; Permanent: Boolean = False);
    
    // Build complete HTTP response
    function BuildResponse: TBytes;
    
    // Properties
    property StatusCode: Integer read FStatusCode write FStatusCode;
    property StatusMessage: string read FStatusMessage write FStatusMessage;
    property Headers: THttpHeaders read FHeaders;
    property Body: TBytes read FBody;
    property Timestamp: TDateTime read FTimestamp write FTimestamp;
    property Sent: Boolean read FSent write FSent;
    
    // Convenience properties
    property ContentLength: Integer read GetContentLength;
    property ContentType: string read GetContentType write SetContentType;
  end;

// Utility functions
function MethodToString(Method: THttpMethod): string;
function StringToMethod(const S: string): THttpMethod;
function GetStatusMessage(Code: Integer): string;
function URLEncode(const S: string): string;
function URLDecode(const S: string): string;

implementation

uses
  StrUtils;

{ Utility functions }

function MethodToString(Method: THttpMethod): string;
begin
  case Method of
    hmGET: Result := 'GET';
    hmPOST: Result := 'POST';
    hmPUT: Result := 'PUT';
    hmDELETE: Result := 'DELETE';
    hmHEAD: Result := 'HEAD';
    hmOPTIONS: Result := 'OPTIONS';
  end;
end;

function StringToMethod(const S: string): THttpMethod;
var
  UpperS: string;
begin
  UpperS := UpperCase(S);
  if UpperS = 'GET' then
    Result := hmGET
  else if UpperS = 'POST' then
    Result := hmPOST
  else if UpperS = 'PUT' then
    Result := hmPUT
  else if UpperS = 'DELETE' then
    Result := hmDELETE
  else if UpperS = 'HEAD' then
    Result := hmHEAD
  else if UpperS = 'OPTIONS' then
    Result := hmOPTIONS
  else
    raise Exception.CreateFmt('Unknown HTTP method: %s', [S]);
end;

function GetStatusMessage(Code: Integer): string;
begin
  if Code = 200 then
    Result := 'OK'
  else if Code = 201 then
    Result := 'Created'
  else if Code = 204 then
    Result := 'No Content'
  else if Code = 301 then
    Result := 'Moved Permanently'
  else if Code = 302 then
    Result := 'Found'
  else if Code = 304 then
    Result := 'Not Modified'
  else if Code = 400 then
    Result := 'Bad Request'
  else if Code = 401 then
    Result := 'Unauthorized'
  else if Code = 403 then
    Result := 'Forbidden'
  else if Code = 404 then
    Result := 'Not Found'
  else if Code = 405 then
    Result := 'Method Not Allowed'
  else if Code = 409 then
    Result := 'Conflict'
  else if Code = 410 then
    Result := 'Gone'
  else if Code = 429 then
    Result := 'Too Many Requests'
  else if Code = 500 then
    Result := 'Internal Server Error'
  else if Code = 501 then
    Result := 'Not Implemented'
  else if Code = 502 then
    Result := 'Bad Gateway'
  else if Code = 503 then
    Result := 'Service Unavailable'
  else
    Result := 'Unknown Status';
end;

function URLEncode(const S: string): string;
var
  i: Integer;
  c: Char;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    c := S[i];
    if c in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~'] then
      Result := Result + c
    else
      Result := Result + '%' + IntToHex(Ord(c), 2);
  end;
end;

function URLDecode(const S: string): string;
var
  i: Integer;
  hex: string;
begin
  Result := '';
  i := 1;
  while i <= Length(S) do
  begin
    if S[i] = '%' then
    begin
      if i + 2 <= Length(S) then
      begin
        hex := S[i + 1] + S[i + 2];
        try
          Result := Result + Chr(StrToInt('$' + hex));
          Inc(i, 3);
        except
          Result := Result + S[i];
          Inc(i);
        end;
      end
      else
      begin
        Result := Result + S[i];
        Inc(i);
      end;
    end
    else if S[i] = '+' then
    begin
      Result := Result + ' ';
      Inc(i);
    end
    else
    begin
      Result := Result + S[i];
      Inc(i);
    end;
  end;
end;

{ THttpHeaders }

constructor THttpHeaders.Create;
begin
  inherited Create;
  FHeaders := TStringList.Create;
  FHeaders.NameValueSeparator := ':';
  FHeaders.CaseSensitive := False; // Headers are case-insensitive
end;

destructor THttpHeaders.Destroy;
begin
  FHeaders.Free;
  inherited Destroy;
end;

function THttpHeaders.NormalizeHeaderName(const Name: string): string;
begin
  // Convert to standard header case (Title-Case)
  Result := LowerCase(Name);
  if Length(Result) > 0 then
    Result[1] := UpCase(Result[1]);
end;

function THttpHeaders.GetHeader(const Name: string): string;
var
  Index: Integer;
begin
  Index := FHeaders.IndexOfName(Name);
  if Index >= 0 then
    Result := FHeaders.ValueFromIndex[Index]
  else
    Result := '';
end;

procedure THttpHeaders.SetHeader(const Name, Value: string);
var
  Index: Integer;
begin
  Index := FHeaders.IndexOfName(Name);
  if Index >= 0 then
    FHeaders.ValueFromIndex[Index] := Value
  else
    FHeaders.Add(Name + ':' + Value);
end;

procedure THttpHeaders.AddHeader(const Name, Value: string);
begin
  FHeaders.Add(Name + ':' + Value);
end;

function THttpHeaders.HasHeader(const Name: string): boolean;
begin
  Result := FHeaders.IndexOfName(Name) >= 0;
end;

procedure THttpHeaders.Clear;
begin
  FHeaders.Clear;
end;

function THttpHeaders.Count: Integer;
begin
  Result := FHeaders.Count;
end;

function THttpHeaders.GetName(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FHeaders.Count) then
    Result := FHeaders.Names[Index]
  else
    Result := '';
end;

function THttpHeaders.GetValue(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FHeaders.Count) then
    Result := FHeaders.ValueFromIndex[Index]
  else
    Result := '';
end;

function THttpHeaders.AsHeaderString: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FHeaders.Count - 1 do
  begin
    Result := Result + FHeaders.Names[i] + ': ' + FHeaders.ValueFromIndex[i] + #13#10;
  end;
end;

{ THttpParameters }

constructor THttpParameters.Create;
begin
  inherited Create;
  FParams := TStringList.Create;
  FParams.NameValueSeparator := '=';
  FParams.CaseSensitive := True; // Parameter names are case-sensitive
end;

destructor THttpParameters.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

procedure THttpParameters.Add(const Name, Value: string);
begin
  FParams.Add(Name + '=' + Value);
end;

function THttpParameters.GetValue(const Name: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FParams.Count - 1 do
  begin
    if SameText(FParams.Names[i], Name) then
    begin
      Result := FParams.ValueFromIndex[i];
      Exit;
    end;
  end;
end;

function THttpParameters.GetValue(const Name: string; const Default: string): string;
begin
  Result := GetValue(Name);
  if Result = '' then
    Result := Default;
end;

function THttpParameters.GetValues(const Name: string): TStringArray;
var
  i: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    for i := 0 to FParams.Count - 1 do
    begin
      if SameText(FParams.Names[i], Name) then
        Values.Add(FParams.ValueFromIndex[i]);
    end;
    
    SetLength(Result, Values.Count);
    for i := 0 to Values.Count - 1 do
      Result[i] := Values[i];
  finally
    Values.Free;
  end;
end;

function THttpParameters.HasParameter(const Name: string): boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FParams.Count - 1 do
  begin
    if SameText(FParams.Names[i], Name) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function THttpParameters.GetValueCount(const Name: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FParams.Count - 1 do
  begin
    if SameText(FParams.Names[i], Name) then
      Inc(Result);
  end;
end;

procedure THttpParameters.Clear;
begin
  FParams.Clear;
end;

function THttpParameters.GetParameterNames: TStringArray;
var
  i: Integer;
  Names: TStringList;
  ParamName: string;
begin
  Names := TStringList.Create;
  Names.Duplicates := dupIgnore;
  Names.Sorted := True;
  
  try
    for i := 0 to FParams.Count - 1 do
    begin
      ParamName := FParams.Names[i];
      if ParamName <> '' then
        Names.Add(ParamName);
    end;
    
    SetLength(Result, Names.Count);
    for i := 0 to Names.Count - 1 do
      Result[i] := Names[i];
  finally
    Names.Free;
  end;
end;

function THttpParameters.AsString: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FParams.Count - 1 do
  begin
    if i > 0 then
      Result := Result + ', ';
    Result := Result + FParams[i];
  end;
end;

{ THttpRequest }

constructor THttpRequest.Create;
begin
  inherited Create;
  FHeaders := THttpHeaders.Create;
  SetLength(FBody, 0);
  FParameters := THttpParameters.Create;
  FTimestamp := Now;
  FVersion := 'HTTP/1.1';
end;

destructor THttpRequest.Destroy;
begin
  FHeaders.Free;
  FParameters.Free;
  SetLength(FBody, 0);
  inherited Destroy;
end;

function THttpRequest.ParseRequestLine(const RequestLine: string): Boolean;
var
  Parts: TStringArray;
  PathAndQuery: TStringArray;
begin
  Result := False;
  Parts := RequestLine.Split(' ');
  if Length(Parts) < 3 then Exit;
  
  // Parse method
  try
    FMethod := StringToMethod(Parts[0]);
  except
    Exit;
  end;
  
  // Parse path and query
  PathAndQuery := Parts[1].Split('?');
  FPath := URLDecode(PathAndQuery[0]);
  
  if Length(PathAndQuery) > 1 then
  begin
    FQuery := PathAndQuery[1];
    ParseQuery(FQuery);
  end;
  
  // Parse version
  FVersion := Parts[2];
  
  Result := True;
end;

procedure THttpRequest.ParseQuery(const QueryString: string);
var
  Pairs: TStringArray;
  Pair: string;
  EqPos: Integer;
  Name, Value: string;
begin
  FParameters.Clear;
  if QueryString = '' then Exit;
  
  Pairs := QueryString.Split('&');
  for Pair in Pairs do
  begin
    EqPos := Pos('=', Pair);
    if EqPos > 0 then
    begin
      Name := URLDecode(Copy(Pair, 1, EqPos - 1));
      Value := URLDecode(Copy(Pair, EqPos + 1, Length(Pair)));
    end
    else
    begin
      Name := URLDecode(Pair);
      Value := '';
    end;
    
    FParameters.Add(Name, Value);
  end;
end;

function THttpRequest.ExtractEncodingFromContentType(const ContentType: string): TEncoding;
var
  LowerContentType: string;
  CharsetPos: Integer;
  CharsetValue: string;
  SemicolonPos: Integer;
begin
  Result := nil;
  
  if ContentType = '' then
    Exit;
    
  LowerContentType := LowerCase(ContentType);
  CharsetPos := Pos('charset=', LowerContentType);
  
  if CharsetPos > 0 then
  begin
    CharsetValue := Copy(ContentType, CharsetPos + 8, Length(ContentType));
    
    // Remove anything after semicolon
    SemicolonPos := Pos(';', CharsetValue);
    if SemicolonPos > 0 then
      CharsetValue := Copy(CharsetValue, 1, SemicolonPos - 1);
      
    // Remove quotes if present
    CharsetValue := Trim(CharsetValue);
    if (Length(CharsetValue) > 1) and (CharsetValue[1] = '"') and (CharsetValue[Length(CharsetValue)] = '"') then
      CharsetValue := Copy(CharsetValue, 2, Length(CharsetValue) - 2);
    
    CharsetValue := LowerCase(Trim(CharsetValue));
    
    // Map common charset names to TEncoding
    if (CharsetValue = 'utf-8') or (CharsetValue = 'utf8') then
      Result := TEncoding.UTF8
    else if (CharsetValue = 'utf-16') or (CharsetValue = 'utf16') then
      Result := TEncoding.Unicode
    else if (CharsetValue = 'utf-16be') or (CharsetValue = 'utf16be') then
      Result := TEncoding.BigEndianUnicode
    else if (CharsetValue = 'ascii') or (CharsetValue = 'us-ascii') then
      Result := TEncoding.ASCII
    else if (CharsetValue = 'iso-8859-1') or (CharsetValue = 'latin1') then
      Result := TEncoding.ANSI;
    // Add more encodings as needed
  end;
end;

procedure THttpRequest.SetBody(const Data: TBytes);
begin
  SetLength(FBody, Length(Data));
  if Length(Data) > 0 then
    Move(Data[0], FBody[0], Length(Data));
end;

procedure THttpRequest.SetBodyFromString(const Text: string; Encoding: TEncoding);
var
  Data: TBytes;
  EncodingToUse: TEncoding;
begin
  if Encoding <> nil then
    EncodingToUse := Encoding
  else
    EncodingToUse := TEncoding.UTF8; // Default for requests
    
  Data := EncodingToUse.GetBytes(Text);
  SetBody(Data);
end;

procedure THttpRequest.AppendToBody(const Data: TBytes);
var
  OldLength: Integer;
begin
  if Length(Data) = 0 then Exit;
  
  OldLength := Length(FBody);
  SetLength(FBody, OldLength + Length(Data));
  Move(Data[0], FBody[OldLength], Length(Data));
end;

procedure THttpRequest.ClearBody;
begin
  SetLength(FBody, 0);
end;

function THttpRequest.GetParameter(const Name: string): string;
begin
  Result := FParameters.GetValue(Name);
end;

function THttpRequest.GetParameter(const Name: string; const Default: string): string;
begin
  Result := FParameters.GetValue(Name, Default);
end;

function THttpRequest.GetParameterValues(const Name: string): TStringArray;
begin
  Result := FParameters.GetValues(Name);
end;

function THttpRequest.HasParameter(const Name: string): boolean;
begin
  Result := FParameters.HasParameter(Name);
end;

function THttpRequest.GetHeader(const Name: string): string;
begin
  Result := FHeaders.GetHeader(Name);
end;

function THttpRequest.HasHeader(const Name: string): boolean;
begin
  Result := FHeaders.HasHeader(Name);
end;

function THttpRequest.URLDecode(const Encoded: string): string;
begin
  Result := http_support.URLDecode(Encoded);
end;

function THttpRequest.GetBodyAsString: string;
var
  ContentType: string;
  Encoding: TEncoding;
begin
  if Length(FBody) = 0 then
  begin
    Result := '';
    Exit;
  end;
  
  ContentType := GetContentType;
  Encoding := ExtractEncodingFromContentType(ContentType);
  
  // Fallback to UTF-8 for requests if no encoding specified
  if Encoding = nil then
    Encoding := TEncoding.UTF8;
  
  try
    Result := Encoding.GetString(FBody);
  except
    // If decoding fails, fall back to ASCII
    Result := TEncoding.ASCII.GetString(FBody);
  end;
end;

function THttpRequest.HasBody: Boolean;
begin
  Result := Length(FBody) > 0;
end;

function THttpRequest.GetContentLength: Integer;
var
  LengthStr: string;
begin
  LengthStr := GetHeader('Content-Length');
  if LengthStr <> '' then
    Result := StrToIntDef(LengthStr, 0)
  else
    Result := Length(FBody);
end;

function THttpRequest.GetContentType: string;
begin
  Result := GetHeader('Content-Type');
end;

function THttpRequest.AsString: string;
begin
  Result := Format('%s %s %s'#13#10'Headers: %d'#13#10'Parameters: %s'#13#10'Body size: %d bytes',
    [MethodToString(FMethod), FPath, FVersion, FHeaders.Count, FParameters.AsString, Length(FBody)]);
end;

{ THttpResponse }

constructor THttpResponse.Create;
begin
  inherited Create;
  FHeaders := THttpHeaders.Create;
  SetLength(FBody, 0);
  FStatusCode := 200;
  FStatusMessage := 'OK';
  FTimestamp := Now;
  FSent := False;
  
  // Set default headers
  SetHeader('Server', 'LightHttp/1.0');
  SetHeader('Date', FormatDateTime('ddd, dd mmm yyyy hh:nn:ss "GMT"', Now));
end;

destructor THttpResponse.Destroy;
begin
  FHeaders.Free;
  SetLength(FBody, 0);
  inherited Destroy;
end;

procedure THttpResponse.SetStatus(Code: Integer; const Message: string);
begin
  FStatusCode := Code;
  if Message <> '' then
    FStatusMessage := Message
  else
    FStatusMessage := GetStatusMessage(Code);
end;

procedure THttpResponse.SetTextContent(const Text: string; const ContentType: string);
var
  Data: TBytes;
begin
  SetLength(FBody, 0);
  if Text <> '' then
  begin
    Data := TEncoding.UTF8.GetBytes(Text);
    SetLength(FBody, Length(Data));
    Move(Data[0], FBody[0], Length(Data));
  end;
  SetContentType(ContentType);
end;

procedure THttpResponse.SetJSONContent(const JSON: string);
begin
  SetTextContent(JSON, 'application/json; charset=utf-8');
end;

procedure THttpResponse.SetHTMLContent(const HTML: string);
begin
  SetTextContent(HTML, 'text/html; charset=utf-8');
end;

procedure THttpResponse.SetBinaryContent(const Data: TBytes; const ContentType: string);
begin
  SetLength(FBody, Length(Data));
  if Length(Data) > 0 then
    Move(Data[0], FBody[0], Length(Data));
  SetContentType(ContentType);
end;

procedure THttpResponse.SetFileContent(const FileName: string; const ContentType: string);
var
  FileStream: TFileStream;
  ContentTypeToUse: string;
begin
  if not FileExists(FileName) then
    raise Exception.CreateFmt('File not found: %s', [FileName]);
  
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(FBody, FileStream.Size);
    if FileStream.Size > 0 then
      FileStream.ReadBuffer(FBody[0], FileStream.Size);
    
    if ContentType <> '' then
      ContentTypeToUse := ContentType
    else
      ContentTypeToUse := 'application/octet-stream'; // Default binary type
      
    SetContentType(ContentTypeToUse);
  finally
    FileStream.Free;
  end;
end;

procedure THttpResponse.AppendContent(const Data: string);
var
  Bytes: TBytes;
  OldLength: Integer;
begin
  if Data <> '' then
  begin
    Bytes := TEncoding.UTF8.GetBytes(Data);
    OldLength := Length(FBody);
    SetLength(FBody, OldLength + Length(Bytes));
    Move(Bytes[0], FBody[OldLength], Length(Bytes));
  end;
end;

procedure THttpResponse.AppendBinaryContent(const Data: TBytes);
var
  OldLength: Integer;
begin
  if Length(Data) > 0 then
  begin
    OldLength := Length(FBody);
    SetLength(FBody, OldLength + Length(Data));
    Move(Data[0], FBody[OldLength], Length(Data));
  end;
end;

procedure THttpResponse.ClearBody;
begin
  SetLength(FBody, 0);
end;

procedure THttpResponse.SetHeader(const Name, Value: string);
begin
  FHeaders.SetHeader(Name, Value);
end;

procedure THttpResponse.AddHeader(const Name, Value: string);
begin
  FHeaders.AddHeader(Name, Value);
end;

function THttpResponse.GetHeader(const Name: string): string;
begin
  Result := FHeaders.GetHeader(Name);
end;

procedure THttpResponse.SetContentType(const ContentType: string);
begin
  SetHeader('Content-Type', ContentType);
end;

function THttpResponse.GetContentType: string;
begin
  Result := GetHeader('Content-Type');
end;

function THttpResponse.GetContentLength: Integer;
begin
  Result := Length(FBody);
end;

procedure THttpResponse.SetCacheControl(const CacheControl: string);
begin
  SetHeader('Cache-Control', CacheControl);
end;

procedure THttpResponse.SetCookie(const Name, Value: string; const Path: string;
  const Domain: string; MaxAge: Integer; Secure: Boolean; HttpOnly: Boolean);
var
  CookieValue: string;
begin
  CookieValue := Name + '=' + Value;
  
  if Path <> '' then
    CookieValue := CookieValue + '; Path=' + Path;
    
  if Domain <> '' then
    CookieValue := CookieValue + '; Domain=' + Domain;
    
  if MaxAge >= 0 then
    CookieValue := CookieValue + '; Max-Age=' + IntToStr(MaxAge);
    
  if Secure then
    CookieValue := CookieValue + '; Secure';
    
  if HttpOnly then
    CookieValue := CookieValue + '; HttpOnly';
    
  AddHeader('Set-Cookie', CookieValue);
end;

procedure THttpResponse.Redirect(const URL: string; Permanent: Boolean);
begin
  if Permanent then
    SetStatus(301, 'Moved Permanently')
  else
    SetStatus(302, 'Found');
    
  SetHeader('Location', URL);
end;

function THttpResponse.BuildResponse: TBytes;
var
  ResponseText: string;
  ResponseBytes: TBytes;
begin
  // Update Content-Length header
  SetHeader('Content-Length', IntToStr(Length(FBody)));
  
  // Build status line and headers
  ResponseText := Format('HTTP/1.1 %d %s'#13#10, [FStatusCode, FStatusMessage]);
  ResponseText := ResponseText + FHeaders.AsHeaderString;
  ResponseText := ResponseText + #13#10; // Empty line separating headers from body
  
  // Convert to bytes
  ResponseBytes := TEncoding.UTF8.GetBytes(ResponseText);
  
  // Combine header and body bytes
  SetLength(Result, Length(ResponseBytes) + Length(FBody));
  if Length(ResponseBytes) > 0 then
    Move(ResponseBytes[0], Result[0], Length(ResponseBytes));
  if Length(FBody) > 0 then
    Move(FBody[0], Result[Length(ResponseBytes)], Length(FBody));
  
  FSent := True;
end;

end.
