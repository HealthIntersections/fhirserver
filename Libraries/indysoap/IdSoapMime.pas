unit IdSoapMime;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  Contnrs,
  IdHeaderList,
  {$IFDEF INDY_V10}
  IdGlobalProtocols,
  {$ENDIF}
  IdSoapClasses,
  IdSoapDebug;

type
  TIdSoapMimeBase = class (TIdBaseObject)
  private
    FHeaders: TIdHeaderList;
    procedure ReadHeaders(AStream : TStream);
    procedure WriteHeaders(AStream : TStream);
  public
    constructor create;
    destructor Destroy; override;
    property Headers : TIdHeaderList read FHeaders;
  end;

  TIdSoapMimePart = class (TIdSoapMimeBase)
  private
    FOwnsContent: Boolean;
    FContent: TStream;
    FId : string;
    procedure SetContent(const AValue: TStream);
    procedure DecodeContent;
    procedure ReadFromStream(AStream : TStream; ABoundary : AnsiString);
    procedure WriteToStream(AStream : TStream);
    function GetMediaType: String;
    function GetTransferEncoding: String;
    procedure SetMediaType(const AValue: String);
    procedure SetTransferEncoding(const AValue: String);
    procedure SetId(const AValue: String);
    function GetContentDisposition: String;
    procedure SetContentDisposition(const sValue: String);
  public
    constructor create;
    destructor Destroy; override;
    property Content : TStream read FContent write SetContent;
    property OwnsContent : Boolean read FOwnsContent write FOwnsContent;
    property Id : String read FId write SetId;
    property MediaType : String read GetMediaType write SetMediaType;
    property ContentDisposition : String read GetContentDisposition write SetContentDisposition;
    property TransferEncoding : String read GetTransferEncoding write SetTransferEncoding;

    function ParamName : String;
  end;

  TIdSoapMimePartList = class (TObjectList)
  private
    function GetPartByIndex(i: integer): TIdSoapMimePart;
    function GetPart(AName : String): TIdSoapMimePart;
    function CommaList: String;
  public
    property PartByIndex[i : integer] : TIdSoapMimePart read GetPartByIndex;
    property Part[AName : String] : TIdSoapMimePart read GetPart; default;
    function AddPart(AId : String) : TIdSoapMimePart;
  end;

  TIdSoapMimeMessage = class (TIdSoapMimeBase)
  private
    FParts: TIdSoapMimePartList;
    FBoundary : Ansistring;
    FStart : String;
    FMainType : String;
    procedure AnalyseContentType(AContent : String);
    function GetMainPart: TIdSoapMimePart;
    procedure Validate;
  public
    constructor create;
    destructor Destroy; override;
    property Parts : TIdSoapMimePartList read FParts;
    property MainPart : TIdSoapMimePart read GetMainPart;
    property Boundary : ansistring read FBoundary write FBoundary;
    property Start : String read FStart write FStart;
    property MainType : String read FMainType write FMainType;

    // for multi-part forms
    function getparam(name : String) : TIdSoapMimePart;
    function hasParam(name : String) : Boolean;

    procedure ReadFromStream(AStream : TStream); overload;
    procedure ReadFromStream(AStream : TStream; AContentType : String); overload; // headers are not part of the stream

    function GetContentTypeHeader : String;
    procedure WriteToStream(AStream : TStream; AHeaders : boolean);
  end;

implementation

uses
  IdGlobal,
  IdSoapBase64,
  IdSoapConsts,
  idSoapUtilities,
  SysUtils;

const
  ASSERT_UNIT = 'IdSoapMime';

{ Stream Readers }

function ReadToValue(AStream : TStream; AValue : AnsiString):AnsiString;
const ASSERT_LOCATION = ASSERT_UNIT+'.ReadToValue';
var
  c : AnsiChar;
begin
  result := '';
  while copy(result, length(result)-length(AValue)+1, length(AValue)) <> AValue do
    begin
    IdRequire(AStream.Size - AStream.Position <> 0, ASSERT_LOCATION+': Premature termination of stream looking for value "'+string(AValue)+'"');
    AStream.Read(c, 1);
    result := result + c;
    end;
  delete(result, length(result)-length(AValue)+1, length(AValue));
end;

function ReadBytes(AStream : TStream; AByteCount : Integer):AnsiString;
const ASSERT_LOCATION = ASSERT_UNIT+'.ReadBytes';
begin
  IdRequire(AStream.Size - AStream.Position >= AByteCount, ASSERT_LOCATION+': Premature termination of stream reading "'+inttostr(AByteCount)+'" bytes');
  SetLength(result, AByteCount);
  if AByteCount > 0 then
    begin
    AStream.Read(result[1], AByteCount);
    end;
end;

{ Stream Writers }

procedure WriteString(AStream : TStream; Const AStr : AnsiString);
begin
  If AStr <> '' then
    begin
    AStream.Write(AStr[1], length(AStr));
    end;
end;

{ TIdSoapMimeBase }

constructor TIdSoapMimeBase.create;
begin
  inherited;
  FHeaders := TIdHeaderList.create{$IFDEF INDY_V10}(QuotePlain{?}){$ENDIF};
end;

destructor TIdSoapMimeBase.destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimeBase.destroy';
begin
  FreeAndNil(FHeaders);
  inherited;
end;

procedure TIdSoapMimeBase.ReadHeaders(AStream: TStream);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimeBase.ReadHeaders';
var
  LHeader : AnsiString;
  LFound : Boolean;
begin
  assert(Self.TestValid(TIdSoapMimeBase), ASSERT_LOCATION+': self is not valid');
  assert(Assigned(AStream), ASSERT_LOCATION+': Stream is not valid');
  LFound := false;
  repeat
    LHeader := ReadToValue(AStream, EOL);
    if LHeader <> '' then
      begin
      LFound := true;
      FHeaders.Add(string(LHeader));
      end
  until LFound and (LHeader = '');
end;

procedure TIdSoapMimeBase.WriteHeaders(AStream: TStream);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimeBase.WriteHeaders';
var
  i : integer;
begin
  assert(assigned(AStream), ASSERT_LOCATION+': stream is not valid');

  For i := 0 to FHeaders.Count - 1 do
    begin
    WriteString(AStream, ansiString(FHeaders[i])+EOL);
    end;
  WriteString(AStream, EOL);
end;

{ TIdSoapMimePart }

constructor TIdSoapMimePart.create;
begin
  inherited;
  FContent := TIdMemoryStream.create;
  FOwnsContent := true;
end;

destructor TIdSoapMimePart.destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimePart.destroy';
begin
  if FOwnsContent then
    begin
    FreeAndNil(FContent);
    end;
  inherited;
end;

procedure TIdSoapMimePart.DecodeContent;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimePart.DecodeContent';
var
  LCnt : String;
begin
  assert(self.TestValid(TIdSoapMimePart), ASSERT_LOCATION+': self is not valid');
  LCnt := FHeaders.Values[ID_SOAP_MIME_TRANSFERENCODING];

  // possible values (rfc 2045):
  // "7bit" / "8bit" / "binary" / "quoted-printable" / "base64"
  // and extendible with ietf-token / x-token

  // we only process base64. everything else is considered to be binary
  // (this is not an email processor). Where this is a problem, notify
  // the indysoap team *with an example*, and this will be extended
  if AnsiSameText(LCnt, 'base64') then
    begin
    Content := IdSoapBase64Decode(Content);
    end
  else
    begin
    // well, we leave the content unchanged
    end;
end;

procedure TIdSoapMimePart.ReadFromStream(AStream: TStream; ABoundary: AnsiString);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimePart.ReadFromStream';
var
  LBuffer : pansichar;
  LEnd : word;
  LComp0 : Pointer;
  LComp1 : Pointer;
  LCompLen : Word;
const
  BUF_LEN = 1024;
begin
  assert(self.TestValid(TIdSoapMimePart), ASSERT_LOCATION+': self is not valid');
  assert(Assigned(AStream), ASSERT_LOCATION+': stream is not valid');
  assert(ABoundary <> '', ASSERT_LOCATION+': Boundary is not valid');

  ReadHeaders(AStream);
  FId := FHeaders.Values[ID_SOAP_MIME_ID];
  if (FId <> '') and (FId[1] = '<') then
    delete(FId, 1, 1);
  if (FId <> '') and (FId[length(fId)] = '>') then
    delete(FId, length(FId), 1);

  // do this fast
  GetMem(LBuffer, BUF_LEN);
  try
    FillChar(LBuffer^, BUF_LEN, 0);
    LEnd := 0;
    LCompLen := length(ABoundary);
    LComp1 := pAnsichar(ABoundary);
    assert(LCompLen < BUF_LEN + 10, ASSERT_LOCATION+': Buffer length is not long enough ('+inttostr(BUF_LEN)+' / '+inttostr(LCompLen)+')');
    while true do
      begin
      if LEnd = BUF_LEN-1 then
        begin
        FContent.Write(LBuffer^, LEnd - LCompLen);
        move(LBuffer[LEnd - LCompLen], LBuffer[0], LCompLen);
        LEnd := LCompLen;
        FillChar(LBuffer[LEnd], BUF_LEN - LEnd, 0);
        end
      else
        begin
        AStream.Read(LBuffer[LEnd], 1);
        inc(LEnd);
        end;
      LComp0 := pointer(NativeUInt(LBuffer)+LEnd-LCompLen);
      if (LEnd >= LCompLen) and CompareMem(LComp0, LComp1, LCompLen) then
        begin
        FContent.Write(LBuffer^, LEnd - (LCompLen + 2 + 2)); // -2 for the EOL, +2 for the other EOL
        break;
        end;
      end;
  finally
    FreeMem(LBuffer);
  end;
  FContent.Position := 0;

  DecodeContent;
end;

procedure TIdSoapMimePart.SetContent(const AValue: TStream);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimePart.SetContent';
begin
  assert(self.TestValid(TIdSoapMimePart), ASSERT_LOCATION+': self is not valid');

  if FOwnsContent then
    begin
    FreeAndNil(FContent);
    end;
  FContent := AValue;
end;

procedure TIdSoapMimePart.WriteToStream(AStream: TStream);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimePart.WriteToStream';
var
  LOldPosition : integer;
  LTemp : AnsiString;
begin
  assert(self.TestValid(TIdSoapMimePart), ASSERT_LOCATION+': self is not valid');
  assert(Assigned(AStream), ASSERT_LOCATION+': stream is not valid');

  LOldPosition := FContent.Position;

  WriteHeaders(AStream);
  if FHeaders.Values[ID_SOAP_MIME_TRANSFERENCODING] = 'base64' then
    begin
    WriteString(AStream, IdSoapBase64EncodeAnsi(FContent, true)+EOL_WINDOWS);
    end
  else
    begin
    AStream.CopyFrom(FContent, (FContent.Size - FContent.Position)-2);
    if FContent.Size - FContent.Position >= 2 then
      LTemp := ReadBytes(FContent, 2)
    else
      LTemp := '';
    WriteString(AStream, LTemp);
    if LTemp <> EOL_WINDOWS then
      begin
      WriteString(AStream, EOL_WINDOWS);
      end;
    end;
  FContent.Position := LOldPosition;
end;

function TIdSoapMimePart.GetMediaType: String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimePart.GetMediaType';
begin
  assert(self.TestValid(TIdSoapMimePart), ASSERT_LOCATION+': self is not valid');
  result := FHeaders.Values[ID_SOAP_CONTENT_TYPE];
end;

function TIdSoapMimePart.GetTransferEncoding: String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimePart.GetTransferEncoding';
begin
  assert(self.TestValid(TIdSoapMimePart), ASSERT_LOCATION+': self is not valid');
  result := FHeaders.Values[ID_SOAP_MIME_TRANSFERENCODING];
end;

Function StringSplit(Const sValue, sDelimiter : String; Var sLeft, sRight: String) : Boolean;
Var
  iIndex : Integer;
  sA, sB : String;
Begin
  // Find the delimiter within the source string
  iIndex := Pos(sDelimiter, sValue);
  Result := iIndex <> 0;

  If Not Result Then
  Begin
    sA := sValue;
    sB := '';
  End
  Else
  Begin
    sA := Copy(sValue, 1, iIndex - 1);
    sB := Copy(sValue, iIndex + Length(sDelimiter), MaxInt);
  End;

  sLeft := sA;
  sRight := sB;
End;

function StartsWith(s, test : String):Boolean;
begin
  result := lowercase(copy(s, 1, length(test))) = lowercase(test);
end;

function TIdSoapMimePart.ParamName: String;
var
  s : String;
begin
  s := Headers.Values['Content-Disposition'];
  StringSplit(s, ';', s, result);
  if (s = 'form-data') and StartsWith(trim(result), 'name="') then
  begin
    result := copy(result, 8, $FFFF);
    result := copy(result, 1, pos('"', result)-1);
  end
  else
    result := '';
end;

procedure TIdSoapMimePart.SetMediaType(const AValue: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimePart.SetMediaType';
begin
  assert(self.TestValid(TIdSoapMimePart), ASSERT_LOCATION+': self is not valid');
  FHeaders.Values[ID_SOAP_CONTENT_TYPE] := AValue;
end;

procedure TIdSoapMimePart.SetTransferEncoding(const AValue: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimePart.SetTransferEncoding';
begin
  assert(self.TestValid(TIdSoapMimePart), ASSERT_LOCATION+': self is not valid');
  FHeaders.Values[ID_SOAP_MIME_TRANSFERENCODING] := AValue;
end;

procedure TIdSoapMimePart.SetId(const AValue: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimePart.SetId';
begin
  assert(self.TestValid(TIdSoapMimePart), ASSERT_LOCATION+': self is not valid');
  FId := AValue;
  FHeaders.Values[ID_SOAP_MIME_ID] := '<'+AValue+'>';
end;

function TIdSoapMimePart.GetContentDisposition: String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimePart.GetContentDisposition';
begin
  assert(self.TestValid(TIdSoapMimePart), ASSERT_LOCATION+': self is not valid');
  result := FHeaders.Values[ID_SOAP_CONTENT_DISPOSITION];
end;

procedure TIdSoapMimePart.SetContentDisposition(const sValue: String);
begin
  FHeaders.Values[ID_SOAP_CONTENT_DISPOSITION] := sValue;
end;

{ TIdSoapMimePartList }

function TIdSoapMimePartList.GetPartByIndex(i: integer): TIdSoapMimePart;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimePartList.GetPart';
begin
  assert((i >= 0) and (i < Count), ASSERT_LOCATION+': i is out of range ('+inttostr(i)+' of '+inttostr(Count)+')');
  result := Items[i] as TIdSoapMimePart;
end;

function TIdSoapMimePartList.GetPart(AName : String): TIdSoapMimePart;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimePartList.GetPart';
var
  i : integer;
  s : String;
begin
  if (AName <> '') and (AName[1] = '<') then
    System.delete(AName, 1, 1);
  if (AName <> '') and (AName[length(AName)] = '>') then
    System.delete(AName, length(AName), 1);

  result := nil;
  for i := 0 to Count - 1 do
    begin
    s := (Items[i] as TIdSoapMimePart).Id;
    if s = AName then
      begin
      result := Items[i] as TIdSoapMimePart;
      exit;
      end
    end;
  IdRequire(False, ASSERT_LOCATION+': Part "'+AName+'" not found in parts '+CommaList);
end;

function TIdSoapMimePartList.CommaList: String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimePartList.CommaList';
var
  i : integer;
begin
  result := '';
  for i := 0 to Count -1 do
    begin
    result := CommaAdd(result, (Items[i] as TIdSoapMimePart).Id);
    end;
end;

function TIdSoapMimePartList.AddPart(AId: String): TIdSoapMimePart;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimePartList.Add';
begin
  assert(assigned(self), ASSERT_LOCATION+': self is not valid');
  assert(AId <> '', ASSERT_LOCATION+': id is not valid');
  result := TIdSoapMimePart.create;
  result.Id := AId;
  add(result);
end;

{ TIdSoapMimeMessage }

function IdAnsiSameText(const S1, S2: ansistring): Boolean;
begin
  Result := AnsiCompareText(String(S1), String(S2)) = 0;
end;


procedure TIdSoapMimeMessage.AnalyseContentType(AContent: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimeMessage.AnalyseContentType';
var
  s, l, r, id : AnsiString;
begin
  // this parser is weak - and needs review
  SplitString(AnsiString(Trim(AContent)), ';',l, s);
  IdRequire(IdAnsiSameText(l, ID_SOAP_MULTIPART_RELATED) or IdAnsiSameText(l, ID_SOAP_MULTIPART_FORMDATA), ASSERT_LOCATION+': attempt to read content as Mime, but the content-Type is "'+String(l)+'", not "'+String(ID_SOAP_MULTIPART_RELATED)+'" or "'+String(ID_SOAP_MULTIPART_FORMDATA)+'" in header '+String(AContent));
  while s <> '' do
    begin
    SplitString(s, ';',l, s);
    SplitString(AnsiString(trim(String(l))), '=', l, r);
    idRequire(l <> '', ASSERT_LOCATION+': Unnamed part in Content_type header '+AContent);
    idRequire(r <> '', ASSERT_LOCATION+': Unvalued part in Content_type header '+AContent);
    if r[1] = '"' then
      begin
      delete(r, 1, 1);
      end;
    if r[length(r)] = '"' then
      begin
      delete(r, length(r), 1);
      end;
    if IdAnsiSameText(l, ID_SOAP_MIME_BOUNDARY) then
      begin
      FBoundary := r;
      end
    else if IdAnsiSameText(l, ID_SOAP_MIME_START) then
      begin
      id := r;
      if (id <> '') and (id[1] = '<') then
        delete(id, 1, 1);
      if (id <> '') and (id[length(id)] = '>') then
        delete(id, length(id), 1);
      FStart := string(id);
      end
    else if IdAnsiSameText(l, ID_SOAP_MIME_TYPE) then
      begin
      FMainType := String(r);
      end;
    end;
end;

constructor TIdSoapMimeMessage.create;
begin
  inherited create;
  FParts := TIdSoapMimePartList.create(true);
end;

destructor TIdSoapMimeMessage.destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimeMessage.destroy';
begin
  assert(self.TestValid(TIdSoapMimeMessage), ASSERT_LOCATION+': self is not valid');
  FreeAndNil(FParts);
  inherited;
end;

procedure TIdSoapMimeMessage.ReadFromStream(AStream: TStream);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimeMessage.ReadFromStream';
var
  LHeader : String;
begin
  assert(self.TestValid(TIdSoapMimeMessage), ASSERT_LOCATION+': self is not valid');
  ReadHeaders(AStream);

  LHeader := FHeaders.Values[ID_SOAP_CONTENT_TYPE];
  IdRequire(LHeader <> '', ASSERT_LOCATION+': '+ID_SOAP_CONTENT_TYPE+' header not found in '+FHeaders.CommaText);
  ReadFromStream(AStream, LHeader);
end;

function TIdSoapMimeMessage.GetMainPart: TIdSoapMimePart;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimeMessage.GetMainPart';
begin
  assert(self.TestValid(TIdSoapMimeMessage), ASSERT_LOCATION+': self is not valid');
  IdRequire(FStart <> '', ASSERT_LOCATION+': Start header not valid');
  result := FParts[FStart];
end;

function TIdSoapMimeMessage.getparam(name: String): TIdSoapMimePart;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Parts.Count - 1 do
    if Parts.PartByIndex[i].ParamName = name then
    begin
      result := Parts.PartByIndex[i];
      exit;
    end;
end;

function TIdSoapMimeMessage.hasParam(name: String): Boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to Parts.Count - 1 do
    result := result or (Parts.PartByIndex[i].ParamName = name);
end;

procedure TIdSoapMimeMessage.ReadFromStream(AStream: TStream; AContentType: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimeMessage.ReadFromStream';
var
  LTemp : AnsiString;
  LPart : TIdSoapMimePart;
begin
  assert(self.TestValid(TIdSoapMimeMessage), ASSERT_LOCATION+': self is not valid');
  IdRequire(AContentType <> '', ASSERT_LOCATION+': Content-Type header not valid');
  AnalyseContentType(AContentType);

  LTemp := ReadToValue(AStream, FBoundary);
  // that was getting going. usually LTemp would be empty, but we will throw it away
  repeat
    LTemp := ReadBytes(AStream, 2);
    if LTemp = EOL then
      begin
      LPart := TIdSoapMimePart.create;
      try
        LPart.ReadFromStream(AStream, FBoundary);
      except
        FreeAndNil(LPart);
        raise;
      end;
      LPart.FContent.Position := 0;
      FParts.Add(LPart);
      end
  until LTemp = '--';
end;

function TIdSoapMimeMessage.GetContentTypeHeader: String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimeMessage.GetContentTypeHeader';
begin
  assert(self.TestValid(TIdSoapMimeMessage), ASSERT_LOCATION+': self is not valid');

  result := String(ID_SOAP_MULTIPART_RELATED)+'; type="application/xop+xml"; '+String(ID_SOAP_MIME_START)+'="'+FStart+'"; start-info="application/soap+xml"; '+String(ID_SOAP_MIME_BOUNDARY)+'="'+String(FBoundary)+'"; action="urn:ihe:iti:2007:ProvideAndRegisterDocumentSet-b"';
  if FMainType <> '' then
    begin
    result := result + '; '+String(ID_SOAP_MIME_TYPE)+'="'+FMainType+'"';
    end;
// oracle   Content-Type: multipart/related; type="application/xop+xml"; start="<rootpart@soapui.org>"; start-info="application/soap+xml"; action="ProvideAndRegisterDocumentSet-b"; boundary="----=_Part_2_2098391526.1311207545005"

end;

procedure TIdSoapMimeMessage.Validate;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimeMessage.Validate';
var
  i, j : integer;
  LFound : boolean;
begin
  assert(self.TestValid(TIdSoapMimeMessage), ASSERT_LOCATION+': self is not valid');
  IdRequire(FBoundary <> '', ASSERT_LOCATION+': Boundary is not valid');
  IdRequire(FStart <> '', ASSERT_LOCATION+': Start is not valid');
  LFound := false;
  for i := 0 to FParts.Count - 1 do
    begin
    IdRequire(FParts.PartByIndex[i].TestValid(TIdSoapMimePart), ASSERT_LOCATION+': Part['+inttostr(i)+'] is not valid');
    IdRequire(FParts.PartByIndex[i].Id <> '', ASSERT_LOCATION+': Part['+inttostr(i)+'].Id is not valid');
    LFound := LFound or (FParts.PartByIndex[i].Id = FStart);
    for j := 0 to i - 1 do
      begin
      IdRequire(FParts.PartByIndex[i].Id <> FParts.PartByIndex[j].Id, ASSERT_LOCATION+': Part['+inttostr(i)+'].Id is a duplicate of Part['+inttostr(j)+'].Id ("'+FParts.PartByIndex[i].Id+'")');
      end;
    end;
  IdRequire(LFound, ASSERT_LOCATION+': The Start Part "'+FStart+'" was not found in the parts '+FParts.CommaList);
end;

procedure TIdSoapMimeMessage.WriteToStream(AStream: TStream; AHeaders: boolean);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMimeMessage.WriteToStream';
var
  i : integer;
begin
  assert(self.TestValid(TIdSoapMimeMessage), ASSERT_LOCATION+': self is not valid');
  assert(Assigned(AStream), ASSERT_LOCATION+': stream is not valid');
  Validate;

  if AHeaders then
    begin
    WriteHeaders(AStream);
    end;
  for i := 0 to FParts.Count - 1 do
    begin
    WriteString(AStream, '--'+FBoundary+EOL);
    FParts.PartByIndex[i].WriteToStream(AStream);
    end;
  WriteString(AStream, '--'+FBoundary+'--');
end;

end.

