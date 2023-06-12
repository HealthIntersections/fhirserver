unit fsl_web_stream;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

Interface


Uses
  {$IFDEF WINDOWS} Windows, ActiveX, {$ENDIF}
  {$IFDEF LINUX} unixtype, baseunix, unix, {$ENDIF}
  {$IFNDEF FPC} AnsiStrings, {$ENDIF}
  SysUtils,Classes, RTLConsts, ZLib,
  IdHeaderList, IdGlobal, IdGlobalProtocols,
  fsl_fpc, fsl_base, fsl_collections, fsl_utilities, fsl_stream;


type
  TMimeBase = class (TFslObject)
  private
    FHeaders: TIdHeaderList;
    procedure ReadHeaders(AStream : TStream);
    procedure WriteHeaders(AStream : TStream);
    function ReadBytes(AStream: TStream; AByteCount: Integer): AnsiString;
    function ReadToValue(AStream: TStream; AValue: AnsiString): AnsiString;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Headers : TIdHeaderList read FHeaders;
  end;

  TMimePart = class (TMimeBase)
  private
    FContent: TFslBuffer;
    FId : string;
    procedure SetContent(const AValue: TFslBuffer);
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
    constructor Create; override;
    destructor Destroy; override;
    function Link : TMimePart; overload;

    property Content : TFslBuffer read FContent write SetContent;
    property Id : String read FId write SetId;
    property MediaType : String read GetMediaType write SetMediaType;
    property ContentDisposition : String read GetContentDisposition write SetContentDisposition;
    property TransferEncoding : String read GetTransferEncoding write SetTransferEncoding;

    function ParamName : String;
    function FileName : String;
  end;

  TMimeMessage = class (TMimeBase)
  private
    FParts: TFslList<TMimePart>;
    FBoundary : Ansistring;
    FStart : String;
    FMainType : String;
    procedure AnalyseContentType(AContent : String);
    function GetMainPart: TMimePart;
    procedure Validate;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TMimeMessage; overload;
    property Parts : TFslList<TMimePart> read FParts;
    function AddPart(id: String) : TMimePart;
    property MainPart : TMimePart read GetMainPart;
    property Boundary : ansistring read FBoundary write FBoundary;
    property Start : String read FStart write FStart;
    property MainType : String read FMainType write FMainType;

    // for multi-part forms
    function getparam(name : String) : TMimePart;
    function hasParam(name : String) : Boolean;

    procedure ReadFromStream(AStream : TStream); overload;
    procedure ReadFromStream(AStream : TStream; AContentType : String); overload; // headers are not part of the stream

    function GetContentTypeHeader : String;
    procedure WriteToStream(AStream : TStream; AHeaders : boolean);
  end;


implementation

const
  CR = #13;
  LF = #10;
  EOL_WINDOWS = CR + LF;
  EOL_PLATFORM = EOL_WINDOWS;
  MIME_TRANSFERENCODING = 'Content-Transfer-Encoding';
  MIME_DEFAULT_START = 'uuid:{FF461456-FE30-4933-9AF6-F8EB226E1BF7}';
  MIME_DEFAULT_BOUNDARY = 'MIME_boundary';
  MIME_ID = 'Content-ID';
  CONTENT_TYPE = 'Content-Type';
  CONTENT_DISPOSITION = 'Content-Disposition';
  MULTIPART_RELATED : AnsiString = 'multipart/related';
  MULTIPART_FORMDATA : AnsiString = 'multipart/form-data';
  MIME_BOUNDARY : AnsiString = 'boundary';
  MIME_START : AnsiString = 'start';
  MIME_TYPE : AnsiString = 'type';


procedure WriteString(AStream : TStream; Const AStr : AnsiString);
begin
  If AStr <> '' then
    begin
    AStream.Write(AStr[1], length(AStr));
    end;
end;

{ TMimeBase }

constructor TMimeBase.create;
begin
  inherited;
  FHeaders := TIdHeaderList.create(QuotePlain);
end;

destructor TMimeBase.destroy;
begin
  FreeAndNil(FHeaders);
  inherited;
end;

function TMimeBase.ReadToValue(AStream : TStream; AValue : AnsiString):AnsiString;
var
  c : AnsiChar;
begin
  result := '';
  while copy(result, length(result)-length(AValue)+1, length(AValue)) <> AValue do
    begin
    CheckCondition(AStream.Size - AStream.Position <> 0, 'ReadToValue', 'Premature termination of stream looking for value "'+string(AValue)+'"');
    AStream.Read(c, 1);
    result := result + c;
    end;
  delete(result, length(result)-length(AValue)+1, length(AValue));
end;

function TMimeBase.ReadBytes(AStream : TStream; AByteCount : Integer):AnsiString;
begin
  CheckCondition(AStream.Size - AStream.Position >= AByteCount, 'ReadBytes', 'Premature termination of stream reading "'+inttostr(AByteCount)+'" bytes');
  SetLength(result, AByteCount);
  if AByteCount > 0 then
    begin
    AStream.Read(result[1], AByteCount);
    end;
end;


procedure TMimeBase.ReadHeaders(AStream: TStream);
var
  LHeader : AnsiString;
  LFound : Boolean;
begin
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

procedure TMimeBase.WriteHeaders(AStream: TStream);
var
  i : integer;
begin

  For i := 0 to FHeaders.Count - 1 do
    begin
    WriteString(AStream, ansiString(FHeaders[i])+EOL);
    end;
  WriteString(AStream, EOL);
end;

{ TMimePart }

constructor TMimePart.create;
begin
  inherited;
  FContent := TFslBuffer.create;
end;

destructor TMimePart.destroy;
begin
  FContent.Free;
  inherited;
end;

function TMimePart.FileName: String;
var
  s : String;
begin
  s := Headers.Values['Content-Disposition'];
  StringSplit(s, ';', s, result);
  if (s = 'form-data') and result.Contains('filename="') then
  begin
    result := result.Substring(result.IndexOf('filename="')+10);
    result := copy(result, 1, pos('"', result)-1);
  end
  else
    result := '';

end;

procedure TMimePart.DecodeContent;
var
  LCnt : String;
begin
  LCnt := FHeaders.Values[MIME_TRANSFERENCODING];

  // possible values (rfc 2045):
  // "7bit" / "8bit" / "binary" / "quoted-printable" / "base64"
  // and extendible with ietf-token / x-token

  // we only process base64. everything else is considered to be binary
  // (this is not an email processor). Where this is a problem, notify
  // the indysoap team *with an example*, and this will be extended
  if AnsiSameText(LCnt, 'base64') then
    begin
    raise ETodo.create('TMimePart.DecodeContent');
//    Content := Base64Decode(Content);
    end
  else
    begin
    // well, we leave the content unchanged
    end;
end;

procedure TMimePart.ReadFromStream(AStream: TStream; ABoundary: AnsiString);
var
  LBuffer : pansichar;
  LEnd : word;
  LComp0 : Pointer;
  LComp1 : Pointer;
  LCompLen : Word;
  offset : integer;
  b : TBytes;
const
  BUF_LEN = 1024;
begin

  ReadHeaders(AStream);
  FId := FHeaders.Values[MIME_ID];
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
    while true do
      begin
      if LEnd = BUF_LEN-1 then
        begin
        offset := length(b);
        SetLength(b, offset + LEnd - LCompLen);
        move(LBuffer^, b[offset], LEnd - LCompLen);
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
        offset := length(b);
        SetLength(b, offset + LEnd - (LCompLen + 2 + 2));// -2 for the EOL, +2 for the other EOL
        if (length(b) > offset) then
          move(LBuffer^, b[offset], LEnd - (LCompLen + 2 + 2));
        FContent.AsBytes := b;
        break;
        end;
      end;
  finally
    FreeMem(LBuffer);
  end;
  DecodeContent;
end;

procedure TMimePart.SetContent(const AValue: TFslBuffer);
begin
  FContent.Free;
  FContent := AValue;
end;

procedure TMimePart.WriteToStream(AStream: TStream);
var
  LTemp : AnsiString;
begin

  WriteHeaders(AStream);
  if FHeaders.Values[MIME_TRANSFERENCODING] = 'base64' then
    begin
    raise ETodo.create('TMimePart.WriteToStream#1');
//    WriteString(AStream, Base64EncodeAnsi(FContent, true)+EOL_WINDOWS);
    end
  else
    begin
    raise ETodo.create('TMimePart.WriteToStream#2');

    //AStream.CopyFrom(FContent, (FContent.Size - FContent.Position)-2);
//    if FContent.Size - FContent.Position >= 2 then
//      LTemp := ReadBytes(FContent, 2)
//    else
//      LTemp := '';
    WriteString(AStream, LTemp);
    if LTemp <> EOL_WINDOWS then
      begin
      WriteString(AStream, EOL_WINDOWS);
      end;
    end;
end;

function TMimePart.GetMediaType: String;
begin
  result := FHeaders.Values[CONTENT_TYPE];
end;

function TMimePart.GetTransferEncoding: String;
begin
  result := FHeaders.Values[MIME_TRANSFERENCODING];
end;

function TMimePart.Link: TMimePart;
begin
  result := TMimePart(inherited Link);
end;

function TMimePart.ParamName: String;
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

procedure TMimePart.SetMediaType(const AValue: String);
begin
  FHeaders.Values[CONTENT_TYPE] := AValue;
end;

procedure TMimePart.SetTransferEncoding(const AValue: String);
begin
  FHeaders.Values[MIME_TRANSFERENCODING] := AValue;
end;

procedure TMimePart.SetId(const AValue: String);
begin
  FId := AValue;
  FHeaders.Values[MIME_ID] := '<'+AValue+'>';
end;

function TMimePart.GetContentDisposition: String;
begin
  result := FHeaders.Values[CONTENT_DISPOSITION];
end;

procedure TMimePart.SetContentDisposition(const sValue: String);
begin
  FHeaders.Values[CONTENT_DISPOSITION] := sValue;
end;

(*{ TMimePartList }

function TMimePart.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FContent.sizeInBytes(magic));
  inc(result, (FId.length * sizeof(char)) + 12);
end;

function TMimePartList.GetPartByIndex(i: integer): TMimePart;
begin
  result := Items[i] as TMimePart;
end;

function TMimePartList.GetPart(AName : String): TMimePart;
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
    s := (Items[i] as TMimePart).Id;
    if s = AName then
      begin
      result := Items[i] as TMimePart;
      exit;
      end
    end;
  Condition(False, 'Part "'+AName+'" not found in parts '+CommaList);
end;

function TMimePartList.CommaList: String;
var
  i : integer;
begin
  result := '';
  for i := 0 to Count -1 do
    begin
    result := CommaAdd(result, (Items[i] as TMimePart).Id);
    end;
end;

function TMimePartList.AddPart(AId: String): TMimePart;
begin
  result := TMimePart.create;
  result.Id := AId;
  add(result);
end;
  *)

{ TMimeMessage }

function IdAnsiSameText(const S1, S2: ansistring): Boolean;
begin
  Result := AnsiCompareText(String(S1), String(S2)) = 0;
end;


function TMimeMessage.AddPart(id: String): TMimePart;
begin
  result := TMimePart.create;
  FParts.Add(result);
  result.Id := id;
end;

procedure TMimeMessage.AnalyseContentType(AContent: String);
var
  s, l, r, id : AnsiString;
begin
  // this parser is weak - and needs review
  StringSplitAnsi(AnsiString(Trim(AContent)), ';', l, s);
  CheckCondition(IdAnsiSameText(l, MULTIPART_RELATED) or IdAnsiSameText(l, MULTIPART_FORMDATA), 'AnalyseContentType', 'attempt to read content as Mime, but the content-Type is "'+String(l)+'", not "'+String(MULTIPART_RELATED)+'" or "'+String(MULTIPART_FORMDATA)+'" in header '+String(AContent));
  while s <> '' do
    begin
    StringSplitAnsi(s, ';',l, s);
    StringSplitAnsi(AnsiString(trim(String(l))), '=', l, r);
    CheckCondition(l <> '', 'AnalyseContentType', 'Unnamed part in Content_type header '+AContent);
    CheckCondition(r <> '', 'AnalyseContentType', 'Unvalued part in Content_type header '+AContent);
    if r[1] = '"' then
      begin
      delete(r, 1, 1);
      end;
    if r[length(r)] = '"' then
      begin
      delete(r, length(r), 1);
      end;
    if AnsiSameText(string(l), string(MIME_BOUNDARY)) then
      begin
      FBoundary := r;
      end
    else if IdAnsiSameText(l, MIME_START) then
      begin
      id := r;
      if (id <> '') and (id[1] = '<') then
        delete(id, 1, 1);
      if (id <> '') and (id[length(id)] = '>') then
        delete(id, length(id), 1);
      FStart := string(id);
      end
    else if IdAnsiSameText(l, MIME_TYPE) then
      begin
      FMainType := String(r);
      end;
    end;
end;

constructor TMimeMessage.create;
begin
  inherited create;
  FParts := TFslList<TMimePart>.create;
end;

destructor TMimeMessage.destroy;
begin
  FParts.Free;
  inherited;
end;

procedure TMimeMessage.ReadFromStream(AStream: TStream);
var
  LHeader : String;
begin
  ReadHeaders(AStream);

  LHeader := FHeaders.Values[CONTENT_TYPE];
  CheckCondition(LHeader <> '', 'ReadFromStream', ''+CONTENT_TYPE+' header not found in '+FHeaders.CommaText);
  ReadFromStream(AStream, LHeader);
end;

function TMimeMessage.GetMainPart: TMimePart;
begin
  CheckCondition(FStart <> '', 'GetMainPart', 'Start header not valid');
  result := getparam(FStart);
end;

function TMimeMessage.getparam(name: String): TMimePart;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Parts.Count - 1 do
    if Parts[i].ParamName = name then
    begin
      result := Parts[i];
      exit;
    end;
end;

function TMimeMessage.hasParam(name: String): Boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to Parts.Count - 1 do
    result := result or (Parts[i].ParamName = name);
end;

function TMimeMessage.Link: TMimeMessage;
begin
  result := TMimeMessage(inherited Link);
end;

procedure TMimeMessage.ReadFromStream(AStream: TStream; AContentType: String);
var
  LTemp : AnsiString;
  LPart : TMimePart;
begin
  CheckCondition(AContentType <> '', 'ReadFromStream', 'Content-Type header not valid');
  AnalyseContentType(AContentType);

  LTemp := ReadToValue(AStream, FBoundary);
  // that was getting going. usually LTemp would be empty, but we will throw it away
  repeat
    LTemp := ReadBytes(AStream, 2);
    if LTemp = EOL then
      begin
      LPart := TMimePart.create;
      try
        LPart.ReadFromStream(AStream, FBoundary);
      except
        FreeAndNil(LPart);
        raise;
      end;
      FParts.Add(LPart);
      end
  until LTemp = '--';
end;

function TMimeMessage.GetContentTypeHeader: String;
begin

  result := String(MULTIPART_RELATED)+'; type="application/xop+xml"; '+String(MIME_START)+'="'+FStart+'"; start-info="application/soap+xml"; '+String(MIME_BOUNDARY)+'="'+String(FBoundary)+'"; action="urn:ihe:iti:2007:ProvideAndRegisterDocumentSet-b"';
  if FMainType <> '' then
    begin
    result := result + '; '+String(MIME_TYPE)+'="'+FMainType+'"';
    end;
// oracle   Content-Type: multipart/related; type="application/xop+xml"; start="<rootpart@soapui.org>"; start-info="application/soap+xml"; action="ProvideAndRegisterDocumentSet-b"; boundary="----=_Part_2_2098391526.1311207545005"

end;

procedure TMimeMessage.Validate;
var
  i, j : integer;
  LFound : boolean;
begin
  CheckCondition(FBoundary <> '', 'Validate', 'Boundary is not valid');
  CheckCondition(FStart <> '', 'Validate', 'Start is not valid');
  LFound := false;
  for i := 0 to FParts.Count - 1 do
    begin
    CheckCondition(FParts[i].Id <> '', 'Validate', 'Part['+inttostr(i)+'].Id is not valid');
    LFound := LFound or (FParts[i].Id = FStart);
    for j := 0 to i - 1 do
      begin
      CheckCondition(FParts[i].Id <> FParts[j].Id, 'Validate', 'Part['+inttostr(i)+'].Id is a duplicate of Part['+inttostr(j)+'].Id ("'+FParts[i].Id+'")');
      end;
    end;
  CheckCondition(LFound, 'Validate', 'The Start Part "'+FStart+'" was not found in the part list');
end;

procedure TMimeMessage.WriteToStream(AStream: TStream; AHeaders: boolean);
var
  i : integer;
begin
  Validate;

  if AHeaders then
    begin
    WriteHeaders(AStream);
    end;
  for i := 0 to FParts.Count - 1 do
    begin
    WriteString(AStream, '--'+FBoundary+EOL);
    FParts[i].WriteToStream(AStream);
    end;
  WriteString(AStream, '--'+FBoundary+'--');
end;


end.

