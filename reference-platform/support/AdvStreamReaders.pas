unit AdvStreamReaders;

{
Copyright (c) 2001-2013, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
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

interface

uses
  Sysutils, RTLConsts, AdvObjects, AdvStreams, MathSupport;

Type
  TAdvTextReader = class (TAdvObject)
  public
    procedure Close; virtual; abstract;
    function Peek: Integer; virtual; abstract;
    function Read: Integer; overload; virtual; abstract;
    function Read(const Buffer: TCharArray; Index, Count: Integer): Integer; overload; virtual; abstract;
    function ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer; virtual; abstract;
    function ReadLine: string; virtual; abstract;
    function ReadToEnd: string; virtual; abstract;

    Function ReadString(Var s : String; iLength : Integer) : Integer;
  end;

  TAdvStringReader = class(TAdvTextReader)
  private
    FContent : String;
    FCursor : integer;
  public
    constructor Create(content : String);
    function Peek: Integer; override;
    function Read: Integer; overload; override;
    function Read(const Buffer: TCharArray; Index, Count: Integer): Integer; overload; override;
    function ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer; override;
    function ReadLine: string; override;
    function ReadToEnd: string; override;
    procedure Close; override;
  end;

  TAdvStreamReader = class(TAdvTextReader)
  private
    FBufferedData: TCharArray;
    FBufferStart : Integer;
    FBufferEnd : Integer;
    FBufferSize: Integer;
    FDetectBOM: Boolean;
    FNoDataInStream: Boolean;
    FSkipPreamble: Boolean;
    FStream: TAdvStream;
    FCursor : Integer;
    FEncoding: TEncoding;
    FCheckEncoding : boolean;
    FClosed : boolean;
    function DetectBOM(var Encoding: TEncoding; Buffer: TBytes): Integer;
    function SkipPreamble(Encoding: TEncoding; Buffer: TBytes): Integer;
    procedure FillBuffer(var Encoding: TEncoding);
    function GetEndOfStream: Boolean;
  protected
    Property Stream : TAdvStream read FStream;
  public
    constructor Create(aStream: TAdvStream); overload;
    constructor Create(aStream: TAdvStream; DetectBOM: Boolean); overload;
    constructor Create(const Filename: string); overload;
    constructor Create(const Filename: string; DetectBOM: Boolean); overload;
    constructor Create(aStream: TAdvStream; Encoding: TEncoding; DetectBOM: Boolean = False; BufferSize: Integer = 0); overload;
    constructor Create(const Filename: string; Encoding: TEncoding; DetectBOM: Boolean = False; BufferSize: Integer = 0); overload;
    destructor Destroy; override;
    procedure Close; override;
    procedure DiscardBufferedData;
//    procedure OwnStream; inline;
    function Peek: Integer; override;
    function Read: Integer; overload; override;
    function Read(const Buffer: TCharArray; Index, Count: Integer): Integer; overload; override;
    function ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer; override;
    function ReadLine: string; override;
    function ReadToEnd: string; override;
    property BaseStream: TAdvStream read FStream;
    property CurrentEncoding: TEncoding read FEncoding;
    property EndOfStream: Boolean read GetEndOfStream;
  end;

implementation

Uses
  AdvFiles;

{ TAdvStreamReader }

constructor TAdvStreamReader.Create(aStream: TAdvStream);
begin
  Create(aStream, TEncoding.UTF8, True);
end;

constructor TAdvStreamReader.Create(aStream: TAdvStream; DetectBOM: Boolean);
begin
  Create(aStream, TEncoding.UTF8, DetectBOM);
end;

constructor TAdvStreamReader.Create(aStream: TAdvStream; Encoding: TEncoding; DetectBOM: Boolean = False; BufferSize: Integer = 0);
begin
  Create;

  if not Assigned(aStream) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Stream']); // DO NOT LOCALIZE
  if not Assigned(Encoding) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Encoding']); // DO NOT LOCALIZE

  FBufferSize := BufferSize;
  if FBufferSize = 0 then
    FBufferSize := aStream.Readable;
  if FBufferSize = 0 then
    FBufferSize := 128;
  SetLength(FBufferedData, BufferSize);
  FBufferEnd := 0;
  FBufferStart := 0;
  FClosed := false;

  FEncoding := Encoding;
  FNoDataInStream := False;
  FStream := aStream;
  FDetectBOM := DetectBOM;
  FSkipPreamble := not FDetectBOM;
  FCursor := 0;
end;

constructor TAdvStreamReader.Create(const Filename: string; Encoding: TEncoding; DetectBOM: Boolean = False; BufferSize: Integer = 0);
var
  oFile : TAdvFile;
begin
  oFile := TAdvFile.Create(FileName, fmOpenRead);
  Try
    Create(oFile.Link, Encoding, DetectBOM, BufferSize);
  Finally
    oFile.Free;
  End;
end;

constructor TAdvStreamReader.Create(const Filename: string; DetectBOM: Boolean);
begin
  Create(Filename, TEncoding.UTF8, DetectBOM);
end;

constructor TAdvStreamReader.Create(const Filename: string);
begin
  Create(Filename, TEncoding.UTF8, true);
end;

destructor TAdvStreamReader.Destroy;
begin
  Close;
  inherited;
end;

procedure TAdvStreamReader.Close;
begin
  FStream.Free;
  FStream := nil;

  DiscardBufferedData;
  FClosed := true;
end;

procedure TAdvStreamReader.DiscardBufferedData;
begin
  if not FClosed then
  begin
    FBufferEnd := 0;
    FBufferStart := 0;
    FNoDataInStream := False;
  end;
end;

function TAdvStreamReader.DetectBOM(var Encoding: TEncoding; Buffer: TBytes): Integer;
var
  LEncoding: TEncoding;
begin
  // try to automatically detect the buffer encoding
  LEncoding := nil;
  Result := TEncoding.GetBufferEncoding(Buffer, LEncoding);

  // detected encoding points to Default and param Encoding requests some other
  // type of Encoding; set the Encoding param to UTF8 as it can also read ANSI (Default)
  if (LEncoding = TEncoding.Default) and (Encoding <> TEncoding.Default) then
    Encoding := TEncoding.UTF8
  else
    Encoding := LEncoding;

  FDetectBOM := False;
end;

procedure TAdvStreamReader.FillBuffer(var Encoding: TEncoding);
const
  BufferPadding = 4;
var
  LString: string;
  LBuffer: TBytes;
  BytesRead: Integer;
  StartIndex: Integer;
  ByteBufLen: Integer;
  ok : boolean;
  tries : integer;
begin
  SetLength(LBuffer, FBufferSize + BufferPadding);

  // Read data from stream
  BytesRead := IntegerMin(FBufferSize, FStream.Readable);
  FStream.Read(LBuffer[0], BytesRead);
  inc(FCursor, BytesRead);
  FNoDataInStream := FStream.Readable = 0;

  // Check for byte order mark and calc start index for character data
  if FDetectBOM then
    StartIndex := DetectBOM(Encoding, LBuffer)
  else if FSkipPreamble then
    StartIndex := SkipPreamble(Encoding, LBuffer)
  else
    StartIndex := 0;

  // Convert to string and calc byte count for the string
  ByteBufLen := BytesRead - StartIndex;
  ok := false;
  tries := 0;
  repeat
    try
      if FCheckEncoding and (FEncoding.GetCharCount(LBuffer, StartIndex, ByteBufLen) = 0) then
      begin
        SetLength(LBuffer, Length(LBuffer) + BufferPadding);
        FStream.Read(LBuffer[ByteBufLen], 1);
        inc(ByteBufLen);
        inc(tries);
      end;

      LString := FEncoding.GetString(LBuffer, StartIndex, ByteBufLen);
      ok := true;
    except
      if FCheckEncoding and (tries > FEncoding.GetMaxByteCount(1)) then
        raise
      else
        FCheckEncoding := true;
    end;
  until ok;

  if (Length(LString) > 0) then
  begin
    // Add string to character data buffer
    if (FBufferStart > 0) and (FBufferEnd = FBufferStart) then
    begin
      FBufferStart := 0;
      FBufferEnd := 0;
    end;
    if Length(FBufferedData) < FBufferEnd + length(LString) then
      SetLength(FBufferedData, Length(FBufferedData) + length(LString) * 2);

    Move(LString[1], FBufferedData[FBufferEnd], length(LString) * SizeOf(Char));
    inc(FBufferEnd, length(LString));
  end;
end;

function TAdvStreamReader.GetEndOfStream: Boolean;
begin
  if not FNoDataInStream and (not FClosed) and (FBufferEnd <= FBufferStart) then
    FillBuffer(FEncoding);
  Result := FNoDataInStream and ((FClosed) or (FBufferEnd = FBufferStart));
end;

function TAdvStreamReader.Peek: Integer;
begin
  Result := -1;
  if (not FClosed) and (not EndOfStream) then
  begin
    if FBufferEnd < 1 + FBufferStart  then
      FillBuffer(FEncoding);
    Result := Integer(FBufferedData[FBufferStart]);
  end;
end;

function TAdvStreamReader.Read(const Buffer: TCharArray; Index, Count: Integer): Integer;
begin
  Result := -1;
  if (not FClosed) and (not EndOfStream) then
  begin
    while (FBufferEnd < Count + FBufferStart) and (not EndOfStream) and (not FNoDataInStream) do
      FillBuffer(FEncoding);

    if FBufferEnd > Count + FBufferStart then
      Result := Count
    else
      Result := FBufferEnd - FBufferStart;

    move(FBufferedData[FBufferStart], buffer[0], result * Sizeof(char));
    inc(FBufferStart, result);
  end;
end;

function TAdvStreamReader.ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer;
begin
  Result := Read(Buffer, Index, Count);
end;

function TAdvStreamReader.Read: Integer;
begin
  Result := -1;
  if (not FClosed) and (not EndOfStream) then
  begin
    if FBufferEnd < 1 + FBufferStart then
      FillBuffer(FEncoding);
    Result := Integer(FBufferedData[FBufferStart]);
    inc(FBufferStart);
  end;
end;

function TAdvStreamReader.ReadLine: string;
{var
  NewLineIndex: Integer;
  PostNewLineIndex: Integer;}
begin
  raise Exception.Create('This needs debugging for buffer changes');
{  Result := '';
  if FClosed then
    Exit;
  NewLineIndex := 0;
  PostNewLineIndex := 0;

  while True do
  begin
    if (NewLineIndex + 2 > FBufferEnd + FBufferStart) and (not FNoDataInStream) then
      FillBuffer(FEncoding);

    if NewLineIndex >= FBufferEnd + FBufferStart then
    begin
      if FNoDataInStream then
      begin
        PostNewLineIndex := NewLineIndex;
        Break;
      end
      else
      begin
        FillBuffer(FEncoding);
        if FBufferEnd = FBufferStart then
          Break;
      end;
    end;
    if FBufferedData[NewLineIndex + FBufferStart] = #10 then
    begin
      PostNewLineIndex := NewLineIndex + 1;
      Break;
    end
    else
    if (FBufferedData[NewLineIndex + FBufferStart] = #13) and (NewLineIndex + 1 < FBufferEnd + FBufferStart) and (FBufferedData[NewLineIndex + 1] = #10) then
    begin
      PostNewLineIndex := NewLineIndex + 2;
      Break;
    end
    else
    if FBufferedData[NewLineIndex + FBufferStart] = #13 then
    begin
      PostNewLineIndex := NewLineIndex + 1;
      Break;
    end;

    Inc(NewLineIndex);
  end;

  Result := FBufferedData.ToString.Substring(FBufferStart);
  SetLength(Result, NewLineIndex);
  inc(FBufferStart, PostNewLineIndex);}
end;

function TAdvStreamReader.ReadToEnd: string;
begin
  raise Exception.Create('This needs debugging for FBufferStart');
  Result := '';
  if (not FClosed) and (not EndOfStream) then
  begin
    repeat
      FillBuffer(FEncoding);
    until FNoDataInStream;
    SetLength(result, FBufferEnd - FBufferStart);
    Move(FBufferedData[FBufferStart], result[1], length(result) * Sizeof(Char));
    FBufferEnd := 0;
    FBufferStart := 0;
  end;
end;

function TAdvStreamReader.SkipPreamble(Encoding: TEncoding; Buffer: TBytes): Integer;
var
  I: Integer;
  LPreamble: TBytes;
  BOMPresent: Boolean;
begin
  Result := 0;
  LPreamble := Encoding.GetPreamble;
  if (Length(LPreamble) > 0) then
  begin
    if Length(Buffer) >= Length(LPreamble) then
    begin
      BOMPresent := True;
      for I := 0 to Length(LPreamble) - 1 do
        if LPreamble[I] <> Buffer[I] then
        begin
          BOMPresent := False;
          Break;
        end;
      if BOMPresent then
        Result := Length(LPreamble);
    end;
  end;
  FSkipPreamble := False;
end;


{ TAdvTextReader }

function TAdvTextReader.ReadString(var s: String; iLength: Integer): Integer;
var
  oBuffer : TCharArray;
begin
  SetLength(oBuffer, iLength);
  result := ReadBlock(oBuffer, 0, iLength);
  SetString(s, pchar(oBuffer), result);
end;

{ TAdvStringReader }

procedure TAdvStringReader.Close;
begin
 // nothing
end;

constructor TAdvStringReader.Create(content: String);
begin
  inherited Create;
  FContent := content;
  FCursor := 1;
end;

function TAdvStringReader.Peek: Integer;
begin
  if FCursor > FContent.Length then
    result := -1
  else
    result := ord(FContent[FCursor]);
end;

function TAdvStringReader.Read: Integer;
begin
  if FCursor > FContent.Length then
    result := -1
  else
  begin
    result := ord(FContent[FCursor]);
    inc(FCursor);
  end;
end;

function TAdvStringReader.Read(const Buffer: TCharArray; Index, Count: Integer): Integer;
begin
  raise Exception.Create('Not done yet');
end;

function TAdvStringReader.ReadBlock(const Buffer: TCharArray; Index, Count: Integer): Integer;
begin
  raise Exception.Create('Not done yet');
end;

function TAdvStringReader.ReadLine: string;
begin
  raise Exception.Create('Not done yet');
end;

function TAdvStringReader.ReadToEnd: string;
begin
  raise Exception.Create('Not done yet');
end;

end.
