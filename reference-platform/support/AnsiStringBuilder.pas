Unit AnsiStringBuilder;

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

Interface


Uses
  Classes,
  SysUtils,
  AdvObjects,
  AdvStreams;

Type
  TAnsiStringBuilder = Class(TAdvObject)
    Private
      FContent : AnsiString;
      FLength : Integer;
      FBufferSize : integer;
    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function AsString : AnsiString;
      Function ToString : String; override;

      Procedure Clear;

      Procedure Append(ch : AnsiChar); Overload;
      Procedure Append(Const sStr : AnsiString); Overload;
      Procedure AppendLine(Const sStr : AnsiString); Overload;
      Procedure Append(Const iInt : Integer); Overload;
      Procedure AppendPadded(Const sStr : AnsiString; iCount : Integer; cPad : AnsiChar = ' ');
      Procedure AppendFixed(Const sStr : AnsiString; iCount : Integer; cPad : AnsiChar = ' ');
      Procedure Append(const bBytes : Array of Byte; amount : Integer); Overload;
      Procedure Append(Const oBuilder : TAnsiStringBuilder); Overload;
      Procedure AppendEOL;

      Procedure CommaAdd(Const sStr : AnsiString); Overload;

      Procedure AddByteAsBytes(iVal : Byte);
      Procedure AddWordAsBytes(iVal : word);
      Procedure AddCardinalAsBytes(iVal : Cardinal);
      Procedure AddInt64AsBytes(iVal : Int64);

      // index is zero based. zero means insert before first character
      Procedure Insert(Const sStr : AnsiString; iIndex : Integer); Overload;
      Procedure Insert(Const oBuilder : TAnsiStringBuilder; iIndex : Integer); Overload;

      Procedure Delete(iIndex, iLength : Integer);

      Property BufferSize : integer read FBufferSize write FBufferSize;
      Function IndexOf(Const sStr : AnsiString; bCase : Boolean = False) : Integer;
      Function LastIndexOf(Const sStr : AnsiString; bCase : Boolean = False) : Integer;

      Property Length : Integer Read FLength;
      Procedure Read(index : integer; var buffer; ilength : integer);
      Procedure Overwrite(index : integer; content : AnsiString);
  End;

type
  TAnsiCharSet = set of AnsiChar;

Function AnsiStringSplit(Const sValue : AnsiString; Const aDelimiters : TAnsiCharSet; Var sLeft, sRight: AnsiString) : Boolean;
Function AnsiPadString(const AStr: AnsiString; AWidth: Integer; APadChar: AnsiChar; APadLeft: Boolean): AnsiString;

Implementation


Uses
  MathSupport,
  StringSupport;

Const
  BUFFER_INCREMENT_SIZE = 1024;


Constructor TAnsiStringBuilder.Create;
Begin
  Inherited;

  FBufferSize := BUFFER_INCREMENT_SIZE;
End;


Destructor TAnsiStringBuilder.Destroy;
Begin
  inherited;
End;


Procedure TAnsiStringBuilder.Clear;
Begin
  FContent := '';
  FLength := 0;
End;


Function TAnsiStringBuilder.ToString : String;
Begin
  Result := Copy(String(FContent), 1, FLength);
End;


Procedure TAnsiStringBuilder.AppendPadded(Const sStr : AnsiString; iCount : Integer; cPad : AnsiChar = ' ');
Var
  iLen : Integer;
Begin
  iLen := IntegerMax(System.Length(sStr), iCount);

  If (iLen > 0) Then
  Begin
    If FLength + iLen > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, iLen));

    Move(sStr[1], FContent[FLength + 1], System.Length(sStr));

    If iLen = iCount Then
      FillChar(FContent[FLength + 1 + System.Length(sStr)], (iCount - System.Length(sStr)), cPad);

    Inc(FLength, iLen);
  End;
End;


Function TAnsiStringBuilder.AsString: AnsiString;
Begin
  Result := Copy(FContent, 1, FLength);
End;


Procedure TAnsiStringBuilder.AppendFixed(Const sStr : AnsiString; iCount : Integer; cPad : AnsiChar = ' ');
Begin
  If (iCount > 0) Then
  Begin
    If FLength + iCount > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, iCount));
    Move(sStr[1], FContent[FLength + 1], IntegerMin(System.Length(sStr), iCount));

    If System.Length(sStr) < iCount Then
      FillChar(FContent[FLength + 1 + System.Length(sStr)], (iCount - System.Length(sStr)), cPad);

    Inc(FLength, iCount);
  End;
End;


Procedure TAnsiStringBuilder.Append(ch : AnsiChar);
Begin
  If FLength + 1 > System.Length(FContent) Then
    SetLength(FContent, System.Length(FContent) + FBufferSize);

  Move(ch, FContent[FLength + 1], 1);
  Inc(FLength);
End;


Procedure TAnsiStringBuilder.Append(Const sStr : AnsiString);
Begin
 If (sStr <> '') Then
  Begin
    If FLength + System.Length(sStr) > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, System.Length(sStr)));

    Move(sStr[1], FContent[FLength + 1], System.Length(sStr));

    Inc(FLength, System.Length(sStr));
  End;
End;


Procedure TAnsiStringBuilder.Append(Const oBuilder : TAnsiStringBuilder);
Begin
  Append(oBuilder.AsString);
End;


Procedure TAnsiStringBuilder.AppendEOL;
Begin
  Append(cReturn);
End;


Procedure TAnsiStringBuilder.Append(Const iInt : Integer);
Begin
  Append(AnsiString(IntegerToString(iInt)));
End;


Procedure TAnsiStringBuilder.Insert(Const sStr : AnsiString; iIndex : Integer);
Begin
  If (sStr <> '') Then
  Begin
    If FLength + System.Length(sStr) > System.Length(FContent) Then
      SetLength(FContent, System.Length(FContent) + IntegerMax(FBufferSize, System.Length(sStr)));

    If (iIndex) <> FLength Then
      Move(FContent[iIndex+1], FContent[iIndex+1 + System.Length(sStr)], (FLength - iIndex));

    Move(sStr[1], FContent[iIndex+1], System.Length(sStr));

    Inc(FLength, System.Length(sStr));
  End;
End;


Procedure TAnsiStringBuilder.Insert(Const oBuilder : TAnsiStringBuilder; iIndex : Integer);
Begin
  Insert(oBuilder.AsString, iIndex);
End;


Procedure TAnsiStringBuilder.Delete(iIndex, iLength : Integer);
Begin
  System.Delete(FContent, iIndex+1, iLength);
  Dec(FLength, iLength);
End;


Function TAnsiStringBuilder.IndexOf(Const sStr : AnsiString; bCase : Boolean = False) : Integer;
Var
  iLoop : Integer;
  iUpper : Integer;
  iLen : Integer;
Begin
  Result := -1;
  iLoop := 1;
  iLen := System.Length(sStr);
  iUpper := FLength - iLen + 1;

  While (Result = -1) And (iLoop <= iUpper) Do
  Begin
    If (bCase And (Copy(FContent, iLoop, iLen) = sStr)) Or (Not bCase And (Copy(FContent, iLoop, iLen) = sStr)) Then
      Result := iLoop - 1;

    Inc(iLoop);
  End;
End;


Function TAnsiStringBuilder.LastIndexOf(Const sStr : AnsiString; bCase : Boolean = False) : Integer;
Var
  iLoop : Integer;
  iUpper : Integer;
  iLen : Integer;
Begin
  Result := -1;
  iLen := System.Length(sStr);
  iUpper := FLength - iLen + 1;
  iLoop := iUpper;
  While (Result = -1) And (iLoop > 0) Do
  Begin
    If (bCase And (Copy(FContent, iLoop, iLen) = sStr)) Or (Not bCase And (Copy(FContent, iLoop, iLen) = sStr)) Then
      Result := iLoop - 1;

    Dec(iLoop);
  End;
End;



Procedure TAnsiStringBuilder.CommaAdd(const sStr: AnsiString);
Begin
  if Length > 0 Then
    Append(', ');
  Append(sStr);
End;


Procedure TAnsiStringBuilder.AddCardinalAsBytes(iVal: Cardinal);
Var
  s : AnsiString;
Begin
  SetLength(s, 4);
  move(iVal, s[1], 4);
  Append(s);
End;


Procedure TAnsiStringBuilder.AddWordAsBytes(iVal: word);
Var
  s : AnsiString;
Begin
  SetLength(s, 2);
  move(iVal, s[1], 2);
  Append(s);
End;


Procedure TAnsiStringBuilder.AddInt64AsBytes(iVal: Int64);
Var
  s : AnsiString;
Begin
  SetLength(s, 8);
  move(iVal, s[1], 8);
  Append(s);
End;


Procedure TAnsiStringBuilder.AddByteAsBytes(iVal: Byte);
Var
  s : AnsiString;
Begin
  SetLength(s, 1);
  move(iVal, s[1], 1);
  Append(s);
End;


Procedure TAnsiStringBuilder.AppendLine(const sStr: AnsiString);
Begin
  Append(sStr);
  AppendEOL;
End;

procedure TAnsiStringBuilder.Append(const bBytes: array of Byte; amount: Integer);
var
  i : integer;
begin
  for i := 0 to amount - 1 Do
    Append(AnsiChar(bBytes[i]));
end;


procedure TAnsiStringBuilder.Overwrite(index: integer; content: AnsiString);
begin
  if index < 1 Then
    RaiseError('Overwrite', 'index < 1');
  if index + System.length(Content) > FLength Then
    RaiseError('Overwrite', 'index > length');
  if content <> '' Then
    Move(Content[1], FContent[index], System.length(Content));
end;

procedure TAnsiStringBuilder.Read(index: integer; var buffer; ilength: integer);
begin
  if index < 1 Then
    RaiseError('Read', 'index < 1');
  if index + ilength > FLength Then
    RaiseError('Read', 'index > length');
  Move(FContent[index], buffer, ilength);
end;


Function AnsiStringSplit(Const sValue : AnsiString; Const aDelimiters : TAnsiCharSet; Var sLeft, sRight: AnsiString) : Boolean;
Var
  iIndex : Integer;
  sA, sB : AnsiString;
Begin
  // Find the delimiter within the source string
  iIndex := StringFind(String(sValue), aDelimiters);
  Result := iIndex <> 0;

  If Not Result Then
  Begin
    sA := sValue;
    sB := '';
  End
  Else
  Begin
    sA := Copy(sValue, 1, iIndex - 1);
    sB := Copy(sValue, iIndex + 1, MaxInt);
  End;

  sLeft := sA;
  sRight := sB;
End;


function AnsiPadString(const AStr: AnsiString; AWidth: Integer; APadChar: AnsiChar; APadLeft: Boolean): AnsiString;
begin
  if Length(AStr) >= AWidth then
    Result := AStr
  else
    begin
    SetLength(Result, AWidth);
    FillChar(Result[1], AWidth, APadChar);
    if AStr <> '' then
      if APadLeft then
        Move(AStr[1], Result[(AWidth - Length(AStr)) + 1], Length(AStr))
      else
        Move(AStr[1], Result[1], Length(AStr))
    end;
end;


End.
