Unit FHIR.Support.Zip;

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
  {$IFDEF MACOS} FHIR.Support.Osx, {$ELSE} Windows, {$ENDIF} // for LongWord
  SysUtils, Classes,
  ZLib,
  FHIR.Support.Strings, FHIR.Support.System,
  FHIR.Support.Objects, FHIR.Support.Collections, FHIR.Support.Exceptions, FHIR.Support.Stream;

Const
  SIG_LOCAL_FILE_HEADER = $04034B50;
  SIG_DATA_DESCRIPTOR = $08074B50;
  SIG_CENTRAL_DIRECTORY_HEADER = $02014B50;
  SIG_DIGITAL_SIGNATURE = $05054B50;
  SEG_TERMINATION = $06054b50;

  METHOD_NONE = 0;
  METHOD_DEFLATE = 8;

Type
  TZipFlag = (
     flagEncrypted,           // Bit 0: If set, indicates that the file is encrypted.

     flagImploding1,
     flagImploding2,

     {     (For Method 6 - Imploding)
          Bit 1: If the compression method used was type 6,
                 Imploding, then this bit, if set, indicates
                 an 8K sliding dictionary was used.  If clear,
                 then a 4K sliding dictionary was used.
          Bit 2: If the compression method used was type 6,
                 Imploding, then this bit, if set, indicates
                 3 Shannon-Fano trees were used to encode the
                 sliding dictionary output.  If clear, then 2
                 Shannon-Fano trees were used.

          (For Methods 8 and 9 - Deflating)
          Bit 2  Bit 1
            0      0    Normal (-en) compression option was used.
            0      1    Maximum (-exx/-ex) compression option was used.
            1      0    Fast (-ef) compression option was used.
            1      1    Super Fast (-es) compression option was used.

          Note:  Bits 1 and 2 are undefined if the compression
                 method is any other.
         }
     flagUsesDataDescriptor,
{          Bit 3: If this bit is set, the fields crc-32, compressed
                 size and uncompressed size are set to zero in the
                 local header.  The correct values are put in the
                 data descriptor immediately following the compressed
                 data.  (Note: PKZIP version 2.04g for DOS only
                 recognizes this bit for method 8 compression, newer
                 versions of PKZIP recognize this bit for any
                 compression method.)                        }
     flagEnhancedDeflate,
                           {
          Bit 4: Reserved for use with method 8, for enhanced
                 deflating. }
     flagCompressPatched
     {
          Bit 5: If this bit is set, this indicates that the file is
                 compressed patched data.  (Note: Requires PKZIP
                 version 2.70 or greater)
      }
  );

Function Bit(iFlags : Word; aFlag : TZipFlag) : Boolean;

Function TimeAndDateToDateTime(iDate, iTime : Word) : TDateTime;
Procedure DateTimeToTimeAndDate(aValue : TDateTime; Out iDate, iTime : Word);

Function GetCRC(oBuffer : TFslBuffer) : LongWord;

type
    TFslZipPart = Class(TFslNameBuffer)
    Private
      FTimestamp: TDateTime;
      FComment : String;

    Public
      Function Link : TFslZipPart;
      Function Clone : TFslZipPart;

      Procedure Assign(oObject : TFslObject); Override;

      Property Timestamp : TDateTime Read FTimestamp Write FTimestamp;
      Property Comment : String Read FComment Write FComment;
  End;

  TFslZipPartList = Class(TFslNameBufferList)
    Private
      Function GetPart(iIndex : Integer) : TFslZipPart;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Function Link : TFslZipPartList;
      Function Clone : TFslZipPartList;

      Function GetByName(Const sName : String) : TFslZipPart;

      Property Part[iIndex : Integer] : TFslZipPart Read GetPart; Default;

      procedure add(name : String; bytes : TBytes); overload;
  End;


  TFslZipWorker = Class (TFslObject)
    Private
      FStream : TFslStream;
      FParts : TFslZipPartList;
      Function GetStream : TFslStream;
      Function GetParts : TFslZipPartList;
      Procedure SetStream(oValue : TFslStream);
      Procedure SetParts(oValue : TFslZipPartList);

    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Function HasStream : Boolean;
      Function HasParts : Boolean;

      Property Stream : TFslStream Read GetStream Write SetStream;
      Property Parts : TFslZipPartList Read GetParts Write SetParts;
  End;

  TFslZipReader = Class (TFslZipWorker)
    Private
      Function EndCondition(iLongWord : LongWord) : Boolean;
      Function ReadLongWord : LongWord;
      Function ReadWord : Word;
      Function ReadByte : Byte;
      Procedure ReadPart;
      Procedure Skip(iCount : Integer);
      Function ReadString(iLength : Word):AnsiString;
      Procedure ReadData(partName : string; iFlags, iComp : Word; iSizeComp, iSizeUncomp : LongWord; oBuffer : TFslBuffer);
      Procedure ReadDeflate(iFlags : Word; partName : string; iSizeComp, iSizeUncomp : LongWord; oBuffer: TFslBuffer);
      Procedure ReadUncompressed(iSizeComp : LongWord; oBuffer: TFslBuffer);
      Procedure ReadUnknownLengthDeflate(partName : string; oBuffer : TFslBuffer);
      Procedure ReadKnownDeflate(pIn : Pointer; partName : string; iSizeComp, iSizeDecomp : LongWord; oBuffer : TFslBuffer);
      Procedure ReadDirectory(iCount : Integer);
      Procedure ReadDigSig;
      Procedure ReadTermination;
    Public
      Procedure ReadZip; Overload; Virtual;
  End;

  Type
  TFslZippedData = Class (TFslObject)
    Private
      FOffset : Integer;
      FCrc : LongWord;
      FCompressedSized : LongWord;
      FDate : Word;
      FTime : Word;
  End;

  TFslZipWriter = Class (TFslZipWorker)
    Private
      FPartInfo : TFslObjectMatch;
      FOffset : Integer;
      FDirOffset : Integer;
      Procedure WriteLongWord(iValue : LongWord);
      Procedure WriteWord(iValue : Word);
      Procedure WriteString(Const sValue : AnsiString);

      Procedure Compress(oSource, oDestination : TFslBuffer);

      Procedure WritePart(oPart : TFslZipPart);
      Procedure WriteDirectory(oPart : TFslZipPart);
      Procedure WriteEnd(iCount : Integer);
    Public
      Constructor Create; Override;
      Destructor Destroy; Override;
      Procedure WriteZip;

      procedure addFile(name, actual : String);
  End;

Implementation


Constructor TFslZipWorker.Create;
Begin
  Inherited;
  FParts := TFslZipPartList.Create;
End;

Destructor TFslZipWorker.Destroy;
Begin
  FStream.Free;
  FParts.Free;
  Inherited;
End;

Function TFslZipWorker.GetParts: TFslZipPartList;
Begin
  Assert(Invariants('GetParts', FParts, TFslZipPartList, 'Parts'));
  Result := FParts;
End;

Function TFslZipWorker.GetStream: TFslStream;
Begin
  Assert(Invariants('GetStream', FStream, TFslStream, 'Stream'));
  Result := FStream;
End;

Function TFslZipWorker.HasParts: Boolean;
Begin
  Result := FParts <> Nil;
End;

Function TFslZipWorker.HasStream: Boolean;
Begin
  Result := FStream <> Nil;
End;

Procedure TFslZipWorker.SetParts(oValue: TFslZipPartList);
Begin
  FParts.Free;
  FParts := oValue;
End;

Procedure TFslZipWorker.SetStream(oValue: TFslStream);
Begin
  FStream.Free;
  FStream := oValue;
End;

Function Bit(iFlags : Word; aFlag : TZipFlag) : Boolean;
Var
  iVal : Word;
Begin
  iVal := 1 Shl Ord(aFlag);
  Result := iFlags And iVal > 0;
End;


Procedure TFslZipPart.Assign(oObject : TFslObject);
Begin
  Inherited;
  FTimestamp := TFslZipPart(oObject).FTimestamp;
  FComment := TFslZipPart(oObject).FComment;
End;



Function TFslZipPart.Link : TFslZipPart;
Begin
  Result := TFslZipPart(Inherited Link);
End;


Function TFslZipPart.Clone : TFslZipPart;
Begin
  Result := TFslZipPart(Inherited Clone);
End;


procedure TFslZipPartList.add(name: String; bytes: TBytes);
var
  part : TFslZipPart;
begin
  part := TFslZipPart.Create;
  try
    part.Name := name;
    part.AsBytes := bytes;
    add(part.Link);
  finally
    part.Free;
  end;
end;

Function TFslZipPartList.Clone : TFslZipPartList;
Begin
  Result := TFslZipPartList(Inherited Clone);
End;


Function TFslZipPartList.Link : TFslZipPartList;
Begin
  Result := TFslZipPartList(Inherited Link);
End;


Function TFslZipPartList.ItemClass : TFslObjectClass;
Begin
  Result := TFslZipPart;
End;



Function TFslZipPartList.GetPart(iIndex : Integer) : TFslZipPart;
Begin
  Result := TFslZipPart(ObjectByIndex[iIndex]);
End;


Function TFslZipPartList.GetByName(Const sName: String): TFslZipPart;
Begin
  Result := TFslZipPart(Inherited GetByName(sName));
End;


Function TFslZipReader.ReadLongWord: LongWord;
Begin
  Result := 0;
  Stream.Read(Result, SizeOf(Result));
End;

Function TFslZipReader.ReadWord: Word;
Begin
  Result := 0;
  Stream.Read(Result, SizeOf(Result));
End;

Function TFslZipReader.ReadByte: Byte;
Begin
  Result := 0;
  Stream.Read(Result, SizeOf(Result));
End;


Procedure TFslZipReader.ReadPart;
Var
  iFlags : Word;
  iComp : Word;
  iSizeComp : LongWord;
  iSizeUncomp : LongWord;
  iNameLen : Word;
  iExtraLen : Word;
  iDate : Word;
  iTime : Word;
  oBuffer : TFslZipPart;
Begin
  oBuffer := TFslZipPart(Parts.New);
  Try
    ReadWord;                          // version needed to extract       2 bytes
    iFlags := ReadWord;               // general purpose bit flag        2 bytes
    iComp := ReadWord;                // compression method              2 bytes
    iTime := ReadWord;                // last mod file time              2 bytes
    iDate := ReadWord;                // last mod file date              2 bytes
    oBuffer.Timestamp := TimeAndDateToDateTime(iDate, iTime);
    ReadLongWord;                          // crc-32                          4 bytes
    iSizeComp := ReadLongWord;           // compressed size                 4 bytes
    iSizeUncomp := ReadLongWord;         // uncompressed size               4 bytes
    iNameLen := ReadWord;             // filename length                 2 bytes
    iExtraLen := ReadWord;            // extra field length              2 bytes
    oBuffer.Name := string(ReadString(iNameLen));    // filename (variable size)
    Skip(iExtraLen);                  // extra field (variable size)

    {Immediately following the local header for a file
      is the compressed or stored data for the file. }
    ReadData(oBuffer.Name, iFlags, iComp, iSizeComp, iSizeUncomp, oBuffer);

    Parts.Add(oBuffer.Link);
  Finally
    oBuffer.Free;
  End;
End;

Procedure TFslZipReader.ReadZip;
Var
  iSig : LongWord;
  iCount : Integer;
Begin
  Parts.Clear;
  Parts.Unsorted;

  iSig := ReadLongWord;
  While (iSig = SIG_LOCAL_FILE_HEADER) Do
  Begin
    ReadPart;
    iSig := ReadLongWord;
  End;
  iCount := 0;
  While (iSig = SIG_CENTRAL_DIRECTORY_HEADER) Do
  Begin
    ReadDirectory(iCount);
    Inc(iCount);
    iSig := ReadLongWord;
  End;
  If (iSig = SIG_DIGITAL_SIGNATURE) Then
  Begin
    ReadDigSig;
    iSig := ReadLongWord;
  End;
  If (iSig = SEG_TERMINATION) Then
  Begin
    ReadTermination;
  End;
  // we ignore the rest of the file!
End;

Procedure TFslZipReader.ReadUncompressed(iSizeComp : LongWord; oBuffer: TFslBuffer);
Begin
  oBuffer.Capacity := iSizeComp;
  If (iSizeComp > 0) Then
    Stream.Read(oBuffer.Data^, oBuffer.Capacity);
End;

Procedure TFslZipReader.ReadDeflate(iFlags : Word; partName : string; iSizeComp, iSizeUncomp : LongWord; oBuffer: TFslBuffer);
Var
  pIn : PAnsiChar;
Begin
  If Bit(iFlags, flagUsesDataDescriptor) Then
    ReadUnknownLengthDeflate(partName, oBuffer)
  Else
  Begin
    GetMem(pIn, iSizeComp+2);
    Try
      pIn[0] := AnsiChar(120);
      pIn[1] := AnsiChar(156);
      Stream.Read(pIn[2], iSizeComp);
      ReadKnownDeflate(pIn, partName, iSizeComp+2, iSizeUncomp, oBuffer);
    Finally
      FreeMem(pIn);
    End;
  End;
End;

Function TFslZipReader.EndCondition(iLongWord : LongWord) : Boolean;
Begin
  Result := //(Stream.Readable = 0) Or // shouldn't run out - should run into the central directory
            (iLongWord = SIG_DATA_DESCRIPTOR);
End;


Procedure TFslZipReader.ReadUnknownLengthDeflate(partName : string; oBuffer : TFslBuffer);
Var
  iCurrent, iCRC : LongWord;
  iByte : Byte;
  oMem : TFslMemoryStream;
  iSizeComp : LongWord;
  iSizeUncomp : LongWord;
  count : integer;
  first : boolean;
Begin
  // well, we don't know how long it's going to be.
  // what we're going to do is read this a byte at a time until
  // we are at the next header or the source runs out. There's a minor chance
  // that this will terminate early (1 in 2^32)
  // then we lop off the last 12 bytes, and treat this is the decompressible
  // we can start with a 4 byte read because we know we have at least 12 bytes
  oMem := TFslMemoryStream.Create;
  Try
    iByte := 120;
    oMem.Write(iByte, 1);
    iByte := 156;
    oMem.Write(iByte, 1);
    iCurrent := ReadLongWord;
    count := 0;
    repeat
      first := true;
      While first or (Not EndCondition(iCurrent)) Do
      Begin
        iByte := iCurrent And $FF;
        oMem.Write(iByte, 1);
        iCurrent := (iCurrent And $FFFFFF00) Shr 8 + ReadByte Shl 24;
        first := false;
      End;
      inc(count);
    until true; // (partName <> 'fhir.schema.json.zip') or (count > 1);
    If iCurrent <> SIG_DATA_DESCRIPTOR Then
      RaiseError('ReadUnknownLengthDeflate', 'Error in zip structure: Source is not terminated by a Data Descriptor');
    iCRC := ReadLongWord;                // crc-32                          4 bytes
    iSizeComp := ReadLongWord;           // compressed size                 4 bytes
    iSizeUncomp := ReadLongWord;         // uncompressed size               4 bytes
    {$WARNINGS OFF} // a widening notifications in this check and the assertion in ReadKnownDeflate
    If oMem.Buffer.Capacity < iSizeComp + 2 Then
      RaiseError('ReadUnknownLengthDeflate', 'Compressed length expected to be '+IntegerToString(iSizeComp)+' bytes but found '+IntegerToString(oMem.Buffer.Capacity)+' bytes');
    ReadKnownDeflate(oMem.Buffer.Data, partName, iSizeComp + 2, iSizeUncomp, oBuffer);
  Finally
    oMem.Free;
  End;
End;

Type
  TPointerMemoryStream = Class (TCustomMemoryStream)
    Constructor Create(pData : Pointer; iSize : Integer);
    Function Write(Const Buffer; Count: LongInt): LongInt; Override;
  End;

Constructor TPointerMemoryStream.Create(pData : Pointer; iSize : Integer);
Begin
  Inherited Create;
  SetPointer(pData, iSize);
End;

Function TPointerMemoryStream.Write(Const Buffer; Count: Integer): LongInt;
Begin
  Raise EFslException.Create('Should never be called');
End;


Procedure TFslZipReader.ReadKnownDeflate(pIn : Pointer; partName : string; iSizeComp, iSizeDecomp : LongWord; oBuffer : TFslBuffer);
Var
  oSrc : TStream;
  oDecompressor : TZDecompressionStream;

{$IFOPT C+}
  iRead : Integer;
{$ENDIF}
Begin
  If iSizeDecomp > 0 Then
  Begin
    oSrc := TPointerMemoryStream.Create(pIn, iSizeComp);
    Try
      oDecompressor := TZDecompressionStream.Create(oSrc);
      Try
        oBuffer.Capacity := iSizeDecomp;

      {$IFOPT C+}
        iRead := oDecompressor.Read(oBuffer.Data^, iSizeDecomp);
        Assert(CheckCondition(iRead = iSizeDecomp, 'ReadKnownDeflate', partName+': Expected to read '+IntegerToString(iSizeDecomp)+
            ' bytes, but actually found '+IntegerToString(iRead)+' bytes'));
      {$ELSE}
        oDecompressor.Read(oBuffer.Data^, iSizeDecomp);
      {$ENDIF}
      Finally
        oDecompressor.Free;
      End;
    Finally
      oSrc.Free;
    End;
  End;
End;

Procedure TFslZipReader.ReadData(partName : string; iFlags, iComp : Word; iSizeComp, iSizeUncomp: LongWord; oBuffer: TFslBuffer);
Begin
  Case iComp Of
    METHOD_NONE: ReadUncompressed(iSizeComp, oBuffer);
    METHOD_DEFLATE: ReadDeflate(iFlags, partName, iSizeComp, iSizeUncomp, oBuffer);
  Else
    RaiseError('Decompress', 'Unknown Compression type '+IntegerToString(iComp));
  End;
End;

Function TFslZipReader.ReadString(iLength: Word): AnsiString;
Begin
  SetLength(Result, iLength);
  If (iLength > 0) Then
    Stream.Read(Result[1], iLength);
End;

Procedure TFslZipReader.Skip(iCount: Integer);
Begin
  If iCount > 0 Then
    ReadString(iCount);
End;

Procedure TFslZipReader.ReadDigSig;
Var
  iLen : Word;
Begin
  iLen := ReadWord;
  Skip(iLen);
End;

Procedure TFslZipReader.ReadDirectory(iCount: Integer);
Var
  oPart : TFslZipPart;
  iNameLen : Word;
  iExtraLen : Word;
  iCommLen : Word;
Begin
  oPart := Parts.Part[iCount];
  ReadWord; //  version made by                 2 bytes    63 vs 20
  ReadWord; //  version needed to extract       2 bytes
  ReadWord; //  general purpose bit flag        2 bytes
  ReadWord; //  compression method              2 bytes
  ReadWord; //  last mod file time              2 bytes
  ReadWord; //  last mod file date              2 bytes
  ReadLongWord; //  crc-32                          4 bytes
  ReadLongWord; //  compressed size                 4 bytes
  ReadLongWord; //  uncompressed size               4 bytes
  iNameLen := ReadWord;  // filename length                 2 bytes
  iExtraLen := ReadWord; // extra field length              2 bytes   36 vs 0
  iCommLen := ReadWord; // file comment length             2 bytes
  ReadWord; // disk number start               2 bytes
  ReadWord; // internal file attributes        2 bytes
  ReadLongWord; // external file attributes        4 bytes  32 vs 0
  ReadLongWord; // relative offset of local header 4 bytes
  Skip(iNameLen);
  Skip(iExtraLen);
  oPart.Comment := string(ReadString(iCommLen));
End;

Procedure TFslZipReader.ReadTermination;
Begin

End;


Constructor TFslZipWriter.Create;
Begin
  Inherited;
  FPartInfo := TFslObjectMatch.Create;
End;

Destructor TFslZipWriter.Destroy;
Begin
  FPartInfo.Free;
  Inherited;
End;


Procedure TFslZipWriter.WriteZip;
Var
  iLoop : Integer;
Begin
  FOffset := 0;
  FPartInfo.Clear;

  For iLoop := 0 To Parts.Count - 1 Do
    WritePart(Parts[iLoop]);
  FDirOffset := FOffset;
  For iLoop := 0 To Parts.Count - 1 Do
    WriteDirectory(Parts[iLoop]);
  WriteEnd(Parts.Count);
End;


Procedure TFslZipWriter.WriteLongWord(iValue : LongWord);
Begin
  Stream.Write(iValue, 4);
  Inc(FOffset, 4);
End;


Procedure TFslZipWriter.WriteWord(iValue : Word);
Begin
  Stream.Write(iValue, 2);
  Inc(FOffset, 2);
End;


Procedure TFslZipWriter.WriteString(Const sValue : AnsiString);
Begin
  If (sValue <> '') Then
  Begin
    Stream.Write(sValue[1], Length(sValue));
    Inc(FOffset, Length(sValue));
  End;
End;


Procedure TFslZipWriter.WritePart(oPart: TFslZipPart);
Var
  oCompressed : TFslBuffer;
  oInfo : TFslZippedData;
Begin
  oInfo := TFslZippedData.Create;
  Try
    oInfo.FOffset := FOffset;
    WriteLongWord(SIG_LOCAL_FILE_HEADER);
    WriteWord($14);    // version needed to extract. don't know why $14, just observed in any .zip file
    WriteWord(0);      // general purpose bit flag. We don't set any flags
    If oPart.Capacity > 0 Then   // compression method  2 bytes
      WriteWord(8)
    Else
      WriteWord(0);
    DateTimeToTimeAndDate(oPart.TimeStamp, oInfo.FDate, oInfo.FTime);
    WriteWord(oInfo.FTime);      // last mod file time              2 bytes
    WriteWord(oInfo.FDate);      // last mod file date              2 bytes
    oInfo.FCrc := GetCRC(oPart);
    WriteLongWord(oInfo.FCrc);     // crc-32

    oCompressed := TFslBuffer.Create;
    Try
      If (oPart.Capacity > 0) Then
        Compress(oPart, oCompressed);

      oInfo.FCompressedSized := oCompressed.Capacity;
      WriteLongWord(oInfo.FCompressedSized); // compressed size                 4 bytes
      WriteLongWord(oPart.Capacity);       // uncompressed size               4 bytes
      WriteWord(Length(oPart.Name));    // filename length                 2 bytes
      WriteWord(0);                     // extra field length - we don't use
      WriteString(AnsiString(oPart.Name));

      If (oCompressed.Capacity > 0) Then
      Begin
        Stream.Write(oCompressed.Data^, oCompressed.Capacity);
        Inc(FOffset, oCompressed.Capacity);
      End;

    Finally
      oCompressed.Free;
    End;
    FPartInfo.Add(oPart.Link, oInfo.Link);
  Finally
    oInfo.Free;
  End;
End;


procedure TFslZipWriter.addFile(name, actual: String);
var
  part : TFslZipPart;
begin
  part := TFslZipPart.Create;
  try
    part.Name := name;
    part.Timestamp := FileGetModified(actual);
    part.LoadFromFileName(actual);
    Parts.Add(part.Link);
  finally
    part.Free;
  end;
end;

Procedure TFslZipWriter.Compress(oSource, oDestination: TFslBuffer);
Var
  oCompressor: TCompressionStream;
  oCompressedStream: TMemoryStream;
  pData : PAnsiChar;
Begin
  oCompressedStream := TMemoryStream.Create;
  Try
    oCompressor := TCompressionStream.Create(clMax, oCompressedStream);
    Try
      oCompressor.Write(oSource.Data^, oSource.Capacity);
    Finally
      oCompressor.Free;
    End;
    pData := oCompressedStream.Memory;
    pData := PData + 2;
    oDestination.Capacity := oCompressedStream.Size - 6; // 2 off the front, 4 off the back
    MemoryMove(oDestination.Data, pData, oDestination.Capacity);
  Finally
    oCompressedStream.Free;
  End;
End;

Procedure TFslZipWriter.WriteDirectory(oPart: TFslZipPart);
Var
  oInfo : TFslZippedData;
Begin
  oInfo := TFslZippedData(FPartInfo.GetValueByKey(oPart));

  WriteLongWord(SIG_CENTRAL_DIRECTORY_HEADER);
  WriteWord($14); // version made by                 2 bytes
  WriteWord($14); // version needed to extract       2 bytes
  WriteWord(0);      // general purpose bit flag. We don't set any flags
  If oPart.Capacity > 0 Then   // compression method  2 bytes
    WriteWord(8)
  Else
    WriteWord(0);
  WriteWord(oInfo.FTime);      // last mod file time              2 bytes
  WriteWord(oInfo.FDate);      // last mod file date              2 bytes
  WriteLongWord(GetCRC(oPart));     // crc-32  TODO: cache this?
  WriteLongWord(oInfo.FCompressedSized); // compressed size                 4 bytes
  WriteLongWord(oPart.Capacity);       // uncompressed size               4 bytes
  WriteWord(Length(oPart.Name));    // filename length                 2 bytes
  WriteWord(0);                     // extra field length - we don't use
  WriteWord(Length(oPart.Comment));    // file comment length             2 bytes
  WriteWord(0);                     // disk number start               2 bytes
  WriteWord(0);                     // internal file attributes        2 bytes
  WriteLongWord(0);                 // external file attributes        4 bytes
  WriteLongWord(oInfo.FOffset);
  WriteString(AnsiString(oPart.Name));          // filename (variable size)
  WriteString(AnsiString(oPart.Comment));       // file comment (variable size)

End;

Procedure TFslZipWriter.WriteEnd(iCount : Integer);
Var
  iOffset : Integer;
Begin
  iOffset := FOffset;
  WriteLongWord(SEG_TERMINATION); // end of central dir signature    4 bytes  (0x06054b50)
  WriteWord(0);     //  number of this disk             2 bytes
  WriteWord(0);     // number of the disk with the start of the central directory  2 bytes
  WriteWord(iCount); // total number of entries in the central dir on this disk    2 bytes
  WriteWord(iCount); // total number of entries in the central dir                 2 bytes
  WriteLongWord(iOffset-FDirOffset); // size of the central directory   4 bytes
  WriteLongWord(FDirOffset);         // offset of start of central
  WriteLongWord(0);     // directory with respect to the starting disk number        4 bytes
  WriteWord(0);     //  .ZIP file comment length        2 bytes
  //       .ZIP file comment       (variable size)
End;


Function TimeAndDateToDateTime(iDate, iTime : Word) : TDateTime;
Var
  iCombined : Integer;
Begin
  LongRec(iCombined).Lo := iTime;
  LongRec(iCombined).Hi := iDate;
  Result := FileDateToDateTime(iCombined);
End;

Procedure DateTimeToTimeAndDate(aValue : TDateTime; Out iDate, iTime : Word);
Var
  iCombined : Integer;
Begin
  If (aValue = 0) Then
    aValue := Now;
  iCombined := DateTimeToFileDate(aValue);
  iTime := LongRec(iCombined).Lo;
  iDate := LongRec(iCombined).Hi;
End;

Function GetCRC(oBuffer : TFslBuffer) : LongWord;
Const
  CRCtable: Array[0..255] Of LongWord = (
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535,
    $9E6495A3, $0EDB8832, $79DCB8A4,
    $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91, $1DB71064,
    $6AB020F2, $F3B97148, $84BE41DE,
    $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856, $646BA8C0, $FD62F97A,
    $8A65C9EC, $14015C4F, $63066CD9,
    $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1,
    $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF,
    $ABD13D59, $26D930AC, $51DE003A,
    $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F, $2802B89E,
    $5F058808, $C60CD9B2, $B10BE924,
    $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D, $76DC4190, $01DB7106, $98D220BC,
    $EFD5102A, $71B18589, $06B6B51F,
    $9FBFE4A5, $E8B8D433, $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB,
    $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1,
    $F50FC457, $65B0D9C6, $12B7E950,
    $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65, $4DB26158,
    $3AB551CE, $A3BC0074, $D4BB30E2,
    $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A, $346ED9FC, $AD678846,
    $DA60B8D0, $44042D73, $33031DE5,
    $AA0A4C5F, $DD0D7CC9, $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525,
    $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B,
    $C0BA6CAD, $EDB88320, $9ABFB3B6,
    $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683, $E3630B12,
    $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1, $F00F9344, $8708A3D2, $1E01F268,
    $6906C2FE, $F762575D, $806567CB,
    $196C3671, $6E6B06E7, $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F,
    $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD,
    $48B2364B, $D80D2BDA, $AF0A1B4C,
    $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79, $CB61B38C,
    $BC66831A, $256FD2A0, $5268E236,
    $CC0C7795, $BB0B4703, $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28, $2BB45A92,
    $5CB36A04, $C2D7FFA7, $B5D0CF31,
    $2CD99E8B, $5BDEAE1D, $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9,
    $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7,
    $0BDBDF21, $86D3D2D4, $F1D4E242,
    $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777, $88085AE6,
    $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45, $A00AE278, $D70DD2EE, $4E048354,
    $3903B3C2, $A7672661, $D06016F7,
    $4969474D, $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53,
    $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729,
    $23D967BF, $B3667A2E, $C4614AB8,
    $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);
Var
  iLoop : Integer;
  pData : pAnsiChar;
Begin
  Result := $FFFFFFFF;
  pData := oBuffer.Data;
  For iLoop := 0 To oBuffer.Capacity - 1 Do
  Begin
    Result := (Result Shr 8) Xor (CRCtable[Byte(Result) Xor Ord(pData^)]);
    Inc(pData);
  End;
  Result := Result Xor $FFFFFFFF;
End;


End.

