Unit AdvZipWriters;

{! 1 !}

Interface

Uses
  Windows,
  Classes,

  AdvBuffers,
  AdvNameBuffers,
  AdvObjectMatches,
  AdvObjects,
  AdvZipDeclarations,
  AdvZipParts,
  AdvZipUtilities,
  AdvZipWorkers,

  Zlib;

Type
  TAdvZippedData = Class (TAdvObject)
    Private
      FOffset : Integer;
      FCrc : LongWord;
      FCompressedSized : LongWord;
      FDate : Word;
      FTime : Word;
  End;

  TAdvZipWriter = Class (TAdvZipWorker)
    Private
      FPartInfo : TAdvObjectMatch;
      FOffset : Integer;
      FDirOffset : Integer;
      Procedure WriteLongWord(iValue : LongWord);
      Procedure WriteWord(iValue : Word);
      Procedure WriteString(Const sValue : AnsiString);

      Procedure Compress(oSource, oDestination : TAdvBuffer);

      Procedure WritePart(oPart : TAdvZipPart);
      Procedure WriteDirectory(oPart : TAdvZipPart);
      Procedure WriteEnd(iCount : Integer);
    Public
      Constructor Create; Override;
      Destructor Destroy; Override;
      Procedure WriteZip;
  End;

Implementation


Constructor TAdvZipWriter.Create;
Begin
  Inherited;
  FPartInfo := TAdvObjectMatch.Create;
End;

Destructor TAdvZipWriter.Destroy;
Begin
  FPartInfo.Free;
  Inherited;
End;


Procedure TAdvZipWriter.WriteZip;
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


Procedure TAdvZipWriter.WriteLongWord(iValue : LongWord);
Begin
  Stream.Write(iValue, 4);
  Inc(FOffset, 4);
End;


Procedure TAdvZipWriter.WriteWord(iValue : Word);
Begin
  Stream.Write(iValue, 2);
  Inc(FOffset, 2);
End;


Procedure TAdvZipWriter.WriteString(Const sValue : AnsiString);
Begin
  If (sValue <> '') Then
  Begin
    Stream.Write(sValue[1], Length(sValue));
    Inc(FOffset, Length(sValue));
  End;
End;


Procedure TAdvZipWriter.WritePart(oPart: TAdvZipPart);
Var
  oCompressed : TAdvBuffer;
  oInfo : TAdvZippedData;
Begin
  oInfo := TAdvZippedData.Create;
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

    oCompressed := TAdvBuffer.Create;
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


Procedure TAdvZipWriter.Compress(oSource, oDestination: TAdvBuffer);
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
    MoveMemory(oDestination.Data, pData, oDestination.Capacity);
  Finally
    oCompressedStream.Free;
  End;
End;

Procedure TAdvZipWriter.WriteDirectory(oPart: TAdvZipPart);
Var
  oInfo : TAdvZippedData;
Begin
  oInfo := TAdvZippedData(FPartInfo.GetValueByKey(oPart));

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

Procedure TAdvZipWriter.WriteEnd(iCount : Integer);
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

End.
