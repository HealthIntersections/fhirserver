Unit wp_loader_dicom;

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
  SysUtils, JPeg,
  GDIPAPI, GDIPOBJ,
  fsl_base, fsl_utilities, fsl_stream,
  dicom_dictionary, dicom_objects, dicom_jpegls,
  wp_graphics, wp_gdiplus;

Type
  TDicomImageExtractor = class (TFslObject)
  Private
    FInstance : TDicomInstance;
    FErrorsInImage : Boolean;
    FScaleContrast : Boolean;

    FRoot : TDicomObject;
    FTransferSyntax : String;
    FBytes : TBytes;
    FHighestValue : Cardinal;
    FLowestValue : Cardinal;
    FWidth : Integer;
    FHeight : Integer;
    FSamplesPerPixel : Integer;
    FFrameCount : integer;
    FBitsAllocated : integer;
    FBitsStored : Integer;
    FHighBit : integer;
    FPixelRepresentation : integer;
    FPlanarConfiguration : integer;
    FStride : Integer;

    FInstanceId : String; // 0008,0018
    FDateTime: String; // 0008,0020 + 0008,0030
    FAccessionNumber : String; // 0008,0050
    FModality: String; // 0080, 0060
    FPatientName: String; // 0010,0010
    FPatientDOB: String; // 0010,0030
    FPatientSex: String; // 0010,0040

    Procedure ScanGray8;
    Procedure ScanGray16X;
    Procedure ScanColor8;
    Procedure ScanColor16X;

    Function ReadGray8(const iFrameIndex : Integer) : TBytes;
    Function ReadGray16X(iFrameIndex : Integer) : TBytes;
    Function ReadRGB24(iFrameIndex : Integer) : TBytes;
    Function ReadRGB36(iFrameIndex : Integer) : TBytes;

    Function LoadJpegLS(oElem : TDicomDataElement) : TBytes;
    Function ReadValue(sTag : String; iDefault : integer = -1) : Integer;
    Function ReadStringValue(sTag : String; iDefault : String = '') : String;
//    Function GetByPath(oRoot : TDicomObject; sRoot : String; var iFrameIndex : Integer) : TDicomObject;
    Procedure CheckForImages(oRoot : TDicomObject; sTransferSyntax : String);
    Procedure LoadBytes(oRoot : TDicomObject);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;

    Property ErrorsInImage : Boolean read FErrorsInImage write FErrorsInImage;
    Property ScaleContrast : Boolean read FScaleContrast write FScaleContrast;
    Property TransferSyntax : String read FTransferSyntax write FTransferSyntax;

    Procedure Open(oInstance : TDicomInstance);
    Property ImageCount : Integer read FFrameCount;
    Procedure LoadImage(iIndex : Integer; bAnnotations : Boolean; oImage : TGdiPlusBitmapImage);
    Procedure Close;
  End;

Implementation

{ TDicomImageExtractor }

destructor TDicomImageExtractor.Destroy;
begin
  Close;
  inherited;
end;

Procedure TDicomImageExtractor.CheckForImages(oRoot : TDicomObject; sTransferSyntax : String);
Begin
  if sTransferSyntax <> '' Then
    FTransferSyntax := sTransferSyntax;
  If (oRoot = nil) or Not oRoot.Elements.ExistsByTag('7FE0,0010') Then
    FFrameCount := 0
  Else
    LoadBytes(oRoot);
End;

procedure TDicomImageExtractor.Open(oInstance: TDicomInstance);
begin
  if Assigned(FInstance) then
    Close;

  FInstance := oInstance.Link;
  FFrameCount := 0;

  Case FInstance.InstanceType of
    ditSimpleObject : CheckForImages(FInstance.SimpleObject, '');
    ditMessage : CheckForImages(FInstance.Message.Data, FInstance.Message.TransferSyntax);
    ditFileObject : CheckForImages(FInstance.FileObject.Content, FInstance.FileObject.TransferSyntaxUID);
  Else
    // ditNull, ditAssociateRequestPDU, ditAssociateAcceptPDU, ditAssociateRejectPDU, ditDataPDU, ditAbortPDU, ditReleaseRequestPDU, ditReleaseResponsePDU
    // nothing
  End;
End;

Function TDicomImageExtractor.ReadValue(sTag : String; iDefault : integer = -1) : Integer;
var
  oElement : TDicomDataElement;
Begin
  oElement := FRoot.Elements.GetByTag(sTag);
  if oElement <> nil Then
    if oElement.Values.KnownType = dvtIS Then
      result := StrToInt(oElement.Values[0].AsIS)
    Else
      result := oElement.Values[0].AsUS
  Else if iDefault > -1 Then
    result := iDefault
  Else
    raise EDicomException.create('Unable to find image tag '+sTag);
End;

Function TDicomImageExtractor.ReadStringValue(sTag : String; iDefault : String = '') : String;
var
  oElement : TDicomDataElement;
Begin
  oElement := FRoot.Elements.GetByTag(sTag);
  if (oElement <> nil) And (oElement.Values.Count > 0) Then
     result := oElement.Values[0].AsString
  Else if iDefault > '' Then
    result := iDefault
  Else
    raise EDicomException.create('Unable to find image tag '+sTag);
End;

Procedure TDicomImageExtractor.LoadBytes(oRoot : TDicomObject);
var
  oElem : TDicomDataElement;
Begin
  FRoot := oRoot.Link;
  FFrameCount := ReadValue('0028,0008', 1);
  FWidth := ReadValue('0028,0011');
  FHeight := ReadValue('0028,0010');
  FSamplesPerPixel := ReadValue('0028,0002', 1);
  FBitsAllocated := ReadValue('0028,0100');
  FBitsStored := ReadValue('0028,0101');
  FHighBit := ReadValue('0028,0102', 0);
  FPixelRepresentation := ReadValue('0028,0103', 0);
  FPlanarConfiguration := ReadValue('0028,0006', 0);

  FInstanceId := ReadStringValue('0008,0018', ' ');
  FDateTime:= ReadStringValue('0008,0020', ' ') + ' ' + ReadStringValue('0008,0030', ' ');
  FAccessionNumber := ReadStringValue('0008,0050', ' ');
  FModality:= ReadStringValue('0080,0060', ' ');
  FPatientName:= ReadStringValue('0010,0010', ' ');
  FPatientDOB:= ReadStringValue('0010,0030', ' ');
  FPatientSex:= ReadStringValue('0010,0040', ' ');

  //if FPixelRepresentation > 0 then
  //  raise EDicomException.create('Pixel Representation '+inttostr(FPixelRepresentation)+' is not yet handled');

  FStride := FWidth * 3;
  if FStride mod 4 <> 0 Then
    FStride := FStride + 4 - FStride mod 4;

  oElem := FRoot.Elements.GetByTag('7FE0,0010');
  if (oElem = nil) Then
    raise EDicomException.create('Unable to find Pixel Data');
  if not (oElem.Values.KnownType in [dvtOB, dvtOW]) Then
    raise EDicomException.create('Unexpected VR Type "'+DICOM_VR_TYPE_NAMES[oElem.Values.KnownType]+'" for Pixel Data');

  if (FTransferSyntax = '1.2.840.10008.1.2.2') or (FTransferSyntax = '1.2.840.10008.1.2.1') or (FTransferSyntax = '1.2.840.10008.1.2') Or (FTransferSyntax = '') Then
  Begin
    if oElem.Values.Count <> 1 Then
      raise EDicomException.create('unexpected value count for pixel data: '+inttostr(oElem.Values.COunt));
    FBytes := oElem.values[0].AsOB;
  End
  Else if FTransferSyntax = '1.2.840.10008.1.2.4.80' Then
    FBytes := LoadJpegLS(oElem)
  Else if FTransferSyntax = '1.2.840.10008.1.2.4.81' Then
    FBytes := LoadJpegLS(oElem)
  Else if (FTransferSyntax = '1.2.840.10008.1.2.5') Then
    raise EDicomException.create('RLE Encoding not yet supported')
  Else if FRoot.Elements.Dictionary.TransferSyntaxName(FTransferSyntax) = '' then
    raise EDicomException.create('Unknown transfer encoding '+ FTransferSyntax)
  Else
    raise EDicomException.create('Unsupported transfer encoding '+ FTransferSyntax+' ('+FRoot.Elements.Dictionary.TransferSyntaxName(FTransferSyntax)+')');

  if FScaleContrast Then
  Begin
    If (FSamplesPerPixel = 1) Then
    Begin
      if (FBitsAllocated = 8) and (FBitsStored = 8) And (FHighBit = 7) Then
        ScanGray8
      Else if (FBitsAllocated = 16) And (FBitsStored <= FBitsAllocated) And (FHighBit = FBitsStored - 1)  Then
        ScanGray16X;
    End
    Else If (FSamplesPerPixel = 3) Then
    Begin
      if (FBitsAllocated = 8) and (FBitsStored = 8) And (FHighBit = 7) Then
        ScanColor8
      Else if (FBitsAllocated = 16) And (FBitsStored <= FBitsAllocated) And (FHighBit = FBitsStored - 1)  Then
        ScanColor16X;
    End;
  End;
End;

Procedure TDicomImageExtractor.ScanGray8;
var
  i : integer;
  b : Byte;
Begin
  FHighestValue := 0;
  FLowestValue := $FFFFFFFF;
  For i := 0 to Length(FBytes) - 1 Do
  Begin
    b := ord(FBytes[i]);
    if b > FHighestValue Then
      FHighestValue := b;
    if b < FLowestValue Then
      FLowestValue := b;
  End;
  inc(FHighestValue);
End;

Procedure TDicomImageExtractor.ScanColor8;
var
  i : integer;
  b : Byte;
Begin
  FHighestValue := 0;
  FLowestValue := $FFFFFFFF;
  For i := 0 to Length(FBytes) - 1 Do
  Begin
    b := ord(FBytes[i]);
    if b > FHighestValue Then
      FHighestValue := b;
    if b < FLowestValue Then
      FLowestValue := b;
  End;
  inc(FHighestValue);
End;

Procedure TDicomImageExtractor.ScanGray16X;
var
  ff, w : word;
  i : integer;
Begin
  ff := (1 shl FBitsStored) - 1;

  FHighestValue := 0;
  FLowestValue := $FFFFFFFF;
  For i := 0 to (Length(FBytes) div 2) - 1 Do
  Begin
    Move(FBytes[(i*2)], w, 2);
    w := w and ff;
    if w > FHighestValue Then
      FHighestValue := w;
    if w < FLowestValue Then
      FLowestValue := w;
  End;
  inc(FHighestValue);
End;

Procedure TDicomImageExtractor.ScanColor16X;
var
  ff, w : word;
  i : integer;
Begin
  ff := (1 shl FBitsStored) - 1;

  FHighestValue := 0;
  FLowestValue := $FFFFFFFF;
  For i := 0 to (Length(FBytes) div 2) - 1 Do
  Begin
    Move(FBytes[(i*2)+1], w, 2);
    w := w and ff;
    if w > FHighestValue Then
      FHighestValue := w;
    if w < FLowestValue Then
      FLowestValue := w;
  End;
  inc(FHighestValue);
End;

Function TDicomImageExtractor.ReadGray8(const iFrameIndex : Integer) : TBytes;
var
  i, iX, iY, iBase : integer;
  b : Byte;
 // ff : word;
Begin
  if length(FBytes) < FWidth * FHeight * (iFrameIndex + 1) Then
    raise EDicomException.create('Insufficient Pixel Data retrieving image '+inttostr(iFrameIndex)+' (0-based)');

//  ff := (1 shl FBitsStored) - 1;
  if not FScaleContrast Or (FHighestValue = 0) Then
    FHighestValue := 1 shl FBitsStored;
  if not FScaleContrast Or (FLowestValue = $FFFFFFFF) Then
    FLowestValue := 0;

  i := FWidth * FHeight * iFrameIndex;
  result := Fillbytes(0, FStride * FHeight);
  For iY := 0 to FHeight - 1 Do
  Begin
    iBase := iY * FStride;
    For iX := 0 to FWidth - 1 Do
    Begin
      b := FBytes[i];
      if FScaleContrast and (b <> 0) Then
        b := Trunc(((ord(b) - FLowestValue) / (FHighestValue - FLowestValue)) * 256);

      result[iBase+(iX*3)+0] := b;
      result[iBase+(iX*3)+1] := b;
      result[iBase+(iX*3)+2] := b;
      inc(i);
    End;
  End;
End;


Function TDicomImageExtractor.ReadGray16X(iFrameIndex : Integer) : TBytes;
var
  i, iX, iY, iBase : integer;
  b : Byte;
  w : Word;
  ff : word;
Begin
  if length(FBytes) < FWidth * FHeight * 2 * (iFrameIndex + 1) Then
    raise EDicomException.create('Insufficient Pixel Data retrieving image '+inttostr(iFrameIndex)+' (0-based)');

  ff := (1 shl FBitsStored) - 1;
  if not FScaleContrast Or (FHighestValue = 0) Then
    FHighestValue := 1 shl FBitsStored;
  if not FScaleContrast Or (FLowestValue = $FFFFFFFF) Then
    FLowestValue := 0;

  i := FWidth * FHeight * 2 * iFrameIndex;
  result := Fillbytes(0, FStride * FHeight);
  For iY := 0 to FHeight - 1 Do
  Begin
    iBase := iY * FStride;
    For iX := 0 to FWidth - 1 Do
    Begin
      Move(FBytes[i], w, 2);
      w := w and ff;
      b := Trunc( ( (w - FLowestValue) / (FHighestValue - FLowestValue)) * 256);
      Move(b, result[iBase+(iX*3)+0], 1);
      Move(b, result[iBase+(iX*3)+1], 1);
      Move(b, result[iBase+(iX*3)+2], 1);
      inc(i, 2);
    End;
  End;
End;


Function TDicomImageExtractor.ReadRGB24(iFrameIndex : Integer) : TBytes;
var
  i, iX, iY, iBase : integer;
Begin
  if length(FBytes) < FWidth * FHeight * 3 * (iFrameIndex + 1) Then
    raise EDicomException.create('Insufficient Pixel Data retrieving image '+inttostr(iFrameIndex)+' (0-based)');

  i := FWidth * FHeight * 3 * iFrameIndex;
  result := Fillbytes(0, FStride * FHeight);
  For iY := 0 to FHeight - 1 Do
  Begin
    iBase := iY * FStride;
    For iX := 0 to FWidth - 1 Do
    Begin
      result[iBase+(iX*3)+0] := FBytes[i+2];
      result[iBase+(iX*3)+1] := FBytes[i+1];
      result[iBase+(iX*3)+2] := FBytes[i+0];
      inc(i, 3);
    End;
  End;
End;

Function TDicomImageExtractor.ReadRGB36(iFrameIndex : Integer) : TBytes;
var
  i, iX, iY, iBase : integer;
  Function Read12(iOffset : Integer) : Byte;
  var
    w : Word;
  Begin
    Move(FBytes[iOffset], w, 2);
    w := w and $FFF;
    result := Trunc( ((w - FLowestValue) / (FHighestValue - FLowestValue)) * 256);
  End;
Begin
  if length(FBytes) < FWidth * FHeight * 6 * (iFrameIndex + 1) Then
    raise EDicomException.create('Insufficient Pixel Data retrieving image '+inttostr(iFrameIndex)+' (0-based)');

  if not FScaleContrast Or (FHighestValue = 0) Then
    FHighestValue := 1 shl FBitsStored;
  if not FScaleContrast Or (FLowestValue = $FFFFFFFF) Then
    FLowestValue := 0;

  i := FWidth * FHeight * 6 * iFrameIndex;
  result := Fillbytes(0, FStride * FHeight);
  For iY := 0 to FHeight - 1 Do
  Begin
    iBase := iY * FStride;
    For iX := 0 to FWidth - 1 Do
    Begin
      result[iBase+(iX*3)+0] := Read12(i+0+4);
      result[iBase+(iX*3)+1] := Read12(i+0+2);
      result[iBase+(iX*3)+2] := Read12(i+0+0);
      inc(i, 6);
    End;
  End;
End;



Procedure TDicomImageExtractor.LoadImage(iIndex : Integer; bAnnotations : Boolean; oImage : TGdiPlusBitmapImage);
var
  oAnnotator : TGdiPlusImageAnnotator;
Begin
  if (iIndex >= FFrameCount) Then
    raise EDicomException.create('Unable to retrieve frame '+inttostr(iIndex)+' of '+inttostr(FFrameCount));

  if (FSamplesPerPixel = 1) Then
  Begin
    if (8 = FBitsAllocated) and (8 = FBitsStored) And (7 = FHighBit) Then
      oImage.LoadFromPixels(FWidth, FHeight, FStride, PixelFormat24bppRGB, ReadGray8(iIndex))
    Else if (FBitsAllocated = 16) And (FBitsStored <= FBitsAllocated) And (FHighBit = FBitsStored - 1)  Then
      oImage.LoadFromPixels(FWidth, FHeight, FStride, PixelFormat24bppRGB, ReadGray16X(iIndex))
    Else
      raise EDicomException.create(StringFormat('not done yet (spp = %d, Ba = %d, Bs = %d, Bh = %d)', [FSamplesPerPixel, FBitsAllocated, FBitsStored, FHighBit]));
  End
  Else if (3 = FSamplesPerPixel) Then
  Begin
    if FPlanarConfiguration > 0 then
      raise EDicomException.create('Planar Configuration > 0 is not supported');

    If (8 = FBitsAllocated) and (8 = FBitsStored) And (7 = FHighBit) Then
      oImage.LoadFromPixels(FWidth, FHeight, FStride, PixelFormat24bppRGB, ReadRGB24(iIndex))
    Else If (16 = FBitsAllocated) and (12 = FBitsStored) And (11 = FHighBit) Then
      oImage.LoadFromPixels(FWidth, FHeight, FStride, PixelFormat24bppRGB, ReadRGB36(iIndex))
    else
      raise EDicomException.create(StringFormat('not done yet (spp = %d, Ba = %d, Bs = %d, Bh = %d)', [FSamplesPerPixel, FBitsAllocated, FBitsStored, FHighBit]));
  End
  Else
    raise EDicomException.create(StringFormat('not done yet (spp = %d, Ba = %d, Bs = %d, Bh = %d)', [FSamplesPerPixel, FBitsAllocated, FBitsStored, FHighBit]));

  if bAnnotations Then
  Begin
    oAnnotator := TGdiPlusImageAnnotator.Create;
    Try
      oAnnotator.Image := oImage.Link;
      oAnnotator.FontName := 'Arial';
      oAnnotator.FontColour := argbLime;
      oAnnotator.Start(25);

      if FInstanceId <> ' ' Then
        oAnnotator.Annotate('Id: '+FInstanceId);
      if FDateTime<> ' ' Then
        oAnnotator.Annotate('Date: '+FDateTime);
      if FAccessionNumber <> ' ' Then
        oAnnotator.Annotate('Accession: '+FAccessionNumber);
      if FModality<> ' ' Then
        oAnnotator.Annotate('Modality: '+FModality);
      if FPatientName<> ' ' Then
        oAnnotator.Annotate('Name: '+FPatientName);
      if FPatientDOB<> ' ' Then
        oAnnotator.Annotate('DOB: '+FPatientDOB);
      if FPatientSex<> ' ' Then
        oAnnotator.Annotate('Sex: '+FPatientSex);

    Finally
      oAnnotator.Free;
    End;
  End;
end;


procedure TDicomImageExtractor.Close;
begin
  FInstance.Free;
  FInstance := nil;
  SetLength(FBytes, 0);
  FRoot.Free;
  FRoot := nil;
end;

Function TDicomImageExtractor.LoadJpegLS(oElem : TDicomDataElement) : TBytes;
var
  i : integer;
  iLength : Cardinal;
  aBytes, t : TBytes;
  pData: Pointer;
  iDataLen: Integer;
  bOk: Boolean;
  resultCode : Cardinal;
Begin
  SetLength(t, 0);
  iLength := 0;
  for i := 0 to oElem.Values.Count - 1 do
    inc(iLength, Length(oElem.Values[i].AsOB));
  SetLength(aBytes, iLength);
  iLength := 0;
  for i := 0 to oElem.Values.Count - 1 do
  Begin
    t := oElem.Values[i].AsOB;
    Move(t[1], aBytes[1+iLength], Length(t));
    inc(iLength, length(t));
  End;

  LoadDll;
  Try
    // specified size post decoding
    iDataLen := FWidth * FHeight * FBitsAllocated * FSamplesPerPixel * FFrameCount;
    resultCode := 0;
    GetMem(pData, iDataLen);
    Try

      bOk := JpegLSDecode (pData, iDataLen, @resultCode, @aBytes[1], length(aBytes), FWidth, FHeight, FBitsStored, FFrameCount);
      If bOk Then
      Begin
        SetLength(aBytes, iDataLen);
        Move(pData^, aBytes[1], iDataLen);
        Result := aBytes;
      End;
    Finally
      FreeMem(pData);
    End;
  Finally
    UnloadDll;
  End;

  If Not bOk Then
    raise EDicomException.create('JPEG/MPEG Encoding not yet supported');
End;


{ Procedure TDicomImageExtractor.HandleJpeg;
Var
  pData: Pointer;
  iDataLen: Integer;
  bOk: Boolean;
  resultCode : Cardinal;
  sTestData: String;
Begin
  LoadDll;
  Try
    // debug input
    StringToFile(FBytes, 'c:\temp\dump.jpg');

    // loading test data
    //sTestData := FileToString('C:\Temp\DicomImage.bmp');
    //FWidth := 640;
    //FHeight := 480;
    //FStride := 1960;
    //FBitsAllocated := 8;
    //FBitsStored := 8;
    //FHighBit := 8 - 1;
    //FFrameCount := 1;
    //iDataLen := Length(sTestData);


    // Not sure how this formular is arrived
    iDataLen := FWidth * FHeight * ((FBitsStored + 7) Div 8) * FFrameCount;
    GetMem(pData, iDataLen);
    resultCode := 0;
    Try

      bOk := JpegLSDecode (pData, iDataLen, @resultCode, @FBytes[17], length(FBytes)-34, FWidth, FHeight, FBitsStored, FFrameCount);
      //bOk := JpegLSDecode (pData, iDataLen, @resultCode, @sTestData[1], length(sTestData), FWidth, FHeight, FBitsStored, FFrameCount);
      //bOk := True;
      If bOk Then
      Begin
        SetLength(FBytes, iDataLen);
        Move(pData^, FBytes[1], iDataLen);
        // debug output
        StringToFile(FBytes, 'c:\temp\dump2.jpg');
      End;
    Finally
      FreeMem(pData);
    End;
  Finally
    UnloadDll;
  End;
  If Not bOk Then
    raise EDicomException.create('JPEG/MPEG Encoding not yet supported');
End; }

function TDicomImageExtractor.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FInstance.sizeInBytes);
  inc(result, FRoot.sizeInBytes);
  inc(result, (FTransferSyntax.length * sizeof(char)) + 12);
  inc(result, (FInstanceId.length * sizeof(char)) + 12);
  inc(result, (FDateTime.length * sizeof(char)) + 12);
  inc(result, (FAccessionNumber.length * sizeof(char)) + 12);
  inc(result, (FModality.length * sizeof(char)) + 12);
  inc(result, (FPatientName.length * sizeof(char)) + 12);
  inc(result, (FPatientDOB.length * sizeof(char)) + 12);
  inc(result, (FPatientSex.length * sizeof(char)) + 12);
end;

End.
