Unit wp_imaging;


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
  Windows, SysUtils,
  Graphics,
  fsl_base, fsl_utilities, fsl_stream,
  dicom_Dictionary, dicom_Objects, dicom_Parser,

  wp_graphics, wp_gdiplus, wp_loader_dicom,
  wp_types, wp_document, wp_working;

Const
  ICO_TRANSPARENT_COLOUR = clOlive;

Type
  TWPImageLoader = Class (TFslObject)
  Private
    FFilename : String;
    FSource : TFslAccessStream;
    FFormat : TWPImageFormat;
    FDicomDictionary: TDicomDictionary;

    Function GetSource : TFslAccessStream;
    Procedure SetSource(Const Value : TFslAccessStream);

    Function StreamIsBMP : Boolean;
    Function StreamIsICO : Boolean;
    Function StreamIsDICOM : Boolean;
    Function LoadAndConvertIcon : TFslBitmapGraphic;
    Function LoadAndConvertDicom : TFslBitmapGraphic;
    Procedure WriteIcon(oIcon : TIcon; oBmp : TBitmap);
    procedure SetDicomDictionary(const Value: TDicomDictionary);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;

    Function Load : TFslGraphic; Overload; Virtual;

    Class Function Wrap(oImage : TFslGraphic) : TWPDocumentImage; Overload; Virtual;
    Class Function WrapWorking(oImage : TFslGraphic) : TWPWorkingDocumentImagePiece; Overload; Virtual;

    Function HasSource : Boolean;

    Procedure Reset; Overload; Virtual;

    Property Format : TWPImageFormat Read FFormat Write FFormat;
    Property Filename : String Read FFilename Write FFilename;
    Property Source : TFslAccessStream Read GetSource Write SetSource;
    Property DicomDictionary : TDicomDictionary read FDicomDictionary write SetDicomDictionary;
  End;


Implementation

Uses
  GraphicEx;

{ TWPImageLoader }


Destructor TWPImageLoader.Destroy;
Begin
  FDicomDictionary.Free;
  FSource.Free;
  Inherited Destroy;
End;


Function TWPImageLoader.GetSource : TFslAccessStream;
Begin
  Assert(Invariants('GetSource', FSource, TFslAccessStream, 'Source'));
  Result := FSource;
End;


procedure TWPImageLoader.SetDicomDictionary(const Value: TDicomDictionary);
begin
  FDicomDictionary.Free;
  FDicomDictionary := Value;
end;


Procedure TWPImageLoader.SetSource(Const Value : TFslAccessStream);
Begin
  FSource.Free;
  FSource := Value;
End;


Function TWPImageLoader.HasSource : Boolean;
Begin
  Result := Assigned(FSource);
End;


Function TWPImageLoader.StreamIsBMP : Boolean;
Var
  sHeader : AnsiString;
Begin
  SetLength(sHeader, IntegerMin(2, Source.Size - Source.Position));
  If Length(sHeader) > 0 Then
    Source.Read(sHeader[1], Length(sHeader));
  Source.Position := Source.Position - 2;
  Result := sHeader = 'BM';
End;


function TWPImageLoader.StreamIsDICOM: Boolean;
Var
  sHeader : AnsiString;
Begin
  SetLength(sHeader, IntegerMin(300, Source.Size - Source.Position));
  If Length(sHeader) > 0 Then
    Source.Read(sHeader[1], Length(sHeader));
  Source.Position := Source.Position - 300;
  sHeader := copy(sHeader, 129, 4);
  Result := sHeader = 'DICM';
end;

Function TWPImageLoader.StreamIsICO : Boolean;
Begin
  Result := False;
End;


function TWPImageLoader.LoadAndConvertDicom: TFslBitmapGraphic;
var
  dicom : TDicomInstance;
  parser : TDicomInstanceDecoder;
  imp : TDicomImageExtractor;
  image : TGdiPlusBitmapImage;
begin
  result := nil;
  parser := TDicomInstanceDecoder.Create(Source.Size);
  try
    parser.Context.Input := FSource.Link;
    parser.Context.Dictionary := FDicomDictionary.Link;
    dicom := parser.Execute;
    try
      imp := TDicomImageExtractor.Create;
      try
        imp.ScaleContrast := true;
        imp.Open(dicom);
        if imp.ImageCount > 1 then
          raise EWPException.create('Multiple images not supported');
        image := TGdiPlusBitmapImage.create;
        try
          imp.LoadImage(0, false, image);
          result := TFslBitmapGraphic.create;
          try
            image.PopulateBitmapGraphic(result);
            result.Link;
          finally
            result.Free;
          end;
        finally
          image.Free;
        end;
      finally
        imp.Free;
      end;
    finally
      dicom.Free;
    end;
  finally
    parser.Free;
  end;
end;

Function TWPImageLoader.LoadAndConvertIcon : TFslBitmapGraphic;
Var
  oIcon : TFslIconGraphic;
Begin
  oIcon := TFslIconGraphic.Create;
  Try
    oIcon.LoadFromStream(FSource);
    TIcon(oIcon.Handle).Handle; // workaround for icon problem - height and width are wrong until the handle is used.
    Result := TFslBitmapGraphic.Create();
    Try
      Result.Height := oIcon.Height;
      Result.Width := oIcon.Width;
      Result.Transparent := True;
      Result.Handle.TransparentColor := ICO_TRANSPARENT_COLOUR;
      WriteIcon(oIcon.Icon, Result.Handle);
      Result.Link;
    Finally
      Result.Free;
    End;
  Finally
    oIcon.Free;
  End;
End;

Procedure TWPImageLoader.WriteIcon(oIcon : TIcon; oBmp : TBitmap);
Var
  aIconInfo : TIconInfo;
  abmMaskInfo : Windows.TBitmap;
  abmColorInfo : Windows.TBitmap;
  oImage : TBitmap;
Const
  ROP_DstCopy = $00AA0029;
Begin
  FillChar(abmMaskInfo, SizeOf(abmMaskInfo), #0);
  FillChar(abmColorInfo, SizeOf(abmColorInfo), #0);
  FillChar(aIconInfo, SizeOf(aIconInfo), #0);
  GetIconInfo(oIcon.Handle, aIconInfo);
  If (aIconInfo.hbmMask <> 0) Then
    GetObject(aIconInfo.hbmMask, SizeOf(abmMaskInfo), @abmMaskInfo);
  If (aIconInfo.hbmColor <> 0) Then
    GetObject(aIconInfo.hbmColor, SizeOf(abmColorInfo), @abmColorInfo);

  oBmp.Canvas.Lock;
  Try
    oBmp.Canvas.Pen.Style := psSolid;
    oBmp.Canvas.Pen.Color := ICO_TRANSPARENT_COLOUR;
    oBmp.Canvas.Brush.Color := ICO_TRANSPARENT_COLOUR;
    oBmp.Canvas.Rectangle(0,0, oBmp.Width, oBmp.Height);
    oImage := TBitmap.Create;
    Try
      oImage.Canvas.Lock;
      Try
        oImage.Handle := aIconInfo.hbmColor;

        Win32Check(MaskBlt(oBmp.Canvas.Handle, 0, 0, oIcon.Width, oIcon.Height, oImage.Canvas.Handle, 0, 0, aIconInfo.hbmMask, 0, 0, MakeRop4(ROP_DstCopy, SRCCOPY)));
      Finally
        oImage.Canvas.Unlock;
      End;
    Finally
      oImage.Free;
    End;
  Finally
    oBmp.Canvas.Unlock;
  End;

  DeleteObject(aIconInfo.hbmMask);
  DeleteObject(aIconInfo.hbmColor);
End;

Function TWPImageLoader.Load : TFslGraphic;
Var
  aGraphicClass: TGraphicExGraphicClass;
  oVCLStream : TVCLStream;
  aFormat : TWPImageFormat;
Begin
  If Not HasSource Then
    Source := TFslFile.Create(FFilename, fmOpenRead + fmShareDenyWrite);

  aGraphicClass := Nil;
  aFormat := FFormat;

  If aFormat = ifUnknown Then
  Begin
    oVCLStream := TVCLStream.Create;
    Try
      oVCLStream.Stream := Source.Link;

      aGraphicClass := FileFormatList.GraphicFromContent(oVCLStream);
    Finally
      oVCLStream.Free;
    End;

    If Not Assigned(aGraphicClass) Then
    Begin
      If StringArrayExistsInsensitive(['.jpg', '.jpeg'], PathExtension(FFileName)) Then
        aFormat := ifJPEG
      Else If StringArrayExistsInsensitive(['.bmp'], PathExtension(FFileName)) Or StreamIsBMP Then
        aFormat := ifBMP
      Else If StringArrayExistsInsensitive(['.ico'], PathExtension(FFileName)) Or StreamIsICO Then
        aFormat := ifICO
      Else If StringArrayExistsInsensitive(['.png'], PathExtension(FFileName)) Then
        aFormat := ifPNG
      Else If StringArrayExistsInsensitive(['.gif'], PathExtension(FFileName)) Then
        aFormat := ifGIF
      Else If StringArrayExistsInsensitive(['.pdf'], PathExtension(FFileName)) Then
        aFormat := ifPDF
      Else If StringArrayExistsInsensitive(['.dcm', '.dicom'], PathExtension(FFileName)) Or StreamIsDICOM Then
        aFormat := ifDicom
      Else
        RaiseError('Load', 'Unable to determine Image type for "'+FFileName+'"');
    End;
  End;

  Case aFormat Of
    ifDicom :
      Result := LoadAndConvertDicom;
    ifJPEG :
    Begin
      Result := TFslJpegGraphic.Create;
    End;

    ifICO :
      Result := LoadAndConvertIcon;

    ifPNG :
    Begin
      Result := TFslBitmapGraphic.Create;
      TFslBitmapGraphic(Result).Handle := TPNGGraphic.Create;
    End;

    ifBMP :
    Begin
      Result := TFslBitmapGraphic.Create;
    End;
  Else {ifUnknown : }
    Assert(CheckCondition(Assigned(aGraphicClass), 'Load', 'Internal Load Sequence Error - image type unknown'));

    Result := TFslBitmapGraphic.Create;
    TFslBitmapGraphic(Result).Handle := aGraphicClass.Create;
  End;
  Try
    If not (aFormat in [ifICO, ifDicom]) Then
      Result.LoadFromStream(Source);
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Procedure TWPImageLoader.Reset;
Begin
  Filename := '';
  Source := Nil;
  Format := ifUnknown;
End;


function TWPImageLoader.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FFilename.length * sizeof(char)) + 12);
  inc(result, FSource.sizeInBytes);
  inc(result, FDicomDictionary.sizeInBytes);
end;

Class Function TWPImageLoader.Wrap(oImage : TFslGraphic) : TWPDocumentImage;
Var
  oBmp : TFslBitmapGraphic;
Begin
  Result := TWPDocumentImage.Create;
  Try
    if not (oImage is TFslVCLGraphic) then
      result.SizePolicy := ImageSizeWholePage;
    Result.ImageHeight := oImage.Height;
    Result.ImageWidth := oImage.Width;
    If (oImage Is TFslBitmapGraphic) Then
    Begin
      oBmp := TFslBitmapGraphic(oImage);
      If (oBmp.Transparent) Then
         Result.TransparentColour := oBmp.Handle.TransparentColor;
    End;

    Result.Image := oImage.Link;

    Result.Link;
  Finally
    Result.Free;
  End;
End;


Class Function TWPImageLoader.WrapWorking(oImage : TFslGraphic) : TWPWorkingDocumentImagePiece;
Var
  oBmp : TFslBitmapGraphic;
Begin
  Result := TWPWorkingDocumentImagePiece.Create;
  Try
    Result.Border := 0;
    If (oImage Is TFslIconGraphic) Then
      TIcon(TFslIconGraphic(oImage).Handle).Handle; // workaround for icon problem - height and width are wrong until the handle is used.

    Result.Height := oImage.Height;
    Result.Width := oImage.Width;
    If (oImage Is TFslBitmapGraphic) Then
    Begin
      oBmp := TFslBitmapGraphic(oImage);
      If (oBmp.Transparent) Then
         Result.TransparentColour := oBmp.Handle.TransparentColor;
    End;
    Result.Image := oImage.Link;

    Result.Link;
  Finally
    Result.Free;
  End;
End;


End.


