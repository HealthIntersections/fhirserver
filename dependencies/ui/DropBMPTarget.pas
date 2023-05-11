unit DropBMPTarget;

// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite
// Component Names: TDropBMPSource
// Module:          DropBMPSource
// Description:     Implements Dragging & Dropping of Bitmaps
//                  FROM your application to another.
// Version:         3.7
// Date:            22-JUL-1999
// Target:          Win32, Delphi 3 - Delphi 5, C++ Builder 3, C++ Builder 4
// Authors:         Angus Johnson,   ajohnson@rpi.net.au
//                  Anders Melander, anders@melander.dk
//                                   http://www.melander.dk
// Copyright        © 1997-99 Angus Johnson & Anders Melander
// -----------------------------------------------------------------------------

interface

uses
  DropSource,
  DropTarget,
  Windows, Classes, Graphics, ActiveX;

{$include DragDrop.inc}

type
  TDropBMPTarget = class(TDropTarget)
  private
    fBitmap: TBitmap;
  protected
    procedure ClearData; override;
    function DoGetData: boolean; override;
    function HasValidFormats: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Bitmap: TBitmap Read fBitmap;
  end;

procedure Register;
procedure CopyDIBToBitmap(Bitmap: TBitmap; BitmapInfo: PBitmapInfo; DIBSize: integer);

implementation

const
  DIBFormatEtc: TFormatEtc = (cfFormat: CF_DIB;
    ptd: nil; dwAspect: DVASPECT_CONTENT; lindex: -1; tymed: TYMED_HGLOBAL);
  BMPFormatEtc: TFormatEtc = (cfFormat: CF_BITMAP;
    ptd: nil; dwAspect: DVASPECT_CONTENT; lindex: -1; tymed: TYMED_GDI);
  PalFormatEtc: TFormatEtc = (cfFormat: CF_PALETTE;
    ptd: nil; dwAspect: DVASPECT_CONTENT; lindex: -1; tymed: TYMED_GDI);

procedure Register;
begin
  RegisterComponents('DragDrop', [TDropBMPTarget]);
end;

// -----------------------------------------------------------------------------
//  Miscellaneous DIB Function
// -----------------------------------------------------------------------------

procedure CopyDIBToBitmap(Bitmap: TBitmap; BitmapInfo: PBitmapInfo; DIBSize: integer);
var
  BitmapFileHeader  : TBitmapFileHeader;
  FileSize    : integer;
  InfoSize    : integer;
  Stream    : TMemoryStream;
begin
  // Write DIB to a stream in the BMP file format
  Stream := TMemoryStream.Create;
  try
    FileSize := sizeof(TBitmapFileHeader) + DIBSize;
    InfoSize := sizeof(TBitmapInfoHeader);
    if (BitmapInfo^.bmiHeader.biBitCount > 8) then
    begin
      if ((BitmapInfo^.bmiHeader.biCompression and BI_BITFIELDS) <> 0) then
        Inc(InfoSize, 12);
    end else
      Inc(InfoSize, sizeof(TRGBQuad) * (1 shl BitmapInfo^.bmiHeader.biBitCount));
    Stream.SetSize(FileSize);
    // Initialize file header
    FillChar(BitmapFileHeader, sizeof(TBitmapFileHeader), 0);
    with BitmapFileHeader do
    begin
      bfType := $4D42; // 'BM' = Windows BMP signature
      bfSize := FileSize; // File size (not needed)
      bfOffBits := sizeof(TBitmapFileHeader) + InfoSize; // Offset of pixel data
    end;
    // Save file header
    Stream.Write(BitmapFileHeader, sizeof(TBitmapFileHeader));
    // Save TBitmapInfo structure and pixel data
    Stream.Write(BitmapInfo^, DIBSize);

    // Rewind and load bitmap from stream
    Stream.Position := 0;
    Bitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

// -----------------------------------------------------------------------------
//      TDropBMPTarget
// -----------------------------------------------------------------------------

constructor TDropBMPTarget.Create( AOwner: TComponent );
begin
   inherited Create( AOwner );
   fBitmap := TBitmap.Create;
end;
// -----------------------------------------------------------------------------

destructor TDropBMPTarget.Destroy;
begin
  fBitmap.Free;
  inherited Destroy;
end;
// ----------------------------------------------------------------------------- 

function TDropBMPTarget.HasValidFormats: boolean;
begin
  result := (DataObject.QueryGetData(DIBFormatEtc) = S_OK) or
                (DataObject.QueryGetData(BMPFormatEtc) = S_OK);
end;
// ----------------------------------------------------------------------------- 

procedure TDropBMPTarget.ClearData;
begin
  fBitmap.handle := 0;
end;
// ----------------------------------------------------------------------------- 

function TDropBMPTarget.DoGetData: boolean;
var
  medium, medium2: TStgMedium;
  DIBData: pointer;
begin
  result := false;

  fBitmap.Canvas.Lock;
  Try
    //--------------------------------------------------------------------------
    if (DataObject.GetData(DIBFormatEtc, medium) = S_OK) then
    begin
      if (medium.tymed = TYMED_HGLOBAL) then
      begin
        DIBData := GlobalLock(medium.HGlobal);
        try
          CopyDIBToBitmap(fBitmap, DIBData, GlobalSize(Medium.HGlobal));
          result := true;
        finally
          GlobalUnlock(medium.HGlobal);
        end;
      end;
      ReleaseStgMedium(medium);
    end
    //--------------------------------------------------------------------------
    else if (DataObject.GetData(BMPFormatEtc, medium) = S_OK) then
    begin
      try
        if (medium.tymed <> TYMED_GDI) then exit;
        if (DataObject.GetData(PalFormatEtc, medium2) = S_OK) then
        begin
          try
            fBitmap.LoadFromClipboardFormat(CF_BITMAP, Medium.hBitmap, Medium2.hBitmap);
          finally
            ReleaseStgMedium(medium2);
          end;
        end
        else
          fBitmap.LoadFromClipboardFormat(CF_BITMAP, Medium.hBitmap, 0);
        result := true;
      finally
        ReleaseStgMedium(medium);
      end;
    end
    //--------------------------------------------------------------------------
    else
      result := false;
  Finally
    fBitmap.Canvas.Unlock;
  End;
end;
// ----------------------------------------------------------------------------- 
// ----------------------------------------------------------------------------- 

end.
