{! 1 !}

unit DropBMPSource;

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
// Acknowledgements:
// Thanks to Dieter Steinwedel for some help with DIBs.
// http://godard.oec.uni-osnabrueck.de/student_home/dsteinwe/delphi/DietersDelphiSite.htm
// -----------------------------------------------------------------------------

interface

uses
  DropSource,
  Classes, Graphics, ActiveX;

{$include DragDrop.inc}

type
  TDropBMPSource = class(TDropSource)
  private
    fBitmap: TBitmap;
    procedure SetBitmap(Bmp: TBitmap);
  protected
    function DoGetData(const FormatEtcIn: TFormatEtc; OUT Medium: TStgMedium):HRESULT; Override;
  public
    constructor Create(aOwner: TComponent); Override;
    destructor Destroy; Override;
    function CutOrCopyToClipboard: boolean; Override;
  published
    property Bitmap: TBitmap Read fBitmap Write SetBitmap;
  end;

procedure Register;

implementation

uses
  Windows,
  SysUtils,
  ClipBrd;

procedure Register;
begin
  RegisterComponents('DragDrop', [TDropBMPSource]);
end;

// -----------------------------------------------------------------------------
//     Miscellaneous DIB Function
// -----------------------------------------------------------------------------

function GetHGlobalDIBFromBitmap(Bitmap: Graphics.TBitmap): HGlobal;
var
  Stream: TMemoryStream;
  DIB: pointer;
  DIBSize: integer;
  bfh: TBitmapFileHeader;
begin
  Stream := TMemoryStream.Create;
  try
    //let Graphics.pas do the work...
    Bitmap.SaveToStream(Stream);
    //BitmapFileHeader will be discarded
    DIBSize := Stream.Size - sizeof(TBitmapFileHeader);
    Result:=GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT or GMEM_SHARE, DIBSize);
    if Result = 0 then exit;
    DIB := GlobalLock(Result);
    if DIB = nil then
    begin
      GlobalFree(Result);
      Result := 0;
    end else
    begin
      Stream.Seek(0,soFromBeginning);
      //skip BitmapFileHeader...
      Stream.readbuffer(bfh,sizeof(TBitmapFileHeader));
      //copy data...
      Stream.readbuffer(DIB^,DIBSize);
      GlobalUnlock(Result);
    end;
  finally
    Stream.free;
  end;
end;

// -----------------------------------------------------------------------------
//      TDropBMPSource
// -----------------------------------------------------------------------------

constructor TDropBMPSource.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fBitmap := Graphics.TBitmap.Create;
  DragTypes := [dtCopy]; // Default to Copy

  AddFormatEtc(CF_BITMAP, NIL, DVASPECT_CONTENT, -1, TYMED_GDI);
  AddFormatEtc(CF_PALETTE, NIL, DVASPECT_CONTENT, -1, TYMED_GDI);
  AddFormatEtc(CF_DIB, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
end;
// ----------------------------------------------------------------------------- 

destructor TDropBMPSource.destroy;
begin
  fBitmap.Free;
  inherited Destroy;
end;
// ----------------------------------------------------------------------------- 

procedure TDropBMPSource.SetBitmap(Bmp: Graphics.TBitmap);
begin
  fBitmap.assign(Bmp);
end;
// ----------------------------------------------------------------------------- 

function TDropBMPSource.CutOrCopyToClipboard: boolean;
var
  data: HGlobal;
begin
  result := false;
  if fBitmap.empty then exit;
  try
    data := GetHGlobalDIBFromBitmap(fBitmap);
    if data = 0 then exit;
    Clipboard.SetAsHandle(CF_DIB,data);
    result := true;
  except
    raise ELibraryException.create('Unable to copy BMP to clipboard.');
  end;
end;
// ----------------------------------------------------------------------------- 

function TDropBMPSource.DoGetData(const FormatEtcIn: TFormatEtc; OUT Medium: TStgMedium):HRESULT;
var
  fmt: WORD;
  pal: HPALETTE;
begin

  Medium.tymed := 0;
  Medium.UnkForRelease := nil;
  Medium.HGlobal := 0;
  //--------------------------------------------------------------------------
  if not fBitmap.empty and
     (FormatEtcIn.cfFormat = CF_DIB) and
     (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
     (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
  begin
    try
      Medium.HGlobal := GetHGlobalDIBFromBitmap(fBitmap);
      if Medium.HGlobal <> 0 then
      begin
        Medium.tymed := TYMED_HGLOBAL;
        result := S_OK
      end else
        result := E_OUTOFMEMORY;
    except
      result := E_OUTOFMEMORY;
    end;
  end
  //--------------------------------------------------------------------------
  else if not fBitmap.empty and
     (FormatEtcIn.cfFormat = CF_BITMAP) and
     (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
     (FormatEtcIn.tymed and TYMED_GDI <> 0) then
  begin
    try
      //This next line just gets a copy of the bitmap handle...
      fBitmap.SaveToClipboardFormat(fmt, THandle(Medium.hBitmap), pal);
      if pal <> 0 then DeleteObject(pal);
      if Medium.hBitmap <> 0 then
      begin
        Medium.tymed := TYMED_GDI;
        result := S_OK
      end else
        result := E_OUTOFMEMORY;
    except
      result := E_OUTOFMEMORY;
    end;
  end
  //--------------------------------------------------------------------------
  else if not fBitmap.empty and
     (FormatEtcIn.cfFormat = CF_PALETTE) and
     (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
     (FormatEtcIn.tymed and TYMED_GDI <> 0) then
  begin
    try
      Medium.hBitmap := CopyPalette(fBitmap.palette);
      if Medium.hBitmap <> 0 then
      begin
        Medium.tymed := TYMED_GDI;
        result := S_OK
      end else
        result := E_OUTOFMEMORY;
    except
      result := E_OUTOFMEMORY;
    end {try}
  end else
    result := DV_E_FORMATETC;
end;
// ----------------------------------------------------------------------------- 
// ----------------------------------------------------------------------------- 

end.
