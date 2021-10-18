unit fsl_qrcode;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Graphics,
  uQRC,
  fsl_base;

Type
  TQRCodeSegmentMode = (qrmBytes, qrmAlpha, qrmNumeric);

  TQRCodeGenerator = class (TFslObject)
  private
    FData, FBuffer : TBytes;
    FSegData : TArray<TBytes>;
    FSegments : TSegments;
  public
    // segments
    procedure start;
    procedure addSegment(mode : TQRCodeSegmentMode; data : String);
    procedure finish(bmp : TBitmap);

    // no segments
    procedure generate(data : String; bmp : TBitMap);
  end;

implementation


procedure TQRCodeGenerator.start;
begin
  SetLength(FData, qrcodegen_BUFFER_LEN_MAX);
  SetLength(FBuffer, qrcodegen_BUFFER_LEN_MAX);
  SetLength(FSegData, 0);
  SetLength(FSegments, 0);
end;

procedure TQRCodeGenerator.addSegment(mode : TQRCodeSegmentMode; data : String);
var
  i : integer;
  d : TBytes;
begin
  i := length(FSegments);
  SetLength(FSegments, i+1);
  SetLength(FSegData, i+1);
  case mode of
    qrmBytes :
      begin
        d := TENcoding.UTF8.getBytes(data);
        SetLength(FSegData[i], qrcodegen_calcSegmentBufferSize(qrcodegen_Mode_BYTE, Length(d)) * sizeof(byte));
        FSegments[i] := qrcodegen_makeBytes(d, FSegData[i]);
      end;
    qrmAlpha :
      begin
        SetLength(FSegData[i], qrcodegen_calcSegmentBufferSize(qrcodegen_Mode_ALPHANUMERIC, Length(data)) * sizeof(byte));
        FSegments[i] := qrcodegen_makeAlphanumeric(data, FSegData[i]);
      end;
    qrmNumeric :
      begin
        SetLength(FSegData[i], qrcodegen_calcSegmentBufferSize(qrcodegen_Mode_NUMERIC, Length(data)) * sizeof(byte));
        FSegments[i] := qrcodegen_makeNumeric(data, FSegData[i]);
      end;
    else
      raise EFslException.Create('Unsupported QR Code mode '+inttostr(ord(mode)));
  end;
end;

procedure TQRCodeGenerator.finish(bmp : TBitmap);
var
  size : integer;
  x, y : integer;
const
  hdr = 2;
begin
  if not qrcodegen_encodeSegments(FSegments, {sizeof(segs) / sizeof(segs[0]), }qrcodegen_Ecc_LOW, FBuffer, FData) then
    raise EFslException.Create('QR Code generation failed');
	size := qrcodegen_getSize(FData);
  bmp.SetSize ((hdr * 2 + size) * 4, (hdr * 2 + size) * 4);
  bmp.Canvas.Brush.Color := clWhite;
  bmp.Canvas.FillRect (Rect (0, 0, bmp.Width - 1, bmp.Height - 1));
	for y := 0 to size - 1 do
  begin
    for x := 0 to size - 1 do
    begin
      if qrcodegen_getModule(FData, x, y) then
        bmp.Canvas.Brush.Color := clBlack
      else
        bmp.Canvas.Brush.Color := clWhite;
      bmp.Canvas.FillRect (Rect ((hdr + x) * 4, (hdr + y) * 4, (hdr + x + 1) * 4, (hdr + y + 1) * 4));
    end;
  end;
end;

procedure TQRCodeGenerator.generate(data : String; bmp : TBitMap);
begin
  raise EFslException.Create('Not done yet');
end;
end.

