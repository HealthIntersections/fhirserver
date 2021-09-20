unit QlpConverters;

{$I QRCodeGenLib.inc}

interface

uses
  SysUtils,
  QlpGuard,
  QlpQRCodeGenLibTypes;

resourcestring
  SEncodingInstanceNil = 'Encoding instance cannot be nil';

type
  TConverters = class sealed(TObject)

  strict private
{$IF DEFINED(VCL_OR_LCL)}
    /// <summary>
    /// Convert a Delphi/Lazarus <c>TColor</c> to <c>HTML</c> Color code in
    /// Hex <c>.</c>
    /// </summary>
    /// <param name="AColor">
    /// the <c>TColor</c> to convert
    /// </param>
    /// <returns>
    /// returns a string containing the <c>HTML</c> Color code representation
    /// of the <c>TColor</c> parameter in Hex
    /// </returns>
    class function TColorToHTMLColorHex(const AColor: TQRCodeGenLibColor)
      : String; inline;

    class function HTMLColorHexToTColor(const AHTMLHexColor: String)
      : TQRCodeGenLibColor; inline;

{$ELSEIF DEFINED(FMX)}
    /// <summary>
    /// Convert a Delphi FireMonkey <c>TAlphaColor</c> to <c>HTML</c> Color code in
    /// Hex <c>.</c>
    /// </summary>
    /// <param name="AColor">
    /// the <c>TAlphaColor</c> to convert
    /// </param>
    /// <returns>
    /// returns a string containing the <c>HTML</c> Color code representation
    /// of the <c>TAlphaColor</c> parameter in Hex
    /// </returns>
    class function TAlphaColorToHTMLColorHex(const AColor: TQRCodeGenLibColor)
      : String; inline;

    class function HTMLColorHexToTAlphaColor(const AHTMLHexColor: String)
      : TQRCodeGenLibColor; inline;

{$ELSEIF DEFINED(FCL)}
    /// <summary>
    /// Convert an FPC <c>TFPColor</c> to <c>HTML</c> Color code in
    /// Hex <c>.</c>
    /// </summary>
    /// <param name="AColor">
    /// the <c>TFPColor</c> to convert
    /// </param>
    /// <returns>
    /// returns a string containing the <c>HTML</c> Color code representation
    /// of the <c>TColor</c> parameter in Hex
    /// </returns>
    class function TFPColorToHTMLColorHex(const AColor: TQRCodeGenLibColor)
      : String; inline;

    class function HTMLColorHexToTFPColor(const AHTMLHexColor: String)
      : TQRCodeGenLibColor; inline;
{$IFEND VCL_OR_LCL}
  public

{$IFDEF VCL_OR_LCL}
    class function GetRValue(Argb: UInt32): Byte; static; inline;
    class function GetGValue(Argb: UInt32): Byte; static; inline;
    class function GetBValue(Argb: UInt32): Byte; static; inline;
    class function RGB(Ar, Ag, Ab: Byte): TQRCodeGenLibColor; static; inline;
{$ENDIF VCL_OR_LCL}
    class function ConvertStringToBytes(const AInput: String;
      const AEncoding: TEncoding): TQRCodeGenLibByteArray; static;

    class function ConvertBytesToString(const AInput: TQRCodeGenLibByteArray;
      const AEncoding: TEncoding): String; static;

    class function QRCodeGenLibColorToHTMLHexColor(const AColor
      : TQRCodeGenLibColor): String; inline;

    class function HTMLHexColorToQRCodeGenLibColor(const AHTMLHexColor: String)
      : TQRCodeGenLibColor; inline;
  end;

implementation

{ TConverters }

class function TConverters.ConvertStringToBytes(const AInput: String;
  const AEncoding: TEncoding): TQRCodeGenLibByteArray;
begin
  TGuard.RequireNotNull(AEncoding, SEncodingInstanceNil);
{$IFDEF FPC}
  result := AEncoding.GetBytes(UnicodeString(AInput));
{$ELSE}
  result := AEncoding.GetBytes(AInput);
{$ENDIF FPC}
end;

class function TConverters.ConvertBytesToString(const AInput
  : TQRCodeGenLibByteArray; const AEncoding: TEncoding): String;
begin
  TGuard.RequireNotNull(AEncoding, SEncodingInstanceNil);
{$IFDEF FPC}
  result := String(AEncoding.GetString(AInput));
{$ELSE}
  result := AEncoding.GetString(AInput);
{$ENDIF FPC}
end;

{$IF DEFINED(VCL_OR_LCL)}

class function TConverters.GetRValue(Argb: UInt32): Byte;
begin
  result := Byte(Argb);
end;

class function TConverters.GetGValue(Argb: UInt32): Byte;
begin
  result := Byte(Argb shr 8);
end;

class function TConverters.GetBValue(Argb: UInt32): Byte;
begin
  result := Byte(Argb shr 16);
end;

class function TConverters.RGB(Ar, Ag, Ab: Byte): TQRCodeGenLibColor;
begin
  result := (Ar or (Ag shl 8) or (Ab shl 16));
end;

class function TConverters.TColorToHTMLColorHex(const AColor
  : TQRCodeGenLibColor): String;
begin
  result := Format('%.2x%.2x%.2x', [GetRValue(QRCodeGenLibColorToRGB(AColor)),
    GetGValue(QRCodeGenLibColorToRGB(AColor)),
    GetBValue(QRCodeGenLibColorToRGB(AColor))]);
end;

class function TConverters.HTMLColorHexToTColor(const AHTMLHexColor: String)
  : TQRCodeGenLibColor;
var
  R, G, B: Byte;
begin
{$IFDEF DEBUG}
  System.Assert(System.Length(AHTMLHexColor) = 6);
{$ENDIF DEBUG}
  R := StrToInt('$' + System.Copy(AHTMLHexColor, 1, 2));
  G := StrToInt('$' + System.Copy(AHTMLHexColor, 3, 2));
  B := StrToInt('$' + System.Copy(AHTMLHexColor, 5, 2));
  result := TQRCodeGenLibColor(RGB(R, G, B));
end;

{$ELSEIF DEFINED(FMX)}

class function TConverters.TAlphaColorToHTMLColorHex(const AColor
  : TQRCodeGenLibColor): String;
begin
  result := Format('%.2x%.2x%.2x', [TQRCodeGenLibAlphaColorRec(AColor).R,
    TQRCodeGenLibAlphaColorRec(AColor).G,
    TQRCodeGenLibAlphaColorRec(AColor).B]);
end;

class function TConverters.HTMLColorHexToTAlphaColor(const AHTMLHexColor
  : String): TQRCodeGenLibColor;
var
  R, G, B: Byte;
  rec: TQRCodeGenLibAlphaColorRec;
begin
{$IFDEF DEBUG}
  System.Assert(System.Length(AHTMLHexColor) = 6);
{$ENDIF DEBUG}
  R := StrToInt('$' + System.Copy(AHTMLHexColor, 1, 2));
  G := StrToInt('$' + System.Copy(AHTMLHexColor, 3, 2));
  B := StrToInt('$' + System.Copy(AHTMLHexColor, 5, 2));
  rec.A := $FF; // for transparency
  rec.R := R;
  rec.G := G;
  rec.B := B;
  result := rec.Color;
end;

{$ELSEIF DEFINED(FCL)}

class function TConverters.TFPColorToHTMLColorHex(const AColor
  : TQRCodeGenLibColor): String;
begin
  result := Format('%.2x%.2x%.2x', [AColor.Red shr 8, AColor.Green shr 8,
    AColor.Blue shr 8]);
end;

class function TConverters.HTMLColorHexToTFPColor(const AHTMLHexColor: String)
  : TQRCodeGenLibColor;
var
  R, G, B: Byte;
begin
{$IFDEF DEBUG}
  System.Assert(System.Length(AHTMLHexColor) = 6);
{$ENDIF DEBUG}
  R := StrToInt('$' + System.Copy(AHTMLHexColor, 1, 2));
  G := StrToInt('$' + System.Copy(AHTMLHexColor, 3, 2));
  B := StrToInt('$' + System.Copy(AHTMLHexColor, 5, 2));
  result.Alpha := $FFFF; // for transparency
  result.Red := (R shl 8) + R;
  result.Green := (G shl 8) + G;
  result.Blue := (B shl 8) + B;
end;

{$IFEND VCL_OR_LCL}

class function TConverters.QRCodeGenLibColorToHTMLHexColor
  (const AColor: TQRCodeGenLibColor): String;
begin
{$IF DEFINED(VCL_OR_LCL)}
  result := TConverters.TColorToHTMLColorHex(AColor);
{$ELSEIF DEFINED(FMX)}
  result := TConverters.TAlphaColorToHTMLColorHex(AColor);
{$ELSEIF DEFINED(FCL)}
  result := TConverters.TFPColorToHTMLColorHex(AColor);
{$IFEND VCL_OR_LCL}
end;

class function TConverters.HTMLHexColorToQRCodeGenLibColor(const AHTMLHexColor
  : String): TQRCodeGenLibColor;
begin
{$IF DEFINED(VCL_OR_LCL)}
  result := TConverters.HTMLColorHexToTColor(AHTMLHexColor);
{$ELSEIF DEFINED(FMX)}
  result := TConverters.HTMLColorHexToTAlphaColor(AHTMLHexColor);
{$ELSEIF DEFINED(FCL)}
  result := TConverters.HTMLColorHexToTFPColor(AHTMLHexColor);
{$IFEND VCL_OR_LCL}
end;

end.
