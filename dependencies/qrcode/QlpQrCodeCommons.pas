unit QlpQrCodeCommons;

{$I QRCodeGenLib.inc}

interface

uses
  QlpBits;

type

  /// <summary>
  /// Class to hold common constants and methods used between QrCode class
  /// and other classes to prevent the <b>dreaded circular reference.</b>
  /// </summary>
  TQrCodeCommons = class sealed(TObject)

  public

    const

    /// <summary>
    /// The minimum version number (1) supported in the QR Code Model 2
    /// standard.
    /// </summary>
    MIN_VERSION = Int32(1);

    /// <summary>
    /// The maximum version number (40) supported in the QR Code Model 2
    /// standard.
    /// </summary>
    MAX_VERSION = Int32(40);

    // Returns 0 or 1 based on the (i mod 32)'th bit of x.
    class function GetBit(Ax, Ai: Int32): Int32; inline;

  end;

implementation

{ TQrCodeCommons }

class function TQrCodeCommons.GetBit(Ax, Ai: Int32): Int32;
begin
  result := (TBits.Asr32(Ax, Byte(Ai))) and 1;
end;

end.
