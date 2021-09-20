unit QlpQrSegmentMode;

{$I QRCodeGenLib.inc}

interface

uses
{$IFDEF DEBUG}
  QlpQrCodeCommons,
{$ENDIF DEBUG}
  QlpQRCodeGenLibTypes;

resourcestring
  SInvalidState = 'Invalid state encountered.';

type
{$SCOPEDENUMS ON}
  /// <summary>
  /// Describes how a segment's data bits are interpreted.
  /// </summary>
  TQrSegmentMode = (qsmNumeric = $1, qsmAlphaNumeric = $2, qsmByte = $4,
    qsmKanji = $8, qsmEci = $7);
{$SCOPEDENUMS OFF}

type
  TQrSegmentModeHelper = record helper for TQrSegmentMode
  strict private
    function GetModeBits(): Int32; inline;
    function GetNumBitsCharCount(): TQRCodeGenLibInt32Array;

  public

    /// <summary>
    /// Returns the bit width of the character count field for a segment in
    /// this mode <br />in a QR Code at the given version number. The result
    /// is in the range [0, 16].
    /// </summary>
    function NumCharCountBits(AVersion: Int32): Int32; inline;

    /// <summary>
    /// The mode indicator bits, which is a uint4 value (range 0 to 15).
    /// </summary>
    property ModeBits: Int32 read GetModeBits;

    /// <summary>
    /// Number of character count bits for three different version ranges.
    /// </summary>
    property NumBitsCharCount: TQRCodeGenLibInt32Array read GetNumBitsCharCount;
  end;

implementation

{ TQrSegmentModeHelper }

function TQrSegmentModeHelper.GetModeBits: Int32;
begin
  result := Ord(Self);
end;

function TQrSegmentModeHelper.GetNumBitsCharCount: TQRCodeGenLibInt32Array;
begin
  case Self of

    TQrSegmentMode.qsmNumeric:
      begin
        result := TQRCodeGenLibInt32Array.Create(10, 12, 14);
        Exit;
      end;

    TQrSegmentMode.qsmAlphaNumeric:
      begin
        result := TQRCodeGenLibInt32Array.Create(9, 11, 13);
        Exit;
      end;

    TQrSegmentMode.qsmByte:
      begin
        result := TQRCodeGenLibInt32Array.Create(8, 16, 16);
        Exit;
      end;

    TQrSegmentMode.qsmKanji:
      begin
        result := TQRCodeGenLibInt32Array.Create(8, 10, 12);
        Exit;
      end;

    TQrSegmentMode.qsmEci:
      begin
        result := TQRCodeGenLibInt32Array.Create(0, 0, 0);
        Exit;
      end
  else
    begin
      raise EInvalidOperationQRCodeGenLibException.CreateRes(@SInvalidState);
    end;

  end;
end;

function TQrSegmentModeHelper.NumCharCountBits(AVersion: Int32): Int32;
begin
{$IFDEF DEBUG}
  System.Assert((TQrCodeCommons.MIN_VERSION <= AVersion) and
    (AVersion <= TQrCodeCommons.MAX_VERSION));
{$ENDIF DEBUG}
  result := NumBitsCharCount[(AVersion + 7) div 17];
end;

end.
