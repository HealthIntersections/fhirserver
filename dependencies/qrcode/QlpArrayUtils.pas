unit QlpArrayUtils;

{$I QRCodeGenLib.inc}

interface

uses
  Math,
  QlpQRCodeGenLibTypes;

type
  TArrayUtils = class sealed(TObject)

  public

    /// <summary>
    /// Copies the specified array, truncating or padding with zeros (if
    /// necessary) so the copy has the specified length. For all indices that
    /// are valid in both the original array and the copy, the two arrays
    /// will contain identical values. For any indices that are valid in the
    /// copy but not the original, the copy will contain <c>0</c>. <br />Such
    /// indices will exist if and only if the specified length is greater
    /// than that of the original array.
    /// </summary>
    /// <param name="ANewLength">
    /// the length of the copy to be returned
    /// </param>
    /// <param name="AData">
    /// the array to be copied
    /// </param>
    /// <remarks>
    /// This Function Assumes <c>AOriginal</c> and <c>ANewLength</c> are
    /// Valid. (Non Null and Non Negative Respectively)
    /// </remarks>
    class function CopyOf(const AOriginal: TQRCodeGenLibInt32Array;
      ANewLength: Int32): TQRCodeGenLibInt32Array; static; inline;

    /// <summary>
    /// Assigns the specified <c>Int32</c> value to each element of the
    /// specified array of <c>Int32s</c>.
    /// </summary>
    /// <param name="ABuffer">
    /// the array to be filled
    /// </param>
    /// <param name="AFiller">
    /// the value to be stored in all elements of the array
    /// </param>
    class procedure Fill(const ABuffer: TQRCodeGenLibInt32Array;
      AFiller: Int32); overload; static; inline;

    /// <summary>
    /// Assigns the specified <c>Byte</c> value to each element of the
    /// specified array of <c>Bytes</c>.
    /// </summary>
    /// <param name="ABuffer">
    /// the array to be filled
    /// </param>
    /// <param name="AFiller">
    /// the value to be stored in all elements of the array
    /// </param>
    class procedure Fill(const ABuffer: TQRCodeGenLibByteArray; AFiller: Byte);
      overload; static; inline;

  end;

implementation

{ TArrayUtils }

class function TArrayUtils.CopyOf(const AOriginal: TQRCodeGenLibInt32Array;
  ANewLength: Int32): TQRCodeGenLibInt32Array;
begin
  System.SetLength(Result, ANewLength);
  System.Move(AOriginal[0], Result[0], Min(System.Length(AOriginal), ANewLength)
    * System.SizeOf(Int32));
end;

class procedure TArrayUtils.Fill(const ABuffer: TQRCodeGenLibInt32Array;
  AFiller: Int32);
var
  LIdx: Int32;
begin
  for LIdx := System.Low(ABuffer) to System.High(ABuffer) do
  begin
    ABuffer[LIdx] := AFiller;
  end;
end;

class procedure TArrayUtils.Fill(const ABuffer: TQRCodeGenLibByteArray;
  AFiller: Byte);
begin
  System.FillChar(ABuffer[0], System.Length(ABuffer) *
    System.SizeOf(Byte), AFiller);
end;

end.
