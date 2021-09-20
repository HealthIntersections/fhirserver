unit QlpBits;

{$I QRCodeGenLib.inc}

interface

type
  TBits = class sealed(TObject)

  public

    /// <summary>
    /// Calculates Arithmetic shift right.
    /// </summary>
    /// <param name="AValue">Int32 value to compute 'Asr' on.</param>
    /// <param name="AShiftBits">Byte, number of bits to shift value to.</param>
    /// <returns>Shifted value.</returns>
    /// <remarks>
    /// Emulated Implementation was gotten from FreePascal sources
    /// </remarks>

    class function Asr32(AValue: Int32; AShiftBits: Byte): Int32;
      static; inline;

    /// <summary>
    /// Calculates Left Shift. This was implemented to circumvent an *issue*
    /// in FPC ARM when performing Shift Left with <b>AShiftBits &gt; 32 .</b><br />
    /// </summary>
    /// <param name="AValue">
    /// Int32 value to compute 'LS' on.
    /// </param>
    /// <param name="AShiftBits">
    /// Int32, number of bits to shift value to.
    /// </param>
    /// <returns>
    /// Shifted value.
    /// </returns>

    class function LeftShift32(AValue: Int32; AShiftBits: Int32): Int32;
      static; inline;

    /// <summary>
    /// Calculates Negative Left Shift. This was implemented to circumvent an
    /// *issue* in FPC ARM when performing Shift Left on certain values with a
    /// Negative Shift Bits. In some C Compilers, such operations are
    /// "Undefined"
    /// </summary>
    /// <param name="AValue">
    /// Int32 value to compute 'NLS' on.
    /// </param>
    /// <param name="AShiftBits">
    /// Int32, number of bits to shift value to. This Number Must be Negative
    /// </param>
    /// <returns>
    /// Shifted value.
    /// </returns>

    class function NegativeLeftShift32(AValue: Int32; AShiftBits: Int32): Int32;
      static; inline;

  end;

implementation

{ TBits }

class function TBits.Asr32(AValue: Int32; AShiftBits: Byte): Int32;

begin
{$IF DEFINED(FPC) AND (NOT DEFINED(CPUARM))}
  Result := SarLongInt(AValue, AShiftBits);
{$ELSE}
  Result := Int32(UInt32(UInt32(UInt32(AValue) shr (AShiftBits and 31)) or
    (UInt32(Int32(UInt32(0 - UInt32(UInt32(AValue) shr 31)) and
    UInt32(Int32(0 - (Ord((AShiftBits and 31) <> 0) { and 1 } )))))
    shl (32 - (AShiftBits and 31)))));
{$IFEND}
end;

class function TBits.LeftShift32(AValue, AShiftBits: Int32): Int32;
begin
  Result := AValue shl (AShiftBits and 31);
end;

class function TBits.NegativeLeftShift32(AValue, AShiftBits: Int32): Int32;
begin
{$IFDEF DEBUG}
  System.Assert(AShiftBits < 0);
{$ENDIF DEBUG}
  Result := AValue shl ((32 + AShiftBits) and 31);
end;

end.
