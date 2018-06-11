Unit FHIR.Support.Decimal;

{
Copyright (c) 2001-2013, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

{$IFDEF FPC}{$mode delphi}{$ENDIF}

Interface

Uses
  SysUtils,
  FHIR.Support.Exceptions, FHIR.Support.System, FHIR.Support.Strings, FHIR.Support.Math;

Const
  INTEGER_PRECISION = 24;

Type
{TFslDecimal
    Precision aware Decimal implementation. Any size number with any number of significant digits is supported.

    Note that operations are precision aware operations. Note that whole numbers are assumed to have
    unlimited precision. For example:
      2 x 2 = 4
      2.0 x 2.0 = 4.0
      2.00 x 2.0 = 4.0
    and
     10 / 3 = 3.33333333333333333333333333333333333333333333333
     10.0 / 3 = 3.33
     10.00 / 3 = 3.333
     10.00 / 3.0 = 3.3
     10 / 3.0 = 3.3

    Addition
      2 + 0.001 = 2.001
      2.0 + 0.001 = 2.0

    Note that the string representation is precision limited, but the internal representation
    is not.
}
  TFslDecimalStatus = (sdsUnknown, sdsUndefined, sdsNumber, sdsInfinite); // undefined is the mathematical concept - different to unknown

  TFslDecimal = record
  // data members - never use these - use the helper methods instead
  private
    FStatus : TFslDecimalStatus;
    FNegative : Boolean;
    FDigits : String;
    FDecimal : integer;
    FPrecision : integer;
    FScientific : Boolean;  // remember the representation

  // operator overloading
  public
    class operator Implicit(a : TFslDecimal) : String;
    class operator Implicit(a : TFslDecimal) : Double;
    class operator Implicit(a : TFslDecimal) : Integer;
    class operator Explicit(a : String) : TFslDecimal;
    class operator Explicit(a : Double) : TFslDecimal;
    class operator Explicit(a : Integer) : TFslDecimal;
    class operator Negative(a : TFslDecimal) : TFslDecimal;
    class operator Positive(a : TFslDecimal) : TFslDecimal;
    class operator Trunc(a : TFslDecimal) : TFslDecimal;
    class operator Equal(a: TFslDecimal; b: TFslDecimal) : Boolean;
    class operator NotEqual(a: TFslDecimal; b: TFslDecimal) : Boolean;
    class operator GreaterThan(a: TFslDecimal; b: TFslDecimal) : Boolean;
    class operator GreaterThanOrEqual(a: TFslDecimal; b: TFslDecimal) : Boolean;
    class operator LessThan(a: TFslDecimal; b: TFslDecimal) : Boolean;
    class operator LessThanOrEqual(a: TFslDecimal; b: TFslDecimal) : Boolean;
    class operator Add(a: TFslDecimal; b: TFslDecimal) : TFslDecimal;
    class operator Subtract(a: TFslDecimal; b: TFslDecimal) : TFslDecimal;
    class operator Multiply(a: TFslDecimal; b: TFslDecimal) : TFslDecimal;
    class operator Divide(a: TFslDecimal; b: TFslDecimal) : TFslDecimal;
    class operator IntDivide(a: TFslDecimal; b: TFslDecimal) : TFslDecimal;
    class operator Modulus(a: TFslDecimal; b: TFslDecimal) : TFslDecimal;
  end;

  TFslDecimalHelper = record helper for TFslDecimal
  private
    Procedure SetValue(sValue : String);
    Procedure SetValueDecimal(sValue : String);
    Procedure SetValueScientific(sValue : String);
    Function GetValue: String;
    Function GetValueDecimal : String;
    Function GetValueScientific : String;

    Function DoAdd(oOther : TFslDecimal) : TFslDecimal;
    Function DoSubtract(oOther : TFslDecimal) : TFslDecimal;

    Function StringAddition(s1, s2 : String):String;
    Function StringSubtraction(s1, s2 : String):String;

    Function dig(c : Char) : integer;
    Function cdig(i : integer) : Char;
    function GetPrecision: integer;
    procedure SetPrecision(const Value: integer);
  public
    // constructors
    class Function ValueOf(value : String) : TFslDecimal; Overload; static; inline;
    class Function ValueOf(value : Integer) : TFslDecimal; Overload; static; inline;
    class Function ValueOf(value : int64) : TFslDecimal; Overload; static; inline;
    class Function ValueOf(value : Double) : TFslDecimal; Overload; static; inline;
    class Function Create(value : String) : TFslDecimal; Overload; static; inline;
    class Function Create(value : Integer) : TFslDecimal; Overload; static; inline;
    class Function Create(value : int64) : TFslDecimal; Overload; static; inline;
    class Function Create(value : Double) : TFslDecimal; Overload; static; inline;

    function Link : TFslDecimal;
    function Clone : TFslDecimal;

    // utility functions
    class Function Equals(oOne, oTwo : TFslDecimal) : Boolean; overload; static; inline;
    class Function Compares(oOne, oTwo : TFslDecimal) : Integer; overload; static;

    class Function makeZero : TFslDecimal; overload; static; inline;
    class Function makeOne : TFslDecimal; overload; static; inline;
    class Function makeInfinity : TFslDecimal; overload; static; inline;
    class Function makeUndefined : TFslDecimal; overload; static; inline;
    class function makeNull : TFslDecimal; static; inline;

    class function CheckValue(sValue : String) : boolean; static; inline;
    class function CheckValueDecimal(sValue : String) : boolean; static; inline;
    class function CheckValueScientific(sValue : String) : boolean; static; inline;

    // accessors
    property value : String read GetValue write SetValue;
    property precision : integer read GetPrecision write SetPrecision;

    Property AsString : String read GetValue;
    Function AsCardinal : Cardinal;
    Function AsInteger : Integer;
    Function AsInt64 : Int64;
    Property AsScientific : String read GetValueScientific;
    Property AsDecimal : String read GetValuedecimal;
    function AsDouble : Double;

    {
     This method exists to populate a database with relatively arucately searchable decimal values wher ethe search is by string semantics
    }
    function normaliseDecimal(digits, decimals : integer; defUp : boolean) : String;

    // properties
    Function IsZero : Boolean;
    Function IsNegative : boolean;
    Function IsOne : Boolean;
    Function IsInfinite : Boolean;
    Function IsWholeNumber : Boolean;
    function IsNull : boolean;
    function IsUndefined : boolean;
    function isANumber : boolean;
    Function upperBound : TFslDecimal; // the upper bound given the imprecision. for example, if the value is 1,0, then the upper bound is 1.05.
    Function lowerBound : TFslDecimal; // the lower bound given the imprecision. for example, if the value is 1,0, then the lower bound is 0.95.
    Function immediateUpperBound : TFslDecimal; // the upper bound given the face value. for example, if the value is 1.0, then the upper bound is 1.000000000000000000000000001.
    Function immediateLowerBound : TFslDecimal;  // the immediate lower bound given the face value. for example, if the value is 1,0, then the upper bound is 0.99999999999999999999999999999999.

    // operators
    Function Trunc : TFslDecimal;
    Function Multiply(oOther : TFslDecimal) : TFslDecimal; Overload;
    Function Multiply(iOther : Integer) : TFslDecimal; Overload;
    Function Divide(oOther : TFslDecimal) : TFslDecimal; Overload;
    Function Divide(iOther : Integer) : TFslDecimal; Overload;
    {
      return the number of times other will "fit into" this number. This is usually called
      Integer Division, but in this implementation, neither this nor other needs to be
      a whole number; however the result of this operation will always be a whole number
    }
    Function DivInt(oOther : TFslDecimal) : TFslDecimal; Overload;
    Function DivInt(iOther : Integer) : TFslDecimal; Overload;
    Function Modulo(oOther : TFslDecimal) : TFslDecimal; Overload;
    Function Modulo(iOther : Integer) : TFslDecimal; Overload;
    Function Add(oOther : TFslDecimal) : TFslDecimal; Overload;
    Function Add(iOther : Integer) : TFslDecimal; Overload;
    Function Subtract(iOther : Integer) : TFslDecimal; Overload;
    Function Subtract(oOther : TFslDecimal) : TFslDecimal; Overload;
    function Abs: TFslDecimal; overload;
    function Negated: TFslDecimal; overload;
    Function Equals(oOther : TFslDecimal) : Boolean; overload;
    Function Compares(oOther : TFslDecimal) : Integer; overload;

end;


Function GUIDAsOIDWrong(Const aGUID : TGUID) : String;
Function GUIDAsOIDRight(Const aGUID : TGUID) : String;

function StringIsDecimal(s : String) : Boolean;

Implementation

function SimpleStringIsDecimal(s : String; allowDec : boolean) : boolean;
var
  bDec : Boolean;
  i : integer;
Begin
  if s.StartsWith('+') or s.StartsWith('-')  then
    delete(s, 1, 1);
  if s = '' then
    exit(false);

  bDec := false;
  result := true;
  for i := 1 to length(s) Do
  begin
    if not (CharInSet(s[i], ['0'..'9'])) then
      if s[i] <> '.' then
        exit(false)
      else if bDec then
        exit(false)
      else
        bDec := true;
  end;
end;

function StringIsDecimal(s : String) : Boolean;
var
  l, r : String;
begin
  if (s.Contains('e')) then
  begin
    StringSplit(s, 'e', l, r);
    result := SimpleStringIsDecimal(l, true) and SimpleStringIsDecimal(r, false)
  end
  else if (s.Contains('e')) or (s.Contains('E')) then
  begin
    StringSplit(s, 'E', l, r);
    result := SimpleStringIsDecimal(l, true) and SimpleStringIsDecimal(r, false)
  end
  else
    result := SimpleStringIsDecimal(s, true);
end;


class function TFslDecimalHelper.CheckValue(sValue: String): boolean;
begin
  if (sValue= '') or (sValue = '-') then
    result := false
  else if pos('e', lowercase(sValue)) > 0 then
    result := CheckValueScientific(lowercase(sValue))
  else
    result := CheckValueDecimal(sValue);
end;

class function TFslDecimalHelper.CheckValueDecimal(sValue: String): boolean;
var
  iDecimal : integer;
  i : integer;
Begin
  result := false;
  iDecimal := 0;
  if (sValue[1] = '-') then
    delete(sValue, 1, 1);

  while (sValue[1] = '0') And (length(sValue) > 1) Do
    delete(sValue, 1, 1);

  for i := 1 to length(sValue) do
    if (sValue[i] = '.') And (iDecimal = 0) then
      iDecimal := i
    else if not CharInSet(sValue[i], ['0'..'9']) then
      exit;

  if iDecimal <> length(sValue) then
    result := true
end;

class function TFslDecimalHelper.CheckValueScientific(sValue: String): boolean;
var
  i : integer;
  s, e : String;
begin
  result := false;
  StringSplit(sValue, 'e', s, e);

  if (s= '') or (s = '-') or not StringIsDecimal(s) then
    exit;

  if (e= '') or (e = '-') or not StringIsDecimal(e) then
    exit;

  if not checkValueDecimal(s) then
    exit;

  // now check for exponent

  if e[1] = '-' then
    i := 2
  Else
    i := 1;
  while i <= length(e) Do
  begin
    if not CharInSet(e[i], ['0'..'9']) then
      exit;
    inc(i);
  end;
  result := true;
end;

function TFslDecimalHelper.Clone: TFslDecimal;
begin
  result.FStatus := FStatus;
  result.FPrecision := FPrecision;
  result.FScientific := FScientific;
  result.FNegative := FNegative;
  result.FDigits := FDigits;
  result.FDecimal := FDecimal;
end;

procedure TFslDecimalHelper.SetPrecision(const Value: integer);
begin
  FPrecision := value;
end;

Procedure TFslDecimalHelper.SetValue(sValue : String);
Begin
  FStatus := sdsUnknown;
  FPrecision := 0;
  FScientific := false;
  FNegative := false;
  FDigits := '';
  FDecimal := 0;

  // special cases:
  if (sValue = '-0') then
    sValue := '0';
  sValue := sValue.ToLower;

  if (sValue = '') then
    FStatus := sdsUnknown
  else if (sValue = 'inf') or (sValue = '+inf') then
    FStatus := sdsInfinite
  else if (sValue = '-inf') then
  begin
    FStatus := sdsInfinite;
    FNegative := true
  end
  else if (sValue = 'nan') or (sValue = '?') then
    FStatus := sdsUndefined
  else
  begin
    FStatus := sdsNumber;
    if (sValue = '-') then
      raise Exception('"'+sValue+'" is not a valid decimal');
    sValue := lowercase(sValue);
    if pos('e', sValue) > 0 then
      SetValueScientific(sValue)
    Else
      SetValueDecimal(sValue);
  end;
end;


Function FirstNonZero(s : String):integer;
var
  i : integer;
begin
  result := length(s);
  for i := 1 to length(s) Do
    if not CharInSet(s[i], ['0', '.']) then
      result := IntegerMin(result, i);
end;

Function AllZerosButLast(s : String; iStart : integer):Boolean;
var
  i : integer;
begin
  result := iStart < length(s) - 1;
  for i := iStart to length(s) - 1 Do
    if s[i] <> '0' then
      result := false;
end;


Function AllZeros(s : String; iStart : integer):Boolean;
var
  i : integer;
begin
  result := true;
  for i := iStart to length(s) Do
    if s[i] <> '0' then
      result := false;
end;

Function CountSignificants(sValue : String):Integer;
var
  i : integer;
Begin
  i := pos('.', sValue);
  if i > 0 then
    delete(sValue, i, 1);
  while (sValue[1] = '0') do
    delete(sValue, 1, 1);
  result := length(sValue);
end;

Procedure TFslDecimalHelper.SetValueDecimal(sValue : String);
var
  iDecimal : integer;
  i : integer;
Begin
  FStatus := sdsNumber;
  FScientific := false;
  iDecimal := 0;
  FNegative := (sValue[1] = '-');
  if FNegative then
    delete(sValue, 1, 1);

  while (sValue[1] = '0') And (length(sValue) > 1) Do
    delete(sValue, 1, 1);

  for i := 1 to length(sValue) do
    if (sValue[i] = '.') And (iDecimal = 0) then
      iDecimal := i
    else if not CharInSet(sValue[i], ['0'..'9']) then
      raise ELibraryException.create('"'+sValue+'" is not a valid decimal');

  if iDecimal = 0 then
  Begin
    Precision := Length(sValue);
    FDecimal := Length(sValue)+1;
    FDigits := sValue;
  end
  else if iDecimal = length(sValue) then
    raise ELibraryException.create('"'+sValue+'" is not a valid FDecimal')
  else
  begin
    FDecimal := iDecimal;
    if AllZeros(sValue, 2) then
      Precision := length(sValue) - 1
    Else
      Precision := CountSignificants(sValue);
    FDigits := sValue;
    Delete(FDigits, FDecimal, 1);
    if AllZeros(FDigits, 1) then
      inc(FPrecision)
    else
      while (FDigits[1] = '0') Do
      begin
        delete(FDigits, 1, 1);
        dec(FDecimal);
      end;
  end;
end;

function TFslDecimalHelper.Abs: TFslDecimal;
begin
  result := self;
  if result.FStatus in [sdsNumber, sdsInfinite] then
    result.FNegative := false;
end;

Function TFslDecimalHelper.Add(iOther : Integer) : TFslDecimal;
var
  oTemp : TFslDecimal;
Begin
  oTemp := Valueof(iOther);
  result := Add(oTemp);
end;

Function TFslDecimalHelper.Add(oOther : TFslDecimal) : TFslDecimal;
Begin
  if isNull or oOther.IsNull then
    exit(makeNull)
  else if IsUndefined or oOther.IsUndefined then
    exit(makeUndefined)
  else if IsInfinite or oOther.IsInfinite then
  begin
    if IsNegative <> oOther.IsNegative then
      exit(makeUndefined)
    else if IsNegative then
      exit(makeInfinity.Negated)
    else
      exit(makeInfinity);
  end
  else if (FNegative = oOther.FNegative) then
  Begin
    result := DoAdd(oOther);
    result.FNegative := FNegative;
  end
  else if (FNegative) then
    result := oOther.DoSubtract(self)
  else
    result := DoSubtract(oOther);
end;

Function TFslDecimalHelper.StringAddition(s1, s2 : String):String;
var
  i, t, c : Integer;
Begin
  assert(length(s1) = length(s2));
  result := stringmultiply('0', length(s2));
  c := 0;
  for i := length(s1) downto 1 do
  begin
    t := c + dig(s1[i]) + dig(s2[i]);
    result[i] := cdig(t mod 10);
    c := t div 10;
  end;
  assert(c = 0);
end;

Function TFslDecimalHelper.DoAdd(oOther : TFslDecimal) : TFslDecimal;
var
  iMax : Integer;
  s1, s2, s3 : String;
Begin
  iMax := IntegerMax(FDecimal, oOther.FDecimal);
  s1 := StringMultiply('0', iMax - FDecimal+1) + FDigits;
  s2 := StringMultiply('0', iMax - oOther.FDecimal+1) + oOther.FDigits;
  if Length(s1) < length(s2) then
    s1 := s1 + StringMultiply('0', length(s2) - length(s1))
  else if Length(s2) < length(s1) then
    s2 := s2 + StringMultiply('0', length(s1) - length(s2));


  s3 := StringAddition(s1, s2);

  if s3[1] = '1' then
    inc(iMax)
  else
    delete(s3, 1, 1);
  if iMax <> length(s3)+1 then
  Begin
    if iMax <= 0 then
      raise ELibraryException.create('unhandled')
    else if imax <= length(s3) then
      insert('.', s3, iMax)
    else
      raise ELibraryException.create('unhandled')
  end;

  result := valueOf(s3);
  result.FScientific := FScientific or oOther.FScientific;
  // todo: the problem with this is you have to figure out the absolute FPrecision and take the lower of the two, not the relative one
  if FDecimal < oOther.FDecimal then
    result.FPrecision := FPrecision
  else if oOther.FDecimal < FDecimal then
    result.FPrecision := oOther.FPrecision
  else
    result.FPrecision := IntegerMin(FPrecision, oOther.FPrecision);
end;


Function TFslDecimalHelper.StringSubtraction(s1, s2 : String):String;
var
  i, t, c : integer;
Begin
  assert(length(s1) = length(s2));

  result := stringmultiply('0', length(s2));
  c := 0;
  for i := length(s1) downto 1 do
  begin
    t := c + (dig(s1[i]) - dig(s2[i]));
    if t < 0 then
    Begin
      inc(t, 10);
      if i = 1 then
        raise ELibraryException.create('internal logic error')
      else
        s1[i-1] := cdig(dig(s1[i-1])-1);
    end;
    result[i] := cdig(t);
  end;
  assert(c = 0);
end;

Function TFslDecimalHelper.DoSubtract(oOther : TFslDecimal) : TFslDecimal;
var
  iMax : Integer;
  s1, s2, s3 : String;
  bNeg : Boolean;
Begin
  iMax := IntegerMax(FDecimal, oOther.FDecimal);
  s1 := StringMultiply('0', iMax - FDecimal+1) + FDigits;
  s2 := StringMultiply('0', iMax - oOther.FDecimal+1) + oOther.FDigits;
  if Length(s1) < length(s2) then
    s1 := s1 + StringMultiply('0', length(s2) - length(s1))
  else if Length(s2) < length(s1) then
    s2 := s2 + StringMultiply('0', length(s1) - length(s2));

  bNeg := (s1 < s2);
  if bNeg then
  Begin
    s3 := s2;
    s2 := s1;
    s1 := s3;
  end;

  s3 := StringSubtraction(s1, s2);

  if s3[1] = '1' then
    inc(iMax)
  else
    delete(s3, 1, 1);
  if iMax <> length(s3)+1 then
  Begin
    if iMax <= 0 then
      raise ELibraryException.create('unhandled')
    else if imax <= length(s3) then
      insert('.', s3, iMax)
    else
      raise ELibraryException.create('unhandled');
  end;

  result := valueOf(s3);
  result.FNegative := bNeg;
  result.FScientific := FScientific or oOther.FScientific;
  if FDecimal < oOther.FDecimal then
    FPrecision := FPrecision
  else if oOther.FDecimal < FDecimal then
    FPrecision := oOther.FPrecision
  else
    FPrecision := IntegerMin(FPrecision, oOther.FPrecision);
end;

Function TFslDecimalHelper.Multiply(iOther : Integer) : TFslDecimal;
var
  oTemp : TFslDecimal;
Begin
  oTemp := valueOf(iOther);
  result := Multiply(oTemp);
end;

function TFslDecimalHelper.Negated: TFslDecimal;
begin
  result := clone;
  if FStatus in [sdsNumber, sdsInfinite] then
    result.FNegative := not result.FNegative;
end;

Function TFslDecimalHelper.Multiply(oOther : TFslDecimal) : TFslDecimal;
var
  iMax : Integer;
  s1, s2, s3 : String;
  s : Array of String;
  res : String;
  i, j, c, t : integer;
  iDec : Integer;
  iPrec : Integer;
Begin
  if isNull or oOther.isNull then
    result := MakeNull
  else if IsUndefined or oOther.IsUndefined then
    result := makeUndefined
  else if ((self.isZero) and (oOther.isInfinite)) or ((oOther.IsZero) and (self.IsInfinite)) then
    result := makeUndefined
  else if ((self.isInfinite)) or (oOther.IsInfinite) then
  begin
    result := makeInfinity;
    if self.IsNegative xor oOther.IsNegative then
      result := result.Negated;
  end
  else if (self.isZero) or (oOther.IsZero) then
    result := makeZero
  else if oOther.isOne then
    result := self
  else if self.isOne then
    result := oOther
  else
  Begin
    iMax := IntegerMax(FDecimal, oOther.FDecimal);
    s1 := StringMultiply('0', iMax - FDecimal+1) + FDigits;
    s2 := StringMultiply('0', iMax - oOther.FDecimal+1) + oOther.FDigits;
    if Length(s1) < length(s2) then
      s1 := s1 + StringMultiply('0', length(s2) - length(s1))
    else if Length(s2) < length(s1) then
      s2 := s2 + StringMultiply('0', length(s1) - length(s2));

    if s2 > s1 then
    Begin
      s3 := s1;
      s1 := s2;
      s2 := s3;
    end;
    SetLength(s, length(s2));

    t := 0;
    for i := length(s2) downto 1 do
    begin
      s[i-1] := StringMultiply('0', length(s2)-i);
      c := 0;
      for j := length(s1) downto 1 do
      begin
        t := c + (dig(s1[j]) * dig(s2[i]));
        insert(cdig(t mod 10), s[i-1], 1);
        c := t div 10;
      end;
      while c > 0 Do
      Begin
        insert(cdig(t mod 10), s[i-1], 1);
        c := t div 10;
      end;
    end;

    t := 0;
    for i := Low(s) to High(s) Do
      t := IntegerMax(t, Length(s[i]));
    for i := Low(s) to High(s) Do
      s[i] := StringMultiply('0', t-Length(s[i]))+s[i];

    res := '';
    c := 0;
    for i := t Downto 1 do
    Begin
      for j := Low(s) to High(s) Do
        c := c + dig(s[j][i]);
      insert(cdig(c mod 10), res, 1);
      c := c div 10;
    end;
      while c > 0 Do
      Begin
        assert(false, 'not implemented yet?');
//        s[i-1] := s[i-1] + cdig(t mod 10);
 //       c := t div 10;
      end;

    iDec := Length(res) + 1 - ((length(s1)-iMax)*2);

    while (res <> '') And (res <> '0') And (res[1] = '0') Do
    begin
      delete(res, 1, 1);
      dec(iDec);
    end;

    if IsWholeNumber and oOther.IsWholeNumber Then
      iPrec := INTEGER_PRECISION
    else if IsWholeNumber then
      iPrec := oOther.FPrecision
    else if oOther.IsWholeNumber then
      iPrec := FPrecision
    Else
      iPrec := IntegerMin(FPrecision, oOther.FPrecision);
    while (length(res) > iPrec) And (res[Length(res)] = '0') Do
      delete(res, Length(res), 1);

    result := valueOf(res);
    result.FStatus := sdsNumber;
    result.FPrecision := iPrec;
    result.FDecimal := iDec;
    result.FNegative := FNegative <> oOther.FNegative;
    result.FScientific := FScientific or oOther.FScientific;
//    writeln('  '+asdecimal+' x ' +oOther.AsDecimal+' = ' +result.AsDecimal);
  end;
end;

Function TFslDecimalHelper.Divide(iOther : Integer) : TFslDecimal;
var
  oTemp : TFslDecimal;
Begin
  oTemp := valueOf(iOther);
  result := Divide(oTemp);
end;


Function TrimLeadingZeros(s : String):String;
begin
  result := s;
  while (result <> '') And (result[1] = '0') do
    delete(result, 1, 1);
  if result = '' then
    result := '0';
end;

Function RoundUp(const s : String; var d : Integer):String;
var
  i : integer;
  bUp : Boolean;
Begin
  result := s;
  i := length(s);
  bUp := true;
  while bUp and (i > 0) Do
  begin
    bUp := result[i] = '9';
    if bUp then
      result[i] := '0'
    else
      result[i] := char(ord(result[i])+1);
    dec(i);
  end;
  if bUp then
  begin
    result := '1'+result;
    inc(d);
  end;
end;

Function TFslDecimalHelper.Divide(oOther : TFslDecimal) : TFslDecimal;
var
  s : String;
  w, r, v : String;
  i, l, m, d, vi, iPrec : integer;
  tens : Array of String;
  bProc, bHandled, bUp : Boolean;
  Procedure Grabnext;
  Begin
    if (vi <= length(v)) then
    begin
      w := w + v[vi];
      inc(vi);
      bhandled := false;
    end
    else
    Begin
      w := w + '0';
      inc(d);
    end;
    while (length(w) < length(tens[0])) do
      w := '0'+w;
  end;
  Function finished : Boolean;
  begin
    result := bhandled and ((l > m) or ((vi > length(v)) And ((w = '') or AllZeros(w, 1))));
  end;
Begin
  if isNull or oOther.isNull then
    result := MakeNull
  else if IsUndefined or oOther.IsUndefined then
    result := makeUndefined
  else if (oOther.isInfinite or self.IsInfinite) then
    result := makeUndefined
  else if IsZero Then
    result := makeZero
  else if oOther.IsZero then
    result := makeUndefined
  else if oOther.isOne then
    result := self
  else
  Begin
    s := '0'+oOther.FDigits;
    m := IntegerMax(length(FDigits), Length(oOther.FDigits)) + 40; // max loops we'll do
    SetLength(tens, 10);
    tens[0] := StringAddition(StringMultiply('0', length(s)), s);
    for i := 1 to 9 do
      tens[i] := StringAddition(tens[i-1], s);
    v := FDigits;
    r := '';
    l := 0;
    d := (length(FDigits) - FDecimal + 1) - (length(oOther.FDigits) - oOther.FDecimal + 1);

    while length(v) < length(tens[0]) do
    begin
      v := v+'0';
      inc(d);
    end;
    if copy(v, 1, length(oOther.FDigits)) < oOther.FDigits then
    Begin
      if length(v) = length(tens[0]) then
      begin
        v := v + '0';
        inc(d);
      end;
      w := copy(v, 1, length(oOther.FDigits)+1);
      vi := length(w)+1;
    end
    Else
    Begin
      w := '0'+copy(v, 1, length(oOther.FDigits));
      vi := length(w);
    end;
    while not finished Do
    Begin
      inc(l);
      bHandled := true;
      bProc := false;
      for i := 8 downto 0 Do
      begin
        if tens[i] <= w then
        Begin
          bProc := true;
          r := r + cdig(i+1);
          w := TrimLeadingZeros(StringSubtraction(w, tens[i]));
          if not finished then
            grabnext;
          break;
        end;
      end;
      if not bProc then
      begin
        assert(w[1] = '0');
        delete(w, 1, 1);
        r := r + '0';
        if not finished then
          grabNext;
      end;
    end;
    if isWholeNumber And oOther.IsWholeNumber and (l < m) then
    begin
      for i := 1 to d do
        if (r[length(r)] = '0') then
        begin
          delete(r, length(r), 1);
          dec(d);
        end;
      iPrec := INTEGER_PRECISION;
    end
    Else
    Begin
      if IsWholeNumber and oOther.IsWholeNumber Then
        iPrec := INTEGER_PRECISION
      else if IsWholeNumber then
        iPrec := IntegerMax(oOther.FPrecision, length(r) - d)
      else if oOther.IsWholeNumber then
        iPrec := IntegerMax(FPrecision, length(r) - d)
      Else
        iPrec := IntegerMax(IntegerMin(FPrecision, oOther.FPrecision), length(r) - d);
      while (length(r) > iPrec) Do
      begin
        bUp := (r[Length(r)] > '5');
        delete(r, Length(r), 1);
        if bUp then
          r := roundUp(r, d);
        dec(d);
      end;
    end;
    result := valueOf(r);
    result.FStatus := sdsNumber;
    result.FDecimal := length(r) - d + 1;
    result.FNegative := FNegative <> oOther.FNegative;
    result.FPrecision := iPrec;
    result.FScientific := FScientific or oOther.FScientific;
//    writeln('  '+asdecimal+' / ' +oOther.AsDecimal+' = ' +result.AsDecimal);
  end;
end;

Function TFslDecimalHelper.DivInt(iOther : Integer) : TFslDecimal;
var
  oTemp : TFslDecimal;
Begin
  oTemp := valueOf(iOther);
  result := DivInt(oTemp);
end;


Function TFslDecimalHelper.DivInt(oOther : TFslDecimal) : TFslDecimal;
Begin
  result := Divide(oOther);
  if (result.FStatus = sdsNumber) then
    result := result.Trunc;
end;


Function TFslDecimalHelper.Modulo(iOther : Integer) : TFslDecimal;
var
  oTemp : TFslDecimal;
Begin
  oTemp := valueOf(iOther);
  result := Modulo(oTemp);
end;


Function TFslDecimalHelper.Modulo(oOther : TFslDecimal) : TFslDecimal;
var
  t, t2 : TFslDecimal;
Begin
  t := DivInt(oOther);
  if (t.FStatus = sdsNumber) then
  begin
    t2 := t.Multiply(oOther);
    result := subtract(t2);
  end
  else
    result := t;
end;


Function TFslDecimalHelper.Subtract(iOther : Integer) : TFslDecimal;
var
  oTemp : TFslDecimal;
Begin
  oTemp := valueOf(iOther);
  result := Subtract(oTemp);
end;

Function TFslDecimalHelper.Subtract(oOther : TFslDecimal) : TFslDecimal;
Begin
  if isNull or oOther.IsNull then
    exit(makeNull)
  else if IsUndefined or oOther.IsUndefined then
    exit(makeUndefined)
  else if IsInfinite or oOther.IsInfinite then
  begin
    if IsNegative = oOther.IsNegative then
      exit(makeUndefined)
    else if IsNegative then
      exit(makeInfinity)
    else
      exit(makeInfinity.Negated);
  end
  else if (FNegative and not oOther.FNegative) then
  Begin
    result := DoAdd(oOther);
    result.FNegative := true;
  end
  Else if (not FNegative and oOther.FNegative) then
    result := DoAdd(oOther)
  Else if (FNegative and oOther.FNegative) then
  begin
    result := DoSubtract(oOther);
    result.FNegative := not result.FNegative;
  end
  Else
  begin
    result := oOther.DoSubtract(self);
    result.FNegative := not result.FNegative;
  end;
end;

Function TFslDecimalHelper.Equals(oOther : TFslDecimal) : Boolean;
Begin
  result := Equals(self, oOther);
end;


Procedure TFslDecimalHelper.SetValueScientific(sValue: String);
var
  i : integer;
  s, e : String;
begin
  StringSplit(sValue, 'e', s, e);

  if (s= '') or (s = '-') or not StringIsDecimal(s) then
    raise ELibraryException.create('"'+sValue+'" is not a valid FDecimal (numeric)');
  if (e= '') or (e = '-') or not StringIsDecimal(e) then
    raise ELibraryException.create('"'+sValue+'" is not a valid FDecimal (exponent)');

  SetValueDecimal(s);
  FScientific := true;

  // now adjust for exponent

  if e[1] = '-' then
    i := 2
  Else
    i := 1;
  while i <= length(e) Do
  begin
    if not CharInSet(e[i], ['0'..'9', '-', '+']) then
      raise ELibraryException.create('"'+sValue+'" is not a valid FDecimal');
    inc(i);
  end;
  i := StrToInt(e);
  FDecimal := FDecimal + i;
end;

function TFslDecimalHelper.GetPrecision: integer;
begin
  result := FPrecision;
end;

Function TFslDecimalHelper.GetValue: String;
begin
  if IsInfinite then
  begin
    if IsNegative then
      result := '-∞'
    else
      result := '∞';
  end
  else if IsUndefined then
    result := '?'
  else if IsNull then
    result := ''
  else if FScientific then
    result := GetValueScientific
  Else
    result := GetValueDecimal;
end;
{
Function TFslDecimal.GetValueByPrecision: String;
begin
  if FScientific then
    result := GetValueScientificByPrecision
  Else
    result := GetValueDecimalByPrecision;
end;
}

Function TFslDecimalHelper.GetValueScientific: String;
var
  bZero : Boolean;
begin
  if IsInfinite then
  begin
    if IsNegative then
      result := '-∞'
    else
      result := '∞';
  end
  else if IsUndefined then
    result := '?'
  else if IsNull then
    result := ''
  else
  begin
    result := FDigits;
    bZero := AllZeros(result, 1);
    if bZero then
    begin
      if FPrecision < 2 then
        result := '0e0'
      Else
        result := '0.'+StringMultiply('0', FPrecision-1)+'e0';
    end
    Else
    begin
      if Length(FDigits) > 1 then
        insert('.', result, 2);
      result := result + 'e'+inttostr(FDecimal - 2);
    end;
    if FNegative and not bZero then
      result := '-' + result;
  end;
end;


function TFslDecimalHelper.immediateLowerBound: TFslDecimal;
var
  i : integer;
begin
  if IsNull or IsUndefined or IsInfinite then
    exit(self)
  else if IsZero then
  begin
    result := immediateUpperBound;
    result.FNegative := true;
  end
  else
  begin
    result := valueOf(AsDecimal);
    inc(result.FPrecision);
    if FNegative then
    result.FDigits := result.FDigits + StringMultiply('0', 25 - length(result.FDigits) - result.FDecimal)+'1'
    else
    begin
      i := length(result.FDigits);
      result.FDigits[i] := char(ord(result.FDigits[i]) - 1);
      while (i > 0) and (result.FDigits[i] < '0') do
      begin
        result.FDigits[i] := '9';
        dec(i);
        result.FDigits[i] := char(ord(result.FDigits[i]) - 1)
      end;
      assert(i > 0);
    result.FDigits := result.FDigits + StringMultiply('9', 24 - length(result.FDigits))+'9'
    end;
  end;
end;

function TFslDecimalHelper.immediateUpperBound: TFslDecimal;
var
  i : integer;
begin
  if IsNull or IsUndefined or IsInfinite then
    exit(self)
  else
  begin
    result := valueOf(AsDecimal);
    inc(result.FPrecision);
    if not FNegative then
      result.FDigits := result.FDigits + StringMultiply('0', 25 - length(result.FDigits) - result.FDecimal)+'1'
    else
    begin
      i := length(result.FDigits);
      result.FDigits[i] := char(ord(result.FDigits[i]) - 1);
      while (i > 1) and (result.FDigits[i] < '0') do
      begin
        result.FDigits[i] := '0';
        dec(i);
        result.FDigits[i] := char(ord(result.FDigits[i]) - 1);
      end;
      assert(i > 0);
      result.FDigits := result.FDigits + StringMultiply('0', 24 - length(result.FDigits))+'9'
    end;
  end;
end;

Function TFslDecimalHelper.GetValueDecimal: String;
begin
  if IsInfinite then
  begin
    if IsNegative then
      result := '-∞'
    else
      result := '∞';
  end
  else if IsUndefined then
    result := '?'
  else if IsNull then
    result := ''
  else
  begin
    result := FDigits;
    if FDecimal <> length(FDigits) + 1 then
      if FDecimal < 1 then
        result := '0.'+StringMultiply('0', 1-FDecimal)+FDigits
      Else if FDecimal <= length(result) then
        if (FDecimal = 1) then
          result := '0.'+result
        Else
          insert('.', result, FDecimal)
      Else
        result := result + stringMultiply('0', FDecimal - length(result)-1);
    if (FPrecision = INTEGER_PRECISION) and result.Contains('.') and (AllZerosButLast(result, pos('.', result)+1)) then
      result := copy(result, 1, pos('.', result)-1);
    if FNegative and not AllZeros(result, 1) then
      result := '-' + result;
  end;
end;

Function TFslDecimalHelper.cdig(i: integer): Char;
begin
//  assert((i >= 0) and (I <= 9));
  result := char(i + ord('0'));
end;

Function TFslDecimalHelper.dig(c: Char): integer;
begin
//  assert(c in ['0'..'9']);
  result := ord(c) - ord('0');
end;

Function TFslDecimalHelper.IsZero: Boolean;
begin
  if FStatus = sdsNumber then
    result := AllZeros(FDigits, 1)
  else
    result := false;
end;

function TFslDecimalHelper.Link: TFslDecimal;
begin
  result.FStatus := FStatus;
  result.FPrecision := FPrecision;
  result.FScientific := FScientific;
  result.FNegative := FNegative;
  result.FDigits := FDigits;
  result.FDecimal := FDecimal;
end;

class function TFslDecimalHelper.makeInfinity: TFslDecimal;
begin
  result := makeNull;
  result.FStatus := sdsInfinite;
end;

function TFslDecimalHelper.IsInfinite: Boolean;
begin
  result := FStatus = sdsInfinite;
end;

function TFslDecimalHelper.IsNegative: boolean;
begin
  if FStatus in [sdsNumber, sdsInfinite] then
    result := FNegative
  else
    result := false;
end;

function TFslDecimalHelper.isANumber: boolean;
begin
  result := FStatus = sdsNumber;
end;

function TFslDecimalHelper.IsNull: boolean;
begin
  result := FStatus = sdsUnknown;
end;

Function TFslDecimalHelper.IsOne: Boolean;
begin
  result := Compares(makeOne) = 0;
end;

function TFslDecimalHelper.IsUndefined: boolean;
begin
  result := FStatus = sdsUndefined;
end;

Function TFslDecimalHelper.Compares(oOther: TFslDecimal): Integer;
Begin
  result := Compares(self, oOther);
end;

class function TFslDecimalHelper.Create(value: String): TFslDecimal;
begin
  result := makeNull;
  result.SetValue(value);
end;

class function TFslDecimalHelper.Create(value: Integer): TFslDecimal;
begin
  result := makeNull;
  result.SetValue(inttostr(value));
end;

class function TFslDecimalHelper.Create(value: int64): TFslDecimal;
begin
  result := makeNull;
  result.SetValue(inttostr(value));
end;

Function TFslDecimalHelper.isWholeNumber: Boolean;
begin
  if FStatus = sdsNumber then
    result := pos('.', GetValueDecimal) = 0
  else
    result := false;
end;

Function CardinalToint64(i : Cardinal):Int64;
Begin
  result := i;
end;

Function TFslDecimalHelper.Trunc: TFslDecimal;
begin
  if IsNull or IsUndefined or IsInfinite then
    exit(self)
  else if FDecimal < 1 then
    result := makeZero
  Else
  begin
    result := self;
    if Length(result.FDigits) >= FDecimal then
      SetLength(result.FDigits, FDecimal-1);
    if result.FDigits = '' then
    begin
      result.FDigits := '0';
      result.FDecimal := 2;
    end;
  end;
end;

class function TFslDecimalHelper.makeUndefined: TFslDecimal;
begin
  result.FStatus := sdsUndefined;
end;

function TFslDecimalHelper.upperBound: TFslDecimal;
var
  i : integer;
begin
  if IsNull or IsUndefined or IsInfinite then
    exit(self)
  else
  begin
    result := valueOf(AsDecimal);
    inc(result.FPrecision);
    if not FNegative then
      result.FDigits := result.FDigits + '5'
    else
    begin
      i := length(result.FDigits);
      result.FDigits[i] := char(ord(result.FDigits[i]) - 1);
      while (i > 1) and (result.FDigits[i] < '0') do
      begin
        result.FDigits[i] := '0';
        dec(i);
        result.FDigits[i] := char(ord(result.FDigits[i]) - 1);
      end;
      assert(i > 0);
      result.FDigits := result.FDigits + '5'
    end;
  end;
end;

class function TFslDecimalHelper.ValueOf(value: Double): TFslDecimal;
begin
  result := makeNull;
  result.SetValue(FloatToStr(value));
end;

Function TFslDecimalHelper.AsCardinal: Cardinal;
var
  r : Int64;
  m : Int64;
begin
  r := AsInt64;
  if r < 0 then
    raise ELibraryException.create('Unable to represent '+AsString+' as an unsigned 4 byte number');
  m := High(Cardinal);
  if r > m then
    raise ELibraryException.create('Unable to represent '+AsString+' as an unsigned 4 byte number');
  result := r;
end;

function TFslDecimalHelper.normaliseDecimal(digits, decimals : integer; defUp : boolean) : String;
var
  s, sd : String;
  d, a : TFslDecimal;
  i : integer;
  function allChar(s : String; tch : char) : boolean;
  var
    ch : char;
  begin
    result := true;
    for ch in s do
      if ch <> tch then
        exit(false);
  end;
begin
  if IsNull or IsUndefined then
    exit('?'+StringPadLeft('', '?', digits)+'.'+StringPadLeft('', '?', decimals));

  d := ValueOf('1'+StringPadRight('', '0', digits));
  if IsNegative then
  begin
    a := Abs;
    if Compares(a, d) > 0 then // this number is smaller than we can cope with
      if defUp then
        exit('!'+StringPadLeft('', '0', digits)+'.'+StringPadLeft('', '0', decimals))
      else
        exit('!'+StringPadLeft('', '#', digits)+'.'+StringPadLeft('', '#', decimals));
    s := d.Subtract(a).AsDecimal;
  end
  else
  begin
    if Compares(self, d) >= 0 then // this number is bigger than we can cope with
      if defUp then
        exit('0'+StringPadLeft('', 'X', digits)+'.'+StringPadLeft('', 'X', decimals))
      else
        exit('0'+StringPadLeft('', '9', digits)+'.'+StringPadLeft('', '9', decimals));
    s := AsDecimal;
  end;

  i := s.IndexOf('.');
  if i < 0 then
    i := s.Length;
  result := s.subString(0, i);
  result := StringPadLeft(result, '0', digits);
  sd := '';
  if i < s.Length then
    sd := s.Substring(i+1);
  if (sd.Length > decimals) then
  begin
    sd := sd.Substring(0, decimals);
    if IsNegative then
    begin
      if allChar(sd, '9') and allChar(result, '9') then
        if defUp then
          exit('0'+StringPadLeft('', '0', digits)+'.'+StringPadLeft('', '0', decimals))
        else
          exit('!'+StringPadLeft('', '9', digits)+'.'+StringPadLeft('', '9', decimals));
    end
    else if allChar(sd, '0') and allChar(result, '0') then
    begin
      if defUp then
        exit('0'+StringPadLeft('', '0', digits)+'.'+StringPadLeft('', '0', decimals-1)+'1')
      else
        exit('0'+StringPadLeft('', '0', digits)+'.'+StringPadLeft('', '0', decimals));
    end;
  end
  else
    sd := StringPadRight(sd, '0', decimals);
  result := result + '.' + sd;
  if IsNegative then
    result := '!'+result
  else
    result := '0'+result;
end;

function TFslDecimalHelper.AsDouble: Double;
begin
  if not isANumber then
    raise ELibraryException.create('Unable to represent '+AsString+' as an double');
  result := StrToFloat(AsScientific);
end;

function TFslDecimalHelper.AsInt64: Int64;
var
  t : TFslDecimal;
begin
  if not isWholeNumber then
    raise ELibraryException.create('Unable to represent '+AsString+' as an integer');
  t := valueOf(Low(Int64));
  if Compares(self, t) < 0 then
    raise ELibraryException.create('Unable to represent '+AsString+' as a signed 8 byte integer');
  t := valueOf(High(Int64));
  if Compares(self, t) > 0 then
    raise ELibraryException.create('Unable to represent '+AsString+' as a signed 8 byte integer');
  result := StrToInt64(AsDecimal);
end;

function TFslDecimalHelper.AsInteger: Integer;
var
  r : Int64;
  m : Int64;
begin
  r := AsInt64;
  m := Low(Integer);
  if r < m then
    raise ELibraryException.create('Unable to represent '+AsString+' as a signed 4 byte number');
  m := high(Integer);
  if r > m then
    raise ELibraryException.create('Unable to represent '+AsString+' as a signed 4 byte number');
  result := r;
end;

class Function TFslDecimalHelper.valueOf(value : String) : TFslDecimal;
begin
  result := makeNull;
  result.SetValue(value);
end;

class Function TFslDecimalHelper.valueOf(value : Integer) : TFslDecimal;
begin
  result := makeNull;
  result.SetValue(inttostr(value));
end;

class Function TFslDecimalHelper.valueOf(value : int64) : TFslDecimal;
begin
  result := makeNull;
  result.SetValue(inttostr(value));
end;

class Function TFslDecimalHelper.Equals(oOne, oTwo : TFslDecimal) : Boolean;
Begin
  if (oOne.FStatus = sdsInfinite) and (oTwo.FStatus = sdsInfinite) then
    result := oOne.IsNegative = oTwo.IsNegative
  else if (oOne.FStatus = sdsUndefined) and (oTwo.FStatus = sdsUndefined) then
    result := true
  else if (oOne.FStatus = sdsNumber) and (oTwo.FStatus = sdsNumber) then
    result := Compares(oOne, oTwo) = 0
  else
    result := false;
end;

class Function TFslDecimalHelper.makeOne: TFslDecimal;
begin
  result := ValueOf(1);
end;

class Function TFslDecimalHelper.makeZero: TFslDecimal;
begin
  result := ValueOf(0);
end;


function round(prefix, s : String; i : integer) : String;
begin
  result := s;
  if s.Length > i then
  begin
    result := copy(result, 1, i);
    if (s[i+1]) >= '5' then
      result[i] := chr(ord(result[i])+1);
    while (i > 0) and  (result[i] > '9') do
    begin
      result[i] := '0';
      dec(i);
      result[i] := chr(ord(result[i])+1);
    end;
  end;
  if i = 0 then
  begin
    result := copy(prefix, 1, length(prefix)-1) +'1' + result;
  end
  else
    result := prefix + result;
end;

class Function TFslDecimalHelper.Compares(oOne, oTwo: TFslDecimal): Integer;
var
  iMax : Integer;
  s1, s2 : String;
Begin
  if (oOne.FStatus in [sdsUnknown, sdsUndefined]) or (oTwo.FStatus in [sdsUnknown, sdsUndefined]) then
    raise ELibraryException.Create('Unable to compare "'+oOne.AsString+'" and "'+oTwo.AsString+'"')
  else if oOne.FNegative and not oTwo.FNegative then
    result := -1
  else if not oOne.FNegative and oTwo.FNegative then
    result := 1
  else if oOne.IsInfinite and oTwo.IsInfinite then
    result := 0
  else if oOne.IsInfinite then
    result := 1
  else if oTwo.IsInfinite then
    result := -1
  else
  begin
    iMax := IntegerMax(oOne.FDecimal, oTwo.FDecimal);
    s1 := round('0'+StringMultiply('0', iMax - oOne.FDecimal+1), oOne.FDigits, oOne.FPrecision);
    s2 := round('0'+StringMultiply('0', iMax - oTwo.FDecimal+1), oTwo.FDigits, oTwo.FPrecision);
    if Length(s1) < length(s2) then
      s1 := s1 + StringMultiply('0', length(s2) - length(s1))
    else if Length(s2) < length(s1) then
      s2 := s2 + StringMultiply('0', length(s1) - length(s2));
    result := StringCompare(s1, s2);
    if oOne.FNegative then
      result := -result;
  End;
end;


Function TFslDecimalHelper.lowerBound: TFslDecimal;
var
  i : integer;
begin
  if IsNull or IsUndefined or IsInfinite then
    exit(self)
  else if IsZero then
  begin
    result := upperBound;
    result.FNegative := true;
  end
  else
  begin
    result := valueOf(AsDecimal);
    inc(result.FPrecision);
    if FNegative then
      result.FDigits := result.FDigits + '5'
    else
    begin
      i := length(result.FDigits);
      result.FDigits[i] := char(ord(result.FDigits[i]) - 1);
      while (i > 0) and (result.FDigits[i] < '0') do
      begin
        result.FDigits[i] := '9';
        dec(i);
        result.FDigits[i] := char(ord(result.FDigits[i]) - 1)
      end;
      assert(i > 0);
      result.FDigits := result.FDigits + '5';
    end;
  end;
end;


class function TFslDecimalHelper.makeNull: TFslDecimal;
begin
  result.FStatus := sdsUnknown;
  result.FNegative := false;
  result.FDigits := '';
  result.FDecimal := 0;
  result.FPrecision := 0;
  result.FScientific := false;
end;

Function GUIDAsOIDWrong(Const aGUID : TGUID) : String;
var
  sGuid, s : String;
  r1, r2, r3, r4 : int64;
  c : integer;
  b1, b2, b3, b4, bs : TFslDecimal;
Begin
  sGuid := GUIDToString(aGuid);
  s := copy(sGuid, 30, 8);
  Val('$'+s, r1, c);
  s := copy(sGuid, 21, 4)+copy(sGuid, 26, 4);
  Val('$'+s, r2, c);
  s := copy(sGuid, 11, 4)+copy(sGuid, 26, 4);
  Val('$'+s, r3, c);
  s := copy(sGuid, 2, 8);
  Val('$'+s, r4, c);

  b1 := TFslDecimal.valueOf(r1);
  b2 := TFslDecimal.valueOf(r2);
  b3 := TFslDecimal.valueOf(r3);
  b4 := TFslDecimal.valueOf(r4);
  bs := TFslDecimal.valueOf('4294967296');
  b2 := b2.Multiply(bs);
  bs := TFslDecimal.valueOf('18446744073709551616');
  b3 := b3.Multiply(bs);
  bs := TFslDecimal.valueOf('79228162514264337593543950336');
  b4 := b4.Multiply(bs);
  b1 := b1.Add(b2);
  b1 := b1.Add(b3);
  b1 := b1.Add(b4);
  result := '2.25.'+b1.AsString;
end;


Function GUIDAsOIDRight(Const aGUID : TGUID) : String;
var
  sGuid, s : String;
  r1, r2, r3, r4 : int64;
  c : integer;
  b1, b2, b3, b4, bs : TFslDecimal;
Begin
  sGuid := GUIDToString(aGuid);
  s := copy(sGuid, 30, 8);
  Val('$'+s, r1, c);
  s := copy(sGuid, 21, 4)+copy(sGuid, 26, 4);
  Val('$'+s, r2, c);
  s := copy(sGuid, 11, 4)+copy(sGuid, 16, 4);
  Val('$'+s, r3, c);
  s := copy(sGuid, 2, 8);
  Val('$'+s, r4, c);

  b1 := TFslDecimal.valueOf(r1);
  b2 := TFslDecimal.valueOf(r2);
  b3 := TFslDecimal.valueOf(r3);
  b4 := TFslDecimal.valueOf(r4);
  bs := TFslDecimal.valueOf('4294967296');
  b2 := b2.Multiply(bs);
  bs := TFslDecimal.valueOf('18446744073709551616');
  b3 := b3.Multiply(bs);
  bs := TFslDecimal.valueOf('79228162514264337593543950336');
  b4 := b4.Multiply(bs);
  b1 := b1.Add(b2);
  b1 := b1.Add(b3);
  b1 := b1.Add(b4);
  result := '2.25.'+b1.AsString;
end;

{ TFslDecimal }

class operator TFslDecimal.Add(a, b: TFslDecimal): TFslDecimal;
begin
  result := a.Add(b);
end;

class operator TFslDecimal.Divide(a, b: TFslDecimal): TFslDecimal;
begin
  result := a.Divide(b);
end;

class operator TFslDecimal.Equal(a, b: TFslDecimal): Boolean;
begin
  result := Equals(a, b);
end;

class operator TFslDecimal.Explicit(a: String): TFslDecimal;
begin
  result := TFslDecimal.ValueOf(a);
end;

class operator TFslDecimal.Explicit(a: Double): TFslDecimal;
begin
  result := TFslDecimal.ValueOf(a);
end;

class operator TFslDecimal.Explicit(a: Integer): TFslDecimal;
begin
  result := TFslDecimal.ValueOf(a);
end;

class operator TFslDecimal.GreaterThan(a, b: TFslDecimal): Boolean;
begin
  result := TFslDecimal.Compares(a, b) > 0;
end;

class operator TFslDecimal.GreaterThanOrEqual(a, b: TFslDecimal): Boolean;
begin
  result := TFslDecimal.Compares(a, b) >= 0;
end;

class operator TFslDecimal.Implicit(a: TFslDecimal): Integer;
begin
  result := a.AsInteger;
end;

class operator TFslDecimal.Implicit(a: TFslDecimal): Double;
begin
  result := a.AsDouble;
end;

class operator TFslDecimal.Implicit(a: TFslDecimal): String;
begin
  result := a.AsString;
end;

class operator TFslDecimal.IntDivide(a, b: TFslDecimal): TFslDecimal;
begin
  result := a.DivInt(b);
end;

class operator TFslDecimal.LessThan(a, b: TFslDecimal): Boolean;
begin
  result := TFslDecimal.Compares(a, b) < 0;
end;

class operator TFslDecimal.LessThanOrEqual(a, b: TFslDecimal): Boolean;
begin
  result := TFslDecimal.Compares(a, b) <= 0;
end;

class operator TFslDecimal.Modulus(a, b: TFslDecimal): TFslDecimal;
begin
  result := a.Modulo(b);
end;

class operator TFslDecimal.Multiply(a, b: TFslDecimal): TFslDecimal;
begin
  result := a.Multiply(b);
end;

class operator TFslDecimal.Negative(a: TFslDecimal): TFslDecimal;
begin
  result := a.Negated;
end;

class operator TFslDecimal.NotEqual(a, b: TFslDecimal): Boolean;
begin
  result := not TFslDecimal.Equals(a, b);
end;

class operator TFslDecimal.Positive(a: TFslDecimal): TFslDecimal;
begin
  result := a;
end;

class operator TFslDecimal.Subtract(a, b: TFslDecimal): TFslDecimal;
begin
  result := a.Subtract(b);
end;

class operator TFslDecimal.Trunc(a: TFslDecimal): TFslDecimal;
begin
  result := a.Trunc;
end;

class function TFslDecimalHelper.Create(value: Double): TFslDecimal;
begin
  result := makeNull;
  result.SetValue(FloatToStr(value));
end;

end.
