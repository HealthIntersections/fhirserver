Unit DecimalSupport;

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

Interface

Uses
  GuidSupport,
  StringSupport,
  MathSupport,
  SysUtils;

Const
  INTEGER_PRECISION = 24;

Type
{TSmartDecimal
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
  TSmartDecimal = record
    Precision : integer;
    Scientific : Boolean;
    Negative : Boolean;
    Digits : String;
    Decimal : integer;
  end;

  TSmartDecimalHelper = record helper for TSmartDecimal
  private
    Procedure SetValue(sValue : String);
    Procedure SetValueDecimal(sValue : String);
    Procedure SetValueScientific(sValue : String);
    Function GetValue: String;
    Function GetValueDecimal : String;
    Function GetValueScientific : String;

    Function DoAdd(oOther : TSmartDecimal) : TSmartDecimal;
    Function DoSubtract(oOther : TSmartDecimal) : TSmartDecimal;

    Function StringAddition(s1, s2 : String):String;
    Function StringSubtraction(s1, s2 : String):String;

    Function dig(c : Char) : integer;
    Function cdig(i : integer) : Char;
  public
    // constructors
    class Function ValueOf(value : String) : TSmartDecimal; Overload; static; inline;
    class Function ValueOf(value : Integer) : TSmartDecimal; Overload; static; inline;
    class Function ValueOf(value : int64) : TSmartDecimal; Overload; static; inline;

    // utility functions
    class Function Equals(oOne, oTwo : TSmartDecimal) : Boolean; overload; static; inline;
    class Function Compares(oOne, oTwo : TSmartDecimal) : Integer; overload; static;
    class Function Zero : TSmartDecimal; overload; static; inline;
    class Function One : TSmartDecimal; overload; static; inline;
    class function CheckValue(sValue : String) : boolean; static; inline;
    class function CheckValueDecimal(sValue : String) : boolean; static; inline;
    class function CheckValueScientific(sValue : String) : boolean; static; inline;

    // accessors
    property value : String read GetValue write SetValue;
    Property AsString : String read GetValue;
    Function AsCardinal : Cardinal;
    Function AsInteger : Integer;
    Function AsInt64 : Int64;
    Property AsScientific : String read GetValueScientific;
    Property AsDecimal : String read GetValuedecimal;
    function AsDouble : Double;

    // properties
    Function IsZero : Boolean;
    Function IsNegative : boolean;
    Function IsOne : Boolean;
    Function IsWholeNumber : Boolean;
    Function upperBound : TSmartDecimal; // the upper bound given the imprecision. for example, if the value is 1,0, then the upper bound is 1.05.
    Function lowerBound : TSmartDecimal; // the lower bound given the imprecision. for example, if the value is 1,0, then the lower bound is 0.95.
    Function immediateUpperBound : TSmartDecimal; // the upper bound given the face value. for example, if the value is 1.0, then the upper bound is 1.000000000000000000000000001.
    Function immediateLowerBound : TSmartDecimal;  // the immediate lower bound given the face value. for example, if the value is 1,0, then the upper bound is 0.99999999999999999999999999999999.

    // operators
    Function Trunc : TSmartDecimal;
    Function Multiply(oOther : TSmartDecimal) : TSmartDecimal; Overload;
    Function Multiply(iOther : Integer) : TSmartDecimal; Overload;
    Function Divide(oOther : TSmartDecimal) : TSmartDecimal; Overload;
    Function Divide(iOther : Integer) : TSmartDecimal; Overload;
    {@member DivInt
      return the number of times other will "fit into" this number. This is usually called
      Integer Division, but in this implementation, neither this nor other needs to be
      a whole number; however the result of this operation will always be a whole number
    }
    Function DivInt(oOther : TSmartDecimal) : TSmartDecimal; Overload;
    Function DivInt(iOther : Integer) : TSmartDecimal; Overload;
    Function Modulo(oOther : TSmartDecimal) : TSmartDecimal; Overload;
    Function Modulo(iOther : Integer) : TSmartDecimal; Overload;
    Function Add(oOther : TSmartDecimal) : TSmartDecimal; Overload;
    Function Add(iOther : Integer) : TSmartDecimal; Overload;
    Function Subtract(iOther : Integer) : TSmartDecimal; Overload;
    Function Subtract(oOther : TSmartDecimal) : TSmartDecimal; Overload;
    Function Equals(oOther : TSmartDecimal) : Boolean; overload;
    Function Compares(oOther : TSmartDecimal) : Integer; overload;

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


class function TSmartDecimalHelper.CheckValue(sValue: String): boolean;
begin
  if (sValue= '') or (sValue = '-') then
    result := false
  else if pos('e', lowercase(sValue)) > 0 then
    result := CheckValueScientific(lowercase(sValue))
  else
    result := CheckValueDecimal(sValue);
end;

class function TSmartDecimalHelper.CheckValueDecimal(sValue: String): boolean;
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

class function TSmartDecimalHelper.CheckValueScientific(sValue: String): boolean;
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

Procedure TSmartDecimalHelper.SetValue(sValue : String);
Begin
  if (sValue= '') or (sValue = '-') then
    raise Exception('"'+sValue+'" is not a valid decimal');
  sValue := lowercase(sValue);
  if pos('e', sValue) > 0 then
    SetValueScientific(sValue)
  Else
    SetValueDecimal(sValue);
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

Procedure TSmartDecimalHelper.SetValueDecimal(sValue : String);
var
  iDecimal : integer;
  i : integer;
Begin
  Scientific := false;
  iDecimal := 0;
  Negative := (sValue[1] = '-');
  if Negative then
    delete(sValue, 1, 1);

  while (sValue[1] = '0') And (length(sValue) > 1) Do
    delete(sValue, 1, 1);

  for i := 1 to length(sValue) do
    if (sValue[i] = '.') And (iDecimal = 0) then
      iDecimal := i
    else if not CharInSet(sValue[i], ['0'..'9']) then
      raise Exception.Create('"'+sValue+'" is not a valid decimal');

  if iDecimal = 0 then
  Begin
    Precision := Length(sValue);
    Decimal := Length(sValue)+1;
    Digits := sValue;
  end
  else if iDecimal = length(sValue) then
    raise Exception.Create('"'+sValue+'" is not a valid decimal')
  else
  begin
    Decimal := iDecimal;
    if AllZeros(sValue, 2) then
      Precision := length(sValue) - 1
    Else
      Precision := CountSignificants(sValue);
    Digits := sValue;
    Delete(Digits, Decimal, 1);
    if AllZeros(Digits, 1) then
      inc(Precision)
    else
      while (Digits[1] = '0') Do
      begin
        delete(Digits, 1, 1);
        dec(Decimal);
      end;
  end;
end;

Function TSmartDecimalHelper.Add(iOther : Integer) : TSmartDecimal;
var
  oTemp : TSmartDecimal;
Begin
  oTemp := Valueof(iOther);
  result := Add(oTemp);
end;

Function TSmartDecimalHelper.Add(oOther : TSmartDecimal) : TSmartDecimal;
Begin
  if (Negative = oOther.Negative) then
  Begin
    result := DoAdd(oOther);
    result.Negative := Negative;
  end
  else if (Negative) then
    result := oOther.DoSubtract(self)
  else
    result := DoSubtract(oOther);
end;

Function TSmartDecimalHelper.StringAddition(s1, s2 : String):String;
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

Function TSmartDecimalHelper.DoAdd(oOther : TSmartDecimal) : TSmartDecimal;
var
  iMax : Integer;
  s1, s2, s3 : String;
Begin
  iMax := IntegerMax(Decimal, oOther.Decimal);
  s1 := StringMultiply('0', iMax - Decimal+1) + Digits;
  s2 := StringMultiply('0', iMax - oOther.Decimal+1) + oOther.Digits;
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
      raise exception.create('unhandled')
    else if imax <= length(s3) then
      insert('.', s3, iMax)
    else
      raise exception.create('unhandled')
  end;

  result := valueOf(s3);
  result.Scientific := Scientific or oOther.Scientific;
  // todo: the problem with this is you have to figure out the absolute precision and take the lower of the two, not the relative one
  if Decimal < oOther.Decimal then
    result.Precision := Precision
  else if oOther.Decimal < Decimal then
    result.Precision := oOther.Precision
  else
    result.Precision := IntegerMin(Precision, oOther.Precision);
end;


Function TSmartDecimalHelper.StringSubtraction(s1, s2 : String):String;
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
        raise exception.create('internal logic error')
      else
        s1[i-1] := cdig(dig(s1[i-1])-1);
    end;
    result[i] := cdig(t);
  end;
  assert(c = 0);
end;

Function TSmartDecimalHelper.DoSubtract(oOther : TSmartDecimal) : TSmartDecimal;
var
  iMax : Integer;
  s1, s2, s3 : String;
  bNeg : Boolean;
Begin
  iMax := IntegerMax(Decimal, oOther.Decimal);
  s1 := StringMultiply('0', iMax - Decimal+1) + Digits;
  s2 := StringMultiply('0', iMax - oOther.Decimal+1) + oOther.Digits;
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
      raise exception.create('unhandled')
    else if imax <= length(s3) then
      insert('.', s3, iMax)
    else
      raise exception.create('unhandled');
  end;

  result := valueOf(s3);
  result.Negative := bNeg;
  result.Scientific := Scientific or oOther.Scientific;
  if Decimal < oOther.Decimal then
    Precision := Precision
  else if oOther.Decimal < Decimal then
    Precision := oOther.Precision
  else
    Precision := IntegerMin(Precision, oOther.Precision);
end;

Function TSmartDecimalHelper.Multiply(iOther : Integer) : TSmartDecimal;
var
  oTemp : TSmartDecimal;
Begin
  oTemp := valueOf(iOther);
  result := Multiply(oTemp);
end;

Function TSmartDecimalHelper.Multiply(oOther : TSmartDecimal) : TSmartDecimal;
var
  iMax : Integer;
  s1, s2, s3 : String;
  s : Array of String;
  res : String;
  i, j, c, t : integer;
  iDec : Integer;
  iPrec : Integer;
Begin
  if (self.isZero) or (oOther.IsZero) then
    result := Zero
  else if oOther.isOne then
    result := self
  else
  Begin
    iMax := IntegerMax(Decimal, oOther.Decimal);
    s1 := StringMultiply('0', iMax - Decimal+1) + Digits;
    s2 := StringMultiply('0', iMax - oOther.Decimal+1) + oOther.Digits;
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
      iPrec := oOther.Precision
    else if oOther.IsWholeNumber then
      iPrec := Precision
    Else
      iPrec := IntegerMin(Precision, oOther.Precision);
    while (length(res) > iPrec) And (res[Length(res)] = '0') Do
      delete(res, Length(res), 1);

    result := valueOf(res);
    result.Precision := iPrec;
    result.Decimal := iDec;
    result.Negative := Negative <> oOther.Negative;
    result.Scientific := Scientific or oOther.Scientific;
//    writeln('  '+asdecimal+' x ' +oOther.AsDecimal+' = ' +result.AsDecimal);
  end;
end;

Function TSmartDecimalHelper.Divide(iOther : Integer) : TSmartDecimal;
var
  oTemp : TSmartDecimal;
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

Function TSmartDecimalHelper.Divide(oOther : TSmartDecimal) : TSmartDecimal;
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
  if IsZero Then
    result := Zero
  else if oOther.IsZero then
    raise Exception.create('Attempt to divide '+asString+' by zero')
  else if oOther.isOne then
    result := self
  else
  Begin
    s := '0'+oOther.Digits;
    m := IntegerMax(length(Digits), Length(oOther.Digits)) + 40; // max loops we'll do
    SetLength(tens, 10);
    tens[0] := StringAddition(StringMultiply('0', length(s)), s);
    for i := 1 to 9 do
      tens[i] := StringAddition(tens[i-1], s);
    v := Digits;
    r := '';
    l := 0;
    d := (length(Digits) - Decimal + 1) - (length(oOther.Digits) - oOther.Decimal + 1);

    while length(v) < length(tens[0]) do
    begin
      v := v+'0';
      inc(d);
    end;
    if copy(v, 1, length(oOther.Digits)) < oOther.Digits then
    Begin
      if length(v) = length(tens[0]) then
      begin
        v := v + '0';
        inc(d);
      end;
      w := copy(v, 1, length(oOther.Digits)+1);
      vi := length(w)+1;
    end
    Else
    Begin
      w := '0'+copy(v, 1, length(oOther.Digits));
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
        iPrec := IntegerMax(oOther.Precision, length(r) - d)
      else if oOther.IsWholeNumber then
        iPrec := IntegerMax(Precision, length(r) - d)
      Else
        iPrec := IntegerMax(IntegerMin(Precision, oOther.Precision), length(r) - d);
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
    result.Decimal := length(r) - d + 1;
    result.Negative := Negative <> oOther.Negative;
    result.Precision := iPrec;
    result.Scientific := Scientific or oOther.Scientific;
//    writeln('  '+asdecimal+' / ' +oOther.AsDecimal+' = ' +result.AsDecimal);
  end;
end;

Function TSmartDecimalHelper.DivInt(iOther : Integer) : TSmartDecimal;
var
  oTemp : TSmartDecimal;
Begin
  oTemp := valueOf(iOther);
  result := DivInt(oTemp);
end;


Function TSmartDecimalHelper.DivInt(oOther : TSmartDecimal) : TSmartDecimal;
var
  t : TSmartDecimal;
Begin
  t := Divide(oOther);
  result := t.Trunc;
end;


Function TSmartDecimalHelper.Modulo(iOther : Integer) : TSmartDecimal;
var
  oTemp : TSmartDecimal;
Begin
  oTemp := valueOf(iOther);
  result := Modulo(oTemp);
end;


Function TSmartDecimalHelper.Modulo(oOther : TSmartDecimal) : TSmartDecimal;
var
  t, t2 : TSmartDecimal;
Begin
  t := DivInt(oOther);
  t2 := t.Multiply(oOther);
  result := subtract(t2);
end;



Function TSmartDecimalHelper.Subtract(iOther : Integer) : TSmartDecimal;
var
  oTemp : TSmartDecimal;
Begin
  oTemp := valueOf(iOther);
  result := Subtract(oTemp);
end;

Function TSmartDecimalHelper.Subtract(oOther : TSmartDecimal) : TSmartDecimal;
Begin
  if (Negative and not oOther.Negative) then
  Begin
    result := DoAdd(oOther);
    result.Negative := true;
  end
  Else if (not Negative and oOther.Negative) then
    result := DoAdd(oOther)
  Else if (Negative and oOther.Negative) then
  begin
    result := DoSubtract(oOther);
    result.Negative := not result.Negative;
  end
  Else
  begin
    result := oOther.DoSubtract(self);
    result.Negative := not result.Negative;
  end;
end;

Function TSmartDecimalHelper.Equals(oOther : TSmartDecimal) : Boolean;
Begin
  result := Equals(self, oOther);
end;


Procedure TSmartDecimalHelper.SetValueScientific(sValue: String);
var
  i : integer;
  s, e : String;
begin
  StringSplit(sValue, 'e', s, e);

  if (s= '') or (s = '-') or not StringIsDecimal(s) then
    raise Exception.create('"'+sValue+'" is not a valid decimal (numeric)');
  if (e= '') or (e = '-') or not StringIsDecimal(e) then
    raise Exception.create('"'+sValue+'" is not a valid decimal (exponent)');

  SetValueDecimal(s);
  Scientific := true;

  // now adjust for exponent

  if e[1] = '-' then
    i := 2
  Else
    i := 1;
  while i <= length(e) Do
  begin
    if not CharInSet(e[i], ['0'..'9']) then
      raise Exception.Create('"'+sValue+'" is not a valid decimal');
    inc(i);
  end;
  i := StrToInt(e);
  Decimal := Decimal + i;
end;

Function TSmartDecimalHelper.GetValue: String;
begin
  if Scientific then
    result := GetValueScientific
  Else
    result := GetValueDecimal;
end;
{
Function TSmartDecimal.GetValueByPrecision: String;
begin
  if Scientific then
    result := GetValueScientificByPrecision
  Else
    result := GetValueDecimalByPrecision;
end;
}

Function TSmartDecimalHelper.GetValueScientific: String;
var
  bZero : Boolean;
begin
  result := Digits;
  bZero := AllZeros(result, 1);
  if bZero then
  begin
    if Precision < 2 then
      result := '0e0'
    Else
      result := '0.'+StringMultiply('0', Precision-1)+'e0';
  end
  Else
  begin
    if Length(Digits) > 1 then
      insert('.', result, 2);
    result := result + 'e'+inttostr(Decimal - 2);
  end;
  if Negative and not bZero then
    result := '-' + result;
end;


function TSmartDecimalHelper.immediateLowerBound: TSmartDecimal;
var
  i : integer;
begin
  if IsZero then
  begin
    result := immediateUpperBound;
    result.Negative := true;
  end
  else
  begin
    result := valueOf(AsDecimal);
    inc(result.Precision);
    if Negative then
    result.Digits := result.Digits + StringMultiply('0', 25 - length(result.Digits) - result.Decimal)+'1'
    else
    begin
      i := length(result.Digits);
      result.Digits[i] := char(ord(result.Digits[i]) - 1);
      while (i > 0) and (result.Digits[i] < '0') do
      begin
        result.Digits[i] := '9';
        dec(i);
        result.Digits[i] := char(ord(result.Digits[i]) - 1)
      end;
      assert(i > 0);
    result.Digits := result.Digits + StringMultiply('9', 24 - length(result.Digits))+'9'
    end;
  end;
end;

function TSmartDecimalHelper.immediateUpperBound: TSmartDecimal;
var
  i : integer;
begin
  result := valueOf(AsDecimal);
  inc(result.Precision);
  if not Negative then
    result.Digits := result.Digits + StringMultiply('0', 25 - length(result.Digits) - result.Decimal)+'1'
  else
  begin
    i := length(result.Digits);
    result.Digits[i] := char(ord(result.Digits[i]) - 1);
    while (i > 1) and (result.Digits[i] < '0') do
    begin
      result.Digits[i] := '0';
      dec(i);
      result.Digits[i] := char(ord(result.Digits[i]) - 1);
    end;
    assert(i > 0);
    result.Digits := result.Digits + StringMultiply('0', 24 - length(result.Digits))+'9'
  end;
end;

Function TSmartDecimalHelper.GetValueDecimal: String;
begin
  result := Digits;
  if Decimal <> length(Digits) + 1 then
    if Decimal < 1 then
      result := '0.'+StringMultiply('0', 1-Decimal)+Digits
    Else if Decimal <= length(result) then
      if (Decimal = 1) then
        result := '0.'+result
      Else
        insert('.', result, Decimal)
    Else
      result := result + stringMultiply('0', Decimal - length(result)-1);
  if (Precision = INTEGER_PRECISION) and result.Contains('.') and (AllZerosButLast(result, pos('.', result)+1)) then
    result := copy(result, 1, pos('.', result)-1);
  if Negative and not AllZeros(result, 1) then
    result := '-' + result;
end;

Function TSmartDecimalHelper.cdig(i: integer): Char;
begin
//  assert((i >= 0) and (I <= 9));
  result := char(i + ord('0'));
end;

Function TSmartDecimalHelper.dig(c: Char): integer;
begin
//  assert(c in ['0'..'9']);
  result := ord(c) - ord('0');
end;

Function TSmartDecimalHelper.IsZero: Boolean;
begin
  result := AllZeros(Digits, 1);
end;

function TSmartDecimalHelper.IsNegative: boolean;
begin
  result := Negative;
end;

Function TSmartDecimalHelper.IsOne: Boolean;
var
  oOne : TSmartDecimal;
begin
  oOne := One;
  result := Compares(oOne) = 0;
end;

Function TSmartDecimalHelper.Compares(oOther: TSmartDecimal): Integer;
Begin
  result := Compares(self, oOther);
end;

Function TSmartDecimalHelper.isWholeNumber: Boolean;
begin
  result := pos('.', GetValueDecimal) = 0;
end;

Function CardinalToint64(i : Cardinal):Int64;
Begin
  result := i;
end;

Function TSmartDecimalHelper.Trunc: TSmartDecimal;
begin
  if Decimal < 1 then
    result := Zero
  Else
  begin
    result := self;
    if Length(result.Digits) >= Decimal then
      SetLength(result.Digits, Decimal-1);
    if result.Digits = '' then
    begin
      result.Digits := '0';
      result.Decimal := 2;
    end;
  end;
end;

function TSmartDecimalHelper.upperBound: TSmartDecimal;
var
  i : integer;
begin
  result := valueOf(AsDecimal);
  inc(result.Precision);
  if not Negative then
    result.Digits := result.Digits + '5'
  else
  begin
    i := length(result.Digits);
    result.Digits[i] := char(ord(result.Digits[i]) - 1);
    while (i > 1) and (result.Digits[i] < '0') do
    begin
      result.Digits[i] := '0';
      dec(i);
      result.Digits[i] := char(ord(result.Digits[i]) - 1);
    end;
    assert(i > 0);
    result.Digits := result.Digits + '5'
  end;
end;

Function TSmartDecimalHelper.AsCardinal: Cardinal;
var
  r : Int64;
  m : Int64;
begin
  r := AsInt64;
  if r < 0 then
    raise exception.create('Unable to represent '+AsString+' as an unsigned 4 byte number');
  m := High(Cardinal);
  if r > m then
    raise exception.create('Unable to represent '+AsString+' as an unsigned 4 byte number');
  result := r;
end;

function TSmartDecimalHelper.AsDouble: Double;
begin
  result := StrToFloat(AsScientific);
end;

function TSmartDecimalHelper.AsInt64: Int64;
var
  t : TSmartDecimal;
begin
  if not isWholeNumber then
    raise exception.create('Unable to represent '+AsString+' as an integer');
  t := valueOf(Low(Int64));
  if Compares(self, t) < 0 then
    raise exception.create('Unable to represent '+AsString+' as a signed 8 byte integer');
  t := valueOf(High(Int64));
  if Compares(self, t) > 0 then
    raise exception.create('Unable to represent '+AsString+' as a signed 8 byte integer');
  result := StrToInt64(AsDecimal);
end;

function TSmartDecimalHelper.AsInteger: Integer;
var
  r : Int64;
  m : Int64;
begin
  r := AsInt64;
  m := Low(Integer);
  if r < m then
    raise exception.create('Unable to represent '+AsString+' as a signed 4 byte number');
  m := high(Integer);
  if r > m then
    raise exception.create('Unable to represent '+AsString+' as a signed 4 byte number');
  result := r;
end;

class Function TSmartDecimalHelper.valueOf(value : String) : TSmartDecimal;
begin
  result.SetValue(value);
end;

class Function TSmartDecimalHelper.valueOf(value : Integer) : TSmartDecimal;
begin
  result.SetValue(inttostr(value));
end;

class Function TSmartDecimalHelper.valueOf(value : int64) : TSmartDecimal;
begin
  result.SetValue(inttostr(value));
end;

class Function TSmartDecimalHelper.Equals(oOne, oTwo : TSmartDecimal) : Boolean;
Begin
  result := Compares(oOne, oTwo) = 0;
end;

class Function TSmartDecimalHelper.One: TSmartDecimal;
begin
  result := ValueOf(1);
end;

class Function TSmartDecimalHelper.Zero: TSmartDecimal;
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

class Function TSmartDecimalHelper.Compares(oOne, oTwo: TSmartDecimal): Integer;
var
  iMax : Integer;
  s1, s2 : String;
Begin
  if oOne.Negative and not oTwo.Negative then
    result := -1
  else if not oOne.Negative and oTwo.Negative then
    result := 1
  else
  begin
    iMax := IntegerMax(oOne.Decimal, oTwo.Decimal);
    s1 := round('0'+StringMultiply('0', iMax - oOne.Decimal+1), oOne.Digits, oOne.Precision);
    s2 := round('0'+StringMultiply('0', iMax - oTwo.Decimal+1), oTwo.Digits, oTwo.Precision);
    if Length(s1) < length(s2) then
      s1 := s1 + StringMultiply('0', length(s2) - length(s1))
    else if Length(s2) < length(s1) then
      s2 := s2 + StringMultiply('0', length(s1) - length(s2));
    result := StringCompare(s1, s2);
    if oOne.Negative then
      result := -result;
  End;
end;


Function TSmartDecimalHelper.lowerBound: TSmartDecimal;
var
  i : integer;
begin
  if IsZero then
  begin
    result := upperBound;
    result.Negative := true;
  end
  else
  begin
    result := valueOf(AsDecimal);
    inc(result.Precision);
    if Negative then
      result.Digits := result.Digits + '5'
    else
    begin
      i := length(result.Digits);
      result.Digits[i] := char(ord(result.Digits[i]) - 1);
      while (i > 0) and (result.Digits[i] < '0') do
      begin
        result.Digits[i] := '9';
        dec(i);
        result.Digits[i] := char(ord(result.Digits[i]) - 1)
      end;
      assert(i > 0);
      result.Digits := result.Digits + '5';
    end;
  end;
end;


Function GUIDAsOIDWrong(Const aGUID : TGUID) : String;
var
  sGuid, s : String;
  r1, r2, r3, r4 : int64;
  c : integer;
  b1, b2, b3, b4, bs : TSmartDecimal;
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

  b1 := TSmartDecimal.valueOf(r1);
  b2 := TSmartDecimal.valueOf(r2);
  b3 := TSmartDecimal.valueOf(r3);
  b4 := TSmartDecimal.valueOf(r4);
  bs := TSmartDecimal.valueOf('4294967296');
  b2 := b2.Multiply(bs);
  bs := TSmartDecimal.valueOf('18446744073709551616');
  b3 := b3.Multiply(bs);
  bs := TSmartDecimal.valueOf('79228162514264337593543950336');
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
  b1, b2, b3, b4, bs : TSmartDecimal;
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

  b1 := TSmartDecimal.valueOf(r1);
  b2 := TSmartDecimal.valueOf(r2);
  b3 := TSmartDecimal.valueOf(r3);
  b4 := TSmartDecimal.valueOf(r4);
  bs := TSmartDecimal.valueOf('4294967296');
  b2 := b2.Multiply(bs);
  bs := TSmartDecimal.valueOf('18446744073709551616');
  b3 := b3.Multiply(bs);
  bs := TSmartDecimal.valueOf('79228162514264337593543950336');
  b4 := b4.Multiply(bs);
  b1 := b1.Add(b2);
  b1 := b1.Add(b3);
  b1 := b1.Add(b4);
  result := '2.25.'+b1.AsString;
end;

end.
