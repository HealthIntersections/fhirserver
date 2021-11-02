unit fsl_unicode;

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


{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, fsl_fpc;

const
  LRE = #$202a;
  RLE = #$202b;
  PDF = #$202c;
  LRO = #$202d;
  RLO = #$202e;
  LRI = #$2066;
  RLI = #$2067;
  FSI = #$2068;
  PDI = #$2069;
  LRM = #$200E;
  RLM = #$200F;
  ALM = #$061C;
  PARA = #10;

  ALL_BIDI_CHARS : TUCharArray = (LRE, RLE, PDF, LRO, RLO, LRI, RLI, FSI, PDI, LRM, RLM, ALM, PARA);
  CONTROL_CHARS_1 : TUCharArray = (LRE, RLE, LRO, RLO, LRM, RLM, ALM);
  CONTROL_CHARS_2 : TUCharArray = (LRI, RLI, FSI);

type

  { TUnicodeUtilities }

  TUnicodeUtilities = class (TObject)
  public
    class function hasUnicodeBiDiChars(src : String) : boolean;
    class function describe(c : UnicodeChar) : String;
    class function replaceBiDiChars(src : String) : String;

    // returns '' if src is well formed, or a description of a structure problem with bi-directional characters
    class function checkUnicodeWellFormed(src : String) : String;
  private
    FList : TObjectList;
    constructor Create;
    function checkWellFormed(src : String) : String;
    procedure popJustOne(chars : TUCharArray);
    procedure popOneAndOthers(chars, others : TUCharArray);
    function summary : String;
  public
    destructor Destroy; override;
  end;


implementation

type

  { TStateStack }

  TStateStack = class
  private
    c : UnicodeChar;
    i : integer;
    constructor create(aC: UnicodeChar; aI : integer);
  end;

function InSet(c : UnicodeChar; arr : TUCharArray) : boolean;
var
  t : UnicodeChar;
begin
  result := false;
  for t in arr do
    if t = c then
      exit(true);
end;

{ TStateStack }

constructor TStateStack.create(aC: UnicodeChar; aI: integer);
begin
  inherited Create;
  c := aC;
  i := aI;
end;


{ TUnicodeUtilities }

class function TUnicodeUtilities.hasUnicodeBiDiChars(src: String): boolean;
var
  c : UnicodeChar;
begin
  result := false;
  for c in unicodeChars(src) do
    if inSet(c, ALL_BIDI_CHARS) then
      exit(true);
end;

class function TUnicodeUtilities.describe(c: UnicodeChar): String;
begin
  case c of
    LRE : result := 'LRE';
    RLE : result := 'RLE';
    PDF : result := 'PDF';
    LRO : result := 'LRO';
    RLO : result := 'RLO';
    LRI : result := 'LRI';
    RLI : result := 'RLI';
    FSI : result := 'FSI';
    PDI : result := 'PDI';
    LRM : result := 'LRM';
    RLM : result := 'RLM';
    ALM : result := 'ALM';
    PARA : result := 'PARA';
  else
    result := c;
  end;
end;

class function TUnicodeUtilities.replaceBiDiChars(src: String): String;
var
  b : TStringBuilder;
  c : UnicodeChar;
begin
  b := TStringBuilder.create;
  try
    for c in unicodeChars(src) do
      if inSet(c, ALL_BIDI_CHARS) then
        b.append('|'+describe(c)+'|')
      else
        b.append(TEncoding.UTF8.getString(TEncoding.UTF8.GetBytes(c)));
    result := b.toString();
  finally
    b.free;
  end;
end;

class function TUnicodeUtilities.checkUnicodeWellFormed(src: String): String;
var
  this : TUnicodeUtilities;
begin
  this := TUnicodeUtilities.create;
  try
    result := this.checkWellFormed(src);
  finally
    this.free;
  end;
end;

constructor TUnicodeUtilities.Create;
begin
  inherited Create;
  FList := TObjectList.create;
  FList.OwnsObjects := true;
end;

function TUnicodeUtilities.checkWellFormed(src: String): String;
var
  i : integer;
  c : UnicodeChar;
begin
  i := 0;
  for c in unicodeChars(src) do
  begin
    inc(i);
    if inSet(c, ALL_BIDI_CHARS) then
    begin
      case c of
        PARA:
          FList.clear();
        LRO, RLO:
          FList.add(TStateStack.create(c, i));
        PDF:
          popJustOne(CONTROL_CHARS_1);
        LRI, RLI, FSI:
          FList.add(TStateStack.create(c, i));
        PDI:
          popOneAndOthers(CONTROL_CHARS_2, CONTROL_CHARS_1);
        LRM, RLM, ALM:
          FList.add(TStateStack.create(c, i));
      end;
    end;
  end;
  if (FList.Count = 0) then
    result := ''
  else
    result := summary;
end;

procedure TUnicodeUtilities.popJustOne(chars: TUCharArray);
begin
  if (FList.count > 0) and InSet(TStateStack(Flist.Last).c, chars) then
    FList.Delete(FList.count-1);
end;

procedure TUnicodeUtilities.popOneAndOthers(chars, others: TUCharArray);
var
  found, done : boolean;
  i : integer;
begin
  found := false;
  for i := 0 to FList.count - 1 do
  begin
    if InSet(TStateStack(Flist[i]).c, chars) then
    begin
      found := true;
      break;
    end;
  end;

  if (found) then
  begin
    while (FList.count > 0) and (InSet(TStateStack(Flist.Last).c, chars) or InSet(TStateStack(Flist.Last).c, others)) do
    begin
      done := InSet(TStateStack(Flist.Last).c, chars);
      FList.Delete(FList.count-1);
      if (done) then
        break;
    end;
  end;
end;

function TUnicodeUtilities.summary: String;
begin
  result := 'Unicode Character '+describe(TStateStack(Flist.Last).c)+' at index '+inttostr(TStateStack(Flist.Last).i)+' has no terminating match';
end;

destructor TUnicodeUtilities.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

end.

