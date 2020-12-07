unit fsl_diff;

// adapted from http://flocke.vssd.de/prog/code/pascal/pasdiff/cur/Diff.pas.html

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_base;

const
  MaxDisplayedBlockOfText = 2;

type
  { TDiffHashValue is the result value of the hash function (unsigned!) }
  TDiffHashValue = Cardinal;

  { This record holds a range of lines. "Start" is the zero based starting
    line number and "Stop" the zero based number of the first line *BEHIND*
    the range. So, if Start and Stop are equal, you know the position but
    the range is empty.
  }
  TRangeOfLines = record
    Start: integer;
    Stop: integer;
  end;

  { A TDifference is a found difference between the first (left) and the
    second (right) string array.
  }
  PDifference = ^TDifference;
  TDifference = record
    Left: TRangeOfLines;
    Right: TRangeOfLines;
  end;

  PDifferenceArray = ^TDifferenceArray;
  TDifferenceArray = array [0 .. MaxInt div 32] of TDifference;

  TTextComparerHash = function(const s: string): TDiffHashValue of object; register;
  TTextComparerCompare = function(const s1, s2: string): boolean of object; register;

  { Exception class used by this module.
  }
  ETextComparerError = class(Exception);

  TFslTextComparer = class (TFslObject)
  private
    FSourceName1 : String;
    FSourceName2 : String;
    FSource1 : TStrings;
    FSource2 : TStrings;
    FOutput1 : TStrings;
    FOutput2 : TStrings;

    FHeuristic: integer;
    FIgnoreCase: boolean;
    FIgnoreSpaces: boolean;
    FSpacesAsOne: boolean;

    FDiffs: PDifferenceArray;
    FCount: integer;
    FAlloc: integer;

    FOnHash: TTextComparerHash;
    FOnCompare: TTextComparerCompare;

    function GetDiff(Index: integer): TDifference;
    procedure SetSize(NewCount: integer);
    procedure Clear;

    function DefaultHash(const s: string): TDiffHashValue;
    function DefaultCompare(const s1, s2: string): boolean;
    function Hash_ynn(const s: string): TDiffHashValue; register;
    function Hash_nyn(const s: string): TDiffHashValue; register;
    function Hash_yyn(const s: string): TDiffHashValue; register;
    function Hash_nny(const s: string): TDiffHashValue; register;
    function Hash_yny(const s: string): TDiffHashValue; register;
    function Compare_ynn(const s1, s2: string): boolean; register;
    function Compare_nyn(const s1, s2: string): boolean; register;
    function Compare_yyn(const s1, s2: string): boolean; register;
    function Compare_nny(const s1, s2: string): boolean; register;
    function Compare_yny(const s1, s2: string): boolean; register;
    procedure DoCompare;
    procedure GenerateOutput;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFslTextComparer; overload;

    property SourceName1 : String read FSourceName1 write FSourceName1;
    property SourceName2 : String read FSourceName2 write FSourceName2;
    property Source1 : TStrings read FSource1 write FSource1;
    property Source2 : TStrings read FSource2 write FSource2;
    property Output1 : TStrings read FOutput1 write FOutput1;
    property Output2 : TStrings read FOutput2 write FOutput2;

    property IgnoreCase: boolean read FIgnoreCase write FIgnoreCase;
    property IgnoreSpaces: boolean read FIgnoreSpaces write FIgnoreSpaces;
    property SpacesAsOne: boolean read FSpacesAsOne write FSpacesAsOne;
    property Heuristic: integer read FHeuristic write FHeuristic;

    procedure Execute;

    property Diff[Index: integer]: TDifference read GetDiff; default;
    property Count: integer read FCount;
  end;

implementation

{ TFslTextComparer }

constructor TFslTextComparer.Create;
begin
  inherited;
  FDiffs := nil;
  FCount := 0;
  FAlloc := 0;
  FHeuristic := 0;
end;

destructor TFslTextComparer.Destroy;
begin
  Clear;
  inherited;
end;

{ Internal routine that sets the size of the difference array }

procedure TFslTextComparer.SetSize(NewCount: integer);
begin
  FAlloc := NewCount;
  ReAllocMem(FDiffs, FAlloc * SizeOf(TDifference));
  if FCount > FAlloc then
    FCount := FAlloc;
end;

procedure TFslTextComparer.Clear;
begin
  SetSize(0);
end;

function TFslTextComparer.GetDiff(Index: integer): TDifference;
begin
  if (Index < 0) or (Index >= FCount) then
    raise ETextComparerError.Create('Invalid item index');

  Result := FDiffs^[Index];
end;

function TFslTextComparer.link: TFslTextComparer;
begin
  result := TFslTextComparer(inherited link);
end;

{ The default hash just calculates a simple base 19 checksum over the
  ascii codes of the character string.
}
function TFslTextComparer.DefaultHash(const s: string): TDiffHashValue;
var
  k, len: integer;
begin
  len := Length(s);
  Result := len;
  for k := 1 to len do
    Result := 19 * Result + Ord(s[k]);
end;

{ The default compare function just compares the two strings literally.
}
function TFslTextComparer.DefaultCompare(const s1, s2: string): boolean;
begin
  Result := s1 = s2;
end;

{----------------------------------------------------------------------}

{ This is the big Mojo that does everything. The algorithm is:
  1. Inside a given range of lines from the left and the right file,
     find the largest block of equal lines.
  2. If none is found, mark the range as being different.
  3. If one is found, recursively check the two ranges before and
     after that block for differences.
}
procedure TFslTextComparer.DoCompare;
type
  TFileInfo = record
    Strings: TStrings;
    Count: integer;
    LineHash: array of TDiffHashValue;    // [0 ..     Count - 1]
    HashLine: array of integer;           // [0 .. 2 * Count - 1]
    HashColl: array of integer;           // [0 .. 2 * Count - 1]
  end;

var
  Info1, Info2: TFileInfo;

  procedure InitFileInfo(var Info: TFileInfo; AStrings: TStrings);
  var
    idx, back, coll: Integer;
  begin
    with Info do
    begin
      Strings := AStrings;
      Count := Strings.Count;
      SetLength(LineHash, Count);
      SetLength(HashLine, 2 * Count);
      SetLength(HashColl, 2 * Count);

      if Count < 1 then
        exit;

      // The upper half of HashLine/HashColl does not need to be initialized.
      for idx := 0 to Count - 1 do
      begin
        LineHash[idx] := FOnHash(Strings[idx]);
        HashLine[idx] := -1;
        HashColl[idx] := -1;
      end;

      coll := Count - 1;
      for idx := 0 to Count - 1 do
      begin
        back := integer(Cardinal(LineHash[idx]) mod Cardinal(Count));
        if HashLine[back] < 0 then
          HashLine[back] := idx
        else
        begin
          inc(coll);
          HashLine[coll] := HashLine[back];
          HashColl[coll] := HashColl[back];
          HashLine[back] := idx;
          HashColl[back] := coll;
        end;
      end;
    end;
  end;

  procedure AddDiff(l1a, l1e, l2a, l2e: integer);
  begin
    if (l1a >= l1e) and (l2a >= l2e) then
      exit;

    if FCount >= FAlloc then
      SetSize(FAlloc + 32);

    with FDiffs^[FCount] do
    begin
      Left.Start := l1a;
      Left.Stop := l1e;
      Right.Start := l2a;
      Right.Stop := l2e;
    end;

    inc(FCount);
  end;

  function DistanceLess(l1a, l1e, l2a, l2e: integer;
                        bp1, bp2, np1, np2: integer): boolean;
  var
    t1, b1, t2, b2: integer;
  begin
    t1 := abs((bp1 - l1a) - (bp2 - l2a));
    b1 := abs((l1e - bp1) - (l2e - bp2));
    t2 := abs((np1 - l2a) - (np2 - l2a));
    b2 := abs((l2e - np1) - (l2e - np2));
    Result := (t2 + b2) < (t1 + b1);
  end;

  function CountEqualLines(l1p, l1e, l2p, l2e, bsz: integer): integer;
  var
    max: integer;
  begin
    // Prescan: compare the hashcodes (Index 1 has already been compared)
    max := 1;
    while (l1p + max < l1e) and
          (l2p + max < l2e) and
          (Info1.LineHash[l1p + max] = Info2.LineHash[l2p + max]) do
      inc(max);

    // Better match possible?
    Result := 0;
    if max < bsz then
      exit;

    // Final scan: really compare the strings
    while (Result < max) and
          FOnCompare(Info1.Strings[l1p], Info2.Strings[l2p]) do
    begin
      inc(l1p);
      inc(l2p);
      inc(Result);
    end;
  end;

  procedure DiffRange(l1a, l1e, l2a, l2e: integer);
  var
    bp1, bp2, bsz, scan, idx, pos, cnt: integer;
  begin
    while true do
    begin
      if (l1a >= l1e) or (l2a >= l2e) then
        break;

      bp1 := -1;
      bp2 := -1;
      bsz := 0;

      if l1e - l1a < l2e - l2a then
      begin
        // Block 1 is smaller
        scan := l1a;
        while scan < l1e - bsz do
        begin
          idx := integer(Cardinal(Info1.LineHash[scan]) mod Cardinal(Info2.Count));
          repeat
            pos := Info2.HashLine[idx];

            // Trick: the lines have been added from 0 up to Count-1. So, for
            // colliding hashes, the lines are stored in REVERSE order. Thus,
            // when we find a line BEFORE l2a, we're done with this block.
            // This also catches the -1 at the end of the queue.
            if pos < l2a then
              break;

            if pos + bsz <= l2e then
              if Info1.LineHash[scan] = Info2.LineHash[pos] then
                if (bsz = 0) or
                   (Info1.LineHash[scan + bsz - 1] = Info2.LineHash[pos + bsz - 1]) then
                begin
                  cnt := CountEqualLines(scan, l1e, pos, l2e, bsz);
                  if (cnt > bsz) or
                     ((cnt = bsz) and DistanceLess(l1a, l1e, l2a, l2e, bp1, bp2, scan, pos)) then
                  begin
                    bsz := cnt;
                    bp1 := scan;
                    bp2 := pos;

                    if (FHeuristic > 0) and (bsz >= FHeuristic) then
                      break;
                  end;
                end;

            idx := Info2.HashColl[idx];
          until idx < 0;

          if (FHeuristic > 0) and (bsz >= FHeuristic) then
            break;

          inc(scan);
        end;
      end
      else
      begin
        // Block 2 is smaller
        scan := l2a;
        while scan < l2e - bsz do
        begin
          idx := integer(Cardinal(Info2.LineHash[scan]) mod Cardinal(Info1.Count));
          repeat
            pos := Info1.HashLine[idx];

            // Trick: the lines have been added from 0 up to Count-1. So, for
            // colliding hashes, the lines are stored in REVERSE order. Thus,
            // when we find a line BEFORE l1a, we're done with this block.
            // This also catches the -1 at the end of the queue.
            if pos < l1a then
              break;

            if pos + bsz <= l1e then
              if Info1.LineHash[pos] = Info2.LineHash[scan] then
                if (bsz = 0) or
                   (Info1.LineHash[pos + bsz - 1] = Info2.LineHash[scan + bsz - 1]) then
                begin
                  cnt := CountEqualLines(pos, l1e, scan, l2e, bsz);
                  if (cnt > bsz) or
                     ((cnt = bsz) and DistanceLess(l1a, l1e, l2a, l2e, bp1, bp2, pos, scan)) then
                  begin
                    bsz := cnt;
                    bp1 := pos;
                    bp2 := scan;

                    if (FHeuristic > 0) and (bsz >= FHeuristic) then
                      break;
                  end;
                end;

            idx := Info1.HashColl[idx];
          until idx < 0;

          if (FHeuristic > 0) and (bsz >= FHeuristic) then
            break;

          inc(scan);
        end;
      end;

      if bsz = 0 then
        break;

      // This way the diffs are always sorted correctly
      DiffRange(l1a, bp1, l2a, bp2);
      l1a := bp1 + bsz;
      l2a := bp2 + bsz;

      // This way the recursion would be optimized, but we had to
      // sort the diffs after all passes
      {
      if (bp1 - l1a) + (bp2 - l2a) < (l1e - bp1 - bsz) + (l2e - bp2 - bsz) then
      begin
        DiffRange(l1a, bp1, l2a, bp2);
        l1a := bp1 + bsz;
        l2a := bp2 + bsz;
      end
      else
      begin
        DiffRange(bp1 + bsz, l1e, bp2 + bsz, l2e);
        l1e := bp1;
        l2e := bp2;
      end;
      }
    end;

    AddDiff(l1a, l1e, l2a, l2e);
  end;

begin
  Clear;
  // Delphi does Initialize/Finalize, no need to protect with try-finally
  InitFileInfo(Info1, Source1);
  InitFileInfo(Info2, Source2);
  DiffRange(0, Info1.Count, 0, Info2.Count);
end;

{----------------------------------------------------------------------}

const
  WhiteSpace = [#0, #9, #10, #13, #32];

{ Remove all whitespace characters from the given string.
}
function RemoveSpaces(const s: string): string;
var
  len, p: integer;
  pc: PChar;
begin
  len := Length(s);
  SetLength(Result, len);

  pc := @s[1];
  p := 0;
  while len > 0 do
  begin
    if not CharInSet(pc^, WhiteSpace) then
    begin
      inc(p);
      Result[p] := pc^;
    end;

    inc(pc);
    dec(len);
  end;

  SetLength(Result, p);
end;

{ Remove all leading and trailing whitespace from the given string and
  also reduce all sequences of whitespace characters in the middle to a
  single space character.
}
function ReduceSpaces(const s: string): string;
var
  len, p, sp: integer;
  pc: PChar;
begin
  len := Length(s);
  SetLength(Result, len);

  pc := @s[1];
  p := 0;
  sp := 0;
  while len > 0 do
  begin
    if CharInSet(pc^, WhiteSpace) then
      inc(sp, p)
    else
    begin
      if sp > 0 then
      begin
        sp := 0;
        inc(p);
        Result[p] := ' ';
      end;

      inc(p);
      Result[p] := pc^;
    end;

    inc(pc);
    dec(len);
  end;

  SetLength(Result, p);
end;

{----------------------------------------------------------------------}

{ Five different hash and compare functions for all possible combinations
  of IgnoreCase, IgnoreSpaces, and SpacesAsOne. Naming:

     Hash_abc     a = IgnoreCase (y/n)
  Compare_abc     b = IgnoreSpaces (y/n)
                  c = SpacesAsOne (y/n)
}
function TFslTextComparer.Hash_ynn(const s: string): TDiffHashValue;
begin
  Result := DefaultHash(Lowercase(s));
end;

function TFslTextComparer.Hash_nyn(const s: string): TDiffHashValue;
begin
  Result := DefaultHash(RemoveSpaces(s));
end;

function TFslTextComparer.Hash_yyn(const s: string): TDiffHashValue;
begin
  Result := DefaultHash(Lowercase(RemoveSpaces(s)));
end;

function TFslTextComparer.Hash_nny(const s: string): TDiffHashValue;
begin
  Result := DefaultHash(ReduceSpaces(s));
end;

function TFslTextComparer.Hash_yny(const s: string): TDiffHashValue;
begin
  Result := DefaultHash(Lowercase(ReduceSpaces(s)));
end;

function TFslTextComparer.Compare_ynn(const s1, s2: string): boolean;
begin
  Result := Lowercase(s1) = Lowercase(s2);
end;

function TFslTextComparer.Compare_nyn(const s1, s2: string): boolean;
begin
  Result := RemoveSpaces(s1) = RemoveSpaces(s2);
end;

function TFslTextComparer.Compare_yyn(const s1, s2: string): boolean;
begin
  Result := Lowercase(RemoveSpaces(s1)) = Lowercase(RemoveSpaces(s2));
end;

function TFslTextComparer.Compare_nny(const s1, s2: string): boolean;
begin
  Result := ReduceSpaces(s1) = ReduceSpaces(s2);
end;

function TFslTextComparer.Compare_yny(const s1, s2: string): boolean;
begin
  Result := Lowercase(ReduceSpaces(s1)) = Lowercase(ReduceSpaces(s2));
end;

{ The new compare function just sets OnHash and OnCompare according to the
  settings of IgnoreCase, IgnoreSpaces and SpacesAsOne and calls the old
  compare function to do the work.
}
procedure TFslTextComparer.Execute;
begin
  case 4 * Ord(FIgnoreCase) + 2 * Ord(FIgnoreSpaces) + Ord(FSpacesAsOne) of
    0 + 0 + 0: begin
      // Use the default hash/compare functions
      FOnHash := DefaultHash;
      FOnCompare := DefaultCompare;
    end;
    0 + 0 + 1: begin
      FOnHash := Hash_nny;
      FOnCompare := Compare_nny;
    end;
    0 + 2 + 0,
    0 + 2 + 1: begin
      FOnHash := Hash_nyn;
      FOnCompare := Compare_nyn;
    end;
    4 + 0 + 0: begin
      FOnHash := Hash_ynn;
      FOnCompare := Compare_ynn;
    end;
    4 + 0 + 1: begin
      FOnHash := Hash_yny;
      FOnCompare := Compare_yny;
    end;
    4 + 2 + 0,
    4 + 2 + 1: begin
      FOnHash := Hash_yyn;
      FOnCompare := Compare_yyn;
    end;
  end;

  DoCompare;
  GenerateOutput;
end;

procedure TFslTextComparer.GenerateOutput;
var
  idx: integer;
  Diff: TDifference;
  Line1, Line2: integer;
  Fmt: string;

  function PrintLines1(Upto: integer): integer;
  var
    Cnt: integer;
  begin
    Result := Output1.Count;

    if Upto - Line1 > 2 * MaxDisplayedBlockOfText + 1 then
    begin
      for Cnt := 1 to MaxDisplayedBlockOfText do
      begin
        Output1.Add(Format(Fmt, [Line1 + 1]) + FSource1[Line1]);
        inc(Line1);
      end;

      Output1.Add('...');
      Line1 := Upto - MaxDisplayedBlockOfText;
    end;

    while Line1 < Upto do
    begin
      Output1.Add(Format(Fmt, [Line1 + 1]) + FSource1[Line1]);
      inc(Line1);
    end;

    Result := Output1.Count - Result;
  end;

  function PrintLines2(Upto: integer): integer;
  var
    Cnt: integer;
  begin
    Result := Output2.Count;

    if Upto - Line2 > 2 * MaxDisplayedBlockOfText + 1 then
    begin
      for Cnt := 1 to MaxDisplayedBlockOfText do
      begin
        Output2.Add(Format(Fmt, [Line2 + 1]) + FSource2[Line2]);
        inc(Line2);
      end;

      Output2.Add('...');
      Line2 := Upto - MaxDisplayedBlockOfText;
    end;

    while Line2 < Upto do
    begin
      Output2.Add(Format(Fmt, [Line2 + 1]) + FSource2[Line2]);
      inc(Line2);
    end;

    Result := Output2.Count - Result;
  end;

  procedure AddEmpty(List : TStrings; cnt: integer);
  begin
    while cnt > 0 do
    begin
      List.Add('');
      dec(cnt);
    end;
  end;

begin
  Output1.BeginUpdate;
  Output2.BeginUpdate;
  try
    Output1.Clear;
    Output2.Clear;

    Output1.Add(Format('Lines: %d', [FSource1.Count]));
    Output1.Add('');

    Output2.Add(Format('Lines: %d', [FSource2.Count]));
    Output2.Add('');

    Line1 := FSource1.Count;
    Line2 := FSource2.Count;
    idx := 1;
    while (Line1 > 9) or (Line2 > 9) do
    begin
      inc(idx);
      Line1 := Line1 div 10;
      Line2 := Line2 div 10;
    end;

    Fmt := Format('%%%d.%dd: ', [idx, idx]);

    Line1 := 0;
    Line2 := 0;

    for idx := 0 to Count - 1 do
    begin
      Diff := self.Diff[idx];

      PrintLines1(Diff.Left.Start);
      PrintLines2(Diff.Right.Start);

      AddEmpty(Output2, PrintLines1(Diff.Left.Stop));
      AddEmpty(Output1, PrintLines2(Diff.Right.Stop));
    end;

    PrintLines1(FSource1.Count);
    PrintLines2(FSource2.Count);
  finally
    Output2.EndUpdate;
    Output1.EndUpdate;
  end;
end;

end.

