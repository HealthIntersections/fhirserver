unit fui_syn_hl7;

interface

{$I fhir.inc}

uses
  SysUtils, Classes, Graphics, Math,
  SynEditTypes, SynEditHighlighter,
  fsl_utilities,
  v2_base, v2_dictionary;

type
  TtkTokenKind = (
    tkSegmentCode, // first 3 letters of a segment
    tkBadSegment,  // first 3 letters of a segment but not a known segment
    tkControl,     // an HL7 control char as defined in MSH-1/2
    tkEscape,      // an escape sequence
    tkRabbitsEars, // "" in a cell
    tkText,        // otherwise unremarkable text
    tkID,          // an IS field
    tkIDInvalid,   // is field with an invalid code
    tkInvalid,     // something around here is not syntactically or structurally valid
    tkMissing,     // Required element missing
    tkTooLong,     // content too long for field
    tkUnknown,     // not described in dictionary
    tkNull);

  { TSynHL7Syn }

  TSynHL7Syn = class(TSynCustomHighlighter)
  private
    FDictionary : THL7V2Dictionary;

    FStatus : Integer;
    fLine: pchar;
    FLen : Integer;
    FRun: Integer;
    fLineNumber: Integer;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    FInvalidReason : String;

    FFldChar : Char;
    FCompChar : Char;
    FRepChar : Char;
    FEscChar : Char;
    FSCompChar : Char;

    FSegCode : String;
    FField : Integer;
    FContentFound : Boolean;
    FInRepeat : Boolean;
    FComp : Integer;
    FSubComp : Integer;
    FVersion : String;

    FHL7Model : THL7V2Model;
    FHL7Segment : THL7V2ModelSegment;
    FHL7Field : THL7V2ModelField;
    FHL7Comp : THL7V2ModelComponent;
    FHL7SubComp : THL7V2ModelComponent;

    FLastRequired : Boolean;
    FFieldEnd : Integer;

    FSegmentCodeAttri: TSynHighlighterAttributes;
    FBadSegmentCodeAttri: TSynHighlighterAttributes;
    FControlAttri: TSynHighlighterAttributes;
    FEscapeAttri: TSynHighlighterAttributes;
    FRabbitsEarsAttri: TSynHighlighterAttributes;
    FTextAttri: TSynHighlighterAttributes;
    FIDAttri: TSynHighlighterAttributes;
    FIDInvalidAttri: TSynHighlighterAttributes;
    FInvalidAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
    FMissingAttri: TSynHighlighterAttributes;
    FTooLongAttri: TSynHighlighterAttributes;
    FDefAttr : TSynHighlighterAttributes;
    FShowWarnings: Boolean;

    procedure ObserveEncodingChars;
    procedure ObserveVersion;
    procedure ParseSegCode;
    procedure ParseSegContent;
    procedure ClearSegment;
    procedure NextField;
    procedure NextComp;
    procedure NextSubComp;
    procedure SetDictionary(AValue: THL7V2Dictionary);
    procedure SetSegment(ACode : String);
    function InMSHControl : Boolean;
    procedure ParseEscape;
    procedure SeeRepeat;
    function EscapeIsValid: Boolean;
    function isType(ATypes : Array of String; ADefValue : Boolean):Boolean;
    function IsKnown : Boolean;
    function IsID : Boolean;
    procedure SetDictSubComp;
    procedure SetDictComp;
    procedure SetDictField;
    function GetTable: THL7V2ModelTable;
  protected
    function GetIdentChars: TSynIdentChars; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetSampleSource : String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    class function GetLanguageName: string; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: string; LineNumber:Integer); override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ReSetRange; override;
    property IdentChars;

    function DescribePoint(ALine : String; AOffset : integer) : String;
    function inID(ALine : String; AOffset : integer; AList1, AList2 : TStrings) : Boolean;

    property Dictionary : THL7V2Dictionary  read FDictionary write SetDictionary;
    property FldChar : Char read FFldChar;
    property CompChar : Char read FCompChar;
    property RepChar : Char read FRepChar;
    property EscChar : Char read FEscChar;
    property SCompChar : Char read FSCompChar;
  published
    property SegmentCodeAttri: TSynHighlighterAttributes read fSegmentCodeAttri write fSegmentCodeAttri;
    property BadSegmentCodeAttri: TSynHighlighterAttributes read fBadSegmentCodeAttri write fBadSegmentCodeAttri;
    property ControlAttri: TSynHighlighterAttributes read fControlAttri write fControlAttri;
    property EscapeAttri: TSynHighlighterAttributes read fEscapeAttri write fEscapeAttri;
    property RabbitsEarsAttri: TSynHighlighterAttributes read fRabbitsEarsAttri write fRabbitsEarsAttri;
    property TextAttri: TSynHighlighterAttributes read fTextAttri write fTextAttri;
    property IDAttri: TSynHighlighterAttributes read fIDAttri write fIDAttri;
    property IDInvalidAttri: TSynHighlighterAttributes read fIDInvalidAttri write fIDInvalidAttri;
    property InvalidAttri: TSynHighlighterAttributes read fInvalidAttri write fInvalidAttri;
    property UnknownAttri: TSynHighlighterAttributes read fUnknownAttri write fUnknownAttri;
    property MissingAttri: TSynHighlighterAttributes read fMissingAttri write fMissingAttri;
    property TooLongAttri: TSynHighlighterAttributes read fTooLongAttri write fTooLongAttri;
    property ShowWarnings : Boolean read FShowWarnings write FShowWarnings;
  end;

implementation

function GetStringCell(const ADelimitedString: String; ACell: Cardinal; ADelimiter: String): String; Overload;
  // returns the string corresponding to cell ACell in a delimited string
  // first cell is 0. returns '' if ACell > actual number
var
  j, k: Integer;
begin
  Result := ADelimitedString;
  for k := 1 to ACell do
    begin
    j := Pos(ADelimiter, Result);
    if j = 0 then
      begin
      Result := '';
      break;
      end;
    Result := copy(Result, j + length(ADelimiter), length(Result));
    end;
  j := Pos(ADelimiter, Result);
  if j <> 0 then
    Result := copy(Result, 1, j - 1);
end;


{ TSynHL7Syn }

constructor TSynHL7Syn.Create(AOwner: TComponent);
begin
  inherited;
  FSegmentCodeAttri := TSynHighlighterAttributes.create('Segment Code');
  FBadSegmentCodeAttri := TSynHighlighterAttributes.create('Unknown Segment Code');
  FControlAttri := TSynHighlighterAttributes.create('Control Code');
  FEscapeAttri := TSynHighlighterAttributes.create('Escape Sequence');
  FRabbitsEarsAttri := TSynHighlighterAttributes.create('Null Characters');
  FTextAttri := TSynHighlighterAttributes.create('Text');
  FIDAttri := TSynHighlighterAttributes.create('ID Field');
  FIDInvalidAttri := TSynHighlighterAttributes.create('ID Field (Invalid Code)');
  FInvalidAttri := TSynHighlighterAttributes.create('Invalid Structure');
  FUnknownAttri := TSynHighlighterAttributes.create('Unknown Content');
  FMissingAttri := TSynHighlighterAttributes.create('Missing Content');
  FTooLongAttri := TSynHighlighterAttributes.create('Content exceeding allowable length');
  FDefAttr := TSynHighlighterAttributes.create('Default Background');

  FSegmentCodeAttri.Foreground:= clBlack;
  FSegmentCodeAttri.Background:= clWhite;
  FSegmentCodeAttri.Style:= [fsBold];
  FBadSegmentCodeAttri.Foreground:= clBlack;
  FBadSegmentCodeAttri.Background:= $E1D6FF;
  FBadSegmentCodeAttri.Style:= [fsBold];
  FControlAttri.Foreground := clGreen;
  FControlAttri.Style:= [fsBold];
  FControlAttri.Background := clWhite;
  FEscapeAttri.Background := $A0F1A0;
  FTextAttri.Background:= clWhite;
  FRabbitsEarsAttri.Foreground:= clFuchsia;
  FRabbitsEarsAttri.Background := clWhite;
  FIDAttri.Foreground:= clblue;
  FIDInvalidAttri.Foreground:= clblue;
  FIDInvalidAttri.Background := $E1D6FF;
  FInvalidAttri.Background:= clRed;
  FInvalidAttri.Style:= [fsItalic];
  FUnknownAttri.Background := $E1D6FF;
  FMissingAttri.Background := $77BFFF;
  FTooLongAttri.Background := $77BFFF;

  AddAttribute(FSegmentCodeAttri);
  AddAttribute(FBadSegmentCodeAttri);
  AddAttribute(FControlAttri);
  AddAttribute(FEscapeAttri);
  AddAttribute(FRabbitsEarsAttri);
  AddAttribute(FTextAttri);
  AddAttribute(FIDAttri);
  AddAttribute(FIDInvalidAttri);
  AddAttribute(FInvalidAttri);
  AddAttribute(FUnknownAttri);
  AddAttribute(FMissingAttri);
  AddAttribute(FTooLongAttri);
  AddAttribute(FDefAttr);

  ShowWarnings := true;
  ObserveEncodingChars;
  ObserveVersion;

  SetAttributesOnChange(DefHighlightChange);
  fDefaultFilter := 'HL7 Message (*.hl7)|*.hl7';
end;

destructor TSynHL7Syn.Destroy;
begin
  FDictionary.Free;
  inherited Destroy;
end;

function TSynHL7Syn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  Result := FDefAttr;
end;

function TSynHL7Syn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynHL7Syn.GetIdentChars: TSynIdentChars;
begin
  Result := [' '..chr(127)];
  result := result - [FFldChar];
  result := result - [FRepChar];
  result := result - [FCompChar];
  result := result - [FSCompChar];
end;

procedure TSynHL7Syn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := FRun-fTokenPos;
  TokenStart := fLine + fTokenPos;
end;

class function TSynHL7Syn.GetLanguageName: string;
begin
  result := 'HL7 Message';
end;

function TSynHL7Syn.GetRange: Pointer;
begin
  Result := Pointer(FStatus);
end;

function TSynHL7Syn.GetSampleSource: String;
begin
  result := 'not done yet';
end;

function TSynHL7Syn.GetToken: string;
var
  len: Longint;
begin
  Len := (FRun - fTokenPos);
  SetString(Result, (FLine + fTokenPos), len);
end;

function TSynHL7Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkSegmentCode : result := FSegmentCodeAttri;
    tkBadSegment  : result := FBadSegmentCodeAttri;
    tkControl     : result := FControlAttri;
    tkEscape      : result := FEscapeAttri;
    tkRabbitsEars : result := FRabbitsEarsAttri;
    tkText        : result := FTextAttri;
    tkID          : result := FIDAttri;
    tkIDInvalid   : result := FIDInvalidAttri;
    tkInvalid     : result := FInvalidAttri;
    tkMissing     : result := FMissingAttri;
    tkTooLong     : result := FTooLongAttri;
    tkUnknown     : result := FUnknownAttri;
  else
    Result := nil;
  end;
end;

function TSynHL7Syn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynHL7Syn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynHL7Syn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynHL7Syn.Next;
begin
  FTokenPos := FRun;
  if (FRun = 0) and (FLen > 0) then
    ParseSegCode
  else if FRun < FLen then
    ParseSegContent
  else
    FTokenID := tkNull;
end;

function AllUpper(const s:String):Boolean;
begin
  result := Uppercase(s) = s;
end;

procedure TSynHL7Syn.ParseSegCode;
var
  s : String;
begin
  // first 3 letters are segment code
  FRun := Math.Min(3, FLen);
  ClearSegment;
  s := GetToken;
  if FRun = 3 then
    SetSegment(s);
  if (FRun = 3) and (FLen >= 4) and (FLine[3] = FFldChar) and AllUpper(s) then
    if not assigned(FHL7Model) or assigned(FHL7Segment) then
      fTokenID := tkSegmentCode
    else
      begin
      FTokenID := tkBadSegment;
      FInvalidReason := 'Segment "'+s+'" not known';
      end
  else
    begin
    fTokenID := tkBadSegment;
    FInvalidReason := 'Not a valid Segment Code';
    end;
end;

function TSynHL7Syn.InMSHControl : Boolean;
begin
  result := (StrLComp(FLine, 'MSH', 3) = 0) and (FField <= 2);
end;

procedure TSynHL7Syn.ParseSegContent;
begin
  if (FLine[FRun] < ' ') or (FLine[FRun] > char(127)) then
    begin
    inc(FRun);
    FTokenID := tkInvalid;
    FInvalidReason := 'Character '+inttostr(ord(FLine[FRun]))+' is illegal';
    end
  else if (FLine[FRun] = FEscChar) and not InMSHControl then
    begin
    ParseEscape;
    end
  else if FLine[FRun] in [FFldChar, FCompChar, FRepChar, FSCompChar, FEscChar] then
    begin
    if FLine[FRun] = FFldChar then
      begin
      if (StrLComp(FLine, 'MSH', 3) = 0) and (FField = 0) then
        NextField; // first field delimiter is a double field delimiter
      NextField
      end
    else if not InMSHControl then
      begin
      if FLine[FRun] = FCompChar then
        NextComp
      else if FLine[FRun] = FRepChar then
        SeeRepeat
      else if FLine[FRun] = FSCompChar then
        NextSubComp;
      end;
    inc(FRun);
    if not IsKnown then
      FTokenID := tkUnknown
    else if (FLine[FRun-1] = FFldChar) and FLastRequired and FShowWarnings then
      FTokenID := tkMissing
    else
      FTokenID := tkControl;
    end
  else if (FLine[FRun] = '"') and (FRun < FLen-1) and (FLine[FRun+1] = '"') then
    begin
    inc(FRun, 2);
    FContentFound := true;
    if not IsKnown then
      FTokenID := tkUnknown
    else
      FTokenID := tkRabbitsEars;
    end
  else
    begin
    FContentFound := true;
    while (FRun < FLen) and not ((FLine[FRun] < ' ') or (FLine[FRun] > char(127)))
         and not (FLine[FRun] in [FFldChar, FCompChar, FRepChar, FEscChar, FSCompChar])
         and ((FFieldEnd = 0) or (fTokenPos >= FFieldEnd) or (FRun < FFieldEnd)) do
      inc(FRun);
    if not IsKnown then
      FTokenID := tkUnknown
    else if IsID then
      begin
      if FShowWarnings and (GetTable <> Nil) And (GetTable.Items.Count > 0) And (not GetTable.Items.ExistsByCode(GetToken)) then
        begin
        FTokenID := tkIDInvalid;
        FInvalidReason := 'Value "'+GetToken+'" not valid in ID Field from table '+inttostr(GetTable.ID);
        end
      else
        FTokenID := tkID
      end
    else if (FRun > FFieldEnd) and (FFieldEnd > 0) then
      begin
      FTokenID := tkTooLong;
      FInvalidReason := 'Field length max = '+inttostr(FHL7Field.RefDataElement.Length_Old);
      end
    else
      fTokenID := tkText;
    end;
end;

procedure TSynHL7Syn.ObserveEncodingChars;
  function GetEncodingChar(APos : integer; ADefault, ACurrent : Char):Char;
  begin
    if APos < FLen then
      result := FLine[APos]
    else
      result := ADefault;
    if result <> ACurrent then
      inc(FStatus);
  end;
begin
  FFldChar := GetEncodingChar(3, '|', FFldChar);
  FCompChar := GetEncodingChar(4, '^', FCompChar);
  FRepChar := GetEncodingChar(5, '~', FRepChar);
  FEscChar := GetEncodingChar(6, '\', FEscChar);
  FSCompChar := GetEncodingChar(7, '&', FSCompChar);
end;

procedure TSynHL7Syn.ReSetRange;
begin
  FStatus := 0;
end;

procedure TSynHL7Syn.SetLine(const NewValue: string; LineNumber: Integer);
begin
  FLine := pchar(NewValue);
  FLen := length(NewValue);
  FRun := 0;
  FLineNumber := LineNumber;
  if FLineNumber = 0 then
    begin
    ObserveEncodingChars;
    ObserveVersion;
    end;
  Next;
end;

procedure TSynHL7Syn.SetRange(Value: Pointer);
begin
  // we don't do anything here
end;

procedure TSynHL7Syn.ClearSegment;
begin
  FHL7Segment := nil;
  FHL7Field := nil;
  FHL7Comp := nil;
  FLastRequired := false;
  FSegCode := '';
  FField := 0;
  FComp := 0;
  FSubComp := 0;
end;

procedure TSynHL7Syn.NextComp;
begin
  FSubComp := 1;
  inc(FComp);
  SetDictComp;
end;

procedure TSynHL7Syn.NextField;
begin
  FInRepeat := False;
  FLastRequired := not FContentFound and assigned(FHL7Field) and FHL7Field.Required and ((fLineNumber > 0) or (FField > 2));
  FContentFound := false;
  FSubComp := 1;
  FComp := 1;
  inc(FField);
  SetDictField;
end;

procedure TSynHL7Syn.SetDictField;
begin
  FHL7Field := nil;
  if assigned(FHL7Segment) and (FField <= FHL7Segment.Fields.Count) then
    FHL7Field := FHL7Segment.Fields[FField-1];
  if assigned(FHL7Field) and assigned(FHL7Field.RefDataElement) then
    FFieldEnd := FRun + FHL7Field.RefDataElement.Length_Old + 1
  else
    FFieldEnd := 0;
  SetDictComp;
end;

procedure TSynHL7Syn.SetDictComp;
begin
  FHL7Comp := nil;
  if assigned(FHL7Field) and assigned(FHL7Field.RefDataElement) and assigned(FHL7Field.RefDataElement.RefStructure)
          and (FHL7Field.RefDataElement.RefStructure.Components.Count > 0) and (FHL7Field.RefDataElement.RefStructure.Components.Count >= FComp) then
    FHL7Comp := FHL7Field.RefDataElement.RefStructure.Components[FComp-1];
  SetDictSubComp;
end;

procedure TSynHL7Syn.SetDictSubComp;
var
  LStruc : THL7V2ModelStructure;
begin
  FHL7SubComp := nil;
  if assigned(FHL7Comp) and assigned(FHL7Comp.RefDataType) then
    begin
    LStruc := FHL7Model.Structures.GetByName(FHL7Comp.RefDataType.Name);
    if assigned(LStruc) and (LStruc.Components.Count > 0) and (LStruc.Components.Count > FSubComp) then
      FHL7SubComp := LStruc.Components[FSubComp];
    end;
end;

procedure TSynHL7Syn.NextSubComp;
begin
  inc(FSubComp);
  SetDictSubComp;
end;

procedure TSynHL7Syn.SetDictionary(AValue: THL7V2Dictionary);
begin
  FDictionary.Free;
  FDictionary := AValue;
end;

function TSynHL7Syn.EscapeIsValid : Boolean;
var
  s : String;
begin
  s := GetToken;
  result := (s[1] = FEscChar) and (s[length(s)] = FEscChar);
  if result then
    begin
    delete(s, 1, 1);
    delete(s, length(s), 1);
    result := (s <> '');
    if not result then
      exit;
    result := (StringArrayExistsSensitive(['F', 'S', 'T', 'R', 'E'], s) or (s[1] = 'Z'));
    result := result or ((Upcase(s[1]) = 'X') and ((length(s) mod 2) = 1) and AllContentHex(Copy(s, 2, length(s) - 1)));
    result := result or ((Upcase(s[1]) = 'C') and ((length(s) mod 4) = 1) and AllContentHex(Copy(s, 2, length(s) - 1)));
    result := result or ((Upcase(s[1]) = 'M') and ((length(s) mod 6) = 1) and AllContentHex(Copy(s, 2, length(s) - 1))
                  and isType(['PN', 'XPN', 'XCN', 'XON', 'XAD'], true));
    if not result and isType(['ST', 'TX', 'CF'], true) then
      begin
      result := result or StringArrayExistsSensitive(['H', 'N'], s);
      result := result or ((s[1] = '.') and (copy(s, 2, 2) = 'sp') and ((length(s) = 3) or StringIsInteger32(copy(s, 4, $FF))));
      result := result or ((s[1] = '.') and (copy(s, 2, 2) = 'br') and (length(s) = 3));
      result := result or ((s[1] = '.') and (copy(s, 2, 2) = 'fi') and (length(s) = 3));
      result := result or ((s[1] = '.') and (copy(s, 2, 2) = 'nf') and (length(s) = 3));
      result := result or ((s[1] = '.') and (copy(s, 2, 2) = 'in') and StringIsInteger32(copy(s, 4, $FF)));
      result := result or ((s[1] = '.') and (copy(s, 2, 2) = 'ti') and StringIsInteger32(copy(s, 4, $FF)));
      result := result or ((s[1] = '.') and (copy(s, 2, 2) = 'sk') and StringIsInteger32(copy(s, 4, $FF)));
      result := result or ((s[1] = '.') and (copy(s, 2, 2) = 'ce') and (length(s) = 3));
      end;
    end;
end;

procedure TSynHL7Syn.ParseEscape;
begin
  inc(FRun);
  while (FRun < FLen) and (FLine[FRun] <> FEscChar) do
    inc(FRun);
  if FRun < FLen then
    begin
    inc(FRun);
    if EscapeIsValid then
      fTokenID := tkEscape
    else
      begin
      FTokenID := tkInvalid;
      FInvalidReason := 'Contents of Escape are invalid'
      end
    end
  else
    begin
    FTokenID := tkInvalid;
    FInvalidReason := 'Unterminated escape';
    end;
end;

procedure TSynHL7Syn.SeeRepeat;
begin
  FInRepeat := true;
  FSubComp := 1;
  FComp := 1;
  SetDictField;
end;

procedure TSynHL7Syn.SetSegment(ACode: String);
begin
  assert(length(ACode) = 3);
  FSegCode := ACode;
  if FSegCode = 'MSH' then
    ObserveVersion;
  if assigned(FHL7Model) then
    FHL7Segment := FHL7Model.Segments.GetBYCode(ACode)
end;

function TSynHL7Syn.isType(ATypes: array of String; ADefValue: Boolean): Boolean;
begin
 result := ADefValue;
end;

procedure TSynHL7Syn.ObserveVersion;
var
  LOldVer : String;
begin
  FHL7Model := nil;
  LOldVer := FVersion;
  FVersion := GetStringCell(GetStringCell(GetStringCell(GetStringCell(FLine, 0, #13), 11, FFldChar), 0, FCompChar), 0, FSCompChar);
  if (FDictionary <> nil) then
    if StringArrayExists(NAMES_HL7V2_VERSION, FVersion) and (FromVersionCode(FVersion) in FDictionary.Versions) then
      FHL7Model := FDictionary.Model[FromVersionCode(FVersion)];
  if LOldVer <> FVersion then
    inc(FStatus);
end;

function TSynHL7Syn.IsKnown: Boolean;
begin
  result := not FShowWarnings or not assigned(FHL7Model);
  if not result then
    begin
    result := assigned(FHL7Field);
    if result and (FComp > 1) then
      result := assigned(FHL7Comp);
    if result and (FSubComp > 1) then
      result := assigned(FHL7SubComp);
    end;
  if not result then
    FInvalidReason := 'Content not described by message structure';
end;

function TSynHL7Syn.IsID: Boolean;
begin
  if assigned(FHL7SubComp) and assigned(FHL7SubComp.RefDataType) then
    result := FHL7SubComp.RefDataType.Name = 'ID'
  else if assigned(FHL7Comp) and assigned(FHL7Comp.RefDataType) then
    result := FHL7Comp.RefDataType.Name = 'ID'
  else if assigned(FHL7Field) and assigned(FHL7Field.RefDataElement) then
    result := FHL7Field.RefDataElement.Structure = 'ID'
  else
    result := false;
end;

function TSynHL7Syn.DescribePoint(ALine: String; AOffset: integer): String;
var
  LType : String;
  sDesc : String;
begin
  if (ALine = '') or (AOffset <= 4) then
    result := 'Segment Code'
  else
    begin
    ReSetRange;
    SetLine(ALine, MAXINT);
    repeat
      next;
    until GetEOL or (FRun >= AOffset - 1);
    LType := '';
    result := copy(ALine, 1, 3)+'-'+inttostr(FField);
    if assigned(FHL7Field) and assigned(FHL7Field.RefDataElement) then
      begin
      LType := FHL7Field.RefDataElement.Structure;
      sDesc := FHL7Field.RefDataElement.Description;
      end
    else
      LType := '?';
    if (FComp <> 1) or assigned(FHL7Comp) or (FSubComp <> 1) then
      begin
      result := result + '-'+inttostr(FComp);
      if assigned(FHL7Comp) and assigned(FHL7Comp.RefDataType) then
        LType := LType + '.'+FHL7Comp.RefDataType.Name
      else
        LType := LType + '.?';
      if (FSubComp <> 1) or assigned(FHL7SubComp) then
        begin
        result := result + '-'+ inttostr(FSubComp);
        if assigned(FHL7SubComp) and assigned(FHL7SubComp.RefDataType) then
          LType := LType + '.'+FHL7SubComp.RefDataType.Name
        else
          LType := LType + '.?';
        end;
      end;
    result := result + ':'+LType;
    if sDesc <> '' then
      result := result +' [' +sDesc+']';
    if fTokenID in [tkIDInvalid, tkInvalid, tkTooLong, tkUnknown] then
      result := result + ' ('+FInvalidReason+')'
    else
      begin
      Next;
      if FTokenID = tkMissing then
        result := result + ' (Required Field missing)';
      end;
    end;
end;

function TSynHL7Syn.inID(ALine: String; AOffset: integer; AList1, AList2 : TStrings): Boolean;
var
  FTable : THL7V2ModelTable;
  i : integer;
begin
  if (ALine = '') or (AOffset <= 4) then
    result := False
  else
    begin
    ReSetRange;
    SetLine(ALine, MAXINT);
    repeat
      next;
    until GetEOL or (FRun >= AOffset - 1);
    result := (fTokenID in [tkID, tkIDInvalid]) or ((fTokenID = tkControl) and IsID);
    if result then
      begin
      FTable := GetTable;
      for i := 0 to FTable.Items.Count - 1 do
        begin
        AList1.Add(FTable.Items[i].Code +' - '+ FTable.Items[i].Description);
        AList2.Add(FTable.Items[i].Code);
        end;
      end;
    end;
end;

function TSynHL7Syn.GetTable : THL7V2ModelTable;
begin
  if assigned(FHL7SubComp) then
    result := FHL7SubComp.RefTable
  else if assigned(FHL7Comp) then
    result := FHL7Comp.RefTable
  else
    result := FHL7Field.RefDataElement.RefTable;
end;

initialization
  RegisterPlaceableHighlighter(TSynHL7Syn);
end.

