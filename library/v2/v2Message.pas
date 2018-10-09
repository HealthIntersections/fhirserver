unit v2Message;

interface

uses
  SysUtils,
  FHIR.Support.Base, FHIR.Support.Stream, FHIR.Support.Utilities;

type
  TV2ContentKind = (ckString, ckNull, ckBinary, ckEscape);

  TV2Content = class (TFSLObject)
  private
    FKind: TV2ContentKind;
    FValue: String;
  public
    constructor Create(kind : TV2ContentKind; value : String);
    function link : TV2Content; overload;

    property kind : TV2ContentKind read FKind write FKind;
    property value : String read FValue write FValue;
  end;

  TV2Cell = class (TFSLObject)
  private
    FContents: TFSLList<TV2Content>;
    FComponents: TFSLList<TV2Cell>;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TV2Cell; overload;

    property contents : TFSLList<TV2Content> read FContents;
    property components : TFSLList<TV2Cell> read FComponents;

    function text : String;
  end;

  TV2Field = class (TFSLObject)
  private
    FElements: TFSLList<TV2Cell>;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TV2Field; overload;

    property elements : TFSLList<TV2Cell> read FElements;
  end;

  TV2Segment = class (TFSLObject)
  private
    FFields: TFSLList<TV2Field>;
    FCode: String;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TV2Segment; overload;

    property code : String read FCode write FCode;
    property fields : TFSLList<TV2Field> read FFields;
    function element(index : integer) : TV2Cell;
  end;

  TV2Message = class (TFSLObject)
  private
    FSegments: TFSLList<TV2Segment>;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TV2Message; overload;

    property segments : TFSLList<TV2Segment> read FSegments;
  end;

  TV2DecodingOption = (v2doPreventOddBinaryLengths, v2doMessageHeaderOnly, v2doIgnoreXmlErrors);
  TV2DecodingOptions = set of TV2DecodingOption;

  TV2Parser = class (TFSLObject)
  private
    FSource : TBytes;
    FOptions : TV2DecodingOptions;
    FEscapeCharacter: Char;
    FRepetitionDelimiter: Char;
    FFieldDelimiter: Char;
    FSubComponentDelimiter: Char;
    FComponentDelimiter: Char;

    procedure ReadDelimiters;
    Function isDelimiterEscape(ch : Char) : Boolean;
    Function getDelimiterEscapeChar(ch : Char) : Char;

    Procedure readBinary(oCell : TV2Cell; Const sContent : String; var iCursor : integer);
    Procedure readEscape(oCell : TV2Cell; Const sContent : String; var iCursor : integer);
    procedure parseContent(oCell: TV2Cell; const sContent: String; bNoEscape: Boolean);
    procedure parseCell(oCell : TV2Cell; const sContent : String; bNoEscape : Boolean; cBreak, cSubBreak : char);
    procedure parseField(ofield: TV2Field; var iCursor: Integer);
    procedure parseSegmentInner(oSegment: TV2Segment; var iCursor: Integer);
    procedure parseSegment(oMessage : TV2Message; var iCursor : Integer);
    Procedure DecodeMessage(msg : TV2Message; options : TV2DecodingOptions); Overload; Virtual;
  public
    class function parse(msg : TBytes; options : TV2DecodingOptions = []) : TV2Message; overload;
    class function parse(msg : String; options : TV2DecodingOptions = []) : TV2Message; overload;
    class function parse(msg : TStream; options : TV2DecodingOptions = []) : TV2Message; overload;
    class function parse(msg : TFslStream; options : TV2DecodingOptions = []) : TV2Message; overload;
    class function parse(msg : TFslBuffer; options : TV2DecodingOptions = []) : TV2Message; overload;
  end;

implementation

{ TV2Content }

constructor TV2Content.Create(kind: TV2ContentKind; value: String);
begin
  inherited Create;
  FKind := kind;
  FValue := value;
end;

function TV2Content.link: TV2Content;
begin
  result := TV2Content(inherited link);
end;

{ TV2Cell }

constructor TV2Cell.Create;
begin
  inherited;
  FContents := TFSLList<TV2Content>.create;
  FComponents := TFSLList<TV2Cell>.create;
end;

destructor TV2Cell.Destroy;
begin
  FComponents.Free;
  FContents.free;
  inherited;
end;

function TV2Cell.link: TV2Cell;
begin
  result := TV2Cell(inherited Link);
end;

function TV2Cell.text: String;
var
  cnt : TV2Content;
begin
  if components.Count > 0 then
    result := components[0].text
  else
  begin
    result := '';
    for cnt in contents do
      result := result + cnt.value;
  end;
end;

{ TV2Field }

constructor TV2Field.Create;
begin
  inherited;
  FElements := TFSLList<TV2Cell>.create;
end;

destructor TV2Field.Destroy;
begin
  FElements.Free;
  inherited;
end;

function TV2Field.link: TV2Field;
begin
  result := TV2Field(inherited Link);
end;

{ TV2Segment }

constructor TV2Segment.Create;
begin
  inherited;
  FFields := TFSLList<TV2Field>.create;
end;

destructor TV2Segment.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TV2Segment.element(index: integer): TV2Cell;
begin
  if index >= FFields.Count then
    result := nil
  else if FFields[index].elements.Count > 0 then
    raise EFslException.Create('Repeats encountered at '+inttostr(index))
  else
    result := FFields[index].elements[0];
end;

function TV2Segment.link: TV2Segment;
begin
  result := TV2Segment(inherited link);
end;

{ TV2Message }

constructor TV2Message.Create;
begin
  inherited;
  FSegments := TFSLList<TV2Segment>.create;
end;

destructor TV2Message.Destroy;
begin
  FSegments.Free;
  inherited;
end;

function TV2Message.link: TV2Message;
begin
  result := TV2Message(inherited link);
end;

{ TV2Parser }

Function cleanPacket(packet : TBytes) : TBytes;
begin
  // some systems are somewhat careless about how they wrap a packet
  // some systems prepend a #13 to the message
  // some fail to append a #13.
  // some even send HL7 segment delimiters as  #13#10 instead of #13
  result := BytesReplace(packet, Bytes([13, 10]), Bytes([13]));
  if length(result) > 0 then
  begin
    while result[0] = TByte(13) do
      result := copy(result, 1, length(result) -1);
    while result[length(result)-1] = TByte(13) do
      SetLength(result, length(result) - 1);
    result := BytesAdd(result, TByte(13));
  end;
end;

class function TV2Parser.parse(msg: TBytes; options: TV2DecodingOptions): TV2Message;
var
  this : TV2Parser;
begin
  this := TV2Parser.Create;
  try
    result := TV2Message.Create;
    try
      this.FSource := cleanPacket(msg);
      this.FOptions := options;
      this.decodeMessage(result, options);
      result.link;
    finally
      result.Free;
    end;
  finally
    this.free;
  end;
end;

class function TV2Parser.parse(msg: String; options: TV2DecodingOptions): TV2Message;
begin
  result := parse(TEncoding.UTF8.GetBytes(msg), options);
end;

class function TV2Parser.parse(msg: TStream; options: TV2DecodingOptions): TV2Message;
begin
  result := parse(StreamToBytes(msg), options);
end;

class function TV2Parser.parse(msg: TFslStream; options: TV2DecodingOptions): TV2Message;
var
  s : TVCLStream;
begin
  s := TVCLStream.Create(msg.Link);
  try
    result := parse(StreamToBytes(s));
  finally
    s.Free;
  end;
end;

Function GetDelimiter(sName, sStr : String; iIndex : Integer) : Char;
Begin
  If Length(sStr) >= iIndex Then
    Result := SStr[iIndex]
  Else
    Begin
    Result := #0;
    raise EER7Exception(sName+' Delimiter not found in MSH-2: "'+sStr+'"');
    End;
End;


const
  DEFAULT_DELIMITER_FIELD = '|';
  DEFAULT_DELIMITER_COMPONENT = '^';
  DEFAULT_DELIMITER_SUBCOMPONENT = '&';
  DEFAULT_DELIMITER_REPETITION = '~';
  DEFAULT_CHARACTER_ESCAPE = '\';

procedure TV2Parser.ReadDelimiters;
var
  sLeft : String;
  sRight : String;
begin
  // basics: this checks that the MSH appears to be valid, records the delimiters,
  // and checks the version
  if Not SameBytes(copy(FSource, 0, 3), 'MSH') then
    raise EER7Exception.create('Packet "' + BytesAsMime(copy(FSource, 0, 12)) + '"does not appear to be valid HL7/ER7: starting MSH not found');
  if not BytesContains(FSource, Tbyte(13)) then
    raise EER7Exception.create('Packet does not appear to be valid HL7/ER7: Segment Delimiter not found');

  FFieldDelimiter := Char(FSource[3]);
  StringSplit(BytesAsString(FSource), FFieldDelimiter, sLeft, sRight);
  StringSplit(sRight, FFieldDelimiter, sLeft, sRight);

  FComponentDelimiter := GetDelimiter('Component', sLeft, 1);
  FRepetitionDelimiter := GetDelimiter('Repetition', sLeft, 2);
  FEscapeCharacter := GetDelimiter('Escape', sLeft, 3);
  if Length(sLeft) > 3 then
    FSubComponentDelimiter := GetDelimiter('SubComponent', sLeft, 4)
  Else
    FSubComponentDelimiter := DEFAULT_DELIMITER_SUBCOMPONENT;

  if FComponentDelimiter = FFieldDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FComponentDelimiter+'" is used for both Component and Field');
  if FSubComponentDelimiter = FFieldDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FSubComponentDelimiter+'" is used for both SubComponent and Field');
  if FSubComponentDelimiter = FComponentDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FSubComponentDelimiter+'" is used for both SubComponent and Component');
  if FRepetitionDelimiter = FFieldDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FRepetitionDelimiter+'" is used for both Repetition and Field');
  if FRepetitionDelimiter = FComponentDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FRepetitionDelimiter+'" is used for both Repetition and Component');
  if FRepetitionDelimiter = FSubComponentDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FRepetitionDelimiter+'" is used for both Repetition and SubComponent');
  if FEscapeCharacter = FFieldDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FEscapeCharacter+'" is used for both Escape and Field');
  if FEscapeCharacter = FComponentDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FEscapeCharacter+'" is used for both Escape and Component');
  if FEscapeCharacter = FSubComponentDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FEscapeCharacter+'" is used for both Escape and SubComponent');
  if FEscapeCharacter = FRepetitionDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FEscapeCharacter+'" is used for both Escape and Repetition');
end;


procedure TV2Parser.DecodeMessage(msg : TV2Message; options: TV2DecodingOptions);
var
  iCursor : Integer;
  iLimit : integer;
begin
  // 1. initial check of MSH: version and delimiters
  ReadDelimiters;

  // 2. Decode MSH
  iCursor := 0;
  ParseSegment(msg, iCursor);

  // 4. decode remaining segments
  while (iCursor < length(FSource)) do
    ParseSegment(msg, iCursor);
end;

class function TV2Parser.parse(msg: TFslBuffer; options: TV2DecodingOptions): TV2Message;
begin
  result := parse(msg.AsBytes);
end;

procedure TV2Parser.ParseSegment(oMessage: TV2Message; var iCursor: Integer);
var
  sSegCode : String;
  oSegment : TV2Segment;
begin
  if (iCursor + 3 > length(FSource)) then //we are either missing a #13 or have an invalid segment code
    raise EER7Exception.create('Remaining length in message too short for a valid segment.' + #13 +
                         'Remainder of packet: ' + StringReplace(copy(BytesAsString(FSource), iCursor, length(FSource)), #13, '<CR>'));

  //recognise segment
  sSegCode := BytesAsString(copy(FSource, iCursor, 3));
  inc(iCursor, 3);

  if not StringIsAlphanumeric(sSegCode) then //check that the segcode is a valid length (length must be 3 chars)
    raise EER7Exception.create('Segment code too short or contains invalid content: ' + StringReplace(sSegCode, #13, '<CR>'));

  if not StringArrayExists(['MSH', 'FHS', 'BHS'], sSegCode) and (FSource[iCursor] <> 13) then
      inc(iCursor); // special case: field is it's own delimiter for first MSH field

  oSegment := TV2Segment.Create;
  try
    oSegment.Code := sSegCode;
    oMessage.Segments.Add(oSegment.Link); // do this to provide a model before setting the code
    parseSegmentInner(oSegment, iCursor);
  finally
    oSegment.Free;
  end;
end;

procedure TV2Parser.parseSegmentInner(oSegment: TV2Segment; var iCursor: Integer);
var
  oField : TV2Field;
  iLoop : Integer;
begin
  while FSource[iCursor] <> 13 do
  begin
    oField := TV2Field.create;
    try
      oSegment.Fields.Add(oField.Link);
      ParseField(oField, iCursor);
    finally
      oField.Free;
    end;
  end;

  // special hacks for MSH, etc
  if StringArrayExists(['MSH', 'FHS', 'BHS'], oSegment.Code) then
  begin
//    oSegment.Fields[0].RawContent := FDelimiters.FieldDelimiter;
//    oSegment.Fields[1].Repeats.Clear;
//    oSegment.Fields[1].Components.Clear;
//    oSegment.Fields[1].RawContent := FDelimiters.ComponentDelimiter + FDelimiters.RepetitionDelimiter + FDelimiters.EscapeCharacter + FDelimiters.SubComponentDelimiter;
  end;

  while (iCursor < Length(FSource)) and (FSource[iCursor] = 13) do
    inc(iCursor);
end;


Function BytesSlice(Const sValue : TBytes; iBegin, iEnd : Integer) : TBytes;
Begin
  Result := Copy(sValue, iBegin, iEnd - iBegin + 1);
End;

function ValidHexString(AStr: String; var VMsg: String): Boolean;
var
  i: Integer;
begin
  if (Length(AStr) mod 2 = 1) then
    begin
    Result := False;
    VMsg := 'Length is odd (' + IntToStr(Length(AStr)) + ')';
    end
  else
    begin
    Result := True;
    for i := 1 to Length(AStr) do
      begin
      if not CharInSet(AStr[i], ['0'..'9', 'A'..'F']) then
        begin
        VMsg := 'Character ' + AStr[i] + ' at location ' + IntToStr(i) + ' is not valid';
        Result := False;
        break;
        end;
      end;
    end;
end;

function HexDecode(ARaw: String): String;
var
  i: Integer;
  LResult: PChar;
  s: String;
begin
  if not ValidHexString(ARaw, s) then
    raise EFslException.Create('Error HEX Decoding "' + ARaw + '": ' + s);
  SetLength(Result, Length(ARaw) div 2);
  LResult := PChar(Result);
  for i := 1 to length(ARaw) div 2 do
    begin
    s := Copy(ARaw, (i * 2) - 1, 2);
    LResult[i - 1] := Chr(StrToInt('$' + s));
    end;
end;

procedure TV2Parser.parseField(ofield: TV2Field; var iCursor : Integer);
var
  oFocus : Tv2Cell;
  iStart : Integer;
  bRepeat : Boolean;
  bEscape : Boolean;
//  LContent: String;
begin
  while (iCursor < length(FSource)) and not (FSource[iCursor] in [13, ord(FFieldDelimiter)]) do
  begin
    oFocus := TV2Cell.Create;
    try
      iStart := iCursor;
      bEscape := False;
      while (iCursor < length(FSource)) and not (FSource[iCursor] in [13, ord(FFieldDelimiter), ord(FRepetitionDelimiter)]) do
      Begin
        bEscape := bEscape or (FSource[iCursor] = ord(FEscapeCharacter));
        inc(iCursor);
      End;
      parseCell(oFocus, BytesAsString(BytesSlice(FSource, iStart, iCursor-1)), not bEscape, FComponentDelimiter, FSubComponentDelimiter);
    finally
      oFocus.Free;
    end;
  end;
  if (iCursor < length(FSource)) and (FSource[iCursor] <> 13) then
    inc(iCursor);
end;


procedure TV2Parser.parseCell(oCell : TV2Cell; const sContent : String; bNoEscape : Boolean; cBreak, cSubBreak : char);
var
  oChild : TV2Cell;
  iCount : integer;
  sLeft : String;
  sRight : String;
begin
  // if children are defined or a seperator is found, then we will break content up into children
  if (pos(cBreak, sContent) > 0) or ((cSubBreak <> #0) and (pos(cSubBreak, sContent) > 0)) then
  begin
    sRight := sContent;
    while sRight <> '' do
    begin
      StringSplit(sRight, cBreak, sLeft, sRight);
      oChild := TV2Cell.Create;
      try
        oChild.components.Add(oChild.link);
        parseCell(oChild, sLeft, bNoEscape, cSubBreak, #0);
      finally
        oChild.Free;
      end;
    end;
  end
  else
  begin
    if pos(#0, sContent) > 0 Then
      parseContent(oCell, StringStrip(sContent, [#0]), bNoEscape)
    else
      parseContent(oCell, sContent, bNoEscape);
  end;
end;


Procedure TV2Parser.parseContent(oCell : TV2Cell; Const sContent : String; bNoEscape : Boolean);
var
  buf : TStringBuilder;
  iCursor : Integer;
  Procedure Commit();
  Begin
    if buf.Length > 0 Then
      Begin
      oCell.Contents.Add(TV2Content.create(ckString, buf.ToString));
      buf.Clear;
      End;
  End;
Begin
  oCell.contents.Clear;
  if (sContent = FEscapeCharacter) Then
    oCell.Contents.Add(TV2Content.create(ckString, sContent))
  Else If (sContent = '""') then
    oCell.Contents.add(TV2Content.create(ckNull, '""'))
  Else If bNoEscape or (pos(FEscapeCharacter, sContent) = 0) Then
  Begin
    If sContent <> '' Then
      oCell.Contents.Add(TV2Content.create(ckString, sContent));
  End
  Else
  Begin
    buf := TStringBuilder.Create;
    try
      iCursor := 1;
      While (iCursor <= Length(sContent)) Do
        Begin
        if (iCursor < Length(sContent)) and (sContent[iCursor] = FEscapeCharacter) Then
          Begin
          inc(iCursor);
          if (isDelimiterEscape(sContent[iCursor])) Then
          Begin
            buf.append(getDelimiterEscapeChar(sContent[iCursor]));
            inc(iCursor);
            if sContent[iCursor] <> FEscapeCharacter Then
              dec(iCursor); // cause it will be inc next. this is really an error, an improperly terminated escape sequence, but we do not report it
          End
          Else If (sContent[iCursor] = 'X') Then
          Begin
            Commit;
            readBinary(oCell, sContent, iCursor);
          End
          Else
            Begin
            Commit;
            readEscape(oCell, sContent, iCursor);
            End;
          End
        Else
          buf.append(sContent[iCursor]);
        inc(iCursor);
        End;
      Commit;
    finally
      buf.Free;
    end;
  End;
End;

Procedure TV2Parser.ReadBinary(oCell : TV2Cell; Const sContent : String; var iCursor : integer);
var
  sBuffer : String;
  iStart : Integer;
Begin
  inc(iCursor); // skip the X at the start
  iStart := iCursor;
  While (iCursor <= Length(sContent)) And Not (sContent[iCursor] = FEscapeCharacter) Do
    Inc(iCursor);
  If (iCursor > length(sContent)) Then
    raise EER7Exception.Create('unterminated binary escape in '+sContent);
  sBuffer := copy(sContent, iStart, iCursor - iStart);
  if Length(sBuffer) mod 2 = 1 then
    If v2doPreventOddBinaryLengths in FOptions Then
      sBuffer := '0'+sBuffer
    Else
      raise EER7Exception.Create('Odd length of binary escape in "'+sContent+'"');

  oCell.Contents.add(TV2Content.create(ckBinary, HexDecode(sBuffer)));
End;

Procedure TV2Parser.ReadEscape(oCell : TV2Cell; Const sContent : String; var iCursor : integer);
var
  iStart : Integer;
Begin
  iStart := iCursor;
  While (iCursor <= Length(sContent)) And Not (sContent[iCursor] = FEscapeCharacter) Do
    Inc(iCursor);
  If (iCursor > length(sContent)) Then
  Begin
    // very often turkeys sending us the escape character do not escape.
    // rather than raising the error, we are going to treat the escape as an escape
    iCursor := iStart - 1;
    oCell.Contents.add(TV2Content.create(ckString, FEscapeCharacter));
  End
  Else
    oCell.Contents.add(TV2Content.create(ckEscape, copy(sContent, iStart, iCursor - iStart)));
End;

Function TV2Parser.isDelimiterEscape(ch : Char) : Boolean;
Begin
  Result := (ch = 'F') or (ch = 'S') or (ch = 'E') or (ch = 'T') or (ch = 'R');
End;

Function TV2Parser.getDelimiterEscapeChar(ch : Char) : Char;
Begin
  Result := #0;
  if (ch = 'E') Then
    Result := FEscapeCharacter
  else if (ch = 'F') Then
    Result := FFieldDelimiter
  else if (ch = 'S') Then
    Result := FComponentDelimiter
  else if (ch = 'T') Then
    Result := FSubComponentDelimiter
  else if (ch = 'R') Then
    Result := FRepetitionDelimiter
  else
    raise EER7Exception.Create('Illegal escape char '+ch);
End;





end.
