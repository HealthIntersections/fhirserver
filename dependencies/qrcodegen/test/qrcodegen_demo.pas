unit qrcodegen_demo;

interface

uses SysUtils;

type
  TPrintQRProc = procedure(qrcode : TBytes; name : string) of object;

procedure doBasicDemo;
procedure doSegmentDemo;
procedure doVarietyDemo;
procedure DoMaskDemo;

var
  PrintQRProc : TPrintQRProc;

implementation

uses qrcodegen;

procedure doBasicDemo;
var
  text: string;
  errCorLvl: TErrorCorrectionLevel;
  qrcode, tempBuffer: TBytes;
begin
  text := 'Hello, world!';
  // text := '1234567890';
  // text := 'http://192.168.0.119/tech.html';
  // text := 'LACHLAN';
  errCorLvl := eclLow; // Error correction level
  // Make and print the QR Code symbol
  SetLength(qrcode, QR_BUFFER_LEN_MAX);
  SetLength(tempBuffer, QR_BUFFER_LEN_MAX);
  EncodeText(text, tempBuffer, qrcode, errCorLvl, Low(TQRVersion), High(TQRVersion), qrMaskAuto, true);
  PrintQRProc(qrcode, 'Basic');
end;

procedure doSegmentDemo;

  procedure Silver;
  var
    qrcode, tempBuffer, segBuf0, segBuf1: TBytes;
    segs: TQRSegments;
    silver0, silver1, concat : string;
  begin
    silver0 := 'THE SQUARE ROOT OF 2 IS 1.';
    silver1 := '41421356237309504880168872420969807856967187537694807317667973799';
    SetLength(qrCode, QR_BUFFER_LEN_MAX);
    SetLength(tempBuffer, QR_BUFFER_LEN_MAX);

    concat := silver0 + silver1;
    EncodeText(concat, tempBuffer, qrcode, eclLow, Low(TQRVersion), High(TQRVersion), qrMaskAuto, true);
    PrintQRProc(qrcode, 'Silver1');

    SetLength(segBuf0, CalcQRSegmentBufferSize(qrmALPHANUMERIC, Length(silver0)) * sizeof(byte));
    SetLength(segBuf1, CalcQRSegmentBufferSize(qrmNUMERIC, Length(silver1)) * sizeof(byte));
    SetLength(segs, 2);
    segs[0] := NewQRSegmentWithAlphanumeric(silver0, segBuf0);
    segs[1] := NewQRSegmentWithNumeric(silver1, segBuf1);
    EncodeSegments(segs, { sizeof(segs) / sizeof(segs[0]), } eclLow, tempBuffer, qrcode);
    segs := nil;
    PrintQRProc(qrcode, 'Silver2');
  end;

  procedure Golden;
  var
    qrcode, tempBuffer, bytes, segBuf0, segBuf1, segBuf2: TBytes;
    segs: TQRSegments;
    golden0, golden1, golden2, concat : ansistring;
    i : integer;
  begin
    golden0 := 'Golden ratio '#$CF#$86' = 1.';
    golden1 := '6180339887498948482045868343656381177203091798057628621354486227052604628189024497072072041893911374';
    golden2 := '......';
    SetLength(qrcode, QR_BUFFER_LEN_MAX);
    SetLength(tempBuffer, QR_BUFFER_LEN_MAX);

    concat := golden0 + golden1 + golden2;
    EncodeText(concat, tempBuffer, qrcode, eclLow, Low(TQRVersion), High(TQRVersion), qrMaskAuto, true);
    PrintQRProc(qrcode, 'Golden1');

    SetLength(bytes, Length(golden0) * sizeof(AnsiChar));
    for i := Low(golden0) to High(golden0) do
      bytes[i - 1] := Byte(AnsiChar(golden0[i]));
    SetLength(segBuf0, CalcQRSegmentBufferSize(qrmBYTE, Length(golden0)) * sizeof(AnsiChar));
    SetLength(segBuf1, CalcQRSegmentBufferSize(qrmNUMERIC, Length(golden1)) * sizeof(AnsiChar));
    SetLength(segBuf2, CalcQRSegmentBufferSize(qrmALPHANUMERIC, Length(golden2)) * sizeof(AnsiChar));
    SetLength(segs, 3);
    segs[0] := NewQRSegmentWithBinary(bytes, segBuf0);
    segs[1] := NewQRSegmentWithNumeric(golden1, segBuf1);
    segs[2] := NewQRSegmentWithAlphanumeric(golden2, segBuf2);
    bytes := nil;

    EncodeSegments(segs, eclLow, tempBuffer, qrcode);
    PrintQRProc(qrcode, 'Golden2');
  end;

  procedure Modoka;
  const
    kanjiChars: array of integer = [ // Kanji mode encoding (13 bits per character)
      $0035, $1002, $0FC0, $0AED, $0AD7,
      $015C, $0147, $0129, $0059, $01BD,
      $018D, $018A, $0036, $0141, $0144,
      $0001, $0000, $0249, $0240, $0249,
      $0000, $0104, $0105, $0113, $0115,
      $0000, $0208, $01FF, $0008
      ];
  var
    qrcode, tempBuffer, segBuf: TBytes;
    madoka : ansistring;
    seg: TQRSegment;
    i, j : integer;
  begin
    SetLength(qrcode, QR_BUFFER_LEN_MAX);
    SetLength(tempBuffer, QR_BUFFER_LEN_MAX);

    madoka := // Encoded in UTF-8
        #$E3#$80#$8C#$E9#$AD#$94#$E6#$B3#$95#$E5 +
        #$B0#$91#$E5#$A5#$B3#$E3#$81#$BE#$E3#$81 +
        #$A9#$E3#$81#$8B#$E2#$98#$86#$E3#$83#$9E +
        #$E3#$82#$AE#$E3#$82#$AB#$E3#$80#$8D#$E3 +
        #$81#$A3#$E3#$81#$A6#$E3#$80#$81#$E3#$80 +
        #$80#$D0#$98#$D0#$90#$D0#$98#$E3#$80#$80 +
        #$EF#$BD#$84#$EF#$BD#$85#$EF#$BD#$93#$EF +
        #$BD#$95#$E3#$80#$80#$CE#$BA#$CE#$B1#$EF +
        #$BC#$9F;
    EncodeText(madoka, tempBuffer, qrcode, eclLow, Low(TQRVersion), High(TQRVersion), qrMaskAuto, true);
    PrintQRProc(qrcode, 'Madoka1');

    SetLength(segBuf, CalcQRSegmentBufferSize(qrmKANJI, Length(kanjiChars)));
    seg.mode := qrmKANJI;
    seg.numChars := Length(kanjiChars);
    seg.bitLength := 0;
    for i := Low(kanjiChars) to High(kanjiChars) do
    begin
      for j := 12 downto 0 do
      begin
        segBuf[seg.bitLength shr 3] := segBuf[seg.bitLength shr 3] or ((kanjiChars[i] shr j) and 1) shl (7 - (seg.bitLength and 7));
        Inc(seg.bitLength);
      end;
    end;

    seg.data := segBuf;
    EncodeSegments([seg], eclLow, tempBuffer, qrcode);
    PrintQRProc(qrcode, 'Madoka2');
  end;

begin
  Silver;
  Golden;
  Modoka; // kanji, kana, Cyrillic, full-width Latin, Greek characters
end;

procedure doVarietyDemo;

  procedure NumericModeEncoding;
  var
    qrcode, tempBuffer: TBytes;
    ok: boolean;
  begin
    { Numeric mode encoding (3.33 bits per digit) }
    SetLength(qrcode, QR_BUFFER_LEN_MAX);
    SetLength(tempBuffer, QR_BUFFER_LEN_MAX);

    EncodeText('314159265358979323846264338327950288419716939937510', tempBuffer, qrcode, eclMedium, Low(TQRVersion), High(TQRVersion), qrMaskAuto, true);
    PrintQRProc(qrcode, 'Variety1');
  end;

  procedure AlphanumericModeEncoding;
  var
    qrcode, tempBuffer: TBytes;
    ok: boolean;
  begin
    { Alphanumeric mode encoding (5.5 bits per character) }
    SetLength(qrcode, QR_BUFFER_LEN_MAX);
    SetLength(tempBuffer, QR_BUFFER_LEN_MAX);

    EncodeText('DOLLAR-AMOUNT:$39.87 PERCENTAGE:100.00% OPERATIONS:+-*/', tempBuffer, qrcode, eclHigh, Low(TQRVersion), High(TQRVersion), qrMaskAuto, true);
    PrintQRProc(qrcode, 'Variety2');
  end;

  procedure UnicodeTextAsUTF8;
  const
    TEXT: ansistring = #$E3#$81#$93#$E3#$82#$93#$E3#$81#$AB#$E3#$81#$A1 + 'wa' + #$E3#$80#$81
      + #$E4#$B8#$96#$E7#$95#$8C#$EF#$BC#$81#$20#$CE#$B1#$CE#$B2#$CE#$B3#$CE#$B4;
  var
    qrcode, tempBuffer: TBytes;
    ok: boolean;
  begin
    { Unicode text as UTF-8 }
    SetLength(qrcode, QR_BUFFER_LEN_MAX);
    SetLength(tempBuffer, QR_BUFFER_LEN_MAX);

    EncodeText(text, tempBuffer, qrcode, eclQuartile, Low(TQRVersion), High(TQRVersion), qrMaskAuto, true);
    PrintQRProc(qrcode, 'Variety3');
  end;

  procedure ModeratelyLargeUsingLongerText;
  const
    text: ansistring
      = 'Alice was beginning to get very tired of sitting by her sister on the bank, '
      + 'and of having nothing to do: once or twice she had peeped into the book her sister was reading, '
      + 'but it had no pictures or conversations in it, ''and what is the use of a book,'' thought Alice '
      + '''without pictures or conversations?'' So she was considering in her own mind (as well as she could, '
      + 'for the hot day made her feel very sleepy and stupid), whether the pleasure of making a '
      + 'daisy-chain would be worth the trouble of getting up and picking the daisies, when suddenly '
      + 'a White Rabbit with pink eyes ran close by her.';
  var
    qrcode, tempBuffer: TBytes;
    ok: boolean;
  begin
    { Moderately large QR Code using longer text (from Lewis Carroll's Alice in Wonderland) }
    SetLength(qrcode, QR_BUFFER_LEN_MAX);
    SetLength(tempBuffer, QR_BUFFER_LEN_MAX);

    EncodeText(text, tempBuffer, qrcode, eclHigh, Low(TQRVersion), High(TQRVersion), qrMaskAuto, true);
    PrintQRProc(qrcode, 'Variety4');
  end;

begin
  NumericModeEncoding;
  AlphanumericModeEncoding;
  UnicodeTextAsUTF8;
  ModeratelyLargeUsingLongerText;
end;

procedure doMaskDemo;

  procedure ProjectNayukiURL;
  var
    qrcode, tempBuffer: TBytes;
    ok: boolean;
  begin
    SetLength(qrcode, QR_BUFFER_LEN_MAX);
    SetLength(tempBuffer, QR_BUFFER_LEN_MAX);

    EncodeText('https://www.nayuki.io/', tempBuffer, qrcode, eclHigh, Low(TQRVersion), High(TQRVersion), qrMaskAuto, true);
    PrintQRProc(qrcode, 'Mask1');

    EncodeText('https://www.nayuki.io/', tempBuffer, qrcode, eclHigh, Low(TQRVersion), High(TQRVersion), qrMask3, true);
    PrintQRProc(qrcode, 'Mask2');
  end;

  procedure ChineseTextAsUTF8;
  const
    text: ansistring
      = #$E7#$B6#$AD#$E5#$9F#$BA#$E7#$99#$BE#$E7#$A7#$91#$EF#$BC#$88#$57#$69#$6B#$69#$70
      + #$65#$64#$69#$61#$EF#$BC#$8C#$E8#$81#$86#$E8#$81#$BD#$69#$2F#$CB#$8C#$77#$C9#$AA
      + #$6B#$E1#$B5#$BB#$CB#$88#$70#$69#$CB#$90#$64#$69#$2E#$C9#$99#$2F#$EF#$BC#$89#$E6
      + #$98#$AF#$E4#$B8#$80#$E5#$80#$8B#$E8#$87#$AA#$E7#$94#$B1#$E5#$85#$A7#$E5#$AE#$B9
      + #$E3#$80#$81#$E5#$85#$AC#$E9#$96#$8B#$E7#$B7#$A8#$E8#$BC#$AF#$E4#$B8#$94#$E5#$A4
      + #$9A#$E8#$AA#$9E#$E8#$A8#$80#$E7#$9A#$84#$E7#$B6#$B2#$E8#$B7#$AF#$E7#$99#$BE#$E7
      + #$A7#$91#$E5#$85#$A8#$E6#$9B#$B8#$E5#$8D#$94#$E4#$BD#$9C#$E8#$A8#$88#$E7#$95#$AB;
  var
    qrcode, tempBuffer: TBytes;
    ok: boolean;
  begin
    SetLength(qrcode, QR_BUFFER_LEN_MAX);
    SetLength(tempBuffer, QR_BUFFER_LEN_MAX);

    EncodeText(text, tempBuffer, qrcode, eclMedium, Low(TQRVersion), High(TQRVersion), qrMask0, true);
    PrintQRProc(qrcode, 'Mask3');

    EncodeText(text, tempBuffer, qrcode, eclMedium, Low(TQRVersion), High(TQRVersion), qrMask1, true);
    PrintQRProc(qrcode, 'Mask4');

    EncodeText(text, tempBuffer, qrcode, eclMedium, Low(TQRVersion), High(TQRVersion), qrMask5, true);
    PrintQRProc(qrcode, 'Mask5');

    EncodeText(text, tempBuffer, qrcode, eclMedium, Low(TQRVersion), High(TQRVersion), qrMask7, true);
    PrintQRProc(qrcode, 'Mask6');
  end;

begin
  ProjectNayukiURL;
  ChineseTextAsUTF8;
end;

end.
