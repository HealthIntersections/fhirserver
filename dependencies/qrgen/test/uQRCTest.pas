unit uQRCTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uQRC, StdCtrls, ExtCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Image1: TImage;
    btnSegments: TButton;
    btnVariety: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    btnMask: TButton;
    procedure btnMaskClick(Sender: TObject);
    procedure btnSegmentsClick(Sender: TObject);
    procedure btnVarietyClick(Sender: TObject);
    procedure Button1Click (Sender : TObject);
    procedure Memo1DblClick (Sender : TObject);
    procedure FormCreate (Sender : TObject);
    procedure Button2Click (Sender : TObject);
    procedure FormDestroy (Sender : TObject);
  private
    procedure doMaskDemo;
    { Private declarations }
  public
    { Public declarations }
    Bitmap1 : TBitmap;

    procedure testReedSolomonComputeDevisor;
    procedure testReedSolomonComputeMultiply;
    procedure testInitializeFunctionModulesEtc;
    procedure testGetSetModule;
    procedure testGetSetModuleRandomly;
    procedure testIsAlphanumeric;
    procedure testGetAlignmentPatternPositions;
    procedure testReedSolomonComputeRemainder;
    procedure testMakeBytes;
    procedure testMakeAlphanumeric;
    procedure testMakeNumeric;
    procedure testMakeEci;
    procedure testGetTotalBits;
    procedure testAddEccAndInterleave;

    procedure doBasicDemo;
    procedure doSegmentDemo;
    procedure doVarietyDemo;
    procedure printQr (qrcode : TBytes; const filename : string);
    procedure btest (a, b : byte);
    procedure booltest (a, b : boolean);
    procedure itest (a, b : integer);
  end;

procedure DoMon (s : string);

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure DoMon (s : string);
begin
  Form1.Memo1.Lines.Add (s);
end;

{ TForm1 }

procedure TForm1.booltest (a, b : boolean);
const
  ft : array[boolean] of string = ('false', 'true');
begin
   if a = b then
    Memo1.Lines.Add ('Test OK - Both ' + ft[a])
  else
    Memo1.Lines.Add ('Test Failed ' + ft[a] + ' v ' + ft[b]);
end;

procedure TForm1.btest (a, b: byte);
begin
  if a = b then
    Memo1.Lines.Add ('Test OK - Both ' + IntToHex (a, 2))
  else
    Memo1.Lines.Add ('Test Failed ' + IntToHex (a, 2) + ' v ' + IntToHex (b, 2));

end;

procedure TForm1.Button1Click (Sender : TObject);
begin
 // testIsAlphanumeric;
//testInitializeFunctionModulesEtc;
//testGetSetModuleRandomly;
//  testReedSolomonComputeRemainder;
  //testGetAlignmentPatternPositions;
  testGetSetModule;
 // testMakeAlphanumeric;
//  testMakeNumeric;
//  testMakeEci;
//  testGetTotalBits;
// testMakeBytes;
// testAddEccAndInterleave;
end;

procedure TForm1.Button2Click (Sender : TObject);
begin
  doBasicDemo;
end;

procedure TForm1.doBasicDemo;
var
  text : string;
  errCorLvl : qrcodegen_Ecc;
  qrcode, tempBuffer : TBytes;
  ok : boolean;
begin
  text := 'Hello, world!';
//  text := '1234567890';
//  text := 'http://192.168.0.119/tech.html';
//  text := 'LACHLAN';
  errCorLvl := qrcodegen_Ecc_LOW;  // Error correction level
  // Make and print the QR Code symbol
  SetLength (qrcode, qrcodegen_BUFFER_LEN_MAX);
  SetLength (tempBuffer, qrcodegen_BUFFER_LEN_MAX);
  try
    ok := qrcodegen_encodeText (text, tempBuffer, qrcode, errCorLvl,
      qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_AUTO, true);
    if ok then
      begin
        DoMon ('Printing QR Code.');
        printQr (qrcode, 'Basic.bin');
      end
     else
       DoMon ('Failed to Encode Text');
  except
    on e : Exception do
      DoMon ('Failed to Encode Text (' + e.Message + ')');
    end;
end;

procedure TForm1.FormCreate (Sender : TObject);
begin
  MonProc := DoMon;
  Bitmap1 := TBitmap.Create;
  Bitmap1.PixelFormat := pf32Bit;
  Bitmap1.SetSize (30, 30);
  Bitmap1.Canvas.Brush.Color := clWhite;
  Bitmap1.Canvas.FillRect(Rect (0, 0, 30, 30));
  Image1.Picture.Assign (Bitmap1);
end;

procedure TForm1.FormDestroy (Sender : TObject);
begin
  Bitmap1.Free;
end;

procedure TForm1.itest (a, b: integer);
begin
  if a = b then
    Memo1.Lines.Add ('Test OK - Both ' + IntToStr (a))
  else
    Memo1.Lines.Add ('Test Failed ' + IntToStr (a) + ' v ' + IntToStr (b));
end;

procedure TForm1.Memo1DblClick(Sender : TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm1.printQr (qrcode : TBytes; const filename : string);
var
  size : integer;
  x, y : integer;
const
  hdr = 2;
begin
	size := qrcodegen_getSize (qrcode);
  Bitmap1.SetSize ((hdr * 2 + size) * 4, (hdr * 2 + size) * 4);
  Bitmap1.Canvas.Brush.Color := clWhite;
  Bitmap1.Canvas.FillRect (Rect (0, 0, Bitmap1.Width - 1, Bitmap1.Height - 1));
	for y := 0 to size - 1 do
    begin
      for x := 0 to size - 1 do
        begin
          if qrcodegen_getModule (qrcode, x, y) then
            Bitmap1.Canvas.Brush.Color := clBlack
          else
            Bitmap1.Canvas.Brush.Color := clWhite;
          Bitmap1.Canvas.FillRect (Rect ((hdr + x) * 4, (hdr + y) * 4, (hdr + x + 1) * 4, (hdr + y + 1) * 4));
        end;
    end;

  WriteToFile(qrcode, size * size, filename);

  Image1.Picture.Assign (Bitmap1);
end;



// Ported from the Java version of the code.
function addEccAndInterleaveReference (const data : TBytes; version : integer; ecl : qrcodegen_Ecc) : TBytes;
type
  TBytess = array of TBytes;
var
  numBlocks, blockEccLen, rawCodewords, numShortBlocks, shortBlockLen : integer;
  generator : TBytes;
  i, j, k : integer;
  datLen : integer;
  blocks : TBytess;
 //tmp1, tmp2 : TBytes;
begin
	// Calculate parameter numbers
	numBlocks := NUM_ERROR_CORRECTION_BLOCKS[integer (ecl)][version];
	blockEccLen := ECC_CODEWORDS_PER_BLOCK[integer (ecl)][version];
	rawCodewords := getNumRawDataModules (version) div 8;
	numShortBlocks := numBlocks - rawCodewords mod numBlocks;
	shortBlockLen := rawCodewords div numBlocks;
  SetLength (blocks, numBlocks);
  SetLength (generator, blockecclen);
  reedSolomonComputeDivisor (blockEccLen, generator);
  k := 0;
  for i := 0 to numBlocks - 1 do
    begin
      SetLength (blocks[i], shortBlockLen);
      if i < numShortBlocks then
        datLen := shortBlockLen - blockEccLen
      else
        datLen := shortBlockLen - blockEccLen + 1;
        for j := 0 to datLen do
          blocks[i][j] := data[j + k];
  // needs to change       reedSolomonComputeRemainder (data[k], datLen, generator, blockEccLen, blocks[shortBlockLen + 1 - blockEccLen]);
        k := k + datLen;
    end;

  SetLength (Result, rawCodewords);
  k := 0;
	for i := 0 to shortBlockLen do
    begin
		  for j := 0 to numBlocks -1 do
        begin
			  // Skip the padding byte in short blocks
          if (i <> shortBlockLen - blockEccLen) or (j >= numShortBlocks) then
            begin
              result[k] := blocks[j][i];
              k := k + 1;
			      end;
		    end;
	  end;
  for i := low (blocks) to high (blocks) do
    SetLength (Blocks[i], 0);
  SetLength (Blocks, 0);
end;

procedure TForm1.btnMaskClick(Sender: TObject);
begin
  doMaskDemo;
end;

      (*

	// Split data into blocks and append ECC to each block
	uint8_t **blocks = malloc(numBlocks * sizeof(uint8_t * ));
	uint8_t *generator = malloc(blockEccLen * sizeof(uint8_t));
	reedSolomonComputeDivisor((int)blockEccLen, generator);
	for (size_t i = 0, k = 0; i < numBlocks; i++) begin
		uint8_t *block = malloc((shortBlockLen + 1) * sizeof(uint8_t));
		size_t datLen = shortBlockLen - blockEccLen + (i < numShortBlocks ? 0 : 1);
		memcpy(block, &data[k], datLen * sizeof(uint8_t));
		reedSolomonComputeRemainder(&data[k], (int)datLen, generator, (int)blockEccLen, &block[shortBlockLen + 1 - blockEccLen]);
		k += datLen;
		blocks[i] = block;
	end;
	free(generator);
	// Interleave (not concatenate) the bytes from every block into a single sequence
	uint8_t *result = malloc(rawCodewords * sizeof(uint8_t));
	for (size_t i = 0, k = 0; i < shortBlockLen + 1; i++) begin
		for (size_t j = 0; j < numBlocks; j++) begin
			// Skip the padding byte in short blocks
			if (i != shortBlockLen - blockEccLen || j >= numShortBlocks) begin
				result[k] = blocks[j][i];
				k++;
			end;
		end;
	end;
	for (size_t i = 0; i < numBlocks; i++)
		free(blocks[i]);
	free(blocks);
	return result;
   *)

procedure TForm1.testAddEccAndInterleave;
var
  version : integer;
  ecl : integer;
  dataLen, dataAndEccLen : integer;
  i : integer;
  pureData, paddedData, actualOutput, expectedOutput : TBytes;
  ok : boolean;
begin
  for version := 1 to 40 do
    begin
		  for ecl := 0 to 3 do
        begin
			    dataLen := getNumDataCodewords (version, qrcodegen_Ecc (ecl));
          SetLength (pureData, dataLen);
			    for i := low  (pureData) to high (pureData) do
          	pureData[i] := random (256);
			    expectedOutput := addEccAndInterleaveReference (pureData, version, qrcodegen_Ecc (ecl));
          dataAndEccLen := getNumRawDataModules(version) div 8;
          SetLength (paddedData, dataAndEccLen);
          for i := 0 to dataLen - 1 do
            paddedData[i] := pureData[i];
          SetLength (actualOutput, dataAndEccLen);
          addEccAndInterleave (paddedData, version, qrcodegen_Ecc (ecl), actualOutput);
          if length (actualOutput) = length (expectedOutput) then
            begin
              ok := true;
              for i := low (actualOutput) to high(expectedOutput) do // dataandecclen
                if actualOutput[i] <> expectedOutput[i] then ok := false;
              if ok then DoMon ('Actual - Expected Match')
              else DoMon ('Actual Expected DONT Match');
            end
          else
            DoMon ('Length mismatch');
     //			assert(memcmp(actualOutput, expectOutput, dataAndEccLen * sizeof(uint8_t)) == 0);
          SetLength (pureData, 0);
          SetLength (expectedOutput, 0);
          SetLength (paddedData, 0);
          SetLength (actualOutput, 0);
		end;
	end;
end;





procedure TForm1.testGetAlignmentPatternPositions;
const
	cases : array [0..11, 0..8] of integer = (
		( 1, 0,  -1,  -1,  -1,  -1,  -1,  -1,  -1),
		( 2, 2,   6,  18,  -1,  -1,  -1,  -1,  -1),
		( 3, 2,   6,  22,  -1,  -1,  -1,  -1,  -1),
		( 6, 2,   6,  34,  -1,  -1,  -1,  -1,  -1),
		( 7, 3,   6,  22,  38,  -1,  -1,  -1,  -1),
		( 8, 3,   6,  24,  42,  -1,  -1,  -1,  -1),
		(16, 4,   6,  26,  50,  74,  -1,  -1,  -1),
		(25, 5,   6,  32,  58,  84, 110,  -1,  -1),
		(32, 6,   6,  34,  60,  86, 112, 138,  -1),
		(33, 6,   6,  30,  58,  86, 114, 142,  -1),
		(39, 7,   6,  26,  54,  82, 110, 138, 166),
		(40, 7,   6,  30,  58,  86, 114, 142, 170));
var
  i, j : integer;
  p : T7Bytes;
  num : integer;
begin
 	for i := 0 to 11 do
    begin
      num := getAlignmentPatternPositions (cases[i, 0], p);
      itest (num, cases[i, 1]);
      for j := 0 to num - 1 do
        itest (p[j], cases[i, 2 + j]);
    end;
end;

procedure TForm1.testGetSetModule;
var
  qrcode : TBytes;
  size : integer;
  x, y : integer;
  light : boolean;
begin
	SetLength (qrcode, qrcodegen_BUFFER_LEN_FOR_VERSION(23));
	initializeFunctionModules (23, qrcode);
	size := qrcodegen_getSize (qrcode);
	for y := 0 to size - 1 do
    begin  // Clear all to light
		  for x := 0 to size - 1 do
			  setModule (qrcode, x, y, false);
	  end;
	for y := 0 to size - 1 do  // Check all light
    begin
      for x := 0 to size - 1 do
        booltest (qrcodegen_getModule(qrcode, x, y), false);
    end;
	for y := 0 to size - 1 do  // Set all to dark
    begin
      for x := 0 to size - 1 do
        setModule (qrcode, x, y, true);
    end;
	for y := 0 to size - 1 do  // Check all dark
    begin
      for x := 0 to size - 1 do
        assert (qrcodegen_getModule(qrcode, x, y) = true);
    end;
	// Set some out of bounds modules to light
	setModuleBounded (qrcode, -1, -1, false);
	setModuleBounded (qrcode, -1, 0, false);
	setModuleBounded (qrcode, 0, -1, false);
	setModuleBounded (qrcode, size, 5, false);
	setModuleBounded (qrcode, 72, size, false);
	setModuleBounded (qrcode, size, size, false);
	for y := 0 to size - 1 do   // Check all dark
    begin
      for x := 0 to size - 1 do
        booltest(qrcodegen_getModule (qrcode, x, y), true);
    end;
	// Set some modules to light
	setModule(qrcode, 3, 8, false);
	setModule(qrcode, 61, 49, false);
	for y := 0 to size - 1 do  // Check most dark
    begin
      for x := 0 to size - 1 do
        begin
          light := ((x = 3) and (y = 8)) or ((x = 61) and (y = 49));
          booltest (qrcodegen_getModule (qrcode, x, y), not light);
        end;
	  end;
end;

procedure TForm1.testGetSetModuleRandomly;
var
  trials : cardinal;
  qrcode : TBytes;
  size : integer;
  x, y : integer;
  i : cardinal;
  isInBounds, oldColor, newColor : boolean;
  modules : array [0..20, 0..20] of boolean;
begin
  SetLength (qrcode, qrcodegen_BUFFER_LEN_FOR_VERSION(1));
	initializeFunctionModules (1, qrcode);
	size := qrcodegen_getSize (qrcode);
	for y := 0 to size - 1 do
    begin
		 for x := 0 to size - 1 do
			modules[y, x] := qrcodegen_getModule (qrcode, x, y);
	  end;
	trials := 100000;
	for i := 0 to trials - 1 do
    begin
  	  x := random(300) mod (size * 2) - size div 2;
		  y := random (300) mod (size * 2) - size div 2;
		  isInBounds := (0 <= x) and (x < size) and (0 <= y) and (y < size);
		  oldColor := isInBounds and modules[y][x];
      if isInBounds then
        booltest (getModule (qrcode, x, y), oldColor);
      booltest(qrcodegen_getModule(qrcode, x, y), oldColor);
      newColor := (random(300) mod 2) = 0;
      if isInBounds then
        modules[y][x] := newColor;
      if isInBounds and (random (300) mod 2 = 0) then
        setModule (qrcode, x, y, newColor)
      else
        setModuleBounded (qrcode, x, y, newColor);
    end;
end;

procedure TForm1.testGetTotalBits;  // this has issues
var
  segs : TSegments;
begin
  itest (getTotalBits (nil, 1), 0);
  itest (getTotalBits (nil, 40), 0);
  DoMon ('First');
  setLength (segs, 1);
  segs[0].mode := qrcodegen_Mode_BYTE;
  segs[0].numChars := 3;
  segs[0].data := nil;
  segs[0].bitLength := 24;
	itest (getTotalBits (segs, 2), 36);
	itest (getTotalBits (segs, 10), 44);
	itest (getTotalBits (segs, 39), 44);
  DoMon ('Second');
  setLength (segs, 4);
  segs[0].mode := qrcodegen_Mode_ECI;
  segs[0].numChars := 0;
  segs[0].data := nil;
  segs[0].bitLength := 8;
  segs[1].mode := qrcodegen_Mode_NUMERIC;
  segs[1].numChars := 7;
  segs[1].data := nil;
  segs[1].bitLength := 24;
  segs[2].mode := qrcodegen_Mode_ALPHANUMERIC;
  segs[2].numChars := 1;
  segs[2].data := nil;
  segs[2].bitLength := 6;
  segs[3].mode := qrcodegen_Mode_KANJI;
  segs[3].numChars := 4;
  segs[3].data := nil;
  segs[3].bitLength := 52;
  itest (getTotalBits (segs, 9), 133);
  itest (getTotalBits (segs, 21), 139);
  itest (getTotalBits (segs, 27), 145);
  DoMon ('Third');
  setLength (segs, 1);
  segs[0].mode := qrcodegen_Mode_BYTE;
  segs[0].numChars := 4093;
  segs[0].data := nil;
  segs[0].bitLength := 32744;
  itest (getTotalBits (segs, 1), -1);
  itest (getTotalBits (segs, 10), 32764);
  itest (getTotalBits (segs, 27), 32764);
  DoMon ('Forth');
  setLength (segs, 5);
  segs[0].mode := qrcodegen_Mode_NUMERIC;
  segs[0].numChars := 2047;
  segs[0].data := nil;
  segs[0].bitLength := 6824;
  segs[1].mode := qrcodegen_Mode_NUMERIC;
  segs[1].numChars := 2047;
  segs[1].data := nil;
  segs[1].bitLength := 6824;
  segs[2].mode := qrcodegen_Mode_NUMERIC;
  segs[2].numChars := 2047;
  segs[2].data := nil;
  segs[2].bitLength := 6824;
  segs[3].mode := qrcodegen_Mode_NUMERIC;
  segs[3].numChars := 2047;
  segs[3].data := nil;
  segs[3].bitLength := 6824;
  segs[4].mode := qrcodegen_Mode_NUMERIC;
  segs[4].numChars := 1617;
  segs[4].data := nil;
  segs[4].bitLength := 5390;
  itest (getTotalBits (segs, 1), -1);
  itest (getTotalBits (segs, 10), 32766);
  itest (getTotalBits (segs, 27), -1);
  DoMon ('Fifth');
  SetLength (segs, 10);
  segs[0].mode := qrcodegen_Mode_KANJI;
  segs[0].numChars := 255;
  segs[0].data := nil;
  segs[0].bitLength := 3315;
  segs[1].mode := qrcodegen_Mode_KANJI;
  segs[1].numChars := 255;
  segs[1].data := nil;
  segs[1].bitLength := 3315;
  segs[2].mode := qrcodegen_Mode_KANJI;
  segs[2].numChars := 255;
  segs[2].data := nil;
  segs[2].bitLength := 3315;
  segs[3].mode := qrcodegen_Mode_KANJI;
  segs[3].numChars := 255;
  segs[3].data := nil;
  segs[3].bitLength := 3315;
  segs[4].mode := qrcodegen_Mode_KANJI;
  segs[4].numChars := 255;
  segs[4].data := nil;
  segs[4].bitLength := 3315;
  segs[5].mode := qrcodegen_Mode_KANJI;
  segs[5].numChars := 255;
  segs[5].data := nil;
  segs[5].bitLength := 3315;
  segs[6].mode := qrcodegen_Mode_KANJI;
  segs[6].numChars := 255;
  segs[6].data := nil;
  segs[6].bitLength := 3315;
  segs[7].mode := qrcodegen_Mode_KANJI;
  segs[7].numChars := 255;
  segs[7].data := nil;
  segs[7].bitLength := 3315;
  segs[8].mode := qrcodegen_Mode_KANJI;
  segs[8].numChars := 255;
  segs[8].data := nil;
  segs[8].bitLength := 3315;
  segs[9].mode := qrcodegen_Mode_ALPHANUMERIC;
  segs[9].numChars := 511;
  segs[9].data := nil;
  segs[9].bitLength := 2811;
  itest (getTotalBits (segs, 9), 32767);
  itest (getTotalBits (segs, 26), -1);
  itest (getTotalBits (segs, 40), -1);
end;

procedure TForm1.testInitializeFunctionModulesEtc;
var            // this seems to test ok
  ver : integer;
  size : integer;
  x, y : integer;
  qrcode : TBytes;
  color : boolean;
  hasLight, hasDark : boolean;
begin
	for ver := 1 to 40 do
    begin
      memo1.Lines.Add ('Testing Version ' + IntToStr (ver));
      SetLength (qrcode, qrcodegen_BUFFER_LEN_FOR_VERSION (ver));
		  initializeFunctionModules (ver, qrcode);
     // memo1.Lines.Add('Initialised Function Modules');
		  size := qrcodegen_getSize (qrcode);
      if ver = 1 then
        itest (size, 21)
      else if ver = 40 then
        itest (size, 177)
      else
        itest (size, ver * 4 + 17);
      hasLight := false;
      hasDark := false;
      for y := 0 to size - 1 do
        begin
          for x := 0 to size - 1 do
            begin
              color := qrcodegen_getModule (qrcode, x, y);
              if color then
                hasDark := true
              else
                hasLight := true;
            end;
        end;
      if hasLight and hasDark then
        Memo1.Lines.Add ('Light Dark Test ok')
      else
        Memo1.Lines.Add ('Light Dark Test Failed');
	end;
end;

procedure TForm1.testIsAlphanumeric;     // seems ok
type
	TestCase = record
		answer : boolean;
		text : string;
	end;
const
  cases : array[0..18] of TestCase = (
    (answer : true; text : ''),
    (answer : true; text : '0'),
    (answer : true; text : 'A'),
    (answer : false; text : 'a'),
    (answer : true; text : ' '),
    (answer : true; text : '.'),
    (answer : true; text : '*'),
    (answer : false; text : ','),
    (answer : false; text : '|'),
    (answer : false; text : '@'),
    (answer : true; text : 'XYZ'),
    (answer : false; text : 'XYZ!'),
    (answer : true; text : '79068'),
    (answer : true; text : '+123 ABC$'),
    (answer : false; text : #01),
    (answer : false; text : #127),
    (answer : false; text : #128),
    (answer : false; text : #192),
    (answer : false; text : #255));
var
  i : integer;
begin
	for i := 0 to 18 do
    begin
    	booltest (qrcodegen_isAlphanumeric (cases[i].text), cases[i].answer);
    end;
end;

procedure TForm1.testMakeAlphanumeric;
var
  buf : TBytes;
  seg : qrcodegen_Segment;
begin
 (* seg := qrcodegen_makeAlphanumeric ('', buf);
  itest (integer (seg.mode), integer (qrcodegen_Mode_ALPHANUMERIC));
  itest (seg.numChars, 0);
  itest (seg.bitLength, 0);  *)
  SetLength (buf, 1);
  seg := qrcodegen_makeAlphanumeric ('A', buf);
  itest (seg.numChars, 1);
	itest (seg.bitLength, 6);
	btest (seg.data[0], $28);
  SetLength (buf, 2);
	seg := qrcodegen_makeAlphanumeric ('%:', buf);
  itest (seg.numChars, 2);
  itest (seg.bitLength, 11);
  btest (seg.data[0], $DB);
  btest (seg.data[1], $40);
  SetLength (buf, 3);
  seg := qrcodegen_makeAlphanumeric ('Q R', buf);
  itest (seg.numChars, 3);
  itest (seg.bitLength,17);
  btest (seg.data[0], $96);
  btest (seg.data[1], $CD);
  btest (seg.data[2], $80);
end;

procedure TForm1.testMakeBytes;
var
  seg : qrcodegen_Segment;
  data, buf : TBytes;
begin
  seg := qrcodegen_makeBytes (nil, buf);
  itest (ord (seg.mode), ord (qrcodegen_Mode_BYTE));
  itest(seg.numChars, 0);
  itest(seg.bitLength, 0);
  SetLength (data, 1);
  data[0] := $00;
  seg := qrcodegen_makeBytes (data, buf);
  itest(seg.numChars, 1);
  itest(seg.bitLength, 8);
  memo1.Lines.Add ('data length ' + IntToStr (length (buf)));
  btest(seg.data[0], $00);
  SetLength (data, 3);
  data[0] := $ef;
  data[1] := $bb;
  data[2] := $bf;
  setLength (buf, 0);
  seg := qrcodegen_makeBytes (data, buf);
  itest (seg.numChars, 3);
  itest (seg.bitLength, 24);
  btest (seg.data[0],$EF);
  btest (seg.data[1], $BB);
  btest (seg.data[2], $BF);
end;

procedure TForm1.testMakeEci;
var
  seg : qrcodegen_Segment;
  buf : TBytes;
begin
  SetLength (buf, 1);
  seg := qrcodegen_makeEci (127, buf);
  itest (integer (seg.mode), integer (qrcodegen_Mode_ECI));
  itest (seg.numChars, 0);
  itest (seg.bitLength, 8);
  btest (seg.data[0], $7F);
  SetLength (buf, 2);
  seg := qrcodegen_makeEci (10345, buf);
  itest(seg.numChars, 0);
  itest(seg.bitLength, 16);
  btest(seg.data[0], $A8);
  btest(seg.data[1], $69);
  SetLength (buf, 3);
  seg := qrcodegen_makeEci (999999, buf);
  itest(seg.numChars, 0);
  itest(seg.bitLength, 24);
  btest(seg.data[0], $CF);
  btest(seg.data[1], $42);
  btest(seg.data[2], $3F);
end;

procedure TForm1.testMakeNumeric;
var
  seg : qrcodegen_Segment;
  buf : TBytes;
begin
  seg := qrcodegen_makeNumeric ('', buf);
  itest (integer (seg.mode), integer (qrcodegen_Mode_NUMERIC));
  itest (seg.numChars, 0);
  itest (seg.bitLength, 0);
  SetLength (buf, 1);
  seg := qrcodegen_makeNumeric ('9', buf);
  itest(seg.numChars, 1);
  itest(seg.bitLength, 4);
  btest (seg.data[0], $90);
  SetLength (buf, 1);
  seg := qrcodegen_makeNumeric ('81', buf);
  itest (seg.numChars, 2);
  itest (seg.bitLength, 7);
  btest (seg.data[0], $A2);
  SetLength (buf, 2);
  seg := qrcodegen_makeNumeric ('673', buf);
  itest (seg.numChars, 3);
  itest (seg.bitLength, 10);
  btest (seg.data[0], $A8);
  btest (seg.data[1], $40);
  SetLength (buf, 5);
  seg := qrcodegen_makeNumeric ('3141592653', buf);
  itest (seg.numChars, 10);
  itest (seg.bitLength, 34);
  btest (seg.data[0], $4E);
  btest(seg.data[1], $89);
  btest(seg.data[2], $F4);
  btest(seg.data[3], $24);
  btest(seg.data[4], $C0);
end;

procedure TForm1.testReedSolomonComputeDevisor;   // seems ok
var
  generator : TBytes;
begin
  SetLength (generator, 30);
	reedSolomonComputeDivisor (1, generator);
	btest (generator[0], $01);
	reedSolomonComputeDivisor (2, generator);
	btest(generator[0], $03);
	btest(generator[1], $02);
	reedSolomonComputeDivisor (5, generator);
	btest(generator[0], $1F);
	btest(generator[1], $C6);
	btest(generator[2], $3F);
	btest(generator[3], $93);
	btest(generator[4], $74);
	reedSolomonComputeDivisor (30, generator);
	btest(generator[ 0], $D4);
	btest(generator[ 1], $F6);
	btest(generator[ 5], $C0);
	btest(generator[12], $16);
	btest(generator[13], $D9);
	btest(generator[20], $12);
	btest(generator[27], $6A);
	btest(generator[29], $96);
end;

procedure TForm1.testReedSolomonComputeMultiply;     // seems ok
const cases : array [0..15, 0..2] of byte = (
		($00, $00, $00),
		($01, $01, $01),
		($02, $02, $04),
		($00, $6E, $00),
		($B2, $DD, $E6),
		($41, $11, $25),
		($B0, $1F, $11),
		($05, $75, $BC),
		($52, $B5, $AE),
		($A8, $20, $A4),
		($0E, $44, $9F),
		($D4, $13, $A0),
		($31, $10, $37),
		($6C, $58, $CB),
		($B6, $75, $3E),
		($FF, $FF, $E2));
var
  i : integer;
begin
	for i := 0 to 15 do
    begin
      btest (reedSolomonMultiply (cases[i, 0], cases[i, 1]), cases[i, 2]);
    end;
end;

procedure TForm1.testReedSolomonComputeRemainder;
var
  data : TBytes;
  generator : TBytes;
  remainder : TBytes;
begin
  SetLength (data, 0);
  SetLength (generator, 3);
  SetLength (remainder, length (generator));

  reedSolomonComputeDivisor (length(generator), generator);
  reedSolomonComputeRemainder (data, length (data), generator, length(generator), remainder);
  btest (remainder[0], 0);
  btest (remainder[1], 0);
  btest (remainder[2], 0);
  SetLength (data, 2); data[0] := 0; data[1] := 1;
  SetLength (generator, 4);
  SetLength (remainder, length (generator));
  reedSolomonComputeDivisor (length (generator), generator);
  reedSolomonComputeRemainder (data, length (data), generator, length(generator), remainder);
  btest (remainder[0], generator[0]);
  btest (remainder[1], generator[1]);
  btest (remainder[2], generator[2]);
  btest (remainder[3], generator[3]);
  SetLength (data, 5); data[0] := $03; data[1] := $3a; data[2] := $60; data[3] := $12; data[4] := $c7;
  SetLength (generator, 5);
  SetLength (remainder, length (generator));
  reedSolomonComputeDivisor (length (generator), generator);
  reedSolomonComputeRemainder (data, length (data), generator, length(generator), remainder);
	btest (remainder[0], $CB);
  btest (remainder[1], $36);
  btest (remainder[2], $16);
  btest (remainder[3], $FA);
  btest (remainder[4], $9D);
	(*
	{
		uint8_t data[43] = {
			0x38, 0x71, 0xDB, 0xF9, 0xD7, 0x28, 0xF6, 0x8E, 0xFE, 0x5E,
			0xE6, 0x7D, 0x7D, 0xB2, 0xA5, 0x58, 0xBC, 0x28, 0x23, 0x53,
			0x14, 0xD5, 0x61, 0xC0, 0x20, 0x6C, 0xDE, 0xDE, 0xFC, 0x79,
			0xB0, 0x8B, 0x78, 0x6B, 0x49, 0xD0, 0x1A, 0xAD, 0xF3, 0xEF,
			0x52, 0x7D, 0x9A,
		};
		uint8_t generator[30];
		uint8_t remainder[ARRAY_LENGTH(generator)];
		reedSolomonComputeDivisor(ARRAY_LENGTH(generator), generator);
		reedSolomonComputeRemainder(data, ARRAY_LENGTH(data), generator, ARRAY_LENGTH(generator), remainder);
		assert(remainder[ 0] == 0xCE);
		assert(remainder[ 1] == 0xF0);
		assert(remainder[ 2] == 0x31);
		assert(remainder[ 3] == 0xDE);
		assert(remainder[ 8] == 0xE1);
		assert(remainder[12] == 0xCA);
		assert(remainder[17] == 0xE3);
		assert(remainder[19] == 0x85);
		assert(remainder[20] == 0x50);
		assert(remainder[24] == 0xBE);
		assert(remainder[29] == 0xB3);
		numTestCases++;
	}
}

*)
end;


procedure TForm1.btnSegmentsClick(Sender: TObject);
begin
  doSegmentDemo;
end;

procedure TForm1.btnVarietyClick(Sender: TObject);
begin
  doVarietyDemo;
end;

procedure TForm1.doSegmentDemo;

  procedure Silver;
  var
    qrcode, tempBuffer, segBuf0, segBuf1 : TBytes;
    ok : boolean;
    segs : TSegments;
  begin
    var silver0 := 'THE SQUARE ROOT OF 2 IS 1.';
    var silver1 := '41421356237309504880168872420969807856967187537694807317667973799';
    SetLength(qrCode, qrcodegen_BUFFER_LEN_MAX);
    SetLength(tempBuffer, qrcodegen_BUFFER_LEN_MAX);

    var concat := silver0 + silver1;
    ok := qrcodegen_encodeText(concat, tempBuffer, qrcode, qrcodegen_Ecc_LOW,
      qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_AUTO, true);
    if ok then
      printQr(qrcode, 'Silver1.bin');

    SetLength(segBuf0, qrcodegen_calcSegmentBufferSize(qrcodegen_Mode_ALPHANUMERIC, Length(silver0)) * sizeof(byte));
    SetLength(segBuf1, qrcodegen_calcSegmentBufferSize(qrcodegen_Mode_NUMERIC, Length(silver1)) * sizeof(byte));
    SetLength(segs, 2);
    segs[0] := qrcodegen_makeAlphanumeric(silver0, segBuf0);
    segs[1] := qrcodegen_makeNumeric(silver1, segBuf1);
    ok := qrcodegen_encodeSegments(segs, {sizeof(segs) / sizeof(segs[0]), }qrcodegen_Ecc_LOW, tempBuffer, qrcode);
    segs := nil;
    if ok then
      printQr(qrcode, 'Silver2.bin');
  end;

  procedure Golden;
  var
    qrcode, tempBuffer, bytes, segBuf0, segBuf1, segBuf2 : TBytes;
    ok : boolean;
    segs : TSegments;
  begin
    var golden0 : ansistring := 'Golden ratio '#$CF#$86' = 1.';
    var golden1 : ansistring := '6180339887498948482045868343656381177203091798057628621354486227052604628189024497072072041893911374';
    var golden2 : ansistring := '......';
    SetLength(qrcode, qrcodegen_BUFFER_LEN_MAX);
    SetLength(tempBuffer, qrcodegen_BUFFER_LEN_MAX);

    var concat : ansistring := golden0 + golden1 + golden2;
    ok := qrcodegen_encodeText(concat, tempBuffer, qrcode, qrcodegen_Ecc_LOW,
                               qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_AUTO, true);
    if ok then
      printQr(qrcode, 'Golden1.bin');

    SetLength(bytes, Length(golden0) * sizeof(AnsiChar));
    for var i := Low(golden0) to High(golden0) do
      bytes[i - 1] := Byte(AnsiChar(golden0[i]));
    SetLength(segBuf0, qrcodegen_calcSegmentBufferSize(qrcodegen_Mode_BYTE, Length(golden0)) * sizeof(AnsiChar));
    SetLength(segBuf1, qrcodegen_calcSegmentBufferSize(qrcodegen_Mode_NUMERIC, Length(golden1)) * sizeof(AnsiChar));
    SetLength(segBuf2, qrcodegen_calcSegmentBufferSize(qrcodegen_Mode_ALPHANUMERIC, Length(golden2)) * sizeof(AnsiChar));
    SetLength(segs, 3);
    segs[0] := qrcodegen_makeBytes(bytes, segBuf0);
    segs[1] := qrcodegen_makeNumeric(golden1, segBuf1);
    segs[2] := qrcodegen_makeAlphanumeric(golden2, segBuf2);
    bytes := nil;

    ok := qrcodegen_encodeSegments(segs, {sizeof(segs) / sizeof(segs[0]), }qrcodegen_Ecc_LOW, tempBuffer, qrcode);
    if ok then
      printQr(qrcode, 'Golden2.bin');
  end;

  procedure Modoka;
  const
    kanjiChars : array of integer = [  // Kanji mode encoding (13 bits per character)
      $0035, $1002, $0FC0, $0AED, $0AD7,
      $015C, $0147, $0129, $0059, $01BD,
      $018D, $018A, $0036, $0141, $0144,
      $0001, $0000, $0249, $0240, $0249,
      $0000, $0104, $0105, $0113, $0115,
      $0000, $0208, $01FF, $0008
		];
  var
    qrcode, tempBuffer, bytes, segBuf : TBytes;
    ok : boolean;
    segs : TSegments;
  begin
		SetLength(qrcode, qrcodegen_BUFFER_LEN_MAX);
		SetLength(tempBuffer, qrcodegen_BUFFER_LEN_MAX);

    var madoka : AnsiString :=  // Encoded in UTF-8
      #$E3#$80#$8C#$E9#$AD#$94#$E6#$B3#$95#$E5 +
      #$B0#$91#$E5#$A5#$B3#$E3#$81#$BE#$E3#$81 +
      #$A9#$E3#$81#$8B#$E2#$98#$86#$E3#$83#$9E +
      #$E3#$82#$AE#$E3#$82#$AB#$E3#$80#$8D#$E3 +
      #$81#$A3#$E3#$81#$A6#$E3#$80#$81#$E3#$80 +
      #$80#$D0#$98#$D0#$90#$D0#$98#$E3#$80#$80 +
      #$EF#$BD#$84#$EF#$BD#$85#$EF#$BD#$93#$EF +
      #$BD#$95#$E3#$80#$80#$CE#$BA#$CE#$B1#$EF +
      #$BC#$9F;
    ok := qrcodegen_encodeText(madoka, tempBuffer, qrcode, qrcodegen_Ecc_LOW,
      qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_AUTO, true);
    if ok then
      printQr(qrcode, 'Madoka1.bin');

    SetLength(segBuf, qrcodegen_calcSegmentBufferSize(qrcodegen_Mode_KANJI, Length(kanjiChars)));
    var seg : qrcodegen_Segment;
    seg.mode := qrcodegen_Mode_KANJI;
    seg.numChars := Length(kanjiChars);
    seg.bitLength := 0;
    for var i := Low(kanjiChars) to High(kanjiChars) do begin
      for var j := 12 downto 0 do begin
        segBuf[seg.bitLength shr 3] := segBuf[seg.bitLength shr 3] or ((kanjiChars[i] shr j) and 1) shl (7 - (seg.bitLength and 7));
        Inc(seg.bitLength);
      end;
    end;

    seg.data := segBuf;
    ok := qrcodegen_encodeSegments([seg], qrcodegen_Ecc_LOW, tempBuffer, qrcode);
    if ok then
      printQr(qrcode, 'Madoka2.bin');
  end;

begin
  Silver;
  Golden;
  Modoka; // kanji, kana, Cyrillic, full-width Latin, Greek characters
end;


procedure TForm1.doVarietyDemo;

  procedure NumericModeEncoding;
  var
    qrcode, tempBuffer: TBytes;
    ok : boolean;
  begin
    { Numeric mode encoding (3.33 bits per digit) }
		SetLength(qrcode, qrcodegen_BUFFER_LEN_MAX);
		SetLength(tempBuffer, qrcodegen_BUFFER_LEN_MAX);

    ok := qrcodegen_encodeText('314159265358979323846264338327950288419716939937510', tempBuffer, qrcode,
          			qrcodegen_Ecc_MEDIUM, qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_AUTO, true);
		if (ok) then
			printQr(qrcode, 'Variety1.bin');
  end;

  procedure AlphanumericModeEncoding;
  var
    qrcode, tempBuffer: TBytes;
    ok : boolean;
  begin
    { Alphanumeric mode encoding (5.5 bits per character) }
		SetLength(qrcode, qrcodegen_BUFFER_LEN_MAX);
		SetLength(tempBuffer, qrcodegen_BUFFER_LEN_MAX);

		ok := qrcodegen_encodeText('DOLLAR-AMOUNT:$39.87 PERCENTAGE:100.00% OPERATIONS:+-*/', tempBuffer, qrcode,
                               qrcodegen_Ecc_HIGH, qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_AUTO, true);
    if (ok) then
			printQr(qrcode, 'Variety2.bin');
  end;

  procedure UnicodeTextAsUTF8;
  const
    TEXT : ansistring = #$E3#$81#$93#$E3#$82#$93#$E3#$81#$AB#$E3#$81#$A1 + 'wa' + #$E3#$80#$81
                      + #$E4#$B8#$96#$E7#$95#$8C#$EF#$BC#$81#$20#$CE#$B1#$CE#$B2#$CE#$B3#$CE#$B4;
  var
    qrcode, tempBuffer: TBytes;
    ok : boolean;
  begin
    { Unicode text as UTF-8 }
		SetLength(qrcode, qrcodegen_BUFFER_LEN_MAX);
		SetLength(tempBuffer, qrcodegen_BUFFER_LEN_MAX);

    ok := qrcodegen_encodeText(text, tempBuffer, qrcode,
                               qrcodegen_Ecc_QUARTILE, qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_AUTO, true);
		if (ok) then
			printQr(qrcode, 'Variety3.bin');
  end;

  procedure ModeratelyLargeUsingLongerText;
  const
    text : ansistring
              = 'Alice was beginning to get very tired of sitting by her sister on the bank, '
              + 'and of having nothing to do: once or twice she had peeped into the book her sister was reading, '
              + 'but it had no pictures or conversations in it, ''and what is the use of a book,'' thought Alice '
              + '''without pictures or conversations?'' So she was considering in her own mind (as well as she could, '
              + 'for the hot day made her feel very sleepy and stupid), whether the pleasure of making a '
              + 'daisy-chain would be worth the trouble of getting up and picking the daisies, when suddenly '
              + 'a White Rabbit with pink eyes ran close by her.';
  var
    qrcode, tempBuffer: TBytes;
    ok : boolean;
  begin
    { Moderately large QR Code using longer text (from Lewis Carroll's Alice in Wonderland) }
		SetLength(qrcode, qrcodegen_BUFFER_LEN_MAX);
		SetLength(tempBuffer, qrcodegen_BUFFER_LEN_MAX);

    ok := qrcodegen_encodeText(text, tempBuffer, qrcode,
                               qrcodegen_Ecc_HIGH, qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_AUTO, true);
    if (ok) then
      printQr(qrcode, 'Variety4.bin');
  end;

begin
  NumericModeEncoding;
  AlphanumericModeEncoding;
  UnicodeTextAsUTF8;
  ModeratelyLargeUsingLongerText;
end;

procedure TForm1.doMaskDemo;

  procedure ProjectNayukiURL;
  var
    qrcode, tempBuffer: TBytes;
    ok : boolean;
  begin
		SetLength(qrcode, qrcodegen_BUFFER_LEN_MAX);
		SetLength(tempBuffer, qrcodegen_BUFFER_LEN_MAX);

    ok := qrcodegen_encodeText('https://www.nayuki.io/', tempBuffer, qrcode,
                                qrcodegen_Ecc_HIGH, qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_AUTO, true);
    if (ok) then
      printQr(qrcode, 'Mask1.bin');

    ok := qrcodegen_encodeText('https://www.nayuki.io/', tempBuffer, qrcode,
                              qrcodegen_Ecc_HIGH, qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_3, true);
    if (ok) then
      printQr(qrcode, 'Mask2.bin');
  end;

  procedure ChineseTextAsUTF8;
  const
    text : ansistring
      = #$E7#$B6#$AD#$E5#$9F#$BA#$E7#$99#$BE#$E7#$A7#$91#$EF#$BC#$88#$57#$69#$6B#$69#$70
      + #$65#$64#$69#$61#$EF#$BC#$8C#$E8#$81#$86#$E8#$81#$BD#$69#$2F#$CB#$8C#$77#$C9#$AA
      + #$6B#$E1#$B5#$BB#$CB#$88#$70#$69#$CB#$90#$64#$69#$2E#$C9#$99#$2F#$EF#$BC#$89#$E6
      + #$98#$AF#$E4#$B8#$80#$E5#$80#$8B#$E8#$87#$AA#$E7#$94#$B1#$E5#$85#$A7#$E5#$AE#$B9
      + #$E3#$80#$81#$E5#$85#$AC#$E9#$96#$8B#$E7#$B7#$A8#$E8#$BC#$AF#$E4#$B8#$94#$E5#$A4
      + #$9A#$E8#$AA#$9E#$E8#$A8#$80#$E7#$9A#$84#$E7#$B6#$B2#$E8#$B7#$AF#$E7#$99#$BE#$E7
      + #$A7#$91#$E5#$85#$A8#$E6#$9B#$B8#$E5#$8D#$94#$E4#$BD#$9C#$E8#$A8#$88#$E7#$95#$AB;
  var
    qrcode, tempBuffer: TBytes;
    ok : boolean;
  begin
		SetLength(qrcode, qrcodegen_BUFFER_LEN_MAX);
		SetLength(tempBuffer, qrcodegen_BUFFER_LEN_MAX);

		ok := qrcodegen_encodeText(text, tempBuffer, qrcode,
			qrcodegen_Ecc_MEDIUM, qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_0, true);
		if (ok) then
			printQr(qrcode, 'Mask3.bin');

		ok := qrcodegen_encodeText(text, tempBuffer, qrcode,
			qrcodegen_Ecc_MEDIUM, qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_1, true);
		if (ok) then
			printQr(qrcode, 'Mask4.bin');

		ok := qrcodegen_encodeText(text, tempBuffer, qrcode,
			qrcodegen_Ecc_MEDIUM, qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_5, true);
		if (ok) then
			printQr(qrcode, 'Mask5.bin');

		ok := qrcodegen_encodeText(text, tempBuffer, qrcode,
			qrcodegen_Ecc_MEDIUM, qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX, qrcodegen_Mask_7, true);
		if (ok) then
			printQr(qrcode, 'Mask6.bin');
  end;

begin
  ProjectNayukiURL;
  ChineseTextAsUTF8;
end;

end.
