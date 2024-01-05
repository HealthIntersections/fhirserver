unit fsl_gzip;

{$i fhir.inc}
interface

uses
  Classes, SysUtils, zflate,
  fsl_base, fsl_stream;

function gzcompress(bytes : TBytes; header : boolean; level: dword=9) : TBytes;
function gzuncompress(bytes : TBytes) : TBytes;

//
//function readZLibHeader(stream : TStream) : TBytes; overload;
//function readZLibHeader(b : TBytes) : TBytes; overload;

implementation

function readZLibHeader(b : TBytes) : TBytes;
var
  p : int64;
  i : integer;
begin
  if (length(b) < 10) or (b[0] <> $1F) or (b[1] <> $8B) then
    result := b
  else
  begin
    i := 10;
    if ((b[3] and $08) > 0) then
    begin
      repeat
        inc(i);
      until (i = length(b)) or (b[i] = 0);
      inc(i);
    end;
    if i >= length(b) then
      result := b
    else
      result := copy(b, i, length(b)-i-8);
  end;
end;

function gzcompress(bytes : TBytes; header : boolean; level: dword=9) : TBytes;
begin
  result := zflate.gzcompress(bytes, level);
end;

function gzuncompress(bytes : TBytes) : TBytes;
begin
  result := zflate.gzuncompress(readZLibHeader(bytes));
  if length(result) = 0 then
    raise EFslException.create('Failed to read compressed content: '+zflatetranslatecode(zlasterror));
  //BytesToFile(bytes, '/Users/grahamegrieve/temp/test.tgz');
  //gzdecode_file('/Users/grahamegrieve/temp/test.tgz', '/Users/grahamegrieve/temp/test.bin');
  //result := FileToBytes('/Users/grahamegrieve/temp/test.bin');
end;


//function InflateRfc1951(b : TBytes) : TBytes;
////var
////  b1, b2 : TBytesStream;
////  z : TZDecompressionStream;
//begin
//  result := gzuncompress(readZLibHeader(b));
//  //b1 := TBytesStream.create(b);// readZLibHeader(b));
//  //try
//  //  z := TZDecompressionStream.create(b1, true); // -15);
//  //  try
//  //    z.position := 0;
//  //    b2  := TBytesStream.Create;
//  //    try
//  //      b2.CopyFrom(z, 2);
//  //      result := b2.Bytes;
//  //      setLength(result, b2.size);
//  //    finally
//  //      b2.free;
//  //    end;
//  //  finally
//  //    z.free;
//  //  end;
//  //finally
//  //  b1.free;
//  //end;
//end;
//
//function DeflateRfc1951(b : TBytes) : TBytes;
//var
//  s : TBytesStream;
//  z : TZCompressionStream;
//begin
//  s := TBytesStream.create();
//  try
//    z := TZCompressionStream.create(clMax, s); // , -15);
//    try
//      z.Write(b, length(b));
//    finally
//      z.free;
//    end;
//    result := s.Bytes;
//    setLength(result, s.size);
//  finally
//    s.free;
//  end;
//end;
//
//

//
//
//function readZLibHeader(stream : TStream) : TBytes;
//begin
//  result := readZLibHeader(StreamToBytes(stream));
//
//end;

end.

