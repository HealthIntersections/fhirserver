{ MIT License

  Copyright (c) 2023 fibodevy https://github.com/fibodevy

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

unit zflate;

{$mode ObjFPC}{$H+}

//comment out to disable error translation
//if disabled, zflatetranslatecode will return error code as string
{$define zflate_error_translation}

interface

uses
  ZBase, ZInflate, ZDeflate;

type
  tzflate = record
    z: z_stream;
    totalout: dword;
    bytesavailable: dword;
    buffer: array of byte;
    error: integer;
  end;

  tzlibinfo = record
    streamat: dword;
    footerlen: dword;
  end;

  tgzipinfo = record
    modtime: dword;
    filename: pchar;
    comment: pchar;
    streamat: dword;  
    footerlen: dword;
  end;

  TBytes = array of byte;

const
  ZFLATE_ZLIB = 1;
  ZFLATE_GZIP = 2;

  ZFLATE_OK           = 0;
  ZFLATE_ECHUNKTOOBIG = 101; //'chunk is too big'
  ZFLATE_EBUFFER      = 102; //'buffer too small'
  ZFLATE_ESTREAM      = 103; //'stream error'
  ZFLATE_EDATA        = 104; //'data error'
  ZFLATE_EDEFLATE     = 105; //'deflate error'
  ZFLATE_EINFLATE     = 106; //'inflate error'
  ZFLATE_EDEFLATEINIT = 107; //'deflate init failed'
  ZFLATE_EINFLATEINIT = 108; //'inflate init failed'
  ZFLATE_EZLIBINVALID = 109; //'invalid zlib header'
  ZFLATE_EGZIPINVALID = 110; //'invalid gzip header'
  ZFLATE_ECHECKSUM    = 111; //'invalid checksum'
  ZFLATE_EOUTPUTSIZE  = 112; //'output size doesnt match original file size'
  ZFLATE_EABORTED     = 113; //'aborted'

var
  zchunkmaxsize: dword = 1024*128; //128 KB default max chunk size
  zbuffersize: dword = 1024*1024*16; //16 MB default buffer size

threadvar
  zlasterror: integer;

//initialize zdeflate
function zdeflateinit(var z: tzflate; level: dword=9; buffersize: dword=0): boolean;
//deflate chunk
function zdeflatewrite(var z: tzflate; data: pointer; size: dword; lastchunk: boolean=false): boolean;

//initialize zinflate
function zinflateinit(var z: tzflate; buffersize: dword=0): boolean;
//inflate chunk
function zinflatewrite(var z: tzflate; data: pointer; size: dword; lastchunk: boolean=false): boolean;

//read zlib header
function zreadzlibheader(data: pointer; var info: tzlibinfo): boolean;
//read gzip header
function zreadgzipheader(data: pointer; var info: tgzipinfo): boolean;
//get stream basic info; by reading just few first bytes you will know the stream type, where is deflate start and how many bytes are trailing bytes (footer)
function zstreambasicinfo(data: pointer; var streamtype: dword; var startsat: dword; var trailing: dword): boolean;
//find out stream type, where deflate stream starts and what is its size
function zfindstream(data: pointer; size: dword; var streamtype: dword; var startsat: dword; var streamsize: dword): boolean;

//compress whole buffer to DEFLATE at once
function gzdeflate(data: pointer; size: dword; var output: pointer; var outputsize: dword; level: dword=9): boolean;
//compress whole string to DEFLATE at once
function gzdeflate(str: string; level: dword=9): string;
//compress whole bytes to DEFLATE at once
function gzdeflate(bytes : TBytes; level: dword=9): TBytes;
//decompress whole DEFLATE buffer at once
function gzinflate(data: pointer; size: dword; var output: pointer; var outputsize: dword): boolean;
//decompress whole DEFLATE string at once
function gzinflate(str: string): string;
//decompress whole DEFLATE bytes at once
function gzinflate(bytes : TBytes): TBytes;

//make ZLIB header
function makezlibheader(compressionlevel: integer): string;
//make ZLIB footer
function makezlibfooter(adler: dword): string;
//compress whole buffer to ZLIB at once
function gzcompress(data: pointer; size: dword; var output: pointer; var outputsize: dword; level: dword=9): boolean;
//compress whole string to ZLIB at once
function gzcompress(str: string; level: dword=9): string;
//compress whole buffer to ZLIB at once
function gzcompress(bytes : TBytes; level: dword=9) : TBytes;
//dempress whole ZLIB buffer at once    !
function gzuncompress(data: pointer; size: dword; var output: pointer; var outputsize: dword): boolean;
//dempress whole ZLIB string at once
function gzuncompress(str: string): string;
//dempress whole ZLIB buffer at once
function gzuncompress(bytes : TBytes) : TBytes;

//make GZIP header
function makegzipheader(compressionlevel: integer; filename: string=''; comment: string=''): string;
//make GZIP footer
function makegzipfooter(originalsize: dword; crc: dword): string;
//compress whole buffer to GZIP at once
function gzencode(data: pointer; size: dword; var output: pointer; var outputsize: dword; level: dword=9; filename: string=''; comment: string=''): boolean;
//compress whole string to GZIP at once
function gzencode(str: string; level: dword=9; filename: string=''; comment: string=''): string;
//compress whole string to GZIP at once
function gzencode(bytes: TBytes; level: dword=9; filename: string=''; comment: string=''): TBytes;
//decompress whole GZIP buffer at once
function gzdecode(data: pointer; size: dword; var output: pointer; var outputsize: dword): boolean;
//decompress whole GZIP string at once
function gzdecode(str: string): string;
//decompress whole GZIP string at once
function gzdecode(bytes: TBytes): TBytes;

//try to detect buffer format and decompress it at once
function zdecompress(data: pointer; size: dword; var output: pointer; var outputsize: dword): boolean;
//try to detect string format and decompress it at once
function zdecompress(str: string): string;
//try to detect bytes format and decompress it at once
function zdecompress(bytes: TBytes): TBytes;

//transalte error code to message
function zflatetranslatecode(code: integer): string;

//compute crc32b checksum
function crc32b(crc: dword; buf: pbyte; len: dword): dword;
//compute adler32 checksum
function adler32(adler: dword; buf: pbyte; len: dword): dword;

implementation

function zerror(var z: tzflate; error: integer): boolean;
begin
  z.error := error;
  zlasterror := error;
  result := false;
end;

// -- deflate chunks ----------------------

function zdeflateinit(var z: tzflate; level: dword=9; buffersize: dword=0): boolean;
begin
  result := false;         
  zlasterror := 0;
  if buffersize = 0 then buffersize := zbuffersize;
  fillchar(z, sizeof(z), 0);
  setlength(z.buffer, buffersize);
  if deflateInit2(z.z, level, Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL, 0) <> Z_OK then exit;
  result := true;
end;

function zdeflatewrite(var z: tzflate; data: pointer; size: dword; lastchunk: boolean=false): boolean;
var
  i: integer;
begin
  result := false;

  if size > zchunkmaxsize then exit(zerror(z, ZFLATE_ECHUNKTOOBIG));

  z.z.next_in := data;
  z.z.avail_in := size;
  z.z.next_out := @z.buffer[0];
  z.z.avail_out := length(z.buffer);

  if lastchunk then
    i := deflate(z.z, Z_FINISH)
  else
    i := deflate(z.z, Z_NO_FLUSH);

  if i = Z_BUF_ERROR then exit(zerror(z, ZFLATE_EBUFFER)); //buffer too small
  if i = Z_STREAM_ERROR then exit(zerror(z, ZFLATE_ESTREAM));
  if i = Z_DATA_ERROR then exit(zerror(z, ZFLATE_EDATA));

  if (i = Z_OK) or (i = Z_STREAM_END) then begin
    z.bytesavailable := z.z.total_out-z.totalout;
    z.totalout += z.bytesavailable;
    result := true;
  end else
    exit(zerror(z, ZFLATE_EDEFLATE));

  if lastchunk then begin
    i := deflateEnd(z.z);
    result := i = Z_OK;
  end;
end;

// -- inflate chunks ----------------------

function zinflateinit(var z: tzflate; buffersize: dword=0): boolean;
begin
  result := false;      
  zlasterror := 0;
  if buffersize = 0 then buffersize := zbuffersize;
  fillchar(z, sizeof(z), 0);
  setlength(z.buffer, buffersize);
  if inflateInit2(z.z, -MAX_WBITS) <> Z_OK then exit;
  result := true;
end;

function zinflatewrite(var z: tzflate; data: pointer; size: dword; lastchunk: boolean=false): boolean;
var
  i: integer;
begin
  result := false;

  if size > zchunkmaxsize then exit(zerror(z, ZFLATE_ECHUNKTOOBIG));

  z.z.next_in := data;
  z.z.avail_in := size;
  z.z.next_out := @z.buffer[0];
  z.z.avail_out := length(z.buffer);

  if lastchunk then
    i := inflate(z.z, Z_FINISH)
  else
    i := inflate(z.z, Z_NO_FLUSH);

  if i = Z_BUF_ERROR then exit(zerror(z, ZFLATE_EBUFFER)); //buffer too small
  if i = Z_STREAM_ERROR then exit(zerror(z, ZFLATE_ESTREAM));
  if i = Z_DATA_ERROR then exit(zerror(z, ZFLATE_EDATA));

  if (i = Z_OK) or (i = Z_STREAM_END) then begin
    z.bytesavailable := z.z.total_out-z.totalout;
    z.totalout += z.bytesavailable;
    result := true;
  end else
    exit(zerror(z, ZFLATE_EINFLATE));

  if lastchunk then begin
    i := inflateEnd(z.z);
    result := i = Z_OK;
  end;
end;

function zreadzlibheader(data: pointer; var info: tzlibinfo): boolean;
begin
  info.footerlen := 0;
  info.streamat := 0;

  result := false;
  try
    fillchar(info, sizeof(info), 0);
    result := (pbyte(data)^ = $78) and (pbyte(data+1)^ in [$01, $5e, $9c, $da]);
    if not result then exit;
    info.footerlen := 4;
    info.streamat := 2;
  except
  end;
end;

function zreadgzipheader(data: pointer; var info: tgzipinfo): boolean;
var
  flags: byte;
  w: word;
begin
  result := false;
  try
    fillchar(info, sizeof(info), 0);
    if not ((pbyte(data)^ = $1f) and (pbyte(data+1)^ = $8b)) then exit;

    info.footerlen := 8;

    //mod time
    move((data+4)^, info.modtime, 4);

    //stream position
    info.streamat := 10;

    //flags
    flags := pbyte(data+3)^;

    //extra
    if (flags and $04) <> 0 then begin
      w := pword(data+info.streamat)^;
      info.streamat += 2+w;
    end;

    //filename
    if (flags and $08) <> 0 then begin
      info.filename := pchar(data+info.streamat);
      info.streamat += length(info.filename)+1;
    end;

    //comment
    if (flags and $10) <> 0 then begin
      info.comment := pchar(data+info.streamat);
      info.streamat += length(info.comment)+1;
    end;

    //crc16?
    if (flags and $02) <> 0 then begin
      info.streamat += 2;
    end;

    result := true;
  except
  end;
end;

function zstreambasicinfo(data: pointer; var streamtype: dword; var startsat: dword; var trailing: dword): boolean;
var
  zlib: tzlibinfo;
  gzip: tgzipinfo;
begin
  result := false;
  streamtype := 0;

  if zreadzlibheader(data, zlib) then begin
    streamtype := ZFLATE_ZLIB;
    startsat := zlib.streamat;
    trailing := 4; //footer: adler32
    exit(true);
  end;

  if zreadgzipheader(data, gzip) then begin
    streamtype := ZFLATE_GZIP;
    startsat := gzip.streamat;
    trailing := 8; //footer: crc32 + original file size
    exit(true);
  end;
end;

function zfindstream(data: pointer; size: dword; var streamtype: dword; var startsat: dword; var streamsize: dword): boolean;
var
  trailing: dword;
begin
  result := false;

  if size < 6 then exit; //6 bytes is minimum for ZLIB, 18 for GZIP

  if zstreambasicinfo(data, streamtype, startsat, trailing) then begin
    streamsize := size-startsat-trailing;
    result := true;
  end;
end;

// -- deflate -----------------------------

function gzdeflate(data: pointer; size: dword; var output: pointer; var outputsize: dword; level: dword=9): boolean;
var
  z: tzflate;
  p, chunksize: dword;
begin
  result := false;
  if not zdeflateinit(z, level) then exit(zerror(z, ZFLATE_EDEFLATEINIT));

  output := nil;
  outputsize := 0;
  p := 0;

  //compress
  while size > 0 do begin
    chunksize := size;
    if chunksize > zchunkmaxsize then chunksize := zchunkmaxsize;
    //deflate
    if not zdeflatewrite(z, data, chunksize, chunksize<zchunkmaxsize) then exit; //might be ZFLATE_EBUFFER = buffer too small
    //alloc mem for output
    if output = nil then output := getmem(z.bytesavailable)
    else output := reallocmem(output, outputsize+z.bytesavailable);
    //move buffer to output
    move(z.buffer[0], (output+p)^, z.bytesavailable);
    //increase output position
    inc(p, z.bytesavailable);
    //increase output size
    inc(outputsize, z.bytesavailable);
    //increase data pointer
    inc(data, chunksize);
    //how much data left
    dec(size, chunksize);
  end;

  result := true;
end;

function gzdeflate(str: string; level: dword=9): string;
var
  p: pointer;
  d: dword;
begin
  result := '';
  if not gzdeflate(@str[1], length(str), p, d, level) then exit;
  setlength(result, d);
  move(p^, result[1], d);
  freemem(p);
end;

function gzdeflate(bytes: TBytes; level: dword=9): TBytes;
var
  p: pointer;
  d: dword;
begin
  result := nil;
  if not gzdeflate(@bytes[0], length(bytes), p, d, level) then exit;
  setlength(result, d);
  move(p^, result[0], d);
  freemem(p);
end;

// -- inflate -----------------------------

function gzinflate(data: pointer; size: dword; var output: pointer; var outputsize: dword): boolean;
var
  z: tzflate;
  p, chunksize: dword;
begin
  result := false;
  if not zinflateinit(z) then exit(zerror(z, ZFLATE_EINFLATEINIT));

  output := nil;
  outputsize := 0;
  p := 0;

  //decompress
  while size > 0 do begin
    chunksize := size;
    if chunksize > zchunkmaxsize then chunksize := zchunkmaxsize;
    //inflate
    if not zinflatewrite(z, data, chunksize, chunksize<zchunkmaxsize) then exit; //might be ZFLATE_EBUFFER = buffer too small
    //alloc mem for output
    if output = nil then output := getmem(z.bytesavailable)
    else output := reallocmem(output, outputsize+z.bytesavailable);
    //move buffer to output
    move(z.buffer[0], (output+p)^, z.bytesavailable);
    //increase output position
    inc(p, z.bytesavailable);
    //increase output size
    inc(outputsize, z.bytesavailable);
    //increase data pointer
    inc(data, chunksize);
    //how much data left
    dec(size, chunksize);
  end;

  result := true;
end;

function gzinflate(str: string): string;
var
  p: pointer;
  d: dword;
begin
  result := '';
  if not gzinflate(@str[1], length(str), p, d) then exit;
  setlength(result, d);
  move(p^, result[1], d);
  freemem(p);
end;

function gzinflate(bytes: TBytes): TBytes;
var
  p: pointer;
  d: dword;
begin
  result := nil;
  if not gzinflate(@bytes[0], length(bytes), p, d) then exit;
  setlength(result, d);
  move(p^, result[0], d);
  freemem(p);
end;

// -- ZLIB compress -----------------------

function makezlibheader(compressionlevel: integer): string;
begin
  result := #$78;

  case compressionlevel of
    1: result += #$01;
    2: result += #$5e;
    3: result += #$5e;
    4: result += #$5e;
    5: result += #$5e;
    6: result += #$9c;
    7: result += #$da;
    8: result += #$da;
    9: result += #$da;
  else
    result += #$da;
  end;
end;

function makezlibfooter(adler: dword): string;
begin
  setlength(result, 4);
  adler := swapendian(adler);
  move(adler, result[1], 4);
end;

function gzcompress(data: pointer; size: dword; var output: pointer; var outputsize: dword; level: dword=9): boolean;
var
  header, footer: string;  
  deflated: pointer;
  deflatedsize: dword;
begin
  result := false;

  header := makezlibheader(level);
  footer := makezlibfooter(adler32(adler32(0, nil, 0), data, size));

  if not gzdeflate(data, size, deflated, deflatedsize, level) then exit;

  outputsize := length(header)+deflatedsize+length(footer);
  output := getmem(outputsize);

  move(header[1], output^, length(header));
  move(deflated^, (output+length(header))^, deflatedsize);
  move(footer[1], (output+length(header)+deflatedsize)^, length(footer));

  freemem(deflated);

  result := true;
end;

function gzcompress(str: string; level: dword=9): string;
var
  p: pointer;
  d: dword;
begin
  result := '';
  if not gzcompress(@str[1], length(str), p, d, level) then exit;
  setlength(result, d);
  move(p^, result[1], d);
  freemem(p);
end;

function gzcompress(bytes : TBytes; level: dword=9) : TBytes;  
var
  p: pointer;
  d: dword;
begin
  result := nil;
  if not gzcompress(@bytes[0], length(bytes), p, d, level) then exit;
  try
    setlength(result, d);
    move(p^, result[0], d);
  finally
    freemem(p);
  end;
end;

// -- ZLIB decompress ---------------------

function gzuncompress(data: pointer; size: dword; var output: pointer; var outputsize: dword): boolean;
var
  zlib: tzlibinfo;
  z: tzflate;
  checksum: dword;
begin
  result := false;
  if not zreadzlibheader(data, zlib) then exit(zerror(z, ZFLATE_EZLIBINVALID));

  checksum := swapendian(pdword(data+size-4)^);

  data += zlib.streamat;
  size -= zlib.streamat+zlib.footerlen;
  if not gzinflate(data, size, output, outputsize) then exit;

  if (adler32(adler32(0, nil, 0), output, outputsize) <> checksum) then exit(zerror(z, ZFLATE_ECHECKSUM));

  result := true;
end;

function gzuncompress(str: string): string;
var
  p: pointer;
  d: dword;
begin
  result := '';
  if not gzuncompress(@str[1], length(str), p, d) then exit;
  setlength(result, d);
  move(p^, result[1], d);
  freemem(p);
end;

function gzuncompress(bytes : TBytes) : TBytes;
var
  p: pointer;
  d: dword;
begin
  result := nil;
  if not gzuncompress(@bytes[0], length(bytes), p, d) then exit;
  try
    setlength(result, d);
    move(p^, result[0], d);
  finally
    freemem(p);
  end;
end;


// -- GZIP compress -----------------------

function makegzipheader(compressionlevel: integer; filename: string=''; comment: string=''): string;
var
  flags: byte;
  modtime: dword;
begin
  setlength(result, 10);
  result[1] := #$1f; //signature
  result[2] := #$8b; //signature
  result[3] := #$08; //deflate algo

  //modification time
  modtime := 0;
  move(modtime, result[5], 4);

  result[9] := #$00; //compression level
  if compressionlevel = 9 then result[9] := #$02; //best compression
  if compressionlevel = 1 then result[9] := #$04; //best speed

  result[10] := #$FF; //file system (00 = FAT?)
  //result[10] := #$00;

  //optional headers
  flags := 0;

  //filename
  if filename <> '' then begin
    flags := flags or $08;
    result += filename;
    result += #$00;
  end;

  //comment
  if comment <> '' then begin
    flags := flags or $10;
    result += comment;
    result += #00;
  end;

  result[4] := chr(flags);
end;

function makegzipfooter(originalsize: dword; crc: dword): string;
begin
  setlength(result, 8);
  move(crc, result[1], 4);
  move(originalsize, result[1+4], 4);
end;

function gzencode(data: pointer; size: dword; var output: pointer; var outputsize: dword; level: dword=9; filename: string=''; comment: string=''): boolean;
var
  header, footer: string;
  deflated: pointer;
  deflatedsize: dword;
begin
  result := false;

  header := makegzipheader(level, filename, comment);
  footer := makegzipfooter(size, crc32b(0, data, size));

  if not gzdeflate(data, size, deflated, deflatedsize, level) then exit;

  outputsize := length(header)+deflatedsize+length(footer);
  output := getmem(outputsize);

  move(header[1], output^, length(header));
  move(deflated^, (output+length(header))^, deflatedsize);
  move(footer[1], (output+length(header)+deflatedsize)^, length(footer));

  freemem(deflated);

  result := true;
end;

function gzencode(str: string; level: dword=9; filename: string=''; comment: string=''): string;
var
  p: pointer;
  d: dword;
begin
  result := '';
  if not gzencode(@str[1], length(str), p, d, level, filename, comment) then exit;
  setlength(result, d);
  move(p^, result[1], d);
  freemem(p);
end;

function gzencode(bytes: TBytes; level: dword=9; filename: string=''; comment: string=''): TBytes;
var
  p: pointer;
  d: dword;
begin
  result := nil;
  if not gzencode(@bytes[0], length(bytes), p, d, level, filename, comment) then exit;
  setlength(result, d);
  move(p^, result[0], d);
  freemem(p);
end;

// -- GZIP decompress ---------------------

function gzdecode(data: pointer; size: dword; var output: pointer; var outputsize: dword): boolean;
var
  gzip: tgzipinfo;
  z: tzflate;
  originalsize, checksum: dword;
begin
  result := false;
  if not zreadgzipheader(data, gzip) then exit(zerror(z, ZFLATE_EGZIPINVALID));

  originalsize := pdword(data+size-4)^;
  checksum := pdword(data+size-8)^;

  data += gzip.streamat;
  size -= gzip.streamat+gzip.footerlen;
  if not gzinflate(data, size, output, outputsize) then exit;

  if originalsize <> outputsize then exit(zerror(z, ZFLATE_EOUTPUTSIZE));
  if crc32b(0, output, outputsize) <> checksum then exit(zerror(z, ZFLATE_ECHECKSUM));

  result := true;
end;

function gzdecode(str: string): string;
var
  p: pointer;
  d: dword;
begin
  result := '';
  if not gzdecode(@str[1], length(str), p, d) then exit;
  setlength(result, d);
  move(p^, result[1], d);
  freemem(p);
end;

function gzdecode(bytes: TBytes): TBytes;
var
  p: pointer;
  d: dword;
begin
  result := nil;
  if not gzdecode(@bytes[0], length(bytes), p, d) then exit;
  setlength(result, d);
  move(p^, result[0], d);
  freemem(p);
end;

// -- decompress anything -----------------

function zdecompress(data: pointer; size: dword; var output: pointer; var outputsize: dword): boolean;
var
  streamsize, startsat, streamtype: dword;
begin
  result := false;

  if not zfindstream(data, size, streamtype, startsat, streamsize) then begin
    //stream not found, assume its pure deflate
    startsat := 0;    
    streamsize := size;
  end;

  if not gzinflate(data+startsat, streamsize, output, outputsize) then exit;

  result := true;
end;

function zdecompress(str: string): string;
var
  p: pointer;
  d: dword;
begin
  result := '';
  if not zdecompress(@str[1], length(str), p, d) then exit;
  setlength(result, d);
  move(p^, result[1], d);
  freemem(p);
end;

function zdecompress(bytes: TBytes): TBytes;
var
  p: pointer;
  d: dword;
begin
  result := nil;
  if not zdecompress(@bytes[0], length(bytes), p, d) then exit;
  setlength(result, d);
  move(p^, result[0], d);
  freemem(p);
end;

// -- error translation -------------------

function zflatetranslatecode(code: integer): string;
begin
  {$ifdef zflate_error_translation}
  result := 'unknown';

  case code of
    ZFLATE_ZLIB        : result := 'ZLIB';
    ZFLATE_GZIP        : result := 'GZIP';
    ZFLATE_OK          : result := 'ok';
    ZFLATE_ECHUNKTOOBIG: result := 'chunk is too big';
    ZFLATE_EBUFFER     : result := 'buffer too small';
    ZFLATE_ESTREAM     : result := 'stream error';
    ZFLATE_EDATA       : result := 'data error';
    ZFLATE_EDEFLATE    : result := 'deflate error';
    ZFLATE_EINFLATE    : result := 'inflate error';
    ZFLATE_EDEFLATEINIT: result := 'deflate init failed';
    ZFLATE_EINFLATEINIT: result := 'inflate init failed';
    ZFLATE_EZLIBINVALID: result := 'invalid zlib header';
    ZFLATE_EGZIPINVALID: result := 'invalid gzip header';
    ZFLATE_ECHECKSUM   : result := 'invalid checksum';
    ZFLATE_EOUTPUTSIZE : result := 'output size doesnt match original file size';
    ZFLATE_EABORTED    : result := 'aborted';
  end;
  {$else}
  system.Str(code, result);
  {$endif}
end;

// -- crc32b ------------------------------

var
  crc32_table: array[byte] of dword;   
  crc32_table_empty: boolean = true;

function crc32b(crc: dword; buf: pbyte; len: dword): dword;
procedure make_crc32_table;
var
  d: dword;
  n, k: integer;
begin
  for n := 0 to 255 do begin
    d := cardinal(n);
    for k := 0 to 7 do begin
      if (d and 1) <> 0 then
        d := (d shr 1) xor uint32($edb88320)
      else
        d := (d shr 1);
    end;
    crc32_table[n] := d;
  end;
  crc32_table_empty := false;
end;
begin
  if buf = nil then exit(0);
  if crc32_table_empty then make_crc32_table;

  crc := crc xor $ffffffff;
  while (len >= 4) do begin
    crc := crc32_table[(crc xor buf[0]) and $ff] xor (crc shr 8);
    crc := crc32_table[(crc xor buf[1]) and $ff] xor (crc shr 8);
    crc := crc32_table[(crc xor buf[2]) and $ff] xor (crc shr 8);
    crc := crc32_table[(crc xor buf[3]) and $ff] xor (crc shr 8);
    inc(buf, 4);
    dec(len, 4);
  end;

  while (len > 0) do begin
    crc := crc32_table[(crc xor buf^) and $ff] xor (crc shr 8);
    inc(buf);
    dec(len);
  end;

  result := crc xor $ffffffff;
end;

// -- adler32 -----------------------------

function adler32(adler: dword; buf: pbyte; len: dword): dword;
const
  base = dword(65521);
  nmax = 3854;
var
  d1, d2: dword;
  k: integer;
begin
  if buf = nil then exit(1);

  d1 := adler and $ffff;
  d2 := (adler shr 16) and $ffff;

  while (len > 0) do begin
    if len < nmax then
      k := len
    else
      k := nmax;
    dec(len, k);
    while (k > 0) do begin
      inc(d1, buf^);
      inc(d2, d1);
      inc(buf);
      dec(k);
    end;
    d1 := d1 mod base;
    d2 := d2 mod base;
  end;
  result := (d2 shl 16) or d1;
end;

end.

