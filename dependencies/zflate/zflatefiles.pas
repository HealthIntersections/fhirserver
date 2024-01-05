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

unit zflatefiles;

{$mode ObjFPC}{$H+}

interface

type
  //return true to continue or false to abort
  tzprogresscb = function(position, totalsize, outputsize: dword): boolean;

//compress a file to GZIP
function gzencode_file(src, dst: string; level: dword=9; filename: string=''; comment: string=''; progresscb: tzprogresscb=nil; resolution: dword=100): boolean;
//decompress a GZIP file
function gzdecode_file(src, dst: string; progresscb: tzprogresscb=nil; resolution: dword=100): boolean;

implementation

uses zflate;

// -- GZIP compress -----------------------

function gzencode_file(src, dst: string; level: dword=9; filename: string=''; comment: string=''; progresscb: tzprogresscb=nil; resolution: dword=100): boolean;
const
  bufsize = 1024*32;
var
  z: tzflate;
  inpt, outp: file of byte;
  buf: array[0..bufsize-1] of byte;
  header, footer: string;
  d, pos, fsize, outsize: dword;
  crc: dword = 0;
  failed: boolean = false;
  progress: dword = 0;
  progressnotified: dword = 0;
begin
  result := false;

  if not zdeflateinit(z, level) then exit;

  AssignFile(inpt, src);
  {$I-} Reset(inpt); {$I+}
  if IOResult <> 0 then exit;

  AssignFile(outp, dst);
  {$I-} Rewrite(outp); {$I+}
  if IOResult <> 0 then begin
    CloseFile(inpt);
    exit;
  end;

  fsize := FileSize(inpt);
  outsize := 0;
  pos := 0;

  try
    //write header
    header := makegzipheader(level, filename, comment);
    BlockWrite(outp, header[1], length(header));
    inc(outsize, length(header));

    while true do begin
      BlockRead(inpt, buf[0], bufsize, d);
      inc(pos, d);

      crc := crc32b(crc, @buf[0], d); //update crc32

      if not zdeflatewrite(z, @buf[0], d, d<bufsize) then begin
        failed := true;
        exit;
      end;

      BlockWrite(outp, z.buffer[0], z.bytesavailable);
      inc(outsize, z.bytesavailable);

      //progress callback
      if progresscb <> nil then begin
        progress := trunc(pos/fsize*resolution);

        if (progress > progressnotified) then begin
          if not progresscb(pos, fsize, outsize) then begin
            failed := true;
            zlasterror := ZFLATE_EABORTED;
            exit;
          end;

          progressnotified := progress;
        end;
      end;

      if d < bufsize then break; //eof
    end;

    //write footer
    footer := makegzipfooter(fsize, crc);
    BlockWrite(outp, footer[1], length(footer));
    inc(outsize, length(footer));

    result := true;
  finally
    CloseFile(inpt);
    CloseFile(outp);

    //delete output file on failure
    if failed then begin
      AssignFile(outp, dst);
      {$I-} Erase(outp); {$I+}
    end;
  end;
end;

// -- GZIP decompress ---------------------

function gzdecode_file(src, dst: string; progresscb: tzprogresscb=nil; resolution: dword=100): boolean;
const
  bufsize = 1024*32;
var
  z: tzflate;
  inpt, outp: file of byte;
  buf: array[0..bufsize-1] of byte;
  header, footer: string;
  d, bytestoread, pos, fsize, outsize: dword;
  crc: dword = 0;
  gzip: tgzipinfo;
  streamsize: dword;
  originalsize, checksum: dword;
  failed: boolean = false;
  progress: dword = 0;
  progressnotified: dword = 0;
begin
  result := false;

  if not zinflateinit(z) then exit;

  AssignFile(inpt, src);
  {$I-} Reset(inpt); {$I+}
  if IOResult <> 0 then exit;

  AssignFile(outp, dst);
  {$I-} Rewrite(outp); {$I+}
  if IOResult <> 0 then begin
    CloseFile(inpt);
    exit;
  end;

  fsize := FileSize(inpt);

  try
    //read header
    setlength(header, 512);
    BlockRead(inpt, header[1], length(header));
    if not zreadgzipheader(@header[1], gzip) then exit;

    //read footer
    Seek(inpt, fsize-8);
    setlength(footer, 8);
    BlockRead(inpt, footer[1], 8);
    checksum := pdword(@footer[1])^;
    originalsize := pdword(@footer[1+4])^;

    outsize := 0;
    pos := 0;
    streamsize := fsize-gzip.streamat-gzip.footerlen;

    Seek(inpt, gzip.streamat);

    while true do begin
      bytestoread := bufsize;
      if bytestoread+pos+gzip.streamat > streamsize then dec(bytestoread, gzip.footerlen); //skip footer

      BlockRead(inpt, buf[0], bytestoread, d);

      inc(pos, d);

      if not zinflatewrite(z, @buf[0], d, d<bufsize) then begin
        failed := true;
        exit;
      end;

      crc := crc32b(crc, @z.buffer[0], z.bytesavailable);

      BlockWrite(outp, z.buffer[0], z.bytesavailable);
      inc(outsize, z.bytesavailable);

      //progress callback
      if progresscb <> nil then begin
        progress := trunc((pos+gzip.streamat)/fsize*resolution);

        if (progress > progressnotified) then begin
          if not progresscb(pos+gzip.streamat, fsize, outsize) then begin
            failed := true;
            zlasterror := ZFLATE_EABORTED;
            exit;
          end;

          progressnotified := progress;
        end;
      end;

      if d < bufsize then break; //eof
    end;

    if FileSize(outp) <> originalsize then begin
      zlasterror := ZFLATE_EOUTPUTSIZE;
      failed := true;
      exit;
    end;

    if crc <> checksum then begin
      zlasterror := ZFLATE_ECHECKSUM;
      failed := true;
      exit;
    end;

    result := true;                                                                                                                                                                  
    CloseFile(inpt);
  finally
    CloseFile(outp);
                    
    //delete output file on failure
    if failed then begin
      AssignFile(outp, dst);
      {$I-} Erase(outp); {$I+}
    end;
  end;
end;

end.

