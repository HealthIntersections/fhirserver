Unit FHIRServerUtilities;

interface

uses
  SysUtils, Classes,
  StringSupport,
  FHIRBase, FHIRResources, FHIRTypes, FHIRLang, FHIRUtilities;

function LoadBinaryResource(lang : String; b: TBytes): TFhirBinary;

implementation

function LoadBinaryResource(lang : String; b: TBytes): TFhirBinary;
var
  s : TBytes;
  i, j : integer;
  ct : AnsiString;
begin
  result := TFhirBinary.create;
  try
    s := ZDecompressBytes(b);
    move(s[0], i, 4);
    setLength(ct, i);
    move(s[4], ct[1], i);
    move(s[4+i], j, 4);

    result.Content := copy(s, 8+i, j);

    result.ContentType := String(ct);
    result.Link;
  finally
    result.free;
  end;
end;


end.

