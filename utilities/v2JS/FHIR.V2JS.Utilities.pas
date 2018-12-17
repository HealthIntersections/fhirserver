unit FHIR.V2JS.Utilities;

interface

uses
  SysUtils, Classes,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream;

const
  FMT_V2 = 1;
  FMT_RESOURCE = 2;
  FMT_JS = 3;
  FMT_MAP = 4;
  FMT_TEMPLATE = 5;

function detectFormat(fn : String) : integer;

implementation

function detectFormat(fn : String) : integer;
var
  s : String;
begin
  s := FileToString(fn, TEncoding.UTF8);
  if s.StartsWith('MSH|') then
    exit(FMT_V2);

  result := 0;
end;

end.
