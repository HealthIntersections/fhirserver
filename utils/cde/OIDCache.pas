unit OIDCache;

interface

uses
  SysUtils, Classes, IniFiles, Generics.Collections,
  FHIR.Support.Strings;

function DescribeOID(oid : String) : String;
procedure ReadOids(ts : TStringList);

implementation

var
  cache : TDictionary<String, String>;

procedure load;
var
  ts : TStringList;
  i : integer;
  l, r : String;
begin
  if cache.Count = 0 then
  begin
    ts := TStringList.Create;
    try
      ts.LoadFromFile('oids.csv');
      for i := 0 to ts.Count - 1 do
        begin
        StringSplit(ts[i], ',', l, r);
        cache.AddOrSetValue(l, r);
        end;
    finally
      ts.Free;
    end;
  end;
end;

function DescribeOID(oid : String) : String;
begin
  if not cache.TryGetValue(oid, result) then
    result := oid;
end;

procedure start;
begin
  cache := TDictionary<String, String>.create;
end;

procedure stop;
begin
  cache.free;
end;

procedure ReadOids(ts : TStringList);
var
  s : String;
begin
  if FileExists('oids.csv') then
    load;
  for s in cache.Keys do
    ts.Add(s +' : '+cache[s]);
end;

initialization
  start;
finalization
  stop;
end.
