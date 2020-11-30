unit OIDCache;

{
Copyright (c) 2014+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

interface

uses
  SysUtils, Classes, IniFiles, Generics.Collections,
  fsl_base, fsl_utilities;

function DescribeOID(oid : String) : String;
procedure ReadOids(ts : TStringList);

implementation

var
  cache : TFslStringDictionary;

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
  cache := TFslStringDictionary.create;
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
