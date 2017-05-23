unit AdvGenericsTests;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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
  SysUtils, Generics.Collections,
  AdvObjects, AdvGenerics;

Type
  TAdvString = class (TAdvObject)
  private
    FString : String;
  public
    constructor create(value : String);
    function Link :  TAdvString; overload;
  end;

  TAdvGenericsTests = class (TAdvObject)
  private
    class procedure testSimple(obj : TAdvObject);
    class procedure testiterate;
    class procedure testRemove;
    class procedure testAddAll;
    class procedure testReplace;

    class procedure testMap;
  public
    class procedure execute;
  end;

implementation


{ TAdvGenericsTests }

class procedure TAdvGenericsTests.execute;
var
  x : TAdvObject;
begin
  x := TAdvObject.Create;
  // you should get one leak when you execute these tests. this exists to make sure that the leak tracking system is working

  testSimple(x);
  testRemove;
  testAddAll;
  testReplace;
  testIterate;
  testMap;
end;

class procedure TAdvGenericsTests.testSimple;
var
  l : TAdvList<TAdvObject>;
begin
  l := TAdvList<TAdvObject>.create;
  try
    l.Add(TAdvObject.Create);
  finally
    l.Free;
  end;
end;

class procedure TAdvGenericsTests.testAddAll;
var
  l : TList<TAdvObject>;
  l2 : TList<TAdvString>;
  o : TAdvString;
begin
  l := TList<TAdvObject>.create;
  l2 := TList<TAdvString>.create;
  try
    l.Add(TAdvObject.Create);
    l2.Add(TAdvString.create('test'));
    for o in l2 do
      l.add(o.Link);
  finally
    l.Free;
    l2.Free;
  end;
end;

class procedure TAdvGenericsTests.testRemove;
var
  l : TAdvList<TAdvObject>;
begin
  l := TAdvList<TAdvObject>.create;
  try
    l.Add(TAdvObject.Create);
    l.Delete(0);
    l.Add(TAdvObject.Create);
  finally
    l.Free;
  end;
end;

class procedure TAdvGenericsTests.testReplace;
var
  l : TAdvList<TAdvObject>;
begin
  l := TAdvList<TAdvObject>.create;
  try
    l.Add(TAdvObject.Create);
    l[0] := TAdvObject.Create;
  finally
    l.Free;
  end;
end;

class procedure TAdvGenericsTests.testIterate;
var
  l : TAdvList<TAdvObject>;
  c : integer;
  o : TAdvObject;
begin
  l := TAdvList<TAdvObject>.create;
  try
    l.Add(TAdvObject.Create);
    l.Add(TAdvObject.Create);
    l.Add(TAdvObject.Create);
    c := 0;
    for o in l do
      if (o = l[c]) then
        inc(c);
    if c <> 3 then
      raise Exception.Create('Wrong Count');
  finally
    l.Free;
  end;
end;

class procedure TAdvGenericsTests.testMap;
var
  map : TAdvMap<TAdvString>;
begin
  map := TAdvMap<TAdvString>.create;
  try
    map.Add('test1', TAdvString.create('test1'));
    map.Add('test2', TAdvString.create('test2'));
    map.AddOrSetValue('test2', TAdvString.create('test3'));
    if map['test1'].FString <> 'test1' then
      raise Exception.Create('Mismatch');
    if map['test2'].FString <> 'test3' then
      raise Exception.Create('Mismatch');
    map.Remove('1est1');
  finally
    map.Free;
  end;
end;

{ TAdvString }

constructor TAdvString.create(value: String);
begin
  inherited Create;
  FString := value;
end;

function TAdvString.Link: TAdvString;
begin
 result := TAdvString(inherited link);
end;

end.
