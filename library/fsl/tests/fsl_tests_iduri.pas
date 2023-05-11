unit fsl_tests_iduri;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$i fhir.inc}

interface

uses
  Sysutils, Classes,
  fsl_testing,
  IdUri;


type
  TIdUriParserTests = Class (TFslTestCase)
  private
    procedure ok(uri : String);
  published
    Procedure TestOK;
    Procedure TestFail;
    Procedure TestUnicode1;
    Procedure TestUnicode2;
  end;

procedure registerTests;

implementation

{ TIdUriParserTests }

procedure TIdUriParserTests.ok(uri: String);
var
  o : TIdUri;
begin
  o := TIdUri.create(uri);
  try
    assertTrue(o <> nil);
  finally
    o.free;
  end;
end;

procedure TIdUriParserTests.TestFail;
begin
  ok('http://foo@127.0.0.1 @google.com/');
end;

procedure TIdUriParserTests.TestOK;
begin
  ok('http://test.fhir.org/r3');
end;

procedure TIdUriParserTests.TestUnicode1;
begin
  ok('http://orange.tw/sandbox/o<.o<./passwd');
end;

procedure TIdUriParserTests.TestUnicode2;
begin
  ok('http://orange.tw/sandbox/%EF%BC%AE%EF%BC%AE/passwd');
end;

procedure registerTests;
// don't use initialization - give other code time to set up directories etc
begin
  RegisterTest('IdUri Tests', TIdUriParserTests.Suite);
end;

end.
