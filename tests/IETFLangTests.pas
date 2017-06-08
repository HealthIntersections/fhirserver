unit IETFLangTests;

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
  Windows, Sysutils, DUnitX.TestFramework,
  TextUtilities,
  IETFLanguageCodeServices;


type
  [TextFixture]
  TIETFLangTests = Class (TObject)
  private
    FDefinitions : TIETFLanguageDefinitions;
    procedure pass(code : String);
    procedure fail(code : String);
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [TestCase] Procedure TestSimple;
    [TestCase] Procedure TestWrong;
  end;

implementation

{ TIETFLangTests }

procedure TIETFLangTests.fail(code : String);
var
  msg : String;
  o : TIETFLanguageCodeConcept;
begin
  o := FDefinitions.parse(code, msg);
  try
    Assert.IsNull(o);
    Assert.IsNotEmpty(msg);
  finally
    o.Free;
  end;
end;

procedure TIETFLangTests.pass(code : String);
var
  msg : String;
  o : TIETFLanguageCodeConcept;
begin
  o := FDefinitions.parse(code, msg);
  try
    Assert.IsNotNull(o, msg);
    Assert.IsEmpty(msg);
  finally
    o.Free;
  end;
end;

procedure TIETFLangTests.Setup;
begin
  FDefinitions := TIETFLanguageDefinitions.create(FileToString('C:\work\fhirserver\sql\lang.txt', TEncoding.ASCII));
end;

procedure TIETFLangTests.TearDown;
begin
  FDefinitions.Free;
end;

procedure TIETFLangTests.TestSimple;
begin
  pass('en');
  pass('en-AU');
  pass('en-Latn-AU');
  pass('en-Brai-US');
end;

procedure TIETFLangTests.TestWrong;
begin
  fail('enAU');
  fail('en-AUA');
end;

initialization
  TDUnitX.RegisterTestFixture(TIETFLangTests);
end.
