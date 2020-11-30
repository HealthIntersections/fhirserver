unit cda_tests;

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

{$I fhir.inc}

interface

uses
  SysUtils, Classes,
  IdTCPConnection,
  fsl_testing,
  fsl_stream, fsl_xml,
  fhir_objects,
  cda_objects, cda_parser, cda_writer;

const
  TEST_PORT = 20032; // err, we hope that this is unused

type
  TCdaTests = Class (TFslTestCase)
  private
    function parse(filename : String) : TcdaClinicalDocument;
    procedure compose(filename : String; doc : TcdaClinicalDocument);
  published
    Procedure TestParseDocument;
  end;

procedure RegisterTests;

implementation


{ Tv2Tests }

function TCdaTests.parse(filename: String): TCDAClinicalDocument;
var
   p : TCDAParser;
   x : TMXmlDocument;
begin
  x := TMXmlParser.parseFile(filename, [xpResolveNamespaces, xpDropComments, xpHTMLEntities]);
  try
    p := TCDAParser.Create;
    try
      result := p.Parse(x);
    finally
      p.Free;
    end;
  finally
    x.free;
  end;
end;

procedure TCdaTests.compose(filename: String; doc: TcdaClinicalDocument);
var
  w : TCDAWriter;
  x : TFslXmlBuilder;
begin
  x := TFslXmlBuilder.Create;
  try
    x.Start;
    w := TCDAWriter.Create;
    try
      w.WriteCDA(x, doc);
    finally
      w.Free;
    end;
    x.Finish;
    StringToFile(x.Build, filename, TEncoding.UTF8);
  finally
    x.Free;
  end;
end;

procedure TCdaTests.TestParseDocument;
var
  doc : TcdaClinicalDocument;
begin
  doc := parse(TestSettings.fhirTestFile(['cda', 'cda-original.xml']));
  try
    assertTrue(doc <> nil);
    assertTrue(doc.templateId[0].root = '2.16.840.1.113883.3.27.1776');
    compose(TestSettings.fhirTestFile(['cda', 'cda-original.out.xml']), doc);
  finally
    doc.free;
  end;
//  assertXmlMatches(
end;

procedure RegisterTests;
// don't use initialization - give other code time to set up directories etc
begin
  RegisterTest('Formats.CDA Tests', TCdaTests.Suite);
end;

end.
