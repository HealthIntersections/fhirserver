unit FHIR.Server.TestRegistry;

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

(*

FHIR.Ucum.Tests
FHIR.Tests.Snomed
FHIR.Cql.Tests

FHIR.Javascript.Tests
FHIR.R4.Tests.Client
FHIR.R4.Tests.Liquid
FHIR.R4.Tests.Maps
FHIR.R4.Tests.Parser
FHIR.R4.Tests.PathEngine
FHIR.R4.Tests.Utilities
FHIR.R4.Tests.Validator
FHIR.R4.Tests.Worker
FHIR.Tools.GraphQL.Tests
FHIR.Tests.FullServer
FHIR.Tests.GraphDefinition
FHIR.Tests.RestFulServer
FHIR.Tests.SearchSyntax
FHIR.Tests.SmartLogin
FHIR.XVersion.Tests
FHIR.v2.Tests
FHIR.R4.Tests.Context
*)

uses
  CommonTestBase, FHIR.Support.Testing,
  MarkdownDaringFireballTests, MarkdownCommonMarkTests,
  FHIR.Support.Tests, FHIR.Tx.IETFLang.Tests, FHIR.Tests.IdUriParser, FHIR.Database.Tests;

procedure registerTests;

implementation

procedure registerTests;
begin
  // before we register the tests, we have to set up the locations of 3 folders:
  // * the markdown github repo location (local root)
  // * the official tests github repo (local root)
  // * the github repo for the server (local root)
  // the tests don't clone these repos - this must be done first

  MDTestRoot := 'c:\work\markdown';
  ServerTestsRoot := 'c:\work\fhirserver';
  FHIRTestsRoot := 'c:\work\org.hl7.fhir\fhir-test-cases';

  MarkdownDaringFireballTests.registerTests;
  MarkdownCommonMarkTests.registerTests;
  FHIR.Support.Tests.registerTests;
  FHIR.Tx.IETFLang.Tests.registerTests;
  FHIR.Tests.IdUriParser.registerTests;
  FHIR.Database.Tests.registerTests;
end;

end.
