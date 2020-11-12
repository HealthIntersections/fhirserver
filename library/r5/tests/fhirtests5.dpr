program fhirtests4;

{
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

{$APPTYPE CONSOLE}

uses
  FastMM4 in '..\..\dependencies\FMM\FastMM4.pas',
  FastMM4Messages in '..\..\dependencies\FMM\FastMM4Messages.pas',
  ActiveX,
  IdSSLOpenSSLHeaders,
  SysUtils,
  TestInsight.DUnitX,
  fsl_base in '..\Support\fsl_base.pas',
  fsl_stream in '..\support\fsl_stream.pas',
  fsl_wininet in '..\support\fsl_wininet.pas',
  fsl_utilities in '..\support\fsl_utilities.pas',
  fsl_xml in '..\support\fsl_xml.pas',
  fsl_msxml in '..\support\fsl_msxml.pas',
  fsl_http in '..\support\fsl_http.pas',
  fsl_rdf in '..\support\fsl_rdf.pas',
  fsl_shell in '..\support\fsl_shell.pas',
  fsl_fetcher in '..\support\fsl_fetcher.pas',
  fsl_turtle in '..\support\fsl_turtle.pas',
  fsl_logging in '..\support\fsl_logging.pas',
  FHIR.Support.Signatures in '..\support\FHIR.Support.Signatures.pas',
  fsl_collections in '..\support\fsl_collections.pas',
  fsl_threads in '..\support\fsl_threads.pas',
  fsl_xml in '..\support\fsl_xml.pas',
  fsl_json in '..\support\fsl_json.pas',
  FHIR.Support.Certs in '..\support\FHIR.Support.Certs.pas',
  fsl_graphql in '..\support\fsl_graphql.pas',
  fhir_ucum in '..\support\fhir_ucum.pas',
  fhir_objects in '..\base\fhir_objects.pas',
  FHIR.Base.Lang in '..\base\FHIR.Base.Lang.pas',
  fhir_parser in '..\base\fhir_parser.pas',
  fsl_scim in '..\base\fsl_scim.pas',
  fhir_xhtml in '..\base\fhir_xhtml.pas',
  FHIR.Version.Parser in '..\version\FHIR.Version.Parser.pas',
  FHIR.Tools.Indexing in '..\tools\FHIR.Tools.Indexing.pas',
  FHIR.Tools.DiffEngine in '..\tools\FHIR.Tools.DiffEngine.pas',
  fhir4_constants,
  fhir4_pathengine in 'fhir4_pathengine.pas',
  fhir4_resources in 'fhir4_resources.pas',
  fhir4_utilities in 'fhir4_utilities.pas',
  fhir4_xml in 'fhir4_xml.pas',
  fhir4_json in 'fhir4_json.pas',
  fhir4_turtle in 'fhir4_turtle.pas',
  FHIR.Tests.Decimal in '..\support\Tests\FHIR.Tests.Decimal.pas',
  DifferenceEngineTests in '..\support\Tests\DifferenceEngineTests.pas',
  fhir4_tests_PathEngine in 'tests\fhir4_tests_PathEngine.pas',
  fhir4_tests_worker in 'tests\fhir4_tests_worker.pas',
  JWTTests in '..\support\Tests\JWTTests.pas',
  JsonTests in '..\support\Tests\javascriptonTests.pas',
  XmlTests in '..\support\Tests\XmlTests.pas',
  fhir_common in '..\base\fhir_common.pas',
  fhir_factory in '..\base\fhir_factory.pas',
  fhir_narrative in '..\base\fhir_narrative.pas',
  fhir_validator in '..\base\fhir_validator.pas',
  fhir_pathengine in '..\base\fhir_pathengine.pas',
  fhir_client in '..\client\fhir_client.pas',
  fhir_client_threaded in '..\client\fhir_client_threaded.pas',
  fhir_client_http in '..\client\fhir_client_http.pas',
  fsl_fpc in '..\support\fsl_fpc.pas',
  FHIR.Support.Osx in '..\support\FHIR.Support.Osx.pas',
  fhir_utilities in '..\base\fhir_utilities.pas',
  fsl_npm_cache in '..
pm\fsl_npm_cache.pas',
  fhir_oauth in '..\client\fhir_oauth.pas';

var
  s: String;

begin
  CoInitialize(nil);
  s := ExtractFilePath(Paramstr(0));
  IdOpenSSLSetLibPath(s);
  GBasePath := 'C:\work\org.hl7.fhir';
  RunRegisteredTests;
  TTestingWorkerContext.closeUp;

end.
