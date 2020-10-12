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
  FHIR.Support.Base in '..\Support\FHIR.Support.Base.pas',
  FHIR.Support.Stream in '..\support\FHIR.Support.Stream.pas',
  FHIR.Web.WinInet in '..\support\FHIR.Web.WinInet.pas',
  FHIR.Support.Utilities in '..\support\FHIR.Support.Utilities.pas',
  FHIR.Support.MXml in '..\support\FHIR.Support.MXml.pas',
  FHIR.Support.MsXml in '..\support\FHIR.Support.MsXml.pas',
  FHIR.Web.Parsers in '..\support\FHIR.Web.Parsers.pas',
  FHIR.Web.Rdf in '..\support\FHIR.Web.Rdf.pas',
  FHIR.Support.Shell in '..\support\FHIR.Support.Shell.pas',
  FHIR.Web.Fetcher in '..\support\FHIR.Web.Fetcher.pas',
  FHIR.Support.Turtle in '..\support\FHIR.Support.Turtle.pas',
  FHIR.Support.Logging in '..\support\FHIR.Support.Logging.pas',
  FHIR.Support.Signatures in '..\support\FHIR.Support.Signatures.pas',
  FHIR.Support.Collections in '..\support\FHIR.Support.Collections.pas',
  FHIR.Support.Threads in '..\support\FHIR.Support.Threads.pas',
  FHIR.Support.Xml in '..\support\FHIR.Support.Xml.pas',
  FHIR.Support.Json in '..\support\FHIR.Support.Json.pas',
  FHIR.Support.Certs in '..\support\FHIR.Support.Certs.pas',
  FHIR.Web.GraphQL in '..\support\FHIR.Web.GraphQL.pas',
  FHIR.Ucum.IFace in '..\support\FHIR.Ucum.IFace.pas',
  FHIR.Base.Objects in '..\base\FHIR.Base.Objects.pas',
  FHIR.Base.Lang in '..\base\FHIR.Base.Lang.pas',
  FHIR.Base.Parser in '..\base\FHIR.Base.Parser.pas',
  FHIR.Base.Scim in '..\base\FHIR.Base.Scim.pas',
  FHIR.Base.Xhtml in '..\base\FHIR.Base.Xhtml.pas',
  FHIR.Version.Parser in '..\version\FHIR.Version.Parser.pas',
  FHIR.Tools.Indexing in '..\tools\FHIR.Tools.Indexing.pas',
  FHIR.Tools.DiffEngine in '..\tools\FHIR.Tools.DiffEngine.pas',
  FHIR.R4.Constants,
  FHIR.R4.PathEngine in 'FHIR.R4.PathEngine.pas',
  FHIR.R4.Resources in 'FHIR.R4.Resources.pas',
  FHIR.R4.Utilities in 'FHIR.R4.Utilities.pas',
  FHIR.R4.Xml in 'FHIR.R4.Xml.pas',
  FHIR.R4.Json in 'FHIR.R4.Json.pas',
  FHIR.R4.Turtle in 'FHIR.R4.Turtle.pas',
  FHIR.Tests.Decimal in '..\support\Tests\FHIR.Tests.Decimal.pas',
  DifferenceEngineTests in '..\support\Tests\DifferenceEngineTests.pas',
  FHIR.R4.Tests.PathEngine in 'tests\FHIR.R4.Tests.PathEngine.pas',
  FHIR.R4.Tests.Worker in 'tests\FHIR.R4.Tests.Worker.pas',
  JWTTests in '..\support\Tests\JWTTests.pas',
  JsonTests in '..\support\Tests\javascriptonTests.pas',
  XmlTests in '..\support\Tests\XmlTests.pas',
  FHIR.Base.Common in '..\base\FHIR.Base.Common.pas',
  FHIR.Base.Factory in '..\base\FHIR.Base.Factory.pas',
  FHIR.Base.Narrative in '..\base\FHIR.Base.Narrative.pas',
  FHIR.Base.Validator in '..\base\FHIR.Base.Validator.pas',
  FHIR.Base.PathEngine in '..\base\FHIR.Base.PathEngine.pas',
  FHIR.Client.Base in '..\client\FHIR.Client.Base.pas',
  FHIR.Client.Threaded in '..\client\FHIR.Client.Threaded.pas',
  FHIR.Client.HTTP in '..\client\FHIR.Client.HTTP.pas',
  fhir.support.fpc in '..\support\fhir.support.fpc.pas',
  FHIR.Support.Osx in '..\support\FHIR.Support.Osx.pas',
  FHIR.Base.Utilities in '..\base\FHIR.Base.Utilities.pas',
  FHIR.Npm.Cache in '..
pm\FHIR.Npm.Cache.pas',
  FHIR.Smart.Utilities in '..\client\FHIR.Smart.Utilities.pas';

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
