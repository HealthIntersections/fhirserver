// JCL_DEBUG_EXPERT_INSERTJDBG ON
program FHIRServer2;
{
Copyright (c) 2001-2013, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$APPTYPE CONSOLE}

{$R *.res}

uses
  FastMM4 in '..\Libraries\FMM\FastMM4.pas',
  FastMM4Messages in '..\Libraries\FMM\FastMM4Messages.pas',
  Windows,
  System.SysUtils,
  Classes,
  IdSSLOpenSSLHeaders,
  JclDebug,
  FHIRServerApplicationCore in 'FHIRServerApplicationCore.pas',
  FHIRRestServer in 'FHIRRestServer.pas',
  FHIR.Support.Strings in '..\reference-platform\Support\FHIR.Support.Strings.pas',
  FHIR.Support.Math in '..\reference-platform\Support\FHIR.Support.Math.pas',
  FHIR.Support.DateTime in '..\reference-platform\Support\FHIR.Support.DateTime.pas',
  FHIR.Support.Binary in '..\reference-platform\Support\FHIR.Support.Binary.pas',
  FHIR.Support.Objects in '..\reference-platform\Support\FHIR.Support.Objects.pas',
  FHIR.Support.Exceptions in '..\reference-platform\Support\FHIR.Support.Exceptions.pas',
  FHIR.Support.Factory in '..\reference-platform\Support\FHIR.Support.Factory.pas',
  FHIR.Support.System in '..\reference-platform\Support\FHIR.Support.System.pas',
  FHIR.Support.Filers in '..\reference-platform\Support\FHIR.Support.Filers.pas',
  FHIR.Support.Collections in '..\reference-platform\Support\FHIR.Support.Collections.pas',
  FHIR.Support.Stream in '..\reference-platform\Support\FHIR.Support.Stream.pas',
  FHIR.Web.Parsers in '..\reference-platform\Support\FHIR.Web.Parsers.pas',
  FHIR.Support.Decimal in '..\reference-platform\Support\FHIR.Support.Decimal.pas',
  FHIR.Support.WInInet in '..\reference-platform\Support\FHIR.Support.WInInet.pas',
  FHIR.Support.Json in '..\reference-platform\Support\FHIR.Support.Json.pas',
  FHIRIndexManagers in 'FHIRIndexManagers.pas',
  FHIR.Ucum.Services in '..\Libraries\Ucum\FHIR.Ucum.Services.pas',
  FHIR.Ucum.Handlers in '..\Libraries\Ucum\FHIR.Ucum.Handlers.pas',
  FHIR.Ucum.Base in '..\Libraries\Ucum\FHIR.Ucum.Base.pas',
  FHIR.Ucum.Validators in '..\Libraries\Ucum\FHIR.Ucum.Validators.pas',
  FHIR.Ucum.Expressions in '..\Libraries\Ucum\FHIR.Ucum.Expressions.pas',
  FHIR.Ucum.Search in '..\Libraries\Ucum\FHIR.Ucum.Search.pas',
  FHIRValueSetExpander in 'FHIRValueSetExpander.pas',
  YuStemmer in '..\Libraries\Stem\YuStemmer.pas',
  FHIR.Loinc.Services in '..\Libraries\loinc\FHIR.Loinc.Services.pas',
  DISystemCompat in '..\Libraries\Stem\DISystemCompat.pas',
  FHIR.Snomed.Services in '..\Libraries\Snomed\FHIR.Snomed.Services.pas',
  FHIR.Web.Fetcher in '..\reference-platform\Support\FHIR.Web.Fetcher.pas',
  FHIR.Misc.Facebook in '..\reference-platform\Support\FHIR.Misc.Facebook.pas',
  FHIR.Support.Service in '..\reference-platform\Support\FHIR.Support.Service.pas',
  DBInstaller in 'DBInstaller.pas',
  FHIR.Database.Dialects in '..\reference-platform\Support\FHIR.Database.Dialects.pas',
  FHIR.Database.Logging in '..\Libraries\db\FHIR.Database.Logging.pas',
  FHIR.Database.Manager in '..\Libraries\db\FHIR.Database.Manager.pas',
  FHIR.Database.Utilities in '..\Libraries\db\FHIR.Database.Utilities.pas',
  FHIR.Database.Settings in '..\Libraries\db\FHIR.Database.Settings.pas',
  FHIR.Snomed.Importer in '..\Libraries\snomed\FHIR.Snomed.Importer.pas',
  FHIR.Snomed.Publisher in '..\Libraries\snomed\FHIR.Snomed.Publisher.pas',
  FHIR.Snomed.Expressions in '..\Libraries\snomed\FHIR.Snomed.Expressions.pas',
  FHIR.Web.HtmlGen in '..\reference-platform\Support\FHIR.Web.HtmlGen.pas',
  FHIR.Loinc.Importer in '..\Libraries\loinc\FHIR.Loinc.Importer.pas',
  FHIR.Loinc.Publisher in '..\Libraries\loinc\FHIR.Loinc.Publisher.pas',
  TerminologyServer in 'TerminologyServer.pas',
  TerminologyServerStore in 'TerminologyServerStore.pas',
  FHIR.Tx.Service in '..\Libraries\FHIR.Tx.Service.pas',
  FHIRValueSetChecker in 'FHIRValueSetChecker.pas',
  TerminologyWebServer in 'TerminologyWebServer.pas',
  FHIRServerConstants in 'FHIRServerConstants.pas',
  FHIRServerUtilities in 'FHIRServerUtilities.pas',
  SearchProcessor in 'SearchProcessor.pas',
  AuthServer in 'AuthServer.pas',
  SCIMServer in 'SCIMServer.pas',
  SCIMSearch in 'SCIMSearch.pas',
  FHIR.Misc.Twilio in '..\Libraries\security\FHIR.Misc.Twilio.pas',
  FHIRSearchSyntax in 'FHIRSearchSyntax.pas',
  FHIR.Support.Shell in '..\reference-platform\Support\FHIR.Support.Shell.pas',
  RectSupport in 'RectSupport.pas',
  CoordinateSupport in 'CoordinateSupport.pas',
  FHIR.Support.Generics in '..\reference-platform\Support\FHIR.Support.Generics.pas',
  FHIR.Support.Signatures in '..\reference-platform\Support\FHIR.Support.Signatures.pas',
  UriServices in 'UriServices.pas',
  UniiServices in 'UniiServices.pas',
  RxNormServices in 'RxNormServices.pas',
  IETFLanguageCodeServices in 'IETFLanguageCodeServices.pas',
  FHIR.Snomed.Analysis in '..\Libraries\snomed\FHIR.Snomed.Analysis.pas',
  AreaCodeServices in 'AreaCodeServices.pas',
  FHIRSubscriptionManager in 'FHIRSubscriptionManager.pas',
  ServerValidator in 'ServerValidator.pas',
  FHIR.Web.Socket in '..\reference-platform\Support\FHIR.Web.Socket.pas',
  FHIR.Support.MsXml in '..\reference-platform\Support\FHIR.Support.MsXml.pas',
  FHIR.Support.Mime in '..\reference-platform\Support\FHIR.Support.Mime.pas',
  FHIR.Support.Lock in '..\reference-platform\Support\FHIR.Support.Lock.pas',
  FHIR.R2.Questionnaire in '..\reference-platform\dstu2\FHIR.R2.Questionnaire.pas',
  FHIR.Base.Scim in '..\reference-platform\base\FHIR.Base.Scim.pas',
  FHIR.R2.Narrative2 in '..\reference-platform\dstu2\FHIR.R2.Narrative2.pas',
  FHIR.Tools.Security in '..\reference-platform\tools\FHIR.Tools.Security.pas',
  FHIR.R2.Narrative in '..\reference-platform\dstu2\FHIR.R2.Narrative.pas',
  FHIR.Client.SmartUtilities in '..\reference-platform\client\FHIR.Client.SmartUtilities.pas',
  FHIR.R2.PathEngine in '..\reference-platform\dstu2\FHIR.R2.PathEngine.pas',
  FHIR.R2.Tags in '..\reference-platform\dstu2\FHIR.R2.Tags.pas',
  FHIR.R2.Profiles in '..\reference-platform\dstu2\FHIR.R2.Profiles.pas',
  FHIR.Base.Objects in '..\reference-platform\base\FHIR.Base.Objects.pas',
  FHIR.R2.Types in '..\reference-platform\dstu2\FHIR.R2.Types.pas',
  FHIR.R2.Resources in '..\reference-platform\dstu2\FHIR.R2.Resources.pas',
  FHIR.Tools.Parser in '..\reference-platform\tools\FHIR.Tools.Parser.pas',
  FHIR.R2.Xml in '..\reference-platform\dstu2\FHIR.R2.Xml.pas',
  FHIR.R2.Json in '..\reference-platform\dstu2\FHIR.R2.Json.pas',
  FHIR.Base.Parser in '..\reference-platform\base\FHIR.Base.Parser.pas',
  FHIR.R2.Constants in '..\reference-platform\dstu2\FHIR.R2.Constants.pas',
  FHIR.Tools.Session in '..\reference-platform\tools\FHIR.Tools.Session.pas',
  FHIR.Base.Lang in '..\reference-platform\base\FHIR.Base.Lang.pas',
  FHIR.R2.Utilities in '..\reference-platform\dstu2\FHIR.R2.Utilities.pas',
  FHIR.Tools.Client in '..\reference-platform\client\FHIR.Tools.Client.pas',
  FHIR.R2.Validator in '..\reference-platform\dstu2\FHIR.R2.Validator.pas',
  ClosureManager in 'ClosureManager.pas',
  FHIR.CdsHooks.Utilities in '..\reference-platform\support\FHIR.CdsHooks.Utilities.pas',
  MarkdownProcessor in '..\..\markdown\source\MarkdownProcessor.pas',
  MarkdownDaringFireball in '..\..\markdown\source\MarkdownDaringFireball.pas',
  MarkdownDaringFireballTests in '..\..\markdown\source\MarkdownDaringFireballTests.pas',
  AccessControlEngine in 'AccessControlEngine.pas',
  MPISearch in 'MPISearch.pas',
  FHIR.Web.Rdf in '..\reference-platform\support\FHIR.Web.Rdf.pas',
  FHIR.R2.Operations in '..\reference-platform\dstu2\FHIR.R2.Operations.pas',
  FHIR.R2.ElementModel in '..\reference-platform\dstu2\FHIR.R2.ElementModel.pas',
  FHIR.R2.IndexInfo in '..\reference-platform\dstu2\FHIR.R2.IndexInfo.pas',
  FHIR.R2.OpBase in '..\reference-platform\dstu2\FHIR.R2.OpBase.pas',
  FHIR.Base.Xhtml in '..\reference-platform\base\FHIR.Base.Xhtml.pas',
  FHIR.R2.Context in '..\reference-platform\dstu2\FHIR.R2.Context.pas',
  FHIR.Debug.Logging in '..\reference-platform\support\FHIR.Debug.Logging.pas',
  FHIR.R2.AuthMap in '..\reference-platform\dstu2\FHIR.R2.AuthMap.pas',
  FHIR.Tools.DiffEngine in '..\reference-platform\tools\FHIR.Tools.DiffEngine.pas',
  ACIRServices in 'ACIRServices.pas',
  ReverseClient in 'ReverseClient.pas',
  FHIRNativeStorage in 'FHIRNativeStorage.pas',
  FHIRUserProvider in 'FHIRUserProvider.pas',
  FHIR.Tools.GraphQL in '..\reference-platform\tools\FHIR.Tools.GraphQL.pas',
  FHIR.Support.MXml in '..\reference-platform\support\FHIR.Support.MXml.pas',
  MarkdownCommonMark in '..\..\markdown\source\MarkdownCommonMark.pas',
  FHIR.Tools.CodeGen in '..\reference-platform\tools\FHIR.Tools.CodeGen.pas',
  CDSHooksServer in 'CDSHooksServer.pas',
  CDSHooksServices in 'CDSHooksServices.pas',
  FHIR.Support.Turtle in '..\reference-platform\support\FHIR.Support.Turtle.pas',
  FHIR.Misc.ApplicationVerifier in '..\Libraries\security\FHIR.Misc.ApplicationVerifier.pas',
  JWTService in 'JWTService.pas',
  FHIR.CdsHooks.Client in '..\reference-platform\support\FHIR.CdsHooks.Client.pas',
  FHIR.Utilities.SCrypt in '..\Libraries\security\FHIR.Utilities.SCrypt.pas',
  TerminologyOperations in 'TerminologyOperations.pas',
  WebSourceProvider in 'WebSourceProvider.pas',
  FHIR.Tools.Indexing in '..\reference-platform\tools\FHIR.Tools.Indexing.pas',
  FHIR.Database.ODBC in '..\Libraries\db\FHIR.Database.ODBC.pas',
  FHIR.Database.ODBC.Objects in '..\Libraries\db\FHIR.Database.ODBC.Objects.pas',
  FHIR.Database.ODBC.Headers in '..\Libraries\db\FHIR.Database.ODBC.Headers.pas',
  FHIR.Database.SQLite3.Objects in '..\Libraries\db\FHIR.Database.SQLite3.Objects.pas',
  FHIR.Database.SQLite3.Utilities in '..\Libraries\db\FHIR.Database.SQLite3.Utilities.pas',
  FHIR.Database.SQLite3.Wrapper in '..\Libraries\db\FHIR.Database.SQLite3.Wrapper.pas',
  FHIR.Database.SQLite in '..\Libraries\db\FHIR.Database.SQLite.pas',
  ServerPostHandlers in 'ServerPostHandlers.pas',
  ServerJavascriptHost in 'ServerJavascriptHost.pas',
  FHIR.Support.Javascript in '..\Libraries\js\FHIR.Support.Javascript.pas',
  FHIR.R2.Javascript in '..\reference-platform\dstu2\FHIR.R2.Javascript.pas',
  FHIR.Javascript.Base in '..\Libraries\js\FHIR.Javascript.Base.pas',
  FHIR.Client.Javascript in '..\Libraries\js\FHIR.Client.Javascript.pas',
  ServerEventJs in 'ServerEventJs.pas',
  FHIR.Javascript in '..\Libraries\js\FHIR.Javascript.pas',
  FHIR.Javascript.Chakra in '..\Libraries\js\FHIR.Javascript.Chakra.pas',
  FHIR.R2.Factory in '..\reference-platform\dstu2\FHIR.R2.Factory.pas',
  USStatesServices in 'USStatesServices.pas',
  CountryCodeServices in 'CountryCodeServices.pas',
  FHIR.R2.PathNode in '..\reference-platform\dstu2\FHIR.R2.PathNode.pas',
  FHIR.Ucum.IFace in '..\reference-platform\support\FHIR.Ucum.IFace.pas',
  FHIR.R2.Base in '..\reference-platform\dstu2\FHIR.R2.Base.pas',
  FHIR.Tools.XhtmlComp in '..\reference-platform\tools\FHIR.Tools.XhtmlComp.pas',
  FHIR.R2.Parser in '..\reference-platform\dstu2\FHIR.R2.Parser.pas',
  FHIR.R2.ParserBase in '..\reference-platform\dstu2\FHIR.R2.ParserBase.pas',
  FHIR.Client.Base in '..\reference-platform\client\FHIR.Client.Base.pas',
  FHIR.Client.HTTP in '..\reference-platform\client\FHIR.Client.HTTP.pas',
  FHIR.Client.Threaded in '..\reference-platform\client\FHIR.Client.Threaded.pas',
  FHIR.R2.Client in '..\reference-platform\dstu2\FHIR.R2.Client.pas',
  FHIR.Support.Text in '..\reference-platform\support\FHIR.Support.Text.pas',
  FHIR.Support.Zip in '..\reference-platform\support\FHIR.Support.Zip.pas',
  FHIR.Support.Xml in '..\reference-platform\support\FHIR.Support.Xml.pas',
  FHIR.Support.Controllers in '..\reference-platform\support\FHIR.Support.Controllers.pas',
  FHIR.Support.Certs in '..\reference-platform\support\FHIR.Support.Certs.pas',
  FHIR.Misc.GraphQL in '..\reference-platform\support\FHIR.Misc.GraphQL.pas',
  FHIR.Base.Factory in '..\reference-platform\base\FHIR.Base.Factory.pas',
  FHIR.Base.Validator in '..\reference-platform\base\FHIR.Base.Validator.pas',
  FHIR.XVersion.Resources in '..\reference-platform\xversion\FHIR.XVersion.Resources.pas',
  FHIR.Base.Narrative in '..\reference-platform\base\FHIR.Base.Narrative.pas',
  FHIR.Base.PathEngine in '..\reference-platform\base\FHIR.Base.PathEngine.pas',
  FHIR.R2.Common in '..\reference-platform\dstu2\FHIR.R2.Common.pas',
  FHIR.Cache.PackageManager in '..\reference-platform\cache\FHIR.Cache.PackageManager.pas',
  FHIR.Support.Tarball in '..\reference-platform\support\FHIR.Support.Tarball.pas';

begin
  logfile := Path([SystemTemp, 'fhirserver.log']);
  if ParamCount = 0 then
  begin
    filelog := true;
    logt('testing');
  end;
  JclStartExceptionTracking;
  IdOpenSSLSetLibPath(ExtractFilePath(Paramstr(0)));
  try
    SetConsoleTitle('FHIR Server DSTU2');
    ExecuteFhirServer;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
    end;
  end;
end.


