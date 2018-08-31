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
  FHIR.Server.Kernel in 'FHIR.Server.Kernel.pas',
  FHIR.Server.Web in 'FHIR.Server.Web.pas',
  FHIR.Support.Base in '..\reference-platform\Support\FHIR.Support.Base.pas',
  FHIR.Support.Utilities in '..\reference-platform\Support\FHIR.Support.Utilities.pas',
  FHIR.Support.Collections in '..\reference-platform\Support\FHIR.Support.Collections.pas',
  FHIR.Support.Stream in '..\reference-platform\Support\FHIR.Support.Stream.pas',
  FHIR.Web.Parsers in '..\reference-platform\Support\FHIR.Web.Parsers.pas',
  FHIR.Web.WinInet in '..\reference-platform\Support\FHIR.Web.WinInet.pas',
  FHIR.Support.Json in '..\reference-platform\Support\FHIR.Support.Json.pas',
  FHIR.Server.Indexing in 'FHIR.Server.Indexing.pas',
  FHIR.Ucum.Services in '..\Libraries\Ucum\FHIR.Ucum.Services.pas',
  FHIR.Ucum.Handlers in '..\Libraries\Ucum\FHIR.Ucum.Handlers.pas',
  FHIR.Ucum.Base in '..\Libraries\Ucum\FHIR.Ucum.Base.pas',
  FHIR.Ucum.Validators in '..\Libraries\Ucum\FHIR.Ucum.Validators.pas',
  FHIR.Ucum.Expressions in '..\Libraries\Ucum\FHIR.Ucum.Expressions.pas',
  FHIR.Ucum.Search in '..\Libraries\Ucum\FHIR.Ucum.Search.pas',
  FHIR.Tx.Expander in 'FHIR.Tx.Expander.pas',
  YuStemmer in '..\Libraries\Stem\YuStemmer.pas',
  FHIR.Loinc.Services in '..\Libraries\loinc\FHIR.Loinc.Services.pas',
  DISystemCompat in '..\Libraries\Stem\DISystemCompat.pas',
  FHIR.Snomed.Services in '..\Libraries\Snomed\FHIR.Snomed.Services.pas',
  FHIR.Web.Fetcher in '..\reference-platform\Support\FHIR.Web.Fetcher.pas',
  FHIR.Web.Facebook in '..\reference-platform\Support\FHIR.Web.Facebook.pas',
  FHIR.Support.Service in '..\reference-platform\Support\FHIR.Support.Service.pas',
  FHIR.Server.DBInstaller in 'FHIR.Server.DBInstaller.pas',
  FHIR.Database.Dialects in '..\Libraries\db\FHIR.Database.Dialects.pas',
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
  FHIR.Tx.Server in 'FHIR.Tx.Server.pas',
  FHIR.Tx.Manager in 'FHIR.Tx.Manager.pas',
  FHIR.Tx.Service in '..\Libraries\FHIR.Tx.Service.pas',
  FHIR.Tx.Validator in 'FHIR.Tx.Validator.pas',
  FHIR.Tx.Web in 'FHIR.Tx.Web.pas',
  FHIR.Server.Constants in 'FHIR.Server.Constants.pas',
  FHIR.Server.Ini in 'FHIR.Server.Ini.pas',
  FHIR.Server.Search in 'FHIR.Server.Search.pas',
  FHIR.Server.AuthMgr in 'FHIR.Server.AuthMgr.pas',
  FHIR.Scim.Server in 'FHIR.Scim.Server.pas',
  FHIR.Scim.Search in 'FHIR.Scim.Search.pas',
  FHIR.Misc.Twilio in '..\Libraries\security\FHIR.Misc.Twilio.pas',
  FHIR.Server.SearchSyntax in 'FHIR.Server.SearchSyntax.pas',
  FHIR.Support.Shell in '..\reference-platform\Support\FHIR.Support.Shell.pas',
  FHIR.Support.Signatures in '..\reference-platform\Support\FHIR.Support.Signatures.pas',
  FHIR.Tx.Uri in 'FHIR.Tx.Uri.pas',
  FHIR.Tx.Unii in 'FHIR.Tx.Unii.pas',
  FHIR.Tx.RxNorm in 'FHIR.Tx.RxNorm.pas',
  FHIR.Tx.Lang in 'FHIR.Tx.Lang.pas',
  FHIR.Snomed.Analysis in '..\Libraries\snomed\FHIR.Snomed.Analysis.pas',
  FHIR.Tx.AreaCode in 'FHIR.Tx.AreaCode.pas',
  FHIR.Server.Subscriptions in 'FHIR.Server.Subscriptions.pas',
  FHIR.Server.Validator in 'FHIR.Server.Validator.pas',
  FHIR.Web.Socket in '..\reference-platform\Support\FHIR.Web.Socket.pas',
  FHIR.Support.MsXml in '..\reference-platform\Support\FHIR.Support.MsXml.pas',
  FHIR.R2.Questionnaire in '..\reference-platform\dstu2\FHIR.R2.Questionnaire.pas',
  FHIR.Base.Scim in '..\reference-platform\base\FHIR.Base.Scim.pas',
  FHIR.R2.Narrative2 in '..\reference-platform\dstu2\FHIR.R2.Narrative2.pas',
  FHIR.Server.Security in '..\reference-platform\tools\FHIR.Server.Security.pas',
  FHIR.R2.Narrative in '..\reference-platform\dstu2\FHIR.R2.Narrative.pas',
  FHIR.Smart.Utilities in '..\reference-platform\client\FHIR.Smart.Utilities.pas',
  FHIR.R2.PathEngine in '..\reference-platform\dstu2\FHIR.R2.PathEngine.pas',
  FHIR.R2.Tags in '..\reference-platform\dstu2\FHIR.R2.Tags.pas',
  FHIR.R2.Profiles in '..\reference-platform\dstu2\FHIR.R2.Profiles.pas',
  FHIR.Base.Objects in '..\reference-platform\base\FHIR.Base.Objects.pas',
  FHIR.R2.Types in '..\reference-platform\dstu2\FHIR.R2.Types.pas',
  FHIR.R2.Resources in '..\reference-platform\dstu2\FHIR.R2.Resources.pas',
  FHIR.Version.Parser in '..\reference-platform\version\FHIR.Version.Parser.pas',
  FHIR.R2.Xml in '..\reference-platform\dstu2\FHIR.R2.Xml.pas',
  FHIR.R2.Json in '..\reference-platform\dstu2\FHIR.R2.Json.pas',
  FHIR.Base.Parser in '..\reference-platform\base\FHIR.Base.Parser.pas',
  FHIR.R2.Constants in '..\reference-platform\dstu2\FHIR.R2.Constants.pas',
  FHIR.Server.Session in '..\reference-platform\tools\FHIR.Server.Session.pas',
  FHIR.Base.Lang in '..\reference-platform\base\FHIR.Base.Lang.pas',
  FHIR.R2.Utilities in '..\reference-platform\dstu2\FHIR.R2.Utilities.pas',
  FHIR.Version.Client in '..\reference-platform\version\FHIR.Version.Client.pas',
  FHIR.R2.Validator in '..\reference-platform\dstu2\FHIR.R2.Validator.pas',
  FHIR.Server.ClosureMgr in 'FHIR.Server.ClosureMgr.pas',
  FHIR.CdsHooks.Utilities in '..\reference-platform\support\FHIR.CdsHooks.Utilities.pas',
  MarkdownProcessor in '..\..\markdown\source\MarkdownProcessor.pas',
  MarkdownDaringFireball in '..\..\markdown\source\MarkdownDaringFireball.pas',
  MarkdownDaringFireballTests in '..\..\markdown\source\MarkdownDaringFireballTests.pas',
  FHIR.Server.AccessControl in 'FHIR.Server.AccessControl.pas',
  FHIR.Server.MpiSearch in 'FHIR.Server.MpiSearch.pas',
  FHIR.Web.Rdf in '..\reference-platform\support\FHIR.Web.Rdf.pas',
  FHIR.R2.Operations in '..\reference-platform\dstu2\FHIR.R2.Operations.pas',
  FHIR.R2.ElementModel in '..\reference-platform\dstu2\FHIR.R2.ElementModel.pas',
  FHIR.R2.IndexInfo in '..\reference-platform\dstu2\FHIR.R2.IndexInfo.pas',
  FHIR.R2.OpBase in '..\reference-platform\dstu2\FHIR.R2.OpBase.pas',
  FHIR.Base.Xhtml in '..\reference-platform\base\FHIR.Base.Xhtml.pas',
  FHIR.R2.Context in '..\reference-platform\dstu2\FHIR.R2.Context.pas',
  FHIR.Support.Logging in '..\reference-platform\support\FHIR.Support.Logging.pas',
  FHIR.R2.AuthMap in '..\reference-platform\dstu2\FHIR.R2.AuthMap.pas',
  FHIR.Tools.DiffEngine in '..\reference-platform\tools\FHIR.Tools.DiffEngine.pas',
  FHIR.Tx.ACIR in 'FHIR.Tx.ACIR.pas',
  FHIR.Server.ReverseClient in 'FHIR.Server.ReverseClient.pas',
  FHIR.Server.Database in 'FHIR.Server.Database.pas',
  FHIR.Server.UserMgr in 'FHIR.Server.UserMgr.pas',
  FHIR.Tools.GraphQL in '..\reference-platform\tools\FHIR.Tools.GraphQL.pas',
  FHIR.Support.MXml in '..\reference-platform\support\FHIR.Support.MXml.pas',
  MarkdownCommonMark in '..\..\markdown\source\MarkdownCommonMark.pas',
  FHIR.Tools.CodeGen in '..\reference-platform\tools\FHIR.Tools.CodeGen.pas',
  FHIR.CdsHooks.Server in 'FHIR.CdsHooks.Server.pas',
  FHIR.CdsHooks.Service in 'FHIR.CdsHooks.Service.pas',
  FHIR.Support.Turtle in '..\reference-platform\support\FHIR.Support.Turtle.pas',
  FHIR.Misc.ApplicationVerifier in '..\Libraries\security\FHIR.Misc.ApplicationVerifier.pas',
  FHIR.Server.Jwt in 'FHIR.Server.Jwt.pas',
  FHIR.CdsHooks.Client in '..\reference-platform\support\FHIR.CdsHooks.Client.pas',
  FHIR.Utilities.SCrypt in '..\Libraries\security\FHIR.Utilities.SCrypt.pas',
  FHIR.Tx.Operations in 'FHIR.Tx.Operations.pas',
  FHIR.Server.WebSource in 'FHIR.Server.WebSource.pas',
  FHIR.Tools.Indexing in '..\reference-platform\tools\FHIR.Tools.Indexing.pas',
  FHIR.Database.ODBC in '..\Libraries\db\FHIR.Database.ODBC.pas',
  FHIR.Database.ODBC.Objects in '..\Libraries\db\FHIR.Database.ODBC.Objects.pas',
  FHIR.Database.ODBC.Headers in '..\Libraries\db\FHIR.Database.ODBC.Headers.pas',
  FHIR.Database.SQLite3.Objects in '..\Libraries\db\FHIR.Database.SQLite3.Objects.pas',
  FHIR.Database.SQLite3.Utilities in '..\Libraries\db\FHIR.Database.SQLite3.Utilities.pas',
  FHIR.Database.SQLite3.Wrapper in '..\Libraries\db\FHIR.Database.SQLite3.Wrapper.pas',
  FHIR.Database.SQLite in '..\Libraries\db\FHIR.Database.SQLite.pas',
  FHIR.Server.PostHandlers in 'FHIR.Server.PostHandlers.pas',
  FHIR.Server.Javascript in 'FHIR.Server.Javascript.pas',
  FHIR.Support.Javascript in '..\Libraries\js\FHIR.Support.Javascript.pas',
  FHIR.R2.Javascript in '..\reference-platform\dstu2\FHIR.R2.Javascript.pas',
  FHIR.Javascript.Base in '..\Libraries\js\FHIR.Javascript.Base.pas',
  FHIR.Client.Javascript in '..\Libraries\js\FHIR.Client.Javascript.pas',
  FHIR.Server.EventJs in 'FHIR.Server.EventJs.pas',
  FHIR.Javascript in '..\Libraries\js\FHIR.Javascript.pas',
  FHIR.Javascript.Chakra in '..\Libraries\js\FHIR.Javascript.Chakra.pas',
  FHIR.R2.Factory in '..\reference-platform\dstu2\FHIR.R2.Factory.pas',
  FHIR.Tx.UsState in 'FHIR.Tx.UsState.pas',
  FHIR.Tx.CountryCode in 'FHIR.Tx.CountryCode.pas',
  FHIR.R2.PathNode in '..\reference-platform\dstu2\FHIR.R2.PathNode.pas',
  FHIR.Ucum.IFace in '..\reference-platform\support\FHIR.Ucum.IFace.pas',
  FHIR.R2.Base in '..\reference-platform\dstu2\FHIR.R2.Base.pas',
  FHIR.Server.XhtmlComp in 'FHIR.Server.XhtmlComp.pas',
  FHIR.R2.Parser in '..\reference-platform\dstu2\FHIR.R2.Parser.pas',
  FHIR.R2.ParserBase in '..\reference-platform\dstu2\FHIR.R2.ParserBase.pas',
  FHIR.Client.Base in '..\reference-platform\client\FHIR.Client.Base.pas',
  FHIR.Client.HTTP in '..\reference-platform\client\FHIR.Client.HTTP.pas',
  FHIR.Client.Threaded in '..\reference-platform\client\FHIR.Client.Threaded.pas',
  FHIR.R2.Client in '..\reference-platform\dstu2\FHIR.R2.Client.pas',
  FHIR.Support.Xml in '..\reference-platform\support\FHIR.Support.Xml.pas',
  FHIR.Support.Threads in '..\reference-platform\support\FHIR.Support.Threads.pas',
  FHIR.Support.Certs in '..\reference-platform\support\FHIR.Support.Certs.pas',
  FHIR.Web.GraphQL in '..\reference-platform\support\FHIR.Web.GraphQL.pas',
  FHIR.Base.Factory in '..\reference-platform\base\FHIR.Base.Factory.pas',
  FHIR.Base.Validator in '..\reference-platform\base\FHIR.Base.Validator.pas',
  FHIR.Base.Common in '..\reference-platform\base\FHIR.Base.Common.pas',
  FHIR.Base.Narrative in '..\reference-platform\base\FHIR.Base.Narrative.pas',
  FHIR.Base.PathEngine in '..\reference-platform\base\FHIR.Base.PathEngine.pas',
  FHIR.R2.Common in '..\reference-platform\dstu2\FHIR.R2.Common.pas',
  FHIR.Cache.PackageManager in '..\reference-platform\cache\FHIR.Cache.PackageManager.pas',
  FHIR.Base.Utilities in '..\reference-platform\base\FHIR.Base.Utilities.pas',
  fhir.support.fpc in '..\reference-platform\support\fhir.support.fpc.pas',
  FHIR.Support.Osx in '..\reference-platform\support\FHIR.Support.Osx.pas',
  FHIR.Tx.MimeTypes in 'FHIR.Tx.MimeTypes.pas',
  FHIR.Tx.Iso4217 in 'FHIR.Tx.Iso4217.pas';

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


