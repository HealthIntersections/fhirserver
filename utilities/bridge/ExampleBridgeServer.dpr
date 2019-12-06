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
program ExampleBridgeServer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  FastMM4 in '..\..\dependencies\FMM\FastMM4.pas',
  System.SysUtils,
  ExampleBridge in '..\ExampleBridge.pas',
  FHIR.Support.Collections in '..\..\library\support\FHIR.Support.Collections.pas',
  FHIR.Support.Json in '..\..\library\support\FHIR.Support.Json.pas',
  FHIR.Support.Base in '..\..\library\support\FHIR.Support.Base.pas',
  FHIR.Support.Stream in '..\..\library\support\FHIR.Support.Stream.pas',
  FHIR.Web.WinInet in '..\..\library\web\FHIR.Web.WinInet.pas',
  FHIR.Base.Objects in '..\..\library\base\FHIR.Base.Objects.pas',
  FHIR.Web.Facebook in '..\..\library\web\FHIR.Web.Facebook.pas',
  FHIR.Support.Utilities in '..\..\library\support\FHIR.Support.Utilities.pas',
  FHIR.Web.HtmlGen in '..\..\library\web\FHIR.Web.HtmlGen.pas',
  FHIR.Web.Socket in '..\..\library\web\FHIR.Web.Socket.pas',
  FHIR.Web.Fetcher in '..\..\library\web\FHIR.Web.Fetcher.pas',
  FHIR.Database.Dialects in '..\..\library\database\FHIR.Database.Dialects.pas',
  FHIR.Web.Parsers in '..\..\library\web\FHIR.Web.Parsers.pas',
  FHIR.Web.Rdf in '..\..\library\web\FHIR.Web.Rdf.pas',
  FHIR.Support.Shell in '..\..\library\support\FHIR.Support.Shell.pas',
  FHIR.Support.Service in '..\..\library\support\FHIR.Support.Service.pas',
  FHIR.Support.Threads in '..\..\library\support\FHIR.Support.Threads.pas',
  MarkdownDaringFireball in '..\..\..\markdown\source\MarkdownDaringFireball.pas',
  MarkdownProcessor in '..\..\..\markdown\source\MarkdownProcessor.pas',
  FHIR.CdsHooks.Utilities in '..\..\library\cdshooks\FHIR.CdsHooks.Utilities.pas',
  FHIR.R3.AuthMap in '..\..\library\r3\FHIR.R3.AuthMap.pas',
  FHIR.Version.Client in '..\..\library\version\FHIR.Version.Client.pas',
  FHIR.R3.Constants in '..\..\library\r3\FHIR.R3.Constants.pas',
  FHIR.R3.Context in '..\..\library\r3\FHIR.R3.Context.pas',
  FHIR.R3.IndexInfo in '..\..\library\r3\FHIR.R3.IndexInfo.pas',
  FHIR.Base.Lang in '..\..\library\base\FHIR.Base.Lang.pas',
  FHIR.R3.ElementModel in '..\..\library\r3\FHIR.R3.ElementModel.pas',
  FHIR.R3.Operations in '..\..\library\r3\FHIR.R3.Operations.pas',
  FHIR.Version.Parser in '..\..\library\version\FHIR.Version.Parser.pas',
  FHIR.R3.Xml in '..\..\library\r3\FHIR.R3.Xml.pas',
  FHIR.R3.Json in '..\..\library\r3\FHIR.R3.Json.pas',
  FHIR.R3.Turtle in '..\..\library\r3\FHIR.R3.Turtle.pas',
  FHIR.Base.Parser in '..\..\library\base\FHIR.Base.Parser.pas',
  FHIR.R3.Profiles in '..\..\library\r3\FHIR.R3.Profiles.pas',
  FHIR.R3.Resources in '..\..\library\r3\FHIR.R3.Resources.pas',
  FHIR.Server.Security in '..\..\server\FHIR.Server.Security.pas',
  FHIR.Server.Session in '..\..\server\FHIR.Server.Session.pas',
  FHIR.R3.Tags in '..\..\library\r3\FHIR.R3.Tags.pas',
  FHIR.R3.Types in '..\..\library\r3\FHIR.R3.Types.pas',
  FHIR.R3.Utilities in '..\..\library\r3\FHIR.R3.Utilities.pas',
  FHIR.R3.Validator in '..\..\library\r3\FHIR.R3.Validator.pas',
  FHIR.Base.Xhtml in '..\..\library\base\FHIR.Base.Xhtml.pas',
  FHIR.R3.OpBase in '..\..\library\r3\FHIR.R3.OpBase.pas',
  FHIR.R3.PathEngine in '..\..\library\r3\FHIR.R3.PathEngine.pas',
  FHIR.R3.Narrative2 in '..\..\library\r3\FHIR.R3.Narrative2.pas',
  FHIR.R3.Questionnaire in '..\..\library\r3\FHIR.R3.Questionnaire.pas',
  FHIR.Base.Scim in '..\..\library\base\FHIR.Base.Scim.pas',
  FHIR.Smart.Utilities in '..\..\library\smart\FHIR.Smart.Utilities.pas',
  FHIR.Tx.Service in '..\..\library\FHIR.Tx.Service.pas',
  FHIR.Database.Logging in '..\..\library\database\FHIR.Database.Logging.pas',
  FHIR.Database.Manager in '..\..\library\database\FHIR.Database.Manager.pas',
  FHIR.Database.Settings in '..\..\library\database\FHIR.Database.Settings.pas',
  FHIR.Loinc.Publisher in '..\..\library\loinc\FHIR.Loinc.Publisher.pas',
  FHIR.Loinc.Services in '..\..\library\loinc\FHIR.Loinc.Services.pas',
  FHIR.Web.Twilio in '..\..\library\web\FHIR.Web.Twilio.pas',
  FHIR.Snomed.Analysis in '..\..\library\snomed\FHIR.Snomed.Analysis.pas',
  FHIR.Snomed.Expressions in '..\..\library\snomed\FHIR.Snomed.Expressions.pas',
  FHIR.Snomed.Publisher in '..\..\library\snomed\FHIR.Snomed.Publisher.pas',
  FHIR.Snomed.Services in '..\..\library\snomed\FHIR.Snomed.Services.pas',
  DISystemCompat in '..\..\dependencies\Stem\DISystemCompat.pas',
  YuStemmer in '..\..\dependencies\Stem\YuStemmer.pas',
  FHIR.Ucum.Base in '..\..\library\Ucum\FHIR.Ucum.Base.pas',
  FHIR.Ucum.Expressions in '..\..\library\Ucum\FHIR.Ucum.Expressions.pas',
  FHIR.Ucum.Handlers in '..\..\library\Ucum\FHIR.Ucum.Handlers.pas',
  FHIR.Ucum.Search in '..\..\library\Ucum\FHIR.Ucum.Search.pas',
  FHIR.Ucum.Services in '..\..\library\Ucum\FHIR.Ucum.Services.pas',
  FHIR.Ucum.Validators in '..\..\library\Ucum\FHIR.Ucum.Validators.pas',
  FHIR.Tx.ACIR in '..\..\Server\FHIR.Tx.ACIR.pas',
  FHIR.Tx.AreaCode in '..\..\Server\FHIR.Tx.AreaCode.pas',
  FHIR.Server.AuthMgr in '..\..\Server\FHIR.Server.AuthMgr.pas',
  FHIR.Server.ClosureMgr in '..\..\Server\FHIR.Server.ClosureMgr.pas',
  FHIR.Server.Indexing in '..\..\Server\FHIR.Server.Indexing.pas',
  FHIR.Support.Logging in '..\..\library\support\FHIR.Support.Logging.pas',
  FHIR.Server.Web in '..\..\Server\FHIR.Server.Web.pas',
  FHIR.Server.Constants in '..\..\Server\FHIR.Server.Constants.pas',
  FHIR.Server.Context in '..\..\Server\FHIR.Server.Context.pas',
  FHIR.Server.Ini in '..\..\Server\FHIR.Server.Ini.pas',
  FHIR.Server.SessionMgr in '..\..\Server\FHIR.Server.SessionMgr.pas',
  FHIR.Server.Storage in '..\..\Server\FHIR.Server.Storage.pas',
  FHIR.Server.Subscriptions in '..\..\Server\FHIR.Server.Subscriptions.pas',
  FHIR.Server.TagMgr in '..\..\Server\FHIR.Server.TagMgr.pas',
  FHIR.Tx.Lang in '..\..\Server\FHIR.Tx.Lang.pas',
  FHIR.Server.OpenMHealth in '..\..\Server\FHIR.Server.OpenMHealth.pas',
  FHIR.Server.ReverseClient in '..\..\Server\FHIR.Server.ReverseClient.pas',
  FHIR.Tx.RxNorm in '..\..\Server\FHIR.Tx.RxNorm.pas',
  FHIR.Scim.Search in '..\..\Server\FHIR.Scim.Search.pas',
  FHIR.Scim.Server in '..\..\Server\FHIR.Scim.Server.pas',
  FHIR.Server.Adaptations in '..\..\Server\FHIR.Server.Adaptations.pas',
  FHIR.Tx.Server in '..\..\Server\FHIR.Tx.Server.pas',
  FHIR.Tx.Manager in '..\..\Server\FHIR.Tx.Manager.pas',
  FHIR.Tx.Web in '..\..\Server\FHIR.Tx.Web.pas',
  FHIR.Tx.Unii in '..\..\Server\FHIR.Tx.Unii.pas',
  FHIR.Tx.Uri in '..\..\Server\FHIR.Tx.Uri.pas',
  FHIR.Server.UserMgr in '..\..\Server\FHIR.Server.UserMgr.pas',
  FastMM4Messages in '..\..\dependencies\FMM\FastMM4Messages.pas',
  FHIR.Tools.GraphQL in '..\..\library\tools\FHIR.Tools.GraphQL.pas',
  FHIR.Support.MXml in '..\..\library\support\FHIR.Support.MXml.pas',
  MarkdownCommonMark in '..\..\..\markdown\source\MarkdownCommonMark.pas',
  FHIR.CdsHooks.Server in '..\..\Server\FHIR.CdsHooks.Server.pas',
  FHIR.Support.Turtle in '..\..\library\support\FHIR.Support.Turtle.pas',
  FHIR.Tools.ApplicationVerifier in '..\..\library\tools\FHIR.Tools.ApplicationVerifier.pas',
  FHIR.Server.Jwt in '..\..\Server\FHIR.Server.Jwt.pas',
  FHIR.Support.SCrypt in '..\..\library\support\FHIR.Support.SCrypt.pas',
  FHIR.CdsHooks.Client in '..\..\library\cdshooks\FHIR.CdsHooks.Client.pas',
  FHIR.Server.WebSource in '..\..\Server\FHIR.Server.WebSource.pas',
  FHIR.Tools.Indexing in '..\..\library\tools\FHIR.Tools.Indexing.pas',
  FHIR.Tx.ICD10 in '..\..\Server\FHIR.Tx.ICD10.pas',
  FHIR.Server.Javascript in '..\..\Server\FHIR.Server.Javascript.pas',
  FHIR.R3.Javascript in '..\..\library\r3\FHIR.R3.Javascript.pas',
  FHIR.Javascript.Base in '..\..\library\javascript\FHIR.Javascript.Base.pas',
  FHIR.Client.Javascript in '..\..\library\client\FHIR.Client.Javascript.pas',
  FHIR.Server.EventJs in '..\..\Server\FHIR.Server.EventJs.pas',
  FHIR.Javascript in '..\..\library\javascript\FHIR.Javascript.pas',
  FHIR.Support.Javascript in '..\..\library\support\FHIR.Support.Javascript.pas',
  FHIR.Support.Signatures in '..\..\library\support\FHIR.Support.Signatures.pas',
  FHIR.R3.Factory in '..\..\library\r3\FHIR.R3.Factory.pas',
  FHIR.Tx.CountryCode in '..\..\Server\FHIR.Tx.CountryCode.pas',
  FHIR.Tx.UsState in '..\..\Server\FHIR.Tx.UsState.pas',
  FHIR.R3.PathNode in '..\..\library\r3\FHIR.R3.PathNode.pas',
  FHIR.Server.Analytics in '..\..\Server\FHIR.Server.Analytics.pas',
  FHIR.Ucum.IFace in '..\..\library\ucum\FHIR.Ucum.IFace.pas',
  FHIR.Server.XhtmlComp in '..\..\Server\FHIR.Server.XhtmlComp.pas',
  FHIR.R3.ParserBase in '..\..\library\r3\FHIR.R3.ParserBase.pas',
  FHIR.R3.Base in '..\..\library\r3\FHIR.R3.Base.pas',
  FHIR.R3.Parser in '..\..\library\r3\FHIR.R3.Parser.pas',
  FHIR.Client.Base in '..\..\library\client\FHIR.Client.Base.pas',
  FHIR.Client.HTTP in '..\..\library\client\FHIR.Client.HTTP.pas',
  FHIR.Client.Threaded in '..\..\library\client\FHIR.Client.Threaded.pas',
  FHIR.R3.Client in '..\..\library\r3\FHIR.R3.Client.pas',
  FHIR.Support.Xml in '..\..\library\support\FHIR.Support.Xml.pas',
  FHIR.Support.Certs in '..\..\library\support\FHIR.Support.Certs.pas',
  FHIR.Web.GraphQL in '..\..\library\web\FHIR.Web.GraphQL.pas',
  FHIR.Support.MsXml in '..\..\library\support\FHIR.Support.MsXml.pas',
  FHIR.Base.Factory in '..\..\library\base\FHIR.Base.Factory.pas',
  FHIR.Base.Validator in '..\..\library\base\FHIR.Base.Validator.pas',
  FHIR.Base.Common in '..\..\library\base\FHIR.Base.Common.pas',
  FHIR.Base.Narrative in '..\..\library\base\FHIR.Base.Narrative.pas',
  FHIR.Base.PathEngine in '..\..\library\base\FHIR.Base.PathEngine.pas',
  FHIR.R3.Common in '..\..\library\r3\FHIR.R3.Common.pas',
  FHIR.R3.Narrative in '..\..\library\r3\FHIR.R3.Narrative.pas',
  FHIR.Base.Utilities in '..\..\library\base\FHIR.Base.Utilities.pas',
  fhir.support.fpc in '..\..\library\support\fhir.support.fpc.pas',
  FHIR.Support.Osx in '..\..\library\support\FHIR.Support.Osx.pas',
  FHIR.Tx.MimeTypes in '..\..\Server\FHIR.Tx.MimeTypes.pas',
  FHIR.Tx.Iso4217 in '..\..\Server\FHIR.Tx.Iso4217.pas',
  FHIR.Server.Tags in '..\..\Server\FHIR.Server.Tags.pas',
  FHIR.Server.BundleBuilder in '..\..\Server\FHIR.Server.BundleBuilder.pas',
  FHIR.Tools.ValueSets in '..\..\library\tools\FHIR.Tools.ValueSets.pas',
  FHIR.Tools.CodeSystemProvider in '..\..\library\tools\FHIR.Tools.CodeSystemProvider.pas',
  FHIR.Server.Factory in '..\..\Server\FHIR.Server.Factory.pas',
  FHIR.Tools.NDJsonParser in '..\..\library\tools\FHIR.Tools.NDJsonParser.pas',
  FHIR.Server.Utilities in '..\..\Server\FHIR.Server.Utilities.pas',
  FHIR.Support.Lang in '..\..\library\support\FHIR.Support.Lang.pas',
  FHIR.Base.OIDs in '..\..\library\base\FHIR.Base.OIDs.pas',
  FHIR.Base.ElementModel in '..\..\library\base\FHIR.Base.ElementModel.pas',
  Compat in '..\..\dependencies\chakracore-delphi\Compat.pas',
  ChakraCommon in '..\..\dependencies\chakracore-delphi\ChakraCommon.pas',
  ChakraCore in '..\..\dependencies\chakracore-delphi\ChakraCore.pas',
  ChakraCoreClasses in '..\..\dependencies\chakracore-delphi\ChakraCoreClasses.pas',
  ChakraCoreUtils in '..\..\dependencies\chakracore-delphi\ChakraCoreUtils.pas',
  ChakraDebug in '..\..\dependencies\chakracore-delphi\ChakraDebug.pas',
  MarkdownHTMLEntities in '..\..\..\markdown\source\MarkdownHTMLEntities.pas',
  FHIR.Server.ConsentEngine in '..\..\Server\FHIR.Server.ConsentEngine.pas',
  FHIR.Tx.NDC in '..\..\Server\FHIR.Tx.NDC.pas',
  FHIR.Server.UsageStats in '..\..\Server\FHIR.Server.UsageStats.pas',
  FHIR.Cache.PackageManager in '..\..\library\cache\FHIR.Cache.PackageManager.pas',
  FHIR.Cache.NpmPackage in '..\..\library\cache\FHIR.Cache.NpmPackage.pas';

var
  gsrv : TExampleFhirServer;
begin
  try
    gsrv := TExampleFhirServer.create;
    try
      gsrv.Port := 971;
      gsrv.SystemName := 'Example Bridge Server';
      gsrv.SystemUID := 'urn:uuid:f374f5fa-5ba2-4bbf-8c44-ce026b9f9772';
      gsrv.Start;
      try
        writeln('FHIR Server running on port 971');
        writeln('Press enter to stop');
        readln;
      finally
        gsrv.stop;
      end;
    finally
      gsrv.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
