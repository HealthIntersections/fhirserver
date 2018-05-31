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
  FastMM4 in '..\Libraries\FMM\FastMM4.pas',
  System.SysUtils,
  ExampleBridge in 'ExampleBridge.pas',
  FHIR.Support.Collections in '..\reference-platform\support\FHIR.Support.Collections.pas',
  FHIR.Support.Exceptions in '..\reference-platform\support\FHIR.Support.Exceptions.pas',
  FHIR.Support.Generics in '..\reference-platform\support\FHIR.Support.Generics.pas',
  FHIR.Support.Json in '..\reference-platform\support\FHIR.Support.Json.pas',
  FHIR.Support.Objects in '..\reference-platform\support\FHIR.Support.Objects.pas',
  FHIR.Support.Stream in '..\reference-platform\support\FHIR.Support.Stream.pas',
  FHIR.Support.WinInet in '..\reference-platform\support\FHIR.Support.WinInet.pas',
  FHIR.Support.Binary in '..\reference-platform\support\FHIR.Support.Binary.pas',
  FHIR.Support.DateTime in '..\reference-platform\support\FHIR.Support.DateTime.pas',
  FHIR.Support.Decimal in '..\reference-platform\support\FHIR.Support.Decimal.pas',
  FHIR.Base.Objects in '..\reference-platform\base\FHIR.Base.Objects.pas',
  FHIR.Misc.Facebook in '..\reference-platform\support\FHIR.Misc.Facebook.pas',
  FHIR.Support.System in '..\reference-platform\support\FHIR.Support.System.pas',
  FHIR.Web.HtmlGen in '..\reference-platform\support\FHIR.Web.HtmlGen.pas',
  FHIR.Web.Socket in '..\reference-platform\support\FHIR.Web.Socket.pas',
  FHIR.Web.Fetcher in '..\reference-platform\support\FHIR.Web.Fetcher.pas',
  FHIR.Database.Dialects in '..\reference-platform\support\FHIR.Database.Dialects.pas',
  FHIR.Support.Math in '..\reference-platform\support\FHIR.Support.Math.pas',
  FHIR.Support.Mime in '..\reference-platform\support\FHIR.Support.Mime.pas',
  FHIR.Web.Parsers in '..\reference-platform\support\FHIR.Web.Parsers.pas',
  FHIR.Web.Rdf in '..\reference-platform\support\FHIR.Web.Rdf.pas',
  FHIR.Support.Shell in '..\reference-platform\support\FHIR.Support.Shell.pas',
  FHIR.Support.Strings in '..\reference-platform\support\FHIR.Support.Strings.pas',
  FHIR.Support.Service in '..\reference-platform\support\FHIR.Support.Service.pas',
  FHIR.Support.Lock in '..\reference-platform\support\FHIR.Support.Lock.pas',
  MarkdownDaringFireball in '..\..\markdown\source\MarkdownDaringFireball.pas',
  MarkdownProcessor in '..\..\markdown\source\MarkdownProcessor.pas',
  FHIR.CdsHooks.Utilities in '..\reference-platform\support\FHIR.CdsHooks.Utilities.pas',
  FHIR.R3.AuthMap in '..\reference-platform\dstu3\FHIR.R3.AuthMap.pas',
  FHIR.Version.Client in '..\reference-platform\version\FHIR.Version.Client.pas',
  FHIR.R3.Constants in '..\reference-platform\dstu3\FHIR.R3.Constants.pas',
  FHIR.R3.Context in '..\reference-platform\dstu3\FHIR.R3.Context.pas',
  FHIR.R3.IndexInfo in '..\reference-platform\dstu3\FHIR.R3.IndexInfo.pas',
  FHIR.Base.Lang in '..\reference-platform\base\FHIR.Base.Lang.pas',
  FHIR.R3.ElementModel in '..\reference-platform\dstu3\FHIR.R3.ElementModel.pas',
  FHIR.R3.Operations in '..\reference-platform\dstu3\FHIR.R3.Operations.pas',
  FHIR.Version.Parser in '..\reference-platform\version\FHIR.Version.Parser.pas',
  FHIR.R3.Xml in '..\reference-platform\dstu3\FHIR.R3.Xml.pas',
  FHIR.R3.Json in '..\reference-platform\dstu3\FHIR.R3.Json.pas',
  FHIR.R3.Turtle in '..\reference-platform\dstu3\FHIR.R3.Turtle.pas',
  FHIR.Base.Parser in '..\reference-platform\base\FHIR.Base.Parser.pas',
  FHIR.R3.Profiles in '..\reference-platform\dstu3\FHIR.R3.Profiles.pas',
  FHIR.R3.Resources in '..\reference-platform\dstu3\FHIR.R3.Resources.pas',
  FHIR.Server.Security in '..\server\FHIR.Server.Security.pas',
  FHIR.Server.Session in '..\server\FHIR.Server.Session.pas',
  FHIR.R3.Tags in '..\reference-platform\dstu3\FHIR.R3.Tags.pas',
  FHIR.R3.Types in '..\reference-platform\dstu3\FHIR.R3.Types.pas',
  FHIR.R3.Utilities in '..\reference-platform\dstu3\FHIR.R3.Utilities.pas',
  FHIR.R3.Validator in '..\reference-platform\dstu3\FHIR.R3.Validator.pas',
  FHIR.Base.Xhtml in '..\reference-platform\base\FHIR.Base.Xhtml.pas',
  FHIR.R3.OpBase in '..\reference-platform\dstu3\FHIR.R3.OpBase.pas',
  FHIR.R3.PathEngine in '..\reference-platform\dstu3\FHIR.R3.PathEngine.pas',
  FHIR.R3.Narrative2 in '..\reference-platform\dstu3\FHIR.R3.Narrative2.pas',
  FHIR.R3.Questionnaire in '..\reference-platform\dstu3\FHIR.R3.Questionnaire.pas',
  FHIR.Base.Scim in '..\reference-platform\base\FHIR.Base.Scim.pas',
  FHIR.Smart.Utilities in '..\reference-platform\client\FHIR.Smart.Utilities.pas',
  FHIR.Tx.Service in '..\Libraries\FHIR.Tx.Service.pas',
  FHIR.Database.Logging in '..\Libraries\db\FHIR.Database.Logging.pas',
  FHIR.Database.Manager in '..\Libraries\db\FHIR.Database.Manager.pas',
  FHIR.Database.Settings in '..\Libraries\db\FHIR.Database.Settings.pas',
  FHIR.Loinc.Publisher in '..\Libraries\loinc\FHIR.Loinc.Publisher.pas',
  FHIR.Loinc.Services in '..\Libraries\loinc\FHIR.Loinc.Services.pas',
  FHIR.Misc.Twilio in '..\Libraries\security\FHIR.Misc.Twilio.pas',
  FHIR.Snomed.Analysis in '..\Libraries\snomed\FHIR.Snomed.Analysis.pas',
  FHIR.Snomed.Expressions in '..\Libraries\snomed\FHIR.Snomed.Expressions.pas',
  FHIR.Snomed.Publisher in '..\Libraries\snomed\FHIR.Snomed.Publisher.pas',
  FHIR.Snomed.Services in '..\Libraries\snomed\FHIR.Snomed.Services.pas',
  DISystemCompat in '..\Libraries\stem\DISystemCompat.pas',
  YuStemmer in '..\Libraries\stem\YuStemmer.pas',
  FHIR.Ucum.Base in '..\Libraries\Ucum\FHIR.Ucum.Base.pas',
  FHIR.Ucum.Expressions in '..\Libraries\Ucum\FHIR.Ucum.Expressions.pas',
  FHIR.Ucum.Handlers in '..\Libraries\Ucum\FHIR.Ucum.Handlers.pas',
  FHIR.Ucum.Search in '..\Libraries\Ucum\FHIR.Ucum.Search.pas',
  FHIR.Ucum.Services in '..\Libraries\Ucum\FHIR.Ucum.Services.pas',
  FHIR.Ucum.Validators in '..\Libraries\Ucum\FHIR.Ucum.Validators.pas',
  FHIR.Tx.ACIR in '..\Server\FHIR.Tx.ACIR.pas',
  FHIR.Tx.AreaCode in '..\Server\FHIR.Tx.AreaCode.pas',
  FHIR.Server.AuthMgr in '..\Server\FHIR.Server.AuthMgr.pas',
  FHIR.Server.ClosureMgr in '..\Server\FHIR.Server.ClosureMgr.pas',
  FHIR.Server.Indexing in '..\Server\FHIR.Server.Indexing.pas',
  FHIR.Debug.Logging in '..\reference-platform\support\FHIR.Debug.Logging.pas',
  FHIR.Server.Web in '..\Server\FHIR.Server.Web.pas',
  FHIR.Server.Constants in '..\Server\FHIR.Server.Constants.pas',
  FHIR.Server.Context in '..\Server\FHIR.Server.Context.pas',
  FHIR.Server.Utilities in '..\Server\FHIR.Server.Utilities.pas',
  FHIR.Server.SessionMgr in '..\Server\FHIR.Server.SessionMgr.pas',
  FHIR.Server.Storage in '..\Server\FHIR.Server.Storage.pas',
  FHIR.Server.Subscriptions in '..\Server\FHIR.Server.Subscriptions.pas',
  FHIR.Server.TagMgr in '..\Server\FHIR.Server.TagMgr.pas',
  FHIR.Tx.Validator in '..\Server\FHIR.Tx.Validator.pas',
  FHIR.Tx.Expander in '..\Server\FHIR.Tx.Expander.pas',
  FHIR.Tx.Lang in '..\Server\FHIR.Tx.Lang.pas',
  FHIR.Server.OpenMHealth in '..\Server\FHIR.Server.OpenMHealth.pas',
  FHIR.Server.ReverseClient in '..\Server\FHIR.Server.ReverseClient.pas',
  FHIR.Tx.RxNorm in '..\Server\FHIR.Tx.RxNorm.pas',
  FHIR.Scim.Search in '..\Server\FHIR.Scim.Search.pas',
  FHIR.Scim.Server in '..\Server\FHIR.Scim.Server.pas',
  FHIR.Server.Adaptations in '..\Server\FHIR.Server.Adaptations.pas',
  FHIR.Server.Validator in '..\Server\FHIR.Server.Validator.pas',
  FHIR.Tx.Server in '..\Server\FHIR.Tx.Server.pas',
  FHIR.Tx.Manager in '..\Server\FHIR.Tx.Manager.pas',
  FHIR.Tx.Web in '..\Server\FHIR.Tx.Web.pas',
  FHIR.Tx.Unii in '..\Server\FHIR.Tx.Unii.pas',
  FHIR.Tx.Uri in '..\Server\FHIR.Tx.Uri.pas',
  FHIR.Server.UserMgr in '..\Server\FHIR.Server.UserMgr.pas',
  FastMM4Messages in '..\Libraries\FMM\FastMM4Messages.pas',
  FHIR.Tools.GraphQL in '..\reference-platform\tools\FHIR.Tools.GraphQL.pas',
  FHIR.Support.MXml in '..\reference-platform\support\FHIR.Support.MXml.pas',
  MarkdownCommonMark in '..\..\markdown\source\MarkdownCommonMark.pas',
  FHIR.CdsHooks.Server in '..\Server\FHIR.CdsHooks.Server.pas',
  FHIR.Support.Turtle in '..\reference-platform\support\FHIR.Support.Turtle.pas',
  FHIR.Misc.ApplicationVerifier in '..\Libraries\security\FHIR.Misc.ApplicationVerifier.pas',
  FHIR.Server.Jwt in '..\Server\FHIR.Server.Jwt.pas',
  FHIR.Utilities.SCrypt in '..\Libraries\security\FHIR.Utilities.SCrypt.pas',
  FHIR.Server.AppCache in '..\Server\FHIR.Server.AppCache.pas',
  FHIR.CdsHooks.Client in '..\reference-platform\support\FHIR.CdsHooks.Client.pas',
  FHIR.Server.WebSource in '..\Server\FHIR.Server.WebSource.pas',
  FHIR.Tools.Indexing in '..\reference-platform\tools\FHIR.Tools.Indexing.pas',
  FHIR.Tx.ICD10 in '..\Server\FHIR.Tx.ICD10.pas',
  FHIR.Server.PostHandlers in '..\Server\FHIR.Server.PostHandlers.pas',
  FHIR.Server.Javascript in '..\Server\FHIR.Server.Javascript.pas',
  FHIR.R3.Javascript in '..\reference-platform\dstu3\FHIR.R3.Javascript.pas',
  FHIR.Javascript.Base in '..\Libraries\js\FHIR.Javascript.Base.pas',
  FHIR.Client.Javascript in '..\Libraries\js\FHIR.Client.Javascript.pas',
  FHIR.Server.EventJs in '..\Server\FHIR.Server.EventJs.pas',
  FHIR.Javascript in '..\Libraries\js\FHIR.Javascript.pas',
  FHIR.Javascript.Chakra in '..\Libraries\js\FHIR.Javascript.Chakra.pas',
  FHIR.Support.Javascript in '..\Libraries\js\FHIR.Support.Javascript.pas',
  FHIR.Support.Signatures in '..\reference-platform\support\FHIR.Support.Signatures.pas',
  FHIR.R3.Factory in '..\reference-platform\dstu3\FHIR.R3.Factory.pas',
  FHIR.Tx.CountryCode in '..\Server\FHIR.Tx.CountryCode.pas',
  FHIR.Tx.UsState in '..\Server\FHIR.Tx.UsState.pas',
  FHIR.R3.PathNode in '..\reference-platform\dstu3\FHIR.R3.PathNode.pas',
  FHIR.Server.Analytics in '..\Server\FHIR.Server.Analytics.pas',
  FHIR.Ucum.IFace in '..\reference-platform\support\FHIR.Ucum.IFace.pas',
  FHIR.Server.XhtmlComp in '..\Server\FHIR.Server.XhtmlComp.pas',
  FHIR.R3.ParserBase in '..\reference-platform\dstu3\FHIR.R3.ParserBase.pas',
  FHIR.R3.Base in '..\reference-platform\dstu3\FHIR.R3.Base.pas',
  FHIR.R3.Parser in '..\reference-platform\dstu3\FHIR.R3.Parser.pas',
  FHIR.Client.Base in '..\reference-platform\client\FHIR.Client.Base.pas',
  FHIR.Client.HTTP in '..\reference-platform\client\FHIR.Client.HTTP.pas',
  FHIR.Client.Threaded in '..\reference-platform\client\FHIR.Client.Threaded.pas',
  FHIR.R3.Client in '..\reference-platform\dstu3\FHIR.R3.Client.pas',
  FHIR.Support.Text in '..\reference-platform\support\FHIR.Support.Text.pas',
  FHIR.Support.Xml in '..\reference-platform\support\FHIR.Support.Xml.pas',
  FHIR.Support.Zip in '..\reference-platform\support\FHIR.Support.Zip.pas',
  FHIR.Support.Controllers in '..\reference-platform\support\FHIR.Support.Controllers.pas',
  FHIR.Support.Certs in '..\reference-platform\support\FHIR.Support.Certs.pas',
  FHIR.Misc.GraphQL in '..\reference-platform\support\FHIR.Misc.GraphQL.pas',
  FHIR.Support.MsXml in '..\reference-platform\support\FHIR.Support.MsXml.pas',
  FHIR.Base.Factory in '..\reference-platform\base\FHIR.Base.Factory.pas',
  FHIR.Base.Validator in '..\reference-platform\base\FHIR.Base.Validator.pas',
  FHIR.Base.Common in '..\reference-platform\base\FHIR.Base.Common.pas',
  FHIR.Base.Narrative in '..\reference-platform\base\FHIR.Base.Narrative.pas',
  FHIR.Base.PathEngine in '..\reference-platform\base\FHIR.Base.PathEngine.pas',
  FHIR.R3.Common in '..\reference-platform\dstu3\FHIR.R3.Common.pas',
  FHIR.R3.Narrative in '..\reference-platform\dstu3\FHIR.R3.Narrative.pas',
  FHIR.Base.Utilities in '..\reference-platform\base\FHIR.Base.Utilities.pas',
  fhir.support.fpc in '..\reference-platform\support\fhir.support.fpc.pas',
  FHIR.Support.Osx in '..\reference-platform\support\FHIR.Support.Osx.pas';

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
