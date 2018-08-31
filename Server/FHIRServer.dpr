// JCL_DEBUG_EXPERT_INSERTJDBG ON
program FHIRServer;

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


{
todo:
STU3 draft:
 - check using UTF-8 in URLs
 - check prepfer = operation outcome
 - check turtle support
 - check versioning in capabilities statement
 - check elements & summary on read/vread
 - check delete return codes
 - changes to id element on update
 - check conditional delete flag.
 - search on multiple resource types
 - check over transaction handling
 - dateTIme parameter on history
 - check use of 401 instead of 403
 - check handling of unsupported parameters
 - check implementations of sa and be prefixes in search
 - review searching on names
 - check search by canonical URL and version
 - add searching for token system|
 - reverse chaining
 - check _list parameter
 - check sorting
 - check handling count = 0
 - composite search parameter work
 - test flag support
 - check document operation
 - check handling binary parameter
 - update FHIR.Version.PathEngine
 - string validation rules
 - Both _include and _revInclude use the wild card "*" for the search parameter name, indicating that any search parameter of type=reference be included.






Add reverse chaining
Grahame, I see you don't respond to either of the following:
 http://hl7connect.healthintersections.com.au/svc/fhir/condition/search?subject=patient/350
 http://hl7connect.healthintersections.com.au/svc/fhir/condition/search?subject=patient/@350
cross resource search
FHIR.Ucum.Base search

}

uses
  FastMM4 in '..\Libraries\FMM\FastMM4.pas',
  FastMM4Messages in '..\Libraries\FMM\FastMM4Messages.pas',
  Windows,
  SysUtils,
  Classes,
  IdSSLOpenSSLHeaders,
  JclDebug,
  YuStemmer in '..\Libraries\Stem\YuStemmer.pas',
  DISystemCompat in '..\Libraries\Stem\DISystemCompat.pas',
  MarkdownCommonMark in '..\..\markdown\source\MarkdownCommonMark.pas',
  MarkdownDaringFireball in '..\..\markdown\source\MarkdownDaringFireball.pas',
  MarkdownProcessor in '..\..\markdown\source\MarkdownProcessor.pas',
  FHIR.Base.Common in '..\reference-platform\base\FHIR.Base.Common.pas',
  FHIR.Base.Factory in '..\reference-platform\base\FHIR.Base.Factory.pas',
  FHIR.Base.Lang in '..\reference-platform\base\FHIR.Base.Lang.pas',
  FHIR.Base.Narrative in '..\reference-platform\base\FHIR.Base.Narrative.pas',
  FHIR.Base.Objects in '..\reference-platform\base\FHIR.Base.Objects.pas',
  FHIR.Base.Parser in '..\reference-platform\base\FHIR.Base.Parser.pas',
  FHIR.Base.PathEngine in '..\reference-platform\base\FHIR.Base.PathEngine.pas',
  FHIR.Base.Scim in '..\reference-platform\base\FHIR.Base.Scim.pas',
  FHIR.Base.Utilities in '..\reference-platform\base\FHIR.Base.Utilities.pas',
  FHIR.Base.Validator in '..\reference-platform\base\FHIR.Base.Validator.pas',
  FHIR.Base.Xhtml in '..\reference-platform\base\FHIR.Base.Xhtml.pas',
  FHIR.Cache.PackageManager in '..\reference-platform\cache\FHIR.Cache.PackageManager.pas',
  FHIR.CdsHooks.Client in '..\reference-platform\support\FHIR.CdsHooks.Client.pas',
  FHIR.CdsHooks.Server in 'FHIR.CdsHooks.Server.pas',
  FHIR.CdsHooks.Service in 'FHIR.CdsHooks.Service.pas',
  FHIR.CdsHooks.Utilities in '..\reference-platform\support\FHIR.CdsHooks.Utilities.pas',
  FHIR.Client.Base in '..\reference-platform\client\FHIR.Client.Base.pas',
  FHIR.Client.HTTP in '..\reference-platform\client\FHIR.Client.HTTP.pas',
  FHIR.Client.Javascript in '..\Libraries\js\FHIR.Client.Javascript.pas',
  FHIR.Client.Threaded in '..\reference-platform\client\FHIR.Client.Threaded.pas',
  FHIR.Database.Dialects in '..\Libraries\db\FHIR.Database.Dialects.pas',
  FHIR.Database.Logging in '..\Libraries\db\FHIR.Database.Logging.pas',
  FHIR.Database.Manager in '..\Libraries\db\FHIR.Database.Manager.pas',
  FHIR.Database.ODBC in '..\Libraries\db\FHIR.Database.ODBC.pas',
  FHIR.Database.ODBC.Headers in '..\Libraries\db\FHIR.Database.ODBC.Headers.pas',
  FHIR.Database.ODBC.Objects in '..\Libraries\db\FHIR.Database.ODBC.Objects.pas',
  FHIR.Database.SQLite in '..\Libraries\db\FHIR.Database.SQLite.pas',
  FHIR.Database.SQLite3.Objects in '..\Libraries\db\FHIR.Database.SQLite3.Objects.pas',
  FHIR.Database.SQLite3.Utilities in '..\Libraries\db\FHIR.Database.SQLite3.Utilities.pas',
  FHIR.Database.SQLite3.Wrapper in '..\Libraries\db\FHIR.Database.SQLite3.Wrapper.pas',
  FHIR.Database.Settings in '..\Libraries\db\FHIR.Database.Settings.pas',
  FHIR.Database.Utilities in '..\Libraries\db\FHIR.Database.Utilities.pas',
  FHIR.Javascript in '..\Libraries\js\FHIR.Javascript.pas',
  FHIR.Javascript.Base in '..\Libraries\js\FHIR.Javascript.Base.pas',
  FHIR.Javascript.Chakra in '..\Libraries\js\FHIR.Javascript.Chakra.pas',
  FHIR.Loinc.Importer in '..\Libraries\loinc\FHIR.Loinc.Importer.pas',
  FHIR.Loinc.Publisher in '..\Libraries\loinc\FHIR.Loinc.Publisher.pas',
  FHIR.Loinc.Services in '..\Libraries\loinc\FHIR.Loinc.Services.pas',
  FHIR.Misc.Twilio in '..\Libraries\security\FHIR.Misc.Twilio.pas',
  FHIR.R2.Base in '..\reference-platform\dstu2\FHIR.R2.Base.pas',
  FHIR.R2.Client in '..\reference-platform\dstu2\FHIR.R2.Client.pas',
  FHIR.R2.Common in '..\reference-platform\dstu2\FHIR.R2.Common.pas',
  FHIR.R2.Constants in '..\reference-platform\dstu2\FHIR.R2.Constants.pas',
  FHIR.R2.Context in '..\reference-platform\dstu2\FHIR.R2.Context.pas',
  FHIR.R2.ElementModel in '..\reference-platform\dstu2\FHIR.R2.ElementModel.pas',
  FHIR.R2.Factory in '..\reference-platform\dstu2\FHIR.R2.Factory.pas',
  FHIR.R2.Json in '..\reference-platform\dstu2\FHIR.R2.Json.pas',
  FHIR.R2.Narrative in '..\reference-platform\dstu2\FHIR.R2.Narrative.pas',
  FHIR.R2.OpBase in '..\reference-platform\dstu2\FHIR.R2.OpBase.pas',
  FHIR.R2.Operations in '..\reference-platform\dstu2\FHIR.R2.Operations.pas',
  FHIR.R2.Parser in '..\reference-platform\dstu2\FHIR.R2.Parser.pas',
  FHIR.R2.ParserBase in '..\reference-platform\dstu2\FHIR.R2.ParserBase.pas',
  FHIR.R2.PathEngine in '..\reference-platform\dstu2\FHIR.R2.PathEngine.pas',
  FHIR.R2.PathNode in '..\reference-platform\dstu2\FHIR.R2.PathNode.pas',
  FHIR.R2.Profiles in '..\reference-platform\dstu2\FHIR.R2.Profiles.pas',
  FHIR.R2.Resources in '..\reference-platform\dstu2\FHIR.R2.Resources.pas',
  FHIR.R2.Types in '..\reference-platform\dstu2\FHIR.R2.Types.pas',
  FHIR.R2.Utilities in '..\reference-platform\dstu2\FHIR.R2.Utilities.pas',
  FHIR.R2.Validator in '..\reference-platform\dstu2\FHIR.R2.Validator.pas',
  FHIR.R2.Xml in '..\reference-platform\dstu2\FHIR.R2.Xml.pas',
  FHIR.R3.Base in '..\reference-platform\dstu3\FHIR.R3.Base.pas',
  FHIR.R3.Client in '..\reference-platform\dstu3\FHIR.R3.Client.pas',
  FHIR.R3.Common in '..\reference-platform\dstu3\FHIR.R3.Common.pas',
  FHIR.R3.Constants in '..\reference-platform\dstu3\FHIR.R3.Constants.pas',
  FHIR.R3.Context in '..\reference-platform\dstu3\FHIR.R3.Context.pas',
  FHIR.R3.ElementModel in '..\reference-platform\dstu3\FHIR.R3.ElementModel.pas',
  FHIR.R3.Factory in '..\reference-platform\dstu3\FHIR.R3.Factory.pas',
  FHIR.R3.Json in '..\reference-platform\dstu3\FHIR.R3.Json.pas',
  FHIR.R3.Narrative in '..\reference-platform\dstu3\FHIR.R3.Narrative.pas',
  FHIR.R3.OpBase in '..\reference-platform\dstu3\FHIR.R3.OpBase.pas',
  FHIR.R3.Operations in '..\reference-platform\dstu3\FHIR.R3.Operations.pas',
  FHIR.R3.Parser in '..\reference-platform\dstu3\FHIR.R3.Parser.pas',
  FHIR.R3.ParserBase in '..\reference-platform\dstu3\FHIR.R3.ParserBase.pas',
  FHIR.R3.PathEngine in '..\reference-platform\dstu3\FHIR.R3.PathEngine.pas',
  FHIR.R3.PathNode in '..\reference-platform\dstu3\FHIR.R3.PathNode.pas',
  FHIR.R3.Profiles in '..\reference-platform\dstu3\FHIR.R3.Profiles.pas',
  FHIR.R3.Resources in '..\reference-platform\dstu3\FHIR.R3.Resources.pas',
  FHIR.R3.Turtle in '..\reference-platform\dstu3\FHIR.R3.Turtle.pas',
  FHIR.R3.Types in '..\reference-platform\dstu3\FHIR.R3.Types.pas',
  FHIR.R3.Utilities in '..\reference-platform\dstu3\FHIR.R3.Utilities.pas',
  FHIR.R3.Validator in '..\reference-platform\dstu3\FHIR.R3.Validator.pas',
  FHIR.R3.Xml in '..\reference-platform\dstu3\FHIR.R3.Xml.pas',
  FHIR.R4.AuthMap in '..\reference-platform\r4\FHIR.R4.AuthMap.pas',
  FHIR.R4.Base in '..\reference-platform\r4\FHIR.R4.Base.pas',
  FHIR.R4.Client in '..\reference-platform\r4\FHIR.R4.Client.pas',
  FHIR.R4.Common in '..\reference-platform\r4\FHIR.R4.Common.pas',
  FHIR.R4.Constants in '..\reference-platform\r4\FHIR.R4.Constants.pas',
  FHIR.R4.Context in '..\reference-platform\r4\FHIR.R4.Context.pas',
  FHIR.R4.ElementModel in '..\reference-platform\r4\FHIR.R4.ElementModel.pas',
  FHIR.R4.Factory in '..\reference-platform\r4\FHIR.R4.Factory.pas',
  FHIR.R4.IndexInfo in '..\reference-platform\r4\FHIR.R4.IndexInfo.pas',
  FHIR.R4.Javascript in '..\reference-platform\r4\FHIR.R4.Javascript.pas',
  FHIR.R4.Json in '..\reference-platform\r4\FHIR.R4.Json.pas',
  FHIR.R4.MapUtilities in '..\reference-platform\r4\FHIR.R4.MapUtilities.pas',
  FHIR.R4.Narrative in '..\reference-platform\r4\FHIR.R4.Narrative.pas',
  FHIR.R4.Narrative2 in '..\reference-platform\r4\FHIR.R4.Narrative2.pas',
  FHIR.R4.OpBase in '..\reference-platform\r4\FHIR.R4.OpBase.pas',
  FHIR.R4.Operations in '..\reference-platform\r4\FHIR.R4.Operations.pas',
  FHIR.R4.Parser in '..\reference-platform\r4\FHIR.R4.Parser.pas',
  FHIR.R4.ParserBase in '..\reference-platform\r4\FHIR.R4.ParserBase.pas',
  FHIR.R4.PathEngine in '..\reference-platform\r4\FHIR.R4.PathEngine.pas',
  FHIR.R4.PathNode in '..\reference-platform\r4\FHIR.R4.PathNode.pas',
  FHIR.R4.Profiles in '..\reference-platform\r4\FHIR.R4.Profiles.pas',
  FHIR.R4.Questionnaire in '..\reference-platform\r4\FHIR.R4.Questionnaire.pas',
  FHIR.R4.Resources in '..\reference-platform\r4\FHIR.R4.Resources.pas',
  FHIR.R4.Turtle in '..\reference-platform\r4\FHIR.R4.Turtle.pas',
  FHIR.R4.Types in '..\reference-platform\r4\FHIR.R4.Types.pas',
  FHIR.R4.Utilities in '..\reference-platform\r4\FHIR.R4.Utilities.pas',
  FHIR.R4.Validator in '..\reference-platform\r4\FHIR.R4.Validator.pas',
  FHIR.R4.Xml in '..\reference-platform\r4\FHIR.R4.Xml.pas',
  FHIR.Scim.Search in 'FHIR.Scim.Search.pas',
  FHIR.Scim.Server in 'FHIR.Scim.Server.pas',
  FHIR.Server.AccessControl in 'FHIR.Server.AccessControl.pas',
  FHIR.Server.AuthMgr in 'FHIR.Server.AuthMgr.pas',
  FHIR.Server.ClosureMgr in 'FHIR.Server.ClosureMgr.pas',
  FHIR.Server.Constants in 'FHIR.Server.Constants.pas',
  FHIR.Server.DBInstaller in 'FHIR.Server.DBInstaller.pas',
  FHIR.Server.Database in 'FHIR.Server.Database.pas',
  FHIR.Server.EventJs in 'FHIR.Server.EventJs.pas',
  FHIR.Server.GraphDefinition in 'FHIR.Server.GraphDefinition.pas',
  FHIR.Server.Javascript in 'FHIR.Server.Javascript.pas',
  FHIR.Server.Jwt in 'FHIR.Server.Jwt.pas',
  FHIR.Server.Kernel in 'FHIR.Server.Kernel.pas',
  FHIR.Server.MpiSearch in 'FHIR.Server.MpiSearch.pas',
  FHIR.Server.ObsStats in 'FHIR.Server.ObsStats.pas',
  FHIR.Server.ReverseClient in 'FHIR.Server.ReverseClient.pas',
  FHIR.Server.Search in 'FHIR.Server.Search.pas',
  FHIR.Server.SearchSyntax in 'FHIR.Server.SearchSyntax.pas',
  FHIR.Server.Security in '..\reference-platform\tools\FHIR.Server.Security.pas',
  FHIR.Server.Session in '..\reference-platform\tools\FHIR.Server.Session.pas',
  FHIR.Server.Storage in 'FHIR.Server.Storage.pas',
  FHIR.Server.Subscriptions in 'FHIR.Server.Subscriptions.pas',
  FHIR.Server.UserMgr in 'FHIR.Server.UserMgr.pas',
  FHIR.Server.Utilities in 'FHIR.Server.Utilities.pas',
  FHIR.Server.Web in 'FHIR.Server.Web.pas',
  FHIR.Server.WebSource in 'FHIR.Server.WebSource.pas',
  FHIR.Server.XhtmlComp in 'FHIR.Server.XhtmlComp.pas',
  FHIR.Smart.Utilities in '..\reference-platform\client\FHIR.Smart.Utilities.pas',
  FHIR.Snomed.Analysis in '..\Libraries\snomed\FHIR.Snomed.Analysis.pas',
  FHIR.Snomed.Expressions in '..\Libraries\snomed\FHIR.Snomed.Expressions.pas',
  FHIR.Snomed.Importer in '..\Libraries\snomed\FHIR.Snomed.Importer.pas',
  FHIR.Snomed.Publisher in '..\Libraries\snomed\FHIR.Snomed.Publisher.pas',
  FHIR.Snomed.Services in '..\Libraries\Snomed\FHIR.Snomed.Services.pas',
  FHIR.Support.Base in '..\reference-platform\Support\FHIR.Support.Base.pas',
  FHIR.Support.Certs in '..\reference-platform\support\FHIR.Support.Certs.pas',
  FHIR.Support.Collections in '..\reference-platform\Support\FHIR.Support.Collections.pas',
  FHIR.Support.Fpc in '..\reference-platform\support\FHIR.Support.Fpc.pas',
  FHIR.Support.Javascript in '..\Libraries\js\FHIR.Support.Javascript.pas',
  FHIR.Support.Json in '..\reference-platform\Support\FHIR.Support.Json.pas',
  FHIR.Support.Logging in '..\reference-platform\support\FHIR.Support.Logging.pas',
  FHIR.Support.MXml in '..\reference-platform\support\FHIR.Support.MXml.pas',
  FHIR.Support.MsXml in '..\reference-platform\support\FHIR.Support.MsXml.pas',
  FHIR.Support.Osx in '..\reference-platform\support\FHIR.Support.Osx.pas',
  FHIR.Support.Service in '..\reference-platform\support\FHIR.Support.Service.pas',
  FHIR.Support.Shell in '..\reference-platform\Support\FHIR.Support.Shell.pas',
  FHIR.Support.Signatures in '..\reference-platform\Support\FHIR.Support.Signatures.pas',
  FHIR.Support.Stream in '..\reference-platform\Support\FHIR.Support.Stream.pas',
  FHIR.Support.Threads in '..\reference-platform\support\FHIR.Support.Threads.pas',
  FHIR.Support.Turtle in '..\reference-platform\support\FHIR.Support.Turtle.pas',
  FHIR.Support.Utilities in '..\reference-platform\Support\FHIR.Support.Utilities.pas',
  FHIR.Support.Xml in '..\reference-platform\support\FHIR.Support.Xml.pas',
  FHIR.Tools.CodeGen in '..\reference-platform\tools\FHIR.Tools.CodeGen.pas',
  FHIR.Tools.DiffEngine in '..\reference-platform\tools\FHIR.Tools.DiffEngine.pas',
  FHIR.Tools.GraphQL in '..\reference-platform\tools\FHIR.Tools.GraphQL.pas',
  FHIR.Tools.Indexing in '..\reference-platform\tools\FHIR.Tools.Indexing.pas',
  FHIR.Tx.ACIR in 'FHIR.Tx.ACIR.pas',
  FHIR.Tx.CountryCode in 'FHIR.Tx.CountryCode.pas',
  FHIR.Tx.Iso4217 in 'FHIR.Tx.Iso4217.pas',
  FHIR.Tx.Lang in 'FHIR.Tx.Lang.pas',
  FHIR.Tx.Manager in 'FHIR.Tx.Manager.pas',
  FHIR.Tx.MimeTypes in 'FHIR.Tx.MimeTypes.pas',
  FHIR.Tx.Operations in 'FHIR.Tx.Operations.pas',
  FHIR.Tx.RxNorm in 'FHIR.Tx.RxNorm.pas',
  FHIR.Tx.Server in 'FHIR.Tx.Server.pas',
  FHIR.Tx.Service in '..\Libraries\FHIR.Tx.Service.pas',
  FHIR.Tx.Unii in 'FHIR.Tx.Unii.pas',
  FHIR.Tx.Uri in 'FHIR.Tx.Uri.pas',
  FHIR.Tx.UsState in 'FHIR.Tx.UsState.pas',
  FHIR.Tx.Web in 'FHIR.Tx.Web.pas',
  FHIR.Ucum.Base in '..\Libraries\Ucum\FHIR.Ucum.Base.pas',
  FHIR.Ucum.Expressions in '..\Libraries\Ucum\FHIR.Ucum.Expressions.pas',
  FHIR.Ucum.Handlers in '..\Libraries\Ucum\FHIR.Ucum.Handlers.pas',
  FHIR.Ucum.IFace in '..\reference-platform\support\FHIR.Ucum.IFace.pas',
  FHIR.Ucum.Search in '..\Libraries\Ucum\FHIR.Ucum.Search.pas',
  FHIR.Ucum.Services in '..\Libraries\Ucum\FHIR.Ucum.Services.pas',
  FHIR.Ucum.Validators in '..\Libraries\Ucum\FHIR.Ucum.Validators.pas',
  FHIR.Utilities.SCrypt in '..\Libraries\security\FHIR.Utilities.SCrypt.pas',
  FHIR.Web.Facebook in '..\reference-platform\Support\FHIR.Web.Facebook.pas',
  FHIR.Web.Fetcher in '..\reference-platform\Support\FHIR.Web.Fetcher.pas',
  FHIR.Web.GraphQL in '..\reference-platform\support\FHIR.Web.GraphQL.pas',
  FHIR.Web.HtmlGen in '..\reference-platform\Support\FHIR.Web.HtmlGen.pas',
  FHIR.Web.Parsers in '..\reference-platform\Support\FHIR.Web.Parsers.pas',
  FHIR.Web.Rdf in '..\reference-platform\support\FHIR.Web.Rdf.pas',
  FHIR.Web.Socket in '..\reference-platform\Support\FHIR.Web.Socket.pas',
  FHIR.Web.WinInet in '..\reference-platform\Support\FHIR.Web.WinInet.pas',
  FHIR.XVersion.ConvBase in '..\reference-platform\xversion\FHIR.XVersion.ConvBase.pas',
  FHIR.Server.Tags in 'FHIR.Server.Tags.pas',
  FHIR.Tools.CodeSystemProvider in '..\reference-platform\tools\FHIR.Tools.CodeSystemProvider.pas',
  FHIR.Tools.ValueSets in '..\reference-platform\tools\FHIR.Tools.ValueSets.pas',
  FHIR.R2.Javascript in '..\reference-platform\dstu2\FHIR.R2.Javascript.pas',
  FHIR.R3.Javascript in '..\reference-platform\dstu3\FHIR.R3.Javascript.pas',
  FHIR.Server.Context in 'FHIR.Server.Context.pas',
  FHIR.Server.Indexing in 'FHIR.Server.Indexing.pas',
  FHIR.R2.IndexInfo in '..\reference-platform\dstu2\FHIR.R2.IndexInfo.pas',
  FHIR.R3.IndexInfo in '..\reference-platform\dstu3\FHIR.R3.IndexInfo.pas',
  FHIR.R2.AuthMap in '..\reference-platform\dstu2\FHIR.R2.AuthMap.pas',
  FHIR.R3.AuthMap in '..\reference-platform\dstu3\FHIR.R3.AuthMap.pas',
  FHIR.R4.GraphDefinition in '..\reference-platform\r4\FHIR.R4.GraphDefinition.pas',
  FHIR.R3.GraphDefinition in '..\reference-platform\dstu3\FHIR.R3.GraphDefinition.pas',
  FHIR.Base.GraphDefinition in '..\reference-platform\base\FHIR.Base.GraphDefinition.pas',
  FHIR.Server.BundleBuilder in 'FHIR.Server.BundleBuilder.pas',
  FHIR.Server.IndexingR4 in 'FHIR.Server.IndexingR4.pas',
  FHIR.Server.IndexingR3 in 'FHIR.Server.IndexingR3.pas',
  FHIR.Server.IndexingR2 in 'FHIR.Server.IndexingR2.pas',
  FHIR.Server.OperationsR2 in 'FHIR.Server.OperationsR2.pas',
  FHIR.Server.OperationsR3 in 'FHIR.Server.OperationsR3.pas',
  FHIR.Server.OperationsR4 in 'FHIR.Server.OperationsR4.pas',
  FHIR.Server.SubscriptionsR2 in 'FHIR.Server.SubscriptionsR2.pas',
  FHIR.Server.SubscriptionsR3 in 'FHIR.Server.SubscriptionsR3.pas',
  FHIR.Server.SubscriptionsR4 in 'FHIR.Server.SubscriptionsR4.pas',
  FHIR.R2.Questionnaire in '..\reference-platform\dstu2\FHIR.R2.Questionnaire.pas',
  FHIR.R2.Narrative2 in '..\reference-platform\dstu2\FHIR.R2.Narrative2.pas',
  FHIR.R3.Questionnaire in '..\reference-platform\dstu3\FHIR.R3.Questionnaire.pas',
  FHIR.R3.Narrative2 in '..\reference-platform\dstu3\FHIR.R3.Narrative2.pas',
  FHIR.XVersion.Convertors in '..\reference-platform\xversion\FHIR.XVersion.Convertors.pas',
  FHIR.XVersion.Conv_30_40 in '..\reference-platform\xversion\FHIR.XVersion.Conv_30_40.pas',
  FHIR.Tools.NDJsonParser in '..\reference-platform\tools\FHIR.Tools.NDJsonParser.pas',
  FHIR.Server.Factory in 'FHIR.Server.Factory.pas',
  FHIR.Server.ValidatorR4 in 'FHIR.Server.ValidatorR4.pas',
  FHIR.Server.ValidatorR2 in 'FHIR.Server.ValidatorR2.pas',
  FHIR.Server.ValidatorR3 in 'FHIR.Server.ValidatorR3.pas',
  FHIR.Server.SessionMgr in 'FHIR.Server.SessionMgr.pas',
  FHIR.Server.Manager in 'FHIR.Server.Manager.pas' {ServerManagerForm},
  FHIR.Cache.PackageManagerDialog in '..\reference-platform\cache\FHIR.Cache.PackageManagerDialog.pas' {PackageCacheForm},
  VirtualTrees.Actions in '..\..\Components\treeview\Source\VirtualTrees.Actions.pas',
  VirtualTrees.Classes in '..\..\Components\treeview\Source\VirtualTrees.Classes.pas',
  VirtualTrees.ClipBoard in '..\..\Components\treeview\Source\VirtualTrees.ClipBoard.pas',
  VirtualTrees.Export in '..\..\Components\treeview\Source\VirtualTrees.Export.pas',
  VirtualTrees in '..\..\Components\treeview\Source\VirtualTrees.pas',
  VirtualTrees.StyleHooks in '..\..\Components\treeview\Source\VirtualTrees.StyleHooks.pas',
  VirtualTrees.Utils in '..\..\Components\treeview\Source\VirtualTrees.Utils.pas',
  VirtualTrees.WorkerThread in '..\..\Components\treeview\Source\VirtualTrees.WorkerThread.pas',
  VTAccessibility in '..\..\Components\treeview\Source\VTAccessibility.pas',
  VTAccessibilityFactory in '..\..\Components\treeview\Source\VTAccessibilityFactory.pas',
  VTHeaderPopup in '..\..\Components\treeview\Source\VTHeaderPopup.pas',
  FHIR.Cache.PackageBrowser in '..\reference-platform\cache\FHIR.Cache.PackageBrowser.pas' {PackageFinderForm},
  FHIR.Snomed.Combiner in '..\Libraries\snomed\FHIR.Snomed.Combiner.pas';

begin
  logfile := IncludeTrailingPathDelimiter(SystemTemp)+'fhirserver.log';
  if ParamCount = 0 then
  begin
    filelog := true;
    logt('testing');
  end;
  JclStartExceptionTracking;
  IdOpenSSLSetLibPath(ExtractFilePath(Paramstr(0)));
  try
    SetConsoleTitle('FHIR Server R4');
    ExecuteFhirServer;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      sleep(1000);
    end;
  end;
end.


