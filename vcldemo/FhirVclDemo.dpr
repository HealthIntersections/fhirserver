program FhirVclDemo;

uses
  FastMM4 in '..\Libraries\FMM\FastMM4.pas' {Vcl.Forms},
  Vcl.Forms,
  MainApplicationWindow in 'MainApplicationWindow.pas' {MainWindowForm},
  FastMM4Messages in '..\Libraries\FMM\FastMM4Messages.pas',
  FHIR.Support.Utilities in '..\reference-platform\support\FHIR.Support.Utilities.pas',
  FHIR.Support.Base in '..\reference-platform\support\FHIR.Support.Base.pas',
  FHIR.Support.Stream in '..\reference-platform\support\FHIR.Support.Stream.pas',
  FHIR.Support.Collections in '..\reference-platform\support\FHIR.Support.Collections.pas',
  FHIR.Base.Objects in '..\reference-platform\base\FHIR.Base.Objects.pas',
  FHIR.R2.Utilities in '..\reference-platform\dstu2\FHIR.R2.Utilities.pas',
  FHIR.Web.Parsers in '..\reference-platform\support\FHIR.Web.Parsers.pas',
  FHIR.Support.Json in '..\reference-platform\support\FHIR.Support.Json.pas',
  FHIR.Web.Fetcher in '..\reference-platform\support\FHIR.Web.Fetcher.pas',
  FHIR.R2.Context in '..\reference-platform\dstu2\FHIR.R2.Context.pas',
  FHIR.R2.Types in '..\reference-platform\dstu2\FHIR.R2.Types.pas',
  FHIR.R2.Resources in '..\reference-platform\dstu2\FHIR.R2.Resources.pas',
  FHIR.Base.Scim in '..\reference-platform\base\FHIR.Base.Scim.pas',
  FHIR.Support.MXml in '..\reference-platform\support\FHIR.Support.MXml.pas',
  FHIR.Tools.Indexing in '..\reference-platform\tools\FHIR.Tools.Indexing.pas',
  FHIR.R2.Constants in '..\reference-platform\dstu2\FHIR.R2.Constants.pas',
  FHIR.R2.Tags in '..\reference-platform\dstu2\FHIR.R2.Tags.pas',
  FHIR.Base.Lang in '..\reference-platform\base\FHIR.Base.Lang.pas',
  FHIR.Base.Xhtml in '..\reference-platform\base\FHIR.Base.Xhtml.pas',
  FHIR.Version.Parser in '..\reference-platform\version\FHIR.Version.Parser.pas',
  FHIR.Base.Parser in '..\reference-platform\base\FHIR.Base.Parser.pas',
  FHIR.Support.Turtle in '..\reference-platform\support\FHIR.Support.Turtle.pas',
  FHIR.R2.Xml in '..\reference-platform\dstu2\FHIR.R2.Xml.pas',
  FHIR.R2.Json in '..\reference-platform\dstu2\FHIR.R2.Json.pas',
  FHIR.R2.ElementModel in '..\reference-platform\dstu2\FHIR.R2.ElementModel.pas',
  FHIR.R2.Profiles in '..\reference-platform\dstu2\FHIR.R2.Profiles.pas',
  FHIR.Support.Threads in '..\reference-platform\support\FHIR.Support.Threads.pas',
  FHIR.R2.PathEngine in '..\reference-platform\dstu2\FHIR.R2.PathEngine.pas',
  FHIR.Version.Client in '..\reference-platform\version\FHIR.Version.Client.pas',
  FHIR.Web.WinInet in '..\reference-platform\support\FHIR.Web.WinInet.pas',
  FHIR.CdsHooks.Utilities in '..\reference-platform\support\FHIR.CdsHooks.Utilities.pas',
  MarkdownProcessor in '..\..\markdown\source\MarkdownProcessor.pas',
  MarkdownDaringFireball in '..\..\markdown\source\MarkdownDaringFireball.pas',
  MarkdownCommonMark in '..\..\markdown\source\MarkdownCommonMark.pas',
  FHIR.Support.Shell in '..\reference-platform\support\FHIR.Support.Shell.pas',
  FHIR.Ucum.IFace in '..\reference-platform\support\FHIR.Ucum.IFace.pas',
  FHIR.R2.PathNode in '..\reference-platform\dstu2\FHIR.R2.PathNode.pas',
  FHIR.R2.Base in '..\reference-platform\dstu2\FHIR.R2.Base.pas',
  FHIR.R2.Parser in '..\reference-platform\dstu2\FHIR.R2.Parser.pas',
  FHIR.R2.ParserBase in '..\reference-platform\dstu2\FHIR.R2.ParserBase.pas',
  FHIR.Client.Base in '..\reference-platform\client\FHIR.Client.Base.pas',
  FHIR.Client.HTTP in '..\reference-platform\client\FHIR.Client.HTTP.pas',
  FHIR.Client.Threaded in '..\reference-platform\client\FHIR.Client.Threaded.pas',
  FHIR.R2.Client in '..\reference-platform\dstu2\FHIR.R2.Client.pas',
  FHIR.Support.Xml in '..\reference-platform\support\FHIR.Support.Xml.pas',
  FHIR.Support.Certs in '..\reference-platform\support\FHIR.Support.Certs.pas',
  FHIR.Web.GraphQL in '..\reference-platform\support\FHIR.Web.GraphQL.pas',
  FHIR.Base.Factory in '..\reference-platform\base\FHIR.Base.Factory.pas',
  FHIR.Base.Validator in '..\reference-platform\base\FHIR.Base.Validator.pas',
  FHIR.Base.Common in '..\reference-platform\base\FHIR.Base.Common.pas',
  FHIR.Base.Narrative in '..\reference-platform\base\FHIR.Base.Narrative.pas',
  FHIR.Base.PathEngine in '..\reference-platform\base\FHIR.Base.PathEngine.pas',
  FHIR.R2.Common in '..\reference-platform\dstu2\FHIR.R2.Common.pas',
  FHIR.Support.Signatures in '..\reference-platform\support\FHIR.Support.Signatures.pas',
  fhir.support.fpc in '..\reference-platform\support\fhir.support.fpc.pas',
  FHIR.Base.Utilities in '..\reference-platform\base\FHIR.Base.Utilities.pas',
  FHIR.Smart.Utilities in '..\reference-platform\client\FHIR.Smart.Utilities.pas',
  FHIR.Smart.Login in '..\reference-platform\client\FHIR.Smart.Login.pas',
  FHIR.Smart.LoginVCL in '..\reference-platform\client\FHIR.Smart.LoginVCL.pas' {SmartOnFhirLoginForm},
  FHIR.R2.Operations in '..\reference-platform\dstu2\FHIR.R2.Operations.pas',
  FHIR.R2.OpBase in '..\reference-platform\dstu2\FHIR.R2.OpBase.pas',
  FHIR.Support.Osx in '..\reference-platform\support\FHIR.Support.Osx.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainWindowForm, MainWindowForm);
  Application.CreateForm(TSmartOnFhirLoginForm, SmartOnFhirLoginForm);
  Application.CreateForm(TSmartOnFhirLoginForm, SmartOnFhirLoginForm);
  Application.CreateForm(TSmartOnFhirLoginForm, SmartOnFhirLoginForm);
  Application.Run;
end.
