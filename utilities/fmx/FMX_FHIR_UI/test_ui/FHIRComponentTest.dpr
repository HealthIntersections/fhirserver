program FHIRComponentTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  fhir_objects in '..\..\..\..\base\fhir_objects.pas',
  fhir4_resources in '..\..\..\..\R4\fhir4_resources.pas',
  FHIR.Server.Session in '..\..\..\..\tools\FHIR.Server.Session.pas',
  fhir_indexing in '..\..\..\..\tools\fhir_indexing.pas',
  FHIR.Server.Security in '..\..\..\..\tools\FHIR.Server.Security.pas',
  FHIR.Version.Parser in '..\..\..\..\version\FHIR.Version.Parser.pas',
  FHIR.FMX.Ctrls in '..\..\..\..\ui\fmx\FMX_FHIR_UI\FHIR.FMX.Ctrls.pas',
  FHIR.Server.XhtmlComp in '..\..\..\..\tools\FHIR.Server.XhtmlComp.pas',
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
