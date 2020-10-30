program FHIRToolkit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  FastMM4,
  Interfaces, // this includes the LCL widgetset
  Forms, frm_main, FHIR.Toolkit.Context, FHIR.Support.Base,
  FHIR.Support.Logging, FHIR.Support.Utilities, frm_fhir_browser,
  frm_fhir_manager, FHIR.LCL.Managers, FHIR.Base.Lang, FHIR.Web.Parsers
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TPackageCacheForm, PackageCacheForm);
  Application.Run;
end.

