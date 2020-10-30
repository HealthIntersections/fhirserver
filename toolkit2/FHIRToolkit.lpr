program FHIRToolkit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  FastMM4,
  Interfaces, // this includes the LCL widgetset
  Forms, frm_main, frm_npm_manager, frm_fhir_browser,
  { you can add units after this }
  FHIR.Toolkit.Context, FHIR.Support.Base, FHIR.Support.Threads,
  FHIR.Support.Logging, FHIR.Support.Utilities, 
  FHIR.LCL.Managers, FHIR.Base.Lang, FHIR.Web.Parsers, frm_progress;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

