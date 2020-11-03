program FHIRToolkit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  FastMM4,
  Interfaces, // this includes the LCL widgetset
  Forms, frm_main, frm_npm_manager, frm_npm_browser,
  { you can add units after this }
  FHIR.Support.Base, FHIR.Support.Threads, FHIR.Support.Logging,
  FHIR.Support.Utilities, FHIR.LCL.Managers, FHIR.Base.Lang, FHIR.Web.Parsers,
  frm_progress, frm_file_format, FHIR.Toolkit.Context, FrameViewer09,
  FHIR.Toolkit.Console, FHIR.Toolkit.FileStore, FHIR.Toolkit.IniEditor,
  FHIR.Toolkit.TextEditor, FHIR.Toolkit.XmlEditor, FHIR.Toolkit.Factory,
  FHIR.Toolkit.JsonEditor, FHIR.Toolkit.PlainTextEditor,
  FHIR.Toolkit.BaseEditor, FHIR.Toolkit.HtmlEditor, frm_settings;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainToolkitForm, MainToolkitForm);
  Application.Run;
end.

