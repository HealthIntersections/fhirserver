program FHIRToolkit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  {$IFDEF WINDOWS}
  FastMM4,
  {$ENDIF}
  Windows,
  Interfaces, // this includes the LCL widgetset
  Forms, frm_main, frm_npm_manager, frm_npm_browser,
  { you can add units after this }
  FHIR.Support.Base, FHIR.Support.Threads, FHIR.Support.Logging,
  FHIR.Support.Utilities, FHIR.Support.Collections, FHIR.Support.MXml,
  FHIR.Support.Json, FHIR.LCL.Managers, FHIR.Base.Lang, FHIR.Base.Objects,
  FHIR.Base.Xhtml, FHIR.Web.Parsers, frm_progress, SynHighlighterHL7,
  FHIR.LCL.Synchroniser, FHIR.v2.Objects, FHIR.v2.Base, FHIR.v2.Dictionary,
  FHIR.Ucum.IFace, FHIR.Client.Base, FHIR.R4.Parser, FHIR.R4.Types,
  FHIR.R4.Base, FHIR.R4.Resources, FHIR.R4.Xml, FHIR.R4.Json, FHIR.R4.Turtle,
  FHIR.R4.Context, FHIR.Cda.Narrative, FHIR.Cda.Base, FHIR.Cda.Types,
  FHIR.Smart.Utilities, frm_file_format, ftk_context, FrameViewer09,
  ftk_console, ftk_store_files, ftk_store_temp, ftk_editor_ini, ftk_editor_text,
  ftk_editor_xml, ftk_factory, ftk_editor_json, ftk_editor_base,
  ftk_editor_html, ftk_editor_js, frm_settings, ftk_editor_hl7, ftk_editor_fhir,
  ftk_editor_md, ftk_search, ftk_frame_codesystem, ftk_frame_resource;

{$R *.res}

begin
  AllocConsole;
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainToolkitForm, MainToolkitForm);
  Application.Run;
end.

