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
  fsl_base, fsl_threads, fsl_logging,
  fsl_utilities, fsl_collections, fsl_xml,
  fsl_json, fui_orchestrators, fhir_objects,
  fhir_xhtml, fsl_http, frm_progress, SynHighlighterHL7,
  FHIR.LCL.Synchroniser, v2_objects, v2_base, v2_dictionary,
  fhir_ucum, fhir_client, fhir4_parser, fhir4_types,
  fhir4_base, fhir4_resources, fhir4_xml, fhir4_json, fhir4_turtle,
  fhir4_context, cda_narrative, cda_base, cda_types,
  fhir_oauth, frm_file_format, ftk_context, FrameViewer09,
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

