program fhirtoolkit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frm_main,
  { you can add units after this }
  fsl_base, fsl_threads, fsl_logging,
  fsl_utilities, fsl_collections, fsl_xml,
  fsl_json, fui_lcl_managers, fhir_objects,
  fhir_xhtml, fsl_http,
  v2_dictionary,
  fsl_ucum, fhir_client,
  fhir_oauth, frm_file_format, ftk_context, FrameViewer09,
  ftk_console, ftk_store_files, ftk_store_temp, ftk_editor_ini, ftk_editor_text,
  ftk_editor_xml, ftk_factory, ftk_editor_json, ftk_editor_base,
  ftk_editor_html, ftk_editor_js, frm_settings, ftk_editor_hl7, ftk_editor_fhir,
  ftk_editor_md, ftk_search, ftk_frame_codesystem, ftk_frame_resource,
frm_about, ftk_version, frm_edit_changes;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainToolkitForm, MainToolkitForm);
  Application.CreateForm(TToolkitAboutForm, ToolkitAboutForm);
  Application.CreateForm(TEditChangeReviewForm, EditChangeReviewForm);
  Application.Run;
end.

