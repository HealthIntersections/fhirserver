program fhirtoolkit;

{$I fhir.inc}

uses
  cmem,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frm_main,
  { you can add units after this }
  fsl_base, fsl_threads, fsl_logging, fsl_utilities, fsl_collections, fsl_xml,
  fsl_json, fui_lcl_managers, fhir_objects, fhir_xhtml, fsl_http, v2_dictionary,
  fsl_ucum, fhir_client, fhir_oauth, frm_file_format, ftk_context,
  FrameViewer09, ftk_console, ftk_store_files, ftk_store_temp, ftk_editor_ini,
  ftk_editor_text, ftk_editor_xml, ftk_factory, ftk_editor_json,
  ftk_editor_base, ftk_editor_html, ftk_editor_js, frm_settings, ftk_editor_hl7,
  ftk_editor_fhir, ftk_editor_md, ftk_search, ftk_frame_codesystem,
  ftk_frame_resource, frm_about, ftk_version, frm_edit_changes,
  frm_server_settings, ftk_utilities, ftk_serverlist, ftk_constants,
  ftk_worker_base, ftk_frame_server, ftk_worker_server, ftk_store_internal,
  ftk_store_server, ftk_frame_resource_tree, ftk_frame_patient, frm_oauth,
  ftk_terminology_service, ftk_editor_jwt, frm_format_chooser, frm_clip_chooser,
  frm_file_deleted, frm_file_changed, frm_project_editor, frm_view_manager,
  dlg_new_resource, ftk_fhir_context, ftk_fhir_context_5, ftk_worker_home,
  dlg_open_url, ftk_store_http;

{$R *.res}

var
  frm : TToolkitAboutForm;

begin
  RequireDerivedFormResource:=True;
  Application.Title:='FHIRToolkit';
  Application.Scaled:=True;
  Application.Initialize;

  frm := TToolkitAboutForm.create(Application);
  try
    frm.Button1.Visible := false;
    frm.Show;
    frm.Update;
    Application.CreateForm(TMainToolkitForm, MainToolkitForm);
    frm.Close;
  finally
    frm.Free;
  end;
  Application.Run;
end.

