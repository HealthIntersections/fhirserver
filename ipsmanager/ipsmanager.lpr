program ipsmanager;

{$I fhir.inc}

uses
  {$IFDEF WINDOWS}
  FastMM4,
  {$ELSE}
  cmem,
  cthreads,
  {$ENDIF}

  Interfaces, // this includes the LCL widgetset
  Forms, datetimectrls,
  { you can add units after this }
  fsl_base, fsl_threads, fsl_logging, fsl_utilities, fsl_collections, fsl_xml,
  fsl_json, fui_lcl_managers, fhir_objects, fhir_xhtml, fsl_http, v2_dictionary,
  fsl_ucum, fhir_client, fhir_oauth, fsl_web_init,
  FrameViewer09,
  frm_home, mvBase, mvDataSources;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'Patient Summary Manager';
  Application.Scaled := True;
  Application.Initialize;

  Application.CreateForm(TIPSManagerForm, IPSManagerForm);
  Application.Run;
end.
