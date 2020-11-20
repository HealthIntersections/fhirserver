program FHIRConsole;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ELSE}
  FastMM4,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  datetimectrls,
  fsl_base,
  fsl_http, fhir_utilities, fsl_ucum,
  fsl_npm_cache, fhir_client, fhir_cdshooks,
  fhir_oauth,
  console_form,
  console_server_form;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainConsoleForm, MainConsoleForm);
  Application.CreateForm(TServerConnectionForm, ServerConnectionForm);
  Application.Run;
end.

