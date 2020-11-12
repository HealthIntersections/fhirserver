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
  FHIR.Snomed.Combiner, fsl_http, fhir_utilities, fhir_ucum,
  fsl_npm_cache, fhir_client, fhir_cdshooks,
  fhir_oauth, FHIR.Tx.Service, FHIR.Database.Manager,
  FHIR.Loinc.Services,
  { you can add units after this }
  FHIR.Server.Console,
  FHIR.Server.Connection.Lcl, IdGlobal;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainConsoleForm, MainConsoleForm);
  Application.CreateForm(TServerConnectionForm, ServerConnectionForm);
  Application.Run;
end.

