program FHIRConsole;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ELSE}
  FastMM4,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, datetimectrls, FHIR.Support.Threads,
  FHIR.Snomed.Combiner, FHIR.Web.Parsers, FHIR.Base.Objects, FHIR.Ucum.IFace,
  FHIR.Npm.Cache, FHIR.Client.Base, FHIR.CdsHooks.Utilities,
  FHIR.Smart.Utilities, FHIR.Tx.Service, FHIR.Database.Manager,
  FHIR.Loinc.Services,
  { you can add units after this }
  FHIR.Server.Console, FHIR.Server.Connection.Lcl;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainConsoleForm, MainConsoleForm);
  Application.CreateForm(TServerConnectionForm, ServerConnectionForm);
  Application.Run;
end.

