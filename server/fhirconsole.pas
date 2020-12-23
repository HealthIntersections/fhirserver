program fhirconsole;

{$i fhir.inc}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms, Dialogs, datetimectrls,
  IdOpenSSLLoader,
  fsl_base, fsl_fpc, fsl_utilities, fsl_openssl,
  fdb_odbc_fpc,
  console_form,
  console_tx_edit, console_ep_edit, install_form, install_log, installer;

{$R *.res}

var
  ok : boolean;
begin
  try
    initialiseTZData(partnerFile('tzdata.tar.gz'));
    InitialiseODBC;
    {$IFDEF WINDOWS}
    GetOpenSSLLoader.OpenSSLPath := ExtractFilePath(Paramstr(0));
    {$ENDIF}
    InitOpenSSL;
    ok := true;
  except
    on e : Exception do
    begin
      MessageDlg('Initialization failure', e.message, mtError, [mbClose], 0);
      ok := false;
    end;
  end;

  if ok then
  begin
    RequireDerivedFormResource:=True;
    Application.Scaled:=True;

    Application.Initialize;
    Application.CreateForm(TMainConsoleForm, MainConsoleForm);
    Application.Run;
  end;
end.

