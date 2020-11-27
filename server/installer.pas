unit installer;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Controls, Dialogs,
  fsl_base,
  fdb_manager,
  server_config, database_installer,
  utilities,
  install_form;

procedure InstallEndPoint(owner : TComponent; cfg : TFHIRServerConfigFile; epInfo : TFHIRServerConfigSection);

implementation

uses
  console_form;

procedure InstallEndPoint(owner : TComponent; cfg : TFHIRServerConfigFile; epInfo : TFHIRServerConfigSection);
var
  t : String;
  db : TFDBManager;
  conn : TFDBConnection;
  dbi : TFHIRDatabaseInstaller;
  form : TEndpointInstallForm;
begin
  db := connectToDatabase(epInfo);
  try
    conn := db.GetConnection('install');
    try
      t := epInfo['type'].value;
      if (t = 'package') then
      begin
        if MessageDlg('Install Package Server', 'This operation will wipe any existing installation in the database. Proceed?', mtConfirmation, mbYesNo, 0) = mrYes then
        begin
          dbi := TFHIRDatabaseInstaller.create(conn, nil, nil);
          try
            dbi.uninstall;
            dbi.installPackageServer;
          finally
            dbi.free;
          end;
        end;
      end
      else
      begin
        form := TEndpointInstallForm.create(owner);
        try
          form.Packages := MainConsoleForm.Packages.link;
          form.Connection := conn.link;
          form.Filename := cfg.filename;
          form.endpoint := epInfo.name;
          form.type_ := epInfo['type'].value;
          form.version := epInfo['version'].value;
          form.ShowModal;
        finally
          form.free;
        end;
      end;
    finally
      conn.release;
    end;
  finally
    db.free;
  end;
end;


end.

