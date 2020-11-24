unit package_endpoint;

{$i fhir.inc}

interface

uses
  fsl_base,
  fdb_manager,
  server_config,
  endpoint;

type
  TPackageServerEndPoint = class (TFHIRServerEndPoint)
  private
  public
    constructor Create(settings : TFHIRServerConfigSection; db : TFDBManager);
    destructor Destroy; override;
  end;

//      {$IFNDEF FHIR3}
//    FPackageServer : TFHIRPackageServer;
//    {$ENDIF}
//    function packageLink: String;

//  {$IFNDEF FHIR3}
//  TPackageUpdaterThread  = class(TFHIRServerThread)
//  private
//    FDB : TFDBManager;
//    FNextRun : TDateTime;
//    FLastEmail : TDateTime;
//    procedure RunUpdater;
//  protected
//    procedure Execute; override;
//  public
//    constructor Create(server: TFhirWebServer; db : TFDBManager);
//    destructor Destroy; override;
//  end;
//  {$ENDIF}
//    {$IFNDEF FHIR3}
//    property PackageServer : TFHIRPackageServer read FPackageServer;
//    {$ENDIF}
//  {$IFNDEF FHIR3}
//  FPackageServer := TFHIRPackageServer.create;
//  FPackageServer.OnReturnProcessFileEvent := ReturnProcessedFile;
//  {$ENDIF}
//  {$IFNDEF FHIR3}
//  FPackageServer.Free;
//  {$ENDIF}
//  {$IFNDEF FHIR3}
//  if Common.ActualPort <> 80 then
//    FPackageServer.pathAbsolute := 'http://'+Common.host+':'+inttostr(Common.ActualPort)+'/packages'
//  else
//    FPackageServer.pathAbsolute := 'http://'+Common.host+'/packages';
//  FPackageServer.pathRelative := '/packages';
//  {$ENDIF}
//        {$IFNDEF FHIR3}
//        else if (FPackageServer.DB <> nil) and request.Document.startsWith('/packages') then
//          summ := FPackageServer.serve(request, response)
//        {$ENDIF}
//          {$IFNDEF FHIR3}
//          else if (FPackageServer.DB <> nil) and request.Document.startsWith('/packages') then
//            summ := FPackageServer.serve(request, response)
//          {$ENDIF}
//function TFhirWebServer.packageLink: String;
//begin
//  {$IFNDEF FHIR3}
//  if FPackageServer <> nil then
//    result := '<p>This server also runs as a <a href="'+FPackageServer.pathRelative+'">package server</a></p>'
//  else
//  {$ENDIF}
//    result := '';
//end;

//  s := s.Replace('[%package-link%]', packageLink, [rfReplaceAll]);

implementation

{ TPackageServerEndPoint }

constructor TPackageServerEndPoint.Create(settings : TFHIRServerConfigSection; db : TFDBManager);
begin

end;

destructor TPackageServerEndPoint.Destroy;
begin

  inherited;
end;

{ TPackageUpdaterThread }

constructor TPackageUpdaterThread.Create(server: TFhirWebServer; db: TFDBManager);
begin
  inherited create(server, false);
  FDB := db;
  FNextRun := now + 1/(24 * 60);
end;

destructor TPackageUpdaterThread.Destroy;
begin
  FDB.Free;
  inherited;
end;

procedure TPackageUpdaterThread.Execute;
begin
  repeat
    sleep(50);
    if not Terminated and (now > FNextRun) and (FNextRun > 0) then
    begin
      FServer.FPackageServer.scanning := true;
      try
        RunUpdater;
      finally
        FServer.FPackageServer.scanning := false;
      end;
      FNextRun := now + 1/24;
      FServer.FPackageServer.NextScan := FNextRun;
    end;
  until (Terminated);
end;

procedure TPackageUpdaterThread.RunUpdater;
var
  conn : TFDBConnection;
  upd : TPackageUpdater;
begin
  conn := FDB.getConnection('server.packages.update');
  try
    upd := TPackageUpdater.create;
    try
      try
        upd.update(conn);
        if (TFslDateTime.makeToday.DateTime <> FLastEmail) then
        begin
          if upd.errors <> '' then
            sendEmail('grahameg@gmail.com', 'Package Feed Errors', upd.errors);
          FLastEmail := TFslDateTime.makeToday.DateTime;
        end;
      except
        on e : exception do
        begin
          Logging.log('Exception updating packages: '+e.Message);
        end;
      end;
    finally
      upd.free;
    end;
    conn.release;
  except
    on e : Exception do
    begin
      conn.error(e);
      raise;
    end;
  end;
end;

end.
