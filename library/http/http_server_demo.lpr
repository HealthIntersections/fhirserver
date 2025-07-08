program http_server_demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  http_server_example;

type

  { THTTPServerDemo }

  THTTPServerDemo = class(TCustomApplication)
  private
    procedure RunServer(port : integer);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ THTTPServerDemo }

procedure THTTPServerDemo.RunServer(port: integer);
var
  WebServer : TMyWebServer;
begin
  WebServer := TMyWebServer.Create;
  try
    WebServer.Start(Port);
    try
      writeLn('Server running on port '+inttostr(port)+'. Press Enter to stop');
      ReadLn; // Wait for user input
    finally
    end;
  finally
    WebServer.Free;
  end;
end;

procedure THTTPServerDemo.DoRun;
var
  ErrorMsg: String;
  Port : integer;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <> '' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  try
    Port := 7180;
    if ParamCount > 0 then
      Port := StrToIntDef(ParamStr(1), 7180);
     RunServer(port);
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ReadLn;
    end;
  end;

  Terminate;
end;

constructor THTTPServerDemo.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor THTTPServerDemo.Destroy;
begin
  inherited Destroy;
end;

procedure THTTPServerDemo.WriteHelp;
begin
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: THTTPServerDemo;
begin
  Application:=THTTPServerDemo.Create(nil);
  Application.Title:='Demonstration HTTP Server';
  Application.Run;
  Application.Free;
end.

