{$MODE delphi}
program TestFHIRApplication;

uses
  Classes, SysUtils, unitregistry, FHIR.R2.Context;

!
type

  { TTestFHIRApplication }

  TTestFHIRApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TTestFHIRApplication }

procedure TTestFHIRApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
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

  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TTestFHIRApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TTestFHIRApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TTestFHIRApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TTestFHIRApplication;
begin
  Application:=TTestFHIRApplication.Create(nil);
  Application.Title:='Test FHIR Application';
  Application.Run;
  Application.Free;
end.

