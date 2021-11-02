program codescan;

{this programs run as part of the ci-build to enforce coding standards
on the code. THe following rules are enforced:

* code can't have any unicode bi-di control characters in it
   file extensions: .pas, .lpr, .inc, .html, .css,
   (including in all the source packages too)
* all .pas files must have a license statement in a comment at the head
   (except for in /dependencies
* never raise Exception directly - always a subclass
   (and all exceptions should subclass EFslException outside /dependencies
* check .pas line endings are all crlf (make them so)
*

for now, the program doesn't apply code formatting to the code.
That might be reviewed if a working code formatter is found in the
future

The program takes one parameter, which is the name of the root folder that contains the source directory.
It assumes that the root of the FHIRServer repository is two folders up from the executable

}

{$i fhir.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };


type

  { TCodeScanner }

  TCodeScanner = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TCodeScanner }

procedure TCodeScanner.DoRun;
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

constructor TCodeScanner.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TCodeScanner.Destroy;
begin
  inherited Destroy;
end;

procedure TCodeScanner.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TCodeScanner;
begin
  Application:=TCodeScanner.Create(nil);
  Application.Title:='FHIR Code Scanner';
  Application.Run;
  Application.Free;
end.

