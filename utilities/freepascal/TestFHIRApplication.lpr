{$MODE delphi}
program TestFHIRApplication;

uses Generics.Collections;

type
  TFslObject = class(TObject)
  end;

  TFslPair<T: TFslObject> = record
  end;

  TFslMap<T: TFslObject> = class(TEnumerable<TFslPair<T>>)
  end;

begin

end.
!!
{$MODE delphi}
program TestFHIRApplication;

uses
  Generics.Collections;

type
  TFslObject = class(TObject)
  end;

  TFslPair<T: TFslObject> = record
  end;

  TFslMap<T: TFslObject> = class(TEnumerable<TFslPair<T>>)
  end;

begin
end.
(*
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, FHIR.Support.Objects, FHIR.Support.DateTime,
  FHIR.Support.Decimal, FHIR.Support.TarBall, FHIR.Base.Objects;

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
*)
