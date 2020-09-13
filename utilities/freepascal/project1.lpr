program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, FHIR.Support.Base, FHIR.Support.Utilities,
  FHIR.Support.Signatures, FHIR.Support.Stream, FHIR.Base.Common,
  FHIR.Base.Objects, FHIR.Base.Xhtml, FHIR.Base.Lang, FHIR.Web.Parsers,
  FHIR.Ucum.IFace, FHIR.Cache.PackageManager, FHIR.Client.Base,
  FHIR.Smart.Utilities, FHIR.Cda.Narrative,
  FHIR.R2.Base, FHIR.R2.Types, FHIR.R2.Resources, FHIR.R2.Json, FHIR.R2.Xml,
  FHIR.R3.Base, FHIR.R3.Types, FHIR.R3.Resources, FHIR.R3.Json, FHIR.R3.Xml,
  FHIR.R4.Base, FHIR.R4.Types, FHIR.R4.Resources, FHIR.R4.Json, FHIR.R4.Xml,
  FHIR.R5.Base, FHIR.R5.Types, FHIR.R5.Resources, FHIR.R5.Json, FHIR.R5.Xml,
  MarkdownHTMLEntities, IdCookie, IdGlobal, IdIPAddress,
  CustApp
  { you can add units after this };

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
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

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

