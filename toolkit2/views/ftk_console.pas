unit ftk_console;

{$i fhir.inc}

interface

uses
  Sysutils, Classes,
  fsl_base, fsl_utilities, fsl_threads, fsl_logging;

type

  { TToolkitConsole }

  TToolkitConsole = class (TLogListener)
  private
    FLock : TFslLock;
    FLines : TStringList;

  protected
    procedure newDay(const s : String); override;
    procedure log(const s : String); override;
    procedure closing; override;

    function transient : boolean; override;
    procedure logStart(s : String); override;
    procedure logContinue(s : String); override;
    procedure logFinish(s : String); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure GetIncoming(list : TStringList);
  end;

implementation

{ TToolkitConsole }

procedure TToolkitConsole.newDay(const s: String);
begin
end;

procedure TToolkitConsole.log(const s: String);
begin
  FLock.Lock;
  try
    FLines.add(s);
  finally
    FLock.Unlock;
  end;
end;

procedure TToolkitConsole.closing;
begin
end;

function TToolkitConsole.transient: boolean;
begin
  Result := false
end;

procedure TToolkitConsole.logStart(s: String);
begin
  inherited logStart(s);
end;

procedure TToolkitConsole.logContinue(s: String);
begin
  inherited logContinue(s);
end;

procedure TToolkitConsole.logFinish(s: String);
begin
  inherited logFinish(s);
end;

constructor TToolkitConsole.Create;
begin
  inherited Create;
  FLock := TFslLock.create('console');
  FLines := TStringList.create;
end;

destructor TToolkitConsole.Destroy;
begin
  FLines.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TToolkitConsole.GetIncoming(list: TStringList);
begin
  FLock.Lock;
  try
    list.assign(FLines);
    FLines.Clear;
  finally
    FLock.Unlock;
  end;
end;

end.
