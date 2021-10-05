unit fui_fake_console;

{$i fhir.inc}

interface

{
can't debug console applications on OSX.
This unit works around that limitation
}

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  fsl_logging, fsl_threads;

type
  TFakeConsoleForm = class;

  { TFakeConsoleListener }

  TFakeConsoleListener = class (TLogListener)
  private
    FLine : String;
    FLines : TStringList;
    FLock : TFslLock;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure newDay(const s : String); override;
    procedure log(const s : String); override;
    procedure closing; override;

    function transient : boolean; override;
    procedure logStart(s : String); override;
    procedure logContinue(s : String); override;
    procedure logFinish(s : String); override;
  end;

  TWorkProcedure = procedure;

  { TWorkerThread }

  TWorkerThread = class (TThread)
  private
    FOp: TWorkProcedure;
  protected
    procedure execute; override;
  public
    constructor Create(op : TWorkProcedure);
  end;

  { TFakeConsoleForm }

  TFakeConsoleForm = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    FLogger : TFakeConsoleListener;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FListener: TFakeConsoleListener;
    FStarted : boolean;
    FOp: TWorkProcedure;

    procedure start;
  public
    property Listener : TFakeConsoleListener read FListener;
    property Op : TWorkProcedure read FOp write FOp;
  end;

var
  FakeConsoleForm: TFakeConsoleForm;

implementation

{$R *.lfm}

{ TWorkerThread }

constructor TWorkerThread.Create(op: TWorkProcedure);
begin
  FOp := op;
  inherited Create(false);
end;

procedure TWorkerThread.execute;
begin
  FOp;
end;

{ TFakeConsoleListener }

constructor TFakeConsoleListener.Create;
begin
  inherited Create;
  FLines := TStringList.create;
  FLock := TFslLock.create;
end;

destructor TFakeConsoleListener.Destroy;
begin
  FLines.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TFakeConsoleListener.newDay(const s: String);
begin
  // nothing
end;

procedure TFakeConsoleListener.log(const s: String);
begin
  FLines.add(s);
end;

procedure TFakeConsoleListener.closing;
begin
  // nothing
end;

function TFakeConsoleListener.transient: boolean;
begin
  Result := true
end;

procedure TFakeConsoleListener.logStart(s: String);
begin
  FLine := s;
end;

procedure TFakeConsoleListener.logContinue(s: String);
begin
  FLine := FLine + s;
end;

procedure TFakeConsoleListener.logFinish(s: String);
begin
  FLines.add(FLine + s);
  FLine := '';
end;

{ TFakeConsoleForm }

procedure TFakeConsoleForm.FormCreate(Sender: TObject);
begin
  FListener := TFakeConsoleListener.create;
  Logging.addListener(FListener);
end;

procedure TFakeConsoleForm.FormDestroy(Sender: TObject);
begin
  Logging.removeListener(FListener);
  FListener.free;
end;

procedure TFakeConsoleForm.FormShow(Sender: TObject);
begin
  Timer1.Enabled := true;
end;

procedure TFakeConsoleForm.Timer1Timer(Sender: TObject);
var
  s : String;
begin
  if not FStarted then
    start;
  FListener.FLock.Lock;
  try
    if memo1.lines.count = 0 then
      memo1.lines.add(FListener.FLine)
    else if memo1.lines[0] <> FListener.FLine then
      memo1.lines[0] := FListener.FLine;
    for s in FListener.FLines do
      memo1.lines.insert(1, s);
    FListener.FLines.clear;
  finally
    FListener.FLock.UnLock;
  end;
end;

procedure TFakeConsoleForm.start;
begin
  FStarted := true;
  TWorkerThread.create(FOp);
end;

end.

