unit fui_lcl_progress;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Buttons, ExtCtrls,
  fsl_threads;

type
  TPerformTaskEvent = procedure (sender : TObject; progress : TWorkProgressEvent) of object;

  { TProgressForm }

  TProgressForm = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    taskid : integer;
    finished : boolean;
    started : boolean;
    stopped : boolean;
    event : TPerformTaskEvent;
    procedure start;
    procedure progress(sender : TObject; pct : integer; done : boolean; desc : String);
    procedure done(id : integer; response : TBackgroundTaskResponsePackage);
  public
  end;

var
  ProgressForm: TProgressForm;

function DoBackgroundTask(owner : TComponent; taskid : integer; request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage) : boolean;
function DoForegroundTask(owner : TComponent; event : TPerformTaskEvent) : boolean;

implementation

{$R *.lfm}

function DoBackgroundTask(owner : TComponent; taskid : integer; request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage) : boolean;
begin
  ProgressForm := TProgressForm.create(owner);
  try
    ProgressForm.taskId:= taskId;
    GBackgroundTasks.queueTask(taskId, request, response, ProgressForm.done);
    ProgressForm.showModal;
    result := ProgressForm.finished;
  finally
    ProgressForm.free;
  end;
end;

function DoForegroundTask(owner : TComponent; event : TPerformTaskEvent) : boolean;
begin
  ProgressForm := TProgressForm.create(owner);
  try
    ProgressForm.event := event;
    ProgressForm.showModal;
    result := ProgressForm.finished;
  finally
    ProgressForm.free;
  end;
end;

{ TProgressForm }

procedure TProgressForm.Timer1Timer(Sender: TObject);
var
  info : TBackgroundTaskStatusInfo;
begin
  if taskId <> 0 then
  begin
    info := GBackgroundTasks.report(taskId);
    try
      Caption := info.name;
      Label1.Caption:= info.message;
      if info.pct < 0 then
        ProgressBar1.Enabled := false
      else
      begin
        ProgressBar1.Enabled := true;
        ProgressBar1.Position := info.pct;
      end;
    finally
      info.free;
    end;
  end
  else
  begin
    if not started then
      start
    else
      application.ProcessMessages;
  end;
end;

procedure TProgressForm.start;
begin
  stopped := false;
  started := true;
  Label1.caption := 'Starting';
  event(self, progress);
end;

procedure TProgressForm.progress(sender: TObject; pct: integer; done: boolean; desc: String);
begin
  ProgressBar1.Position := pct;
  label1.caption := desc;
  finished := done;
  if stopped then
    abort
  else
    Application.ProcessMessages;
  if finished then
    close;
end;

procedure TProgressForm.done(id : integer; response : TBackgroundTaskResponsePackage);
begin
  finished := true;
  close;
end;

procedure TProgressForm.BitBtn1Click(Sender: TObject);
begin
  if taskId <> 0 then
  begin
    GBackgroundTasks.killTask(taskId);
    close;
  end
  else
  begin
    stopped := true;
  end;
end;

procedure TProgressForm.FormShow(Sender: TObject);
begin
  Timer1.Enabled := true;
end;

end.

