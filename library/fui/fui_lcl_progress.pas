unit fui_lcl_progress;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Buttons, ExtCtrls,
  fsl_threads;

type
  { TProgressForm }

  TProgressForm = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure BitBtn1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    taskid : integer;
    finished : boolean;
    procedure done(id : integer; response : TBackgroundTaskResponsePackage);
  public
  end;

var
  ProgressForm: TProgressForm;

function DoTask(owner : TComponent; taskid : integer; request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage) : boolean;

implementation

{$R *.lfm}

function DoTask(owner : TComponent; taskid : integer; request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage) : boolean;
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

{ TProgressForm }

procedure TProgressForm.Timer1Timer(Sender: TObject);
var
  info : TBackgroundTaskStatusInfo;
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
end;

procedure TProgressForm.done(id : integer; response : TBackgroundTaskResponsePackage);
begin
  finished := true;
  close;
end;

procedure TProgressForm.BitBtn1Click(Sender: TObject);
begin
  GBackgroundTasks.killTask(taskId);
  close;
end;

end.

