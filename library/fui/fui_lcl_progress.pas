unit fui_lcl_progress;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Buttons, ExtCtrls,
  fsl_threads;

type
  TPerformTaskEvent = procedure (sender : TObject; context : TObject; progress : TWorkProgressEvent) of object;

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
    FContext : TObject;
    procedure start;
    procedure progress(sender : TObject; pct : integer; done : boolean; desc : String);
    procedure done(id : integer; response : TBackgroundTaskResponsePackage);
  public
  end;

var
  ProgressForm: TProgressForm;

function DoBackgroundTask(owner : TComponent; taskid : integer; request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage) : boolean;
function DoForegroundTask(owner : TComponent; context : TObject; event : TPerformTaskEvent) : boolean;

implementation

{$R *.lfm}

function DoBackgroundTask(owner : TComponent; taskid : integer; request : TBackgroundTaskRequestPackage; response : TBackgroundTaskResponsePackage) : boolean;
begin
  ProgressForm := TProgressForm.Create(owner);
  try
    ProgressForm.taskId := taskId;
    GBackgroundTasks.queueTask(taskId, request, response, ProgressForm.done);
    ProgressForm.showModal;
    result := ProgressForm.finished;
  finally
    ProgressForm.free;
  end;
end;

function DoForegroundTask(owner : TComponent; context : TObject; event : TPerformTaskEvent) : boolean;
begin
  ProgressForm := TProgressForm.Create(owner);
  try
    ProgressForm.event := event;
    ProgressForm.FContext := context;

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
      Label1.Caption := info.message;
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
  event(self, FContext, progress);
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

