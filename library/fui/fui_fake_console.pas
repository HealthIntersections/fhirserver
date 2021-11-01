unit fui_fake_console;

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

