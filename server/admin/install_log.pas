unit install_log;

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
  Process,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ComCtrls,
  ExtCtrls, StdCtrls,
  fsl_base, fsl_threads, fsl_utilities, fsl_logging,
  kernel;

type
  TInstallProgressForm = class;
  TInstallerThread = class;

  { TInstallerListener }
  TInstallerListener = class (TLogListener)
  private
    FThread : TInstallerThread;
  public                        
    procedure log(const s : String); override;
  end;

  TInstallerThreadState = (itsWaiting, itsRunning, itsStopping, itsFinished);

  { TInstallerThread }

  TInstallerThread = class (TFslThread)
  private
    FForm : TInstallProgressForm;
    FIncoming : TStringList;
    FState : TInstallerThreadState;
    FLock : TFslLock;
    procedure getLog(ts : TStringList; var status : String);
  protected
    procedure Execute; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TInstallProgressForm }

  TInstallProgressForm = class(TForm)
    btnCopy: TBitBtn;
    btnCancel: TBitBtn;
    lblStatus: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure btnCopyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FCommand: String;
    FThread : TInstallerThread;

    procedure addToLog(s : String);

  public
    property command : String read FCommand write FCommand;
  end;

var
  InstallProgressForm: TInstallProgressForm;
  CODES_TInstallerThreadState : array [TInstallerThreadState] of String = ('Waiting', 'Running', 'Stopping', 'Finished');

implementation

{$R *.lfm}

{ TInstallProgressForm }

procedure TInstallProgressForm.FormCreate(Sender: TObject);
begin
  FThread := TInstallerThread.Create;
  FThread.FForm := self;
  Timer1.Enabled := true;
end;

procedure TInstallProgressForm.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := false;
  FThread.StopAndWait(2000);
  FThread.free;
end;

procedure TInstallProgressForm.FormShow(Sender: TObject);
begin
  FThread.Start;
end;

procedure TInstallProgressForm.btnCopyClick(Sender: TObject);
begin
  Memo1.CopyToClipboard;
end;

procedure TInstallProgressForm.btnCancelClick(Sender: TObject);
begin
  logging.log('Terminating...');
  FThread.Stop;
end;

procedure TInstallProgressForm.Timer1Timer(Sender: TObject);
var
  ts : TStringList;
  s : String;
begin
  ts := TStringList.create;
  try
    FThread.getLog(ts, s);
    lblStatus.caption := s;
    if (s = 'Finished') then


    for s in ts do
      addToLog(s);
  finally
    ts.free;
  end;
end;

procedure TInstallProgressForm.addToLog(s: String);
begin
  Memo1.lines.Add(s);
  btnCopy.enabled := true;
end;
               
{ TInstallerListener }

procedure TInstallerListener.log(const s: String);
begin
  FThread.Flock.Lock;
  try
    FThread.FIncoming.add(s);
  finally
    FThread.Flock.Unlock;
  end;
end;

{ TInstallerThread }
                      
constructor TInstallerThread.Create;
begin
  inherited Create;
  FIncoming := TStringList.create; 
  FLock := TFslLock.Create;
end;

destructor TInstallerThread.Destroy;
begin
  FLock.free;
  FIncoming.free;
  inherited;
end;

procedure TInstallerThread.getLog(ts: TStringList; var status : String);
begin
  FLock.lock;
  try
    ts.Assign(FIncoming);
    FIncoming.Clear;
    status := CODES_TInstallerThreadState[FState]
  finally
    FLock.Unlock;
  end;
end;

procedure TInstallerThread.Execute;
var
  cp : TCommandLineParameters;
  listener : TInstallerListener;
begin
  listener := TInstallerListener.create;
  cp := TCommandLineParameters.create(FForm.command);
  try
    listener.FThread := self;
    Logging.addListener(listener);
    try
      FState := itsRunning;
      try
        ExecuteFhirServer(cp);
      finally
        FState := itsFinished;
      end;
    finally
      Logging.removeListener(listener);
    end;
  finally
    cp.free;
    listener.free;
  end;
end;

end.
//
//              
//const                  ModalResult := mrCancel;
//
//  BUF_SIZE = 2048; // Buffer size for reading the output in chunks
//
//end.          
//  FLock.Lock;
//  try
//    for s in FIncoming do
//      log(s);
//    FIncoming.clear;
//  finally
//    FLock.unlock;
//  end;
//    //
////    procedure processOutput(s : String);       
//  if (s.contains('---completed ok---')) then
//    ModalResult := mrOk;
//
//
//procedure TInstallerThread.processOutput(text : String);
//var
//  curr, s : String;
//begin
//  curr := FCarry + text;
//  while curr.contains(#13#10) do
//  begin
//    StringSplit(curr, #13#10, s, curr);
//    fform.processOutput(s);
//  end;
//  FCarry := curr;
//end;
//
//  FIncoming.free;

//
//procedure TInstallProgressForm.processOutput(s: String);
//begin
//  FLock.Lock;
//  try
//    FIncoming.add(s);
//  finally
//    FLock.unlock;
//  end;
//end;
//
