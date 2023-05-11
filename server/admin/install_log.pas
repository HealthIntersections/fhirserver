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
  fsl_base, fsl_threads, fsl_utilities;

type
  TInstallProgressForm = class;

  { TInstallerThread }

  TInstallerThread = class (TFslThread)
  private
    FForm : TInstallProgressForm;
    FCarry : String;
    process : TProcess;
    procedure processOutput(text : String);
  protected
    procedure Execute; override;
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
    FIncoming : TStringList;
    FThread : TInstallerThread;
    FLock : TFslLock;

    procedure processOutput(s : String);
    procedure log(s : String);

  public
    property command : String read FCommand write FCommand;
  end;

var
  InstallProgressForm: TInstallProgressForm;

implementation

{$R *.lfm}

const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks

{ TInstallProgressForm }

procedure TInstallProgressForm.FormCreate(Sender: TObject);
begin
  FThread := TInstallerThread.create;
  FThread.FForm := self;
  FLock := TFslLock.create;
  FIncoming := TStringList.create;
  Timer1.Enabled := true;
end;

procedure TInstallProgressForm.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := false;
  FThread.StopAndWait(2000);
  FThread.Free;
  FIncoming.Free;
  FLock.Free;
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
  if FThread.process <> nil then
    FThread.process.Terminate(0);
  ModalResult := mrCancel;
end;

procedure TInstallProgressForm.processOutput(s: String);
begin
  FLock.Lock;
  try
    FIncoming.add(s);
  finally
    FLock.unlock;
  end;
end;

procedure TInstallProgressForm.Timer1Timer(Sender: TObject);
var
  s : String;
begin
  FLock.Lock;
  try
    for s in FIncoming do
      log(s);
    FIncoming.clear;
  finally
    FLock.unlock;
  end;
end;

procedure TInstallProgressForm.log(s: String);
begin
  Memo1.lines.Add(s);
  btnCopy.enabled := true;
  if (s.contains('---completed ok---')) then
    ModalResult := mrOk;
end;

{ TInstallerThread }

procedure TInstallerThread.processOutput(text : String);
var
  curr, s : String;
begin
  curr := FCarry + text;
  while curr.contains(#13#10) do
  begin
    StringSplit(curr, #13#10, s, curr);
    fform.processOutput(s);
  end;
  FCarry := curr;
end;

procedure TInstallerThread.Execute;
var
  BytesRead    : longint;
  Buffer       : TBytes;
  s : String;
begin
  fform.lblStatus.caption := 'Getting Ready';
  fform.processOutput('Running Server to do the install...');
  process := TProcess.create(nil);
  try
    process.Executable := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)))+'fhirserver.exe';
    for s in FForm.command.split([' ']) do
      process.Parameters.add(s);
    process.Options := [poUsePipes];
    process.ShowWindow := swoHIDE;
    process.Execute;
    fform.lblStatus.caption := 'Running';
    repeat
      SetLength(Buffer, BUF_SIZE);
      BytesRead := process.Output.Read(Buffer, BUF_SIZE);
      processOutput(TEncoding.UTF8.GetString(Buffer, 0, BytesRead));
    until BytesRead = 0;
  finally
    process.free;
  end;
  process := nil;
  fform.lblStatus.caption := 'Finished';
  fform.btnCancel.caption := 'Close';
  fform.processOutput('Server Process Terminated');
end;


end.

