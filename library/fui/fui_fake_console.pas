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
  Menus, IniFiles,
  fsl_utilities, fsl_logging, fsl_threads,
  fui_fake_console_settings;

const
  MAX_MEMO_LINE_COUNT = 200;

type
  TFakeConsoleForm = class;

  { TFakeConsoleListener }

  TFakeConsoleListener = class (TLogListener)
  private
    FLine : String;
    FLines : TStringList;
    FLock : TFslLock;
    FFinished : boolean;
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
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    mnuStop: TMenuItem;
    mnuApple: TMenuItem;
    MenuItem7: TMenuItem;
    mnuPreferences: TMenuItem;
    Timer1: TTimer;
    FLogger : TFakeConsoleListener;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure mnuPreferencesClick(Sender: TObject);
    procedure mnuStopClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FListener: TFakeConsoleListener;
    FOnStop: TNotifyEvent;
    FStarted : boolean;
    FForwards : boolean;
    FAutoClose : boolean;
    FOp: TWorkProcedure;
    FIni : TIniFile;
    FCache : TStringList;
    FWorkingLine : integer;
    FMaxLines : integer;
    procedure start;
  public
    property Listener : TFakeConsoleListener read FListener;
    property Op : TWorkProcedure read FOp write FOp;
    property OnStop : TNotifyEvent read FOnStop write FOnStop;
  end;

var
  FakeConsoleForm: TFakeConsoleForm;

implementation

{$R *.lfm}

var
  GFinished : boolean;

{ TWorkerThread }

constructor TWorkerThread.Create(op: TWorkProcedure);
begin
  FOp := op;
  inherited Create(false);
end;

procedure TWorkerThread.execute;
begin
  try
    FOp;
    Logging.Log('Server Closed');
  except
    on e : Exception do
      Logging.Log('Server Failed: '+e.message);
  end;
  GFinished := true;
end;

{ TFakeConsoleListener }

constructor TFakeConsoleListener.Create;
begin
  inherited Create;
  FLines := TStringList.create;
  FLock := TFslLock.create;
  FFinished := true;
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
  FLock.Lock;
  try
    FLines.add(s);
  finally
    FLock.Unlock;
  end;
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
  FLock.Lock;
  try
    FLine := s;
    FFinished := false;
  finally
     FLock.Unlock;
   end;
end;

procedure TFakeConsoleListener.logContinue(s: String);
begin
  FLock.Lock;
  try
    FLine := FLine + s;
  finally
     FLock.Unlock;
   end;
end;

procedure TFakeConsoleListener.logFinish(s: String);
begin
  FLock.Lock;
  try
    FLine := FLine + s;
    FFinished := true;
  finally
     FLock.Unlock;
   end;
end;

{ TFakeConsoleForm }

procedure TFakeConsoleForm.FormCreate(Sender: TObject);
begin
  FListener := TFakeConsoleListener.create;
  Logging.addListener(FListener);
  mnuApple.caption := #$EF#$A3#$BF;
  mnuPreferences.caption := 'Preferences...';
  GFinished := false;
  FIni := TIniFile.create(FilePath([GetAppConfigDir(false), 'fhir_fake_console.ini']));
  FAutoClose := FIni.ReadBool('settings', 'autoclose', false);
  FForwards := FIni.ReadBool('settings', 'forwards', true);
  FMaxLines := FIni.ReadInteger('settings', 'maxlines', MAX_MEMO_LINE_COUNT);
  FCache := TStringList.create;
end;

procedure TFakeConsoleForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := GFinished;
end;

procedure TFakeConsoleForm.FormDestroy(Sender: TObject);
begin
  Logging.removeListener(FListener);
  FListener.free;
  FCache.Free;
end;

procedure TFakeConsoleForm.FormShow(Sender: TObject);
begin
  Timer1.Enabled := true;
end;

procedure TFakeConsoleForm.Memo1Change(Sender: TObject);
begin

end;

procedure TFakeConsoleForm.MenuItem4Click(Sender: TObject);
begin

end;

procedure TFakeConsoleForm.mnuPreferencesClick(Sender: TObject);
begin
  FakeConsoleSettingsForm := TFakeConsoleSettingsForm.create(self);
  try
    FakeConsoleSettingsForm.chkForwards.checked := FIni.ReadBool('settings', 'forwards', false);
    FakeConsoleSettingsForm.chkAutoClose.checked := FIni.ReadBool('settings', 'autoclose', false);
    FakeConsoleSettingsForm.eLines.text := inttostr(FMaxLines);
    if FakeConsoleSettingsForm.showModal = mrOK then
    begin
      FIni.WriteBool('settings', 'forwards', FakeConsoleSettingsForm.chkForwards.checked);
      FIni.WriteBool('settings', 'autoclose', FakeConsoleSettingsForm.chkAutoClose.checked);
      FIni.WriteInteger('settings', 'maxlines', StrToIntDef(FakeConsoleSettingsForm.eLines.text, MAX_MEMO_LINE_COUNT));
      FAutoClose := FIni.ReadBool('settings', 'autoclose', false);
      FForwards := FIni.ReadBool('settings', 'forwards', true);
      FMaxLines := FIni.ReadInteger('settings', 'maxlines', MAX_MEMO_LINE_COUNT);
    end;
  finally
    FakeConsoleSettingsForm.Free;
    FakeConsoleSettingsForm := nil;
  end;
end;

procedure TFakeConsoleForm.mnuStopClick(Sender: TObject);
begin
  if assigned(FOnStop) then
    FOnStop(self);
end;

procedure TFakeConsoleForm.Timer1Timer(Sender: TObject);
var
  l, s : String;
  f, b : boolean;
begin
  if not FStarted then
    start;
  FListener.FLock.Lock;
  try
    l := FListener.FLine;
    FCache.assign(FListener.FLines);
    FListener.FLines.clear;
    f := FListener.FFinished;
    if f then
      FListener.FLine := '';
  finally
    FListener.FLock.UnLock;
  end;
  if FForwards then
  begin
    while memo1.lines.count > FMaxLines do
      memo1.lines.delete(0);
    if (l <> '') then
    begin
      if FWorkingLine < 0 then
      begin
        FWorkingLine := memo1.lines.count;
        memo1.lines.add('');
      end;
      memo1.lines[FWorkingLine] := l;
    end;
    b := false;
    for s in FCache do
    begin
      memo1.lines.append(s);
      b := true;
    end;
    if (b) then
      memo1.selStart := length(memo1.text) - length(memo1.lines[memo1.lines.count-1]);
  end
  else
  begin
    while memo1.lines.count > FMaxLines do
      memo1.lines.delete(memo1.lines.count - 1);

    if (l <> '') then
    begin
      if FWorkingLine < 0 then
      begin
        FWorkingLine := 0;
        memo1.lines.insert(0, s)
      end;
      memo1.lines[FWorkingLine] := l;
    end;
    for s in FCache do
    begin
      memo1.lines.insert(0, s);
      if FWorkingLine >= 0 then
        inc(FWorkingLine);
    end;
  end;
  if f then
    FWorkingLine := -1;

  mnuStop.enabled := not GFinished;
  if GFinished and FAutoClose then
    Close;
end;

procedure TFakeConsoleForm.start;
begin
  FStarted := true;
  mnuStop.enabled := Assigned(FOnStop);
  TWorkerThread.create(FOp);
end;

end.

