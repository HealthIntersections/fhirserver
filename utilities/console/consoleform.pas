unit ConsoleForm;

{
Copyright (c) 2020+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Menus, ActnList, StdActns, IniFiles, Math,
  IdTelnet, IdGlobal,
  FHIR.Support.Base, FHIR.Support.Threads, FHIR.Support.Utilities, FHIR.Support.Logging;

const
   DEF_PASSWORD = 'AA8FF8CC-81C8-41D7-93BA-26AD5E89A1C1';

type
  TConnectionStatus = (csDiconnected, csUsername, csPassword, csConnected, csEnhanced);

  { TConnectingThread }

  TConnectingThread = class (TFslThread)
  protected
    procedure execute; override;
  end;

  { TServerSession }

  TServerSession = class (TFslObject)
  private
    FLog : TStringList;
    FStart : int64;
    FLocal : TDateTime;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TServerSessionStatistics }

  TServerSessionStatistics = class (TFslObject)
  private
    FCursor : integer;
    FStarts : array [0..1000] of int64;
    FLengths : array [0..1000] of int64;
    FTotal : integer;
    FLength : int64;
    FCounts : array [0..9] of integer;
  public
    procedure recordSession(start, length : int64);
    function report : String;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    FileNewAction: TAction;
    ActionList1: TActionList;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditDelete1: TEditDelete;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    edtFilter: TEdit;
    FileExit1: TFileExit;
    FileOpenAction: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    HelpContents1: THelpContents;
    ImageList1: TImageList;
    lbSessions: TListBox;
    MainMenu1: TMainMenu;
    mConsole: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    N8: TMenuItem;
    MenuItem37: TMenuItem;
    N7: TMenuItem;
    MenuItem34: TMenuItem;
    N5: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    N6: TMenuItem;
    MenuItem27: TMenuItem;
    N4: TMenuItem;
    N3: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mStats: TMemo;
    mSession: TMemo;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    sBar: TStatusBar;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    TreeView1: TTreeView;
    procedure edtFilterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
  private
    FLock : TFslLock;
    FTelnet: TIdTelnet;
    FIncoming : TStringList;
    FServerStatus : String;
    FLines : TStringList;
    FSessions : TFslMap<TServerSession>;
    FStatistics : TServerSessionStatistics;
    FLastIncoming : TDateTime;
    FStatus : TConnectionStatus;
    FThread : TConnectingThread;
    FAddress : String;
    FPassword : String;
    FFilter : String;
    FIni : TIniFile;
    procedure recordSessionLength(start, length : int64);
    procedure DoIncoming(Sender: TIdTelnet; const Buffer: TIdBytes);
    procedure DoConnected(Sender: TObject);
    procedure handleSession(line: String);
    procedure processIncomingLine(line: String);
    function passesFilter(line: String) : boolean;
    function handleCommand(line: String) : boolean;
    procedure Connect;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  frmServerConnection;

{ TServerSessionStatistics }

procedure TServerSessionStatistics.recordSession(start, length: int64);
begin
  inc(FCursor);
  if (FCursor = 1000) then
    FCursor := 0;
  FStarts[FCursor] := start;
  FLengths[FCursor] := length;
  inc(FTotal);
  inc(FLength, length);
  if      (length <=   100) then inc(FCounts[0])
  else if (length <=   500) then inc(FCounts[1])
  else if (length <=  1000) then inc(FCounts[2])
  else if (length <=  2000) then inc(FCounts[3])
  else if (length <=  4000) then inc(FCounts[4])
  else if (length <=  8000) then inc(FCounts[5])
  else if (length <= 16000) then inc(FCounts[6])
  else if (length <= 32000) then inc(FCounts[7])
  else if (length <= 48000) then inc(FCounts[8])
  else {if (length <= 100) then} inc(FCounts[9])
end;

function TServerSessionStatistics.report: String;
var
  i, t, c : integer;
  latest, span, length : int64;

begin
  result := 'Total Requests: '+inttostr(FTotal)+#13#10;
  if (FTotal > 0) then
  begin
    result := result + 'Avg Length: '+FloatToStr(FLength / FTotal)+'ms'+#13#10;
    t := FCursor;
    latest := FStarts[t];
    length := 0;
  end;
  if (FTotal > 1) then
  begin
    c := Math.Min(20, FTotal);
    result := result + 'Last '+inttostr(c)+' Requests: '+#13#10;
    for i := 1 to c do
    begin
      length := length + FLengths[t];
      span := latest - FStarts[t];
      dec(t);
      if t < 0 then
        t := 999;
    end;
    result := result + '  Frequency: '+FloatToStr((c * 1000) / span )+'hz'+#13#10;
    result := result + '  Avg Length: '+FloatToStr(length / c)+'ms'+#13#10;
  end;
  result := result + #13#10+'Histogram (seconds): '+#13#10;
  result := result + '  0 - 0.1: '+inttostr(FCounts[0])+#13#10;
  result := result + '0.1 - 0.5: '+inttostr(FCounts[1])+#13#10;
  result := result + '0.5 - 1.0: '+inttostr(FCounts[2])+#13#10;
  result := result + '  1 -  2: '+inttostr(FCounts[3])+#13#10;
  result := result + '  2 -  4: '+inttostr(FCounts[4])+#13#10;
  result := result + '  4 -  8: '+inttostr(FCounts[5])+#13#10;
  result := result + '  8 - 16: '+inttostr(FCounts[6])+#13#10;
  result := result + ' 16 - 32: '+inttostr(FCounts[7])+#13#10;
  result := result + ' 32 - 48: '+inttostr(FCounts[8])+#13#10;
  result := result + ' >48    : '+inttostr(FCounts[9])+#13#10;
end;

{ TServerSession }

constructor TServerSession.Create;
begin
  inherited Create;
  FLog := TStringList.create;
  FLocal := Now;
end;

destructor TServerSession.Destroy;
begin
  FLog.Free;
  inherited Destroy;
end;

{ TConnectingThread }

procedure TConnectingThread.execute;
begin
  while Active do
  begin
    try
      Form1.Connect;
    except
    end;
    sleep(50);
  end;
  Form1.FThread := nil;
  free;
end;


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FIni := TIniFile.create('FHIRConsole.ini');
  FAddress := FIni.ReadString('console', 'address', 'Localhost');
  FPassword := FIni.ReadString('console', 'password', DEF_PASSWORD); // this password only works from localhost

  FTelnet := TIdTelnet.create(nil);
  FTelnet.Port := 44123;
  FTelnet.ThreadedEvent := true;
  FTelnet.OnConnected := DoConnected;
  FTelnet.OnDataAvailable := DoIncoming;

  FStatus := csDiconnected;
  FIncoming := TStringList.create;
  FLines := TStringList.create;
  FSessions := TFslMap<TServerSession>.create('session map');
  FStatistics := TServerSessionStatistics.create;
  FLock := TFslLock.create('incoming');
  FThread := TConnectingThread.create;
  FThread.Open;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FThread.Stop;
  while assigned(FThread) do
    ;
  FTelnet.Free;
  FIncoming.Free;
  FLines.Free;
  FSessions.Free;
  FStatistics.Free;
  FLock.Free;
  FIni.Free;
end;

procedure TForm1.edtFilterChange(Sender: TObject);
var
  s : String;
begin
  FFilter := edtFilter.Text;
  FFilter := FFilter.ToLower;
  mConsole.lines.BeginUpdate;
  try
    mConsole.Lines.clear;
    for s in FLines do
      if passesFilter(s) then
        mConsole.Lines.add(s);
  finally
    mConsole.Lines.EndUpdate;
  end;
  mConsole.SelStart := mConsole.Lines.Text.Length-1;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  ts, tsl, tsd : TStringList;
  s, ss, rs : String;
  st : TConnectionStatus;
  id : String;
  session : TServerSession;
  i : integer;
  d : TDateTime;
begin
  if lbSessions.ItemIndex = -1 then
    id := ''
  else
    id := lbSessions.Items[lbSessions.ItemIndex];

  ts := TStringList.create;
  tsl := TStringList.create;
  tsd := TStringList.create;
  try
    d := 0;
    Flock.Lock;
    try
      st := FStatus;
      ss := FServerStatus;
      ts.assign(FIncoming);
      FIncoming.clear;

      for s in FSessions.SortedKeys do
        tsl.add(s);
      if (id <> '') and FSessions.ContainsKey(id) then
      begin
        tsd.Assign(FSessions[id].FLog);
        d := FSessions[id].FLocal;
      end;
      rs := FStatistics.report;
    finally
      FLock.Unlock;
    end;
    case st of
      csDiconnected :
        begin
          mConsole.Color := $00EFEFEF;
          Caption := 'FHIR Console - Connecting to '+FAddress;
          sBar.Panels[0].Text := 'Connecting';
        end;
      csUsername :
        begin
          mConsole.Color := $00EFFFEF;
          Caption := 'FHIR Console - Authenticating to '+FAddress;
          sBar.Panels[0].Text := 'Authenticating (U)';
        end;
      csPassword :
        begin
          mConsole.Color := $00EFFFFF;
          Caption := 'FHIR Console - Authenticating to '+FAddress;
          sBar.Panels[0].Text := 'Authenticating (P)';
        end;
      csConnected, csEnhanced:
        begin
          mConsole.Color := $00FFFFFF;
          Caption := 'FHIR Console - Connected to '+FAddress;
          sBar.Panels[0].Text := 'Connected';
          if not FTelnet.connected then
          begin
            Flock.Lock;
            try
              FStatus := csDiconnected;
            finally
              FLock.Unlock;
            end;
          end;
        end;
    end;

    lbSessions.Items.assign(tsl);
    lbSessions.ItemIndex := lbSessions.items.indexOf(id);
    if lbSessions.ItemIndex > -1 then
    begin
      mSession.lines.assign(tsd);
      if (d <> 0) then
        mSession.lines.add(DescribePeriod(now - d));
    end
    else
      mSession.lines.Clear;

    for s in ts do
    begin
      FLines.add(s);
      if passesFilter(s) then
        mConsole.lines.add(s);
    end;
    while (mConsole.lines.count > 1000) and (edtFilter.Text = '') do
    begin
      FLines.delete(0);
      mConsole.lines.delete(0);
    end;
    mConsole.SelStart := mConsole.Lines.Text.Length-1;
    mStats.Text := rs;
  finally
    ts.free;
    tsl.free;
    tsd.free;
  end;
  if st = csDiconnected then
  begin
    sBar.Panels[1].Text := 'n/a';
    if (ss = '') then
      sBar.Panels[4].Text := ''
    else
      sBar.Panels[4].Text := 'Last Server: '+ss;
  end
  else
  begin
    sBar.Panels[1].Text := DescribePeriodNoMsec(now - FLastIncoming);
    sBar.Panels[4].Text := 'Server: '+ss;
  end;
  sBar.Panels[2].Text := inttostr(mConsole.lines.count) + ' '+StringPlural('Line', mConsole.lines.count);
  sBar.Panels[3].Text := MemoryStatus;
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
  mConsole.lines.clear;
end;

procedure TForm1.ToolButton3Click(Sender: TObject);
begin
  ServerConnectionForm.edtServer.Text := FAddress;
  ServerConnectionForm.edtPassword.Text := FPassword;
  if ServerConnectionForm.ShowModal = mrOk then
  begin
    FAddress := ServerConnectionForm.edtServer.Text;
    FPassword := ServerConnectionForm.edtPassword.Text;
    FIni.WriteString('console', 'address', FAddress);
    if FPassword = '' then
      FPassword := DEF_PASSWORD;
    FIni.WriteString('console', 'password', FPassword);
    FLines.clear;
    mConsole.Lines.Clear;
    FTelnet.Disconnect;
    FStatus := csDiconnected;
  end;
end;

procedure TForm1.recordSessionLength(start, length: int64);
begin
  FStatistics.recordSession(start, length);
end;

function ignoreLine(s : String) : boolean;
var
  ch : char;
begin
  s := trim(s);
  if (s = '') or (s = '.') or (s = 'console') then
    result := true
  else
  begin
    result := true;
    for ch in s do
      if ch <> '*' then
         exit(false);
  end;
end;

procedure TForm1.processIncomingLine(line : String);
var
  reply : String;
begin
  reply := '';
  FLock.Lock;
  try
    case FStatus of
      csDiconnected :
        FIncoming.add('!!'+line); // this is a timing issue if it does happen but it should not
      csUsername:
        if (line = 'Username: ') then
        begin
          reply := 'console';
          FStatus := csPassword;
        end
        else
          ; // ignore line
      csPassword:
        if (line = 'Password: ') then
        begin
          reply := FPassword;
          FStatus := csConnected;
          FIncoming.add('----------------------------------------------------------');
        end
        else
          ; // ignore line
      csConnected:
        begin
          if not handleCommand(line) then
            if not ignoreLine(line) then
              FIncoming.add(line);
          reply := '@console';
          FStatus := csEnhanced;
        end;
      csEnhanced:
        if not handleCommand(line) then
          if not ignoreLine(line) then
            FIncoming.add(line);
    end;
  finally
    FLock.Unlock;
  end;
  if (reply <> '') then
    FTelnet.SendString(reply+#10);
end;

function TForm1.passesFilter(line: String): boolean;
begin
  result := (FFilter = '') or line.ToLower.Contains(FFilter);
end;

procedure TForm1.handleSession(line: String);
var
  id, cmd, t, msg : String;
  session : TServerSession;
begin
  id := line.Substring(0, line.IndexOf(':')).trim();
  cmd := line.Substring(line.IndexOf(':') + 1).trim();
  if cmd.startsWith('@stop|') then
  begin
    if FSessions.ContainsKey(id) then
    begin
      t := cmd.Substring(6);
      recordSessionLength(FSessions[id].FStart, StrToInt64(t) - FSessions[id].FStart);
      FSessions.Remove(id);
    end;
  end
  else if cmd.StartsWith('@start|') then
  begin
    session := TServerSession.create;
    FSessions.AddOrSetValue(id, session);
    line := cmd.Substring(7);
    t := line.Substring(0, line.IndexOf('|')).trim();
    msg := line.Substring(line.IndexOf('|') + 1).trim();
    session.FStart := StrToInt64(t);
    session.FLog.add(msg);
  end
  else if cmd.StartsWith('@msg|') and FSessions.ContainsKey(id) then
  begin
    session := FSessions[id];
    session.FLog.add(cmd.Substring(5));
  end;
end;

function TForm1.handleCommand(line: String): boolean;
begin
  result := false;
  if (line.startsWith('$@')) then
  begin
    if line.startsWith('$@ping: ') then
    begin
      FLock.Lock;
      try
        FServerStatus := line.Substring(8);
      finally
        FLock.unLock;
      end;
      exit(true);
    end;
    if line.startsWith('$@session-') then
    begin
      FLock.Lock;
      try
        handleSession(line.Substring(10));
      finally
        FLock.unLock;
      end;
      exit(true);
    end;
  end;
end;

procedure TForm1.Connect;
begin
  if FStatus = csDiconnected then
  begin
    FTelnet.Host := FAddress;
    FTelnet.Connect;
  end;
end;

procedure TForm1.DoIncoming(Sender: TIdTelnet; const Buffer: TIdBytes);
var
  s : String;
  ts : TStringList;
begin
  FLastIncoming := now;
  ts := TStringList.create;
  try
    ts.text := TEncoding.UTF8.GetAnsiString(buffer);
    for s in ts do
      processIncomingLine(s);
  finally
    ts.free;
  end;
end;

procedure TForm1.DoConnected(Sender: TObject);
begin
  FLock.Lock;
  try
    FStatus := csUsername;
  finally
    FLock.Unlock;
  end;
end;

end.

