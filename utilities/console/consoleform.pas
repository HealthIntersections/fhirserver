unit ConsoleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, IniFiles,
  IdTelnet, IdGlobal,
  FHIR.Support.Threads, FHIR.Support.Utilities, FHIR.Support.Logging;

const
   DEF_PASSWORD = 'AA8FF8CC-81C8-41D7-93BA-26AD5E89A1C1';

type
  TConnectionStatus = (csDiconnected, csUsername, csPassword, csConnected);

  { TConnectingThread }

  TConnectingThread = class (TFslThread)
  protected
    procedure execute; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    edtFilter: TEdit;
    ImageList1: TImageList;
    ListBox1: TListBox;
    mConsole: TMemo;
    Memo2: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Splitter1: TSplitter;
    sBar: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
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
    FLines : TStringList;
    FLastIncoming : TDateTime;
    FStatus : TConnectionStatus;
    FThread : TConnectingThread;
    FAddress : String;
    FPassword : String;
    FFilter : String;
    FIni : TIniFile;
    procedure DoIncoming(Sender: TIdTelnet; const Buffer: TIdBytes);
    procedure DoConnected(Sender: TObject);
    procedure processIncomingLine(line: String);
    function passesFilter(line: String) : boolean;
    procedure Connect;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  frmServerConnection;

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
  FTelnet.OnConnected := @DoConnected;
  FTelnet.OnDataAvailable := @DoIncoming;

  FStatus := csDiconnected;
  FIncoming := TStringList.create;
  FLines := TStringList.create;
  FLock := TFslLock.create('incoming');
  FThread := TConnectingThread.create;
  FThread.Open;
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

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FThread.Stop;
  while assigned(FThread) do
    ;
  FTelnet.Free;
  FIncoming.Free;
  FLines.Free;
  FLock.Free;
  FIni.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  ts : TStringList;
  s : String;
  st : TConnectionStatus;
begin
  ts := TStringList.create;
  try
    Flock.Lock;
    try
      st := FStatus;
      ts.assign(FIncoming);
      FIncoming.clear;
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
      csConnected:
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
  finally
    ts.free;
  end;
  if st = csDiconnected then
    sBar.Panels[1].Text := 'n/a'
  else
    sBar.Panels[1].Text := DescribePeriodNoMsec(now - FLastIncoming);
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

