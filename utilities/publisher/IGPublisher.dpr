program IGPublisher;

uses
  WinApi.Windows,
  Winapi.Messages,
  SysUtils,
  Vcl.Forms,
  PublisherHome in 'PublisherHome.pas' {PublisherForm},
  FHIR.Tools.IGPublisher in '..\..\library\tools\FHIR.Tools.IGPublisher.pas';

{$R *.res}

function SendParamsToPrevInst(Wdw: HWND): Boolean;
var
  CopyData: TCopyDataStruct;
  I: Integer;
  CharCount: Integer;
  Data: PChar;
  PData: PChar;
begin
  CharCount := 0;
  for I := 1 to ParamCount do
    Inc(CharCount, Length(ParamStr(I)) + 1);
  Inc(CharCount);
  Data := StrAlloc(CharCount);
  try
    PData := Data;
    for I := 1 to ParamCount do
    begin
      StrPCopy(PData, ParamStr(I));
      Inc(PData, Length(ParamStr(I)) + 1);
    end;
    PData^ := #0;
    CopyData.lpData := Data;
    CopyData.cbData := CharCount * SizeOf(Char);
    CopyData.dwData := 2342342334;
    Result := SendMessage(Wdw, WM_COPYDATA, 0, LPARAM(@CopyData)
    ) = 1;
  finally
    StrDispose(Data);
  end;
end;

function SwitchToPrevInst(Wdw: HWND): Boolean;
begin
  Assert(Wdw <> 0);
  if ParamCount > 0 then
    Result := SendParamsToPrevInst(Wdw)
  else
    Result := True;
  if Result then
    SendMessage(Wdw, UM_ENSURERESTORED, 0, 0);
end;

function FindDuplicateMainWdw: HWND;
begin
  Result := FindWindow('org.hl7.fhir.ig-publisher-wrapper', nil);
end;

function CanStart: Boolean;
var
  Wdw: HWND;
begin
  Wdw := FindDuplicateMainWdw;
  if Wdw = 0 then
    // no instance running: we can start our app
    Result := True
  else
    // instance running: try to pass command line to it
    // terminate this instance if this succeeds
    // (SwitchToPrevInst routine explained later)
    Result := not SwitchToPrevInst(Wdw);
end;

begin
  if CanStart then
  begin
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TPublisherForm, PublisherForm);
  Application.Run;
  end;
end.
