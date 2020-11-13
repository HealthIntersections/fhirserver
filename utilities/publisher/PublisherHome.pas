unit PublisherHome;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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

{$I fhir.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, IniFiles,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.ImageList, System.AnsiStrings,
  Vcl.ImgList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ExtCtrls, Vcl.Clipbrd,
  JclSysUtils,
  fsl_base, fsl_utilities, fsl_stream, fsl_threads, fsl_shell,
 // FHIR.Tools.IGPublisher,
  Vcl.Buttons;

const
  UM_ENSURERESTORED = WM_USER + 1;

type
  TRunRecord = class (TFslObject)
  private
    FLast : String;
    FPrevious : String;
    FStart : TDateTime;
    FDuration : TDateTime;
  public
    constructor create(d : TDateTime);
  end;

  TPublisherForm = class;


  TPublisherForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ImageList1: TImageList;
    tbExecute: TToolButton;
    ToolButton3: TToolButton;
    lbFolders: TListBox;
    Panel3: TPanel;
    memOutput: TMemo;
    fd: TFileOpenDialog;
    pnlFolder: TPanel;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    lblFolder: TLabel;
    SpeedButton1: TSpeedButton;
    od: TFileOpenDialog;
    procedure ToolButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tbExecuteClick(Sender: TObject);
    procedure lbFoldersDblClick(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FIni : TIniFile;
    FRecord : TRunRecord;
    FLock : TFslLock;
    FRuns : TFslMap<TRunRecord>;
    FQueue : TStringList;
    FCompare : String;
    FirstShow:boolean;
//    FPublisher : TFHIRIGPublisher;
    procedure addFolder(dir : String; run : boolean);
    procedure cmdOutput(const Text: string);
    procedure saveList;
    procedure finish(c : Cardinal);
    procedure start(sf : String);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
    procedure UMEnsureRestored(var Msg: TMessage); message UM_ENSURERESTORED;
  public
    IGtoPublish:string;
  end;

var
  PublisherForm: TPublisherForm;

implementation

{$R *.dfm}

procedure TPublisherForm.cmdOutput(const Text: string);
begin
  FLock.Lock;
  try
    FQueue.Add(text);
  finally
    FLock.Unlock;
  end;
end;

procedure TPublisherForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  StrCopy(Params.WinClassName, 'org.hl7.fhir.ig-publisher-wrapper');
end;

procedure TPublisherForm.finish(c: Cardinal);
begin
  Timer1Timer(nil);
  if (c = 0) then
    FRecord.FDuration := now - FRecord.FStart;
  FRecord.FLast := memOutput.Text;
  ProgressBar1.Visible := false;
  tbExecute.ImageIndex := 1;
  FRecord := nil;
  saveList;
end;

procedure TPublisherForm.addFolder(dir: String; run : boolean);
var
  s : String;
begin
  s := ExtractFileName(dir) + ' : '+dir;
  if (lbFolders.Items.IndexOf(s) > -1) then
    lbFolders.Items.Delete(lbFolders.Items.IndexOf(s));
  lbFolders.Items.Insert(0, s);
  saveList;
  lbFolders.ItemIndex := 0;
  if (run) then
    tbExecuteClick(nil);
end;

procedure TPublisherForm.FormCreate(Sender: TObject);
var
  s : String;
  i : integer;
begin
  FIni := TIniFile.Create(Path([ExtractFilePath(ParamStr(0)), 'fhir-publisher.ini']));
  FIni.ReadSection('folders', lbFolders.Items);
//  FPublisher := TFHIRIGPublisher.Create(FIni);

  Left := FIni.ReadInteger('window', 'left', Left);
  Top := FIni.ReadInteger('window', 'top', Top);
  ClientHeight := FIni.ReadInteger('window', 'height', ClientHeight);
  ClientWidth := FIni.ReadInteger('window', 'width', ClientWidth);
  panel2.Width := FIni.ReadInteger('window', 'split', panel2.Width);
  Fini.WriteDateTime('status', 'last-start', now);
  lbFolders.ItemIndex := 0;
  FRuns := TFslMap<TRunRecord>.create('Publisher');
  for s in lbFolders.Items do
    FRuns.Add(s, TRunRecord.Create(FIni.ReadFloat('folders', s, 0)));
  FLock := TFslLock.Create('msg-queue');
  FQueue := TStringList.Create;
  FCompare := FIni.ReadString('tools', 'compare', 'C:\Program Files (x86)\WinMerge\WinMergeU.exe');
  for i := 1 to ParamCount do
    if FolderExists(ParamStr(i)) then
      addFolder(paramStr(i), true);
  firstShow:=true;

end;

procedure TPublisherForm.FormDestroy(Sender: TObject);
begin
  saveList;
  FRuns.Free;
  FIni.WriteInteger('window', 'left', Left);
  FIni.WriteInteger('window', 'top', Top);
  FIni.WriteInteger('window', 'height', ClientHeight);
  FIni.WriteInteger('window', 'width', ClientWidth);
  FIni.WriteInteger('window', 'split', panel2.Width);

  FIni.Free;
  FQueue.Free;
  FLock.Free;
end;

procedure TPublisherForm.FormShow(Sender: TObject);
begin
  if FirstShow then
  begin
    FirstShow:=False;
    if (IGtoPublish <>'') and (directoryexists(IGtoPublish)) then
      addFolder(IGtoPublish, true);

//  FJarFile := FIni.ReadString('tools', 'jar', ''); // 'C:\work\org.hl7.fhir\latest-ig-publisher\org.hl7.fhir.publisher.jar');
//  if fileExists(IGtoPublish+'\input-cache\org.hl7.fhir.publisher.jar')
//    then FjarFile:= IGtoPublish+'\input-cache\org.hl7.fhir.publisher.jar';
//  if (not FileExists(FJarFile)) then
//  begin
//    if not (od.Execute) then
//      exit;
//    FJarFile := od.FileName;
//    FIni.writeString('tools', 'jar', FJarFile);
//  end;
//

//  if not FPublisher.Configure(self) then
// exit;
  end;
end;

procedure TPublisherForm.lbFoldersDblClick(Sender: TObject);
begin
  tbExecuteClick(self);
end;

procedure TPublisherForm.saveList;
var
  s : String;
begin
  FIni.EraseSection('folders');
  for s in lbFolders.Items do
    if FRuns.ContainsKey(s) then
      Fini.WriteFloat('folders', s, FRuns[s].FDuration)
    else
      Fini.WriteFloat('folders', s, 0);
end;

procedure TPublisherForm.SpeedButton1Click(Sender: TObject);
begin
  Clipboard.AsText := lblFolder.Caption;
end;

procedure TPublisherForm.start(sf : String);
begin
  memOutput.clear;
  FQueue.Clear;
  ProgressBar1.Visible := FRecord.FDuration > 0;
  FRecord.FStart := now;
  tbExecute.ImageIndex := 3;
  lblFolder.Caption := '  '+sf;
end;

procedure TPublisherForm.Timer1Timer(Sender: TObject);
begin
  Application.processmessages;
  FLock.Lock;
  try
    if ({FThread <> nil) and (}FQueue.Count > 0) then
    begin
      memOutput.Lines.AddStrings(FQueue);
      FQueue.Clear;
      SendMessage(memOutput.Handle, EM_LINESCROLL, 0, memOutput.Lines.Count);
      if (FRecord.FDuration > 0) then
        ProgressBar1.Position := trunc((now - FRecord.FStart) / FRecord.FDuration * 100);
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TPublisherForm.ToolButton1Click(Sender: TObject);
begin
  if fd.Execute then
    addFolder(fd.FileName, true);
end;

procedure TPublisherForm.ToolButton2Click(Sender: TObject);
var
  s : String;
begin
  s := lbFolders.Items[lbFolders.ItemIndex];
  s := s.Substring(s.IndexOf(':')+1).trim;
//  Clipboard.AsText := 'java -jar '+FPublisher.Location+' -ig '+s;
  IGtoPublish:=s;
end;

procedure TPublisherForm.ToolButton5Click(Sender: TObject);
var
  s : String;
  a : AnsiString;
  r : TRunRecord;
begin
  s := lbFolders.Items[lbFolders.ItemIndex];
  if FRuns.ContainsKey(s) then
  begin
    r := FRuns[s];
    StringToFile(r.FPrevious, tempFile('previous.txt'), TEncoding.UTF8);
    StringToFile(r.FLast, tempFile('current.txt'), TEncoding.UTF8);
    a := FCompare+' '+tempFile('previous.txt')+' '+tempFile('current.txt');
    WinExec(pAnsiChar(a), SW_Normal);
  end;
end;

procedure TPublisherForm.ToolButton6Click(Sender: TObject);
begin
  ExecuteLaunch('open', FIni.FileName);
end;

procedure TPublisherForm.ToolButton7Click(Sender: TObject);
var
  s : String;
begin
  s := lbFolders.Items[lbFolders.ItemIndex];
  s := s.Substring(s.IndexOf(':')+1).trim;
  ExecuteLaunch('open', Path([s, 'output', 'index.html']));
end;

procedure TPublisherForm.tbExecuteClick(Sender: TObject);
var
  sl, sf : String;
  jarPath:string;
begin
  FormShow(self);

//  if FThread <> nil then
//    FThread.Fabort := true
//  else
//  begin
//<<<<<<< .mine
//
//
//=======
//
//
//>>>>>>> .theirs
//    sl := lbFolders.Items[lbFolders.ItemIndex];
//    if not FRuns.ContainsKey(sl) then
//      FRuns.Add(sl, TRunRecord.create(0));
//    FRecord := FRuns[sl];
//    sf := sl.Substring(sl.IndexOf(':')+1).trim;
//    addFolder(sf, false);
//
//    IGtoPublish:=sf;
//    if fileExists(ExtractFileDir(ExcludeTrailingBackslash(IGtoPublish))+'\org.hl7.fhir.publisher.jar')
//      then FjarFile:= ExtractFileDir(ExcludeTrailingBackslash(IGtoPublish))+'\org.hl7.fhir.publisher.jar';
//    if fileExists(IGtoPublish+'\input-cache\org.hl7.fhir.publisher.jar')
//      then FjarFile:= IGtoPublish+'\input-cache\org.hl7.fhir.publisher.jar';
//
//    FRecord.FPrevious := FRecord.FLast;
//    start(sf);
//  end;
end;

procedure TPublisherForm.UMEnsureRestored(var Msg: TMessage);
begin
  if IsIconic(Application.MainForm.Handle) then
    Application.Restore;
  if not Visible then
    Visible := True;
  Application.BringToFront;
  SetForegroundWindow(Self.Handle);
end;

procedure TPublisherForm.WMCopyData(var Msg: TWMCopyData);
var
  PData: PChar;  // walks thru data
  Param: string; // a parameter
begin
  if Msg.CopyDataStruct.dwData = 2342342334 then
  begin
    PData := Msg.CopyDataStruct.lpData;
    Param := PData;
    addFolder(Param, true);
    Msg.Result := 1;
  end;
end;

{ TRunRecord }

constructor TRunRecord.create(d: TDateTime);
begin
  inherited Create;
  FDuration := d;
end;

end.
