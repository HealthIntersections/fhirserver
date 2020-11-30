unit IGPublishSettings;

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

interface

uses
{$IFDEF OSX}
{$ELSE}
  ShellApi, Winapi.Windows, FMX.Platform.Win, //JclSysUtils,
{$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.zip, System.IOUtils,
  FMX.Dialogs, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Types,
  FMX.dialogservice, IdHTTP, IdComponent, System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent, FDownloadForm, FMX.TabControl,
  FMX.ListBox;

type
  TIGPublishForm = class(TForm)
    Memo1: TMemo;
    EditFileNamex: TEdit;
    EditURLx: TEdit;
    Edit3x: TEdit;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    CheckBox1: TCheckBox;
    Button3: TButton;
    Button4: TButton;
    CheckBox2: TCheckBox;
    Edit5: TEdit;
    Button7: TButton;
    Button9: TButton;
    Button11: TButton;
    TabItem2: TTabItem;
    Button2: TButton;
    TabItem3: TTabItem;
    Button12: TButton;
    Edit2: TEdit;
    Label5: TLabel;
    Panel2: TPanel;
    Button10: TButton;
    btnCheckDependencies: TButton;
    lblJekyllAvailable: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    btnDownloadIGPublisher: TButton;
    btnLocalBuild: TButton;
    Memo2: TMemo;
    Label3: TLabel;
    ComboBox1: TComboBox;
    lblPubFolder: TLabel;
    Label6: TLabel;
    lblPubPresent: TLabel;
    Label8: TLabel;
    Button5: TButton;

    function CheckFolder(Sender: TObject):boolean;
    procedure Button1Click(Sender: TObject);
    procedure btnCheckDependenciesClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnDownloadIGPublisherClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    Procedure SetUpIGPublisherFiles;
    procedure Button11Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure HandleOutput(const Text: string);
    procedure RunInMemo(CommandLine: string; Work: string; parameters: string; Memo: TMemo);
    // procedure CaptureConsoleOutput(const WorkDir, ACommand, AParameters: String; AMemo: TMemo);
    procedure Edit1Exit(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    { //    procedure Download;
      procedure HttpWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
      procedure HttpWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
      procedure HttpWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    }

    /// ////////////////////
  private
    { Private declarations }

    { TotalBytes: Int64;    LastWorkCount: Int64;
      LastTicks: LongWord; }
    { FClient: THTTPClient; }
    FGlobalStart: Cardinal;
    FAsyncResult: IAsyncResult;
    FDownloadStream: TStream;
    isBuilding: Boolean;
  public
    { Public declarations }
    dependencies: Boolean;
    fwInstalled: Boolean;
    igRootFolder, IGPublisherFolder, IGFileName: String;
    depJekyllVersion: string;

    resourcesFolder, pagecontentfolder, igcontentfolder, mediafolder, pandocfolder, tempfolder: string;

    function runAndWait(Path, command, parameters: String): integer;
    function checkContentFolders(rootFolder: string): Boolean;
//    function checkPublishFolders(publisherFolder: string): Boolean;
    procedure createFolders(rootFolder: string);
    { procedure DownloadFile; }
  end;

var
  IGPublishForm: TIGPublishForm;
  buildOK: Boolean;
  svc: string = 'git';

implementation

{$R *.fmx}

uses
  fsl_base,
  ImplementationGuideEditor;




function TIGPublishForm.CheckFolder(Sender: TObject):boolean;
var tempstr:string;
begin
  btnLocalBuild.enabled := false;
  tempstr := IGPublisherFolder;
  if tempstr = '' then
  begin
    TDialogService.MessageDialog('IG Publisher folder not defined.'#13#10'Set the location of the IG Publisher', System.UITypes.TMsgDlgType.mtInformation, [System.UITypes.TMsgDlgBtn.mbOk],
      System.UITypes.TMsgDlgBtn.mbOk, 0, nil, nil);
    lblPubFolder.TextSettings.FontColor:= TAlphaColorRec.Maroon;
    lblPubFolder.Text:='Not found';
    exit;
  end else begin
    lblPubFolder.TextSettings.FontColor:= TAlphaColorRec.Green;
    lblPubFolder.Text:='Present';

  end;


  if not(fileexists(tempstr + '\BUILD.bat')) then
  begin
    TDialogService.MessageDialog('IG Publisher not found in folder.'#13#10'Please download the IG Publisher.', System.UITypes.TMsgDlgType.mtInformation, [System.UITypes.TMsgDlgBtn.mbOk],
      System.UITypes.TMsgDlgBtn.mbOk, 0, nil, nil);
    lblPubPresent.TextSettings.FontColor:= TAlphaColorRec.Maroon;
    lblPubPresent.Text:='Not found';
    exit;
  end
  else begin
    lblPubPresent.TextSettings.FontColor:= TAlphaColorRec.Green;
    lblPubPresent.Text:='Present';
    btnLocalBuild.enabled := true;
  end;


end;



procedure TIGPublishForm.btnCheckDependenciesClick(Sender: TObject);
{$IFDEF OSX}
begin
  raise EFslException.Create('Not done yet for OSX');
end;
{$ELSE}

var
  str: string;

begin

 checkfolder(self);




  Memo1.Lines.Clear;
// todo - doesn't compile...  execute('cmd.exe /C jekyll -v', str, true);
  if copy(str, 1, 6) = 'jekyll' then
  begin
    delete(str, 1, 7);
    depJekyllVersion := str;
  end
  else
    depJekyllVersion := 'Not Found';

  lblJekyllAvailable.Text := depJekyllVersion;



end;
{$ENDIF}


procedure TIGPublishForm.Button10Click(Sender: TObject);
var
  currDir, filestr, tempstr: string;
begin
  if isBuilding then
    exit;

  tempstr := IGPublisherFolder;
  if tempstr = '' then
  begin
    TDialogService.MessageDialog('IG Publisher folder not defined.'#13#10'Set the location of the IG Publisher', System.UITypes.TMsgDlgType.mtInformation, [System.UITypes.TMsgDlgBtn.mbOk],
      System.UITypes.TMsgDlgBtn.mbOk, 0, nil, nil);
    exit;
  end;

  if not(fileexists(tempstr + '\BUILD.bat')) then
  begin
    TDialogService.MessageDialog('IG Publisher not found in folder.'#13#10'Please download the IG Publisher.', System.UITypes.TMsgDlgType.mtInformation, [System.UITypes.TMsgDlgBtn.mbOk],
      System.UITypes.TMsgDlgBtn.mbOk, 0, nil, nil);
    exit;
  end

  else

    try
      if fileexists(tempstr + '\BUILD.bat') then
      begin
        btnLocalBuild.enabled := false;
        isBuilding := true;
        sleep(1000);
        SetupIGPublisherFiles;
        // runAndWait(tempstr, 'BUILD', igRootFolder);
        RunInMemo('cmd.exe /C BUILD', tempstr, igRootFolder, Memo2);
        // CaptureConsoleOutput(tempstr, 'cmd.exe /C BUILD', igRootFolder, Memo2);
        filestr := IGPublisherFolder + '\output\index.html';
        filestr := stringreplace(filestr, '\', '/', [rfReplaceAll, rfIgnoreCase]);
        if runAndWait(tempstr, filestr, '') = 0 then
          buildOK := true
        else
          buildOK := false;
        // close;
      end
      else
        TDialogService.MessageDialog('IG Publisher not found. '#13#10'Ensure the IG Publisher is in the same folder as the IG file.', System.UITypes.TMsgDlgType.mtInformation,
          [System.UITypes.TMsgDlgBtn.mbOk], System.UITypes.TMsgDlgBtn.mbOk, 0, nil, nil);
    finally
      btnLocalBuild.enabled := true;
      isBuilding := false;
    end;
end;

procedure TIGPublishForm.HandleOutput(const Text: string);
begin
  Memo2.Lines.Add(Text);
end;

procedure TIGPublishForm.Button11Click(Sender: TObject);
begin
  exit;
  if (directoryexists(pwidechar(igRootFolder + '\content'))) then
  begin
    igcontentfolder := igRootFolder + '\src';
    if (directoryexists(pwidechar(igcontentfolder + '\temp'))) then
      tempfolder := igcontentfolder + '\temp';
    if (directoryexists(pwidechar(igcontentfolder + '\pagecontent'))) then
      pagecontentfolder := igcontentfolder + '\pagecontent';
    if (directoryexists(pwidechar(igcontentfolder + '\resources'))) then
      resourcesFolder := igcontentfolder + '\resources';
    if (directoryexists(pwidechar(igcontentfolder + '\images'))) then
      mediafolder := igcontentfolder + '\images';
  end;

end;

procedure TIGPublishForm.Button13Click(Sender: TObject);
var
  cmd: string;
begin
  svc := 'git';
  cmd := 'show > nul';
  if runAndWait(igRootFolder, svc, cmd) <> 0 then
  begin
    TDialogService.MessageDialog('Repository not found', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbOk], TMsgDlgBtn.mbOk, 0,
      procedure(const AResult: TModalResult)
      begin
        exit;
      end);

    exit;
  end
  else
  begin
    cmd := 'pull';
    if runAndWait(igRootFolder, svc, cmd) = 0 then
    begin
    end
  end

end;

procedure TIGPublishForm.Button14Click(Sender: TObject);
var
  cmd: string;
begin
  svc := 'git';
  cmd := 'add .';
  if runAndWait(igRootFolder, svc, cmd) = 0 then
  begin
    cmd := 'commit';
    if runAndWait(igRootFolder, svc, cmd) = 0 then
    begin
    end

  end

end;

procedure TIGPublishForm.Button15Click(Sender: TObject);
var
  cmd: string;
begin
  svc := 'git';
  cmd := 'push';

  if runAndWait(igRootFolder, svc, cmd) = 0 then
  begin
  end

end;

Procedure TIGPublishForm.Button1Click(Sender: TObject);
var
  dir, folder: string;

begin
  if SelectDirectory('Select path to IG framework (contains License.md)', '', dir) then
  begin
    IGPublisherFolder := dir;
    Edit1.Text := dir;
  end;
  checkfolder(self);

end;

procedure TIGPublishForm.Button2Click(Sender: TObject);
var
  url, FileName: string;
  LStream: TFileStream;

begin
  // url := 'https://github.com/madhur/PortableJekyll/archive/master.zip';
  // url := 'https://github.com/costateixeira/ihe_mma/archive/master.zip';

end;

procedure TIGPublishForm.Button5Click(Sender: TObject);
begin
memo2.SelectAll;
memo2.CopyToClipboard;
Memo2.SelLength := 0;
Memo2.SelStart := Length(Memo2.Text) - 1;
Memo2.GoToTextEnd;

end;

procedure TIGPublishForm.RunInMemo(CommandLine: string; Work: string; parameters: string; Memo: TMemo);
{$IFDEF OSX}
begin
  raise EFslException.Create('Not done yet for OSX');
end;
{$ELSE}
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array [0 .. 255] of AnsiChar;
  BytesRead: Cardinal;
  WorkDir: string;
  Handle: Boolean;
begin
  Memo.Text := '';
  with SA do
  begin
    nLength := SizeOf(SA);
    bInheritHandle := true;
    lpSecurityDescriptor := nil;
  end;
  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  try
    with SI do
    begin
      FillChar(SI, SizeOf(SI), 0);
      cb := SizeOf(SI);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
      hStdOutput := StdOutPipeWrite;
      hStdError := StdOutPipeWrite;
    end;
    WorkDir := Work;
    Handle := CreateProcess(nil, PChar('cmd.exe /C ' + CommandLine + ' ' + parameters), nil, nil, true, 0, nil, PChar(WorkDir), SI, PI);
    CloseHandle(StdOutPipeWrite);
    if Handle then
      try
        repeat
          WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;
            memo.BeginUpdate;
            Memo.Text := Memo.Text + Buffer;
            Memo.SelLength := 0;
            Memo.SelStart := Length(Memo.Text) - 1;
            Memo.GoToTextEnd;
            memo.EndUpdate;
            Application.ProcessMessages();
            Memo.GoToTextEnd;
 //           memo.Dispatch();
          end;
        until not WasOK or (BytesRead = 0);
        WaitForSingleObject(PI.hProcess, INFINITE);
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;
{$ENDIF}

procedure TIGPublishForm.btnDownloadIGPublisherClick(Sender: TObject);
var
  DownloadForm: TDownloadForm;
begin

  if Edit1.Text = '' then
    TDialogService.MessageDialog('IG Publisher folder not specified. ' + #13#10 + 'Do you want to download the IG Publisher in the current work folder (' + igRootFolder + ')?.',
    // System.UITypes.TMsgDlgType.mtCustom, [System.UITypes.TMsgDlgBtn.mbYes,System.UITypes.TMsgDlgBtn.mbCancel], System.UITypes.TMsgDlgBtn.mbYes, 0,
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, 0,
      procedure(const AResult: TModalResult)
      begin
        case AResult of
          mrYes:
            Edit1.Text := igRootFolder + '\publish';
        end;
      end);

  if Edit1.Text <> '' then
  begin
    IGPublisherFolder := Edit1.Text;
    forceDirectories(pwidechar(IGPublisherFolder));
    // how to check whether the publisher is already there. ???

    DownloadForm := TDownloadForm.Create(self);
    DownloadForm.SourceURL := combobox1.Selected.Text;;
    DownloadForm.localFileName := IGPublisherFolder + '\publish.zip';
    DownloadForm.UnzipLocation := IGPublisherFolder;
    DownloadForm.Unzip := true;
    DownloadForm.ShowModal;

    Button11Click(nil);

    DownloadForm.close;
    DownloadForm.Destroy;

    createFolders(igRootFolder);
    edit1.Text:=IGPublisherFolder;

    if checkFolder(self) then
    begin

    end

  end;

end;

procedure TIGPublishForm.Button6Click(Sender: TObject);
begin
  // igRootFolder := Edit4.Text;
  // createFolders(Edit4.Text);

end;

function TIGPublishForm.checkContentFolders(rootFolder: string): Boolean;
begin
  igRootFolder := rootFolder;
  igcontentfolder := igRootFolder + '\content';
  tempfolder := igcontentfolder + '\temp';
  pagecontentfolder := igcontentfolder + '\pagecontent';
  resourcesFolder := igcontentfolder + '\resources';
  mediafolder := igcontentfolder + '\images';

  begin
    if ((directoryexists(pwidechar(igRootFolder))) and (directoryexists(pwidechar(igcontentfolder))) and (directoryexists(pwidechar(pagecontentfolder))) and
      (directoryexists(pwidechar(resourcesFolder))) and (directoryexists(pwidechar(mediafolder)))) then
      result := true
    else
      result := false;
  end;

  if result then
  begin
    // Button6.Text := 'IG Folders OK';
    // Button6.Enabled := False
  end
  else
  begin
    // Button6.Text := 'Create IG Folders';
    // Button6.Enabled := true
  end

end;


procedure TIGPublishForm.createFolders(rootFolder: string);
begin
  igRootFolder := rootFolder;
  igcontentfolder := igRootFolder + '\content';
  tempfolder := igcontentfolder + '\temp';
  pagecontentfolder := igcontentfolder + '\pagecontent';
  resourcesFolder := igcontentfolder + '\resources';
  mediafolder := igcontentfolder + '\images';

  if IGPublisherFolder = '' then IGPublisherFolder := igRootFolder + '\publish';
  Edit1.Text := IGPublisherFolder;
  pandocfolder := IGPublisherFolder + '\framework\pandoc';

  begin
    if not(directoryexists(pwidechar(igRootFolder))) then
      forceDirectories(pwidechar(igRootFolder));
    if not(directoryexists(pwidechar(igcontentfolder))) then
      forceDirectories(pwidechar(igcontentfolder));
    if not(directoryexists(pwidechar(pagecontentfolder))) then
      forceDirectories(pwidechar(pagecontentfolder));
    if not(directoryexists(pwidechar(resourcesFolder))) then
      forceDirectories(pwidechar(resourcesFolder));
    if not(directoryexists(pwidechar(mediafolder))) then
      forceDirectories(pwidechar(mediafolder));
  end;
  checkContentFolders(igRootFolder);



end;

procedure TIGPublishForm.Edit1Exit(Sender: TObject);
begin
  IGPublisherFolder := Edit1.Text;
  checkfolder(self);
end;

procedure TIGPublishForm.Button7Click(Sender: TObject);
var
  dir: string;
begin
  if SelectDirectory('Select path to Base IG template (contains folders "content", etc', '', dir) then
  begin
    IGPublisherFolder := dir;
    Edit5.Text := dir;
  end;
end;

procedure TIGPublishForm.Button8Click(Sender: TObject);
var
  dir: string;
begin
  if SelectDirectory('Select path to IG content (contains folders "content", etc.)', '', dir) then
  begin
    igRootFolder := dir;
    // Edit4.Text := dir;
  end;
  // if checkContentFolders(Edit4.Text) then
  // Button11Click(nil);

end;

procedure TIGPublishForm.Button9Click(Sender: TObject);
var
  DownloadForm: TDownloadForm;
begin

  DownloadForm := TDownloadForm.Create(nil);

  DownloadForm.localFileName := 'c:\temp\xxx.zip';
  DownloadForm.UnzipLocation := 'c:\temp\ttx';
  DownloadForm.Unzip := true;
  DownloadForm.show;
  DownloadForm.Destroy;

end;

procedure TIGPublishForm.FormShow(Sender: TObject);
begin
  Edit1.Text := IGPublisherFolder;

  if IGPublisherFolder <> '' then
    Edit1.Text := IGPublisherFolder;

  // if checkPublishFolders(Edit1.Text) then
  // Button10.Enabled := true;

  Button11Click(self);
end;

function TIGPublishForm.runAndWait(Path, command, parameters: String): integer;
{$IFDEF OSX}
begin
  raise EFslException.Create('Not done yet for OSX');
end;
{$ELSE}

var
  folderstr, filestr: string;
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
  sCmd, ExecuteFile, ParamString, StartInString: string;

begin
  folderstr := getCurrentDir;

  begin
    FillChar(SEInfo, SizeOf(SEInfo), 0);
    SEInfo.cbSize := SizeOf(TShellExecuteInfo);
    with SEInfo do
    begin
      fMask := SEE_MASK_NOCLOSEPROCESS;
      // Wnd := FmxHandleToHWND(ImplementationGuideEditor.Handle);
      lpFile := PChar(command);
      lpDirectory := PChar(Path);
      lpParameters := PChar(parameters);
      nShow := SW_SHOWNORMAL;
    end;
    if ShellExecuteEx(@SEInfo) then
    begin
      repeat
        // application.ProcessMessages;
        GetExitCodeProcess(SEInfo.hProcess, ExitCode);
      until (ExitCode <> STILL_ACTIVE) or Application.Terminated;
    end;
    result := ExitCode;

  end;

end;
{$ENDIF}

procedure TIGPublishForm.SetupIGPublisherFiles;
{$IFDEF OSX}
begin
  raise EFslException.Create('Not done yet for OSX');
end;
{$ELSE}
var
  str: string;
  i: integer;
  guidenamestr: string;
  parameter_line: integer;
  destFile: string;

  SA: TStringDynArray;

begin

  begin // Create & update properties.txt
    guidenamestr := 'guidename=' + ChangeFileExt(IGFileName, '');
    parameter_line := -1;
    try
      Memo1.Lines.LoadFromFile(IGPublisherFolder + '\src\properties.txt');
    except
      Memo1.Lines.Clear;
    end;
    if Memo1.Lines.Count > 0 then
      for i := 0 to Memo1.Lines.Count - 1 do
      begin
        str := Memo1.Lines[i];
        if copy(str, 0, 10) = 'guidename=' then
        begin
          parameter_line := i;
          Memo1.Lines[i] := guidenamestr;
        end;
      end;
    if parameter_line < 0 then
      Memo1.Lines.Add(guidenamestr);
    Memo1.Lines.SaveToFile(IGPublisherFolder + '\src\properties.txt');
  end;

  try
    tfile.delete(IGPublisherFolder + '\src\' + extractFileName(IGFileName));
  except
  end;
  tfile.copy(igRootFolder + '\' + IGFileName, IGPublisherFolder + '\src\' + extractFileName(IGFileName));

  begin
    SA := TDirectory.GetFiles(igcontentfolder + '\resources');
    for i := 0 to Length(SA) - 1 do
    begin
      destFile := IGPublisherFolder + '\src\resources\' + extractFileName(SA[i]);
      try
        if fileexists(pwidechar(destFile)) then tfile.delete(pwidechar(destFile));
      finally
        if forcedirectories(pwidechar(ExtractFilePath(destFile))) then tfile.copy(pwidechar(SA[i]), pwidechar(destFile));
      end;
    end;
  end;

  begin
    SA := TDirectory.GetFiles(igcontentfolder + '\pagecontent');
    for i := 0 to Length(SA) - 1 do
    begin
      destFile := IGPublisherFolder + '\src\pagecontent\' + extractFileName(SA[i]);
      try
        if fileexists(pwidechar(destFile)) then tfile.delete(pwidechar(destFile));
      finally
        if forcedirectories(pwidechar(ExtractFilePath(destFile))) then tfile.copy(pwidechar(SA[i]), pwidechar(destFile));
      end;
    end;
  end;

  begin
    SA := TDirectory.GetFiles(igcontentfolder + '\images');
    for i := 0 to Length(SA) - 1 do
    begin
      destFile := IGPublisherFolder + '\src\images\' + extractFileName(SA[i]);
      try
        if fileexists(pwidechar(destFile)) then tfile.delete(pwidechar(destFile));
      finally
        if forcedirectories(pwidechar(ExtractFilePath(destFile))) then tfile.copy(pwidechar(SA[i]), pwidechar(destFile));
      end;
    end;
  end;
end;
{$ENDIF}

end.
