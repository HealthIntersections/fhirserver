unit IGSettings;

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
  ShellApi, Winapi.Windows, FMX.Platform.Win, JclSysUtils,
{$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.zip, System.IOUtils,
  FMX.Dialogs, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Types,
  FMX.dialogservice, IdHTTP, IdComponent, System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent, FDownloadForm, FMX.TabControl;

type
  TIGSettingsForm = class(TForm)
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
    Panel1: TPanel;
    TabItem3: TTabItem;
    Button12: TButton;
    Edit2: TEdit;
    Label5: TLabel;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Panel2: TPanel;
    Button10: TButton;
    btnCheckDependencies: TButton;
    Label4: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Button5: TButton;
    Button6: TButton;
    Button8: TButton;
    Edit4: TEdit;
    Label3: TLabel;

    procedure Button1Click(Sender: TObject);
    procedure btnCheckDependenciesClick(Sender: TObject);
    procedure OnZipProgressEvent(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure SetupIGPublisherFiles;
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);

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
  public
    { Public declarations }
    dependencies: boolean;
    fwInstalled: boolean;
    igRootFolder, IGPublisherFolder, IGFileName, BaseTemplateFolder: String;

    resourcesFolder, pagecontentfolder, igcontentfolder, mediafolder, pandocfolder, tempfolder: string;

    function runAndWait(Path, command, parameters: String): integer;
    function checkContentFolders(rootFolder: string): boolean;
    function checkPublishFolders(publisherFolder: string): boolean;
    procedure createFolders(rootFolder: string);
    { procedure DownloadFile; }
  end;

var
  IGSettingsForm: TIGSettingsForm;
  buildOK: boolean;
  svc: string = 'git';

implementation

{$R *.fmx}

uses
  FHIR.Support.Base,
  ImplementationGuideEditor;

procedure TIGSettingsForm.btnCheckDependenciesClick(Sender: TObject);
{$IFDEF OSX}
begin
  raise EFslException.Create('Not done yet for OSX');
end;
{$ELSE}

var
  str: string;

begin
  Memo1.Lines.Clear;

  TDialogService.MessageDialog('Not Implemented yet', System.UITypes.TMsgDlgType.mtInformation, [System.UITypes.TMsgDlgBtn.mbOk], System.UITypes.TMsgDlgBtn.mbOk, 0, nil, nil);
  exit;
  execute('cmd.exe /C jekyll -v', str, true);

  // try
  // TZipFile.ExtractZipFile('C:\temp\xxx.zip', 'C:\temp\xxxunzipped')
  // except
  // end;

  Memo1.Lines.Add(str);
end;
{$ENDIF}

procedure TIGSettingsForm.Button10Click(Sender: TObject);
var
  filestr, tempstr: string;
begin

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

    if fileexists(tempstr + '\BUILD.bat') then
  begin
    sleep(2000);
    SetupIGPublisherFiles;
    runAndWait(tempstr, 'BUILD', igRootFolder);
    filestr := IGPublisherFolder + '\output\index.html';
    filestr := stringreplace(filestr, '\', '/', [rfReplaceAll, rfIgnoreCase]);
    if runAndWait(tempstr, filestr, '') = 0 then
      buildOK := true
    else
      buildOK := False;
    // filestr := stringreplace(filestr, '\', '/', [rfReplaceAll, rfIgnoreCase]);
    // runAndWait(igRootFolder, 'open', filestr);
    close;
  end
  else
    TDialogService.MessageDialog('IG Publisher not found. '#13#10'Ensure the IG Publisher is in the same folder as the IG file.', System.UITypes.TMsgDlgType.mtInformation,
      [System.UITypes.TMsgDlgBtn.mbOk], System.UITypes.TMsgDlgBtn.mbOk, 0, nil, nil);
end;

procedure TIGSettingsForm.Button11Click(Sender: TObject);
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

  { if (directoryexists(pwidechar(igRootFolder + '\publish'))) then  ssssssss
    begin
    IGPublisherFolder := igRootFolder + '\publish';  sssssssss
    Edit1.Text := IGPublisherFolder;
    if (directoryexists(pwidechar(IGPublisherFolder + '\framework\pandoc'))) then
    pandocfolder := IGPublisherFolder + '\framework\pandoc';
    end;
  }

end;

procedure TIGSettingsForm.Button12Click(Sender: TObject);
begin
  // IGPublisherFolder := igRootfolder+'\publish';  ssssssssssss
  // Edit1.Text := IGPublisherFolder;
end;

procedure TIGSettingsForm.Button13Click(Sender: TObject);
var
  cmd: string;
begin
  svc := 'git';
  cmd := 'show > nul';
  if runAndWait(igRootFolder, svc, cmd) <> 0 then begin
    TDialogService.MessageDialog('Repository not found',
      TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0,
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

procedure TIGSettingsForm.Button14Click(Sender: TObject);
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

procedure TIGSettingsForm.Button15Click(Sender: TObject);
var
  cmd: string;
begin
  svc := 'git';
  cmd := 'push';

  if runAndWait(igRootFolder, svc, cmd) = 0 then
  begin
  end

end;

Procedure TIGSettingsForm.Button1Click(Sender: TObject);
var
  dir, folder: string;

begin
  if SelectDirectory('Select path to IG framework (contains License.md)', '', dir) then
  begin
    IGPublisherFolder := dir;
    Edit1.Text := dir;
  end;

end;

procedure TIGSettingsForm.Button2Click(Sender: TObject);
var
  url, FileName: string;
  LStream: TFileStream;

begin
  // url := 'https://github.com/madhur/PortableJekyll/archive/master.zip';
  // url := 'https://github.com/costateixeira/ihe_mma/archive/master.zip';

end;

procedure TIGSettingsForm.Button5Click(Sender: TObject);
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
    DownloadForm.SourceURL := 'https://bitbucket.org/costateixeira/ig-builder/downloads/publish_2_flat.zip';
    DownloadForm.localFileName := IGPublisherFolder + '\publish.zip';
    DownloadForm.UnzipLocation := IGPublisherFolder;
    DownloadForm.Unzip := true;
    DownloadForm.ShowModal;

    Button11Click(nil);

    DownloadForm.close;
    DownloadForm.Destroy;

    if checkPublishFolders(IGPublisherFolder) then
    begin
      Button5.Text := 'Folders OK';
      Button10.Enabled := true
    end
    else
    begin
      Button5.Text := 'Download';
      Button10.Enabled := False
    end

  end;
end;

procedure TIGSettingsForm.Button6Click(Sender: TObject);
begin
  igRootFolder := Edit4.Text;
  createFolders(Edit4.Text);

end;

function TIGSettingsForm.checkContentFolders(rootFolder: string): boolean;
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
      result := False;
  end;

  if result then
  begin
    Button6.Text := 'IG Folders OK';
    Button6.Enabled := False
  end
  else
  begin
    Button6.Text := 'Create IG Folders';
    Button6.Enabled := true
  end

end;

function TIGSettingsForm.checkPublishFolders(publisherFolder: string): boolean;
begin

  if IGPublisherFolder = '' then
  begin
    result := False;
  end
  else
  begin
    // ForceDirectories(pwidechar(IGPublisherFolder));
    if directoryexists(publisherFolder) then
    begin
      IGPublisherFolder := publisherFolder;

      // pandoc checking should go somewhere else
      pandocfolder := IGPublisherFolder + '\framework\pandoc';
    end;

    if ((directoryexists(pwidechar(IGPublisherFolder))) and (directoryexists(pwidechar(pandocfolder)))) then
      result := true
    else
      result := False;

  end;
end;

procedure TIGSettingsForm.createFolders(rootFolder: string);
begin
  igRootFolder := rootFolder;
  igcontentfolder := igRootFolder + '\content';
  tempfolder := igcontentfolder + '\temp';
  pagecontentfolder := igcontentfolder + '\pagecontent';
  resourcesFolder := igcontentfolder + '\resources';
  mediafolder := igcontentfolder + '\images';

  IGPublisherFolder := igRootFolder + '\publish';
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

procedure TIGSettingsForm.Button7Click(Sender: TObject);
var
  dir: string;
begin
  if SelectDirectory('Select path to Base IG template (contains folders "content", etc', '', dir) then
  begin
    IGPublisherFolder := dir;
    Edit5.Text := dir;
  end;
end;

procedure TIGSettingsForm.Button8Click(Sender: TObject);
var
  dir: string;
begin
  if SelectDirectory('Select path to IG content (contains folders "content", etc.)', '', dir) then
  begin
    igRootFolder := dir;
    Edit4.Text := dir;
  end;
  if checkContentFolders(Edit4.Text) then
    Button11Click(nil);

end;

procedure TIGSettingsForm.Button9Click(Sender: TObject);
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

procedure TIGSettingsForm.FormShow(Sender: TObject);
begin
  Edit1.Text := IGPublisherFolder;
  Edit5.Text := BaseTemplateFolder;
  Edit4.Text := igRootFolder;
  if checkContentFolders(Edit4.Text) then
    Button6.Enabled := true;

  if IGPublisherFolder <> '' then
    Edit1.Text := IGPublisherFolder;

  // if checkPublishFolders(Edit1.Text) then
  // Button10.Enabled := true;

  Button11Click(self);
end;

procedure TIGSettingsForm.OnZipProgressEvent(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
begin
  // ProgressBar1.Value := (Position * 100) div Header.UncompressedSize;
  // application.ProcessMessages;
end;

function TIGSettingsForm.runAndWait(Path, command, parameters: String): integer;
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
      until (ExitCode <> STILL_ACTIVE) or application.Terminated;
    end;
    result := ExitCode;

  end;

end;
{$ENDIF}

procedure TIGSettingsForm.SetupIGPublisherFiles;
var
  str: string;
  i: integer;
  guidenamestr: string;
  parameter_line: integer;
  destFile: string;

  sa: TStringDynArray;

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
    tfile.Delete(IGPublisherFolder + '\src\' + extractFileName(IGFileName));
  except
  end;
  tfile.copy(igRootFolder + '\' + IGFileName, IGPublisherFolder + '\src\' + extractFileName(IGFileName));

  begin
    sa := TDirectory.GetFiles(igcontentfolder + '\resources');
    for i := 0 to Length(sa) - 1 do
    begin
      destFile := IGPublisherFolder + '\src\resources\' + extractFileName(sa[i]);
      try
        tfile.Delete(pwidechar(destFile));
      finally
        tfile.copy(pwidechar(sa[i]), pwidechar(destFile));
      end;
    end;
  end;

  begin
    sa := TDirectory.GetFiles(igcontentfolder + '\pagecontent');
    for i := 0 to Length(sa) - 1 do
    begin
      destFile := IGPublisherFolder + '\src\pagecontent\' + extractFileName(sa[i]);
      try
        tfile.Delete(pwidechar(destFile));
      finally
        tfile.copy(pwidechar(sa[i]), pwidechar(destFile));
      end;
    end;
  end;

  begin
    sa := TDirectory.GetFiles(igcontentfolder + '\images');
    for i := 0 to Length(sa) - 1 do
    begin
      destFile := IGPublisherFolder + '\src\images\' + extractFileName(sa[i]);
      try
        tfile.Delete(pwidechar(destFile));
      finally
        tfile.copy(pwidechar(sa[i]), pwidechar(destFile));
      end;
    end;
  end;

end;

end.
