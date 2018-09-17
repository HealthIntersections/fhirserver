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
  FMX.dialogservice, System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent, FDownloadForm;

type
  TIGSettingsForm = class(TForm)
    btnCheckDependencies: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
    Button3: TButton;
    Button4: TButton;
    EditFileNamex: TEdit;
    EditURLx: TEdit;
    CheckBox2: TCheckBox;
    Button6: TButton;
    Edit3x: TEdit;
    Edit4: TEdit;
    Label3: TLabel;
    Edit5: TEdit;
    Button7: TButton;
    Button8: TButton;
    Button2: TButton;
    Button5: TButton;
    Label2: TLabel;
    Label4: TLabel;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    procedure Button1Click(Sender: TObject);
    procedure btnCheckDependenciesClick(Sender: TObject);
    procedure OnZipProgressEvent(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure SetupIGPublisherFiles;
    procedure Button11Click(Sender: TObject);

    /// ////////////////////
  private
    { Private declarations }
    FClient: THTTPClient;
    FGlobalStart: Cardinal;
    FAsyncResult: IAsyncResult;
    FDownloadStream: TStream;
  public
    { Public declarations }
    dependencies: boolean;
    fwInstalled: boolean;
    igRootFolder, IGPublisherFolder, IGFileName, BaseTemplateFolder: String;

    resourcesFolder, pagecontentfolder, igcontentfolder, mediafolder, pandocfolder, tempfolder: string;

    procedure runAndWait(Path, command, parameters: String);
    procedure createFolders(rootFolder: string);

  end;

var
  IGSettingsForm: TIGSettingsForm;

implementation

{$R *.fmx}

uses ImplementationGuideEditor;

procedure TIGSettingsForm.btnCheckDependenciesClick(Sender: TObject);
{$IFDEF OSX}
begin
  raise EFslException.Create('Not done yet for OSX');
end;
{$ELSE}
var
  str: string;

begin
exit;
  Memo1.Lines.Clear;

  TDialogService.MessageDialog('Not Implemented yet', System.UITypes.TMsgDlgType.mtInformation, [System.UITypes.TMsgDlgBtn.mbOk], System.UITypes.TMsgDlgBtn.mbOk, 0, nil, nil);
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
  // TDialogService.MessageDialog('Not Implemented yet', System.UITypes.TMsgDlgType.mtInformation, [System.UITypes.TMsgDlgBtn.mbOk], System.UITypes.TMsgDlgBtn.mbOk, 0, nil, nil);

  SetupIGPublisherFiles;

  tempstr := IGPublisherFolder;
  if fileexists(tempstr + '\_genonce.bat') then
  begin
    runAndWait(tempstr, '_genonce', '');
    filestr := IGPublisherFolder + '\output\index.html';
    filestr := stringreplace(filestr, '\', '/', [rfReplaceAll, rfIgnoreCase]);
    runAndWait(tempstr, filestr, '');
    // filestr := stringreplace(filestr, '\', '/', [rfReplaceAll, rfIgnoreCase]);
    // runAndWait(igRootFolder, 'open', filestr);
  end
  else
    TDialogService.MessageDialog('IG Publisher not found. '#13#10'Ensure the IG Publisher is in the same folder as the IG file.', System.UITypes.TMsgDlgType.mtInformation,
      [System.UITypes.TMsgDlgBtn.mbOk], System.UITypes.TMsgDlgBtn.mbOk, 0, nil, nil);
end;

procedure TIGSettingsForm.Button11Click(Sender: TObject);
begin

  if (directoryexists(pwidechar(igRootFolder + '\content'))) then
  begin
    igcontentfolder := igRootFolder + '\content';
    if (directoryexists(pwidechar(igcontentfolder + '\temp'))) then
      tempfolder := igcontentfolder + '\temp';
    if (directoryexists(pwidechar(igcontentfolder + '\pagecontent'))) then
      pagecontentfolder := igcontentfolder + '\pagecontent';
    if (directoryexists(pwidechar(igcontentfolder + '\resources'))) then
      resourcesFolder := igcontentfolder + '\resources';
    if (directoryexists(pwidechar(igcontentfolder + '\images'))) then
      mediafolder := igcontentfolder + '\images';
  end;

  if (directoryexists(pwidechar(igRootFolder + '\publish'))) then
  begin
    IGPublisherFolder := igRootFolder + '\publish';
    Edit1.Text := IGPublisherFolder;
    if (directoryexists(pwidechar(IGPublisherFolder + '\framework\pandoc'))) then
      pandocfolder := IGPublisherFolder + '\framework\pandoc';
  end;

end;

procedure TIGSettingsForm.Button1Click(Sender: TObject);
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
  url := 'https://github.com/madhur/PortableJekyll/archive/master.zip';
  url := 'https://github.com/costateixeira/ihe_mma/archive/master.zip';

  FileName := 'C:\temp\tempunzipped\xxx.zip';

  LStream := TFileStream.create(FileName, fmCreate or fmOpenWrite or fmShareDenyNone);
  try
    // NetHTTPClient1.Get(url,Lstream);
  finally
    LStream.Free;
  end;

end;

procedure TIGSettingsForm.Button3Click(Sender: TObject);
begin
  // 1. download and install the IG FW
  // 2. copy resource to ig-xxx.xml
  // 3. update properties.txt
  // 4. copy media\*.* to src\images
  // 5. copy pages \*.* to src\pagecontent
  // 6. build?

end;

procedure TIGSettingsForm.Button5Click(Sender: TObject);
var
  DownloadForm: TDownloadForm;
begin

  DownloadForm := TDownloadForm.create(nil);

  // downloadform.UnzipFile:='c:\temp\xxx.zip';
  DownloadForm.url := 'file:///C:/ImpGuide/publish/publish.zip';
  DownloadForm.url := 'https://bitbucket.org/costateixeira/ig-builder/downloads/publish.zip';
  DownloadForm.localFileName := igRootFolder + '\publish.zip';
  DownloadForm.UnzipLocation := igRootFolder;
  DownloadForm.Unzip := true;
  DownloadForm.Show;
  application.ProcessMessages;
  DownloadForm.sampleDownload;
  // DownloadForm.DoUnzip;
  while not DownloadForm.DownloadComplete do
    application.ProcessMessages;
  Button11Click(nil);

  DownloadForm.Destroy;
end;

procedure TIGSettingsForm.Button6Click(Sender: TObject);
begin

  igRootFolder := Edit4.Text;
  createFolders(Edit4.Text);
  exit;

  begin
    TDialogService.MessageDialog('IG content folder not found. Do you want to create it? ' + #13#10 + 'Click No if you want to save in the same folder, cancel to skip saving',
      System.UITypes.TMsgDlgType.mtInformation, [System.UITypes.TMsgDlgBtn.mbYes, System.UITypes.TMsgDlgBtn.mbNo, System.UITypes.TMsgDlgBtn.mbCancel], System.UITypes.TMsgDlgBtn.mbYes, 0,
      procedure(const AResult: TModalResult)
      begin
        case AResult of
          mrYES:
            begin
            end;
          mrNo:
            begin
              // tempstr := igrootfolder + '\' + Edit1.text + '.xml';
              // copyfile(pwidechar(filestr), pwidechar(tempstr), true);
            end;
        end;
      end);
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
      ForceDirectories(pwidechar(igRootFolder));
    if not(directoryexists(pwidechar(igcontentfolder))) then
      ForceDirectories(pwidechar(igcontentfolder));
    if not(directoryexists(pwidechar(pagecontentfolder))) then
      ForceDirectories(pwidechar(pagecontentfolder));
    if not(directoryexists(pwidechar(resourcesFolder))) then
      ForceDirectories(pwidechar(resourcesFolder));
    if not(directoryexists(pwidechar(mediafolder))) then
      ForceDirectories(pwidechar(mediafolder));
  end;

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

  Button11Click(nil);
end;

procedure TIGSettingsForm.Button9Click(Sender: TObject);
var
  DownloadForm: TDownloadForm;
begin

  DownloadForm := TDownloadForm.create(nil);

  DownloadForm.localFileName := 'c:\temp\xxx.zip';
  DownloadForm.UnzipLocation := 'c:\temp\ttx';
  DownloadForm.Unzip := true;
  DownloadForm.Show;
  application.ProcessMessages;
  DownloadForm.DoUnzip;
  DownloadForm.Destroy;

end;

procedure TIGSettingsForm.FormShow(Sender: TObject);
begin
  Edit1.Text := IGPublisherFolder;
  Edit5.Text := BaseTemplateFolder;
  Edit4.Text := igRootFolder;

  button11click(self);
end;

procedure TIGSettingsForm.OnZipProgressEvent(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
begin
  ProgressBar1.Value := (Position * 100) div Header.UncompressedSize;
  application.ProcessMessages;
end;

procedure TIGSettingsForm.runAndWait(Path, command, parameters: String);
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
        application.ProcessMessages;
        GetExitCodeProcess(SEInfo.hProcess, ExitCode);
      until (ExitCode <> STILL_ACTIVE) or application.Terminated;
    end;
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
