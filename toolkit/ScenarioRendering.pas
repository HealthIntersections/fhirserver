unit ScenarioRendering;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  {$IFDEF WINDOWS}
  winapi.shellapi, fmx.platform.win, winapi.windows, jclshell,
  {$ENDIF}
  FHIR.Version.Utilities, fhir_objects,FHIR.Version.Resources,
  FMX.dialogservice, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FDownloadForm,
  FMX.ListBox, FMX.ScrollBox, FMX.Memo;

type
  TESPublishForm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button5: TButton;
    Button10: TButton;
    ComboBox1: TComboBox;
    Label3: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure RunInMemo(CommandLine: string; Work: string; parameters: string; Memo: TMemo);
  private
    { Private declarations }
  public
    { Public declarations }
    ESFileName:String;
    ESPublisherFolder:String;
    ESRootFolder:String;
    resource:TFHIRExampleScenario;
  end;

var
  ESPublishForm: TESPublishForm;


implementation

{$R *.fmx}

procedure TESPublishForm.Button10Click(Sender: TObject);
{$IFDEF WINDOWS}
var
  SEInfo: TShellExecuteInfo;
  ESPublisherFolder: string;
  ExitCode: DWORD;
  fileStr, appDir, ExecuteFile, ParamString, StartInString: string;

{$ENDIF}
begin

//  if edit1.text='' then edit1.Text:=extractfilepath(appDir)+'\Rendering\ExampleScenario';
{$IFDEF WINDOWS}
  appDir:=GetCurrentDir;
if directoryexists(edit1.text) then ESPublisherFolder:=edit1.text else ESPublisherFolder := getCurrentDir+'\Rendering\ExampleScenario';
  SetCurrentDir(ESPublisherFolder+'');
  resource:=filetoresource(ESRootFolder+'\'+ESFileName) as TFHIRExampleScenario;
  resourceToFile(resource, ESPublisherFolder+'\current.xml', ffXml, OutputStylePretty);
  ExecuteFile := 'BUILD.bat';



    try
      if fileexists(ESPublisherFolder + '\BUILD.bat') then
      begin
        button10.enabled := false;
//        isBuilding := true;
        RunInMemo('cmd.exe /C BUILD', ESPublisherFolder , '', Memo1);
        // CaptureConsoleOutput(tempstr, 'cmd.exe /C BUILD', igRootFolder, Memo2);
//        filestr := ESPublisherFolder + '\output\pages\current.html';
//        filestr := stringreplace(filestr, '\', '/', [rfReplaceAll, rfIgnoreCase]);
    SetCurrentDir(ESPublisherFolder+'\output\pages');
    ShellExec(0, 'open', '.\current.html', '', '', 0);
    SetCurrentDir(appDir);

      end
      else
        TDialogService.MessageDialog('Publisher not found. '#13#10'Ensure the Publisher is in the same folder as the resource file.', System.UITypes.TMsgDlgType.mtInformation,
          [System.UITypes.TMsgDlgBtn.mbOk], System.UITypes.TMsgDlgBtn.mbOk, 0, nil, nil);
    finally
      button10.enabled := true;
//      isBuilding := false;
    end;



{
 try
//  RunInMemo('cmd.exe /C BUILD.BAT', GetCurrentDir, ESPublisherFolder, Memo1);


  begin
      FillChar(SEInfo, SizeOf(SEInfo), 0);
      SEInfo.cbSize := SizeOf(TShellExecuteInfo);
      with SEInfo do
      begin
        fMask := SEE_MASK_NOCLOSEPROCESS;
        Wnd := FmxHandleToHWND(self.Handle);
        lpFile := PChar(ExecuteFile);
        lpDirectory := PChar(ESPublisherFolder);
        nShow := SW_SHOWNORMAL;
      end;
      if ShellExecuteEx(@SEInfo) then
      begin
        repeat
          Application.ProcessMessages;
          GetExitCodeProcess(SEInfo.hProcess, ExitCode);
        until (ExitCode <> STILL_ACTIVE) or Application.Terminated;
        // ShowMessage('Calculator terminated');
      end
      // else ShowMessage('Error starting Calc!');
        ;



end;

    finally
end;
}


{$ELSE}
  raise Exception.Create('Not done yet');
{$ENDIF}



end;

procedure TESPublishForm.Button1Click(Sender: TObject);
var
  dir, folder: string;

begin
  if SelectDirectory('Select path to Example Scenario Render (contains License.md)', '', dir) then
  begin
    ESPublisherFolder := dir;
    Edit1.Text := dir;
  end;

end;

procedure TESPublishForm.Button5Click(Sender: TObject);
var
  DownloadForm: TDownloadForm;
begin
{
  DownloadForm := TDownloadForm.create(self);
  DownloadForm.SourceURL := 'https://bitbucket.org/costateixeira/ig-builder/downloads/simpleESRender.zip';
  DownloadForm.localFileName := ESRenderFolder + '\render.zip';
  DownloadForm.UnzipLocation := ESRenderFolder;
  DownloadForm.Unzip := true;
  DownloadForm.ShowModal;


  DownloadForm.Close;
  DownloadForm.Destroy;
}


  if Edit1.Text = '' then
    TDialogService.MessageDialog('Scenario renderer folder not specified. ' + #13#10 + 'Do you want to download the scenario renderer to the current work folder?.',
    // System.UITypes.TMsgDlgType.mtCustom, [System.UITypes.TMsgDlgBtn.mbYes,System.UITypes.TMsgDlgBtn.mbCancel], System.UITypes.TMsgDlgBtn.mbYes, 0,
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, 0,
      procedure(const AResult: TModalResult)
      begin
        case AResult of
          mrYes:
            Edit1.Text := 'c:\publish';
        end;
      end);

  if Edit1.Text <> '' then
  begin
    ESPublisherFolder := Edit1.Text;
    forceDirectories(pwidechar(ESPublisherFolder));
    // how to check whether the publisher is already there. ???

    DownloadForm := TDownloadForm.Create(self);
    DownloadForm.SourceURL := combobox1.Selected.Text;;
    DownloadForm.localFileName := ESPublisherFolder + '\espublish.zip';
    DownloadForm.UnzipLocation := ESPublisherFolder;
    DownloadForm.Unzip := true;
    DownloadForm.ShowModal;


    DownloadForm.close;
    DownloadForm.Destroy;

//    createFolders(igRootFolder);
    edit1.Text:=ESPublisherFolder;


  end;



end;




procedure TESPublishForm.RunInMemo(CommandLine: string; Work: string; parameters: string; Memo: TMemo);
{$IFDEF WINDOWS}
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
//            Application.ProcessMessages();
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
{$ELSE}
begin
{$ENDIF}
end;


end.
