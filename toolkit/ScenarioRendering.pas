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

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  shellapi, fmx.platform.win, winapi.windows,
  FHIR.Version.Utilities, FHIR.Base.Objects,FHIR.Version.Resources,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FDownloadForm;

type
  TESRender = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button5: TButton;
    Button10: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    filename:String;
    ESRenderFolder:String;
    ESRootFolder:String;
    resource:TFHIRExampleScenario;
  end;

var
  ESRender: TESRender;

implementation

{$R *.fmx}

procedure TESRender.Button10Click(Sender: TObject);
var
  str: string;
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
  ExecuteFile, ParamString, StartInString: string;
begin
if directoryexists(edit1.text) then str:=edit1.text else  str := getCurrentDir;
  SetCurrentDir(ESRenderFolder+'\simpleRender');
  resourceToFile(resource, '.\current.xml', ffXml, OutputStylePretty);
  ExecuteFile := 'parse_all.bat';

  begin
    FillChar(SEInfo, SizeOf(SEInfo), 0);
    SEInfo.cbSize := SizeOf(TShellExecuteInfo);
    with SEInfo do
    begin
      fMask := SEE_MASK_NOCLOSEPROCESS;
      Wnd := FmxHandleToHWND(self.Handle );
      lpFile := PChar(ExecuteFile);
      lpDirectory := PChar(StartInString);
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
    SetCurrentDir('.\output\pages');
    ShellExecute(0, 'open', '.\current.html', '', '', SW_SHOWNORMAL);
    SetCurrentDir(str);
  end;

end;

procedure TESRender.Button1Click(Sender: TObject);
var
  dir, folder: string;

begin
  if SelectDirectory('Select path to Example Scenario Render (contains License.md)', '', dir) then
  begin
    ESRenderFolder := dir;
    Edit1.Text := dir;
  end;

end;

procedure TESRender.Button5Click(Sender: TObject);
var
  DownloadForm: TDownloadForm;
begin

  DownloadForm := TDownloadForm.create(self);
  DownloadForm.SourceURL := 'https://bitbucket.org/costateixeira/ig-builder/downloads/simpleESRender.zip';
  DownloadForm.localFileName := ESRenderFolder + '\render.zip';
  DownloadForm.UnzipLocation := ESRenderFolder;
  DownloadForm.Unzip := true;
  DownloadForm.ShowModal;


  DownloadForm.Close;
  DownloadForm.Destroy;


end;

end.
