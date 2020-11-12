unit ProjectFilesDialog;

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
{$IF defined(MSWINDOWS)}
  winapi.ShellApi, ShlObj, fmx.platform.win, winapi.windows,
  jclsysutils,
{$ENDIF}
  fmx.Types, fmx.Controls, fmx.Forms, fmx.Graphics, fmx.Dialogs, fmx.StdCtrls, fmx.Edit, fmx.Controls.Presentation, System.ImageList, fmx.ImgList,
  FMX.ScrollBox, FMX.Memo,
  fsl_base;

type
  TProjectDialog = class(TForm)
    Label3: TLabel;
    Edit4: TEdit;
    Button8: TButton;
    Button6: TButton;
    Panel1: TPanel;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Label1: TLabel;
    ImageList1: TImageList;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure runAndWait(Path, command, parameters: String);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure HandleOutput( const Text: string );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ProjectDialog: TProjectDialog;

implementation

{$R *.fmx}

procedure TProjectDialog.HandleOutput( const Text: string );
begin
  Memo1.Lines.Add( Text );
end;


procedure TProjectDialog.Button13Click(Sender: TObject);
var str:string;


begin
str:='git pull';

{$IF defined(MSWINDOWS)}
//  runandwait(Edit4.text, 'cmd.exe', '/K '+str)
Execute('cmd /c '+str, HandleOutput);

{$ENDIF}

end;

procedure TProjectDialog.Button14Click(Sender: TObject);
var str1, str2:string;
begin
str1:='git add --all';
str2:='git commit';
{$IF defined(MSWINDOWS)}
  runandwait(Edit4.text, 'cmd.exe', '/K '+str1)
{$ENDIF}  ;
{$IF defined(MSWINDOWS)}
  runandwait(Edit4.text, 'cmd.exe', '/K '+str2)
{$ENDIF}


end;

procedure TProjectDialog.Button1Click(Sender: TObject);
begin
{$IF defined(MSWINDOWS)}
  ShellExecute(0, nil, 'explorer.exe', PChar(Edit4.text), nil, SW_SHOWNORMAL);
{$ENDIF}
end;

procedure TProjectDialog.Button2Click(Sender: TObject);
begin
  begin
{$IF defined(MSWINDOWS)}
    ShellExecute(0, nil, 'cmd.exe', PChar('/K cd /d ' + Edit4.text), nil, SW_SHOWNORMAL);
{$ENDIF}
  end;

end;

procedure TProjectDialog.Button8Click(Sender: TObject);
var
  str: string;
begin

  if SelectDirectory('Select folder', '', str) then
  begin
    Edit4.text := str;
    // Edit1.Text := dir;
  end;

end;



procedure TProjectDialog.runAndWait(Path, command, parameters: String);
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
  folderstr := getcurrentdir;

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
  end;
end;
{$ENDIF}


end.
