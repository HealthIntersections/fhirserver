unit gui_vcl;

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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.FileCtrl, Vcl.Menus, Vcl.Buttons,
  Vcl.StdCtrls, Vcl.ExtCtrls,
  fsl_utilities, fsl_shell,
  server_config, server_constants, gui_controller;

type
  TServerGUI = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    mLog: TMemo;
    btnStatus: TButton;
    btnBrowser: TButton;
    lblStatus: TLabel;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    edtFolder: TEdit;
    edtPort: TEdit;
    btnFolder: TBitBtn;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    About1: TMenuItem;
    Timer1: TTimer;
    procedure btnFolderClick(Sender: TObject);
    procedure btnStatusClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtFolderChange(Sender: TObject);
    procedure edtPortChange(Sender: TObject);
    procedure btnBrowserClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Exit1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
  private
    { Private declarations }
    FIni : TFHIRServerConfigFile;
    FServer : TFHIRServerController;
    FCount : integer;
    FWantStop : boolean;
    procedure serverStatusChange(Sender: TObject);
    procedure log(msg : String);
  public
    { Public declarations }
  end;

var
  ServerGUI: TServerGUI;

implementation

{$R *.dfm}

procedure TServerGUI.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FServer.Status = ssNotRunning then
    CanClose := true
  else
  begin
    CanClose := false;
    FServer.Stop;
    FWantStop := true;
  end;
end;

procedure TServerGUI.FormCreate(Sender: TObject);
begin
  Fini := TFHIRServerConfigFile.Create(Path([ExtractFilePath(paramstr(0)), 'fhir-server-gui.cfg']));
  FServer := TFHIRServerController.create(FIni.link);
  FServer.OnStatusChange := serverStatusChange;
  FServer.OnLog := log;
  FServer.Initialise;
  edtFolder.Text := FIni.service['utg-folder'].value;
  edtPort.Text := Fini.web['http'].value;
  FWantStop := false;
end;

procedure TServerGUI.FormDestroy(Sender: TObject);
begin
  FServer.Free;
  FIni.Free;
end;

procedure TServerGUI.log(msg: String);
var
   s : String;
begin
  if (msg.StartsWith('~')) then
    mLog.lines[mLog.Lines.Count-1] := mLog.lines[mLog.Lines.Count-1]+msg.Substring(1)
  else
    mLog.lines.add(msg);
  s := mlog.text;
  mLog.selStart := s.Length - msg.length;
end;

procedure TServerGUI.btnFolderClick(Sender: TObject);
var
  s : String;
begin
  s := FIni.service['utg-folder'].value;
  if SelectDirectory('Select UTG Folder', '', s, [sdNewFolder, sdShowEdit, sdNewUI], self) then
  begin
    FIni.service['utg-folder'].value := s;
    edtFolder.Text := s;
  end;
end;

procedure TServerGUI.edtFolderChange(Sender: TObject);
begin
  FIni.service['utg-folder'].value := edtFolder.Text;
end;

procedure TServerGUI.edtPortChange(Sender: TObject);
begin
  Fini.web['http'].value := edtPort.Text;
end;

procedure TServerGUI.Exit1Click(Sender: TObject);
var
  cc : boolean;
begin
  FormCloseQuery(self, cc);
  if (cc) then
    close;
end;

procedure TServerGUI.About1Click(Sender: TObject);
begin
  ShowMessage('FHIR Terminology Server v'+SERVER_FULL_VERSION);
end;

procedure TServerGUI.btnBrowserClick(Sender: TObject);
begin
  ExecuteURL(FServer.Address);
end;

procedure TServerGUI.btnStatusClick(Sender: TObject);
begin
  if FServer.Status = ssNotRunning then
  begin
    FServer.checkOk;
    FServer.Start;
    btnStatus.Enabled := false;
  end
  else
  begin
    FServer.Stop;
  end;
end;

procedure TServerGUI.serverStatusChange(Sender: TObject);
begin
  case FServer.Status of
    ssStarting :
      begin
      btnStatus.Enabled := false;
      lblStatus.Caption := 'Starting...';
      btnBrowser.Enabled := false;
      edtFolder.Enabled := false;
      edtPort.Enabled := false;
      btnFolder.enabled := false;
      mLog.Color := clWhite;
      end;
    ssRunning :
      begin
      btnStatus.Enabled := true;
      btnStatus.Caption := 'Stop';
      btnBrowser.Enabled := true;
      if FServer.Stats <> nil then
        lblStatus.Caption := 'Running. '+FServer.Stats.Present
      else
        lblStatus.Caption := 'Running. ??';
      edtFolder.Enabled := false;
      edtPort.Enabled := false;
      btnFolder.enabled := false;
      mLog.Color := clWhite;
      end;
    ssStopping :
      begin
      btnStatus.Enabled := false;
      lblStatus.Caption := 'Stopping...';
      btnBrowser.Enabled := false;
      edtFolder.Enabled := false;
      edtPort.Enabled := false;
      btnFolder.enabled := false;
      mLog.Color := clWhite;
      end;
    ssNotRunning :
      begin
      btnStatus.Enabled := true;
      btnStatus.Caption := 'Start';
      lblStatus.Caption := 'Not Running';
      btnBrowser.Enabled := false;
      edtFolder.Enabled := true;
      edtPort.Enabled := true;
      btnFolder.enabled := true;
      edtFolder.Enabled := false;
      edtPort.Enabled := false;
      btnFolder.enabled := false;
      mLog.Color := ColourCompose($f2, $f2, $f2, 0);
      if (FWantStop) then
        close;
      end;
  end;
end;

procedure TServerGUI.Timer1Timer(Sender: TObject);
begin
  FServer.Ping;
  inc(fCount);
  if (Fcount = 10) then
  begin
    FCount := 0;
    serverStatusChange(self);
  end;
end;

end.



