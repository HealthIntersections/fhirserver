unit gui_lcl;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  StdCtrls, Buttons, lclintf,
  fsl_utilities,
  server_config, gui_controller;

type

  { TServerGUI }

  TServerGUI = class(TForm)
    About1: TMenuItem;
    BitBtn1: TBitBtn;
    btnBrowser: TButton;
    btnStatus: TButton;
    Contents1: TMenuItem;
    edtFolder: TEdit;
    edtPort: TEdit;
    Exit1: TMenuItem;
    File1: TMenuItem;
    Help1: TMenuItem;
    Label1: TLabel;
    Label2: TLabel;
    lblStatus: TLabel;
    MainMenu1: TMainMenu;
    mLog: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Timer1: TTimer;
    procedure BitBtn1Click(Sender: TObject);
    procedure btnBrowserClick(Sender: TObject);
    procedure btnStatusClick(Sender: TObject);
    procedure edtFolderChange(Sender: TObject);
    procedure edtPortChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FIni : TFHIRServerConfigFile;
    FServer : TFHIRServerController;
    procedure serverStatusChange(Sender: TObject);
    procedure log(msg : String);
  public

  end;

var
  ServerGUI: TServerGUI;

implementation

{$R *.lfm}

{ TServerGUI }

procedure TServerGUI.FormCreate(Sender: TObject);
begin
  Fini := TFHIRServerConfigFile.Create(Path([ExtractFilePath(paramstr(0)), 'fhir-server-gui.cfg']));
  FServer := TFHIRServerController.create(FIni.link);
  FServer.OnStatusChange := serverStatusChange;
  FServer.OnLog := log;
  FServer.Initialise;
  edtFolder.Text := FIni.service['utg-folder'].value;
  edtPort.Text := Fini.web['http'].value;
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


procedure TServerGUI.BitBtn1Click(Sender: TObject);
var
  s, s2 : String;
begin
  s := FIni.service['utg-folder'].value;
  if SelectDirectory('Select UTG Folder', s, s2) then
  begin
    FIni.service['utg-folder'].value := s2;
    edtFolder.Text := s2;
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

procedure TServerGUI.btnBrowserClick(Sender: TObject);
begin
  OpenURL(FServer.Address);
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
      end;
    ssStopping :
      begin
      btnStatus.Enabled := false;
      lblStatus.Caption := 'Stopping...';
      btnBrowser.Enabled := false;
      end;
    ssNotRunning :
      begin
      btnStatus.Enabled := true;
      lblStatus.Caption := 'Start';
      lblStatus.Caption := 'Not Running';
      btnBrowser.Enabled := false;
      end;
  end;
end;

procedure TServerGUI.Timer1Timer(Sender: TObject);
begin
  FServer.Ping;
end;

end.

