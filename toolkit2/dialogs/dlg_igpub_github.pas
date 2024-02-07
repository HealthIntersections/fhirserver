unit dlg_igpub_github;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, ClipBrd,
  fsl_base, fsl_utilities, fsl_threads,
  fui_lcl_utilities;

type

  { TIgGitHubDialog }

  TIgGitHubDialog = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    btnPaste: TButton;
    btnCopyGHUrl: TButton;
    edtLocalFolder: TEdit;
    edtGitOrg: TEdit;
    edtBranch: TEdit;
    edtFolder: TEdit;
    edtGitRepoName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    fd: TSelectDirectoryDialog;
    SpeedButton1: TSpeedButton;
    Timer1: TTimer;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCopyGHUrlClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FLines : TStringList;
    FLock : TFslLock;
    FGit : TFslExternalProcessThread;
    procedure doPaste(error : boolean);
    function cloneIg : boolean;
    procedure doLine(sender : TFslExternalProcessThread; line : String; repl : boolean);
  public

  end;

var
  IgGitHubDialog: TIgGitHubDialog;

implementation

{$R *.lfm}

{ TIgGitHubDialog }

procedure TIgGitHubDialog.FormCreate(Sender: TObject);
begin
  setForOs(btnOk, btnCancel);
  FLines := TStringList.Create;
  FLock := TFslLock.Create;
  if isAbsoluteUrl(Clipboard.asText) then
    doPaste(false);
end;

procedure TIgGitHubDialog.FormDestroy(Sender: TObject);
begin
  FLines.free;
  FLock.free;
end;

procedure TIgGitHubDialog.btnPasteClick(Sender: TObject);
begin
  doPaste(true);
end;

procedure TIgGitHubDialog.btnOkClick(Sender: TObject);
begin
  if cloneIg then
    ModalResult := mrOk;
end;

procedure TIgGitHubDialog.btnCopyGHUrlClick(Sender: TObject);
begin
  Clipboard.AsText:= 'https://github.com/'+edtGitOrg.text+'/'+edtGitRepoName.text;
end;

procedure TIgGitHubDialog.btnCancelClick(Sender: TObject);
begin
  if FGit <> nil then
    FGit.terminate
  else
    ModalResult := mrCancel;
end;

function makeFileName(s : String) : String;
var
  b : TFslStringBuilder;
  ch : char;
begin
  b := TFslStringBuilder.Create;
  try
    for ch in s do
      if (ch in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', '#', '$', '.']) then
        b.append(ch);
    result := b.ToString;
  finally
    b.free;
  end;
end;

procedure TIgGitHubDialog.doPaste(error : boolean);
var
  url, org, repo, branch : String;
  p : TStringArray;
begin
  try
    if not isAbsoluteUrl(Clipboard.asText) then
      raise EFslException.Create('Not a url: '+Clipboard.asText);

    branch := 'master';
    url := Clipboard.asText;
    p := url.split(['/']);

    if (length(p) > 5) and (url.startsWith('https://build.fhir.org/ig') or url.startsWith('http://build.fhir.org/ig')) then
    begin
      org := p[4];
      repo := p[5];
      if (length(p) >= 8) then
      begin
        if ('branches' <> p[6]) then
          raise EFslException.Create('Unable to understand IG location '+url)
        else
          branch := p[7];
      end;
    end
    else if (length(p) > 4) and (url.startsWith('https://github.com/') or url.startsWith('http://github.com/')) then
    begin
      org := p[3];
      repo := p[4];
      if (length(p) > 6) then
      begin
        if ('tree' = p[5]) or ('blob' = p[5]) then
          branch := p[6]
        else
          raise EFslException.Create('Unable to understand IG location '+url);
      end;
    end;
    if (org = '') or (repo = '') then
      raise EFslException.Create('Unable to understand IG location: '+url);

    edtGitOrg.Text := org;
    edtGitRepoName.Text := repo;
    edtBranch.Text := branch;
    edtLocalFolder.Text := makeFileName(org+'-'+repo+'#'+branch);
  except
    on e : Exception do
    begin
      if error then
        MessageDlg('URL Error', e.message, mtError, [mbok], 0);
    end;
  end;
end;

function TIgGitHubDialog.cloneIg: boolean;
var
  lf : String;
begin
  lf := FilePath([edtFolder.text, edtLocalFolder.text]);
  Memo1.lines.clear;
  memo1.lines.add('Clone Git repo (git clone --branch '+edtBranch.Text+' https://github.com/'+edtGitOrg.text+'/'+edtGitRepoName.text+' '+lf);

  FGit := TFslExternalProcessThread.Create;
  try
    FGit.command := 'git';
    FGit.parameters.Add('clone');
    FGit.parameters.Add('--progress');
    FGit.parameters.Add('--branch');
    FGit.parameters.Add(edtBranch.Text);
    FGit.parameters.Add('https://github.com/'+edtGitOrg.text+'/'+edtGitRepoName.text);
    FGit.parameters.Add(lf);
    FGit.environmentVars := false;
    FGit.folder := GetTempDir;
    FGit.OnEmitLine := doLine;
    FGit.Start;
    while FGit.Running do
      Application.processMessages;

    if FGit.status <> epsTerminated then
      result := FGit.exitCode = 0
    else
    begin
      FolderDelete(lf);
      memo1.lines.add('Deleted '+lf);
    end;
  finally
    FGit.free;
    FGit := nil;
  end;
  memo1.lines.add('Done');
end;

procedure TIgGitHubDialog.doLine(sender: TFslExternalProcessThread; line: String; repl : boolean);
begin
  FLock.Lock;
  try
    if (repl) then
      FLines.add('t'+line)
    else
      FLines.add('f'+line);
  finally
    FLock.Unlock;
  end;
end;

procedure TIgGitHubDialog.SpeedButton1Click(Sender: TObject);
begin
  fd.filename := edtFolder.text;
  if fd.execute then
    edtFolder.text := fd.filename;
end;

procedure TIgGitHubDialog.Timer1Timer(Sender: TObject);
var
  s : String;
  ok : boolean;
begin
  FLock.Lock;
  try
    ok := false;
    for s in FLines do
    begin
      if (s.startsWith('t')) and (memo1.Lines.Count > 0) then
        memo1.Lines[memo1.Lines.Count - 1] := s.Substring(1)
      else
        memo1.lines.add(s.Substring(1));
      ok := true;
    end;
    FLines.clear;
  finally
    FLock.Unlock;
  end;
  if ok then
    memo1.SelStart := length(memo1.text);
end;

end.

