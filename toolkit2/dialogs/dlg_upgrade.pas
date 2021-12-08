unit dlg_upgrade;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, lclintf,
  HtmlView,
  MarkdownProcessor,
  fsl_base, fsl_utilities, fsl_fetcher,
  fui_lcl_utilities;

type

  { TToolkitUpgradeForm }

  TToolkitUpgradeForm = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    btnReset: TButton;
    html: THtmlViewer;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnOkClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMarkdown: String;
    FUrlExe: String;
    FUrlPage: String;
    procedure SetMarkdown(AValue: String);
  public
    property urlPage : String read FUrlPage write FUrlPage;
    property urlExe : String read FUrlExe write FUrlExe;
    property markdown : String read FMarkdown write SetMarkdown;
  end;

var
  ToolkitUpgradeForm: TToolkitUpgradeForm;

function showUpgradeInformation(owner : TComponent; urlPage, urlExe, md : String) : TModalResult;

implementation

{$R *.lfm}

function showUpgradeInformation(owner : TComponent; urlPage, urlExe, md : String) : TModalResult;
begin
  ToolkitUpgradeForm := TToolkitUpgradeForm.create(owner);
  try
    ToolkitUpgradeForm.urlPage := urlPage;
    ToolkitUpgradeForm.urlExe := urlExe;
    ToolkitUpgradeForm.markdown := md;
    result := ToolkitUpgradeForm.ShowModal;
  finally
    ToolkitUpgradeForm.Free;
  end;
end;

{ TToolkitUpgradeForm }

procedure TToolkitUpgradeForm.FormCreate(Sender: TObject);
begin
  setForOs(btnOk, btnCancel);
end;

procedure TToolkitUpgradeForm.btnResetClick(Sender: TObject);
begin
  openURL(FUrlPage);
end;

procedure TToolkitUpgradeForm.btnOkClick(Sender: TObject);
var
  fn : String;
begin
  fn := FilePath([DownloadsFolder, ExtractFileName(FUrlExe)]);
  try
    BytesToFile(TInternetFetcher.fetchUrl(FUrlExe), fn);
    {$IFDEF WINDOWS}
    if OpenDocument(fn) then
      ModalResult := mrOk
    else
      raise EFslException.create('Unable to execute download '+fn);
    {$ELSE}
    raise EFslException.create('Not implemented yet');
    {$ENDIF}
  except
    on e : Exception do
      MessageDlg('Download', 'Error: '+e.message, mtError, [mbok], 0);
  end;
end;

procedure TToolkitUpgradeForm.SetMarkdown(AValue: String);
var
  proc : TMarkdownProcessor;
begin
  FMarkdown := AValue;

  proc := TMarkdownProcessor.createDialect(mdCommonMark);
  try
    proc.allowUnsafe := false;
    html.LoadFromString(proc.process(FMarkdown));
  finally
    proc.free;
  end;
end;

end.

