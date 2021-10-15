unit frm_edit_changes;

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, SynEdit,
  Process,
  fsl_diff, fsl_utilities,
  fui_lcl_utilities,
  ftk_context;

type

  { TEditChangeReviewForm }

  TEditChangeReviewForm = class(TForm)
    btnCancel: TButton;
    btnReplace: TButton;
    btnExternalDiff: TButton;
    lblStatus: TLabel;
    lblSummary: TLabel;
    mBase: TMemo;
    mCurr: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlLeftSummary: TPanel;
    pnlRightSummary: TPanel;
    pnlTextLeft: TPanel;
    pnlTextRight: TPanel;
    mSource: TSynEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Timer1: TTimer;
    procedure btnCancelClick(Sender: TObject);
    procedure btnExternalDiffClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mSourceChange(Sender: TObject);
    procedure TabSheet1Resize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FDiffTool: String;
    FEditor: TToolkitEditor;
    baseBytes, currBytes : TBytes;
    FFileTime : TDateTime;
    FDirty : boolean;
    currFile : String;
    procedure SetEditor(AValue: TToolkitEditor);
    procedure buildTextDiff;
  public
    property editor : TToolkitEditor read FEditor write SetEditor;
    property DiffTool : String read FDiffTool write FDiffTool;
  end;

var
  EditChangeReviewForm: TEditChangeReviewForm;

implementation

{$R *.lfm}

{ TEditChangeReviewForm }

procedure TEditChangeReviewForm.FormDestroy(Sender: TObject);
begin
  FEditor.Free;
end;

procedure TEditChangeReviewForm.btnExternalDiffClick(Sender: TObject);
var
  baseFile : String;
  p : TProcess;
begin
  if editor.Session.Address.StartsWith('file:') then
    baseFile := editor.Session.Address.Substring(5)
  else
  begin
    baseFile := FilePath(['[tmp]', 'base.'+editor.Store.MakeFilename(editor.session.Address)]);
    BytesToFile(baseBytes, baseFile);
  end;
  currFile := FilePath(['[tmp]', 'current.'+ExtractFileName(baseFile)]);
  BytesToFile(currBytes, currFile);
  p := TProcess.create(nil);
  try
    p.Executable := DiffTool;
    p.Parameters.add(baseFile);
    p.Parameters.add(currFile);
    p.Options := [];
    p.Execute;
  finally
    p.free;
  end;
  lblStatus.caption := 'Watching '+currFile+' for changes';
  lblStatus.visible := true;
  FFileTime := FileGetModified(currFile);
  Timer1.enabled := true;
end;

procedure TEditChangeReviewForm.FormCreate(Sender: TObject);
begin
  setForOs(btnReplace, btnCancel);
end;

procedure TEditChangeReviewForm.btnCancelClick(Sender: TObject);
begin
  if (not FDirty) or (MessageDlg('Change Review', 'The source has changed. Exit without updating the editor?', mtConfirmation, mbYesNo, 0) = mrNo) then
    ModalResult := mrCancel;
end;

procedure TEditChangeReviewForm.FormShow(Sender: TObject);
begin
  buildTextDiff;
  btnExternalDiff.enabled := fileExists(FDiffTool);
end;

procedure TEditChangeReviewForm.mSourceChange(Sender: TObject);
begin
  FDirty := true;
  btnReplace.Enabled := true;
end;

procedure TEditChangeReviewForm.TabSheet1Resize(Sender: TObject);
begin
  pnlTextLeft.Width := TabSheet1.Width div 2;
end;

procedure TEditChangeReviewForm.Timer1Timer(Sender: TObject);
begin
  if FFileTime <> 0 then
  begin
    if FFileTime <> FileGetModified(currFile) then
    begin
      Timer1.enabled := false;
      if (MessageDlg('Review Change', 'The file '+ExtractFileName(currFile)+' has changed. Reload?', mtConfirmation, mbYesNo, 0) = mrYes) then
        mSource.Lines.LoadFromFile(CurrFile);
    end;
  end;
end;

procedure TEditChangeReviewForm.SetEditor(AValue: TToolkitEditor);
begin
  FEditor.Free;
  FEditor := AValue;
end;

procedure TEditChangeReviewForm.buildTextDiff;
var
  base, curr : TStringList;
  diff : TFslTextComparer;
begin
  baseBytes := editor.Store.load(editor.session.address, true).content;
  currBytes := editor.GetBytes;
  mSource.text := TEncoding.UTF8.GetAnsiString(currBytes);
  btnReplace.Enabled := false;
  FDirty := false;

  base := TStringList.create;
  curr := TStringList.create;
  diff := TFslTextComparer.create;
  try
    base.text := TEncoding.UTF8.GetAnsiString(baseBytes).trim+#13#10;
    curr.text := TEncoding.UTF8.GetAnsiString(currBytes).trim+#13#10;
    diff.IgnoreCase := false;
    diff.IgnoreSpaces := true;
    diff.Heuristic := 50;
    diff.Source1 := base;
    diff.Source2 := curr;
    diff.Output1 := mBase.lines;
    diff.Output2 := mCurr.lines;
    diff.Execute;

    lblSummary.caption := 'Comparison between Saved and Current version of '+editor.Session.Caption+': '+inttostr(diff.count)+' '+StringPlural('change', diff.count)+' found';
    pnlLeftSummary.caption := inttostr(base.count)+' lines';
    pnlRightSummary.caption := inttostr(curr.count)+' lines';
  finally
    diff.free;
    base.free;
    curr.free;
  end;
end;

end.

