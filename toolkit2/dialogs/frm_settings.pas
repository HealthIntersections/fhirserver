unit frm_settings;

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
  fsl_utilities,
  fhir_colour_utils,
  fui_lcl_utilities;

type

  { TToolkitSettingsForm }

  TToolkitSettingsForm = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    btnEditorFont: TButton;
    btnLogFont: TButton;
    btnViewFont: TButton;
    btnClearCache: TButton;
    Button6: TButton;
    chkSideBySide: TCheckBox;
    dlgFont: TFontDialog;
    edtCache: TEdit;
    edtTxServer: TEdit;
    edtTxLog: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    lblEditorFont: TLabel;
    Label4: TLabel;
    lblDiff: TLabel;
    lblLogFont: TLabel;
    Label6: TLabel;
    lblViewFont: TLabel;
    dlgExe: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnClearCacheClick(Sender: TObject);
    procedure btnEditorFontClick(Sender: TObject);
    procedure btnLogFontClick(Sender: TObject);
    procedure btnViewFontClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDiffTool : String;
  public
    property DiffTool : String read FDiffTool write FDiffTool;
  end;

var
  ToolkitSettingsForm: TToolkitSettingsForm;

implementation

{$R *.lfm}

{ TToolkitSettingsForm }

function describeFont(font : TFont) : String;
begin
  if font.name = '' then
    result := '(default)'
  else
    result := font.name;
  if font.size <> 0 then
    result := result+', '+inttostr(font.size)+'pt';
  if fsBold in font.Style then
    result := result+', bold';
  if fsItalic in font.Style then
      result := result+', italic';
  if fsUnderline in font.Style then
    result := result+', underline';
  if font.Color <> clDefault then
    result := result+'; color = '+ColourToString(font.color);
end;

procedure TToolkitSettingsForm.FormShow(Sender: TObject);
begin
  lblEditorFont.caption := describeFont(lblEditorFont.Font);
  lblLogFont.caption := describeFont(lblLogFont.Font);
  lblViewFont.caption := describeFont(lblViewFont.Font);
  lblDiff.caption := ExtractFileName(FDiffTool);
end;

procedure TToolkitSettingsForm.btnEditorFontClick(Sender: TObject);
begin
  dlgFont.Font.assign(lblEditorFont.Font);
  if (dlgFont.Execute) then
  begin
    lblEditorFont.Font.assign(dlgFont.Font);
    lblEditorFont.caption := describeFont(lblEditorFont.Font);
  end;
end;

procedure TToolkitSettingsForm.btnClearCacheClick(Sender: TObject);
begin
  DeleteFile(edtCache.text);
end;

procedure TToolkitSettingsForm.btnLogFontClick(Sender: TObject);
begin
  dlgFont.Font.assign(lblLogFont.Font);
  if (dlgFont.Execute) then
  begin
    lblLogFont.Font.assign(dlgFont.Font);
    lblLogFont.caption := describeFont(lblLogFont.Font);
  end;
end;

procedure TToolkitSettingsForm.btnViewFontClick(Sender: TObject);
begin
  dlgFont.Font.assign(lblViewFont.Font);
  if (dlgFont.Execute) then
  begin
    lblViewFont.Font.assign(dlgFont.Font);
    lblViewFont.caption := describeFont(lblViewFont.Font);
  end;
end;

procedure TToolkitSettingsForm.Button6Click(Sender: TObject);
begin
  dlgExe.filename := FDiffTool;
  if dlgExe.Execute then
  begin
    FDiffTool := dlgExe.filename;
    lblDiff.caption := ExtractFileName(FDiffTool);
  end;
end;

procedure TToolkitSettingsForm.FormCreate(Sender: TObject);
begin
  setForOs(btnOk, btnCancel);
end;

end.

