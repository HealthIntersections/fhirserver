unit frm_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  fsl_utilities;

type

  { TToolkitSettingsForm }

  TToolkitSettingsForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    btnEditorFont: TButton;
    btnLogFont: TButton;
    btnViewFont: TButton;
    Button6: TButton;
    chkSideBySide: TCheckBox;
    dlgFont: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    lblEditorFont: TLabel;
    Label4: TLabel;
    lblDiff: TLabel;
    lblLogFont: TLabel;
    Label6: TLabel;
    lblViewFont: TLabel;
    dlgExe: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnEditorFontClick(Sender: TObject);
    procedure btnLogFontClick(Sender: TObject);
    procedure btnViewFontClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
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

end.

