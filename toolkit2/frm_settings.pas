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
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    dlgFont: TFontDialog;
    Label1: TLabel;
    Label2: TLabel;
    lblEditorFont: TLabel;
    Label4: TLabel;
    lblLogFont: TLabel;
    Label6: TLabel;
    lblViewFont: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

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
end;

procedure TToolkitSettingsForm.Button3Click(Sender: TObject);
begin
  dlgFont.Font.assign(lblEditorFont.Font);
  if (dlgFont.Execute) then
  begin
    lblEditorFont.Font.assign(dlgFont.Font);
    lblEditorFont.caption := describeFont(lblEditorFont.Font);
  end;
end;

procedure TToolkitSettingsForm.Button4Click(Sender: TObject);
begin
  dlgFont.Font.assign(lblLogFont.Font);
  if (dlgFont.Execute) then
  begin
    lblLogFont.Font.assign(dlgFont.Font);
    lblLogFont.caption := describeFont(lblLogFont.Font);
  end;
end;

procedure TToolkitSettingsForm.Button5Click(Sender: TObject);
begin
  dlgFont.Font.assign(lblViewFont.Font);
  if (dlgFont.Execute) then
  begin
    lblViewFont.Font.assign(dlgFont.Font);
    lblViewFont.caption := describeFont(lblViewFont.Font);
  end;
end;

end.

