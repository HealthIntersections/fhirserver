unit FHIR.Transformer.SettingsDialog;

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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, IniFiles;

type
  TTransformerOptionsForm = class(TForm)
    Panel1: TPanel;
    btnok: TButton;
    btnCancel: TButton;
    cbAutosave: TCheckBox;
    Label1: TLabel;
    cbxTerminologyServer: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure btnokClick(Sender: TObject);
  private
    FIni : TIniFile;

  public
    Property Ini : TIniFile read FIni write FIni;
  end;

var
  TransformerOptionsForm: TTransformerOptionsForm;

implementation

{$R *.dfm}

procedure TTransformerOptionsForm.btnokClick(Sender: TObject);
begin
  FIni.WriteBool('Workspace', 'AutoSave', cbAutosave.Checked);
  FIni.WriteString('Workspace', 'TerminologyServer', cbxTerminologyServer.Text);
end;

procedure TTransformerOptionsForm.FormShow(Sender: TObject);
begin
  cbAutosave.Checked := FIni.ReadBool('Workspace', 'AutoSave', false);
  FIni.ReadSection('TerminologyServers', cbxTerminologyServer.items);
  if cbxTerminologyServer.items.Count = 0 then
    cbxTerminologyServer.items.Add('http://tx.fhir.org/r4');
  cbxTerminologyServer.Text := FIni.ReadString('Workspace', 'TerminologyServer', '');
end;

end.
