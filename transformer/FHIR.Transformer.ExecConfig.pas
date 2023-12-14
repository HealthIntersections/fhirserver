unit FHIR.Transformer.ExecConfig;

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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  FHIR.Transformer.Workspace;

type
  TTransformerExecConfigForm = class(TForm)
    Panel1: TPanel;
    btnok: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    cbxScripts: TComboBox;
    Label2: TLabel;
    cbxFocus: TComboBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnokClick(Sender: TObject);
  private
    FConfig : TWorkspaceExecConfig;
    FWorkspace : TWorkspace;
    procedure SetConfig(const Value: TWorkspaceExecConfig);
    procedure SetWorkspace(const Value: TWorkspace);
  public
    property Config : TWorkspaceExecConfig read FConfig write SetConfig;
    property Workspace : TWorkspace read FWorkspace write SetWorkspace;
  end;

var
  TransformerExecConfigForm: TTransformerExecConfigForm;

implementation

{$R *.dfm}

procedure TTransformerExecConfigForm.btnokClick(Sender: TObject);
begin
  FConfig.script := cbxScripts.Text;
  FConfig.focus := cbxFocus.Text;
end;

procedure TTransformerExecConfigForm.FormDestroy(Sender: TObject);
begin
  FConfig.free;
  FWorkspace.free;
end;

procedure TTransformerExecConfigForm.FormShow(Sender: TObject);
var
  f : TWorkspaceFile;
begin
  cbxScripts.Items.Clear;
  for f in FWorkspace.scripts do
    cbxScripts.Items.AddObject(f.filename, f);
  cbxScripts.ItemIndex := cbxScripts.Items.IndexOf(FConfig.script);

  cbxFocus.Items.Clear;
  for f in FWorkspace.messages do
    cbxFocus.Items.AddObject(f.filename, f);
  for f in FWorkspace.documents do
    cbxFocus.Items.AddObject(f.filename, f);
  cbxFocus.ItemIndex := cbxFocus.Items.IndexOf(FConfig.focus);
end;

procedure TTransformerExecConfigForm.SetConfig(const Value: TWorkspaceExecConfig);
begin
  FConfig.free;
  FConfig := Value;
end;

procedure TTransformerExecConfigForm.SetWorkspace(const Value: TWorkspace);
begin
  FWorkspace.free;
  FWorkspace := Value;
end;

end.
