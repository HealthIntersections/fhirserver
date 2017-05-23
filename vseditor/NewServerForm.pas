unit NewServerForm;

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


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, ValueSetEditorCore;

type
  TfrmNewServer = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    edtName: TEdit;
    edtAddress: TEdit;
    Label3: TLabel;
    edtUsername: TEdit;
    Label4: TLabel;
    edtPassword: TEdit;
    CheckBox1: TCheckBox;
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    FContext : TValueSetEditorContext;
    FDoesSearch : boolean;
    procedure SetContext(const Value: TValueSetEditorContext);
  public
    property Context : TValueSetEditorContext read FContext write SetContext;
    Property DoesSearch : boolean read FDoesSearch write FDoesSearch;
  end;

var
  frmNewServer: TfrmNewServer;

implementation

{$R *.dfm}

procedure TfrmNewServer.Button1Click(Sender: TObject);
var
  msg : String;
begin
  screen.Cursor := crHourGlass;
  Panel1.Caption := '   Checking Server';
  try
    Panel1.Update;
    if Context.CheckServer(frmNewServer.edtAddress.Text, msg, FDoesSearch) then
      ModalResult := mrOk
  finally
    screen.Cursor := crDefault;
    Panel1.Caption := '';
  end;
  if ModalResult <> mrOk then
    ShowMessage(msg);
end;

procedure TfrmNewServer.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    edtPassword.PasswordChar := #0
  else
    edtPassword.PasswordChar := '*';
end;

procedure TfrmNewServer.FormDestroy(Sender: TObject);
begin
  FContext.Free;
end;

procedure TfrmNewServer.SetContext(const Value: TValueSetEditorContext);
begin
  FContext.Free;
  FContext := value;
end;

end.
