unit console_id_edit;

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
  Buttons,
  server_config;

type
  { TEditIDForm }

  TEditIDForm = class(TForm)
    btnDBTest1: TBitBtn;
    btnDBTest3: TBitBtn;
    edtIdentity: TEdit;
    edtAppSecret: TEdit;
    edtAppid: TEdit;
    edtAPIKey: TEdit;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
  private
    FId: TFHIRServerConfigSection;
    procedure SetId(AValue: TFHIRServerConfigSection);
  public
    property Id : TFHIRServerConfigSection read FId write SetId;
    procedure update;
  end;

var
  EditIDForm: TEditIDForm;

implementation

{$R *.lfm}

{ TEditIDForm }

procedure TEditIDForm.FormDestroy(Sender: TObject);
begin
  FId.Free;
end;

procedure TEditIDForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOk then
  begin
    update;
    CanClose := true;
  end;
end;

procedure TEditIDForm.SetId(AValue: TFHIRServerConfigSection);
begin
  FId.Free;
  FId := AValue;

  if FId <> nil then
  begin
    edtIdentity.text := Id.name;
    edtAppid.text := Id['app-id'].value;
    edtAppSecret.text := Id['app-secret'].value;
    edtAPIKey.text := Id['api-key'].value;
  end;
end;

procedure TEditIDForm.update;
begin
  Id.name := edtIdentity.text;
  Id['app-id'].value := edtAppid.Text;
  Id['app-secret'].value := edtAppSecret.Text;
  Id['api-key'].value := edtAPIKey.Text;
end;

end.

