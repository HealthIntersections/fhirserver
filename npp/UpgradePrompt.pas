unit UpgradePrompt;

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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FHIR.Npp.Form, Vcl.StdCtrls, Vcl.ExtCtrls, fsl_utilities,
  Vcl.ComCtrls, Vcl.CheckLst, Vcl.Imaging.pngimage, FHIR.Npp.Base, shellapi;

type
  TUpgradePromptForm = class(TNppForm)
    Panel1: TPanel;
    btnOk: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
    FLink : String;
  public
    { Public declarations }
  end;

var
  UpgradePromptForm: TUpgradePromptForm;

procedure ShowUpgradePrompt(owner : TNppPlugin; link, notes : String);

implementation

{$R *.dfm}

uses
  FHIR.Npp.Settings, FHIR.Npp.Configuration;

procedure ShowUpgradePrompt(owner : TNppPlugin; link, notes : String);
begin
  UpgradePromptForm := TUpgradePromptForm.create(owner);
  try
    UpgradePromptForm.Memo1.text := notes;
    UpgradePromptForm.FLink := link;
    UpgradePromptForm.ShowModal;
  finally
    FreeAndNil(UpgradePromptForm);
  end;
end;

procedure TUpgradePromptForm.btnOkClick(Sender: TObject);
begin
  Settings.BuildPrompt := FLink;
end;

procedure TUpgradePromptForm.Button1Click(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', PChar(FLink), '', '', SW_SHOWNORMAL);
end;

end.
