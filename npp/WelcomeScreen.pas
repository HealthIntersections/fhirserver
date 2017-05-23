unit WelcomeScreen;

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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, NppForms, Vcl.StdCtrls, Vcl.ExtCtrls, StringSupport,
  Vcl.ComCtrls, Vcl.CheckLst, Vcl.Imaging.pngimage, nppplugin;

type
  TWelcomeScreenForm = class(TNppForm)
    Panel1: TPanel;
    btnOk: TButton;
    chkWelcomeScreen: TCheckBox;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Image1: TImage;
    Image2: TImage;
    Label4: TLabel;
    Label5: TLabel;
    chkToolbox: TCheckBox;
    Label6: TLabel;
    Label7: TLabel;
    chkVisualizer: TCheckBox;
    Image3: TImage;
    Label3: TLabel;
    Label8: TLabel;
    chkValidation: TCheckBox;
    Button1: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WelcomeScreenForm: TWelcomeScreenForm;

procedure ShowWelcomeScreen(owner : TNppPlugin);

implementation

{$R *.dfm}

uses
  FHIRPluginSettings, FHIRClient, FHIRResources, SmartOnFhirUtilities, SettingsForm;

procedure ShowWelcomeScreen(owner : TNppPlugin);
begin
  WelcomeScreenForm := TWelcomeScreenForm.Create(owner);
  try
    WelcomeScreenForm.ShowModal;
  finally
    FreeAndNil(WelcomeScreenForm);
  end;
end;

procedure TWelcomeScreenForm.btnOkClick(Sender: TObject);
begin
  Settings.ToolboxVisible := chkToolbox.Checked;
  Settings.NoWelcomeScreen := chkWelcomeScreen.Checked;
  Settings.VisualiserVisible := chkVisualizer.Checked;
  Settings.BackgroundValidation := chkValidation.Checked;
end;

procedure TWelcomeScreenForm.Button1Click(Sender: TObject);
begin
  SettingForm := TSettingForm.Create(self);
  try
    SettingForm.ShowModal;
  finally
    FreeAndNil(SettingForm);
  end;
end;

end.
