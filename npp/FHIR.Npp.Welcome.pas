unit FHIR.Npp.Welcome;

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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.CheckLst, Vcl.Imaging.pngimage,
  FHIR.Npp.Base, FHIR.Npp.Form,
  fsl_utilities,
  fsl_npm_cache, FHIR.Npm.Manager,
  FHIR.Npp.Context;

type
  TWelcomeScreenForm = class(TNppForm)
    Panel1: TPanel;
    btnOk: TButton;
    chkWelcomeScreen: TCheckBox;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
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
    Label9: TLabel;
    Label10: TLabel;
    chkR2: TCheckBox;
    chkR3: TCheckBox;
    chkR4: TCheckBox;
    Image1: TImage;
    procedure btnOkClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkR2Click(Sender: TObject);
    procedure chkR3Click(Sender: TObject);
    procedure chkR4Click(Sender: TObject);
  private
    FContext: TFHIRNppContext;
    FCache : TFHIRPackageManager;
    procedure SetContext(const Value: TFHIRNppContext);
  public
    property Context : TFHIRNppContext read FContext write SetContext;
  end;

var
  WelcomeForm: TWelcomeScreenForm;

procedure ShowWelcomeForm(owner : TNppPlugin);

implementation

{$R *.dfm}

uses
  FHIR.Npp.Settings, FHIR.Npp.Configuration;

procedure ShowWelcomeForm(owner : TNppPlugin);
begin
  WelcomeForm := TWelcomeScreenForm.Create(owner);
  try
    WelcomeForm.ShowModal;
  finally
    FreeAndNil(WelcomeForm);
  end;
end;

procedure TWelcomeScreenForm.btnOkClick(Sender: TObject);
begin
  Settings.ToolboxVisible := chkToolbox.Checked;
  Settings.NoWelcomeForm := chkWelcomeScreen.Checked;
  Settings.VisualiserVisible := chkVisualizer.Checked;
  Settings.BackgroundValidation := chkValidation.Checked;
  Settings.CommitChanges;
end;

procedure TWelcomeScreenForm.Button1Click(Sender: TObject);
begin
  SettingForm := TSettingForm.Create(self);
  try
    SettingForm.Context := Context.link;
    SettingForm.ShowModal;
  finally
    FreeAndNil(SettingForm);
  end;
end;

procedure TWelcomeScreenForm.chkR2Click(Sender: TObject);
begin
  if chkR2.Checked and not FCache.packageExists('hl7.fhir.core', '1.0.2') then
  begin
    PackageCacheForm := TPackageCacheForm.Create(self);
    try
      PackageCacheForm.UserMode := true;
      PackageCacheForm.GoUrl := 'http://hl7.org/fhir/DSTU2';
      PackageCacheForm.showModal;
    finally
      FreeAndNil(PackageCacheForm);
    end;
    if FCache.packageExists('hl7.fhir.core', '1.0.2') then
    begin
      Settings.loadR2 := true;
      Settings.CommitChanges;
    end;
    chkR2.Checked := FCache.packageExists('hl7.fhir.core', '1.0.2') and Settings.loadR2;
  end;
end;

procedure TWelcomeScreenForm.chkR3Click(Sender: TObject);
begin
  if chkR3.Checked and not FCache.packageExists('hl7.fhir.core', '3.0.2') then
  begin
    PackageCacheForm := TPackageCacheForm.Create(self);
    try
      PackageCacheForm.UserMode := true;
      PackageCacheForm.GoUrl := 'http://hl7.org/fhir';
      PackageCacheForm.showModal;
    finally
      FreeAndNil(PackageCacheForm);
    end;
    if FCache.packageExists('hl7.fhir.core', '3.0.2') then
    begin
      Settings.loadR3 := true;
      Settings.CommitChanges;
    end;
    chkR3.Checked := FCache.packageExists('hl7.fhir.core', '3.0.2') and Settings.loadR3;
  end;
end;

procedure TWelcomeScreenForm.chkR4Click(Sender: TObject);
begin
 if chkR4.Checked and not FCache.packageExists('hl7.fhir.core', '3.4.0') then
  begin
    PackageCacheForm := TPackageCacheForm.Create(self);
    try
      PackageCacheForm.UserMode := true;
      PackageCacheForm.GoUrl := 'http://build.fhir.org/';
      PackageCacheForm.showModal;
    finally
      FreeAndNil(PackageCacheForm);
    end;
    if FCache.packageExists('hl7.fhir.core', '3.4.0') then
    begin
      Settings.loadR4 := true;
      Settings.CommitChanges;
    end;
    chkR4.Checked := FCache.packageExists('hl7.fhir.core', '3.4.0') and Settings.loadR4;
  end;
end;

procedure TWelcomeScreenForm.FormDestroy(Sender: TObject);
begin
  FContext.free;
  FCache.Free;
end;

procedure TWelcomeScreenForm.FormShow(Sender: TObject);
begin
  FCache := TFHIRPackageManager.Create(true);
  chkToolbox.Checked := Settings.ToolboxVisible;
  chkWelcomeScreen.Checked := Settings.NoWelcomeForm;
  chkVisualizer.Checked := Settings.VisualiserVisible;
  chkValidation.Checked := Settings.BackgroundValidation;
  chkR4.Checked := FCache.packageExists('hl7.fhir.core', 'current') and Settings.loadR4;
  chkR3.Checked := FCache.packageExists('hl7.fhir.core', '3.0.2') and Settings.loadR3;
  chkR2.Checked := FCache.packageExists('hl7.fhir.core', '1.0.2') and Settings.loadR2;
end;

procedure TWelcomeScreenForm.SetContext(const Value: TFHIRNppContext);
begin
  FContext.free;
  FContext := Value;
end;

end.
