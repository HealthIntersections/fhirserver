unit AboutForms;


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
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, NppForms, Vcl.StdCtrls, Vcl.Imaging.pngimage, Vcl.ExtCtrls, ShellApi,
  FHIRProfileUtilities, FHIRContext;

type
  TAboutForm = class(TNppForm)
    Button1: TButton;
    Panel1: TPanel;
    Image1: TImage;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblVersion: TLabel;
    Label5: TLabel;
    GroupBox1: TGroupBox;
    lnkDoco: TLabel;
    lnkIssue: TLabel;
    lnkSpec: TLabel;
    lnkUpdates: TLabel;
    lblDefinitions: TLabel;
    procedure FormShow(Sender: TObject);
    procedure lnkDocoClick(Sender: TObject);
    procedure lnkIssueClick(Sender: TObject);
    procedure lnkSpecClick(Sender: TObject);
    procedure lnkUpdatesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

uses
  FHIRConstants,
  FHIRPlugin,
  FHIRPath,
  nppbuildcount;

procedure TAboutForm.FormShow(Sender: TObject);
begin
  inherited;
  lblVersion.Caption := 'Plugin Version 1.0.'+inttostr(buildcount)+', FHIR Version '+FHIR_GENERATED_VERSION
end;

procedure TAboutForm.lnkDocoClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', 'http://wiki.hl7.org/index.php?title=FHIR_Notepad%2B%2B_Plugin_Documentation', '', '', SW_SHOWNORMAL);
end;

procedure TAboutForm.lnkIssueClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', 'https://github.com/grahamegrieve/fhirserver/issues', '', '', SW_SHOWNORMAL);
end;

procedure TAboutForm.lnkSpecClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', 'http://hl7.org/fhir/index.html', '', '', SW_SHOWNORMAL);
end;

procedure TAboutForm.lnkUpdatesClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', 'http://www.healthintersections.com.au/FhirServer/fhirnpp.htm', '', '', SW_SHOWNORMAL);
end;


end.
