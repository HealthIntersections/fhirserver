unit AboutDialog;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects, FMX.Controls.Presentation,
  OSXUIUtils,
  FHIRConstants,
  toolkitversion;

type
  TAboutForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblVersion: TLabel;
    Label4: TLabel;
    lblDoco: TLabel;
    lblIssue: TLabel;
    lblSpec: TLabel;
    lblUpdates: TLabel;
    procedure lblUpdatesClick(Sender: TObject);
    procedure lblSpecClick(Sender: TObject);
    procedure lblDocoClick(Sender: TObject);
    procedure lblIssueClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.fmx}

procedure TAboutForm.FormShow(Sender: TObject);
begin
  lblVersion.Text := 'Toolkit Version '+ToolKitVersionBase+inttostr(BuildCount)+', FHIR Version '+FHIR_GENERATED_VERSION;
end;

procedure TAboutForm.lblDocoClick(Sender: TObject);
begin
  OpenURL('http://wiki.hl7.org/index.php?title=FHIR_Toolkit_Documentation');
end;

procedure TAboutForm.lblIssueClick(Sender: TObject);
begin
  OpenURL('https://github.com/grahamegrieve/fhirserver/issues');
end;

procedure TAboutForm.lblSpecClick(Sender: TObject);
begin
  OpenURL('http://www.hl7.org/fhir');
end;

procedure TAboutForm.lblUpdatesClick(Sender: TObject);
begin
  OpenURL('http://www.healthintersections.com.au/FhirServer/toolkit.htm');
end;

end.
