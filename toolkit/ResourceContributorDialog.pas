unit ResourceContributorDialog;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.ImageList, FMX.ImgList, FMX.ScrollBox, FMX.Memo, FMX.Edit,
  FMX.DateTimeCtrls, FMX.StdCtrls, FMX.Controls.Presentation,
  fsl_utilities,
  FHIR.Version.Types, FHIR.Version.Utilities;

type
  TResourceContributorForm = class(TForm)
    Panel1: TPanel;
    btnOk: TButton;
    Button2: TButton;
    btnAsChild: TButton;
    lblId: TLabel;
    LblAuthor: TLabel;
    lblNotes: TLabel;
    edtName: TEdit;
    edtRole: TEdit;
    memNotes: TMemo;
    procedure FormShow(Sender: TObject);
    procedure edtDateChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    FExtension : TFhirExtension;
    procedure SetExtension(const Value: TFhirExtension);
  public
    destructor Destroy; override;
    property Extension : TFhirExtension read FExtension write SetExtension;
  end;

var
  ResourceContributorForm: TResourceContributorForm;

implementation

{$R *.fmx}

{ TForm1 }


procedure TResourceContributorForm.btnOkClick(Sender: TObject);
begin
  FExtension.setExtensionString('name', edtName.Text);
  FExtension.setExtensionString('role', edtRole.Text);
  FExtension.setExtensionString('notes', memNotes.Text);
end;

destructor TResourceContributorForm.Destroy;
begin
  FExtension.Free;
  inherited;
end;

procedure TResourceContributorForm.edtDateChange(Sender: TObject);
begin
  btnOk.Enabled := (edtName.Text <> '') and (edtRole.Text <> '');
end;

procedure TResourceContributorForm.FormShow(Sender: TObject);
begin
  edtName.Text := FExtension.getExtensionString('name');
  edtRole.Text := FExtension.getExtensionString('role');
  memNotes.Text := FExtension.getExtensionString('notes');
end;

procedure TResourceContributorForm.SetExtension(const Value: TFhirExtension);
begin
  FExtension.Free;
  FExtension := Value;
end;

end.
