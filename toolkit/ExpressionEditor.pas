unit ExpressionEditor;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.ComboEdit, FMX.StdCtrls, System.ImageList, FMX.ImgList,
  FMX.Edit, FMX.Controls.Presentation,
  fsl_base,
  fhir4_types, fhir4_resources,
  TranslationsEditorDialog;

type
  TExpressionEditorForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    edtDescription: TEdit;
    ToolbarImages: TImageList;
    btnText: TButton;
    edtName: TEdit;
    cbeLanguage: TComboEdit;
    edtReference: TEdit;
    memExpression: TMemo;
    btnOk: TButton;
    Button3: TButton;
    lblDoco: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    btnClear: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure btnTextClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    exp : TFHIRExpression;
    resource : TFHIRResource;
  public
    { Public declarations }
  end;

var
  ExpressionEditorForm: TExpressionEditorForm;

function editExpression(parent : TComponent; resource : TFHIRResource; exp : TFHIRExpression; doco : String) : TFHIRExpression;

implementation

{$R *.fmx}

function editExpression(parent : TComponent; resource : TFHIRResource; exp : TFHIRExpression; doco : String) : TFHIRExpression;
begin
  ExpressionEditorForm := TExpressionEditorForm.create(parent);
  try
    ExpressionEditorForm.lblDoco.text := doco;

    if exp <> nil then
    begin
      ExpressionEditorForm.edtDescription.text := exp.description;
      ExpressionEditorForm.edtName.text := exp.name;
      ExpressionEditorForm.cbeLanguage.text := exp.language;
      ExpressionEditorForm.edtReference.text := exp.reference;
      ExpressionEditorForm.memExpression.text := exp.expression;
      ExpressionEditorForm.exp := exp.link;
      ExpressionEditorForm.resource := resource.link;
    end
    else
    begin
      ExpressionEditorForm.btnClear.enabled := false;
      ExpressionEditorForm.exp := TFHIRExpression.create;
      ExpressionEditorForm.resource := resource.link;
    end;
    case ExpressionEditorForm.showModal of
      mrOK:
        begin
          result := ExpressionEditorForm.exp.link;
          result.description := ExpressionEditorForm.edtDescription.text;
          result.name := ExpressionEditorForm.edtName.text;
          result.language := ExpressionEditorForm.cbeLanguage.text;
          result.reference := ExpressionEditorForm.edtReference.text;
          result.expression := ExpressionEditorForm.memExpression.text;
        end;
      mrCancel: result := exp.link;
      mrAbort: result := nil;
    end;
  finally
    ExpressionEditorForm.free;
  end;
end;

procedure TExpressionEditorForm.btnOkClick(Sender: TObject);
begin
  if (edtReference.text = '') and (memExpression.text = '') then
    raise EFslException.Create('Must provide at least one of reference of content');
end;

procedure TExpressionEditorForm.btnTextClick(Sender: TObject);
begin
  if exp.descriptionElement = nil then
    exp.descriptionElement := TFhirString.Create;
  editStringDialog(self, 'Expression Description', btnText, edtDescription, resource, exp.descriptionElement);
end;

procedure TExpressionEditorForm.FormDestroy(Sender: TObject);
begin
  resource.free;
  exp.free;
end;

end.
