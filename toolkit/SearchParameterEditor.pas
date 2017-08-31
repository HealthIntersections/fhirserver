unit SearchParameterEditor;

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
  FHIRTypes, FHIRResources, FMX.ScrollBox, FMX.Memo, FMX.ListBox, FMX.Edit,
  FMX.StdCtrls, FMX.Controls.Presentation;

type
  TSearchParameterEditorForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edtName: TEdit;
    edtDefinition: TEdit;
    cbxType: TComboBox;
    mDocumentation: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FParam: TFhirCapabilityStatementRestResourceSearchParam;
    procedure SetParam(const Value: TFhirCapabilityStatementRestResourceSearchParam);
  public
    destructor Destroy; override;
    property Param : TFhirCapabilityStatementRestResourceSearchParam read FParam write SetParam;
  end;

var
  SearchParameterEditorForm: TSearchParameterEditorForm;

implementation

{$R *.fmx}

{ TForm1 }

procedure TSearchParameterEditorForm.Button1Click(Sender: TObject);
begin
  param.name := edtName.text;
  param.type_ := TFhirSearchParamTypeEnum(cbxType.ItemIndex);
  param.definition := edtDefinition.Text;
  param.documentation := mDocumentation.Text;
  if not IsValidIdent(param.name) then
    raise Exception.Create('The parameter name "'+param.name+'" is not valid');
  if param.type_ = SearchParamTypeNull then
    raise Exception.Create('Please choose a parameter type');
end;

destructor TSearchParameterEditorForm.Destroy;
begin
  FParam.Free;
  inherited;
end;

procedure TSearchParameterEditorForm.FormShow(Sender: TObject);
begin
  edtName.text := param.name;
  cbxType.ItemIndex := ord(param.type_);
  edtDefinition.Text := param.definition;
  mDocumentation.Text := param.documentation;
end;

procedure TSearchParameterEditorForm.SetParam(const Value: TFhirCapabilityStatementRestResourceSearchParam);
begin
  FParam.Free;
  FParam := Value;
end;

end.
