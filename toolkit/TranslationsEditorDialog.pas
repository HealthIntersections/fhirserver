unit TranslationsEditorDialog;

{
Copyright (c) 2018+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.Memo,
  FMX.ScrollBox, FMX.Edit, FMX.StdCtrls, FMX.Controls.Presentation,
  fsl_base, FHIR.Ui.Fmx,
  
  FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Utilities,
  ToolKitUtilities;

type
  TTranslationsEditorForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    edtPrimary: TEdit;
    Panel3: TPanel;
    Label2: TLabel;
    grid: TGrid;
    PopupColumn1: TPopupColumn;
    StringColumn1: TStringColumn;
    btnAdd: TButton;
    btnDelete: TButton;
    procedure FormShow(Sender: TObject);
    procedure gridGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure btnAddClick(Sender: TObject);
    procedure gridSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure btnDeleteClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FResource: TFHIRResource;
    FElement: TFHIRString;
    FExtensions : TFslList<TFhirExtension>;
    procedure SetElement(const Value: TFHIRString);
    procedure SetResource(const Value: TFHIRResource);
  public
    constructor Create(owner : TComponent); override;
    destructor Destroy; override;

    property Resource : TFHIRResource read FResource write SetResource;
    property Element : TFHIRString read FElement write SetElement;

  end;

var
  TranslationsEditorForm: TTranslationsEditorForm;

function editStringDialog(owner : TComponent; title : String; button : TButton; edit : TEdit; resource : TFHIRResource; element : TFHIRString) : boolean; overload;

implementation

{$R *.fmx}

function editStringDialog(owner : TComponent; title : String; button : TButton; edit : TEdit; resource : TFHIRResource; element : TFHIRString) : boolean;
begin
  TranslationsEditorForm := TTranslationsEditorForm.Create(owner);
  try
    TranslationsEditorForm.Resource := resource.Link;
    TranslationsEditorForm.Element := element.Link;
    TranslationsEditorForm.Caption := title;
    result := ShowModalHack(TranslationsEditorForm) = mrOk;
    if result then
    begin
      if edit <> nil then
        edit.Text := element.value;
      if button <> nil then
        button.ImageIndex := translationsImageIndex(element);
    end;
  finally
    TranslationsEditorForm.Free;
  end;
end;

{ TForm1 }

procedure TTranslationsEditorForm.btnAddClick(Sender: TObject);
begin
  FExtensions.Add(TFHIRExtension.Create);
  grid.RowCount := grid.RowCount + 1;
  grid.Row := grid.RowCount - 1;
  btnDelete.Enabled := true;
end;

procedure TTranslationsEditorForm.btnDeleteClick(Sender: TObject);
begin
  FExtensions.Delete(grid.Row);
  grid.RowCount := grid.RowCount - 1;
  if grid.Row = grid.RowCount then
    grid.Row := grid.RowCount - 1;
end;

procedure TTranslationsEditorForm.Button1Click(Sender: TObject);
var
  ext : TFHIRExtension;
  langs : TStringList;
  s : string;
begin
  langs := TStringList.Create;
  try
    for ext in FExtensions do
    begin
      s := ext.getExtensionString('lang');
      if s = '' then
        raise EFHIRException.create('Language missing on a translation');
      if langs.IndexOf(s) > -1 then
        raise EFHIRException.create('Duplicate translation for '+s);
      langs.Add(s);
      ext.url := 'http://hl7.org/fhir/StructureDefinition/translation';
    end;
  finally
    langs.Free;
  end;
  element.value := edtPrimary.Text;
  element.removeExtension('http://hl7.org/fhir/StructureDefinition/translation');
  for ext in FExtensions do
    element.extensionList.add(ext.Link);
  modalResult := mrOk;
end;

constructor TTranslationsEditorForm.Create;
begin
  inherited;
  FExtensions := TFslList<TFhirExtension>.create;
end;

destructor TTranslationsEditorForm.Destroy;
begin
  FExtensions.Free;
  FElement.Free;
  FResource.Free;
  inherited;
end;

procedure TTranslationsEditorForm.FormShow(Sender: TObject);
var
  ext : TFhirExtension;
  st : TStringList;
  s : String;
begin
  edtPrimary.Text := Element.primitiveValue;
  FExtensions.Clear;
  grid.RowCount := 0;
  for ext in Element.ExtensionList do
    if ext.url = 'http://hl7.org/fhir/StructureDefinition/translation' then
      FExtensions.Add(ext.Link);
  grid.RowCount := FExtensions.Count;
  btnDelete.Enabled := grid.RowCount > 0;
  st := langList;
  try
    PopupColumn1.Items.Clear;
    for s in st do
      PopupColumn1.Items.add(langDesc(s));
  finally
    st.Free;
  end;
end;

procedure TTranslationsEditorForm.gridGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
begin
  case ACol of
    0:Value := FExtensions[ARow].getExtensionString('lang');
    1:Value := FExtensions[ARow].getExtensionString('content');
  end;
end;

procedure TTranslationsEditorForm.gridSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
begin
  case ACol of
    0: FExtensions[ARow].setExtension('lang', TFHIRCode.Create(value.AsString));
    1: FExtensions[ARow].setExtension('content', TFHIRString.Create(value.AsString));
  end;
end;

procedure TTranslationsEditorForm.SetElement(const Value: TFHIRString);
begin
  FElement.Free;
  FElement := Value;
end;

procedure TTranslationsEditorForm.SetResource(const Value: TFHIRResource);
begin
  FResource.Free;
  FResource := Value;
end;

end.
