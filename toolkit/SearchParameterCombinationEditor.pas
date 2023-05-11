unit SearchParameterCombinationEditor;

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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.ListBox, FMX.Edit, FMX.StdCtrls, FMX.Controls.Presentation,
  fsl_base,
  
  FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Utilities,
  FMX.Layouts;

type
  TSearchParameterCombinationEditorForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Label27: TLabel;
    cbxConformance: TComboBox;
    Label1: TLabel;
    lbParameters: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FExtension: TFhirExtension;
    FParameters: TFhirCapabilityStatementRestResourceSearchParamList;

    procedure SetExtension(const Value: TFhirExtension);
    procedure SetParameters(const Value: TFhirCapabilityStatementRestResourceSearchParamList);
  public
    destructor Destroy; override;

    property Parameters : TFhirCapabilityStatementRestResourceSearchParamList read FParameters write SetParameters;
    property Extension : TFhirExtension read FExtension write SetExtension;
  end;

var
  SearchParameterCombinationEditorForm: TSearchParameterCombinationEditorForm;

implementation

{$R *.fmx}

uses
  fhir_utilities;

function IsValidSearchParam(s : String) : boolean;
begin
  result := IsId(s);
end;
{ TForm1 }

procedure TSearchParameterCombinationEditorForm.Button1Click(Sender: TObject);
var
  i, t : integer;
begin
  t := 0;
  for i := 0 to lbParameters.Items.Count - 1 do
    if lbParameters.ListItems[i].IsChecked then
      inc(t);

  if t < 2 then
    raise EFHIRException.create('At least 2 parameters must be selected');

  FExtension.removeExtension('required');
  for i := 0 to lbParameters.Items.Count - 1 do
    if lbParameters.ListItems[i].IsChecked then
      FExtension.addExtension('required', lbParameters.items[i]);


  case cbxConformance.ItemIndex of
    1: FExtension.setExtension('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation', TFHIRCode.create('SHALL'));
    2: FExtension.setExtension('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation', TFHIRCode.create('SHOULD'));
    3: FExtension.setExtension('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation', TFHIRCode.create('MAY'));
    4: FExtension.setExtension('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation', TFHIRCode.create('SHALL NOT'));
  else
    FExtension.removeExtension('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation')
  end;


end;

destructor TSearchParameterCombinationEditorForm.Destroy;
begin
  FParameters.Free;
  FExtension.Free;
  inherited;
end;

procedure TSearchParameterCombinationEditorForm.FormShow(Sender: TObject);
var
  s : String;
  p : TFhirCapabilityStatementRestResourceSearchParam;
  exl : TFslList<TFHIRExtension>;
  ex : TFhirExtension;
  i : integer;
begin
  lbParameters.Items.Clear;
  for p in FParameters do
    lbParameters.Items.Add(p.name);

  exl := FExtension.listExtensions('required');
  try
    for ex in exl do
    begin
      for i := 0 to lbParameters.Items.Count - 1 do
        if lbParameters.Items[i] = ex.value.primitiveValue then
          lbParameters.ListItems[i].IsChecked := true;
    end;
  finally
    exl.free;
  end;

  cbxConformance.ItemIndex := 0;
  if Extension.hasExtension('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation') then
  begin
    s := Extension.getExtensionString('http://hl7.org/fhir/StructureDefinition/capabilitystatement-expectation');
    if s = 'SHALL' then
      cbxConformance.ItemIndex := 1;
    if s = 'SHOULD' then
      cbxConformance.ItemIndex := 2;
    if s = 'MAY' then
      cbxConformance.ItemIndex := 3;
    if s = 'SHALL NOT' then
      cbxConformance.ItemIndex := 4;
  end;
end;

procedure TSearchParameterCombinationEditorForm.SetExtension(const Value: TFhirExtension);
begin
  FExtension.Free;
  FExtension := Value;
end;

procedure TSearchParameterCombinationEditorForm.SetParameters(const Value: TFhirCapabilityStatementRestResourceSearchParamList);
begin
  FParameters.Free;
  FParameters := Value;
end;

end.
