unit UsageContextForm;

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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Edit, FMX.ListBox, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts,
  
  FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Utilities, FHIR.Version.Client,
  ToolkitUtilities, BaseDialog;

type
  TForm = TBaseForm;

  TUsageContextDialog = class(TForm)
    Panel1: TPanel;
    Panel3: TPanel;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    cbxCommon: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    edtSystem: TEdit;
    edtCode: TEdit;
    Label5: TLabel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Label6: TLabel;
    edtCCText: TEdit;
    Label7: TLabel;
    lbCodes: TListBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    edtQValue: TEdit;
    edtQUnit: TEdit;
    edtQCode: TEdit;
    cbxQSystem: TComboBox;
    Label11: TLabel;
    edtRangeUnit: TEdit;
    Label12: TLabel;
    edtRangeHigh: TEdit;
    Label13: TLabel;
    edtRangeCode: TEdit;
    cbxRangeSystem: TComboBox;
    Label14: TLabel;
    edtRangeLow: TEdit;
    lblErr: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cbxCommonChange(Sender: TObject);
  private
    FUsageContext: TFhirUsageContext;
    FLookup : TCodeableConceptLookup;
    procedure SetUsageContext(const Value: TFhirUsageContext);
  public
    destructor Destroy; override;

    property UsageContext : TFhirUsageContext read FUsageContext write SetUsageContext;
  end;

var
  UsageContextDialog: TUsageContextDialog;

implementation

{$R *.fmx}

{ TForm1 }

procedure TUsageContextDialog.Button2Click(Sender: TObject);
var
  cc : TFhirCodeableConcept;
  qty : TFhirQuantity;
  rng : TFhirRange;
begin
  case cbxCommon.ItemIndex of
    0 : FUsageContext.code := TFHIRCoding.create('http://hl7.org/fhir/usage-context-type', 'gender');
    1 : FUsageContext.code := TFHIRCoding.create('http://hl7.org/fhir/usage-context-type', 'age');
    2 : FUsageContext.code := TFHIRCoding.create('http://hl7.org/fhir/usage-context-type', 'focus');
    3 : FUsageContext.code := TFHIRCoding.create('http://hl7.org/fhir/usage-context-type', 'user');
    4 : FUsageContext.code := TFHIRCoding.create('http://hl7.org/fhir/usage-context-type', 'workflow');
    5 : FUsageContext.code := TFHIRCoding.create('http://hl7.org/fhir/usage-context-type', 'task');
    6 : FUsageContext.code := TFHIRCoding.create('http://hl7.org/fhir/usage-context-type', 'venue');
    7 : FUsageContext.code := TFHIRCoding.create('http://hl7.org/fhir/usage-context-type', 'species');
    8 :
      begin
        if (edtSystem.Text <> '') or (edtCode.Text <> '') then
          FUsageContext.code := TFHIRCoding.create(edtSystem.Text, edtCode.Text)
        else
         raise EFHIRException.create('Error: must provide a system/code');
      end;
  end;

  if TabControl1.ActiveTab = TabItem1 then
  begin
    if (FUsageContext.value = nil) or not (FUsageContext.value is TFhirCodeableConcept) then
      FUsageContext.value := TFhirCodeableConcept.Create;
    cc := FUsageContext.value as TFhirCodeableConcept;
    cc.text := edtCCText.text;
  end
  else if TabControl1.ActiveTab = TabItem2 then
  begin
    if (FUsageContext.value = nil) or not (FUsageContext.value is TFhirQuantity) then
      FUsageContext.value := TFhirQuantity.Create;
    qty := FUsageContext.value as TFhirQuantity;
    qty.value := edtQValue.text;
    qty.unit_ := edtQUnit.text;
    qty.code := edtQCode.text;
    case cbxQSystem.ItemIndex of
      0: qty.system := 'http://unitsofmeasure.org';
      1: qty.system := 'http://snomed.info/sct';
    end;
  end
  else if TabControl1.ActiveTab = TabItem3 then
  begin
    if (FUsageContext.value = nil) or not (FUsageContext.value is TFhirRange) then
      FUsageContext.value := TFhirRange.Create;
    rng := FUsageContext.value as TFhirRange;
    if edtRangeLow.text <> '' then
    begin
      if rng.low = nil then
        rng.low := TFhirQuantity.Create;
      rng.low.value := edtRangeLow.text;
      rng.low.unit_ := edtRangeUnit.text;
      rng.low.code := edtRangeCode.text;
      case cbxRangeSystem.ItemIndex of
        0: rng.low.system := 'http://unitsofmeasure.org';
        1: rng.low.system := 'http://snomed.info/sct';
      end;
    end;
    if edtRangeHigh.text <> '' then
    begin
      if rng.high = nil then
        rng.high := TFhirQuantity.Create;
      rng.high.value := edtRangeHigh.text;
      rng.high.unit_ := edtRangeUnit.text;
      rng.high.code := edtRangeCode.text;
      case cbxRangeSystem.ItemIndex of
        0: rng.high.system := 'http://unitsofmeasure.org';
        1: rng.high.system := 'http://snomed.info/sct';
      end;
    end;
  end;

  ModalResult := mrOk;
end;

procedure TUsageContextDialog.cbxCommonChange(Sender: TObject);
begin
  edtSystem.enabled := cbxCommon.ItemIndex = 8;
  edtCode.enabled := cbxCommon.ItemIndex = 8;
  FLookup.ValueSet := '';
  case cbxCommon.ItemIndex of
    0: FLookup.ValueSet := 'http://hl7.org/fhir/ValueSet/administrative-gender';
    2: FLookup.ValueSet := 'http://hl7.org/fhir/ValueSet/condition-code';
    3: FLookup.ValueSet := 'http://hl7.org/fhir/ValueSet/provider-taxonomy';
    4: FLookup.ValueSet := 'http://hl7.org/fhir/ValueSet/v3-ActEncounterCode';
    5: FLookup.ValueSet := 'http://hl7.org/fhir/ValueSet/v3-ActTaskCode';
    6: FLookup.ValueSet := 'http://hl7.org/fhir/ValueSet/v3-ServiceDeliveryLocationRoleType';
    7: FLookup.ValueSet := 'http://hl7.org/fhir/us/sdc/ValueSet/species';
  end
end;

destructor TUsageContextDialog.Destroy;
begin
  FUsageContext.Free;
  FLookup.Free;
  inherited;
end;

procedure TUsageContextDialog.FormShow(Sender: TObject);
var
  qty : TFhirQuantity;
  rng : TFhirRange;
begin
  FLookup := TCodeableConceptLookup.create(edtCCText, lbCodes, lblErr,
    TFhirClients.makeHTTP(nil, Settings.defaultAddress('Terminology'),
      false, Settings.timeout * 1000, Settings.proxy));

  edtSystem.text := '';
  edtCode.text := '';
  cbxCommon.ItemIndex := -1;

  if FUsageContext.code <> nil then
  begin
    if FUsageContext.code.hasCode('http://hl7.org/fhir/usage-context-type', 'gender') then
      cbxCommon.ItemIndex := 0
    else if FUsageContext.code.hasCode('http://hl7.org/fhir/usage-context-type', 'age') then
      cbxCommon.ItemIndex := 1
    else if FUsageContext.code.hasCode('http://hl7.org/fhir/usage-context-type', 'focus') then
      cbxCommon.ItemIndex := 2
    else if FUsageContext.code.hasCode('http://hl7.org/fhir/usage-context-type', 'user') then
      cbxCommon.ItemIndex := 3
    else if FUsageContext.code.hasCode('http://hl7.org/fhir/usage-context-type', 'workflow') then
      cbxCommon.ItemIndex := 4
    else if FUsageContext.code.hasCode('http://hl7.org/fhir/usage-context-type', 'task') then
      cbxCommon.ItemIndex := 5
    else if FUsageContext.code.hasCode('http://hl7.org/fhir/usage-context-type', 'venue') then
      cbxCommon.ItemIndex := 6
    else if FUsageContext.code.hasCode('http://hl7.org/fhir/usage-context-type', 'species') then
      cbxCommon.ItemIndex := 7
    else
    begin
      cbxCommon.ItemIndex := 8;
      edtSystem.text := FUsageContext.code.code;
      edtCode.text := FUsageContext.code.system;
    end;
  end;
  cbxCommonChange(self);

  edtCCText.text := '';
  edtQValue.text := '';
  edtQUnit.text := '';
  edtQCode.text := '';
  cbxQSystem.ItemIndex := 0;
  edtRangeLow.text := '';
  edtRangeHigh.text := '';
  edtRangeUnit.text := '';
  edtRangeCode.text := '';
  cbxRangeSystem.ItemIndex := 0;


  TabControl1.ActiveTab := TabItem1;
  if FUsageContext.value <> nil then
  begin
    if FUsageContext.value is TFhirCodeableConcept then
    begin
      TabControl1.ActiveTab := TabItem1;
      edtCCText.text := (FUsageContext.value as TFhirCodeableConcept).text;
    end
    else if FUsageContext.value is TFhirQuantity then
    begin
      TabControl1.ActiveTab := TabItem2;
      qty := FUsageContext.value as TFhirQuantity;
      edtQValue.text := qty.value;
      edtQUnit.text := qty.unit_;
      edtQCode.text := qty.code;
      if qty.system = 'http://unitsofmeasure.org' then
        cbxQSystem.ItemIndex := 0
      else if qty.system = 'http://snomed.info/sct' then
        cbxQSystem.ItemIndex := 1
      else
        cbxQSystem.ItemIndex := 0;
    end
    else if FUsageContext.value is TFhirRange then
    begin
      TabControl1.ActiveTab := TabItem3;
      rng := FUsageContext.value as TFhirRange;
      if (rng.low <> nil) then
        edtRangeLow.text := rng.low.value;
      if (rng.high <> nil) then
        edtRangeHigh.text := rng.high.value;
      if (rng.either <> nil) then
      begin
        edtRangeUnit.text := rng.either.unit_;
        edtRangeCode.text := rng.either.code;
        if rng.either.system = 'http://unitsofmeasure.org' then
          cbxRangeSystem.ItemIndex := 0
        else if rng.either.system = 'http://snomed.info/sct' then
          cbxRangeSystem.ItemIndex := 1
        else
          cbxRangeSystem.ItemIndex := 0;
      end;
    end
  end;
end;

procedure TUsageContextDialog.SetUsageContext(const Value: TFhirUsageContext);
begin
  FUsageContext.Free;
  FUsageContext := Value;
end;

end.
