unit CodeSystemConceptDialog;

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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox,
  FMX.Memo, FMX.Edit, FMX.DateTimeCtrls, System.ImageList, FMX.ImgList,
  FHIR.Support.Utilities,
  ResourceEditingSupport, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Utilities,
  ToolkitUtilities, TranslationsEditorDialog, MemoEditorDialog, FMX.TabControl, ResourceHistoryDialog;

type
  TCodeSystemConceptForm = class(TForm)
    Panel1: TPanel;
    btnOk: TButton;
    Button2: TButton;
    Label1: TLabel;
    edtCode: TEdit;
    Label2: TLabel;
    edtDIsplay: TEdit;
    Label3: TLabel;
    memDefinition: TMemo;
    btnDeleteDesignation: TButton;
    btnAddDesignation: TButton;
    gridDesignations: TGrid;
    PopupColumn2: TPopupColumn;
    PopupColumn3: TPopupColumn;
    StringColumn9: TStringColumn;
    Label20: TLabel;
    lblProperties: TLabel;
    ToolbarImages: TImageList;
    btnDisplay: TButton;
    btnDefinition: TButton;
    TabControl1: TTabControl;
    tbGeneral: TTabItem;
    tbHl7: TTabItem;
    Label18: TLabel;
    gridHistory: TGrid;
    DateColumn1: TDateColumn;
    StringColumn18: TStringColumn;
    StringColumn19: TStringColumn;
    btnHistoryAdd: TButton;
    btnHistoryEdit: TButton;
    btnHistoryUp: TButton;
    btnHistoryDown: TButton;
    btnHistoryDelete: TButton;
    Label4: TLabel;
    memOpenIssues: TMemo;
    Label5: TLabel;
    CheckColumn1: TCheckColumn;
    CheckColumn2: TCheckColumn;
    procedure FormShow(Sender: TObject);
    procedure edtCodeChangeTracking(Sender: TObject);
    procedure gridDesignationsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridDesignationsSelChanged(Sender: TObject);
    procedure gridDesignationsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure btnDeleteDesignationClick(Sender: TObject);
    procedure btnAddDesignationClick(Sender: TObject);
    procedure btnDisplayClick(Sender: TObject);
    procedure btnDefinitionClick(Sender: TObject);
    procedure btnHistoryAddClick(Sender: TObject);
    procedure btnHistoryEditClick(Sender: TObject);
    procedure btnHistoryUpClick(Sender: TObject);
    procedure btnHistoryDownClick(Sender: TObject);
    procedure btnHistoryDeleteClick(Sender: TObject);
    procedure gridHistoryGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridHistorySelChanged(Sender: TObject);
    procedure gridHistorySetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
  private
    FLoading : boolean;
    FConcept: TFHIRCodeSystemConcept;
    FCodeSystem: TFhirCodeSystem;
    FHistory : TFhirExtensionList;
    procedure SetConcept(const Value: TFHIRCodeSystemConcept);
    procedure loadProperties;
    procedure loadProperty(Value: TFhirCodeSystemProperty; top : Double);
    procedure SetCodeSystem(const Value: TFhirCodeSystem);
  public
    destructor Destroy; override;

    property Concept : TFHIRCodeSystemConcept read FConcept write SetConcept;
    property CodeSystem : TFhirCodeSystem read FCodeSystem write SetCodeSystem;
  end;

var
  CodeSystemConceptForm: TCodeSystemConceptForm;

implementation

{$R *.fmx}

{ TCodeSystemConceptForm }

procedure TCodeSystemConceptForm.btnAddDesignationClick(Sender: TObject);
begin
  concept.designationList.Append.language := 'en';
  gridDesignations.RowCount := 0;
  gridDesignations.RowCount := concept.designationList.count;
  btnDeleteDesignation.Enabled := gridDesignations.Row > -1;
  btnOk.Enabled := true;
end;

procedure TCodeSystemConceptForm.btnDefinitionClick(Sender: TObject);
begin
  if Concept.definitionElement = nil then
    Concept.definitionElement := TFhirString.Create;
  editMarkdownDialog(self, 'Concept Definition', btnDefinition, memDefinition, CodeSystem, Concept.definitionElement);
end;

procedure TCodeSystemConceptForm.btnDeleteDesignationClick(Sender: TObject);
var
  designation : TFhirValueSetComposeIncludeConceptDesignation;
begin
  concept.designationList.Remove(gridDesignations.Row);
  gridDesignations.RowCount := 0;
  gridDesignations.RowCount := concept.designationList.count;
  if gridDesignations.Row >= concept.designationList.count then
    gridDesignations.Row := gridDesignations.Row - 1;
  btnDeleteDesignation.Enabled := gridDesignations.Row > -1;
  btnOk.Enabled := true;
end;

procedure TCodeSystemConceptForm.btnDisplayClick(Sender: TObject);
begin
  if Concept.displayElement = nil then
    Concept.displayElement := TFhirString.Create;
  editStringDialog(self, 'Concept Display', btnDisplay, edtDIsplay, CodeSystem, Concept.displayElement);
end;

procedure TCodeSystemConceptForm.btnHistoryAddClick(Sender: TObject);
var
  ext : TFhirExtension;
  frm : TResourceHistoryForm;
begin
  ext := TFhirExtension.Create;
  try
    ext.url := 'http://hl7.org/fhir/StructureDefinition/concept-history';
    ext.setExtensionDate('date', TDateTimeEx.makeToday.toXML);
    frm := TResourceHistoryForm.create(self);
    try
       frm.Adapt([hfDate, hfAuthor, hfSubst, hfBreaking, hfNotes]);
       frm.Extension := ext.Link;
       if frm.showModal = mrOk then
       begin
         FHistory.InsertItem(0, ext.link);
         gridHistory.RowCount := 0;
         gridHistory.RowCount := FHistory.count;
         edtCodeChangeTracking(nil);
       end;
    finally
      frm.free;
    end;
  finally
    ext.Free;
  end;
end;

procedure TCodeSystemConceptForm.btnHistoryDeleteClick(Sender: TObject);
begin
  FHistory.Exchange(gridHistory.Row, gridHistory.Row + 1);
  gridHistory.RowCount := 0;
  gridHistory.RowCount := FHistory.count;
  edtCodeChangeTracking(nil);
end;

procedure TCodeSystemConceptForm.btnHistoryDownClick(Sender: TObject);
begin
  FHistory.Exchange(gridHistory.Row, gridHistory.Row + 1);
  gridHistory.RowCount := 0;
  gridHistory.RowCount := FHistory.count;
  edtCodeChangeTracking(nil);
end;

procedure TCodeSystemConceptForm.btnHistoryEditClick(Sender: TObject);
var
  ext : TFhirExtension;
  frm : TResourceHistoryForm;
begin
  ext := FHistory[gridHistory.Row];
  frm := TResourceHistoryForm.create(self);
  try
     frm.Adapt([hfDate, hfAuthor, hfSubst, hfBreaking, hfNotes]);
     frm.Extension := ext.Link;
     if frm.showModal = mrOk then
     begin
       edtCodeChangeTracking(nil);
       gridHistory.RowCount := 0;
       gridHistory.RowCount := FHistory.count;
     end;
  finally
    frm.free;
  end;
end;

procedure TCodeSystemConceptForm.btnHistoryUpClick(Sender: TObject);
begin
  FHistory.Exchange(gridHistory.Row, gridHistory.Row - 1);
  gridHistory.RowCount := 0;
  gridHistory.RowCount := FHistory.count;
  edtCodeChangeTracking(nil);
end;

destructor TCodeSystemConceptForm.Destroy;
var
  Value: TFhirCodeSystemProperty;
begin
  for value in FCodeSystem.property_List do
    value.TagObject := nil;
  FCodeSystem.Free;
  FConcept.Free;
  FHistory.Free;
  inherited;
end;

procedure TCodeSystemConceptForm.edtCodeChangeTracking(Sender: TObject);
var
  prop: TFhirCodeSystemProperty;
  value : TFhirCodeSystemConceptProperty;
  s : String;
  d : TFHIRDateTime;
begin
  if not FLoading then
  begin
    btnOk.Enabled := true;
    Concept.code := edtCode.Text;
    Concept.display := edtDIsplay.Text;
    Concept.definition := memDefinition.Text;
    for prop in FCodeSystem.property_List do
    begin
      value := concept.prop(prop.code);
      case prop.type_ of
        ConceptPropertyTypeCode, ConceptPropertyTypeString, ConceptPropertyTypeInteger:
          begin
          s := TEdit(prop.TagObject).Text;
          if s <> '' then
          begin
            if value = nil then
              value := concept.addProp(prop.code);
            value.value := TFhirString.Create(s);
          end
          else if value <> nil then
            concept.deleteProp(prop.code);
          end;
        ConceptPropertyTypeBoolean:
          if TCheckBox(prop.TagObject).IsChecked then
          begin
            if value = nil then
              value := concept.addProp(prop.code);
            value.value := TFhirBoolean.Create(true);
          end
          else if value <> nil then
            value.value := TFhirBoolean.Create(false);
        ConceptPropertyTypeDateTime:
          begin
          d := storeDateTime(TDateEdit(prop.TagObject), TTimeEdit(TDateEdit(prop.TagObject).TagObject));
          if d <> nil then
          begin
            if value = nil then
              value := concept.addProp(prop.code);
            value.value := d;
          end
          else if value <> nil then
            concept.deleteProp(prop.code);
          end;
//          ConceptPropertyTypeCoding: ;
      end;
    end;
    Concept.removeExtension('http://hl7.org/fhir/StructureDefinition/concept-history');
    Concept.extensionList.AddAll(FHistory);
    Concept.removeExtension('http://hl7.org/fhir/StructureDefinition/concept-openIssue');
    for s in memOpenIssues.Lines do
      if s <> '' then
        Concept.addExtension('http://hl7.org/fhir/StructureDefinition/concept-openIssue', s);
  end;
end;

procedure TCodeSystemConceptForm.FormShow(Sender: TObject);
var
  prop: TFhirCodeSystemProperty;
  value : TFhirCodeSystemConceptProperty;
  ext : TFhirExtension;
  d : TDateTime;
begin
  FLoading := true;
  try
    edtCode.Text := Concept.code;
    edtDIsplay.Text := Concept.display;
    btnDisplay.ImageIndex := translationsImageIndex(Concept.displayElement);
    memDefinition.Text := Concept.definition;
    btnDefinition.ImageIndex := translationsImageIndex(Concept.definitionElement);
    gridDesignations.RowCount := Concept.designationList.Count;
    for prop in FCodeSystem.property_List do
    begin
      value := concept.prop(prop.code);
      if value <> nil then
      begin
        case prop.type_ of
          ConceptPropertyTypeCode, ConceptPropertyTypeString, ConceptPropertyTypeInteger:
            TEdit(prop.TagObject).Text := value.value.primitiveValue;
          ConceptPropertyTypeBoolean:
            TCheckBox(prop.TagObject).IsChecked := StrToBoolDef(value.value.primitiveValue, false);
          ConceptPropertyTypeDateTime:
            begin
              d := TFhirDateTime(value.value).value.DateTime;
              TDateEdit(prop.TagObject).DateTime := trunc(d);
              if d - trunc(d) <> 0 then
                TTimeEdit(TDateEdit(prop.TagObject).TagObject).DateTime := d - trunc(d);
            end;
//          ConceptPropertyTypeCoding: ;
        end;
      end;
    end;
    if FHistory = nil then
      FHistory := TFhirExtensionList.Create
    else
      FHistory.clear;
    memOpenIssues.Text := '';
    for ext in Concept.extensionList do
    begin
      if ext.url = 'http://hl7.org/fhir/StructureDefinition/concept-history' then
        FHistory.Add(ext.Link);
      if ext.url = 'http://hl7.org/fhir/StructureDefinition/concept-openIssue' then
        memOpenIssues.lines.Add(ext.value.primitiveValue);
    end;
    gridHistory.RowCount := FHistory.Count;

  finally
    FLoading := false;
  end;
  TabControl1.ActiveTab := tbGeneral;
end;

procedure TCodeSystemConceptForm.gridDesignationsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  designation : TFhirCodeSystemConceptDesignation;
begin
  designation := concept.designationList[aRow];
  case aCol of
    0: value := displayLang(designation.language);
    1: value := displayUse(designation.use);
    2: value := designation.value
  end;
end;

procedure TCodeSystemConceptForm.gridDesignationsSelChanged(Sender: TObject);
begin
  btnDeleteDesignation.Enabled := gridDesignations.Row > 1;
end;

procedure TCodeSystemConceptForm.gridDesignationsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  designation : TFhirCodeSystemConceptDesignation;
  s : string;
begin
  designation := concept.designationList[aRow];
  s := value.AsString;
  case aCol of
    0: if s = '' then designation.language := '' else designation.language := s.Substring(0, 2);
    1: if s = '' then designation.use := nil else designation.use := TFHIRCoding.Create('http://snomed.info/sct', codeForUse(s));
    2: designation.value := s;
  end;
  btnOk.Enabled := true;
end;

procedure TCodeSystemConceptForm.gridHistoryGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  ext : TFhirExtension;
begin
  ext := FHistory[ARow];
  Value := '';
  case aCol of
    0 { date }: Value := ext.getExtensionDateAsString('date');
    1 { editor }: Value := ext.getExtensionString('author');
    2 { subst? }: Value := ext.getExtensionBoolean('substantive');
    3 { breaking? }: Value := not ext.getExtensionBoolean('backwardCompatible');
    4 { notes }: Value := ext.getExtensionString('notes');
  end;
end;

procedure TCodeSystemConceptForm.gridHistorySelChanged(Sender: TObject);
begin
  btnHistoryAdd.Enabled := true;
  btnHistoryEdit.Enabled := gridHistory.Row > -1;
  btnHistoryUp.Enabled := gridHistory.Row > 0;
  btnHistoryDown.Enabled := gridHistory.Row < gridHistory.RowCount-1;
  btnHistoryDelete.Enabled := gridHistory.Row > -1;
end;

procedure TCodeSystemConceptForm.gridHistorySetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  ext : TFhirExtension;
begin
  ext := FHistory[ARow];
  case aCol of
    0 { date }: ext.setExtensionDate('date', value.AsString);
    1 { editor }: ext.setExtensionString('author', value.AsString);
    2 { subst }: ext.setExtensionBoolean('substantive', value.AsBoolean);
    3 { breaking }: ext.setExtensionBoolean('backwardCompatible', not value.AsBoolean);
    4 { notes }: ext.setExtensionString('notes', value.AsString);
  end;
end;

procedure TCodeSystemConceptForm.loadProperties;
var
  top : Double;
  Value: TFhirCodeSystemProperty;
const
  DELTA = 32;
begin
  top := lblProperties.Position.Y + DELTA;
  for value in FCodeSystem.property_List do
  begin
    loadProperty(value, top);
    top := top + delta;
  end;
  Height := trunc(top + delta + 10 + panel1.height)+20;
  if top = lblProperties.Position.Y + DELTA then
    lblProperties.visible := false;

end;

procedure TCodeSystemConceptForm.loadProperty(Value: TFhirCodeSystemProperty; top : Double);
var
  lbl : TLabel;
  edt : TEdit;
  cb : TCheckBox;
  ded : TDateEdit;
  ted : TTimeEdit;
begin
  lbl := TLabel.Create(tbGeneral);
  lbl.Parent := tbGeneral;
  lbl.Position.X := lblProperties.Position.X;
  lbl.Position.Y := top + 2;
  lbl.Text := value.code;
  lbl.Height := lblProperties.Height;
  lbl.Width := lblProperties.Width;

  case value.type_ of
    ConceptPropertyTypeCode, ConceptPropertyTypeString, ConceptPropertyTypeInteger:
      begin
      edt := TEdit.Create(tbGeneral);
      edt.Parent := tbGeneral;
      edt.Position.X := edtCode.Position.X;
      edt.Position.Y := top;
      value.TagObject := edt;
      edt.Height := edtCode.Height;
      edt.Width := edtDIsplay.Position.X + edtDIsplay.Width - edt.Position.X;
      edt.Hint := value.description;
      edt.OnChangeTracking := edtCodeChangeTracking;
      edt.OnChange := edtCodeChangeTracking;
      end;
    ConceptPropertyTypeBoolean :
      begin
      cb := TCheckBox.Create(tbGeneral);
      cb.Parent := tbGeneral;
      cb.Position.X := edtCode.Position.X;
      cb.Position.Y := top+2;
      value.TagObject := cb;
      cb.Height := edtCode.Height;
      cb.Width := edtDIsplay.Position.X + edtDIsplay.Width - cb.Position.X;
      cb.Hint := value.description;
      cb.OnChange := edtCodeChangeTracking;
      cb.OnClick := edtCodeChangeTracking;
      end;
//    ConceptPropertyTypeCoding, {@enum.value ConceptPropertyTypeCoding  }
    ConceptPropertyTypeDateTime: {@enum.value ConceptPropertyTypeDateTime  }
      begin
      ded := TDateEdit.Create(tbGeneral);
      ded.Parent := tbGeneral;
      ded.Position.X := edtCode.Position.X;
      ded.Position.Y := top+2;
      value.TagObject := ded;
      ded.Height := edtCode.Height;
      ded.Width := (edtDisplay.Position.X + edtDIsplay.Width - ded.Position.X) / 2 - 5;
      ded.Hint := value.description;
      ded.OnChange := edtCodeChangeTracking;
      ded.OnClick := edtCodeChangeTracking;
      ded.Text := '';
      ted := TTimeEdit.Create(self);
      ted.Parent := self;
      ted.Position.X := ded.Position.X + ded.width + 10;
      ted.Position.Y := top+2;
      ded.TagObject := ted;
      ted.Height := edtCode.Height;
      ted.Width := edtDIsplay.Position.X + edtDIsplay.Width - ted.Position.X;
      ted.Hint := value.description;
      ted.OnChange := edtCodeChangeTracking;
      ted.OnClick := edtCodeChangeTracking;
      ted.Text := '';
      end;
  end;
end;

procedure TCodeSystemConceptForm.SetCodeSystem(const Value: TFhirCodeSystem);
begin
  FCodeSystem.Free;
  FCodeSystem := Value;
  loadProperties;
end;

procedure TCodeSystemConceptForm.SetConcept(const Value: TFHIRCodeSystemConcept);
begin
  FConcept.Free;
  FConcept := Value;
end;

end.
