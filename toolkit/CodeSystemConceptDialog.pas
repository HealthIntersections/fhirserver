unit CodeSystemConceptDialog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox,
  FMX.Memo, FMX.Edit, FMX.DateTimeCtrls,
  DateSupport,
  ResourceEditingSupport, FHIRTypes, FHIRResources, FHIRUtilities;

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
    procedure FormShow(Sender: TObject);
    procedure edtCodeChangeTracking(Sender: TObject);
    procedure gridDesignationsGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridDesignationsSelChanged(Sender: TObject);
    procedure gridDesignationsSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure btnDeleteDesignationClick(Sender: TObject);
    procedure btnAddDesignationClick(Sender: TObject);
  private
    FLoading : boolean;
    FConcept: TFHIRCodeSystemConcept;
    FProperties: TFhirCodeSystemPropertyList;
    procedure SetConcept(const Value: TFHIRCodeSystemConcept);
    procedure SetProperties(const Value: TFhirCodeSystemPropertyList);
    procedure loadProperties;
    procedure loadProperty(Value: TFhirCodeSystemProperty; top : Double);
  public
    Destructor Destroy; override;

    property Concept : TFHIRCodeSystemConcept read FConcept write SetConcept;
    property properties : TFhirCodeSystemPropertyList read FProperties write SetProperties;
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

destructor TCodeSystemConceptForm.Destroy;
var
  Value: TFhirCodeSystemProperty;
begin
  for value in FProperties do
    value.TagObject := nil;
  FProperties.Free;
  FConcept.Free;
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
    for prop in FProperties do
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
  end;
end;

procedure TCodeSystemConceptForm.FormShow(Sender: TObject);
var
  prop: TFhirCodeSystemProperty;
  value : TFhirCodeSystemConceptProperty;
  d : TDateTime;
begin
  FLoading := true;
  try
    edtCode.Text := Concept.code;
    edtDIsplay.Text := Concept.display;
    memDefinition.Text := Concept.definition;
    gridDesignations.RowCount := Concept.designationList.Count;
    for prop in FProperties do
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

  finally
    FLoading := false;
  end;
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

procedure TCodeSystemConceptForm.loadProperties;
var
  top : Double;
  Value: TFhirCodeSystemProperty;
const
  DELTA = 32;
begin
  top := lblProperties.Position.Y + DELTA;
  for value in FProperties do
  begin
    loadProperty(value, top);
    top := top + delta;
  end;
  Height := trunc(top + delta + 10 + panel1.height);
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
  lbl := TLabel.Create(self);
  lbl.Parent := self;
  lbl.Position.X := lblProperties.Position.X;
  lbl.Position.Y := top + 2;
  lbl.Text := value.code;
  lbl.Height := lblProperties.Height;
  lbl.Width := lblProperties.Width;

  case value.type_ of
    ConceptPropertyTypeCode, ConceptPropertyTypeString, ConceptPropertyTypeInteger:
      begin
      edt := TEdit.Create(self);
      edt.Parent := self;
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
      cb := TCheckBox.Create(self);
      cb.Parent := self;
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
      ded := TDateEdit.Create(self);
      ded.Parent := self;
      ded.Position.X := edtCode.Position.X;
      ded.Position.Y := top+2;
      value.TagObject := ded;
      ded.Height := edtCode.Height;
      ded.Width := (edtDIsplay.Position.X + edtDIsplay.Width - ded.Position.X) / 2 - 5;
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

procedure TCodeSystemConceptForm.SetConcept(const Value: TFHIRCodeSystemConcept);
begin
  FConcept.Free;
  FConcept := Value;
end;

procedure TCodeSystemConceptForm.SetProperties(const Value: TFhirCodeSystemPropertyList);
begin
  FProperties.Free;
  FProperties := Value;
  loadProperties;
end;

end.
