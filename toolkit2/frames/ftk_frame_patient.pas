unit ftk_frame_patient;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Graphics,
  Menus, ExtDlgs, IntfGraphics, Buttons, DateTimePicker, FPImage, FPWriteBMP,
  HtmlView,
  fsl_base, fsl_utilities, fsl_json, fsl_crypto,
  fhir_objects, fhir_parser, fhir_healthcard, fhir_common, fhir_factory,
  fui_lcl_managers,
  ftk_context, ftk_constants,
  ftk_frame_resource,
  dlg_gender_identity;

type
  TFrame = TResourceDesignerFrame;
  TPatientFrame = class;

  { THealthcardManager }

  THealthcardManager = class (TListManager<THealthcareCard>)
  private
    FFrame : TPatientFrame;
  public
    function canSort : boolean; override;
    function doubleClickEdit : boolean; override;
    function allowedOperations(item : THealthcareCard) : TNodeOperationSet; override;
    function loadList : boolean; override;

    procedure buildMenu; override;
    function getCellText(item : THealthcareCard; col : integer) : String; override;
    function getSummaryText(item : THealthcareCard) : String; override;
    function compareItem(left, right : THealthcareCard; col : integer) : integer; override;
    function filterItem(item : THealthcareCard; s : String) : boolean; override;

    function executeItem(item : THealthcareCard; mode : String) : boolean; override;
  end;

  { TGenderIdentityManager }

  TGenderIdentityManager = class (TListManager<TFHIRExtensionW>)
  private
    FFrame : TPatientFrame;
  public
    function canSort : boolean; override;
    function allowedOperations(item : TFHIRExtensionW) : TNodeOperationSet; override;
    function loadList : boolean; override;

    function getCellText(item : TFHIRExtensionW; col : integer) : String; override;
    function getSummaryText(item : TFHIRExtensionW) : String; override;
    function compareItem(left, right : TFHIRExtensionW; col : integer) : integer; override;
    function addItem(mode : String) : TFHIRExtensionW; override;
    function editItem(item : TFHIRExtensionW; mode : String) : boolean; override;
    function deleteItem(item : TFHIRExtensionW) : boolean; override;
  end;

  { TPronounManager }

   TPronounManager = class (TListManager<TFHIRExtensionW>)
   private
     FFrame : TPatientFrame;
   public
     function canSort : boolean; override;
     function allowedOperations(item : TFHIRExtensionW) : TNodeOperationSet; override;
     function loadList : boolean; override;

     function getCellText(item : TFHIRExtensionW; col : integer) : String; override;
     function getSummaryText(item : TFHIRExtensionW) : String; override;
     function compareItem(left, right : TFHIRExtensionW; col : integer) : integer; override;
   end;

  { TSFCUManager }

   TSFCUManager = class (TListManager<TFHIRExtensionW>)
   private
     FFrame : TPatientFrame;
   public
     function canSort : boolean; override;
     function allowedOperations(item : TFHIRExtensionW) : TNodeOperationSet; override;
     function loadList : boolean; override;

     function getCellText(item : TFHIRExtensionW; col : integer) : String; override;
     function getSummaryText(item : TFHIRExtensionW) : String; override;
     function compareItem(left, right : TFHIRExtensionW; col : integer) : integer; override;
   end;

   { TRecordedSexManager }

   TRecordedSexManager = class (TListManager<TFHIRExtensionW>)
   private
     FFrame : TPatientFrame;
   public
     function canSort : boolean; override;
     function allowedOperations(item : TFHIRExtensionW) : TNodeOperationSet; override;
     function loadList : boolean; override;

     function getCellText(item : TFHIRExtensionW; col : integer) : String; override;
     function getSummaryText(item : TFHIRExtensionW) : String; override;
     function compareItem(left, right : TFHIRExtensionW; col : integer) : integer; override;
   end;

  { TPatientFrame }

  TPatientFrame = class(TFrame)
    btnAddGenderId: TBitBtn;
    btnAddPronoun: TBitBtn;
    btnAddSFCU: TBitBtn;
    btnAddRecord: TBitBtn;
    btnDeleteGenderId: TBitBtn;
    btnDeletePronoun: TBitBtn;
    btnDeleteSFCU: TBitBtn;
    btnDeleteRecord: TBitBtn;
    btnEditGenderid: TBitBtn;
    btnEditPronoun: TBitBtn;
    btnEditSFCU: TBitBtn;
    btnEditRecord: TBitBtn;
    btnFetchHealthCards: TButton;
    cbCovidOnly: TCheckBox;
    cbActive: TCheckBox;
    cbxGender: TComboBox;
    edtDoB: TDateTimePicker;
    edtNameSummary: TEdit;
    gbGenderIdentity: TGroupBox;
    gbPronouns: TGroupBox;
    gbSexForClinicalUse: TGroupBox;
    gbRecordedSexOrGender: TGroupBox;
    htmlCard: THtmlViewer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    lvGenderId: TListView;
    lvPronouns: TListView;
    lvSFCU: TListView;
    lvRecordedSex: TListView;
    lvCards: TListView;
    mnuSaveQR: TMenuItem;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    pnlAdminGender: TPanel;
    pnlPatientDetails: TPanel;
    pbCard: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlHealthcardsOutome: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    pmImage: TPopupMenu;
    sd: TSavePictureDialog;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    procedure btnFetchHealthCardsClick(Sender: TObject);
    procedure mnuSaveQRClick(Sender: TObject);
    procedure Panel4Resize(Sender: TObject);
    procedure pbCardPaint(Sender: TObject);
    procedure TabSheet4Resize(Sender: TObject);
  private
    FFactory : TFHIRFactory;
    FCardManager : THealthcardManager;
    FGenderIdManager : TGenderIdentityManager;
    FPronounsManager : TPronounManager;
    FSFCUManager : TSFCUManager;
    FRecordedSexManager : TRecordedSexManager;
    FCards : TFHIRParametersW;
    FPatient : TFhirPatientW;
    FExtensions : TFslList<TFHIRExtensionW>;

    procedure DoSelectCard(sender : TObject);
  public
    destructor Destroy; override;

    procedure initialize; override;
    procedure bind; override;
    procedure saveStatus; override;
  end;

implementation

{$R *.lfm}

function getDisplay(cc : TFHIRCodeableConceptW) : String;
var
  c : TFhirCodingW;
begin
  if cc = nil then
    result := ''
  else if cc.hasCode('http://snomed.info/sct', '446141000124107') then
    result := 'Female'
  else if cc.hasCode('http://snomed.info/sct', '446151000124109') then
    result := 'Male'
  else if cc.hasCode('http://snomed.info/sct', '33791000087105') then
    result := 'Non-Binary'
  else if cc.hasCode('http://terminology.hl7.org/CodeSystem/v3-NullFlavor', 'UNK') then
    result := 'Unknown'

  else if cc.hasCode('http://loinc.org', 'LA29518-0') then
    result := 'He/Him'
  else if cc.hasCode('http://loinc.org', 'LA29519-8') then
    result := 'She/Her'
  else if cc.hasCode('http://loinc.org', 'LA29520-6') then
    result := 'They/Them'

  else if cc.hasCode('http://terminology.hl7.org/CodeSystem/sex-for-clinical-use', 'female') then
    result := 'Female'
  else if cc.hasCode('http://terminology.hl7.org/CodeSystem/sex-for-clinical-use', 'male') then
    result := 'Male'
  else if cc.hasCode('http://terminology.hl7.org/CodeSystem/sex-for-clinical-use', 'specified') then
    result := 'As specified'
  else if cc.hasCode('http://terminology.hl7.org/CodeSystem/sex-for-clinical-use', 'unknown') then
    result := 'Unknown'

  else if cc.hasCode('http://hl7.org/fhir/administrative-gender', 'female') then
    result := 'Female'
  else if cc.hasCode('http://hl7.org/fhir/administrative-gender', 'male') then
    result := 'Male'
  else if cc.hasCode('http://hl7.org/fhir/administrative-gender', 'other') then
    result := 'Other'
  else if cc.hasCode('http://hl7.org/fhir/administrative-gender', 'unknown') then
    result := 'Unknown'

  else if cc.hasCode('http://terminology.hl7.org/CodeSystem/international-civil-aviation-organization-sex-or-gender', 'F') then
    result := 'Female'
  else if cc.hasCode('http://terminology.hl7.org/CodeSystem/international-civil-aviation-organization-sex-or-gender', 'M') then
    result := 'Male'
  else if cc.hasCode('http://terminology.hl7.org/CodeSystem/international-civil-aviation-organization-sex-or-gender', '<') then
    result := 'Other/Not Recorded'

  else if cc.hasCode('http://terminology.hl7.org/CodeSystem/icaosex', 'F') then
    result := 'Female'
  else if cc.hasCode('http://terminology.hl7.org/CodeSystem/icaosex', 'M') then
    result := 'Male'
  else if cc.hasCode('http://terminology.hl7.org/CodeSystem/icaosex', '<') then
    result := 'Other/Not Recorded'

  else if cc.hasCode('http://loinc.org', '46098-0') then
    result := 'Sex'
  else if cc.hasCode('http://loinc.org', '76689-9') then
    result := 'Assigned at Birth'
  else if cc.hasCode('http://loinc.org', '76691-5') then
    result := 'Gender Identity'
  else if cc.hasCode('http://dicom.nema.org/resources/ontology/DCM', '0010,0040') then
    result := 'Sex'

  else
  begin
    for c in cc.codings.forEnum do
    begin
      if (c.systemUri = 'urn:iso:std:iso:3166') then
        exit(c.code);
      if (c.systemUri = 'urn:iso:std:iso:3166:-2') then
        exit(c.code);
      if (c.systemUri = 'https://www.usps.com/') then
        exit(c.code);
      result := c.display;
    end;
    if (result = '') then
      result := cc.renderText;
  end;
end;

{ TGenderIdentityManager }

function TGenderIdentityManager.canSort: boolean;
begin
  Result := true
end;

function TGenderIdentityManager.allowedOperations(item: TFHIRExtensionW): TNodeOperationSet;
begin
  result := [opAdd, opDelete, opEdit];
end;

function TGenderIdentityManager.loadList: boolean;
var
  ext : TFHIRExtensionW;
begin
  for ext in FFrame.FExtensions do
    if ext.url = 'http://hl7.org/fhir/StructureDefinition/individual-genderIdentity' then
      FData.add(ext.link);
end;

function TGenderIdentityManager.getCellText(item: TFHIRExtensionW; col: integer): String;
var
  ext : TFhirExtensionW;
  cc : TFhirCodeableConceptW;
begin
  result := '';
  case col of
    0: ext := item.getExtensionW('value');
    1: ext := item.getExtensionW('period');
    2: ext := item.getExtensionW('comment');
  else
    ext := nil;
  end;
  if (ext <> nil) then
  begin
    try
      if col = 0 then
      begin
        cc := ext.valueAsCodeableConcept;
        try
          result := getDisplay(cc);
        finally
          cc.free;
        end;
      end
      else
        result := ext.renderText;
    finally
      ext.free;
    end;
  end
  else
end;

function TGenderIdentityManager.getSummaryText(item: TFHIRExtensionW): String;
begin
  result := GetCellText(item, 2);
  if result <> '' then
    Result := GetCellText(item, 0)+ ' '+result
  else
    Result := GetCellText(item, 0);
end;

function TGenderIdentityManager.compareItem(left, right: TFHIRExtensionW; col: integer): integer;
begin
  if (col = -1) then
    result := integer(NativeInt(left) - NativeInt(right))
  else
    Result := String.compareText(GetCellText(left, col), GetCellText(right, col));
end;

function TGenderIdentityManager.addItem(mode: String): TFHIRExtensionW;
var
  ext : TFHIRExtensionW;
begin
  result := nil;
  ext := FFrame.FFactory.wrapExtension(FFrame.FFactory.makeByName('Extension'));
  try
    GenderIdentityDialog := TGenderIdentityDialog.Create(FFrame);
    try
      GenderIdentityDialog.Factory := FFrame.FFactory.Link;
      GenderIdentityDialog.load(ext);
      if GenderIdentityDialog.ShowModal = mrOk then
      begin
        GenderIdentityDialog.save(ext);
        result := ext.link;
      end;
    finally
      GenderIdentityDialog.free;
    end;
  finally
    ext.free;
  end;
end;

function TGenderIdentityManager.editItem(item: TFHIRExtensionW; mode: String): boolean;
begin
  GenderIdentityDialog := TGenderIdentityDialog.Create(FFrame);
  try
    GenderIdentityDialog.Factory := FFrame.FFactory.Link;
    GenderIdentityDialog.load(item);
    result := GenderIdentityDialog.ShowModal = mrOk;
    if result then
      GenderIdentityDialog.save(item);
  finally
    GenderIdentityDialog.free;
  end;
end;

function TGenderIdentityManager.deleteItem(item: TFHIRExtensionW): boolean;
begin
  FFrame.FExtensions.Remove(item);
end;

{ TPronounManager }

function TPronounManager.canSort: boolean;
begin
  Result := true
end;

function TPronounManager.allowedOperations(item: TFHIRExtensionW): TNodeOperationSet;
begin
  result := [opAdd, opDelete, opEdit];
end;

function TPronounManager.loadList: boolean;
var
  ext : TFHIRExtensionW;
begin
  for ext in FFrame.FExtensions do
    if ext.url = 'http://hl7.org/fhir/StructureDefinition/individual-pronouns' then
      FData.add(ext.link);
end;

function TPronounManager.getCellText(item: TFHIRExtensionW; col: integer): String;
var
  ext : TFhirExtensionW;
  cc : TFhirCodeableConceptW;
begin
  result := '';
  case col of
    0: ext := item.getExtensionW('value');
    1: ext := item.getExtensionW('period');
    2: ext := item.getExtensionW('comment');
  end;
  if (ext <> nil) then
  begin
    try
      if col = 0 then
      begin
        cc := ext.valueAsCodeableConcept;
        try
          result := getDisplay(cc);
        finally
          cc.free;
        end;
      end
      else
        result := ext.renderText;
    finally
      ext.free;
    end;
  end
  else
end;

function TPronounManager.getSummaryText(item: TFHIRExtensionW): String;
begin
  result := GetCellText(item, 2);
  if result <> '' then
    Result := GetCellText(item, 0)+ ' '+result
  else
    Result := GetCellText(item, 0);
end;

function TPronounManager.compareItem(left, right: TFHIRExtensionW; col: integer): integer;
begin
  if (col = -1) then
    result := integer(NativeInt(left) - NativeInt(right))
  else
    Result := String.compareText(GetCellText(left, col), GetCellText(right, col));
end;

{ TSFCUManager }

function TSFCUManager.canSort: boolean;
begin
  Result := true
end;

function TSFCUManager.allowedOperations(item: TFHIRExtensionW): TNodeOperationSet;
begin
  result := [opAdd, opDelete, opEdit];
end;

function TSFCUManager.loadList: boolean;
var
  ext : TFHIRExtensionW;
begin
  for ext in FFrame.FExtensions do
    if ext.url = 'http://hl7.org/fhir/StructureDefinition/patient-sexForClinicalUse' then
      FData.add(ext.link);
end;

function TSFCUManager.getCellText(item: TFHIRExtensionW; col: integer): String;
var
  ext : TFslList<TFhirExtensionW>;
  cc : TFhirCodeableConceptW;
  e : TFhirExtensionW;
begin
  result := '';
  case col of
    0: ext := item.getExtensionsW('value');
    1: ext := item.getExtensionsW('period');
    2: ext := item.getExtensionsW('comment');
    3: ext := item.getExtensionsW('supportingInfo');
  end;
  try
    if (ext.count > 0) then
    begin
      if col = 0 then
      begin
        cc := ext[0].valueAsCodeableConcept;
        try
          result := getDisplay(cc);
        finally
          cc.free;
        end;
      end
      else if (col = 3) then
      begin
        result := '';
        for e in ext do
          CommaAdd(result, e.renderText);
      end
      else
        result := ext[0].renderText;
    end;
  finally
    ext.free;
  end;
end;

function TSFCUManager.getSummaryText(item: TFHIRExtensionW): String;
begin
  result := GetCellText(item, 2);
  if result <> '' then
    Result := GetCellText(item, 0)+ ' '+result
  else
    Result := GetCellText(item, 0);
end;

function TSFCUManager.compareItem(left, right: TFHIRExtensionW; col: integer): integer;
begin
  if (col = -1) then
    result := integer(NativeInt(left) - NativeInt(right))
  else
    Result := String.compareText(GetCellText(left, col), GetCellText(right, col));
end;

{ TRecordedSexManager }

function TRecordedSexManager.canSort: boolean;
begin
  Result := true
end;

function TRecordedSexManager.allowedOperations(item: TFHIRExtensionW): TNodeOperationSet;
begin
  result := [opAdd, opDelete, opEdit];
end;

function TRecordedSexManager.loadList: boolean;
var
  ext : TFHIRExtensionW;
begin
  for ext in FFrame.FExtensions do
    if ext.url = 'http://hl7.org/fhir/StructureDefinition/individual-recordedSexOrGender' then
      FData.add(ext.link);
end;

function TRecordedSexManager.getCellText(item: TFHIRExtensionW; col: integer): String;
var
  ext : TFhirExtensionW;
  cc : TFhirCodeableConceptW;
begin
  result := '';
  case col of
    0: ext := item.getExtensionW('value');
    1: ext := item.getExtensionW('internationalEquivalent');
    2: ext := item.getExtensionW('type');
    3: ext := item.getExtensionW('effectivePeriod');
    4: ext := item.getExtensionW('acquisitionDate');
    5: ext := item.getExtensionW('sourceDocument');
    6: ext := item.getExtensionW('jurisdiction');
    7: ext := item.getExtensionW('comment');
  end;
  if (ext <> nil) then
  begin
    try
      if (col = 0) or (col = 1) or (col = 2) or (col = 6) then
      begin
        cc := ext.valueAsCodeableConcept;
        try
          result := getDisplay(cc);
        finally
          cc.free;
        end;
      end
      else
        result := ext.renderText;
    finally
      ext.free;
    end;
  end
  else
end;

function TRecordedSexManager.getSummaryText(item: TFHIRExtensionW): String;
begin
  result := GetCellText(item, 2);
  if result <> '' then
    Result := GetCellText(item, 0)+ ' '+result
  else
    Result := GetCellText(item, 0);
end;

function TRecordedSexManager.compareItem(left, right: TFHIRExtensionW; col: integer): integer;
begin
  if (col = -1) then
    result := integer(NativeInt(left) - NativeInt(right))
  else
    Result:= String.compareText(GetCellText(left, col), GetCellText(right, col));
end;

{ THealthcardManager }

function THealthcardManager.canSort: boolean;
begin
  Result := true;
end;

function THealthcardManager.doubleClickEdit: boolean;
begin
  Result := false;
end;

function THealthcardManager.allowedOperations(item: THealthcareCard): TNodeOperationSet;
begin
  result := [opExecute];
end;

function THealthcardManager.loadList: boolean;
var
  p, pp : TFhirParametersParameterW;
  utils : THealthcareCardUtilities;
begin
  if FFrame.FCards <> nil then
  begin
    utils := THealthcareCardUtilities.create;
    try
      utils.JWKList := TJWKList.create;
      utils.Factory := FFrame.sync.Factory.link;
      for p in FFrame.FCards.parameterList do
        if p.name = 'verifiableCredential' then
          Data.add(utils.verify(p.valueString));
    finally
      utils.free;
    end;
  end;
end;

procedure THealthcardManager.buildMenu;
begin
  inherited buildMenu;
  registerMenuEntry('Open as Bundle', ICON_OPEN, copExecute);
  registerMenuEntry('Open as JWT', ICON_SIG, copExecute, 'jwt');
end;

function THealthcardManager.getCellText(item: THealthcareCard; col: integer): String;
begin
  case col of
    0: result := item.issueDate.toString;
    1: result := item.issuer;
    2: result := item.cardTypesSummary;
    3: if item.isValid then result := 'Yes' else result := 'No';
    4: result := item.summary;
  end;
end;

function THealthcardManager.getSummaryText(item: THealthcareCard): String;
begin
  Result := item.summary;
end;

function THealthcardManager.compareItem(left, right: THealthcareCard; col: integer): integer;
begin
  case col of
    -1 : result := integer(NativeInt(left) - NativeInt(right));
    0: result := left.IssueDate.compare(right.IssueDate);
    1: result := String.compare(left.Issuer, right.Issuer);
    2: result := String.compare(left.cardTypesSummary, right.cardTypesSummary);
    3: result := ord(left.isValid) - ord(right.isValid);
    4: result := String.compare(left.summary, right.summary);
  else
    result := 0;
  end;
end;

function THealthcardManager.filterItem(item: THealthcareCard; s: String): boolean;
begin
  Result := item.issuer.contains(s) or item.cardTypesSummary.contains(s) or item.summary.contains(s);
end;

function THealthcardManager.executeItem(item: THealthcareCard; mode: String): boolean;
begin
  if mode = 'jwt' then
    FFrame.context.OnOpenSource(self, TEncoding.ASCII.getBytes(item.jws), sekJWT)
  else
    FFrame.context.OnOpenResourceObj(self, item.bundle);
end;

{ TPatientFrame }

destructor TPatientFrame.Destroy;
begin
  FPatient.free;
  FCardManager.Free;
  FCards.free;

  FGenderIdManager.free;
  FPronounsManager.free;
  FSFCUManager.free;
  FRecordedSexManager.free;
  FExtensions.free;
  inherited;
end;

procedure TPatientFrame.initialize;
begin
  lvCards.SmallImages := Context.images;

  FCardManager := THealthcardManager.create;
  FCardManager.Settings := Context.Settings;
  FCardManager.FFrame := self;
  FCardManager.Images := Context.images;
  FCardManager.List := lvCards;
  FCardManager.OnSetFocus := DoSelectCard;

  FGenderIdManager := TGenderIdentityManager.Create;
  FGenderIdManager.Settings := Context.Settings;
  FGenderIdManager.FFrame := self;
  FGenderIdManager.Images := Context.images;
  FGenderIdManager.List := lvGenderId;
  FGenderIdManager.registerControl(btnAddGenderId, copAdd);
  FGenderIdManager.registerControl(btnEditGenderId, copEdit);
  FGenderIdManager.registerControl(btnDeleteGenderId, copDelete);

  FPronounsManager := TPronounManager.Create;
  FPronounsManager.Settings := Context.Settings;
  FPronounsManager.FFrame := self;
  FPronounsManager.Images := Context.images;
  FPronounsManager.List := lvPronouns;
  FPronounsManager.registerControl(btnAddPronoun, copAdd);
  FPronounsManager.registerControl(btnEditPronoun, copEdit);
  FPronounsManager.registerControl(btnDeletePronoun, copDelete);

  FSFCUManager := TSFCUManager.Create;
  FSFCUManager.Settings := Context.Settings;
  FSFCUManager.FFrame := self;
  FSFCUManager.Images := Context.images;
  FSFCUManager.List := lvSFCU;
  FSFCUManager.registerControl(btnAddSFCU, copAdd);
  FSFCUManager.registerControl(btnEditSFCU, copEdit);
  FSFCUManager.registerControl(btnDeleteSFCU, copDelete);

  FRecordedSexManager := TRecordedSexManager.Create;
  FRecordedSexManager.Settings := Context.Settings;
  FRecordedSexManager.FFrame := self;
  FRecordedSexManager.Images := Context.images;
  FRecordedSexManager.List := lvRecordedSex;
  FRecordedSexManager.registerControl(btnAddRecord, copAdd);
  FRecordedSexManager.registerControl(btnEditRecord, copEdit);
  FRecordedSexManager.registerControl(btnDeleteRecord, copDelete);
end;

procedure TPatientFrame.bind;
begin
  FFactory := sync.Factory;
  FPatient := FFactory.wrapPatient(resource.link);
  FExtensions := FPatient.getExtensionsW('');
  if Client = nil then
  begin
    TabSheet2.Visible := false;
    PageControl1.ShowTabs := false;
  end;
  cbxGender.itemIndex := StringArrayIndexOf(['male', 'female', 'other', 'unknown'], FPatient.gender);
  case StringArrayIndexOf(['', 'true', 'false'], FPatient.activeStr) of
    0: cbActive.State := cbGrayed;
    1: cbActive.State := cbChecked;
    2: cbActive.State := cbUnchecked;
  end;
  edtNameSummary.text := FPatient.nameSummary;
  edtDoB.Date := TFslDateTime.fromXml(FPatient.dob).DateTime;
  FGenderIdManager.doLoad;
  FPronounsManager.doLoad;
  FSFCUManager.doLoad;
  FRecordedSexManager.doLoad;
end;

procedure TPatientFrame.saveStatus;
begin
  inherited saveStatus;
  FCardManager.saveStatus;
  FGenderIdManager.saveStatus;
end;

procedure TPatientFrame.btnFetchHealthCardsClick(Sender: TObject);
var
  p : TFHIRParametersW;
  r : TFHIRResourceV;
  s : String;
  t : UInt64;
begin
  cursor := crHourGlass;
  try
    pnlHealthcardsOutome.caption := '  Fetching Healthcare Cards';
    Application.ProcessMessages;
    try
      FCards.Free;
      FCards := nil;
      FCardManager.doLoad;
      p := sync.Factory.makeParameters;
      try
        p.addParam('credentialType', sync.Factory.makeUri('https://smarthealth.cards#health-card'));
        if cbCovidOnly.checked then
          p.addParam('credentialType', sync.Factory.makeUri('https://smarthealth.cards#covid19'));
        t := GetTickCount64;
        r := client.operationV('Patient', FPatient.id, 'health-cards-issue', p.Resource);
        try
          FCards := sync.Factory.wrapParams(r.link);
        finally
          r.free;
        end;
        t := GetTickCount64 - t;
        FCardManager.doLoad;
        s := '  Health Cards: '+inttostr(FCardManager.Data.count)+' found';
        s := s + ' as of '+TFslDateTime.makeLocal.toXML+' (local)';
        s := s + ' ('+inttostr(t)+'ms)';
        pnlHealthcardsOutome.caption := s;
      finally
        p.free;
      end;
    except
      on e : Exception do
      begin
        pnlHealthcardsOutome.caption := '  Error: '+e.message;
        raise;
      end;
    end;
  finally
    Cursor := crDefault;
  end;
end;

procedure TPatientFrame.mnuSaveQRClick(Sender: TObject);
var
  bmp : TBitmap;
begin
  if sd.execute then
  begin
    bmp := TBitmap.create;
    try
      FCardManager.Focus.toBmp(bmp);
      bmp.SaveToFile(sd.filename);
    finally
      bmp.free;
    end;
  end;
end;

procedure TPatientFrame.Panel4Resize(Sender: TObject);
begin
end;

procedure TPatientFrame.pbCardPaint(Sender: TObject);
var
  bmp : TBitMap;
  scale : double;
begin
  pbCard.Canvas.Brush.Color := clWhite;
  pbCard.Canvas.FillRect(Rect(0, 0, pbCard.Width, pbCard.Height));
  if FCardManager.Focus <> nil then
  begin
    bmp := TBitmap.create;
    try
      FCardManager.Focus.toBmp(bmp);
      if (pbCard.Width < pbCard.Height) then
        scale := pbCard.Width / bmp.Width
      else
        scale := pbCard.Height / bmp.Height;
      pbCard.Canvas.StretchDraw(Rect(0, 0, Trunc(scale * bmp.Width), Trunc(scale * bmp.Height)), bmp);
    finally
      bmp.free;
    end;
  end;
end;

procedure TPatientFrame.TabSheet4Resize(Sender: TObject);
begin
  gbGenderIdentity.Height := (tabSheet4.Height - pnlAdminGender.height) div 4;
  gbPronouns.Height := (tabSheet4.Height - pnlAdminGender.height) div 4;
  gbSexForClinicalUse.Height := (tabSheet4.Height - pnlAdminGender.height) div 4;
end;

procedure TPatientFrame.DoSelectCard(sender: TObject);
begin
  if FCardManager.Focus = nil then
  begin
    htmlCard.Clear;
    mnuSaveQR.Enabled := false;
  end
  else
  begin
    htmlCard.LoadFromString(FCardManager.Focus.htmlReport(context.TxServers.defaultServer));
    mnuSaveQR.Enabled := true;
  end;
  pbCard.Invalidate;
end;

end.

