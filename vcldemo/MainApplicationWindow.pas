unit MainApplicationWindow;

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

{
VCL demo for a standalone Smart on FHIR client that accesses
allergy and medication information

The flow is simple:
- start up the application. Load configuration from %temp%/fhir-vcl-demo.ini
- do smart on fhir login, and succeed or close down
- fill the patient banner by querying for the specified patient
- load the allergies and medications by querying for AllergyIntolerance, MedicationOrder and MedicationStatement for the patient.
  - note that there are 2 kinds of medication resources (system prescribed and patient reported)
- if the user clicks on any of the entries, show the narrative and additional details
- log all the details, either to a text file, or to a FHIR server using AuditEvent

}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  IniFiles,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.OleCtrls,
  SHDocVw,
  PngImage,
  Generics.Collections,
  Generics.Defaults,
  VirtualTrees,
  FHIR.Support.Base,
  FHIR.Support.Utilities,

  FHIR.Base.Objects,
  FHIR.Version.Client,
  FHIR.Version.Types,
  FHIR.Version.Resources,
  FHIR.Version.Utilities,
  FHIR.Base.Xhtml,
  ServerLoginDialog,
  ProgressDialog,
  FHIRDemoLogging;

type
  TMainWindowForm = class;

  TAllergiesComparer = class (TFslObject, IComparer<TFhirAllergyIntolerance>)
  private
    FColumn: integer;
    FDesc : boolean;
    FWindow : TMainWindowForm;
  public
    function Compare(const Left, Right: TFhirAllergyIntolerance): Integer;
  end;

  TMedicationsComparer = class (TFslObject, IComparer<TFHIRResource>)
  private
    FColumn: integer;
    FDesc : boolean;
    FWindow : TMainWindowForm;
  public
    function Compare(const Left, Right: TFHIRResource): Integer;
  end;

  TValueNode = class (TFslObject)
  private
    FName : String;
    FValue : TFHIRElement;
    FChildren : TFslList<TValueNode>;
  public
    constructor Create(name : string; element : TFHIRElement);
    destructor Destroy; override;
  end;

  TMainWindowForm = class(TForm)
    Panel1: TPanel;
    pnlPatientLeft: TPanel;
    pnlPatientRight: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblName: TLabel;
    lblDoB: TLabel;
    lblGender: TLabel;
    Label4: TLabel;
    lblMRN: TLabel;
    Label6: TLabel;
    lblAddress: TLabel;
    Label8: TLabel;
    lblPhone: TLabel;
    imgPatient: TImage;
    pnlLists: TPanel;
    Splitter2: TSplitter;
    Panel3: TPanel;
    Label5: TLabel;
    vtAllergies: TVirtualStringTree;
    Label7: TLabel;
    lblAllergiesMessage: TLabel;
    pnlAllergiesList: TPanel;
    Splitter3: TSplitter;
    pnlMedsList: TPanel;
    vtDisplay: TVirtualStringTree;
    pgDisplay: TPageControl;
    tabNarrative: TTabSheet;
    tabValues: TTabSheet;
    webNarrative: TWebBrowser;
    vtMedications: TVirtualStringTree;
    lblMedicationsMessage: TLabel;
    Panel2: TPanel;
    Button1: TButton;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure vtAllergiesGetCellText(Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
    procedure vtAllergiesGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure vtAllergiesColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    procedure FormShow(Sender: TObject);
    procedure pnlListsResize(Sender: TObject);
    procedure pnlAllergiesListResize(Sender: TObject);
    procedure vtAllergiesHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure vtAllergiesNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure vtDisplayGetCellText(Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure vtMedicationsColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    procedure vtMedicationsGetCellText(Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
    procedure vtMedicationsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure vtMedicationsHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure vtMedicationsNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure vtDisplayInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure Button1Click(Sender: TObject);
  private
    FClient : TFHIRClient;
    FIni : TIniFile;
    FPatientId : String;
    FProgressForm : TProgressWindow;
    FAllergies : TFslList<TFHIRAllergyIntolerance>;
    FMedications : TFslList<TFHIRResource>;
    FLoaded : boolean;
    FAllergiesComparer : TAllergiesComparer;
    FMedicationsComparer : TMedicationsComparer;
    FValues : TFslList<TValueNode>;
    FLogService : TLoggingService;

    procedure displayAllergy(allergy : TFhirAllergyIntolerance);
    procedure displayMedicationOrder(med : TFhirMedicationOrder);
    procedure displayMedicationStatement(med : TFhirMedicationStatement);
    procedure displayResourceNarrative(res : TFhirDomainResource);
    procedure LoadAllergies;
    procedure LoadMedications;
    procedure LoadPatient;
    procedure RetrieveMedsAndAllergies;
    function TextForCellAllergy(allergy : TFhirAllergyIntolerance; col: integer): String;
    function TextForCellMedication(med : TFhirResource; col: integer): String;
  end;

var
  MainWindowForm: TMainWindowForm;

implementation

{$R *.dfm}

function AppExePath: UnicodeString;
begin
  Result := ExtractFilePath(Application.ExeName) + '\';
end;




procedure TMainWindowForm.FormCreate(Sender: TObject);
begin
  FLogService := TFileLoggingService.create(path([AppExePath, 'fhir-vcl-demo.log']));
  FProgressForm := TProgressWindow.Create(self);
  FIni := TIniFile.Create(Path([SystemTemp, 'fhir-vcl-demo.ini']));
  FAllergies := TFslList<TFHIRAllergyIntolerance>.create;
  FAllergiesComparer := TAllergiesComparer.create;
  FAllergiesComparer.FWindow := self;
  FAllergiesComparer.FColumn := 0;
  FMedicationsComparer := TMedicationsComparer.create;
  FMedicationsComparer.FWindow := self;
  FMedicationsComparer.FColumn := 0;
  FMedications := TFslList<TFHIRResource>.create;

  FValues := TFslList<TValueNode>.create;
end;

procedure TMainWindowForm.FormActivate(Sender: TObject);
begin
  RetrieveMedsAndAllergies;
end;

procedure TMainWindowForm.RetrieveMedsAndAllergies;
var
  ServerLoginForm : TServerLoginForm;
begin
  if FClient = nil then
  begin
    ServerLoginForm := TServerLoginForm.Create(self);
    try
      ServerLoginForm.Ini := FIni;
      ServerLoginForm.ProgressForm := FProgressForm;
      ServerLoginForm.LogService := FLogService;
      try
        if ServerLoginForm.ShowModal = mrOk then
        begin
          FClient := ServerLoginForm.Client.link;
          FPatientId := ServerLoginForm.edtPatientId.Text;
          LoadPatient;
        end
        else
          close;
      finally
        FProgressForm.Close;
      end;
    finally
      ServerLoginForm.Free;
    end;
  end;
end;

procedure TMainWindowForm.FormShow(Sender: TObject);
begin
  vtAllergies.Header.Columns[0].Width := FIni.ReadInteger('Allergies', 'col-width-0', vtAllergies.Header.Columns[0].Width);
  vtAllergies.Header.Columns[1].Width := FIni.ReadInteger('Allergies', 'col-width-1', vtAllergies.Header.Columns[1].Width);
  vtAllergies.Header.Columns[2].Width := FIni.ReadInteger('Allergies', 'col-width-2', vtAllergies.Header.Columns[2].Width);
  vtAllergies.Header.Columns[3].Width := FIni.ReadInteger('Allergies', 'col-width-3', vtAllergies.Header.Columns[3].Width);
  vtAllergies.Header.Columns[4].Width := FIni.ReadInteger('Allergies', 'col-width-4', vtAllergies.Header.Columns[4].Width);
  vtAllergies.Header.Columns[5].Width := FIni.ReadInteger('Allergies', 'col-width-5', vtAllergies.Header.Columns[5].Width);
  vtAllergies.Header.Columns[6].Width := FIni.ReadInteger('Allergies', 'col-width-6', vtAllergies.Header.Columns[6].Width);
  vtMedications.Header.Columns[0].Width := FIni.ReadInteger('Medications', 'col-width-0', vtMedications.Header.Columns[0].Width);
  vtMedications.Header.Columns[1].Width := FIni.ReadInteger('Medications', 'col-width-1', vtMedications.Header.Columns[1].Width);
  vtMedications.Header.Columns[2].Width := FIni.ReadInteger('Medications', 'col-width-2', vtMedications.Header.Columns[2].Width);
  vtMedications.Header.Columns[3].Width := FIni.ReadInteger('Medications', 'col-width-3', vtMedications.Header.Columns[3].Width);
  vtMedications.Header.Columns[4].Width := FIni.ReadInteger('Medications', 'col-width-4', vtMedications.Header.Columns[4].Width);
  vtMedications.Header.Columns[5].Width := FIni.ReadInteger('Medications', 'col-width-5', vtMedications.Header.Columns[5].Width);
  vtMedications.Header.Columns[6].Width := FIni.ReadInteger('Medications', 'col-width-6', vtMedications.Header.Columns[6].Width);
  pnlLists.Width := FIni.ReadInteger('Layout', 'list-width', pnlLists.Width);
  pnlAllergiesList.Height := FIni.ReadInteger('Layout', 'allergy-height', pnlAllergiesList.Height);
  Width := FIni.ReadInteger('Layout', 'width', Width);
  FLoaded := true;
end;

procedure TMainWindowForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // https://stackoverflow.com/questions/30667585/access-violation-on-shutdown-from-hidden-twebbrowser
  FreeAndNil(webNarrative);
end;

procedure TMainWindowForm.FormDestroy(Sender: TObject);
begin
  if FLogService <> nil then
    FLogService.recordLogout;
  FIni.Free;
  FMedications.Free;
  FAllergies.Free;
  FAllergiesComparer.Free;
  FMedicationsComparer.Free;
  FValues.Free;
  FreeAndNil(FProgressForm);
  FClient.Free;
  FLogService.Free;
end;

procedure TMainWindowForm.FormResize(Sender: TObject);
begin
  pnlPatientLeft.Width := ClientWidth div 2;
  if FLoaded then
    FIni.WriteInteger('Layout', 'width', Width);
end;


procedure TMainWindowForm.Button1Click(Sender: TObject);
begin
  if Assigned(FClient) then
    FreeAndNil(FClient);
  vtAllergies.Clear;
  vtMedications.Clear;
  vtDisplay.Clear;
  FValues.Clear;
  FAllergies.Clear;
  FMedications.Clear;
  webNarrative.Navigate('about:blank');
  RetrieveMedsAndAllergies;
end;

procedure TMainWindowForm.displayAllergy(allergy: TFhirAllergyIntolerance);
var
  rxn : TFhirAllergyIntoleranceReaction;
  cc : TFhirCodeableConcept;
begin
  FLogService.recordResourceReadSuccess('n/a', frtAllergyIntolerance, allergy.id, allergy);
  displayResourceNarrative(allergy);
  FValues.Clear;
  vtDisplay.Clear;

  FValues.Add(TValueNode.Create('Id', allergy.idElement.Link));
  FValues.Add(TValueNode.Create('Substance', allergy.substance.Link));
  FValues.Add(TValueNode.Create('Status', allergy.statusElement.Link));
  FValues.Add(TValueNode.Create('Onset Date', allergy.onsetElement.Link));
  FValues.Add(TValueNode.Create('Date/Time recorded', allergy.recordedDateElement.Link));
  FValues.Add(TValueNode.Create('Who recorded', allergy.recorder.Link));
  FValues.Add(TValueNode.Create('Criticality', allergy.criticalityElement.Link));
  FValues.Add(TValueNode.Create('Category', allergy.categoryElement.Link));
  for rxn in allergy.reactionList do
    for cc in rxn.manifestationList do
      FValues.Add(TValueNode.Create('Manifestation', cc.Link));
  FValues.Add(TValueNode.Create('Comment', allergy.note.Link));

  vtDisplay.RootNodeCount := FValues.count;
  vtDisplay.Invalidate;
end;

procedure TMainWindowForm.displayMedicationOrder(med: TFhirMedicationOrder);
var
  inst : TFhirMedicationOrderDosageInstruction;
  v : TValueNode;
begin
  FLogService.recordResourceReadSuccess('n/a', frtMedicationOrder, med.id, med);
  displayResourceNarrative(med);
  FValues.Clear;
  vtDisplay.Clear;

  FValues.Add(TValueNode.Create('Type', TFhirString.Create('MedicationOrder')));
  FValues.Add(TValueNode.Create('Id', med.idElement.Link));
  FValues.Add(TValueNode.Create('Date/Time written', med.dateWrittenElement.Link));
  FValues.Add(TValueNode.Create('Status', med.statusElement.Link));
  FValues.Add(TValueNode.Create('Date/Time to Stop', med.dateEndedElement.Link));
  FValues.Add(TValueNode.Create('Prescriber', med.prescriber.Link));
  FValues.Add(TValueNode.Create('Encounter', med.encounter.Link));
  FValues.Add(TValueNode.Create('Medication', med.medication.Link));
  for inst in med.dosageInstructionList do
  begin
    v := TValueNode.Create('Instruction', nil);
    FValues.Add(v);
    v.FChildren.Add(TValueNode.Create('Patient Text', inst.getExtensionValue('https://fhir-ehr.cerner.com/dstu2/StructureDefinition/patient-friendly-display').Link));
    v.FChildren.Add(TValueNode.Create('Additional', inst.additionalInstructions.Link));
    v.FChildren.Add(TValueNode.Create('Timing', inst.timing.Link));
    v.FChildren.Add(TValueNode.Create('As Needed', inst.asNeeded.Link));
    v.FChildren.Add(TValueNode.Create('Route', inst.route.Link));
    v.FChildren.Add(TValueNode.Create('Dose', inst.dose.Link));
  end;
  v := TValueNode.Create('Dispesning', nil);
  FValues.Add(v);
  v.FChildren.Add(TValueNode.Create('Period', med.dispenseRequest.validityPeriod.Link));
  v.FChildren.Add(TValueNode.Create('Refill #', med.dispenseRequest.numberOfRepeatsAllowedElement.Link));
  v.FChildren.Add(TValueNode.Create('Amount', med.dispenseRequest.quantityElement.Link));
  vtDisplay.RootNodeCount := FValues.count;
  vtDisplay.Invalidate;
end;

procedure TMainWindowForm.displayMedicationStatement(med: TFhirMedicationStatement);
var
  dosage : TFhirMedicationStatementDosage;
//  disp : TFhirMedicationOrderDispenseRequest;
  v : TValueNode;
begin
  FLogService.recordResourceReadSuccess('n/a', frtMedicationStatement, med.id, med);
  displayResourceNarrative(med);
  FValues.Clear;
  vtDisplay.Clear;

  FValues.Add(TValueNode.Create('Type', TFhirString.Create('MedicationStatement')));
  FValues.Add(TValueNode.Create('Id', med.idElement.Link));
  FValues.Add(TValueNode.Create('Source', med.informationSource.Link));
  FValues.Add(TValueNode.Create('Date/Time recorded', med.dateAssertedElement.Link));
  FValues.Add(TValueNode.Create('Status', med.statusElement.Link));
  FValues.Add(TValueNode.Create('Not Taken?', med.wasNotTakenElement.Link));
  FValues.Add(TValueNode.Create('Effective Time', med.effective.Link));
  FValues.Add(TValueNode.Create('Medication', med.medication.Link));
  FValues.Add(TValueNode.Create('Category', med.getExtensionValue('https://fhir-ehr.cerner.com/dstu2/StructureDefinition/medication-statement-category').Link));
  FValues.Add(TValueNode.Create('Reason for use', med.reasonForUse.Link));
  FValues.Add(TValueNode.Create('Comments', med.noteElement.Link));

  for dosage in med.dosageList do
  begin
    v := TValueNode.Create('Dosage', nil);
    FValues.Add(v);
    v.FChildren.Add(TValueNode.Create('Route', dosage.routeElement.Link));
    v.FChildren.Add(TValueNode.Create('Frequency', dosage.timing.Link));
    v.FChildren.Add(TValueNode.Create('Quantity', dosage.quantity.Link));
    v.FChildren.Add(TValueNode.Create('Site', dosage.site.Link));
    v.FChildren.Add(TValueNode.Create('Rate', dosage.rate.Link));
    v.FChildren.Add(TValueNode.Create('Patient Text', dosage.getExtensionValue('https://fhir-ehr.cerner.com/dstu2/StructureDefinition/patient-friendly-display').Link));
  end;
  vtDisplay.RootNodeCount := FValues.count;
  vtDisplay.Invalidate;
end;

procedure TMainWindowForm.displayResourceNarrative(res: TFhirDomainResource);
var
  doc : OleVariant;
  b : TFslStringBuilder;
begin
  b := TFslStringBuilder.Create;
  try
    TFHIRXhtmlParser.compose(res.text.div_, b, false);

    if NOT Assigned(webNarrative.Document) then
      webNarrative.Navigate('about:blank');

    Doc := webNarrative.Document;
    Doc.Clear;
    Doc.Write(b.AsString);
    Doc.Close;
  finally
    b.Free;
  end;
end;

procedure TMainWindowForm.LoadAllergies;
var
  params : TStringList;
  bnd : TFHIRBundle;
  be : TFhirBundleEntry;
  msg : String;
begin
  FProgressForm.Message := 'Loading Patient Allergies';
  params := TStringList.create;
  try
    params.AddPair('patient', FPatientId);
    try
      bnd := FClient.search(frtAllergyIntolerance, true, params);
    except
      on e : Exception do
      begin
        FLogService.recordResourceSearchFail(FClient.LastHeaders.lastOperationId, frtAllergyIntolerance, params, e);
        raise
      end;
    end;
    try
      FLogService.recordResourceSearchSuccess(FClient.LastHeaders.lastOperationId, frtAllergyIntolerance, params, bnd);
      for be in bnd.entryList do
      begin
        if (be.search = nil) or (be.search.mode = SearchEntryModeMatch) then
          FAllergies.Add((be.resource as TFhirAllergyIntolerance).link);
        if be.resource is TFhirOperationOutcome then
          msg := (be.resource as TFhirOperationOutcome).asExceptionMessage;
      end;
      vtAllergies.RootNodeCount := FAllergies.Count;
      vtAllergies.Invalidate;
      if FAllergies.Count = 1 then
        lblAllergiesMessage.Caption := '1 Allergy found'
      else
        lblAllergiesMessage.Caption := inttostr(FAllergies.Count)+' Allergies found';
      if msg <> '' then
        lblAllergiesMessage.Caption := lblAllergiesMessage.Caption +'. '+msg;
      lblAllergiesMessage.hint := lblAllergiesMessage.Caption;
      FAllergies.Sort(FAllergiesComparer);
    finally
      bnd.Free;
    end;
  finally
    params.Free;
  end;
end;

procedure TMainWindowForm.LoadMedications;
var
  params : TStringList;
  bnd : TFHIRBundle;
  be : TFhirBundleEntry;
  msg : String;
begin
  FProgressForm.Message := 'Loading Patient Medications';
  params := TStringList.create;
  try
    params.AddPair('patient', FPatientId);
    try
      bnd := FClient.search(frtMedicationOrder, true, params);
    except
      on e : Exception do
      begin
        FLogService.recordResourceSearchFail(FClient.LastHeaders.lastOperationId, frtMedicationOrder, params, e);
        raise
      end;
    end;
    try
      FLogService.recordResourceSearchSuccess(FClient.LastHeaders.lastOperationId, frtMedicationOrder, params, bnd);
      for be in bnd.entryList do
      begin
        if (be.search = nil) or (be.search.mode = SearchEntryModeMatch) then
          FMedications.Add((be.resource as TFhirMedicationOrder).link);
        if be.resource is TFhirOperationOutcome then
          msg := (be.resource as TFhirOperationOutcome).asExceptionMessage;
      end;
    finally
      bnd.Free;
    end;
    try
      bnd := FClient.search(frtMedicationStatement, true, params);
    except
      on e : Exception do
      begin
        FLogService.recordResourceSearchFail(FClient.LastHeaders.lastOperationId, frtMedicationStatement, params, e);
        raise
      end;
    end;
    try
      FLogService.recordResourceSearchSuccess(FClient.LastHeaders.lastOperationId, frtMedicationStatement, params, bnd);
      for be in bnd.entryList do
      begin
        if (be.search = nil) or (be.search.mode = SearchEntryModeMatch) then
          FMedications.Add((be.resource as TFhirMedicationStatement).link);
        if be.resource is TFhirOperationOutcome then
          msg := (be.resource as TFhirOperationOutcome).asExceptionMessage;
      end;
    finally
      bnd.Free;
    end;

    vtMedications.RootNodeCount := FAllergies.Count;
    vtMedications.Invalidate;
    if FMedications.Count = 1 then
      lblMedicationsMessage.Caption := '1 Medication found'
    else
      lblMedicationsMessage.Caption := inttostr(FMedications.Count)+' Medications found';
    if msg <> '' then
      lblMedicationsMessage.Caption := lblAllergiesMessage.Caption +'. '+msg;
    lblMedicationsMessage.hint := lblMedicationsMessage.Caption;
    FMedications.Sort(FMedicationsComparer);
  finally
    params.Free;
  end;
end;

procedure TMainWindowForm.LoadPatient;
var
  pat : TFhirPatient;
  id : TFhirIdentifier;
  cp : TFhirContactPoint;
  m : TStream;
  bmp: TBitmap;
begin
  FProgressForm.Message := 'Loading Patient';
  try
    pat := FClient.readResource(frtPatient, FPatientId) as TFhirPatient;
    FLogService.recordResourceReadSuccess(FClient.LastHeaders.lastOperationId, frtPatient, FPatientId, pat);
  except
    on e : Exception do
    begin
      FLogService.recordResourceReadFail(FClient.LastHeaders.lastOperationId, frtPatient, FPatientId, e);
      showMessage('Unable to retrieve patient: '+e.Message);
      exit;
    end;
  end;
  try
    lblName.Caption := HumanNamesAsText(pat.nameList);
    lblDoB.Caption := gen(pat.birthDateElement);
    lblGender.Caption := CODES_TFhirAdministrativeGenderEnum[pat.gender];
    lblMRN.Caption := 'n/a';
    for id in pat.identifierList do
      if id.isType('MR') then
        lblMRN.Caption := gen(id);
    lblAddress.Caption := '';
    if pat.addressList.Count > 0 then
      lblAddress.Caption := gen(pat.addressList[0]);
    lblPhone.Caption := '';
    for cp in pat.telecomList do
      if cp.system = ContactPointSystemPhone then
        lblPhone.Caption := gen(cp);
    if pat.photoList.count > 0 then
    begin
      m := TMemoryStream.Create;
      try
        m.Write(pat.photoList[0].data[0], length(pat.photoList[0].data));
        m.Position := 0;
        try
          bmp := TBitmap.Create;
          bmp.LoadFromStream(m);
          imgPatient.Picture.Assign(bmp);
          bmp.Free;
        except
          // just ignore; this is not always populated correctly in the sample data
        end;
      finally
        m.free;
      end;
//      FProgressForm.Message := 'Loading Patient';
    end;
    LoadAllergies;
    LoadMedications;
  finally
    pat.Free;
  end;
end;

procedure TMainWindowForm.pnlAllergiesListResize(Sender: TObject);
begin
  if FLoaded then
    FIni.WriteInteger('Layout', 'allergy-height', pnlAllergiesList.Height);
end;

procedure TMainWindowForm.pnlListsResize(Sender: TObject);
begin
  if FLoaded then
    FIni.WriteInteger('Layout', 'list-width', pnlLists.Width);
end;

procedure TMainWindowForm.vtAllergiesColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  if FLoaded then
    FIni.WriteInteger('Allergies', 'col-width-'+inttostr(column), Sender.Columns[Column].Width);
end;

procedure TMainWindowForm.vtAllergiesGetCellText(Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
begin
  e.CellText := TextForCellAllergy(FAllergies[e.Node.Index], e.Column);
end;

const
  APP_CODES_TFhirAllergyIntoleranceCriticalityEnum : Array[TFhirAllergyIntoleranceCriticalityEnum] of String = ('', 'low', 'high', 'unknown');

function TMainWindowForm.TextForCellAllergy(allergy : TFhirAllergyIntolerance; col : integer) : String;
var
  rxn : TFhirAllergyIntoleranceReaction;
  cc : TFhirCodeableConcept;
  s, m : String;
begin
  case col of
    0: {onset} result := allergy.onset.toString('c');
    1: {status} result := CODES_TFhirAllergyIntoleranceStatusEnum[allergy.status];
    2: {criticality} result := APP_CODES_TFhirAllergyIntoleranceCriticalityEnum[allergy.criticality];
    3: {category} result := CODES_TFhirAllergyIntoleranceCategoryEnum[allergy.category];
    4: {substance} result := gen(allergy.substance);
    5: {manifestation}
       begin
       s := '';
       for rxn in allergy.reactionList do
         for cc in rxn.manifestationList do
         begin
           m := gen(cc);
           if (m <> '') then
             if s = '' then
               s := m
             else
               s := s + ', '+s;
         end;
       result := s;
       end;
    6: {recordedDate / recorder}
       begin
       if (allergy.recordedDateElement = nil) and (allergy.recorderElement = nil) then
         result := ''
       else if (allergy.recordedDateElement = nil) then
         result := allergy.recorder.display
       else if (allergy.recorderElement = nil) then
         result := allergy.recordedDate.toString('c')
       else
         result := allergy.recordedDate.toString('c') +' by' + allergy.recorder.display;
       end
  else
    result := '';
  end;
end;

function TMainWindowForm.TextForCellMedication(med: TFhirResource; col: integer): String;
var
  o : TFhirMedicationOrder;
  s : TFhirMedicationStatement;
begin
  if med is TFhirMedicationOrder then
  begin
    o := med as TFhirMedicationOrder;
    case col of
      0: { Type } result := 'Order';
      1: { Date/Time Start } result := o.dateWritten.toString('c');
      2: { Date/Time End } result := o.dateEnded.toString('c');
      3: { Status } result := CODES_TFhirMedicationOrderStatusEnum[o.status];
      4: { Medication } result := gen(o.medication);
      5: { Not Taken (statement only) } result := '';
      6: { Source (prescriber for Order) } result := gen(o.prescriber);
      7: { Reason for use (statement only) } result := '';
    else
      result := '';
    end;
  end
  else
  begin
    s := med as TFhirMedicationStatement;
    case col of
      0: { Type } result := 'Reported';
      1: { Date/Time Start }
        if s.effective = nil then
          result := ''
        else if s.effective is TFhirDateTime then
          result := (s.effective as TFhirDateTime).value.toString('c')
        else if s.effective is TFhirPeriod then
          result := (s.effective as TFhirPeriod).start.toString('c')
        else
          result := '?';
      2: { Date/Time End }
        if s.effective = nil then
          result := ''
        else if s.effective is TFhirDateTime then
          result := ''
        else if s.effective is TFhirPeriod then
          result := (s.effective as TFhirPeriod).end_.toString('c')
        else result := '?';
      3: { Status } result := CODES_TFhirMedicationStatementStatusEnum[s.status];
      4: { Medication } result := gen(s.medication);
      5: { Not Taken (statement only) } if s.wasNotTaken then result := 'Y - Not Taken' else result := '';
      6: { Source (prescriber for Order) } result := gen(s.informationSource);
      7: { Reason for use (statement only) } result := gen(s.reasonForUse);
    else
      result := '';
    end;
  end;
end;

procedure TMainWindowForm.vtAllergiesGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
begin
  HintText := TextForCellAllergy(FAllergies[node.Index], column);
end;

procedure TMainWindowForm.vtAllergiesHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.column >= 0 then
  begin
    if HitInfo.column <> vtAllergies.Header.SortColumn then
    begin
      vtAllergies.Header.SortColumn := HitInfo.Column;
      vtAllergies.Header.SortDirection := sdAscending;
      FAllergiesComparer.FColumn := HitInfo.Column;
      FAllergiesComparer.FDesc := false;
    end
    else
    begin
      FAllergiesComparer.FDesc := not FAllergiesComparer.FDesc;
      if FAllergiesComparer.FDesc then
        vtAllergies.Header.SortDirection := sdDescending
      else
        vtAllergies.Header.SortDirection := sdAscending;
    end;
    FAllergies.Sort(FAllergiesComparer);
    vtAllergies.Invalidate;
  end;
end;

procedure TMainWindowForm.vtAllergiesNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  displayAllergy(FAllergies[HitInfo.HitNode.Index]);
end;

procedure TMainWindowForm.vtDisplayGetCellText(Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
var
  value : TValueNode;
begin
  value := e.node.getData<TValueNode>;

  if e.Column = 0 then
    e.CellText := value.FName
  else if value.FValue = nil then
    e.CellText := ''
  else if value.FValue.isPrimitive then
    e.CellText := value.FValue.primitiveValue
  else
    e.CellText := gen(value.FValue as TFhirType);
end;

procedure TMainWindowForm.vtDisplayInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  value, v : TValueNode;
  p, p1 : Pointer;
begin
  if ParentNode = nil then
    value := FValues[Node.Index]
  else
    value := ParentNode.GetData<TValueNode>.FChildren[Node.Index];

  Node.SetData(value);
  for v in value.FChildren do
    vtDisplay.AddChild(node, v);
end;

procedure TMainWindowForm.vtMedicationsColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  if FLoaded then
    FIni.WriteInteger('Medications', 'col-width-'+inttostr(column), Sender.Columns[Column].Width);
end;

procedure TMainWindowForm.vtMedicationsGetCellText(Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
begin
  e.CellText := TextForCellMedication(FMedications[e.Node.Index], e.Column);
end;

procedure TMainWindowForm.vtMedicationsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
begin
  HintText := TextForCellMedication(FMedications[node.Index], column);
end;

procedure TMainWindowForm.vtMedicationsHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.column >= 0 then
  begin
    if HitInfo.column <> vtMedications.Header.SortColumn then
    begin
      vtMedications.Header.SortColumn := HitInfo.Column;
      vtMedications.Header.SortDirection := sdAscending;
      FMedicationsComparer.FColumn := HitInfo.Column;
      FMedicationsComparer.FDesc := false;
    end
    else
    begin
      FMedicationsComparer.FDesc := not FMedicationsComparer.FDesc;
      if FMedicationsComparer.FDesc then
        vtMedications.Header.SortDirection := sdDescending
      else
        vtMedications.Header.SortDirection := sdAscending;
    end;
    FMedications.Sort(FMedicationsComparer);
    vtMedications.Invalidate;
  end;
end;

procedure TMainWindowForm.vtMedicationsNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  if FMedications[HitInfo.HitNode.Index] is TFHIRMedicationStatement then
    displayMedicationStatement(FMedications[HitInfo.HitNode.Index] as TFHIRMedicationStatement)
  else
    displayMedicationOrder(FMedications[HitInfo.HitNode.Index] as TFHIRMedicationOrder);
end;

{ TAllergiesComparer }

function TAllergiesComparer.Compare(const Left, Right: TFhirAllergyIntolerance): Integer;
var
  sl, sr : String;
begin
  if FColumn = 0 then
    result := left.onset.compare(right.onset)
  else if (FColumn = 6) and (left.recordedDateElement <> nil) and (Right.recordedDateElement <> nil) then
    result := left.recordedDate.compare(right.recordedDate)
  else
  begin
    sl := FWindow.TextForCellAllergy(left, FColumn);
    sr := FWindow.TextForCellAllergy(Right, FColumn);
    result := CompareStr(sl, sr);
  end;
  if FDesc then
    result := -result;
end;

{ TValueNode }

constructor TValueNode.Create(name: string; element: TFHIRElement);
begin
  inherited Create;
  FName := name;
  FValue := element;
  FChildren := TFslList<TValueNode>.create;
end;

destructor TValueNode.Destroy;
begin
  FChildren.Free;
  FValue.Free;
  inherited;
end;

{ TMedicationsComparer }

function TMedicationsComparer.Compare(const Left, Right: TFHIRResource): Integer;
var
  sl, sr, el, er: TDateTimeEx;
  t : TFhirType;
  ssl, ssr : String;
begin
  if left is TFhirMedicationOrder then
  begin
    sl := (left as TFhirMedicationOrder).dateWritten;
    el := (left as TFhirMedicationOrder).dateEnded;
  end
  else
  begin
    t := (left as TFhirMedicationStatement).effective;
    if t is TFhirPeriod then
    begin
      sl := TFhirPeriod(t).start;
      el := TFhirPeriod(t).end_;
    end
    else if t is TFhirDateTime then
    begin
      sl := TFhirDateTime(t).value;
      el := TDateTimeEx.makeNull;
    end
    else
    begin
      sl := TDateTimeEx.makeNull;
      el := TDateTimeEx.makeNull;
    end;
  end;
  if right is TFhirMedicationOrder then
  begin
    sr := (right as TFhirMedicationOrder).dateWritten;
    er := (right as TFhirMedicationOrder).dateEnded;
  end
  else
  begin
    t := (right as TFhirMedicationStatement).effective;
    if t is TFhirPeriod then
    begin
      sr := TFhirPeriod(t).start;
      er := TFhirPeriod(t).end_;
    end
    else
    begin
      sr := TDateTimeEx.makeNull;
      er := TDateTimeEx.makeNull;
    end;
  end;

  case FColumn of
    1: result := sl.compare(sr);
    2: result := el.compare(er);
  else
    begin
      ssl := FWindow.TextForCellMedication(left, FColumn);
      ssr := FWindow.TextForCellMedication(Right, FColumn);
      result := CompareStr(ssl, ssr);
    end;
  end;
  if FDesc then
    result := -result;
end;

end.
