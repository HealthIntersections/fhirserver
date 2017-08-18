unit CapabilityStatementEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, FMX.Layouts, FMX.TreeView, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, FMX.DateTimeCtrls, FMX.ListBox, FMX.Edit,
  BaseResourceFrame,
  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities;

type
  TFrame = TBaseResourceFrame; // re-aliasing the Frame to work around a designer bug

  TCapabilityStatementEditorFrame = class(TFrame)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    tvStructure: TTreeView;
    tbStructure: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Label1: TLabel;
    btnClose: TButton;
    btnSave: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    cbExperimental: TCheckBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    cbXml: TCheckBox;
    cbJson: TCheckBox;
    cbTurtle: TCheckBox;
    cbTerminologyService: TCheckBox;
    Label15: TLabel;
    edtURL: TEdit;
    edtName: TEdit;
    edtTitle: TEdit;
    cbxStatus: TComboBox;
    dedDate: TDateEdit;
    edtPublisher: TEdit;
    edtDescription: TEdit;
    edtPurpose: TEdit;
    edtCopyright: TEdit;
    cbxKind: TComboBox;
    cbxJurisdiction: TComboBox;
    edtFHIRVersion: TEdit;
    mImplementationGuides: TMemo;
    edtVersion: TEdit;
    tvMetadata: TTreeViewItem;
    Label16: TLabel;
    mSecurity: TMemo;
    cbCORS: TCheckBox;
    cbClientCerts: TCheckBox;
    cbOAuth: TCheckBox;
    cbSmart: TCheckBox;
    Label17: TLabel;
    mDoco: TMemo;
    TabItem3: TTabItem;
    Label18: TLabel;
    mDocoRes: TMemo;
    Label19: TLabel;
    Label20: TLabel;
    cbRead: TCheckBox;
    edtDocoRead: TEdit;
    cbVRead: TCheckBox;
    edtDocoVRead: TEdit;
    edtDocoSearch: TEdit;
    cbUpdate: TCheckBox;
    edtDocoCreate: TEdit;
    cbPatch: TCheckBox;
    cbDelete: TCheckBox;
    edtDocoUpdate: TEdit;
    cbHistoryInstance: TCheckBox;
    edtDocoPatch: TEdit;
    cbHistoryType: TCheckBox;
    edtDocoDelete: TEdit;
    cbCreate: TCheckBox;
    edtDocoHistoryInstance: TEdit;
    edtDocoHistoryType: TEdit;
    cbSearch: TCheckBox;
    cbUpdateCreate: TCheckBox;
    Label21: TLabel;
    cbCondCreate: TCheckBox;
    cbCondUpdate: TCheckBox;
    cbCondDelete: TCheckBox;
    Label22: TLabel;
    cbxReadCondition: TComboBox;
    Label23: TLabel;
    cbRefLiteral: TCheckBox;
    cbRefLogical: TCheckBox;
    cbRefResolve: TCheckBox;
    cbRefEnforced: TCheckBox;
    cbRefLocal: TCheckBox;
    edtProfile: TEdit;
    Label24: TLabel;
    cbxVersioning: TComboBox;
    procedure tvStructureClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    function GetCapabilityStatement: TFHIRCapabilityStatement;
    function readJurisdiction : Integer;

    procedure loadMetadata;
    procedure loadRest(rest : TFhirCapabilityStatementRest);
    procedure loadResource(res : TFhirCapabilityStatementRestResource);
  public
    { Public declarations }
    property CapabilityStatement : TFHIRCapabilityStatement read GetCapabilityStatement;
    procedure load; override;
  end;

implementation

{$R *.fmx}

{ TCapabilityStatementEditorFrame }

procedure TCapabilityStatementEditorFrame.btnCloseClick(Sender: TObject);
begin
  Close;
end;

function TCapabilityStatementEditorFrame.GetCapabilityStatement: TFHIRCapabilityStatement;
begin
  result := TFHIRCapabilityStatement(Resource);
end;

procedure TCapabilityStatementEditorFrame.load;
var
  rest : TFhirCapabilityStatementRest;
  tiRest, tiRes : TTreeViewItem;
  res : TFhirCapabilityStatementRestResource;
begin
  inherited;

  tvMetadata.TagObject := CapabilityStatement;

  for rest in CapabilityStatement.restList do
  begin
    tiRest := TTreeViewItem.Create(tvMetadata);
    if rest.mode = RestfulCapabilityModeClient then
      tiRest.text := 'Client'
    else
      tiRest.text := 'Server';
    tvMetadata.AddObject(tiRest);
    tiRest.TagObject := rest;
    rest.TagObject := tiRest;
    for res in rest.resourceList do
    begin
      tiRes := TTreeViewItem.Create(tiRest);
      tiRes.text := CODES_TFhirResourceTypesEnum[res.type_];
      tiRest.AddObject(tiRes);
      tiRes.TagObject := res;
      res.TagObject := tiRes;
    end;
  end;
  tvStructure.Selected := tvMetadata;
  tvStructureClick(nil);
end;

procedure TCapabilityStatementEditorFrame.loadMetadata;
var
  url : TFHIRUri;
begin
  cbExperimental.IsChecked := CapabilityStatement.experimental;
  cbXml.IsChecked := CapabilityStatement.hasFormat(ffXml);
  cbJson.IsChecked := CapabilityStatement.hasFormat(ffJson);
  cbTurtle.IsChecked := CapabilityStatement.hasFormat(ffTurtle);
  cbTerminologyService.IsChecked := CapabilityStatement.instantiates('http://hl7.org/fhir/CapabilityStatement/terminology-server');

  edtURL.Text := CapabilityStatement.url;
  edtName.Text := CapabilityStatement.name;
  edtTitle.Text := CapabilityStatement.title;
  edtFHIRVersion.Text := CapabilityStatement.fhirVersion;
  edtVersion.Text := CapabilityStatement.version;
  edtPublisher.text := CapabilityStatement.publisher;
  edtDescription.Text := CapabilityStatement.description;
  edtPurpose.Text := CapabilityStatement.purpose;
  edtCopyright.Text := CapabilityStatement.copyright;
  cbxStatus.ItemIndex := ord(CapabilityStatement.status) - 1;
  dedDate.DateTime := CapabilityStatement.date.DateTime;
  cbxKind.ItemIndex := ord(CapabilityStatement.kind) - 1;
  cbxJurisdiction.ItemIndex := readJurisdiction;

  mImplementationGuides.Text := '';
  for url in CapabilityStatement.implementationGuideList do
    mImplementationGuides.Text := mImplementationGuides.Text + url.value+#13#10;
end;

procedure TCapabilityStatementEditorFrame.loadResource(res: TFhirCapabilityStatementRestResource);
  procedure interaction(code : TFhirTypeRestfulInteractionEnum; cb : TCheckBox; edt : TEdit);
  var
    ri : TFhirCapabilityStatementRestResourceInteraction;
  begin
    ri := res.interaction(code);
    cb.IsChecked := ri <> nil;
    if cb.IsChecked then
      edt.Text := ri.documentation;
  end;
begin
  mDocoRes.Text := res.documentation;
  if res.profile <> nil then
    edtProfile.Text := res.profile.reference
  else
    edtProfile.Text := '';

  interaction(TypeRestfulInteractionread, cbRead, edtDocoRead);
  interaction(TypeRestfulInteractionVread, cbVRead, edtDocoVRead);
  interaction(TypeRestfulInteractionUpdate, cbUpdate, edtDocoUpdate);
  interaction(TypeRestfulInteractionPatch, cbPatch, edtDocoPatch);
  interaction(TypeRestfulInteractionDelete, cbDelete, edtDocoDelete);
  interaction(TypeRestfulInteractionHistoryInstance, cbHistoryInstance, edtDocoHistoryInstance);
  interaction(TypeRestfulInteractionHistoryType, cbHistoryType, edtDocoHistoryType);
  interaction(TypeRestfulInteractioncreate, cbCreate, edtDocoCreate);
  interaction(TypeRestfulInteractionSearchType, cbSearch, edtDocoSearch);

  cbxVersioning.ItemIndex := ord(res.versioning)-1;
  cbUpdateCreate.IsChecked := res.updateCreate;
  cbCondCreate.IsChecked := res.conditionalCreate;
  cbCondUpdate.IsChecked := res.conditionalUpdate;
  cbCondDelete.IsChecked := ord(res.conditionalDelete) > 1;
  cbxReadCondition.ItemIndex := ord(res.conditionalRead) - 1;
  cbRefLiteral.IsChecked := ReferenceHandlingPolicyLiteral in res.referencePolicy;
  cbRefLogical.IsChecked := ReferenceHandlingPolicyLogical in res.referencePolicy;
  cbRefResolve.IsChecked := ReferenceHandlingPolicyResolves in res.referencePolicy;
  cbRefEnforced.IsChecked := ReferenceHandlingPolicyEnforced in res.referencePolicy;
  cbRefLocal.IsChecked := ReferenceHandlingPolicyLocal in res.referencePolicy;
end;


procedure TCapabilityStatementEditorFrame.loadRest(rest: TFhirCapabilityStatementRest);
begin
  mDoco.Text := rest.documentation;
  if rest.security <> nil then
  begin
    mSecurity.Text := rest.security.description;
    cbCORS.IsChecked := rest.security.cors;
    cbOAuth.IsChecked := rest.security.serviceList.hasCode('http://hl7.org/fhir/restful-security-service', 'OAuth');
    cbClientCerts.IsChecked := rest.security.serviceList.hasCode('http://hl7.org/fhir/restful-security-service', 'Certificates');
    cbSmart.IsChecked := rest.security.serviceList.hasCode('http://hl7.org/fhir/restful-security-service', 'SMART-on-FHIR	');
  end;
end;

function TCapabilityStatementEditorFrame.readJurisdiction: Integer;
var
  cc : TFhirCodeableConcept;
  c : TFhirCoding;
begin
  result := -1;
  for cc in CapabilityStatement.jurisdictionList do
    for c in cc.codingList do
    begin
      if c.system = 'urn:iso:std:iso:3166' then
      begin
        if c.code = 'AT' then exit(0);
        if c.code = 'AU' then exit(1);
        if c.code = 'BR' then exit(2);
        if c.code = 'CA' then exit(3);
        if c.code = 'CH' then exit(4);
        if c.code = 'CL' then exit(5);
        if c.code = 'CN' then exit(6);
        if c.code = 'DE' then exit(7);
        if c.code = 'DK' then exit(8);
        if c.code = 'EE' then exit(9);
        if c.code = 'ES' then exit(10);
        if c.code = 'FI' then exit(11);
        if c.code = 'FR' then exit(12);
        if c.code = 'GB' then exit(13);
        if c.code = 'NL' then exit(14);
        if c.code = 'NO' then exit(15);
        if c.code = 'NZ' then exit(16);
        if c.code = 'RU' then exit(17);
        if c.code = 'US' then exit(18);
        if c.code = 'VN' then exit(19);
      end
      else if c.system = 'http://unstats.un.org/unsd/methods/m49/m49.htm' then
      begin
        if c.code = '001' { World } then exit(21);
        if c.code = '002' { Africa } then exit(22);
        if c.code = '019' { Americas } then exit(23);
        if c.code = '142' { Asia } then exit(24);
        if c.code = '150' { Europe } then exit(25);
        if c.code = '053' { Australia and New Zealand } then exit(26);
      end
    end;
end;

procedure TCapabilityStatementEditorFrame.tvStructureClick(Sender: TObject);
var
  obj : TObject;
begin
  obj := tvStructure.Selected.TagObject;
  if obj is TFhirCapabilityStatement then
  begin
    tbStructure.TabIndex := 0;
    loadMetadata;
  end
  else if obj is TFhirCapabilityStatementRest then
  begin
    tbStructure.TabIndex := 1;
    loadRest(obj as TFhirCapabilityStatementRest);
  end
  else if obj is TFhirCapabilityStatementRestResource then
  begin
    tbStructure.TabIndex := 2;
    loadResource(obj as TFhirCapabilityStatementRestResource);
  end
end;

end.
