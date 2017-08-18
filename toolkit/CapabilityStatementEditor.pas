unit CapabilityStatementEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, FMX.Layouts, FMX.TreeView, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, FMX.DateTimeCtrls, FMX.ListBox, FMX.Edit,
  BaseResourceFrame,
  DateSupport,
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
    tbMetadata: TTabItem;
    tbRest: TTabItem;
    Label1: TLabel;
    btnCancel: TButton;
    btnCommit: TButton;
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
    tbResource: TTabItem;
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
    procedure btnCancelClick(Sender: TObject);
    procedure inputChanged(Sender: TObject);
    procedure btnCommitClick(Sender: TObject);
  private
    FActive : TTreeViewItem;
    function GetCapabilityStatement: TFHIRCapabilityStatement;
    function readJurisdiction : Integer;
    function getJurisdiction(i : integer) : TFHIRCodeableConcept;

    procedure loadMetadata;
    procedure loadRest(rest : TFhirCapabilityStatementRest);
    procedure loadResource(res : TFhirCapabilityStatementRestResource);

    procedure commitMetadata;
    procedure commitRest(rest : TFhirCapabilityStatementRest);
    procedure commitResource(res : TFhirCapabilityStatementRestResource);

    procedure setInputIsDirty;
    procedure SetInputIsNotDirty;

    procedure commit; override;
    procedure cancel; override;
  public
    { Public declarations }
    property CapabilityStatement : TFHIRCapabilityStatement read GetCapabilityStatement;
    procedure load; override;

  end;

implementation

{$R *.fmx}

{ TCapabilityStatementEditorFrame }

procedure TCapabilityStatementEditorFrame.btnCancelClick(Sender: TObject);
begin
  cancel;
end;

procedure TCapabilityStatementEditorFrame.btnCommitClick(Sender: TObject);
begin
  commit;
end;

procedure TCapabilityStatementEditorFrame.cancel;
begin
  tvStructure.Selected := FActive;
  tvStructureClick(nil);
  SetInputIsNotDirty;
end;

procedure TCapabilityStatementEditorFrame.commit;
var
  obj : TObject;
begin
  obj := tvStructure.Selected.TagObject;
  if obj is TFhirCapabilityStatement then
    CommitMetadata
  else if obj is TFhirCapabilityStatementRest then
    commitRest(obj as TFhirCapabilityStatementRest)
  else if obj is TFhirCapabilityStatementRestResource then
    commitResource(obj as TFhirCapabilityStatementRestResource);
  SetInputIsNotDirty;
  ResourceIsDirty := true;
end;

procedure TCapabilityStatementEditorFrame.commitMetadata;
var
  s : String;
  cc : TFHIRCodeableConcept;
begin
  CapabilityStatement.experimental := cbExperimental.IsChecked;
  CapabilityStatement.Format[ffXml] := cbXml.IsChecked;
  CapabilityStatement.Format[ffJson] := cbJson.IsChecked;
  CapabilityStatement.Format[ffTurtle] := cbTurtle.IsChecked;
  CapabilityStatement.instantiates['http://hl7.org/fhir/CapabilityStatement/terminology-server'] := cbTerminologyService.IsChecked;

  CapabilityStatement.url := edtURL.Text;
  CapabilityStatement.name := edtName.Text;
  CapabilityStatement.title := edtTitle.Text;
  CapabilityStatement.fhirVersion := edtFHIRVersion.Text;
  CapabilityStatement.version := edtVersion.Text;
  CapabilityStatement.publisher := edtPublisher.text;
  CapabilityStatement.description := edtDescription.Text;
  CapabilityStatement.purpose := edtPurpose.Text;
  CapabilityStatement.copyright := edtCopyright.Text;
  CapabilityStatement.status := TFhirPublicationStatusEnum(cbxStatus.ItemIndex);
  CapabilityStatement.date := TDateTimeEx.make(dedDate.DateTime, dttzLocal);
  CapabilityStatement.kind := TFhirCapabilityStatementKindEnum(cbxKind.ItemIndex);
  CapabilityStatement.jurisdictionList.Clear;
  cc := getJurisdiction(cbxJurisdiction.ItemIndex);
  if (cc <> nil) then
    CapabilityStatement.jurisdictionList.add(cc);

  CapabilityStatement.implementationGuideList.Clear;
  for s in mImplementationGuides.Lines do
    CapabilityStatement.implementationGuideList.Append.value := s;
end;

procedure TCapabilityStatementEditorFrame.commitResource(res: TFhirCapabilityStatementRestResource);
  procedure interaction(code : TFhirTypeRestfulInteractionEnum; cb : TCheckBox; edt : TEdit);
  var
    ri : TFhirCapabilityStatementRestResourceInteraction;
  begin
    ri := res.interaction(code);
    if cb.IsChecked then
    begin
      if (ri = nil) then
      begin
        ri := res.interactionList.Append;
        ri.code := code;
      end;
      ri.documentation := edt.Text;
    end
    else if (ri <> nil) then
      res.interactionList.DeleteByReference(ri);
  end;
begin
  res.documentation := mDocoRes.Text;
  if edtProfile.Text = '' then
    res.profile := nil
  else
  begin
    if res.profile = nil then
      res.profile := TFhirReference.Create();
    res.profile.reference := edtProfile.Text;
  end;

  interaction(TypeRestfulInteractionread, cbRead, edtDocoRead);
  interaction(TypeRestfulInteractionVread, cbVRead, edtDocoVRead);
  interaction(TypeRestfulInteractionUpdate, cbUpdate, edtDocoUpdate);
  interaction(TypeRestfulInteractionPatch, cbPatch, edtDocoPatch);
  interaction(TypeRestfulInteractionDelete, cbDelete, edtDocoDelete);
  interaction(TypeRestfulInteractionHistoryInstance, cbHistoryInstance, edtDocoHistoryInstance);
  interaction(TypeRestfulInteractionHistoryType, cbHistoryType, edtDocoHistoryType);
  interaction(TypeRestfulInteractioncreate, cbCreate, edtDocoCreate);
  interaction(TypeRestfulInteractionSearchType, cbSearch, edtDocoSearch);

  res.versioning := TFhirVersioningPolicyEnum(cbxVersioning.ItemIndex);
  res.updateCreate := cbUpdateCreate.IsChecked;
  res.conditionalCreate := cbCondCreate.IsChecked;
  res.conditionalUpdate := cbCondUpdate.IsChecked;
  if cbCondDelete.IsChecked then
    res.conditionalDelete := ConditionalDeleteStatusSingle
  else
    res.conditionalDelete := ConditionalDeleteStatusNotSupported;
  res.conditionalRead := TFhirConditionalReadStatusEnum(cbxReadCondition.ItemIndex);

  if cbRefLiteral.IsChecked then
    res.referencePolicy := res.referencePolicy + [ReferenceHandlingPolicyLiteral]
  else
    res.referencePolicy := res.referencePolicy - [ReferenceHandlingPolicyLiteral];
  if cbRefLogical.IsChecked then
    res.referencePolicy := res.referencePolicy + [ReferenceHandlingPolicyLogical]
  else
    res.referencePolicy := res.referencePolicy - [ReferenceHandlingPolicyLogical];
  if cbRefResolve.IsChecked then
    res.referencePolicy := res.referencePolicy + [ReferenceHandlingPolicyResolves]
  else
    res.referencePolicy := res.referencePolicy - [ReferenceHandlingPolicyResolves];
  if cbRefEnforced.IsChecked then
    res.referencePolicy := res.referencePolicy + [ReferenceHandlingPolicyEnforced]
  else
    res.referencePolicy := res.referencePolicy - [ReferenceHandlingPolicyEnforced];
  if cbRefLocal.IsChecked then
    res.referencePolicy := res.referencePolicy + [ReferenceHandlingPolicyLocal]
  else
    res.referencePolicy := res.referencePolicy - [ReferenceHandlingPolicyLocal];
end;

procedure TCapabilityStatementEditorFrame.commitRest(rest: TFhirCapabilityStatementRest);
begin
  rest.documentation := mDoco.Text;
  if (mSecurity.Text = '') and not (cbCORS.IsChecked or cbOAuth.IsChecked or cbClientCerts.IsChecked or cbSmart.IsChecked) then
    rest.security := nil
  else
  begin
    if rest.security = nil then
      rest.security := TFhirCapabilityStatementRestSecurity.Create;
    rest.security.description := mSecurity.Text;
    rest.security.cors := cbCORS.IsChecked;
    rest.security.serviceList.hasCode['http://hl7.org/fhir/restful-security-service', 'OAuth'] := cbOAuth.IsChecked;
    rest.security.serviceList.hasCode['http://hl7.org/fhir/restful-security-service', 'Certificates'] := cbClientCerts.IsChecked;
    rest.security.serviceList.hasCode['http://hl7.org/fhir/restful-security-service', 'SMART-on-FHIR'] := cbSmart.IsChecked;
  end;
end;

function TCapabilityStatementEditorFrame.GetCapabilityStatement: TFHIRCapabilityStatement;
begin
  result := TFHIRCapabilityStatement(Resource);
end;

function TCapabilityStatementEditorFrame.getJurisdiction(i: integer): TFHIRCodeableConcept;
begin
  case i of
    1:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'AT');
    2:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'AU');
    3:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'BR');
    4:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'CA');
    5:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'CH');
    6:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'CL');
    7:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'CN');
    8:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'DE');
    9:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'DK');
    10:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'EE');
    11:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'ES');
    12:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'FI');
    13:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'FR');
    14:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'GB');
    15:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'NL');
    16:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'NO');
    17:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'NZ');
    18:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'RU');
    19:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'US');
    21:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'VN');
    22:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '001');
    23:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '002');
    24:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '019');
    25:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '142');
    26:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '150');
    27:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '053');
    else
      result := nil;
  end;
end;

procedure TCapabilityStatementEditorFrame.inputChanged(Sender: TObject);
begin
  if not loading then
    setInputIsDirty;
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
  SetInputIsNotDirty;
end;

procedure TCapabilityStatementEditorFrame.loadMetadata;
var
  url : TFHIRUri;
begin
  cbExperimental.IsChecked := CapabilityStatement.experimental;
  cbXml.IsChecked := CapabilityStatement.Format[ffXml];
  cbJson.IsChecked := CapabilityStatement.Format[ffJson];
  cbTurtle.IsChecked := CapabilityStatement.Format[ffTurtle];
  cbTerminologyService.IsChecked := CapabilityStatement.instantiates['http://hl7.org/fhir/CapabilityStatement/terminology-server'];

  edtURL.Text := CapabilityStatement.url;
  edtName.Text := CapabilityStatement.name;
  edtTitle.Text := CapabilityStatement.title;
  edtFHIRVersion.Text := CapabilityStatement.fhirVersion;
  edtVersion.Text := CapabilityStatement.version;
  edtPublisher.text := CapabilityStatement.publisher;
  edtDescription.Text := CapabilityStatement.description;
  edtPurpose.Text := CapabilityStatement.purpose;
  edtCopyright.Text := CapabilityStatement.copyright;
  cbxStatus.ItemIndex := ord(CapabilityStatement.status);
  dedDate.DateTime := CapabilityStatement.date.DateTime;
  cbxKind.ItemIndex := ord(CapabilityStatement.kind);
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

  cbxVersioning.ItemIndex := ord(res.versioning);
  cbUpdateCreate.IsChecked := res.updateCreate;
  cbCondCreate.IsChecked := res.conditionalCreate;
  cbCondUpdate.IsChecked := res.conditionalUpdate;
  cbCondDelete.IsChecked := ord(res.conditionalDelete) > 1;
  cbxReadCondition.ItemIndex := ord(res.conditionalRead);
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
    cbOAuth.IsChecked := rest.security.serviceList.hasCode['http://hl7.org/fhir/restful-security-service', 'OAuth'];
    cbClientCerts.IsChecked := rest.security.serviceList.hasCode['http://hl7.org/fhir/restful-security-service', 'Certificates'];
    cbSmart.IsChecked := rest.security.serviceList.hasCode['http://hl7.org/fhir/restful-security-service', 'SMART-on-FHIR'];
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
        if c.code = 'AT' then exit(1);
        if c.code = 'AU' then exit(2);
        if c.code = 'BR' then exit(3);
        if c.code = 'CA' then exit(4);
        if c.code = 'CH' then exit(5);
        if c.code = 'CL' then exit(6);
        if c.code = 'CN' then exit(7);
        if c.code = 'DE' then exit(8);
        if c.code = 'DK' then exit(9);
        if c.code = 'EE' then exit(10);
        if c.code = 'ES' then exit(11);
        if c.code = 'FI' then exit(12);
        if c.code = 'FR' then exit(13);
        if c.code = 'GB' then exit(14);
        if c.code = 'NL' then exit(15);
        if c.code = 'NO' then exit(16);
        if c.code = 'NZ' then exit(17);
        if c.code = 'RU' then exit(18);
        if c.code = 'US' then exit(19);
        if c.code = 'VN' then exit(20);
      end
      else if c.system = 'http://unstats.un.org/unsd/methods/m49/m49.htm' then
      begin
        if c.code = '001' { World } then exit(22);
        if c.code = '002' { Africa } then exit(23);
        if c.code = '019' { Americas } then exit(24);
        if c.code = '142' { Asia } then exit(25);
        if c.code = '150' { Europe } then exit(26);
        if c.code = '053' { Australia and New Zealand } then exit(27);
      end
    end;
end;

procedure TCapabilityStatementEditorFrame.setInputIsDirty;
begin
  if not InputIsDirty then
  begin
    InputIsDirty := true;
    btnCommit.Enabled := true;
    btnCancel.Enabled := true;
    tvStructure.Enabled := false;
    FActive := tvStructure.Selected;
  end;
end;

procedure TCapabilityStatementEditorFrame.SetInputIsNotDirty;
begin
  InputIsDirty := false;
  btnCommit.Enabled := false;
  btnCancel.Enabled := false;
  tvStructure.Enabled := true;
end;

procedure TCapabilityStatementEditorFrame.tvStructureClick(Sender: TObject);
var
  obj : TObject;
begin
  Loading := true;
  try
    obj := tvStructure.Selected.TagObject;
    if obj is TFhirCapabilityStatement then
    begin
      tbMetadata.TagObject := obj;
      tbStructure.ActiveTab := tbMetadata;
      loadMetadata;
    end
    else if obj is TFhirCapabilityStatementRest then
    begin
      tbRest.TagObject := obj;
      tbStructure.ActiveTab := tbRest;
      loadRest(obj as TFhirCapabilityStatementRest);
    end
    else if obj is TFhirCapabilityStatementRestResource then
    begin
      tbResource.TagObject := obj;
      tbStructure.ActiveTab := tbResource;
      loadResource(obj as TFhirCapabilityStatementRestResource);
    end
  finally
    Loading := false;
  end;
end;

end.
