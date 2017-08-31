unit AppEndorserForm;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, IniFiles,
  SystemSupport, DateSupport, GuidSupport, FileSupport, StringSupport,
  FHIRClient, FHIRTypes, FHIRResources, FHIRUtilities, FHIRParser,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.StdCtrls, FMX.ListView, FMX.Layouts, FMX.TreeView, FMX.Platform,
  FMX.Controls.Presentation, FMX.Edit, FMX.TabControl, FMX.ListBox,
  OrganizationChooser;

type
  TAppEndorsementForm = class(TForm)
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    tvEntities: TTreeView;
    Splitter1: TSplitter;
    btnClose: TButton;
    Label2: TLabel;
    Label3: TLabel;
    tiApplications: TTreeViewItem;
    tiOrganizations: TTreeViewItem;
    tabPages: TTabControl;
    tabNull: TTabItem;
    tabOrganization: TTabItem;
    tabApplication: TTabItem;
    Label5: TLabel;
    btnSave: TButton;
    edtOrgName: TEdit;
    cbOrgActive: TCheckBox;
    Label7: TLabel;
    edtAppName: TEdit;
    Label6: TLabel;
    cbxAppStatus: TComboBox;
    Label8: TLabel;
    edtAppJWT: TEdit;
    Button1: TButton;
    ComboBox2: TComboBox;
    btnAppNewEndorsement: TButton;
    btnNewApplication: TButton;
    btnNewOrganization: TButton;
    Panel7: TPanel;
    btnLeftNewApplication: TButton;
    btnLeftNewOrganization: TButton;
    tabObservation: TTabItem;
    Label4: TLabel;
    edtEndAppName: TEdit;
    Label9: TLabel;
    edtEndOrgName: TEdit;
    Label10: TLabel;
    cbxEndorsementType: TComboBox;
    Label11: TLabel;
    edtEndorsementText: TEdit;
    Label12: TLabel;
    cbxEndorsementStatus: TComboBox;
    btnDownload: TButton;
    sdPack: TSaveDialog;
    btnUpload: TButton;
    odUpload: TOpenDialog;
    Label13: TLabel;
    edtOrgURL: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure tvEntitiesClick(Sender: TObject);
    procedure editorGeneralChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnSaveClick(Sender: TObject);
    procedure Panel7Resize(Sender: TObject);
    procedure btnNewApplicationClick(Sender: TObject);
    procedure btnLeftNewApplicationClick(Sender: TObject);
    procedure btnLeftNewOrganizationClick(Sender: TObject);
    procedure btnNewOrganizationClick(Sender: TObject);
    procedure btnAppNewEndorsementClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
    procedure btnUploadClick(Sender: TObject);
  private
    { Private declarations }
    FClient : TFhirClient;
    FOrganizations : TFhirOrganizationList;
    FApplications : TFhirDeviceList;
    FEndorsements : TFhirObservationList;
    FActive : TFhirResource;

    procedure SetNotDirty;
    procedure SetIsDirty;
    procedure disableTreeView;
    procedure enableTreeView;
    procedure disableChildren(children : TFmxChildrenList);
    procedure enableChildren(children : TFmxChildrenList);

    function describeOrg(ref : TFhirReference) : String;
    function describeApp(ref : TFhirReference) : String;
    procedure ClearApplications;
    procedure ClearOrganizations;
    procedure ClearEndorsements;
    procedure LoadApplications;
    procedure LoadApplication(app : TFhirDevice);
    procedure saveApplication;
    procedure addApplication;
    procedure LoadOrganizations;
    procedure LoadOrganization(org : TFhirOrganization);
    procedure saveOrganization;
    procedure addOrganization;
    function findApplication(ref : String) : TFhirDevice;
    function findOrganization(ref : String) : TFhirOrganization;

    procedure LoadEndorsements;
    procedure addEndorsementForApp(org : TFhirOrganization);
    procedure LoadEndorsement(endorsement : TFhirObservation);
    procedure saveEndorsement;
  public
    { Public declarations }
    procedure Load(client : TFhirClient);
  end;

var
  AppEndorsementForm: TAppEndorsementForm;

implementation

{$R *.fmx}
{$R *.Surface.fmx MSWINDOWS}
{$R *.Windows.fmx MSWINDOWS}
{$R *.Macintosh.fmx MACOS}

const
  MAGIC_OBS = 'http://healthintersections.com.au/fhir/codes/obs';
  EXT_JWT = 'http://www.healthintersections.com.au/fhir/StructureDefinition/JWT';

procedure TAppEndorsementForm.Load(client : TFhirClient);
var
  cs : IFMXCursorService;
begin
  FClient := client.link;
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    cs := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService
  else
    cs := nil;
  if Assigned(CS) then
  begin
    Cursor := CS.GetCursor;
    CS.SetCursor(crHourGlass);
  end;
  try
    ClearEndorsements;
    ClearApplications;
    ClearOrganizations;

    btnDownload.Enabled := true;
    btnUpload.Enabled := true;
    LoadApplications;
    LoadOrganizations;
    LoadEndorsements;
    EnableTreeView;
  finally
    if Assigned(CS) then
      cs.setCursor(Cursor);
  end;
  tvEntitiesClick(nil);
end;

procedure TAppEndorsementForm.btnSaveClick(Sender: TObject);
begin
  if FActive = nil then
    raise Exception.Create('Active is nil?')
  else if FActive is TFhirOrganization then
    saveOrganization
  else if FActive is TFhirDevice then
    saveApplication
  else if FActive is TFhirObservation then
    saveEndorsement;
end;

function localPathFromURL(url : String) : String;
var
  l, r : String;
begin
  StringSplit(url, '//', l, r);
  StringSplit(r, '/', l, r);
  result := '/'+r;
end;

procedure TAppEndorsementForm.btnUploadClick(Sender: TObject);
var
  bnd : TFhirBundle;
  be : TFhirBundleEntry;
begin
  if odUpload.Execute then
  begin
    bnd := TFHIRJsonParser.ParseFile(nil, 'en', odUpload.filename) as TFhirBundle;
    try
      bnd.type_ := BundleTypeTransaction;
      for be in bnd.entryList do
      begin
        be.request := TFhirBundleEntryRequest.Create;
        be.request.method := HttpVerbPUT;
        be.request.url := localPathFromURL(be.fullUrl);
      end;
      FClient.transaction(bnd);
    finally
      bnd.free;
    end;
  end;
end;

procedure TAppEndorsementForm.Button1Click(Sender: TObject);
var
  pIn, pOut : TFhirParameters;
begin
  pIn := TFhirParameters.Create;
  try
    with pIn.parameterList.Append do
    begin
      name := 'for';
      value := TFhirString.Create(edtAppName.Text);
    end;
    pOut := FClient.operation(frtNull, 'jwt', pIn) as TFhirParameters;
    try
      edtAppJWT.Text := pOut.str['jwt'];
    finally
      pOut.Free;
    end;
  finally
    pIn.Free;
  end;

end;

procedure TAppEndorsementForm.addApplication;
var
  app : TFhirDevice;
  ti : TTreeViewItem;
begin
  app := TFhirDevice.Create;
  try
    app.model := 'New Application';
    app.status := DeviceStatusActive;
    app.type_ := TFhirCodeableConcept.Create;
    with app.type_.codingList.Append do
    begin
      system := MAGIC_OBS;
      code := 'app';
    end;
    FApplications.add(app.Link);

    ti := TTreeViewItem.Create(tiApplications);
    ti.text := app.model;
    ti.TagObject := app;
    app.TagObject := ti;
    tiApplications.AddObject(ti);
    tvEntities.Selected := ti;
    tvEntitiesClick(nil);
  finally
    app.Free;
  end;
  SetIsDirty;
end;

procedure TAppEndorsementForm.addEndorsementForApp(org: TFhirOrganization);
var
  app : TFhirDevice;
  endorsement : TFhirObservation;
  ti : TTreeViewItem;
begin
  app := tvEntities.Selected.TagObject as TFhirDevice;

  endorsement := TFhirObservation.Create;
  try
    endorsement.subject := TFhirReference.Create;
    endorsement.subject.reference := 'Device/'+app.id;
    endorsement.performerList.Append.reference := 'Organization/'+org.id;
    endorsement.status := ObservationStatusFinal;
    endorsement.effective := TFhirDateTime.Create(TDateTimeEx.makeUTC);
    with endorsement.categoryList.Append.codingList.Append do
    begin
      system := MAGIC_OBS;
      code := 'endorsement';
    end;
    FEndorsements.add(endorsement.Link);

    ti := TTreeViewItem.Create(tvEntities.Selected);
    ti.text := org.name;
    ti.TagObject := endorsement;
    org.TagObject := ti;
    tvEntities.Selected.AddObject(ti);
    tvEntities.Selected := ti;
    tvEntitiesClick(nil);
  finally
    endorsement.Free;
  end;
  SetIsDirty;
end;

procedure TAppEndorsementForm.addOrganization;
var
  org : TFhirOrganization;
  ti : TTreeViewItem;
begin
  org := TFhirOrganization.Create;
  try
    org.name := 'New Organization';
    org.active := true;
    FOrganizations.add(org.Link);

    ti := TTreeViewItem.Create(tiOrganizations);
    ti.text := org.name;
    ti.TagObject := org;
    org.TagObject := ti;
    tiOrganizations.AddObject(ti);
    tvEntities.Selected := ti;
    tvEntitiesClick(nil);
  finally
    org.Free;
  end;
  SetIsDirty;
end;

procedure TAppEndorsementForm.btnAppNewEndorsementClick(Sender: TObject);
var
  org : TFhirOrganization;
begin
  if FOrganizations.Count = 0 then
    ShowMessage('You need to add at least one organization')
  else if pickOrganization(self, FOrganizations, org) then
    addEndorsementForApp(org);
end;

procedure TAppEndorsementForm.btnCloseClick(Sender: TObject);
begin
  if not btnSave.Enabled then
    Close
  else if MessageDlg('Abandon Changes?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    tvEntitiesClick(nil);
end;

procedure TAppEndorsementForm.ClearApplications;
var
  i : integer;
begin
  for i := tiApplications.Count - 1 downto 0 do
    tiApplications.RemoveObject(tiApplications.Items[i]);
  FApplications.Free;
  FApplications := nil;
end;

procedure TAppEndorsementForm.ClearEndorsements;
begin
  FEndorsements.Free;
  FEndorsements := nil;
end;

procedure TAppEndorsementForm.ClearOrganizations;
var
  i : integer;
begin
  for i := tiOrganizations.Count - 1 downto 0 do
    tiOrganizations.RemoveObject(tiOrganizations.Items[i]);
  FOrganizations.Free;
  FOrganizations := nil;
end;

function TAppEndorsementForm.describeApp(ref: TFhirReference): String;
var
  app : TFhirDevice;
begin
  result := '';
  if (ref <> nil) then
  begin
    for app in FApplications do
      if ref.reference = 'Device/'+app.id then
        if app.model <> '' then
          exit(app.model);
  end;
end;

function TAppEndorsementForm.describeOrg(ref: TFhirReference): String;
var
  org : TFhirOrganization;
begin
  result := '';
  if (ref <> nil) then
  begin
    for org in FOrganizations do
      if ref.reference = 'Organization/'+org.id then
        if org.name <> '' then
          exit(org.name);
  end;
end;

procedure TAppEndorsementForm.disableChildren(children: TFmxChildrenList);
var
  obj : TFmxObject;
begin
  if children <> nil then
    for obj in children do
      if obj is TControl then
      begin
        TControl(obj).enabled := false;
        disableChildren(obj.Children);
      end;
end;

procedure TAppEndorsementForm.disableTreeView;
begin
  tvEntities.Enabled := false;
  disableChildren(tvEntities.Children);
end;

procedure TAppEndorsementForm.editorGeneralChange(Sender: TObject);
begin
  SetIsDirty;
end;

procedure TAppEndorsementForm.enableChildren(children: TFmxChildrenList);
var
  obj : TFmxObject;
begin
  if children <> nil then
    for obj in children do
      if obj is TControl then
      begin
        TControl(obj).enabled := true;
        enableChildren(obj.Children);
      end;
end;

procedure TAppEndorsementForm.enableTreeView;
begin
  tvEntities.Enabled := true;
  enableChildren(tvEntities.Children);
end;

function TAppEndorsementForm.findApplication(ref: String): TFhirDevice;
var
  dev :  TFhirDevice;
begin
  result := nil;
  if ref.StartsWith('Device/') then
  begin
    ref := ref.subString(7);
    for dev in FApplications do
      if dev.id = ref then
        exit(dev);
  end;
end;

function TAppEndorsementForm.findOrganization(ref: String): TFhirOrganization;
var
  org :  TFhirOrganization;
begin
  result := nil;
  if ref.StartsWith('Organization/') then
  begin
    ref := ref.subString(13);
    for org in FOrganizations do
      if org.id = ref then
        exit(org);
  end;
end;

procedure TAppEndorsementForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if btnSave.Enabled then
    CanClose := MessageDlg('Abandon Changes?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes
  else
    CanClose := true;
end;

procedure TAppEndorsementForm.FormCreate(Sender: TObject);
begin
  tabPages.ActiveTab := tabNull;
  disableTreeView;
end;

procedure TAppEndorsementForm.FormDestroy(Sender: TObject);
begin
  ClearEndorsements;
  ClearApplications;
  ClearOrganizations;
  FActive.Free;
  FClient.Free;
end;

procedure TAppEndorsementForm.LoadApplication(app: TFhirDevice);
var
  id : TFhirIdentifier;
begin
  edtAppName.text := app.model;
  cbxAppStatus.ItemIndex := ord(app.status)-1;
  edtAppJWT.text := app.getExtensionString(EXT_JWT);
  FActive := app.clone;
  SetNotDirty;
end;

procedure TAppEndorsementForm.LoadApplications;
var
  bnd : TFHIRBundle;
  be :  TFHIRBundleEntry;
  dev : TFhirDevice;
  ti : TTreeViewItem;
begin
  ClearApplications;

  FApplications := TFhirDeviceList.Create;

  bnd := FClient.search(frtDevice, true, 'type=app&sort=+model');
  try
    for be in bnd.entryList do
    begin
      dev := be.resource as TFhirDevice;
      ti := TTreeViewItem.Create(tiApplications);
      ti.text := dev.model;
      ti.TagObject := dev;
      dev.TagObject := ti;
      tiApplications.AddObject(ti);
      FApplications.Add(dev.Link);
    end;
    tiApplications.expand;
  finally
    bnd.free;
  end;
end;

procedure TAppEndorsementForm.LoadEndorsement(endorsement: TFhirObservation);
begin
  edtEndAppName.Text := describeApp(endorsement.subject);
  if endorsement.performerList.Count = 0 then
    edtEndOrgName.Text := '??'
  else
    edtEndOrgName.Text := describeOrg(endorsement.performerList[0]);
  edtEndorsementText.text := endorsement.comment;
  if (endorsement.code = nil) or (endorsement.code.codingList.Count = 0) or (endorsement.code.codingList[0].system <> MAGIC_OBS) then
    cbxEndorsementType.ItemIndex := -1
  else if endorsement.code.codingList[0].code = 'approval' then
    cbxEndorsementType.ItemIndex := 0
  else if endorsement.code.codingList[0].code = 'warning' then
    cbxEndorsementType.ItemIndex := 1
  else if endorsement.code.codingList[0].code = 'usage-note' then
    cbxEndorsementType.ItemIndex := 2;
  cbxEndorsementStatus.ItemIndex := ord(endorsement.status)-1;
  FActive := endorsement.clone;
  SetNotDirty;
end;

procedure TAppEndorsementForm.LoadEndorsements;
var
  bnd : TFHIRBundle;
  be : TFHIRBundleEntry;
  obs : TFhirObservation;
  ti : TTreeViewItem;
  app : TFHIRDevice;
  org : TFhirOrganization;
begin
  ClearEndorsements;

  FEndorsements := TFhirObservationList.Create;
  bnd := FClient.search(frtObservation, true, 'category='+MAGIC_OBS+'|endorsement&sort=+date');
  try
    for be in bnd.entryList do
    begin
      obs := be.resource as TFhirObservation;
      if (obs.subject <> nil) and (obs.performerList.Count > 0) then
      begin
        app := findApplication(obs.subject.reference);
        org := findOrganization(obs.performerList[0].reference);
        if (app <> nil) and (org <> nil) then
        begin
          ti := TTreeViewItem.Create(TTreeViewItem(app.TagObject));
          ti.text := org.name;
          ti.TagObject := obs;
          obs.TagObject := ti;
          TTreeViewItem(app.TagObject).AddObject(ti);
          FEndorsements.Add(obs.Link);
        end;
      end;
    end;
  finally
    bnd.free;
  end;
end;

procedure TAppEndorsementForm.LoadOrganization(org: TFhirOrganization);
var
  c : TFhirContactPoint;
begin
  edtOrgName.text := org.name;
  for c in org.telecomList do
    if c.system = ContactPointSystemUrl then
      edtOrgURL.text := c.value;
  edtOrgName.text := org.name;
  cbOrgActive.IsChecked := org.active;
  FActive := org.clone;
  SetNotDirty;
end;

procedure TAppEndorsementForm.LoadOrganizations;
var
  bnd : TFHIRBundle;
  be : TFHIRBundleEntry;
  org : TFhirOrganization;
  ti : TTreeViewItem;
begin
  ClearOrganizations;

  FOrganizations := TFhirOrganizationList.Create();
  bnd := FClient.search(frtOrganization, true, 'sort=+name');
  try
    for be in bnd.entryList do
    begin
      org := be.resource as TFhirOrganization;
      ti := TTreeViewItem.Create(tiOrganizations);
      ti.text := org.name;
      ti.TagObject := org;
      org.TagObject := ti;
      tiOrganizations.AddObject(ti);
      FOrganizations.Add(org.Link);
    end;
    tiOrganizations.expand;
  finally
    bnd.free;
  end;
end;

procedure TAppEndorsementForm.Panel7Resize(Sender: TObject);
begin
  btnLeftNewApplication.Width := Panel7.Width / 2;
end;

procedure TAppEndorsementForm.saveApplication;
var
  app, upd, res : TFhirDevice;
  id : String;
begin
  if edtAppName.Text = '' then
    raise Exception.Create('An Application Name is required');
  if cbxAppStatus.ItemIndex < 0 then
    raise Exception.Create('An Application status is required');

  app := FActive as TFhirDevice;
  app.model := edtAppName.Text;
  app.status := TFhirDeviceStatusEnum(cbxAppStatus.ItemIndex+1);
  if edtAppJWT.Text <> '' then
    app.setExtensionString(EXT_JWT, edtAppJWT.Text)
  else
    app.removeExtension(EXT_JWT);

  if (app.id <> '') then
    upd := FClient.updateResource(app) as TFhirDevice
  else
    upd := FClient.CreateResource(app, id) as TFhirDevice;
  try
    for res in FApplications do
      if res.id = app.id then
      begin
        res.assign(upd);
        TTreeViewItem(res.TagObject).Text := upd.model;
      end;
  finally
    upd.free;
  end;
  tvEntities.selected := TTreeViewItem(app.tagObject);
  SetNotDirty;
  tvEntitiesClick(nil);
end;

procedure TAppEndorsementForm.saveEndorsement;
var
  obs, upd, res : TFhirObservation;
  id : String;
begin
  if edtEndorsementText.Text = '' then
    raise Exception.Create('Some text is required');
  if cbxEndorsementType.ItemIndex < 0 then
    raise Exception.Create('An Endorsement Type is required');

  obs := FActive as TFhirObservation;
  obs.comment := edtEndorsementText.Text;
  obs.code := TFhirCodeableConcept.Create;
  with obs.code.codingList.Append do
  begin
    system := MAGIC_OBS;
    case cbxEndorsementType.ItemIndex of
      0: code := 'approval';
      1: code := 'warning';
      2: code := 'usage-note';
    end;
  end;
  obs.status := TFhirObservationStatusEnum(cbxEndorsementStatus.ItemIndex+1);

  if (obs.id <> '') then
    upd := FClient.updateResource(obs) as TFhirObservation
  else
    upd := FClient.CreateResource(obs, id) as TFhirObservation;
  try
    for res in FEndorsements do
      if res.id = obs.id then
        res.assign(upd);
  finally
    upd.free;
  end;
  tvEntities.selected := TTreeViewItem(obs.tagObject);
  SetNotDirty;
  tvEntitiesClick(nil);
end;

procedure TAppEndorsementForm.saveOrganization;
var
  org, upd, res : TFhirOrganization;
  id : String;
  i : integer;
  c : TFhirContactPoint;
  b : boolean;
begin
  if edtOrgName.Text = '' then
    raise Exception.Create('An Application Organization is required');

  org := FActive as TFhirOrganization;
  org.name := edtOrgName.Text;
  org.active := cbOrgActive.IsChecked;
  b := false;
  if edtOrgURL.text <> '' then
  begin
    for c in org.telecomList do
      if c.system = ContactPointSystemUrl then
      begin
        b := true;
        c.value := edtOrgURL.text;
      end;
    if not b then
      with org.telecomList.Append do
      begin
        system := ContactPointSystemUrl;
        value := edtOrgURL.text;
      end;
  end
  else
    for i := org.telecomList.Count - 1 downto 0 do
      if org.telecomList[i].system = ContactPointSystemUrl then
        org.telecomList.Remove(i);

  if (org.id <> '') then
    upd := FClient.updateResource(org) as TFhirOrganization
  else
    upd := FClient.CreateResource(org, id) as TFhirOrganization;
  try
    for res in FOrganizations do
      if res.id = org.id then
      begin
        res.assign(upd);
        TTreeViewItem(res.TagObject).Text := upd.name;
      end;
  finally
    upd.free;
  end;
  tvEntities.selected := TTreeViewItem(org.tagObject);
  SetNotDirty;

  tvEntitiesClick(nil);
end;

procedure TAppEndorsementForm.SetIsDirty;
begin
  btnSave.Enabled := true;
  btnClose.Text := 'Cancel';
  tvEntities.Enabled := false;
  btnNewOrganization.Enabled := false;
  btnLeftNewOrganization.Enabled := false;
  btnNewApplication.Enabled := false;
  btnLeftNewApplication.Enabled := false;
  btnAppNewEndorsement.Enabled := false;
end;

procedure TAppEndorsementForm.SetNotDirty;
begin
  btnSave.Enabled := false;
  btnClose.Text := 'Close';
  tvEntities.Enabled := true;
  btnNewOrganization.Enabled := true;
  btnLeftNewOrganization.Enabled := true;
  btnNewApplication.Enabled := true;
  btnLeftNewApplication.Enabled := true;
  btnAppNewEndorsement.Enabled := true;
end;

procedure TAppEndorsementForm.tvEntitiesClick(Sender: TObject);
var
  item : TTreeViewItem;
begin
  if not tvEntities.Enabled then
    exit;

  FActive.Free;
  FActive := nil;

  item := tvEntities.Selected;
  btnNewOrganization.Enabled := true;
  btnLeftNewOrganization.Enabled := true;
  btnNewApplication.Enabled := true;
  btnLeftNewApplication.Enabled := true;
  if (item = nil) then
  begin
    tabPages.ActiveTab := tabNull;
  end
  else if (item.TagObject = nil) then
  begin
    tabPages.ActiveTab := tabNull;
    btnNewOrganization.Enabled := FClient <> nil;
    btnLeftNewOrganization.Enabled := FClient <> nil;
    btnNewApplication.Enabled := FClient <> nil;
    btnLeftNewApplication.Enabled := FClient <> nil;
  end
  else if item.TagObject is TFhirDevice then
  begin
    tabPages.ActiveTab := tabApplication;
    LoadApplication(item.TagObject as TFhirDevice);
  end
  else if item.TagObject is TFhirOrganization then
  begin
    tabPages.ActiveTab := tabOrganization;
    LoadOrganization(item.TagObject as TFhirOrganization);
  end
  else if item.TagObject is TFhirObservation then
  begin
    tabPages.ActiveTab := tabObservation;
    LoadEndorsement(item.TagObject as TFhirObservation);
  end;
end;

procedure TAppEndorsementForm.btnNewApplicationClick(Sender: TObject);
begin
  addApplication;
end;

procedure TAppEndorsementForm.btnNewOrganizationClick(Sender: TObject);
begin
  addOrganization;
end;

procedure TAppEndorsementForm.btnDownloadClick(Sender: TObject);
var
  bnd : TFhirBundle;
  app : TFhirDevice;
  org : TFhirOrganization;
  endorsement : TFhirObservation;
begin
  if sdPack.execute then
  begin
    bnd := TFhirBundle.Create(BundleTypeCollection);
    try
      bnd.id := NewGuidId;
      for app in FApplications do
        with bnd.entryList.Append do
        begin
          resource := app.Link;
          fullUrl := URLPath([FClient.address, 'Device', app.id]);
        end;
      for org in FOrganizations do
        with bnd.entryList.Append do
        begin
          resource := org.Link;
          fullUrl := URLPath([FClient.address, 'Organization', org.id]);
        end;
      for endorsement in FEndorsements do
        with bnd.entryList.Append do
        begin
          resource := endorsement.Link;
          fullUrl := URLPath([FClient.address, 'Observation', endorsement.id]);
        end;
      TFHIRJsonComposer.composeFile(nil, bnd, 'en', sdPack.FileName, true);
    finally
      bnd.Free;
    end;
  end;
end;

procedure TAppEndorsementForm.btnLeftNewApplicationClick(Sender: TObject);
begin
  addApplication;
end;

procedure TAppEndorsementForm.btnLeftNewOrganizationClick(Sender: TObject);
begin
  addOrganization;
end;

end.


