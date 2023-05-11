unit ProviderDirectoryForm;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.TabControl, FMX.ListBox, FMX.Layouts, FMX.DateTimeCtrls,
  FMX.Edit, System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.Platform,
  IdComponent,
  fsl_base, fsl_utilities,
  fhir_objects, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Resources.Base, FHIR.Version.Client, FHIR.Version.Utilities, fhir_xhtml, FHIR.Ui.ComboFMX,
  BaseFrame, CapabilityStatementEditor, FMX.WebBrowser,
  FMX.Memo;

const
  HPIO_SYSTEM = 'http://ns.electronichealth.net.au/id/hi/hpio/1.0';

type
  TFrame = TBaseFrame; // re-aliasing the Frame to work around a designer bug

  TProviderDirectoryFrame = class (TFrame)
    Panel2: TPanel;
    Panel3: TPanel;
    lblOutcome: TLabel;
    btnFetchMore: TButton;
    Panel1: TPanel;
    Panel4: TPanel;
    pnlSearch: TPanel;
    btnHSSearch: TButton;
    Splitter1: TSplitter;
    pnlMessages: TPanel;
    lblMessages: TLabel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    edtServiceName: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    edtHPIO: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    cbxType: TComboBox;
    gridHSMatches: TGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn7: TStringColumn;
    StringColumn8: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    Panel5: TPanel;
    btnSearchProvider: TButton;
    Label4: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    edtIdentifier: TEdit;
    edtPostcode: TEdit;
    edtFamily: TEdit;
    edtSuburb: TEdit;
    cbxState: TComboBox;
    gridPRMatches: TGrid;
    StringColumn9: TStringColumn;
    StringColumn10: TStringColumn;
    StringColumn11: TStringColumn;
    StringColumn12: TStringColumn;
    StringColumn13: TStringColumn;
    StringColumn14: TStringColumn;
    StringColumn15: TStringColumn;
    StringColumn16: TStringColumn;
    Label16: TLabel;
    edtGiven: TEdit;
    Label17: TLabel;
    cbxPayload: TComboBox;
    Label19: TLabel;
    cbxIdType: TComboBox;
    Label9: TLabel;
    edtOrgName: TEdit;
    StringColumn17: TStringColumn;
    gridProperties: TGrid;
    StringColumn18: TStringColumn;
    StringColumn19: TStringColumn;
    edtSpecialty: TEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure btnHSSearchClick(Sender: TObject);
    procedure gridHSMatchesGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure btnFetchMoreClick(Sender: TObject);
    procedure cbxTypeChange(Sender: TObject);
    procedure cbConfUseLastUpdatedClick(Sender: TObject);
    procedure gridHSMatchesSelectCell(Sender: TObject; const ACol, ARow: Integer; var CanSelect: Boolean);
    procedure lblOutcomeClick(Sender: TObject);
    procedure lblMessagesClick(Sender: TObject);
    procedure btnSearchProviderClick(Sender: TObject);
    procedure gridPRMatchesGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridPropertiesGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridPRMatchesSelectCell(Sender: TObject; const ACol, ARow: Integer; var CanSelect: Boolean);
  private
    FClient: TFHIRClient;
    FCSTab : TTabItem;
    FCsForm : TCapabilityStatementEditorFrame;
    FHSBundle : TFhirBundle;
    FHSMatches : TFslList<TFhirHealthcareService>;
    FPrBundle : TFhirBundle;
    FPrMatches : TFslList<TFhirPractitionerRole>;
    FOtherResources : TFslMap<TFHIRResource>;
    FRows : TStringList;
    procedure DoWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    function resolve(ref : TFHIRReference) : TFHIRResource;
    procedure describeHealthcareService(hs : TFhirHealthcareService);
    procedure describePractitioner(name : String; pr : TFhirPractitioner);
    procedure describePractitionerRole(pr : TFhirPractitionerRole);
    procedure describeOrganization(name : String; org : TFhirOrganization);
    procedure describeLocation(name : String; loc : TFhirLocation);
    procedure describeEndPoint(name : String; ep : TFhirEndpoint);
    procedure prop(n, v : String; ifBlank : boolean = false);
    procedure SetClient(const Value: TFHIRClient);
  public
    destructor Destroy; override;
    property Client : TFHIRClient read FClient write SetClient;

    procedure load; override;
  end;

implementation

{$R *.fmx}

uses
  ResourceEditingSupport;

{ TRegistryFrame }

procedure TProviderDirectoryFrame.btnCloseClick(Sender: TObject);
begin
  try
//    Settings.storeValue('Provider-Directory-search', 'url', edtConfUrl.Text);
    Settings.save;
  except
  end;
  Close;
end;

procedure TProviderDirectoryFrame.btnHSSearchClick(Sender: TObject);
var
  op : TFhirOperationOutcome;
begin
  Settings.storeValue('Provider-Directory-search', 'ServiceName', edtServiceName.Text);
  Settings.storeValue('Provider-Directory-search', 'FamilyName', edtFamily.Text);
  Settings.storeValue('Provider-Directory-search', 'GivenName', edtGiven.Text);
  Settings.storeValue('Provider-Directory-search', 'OrganisationName', edtOrgName.Text);
  Settings.storeValue('Provider-Directory-search', 'HPIO', edtHPIO.Text);
  Settings.storeValue('Provider-Directory-search', 'Identifier', edtIdentifier.Text);
  Settings.storeValue('Provider-Directory-search', 'IdType', cbxIdType.ItemIndex);
  Settings.storeValue('Provider-Directory-search', 'Suburb', edtSuburb.Text);
  Settings.storeValue('Provider-Directory-search', 'Postcode', edtPostcode.Text);
  Settings.storeValue('Provider-Directory-search', 'State', cbxState.ItemIndex);
  Settings.storeValue('Provider-Directory-search', 'Specialty', edtSpecialty.Text);
  Settings.storeValue('Provider-Directory-search', 'Type', cbxType.ItemIndex);
  Settings.storeValue('Provider-Directory-search', 'Category', cbxPayload.ItemIndex);

  pnlMessages.Height := 0;
  work('Fetch Resources', true,
    procedure (context : pointer)
    var
      be : TFhirBundleEntry;
      params : TStringList;
      start : TDateTime;
      procedure param(n, v, s : String);
      begin
        if v <> '' then
          if s <> '' then
            params.AddPair(n, s+'|'+v)
          else
            params.AddPair(n, v);
      end;
    begin
      FHSMatches.Clear;
      gridHsMatches.RowCount := FHSMatches.Count;
      FHSBundle.Free;
      FHSBundle := nil;
      params := TStringList.create;
      try

        param('_summary', 'true', '');
        param('_include', 'HealthcareService:organization', '');
        param('_include', 'HealthcareService:endpoint', '');
        param('_include', 'HealthcareService:location', '');
        param('name', edtServiceName.Text, '');
        param('organization.name', edtOrgName.Text, '');
        param('organization.identifier', edtHPIO.Text, 'http://ns.electronichealth.net.au/id/hi/hpio/1.0');
        param('specialty:text', edtSpecialty.Text, '');
        param('location.address-postalcode', edtPostcode.Text, '');
        param('location.address-city', edtSuburb.Text, '');
        param('location.address-state', cbxState.code, '');
        param('endpoint.connection-type', cbxType.searchCode, '');
        param('endpoint.payload-type', cbxPayload.searchCode, '');

        start := now;
        FHSBundle := Client.search(frtHealthcareService, false, params);
        for be in FHSBundle.entryList do
        begin
          if ((be.search = nil) or (be.search.mode = SearchEntryModeNull)) then
          begin
            if be.resource is TFhirOperationOutcome then
            begin
              op := TFhirOperationOutcome(be.resource);
              pnlMessages.Height := 26;
              lblMessages.Text := op.asExceptionMessage;
            end
            else if be.resource is TFhirHealthcareService then
              FHSMatches.Add(be.resource.Link as TFhirHealthcareService)
            else if (be.resource <> nil) and (be.resource.id <> '') then
              FOtherResources.Add(be.resource.fhirType+'/'+be.resource.id, be.resource.Link);
          end
          else
            case be.search.mode of
              SearchEntryModeMatch:
                FHSMatches.Add(be.resource.Link as TFhirHealthcareService);
              SearchEntryModeInclude:
                if be.resource.id <> '' then
                  FOtherResources.AddOrSetValue(be.resource.fhirType+'/'+be.resource.id, be.resource.Link);
              SearchEntryModeOutcome:
                begin
                  op := TFhirOperationOutcome(be.resource);
                  pnlMessages.Height := 26;
                  lblMessages.Text := op.asExceptionMessage;
                end
            end;
          if (be.resource <> nil) and (be.fullUrl <> '') then
            be.resource.Tags['fullUrl'] := be.fullUrl;
        end;
        gridHSMatches.RowCount := FHSMatches.Count;
        lblOutcome.Text := 'Fetched '+inttostr(FHSMatches.Count)+' of '+FHSBundle.total+' resources in '+describePeriod(now - start)+'. search String = '+FMXescape(FClient.lastURL);
        btnFetchMore.Visible := FHSBundle.Links['next'] <> '';
      finally
        params.Free;
      end;
    end);
end;

procedure TProviderDirectoryFrame.btnSearchProviderClick(Sender: TObject);
var
  op : TFhirOperationOutcome;
begin
  Settings.storeValue('Provider-Directory-search', 'ServiceName', edtServiceName.Text);
  Settings.storeValue('Provider-Directory-search', 'FamilyName', edtFamily.Text);
  Settings.storeValue('Provider-Directory-search', 'GivenName', edtGiven.Text);
  Settings.storeValue('Provider-Directory-search', 'OrganisationName', edtOrgName.Text);
  Settings.storeValue('Provider-Directory-search', 'HPIO', edtHPIO.Text);
  Settings.storeValue('Provider-Directory-search', 'Identifier', edtIdentifier.Text);
  Settings.storeValue('Provider-Directory-search', 'IdType', cbxIdType.ItemIndex);
  Settings.storeValue('Provider-Directory-search', 'Suburb', edtSuburb.Text);
  Settings.storeValue('Provider-Directory-search', 'Postcode', edtPostcode.Text);
  Settings.storeValue('Provider-Directory-search', 'State', cbxState.ItemIndex);
  Settings.storeValue('Provider-Directory-search', 'Specialty', edtSpecialty.Text);
  Settings.storeValue('Provider-Directory-search', 'Type', cbxType.ItemIndex);
  Settings.storeValue('Provider-Directory-search', 'Category', cbxPayload.ItemIndex);

  pnlMessages.Height := 0;
  work('Fetch Resources', true,
    procedure (context : pointer)
    var
      be : TFhirBundleEntry;
      params : TStringList;
      start : TDateTime;
      procedure param(n, v, s : String);
      begin
        if v <> '' then
          if s <> '' then
            params.AddPair(n, s+'|'+v)
          else
            params.AddPair(n, v);
      end;
    begin
      FPrMatches.Clear;
      gridPrMatches.RowCount := FPRMatches.Count;
      FPrBundle.Free;
      FPrBundle := nil;
      params := TStringList.create;
      try
        param('_summary', 'true', '');
        param('_include', 'PractitionerRole:practitioner', '');
        param('_include', 'PractitionerRole:organization', '');
        param('_include', 'PractitionerRole:endpoint', '');
        param('_include', 'PractitionerRole:location', '');
        param('practitioner.family', edtFamily.Text, '');
        param('practitioner.given', edtGiven.Text, '');
        param('organization.name', edtOrgName.Text, '');
        case cbxIdType.ItemIndex of
          0: { Provider# } param('identifier', edtIdentifier.Text, 'http://ns.electronichealth.net.au/id/medicare-provider-number');
          1: { HPI-I } param('practitioner.identifier', edtIdentifier.Text, 'http://ns.electronichealth.net.au/id/hi/hpii/1.0');
          2: { HPI-O } param('organization.identifier', edtIdentifier.Text, 'http://ns.electronichealth.net.au/id/hi/hpio/1.0');
          3: { Vendor ID } param('organization.identifier', edtIdentifier.Text, {todo} '');
          4: { NP/OI } param('identifier', edtIdentifier.Text, 'http://ns.electronichealth.net.au/id/npio');
        end;

        param('specialty:text', edtSpecialty.text, '');
        param('location.address-postalcode', edtPostcode.Text, '');
        param('location.address-city', edtSuburb.Text, '');
        param('location.address-state', cbxState.code, '');
        param('endpoint.connection-type', cbxType.searchCode, '');
        param('endpoint.payload-type', cbxPayload.searchCode, '');

        start := now;
        FPrBundle := Client.search(frtPractitionerRole, false, params);
        for be in FPrBundle.entryList do
        begin
          if ((be.search = nil) or (be.search.mode = SearchEntryModeNull)) then
          begin
            if be.resource is TFhirOperationOutcome then
            begin
              op := TFhirOperationOutcome(be.resource);
              pnlMessages.Height := 26;
              lblMessages.Text := op.asExceptionMessage;
            end
            else if be.resource is TFhirPractitionerRole then
              FPrMatches.Add(be.resource.Link as TFhirPractitionerRole)
            else if (be.resource <> nil) and (be.resource.id <> '') then
              FOtherResources.AddOrSetValue(be.resource.fhirType+'/'+be.resource.id, be.resource.Link);
          end
          else
            case be.search.mode of
              SearchEntryModeMatch:
                FPrMatches.Add(be.resource.Link as TFhirPractitionerRole);
              SearchEntryModeInclude:
                if be.resource.id <> '' then
                  FOtherResources.AddOrSetValue(be.resource.fhirType+'/'+be.resource.id, be.resource.Link);
              SearchEntryModeOutcome:
                begin
                  op := TFhirOperationOutcome(be.resource);
                  pnlMessages.Height := 26;
                  lblMessages.Text := op.asExceptionMessage;
                end
            end;
          if (be.resource <> nil) and (be.fullUrl <> '') then
            be.resource.Tags['fullUrl'] := be.fullUrl;
        end;
        gridPrMatches.RowCount := FPrMatches.Count;
        lblOutcome.Text := 'Fetched '+inttostr(FPrMatches.Count)+' of '+FPrBundle.total+' resources in '+describePeriod(now - start)+'. search String = '+FMXescape(FClient.lastURL);
        btnFetchMore.Visible := FPrBundle.Links['next'] <> '';
      finally
        params.Free;
      end;
    end);
end;

procedure TProviderDirectoryFrame.btnFetchMoreClick(Sender: TObject);
var
  op : TFhirOperationOutcome;
begin
  work('Fetch More', true,
    procedure (context : pointer)
    var
      be : TFhirBundleEntry;
      start : TDateTime;
      l : TFhirBundleLink;
      i : integer;
      url : String;
    begin
      btnFetchMore.Visible := false;
      url := FHSBundle.Links['next'];
      FHSBundle.Free;
      FHSBundle := nil;
      start := now;
      FHSBundle := Client.searchAgain(url);
      i := 0;
      for be in FHSBundle.entryList do
        if ((be.search <> nil)) then
          case be.search.mode of
            SearchEntryModeMatch:
              FHSMatches.Add(be.resource.Link as TFhirHealthcareService);
            SearchEntryModeInclude:
              FOtherResources.Add(be.resource.fhirType+'/'+be.resource.id, be.resource.Link);
            SearchEntryModeOutcome:
              begin
                op := TFhirOperationOutcome(be.resource);
                pnlMessages.Height := 26;
                lblMessages.Text := op.asExceptionMessage;
              end
          end;
      gridHSMatches.RowCount := FHSMatches.Count;
      lblOutcome.Text := 'Fetched '+inttostr(i)+' of '+FHSBundle.total+' resources in '+describePeriod(now - start);
      btnFetchMore.Visible := FHSBundle.Links['next'] <> '';
    end);
end;

procedure TProviderDirectoryFrame.cbConfUseLastUpdatedClick(Sender: TObject);
begin
//  edtConfUpdated.Enabled := cbConfUseLastUpdated.IsChecked;
end;

procedure TProviderDirectoryFrame.cbxTypeChange(Sender: TObject);
begin
//  cbxProfile.Enabled := cbxType.ItemIndex = 0;
end;

procedure TProviderDirectoryFrame.describeEndPoint(name: String; ep: TFhirEndpoint);
var
  id : TFhirIdentifier;
  cp : TFHIRContactPoint;
  cc : TFhirCodeableConcept;
  c : TFhirCode;
  s : TFhirString;
begin
  prop(name, '', true);
  prop('  name', ep.name);
  prop('  source', ep.getExtensionString('http://hl7.org/fhir/StructureDefinition/extension-Meta.source|3.2'));
  prop('  full url', ep.Tags['fullUrl']);
  prop('  address', ep.address);
  for id in ep.identifierList do
    if id.system = 'http://ns.electronichealth.net.au/smd/target' then
      prop('  smd-target', id.value)
    else
      prop('  identifier', gen(id));
  prop('  status', CODES_TFhirEndpointStatusEnum[ep.status]);
  prop('  connection type', gen(ep.connectionType));
  for cp in ep.contactList do
    prop('  contact', gen(cp));
  prop('  period', gen(ep.period));
  for cc in ep.payloadTypeList do
    prop('  payload type', gen(cc));
  for c in ep.payloadMimeTypeList do
    prop('  mime-type', c.value);
  for s in ep.headerList do
    prop('  header', s.value);
end;

procedure TProviderDirectoryFrame.describeHealthcareService(hs: TFhirHealthcareService);
var
  id : TFhirIdentifier;
  cc : TFhirCodeableConcept;
  cp : TFHIRContactPoint;
  s : TFHIRString;
  c : TFhirEnum;
  at : TFhirHealthcareServiceAvailableTime;
  {$IFNDEF FHIR3}
  el: TFhirHealthcareServiceEligibility;   //JCT: Added
  {$ENDIF}
  na : TFhirHealthcareServiceNotAvailable;
  ref : TFHIRReference;
begin
  prop('id', hs.id, true);
  prop('full url', hs.Tags['fullUrl']);
  prop('source', hs.getExtensionString('http://hl7.org/fhir/StructureDefinition/extension-Meta.source|3.2'));
  for id in hs.identifierList do
    if id.system = HPIO_SYSTEM then
      prop('hpi-o', id.value)
    else if id.hasType('VDI') then
      prop('vendor-id', id.value)
    else
      prop('identifier', gen(id));
  if hs.activeElement <> nil then
    prop('active', BoolToStr(hs.active, true));
  prop('name', hs.name);
  prop('category', gen(hs.category));
  for cc in hs.type_List do
    prop('type', gen(cc));
  for cc in hs.specialtyList do
    prop('specialty', gen(cc));
  prop('comment', hs.comment);
  prop('details', hs.extraDetails);
  for cp in hs.telecomList do
    prop('contact', gen(cp));
  for cc in hs.serviceProvisionCodeList do
    prop('service provision', gen(cc));
  {$IFNDEF FHIR3}
// JCT:Edited
  for el in hs.eligibilityList do
  begin
    prop('Eligibility', '', true);
    prop('  .code', gen(el.code));
    prop('  .comment', el.comment);
  end;
  for cc in hs.program_List do
    prop('program', s.value);
// JCT
 {$ENDIF}
  for cc in hs.characteristicList do
    prop('characteristic', gen(cc));
  for cc in hs.referralMethodList do
    prop('referral method', gen(cc));
  if hs.appointmentRequiredElement <> nil then
    prop('appointment required?', BoolToStr(hs.appointmentRequired, true));
  for at in hs.availableTimeList do
  begin
    prop('Availibility Time', '', true);
    for c in at.daysOfWeekList do
      prop('  .day', c.value);
    if at.allDayElement <> nil then
      prop('  .All Day?', BoolToStr(at.allDay, true));
    prop('  .open', at.availableStartTime);
    prop('  .close', at.availableEndTime);
  end;
  for na in hs.notAvailableList do
  begin
    prop('Not Available', '', true);
    prop('  .during', na.description);
    prop('  .period', gen(na.during));
  end;
  if (hs.providedBy <> nil) and (hs.providedBy.TagObject <> nil) then
    describeOrganization('Organization', hs.providedBy.TagObject as TFhirOrganization);
  for ref in hs.locationList do
    if ref.tagObject <> nil then
      describeLocation('location', ref.TagObject as TFhirLocation);
  for ref in hs.endpointList do
    if ref.tagObject <> nil then
      describeEndPoint('endpoint', ref.TagObject as TFhirEndPoint);
end;

procedure TProviderDirectoryFrame.describeLocation(name: String; loc: TFhirLocation);
var
  cc : TFhirCodeableConcept;
  s : TFHIRString;
  cp : TFHIRContactPoint;
begin
  prop(name, '', true);
  prop('  full url', loc.Tags['fullUrl']);
  prop('  source', loc.getExtensionString('http://hl7.org/fhir/StructureDefinition/extension-Meta.source|3.2'));
  prop('  name', loc.name);
  for s in loc.aliasList do
    prop('  alias', s.value);
  prop('  description', loc.description);
  prop('  mode', CODES_TFhirLocationModeEnum[loc.mode]);
  prop('  type', gen(loc.type_));
  for cp in loc.telecomList do
    prop('  contact', gen(cp));
  prop('  address', gen(loc.address));
  prop('  physical type', gen(loc.physicalType));
  if loc.position <> nil then
  begin
    prop('  location.long', loc.position.longitude, true);
    prop('  location.latt', loc.position.latitude, true);
    prop('  location.alt', loc.position.altitude, true);
  end;
end;


procedure TProviderDirectoryFrame.describeOrganization(name: String; org: TFhirOrganization);
var
  id : TFhirIdentifier;
  cc : TFhirCodeableConcept;
  s : TFHIRString;
  cp : TFHIRContactPoint;
  ad : TFhirAddress;
  ct : TFhirOrganizationContact;
begin
  prop(name, '', true);
  prop('  source', org.getExtensionString('http://hl7.org/fhir/StructureDefinition/extension-Meta.source|3.2'));
  prop('  full url', org.Tags['fullUrl']);
  if org.activeElement <> nil then
    prop('  active', BoolToStr(org.active, true));
  prop('  name', org.name);
  for id in org.identifierList do
    if id.system = HPIO_SYSTEM then
      prop('  hpi-o', id.value)
    else if id.system = 'http://business.gov.au/abn' then
      prop('  abn', id.value)
    else if id.system = 'http://business.gov.au/acn' then
      prop('  acn', id.value)
    else if id.system = 'http://business.gov.au/arbn' then
      prop('  arbn', id.value)
    else if id.system = 'http://ns.electronichealth.net.au/id/pcehr/paio/1.0' then
      prop('  pai-o', id.value)
    else if id.system = 'http://ns.electronichealth.net.au/id/hi/csp/1.0' then
      prop('  csp', id.value)
    else if id.hasType('VDI') then
      prop('  vendor-id', id.value)
    else
      prop('  identifier', gen(id));
  for cc in org.type_List do
    prop('  type', gen(cc));
  for s in org.aliasList do
    prop('  alias', s.value);
  for cp in org.telecomList do
    prop('  contact', gen(cp));
  for ad in org.addressList do
    prop('  address', gen(ad));
  for ct in org.contactList do
  begin
    prop('  contact', '', true);
    prop('    purpose', gen(ct.purpose));
    prop('    name', gen(ct.name));
    for cp in ct.telecomList do
       prop('    contact', gen(cp));
    prop('    address', gen(ct.address));
  end;
end;

procedure TProviderDirectoryFrame.describePractitioner(name : String; pr: TFhirPractitioner);
var
  id : TFhirIdentifier;
  hn : TFhirHumanName;
  cp : TFHIRContactPoint;
  ad : TFhirAddress;
  q : TFhirPractitionerQualification;
  cc : TFhirCodeableConcept;
begin
  prop(name, '', true);
  prop('  full url', pr.Tags['fullUrl']);
  prop('  source', pr.getExtensionString('http://hl7.org/fhir/StructureDefinition/extension-Meta.source|3.2'));
  if pr.activeElement <> nil then
    prop('  active', BoolToStr(pr.active, true));
  for hn in pr.nameList do
    prop('  name', gen(hn));
  for id in pr.identifierList do
    if id.system = 'http://ns.electronichealth.net.au/id/hi/hpii/1.0' then
      prop('hpi-i', id.value)
    else if id.system = 'http://ns.electronichealth.net.au/id/prescriber-number' then
      prop('prescriber#', id.value)
    else if id.hasType('AHPRA') then
      prop('AHPRA ID', id.value)
    else
      prop('identifier', gen(id));
  for cp in pr.telecomList do
    prop('  contact', gen(cp));
  for ad in pr.addressList do
    prop('  address', gen(ad));
  prop('  gender', CODES_TFhirAdministrativeGenderEnum[pr.gender]);
  prop('  d-o-b', pr.birthDate.toXML);
  for q in pr.qualificationList do
  begin
    prop('  qualification', '', true);
    for id in q.identifierList do
      prop('    identifier', gen(id));
    prop('    code', gen(q.code));
    prop('    period', gen(q.period));
  end;
  for cc in pr.communicationList do
    prop('  communication', gen(cc));
end;

procedure TProviderDirectoryFrame.describePractitionerRole(pr: TFhirPractitionerRole);
var
  id : TFhirIdentifier;
  cc : TFhirCodeableConcept;
  cp : TFHIRContactPoint;
  s : TFHIRString;
  c : TFhirEnum;
  at : TFhirPractitionerRoleAvailableTime;
  na : TFhirPractitionerRoleNotAvailable;
  ref : TFHIRReference;
begin
  prop('id', pr.id, true);
  prop('source', pr.getExtensionString('http://hl7.org/fhir/StructureDefinition/extension-Meta.source|3.2'));
  prop('full url', pr.Tags['fullUrl']);
  for id in pr.identifierList do
    if id.system = 'http://ns.electronichealth.net.au/id/medicare-provider-number' then
      prop('provider #', id.value)
    else if id.system = 'http://ns.electronichealth.net.au/id/npio' then
      prop('np/oi', id.value)
    else if id.system = 'http://ns.electronichealth.net.au/id/pcehr/caei/1.0' then
      prop('caei', id.value)
    else if id.hasType('EI') then
      prop('Employer ID', id.value)
    else if id.hasType('VDI') then
      prop('Vendor ID', id.value)
    else
      prop('identifier', gen(id));
  if pr.activeElement <> nil then
    prop('active', BoolToStr(pr.active, true));
  prop('period', gen(pr.period));
  for cc in pr.codeList do
    prop('code', gen(cc));
  for cc in pr.specialtyList do
    prop('specialty', gen(cc));
  for cp in pr.telecomList do
    prop('contact', gen(cp));
  for at in pr.availableTimeList do
  begin
    prop('Availibility Time', '', true);
    for c in at.daysOfWeekList do
      prop('  .day', c.value);
    if at.allDayElement <> nil then
      prop('  .All Day?', BoolToStr(at.allDay, true));
    prop('  .open', at.availableStartTime);
    prop('  .close', at.availableEndTime);
  end;
  for na in pr.notAvailableList do
  begin
    prop('Not Available', '', true);
    prop('  .during', na.description);
    prop('  .period', gen(na.during));
  end;
  if (pr.practitioner <> nil) and (pr.practitioner.TagObject <> nil) then
    describePractitioner('Practitioner', pr.practitioner.TagObject as TFhirPractitioner);
  if (pr.organization <> nil) and (pr.organization.TagObject <> nil) then
    describeOrganization('Organization', pr.organization.TagObject as TFhirOrganization);
  for ref in pr.locationList do
    if ref.tagObject <> nil then
      describeLocation('location', ref.TagObject as TFhirLocation);
  for ref in pr.endpointList do
    if ref.tagObject <> nil then
      describeEndPoint('endpoint', ref.TagObject as TFhirEndPoint);
end;

destructor TProviderDirectoryFrame.Destroy;
begin
  FClient.free;
  FHSBundle.Free;
  FHSMatches.Free;
  FPrBundle.Free;
  FPrMatches.Free;
  FOtherResources.Free;
  FRows.Free;

  inherited;
end;

procedure TProviderDirectoryFrame.DoWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  Application.ProcessMessages;
  if assigned(OnStopped) and OnStopped(nil) then
    abort;
end;

procedure TProviderDirectoryFrame.gridHSMatchesGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  hs : TFhirHealthcareService;
  cs : TCommaBuilder;
  ref : TFhirReference;
  ad : TFhirAddress;
  cp : TFhirContactPoint;
  cc: TFhirCodeableConcept;
begin
  hs := FHSMatches[aRow] as TFhirHealthcareService;
  for ref in hs.endpointList do
    if ref.tagObject = nil then
      ref.TagObject := resolve(ref);
  if hs.providedBy <> nil then
    if hs.providedBy.TagObject = nil then
      hs.providedBy.TagObject := resolve(hs.providedBy);
  for ref in hs.locationList do
    if ref.tagObject = nil then
      ref.TagObject := resolve(ref);

  cs := TCommaBuilder.create;
  try
    cs.ignoreDuplicates := true;
    case ACol of
      0: {name}
        cs.add(hs.name);
      1: {address}
        begin
          for ref in hs.locationList do
            if ref.TagObject <> nil then
              cs.add(gen((ref.TagObject as TFhirLocation).address));
          if hs.providedBy <> nil then
            if hs.providedBy.TagObject <> nil then
              for ad in (hs.providedBy.TagObject as TFhirOrganization).addressList do
                cs.add(gen(ad));
        end;
      2: {telephone / fax}
        begin
          for cp in hs.telecomList do
            if cp.isPhoneOrFax then
              cs.add(gen(cp));
          for ref in hs.locationList do
            if ref.TagObject <> nil then
              for cp in (ref.TagObject as TFhirLocation).telecomList do
                if cp.isPhoneOrFax then
                  cs.add(gen(cp));
          if hs.providedBy <> nil then
            if hs.providedBy.TagObject <> nil then
              for cp in (hs.providedBy.TagObject as TFhirOrganization).telecomList do
                if cp.isPhoneOrFax then
                  cs.add(gen(cp));
        end;
      3: {email}
        begin
          for cp in hs.telecomList do
            if cp.isEmail then
              cs.add(gen(cp));
          for ref in hs.locationList do
            if ref.TagObject <> nil then
              for cp in (ref.TagObject as TFhirLocation).telecomList do
                if cp.isEmail then
                  cs.add(gen(cp));
          if hs.providedBy <> nil then
            if hs.providedBy.TagObject <> nil then
              for cp in (hs.providedBy.TagObject as TFhirOrganization).telecomList do
                if cp.isEmail then
                  cs.add(gen(cp));
        end;
      4: {org name}
        if hs.providedBy <> nil then
          if hs.providedBy.TagObject <> nil then
            cs.add((hs.providedBy.TagObject as TFhirOrganization).name);
      5: {specialties}
        for cc in hs.specialtyList do
          cs.add(gen(cc));
      6: {Connection Types}
          for ref in hs.endpointList do
            if ref.TagObject <> nil then
              cs.add(gen((ref.TagObject as TFhirEndpoint).connectionType));
      7: {payload types}
          for ref in hs.endpointList do
            if ref.TagObject <> nil then
              for cc in (ref.TagObject as TFhirEndpoint).payloadTypeList do
                cs.add(gen(cc));
    end;
    Value := cs.asString;
  finally
    cs.free;
  end;
end;

function top200(s : String) : String;
var
  i, t : integer;
begin
  i := 1;
  t := 0;
  while (i < length(s)) do
  begin
    if s[i] = #10 then
      inc(t);
    if t = 200 then
      exit(s.Substring(0, i+1));
    inc(i);
  end;
  result := s;
end;

procedure TProviderDirectoryFrame.gridHSMatchesSelectCell(Sender: TObject; const ACol, ARow: Integer; var CanSelect: Boolean);
var
  res : TFhirHealthcareService;
begin
  FRows.Clear;
  gridProperties.RowCount := 0;
  if aRow > -1 then
  begin
    res := FHSMatches[aRow];
    describeHealthcareService(res);
  end;
  gridProperties.RowCount := FRows.Count;
end;

procedure TProviderDirectoryFrame.gridPRMatchesGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  pr : TFhirPractitionerRole;
  cs : TCommaBuilder;
  ref : TFhirReference;
  ad : TFhirAddress;
  hn : TFhirHumanName;
  cp : TFhirContactPoint;
  cc : TFhirCodeableConcept;
begin
  pr := FPrMatches[aRow] as TFhirPractitionerRole;
  for ref in pr.endpointList do
    if ref.tagObject = nil then
      ref.TagObject := resolve(ref);
  if (pr.organization <> nil) and (pr.organization.TagObject = nil) then
    pr.organization.TagObject := resolve(pr.organization);
  if pr.practitioner <> nil then
    if pr.practitioner.TagObject = nil then
      pr.practitioner.TagObject := resolve(pr.practitioner);
  for ref in pr.locationList do
    if ref.tagObject = nil then
      ref.TagObject := resolve(ref);

  cs := TCommaBuilder.create;
  try
    cs.ignoreDuplicates := true;
    case ACol of
      0: {name}
        if (pr.practitioner <> nil) and (pr.practitioner.TagObject <> nil) then
          for hn in (pr.practitioner.TagObject as TFhirPractitioner).nameList do
            cs.add(gen(hn));
      2: {address}
        begin
          if pr.practitioner <> nil then
            if pr.practitioner.TagObject <> nil then
              for ad in (pr.practitioner.TagObject as TFhirPractitioner).addressList do
                cs.add(gen(ad));
          for ref in pr.locationList do
            if ref.TagObject <> nil then
              cs.add(gen((ref.TagObject as TFhirLocation).address));
          if pr.organization <> nil then
            if pr.organization.TagObject <> nil then
              for ad in (pr.organization.TagObject as TFhirOrganization).addressList do
                cs.add(gen(ad));
        end;
      3: {telephone / fax}
        begin
          for cp in pr.telecomList do
            if cp.isPhoneOrFax then
              cs.add(gen(cp));
          if pr.practitioner <> nil then
            if pr.practitioner.TagObject <> nil then
              for cp in (pr.practitioner.TagObject as TFhirPractitioner).telecomList do
                if cp.isPhoneOrFax then
                  cs.add(gen(cp));
          for ref in pr.locationList do
            if ref.TagObject <> nil then
              for cp in (ref.TagObject as TFhirLocation).telecomList do
                if cp.isPhoneOrFax then
                  cs.add(gen(cp));
          if pr.organization <> nil then
            if pr.organization.TagObject <> nil then
              for cp in (pr.organization.TagObject as TFhirOrganization).telecomList do
                if cp.isPhoneOrFax then
                  cs.add(gen(cp));
        end;
      4: {email}
        begin
          for cp in pr.telecomList do
            if cp.isEmail then
              cs.add(gen(cp));
          if pr.practitioner <> nil then
            if pr.practitioner.TagObject <> nil then
              for cp in (pr.practitioner.TagObject as TFhirPractitioner).telecomList do
                if cp.isEmail then
                  cs.add(gen(cp));
          for ref in pr.locationList do
            if ref.TagObject <> nil then
              for cp in (ref.TagObject as TFhirLocation).telecomList do
                if cp.isEmail then
                  cs.add(gen(cp));
          if pr.organization <> nil then
            if pr.organization.TagObject <> nil then
              for cp in (pr.organization.TagObject as TFhirOrganization).telecomList do
                if cp.isEmail then
                  cs.add(gen(cp));
        end;
      5: {org name}
        if pr.organization <> nil then
          if pr.organization.TagObject <> nil then
            cs.add((pr.organization.TagObject as TFhirOrganization).name);
      6: {specialties}
        for cc in pr.specialtyList do
          cs.add(gen(cc));
      7: {Connection Types}
          for ref in pr.endpointList do
            if ref.TagObject <> nil then
              cs.add(gen((ref.TagObject as TFhirEndpoint).connectionType));
      8: {payload types}
          for ref in pr.endpointList do
            if ref.TagObject <> nil then
              for cc in (ref.TagObject as TFhirEndpoint).payloadTypeList do
                cs.add(gen(cc));
    end;
    Value := cs.asString;
  finally
    cs.free;
  end;

end;

procedure TProviderDirectoryFrame.gridPRMatchesSelectCell(Sender: TObject; const ACol, ARow: Integer; var CanSelect: Boolean);
var
  res : TFhirPractitionerRole;
begin
  FRows.Clear;
  if aRow > -1 then
  begin
    res := FPRMatches[aRow];
    describePractitionerRole(res);
  end;
  gridProperties.RowCount := FRows.Count;
end;

procedure TProviderDirectoryFrame.gridPropertiesGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
begin
  case ACol of
    0 : value := FRows.Names[aRow];
    1 : value := FRows.ValueFromIndex[aRow];
  end;
end;

procedure TProviderDirectoryFrame.lblMessagesClick(Sender: TObject);
//var
//  Svc: IFMXClipboardService;
begin
//  TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc);
//  svc.SetClipboard(lblMessages.Text);
end;

procedure TProviderDirectoryFrame.lblOutcomeClick(Sender: TObject);
var
  Svc: IFMXClipboardService;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc);
  svc.SetClipboard(lblOutcome.Text);
end;

procedure TProviderDirectoryFrame.load;
begin
  cbxState.ValueSet := fileToResource('au-states.xml') as TFhirValueSet;
  cbxType.ValueSet := fileToResource('ep-connection-types.xml') as TFhirValueSet;
  cbxPayload.ValueSet := fileToResource('ep-payload-types.xml') as TFhirValueSet;

  edtServiceName.Text := Settings.getValue('Provider-Directory-search', 'ServiceName', '');
  edtOrgName.Text := Settings.getValue('Provider-Directory-search', 'OrganisationName', '');
  edtFamily.Text := Settings.getValue('Provider-Directory-search', 'FamilyName', '');
  edtGiven.Text := Settings.getValue('Provider-Directory-search', 'GivenName', '');
  edtHPIO.Text := Settings.getValue('Provider-Directory-search', 'HPIO', '');
  edtIdentifier.Text := Settings.getValue('Provider-Directory-search', 'Identifier', '');
  cbxIdType.ItemIndex := Settings.getValue('Provider-Directory-search', 'IdType', 0);
  edtSuburb.Text := Settings.getValue('Provider-Directory-search', 'Suburb', '');;
  edtPostcode.Text := Settings.getValue('Provider-Directory-search', 'Postcode', '');;
  cbxState.ItemIndex := Settings.getValue('Provider-Directory-search', 'State', -1);
  edtSpecialty.Text := Settings.getValue('Provider-Directory-search', 'Specialty', '');
  cbxType.ItemIndex := Settings.getValue('Provider-Directory-search', 'Type', -1);
  cbxPayload.ItemIndex := Settings.getValue('Provider-Directory-search', 'Category', -1);

  btnFetchMore.Visible := false;
  FHSMatches := TFslList<TFhirHealthcareService>.create;
  FPrMatches := TFslList<TFhirPractitionerRole>.create;
  FOtherResources := TFslMap<TFHIRResource>.create('other resources');
  FRows := TStringList.Create;
end;


procedure TProviderDirectoryFrame.prop(n, v: String; ifBlank: boolean);
begin
  if ifBlank or (v <>'') then
    FRows.AddPair(n, v);
end;

function TProviderDirectoryFrame.resolve(ref: TFHIRReference): TFHIRResource;
begin
  if not FOtherResources.TryGetValue(ref.reference, result) then
    result := nil;
end;

procedure TProviderDirectoryFrame.SetClient(const Value: TFHIRClient);
begin
  FClient.Free;
  FClient := Value;
end;

end.
