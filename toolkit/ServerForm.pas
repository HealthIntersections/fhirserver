unit ServerForm;

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
  fsl_base, fsl_utilities, FHIR.Ui.Fmx,
  FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Resources.Base, FHIR.Version.Client, FHIR.Version.Utilities,
  BaseFrame, CapabilityStatementEditor, VitalSignsGeneratorDialog,
  ProviderDirectoryForm, PatientHomeForm, BulkDataForm;

type
  TFrame = TBaseFrame; // re-aliasing the Frame to work around a designer bug

  TServerFrame = class (TFrame)
    Panel1: TPanel;
    Label1: TLabel;
    Button1: TButton;
    pnlSearch: TPanel;
    Splitter1: TSplitter;
    Label2: TLabel;
    cbxSearchType: TComboBox;
    tabSearch: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Panel2: TPanel;
    btnClose: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    edtConfUrl: TEdit;
    edtConfId: TEdit;
    edtConfVersion: TEdit;
    edtConfName: TEdit;
    edtConfTitle: TEdit;
    edtConfText: TEdit;
    dedConfDate: TDateEdit;
    edtConfJurisdiction: TComboBox;
    edtConfPub: TEdit;
    cbxConfStatus: TComboBox;
    edtConfUpdated: TDateEdit;
    edtConfTag: TEdit;
    btnConfSearch: TButton;
    gridConfMatches: TGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    StringColumn7: TStringColumn;
    StringColumn8: TStringColumn;
    Panel3: TPanel;
    lblOutcome: TLabel;
    Label17: TLabel;
    edtPText: TEdit;
    Label18: TLabel;
    dedPLastEdit: TDateEdit;
    Label19: TLabel;
    Label23: TLabel;
    edtPTag: TEdit;
    btnSearchPatients: TButton;
    Label15: TLabel;
    Label16: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    edtPName: TEdit;
    edtPTelecom: TEdit;
    cbxPDob: TComboBox;
    dedPDob: TDateEdit;
    cbxPGender: TComboBox;
    cbxPDeceased: TComboBox;
    dedPDeceased: TDateEdit;
    cbxPActive: TComboBox;
    edtPIdentifier: TEdit;
    cbxPSpecies: TComboBox;
    cbxPLanguage: TComboBox;
    cbPUseLastUpdated: TCheckBox;
    cbConfUseLastUpdated: TCheckBox;
    gridPMatches: TGrid;
    StringColumn9: TStringColumn;
    StringColumn10: TStringColumn;
    StringColumn11: TStringColumn;
    StringColumn12: TStringColumn;
    StringColumn13: TStringColumn;
    StringColumn14: TStringColumn;
    StringColumn15: TStringColumn;
    StringColumn16: TStringColumn;
    StringColumn17: TStringColumn;
    tabMatches: TTabControl;
    TabItem4: TTabItem;
    TabItem5: TTabItem;
    TabItem6: TTabItem;
    btnFetchMore: TButton;
    Button2: TButton;
    Button3: TButton;
    btnBulkData: TButton;
    btnMatch: TButton;
    Label27: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnConfSearchClick(Sender: TObject);
    procedure gridConfMatchesGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridConfMatchesCellDblClick(const Column: TColumn; const Row: Integer);
    procedure cbxSearchTypeChange(Sender: TObject);
    procedure cbxPDobChange(Sender: TObject);
    procedure cbxPDeceasedChange(Sender: TObject);
    procedure cbConfUseLastUpdatedChange(Sender: TObject);
    procedure cbPUseLastUpdatedChange(Sender: TObject);
    procedure btnSearchPatientsClick(Sender: TObject);
    procedure gridPMatchesGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure btnFetchMoreClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure gridPMatchesCellDblClick(const Column: TColumn; const Row: Integer);
    procedure btnBulkDataClick(Sender: TObject);
    procedure btnMatchClick(Sender: TObject);
  private
    FClient: TFHIRClient;
    FCapabilityStatement: TFhirCapabilityStatement;
    FCSTab : TTabItem;
    FPDTab : TTabItem;
    FCsForm : TCapabilityStatementEditorFrame;
    FPdForm : TProviderDirectoryFrame;
    FPatBundle, FConfBundle : TFhirBundle;
    FConfMatches : TFslList<TFHIRResource>;
    FPatMatches : TFslList<TFHIRPatient>;
    procedure SetClient(const Value: TFHIRClient);
    procedure SetCapabilityStatement(const Value: TFhirCapabilityStatement);
    procedure DoWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure openPatient(pat : TFHIRPatient);
  public
    destructor Destroy; override;
    property Client : TFHIRClient read FClient write SetClient;
    property CapabilityStatement : TFhirCapabilityStatement read FCapabilityStatement write SetCapabilityStatement;

    procedure load; override;
  end;

implementation

{$R *.fmx}

{ TServerFrame }

procedure TServerFrame.btnBulkDataClick(Sender: TObject);
var
  form : TBulkDataDialog;
begin
  form := TBulkDataDialog.create(self.form);
  try
    form.server := client.link;
    form.settings := settings.link;
    showmodalHack(form);
  finally
    form.free;
  end;
end;

procedure TServerFrame.btnCloseClick(Sender: TObject);
begin
  try
    Settings.storeValue('Conformance-search', 'url', edtConfUrl.Text);
    Settings.storeValue('Conformance-search', 'id', edtConfId.Text);
    Settings.storeValue('Conformance-search', 'version', edtConfVersion.Text);
    Settings.storeValue('Conformance-search', 'name', edtConfName.Text);
    Settings.storeValue('Conformance-search', 'title', edtConfTitle.Text);
    Settings.storeValue('Conformance-search', 'text', edtConfText.Text);
    Settings.storeValue('Conformance-search', 'date', dedConfDate.Text);
    Settings.storeValue('Conformance-search', 'jurisdiction', edtConfJurisdiction.ItemIndex);
    Settings.storeValue('Conformance-search', 'publisher', edtConfPub.Text);
    Settings.storeValue('Conformance-search', 'status', cbxConfStatus.ItemIndex);
    Settings.storeValue('Conformance-search', 'updated', edtConfUpdated.Text);
    Settings.storeValue('Conformance-search', 'tag', edtConfTag.Text);
    Settings.storeValue('Conformance-search', 'updated-opt', cbConfUseLastUpdated.IsChecked);

    Settings.storeValue('Patient-search', 'text', edtPText.Text);
    Settings.storeValue('Patient-search', 'updated', dedPLastEdit.Text);
    Settings.storeValue('Patient-search', 'tag', edtPTag.Text);
    Settings.storeValue('Patient-search', 'name', edtPName.Text);
    Settings.storeValue('Patient-search', 'telecom', edtPTelecom.Text);
    Settings.storeValue('Patient-search', 'dobopt', cbxPDob.ItemIndex);
    Settings.storeValue('Patient-search', 'dob', dedPDob.Text);
    Settings.storeValue('Patient-search', 'gender', cbxPGender.ItemIndex);
    Settings.storeValue('Patient-search', 'dod', cbxPDeceased.ItemIndex);
    Settings.storeValue('Patient-search', 'dodopt', dedPDeceased.Text);
    Settings.storeValue('Patient-search', 'active', cbxPActive.ItemIndex);
    Settings.storeValue('Patient-search', 'id', edtPIdentifier.Text);
    Settings.storeValue('Patient-search', 'species', cbxPSpecies.ItemIndex);
    Settings.storeValue('Patient-search', 'lang', cbxPLanguage.ItemIndex);
    Settings.storeValue('Patient-search', 'updated-opt', cbPUseLastUpdated.IsChecked);
    Settings.save;
  except
  end;
  Close;
end;

procedure TServerFrame.Button1Click(Sender: TObject);
begin
  if FCSTab <> nil then
  begin
    FcsForm.Load;
    Tabs.ActiveTab := FCSTab;
  end
  else
  begin
    FCSTab := Tabs.Add(TTabItem);
    Tabs.ActiveTab := FCSTab;
    FCSTab.Text := 'Capability Statement for '+FClient.address;
    FcsForm := TCapabilityStatementEditorFrame.create(tab);
    FCSTab.TagObject := FCsForm;
    FCsForm.TagObject := FCSTab;
    FcsForm.Parent := FCSTab;
    FcsForm.Tabs := tabs;
    FcsForm.OnWork := onwork;
    FcsForm.Settings := Settings.link;
    FcsForm.tab := FCSTab;
    FcsForm.Align := TAlignLayout.Client;
    FcsForm.Client := client.link;
    FcsForm.Resource := CapabilityStatement.Link;
    FcsForm.Filename := '$$';
    FcsForm.Load;
  end;
end;

procedure TServerFrame.Button2Click(Sender: TObject);
var
  form : TVitalSignsGeneratorForm;
begin
  form := TVitalSignsGeneratorForm.create(self);
  try
    form.Client := FClient.Link;
    form.Settings := Settings.Link;
    ShowModalHack(form);
  finally
    form.free;
  end;
end;

procedure TServerFrame.Button3Click(Sender: TObject);
begin
  if FPDTab <> nil then
  begin
    FPDForm.Load;
    Tabs.ActiveTab := FPDTab;
  end
  else
  begin
    FPDTab := Tabs.Add(TTabItem);
    Tabs.ActiveTab := FPDTab;
    FPDTab.Text := 'Provider Directory on '+FClient.address;
    FpdForm := TProviderDirectoryFrame.create(tab);
    FPDTab.TagObject := FPDForm;
    FPDForm.TagObject := FPDTab;
    FPDForm.Parent := FPDTab;
    FPDForm.Tabs := tabs;
    FPDForm.OnWork := onwork;
    FPDForm.Settings := Settings.link;
    FPDForm.tab := FPDTab;
    FPDForm.Align := TAlignLayout.Client;
    FPDForm.Client := client.link;
    FPDForm.Load;
  end;
end;

procedure TServerFrame.cbConfUseLastUpdatedChange(Sender: TObject);
begin
  dedConfDate.Enabled := cbConfUseLastUpdated.IsChecked;
end;

procedure TServerFrame.cbPUseLastUpdatedChange(Sender: TObject);
begin
  dedPLastEdit.Enabled := cbPUseLastUpdated.IsChecked;
end;

procedure TServerFrame.cbxPDeceasedChange(Sender: TObject);
begin
  dedPDeceased.Enabled := cbxPDeceased.ItemIndex > 2;
end;

procedure TServerFrame.cbxPDobChange(Sender: TObject);
begin
  dedPDob.Enabled := cbxPDob.ItemIndex > 0;
end;

procedure TServerFrame.cbxSearchTypeChange(Sender: TObject);
begin
  tabSearch.TabIndex := cbxSearchType.ItemIndex;
  tabMatches.TabIndex := cbxSearchType.ItemIndex;
end;

function getJurisdictionSearch(i: integer): string;
begin
  case i of
    1:result := 'urn:iso:std:iso:3166|AT';
    2:result := 'urn:iso:std:iso:3166|AU';
    3:result := 'urn:iso:std:iso:3166|BR';
    4:result := 'urn:iso:std:iso:3166|CA';
    5:result := 'urn:iso:std:iso:3166|CH';
    6:result := 'urn:iso:std:iso:3166|CL';
    7:result := 'urn:iso:std:iso:3166|CN';
    8:result := 'urn:iso:std:iso:3166|DE';
    9:result := 'urn:iso:std:iso:3166|DK';
    10:result := 'urn:iso:std:iso:3166|EE';
    11:result := 'urn:iso:std:iso:3166|ES';
    12:result := 'urn:iso:std:iso:3166|FI';
    13:result := 'urn:iso:std:iso:3166|FR';
    14:result := 'urn:iso:std:iso:3166|GB';
    15:result := 'urn:iso:std:iso:3166|NL';
    16:result := 'urn:iso:std:iso:3166|NO';
    17:result := 'urn:iso:std:iso:3166|NZ';
    18:result := 'urn:iso:std:iso:3166|RU';
    19:result := 'urn:iso:std:iso:3166|US';
    21:result := 'urn:iso:std:iso:3166|VN';
    22:result := 'http://unstats.un.org/unsd/methods/m49/m49.htm|001';
    23:result := 'http://unstats.un.org/unsd/methods/m49/m49.htm|002';
    24:result := 'http://unstats.un.org/unsd/methods/m49/m49.htm|019';
    25:result := 'http://unstats.un.org/unsd/methods/m49/m49.htm|142';
    26:result := 'http://unstats.un.org/unsd/methods/m49/m49.htm|150';
    27:result := 'http://unstats.un.org/unsd/methods/m49/m49.htm|053';
  else
    result := '';
  end;
end;

function readJurisdiction(res : TFhirMetadataResource): String;
var
  cc : TFhirCodeableConcept;
  c : TFhirCoding;
begin
  result := '';
  for cc in res.jurisdictionList do
    for c in cc.codingList do
    begin
      if c.system = 'urn:iso:std:iso:3166' then
      begin
        if c.code = 'AT' then exit('Austraia');
        if c.code = 'AU' then exit('Australia');
        if c.code = 'BR' then exit('Brazil');
        if c.code = 'CA' then exit('Canada');
        if c.code = 'CH' then exit('Switzerland');
        if c.code = 'CL' then exit('Chile');
        if c.code = 'CN' then exit('China');
        if c.code = 'DE' then exit('Germany');
        if c.code = 'DK' then exit('Denmark');
        if c.code = 'EE' then exit('Estonia');
        if c.code = 'ES' then exit('Spain');
        if c.code = 'FI' then exit('Finland');
        if c.code = 'FR' then exit('France');
        if c.code = 'GB' then exit('UK');
        if c.code = 'NL' then exit('Netherlands');
        if c.code = 'NO' then exit('Norway');
        if c.code = 'NZ' then exit('NZ');
        if c.code = 'RU' then exit('Russia');
        if c.code = 'US' then exit('USA');
        if c.code = 'VN' then exit('Vietnam');
      end
      else if c.system = 'http://unstats.un.org/unsd/methods/m49/m49.htm' then
      begin
        if c.code = '001' { World } then exit('World');
        if c.code = '002' { Africa } then exit('Africa');
        if c.code = '019' { Americas } then exit('Americas');
        if c.code = '142' { Asia } then exit('Asia');
        if c.code = '150' { Europe } then exit('Europe');
        if c.code = '053' { Australia and New Zealand } then exit('Australasia');
      end
    end;
end;




procedure TServerFrame.btnConfSearchClick(Sender: TObject);
begin
  work('Fetch Resources', true,
    procedure (context : pointer)
    var
      be : TFhirBundleEntry;
      params : TStringList;
      start : TDateTime;
    begin
      FConfMatches.Clear;
      gridConfMatches.RowCount := FConfMatches.Count;
      FConfBundle.Free;
      FConfBundle := nil;

      params := TStringList.create;
      try
        params.AddPair('_type', 'CapabilityStatement,StructureDefinition,ImplementationGuide,SearchParameter,MessageDefinition,OperationDefinition,CompartmentDefinition,StructureMap,GraphDefinition,CodeSystem,ValueSet,ConceptMap,NamingSystem');
        params.AddPair('_summary', 'true');

        if edtConfUrl.Text <> '' then
          params.addPair('url', edtConfUrl.Text);
        if edtConfId.Text <> '' then
          params.addPair('identifier', edtConfId.Text);
        if edtConfVersion.Text <> '' then
          params.addPair('version', edtConfVersion.Text);
        if edtConfName.Text <> '' then
          params.addPair('name', edtConfName.Text);
        if edtConfTitle.Text <> '' then
          params.addPair('title', edtConfTitle.Text);
        if edtConfText.Text <> '' then
          params.addPair('_text', edtConfText.Text);
        if dedConfDate.Text <> '' then
          params.addPair('date', dedConfDate.Text);
        if edtConfJurisdiction.ItemIndex <> -1 then
          params.addPair('jurisdiction', getJurisdictionSearch(edtConfJurisdiction.ItemIndex));
        if edtConfPub.Text <> '' then
          params.addPair('publisher', edtConfPub.Text);
        if cbxConfStatus.ItemIndex <> -1 then
          params.addPair('status', cbxConfStatus.Items[cbxConfStatus.ItemIndex]);
        if cbConfUseLastUpdated.IsChecked then
          params.addPair('_lastUpdated', edtConfUpdated.Text);
        if edtConfTag.Text <> '' then
          params.addPair('_tag', edtConfTag.Text);

        start := now;
        FConfBundle := FClient.search(false, params);
        for be in FConfBundle.entryList do
          if ((be.search = nil) or (be.search.mode = SearchEntryModeMatch)) and (be.resource <> nil) then
            FConfMatches.Add(be.resource.Link);
        gridConfMatches.RowCount := FConfMatches.Count;
        lblOutcome.Text := 'Fetched '+inttostr(FConfMatches.Count)+' of '+FConfBundle.total+' resources in '+describePeriod(now - start);
        btnFetchMore.Visible := FConfBundle.Links['next'] <> '';
      finally
        params.Free;
      end;
    end);
end;

procedure TServerFrame.btnFetchMoreClick(Sender: TObject);
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
      case cbxSearchType.ItemIndex of
        0:
          begin
          url := FPatBundle.Links['next'];
          FPatBundle.Free;
          FPatBundle := nil;
          start := now;
          FPatBundle := FClient.searchAgain(url);
          i := 0;
          for be in FPatBundle.entryList do
            if (be.search.mode = SearchEntryModeMatch) and (be.resource <> nil) then
            begin
              FPatMatches.Add(be.resource.Link as TFHIRPatient);
              inc(i);
            end;
          gridPMatches.RowCount := FpatMatches.Count;
          lblOutcome.Text := 'Fetched '+inttostr(i)+' of '+FPatBundle.total+' patients in '+describePeriod(now - start);
          btnFetchMore.Visible := FPatBundle.Links['next'] <> '';
          end;
        1:
          begin
          url := FConfBundle.Links['next'];
          FConfBundle.Free;
          FConfBundle := nil;
          start := now;
          FConfBundle := FClient.searchAgain(url);
          i := 0;
          for be in FConfBundle.entryList do
            if (be.search.mode = SearchEntryModeMatch) and (be.resource <> nil) then
            begin
              FConfMatches.Add(be.resource.Link);
              inc(i);
            end;
          gridConfMatches.RowCount := FConfMatches.Count;
          lblOutcome.Text := 'Fetched '+inttostr(i)+' of '+FConfBundle.total+' resources in '+describePeriod(now - start);
          btnFetchMore.Visible := FConfBundle.Links['next'] <> '';
          end;
      end;
    end);
end;

procedure TServerFrame.btnMatchClick(Sender: TObject);
begin
  work('Match Patients', true,
    procedure (context : pointer)
    var
      pat : TFHIRPatient;
      params : TFhirParameters;
      be : TFhirBundleEntry;
      start : TDateTime;
    begin
      pat := TFHIRPatient.create;
      try
        if edtPName.text <> '' then
          pat.nameList.add(TFhirHumanName.FromEdit(edtPName.text));
        if (edtPTelecom.text <> '') then
          pat.telecomList.add(TFhirContactPoint.fromEdit(edtPTelecom.text));
        if dedPDob.text <> '' then
          pat.birthDate := TFslDateTime.makeLocal(dedPDob.date);
        if cbxPGender.itemIndex <> -1 then
          pat.gender := TFhirAdministrativeGenderEnum(cbxPGender.itemIndex);
        if cbxPActive.itemIndex > 0 then
          pat.active := cbxPActive.itemIndex = 1;
        if edtPIdentifier.text <> '' then
          pat.identifierList.append.value := edtPIdentifier.text;
        params := TFhirParameters.Create;
        try
          params.AddParameter('resource', pat.Link);
          start := now;
          FPatBundle := FClient.operation(frtPatient, 'match', params) as TFhirBundle;
            for be in FPatBundle.entryList do
              if ((be.search = nil) or (be.search.mode = SearchEntryModeMatch)) and (be.resource <> nil) then
                FPatMatches.Add(be.resource.Link as TFHIRPatient);
            gridPMatches.RowCount := FpatMatches.Count;
            lblOutcome.Text := 'Fetched '+inttostr(FPatMatches.Count)+' of '+FPatBundle.total+' patients in '+describePeriod(now - start);
            btnFetchMore.Visible := FPatBundle.Links['next'] <> '';
        finally
          params.Free;
        end;
      finally
        pat.free;
      end;
    end);
end;

procedure TServerFrame.btnSearchPatientsClick(Sender: TObject);
begin
  work('Search Patients', true,
    procedure  (context : pointer)
    var
      params : TStringList;
      be : TFhirBundleEntry;
      start : TDateTime;
      l : TFhirBundleLink;
    begin
      FPatMatches.Clear;
      gridPMatches.RowCount := FConfMatches.Count;
      FPatBundle.Free;
      FPatBundle := nil;

      params := TStringList.create;
      try
        params.AddPair('_summary', 'true');
        if edtPName.Text <> '' then
          params.addPair('name', edtPName.Text);
        if edtPTelecom.Text <> '' then
          params.addPair('telecom', edtPTelecom.Text);
        if edtPIdentifier.Text <> '' then
          params.addPair('identifier', edtPIdentifier.Text);

        case cbxPDob.ItemIndex of
          1: {on} params.addPair('birthdate', TFslDateTime.make(dedPDob.Date, dttzUnknown).toXML);
          2: {before} params.addPair('birthdate', 'le'+TFslDateTime.make(dedPDob.Date, dttzUnknown).toXML);
          3: {after}  params.addPair('birthdate', 'ge'+TFslDateTime.make(dedPDob.Date, dttzUnknown).toXML);
          4: {around} params.addPair('birthdate', 'ap'+TFslDateTime.make(dedPDob.Date, dttzUnknown).toXML);
        end;
        case cbxPGender.ItemIndex of
          1: { Male } params.addPair('gender', 'male');
          2: { Female } params.addPair('gender', 'female');
          3: { Other } params.addPair('gender', 'other');
          4: { Unknown } params.addPair('gender', 'unknown');
        end;
        case cbxPDeceased.ItemIndex of
          1: { Alive} params.addPair('deceased', 'false');
          2: { Deceased} params.addPair('deceased', 'true');
          3: { On} params.addPair('death-date', TFslDateTime.make(dedPDob.Date, dttzUnknown).toXML);
          4: { Before} params.addPair('death-date', 'le'+TFslDateTime.make(dedPDob.Date, dttzUnknown).toXML);
          5: { After} params.addPair('death-date', 'ge'+TFslDateTime.make(dedPDob.Date, dttzUnknown).toXML);
          6: { Around} params.addPair('death-date', 'ap'+TFslDateTime.make(dedPDob.Date, dttzUnknown).toXML);
        end;
        case cbxPActive.ItemIndex of
          1: { true } params.addPair('gender', 'true');
          2: { false } params.addPair('gender', 'false');
        end;
        case cbxPActive.ItemIndex of
          1: { human } params.addPair('animal-species:missing', 'true');
          2: { non-human } params.addPair('animal-species:missing', 'false');
        end;
        if cbxPLanguage.ItemIndex > 0 then
          params.addPair('language', cbxPLanguage.Items[cbxPLanguage.ItemIndex].Substring(0, 2));

        if cbPUseLastUpdated.IsChecked then
          params.addPair('_lastUpdated', dedPLastEdit.Text);

        if edtPTag.Text <> '' then
          params.addPair('_tag', edtPTag.Text);
        if edtPText.Text <> '' then
          params.addPair('_text', edtPText.Text);

        start := now;
        FPatBundle := FClient.search(frtPatient, false, params);
        for be in FPatBundle.entryList do
          if ((be.search = nil) or (be.search.mode = SearchEntryModeMatch)) and (be.resource <> nil) then
            FPatMatches.Add(be.resource.Link as TFHIRPatient);
        gridPMatches.RowCount := FpatMatches.Count;
        lblOutcome.Text := 'Fetched '+inttostr(FPatMatches.Count)+' of '+FPatBundle.total+' patients in '+describePeriod(now - start);
        btnFetchMore.Visible := FPatBundle.Links['next'] <> '';
      finally
        params.Free;
      end;
    end);
end;

destructor TServerFrame.Destroy;
begin
  FClient.free;
  FCapabilityStatement.free;
  FConfBundle.Free;
  FPatBundle.Free;
  FConfMatches.Free;
  FPatMatches.Free;

  inherited;
end;

procedure TServerFrame.DoWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  Application.ProcessMessages;
  if assigned(OnStopped) and OnStopped(nil) then
    abort;
end;

procedure TServerFrame.gridConfMatchesCellDblClick(const Column: TColumn; const Row: Integer);
var
  res : TFhirResource;
begin
  res := Client.readResource(FConfMatches[Row].ResourceType, FConfMatches[Row].id);
  try
    if res.ResourceType = frtPatient then
      openPatient(res as TFhirPatient)
    else
      OnOpenResource(self, client, client.format, res);
  finally
    res.Free;
  end;
end;

procedure TServerFrame.gridConfMatchesGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  res : TFhirMetadataResource;
begin
  if not (FConfMatches[aRow] is TFhirMetadataResource) then
  begin
    case ACol of
      0: Value := FConfMatches[aRow].fhirType;
      1: Value := FConfMatches[aRow].id;
    end;
  end
  else
  begin
    res := FConfMatches[aRow] as TFhirMetadataResource;
    case ACol of
      0: Value := res.fhirType;
      1: Value := res.id;
      2: Value := res.url;
      3: Value := res.name;
      4: Value := res.version;
      5: Value := CODES_TFhirPublicationStatusEnum[res.status];
      6: Value := res.date.toXML;
      7: Value := readJurisdiction(res);
    end;
  end;
end;

procedure TServerFrame.gridPMatchesCellDblClick(const Column: TColumn; const Row: Integer);
var
  res : TFhirResource;
begin
  res := Client.readResource(FPatMatches[Row].ResourceType, FPatMatches[Row].id);
  try
    openPatient(res as TFhirPatient)
  finally
    res.Free;
  end;
end;

procedure TServerFrame.gridPMatchesGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  pat : TFhirPatient;
begin
  pat := FPatMatches[ARow];
  case ACol of
    0: Value := pat.id;
    1: if pat.nameList.Count > 0 then Value := pat.nameList[0].family;
    2: if pat.nameList.Count > 0 then Value := pat.nameList[0].given;
    3: Value := CODES_TFhirAdministrativeGenderEnum[pat.gender];
    4: Value := pat.birthDate.toString('cd');
    5: if pat.activeElement <> nil then Value := BooleanToString(pat.active);
    6: if pat.deceased <> nil then if pat.deceased is TFhirBoolean then Value := BooleanToString(pat.active) else Value := (pat.deceased as TFhirDateTime).value.toString('c');
    7: {$IFDEF FHIR4}Value := 'n/a' {$ELSE}if pat.animal = nil then Value := '(human)' else Value := gen(pat.animal.species){$ENDIF};
    8: Value := pat.identifierList.withCommas;
  end;
end;

procedure TServerFrame.load;
begin
  edtConfUrl.Text := Settings.getValue('Conformance-search', 'url', '');
  edtConfId.Text := Settings.getValue('Conformance-search', 'id', '');
  edtConfVersion.Text := Settings.getValue('Conformance-search', 'version', '');
  edtConfName.Text := Settings.getValue('Conformance-search', 'name', '');
  edtConfTitle.Text := Settings.getValue('Conformance-search', 'title', '');
  edtConfText.Text := Settings.getValue('Conformance-search', 'text', '');
  dedConfDate.Text := Settings.getValue('Conformance-search', 'date', '');
  edtConfJurisdiction.ItemIndex := Settings.getValue('Conformance-search', 'jurisdiction', 0);
  edtConfPub.Text := Settings.getValue('Conformance-search', 'publisher', '');
  cbxConfStatus.ItemIndex := Settings.getValue('Conformance-search', 'status', 0);
  edtConfUpdated.Text := Settings.getValue('Conformance-search', 'updated', '');
  cbConfUseLastUpdated.IsChecked := Settings.getValue('Conformance-search', 'updated-opt', false);
  edtConfTag.Text := Settings.getValue('Conformance-search', 'tag', '');

  edtPText.Text := Settings.getValue('Patient-search', 'text', '');
  dedPLastEdit.Text := Settings.getValue('Patient-search', 'updated', '');
  edtPTag.Text := Settings.getValue('Patient-search', 'tag', '');
  edtPName.Text := Settings.getValue('Patient-search', 'name', '');
  edtPTelecom.Text := Settings.getValue('Patient-search', 'telecom', '');
  cbxPDob.ItemIndex := Settings.getValue('Patient-search', 'dobopt', 0);
  dedPDob.Text := Settings.getValue('Patient-search', 'dob', '');
  cbxPGender.ItemIndex := Settings.getValue('Patient-search', 'gender', 0);
  cbxPDeceased.ItemIndex := Settings.getValue('Patient-search', 'dod', 0);
  dedPDeceased.Text := Settings.getValue('Patient-search', 'dodopt', '');
  cbxPActive.ItemIndex := Settings.getValue('Patient-search', 'active', 0);
  edtPIdentifier.Text := Settings.getValue('Patient-search', 'id', '');
  cbxPSpecies.ItemIndex := Settings.getValue('Patient-search', 'species', 0);
  cbxPLanguage.ItemIndex := Settings.getValue('Patient-search', 'lang', 0);
  cbPUseLastUpdated.IsChecked := Settings.getValue('Patient-search', 'updated-opt', false);

  btnFetchMore.Visible := false;
  btnMatch.enabled := FCapabilityStatement.supportsOperation('Patient', 'match');
  btnMatch.visible := btnMatch.enabled;
  FConfMatches := TFslList<TFHIRResource>.create;
  FPatMatches := TFslList<TFHIRPatient>.create;
  cbxSearchTypeChange(nil);
  cbxPDobChange(nil);
  cbxPDeceasedChange(nil);
  cbPUseLastUpdatedChange(nil);
  cbConfUseLastUpdatedChange(nil);
end;


procedure TServerFrame.openPatient(pat: TFHIRPatient);
var
  tab : TTabItem;
  frm : TPatientHomeFrame;
begin
  tab := Tabs.Add(TTabItem);
  Tabs.ActiveTab := tab;
  tab.Text := 'Patient '+pat.id+' on '+FClient.address;
  frm := TPatientHomeFrame.create(tab);
  tab.TagObject := frm;
  frm.TagObject := tab;
  frm.Parent := tab;
  frm.Tabs := tabs;
  frm.OnWork := onwork;
  frm.Settings := Settings.link;
  frm.tab := tab;
  frm.Align := TAlignLayout.Client;
  frm.Client := client.link;
  frm.CapabilityStatement := CapabilityStatement.Link;
  frm.Patient := pat.Link;
  frm.Load;
end;

procedure TServerFrame.SetCapabilityStatement(const Value: TFhirCapabilityStatement);
begin
  FCapabilityStatement.free;
  FCapabilityStatement := Value;
end;

procedure TServerFrame.SetClient(const Value: TFHIRClient);
begin
  FClient.free;
  FClient := Value;
end;

end.
