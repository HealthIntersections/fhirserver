unit ServerForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.TabControl, FMX.ListBox, FMX.Layouts, FMX.DateTimeCtrls,
  FMX.Edit, System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.Platform,
  DateSupport,
  AdvGenerics,
  FHIRTypes, FHIRResources, FHIRClient,
  BaseFrame, AppEndorserFrame, CapabilityStatementEditor;

type
  TFrame = TBaseFrame; // re-aliasing the Frame to work around a designer bug

  TServerFrame = class (TFrame)
    btnTest: TButton;
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
    procedure btnTestClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnConfSearchClick(Sender: TObject);
    procedure gridConfMatchesGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure gridConfMatchesCellDblClick(const Column: TColumn; const Row: Integer);
    procedure cbxSearchTypeChange(Sender: TObject);
  private
    FClient: TFHIRClient;
    FCapabilityStatement: TFhirCapabilityStatement;
    FCSTab : TTabItem;
    FCsForm : TCapabilityStatementEditorFrame;
    FBundle : TFhirBundle;
    FMatches : TAdvList<TFHIRResource>;
    procedure SetClient(const Value: TFHIRClient);
    procedure SetCapabilityStatement(const Value: TFhirCapabilityStatement);
    { Private declarations }
  public
    { Public declarations }
    Destructor Destroy; override;
    property Client : TFHIRClient read FClient write SetClient;
    property CapabilityStatement : TFhirCapabilityStatement read FCapabilityStatement write SetCapabilityStatement;

    procedure load; override;
  end;

implementation

{$R *.fmx}

{ TServerFrame }

procedure TServerFrame.btnCloseClick(Sender: TObject);
begin
  try
    Ini.WriteString('Conformance-search', 'url', edtConfUrl.Text);
    Ini.WriteString('Conformance-search', 'id', edtConfId.Text);
    Ini.WriteString('Conformance-search', 'version', edtConfVersion.Text);
    Ini.WriteString('Conformance-search', 'name', edtConfName.Text);
    Ini.WriteString('Conformance-search', 'title', edtConfTitle.Text);
    Ini.WriteString('Conformance-search', 'text', edtConfText.Text);
    Ini.WriteString('Conformance-search', 'date', dedConfDate.Text);
    Ini.WriteInteger('Conformance-search', 'jurisdiction', edtConfJurisdiction.ItemIndex);
    Ini.WriteString('Conformance-search', 'publisher', edtConfPub.Text);
    Ini.WriteInteger('Conformance-search', 'status', cbxConfStatus.ItemIndex);
    Ini.WriteString('Conformance-search', 'updated', edtConfUpdated.Text);
    Ini.WriteString('Conformance-search', 'tag', edtConfTag.Text);
  except
  end;
  Close;
end;

procedure TServerFrame.btnTestClick(Sender: TObject);
var
  tab : TTabItem;
  appForm : TAppEndorsementFrame;
begin
  tab := Tabs.Add(TTabItem);
  Tabs.ActiveTab := tab;
  tab.Text := 'AppEndorser for '+FClient.address;
  appForm := TAppEndorsementFrame.create(tab);
  tab.TagObject := appForm;
  appForm.TagObject := tab;
  appForm.Parent := tab;
  appForm.Tabs := tabs;
  appForm.Ini := Ini;
  appForm.tab := tab;
  appForm.Align := TAlignLayout.Client;
  appForm.Client := client.link;
  appForm.load;
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
    FcsForm.Ini := Ini;
    FcsForm.tab := FCSTab;
    FcsForm.Align := TAlignLayout.Client;
    FcsForm.Client := client.link;
    FcsForm.Resource := CapabilityStatement.Link;
    FcsForm.Filename := '$$';
    FcsForm.Load;
  end;
end;

procedure TServerFrame.cbxSearchTypeChange(Sender: TObject);
begin
  tabSearch.TabIndex := cbxSearchType.ItemIndex;
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
var
  params : TDictionary<String, String>;
  be : TFhirBundleEntry;
  fcs : IFMXCursorService;
  start : TDateTime;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
    fcs := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService
  else
    fcs := nil;
  if Assigned(fcs) then
  begin
    Cursor := fcs.GetCursor;
    fcs.SetCursor(crHourGlass);
  end;
  try
    FMatches.Clear;
    gridConfMatches.RowCount := FMatches.Count;
    FBundle.Free;
    FBundle := nil;

    params := TDictionary<String, String>.create;
    try
      params.Add('_type', 'CapabilityStatement,StructureDefinition,ImplementationGuide,SearchParameter,MessageDefinition,OperationDefinition,CompartmentDefinition,StructureMap,GraphDefinition,CodeSystem,ValueSet,ConceptMap,ExpansionProfile,NamingSystem');
      params.Add('_summary', 'true');

      if edtConfUrl.Text <> '' then
        params.add('url', edtConfUrl.Text);
      if edtConfId.Text <> '' then
        params.add('identifier', edtConfId.Text);
      if edtConfVersion.Text <> '' then
        params.add('version', edtConfVersion.Text);
      if edtConfName.Text <> '' then
        params.add('name', edtConfName.Text);
      if edtConfTitle.Text <> '' then
        params.add('title', edtConfTitle.Text);
      if edtConfText.Text <> '' then
        params.add('_text', edtConfText.Text);
      if dedConfDate.Text <> '' then
        params.add('date', dedConfDate.Text);
      if edtConfJurisdiction.ItemIndex <> -1 then
        params.add('jurisdiction', getJurisdictionSearch(edtConfJurisdiction.ItemIndex));
      if edtConfPub.Text <> '' then
        params.add('publisher', edtConfPub.Text);
      if cbxConfStatus.ItemIndex <> -1 then
        params.add('status', cbxConfStatus.Items[cbxConfStatus.ItemIndex]);
      if edtConfUpdated.Text <> '' then
        params.add('_lastUpdated', edtConfUpdated.Text);
      if edtConfTag.Text <> '' then
        params.add('_tag', edtConfTag.Text);

      start := now;
      FBundle := FClient.search(false, params);
      for be in FBundle.entryList do
        if (be.search.mode = SearchEntryModeMatch) and (be.resource <> nil) then
          FMatches.Add(be.resource.Link);
      gridConfMatches.RowCount := FMatches.Count;
      lblOutcome.Text := inttostr(FMatches.Count)+' resources in '+describePeriod(now - start);
    finally
      params.Free;
    end;
  finally
    if Assigned(fCS) then
      fcs.setCursor(Cursor);
  end;
end;

destructor TServerFrame.Destroy;
begin
  FClient.free;
  FCapabilityStatement.free;
  FBundle.Free;
  FMatches.Free;

  inherited;
end;

procedure TServerFrame.gridConfMatchesCellDblClick(const Column: TColumn; const Row: Integer);
var
  res : TFhirResource;
begin
  res := Client.readResource(FMatches[Row].ResourceType, FMatches[Row].id);
  try
    OnOpenResource(self, client, client.format, res);
  finally
    res.Free;
  end;
end;

procedure TServerFrame.gridConfMatchesGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  res : TFhirMetadataResource;
begin
  if not (FMatches[aRow] is TFhirMetadataResource) then
  begin
    case ACol of
      0: Value := FMatches[aRow].fhirType;
      1: Value := FMatches[aRow].id;
    end;
  end
  else
  begin
    res := FMatches[aRow] as TFhirMetadataResource;
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

procedure TServerFrame.load;
begin
  edtConfUrl.Text := Ini.ReadString('Conformance-search', 'url', '');
  edtConfId.Text := Ini.ReadString('Conformance-search', 'id', '');
  edtConfVersion.Text := Ini.ReadString('Conformance-search', 'version', '');
  edtConfName.Text := Ini.ReadString('Conformance-search', 'name', '');
  edtConfTitle.Text := Ini.ReadString('Conformance-search', 'title', '');
  edtConfText.Text := Ini.ReadString('Conformance-search', 'text', '');
  dedConfDate.Text := Ini.ReadString('Conformance-search', 'date', '');
  edtConfJurisdiction.ItemIndex := Ini.ReadInteger('Conformance-search', 'jurisdiction', -1);
  edtConfPub.Text := Ini.ReadString('Conformance-search', 'publisher', '');
  cbxConfStatus.ItemIndex := Ini.ReadInteger('Conformance-search', 'status', -1);
  edtConfUpdated.Text := Ini.ReadString('Conformance-search', 'updated', '');
  edtConfTag.Text := Ini.ReadString('Conformance-search', 'tag', '');
  FMatches := TAdvList<TFHIRResource>.create;
  cbxSearchTypeChange(nil);
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
