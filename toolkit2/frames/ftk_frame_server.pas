unit ftk_frame_server;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Dialogs,
  fsl_utilities, fsl_json,
  fui_lcl_managers,
  fhir_common, fhir_factory, fhir_client,
  ftk_utilities, ftk_constants, ftk_context,
  ftk_worker_base;

type
  TFrame = TBaseWorkerFrame;
  TServerWorkerFrame = class;

  { TPatientSearchManager }

  TPatientSearchManager = class (TListManager<TFHIRPatientW>)
  private
    FView : TServerWorkerFrame;
  public
    function canSort : boolean; override;
    function doubleClickEdit : boolean; override;
    function allowedOperations(item : TFHIRPatientW) : TNodeOperationSet; override;
    function loadList : boolean; override;

    procedure buildMenu; override;
    function getCellText(item : TFHIRPatientW; col : integer) : String; override;
    function getSummaryText(item : TFHIRPatientW) : String; override;
    function compareItem(left, right : TFHIRPatientW; col : integer) : integer; override;
    function filterItem(item : TFHIRPatientW; s : String) : boolean; override;

    function executeItem(item : TFHIRPatientW; mode : String) : boolean; override;
  end;

  { TServerWorkerFrame }

  TServerWorkerFrame = class(TFrame)
    btnSearch: TButton;
    cbxSearchType: TComboBox;
    cbxGender: TComboBox;
    edtId: TEdit;
    edtName: TEdit;
    edtDob: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lvPatients: TListView;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlSearchOutcome: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    procedure btnSearchClick(Sender: TObject);
    procedure cbxGenderChange(Sender: TObject);
    procedure edtDobChange(Sender: TObject);
    procedure edtIdChange(Sender: TObject);
    procedure edtNameChange(Sender: TObject);
    procedure FrameClick(Sender: TObject);
  private
    FServer: TFHIRServerEntry;

    FSearch : TFHIRBundleW;
    FFactory : TFHIRFactory;

    FPatientManager : TPatientSearchManager;

    procedure SetServer(AValue: TFHIRServerEntry);
    procedure doPatientSearch;
  protected
    procedure save(json : TJsonObject); override;
  public
    destructor Destroy; override;
    procedure init(json : TJsonObject); override;
    procedure saveStatus; override;

    property server : TFHIRServerEntry read FServer write SetServer;
  end;

implementation

{$R *.lfm}

{ TPatientSearchManager }

function TPatientSearchManager.canSort: boolean;
begin
  Result := true;
end;

function TPatientSearchManager.doubleClickEdit: boolean;
begin
  Result := false;
end;

function TPatientSearchManager.allowedOperations(item: TFHIRPatientW): TNodeOperationSet;
begin
  result := [opExecute];
end;

function TPatientSearchManager.loadList: boolean;
var
  be : TFhirBundleEntryW;
  pat : TFhirPatientW;
begin
  result := true;
  for be in FView.FSearch.entries.forEnum do
  begin
    if be.resource.fhirType = 'Patient' then
    begin
      pat := FView.FFactory.wrapPatient(be.resource.link);
      Data.Add(pat);
//      pat.Tags['url'] := be.url;
    end;
  end;
end;

procedure TPatientSearchManager.buildMenu;
begin
  inherited buildMenu;
  registerMenuEntry('Open', ICON_OPEN, copExecute);
end;

function TPatientSearchManager.getCellText(item: TFHIRPatientW; col: integer): String;
begin
  case col of
    0: result := item.id;
    1: result := item.active;
    2: result := item.nameSummary;
    3: result := item.gender;
    4: result := item.dob;
    5: result := item.identifierSummary;
    6: result := item.contactSummary;
  else
    result := '??';
  end;
end;

function TPatientSearchManager.getSummaryText(item: TFHIRPatientW): String;
begin
  Result := item.describe;
end;

function TPatientSearchManager.compareItem(left, right: TFHIRPatientW; col: integer): integer;
begin
  case col of
    0: result := String.Compare(left.id, right.id);
    1: result := String.Compare(left.active, right.active);
    2: result := String.Compare(left.nameSummary, right.nameSummary);
    3: result := String.Compare(left.gender, right.gender);
    4: result := String.Compare(left.dob, right.dob);
    5: result := String.Compare(left.identifierSummary, right.identifierSummary);
    6: result := String.Compare(left.contactSummary, right.contactSummary);
  else
    result := 0;
  end;
end;

function TPatientSearchManager.filterItem(item: TFHIRPatientW; s: String): boolean;
begin
  Result := item.nameSummary.contains(s);
end;

function TPatientSearchManager.executeItem(item: TFHIRPatientW; mode: String): boolean;
begin
  Result := true;
  FView.Context.OpenResource(URLPath([FView.FServer.URL, 'Patient', item.id]));
end;

{ TServerWorkerFrame }

destructor TServerWorkerFrame.Destroy;
begin
  FPatientManager.Free;
  FServer.Free;
  FFactory.Free;
  FSearch.Free;
  inherited;
end;

procedure TServerWorkerFrame.saveStatus;
begin
  inherited;
  FPatientManager.saveStatus;
end;

procedure TServerWorkerFrame.init(json: TJsonObject);
begin
  FPatientManager := TPatientSearchManager.create;
  FPatientManager.Images := FContext.images;
  FPatientManager.Settings := FContext.Settings;
  FPatientManager.List := lvPatients;
  FPatientManager.FView := self;

  FFactory := FContext.factory(FServer.version);

  edtName.Text := json.str['patient-name'];
  cbxGender.Text := json.str['patient-gender'];
  edtDoB.Text := json.str['patient-dob'];
  edtId.Text := json.str['patient-id'];
end;

procedure TServerWorkerFrame.doPatientSearch;
var
  params : TStringList;
  t : UInt64;
begin
  cursor := crHourGlass;
  try
    if FServer.client = nil then
      Context.OnConnectToServer(self, FServer);
    FSearch.free;
    FSearch := nil;
    params := TStringList.create;
    try
      if edtName.Text <> '' then
        params.add('name='+edtName.text);
      if cbxGender.Text <> '' then
        params.add('gender='+cbxGender.text);
      if edtDoB.Text <> '' then
        params.add('birthdate='+edtDoB.text);
      if edtId.Text <> '' then
        params.add('identifier='+edtDoB.text);
      t := GetTickCount64;
      FSearch := FFactory.wrapBundle(FServer.client.searchV('Patient', true, params));
      t := GetTickCount64 - t;
      FPatientManager.doLoad;
      pnlSearchOutcome.caption := '  Search: '+inttostr(FSearch.count('Patient'))+' of '+inttostr(FSearch.total)+' as of '+FSearch.timestamp.toXML+' ('+inttostr(t)+'ms)';
    finally
      params.free;
    end;
  finally
    cursor := crDefault;
  end;
end;

procedure TServerWorkerFrame.FrameClick(Sender: TObject);
begin
end;

procedure TServerWorkerFrame.btnSearchClick(Sender: TObject);
begin
  case cbxSearchType.itemIndex of
    0: doPatientSearch;
  else
    abort;
  end;
end;

procedure TServerWorkerFrame.cbxGenderChange(Sender: TObject);
begin
  changed;
end;

procedure TServerWorkerFrame.edtDobChange(Sender: TObject);
begin
  changed;
end;

procedure TServerWorkerFrame.edtIdChange(Sender: TObject);
begin
  changed;
end;

procedure TServerWorkerFrame.edtNameChange(Sender: TObject);
begin
  changed;
end;

procedure TServerWorkerFrame.SetServer(AValue: TFHIRServerEntry);
begin
  FServer.Free;
  FServer := AValue;
end;

procedure TServerWorkerFrame.save(json: TJsonObject);
begin
  json.str['patient-name'] := edtName.Text;
  json.str['patient-gender'] := cbxGender.Text;
  json.str['patient-dob'] := edtDoB.Text;
  json.str['patient-id'] := edtId.Text;
end;

end.

