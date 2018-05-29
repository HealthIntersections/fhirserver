unit FHIR.Cache.PackageBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  {$IFDEF NPPUNICODE}FHIR.Npp.Form, {$ENDIF}
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  VirtualTrees,
  FHIR.Support.Objects, FHIR.Support.Generics, FHIR.Support.Json, FHIR.Web.Fetcher, FHIR.Support.DateTime,
  FHIR.Cache.PackageManager;

type
  TOnLoadUrlEvent = procedure (sender : TObject; url : String) of object;

  TPackageFinderForm = class({$IFDEF NPPUNICODE} TNppForm {$ELSE} TForm {$ENDIF})
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    edtFilter: TEdit;
    Label2: TLabel;
    ComboBox1: TComboBox;
    btnClose: TButton;
    btnInstall: TButton;
    Panel3: TPanel;
    grid: TVirtualStringTree;
    procedure edtFilterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure gridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure gridRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure gridAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FChanged: boolean;
    FLoaded : boolean;
    FList : TFslList<TPackageDefinition>;
    FFiltered : TFslList<TPackageDefinition>;
    FOnLoad : TOnLoadUrlEvent;
    FIndex : integer;
    procedure loadServer;
    procedure applyFilter;
    function matchesFilter(pck : TPackageDefinition) : boolean;
  public
    property OnLoadUrl : TOnLoadUrlEvent read FOnLoad write FOnLoad;
  end;

var
  PackageFinderForm: TPackageFinderForm;

implementation

{$R *.dfm}


procedure TPackageFinderForm.btnInstallClick(Sender: TObject);
begin
  FOnLoad(self, FFiltered[FIndex].Url);
end;

procedure TPackageFinderForm.edtFilterChange(Sender: TObject);
begin
  applyFilter;
  grid.RootNodeCount := 0;
  grid.RootNodeCount := FFiltered.Count;
//  gridCellClick(nil, grid.Row);
end;

procedure TPackageFinderForm.FormActivate(Sender: TObject);
begin
  if not FLoaded then
    loadServer;
end;

procedure TPackageFinderForm.FormCreate(Sender: TObject);
begin
  FList := TFslList<TPackageDefinition>.create;
  FFiltered := TFslList<TPackageDefinition>.create;
end;

procedure TPackageFinderForm.FormDestroy(Sender: TObject);
begin
  FFiltered.Free;
  FList.Free;
end;

procedure TPackageFinderForm.gridAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  btnInstall.Enabled := true;
  FIndex := Node.Index;
end;

procedure TPackageFinderForm.gridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  pck : TPackageDefinition;
begin
  pck := FFiltered[Node.Index];
  case Column of
    0: CellText := pck.Id;
    1: CellText := pck.Version;
    2: CellText := pck.Description;
    3: CellText := pck.Canonical;
    4: CellText := pck.FHIRVersion;
    5: CellText := FormatDateTime('c', pck.Date);
  end;
end;

procedure TPackageFinderForm.gridRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  btnInstall.Enabled := false;
end;

procedure TPackageFinderForm.loadServer;
var
  j : TJsonObject;
  a : TJsonArray;
  i : TJsonNode;
  p : TPackageDefinition;
begin
  FList.clear;
  AddStandardPackages(FList);
  a := TInternetFetcher.fetchJsonArr('https://build.fhir.org/ig/qas.json');
  try
    for i in a do
    begin
      j := i as TJsonObject;
      if (j.str['package-id'].Contains('.')) then
      begin
        p := TPackageDefinition.Create;
        try
          p.Id := j.str['package-id'];
          p.Version := j.str['ig-ver'];
          p.Canonical := j.str['url'];
          p.Date := TDateTimeEx.fromFormat('DDD, dd mmm, yyyy hh:nn:ss Z', j.str['date']).DateTime;
          p.Description := j.str['name'];
          p.FHIRVersion := j.str['version'];
          p.Url := 'https://build.fhir.org/ig/'+j.str['repo'];
          FList.Add(p.Link);
        finally
          p.Free;
        end;
      end;
    end;
  finally
    a.Free;
  end;
  FLoaded := true;
  applyFilter;
  grid.RootNodeCount := 0;
  grid.RootNodeCount := FFiltered.Count;
//  gridCellClick(nil, grid.Row);
end;

function TPackageFinderForm.matchesFilter(pck: TPackageDefinition): boolean;
begin
  result :=
    pck.id.Contains(edtFilter.Text) or
    pck.Version.Contains(edtFilter.Text) or
    pck.Description.Contains(edtFilter.Text) or
    pck.FHIRVersion.Contains(edtFilter.Text) or
    pck.Canonical.Contains(edtFilter.Text) or
    formatDateTime('c', pck.Date).Contains(edtFilter.Text);
end;

procedure TPackageFinderForm.applyFilter;
var
  pck : TPackageDefinition;
begin
  FFiltered.Clear;
  if edtFilter.Text = '' then
    FFiltered.AddAll(FList)
  else
  begin
    for pck in FList do
      if matchesFilter(pck) then
        FFiltered.Add(pck.link);
  end;
end;

end.
