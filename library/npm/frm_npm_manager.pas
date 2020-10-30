unit frm_npm_manager;

{$I fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, IniFiles,
  frm_progress,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Logging,
  FHIR.Npm.Package, FHIR.Npm.Cache, FHIR.LCL.Managers;

type

  { TPackageListManager }

  TPackageListManager = class (TListManager<TNpmPackage>)
  private
    FCache : TFHIRPackageManager;
  public
    Constructor Create; override;
    destructor Destroy; override;

    function allowedOperations(item : TNpmPackage) : TNodeOperationSet; override;
    function ShowLoadingProgress : boolean; override;
    function loadList : boolean; override;

    function getImageIndex(item : TNpmPackage) : integer; override;
    function getCellText(item : TNpmPackage; col : integer) : String; override;
    function compare(left, right : TNpmPackage) : integer; override;
  end;


  { TPackageCacheForm }

  TPackageCacheForm = class(TForm)
    btnCancel: TButton;
    btnDelete: TButton;
    btnReload: TButton;
    Button1: TButton;
    btnImportFile: TButton;
    btnImportUrl: TButton;
    btnFindPackages: TButton;
    btnCommon: TButton;
    btnCopyReport: TButton;
    ImageList1: TImageList;
    Label1: TLabel;
    lblDownload: TLabel;
    ListView1: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pbDownload: TProgressBar;
    rbUserMode: TRadioButton;
    rbSystem: TRadioButton;
    procedure btnReloadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbSystemChange(Sender: TObject);
    procedure rbUserModeChange(Sender: TObject);
  private
    FIni: TIniFile;
    FManager : TPackageListManager;
  public
    property Ini : TIniFile read FIni write FIni;
  end;

var
  PackageCacheForm: TPackageCacheForm;

implementation

{$R *.lfm}

{ TPackageListManager }

constructor TPackageListManager.Create;
begin
  inherited Create;
  FCache := TFHIRPackageManager.create(true);
end;

destructor TPackageListManager.Destroy;
begin
  FCache.Free;
  inherited Destroy;
end;

function TPackageListManager.allowedOperations(item : TNpmPackage): TNodeOperationSet;
begin
  result := [opAdd, opDelete];
end;

function TPackageListManager.ShowLoadingProgress: boolean;
begin
  Result:= inherited ShowLoadingProgress;
end;

function TPackageListManager.loadList : boolean;
var
  resp : TFHIRLoadPackagesTaskResponse;
begin
  resp := TFHIRLoadPackagesTaskResponse.create;
  try
    result := DoTask(PackageCacheForm, GPackageLoaderTaskId, TFHIRLoadPackagesTaskRequest.create(FCache.link), resp.link);
    if result then
      Data.AddAll(resp.Packages);
  finally
    resp.free;
  end;
end;

function TPackageListManager.getImageIndex(item: TNpmPackage): integer;
begin
  result := -1;
end;

function TPackageListManager.getCellText(item: TNpmPackage; col: integer): String;
begin
  case col of
    0: result := item.name;
    1: result := item.version;
    2: result := item.fhirVersion;
    3: if item.installed = 0 then result := '??' else result := DescribePeriod(now - item.installed);
    4: result := DescribeBytes(item.size);
    5: result := item.canonical;
    6: result := item.dependencySummary;
  end;
end;

function TPackageListManager.compare(left, right: TNpmPackage): integer;
begin
  if left.name = right.name then
    result := CompareStr(left.version, right.version)
  else
    result := CompareStr(left.name, right.name)
end;


{ TPackageCacheForm }

procedure TPackageCacheForm.FormCreate(Sender: TObject);
begin
  FManager := TPackageListManager.create;
  FManager.List := ListView1;
  FManager.registerControl(btnImportFile, copAdd, 'file');
  FManager.registerControl(btnImportURL, copAdd, 'url');
  FManager.registerControl(btnReload, copReload);
  FManager.registerControl(btnDelete, copDelete);
end;

procedure TPackageCacheForm.btnReloadClick(Sender: TObject);
begin
end;

procedure TPackageCacheForm.FormDestroy(Sender: TObject);
begin
  ini.writeInteger('package-manager-view', 'width-name', ListView1.Columns[0].width);
  ini.writeInteger('package-manager-view', 'width-ver', ListView1.Columns[1].width);
  ini.writeInteger('package-manager-view', 'width-fver', ListView1.Columns[2].width);
  ini.writeInteger('package-manager-view', 'width-age', ListView1.Columns[3].width);
  ini.writeInteger('package-manager-view', 'width-size', ListView1.Columns[4].width);
  ini.writeInteger('package-manager-view', 'width-canonical', ListView1.Columns[5].width);
  ini.writeInteger('package-manager-view', 'width', width);
  ini.writeInteger('package-manager-view', 'height', height);
  FManager.Free;
end;

procedure TPackageCacheForm.FormShow(Sender: TObject);
begin
  ListView1.Columns[0].width := ini.readInteger('package-manager-view', 'width-name', ListView1.Columns[0].width);
  ListView1.Columns[1].width := ini.readInteger('package-manager-view', 'width-ver', ListView1.Columns[1].width);
  ListView1.Columns[2].width := ini.readInteger('package-manager-view', 'width-fver', ListView1.Columns[2].width);
  ListView1.Columns[3].width := ini.readInteger('package-manager-view', 'width-age', ListView1.Columns[3].width);
  ListView1.Columns[4].width := ini.readInteger('package-manager-view', 'width-size', ListView1.Columns[4].width);
  ListView1.Columns[5].width := ini.readInteger('package-manager-view', 'width-canonical', ListView1.Columns[5].width);
  width := ini.readInteger('package-manager-view', 'width', width);
  height := ini.readInteger('package-manager-view', 'height', height);
  if not FManager.doLoad then
    Close;
end;

procedure TPackageCacheForm.rbSystemChange(Sender: TObject);
begin
  if FManager.FCache.UserMode then
  begin
    FManager.FCache := TFHIRPackageManager.create(false);
    FManager.doLoad;
  end;
end;

procedure TPackageCacheForm.rbUserModeChange(Sender: TObject);
begin
  if not FManager.FCache.UserMode then
  begin
    FManager.FCache := TFHIRPackageManager.create(true);
    FManager.doLoad;
  end;
end;

end.

