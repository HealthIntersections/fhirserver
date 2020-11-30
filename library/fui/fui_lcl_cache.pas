unit fui_lcl_cache;

{$I fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, DateUtils,
  StdCtrls, IniFiles, Clipbrd, Menus,
  fui_lcl_progress, fui_lcl_registry,
  fsl_base, fsl_utilities, fsl_logging,
  fsl_fetcher,
  fsl_npm, fsl_npm_cache, fui_lcl_managers;

type

  { TPackageListManager }

  TPackageListManager = class (TListManager<TNpmPackage>)
  private
    FCache : TFHIRPackageManager;
  public
    Constructor Create; override;
    destructor Destroy; override;

    function canSort : boolean; override;
    function allowedOperations(item : TNpmPackage) : TNodeOperationSet; override;
    function loadList : boolean; override;

    function getCellText(item : TNpmPackage; col : integer) : String; override;
    function compareItem(left, right : TNpmPackage; col : integer) : integer; override;

    function addItem(mode : String) : TNpmPackage; override;
    procedure DeleteItem(item : TNpmPackage); override;
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
    dlgOpen: TOpenDialog;
    FHIRR21: TMenuItem;
    FHIRR31: TMenuItem;
    FHIRR41: TMenuItem;
    ImageList1: TImageList;
    Label1: TLabel;
    lblProgress: TLabel;
    ListView1: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pbDownload: TProgressBar;
    pmImport: TPopupMenu;
    rbUserMode: TRadioButton;
    rbSystem: TRadioButton;
    USCoreCurrentStable1: TMenuItem;
    procedure btnCancelClick(Sender: TObject);
    procedure btnCommonClick(Sender: TObject);
    procedure btnCopyReportClick(Sender: TObject);
    procedure btnFindPackagesClick(Sender: TObject);
    procedure FHIRR21Click(Sender: TObject);
    procedure FHIRR31Click(Sender: TObject);
    procedure FHIRR41Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbSystemChange(Sender: TObject);
    procedure rbUserModeChange(Sender: TObject);
    procedure USCoreCurrentStable1Click(Sender: TObject);
  private
    FIni: TIniFile;
    FManager : TPackageListManager;
    FStop : boolean;
    procedure packageWork(sender : TObject; pct : integer; done : boolean; msg : String);
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

function TPackageListManager.canSort: boolean;
begin
  Result:= true;
end;

function TPackageListManager.allowedOperations(item : TNpmPackage): TNodeOperationSet;
begin
  result := [opAdd, opDelete];
end;

function TPackageListManager.loadList : boolean;
var
  resp : TFHIRLoadPackagesTaskResponse;
begin
  resp := TFHIRLoadPackagesTaskResponse.create;
  try
    result := DoBackgroundTask(PackageCacheForm, GPackageLoaderTaskId, TFHIRLoadPackagesTaskRequest.create(FCache.link), resp.link);
    if result then
      Data.AddAll(resp.Packages);
  finally
    resp.free;
  end;
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

function TPackageListManager.compareItem(left, right: TNpmPackage; col : integer): integer;
begin
  case col of
    -1: if left.name = right.name then
          result := CompareStr(left.version, right.version)
        else
          result := CompareStr(left.name, right.name);
    0: result := CompareStr(left.name, right.name);
    1: result := CompareStr(left.version, right.version);
    2: result := CompareStr(left.fhirVersion, right.fhirVersion);
    3: result := compareDate(left.installed, right.installed);
    4: result := CompareStr(left.canonical, right.canonical);
    5: result := CompareStr(left.dependencySummary, right.dependencySummary);
  else
    result := 0;
  end;
end;

function TPackageListManager.addItem(mode: String): TNpmPackage;
var
  url : String;
begin
  result := nil;
  if (mode = 'file') then
  begin
    if PackageCacheForm.dlgOpen.Execute then
    begin
      PackageCacheForm.Cursor := crHourGlass;
      try
        FCache.OnWork := PackageCacheForm.packageWork;
        result := FCache.Import(fileToBytes(PackageCacheForm.dlgOpen.FileName));
      finally
        PackageCacheForm.Cursor := crDefault;
      end;
    end;
  end
  else if (mode = 'url') then
  begin
    FCache.OnWork := PackageCacheForm.packageWork;
    if InputQuery('Fetch Package from Web', 'Enter URL:', url) then
      result := FCache.Import(TInternetFetcher.fetchUrl(url));
  end
  else if not FCache.packageExists(mode, '') then
  begin
    FCache.OnWork := PackageCacheForm.packageWork;
    if FCache.autoInstallPackage(mode, '') then
      result := FCache.loadPackage(mode, '');
  end;
end;

procedure TPackageListManager.DeleteItem(item: TNpmPackage);
begin
  FCache.remove(item.name, item.version);
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

procedure TPackageCacheForm.btnCancelClick(Sender: TObject);
begin
  FStop := true;
end;

procedure TPackageCacheForm.btnCommonClick(Sender: TObject);
var
  pt, pt2: TPoint;
begin
  pt.x := panel3.left+btnCommon.Left;
  pt.y := panel3.top+btnCommon.Top + btnCommon.Height;
  pt2 := ClientToScreen(pt);
  pmImport.PopUp(pt2.x, pt2.y);
end;

procedure TPackageCacheForm.btnCopyReportClick(Sender: TObject);
begin
  Clipboard.Open;
  Clipboard.AsText := FManager.FCache.Report;
  Clipboard.Close;
end;

procedure TPackageCacheForm.btnFindPackagesClick(Sender: TObject);
begin
  PackageRegistryForm := TPackageRegistryForm.create(self);
  try
    PackageRegistryForm.Ini := FIni;
    PackageRegistryForm.showModal;
  finally
    PackageRegistryForm.free;
  end;
end;

procedure TPackageCacheForm.FHIRR21Click(Sender: TObject);
begin
  FManager.doAdd('hl7.fhir.r2.core');
end;

procedure TPackageCacheForm.FHIRR31Click(Sender: TObject);
begin
  FManager.doAdd('hl7.fhir.r3.core');
end;

procedure TPackageCacheForm.FHIRR41Click(Sender: TObject);
begin
  FManager.doAdd('hl7.fhir.r4.core');
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

procedure TPackageCacheForm.USCoreCurrentStable1Click(Sender: TObject);
begin
  FManager.doAdd('hl7.fhir.us.core');
end;

procedure TPackageCacheForm.packageWork(sender: TObject; pct: integer; done: boolean; msg: String);
begin
  if done then
  begin
    pbDownload.Visible := false;
    btnCancel.Visible := false;
    lblProgress.Visible := false;
  end
  else
  begin
    if not pbDownload.Visible then
    begin
      FStop := false;
      pbDownload.Visible := true;
      pbDownload.Position := 0;
      lblProgress.Visible := true;
      btnCancel.Visible := true;
    end;
    lblProgress.caption := msg;
    pbDownload.Position := pct;
    if FStop then
    begin
      pbDownload.Visible := false;
      btnCancel.Visible := false;
      lblProgress.Visible := false;
      abort;
    end;
  end;
  Application.ProcessMessages;
end;

end.

