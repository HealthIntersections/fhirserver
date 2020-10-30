unit frm_fhir_manager;


{$I fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls,
  FHIR.LCL.Managers,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Logging,
  FHIR.Npm.Package, FHIR.Npm.Cache;

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
    procedure loadList; override;

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
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure btnReloadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FManager : TPackageListManager;
  public

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
  Result:=inherited ShowLoadingProgress;
end;

procedure TPackageListManager.loadList;
begin
  FCache.ListPackages(All_Package_Kinds, Data);
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
    5: result := item.dependencySummary;
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
  FManager.Free;
end;

procedure TPackageCacheForm.FormShow(Sender: TObject);
begin
  FManager.doLoad;
end;

end.

