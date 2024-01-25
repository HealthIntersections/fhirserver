unit fui_lcl_cache;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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
    Constructor Create(pc : TFHIRPackageManager);
    destructor Destroy; override;

    function canSort : boolean; override;
    function allowedOperations(item : TNpmPackage) : TNodeOperationSet; override;
    function loadList : boolean; override;

    function getCellText(item : TNpmPackage; col : integer) : String; override;
    function compareItem(left, right : TNpmPackage; col : integer) : integer; override;

    function addItem(mode : String) : TNpmPackage; override;
    function deleteItem(item : TNpmPackage) : boolean; override;
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
    procedure SetIni(AValue: TIniFile);
    function GetCache: TFHIRPackageManager;
    procedure SetCache(const Value: TFHIRPackageManager);
    function npmMode : TFHIRPackageManagerMode;
  public
    property Ini : TIniFile read FIni write SetIni;
    property Cache : TFHIRPackageManager read GetCache write SetCache;
  end;

var
  PackageCacheForm: TPackageCacheForm;

implementation

{$R *.lfm}

{ TPackageListManager }

constructor TPackageListManager.Create(pc : TFHIRPackageManager);
begin
  inherited Create;
  FCache := pc;
end;

destructor TPackageListManager.Destroy;
begin
  FCache.free;
  inherited Destroy;
end;

function TPackageListManager.canSort: boolean;
begin
  Result := true;
end;

function TPackageListManager.allowedOperations(item : TNpmPackage): TNodeOperationSet;
begin
  result := [opAdd, opDelete];
end;

function TPackageListManager.loadList : boolean;
var
  resp : TFHIRLoadPackagesTaskResponse;
begin
  resp := TFHIRLoadPackagesTaskResponse.Create;
  try
    result := DoBackgroundTask(PackageCacheForm, GPackageLoaderTaskId, TFHIRLoadPackagesTaskRequest.Create(FCache.link), resp.link);
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
        result := FCache.Import(fileToBytes(PackageCacheForm.dlgOpen.FileName), PackageCacheForm.dlgOpen.FileName);
      finally
        PackageCacheForm.Cursor := crDefault;
      end;
    end;
  end
  else if (mode = 'url') then
  begin
    FCache.OnWork := PackageCacheForm.packageWork;
    if InputQuery('Fetch Package from Web', 'Enter URL:', url) then
      result := FCache.Import(TInternetFetcher.fetchUrl(url), url);
  end
  else if not FCache.packageExists(mode, '') then
  begin
    FCache.OnWork := PackageCacheForm.packageWork;
    if FCache.autoInstallPackage(mode, '') then
      result := FCache.loadPackage(mode, '');
  end;
end;

function TPackageListManager.deleteItem(item: TNpmPackage) : boolean;
begin
  FCache.remove(item.name, item.version);
  result := true;
end;


{ TPackageCacheForm }

procedure TPackageCacheForm.FormCreate(Sender: TObject);
begin
  FManager := TPackageListManager.Create;
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
  PackageRegistryForm := TPackageRegistryForm.Create(self);
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
  FManager.free;
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
  if FManager.FCache = nil then
    FManager.FCache := TFHIRPackageManager.Create(npmMode);

  if not FManager.doLoad then
    Close;
end;

function TPackageCacheForm.npmMode : TFHIRPackageManagerMode;
begin
  if rbUserMode.checked then
    result := npmModeUser
  else
    result := npmModeSystem;
end;

function TPackageCacheForm.GetCache: TFHIRPackageManager;
begin
  if FManager.FCache = nil then
  begin
    FManager.FCache := TFHIRPackageManager.Create(npmMode);
    FManager.doLoad;
  end;
  result := FManager.FCache;
end;

procedure TPackageCacheForm.rbSystemChange(Sender: TObject);
begin
  if FManager.FCache.Mode <> npmMode then
  begin
    FManager.FCache.free;
    FManager.FCache := TFHIRPackageManager.Create(npmMode);
    FManager.doLoad;
  end;
end;

procedure TPackageCacheForm.rbUserModeChange(Sender: TObject);
begin
  if FManager.FCache.Mode <> npmMode then
  begin
    FManager.FCache.free;
    FManager.FCache := TFHIRPackageManager.Create(npmMode);
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

procedure TPackageCacheForm.SetCache(const Value: TFHIRPackageManager);
begin
  FManager.FCache := value;
  rbUserMode.enabled := false;
  rbSystem.enabled := false;
end;

procedure TPackageCacheForm.SetIni(AValue: TIniFile);
begin
  FIni := AValue;
  FManager.Settings := FIni;
end;

end.

