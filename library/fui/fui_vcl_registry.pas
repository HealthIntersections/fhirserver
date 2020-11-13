unit FHIR.Npm.Browser;

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

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  {$IFDEF NPPUNICODE}FHIR.Npp.Form, {$ENDIF}
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  VirtualTrees,
  fsl_base, fsl_json, fsl_fetcher, fsl_utilities,
  fsl_npm_cache, fsl_npm_client, Vcl.ComCtrls, FHIR.Ui.WorkerTask,
  FHIR.Npm.VersionBrowser;

const
  UMSG_PB = WM_USER + 1;

type
  TOnLoadUrlEvent = procedure (sender : TObject; url : String) of object;

  TPackageFinderForm = class({$IFDEF NPPUNICODE} TNppForm {$ELSE} TForm {$ENDIF})
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    edtFilter: TEdit;
    Label2: TLabel;
    cbxServer: TComboBox;
    btnClose: TButton;
    btnInstall: TButton;
    Panel3: TPanel;
    grid: TVirtualStringTree;
    lblDownload: TLabel;
    pbDownload: TProgressBar;
    btnCancel: TButton;
    procedure edtFilterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure gridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure gridRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure gridAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure btnCancelClick(Sender: TObject);
    procedure cbxServerChange(Sender: TObject);
    procedure gridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure FormShow(Sender: TObject);
  private
    FLoaded : boolean;
    FList : TFslList<TFHIRPackageInfo>;
    FFiltered : TFslList<TFHIRPackageInfo>;
    FOnLoad : TOnLoadUrlEvent;
    FIndex : integer;
    FStop : boolean;
    FServer : String;
    procedure loadPackages;
    procedure applyFilter;
    function matchesFilter(pck : TFHIRPackageInfo) : boolean;
    procedure loadFromServer;
    procedure sortPackages;
    procedure Init(var Msg: TMessage); message UMSG_PB;
  public
    property OnLoadUrl : TOnLoadUrlEvent read FOnLoad write FOnLoad;
    procedure packageWork(sender : TObject; pct : integer; done : boolean; msg : String);
  end;


  TFetchingTask = class (TWorkerObject)
  private
    FList : TFslList<TFHIRPackageInfo>;
    FServer : String;
  public
    destructor Destroy; override;
  end;

  TFetchPackagesTask = class (TFetchingTask)
  protected
    procedure execute; override;
    function caption : String; override;
  end;

  TFetchPackageVersionsTask = class (TFetchingTask)
  protected
    id : string;
    procedure execute; override;
    function caption : String; override;
  end;

var
  PackageFinderForm: TPackageFinderForm;

implementation

{$R *.dfm}


procedure TPackageFinderForm.btnCancelClick(Sender: TObject);
begin
  FStop := true;
end;

procedure TPackageFinderForm.btnInstallClick(Sender: TObject);
var
  task : TFetchPackageVersionsTask;
  form : TPackageVersionChooserForm;
begin
  if FServer.Contains('build.fhir.org') then
    FOnLoad(self, FFiltered[FIndex].Url)
  else
  begin
    task := TFetchPackageVersionsTask.create;
    try
      task.FServer := FServer;
      task.id := FFiltered[FIndex].id;
      task.runTask(self);
      form := TPackageVersionChooserForm.Create(Self);
      try
        form.LoadPackages(task.FList);
        if form.ShowModal = mrOk then
          FOnLoad(self, task.FList[form.Index].Url)
      finally
        form.Free;
      end;
    finally
      task.free;
    end;
  end;
end;

procedure TPackageFinderForm.cbxServerChange(Sender: TObject);
begin
  case cbxServer.itemIndex of
    0: FServer := PACKAGE_SERVER_PRIMARY;
    1: FServer := PACKAGE_SERVER_BACKUP;
    2: FServer := PACKAGE_SERVER_CIBUILD;
  end;
  loadFromServer;
end;

procedure TPackageFinderForm.edtFilterChange(Sender: TObject);
begin
  applyFilter;
  grid.RootNodeCount := 0;
  grid.RootNodeCount := FFiltered.Count;
//  gridCellClick(nil, grid.Row);
end;

procedure TPackageFinderForm.FormCreate(Sender: TObject);
begin
  FList := TFslList<TFHIRPackageInfo>.create;
  FFiltered := TFslList<TFHIRPackageInfo>.create;
end;

procedure TPackageFinderForm.FormDestroy(Sender: TObject);
begin
  FFiltered.Free;
  FList.Free;
end;

procedure TPackageFinderForm.FormShow(Sender: TObject);
begin
  if not FLoaded then
    PostMessage(handle, UMSG_PB, 0, 0);
end;

procedure TPackageFinderForm.gridAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  btnInstall.Enabled := true;
  FIndex := Node.Index;
end;

procedure TPackageFinderForm.gridGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  pck : TFHIRPackageInfo;
begin
  pck := FFiltered[Node.Index];
  case Column of
    0: CellText := pck.Id;
    1: CellText := pck.Version;
    2: CellText := pck.Description;
    3: CellText := pck.canonical;
    4: CellText := pck.FHIRVersion;
    5: CellText := pck.presentDate;
  end;
end;

procedure TPackageFinderForm.gridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Column = grid.Header.SortColumn then
    if grid.Header.SortDirection = sdDescending then
      grid.Header.SortDirection := sdAscending
    else
      grid.Header.SortDirection := sdDescending
  else
  begin
    grid.Header.SortColumn := HitInfo.Column;
    grid.Header.SortDirection := sdAscending;
  end;
  sortPackages;
end;

procedure TPackageFinderForm.gridRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  btnInstall.Enabled := false;
end;

procedure TPackageFinderForm.Init(var Msg: TMessage);
begin
  cbxServerChange(nil);
end;

procedure TPackageFinderForm.loadFromServer;
var
  task : TFetchPackagesTask;
begin
  FList.clear;
  task := TFetchPackagesTask.create;
  try
    task.FServer := FServer;
    task.runTask(self);
    FList.addAll(task.FList);
  finally
    task.free;
  end;
  LoadPackages;
end;

procedure TPackageFinderForm.loadPackages;
begin
  FLoaded := true;
  applyFilter;
  grid.RootNodeCount := 0;
  grid.RootNodeCount := FFiltered.Count;
//  gridCellClick(nil, grid.Row);
end;

function TPackageFinderForm.matchesFilter(pck: TFHIRPackageInfo): boolean;
begin
  result :=
    pck.id.Contains(edtFilter.Text) or
    pck.Version.Contains(edtFilter.Text) or
    pck.Description.Contains(edtFilter.Text) or
    pck.FHIRVersion.Contains(edtFilter.Text) or
    pck.URL.Contains(edtFilter.Text) or
    formatDateTime('c', pck.Date).Contains(edtFilter.Text);
end;

procedure TPackageFinderForm.packageWork(sender: TObject; pct: integer; done: boolean; msg: String);
begin
  if done then
  begin
    pbDownload.Visible := false;
    btnCancel.Visible := false;
    lblDownload.Visible := false;
    btnClose.Enabled := true;
    btnInstall.Enabled := true;
  end
  else
  begin
    if not pbDownload.Visible then
    begin
      FStop := false;
      pbDownload.Visible := true;
      pbDownload.Position := 0;
      lblDownload.Visible := true;
      btnCancel.Visible := true;
      btnClose.Enabled := false;
      btnInstall.Enabled := false;
    end;
    lblDownload.caption := msg;
    pbDownload.Position := pct;
    if FStop then
      abort;
  end;
end;

procedure TPackageFinderForm.sortPackages;
begin
  FList.SortF(function (const l, r: TFHIRPackageInfo): Integer
    begin
      case grid.Header.SortColumn of
        0: result := CompareStr(l.Id, r.Id);
        1: result := CompareStr(l.Version, r.Version);
        2: result := CompareStr(l.Description, r.Description);
        3: result := CompareStr(l.canonical, r.canonical);
        4: result := CompareStr(l.FHIRVersion, r.FHIRVersion);
        5: if (l.Date > r.Date) then
             result := 1
           else if (l.Date < r.Date) then
             result := -1
           else
             result := 0;
      else
        result := 0;
      end;
      if grid.Header.SortDirection = sdDescending then
        result := 0 - result;
    end);
  applyFilter;
  grid.RootNodeCount := 0;
  grid.RootNodeCount := FFiltered.Count;
end;

procedure TPackageFinderForm.applyFilter;
var
  pck : TFHIRPackageInfo;
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

{ TFetchingTask }

destructor TFetchingTask.Destroy;
begin
  FList.Free;
  inherited;
end;

{ TFetchPackagesTask }

function TFetchPackagesTask.caption: String;
begin
  result := 'Fetching Packages';
end;

procedure TFetchPackagesTask.execute;
var
  pc : TFHIRPackageClient;
begin
  pc := TFHIRPackageClient.create(FServer);
  try
    if FServer.Contains('build.fhir.org') then
      FList := pc.fetchFromCIBuild
    else
      FList := pc.search('', '', '', false);
  finally
    pc.free;
  end;
end;

{ TFetchPackageVersionsTask }

function TFetchPackageVersionsTask.caption: String;
begin
  result := 'Fetching Package Versions';
end;

procedure TFetchPackageVersionsTask.execute;
var
  pc : TFHIRPackageClient;
begin
  pc := TFHIRPackageClient.create(FServer);
  try
    FList := pc.getVersions(id);
  finally
    pc.free;
  end;
end;

end.
