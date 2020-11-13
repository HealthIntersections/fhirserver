unit FHIR.Npm.Manager;

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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, VirtualTrees, Vcl.ExtCtrls, UITypes, Vcl.Clipbrd,
  {$IFDEF NPPUNICODE}FHIR.Npp.Form, {$ENDIF}
  fsl_base, fsl_stream, fsl_utilities, fsl_shell, fsl_threads,
  fsl_fetcher,
  System.ImageList, Vcl.ImgList, Vcl.Menus,
  FHIR.Ui.TextPresentation,
  fsl_npm_cache, fsl_npm, FHIR.Ui.WorkerTask,
  Vcl.ComCtrls;

const
  UMSG_PC = WM_USER + 1;

type
  TTreeDataPointer = record
    obj : TNpmPackageObject;
  end;
  PTreeDataPointer = ^TTreeDataPointer;

  TPackageCacheForm = class({$IFDEF NPPUNICODE} TNppForm {$ELSE} TForm {$ENDIF})
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    btnDelete: TButton;
    ImageList1: TImageList;
    dlgOpen: TOpenDialog;
    lblFolder: TLabel;
    Label1: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Button4: TButton;
    Button5: TButton;
    btnReload: TButton;
    Button7: TButton;
    pmImport: TPopupMenu;
    FHIRR21: TMenuItem;
    FHIRR31: TMenuItem;
    FHIRR41: TMenuItem;
    USCoreCurrentStable1: TMenuItem;
    pbDownload: TProgressBar;
    btnCancel: TButton;
    lblDownload: TLabel;
    vtPackages: TVirtualStringTree;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure vtPackagesAddToSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vtPackagesRemoveFromSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure btnDeleteClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure lblFolderClick(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure btnReloadClick(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CurrentValidator1Click(Sender: TObject);
    procedure FHIRR21Click(Sender: TObject);
    procedure FHIRR31Click(Sender: TObject);
    procedure FHIRR41Click(Sender: TObject);
    procedure USCoreCurrentStable1Click(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure vtPackagesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtPackagesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vtPackagesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vtPackagesHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
    procedure vtPackagesDblClick(Sender: TObject);
    procedure vtPackagesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FCache : TFHIRPackageManager;
    FPackages : TFslList<TNpmPackage>;
    FUserMode: boolean;
    FLoading : boolean;
    FStop : boolean;
    FGoUrl: String;
    FActionMessage : String;
    FSelNode : PVirtualNode;
    FProgress : TWorkingForm;
    procedure LoadPackages;
    procedure selChanged;
    procedure changeMode;
    procedure importUrl(sender : TObject; url: String);
    procedure fetchProgress(sender : TObject; progress : integer);
    function packageCheck(sender : TObject; msg : String) : boolean;
    procedure packageWork(sender : TObject; pct : integer; done : boolean; msg : String);
    procedure sortPackageVersions;
    procedure Init(var Msg: TMessage); message UMSG_PC;
  public
    property UserMode : boolean read FUserMode write FUserMode;
    property GoUrl : String read FGoUrl write FGoUrl;
  end;

  TLoadPackagesTask = class (TWorkerObject)
  private
    Form : TPackageCacheForm;
  protected
    procedure execute; override;
    function caption : String; override;
  end;

var
  PackageCacheForm: TPackageCacheForm;

implementation

{$R *.dfm}

uses FHIR.Npm.Browser;

procedure TPackageCacheForm.btnCancelClick(Sender: TObject);
begin
  FStop := true;
end;

procedure TPackageCacheForm.btnDeleteClick(Sender: TObject);
var
  p, ps : PVirtualNode;
  pck : TNpmPackage;
begin
  ps := nil;
  for p in vtPackages.SelectedNodes() do
    ps := p;
  pck := PTreeDataPointer(vtPackages.GetNodeData(ps)).obj as TNpmPackage;
  if MessageDlg('Delete '+pck.name+'#'+pck.Version+'?', mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    FCache.remove(pck.name, pck.Version);
    LoadPackages;
  end;
end;

procedure TPackageCacheForm.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TPackageCacheForm.Button2Click(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    Cursor := crHourGlass;
    try
      FCache.Import(fileToBytes(dlgOpen.FileName));
      LoadPackages;
    finally
      Cursor := crDefault;
    end;
  end;
end;

procedure TPackageCacheForm.importUrl(sender : TObject; url : String);
begin
  FStop := false;
  FActionMessage := 'Downloading '+url;
  packageWork(sender, 0, false, FActionMessage);
  try
    Application.ProcessMessages;
    if FCache.install(url) then
      LoadPackages;
  finally
    packageWork(sender, 100, true, '');
  end;
end;

procedure TPackageCacheForm.Init(var Msg: TMessage);
begin
  LoadPackages;
end;

procedure TPackageCacheForm.Button3Click(Sender: TObject);
var
  url : String;
begin
  if InputQuery('Fetch Package from Web', 'Enter URL:', url) then
    importUrl(nil, url);
end;

procedure TPackageCacheForm.Button4Click(Sender: TObject);
begin
  PackageFinderForm := TPackageFinderForm.create(self);
  try
    PackageFinderForm.OnLoadUrl := importUrl;
    PackageFinderForm.ShowModal;
  finally
    FreeAndNil(PackageFinderForm);
  end;
end;

procedure TPackageCacheForm.Button5Click(Sender: TObject);
var
  pt : TPoint;
begin
  pt.X := Panel3.Left + Button5.left;
  pt.y := Panel3.Top + Button5.top + Button5.Height;
  pt := ClientToScreen(pt);
  pmImport.Popup(pt.X, pt.Y);
end;

procedure TPackageCacheForm.btnReloadClick(Sender: TObject);
begin
  LoadPackages;
end;

procedure TPackageCacheForm.Button7Click(Sender: TObject);
begin
  Clipboard.Open;
  Clipboard.AsText := FCache.Report;
  Clipboard.Close;
end;

procedure TPackageCacheForm.changeMode;
begin
  FUserMode := not FUserMode;
  FormShow(nil);
end;

procedure TPackageCacheForm.CurrentValidator1Click(Sender: TObject);
begin
  importUrl(nil, 'http://build.fhir.org/validator.tgz');
end;

procedure TPackageCacheForm.fetchProgress(sender: TObject; progress: integer);
begin
  packageWork(sender, progress, false, FActionMessage);
end;

procedure TPackageCacheForm.FHIRR21Click(Sender: TObject);
begin
  importUrl(nil, 'http://hl7.org/fhir/DSTU2');
end;

procedure TPackageCacheForm.FHIRR31Click(Sender: TObject);
begin
  importUrl(nil, 'http://hl7.org/fhir/STU3');
end;

procedure TPackageCacheForm.FHIRR41Click(Sender: TObject);
begin
  importUrl(nil, 'http://hl7.org/fhir/R4');
end;

procedure TPackageCacheForm.FormActivate(Sender: TObject);
begin
  if GoUrl <> '' then
  begin
    importUrl(nil, GoURL);
    ModalResult := mrClose;
  end;
end;

procedure TPackageCacheForm.FormCreate(Sender: TObject);
begin
  vtPackages.NodeDataSize := sizeof(TTreeDataPointer);
  UserMode := true;
  FLoading := true;
end;

procedure TPackageCacheForm.FormDestroy(Sender: TObject);
begin
  FCache.Free;
  FPackages.Free;
end;

procedure TPackageCacheForm.FormShow(Sender: TObject);
begin
  if assigned(FCache) then
    FCache.Free;
  FCache := TFHIRPackageManager.Create(UserMode);
  FCache.OnCheck := packageCheck;
  FCache.OnWork := packageWork;
  lblDownload.Visible := false;
  lblFolder.visible := true;
  lblFolder.Caption := 'Cache: '+FCache.Folder;
  lblFolder.Refresh;
  Caption := 'FHIR Package Cache Manager - '+FCache.description;
  if FLoading then
    if UserMode then
      RadioButton1.Checked := true
    else
      RadioButton2.Checked := true;
  FLoading := false;
  PostMessage(handle, UMSG_PC, 0, 0);
end;

procedure TPackageCacheForm.lblFolderClick(Sender: TObject);
begin
  ExecuteFolder(FCache.Folder);
end;

procedure TPackageCacheForm.LoadPackages;
var
  task : TLoadPackagesTask;
begin
  if FPackages = nil then
    FPackages := TFslList<TNpmPackage>.Create;
  FPackages.Clear;
  task := TLoadPackagesTask.Create;
  try
    task.Form := self;
    task.runTask(self);
  finally
    task.Free;
  end;
  sortPackageVersions;
  vtPackages.RootNodeCount := 0;
  vtPackages.RootNodeCount := FPackages.Count;
  vtPackages.Invalidate;
end;

function TPackageCacheForm.packageCheck(sender : TObject; msg: String): boolean;
begin
  result := MessageDlg(msg, mtConfirmation, mbYesNo, 0) = mrYes;
end;

procedure TPackageCacheForm.packageWork(sender: TObject; pct: integer; done: boolean; msg: String);
begin
  if (PackageFinderForm <> nil) then
  begin
    PackageFinderForm.packageWork(sender, pct, done, msg);
  end
  else
  begin
    if done then
    begin
      pbDownload.Visible := false;
      btnCancel.Visible := false;
      lblDownload.Visible := false;
      lblFolder.visible := true;
    end
    else
    begin
      if not pbDownload.Visible then
      begin
        FStop := false;
        pbDownload.Visible := true;
        pbDownload.Position := 0;
        lblDownload.Visible := true;
        lblFolder.visible := false;
        btnCancel.Visible := true;
      end;
      lblDownload.caption := msg;
      pbDownload.Position := pct;
      if FStop then
        abort;
    end;
  end;
  Application.ProcessMessages;
end;

procedure TPackageCacheForm.Panel1Click(Sender: TObject);
begin
  TFileLauncher.Open(FCache.Folder);
end;

procedure TPackageCacheForm.RadioButton2Click(Sender: TObject);
begin
  if not FLoading then
    changeMode;
end;

procedure TPackageCacheForm.selChanged;
var
  ok : boolean;
  pp : PTreeDataPointer;
begin
  ok := false;
  if FSelNode <> nil then
  begin
    pp := vtPackages.GetNodeData(FSelNode);
    if (pp.obj is TNpmPackage) then
      ok := true;
  end;
  btnDelete.Enabled := ok;
end;

procedure TPackageCacheForm.sortPackageVersions;
begin
  FPackages.SortF(function (const l, r: TNpmPackage): Integer
    begin
      case vtPackages.Header.SortColumn of
        0: result := CompareStr(l.name, r.name);
        1: result := CompareStr(l.Version, r.Version);
        2: result := CompareStr(l.fhirVersion, r.fhirVersion);
        3: if r.installed > l.installed then
             result := 1
           else if r.installed < l.installed then
             result := -1
           else
             result := 0;
        4: result := l.size - r.size;
        5: result := CompareStr(l.dependencySummary, r.dependencySummary);
      else
        result := 0;
      end;
      if vtPackages.Header.SortDirection = sdDescending then
        result := 0 - result;

    end);
  vtPackages.RootNodeCount := 0;
  vtPackages.RootNodeCount := FPackages.Count;
  vtPackages.Invalidate;
end;

procedure TPackageCacheForm.USCoreCurrentStable1Click(Sender: TObject);
begin
  importUrl(nil, 'http://hl7.org/fhir/us/core');
end;

procedure TPackageCacheForm.vtPackagesAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FSelNode := Node;
  selChanged;
end;

procedure TPackageCacheForm.vtPackagesDblClick(Sender: TObject);
var
  p, ps : PVirtualNode;
  pck : TNpmPackage;
begin
  ps := nil;
  for p in vtPackages.SelectedNodes() do
    ps := p;
  pck := PTreeDataPointer(vtPackages.GetNodeData(ps)).obj as TNpmPackage;
  showTextPresentation(self, 'Package Information', pck.presentation);
end;

procedure TPackageCacheForm.vtPackagesRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FSelNode := nil;
  selChanged;
end;

procedure TPackageCacheForm.vtPackagesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  p : PTreeDataPointer;
begin
  if Column = 0 then
  begin
    if kind in [ikNormal, ikSelected] then
    begin
      p := vtPackages.GetNodeData(Node);
      if (p.obj as TNpmPackage).isCore then
        ImageIndex := 4
      else
        ImageIndex := 3;
    end
    else
      ImageIndex := -1;
  end;
end;

procedure TPackageCacheForm.vtPackagesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  p : PTreeDataPointer;
  v : TNpmPackage;
begin
  p := vtPackages.GetNodeData(Node);
  v := p.obj as TNpmPackage;
  case column of
    0: CellText := v.name;
    1: CellText := v.Version;
    2: CellText := v.fhirVersion;
    3: if v.installed = 0 then CellText := '??' else CellText :=  DescribePeriod(now - v.installed);
    4: CellText := DescribeBytes(v.size);
    5: CellText := v.dependencySummary;
  end;
end;

procedure TPackageCacheForm.vtPackagesHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Column = vtPackages.Header.SortColumn then
    if vtPackages.Header.SortDirection = sdDescending then
      vtPackages.Header.SortDirection := sdAscending
    else
      vtPackages.Header.SortDirection := sdDescending
  else
  begin
    vtPackages.Header.SortColumn := HitInfo.Column;
    vtPackages.Header.SortDirection := sdAscending;
  end;
  sortPackageVersions;
end;

procedure TPackageCacheForm.vtPackagesInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  p : PTreeDataPointer;
begin
  p := vtPackages.GetNodeData(Node);
  p.obj := FPackages[Node.Index];
  InitialStates := [];
end;

procedure TPackageCacheForm.vtPackagesKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
  if Key = VK_DELETE then
    btnDeleteClick(self);
end;

{ TLoadPackagesTask }

function TLoadPackagesTask.caption: String;
begin
  result := 'Loading Packages';
end;

procedure TLoadPackagesTask.execute;
var
  oe : TWorkProgressEvent;
begin
  oe := Form.FCache.OnWork;
  try
    Form.FCache.OnWork := progress;
    Form.FCache.ListPackages(All_Package_Kinds, Form.FPackages);
  finally
    Form.FCache.OnWork := oe;
  end;
end;

end.
