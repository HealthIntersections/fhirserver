unit PackageManagerFrame;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, System.ImageList, FMX.ImgList, FMX.Menus,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.TabControl, FMX.ListBox, FMX.Layouts, FMX.DateTimeCtrls,
  FMX.Edit, System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.Platform,
  FMX.Memo, FMX.TreeView,
  BaseFrame,
  fsl_base, fsl_utilities, fsl_stream, fsl_fetcher, FHIR.Support.Osx, FHIR.Ui.Fmx,
  fsl_npm, fsl_npm_cache, PackageEditorFrame;

const
  PCMode_User = true;
  PCMode_System = false;

type
  TFrame = TBaseFrame; // re-aliasing the Frame to work around a designer bug

  TPackageManagerFrame = class (TFrame)
    Panel2: TPanel;
    rbUser: TRadioButton;
    rbSystem: TRadioButton;
    label1: TLabel;
    Panel3: TPanel;
    btnImportPackageFile: TButton;
    btnImportPackageUrl: TButton;
    btnDeletePackage: TButton;
    Button5: TButton;
    Panel1: TPanel;
    btnCancel: TButton;
    lblFolder: TLabel;
    pbDownload: TProgressBar;
    imgPackages: TImageList;
    dlgOpenPackage: TOpenDialog;
    pmImport: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem2: TMenuItem;
    pmPackage: TPopupMenu;
    mnuOpen: TMenuItem;
    mnuDelete: TMenuItem;
    btnFind: TButton;
    btnDebug: TButton;
    btnRefresh: TButton;
    grid: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    procedure rbUserChange(Sender: TObject);
    procedure rbSystemChange(Sender: TObject);
    procedure btnImportPackageFileClick(Sender: TObject);
    procedure btnImportPackageUrlClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure btnDeletePackageClick(Sender: TObject);
    procedure lblFolderClick(Sender: TObject);
    procedure pmPackagePopup(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btnDebugClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FLoading : boolean;
    FStop : boolean;
    FPcm : TFHIRPackageManager;
    FPackages : TFslList<TNpmPackage>;
    procedure changePackageManagerMode(mode : boolean);
    procedure reloadPackages;
    procedure fetchProgress(sender : TObject; progress : integer);
    procedure importUrl(sender : TObject; url : String; pbar : TProgressBar);
    function CheckImp(sender : TObject; msg : String) : boolean;
  public
    destructor Destroy; override;

    procedure load; override;
  end;

implementation

{$R *.fmx}

uses
  OsxPopupmenuWorkaround,
  PackageBrowser;

{ TPackageManagerFrame }

procedure TPackageManagerFrame.btnCancelClick(Sender: TObject);
begin
  FStop := true;
end;

procedure TPackageManagerFrame.btnDebugClick(Sender: TObject);
var  Svc: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
  begin
    Svc.SetClipboard(FPcm.Report);
    ShowMessage('Report posted to clipboard');
  end;
end;

procedure TPackageManagerFrame.btnDeletePackageClick(Sender: TObject);
var
  p : TNpmPackage;
begin
  if grid.row > -1 then
  begin
    p := FPackages[grid.row];
    if MessageDlg('Delete '+p.name+'#'+p.version+'?', TMsgDlgType.mtConfirmation, mbYesNo, 0) = mrYes then
    begin
      FPcm.remove(p.name, p.Version);
      reloadPackages;
    end;
  end;
end;

procedure TPackageManagerFrame.btnFindClick(Sender: TObject);
begin
  PackageFinderForm := TPackageFinderForm.create(self);
  try
    PackageFinderForm.OnLoadUrl := importUrl;
    ShowModalHack(PackageFinderForm);
  finally
    PackageFinderForm.Free;
  end;
end;

function TPackageManagerFrame.CheckImp(sender : TObject; msg : String) : boolean;
begin
  result := MessageDlg(msg, TMsgDlgType.mtConfirmation, mbYesNo, 0) = mrYes;
end;


procedure TPackageManagerFrame.btnImportPackageFileClick(Sender: TObject);
begin
  if dlgOpenPackage.Execute then
  begin
    Cursor := crHourGlass;
    try
      FPcm.Import(fileToBytes(dlgOpenPackage.FileName));
      reloadPackages;
    finally
      Cursor := crDefault;
    end;
  end;
end;

procedure TPackageManagerFrame.btnImportPackageUrlClick(Sender: TObject);
var
  url : String;
begin
  url := 'https://';
  if InputQuery('Fetch Package from Web', 'Enter URL:', url) then
    importUrl(nil, url, pbDownload);
end;

procedure TPackageManagerFrame.btnRefreshClick(Sender: TObject);
begin
  reloadPackages;
end;

procedure TPackageManagerFrame.Button5Click(Sender: TObject);
var
  pt : TPointF;
  i : integer;
begin
  pt.X := 0;
  pt.Y := Button5.Height;
  pt := Button5.LocalToAbsolute(pt);
  pt := form.ClientToScreen(pt);
  {$IFDEF OSX}
  i := runPopupAlternative(self, pmImport, trunc(pt.X), trunc(pt.Y));
  if i > -1 then
    pmImport.Items[i].OnClick(self);
  {$ELSE}
  pmImport.Popup(pt.X, pt.Y);
  {$ENDIF}
end;

procedure TPackageManagerFrame.changePackageManagerMode(mode: boolean);
begin
  if (FPcm <> nil) then
    FPcm.Free;
  FPcm := TFHIRPackageManager.Create(mode);
  FPcm.OnCheck := checkImp;
  lblFolder.Text := FPcm.Folder;
  reloadPackages;
end;

destructor TPackageManagerFrame.Destroy;
begin
  FPackages.Free;
  FPcm.Free;
  inherited;
end;

procedure TPackageManagerFrame.fetchProgress(sender: TObject; progress: integer);
begin
  TProgressBar(TFslObject(sender).TagObject).Value := progress;
  TProgressBar(TFslObject(sender).TagObject).Repaint;
  Application.ProcessMessages;
  if FStop then
    abort;
end;

procedure TPackageManagerFrame.importUrl(sender : TObject; url: String; pbar : TProgressBar);
var
  fetch : TInternetFetcher;
  ok : boolean;
  aborted : boolean;
  s : String;
begin
  FStop := false;
  pbar.Visible := true;
  pbar.Value := 0;
  btnCancel.Enabled := true;
  try
    Application.ProcessMessages;
    fetch := TInternetFetcher.Create;
    try
      fetch.onProgress := fetchProgress;
      fetch.TagObject := pbar;
      fetch.Buffer := TFslBuffer.create;
      aborted := false;
      s := '';
      ok := false;
      try
        fetch.URL := URLPath([url, 'package.tgz']);
        fetch.Fetch;
        ok := (fetch.ContentType = 'application/x-compressed') or (fetch.ContentType = 'application/octet-stream') or (fetch.ContentType = 'application/x-tar');
      except
        on e : exception do
        begin
          s := e.Message;
          aborted := e is EAbort;
        end;
      end;
      if not ok then
      begin
        try
          fetch.URL := url;
          fetch.Fetch;
          ok := (fetch.ContentType = 'application/x-compressed') or (fetch.ContentType = 'application/octet-stream') or (fetch.ContentType = 'application/x-tar');
        except
          on e : exception do
          begin
            s := e.Message;
            aborted := e is EAbort;
          end;
        end;
      end;
      if not ok  and not aborted then
        raise EIOException.create('Unable to find package for '+url+': '+s);
      if ok then
      begin
        FPcm.Import(fetch.Buffer.AsBytes);
        reloadPackages;
      end;
    finally
      fetch.Free;
    end;
  finally
    pbar.Visible := false;
    btnCancel.Enabled := false;
    Cursor := crDefault;
  end;
end;

procedure TPackageManagerFrame.lblFolderClick(Sender: TObject);
begin
  TFileLauncher.Open(FPcm.Folder);
end;

procedure TPackageManagerFrame.load;
begin
  FLoading := true;
  try
    if Settings.getValue('PackageManager', 'mode', true) then
      rbUser.IsChecked := true
    else
      rbSystem.IsChecked := true
  finally
    FLoading := false;
  end;
  FPackages := TFslList<TNpmPackage>.create;
  if (FPcm = nil) or (rbUser.IsChecked <> FPcm.UserMode) then
    changePackageManagerMode(rbUser.IsChecked);
end;


procedure TPackageManagerFrame.MenuItem1Click(Sender: TObject);
begin
  importUrl(nil, 'http://build.fhir.org/validator.tgz', pbDownload);
end;

procedure TPackageManagerFrame.MenuItem2Click(Sender: TObject);
begin
  importUrl(nil, 'http://hl7.org/fhir/us/core', pbDownload);
end;

procedure TPackageManagerFrame.MenuItem3Click(Sender: TObject);
begin
  importUrl(nil, 'http://hl7.org/fhir/DSTU2', pbDownload);
end;

procedure TPackageManagerFrame.MenuItem4Click(Sender: TObject);
begin
  importUrl(nil, 'http://hl7.org/fhir/STU3', pbDownload);
end;

procedure TPackageManagerFrame.MenuItem5Click(Sender: TObject);
begin
  importUrl(nil, 'http://hl7.org/fhir/R4', pbDownload);
end;


procedure TPackageManagerFrame.mnuOpenClick(Sender: TObject);
var
  vi : TNpmPackage;
  frame: TPackageEditorFrame;
  tab : TTabItem;
begin
  vi := FPackages[grid.row];
  tab := Tabs.Add(TTabItem);
  tabs.ActiveTab := tab;
  tab.Text := ExtractFileName(vi.name+'#'+vi.version);
  frame := TPackageEditorFrame.Create(tab);
  frame.form := form;
  tab.TagObject := frame;
  frame.TagObject := tab;
  frame.Parent := tab;
  frame.tabs := tabs;
  frame.OnWork := OnWork;
  frame.Settings := Settings.Link;
  frame.tab := tab;
  frame.Align := TAlignLayout.Client;
  frame.Source := vi.loadPath;
  frame.load;
end;

procedure TPackageManagerFrame.pmPackagePopup(Sender: TObject);
begin
  mnuDelete.Enabled := (grid.row > -1);
  mnuOpen.Enabled := mnuDelete.Enabled;
end;

procedure TPackageManagerFrame.rbSystemChange(Sender: TObject);
begin
  if not FLoading and (rbSystem.IsChecked = FPcm.UserMode) then
    changePackageManagerMode(not rbSystem.IsChecked);
end;

procedure TPackageManagerFrame.rbUserChange(Sender: TObject);
begin
  if not FLoading and (rbUser.IsChecked <> FPcm.UserMode) then
    changePackageManagerMode(rbUser.IsChecked);
end;

procedure TPackageManagerFrame.reloadPackages;
var
  p : TNpmPackage;
  row : integer;
//  v : TFHIRPackageVersionInfo;
//  d : TFHIRPackageDependencyInfo;
//  pi, vi : TTreeViewItem;
//  a : TFHIRPackageKind;
begin
  FPackages.Clear;
  grid.rowCount := 0;
  FPcm.ListPackages(All_Package_Kinds, FPackages);
  grid.RowCount := FPackages.count;
  for row := 0 to FPackages.count - 1 do
  begin
    grid.cells[0, row] := FPackages[row].name;
    grid.cells[1, row] := FPackages[row].version;
    grid.cells[2, row] := FPackages[row].FhirVersionList;
    grid.cells[3, row] := DescribePeriod(now - FPackages[row].installed);
    grid.cells[4, row] := DescribeBytes(FPackages[row].size);
    grid.cells[5, row] := FPackages[row].dependencySummary;
  end;

end;

end.
