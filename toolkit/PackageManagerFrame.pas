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
  FHIR.Support.Exceptions, FHIR.Support.Generics, FHIR.Support.System, FHIR.Support.Stream, FHIR.Support.Text, FHIR.Web.Fetcher, FHIR.Support.Osx,
  FHIR.Cache.PackageManager;

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
    tvPackages: TTreeView;
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
    procedure rbUserChange(Sender: TObject);
    procedure rbSystemChange(Sender: TObject);
    procedure tvPackagesClick(Sender: TObject);
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
    FPackages : TFslList<TFHIRPackageInfo>;
    procedure changePackageManagerMode(mode : boolean);
    procedure reloadPackages;
    procedure fetchProgress(sender : TObject; progress : integer);
    procedure importUrl(sender : TObject; url : String);
    function CheckImp(sender : TObject; msg : String) : boolean;
  public
    Destructor Destroy; override;

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
  p : TFHIRPackageInfo;
  v : TFHIRPackageVersionInfo;
begin
  if tvPackages.Selected.TagObject is TFHIRPackageInfo then
  begin
    p := tvPackages.Selected.TagObject as TFHIRPackageInfo;
    if (p.id = 'hl7.fhir.core') then
      MessageDlg('You cannot delete all the versions of hl7.fhir.core',TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0)
    else if MessageDlg('Delete All versions of '+p.summary+'?', TMsgDlgType.mtConfirmation, mbYesNo, 0) = mrYes then
    begin
      FPcm.remove(p.id);
      reloadPackages;
    end;
  end
  else
  begin
    if tvPackages.Selected.TagObject is TFHIRPackageVersionInfo then
    begin
      v := tvPackages.Selected.TagObject as TFHIRPackageVersionInfo;
      p := tvPackages.Selected.ParentItem.TagObject as TFHIRPackageInfo;
    end
    else
    begin
      v := tvPackages.Selected.ParentItem.TagObject as TFHIRPackageVersionInfo;
      p := tvPackages.Selected.ParentItem.ParentItem.TagObject as TFHIRPackageInfo;
    end;
    if (p.id = 'hl7.fhir.core') and (p.versions.Count = 1) then
      MessageDlg('You cannot delete all the versions of hl7.fhir.core',TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0)
    else if MessageDlg('Delete v'+v.StatedVersion+' of '+p.id+'?', TMsgDlgType.mtConfirmation, mbYesNo, 0) = mrYes then
    begin
      FPcm.remove(p.id, v.StatedVersion);
      reloadPackages;
    end;
  end;
end;

procedure TPackageManagerFrame.btnFindClick(Sender: TObject);
begin
  PackageFinderForm := TPackageFinderForm.create(self);
  try
    PackageFinderForm.OnLoadUrl := importUrl;
    PackageFinderForm.ShowModal;
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
    importUrl(nil, url);
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
  pbDownload.Value := progress;
  pbDownload.Repaint;
  Application.ProcessMessages;
  if FStop then
    abort;
end;

procedure TPackageManagerFrame.importUrl(sender : TObject; url: String);
var
  fetch : TInternetFetcher;
  ok : boolean;
  aborted : boolean;
  s : String;
begin
  FStop := false;
  pbDownload.Visible := true;
  pbDownload.Value := 0;
  btnCancel.Enabled := true;
  try
    Application.ProcessMessages;
    fetch := TInternetFetcher.Create;
    try
      fetch.onProgress := fetchProgress;
      fetch.Buffer := TFslBuffer.create;
      aborted := false;
      s := '';
      ok := false;
      try
        fetch.URL := URLPath([url, 'package.tgz']);
        fetch.Fetch;
        ok := (fetch.ContentType = 'application/x-compressed') or (fetch.ContentType = 'application/octet-stream');
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
          ok := (fetch.ContentType = 'application/x-compressed') or (fetch.ContentType = 'application/octet-stream');
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
    pbDownload.Visible := false;
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
  FPackages := TFslList<TFHIRPackageInfo>.create;
  if (FPcm = nil) or (rbUser.IsChecked <> FPcm.UserMode) then
    changePackageManagerMode(rbUser.IsChecked);
end;


procedure TPackageManagerFrame.MenuItem1Click(Sender: TObject);
begin
  importUrl(nil, 'http://build.fhir.org/validator.tgz');
end;

procedure TPackageManagerFrame.MenuItem2Click(Sender: TObject);
begin
  importUrl(nil, 'http://hl7.org/fhir/us/core');
end;

procedure TPackageManagerFrame.MenuItem3Click(Sender: TObject);
begin
  importUrl(nil, 'http://hl7.org/fhir/DSTU2');
end;

procedure TPackageManagerFrame.MenuItem4Click(Sender: TObject);
begin
  importUrl(nil, 'http://hl7.org/fhir/STU3');
end;

procedure TPackageManagerFrame.MenuItem5Click(Sender: TObject);
begin
  importUrl(nil, 'http://build.fhir.org/');
end;


procedure TPackageManagerFrame.mnuOpenClick(Sender: TObject);
var
  vi : TFHIRPackageVersionInfo;
begin
  vi := (tvPackages.Selected.TagObject as TFHIRPackageVersionInfo);
  TFileLauncher.Open(vi.folder);
end;

procedure TPackageManagerFrame.pmPackagePopup(Sender: TObject);
begin
  mnuDelete.Enabled := (tvPackages.Selected <> nil) and (tvPackages.Selected.TagObject <> nil);
  mnuOpen.Enabled := mnuDelete.Enabled and (tvPackages.Selected.TagObject is TFHIRPackageVersionInfo);
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
  roots : Array [TFHIRPackageKind] of TTreeViewItem;
  function root(k : TFHIRPackageKind) : TTreeViewItem;
  begin
    if roots[k] = nil then
    begin
      roots[k] := TTreeViewItem.Create(Self);
      roots[k].Text := NAMES_TFHIRPackageKind[k];
      roots[k].ImageIndex := -1;
      roots[k].Parent := tvPackages;
      roots[k].TagObject := nil;
    end;
    result := roots[k];
  end;

  function addItem(text : String; parent : TFmxObject; img : integer; data : TObject) : TTreeViewItem;
  begin
    result := TTreeViewItem.Create(Self);
    result.Text := Text;
    result.ImageIndex := img;
    result.Parent := parent;
    result.TagObject := data;
  end;

  function imgIndexForKind(k : TFHIRPackageKind) : integer;
  begin
    case k of
      fpkNull: result := -1;
      fpkCore: result := 3;
      fpkIG: result := 4;
      fpkIGTemplate: result := 7;
      fpkTool: result := 6;
    else
      result := -1;
    end;
  end;
var
  p : TFHIRPackageInfo;
  v : TFHIRPackageVersionInfo;
  d : TFHIRPackageDependencyInfo;
  pi, vi : TTreeViewItem;
  a : TFHIRPackageKind;
begin
  FPackages.Clear;
  tvPackages.Clear;
  btnDeletePackage.Enabled := false;
  FPcm.ListPackages(All_Package_Kinds, FPackages);

  for a := low(TFHIRPackageKind) to high(TFHIRPackageKind) do
    roots[a] := nil;
  for p in FPackages do
  begin
    pi := addItem(p.summary, root(p.kind), imgIndexForKind(p.kind), p);
    for v in p.versions do
    begin
      vi := addItem(v.summary, pi, 5, v);
      for d in v.dependencies do
        case d.status of
          stUnknown: addItem(d.summary, vi, -1, d);
          stOK: addItem(d.summary, vi, 0, d);
          stMoreRecentVersion: addItem(d.summary, vi, 1, d);
          stNotResolved: addItem(d.summary, vi, 2, d);
        end;
    end;
  end;
  tvPackages.ExpandAll;
end;

procedure TPackageManagerFrame.tvPackagesClick(Sender: TObject);
begin
  btnDeletePackage.Enabled := (tvPackages.Selected <> nil) and (tvPackages.Selected.TagObject <> nil);
end;

end.
