unit FHIR.Cache.PackageManagerDialog;

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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, VirtualTrees, Vcl.ExtCtrls, UITypes, Clipbrd,
  {$IFDEF NPPUNICODE}FHIR.Npp.Form, {$ENDIF}
  FHIR.Support.Exceptions, FHIR.Support.Generics, FHIR.Web.Fetcher, FHIR.Support.Stream, FHIR.Support.System, FHIR.Support.Text, FHIR.Support.Shell,
  FHIR.Cache.PackageManager, System.ImageList, Vcl.ImgList, Vcl.Menus,
  Vcl.ComCtrls;

type
  TTreeDataPointer = record
    obj : TFHIRPackageObject;
  end;
  PTreeDataPointer = ^TTreeDataPointer;

  TPackageCacheForm = class({$IFDEF NPPUNICODE} TNppForm {$ELSE} TForm {$ENDIF})
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    vtPackages: TVirtualStringTree;
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
    CurrentValidator1: TMenuItem;
    FHIRR21: TMenuItem;
    FHIRR31: TMenuItem;
    FHIRR41: TMenuItem;
    USCoreCurrentStable1: TMenuItem;
    pbDownload: TProgressBar;
    btnCancel: TButton;
    lblDownload: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure vtPackagesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtPackagesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtPackagesInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure vtPackagesAddToSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vtPackagesRemoveFromSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure btnDeleteClick(Sender: TObject);
    procedure vtPackagesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
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
  private
    FCache : TFHIRPackageManager;
    FPackages : TFslList<TFHIRPackageInfo>;
    FUserMode: boolean;
    FLoading : boolean;
    FStop : boolean;
    FGoUrl: String;
    procedure LoadPackages;
    procedure selChanged;
    procedure changeMode;
    procedure importUrl(sender : TObject; url: String);
    procedure fetchProgress(sender : TObject; progress : integer);
    function packageCheck(sender : TObject; msg : String) : boolean;
    procedure packageWork(sender : TObject; pct : integer; done : boolean; msg : String);
  public
    property UserMode : boolean read FUserMode write FUserMode;
    property GoUrl : String read FGoUrl write FGoUrl;
  end;

var
  PackageCacheForm: TPackageCacheForm;

implementation

{$R *.dfm}

uses FHIR.Cache.PackageBrowser;

procedure TPackageCacheForm.btnCancelClick(Sender: TObject);
begin
  FStop := true;
end;

procedure TPackageCacheForm.btnDeleteClick(Sender: TObject);
var
  pp, pc : PTreeDataPointer;
  p, ps : PVirtualNode;
begin
  ps := nil;
  for p in vtPackages.SelectedNodes() do
    ps := p;
  pp := vtPackages.GetNodeData(ps);
  if pp.obj is TFHIRPackageInfo then
  begin
    if (pp.obj as TFHIRPackageInfo).id = 'hl7.fhir.core' then
      MessageDlg('You cannot delete all the versions of hl7.fhir.core', mtError, [mbyes], 0)
    else if MessageDlg('Delete All versions of '+pp.obj.summary+'?', TMsgDlgType.mtConfirmation, mbYesNo, 0) = mrYes then
    begin
      FCache.remove((pp.obj as TFHIRPackageInfo).id);
      LoadPackages;
    end;
  end
  else
  begin
    pc := vtPackages.GetNodeData(ps.Parent);
    if ((pc.obj as TFHIRPackageInfo).id = 'hl7.fhir.core') and (pc.obj.childCount = 1) then
      MessageDlg('You cannot delete all the versions of hl7.fhir.core', mtError, [mbyes], 0)
    else if MessageDlg('Delete v'+(pp.obj as TFHIRPackageVersionInfo).StatedVersion+' of '+(pc.obj as TFHIRPackageInfo).id+'?', mtConfirmation, mbYesNo, 0) = mrYes then
    begin
      FCache.remove((pc.obj as TFHIRPackageInfo).id, (pp.obj as TFHIRPackageVersionInfo).StatedVersion);
      LoadPackages;
    end;
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
var
  fetch : TInternetFetcher;
  ok : boolean;
  aborted : boolean;
  s : String;
begin
  FStop := false;
  pbDownload.Visible := true;
  pbDownload.Position := 0;
  lblDownload.Visible := true;
  lblDownload.Caption := 'Installing '+url;
  lblFolder.visible := false;
  btnCancel.Visible := true;
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
        FCache.Import(fetch.Buffer.AsBytes);
        LoadPackages;
      end;
    finally
      fetch.Free;
    end;
  finally
    pbDownload.Visible := false;
    btnCancel.Visible := false;
    lblDownload.Visible := false;
    lblFolder.visible := true;
  end;
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
    PackageFinderForm.Free;
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
  pbDownload.Position := progress;
  pbDownload.Invalidate;
  Application.ProcessMessages;
  if FStop then
    abort;
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
  importUrl(nil, 'http://build.fhir.org/');
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
  LoadPackages;
  if FLoading then
    if UserMode then
      RadioButton1.Checked := true
    else
      RadioButton2.Checked := true;
  FLoading := false;
end;

procedure TPackageCacheForm.lblFolderClick(Sender: TObject);
begin
  ExecuteFolder(FCache.Folder);
end;

procedure TPackageCacheForm.LoadPackages;
begin
  if FPackages = nil then
    FPackages := TFslList<TFHIRPackageInfo>.Create;
  FPackages.Clear;
  FCache.ListPackages(All_Package_Kinds, FPackages);
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
  p : PVirtualNode;
  pp : PTreeDataPointer;
begin
  ok := false;
  for p in vtPackages.SelectedNodes() do
  begin
    pp := vtPackages.GetNodeData(p);
    if not (pp.obj is TFHIRPackageDependencyInfo) then
      ok := true;
  end;
  btnDelete.Enabled := ok;
end;

procedure TPackageCacheForm.USCoreCurrentStable1Click(Sender: TObject);
begin
  importUrl(nil, 'http://hl7.org/fhir/us/core');
end;

procedure TPackageCacheForm.vtPackagesAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  selChanged;
end;

procedure TPackageCacheForm.vtPackagesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;  var Ghosted: Boolean; var ImageIndex: Integer);
var
  p : PTreeDataPointer;
begin
  if kind in [ikNormal, ikSelected] then
  begin
    p := vtPackages.GetNodeData(Node);
    if (p.obj is TFHIRPackageDependencyInfo) then
      ImageIndex := ord((p.obj as TFHIRPackageDependencyInfo).status)-1
    else if (p.obj is TFHIRPackageVersionInfo) then
      ImageIndex := 5
    else if (p.obj as TFHIRPackageInfo).id = 'hl7.fhir.core' then
      ImageIndex := 3
    else
      ImageIndex := 4;
  end
  else
    ImageIndex := -1;
end;

procedure TPackageCacheForm.vtPackagesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  p : PTreeDataPointer;
begin
  p := vtPackages.GetNodeData(Node);
  CellText := p.obj.summary;
end;

procedure TPackageCacheForm.vtPackagesInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  p, pp : PTreeDataPointer;
begin
  p := vtPackages.GetNodeData(Node);
  ChildCount := p.obj.childCount;
end;

procedure TPackageCacheForm.vtPackagesInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  p, pp : PTreeDataPointer;
begin
  p := vtPackages.GetNodeData(Node);
  if ParentNode = nil then
    p.obj := FPackages[Node.Index]
  else
  begin
    pp := vtPackages.GetNodeData(parentNode);
    if pp.obj is TFHIRPackageInfo then
      p.obj := (pp.obj as TFHIRPackageInfo).versions[Node.Index]
    else
      p.obj := (pp.obj as TFHIRPackageVersionInfo).dependencies[Node.Index]
  end;
  if p.obj.childCount > 0 then
    InitialStates := [ivsHasChildren, ivsExpanded];
end;

procedure TPackageCacheForm.vtPackagesRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  selChanged;
end;

end.
