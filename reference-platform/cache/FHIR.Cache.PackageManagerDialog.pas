unit FHIR.Cache.PackageManagerDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, VirtualTrees, Vcl.ExtCtrls, UITypes,
  {$IFDEF NPPUNICODE}NppForms, {$ENDIF}
  FHIR.Support.Generics, FHIR.Web.Fetcher, FHIR.Support.Stream, FHIR.Support.System, FHIR.Support.Text,
  FHIR.Support.Shell,
  FHIR.Cache.PackageManager, System.ImageList, Vcl.ImgList;

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
  private
    FCache : TFHIRPackageManager;
    FPackages : TFslList<TFHIRPackageInfo>;
    FUserMode: boolean;
    FLoading : boolean;
    procedure LoadPackages;
    procedure selChanged;
    procedure changeMode;
  public
    property UserMode : boolean read FUserMode write FUserMode;
  end;

var
  PackageCacheForm: TPackageCacheForm;

implementation

{$R *.dfm}

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
      FCache.Import(fileToBytes(dlgOpen.FileName),
        function (msg : String) : boolean
        begin
          result := MessageDlg(msg, mtConfirmation, mbYesNo, 0) = mrYes;
        end);
      LoadPackages;
    finally
      Cursor := crDefault;
    end;
  end;
end;

procedure TPackageCacheForm.Button3Click(Sender: TObject);
var
  url : String;
  fetch : TInternetFetcher;
  ok : boolean;
begin
  if InputQuery('Fetch Package from Web', 'Enter URL:', url) then
  begin
    Cursor := crHourGlass;
    try
      fetch := TInternetFetcher.Create;
      try
        fetch.Buffer := TFslBuffer.create;
        ok := false;
        try
          fetch.URL := URLPath([url, 'package.tgz']);
          fetch.Fetch;
          ok := fetch.ContentType = 'application/x-compressed';
        except
        end;
        if not ok then
        begin
          try
            fetch.URL := url;
            fetch.Fetch;
            ok := fetch.ContentType = 'application/x-compressed';
          except
          end;
        end;
        if not ok then
          raise Exception.Create('Unable to find package for '+url);
        FCache.Import(fetch.Buffer.AsBytes,
          function (msg : String) : boolean
          begin
            result := MessageDlg(msg, mtConfirmation, mbYesNo, 0) = mrYes;
          end);
        LoadPackages;
      finally
        fetch.Free;
      end;
    finally
      Cursor := crDefault;
    end;
  end;
end;

procedure TPackageCacheForm.changeMode;
begin
  FUserMode := not FUserMode;
  FormShow(nil);
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
