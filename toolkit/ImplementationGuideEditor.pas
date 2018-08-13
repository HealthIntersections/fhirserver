unit ImplementationGuideEditor;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.types,
  System.UITypes,
  FHIR.Base.Objects,
  FHIR.R4.types,
  FHIR.R4.Resources,
  FHIR.R4.Utilities,
  FHIR.Support.Utilities,
  JclSysUtils,
  System.ImageList, FMX.types, FMX.ScrollBox, System.Classes,
  FMX.Controls.Presentation, FMX.DateTimeCtrls, FMX.Edit,
  FMX.Memo, FMX.TreeView, FMX.Dialogs, FMX.DialogService,
  FMX.StdCtrls, FMX.ListBox, FMX.Layouts, FMX.TabControl,
  FMX.Menus, FMX.WebBrowser, FMX.Controls, FMX.Forms, IGSettings, FMX.ImgList,

  shellapi, winapi.windows, FMX.platform.win,
  BaseResourceFrame,
  FMX.ComboEdit;

type
  TFrame = TBaseResourceFrame;

  TImplementationGuideEditorFrame = class(TFrame)
    ImageList1: TImageList;
    ImageList2: TImageList;
    ImageList3: TImageList;
    ImageList4: TImageList;
    OpenDialog2: TOpenDialog;
    PopupMenu1: TPopupMenu;
    DeleteItem: TMenuItem;
    SaveDialog1: TSaveDialog;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    btnRender: TCornerButton;
    Button1: TButton;
    OpenDocDialog: TOpenDialog;
    Panel2: TPanel;
    Panel4: TPanel;
    Label1: TLabel;
    tvStructure: TTreeView;
    StyleBook2: TStyleBook;
    Panel3: TPanel;
    TabControl2: TTabControl;
    Intro: TTabItem;
    btnSave: TButton;
    Open: TButton;
    Button3: TButton;
    Memo9: TMemo;
    tbDefinition: TTabItem;
    ScrollBox4: TScrollBox;
    ScrollBox5: TScrollBox;
    Button4: TButton;
    Panel8: TPanel;
    CornerButton7: TCornerButton;
    ActorUp: TCornerButton;
    ActorDown: TCornerButton;
    CornerButton4: TCornerButton;
    CornerButton5: TCornerButton;
    btnNewPage: TCornerButton;
    tbIGuide: TTabItem;
    ScrollBox2: TScrollBox;
    UpdateImplementationGuide: TButton;
    Panel6: TPanel;
    btnDefinition: TCornerButton;
    btnResource: TCornerButton;
    btnProcess: TCornerButton;
    CornerButton12: TCornerButton;
    ScrollBox6: TScrollBox;
    Button5: TButton;
    CheckBox1: TCheckBox;
    ComboBox2: TComboBox;
    Edit7: TEdit;
    Edit8: TEdit;
    edtESid: TEdit;
    edtESName: TEdit;
    edtESURL: TEdit;
    Label14: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Memo3: TMemo;
    DateEdit1: TDateEdit;
    Label52: TLabel;
    ComboBox8: TComboBox;
    Edit3: TEdit;
    Label23: TLabel;
    Button9: TButton;
    tbPackage: TTabItem;
    Panel9: TPanel;
    CornerButton11: TCornerButton;
    packageUp: TCornerButton;
    packageDown: TCornerButton;
    ScrollBox8: TScrollBox;
    DeletePackage: TButton;
    Edit12: TEdit;
    Memo2: TMemo;
    UpdatePackage: TButton;
    Name: TLabel;
    Description: TLabel;
    tbPage: TTabItem;
    Panel11: TPanel;
    CornerButton9: TCornerButton;
    CornerButton8: TCornerButton;
    ScrollBox10: TScrollBox;
    btnPause: TCornerButton;
    Label53: TLabel;
    Label54: TLabel;
    Edit5: TEdit;
    Label55: TLabel;
    ComboBox9: TComboBox;
    Button6: TButton;
    Button7: TButton;
    Edit4: TEdit;
    Edit1: TEdit;
    UpdatePage: TButton;
    tbResource: TTabItem;
    ScrollBox3: TScrollBox;
    Edit6: TEdit;
    Edit9: TEdit;
    Edit11: TEdit;
    CheckBox2: TCheckBox;
    ComboBox1: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo4: TMemo;
    Label5: TLabel;
    UpdateResource: TButton;
    Panel10: TPanel;
    bnStep: TCornerButton;
    CornerButton10: TCornerButton;
    resourceUp: TCornerButton;
    resourceDown: TCornerButton;
    Splitter1: TSplitter;
    NewImplementationGuide: TButton;
    pageUp: TCornerButton;
    pageDown: TCornerButton;
    CornerButton1: TCornerButton;
    OpenDialog1: TOpenDialog;
    AutoPreview: TCheckBox;
    Button2: TButton;
    Button8: TButton;
    OpenDialog3: TOpenDialog;
    CornerButton2: TCornerButton;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Edit2: TEdit;
    Edit10: TEdit;
    Label9: TLabel;
    function addTVItem(TreeView: TTreeView; parent: TTreeViewItem; itemType, text: string; obj: tFHIRObject): TTreeViewItem;
    procedure packageUpClick(Sender: TObject);
    procedure packageDownClick(Sender: TObject);
    procedure ReloadTreeview(sel_item: TTreeViewItem);
    procedure showTab(obj: tFHIRObject);
    procedure ActorDownClick(Sender: TObject);
    procedure ActorUpClick(Sender: TObject);
    procedure btnDefinitionClick(Sender: TObject);
    procedure btnNewPageClick(Sender: TObject);
    procedure btnProcess2Click(Sender: TObject);
    procedure tvStructureChange(Sender: TObject);
    procedure DeleteItemClick(Sender: TObject);
    function SaveAs: integer;
    procedure UpdateImplementationGuideClick(Sender: TObject);
    procedure UpdatePageClick(Sender: TObject);
    procedure NewImplementationGuideClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnSaveClick(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Edit4DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure Edit4DragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure Button6Click(Sender: TObject);
    procedure CornerButton4Click(Sender: TObject);
    procedure UpdatePackageClick(Sender: TObject);
    procedure UpdateResourceClick(Sender: TObject);
    procedure CornerButton5Click(Sender: TObject);
    procedure runAndWait(Path, command, parameters: String);
    procedure autopreviewClick(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure pageUpClick(Sender: TObject);
    procedure pageDownClick(Sender: TObject);
    procedure resourceUpClick(Sender: TObject);
    procedure resourceDownClick(Sender: TObject);
    procedure CornerButton1Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure CornerButton2Click(Sender: TObject);
    procedure CornerButton10Click(Sender: TObject);
    procedure CornerButton9Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    igrootfolder, igcontentfolder, pagecontentfolder, mediafolder,
    // pandocfolder,
    tempfolder: string;
    procedure ConcatenateFiles(const InFileNames: array of string; const OutFileName: string);
    Destructor Destroy; override;
    procedure load; override;
    procedure commit; override;
    procedure cancel; override;
  end;

var
  ImplementationGuideEditorFrame: TImplementationGuideEditorFrame;
  // resource: TFHIRImplementationGuide;
  IGFileName, BaseTemplateFolder, IGPublisherFolder, binaryfolder: string;
  internal: boolean;
  resourceIsDirty: boolean;
  foldersDefined: boolean;

implementation

{$R *.fmx}

uses Import2html;

procedure TImplementationGuideEditorFrame.cancel;
begin
end;

procedure TImplementationGuideEditorFrame.commit;
begin
  { if tvStructure.Selected = tvMetadata then
    CommitMetadata;
    if tvStructure.Selected = tvSDC then
    CommitSDC;
  } resourceIsDirty := true;

end;

Destructor TImplementationGuideEditorFrame.Destroy;
begin
  inherited;
end;

procedure TImplementationGuideEditorFrame.Edit4DragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  Edit4.text := TTabItem(Data.source).text;
end;

procedure TImplementationGuideEditorFrame.Edit4DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  Operation := TDragOperation.Link;
end;

/// ////////////////////////////////////////////////////////////////////////////
// 1. 1 recursive procedure to add a FHIRObject to a treeview
// each part must know what to recurse - e.g. if it is an exampleScenario it must check the actors, resources, and processes
// so we could scan this from the structureDefinition
/// ////////////////////////////////////////////////////////////////////////////

function TImplementationGuideEditorFrame.addTVItem(TreeView: TTreeView; parent: TTreeViewItem; itemType, text: string; obj: tFHIRObject): TTreeViewItem;
var
  current_item, Item: TTreeViewItem;
  i: integer;
  objtype: string;
begin

  Item := TTreeViewItem.Create(TreeView);
  objtype := obj.fhirType;
  TreeView.AddObject(Item);
  if (parent = nil) then
    Item.parent := TreeView
  else
    Item.parent := parent;

  Item.ImageIndex := -1;
  if itemType = 'page' then
    Item.ImageIndex := 9;
  if itemType = 'manifest' then
    Item.ImageIndex := 10;
  if itemType = 'definition' then
    Item.ImageIndex := 11;
  if itemType = 'resource' then
    Item.ImageIndex := 12;
  if itemType = 'package' then
    Item.ImageIndex := 15;
  if itemType = 'implementationguide' then
    Item.ImageIndex := 14;

  Item.StylesData['attrType'] := itemType;
  Item.StylesData['sortOrder'] := 1;
  TreeView.Selected := Item;
  Item.Height := 20;
  Item.text := text;
  Item.tagObject := obj;

  Item.IsExpanded := true;

  // do this for the object - any object type:
  if obj is tfhirImplementationGuide then
  begin
    if text <> '' then
      tab.text := text;
    current_item := Item;
    for i := 0 to tfhirImplementationGuide(obj).dependsOnList.Count - 1 do
      addTVItem(TreeView, current_item, 'Depends On', tfhirImplementationGuide(obj).dependsOnList[i].version, tfhirImplementationGuide(obj).dependsOnList[i]);

    for i := 0 to tfhirImplementationGuide(obj).globalList.Count - 1 do
      addTVItem(TreeView, current_item, 'Global', tfhirImplementationGuide(obj).globalList[i].profile, tfhirImplementationGuide(obj).globalList[i]);

    if tfhirImplementationGuide(obj).definition <> nil then
    begin
      addTVItem(TreeView, current_item, 'definition', 'Definition', tfhirImplementationGuide(obj).definition);
    end;

    if tfhirImplementationGuide(obj).manifest <> nil then
    begin
      addTVItem(TreeView, current_item, 'manifest', 'Manifest', tfhirImplementationGuide(obj).manifest);
    end;

  end;

  if obj is tfhirImplementationGuideDefinition then
  begin
    current_item := Item;

    for i := 0 to tfhirImplementationGuideDefinition(obj).packageList.Count - 1 do
      addTVItem(TreeView, current_item, 'package', tfhirImplementationGuideDefinition(obj).packageList[i].Name, tfhirImplementationGuideDefinition(obj).packageList[i]);

    for i := 0 to tfhirImplementationGuideDefinition(obj).resourceList.Count - 1 do
      addTVItem(TreeView, current_item, 'resource', tfhirImplementationGuideDefinition(obj).resourceList[i].Name, tfhirImplementationGuideDefinition(obj).resourceList[i]);

    if tfhirImplementationGuideDefinition(obj).page <> nil then
    begin
      addTVItem(TreeView, current_item, 'page', tfhirImplementationGuideDefinition(obj).page.title, tfhirImplementationGuideDefinition(obj).page);
    end;

    for i := 0 to tfhirImplementationGuideDefinition(obj).parameterList.Count - 1 do
      addTVItem(TreeView, current_item, 'parameter', tfhirImplementationGuideDefinition(obj).parameterList[i].value, tfhirImplementationGuideDefinition(obj).parameterList[i]);

  end;

  if obj is tfhirImplementationGuideDefinitionPage then
  begin
    current_item := Item;

    for i := 0 to tfhirImplementationGuideDefinitionPage(obj).pageList.Count - 1 do
    begin
      addTVItem(TreeView, current_item, 'page', tfhirImplementationGuideDefinitionPage(obj).pageList[i].title, tfhirImplementationGuideDefinitionPage(obj).pageList[i]);
    end;

  end;

  result := Item;

end;

procedure TImplementationGuideEditorFrame.autopreviewClick(Sender: TObject);
begin
  AutoPreview.IsChecked := not AutoPreview.IsChecked;

end;

procedure TImplementationGuideEditorFrame.FormShow(Sender: TObject);
begin

  // binfolder := getCurrentDir;

  if tvStructure.globalCount = 0 then
  begin
    if not Assigned(resource) then
      NewImplementationGuideClick(Self);
  end;

  TabControl1.TabHeight := 1;
  TabControl2.TabHeight := 1;

  // form1.Caption:= inttostr(trunc(double(now)));
  // if trunc(double(now)) > 43275 then begin
  // ShowMessage('This is a preview. Please check the FHIR toolkit for official release and updates.');
  // close;
  // end;
end;

procedure TImplementationGuideEditorFrame.packageDownClick(Sender: TObject);
var
  idx: integer;
begin

  idx := tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).packageList.IndexOf(tfhirImplementationGuideDefinitionPackage(tvStructure.Selected.tagObject));

  if idx < tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).packageList.Count - 1 then
  begin
    tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).packageList.Exchange(idx, idx + 1);
    tvStructure.Selected.Index := tvStructure.Selected.Index + 1;
    resourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.packageUpClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).packageList.IndexOf(tfhirImplementationGuideDefinitionPackage(tvStructure.Selected.tagObject));
  if idx > 0 then
  begin
    tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).packageList.Exchange(idx, idx - 1);
    tvStructure.Selected.Index := tvStructure.Selected.Index - 1;
    resourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.pageDownClick(Sender: TObject);
var
  idx: integer;
begin

  if not(tvStructure.Selected.ParentItem.tagObject is tfhirImplementationGuideDefinitionPage) then
    exit;

  idx := tfhirImplementationGuideDefinitionPage(tvStructure.Selected.ParentItem.tagObject).pageList.IndexOf(tfhirImplementationGuideDefinitionPage(tvStructure.Selected.tagObject));

  if idx < tfhirImplementationGuideDefinitionPage(tvStructure.Selected.ParentItem.tagObject).pageList.Count - 1 then
  begin
    tfhirImplementationGuideDefinitionPage(tvStructure.Selected.ParentItem.tagObject).pageList.Exchange(idx, idx + 1);
    tvStructure.Selected.Index := tvStructure.Selected.Index + 1;
    resourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.pageUpClick(Sender: TObject);
var
  idx: integer;
begin

  if not(tvStructure.Selected.ParentItem.tagObject is tfhirImplementationGuideDefinitionPage) then
    exit;

  idx := tfhirImplementationGuideDefinitionPage(tvStructure.Selected.ParentItem.tagObject).pageList.IndexOf(tfhirImplementationGuideDefinitionPage(tvStructure.Selected.tagObject));
  if idx > 0 then
  begin
    tfhirImplementationGuideDefinitionPage(tvStructure.Selected.ParentItem.tagObject).pageList.Exchange(idx, idx - 1);
    tvStructure.Selected.Index := tvStructure.Selected.Index - 1;
    resourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.load;
begin

  inherited;

  // binfolder := getCurrentDir;
  TabControl1.TabHeight := 1;
  TabControl2.TabHeight := 1;

  begin
    tvStructure.beginUpdate;
    try
      addTVItem(tvStructure, nil, 'implementationguide', tfhirImplementationGuide(resource).Name, resource);
    finally
      tvStructure.EndUpdate;
      // tvStructure.Repaint;
    end;
    // webbrowser1.Enabled:=false;
  end;

  tfhirImplementationGuide(resource).id:=extractfilename(changefileext(filename,''));


  application.ProcessMessages;
  tvStructure.ExpandAll;

  tvStructure.Selected := tvStructure.ItemByGlobalIndex(0);

  showTab(tFHIRObject(tFHIRObject(tvStructure.Selected.tagObject)));
  // application.ProcessMessages;

end;

procedure TImplementationGuideEditorFrame.CornerButton10Click(Sender: TObject);
var
  idx: integer;

begin
  idx := tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).resourceList.IndexOf(tfhirImplementationGuideDefinitionResource(tvStructure.Selected.tagObject));
  resourceIsDirty := true;
  tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).resourceList.Remove(idx);
  ReloadTreeview(tvStructure.Selected.ParentItem);

end;

procedure TImplementationGuideEditorFrame.CornerButton1Click(Sender: TObject);
var
  filestr: string;
  IGSettingsForm: TIGSettingsForm;
begin

  // IGFileName:=nameForSaveDialog;

  if filename <> '' then
  begin
    igrootfolder := extractfilepath(filename);
    IGFileName := extractfilename(filename);
  end;

  IGSettingsForm := TIGSettingsForm.Create(Self);

  if igrootfolder <> '' then
    if copy(igrootfolder, length(igrootfolder), 1) = '\' then
      igrootfolder := copy(igrootfolder, 1, length(igrootfolder) - 1);

  IGSettingsForm.igrootfolder := igrootfolder;
  IGSettingsForm.IGPublisherFolder := IGPublisherFolder;
  IGSettingsForm.BaseTemplateFolder := BaseTemplateFolder;
  IGSettingsForm.IGFileName := IGFileName; //

  IGSettingsForm.ShowModal;

  if IGSettingsForm.ModalResult = mrOK then
  begin
    igrootfolder := IGSettingsForm.igrootfolder;
    IGPublisherFolder := IGSettingsForm.IGPublisherFolder;
    BaseTemplateFolder := IGSettingsForm.BaseTemplateFolder;

    igcontentfolder := IGSettingsForm.igcontentfolder;
    mediafolder := IGSettingsForm.mediafolder;
    pagecontentfolder := IGSettingsForm.pagecontentfolder;
    tempfolder := IGSettingsForm.tempfolder;
    // pandocfolder := IGSettingsForm.pandocFolder;

  end;
  IGSettingsForm.Destroy;

  // TDialogService.MessageDialog('Not Implemented yet', System.UITypes.TMsgDlgType.mtInformation, [System.UITypes.TMsgDlgBtn.mbOk], System.UITypes.TMsgDlgBtn.mbOk, 0, nil, nil);

end;

procedure TImplementationGuideEditorFrame.CornerButton2Click(Sender: TObject);
var
  i: integer;
  resx: tfhirImplementationGuideDefinitionResource;
  resref: tfhirReference;
begin

  // opendialog3.Options:=[TOpenOption.ofAllowMultiSelect, TOpenOption.ofHideReadOnly];
  OpenDialog3.Options := [TOpenOption.ofAllowMultiSelect];

  if OpenDialog3.Execute then
  begin
    for i := 0 to OpenDialog3.Files.Count - 1 do
    begin
      resx := tfhirImplementationGuideDefinitionResource(tvStructure.Selected.TagObject);
      if not assigned(resx) then resx.reference:= tfhirReference.Create;
      resx.reference.reference := ChangeFileExt((extractfilename(OpenDialog3.Files[i])), '');
      resx.Name := ChangeFileExt(extractfilename(OpenDialog3.Files[i]), '');
      if igcontentfolder <> '' then
        try
          TFile.copy(OpenDialog3.Files[i], igcontentfolder + '\resources\' + extractfilename(OpenDialog3.Files[i]));
        except

        end;

//      tfhirImplementationGuide(resource).definition.resourceList.Add(resx);
      // tfhirImplementationGuideDefinition(tvStructure.Selected.parent.tagObject).resourceList.Add(resx);

    end;


    resourceIsDirty := true;
    ReloadTreeview(tvStructure.Selected);

  end;

end;

procedure TImplementationGuideEditorFrame.CornerButton4Click(Sender: TObject);
var
  package: tfhirImplementationGuideDefinitionPackage;
begin
  package := tfhirImplementationGuideDefinitionPackage.Create;
  Package.Name := 'Package';

  if tvStructure.Selected.tagObject is tfhirImplementationGuideDefinition then
    tfhirImplementationGuideDefinition(tvStructure.Selected.tagObject).packageList.Add(Package);

  resourceIsDirty := true;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.CornerButton5Click(Sender: TObject);
var
  res: tfhirImplementationGuideDefinitionResource;
  resref: tfhirReference;

begin
  res := tfhirImplementationGuideDefinitionResource.Create;
  res.Name := 'resource';
  res.reference := tfhirReference.Create;

  tfhirImplementationGuideDefinition(tvStructure.Selected.tagObject).resourceList.Add(res);
  resourceIsDirty := true;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.CornerButton9Click(Sender: TObject);
var
  idx: integer;

begin

  TDialogService.MessageDialog('Delete element and all its children?', TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      if (tvStructure.Selected.ParentItem.tagObject) is tfhirImplementationGuideDefinition then
      begin
        resourceIsDirty := true;
        tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).deleteProperty('page', tfhirImplementationGuideDefinitionPage(tvStructure.Selected.tagObject));
        ReloadTreeview(tvStructure.Selected.ParentItem);
      end

      else if (tvStructure.Selected.ParentItem.tagObject) is tfhirImplementationGuideDefinitionPage then
      begin
        idx := tfhirImplementationGuideDefinitionPage(tvStructure.Selected.ParentItem.tagObject).pageList.IndexOf(tfhirImplementationGuideDefinitionPage(tvStructure.Selected.tagObject));
        resourceIsDirty := true;
        tfhirImplementationGuideDefinitionPage(tvStructure.Selected.ParentItem.tagObject).pageList.Remove(idx);
        ReloadTreeview(tvStructure.Selected.ParentItem);
      end;

    end);

end;

procedure TImplementationGuideEditorFrame.ActorDownClick(Sender: TObject);
var
  idx: integer;
begin

end;

procedure TImplementationGuideEditorFrame.ActorUpClick(Sender: TObject);
var
  idx: integer;
begin

end;

procedure TImplementationGuideEditorFrame.btnDefinitionClick(Sender: TObject);
var
  definition: tfhirImplementationGuideDefinition;

begin
  definition := tfhirImplementationGuideDefinition.Create;
  tfhirImplementationGuide(tvStructure.Selected.tagObject).definition := definition;
  resourceIsDirty := true;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.btnNewPageClick(Sender: TObject);
var
  page: tfhirImplementationGuideDefinitionPage;
begin

  if tvStructure.Selected.tagObject is tfhirImplementationGuideDefinition then
    if tfhirImplementationGuideDefinition(tvStructure.Selected.tagObject).page <> nil then
      exit;

  page := tfhirImplementationGuideDefinitionPage.Create;
  page.title := 'Page';

  if tvStructure.Selected.tagObject is tfhirImplementationGuideDefinition then
    tfhirImplementationGuideDefinition(tvStructure.Selected.tagObject).page := page;

  if tvStructure.Selected.tagObject is tfhirImplementationGuideDefinitionPage then
    tfhirImplementationGuideDefinitionPage(tvStructure.Selected.tagObject).pageList.Add(page);

  resourceIsDirty := true;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.btnProcess2Click(Sender: TObject);
begin
end;

// 2.5 Main process
procedure TImplementationGuideEditorFrame.NewImplementationGuideClick(Sender: TObject);

var
  ImplementationGuide: tfhirImplementationGuide;
begin
  ImplementationGuide := tfhirImplementationGuide.Create;
  ImplementationGuide.Name := 'Implementation Guide';
  resource := ImplementationGuide;
  ReloadTreeview(nil);

  /// / showTab(TFHIRObject(TFHIRObject(TVStructure.Selected.TagObject)));

  // showTab(exsc);

end;

procedure TImplementationGuideEditorFrame.Button2Click(Sender: TObject);
var
  tempstr, filestr: string;
  SL: TStringList;
begin

  tempstr := igrootfolder + '\pagecontent\' + ChangeFileExt(Edit1.text, '.xml');

  filestr := tempfolder + '\tmp.html';

  SL := TStringList.Create;
  try
    SL.LoadFromFile(tempstr);
    begin
      SL.Insert(0,
        '<div xmlns="http://www.w3.org/1999/xhtml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"> <head> <meta http-equiv="Content-Type" content="text/html; charset=utf-8" /> </head>');
      SL.Append('</div>');
    end;

    begin
      SL.Insert(0, '<html><body>');
      SL.Append('</body></html>');
      SL.SaveToFile(filestr);
    end;

  finally
    SL.Free;
  end;

  filestr := stringreplace(filestr, '\', '/', [rfReplaceAll, rfIgnoreCase]);

  // webbrowser1.Navigate( filestr );

end;

procedure TImplementationGuideEditorFrame.Button6Click(Sender: TObject);
var
  tempstr, filestr: string;
  SL: TStringList;
begin

  tempstr := igrootfolder + '\pagecontent\' + ChangeFileExt(Edit1.text, '.xml');
  filestr := igrootfolder + '\tmp.html';

  SL := TStringList.Create;
  try
    SL.LoadFromFile(tempstr);
    begin
      SL.Insert(0,
        '<div xmlns="http://www.w3.org/1999/xhtml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"> <head> <meta http-equiv="Content-Type" content="text/html; charset=utf-8" /> </head>');
      SL.Append('</div>');
    end;

    begin
      SL.Insert(0, '<html><body>');
      SL.Append('</body></html>');
      SL.SaveToFile(filestr);
    end;

  finally
    SL.Free;
  end;

  filestr := stringreplace(filestr, '\', '/', [rfReplaceAll, rfIgnoreCase]);
  runAndWait(igrootfolder, 'open', filestr);
  // webbrowser1.Navigate( filestr );

end;

procedure TImplementationGuideEditorFrame.Button7Click(Sender: TObject);
var
  optionsStr, tempstr, filestr, sCmd, ExecuteFile, ParamString, StartInString: string;
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
  SL: TStringList;
  i, InsTextPos: integer;
  destfile, lResultStr: String;
  contentImport: TcontentImport;
  sa: TStringDynArray;

begin

  { igrootfolder:='C:\ImpGuide';
    igcontentfolder := igrootfolder + '\content';
    tempfolder := igcontentfolder + '\temp';
    pagecontentfolder := igcontentfolder + '\pagecontent';
    mediafolder := igcontentfolder + '\images';
  }
  IGPublisherFolder := igrootfolder + '\publish';

  If (igcontentfolder = '') or not(directoryexists(pwidechar(igcontentfolder)) and directoryexists(pwidechar(pagecontentfolder)) and directoryexists(pwidechar(mediafolder))) then
  begin
    TDialogService.MessageDialog('IG content folders not defined or not found.' + #13#10 + ' Do you want to create them?', System.UITypes.TMsgDlgType.mtConfirmation,
      [System.UITypes.TMsgDlgBtn.mbYes, System.UITypes.TMsgDlgBtn.mbNo], System.UITypes.TMsgDlgBtn.mbYes, 0,
      procedure(const AResult: TModalResult)
      begin
        case AResult of
          mrYES:
            begin
              Button9Click(nil);
            end;
          mrNo:
            begin
            end;
        end;
      end);
  end;

  If ((igcontentfolder <> '') and (IGPublisherFolder <> '') and (directoryexists(pwidechar(igcontentfolder)) and directoryexists(pwidechar(pagecontentfolder)) and
    directoryexists(pwidechar(mediafolder)))) then
  begin

    contentImport := TcontentImport.Create(Self);
    contentImport.igContentfolder:=igContentFolder;
    contentImport.igPublisherfolder:=igPublisherFolder;
    contentImport.Edit1.text := Edit1.text;
    contentImport.ShowModal;
    contentImport.Free;

    exit;

  end;

  exit;

  {
    if OpenDialog2.Execute then
    begin

    tempstr := tempfolder + '\tmp.docx';
    copyfile(pwidechar(OpenDialog2.filename), pwidechar(tempstr), false);

    optionsStr:= '--extract-media '+tempfolder+' --from=docx --to=html4 '+tempfolder+'\tmp.docx -o '+tempfolder+'\tmp.txt';
    runAndWait(pandocfolder, 'pandoc', optionsStr);

    if directoryexists(tempfolder+'\media') then begin

    sa:=TDirectory.GetFiles(tempfolder+'\media');
    for i:= 0 to Length(sa) - 1 do
    begin
    destfile:=mediafolder+'\'+extractfilename(sa[i]);
    movefile(pwidechar(sa[i]), pwidechar(destfile));
    end;
    end;

    //    delete media folder

    SL := TStringList.Create;
    try
    SL.LoadFromFile(tempfolder + '\tmp.txt');
    begin
    SL.Insert(0,
    '<div xmlns="http://www.w3.org/1999/xhtml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"> <head> <meta http-equiv="Content-Type" content="text/html; charset=utf-8" /> </head>');
    SL.Append('</div>');
    SL.SaveToFile(tempfolder + '\tmp.xml');
    end;
    begin
    SL.Insert(0, '<html><body>');
    SL.Append('</body></html>');
    SL.SaveToFile(tempfolder + '\tmp.html');
    end;
    finally
    SL.Free;
    end;

    filestr := tempfolder + '\tmp.html';
    filestr := stringreplace(filestr, '\', '/', [rfReplaceAll, rfIgnoreCase]);
    // If preview
    runAndWait(igrootfolder, filestr, filestr);
    //
    tempstr := tempfolder + '\tmp.docx';
    deletefile(pwidechar(tempstr));
    //    deletefile(pwidechar(tempfolder + '\tmp.html'));

    if Edit1.text <> '' then
    try
    filestr := tempfolder + '\tmp.xml';
    copyfile(pwidechar(filestr), pwidechar(pagecontentfolder + '\'+ Edit1.text + '.xml'), false);
    except
    end
    else
    TDialogService.MessageDialog('File not saved - page name is empty.' + #13#10 + 'To save, enter a page name and import again.',
    System.UITypes.TMsgDlgType.mtInformation, [System.UITypes.TMsgDlgBtn.mbOK], System.UITypes.TMsgDlgBtn.mbOK, 0,
    procedure(const AResult: TModalResult)
    begin
    end);
    ;

    //    TDirectory.Delete(mediafolder, True);
    TDirectory.Delete(tempfolder, True);

    SetCurrentDir(binfolder);

    end;
  }

end;

procedure TImplementationGuideEditorFrame.Button8Click(Sender: TObject);
var
  i: integer;
  resx: tfhirImplementationGuideDefinitionResource;
  resref: tfhirReference;
  extResource:TFHIRResource;
   format : TFHIRFormat;
   restype:string;
   resFileName:string;
begin
  // opendialog3.Options:=[TOpenOption.ofHideReadOnly];
  OpenDialog3.Options := [];

  if OpenDialog3.Execute then
  begin

    /// if tvStructure.Selected.TagObject then

    for i := 0 to OpenDialog3.Files.Count - 1 do
    begin
      resx := tfhirImplementationGuideDefinitionResource(tvStructure.Selected.TagObject);
      if not assigned(resx) then resx.reference:= tfhirReference.Create;

      format := ffUnspecified;

      resType:='';
      try
        extResource:=fileToResource(OpenDialog3.Files[i], format);
        resType:=extResource.fhirType;

      finally
      extresource.Free;

      end;

      resFileName:=ChangeFileExt(extractfilename(OpenDialog3.Files[i]),'');

// This bit is because the publisher expects the resource to be called [type]/name   ... from here...

if (pos('-',extractfilename(resFileName))>0) and
   ( uppercase(copy(resFileName,1,pos('-',resFileName)-1))=uppercase(resType) ) then
      resx.reference.reference := resType+'/'+
      ChangeFileExt(copy(resFileName,pos('-',resFileName)+1, length(resFileName)-pos('-',resFileName)), '')
else
// ...until here
      resx.reference.reference := resType+'/'+resFileName;


      resx.Name := ChangeFileExt(extractfilename(OpenDialog3.Files[i]), '');
      if igcontentfolder <> '' then
        try
          TFile.copy(OpenDialog3.Files[i], igcontentfolder + '\resources\' + extractfilename(OpenDialog3.Files[i]));
        except

        end;

//      tfhirImplementationGuide(resource).definition.resourceList.Add(resx);
      // tfhirImplementationGuideDefinition(tvStructure.Selected.parent.tagObject).resourceList.Add(resx);

    end;
    resourceIsDirty := true;
    ReloadTreeview(tvStructure.Selected);

  end;

end;

procedure TImplementationGuideEditorFrame.Button9Click(Sender: TObject);
// var
// dir: string;
// IGSettingsForm: TIGSettingsForm;
begin
  {
    IGSettingsForm := TIGSettingsForm.Create(Self);
    IGSettingsForm.IGPublisherFolder := IGPublisherFolder;
    IGSettingsForm.BaseIGTemplateFolder := BaseIGTemplateFolder;

    IGSettingsForm.ShowModal;
    if IGSettingsForm.ModalResult = mrOK then
    begin
    IGPublisherFolder := IGSettingsForm.IGPublisherFolder;
    BaseIGTemplateFolder := IGSettingsForm.BaseIGTemplateFolder;
    end;
    IGSettingsForm.Destroy;
  }
end;

Procedure TImplementationGuideEditorFrame.ReloadTreeview(sel_item: TTreeViewItem);
var
  current_item: TTreeViewItem;
  i: integer;
  Instance: tfhirImplementationGuide;
  sel_index: integer;
  sel_text: string;

begin
  sel_index := -1;
  sel_text := '';
  if sel_item <> nil then
    sel_index := sel_item.GlobalIndex;

  // tvStructure.BeginUpdate;

  try
    tvStructure.Clear;
    current_item := addTVItem(tvStructure, nil, 'implementationguide', 'Implementation Guide', resource);
  finally
    // tvStructure.EndUpdate;
    tvStructure.Repaint;
  end;
  // tvStructure.repaint;
  // application.ProcessMessages;
  if sel_index <> -1 then
    tvStructure.Selected := tvStructure.ItemByGlobalIndex(sel_index);
  // application.ProcessMessages;
  /// / showTab(TFHIRObject(TVStructure.Selected.TagObject));

end;

procedure TImplementationGuideEditorFrame.showTab(obj: tFHIRObject);
var
  selIndex, i, lastSlashPos: integer;
  extracted_ID:string;
  ImplementationGuide: tfhirImplementationGuide;

begin
  selIndex := 0;
  // webbrowser1.Enabled:=false;

  if obj is tfhirImplementationGuide then
  begin

    btnDefinition.enabled := true;
    if tfhirImplementationGuide(tvStructure.Selected.tagObject).definition <> nil then
      btnDefinition.enabled := false;

    selIndex := 2;
    UpdateImplementationGuide.enabled := true;
    edtESName.text := tfhirImplementationGuide(obj).Name;

    edtESURL.text := tfhirImplementationGuide(obj).url;

    LastSlashPos:=-1;
    LastSlashPos:=LastDelimiter('/',edtESURL.text);
    if LastSlashPos>=0 then
    extracted_id:= copy(edtESURL.text, lastSlashPos+1, length(edtESURL.text)-lastSlashPos);
    if LastSlashPos>=0 then
    edtESURL.text:= copy(edtESURL.text, 1, lastSlashPos);


    edtESid.text := tfhirImplementationGuide(obj).id;

  if (edtESURL.text <>'') and (copy(edtESURL.text,length(edtESURL.text),1) <>'/') then
   edtESURL.text:=edtESURL.text + '/';




  label7.text:=edtESURL.text+edtESid.text;
  label7.hint:=edtESURL.text+edtESid.text;

    if extracted_id=edtESid.text then
    edtESURL.text := copy(edtESURL.text, 1, lastSlashPos);


    ComboBox2.ItemIndex := integer(tfhirImplementationGuide(obj).status);
    if tfhirImplementationGuide(obj).experimental then
      CheckBox1.IsChecked := true
    else
      CheckBox1.IsChecked := false;
    DateEdit1.text := tfhirImplementationGuide(obj).date.toString;
    Edit8.text := tfhirImplementationGuide(obj).packageID;
    Edit7.text := tfhirImplementationGuide(obj).publisher;
    Memo3.text := tfhirImplementationGuide(obj).Description;
    Edit3.text := tfhirImplementationGuide(obj).version;
    combobox8.itemIndex := integer(tfhirImplementationGuide(obj).license);

// This needs to change - only US supported now;

    edit10.Text := 'US';


  end;

  if obj is tfhirImplementationGuideDefinition then
  begin
    selIndex := 1;
    pageUp.Visible := false;
    pageDown.Visible := false;
    btnNewPage.Visible := true;
    if tfhirImplementationGuideDefinition(tvStructure.Selected.tagObject).page <> nil then
      btnNewPage.Visible := false;

  end;

  if obj is tfhirImplementationGuideDefinitionPackage then
  begin
    selIndex := 3;
    UpdatePage.enabled := true;
    Edit12.text := tfhirImplementationGuideDefinitionPackage(obj).Name;
    Memo2.text := tfhirImplementationGuideDefinitionPackage(obj).Description;

  end;

  if obj is tfhirImplementationGuideDefinitionResource then
  begin
    selIndex := 5;
    UpdatePage.enabled := true;
    // Edit1.text := TFHIRImplementationGuideDefinitionPage(obj).name ;

    ComboBox1.Items.Clear;
    for i := 0 to tfhirImplementationGuide(resource).definition.packageList.Count - 1 do
      ComboBox1.Items.Add(tfhirImplementationGuide(resource).definition.packageList[i].Name);

    ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(tfhirImplementationGuideDefinitionResource(obj).package);

    Edit6.text := tfhirImplementationGuideDefinitionResource(obj).reference.reference;
    Edit9.text := tfhirImplementationGuideDefinitionResource(obj).Name;
    Memo4.text := tfhirImplementationGuideDefinitionResource(obj).Description;

  end;

  if obj is tfhirImplementationGuideDefinitionPage then
  begin
    pageUp.Visible := true;
    pageDown.Visible := true;
    selIndex := 4;
    UpdatePage.enabled := true;
    if tfhirImplementationGuideDefinitionPage(obj).nameElement <> nil then
    begin
      Edit1.text := (tfhirImplementationGuideDefinitionPage(obj).nameElement as tfhirURL).value;
    end
    else
      Edit1.text := '';
    // webbrowser1.Enabled:=true;
    Edit5.text := tfhirImplementationGuideDefinitionPage(obj).title;
    ComboBox9.ItemIndex := integer(tfhirImplementationGuideDefinitionPage(obj).generation);
    if AutoPreview.IsChecked then
      Button6Click(Self);

  end;

  TabControl2.tabindex := selIndex;
  for i := 0 to TabControl2.TabCount - 1 do
    TabControl2.Tabs[i].Visible := (i = selIndex);
  TabControl2.ActiveTab := TabControl2.Tabs[selIndex];

  // tabcontrol2.EndUpdate;
  TabControl2.Repaint;
end;

procedure TImplementationGuideEditorFrame.tvStructureChange(Sender: TObject);
var
  obj: tFHIRObject;
begin
  if tvStructure.Selected <> nil then
  begin
    obj := tFHIRObject(tvStructure.Selected.tagObject);
    showTab(obj);
  end;
end;

procedure TImplementationGuideEditorFrame.DeleteItemClick(Sender: TObject);
var
  prt: TTreeViewItem;
  obj: tFHIRObject;

begin

  TDialogService.MessageDialog('Delete element and all its children?', TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      resourceIsDirty := true;
      prt := tvStructure.Selected.ParentItem;
      obj := tFHIRObject(tvStructure.Selected.tagObject);

      if Sender is TFHIRObjectList then
      begin
        TFHIRObjectList(Sender).DeleteByReference(obj);
      end
      else
        tFHIRObject(tvStructure.Selected.ParentItem.tagObject).dropEmpty;

      ReloadTreeview(prt);
    end);

end;

procedure TImplementationGuideEditorFrame.MenuItem5Click(Sender: TObject);
var
  filestr, tempstr: string;

begin
  { tempstr := binfolder + '\IGPub';
    runAndWait(tempstr, '_genonce', '');
    filestr := binfolder + '\igPub\output\index.html';
    runAndWait(tempstr, filestr, '');
    filestr := stringreplace(filestr, '\', '/', [rfReplaceAll, rfIgnoreCase]);
    runAndWait(igrootfolder, 'open', filestr); }
end;

procedure TImplementationGuideEditorFrame.mnSaveClick(Sender: TObject);
begin
  if filename = '' then
  begin
    if SaveAs = -1 then
    begin
      Showmessage('Not saved');
      exit;
    end;
  end;
  resourceToFile(resource, filename, ffXml, OutputStylePretty);
  ReloadTreeview(tvStructure.ItemByGlobalIndex(0));
  showTab(resource);
end;

function TImplementationGuideEditorFrame.SaveAs: integer;
begin
  result := -1;
  if SaveDialog1.Execute then
  begin
    filename := SaveDialog1.filename;
    igrootfolder := extractfiledir(filename);
    resourceToFile(resource, filename, ffXml, OutputStylePretty);
    showTab(resource);
    result := 0;
  end;

end;

procedure TImplementationGuideEditorFrame.UpdateImplementationGuideClick(Sender: TObject);
var
  obj: tfhirImplementationGuide;
  jurisdiction:TFhirCodeableConcept;

begin
  obj := tfhirImplementationGuide(TTreeViewItem(tvStructure.Selected).tagObject);
  if obj = nil then
    exit;
  obj.Name := edtESName.text;

  if (edtESURL.text <>'') and (copy(edtESURL.text,length(edtESURL.text),1) <>'/') then
   edtESURL.text:=edtESURL.text + '/';

  (obj).url := edtESURL.text+edtESid.text;
  label7.text:=(obj).url;
  label7.hint:=(obj).url;

  (obj).status := TfHIRPublicationStatusEnum(ComboBox2.ItemIndex);
  obj.experimental := CheckBox1.IsChecked;
  obj.id := edtESid.text;
  obj.date := TDateTimeEx.makeLocal(DateEdit1.DateTime);
  obj.version := Edit3.text;
  obj.packageID := Edit8.text;
  obj.publisher := Edit7.text;
  obj.Description := Memo3.text;
  obj.license:=TFHIRspdxLicenseEnum(combobox8.itemIndex);
  tvStructure.Selected.text := obj.Name;


// this must change - at this moment nly us is upported




  if (obj).jurisdictionList.Count=0 then begin
    jurisdiction:=TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'US');
    (obj).jurisdictionList.AddItem(jurisdiction);
  end;



end;

// 6.2
procedure TImplementationGuideEditorFrame.UpdatePackageClick(Sender: TObject);
var
  obj: tfhirImplementationGuideDefinitionPackage;
begin
  obj := tfhirImplementationGuideDefinitionPackage(tvStructure.Selected.tagObject);
  if obj = nil then
    exit;
  obj.Name := Edit12.text;
  obj.Description := Memo2.text;
  tvStructure.Selected.text := obj.Name;
  ReloadTreeview(tvStructure.Selected);
  tvStructure.Selected.text := obj.Name;

end;

procedure TImplementationGuideEditorFrame.UpdatePageClick(Sender: TObject);
var
  obj: tfhirImplementationGuideDefinitionPage;
begin
  obj := tfhirImplementationGuideDefinitionPage(TTreeViewItem(tvStructure.Selected).tagObject);
  if obj = nil then
    exit;
  if (obj).nameElement = nil then
    (obj).nameElement := tfhirURL.Create;
  tfhirURL((obj).nameElement).value := Edit1.text;
  obj.generation := TFHIRGuidePageGenerationEnum(ComboBox9.ItemIndex);
  obj.title := Edit5.text;
  tvStructure.Selected.text := obj.title;

end;

procedure TImplementationGuideEditorFrame.UpdateResourceClick(Sender: TObject);
var
  obj: tfhirImplementationGuideDefinitionResource;
begin
  obj := tfhirImplementationGuideDefinitionResource(TTreeViewItem(tvStructure.Selected).tagObject);
  if obj = nil then
    exit;
  obj.reference.reference := Edit6.text;
  obj.Name := Edit9.text;
  obj.Description := Memo4.text;
  if ComboBox1.ItemIndex <> -1 then
    obj.package := ComboBox1.Items[ComboBox1.ItemIndex];
  tvStructure.Selected.text := obj.Name;

end;

procedure TImplementationGuideEditorFrame.ConcatenateFiles(const InFileNames: array of string; const OutFileName: string);
var
  i: integer;
  InStream, OutStream: TFileStream;
begin
  OutStream := TFileStream.Create(OutFileName, fmCreate);
  try
    for i := 0 to high(InFileNames) do
    begin
      InStream := TFileStream.Create(InFileNames[i], fmOpenRead);
      try
        OutStream.CopyFrom(InStream, InStream.Size);
      finally
        InStream.Free;
      end;
    end;
  finally
    OutStream.Free;
  end;
end;

procedure TImplementationGuideEditorFrame.resourceDownClick(Sender: TObject);
var
  idx: integer;
begin

  idx := tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).resourceList.IndexOf(tfhirImplementationGuideDefinitionResource(tvStructure.Selected.tagObject));

  if idx < tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).resourceList.Count - 1 then
  begin
    tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).resourceList.Exchange(idx, idx + 1);
    tvStructure.Selected.Index := tvStructure.Selected.Index + 1;
    resourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.resourceUpClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).resourceList.IndexOf(tfhirImplementationGuideDefinitionResource(tvStructure.Selected.tagObject));
  if idx > 0 then
  begin
    tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).resourceList.Exchange(idx, idx - 1);
    tvStructure.Selected.Index := tvStructure.Selected.Index - 1;
    resourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.runAndWait(Path, command, parameters: String);
var
  folderstr, filestr: string;
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
  sCmd, ExecuteFile, ParamString, StartInString: string;

begin
  folderstr := getCurrentDir;

  begin
    FillChar(SEInfo, SizeOf(SEInfo), 0);
    SEInfo.cbSize := SizeOf(TShellExecuteInfo);
    with SEInfo do
    begin
      fMask := SEE_MASK_NOCLOSEPROCESS;
      // Wnd := FmxHandleToHWND(ImplementationGuideEditor.Handle);
      lpFile := PChar(command);
      lpDirectory := PChar(Path);
      lpParameters := PChar(parameters);
      nShow := SW_SHOWNORMAL;
    end;
    if ShellExecuteEx(@SEInfo) then
    begin
      repeat
        application.ProcessMessages;
        GetExitCodeProcess(SEInfo.hProcess, ExitCode);
      until (ExitCode <> STILL_ACTIVE) or application.Terminated;
    end;
  end;

end;

end.
