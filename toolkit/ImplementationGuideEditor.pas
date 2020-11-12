unit ImplementationGuideEditor;

{
  Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
{$IFDEF OSX}
{$ELSE}
  Shellapi, Winapi.Windows, FMX.Platform.Win, //JclSysUtils,
{$ENDIF}
  System.SysUtils,
  System.IOUtils,
  System.types,
  System.UITypes,
  fhir_objects,
  FHIR.Version.types,
  FHIR.Version.Resources,
  FHIR.Version.Utilities,
  fsl_utilities,
  System.ImageList, FMX.types, FMX.ScrollBox, System.Classes,
  FMX.Controls.Presentation, FMX.DateTimeCtrls, FMX.Edit,
  FMX.Memo, FMX.TreeView, FMX.Dialogs, FMX.DialogService,
  FMX.StdCtrls, FMX.ListBox, FMX.Layouts, FMX.TabControl,
  FMX.Menus, FMX.WebBrowser, FMX.Controls, FMX.Forms, IGPublishSettings, FMX.ImgList,

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
    OpenDocDialog: TOpenDialog;
    Panel2: TPanel;
    Panel4: TPanel;
    Label1: TLabel;
    tvStructure: TTreeView;
    StyleBook2: TStyleBook;
    Panel3: TPanel;
    TabControl2: TTabControl;
    Intro: TTabItem;
    tbDefinition: TTabItem;
    ScrollBox4: TScrollBox;
    ScrollBox5: TScrollBox;
    Panel8: TPanel;
    btnDeleteDefinition: TCornerButton;
    btnAddGrouping: TCornerButton;
    btnAddResource: TCornerButton;
    btnNewPage: TCornerButton;
    tbIGuide: TTabItem;
    ScrollBox2: TScrollBox;
    UpdateImplementationGuide: TButton;
    Panel6: TPanel;
    btnAddDefinition: TCornerButton;
    btnDependsOn: TCornerButton;
    btAddGlobal: TCornerButton;
    btnAddManifest: TCornerButton;
    ScrollBox6: TScrollBox;
    chkExperimental: TCheckBox;
    cbIGStatus: TComboBox;
    edtIGPublisher: TEdit;
    edtIGPackageId: TEdit;
    edtIGId: TEdit;
    edtIGName: TEdit;
    edtIGURL: TEdit;
    Label14: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    mmIGPurpose: TMemo;
    dtIGDate: TDateEdit;
    Label52: TLabel;
    cbIGLicense: TComboBox;
    edtIGVersion: TEdit;
    Label23: TLabel;
    tbPackage: TTabItem;
    Panel9: TPanel;
    btnDeleteGrouping: TCornerButton;
    packageUp: TCornerButton;
    packageDown: TCornerButton;
    ScrollBox8: TScrollBox;
    edtPackageName: TEdit;
    mmPackageDescription: TMemo;
    UpdatePackage: TButton;
    Name: TLabel;
    Description: TLabel;
    tbPage: TTabItem;
    Panel11: TPanel;
    btnDeletePage: TCornerButton;
    btnAddSubPage: TCornerButton;
    ScrollBox10: TScrollBox;
    Label53: TLabel;
    Label54: TLabel;
    edtPageTitle: TEdit;
    Label55: TLabel;
    cbPageGeneration: TComboBox;
    btnPagePreview: TButton;
    btnImportDoc: TButton;
    edtPageNameURL: TEdit;
    UpdatePage: TButton;
    tbResource: TTabItem;
    ScrollBox3: TScrollBox;
    edtResourceReference: TEdit;
    edtResourceName: TEdit;
    edtExampleResourceCanonical: TEdit;
    chkResourceExampleBoolean: TCheckBox;
    cbResourcePackage: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    mmResourceDescription: TMemo;
    Label5: TLabel;
    UpdateResource: TButton;
    Panel10: TPanel;
    bnStep: TCornerButton;
    btnDeleteResource: TCornerButton;
    resourceUp: TCornerButton;
    resourceDown: TCornerButton;
    Splitter1: TSplitter;
    NewImplementationGuide: TButton;
    pageUp: TCornerButton;
    pageDown: TCornerButton;
    OpenDialog1: TOpenDialog;
    AutoPreview: TCheckBox;
    btnInlinePreview: TButton;
    btnImportResource: TButton;
    OpenDialog3: TOpenDialog;
    btnImportResources: TCornerButton;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    edtIGContact: TEdit;
    edtIGJurisdiction: TEdit;
    Label9: TLabel;
    tbGlobal: TTabItem;
    Panel1: TPanel;
    btnUpdateGlobal: TCornerButton;
    btnDeleteGlobal: TCornerButton;
    btnGlobalUp: TCornerButton;
    btnGlobalDown: TCornerButton;
    ScrollBox1: TScrollBox;
    edtGlobalProfileName: TEdit;
    Label10: TLabel;
    Label13: TLabel;
    Button2: TButton;
    cbGlobalType: TComboBox;
    tbDependsOn: TTabItem;
    Panel5: TPanel;
    btnDeleteDependsOn: TCornerButton;
    btnDependsOnUp: TCornerButton;
    btnDependsOnDown: TCornerButton;
    ScrollBox7: TScrollBox;
    edtDepOnPackage: TEdit;
    Label11: TLabel;
    btnUpdateDependsOn: TCornerButton;
    edtDepOnURI: TEdit;
    Label15: TLabel;
    edtDepOnVersion: TEdit;
    Label24: TLabel;
    edtIGTitle: TEdit;
    Label12: TLabel;
    Label25: TLabel;
    lbResourceVersions: TListBox;

    function addTVItem(TreeView: TTreeView; parent: TTreeViewItem; itemType, text: string; obj: tFHIRObject): TTreeViewItem;
    procedure packageUpClick(Sender: TObject);
    procedure packageDownClick(Sender: TObject);
    procedure ReloadTreeview(sel_item: TTreeViewItem);
    procedure showTab(obj: tFHIRObject);
    procedure ActorDownClick(Sender: TObject);
    procedure ActorUpClick(Sender: TObject);
    procedure btnAddDefinitionClick(Sender: TObject);
    procedure btnNewPageClick(Sender: TObject);
    procedure tvStructureChange(Sender: TObject);
    procedure DeleteItemClick(Sender: TObject);
    function SaveAs: integer;
    procedure UpdateImplementationGuideClick(Sender: TObject);
    procedure UpdatePageClick(Sender: TObject);
    procedure NewImplementationGuideClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnSaveClick(Sender: TObject);
    procedure btnImportDocClick(Sender: TObject);
    procedure edtDroppedDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure btnPagePreviewClick(Sender: TObject);
    procedure btnAddGroupingClick(Sender: TObject);
    procedure UpdatePackageClick(Sender: TObject);
    procedure UpdateResourceClick(Sender: TObject);
    procedure btnAddResourceClick(Sender: TObject);
    procedure runAndWait(Path, command, parameters: String);
    procedure autopreviewClick(Sender: TObject);
    procedure pageUpClick(Sender: TObject);
    procedure pageDownClick(Sender: TObject);
    procedure resourceUpClick(Sender: TObject);
    procedure resourceDownClick(Sender: TObject);
    procedure btnInlinePreviewClick(Sender: TObject);
    procedure btnImportResourceClick(Sender: TObject);
    procedure btnImportResourcesClick(Sender: TObject);
    procedure btnDeleteResourceClick(Sender: TObject);
    procedure btnDeletePageClick(Sender: TObject);
    procedure btnDeleteGroupingClick(Sender: TObject);
    procedure btnDependsOnClick(Sender: TObject);
    procedure btAddGlobalClick(Sender: TObject);
    procedure btnDeleteGlobalClick(Sender: TObject);
    procedure btnUpdateGlobalClick(Sender: TObject);
    procedure btnUpdateDependsOnClick(Sender: TObject);
    procedure btnDeleteDependsOnClick(Sender: TObject);
    procedure btnGlobalUpClick(Sender: TObject);
    procedure btnDependsOnUpClick(Sender: TObject);
    procedure btnGlobalDownClick(Sender: TObject);
    procedure btnDependsOnDownClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    IGRootFolder: string;
    IGFileName: String; // Is this needed?
    IGPublisherFolder: string;
    IGContentFolder: String;
    IGMediaFolder: string;
    IGPageContentFolder: string;
    IGTempFolder: string;

    procedure ConcatenateFiles(const InFileNames: array of string; const OutFileName: string);
    destructor Destroy; override;
    procedure load; override;
    procedure commit; override;
    procedure cancel; override;

  end;

var
  ImplementationGuideEditorFrame: TImplementationGuideEditorFrame;
  // resource: TFHIRImplementationGuide;
  internal: boolean;
  resourceIsDirty: boolean;
  foldersDefined: boolean;

implementation

{$R *.fmx}

uses Import2html, fsl_base;

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
  tvStructure.DeleteChildren;
  inherited;
end;

procedure TImplementationGuideEditorFrame.edtDroppedDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  Operation := TDragOperation.Link;
end;

/// ////////////////////////////////////////////////////////////////////////////
// 1. 1 recursive procedure to add a FHIRObject to a treeview
// each part must know what to recurse - e.g. if it is an exampleScenario it must check the actors, resources, and processes
// we could scan this from the structureDefinition
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
  if itemType = 'global' then
    Item.ImageIndex := 18;
  if itemType = 'dependson' then
    Item.ImageIndex := 19;

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
      addTVItem(TreeView, current_item, 'dependson', 'DependsOn', tfhirImplementationGuide(obj).dependsOnList[i]);
    for i := 0 to tfhirImplementationGuide(obj).globalList.Count - 1 do
      addTVItem(TreeView, current_item, 'global', 'Global', tfhirImplementationGuide(obj).globalList[i]);
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
    for i := 0 to tfhirImplementationGuideDefinition(obj).groupingList.Count - 1 do
      addTVItem(TreeView, current_item, 'grouping', tfhirImplementationGuideDefinition(obj).groupingList[i].Name, tfhirImplementationGuideDefinition(obj).groupingList[i]);
    for i := 0 to tfhirImplementationGuideDefinition(obj).resourceList.Count - 1 do
      addTVItem(TreeView, current_item, 'resource', tfhirImplementationGuideDefinition(obj).resourceList[i].Name, tfhirImplementationGuideDefinition(obj).resourceList[i]);
    if tfhirImplementationGuideDefinition(obj).page <> nil then
    begin
      addTVItem(TreeView, current_item, 'page', tfhirImplementationGuideDefinition(obj).page.title, tfhirImplementationGuideDefinition(obj).page);
    end;
    for i := 0 to tfhirImplementationGuideDefinition(obj).parameterList.Count - 1 do
      addTVItem(TreeView, current_item, 'parameter', tfhirImplementationGuideDefinition(obj).parameterList[i].value, tfhirImplementationGuideDefinition(obj).parameterList[i]);
    for i := 0 to tfhirImplementationGuideDefinition(obj).templateList.Count - 1 do
      addTVItem(TreeView, current_item, 'template', tfhirImplementationGuideDefinition(obj).templateList[i].code, tfhirImplementationGuideDefinition(obj).templateList[i]);

  end;

  if obj is tfhirImplementationGuideDefinitionPage then
  begin
    current_item := Item;
    for i := 0 to tfhirImplementationGuideDefinitionPage(obj).pageList.Count - 1 do
    begin
      addTVItem(TreeView, current_item, 'page', tfhirImplementationGuideDefinitionPage(obj).pageList[i].title, tfhirImplementationGuideDefinitionPage(obj).pageList[i]);
    end;
  end;

  if obj is tfhirImplementationGuideGlobal then
  begin
    current_item := Item;
  end;

  if obj is tfhirImplementationGuideDependsOn then
  begin
    current_item := Item;
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

  idx := tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).groupingList.IndexOf(tfhirImplementationGuideDefinitionGrouping(tvStructure.Selected.tagObject));

  if idx < tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).groupingList.Count - 1 then
  begin
    tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).groupingList.Exchange(idx, idx + 1);
    tvStructure.Selected.Index := tvStructure.Selected.Index + 1;
    resourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.packageUpClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).groupingList.IndexOf(tfhirImplementationGuideDefinitionGrouping(tvStructure.Selected.tagObject));
  if idx > 0 then
  begin
    tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).groupingList.Exchange(idx, idx - 1);
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
var
  displayName: string;

begin
  inherited;

  // binfolder := getCurrentDir;
  TabControl1.TabHeight := 1;
  TabControl2.TabHeight := 1;

  begin
    tvStructure.beginUpdate;
    try
      if tfhirImplementationGuide(resource).Name = '' then
        displayName := 'Implementation Guide'
      else
        displayName := tfhirImplementationGuide(resource).Name;
      addTVItem(tvStructure, nil, 'implementationguide', displayName, resource);
    finally
      tvStructure.EndUpdate;
      // tvStructure.Repaint;
    end;
    // webbrowser1.Enabled:=false;
  end;

  tfhirImplementationGuide(resource).id := extractfilename(changefileext(filename, ''));

  application.ProcessMessages;
  tvStructure.ExpandAll;

  tvStructure.Selected := tvStructure.ItemByGlobalIndex(0);

  tvStructureChange(tvStructure.ItemByGlobalIndex(0));
  // showTab(tFHIRObject(tFHIRObject(tvStructure.Selected.tagObject)));
  // application.ProcessMessages;

end;

procedure TImplementationGuideEditorFrame.btnDeleteResourceClick(Sender: TObject);
var
  idx: integer;
  prt: TTreeViewItem;
  prtObj, obj: TObject;

begin

  TDialogService.MessageDialog('Delete element?', TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      resourceIsDirty := true;
      prt := tvStructure.Selected.ParentItem;
      prtObj := prt.tagObject;
      idx := tfhirImplementationGuideDefinition(prtObj).resourceList.IndexOf(tfhirImplementationGuideDefinitionResource(tvStructure.Selected.tagObject));
      tfhirImplementationGuideDefinition(prtObj).resourceList.Remove(idx);
      ReloadTreeview(prt);
    end);

end;

procedure TImplementationGuideEditorFrame.btnDeleteDependsOnClick(Sender: TObject);
var
  idx: integer;
  prt: TTreeViewItem;
  prtObj, obj: TObject;

begin

  TDialogService.MessageDialog('Delete element?', TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      resourceIsDirty := true;
      prt := tvStructure.Selected.ParentItem;
      prtObj := prt.tagObject;
      idx := tfhirImplementationGuide(prtObj).dependsOnList.IndexOf(tfhirImplementationGuideDependsOn(tvStructure.Selected.tagObject));
      tfhirImplementationGuide(prtObj).dependsOnList.Remove(idx);
      ReloadTreeview(prt);
    end);

end;

procedure TImplementationGuideEditorFrame.btnDeleteGlobalClick(Sender: TObject);
var
  idx: integer;
  prt: TTreeViewItem;
  prtObj, obj: TObject;

begin

  TDialogService.MessageDialog('Delete element?', TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      resourceIsDirty := true;
      prt := tvStructure.Selected.ParentItem;
      prtObj := prt.tagObject;
      idx := tfhirImplementationGuide(prtObj).globalList.IndexOf(tfhirImplementationGuideGlobal(tvStructure.Selected.tagObject));
      tfhirImplementationGuide(prtObj).globalList.Remove(idx);
      ReloadTreeview(prt);
    end);

end;

procedure TImplementationGuideEditorFrame.btnDeleteGroupingClick(Sender: TObject);
var
  idx: integer;
  prt: TTreeViewItem;
  prtObj, obj: TObject;

begin

  {
    if (prtObj) is tfhirImplementationGuideDefinition then
    begin
    tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).deleteProperty('page', tfhirImplementationGuideDefinitionPage(tvStructure.Selected.tagObject));
    end

    else if (prtObj) is tfhirImplementationGuideDefinitionPage then
    begin
    idx := tfhirImplementationGuideDefinitionPage(prtObj).pageList.IndexOf(tfhirImplementationGuideDefinitionPage(tvStructure.Selected.tagObject));
    tfhirImplementationGuideDefinitionPage(prtObj).pageList.Remove(idx);
    end;
    ReloadTreeview(tvStructure.Selected.ParentItem);
    end);

    end;

  }

  TDialogService.MessageDialog('Delete element and all its children?', TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      resourceIsDirty := true;
      prt := tvStructure.Selected.ParentItem;
      prtObj := prt.tagObject;

      idx := tfhirImplementationGuideDefinition(prtObj).groupingList.IndexOf(tfhirImplementationGuideDefinitionGrouping(tvStructure.Selected.tagObject));
      tfhirImplementationGuideDefinition(prtObj).groupingList.Remove(idx);

      ReloadTreeview(prt);
    end);

end;

procedure TImplementationGuideEditorFrame.btnImportResourcesClick(Sender: TObject);
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
      resx := tfhirImplementationGuideDefinitionResource(tvStructure.Selected.tagObject);
      if not Assigned(resx) then
        resx.reference := tfhirReference.Create;
      resx.reference.reference := changefileext((extractfilename(OpenDialog3.Files[i])), '');
      resx.Name := changefileext(extractfilename(OpenDialog3.Files[i]), '');
      if IGContentFolder <> '' then
        try
          TFile.copy(OpenDialog3.Files[i], IGContentFolder + '\resources\' + extractfilename(OpenDialog3.Files[i]));
        except

        end;

      // tfhirImplementationGuide(resource).definition.resourceList.Add(resx);
      // tfhirImplementationGuideDefinition(tvStructure.Selected.parent.tagObject).resourceList.Add(resx);

    end;

    resourceIsDirty := true;
    ReloadTreeview(tvStructure.Selected);

  end;

end;

procedure TImplementationGuideEditorFrame.btnAddGroupingClick(Sender: TObject);
var
  package: tfhirImplementationGuideDefinitionGrouping;
begin
  package := tfhirImplementationGuideDefinitionGrouping.Create;
  Package.Name := 'Grouping';

  if tvStructure.Selected.tagObject is tfhirImplementationGuideDefinition then
    tfhirImplementationGuideDefinition(tvStructure.Selected.tagObject).groupingList.Add(Package);

  resourceIsDirty := true;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.btnAddResourceClick(Sender: TObject);
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

procedure TImplementationGuideEditorFrame.btnDeletePageClick(Sender: TObject);
var
  idx: integer;
  prtObj: TObject;
begin
  // tFHIRObject(tvStructure.Selected.tagObject).dropEmpty;
  prtObj := tvStructure.Selected.ParentItem.tagObject;

  TDialogService.MessageDialog('Delete element and all its children?', TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      resourceIsDirty := true;
      if (prtObj) is tfhirImplementationGuideDefinition then
      begin
        tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).deleteProperty('page', tfhirImplementationGuideDefinitionPage(tvStructure.Selected.tagObject));
      end

      else if (prtObj) is tfhirImplementationGuideDefinitionPage then
      begin
        idx := tfhirImplementationGuideDefinitionPage(prtObj).pageList.IndexOf(tfhirImplementationGuideDefinitionPage(tvStructure.Selected.tagObject));
        tfhirImplementationGuideDefinitionPage(prtObj).pageList.Remove(idx);
      end;
      ReloadTreeview(tvStructure.Selected.ParentItem);
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

procedure TImplementationGuideEditorFrame.btAddGlobalClick(Sender: TObject);
var
  global: tfhirImplementationGuideGlobal;
begin
  global := tfhirImplementationGuideGlobal.Create;
  tfhirImplementationGuide(tvStructure.Selected.tagObject).globalList.AddItem(global);
  resourceIsDirty := true;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.btnAddDefinitionClick(Sender: TObject);
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

procedure TImplementationGuideEditorFrame.btnDependsOnClick(Sender: TObject);
var
  dependsOn: tfhirImplementationGuideDependsOn;
begin
  dependsOn := tfhirImplementationGuideDependsOn.Create;
  tfhirImplementationGuide(tvStructure.Selected.tagObject).dependsOnList.AddItem(dependsOn);
  resourceIsDirty := true;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.btnDependsOnUpClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirImplementationGuide(tvStructure.Selected.ParentItem.tagObject).dependsOnList.IndexOf(tfhirImplementationGuideDependsOn(tvStructure.Selected.tagObject));
  if idx > 0 then
  begin
    tfhirImplementationGuide(tvStructure.Selected.ParentItem.tagObject).dependsOnList.Exchange(idx, idx - 1);
    tvStructure.Selected.Index := tvStructure.Selected.Index - 1;
    resourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.btnGlobalDownClick(Sender: TObject);
var
  idx: integer;
begin

  idx := tfhirImplementationGuide(tvStructure.Selected.ParentItem.tagObject).globalList.IndexOf(tfhirImplementationGuideGlobal(tvStructure.Selected.tagObject));

  if idx < tfhirImplementationGuide(tvStructure.Selected.ParentItem.tagObject).globalList.Count - 1 then
  begin
    tfhirImplementationGuide(tvStructure.Selected.ParentItem.tagObject).globalList.Exchange(idx, idx + 1);
    tvStructure.Selected.Index := tvStructure.Selected.Index + 1;
    resourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.btnGlobalUpClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirImplementationGuide(tvStructure.Selected.ParentItem.tagObject).globalList.IndexOf(tfhirImplementationGuideGlobal(tvStructure.Selected.tagObject));
  if idx > 0 then
  begin
    tfhirImplementationGuide(tvStructure.Selected.ParentItem.tagObject).globalList.Exchange(idx, idx - 1);
    tvStructure.Selected.Index := tvStructure.Selected.Index - 1;
    resourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);

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

procedure TImplementationGuideEditorFrame.btnInlinePreviewClick(Sender: TObject);
var
  tempstr, filestr: string;
  SL: TStringList;
begin

  tempstr := IGContentFolder + '\pagecontent\' + changefileext(edtPageNameURL.text, '.xml');

  filestr := IGTempFolder + '\tmp.html';

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

procedure TImplementationGuideEditorFrame.btnPagePreviewClick(Sender: TObject);
var
  tempstr, filestr: string;
  SL: TStringList;
begin

  if not((IGContentFolder <> '') and directoryExists(IGContentFolder) and directoryExists(IGContentFolder + '\pagecontent')) then
  begin
    TDialogService.MessageDialog('IG content folders not defined or not found.' + #13#10 + ' Select the top element (Implementation Guide) and Click "Publish", and check for the folders',
    // System.UITypes.TMsgDlgType.mtConfirmation, [System.UITypes.TMsgDlgBtn.mbOK], System.UITypes.TMsgDlgBtn.mbOK, 0,
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbOk], TMsgDlgBtn.mbOk, 0,
      procedure(const AResult: TModalResult)
      begin
        case AResult of
          mrOk:
            begin
            end;
        end;
      end);

  end
  else
  begin
    filestr := IGContentFolder + '\tmp.html';
    tempstr := IGContentFolder + '\pagecontent\' + changefileext(edtPageNameURL.text, '.xml');
    filestr := IGContentFolder + '\tmp.html';
    tempstr := IGContentFolder + '\pagecontent\' + changefileext(edtPageNameURL.text, '.xml');

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
    runAndWait(IGRootFolder, filestr, filestr);
    // webbrowser1.Navigate( filestr );

  end;
end;

procedure TImplementationGuideEditorFrame.btnUpdateDependsOnClick(Sender: TObject);
var
  obj: tfhirImplementationGuideDependsOn;

begin
  obj := tfhirImplementationGuideDependsOn(TTreeViewItem(tvStructure.Selected).tagObject);
  if obj = nil then
    exit;
  obj.uri := edtDepOnURI.text;
  obj.packageId := edtDepOnPackage.text;
  obj.version := edtDepOnVersion.text;

end;

procedure TImplementationGuideEditorFrame.btnUpdateGlobalClick(Sender: TObject);
var
  obj: tfhirImplementationGuideGlobal;

begin
  obj := tfhirImplementationGuideGlobal(TTreeViewItem(tvStructure.Selected).tagObject);
  if obj = nil then
    exit;
  obj.type_ := TFHIRResourceTypesEnum(cbGlobalType.ItemIndex);
  obj.profile := edtGlobalProfileName.text;

end;

procedure TImplementationGuideEditorFrame.btnImportDocClick(Sender: TObject);
{$IFDEF OSX}
begin
  raise EFslException.Create('Not done yet for OSX');
end;
{$ELSE}

var
  optionsStr, tempstr, filestr, sCmd, ExecuteFile, ParamString, StartInString: string;
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
  // SL: TStringList;
  i, InsTextPos: integer;
  destfile, lResultStr: String;
  contentImport: TcontentImport;
  sa: TStringDynArray;

begin



  If (IGContentFolder = '') or not(directoryExists(pwidechar(IGContentFolder)) and directoryExists(pwidechar(IGContentFolder)+'\input\pagecontent') and directoryExists(pwidechar(IGMediaFolder))) then
  begin
    TDialogService.MessageDialog('IG content folders not defined or not found.' + #13#10 + ' Select the top element (Implementation Guide) and Click "Publish", check for the folders, and Save',
      System.UITypes.TMsgDlgType.mtConfirmation, [System.UITypes.TMsgDlgBtn.mbOk], System.UITypes.TMsgDlgBtn.mbOk, 0,
      procedure(const AResult: TModalResult)
      begin
        case AResult of
          mrYES:
            begin
            end;
          mrNo:
            begin
              exit;
            end;
        end;
      end);
  end;

  If ((IGContentFolder <> '')
  // and (IGPublisherFolder <> '')
    and (directoryExists(pwidechar(IGContentFolder)) and directoryExists(pwidechar(IGPageContentFolder)) and directoryExists(pwidechar(IGMediaFolder)))) then

  begin

    contentImport := TcontentImport.Create(Self);
    contentImport.IGContentFolder := IGContentFolder;
    contentImport.IGPublisherFolder := IGPublisherFolder;
    contentImport.IGRootFolder := IGRootFolder;
    contentImport.PageContentFolder := IGPageContentFolder;
    contentImport.mediaFolder := IGMediaFolder;
    contentImport.tempFolder := IGTempFolder;

    contentImport.Edit1.text := edtPageNameURL.text;
    contentImport.ShowModal;
    contentImport.Free;

  end;

  exit;

end;
{$ENDIF}

procedure TImplementationGuideEditorFrame.btnImportResourceClick(Sender: TObject);
var
  i: integer;
  resx: tfhirImplementationGuideDefinitionResource;
  resref: tfhirReference;
  extResource: TFHIRResource;
  format: TFHIRFormat;
  restype: string;
  resFileName: string;
begin
  // opendialog3.Options:=[TOpenOption.ofHideReadOnly];
  OpenDialog3.Options := [];

  if OpenDialog3.Execute then
  begin

    /// if tvStructure.Selected.TagObject then

    for i := 0 to OpenDialog3.Files.Count - 1 do
    begin
      resx := tfhirImplementationGuideDefinitionResource(tvStructure.Selected.tagObject);
      if not Assigned(resx) then
        resx.reference := tfhirReference.Create;

      format := ffUnspecified;

      restype := '';
      try
        extResource := fileToResource(OpenDialog3.Files[i], format);
        restype := extResource.fhirType;

      finally
        extResource.Free;

      end;

      resFileName := changefileext(extractfilename(OpenDialog3.Files[i]), '');

      // This bit is because the publisher expects the resource to be called [type]/name   ... from here...

      if (pos('-', extractfilename(resFileName)) > 0) and (uppercase(copy(resFileName, 1, pos('-', resFileName) - 1)) = uppercase(restype)) then
        resx.reference.reference := restype + '/' + changefileext(copy(resFileName, pos('-', resFileName) + 1, length(resFileName) - pos('-', resFileName)), '')
      else
        // ...until here
        resx.reference.reference := restype + '/' + resFileName;

      resx.Name := changefileext(extractfilename(OpenDialog3.Files[i]), '');
      if IGContentFolder <> '' then
        try
          TFile.copy(OpenDialog3.Files[i], IGContentFolder + '\resources\' + extractfilename(OpenDialog3.Files[i]));
        except

        end;

      // tfhirImplementationGuide(resource).definition.resourceList.Add(resx);
      // tfhirImplementationGuideDefinition(tvStructure.Selected.parent.tagObject).resourceList.Add(resx);

    end;
    resourceIsDirty := true;
    ReloadTreeview(tvStructure.Selected);

  end;

end;

Procedure TImplementationGuideEditorFrame.ReloadTreeview(sel_item: TTreeViewItem);
var
  current_item: TTreeViewItem;
  i: integer;
  Instance: tfhirImplementationGuide;
  sel_index: integer;
  sel_text: string;
  displayName: string;

begin
  sel_index := -1;
  sel_text := '';
  if sel_item <> nil then
    sel_index := sel_item.GlobalIndex;

  try
    tvStructure.Clear;
    if tfhirImplementationGuide(resource).Name = '' then
      displayName := 'Implementation Guide'
    else
      displayName := tfhirImplementationGuide(resource).Name;
    current_item := addTVItem(tvStructure, nil, 'implementationguide', displayName, resource);
  finally
    // tvStructure.EndUpdate;
    tvStructure.Repaint;
  end;
  // tvStructure.repaint;
  // application.ProcessMessages;
  if sel_index <> -1 then
    tvStructure.Selected := tvStructure.ItemByGlobalIndex(sel_index);
  showTab(tFHIRObject(tvStructure.Selected.tagObject));

end;

procedure TImplementationGuideEditorFrame.showTab(obj: tFHIRObject);
var
  selIndex, i, lastSlashPos: integer;
  extracted_ID: string;
  ImplementationGuide: tfhirImplementationGuide;
  ver: TFHIRFHIRVersionEnum;

begin
  selIndex := 0;
  // webbrowser1.Enabled:=false;

  if obj is tfhirImplementationGuide then
  begin

    btnAddDefinition.enabled := true;
    if tfhirImplementationGuide(tvStructure.Selected.tagObject).definition <> nil then
      btnAddDefinition.enabled := false;

    selIndex := 2;
    UpdateImplementationGuide.enabled := true;
    edtIGName.text := tfhirImplementationGuide(obj).Name;
    edtIGTitle.text := tfhirImplementationGuide(obj).title;

    edtIGURL.text := tfhirImplementationGuide(obj).url;

    lastSlashPos := -1;
    lastSlashPos := LastDelimiter('/', edtIGURL.text);
    if lastSlashPos >= 0 then
      extracted_ID := copy(edtIGURL.text, lastSlashPos + 1, length(edtIGURL.text) - lastSlashPos);
    if lastSlashPos >= 0 then
      edtIGURL.text := copy(edtIGURL.text, 1, lastSlashPos);

    edtIGId.text := tfhirImplementationGuide(obj).id;

    if (edtIGURL.text <> '') and (copy(edtIGURL.text, length(edtIGURL.text), 1) <> '/') then
      edtIGURL.text := edtIGURL.text + '/';

    Label7.text := edtIGURL.text + edtIGId.text;
    Label7.hint := edtIGURL.text + edtIGId.text;

    if extracted_ID = edtIGId.text then
      edtIGURL.text := copy(edtIGURL.text, 1, lastSlashPos);

    cbIGStatus.ItemIndex := integer(tfhirImplementationGuide(obj).status);
    if tfhirImplementationGuide(obj).experimental then
      chkExperimental.IsChecked := true
    else
      chkExperimental.IsChecked := false;
    dtIGDate.text := tfhirImplementationGuide(obj).date.toString;
    edtIGPackageId.text := tfhirImplementationGuide(obj).packageId;
    edtIGPublisher.text := tfhirImplementationGuide(obj).publisher;
    mmIGPurpose.text := tfhirImplementationGuide(obj).Description;
    edtIGVersion.text := tfhirImplementationGuide(obj).version;
    cbIGLicense.ItemIndex := integer(tfhirImplementationGuide(obj).license);

    // This needs to change - only US supported now;

    edtIGJurisdiction.text := 'US';

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

  if obj is tfhirImplementationGuideGlobal then
  begin
    selIndex := 6;
    cbGlobalType.ItemIndex := integer(tfhirImplementationGuideGlobal(obj).type_);
    edtGlobalProfileName.text := tfhirImplementationGuideGlobal(obj).profile;
  end;

  if obj is tfhirImplementationGuideDependsOn then
  begin
    selIndex := 7;
    edtDepOnURI.text := tfhirImplementationGuideDependsOn(obj).uri;
    edtDepOnPackage.text := tfhirImplementationGuideDependsOn(obj).packageId;
    edtDepOnVersion.text := tfhirImplementationGuideDependsOn(obj).version;
  end;

  if obj is tfhirImplementationGuideDefinitionGrouping then
  begin
    selIndex := 3;
    UpdatePage.enabled := true;
    edtPackageName.text := tfhirImplementationGuideDefinitionGrouping(obj).Name;
    mmPackageDescription.text := tfhirImplementationGuideDefinitionGrouping(obj).Description;

  end;

  if obj is tfhirImplementationGuideDefinitionResource then
  begin
    selIndex := 5;
    UpdatePage.enabled := true;
    // Edit1.text := TFHIRImplementationGuideDefinitionPage(obj).name ;

    cbResourcePackage.Items.Clear;
    for i := 0 to tfhirImplementationGuide(resource).definition.groupingList.Count - 1 do
      cbResourcePackage.Items.Add(tfhirImplementationGuide(resource).definition.groupingList[i].Name);

    cbResourcePackage.ItemIndex := cbResourcePackage.Items.IndexOf(tfhirImplementationGuideDefinitionResource(obj).groupingId);

    edtResourceReference.text := tfhirImplementationGuideDefinitionResource(obj).reference.reference;
    edtResourceName.text := tfhirImplementationGuideDefinitionResource(obj).Name;
    mmResourceDescription.text := tfhirImplementationGuideDefinitionResource(obj).Description;

    if (tfhirImplementationGuideDefinitionResource(obj).fhirVersionList.Count > 0) then
      for ver in TFHIRFHIRVersionEnumList(tfhirImplementationGuideDefinitionResource(obj).fhirVersion) do
      begin
        lbResourceVersions.ItemByIndex(lbResourceVersions.Items.Count - integer(ver)).IsChecked := true;
      end;

  end;

  if obj is tfhirImplementationGuideDefinitionPage then
  begin
    pageUp.Visible := true;
    pageDown.Visible := true;
    selIndex := 4;
    UpdatePage.enabled := true;
    if tfhirImplementationGuideDefinitionPage(obj).nameElement <> nil then
    begin
      edtPageNameURL.text := (tfhirImplementationGuideDefinitionPage(obj).nameElement as tfhirURL).value;
    end
    else
      edtPageNameURL.text := '';
    // webbrowser1.Enabled:=true;
    edtPageTitle.text := tfhirImplementationGuideDefinitionPage(obj).title;
    cbPageGeneration.ItemIndex := integer(tfhirImplementationGuideDefinitionPage(obj).generation);
    if AutoPreview.IsChecked then
      btnInlinePreviewClick(Self);

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
  idx: integer;
  prt: TTreeViewItem;
  prtObj, obj: TObject;

begin

  {
    if (prtObj) is tfhirImplementationGuideDefinition then
    begin
    tfhirImplementationGuideDefinition(tvStructure.Selected.ParentItem.tagObject).deleteProperty('page', tfhirImplementationGuideDefinitionPage(tvStructure.Selected.tagObject));
    end

    else if (prtObj) is tfhirImplementationGuideDefinitionPage then
    begin
    idx := tfhirImplementationGuideDefinitionPage(prtObj).pageList.IndexOf(tfhirImplementationGuideDefinitionPage(tvStructure.Selected.tagObject));
    tfhirImplementationGuideDefinitionPage(prtObj).pageList.Remove(idx);
    end;
    ReloadTreeview(tvStructure.Selected.ParentItem);
    end);

    end;

  }

  TDialogService.MessageDialog('Delete element and all its children?', TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      resourceIsDirty := true;
      prt := tvStructure.Selected.ParentItem;
      prtObj := tvStructure.Selected.ParentItem.tagObject;

      tfhirImplementationGuide(prtObj).deleteProperty('definition', tfhirImplementationGuideDefinition(tvStructure.Selected.tagObject));

      ReloadTreeview(prt);
    end);

end;

procedure TImplementationGuideEditorFrame.mnSaveClick(Sender: TObject);
begin
  if filename = '' then
  begin
    if SaveAs = -1 then
    begin
      ShowMessage('Not saved');
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
    IGRootFolder := extractfiledir(filename);
    resourceToFile(resource, filename, ffXml, OutputStylePretty);
    showTab(resource);
    result := 0;
  end;

end;

procedure TImplementationGuideEditorFrame.UpdateImplementationGuideClick(Sender: TObject);
var
  obj: tfhirImplementationGuide;
  jurisdiction: TFhirCodeableConcept;

begin
  obj := tfhirImplementationGuide(TTreeViewItem(tvStructure.Selected).tagObject);
  if obj = nil then
    exit;
  obj.Name := edtIGName.text;
  obj.title := edtIGTitle.text;

  if (edtIGURL.text <> '') and (copy(edtIGURL.text, length(edtIGURL.text), 1) <> '/') then
    edtIGURL.text := edtIGURL.text + '/';

  (obj).url := edtIGURL.text + edtIGId.text;
  Label7.text := (obj).url;
  Label7.hint := (obj).url;

  (obj).status := TfHIRPublicationStatusEnum(cbIGStatus.ItemIndex);
  obj.experimental := chkExperimental.IsChecked;
  obj.id := edtIGId.text;
  obj.date := TFslDateTime.makeLocal(dtIGDate.DateTime);
  obj.version := edtIGVersion.text;
  obj.packageId := edtIGPackageId.text;
  obj.publisher := edtIGPublisher.text;
  obj.Description := mmIGPurpose.text;
  obj.license := TFHIRspdxLicenseEnum(cbIGLicense.ItemIndex);
  if obj.Name <> '' then
    tvStructure.Selected.text := obj.Name;


  // this must change - at this moment nly us is upported

  if (obj).jurisdictionList.Count = 0 then
  begin
    jurisdiction := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'US');
    (obj).jurisdictionList.AddItem(jurisdiction);
  end;

end;

// 6.2
procedure TImplementationGuideEditorFrame.UpdatePackageClick(Sender: TObject);
var
  obj: tfhirImplementationGuideDefinitionGrouping;
begin
  obj := tfhirImplementationGuideDefinitionGrouping(tvStructure.Selected.tagObject);
  if obj = nil then
    exit;
  obj.Name := edtPackageName.text;
  obj.Description := mmPackageDescription.text;
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
  tfhirURL((obj).nameElement).value := edtPageNameURL.text;
  obj.generation := TFHIRGuidePageGenerationEnum(cbPageGeneration.ItemIndex);
  obj.title := edtPageTitle.text;
  tvStructure.Selected.text := obj.title;

end;

procedure TImplementationGuideEditorFrame.UpdateResourceClick(Sender: TObject);
var
  obj: tfhirImplementationGuideDefinitionResource;
  i: integer;
  ver: TFHIRFHIRVersionEnum;
begin
  obj := tfhirImplementationGuideDefinitionResource(TTreeViewItem(tvStructure.Selected).tagObject);
  if obj = nil then
    exit;
  if edtResourceReference.text <> '' then
  begin
    if (obj.reference = nil) then
      obj.reference := tfhirReference.Create;
    obj.reference.reference := edtResourceReference.text;
  end;

  obj.Name := edtResourceName.text;
  obj.Description := mmResourceDescription.text;

  obj.fhirVersionList.ClearItems;
  for i := 0 to lbResourceVersions.Items.Count - 1 do
  begin
    if lbResourceVersions.ItemByIndex(i).IsChecked then
      obj.fhirVersion := obj.fhirVersion + [TFHIRFHIRVersionEnum(lbResourceVersions.Items.Count - i)]; // subtract index because list is sorted in reverse order

  end;

  if cbResourcePackage.ItemIndex <> -1 then
    obj.groupingId := cbResourcePackage.Items[cbResourcePackage.ItemIndex];
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

procedure TImplementationGuideEditorFrame.btnDependsOnDownClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirImplementationGuide(tvStructure.Selected.ParentItem.tagObject).dependsOnList.IndexOf(tfhirImplementationGuideDependsOn(tvStructure.Selected.tagObject));

  if idx < tfhirImplementationGuide(tvStructure.Selected.ParentItem.tagObject).dependsOnList.Count - 1 then
  begin
    tfhirImplementationGuide(tvStructure.Selected.ParentItem.tagObject).dependsOnList.Exchange(idx, idx + 1);
    tvStructure.Selected.Index := tvStructure.Selected.Index + 1;
    resourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);

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
{$IFDEF OSX}
begin
  raise EFslException.Create('Not done yet for OSX');
end;
{$ELSE}var
  folderstr, filestr: string;
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
  // sCmd, ExecuteFile, ParamString, StartInString: string;

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
{$ENDIF}

end.
