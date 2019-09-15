unit ExampleScenarioEditor;

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
  System.SysUtils, System.Rtti, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, FMX.Layouts, FMX.TreeView, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, FMX.DateTimeCtrls, FMX.ListBox, FMX.Edit, FMX.DialogService,
  FMX.Grid.Style, FMX.Grid, FMX.Menus, FMX.ImgList, FHIR.Support.Collections,
  FHIR.Support.Base, FHIR.Support.Utilities,
  FHIR.Base.Objects, FHIR.Version.Constants, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Utilities, FHIR.Tools.Indexing, FHIR.Version.IndexInfo,
  BaseResourceFrame, ToolKitUtilities,
  SearchParameterEditor, ListSelector, AddRestResourceDialog, ValuesetExpansion, ValuesetSelectDialog, MemoEditorDialog,
  FMX.Platform, System.ImageList, TranslationsEditorDialog;

type
  TFrame = TBaseResourceFrame;

  TExampleScenarioEditorFrame = class(TFrame)
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    tvStructure: TTreeView;
    tbStructure: TTabControl;
    tbMetadata: TTabItem;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    cbExperimental: TCheckBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    edtURL: TEdit;
    edtName: TEdit;
    edtTitle: TEdit;
    cbxStatus: TComboBox;
    dedDate: TDateEdit;
    edtPublisher: TEdit;
    edtDescription: TEdit;
    edtPurpose: TEdit;
    edtCopyright: TEdit;
    cbxJurisdiction: TComboBox;
    edtVersion: TEdit;
    tvMetadata: TTreeViewItem;
    btnMemoForDesc: TButton;
    btnMemoPurpose: TButton;
    btnMemoCopyright: TButton;
    Label12: TLabel;
    edtIdSystem: TEdit;
    Label25: TLabel;
    edtIdValue: TEdit;
    Label26: TLabel;
    Label27: TLabel;
    edtValueSet: TEdit;
    Label28: TLabel;
    cbxHeirarchy: TComboBox;
    Label29: TLabel;
    cbxContent: TComboBox;
    Label30: TLabel;
    edtConceptCount: TEdit;
    Button1: TButton;
    cbCaseSensitive: TCheckBox;
    cbCompositional: TCheckBox;
    cbNeedsVersion: TCheckBox;
    ScrollBox1: TScrollBox;
    tbProperties: TTabItem;
    tbFilters: TTabItem;
    tbConcepts: TTabItem;
    pnlPropertyActions: TPanel;
    grdProperties: TGrid;
    btnAddProperty: TButton;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    PopupColumn1: TPopupColumn;
    StringColumn3: TStringColumn;
    btnDeleteProperty: TButton;
    Panel1: TPanel;
    btnAddFIlter: TButton;
    btnDeleteFilter: TButton;
    grdFilters: TGrid;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    StringColumn7: TStringColumn;
    Panel5: TPanel;
    btnAddConcept: TButton;
    btnAddChildConcept: TButton;
    btnEditConcept: TButton;
    btnConceptUp: TButton;
    btnConceptDown: TButton;
    btnConceptIn: TButton;
    btnConceptOut: TButton;
    btnDeleteConcept: TButton;
    btnExport: TButton;
    btnName: TButton;
    btnTitle: TButton;
    btnPublisher: TButton;
    ImageColumn1: TImageColumn;
    ImageColumn2: TImageColumn;
    Label15: TLabel;
    edtSupplements: TEdit;
    StyleBook1: TStyleBook;
    ImageList3: TImageList;
    TabControl2: TTabControl;
    Intro: TTabItem;
    btnSave: TButton;
    Open: TButton;
    TabItem2: TTabItem;
    ScrollBox2: TScrollBox;
    ComboBox2: TComboBox;
    Label14: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    DateEdit1: TDateEdit;
    edtESid: TEdit;
    Edit7: TEdit;
    Label18: TLabel;
    Edit8: TEdit;
    Label19: TLabel;
    Memo3: TMemo;
    Label20: TLabel;
    CheckBox1: TCheckBox;
    ActorTab: TTabItem;
    UpdateActor: TButton;
    Instance_tab: TTabItem;
    Edit9: TEdit;
    Label33: TLabel;
    Edit10: TEdit;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Memo4: TMemo;
    ComboBox3: TComboBox;
    UpdateInstance: TButton;
    DeleteInstance: TButton;
    Process_tab: TTabItem;
    Edit4: TEdit;
    Label37: TLabel;
    ScrollBox3: TScrollBox;
    Label38: TLabel;
    Memo2: TMemo;
    Memo6: TMemo;
    Label39: TLabel;
    Label40: TLabel;
    Memo7: TMemo;
    UpdateProcess: TButton;
    Step_tab: TTabItem;
    btnPause: TCornerButton;
    chkPause: TCheckBox;
    btnUpdateStep: TButton;
    Alternative_tab: TTabItem;
    Label41: TLabel;
    Edit12: TEdit;
    UpdateAlternative: TButton;
    Option_tab: TTabItem;
    Memo5: TMemo;
    Label42: TLabel;
    Label43: TLabel;
    Edit13: TEdit;
    CheckBox2: TCheckBox;
    UpdateOption: TButton;
    Operation_tab: TTabItem;
    Label44: TLabel;
    Edit14: TEdit;
    Edit15: TEdit;
    Label45: TLabel;
    Label46: TLabel;
    Edit16: TEdit;
    ComboBox4: TComboBox;
    Label47: TLabel;
    ComboBox5: TComboBox;
    Label48: TLabel;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Label49: TLabel;
    ComboBox6: TComboBox;
    Label50: TLabel;
    ComboBox7: TComboBox;
    Memo8: TMemo;
    Label51: TLabel;
    UpdateOperation: TButton;
    Button2: TButton;
    Memo9: TMemo;
    Label21: TLabel;
    Label22: TLabel;
    edtESURL: TEdit;
    UpdateExampleScenario: TButton;
    Panel6: TPanel;
    btnActor: TCornerButton;
    btnResource: TCornerButton;
    btnProcess: TCornerButton;
    BtnIcons: TImageList;
    Panel9: TPanel;
    ContResource: TCornerButton;
    Version: TCornerButton;
    CornerButton11: TCornerButton;
    instanceUp: TCornerButton;
    InstanceDown: TCornerButton;
    ScrollBox4: TScrollBox;
    Button4: TButton;
    Panel8: TPanel;
    CornerButton7: TCornerButton;
    ActorUp: TCornerButton;
    ActorDown: TCornerButton;
    Panel10: TPanel;
    bnStep: TCornerButton;
    CornerButton10: TCornerButton;
    ProcessUp: TCornerButton;
    ProcessDown: TCornerButton;
    Panel11: TPanel;
    btnProcess2: TCornerButton;
    CornerButton9: TCornerButton;
    btnOperation: TCornerButton;
    btnAlternative: TCornerButton;
    edtESName: TEdit;
    Button5: TButton;
    Panel7: TPanel;
    CornerButton2: TCornerButton;
    btnOption: TCornerButton;
    Panel12: TPanel;
    CornerButton1: TCornerButton;
    ScrollBox5: TScrollBox;
    Label31: TLabel;
    Edit2: TEdit;
    Label24: TLabel;
    ComboBox1: TComboBox;
    Label23: TLabel;
    Edit3: TEdit;
    Label32: TLabel;
    Memo1: TMemo;
    ScrollBox6: TScrollBox;
    ScrollBox8: TScrollBox;
    ScrollBox10: TScrollBox;
    ScrollBox11: TScrollBox;
    ScrollBox12: TScrollBox;
    ScrollBox13: TScrollBox;
    Panel13: TPanel;
    CornerButton3: TCornerButton;
    btnStep2: TCornerButton;
    btnOptionDown: TCornerButton;
    btnOptionUp: TCornerButton;
    CornerButton4: TCornerButton;
    function addTVItem(TreeView: TTreeView; parent: TTreeViewItem; itemType, text: string; obj: tFHIRObject): TTreeViewItem;
    procedure btnActorClick(Sender: TObject);
    procedure btnOptionClick(Sender: TObject);
    procedure btnProcessClick(Sender: TObject);
    procedure btnResourceClick(Sender: TObject);
    procedure NewExampleScenarioClick(Sender: TObject);
    procedure btnStep22Click(Sender: TObject);
    procedure btnVersionClick(Sender: TObject);
    procedure ReloadTreeview(sel_item: TTreeViewItem);
    procedure showTab(obj: tFHIRObject);
    procedure tvStructureChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnUpdateStepClick(Sender: TObject);
    procedure DeleteItemClick(Sender: TObject);
    procedure MoveDownClick(Sender: TObject);
    procedure MoveUpClick(Sender: TObject);
    procedure chkPauseExit(Sender: TObject);
    procedure btnConceptUpClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ActorTabPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure ActorUpClick(Sender: TObject);
    procedure ActorDownClick(Sender: TObject);
    procedure VersionClick(Sender: TObject);
    procedure bnStepClick(Sender: TObject);
    procedure btnProcess2Click(Sender: TObject);
    procedure btnOperationClick(Sender: TObject);
    procedure btnAlternativeClick(Sender: TObject);
    procedure instanceUpClick(Sender: TObject);
    procedure InstanceDownClick(Sender: TObject);
    procedure ProcessDownClick(Sender: TObject);
    procedure ProcessUpClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure UpdateExampleScenarioClick(Sender: TObject);
    procedure UpdateProcessClick(Sender: TObject);
    procedure UpdateActorClick(Sender: TObject);
    procedure UpdateInstanceClick(Sender: TObject);
    procedure UpdateAlternativeClick(Sender: TObject);
    procedure UpdateOptionClick(Sender: TObject);
    procedure UpdateOperationClick(Sender: TObject);
    procedure btnOptionUpClick(Sender: TObject);
    procedure btnOptionDownClick(Sender: TObject);
    procedure CornerButton4Click(Sender: TObject);

  private
    selchanging : boolean;
    CURRobjectlist:TFHIRObjectList;

  public
    ESRootFolder: string;
    ESFileName: String; // Is this needed?
    ESPublisherFolder: string;

    constructor Create(owner : TComponent); override;
    destructor Destroy; override;

    procedure load; override;

    procedure commit; override;
    procedure cancel; override;



  end;

implementation

{$R *.fmx}

uses ScenarioRendering;

function polish(s : String): String;
begin
  result := s.trim.replace(#13, ' ').replace(#10, ' ').replace('  ', ' ');
end;



{ TExampleScenarioEditorFrame }























function TExampleScenarioEditorFrame.addTVItem(TreeView: TTreeView; parent: TTreeViewItem; itemType, text: string; obj: tFHIRObject): TTreeViewItem;
var
  current_item, Item: TTreeViewItem;
  i: integer;
  objtype:string;
begin
  Item := TTreeViewItem.Create(TreeView);
  objtype:=obj.fhirType;

    TreeView.AddObject(Item) ;
  if (parent = nil) then
    Item.parent := TreeView
  else
    Item.parent := parent;

  Item.ImageIndex:=-1;

  if itemType = 'examplescenario' then Item.ImageIndex:=9;
  if itemType = 'actor' then Item.ImageIndex:=1;
  if itemType = 'instance' then  Item.ImageIndex:=2;
  if itemType = 'process' then  Item.ImageIndex:=3;
  if itemType = 'version' then  Item.ImageIndex:=4;
  if itemType = 'step' then  Item.ImageIndex:=5;
  if itemType = 'operation' then  Item.ImageIndex:=6;
  if itemType = 'alternative' then  Item.ImageIndex:=7;
  if itemType = 'option' then  Item.ImageIndex:=8;

//  Item.StylesData['info'] := TValue.From<TNotifyEvent>(DoInfoClick);
//  Item.OnApplyStyleLookup := DoApplyStyleLookup;
  Item.StylesData['attrType'] := itemType;
  Item.StylesData['sortOrder'] := 1;
  TreeView.Selected := Item;
  Item.Height := 20;
  Item.text := text;
  Item.tagObject := obj;
  Item.IsExpanded := true;

// do this for the object - any object type:
  if obj is tfhirexamplescenarioinstance then
  begin
    current_item := Item;

// do this for each object in that object type:
    for i := 0 to tfhirexamplescenarioinstance(obj).versionList.Count - 1 do
    begin
      addTVItem(TreeView, current_item, 'version', tfhirexamplescenarioinstance(obj).versionList[i].versionId, tfhirexamplescenarioinstance(obj).versionList[i]);
    end;
//// until here
  end;

  if obj is tfhirexamplescenarioProcess then
  begin
    current_item := Item;
    for i := 0 to tfhirexamplescenarioProcess(obj).stepList.Count - 1 do
    begin
      addTVItem(TreeView, current_item, 'step', 'Step', tfhirexamplescenarioProcess(obj).stepList[i]);
    end;
  end;

  if obj is tfhirexamplescenarioProcessStep then
  begin
    current_item := Item;
    if tfhirexamplescenarioProcessStep(obj).processList.Count <> 0 then
      for i := 0 to tfhirexamplescenarioProcessStep(obj).processList.Count - 1 do
      begin
        addTVItem(TreeView, current_item, 'process', tfhirexamplescenarioProcessStep(obj).processList[i].title, tfhirexamplescenarioProcessStep(obj).processList[i]);
      end;

    if tfhirexamplescenarioProcessStep(obj).pauseElement <> nil then
    begin
//      addTVItem(TreeView1, current_item, 'pause', '(pause)', tfhirexamplescenarioProcessStep(obj).pauseElement);
    end;
    if tfhirexamplescenarioProcessStep(obj).operation <> nil then
    begin
      addTVItem(TreeView, current_item, 'operation', tfhirexamplescenarioProcessStep(obj).operation.name, tfhirexamplescenarioProcessStep(obj).operation);
    end;

    if tfhirexamplescenarioProcessStep(obj).alternativeList.Count > 0 then
    begin
      addTVItem(TreeView, current_item, 'alternative', tfhirexamplescenarioProcessStep(obj).alternativeList[0].title, tfhirexamplescenarioProcessStep(obj).alternativeList[0]);
    end;
  end;

  if obj is tfhirexamplescenarioProcessStepAlternative then
  begin
    current_item := Item;
    for i := 0 to tfhirexamplescenarioProcessStepAlternative(obj).stepList.Count - 1 do
    begin
      addTVItem(TreeView, current_item, 'option', 'Option', tfhirexamplescenarioProcessStepAlternative(obj).stepList[i]);
    end;
  end;


  if obj is tfhirexamplescenario then
  begin
    if text <> '' then
      tab.text := text;
    current_item := Item;    for i := 0 to tfhirexamplescenario(obj).actorList.Count - 1 do
      addTVItem(TreeView, current_item, 'actor', tfhirexamplescenario(obj).actorList[i].name, tfhirexamplescenario(obj).actorList[i]);

    for i := 0 to tfhirexamplescenario(obj).instanceList.Count - 1 do
      addTVItem(TreeView, current_item, 'instance', tfhirexamplescenario(obj).instanceList[i].name, tfhirexamplescenario(obj).instanceList[i]);

    for i := 0 to tfhirexamplescenario(obj).processList.count - 1 do
      if tfhirexamplescenario(obj).ProcessList[i] <> nil then
        addTVItem(TreeView, current_item, 'process', tfhirexamplescenario(obj).ProcessList[i].title, tfhirexamplescenario(obj).ProcessList[i]);
  end;

//  TreeView.EndUpdate;
//  TreeView.repaint;
//  result := Item;
//
end;




procedure TExampleScenarioEditorFrame.btnAlternativeClick(Sender: TObject);
begin
  tfhirexamplescenarioProcessStep(TVStructure.Selected.TagObject).alternativeList.Append.title := 'Alternative';
  ResourceIsDirty := true;
  ReloadTreeview(tvStructure.Selected);
end;


procedure TExampleScenarioEditorFrame.CornerButton4Click(Sender: TObject);
var esRender:TESPublishForm;
begin


  esRender:=TESPublishForm.create(self);
  esRender.resource:=TFHIRExampleScenario(resource);

  if filename <> '' then
  begin
    esRender.ESRootFolder := extractfilepath(filename);
    esRender.ESfilename:=filename;
  end;

  esRender.showModal;
  esRender.destroy;

end;

constructor TExampleScenarioEditorFrame.Create(owner: TComponent);
begin
  inherited;
//  flatItems := TFslList<TFhirQuestionnaireItem>.create;

//show;
//  ReloadTreeview(tvStructure.Selected);
end;

destructor TExampleScenarioEditorFrame.Destroy;
begin
//  flatItems.Free;
  inherited;
end;



procedure TExampleScenarioEditorFrame.InstanceDownClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirexamplescenario(resource).instanceList.IndexOf(tfhirexamplescenarioInstance(TVStructure.Selected.TagObject));
  if idx < tfhirexamplescenario(resource).instanceList.Count - 1 then begin
  tfhirexamplescenario(resource).instanceList.Exchange(idx, idx + 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index + 1;
  ResourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);
end;

procedure TExampleScenarioEditorFrame.instanceUpClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirexamplescenario(resource).instanceList.IndexOf(tfhirexamplescenarioInstance(TVStructure.Selected.TagObject));
  if idx > 0 then begin
  tfhirexamplescenario(resource).instanceList.Exchange(idx, idx - 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index - 1;
  ResourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);
end;

procedure TExampleScenarioEditorFrame.load;
begin

  inherited;

  TabControl2.TabHeight := 1;
  begin
    tvStructure.beginUpdate;
    try
      addTVItem(tvStructure, nil, 'examplescenario', 'Example Scenario', resource);
    finally
      tvStructure.EndUpdate;
    end;
  end;
  tfhirExampleScenario(resource).id:=extractfilename(changefileext(filename,''));
  application.ProcessMessages;
  tvStructure.Selected := tvStructure.ItemByGlobalIndex(1);

  showTab(TFHIRObject(TFHIRObject(TVStructure.Selected.TagObject)));


//EXIT;


end;


procedure TExampleScenarioEditorFrame.cancel;
begin
end;

procedure TExampleScenarioEditorFrame.chkPauseExit(Sender: TObject);
begin
 btnUpdateStepClick(Self);
end;

procedure TExampleScenarioEditorFrame.commit;
begin
{  if tvStructure.Selected = tvMetadata then
    CommitMetadata;
  if tvStructure.Selected = tvSDC then
    CommitSDC;
  ResourceIsDirty := true;
}
end;





procedure TExampleScenarioEditorFrame.ActorDownClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirexamplescenario(resource).actorList.IndexOf(tfhirexamplescenarioActor(TVStructure.Selected.TagObject));
  if idx < tfhirexamplescenario(resource).actorList.Count - 1 then begin
  tfhirexamplescenario(resource).actorList.Exchange(idx, idx + 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index + 1;
  ResourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);
end;

procedure TExampleScenarioEditorFrame.ActorTabPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
CURRobjectlist:=tfhirexamplescenario(resource).actorList;

end;

procedure TExampleScenarioEditorFrame.ActorUpClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirexamplescenario(resource).actorList.IndexOf(tfhirexamplescenarioActor(TVStructure.Selected.TagObject));
  if idx > 0 then begin
  tfhirexamplescenario(resource).actorList.Exchange(idx, idx - 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index - 1;
  ResourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);
end;





procedure TExampleScenarioEditorFrame.bnStepClick(Sender: TObject);
var
  Step: tfhirexamplescenarioProcessStep;
begin
  Step := tfhirexamplescenarioProcessStep.Create;
  tfhirexamplescenarioProcess(TVStructure.Selected.TagObject).stepList.AddItem(Step);
  ResourceIsDirty := true;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TExampleScenarioEditorFrame.btnActorClick(Sender: TObject);
var
  Actor: tfhirexamplescenarioActor;
  objclass: System.tclass;

  RttiContext: TRttiContext;
  RttiType: TRttiInstanceType;
  Foo: TObject;

  ctx: TRttiContext;
  objType: TRttiType;
  Prop: TRttiProperty;

begin
  Actor := tfhirexamplescenarioActor.Create;
  Actor.type_ := ExamplescenarioActorTypePerson;
  Actor.name := 'Actor ' + inttostr(tfhirexamplescenario(TVStructure.Selected.tagObject).actorList.Count + 1);
  Actor.actorId := 'A' + inttostr(tfhirexamplescenario(TVStructure.Selected.TagObject).actorList.Count + 1);
  tfhirexamplescenario(TVStructure.Selected.TagObject).actorList.AddItem(Actor);
  ResourceIsDirty := true;
  ReloadTreeview(TVStructure.Selected);
  tvStructureChange(self);
end;



procedure TExampleScenarioEditorFrame.btnConceptUpClick(Sender: TObject);
var
  list : TFSLItemList;
  idx:integer;
begin
//  idx := CURRobjectlist.IndexOf(tfhirexamplescenarioActor(TVStructure.Selected.TagObject));
  if idx > 0 then begin
  tfhirexamplescenario(resource).actorList.Exchange(idx, idx - 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index - 1;
  end;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TExampleScenarioEditorFrame.btnOptionClick(Sender: TObject);
var
  Option: tfhirexamplescenarioProcessStep;
begin
  Option := tfhirexamplescenarioProcessStep.Create;
//Set properties for object
//  Option.description := 'Option ' + inttostr(tfhirexamplescenarioProcessStepAlternative(TVStructure.Selected.TagObject).stepList.Count + 1);


  tfhirexamplescenarioProcessStepAlternative(TVStructure.Selected.TagObject).StepList.AddItem(Option);
  ResourceIsDirty := true;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TExampleScenarioEditorFrame.btnOptionDownClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirexamplescenarioProcessStepAlternative(TVStructure.Selected.ParentItem.TagObject).stepList.IndexOf(tfhirexamplescenarioProcessStep(TVStructure.Selected.TagObject));
  if idx < tfhirexamplescenarioProcessStepAlternative(TVStructure.Selected.ParentItem.TagObject).stepList.Count - 1 then begin
  tfhirexamplescenarioProcessStepAlternative(TVStructure.Selected.ParentItem.TagObject).stepList.Exchange(idx, idx + 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index + 1;
  ResourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);
end;


procedure TExampleScenarioEditorFrame.btnOptionUpClick(Sender: TObject);
var
  idx: integer;
begin

  idx := tfhirexamplescenarioProcessStepAlternative(TVStructure.Selected.ParentItem.TagObject).stepList.IndexOf(tfhirexamplescenarioProcessStep(TVStructure.Selected.TagObject));
  if idx > 0 then begin
  tfhirexamplescenarioProcessStepAlternative(TVStructure.Selected.ParentItem.TagObject).stepList.Exchange(idx, idx - 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index - 1;
  ResourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);
end;

procedure TExampleScenarioEditorFrame.btnOperationClick(Sender: TObject);
var
  operation: tfhirexamplescenarioProcessStepOperation;
  Item: TTreeViewItem;
begin
  operation := tfhirexamplescenarioProcessStepOperation.Create;
  operation.name := 'Operation';
  tfhirexamplescenarioProcessStep(TVStructure.Selected.TagObject).operation := operation;
  ResourceIsDirty := true;
  ReloadTreeview(tvStructure.Selected);
end;

//2.4 Sub-Process
procedure TExampleScenarioEditorFrame.btnProcess2Click(Sender: TObject);
var
  Process: tfhirexamplescenarioProcess;
begin
  Process := tfhirexamplescenarioProcess.Create;
  Process.title := 'Process';
  tfhirexamplescenarioProcessStep(TVStructure.Selected.TagObject).processList.AddItem(Process);
  ResourceIsDirty := true;
  ReloadTreeview(tvStructure.Selected);
end;

//2.5 Main process
procedure TExampleScenarioEditorFrame.btnProcessClick(Sender: TObject);
var
  Process: tfhirexamplescenarioProcess;
begin
  Process := tfhirexamplescenarioProcess.Create;
  Process.title := 'Process';
  tfhirexamplescenario(TVStructure.Selected.TagObject).processList.AddItem(Process);
  ResourceIsDirty := true;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TExampleScenarioEditorFrame.btnResourceClick(Sender: TObject);
var
  Instance: tfhirexamplescenarioinstance;
begin
  Instance := tFHIRExampleScenarioInstance.Create;
  Instance.resourceType := ResourceTypesNull;
  Instance.name := 'Resource ' + inttostr(tfhirexamplescenario(TVStructure.Selected.TagObject).instanceList.Count + 1);
  Instance.resourceId := 'R' + inttostr(tfhirexamplescenario(TVStructure.Selected.TagObject).instanceList.Count + 1);
  ResourceIsDirty := true;
//  exsc.instanceList.AddItem(Instance);
  tfhirexamplescenario(TVStructure.Selected.TagObject).instanceList.AddItem(Instance);

  ReloadTreeview(tvStructure.Selected);
end;


procedure TExampleScenarioEditorFrame.NewExampleScenarioClick(Sender: TObject);
var
  ExampleScenario: TFHIRExampleScenario;
begin
  ExampleScenario := TFHIRExampleScenario.create;
  ExampleScenario.name := 'Example Scenario';
  resource := ExampleScenario;
  ReloadTreeview(tvStructure.Selected);

  showTab(TFHIRObject(tvStructure.Selected.tagObject));

//  showTab(exsc);



end;


procedure TExampleScenarioEditorFrame.ProcessDownClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirexamplescenario(resource).ProcessList.IndexOf(tfhirexamplescenarioProcess(TVStructure.Selected.TagObject));
  if idx < tfhirexamplescenario(resource).ProcessList.Count - 1 then begin
  tfhirexamplescenario(resource).ProcessList.Exchange(idx, idx + 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index + 1;
  ResourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);
end;

procedure TExampleScenarioEditorFrame.ProcessUpClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirexamplescenario(resource).processList.IndexOf(tfhirexamplescenarioProcess(TVStructure.Selected.TagObject));
  if idx > 0 then begin
  tfhirexamplescenario(resource).processList.Exchange(idx, idx - 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index - 1;
  ResourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);
end;

procedure TExampleScenarioEditorFrame.btnStep22Click(Sender: TObject);
var
  Step: tfhirexamplescenarioProcessStep;
begin
  Step := tfhirexamplescenarioProcessStep.Create;
  tfhirexamplescenarioProcess(TVStructure.Selected.TagObject).stepList.AddItem(Step);
  ResourceIsDirty := true;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TExampleScenarioEditorFrame.btnUpdateStepClick(Sender: TObject);
var
  obj: tfhirexamplescenarioProcessStep;

begin
  obj := tfhirexamplescenarioProcessStep(TVStructure.Selected.TagObject);
  if obj = nil then
    exit;

if chkPause.ischecked then
obj.pauseElement:=TFHIRBoolean.Create(true)
else
obj.deleteProperty('pause', nil);

  ReloadTreeview(tvStructure.Selected);

end;

procedure TExampleScenarioEditorFrame.btnVersionClick(Sender: TObject);
var
  version: tfhirexamplescenarioInstanceVersion;
begin
  version := tfhirexamplescenarioInstanceVersion.Create;
  tfhirexamplescenarioinstance(TVStructure.Selected.TagObject).versionList.AddItem(version);
  ReloadTreeview(tvStructure.Selected);

end;



procedure TExampleScenarioEditorFrame.Button2Click(Sender: TObject);
begin

//  ReloadTreeview(tvStructure.Selected);
//  showTab(TFHIRObject(TFHIRObject(TVStructure.Selected.TagObject)));

end;

procedure TExampleScenarioEditorFrame.Button4Click(Sender: TObject);
begin
button4.text:=TRttiContext.Create.GetType(button4.ClassType.ClassParent).ToString;

end;

procedure TExampleScenarioEditorFrame.Button5Click(Sender: TObject);
begin
//edit5.OnChange(edit5);


//button5.text:=
//edit5.FhirProperty.value;

button5.text:=
TFHIRExampleScenario(resource).nameElement.value;


button5.text:=
TFHIRExampleScenario(TVStructure.Selected.TagObject).nameElement.value;


end;

Procedure TExampleScenarioEditorFrame.ReloadTreeview(sel_item: TTreeViewItem);
var
  current_item: TTreeViewItem;
  i: integer;
  Actor: tfhirexamplescenarioActor;
  Instance: tfhirexamplescenarioinstance;
  sel_index: integer;
  sel_text: string;

begin
  sel_index := -1;
  sel_text := '';
  if sel_item <> nil then sel_index := sel_item.GlobalIndex;

  try
    tvStructure.Clear;
  // addTVItem is recursive so we just need to start populating the top level
  current_item := addTVItem(tvStructure, nil, 'examplescenario', 'Example Scenario', resource);
  finally
    // tvStructure.EndUpdate;
    tvStructure.Repaint;
  end;

  if sel_index <> -1 then
    tvStructure.Selected := tvStructure.ItemByGlobalIndex(sel_index);

//  showTab(TFHIRObject(TVStructure.Selected.TagObject));

end;



procedure TExampleScenarioEditorFrame.showTab(obj: tFHIRObject);
var
  i: integer;
  ExampleScenario2:TFHIRExampleScenario;

begin
  for i := 0 to TabControl2.TabCount - 1 do
    TabControl2.Tabs[i].Visible := false;

  TabControl2.tabindex := 0;

  if obj is TFHIRExampleScenario then
  begin

    TabControl2.tabindex := 1;
    UpdateExampleScenario.enabled := true;
    edtESName.text := TFHIRExampleScenario(obj).name;
    edtESURL.text := TFHIRExampleScenario(obj).title;
    ComboBox2.ItemIndex := integer(TFHIRExampleScenario(obj).status);
    if TFHIRExampleScenario(obj).experimental then
      CheckBox1.IsChecked := true
    else
      CheckBox1.IsChecked := false;
    edtESid.text := TFHIRExampleScenario(obj).id;
    DateEdit1.text := TFHIRExampleScenario(obj).date.toString;
    Edit8.text := TFHIRExampleScenario(obj).version;
    Edit7.text := TFHIRExampleScenario(obj).publisher;
    Memo3.text := TFHIRExampleScenario(obj).purpose;

  end;
  if obj is tfhirexamplescenarioActor then
  begin
    TabControl2.tabindex := 2;
    UpdateActor.enabled := true;
    Edit3.text := tfhirexamplescenarioActor(obj).name;
//    Edit3.FhirProperty:=tfhirProperty(TFHIRExampleScenarioActor(obj).name);
//    edit3.FHIRProperty:= TFHIRExampleScenarioActor(obj).nameElement;
//    edit3.load;

    ComboBox1.ItemIndex := integer(tfhirexamplescenarioActor(obj).type_);
    Edit2.text := tfhirexamplescenarioActor(obj).actorId;
    Memo1.text := tfhirexamplescenarioActor(obj).description;
  end;
  if obj is tfhirexamplescenarioinstance then
  begin
    TabControl2.tabindex := 3;
    UpdateInstance.enabled := true;
    Edit10.text := tfhirexamplescenarioinstance(obj).resourceId;
    ComboBox3.ItemIndex := integer(tfhirexamplescenarioinstance(obj).resourceType);
    Edit9.text := tfhirexamplescenarioinstance(obj).name;
    Memo4.text := tfhirexamplescenarioinstance(obj).description;

  end;
  if obj is tfhirexamplescenarioProcess then
  begin
    TabControl2.tabindex := 4;
    UpdateProcess.enabled := true;
    Edit4.text := tfhirexamplescenarioProcess(obj).title;
    Memo2.text := tfhirexamplescenarioProcess(obj).description;
    Memo6.text := tfhirexamplescenarioProcess(obj).preConditions;
    Memo7.text := tfhirexamplescenarioProcess(obj).postConditions;
  end;
  if obj is tfhirexamplescenarioProcessStep then
  begin
    TabControl2.tabindex := 5;
    if tfhirexamplescenarioProcessStep(TVStructure.Selected.TagObject).pause then
    chkPause.ischecked:= true else chkPause.ischecked:= false;
  end;
  if obj is tfhirexamplescenarioProcessStepAlternative then
  begin
    TabControl2.tabindex := 6;
    UpdateAlternative.enabled := true;
    Edit12.text := tfhirexamplescenarioProcessStepAlternative(obj).title;
  end;
// GDG - needs fixing   if obj is tfhirexamplescenarioProcessStepAlternativeOption then
//  begin
//    TabControl2.tabindex := 7;
//    UpdateOption.enabled := true;
////    Edit13.text := tfhirexamplescenarioProcessStepAlternativeOption(obj).name;
//    Memo5.text := tfhirexamplescenarioProcessStepAlternativeOption(obj).description;
//  end;
  if obj is tfhirexamplescenarioProcessStepOperation then
  begin
    TabControl2.tabindex := 8;
    UpdateOperation.enabled := true;
    Edit14.text := tfhirexamplescenarioProcessStepOperation(obj).name;
    Edit15.text := tfhirexamplescenarioProcessStepOperation(obj).number;
    Edit16.text := tfhirexamplescenarioProcessStepOperation(obj).type_;
    Memo8.text := tfhirexamplescenarioProcessStepOperation(obj).description;

    ComboBox4.Items.Clear;
    ComboBox4.Items.add('(none)');
    ComboBox5.Items.add('(none)');
    ComboBox5.Items.Clear;


    for i := 0 to tfhirexamplescenario(resource).actorList.Count - 1 do
      ComboBox4.Items.add(tfhirexamplescenario(resource).actorList[i].actorId);
    for i := 0 to tfhirexamplescenario(resource).actorList.Count - 1 do
      ComboBox5.Items.add(tfhirexamplescenario(resource).actorList[i].actorId);
    ComboBox4.ItemIndex := ComboBox4.Items.IndexOf(tfhirexamplescenarioProcessStepOperation(obj).initiator);
    ComboBox5.ItemIndex := ComboBox5.Items.IndexOf(tfhirexamplescenarioProcessStepOperation(obj).receiver);

    ComboBox6.Items.Clear;
    ComboBox7.Items.Clear;
    ComboBox6.Items.add('(none)');
    ComboBox7.Items.add('(none)');
    for i := 0 to tfhirexamplescenario(resource).instanceList.Count - 1 do
      ComboBox6.Items.add(tfhirexamplescenario(resource).instanceList[i].resourceId);
    for i := 0 to tfhirexamplescenario(resource).instanceList.Count - 1 do
      ComboBox7.Items.add(tfhirexamplescenario(resource).instanceList[i].resourceId);

    ComboBox6.ItemIndex := 0;
    if tfhirexamplescenarioProcessStepOperation(obj).request <> nil then
      ComboBox6.ItemIndex := ComboBox6.Items.IndexOf(tfhirexamplescenarioProcessStepOperation(obj).request.resourceId);

    ComboBox7.ItemIndex := 0;
    if tfhirexamplescenarioProcessStepOperation(obj).response <> nil then
      ComboBox7.ItemIndex := ComboBox7.Items.IndexOf(tfhirexamplescenarioProcessStepOperation(obj).response.resourceId);

  end;

  TabControl2.ActiveTab.Visible := true;
    TabControl2.Repaint;
end;


procedure TExampleScenarioEditorFrame.tvStructureChange(Sender: TObject);
var
  obj: tFHIRObject;
begin
  if tvStructure.Selected <> nil then
  begin
    obj := tFHIRObject(tvStructure.Selected.tagObject);
    showTab(obj);
  end;
end;




procedure TExampleScenarioEditorFrame.VersionClick(Sender: TObject);
var
  version: tfhirexamplescenarioInstanceVersion;
begin
  version := tfhirexamplescenarioInstanceVersion.Create;
  tfhirexamplescenarioinstance(TVStructure.Selected.TagObject).versionList.AddItem(version);
  ResourceIsDirty := true;
  ReloadTreeview(tvStructure.Selected);
end;

procedure TExampleScenarioEditorFrame.DeleteItemClick(Sender: TObject);
var
  prt: TTreeViewItem;
  obj: tFHIRObject;

begin
  ResourceIsDirty := true;
  prt := tvStructure.Selected.ParentItem;
  obj := TFHIRObject(TVStructure.Selected.TagObject);

  if obj is tfhirexamplescenarioActor then
    TFHIRExampleScenario(tvStructure.Selected.ParentItem.tagObject).actorList.DeleteByReference(TFslObject(TVStructure.Selected.TagObject));

  if obj is tfhirexamplescenarioinstance then
    TFHIRExampleScenario(tvStructure.Selected.ParentItem.TagObject).instanceList.DeleteByReference(TFSLObject(TVStructure.Selected.TagObject));

  if obj is tfhirexamplescenarioProcess then
  begin
    if tvStructure.Selected.ParentItem.TagObject is TFHIRExampleScenario then
    begin
      TFHIRExampleScenario(tvStructure.Selected.ParentItem.TagObject).processList.DeleteByReference(TFSLObject(TVStructure.Selected.TagObject));
    end
    else if tvStructure.Selected.ParentItem.TagObject is tfhirexamplescenarioProcessStep then
    begin
      tfhirexamplescenarioProcessStep(tvStructure.Selected.ParentItem.TagObject).processList.DeleteByReference(TFSLObject(TVStructure.Selected.TagObject))
    end;
  end;

  if obj is tfhirexamplescenarioProcessStep then
  begin
    tfhirexamplescenarioProcessStep(TVStructure.Selected.TagObject).dropEmpty;
    if tvStructure.Selected.ParentItem.TagObject is tfhirexamplescenarioProcess then
      tfhirexamplescenarioProcess(tvStructure.Selected.ParentItem.TagObject).stepList.DeleteByReference(TFSLObject(TVStructure.Selected.TagObject));
  if tvStructure.Selected.ParentItem.TagObject is tfhirexamplescenarioProcessStepAlternative then
      tfhirexamplescenarioProcessStepAlternative(tvStructure.Selected.ParentItem.TagObject).stepList.DeleteByReference(TFSLObject(TVStructure.Selected.TagObject));
  end;


  if obj is tfhirexamplescenarioProcessStepAlternative then
  begin
    tfhirexamplescenarioProcessStepAlternative(TVStructure.Selected.TagObject).dropEmpty;
    TFHIRExampleScenarioProcessStep(tvStructure.Selected.ParentItem.TagObject).deleteProperty('alternative', nil);
  end;

// GDG - needs fixing   if obj is tfhirexamplescenarioProcessStepAlternativeOption then
//  begin
//    tfhirexamplescenarioProcessStepAlternativeOption(TVStructure.Selected.TagObject).dropEmpty;
//
//  end;

  if obj is tfhirexamplescenarioProcessStepOperation then
  begin
    tfhirexamplescenarioProcessStepOperation(TVStructure.Selected.TagObject).dropEmpty;
    TFHIRExampleScenarioProcessStep(tvStructure.Selected.ParentItem.TagObject).deleteProperty('operation', nil);
  end;


  ReloadTreeview(prt);

end;



procedure TExampleScenarioEditorFrame.MoveUpClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirexamplescenario(resource).actorList.IndexOf(tfhirexamplescenarioActor(TVStructure.Selected.TagObject));
  if idx > 0 then begin
  tfhirexamplescenario(resource).actorList.Exchange(idx, idx - 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index - 1;
  end;
  ReloadTreeview(tvStructure.Selected);
end;



procedure TExampleScenarioEditorFrame.MoveDownClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirexamplescenario(resource).actorList.IndexOf(tfhirexamplescenarioActor(TVStructure.Selected.TagObject));
  if idx < tfhirexamplescenario(resource).actorList.Count - 1 then begin
  tfhirexamplescenario(resource).actorList.Exchange(idx, idx + 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index + 1;
  end;
  ReloadTreeview(tvStructure.Selected);

end;



procedure TExampleScenarioEditorFrame.UpdateExampleScenarioClick(Sender: TObject);
var
  obj: TFHIRExampleScenario;

begin
  obj := tfhirexamplescenario(TTreeViewItem(tvStructure.Selected).TagObject);
  if obj = nil then
    exit;
  obj.name := edtESName.text;
  (obj).title := edtESURL.text;
  (obj).status := TfHIRPublicationStatusEnum(ComboBox2.ItemIndex);
  obj.experimental := CheckBox1.IsChecked;
  obj.id := edtESid.text;
  obj.date := TFslDateTime.makeLocal(DateEdit1.DateTime);
  obj.version := Edit8.text;
  obj.publisher := Edit7.text;
  obj.purpose := Memo3.text;

end;

//6.2
procedure TExampleScenarioEditorFrame.UpdateProcessClick(Sender: TObject);
var
  obj: tfhirexamplescenarioProcess;

begin

  obj := tfhirexamplescenarioProcess(TTreeViewItem(tvStructure.Selected).TagObject);
  if obj = nil then
    exit;
  obj.title := Edit4.text;
  obj.description := Memo2.text;
  obj.preConditions := Memo6.text;
  obj.postConditions := Memo7.text;

end;

//6.3
procedure TExampleScenarioEditorFrame.UpdateActorClick(Sender: TObject);
var
  obj: tfhirexamplescenarioActor;

begin
  obj := tfhirexamplescenarioActor(TTreeViewItem(tvStructure.Selected).TagObject);
  if obj = nil then
    exit;
  obj.name := Edit3.text;
  obj.type_ := TfHIRExampleScenarioActorTypeEnum(ComboBox1.ItemIndex);
  obj.actorId := Edit2.text;
  obj.description := Memo1.text;
end;

//6.4
procedure TExampleScenarioEditorFrame.UpdateInstanceClick(Sender: TObject);
var
  obj: tfhirexamplescenarioinstance;

begin
  obj := tfhirexamplescenarioinstance(TTreeViewItem(tvStructure.Selected).TagObject);
  if obj = nil then
    exit;
  obj.resourceId := Edit10.text;
  obj.resourceType := TFHIRResourceTypesEnum(ComboBox3.ItemIndex);
  obj.name := Edit9.text;
  obj.description := Memo4.text;

end;

//6.5
procedure TExampleScenarioEditorFrame.UpdateAlternativeClick(Sender: TObject);
var
  obj: tfhirexamplescenarioProcessStepAlternative;

begin
  obj := tfhirexamplescenarioProcessStepAlternative(TTreeViewItem(tvStructure.Selected).TagObject);
  if obj = nil then
    exit;
    obj.title := Edit12.text;
end;

//6.6
procedure TExampleScenarioEditorFrame.UpdateOptionClick(Sender: TObject);
//GDG - needs fixing  var
//  obj: tfhirexamplescenarioProcessStepAlternativeOption;

begin
//  obj := tfhirexamplescenarioProcessStepAlternativeOption(TTreeViewItem(tvStructure.Selected).TagObject);
//  if obj = nil then
//    exit;
//  (obj).description := Memo5.text;
//  (obj).name := edit13.text;

end;

//6.7
procedure TExampleScenarioEditorFrame.UpdateOperationClick(Sender: TObject);
var
  obj: tfhirexamplescenarioProcessStepOperation;

begin

    obj := tfhirexamplescenarioProcessStepOperation(TTreeViewItem(tvStructure.Selected).tagObject);
    if obj = nil then Exit;

    (obj).name := Edit14.text;
    (obj).number := Edit15.text;
    (obj).type_ := Edit16.text;
    (obj).description := Memo8.text;

if ComboBox4.ItemIndex<>-1 then (obj).initiator := ComboBox4.Items[ComboBox4.ItemIndex];
if ComboBox5.ItemIndex<>-1 then (obj).receiver := ComboBox5.Items[ComboBox5.ItemIndex];

//    obj.request.Free;
    if ComboBox6.ItemIndex <> -1 then
    begin
    if obj.request = nil then
    obj.request := TFhirExampleScenarioInstanceContainedInstance.Create;
    obj.request.resourceId := ComboBox6.Items[ComboBox6.ItemIndex];
    end;

end;








end.


