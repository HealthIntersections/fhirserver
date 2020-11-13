unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TreeView, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.TabControl, FMX.Edit,
  fhir_objects, System.ImageList, FMX.ImgList, FMX.Menus, FMX.ScrollBox, FMX.Memo,
  FMX.DateTimeCtrls, FMX.ListBox,   fsl_utilities  , fsl_collections   ,
  fhir4_factory , fhir4_resources, fhir4_types , Fhir.R4.Utilities, FHIR.FMX.Ctrls ;

const
 ButtonColumns=3;

type


  TFHIRTreeViewItem = class(TTreeViewItem)
  private
    fFhirObject: tFHIRObject;
  public
    property FhirObject: tFHIRObject read fFhirObject write fFhirObject;
  end;


  TFHIRButton=Class(TButton)
    private
      { Private declarations }
    public
      { Public declarations }
  //  property multiple: integer; // this is to indicate that the button can be clicked even if the asset exists. In other words if it will be a backbone element or an attribute. maybe replaced by the lower line if we can make that work.
  //  property objtype: string; // this don't work. How can we make this in a way that when we call the code, it will instanciate one ofject of the type specified here??
  end;


  TForm1 = class(TForm)
    TreeView1: TTreeView;
    ExampleScenario: TTreeViewItem;
    Actor: TTreeViewItem;
    Instance: TTreeViewItem;
    Process: TTreeViewItem;
    Version: TTreeViewItem;
    Step: TTreeViewItem;
    Operation: TTreeViewItem;
    Alternative: TTreeViewItem;
    Option: TTreeViewItem;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    Button2: TButton;
    actorName: TTreeViewItem;
    versionid: TTreeViewItem;
    versiondescription: TTreeViewItem;
    FramedScrollBox2: TFramedScrollBox;
    esUrl: TTreeViewItem;
    esIdentifier: TTreeViewItem;
    esVersion: TTreeViewItem;
    esName: TTreeViewItem;
    esPublisher: TTreeViewItem;
    esCopyright: TTreeViewItem;
    esPurpose: TTreeViewItem;
    esExperimental: TTreeViewItem;
    actorDescription: TTreeViewItem;
    Button1: TButton;
    Button4: TButton;
    Edit1: TEdit;
    StyleBook1: TStyleBook;
    FramedScrollBox1: TFramedScrollBox;
    ResourceTV: TTreeView;
    Button3: TButton;
    Button5: TButton;
//    Procedure createTab(element: TTreeViewItem; tab: TTabItem);
    Procedure createObjUIGroup(obj: TFHIRObject; objectTypeName:String; tab: TTabItem);
    Procedure createPropUIGroup(obj: TFHIRProperty; parentPrefix:string; tab: TTabItem);
    procedure createEntry(labelpos: tAlignLayout; FHIRProp: TFHIRProperty; propname, labeltext, datatype: string; parentframe: TFramedScrollBox);
    procedure Button2Click(Sender: TObject);
    procedure addButton(labelpos: tAlignLayout; labeltext: string; parentframe: TScrollBox; bNumber:integer);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ReloadTreeview(resource:TFHIRResource; treeview:TTreeView; sel_item: TFHIRTreeViewItem);
    procedure showTab(tabcontrol:TTabControl; obj: tFHIRObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function objectcount(obj:TFHIRObject): integer;
  end;

var
  Form1: TForm1;
  resource: TFHIRObject;
  ffactory:TFHIRFactoryR4;


implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
begin end
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
{
  ExampleScenario.Stylesdata['type'] := 'object';
  esUrl.Stylesdata['type'] := 'attribute';
  esUrl.Stylesdata['datatype'] := 'string';
  esIdentifier.Stylesdata['type'] := 'attribute';
  esIdentifier.Stylesdata['datatype'] := 'string';
  esVersion.Stylesdata['type'] := 'attribute';
  esVersion.Stylesdata['datatype'] := 'string';
  esName.Stylesdata['type'] := 'attribute';
  esName.Stylesdata['datatype'] := 'string';
  esPublisher.Stylesdata['type'] := 'attribute';
  esPublisher.Stylesdata['datatype'] := 'string';
  esCopyright.Stylesdata['type'] := 'attribute';
  esCopyright.Stylesdata['datatype'] := 'markdown';
  esPurpose.Stylesdata['type'] := 'attribute';
  esPurpose.Stylesdata['datatype'] := 'markdown';

  Actor.Stylesdata['type'] := 'object';
  actorName.Stylesdata['type'] := 'attribute';
  actorName.Stylesdata['datatype'] := 'string';
  actorDescription.Stylesdata['type'] := 'attribute';
  actorDescription.Stylesdata['datatype'] := 'markdown';

  Instance.Stylesdata['type'] := 'object';
  Version.Stylesdata['type'] := 'object';
  versionid.Stylesdata['type'] := 'attribute';
  versionid.Stylesdata['datatype'] := 'string';
  versionid.Stylesdata['position'] := 1;
  versiondescription.Stylesdata['type'] := 'attribute';
  versiondescription.Stylesdata['datatype'] := 'markdown';
  versiondescription.Stylesdata['position'] := 2;
  Process.Stylesdata['type'] := 'object';
  Step.Stylesdata['type'] := 'object';
  Operation.Stylesdata['type'] := 'object';
  Alternative.Stylesdata['type'] := 'object';
  Option.Stylesdata['type'] := 'object';

  createTab(TreeView1.Selected, TabItem1);
 }
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  ExampleScenario: TFHIRExampleScenario;
begin
  resourceToFile(TFHIRResource(resource), 'c:\temp\asdasd.xml', ffXml, OutputStylePretty);
//  ReloadTreeview(TFHIRResource(resource), ResourceTV, TFHIRTreeviewItem(ResourceTV.Selected));
//  showTab(TFHIRTreeViewItem(ResourceTV.Selected).fFhirObject);

end;

procedure TForm1.Button4Click(Sender: TObject);
begin
//TFHIRObject.Create;
ffactory:=TfhirfactoryR4.Create;
resource:=ffactory.makeByName(edit1.text);
ffactory.Assign(resource);
  createObjUIGroup(resource, edit1.text, TabItem1);

end;

procedure TForm1.Button5Click(Sender: TObject);
begin
//enumcontrols(tabcontrol1.Tabs[1]);
showtab(tabcontrol1,resource);
end;

function TForm1.objectcount(obj:TFHIRObject): integer;
var PL:TFHIRPropertyList;
begin
  PL:= ffactory.createPropertyList(obj.ClassName,true)     ;
  result:=PL.Count;
  PL.Destroy
end;

Procedure TForm1.createObjUIGroup(obj: TFHIRObject; objectTypeName:String; tab: TTabItem);
var
  btns,
  j, i: integer;
  curr_height: single;
  T: TTabItem;
  Fr: TFramedScrollBox;
  frb: TScrollBox;
  edt: TEdit;
  lbl: TLabel;
  mem: TMemo;
  childProperty: TFHIRProperty;
  B: TButton;
  PL:TFHIRPropertyList;
  st:string;

begin
  T := TTabItem.Create(TabControl1);
  T.text := obj.fhirtype;
  T.Parent := TabControl1;

  frb := TScrollBox.Create(T);
  frb.Parent := T;
  frb.Height := 23;
  frb.Margins.Top := 5;
  frb.Margins.bottom := 5;
  frb.Align := tAlignLayout.Top;
  frb.ShowScrollBars := False;

  Fr := TFramedScrollBox.Create(T);
  Fr.Align := tAlignLayout.Client;
  Fr.Padding.Top := 7;
  Fr.Padding.left := 4;
  Fr.Padding.right := 4;
  Fr.Padding.bottom := 7;
  Fr.ShowScrollBars := true;
  Fr.Parent := T;
  btns:=0;
  ffactory:=TfhirfactoryR4.Create;
  ffactory.Assign(obj);

  st:=obj.fhirType;
//  st:='ExampleScenario';

  PL:= ffactory.createPropertyList(st,true)     ;

  for i := 0 to PL.Count-1 do
  begin
    childProperty := PL.Properties[i];
    if PL.Properties[i].Type_ ='' then
    begin
      inc(btns);
      addButton(tAlignLayout.top, PL.Properties[i].Name, Frb, btns);
      //recurse
      createPropUIGroup(PL.Properties[i], st, T);

    end;
    if PL.Properties[i].Type_ <>'' then
    begin
      if PL.Properties[i].Type_ = 'string' then
      begin
//        obj.createPropertyValue(PL.Properties[i].Name);
obj.
        childProperty:=TFHIRProperty(obj.createPropertyValue(PL.Properties[i].Name));
        createEntry(tAlignLayout.left, childProperty , PL.Properties[i].Name, PL.Properties[i].Name, 'string', Fr);
      end;
    end;
  end;
end;



Procedure TForm1.createPropUIGroup(obj: TFHIRProperty; parentPrefix:string; tab: TTabItem);
var
  btns,
  j, i: integer;
  curr_height: single;
  T: TTabItem;
  Fr: TFramedScrollBox;
  frb: TScrollBox;
  edt: TEdit;
  lbl: TLabel;
  mem: TMemo;
  childProperty: TFHIRProperty;
  B: TButton;
  PL:TFHIRPropertyList;
  st:string;

begin
  T := TTabItem.Create(TabControl1);
  T.text := obj.Name;
  T.Parent := TabControl1;

  frb := TScrollBox.Create(T);
  frb.Parent := T;
  frb.Height := 23;
  frb.Margins.Top := 5;
  frb.Margins.bottom := 5;
  frb.Align := tAlignLayout.Top;
  frb.ShowScrollBars := False;

  Fr := TFramedScrollBox.Create(T);
  Fr.Align := tAlignLayout.Client;
  Fr.Padding.Top := 7;
  Fr.Padding.left := 4;
  Fr.Padding.right := 4;
  Fr.Padding.bottom := 7;
  Fr.ShowScrollBars := true;
  Fr.Parent := T;
  btns:=0;
  ffactory:=TfhirfactoryR4.Create;
  ffactory.Assign(obj);

  st:=parentPrefix+'.'+obj.Name;

  PL:= ffactory.createPropertyList(st,true)     ;

  for i := 0 to PL.Count-1 do
  begin
    childProperty := PL.Properties[i];
    if PL.Properties[i].Type_ ='' then
    begin
      inc(btns);
      addButton(tAlignLayout.top, PL.Properties[i].Name, Frb, btns);
      createPropUIGroup(PL.Properties[i], st, T);
    end;
    if PL.Properties[i].Type_ <>'' then
    begin
      if PL.Properties[i].Type_ = 'string' then
      createEntry(tAlignLayout.left, PL.Properties[i], PL.Properties[i].Name, PL.Properties[i].Name, 'string', Fr);
    end;
  end;
end;



procedure TForm1.createEntry(labelpos: tAlignLayout; FHIRProp: TFHIRProperty; propname, labeltext, datatype: string; parentframe: TFramedScrollBox);
var
  edt: TFHIRStringEdit;
  mem: TMemo;
begin
  if datatype = 'string' then
  begin
    edt := TFHIRStringEdit.Create(parentframe);
    edt.Parent:=parentframe;
    FHIRProp:= TFHIRProperty(edt.associate(TFHIRString(FHIRProp)));
    edt.FHIRPropertyName:=propname;
  end;

  if datatype = 'memo' then
  begin
//    fr2.Height := 133;
//    mem := TMemo.Create(fr4);
//    mem.Parent := fr4;
//    mem.Align := tAlignLayout.Client;
  end;

end;


procedure TForm1.addButton(labelpos: tAlignLayout; labeltext: string; parentframe: TScrollBox; bNumber:integer);

var
  b: TButton;
  fr2, fr3, fr4: TScrollBox;
  lbl: TLabel;
  edt: TEdit;
  mem: TMemo;

begin
  B := TButton.Create(parentframe);
  B.text := Format('Button %d', [1]);
  B.text := labeltext;
  B.Parent := parentframe;
  B.Height := 23;
  B.Width := 100;
  B.position.x := 27 + ((bNumber-1) mod ButtonColumns) * 121;
  B.position.y := 10  + ((bNumber-1) div ButtonColumns) * 27;
  B.OnClick := Button1Click;
  parentframe.Height:= 41 + ((bNumber-1) div ButtonColumns) * 25;

end;


Procedure TForm1.ReloadTreeview(resource:TFHIRResource; treeview:TTreeView; sel_item: TFHIRTreeViewItem);
var
  current_item: TFHIRTreeViewItem;
  i: integer;
  Actor: tfhirexamplescenarioActor;
  Instance: tfhirexamplescenarioinstance;
  sel_index: integer;
  sel_text: string;

begin
  sel_index := -1;
  sel_text := '';
  if sel_item <> nil then sel_index := sel_item.GlobalIndex;
  treeview.Clear;
//  current_item := addTVItem(treeview, nil, 'examplescenario', 'Example Scenario', resource);
  if sel_index <> -1 then treeview.Selected := TreeView1.ItemByGlobalIndex(sel_index);
  showTab(TabControl1, TFHIRTreeViewItem(treeview.Selected).FhirObject);
end;







procedure TForm1.showTab(tabcontrol:TTabControl; obj: tFHIRObject);
var
  i: integer;
  ExampleScenario2:TFHIRExampleScenario;
  list:TFMXChildrenList;
  fmxobj:TFMXObject;
  st:string;
  sbox:TScrollBox;
  fmxctrl:TControl;
  fmxeditor:TControl;
  currProperty:TFHIRPrimitiveType;
begin

  if obj is TFHIRExampleScenario then
  begin
    tabcontrol.tabindex := 1;
    for fmxctrl in tabcontrol.ActiveTab.Controls[1].Controls[1].Controls[1].Controls do begin
      if fmxctrl is TScrollBox then
      if fmxctrl.Controls[1].Controls[2].Controls[1].Controls[0] is TFHIRStringEdit then begin
         fmxeditor:=fmxctrl.Controls[1].Controls[2].Controls[1].Controls[0];
         st:=TFHIRStringEdit(FMXEditor).FHIRPropertyName;
//         TFHIRExampleScenario(obj).nameElement:=TFHIRStringEdit(FMXEditor).associate(TFHIRString(TFHIRExampleScenario(obj).nameElement));
      end;
    end;
// working code before (for ref)     TFHIRExampleScenario(obj).nameElement:= edit5.associate(TFHIRExampleScenario(obj).nameElement);

  end;

end;







end.
