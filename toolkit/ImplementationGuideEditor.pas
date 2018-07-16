unit ImplementationGuideEditor;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.types,
  System.UITypes,
  FHIR.Base.Objects,
  FHIR.R4.Types,
  FHIR.R4.Resources,
  FHIR.R4.Utilities,
  FHIR.Support.Utilities,
  System.ImageList, FMX.Types, FMX.ScrollBox, System.Classes,
  FMX.Controls.Presentation, FMX.DateTimeCtrls, FMX.Edit,
  FMX.Memo, FMX.TreeView, FMX.Dialogs, FMX.DialogService,
  FMX.StdCtrls, FMX.ListBox, FMX.Layouts, FMX.TabControl,
  FMX.Menus, FMX.WebBrowser, FMX.Controls, FMX.Forms, IGSettings, FMX.ImgList,

  shellapi, winapi.windows,fmx.platform.win,
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
  private
    { Private declarations }
  public
    { Public declarations }
   procedure ConcatenateFiles(const InFileNames: array of string; const OutFileName: string);
   Destructor Destroy; override;
   procedure load;  override;
   procedure commit; override;
   procedure cancel;  override;
  end;


var
  ImplementationGuideEditorFrame: TImplementationGuideEditorFrame;
//  resource: TFHIRImplementationGuide;
  fwbinfolder, binfolder, igrootfolder, filename: string;
  internal: boolean;
  resourceIsDirty:Boolean;

implementation

{$R *.fmx}


procedure TImplementationGuideEditorFrame.cancel;
begin
end;

procedure TImplementationGuideEditorFrame.commit;
begin
{if tvStructure.Selected = tvMetadata then
    CommitMetadata;
  if tvStructure.Selected = tvSDC then
    CommitSDC;
}  ResourceIsDirty := true;

end;




Destructor TImplementationGuideEditorFrame.Destroy;
begin
  inherited;
end;

procedure TImplementationGuideEditorFrame.Edit4DragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  edit4.Text:=ttabitem(data.source).Text;
end;

procedure TImplementationGuideEditorFrame.Edit4DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  Operation:=TDragOperation.Link;
end;


///////////////////////////////////////////////////////////////////////////////
// 1. 1 recursive procedure to add a FHIRObject to a treeview
// each part must know what to recurse - e.g. if it is an exampleScenario it must check the actors, resources, and processes
// so we could scan this from the structureDefinition
///////////////////////////////////////////////////////////////////////////////

function TImplementationGuideEditorFrame.addTVItem(TreeView: TTreeView; parent: TTreeViewItem; itemType, text: string; obj: tFHIRObject): TTreeViewItem;
var
  current_item, Item: TTreeViewItem;
  i: integer;
  objtype:string;
begin

  Item := TTreeViewItem.Create(TreeView);
  objtype:=obj.fhirType;
  TreeView.AddObject(Item);
  if (parent = nil) then
    item.Parent:=TreeView
  else
    item.Parent:=parent;

  Item.ImageIndex:=-1;
  if itemType = 'page' then Item.ImageIndex:=9;
  if itemType = 'manifest' then Item.ImageIndex:=10;
  if itemType = 'definition' then Item.ImageIndex:=11;
  if itemType = 'resource' then Item.ImageIndex:=12;
  if itemType = 'package' then Item.ImageIndex:=15;
  if itemType = 'implementationguide' then Item.ImageIndex:=14;

  Item.StylesData['attrType'] := itemType;
  Item.StylesData['sortOrder'] := 1;
  TreeView.Selected := Item;
  Item.Height := 20;
  Item.text := text;
  Item.tagObject := obj;

// do this for the object - any object type:
  if obj is tfhirImplementationGuide then
  begin
    tab.Text:=text ;
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
      addTVItem(TreeView, current_item, 'package', tfhirImplementationGuideDefinition(obj).packageList[i].name, tfhirImplementationGuideDefinition(obj).packageList[i]);

    for i := 0 to tfhirImplementationGuideDefinition(obj).resourceList.Count - 1 do
      addTVItem(TreeView, current_item, 'resource', tfhirImplementationGuideDefinition(obj).resourceList[i].name, tfhirImplementationGuideDefinition(obj).resourceList[i]);

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
      addTVItem(TreeView, current_item, 'page', tfhirImplementationGuideDefinitionPage(obj).pageList[i].title , tfhirImplementationGuideDefinitionPage(obj).pageList[i]);
    end;


  end;

  result := Item;


end;




procedure TImplementationGuideEditorFrame.autopreviewClick(Sender: TObject);
begin
  autopreview.IsChecked:=not autopreview.IsChecked;

end;

procedure TImplementationGuideEditorFrame.FormShow(Sender: TObject);
begin
  filename := '';
  binfolder:=getCurrentDir;


  if TVstructure.globalCount = 0 then
  begin
    if not Assigned(resource) then
      NewImplementationGuideClick(Self);
  end;


  tabcontrol1.TabHeight:=1;
  tabcontrol2.TabHeight:=1;

// form1.Caption:= inttostr(trunc(double(now)));
//if trunc(double(now)) > 43275 then begin
//  ShowMessage('This is a preview. Please check the FHIR toolkit for official release and updates.');
//close;
//end;
end;

procedure TImplementationGuideEditorFrame.packageDownClick(Sender: TObject);
var
  idx: integer;
begin

  idx := tfhirImplementationGuideDefinition(TVStructure.Selected.ParentItem.TagObject).packageList.IndexOf(tfhirImplementationGuideDefinitionPackage(TVStructure.Selected.TagObject));

  if idx < tfhirImplementationGuideDefinition(TVStructure.Selected.ParentItem.TagObject).packageList.Count - 1 then begin
  tfhirImplementationGuideDefinition(TVStructure.Selected.ParentItem.TagObject).packageList.Exchange(idx, idx + 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index + 1;
  ResourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);

 end;

procedure TImplementationGuideEditorFrame.packageUpClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirImplementationGuideDefinition(TVStructure.Selected.ParentItem.TagObject).packageList.IndexOf(tfhirImplementationGuideDefinitionPackage(TVStructure.Selected.TagObject));
  if idx > 0 then begin
  tfhirImplementationGuideDefinition(TVStructure.Selected.ParentItem.TagObject).packageList.Exchange(idx, idx - 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index - 1;
  ResourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);


end;

procedure TImplementationGuideEditorFrame.pageDownClick(Sender: TObject);
var
  idx: integer;
begin

  if not (TVStructure.Selected.ParentItem.TagObject is tfhirImplementationGuideDefinitionPage) then exit;


  idx := tfhirImplementationGuideDefinitionPage(TVStructure.Selected.ParentItem.TagObject).pageList.IndexOf(tfhirImplementationGuideDefinitionPage(TVStructure.Selected.TagObject));

  if idx < tfhirImplementationGuideDefinitionPage(TVStructure.Selected.ParentItem.TagObject).pageList.Count - 1 then begin
  tfhirImplementationGuideDefinitionPage(TVStructure.Selected.ParentItem.TagObject).pageList.Exchange(idx, idx + 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index + 1;
  ResourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);


end;

procedure TImplementationGuideEditorFrame.pageUpClick(Sender: TObject);
var
  idx: integer;
begin

  if not (TVStructure.Selected.ParentItem.TagObject is tfhirImplementationGuideDefinitionPage) then exit;


  idx := tfhirImplementationGuideDefinitionPage(TVStructure.Selected.ParentItem.TagObject).pageList.IndexOf(tfhirImplementationGuideDefinitionPage(TVStructure.Selected.TagObject));
  if idx > 0 then begin
  tfhirImplementationGuideDefinitionPage(TVStructure.Selected.ParentItem.TagObject).pageList.Exchange(idx, idx - 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index - 1;
  ResourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);

end;


procedure TImplementationGuideEditorFrame.load;
begin

  inherited;

  filename := '';
  binfolder:=getCurrentDir;

  if TVstructure.globalCount = 0 then
  begin
    if not Assigned(resource) then
      NewImplementationGuideClick(Self)
    else begin
     tvStructure.beginUpdate;
     try
      addTVItem(tvStructure, nil, 'implementationguide', TFHIRImplementationGuide(resource).name, resource);
     finally
     tvStructure.EndUpdate;
     tvstructure.Repaint;
     end;
    end;
//    webbrowser1.Enabled:=false;
     application.ProcessMessages;
    tvstructure.Selected:=tvstructure.ItemByGlobalIndex(0);

	 
  end;

  tabcontrol1.TabHeight:=1;
  tabcontrol2.TabHeight:=1;

//  showTab(TFHIRObject(TFHIRObject(TVStructure.Selected.TagObject)));

end;



procedure TImplementationGuideEditorFrame.CornerButton1Click(Sender: TObject);
begin
  TDialogService.MessageDialog('Not Implemented yet', System.UITypes.TMsgDlgType.mtInformation, [System.UITypes.TMsgDlgBtn.mbOk], System.UITypes.TMsgDlgBtn.mbOk, 0, nil, nil);

end;

procedure TImplementationGuideEditorFrame.CornerButton4Click(Sender: TObject);
var
  package: tfhirImplementationGuideDefinitionPackage;
begin
  package := tfhirImplementationGuideDefinitionPackage.Create;
  Package.name:='Package';

  if TVStructure.Selected.TagObject is tfhirImplementationGuideDefinition then
    tfhirImplementationGuideDefinition(TVStructure.Selected.TagObject).packageList.Add(Package);

    ResourceIsDirty := true;
      ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.CornerButton5Click(Sender: TObject);
var res :tfhirImplementationGuideDefinitionResource;
    resref :tfhirReference;


begin
  res := tfhirImplementationGuideDefinitionResource.Create;
  res.name:='resource';
  res.reference:=tfhirReference.Create;

  tfhirImplementationGuideDefinition(TVStructure.Selected.TagObject).resourceList.Add(res);
  ResourceIsDirty := true;
  ReloadTreeview(TVStructure.Selected);


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
  Definition: tfhirImplementationGuideDefinition;

begin
  Definition := tfhirImplementationGuideDefinition.Create;
  tfhirImplementationGuide(TVStructure.Selected.TagObject).definition:=Definition;
  ResourceIsDirty := true;
  ReloadTreeview(TVStructure.Selected);

end;


procedure TImplementationGuideEditorFrame.btnNewPageClick(Sender: TObject);
var
  page: tfhirImplementationGuideDefinitionPage;
begin

  if TVStructure.Selected.TagObject is tfhirImplementationGuideDefinition then if tfhirImplementationGuideDefinition(TVStructure.Selected.TagObject).page <> nil then exit;


  Page := tfhirImplementationGuideDefinitionPage.Create;
  Page.title:='Page';

  if TVStructure.Selected.TagObject is tfhirImplementationGuideDefinition then
    tfhirImplementationGuideDefinition(TVStructure.Selected.TagObject).page:=Page;

  if TVStructure.Selected.TagObject is tfhirImplementationGuideDefinitionPage then
    tfhirImplementationGuideDefinitionPage(TVStructure.Selected.TagObject).pageList.Add(Page);

    ResourceIsDirty := true;
      ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.btnProcess2Click(Sender: TObject);
begin
end;

//2.5 Main process
procedure TImplementationGuideEditorFrame.NewImplementationGuideClick(Sender: TObject);

var
  ImplementationGuide: TFHIRImplementationGuide;
begin
  ImplementationGuide := TFHIRImplementationGuide.create;
  ImplementationGuide.name := 'Implementation Guide';
  resource := ImplementationGuide;
  ReloadTreeview(nil);

 //// showTab(TFHIRObject(TFHIRObject(TVStructure.Selected.TagObject)));

//  showTab(exsc);


end;

procedure TImplementationGuideEditorFrame.Button2Click(Sender: TObject);
var
  tempstr, filestr: string;
  SL: TStringList;
begin

    tempstr:=igrootfolder+'\pagecontent\'+ChangeFileExt(edit1.Text, '.xml');

    filestr:=binfolder+'\tmp.html';

  SL := TStringList.Create;
  try
    SL.LoadFromFile(tempstr);
    begin
      SL.Insert(0, '<div xmlns="http://www.w3.org/1999/xhtml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"> <head> <meta http-equiv="Content-Type" content="text/html; charset=utf-8" /> </head>');
      SL.Append('</div>');
    end;

    begin
      SL.Insert(0,'<html><body>');
      SL.Append('</body></html>');
      SL.SaveToFile(filestr);
    end;

  finally
    SL.Free;
  end;

  filestr:= stringreplace(filestr, '\', '/', [rfReplaceAll, rfIgnoreCase]);

  //webbrowser1.Navigate( filestr );

end;

procedure TImplementationGuideEditorFrame.Button6Click(Sender: TObject);
var
  tempstr, filestr: string;
  SL: TStringList;
begin
if True then

    tempstr:=igrootfolder+'\pagecontent\'+ChangeFileExt(edit1.Text, '.xml');

    filestr:=binfolder+'\tmp.html';

  SL := TStringList.Create;
  try
    SL.LoadFromFile(tempstr);
    begin
      SL.Insert(0, '<div xmlns="http://www.w3.org/1999/xhtml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"> <head> <meta http-equiv="Content-Type" content="text/html; charset=utf-8" /> </head>');
      SL.Append('</div>');
    end;

    begin
      SL.Insert(0,'<html><body>');
      SL.Append('</body></html>');
      SL.SaveToFile(filestr);
    end;

  finally
    SL.Free;
  end;

  filestr:= stringreplace(filestr, '\', '/', [rfReplaceAll, rfIgnoreCase]);

  runandwait(igrootfolder, 'open', filestr);
//  webbrowser1.Navigate( filestr );

end;

procedure TImplementationGuideEditorFrame.Button7Click(Sender: TObject);
var
  tempstr, filestr: string;
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
  sCmd,ExecuteFile, ParamString, StartInString: string;
  SL: TStringList;
  InsTextPos: Integer;
  lResultStr: String;
begin
  if fwbinfolder='' then fwbinfolder:=binfolder;
  opendialog2.InitialDir := igrootfolder;

  if opendialog2.Execute then begin
    tempstr:=fwbinfolder+'\tmp.docx';
    copyfile(pwidechar(opendialog2.filename),pwidechar(tempstr), false);
    tempstr:=fwbinfolder+'';

    //runandwait(tempstr, 'pandoc', '--from=docx -s --to=html4 tmp.docx -o tmp.html');
    runandwait(tempstr, 'pandoc', '--extract-media . --from=docx --to=html4 tmp.docx -o tmp.txt');
    //concatenatefiles(['xheader.txt','tmp.txt','xfooter.txt'],'tmp.xml');

  SL := TStringList.Create;
  try
    SL.LoadFromFile(fwbinfolder+'\tmp.txt');
    begin
      SL.Insert(0, '<div xmlns="http://www.w3.org/1999/xhtml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"> <head> <meta http-equiv="Content-Type" content="text/html; charset=utf-8" /> </head>');
      SL.Append('</div>');
      SL.SaveToFile(fwbinfolder+'\tmp.xml');
    end;

    begin
      SL.Insert(0,'<html><body>');
      SL.Append('</body></html>');
      SL.SaveToFile(fwbinfolder+'\tmp.html');
    end;

  finally
    SL.Free;
  end;





    filestr:=fwbinfolder+'\tmp.html';
    filestr:= stringreplace(filestr, '\', '/', [rfReplaceAll, rfIgnoreCase]);
  runandwait(igrootfolder, filestr, filestr);
    tempstr:=fwbinfolder+'\tmp.docx';
    deletefile(pwidechar(tempstr));



    if edit1.Text<>'' then try
      filestr:=fwbinfolder+'\tmp.html';
      if (directoryexists(pwidechar(igrootfolder+'\pages'))) then
        begin
          tempstr:=igrootfolder+'\pages\'+edit1.text+'.xml';
          copyfile(pwidechar(filestr),pwidechar(tempstr), false)
        end
      else
        begin
            TDialogService.MessageDialog('Pages folder not found. Do you want to create it? '+#13#10+'Click No if you want to save in the same folder, cancel to skip saving', System.UITypes.TMsgDlgType.mtInformation,
              [System.UITypes.TMsgDlgBtn.mbYes, System.UITypes.TMsgDlgBtn.mbNo, System.UITypes.TMsgDlgBtn.mbCancel],
              System.UITypes.TMsgDlgBtn.mbYes, 0,
                  procedure(const AResult: TModalResult)
                  begin
                    case AResult of
                      mrYES:
                        begin
                          tempstr:=igrootfolder+'\pages\'+edit1.text+'.xml';
                          createdir(pwidechar(igrootfolder+'\pages'));
                          copyfile(pwidechar(filestr),pwidechar(tempstr), false);
                        end;
                      mrNo:
                        begin
                          tempstr:=igrootfolder+'\'+edit1.text+'.xml';
                          copyfile(pwidechar(filestr),pwidechar(tempstr), true);
                        end;
                    end;
                  end);
        end;
    except
    end;

    SetCurrentDir(binfolder);
  end;



end;


procedure TImplementationGuideEditorFrame.Button8Click(Sender: TObject);
begin
  TDialogService.MessageDialog('Not Implemented yet', System.UITypes.TMsgDlgType.mtInformation, [System.UITypes.TMsgDlgBtn.mbOk], System.UITypes.TMsgDlgBtn.mbOk, 0, nil, nil);
 end;

procedure TImplementationGuideEditorFrame.Button9Click(Sender: TObject);
var dir:string;
   IGSettingsForm:TIGSettingsForm;
begin
  IGSettingsForm:=TIGSettingsForm.create(self);
  IGSettingsForm.fwBinFolder:=fwbinfolder;

  IGSettingsForm.ShowModal;
  if IGSettingsForm.ModalResult=mrOK then

  fwbinfolder:=IGSettingsForm.fwBinFolder;
  IGSettingsForm.destroy;

end;

Procedure TImplementationGuideEditorFrame.ReloadTreeview(sel_item: TTreeViewItem);
var
  current_item: TTreeViewItem;
  i: integer;
  Instance: TFHIRImplementationGuide;
  sel_index: integer;
  sel_text: string;

begin
  sel_index := -1;
  sel_text := '';
  if sel_item <> nil then sel_index := sel_item.GlobalIndex;

//  tvStructure.BeginUpdate;

  try
  tvStructure.Clear;
  current_item := addTVItem(tvStructure, nil, 'implementationguide', 'Implementation Guide', resource);
  finally
//  tvStructure.EndUpdate;
  tvstructure.Repaint;
  end;
//  tvStructure.repaint;
//  application.ProcessMessages;
  if sel_index <> -1 then tvStructure.Selected := tvStructure.ItemByGlobalIndex(sel_index);
//  application.ProcessMessages;
  //// showTab(TFHIRObject(TVStructure.Selected.TagObject));

end;



procedure TImplementationGuideEditorFrame.showTab(obj: tFHIRObject);
var
  selIndex, i: integer;
  ImplementationGuide:TFHIRImplementationGuide;

begin
  selIndex := 0;
//  webbrowser1.Enabled:=false;

  if obj is TFHIRImplementationGuide then
  begin

    btnDefinition.enabled:=true;
    if tfhirImplementationGuide(TVStructure.Selected.TagObject).definition <> nil then btnDefinition.enabled:=false;

    selIndex := 2;
    UpdateImplementationGuide.enabled := true;
    edtESName.text := TFHIRImplementationGuide(obj).name;
    edtESURL.text := TFHIRImplementationGuide(obj).url;
    ComboBox2.ItemIndex := integer(TFHIRImplementationGuide(obj).status);
    if TFHIRImplementationGuide(obj).experimental then
      CheckBox1.IsChecked := true
    else
      CheckBox1.IsChecked := false;
    edtESid.text := TFHIRImplementationGuide(obj).id;
    DateEdit1.text := TFHIRImplementationGuide(obj).date.toString;
    Edit8.text := TFHIRImplementationGuide(obj).packageID;
    Edit7.text := TFHIRImplementationGuide(obj).publisher;
    Memo3.text := TFHIRImplementationGuide(obj).description;
    Edit3.text := TFHIRImplementationGuide(obj).version;

  end;



  if obj is TFHIRImplementationGuideDefinition then
  begin
    selIndex := 1;
    pageUp.Visible:=False;
    pageDown.Visible:=False;
    btnNewPage.Visible:=True;
    if tfhirImplementationGuideDefinition(TVStructure.Selected.TagObject).page <> nil then btnNewPage.Visible:=false;

  end;

  if obj is TFHIRImplementationGuideDefinitionPackage then
  begin
    selIndex := 3;
    UpdatePage.enabled := true;
    Edit12.text := TFHIRImplementationGuideDefinitionPackage(obj).name ;
    Memo2.text := TFHIRImplementationGuideDefinitionPackage(obj).description ;


  end;


  if obj is TFHIRImplementationGuideDefinitionResource then
  begin
    selIndex := 5;
    UpdatePage.enabled := true;
//    Edit1.text := TFHIRImplementationGuideDefinitionPage(obj).name ;

    ComboBox1.Items.Clear;
    for I := 0 to tfhirimplementationguide(resource).definition.packageList.Count-1 do
      ComboBox1.Items.Add(tfhirimplementationguide(resource).definition.packageList[i].name);

    ComboBox1.ItemIndex :=ComboBox1.Items.IndexOf(TFHIRImplementationGuideDefinitionResource(obj).package) ;


    Edit6.Text:=TFHIRImplementationGuideDefinitionResource(obj).reference.reference;
    edit9.Text:=TFHIRImplementationGuideDefinitionResource(obj).name;
    memo4.Text:=TFHIRImplementationGuideDefinitionResource(obj).description;

  end;



  if obj is TFHIRImplementationGuideDefinitionPage then
  begin
    pageUp.Visible:=True;
    pageDown.Visible:=True;
    selIndex := 4;
    UpdatePage.enabled := true;
    if TFHIRImplementationGuideDefinitionPage(obj).nameElement <> nil then begin
      Edit1.text := (TFHIRImplementationGuideDefinitionPage(obj).nameElement as tfhirURL).value ;
    end else Edit1.text := '';
//    webbrowser1.Enabled:=true;
    Edit5.text := TFHIRImplementationGuideDefinitionPage(obj).title ;
    ComboBox9.ItemIndex := integer(TFHIRImplementationGuideDefinitionPage(obj).generation);
    if autoPreview.IsChecked then Button6click(self);

  end;

  TabControl2.tabindex := selIndex;
  for i := 0 to TabControl2.TabCount - 1 do
    TabControl2.Tabs[i].Visible := (i=selIndex);
  TabControl2.ActiveTab:=TabControl2.Tabs[selindex];


//  tabcontrol2.EndUpdate;
tabcontrol2.Repaint;
end;


procedure TImplementationGuideEditorFrame.tvStructureChange(Sender: TObject);
var
  obj: tFHIRObject;
begin
  if tvStructure.Selected <> nil then begin
    obj := TFHIRObject(TVStructure.Selected.TagObject);
    showTab(obj);
  end;
end;




procedure TImplementationGuideEditorFrame.DeleteItemClick(Sender: TObject);
var
  prt: TTreeViewItem;
  obj: tFHIRObject;

begin
  ResourceIsDirty := true;
  prt := tvStructure.Selected.ParentItem;
  obj := TFHIRObject(TVStructure.Selected.TagObject);

    TFHIRImplementationGuide(tvStructure.Selected.ParentItem.tagObject).dropEmpty;

  ReloadTreeview(prt);

end;





procedure TImplementationGuideEditorFrame.MenuItem5Click(Sender: TObject);
var
  filestr, tempstr: string;

begin

tempStr:=binFolder+'\IGPub';
runandwait(tempstr, '_genonce', '');

filestr:=binfolder+'\igPub\output\index.html';
runandwait(tempstr, filestr, '');
filestr:= stringreplace(filestr, '\', '/', [rfReplaceAll, rfIgnoreCase]);
  runandwait(igrootfolder, 'open', filestr);


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
  reloadtreeview(tvstructure.ItemByGlobalIndex(0));
  showTab(resource);

end;


function TImplementationGuideEditorFrame.SaveAs: integer;
begin
  result := -1;
  if SaveDialog1.Execute then
  begin
    filename := SaveDialog1.filename;
    igrootfolder:=extractfiledir(filename);
    resourceToFile(resource, filename, ffXml, OutputStylePretty);
    showTab(resource);
    result := 0;
  end;

end;


procedure TImplementationGuideEditorFrame.UpdateImplementationGuideClick(Sender: TObject);
var
  obj: TFHIRImplementationGuide;

begin
  obj := TFHIRImplementationGuide(TTreeViewItem(tvStructure.Selected).TagObject);
  if obj = nil then
    exit;
  obj.name := edtESName.text;
  (obj).url := edtESURL.text;
  (obj).status := TfHIRPublicationStatusEnum(ComboBox2.ItemIndex);
  obj.experimental := CheckBox1.IsChecked;
  obj.id := edtESid.text;
  obj.date := TDateTimeEx.makeLocal(DateEdit1.DateTime);
  obj.version := Edit3.text;
  obj.packageId := Edit8.text;
  obj.publisher := Edit7.text;
  obj.description := Memo3.text;
  tvStructure.Selected.Text:=obj.name;

end;

//6.2
procedure TImplementationGuideEditorFrame.UpdatePackageClick(Sender: TObject);
var obj:TFHIRImplementationGuideDefinitionPackage;
begin
  obj := TFHIRImplementationGuideDefinitionPackage(TVStructure.Selected.TagObject);
  if obj = nil then
      exit;
      obj.name:=edit12.Text;
      obj.description:=memo2.Text;
      tvstructure.Selected.Text:=obj.name;
      ReloadTreeview(tvStructure.Selected);
      tvStructure.Selected.Text:=obj.name;

end;

procedure TImplementationGuideEditorFrame.UpdatePageClick(Sender: TObject);
var
  obj: TFHIRImplementationGuideDefinitionPage;
begin
  obj := TFHIRImplementationGuideDefinitionPage(TTreeViewItem(tvStructure.Selected).TagObject);
  if obj = nil then
      exit;
  if (obj).nameElement = nil then (obj).nameElement:=tfhirURL.Create;
  tfhirURL((obj).nameElement).value := Edit1.text;
  obj.generation := TFHIRGuidePageGenerationEnum(ComboBox9.ItemIndex);
  obj.title := Edit5.text;
  tvStructure.Selected.Text:=obj.title;

end;

procedure TImplementationGuideEditorFrame.UpdateResourceClick(Sender: TObject);
var
  obj: TFHIRImplementationGuideDefinitionResource;
begin
  obj := TFHIRImplementationGuideDefinitionResource(TTreeViewItem(tvStructure.Selected).TagObject);
  if obj = nil then
      exit;
    obj.reference.reference:=Edit6.Text;
    obj.name:=edit9.Text;
    obj.description:=memo4.Text;
    if ComboBox1.ItemIndex <> -1 then
      obj.package:= ComboBox1.Items[ComboBox1.ItemIndex];
    tvStructure.Selected.Text:=obj.name;


      end;



procedure TImplementationGuideEditorFrame.ConcatenateFiles(const InFileNames: array of string; const OutFileName: string);
var
  i: Integer;
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

  idx := tfhirImplementationGuideDefinition(TVStructure.Selected.ParentItem.TagObject).resourceList.IndexOf(tfhirImplementationGuideDefinitionResource(TVStructure.Selected.TagObject));

  if idx < tfhirImplementationGuideDefinition(TVStructure.Selected.ParentItem.TagObject).resourceList.Count - 1 then begin
  tfhirImplementationGuideDefinition(TVStructure.Selected.ParentItem.TagObject).resourceList.Exchange(idx, idx + 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index + 1;
  ResourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);

end;

procedure TImplementationGuideEditorFrame.resourceUpClick(Sender: TObject);
var
  idx: integer;
begin
  idx := tfhirImplementationGuideDefinition(TVStructure.Selected.ParentItem.TagObject).resourceList.IndexOf(tfhirImplementationGuideDefinitionResource(TVStructure.Selected.TagObject));
  if idx > 0 then begin
  tfhirImplementationGuideDefinition(TVStructure.Selected.ParentItem.TagObject).resourceList.Exchange(idx, idx - 1);
  tvStructure.Selected.Index := tvStructure.Selected.Index - 1;
  ResourceIsDirty := true;
  end;
  ReloadTreeview(tvStructure.Selected);


end;

procedure TImplementationGuideEditorFrame.runAndWait(Path, command, parameters: String);
var
  folderstr, filestr: string;
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
  sCmd,ExecuteFile, ParamString, StartInString: string;

begin
  folderstr := getCurrentDir;

    begin
      FillChar(SEInfo, SizeOf(SEInfo), 0);
      SEInfo.cbSize := SizeOf(TShellExecuteInfo);
      with SEInfo do
      begin
        fMask := SEE_MASK_NOCLOSEPROCESS;
//        Wnd := FmxHandleToHWND(ImplementationGuideEditor.Handle);
        lpFile := PChar(command);
        lpDirectory := PChar(Path);
        lpParameters := PChar(Parameters);
        nShow := SW_SHOWNORMAL;
      end;
      if ShellExecuteEx(@SEInfo) then
      begin
        repeat
          Application.ProcessMessages;
          GetExitCodeProcess(SEInfo.hProcess, ExitCode);
        until (ExitCode <> STILL_ACTIVE) or Application.Terminated;
      end;
    end;

end;


end.
