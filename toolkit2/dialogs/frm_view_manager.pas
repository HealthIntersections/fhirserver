unit frm_view_manager;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, IniFiles, fsl_base, fsl_utilities;

type
  TViewManagerLocation = (tvlLeft, tvlRight, tvlBottom);

  TViewManagerPanelId = (tviProjects, tviServers, tviInspector, tviVariables, tviStack, tviMessages, tviLog, tviSearch, tviBreakpoints, tviTasks, tviFHIRPath, tviPackages);

const
  CODES_TViewManagerLocation : Array [TViewManagerLocation] of String = ('Left', 'Right', 'Bottom');
  CODES_TViewManagerPanelId : Array [TViewManagerPanelId] of String = ('Projects', 'Servers', 'Inspector', 'Variables', 'Stack', 'Messages', 'Log', 'Search', 'Breakpoints', 'Tasks', 'FHIRPath', 'Packages');
  TITLES_TViewManagerPanelId : Array [TViewManagerPanelId] of String = ('Projects', 'Servers', 'Inspector', 'Variables', 'Stack', 'Messages', 'Log', 'Search', 'Break Points', 'Tasks', 'FHIR Path', 'Packages');
  ICONS_TViewManagerPanelId : Array [TViewManagerPanelId] of Integer = (38, 42, 49, 33, 32, 44, 45, 43, 83, 46, 47, 37);

type
  { TViewManager }

  TViewManager = class (TFslObject)
  private
    FIniFile: TIniFile;
    FLeft : TStringList;
    FRight : TStringList;
    FBottom : TStringList;
    FScale: Integer;
    FShowToolbarButtons: boolean;
    FSrcIsMaximised: boolean;
    function GetActive(location : TViewManagerLocation): integer;
    function GetTabbed(location : TViewManagerLocation): boolean;
    procedure SetActive(location : TViewManagerLocation; AValue: integer);
    procedure SetIniFile(AValue: TIniFile);

    function readPanelId(code : String) : TViewManagerPanelId;
    procedure SetScale(AValue: Integer);
    procedure SetShowToolbarButtons(AValue: boolean);
    procedure SetSrcIsMaximised(AValue: boolean);
    procedure SetTabbed(location : TViewManagerLocation; AValue: boolean);
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TViewManager; overload;

    property IniFile : TIniFile read FIniFile write SetIniFile;

    procedure save;

    function size(location : TViewManagerLocation; def : integer) : integer;
    procedure setSize(location : TViewManagerLocation; width : integer);

    property srcIsMaximised : boolean read FSrcIsMaximised write SetSrcIsMaximised;
    property showToolbarButtons : boolean read FShowToolbarButtons write SetShowToolbarButtons;
    property scale : Integer read FScale write SetScale;
    property active[location : TViewManagerLocation] : integer read GetActive write SetActive;
    property tabbed[location : TViewManagerLocation] : boolean read GetTabbed write SetTabbed;

    function location(package : TViewManagerPanelId) : TViewManagerLocation;
    function index(package : TViewManagerPanelId) : integer;

    function count(location : TViewManagerLocation) : integer;
    function item(location : TViewManagerLocation; index : integer) : TViewManagerPanelId;
  end;

  { TViewManagerForm }
  TViewManagerForm = class(TForm)
    btnCancel1: TButton;
    btnLeftUp: TButton;
    btnOk1: TButton;
    btnRightDown: TButton;
    btnRightLeft: TButton;
    btnRightBottom: TButton;
    btn100: TButton;
    btnLeftDown: TButton;
    btnLeftBottom: TButton;
    btnLeftRight: TButton;
    btnBottomUp: TButton;
    btnBottomDown: TButton;
    btnBottomLeft: TButton;
    btnBottomRight: TButton;
    btnRightUp: TButton;
    Button14: TButton;
    chkToolButtons: TCheckBox;
    chkSrcIsMaximised: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbleft: TListBox;
    lbBottom: TListBox;
    lbRight: TListBox;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    rbLeftTabbed: TRadioButton;
    rbLeftStacked: TRadioButton;
    RadioButton3: TRadioButton;
    rbRightTabbed: TRadioButton;
    rbRightStacked: TRadioButton;
    edtLeftWidth: TSpinEdit;
    edtBottomWidth: TSpinEdit;
    edtRightWidth: TSpinEdit;
    edtZoom: TSpinEdit;
    procedure btn100Click(Sender: TObject);
    procedure btnBottomDownClick(Sender: TObject);
    procedure btnBottomLeftClick(Sender: TObject);
    procedure btnBottomRightClick(Sender: TObject);
    procedure btnBottomUpClick(Sender: TObject);
    procedure btnLeftBottomClick(Sender: TObject);
    procedure btnLeftDownClick(Sender: TObject);
    procedure btnLeftRightClick(Sender: TObject);
    procedure btnLeftUpClick(Sender: TObject);
    procedure btnOk1Click(Sender: TObject);
    procedure btnRightBottomClick(Sender: TObject);
    procedure btnRightDownClick(Sender: TObject);
    procedure btnRightLeftClick(Sender: TObject);
    procedure btnRightUpClick(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure updateStatus(Sender: TObject);
  private
    FManager: TViewManager;
    procedure SetManager(AValue: TViewManager);
    procedure build;
    procedure commit;

  public
    property Manager : TViewManager read FManager write SetManager;
  end;

var
  ViewManagerForm: TViewManagerForm;

implementation

{$R *.lfm}

{ TViewManager }

constructor TViewManager.Create;
begin
  inherited Create;
  FLeft := TStringList.create;
  FRight := TStringList.create;
  FBottom := TStringList.create;
end;

destructor TViewManager.Destroy;
begin
  FBottom.Free;
  FRight.Free;
  FLeft.Free;
  inherited Destroy;
end;

function TViewManager.link: TViewManager;
begin
  result := TViewManager(inherited link);
end;

procedure TViewManager.SetIniFile(AValue: TIniFile);
begin
  FIniFile := AValue;
  FLeft.CommaText := FIniFile.readString('layout', 'left-views', '');
  FRight.CommaText := FIniFile.readString('layout', 'right-views', '');
  FBottom.CommaText := FIniFile.readString('layout', 'bottom-views', '');
  FScale := FIniFile.ReadInteger('main-form', 'scale', 100);
  FSrcIsMaximised := FIniFile.readBool('main-form', 'source.maximised', false);
  FShowToolbarButtons := FIniFile.readBool('main-form', 'toolbar-views', false);
end;

function TViewManager.GetActive(location : TViewManagerLocation): integer;
begin
   result := FIniFile.ReadInteger('layout', CODES_TViewManagerLocation[location]+'-tab', 0);
end;

function TViewManager.GetTabbed(location : TViewManagerLocation): boolean;
begin
  result := FIniFile.ReadBool('layout', CODES_TViewManagerLocation[location]+'-layout', true);
end;

procedure TViewManager.SetActive(location : TViewManagerLocation; AValue: integer);
begin
  FIniFile.WriteInteger('layout', CODES_TViewManagerLocation[location]+'-tab', aValue);
end;

function TViewManager.readPanelId(code: String): TViewManagerPanelId;
var
  i : integer;
begin
  i := StringArrayIndexOf(CODES_TViewManagerPanelId, code);
  if i < 0 then
    raise EFslException.create('Illegal Panel code '+code)
  else
    result := TViewManagerPanelId(i);
end;

procedure TViewManager.SetScale(AValue: Integer);
begin
  if FScale = AValue then Exit;
  FScale := AValue;
  save;
end;

procedure TViewManager.SetShowToolbarButtons(AValue: boolean);
begin
  if FShowToolbarButtons = AValue then Exit;
  FShowToolbarButtons := AValue;
  save;
end;

procedure TViewManager.SetSrcIsMaximised(AValue: boolean);
begin
  if FSrcIsMaximised = AValue then Exit;
  FSrcIsMaximised := AValue;
  save;
end;

procedure TViewManager.SetTabbed(location : TViewManagerLocation; AValue: boolean);
begin
  FIniFile.WriteBool('layout', CODES_TViewManagerLocation[location]+'-layout', aValue);
end;

procedure TViewManager.save;
begin
  FIniFile.writeString('layout', 'left-views', FLeft.CommaText);
  FIniFile.writeString('layout', 'right-views', FRight.CommaText);
  FIniFile.writeString('layout', 'bottom-views', FBottom.CommaText);


  FIniFile.WriteInteger('main-form', 'scale', FScale);
  FIniFile.WriteBool('main-form', 'source.maximised', FSrcIsMaximised);
  FIniFile.WriteBool('main-form', 'toolbar-views', FShowToolbarButtons);
end;

function TViewManager.size(location: TViewManagerLocation; def: integer): integer;
begin
  result := FIniFile.readInteger('main-form','panel.'+CODES_TViewManagerLocation[location]+'.size', 0);
  if (result = 0) then
  begin
    result := def;
    setSize(location, def);
  end;
end;

procedure TViewManager.setSize(location: TViewManagerLocation; width: integer);
begin
  FIniFile.WriteInteger('main-form','panel.'+CODES_TViewManagerLocation[location]+'.size', width);
end;

function TViewManager.location(package: TViewManagerPanelId): TViewManagerLocation;
begin
  if FLeft.IndexOf(CODES_TViewManagerPanelId[package]) > -1 then
    result := tvlLeft
  else if FRight.IndexOf(CODES_TViewManagerPanelId[package]) > -1 then
    result := tvlRight
  else if FBottom.IndexOf(CODES_TViewManagerPanelId[package]) > -1 then
    result := tvlBottom
  else case package of
    tviProjects,
    tviServers :
      result := tvlLeft;
    tviInspector,
    tviVariables,
    tviStack :
      result := tvlRight;
    tviMessages,
    tviLog,
    tviSearch,
    tviBreakpoints,
    tviTasks,
    tviFHIRPath,
    tviPackages:
      result := tvlBottom;
  end;
end;

function TViewManager.index(package: TViewManagerPanelId): integer;
begin
  if FLeft.IndexOf(CODES_TViewManagerPanelId[package]) > -1 then
    result := FLeft.IndexOf(CODES_TViewManagerPanelId[package])
  else if FRight.IndexOf(CODES_TViewManagerPanelId[package]) > -1 then
    result := FRight.IndexOf(CODES_TViewManagerPanelId[package])
  else if FBottom.IndexOf(CODES_TViewManagerPanelId[package]) > -1 then
    result := FBottom.IndexOf(CODES_TViewManagerPanelId[package])
  else case package of
    tviProjects : result := 0;
    tviServers : result := 1;
    tviInspector : result := 0;
    tviVariables : result := 1;
    tviStack : result := 2;
    tviMessages : result := 0;
    tviLog : result := 1;
    tviSearch : result := 2;
    tviBreakpoints : result := 3;
    tviTasks : result := 4;
    tviFHIRPath : result := 5;
    tviPackages : result := 6;
  end;
end;

function TViewManager.count(location: TViewManagerLocation): integer;
  function check(count, def : integer) : Integer;
  begin
    if count = 0 then
      count := def;
  end;
begin
  case location of
    tvlLeft : result := check(FLeft.count, 2);
    tvlRight : result := check(FRight.count, 3);
    tvlBottom : result := check(FBottom.count, 7);
  end;
end;

function TViewManager.item(location: TViewManagerLocation; index: integer): TViewManagerPanelId;
begin
  result := tviProjects;
  case location of
    tvlLeft :
      if FLeft.count > 0 then
        result := readPanelId(FLeft[index])
      else case index of
        0: result := tviProjects;
        1: result := tviServers;
      end;
    tvlRight :
      if FRight.count > 0 then
        result := readPanelId(FRight[index])
      else case index of
        0: result := tviInspector;
        1: result := tviVariables;
        2: result := tviStack;
      end;
    tvlBottom :
      if FBottom.count > 0 then
        result := readPanelId(FBottom[index])
      else case index of
        0: result := tviMessages;
        1: result := tviLog;
        2: result := tviSearch;
        3: result := tviBreakpoints;
        4: result := tviTasks;
        5: result := tviFHIRPath;
        6: result := tviPackages;
      end;
  end;
end;

{ TViewManagerForm }

procedure TViewManagerForm.FormDestroy(Sender: TObject);
begin
  FManager.free;
end;

procedure TViewManagerForm.btnLeftUpClick(Sender: TObject);
begin
  lbLeft.Items.Exchange(lbLeft.ItemIndex, lbLeft.ItemIndex - 1);
  lbLeft.ItemIndex := lbLeft.ItemIndex - 1;
  updateStatus(nil);
end;

procedure TViewManagerForm.btnOk1Click(Sender: TObject);
begin
  commit;
  ModalResult := mrOK;
end;

procedure TViewManagerForm.btnRightBottomClick(Sender: TObject);
var
  i : integer;
begin
  i := lbRight.ItemIndex;
  lbBottom.ItemIndex := lbBottom.items.AddObject(lbRight.items[i], lbRight.items.Objects[i]);
  lbRight.Items.delete(i);
  lbRight.ItemIndex := IntegerMax(i - 1, lbRight.Items.Count - 1);
  updateStatus(nil);
end;

procedure TViewManagerForm.btnRightDownClick(Sender: TObject);
begin
  lbRight.Items.Exchange(lbRight.ItemIndex, lbRight.ItemIndex + 1);
  lbRight.ItemIndex := lbRight.ItemIndex + 1;
  updateStatus(nil);
end;

procedure TViewManagerForm.btnRightLeftClick(Sender: TObject);
var
  i : integer;
begin
  i := lbRight.ItemIndex;
  lbLeft.ItemIndex := lbLeft.items.AddObject(lbRight.items[i], lbRight.items.Objects[i]);
  lbRight.Items.delete(i);
  lbRight.ItemIndex := IntegerMax(i - 1, lbRight.Items.Count - 1);
  updateStatus(nil);
end;

procedure TViewManagerForm.btnRightUpClick(Sender: TObject);
begin
  lbRight.Items.Exchange(lbRight.ItemIndex, lbRight.ItemIndex - 1);
  lbRight.ItemIndex := lbRight.ItemIndex - 1;
  updateStatus(nil);
end;

procedure TViewManagerForm.Button13Click(Sender: TObject);
begin
  edtZoom.value := 100;
  edtLeftWidth.value := 170;
  edtRightWidth.value := 230;
  edtBottomWidth.value := 130;
  chkSrcIsMaximised.checked := false;

  rbLeftTabbed.Checked := true;
  rbRightTabbed.Checked := true;

  FManager.FLeft.Clear;
  FManager.FRight.Clear;
  FManager.FBottom.Clear;
  build;
end;

procedure TViewManagerForm.btnBottomUpClick(Sender: TObject);
begin
  lbBottom.Items.Exchange(lbBottom.ItemIndex, lbBottom.ItemIndex - 1);
  lbBottom.ItemIndex := lbBottom.ItemIndex - 1;
  updateStatus(nil);
end;

procedure TViewManagerForm.btnLeftBottomClick(Sender: TObject);
var
  i : integer;
begin
  i := lbleft.ItemIndex;
  lbBottom.ItemIndex := lbBottom.items.AddObject(lbleft.items[i], lbleft.items.Objects[i]);
  lbleft.Items.delete(i);
  lbleft.ItemIndex := IntegerMax(i - 1, lbleft.Items.Count - 1);
end;

procedure TViewManagerForm.btnBottomDownClick(Sender: TObject);
begin
  lbBottom.Items.Exchange(lbBottom.ItemIndex, lbBottom.ItemIndex  + 1);
  lbBottom.ItemIndex := lbBottom.ItemIndex + 1;
  updateStatus(nil);
end;

procedure TViewManagerForm.btn100Click(Sender: TObject);
begin
  edtZoom.value := 100;
end;

procedure TViewManagerForm.btnBottomLeftClick(Sender: TObject);
var
  i : integer;
begin
  i := lbBottom.ItemIndex;
  lbLeft.ItemIndex := lbLeft.items.AddObject(lbBottom.items[i], lbBottom.items.Objects[i]);
  lbBottom.Items.delete(i);
  lbBottom.ItemIndex := IntegerMax(i - 1, lbBottom.Items.Count - 1);
  updateStatus(nil);
end;

procedure TViewManagerForm.btnBottomRightClick(Sender: TObject);
var
  i : integer;
begin
  i := lbBottom.ItemIndex;
  lbRight.ItemIndex := lbRight.items.AddObject(lbBottom.items[i], lbBottom.items.Objects[i]);
  lbBottom.Items.delete(i);
  lbBottom.ItemIndex := IntegerMax(i - 1, lbBottom.Items.Count - 1);
  updateStatus(nil);
end;

procedure TViewManagerForm.btnLeftDownClick(Sender: TObject);
begin
  lbLeft.Items.Exchange(lbLeft.ItemIndex, lbLeft.ItemIndex + 1);
  lbLeft.ItemIndex := lbLeft.ItemIndex + 1;
  updateStatus(nil);
end;

procedure TViewManagerForm.btnLeftRightClick(Sender: TObject);
var
  i : integer;
begin
  i := lbleft.ItemIndex;
  lbRight.ItemIndex := lbRight.items.AddObject(lbleft.items[i], lbleft.items.Objects[i]);
  lbleft.Items.delete(i);
  lbleft.ItemIndex := IntegerMax(i - 1, lbleft.Items.Count - 1);
  updateStatus(nil);
end;

procedure TViewManagerForm.FormShow(Sender: TObject);
begin
  build;
  edtLeftWidth.Value := FManager.size(tvlLeft, 0);
  edtRightWidth.Value := FManager.size(tvlRight, 0);
  edtBottomWidth.Value := FManager.size(tvlBottom, 0);
  edtZoom.Value := FManager.FScale;
  chkSrcIsMaximised.Checked := FManager.srcIsMaximised;
  chkToolButtons.Checked := FManager.showToolbarButtons;
  rbLeftTabbed.Checked := FManager.tabbed[tvlLeft];
  rbLeftStacked.Checked := not rbLeftTabbed.Checked;
  rbRightTabbed.Checked := FManager.tabbed[tvlRight];
  rbRightStacked.Checked := not rbRightTabbed.Checked;
end;

procedure TViewManagerForm.updateStatus(Sender: TObject);
begin
  btnLeftUp.Enabled := lbLeft.ItemIndex > 0;
  btnLeftDown.Enabled := lbLeft.ItemIndex < lbleft.Items.count - 1;
  btnLeftBottom.Enabled := (lbLeft.ItemIndex >= 0) and (lbleft.Items.count > 0);
  btnLeftRight.Enabled := (lbLeft.ItemIndex >= 0) and (lbleft.Items.count > 0);

  btnBottomUp.Enabled := lbBottom.ItemIndex > 0;
  btnBottomDown.Enabled := lbBottom.ItemIndex < lbBottom.Items.count - 1;
  btnBottomLeft.Enabled := (lbBottom.ItemIndex >= 0) and (lbBottom.Items.count > 0);
  btnBottomRight.Enabled := (lbBottom.ItemIndex >= 0) and (lbBottom.Items.count > 0);

  btnRightUp.Enabled := lbRight.ItemIndex > 0;
  btnRightDown.Enabled := lbRight.ItemIndex < lbRight.Items.count - 1;
  btnRightLeft.Enabled := (lbRight.ItemIndex >= 0) and (lbRight.Items.count > 0);
  btnRightBottom.Enabled := (lbRight.ItemIndex >= 0) and (lbRight.Items.count > 0);
end;

procedure TViewManagerForm.build;
  procedure add(loc : TViewManagerLocation; id : TViewManagerPanelId);
  begin
    case loc of
      tvlLeft : lbLeft.items.addObject(CODES_TViewManagerPanelId[id], TObject(id));
      tvlRight : lbRight.items.addObject(CODES_TViewManagerPanelId[id], TObject(id));
      tvlBottom : lbBottom.items.addObject(CODES_TViewManagerPanelId[id], TObject(id));
    end;
  end;
var
  i : integer;
begin
  lbLeft.items.clear;
  lbRight.items.clear;
  lbBottom.items.clear;

  for i := 0 to FManager.count(tvlLeft) - 1 do
    add(tvlLeft, FManager.item(tvlLeft, i));
  for i := 0 to FManager.count(tvlRight) - 1 do
    add(tvlRight, FManager.item(tvlRight, i));
  for i := 0 to FManager.count(tvlBottom) - 1 do
    add(tvlBottom, FManager.item(tvlBottom, i));

  lbLeft.itemIndex := -1;
  lbRight.itemIndex := -1;
  lbBottom.itemIndex := -1;
  updateStatus(nil);
end;

procedure TViewManagerForm.commit;
begin
  // checks:
  FManager.setSize(tvlLeft, edtLeftWidth.Value);
  FManager.setSize(tvlRight, edtRightWidth.Value);
  FManager.setSize(tvlBottom, edtBottomWidth.Value);
  FManager.Scale := edtZoom.Value;
  FManager.srcIsMaximised := chkSrcIsMaximised.Checked;
  FManager.showToolbarButtons := chkToolButtons.Checked;

  FManager.tabbed[tvlLeft] := rbLeftTabbed.Checked;
  FManager.tabbed[tvlRight] := rbRightTabbed.Checked;

  FManager.FLeft.assign(lbLeft.items);
  FManager.FRight.assign(lbRight.items);
  FManager.FBottom.assign(lbBottom.items);
  FManager.save;
end;

procedure TViewManagerForm.SetManager(AValue: TViewManager);
begin
  FManager.free;
  FManager := AValue;
end;



end.

