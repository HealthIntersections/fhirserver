unit frm_view_manager;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, Menus, IniFiles, fsl_base, fsl_utilities, fui_lcl_utilities, Types;

type
  TViewManagerLocation = (tvlLeft, tvlRight, tvlBottom, tvlHidden);

  TViewManagerPanelId = (tviProjects, tviServers, tviInspector, tviVariables, tviStack, tviMessages, tviLog, tviSearch, tviBreakpoints, tviTasks, tviFHIRPath, tviPackages, tviTextTools);
  TViewManagerPanelIdSet = set of TViewManagerPanelId;

const
  CODES_TViewManagerLocation : Array [TViewManagerLocation] of String = ('Left', 'Right', 'Bottom', 'Hidden');
  CODES_TViewManagerPanelId : Array [TViewManagerPanelId] of String = ('Projects', 'Servers', 'Inspector', 'Variables', 'Stack', 'Messages', 'Log', 'Search', 'Breakpoints', 'Tasks', 'FHIRPath', 'Packages', 'TextTools');
  TITLES_TViewManagerPanelId : Array [TViewManagerPanelId] of String = ('Projects', 'Servers', 'Inspector', 'Variables', 'Stack', 'Messages', 'Log', 'Search', 'Break Points', 'Tasks', 'FHIR Path', 'Packages', 'Text Tools');
  ICONS_TViewManagerPanelId : Array [TViewManagerPanelId] of Integer = (38, 42, 49, 33, 32, 44, 45, 43, 83, 46, 47, 37, 140);

type
  { TViewManager }

  TViewManager = class (TFslObject)
  private
    FIniFile: TIniFile;
    FLeft : TStringList;
    FRight : TStringList;
    FBottom : TStringList;
    FHidden : TStringList;
    FScale: Integer;
    FSrcIsMaximised: boolean;
    FBigToolbar : boolean;
    function GetActive(location : TViewManagerLocation): integer;
    function GetTabbed(location : TViewManagerLocation): boolean;
    procedure SetActive(location : TViewManagerLocation; AValue: integer);
    procedure SetIniFile(AValue: TIniFile);

    function readPanelId(code : String) : TViewManagerPanelId;
    procedure SetScale(AValue: Integer);
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
    property bigToolbar : boolean read FBigToolbar write FBigToolbar;
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
    btnCancel: TButton;
    btnLeftUp: TButton;
    btnOk: TButton;
    btnRightDown: TButton;
    btn100: TButton;
    btnLeftDown: TButton;
    btnBottomUp: TButton;
    btnBottomDown: TButton;
    btnRightUp: TButton;
    btnLeftMove: TButton;
    btnRestore: TButton;
    btnRightMove: TButton;
    btnBottomMove: TButton;
    btnHiddenMove: TButton;
    chkSrcIsMaximised: TCheckBox;
    cbxToolbar: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbleft: TListBox;
    lbBottom: TListBox;
    lbRight: TListBox;
    lbHidden: TListBox;
    mnuMoveLeft: TMenuItem;
    mnuListUp: TMenuItem;
    mnuMoveRight: TMenuItem;
    mnuMoveBottom: TMenuItem;
    mnuMoveHidden: TMenuItem;
    mnuListMoveLeft: TMenuItem;
    mnuListMoveRIght: TMenuItem;
    mnuListMoveBottom: TMenuItem;
    mnuListMoveHidden: TMenuItem;
    mnuListDown: TMenuItem;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel2: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel23: TPanel;
    Panel24: TPanel;
    Panel25: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    pmMove: TPopupMenu;
    pmList: TPopupMenu;
    rbLeftTabbed: TRadioButton;
    rbLeftStacked: TRadioButton;
    rbRightTabbed: TRadioButton;
    rbRightStacked: TRadioButton;
    edtLeftWidth: TSpinEdit;
    edtBottomWidth: TSpinEdit;
    edtRightWidth: TSpinEdit;
    edtZoom: TSpinEdit;
    procedure btn100Click(Sender: TObject);
    procedure btnBottomDownClick(Sender: TObject);
    procedure btnBottomUpClick(Sender: TObject);
    procedure btnLeftDownClick(Sender: TObject);
    procedure btnLeftUpClick(Sender: TObject);
    procedure btnMoveClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnRightDownClick(Sender: TObject);
    procedure btnRightUpClick(Sender: TObject);
    procedure btnRestoreClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbleftMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbRightContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure mnuListDownClick(Sender: TObject);
    procedure mnuListMoveBottomClick(Sender: TObject);
    procedure mnuListMoveHiddenClick(Sender: TObject);
    procedure mnuListMoveLeftClick(Sender: TObject);
    procedure mnuListMoveRIghtClick(Sender: TObject);
    procedure mnuListUpClick(Sender: TObject);
    procedure mnuMoveBottomClick(Sender: TObject);
    procedure mnuMoveHiddenClick(Sender: TObject);
    procedure mnuMoveLeftClick(Sender: TObject);
    procedure mnuMoveRightClick(Sender: TObject);
    procedure pmListPopup(Sender: TObject);
    procedure updateStatus(Sender: TObject);
  private
    FManager: TViewManager;
    FPopupList : TListBox;
    procedure SetManager(AValue: TViewManager);
    procedure build;
    procedure commit;
    procedure moveItem(dst : TListBox);

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
  FHidden := TStringList.create;
end;

destructor TViewManager.Destroy;
begin
  FHidden.Free;
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
  FHidden.CommaText := FIniFile.readString('layout', 'hidden-views', '');
  FScale := FIniFile.ReadInteger('main-form', 'scale', 100);
  FSrcIsMaximised := FIniFile.readBool('main-form', 'source.maximised', false);
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
  FIniFile.writeString('layout', 'hidden-views', FHidden.CommaText);

  FIniFile.WriteInteger('main-form', 'scale', FScale);
  FIniFile.WriteBool('main-form', 'source.maximised', FSrcIsMaximised);
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
  else if FHidden.IndexOf(CODES_TViewManagerPanelId[package]) > -1 then
    result := tvlHidden
  else case package of
    tviProjects,
    tviServers :
      result := tvlLeft;
    tviInspector,
    tviVariables,
    tviStack,
    tviTextTools:
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
  else if FHidden.IndexOf(CODES_TViewManagerPanelId[package]) > -1 then
    result := FHidden.IndexOf(CODES_TViewManagerPanelId[package])
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
    tviTextTools : result := 3;
  end;
end;

function check(count, def : integer) : Integer;
begin
  if count <= 0 then
    result := def
  else
    result := count;
end;
function TViewManager.count(location: TViewManagerLocation): integer;

begin
  case location of
    tvlLeft : result := check(FLeft.count, 2);
    tvlRight : result := check(FRight.count, 3);
    tvlBottom : result := check(FBottom.count, 7);
    tvlHidden : result := check(FHidden.count, 0);
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
    tvlHidden :
      if FHidden.count > 0 then
        result := readPanelId(FHidden[index]);
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

procedure TViewManagerForm.btnMoveClick(Sender: TObject);
var
  btn : TButton;
begin
  btn := sender as TButton;
  mnuMoveLeft.Visible := true;
  mnuMoveLeft.Enabled := true;
  mnuMoveRight.Visible := true;
  mnuMoveRight.Enabled := true;
  mnuMoveBottom.Visible := true;
  mnuMoveBottom.Enabled := true;
  mnuMoveHidden.Visible := true;
  mnuMoveHidden.Enabled := true;
  case btn.tag of
    0:begin
      mnuMoveLeft.Visible := false;
      mnuMoveLeft.Enabled := false;
      end;
    1:begin
      mnuMoveRight.Visible := false;
      mnuMoveRight.Enabled := false;
      end;
    2:begin
      mnuMoveBottom.Visible := false;
      mnuMoveBottom.Enabled := false;
      end;
    3:begin
      mnuMoveHidden.Visible := false;
      mnuMoveHidden.Enabled := false;
      end;
  end;
  pmMove.Tag := btn.Tag;
  with btn.ClientToScreen(point(0, btn.Height)) do
     pmMove.Popup(X, Y);
end;

procedure TViewManagerForm.btnOkClick(Sender: TObject);
begin
  commit;
  ModalResult := mrOK;
end;

procedure TViewManagerForm.btnRightDownClick(Sender: TObject);
begin
  lbRight.Items.Exchange(lbRight.ItemIndex, lbRight.ItemIndex + 1);
  lbRight.ItemIndex := lbRight.ItemIndex + 1;
  updateStatus(nil);
end;

procedure TViewManagerForm.btnRightUpClick(Sender: TObject);
begin
  lbRight.Items.Exchange(lbRight.ItemIndex, lbRight.ItemIndex - 1);
  lbRight.ItemIndex := lbRight.ItemIndex - 1;
  updateStatus(nil);
end;

procedure TViewManagerForm.btnRestoreClick(Sender: TObject);
begin
  edtZoom.value := 100;
  edtLeftWidth.value := 170;
  edtRightWidth.value := 230;
  edtBottomWidth.value := 130;
  chkSrcIsMaximised.checked := false;
  cbxToolbar.ItemIndex := 0;

  rbLeftTabbed.Checked := true;
  rbRightTabbed.Checked := true;

  FManager.FLeft.Clear;
  FManager.FRight.Clear;
  FManager.FBottom.Clear;
  FManager.FHidden.Clear;
  build;
end;

procedure TViewManagerForm.FormCreate(Sender: TObject);
begin
  setForOs(btnOk, btnCancel);
end;

procedure TViewManagerForm.btnBottomUpClick(Sender: TObject);
begin
  lbBottom.Items.Exchange(lbBottom.ItemIndex, lbBottom.ItemIndex - 1);
  lbBottom.ItemIndex := lbBottom.ItemIndex - 1;
  updateStatus(nil);
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

procedure TViewManagerForm.btnLeftDownClick(Sender: TObject);
begin
  lbLeft.Items.Exchange(lbLeft.ItemIndex, lbLeft.ItemIndex + 1);
  lbLeft.ItemIndex := lbLeft.ItemIndex + 1;
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
  if FManager.bigToolbar then
    cbxToolbar.ItemIndex := 1
  else
    cbxToolbar.ItemIndex := 0;
  if FManager.tabbed[tvlLeft] then
    rbLeftTabbed.Checked := true
  else
    rbLeftStacked.Checked := true;
  if FManager.tabbed[tvlRight] then
    rbRightTabbed.Checked := true
  else
    rbRightStacked.Checked := true;
end;

procedure TViewManagerForm.lbleftMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MousePos      : TPoint;
  OverItemIndex : integer;
  list : TListBox;
begin
  if Button = mbRight then
  begin
    list := sender as TListBox;
    MousePos.x := X;
    MousePos.y := Y;
    OverItemIndex := list.ItemAtPos(MousePos, False);
    list.ItemIndex := OverItemIndex;
  end;
end;

procedure TViewManagerForm.lbRightContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  if sender is TListBox then
  begin
    FPopupList := sender as TListBox;
    pmMove.tag := FPopupList.Tag;
  end
  else
    FPopupList := nil;
  Handled := false;
end;

procedure TViewManagerForm.mnuListDownClick(Sender: TObject);
begin
  FPopupList.Items.Exchange(FPopupList.ItemIndex, FPopupList.ItemIndex + 1);
  FPopupList.ItemIndex := FPopupList.ItemIndex + 1;
  updateStatus(nil);
end;

procedure TViewManagerForm.mnuListMoveBottomClick(Sender: TObject);
begin
  moveItem(lbBottom);
end;

procedure TViewManagerForm.mnuListMoveHiddenClick(Sender: TObject);
begin
  moveItem(lbHidden);
end;

procedure TViewManagerForm.mnuListMoveLeftClick(Sender: TObject);
begin
  moveItem(lbLeft);
end;

procedure TViewManagerForm.mnuListMoveRIghtClick(Sender: TObject);
begin
  moveItem(lbRight);
end;

procedure TViewManagerForm.mnuListUpClick(Sender: TObject);
begin
  FPopupList.Items.Exchange(FPopupList.ItemIndex, FPopupList.ItemIndex - 1);
  FPopupList.ItemIndex := FPopupList.ItemIndex - 1;
  updateStatus(nil);
end;

procedure TViewManagerForm.moveItem(dst : TListBox);
var
  src : TListBox;
  i : integer;
begin
  case pmMove.Tag of
    0: src := lbleft;
    1: src := lbRight;
    2: src := lbBottom;
    3: src := lbHidden;
  end;
  Assert(src <> dst);
  i := src.ItemIndex;
  dst.ItemIndex := dst.items.AddObject(src.items[i], src.items.Objects[i]);
  src.Items.delete(i);
  src.ItemIndex := IntegerMax(i - 1, src.Items.Count - 1);
  updateStatus(nil);
end;

procedure TViewManagerForm.mnuMoveBottomClick(Sender: TObject);
begin
  moveItem(lbBottom);
end;

procedure TViewManagerForm.mnuMoveHiddenClick(Sender: TObject);
begin
  moveItem(lbHidden);
end;

procedure TViewManagerForm.mnuMoveLeftClick(Sender: TObject);
begin
  moveItem(lbLeft);
end;

procedure TViewManagerForm.mnuMoveRightClick(Sender: TObject);
begin
  moveItem(lbRight);
end;

procedure TViewManagerForm.pmListPopup(Sender: TObject);
begin
  mnuListUp.enabled := false;
  mnuListDown.enabled := false;
  mnuListMoveLeft.visible := false;
  mnuListMoveLeft.enabled := false;
  mnuListMoveRight.visible := false;
  mnuListMoveRight.enabled := false;
  mnuListMoveBottom.visible := false;
  mnuListMoveBottom.enabled := false;
  mnuListMoveHidden.visible := false;
  mnuListMoveHidden.enabled := false;

  if FPopupList = nil then
    exit;

  if FPopupList.itemIndex > -1 then
  begin
    mnuListUp.enabled := FPopupList.itemIndex > 0;
    mnuListDown.enabled := FPopupList.itemIndex < FPopupList.items.count - 1;
    mnuListMoveLeft.visible := true;
    mnuListMoveLeft.enabled := FPopupList.count > 1;
    mnuListMoveRight.visible := true;
    mnuListMoveRight.enabled := FPopupList.count > 1;
    mnuListMoveBottom.visible := true;
    mnuListMoveBottom.enabled := FPopupList.count > 1;
    mnuListMoveHidden.visible := true;
    mnuListMoveHidden.enabled := FPopupList.count > 1;

    case FPopupList.tag of
      0:begin
        mnuListMoveLeft.visible := false;
        mnuListMoveLeft.enabled := false;
        end;
      1:begin
        mnuListMoveRight.visible := false;
        mnuListMoveRight.enabled := false;
        end;
      2:begin
        mnuListMoveBottom.visible := false;
        mnuListMoveBottom.enabled := false;
        end;
      3:begin
        mnuListUp.enabled := false;
        mnuListDown.enabled := false;
        mnuListMoveHidden.visible := false;
        mnuListMoveHidden.enabled := false;
        end;
    end;
  end;
end;

procedure TViewManagerForm.updateStatus(Sender: TObject);
begin
  btnLeftUp.Enabled := lbLeft.ItemIndex > 0;
  btnLeftDown.Enabled := lbLeft.ItemIndex < lbleft.Items.count - 1;
  btnLeftMove.Enabled := (lbLeft.ItemIndex >= 0) and (lbleft.Items.count > 0);

  btnBottomUp.Enabled := lbBottom.ItemIndex > 0;
  btnBottomDown.Enabled := lbBottom.ItemIndex < lbBottom.Items.count - 1;
  btnBottomMove.Enabled := (lbBottom.ItemIndex >= 0) and (lbBottom.Items.count > 0);

  btnRightUp.Enabled := lbRight.ItemIndex > 0;
  btnRightDown.Enabled := lbRight.ItemIndex < lbRight.Items.count - 1;
  btnRightMove.Enabled := (lbRight.ItemIndex >= 0) and (lbRight.Items.count > 0);

  btnHiddenMove.Enabled := (lbHidden.ItemIndex >= 0) and (lbHidden.Items.count > 0);
end;

procedure TViewManagerForm.build;
var
  viewSet : TViewManagerPanelIdSet;
  procedure add(loc : TViewManagerLocation; id : TViewManagerPanelId);
  begin
    viewSet := viewSet + [id];
    case loc of
      tvlLeft : lbLeft.items.addObject(CODES_TViewManagerPanelId[id], TObject(id));
      tvlRight : lbRight.items.addObject(CODES_TViewManagerPanelId[id], TObject(id));
      tvlBottom : lbBottom.items.addObject(CODES_TViewManagerPanelId[id], TObject(id));
      tvlhidden : lbHidden.items.addObject(CODES_TViewManagerPanelId[id], TObject(id));
    end;
  end;
var
  i : integer;
  v : TViewManagerPanelId;
begin
  lbLeft.items.clear;
  lbRight.items.clear;
  lbBottom.items.clear;
  lbHidden.items.clear;

  viewSet := [];

  for i := 0 to FManager.count(tvlLeft) - 1 do
    add(tvlLeft, FManager.item(tvlLeft, i));
  for i := 0 to FManager.count(tvlRight) - 1 do
    add(tvlRight, FManager.item(tvlRight, i));
  for i := 0 to FManager.count(tvlBottom) - 1 do
    add(tvlBottom, FManager.item(tvlBottom, i));
  for i := 0 to FManager.count(tvlHidden) - 1 do
    add(tvlHidden, FManager.item(tvlHidden, i));

  for v := Low(TViewManagerPanelId) to High(TViewManagerPanelId) do
      if not (v in viewSet) then
        add(FManager.location(v), v);

  lbLeft.itemIndex := -1;
  lbRight.itemIndex := -1;
  lbBottom.itemIndex := -1;
  lbHidden.itemIndex := -1;
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
  FManager.bigToolbar := cbxToolbar.ItemIndex = 1;

  FManager.tabbed[tvlLeft] := rbLeftTabbed.Checked;
  FManager.tabbed[tvlRight] := rbRightTabbed.Checked;

  FManager.FLeft.assign(lbLeft.items);
  FManager.FRight.assign(lbRight.items);
  FManager.FBottom.assign(lbBottom.items);
  FManager.FHidden.assign(lbHidden.items);
  FManager.save;
end;

procedure TViewManagerForm.SetManager(AValue: TViewManager);
begin
  FManager.free;
  FManager := AValue;
end;



end.

