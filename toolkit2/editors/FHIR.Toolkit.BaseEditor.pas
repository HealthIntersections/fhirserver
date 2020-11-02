unit FHIR.Toolkit.BaseEditor;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  Controls, ComCtrls, Menus, SynEdit, SynEditHighlighter,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream,
  FHIR.Toolkit.Context, FHIR.Toolkit.Store;

type

  { TContentSubAction }

  TContentSubAction = class (TFslObject)
  private
    FMenuItemP: TMenuItem;
    FMenuItem: TMenuItem;
    function GetCaption: String;
    function GetEnabled: boolean;
    function GetEvent: TNotifyEvent;
    function GetImageIndex: integer;
    function GetTag: integer;
    procedure SetCaption(AValue: String);
    procedure SetEnabled(AValue: boolean);
    procedure SetEvent(AValue: TNotifyEvent);
    procedure SetImageIndex(AValue: integer);
    procedure SetTag(AValue: integer);
  public
    function link : TContentSubAction; overload;

    property menuItem : TMenuItem read FMenuItem;
    property menuItemP : TMenuItem read FMenuItemP;

    property caption : String read GetCaption write SetCaption;
    property imageIndex : integer read GetImageIndex write SetImageIndex;
    property tag : integer read GetTag write SetTag;
    property enabled : boolean read GetEnabled write SetEnabled;
    property event : TNotifyEvent read GetEvent write SetEvent;
  end;

  { TContentAction }

  TContentAction = class (TFslObject)
  private
    FButton: TToolButton;
    FMenuItem: TMenuItem;
    FPopup : TPopupMenu;
    FOnPopulate: TNotifyEvent;
    FSubItems: TFslList<TContentSubAction>;
    FMenu : TPopupMenu;
    function GetCaption: String;
    function GetEnabled: boolean;
    function GetEvent: TNotifyEvent;
    function GetImageIndex: integer;
    function GetTag: integer;
    procedure SetCaption(AValue: String);
    procedure SetEnabled(AValue: boolean);
    procedure SetEvent(AValue: TNotifyEvent);
    procedure SetImageIndex(AValue: integer);
    procedure SetTag(AValue: integer);
  public
    constructor Create; override;
    Destructor destroy; override;
    function link : TContentAction; overload;

    property menuItem : TMenuItem read FMenuItem;
    property button : TToolButton read Fbutton;

    property caption : String read GetCaption write SetCaption;
    property imageIndex : integer read GetImageIndex write SetImageIndex;
    property tag : integer read GetTag write SetTag;
    property enabled : boolean read GetEnabled write SetEnabled;
    property event : TNotifyEvent read GetEvent write SetEvent;
    property subItems : TFslList<TContentSubAction> read FSubItems;

    property OnPopulate : TNotifyEvent read FOnPopulate write FOnPopulate;
  end;

  { TBaseEditor }

  TBaseEditor = class (TToolkitEditor)
  private
    FActions : TFslList<TContentAction>;
    FFocusCount : integer;

    function addButtonToToolbar(bar: TToolBar): TToolButton;
    procedure doPopulateActionMenu(act: TContentAction);
    procedure populateDynamicMenus(sender : TObject);
    procedure ShowSubMenu(sender : TObject);
    procedure ShowPopup(action : TContentAction);
    procedure bindToContentMenu(content : TMenuItem);
  protected
    function makeSubAction(action : TContentAction; caption : String; imageIndex, tag : integer; event : TNotifyEvent) : TContentSubAction;
    function makeAction(bar: TToolBar; caption : String; imageIndex : integer) : TContentAction; overload;
    function makeAction(bar: TToolBar; caption : String; imageIndex, tag : integer; event : TNotifyEvent) : TContentAction; overload;
    procedure makeDivider(bar: TToolBar);
  public
    constructor Create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService); override;
    destructor Destroy; override;

    procedure getFocus(content : TMenuItem); override;
    procedure loseFocus(); override;
  end;

implementation

{ TContentAction }

constructor TContentAction.Create;
begin
  inherited Create;
  FSubItems := TFslList<TContentSubAction>.create;
end;

destructor TContentAction.destroy;
begin
  FSubItems.Free;
  inherited Destroy;
end;

function TContentAction.link: TContentAction;
begin
  result := TContentAction(inherited link);
end;

function TContentAction.GetCaption: String;
begin
  result := FButton.Caption;
end;

function TContentAction.GetEnabled: boolean;
begin
  result := FButton.Enabled;
end;

function TContentAction.GetEvent: TNotifyEvent;
begin
  result := FButton.OnClick;
end;

function TContentAction.GetImageIndex: integer;
begin
  result := FButton.ImageIndex;
end;

function TContentAction.GetTag: integer;
begin
  result := FButton.Tag;
end;

procedure TContentAction.SetCaption(AValue: String);
begin
  FButton.Caption := AValue;
  if FMenuItem <> nil then
    FMenuItem.Caption := AValue;
end;

procedure TContentAction.SetEnabled(AValue: boolean);
begin
  FButton.Enabled := AValue;
  if FMenuItem <> nil then
    FMenuItem.Enabled := AValue;
end;

procedure TContentAction.SetEvent(AValue: TNotifyEvent);
begin
  FButton.OnClick := AValue;
  if FMenuItem <> nil then
    FMenuItem.OnClick := AValue;
end;

procedure TContentAction.SetImageIndex(AValue: integer);
begin
  FButton.ImageIndex := AValue;
  if FMenuItem <> nil then
    FMenuItem.ImageIndex := AValue;
end;

procedure TContentAction.SetTag(AValue: integer);
begin
  FButton.Tag := AValue;
  if FMenuItem <> nil then
    FMenuItem.Tag := AValue;
end;


{ TContentSubAction }

function TContentSubAction.GetCaption: String;
begin
  result := FMenuItemP.Caption;
end;

function TContentSubAction.GetEnabled: boolean;
begin
  result := FMenuItemP.Enabled;
end;

function TContentSubAction.GetEvent: TNotifyEvent;
begin
  result := FMenuItemP.OnClick;
end;

function TContentSubAction.GetImageIndex: integer;
begin
  result := FMenuItemP.ImageIndex;
end;

function TContentSubAction.GetTag: integer;
begin
  result := FMenuItemP.Tag;
end;

procedure TContentSubAction.SetCaption(AValue: String);
begin
  FMenuItemP.Caption := AValue;
  if (FMenuItem <> nil) then
    FMenuItem.Caption := AValue;
end;

procedure TContentSubAction.SetEnabled(AValue: boolean);
begin
  FMenuItemP.Enabled := AValue;
  if (FMenuItem <> nil) then
    FMenuItem.Enabled := AValue;
end;

procedure TContentSubAction.SetEvent(AValue: TNotifyEvent);
begin
  FMenuItemP.OnClick := AValue;
  if (FMenuItem <> nil) then
    FMenuItem.OnClick := AValue;
end;

procedure TContentSubAction.SetImageIndex(AValue: integer);
begin
  FMenuItemP.ImageIndex := AValue;
  if (FMenuItem <> nil) then
    FMenuItem.ImageIndex := AValue;
end;

procedure TContentSubAction.SetTag(AValue: integer);
begin
  FMenuItemP.Tag := AValue;
  if (FMenuItem <> nil) then
    FMenuItem.Tag := AValue;
end;

function TContentSubAction.link: TContentSubAction;
begin
  result := TContentSubAction(inherited link);
end;

{ TBaseEditor }

constructor TBaseEditor.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService);
begin
  inherited Create(context, session, store);
  FActions := TFslList<TContentAction>.create;
end;

destructor TBaseEditor.Destroy;
begin
  FActions.Free;
  inherited Destroy;
end;

procedure TBaseEditor.getFocus(content: TMenuItem);
begin
  inherited getFocus(content);
  inc(FFocusCount);
  if FFocusCount = 1 then
    bindToContentMenu(content);
end;

procedure TBaseEditor.loseFocus();
var
  a : TContentAction;
  sa : TContentSubAction;
begin
  dec(FFocusCount);
  if FFocusCount = 0 then
  begin
    for a in FActions do
    begin
      a.FmenuItem := nil;
      for sa in a.FSubItems do
        sa.FMenuItem := nil;
    end;
  end;
  inherited loseFocus();
end;

procedure TBaseEditor.ShowPopup(action: TContentAction);
var
  act: TContentSubAction;
  pt: TPoint;
begin
  if (assigned(action.OnPopulate)) then
  begin
    action.subItems.clear;
    action.FPopup.Items.Clear;
    action.OnPopulate(self);
  end;
  pt := Context.locateOnScreen(action.FButton.Left, action.FButton.top + action.FButton.Height);
  action.FPopup.PopUp(pt.x, pt.y);
end;

procedure TBaseEditor.bindToContentMenu(content: TMenuItem);
var
  a : TContentAction;
  sa : TContentSubAction;
begin
  content.onClick := populateDynamicMenus;
  for a in FActions do
  begin
    a.FmenuItem := TMenuItem.create(tab);
    content.Add(a.FMenuItem);
    a.FmenuItem.Caption := a.Caption;
    a.FmenuItem.Tag := a.Tag;
    a.FmenuItem.ImageIndex := a.ImageIndex;
    a.FmenuItem.Enabled := a.Enabled;
    for sa in a.FSubItems do
    begin
      sa.FMenuItem := TMenuItem.create(tab);
      a.FmenuItem.Add(sa.FMenuItem);
      sa.FmenuItem.Caption := sa.Caption;
      sa.FmenuItem.Tag := sa.Tag;
      sa.FmenuItem.ImageIndex := sa.ImageIndex;
      sa.FmenuItem.Enabled := sa.Enabled;
      sa.FmenuItem.OnClick := sa.Event;
    end;
  end;
end;

function TBaseEditor.addButtonToToolbar(bar: TToolBar): TToolButton;
var
  index : integer;
begin
  result := TToolButton.Create(bar);
  index := bar.ButtonCount - 1;
  if index > -1 then
    result.Left := bar.Buttons[index].Left + bar.Buttons[index].Width
  else
    result.Left := 0;
  result.Parent := bar;
end;

procedure TBaseEditor.ShowSubMenu(sender: TObject);
var
  act : TContentAction;
begin
  for act in FActions do
    if sender = act.button then
      ShowPopup(act);
end;


procedure TBaseEditor.doPopulateActionMenu(act : TContentAction);
var
  sa : TContentSubAction;
begin
  act.FMenuItem.Clear;
  act.subItems.clear;
  act.FPopup.Items.Clear;
  act.OnPopulate(self);
  for sa in act.subItems do
  begin
    sa.FMenuItem := TMenuItem.create(tab);
    act.FmenuItem.Add(sa.FMenuItem);
    sa.FmenuItem.Caption := sa.Caption;
    sa.FmenuItem.Tag := sa.Tag;
    sa.FmenuItem.ImageIndex := sa.ImageIndex;
    sa.FmenuItem.Enabled := sa.Enabled;
    sa.FmenuItem.OnClick := sa.Event;
  end;
end;

procedure TBaseEditor.populateDynamicMenus(sender: TObject);
var
  act : TContentAction;
begin
  for act in FActions do
    if assigned(act.OnPopulate) then
      doPopulateActionMenu(act);
end;

function TBaseEditor.makeAction(bar: TToolBar; caption: String; imageIndex: integer): TContentAction;
begin
  result := TContentAction.create;
  try
    result.FButton := addButtonToToolbar(bar);
    result.FPopup := TPopupMenu.create(tab);
    result.FPopup.Images := Context.images;
    result.caption := caption;
    result.imageIndex := imageIndex;
    result.enabled := true;
    result.Event := ShowSubMenu;
    FActions.add(result.link);
  finally
    result.free;
  end;
end;

function TBaseEditor.makeAction(bar: TToolBar; caption: String; imageIndex, tag: integer; event: TNotifyEvent): TContentAction;
begin
  result := TContentAction.create;
  try
    result.FButton := addButtonToToolbar(bar);
    result.caption := caption;
    result.imageIndex := imageIndex;
    result.tag := tag;
    result.event := event;
    FActions.add(result.link);
  finally
    result.free;
  end;
end;

function TBaseEditor.makeSubAction(action : TContentAction; caption: String; imageIndex, tag : integer; event : TNotifyEvent): TContentSubAction;
begin
  result := TContentSubAction.create;
  try
    result.FMenuItemP := TMenuItem.create(tab);
    result.caption := caption;
    result.imageIndex := imageIndex;
    result.tag := tag;
    result.event := event;
    action.subItems.add(result.link);
    action.FPopup.Items.Add(result.FMenuItemP);
  finally
    result.free;
  end;
end;

procedure TBaseEditor.makeDivider(bar: TToolBar);
var
  act : TContentAction;
begin
  act := TContentAction.create;
  try
    act.FButton := addButtonToToolbar(bar);
    act.FButton.Style := tbsDivider;
    act.caption := '-';
    act.imageIndex := -1;
    act.enabled := false;
    FActions.add(act.link);
  finally
    act.free;
  end;
end;

end.


