unit ftk_editor_base;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Math,
  Graphics, Controls, ExtCtrls, ComCtrls, Menus,
  SynEdit, SynEditHighlighter, SynEditTypes,
  fsl_base, fsl_utilities, fsl_stream, fsl_fpc, fsl_logging,
  ftk_context, ftk_store;

type
  TCurrentViewMode = (vmText, vmDesigner);

  TValidationMode = (vmInspect, vmValidate);
  TValidationModes = set of TValidationMode;

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
    FNoContextMenu: boolean;
    FOnShow: TNotifyEvent;
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
    property noContextMenu : boolean read FNoContextMenu write FNoContextMenu;

    property OnPopulate : TNotifyEvent read FOnPopulate write FOnPopulate;
    property OnShow : TNotifyEvent read FOnShow write FOnShow;
  end;

  { TBaseEditor }

  TBaseEditor = class (TToolkitEditor)
  private
    FActions : TFslList<TContentAction>;
    FFocusCount : integer;
    actViewDesigner : TContentAction;
    actViewTextTab : TContentAction;
    actNavigate : TContentAction;
    actEncoding : TContentAction;
    actLineMarkers : TContentAction;
    actBOM : TContentAction;
    FTextPanelBase : TPanel;
    FDesignerPanelBase : TPanel;

    FPageControl : TPageControl;
    FTextTab : TTabSheet;
    FDesignTab : TTabSheet;
    FSplitter : TSplitter;

    function addButtonToToolbar(bar: TToolBar): TToolButton;
    procedure doPopulateActionMenu(act: TContentAction);
    procedure populateDynamicMenus(sender : TObject);
    procedure ShowSubMenu(sender : TObject);
    procedure ShowPopup(action : TContentAction);
    procedure bindToContentMenu(content : TMenuItem);
    procedure MakeNavigationItems(sender : TObject);
    procedure DoMnuEncoding(sender : TObject);
    procedure DoMnuLineMarkers(sender : TObject);
    procedure DoMnuBOM(sender : TObject);
    procedure DoMnuNavigate(sender : TObject);
    procedure DoTextEditorChange(sender : TObject);
    procedure DoTextEditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure DoShowDesigner(sender : TObject);
    procedure DoShowTextTab(sender : TObject);
    procedure DoMnuFormat(sender : TObject);
  protected
    // if hasText
    FContent : TStringList;
    // text & designer may be side by side or a binary choice, but only one can have focus
    FTextPanelWork : TPanel;
    FDesignerPanelWork : TPanel;
    FMode : TCurrentViewMode;
    actFormat : TContentAction;


    TextToolbar : TToolBar;
    DesignerToolbar : TToolbar;
    TextEditor : TSynEdit;
    HighLighter : TSynCustomHighlighter;
    FEditorPopup : TPopupMenu;

    FSelectCursor : integer;

    function GetCanBeSaved: boolean; override;
    procedure GotoLine(line : integer);
    function AddActions(tb : TToolBar) : boolean; virtual;
    function makeHighlighter : TSynCustomHighlighter; virtual;
    procedure getNavigationList(ts : TStringList); virtual;
    function MustBeUnicode : boolean; virtual;

    // the kind of operations required to populate the inspector are generally those already required for validation, so we do both at the same time.
    // some validations are a 3 phase process: syntax parse, minimal check and populate the inspection, and then a slower full check
    procedure validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList); virtual;

    procedure checkForEncoding(s : String; line : integer);

    function makeSubAction(action : TContentAction; caption : String; imageIndex, tag : integer; event : TNotifyEvent) : TContentSubAction;
    function makeAction(bar: TToolBar; caption : String; imageIndex : integer) : TContentAction; overload;
    function makeAction(bar: TToolBar; caption : String; imageIndex, tag : integer; event : TNotifyEvent; dontAddToContextMenu : boolean = false) : TContentAction; overload;
    procedure makeDivider(bar: TToolBar);

    procedure ContentChanged; virtual;
    procedure updateToContent;
    procedure updateFromContent;

    procedure makeTextTab; virtual;
    procedure doResize(sender : TObject); virtual;

    function hasFormatCommands : boolean; virtual;
    procedure updateFormatMenu; virtual;
    procedure makeDesigner; virtual;
    procedure updateDesigner; virtual;
    procedure commitDesigner; virtual;

    procedure SetContentUndoable(src : string);
  public
    constructor Create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService); override;
    destructor Destroy; override;

    function GetBytes: TBytes; override;
    procedure LoadBytes(bytes: TBytes); override;
    procedure bindToTab(tab : TTabSheet); override;
    procedure locate(location : TSourceLocation); override;
    function location : String; override;
    procedure redo; override;
    procedure updateToolbarButtons; virtual;
    procedure getFocus(content : TMenuItem); override;
    procedure loseFocus(); override;
    procedure EditPause; override;
    procedure MovePause; override;
    procedure ChangeSideBySideMode; override;
    function hasTextTab : boolean; override;
    function hasDesigner : boolean; override;
    function IsShowingDesigner : boolean; override;
    procedure showDesigner; override;
    procedure showTextTab; override;
    procedure BeginEndSelect; override;
    procedure updateFont; override;
    function getSource : String; override;
    procedure resizeControls; override;
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
  FButton.Hint := AValue.replace('&', '');
  FButton.ShowHint := true;
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
  FContent := TStringList.create;
  FSelectCursor := -1;
end;

destructor TBaseEditor.Destroy;
begin
  FContent.free;
  FActions.Free;
  inherited Destroy;
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
  if (assigned(action.OnShow)) then
    action.OnShow(self);
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
    if not a.noContextMenu then
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
    result.noContextMenu := false;
    FActions.add(result.link);
  finally
    result.free;
  end;
end;

function TBaseEditor.makeAction(bar: TToolBar; caption: String; imageIndex, tag: integer; event: TNotifyEvent; dontAddToContextMenu : boolean = false): TContentAction;
begin
  result := TContentAction.create;
  try
    result.FButton := addButtonToToolbar(bar);
    result.caption := caption;
    result.imageIndex := imageIndex;
    result.tag := tag;
    result.event := event;
    result.noContextMenu := dontAddToContextMenu;
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

procedure TBaseEditor.ContentChanged;
begin
  // nothing
end;

procedure TBaseEditor.updateToContent;
begin
  if HasTextTab then
    FContent.assign(TextEditor.lines)
  else
    raise Exception.create('Not supported yet');
end;

procedure TBaseEditor.updateFromContent;
begin
  if HasTextTab then
    TextEditor.lines.assign(FContent);
end;

{ TBaseEditor }

function TBaseEditor.GetBytes: TBytes;
var
  b : TStringBuilder;
  s : String;
  first : boolean;
begin
  updateToContent;
  b := TStringBuilder.create;
  try
    first := true;
    for s in FContent do
    begin
      if first then
        first := false
      else case Session.EndOfLines of
          slCR : b.append(#13);
          slLF : b.append(#10);
        else
          b.append(#13#10);
        end;
      b.append(s);
    end;
    s := b.toString;
    case Session.Encoding of
      senASCII : result := TEncoding.ASCII.GetBytes(s);
      senUTF16BE : result := TEncoding.BigEndianUnicode.GetBytes(s);
      senUTF16LE : result := TEncoding.Unicode.GetBytes(s);
    else
      result := TEncoding.UTF8.GetBytes(s);
    end;
  finally
    b.free;
  end;
end;

//
//begin
//  // try to automatically detect the buffer encoding
//
//  // detected encoding points to Default and param Encoding requests some other
//  // type of Encoding; set the Encoding param to UTF8 as it can also read ANSI (Default)
//  if (LEncoding = TEncoding.Default) and (Encoding <> TEncoding.Default) then
//    Encoding := TEncoding.UTF8
//  else
//    Encoding := LEncoding;
//
//  FDetectBOM := False;
//end;
//

function oddBytesZero(bytes : TBytes) : boolean;
var
  i : integer;
begin
  result := true;
  i := 1;
  while (i < length(bytes)) do
  begin
    if bytes[i] <> 0 then
      exit(false);
    inc(i, 2);
  end;
end;

function evenBytesZero(bytes : TBytes) : boolean;
var
  i : integer;
begin
  result := true;
  i := 0;
  while (i < length(bytes)) do
  begin
    if bytes[i] <> 0 then
      exit(false);
    inc(i, 2);
  end;
end;

function allBytesAscii(bytes : TBytes) : boolean;
var
  i : integer;
begin
  result := true;
  i := 0;
  while (i < length(bytes)) do
  begin
    if not (bytes[i] in [9, 10, 13, 32..127]) then
      exit(false);
    inc(i);
  end;
end;

procedure TBaseEditor.LoadBytes(bytes: TBytes);
var
  encoding: TEncoding;
  start, len : integer;
  src : String;
  b : TBytes;
  lf, cr, crlf : boolean;
  cursor, linestart : integer;
  ch : char;
begin
  if (length(bytes) = 0) then
  begin
    Session.HasBOM := false;
    Session.EndOfLines := PLATFORM_DEFAULT_EOLN;
    Session.Encoding := senUTF8;
    TextEditor.Clear;
  end
  else
  begin
    encoding := nil;
    // do we have a BOM?
    start := TEncoding.GetBufferEncoding(bytes, encoding);
    if (start <> 0) then
    begin
      Session.HasBOM := true;
      if encoding = TEncoding.UTF8 then
        Session.Encoding := senUTF8
      else if encoding = TEncoding.BigEndianUnicode then
        Session.Encoding := senUTF16BE
      else if encoding = TEncoding.Unicode then
        Session.Encoding := senUTF16LE
      else
        Session.Encoding := senUTF8; // ...?
    end
    else if (encoding = nil) then
    begin
      Session.HasBOM := false;
      if (length(bytes) > 10) then
      begin
        b := copy(bytes, 1, 10);
        if oddBytesZero(b) and not evenBytesZero(b) then
        begin
          Session.Encoding := senUTF16BE;
          encoding := TEncoding.BigEndianUnicode;
        end
        else if not oddBytesZero(b) and evenBytesZero(b) then
        begin
          Session.Encoding := senUTF16LE;
          encoding := TEncoding.Unicode;
        end
      end;
      if encoding = nil then
      begin
        if allBytesAscii(bytes) and not MustBeUnicode then
        begin
          Session.Encoding := senASCII;
          encoding := TEncoding.ASCII;
        end
        else
        begin
          Session.Encoding := senUTF8;
          encoding := TEncoding.UTF8;
        end;
      end;
    end
    else if encoding = TEncoding.Unicode then
      Session.Encoding := senUTF16LE
    else if encoding = TEncoding.BigEndianUnicode then
      Session.Encoding := senUTF16BE
    else if encoding = TEncoding.ASCII then
      Session.Encoding := senASCII
    else
      Session.Encoding := senUTF8;

    src := encoding.GetString(bytes, start, length(bytes) - start);

    lf := false;
    cr := false;
    crlf := false;
    FContent.clear;
    lineStart := 1;
    Cursor := 1;
    len := length(src);
    while (Cursor <= len) do
    begin
      ch := src[Cursor];
      if (ch in [#13,#10]) then
      begin
        FContent.add(copy(src, lineStart, cursor-linestart));
        if (Cursor < len) and (ch = #13) and (src[Cursor+1] = #10) then
        begin
          crlf := true;
          inc(cursor, 2);
        end
        else
        begin
          lf := lf or (ch = #10);
          cr := cr or (ch = #13);
          inc(cursor);
        end;
        lineStart := cursor;
      end
      else
        inc(cursor);
    end;
    if (LineStart <= len) then
      FContent.add(copy(src, lineStart, cursor-linestart))
    else
      FContent.add('');
    if crlf then
      Session.EndOfLines := slCRLF
    else if cr then
      Session.EndOfLines := slCR
    else
      Session.EndOfLines := slLF
  end;

  updateFromContent;
  updateToolbarButtons;
end;

function TBaseEditor.GetCanBeSaved: boolean;
begin
  result := true;
end;

procedure TBaseEditor.GotoLine(line: integer);
begin
  TextEditor.CaretXY := Point(1,line+1);
end;

function TBaseEditor.AddActions(tb : TToolBar): boolean;
begin
  result := false;
end;

procedure TBaseEditor.updateToolbarButtons;
begin
  case Session.EndOfLines of
    slUnknown : actLineMarkers.ImageIndex := 51;
    slCRLF: actLineMarkers.ImageIndex := 51;
    slLF : actLineMarkers.ImageIndex := 53;
    slCR : actLineMarkers.ImageIndex := 54;
  end;
  case Session.Encoding of
    senUnknown: actEncoding.ImageIndex := 55;
    senBinary: actEncoding.ImageIndex := 56;
    senUTF8: actEncoding.ImageIndex := 58;
    senASCII: actEncoding.ImageIndex := 57;
    senUTF16BE: actEncoding.ImageIndex := 59;
    senUTF16LE: actEncoding.ImageIndex := 60;
  end;
  if Session.Encoding in [senUnknown, senBinary, senASCII] then
  begin
    actBOM.Enabled := false;
    actBOM.ImageIndex := 61;
  end
  else
  begin
    actBOM.Enabled := true;
    if Session.HasBOM then
      actBOM.ImageIndex := 62
    else
      actBOM.ImageIndex := 61;
  end;
end;

procedure TBaseEditor.getFocus(content: TMenuItem);
begin
  inc(FFocusCount);
  if FFocusCount = 1 then
    bindToContentMenu(content);
  EditPause;
  TextEditor.SetFocus;
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
end;

procedure TBaseEditor.EditPause;
var
  ts : TStringList;
  loc : TSourceLocation;
  t : QWord;
begin
  t := GetTickCount64;
  LastChangeChecked := true;
  LastMoveChecked := true;
  updateToContent;
  if Context.SideBySide then
  try
    updateDesigner;
  except
    on E : Exception do
      Logging.Log('Exception updating designer: '+e.message);
  end;

  loc := TSourceLocation.fromPoint(TextEditor.CaretXY);
  ts := TStringList.create;
  try
    validate(true, true, loc, ts);
    if (Context.Focus = self) then
      Context.Inspector.Populate(ts);
  finally
    ts.free;
  end;
  Pause := (GetTickCount64 - t) * 4;
end;

procedure TBaseEditor.MovePause;
var
  ts : TStringList;
  loc : TSourceLocation;
begin
  LastMoveChecked := true;

  if (Context.Focus = self) then
  begin
    loc := TSourceLocation.fromPoint(TextEditor.CaretXY);
    ts := TStringList.create;
    try
      validate(false, true, loc, ts);
        Context.Inspector.Populate(ts);
    finally
      ts.free;
    end;
  end;
end;

procedure TBaseEditor.ChangeSideBySideMode;
begin
  if (Context.SideBySide and hasTextTab and hasDesigner) then
  begin
    FTextPanelBase.parent := Tab;
    FSplitter.parent := Tab;
    FDesignerPanelBase.parent := Tab;
    FTextPanelWork.parent := FTextPanelBase;
    FDesignerPanelWork.parent := FDesignerPanelBase;
    FPageControl.Parent := nil;
    actViewTextTab.Caption := '&Activate';
    actViewTextTab.ImageIndex := 76;
    actViewTextTab.Event := DoShowDesigner;
    actViewDesigner.Caption := '&Activate';
    actViewDesigner.ImageIndex := 77;
    actViewDesigner.Event := DoShowTextTab;
    updateDesigner;
  end
  else
  begin
    FPageControl.Parent := tab;
    FTextPanelBase.parent := nil;
    FSplitter.parent := nil;
    FDesignerPanelBase.parent := nil;

    if hasTextTab then
      FTextPanelWork.parent := FTextTab;
    if hasDesigner then
      FDesignerPanelWork.parent := FDesignTab;
    if hasTextTab and hasDesigner then
    begin
      actViewTextTab.Caption := 'Text E&ditor';
      actViewTextTab.ImageIndex := 77;
      actViewTextTab.Event := DoShowTextTab;
      actViewDesigner.Caption := '&Designer';
      actViewDesigner.ImageIndex := 76;
      actViewDesigner.Event := DoShowDesigner;
    end;
  end;
end;

function TBaseEditor.hasTextTab: boolean;
begin
  Result := true;
end;

function TBaseEditor.hasDesigner: boolean;
begin
  result := false;
end;

function TBaseEditor.makeHighlighter: TSynCustomHighlighter;
begin
  result := nil;
end;

procedure TBaseEditor.getNavigationList(ts: TStringList);
begin
end;

function TBaseEditor.MustBeUnicode: boolean;
begin
  result := false;
end;

procedure TBaseEditor.validate(validate: boolean; inspect: boolean;
  cursor: TSourceLocation; inspection: TStringList);
begin
  // nothing
end;

procedure TBaseEditor.checkForEncoding(s: String; line: integer);
var
  ch : UnicodeChar;
  i : integer;
begin
  i := 1;
  if Session.Encoding = senASCII then
    for ch in unicodeChars(s) do
      if (ord(ch) > 127) then
      begin
        validationError(TSourceLocation.Create(line, i), 'the character '+ch+' is illegal when the source is constrained to ASCII');
        exit;
      end
      else
        inc(i);


end;

procedure TBaseEditor.bindToTab(tab: TTabSheet);
begin
  inherited bindToTab(tab);

  // create the pages
  FPageControl := TPageControl.create(tab);
  FPageControl.ShowTabs := false;
  FPageControl.Align := alClient;
  FTextTab := FPageControl.AddTabSheet;
  FDesignTab := FPageControl.AddTabSheet;

  // create the side by side
  FTextPanelBase := TPanel.create(tab);
  FTextPanelBase.BevelWidth := 1;
  FTextPanelBase.BorderWidth := 2;
  FTextPanelBase.Align := alLeft;
  FTextPanelBase.Width := (tab.width div 2) - 4;
  FTextPanelBase.Color := clLime;
  FTextPanelBase.ShowHint := false;
  FSplitter := TSplitter.create(tab);
  FSplitter.Width := 8;
  FSplitter.Align := alLeft;
  FSplitter.ResizeControl := FTextPanelBase;
  FDesignerPanelBase := TPanel.create(tab);
  FDesignerPanelBase.BevelWidth := 1;
  FDesignerPanelBase.BorderWidth := 2;
  FDesignerPanelBase.Align := alClient;
  FDesignerPanelBase.Color := clGray;
  FDesignerPanelBase.ShowHint := false;
  tab.OnResize := doResize;

  // create the panels
  if hasTextTab then
  begin
    FTextPanelWork := TPanel.create(tab);
    FTextPanelWork.BevelWidth := 1;
    FTextPanelWork.Align := alClient;
    FTextPanelWork.Color := clBtnFace;
  end;
  if hasDesigner then
  begin
    FDesignerPanelWork := TPanel.create(tab);
    FDesignerPanelWork.BevelWidth := 1;
    FDesignerPanelWork.Align := alClient;
    FDesignerPanelWork.Color := clBtnFace;
  end;


  // bind mode
  if (Context.SideBySide and hasTextTab and hasDesigner) then
  begin
    FTextPanelBase.parent := Tab;
    FSplitter.parent := Tab;
    FDesignerPanelBase.parent := Tab;
    FTextPanelWork.parent := FTextPanelBase;
    FDesignerPanelWork.parent := FDesignerPanelBase;
    FPageControl.Parent := nil;
  end
  else
  begin
    FPageControl.Parent := tab;
    FTextPanelBase.parent := nil;
    FSplitter.parent := nil;
    FDesignerPanelBase.parent := nil;

    if hasTextTab then
      FTextPanelWork.parent := FTextTab;
    if hasDesigner then
      FDesignerPanelWork.parent := FDesignTab;

    if hasTextTab then
      FPageControl.ActivePage := FTextTab
    else
      FPageControl.ActivePage := FDesignTab;
  end;

  if hasTextTab then
    makeTextTab;

  if hasDesigner then
    makeDesigner;

  FMode := vmText;
end;

procedure TBaseEditor.makeTextTab;
var
  mnu : TMenuItem;
begin
  FEditorPopup := TPopupMenu.create(tab);
  FEditorPopup.Images := Context.images;

  mnu := TMenuItem.create(tab);
  mnu.Action := Context.actions.ActionByName('actionEditUndo');
  FEditorPopup.Items.Add(mnu);

  mnu := TMenuItem.create(tab);
  mnu.Action := Context.actions.ActionByName('actionEditCut');
  FEditorPopup.Items.Add(mnu);

  mnu := TMenuItem.create(tab);
  mnu.Action := Context.actions.ActionByName('actionEditCopy');
  FEditorPopup.Items.Add(mnu);

  mnu := TMenuItem.create(tab);
  mnu.Action := Context.actions.ActionByName('actionEditPaste');
  FEditorPopup.Items.Add(mnu);

  TextToolbar := TToolBar.create(tab);
  TextToolbar.parent := FTextPanelWork;
  TextToolbar.align := alTop;
  TextToolbar.Images := Context.Images;
  TextToolbar.Height := Context.ToolBarHeight;

  if (hasDesigner) then
    if Context.SideBySide then
      actViewDesigner := makeAction(TextToolbar, '&Activate', 77, 0, DoShowTextTab, true)
    else
      actViewDesigner := makeAction(TextToolbar, '&Designer', 76, 0, DoShowDesigner, true);
  actNavigate := makeAction(TextToolbar, '&Navigate', 63);
  actNavigate.OnPopulate := MakeNavigationItems;
  makeDivider(TextToolbar);
  if AddActions(TextToolbar) then
    makeDivider(TextToolbar);

  if (hasFormatCommands) then
  begin
    actFormat := makeAction(TextToolbar, 'Format', 86);
    actFormat.OnShow := DoMnuFormat;
  end;

  actEncoding := makeAction(TextToolbar, 'Encoding', 0);
  makeSubAction(actEncoding, 'ASCII', 57, 0, DoMnuEncoding);
  makeSubAction(actEncoding, 'UTF8 (Unicode)', 58, 1, DoMnuEncoding);
  makeSubAction(actEncoding, 'UTF16 BE', 59, 2, DoMnuEncoding);
  makeSubAction(actEncoding, 'UTF16 LE', 60, 3, DoMnuEncoding);

  actLineMarkers := makeAction(TextToolbar, 'End of Lines', 4);
  makeSubAction(actLineMarkers, 'Windows (CR/LF)', 51, 0, DoMnuLineMarkers);
  makeSubAction(actLineMarkers, 'Unix (LF)', {$IFDEF OSX}52{$ELSE}53{$ENDIF}, 1, DoMnuLineMarkers);
  makeSubAction(actLineMarkers, 'Macintosh (CR)', 54, 3, DoMnuLineMarkers);

  actBOM := makeAction(TextToolbar, 'Byte Order Mark', 10);
  makeSubAction(actBOM, 'No BOM', 61, 0, DoMnuBOM);
  makeSubAction(actBOM, 'BOM', 62, 1, DoMnuBOM);

  // 2. the Synedit
  Highlighter := makeHighlighter;
  TextEditor := TSynEdit.create(tab);
  TextEditor.parent := FTextPanelWork;
  TextEditor.align := alClient;
  TextEditor.Font.Size := 10;
  TextEditor.Highlighter := HighLighter;
  TextEditor.OnChange := DoTextEditorChange;
  TextEditor.OnStatusChange := DoTextEditorStatusChange;
  TextEditor.PopupMenu := FEditorPopup;
end;

procedure TBaseEditor.makeDesigner;
begin
  DesignerToolbar := TToolBar.create(tab);
  DesignerToolbar.parent := FDesignerPanelWork;
  DesignerToolbar.align := alTop;
  DesignerToolbar.Images := Context.Images;
  DesignerToolbar.Height := Context.ToolBarHeight;

  if (hasTextTab) then
    if Context.SideBySide then
      actViewTextTab := makeAction(DesignerToolbar, '&Activate', 76, 0, DoShowDesigner, true)
    else
      actViewTextTab := makeAction(DesignerToolbar, 'Text E&ditor', 77, 0, DoShowTextTab, true)
end;

procedure TBaseEditor.updateDesigner;
begin
  // nothing here
end;

procedure TBaseEditor.commitDesigner;
begin
  // nothing here
end;

procedure TBaseEditor.SetContentUndoable(src : string);
var
  ps, pf : TPoint;
begin
  FContent.Text := src;
  ps := TPoint.Create(1, 1);
  TextEditor.SelStart := length(TextEditor.Text);
  pf := TextEditor.CaretXY;
  TextEditor.BeginUndoBlock;
  TextEditor.SetTextBetweenPoints(ps, pf, src);
  TextEditor.EndUndoBlock;
  TextEditor.CaretXY := ps;
end;

procedure TBaseEditor.doResize(sender: TObject);
begin
  FTextPanelBase.Width := (tab.width div 2) - 4;
end;

function TBaseEditor.hasFormatCommands: boolean;
begin
  result := false;
end;

procedure TBaseEditor.updateFormatMenu;
begin

end;

procedure TBaseEditor.DoTextEditorChange(sender: TObject);
begin
  canRedo := TextEditor.CanRedo;
  Session.NeedsSaving := true;
  lastChange := GetTickCount64;
  lastChangeChecked := false;
  ContentChanged;
  Context.Inspector.Active := false;
  Context.OnUpdateActions(self);;
end;

procedure TBaseEditor.DoTextEditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if (scCaretX in changes) or (scCaretY in changes) or (scSelection in changes) then
  begin
    lastMove := GetTickCount64;
    lastMoveChecked := false;
    Context.Inspector.Active := false;
  end;
end;

procedure TBaseEditor.DoShowDesigner(sender: TObject);
begin
  showDesigner;
end;

procedure TBaseEditor.DoShowTextTab(sender: TObject);
begin
  showTextTab;
end;

procedure TBaseEditor.DoMnuFormat(sender: TObject);
begin
  updateFormatMenu;
end;

procedure TBaseEditor.MakeNavigationItems(sender: TObject);
var
  ts : TStringList;
  i : integer;
begin
  actNavigate.subItems.clear;
  makeSubAction(actNavigate, 'Start', -1, 0, DoMnuNavigate);
  ts := TStringList.create;
  try
    getNavigationList(ts);
    for i := 0 to ts.count - 1 do
      makeSubAction(actNavigate, ts[i], -1, integer(ts.objects[i]), DoMnuNavigate);
  finally
    ts.free;
  end;
  makeSubAction(actNavigate, 'End', -1, $FFFFFFF, DoMnuNavigate);
end;

procedure TBaseEditor.DoMnuEncoding(sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
    10: Session.Encoding := senUnknown;
    11: Session.Encoding := senBinary;
    0: Session.Encoding := senASCII;
    1: Session.Encoding := senUTF8;
    2: Session.Encoding := senUTF16BE;
    3: Session.Encoding := senUTF16LE;
  end;
  updateToolbarButtons;
end;

procedure TBaseEditor.DoMnuLineMarkers(sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
    0: Session.EndOfLines := slCRLF;
    1: Session.EndOfLines := slLF;
    2: Session.EndOfLines := slLF;
    3: Session.EndOfLines := slCR;
  end;
  updateToolbarButtons;
end;

procedure TBaseEditor.DoMnuBOM(sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
    0: Session.HasBOM := false;
    1: Session.HasBOM := true;
  end;
  updateToolbarButtons;
end;

procedure TBaseEditor.DoMnuNavigate(sender: TObject);
var
  mnu : TMenuItem;
begin
  mnu := sender as TMenuItem;
  if mnu.Tag = $FFFFFFF then
    GotoLine(TextEditor.lines.count - 1)
  else
    GotoLine(mnu.tag);
  TextEditor.SetFocus;
end;

procedure TBaseEditor.locate(location: TSourceLocation);
begin
  TextEditor.CaretXY := location.toPoint;
  TextEditor.SetFocus;
end;

function TBaseEditor.location: String;
begin
  result := 'Ln: '+inttostr(TextEditor.CaretXY.Y)+', Col: '+inttostr(TextEditor.CaretXY.X);
end;

procedure TBaseEditor.redo;
begin
  TextEditor.redo;
end;

function TBaseEditor.IsShowingDesigner: boolean;
begin
  result := (FMode = vmDesigner)
end;

procedure TBaseEditor.showDesigner;
begin
  if Context.SideBySide then
  begin
    updateToContent;
    updateDesigner;
    TextEditor.Readonly := true;
    FTextPanelBase.color := clGray;
    FDesignerPanelBase.color := clLime;
  end
  else
  begin
    FPageControl.ActivePage := FDesignTab;
    updateDesigner;;
  end;
  FMode := vmDesigner;
  updateToolbarButtons;
end;

procedure TBaseEditor.showTextTab;
begin
  if Context.SideBySide then
  begin
    TextEditor.Readonly := false;
    commitDesigner;
    FTextPanelBase.color := clLime;
    FDesignerPanelBase.color := clGray;
  end
  else
    FPageControl.ActivePage := FTextTab;
  FMode := vmText;
  updateToolbarButtons;
end;

procedure TBaseEditor.BeginEndSelect;
var
  iEnd : integer;
begin
  if FSelectCursor = -1 then
    FSelectCursor := TextEditor.SelStart
  else if FSelectCursor <> TextEditor.SelStart then
  begin
    iEnd := TextEditor.SelStart;
    TextEditor.SelStart := Math.Min(FSelectCursor, iEnd);
    TextEditor.SelEnd := Math.Max(FSelectCursor, iEnd);
    FSelectCursor := -1;
  end;
end;

procedure TBaseEditor.updateFont;
begin
  if TextEditor <> nil then
    TextEditor.font.assign(Context.Font);
end;

function TBaseEditor.getSource: String;
begin
  result := TextEditor.Text;
end;

procedure TBaseEditor.resizeControls;
begin
  TextToolbar.Height := Context.ToolBarHeight;
  if DesignerToolbar <> nil then
    DesignerToolbar.Height := Context.ToolBarHeight;
end;


end.


