unit fui_lcl_managers;

{
Copyright (c) 2020+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{

This unit contains a set of classes that orchestrate the UI interface

- TListManager - binds a list of objects to a set of UI controls, with a TListView as the centerpiece
- TTreeManager - binds a tree of objects to a set of UI controls, with a TTreeView as a centerpiece
  - TVTreeManager - binds a tree of objects to a set of UI controls, with a TVirtualStringTree as a centerpiece
- TPanelStack - binds to a TPanel and manages a set of subpanels that arew laid out vertically (logical alternative to a TPageControl)
- TObjectManager - binds a set of edits that edit the properties of an object
- TFHIRSynEditSynchroniser - keeps a SynEdit source for a resource in sync with a loaded resource

}

Interface

uses
  SysUtils, Classes, Contnrs, Graphics, IniFiles, LCLVersion,
  Controls, StdCtrls, Buttons, ExtCtrls, EditBtn, ComCtrls, Dialogs, Menus, ClipBrd,
  SynEdit, SynEditTypes,
  laz.virtualtrees,
  fsl_base, fsl_stream, fsl_http,
  fhir_objects, fhir_factory, fhir_parser;

type
  TNodeOperation = (opAdd, opDelete, opEdit, opExecute, opOrder, opHeirarchy, opRefresh, opStop, opUpdate, opInfo);
  TNodeOperationSet = set of TNodeOperation;

  { TControlEntry }

  TControlOperation = (copNone, copAdd, copAddSet, copEdit, copDelete, copUp, copDown, copReload, copExecute, copRefresh, copStop, copCopy, copUpdate, copInfo);

  { TControlEntryMenuItem }

  TControlEntryMenuItem = class (TFslObject)
  private
    FMenuItem: TMenuItem;
    FMode: String;
    FOp: TControlOperation;
  public
    function link : TControlEntryMenuItem; overload;

    property menuItem : TMenuItem read FMenuItem write FMenuItem;
    property op : TControlOperation read FOp write FOp;
    property mode : String read FMode write FMode;
  end;

  TControlEntry = class (TFslObject)
  private
    FControl: TControl;
    FMode: String;
    FOp: TControlOperation;
    FMenuItems : TFslList<TControlEntryMenuItem>;
    FMenu : TPopupMenu;
  public
    destructor Destroy; override;

    function link : TControlEntry; overload;


    property control : TControl read FControl write FControl;
    property op : TControlOperation read FOp write FOp;
    property mode : String read FMode write FMode;
  end;

  { TListManager }

  { TListOrTreeManagerBase }

  TListOrTreeManagerBase = class abstract (TFslObject)
  protected
    FImages: TImagelist;
    FOnSetFocus: TNotifyEvent;
    FControls : TFslList<TControlEntry>;
    FEnabled : boolean;
    FCanEdit : boolean;
    FSettings: TIniFile;
    FPopup : TPopupMenu;
    FHasCopy : boolean;

    procedure doControl(sender : TObject); virtual; abstract;
    procedure doMnuClick(Sender: TObject); virtual; abstract;
    procedure updateControls(op : TControlOperation; allowed : boolean);
    procedure updateMenuControls;
    procedure SetImages(AValue: TImagelist); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    property OnSetFocus : TNotifyEvent read FOnSetFocus write FOnSetFocus;
    property Images : TImagelist read FImages write SetImages;

    // to override:
    function doubleClickEdit : boolean; virtual;
    procedure getCopyModes(modes : TStringList); virtual;

    procedure registerControl(c : TControl; op : TControlOperation; mode : String = '');
    function registerControlForMenu(c : TControl; menu : TPopupMenu) : TControlEntry;
    function registerMenuEntry(caption : String; imageIndex : integer; op : TControlOperation; mode : String = '') : TMenuItem; overload;
    function registerMenuEntry(grp : TControlEntry; caption : String; imageIndex : integer; op : TControlOperation; mode : String = '') : TMenuItem; overload;
    function registerSubMenuEntry(parent : TMenuItem; caption : String; imageIndex : integer; op : TControlOperation; mode : String = '') : TMenuItem;
  end;

  TListManager<T : TFslObject> = class abstract (TListOrTreeManagerBase)
  private
    FData : TFslList<T>;
    FFiltered : TFslList<T>;
    FList : TListView;
    FFilter : TEdit;

    procedure doFilter;
    function GetHasFocus: boolean;
    procedure rebuild(focus : T);
    procedure FilterChange(sender : TObject);
    function Filtered : boolean;
    function GetFocus: T;
    procedure SetEnabled(AValue: boolean);
    procedure SetFilter(AValue: TEdit);
    procedure SetFocus(AValue: T);
    procedure SetList(AValue: TListView);
    procedure SetSettings(AValue: TIniFile);
    procedure updateStatus;
    procedure doListCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var result: Integer);
    procedure doListChange(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure doListDoubleClick(Sender: TObject);
    procedure doListDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure doMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure populateEntry(entry : TListItem; item : T);
    procedure SetImages(AValue: TImagelist); override;
    procedure internalAddItem(item : T);
    procedure doPopup(sender : TObject);
  protected
    procedure doControl(sender : TObject); override;
    procedure doMnuClick(Sender: TObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    // configuration
    property Data : TFslList<T> read FData;
    property Focus : T read GetFocus write SetFocus;
    property hasFocus : boolean read GetHasFocus;
    property List : TListView read FList write SetList;
    property Filter : TEdit read FFilter write SetFilter;
    Property Enabled : boolean read FEnabled write SetEnabled;
    property Settings : TIniFile read FSettings write SetSettings;

    // control
    function doLoad : boolean; // call this when something changes the data to load
    procedure saveStatus;

    // control. These are not usually needed from outside
    procedure doAdd(mode : String);
    procedure doAddSet(mode : String);
    procedure doEdit(mode : String);
    procedure doDelete(mode : String);
    procedure doUp;
    procedure doDown;
    procedure doExecute(mode : String);
    procedure doUpdate(mode : String);
    procedure doStop(mode : String);
    procedure doInfo(mode : String);
    procedure doCopy(mode : String);
    procedure refresh(item : T = nil); // leaves the items in place (and doesn't refilter) but updates text displays. it item = nil, updates all displayed items

    // to override:
    function canSort : boolean; virtual;
    function manageColWidths : boolean; virtual;
    function allowedOperations(item : T) : TNodeOperationSet; virtual; abstract; // return what is allowed in principle; no need to be concerned with the selection, except for whether modify/delete is allowed
    function AskOnDelete(item : T) : boolean; virtual;
    function loadList : boolean; virtual; abstract; // return false if not loaded ok

    procedure buildMenu; virtual;
    function getImageIndex(item : T) : integer; virtual;
    function getCellText(item : T; col : integer) : String; virtual; abstract;
    function getCellColors(item : T; col : integer; var fore, back : TColor) : boolean; virtual;
    function getSummaryText(item : T) : String; virtual;
    function compareItem(left, right : T; col : integer) : integer; virtual; // if col is -1, then the comparison is for the object as a whole
    function filterItem(item : T; s : String) : boolean; virtual;
    function getCanCopy(item : T; mode : String) : boolean; virtual;
    function getCopyValue(item : T; mode : String) : String; virtual;

    procedure focusItemChange(item : T); virtual;
    function addItem(mode : String) : T; virtual;
    function addItems(mode : String) : TFslList<T>; virtual;
    function editItem(item : T; mode : String) : boolean; virtual;
    function deleteItem(item : T) : boolean; virtual;
    function executeItem(item : T; mode : String) : boolean; virtual;
    function updateItem(item : T; mode : String) : T; virtual;
    function stopItem(item : T; mode : String) : boolean; virtual;
    function refreshItem(item : T) : boolean; virtual;
    procedure itemInfo(item : T; mode : String); virtual;
  end;

  TFslTreeNode = class abstract (TFslObject)
  private
    FNode : TTreeNode;
    FPnode : PVirtualNode;
  protected
    function getChildCount : integer; virtual; abstract;
    function getChild(index : integer) : TFslTreeNode; virtual; abstract;
  end;

  { TTreeManager }

  TTreeManager<T : TFslTreeNode> = class abstract (TListOrTreeManagerBase)
  private
    FTree : TTreeView;

    procedure setTree(value : TTreeView);
    function getFocus : T;
    function GetHasFocus : boolean;
    procedure SetSettings(AValue: TIniFile);
    procedure SetEnabled(AValue: boolean);

    procedure clearTreeNode(item : T);
    procedure buildTreeNode(parent : TTreenode; item : T);
    procedure populateTreeNode(item : T);
    procedure refreshTreeNode(item : T);
    procedure LoadTreeNodes;
    procedure doTreeChange(Sender: TObject);
    procedure doTreeDoubleClick(Sender: TObject);
    procedure updateStatus;
    procedure DoEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure SetImages(AValue: TImagelist); override;
    procedure doPopup(sender : TObject);
  protected
    FData : TFslList<T>; // hidden root

    procedure doControl(sender : TObject); override;
    procedure doMnuClick(Sender: TObject); override;
    function readOnly : boolean; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Tree : TTreeView read FTree write SetTree;
    property Data : TFslList<T> read FData;
    property Focus : T read GetFocus;
    property hasFocus : boolean read GetHasFocus;
    Property Enabled : boolean read FEnabled write SetEnabled;
    property Settings : TIniFile read FSettings write SetSettings;

    // control
    function doLoad : boolean; // call this when something changes the data to load
    procedure refresh(item : T = nil); // leaves the items in place (and doesn't refilter) but updates text displays. it item = nil, updates all displayed items
    procedure saveStatus;

    // control. These are not usually needed from outside
    procedure doAdd(mode : String);
    procedure doAddSet(mode : String);
    procedure doEdit(mode : String);
    procedure doDelete(mode : String);
    procedure doExecute(mode : String);
    procedure doUpdate(mode : String);
    procedure doStop(mode : String);
    procedure doInfo(mode : String);
    procedure doCopy(mode : String);

    // to override:
    function LoadData : boolean; virtual; abstract;
    function allowedOperations(item : T) : TNodeOperationSet; virtual; abstract; // return what is allowed in principle; no need to be concerned with the selection, except for whether modify/delete is allowed
    function AskOnDelete(item : T) : boolean; virtual;

    procedure buildMenu; virtual;
    function getImageIndex(item : T) : integer; virtual;
    function getCellText(item : T) : String; virtual; abstract;
    function getCellColors(item : T; var fore, back : TColor) : boolean; virtual;
    function getSummaryText(item : T) : String; virtual;

    procedure changed; virtual; // e.g. to save

    procedure focusItemChange(item : T); virtual;
    function addItem(parent : T; mode : String) : T; virtual;
    function editItem(item : T; mode : String) : boolean; virtual;
    function editItemText(parent, item : T; var text : String) : boolean; virtual;
    function deleteItem(parent, item : T) : boolean; virtual; // parent might be nil if we're at the root
    function executeItem(item : T; mode : String) : boolean; virtual;
    function updateItem(item : T; mode : String) : T; virtual;
    function stopItem(item : T; mode : String) : boolean; virtual;
    function refreshItem(item : T) : boolean; virtual;
    function getCopyValue(item : T; mode : String) : String; virtual;
    function getCanCopy(item : T; mode : String) : boolean; virtual;
    procedure itemInfo(item : T; mode : String); virtual;
  end;

  PTreeData = ^TTreeData;
  TTreeData = record
    item : TFslTreeNode;
  end;

  { TVTreeManager }
  {$IF LCL_FullVersion >= 02030000}
  TVirtualStringTree = TLazVirtualStringTree;
  {$ENDIF}

  TVTreeManager<T : TFslTreeNode> = class abstract (TListOrTreeManagerBase)
  private
    FTree : TVirtualStringTree;

    function getT(p : PVirtualNode) : T;
    procedure setT(p : PVirtualNode; item : T);
    procedure setTree(value : TVirtualStringTree);
    function getFocus : T;
    function GetHasFocus : boolean;
    procedure SetSettings(AValue: TIniFile);
    procedure SetEnabled(AValue: boolean);

    procedure clearTreeNode(item : T);
    procedure buildTreeNode(parent : PVirtualNode; item : T);
    procedure populateTreeNode(item : T);
    procedure refreshTreeNode(item : T);
    procedure LoadTreeNodes;
    procedure doTreeChange(Sender: TObject);
    procedure doTreeDoubleClick(Sender: TObject);
    procedure updateStatus;
    procedure DoEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

    procedure doGetTreeText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure doGetTreeImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure doPaintTreeText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure doPaintTreeCell(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure doPopup(sender : TObject);
  protected
    FData : TFslList<T>; // hidden root

    procedure doControl(sender : TObject); override;
    procedure doMnuClick(Sender: TObject); override;
    function readOnly : boolean; virtual;
    procedure SetImages(AValue: TImagelist); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Tree : TVirtualStringTree read FTree write SetTree;
    property Data : TFslList<T> read FData;
    property Focus : T read GetFocus;
    property hasFocus : boolean read GetHasFocus;
    Property Enabled : boolean read FEnabled write SetEnabled;
    property Settings : TIniFile read FSettings write SetSettings;

    // control
    function doLoad : boolean; // call this when something changes the data to load
    procedure refresh(item : T = nil); // leaves the items in place (and doesn't refilter) but updates text displays. it item = nil, updates all displayed items
    procedure saveStatus;

    // control. These are not usually needed from outside
    procedure doAdd(mode : String);
    procedure doAddSet(mode : String);
    procedure doEdit(mode : String);
    procedure doDelete(mode : String);
    procedure doExecute(mode : String);
    procedure doUpdate(mode : String);
    procedure doStop(mode : String);
    procedure doInfo(mode : String);
    procedure doCopy(mode : String);

    // to override:
    function LoadData : boolean; virtual; abstract;
    function allowedOperations(item : T) : TNodeOperationSet; virtual; abstract; // return what is allowed in principle; no need to be concerned with the selection, except for whether modify/delete is allowed
    function AskOnDelete(item : T) : boolean; virtual;

    procedure buildMenu; virtual;
    function getImageIndex(item : T) : integer; virtual;
    function getCellText(item : T) : String; virtual; abstract;
    function getCellColors(item : T; var fore, back : TColor) : boolean; virtual;
    function getSummaryText(item : T) : String; virtual;

    procedure changed; virtual; // e.g. to save

    procedure focusItemChange(item : T); virtual;
    function addItem(parent : T; mode : String) : T; virtual;
    function editItem(item : T; mode : String) : boolean; virtual;
    function editItemText(parent, item : T; var text : String) : boolean; virtual;
    function deleteItem(parent, item : T) : boolean; virtual; // parent might be nil if we're at the root
    function executeItem(item : T; mode : String) : boolean; virtual;
    function updateItem(item : T; mode : String) : T; virtual;
    function stopItem(item : T; mode : String) : boolean; virtual;
    function refreshItem(item : T) : boolean; virtual;
    function getCopyValue(item : T; mode : String) : String; virtual;
    function getCanCopy(item : T; mode : String) : boolean; virtual;
    procedure itemInfo(item : T; mode : String); virtual;
  end;

  TLookupValueEvent = procedure (sender : TObject; propName : String; propValue : TFHIRObject; var index : integer) of object;
  TFillListManagerEvent = procedure (sender : TObject; propName : String; propValues : TFHIRObjectList) of object;

  TObjectManagerControlKind = (ckEdit, ckList, ckDate, ckCombo, ckCheck);

  { TObjectManagerControl }

  TObjectManagerControl = class (TFslObject)
  private
    FButton: TSpeedButton;
    FControl: TControl;
    FKind: TObjectManagerControlKind;
    FList: TListOrTreeManagerBase;
    FName: String;
    FOnFillList: TObjectManagerControl;
    FOnLookup: TLookupValueEvent;
    procedure SetList(AValue: TListOrTreeManagerBase);
  public
    constructor Create(kind : TObjectManagerControlKind); overload;
    destructor Destroy; override;

    property name : String read FName write FName;
    property kind : TObjectManagerControlKind read FKind write FKind;
    property list : TListOrTreeManagerBase read FList write SetList;
    property control : TControl read FControl write FControl;
    property button : TSpeedButton read FButton write FButton;
    property OnLookup : TLookupValueEvent read FOnLookup write FOnLookup;
    property OnFillList : TObjectManagerControl read FOnFillList write FOnFillList;
  end;

  { TObjectManager }

  TObjectManager = class (TFslObject)
  private
    FFocus: TFHIRObject;
    FControls : TFslList<TObjectManagerControl>;
    procedure SetFocus(AValue: TFHIRObject);
    procedure populateControls;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Focus : TFHIRObject read FFocus write SetFocus;

    procedure registerControl(propName : String; control : TEdit); overload;
    procedure registerControl(propName : String; control : TEdit; button : TSpeedButton); overload;
    procedure registerManager(propName : String; manager : TListOrTreeManagerBase; fillManager : TFillListManagerEvent); overload;
    procedure registerControl(propName : String; control : TDateEdit); overload;
    procedure registerControl(propName : String; control : TCombobox; lookupEvent : TLookupValueEvent); overload;
    procedure registerControl(propName : String; control : TCheckBox); overload;
  end;


  {
  This object maintains a SynEdit source code for a resource in sync with an object model
  that represents the source code. The object instantiates the model, and then any changes
  made to the object (by code, usually,
  }

  type
    TFHIRSynEditSynchroniserOpStatus = (opNone, opChange);

    { TFHIRSynEditSynchroniser }

    TFHIRSynEditSynchroniser = class (TFslObject)
    private
      FEdit: TSynEdit;
      FFactory: TFHIRFactory;
      FFormat: TFHIRFormat;
      FResource: TFHIRResourceV;

      FOpInProgress : TFHIRSynEditSynchroniserOpStatus;
      FContainer : TFHIRObject;
      FFocus : TFHIRObject;
      FStart : TSourceLocation;
      FFinish : TSourceLocation;

      procedure SetFactory(AValue: TFHIRFactory);

      procedure loadXml;
      procedure loadJson;

      function writeToSource : String;
      function extractFromLines(lines : TStringList; start, finish : TSourceLocation) : String;
      function measure(source : String) : TSourceRange;

      procedure finishOpChange;
    public
      destructor Destroy; override;

      function link : TFHIRSynEditSynchroniser; overload;

      // set up
      property SynEdit : TSynEdit read FEdit write FEdit;
      property Factory : TFHIRFactory read FFactory write SetFactory;
      property Format : TFHIRFormat read FFormat write FFormat;

      // working status
      property Resource : TFHIRResourceV read FResource;
      procedure load;

      // editing interface
      // editing is a 3 phase process.
      //  1. set up an edit operation
      //  2. make the changes to the object tree
      //  3. call commit
      //
      // Only one operation per commit

      // set the value of the provided property, which already exists. The property is an object, which may be quite extensive.
      // from a user point of view, smaller operations are better than big ones, but they are technically possible.
      // the objects maye be (typically are) primitives. The object may be in a repeating list
      procedure changeProperty(container : TFHIRObject; obj : TFHIRObject);

      // add a property, not to a list
      procedure addProperty(owner : TFHIRObject; name : String);

      // delete property, not from a list
      procedure deleteProperty(owner : TFHIRObject; name : String);

      // add to a list, after = nil to add in start
      procedure addToList(owner : TFHIRObject; name : String; after : TFHIRObject);

      // delete from the list
      procedure deleteFromList(owner : TFHIRObject; name : String; obj : TFHIRObject);

      // move in the list,
      procedure moveInList(owner : TFHIRObject; name : String; obj : TFHIRObject; up : boolean);

      procedure commit;
      procedure abandon;
    end;

  { TPanelStackSubPanel }

  TPanelStackSubPanel = class (TFslObject)
  private
    FContainer: TPanel;
    FFraction: Double;
    FHeading: TPanel;
  public
    constructor create(container, heading : TPanel);

    property container : TPanel read FContainer write FContainer;
    property heading : TPanel read FHeading write FHeading;
    property fraction : Double read FFraction write FFraction;
  end;

  { TPanelStack }

  // this looks like a list of panels, but in fact, it's a nested set, so that splitters work
  TPanelStack = class (TFslObject)
  private
    FSpace : TPanel;
    FCount : integer;
    FPanels : TFslList<TPanelStackSubPanel>;
    FImageList: TImageList;
    FLastSize : integer;
    function GetPanel(index : integer): TPanel;
    procedure AddPanel;
    procedure doResize(sender : TObject);
  public
    constructor Create(space : TPanel);
    destructor Destroy; override;

    property imageList : TImageList read FImageList write FImageList;

    property panel[index : integer] : TPanel read GetPanel;
    procedure reset(count : integer);
    procedure resize;

    procedure setCaption(index : integer; caption : String; imageIndex : integer);
  end;

Implementation

{ TControlEntryMenuItem }

function TControlEntryMenuItem.link: TControlEntryMenuItem;
begin
  result := TControlEntryMenuItem(inherited link);
end;

{ TPanelStackSubPanel }

constructor TPanelStackSubPanel.create(container, heading: TPanel);
begin
  inherited Create;
  FContainer := container;
  FHeading := heading;
end;


{ TObjectManagerControl }

constructor TObjectManagerControl.Create(kind: TObjectManagerControlKind);
begin
  inherited create;
  FKind := kind;
end;

destructor TObjectManagerControl.Destroy;
begin
  FList.free;
  inherited Destroy;
end;

procedure TObjectManagerControl.SetList(AValue: TListOrTreeManagerBase);
begin
  FList.Free;
  FList := AValue;
end;

{ TObjectManager }

procedure TObjectManager.SetFocus(AValue: TFHIRObject);
begin
  if FFocus=AValue then Exit;
  FFocus := AValue;
end;

procedure TObjectManager.populateControls;
var
  p : TFHIRProperty;
  ctrl : TObjectManagerControl;
begin
  for ctrl in FControls do
  begin
    p := FFocus.getPropertyValue(ctrl.name
    );
  end;


end;

constructor TObjectManager.Create;
begin
  inherited Create;
  FControls := TFslList<TObjectManagerControl>.create;
  FFocus := nil;
end;

destructor TObjectManager.Destroy;
begin
  FFocus.Free;
  FControls.Free;
  inherited Destroy;
end;

procedure TObjectManager.registerControl(propName: String; control: TEdit);
var
  ctrl : TObjectManagerControl;
begin
  ctrl := TObjectManagerControl.create(ckEdit);
  FControls.add(ctrl);
  ctrl.control := control;
end;

procedure TObjectManager.registerControl(propName: String; control: TEdit; button: TSpeedButton);
var
  ctrl : TObjectManagerControl;
begin
  ctrl := TObjectManagerControl.create(ckEdit);
  FControls.add(ctrl);
  ctrl.control := control;
  ctrl.button := button;
end;

procedure TObjectManager.registerManager(propName: String; manager: TListOrTreeManagerBase; fillManager: TFillListManagerEvent);
var
  ctrl : TObjectManagerControl;
begin
  ctrl := TObjectManagerControl.create(ckList);
  FControls.add(ctrl);
  ctrl.list := manager;
end;

procedure TObjectManager.registerControl(propName: String; control: TDateEdit);
var
  ctrl : TObjectManagerControl;
begin
  ctrl := TObjectManagerControl.create(ckDate);
  FControls.add(ctrl);
  ctrl.control := control;
end;

procedure TObjectManager.registerControl(propName: String; control: TCombobox; lookupEvent: TLookupValueEvent);
var
  ctrl : TObjectManagerControl;
begin
  ctrl := TObjectManagerControl.create(ckCombo);
  FControls.add(ctrl);
  ctrl.control := control;
  ctrl.OnLookup := lookupEvent;
end;

procedure TObjectManager.registerControl(propName: String; control: TCheckBox);
var
  ctrl : TObjectManagerControl;
begin
  ctrl := TObjectManagerControl.create(ckCheck);
  FControls.add(ctrl);
  ctrl.control := control;
end;

{ TControlEntry }

destructor TControlEntry.Destroy;
begin
  FMenuItems.Free;
  inherited Destroy;
end;

function TControlEntry.link: TControlEntry;
begin
  result := TControlEntry(inherited link);
end;

{ TListOrTreeManagerBase }

constructor TListOrTreeManagerBase.Create;
begin
  inherited Create;
  FPopup := TPopupMenu.create(nil);
  FControls := TFslList<TControlEntry>.create;
end;

destructor TListOrTreeManagerBase.Destroy;
begin
  FPopUp.Free;
  FControls.Free;
  inherited Destroy;
end;

function TListOrTreeManagerBase.doubleClickEdit: boolean;
begin
  result := true;
end;

procedure TListOrTreeManagerBase.getCopyModes(modes: TStringList);
begin
  modes.AddPair('caption', 'Caption');
end;

procedure TListOrTreeManagerBase.registerControl(c : TControl; op : TControlOperation; mode : String = '');
var
  entry : TControlEntry;
begin
  entry := TControlEntry.create;
  try
    entry.control := c;
    entry.op := op;
    entry.mode := mode;
    c.OnClick := doControl;
    c.enabled := false;
    FControls.add(entry.link);
  finally
    entry.free;
  end;
end;

function TListOrTreeManagerBase.registerControlForMenu(c: TControl; menu : TPopupMenu): TControlEntry;
begin
  result := TControlEntry.create;
  try
    result.control := c;
    result.op := copNone;
    result.FMenu := menu;
    c.enabled := false;
    FControls.add(result.link);
  finally
    result.free;
  end;
end;

function TListOrTreeManagerBase.registerMenuEntry(caption: String; imageIndex: integer; op: TControlOperation; mode : String = '') : TMenuItem;
var
  list : TStringList;
  i : integer;
begin
  result  := TMenuItem.create(nil);
  FPopup.Items.add(result);
  result.caption := caption;
  result.imageIndex := imageIndex;
  result.Tag := Integer(op);
  if mode <> '' then
    result.name := 'mnuMode'+mode;
  if op <> copNone then
    result.OnClick := doMnuClick;
  if (op = copCopy) then
  begin
    FHasCopy := true;
    list := TStringList.create;
    try
      getCopyModes(list);
      for i := 0 to list.count - 1 do
        if list.Objects[i] <> nil then
          registerSubMenuEntry(result, list.ValueFromIndex[i], integer(list.Objects[i]), copCopy, list.Names[i])
        else
          registerSubMenuEntry(result, list.ValueFromIndex[i], -1, copCopy, list.Names[i]);
    finally
      list.free;
    end;
  end;
end;

function TListOrTreeManagerBase.registerMenuEntry(grp: TControlEntry; caption: String; imageIndex: integer; op: TControlOperation; mode: String): TMenuItem;
var
  list : TStringList;
  i : integer;
begin
  result  := TMenuItem.create(nil);
  grp.FMenu.Items.add(result);
  result.caption := caption;
  result.imageIndex := imageIndex;
  result.Tag := Integer(op);
  if mode <> '' then
    result.name := 'mnuMode'+mode;
  if op <> copNone then
    result.OnClick := doMnuClick;
end;

function TListOrTreeManagerBase.registerSubMenuEntry(parent: TMenuItem; caption: String; imageIndex: integer; op: TControlOperation; mode: String): TMenuItem;
begin
  result  := TMenuItem.create(nil);
  parent.add(result);
  result.caption := caption;
  result.imageIndex := imageIndex;
  result.Tag := Integer(op);
  if mode <> '' then
    result.name := 'mnuMode'+mode;
  result.OnClick := doMnuClick;
end;

procedure TListOrTreeManagerBase.SetImages(AValue: TImagelist);
begin
  if FImages=AValue then Exit;
  FImages:=AValue;
end;

procedure TListOrTreeManagerBase.updateControls(op: TControlOperation; allowed: boolean);
var
  entry : TControlEntry;
  mnu : TMenuItem;
  i : integer;
begin
  for entry in FControls do
  begin
    if entry.op = op then
      entry.control.enabled := allowed;
    if (entry.FMenu <> nil) then
      for i := 0 to entry.FMenu.Items.Count - 1 do
        if (TControlOperation(entry.FMenu.Items[i].tag) = op) then
          entry.FMenu.Items[i].enabled := allowed;
  end;
  for i := 0 to FPopup.Items.Count - 1 do
    if (TControlOperation(FPopup.Items[i].tag) = op) then
      FPopup.Items[i].enabled := allowed;
end;

procedure TListOrTreeManagerBase.updateMenuControls;
var
  entry : TControlEntry;
  i : integer;
  ok : boolean;
begin
  for entry in FControls do
  begin
    if (entry.FMenu <> nil) then
    begin
      ok := false;
      for i := 0 to entry.FMenu.Items.Count - 1 do
        if entry.FMenu.Items[i].Enabled then
          ok := true;
      entry.control.Enabled := ok;
    end;
  end;
end;

{ TListManager }

constructor TListManager<T>.Create;
begin
  inherited Create;
  FData := TFslList<T>.create;
  FFiltered := TFslList<T>.create;
end;

destructor TListManager<T>.Destroy;
begin
  FFiltered.free;
  FData.free;
  inherited Destroy;
end;

procedure TListManager<T>.doFilter;
var
  f, item : T;
begin
  f := Focus.link;
  try
    FFiltered.Clear;
    if Filtered then
    begin
      for item in FData do
        if filterItem(item, Filter.text) then
          FFiltered.add(item.link);
    end
    else
      FFiltered.addAll(FData);
    rebuild(f);
  finally
    f.free;
  end;
end;

function TListManager<T>.GetHasFocus: boolean;
begin
  result := GetFocus <> nil;
end;

procedure TListManager<T>.doControl(sender : TObject);
var
  entry : TControlEntry;
begin
  for entry in FControls do
    if entry.control = sender then
      case entry.op of
        copAdd : doAdd(entry.mode);
        copAddSet : doAddSet(entry.mode);
        copEdit : doEdit(entry.mode);
        copDelete : doDelete(entry.mode);
        copUp : doUp;
        copDown : doDown;
        copReload : doLoad;
        copRefresh : refresh(focus);
        copExecute : doExecute(entry.mode);
        copUpdate : doUpdate(entry.mode);
        copStop : doStop(entry.mode);
        copCopy : doCopy(entry.mode);
        copInfo : doInfo(entry.mode);
      end;
end;

procedure TListManager<T>.FilterChange(sender: TObject);
begin
  doFilter;
end;

function TListManager<T>.Filtered: boolean;
begin
  result := (Filter <> nil) and (Filter.text <> '');
end;

function TListManager<T>.GetFocus: T;
begin
  if (FList.itemindex = -1) or (FList.ItemIndex >= FFiltered.count) then
    result := nil
  else
    result := FFiltered[FList.itemIndex];
end;

procedure TListManager<T>.SetEnabled(AValue: boolean);
var
  ce : TControlEntry;
begin
  FEnabled := AValue;
  FData.Clear;
  FFiltered.Clear;
  //if (FFilter <> nil) then
  //  FFilter.ReadOnly := not Enabled;
  for ce in FControls do
    ce.control.Enabled := Enabled;
  if enabled then
    doLoad;
end;

procedure TListManager<T>.SetFilter(AValue: TEdit);
begin
  FFilter := AValue;
  FFilter.OnChange := FilterChange;
end;

procedure TListManager<T>.SetFocus(AValue: T);
var
  i : integer;
begin
  if AValue = nil then
    List.itemIndex := -1
  else
    List.ItemIndex := FData.IndexOf(aValue);
end;

procedure TListManager<T>.SetList(AValue: TListView);
var
  i : integer;
begin
  FList := AValue;
  List.AutoWidthLastColumn := ManageColWidths;
  List.RowSelect := true;
  List.OnCompare := doListCompare;
  List.OnSelectItem := doListChange;
  List.OnDblClick := doListDoubleClick;
  List.OnCustomDrawSubItem := doListDrawSubItem;
  List.OnMouseMove := doMouseMove;
  List.smallImages := FImages;
  List.Hint := 'List';
  List.showHint := true;
  FPopup.Images := FImages;
  FPopup.OnPopup := doPopup;
  buildMenu;
  if FPopup.Items.Count > 0 then
    List.PopupMenu := FPopup;
  if FSettings <> nil then
    for i := 0 to List.columns.count - 1 do
      list.columns[i].width := FSettings.ReadInteger(List.Name, 'column'+inttostr(i), list.columns[i].width);
  FList.ReadOnly := true;
  if canSort then
  begin
    List.AutoSort := true;
    List.AutoSortIndicator := true;
    List.SortColumn := 0;
    List.SortDirection := ComCtrls.sdAscending;
    List.SortType := stData;
  end
  else
  begin
    List.AutoSort := false;
    List.AutoSortIndicator := false;
    List.SortType := stNone;
  end;
end;

procedure TListManager<T>.SetSettings(AValue: TIniFile);
var
  i : integer;
begin
  FSettings := AValue;
  if (FSettings <> nil) and (FList <> nil) then
    for i := 0 to List.columns.count - 1 do
      list.columns[i].width := FSettings.ReadInteger(List.Name, 'column'+inttostr(i), list.columns[i].width);
end;

procedure TListManager<T>.updateStatus;
var
  i : integer;
  ops : TNodeOperationSet;
  item : T;
begin
  i := FList.itemIndex;
  if i = -1 then
    item := nil
  else
    item := FFiltered[i];
  ops := allowedOperations(item);

  updateControls(copAdd, opAdd in ops);
  updateControls(copAddSet, opAdd in ops);
  updateControls(copEdit, (opEdit in ops) and (i > -1));
  updateControls(copDelete, (opDelete in ops) and (i > -1));
  updateControls(copUp, (opOrder in ops) and (i > 0) and not Filtered);
  updateControls(copDown, (opOrder in ops) and (i > -1) and (i < FFiltered.count - 1) and not Filtered);
  updateControls(copRefresh, opRefresh in ops);
  updateControls(copInfo, opInfo in ops);
  updateControls(copExecute, opExecute in ops);
  updateControls(copUpdate, opUpdate in ops);
  updateControls(copStop, opStop in ops);
  updateControls(copCopy, FHasCopy);
  updateMenuControls();
  FCanEdit := opEdit in ops;

  focusItemChange(item);
  if assigned(FOnSetFocus) then
    FOnSetFocus(self);
end;

procedure TListManager<T>.doListCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var result: Integer);
var
  left, right : T;
begin
  left := T(item1.data);
  right := T(item2.data);
  result := CompareItem(left, right, List.SortColumn);
  if List.SortDirection = ComCtrls.sdDescending then
    result := - result;
end;

procedure TListManager<T>.doListChange(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  updateStatus;
end;

procedure TListManager<T>.doListDoubleClick(Sender: TObject);
var
  item : T;
  entry : TListItem;
begin
  item := GetFocus;
  if doubleClickEdit then
  begin
    if FCanEdit and hasFocus and EditItem(item, '') then
    begin
      entry := FList.items[FList.itemindex];
      entry.SubItems.Clear;
      populateEntry(entry, item);
    end;
  end
  else if hasFocus then
  begin
    doExecute('');
  end;
end;

procedure TListManager<T>.doListDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  fore, back : TColor;
begin
  fore := sender.Canvas.Font.Color;
  back := sender.Canvas.Brush.Color;
  if getCellColors(T(item.data), subItem, fore, back) then
  begin
    sender.Canvas.Font.Color := fore;
    sender.Canvas.Brush.Color := back;
  end;
end;

procedure TListManager<T>.doMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  li : TListItem;
  i : T;
begin
  li := FList.getItemAt(x, y);
  if (li = nil) then
    List.Hint := '--'
  else
  begin
    i := T(li.data);
    List.Hint := getSummaryText(i);
  end;
end;

procedure TListManager<T>.doMnuClick(Sender: TObject);
var
  mnu : TMenuItem;
  mode : String;
begin
  mnu := (Sender as TMenuItem);
  if mnu.Name.StartsWith('mnuMode') then
    mode := mnu.Name.Substring(7)
  else
    mode := '';

  case TControlOperation(mnu.Tag) of
    copAdd : doAdd(mode);
    copAddSet : doAddSet(mode);
    copEdit : doEdit(mode);
    copDelete : doDelete(mode);
    copUp : doUp;
    copDown : doDown;
    copReload : doLoad;
    copRefresh : refresh(focus);
    copExecute : doExecute(mode);
    copUpdate : doupdate(mode);
    copStop : doStop(mode);
    copCopy : doCopy(mode);
    copInfo : doInfo(mode);
  end;
end;

procedure TListManager<T>.internalAddItem(item : T);
var
  itemT, itemN : T;
  entry : TListItem;
  i : integer;
begin
  // so, does this replace and existing item?
  itemN := nil;
  for itemT in FData do
  begin
    if (compareItem(item, itemT, -1) = 0) then
      itemN := itemT;
  end;

  if itemN = nil then
  begin
    entry := FList.items.add;
    entry.Data := pointer(item);
    populateEntry(entry, item);
    FData.add(item.link);
    FFiltered.add(item.link); // even if it fails filter
    FList.ItemIndex := FList.items.count - 1;
  end
  else
  begin
    i := FData.indexOf(itemN);
    FData[i] := item.link;
    i := FFiltered.indexOf(itemN);
    if i > -1 then
    begin
      entry := FList.items[i];
      FList.ItemIndex := i;
      FFiltered[i] := item.link;
    end
    else
    begin
      FFiltered.add(item.link); // even if it fails filter
      entry := FList.items.add;
      FList.ItemIndex := FList.items.count - 1;
    end;
    entry.Data := pointer(item);
    populateEntry(entry, item);
  end;
end;

procedure TListManager<T>.doPopup(sender: TObject);
  procedure visitItem(item : TMenuItem);
  var
    i : integer;
  begin
    if item.Tag = ord(copCopy) then
      item.Enabled := getCanCopy(focus, item.name);
    for i := 0 to item.Count - 1 do
      visitItem(item.Items[i]);
  end;
begin
  visitItem(FPopup.items);
end;

procedure TListManager<T>.doAdd(mode : String);
var
  item : T;
begin
  item := addItem(mode);
  if (item <> nil) then
  begin
    try
      internalAddItem(item);
      updateStatus;
    finally
      item.Free;
    end;
  end;
end;

procedure TListManager<T>.doAddSet(mode: String);
var
  items : TFslList<T>;
  item : T;
begin
  items := addItems(mode);
  if (items <> nil) then
  begin
    try
      for item in items do
        internalAddItem(item);
      updateStatus;
    finally
      items.Free;
    end;
  end;
end;

procedure TListManager<T>.populateEntry(entry : TListItem; item : T);
var
  c : integer;
begin
  entry.caption := getCellText(item, 0);
  entry.imageIndex := getImageIndex(item);
  entry.subItems.Clear;
  for c := 1 to FList.columnCount - 1 do
    entry.subItems.add(getCellText(item, c));
end;

procedure TListManager<T>.SetImages(AValue: TImagelist);
begin
  inherited SetImages(AValue);
  if FList <> nil then
    FList.SmallImages := AValue;
end;

procedure TListManager<T>.buildMenu;
begin
end;

function TListManager<T>.getImageIndex(item: T): integer;
begin
  result := -1;
end;

procedure TListManager<T>.doEdit(mode : String);
begin
  if focus <> nil then
    editItem(focus, mode);
end;

procedure TListManager<T>.doDelete(mode : String);
var
  f : T;
  s : String;
  i : integer;
begin
  f := focus;
  if AskOnDelete(f) then
  begin
    if (QuestionDlg('Delete', 'Delete '+getSummaryText(f)+'?', mtConfirmation, [mrYes, mrNo], '') = mrYes) then
    begin
      DeleteItem(f);
      i := FList.itemIndex;
      FData.Delete(i);
      FList.Items.Delete(i);
      updateStatus;
    end;
  end
  else
  begin
    if DeleteItem(f) then
    begin
      i := FList.itemIndex;
      FData.Delete(i);
      FList.Items.Delete(i);
      updateStatus;
    end;
  end;
end;

procedure TListManager<T>.doUp;
begin
  raise EFslException.Create('not done yet');
end;

procedure TListManager<T>.doDown;
begin
  raise EFslException.Create('not done yet');
end;

procedure TListManager<T>.doExecute(mode: String);
var
  item : T;
begin
  if HasFocus then
  begin
    item := getFocus;
    if ExecuteItem(item, mode) then
      refresh(item);
  end;
end;

procedure TListManager<T>.doUpdate(mode: String);
var
  item, n : T;
begin
  if HasFocus then
  begin
    item := getFocus;
    n := UpdateItem(item, mode);
    if n <> nil then
      refreshItem(n);
  end;
end;

procedure TListManager<T>.doStop(mode: String);
var
  item : T;
begin
  if HasFocus then
  begin
    item := getFocus;
    if StopItem(item, mode) then
      refresh(item);
  end;
end;

procedure TListManager<T>.doInfo(mode: String);
var
  item : T;
begin
  if HasFocus then
  begin
    item := getFocus;
    itemInfo(item, mode);
  end;
end;

procedure TListManager<T>.doCopy(mode: String);
var
  item : T;
  s : String;
begin
  if HasFocus then
  begin
    item := getFocus;
    s := getCopyValue(item, mode);
    if (s <> '') then
      Clipboard.AsText := s;
  end;
end;

procedure TListManager<T>.rebuild(focus : T);
var
  item : T;
  entry : TListItem;
  i : integer;
begin
  FList.BeginUpdate;
  try
    FList.ItemIndex := -1;
    FList.items.clear;
    i := 0;
    for item in FFiltered do
    begin
      entry := FList.items.add;
      entry.Data := pointer(item);
      populateEntry(entry, item);
      if (focus <> nil) and (compareItem(focus, item, -1) = 0) then
        FList.ItemIndex := i;
      inc(i);
    end;
  finally
    FList.EndUpdate;
  end;
  updateStatus;
end;

function TListManager<T>.doLoad : boolean;
var
  f : T;
begin
  f := Focus.link;
  try
    FData.Clear;
    result := LoadList;
    doFilter;
  finally
    f.free;
  end;
end;

procedure TListManager<T>.saveStatus;
var
  i : integer;
begin
  if FSettings <> nil then
    for i := 0 to List.columns.count - 1 do
      FSettings.WriteInteger(List.Name, 'column'+inttostr(i), list.columns[i].width);
end;

procedure TListManager<T>.refresh(item : T);
var
  ti : T;
  entry : TListItem;
  i, c : integer;
begin
  if (item <> nil) then
  begin
    refreshItem(item);
    updateStatus;
  end;

  FList.BeginUpdate;
  try
    for entry in FList.items do
    begin
      ti := T(entry.Data);
      if (item = nil) or (item = ti) then
        populateEntry(entry, ti);
    end;
  finally
    FList.EndUpdate;
  end;
end;

function TListManager<T>.canSort: boolean;
begin
  result := false;
end;

function TListManager<T>.manageColWidths: boolean;
begin
  result := true;
end;

function TListManager<T>.AskOnDelete(item: T): boolean;
begin
  result := true;
end;

function TListManager<T>.getCellColors(item: T; col: integer; var fore, back: TColor): boolean;
begin
  result := false;
end;

function TListManager<T>.getSummaryText(item: T): String;
begin
  result := getCellText(item, 0);
end;

function TListManager<T>.compareItem(left, right: T; col : integer): integer;
begin
  result := NativeUInt(left) - NativeUInt(right);
end;

function TListManager<T>.filterItem(item: T; s: String): boolean;
begin
  result := true;
end;

function TListManager<T>.getCanCopy(item: T; mode: String): boolean;
begin
  result := item <> nil;
end;

procedure TListManager<T>.focusItemChange(item: T);
begin
  // nothing here
end;

function TListManager<T>.getCopyValue(item: T; mode: String): String;
begin
  if mode = 'caption' then
    result := getCellText(item, 0)
  else
    result := '';
end;

function TListManager<T>.addItem(mode: String): T;
begin
  result := nil;
end;

function TListManager<T>.addItems(mode: String): TFslList<T>;
begin

end;

function TListManager<T>.editItem(item: T; mode: String): boolean;
begin
  result := false;
end;

function TListManager<T>.deleteItem(item: T) : boolean;
begin
  raise EFslException.Create('Delete is not supported here');
end;

function TListManager<T>.executeItem(item: T; mode: String): boolean;
begin
  raise EFslException.Create('Execute is not supported here');
end;

function TListManager<T>.updateItem(item: T; mode: String): T;
begin
  raise EFslException.Create('Update is not supported here');
end;

function TListManager<T>.stopItem(item: T; mode: String): boolean;
begin
  raise EFslException.Create('Stop is not supported here');
end;

function TListManager<T>.refreshItem(item: T) : boolean;
begin
  result := false;
end;

procedure TListManager<T>.itemInfo(item: T; mode: String);
begin
  raise EFslException.Create('ItemInfo is not supported here');
end;


{ TFHIRSynEditSynchroniser }

// we start by parsing the source.
//
// we don't how much of the source is pretty-printed, and even if it was, it might not be the way we do it exactly
// so we don't make any assumptions about that. But we do use out style of pretty printing when generating text
//
// so we parse the source, and have the source locations on all the objects.
// note that for json primitives, we may have 2 locations.
//
// when an operation is initiated, we determine the start and end of the object in question.
// at the end of the operation, we re-render the object in question (actually, the entire resource)
// then we delete the old content, and insert the new content (if any), and then adjust the
// source locations for anything that follows

destructor TFHIRSynEditSynchroniser.Destroy;
begin
  FFocus.Free;  // though we really expect it to be nil
  FFactory.free;
  FResource.Free;
  inherited Destroy;
end;

function TFHIRSynEditSynchroniser.link: TFHIRSynEditSynchroniser;
begin
  result := TFHIRSynEditSynchroniser(inherited link);
end;

procedure TFHIRSynEditSynchroniser.SetFactory(AValue: TFHIRFactory);
begin
  FFactory.free;
  FFactory := AValue;
end;

procedure TFHIRSynEditSynchroniser.loadXml;
var
  p : TFHIRParser;
begin
  FResource.Free;
  FResource := nil;

  // todo: for the synchronizer, we require that we're using default namespaces. It's not clear what's the best way
  // to manage this
  p := FFactory.makeParser(nil, FFormat, THTTPLanguages.Create('en'));
  try
    p.KeepParseLocations := true;
    FResource := p.parseResource(SynEdit.text); // this will use UTF8, so that positions match, since we are UTF-8 internally
  finally
    p.free;
  end;
end;

procedure TFHIRSynEditSynchroniser.loadJson;
var
  p : TFHIRParser;
begin
  FResource.Free;
  FResource := nil;

  p := FFactory.makeParser(nil, FFormat, THTTPLanguages.Create('en'));
  try
    p.KeepParseLocations := true;
    FResource := p.parseResource(SynEdit.text); // this will use UTF8, so that positions match, since we are UTF-8 internally
  finally
    p.free;
  end;
end;

function TFHIRSynEditSynchroniser.writeToSource: String;
var
  ss : TStringStream;
  c : TFHIRComposer;
begin
  ss := TStringStream.create('', TEncoding.UTF8);
  try
    c := FFactory.makeComposer(nil, FFormat, THTTPLanguages.Create('en'), OutputStylePretty);
    try
      c.KeepLocationData := true;
      c.compose(ss, FResource);
    finally
      c.free;
    end;
    result := ss.DataString;
  finally
    ss.free;
  end;
end;

function TFHIRSynEditSynchroniser.extractFromLines(lines: TStringList; start, finish: TSourceLocation): String;
var
  i : integer;
begin
  if start.line > finish.line then
    result := ''
  else if start.line = finish.line then
    result := lines[start.line].subString(start.col, finish.col - start.col)
  else
  begin
    result :=  lines[start.line].subString(start.col);
    for i := start.line +1 to finish.line - 1 do
      result := result + #13#10 + lines[i];
    result := result + #13#10 + lines[finish.line].subString(0, finish.col);
  end;
end;

function TFHIRSynEditSynchroniser.measure(source: String): TSourceRange;
var
  i : integer;
begin
  result := TSourceRange.Create;
  i := 1;
  while i <= source.length do
  begin
    case source[i] of
      #10:
          result.incLines;
      #13:
        begin
          result.incLines;
          if (i < source.Length) and (source[i + 1] = #10) then
            inc(i);
        end;
      else
        result.incCols;
    end;
    inc(i);
  end;
end;

procedure TFHIRSynEditSynchroniser.finishOpChange;
var
  src : String;
  lines : TStringList;
  added, removed : TSourceRange;
  ps, pf : TPoint;
  do1, do2 : boolean;
begin
  do1 := FFocus.LocationData.hasLocation1;
  do2 := FFocus.LocationData.hasLocation2;
  if do1 and do2 then
    raise EFslException.Create('not supported yet');

  lines := TStringList.create;
  try
    // write the parent object to the selected format
    lines.Text := writeToSource; // for more efficiency, try just the immediate parent (not ready yet)

    // figure out the new text to insert
    if do1 then
      src := extractFromLines(lines, FFocus.LocationData.composeStart, FFocus.LocationData.composeFinish).Trim
    else
      src := extractFromLines(lines, FFocus.LocationData.composeStart2, FFocus.LocationData.composeFinish2).Trim;

    if (FFormat = ffJson) and src.EndsWith(',') then
      src := src.Substring(0, src.length-1);

    added := measure(src);
  finally
    lines.Free;
  end;

  // start a transaction
  SynEdit.BeginUndoBlock;

  // replace the existing content
  if (do1) then
  begin
    ps := FFocus.LocationData.parseStart.toPoint;
    pf := FFocus.LocationData.parseFinish.toPoint;
    removed := FFocus.LocationData.parseFinish - FFocus.LocationData.parseStart;
  end
  else
  begin
    ps := FFocus.LocationData.parseStart2.toPoint;
    pf := FFocus.LocationData.parseFinish2.toPoint;
    removed := FFocus.LocationData.parseFinish2 - FFocus.LocationData.parseStart2;
  end;
  SynEdit.SetTextBetweenPoints(ps, pf, src, [setSelect], scamIgnore, smaMoveUp, smNormal);
  SynEdit.EndUndoBlock;

  // update remaining content for new content metrics
  if (do1) then
    FResource.updateLocationData(FFocus.LocationData.parseStart, removed, added, FFocus)
  else
    FResource.updateLocationData(FFocus.LocationData.parseStart2, removed, added, FFocus);
end;

procedure TFHIRSynEditSynchroniser.load;
begin
  case format of
    ffXml : loadXml;
    ffJson : loadJson;
  else
    raise EFslException.Create('This format is not supported');
  end;
end;

procedure TFHIRSynEditSynchroniser.changeProperty(container : TFHIRObject; obj: TFHIRObject);
begin
  FOpInProgress := opChange;
  FStart := obj.LocationData.ParseStart;
  FFinish := obj.LocationData.parseFinish;
  FContainer := container.link;
  FFocus := obj.link;
end;

procedure TFHIRSynEditSynchroniser.addProperty(owner: TFHIRObject; name: String);
begin
  raise EFslException.Create('not done yet');
end;

procedure TFHIRSynEditSynchroniser.deleteProperty(owner: TFHIRObject; name: String);
begin
  raise EFslException.Create('not done yet');
end;

procedure TFHIRSynEditSynchroniser.addToList(owner: TFHIRObject; name: String; after: TFHIRObject);
begin
  raise EFslException.Create('not done yet');
end;

procedure TFHIRSynEditSynchroniser.deleteFromList(owner: TFHIRObject; name: String; obj: TFHIRObject);
begin
  raise EFslException.Create('not done yet');
end;

procedure TFHIRSynEditSynchroniser.moveInList(owner: TFHIRObject; name: String; obj: TFHIRObject; up: boolean);
begin
  raise EFslException.Create('not done yet');
end;

procedure TFHIRSynEditSynchroniser.commit;
begin
  case FOpInProgress of
    opNone : raise EFslException.Create('No operation in process');
    opChange : finishOpChange;
  else
    raise EFslException.Create('not done yet');
  end;
  FOpInProgress := opNone;
  FContainer := nil;
  FFocus := nil;
  FStart := TSourceLocation.CreateNull;
  FFinish := TSourceLocation.CreateNull;
end;

procedure TFHIRSynEditSynchroniser.abandon;
begin
  FOpInProgress := opNone;
  FFocus.Free;
end;

{ TTreeManager }

constructor TTreeManager<T>.Create;
begin
  inherited Create;
  FData := TFslList<T>.create;
end;

destructor TTreeManager<T>.Destroy;
begin
  FData.Free;
  Inherited Destroy;
end;

function TTreeManager<T>.doLoad: boolean;
var
  f : T;
begin
  f := Focus.link;
  try
    FTree.Items.Clear;
    FData.Clear;
    result := LoadData;
    LoadTreeNodes;
  finally
    f.free;
  end;
end;

procedure TTreeManager<T>.refresh(item: T);
var
  i : T;
begin
  if (item = nil) then
  begin
    for i in FData do
      refreshTreeNode(i)
  end
  else if not refreshItem(item) then
    refreshTreeNode(item);
end;

procedure TTreeManager<T>.saveStatus;
begin
  // no status to save
end;

procedure TTreeManager<T>.doAdd(mode: String);
var
  item, pp : T;
  p : TTreeNode;
  s : String;
  i : integer;
begin
  pp := focus;
  if pp = nil then
    p := nil
  else
    p := pp.FNode;
  item := addItem(pp, mode);
  if item <> nil then
  begin
    if (p = nil) then
      item.FNode := FTree.Items.add(nil, '')
    else
      item.FNode := FTree.Items.addChild(p, '');
    item.FNode.Data := pointer(item);
    populateTreeNode(item);
    FTree.Selected := item.FNode;
    changed;
    updateStatus;
  end;
end;

procedure TTreeManager<T>.doAddSet(mode: String);
begin
  raise ETodo.create('doAddSet');
end;

procedure TTreeManager<T>.doEdit(mode: String);
begin
  changed;
  raise ETodo.create('doEdit');
end;

procedure TTreeManager<T>.doDelete(mode: String);
var
  f, pp : T;
  n, p : TTreeNode;
  s : String;
  i : integer;
begin
  f := focus;
  n := f.FNode;
  p := n.parent;
  if (p = nil) then
    pp := nil
  else
    pp := TFslObject(p.Data) as T;

  if AskOnDelete(f) then
  begin
    if (QuestionDlg('Delete', 'Delete '+getSummaryText(f)+' from '+getSummaryText(pp)+'?', mtConfirmation, [mrYes, mrNo], '') = mrYes) then
    begin
      DeleteItem(pp, f);
      FTree.items.delete(n);
      changed;
      updateStatus;
    end;
  end
  else
  begin
    if DeleteItem(pp, f) then
    begin
      FTree.items.delete(n);
      changed;
      updateStatus;
    end;
  end;
end;

procedure TTreeManager<T>.doExecute(mode: String);
begin
  if HasFocus then
    if ExecuteItem(getFocus, mode) then
      refreshTreeNode(getFocus);
end;

procedure TTreeManager<T>.doUpdate(mode: String);
var
  n : T;
begin
  if HasFocus then
  begin
    n := UpdateItem(getFocus, mode);
    if n <> nil then
      refreshItem(n);
  end;
end;

procedure TTreeManager<T>.doStop(mode: String);
begin
  if HasFocus then
    if StopItem(getFocus, mode) then
      refreshTreeNode(getFocus);
end;

procedure TTreeManager<T>.doInfo(mode: String);
var
  item : T;
begin
  if HasFocus then
  begin
    item := getFocus;
    itemInfo(item, mode);
  end;
end;

procedure TTreeManager<T>.doCopy(mode: String);
var
  item : T;
  s : String;
begin
  if HasFocus then
  begin
    item := getFocus;
    s := getCopyValue(item, mode);
    if (s <> '') then
      Clipboard.AsText := s;
  end;
end;

function TTreeManager<T>.AskOnDelete(item: T): boolean;
begin
  result := true;
end;

procedure TTreeManager<T>.setTree(value: TTreeView);
var
  i : integer;
begin
  FTree := value;
  FTree.MultiSelect := false;
  FTree.OnClick := doTreeChange;
  Tree.OnDblClick := doTreeDoubleClick;
  Tree.Images := FImages;
  FPopup.Images := FImages;
  FPopup.OnPopup := doPopup;
  buildMenu;
  if FPopup.Items.Count > 0 then
    FTree.PopupMenu := FPopup;
  FTree.ReadOnly := readOnly;
  FTree.OnEdited := DoEdited;
  FTree.SortType := stNone;
end;

procedure TTreeManager<T>.SetSettings(AValue: TIniFile);
begin
  FSettings := AValue;
  //  nothing to remember at this time
  //  if (FSettings <> nil) and (FTree <> nil) then
end;

function TTreeManager<T>.getFocus : T;
var
  sel : TTreeNode;
begin
  sel := Tree.Selected;
  if sel = nil then
    result := nil
  else
    result := TFslObject(sel.Data) as T;
end;

function TTreeManager<T>.GetHasFocus : boolean;
begin
  result := GetFocus <> nil;

end;

procedure TTreeManager<T>.SetEnabled(AValue: boolean);
//var
//  ce : TControlEntry;
begin
  //FEnabled := AValue;
  //FData.Clear;
  //FFiltered.Clear;
  ////if (FFilter <> nil) then
  ////  FFilter.ReadOnly := not Enabled;
  //for ce in FControls do
  //  ce.control.Enabled := Enabled;
  //if enabled then
  //  doLoad;
end;

procedure TTreeManager<T>.buildTreeNode(parent : TTreenode; item : T);
begin
  if item.FNode <> nil then
    raise ELibraryException.create('Node appears in the tree more than once');
  if (parent = nil) then
    item.FNode := FTree.Items.add(nil, '')
  else
    item.FNode := FTree.Items.addChild(parent, '');
  item.FNode.Data := pointer(item);
  populateTreeNode(item);
end;

procedure TTreeManager<T>.populateTreeNode(item : T);
var
  i : integer;
begin
  item.FNode.Text := getCellText(item);
  i := getImageIndex(item);
  item.FNode.ImageIndex := i;
  item.FNode.SelectedIndex := i;
  for i := 0 to item.getchildCount - 1 do
    buildTreeNode(item.FNode, item.getChild(i) as T);
end;

procedure TTreeManager<T>.refreshTreeNode(item: T);
var
  i : integer;
begin
  item.FNode.Text := getCellText(item);
  i := getImageIndex(item);
  item.FNode.ImageIndex := i;
  item.FNode.SelectedIndex := i;
  for i := 0 to item.getchildCount - 1 do
    refreshTreeNode(item.getChild(i) as T);
end;

procedure TTreeManager<T>.clearTreeNode(item : T);
var
  i : integer;
begin
  item.FNode := nil;
  for i := 0 to item.getchildCount - 1 do
    clearTreeNode(item.getChild(i) as T);
end;

procedure TTreeManager<T>.LoadTreeNodes;
var
  i : T;
begin
  for i in FData do
    clearTreeNode(i);

  for i in FData do
    buildTreeNode(nil, i);
end;

procedure TTreeManager<T>.doTreeChange(Sender: TObject);
begin
  updateStatus;
end;

procedure TTreeManager<T>.doTreeDoubleClick(Sender: TObject);
var
  item : T;
  i : integer;
begin
  if hasFocus then
  begin
    item := GetFocus;
    if doubleClickEdit then
    begin
      if FCanEdit and hasFocus and EditItem(item, '') then
      begin
        for i := item.FNode.Count - 1 downto 0 do
          item.FNode.Items[i].delete;
        populateTreeNode(item);
      end;
    end
    else
      doExecute('');
  end;
end;

procedure TTreeManager<T>.updateStatus;
var
  sel : TTreeNode;
  ops : TNodeOperationSet;
  op : TControlOperation;
begin
  sel := FTree.Selected;
  if sel = nil then
    ops := allowedOperations(nil)
  else
    ops := allowedOperations(TFslObject(sel.Data) as T);

  updateControls(copAdd, opAdd in ops);
  updateControls(copAddSet, opAdd in ops);
  updateControls(copEdit, (opEdit in ops) and (sel <> nil));
  updateControls(copDelete, (opDelete in ops) and (sel <> nil));
  updateControls(copRefresh, opRefresh in ops);
  updateControls(copExecute, opExecute in ops);
  updateControls(copUpdate, opUpdate in ops);
  updateControls(copStop, opStop in ops);
  updateControls(copCopy, FHasCopy);
  updateControls(copInfo, opInfo in ops);
  updateMenuControls;
  FCanEdit := opEdit in ops;

  focusItemChange(focus);
  if assigned(FOnSetFocus) then
    FOnSetFocus(self);
end;

procedure TTreeManager<T>.DoEdited(Sender: TObject; Node: TTreeNode; var S: string);
var
  item, pp : T;
  p : TTreeNode;
begin
  item := focus;
  p := item.FNode.parent;
  if (p <> nil) then
    pp := TFslObject(p.data) as T
  else
    pp := nil;
  if (s <> getCellText(item)) then
    if editItemText(pp, item, s) then
      changed
    else
      s := getCellText(item);
end;

procedure TTreeManager<T>.SetImages(AValue: TImagelist);
begin
  inherited SetImages(AValue);
  if FTree <> nil then
    FTree.images := AValue;
end;

procedure TTreeManager<T>.doPopup(sender: TObject);
  procedure visitItem(item : TMenuItem);
  var
    i : integer;
  begin
    if item.Tag = ord(copCopy) then
      item.Enabled := getCanCopy(focus, item.name.subString(7));
    for i := 0 to item.Count - 1 do
      visitItem(item.Items[i]);
  end;
begin
  visitItem(FPopup.items);
end;

procedure TTreeManager<T>.doControl(sender: TObject);
var
  entry : TControlEntry;
begin
  for entry in FControls do
    if entry.control = sender then
      case entry.op of
        copAdd : doAdd(entry.mode);
        copAddSet : doAddSet(entry.mode);
        copEdit : doEdit(entry.mode);
        copDelete : doDelete(entry.mode);
        //copUp : doUp;
        //copDown : doDown;
        copReload : doLoad;
        copRefresh : refresh(focus);
        copExecute : doExecute(entry.mode);
        copUpdate : doUpdate(entry.mode);
        copStop : doStop(entry.mode);
        copCopy : doCopy(entry.mode);
        copInfo : doInfo(entry.mode);
      end;
end;

procedure TTreeManager<T>.doMnuClick(Sender: TObject);
var
  mnu : TMenuItem;
  mode : String;
begin
  mnu := (Sender as TMenuItem);
  if mnu.Name.StartsWith('mnuMode') then
    mode := mnu.Name.Substring(7)
  else
    mode := '';

  case TControlOperation(mnu.Tag) of
    copAdd : doAdd(mode);
    copAddSet : doAddSet(mode);
    copEdit : doEdit(mode);
    copDelete : doDelete(mode);
    //copUp : doUp;
    //copDown : doDown;
    copReload : doLoad;
    copRefresh : refresh(focus);
    copExecute : doExecute(mode);
    copUpdate : doUpdate(mode);
    copStop : doStop(mode);
    copCopy : doCopy(mode);
    copInfo : doInfo(mode);
  end;
end;

function TTreeManager<T>.readOnly: boolean;
begin

end;

procedure TTreeManager<T>.buildMenu;
begin
end;

function TTreeManager<T>.getImageIndex(item : T) : integer;
begin
  result := -1;
end;

function TTreeManager<T>.getCellColors(item : T; var fore, back : TColor) : boolean;
begin
  result := false;
end;

function TTreeManager<T>.getSummaryText(item : T) : String;
begin
  result := getCellText(item);
end;

procedure TTreeManager<T>.changed;
begin
  //  nothing
end;

procedure TTreeManager<T>.focusItemChange(item: T);
begin
  // nothing here
end;

function TTreeManager<T>.addItem(parent : T; mode : String) : T;
begin
  result := nil;
end;

function TTreeManager<T>.editItem(item : T; mode : String) : boolean;
begin
  result := false;
end;

function TTreeManager<T>.editItemText(parent, item: T; var text: String): boolean;
begin
    result := false;
end;

function TTreeManager<T>.deleteItem(parent, item : T) : boolean;
begin
  raise EFslException.Create('Delete is not supported here');
end;

function TTreeManager<T>.executeItem(item : T; mode : String) : boolean;
begin
  raise EFslException.Create('Execute is not supported here');
end;

function TTreeManager<T>.updateItem(item : T; mode : String) : T;
begin
  raise EFslException.Create('Update is not supported here');
end;

function TTreeManager<T>.stopItem(item : T; mode : String) : boolean;
begin
  raise EFslException.Create('Stop is not supported here');
end;

function TTreeManager<T>.refreshItem(item: T) : boolean;
begin
  result := false;
end;

function TTreeManager<T>.getCopyValue(item: T; mode: String): String;
begin
  if mode = 'caption' then
    result := getCellText(item)
  else
    result := '';
end;

function TTreeManager<T>.getCanCopy(item: T; mode: String): boolean;
begin
  result := item <> nil;
end;

procedure TTreeManager<T>.itemInfo(item: T; mode: String);
begin
  raise EFslException.Create('ItemInfo is not supported here');
end;

{ TVTreeManager }

constructor TVTreeManager<T>.Create;
begin
  inherited Create;
  FData := TFslList<T>.create;
end;

destructor TVTreeManager<T>.Destroy;
begin
  FData.Free;
  Inherited Destroy;
end;

function TVTreeManager<T>.doLoad: boolean;
var
  f : T;
begin
  f := Focus.link;
  try
    FTree.Clear;
    FData.Clear;
    result := LoadData;
    LoadTreeNodes;
  finally
    f.free;
  end;
end;

procedure TVTreeManager<T>.refresh(item: T);
var
  i : T;
begin
  if (item = nil) then
  begin
    for i in FData do
      refreshTreeNode(i)
  end
  else if not refreshItem(item) then
    refreshTreeNode(item);
end;

procedure TVTreeManager<T>.saveStatus;
begin
  // no status to save
end;

procedure TVTreeManager<T>.doAdd(mode: String);
var
  item, pp : T;
  p : PVirtualNode;
  s : String;
  i : integer;
begin
  pp := focus;
  if pp = nil then
    p := nil
  else
    p := pp.FPNode;
  item := addItem(pp, mode);
  if item <> nil then
  begin
    item.FPNode := FTree.addChild(p);
    setT(item.FPNode, item);
    populateTreeNode(item);
    FTree.Selected[item.FPNode] := true;
    changed;
    updateStatus;
  end;
end;

procedure TVTreeManager<T>.doAddSet(mode: String);
begin
  raise ETodo.create('doAddSet');
end;

procedure TVTreeManager<T>.doEdit(mode: String);
begin
  if focus <> nil then
    if editItem(focus, mode) then
    begin
      updateStatus;
      FTree.invalidateNode(focus.FPNode);
    end;
end;

procedure TVTreeManager<T>.doDelete(mode: String);
var
  f, pp : T;
  n, p : PVirtualNode;
  s : String;
  i : integer;
begin
  f := focus;
  n := f.FPNode;
  p := n.parent;
  if (p = nil) then
    pp := nil
  else
    pp := getT(p);

  if AskOnDelete(f) then
  begin
    if (QuestionDlg('Delete', 'Delete '+getSummaryText(f)+' from '+getSummaryText(pp)+'?', mtConfirmation, [mrYes, mrNo], '') = mrYes) then
    begin
      DeleteItem(pp, f);
      FTree.deleteNode(n);
      changed;
      updateStatus;
    end;
  end
  else
  begin
    if DeleteItem(pp, f) then
    begin
      FTree.deleteNode(n);
      changed;
      updateStatus;
    end;
  end;
end;

procedure TVTreeManager<T>.doExecute(mode: String);
begin
  if HasFocus then
    if ExecuteItem(getFocus, mode) then
      refreshTreeNode(getFocus);
end;

procedure TVTreeManager<T>.doUpdate(mode: String);
var
  n : T;
begin
  if HasFocus then
  begin
    n := UpdateItem(getFocus, mode);
    if n <> nil then
      refreshitem(getFocus);
  end;
end;

procedure TVTreeManager<T>.doStop(mode: String);
begin
  if HasFocus then
    if StopItem(getFocus, mode) then
      refreshTreeNode(getFocus);
end;

procedure TVTreeManager<T>.doInfo(mode: String);
var
  item : T;
begin
  if HasFocus then
  begin
    item := getFocus;
    itemInfo(item, mode);
  end;
end;

procedure TVTreeManager<T>.doCopy(mode: String);
var
  item : T;
  s : String;
begin
  if HasFocus then
  begin
    item := getFocus;
    s := getCopyValue(item, mode);
    if (s <> '') then
      Clipboard.AsText := s;
  end;
end;

function TVTreeManager<T>.AskOnDelete(item: T): boolean;
begin
  result := true;
end;

function TVTreeManager<T>.getT(p: PVirtualNode): T;
var
  data: PTreeData;
begin
  data := FTree.getNodeData(p);
  result := data.item as T;
end;

procedure TVTreeManager<T>.setT(p: PVirtualNode; item: T);
var
  data: PTreeData;
begin
  data := FTree.getNodeData(p);
  data.item := item;
end;

procedure TVTreeManager<T>.setTree(value: TVirtualStringTree);
var
  i : integer;
begin
  FTree := value;
  FTree.TreeOptions.SelectionOptions := [toFullRowSelect, toRightClickSelect, toAlwaysSelectNode];
  FTree.TreeOptions.AnimationOptions := [];
  FTree.TreeOptions.AutoOptions := [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoExpand];
  if readonly then
    FTree.TreeOptions.MiscOptions := [toReportMode]
  else
    FTree.TreeOptions.MiscOptions := [toEditable, toReportMode, toEditOnClick];
  FTree.TreeOptions.PaintOptions := [toShowButtons, toShowTreeLines, toShowRoot];

  FTree.OnClick := doTreeChange;
  Tree.OnDblClick := doTreeDoubleClick;
  Tree.Images := FImages;
  FPopup.Images := FImages;
  FPopup.OnPopup := doPopup;
  buildMenu;
  if FPopup.Items.Count > 0 then
    FTree.PopupMenu := FPopup;
  FTree.OnEdited := DoEdited;
  FTree.header.SortColumn := -1;
  FTree.header.SortDirection := sdAscending;
  FTree.NodeDataSize := sizeof(pointer);
  FTree.OnGetText := doGetTreeText;
  FTree.OnGetImageIndex := doGetTreeImageIndex;
  FTree.OnPaintText := doPaintTreeText;
  FTree.OnBeforeCellPaint := doPaintTreeCell;
end;

procedure TVTreeManager<T>.SetSettings(AValue: TIniFile);
begin
  FSettings := AValue;
  //  nothing to remember at this time
  //  if (FSettings <> nil) and (FTree <> nil) then
end;

function TVTreeManager<T>.getFocus : T;
var
  sel : PVirtualNode;
begin
  sel := FTree.GetFirstSelected;
  if sel = nil then
    result := nil
  else
    result := getT(sel);
end;

function TVTreeManager<T>.GetHasFocus : boolean;
begin
  result := GetFocus <> nil;

end;

procedure TVTreeManager<T>.SetEnabled(AValue: boolean);
//var
//  ce : TControlEntry;
begin
  //FEnabled := AValue;
  //FData.Clear;
  //FFiltered.Clear;
  ////if (FFilter <> nil) then
  ////  FFilter.ReadOnly := not Enabled;
  //for ce in FControls do
  //  ce.control.Enabled := Enabled;
  //if enabled then
  //  doLoad;
end;

procedure TVTreeManager<T>.buildTreeNode(parent : PVirtualNode; item : T);
begin
  if item.FNode <> nil then
    raise ELibraryException.create('Node appears in the tree more than once');
  item.FPNode := FTree.AddChild(parent);
  setT(item.FPNode, item);
  populateTreeNode(item);
end;

procedure TVTreeManager<T>.populateTreeNode(item : T);
var
  i : integer;
begin
  for i := 0 to item.getchildCount - 1 do
    buildTreeNode(item.FPNode, item.getChild(i) as T);
end;

procedure TVTreeManager<T>.refreshTreeNode(item: T);
var
  i : integer;
begin
  FTree.invalidateNode(item.FPNode);
  for i := 0 to item.getchildCount - 1 do
    refreshTreeNode(item.getChild(i) as T);
end;

procedure TVTreeManager<T>.clearTreeNode(item : T);
var
  i : integer;
begin
  item.FNode := nil;
  for i := 0 to item.getchildCount - 1 do
    clearTreeNode(item.getChild(i) as T);
end;

procedure TVTreeManager<T>.LoadTreeNodes;
var
  i : T;
begin
  for i in FData do
    clearTreeNode(i);

  for i in FData do
    buildTreeNode(nil, i);
end;

procedure TVTreeManager<T>.doTreeChange(Sender: TObject);
begin
  updateStatus;
end;

procedure TVTreeManager<T>.doTreeDoubleClick(Sender: TObject);
var
  item : T;
  i : integer;
begin
  if hasFocus then
  begin
    item := GetFocus;
    if doubleClickEdit then
    begin
      if FCanEdit and hasFocus and EditItem(item, '') then
      begin
        for i := item.FNode.Count - 1 downto 0 do
          item.FNode.Items[i].delete;
        populateTreeNode(item);
      end;
    end
    else
      doExecute('');
  end;
end;

procedure TVTreeManager<T>.updateStatus;
var
  sel : PVirtualNode;
  ops : TNodeOperationSet;
  op : TControlOperation;
begin
  sel := FTree.GetFirstSelected;
  if sel = nil then
    ops := allowedOperations(nil)
  else
    ops := allowedOperations(getT(sel));

  updateControls(copAdd, opAdd in ops);
  updateControls(copAddSet, opAdd in ops);
  updateControls(copEdit, (opEdit in ops) and (sel <> nil));
  updateControls(copDelete, (opDelete in ops) and (sel <> nil));
  updateControls(copRefresh, opRefresh in ops);
  updateControls(copExecute, opExecute in ops);
  updateControls(copUpdate, opUpdate in ops);
  updateControls(copStop, opStop in ops);
  updateControls(copCopy, FHasCopy);
  updateControls(copInfo, opInfo in ops);
  updateMenuControls;
  FCanEdit := opEdit in ops;

  focusItemChange(focus);
  if assigned(FOnSetFocus) then
    FOnSetFocus(self);
end;

procedure TVTreeManager<T>.DoEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  item, pp : T;
  p : PVirtualNode;
begin
  item := focus;
  p := item.FPNode.parent;
  if (p <> nil) then
    pp := getT(p)
  else
    pp := nil;
  // todo: editing....
  //if (s <> getCellText(item)) then
  //  if editItemText(pp, item, s) then
  //    changed
  //  else
  //    s := getCellText(item);
end;

procedure TVTreeManager<T>.SetImages(AValue: TImagelist);
begin
  inherited SetImages(AValue);
  if FTree <> nil then
    FTree.images := AValue;
end;

procedure TVTreeManager<T>.doGetTreeText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  item : T;
begin
  item := getT(node);
  CellText := getCellText(item);
end;

procedure TVTreeManager<T>.doGetTreeImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  Ghosted := false;
  ImageIndex := getImageIndex(GetT(node));
end;

procedure TVTreeManager<T>.doPaintTreeText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  fore, back : TColor;
begin
  fore := clBlack;
  back := clWhite;
  getCellColors(getT(node), fore, back);
  TargetCanvas.Font.Color := fore;
end;

procedure TVTreeManager<T>.doPaintTreeCell(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  fore, back : TColor;
begin
  fore := clBlack;
  back := clWhite;
  getCellColors(getT(node), fore, back);
  //TargetCanvas.Brush.Color := back;
  //TargetCanvas.Brush.Style := bsSolid;
  //TargetCanvas.Rectangle(cellRect);
end;

procedure TVTreeManager<T>.doPopup(sender: TObject);
  procedure visitItem(item : TMenuItem);
  var
    i : integer;
  begin
    if item.Tag = ord(copCopy) then
      item.Enabled := getCanCopy(focus, item.name.subString(7));
    for i := 0 to item.Count do
      visitItem(item.Items[i]);
  end;
begin
  visitItem(FPopup.items);
end;

procedure TVTreeManager<T>.doControl(sender: TObject);
var
  entry : TControlEntry;
begin
  for entry in FControls do
    if entry.control = sender then
      case entry.op of
        copAdd : doAdd(entry.mode);
        copAddSet : doAddSet(entry.mode);
        copEdit : doEdit(entry.mode);
        copDelete : doDelete(entry.mode);
        //copUp : doUp;
        //copDown : doDown;
        copReload : doLoad;
        copRefresh : refresh(focus);
        copExecute : doExecute(entry.mode);
        copupdate : doUpdate(entry.mode);
        copStop : doStop(entry.mode);
        copCopy : doCopy(entry.mode);
        copInfo : doInfo(entry.mode);
      end;
end;

procedure TVTreeManager<T>.doMnuClick(Sender: TObject);
var
  mnu : TMenuItem;
  mode : String;
begin
  mnu := (Sender as TMenuItem);
  if mnu.Name.StartsWith('mnuMode') then
    mode := mnu.Name.Substring(7)
  else
    mode := '';

  case TControlOperation(mnu.Tag) of
    copAdd : doAdd(mode);
    copAddSet : doAddSet(mode);
    copEdit : doEdit(mode);
    copDelete : doDelete(mode);
    //copUp : doUp;
    //copDown : doDown;
    copReload : doLoad;
    copRefresh : refresh(focus);
    copExecute : doExecute(mode);
    copUpdate : doUpdate(mode);
    copStop : doStop(mode);
    copCopy : doCopy(mode);
    copInfo : doInfo(mode);
  end;
end;

function TVTreeManager<T>.readOnly: boolean;
begin

end;

procedure TVTreeManager<T>.buildMenu;
begin
end;

function TVTreeManager<T>.getImageIndex(item : T) : integer;
begin
  result := -1;
end;

function TVTreeManager<T>.getCellColors(item : T; var fore, back : TColor) : boolean;
begin
  result := false;
end;

function TVTreeManager<T>.getSummaryText(item : T) : String;
begin
  result := getCellText(item);
end;

procedure TVTreeManager<T>.changed;
begin
  //  nothing
end;

procedure TVTreeManager<T>.focusItemChange(item: T);
begin
  // nothing here
end;

function TVTreeManager<T>.addItem(parent : T; mode : String) : T;
begin
  result := nil;
end;

function TVTreeManager<T>.editItem(item : T; mode : String) : boolean;
begin
  result := false;
end;

function TVTreeManager<T>.editItemText(parent, item: T; var text: String): boolean;
begin
    result := false;
end;

function TVTreeManager<T>.deleteItem(parent, item : T) : boolean;
begin
  raise EFslException.Create('Delete is not supported here');
end;

function TVTreeManager<T>.executeItem(item : T; mode : String) : boolean;
begin
  raise EFslException.Create('Execute is not supported here');
end;

function TVTreeManager<T>.updateItem(item : T; mode : String) : T;
begin
  raise EFslException.Create('Update is not supported here');
end;

function TVTreeManager<T>.stopItem(item : T; mode : String) : boolean;
begin
  raise EFslException.Create('Stop is not supported here');
end;

function TVTreeManager<T>.refreshItem(item: T) : boolean;
begin
  result := false;
end;

function TVTreeManager<T>.getCopyValue(item: T; mode: String): String;
begin
  if mode = 'caption' then
    result := getCellText(item)
  else
    result := '';
end;

function TVTreeManager<T>.getCanCopy(item: T; mode: String): boolean;
begin
  result := item <> nil;
end;

procedure TVTreeManager<T>.itemInfo(item: T; mode: String);
begin
  raise EFslException.Create('ItemInfo is not supported here');
end;

{ TPanelStack }

constructor TPanelStack.Create(space: TPanel);
begin
  inherited Create;
  FSpace := space;
  FSpace.OnResize := doResize;
  FPanels := TFslList<TPanelStackSubPanel>.create;
end;

destructor TPanelStack.Destroy;
begin
  FPanels.Free;
  inherited Destroy;
end;

function TPanelStack.GetPanel(index : integer): TPanel;
begin
  result := FPanels[index].container;
end;

procedure TPanelStack.AddPanel;
var
  h, c : TPanel;
  s : TSplitter;
begin
  c := TPanel.create(FSpace);
  if FPanels.Count = 0 then // the first is special
  begin
    c.parent := FSpace;
    c.align := alClient;
    c.Height := FSpace.height;
    s := nil;
  end
  else
  begin
    c.parent := FPanels.Last.container;
    c.align := alBottom;
    c.Height := FSpace.height div FCount; // gets fixed up later
    s := TSplitter.create(FSpace);
    s.parent := FPanels.Last.Container;
    s.align := alBottom;
  end;
  c.BevelOuter := bvNone;
  c.caption := '';
  c.ParentColor := false;
  c.color := clWhite;
  h := TPanel.create(c);
  h.parent := c;
  h.align := alTop;
  h.Height := 26;
  h.BevelOuter := bvNone;
  h.Alignment := taLeftJustify;
  h.caption := '  Caption';
  h.ParentColor := false;
  h.color := clBtnFace;
  FPanels.add(TPanelStackSubPanel.create(c, h));
end;

procedure TPanelStack.doResize(sender: TObject);
var
  i, t, h : integer;
begin

  if FLastSize = 0 then
    exit;
  t := 0;
  for i := FCount - 1 downto 0 do
  begin
    h := FPanels[i].container.height - t;
    FPanels[i].fraction := h / FLastSize;
    t := FPanels[i].container.height;
  end;

  t := 0;
  for i := FCount - 1 downto 0 do
  begin
    h := trunc(FPanels[i].fraction * FSpace.height);
    inc(t, h);
    FPanels[i].container.height := t;
  end;
  FLastSize := FSpace.Height;
end;


procedure TPanelStack.reset(count: integer);
var
  i : integer;
begin
  FLastSize := 0;
  FCount := count;
  while (FPanels.count <= count) do
    AddPanel;
  // we can't delete panels we don't need, as they might contain content we don't want to lose, so we just hide them
  for i := count to FPanels.Count - 1 do
    FPanels[i].container.visible := false;
end;

procedure TPanelStack.setCaption(index: integer; caption: String; imageIndex: integer);
begin
  FPanels[index].heading.caption := '  '+caption;
end;


procedure TPanelStack.resize;
var
  i, t : integer;
begin
  t := 0;
  for i := FCount - 1 downto 0 do
  begin
    inc(t, FSpace.height div FCount);
    FPanels[i].container.height := t;
  end;
  FLastSize := FSpace.Height;


end;

End.

