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

- TObjectManager - binds a set of edits that edit the properties of an object
- TListManager - binds a list of objects to a set of UI controls, with a TListView as the centerpiece
- TTreeManager - binds a tree of objects to a set of UI controls, with a TTreeView as a centerpiece
- TFHIRSynEditSynchroniser - keeps a SynEdit source for a resource in sync with a loaded resource

}

Interface

uses
  SysUtils, Classes, Graphics, IniFiles,
  Controls, StdCtrls, Buttons, ExtCtrls, EditBtn, ComCtrls, Dialogs, Menus,
  SynEdit, SynEditTypes,
  fsl_base, fsl_stream, fsl_http,
  fhir_objects, fhir_factory, fhir_parser;

type
  TNodeOperation = (opAdd, opDelete, opEdit, opExecute, opOrder, opHeirarchy);
  TNodeOperationSet = set of TNodeOperation;

//
//  TTreeManager = class abstract (TFslObject)
//  public
//    constructor Create(tree : TTreeView);
//    destructor Destroy; override;
//
//    function getChildcount : integer; virtual; abstract;
//    procedure getChild(var kind : integer; var item : TFslObject); virtual; abstract;
//    function getDisplay(kind : integer; obj : TFslObject) : String; virtual; abstract;
//    function getImageIndex(kind : integer; obj : TFslObject) : integer; virtual; abstract;
//    function getOperations(kind : integer; obj : TFslObject) : TOperationSet; virtual; abstract;
//  end;

  { TControlEntry }

  TControlOperation = (copAdd, copEdit, copDelete, copUp, copDown, copReload, copExecute);

  TControlEntry = class (TFslObject)
  private
    FControl: TControl;
    FMode: String;
    FOp: TControlOperation;
  public
    function link : TControlEntry; overload;

    property control : TControl read FControl write FControl;
    property op : TControlOperation read FOp write FOp;
    property mode : String read FMode write FMode;
  end;

  { TListManager }

  TListManagerBase = class (TFslObject)
  end;

  TListManager<T : TFslObject> = class abstract (TListManagerBase)
  private
    FData : TFslList<T>;
    FFiltered : TFslList<T>;
    FImages: TImagelist;
    FList : TListView;
    FFilter : TEdit;
    FControls : TFslList<TControlEntry>;
    FEnabled : boolean;
    FCanEdit : boolean;
    FOnSetFocus: TNotifyEvent;
    FSettings: TIniFile;
    FPopup : TPopupMenu;

    procedure doFilter;
    function GetHasFocus: boolean;
    procedure rebuild(focus : T);
    procedure doControl(sender : TObject);
    procedure FilterChange(sender : TObject);
    function Filtered : boolean;
    function GetFocus: T;
    procedure SetEnabled(AValue: boolean);
    procedure SetFilter(AValue: TEdit);
    procedure SetList(AValue: TListView);
    procedure updateStatus;
    procedure updateControls(op : TControlOperation; allowed : boolean);
    procedure doListCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var result: Integer);
    procedure doListChange(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure doListDoubleClick(Sender: TObject);
    procedure doListDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure doMnuClick(Sender: TObject);
    procedure populateEntry(entry : TListItem; item : T);
  public
    constructor Create; override;
    destructor Destroy; override;

    // configuration
    property Data : TFslList<T> read FData;
    property Focus : T read GetFocus;
    property hasFocus : boolean read GetHasFocus;
    property List : TListView read FList write SetList;
    property Images : TImagelist read FImages write FImages;
    property Filter : TEdit read FFilter write SetFilter;
    procedure registerControl(c : TControl; op : TControlOperation; mode : String = '');
    procedure registerMenuEntry(caption : String; imageIndex : integer; op : TControlOperation);
    Property Enabled : boolean read FEnabled write SetEnabled;
    property Settings : TIniFile read FSettings write FSettings;

    property OnSetFocus : TNotifyEvent read FOnSetFocus write FOnSetFocus;

    // control. These are not usually needed from outside
    procedure doAdd(mode : String);
    procedure doEdit(mode : String);
    procedure doDelete(mode : String);
    procedure doUp;
    procedure doDown;
    procedure doExecute(mode : String);
    function doLoad : boolean;
    procedure refresh(item : T = nil); // leaves the items in place (and doesn't refilter) but updates text displays. it item = nil, updates all displayed items

    // to override:
    function canSort : boolean; virtual;
    function allowedOperations(item : T) : TNodeOperationSet; virtual; abstract; // return what is allowed in principle; no need to be concerned with the selection, except for whether modify/delete is allowed
    function loadList : boolean; virtual; abstract; // return false if not loaded ok

    procedure buildMenu; virtual;
    function getImageIndex(item : T) : integer; virtual; abstract;
    function getCellText(item : T; col : integer) : String; virtual; abstract;
    function getCellColors(item : T; col : integer; var fore, back : TColor) : boolean; virtual;
    function getSummaryText(item : T) : String; virtual;
    function compareItem(left, right : T; col : integer) : integer; virtual; // if col is -1, then the comparison is for the object as a whole
    function filterItem(item : T; s : String) : boolean; virtual;

    function AddItem(mode : String) : T; virtual;
    function EditItem(item : T; mode : String) : boolean; virtual;
    procedure DeleteItem(item : T); virtual;
    function ExecuteItem(item : T; mode : String) : boolean; virtual;
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
    FList: TListManagerBase;
    FName: String;
    FOnFillList: TObjectManagerControl;
    FOnLookup: TLookupValueEvent;
    procedure SetList(AValue: TListManagerBase);
  public
    constructor Create(kind : TObjectManagerControlKind); overload;
    destructor Destroy; override;

    property name : String read FName write FName;
    property kind : TObjectManagerControlKind read FKind write FKind;
    property list : TListManagerBase read FList write SetList;
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
    procedure registerManager(propName : String; manager : TListManagerBase; fillManager : TFillListManagerEvent); overload;
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


Implementation

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

procedure TObjectManagerControl.SetList(AValue: TListManagerBase);
begin
  FList.Free;
  FList := AValue;
end;

{ TObjectManager }

procedure TObjectManager.SetFocus(AValue: TFHIRObject);
begin
  if FFocus=AValue then Exit;
  FFocus:=AValue;
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

procedure TObjectManager.registerManager(propName: String; manager: TListManagerBase; fillManager: TFillListManagerEvent);
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

function TControlEntry.link: TControlEntry;
begin
  result := TControlEntry(inherited link);
end;

{ TListManager }

constructor TListManager<T>.Create;
begin
  inherited Create;
  FData := TFslList<T>.create;
  FFiltered := TFslList<T>.create;
  FControls := TFslList<TControlEntry>.create;
  FPopup := TPopupMenu.create(nil);
end;

destructor TListManager<T>.Destroy;
var
  i : integer;
begin
  if FSettings <> nil then
    for i := 0 to List.columns.count - 1 do
      FSettings.WriteInteger(List.Name, 'column'+inttostr(i), list.columns[i].width);

  FControls.Free;
  FFiltered.free;
  FData.free;
  inherited Destroy;
end;

procedure TListManager<T>.registerControl(c : TControl; op : TControlOperation; mode : String = '');
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

procedure TListManager<T>.registerMenuEntry(caption: String; imageIndex: integer; op: TControlOperation);
var
  item : TMenuItem;
begin
  item  := TMenuItem.create(nil);
  FPopup.Items.add(item);
  item.caption := caption;
  item.imageIndex := imageIndex;
  item.Tag := Integer(op);
  item.OnClick := doMnuClick;
end;

procedure TListManager<T>.doFilter;
var
  f, item : T;
begin
  f := Focus;
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
        copEdit : doEdit(entry.mode);
        copDelete : doDelete(entry.mode);
        copUp : doUp;
        copDown : doDown;
        copReload : doLoad;
        copExecute : doExecute(entry.mode);
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
  if FList.itemindex = -1 then
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

procedure TListManager<T>.SetList(AValue: TListView);
var
  i : integer;
begin
  FList := AValue;
  List.OnCompare := doListCompare;
  List.OnSelectItem := doListChange;
  List.OnDblClick := doListDoubleClick;
  List.OnCustomDrawSubItem := doListDrawSubItem;
  FPopup.Images := FImages;
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
    List.SortDirection := sdAscending;
    List.SortType := stData;
  end
  else
  begin
    List.AutoSort := false;
    List.AutoSortIndicator := false;
    List.SortType := stNone;
  end;
end;

procedure TListManager<T>.updateStatus;
var
  i : integer;
  ops : TNodeOperationSet;
  op : TControlOperation;
begin
  i := FList.itemIndex;
  if i = -1 then
    ops := allowedOperations(nil)
  else
    ops := allowedOperations(FFiltered[i]);

  updateControls(copAdd, opAdd in ops);
  updateControls(copEdit, (opEdit in ops) and (i > -1));
  updateControls(copDelete, (opDelete in ops) and (i > -1));
  updateControls(copUp, (opOrder in ops) and (i > 0) and not Filtered);
  updateControls(copDown, (opOrder in ops) and (i > -1) and (i < FFiltered.count - 1) and not Filtered);
  updateControls(copReload, true);
  updateControls(copExecute, opExecute in ops);
  FCanEdit := opEdit in ops;

  if assigned(FOnSetFocus) then
    FOnSetFocus(self);
end;

procedure TListManager<T>.updateControls(op: TControlOperation; allowed: boolean);
var
  entry : TControlEntry;
  mnu : TMenuItem;
  i : integer;
begin
  for entry in FControls do
    if entry.op = op then
      entry.control.enabled := allowed;
  for i := 0 to FPopup.Items.Count - 1 do
    if (TControlOperation(FPopup.Items[i].tag) = op) then
      FPopup.Items[i].enabled := allowed;
end;

procedure TListManager<T>.doListCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var result: Integer);
var
  left, right : T;
begin
  left := T(item1.data);
  right := T(item2.data);
  result := CompareItem(left, right, List.SortColumn);
  if List.SortDirection = sdDescending then
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
  if FCanEdit and hasFocus and EditItem(item, '') then
  begin
    entry := FList.items[FList.itemindex];
    entry.SubItems.Clear;
    populateEntry(entry, item);
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

procedure TListManager<T>.doMnuClick(Sender: TObject);
begin
  case TControlOperation((Sender as TMenuItem).Tag) of
    copAdd : doAdd('');
    copEdit : doEdit('');
    copDelete : doDelete('');
    copUp : doUp;
    copDown : doDown;
    copReload : doLoad;
    copExecute : doExecute('');
  end;
end;

procedure TListManager<T>.doAdd(mode : String);
var
  item, itemT, itemN : T;
  entry : TListItem;
  i : integer;
begin
  item := addItem(mode);
  if (item <> nil) then
  begin
    try
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
      updateStatus;
    finally
      item.Free;
    end;
  end;
end;

procedure TListManager<T>.populateEntry(entry : TListItem; item : T);
var
  c : integer;
begin
  entry.caption := getCellText(item, 0);
  entry.subItems.Clear;
  for c := 1 to FList.columnCount - 1 do
    entry.subItems.add(getCellText(item, c));
end;

procedure TListManager<T>.buildMenu;
begin
end;

procedure TListManager<T>.doEdit(mode : String);
begin
  raise Exception.create('not done yet');
end;

procedure TListManager<T>.doDelete(mode : String);
var
  f : T;
  s : String;
  i : integer;
begin
  f := focus;
  if (QuestionDlg('Delete', 'Delete '+getSummaryText(f)+'?', mtConfirmation, [mrYes, mrNo], '') = mrYes) then
  begin
    DeleteItem(f);
    i := FList.itemIndex;
    FData.Delete(i);
    FList.Items.Delete(i);
    updateStatus;
  end;
end;

procedure TListManager<T>.doUp;
begin
  raise Exception.create('not done yet');
end;

procedure TListManager<T>.doDown;
begin
  raise Exception.create('not done yet');
end;

procedure TListManager<T>.doExecute(mode: String);
begin
  if HasFocus then
    if ExecuteItem(getFocus, mode) then
      refresh(getFocus);
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
    focus.free;
  end;
end;

procedure TListManager<T>.refresh(item : T);
var
  ti : T;
  entry : TListItem;
  i, c : integer;
begin
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

function TListManager<T>.AddItem(mode : String): T;
begin
  result := nil;
end;

function TListManager<T>.EditItem(item : T; mode: String): boolean;
begin
  result := false;
end;

procedure TListManager<T>.DeleteItem(item: T);
begin
  raise Exception.create('Delete is not supported here');
end;

function TListManager<T>.ExecuteItem(item: T; mode: String) : boolean;
begin
  raise Exception.create('Execute is not supported here');
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
  FFocus.Free;  // though we really expect it to nil
  FFactory.free;
  inherited Destroy;
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
    raise Exception.create('not supported yet');

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
    raise Exception.create('This format is not supported');
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
  raise Exception.create('not done yet');
end;

procedure TFHIRSynEditSynchroniser.deleteProperty(owner: TFHIRObject; name: String);
begin
  raise Exception.create('not done yet');
end;

procedure TFHIRSynEditSynchroniser.addToList(owner: TFHIRObject; name: String; after: TFHIRObject);
begin
  raise Exception.create('not done yet');
end;

procedure TFHIRSynEditSynchroniser.deleteFromList(owner: TFHIRObject; name: String; obj: TFHIRObject);
begin
  raise Exception.create('not done yet');
end;

procedure TFHIRSynEditSynchroniser.moveInList(owner: TFHIRObject; name: String; obj: TFHIRObject; up: boolean);
begin
  raise Exception.create('not done yet');
end;

procedure TFHIRSynEditSynchroniser.commit;
begin
  case FOpInProgress of
    opNone : raise Exception.create('No operation in process');
    opChange : finishOpChange;
  else
    raise Exception.create('not done yet');
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

End.

