Unit FHIR.LCL.Managers;

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

{$I fhir.inc}

Interface

uses
  SysUtils, Classes,
  Controls, ComCtrls, Dialogs,

  FHIR.Support.Base;

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

  TControlOperation = (copAdd, copEdit, copDelete, copUp, copDown, copReload);

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

  TListManager<T : TFslObject> = class abstract (TFslObject)
  private
    FData : TFslList<T>;
    FList : TListView;
    FControls : TFslList<TControlEntry>;

    procedure doControl(sender : TObject);
    function GetFocus: T;
    procedure SetList(AValue: TListView);
    procedure updateStatus;
    procedure updateControls(op : TControlOperation; allowed : boolean);
    procedure doListCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var result: Integer);
    procedure doListChange(Sender: TObject; Item: TListItem; Selected: Boolean);
  public
    constructor Create; override;
    destructor Destroy; override;

    // configuration
    property Data : TFslList<T> read FData;
    property Focus : T read GetFocus;
    property List : TListView read FList write SetList;
    procedure registerControl(c : TControl; op : TControlOperation; mode : String = '');

    // control. These are not usually needed from outside
    procedure doAdd(mode : String);
    procedure doEdit(mode : String);
    procedure doDelete(mode : String);
    procedure doUp;
    procedure doDown;
    function doLoad : boolean;

    // to override:
    function canSort : boolean; virtual;
    function allowedOperations(item : T) : TNodeOperationSet; virtual; abstract; // return what is allowed in principle; no need to be concerned with the selection, except for whether modify/delete is allowed
    function ShowLoadingProgress : boolean; virtual;
    function loadList : boolean; virtual; abstract; // return false if not loaded ok

    function getImageIndex(item : T) : integer; virtual; abstract;
    function getCellText(item : T; col : integer) : String; virtual; abstract;
    function getSummaryText(item : T) : String; virtual;
    function compare(left, right : T; col : integer) : integer; virtual; // if col is -1, then the comparison is for the object as a whole

    function AddItem(mode : String) : T; virtual;
    procedure DeleteItem(item : T); virtual;
  end;

Implementation

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
  FControls := TFslList<TControlEntry>.create;
end;

destructor TListManager<T>.Destroy;
begin
  FControls.Free;
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
      end;
end;

function TListManager<T>.GetFocus: T;
begin
  if FList.itemindex = -1 then
    result := nil
  else
    result := FData[FList.itemIndex];
end;

procedure TListManager<T>.SetList(AValue: TListView);
begin
  FList := AValue;
  List.OnCompare := doListCompare;
  List.OnSelectItem := doListChange;
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
    ops := allowedOperations(FData[i]);

  updateControls(copAdd, opAdd in ops);
  updateControls(copEdit, (opEdit in ops) and (i > -1));
  updateControls(copDelete, (opDelete in ops) and (i > -1));
  updateControls(copUp, (opOrder in ops) and (i > 0));
  updateControls(copDown, (opOrder in ops) and (i > -1) and (i < FData.count - 1));
  updateControls(copReload, true);
end;

procedure TListManager<T>.updateControls(op: TControlOperation; allowed: boolean);
var
  entry : TControlEntry;
begin
  for entry in FControls do
    if entry.op = op then
      entry.control.enabled := allowed;
end;

procedure TListManager<T>.doListCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var result: Integer);
var
  left, right : T;
begin
  left := T(item1.data);
  right := T(item2.data);
  result := Compare(left, right, List.SortColumn);
  if List.SortDirection = sdDescending then
    result := - result;
end;

procedure TListManager<T>.doListChange(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  updateStatus;
end;

procedure TListManager<T>.doAdd(mode : String);
var
  item, itemT, itemN : T;
  entry : TListItem;
  i, c : integer;
begin
  item := addItem(mode);
  if (item <> nil) then
  begin
    try
      // so, does this replace and existing item?
      itemN := nil;
      for itemT in FData do
      begin
        if (compare(item, itemT, -1) = 0) then
          itemN := itemT;
      end;

      if itemN = nil then
      begin
        entry := FList.items.add;
        entry.Data := pointer(item);
        entry.caption := getCellText(item, 0);
        for c := 1 to FList.columnCount - 1 do
          entry.subItems.add(getCellText(item, c));
        FData.add(item.link);
        FList.ItemIndex := FList.items.count - 1;
      end
      else
      begin
        i := FData.indexOf(itemn);
        entry := FList.items[i];
        entry.Data := pointer(item);
        entry.caption := getCellText(item, 0);
        entry.subItems.Clear;
        for c := 1 to FList.columnCount - 1 do
          entry.subItems.add(getCellText(item, c));
        FData[i] := item.link;
        FList.ItemIndex := i;
      end;
      updateStatus;
    finally
      item.Free;
    end;
  end;
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

function TListManager<T>.doLoad : boolean;
var
  f : T;
  item : T;
  entry : TListItem;
  i, c : integer;
begin
  f := Focus.link;
  try
    FData.Clear;
    result := LoadList;
    if (result) then
    begin
      FList.BeginUpdate;
      try
        FList.ItemIndex := -1;
        FList.items.clear;
        i := 0;
        for item in FData do
        begin
          entry := FList.items.add;
          entry.Data := pointer(item);
          entry.caption := getCellText(item, 0);
          for c := 1 to FList.columnCount - 1 do
            entry.subItems.add(getCellText(item, c));
          if (f <> nil) and (compare(f, item, -1) = 0) then
            FList.ItemIndex := i;
          inc(i);
        end;
      finally
        FList.EndUpdate;
      end;
    end
    else
      FList.items.clear;
    updateStatus;
  finally
    focus.free;
  end;
end;

function TListManager<T>.canSort: boolean;
begin
  result := false;
end;

function TListManager<T>.ShowLoadingProgress: boolean;
begin
  result := false;
end;

function TListManager<T>.getSummaryText(item: T): String;
begin
  result := getCellText(item, 0);
end;

function TListManager<T>.compare(left, right: T; col : integer): integer;
begin
  result := NativeUInt(left) - NativeUInt(right);
end;

function TListManager<T>.AddItem(mode : String): T;
begin
  result := nil;
end;

procedure TListManager<T>.DeleteItem(item: T);
begin
  raise Exception.create('Delete is not supported here');
end;

End.

