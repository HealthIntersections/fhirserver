unit VirtualStringTreeComboBox;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

Uses
  Windows, SysUtils, Types, Classes, Graphics, Messages, Controls, StdCtrls, Forms,
  VirtualTrees;

Type
  TGetListEvent = procedure (context : String; list : TStrings) of object;

  TVirtualStringTreeComboBox = class (TInterfacedObject, IVTEditLink)
  private
    FCombo: TCombobox;        // One of the property editor classes.
    FTree: TVirtualStringTree; // A back reference to the tree calling.
    FNode: PVirtualNode;       // The node being edited.
    FColumn: Integer;          // The column of the node being edited.
    FText : String;
    FOnlyList : Boolean;
    FGetList : TGetListEvent;
    FContext : String;
  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    Constructor Create(text : String; onlyList : boolean; OnGetList : TGetListEvent; context : string);
    destructor Destroy; override;

    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

  // this is defined to support autoedit - capture the first typed character
  TVirtualStringTreeEdit = class (TInterfacedObject, IVTEditLink)
  private
    FEdit : TEdit;        // One of the property editor classes.
    FTree: TVirtualStringTree; // A back reference to the tree calling.
    FNode: PVirtualNode;       // The node being edited.
    FColumn: Integer;          // The column of the node being edited.
    FText : String;
    FNoSelect : boolean;
  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    Constructor Create(text : String; NoSelect : boolean);
    destructor Destroy; override;

    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

implementation

{ TVirtualStringTreeComboBox }

constructor TVirtualStringTreeComboBox.Create(text : String; onlyList: boolean; OnGetList: TGetListEvent; context: string);
begin
  inherited Create;
  FText := text;
  FOnlyList := onlyList;
  FGetList := OnGetList;
  FContext := context;
end;

destructor TVirtualStringTreeComboBox.Destroy;
begin
  //FEdit.Free; casues issue #357. Fix:
  if FCombo.HandleAllocated then
    PostMessage(FCombo.Handle, CM_RELEASE, 0, 0);
  inherited;
end;

function TVirtualStringTreeComboBox.BeginEdit: Boolean;

begin
  Result := True;
  FCombo.Show;
  FCombo.SetFocus;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualStringTreeComboBox.CancelEdit: Boolean;
begin
  Result := True;
  FCombo.Hide;
end;

function TVirtualStringTreeComboBox.EndEdit: Boolean;
begin
  Result := True;
  if FCombo.Text <> FText then
  begin
    FTree.OnNewText(FTree, FNode, FColumn, FCombo.Text);
    FTree.InvalidateNode(FNode);
  end;
  FCombo.Hide;
  FTree.SetFocus;
end;


function TVirtualStringTreeComboBox.GetBounds: TRect;
begin
  Result := FCombo.BoundsRect;
end;

function TVirtualStringTreeComboBox.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Result := True;
  FTree := Tree as TVirtualStringTree;
  FNode := Node;
  FColumn := Column;

  // determine what edit type actually is needed
  FCombo.Free;
  FCombo := nil;
  FCombo := TComboBox.Create(nil);
  FCombo.Visible := False;
  FCombo.Parent := Tree;
  if FOnlyList then
    FCombo.Style := csDropDownList
  else
    FCombo.Style := csDropDown;
  FGetList(FContext, FCombo.Items);
  if FCombo.Items.IndexOf(FText) = -1 then
    FCombo.Items.Insert(0, FText);
  FCombo.Text := FText;
  FCombo.OnKeyDown := EditKeyDown;
  FCombo.OnKeyUp := EditKeyUp;
end;

procedure TVirtualStringTreeComboBox.ProcessMessage(var Message: TMessage);

begin
  FCombo.WindowProc(Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualStringTreeComboBox.SetBounds(R: TRect);

var
  Dummy: Integer;

begin
  // Since we don't want to activate grid extensions in the tree (this would influence how the selection is drawn)
  // we have to set the edit's width explicitly to the width of the column.
  FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, R.Right);
  FCombo.BoundsRect := R;
end;

procedure TVirtualStringTreeComboBox.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

var
  CanAdvance: Boolean;

begin
  CanAdvance := true;

  case Key of
    VK_ESCAPE:
      begin
        Key := 0;//ESC will be handled in EditKeyUp()
      end;
    VK_RETURN:
      if CanAdvance then
      begin
        FTree.EndEditNode;
        Key := 0;
      end;

    VK_UP,
    VK_DOWN:
      begin
        // Consider special cases before finishing edit mode.
        CanAdvance := Shift = [];
        CanAdvance := CanAdvance and not FCombo.DroppedDown;
        if CanAdvance then
        begin
          // Forward the keypress to the tree. It will asynchronously change the focused node.
          PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
          Key := 0;
        end;
      end;
  end;
end;

procedure TVirtualStringTreeComboBox.EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        FTree.CancelEditNode;
        Key := 0;
      end;//VK_ESCAPE
  end;//case
end;

{ TVirtualStringTreeEdit }

constructor TVirtualStringTreeEdit.Create(text : String; NoSelect : boolean);
begin
  inherited Create;
  FText := text;
  FNoSelect := NoSelect;
end;

destructor TVirtualStringTreeEdit.Destroy;
begin
  //FEdit.Free; casues issue #357. Fix:
  if FEdit.HandleAllocated then
    PostMessage(FEdit.Handle, CM_RELEASE, 0, 0);
  inherited;
end;

function TVirtualStringTreeEdit.BeginEdit: Boolean;
begin
  Result := True;
  FEdit.Show;
  FEdit.SetFocus;
  if FNoSelect then
    FEdit.SelStart := length(FEdit.Text);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualStringTreeEdit.CancelEdit: Boolean;
begin
  Result := True;
  FEdit.Hide;
end;

function TVirtualStringTreeEdit.EndEdit: Boolean;
begin
  Result := True;
  if FEdit.Text <> FText then
  begin
    FTree.OnNewText(FTree, FNode, FColumn, FEdit.Text);
    FTree.InvalidateNode(FNode);
  end;
  FEdit.Hide;
  FTree.SetFocus;
end;


function TVirtualStringTreeEdit.GetBounds: TRect;
begin
  Result := FEdit.BoundsRect;
end;

function TVirtualStringTreeEdit.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
begin
  Result := True;
  FTree := Tree as TVirtualStringTree;
  FNode := Node;
  FColumn := Column;

  // determine what edit type actually is needed
  FEdit.Free;
  FEdit := nil;
  FEdit := TEdit.Create(nil);
  FEdit.Visible := False;
  FEdit.Parent := Tree;
  FEdit.Text := FText;
  FEdit.OnKeyDown := EditKeyDown;
  FEdit.OnKeyUp := EditKeyUp;
end;

procedure TVirtualStringTreeEdit.ProcessMessage(var Message: TMessage);
begin
  FEdit.WindowProc(Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualStringTreeEdit.SetBounds(R: TRect);

var
  Dummy: Integer;

begin
  // Since we don't want to activate grid extensions in the tree (this would influence how the selection is drawn)
  // we have to set the edit's width explicitly to the width of the column.
  FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, R.Right);
  FEdit.BoundsRect := R;
end;

procedure TVirtualStringTreeEdit.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

var
  CanAdvance: Boolean;

begin
  CanAdvance := true;

  case Key of
    VK_ESCAPE:
      begin
        Key := 0;//ESC will be handled in EditKeyUp()
      end;
    VK_RETURN:
      if CanAdvance then
      begin
        FTree.EndEditNode;
        Key := 0;
      end;

    VK_UP,
    VK_DOWN:
      begin
        // Consider special cases before finishing edit mode.
        CanAdvance := Shift = [];
        //CanAdvance := CanAdvance and not FCombo.DroppedDown;
        if CanAdvance then
        begin
          // Forward the keypress to the tree. It will asynchronously change the focused node.
          PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
          Key := 0;
        end;
      end;
  end;
end;

procedure TVirtualStringTreeEdit.EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        FTree.CancelEditNode;
        Key := 0;
      end;//VK_ESCAPE
  end;//case
end;

end.
