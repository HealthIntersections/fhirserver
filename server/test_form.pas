unit test_form;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, laz.VirtualTrees, fsl_base, fui_lcl_managers;

type
  { TTestNode }

  TTestNode = class (TFslTreeNode)
  private
    FChildren : TFslList<TTestNode>;
    FIndex: Integer;
    FName: String;
  protected
    function getChildCount : integer; override;
    function getChild(index : integer) : TFslTreeNode; override;
  public
    constructor Create; override;
    constructor Create(index : integer; name : String);
    destructor Destroy; override;
    function link : TTestNode; overload;

    property index : Integer read FIndex write FIndex;
    property Name : String read FName write FName;
    property Children : TFslList<TTestNode> read FChildren;

    function describe : String;
  end;

  TTestForm = class;

  { TTestTreeManager }

  TTestTreeManager = class (TVTreeManager<TTestNode>)
  private
    form : TTestForm;
  public
    function doubleClickEdit : boolean; override;
    function LoadData : boolean; override;
    function allowedOperations(item : TTestNode) : TNodeOperationSet; override;

    function getImageIndex(item : TTestNode) : integer; override;
    function getCellText(item : TTestNode) : String; override;
    function getCellColors(item : TTestNode; var fore, back : TColor) : boolean; override;
    function getSummaryText(item : TTestNode) : String; override;

    procedure changed; override;
    procedure focusItemChange(item : TTestNode); override;
    function addItem(parent : TTestNode; mode : String) : TTestNode; override;
    function editItem(item : TTestNode; mode : String) : boolean; override;
    function executeItem(item : TTestNode; mode : String) : boolean; override;
    function refreshItem(item : TTestNode) : boolean; override;
    function deleteItem(parent, item : TTestNode) : boolean; override;
  end;

  { TTestForm }

  TTestForm = class(TForm)
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    ImageList1: TImageList;
    LazVirtualStringTree1: TLazVirtualStringTree;
    mFocus: TMemo;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    tbAdd: TToolButton;
    tbEdit: TToolButton;
    tbDelete: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FManager : TTestTreeManager;
  public

  end;

var
  TestForm: TTestForm;

implementation

{$R *.lfm}

{ TTestForm }

procedure TTestForm.FormCreate(Sender: TObject);
begin
  FManager := TTestTreeManager.Create;
  FManager.registerControl(btnAdd, copAdd);
  FManager.registerControl(btnEdit, copEdit);
  FManager.registerControl(btnDelete, copDelete);
  FManager.registerControl(tbAdd, copAdd);
  FManager.registerControl(tbEdit, copEdit);
  FManager.registerControl(tbDelete, copDelete);
  FManager.registerMenuEntry('&Add', 15, copAdd);
  FManager.registerMenuEntry('&Edit', 16, copEdit);
  FManager.registerMenuEntry('E&xecute', 18, copExecute);
  FManager.registerMenuEntry('&Delete', 17, copDelete);
  FManager.Images := ImageList1;
  FManager.Tree := LazVirtualStringTree1;
  FManager.form := self;
  FManager.doLoad;
end;

procedure TTestForm.FormDestroy(Sender: TObject);
begin
  FManager.free;
end;

{ TTestTreeManager }

function TTestTreeManager.doubleClickEdit: boolean;
begin
  Result := false;
end;

function TTestTreeManager.LoadData: boolean;
var
  i, j : integer;
  n : TTestNode;
begin
  result := true;
  for i := 0 to Random(10) do
  begin
    n := TTestNode.Create(Random(Images.count-3), 'Root.'+inttostr(i));
    Data.add(n);
    for j := 0 to Random(5) do
      n.Children.add(TTestNode.Create(Random(Images.count-3), 'Root.'+inttostr(i)+'-'+inttostr(j)));
  end;
end;

function TTestTreeManager.allowedOperations(item: TTestNode): TNodeOperationSet;
begin
  if (item.Name.Contains('-')) then
    result := [opEdit, opExecute, opRefresh, opDelete]
  else
    result := [opAdd, opExecute, opRefresh, opDelete];
end;

function TTestTreeManager.getImageIndex(item: TTestNode): integer;
begin
  Result := item.index;
end;

function TTestTreeManager.getCellText(item: TTestNode): String;
begin
  result := item.Name;
end;

function TTestTreeManager.getCellColors(item: TTestNode; var fore, back: TColor): boolean;
begin
  if item.Index > images.count div 2 then
  begin
    result := true;
    fore := clMaroon;
    back := clWhite;
  end
  else
    Result := inherited getCellColors(item, fore, back);
end;

function TTestTreeManager.getSummaryText(item: TTestNode): String;
begin
  Result := item.name;
end;

procedure TTestTreeManager.changed;
begin
  // do nothing here, bu we could save the tree
end;

procedure TTestTreeManager.focusItemChange(item : TTestNode);
begin
  form.mFocus.text := item.describe;
end;

function TTestTreeManager.addItem(parent: TTestNode; mode: String): TTestNode;
var
  n : String;
begin
  result := nil;
  n := 'Node.n-n';
  if InputQuery('Add Test Node', 'Enter Name:', n) then
  begin
    result := TTestNode.Create(Random(Images.count-3), n);
    parent.Children.add(result);
  end;
end;

function TTestTreeManager.executeItem(item: TTestNode; mode: String): boolean;
begin
  ShowMessage('Execute '+item.name);
end;

function TTestTreeManager.refreshItem(item: TTestNode): boolean;
begin
  ShowMessage('Reload '+item.name);
end;

function TTestTreeManager.deleteItem(parent, item: TTestNode): boolean;
begin
  parent.Children.Remove(item);
end;

function TTestTreeManager.editItem(item : TTestNode; mode : String) : boolean;
var
  n : String;
begin
  n := item.name;
  result :=  InputQuery('Test Edit', 'Enter Name:', n);
  if result then
    item.name := n;
end;

{ TTestNode }

constructor TTestNode.Create;
begin
  inherited Create;
  FChildren := TFslList<TTestNode>.Create;
end;

constructor TTestNode.Create(index: integer; name: String);
begin
  Create;
  self.Index := index;
  self.Name := Name;
end;

destructor TTestNode.Destroy;
begin
  FChildren.free;
  inherited Destroy;
end;

function TTestNode.link: TTestNode;
begin
  result := TTestNode(inherited link);
end;

function TTestNode.describe: String;
begin
  result :=
    'Name: '+name+#13#10+
    'Image: '+inttostr(index)+#13#10+
    'Children: '+inttostr(FChildren.count)+#13#10;
end;

function TTestNode.getChildCount: integer;
begin
  result := FChildren.count;
end;

function TTestNode.getChild(index: integer): TFslTreeNode;
begin
  result := FChildren[index];
end;

end.

