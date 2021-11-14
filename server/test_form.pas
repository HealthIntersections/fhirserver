unit test_form;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, laz.VirtualTrees,
  fsl_base,
  fui_lcl_managers;

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
  end;

  { TTestTreeManager }

  TTestTreeManager = class (TVTreeManager<TTestNode>)
  private
  public
    function doubleClickEdit : boolean; override;
    function LoadData : boolean; override;
    function allowedOperations(item : TTestNode) : TNodeOperationSet; override;

    function getImageIndex(item : TTestNode) : integer; override;
    function getCellText(item : TTestNode) : String; override;
    function getCellColors(item : TTestNode; var fore, back : TColor) : boolean; override;
    function getSummaryText(item : TTestNode) : String; override;

    procedure changed; override;
    function executeItem(item : TTestNode; mode : String) : boolean; override;
    function refreshItem(item : TTestNode) : boolean; override;
  end;

  { TTestForm }

  TTestForm = class(TForm)
    ImageList1: TImageList;
    LazVirtualStringTree1: TLazVirtualStringTree;
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
  FManager := TTestTreeManager.create;
  FManager.Tree := LazVirtualStringTree1;
  FManager.Images := ImageList1;
  FManager.doLoad;
end;

procedure TTestForm.FormDestroy(Sender: TObject);
begin
  FManager.Free;
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
    n := TTestNode.create(Random(Images.count), 'Root.'+inttostr(i));
    Data.add(n);
    for j := 0 to Random(5) do
      n.Children.add(TTestNode.create(Random(Images.count), 'Root.'+inttostr(i)+'-'+inttostr(j)));
  end;
end;

function TTestTreeManager.allowedOperations(item: TTestNode): TNodeOperationSet;
begin
  result := [opExecute, opRefresh];
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
  if item.Index > 2 then
  begin
    result := true;
    fore := clWhite;
    back := clBlack;
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
  // do nothing here
end;

function TTestTreeManager.executeItem(item: TTestNode; mode: String): boolean;
begin
  ShowMessage('Execute '+item.name);
end;

function TTestTreeManager.refreshItem(item: TTestNode): boolean;
begin
  ShowMessage('Reload '+item.name);
end;

{ TTestNode }

constructor TTestNode.Create;
begin
  inherited Create;
  FChildren := TFslList<TTestNode>.create;
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

function TTestNode.getChildCount: integer;
begin
  result := FChildren.count;
end;

function TTestNode.getChild(index: integer): TFslTreeNode;
begin
  result := FChildren[index];
end;

end.

