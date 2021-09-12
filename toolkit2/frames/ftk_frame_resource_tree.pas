unit ftk_frame_resource_tree;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls,
  fhir_objects, fhir_parser,
  ftk_frame_resource;

type
  TFrame = TResourceDesignerFrame;


  { TResourceTreeFrame }

  TResourceTreeFrame = class(TFrame)
  private
    FTree : TTreeView;
    FMemo : TMemo;
    procedure DoTreeClick(sender : TObject);
    procedure LoadObject(item : TTreeNode; obj : TFHIRObject);

  public
    procedure initialize; override;
    procedure bind; override;

  end;

implementation

{$R *.lfm}

procedure TResourceTreeFrame.initialize;
begin
  FMemo := TMemo.create(self);
  FMemo.parent := self;
  FMemo.align := alBottom;
  FMemo.ReadOnly := true;
  FMemo.Height := 300;

  FTree := TTreeView.create(self);
  FTree.parent := self;
  FTree.align := alClient;
  FTree.ReadOnly := true;
  FTree.OnClick := DoTreeClick;
end;

procedure TResourceTreeFrame.bind;
var
  root : TTreeNode;
begin
  FTree.items.Clear;
  root := FTree.Items.Add (nil, 'Resource '+Resource.fhirType);
  root.Data := Resource;
  loadObject(root, Resource);
end;

procedure TResourceTreeFrame.DoTreeClick(sender: TObject);
var
  loc : TFHIRObjectLocationData;
  c : TFHIRComposer;
begin
  if FTree.Selected <> nil then
  begin
    if (TObject(FTree.Selected.Data) is TFHIRObjectList) then
      loc := TFHIRObjectList(FTree.Selected.Data).LocationData
    else
      loc := TFhirObject(FTree.Selected.Data).LocationData;

    if loc.hasLocation2 then
      OnSelectSourceRange(self, loc.parseStart2.toPoint, loc.parseFinish2.toPoint)
    else
      OnSelectSourceRange(self, loc.parseStart.toPoint, loc.parseFinish.toPoint);
  end;

  //c := FFactory.makeComposer(nil, FFormat, THTTPLanguages.create('en'), TFHIROutputStyle.OutputStylePretty);
  //try
  //  FMemo.text := c.composeBase(o);
  //finally
  //  sync.free;
  //end;
end;

procedure TResourceTreeFrame.LoadObject(item : TTreeNode; obj : TFHIRObject);
var
  prop : TFHIRNamedValue;
  child : TTreeNode;
begin
  for prop in obj.getNamedChildren.forEnum do
  begin
    if prop.value.isPrimitive then
    begin
      child := FTree.items.AddChildObject(item, prop.Name +': '+prop.value.fhirType+' = '+prop.value.primitiveValue, prop.value);
    end
    else
    begin
      child := FTree.Items.AddChildObject(item, prop.Name +': '+prop.value.fhirType, prop.value);
      if prop.list <> nil then
        FTree.Items.AddChildObject(child, '(list)', prop.list);
      LoadObject(child, prop.value);
    end;
  end;

end;


end.

