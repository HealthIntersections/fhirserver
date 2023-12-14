unit ftk_frame_resource_tree;

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
  FMemo := TMemo.Create(self);
  FMemo.parent := self;
  FMemo.align := alBottom;
  FMemo.ReadOnly := true;
  FMemo.Height := 300;

  FTree := TTreeView.Create(self);
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

  //c := FFactory.makeComposer(nil, FFormat, nil, TFHIROutputStyle.OutputStylePretty);
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

