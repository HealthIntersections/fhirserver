unit QuestionnairePanel;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

uses
  Sysutils, Classes, Generics.Collections, UITypes, Types,
  FMX.Controls, FMX.StdCtrls, FMX.ImgList, FMX.Layouts, FMX.Objects,
  FMX.Dialogs, FMX.Graphics, FMX.DialogService, FHIR.Ui.Fmx,
  fhir_objects, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Utilities,
  ToolKitSettings, BaseFrame,
  QuestionnaireItemPanel, QuestionnaireItemDialog;

Const
  PANEL_HEIGHT = 40;

Type
  TQuestionnairePanel = class (TVertScrollBox)
  private
    FQuestionnaire: TFhirQuestionnaire;
    FPanels : TList<TQuestionnaireItemPanel>;
    FFocused : TQuestionnaireItemPanel;
    FImageList: TImageList;
    FLines : TList<TLine>;
    FSettings: TFHIRToolkitSettings;
    FOnWork: TWorkEvent;
    procedure processList(list : TFhirQuestionnaireItemList; parent : TQuestionnaireItemPanel; level : integer; var top : integer);
    procedure doLayout(list : TFhirQuestionnaireItemList; parent : TQuestionnaireItemPanel; level : integer; var top : integer);
    function drawLines(list : TFhirQuestionnaireItemList; parent : TQuestionnaireItemPanel; pleft : Single) : Single;
    procedure clear;
    procedure clearLines;
    procedure SetQuestionnaire(const Value: TFhirQuestionnaire);
    procedure SetSettings(const Value: TFHIRToolkitSettings);
  protected
    procedure Resize; override;
  public
    constructor Create(owner : TComponent); override;
    destructor Destroy; override;

    procedure build;
    procedure layout;

    procedure moveUp(panel : TQuestionnaireItemPanel);
    procedure moveDown(panel : TQuestionnaireItemPanel);
    procedure moveIn(panel : TQuestionnaireItemPanel);
    procedure moveOut(panel : TQuestionnaireItemPanel);
    procedure delete(panel : TQuestionnaireItemPanel);
    procedure addItem(panel : TQuestionnaireItemPanel);

    procedure setFocusedPanel(panel : TQuestionnaireItemPanel);
    procedure markChange(item : TFhirQuestionnaireItem);
    property Questionnaire : TFhirQuestionnaire read FQuestionnaire write SetQuestionnaire;
    property ImageList: TImageList read FImageList write FImageList;
    property Settings : TFHIRToolkitSettings read FSettings write SetSettings;
    property OnWork : TWorkEvent read FOnWork write FOnWork;
  end;

implementation

uses
  QuestionnaireEditor;

{ TQuestionnairePanel }

procedure TQuestionnairePanel.addItem(panel: TQuestionnaireItemPanel);
var
  item, new, next, prev : TFhirQuestionnaireItem;
  index : integer;
  list : TFhirQuestionnaireItemList;
  npanel: TQuestionnaireItemPanel;
  form : TQuestionnaireItemForm;
begin
  if panel = nil then
  begin
    list := Questionnaire.itemList ;
    index := Questionnaire.itemList.Count - 1;
  end
  else
  begin
    item := panel.Item;
    if panel.ParentItem <> nil then
      list := panel.ParentItem.itemList
    else
      list := Questionnaire.itemList;
    index := list.IndexByReference(item);
  end;
  new := TFhirQuestionnaireItem.Create;
  try
    new.linkId := 'item'+inttostr(FPanels.Count+1);
    new.text := 'new item';
    form := TQuestionnaireItemForm.Create(self);
    try
      form.item := new.clone;
      form.Settings := Settings.link;
      form.questionnaire := questionnaire.Link;
      form.OnWork := OnWork;
      if panel <> nil then
      begin
        form.btnAsChild.Visible := true;
        form.btnOk.Text := 'Add Sibling';
      end;
      if ShowModalHack(form) <> mrCancel then
      begin
        new.Assign(form.item);
        if form.ModalResult = mrOK then
        begin
          if index = list.Count - 1 then
            next := nil
          else
            next := list[index+1];
          if index < 0 then
            prev := nil
          else
            prev := list[index];
        end
        else
        begin
          next := nil;
          list := item.itemList;
          index := list.Count-1;
          if list.Count = 0 then
            prev := nil
          else
            prev := list[index];
        end;
        list.InsertItem(index+1, new.Link);
        npanel := TQuestionnaireItemPanel.Create(self);
        npanel.Parent := self;
        npanel.position.Y := top;
        npanel.position.X := 0;
        npanel.Width := Width-26;
        npanel.Height := PANEL_HEIGHT;
        npanel.Item := new.Link;
        if panel = nil then
        begin
          npanel.Level := 0;
          npanel.ParentItem := nil;
        end
        else
        begin
          npanel.Level := panel.level;
          npanel.ParentItem := panel.parentItem.link;
        end;
        npanel.imagelist := imageList;
        npanel.Previous := prev.Link;
        npanel.Next := next.Link;
        npanel.questionnaire := Questionnaire.Link;
        npanel.Settings := Settings.link;
        npanel.OnWork := OnWork;
        npanel.build;
        new.TagObject := npanel;
        FPanels.Add(npanel);
      end;
      layout;
      npanel.grabFocus;
      markChange(item);
    finally
      form.Free;
    end;
  finally
    new.Free;
  end;
end;

procedure TQuestionnairePanel.build;
var
  top : integer;
begin
  clear;
  top := 0;
  processList(questionnaire.itemList, nil, 0, top);
  clearLines;
  drawLines(questionnaire.itemList, nil, 0);
end;

procedure TQuestionnairePanel.clear;
var
  panel : TQuestionnaireItemPanel;
begin
  for panel in FPanels do
  begin
    RemoveObject(panel);
    panel.Free;
  end;
  FPanels.Clear;
  FFocused := nil;
end;

procedure TQuestionnairePanel.clearLines;
var
  line : TLine;
begin
  for line in FLines do
    RemoveObject(line);
  FLines.Clear;
end;

constructor TQuestionnairePanel.Create(owner: TComponent);
begin
  inherited;
  FPanels := TList<TQuestionnaireItemPanel>.create;
  FLines := TList<TLine>.create;
end;

procedure TQuestionnairePanel.delete(panel: TQuestionnaireItemPanel);
var
  nf, parent : TFhirQuestionnaireItem;
  index, count : integer;
  s : String;
  item : TFhirQuestionnaireItem;
  list : TFhirQuestionnaireItemList;
begin
  item := panel.Item;
  if panel.ParentItem <> nil then
    list := panel.ParentItem.itemList
  else
    list := Questionnaire.itemList;
  index := list.IndexByReference(item);

  count := item.countDescendents;
  if count = 0 then
    s := 'Delete Item '+item.linkId+'?'
  else if count = 1 then
    s := 'Delete Item '+item.linkId+' and 1 child?'
  else
    s := 'Delete Item '+item.linkId+' and it''s '+inttostr(count)+' children?';

  TDialogService.MessageDialog(s, TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0,
    procedure (const AResult: TModalResult)
    var p : TFhirQuestionnaireItem;
    begin
      if AResult = mrYes then
      begin
        if index > 0 then
          nf := list[index - 1]
        else if list.Count > 1 then
          nf := list[index + 1]
        else
          nf := nil;
        markChange(item);
        RemoveObject(TQuestionnaireItemPanel(item.TagObject));
        list.Remove(index);
        layout;
      end;
    end
  );
end;

destructor TQuestionnairePanel.Destroy;
begin
  FLines.Free;
  FPanels.Free;
  FQuestionnaire.Free;
  FSettings.Free;
  inherited;
end;

procedure TQuestionnairePanel.doLayout(list: TFhirQuestionnaireItemList; parent : TQuestionnaireItemPanel; level: integer; var top: integer);
var
  focus, next, last : TFhirQuestionnaireItem;
  i : integer;
  panel : TQuestionnaireItemPanel;
begin
  for i := 0 to list.Count - 1 do
  begin
    if i = 0 then
      last := nil
    else
      last := list[i-1];
    focus := list[i];
    if i = list.Count - 1 then
      next := nil
    else
      next := list[i+1];
    panel := TQuestionnaireItemPanel(focus.tagObject);
    panel.position.Y := top;
    inc(top, PANEL_HEIGHT);
    panel.Previous := last.Link;
    panel.Next := next.link;
    panel.Level := level;
    if parent <> nil then
      panel.ParentItem := parent.Item.link
    else
      panel.ParentItem := nil;
    panel.updateLayout;
    if focus.itemList.Count > 0 then
      doLayout(focus.itemList, panel, level+1, top);
  end;
end;

function TQuestionnairePanel.drawLines(list : TFhirQuestionnaireItemList; parent : TQuestionnaireItemPanel; pleft : Single) : Single;
var
  focus : TFhirQuestionnaireItem;
  i : integer;
  t : Single;
  panel : TQuestionnaireItemPanel;
  line : TLine;
begin
  result := 0;
  for i := 0 to list.Count - 1 do
  begin
    focus := list[i];
    panel := TQuestionnaireItemPanel(focus.TagObject);
    result := panel.position.Y;
    if parent <> nil then
    begin
      line := TLine.Create(self);
      line.Position.X := pleft;
      line.Position.Y := panel.position.Y + PANEL_HEIGHT / 2;
      line.LineType := TLineType.Top;
      line.Width := panel.lineStartX - pleft;
      line.Height := 10;
      line.Stroke.Dash := TStrokeDash.Dot;
      line.Stroke.Thickness := 1.0;
      line.Stroke.Color := TAlphaColorRec.Darkgray;
      line.Parent := self;
      FLines.Add(line);
    end;
    if focus.itemList.Count > 0 then
    begin
      t := drawLines(focus.itemList, panel, panel.lineStartX);
      line := TLine.Create(self);
      line.Position.X := panel.lineStartX;
      line.Position.Y := panel.position.Y + panel.lineStartY;
      line.LineType := TLineType.Left;
      line.Width := 10;
      line.Height := t - panel.position.Y - panel.lineStartY + PANEL_HEIGHT / 2;
      line.Stroke.Dash := TStrokeDash.Dot;
      line.Stroke.Thickness := 1.0;
      line.Stroke.Color := TAlphaColorRec.Darkgray;
      line.Parent := self;
      FLines.Add(line);
    end;
  end;
end;

procedure TQuestionnairePanel.layout;
var
  top : integer;
begin
  top := 0;
  doLayout(questionnaire.itemList, nil, 0, top);
  clearLines;
  drawLines(questionnaire.itemList, nil, 0);
end;

procedure TQuestionnairePanel.markChange(item: TFhirQuestionnaireItem);
begin
  TQuestionnaireEditorFrame(owner).ResourceIsDirty := true;
end;

procedure TQuestionnairePanel.moveDown(panel: TQuestionnaireItemPanel);
var
  item : TFhirQuestionnaireItem;
  list : TFhirQuestionnaireItemList;
  vp : TPointF;
  index : integer;
begin
  // temporary: remember the focus option
  item := panel.Item;
  if panel.ParentItem <> nil then
    list := panel.ParentItem.itemList
  else
    list := Questionnaire.itemList;
  index := list.IndexByReference(item);
  list.Exchange(index, index+1);
  // update layout
  layout;
  // get new panel
  panel := item.TagObject as TQuestionnaireItemPanel;
  // make sure it's in view
  vp := ViewportPosition;
  if panel.Position.Y < vp.Y then
  begin
    vp.Y := panel.Position.Y;
    ViewportPosition := vp;
  end
  else if panel.Position.Y + panel.Height > vp.Y + Height then
  begin
    vp.Y := panel.Position.Y - panel.Height;
    ViewportPosition := vp;
  end;
  // make sure it has focus
  panel.grabFocus;
  markChange(item);
end;

procedure TQuestionnairePanel.moveIn(panel: TQuestionnaireItemPanel);
var
  item, pItem : TFhirQuestionnaireItem;
  list, glist : TFhirQuestionnaireItemList;
  vp : TPointF;
  index, pIndex : integer;
  pPanel : TQuestionnaireItemPanel;
begin
  // temporary: remember the focus option
  item := panel.Item;
  pItem := panel.ParentItem;
  list := pItem.itemList;
  index := list.IndexByReference(item);
  pPanel := TQuestionnaireItemPanel(pItem.TagObject);
  if pPanel.ParentItem <> nil then
    gList := pPanel.ParentItem.itemList
  else
    gList := Questionnaire.itemList;
  pIndex := gList.IndexByReference(pItem);
  glist.InsertItem(pIndex+1, item.Link);
  list.DeleteByIndex(index);
  // update layout
  layout;
  // make sure it's in view
  vp := ViewportPosition;
  if panel.Position.Y < vp.Y then
  begin
    vp.Y := panel.Position.Y;
    ViewportPosition := vp;
  end
  else if panel.Position.Y + panel.Height > vp.Y + Height then
  begin
    vp.Y := panel.Position.Y - panel.Height;
    ViewportPosition := vp;
  end;
  // make sure it has focus
  panel.grabFocus;
  markChange(item);
end;


procedure TQuestionnairePanel.moveOut(panel: TQuestionnaireItemPanel);
var
  item : TFhirQuestionnaireItem;
  list : TFhirQuestionnaireItemList;
  vp : TPointF;
  index : integer;
begin
  // temporary: remember the focus option
  item := panel.Item;
  if panel.ParentItem <> nil then
    list := panel.ParentItem.itemList
  else
    list := Questionnaire.itemList;
  index := list.IndexByReference(item);
  list[index-1].itemList.Add(item.Link);
  list.Remove(index);
  // update layout
  layout;
  // make sure it's in view
  vp := ViewportPosition;
  if panel.Position.Y < vp.Y then
  begin
    vp.Y := panel.Position.Y;
    ViewportPosition := vp;
  end
  else if panel.Position.Y + panel.Height > vp.Y + Height then
  begin
    vp.Y := panel.Position.Y - panel.Height;
    ViewportPosition := vp;
  end;
  // make sure it has focus
  panel.grabFocus;
  markChange(item);
end;

procedure TQuestionnairePanel.moveUp(panel: TQuestionnaireItemPanel);
var
  item : TFhirQuestionnaireItem;
  list : TFhirQuestionnaireItemList;
  vp : TPointF;
  index : integer;
begin
  // temporary: remember the focus option
  item := panel.Item;
  if panel.ParentItem <> nil then
    list := panel.ParentItem.itemList
  else
    list := Questionnaire.itemList;
  index := list.IndexByReference(item);
  list.Exchange(index, index-1);
  // update layout
  layout;
  // get new panel
  panel := item.TagObject as TQuestionnaireItemPanel;
  // make sure it's in view
  vp := ViewportPosition;
  if panel.Position.Y < vp.Y then
  begin
    vp.Y := panel.Position.Y;
    ViewportPosition := vp;
  end
  else if panel.Position.Y + panel.Height > vp.Y + Height then
  begin
    vp.Y := panel.Position.Y - panel.Height;
    ViewportPosition := vp;
  end;
  // make sure it has focus
  panel.grabFocus;
  markChange(item);
end;

procedure TQuestionnairePanel.SetQuestionnaire(const Value: TFhirQuestionnaire);
begin
  FQuestionnaire.Free;
  FQuestionnaire := Value;
end;

procedure TQuestionnairePanel.SetSettings(const Value: TFHIRToolkitSettings);
begin
  FSettings := Value;
end;

procedure TQuestionnairePanel.processList(list: TFhirQuestionnaireItemList; parent: TQuestionnaireItemPanel; level : integer; var top : integer);
var
  focus, next, last : TFhirQuestionnaireItem;
  i : integer;
  panel : TQuestionnaireItemPanel;
begin
  for i := 0 to list.Count - 1 do
  begin
    if i = 0 then
      last := nil
    else
      last := list[i-1];
    focus := list[i];
    if i = list.Count - 1 then
      next := nil
    else
      next := list[i+1];
    panel := TQuestionnaireItemPanel.Create(self);
    panel.Parent := self;
    panel.position.Y := top;
    panel.position.X := 0;
    panel.Width := Width-26;
    panel.Height := PANEL_HEIGHT;
    inc(top, PANEL_HEIGHT);
    panel.Item := focus.Link;
    panel.Level := level;
    panel.imagelist := imageList;
    panel.Settings := Settings.link;
    panel.questionnaire := Questionnaire.Link;
    panel.OnWork := OnWork;
    panel.Previous := last.Link;
    panel.Next := next.Link;
    if parent <> nil then
      panel.ParentItem := parent.Item.link;
    panel.build;
    focus.TagObject := panel;
    FPanels.Add(panel);
    if focus.itemList.Count > 0 then
      processList(focus.itemList, panel, level+1, top);
  end;
end;

procedure TQuestionnairePanel.Resize;
var
  panel : TQuestionnaireItemPanel;
begin
  inherited;
  for panel in FPanels do
    panel.Width := width-26;
end;

procedure TQuestionnairePanel.setFocusedPanel(panel: TQuestionnaireItemPanel);
begin
  if panel = FFocused then
    exit;
  if (FFocused <> nil) then
    FFocused.loseFocus;
  FFocused := panel;
end;

end.
