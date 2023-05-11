unit QuestionnaireItemPanel;

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
  Sysutils, Classes, FMX.Graphics, UITypes,
  FMX.Controls, FMX.StdCtrls, FMX.Types, FMX.Objects, FMX.ImgList, FMX.Edit, FMX.DateTimeCtrls, FMX.ListBox, FMX.ComboEdit,
  FHIR.Ui.Fmx,
  fhir_objects, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Utilities,
  ToolkitSettings, BaseFrame,
  QuestionnaireItemDialog;

Type
  TQuestionnaireItemPanel = class (TPanel)
  private
    FBase : TRectangle;
    FLabel : TLabel;
    FInputControl1 : TControl;
    FInputControl2 : TControl;
    FEditButton : TButton;
    FUp : TButton;
    FDown : TButton;
    FIn : TButton;
    FOut : TButton;
    FAddItem : TButton;
    FDelete : TButton;
    FTextEdit : TEdit;

    FLevel: integer;

    FParent: TFhirQuestionnaireItem;
    FItem: TFhirQuestionnaireItem;
    FPrevious: TFhirQuestionnaireItem;
    FNext: TFhirQuestionnaireItem;

    FHasFocus : boolean;
    FImageList: TImageList;
    FSettings: TFHIRToolkitSettings;
    FOnWork: TWorkEvent;
    FQuestionnaire: TFhirQuestionnaire;

    procedure SetItem(const Value: TFhirQuestionnaireItem);
    procedure SetNext(const Value: TFhirQuestionnaireItem);
    procedure SetParentItem(const Value: TFhirQuestionnaireItem);
    procedure SetPrevious(const Value: TFhirQuestionnaireItem);

    procedure InClick(sender : TObject);
    procedure OutClick(sender : TObject);
    procedure UpClick(sender : TObject);
    procedure DownClick(sender : TObject);
    procedure DeleteClick(sender : TObject);
    procedure AddItemClick(sender : TObject);
    procedure SelfClick(sender : TObject);
    procedure EditClick(sender : TObject);
    procedure SetSettings(const Value: TFHIRToolkitSettings);
    procedure privGrabFocus;
    procedure LabelDlbClick(sender : TObject);
    procedure TextEditKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure makeLabelText;
    procedure SetQuestionnaire(const Value: TFhirQuestionnaire);
  protected
    procedure Resize; override;
  public
    constructor Create(owner : TComponent); override;
    destructor Destroy; override;

    procedure grabFocus;
    procedure loseFocus;

    procedure clear;
    procedure build;
    procedure updateLayout;

    function lineStartX : Single;
    function lineStartY : Single;

    property ImageList: TImageList read FImageList write FImageList;
    property Item : TFhirQuestionnaireItem read FItem write SetItem;
    property Previous : TFhirQuestionnaireItem read FPrevious write SetPrevious;
    property Next : TFhirQuestionnaireItem read FNext write SetNext;
    property ParentItem : TFhirQuestionnaireItem read FParent write SetParentItem;
    property Level : integer read FLevel write FLevel;
    property Settings : TFHIRToolkitSettings read FSettings write SetSettings;
    property questionnaire : TFhirQuestionnaire read FQuestionnaire write SetQuestionnaire;
    property OnWork : TWorkEvent read FOnWork write FOnWork;
  end;

implementation

uses
  QuestionnairePanel;

{ TQuestionnaireItemPanel }

procedure TQuestionnaireItemPanel.AddItemClick(sender: TObject);
begin
  TQuestionnairePanel(Owner).addItem(self);
end;

procedure TQuestionnaireItemPanel.makeLabelText;
begin
  if item.prefix <> '' then
    FLabel.Text := item.prefix+' '+Item.text
  else
    FLabel.Text := Item.text;
  if FLabel.Text = '' then
  begin
    FLabel.Text := '('+item.linkId+')';
    FLabel.TextSettings.Font.Style := [TFontStyle.fsItalic];
  end;
end;

procedure TQuestionnaireItemPanel.build;
var
  opt : TFhirQuestionnaireItemOption;
begin
  clear;
  HitTest := true;
  OnClick := SelfClick;

  FBase := TRectangle.Create(self);
  FBase.Parent := self;
  FBase.Position.Y := 0;
  FBase.Position.X := 0;
  FBase.Height := Height;
  FBase.Width := Width;
  FBase.Fill.Color := TAlphaColorRec.White;
  FBase.Fill.Kind := TBrushKind.Solid;
  FBase.Stroke.Thickness := 0.0;
  FBase.Stroke.Dash := TStrokeDash.Dot;
  FBase.Stroke.Color := TAlphaColorRec.Lightgray;
//  FBase.Sides := [TSide.Bottom];
  FBase.Opacity := 1;
  FBase.OnClick := SelfClick;
  FBase.OnDblClick := EditClick;

  FLabel := TLabel.Create(self);
  FLabel.Parent := FBase;
  FLabel.Position.Y := 0;
  FLabel.Position.X := 60 + Level * 10;
  FLabel.Height := Height;
  FLabel.StyledSettings := [TStyledSetting.Family, TStyledSetting.Size, TStyledSetting.FontColor];
  FLabel.VertTextAlign := TTextAlign.Center;
  makeLabelText;
  FLabel.Width := FLabel.Canvas.TextWidth(FLabel.Text);
  FLabel.HitTest := true;
  FLabel.OnClick := SelfClick;
  FLabel.OnDblClick := LabelDlbClick;

  //creating the control....
  if item.optionList.Count > 0 then
  begin
    FInputControl1 := TComboBox.Create(self);
    for opt in item.optionList do
      TComboBox(FInputControl1).Items.Add(opt.value.primitiveValue);
  end
  else case Item.type_ of
    ItemTypeNull: ;
    ItemTypeGroup: ;
    ItemTypeDisplay: ;
    ItemTypeBoolean:
      begin
      FInputControl1 := TCheckBox.Create(self);
      TCheckBox(FInputControl1).Text := 'True';
      end;
    ItemTypeDecimal: FInputControl1 := TEdit.Create(self);
    ItemTypeInteger: FInputControl1 := TEdit.Create(self);
    ItemTypeDate: FInputControl1 := TDateEdit.Create(self);
    ItemTypeDateTime:
      begin
      FInputControl1 := TDateEdit.Create(self);
      FInputControl2 := TTimeEdit.Create(self);
      end;
    ItemTypeTime: FInputControl1 := TTimeEdit.Create(self);
    ItemTypeString: FInputControl1 := TEdit.Create(self);
    ItemTypeText: FInputControl1 := TEdit.Create(self);
    ItemTypeUrl: FInputControl1 := TEdit.Create(self);
    ItemTypeChoice: FInputControl1 := TComboBox.Create(self);
    ItemTypeOpenChoice: FInputControl1 := TComboEdit.Create(self);
    ItemTypeAttachment:
      begin
      FInputControl1 := TButton.Create(self);
      TButton(FInputControl1).Text := 'Attach...';
      end;
    ItemTypeReference: FInputControl1 := TComboBox.Create(self);
    ItemTypeQuantity:
      begin
      FInputControl1 := TEdit.Create(self);
      FInputControl2 := TComboEdit.Create(self);
      end;
  end;
  if FInputControl1 <> nil then
  begin
    FInputControl1.Parent := FBase;
    FInputControl1.OnEnter := SelfClick;
    FInputControl1.OnDblClick := EditClick;
    FInputControl1.Position.Y := (Height - FInputControl1.Height) / 2;
    FInputControl1.Position.X := FLabel.Position.X+FLabel.Width+10;
    FInputControl1.Width := Width - FInputControl1.Position.X - 80 - 10;
  end;
  if FInputControl2 <> nil then
  begin
    FInputControl2.Parent := FBase;
    FInputControl2.OnEnter := SelfClick;
    FInputControl2.OnDblClick := EditClick;
    FInputControl2.Position.Y := (Height - FInputControl1.Height) / 2;
    FInputControl1.Width := FInputControl1.width / 2 - 10;
    FInputControl2.Position.X := FInputControl1.Position.X +FInputControl1.Width + 10;
    FInputControl2.Width := FInputControl1.Width;
  end;

  FDelete := TButton.Create(self);
  FDelete.Parent := FBase;
  FDelete.Position.y := (height - 20) / 2;
  FDelete.Height := 20;
  FDelete.Width := 20;
  FDelete.OnClick := DeleteClick;
  FDelete.Images := ImageList;
  FDelete.ImageIndex := 4;
  FEditButton := TButton.Create(self);
  FEditButton.Parent := FBase;
  FEditButton.Position.y := (height - 20) / 2;
  FEditButton.Height := 20;
  FEditButton.Width := 20;
  FEditButton.OnClick := EditClick;
  FEditButton.Images := ImageList;
  FEditButton.ImageIndex := 7;
  FAddItem := TButton.Create(self);
  FAddItem.Parent := FBase;
  FAddItem.Position.y := (height - 20) / 2;
  FAddItem.Height := 20;
  FAddItem.Width := 20;
  FAddItem.OnClick := AddItemClick;
  FAddItem.Images := ImageList;
  FAddItem.ImageIndex := 5;

  FIn := TButton.Create(self);
  FIn.Parent := FBase;
  FIn.Position.X := 6;
  FIn.Position.y := (height - 20) / 2;
  FIn.Height := 16;
  FIn.Width := 16;
  FIn.OnClick := InClick;
  FIn.Enabled := ParentItem <> nil;
  if FIn.Enabled then
  begin
    FIn.Images := ImageList;
    FIn.ImageIndex := 0;
  end;
  FUp := TButton.Create(self);
  FUp.Parent := FBase;
  FUp.Position.X := 22;
  FUp.Position.y := (height - 16) / 2 - 8;
  FUp.Height := 16;
  FUp.Width := 16;
  FUp.OnClick := UpClick;
  FUp.Enabled := FPrevious <> nil;
  if FUp.Enabled then
  begin
    FUp.Images := ImageList;
    FUp.ImageIndex := 2;
  end;
  FDown := TButton.Create(self);
  FDown.Parent := FBase;
  FDown.Position.X := 22;
  FDown.Position.y := (height - 16) / 2 + 8;
  FDown.Height := 16;
  FDown.Width := 16;
  FDown.OnClick := DownClick;
  FDown.Enabled := FNext <> nil;
  if FDown.Enabled then
  begin
    FDown.Images := ImageList;
    FDown.ImageIndex := 3;
  end;
  FOut := TButton.Create(self);
  FOut.Parent := FBase;
  FOut.Position.X := 38;
  FOut.Position.y := (height - 16) / 2;
  FOut.Height := 16;
  FOut.Width := 16;
  FOut.OnClick := OutClick;
  FOut.Enabled := FPrevious <> nil;
  if FOut.Enabled then
  begin
    FOut.Images := ImageList;
    FOut.ImageIndex := 1;
  end;

  FDelete.Position.X := width - 25;
  FEditButton.Position.X := width - 78;
  FAddItem.Position.X := width - 47;
  FDelete.Visible := false;
  FOut.Visible := false;
  FIn.Visible := false;
  FUp.Visible := false;
  FDown.Visible := false;
  FEditButton.Visible := false;
  FAddItem.Visible := false;

  if FHasFocus then
    privGrabFocus;
end;

procedure TQuestionnaireItemPanel.clear;
begin
  if FBase <> nil then
    RemoveObject(FBase);
  if FLabel <> nil then
    RemoveObject(FLabel);
  if FInputControl1 <> nil then
    RemoveObject(FInputControl1);
  if FInputControl2 <> nil then
    RemoveObject(FInputControl2);
  if FEditButton <> nil then
    RemoveObject(FEditButton);
  if FUp <> nil then
    RemoveObject(FUp);
  if FDown <> nil then
    RemoveObject(FDown);
  if FIn <> nil then
    RemoveObject(FIn);
  if FOut <> nil then
    RemoveObject(FOut);
  if FAddItem <> nil then
    RemoveObject(FAddItem);
  if FDelete <> nil then
    RemoveObject(FDelete);
end;


constructor TQuestionnaireItemPanel.Create(owner: TComponent);
begin
  inherited;

end;

procedure TQuestionnaireItemPanel.DeleteClick(sender: TObject);
begin
  TQuestionnairePanel(Owner).delete(self);
end;

destructor TQuestionnaireItemPanel.Destroy;
begin
  FQuestionnaire.Free;
  FSettings.Free;
  FParent.Free;
  FItem.Free;
  FPrevious.Free;
  FNext.Free;
  inherited;
end;

procedure TQuestionnaireItemPanel.DownClick(sender: TObject);
begin
  TQuestionnairePanel(Owner).MoveDown(self);
end;

procedure TQuestionnaireItemPanel.EditClick(sender: TObject);
var
  form : TQuestionnaireItemForm;
begin
  form := TQuestionnaireItemForm.Create(self);
  try
    form.item := item.clone;
    form.questionnaire := questionnaire.Link;
    form.Settings := Settings.link;
    form.OnWork := OnWork;
    if ShowModalHack(form) = mrOk then
    begin
      item.Assign(form.item);
      build;
      TQuestionnairePanel(Owner).markChange(item);
    end;
  finally
    form.Free;
  end;
end;

procedure TQuestionnaireItemPanel.grabFocus;
begin
  if FHasFocus then
    exit;
  privGrabFocus;
end;

procedure TQuestionnaireItemPanel.InClick(sender: TObject);
begin
  TQuestionnairePanel(Owner).MoveIn(self);
end;

procedure TQuestionnaireItemPanel.LabelDlbClick(sender: TObject);
begin
  FLabel.Text := '';
  FTextEdit := TEdit.Create(self);
  FTextEdit.Parent := FBase;
  FTextEdit.Position.Y := (Height - FTextEdit.Height) / 2;
  FTextEdit.Position.X := FLabel.Position.X;
  FTextEdit.Width := FLabel.Width+10;
  FTextEdit.Text := item.text;
  FTextEdit.OnKeyDown := TextEditKeyDown;
end;

procedure TQuestionnaireItemPanel.privGrabFocus;
begin
  TQuestionnairePanel(Owner).setFocusedPanel(self);
  FHasFocus := true;
  FBase.Fill.Color := TAlphaColorRec.Lightgoldenrodyellow;
  FBase.Stroke.Thickness := 1.0;
  FDelete.Visible := true;
  FOut.Visible := true;
  FIn.Visible := true;
  FUp.Visible := true;
  FDown.Visible := true;
  FEditButton.Visible := true;
  FAddItem.Visible := true;
end;

function TQuestionnaireItemPanel.lineStartX: Single;
begin
  result := FLabel.Position.X;
end;

function TQuestionnaireItemPanel.lineStartY: Single;
begin
  result := FLabel.Position.Y + FLabel.Height;
end;

procedure TQuestionnaireItemPanel.loseFocus;
begin
  FHasFocus := false;
  if FTextEdit <> nil then
  begin
    RemoveObject(FTextEdit);
    FTextEdit := nil;
    makeLabelText;
  end;
  if FBase <> nil then
  begin
    FBase.Fill.Color := TAlphaColorRec.White;
    FBase.Stroke.Thickness := 0.0;
    FDelete.Visible := false;
    FOut.Visible := false;
    FIn.Visible := false;
    FUp.Visible := false;
    FDown.Visible := false;
    FEditButton.Visible := false;
    FAddItem.Visible := false;
  end;
end;

procedure TQuestionnaireItemPanel.OutClick(sender: TObject);
begin
  TQuestionnairePanel(Owner).MoveOut(self);
end;

procedure TQuestionnaireItemPanel.Resize;
begin
  inherited;
  if FBase <> nil then
  begin
    FBase.Width := Width;
    FDelete.Position.X := width - 25;
    FEditButton.Position.X := width - 78;
    FAddItem.Position.X := width - 47;
  end;
  if FInputControl1 <> nil then
  begin
    FInputControl1.Width := Width - FInputControl1.Position.X - 80 - 10;
    if FInputControl2 <> nil then
    begin
      FInputControl1.Width := FInputControl1.width / 2 - 10;
      FInputControl2.Position.X := FInputControl1.Position.X +FInputControl1.Width +10;
      FInputControl2.Width := FInputControl1.Width;
    end;
  end;
end;

procedure TQuestionnaireItemPanel.SelfClick(sender: TObject);
begin
  grabFocus;
end;

procedure TQuestionnaireItemPanel.SetItem(const Value: TFhirQuestionnaireItem);
begin
  FItem.Free;
  FItem := Value;
end;

procedure TQuestionnaireItemPanel.SetNext(const Value: TFhirQuestionnaireItem);
begin
  FNext.Free;
  FNext := Value;
end;

procedure TQuestionnaireItemPanel.SetParentItem(const Value: TFhirQuestionnaireItem);
begin
  FParent.Free;
  FParent := Value;
end;

procedure TQuestionnaireItemPanel.SetPrevious(const Value: TFhirQuestionnaireItem);
begin
  FPrevious.Free;
  FPrevious := Value;
end;

procedure TQuestionnaireItemPanel.SetQuestionnaire(const Value: TFhirQuestionnaire);
begin
  FQuestionnaire.Free;
  FQuestionnaire := Value;
end;

procedure TQuestionnaireItemPanel.SetSettings(const Value: TFHIRToolkitSettings);
begin
  FSettings.Free;
  FSettings := Value;
end;

procedure TQuestionnaireItemPanel.TextEditKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  if key = vkReturn then
  begin
    item.text := FTextEdit.Text;
    build;
    Key := 0;
    RemoveObject(FTextEdit);
    FTextEdit := nil;
  end;
end;

procedure TQuestionnaireItemPanel.UpClick(sender: TObject);
begin
  TQuestionnairePanel(Owner).MoveUp(self);
end;

procedure TQuestionnaireItemPanel.updateLayout;
begin
  FLabel.Position.X := 60 + Level * 10;

  if FInputControl1 <> nil then
    FInputControl1.Position.X := FLabel.Position.X+FLabel.Width+10;
  if FInputControl2 <> nil then
  begin
    FInputControl1.Width := FInputControl1.width / 2 - 10;
    FInputControl2.Position.X := FInputControl1.Position.X +FInputControl1.Width + 10;
    FInputControl2.Width := FInputControl1.Width;
  end;

  FIn.Enabled := ParentItem <> nil;
  if FIn.Enabled then
  begin
    FIn.Images := ImageList;
    FIn.ImageIndex := 0;
  end
  else
    FIn.Images := nil;
  FUp.Enabled := FPrevious <> nil;
  if FUp.Enabled then
  begin
    FUp.Images := ImageList;
    FUp.ImageIndex := 2;
  end
  else
    FUp.Images := nil;
  FDown.Enabled := FNext <> nil;
  if FDown.Enabled then
  begin
    FDown.Images := ImageList;
    FDown.ImageIndex := 3;
  end
  else
    FDown.Images := nil;
  FOut.Enabled := FPrevious <> nil;
  if FOut.Enabled then
  begin
    FOut.Images := ImageList;
    FOut.ImageIndex := 1;
  end
  else
    FOut.Images := nil;

  FDelete.Position.X := width - 25;
  FEditButton.Position.X := width - 80;
  FAddItem.Position.X := width - 47;
end;

end.
