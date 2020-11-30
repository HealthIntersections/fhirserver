unit MemoEditorDialog;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.Layouts,
  FMX.ListBox,
  fsl_base, FHIR.Ui.Fmx,
  fhir_objects, 
  FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Utilities,
  ToolkitUtilities, ListSelector;

type
  TMemoEditorForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Panel4: TPanel;
    btnAdd: TButton;
    btnDelete: TButton;
    ListBox1: TListBox;
    procedure FormShow(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FResource: TFHIRResource;
    FElement: TFHIRMarkdown;
    FExtensions : TFslList<TFhirExtension>;
    FCurrentIndex : integer;
    FPrimary : String;
    FOffset : integer;
    procedure SetElement(const Value: TFHIRMarkdown);
    procedure SetResource(const Value: TFHIRResource);
  public
    constructor Create(owner : TComponent); override;
    destructor Destroy; override;

    property Resource : TFHIRResource read FResource write SetResource;
    property Element : TFHIRMarkdown read FElement write SetElement;
  end;

var
  MemoEditorForm: TMemoEditorForm;

function editMarkdownDialog(owner : TComponent; title : String; button : TButton; edit : TEdit; resource : TFHIRResource; element : TFHIRMarkdown) : boolean; overload;
function editMarkdownDialog(owner : TComponent; title : String; button : TButton; edit : TMemo; resource : TFHIRResource; element : TFHIRMarkdown) : boolean; overload;
function editMarkdownDialog(owner : TComponent; title : String; button : TButton; edit : TMemo; resource : TFHIRResource; element : TFHIRString) : boolean; overload;

implementation

{$R *.fmx}

function editMarkdownDialog(owner : TComponent; title : String; button : TButton; edit : TEdit; resource : TFHIRResource; element : TFHIRMarkdown) : boolean;
begin
  MemoEditorForm := TMemoEditorForm.Create(owner);
  try
    MemoEditorForm.Resource := resource.Link;
    MemoEditorForm.Element := element.Link;
    MemoEditorForm.Caption := title;
    result := ShowModalHack(MemoEditorForm) = mrOk;
    if result then
    begin
      edit.Text := element.value;
      button.ImageIndex := translationsImageIndex(element);
    end;
  finally
    MemoEditorForm.Free;
  end;
end;

function editMarkdownDialog(owner : TComponent; title : String; button : TButton; edit : TMemo; resource : TFHIRResource; element : TFHIRMarkdown) : boolean;
begin
  MemoEditorForm := TMemoEditorForm.Create(owner);
  try
    MemoEditorForm.Resource := resource.Link;
    MemoEditorForm.Element := element.Link;
    MemoEditorForm.Caption := title;
    result := ShowModalHack(MemoEditorForm) = mrOk;
    if result then
    begin
      edit.Text := element.value;
      button.ImageIndex := translationsImageIndex(element);
    end;
  finally
    MemoEditorForm.Free;
  end;
end;

function editMarkdownDialog(owner : TComponent; title : String; button : TButton; edit : TMemo; resource : TFHIRResource; element : TFHIRString) : boolean;
var
  md : TFhirMarkdown;
begin
  md := TFhirMarkdown.Create;
  try
    md.value := element.value;
    MemoEditorForm := TMemoEditorForm.Create(owner);
    try
      MemoEditorForm.Resource := resource.Link;
      MemoEditorForm.Element := md.Link;
      MemoEditorForm.Caption := title;
      result := ShowModalHack(MemoEditorForm) = mrOk;
      if result then
      begin
        element.value := md.value;
        edit.Text := md.value;
        button.ImageIndex := translationsImageIndex(element);
      end;
    finally
      MemoEditorForm.Free;
    end;
  finally
    md.Free;
  end;
end;

procedure TMemoEditorForm.btnAddClick(Sender: TObject);
var
  dlg : TListSelectorForm;
  s: TObject;
  procedure checkAdd(code : String);
  var
    exists : boolean;
    ext : TFhirExtension;
  begin
    exists := false;
    for ext in FExtensions do
      exists := exists or (ext.getExtensionString('lang') = code);
    if not exists then
      dlg.ListBox1.Items.Add(langDesc(code));
  end;
var
  i : integer;
  c : String;
  ext : TFhirExtension;
  st : TStringList;
begin
  dlg := TListSelectorForm.Create(self);
  try
    st := TStringList.Create;
    try
      for c in st do
        checkadd(c);
    finally
      st.Free;
    end;

    if dlg.ShowModal = mrOk then
    begin
      for i := 0 to dlg.ListBox1.Items.Count - 1 do
        if dlg.ListBox1.ListItems[i].IsChecked then
        begin
          ext := TFhirExtension.Create;
          FExtensions.Add(ext);
          ext.url := 'http://hl7.org/fhir/StructureDefinition/translation';
          c := langCode(dlg.ListBox1.Items[i]);
          ext.setExtensionString('lang', c);
          ListBox1.Items.Add(langDesc(c));
        end;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TMemoEditorForm.btnDeleteClick(Sender: TObject);
begin
  FExtensions.Delete(FCurrentIndex-1);
  ListBox1.items.Delete(FCurrentIndex);
  ListBox1.ItemIndex := FCurrentIndex - 1;
  FCurrentIndex := -1;
  ListBox1Click(self);
end;

procedure TMemoEditorForm.Button1Click(Sender: TObject);
var
  ext : TFHIRExtension;
  langs : TStringList;
  s : string;
begin
  ListBox1Click(self);
  langs := TStringList.Create;
  try
    for ext in FExtensions do
    begin
      s := ext.getExtensionString('lang');
      if s = '' then
        raise EFHIRException.create('Language missing on a translation');
      if langs.IndexOf(s) > -1 then
        raise EFHIRException.create('Duplicate translation for '+s);
      langs.Add(s);
      ext.url := 'http://hl7.org/fhir/StructureDefinition/translation';
    end;
  finally
    langs.Free;
  end;
  element.value := FPrimary;
  element.removeExtension('http://hl7.org/fhir/StructureDefinition/translation');
  for ext in FExtensions do
    element.extensionList.add(ext.Link);
  modalResult := mrOk;
end;

constructor TMemoEditorForm.Create(owner: TComponent);
begin
  inherited;
  FExtensions := TFslList<TFhirExtension>.create;
end;

destructor TMemoEditorForm.Destroy;
begin
  FExtensions.Free;
  FElement.Free;
  FResource.Free;
  inherited;
end;

procedure TMemoEditorForm.FormShow(Sender: TObject);
var
  ext : TFhirExtension;
begin
  FCurrentIndex := -1;
  ListBox1.items.Clear;
  ListBox1.items.Add('Primary Lang');
  FPrimary := Element.value;
  FOffset := 0;
  Memo1.Text := FPrimary;
  FExtensions.Clear;
  for ext in Element.ExtensionList do
    if ext.url = 'http://hl7.org/fhir/StructureDefinition/translation' then
    begin
      FExtensions.Add(ext.Link);
      ListBox1.items.Add(langDesc(ext.getExtensionString('lang')));
    end;
  ListBox1.ItemIndex := 0;
  ListBox1Click(self);
  btnDelete.Enabled := false;
end;

procedure TMemoEditorForm.ListBox1Click(Sender: TObject);
begin
  if FCurrentIndex > -1 then
  begin
    if FCurrentIndex < 1 then
    begin
      FPrimary := Memo1.Text;
      FOffset := Memo1.SelStart;
    end
    else
    begin
      FExtensions[FCurrentIndex - 1].setExtensionString('content', Memo1.Text);
      FExtensions[FCurrentIndex - 1].TagInt := Memo1.SelStart;
    end;
  end;
  FCurrentIndex := ListBox1.ItemIndex;
  if FCurrentIndex < 1 then
  begin
    Memo1.Text := FPrimary;
    Memo1.SelStart := FOffset;
  end
  else
  begin
    Memo1.Text := FExtensions[FCurrentIndex - 1].getExtensionString('content');
    Memo1.SelStart := FExtensions[FCurrentIndex - 1].TagInt;
  end;
  btnDelete.Enabled := FCurrentIndex > 0;
end;

procedure TMemoEditorForm.SetElement(const Value: TFHIRMarkdown);
begin
  FElement.Free;
  FElement := Value;
end;

procedure TMemoEditorForm.SetResource(const Value: TFHIRResource);
begin
  FResource.Free;
  FResource := Value;
end;

end.
