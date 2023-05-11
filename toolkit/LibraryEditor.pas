unit LibraryEditor;

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
  System.SysUtils, System.Rtti, System.Types, System.UITypes, System.Classes, System.Variants, Soap.EncdDecd,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, FMX.Layouts, FMX.TreeView, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, FMX.DateTimeCtrls, FMX.ListBox, FMX.Edit, FMX.DialogService,
  FMX.Grid.Style, FMX.Grid, FMX.Menus, FMX.ImgList,
  fsl_utilities, fsl_http, fsl_stream,
  fhir_objects, 
  FHIR.Version.Constants, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Utilities, fhir_indexing, FHIR.Version.IndexInfo,
  BaseResourceFrame, ToolKitUtilities,
  SearchParameterEditor, ListSelector, AddRestResourceDialog, ValuesetExpansion, ValuesetSelectDialog, MemoEditorDialog,
  CodeSystemConceptDialog, FMX.Platform, System.ImageList, TranslationsEditorDialog;

type
  TFrame = TBaseResourceFrame; // re-aliasing the Frame to work around a designer bug

  TLibraryEditorFrame = class(TFrame)
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
    tvStructure: TTreeView;
    tbStructure: TTabControl;
    tbMetadata: TTabItem;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    cbExperimental: TCheckBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    edtURL: TEdit;
    edtName: TEdit;
    edtTitle: TEdit;
    cbxStatus: TComboBox;
    dedDate: TDateEdit;
    edtPublisher: TEdit;
    edtDescription: TEdit;
    edtPurpose: TEdit;
    edtCopyright: TEdit;
    cbxJurisdiction: TComboBox;
    edtVersion: TEdit;
    tvMetadata: TTreeViewItem;
    btnMemoForDesc: TButton;
    btnMemoPurpose: TButton;
    btnMemoCopyright: TButton;
    Label12: TLabel;
    edtIdSystem: TEdit;
    Label25: TLabel;
    edtIdValue: TEdit;
    Label26: TLabel;
    ScrollBox1: TScrollBox;
    tbContent: TTabItem;
    tvContent: TTreeViewItem;
    pnlPropertyActions: TPanel;
    dlgExport: TSaveDialog;
    btnName: TButton;
    ToolbarImages: TImageList;
    btnTitle: TButton;
    btnPublisher: TButton;
    Label14: TLabel;
    edtMimeType: TEdit;
    mContent: TMemo;
    procedure tvStructureClick(Sender: TObject);
    procedure inputChanged(Sender: TObject);
    procedure btnMemoForDescClick(Sender: TObject);
    procedure btnMemoPurposeClick(Sender: TObject);
    procedure btnMemoCopyrightClick(Sender: TObject);
    procedure btnNameClick(Sender: TObject);
    procedure btnTitleClick(Sender: TObject);
    procedure btnPublisherClick(Sender: TObject);
  private

    function GetLibrary: TFHIRLibrary;
    function getEncoding(s : String) : TEncoding;
    function readJurisdiction : Integer;
    function getJurisdiction(i : integer) : TFHIRCodeableConcept;

    procedure loadMetadata;
    procedure loadContent;

    procedure commitMetadata;
    procedure commitContent;
  public
    constructor Create(owner : TComponent); override;
    destructor Destroy; override;

    property Lib : TFHIRLibrary read GetLibrary;
    procedure load; override;

    procedure commit; override;
    procedure cancel; override;

  end;

implementation

{$R *.fmx}

function polish(s : String): String;
begin
  result := s.trim.replace(#13, ' ').replace(#10, ' ').replace('  ', ' ');
end;



{ TLibraryEditorFrame }

constructor TLibraryEditorFrame.Create(owner: TComponent);
begin
  inherited;
end;

destructor TLibraryEditorFrame.Destroy;
begin
  inherited;
end;

procedure TLibraryEditorFrame.btnMemoCopyrightClick(Sender: TObject);
begin
  if Lib.copyrightElement = nil then
    Lib.copyrightElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'Libary Copyright', btnMemoCopyright, edtCopyright, Lib, Lib.copyrightElement);
end;

procedure TLibraryEditorFrame.btnMemoForDescClick(Sender: TObject);
begin
  if Lib.descriptionElement = nil then
    Lib.descriptionElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'Library Description', btnMemoForDesc, edtDescription, Lib, Lib.descriptionElement);
end;

procedure TLibraryEditorFrame.btnMemoPurposeClick(Sender: TObject);
begin
  if Lib.purposeElement = nil then
    Lib.purposeElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'Library Purpose', btnMemoPurpose, edtPurpose, Lib, Lib.purposeElement);
end;

procedure TLibraryEditorFrame.btnNameClick(Sender: TObject);
begin
  if Lib.nameElement = nil then
    Lib.nameElement := TFhirString.Create;
  editStringDialog(self, 'Library Name', btnName, edtName, Lib, Lib.nameElement);
end;

procedure TLibraryEditorFrame.btnPublisherClick(Sender: TObject);
begin
  if Lib.publisherElement = nil then
    Lib.publisherElement := TFhirString.Create;
  editStringDialog(self, 'Library Publisher', btnPublisher, edtPublisher, Lib, Lib.publisherElement);
end;

procedure TLibraryEditorFrame.btnTitleClick(Sender: TObject);
begin
  if Lib.titleElement = nil then
    Lib.titleElement := TFhirString.Create;
  editStringDialog(self, 'Library Title', btnTitle, edtTitle, Lib, Lib.titleElement);
end;

procedure TLibraryEditorFrame.cancel;
begin
end;

procedure TLibraryEditorFrame.commit;
begin
  if tvStructure.Selected = tvMetadata then
    CommitMetadata
  else if tvStructure.Selected = tvContent then
    CommitContent;
  ResourceIsDirty := true;
end;

procedure TLibraryEditorFrame.commitMetadata;
var
  s : String;
  cc : TFHIRCodeableConcept;
begin
  Lib.experimental := cbExperimental.IsChecked;

  Lib.url := edtURL.Text;
  Lib.name := edtName.Text;
  Lib.title := edtTitle.Text;
  Lib.version := edtVersion.Text;
  Lib.publisher := edtPublisher.text;
  Lib.description := edtDescription.Text;
  Lib.purpose := edtPurpose.Text;
  Lib.copyright := edtCopyright.Text;
  Lib.status := TFhirPublicationStatusEnum(cbxStatus.ItemIndex);
  Lib.date := TFslDateTime.make(dedDate.DateTime, dttzLocal);
  Lib.jurisdictionList.Clear;
  cc := getJurisdiction(cbxJurisdiction.ItemIndex);
  if (cc <> nil) then
    Lib.jurisdictionList.add(cc);

  if (edtIdSystem.Text <> '') or (edtIdValue.Text <> '') then
  begin
    if Lib.identifierList.Count = 0 then
      Lib.identifierList.Append;
    Lib.identifierList[0].system := edtIdSystem.Text;
    Lib.identifierList[0].value := edtIdValue.Text;
  end
  else
    Lib.identifierList.Clear;
end;

procedure TLibraryEditorFrame.commitContent;
var
  enc : TEncoding;
begin
  if Lib.contentList.IsEmpty then
    Lib.contentList.Append;
  Lib.contentList[0].contentType := edtMimeType.Text;
  enc := getEncoding(edtMimeType.Text);
  Lib.contentList[0].data := enc.GetBytes(mContent.Text);
end;

function TLibraryEditorFrame.GetLibrary: TFHIRLibrary;
begin
  result := TFHIRLibrary(Resource);
end;

function displayLang(lang : String) : string;
begin
  if lang = '' then
    result := ''
  else if lang.StartsWith('bn') then result := 'bn (Bengali)'
  else if lang.startsWith('cs') then result := 'cs (Czech)'
  else if lang.startsWith('da') then result := 'da (Danish)'
  else if lang.startsWith('de') then result := 'de (German)'
  else if lang.startsWith('el') then result := 'el (Greek)'
  else if lang.startsWith('en') then result := 'en (English)'
  else if lang.startsWith('es') then result := 'es (Spanish)'
  else if lang.startsWith('fi') then result := 'fi (Finnish)'
  else if lang.startsWith('fr') then result := 'fr (French)'
  else if lang.startsWith('fy') then result := 'fy (Frysian)'
  else if lang.startsWith('hi') then result := 'hi (Hindi)'
  else if lang.startsWith('hr') then result := 'hr (Croatian)'
  else if lang.startsWith('it') then result := 'it (Italian)'
  else if lang.startsWith('ja') then result := 'ja (Japanese)'
  else if lang.startsWith('ko') then result := 'ko (Korean)'
  else if lang.startsWith('nl') then result := 'nl (Dutch)'
  else if lang.startsWith('no') then result := 'no (Norwegian)'
  else if lang.startsWith('pa') then result := 'pa (Punjabi)'
  else if lang.startsWith('pt') then result := 'pt (Portuguese)'
  else if lang.startsWith('ru') then result := 'ru (Russian)'
  else if lang.startsWith('sr') then result := 'sr (Serbian)'
  else if lang.startsWith('sv') then result := 'sv (Swedish)'
  else if lang.startsWith('te') then result := 'te (Telegu)'
  else if lang.startsWith('zh') then result := 'zh (Chinese))'
  else
    result := lang;
end;

function TLibraryEditorFrame.getEncoding(s: String): TEncoding;
var
  c : TMimeContentType;
begin
  c := TMimeContentType.parseSingle(s);
  try
    if not c.hasParam('charset') then
      result := TEncoding.UTF8
    else if c.Params['charset'] = 'utf-8' then
      result := TEncoding.UTF8
    else if c.Params['charset'] = 'utf-16' then
      result := TEncoding.BigEndianUnicode
    else if c.Params['charset'] = 'ascii' then
      result := TEncoding.ASCII
    else
      raise EFHIRException.create('Unknown character set '+c.Params['charset']);
  finally
    c.Free;
  end;
end;

function TLibraryEditorFrame.getJurisdiction(i: integer): TFHIRCodeableConcept;
begin
  case i of
    1:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'AT');
    2:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'AU');
    3:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'BR');
    4:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'CA');
    5:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'CH');
    6:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'CL');
    7:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'CN');
    8:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'DE');
    9:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'DK');
    10:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'EE');
    11:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'ES');
    12:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'FI');
    13:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'FR');
    14:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'GB');
    15:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'NL');
    16:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'NO');
    17:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'NZ');
    18:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'RU');
    19:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'US');
    21:result := TFhirCodeableConcept.Create('urn:iso:std:iso:3166', 'VN');
    22:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '001');
    23:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '002');
    24:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '019');
    25:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '142');
    26:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '150');
    27:result := TFhirCodeableConcept.Create('http://unstats.un.org/unsd/methods/m49/m49.htm', '053');
    else
      result := nil;
  end;
end;

procedure TLibraryEditorFrame.inputChanged(Sender: TObject);
begin
  if not Loading then
    commit;
end;

procedure TLibraryEditorFrame.load;
begin
  tvStructure.Selected := tvMetadata;
  tvStructure.ExpandAll;
  tvStructureClick(nil);
end;

procedure TLibraryEditorFrame.loadMetadata;
var
  url : TFHIRUri;
begin
  cbExperimental.IsChecked := Lib.experimental;

  edtURL.Text := Lib.url;
  edtName.Text := Lib.name;
  btnName.ImageIndex := translationsImageIndex(Lib.nameElement);
  edtTitle.Text := Lib.title;
  btnTitle.ImageIndex := translationsImageIndex(Lib.titleElement);
  edtVersion.Text := Lib.version;
  edtPublisher.text := Lib.publisher;
  btnPublisher.ImageIndex := translationsImageIndex(Lib.publisherElement);
  edtDescription.Text := Lib.description;
  btnMemoForDesc.ImageIndex := translationsImageIndex(Lib.descriptionElement);
  edtPurpose.Text := Lib.purpose;
  btnMemoPurpose.ImageIndex := translationsImageIndex(Lib.purposeElement);
  edtCopyright.Text := Lib.copyright;
  btnMemoCopyright.ImageIndex := translationsImageIndex(Lib.copyrightElement);
  cbxStatus.ItemIndex := ord(Lib.status);
  if Lib.dateElement = nil then
    dedDate.Text := ''
  else
    dedDate.DateTime := Lib.date.DateTime;
  cbxJurisdiction.ItemIndex := readJurisdiction;

  if Lib.identifierList.Count > 0 then
  begin
    edtIdSystem.Text := Lib.identifierList[0].system;
    edtIdValue.Text := Lib.identifierList[0].value;
  end
  else
  begin
    edtIdSystem.Text := '';
    edtIdValue.Text := '';
  end;
end;


procedure TLibraryEditorFrame.loadContent;
var
  enc : TEncoding;
begin
  if Lib.contentList.IsEmpty then
    with Lib.contentList.Append do
    begin
      data := TEncoding.UTF8.GetBytes(#13#10);
      contentType := 'text/plain; charset=utf-8';
      ResourceIsDirty := true;
    end;
  edtMimeType.Text := Lib.contentList[0].contentType;
  enc := getEncoding(edtMimeType.Text);
  mContent.Text := enc.GetString(Lib.contentList[0].data);
end;

function TLibraryEditorFrame.readJurisdiction: Integer;
var
  cc : TFhirCodeableConcept;
  c : TFhirCoding;
begin
  result := -1;
  for cc in Lib.jurisdictionList do
    for c in cc.codingList do
    begin
      if c.system = 'urn:iso:std:iso:3166' then
      begin
        if c.code = 'AT' then exit(1);
        if c.code = 'AU' then exit(2);
        if c.code = 'BR' then exit(3);
        if c.code = 'CA' then exit(4);
        if c.code = 'CH' then exit(5);
        if c.code = 'CL' then exit(6);
        if c.code = 'CN' then exit(7);
        if c.code = 'DE' then exit(8);
        if c.code = 'DK' then exit(9);
        if c.code = 'EE' then exit(10);
        if c.code = 'ES' then exit(11);
        if c.code = 'FI' then exit(12);
        if c.code = 'FR' then exit(13);
        if c.code = 'GB' then exit(14);
        if c.code = 'NL' then exit(15);
        if c.code = 'NO' then exit(16);
        if c.code = 'NZ' then exit(17);
        if c.code = 'RU' then exit(18);
        if c.code = 'US' then exit(19);
        if c.code = 'VN' then exit(20);
      end
      else if c.system = 'http://unstats.un.org/unsd/methods/m49/m49.htm' then
      begin
        if c.code = '001' { World } then exit(22);
        if c.code = '002' { Africa } then exit(23);
        if c.code = '019' { Americas } then exit(24);
        if c.code = '142' { Asia } then exit(25);
        if c.code = '150' { Europe } then exit(26);
        if c.code = '053' { Australia and New Zealand } then exit(27);
      end
    end;
end;


procedure TLibraryEditorFrame.tvStructureClick(Sender: TObject);
begin
  Loading := true;
  try
    if tvStructure.Selected = tvMetadata then
    begin
      tbStructure.ActiveTab := tbMetadata;
      loadMetadata;
    end
    else if tvStructure.Selected = tvContent then
    begin
      tbStructure.ActiveTab := tbContent;
      loadContent;
    end
  finally
    Loading := false;
  end;
end;


end.


