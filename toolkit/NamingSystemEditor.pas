unit NamingSystemEditor;

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
  System.SysUtils, System.Rtti, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, FMX.Layouts, FMX.TreeView, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, FMX.DateTimeCtrls, FMX.ListBox, FMX.Edit, FMX.DialogService,
  FMX.Grid.Style, FMX.Grid, FMX.Menus,
  fsl_utilities, fsl_stream, FHIR.Ui.Fmx,
  fhir_objects, 
  FHIR.Version.Constants, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Utilities, fhir_indexing, FHIR.Version.IndexInfo,
  BaseResourceFrame,
  SearchParameterEditor, ListSelector, AddRestResourceDialog, MemoEditorDialog, TranslationsEditorDialog,
  ResourceEditingSupport, System.ImageList, FMX.ImgList, ResourceHistoryDialog;

type
  TFrame = TBaseResourceFrame; // re-aliasing the Frame to work around a designer bug

  TNamingSystemEditorFrame = class(TFrame)
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    cbExperimental: TCheckBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label13: TLabel;
    edtName: TEdit;
    cbxStatus: TComboBox;
    dedDate: TDateEdit;
    edtPublisher: TEdit;
    edtDescription: TEdit;
    edtPurpose: TEdit;
    cbxJurisdiction: TComboBox;
    edtVersion: TEdit;
    btnMemoForDesc: TButton;
    btnMemoPurpose: TButton;
    btnNameTranslations: TButton;
    cbxKind: TComboBox;
    Label2: TLabel;
    ToolbarImages: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    grid: TGrid;
    Label1: TLabel;
    btnAdd: TButton;
    btnUp: TButton;
    btnDown: TButton;
    Button4: TButton;
    colValue: TStringColumn;
    colPreferred: TCheckColumn;
    colType: TPopupColumn;
    coLComment: TStringColumn;
    colPeriodStart: TDateColumn;
    colPeriodEnd: TDateColumn;
    Label5: TLabel;
    edtUrl: TEdit;
    procedure inputChanged(Sender: TObject);
    procedure btnMemoForDescClick(Sender: TObject);
    procedure btnMemoPurposeClick(Sender: TObject);
    procedure btnNameTranslationsClick(Sender: TObject);
    procedure gridResize(Sender: TObject);
    procedure gridGetValue(Sender: TObject; const ACol, ARow: Integer;
      var Value: TValue);
    procedure gridSetValue(Sender: TObject; const ACol, ARow: Integer;
      const Value: TValue);
    procedure btnAddClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    function GetNamingSystem: TFHIRNamingSystem;
    function readJurisdiction : Integer;
    function getJurisdiction(i : integer) : TFHIRCodeableConcept;
  public
    destructor Destroy; override;

    property NamingSystem : TFHIRNamingSystem read GetNamingSystem;
    procedure load; override;

    procedure commit; override;
    procedure cancel; override;

  end;

implementation

{$R *.fmx}

{ TNamingSystemEditorFrame }

procedure TNamingSystemEditorFrame.btnAddClick(Sender: TObject);
begin
  NamingSystem.uniqueIdList.append;
  grid.rowCount := grid.RowCount + 1;
  grid.row := grid.rowCount - 1;
end;

procedure TNamingSystemEditorFrame.btnDownClick(Sender: TObject);
var
  i : integer;
begin
  if grid.row < NamingSystem.uniqueIdList.Count - 1 then
  begin
    i := grid.row;
    grid.RowCount := 0;
    NamingSystem.uniqueIdList.Exchange(i, i+1);
    grid.RowCount := NamingSystem.uniqueIdList.Count;
    grid.row := i-1;
  end
  else
    Beep;
end;

procedure TNamingSystemEditorFrame.btnMemoForDescClick(Sender: TObject);
begin
  if NamingSystem.descriptionElement = nil then
    NamingSystem.descriptionElement := TFhirMarkdown.Create;
  editMarkdownDialog(self, 'NamingSystem Description', btnMemoForDesc, edtDescription, NamingSystem, NamingSystem.descriptionElement);
end;

procedure TNamingSystemEditorFrame.btnMemoPurposeClick(Sender: TObject);
begin
  if NamingSystem.usageElement = nil then
    NamingSystem.usageElement := TFhirMarkdown.Create;
  editStringDialog(self, 'NamingSystem Purpose', btnMemoPurpose, edtPurpose, NamingSystem, NamingSystem.usageElement);
end;

procedure TNamingSystemEditorFrame.btnNameTranslationsClick(Sender: TObject);
begin
  if NamingSystem.nameElement = nil then
    NamingSystem.nameElement := TFhirString.Create;
  editStringDialog(self, 'NamingSystem Name', btnNameTranslations, edtName, NamingSystem, NamingSystem.nameElement);
end;

procedure TNamingSystemEditorFrame.btnUpClick(Sender: TObject);
var
  i : integer;
begin
  if grid.row > 0 then
  begin
    i := grid.row;
    grid.RowCount := 0;
    NamingSystem.uniqueIdList.Exchange(i, i-1);
    grid.RowCount := NamingSystem.uniqueIdList.Count;
    grid.row := i-1;
  end
  else
    Beep;
end;

procedure TNamingSystemEditorFrame.Button4Click(Sender: TObject);
var
  i : integer;
begin
  if (NamingSystem.uniqueIdList.Count > 0) and (grid.row >= 0) then
  begin
    i := grid.row;
    grid.RowCount := 0;
    NamingSystem.uniqueIdList.remove(i);
    grid.RowCount := NamingSystem.uniqueIdList.Count;
    grid.row := i-1;
  end
  else
    Beep;
end;

procedure TNamingSystemEditorFrame.cancel;
begin
end;

procedure TNamingSystemEditorFrame.commit;
var
  cc : TFHIRCodeableConcept;
begin
  NamingSystem.name := edtName.Text;
  NamingSystem.publisher := edtPublisher.text;
  if edtUrl.text = '' then
    NamingSystem.removeExtension('http://hl7.org/fhir/5.0/StructureDefinition/extension-NamingSystem.url')
  else
    NamingSystem.setExtensionString('http://hl7.org/fhir/5.0/StructureDefinition/extension-NamingSystem.url',  edtUrl.text);
  NamingSystem.description := edtDescription.Text;
  NamingSystem.usage := edtPurpose.Text;
  NamingSystem.kind := TFhirNamingsystemTypeEnum(cbxKind.ItemIndex);
  NamingSystem.status := TFhirPublicationStatusEnum(cbxStatus.ItemIndex);
  NamingSystem.date := TFslDateTime.make(dedDate.DateTime, dttzLocal);
  NamingSystem.jurisdictionList.Clear;
  cc := getJurisdiction(cbxJurisdiction.ItemIndex);
  if (cc <> nil) then
    NamingSystem.jurisdictionList.add(cc);
  ResourceIsDirty := true;
end;

destructor TNamingSystemEditorFrame.Destroy;
begin
  inherited;
end;

function TNamingSystemEditorFrame.GetNamingSystem: TFHIRNamingSystem;
begin
  result := TFHIRNamingSystem(Resource);
end;

procedure TNamingSystemEditorFrame.gridGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  id : TFhirNamingSystemUniqueId;
begin
  id := NamingSystem.uniqueIdList[ARow];
  case ACol of
    0: value := CODES_TFhirNamingsystemIdentifierTypeEnum[id.type_];
    1: value := id.Value;
    2: value := id.preferred;
    3: if (id.period <> nil) and not id.period.start.null then
         value := id.period.start.dateTime
       else
         value := nil;
    4: if (id.period <> nil) and not id.period.end_.null then
         value := id.period.end_.dateTime
       else
         value := nil;
    5: value := id.comment;
  end;
end;

procedure TNamingSystemEditorFrame.gridResize(Sender: TObject);
begin
  coLComment.width := grid.width - (colValue.Width +colPreferred.Width + colType.Width + colPeriodStart.Width + colPeriodEnd.Width) - 30 {scrollbar};
end;

procedure TNamingSystemEditorFrame.gridSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
var
  id : TFhirNamingSystemUniqueId;
begin
  id := NamingSystem.uniqueIdList[ARow];
  case ACol of
    0: id.type_ := TFhirNamingsystemIdentifierTypeEnum(ord(StringArrayIndexOf(CODES_TFhirNamingsystemIdentifierTypeEnum, value.AsString)));
    1: id.Value := value.asString;
    2: id.preferred := value.asBoolean;
    3: begin
       if (id.period = nil) then
         id.period := TFHIRPeriod.create;
       id.period.start := TFslDateTime.makeLocal(value.asExtended).TruncToDay;
       end;
    4: begin
       if (id.period = nil) then
         id.period := TFHIRPeriod.create;
       id.period.end_ := TFslDateTime.makeLocal(value.asExtended).TruncToDay;
       end;
    5: id.comment := value.asString;
  end;

end;

function TNamingSystemEditorFrame.getJurisdiction(i: integer): TFHIRCodeableConcept;
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

procedure TNamingSystemEditorFrame.inputChanged(Sender: TObject);
begin
  if not Loading then
    commit;
end;

procedure TNamingSystemEditorFrame.load;
begin
  inherited;
  Loading := true;
  try
    edtName.Text := NamingSystem.name;
    edtPublisher.text := NamingSystem.publisher;
    edtUrl.text := NamingSystem.getExtensionString('http://hl7.org/fhir/5.0/StructureDefinition/extension-NamingSystem.url');
    edtDescription.Text := NamingSystem.description;
    edtPurpose.Text := NamingSystem.usage;
    cbxKind.ItemIndex := ord(NamingSystem.kind);
    cbxStatus.ItemIndex := ord(NamingSystem.status);
    if NamingSystem.dateElement = nil then
      dedDate.Text := ''
    else
      dedDate.DateTime := NamingSystem.date.DateTime;
    cbxJurisdiction.ItemIndex := readJurisdiction;
    grid.rowcount := 0;
    grid.rowCount := NamingSystem.uniqueidList.count;
  finally
    Loading := false;
  end;
end;

function TNamingSystemEditorFrame.readJurisdiction: Integer;
var
  cc : TFhirCodeableConcept;
  c : TFhirCoding;
begin
  result := -1;
  for cc in NamingSystem.jurisdictionList do
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


end.


