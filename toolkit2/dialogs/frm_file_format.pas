unit frm_file_format;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls,
  fsl_base,
  fui_lcl_utilities, fui_lcl_managers,
  ftk_context, ftk_constants;

type

  { TSourceFormat }

  TSourceFormat = class (TFslObject)
  private
    FKind: TSourceEditorKind;
    FName: String;
    FReference: String;
  public
    constructor Create(kind: TSourceEditorKind; name: String; reference: String);
    function link : TSourceFormat; overload;

    property kind : TSourceEditorKind read FKind write FKind;
    property name : String read FName write FName;
    property reference : String read FReference write FReference;
  end;

  { TFormatListManager }

  TFormatListManager = class (TListManager<TSourceFormat>)
  private
    FKinds : TSourceEditorKindSet;
  public
    function canSort : boolean; override;
    function allowedOperations(item : TSourceFormat) : TNodeOperationSet; override;
    function loadList : boolean; override;
    function getImageIndex(item : TSourceFormat) : integer; override;
    function getCellText(item : TSourceFormat; col : integer) : String; override;
    function filterItem(item : TSourceFormat; s : String) : boolean; override;

    property Kinds : TSourceEditorKindSet read FKinds write FKinds;
  end;

  { TFileFormatChooser }

  TFileFormatChooser = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    lvFormats: TListView;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    FManager : TFormatListManager;
  public
    procedure setFHIRResource;
  end;

var
  FileFormatChooser: TFileFormatChooser;

implementation

{$R *.lfm}

{ TFormatListManager }

function TFormatListManager.canSort: boolean;
begin
  Result := false;
end;

function TFormatListManager.allowedOperations(item: TSourceFormat): TNodeOperationSet;
begin
  result := [opRefresh];
end;

function TFormatListManager.loadList: boolean;
begin
  Data.Add(TSourceFormat.create(sekFHIR, NAMES_TSourceEditorKind[sekFHIR], 'http://hl7.org/fhir'));
  Data.Add(TSourceFormat.create(sekv2, NAMES_TSourceEditorKind[sekv2], 'https://en.wikipedia.org/wiki/Health_Level_7#Version_2_messaging'));
  Data.Add(TSourceFormat.create(sekCDA, NAMES_TSourceEditorKind[sekCDA], 'http://hl7.org/cda'));
  Data.Add(TSourceFormat.create(sekXML, NAMES_TSourceEditorKind[sekXML], 'https://www.w3.org/TR/xml/'));
  Data.Add(TSourceFormat.create(sekJson, NAMES_TSourceEditorKind[sekJson], 'https://www.json.org/json-en.html'));
  Data.Add(TSourceFormat.create(sekLiquid, NAMES_TSourceEditorKind[sekLiquid], 'https://shopify.github.io/liquid/'));
  Data.Add(TSourceFormat.create(sekMap, NAMES_TSourceEditorKind[sekMap], 'http://hl7.org/fhir/StructureMap'));
  Data.Add(TSourceFormat.create(sekIni, NAMES_TSourceEditorKind[sekIni], 'https://en.wikipedia.org/wiki/INI_file'));
  Data.Add(TSourceFormat.create(sekText, NAMES_TSourceEditorKind[sekText], 'https://home.unicode.org/'));
  Data.Add(TSourceFormat.create(sekMD, NAMES_TSourceEditorKind[sekMD], 'https://spec.commonmark.org/'));
  Data.Add(TSourceFormat.create(sekJS, NAMES_TSourceEditorKind[sekJS], 'https://www.ecma-international.org/publications-and-standards/standards/ecma-262/'));
  Data.Add(TSourceFormat.create(sekHTML, NAMES_TSourceEditorKind[sekHTML], 'https://en.wikipedia.org/wiki/HTML5'));
  Data.Add(TSourceFormat.create(sekDicom, NAMES_TSourceEditorKind[sekDicom], 'https://www.dicomstandard.org/'));
  Data.Add(TSourceFormat.create(sekJWT, NAMES_TSourceEditorKind[sekJWT], 'https://datatracker.ietf.org/doc/html/rfc7519'));
end;

function TFormatListManager.getImageIndex(item: TSourceFormat): integer;
begin
  Result := ICONS_TSourceEditorKind[item.kind];
end;

function TFormatListManager.getCellText(item: TSourceFormat; col: integer): String;
begin
  case col of
    0: result := item.name;
    1: result := item.reference;
  end;
end;

function TFormatListManager.filterItem(item: TSourceFormat; s: String): boolean;
begin
  Result := item.kind in FKinds;
end;

{ TFileFormatChooser }

procedure TFileFormatChooser.ListBox1DblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFileFormatChooser.setFHIRResource;
begin
  ListBox1.ItemIndex := 0;
  ListBox1Click(self);
end;

procedure TFileFormatChooser.ListBox1Click(Sender: TObject);
begin
  btnOk.enabled := ListBox1.ItemIndex > -1;
  if ListBox1.ItemIndex = 0 then
  begin
    btnOk.caption := '&Next >>';
    btnOk.modalresult := mrNone;
  end
  else
  begin
    btnOk.caption := 'OK';
    btnOk.modalresult := mrOK;
  end;
end;

procedure TFileFormatChooser.btnOkClick(Sender: TObject);
begin
  if ListBox1.ItemIndex = 0 then
  begin
    PageControl1.ActivePage := TabSheet2;
    btnOk.caption := 'OK';
    btnOk.modalresult := mrOK;
    btnCancel.caption := 'Back';
    btnCancel.ModalResult := mrNone;
  end;
end;

procedure TFileFormatChooser.FormCreate(Sender: TObject);
begin
  setForOs(btnOk, btnCancel);
  FManager := TFormatListManager.create;
  FManager.Kinds := ALL_SourceEditorKinds;
  Fmanager.List :=
end;

procedure TFileFormatChooser.FormDestroy(Sender: TObject);
begin
  FManager.free;
end;

procedure TFileFormatChooser.btnCancelClick(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheet1;
  btnCancel.caption := 'Cancel';
  btnCancel.ModalResult := mrCancel;
  ListBox1Click(self);
end;

end.

