unit cdaHeaderForm;

{
Copyright (c) 2014+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  fsl_utilities,
  cda_base, cda_types, cda_objects, cda_documents,
  IIEditor, OIDCache;

type
  TCdaHeaderDialog = class(TForm)
    Document: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edtDocTitle: TEdit;
    edtDocCode: TEdit;
    Button3: TButton;
    edtDocId: TEdit;
    btnDocIdEdit: TButton;
    btnDocIdGuid: TButton;
    edtDocTime: TEdit; // TESBDateTimeEdit;
    btnDocTimeNow: TButton;
    edtDocLang: TEdit;
    btnDocLangDef: TButton;
    edtDocTimezone: TEdit;
    btnDocTimeClear: TButton;
    procedure btnDocIdGuidClick(Sender: TObject);
    procedure btnDocLangDefClick(Sender: TObject);
    procedure btnDocTimeNowClick(Sender: TObject);
    procedure btnDocTimeClearClick(Sender: TObject);
    procedure btnDocIdEditClick(Sender: TObject);
    procedure edtDocTitleChange(Sender: TObject);
    procedure edtDocTimeChange(Sender: TObject; const ElementNo: Cardinal);
    procedure edtDocTimezoneChange(Sender: TObject);
    procedure edtDocLangChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    cda : TcdaClinicalDocument;
    bound : boolean;
    function valid(var s : String):boolean;
    procedure bind;
    procedure setcda(const value: TcdaClinicalDocument);
  public
    { Public declarations }
    destructor Destroy; override;

    property CDADocument : TcdaClinicalDocument read cda write setcda;
  end;

var
  CdaHeaderDialog: TCdaHeaderDialog;

implementation

{$R *.dfm}

{ TCdaHeaderDialog }

function iif(test : boolean; v1, v2 : String) : String;
begin
  if test then
    result := v1
  else
    result := v2;
end;

function describeCD(cd : Tv3CD) : String;
begin
  if (cd = nil) then
    result := ''
  else
  begin
    if (cd.code <> '') and (cd.codesystem <> '') then
      result := DescribeOID(cd.codeSystem)+'::'+cd.code
    else
      result := 'todo';
  end;
end;

function describeII(ii : Tv3II) : String;
begin
  if (ii = nil) then
    result := ''
  else
  begin
    if (ii.extension <> '') and (ii.root <> '') then
      if ii.identifierName <> '' then
        result := ii.identifierName+'::'+ii.extension
      else
        result := DescribeOID(ii.root)+'::'+ii.extension
    else
      result := ii.root;
  end;
end;

procedure TCdaHeaderDialog.bind;
var
  l, r : String;
begin
  // doc
  edtDocTitle.Text := iif(cda.title <> nil, cda.title.value, '');
  edtDocCode.Text := iif(cda.code <> nil, describeCD(cda.code), '');
  edtDocId.Text := iif(cda.id <> nil, describeII(cda.id), '');
  if cda.effectiveTime <> nil then
  begin
    if pos('-', cda.effectiveTime.value) > 0 then
      StringSplit(cda.effectiveTime.value, '-', l, r)
    else
      StringSplit(cda.effectiveTime.value, '+', l, r);

    // edtDocTime.AsDateTime := HL7StringToDate('YYYYMMDDHHNNSS', l, false);
    edtDocTime.text := l;
    if pos('-', cda.effectiveTime.value) > 0 then
      edtDocTimezone.Text := '-'+r
    else
      edtDocTimezone.Text := '+'+r;
  end
  else
  begin
//    edtDocTime.Null := true;
    edtDocTimezone.Text := '';
  end;

  if (cda.languageCode <> nil) then
    edtDocLang.text := cda.languageCode.code
  else
    edtDocLang.text := '';
  bound := true;
end;

procedure TCdaHeaderDialog.btnDocIdEditClick(Sender: TObject);
var
  iiedit : TIIEditForm;
begin
  iiedit := TIIEditForm.create(self);
  try
    iiedit.ii := cda.id.Clone(nil);
    if iiedit.ShowModal = mrOk then
    begin
      cda.id := iiedit.ii.link;
      edtDocId.Text := iif(cda.id <> nil, describeII(cda.id), '');
    end;
  finally
    iiedit.free;
  end;
end;

procedure TCdaHeaderDialog.btnDocIdGuidClick(Sender: TObject);
begin
  cda.id.Clear;
  cda.id.root := lowercase(copy(GUIDToString(createGuid), 2, 36));
  edtDocId.Text := describeII(cda.id);
end;

procedure TCdaHeaderDialog.btnDocLangDefClick(Sender: TObject);
var
  wLang : LangID;
  szLang: Array [0..254] of Char;
  s : String;
begin
  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SISO639LANGNAME, szLang, wLang);
  s := szLang;
  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SISO3166CTRYNAME, szLang, wLang);
  s := s + '-'+szLang;
  if cda.languageCode = nil then
    cda.languageCode := Tv3CS.Create;
  cda.languageCode.code := s;
  edtDocLang.text := cda.languageCode.code;
end;

procedure TCdaHeaderDialog.btnDocTimeClearClick(Sender: TObject);
begin
  cda.effectiveTime := nil;
  // edtDocTime.Null := true;
  edtDocTimezone.Text := '';
end;

procedure TCdaHeaderDialog.btnDocTimeNowClick(Sender: TObject);
var
  l, r : String;
begin
  if cda.effectiveTime = nil then
    cda.effectiveTime := Tv3TS.create;
  cda.effectiveTime.value := TFslDateTime.makeLocal(dtpSec).toHL7;
  if pos('-', cda.effectiveTime.value) > 0 then
    StringSplit(cda.effectiveTime.value, '-', l, r)
  else
    StringSplit(cda.effectiveTime.value, '+', l, r);

  // edtDocTime.AsDateTime := HL7StringToDate('YYYYMMDDHHNNSS', l, false);
  edtDocTime.Text := l;
  if pos('-', cda.effectiveTime.value) > 0 then
    edtDocTimezone.Text := '-'+r
  else
    edtDocTimezone.Text := '+'+r;
end;

procedure TCdaHeaderDialog.Button2Click(Sender: TObject);
var
  msg : string;
begin
  if Valid(msg) then
    ModalResult := mrOk
  else
    ShowMessage(msg);
end;

destructor TCdaHeaderDialog.Destroy;
begin
  cda.Free;
  inherited;
end;

procedure TCdaHeaderDialog.edtDocLangChange(Sender: TObject);
begin
  if not bound then
    exit;

end;

procedure TCdaHeaderDialog.edtDocTimeChange(Sender: TObject; const ElementNo: Cardinal);
begin
  if not bound then
    exit;
  if edtDocTime.text = ''{.null} then
    cda.effectiveTime := nil
  else
  begin
    if cda.effectiveTime = nil then
      cda.effectiveTime := Tv3TS.create;
    // cda.effectiveTime.value := HL7DateToString(edtDocTime.AsDateTime, 'yyyymmddhhnnss', false) + edtDocTimezone.Text;
    cda.effectiveTime.value := edtDocTime.text;
  end;
end;

procedure TCdaHeaderDialog.edtDocTimezoneChange(Sender: TObject);
begin
  if not bound then
    exit;
  if edtDocTime.text = '' {.null} then
    cda.effectiveTime := nil
  else
  begin
    if cda.effectiveTime = nil then
      cda.effectiveTime := Tv3TS.create;
//    cda.effectiveTime.value := HL7DateToString(edtDocTime.AsDateTime, 'yyyymmddhhnnss', false) + edtDocTimezone.Text;
    cda.effectiveTime.value := edtDocTime.Text;
  end;
end;

procedure TCdaHeaderDialog.edtDocTitleChange(Sender: TObject);
begin
  if not bound then
    exit;
  if edtDocTitle.Text = '' then
    cda.title := nil
  else
  begin
    cda.title := Tv3ST.Create;
    cda.title.value := edtDocTitle.Text;
  end;
end;

procedure TCdaHeaderDialog.setcda(const value: TcdaClinicalDocument);
begin
  cda.Free;
  cda := value;
  if (cda <> nil) then
    bind;
end;

function TCdaHeaderDialog.valid(var s: String): boolean;
begin
  result := false;
  if (cda.id = nil) then
    s := 'A document.id must be provided'
  else if (cda.code = nil) then
    s := 'A docuemnt.code must be provided'
  else if (cda.effectiveTime = nil) then
    s := 'A document.date/time must be provided'
  else
    result := true;
end;

end.
