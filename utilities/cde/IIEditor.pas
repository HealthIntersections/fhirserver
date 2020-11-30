unit IIEditor;

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  fsl_utilities,
  cda_base, cda_types,
  OIDCache;

type
  TIIEditForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Root: TLabel;
    Extension: TLabel;
    Label1: TLabel;
    cbxOIDs: TComboBox;
    Edit1: TEdit;
    edtExtension: TEdit;
    edtName: TEdit;
    edtRoot: TEdit;
    Label2: TLabel;
    Bevel1: TBevel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FilterOids(Sender: TObject);
    procedure cbxOIDsChange(Sender: TObject);
    procedure edtRootChange(Sender: TObject);
    procedure edtNameChange(Sender: TObject);
    procedure edtExtensionChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    oids : TStringList;
    fii : Tv3II;
    bound : boolean;
    procedure bind;
    procedure Setii(const Value: Tv3II);
    function valid(var s : String):boolean;
  public
    { Public declarations }
    property ii : Tv3II read FIi write Setii;
  end;

var
  IIEditForm: TIIEditForm;

implementation

{$R *.dfm}

procedure TIIEditForm.bind;
begin
  if ii <> nil then
  begin
    edtRoot.Text := ii.root;
    edtExtension.Text := ii.extension;
    edtName.Text := ii.identifierName;
  end
  else
  begin
    edtRoot.Text := '';
    edtExtension.Text := '';
    edtName.Text := '';
  end;
  bound := true;
end;

procedure TIIEditForm.Button2Click(Sender: TObject);
var
  msg : string;
begin
  if Valid(msg) then
    ModalResult := mrOk
  else
    ShowMessage(msg);
end;

procedure TIIEditForm.cbxOIDsChange(Sender: TObject);
var
  s, l, r, t : String;
begin
  if not bound then
    exit;
  StringSplit(cbxOIDs.text, ':', l, s);
  StringSplit(s, '(', r, s);
  StringSplit(s, ')', t, s);
  l := trim(l);
  if isOid(l) then
  begin
    edtRoot.Text := l;
    if (trim(r) = '') then
      edtName.Text := trim(t)
    else
      edtName.Text := trim(r);
  end;
end;

procedure TIIEditForm.edtExtensionChange(Sender: TObject);
begin
  if not bound then
    exit;
  if ii = nil then
    ii := Tv3II.Create;
  ii.extension := edtExtension.text;
end;

procedure TIIEditForm.edtNameChange(Sender: TObject);
begin
  if not bound then
    exit;
  if ii = nil then
    ii := Tv3II.Create;
  ii.identifierName := edtName.text;
end;

procedure TIIEditForm.edtRootChange(Sender: TObject);
begin
  if not bound then
    exit;
  if ii = nil then
    ii := Tv3II.Create;
  ii.root := edtRoot.text;
end;

procedure TIIEditForm.FilterOids(Sender: TObject);
var
  i : integer;
  s : string;
begin
  cbxOIDs.Items.Clear;
  s := lowercase(Edit1.Text);
  for i := 0 to oids.Count - 1 do
    if (s = '') or (pos(s, lowercase(OIds[i])) > 0) then
      cbxOIDs.Items.Add(OIds[i]);
end;

procedure TIIEditForm.FormCreate(Sender: TObject);
begin
  oids := TStringList.Create;
  ReadOids(oids);
  FilterOids(self);
end;

procedure TIIEditForm.FormDestroy(Sender: TObject);
begin
  fii.Free;
  oids.Free;
end;

procedure TIIEditForm.Setii(const Value: Tv3II);
begin
  FIi.Free;
  FIi := Value;
  bind;
end;

function TIIEditForm.valid(var s: String): boolean;
begin
  result := false;
  if (ii.extension <> '') and (ii.root = '') then
    s := 'If an extension is present, there must be a root'
  else
    result := true;
end;

end.
