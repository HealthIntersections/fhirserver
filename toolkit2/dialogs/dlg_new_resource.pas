unit dlg_new_resource;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, IniFiles,
  fsl_base, fsl_http,
  fhir_objects, fhir_parser, fhir_common, fhir_factory,
  fui_lcl_utilities,
  ftk_context;

type
  { TNewResourceDialog }

  TNewResourceDialog = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    cbxVersion: TComboBox;
    cbxResource: TComboBox;
    cbxProfile: TComboBox;
    cbxFormat: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    procedure cbxProfileChange(Sender: TObject);
    procedure cbxResourceChange(Sender: TObject);
    procedure cbxVersionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FContext: TToolkitContext;
    FIniFile: TIniFile;
    FList : TFslList<TFhirStructureDefinitionW>;
    procedure SetContext(AValue: TToolkitContext);

    function makeResource(c : TFHIRWorkerContextWithFactory) : TFHIRResourceV;
  public
    property IniFile : TIniFile read FIniFile write FIniFile;
    property Context : TToolkitContext read FContext write SetContext;

    function version : String;
    function format : String;
    function generate : TBytes;
  end;

var
  NewResourceDialog: TNewResourceDialog;

implementation

{$R *.lfm}

{ TNewResourceDialog }

procedure TNewResourceDialog.FormDestroy(Sender: TObject);
begin
  FList.free;
  FContext.free;
end;

procedure TNewResourceDialog.FormShow(Sender: TObject);
var
  a : TFHIRVersion;
begin
  cbxResource.Text := IniFile.ReadString('new-resource', 'resource', '');
  cbxProfile.Text := IniFile.ReadString('new-resource', 'profile', '');
  cbxFormat.Text := IniFile.ReadString('new-resource', 'format', 'json');

  cbxVersion.Items.Clear;
  for a in TFHIRVersion do
    if context.context[a] <> nil then
      cbxVersion.items.addObject(context.context[a].versionString, TObject(a));
  cbxVersion.itemIndex := cbxVersion.items.IndexOf(IniFile.ReadString('new-resource', 'version', ''));
  if cbxVersion.itemIndex = -1 then
    cbxVersion.itemIndex := cbxVersion.items.count -1;
  cbxVersionChange(nil);
end;

procedure TNewResourceDialog.cbxVersionChange(Sender: TObject);
var
  v : TFHIRVersion;
  rn, s : String;
begin
  v := TFHIRVersion(cbxVersion.items.Objects[cbxVersion.ItemIndex]);
  rn := cbxResource.Text;
  cbxResource.Items.Clear;
  for s in context.context[v].allResourceNames do
    cbxResource.Items.add(s);
  cbxResource.ItemIndex := cbxResource.Items.IndexOf(rn);
  cbxResourceChange(nil);
end;

procedure TNewResourceDialog.FormCreate(Sender: TObject);
begin
  setForOs(btnOk, btnCancel);
  FList := TFslList<TFhirStructureDefinitionW>.Create;
end;

procedure TNewResourceDialog.cbxResourceChange(Sender: TObject);
var
  v : TFHIRVersion;
  rn, pn : String;
  list : TFslList<TFhirStructureDefinitionW>;
  sd : TFhirStructureDefinitionW;
begin
  v := TFHIRVersion(cbxVersion.items.Objects[cbxVersion.ItemIndex]);
  rn := cbxResource.Text;
  pn := cbxProfile.Text;
  cbxProfile.Items.Clear;
  FList.Clear;
  cbxProfile.Items.add('(empty)');
  context.context[v].listStructures(FList);
  for sd in FList do
    if (sd.kind = sdkResource) and (sd.type_ = rn) then
      cbxProfile.Items.addObject(sd.name, sd);
  cbxProfile.ItemIndex := cbxProfile.Items.IndexOf(pn);
  cbxProfileChange(nil);
end;

procedure TNewResourceDialog.cbxProfileChange(Sender: TObject);
begin
  btnOk.enabled := (cbxVersion.ItemIndex > -1) and (cbxResource.ItemIndex > -1) and (cbxProfile.ItemIndex > -1) and (cbxFormat.ItemIndex > -1);
end;

procedure TNewResourceDialog.SetContext(AValue: TToolkitContext);
begin
  FContext.free;
  FContext := AValue;
end;

function TNewResourceDialog.makeResource(c : TFHIRWorkerContextWithFactory) : TFHIRResourceV;
var
  sd : TFhirStructureDefinitionW;
begin
  if cbxProfile.ItemIndex = 0 then
    result := c.Factory.makeResource(cbxResource.Text)
  else
  begin
    sd := cbxProfile.items.Objects[cbxProfile.ItemIndex] as TFhirStructureDefinitionW;
    result := c.Factory.createFromProfile(c, sd);
  end;
end;

function TNewResourceDialog.version: String;
begin
  result := cbxVersion.text;
end;

function TNewResourceDialog.format: String;
begin
  result := cbxFormat.text;
end;

function TNewResourceDialog.generate: TBytes;
var
  c : TFHIRWorkerContextWithFactory;
  v : TFHIRVersion;
  r : TFHIRResourceV;
  p : TFHIRComposer;
begin
  v := TFHIRVersion(cbxVersion.items.Objects[cbxVersion.ItemIndex]);
  c := context.context[v];
  r := makeResource(c);
  try
    if format = 'json' then
      p := c.Factory.makeComposer(c.Link, ffJson, nil, OutputStylePretty)
    else
      p := c.Factory.makeComposer(c.Link, ffJson, nil, OutputStylePretty);
    try
      result := p.ComposeBytes(r);
    finally
      p.free;
    end;
  finally
    r.free;
  end;
end;

end.

