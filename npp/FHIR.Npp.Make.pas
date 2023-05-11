unit FHIR.Npp.Make;


{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  FHIR.Npp.Form,
  fsl_base,
  fsl_npm, fsl_npm_cache,
  fhir_objects, fhir_parser, fhir_common,
  FHIR.Npp.Context;

type
  TResourceNewForm = class(TNppForm)
    Panel1: TPanel;
    Label1: TLabel;
    edtFilter: TEdit;
    btnCreate: TButton;
    Button2: TButton;
    Panel2: TPanel;
    rbXml: TRadioButton;
    rbJson: TRadioButton;
    lbProfiles: TListBox;
    Label2: TLabel;
    cbxVersion: TComboBox;
    Label3: TLabel;
    cbxPackage: TComboBox;
    Label4: TLabel;
    procedure btnCreateClick(Sender: TObject);
    procedure lbProfilesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbxVersionChange(Sender: TObject);
    procedure cbxPackageChange(Sender: TObject);
  private
    FContext : TFHIRNppContext;
    FList : TFslList<TFhirStructureDefinitionW>;
    procedure populateVersions;
    procedure SetContext(const Value: TFHIRNppContext);
    procedure loadItem(rType, id : String; stream : TStream);
  public
    destructor Destroy; override;

    property Context : TFHIRNppContext read FContext write SetContext;
  end;

var
  ResourceNewForm: TResourceNewForm;

implementation

{$R *.dfm}

Uses
  FHIR.Npp.Plugin;

procedure TResourceNewForm.btnCreateClick(Sender: TObject);
var
  v : TFHIRVersion;
  fmt : TFHIRFormat;
  f : TFHIRNppVersionFactory;
  sd : TFhirStructureDefinitionW;
  r : TFHIRResourceV;
  c : TFHIRComposer;
begin
  v := TFHIRVersion(cbxVersion.Items.Objects[cbxVersion.ItemIndex]);
  if rbXml.Checked then
    fmt := ffXml
  else
    fmt := ffJson;
  f := FContext.Version[v];
  sd := lbProfiles.Items.Objects[lbProfiles.ItemIndex] as TFhirStructureDefinitionW;
  r := f.Factory.createFromProfile(f.Worker, sd);
  try
    c := f.Factory.makeComposer(f.Worker.link, fmt, f.Worker.Create.lang, OutputStylePretty);
    try
      FNpp.newResource(c.Compose(r), v, fmt, '');
    finally
      c.Free;
    end;
  finally
    r.Free;
  end;
  ModalResult := mrOK;
end;

destructor TResourceNewForm.Destroy;
begin
  FContext.Free;
  FList.Free;
  inherited;
end;

procedure TResourceNewForm.FormShow(Sender: TObject);
begin
  populateVersions;
end;

procedure TResourceNewForm.SetContext(const Value: TFHIRNppContext);
begin
  FContext.Free;
  FContext := Value;
end;

procedure TResourceNewForm.lbProfilesClick(Sender: TObject);
begin
  btnCreate.Enabled := lbProfiles.ItemIndex > -1;
end;

procedure TResourceNewForm.loadItem(rType, id: String; stream: TStream);
var
  f : TFHIRNppVersionFactory;
  p : TFHIRParser;
  r : TFHIRResourceV;
begin
  f := FContext.Version[TFHIRVersion(cbxVersion.Items.Objects[cbxVersion.ItemIndex])];
  p := f.makeParser(ffJson);
  try
    r := p.parseResource(stream);
    try
      Flist.add(f.Factory.wrapStructureDefinition(r.link));
    finally
      r.Free;
    end;
  finally
    p.Free;
  end;
end;

procedure TResourceNewForm.populateVersions;
var
  a : TFHIRVersion;
begin
  cbxVersion.Items.clear;
  for a in SUPPORTED_VERSIONS do
    if (FContext.VersionLoading[a] = vlsLoaded) then
      cbxVersion.Items.addObject(CODES_TFHIRVersion[a], TObject(a));
  cbxVersion.ItemIndex := cbxVersion.Items.Count - 1;
  cbxVersionChange(nil);
end;

procedure TResourceNewForm.cbxVersionChange(Sender: TObject);
var
  pl : TFslList<TNpmPackage>;
  p : TNpmPackage;
begin
  if cbxVersion.ItemIndex = -1 then
  begin
    cbxPackage.Items.Clear;
    cbxPackage.ItemIndex := -1;
  end
  else
  begin
    pl := TFslList<TNpmPackage>.create;
    try
      FContext.Cache.ListPackages([fpkCore, fpkIG], pl);
      for p in pl do
        if p.version = FHIR_VERSIONS[TFHIRVersion(cbxVersion.Items.Objects[cbxVersion.ItemIndex])] then
          cbxPackage.Items.AddObject(p.summary, p);
    finally
      pl.Free;
    end;
    cbxPackage.ItemIndex := 0;
  end;
  cbxPackageChange(nil);
end;

procedure TResourceNewForm.cbxPackageChange(Sender: TObject);
var
  sd : TFhirStructureDefinitionW;
  li : TPackageLoadingInformation;
begin
  if FList = nil then
    FList := TFslList<TFhirStructureDefinitionW>.create;
  FList.Clear;
  lbProfiles.Items.Clear;
  lbProfiles.ItemIndex := -1;
  if cbxPackage.ItemIndex > -1 then
  begin
    if cbxPackage.Items[cbxPackage.ItemIndex].startsWith('hl7.fhir.core-') then
      Context.Version[TFHIRVersion(cbxVersion.Items.Objects[cbxVersion.ItemIndex])].Worker.listStructures(FList)
    else
    begin
      li := FContext.LoadInfo[TFHIRVersion(cbxVersion.Items.Objects[cbxVersion.ItemIndex])];
      li.OnLoadEvent := loadItem;
      FContext.Cache.loadPackage(cbxPackage.Items[cbxPackage.ItemIndex], ['StructureDefinition'], li);
    end;
    for sd in FList do
      if sd.kind = sdkResource then
        lbProfiles.Items.AddObject(sd.name+' ('+sd.url+')', sd);
  end;
  lbProfilesClick(nil);
end;


end.
