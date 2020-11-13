unit DocumentGenerationForm;

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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.ListBox, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.TreeView,
  FMX.WebBrowser, FMX.TabControl, FMX.Controls.Presentation,
  fsl_base,
  fhir_objects, FHIR.Version.Types, FHIR.Version.Resources, FHIR.Version.Resources.Base, FHIR.Version.Utilities, FHIR.Version.Client,
  ToolkitSettings, System.ImageList, FMX.ImgList, FMX.Memo.Types;

type
  TDocumentGeneratorForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Button1: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    WebBrowser1: TWebBrowser;
    tvStructure: TTreeView;
    mSource: TMemo;
    Panel4: TPanel;
    btnSave: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edCertificate: TEdit;
    btnCertificate: TButton;
    Label4: TLabel;
    cbxSignature: TComboBox;
    edPassphrase: TEdit;
    cbRemember: TCheckBox;
    od: TOpenDialog;
    btnPost: TButton;
    Label5: TLabel;
    cbxMode: TComboBox;
    Label6: TLabel;
    cbxIdentity: TComboBox;
    Label7: TLabel;
    cbxFormat: TComboBox;
    sd: TSaveDialog;
    ImageList1: TImageList;
    procedure FormShow(Sender: TObject);
    procedure btnCertificateClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnPostClick(Sender: TObject);
    procedure cbxFormatChange(Sender: TObject);
  private
    FPatient: TFHIRPatient;
    FClient: TFHIRClient;
    FBundle: TFhirBundle;
    FSettings: TFHIRToolkitSettings;
//    FAuthors: TFslList<TFHIRPractitioner>;
    FDocumentReference: TFhirDocumentReference;
    FProvenance: TFhirProvenance;
    procedure SetClient(const Value: TFHIRClient);
    procedure SetPatient(const Value: TFHIRPatient);
    procedure SetBundle(const Value: TFhirBundle);
    procedure SetSettings(const Value: TFHIRToolkitSettings);

    procedure LoadDocumentStructure;
    procedure loadComposition(doc : TTreeViewItem; cmp : TFHIRComposition; refList : TFslList<TFhirResource>);
    procedure Remember;
    procedure Prepare;
//    procedure SetAuthors(const Value: TFslList<TFHIRPractitioner>);
    procedure SetDocumentReference(const Value: TFhirDocumentReference);
    procedure SetProvenance(const Value: TFhirProvenance);
  public
    destructor Destroy; override;

    property Settings : TFHIRToolkitSettings read FSettings write SetSettings;
    property Client : TFHIRClient read FClient write SetClient;
    property Document : TFhirBundle read FBundle write SetBundle;
    property Provenance : TFhirProvenance read FProvenance write SetProvenance;
    property DocumentReference : TFhirDocumentReference read FDocumentReference write SetDocumentReference;
    property Patient : TFHIRPatient read FPatient write SetPatient;
//    property Authors : TFslList<TFHIRPractitioner> read FAuthors write SetAuthors;
  end;

var
  DocumentGeneratorForm: TDocumentGeneratorForm;

implementation

{$R *.fmx}


{ TDocumentGeneratorForm }

procedure TDocumentGeneratorForm.btnCertificateClick(Sender: TObject);
begin
  if od.Execute then
    edCertificate.Text := od.FileName;
end;

procedure TDocumentGeneratorForm.btnPostClick(Sender: TObject);
var
  docid, refid, provid : String;
  binary : TFhirBinary;
  s : String;
begin
  Remember;
  Prepare;
  s := 'Created Resources'+#13#10;
  binary := TFhirBinary.Create;
  try
    if cbxFormat.ItemIndex = 0 then
    begin
      binary.content := resourceToBytes(Document, ffXml, OutputStylePretty);
      binary.contentType := 'application/fhir+xml'
    end
    else
    begin
      binary.content := resourceToBytes(Document, ffJson, OutputStylePretty);
      binary.contentType := 'application/fhir+json'
    end;
    client.createResource(binary, docid);
    s := s + '  Document: '+Client.address+'/Binary/'+docid+#13#10;
  finally
    binary.free;
  end;
  if DocumentReference <> nil then
  begin
    with DocumentReference.contentList.Append do
    begin
      Attachment := TFHIRAttachment.Create;
      attachment.url := Client.address+'/Binary/'+docid;
      if cbxFormat.ItemIndex = 0 then
        attachment.contentType := 'application/fhir+xml'
      else
        attachment.contentType := 'application/fhir+json';
    end;
    client.createResource(DocumentReference, refid);
    s := s + '  DocumentReference: '+Client.address+'/DocumentReference/'+refid+#13#10;
  end;
  if Provenance <> nil then
  begin
    Provenance.targetList.Append.reference := Client.address+'/Binary/'+docid;
    client.createResource(Provenance, provid);
    s := s + '  Provenance: '+Client.address+'/Provenance/'+provid+#13#10;
  end;
  showmessage(s);
  ModalResult := mrOk;
end;

procedure TDocumentGeneratorForm.btnSaveClick(Sender: TObject);
begin
  if sd.execute then
  begin
    Remember;
    Prepare;
    if cbxFormat.ItemIndex = 0 then
      ResourceToFile(Document, sd.filename, ffXml, OutputStylePretty)
    else
      ResourceToFile(Document, sd.filename, ffJson, OutputStylePretty);
    if DocumentReference <> nil then
    begin
      with DocumentReference.contentList.Append do
      begin
        Attachment := TFHIRAttachment.Create;
        attachment.url := sd.FileName;
        if cbxFormat.ItemIndex = 0 then
        begin
          attachment.contentType := 'application/fhir+xml';
          ResourceToFile(DocumentReference, ChangeFileExt(sd.filename, '.reference.xml'), ffXml, OutputStylePretty);
        end
        else
        begin
          attachment.contentType := 'application/fhir+json';
          ResourceToFile(DocumentReference, ChangeFileExt(sd.filename, '.reference.json'), ffXml, OutputStylePretty);
        end;
      end;
    end;
    if Provenance <> nil then
    begin
      Provenance.targetList.Append.reference := sd.FileName;
      if cbxFormat.ItemIndex = 0 then
        ResourceToFile(Provenance, ChangeFileExt(sd.filename, '.provenance.xml'), ffXml, OutputStylePretty)
      else
        ResourceToFile(Provenance, ChangeFileExt(sd.filename, '.provenance.json'), ffXml, OutputStylePretty);
    end;
  end;
  ModalResult := mrOk;
end;

procedure TDocumentGeneratorForm.cbxFormatChange(Sender: TObject);
begin
  if cbxFormat.ItemIndex = 0 then
    mSource.Text := resourceToString(document, ffXml, OutputStylePretty)
  else
    mSource.Text := resourceToString(document, ffJson, OutputStylePretty);
end;

destructor TDocumentGeneratorForm.Destroy;
begin
  FProvenance.Free;
  FDocumentReference.Free;
  //FAuthors.Free;
  FSettings.Free;
  FPatient.Free;
  FClient.Free;
  FBundle.Free;
  inherited;
end;

procedure TDocumentGeneratorForm.FormShow(Sender: TObject);
var
  p : TFhirPractitioner;
  pid : String;
begin
  cbxSignature.ItemIndex := Settings.getValue('documents', 'signature', 0);
  edCertificate.Text := Settings.getValue('documents', 'certificate', '');
  cbRemember.IsChecked := Settings.getValue('documents', 'persistPassphrase', false);
  if cbRemember.IsChecked then
    edPassphrase.Text := Settings.getValue('documents', 'passphrase', '');
  cbxMode.ItemIndex := Settings.getValue('documents', 'mode', 0);
  cbxFormat.ItemIndex := Settings.getValue('documents', 'format', 0);

  WebBrowser1.LoadFromStrings(document.generatePresentation, 'my.html');
  if cbxFormat.ItemIndex = 0 then
    mSource.Text := resourceToString(document, ffXml, OutputStylePretty)
  else
    mSource.Text := resourceToString(document, ffJson, OutputStylePretty);
  LoadDocumentStructure;

  cbxIdentity.Items.Clear;
  pid := Settings.getValue('documents', 'author', '');
//  for p in FAuthors do
//  begin
//    cbxIdentity.Items.AddObject(p.display, p);
//    if (p.id = pid) then
//      cbxIdentity.ItemIndex := cbxIdentity.Items.Count - 1;
//  end;
  if cbxIdentity.ItemIndex = -1 then
    cbxIdentity.ItemIndex := 0;
end;

procedure TDocumentGeneratorForm.loadComposition(doc : TTreeViewItem; cmp : TFHIRComposition; refList : TFslList<TFhirResource>);
  procedure addItem(name : String; ref : TFhirReference; mand : boolean; p : TTreeViewItem);
  var
    s : String;
    ti : TTreeViewItem;
    r : TFHIRResource;
  begin
    if (ref <> nil) or mand then
    begin
      if (ref = nil) then
        s := ' (Mandatory Reference not present)'
      else
      begin
        r := Document.findResource(ref);
        if (r = nil) then
          s := ' (Reference not found in Document)'
        else
        begin
          s := '';
          refList.Add(r.Link);
        end;
      end;
      ti := TTreeViewItem.Create(tvStructure);
      if ref = nil then
        ti.text := name+': nil'
      else
        ti.text := name+': '+ref.reference+s;
      if s = '' then
        ti.ImageIndex := 1
      else
        ti.ImageIndex := 2;
      p.AddObject(ti);
      ti.TagObject := ref;
    end;
  end;
  function addGroup(name : String; p : TTreeViewItem) : TTreeViewItem;
  var
    ti : TTreeViewItem;
  begin
    ti := TTreeViewItem.Create(tvStructure);
    ti.text := name ;
    ti.ImageIndex := 4;
    p.AddObject(ti);
    result := ti;
  end;
  procedure addSection(section : TFHIRCompositionSection; p : TTreeViewItem);
  var
    ti : TTreeViewItem;
    sect : TFHIRCompositionSection;
    entry : TFhirReference;
  begin
    ti := TTreeViewItem.Create(tvStructure);
    ti.text := 'Section: '+section.display;
    ti.ImageIndex := 4;
    p.AddObject(ti);
    for entry in section.entryList do
      addItem('entry', entry, false, ti);
    for sect in section.sectionList do
      addSection(sect, ti);
  end;
var
  g, gc : TTreeViewItem;
  att : TFhirCompositionAttester;
  ev : TFhirCompositionEvent;
  s : String;
  a : TFhirCompositionAttestationModeEnum;
  cc : TFHIRCodeableConcept;
  ref : TFHIRReference;
  sect : TFHIRCompositionSection;
begin
  addItem('subject', cmp.subject, true, doc);
  addItem('encounter', cmp.encounter, false, doc);
  addItem('author', cmp.encounter, true, doc);
  if cmp.attesterList.Count > 0 then
  begin
    g := addGroup('attesters', doc);
    for att in cmp.attesterList do
    begin
      s := '';
      for a := low(TFhirCompositionAttestationModeEnum) to high(TFhirCompositionAttestationModeEnum) do
        {$IFDEF FHIR4}
        if a = att.mode then
        {$ELSE}
        if a in att.mode then
        {$ENDIF}
          s := s + ','+CODES_TFhirCompositionAttestationModeEnum[a];
      addItem(s.substring(1), att.party, false, g);
    end;
  end;
  addItem('custodian', cmp.custodian, false, doc);
  if cmp.eventList.Count > 0 then
  begin
    g := addGroup('events', doc);
    for ev in cmp.eventList do
    begin
      s := '';
      for cc in ev.codeList do
        s := s + ','+gen(cc);
      if ev.detailList.Count > 0 then
      begin
        gc := addGroup(s.Substring(1), g);
        for ref in ev.detailList do
        begin
          addItem('reference', ref, false, gc);
        end;
      end;
    end;
  end;
  for sect in cmp.sectionList do
    addSection(sect, doc);
end;

procedure TDocumentGeneratorForm.LoadDocumentStructure;
var
  bi, ei : TTreeViewItem;
  be : TFhirBundleEntry;
  first : boolean;
  refList : TFslList<TFhirResource>;
begin
  bi := TTreeViewItem.Create(tvStructure);
  bi.ImageIndex := 4;
  bi.text := 'Document ('+Document.id+')';
  tvStructure.AddObject(bi);
  bi.TagObject := Document;

  refList := TFslList<TFhirResource>.create;
  try
    first := true;
    for be in Document.entryList do
    begin
      if be.resource <> nil then
      begin
        ei := TTreeViewItem.Create(tvStructure);
        if be.fullUrl <> '' then
          ei.text := be.resource.fhirType+' '+be.resource.id+' ('+be.fullUrl+')'
        else
          ei.text := be.resource.fhirType+' '+be.resource.id;
        bi.AddObject(ei);
        ei.TagObject := be.resource;
        if first then
        begin
          first := false;
          if be.resource.ResourceType = frtComposition then
          begin
            loadComposition(ei, be.resource as TFhirComposition, refList);
            ei.ImageIndex := 0;
          end
          else
            ei.ImageIndex := 3
        end
        else if refList.Contains(be.resource) then
          ei.ImageIndex := 1
        else if be.resource is TFhirProvenance then
          ei.ImageIndex := 5
        else
        begin
          ei.Text := ei.Text+' (not referenced from the Composition)';
          ei.ImageIndex := 6
        end;
      end;
    end;
  finally
    refList.Free;
  end;
  tvStructure.ExpandAll;
end;

procedure TDocumentGeneratorForm.Prepare;
begin
  case cbxSignature.itemIndex of
    1:
      begin
        if cbxFormat.ItemIndex = 0 then
          document.signRef(TSignatureType(cbxSignature.ItemIndex),
            'Practitioner/'+(cbxIdentity.Items.Objects[cbxIdentity.ItemIndex] as TFhirResource).id,
            ffXml, edCertificate.text)
        else
          document.signRef(TSignatureType(cbxSignature.ItemIndex),
            'Practitioner/'+(cbxIdentity.Items.Objects[cbxIdentity.ItemIndex] as TFhirResource).id,
            ffJson, edCertificate.text)
      end;
    2:begin
        if cbxFormat.ItemIndex = 0 then
          Provenance := document.signRef2Provenance(TSignatureType(cbxSignature.ItemIndex),
            'Practitioner/'+(cbxIdentity.Items.Objects[cbxIdentity.ItemIndex] as TFhirResource).id,
            ffXml, edCertificate.text)
        else
          Provenance := document.signRef2Provenance(TSignatureType(cbxSignature.ItemIndex),
            'Practitioner/'+(cbxIdentity.Items.Objects[cbxIdentity.ItemIndex] as TFhirResource).id,
            ffJson, edCertificate.text)
      end;
  else
    ;
  end;
  DocumentReference := Document.AsReference;
end;

procedure TDocumentGeneratorForm.Remember;
var
  p : TFhirPractitioner;
  pid : String;
begin
  Settings.storeValue('documents', 'signature', cbxSignature.ItemIndex);
  Settings.storeValue('documents', 'certificate', edCertificate.Text);
  Settings.storeValue('documents', 'persistPassphrase', cbRemember.IsChecked);
  if cbRemember.IsChecked then
    Settings.storeValue('documents', 'passphrase', edPassphrase.Text);
  Settings.storeValue('documents', 'mode', cbxMode.ItemIndex);
  Settings.StoreValue('documents', 'format', cbxFormat.ItemIndex);

  if cbxIdentity.ItemIndex > -1 then
    Settings.storeValue('documents', 'author', (cbxIdentity.Items.Objects[cbxIdentity.ItemIndex] as TFhirResource).id);
end;

//procedure TDocumentGeneratorForm.SetAuthors(const Value: TFslList<TFHIRPractitioner>);
//begin
//  FAuthors.Free;
//  FAuthors := Value;
//end;
//
procedure TDocumentGeneratorForm.SetBundle(const Value: TFhirBundle);
begin
  FBundle.Free;
  FBundle := Value;
end;

procedure TDocumentGeneratorForm.SetClient(const Value: TFHIRClient);
begin
  FClient.Free;
  FClient := Value;
end;

procedure TDocumentGeneratorForm.SetDocumentReference(const Value: TFhirDocumentReference);
begin
  FDocumentReference.Free;
  FDocumentReference := Value;
end;

procedure TDocumentGeneratorForm.SetPatient(const Value: TFHIRPatient);
begin
  FPatient.Free;
  FPatient := Value;
end;

procedure TDocumentGeneratorForm.SetProvenance(const Value: TFhirProvenance);
begin
  FProvenance.Free;
  FProvenance := Value;
end;

procedure TDocumentGeneratorForm.SetSettings(const Value: TFHIRToolkitSettings);
begin
  FSettings.Free;
  FSettings := Value;
end;

end.
