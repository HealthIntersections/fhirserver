unit dlg_gender_identity;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn,
  fsl_utilities,
  fui_lcl_utilities,
  fhir_objects, fhir_factory, fhir_common;

type

  { TGenderIdentityDialog }

  TGenderIdentityDialog = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    cbxGenderId: TComboBox;
    dtStart: TDateEdit;
    dtEnd: TDateEdit;
    edtOther: TEdit;
    edtComment: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblErr: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    procedure cbxGenderIdChange(Sender: TObject);
    procedure dtEndChange(Sender: TObject);
    procedure dtStartChange(Sender: TObject);
    procedure edtCommentChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FFactory : TFhirFactory;
    procedure SetFactory(value : TFhirFactory);

  public
    destructor Destroy; override;

    property Factory : TFhirFactory read FFactory write SetFactory;
    procedure load(gid : TFHIRExtensionW);
    procedure save(gid : TFHIRExtensionW);
  end;

var
  GenderIdentityDialog: TGenderIdentityDialog;

implementation

{$R *.lfm}

{ TGenderIdentityDialog }

procedure TGenderIdentityDialog.FormCreate(Sender: TObject);
begin
  setForOs(btnOk, btnCancel);
end;

procedure TGenderIdentityDialog.SetFactory(value: TFhirFactory);
begin
  FFactory.free;
  FFactory := value;
end;

destructor TGenderIdentityDialog.Destroy;
begin
  FFactory.free;
  inherited Destroy;
end;

procedure TGenderIdentityDialog.load(gid: TFHIRExtensionW);
var
  ext : TFHIRExtensionW;
  cc : TFHIRCodeableConceptW;
  p : TFHIRPeriodW;
begin
  edtOther.text := '';
  cbxGenderId.ItemIndex := 0;

  ext := gid.getExtensionW('value');
  try
    if ext <> nil then
    begin
      cc := ext.valueAsCodeableConcept;
      try
        if cc <> nil then
        begin
          if cc.hasCode('http://snomed.info/sct', '446141000124107') then
            cbxGenderId.ItemIndex := 0
          else if cc.hasCode('http://snomed.info/sct', '446151000124109') then
            cbxGenderId.ItemIndex := 1
          else if cc.hasCode('http://snomed.info/sct', '33791000087105') then
            cbxGenderId.ItemIndex := 2
          else if cc.hasCode('http://terminology.hl7.org/CodeSystem/v3-NullFlavor', 'UNK') then
            cbxGenderId.ItemIndex := 3
          else if cc.text <> '' then
          begin
            cbxGenderId.ItemIndex := 4;
            edtOther.text := cc.text;
          end
        end;
      finally
        cc.free;
      end;
    end;
  finally
    ext.free;
  end;
  dtStart.Text := '';
  dtEnd.Text := '';
  ext := gid.getExtensionW('period');
  try
    if ext <> nil then
    begin
      p := ext.valueAsPeriod;
      try
        if p <> nil then
        begin
          if p.start.notNull then
            dtStart.Date := p.start.DateTime;
          if p.end_.notNull then
            dtEnd.Date := p.end_.DateTime;
        end;
      finally
        p.free;
      end;
    end;
  finally
    ext.free;
  end;
  edtComment.Text := '';
  ext := gid.getExtensionW('comment');
  try
    if ext <> nil then
      edtComment.Text := ext.valueAsString;
  finally
    ext.free;
  end;
  lblErr.caption := '';
end;

procedure TGenderIdentityDialog.save(gid: TFHIRExtensionW);
var
  ext : TFHIRExtensionW;
  cc : TFHIRCodeableConceptW;
  p : TFHIRPeriodW;
  v : TFHIRObject;
begin
  if (not gid.hasExtension('value')) then
    gid.addExtensionV('value', factory.makeByName('CodeableConcept'));

  ext := gid.getExtensionW('value');
  try
    cc := ext.valueAsCodeableConcept;
    if cc = nil then
    begin
      cc := factory.wrapCodeableConcept(factory.makeByName('CodeableConcept'));
      ext.setValueW(cc);
    end;
    try
      cc.clearCodings;
      cc.Text := '';
      case cbxGenderId.itemIndex of
        0: cc.addCoding('http://snomed.info/sct', '', '446141000124107', 'Identifies as female gender');
        1: cc.addCoding('http://snomed.info/sct', '', '446151000124109', 'Identifies as male gender ');
        2: cc.addCoding('http://snomed.info/sct', '', '33791000087105', 'Identifies as nonbinary gender');
        3: cc.addCoding('http://terminology.hl7.org/CodeSystem/v3-NullFlavor', '', 'UNK', 'Unknown');
        4: cc.Text := edtOther.Text;
      end;
    finally
      cc.free;
    end;
  finally
    ext.free;
  end;

  if (dtStart.text = '') and (dtEnd.Text = '') then
    gid.deleteExtensionByUrl('period')
  else
  begin
    if (not gid.hasExtension('period')) then
      gid.addExtensionV('period', factory.makeByName('Period'));
    ext := gid.getExtensionW('period');
    try
      p := ext.valueAsPeriod;
      if p = nil then
      begin
        p := factory.wrapPeriod(factory.makeByName('Period'));
        ext.setValueW(p);
      end;
      try
        if (dtStart.text = '') then
          p.start := TFslDateTime.makeNull
        else
          p.start := TFslDateTime.make(dtStart.Date, dttzUnknown);
        if (dtEnd.text = '') then
          p.end_ := TFslDateTime.makeNull
        else
          p.end_ := TFslDateTime.make(dtEnd.Date, dttzUnknown);
      finally
        p.free;
      end;
    finally
      ext.free;
    end;
  end;

  if (edtComment.Text = '') then
    gid.deleteExtensionByUrl('comment')
  else
  begin
    if (not gid.hasExtension('comment')) then
      gid.addExtensionV('comment', factory.makeByName('String'));
    ext := gid.getExtensionW('comment');
    try
      v := ext.makeStringValue(edtComment.text);
      try
        ext.setValueV(v);
      finally
        v.free;
      end;
    finally
      ext.free;
    end;
  end;
end;

procedure TGenderIdentityDialog.cbxGenderIdChange(Sender: TObject);
begin
  edtOther.enabled := cbxGenderId.itemIndex = 4;
  lblErr.caption := '';
end;

procedure TGenderIdentityDialog.dtEndChange(Sender: TObject);
begin
  lblErr.caption := '';
end;

procedure TGenderIdentityDialog.dtStartChange(Sender: TObject);
begin
  lblErr.caption := '';
end;

procedure TGenderIdentityDialog.edtCommentChange(Sender: TObject);
begin
  lblErr.caption := '';
end;

procedure TGenderIdentityDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrCancel then
    CanClose := true
  else
  begin
    CanClose := false;
    if cbxGenderId.itemIndex = -1 then
      lblErr.caption := 'You must choose a gender'
    else if (cbxGenderId.itemIndex = 4) and (edtOther.text = '') then
      lblErr.caption := 'You must provide some alternative text'
    else if (dtStart.Text <> '') and (dtEnd.Text <> '') and (dtStart.Date > dtEnd.Date) then
      lblErr.Caption := 'Start is after End'
    else
      CanClose := true;
  end;
end;

end.

