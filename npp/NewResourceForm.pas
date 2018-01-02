unit NewResourceForm;


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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  nppforms,
  FHIRBase, FHIRResources, FHIRTypes, FHIRPluginValidator, FHIRProfileUtilities, FHIRParserBase, FHIRParser, FHIRContext;

type
  TResourceNewForm = class(TNppForm)
    PageControl1: TPageControl;
    Panel1: TPanel;
    tbResources: TTabSheet;
    tbProfiles: TTabSheet;
    lbResources: TListBox;
    lbProfiles: TListBox;
    Label1: TLabel;
    edtFilter: TEdit;
    btnCreate: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Label2: TLabel;
    rbJson: TRadioButton;
    rbXml: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure lbResourcesClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure lbProfilesClick(Sender: TObject);
  private
    { Private declarations }
    FContext : TFHIRWorkerContext;
    procedure loadLists;
    procedure SetContext(const Value: TFHIRWorkerContext);
  public
    { Public declarations }
    destructor Destroy; override;

    property Context : TFHIRWorkerContext read FContext write SetContext;
  end;

var
  ResourceNewForm: TResourceNewForm;

implementation

{$R *.dfm}

Uses
  FhirPlugin;

procedure TResourceNewForm.btnCreateClick(Sender: TObject);
var
  sd : TFhirStructureDefinition;
  pu : TProfileUtilities;
  res : TFhirResource;
  comp : TFHIRComposer;
  s : TStringStream;
begin
  if PageControl1.ActivePageIndex = 0 then
    sd := lbResources.items.objects[lbResources.ItemIndex] as TFhirStructureDefinition
  else
    sd := lbProfiles.items.objects[lbProfiles.ItemIndex] as TFhirStructureDefinition;
  pu := TProfileUtilities.create(FContext.Link, nil);
  try
    res := pu.populateByProfile(sd);
    try
      if rbJson.Checked then
        comp := TFHIRJsonComposer.Create(FContext.link, OutputStylePretty, 'en')
      else
        comp := TFHIRXmlComposer.Create(FContext.link, OutputStylePretty, 'en');
      try
        s := TStringStream.Create;
        try
          comp.Compose(s, res);
          Npp.NewFile(s.DataString);
        finally
          s.Free;
        end;
      finally
        comp.Free;
      end;
    finally
      res.Free;
    end;
  finally
    pu.Free;
  end;
  ModalResult := mrOK;
end;

destructor TResourceNewForm.Destroy;
begin
  FContext.Free;
  inherited;
end;

procedure TResourceNewForm.SetContext(const Value: TFHIRWorkerContext);
begin
  FContext.Free;
  FContext := Value;
end;

procedure TResourceNewForm.edtFilterChange(Sender: TObject);
begin
  loadLists;
end;

procedure TResourceNewForm.FormShow(Sender: TObject);
begin
  LoadLists;
end;

procedure TResourceNewForm.lbProfilesClick(Sender: TObject);
begin
  btnCreate.Enabled := lbProfiles.ItemIndex > -1;
end;

procedure TResourceNewForm.lbResourcesClick(Sender: TObject);
begin
  btnCreate.Enabled := lbResources.ItemIndex > -1;
end;

procedure TResourceNewForm.loadLists;
var
  sd : TFhirStructureDefinition;
  s : String;
begin
  lbResources.Clear;
  lbProfiles.Clear;
  s := edtFilter.Text;
  s := s.toLower;
  for sd in TFHIRPluginValidatorContext(FContext).Profiles.ProfilesByURL.Values do
    if (sd.kind = StructureDefinitionKindResource) and ((edtFilter.Text = '') or sd.name.ToLower.Contains(s)) then
      if sd.derivation = TypeDerivationRuleSpecialization then
        lbResources.Items.AddObject(sd.name, sd)
      else
        lbProfiles.Items.AddObject(sd.name, sd)
end;

end.
