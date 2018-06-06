unit PatientHomeForm;

{
Copyright (c) 2018+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox,
  FHIR.Support.DateTime,
  FHIR.Support.Objects, FHIR.Support.Generics,
  FHIR.Version.Resources, FHIR.Version.Client, FHIR.Version.Utilities,
  BaseFrame, DocumentGenerationForm;

type
  TFrame = TBaseFrame; // re-aliasing the Frame to work around a designer bug

  TPatientHomeFrame = class(TFrame)
    Panel1: TPanel;
    lblName: TLabel;
    Button1: TButton;
    Label1: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label2: TLabel;
    lbCompositions: TListBox;
    Panel4: TPanel;
    lblOutcome: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure lbCompositionsDblClick(Sender: TObject);
  private
    FPatient: TFHIRPatient;
    FCapabilityStatement: TFhirCapabilityStatement;
    FClient: TFHIRClient;

    FResources : TFslMap<TFHIRResource>;
    procedure SetCapabilityStatement(const Value: TFhirCapabilityStatement);
    procedure SetClient(const Value: TFHIRClient);
    procedure SetPatient(const Value: TFHIRPatient);
  public
    destructor Destroy; override;

    property Client : TFHIRClient read FClient write SetClient;
    property CapabilityStatement : TFhirCapabilityStatement read FCapabilityStatement write SetCapabilityStatement;
    property Patient : TFHIRPatient read FPatient write SetPatient;

    procedure load; override;
  end;

implementation

{$R *.fmx}

{ TPatientHomeFrame }

procedure TPatientHomeFrame.Button1Click(Sender: TObject);
begin
  ShowMessage('not done yet');
end;

destructor TPatientHomeFrame.Destroy;
begin
  FResources.free;
  FPatient.Free;
  FCapabilityStatement.Free;
  FClient.Free;
  inherited;
end;

procedure TPatientHomeFrame.lbCompositionsDblClick(Sender: TObject);
var
  form : TDocumentGeneratorForm;
  cmp :  TFHIRComposition;
  doc : TFHIRBundle;
  prac : TFslList<TFHIRPractitioner>;
begin
  cmp := lbCompositions.Items.Objects[lbCompositions.itemIndex] as TFhirComposition;
  prac := TFslList<TFHIRPractitioner>.create;
  try
    doc := nil;
    work('Fetch Document', true, procedure
        var
          bnd : TFHIRBundle;
          be : TFhirBundleEntry;
        begin
          doc := Client.operation(frtComposition, cmp.id, 'document', nil) as TFHIRBundle;
          bnd := Client.search(frtPractitioner, true, '_summary=true');
          try
            for be in bnd.entryList do
              if (be.resource <> nil) and (be.resource is TFhirPractitioner)  then
                prac.Add(be.resource.Link as TFhirPractitioner);
          finally
            bnd.Free;
          end;
        end);
    try
      form := TDocumentGeneratorForm.create(self);
      try
        form.settings := settings.link;
        form.client := Client.link;
        form.patient := Patient.Link;
        form.Document := doc.link;
        form.Authors := prac.link;
        form.ShowModal;
      finally
        form.free;
      end;
    finally
      doc.Free;
    end;
  finally
    prac.free;
  end;
end;

procedure TPatientHomeFrame.load;
var
  bundle : TFhirBundle;
  start : TDateTime;
begin
  inherited;
  lblName.text := gen(patient.nameList[0]);

  if FResources = nil then
    FResources := TFslMap<TFhirResource>.create;

  work('Loading Patient Data', true,
    procedure
    var
      be : TFhirBundleEntry;
    begin
      // clearing all controls
      lbCompositions.Clear;
      FResources.clear;

      bundle := FClient.search(frtComposition, true, 'patient=Patient/'+FPatient.id) as TFhirBundle;
      try
        start := now;
        for be in bundle.entryList do
          if (be.resource <> nil) then
          begin
            FResources.Add(be.resource.fhirType+'/'+be.resource.id, be.resource.Link);
            if be.resource.ResourceType = frtComposition then
              lbCompositions.Items.AddObject((be.resource as TFHIRComposition).summary, be.resource);
          end;
        lblOutcome.Text := 'Fetched '+inttostr(FResources.Count)+' resources in '+describePeriod(now - start);
      finally
        bundle.Free;
      end;
  end);
end;

procedure TPatientHomeFrame.SetCapabilityStatement(const Value: TFhirCapabilityStatement);
begin
  FCapabilityStatement.Free;
  FCapabilityStatement := Value;
end;

procedure TPatientHomeFrame.SetClient(const Value: TFHIRClient);
begin
  FClient.Free;
  FClient := Value;
end;

procedure TPatientHomeFrame.SetPatient(const Value: TFHIRPatient);
begin
  FPatient.Free;
  FPatient := Value;
end;

end.
