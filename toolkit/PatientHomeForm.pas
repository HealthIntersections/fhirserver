unit PatientHomeForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox,
  DateSupport,
  AdvObjects, AdvGenerics,
  FHIRResources, FHIRClient, FHIRUtilities,
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

    FResources : TAdvMap<TFHIRResource>;
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
  prac : TAdvList<TFHIRPractitioner>;
begin
  cmp := lbCompositions.Items.Objects[lbCompositions.itemIndex] as TFhirComposition;
  prac := TAdvList<TFHIRPractitioner>.create;
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
    FResources := TAdvMap<TFhirResource>.create;

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
