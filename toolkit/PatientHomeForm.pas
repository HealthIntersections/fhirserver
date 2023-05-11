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
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox, FMX.TabControl,
  FHIR.Ui.Graph, FHIR.Tools.ObsGraph,
  fsl_utilities, FHIR.Ui.Fmx,
  fsl_base, fhir_objects,
  FHIR.Version.Resources, FHIR.Version.Resources.Base, FHIR.Version.Client, FHIR.Version.Utilities, FHIR.Version.Types,
  BaseFrame, DocumentGenerationForm, System.Rtti, FMX.Grid.Style, FMX.Grid,
  FMX.ScrollBox;

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
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    pnlHR: TPanel;
    pnlO2: TPanel;
    pnlBP: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    Label6: TLabel;
    grdName: TGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    StringColumn7: TStringColumn;
    StringColumn8: TStringColumn;
    Panel11: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure lbCompositionsDblClick(Sender: TObject);
    procedure grdNameGetValue(Sender: TObject; const ACol, ARow: Integer;
      var Value: TValue);
  private
    FPatient: TFHIRPatient;
    FCapabilityStatement: TFhirCapabilityStatement;
    FClient: TFHIRClient;
    FgrHR : TFGraph;

    FResources : TFslMap<TFHIRResource>;
    procedure SetCapabilityStatement(const Value: TFhirCapabilityStatement);
    procedure SetClient(const Value: TFHIRClient);
    procedure SetPatient(const Value: TFHIRPatient);

    procedure buildGraphs;
    procedure configureGraph(graph: TFGraph);
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

procedure TPatientHomeFrame.configureGraph(graph : TFGraph);
begin
  graph.Align := TAlignLayout.Client;
  graph.Cursor := 0;
  graph.Visible := True;
{  graph.Plotting := True;
  graph.Dimensions.BottomMargin := 20;
  graph.Dimensions.LeftMargin := 25;
  graph.Dimensions.RightMargin := 10;
  graph.Dimensions.TopMargin := 10;
  graph.Dimensions.TickLength := 4;
  graph.Dimensions.XAxisTitleOffset := 4;
  graph.Dimensions.XAxisLabelOffset := 2;
  graph.Dimensions.YAxisTitleOffset := 10;
  graph.Dimensions.YAxisLabelOffset := 2;
  graph.Dimensions.GraphTitleOffset := 7;
  graph.Dimensions.PrintXOffsetPct := 5;
  graph.Dimensions.PrintYOffsetPct := 20;
  graph.Dimensions.PrintScalePct := 90;
  graph.Appearance.AxesColor := clBlack;
  graph.Appearance.BackgroundColor := clWhite;
  graph.Appearance.MarginColor := clWhite;
  graph.Appearance.PrintBkgndColor := False;
  graph.Appearance.GridColor := clSilver;
  graph.Appearance.GridStyle := psDot;
  graph.Appearance.ShowGraphLabels := True;
  graph.Appearance.PlotOffGraph := False;
  graph.Appearance.ShowMarks := True;
  graph.Appearance.ShowTicks := True;
  graph.Appearance.GraphTitle := '';
  graph.Appearance.MinSteps := 5;
  graph.Appearance.MaxSteps := 50;
  graph.Appearance.TitleFont.Family := 'Tahoma';
  graph.Appearance.TitleFont.Size := 9;
  graph.Appearance.TitleFont.Style := [TFontStyle.fsBold];
  graph.Appearance.CaptionFont.Family := 'Tahoma';
  graph.Appearance.CaptionFont.Size := 8;
  graph.Appearance.CaptionFont.Style := [];
  graph.Appearance.LabelFont.Family := 'Tahoma';
  graph.Appearance.LabelFont.Size := 8;
  graph.Appearance.LabelFont.Style := [];
  graph.Appearance.CrossAtZero := False;
  graph.Appearance.CrossColor := clRed;
  graph.Appearance.Crosslength := 4;
  graph.Appearance.PrintLineStyle := True;
  graph.Appearance.MinPointClearance := 0;
  graph.Appearance.CrossWire := False;
  graph.Legend.visible := False;
  graph.XAxis.Title := '';
  graph.XAxis.LabelDecimals := 0;
  graph.XAxis.AutoLabelDecimals := True;
  graph.XAxis.LogCycleDivisions := 2;
  graph.XAxis.LogScale := False;
  graph.XAxis.Max := 0;
  graph.XAxis.Min := -8;
  graph.XAxis.StepSize := 4;
  graph.XAxis.MinScaleLength := 0;
  graph.XAxis.ShowAsTime := False;
  graph.XAxis.DateTimeFormat := 'dd-mmm';
  graph.XAxis.DateTickType := dt_minute;
  graph.XAxis.ShowAxis := True;
  graph.XAxis.Reversed := False;
  graph.XAxis.OffsetType := ao_Minimum;
  graph.XAxis.Offset := 0;
  graph.XAxis.Gridlines := True;
  graph.XAxis.AutoSizing := False;
  graph.XAxis.AutoStepping := False;
  graph.YAxis1.Title := '';
  graph.YAxis1.LabelDecimals := 0;
  graph.YAxis1.AutoLabelDecimals := False;
  graph.YAxis1.LogCycleDivisions := 2;
  graph.YAxis1.LogScale := False;
  graph.YAxis1.Max := 170;
  graph.YAxis1.Min := 50;
  graph.YAxis1.StepSize := 25;
  graph.YAxis1.MinScaleLength := 0;
  graph.YAxis1.ShowAsTime := False;
  graph.YAxis1.DateTimeFormat := 'dd-mmm';
  graph.YAxis1.DateTickType := dt_minute;
  graph.YAxis1.ShowAxis := True;
  graph.YAxis1.Reversed := False;
  graph.YAxis1.OffsetType := ao_Minimum;
  graph.YAxis1.Offset := 0;
  graph.YAxis1.Gridlines := True;
  graph.YAxis1.AutoSizing := False;
  graph.YAxis1.AutoStepping := False;
  graph.YAxis2.ShowAxis := False;}
end;

procedure TPatientHomeFrame.buildGraphs;
//var
//  dp : TObservationDataProvider;
//  series : TFGraphSeries;
begin
  FgrHR := TFGraph.Create(self);
  pnlHR.AddObject(FgrHR);
  configureGraph(FgrHR);
{  FgrHR.addBand(TAlphaColors.Green, 60, 90, 0.1);
  FgrHR.addBand(TAlphaColors.Orange, 90, 150, 0.1);
  FgrHR.addBand(TAlphaColors.Red, 150, 170, 0.1);

  dp := TObservationDataProvider.Create;
  try
    dp.code := '8867-4';
    dp.server := FClient.address;
    dp.window := 1 / 3;
    dp.SeriesName := 'Heart Rate';
    dp.patientId := Patient.id;
    dp.load;
    series := TFGraphSeries.Create(dp.link);
    try
      series.DrawPoints := true;
      series.PointColor := TAlphaColors.Black;
      series.PointShape := ps_Circle;
      series.PointSize := 2;
      series.FillPoints := true;
      series.Active := true;
      FgrHR.Series.Add(series.Link);
    finally
      series.Free;
    end;
  finally
    dp.Free;
  end;       }
end;

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

function txt(list : TFhirStringList) : String;
var
  s : TFHIRString;
begin
  result := ' ';
  for s in list do
    result := result + s.value+' ';
  result := result.trim;
end;

procedure TPatientHomeFrame.grdNameGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  name : TFHIRHumanName;
begin
  name := patient.nameList[AROw];
  case aCol of
    0: Value := txt(name.prefixList);
    1: Value := name.family;
    2: Value := txt(name.givenList);
    3: Value := txt(name.suffixList);
    4: Value := CODES_TFhirNameUseEnum[name.use];
    5: Value := gen(name.period);
    6: if name.familyElement <> nil then
         Value := name.familyElement.getExtensionString('http://hl7.org/fhir/StructureDefinition/humanname-fathers-family');
    7: if name.familyElement <> nil then
         Value := name.familyElement.getExtensionString('http://hl7.org/fhir/StructureDefinition/humanname-mothers-family')
  end;
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
    work('Fetch Document', true, procedure (context : pointer)
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
//        form.Authors := prac.link;
        ShowModalHack(form);
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
  if FCapabilityStatement.supportsResource('Observation', [fcmdRead, fcmdSearch]) then
    buildGraphs;

  lblName.text := gen(patient.nameList[0]);

  grdName.RowCount := 0;
  grdName.RowCount := patient.nameList.count;

  if FResources = nil then
    FResources := TFslMap<TFhirResource>.create('resources');

  work('Loading Patient Data', true,
    procedure  (context : pointer)
    var
      be : TFhirBundleEntry;
    begin
      // clearing all controls
      lbCompositions.Clear;
      FResources.clear;

      if FCapabilityStatement.supportsResource('Composition', [fcmdRead, fcmdSearch]) then
      begin
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
