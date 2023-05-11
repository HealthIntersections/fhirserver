unit FHIR.Tools.ObsGraph;

interface

uses
  SysUtils, Classes, Generics.Defaults, Math,
  fsl_base, fsl_utilities, fsl_threads,
  FHIR.Ui.Graph,
  fhir_objects, fhir_client,
  fhir_oauth,
  FHIR.Version.Client, FHIR.Version.Resources, FHIR.Version.Types, FHIR.Version.Utilities;

type
  TObservationGraphingContext = class (TFslObject)
  private
    FParams: String;
    FAddress: String;
  public
    constructor Create(address, params : string);
    property address : String read FAddress write FAddress;
    property params : String read FParams write FParams;
  end;

  TFHIRObsNode = class (TFslObject)
  private
    FTime : Double; // duration back from 'now'
    FValue : Double;
    FResource: TFhirObservation;
    FMessage: String;
    procedure SetResource(const Value: TFhirObservation);
  public
    destructor Destroy; override;

    property time : Double read FTime write FTime;
    property value : Double read FValue write FValue;
    property message : String read FMessage write FMessage;
    property resource : TFhirObservation read FResource write SetResource;
  end;

  TObservationGraphRetrievalTask = class (TBackgroundTaskEngine, IComparer<TFHIRObsNode>)
  private
    FClient : TFHIRClient;
    function details : TObservationGraphingContext;
    procedure clientProgress(client : TObject; details : String; pct : integer; done : boolean);
    function processObs(obs : TFhirObservation) : TFHIRObsNode;
    function Compare(const Left, Right: TFHIRObsNode): Integer;
  public
    function name: string; override;
    procedure execute; override;
  end;


  TObservationDataProvider = class (TFGraphDataProvider)
  private
    FTaskId : Integer;
    FCode : String;
    FWindow : TDuration;
    FServer : String;

    FObervations : TFslList<TFHIRObsNode>;
    FDataCount : integer;
    FXmin : Double;
    FXmax : Double;
    FYmin : Double;
    FYmax : Double;
    FHasDuplicates : boolean;
    FSeriesName: String;
    FPatientId: String;

    procedure processData(id : integer; response : TBackgroundTaskPackage);

    function queryString : String;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TObservationDataProvider;

    property server : String read FServer write FServer;
    property code : String read FCode write FCode;
    property window : TDuration read FWindow write FWindow;
    property SeriesName : String read FSeriesName write FSeriesName;
    property patientId : String read FPatientId write FPatientId;

    procedure load;

    function name : String; override;
    function HasDuplicateXValues : boolean; override;
    function count : integer; override;
    function dataCount : integer; override;
    function getPoint( i : integer) : TFGraphDataPoint; override;
    procedure prepare; override;
    function getMinXValue : Double; override;
    function getMaxXValue : Double; override;
    function getMinYValue : Double; override;
    function getMaxYValue : Double; override;
  end;

implementation

{ TFHIRObsNode }

destructor TFHIRObsNode.Destroy;
begin
  FResource.Free;
  inherited;
end;

procedure TFHIRObsNode.SetResource(const Value: TFhirObservation);
begin
  FResource.Free;
  FResource := Value;
end;

{ TObservationDataProvider }

constructor TObservationDataProvider.Create;
begin
  inherited;
  FTaskId := GBackgroundTasks.registerTaskEngine(TObservationGraphRetrievalTask.Create(processData));
end;

destructor TObservationDataProvider.Destroy;
begin
  inherited;
end;

function TObservationDataProvider.count: integer;
begin
  if FObervations = nil then
    result := 0
  else
    result := FObervations.Count;
end;

function TObservationDataProvider.dataCount: integer;
begin
  result := FDataCount;
end;

function TObservationDataProvider.getMaxXValue: Double;
begin
  result := FXmax;
end;

function TObservationDataProvider.getMaxYValue: Double;
begin
  result := FYmax;
end;

function TObservationDataProvider.getMinXValue: Double;
begin
  result := FXmin;
end;

function TObservationDataProvider.getMinYValue: Double;
begin
  result := FYmin;
end;

function TObservationDataProvider.getPoint(i: integer): TFGraphDataPoint;
begin
  result.clear;
  result.error := '';
  result.id := i;
  result.x := FObervations[i].time;
  if FObervations[i].message <> '' then
    result.error := FObervations[i].message
  else
    result.y := FObervations[i].value;
end;

function TObservationDataProvider.HasDuplicateXValues: boolean;
begin
  result := FHasDuplicates;
end;

function TObservationDataProvider.link: TObservationDataProvider;
begin
  result := TObservationDataProvider(inherited link);
end;

function TObservationDataProvider.name: String;
begin
  result := SeriesName;
end;

procedure TObservationDataProvider.prepare;
begin
end;

procedure TObservationDataProvider.processData(id : integer; response : TBackgroundTaskPackage);
var
  i, j : integer;
  n : TFHIRObsNode;
begin
  FObervations.Free;
  FObervations := (response as TFslList<TFHIRObsNode>).Link;

  FDataCount := 0;
  FXmin := NO_VALUE;
  FXmax := NO_VALUE;
  FYmin := NO_VALUE;
  FYmax := NO_VALUE;
  FHasDuplicates := false;

  for i := 0 to FObervations.Count - 1 do
  begin
    if FObervations[i].message = '' then
    begin
      inc(FdataCount);
      if FXmin = NO_VALUE then
        FXmin := FObervations[i].FTime
      else
        FXmin := min(FXmin, FObervations[i].FTime);
      if FXmax = NO_VALUE then
        FXmax := FObervations[i].FTime
      else
        FXmax := max(FXmax, FObervations[i].FTime);

      if FYmin = NO_VALUE then
        FYmin := FObervations[i].FValue
      else
        FYmin := min(FYmin, FObervations[i].FValue);
      if FYmax = NO_VALUE then
        FYmax := FObervations[i].FValue
      else
        FYmax := max(FYmax, FObervations[i].FValue);
    end;
    if not FHasDuplicates then
    begin
      for j := i + 1 to FObervations.Count - 1 do
        if FObervations[i].time = FObervations[j].time then
          FHasDuplicates := true;
    end;
  end;
  Change;
end;

function TObservationDataProvider.queryString: String;
begin
  result := 'patient:patient='+patientId+'&code='+code+'&_sort=date&date=gt'+(TFslDateTime.makeUTC-window-1).fixPrecision(dtpSec).toXML;
end;

procedure TObservationDataProvider.load;
begin
  GBackgroundTasks.queueTask(FTaskId, TObservationGraphingContext.Create(server, queryString));
end;

{ TObservationGraphRetrievalTask }

function TObservationGraphRetrievalTask.details: TObservationGraphingContext;
begin
  result := request as TObservationGraphingContext;
end;

procedure TObservationGraphRetrievalTask.execute;
var
  bnd : TFhirBundle;
  proc  : TFslList<TFHIRObsNode>;
  be : TFhirBundleEntry;
  node : TFHIRObsNode;
begin
  if (FClient = nil) or (FClient.address <> details.address) then
  begin
    FClient.Free;
    FClient := TFhirClient.create(nil, THTTPLanguages.create('en'), TFHIRHTTPCommunicator.Create(details.address));
    FClient.format := ffJson;
    FClient.onProgress := clientProgress;
//    FClient.smartToken := todo.........
  end;

  writeln('Query: '+fclient.address+'/Observation?'+details.params);
  try
    bnd := FClient.search(frtObservation, true, details.params);
    try
      proc := TFslList<TFHIRObsNode>.create;
      try
        writeln('.. found : '+inttostr(bnd.entryList.Count));
        for be in bnd.entryList do
          if ((be.search = nil) or (be.search.mode = SearchEntryModeMatch)) and (be.resource is TFhirObservation) then
          begin
            node := processObs(be.resource as TFhirObservation);
            if node <> nil then
              proc.Add(node);
          end;
        proc.Sort(self);

        Response := proc.link;
      finally
        proc.Free;
      end;
    finally
      bnd.Free;
    end;
  except
    on e : exception do
      writeln('.. failed: '+e.Message);
  end;
end;

procedure TObservationGraphRetrievalTask.clientProgress(client: TObject; details: String; pct: integer; done: boolean);
begin
  progress(details, pct);
end;

function TObservationGraphRetrievalTask.name: string;
begin
  result := 'Observation Query';
end;

function TObservationGraphRetrievalTask.processObs(obs: TFhirObservation): TFHIRObsNode;
var
  t : TDateTime;
begin
  t := 0;
  // time - we *must* find a time, or we ignore the obs
  if obs.effective is TFhirDateTime then
    t := (obs.effective as TFhirDateTime).value.Local.DateTime
  else if obs.effective is TFhirPeriod then
    t := (obs.effective as TFhirPeriod).point;
  if t = 0 then
    exit(nil);

//  if (obs.value is TFhirQuantity) and ((obs.value as TFhirQuantity).valueElement <> nil) then
//    if TFslDecimal.ValueOf((obs.value as TFhirQuantity).value).AsDouble >= 100 then
//      exit(nil);

  result := TFHIRObsNode.Create;
  try
    result.resource := obs.Link;
    result.time := (t - now) * HoursPerDay;
    if obs.value = nil then
    begin
      if obs.comment <> '' then
        result.message := obs.comment
      else
        result.message := 'No Value Provided'
    end
    else if obs.value is TFhirDecimal then
      result.value := TFslDecimal.ValueOf((obs.value as TFhirDecimal).value).AsDouble
    else if (obs.value is TFhirQuantity) and ((obs.value as TFhirQuantity).valueElement <> nil) then
      result.value := TFslDecimal.ValueOf((obs.value as TFhirQuantity).value).AsDouble
    else
      result.message := gen(obs.value);

    result.Link;
  finally
    result.Free;
  end;
end;

function TObservationGraphRetrievalTask.Compare(const Left, Right: TFHIRObsNode): Integer;
begin
  if left.FTime > right.time then
    result := -1
  else if left.FTime < right.time then
    result := 1
  else
    result := 0;
end;


{ TObservationGraphingContext }

constructor TObservationGraphingContext.create(address, params: string);
begin
  inherited create;
  FAddress := address;
  FParams := params;
end;

end.
