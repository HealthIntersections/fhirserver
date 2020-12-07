{

spec to do:
- clarify the name of the statistics parameter
- security on observations
}
unit obsservation_stats;

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

{$I fhir.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  fsl_base, fsl_utilities,
  fdb_manager,
  fhir_objects,  fhir_factory, fhir_common;

type
  TObservationStatsParameter = (
      osp_average, osp_maximum, osp_minimum, osp_count, osp_totalcount, osp_median, osp_std_dev, osp_sum, osp_variance,
      osp_pct20, osp_pct80, osp_4_lower, osp_4_upper, osp_4_dev, osp_5_1, osp_5_2, osp_5_3, osp_5_4, osp_skew, osp_kurtosis, osp_regression, osp_intercept);
  TObservationStatsParameterSet = set of TObservationStatsParameter;

  TObservationStatsMode = (osmNative, osmCanonical, osmInconsistent);

Const
  CODES_TObservationStatsParameter : Array [TObservationStatsParameter] of String = ('average', 'maximum', 'minimum', 'count', 'totalcount', 'median', 'std-dev', 'sum', 'variance', '%20', '%80', '4-lower', '4-upper', '4-dev', '5-1', '5-2', '5-3', '5-4', 'skew', 'kurtosis', 'regression', #1);
  UNITS_TObservationStatsParameter : Array [TObservationStatsParameter] of String = ('*',       '*',       '*',       '1',     '1',          '*',      '',        '*',   '',         '*',   '*',   '*',       '*',       '*',     '*',   '*',   '*',   '*',   '',     '',         '*/d',       '*');
  HUNITS_TObservationStatsParameter : Array [TObservationStatsParameter] of String =('*',       '*',       '*',       '',      '',           '*',      '',        '*',   '',         '*',   '*',   '*',       '*',       '*',     '*',   '*',   '*',   '*',   '',     '',         '',           '');

Type
  TObservation = class (TFslObject)
  private
    Key : integer;
    ConceptKey : integer;
    dateTime : TDateTime;
    value : Double;
    vunit : Integer;
    canonical : Double;
    cunit : Integer;
    vConcept:Integer;
    function v(mode : TObservationStatsMode): Double;
  end;

  TObservationStatsComparer = class (TFslComparer<TObservation>)
  public
    function Compare(const Left, Right: TObservation): Integer; override;
  end;

  TObservationStatsEvaluator = class (TFslObject)
  private
    FFactory : TFHIRFactory;
    FConn : TFDBConnection;
    FSubject: String;
    FConcepts : TFslList<TFHIRCodingW>;
    FFinish: TDateTime;
    FStart: TDateTime;
    FParameters: TObservationStatsParameterSet;
    FResp: TFHIRStatsOpResponseW;
    FSubjectKey: integer;
    FAllData : TFslList<TObservation>; // by date
    FValidData : TFslList<TObservation>; // by date
    FMode : TObservationStatsMode;
    FUnit : string;
    FObservations : TList<Integer>;

    FAverage : Double;
    FMaximum : Double;
    FMinimum : Double;
    FMedian : Double;
    FStd_dev : Double;
    FSum : Double;
    FVariance : Double;
    FPct20 : Double;
    FPct80 : Double;
    F4_lower : Double;
    F4_upper : Double;
    F4_dev : Double;
    F5_1 : Double;
    F5_2 : Double;
    F5_3 : Double;
    F5_4 : Double;
    FSkew : Double;
    FKurtosis : Double;
    FRegression : Double;
    FIntercept : Double;

    function qty(value : double; humanUnits, ucumUnits : String) : TFhirQuantityW;

    procedure init;
    procedure executeConcept(c : TFHIRCodingW);
    function lookupConcept(c : TFHIRCodingW) : integer;
    procedure loadData(c : TFHIRCodingW);
    function genStat(p : TObservationStatsParameter) : TFhirQuantityW;
    function genAverage() : Double;
    function genMaximum() : Double;
    function genMinimum() : Double;
    function genMedian() : Double;
    function genStd_dev() : Double;
    function genSum() : Double;
    function genVariance() : Double;
    function genPct20() : Double;
    function genPct80() : Double;
    function gen4_lower() : Double;
    function gen4_upper() : Double;
    function gen4_dev() : Double;
    function gen5_1() : Double;
    function gen5_2() : Double;
    function gen5_3() : Double;
    function gen5_4() : Double;
    function genSkew() : Double;
    function genKurtosis() : Double;
    function genRegression() : Double;
    function genIntercept() : Double;

  public
    constructor Create(factory : TFHIRFactory; conn : TFDBConnection; resp : TFHIRStatsOpResponseW);
    destructor Destroy; override;

    property subject : String read FSubject write FSubject;
    property subjectKey : integer read FSubjectKey write FSubjectKey;
    property concepts : TFslList<TFHIRCodingW> read FConcepts;
    property start : TDateTime read FStart write FStart;
    property finish : TDateTime read FFinish write FFinish;
    property parameters : TObservationStatsParameterSet read FParameters write FParameters;

    procedure execute;

    property Resp : TFHIRStatsOpResponseW read FResp;
    property Observations : TList<Integer> read FObservations;

  end;

  TObservationLastNEvaluator = class (TFslObject)
  private
    FConn : TFDBConnection;

    FCount: integer;
    FObservations : TStringList;
    FSubjectKey: integer;
    FConcepts: TFslList<TFHIRCodingW>;
    FCategory: TFHIRCodingW;
    procedure SetCategory(const Value: TFHIRCodingW);
    function GetObservations: String;

    procedure listConcepts(cks : TStringList);
    procedure addMostRecentObservations(ck : integer);
    function lookupConcept(c : TFHIRCodingW) : integer;
  public
    constructor Create(conn : TFDBConnection);
    destructor Destroy; override;

    // in
    property subjectKey : integer read FSubjectKey write FSubjectKey;
    property category : TFHIRCodingW read FCategory write SetCategory;
    property concepts : TFslList<TFHIRCodingW> read FConcepts;
    property count : integer read FCount write FCount;

    procedure execute;

    // out
    property Observations : String read GetObservations;
  end;

implementation

{ TObservationStatsComparer }

function TObservationStatsComparer.Compare(const Left, Right: TObservation): Integer;
begin
  if (left = nil) or (right = nil) then
    exit(0);

  if Left.canonical < right.canonical then
    result := 1
  else if Left.canonical > right.canonical then
    result := -1
  else
    result := 1;
end;

{ TObservationStatsEvaluator }

constructor TObservationStatsEvaluator.Create(factory : TFHIRFactory; conn: TFDBConnection; resp : TFHIRStatsOpResponseW);
begin
  inherited create;
  FFactory := factory;
  FConn := conn;
  FConcepts := TFslList<TFHIRCodingW>.create;
  FResp := resp;
  FObservations := TList<Integer>.create;
end;

destructor TObservationStatsEvaluator.Destroy;
begin
  FFactory.Free;
  FObservations.Free;
  FResp.Free;
  FConcepts.free;
  inherited;
end;

procedure TObservationStatsEvaluator.execute;
var
  c : TFHIRCodingW;
begin
  if osp_regression in FParameters then
    FParameters := FParameters + [osp_intercept];
  for c in FConcepts do
    executeConcept(c);
end;

procedure TObservationStatsEvaluator.executeConcept(c: TFHIRCodingW);
var
  obs : TFhirObservationW;
  comp : TFhirObservationComponentW;
  p : TObservationStatsParameter;
  t : TFHIRObject;
begin
  init();
  obs := FFactory.wrapObservation(ffactory.makeResource('Observation'));
  try
    resp.addObs(obs.Resource.Link);
    obs.status := obssFinal;
    obs.setCode(c);
    obs.subject := subject;
    obs.setPeriod(start, finish);
    FAllData := TFslList<TObservation>.create;
    FValidData := TFslList<TObservation>.create;
    try
      loadData(c);
      comp := obs.addComp('http://hl7.org/fhir/observation-paramcode', 'totalcount');
      try
        comp.value := qty(FAllData.Count, '', '{count}');
      finally
        comp.free;
      end;
      comp := obs.addComp('http://hl7.org/fhir/observation-paramcode', 'count');
      try
        comp.value := qty(FValidData.Count, '', '{count}');
      finally
        comp.free;
      end;
      if FValidData.count > 0 then
      begin
        for p in FParameters do
          if (p <> osp_totalcount) and (p <> osp_count) then
          begin
            t := genStat(p);
            if (t <> nil) then
            begin
              comp := obs.addComp('http://hl7.org/fhir/observation-paramcode', CODES_TObservationStatsParameter[p]);
              try
                comp.value := t;
              finally
                comp.free;
              end;
            end;
          end;
      end;
    finally
      FAllData.free;
      FValidData.free;
    end;
  finally
    obs.free;
  end;
end;

procedure TObservationStatsEvaluator.loadData(c: TFHIRCodingW);
var
  obs : TObservation;
  ck, u, cu : integer;
  AllSameUnit, AllSameCanonicalUnit : boolean;
begin
  ck := lookupConcept(c);
  if ck = 0 then
    exit;

  FConn.sql := 'Select ObservationKey, ResourceKey, SubjectKey, DateTime, DateTimeMin, DateTimeMax, Value, ValueUnit, Canonical, CanonicalUnit, ValueConcept ' +
               'from Observations where SubjectKey = '+inttostr(FSubjectKey)+' and ObservationKey in (select ObservationKey from ObservationCodes where ConceptKey = '+inttostr(ck)+' and Source != 1) and Observations.DateTimeMax >= :d1 and Observations.DateTimeMin <= :d2 order by ObservationKey asc';
  FConn.prepare;
  FConn.BindTimeStamp('d1', DateTimeToTS(FStart));
  FConn.BindTimeStamp('d2', DateTimeToTS(FFinish));
  FConn.Execute;
  while FConn.FetchNext do
  begin
    obs := TObservation.Create;
    FAllData.Add(obs);
    obs.Key := FConn.ColIntegerByName['ResourceKey'];
    obs.ConceptKey := ck;
    obs.dateTime := TSToDateTime(FConn.ColTimeStampByName['DateTime']);
    obs.value := FConn.ColDoubleByName['Value'];
    obs.vunit := FConn.ColIntegerByName['ValueUnit'];
    obs.canonical := FConn.ColDoubleByName['Canonical'];
    obs.cunit := FConn.ColIntegerByName['CanonicalUnit'];
    obs.vConcept := FConn.ColIntegerByName['ValueConcept'];
    if (obs.vConcept = 0) then
    begin
      FValidData.Add(obs.Link as TObservation);
      FObservations.Add(FConn.ColIntegerByName['ResourceKey']);
    end;
  end;
  if not FValidData.Empty then
  begin
    FValidData.Sort(TObservationStatsComparer.Create);
    AllSameUnit := true;
    AllSameCanonicalUnit := true;
    u := FValidData[0].vunit;
    cu := FValidData[0].cunit;
    for obs in FValidData do
    begin
      if u <> obs.vunit then
        AllSameUnit := false;
      if cu <> obs.cunit then
        AllSameCanonicalUnit := false;
    end;
    if AllSameUnit then
    begin
      FMode := osmNative;
      FUnit := FConn.Lookup('Concepts', 'ConceptKey', inttostr(u), 'Code', '');
    end
    else if AllSameCanonicalUnit then
    begin
      FMode := osmCanonical;
      FUnit := FConn.Lookup('Concepts', 'ConceptKey', inttostr(cu), 'Code', '');
    end
    else
      FMode := osmInconsistent;
  end;
end;


function TObservationStatsEvaluator.lookupConcept(c: TFHIRCodingW): integer;
begin
  FConn.sql := 'Select ConceptKey from Concepts where Concepts.Code = '''+SQLWrapString(c.code)+''' and Concepts.URL = '''+sqlwrapString(c.systemUri)+'''';
  FConn.prepare;
  FConn.Execute;
  if FConn.FetchNext then
    result := FConn.ColIntegerByName['ConceptKey']
  else
    result := 0;
  FConn.terminate;
end;

function TObservationStatsEvaluator.qty(value: double; humanUnits, ucumUnits: String): TFhirQuantityW;
begin
  result := ffactory.wrapQuantity(FFactory.makeByName('Quantity'));
  try
    result.value := FloatToStr(value);
    result.units := humanUnits;
    if ucumUnits <> '' then
    begin
      result.systemUri := 'http://unitsofmeasure.org';
      result.code := ucumUnits;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TObservationStatsEvaluator.gen4_dev(): double;
begin
  if F4_dev <> -MaxInt then
    exit(F4_dev);
  if FValidData.Count < 4 then
    result := -MaxInt
  else
    result := (FValidData[trunc(FValidData.Count * 3 / 4)].v(FMode) - FValidData[trunc(FValidData.Count / 4)].v(FMode)) / 2;
  F4_dev := result;
end;

function TObservationStatsEvaluator.gen4_lower(): double;
begin
  if F4_lower <> -MaxInt then
    exit(F4_lower);
  if FValidData.Count < 4 then
    result := -MaxInt
  else
    result := FValidData[trunc(FValidData.Count / 4)].v(FMode);
  F4_lower := result;
end;

function TObservationStatsEvaluator.gen4_upper(): double;
begin
  if F4_upper <> -MaxInt then
    exit(F4_upper);
  if FValidData.Count < 4 then
    result := -MaxInt
  else
    result := FValidData[trunc(FValidData.Count * 3 / 4)].v(FMode);
  F4_upper := result;
end;

function TObservationStatsEvaluator.gen5_1(): double;
begin
  if F5_1 <> -MaxInt then
    exit(F5_1);
  if FValidData.Count < 10 then
    result := -MaxInt
  else
    result := FValidData[trunc(FValidData.Count * 1 / 5)].v(FMode);
  F5_1 := result;
end;

function TObservationStatsEvaluator.gen5_2(): double;
begin
  if F5_2 <> -MaxInt then
    exit(F5_2);
  if FValidData.Count < 10 then
    result := -MaxInt
  else
    result := FValidData[trunc(FValidData.Count * 2 / 5)].v(FMode);
  F5_2 := result;
end;

function TObservationStatsEvaluator.gen5_3(): double;
begin
  if F5_3 <> -MaxInt then
    exit(F5_3);
  if FValidData.Count < 10 then
    result := -MaxInt
  else
    result := FValidData[trunc(FValidData.Count * 3 / 5)].v(FMode);
  F5_3 := result;
end;

function TObservationStatsEvaluator.gen5_4(): double;
begin
  if F5_4 <> -MaxInt then
    exit(F5_4);
  if FValidData.Count < 10 then
    result := -MaxInt
  else
    result := FValidData[trunc(FValidData.Count * 4 / 5)].v(FMode);
  F5_4 := result;
end;

function TObservationStatsEvaluator.genAverage(): double;
var
  obs : TObservation;
  t : double;
  c : integer;
begin
  if FAverage <> -MaxInt then
    exit(FAverage);

  t := 0;
  c := 0;
  for obs in FValidData do
  begin
     t := t + obs.v(FMode);
     inc(c);
  end;
  result := t/c;
  FAverage := result;
end;


function TObservationStatsEvaluator.genIntercept: Double;
begin
  if FRegression = -MaxInt then
    genRegression;
  result := FIntercept;
end;

function TObservationStatsEvaluator.genKurtosis: Double;
var
//  av : Double;
  obs : TObservation;
  t, m, d : double;
begin
  if FKurtosis <> -MaxInt then
    exit(FKurtosis);
  //av := genAverage;
  m := 0;
  d := 0;
  for Obs in FValidData do
  begin
    t := obs.v(FMode);
    m := m + (t*t*t*t);
    d := d + (t*t);
  end;
  result := (m / (d*d)) - 3;
  FKurtosis := result;
end;

function TObservationStatsEvaluator.genMaximum(): double;
var
  obs : TObservation;
  m : double;
begin
  if FMaximum <> -MaxInt then
    exit(FMaximum);
  m := FValidData[0].value;
  for obs in FValidData do
    if (obs.v(FMode) > m) then
      m := obs.v(FMode);
  result := m;
  FMaximum := result;
end;

function TObservationStatsEvaluator.genMedian(): double;
var
  m : double;
begin
  if FMedian <> -MaxInt then
    exit(FMedian);
  if FValidData.Count = 1 then
    m := FValidData[0].v(FMode)
  else if FValidData.Count mod 2 = 1 then
    m := FValidData[FValidData.Count div 2].v(FMode)
  else
    m := (FValidData[FValidData.Count div 2-1].v(FMode) + FValidData[FValidData.Count div 2].v(FMode)) / 2;
  result := m;
  FMedian := result;
end;

function TObservationStatsEvaluator.genMinimum(): double;
var
  obs : TObservation;
  m : double;
begin
  if FMinimum <> -MaxInt then
    exit(FMinimum);
  m := FValidData[0].value;
  for obs in FValidData do
    if (obs.v(FMode) < m) then
      m := obs.v(FMode);
  result := m;
  FMinimum := result;
end;

function TObservationStatsEvaluator.genPct20(): double;
begin
  if FPct20 <> -MaxInt then
    exit(FPct20);
  if FValidData.Count < 10 then
    result := -MaxInt
  else
    result := FValidData[trunc(FValidData.Count / 5)].v(FMode);
  FPct20 := result;
end;

function TObservationStatsEvaluator.genPct80(): double;
begin
  if FPct80 <> -MaxInt then
    exit(FPct80);
  if FValidData.Count < 10 then
    result := -MaxInt
  else
    result := FValidData[trunc(FValidData.Count * 4 / 5)].v(FMode);
  FPct80 := result;
end;

function TObservationStatsEvaluator.genRegression: Double;
var
  sumX, sumY, sumXY, sumX2: Double;
  obs : TObservation;
begin
  if FRegression <> -MaxInt then
    exit(FRegression);

  sumX := 0;
  sumY := 0;
  sumX2 := 0;
  sumXY := 0;
  for obs in FValidData do
  begin
    sumX := sumX + obs.dateTime;
    sumY := sumY + obs.v(FMode);
    sumX2 := sumX2 + (obs.dateTime * obs.dateTime);
    sumXY := sumXY + (obs.dateTime * obs.v(FMode));
  end;
  FRegression := ((FValidData.Count * sumXY) - (sumX * sumY)) / ((FValidData.Count * sumX2) - (sumX * sumX));
  FIntercept := (sumY / FValidData.Count) - FRegression * (sumX / FValidData.Count);
  result := FRegression;
end;

function TObservationStatsEvaluator.genSkew: Double;
begin
  if FSkew <> -MaxInt then
    exit(FSkew);
  result := 3 * (genAverage - genMedian) / genStd_dev;
  FSkew := result;
end;

function TObservationStatsEvaluator.genStat(p: TObservationStatsParameter): TFhirQuantityW;
var
  d : Double;
  u : string;
begin
  if (p in [osp_count, osp_totalcount]) then
    result := nil
  else
  begin
    if FMode = osmInconsistent then
      raise EFHIRException.create('The stat '+CODES_TObservationStatsParameter[p]+' cannot be generated when some of the canonical units differ');
    case p of
      osp_average: d := genAverage();
      osp_maximum: d := genMaximum();
      osp_minimum: d := genMinimum();
      osp_median: d := genMedian();
      osp_std_dev: d := genStd_dev();
      osp_sum: d := genSum();
      osp_variance: d := genVariance();
      osp_pct20: d := genPct20();
      osp_pct80: d := genPct80();
      osp_4_lower: d := gen4_lower();
      osp_4_upper: d := gen4_upper();
      osp_4_dev: d := gen4_dev();
      osp_5_1: d := gen5_1();
      osp_5_2: d := gen5_2();
      osp_5_3: d := gen5_3();
      osp_5_4: d := gen5_4();
      osp_skew : d := genSkew();
      osp_kurtosis : d := genKurtosis();
      osp_regression : d := genRegression();
      osp_intercept : d := genIntercept();
    else
      exit(nil);
    end;
    if d = -MaxInt then
      exit(nil)
    else
    begin
      result := ffactory.wrapQuantity(FFactory.makeByName('Quantity'));
      try
        result.value := FloatToStr(d);
        u := HUNITS_TObservationStatsParameter[p];
        if u = '*' then
          u := FUnit;
        result.units := u;
        u := UNITS_TObservationStatsParameter[p];
        if u = '*' then
          u := FUnit;
        if u <> '' then
        begin
          result.systemUri := 'http://unitsofmeasure.org';
          result.code := u;
        end;
        result.Link;
      finally
        result.Free;
      end;
    end;
  end;
end;

function TObservationStatsEvaluator.genStd_dev(): double;
var
  obs : TObservation;
  t : double;
  c : integer;
  av, m : double;
begin
  if FStd_dev <> -MaxInt then
    exit(FStd_dev);
  // figure the average
  t := 0;
  c := 0;
  for obs in FValidData do
  begin
    t := t + obs.v(FMode);
    inc(c);
  end;
  av := t/c;
  m := 0;
  for obs in FValidData do
  begin
    t := obs.v(FMode) - av;
    m := m + (t*t);
  end;
  result := sqrt(m/c);
  FStd_dev := result;
end;

function TObservationStatsEvaluator.genSum(): double;
var
  obs : TObservation;
  m : double;
begin
  if FSum <> -MaxInt then
    exit(FSum);
  m := FValidData[0].value;
  for obs in FValidData do
     m := m + obs.v(FMode);
  result := m;
  FSum := result;
end;


function TObservationStatsEvaluator.genVariance(): double;
var
  obs : TObservation;
  t : double;
  c : integer;
  av, m : double;
begin
  if FVariance <> -MaxInt then
    exit(FVariance);
  // figure the average
  t := 0;
  c := 0;
  for obs in FValidData do
  begin
    t := t + obs.v(FMode);
    inc(c);
  end;
  av := t/c;
  m := 0;
  for obs in FValidData do
  begin
    t := obs.v(FMode) - av;
    m := m + (t*t);
  end;
  result := m/c;
  FVariance := result;
end;



procedure TObservationStatsEvaluator.init;
begin
  FAverage := -MaxInt;
  FMaximum := -MaxInt;
  FMinimum := -MaxInt;
  FMedian := -MaxInt;
  FStd_dev := -MaxInt;
  FSum := -MaxInt;
  FVariance := -MaxInt;
  FPct20 := -MaxInt;
  FPct80 := -MaxInt;
  F4_lower := -MaxInt;
  F4_upper := -MaxInt;
  F4_dev := -MaxInt;
  F5_1 := -MaxInt;
  F5_2 := -MaxInt;
  F5_3 := -MaxInt;
  F5_4 := -MaxInt;
  FSkew := -MaxInt;
  FKurtosis := -MaxInt;
  FRegression := -MaxInt;
  FIntercept := -MaxInt;
end;

{ TObservation }

function TObservation.v(mode : TObservationStatsMode): Double;
begin
  if mode = osmCanonical then
    result := canonical
  else
    result := value;
end;

{ TObservationLastNEvaluator }

constructor TObservationLastNEvaluator.Create(conn: TFDBConnection);
begin
  inherited create;
  FConn := conn;
  FCount := 1;
  FObservations := TStringList.create;
  FObservations.Sorted := true;
  FObservations.Duplicates := dupIgnore;
  FSubjectKey := 0;
  FConcepts := TFslList<TFHIRCodingW>.create;
end;

destructor TObservationLastNEvaluator.Destroy;
begin
  FObservations.Free;
  FConcepts.Free;
  FCategory.Free;
  inherited;
end;

procedure TObservationLastNEvaluator.SetCategory(const Value: TFHIRCodingW);
begin
  FCategory.Free;
  FCategory := Value;
end;

function TObservationLastNEvaluator.GetObservations: String;
begin
  result := FObservations.CommaText;
end;

procedure TObservationLastNEvaluator.execute;
var
  cks : TStringList;
  i : integer;
begin
  FObservations.Clear;
  cks := TStringList.create;
  try
    listConcepts(cks);
    cks.Sort;
    for i := 0 to cks.Count - 1 do
      addMostRecentObservations(integer(cks.Objects[i]));
  finally
    cks.Free;
  end;
end;

procedure TObservationLastNEvaluator.listConcepts(cks: TStringList);
var
  c : TFHIRCodingW;
begin
  if FCategory <> nil then
  begin
    FConn.SQL := 'Select distinct ObservationCodes.ConceptKey, URL, Code from ObservationCodes, Concepts where ObservationCodes.ConceptKey = Concepts.ConceptKey and source = 2 and ObservationKey in (Select ObservationKey from ObservationCodes where ConceptKey = '+inttostr(lookupConcept(FCategory))+' and source = 1)';
    FConn.Prepare;
    FConn.Execute;
    while FConn.FetchNext do
      cks.AddObject(FConn.ColStringByName['URL']+'::'+FConn.ColStringByName['Code'], TObject(FConn.ColIntegerByName['ConceptKey']));
    FConn.Terminate;
  end
  else
    for c in FConcepts do
      cks.AddObject(c.systemUri+'::'+c.code, TObject(lookupConcept(c)));
end;

function TObservationLastNEvaluator.lookupConcept(c: TFHIRCodingW): integer;
begin
  FConn.sql := 'Select ConceptKey from Concepts where Concepts.Code = '''+SQLWrapString(c.code)+''' and Concepts.URL = '''+sqlwrapString(c.systemUri)+'''';
  FConn.prepare;
  FConn.Execute;
  if FConn.FetchNext then
    result := FConn.ColIntegerByName['ConceptKey']
  else
    result := 0;
  FConn.terminate;
end;

procedure TObservationLastNEvaluator.addMostRecentObservations(ck: integer);
begin
  FConn.SQL := 'select top '+inttostr(FCount)+' ResourceKey from Observations where subject = '+inttostr(subjectKey)+' and ObservationKey in (select ObservationKey ObservationCodes where Source = 2 and ConceptKey = '+inttostr(ck)+') from sort by DateTime desc';
  FConn.prepare;
  FConn.Execute;
  while FConn.FetchNext do
    FObservations.Add(FConn.ColStringByName['ResourceKey']);
  FConn.Terminate;
end;


end.
