unit OpenMHealthServer;

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
  SysUtils, Classes,
  IdContext, IdCustomHTTPServer,
  AdvObjects, AdvJson, DateSupport,
  FHIRBase, FHIRSupport, FHIRTypes, FHIRResources, FHIRUtilities;

type
  TOpenMHealthAdaptor = class (TFHIRFormatAdaptor)
  private
    function loadDataPoint(stream : TStream) : TFHIRObservation;
    function readDataPoint(json : TJsonObject) : TFHIRObservation;
    function readHeader(hdr: TJsonObject; obs: TFhirObservation) : String;
    procedure readPhysicalActivity(body: TJsonObject; obs: TFhirObservation);
    procedure readBloodGlucose(body: TJsonObject; obs: TFhirObservation);
    function readTimeInterval(obj : TJsonObject) : TFHIRPeriod;
    function readQuantity(obj : TJsonObject) : TFHIRQuantity;
    function convertUCUMUnit(s : String) : String;
    function convertObsStat(s : String) : String;

    procedure writeObservation(obs : TFHIRObservation; json : TJsonObject);
    procedure writeBundle(obs : TFHIRBundle; json : TJsonObject);
    function writeHeader(obs : TFHIRObservation; hdr : TJsonObject) : String;
    procedure writePhysicalActivity(obs : TFHIRObservation; body : TJsonObject);
    procedure writeBloodGlucose(obs : TFHIRObservation; body : TJsonObject);
    function writePeriod(period : TFHIRPeriod) : TJsonObject;
    function writeQuantity(qty : TFHIRQuantity) : TJsonObject;
    function unconvertUCUMUnit(s : String) : String;
    function unconvertObsStat(s : String) : String;
  public
    procedure load(req : TFHIRRequest; stream : TStream); override;
    function NewIdStatus : TCreateIdState; override;
    function ResourceName : String; override;
    procedure compose(response : TFHIRResponse; stream : TStream); override;
    function MimeType : String; override;
    procedure editSearch(req : TFHIRRequest); override;
  end;

implementation

{ TOpenMHealthAdaptor }


procedure TOpenMHealthAdaptor.compose(response: TFHIRResponse; stream: TStream);
var
  json : TJsonObject;
begin
  json := TJsonObject.Create;
  try
    if response.Resource = nil then
      raise Exception.Create('Cannot represent a resource of (nil) as an OpenMHealth data point')
    else if response.Resource is TFHIRObservation then
      writeObservation(response.Resource as TFHIRObservation, json)
    else if response.Resource is TFHIRBundle then
      writeBundle(response.Resource as TFHIRBundle, json)
    else
      raise Exception.Create('Cannot represent a resource of type '+response.Resource.fhirType+' as an OpenMHealth data point');
    TJSONWriter.writeObject(stream, json, true);
  finally
    json.Free;
  end;
end;

function TOpenMHealthAdaptor.convertObsStat(s: String): String;
begin
  if (s = 'average') then
    result := 'average'
  else if (s = 'maximum') then
    result := 'maximum'
  else if (s = 'minimum') then
    result := 'minimum'
  else if (s = 'count') then
    result := 'count'
  else if (s = 'median') then
    result := 'median'
  else if (s = 'standard deviation') then
    result := 'std-dev'
  else if (s = 'sum') then
    result := 'sum'
  else if (s = 'variance') then
    result := 'variance'
  else if (s = '20th percentile') then
    result := '%20'
  else if (s = '80th percentile') then
    result := '%80'
  else if (s = 'lower quartile') then
    result := '4-lower'
  else if (s = 'upper quartile') then
    result := '4-upper'
  else if (s = 'quartile deviation') then
    result := '4-dev'
  else if (s = '1st quintile') then
    result := '5-1'
  else if (s = '2nd quintile') then
    result := '5-2'
  else if (s = '3rd quintile') then
    result := '5-3'
  else if (s = '4th quintile') then
    result := '5-4'
  else
    result := s;
end;

function TOpenMHealthAdaptor.convertUCUMUnit(s: String): String;
begin
  if (s = 'in') then
    result := '[in_i]'
  else if (s = 'ft') then
    result := '[ft_i]'
  else if (s = 'yd') then
    result := '[yd_i]'
  else if (s = 'mi') then
    result := '[mi_i]'
  else if (s = 'sec') then
    result := 's'
  else if (s = 'Mo') then
    result := 'mo'
  else if (s = 'yr') then
    result := 'a'
  else
    result := s;
end;

procedure TOpenMHealthAdaptor.editSearch(req: TFHIRRequest);
var
  p : String;
begin
  if req.Parameters.GetVar('schema_namespace') <> 'omh' then
    raise Exception.Create('Unknown schema namespace');
  if req.Parameters.GetVar('schema_version') <> '1.0' then
    raise Exception.Create('Unknown schema version for OMH 1.0');
  p := '_profile=http://www.openmhealth.org/schemas/fhir/'+req.Parameters.GetVar('schema_namespace')+'/'+req.Parameters.GetVar('schema_version')+'/'+req.Parameters.GetVar('schema_name');
  if req.Parameters.VarExists('created_on_or_after') then
    p := p + '&date=ge'+req.Parameters.getvar('created_on_or_after');
  if req.Parameters.VarExists('created_before') then
    p := p + '&date=le'+req.Parameters.getvar('created_before');
  req.LoadParams(p);
end;

procedure TOpenMHealthAdaptor.load(req: TFHIRRequest; stream : TStream);
begin
  req.PostFormat := ffJson;
  req.Resource := loadDataPoint(stream);
  req.ResourceName := 'Observation';
  req.Id := req.Resource.id;
end;

function TOpenMHealthAdaptor.loadDataPoint(stream: TStream): TFHIRObservation;
var
  json : TJsonObject;
begin
  json := TJSONParser.Parse(stream);
  try
    result := readDataPoint(json);
  finally
    json.Free;
  end;
end;

function TOpenMHealthAdaptor.MimeType: String;
begin
  result := 'application/json';
end;

function TOpenMHealthAdaptor.NewIdStatus: TCreateIdState;
begin
  result := idCheckNew;
end;

function TOpenMHealthAdaptor.readDataPoint(json: TJsonObject): TFHIRObservation;
var
  schema : string;
begin
  result := TFHIRObservation.Create;
  try
    result.meta := TFHIRMeta.Create;
    // not set by OMH
    result.status := ObservationStatusFinal; // final is reasonable for most mobilde data

    if (not json.has('header')) then // it must, but check anyway
      raise Exception.Create('Cannot process without header');

    schema := readHeader(json.obj['header'], result);
    if (schema = 'physical-activity') then
      readPhysicalActivity(json.obj['body'], result)
    else if (schema = 'blood-glucose') then
      readBloodGlucose(json.obj['body'], result)
    else
      raise Exception.Create('Unsupported schema type '+schema);
    result.Link;
  finally
    result.Free;
  end;

end;

function TOpenMHealthAdaptor.readHeader(hdr: TJsonObject; obs: TFhirObservation) : String;
var
  ext : TFHIRExtension;
  obj : TJsonObject;
begin
  // id --> resource.id.
  obs.id := hdr['id'];

  // creation_date_time --> extension on metadata. What is the significance of this value? why does it matter? or is it actually last_updated?
  ext := obs.meta.extensionList.Append;
  ext.url := 'http://healthintersections.com.au/fhir/StructureDefinition/first-created';
  ext.value := TFhirDateTime.Create(TDateTimeEx.fromXml(hdr['creation_date_time']));

  // schema_id --> this maps to a profile on observation. todo: what is the correct URL?
  obj := hdr.obj['schema_id'];
  result := obj['name'];
  obs.meta.profileList.Add(TFHIRUri.create('http://www.openmhealth.org/schemas/fhir/'+obj['namespace']+'/'+obj['version']+'/'+obj['name']));

  obj := hdr.obj['acquisition_provenance'];
  if (obj <> nil) then
  begin
    // acquisition_provenance.source_name --> observation.device
    if obj.has('source_name') then // though this is required
    begin
      obs.device := TFhirReference.Create;
      obs.device.display := obj['source_name'];
    end;
    // acquisition_provenance.source_creation_date_time --> observation.
    if obj.has('source_creation_date_time') then
    begin
      obs.issued := TDateTimeEx.fromXml(obj['source_creation_date_time']);
    end;
    // acquisition_provenance.modality --> observation.method
    if obj.has('modality') then
    begin
      obs.method := TFhirCodeableConcept.Create;
      obs.method.text := obj['modality'];
    end;
  end;
  // user_id --> Observation.subject
  if hdr.has('user_id') then
  begin
    obs.subject := TFhirReference.Create;
    obs.subject.reference := 'Patient/'+hdr['user_id'];
  end;
end;

procedure TOpenMHealthAdaptor.readPhysicalActivity(body: TJsonObject; obs: TFhirObservation);
var
  c : TFHIRCoding;
  obj : TJsonObject;
begin
  // physical activity is the category
  c := obs.categoryList.Append.codingList.Append;
  c.system := 'http://openmhealth.org/codes';
  c.code := 'omh';
  c.display := 'OpenMHealth Data';

  // code comes from activity name
  obs.code := TFhirCodeableConcept.Create;
  obs.code.text := body['activity_name'];

  // effective_time_frame --> Observation.effective
  obj := body.obj['effective_time_frame'];
  if (obj <> nil) then
  begin
    if (obj.has('time_interval')) then
      obs.effective := readTimeInterval(obj.obj['time_interval'])
    else if (obj.has('date_time')) then
      obs.effective := TFhirDateTime.Create(TDateTimeEx.fromXml(obj['date_time']));
  end;

  // distance --> Observation.value
  if body.has('distance') then
    obs.value := readQuantity(body.obj['distance']);

  if body.has('kcal_burned') then
    obs.addComponent('http://loinc.org', '41981-2').value := readQuantity(body.obj['kcal_burned']);

  if body.has('reported_activity_intensity') then
    obs.addComponent('http://openmhealth.org/codes', 'reported_activity_intensity').value := TFHIRString.create(body['reported_activity_intensity']);

  if body.has('met_value') then
    obs.addComponent('http://snomed.info/sct', '698834005').value := readQuantity(body.obj['met_value']);
end;

function TOpenMHealthAdaptor.readQuantity(obj: TJsonObject): TFHIRQuantity;
begin
  result := TFhirQuantity.Create;
  try
    result.value := obj['value'];
    result.system := 'http://unitsofmeasure.org';
    result.unit_ := obj['unit'];
    result.code := convertUCUMUnit(result.unit_);
    result.Link;
  finally
    result.Free;
  end;
end;

function TOpenMHealthAdaptor.readTimeInterval(obj: TJsonObject): TFHIRPeriod;
var
  qty : TFHIRQuantity;
  ext : TFHIRExtension;
  day : TDateTimeEx;
  s : String;
begin
  result := TFHIRPeriod.create;
  try
    // creation_date_time --> extension on metadata. What is the significance of this value? why does it matter? or is it actually last_updated?
    ext := result.extensionList.Append;
    ext.url := 'http://healthintersections.com.au/fhir/StructureDefinition/period-form';
    if obj.has('start_date_time') and obj.has('end_date_time') then
    begin
      // The interval is defined by a precise start and end time
      result.start := TDateTimeEx.fromXml(obj['start_date_time']);
      result.end_ := TDateTimeEx.fromXml(obj['end_date_time']);
      ext.value := TFhirCode.Create('normal');
    end
    else if obj.has('start_date_time') and obj.has('duration') then
    begin
      // The interval is defined by a precise start time and a duration
      result.start := TDateTimeEx.fromXml(obj['start_date_time']);
      qty := readQuantity(obj.obj['duration']);
      try
        result.end_ := result.start.add(qty.asDuration);
      finally
        qty.Free;
      end;
      ext.value := TFhirCode.Create('start.duration');
    end
    else if obj.has('end_date_time') and obj.has('duration') then
    begin
      // The interval is defined by a duration and a precise end time
      result.end_ := TDateTimeEx.fromXml(obj['end_date_time']);
      qty := readQuantity(obj.obj['duration']);
      try
        result.start := result.start.subtract(qty.asDuration);
      finally
        qty.Free;
      end;
      ext.value := TFhirCode.Create('end.duration');
    end
    else
    begin
      // the interval is defined by a date and a part of the day (morning, afternoon, evening, night)
      day := TDateTimeEx.fromXml(obj['date']);
      s := obj['part_of_day'];
      if (s = 'morning') then
      begin
        ext.value := TFhirCode.Create('day.morning');
        result.start := day.add(0.25); // 6am --> 12noon
        result.end_ := day.add(0.5);
      end
      else if (s = 'afternoon') then
      begin
        ext.value := TFhirCode.Create('day.afternoon');
        result.start := day.add(0.5); // 12noon --> 6pm
        result.end_ := day.add(0.75);
      end
      else if (s = 'evening') then
      begin
        ext.value := TFhirCode.Create('day.evening');
        result.start := day.add(0.75); // 6pm --> midnight
        result.end_ := day.add(1);
      end
      else if (s = 'night') then
      begin
        ext.value := TFhirCode.Create('day.night');
        result.start := day.add(0); // midnight --> 6am - but is it the next day?
        result.end_ := day.add(0.25);
      end;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TOpenMHealthAdaptor.ResourceName: String;
begin
  result := 'Observation';
end;

function TOpenMHealthAdaptor.unconvertObsStat(s: String): String;
begin
  if (s = 'average') then
    result := 'average'
  else if (s = 'maximum') then
    result := 'maximum'
  else if (s = 'minimum') then
    result := 'minimum'
  else if (s = 'count') then
    result := 'count'
  else if (s = 'median') then
    result := 'median'
  else if (s = 'std-dev') then
    result := 'standard deviation'
  else if (s = 'sum') then
    result := 'sum'
  else if (s = 'variance') then
    result := 'variance'
  else if (s = '%20') then
    result := '20th percentile'
  else if (s = '%80') then
    result := '80th percentile'
  else if (s = '4-lower') then
    result := 'lower quartile'
  else if (s = '4-upper') then
    result := 'upper quartile'
  else if (s = '4-dev') then
    result := 'quartile deviation'
  else if (s = '5-1') then
    result := '1st quintile'
  else if (s = '5-2') then
    result := '2nd quintile'
  else if (s = '5-3') then
    result := '3rd quintile'
  else if (s = '5-4') then
    result := '4th quintile'
  else
    result := s;
end;

function TOpenMHealthAdaptor.unconvertUCUMUnit(s: String): String;
begin
  if (s = '[in_i]') then
    result := 'in'
  else if (s = '[ft_i]') then
    result := 'ft'
  else if (s = '[yd_i]') then
    result := 'yd'
  else if (s = '[mi_i]') then
    result := 'mi'
  else if (s = 's') then
    result := 'sec'
  else if (s = 'mo') then
    result := 'Mo'
  else if (s = 'a') then
    result := 'yr'
  else
    result := s;
end;

procedure TOpenMHealthAdaptor.writeObservation(obs: TFHIRObservation; json: TJsonObject);
var
  schema : String;
begin
  schema := writeHeader(obs, json.forceObj['header']);
  if (schema = 'physical-activity') then
    writePhysicalActivity(obs, json.forceObj['body'])
  else if (schema = 'blood-glucose') then
    writeBloodGlucose(obs, json.forceObj['body'])
  else
    raise Exception.Create('Unsupported schema type '+schema);
end;

function TOpenMHealthAdaptor.writePeriod(period: TFHIRPeriod): TJsonObject;
var
  form : String;
  qty : TFhirQuantity;
begin
  if (period.start.null) then
    raise Exception.Create('Can''t convert a period to OpenMHealth when periods are incomplete');
  if (period.end_.null) then
    raise Exception.Create('Can''t convert a period to OpenMHealth when periods are incomplete');

  result := TJsonObject.Create;
  try
    form := period.getExtensionString('http://healthintersections.com.au/fhir/StructureDefinition/period-form');
    if (form = 'start.duration') then
    begin
      result['start_date_time'] := period.start.toXml;
      qty := TFHIRQuantity.fromDuration(period.end_.UTC.dateTime - period.start.UTC.dateTime);
      try
        result.obj['duration'] := writeQuantity(qty);
      finally
        qty.Free;
      end;
    end
    else if (form = 'end.duration') then
    begin
      result['end_date_time'] := period.end_.toXml;
      qty := TFHIRQuantity.fromDuration(period.start.UTC.dateTime - period.end_.UTC.dateTime);
      try
        result.obj['duration'] := writeQuantity(qty);
      finally
        qty.Free;
      end;
    end
    else if (form = 'day.morning') then
    begin
      result['date'] := period.start.toXml.Substring(0, 10);
      result.str['part_of_day'] := 'morning';
    end
    else if (form = 'day.afternoon') then
    begin
      result['date'] := period.start.toXml.Substring(0, 10);
      result.str['part_of_day'] := 'afternoon';
    end
    else if (form = 'day.evening') then
    begin
      result['date'] := period.start.toXml.Substring(0, 10);
      result.str['part_of_day'] := 'evening';
    end
    else if (form = 'day.night') then
    begin
      result['date'] := period.start.toXml.Substring(0, 10);
      result.str['part_of_day'] := 'night';
    end
    else
    begin
      result['start_date_time'] := period.start.toXml;
      result['end_date_time'] := period.end_.toXml;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

procedure TOpenMHealthAdaptor.writePhysicalActivity(obs: TFHIRObservation; body: TJsonObject);
var
  comp : TFhirObservationComponent;
begin
  body['activity_name'] := obs.code.text;

  if (obs.effective <> nil) then
  begin
    if (obs.effective is TFhirPeriod) then
      body.forceObj['effective_time_frame'].obj['time_interval'] := writePeriod(TFhirPeriod(obs.effective))
    else if (obs.effective is TFhirDateTime) then
      body.forceObj['effective_time_frame']['date_time'] := TFhirDateTime(obs.effective).value.toXml;
  end;

  if obs.value is TFHIRQuantity then
    body.obj['distance'] := writeQuantity(obs.value as TFhirQuantity);

  if obs.getComponent('http://loinc.org', '41981-2', comp) then
    body.obj['kcal_burned'] := writeQuantity(comp.value as TFhirQuantity);

  if obs.getComponent('http://openmhealth.org/codes', 'reported_activity_intensity', comp) then
    body['reported_activity_intensity'] := (comp.value as TFhirString).value;

  if obs.getComponent('http://snomed.info/sct', '698834005', comp) then
    body.obj['met_value'] := writeQuantity(comp.value as TFhirQuantity);
end;

function TOpenMHealthAdaptor.writeQuantity(qty: TFHIRQuantity): TJsonObject;
begin
  result := TJsonObject.Create;
  try
    result['value'] := qty.value;
    result['unit'] := unconvertUCUMUnit(qty.code);
    result.Link;
  finally
    result.Free;
  end;
end;

function TOpenMHealthAdaptor.writeHeader(obs: TFHIRObservation; hdr: TJsonObject) : String;
var
  obj : TJsonObject;
  u : TFHIRUri;
  s : String;
  p : TArray<String>;
begin
  hdr['id'] := obs.id;
  if (obs.meta.hasExtension('http://healthintersections.com.au/fhir/StructureDefinition/first-created')) then
    hdr['creation_date_time'] := obs.meta.getExtensionString('http://healthintersections.com.au/fhir/StructureDefinition/first-created');

  s := '';
  for u in obs.meta.profileList do
    if u.value.StartsWith('http://www.openmhealth.org/schemas/fhir/') then
      s := u.value;
  if (s = '') then // todo: try doing it anyway
    raise Exception.Create('Cannot represent an observation with no OpenMHealth profile as an OpenMHealth data point');

  p := s.Split(['/']);
  obj := hdr.forceObj['schema_id'];
  obj['namespace'] := p[5];
  obj['version'] := p[6];
  obj['name'] := p[7];
  result := p[7];

  if (obs.device <> nil) or (obs.issued.notNull) or (obs.method <> nil) then
  begin
    obj := hdr.forceobj['acquisition_provenance'];
    if (obs.device <> nil) then
      obj['source_name'] := obs.device.display;
    if (obs.issued.notNull) then
      obj['source_creation_date_time'] := obs.issued.toXml;
    if (obs.method <> nil) then
      obj['modality'] := obs.method.text;
  end;
  if (obs.subject <> nil) and (obs.subject.reference.StartsWith('Patient/')) then
    hdr['user_id'] := obs.subject.reference.Substring(8);
end;

procedure TOpenMHealthAdaptor.readBloodGlucose(body: TJsonObject; obs: TFhirObservation);
var
  c : TFHIRCoding;
  qty : TFhirQuantity;
  sp : TFHIRSpecimen;
  obj : TJsonObject;
begin
  // physical activity is the category
  c := obs.categoryList.Append.codingList.Append;
  c.system := 'http://openmhealth.org/codes';
  c.code := 'omh';
  c.display := 'OpenMHealth Data';

  // LOINC code depends on units..
  qty := readQuantity(body.obj['blood_glucose']);
  obs.value := qty;

  obs.code := TFhirCodeableConcept.Create;
  c := obs.code.codingList.Append;
  c.system := 'http://loinc.org';
  if (qty.code = 'mg/dL') then
  begin
    c.code := '2339-0';
    c.display := 'Glucose [Mass/volume] in Blood';
  end
  else
  begin
    c.code := '15074-8';
    c.display := 'Glucose [Moles/volume] in Blood';
  end;

  // specimen_source --> observation.specimen.code
  if (body.has('specimen_source')) then
  begin
    sp := TFhirSpecimen.Create;
    sp.id := 'sp';
    obs.containedList.Add(sp);
    obs.specimen := TFhirReference.Create;
    obs.specimen.reference := '#sp';
    sp.type_ := TFhirCodeableConcept.Create;
    sp.type_.text := body['specimen_source']; // todo: can this be coded?
  end;

  // effective_time_frame --> Observation.effective
  obj := body.obj['effective_time_frame'];
  if (obj <> nil) then
  begin
    if (obj.has('time_interval')) then
      obs.effective := readTimeInterval(obj.obj['time_interval'])
    else if (obj.has('date_time')) then
      obs.effective := TFhirDateTime.Create(TDateTimeEx.fromXml(obj['date_time']));
  end;

  // temporal_relationship_to_meal/sleep --> component.value
  if (body.has('temporal_relationship_to_meal')) then
    obs.addComponent('http://snomed.info/sct', '309602000').value := TFHIRString.create(body['temporal_relationship_to_meal']);
  if (body.has('temporal_relationship_to_sleep')) then
    obs.addComponent('http://snomed.info/sct', '309609009').value := TFHIRString.create(body['temporal_relationship_to_sleep']);

  // descriptive stat- follow the $stats patterns
  if (body.has('descriptive_statistic')) then
  begin
    obs.addComponent('http://hl7.org/fhir/observation-statistics', convertObsStat(body['descriptive_statistic'])).value := obs.value.Link;
    obs.value := nil;
  end;

  // user_notes --> Observation.comment
  if (body.has('user_notes')) then
    obs.comment := body['user_notes'];
end;

procedure TOpenMHealthAdaptor.writeBloodGlucose(obs: TFHIRObservation; body: TJsonObject);
var
  comp : TFhirObservationComponent;
  sp : TFHIRSpecimen;
begin
  if obs.getComponent('http://hl7.org/fhir/observation-statistics', comp) then
  begin
    body.obj['blood_glucose'] := writeQuantity(comp.value as TFHIRQuantity);
    body['descriptive_statistic'] := unconvertObsStat(comp.code.codingList[0].code);
  end
  else
    body.obj['blood_glucose'] := writeQuantity(obs.value as TFHIRQuantity);

  sp := obs.Contained['sp'] as TFhirSpecimen;
  if (sp <> nil) then
    body['specimen_source'] := sp.type_.text;

  if (obs.effective <> nil) then
  begin
    if (obs.effective is TFhirPeriod) then
      body.forceObj['effective_time_frame'].obj['time_interval'] := writePeriod(TFhirPeriod(obs.effective))
    else if (obs.effective is TFhirDateTime) then
      body.forceObj['effective_time_frame']['date_time'] := TFhirDateTime(obs.effective).value.toXml;
  end;

  if obs.getComponent('http://snomed.info/sct', '309602000', comp) then
    body['temporal_relationship_to_meal'] := (comp.value as TFhirString).value;
  if obs.getComponent('http://snomed.info/sct', '309609009', comp) then
    body['temporal_relationship_to_sleep'] := (comp.value as TFhirString).value;

  if obs.comment <> '' then
    body['user_notes'] := obs.comment;
end;

procedure TOpenMHealthAdaptor.writeBundle(obs: TFHIRBundle; json: TJsonObject);
var
  arr : TJsonArray;
  be : TFhirBundleEntry;
begin
  arr := json.forceArr['matches'];
  for be in obs.entryList do
  begin
    if be.resource is TFHIRObservation then
      writeObservation(be.resource as TFHIRObservation, arr.addObject());
  end;
end;

end.
