unit FHIR.Server.Covid;

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
  SysUtils, Classes,
  IdCustomHTTPServer,
  fsl_base, fsl_utilities,
  fsl_http,
  fhir_objects, fhir_factory, fhir_client, fhir_pathengine,
  fhir4_resources, fhir4_types, fhir4_liquid, fhir4_pathengine, fhir4_context, fhir4_client, fhir4_resources_base,
  session, web_base, endpoint, webserver, web_source, server_context;

type
  TCovidScriptContext = class;

  TCovidExtension = class (TFHIRWebServerExtension)
  private
    FCovid : TCovidScriptContext;
    function evaluateToString(context : TFHIRPathExecutionContext; engine : TFHIRPathEngineV; focus: TFHIRObject; param : TFHIRPathExpressionNodeV) : String;
    function funcHtmlSelect(context : TFHIRPathExecutionContext; engine : TFHIRPathEngineV; focus: TFHIRObject; params : TFslList<TFHIRPathExpressionNodeV>) : String;
  public
    constructor Create(context : TFHIRServerContext; covid : TCovidScriptContext);
    destructor Destroy; override;

    function resolveConstant(context : TFHIRPathExecutionContext; s : String; var obj : TFHIRObject) : boolean; override;
    function isValidFunction(name : String) : boolean; override;
    function functionApplies(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; name : String): boolean; override;
    function execute(context : TFHIRPathExecutionContext; focus: TFHIRObject; name : String; params : TFslList<TFHIRPathExpressionNodeV>; engine : TFHIRPathEngineV): TFHIRSelectionList; override;
  end;

  TCovidScriptContext = class (TFslObject)
  private
    FClient : TFhirClient4;
    FId : String;
    FPatient : TFhirPatient;

    function getPatient : TFhirPatient;
  public
    constructor Create(client : TFhirClient4; id : String);
    destructor Destroy; override;
  end;

  TCovidScriptPlugin = class (TFHIRWebServerScriptPlugin)
  private
    FSource : TFHIRWebServerSourceProvider;
    FContext : TFHIRServerContext;
    function banner(context : TCovidScriptContext; formName : String; variables : TFslMap<TFHIRObject>) : String;
    function newPatient(context : TCovidScriptContext; formName : String) : String;
    function editPatient(context : TCovidScriptContext; formName : String; variables : TFslMap<TFHIRObject>) : String;
    function newObservation(context : TCovidScriptContext; formName : String) : String;
    function matches(context : TCovidScriptContext; rowName : String; pm : THTTPParameters) : String;
  public
    constructor Create(source : TFHIRWebServerSourceProvider; context : TFHIRServerContext);
    destructor Destroy; override;

    function process(s : String; request : TIdHTTPRequestInfo; pm : THTTPParameters; variables : TFslMap<TFHIRObject>; Session: TFHIRSession; client : TFhirClientV) : String; override;
    function processPage(request : TIdHTTPRequestInfo; pm : THTTPParameters; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject>; client : TFhirClientV) : boolean; override;
  end;

implementation

{ TCovidScriptPlugin }

constructor TCovidScriptPlugin.Create(source: TFHIRWebServerSourceProvider; context : TFHIRServerContext);
begin
  inherited Create;
  FSource := source;
  FContext := context;
end;

destructor TCovidScriptPlugin.Destroy;
begin
  FSource.Free;
  FContext.Free;
  inherited;
end;

function TCovidScriptPlugin.matches(context : TCovidScriptContext; rowName: String; pm : THTTPParameters): String;
var
  bnd : TFHIRBundle;
  be : TFhirBundleEntry;
  liquid : TFHIRLiquidEngine;
  doc : TFHIRLiquidDocument;
  params : TStringList;
begin
  result := '<p>Matches:</p>';
  result := result + '<table class="grid">'+#13#10;
  params := TStringList.Create;
  try
    if pm.has('name') then
      params.Values['name'] := pm['name'];
    if pm.has('gender') then
      params.Values['gender'] := pm['gender'];
    if pm.has('dob') then
      params.Values['birthdate'] := pm['dob'];
    if pm.has('phone') then
      params.Values['phone'] := pm['phone'];
    if pm.has('email') then
      params.Values['email'] := pm['email'];

    bnd := context.FClient.search(frtPatient, true, params);
    try
      if bnd.entryList.count = 0 then
        exit('<p><i>No Matching Patients Found</i></p>');
      liquid := TFHIRLiquidEngine.create(TFHIRPathEngine4.Create(FContext.ValidatorContext.link as TFHIRWorkerContext, nil));
      try
        liquid.Engine.registerExtension(TCovidExtension.Create(FContext.Link, context.Link as TCovidScriptContext));
        doc := liquid.parse(FSource.getSource(rowName), rowName);
        try
          for be in bnd.entryList do
            result := result + liquid.evaluate(doc, be.resource, nil);
        finally
          doc.free;
        end;
      finally
        liquid.Free;
      end;
    finally
      bnd.free;
    end;
  finally
    params.Free;
  end;
  result := result + '</table>'+#13#10;
end;

function TCovidScriptPlugin.newPatient(context : TCovidScriptContext; formName: String): String;
var
  pat : TFHIRPatient;
  liquid : TFHIRLiquidEngine;
  doc : TFHIRLiquidDocument;
begin
  pat := TFHIRPatient.create;
  try
    liquid := TFHIRLiquidEngine.create(TFHIRPathEngine4.Create(FContext.ValidatorContext.link as TFHIRWorkerContext, nil));
    try
      liquid.Engine.registerExtension(TCovidExtension.Create(FContext.link, context.Link as TCovidScriptContext));
      doc := liquid.parse(FSource.getSource(formName), formName);
      try
        result := '<hr/><p>Didn''t find a match? Add a new Patient:</p><form method="POST" action="covid-patient.html"/>'#13#10+liquid.evaluate(doc, pat, nil)+'<input type="submit" value="Create"/></form>';
      finally
        doc.free;
      end;
    finally
      liquid.Free;
    end;
  finally
    pat.free;
  end;
end;

function TCovidScriptPlugin.newObservation(context : TCovidScriptContext; formName: String): String;
var
  obs : TFHIRObservation;
  liquid : TFHIRLiquidEngine;
  doc : TFHIRLiquidDocument;
begin
  obs := TFHIRObservation.create;
  try
    liquid := TFHIRLiquidEngine.create(TFHIRPathEngine4.Create(FContext.ValidatorContext.link as TFHIRWorkerContext, nil));
    try
      liquid.Engine.registerExtension(TCovidExtension.Create(FContext.Link, context.Link as TCovidScriptContext));
      doc := liquid.parse(FSource.getSource(formName), formName);
      try
        result := '</p><form method="POST" action="covid-observation.html"/>'#13#10+liquid.evaluate(doc, obs, nil)+'<input type="submit" value="Create"/></form>';
      finally
        doc.free;
      end;
    finally
      liquid.Free;
    end;
  finally
    obs.free;
  end;
end;

function TCovidScriptPlugin.banner(context : TCovidScriptContext; formName: String; variables : TFslMap<TFHIRObject>): String;
var
  liquid : TFHIRLiquidEngine;
  doc : TFHIRLiquidDocument;
begin
  liquid := TFHIRLiquidEngine.create(TFHIRPathEngine4.Create(FContext.ValidatorContext.link as TFHIRWorkerContext, nil));
  try
    liquid.Engine.registerExtension(TCovidExtension.Create(FContext.link, context.Link as TCovidScriptContext));
    doc := liquid.parse(FSource.getSource(formName), formName);
    try
      result := liquid.evaluate(doc, context.getPatient, nil);
    finally
      doc.free;
    end;
  finally
    liquid.Free;
  end;
end;


function TCovidScriptPlugin.editPatient(context : TCovidScriptContext; formName: String; variables : TFslMap<TFHIRObject>): String;
var
  liquid : TFHIRLiquidEngine;
  doc : TFHIRLiquidDocument;
  tuple : TFHIRSystemTuple;
begin
  liquid := TFHIRLiquidEngine.create(TFHIRPathEngine4.Create(FContext.ValidatorContext.link as TFHIRWorkerContext, nil));
  try
    liquid.Engine.registerExtension(TCovidExtension.Create(FContext.Link, context.Link as TCovidScriptContext));
    tuple := TFHIRSystemTuple.Create;
    try
      tuple.Fields.addAll(variables);
      doc := liquid.parse(FSource.getSource(formName), formName);
      try
        result := liquid.evaluate(doc, context.getPatient, tuple);
      finally
        doc.free;
      end;
    finally
      tuple.Free;
    end;
  finally
    liquid.Free;
  end;
end;

function TCovidScriptPlugin.process(s: String; request: TIdHTTPRequestInfo; pm : THTTPParameters; variables : TFslMap<TFHIRObject>; Session: TFHIRSession; client : TFhirClientV): String;
var
  b, e : integer;
  t, n : String;
  context : TCovidScriptContext;
begin
   context := TCovidScriptContext.create(client.link as TFhirClient4, pm['patient']);
   try
     b := s.IndexOf('[%covid.');
     while b > 0 do
     begin
       t := s.Substring(b);
       s := s.Substring(0, b);
       e := t.IndexOf('%]');
       n := t.Substring(8, e-8);
       if n.StartsWith('matches ') and (request.UnparsedParams <> '') then
         s := s + matches(context, n.Substring(8), pm)+t.Substring(e+2)
       else if n.StartsWith('newpatient ') and (request.UnparsedParams <> '') then
         s := s + newPatient(context, n.Substring(11))+t.Substring(e+2)
       else if n.StartsWith('newobservation ') and (request.UnparsedParams <> '') then
         s := s + newObservation(context, n.Substring(15))+t.Substring(e+2)
       else if n.StartsWith('editpatient ') then
         s := s + editPatient(context, n.Substring(12), variables)+t.Substring(e+2)
       else if n.StartsWith('patient-banner ') then
         s := s + banner(context, n.Substring(15), variables)+t.Substring(e+2)
       else if n = 'patient' then
         s := s + pm['patient'] + t.Substring(e+2)
       else if (n = 'error') and (variables.ContainsKey('form-error')) then
         s := s + '<p style="color: maroon">'+variables['form-error'].primitiveValue+'</p>' + t.Substring(e+2)
       else
         s := s +t.Substring(e+2);
       b := s.IndexOf('[%covid.');
     end;
     result := s;
   finally
     context.free;
   end;
end;


function TCovidScriptPlugin.processPage(request: TIdHTTPRequestInfo; pm : THTTPParameters; response: TIdHTTPResponseInfo; Session: TFHIRSession; claimed, actual: String; secure: boolean; variables: TFslMap<TFHIRObject>; client : TFhirClientV): boolean;
var
  pat : TFhirPatient;
  i : integer;
  id : String;
begin
  if (request.CommandType = hcPost) and (request.Document = '/r4/covid-patient.html') and (not variables.ContainsKey('form')) then
  begin
    result := true;
    try
      if pm['patient'] <> '' then
        pat := client.readResourceV('Patient', pm['patient']) as TFhirPatient
      else
        pat := TFhirPatient.Create;

      // business rules
      if pm['given'] = '' then
        raise EFslException.Create('Error: A given name is required');
      if (pm['mobile'] = '') and (pm['email'] = '') then
        raise EFslException.Create('Error: A mobile phone or an email address is required');
      if (pm['gender'] <> '') and (StringArrayIndexOfSensitive(CODES_TFhirAdministrativeGenderEnum, pm['gender']) = -1) then
        raise EFslException.Create('Error: Invalid Gender: '+pm['gender']);
      if (pm['dob'] <> '') and not TFslDateTime.isValidXmlDate(pm['dob']) then
        raise EFslException.Create('Error: Invalid Date of Birth');

      if pat.nameList.IsEmpty then
        pat.nameList.Append;
      pat.nameList[0].family := pm['family'];
      pat.nameList[0].givenList.clear;
      pat.nameList[0].givenList.Append.value := pm['given'];
      if (pm['middle'] <> '') then
        pat.nameList[0].givenList.Append.value := pm['middle'];

      if (pm['gender'] <> '') then
        pat.gender := TFhirAdministrativeGenderEnum(StringArrayIndexOfSensitive(CODES_TFhirAdministrativeGenderEnum, pm['gender']));
      if (pm['dob'] <> '') then
        pat.birthDate := TFslDateTime.fromXML(pm['dob']);

      for i := pat.telecomList.Count -1 downto 0 do
        if pat.telecomList[i].use = ContactPointUseMobile then
          pat.telecomList.Remove(i);
      if pm['mobile'] <> '' then
        with pat.telecomList.Append do
        begin
          use := ContactPointUseMobile;
          system := ContactPointSystemPhone;
          value := pm['mobile'];
        end;

      for i := pat.telecomList.Count -1 downto 0 do
        if pat.telecomList[i].system = ContactPointSystemEmail then
          pat.telecomList.Remove(i);
      if pm['email'] <> '' then
        with pat.telecomList.Append do
        begin
          system := ContactPointSystemEmail;
          value := pm['email'];
        end;

      if pat.addressList.IsEmpty then
        pat.addressList.Append;
      pat.addressList[0].lineList.clear;
      if (pm['line1'] <> '') then
      begin
        pat.addressList[0].lineList.Append.value := pm['line1'];
        if (pm['line2'] <> '') then
          pat.addressList[0].lineList.Append.value := pm['line2'];
      end;
      pat.addressList[0].city := pm['city'];
      pat.addressList[0].state := pm['state'];
      pat.addressList[0].postalCode := pm['postcode'];

      if pm['patient'] <> '' then
      begin
        client.updateResourceV(pat);
        id := pat.id;
      end
      else
        client.createResourceV(pat, id);
      response.Redirect('/r4/covid-patient.html?id='+id);
    except
      on e : Exception do
      begin
        variables.Add('form', TFHIRSystemTuple.fromParams(pm));
        variables.Add('form-error', TFHIRSystemString.create(e.message));
        ProcessFile('covid-patient.html', request, pm, response, session, true, variables);
        response.ResponseNo := 500;
        response.ResponseText := 'Error';
      end;
    end;
  end
  else
    result := false;
end;

{ TCovidScriptContext }

constructor TCovidScriptContext.Create(client: TFhirClient4; id: String);
begin
  inherited create;
  FClient := client;
  FId := id;
end;

destructor TCovidScriptContext.Destroy;
begin
  FClient.Free;
  FPatient.free;
  inherited;
end;

function TCovidScriptContext.getPatient: TFhirPatient;
begin
  if FPatient = nil then
  begin
    FPatient := FClient.readResource(frtPatient, FId) as TFHIRPatient;
  end;
  result := FPatient;
end;

{ TCovidExtension }

constructor TCovidExtension.Create(context : TFHIRServerContext; covid : TCovidScriptContext);
begin
  inherited Create(context);
  FCovid := covid;
end;

destructor TCovidExtension.Destroy;
begin
  FCovid.free;
  inherited;
end;

function TCovidExtension.evaluateToString(context: TFHIRPathExecutionContext; engine: TFHIRPathEngineV; focus: TFHIRObject; param: TFHIRPathExpressionNodeV): String;
var
  c, n1 : TFHIRSelectionList;
begin
  c := TFHIRSelectionList.create(focus.link);
  try
    n1 := executeV(engine, context, c, param, true);
    try
      result := engine.convertToString(n1);
    finally
      n1.free;
    end;
  finally
    c.free;
  end;
end;

function TCovidExtension.execute(context: TFHIRPathExecutionContext; focus: TFHIRObject; name: String; params: TFslList<TFHIRPathExpressionNodeV>; engine: TFHIRPathEngineV): TFHIRSelectionList;
begin
  result := TFHIRSelectionList.create;
  try
    if (focus is TFHIRPathServerObject) then
    begin
      if name = 'htmlSelect' then
        result.add(TFHIRSelection.create(FContext.Factory.makeString(funcHtmlSelect(context, engine, focus, params))));
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TCovidExtension.funcHtmlSelect(context : TFHIRPathExecutionContext; engine : TFHIRPathEngineV; focus: TFHIRObject; params : TFslList<TFHIRPathExpressionNodeV>) : String;
var
  name, valueset, sel : String;
  vs : TFHIRValueSet;
  cc : TFhirValueSetExpansionContains;
  p : TFhirParameters;
  s : TFslStringBuilder;
begin
  name := evaluateToString(context, engine, focus, params[0]);
  valueSet := evaluateToString(context, engine, focus, params[1]);
  sel := evaluateToString(context, engine, focus, params[2]);
  p := TFhirParameters.Create;
  try
    with p.parameterList.Append do
    begin
      name := 'url';
      value := TFHIRString.create(valueSet);
    end;
    vs := FCovid.FClient.operation(frtValueSet, 'expand', p) as TFhirValueSet;
    try
      s := TFslStringBuilder.Create;
      try
        s.Append('<select name="');
        s.Append(name);
        s.Append('">'#13#10);
        for cc in vs.expansion.containsList do
        begin
          if cc.code <> '' then
          begin
            s.Append('  <option value="');
            s.append(cc.code);
            s.Append('" ');
            if cc.code = sel then
              s.Append(' selected');
            s.Append('>');
            if cc.display <> '' then
              s.Append(cc.display)
            else
              s.Append(cc.code);
            s.Append('</option>'#13#10);
          end;
        end;
        s.Append('</select>'#13#10);
        result := s.ToString;
      finally
        s.Free;
      end;
    finally
      vs.Free;
    end;
  finally
    p.Free;
  end;
end;

function TCovidExtension.functionApplies(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; name: String): boolean;
begin
  if (focus.Count = 1) and (focus[0].value is TFHIRPathServerObject) then
    result := StringArrayExistsSensitive(['htmlSelect'], name)
  else
    result := false;
end;

function TCovidExtension.isValidFunction(name: String): boolean;
begin
  result := StringArrayExistsSensitive(['htmlSelect'], name);
end;

function TCovidExtension.resolveConstant(context: TFHIRPathExecutionContext; s: String; var obj: TFHIRObject): boolean;
begin
  if s = '%covid' then
    raise Exception.Create('No Covid Specific object yet')
  else
    result := inherited resolveConstant(context, s, obj);
end;

end.
