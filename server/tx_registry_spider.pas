unit tx_registry_spider;

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
  SysUtils, Classes, IniFiles,
  IdHashSHA,
  fsl_base, fsl_utilities, fsl_json, fsl_logging, fsl_versions, fsl_http,
  fsl_fetcher, fsl_zulip,
  fhir_objects, fhir_client_http,
  fhir3_client, fhir3_types, fhir3_resources, fhir3_resources_canonical, fhir3_utilities,
  fhir4_client, fhir4_types, fhir4_resources_canonical, fhir4_utilities,
  fhir5_client, fhir5_types, fhir5_resources_canonical, fhir5_enums, fhir5_utilities,
  tx_registry_model;

const                                                        
  MASTER_URL = 'https://raw.githubusercontent.com/FHIR/ig-registry/master/tx-servers.json';
  EMAIL_DAYS_LIMIT = 7;

Type
  TSendEmailEvent = procedure (dest, subj, body : String) of object;

  TZulipItem = class (TFslObject)
  private
    FWhen : TDateTime;
  end;

  TZulipTracker = class (TFslObject)
  private
    FZulip : TZulipSender;
    FErrors : TFslMap<TZulipItem>;
  public
    constructor Create(address, email, apikey : String);
    destructor Destroy; override;

    function Link : TZulipTracker; overload;

    procedure error(err : String);
    procedure send;
  end;

  { TTxRegistryScanner }

  TTxRegistryScanner = class (TFslObject)
  private
    FTotalBytes : cardinal;
    FAddress: String;
    FErrors: String;
    FRegistryErrors : String;
    FOnSendEmail : TSendEmailEvent;
    FIni : TIniFile;
    FZulip : TZulipTracker;
    procedure DoSendEmail(dest, subj, body : String);
    procedure log(msg, source : String; error : boolean);

    function fetchUrl(url, mimetype : string) : TBytes;
    function fetchJson(url : string) : TJsonObject;

    procedure processServerVersionR3(version, source, url : String; ver : TServerVersionInformation);
    procedure processServerVersionR4(version, source, url : String; ver : TServerVersionInformation);
    procedure processServerVersionR5(version, source, url : String; ver : TServerVersionInformation);
    procedure processServerVersion(source : String; srvr: TServerInformation; obj : TJsonObject; ver : TServerVersionInformation);
    procedure processServer(source : String; obj : TJsonObject; srvr : TServerInformation);
    procedure processRegistry(obj : TJsonObject; reg : TServerRegistry);

  public
    constructor Create(zulip : TZulipTracker);
    destructor Destroy; override;

    procedure update(name : String; info : TServerRegistries);

    property address : String read FAddress write FAddress;
    property errors : String read FErrors;
    property OnSendEmail : TSendEmailEvent read FOnSendEmail write FOnSendEmail;
  end;

implementation

function fix(url : String) : String;
begin
  result := url.Replace('http:', 'https:');
end;

{ TTxRegistryScanner }

constructor TTxRegistryScanner.Create(zulip: TZulipTracker);
begin
  inherited Create;
  FZulip := zulip;
  FAddress := MASTER_URL;
end;

destructor TTxRegistryScanner.Destroy;
begin
  FZulip.free;
  inherited;
end;

procedure TTxRegistryScanner.DoSendEmail(dest, subj, body: String);
var
  dt : TDateTime;
begin
  dt := FIni.ReadDate('sent', dest, 0);
  if dt < now - EMAIL_DAYS_LIMIT then
  begin
    FIni.WriteDate('sent', dest, now);
    FOnSendEmail(dest, subj, body);
  end;
end;

function TTxRegistryScanner.fetchUrl(url, mimetype: string): TBytes;
var
  fetcher : TInternetFetcher;
begin
  fetcher := TInternetFetcher.Create;
  try
    fetcher.URL := url+'?'+TFslDateTime.makeLocal().toHL7;
    // fetcher.Accept := mimetype;
    fetcher.userAgent := 'HealthIntersections/FhirServer';
    fetcher.Fetch;
    result := fetcher.Buffer.AsBytes;
  finally
    fetcher.free;
  end;
  FTotalBytes := FTotalBytes + length(result);
end;

function TTxRegistryScanner.fetchJson(url: string): TJsonObject;
begin
  result := TJSONParser.Parse(fetchUrl(url, 'application/json'));
end;

procedure TTxRegistryScanner.log(msg, source: String; error : boolean);
begin
  if error then
  begin
    FErrors := FErrors + msg+' (from '+source+')'+#13#10;
    FRegistryErrors := FRegistryErrors + msg+' (from '+source+')'+#13#10;
    if FZulip <> nil then
      FZulip.error(msg+' (from '+source+')');
  end;
  Logging.log(msg);
end;

procedure TTxRegistryScanner.update(name : String; info : TServerRegistries);
var
  json : TJsonObject;
  arr : TJsonArray;
  i : integer;
  reg : TServerRegistry;
begin
  FIni := TIniFile.Create(tempFile('tx-registry-'+name+'.ini'));
  try
    info.LastRun := TFslDateTime.makeUTC;
    log('Start Tx-Server registry Scan', '', false);
    FTotalBytes := 0;
    FErrors := '';
    try
      log('Fetch '+FAddress, '', false);
      json := fetchJson(FAddress);
      try
        if json.str['version'] <> '1' then
          raise EFslException.Create('Unable to proceed: registries version is '+json.str['version']+' not "1"');

        arr := json.arr['registries'];
        for i := 0 to arr.Count - 1 do
        begin
          reg := TServerRegistry.Create;
          try
            info.Registries.add(reg.link);
            processRegistry(arr.Obj[i], reg);
          finally
            reg.free;
          end;
        end;
      finally
        json.free;
      end;
      info.Outcome := 'All OK';
    except
      on e : Exception do
      begin
        Log('Exception Processing TX Registry: '+e.Message, FAddress, true);
        info.Outcome := 'Error: '+e.message;
      end;
    end;
    if FZulip <> nil then
      FZulip.send;
    log('Finish txRegistry Scan - '+Logging.DescribeSize(FTotalBytes, 0), '', false);
  finally
    FIni.free;
  end;
end;

procedure TTxRegistryScanner.processRegistry(obj : TJsonObject; reg : TServerRegistry);
var
  json : TJsonObject;
  arr : TJsonArray;
  i : integer;
  srvr : TServerInformation;
begin
  try
    reg.Code := obj.str['code'];
    reg.Name := obj.str['name'];
    if (reg.Name = '') then
      raise EFslException.Create('No name provided');
    reg.Authority := obj.str['authority'];
    reg.Address := obj.str['url'];
    if (reg.Address = '') then
      raise EFslException.Create('No url provided for '+reg.Name);

    log('Fetch '+reg.Address, FAddress, false);
    FRegistryErrors := '';
    json := fetchJson(reg.Address);
    try
      if json.str['version'] <> '1' then
        raise EFslException.Create('Unable to proceed: registry version @'+reg.Address+' is '+json.str['version']+' not "1"');
      
      arr := json.arr['servers'];
      for i := 0 to arr.Count - 1 do
      begin
        srvr := TServerInformation.Create;
        try
          reg.Servers.add(srvr.link);
          processServer(reg.Address, arr.Obj[i], srvr);
        finally
          srvr.free;
        end;
      end;
    finally
      json.free;
    end;

    //if (FRegistryErrors <> '') and (email <> '') then
    //    DoSendEmail(email, 'Errors Processing '+reg.Address, FRegistryErrors);
  except
    on e : Exception do
    begin
      log('Exception processing registry: '+reg.Name+'@'+reg.address+' : '+e.Message, FAddress, false);
      //if (email <> '') then
      //  DoSendEmail(email, 'Exception Processing '+reg.Name+'@'+reg.address, e.Message);
    end;
  end;
end;

procedure TTxRegistryScanner.processServer(source : String; obj: TJsonObject; srvr: TServerInformation);
var
  json : TJsonObject;
  arr : TJsonArray;
  i : integer;
  v : TServerVersionInformation;
begin
  srvr.Code := obj.str['code'];
  srvr.Name := obj.str['name'];
  if (srvr.Name = '') then
    raise EFslException.Create('No name provided');
  srvr.AccessInfo := obj.str['access_info'];
  srvr.Address := obj.str['url'];
  if (srvr.Address = '') then
    raise EFslException.Create('No url provided for '+srvr.Name);
  obj.forceArr['authoritative'].readStrings(srvr.AuthList);

  arr := obj.arr['versions'];
  for i := 0 to arr.Count - 1 do
  begin
    v := TServerVersionInformation.Create;
    try
      srvr.Versions.add(v.link);
      processServerVersion(source, srvr, arr.Obj[i], v);
    finally
      v.free;
    end;
  end;
end;


procedure TTxRegistryScanner.processServerVersion(source: String; srvr: TServerInformation; obj: TJsonObject; ver: TServerVersionInformation);
var
  v : TSemanticVersion;
begin   
  try
    ver.Address := obj.str['url']; 
    ver.Security := [ssOpen];
    v := TSemanticVersion.fromString(obj.str['version']);
    case v.Major of
      3: processServerVersionR3(obj.str['version'], source, obj.str['url'], ver);
      4: processServerVersionR4(obj.str['version'], source, obj.str['url'], ver);
      5: processServerVersionR5(obj.str['version'], source, obj.str['url'], ver);
    else
      log('Exception processing server: '+srvr.Name+'@'+srvr.address+' : Version '+obj.str['version']+' not supported', source, false);
    end;
    ver.LastSuccess := TFslDateTime.makeUTC;
  except
    on e : Exception do
      ver.Error := e.message;
  end;
end;

procedure TTxRegistryScanner.processServerVersionR4(version, source, url : String; ver : TServerVersionInformation);
var
  client : TFhirClient4;
  cs : fhir4_resources_canonical.TFhirCapabilityStatement;
  csr : fhir4_resources_canonical.TFhirCapabilityStatementRest;
  cc : fhir4_types.TFhirCodeableConcept;
  tc :  fhir4_resources_canonical.TFhirTerminologyCapabilities;
  tcs : fhir4_resources_canonical.TFhirTerminologyCapabilitiesCodeSystem;
  tcsv : fhir4_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemVersion;
begin
  client := TFhirClient4.Create(nil, nil, TFHIRHTTPCommunicator.Create(url));
  try
    client.format := ffJson;
    cs := client.conformance(true);
    try
      ver.Version := cs.fhirVersionElement.ToString;
      for csr in cs.restList do
        if (csr.mode = fhir4_types.RestfulCapabilityModeServer) then
        begin
          if csr.security <> nil then
            for cc in csr.security.serviceList do
            begin
              if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'OAuth')) then
                ver.Security := ver.Security + [ssOAuth]
              else if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'SMART-on-FHIR')) then
                ver.Security := ver.Security + [ssSmart]
              else if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'Basic')) then
                ver.Security := ver.Security + [ssPassword]
              else if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'Certificates')) then
                ver.Security := ver.Security + [ssCert]
              else if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'Token')) then
                ver.Security := ver.Security + [ssToken]
              else if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'Open')) then
                ver.Security := ver.Security + [ssOpen];
            end;
        end;
    finally
      cs.free;
    end;
    tc := client. terminologyCaps;
    try
      for tcs in tc.codeSystemList do
      begin
        ver.Terminologies.add(tcs.uri);
        for tcsv in tcs.versionList do
          ver.Terminologies.add(tcs.uri+'|'+tcsv.code);
      end;
    finally
      tc.free;
    end;
  finally
    client.free;
  end;
end;

procedure TTxRegistryScanner.processServerVersionR5(version, source, url : String; ver : TServerVersionInformation);
var
  client : TFhirClient5;
  cs : fhir5_resources_canonical.TFhirCapabilityStatement;
  csr : fhir5_resources_canonical.TFhirCapabilityStatementRest;
  cc : fhir5_types.TFhirCodeableConcept;
  tc :  fhir5_resources_canonical.TFhirTerminologyCapabilities;
  tcs : fhir5_resources_canonical.TFhirTerminologyCapabilitiesCodeSystem;
  tcsv : fhir5_resources_canonical.TFhirTerminologyCapabilitiesCodeSystemVersion;
begin
  client := TFhirClient5.Create(nil, nil, TFHIRHTTPCommunicator.Create(url));
  try                    
    client.format := ffJson;
    cs := client.conformance(true);
    try                            
      ver.Version := cs.fhirVersionElement.ToString;
      for csr in cs.restList do
        if (csr.mode = fhir5_enums.RestfulCapabilityModeServer) then
        begin
          if csr.security <> nil then
            for cc in csr.security.serviceList do
            begin
              if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'OAuth')) then
                ver.Security := ver.Security + [ssOAuth]
              else if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'SMART-on-FHIR')) then
                ver.Security := ver.Security + [ssSmart]
              else if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'Basic')) then
                ver.Security := ver.Security + [ssPassword]
              else if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'Certificates')) then
                ver.Security := ver.Security + [ssCert]
              else if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'Token')) then
                ver.Security := ver.Security + [ssToken]
              else if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'Open')) then
                ver.Security := ver.Security + [ssOpen];
            end;
        end;
    finally
      cs.free;
    end;
    tc := client.terminologyCaps;
    try
      for tcs in tc.codeSystemList do
      begin
        ver.Terminologies.add(tcs.uri);
        for tcsv in tcs.versionList do
          ver.Terminologies.add(tcs.uri+'|'+tcsv.code);
      end;
    finally
      tc.free;
    end;
  finally
    client.free;
  end;
end;

procedure TTxRegistryScanner.processServerVersionR3(version, source, url : String; ver : TServerVersionInformation);
var
  client : TFhirClient3;
  cs : fhir3_resources_canonical.TFhirCapabilityStatement;
  csr : fhir3_resources_canonical.TFhirCapabilityStatementRest;
  cc : fhir3_types.TFhirCodeableConcept;
  tc :  fhir3_resources.TFhirParameters;
  tcs : fhir3_resources.TFhirParametersParameter;
  tcsv : fhir3_resources.TFhirParametersParameter;
  n : String;
begin
  client := TFhirClient3.Create(nil, nil, TFHIRHTTPCommunicator.Create(url));
  try     
    client.format := ffJson;
    cs := client.conformance(true);
    try      
      ver.Version := cs.fhirVersionElement.ToString;
      for csr in cs.restList do
        if (csr.mode = fhir3_types.RestfulCapabilityModeServer) then
        begin
          if csr.security <> nil then
            for cc in csr.security.serviceList do
            begin
              if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'OAuth')) then
                ver.Security := ver.Security + [ssOAuth]
              else if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'SMART-on-FHIR')) then
                ver.Security := ver.Security + [ssSmart]
              else if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'Basic')) then
                ver.Security := ver.Security + [ssPassword]
              else if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'Certificates')) then
                ver.Security := ver.Security + [ssCert]
              else if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'Token')) then
                ver.Security := ver.Security + [ssToken]
              //else if (cc.hasCode('http://hl7.org/fhir/restful-security-service', 'Open')) then
              //  ver.Security := ver.Security + [ssOpen];
            end;
        end;
    finally
      cs.free;
    end;
    tc := client.terminologyCaps;
    try
      for tcs in tc.parameterList do
      begin
        n := tcs.name;
        if (n = 'system') then
        begin
          ver.Terminologies.add(tcs.value.primitiveValue);
          for tcsv in tcs.partList do
          begin
            n := tcsv.name;
            if (n = 'version') then
              ver.Terminologies.add(tcs.value.primitiveValue+'|'+tcsv.value.primitiveValue);
          end;
        end;
      end;
    finally
      tc.free;
    end;
  finally
    client.free;
  end;
end;

{ TZulipTracker }

constructor TZulipTracker.Create(address, email, apikey: String);
begin
  inherited Create;
  FZulip := TZulipSender.Create(address, email, apikey);
  FErrors := TFslMap<TZulipItem>.Create;
end;

destructor TZulipTracker.Destroy;
begin
  FErrors.free;
  FZulip.free;
  inherited;
end;

procedure TZulipTracker.error(err: String);
begin
  if not FErrors.ContainsKey(err) then
    FErrors.Add(err, TZulipItem.create);
end;

function TZulipTracker.Link: TZulipTracker;
begin
  result := TZulipTracker(inherited link);
end;

procedure TZulipTracker.send;
var
  msg : String;
  s : string;
  item : TZulipItem;
begin
  msg := '';
  for s in Ferrors.Keys do
  begin
    item := FErrors[s];
    if item.FWhen < now - 1 then
    begin
      msg := msg + '* '+s+#13#10;
      item.FWhen := now;
    end;
  end;
  if msg <> '' then
  begin
    Logging.log('Send to Zulip: '+msg);
    FZulip.sendMessage('tooling/Package Crawlers', 'Packages2', msg);
  end;
end;

end.
