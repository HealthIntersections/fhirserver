unit FHIRDemoLogging;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{
Simple Logging framework - can log to a file, or to a FHIR Endpoint that
handles AuditEvent resources (e.g. RESTful ATNA)

Note: at this time, Argonaut EHRs do not offer this service, and have
no plans to offer this, but the standard general purpose FHIR servers do.

}

interface

uses
  SysUtils, Classes, Generics.Collections,
  fsl_utilities, fsl_json, fsl_logging,
  fhir_objects,  fhir_oauth, fhir_client, fhir_factory;

type
  TLoggingService = class abstract
  private
    FOpenIdToken: TJWT; // will be nil in case of a system login
    FServer: TRegisteredFHIRServer;
    procedure SetOpenIdToken(const Value: TJWT);
    procedure SetServer(const Value: TRegisteredFHIRServer);

    function encodeParams(params : TStringList) : String;
  public
    destructor Destroy; override;
    Property openIdToken : TJWT read FOpenIdToken write SetOpenIdToken;
    Property Server : TRegisteredFHIRServer read FServer write SetServer;

    procedure recordLogin; virtual;
    procedure recordLogout; virtual;
    procedure recordResourceReadSuccess(logId : String; resourceType : String; id : string; r : TFHIRResourceV); virtual;
    procedure recordResourceReadFail(logId : String; resourceType : String; id : string; e : exception = nil); virtual;
    procedure recordResourceSearchSuccess(logId : String; resourceType : String; params : TStringList; bnd : TFHIRResourceV); virtual;
    procedure recordResourceSearchFail(logId : String; resourceType : String; params : TStringList; e : exception = nil); virtual;
  end;

  TDemoHttpLogger = class (TFHIRClientLogger)
  private
    FLog : TLogger;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure logExchange(verb, url, status, requestHeaders, responseHeaders : String; request, response : TBytes); override;
  end;

  TFileLoggingService = class (TLoggingService)
  private
    FFile : TStream;
    procedure log(s : String);
    function resourceToString(r : TFHIRResourceV) : String;
  public
    constructor Create(filename : String);
    destructor Destroy; override;

    procedure recordLogin; override;
    procedure recordLogout; override;
    procedure recordResourceReadSuccess(logId : String; resourceType : String; id : string; r : TFHIRResourceV); override;
    procedure recordResourceReadFail(logId : String; resourceType : String; id : string; e : exception = nil); override;
    procedure recordResourceSearchSuccess(logId : String; resourceType : String; params : TStringList; bnd : TFHIRResourceV); override;
    procedure recordResourceSearchFail(logId : String; resourceType : String; params : TStringList; e : exception = nil); override;
  end;

  TAuditEventLoggingService = class (TLoggingService)
  private
    FClient : TFhirClientV;
//    procedure log(ae : TFhirAuditEvent);
  public
    constructor Create(client : TFHIRClientV);
    destructor Destroy; override;

    procedure recordLogin; override;
    procedure recordLogout; override;
    procedure recordResourceReadSuccess(logId : String; resourceType : String; id : string; r : TFHIRResourceV); override;
    procedure recordResourceReadFail(logId : String; resourceType : String; id : string; e : exception = nil); override;
    procedure recordResourceSearchSuccess(logId : String; resourceType : String; params : TStringList; bnd : TFHIRResourceV); override;
    procedure recordResourceSearchFail(logId : String; resourceType : String; params : TStringList; e : exception = nil); override;
  end;

implementation

{ TLoggingService }

destructor TLoggingService.Destroy;
begin
  FServer.Free;
  FOpenIdToken.Free;
  inherited;
end;

procedure TLoggingService.recordResourceReadFail(logId : String; resourceType: String; id: string; e: exception);
begin
  raise EFHIRException.create('Need to override recordResourceReadFail in ' + className);
end;

procedure TLoggingService.recordResourceReadSuccess(logId : String; resourceType: String; id: string; r : TFHIRResourceV);
begin
  raise EFHIRException.create('Need to override recordResourceReadSuccess in ' + className);
end;

procedure TLoggingService.recordResourceSearchFail(logId : String; resourceType: String; params: TStringList; e : exception = nil);
begin
  raise EFHIRException.create('Need to override recordResourceSearchFail in ' + className);
end;

procedure TLoggingService.recordResourceSearchSuccess(logId : String; resourceType: String; params: TStringList; bnd: TFHIRResourceV);
begin
  raise EFHIRException.create('Need to override recordResourceSearchSuccess in ' + className);
end;

function TLoggingService.encodeParams(params: TStringList): String;
var
  i : integer;
begin
  result := '';
  for i := 0 to params.Count - 1 do
  begin
     if i > 0 then
       result := result + '&';
     result := result + EncodeMIME(params.Names[i])  + '=' + EncodeMIME(params.ValueFromIndex[i]);
  end;
end;

procedure TLoggingService.recordLogin;
begin
  raise EFHIRException.create('Need to override recordLogin in ' + className);
end;

procedure TLoggingService.recordLogout;
begin
  raise EFHIRException.create('Need to override recordLogout in ' + className);
end;

procedure TLoggingService.SetOpenIdToken(const Value: TJWT);
begin
  FOpenIdToken.Free;
  FOpenIdToken := Value;
end;

procedure TLoggingService.SetServer(const Value: TRegisteredFHIRServer);
begin
  FServer.Free;
  FServer := Value;
end;

{ TFileLoggingService }

constructor TFileLoggingService.Create(filename: String);
begin
  inherited Create;
  if not FileExists(filename) then
    FFile := TFileStream.Create(filename, fmCreate)
  else
    FFile := TFileStream.Create(filename, fmOpenWrite + fmShareDenyWrite);
  FFile.Position := FFile.Size;
end;

destructor TFileLoggingService.Destroy;
begin
  FFile.Free;
  inherited;
end;

procedure TFileLoggingService.log(s: String);
var
  b : TBytes;
begin
  s := '[' + DateTimeToStr(Now) + ']: ' + s;
  b := TEncoding.UTF8.GetBytes(s + #13#10);
  FFile.Write(b[0], length(b));
end;

procedure TFileLoggingService.recordResourceReadFail(logId : String; resourceType: String; id: string; e : exception);
begin
  if openIdToken = nil then
    log('Read Resource ' + Server.fhirEndpoint + '/' + resourceType + '/' + id + ': error = ' + e.Message + ', id = ' + logid)
  else
    log('Read Resource ' + Server.fhirEndpoint + '/' + resourceType + '/' + id + ' by ' + openIdToken.id + ': error = ' + e.Message + ', id = ' + logid)
end;

procedure TFileLoggingService.recordResourceReadSuccess(logId : String; resourceType: String; id: string; r: TFHIRResourceV);
begin
  if openIdToken = nil then
    log('Read Resource ' + Server.fhirEndpoint + '/' + resourceType + '/' + id + ', id = ' + logid)
  else
    log('Read Resource ' + Server.fhirEndpoint + '/' + resourceType + '/' + id + ' by ' + openIdToken.id + ', id = ' + logid);
  log('== Content: ==');
  log(resourceToString(r));
  log('===============');
end;

procedure TFileLoggingService.recordResourceSearchFail(logId : String; resourceType: String; params: TStringList; e : exception);
begin
  if openIdToken = nil then
    log('Search Resources ' + Server.fhirEndpoint + '/' + resourceType + '?' + encodeParams(params) + ': error = ' + e.Message + ', id = ' + logid)
  else
    log('Search Resources ' + Server.fhirEndpoint + '/' + resourceType + '?' + encodeParams(params) + ' by ' + openIdToken.id + ': error = ' + e.Message + ', id = ' + logid)
end;

procedure TFileLoggingService.recordResourceSearchSuccess(logId : String; resourceType: String; params: TStringList; bnd: TFHIRResourceV);
begin
  if openIdToken = nil then
    log('Search Resources ' + Server.fhirEndpoint + '/' + resourceType + '?' + encodeParams(params) + ', id = ' + logid)
  else
    log('Search Resources ' + Server.fhirEndpoint + '/' + resourceType + '?' + encodeParams(params) + ' by ' + openIdToken.id + ', id = ' + logid);
  log('== Content: ==');
  log(resourceToString(bnd));
  log('===============');
end;

function TFileLoggingService.resourceToString(r: TFHIRResourceV): String;
begin
  result := 'todo';
end;

procedure TFileLoggingService.recordLogin;
begin
  if openIdToken = nil then
    log('System Login to ' + Server.fhirEndpoint)
  else
    log('Login to ' + Server.fhirEndpoint + ' by ' + openIdToken.id);
end;

procedure TFileLoggingService.recordLogout;
begin
  if Server = nil then
    exit;

  if openIdToken = nil then
    log('System Logout from ' + Server.fhirEndpoint)
  else
    log('Logout from ' + Server.fhirEndpoint + ' by ' + openIdToken.id);
end;

{ TAuditEventLoggingService }

constructor TAuditEventLoggingService.Create(client: TFHIRClientV);
begin
  inherited Create;
  FClient := client;
end;

destructor TAuditEventLoggingService.Destroy;
begin
  FClient.Free;
  inherited;
end;

//procedure TAuditEventLoggingService.log(ae: TFhirAuditEvent);
//var
//  id : string;
//begin
//  FClient.createResource(ae, id);
//end;
//
procedure TAuditEventLoggingService.recordResourceReadFail(logId : String; resourceType: String; id: string; e : exception);
//var
//  se: TFhirAuditEvent;
//  C: TFHIRCoding;
//  p: TFhirAuditEventParticipant;
//  o : TFhirAuditEventObject;
begin
//  se := TFhirAuditEvent.Create;
//  try
//    se.event := TFhirAuditEventEvent.Create;
//    se.event.type_ := TFHIRCoding.Create;
//    C := se.event.type_;
//    C.code := 'rest';
//    C.system := 'http://hl7.org/fhir/security-event-type';
//    C.Display := 'Restful Operation';
//    C := se.event.subtypeList.append;
//    C.code := 'read';
//    C.system := 'http://hl7.org/fhir/restful-operation';
//    C.Display := 'Read';
//    se.event.action := AuditEventActionE;
//    se.event.outcome := AuditEventOutcome8;
//    se.event.outcomeDesc := e.Message;
//    se.event.dateTime := TFslDateTime.makeUTC;
//    se.event.id := logId;
//    se.source := TFhirAuditEventSource.Create;
//    se.source.site := Server.name;
//    se.source.identifier := TFhirIdentifier.Create;
//    se.source.identifier.system := 'urn:ietf:rfc:3986';
//    se.source.identifier.value := Server.fhirEndpoint;
//    C := se.source.type_List.append;
//    C.code := '3';
//    C.Display := 'Web Server';
//    C.system := 'http://hl7.org/fhir/security-source-type';
//
//    // participant - the web browser / user proxy
//    if openIdToken = nil then
//    begin
//      p := se.participantList.append;
//      p.userId := TFhirIdentifier.Create;
//      p.userId.system := Server.fhirEndpoint;
//      p.userId.value := openIdToken.id;
//      p.name := openIdToken.name;
//    end;
//
//    o := se.object_List.Append;
//    o.reference := TFhirReference.create;
//    o.reference.reference := resourceType + '/' + id;
//    o.type_ := TFhirCoding.Create;
//    o.type_.system := 'http://hl7.org/fhir/security-source-type';
//    o.type_.code := '2';
//    o.lifecycle := TFhirCoding.Create;
//    o.lifecycle.system := 'http://hl7.org/fhir/object-lifecycle';
//    o.lifecycle.code := '6';
//
//    log(se);
//  finally
//    se.free;
//  end;
end;


procedure TAuditEventLoggingService.recordResourceReadSuccess(logId : String; resourceType: String; id: string; r: TFHIRResourceV);
//var
//  se: TFhirAuditEvent;
//  C: TFHIRCoding;
//  p: TFhirAuditEventParticipant;
//  o : TFhirAuditEventObject;
begin
//  se := TFhirAuditEvent.Create;
//  try
//    se.event := TFhirAuditEventEvent.Create;
//    se.event.type_ := TFHIRCoding.Create;
//    C := se.event.type_;
//    C.code := 'rest';
//    C.system := 'http://hl7.org/fhir/security-event-type';
//    C.Display := 'Restful Operation';
//    C := se.event.subtypeList.append;
//    C.code := 'read';
//    C.system := 'http://hl7.org/fhir/restful-operation';
//    C.Display := 'Read';
//    se.event.action := AuditEventActionE;
//    se.event.outcome := AuditEventOutcome0;
//    se.event.dateTime := TFslDateTime.makeUTC;
//    se.event.id := logId;
//    se.source := TFhirAuditEventSource.Create;
//    se.source.site := Server.name;
//    se.source.identifier := TFhirIdentifier.Create;
//    se.source.identifier.system := 'urn:ietf:rfc:3986';
//    se.source.identifier.value := Server.fhirEndpoint;
//    C := se.source.type_List.append;
//    C.code := '3';
//    C.Display := 'Web Server';
//    C.system := 'http://hl7.org/fhir/security-source-type';
//
//    if openIdToken = nil then
//    begin
//      // participant - the web browser / user proxy
//      p := se.participantList.append;
//      p.userId := TFhirIdentifier.Create;
//      p.userId.system := Server.fhirEndpoint;
//      p.userId.value := openIdToken.id;
//      p.name := openIdToken.name;
//    end;
//
//    o := se.object_List.Append;
//    o.reference := TFhirReference.create;
//    o.reference.reference := resourceType + '/' + id;
//    o.type_ := TFhirCoding.Create;
//    o.type_.system := 'http://hl7.org/fhir/security-source-type';
//    o.type_.code := '2';
//    o.lifecycle := TFhirCoding.Create;
//    o.lifecycle.system := 'http://hl7.org/fhir/object-lifecycle';
//    o.lifecycle.code := '6';
//    o.detailList.Append.value := resourceToBytes(r, ffJson);
//    log(se);
//  finally
//    se.free;
//  end;
end;

procedure TAuditEventLoggingService.recordResourceSearchFail(logId : String; resourceType: String; params: TStringList; e : exception);
//var
//  se: TFhirAuditEvent;
//  C: TFHIRCoding;
//  p: TFhirAuditEventParticipant;
//  o : TFhirAuditEventObject;
begin
//  se := TFhirAuditEvent.Create;
//  try
//    se.event := TFhirAuditEventEvent.Create;
//    se.event.type_ := TFHIRCoding.Create;
//    C := se.event.type_;
//    C.code := 'rest';
//    C.system := 'http://hl7.org/fhir/security-event-type';
//    C.Display := 'Restful Operation';
//    C := se.event.subtypeList.append;
//    C.code := 'read';
//    C.system := 'http://hl7.org/fhir/restful-operation';
//    C.Display := 'Read';
//    se.event.action := AuditEventActionE;
//    se.event.outcome := AuditEventOutcome8;
//    se.event.outcomeDesc := e.Message;
//    se.event.dateTime := TFslDateTime.makeUTC;
//    se.event.id := logId;
//    se.source := TFhirAuditEventSource.Create;
//    se.source.site := Server.name;
//    se.source.identifier := TFhirIdentifier.Create;
//    se.source.identifier.system := 'urn:ietf:rfc:3986';
//    se.source.identifier.value := Server.fhirEndpoint;
//    C := se.source.type_List.append;
//    C.code := '3';
//    C.Display := 'Web Server';
//    C.system := 'http://hl7.org/fhir/security-source-type';
//
//    if openIdToken = nil then
//    begin
//      // participant - the web browser / user proxy
//      p := se.participantList.append;
//      p.userId := TFhirIdentifier.Create;
//      p.userId.system := Server.fhirEndpoint;
//      p.userId.value := openIdToken.id;
//      p.name := openIdToken.name;
//    end;
//
//    o := se.object_List.Append;
//    o.reference := TFhirReference.create;
//    o.reference.reference := resourceType;
//    o.type_ := TFhirCoding.Create;
//    o.type_.system := 'http://hl7.org/fhir/security-source-type';
//    o.type_.code := '2';
//    o.lifecycle := TFhirCoding.Create;
//    o.lifecycle.system := 'http://hl7.org/fhir/object-lifecycle';
//    o.lifecycle.code := '6';
//    o.query := TEncoding.UTF8.GetBytes(encodeParams(params));
//    log(se);
//  finally
//    se.free;
//  end;
end;

procedure TAuditEventLoggingService.recordResourceSearchSuccess(logId : String; resourceType: String; params: TStringList; bnd: TFHIRResourceV);
//var
//  se: TFhirAuditEvent;
//  C: TFHIRCoding;
//  p: TFhirAuditEventParticipant;
//  o : TFhirAuditEventObject;
begin
//  se := TFhirAuditEvent.Create;
//  try
//    se.event := TFhirAuditEventEvent.Create;
//    se.event.type_ := TFHIRCoding.Create;
//    C := se.event.type_;
//    C.code := 'rest';
//    C.system := 'http://hl7.org/fhir/security-event-type';
//    C.Display := 'Restful Operation';
//    C := se.event.subtypeList.append;
//    C.code := 'read';
//    C.system := 'http://hl7.org/fhir/restful-operation';
//    C.Display := 'Read';
//    se.event.action := AuditEventActionE;
//    se.event.outcome := AuditEventOutcome0;
//    se.event.dateTime := TFslDateTime.makeUTC;
//    se.event.id := logId;
//    se.source := TFhirAuditEventSource.Create;
//    se.source.site := Server.name;
//    se.source.identifier := TFhirIdentifier.Create;
//    se.source.identifier.system := 'urn:ietf:rfc:3986';
//    se.source.identifier.value := Server.fhirEndpoint;
//    C := se.source.type_List.append;
//    C.code := '3';
//    C.Display := 'Web Server';
//    C.system := 'http://hl7.org/fhir/security-source-type';
//
//    if openIdToken = nil then
//    begin
//      // participant - the web browser / user proxy
//      p := se.participantList.append;
//      p.userId := TFhirIdentifier.Create;
//      p.userId.system := Server.fhirEndpoint;
//      p.userId.value := openIdToken.id;
//      p.name := openIdToken.name;
//    end;
//
//    o := se.object_List.Append;
//    o.reference := TFhirReference.create;
//    o.reference.reference := resourceType;
//    o.type_ := TFhirCoding.Create;
//    o.type_.system := 'http://hl7.org/fhir/security-source-type';
//    o.type_.code := '2';
//    o.lifecycle := TFhirCoding.Create;
//    o.lifecycle.system := 'http://hl7.org/fhir/object-lifecycle';
//    o.lifecycle.code := '6';
//    o.query := TEncoding.UTF8.GetBytes(encodeParams(params));
//    o.detailList.Append.value := resourceToBytes(bnd, ffJson);
//    log(se);
//  finally
//    se.free;
//  end;
end;

procedure TAuditEventLoggingService.recordLogin;
//var
//  se: TFhirAuditEvent;
//  C: TFHIRCoding;
//  p: TFhirAuditEventParticipant;
begin
//  se := TFhirAuditEvent.Create;
//  try
//    se.event := TFhirAuditEventEvent.Create;
//    se.event.type_ := TFHIRCoding.Create;
//    C := se.event.type_;
//    C.code := '110114';
//    C.system := 'http://nema.org/dicom/dcid';
//    C.Display := 'User Authentication';
//    C := se.event.subtypeList.append;
//    C.code := '110122';
//    C.system := 'http://nema.org/dicom/dcid';
//    C.Display := 'Login';
//    se.event.action := AuditEventActionE;
//    se.event.outcome := AuditEventOutcome0;
//    se.event.dateTime := TFslDateTime.makeUTC;
//    se.source := TFhirAuditEventSource.Create;
//    se.source.site := Server.name;
//    se.source.identifier := TFhirIdentifier.Create;
//    se.source.identifier.system := 'urn:ietf:rfc:3986';
//    se.source.identifier.value := Server.fhirEndpoint;
//    C := se.source.type_List.append;
//    C.code := '3';
//    C.Display := 'Web Server';
//    C.system := 'http://hl7.org/fhir/security-source-type';
//
//    if openIdToken = nil then
//    begin
//      // participant - the web browser / user proxy
//      p := se.participantList.append;
//      p.userId := TFhirIdentifier.Create;
//      p.userId.system := Server.fhirEndpoint;
//      p.userId.value := openIdToken.id;
//      p.name := openIdToken.name;
//    end;
//    log(se);
//  finally
//    se.free;
//  end;
end;

procedure TAuditEventLoggingService.recordLogout;
//var
//  se: TFhirAuditEvent;
//  C: TFHIRCoding;
//  p: TFhirAuditEventParticipant;
begin
//  se := TFhirAuditEvent.Create;
//  try
//    se.event := TFhirAuditEventEvent.Create;
//    se.event.type_ := TFHIRCoding.Create;
//    C := se.event.type_;
//    C.code := '110114';
//    C.system := 'http://nema.org/dicom/dcid';
//    C.Display := 'User Authentication';
//    C := se.event.subtypeList.append;
//    C.code := '110123';
//    C.system := 'http://nema.org/dicom/dcid';
//    C.Display := 'Logout';
//    se.event.action := AuditEventActionE;
//    se.event.outcome := AuditEventOutcome0;
//    se.event.dateTime := TFslDateTime.makeUTC;
//    se.source := TFhirAuditEventSource.Create;
//    se.source.site := Server.name;
//    se.source.identifier := TFhirIdentifier.Create;
//    se.source.identifier.system := 'urn:ietf:rfc:3986';
//    se.source.identifier.value := Server.fhirEndpoint;
//    C := se.source.type_List.append;
//    C.code := '3';
//    C.Display := 'Web Server';
//    C.system := 'http://hl7.org/fhir/security-source-type';
//
//    if openIdToken = nil then
//    begin
//      // participant - the web browser / user proxy
//      p := se.participantList.append;
//      p.userId := TFhirIdentifier.Create;
//      p.userId.system := Server.fhirEndpoint;
//      p.userId.value := openIdToken.id;
//      p.name := openIdToken.name;
//    end;
//    log(se);
//  finally
//    se.free;
//  end;
end;

{ TDemoHttpLogger }

constructor TDemoHttpLogger.Create;
begin
  inherited;
  if DirectoryExists('c:\temp') then
    FLog := TLogger.Create('c:\temp\vcldemo.fhir.log')
  else
    FLog := TLogger.Create(IncludeTrailingPathDelimiter(SystemTemp)+ 'vcldemo.fhir.log');
  FLog.clear;
end;

destructor TDemoHttpLogger.Destroy;
begin
  FLog.Free;
  inherited;
end;

procedure TDemoHttpLogger.logExchange(verb, url, status, requestHeaders, responseHeaders: String; request, response: TBytes);
begin
  FLog.WriteToLog('=================================='#13#10);
  FLog.WriteToLog(verb+' '+url+' HTTP/1.0'#13#10);
  FLog.WriteToLog(requestHeaders+#13#10);
  if request <> nil then
  begin
    Flog.WriteToLog(request);
    Flog.WriteToLog(#13#10);
  end;
  FLog.WriteToLog('----------------------------------'#13#10);
  FLog.WriteToLog(status+' HTTP/1.0'#13#10);
  FLog.WriteToLog(responseHeaders+#13#10);
  if response <> nil then
  begin
    Flog.WriteToLog(response);
    Flog.WriteToLog(#13#10);
  end;
end;

end.

