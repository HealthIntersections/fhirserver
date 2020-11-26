unit endpoint_twilio;

{$i fhir.inc}

interface

{
    FTwilioDB : String;
    FTwilioResponse : String;

    FTwilioServer : TTwilioServer;

    function HandleTwilio(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean) : string;
  FTwilioServer.Free;
  FTwilioDB := ini.admin['twilio'].value;
  FTwilioResponse := ini.admin['twilio-text'].value;
  {$IFDEF WINDOWS}
//  if FTwilioDB <> '' then
//    FTwilioServer := TTwilioServer.Create(TFDBOdbcManager.create('twilio', kdbSqlServer, 20, 5000, 'SQL Server Native Client 11.0', '(local)', FTwilioDB, '', ''), FTwilioResponse);
//  {$ENDIF}
//          else if request.Document = '/twilio' then
//            summ := HandleTwilio(AContext, request, response, false, false)
//
//function TFhirWebServer.HandleTwilio(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; ssl, secure: boolean) : String;
//begin
//  result := FTwilioServer.process(AContext, request, response);
//end;
//
//}
implementation

end.
