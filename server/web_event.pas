unit web_event;

{$i fhir.inc}

interface

uses
  IdCustomHTTPServer,
  fsl_base,
  fhir_objects,
  session;

type
  TReturnProcessFileEvent = procedure (request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; secure : boolean; variables: TFslMap<TFHIRObject>) of Object;

implementation

end.