unit web_event;

{$i fhir.inc}

interface

uses
  IdCustomHTTPServer,
  fsl_base,
  fhir_objects,
  session;

type
  TWebReturnProcessedFileEvent = procedure (sender : TObject; request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; secure : boolean; variables: TFslMap<TFHIRObject>) of Object;
  TWebProcessFileEvent = procedure (sender : TObject; session : TFhirSession; named, path: String; secure : boolean; variables: TFslMap<TFHIRObject>; var result : String) of Object;

implementation

end.