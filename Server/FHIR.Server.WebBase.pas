unit FHIR.Server.WebBase;

interface

uses
  SysUtils, Classes,
  IdContext, IdCustomHTTPServer,
  FHIR.Support.Base,
  FHIR.Base.Objects,
  FHIR.Server.Session;

type
  TReturnProcessFileEvent = procedure (request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; secure : boolean; variables: TFslMap<TFHIRObject>) of Object;

implementation

end.
