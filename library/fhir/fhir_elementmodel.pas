unit fhir_elementmodel;

interface

uses
  SysUtils, Classes,
  fsl_base,
  fhir_objects;

Type
  TFHIRBaseMMManager = class (TFslObject)
  public
    function parseV(context : TFHIRWorkerContextV; source : TStream; inputFormat : TFhirFormat) : TFHIRObject; virtual; abstract;
    procedure composeV(context : TFHIRWorkerContextV; e : TFHIRObject; destination : TStream; outputFormat : TFhirFormat; style : TFHIROutputStyle; base : String = ''); overload; virtual; abstract;
    function composeV(context : TFHIRWorkerContextV; e : TFHIRObject; outputFormat : TFhirFormat; style : TFHIROutputStyle; base : String = '') : String; overload;
  end;

implementation

{ TFHIRBaseMMManager }

function TFHIRBaseMMManager.composeV(context: TFHIRWorkerContextV; e: TFHIRObject; outputFormat: TFhirFormat; style: TFHIROutputStyle; base: String): String;
var
  ss : TStringStream;
begin
  ss := TStringStream.Create;
  try
    composeV(context, e, ss, outputFormat, style, base);
    result := ss.DataString;
  finally
    ss.Free;
  end;
end;

end.
