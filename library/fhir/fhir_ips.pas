unit fhir_ips;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_json, fsl_stream;

type

  { TIPSWrapper }

  TIPSWrapper = class (TFslObject)
  private
    FManifest : TJsonObject;
    FContent : TFslMap<TFslBuffer>;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function fromStream(stream : TStream) : TIPSWrapper; overload;
    class function fromStream(stream : TFslStream) : TIPSWrapper; overload;
    procedure saveToStream(stream : TStream); overload;
    procedure saveToStream(stream : TFslStream); overload;
  end;

implementation

{ TIPSWrapper }

constructor TIPSWrapper.Create;
begin
  inherited Create;
end;

destructor TIPSWrapper.Destroy;
begin
  inherited Destroy;
end;

class function TIPSWrapper.fromStream(stream: TStream): TIPSWrapper;
begin

end;

class function TIPSWrapper.fromStream(stream: TFslStream): TIPSWrapper;
begin

end;

procedure TIPSWrapper.saveToStream(stream: TStream);
begin

end;

procedure TIPSWrapper.saveToStream(stream: TFslStream);
begin

end;

end.

