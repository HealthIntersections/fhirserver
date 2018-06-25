unit FHIR.Server.BundleBuilder;

interface

uses
  SysUtils,
  FHIR.Support.Base, FHIR.Support.Utilities,
  FHIR.Base.Objects, FHIR.Base.Common, FHIR.Base.Factory;

type
  TFHIRBundleBuilder = class (TFslObject)
  private
    FHasSecureOp: boolean;
  protected
    FBundle : TFHIRBundleW;
    FFactory : TFHIRFactory;
  public
    constructor Create(factory : TFHIRFactory; bundle : TFHIRBundleW);
    destructor Destroy; override;

    Property hasSecureOp : boolean read FHasSecureOp write FHasSecureOp;

    procedure setId(id : string);
    procedure setLastUpdated(dt : TDateTimeEx);
    procedure setTotal(t : integer);
    procedure tag(n, v : String);
    procedure addLink(rt, url : String);
    procedure addEntry(entry : TFhirBundleEntryW; first : boolean); virtual; abstract;
    function moveToFirst(res : TFhirResourceV) : TFhirBundleEntryW; virtual; abstract;
    function getBundle : TFHIRResourceV; virtual; abstract;
  end;

  TFHIRBundleBuilderSimple = class (TFHIRBundleBuilder)
  public
    procedure addEntry(entry : TFhirBundleEntryW; first : boolean); override;
    function moveToFirst(res : TFhirResourceV) : TFhirBundleEntryW; override;
    function getBundle : TFHIRResourceV; override;
  end;


implementation


{ TFHIRBundleBuilder }

constructor TFHIRBundleBuilder.Create(factory : TFHIRFactory; bundle : TFHIRBundleW);
begin
  inherited Create;
  FBundle := bundle;
  FFactory := factory;
end;

destructor TFHIRBundleBuilder.Destroy;
begin
  FBundle.Free;
  FFactory.Free;
  inherited;
end;

procedure TFHIRBundleBuilder.addLink(rt, url: String);
begin
  FBundle.links[rt] := url;
end;

procedure TFHIRBundleBuilder.setId(id: string);
begin
  FBundle.id := id;
end;

procedure TFHIRBundleBuilder.setLastUpdated(dt: TDateTimeEx);
begin
  FBundle.lastUpdated := dt;
end;

procedure TFHIRBundleBuilder.setTotal(t: integer);
begin
  FBundle.total := t;
end;

procedure TFHIRBundleBuilder.tag(n, v: String);
begin
  FBundle.Tags[n] := v;
end;

{ TFHIRBundleBuilderSimple }

procedure TFHIRBundleBuilderSimple.addEntry(entry: TFhirBundleEntryW; first : boolean);
begin
  FBundle.addEntry(entry, first);
end;


function TFHIRBundleBuilderSimple.getBundle: TFHIRResourceV;
begin
  result := FBundle.Resource.Link;
end;

function TFHIRBundleBuilderSimple.moveToFirst(res: TFhirResourceV): TFhirBundleEntryW;
begin
  result := FBundle.MoveToFirst(res);
end;


end.
