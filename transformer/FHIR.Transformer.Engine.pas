unit FHIR.Transformer.Engine;

interface

uses
  SysUtils, Classes,
  FHIR.Support.Base,
  FHIR.Transformer.Workspace;

type
  TConversionEngine = class;

  TConversionEngineFetchSourceEvent = function (sender : TConversionEngine; filename : String) : TStream of object;
  TConversionEngineStatusEvent = procedure (sender : TConversionEngine; message : String) of object;
  TConversionEngineLogEvent = procedure (sender : TConversionEngine; message : String) of object;

  TConversionEngine = class abstract (TFslObject)
  private
    FSource: TWorkspaceFile;
    FOnWantSource: TConversionEngineFetchSourceEvent;
    FOnStatus: TConversionEngineStatusEvent;
    FOnLog: TConversionEngineLogEvent;
    procedure SetSource(const Value: TWorkspaceFile);
  public
    destructor Destroy; override;
    function link : TConversionEngine; overload;

    property source : TWorkspaceFile read FSource write SetSource;
    property OnWantSource : TConversionEngineFetchSourceEvent read FOnWantSource write FOnWantSource;
    property OnStatus : TConversionEngineStatusEvent read FOnStatus write FOnStatus;
    property OnLog : TConversionEngineLogEvent read FOnLog write FOnLog;

    procedure execute; virtual; abstract;
  end;

  TCDAConversionEngine = class (TConversionEngine)
  private
  public
    function link : TCDAConversionEngine; overload;
  end;

implementation

{ TConversionEngine }

destructor TConversionEngine.Destroy;
begin
  FSource.Free;
  inherited;
end;

function TConversionEngine.link: TConversionEngine;
begin
  result := TConversionEngine(inherited Link);
end;

procedure TConversionEngine.SetSource(const Value: TWorkspaceFile);
begin
  FSource.Free;
  FSource := Value;
end;

{ TCDAConversionEngine }

function TCDAConversionEngine.link: TCDAConversionEngine;
begin
  result := TCDAConversionEngine(inherited Link);
end;

end.
