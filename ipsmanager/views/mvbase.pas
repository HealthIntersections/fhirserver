unit mvbase;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  ExtCtrls,
  laz.VirtualTrees,
  fsl_base,
  fhir4_client;

type

  { TViewManager }

  TViewManager = class (TFslObject)
  private
    FClient: TFhirClient4;
    FNavigator: TLazVirtualStringTree;
    FNavPanel: TPanel;
    FPresentation: TPanel;
    procedure SetClient(AValue: TFhirClient4);
    procedure SetNavigator(AValue: TLazVirtualStringTree);
  public
    property client : TFhirClient4 read FClient write SetClient;
    property NavPanel : TPanel read FNavPanel write FNavPanel;
    property navigator :  TLazVirtualStringTree read FNavigator write SetNavigator;
    property presentation : TPanel read FPresentation write FPresentation;

    procedure initialize; virtual;
  end;

implementation

{ TViewManager }

procedure TViewManager.SetClient(AValue: TFhirClient4);
begin
  FClient.free;
  FClient:=AValue;
end;

procedure TViewManager.SetNavigator(AValue: TLazVirtualStringTree);
begin
  FNavigator:=AValue;
end;

procedure TViewManager.initialize;
begin

end;

end.
