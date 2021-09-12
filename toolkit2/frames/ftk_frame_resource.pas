unit ftk_frame_resource;

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  Forms,
  fhir_objects, fhir_client,
  fui_lcl_managers,
  ftk_context;

type
   TSelectSourceRangeEvent = procedure (sender : TObject; start, stop : TPoint) of object;

   { TResourceDesignerFrame }

   TResourceDesignerFrame = class (TFrame)
   private
     FClient: TFHIRClientV;
     FContext: TToolkitContext;
     FResource: TFHIRResourceV;
     FSync: TFHIRSynEditSynchroniser;
     FOnSelectSourceRange: TSelectSourceRangeEvent;
     procedure SetClient(AValue: TFHIRClientV);
     procedure SetContext(AValue: TToolkitContext);
     procedure SetResource(AValue: TFHIRResourceV);
     procedure SetSync(AValue: TFHIRSynEditSynchroniser);
   public
     destructor Destroy; override;

     property context : TToolkitContext read FContext write SetContext;
     property sync : TFHIRSynEditSynchroniser read FSync write SetSync;
     property resource : TFHIRResourceV read FResource write SetResource;
     property client : TFHIRClientV read FClient write SetClient; // access to client, if this came from a client (for derived information)

     procedure initialize; virtual; // called once, at start up, to bind all resources etc
     procedure bind; virtual; // called any time that the resource changes
     procedure saveStatus; virtual; // called before shut down because shut down order isn't always predictable

     property OnSelectSourceRange : TSelectSourceRangeEvent read FOnSelectSourceRange write FOnSelectSourceRange;
   end;

implementation

{ TResourceDesignerFrame }

destructor TResourceDesignerFrame.Destroy;
begin
  FResource.Free;
  FSync.Free;
  FClient.Free;
  inherited Destroy;
end;

procedure TResourceDesignerFrame.SetResource(AValue: TFHIRResourceV);
begin
  FResource.Free;
  FResource := AValue;
  bind;
end;

procedure TResourceDesignerFrame.SetContext(AValue: TToolkitContext);
begin
  FContext.Free;
  FContext := AValue;
end;

procedure TResourceDesignerFrame.SetClient(AValue: TFHIRClientV);
begin
  FClient.Free;
  FClient := AValue;
end;

procedure TResourceDesignerFrame.SetSync(AValue: TFHIRSynEditSynchroniser);
begin
  FSync.Free;
  FSync := AValue;
end;

procedure TResourceDesignerFrame.initialize;
begin
  // nothing here
end;

procedure TResourceDesignerFrame.bind;
begin
  // nothing here
end;

procedure TResourceDesignerFrame.saveStatus;
begin
  FContext.Free;
  FContext := nil;
end;


end.
