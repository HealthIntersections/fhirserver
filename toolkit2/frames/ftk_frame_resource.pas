unit ftk_frame_resource;

interface

uses
  SysUtils, Classes,
  Forms,
  fhir_objects,
  fui_lcl_managers,
  ftk_context;

type

   { TResourceEditorFrame }

   TResourceEditorFrame = class (TFrame)
   private
     FContext: TToolkitContext;
     FResource: TFHIRResourceV;
     FSync: TFHIRSynEditSynchroniser;
     procedure SetContext(AValue: TToolkitContext);
     procedure SetResource(AValue: TFHIRResourceV);
     procedure SetSync(AValue: TFHIRSynEditSynchroniser);
   public
     destructor Destroy; override;

     property context : TToolkitContext read FContext write SetContext;
     property sync : TFHIRSynEditSynchroniser read FSync write SetSync;
     property resource : TFHIRResourceV read FResource write SetResource;

     procedure initialize; virtual; // called once, at start up, to bind all resources etc
     procedure bind; virtual; // called any time that the resource changes
   end;

implementation

{ TResourceEditorFrame }

destructor TResourceEditorFrame.Destroy;
begin
  FContext.Free;
  FResource.Free;
  FSync.Free;
  inherited Destroy;
end;

procedure TResourceEditorFrame.SetResource(AValue: TFHIRResourceV);
begin
  FResource.Free;
  FResource := AValue;
  bind;
end;

procedure TResourceEditorFrame.SetContext(AValue: TToolkitContext);
begin
  FContext.Free;
  FContext := AValue;
end;

procedure TResourceEditorFrame.SetSync(AValue: TFHIRSynEditSynchroniser);
begin
  FSync.Free;
  FSync := AValue;
end;

procedure TResourceEditorFrame.initialize;
begin
  // nothing here
end;

procedure TResourceEditorFrame.bind;
begin
  // nothing here
end;

end.
