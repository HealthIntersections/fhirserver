unit FHIR.Client.Debugger;

interface

uses
  SysUtils, Classes,
  FHIR.Support.Base,
  FHIR.Base.Objects,
  FHIR.Client.Base, FHIR.Client.Threaded;

type
  TFhirDebuggerCommunicator = class (TFhirFacadeCommunicator)
  private
    FOwner : TComponent;
  protected
    procedure process(Package : TFhirThreadedClientPackage); override;
  public
    constructor Create(internal: TFhirClientV; owner : TComponent);
  end;

implementation

{$IFDEF FMX}
uses
  FHIR.Client.InteractiveFMX;
{$ENDIF}

{ TFhirDebuggerCommunicator }

constructor TFhirDebuggerCommunicator.Create(internal: TFhirClientV; owner: TComponent);
begin
  inherited Create(internal);
  FOwner := owner;
end;

procedure TFhirDebuggerCommunicator.process(Package: TFhirThreadedClientPackage);
begin
  {$IFDEF FMX}
  InteractiveClientForm := TInteractiveClientForm.Create(FOwner);
  try
    InteractiveClientForm.package := Package.Link;
    InteractiveClientForm.client := FClient.Link;
    case InteractiveClientForm.showModal of
    !
    end;
  finally
    InteractiveClientForm.Free;
  end;
  {$ELSE}
  raise Exception.Create('VCL not done yet');!
  {$ENDIF}
end;

end.
