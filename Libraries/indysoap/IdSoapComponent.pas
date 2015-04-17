{
IndySOAP: This unit defines a base component which plugs in to the leak tracking architecture
}


unit IdSoapComponent;


{$I IdSoapDefines.inc}

interface

uses
  Classes,
  IdComponent,
  IdSoapDebug;

type
{==============================================================================

 Soap version support

 currently IndySoap only implements Version 1.1                                }

  TIdSoapVersion = (IdSoapV1_1);

{
  IdSoapV1_1: Soap Version 1.1 specification, as widely implemented.

    References
       SOAP  http://www.w3.org/TR/2000/NOTE-SOAP-20000508
       WSDL  http://www.w3.org/TR/2001/NOTE-wsdl-20010315                      }

  TIdSoapVersionSet = set of TIdSoapVersion;

{==============================================================================}

  TIdSoapComponent = class(TIdComponent)
  Private
    FSerialNo : Cardinal;
  Public
  {$IFNDEF INDY_V10}
    constructor Create(AOwner: TComponent); Override;
    {$ENDIF}
    destructor Destroy; Override;
  {$IFDEF INDY_V10}
    procedure InitComponent; override;
  {$ENDIF}
    function TestValid(AClassType: TClass = NIL): Boolean;
  end;

implementation

{ TIdSoapComponent }

{$IFNDEF INDY_V10}
constructor TIdSoapComponent.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF OBJECT_TRACKING}
  FSerialNo := IdObjectRegister(self);
  {$ENDIF}
end;
{$ENDIF}

destructor TIdSoapComponent.Destroy;
begin
  {$IFDEF OBJECT_TRACKING}
  IdObjectDeregister(self, FSerialNo);
  {$ENDIF}
  inherited;
end;

{$IFDEF INDY_V10}
procedure TIdSoapComponent.InitComponent;
begin
  inherited;
  {$IFDEF OBJECT_TRACKING}
  IdObjectRegister(self);
  {$ENDIF}
end;
{$ENDIF}

function TIdSoapComponent.TestValid(AClassType: TClass): Boolean;
begin
  {$IFDEF OBJECT_TRACKING}
  Result := IdObjectTestValid(self);
  {$ELSE}
  Result := Assigned(self);
  {$ENDIF}
  if Result and assigned(AClassType) then
    begin
    Result := self is AClassType;
    end;
end;

end.
