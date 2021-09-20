unit QlpGuard;

{$I QRCodeGenLib.inc}

interface

uses
  QlpQRCodeGenLibTypes;

type
  TGuard = class sealed(TObject)

  public
    class procedure RequireNotNull(const AObject: TObject;
      const AMessage: String = ''); overload; static;
    class procedure RequireNotNull(const AInterface: IInterface;
      const AMessage: String = ''); overload; static;
  end;

implementation

{ TGuard }

class procedure TGuard.RequireNotNull(const AObject: TObject;
  const AMessage: String);
begin
  if AObject = Nil then
  begin
    raise ENullReferenceQRCodeGenLibException.Create(AMessage);
  end;
end;

class procedure TGuard.RequireNotNull(const AInterface: IInterface;
  const AMessage: String);
begin
  if AInterface = Nil then
  begin
    raise ENullReferenceQRCodeGenLibException.Create(AMessage);
  end;
end;

end.
