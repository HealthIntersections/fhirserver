unit CertificateSupport;

interface

uses
  SysUtils,
  IdSSLOpenSSL, IdSSLOpenSSLHeaders;

type
  TIdX509Helper = class helper for TIdX509
  private
    function GetCanonicalName: String;
  public
    property CanonicalName : String read GetCanonicalName;
  end;


implementation


function TIdX509Helper.GetCanonicalName() : String;
var
  s : String;
  p : TArray<String>;
begin
  s := String(self.Certificate.name);
  p := s.Split(['/']);
  result := '(no canonical name)';
  for s in p do
    if (s.StartsWith('CN=')) then
      exit(s.Substring(3));
end;

end.
