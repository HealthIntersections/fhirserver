unit ToolKitUtilities;

interface

uses
  SysUtils, Classes,
  IdHTTP, IdSSLOpenSSL, IdComponent,
  OSXUIUtils;

function checkUpgrade : String;
procedure doUpgrade(newVersion : String);


implementation

function checkUpgrade : String;
var
  http : TIdHTTP;
  inc : String;
begin
  http := TIdHTTP.Create(nil);
  try
    inc := http.Get('http://www.healthintersections.com.au/FhirServer/toolkit.inc');
    inc := inc.substring(inc.indexof('<td>v')+5);
    inc := inc.substring(0, inc.indexof('</td>'));
    result := inc;
  finally
    http.free;
  end;
end;

procedure doUpgrade(newVersion : String);
begin
  {$IFDEF MACOS}
  OpenURL('http://www.healthintersections.com.au/FhirServer/fhir-toolkit-osx-'+newVersion+'.zip');
  {$ELSE}
  OpenURL('http://www.healthintersections.com.au/FhirServer/fhir-toolkit-install-'+newVersion+'.exe');
  {$ENDIF}
end;

end.
