unit ToolKitUtilities;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}
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
