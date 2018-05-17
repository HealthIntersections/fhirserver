unit FHIR.Base.Utilities;

{
Copyright (c) 2018+, Health Intersections Pty Ltd
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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
  SysUtils,
  FHIR.Support.Strings,
  FHIR.Web.Parsers,
  FHIR.Base.Objects;

function mimeTypeToFormat(mt : String; def : TFHIRFormat = ffUnspecified) : TFHIRFormat;

implementation

function mimeTypeToFormat(mt : String; def : TFHIRFormat = ffUnspecified) : TFHIRFormat;
var
  ct : TMimeContentType;
begin
  result := def;
  ct := TMimeContentType.parseSingle(mt);
  try
    if      (ct.base = 'application/json') or (ct.base = 'application/fhir+json') or (ct.base = 'application/json+fhir') then result := ffJson
    else if (ct.base = 'application/xml') or (ct.base = 'application/fhir+xml') or (ct.base = 'application/xml+fhir') then result := ffXml
    else if (ct.base = 'application/x-ndjson') or (ct.base = 'application/fhir+ndjson') then result := ffNDJson
    else if (ct.base = 'text/turtle') or (ct.base = 'application/fhir+turtle') then result := ffTurtle

    else if (ct.base = 'text/json') then result := ffJson
    else if (ct.base = 'text/html') then result := ffXhtml
    else if (ct.base = 'text/xml') then result := ffXml
    else if (ct.base = 'application/x-zip-compressed') or (ct.base = 'application/zip') then result := ffXhtml
    else if (ct.base = 'text/plain') then result := ffText

    else if StringExistsInsensitive(ct.base, 'json') then result := ffJson
    else if StringExistsInsensitive(ct.base, 'xml') then result := ffXml
    else if StringExistsInsensitive(ct.base, 'html') then result := ffXhtml
    else if StringExistsInsensitive(ct.base, 'text') then result := ffText
    else if StringExistsInsensitive(ct.base, 'rdf') then result := ffTurtle
    else if StringExistsInsensitive(ct.base, 'turtle') then result := ffTurtle
    else if StringExistsSensitive(ct.base, '*/*') Then result := ffXhtml;
  finally
    ct.Free;
  end;
end;

end.
