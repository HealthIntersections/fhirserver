unit fxver_convertors;

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

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_http,
  fhir_objects, fhir_parser,
  fhir3_resources, fhir3_parser, fhir4_resources, fhir4_parser,
  fxver_convertor_30_40;

type
  TFhirVersionConvertors = class
  private
    class function parse(s : TStream; fmt : TFHIRFormat; version : TFHIRVersion; const lang : THTTPLanguages) : TFHIRResourceV;
    class procedure compose(s : TStream; fmt : TFHIRFormat; style : TFHIROutputStyle; version : TFHIRVersion; const lang : THTTPLanguages; resource : TFHIRResourceV);
  public
    class function convertResource(resource : TFHIRResourceV; const lang : THTTPLanguages; vSource, vDest : TFHIRVersion) : TFHIRResourceV; overload;
    class function convertResource(resource : TBytes; fmt : TFHIRFormat; style : TFHIROutputStyle; const lang : THTTPLanguages; vSource, vDest : TFHIRVersion) : TBytes; overload;
    class procedure convertResource(resource : TStream; fmt : TFHIRFormat; style : TFHIROutputStyle; const lang : THTTPLanguages; vSource, vDest : TFHIRVersion; dest : TStream); overload;
  end;

implementation

{ TFhirVersionConvertors }

class function TFhirVersionConvertors.parse(s: TStream; fmt : TFHIRFormat; version: TFHIRVersion; const lang : THTTPLanguages): TFHIRResourceV;
var
  p : TFHIRParser;
begin
  case version of
 //   fhirVersionRelease2: c := TFHIR;
    fhirVersionRelease3: p := TFHIRParsers3.parser(nil, fmt, lang);
    fhirVersionRelease4: p := TFHIRParsers4.parser(nil, fmt, lang);
  else
    raise EFHIRException.CreateLang('Unsupported Version %s', lang, [CODES_TFHIRVersion[version]] );
  end;
  try
    result := p.parseResource(s);
  finally
    p.Free;
  end;
end;

class procedure TFhirVersionConvertors.compose(s: TStream; fmt : TFHIRFormat; style : TFHIROutputStyle; version: TFHIRVersion; const lang : THTTPLanguages; resource: TFHIRResourceV);
var
  c : TFHIRComposer;
begin
  case version of
 //   fhirVersionRelease2: c := TFHIR;
    fhirVersionRelease3: c := TFHIRParsers3.composer(nil, fmt, lang, style);
    fhirVersionRelease4: c := TFHIRParsers4.composer(nil, fmt, lang, style);
  else
    raise EFHIRException.CreateLang('Unsupported Version %s', lang, [CODES_TFHIRVersion[version]] );
  end;
  try
    c.Compose(s, resource);
  finally
    c.Free;
  end;
end;

class function TFhirVersionConvertors.convertResource(resource: TBytes; fmt: TFHIRFormat; style : TFHIROutputStyle; const lang : THTTPLanguages; vSource, vDest: TFHIRVersion): TBytes;
var
  s1, s2 : TBytesStream;
  r1, r2 : TFHIRResourceV;
begin
  s1 := TBytesStream.Create(resource);
  try
    r1 := parse(s1, fmt, vSource, lang);
    try
      r2 := convertResource(r1, lang, vSource, vDest);
      try
        s2 := TBytesStream.Create;
        try
          compose(s2, fmt, style, vDest, lang, r2);
          result := copy(s2.Bytes, 0, s2.position);
        finally
          s2.Free;
        end;
      finally
        r2.Free;
      end;
    finally
      r1.Free;
    end;
  finally
    s1.Free;
  end;
end;

class procedure TFhirVersionConvertors.convertResource(resource: TStream; fmt: TFHIRFormat; style : TFHIROutputStyle; const lang : THTTPLanguages; vSource, vDest: TFHIRVersion; dest: TStream);
var
  r1, r2 : TFHIRResourceV;
begin
  r1 := parse(resource, fmt, vSource, lang);
  try
    r2 := convertResource(r1, lang, vSource, vDest);
    try
      compose(dest, fmt, style, vDest, lang, r2);
    finally
      r2.Free;
    end;
  finally
    r1.Free;
  end;
end;

class function TFhirVersionConvertors.convertResource(resource: TFHIRResourceV; const lang : THTTPLanguages; vSource, vDest: TFHIRVersion): TFHIRResourceV;
begin
  case vSource of
    fhirVersionRelease3:
      begin
        if not (resource is fhir3_resources.TFhirResource) then
          raise EFHIRException.CreateLang('Unsupported Version conversion source resource is not actually %s', lang, [CODES_TFHIRVersion[vSource]] );
        case vDest of
          fhirVersionRelease4: exit(TVersionConvertor_30_40.convertResource3(resource as fhir3_resources.TFhirResource));
        end;
      end;
    fhirVersionRelease4:
      begin
        if not (resource is fhir4_resources.TFhirResource) then
          raise EFHIRException.CreateLang('Unsupported Version conversion source resource is not actually %s', lang, [CODES_TFHIRVersion[vSource]] );
        case vDest of
          fhirVersionRelease3: exit(TVersionConvertor_30_40.convertResource4(resource as fhir4_resources.TFhirResource));
        end;
      end;
  end;
  raise EFHIRException.CreateLang('Unsupported Version conversion from %s to %s', lang, [CODES_TFHIRVersion[vSource], CODES_TFHIRVersion[vDest]] );
end;

end.
