unit ftk_fhir_context;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_lang, fsl_npm_cache,
  fhir_objects, fhir_factory,
  ftk_fhir_context_2, ftk_fhir_context_3, ftk_fhir_context_4, ftk_fhir_context_5;

type
  TToolkitValidatorContext = class
  public
    class function create(languages : TIETFLanguageDefinitions; factory : TFHIRFactory; TerminologyServer : String; pcm : TFHIRPackageManager) : TFHIRWorkerContextWithFactory;
  end;

implementation

{ TToolkitValidatorContext }

class function TToolkitValidatorContext.create(languages : TIETFLanguageDefinitions; factory: TFHIRFactory; TerminologyServer: String; pcm : TFHIRPackageManager): TFHIRWorkerContextWithFactory;
begin
  case factory.version of
    fhirVersionRelease2 : result := TToolkitValidatorContextR2.Create(factory, languages, TerminologyServer, pcm.link);
    fhirVersionRelease3 : result := TToolkitValidatorContextR3.Create(factory, languages, TerminologyServer, pcm.link);
    fhirVersionRelease4 : result := TToolkitValidatorContextR4.Create(factory, languages, TerminologyServer, pcm.link);
    fhirVersionRelease5 : result := TToolkitValidatorContextR5.Create(factory, languages, TerminologyServer, pcm.link);
  else
    raise EFHIRException.create('Unexpected version');
  end;
end;


end.
