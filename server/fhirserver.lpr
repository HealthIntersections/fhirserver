program fhirserver;

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

uses             
  {$IFDEF WINDOWS}
  FastMM4,
  {$ELSE}
  cmem,
  cthreads,
  {$ENDIF}

  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF OSX}
  forms, Interfaces,
  {$ENDIF}
  Classes, SysUtils,
  fsl_fpc_memory, fsl_utilities,

  kernel, server_testing, server_stats, zero_config, telnet_server,
  tx_registry_spider, tx_omop, tx_registry_model,
  endpoint_txregistry, endpoint_icao, tests_cpt, tx_cpt,
  web_server, web_cache;

{$R *.res}

var
  cp : TCommandLineParameters;
begin
  isMultiThread := true;
  {$IFDEF FPC}
  TFPCMemoryManagerTracker.install;
  {$ENDIF}
  cp := TCommandLineParameters.create;
  try
    ExecuteFhirServer(cp);
  finally
    cp.free;
  end;
end.

