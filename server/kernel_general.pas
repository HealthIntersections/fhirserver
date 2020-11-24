unit kernel_general;

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

{$I fhir.inc}

interface

Uses
  {$IFDEF WINDOWS} Windows, ActiveX, {$ENDIF}
  SysUtils, StrUtils, Classes, IniFiles,

  fsl_base, fsl_utilities, fsl_logging,
  ftx_ucum_services, fsl_http,
  fhir_objects,  fhir_factory, fhir_pathengine, fhir_parser, fhir_common,
  {$IFNDEF NO_JS}fhir_javascript, {$ENDIF}
  fsl_npm_cache,

  fhir2_factory, fhir3_factory, fhir4_factory, fhir5_factory,
  fhir2_context, fhir3_context, fhir4_context, fhir5_context,
  fhir2_indexinfo, fhir3_indexinfo, fhir4_indexinfo, fhir5_indexinfo,
  indexing_r2, indexing_r3, indexing_r4, indexing_r5,
  subscriptions_r2, subscriptions_r3, subscriptions_r4, subscriptions_r5,
  operations_r2, operations_r3, operations_r4, operations_r5,
  fhir2_validator, fhir3_validator, fhir4_validator, fhir5_validator,
  validator_r2, validator_r3, validator_r4, validator_r5,
  {$IFNDEF NO_JS} fhir2_javascript, fhir3_javascript, fhir4_javascript, fhir5_javascript, {$ENDIF}
  fhir2_pathengine, fhir3_pathengine, fhir4_pathengine, fhir5_pathengine,

  fdb_manager,
  fhir_indexing,
  tx_manager, tx_server,
  tx_unii,
  scim_server,
  database_installer, server_version, server_ini, utilities,
  server_context,
  {$IFNDEF NO_JS}server_javascript, {$ENDIF}
  storage, database,
  server_factory, indexing, subscriptions, webserver,
  kernel_base,
  general_endpoint;

implementation


end.
