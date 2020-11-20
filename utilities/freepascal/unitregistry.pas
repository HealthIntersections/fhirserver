unit UnitRegistry;

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

uses
  Classes, SysUtils,
  // indy units
  IdHeaderList, IdGlobal, IdIPAddress, fsl_base,

  // jcl
  JclDebug,

  // markdown
  MarkdownHTMLEntities,

  // FHIR.Support
  fsl_utilities, FHIR.Support.Osx,  fsl_collections, fsl_stream, FHIR.Support.Lang, fsl_logging, fsl_threads,
  fsl_json, fsl_xml, fsl_turtle, FHIR.Support.Certs, FHIR.Support.Signatures,

  // FHIR.Web
  fsl_fetcher, fsl_http, fsl_graphql,

  // Database
  fdb_sqlite3_objects, fdb_sqlite3_utilities, fdb_sqlite3_wrapper, FHIR.Database.Utilities, fdb_dialects, fdb_logging, fdb_manager, fdb_odbc_headers, fdb_odbc_objects, fdb_odbc, fdb_sqlite3,

  // FHIR.Base + others
    fhir_common, fhir_cdshooks, fhir_oauth, fhir_client, ftx_service,

  // FHIR.Cache
  fsl_npm_cache, FHIR.Npm.Client, fsl_npm,

  // UCUM
  ftx_ucum_search, ftx_ucum_services, ftx_ucum_validators, ftx_ucum_base, ftx_ucum_expressions, ftx_ucum_handlers, fsl_ucum,

  // LOINC
  ftx_loinc_publisher, ftx_loinc_services, ftx_loinc_importer,

  // SNOMED
  ftx_sct_importer, ftx_sct_publisher, ftx_sct_services, ftx_sct_analysis, ftx_sct_combiner, ftx_sct_expressions,

  // CDA
  cda_narrative, cda_objects, cda_parser, cda_types, cda_writer, cda_base, cda_documents,

  // Tools
  fhir_graphql,

  // R2:
  FHIR.R2.Tags, fhir2_types, fhir2_utilities, fhir2_xml, fhir2_authmap, fhir2_base, fhir2_client, fhir2_common,
  fhir2_constants, fhir2_context, fhir2_elementmodel, fhir2_factory, fhir2_indexinfo, fhir2_json,
  fhir2_narrative, fhir2_narrative2, fhir2_opbase, fhir2_operations, fhir2_parser, fhir2_parserBase,
  fhir2_patch, fhir2_pathengine, fhir2_pathnode, fhir2_profiles, fhir2_resources,

  // R3:
  fhir3_tags, fhir3_turtle, fhir3_types, fhir3_utilities, fhir3_xml, fhir3_authmap, fhir3_base, fhir3_client, fhir3_common,
  fhir3_constants, fhir3_context, fhir3_elementmodel, fhir3_factory, fhir3_indexinfo, fhir3_json, fhir3_liquid,
  fhir3_maputils, fhir3_narrative, fhir3_narrative2, fhir3_opbase, fhir3_operations, fhir3_organiser, fhir3_parser, fhir3_parserBase,
  fhir3_patch, fhir3_pathengine, fhir3_pathnode, fhir3_profiles, fhir3_resources,

  // R4:
  fhir4_tags, fhir4_turtle, fhir4_types, fhir4_utilities, fhir4_xml, fhir4_authmap, fhir4_base, fhir4_client, fhir4_common,
  fhir4_constants, fhir4_context, fhir4_elementmodel, fhir4_factory, fhir4_graphdefinition, fhir4_indexinfo, fhir4_json, fhir4_liquid,
  fhir4_maputils, fhir4_narrative, fhir4_opbase, fhir4_operations, fhir4_organiser, fhir4_parser, fhir4_parserBase,
  fhir4_patch, fhir4_pathengine, fhir4_pathnode, fhir4_profiles, fhir4_questionnaire, fhir4_resources,

  // v2
  v2_dictionary_v25, v2_dictionary_v26, v2_dictionary_v27, v2_dictionary_v231, v2_dictionary_v251, v2_dictionary_Versions,
  v2_message, v2_objects, v2_protocol, v2_base, v2_conformance, v2_dictionary_Compiled, v2_dictionary_Database,
  v2_dictionary, v2_dictionary_v21, v2_dictionary_v22, v2_dictionary_v23, v2_dictionary_v24;

implementation

end.

