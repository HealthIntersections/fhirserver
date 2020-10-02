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
  IdHeaderList, IdGlobal, IdIPAddress, FHIR.Support.Base,

  // jcl
  JclDebug,

  // markdown
  MarkdownHTMLEntities,

  // FHIR.Support
  FHIR.Support.Utilities, FHIR.Support.Osx,  FHIR.Support.Collections, FHIR.Support.Stream, FHIR.Support.Lang, FHIR.Support.Logging, FHIR.Support.Threads,
  FHIR.Support.JSON, FHIR.Support.Xml, FHIR.Support.Turtle, FHIR.Support.Certs, FHIR.Support.Signatures,

  // FHIR.Web
  FHIR.Web.Fetcher, FHIR.Web.Parsers, FHIR.Web.GraphQL,

  // Database
  FHIR.Database.SQLite3.Objects, FHIR.Database.SQLite3.Utilities, FHIR.Database.SQLite3.Wrapper, FHIR.Database.Utilities, FHIR.Database.Dialects, FHIR.Database.Logging, FHIR.Database.Manager, FHIR.Database.ODBC.Headers, FHIR.Database.ODBC.Objects, FHIR.Database.ODBC, FHIR.Database.SQLite,

  // FHIR.Base + others
  FHIR.Base.Lang,  FHIR.Base.Common, FHIR.CdsHooks.Utilities, FHIR.Smart.Utilities, FHIR.Client.Base, FHIR.Tx.Service,

  // FHIR.Cache
  FHIR.Cache.PackageManager, FHIR.Cache.PackageClient, FHIR.Cache.NpmPackage,

  // UCUM
  FHIR.Ucum.Search, FHIR.Ucum.Services, FHIR.Ucum.Validators, FHIR.Ucum.Base, FHIR.Ucum.Expressions, FHIR.Ucum.Handlers, FHIR.Ucum.IFace,

  // LOINC
  FHIR.Loinc.Publisher, FHIR.Loinc.Services, FHIR.Loinc.Importer,

  // SNOMED
  FHIR.Snomed.Importer, FHIR.Snomed.Publisher, FHIR.Snomed.Services, FHIR.Snomed.Analysis, FHIR.Snomed.Combiner, FHIR.Snomed.Expressions,

  // CDA
  FHIR.Cda.Narrative, FHIR.Cda.Objects, FHIR.Cda.Parser, FHIR.Cda.Types, FHIR.Cda.Writer, FHIR.Cda.Base, FHIR.Cda.Documents,

  // Tools
  FHIR.Tools.GraphQL,

  // R2:
  FHIR.R2.Tags, FHIR.R2.Types, FHIR.R2.Utilities, FHIR.R2.Xml, FHIR.R2.AuthMap, FHIR.R2.Base, FHIR.R2.Client, FHIR.R2.Common,
  FHIR.R2.Constants, FHIR.R2.Context, FHIR.R2.ElementModel, FHIR.R2.Factory, FHIR.R2.IndexInfo, FHIR.R2.Json,
  FHIR.R2.Narrative, FHIR.R2.Narrative2, FHIR.R2.OpBase, FHIR.R2.Operations, FHIR.R2.Parser, FHIR.R2.ParserBase,
  FHIR.R2.Patch, FHIR.R2.PathEngine, FHIR.R2.PathNode, FHIR.R2.Profiles, FHIR.R2.Resources,

  // R3:
  FHIR.R3.Tags, FHIR.R3.Turtle, FHIR.R3.Types, FHIR.R3.Utilities, FHIR.R3.Xml, FHIR.R3.AuthMap, FHIR.R3.Base, FHIR.R3.Client, FHIR.R3.Common,
  FHIR.R3.Constants, FHIR.R3.Context, FHIR.R3.ElementModel, FHIR.R3.Factory, FHIR.R3.IndexInfo, FHIR.R3.Json, FHIR.R3.Liquid,
  FHIR.R3.MapUtilities, FHIR.R3.Narrative, FHIR.R3.Narrative2, FHIR.R3.OpBase, FHIR.R3.Operations, FHIR.R3.Organiser, FHIR.R3.Parser, FHIR.R3.ParserBase,
  FHIR.R3.Patch, FHIR.R3.PathEngine, FHIR.R3.PathNode, FHIR.R3.Profiles, FHIR.R3.Resources,

  // R4:
  FHIR.R4.Tags, FHIR.R4.Turtle, FHIR.R4.Types, FHIR.R4.Utilities, FHIR.R4.Xml, FHIR.R4.AuthMap, FHIR.R4.Base, FHIR.R4.Client, FHIR.R4.Common,
  FHIR.R4.Constants, FHIR.R4.Context, FHIR.R4.ElementModel, FHIR.R4.Factory, FHIR.R4.GraphDefinition, FHIR.R4.IndexInfo, FHIR.R4.Json, FHIR.R4.Liquid,
  FHIR.R4.MapUtilities, FHIR.R4.Narrative, FHIR.R4.OpBase, FHIR.R4.Operations, FHIR.R4.Organiser, FHIR.R4.Parser, FHIR.R4.ParserBase,
  FHIR.R4.Patch, FHIR.R4.PathEngine, FHIR.R4.PathNode, FHIR.R4.Profiles, FHIR.R4.Questionnaire, FHIR.R4.Resources,

  // v2
  FHIR.v2.Dictionary.v25, FHIR.v2.Dictionary.v26, FHIR.v2.Dictionary.v27, FHIR.v2.Dictionary.v231, FHIR.v2.Dictionary.v251, FHIR.v2.Dictionary.Versions,
  FHIR.v2.Message, FHIR.V2.Objects, FHIR.v2.Protocol, FHIR.v2.Base, FHIR.v2.Conformance, FHIR.v2.Dictionary.Compiled, FHIR.v2.Dictionary.Database,
  FHIR.v2.Dictionary, FHIR.v2.Dictionary.v21, FHIR.v2.Dictionary.v22, FHIR.v2.Dictionary.v23, FHIR.v2.Dictionary.v24;

implementation

end.

