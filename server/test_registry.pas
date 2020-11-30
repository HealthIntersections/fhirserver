unit test_registry;

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

(*
Tests Still to restore:

* fui_tests_graph
* fhir_tests_smart
* fhir4_tests_graphdefinition
* fhir4_tests_Maps
* tests_java_bridge
* tests_server_restful
* tests_server_full
*)

uses
  SysUtils, IniFiles,
  fsl_testing, fsl_utilities,
  MarkdownDaringFireballTests, MarkdownCommonMarkTests,
  fsl_tests, fsl_tests_web, fsl_tests_scrypt, fsl_tests_npm, fsl_tests_iduri,
  v2_tests, cda_tests, fdb_tests,
  ftx_tests_lang, ftx_tests_ucum, ftx_tests_sct,
  {$IFNDEF NO_JS} fhir_tests_javascript, {$ENDIF}
  fhir4_tests_parser, fhir4_tests_context, fhir4_tests_utilities, fhir4_tests_client, fhir4_tests_liquid, fhir4_tests_pathengine, fhir4_tests_graphql,
  fxver_tests,

  tests_search_syntax, test_server_config;

procedure registerTests;

implementation

  // before we register the tests, we have to set up the locations of 3 folders:
  // * the markdown github repo location (local root)
  // * the official tests github repo (local root)
  // * the github repo for the server (local root)
  // the tests don't clone these repos - this must be done first

const
{$IFDEF WINDOWS}
  DefaultMDTestRoot =      'c:\work\markdown';
  DefaultServerTestsRoot = 'c:\work\fhirserver';
  DefaultFHIRTestsRoot =   'c:\work\org.hl7.fhir\fhir-test-cases';
  DefaultMSSQLDriver = 'SQL Server';
  DefaultMySQLDriver = 'MySQL ODBC 8.0 Unicode Driver';
{$ENDIF}
{$IFDEF LINUX}
  DefaultMDTestRoot =      '/home/gg/markdown';
  DefaultServerTestsRoot = '/home/gg/fhirserver';
  DefaultFHIRTestsRoot =   '/home/gg/fhir-test-cases';
  DefaultMSSQLDriver = 'ODBC Driver 17 for SQL Server';
  DefaultMySQLDriver = 'MySQL ODBC 8.0 Unicode Driver';
{$ENDIF}
{$IFDEF OSX}
  DefaultServerTestsRoot = '/users/grahamegrieve/work/fhirserver';
  DefaultMDTestRoot =      '/users/grahamegrieve/work/markdown';
  DefaultFHIRTestsRoot =   '/users/grahamegrieve/work/fhir-test-cases';
  DefaultMSSQLDriver = 'ODBC Driver 17 for SQL Server';
  DefaultMySQLDriver = 'MySQL ODBC 8.0 Unicode Driver';
{$ENDIF}

Procedure SetUpDefaultTestSettings(filename : String);
var
  ini : TIniFile;
begin
  ini := TIniFile.Create(filename);
  try
    ini.WriteString('locations', 'fhirserver', DefaultServerTestsRoot);
    ini.WriteString('locations', 'fhir-test-cases', DefaultFHIRTestsRoot);
    ini.WriteString('locations', 'markdown', DefaultMDTestRoot);

    // database tests:
    ini.WriteString('mssql', 'driver', DefaultMSSQLDriver);
    ini.WriteString('mssql', 'server', '(local)');
    ini.WriteString('mssql', 'database', 'test');

    // database tests:
    ini.WriteString('mysql', 'driver', DefaultMySQLDriver);
    ini.WriteString('mysql', 'server', 'localhost');
    ini.WriteString('mysql', 'database', 'test');
    ini.WriteString('mysql', 'username', 'test');
    ini.WriteString('mysql', 'password', 'test');
  finally
    ini.Free;
  end;
end;

procedure registerTests;
var
  iniName : String;
begin
  if not getCommandLineParam('test-settings', iniName) then
    iniName := partnerFile('test-settings.ini');

  if not FileExists(iniName) then
     setupDefaultTestSettings(iniName);

  TestSettings.free;
  TestSettings := TFslTestSettings.Create(iniName);

  MarkdownDaringFireballTests.registerTests;
  MarkdownCommonMarkTests.registerTests;
  fsl_tests.registerTests;
  fsl_tests_iduri.registerTests;
  fsl_tests_scrypt.registerTests;
  fsl_tests_web.registerTests;
  cda_tests.registerTests;
  ftx_tests_lang.registerTests;
  fdb_tests.registerTests;
  ftx_tests_ucum.registerTests;
  ftx_tests_sct.registerTests;
  v2_tests.registerTests;
  {$IFNDEF NO_JS}
  fhir_tests_javascript.registerTests;
  {$ENDIF}
  fsl_tests_npm.registerTests;
  fhir4_tests_Parser.registerTests;
  fhir4_tests_context.registerTests;
  fhir4_tests_utilities.registerTests;
  fhir4_tests_client.registerTests;
  fhir4_tests_liquid.registerTests;
  fhir4_tests_pathengine.registerTests;
  fhir4_tests_graphql.registerTests;
  fxver_tests.registerTests;
  tests_search_syntax.registerTests;
  test_server_config.registerTests;
end;

end.
