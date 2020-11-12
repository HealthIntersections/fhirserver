{
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
program fhirtests2;

{$APPTYPE CONSOLE}

uses
  FastMM4 in '..\..\dependencies\FMM\FastMM4.pas',
  FastMM4Messages in '..\..\dependencies\FMM\FastMM4Messages.pas',
  ActiveX,
  IdSSLOpenSSLHeaders,
  SysUtils,
  TestInsight.DUnitX,
  fsl_base in '..\Support\fsl_base.pas',
  fsl_stream in '..\support\fsl_stream.pas',
  fsl_wininet in '..\support\fsl_wininet.pas',
  fsl_utilities in '..\support\fsl_utilities.pas',
  fsl_threads in '..\support\fsl_threads.pas',
  fsl_xml in '..\support\fsl_xml.pas',
  fsl_msxml in '..\support\fsl_msxml.pas',
  fsl_http in '..\support\fsl_http.pas',
  fsl_rdf in '..\support\fsl_rdf.pas',
  fsl_shell in '..\support\fsl_shell.pas',
  fsl_fetcher in '..\support\fsl_fetcher.pas',
  fsl_turtle in '..\support\fsl_turtle.pas',
  fsl_logging in '..\support\fsl_logging.pas',
  FHIR.Support.Signatures in '..\support\FHIR.Support.Signatures.pas',
  fsl_collections in '..\support\fsl_collections.pas',
  fsl_xml in '..\support\fsl_xml.pas',
  fsl_json in '..\support\fsl_json.pas',
  FHIR.Support.Certs in '..\support\FHIR.Support.Certs.pas',
  fsl_graphql in '..\support\fsl_graphql.pas',
  fhir_ucum in '..\support\fhir_ucum.pas',
  fhir_objects in '..\base\fhir_objects.pas',
  FHIR.Base.Lang in '..\base\FHIR.Base.Lang.pas',
  fhir_parser in '..\base\fhir_parser.pas',
  fsl_scim in '..\base\fsl_scim.pas',
  fhir_xhtml in '..\base\fhir_xhtml.pas',
  FHIR.Version.Parser in '..\version\FHIR.Version.Parser.pas',
  FHIR.Tools.Indexing in '..\tools\FHIR.Tools.Indexing.pas',
  FHIR.Tools.DiffEngine in '..\tools\FHIR.Tools.DiffEngine.pas',
  fhir2_constants,
  fhir2_pathengine in 'fhir2_pathengine.pas',
  fhir2_resources in 'fhir2_resources.pas',
  fhir2_utilities in 'fhir2_utilities.pas',
  fhir2_xml in 'fhir2_xml.pas',
  fhir2_json in 'fhir2_json.pas',
  FHIR.Tests.Decimal in '..\support\Tests\FHIR.Tests.Decimal.pas',
  DifferenceEngineTests in '..\support\Tests\DifferenceEngineTests.pas',
  FHIR.R2.Tests.PathEngine in 'tests\FHIR.R2.Tests.PathEngine.pas',
  FHIR.R2.Tests.Worker in 'tests\FHIR.R2.Tests.Worker.pas',
  JWTTests in '..\support\Tests\JWTTests.pas',
  JsonTests in '..\support\Tests\javascriptonTests.pas',
  XmlTests in '..\support\Tests\XmlTests.pas',
  fhir_common in '..\base\fhir_common.pas',
  fhir_factory in '..\base\fhir_factory.pas',
  fhir_narrative in '..\base\fhir_narrative.pas',
  fhir_validator in '..\base\fhir_validator.pas',
  fhir_pathengine in '..\base\fhir_pathengine.pas',
  fhir_client in '..\client\fhir_client.pas',
  fhir_client_threaded in '..\client\fhir_client_threaded.pas',
  fhir_client_http in '..\client\fhir_client_http.pas',
  fsl_fpc in '..\support\fsl_fpc.pas',
  FHIR.Support.Osx in '..\support\FHIR.Support.Osx.pas',
  fhir_utilities in '..\base\fhir_utilities.pas',
  fsl_npm_cache in '..
pm\fsl_npm_cache.pas',
  fhir_oauth in '..\client\fhir_oauth.pas';

(*
procedure SaveStringToFile(s : AnsiString; fn : String);
var
  f : TFileStream;
begin
  f := TFileStream.Create(fn, fmCreate);
  try
    f.Write(s[1], length(s));
  finally
    f.free;
  end;
end;

var
  f : TFileStream;
  m : TMemoryStream;
  p : TFHIRParser;
  c : TFHIRComposer;
  r : TFhirResource;
procedure Roundtrip(Source, Dest : String);
begin
  try
    p := TFHIRXmlParser.Create('en');
    try
      f := TFileStream.Create(source, fmopenRead,+ fmShareDenyWrite);
      try
        p.source := f;
        p.Parse;
        r := p.resource.Link;
      finally
        f.Free;
      end;
    finally
      p.free;
    end;
    m := TMemoryStream.Create;
    try
      c := TFHIRJsonComposer.Create('en');
      try
        TFHIRJsonComposer(c).Comments := true;
        c.Compose(m, r, true, nil);
      finally
        c.free;
      end;
      m.Position := 0;
      m.SaveToFile(ChangeFileExt(dest, '.json'));
      m.Position := 0;
      r.Free;
      r := nil;
      p := TFHIRJsonParser.Create('en');
      try
        p.source := m;
        p.Parse;
        r := p.resource.Link;
      finally
        p.Free;
      end;
    finally
      m.Free;
    end;
    f := TFileStream.Create(dest, fmCreate);
    try
      c := TFHIRXMLComposer.Create('en');
      try
        c.Compose(f, r, true, nil);
      finally
        c.free;
      end;
    finally
      f.free;
    end;
  finally
    r.Free;
  end;
end;

procedure roundTripDirectory(pathSource, pathDest : String);
var
  SR: TSearchRec;
  s : String;
begin
  if FindFirst(IncludeTrailingPathDelimiter(PathSource) + '*.xml', faAnyFile, SR) = 0 then
  begin
    repeat
      s := copy(SR.Name, 1, pos('.', SR.Name)-1);
      if (SR.Attr <> faDirectory) and (copy(s, length(s)-1, 2) <> '-d') then
      begin
        Writeln(SR.name);
        Roundtrip(IncludeTrailingPathDelimiter(pathSource)+SR.Name, IncludeTrailingPathDelimiter(pathDest)+s+'-d'+ExtractFileExt((SR.Name)));
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

begin
  try
    CoInitialize(nil);
    if DirectoryExists(ParamStr(2)) and DirectoryExists(Paramstr(1)) then
      roundTripDirectory(Paramstr(1), ParamStr(2))
    else
    begin
      if (ParamStr(1) = '') or (ParamStr(2) = '') or not FileExists(paramstr(1)) then
        raise EFHIRException.create('Provide input and output file names');
      roundTrip(paramStr(1), paramStr(2));
    end;
  except
    on e:exception do
      SaveStringToFile(AnsiString(e.Message), ParamStr(2)+'.err');
  end;
  *)
var
  s : String;
begin
  CoInitialize(nil);
  s := ExtractFilePath(Paramstr(0));
  IdOpenSSLSetLibPath(s);
  GBasePath := 'C:\work\org.hl7.fhir.2016May';
  RunRegisteredTests;
  TTestingWorkerContext.closeUp;
end.
