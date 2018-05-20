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
program fhirtests2;

{$APPTYPE CONSOLE}

uses
  FastMM4 in '..\..\Libraries\FMM\FastMM4.pas',
  FastMM4Messages in '..\..\Libraries\FMM\FastMM4Messages.pas',
  ActiveX,
  IdSSLOpenSSLHeaders,
  SysUtils,
  TestInsight.DUnitX,
  FHIR.Support.Objects in '..\Support\FHIR.Support.Objects.pas',
  FHIR.Support.Stream in '..\support\FHIR.Support.Stream.pas',
  FHIR.Support.WInInet in '..\support\FHIR.Support.WInInet.pas',
  FHIR.Support.Binary in '..\support\FHIR.Support.Binary.pas',
  FHIR.Support.DateTime in '..\support\FHIR.Support.DateTime.pas',
  FHIR.Support.Decimal in '..\support\FHIR.Support.Decimal.pas',
  FHIR.Support.System in '..\support\FHIR.Support.System.pas',
  FHIR.Support.Lock in '..\support\FHIR.Support.Lock.pas',
  FHIR.Support.MXml in '..\support\FHIR.Support.MXml.pas',
  FHIR.Support.Math in '..\support\FHIR.Support.Math.pas',
  FHIR.Support.Mime in '..\support\FHIR.Support.Mime.pas',
  FHIR.Support.MsXml in '..\support\FHIR.Support.MsXml.pas',
  FHIR.Web.Parsers in '..\support\FHIR.Web.Parsers.pas',
  FHIR.Web.Rdf in '..\support\FHIR.Web.Rdf.pas',
  FHIR.Support.Shell in '..\support\FHIR.Support.Shell.pas',
  FHIR.Support.Strings in '..\support\FHIR.Support.Strings.pas',
  FHIR.Web.Fetcher in '..\support\FHIR.Web.Fetcher.pas',
  FHIR.Support.Turtle in '..\support\FHIR.Support.Turtle.pas',
  FHIR.Debug.Logging in '..\support\FHIR.Debug.Logging.pas',
  FHIR.Support.Signatures in '..\support\FHIR.Support.Signatures.pas',
  FHIR.Support.Exceptions in '..\support\FHIR.Support.Exceptions.pas',
  FHIR.Support.Collections in '..\support\FHIR.Support.Collections.pas',
  FHIR.Support.Factory in '..\support\FHIR.Support.Factory.pas',
  FHIR.Support.Controllers in '..\support\FHIR.Support.Controllers.pas',
  FHIR.Support.Text in '..\support\FHIR.Support.Text.pas',
  FHIR.Support.Generics in '..\support\FHIR.Support.Generics.pas',
  FHIR.Support.Xml in '..\support\FHIR.Support.Xml.pas',
  FHIR.Support.Json in '..\support\FHIR.Support.Json.pas',
  FHIR.Support.Certs in '..\support\FHIR.Support.Certs.pas',
  FHIR.Misc.GraphQL in '..\support\FHIR.Misc.GraphQL.pas',
  FHIR.Support.Zip in '..\support\FHIR.Support.Zip.pas',
  FHIR.Ucum.IFace in '..\support\FHIR.Ucum.IFace.pas',
  FHIR.Base.Objects in '..\base\FHIR.Base.Objects.pas',
  FHIR.Base.Lang in '..\base\FHIR.Base.Lang.pas',
  FHIR.Base.Parser in '..\base\FHIR.Base.Parser.pas',
  FHIR.Base.Scim in '..\base\FHIR.Base.Scim.pas',
  FHIR.Base.Xhtml in '..\base\FHIR.Base.Xhtml.pas',
  FHIR.Tools.Parser in '..\tools\FHIR.Tools.Parser.pas',
  FHIR.Tools.Session in '..\tools\FHIR.Tools.Session.pas',
  FHIR.Tools.Security in '..\tools\FHIR.Tools.Security.pas',
  FHIR.Tools.Indexing in '..\tools\FHIR.Tools.Indexing.pas',
  FHIR.Tools.XhtmlComp in '..\tools\FHIR.Tools.XhtmlComp.pas',
  FHIR.Tools.DiffEngine in '..\tools\FHIR.Tools.DiffEngine.pas',
  FHIR.R2.Constants,
  FHIR.R2.PathEngine in 'FHIR.R2.PathEngine.pas',
  FHIR.R2.Resources in 'FHIR.R2.Resources.pas',
  FHIR.R2.Utilities in 'FHIR.R2.Utilities.pas',
  FHIR.R2.Xml in 'FHIR.R2.Xml.pas',
  FHIR.R2.Json in 'FHIR.R2.Json.pas',
  DecimalTests in '..\support\Tests\DecimalTests.pas',
  DifferenceEngineTests in '..\support\Tests\DifferenceEngineTests.pas',
  FHIR.R2.Tests.PathEngine in 'tests\FHIR.R2.Tests.PathEngine.pas',
  FHIR.R2.Tests.Worker in 'tests\FHIR.R2.Tests.Worker.pas',
  JWTTests in '..\support\Tests\JWTTests.pas',
  JsonTests in '..\support\Tests\JsonTests.pas',
  XmlTests in '..\support\Tests\XmlTests.pas',
  FHIR.XVersion.Resources in '..\xversion\FHIR.XVersion.Resources.pas',
  FHIR.Base.Factory in '..\base\FHIR.Base.Factory.pas',
  FHIR.Base.Narrative in '..\base\FHIR.Base.Narrative.pas',
  FHIR.Base.Validator in '..\base\FHIR.Base.Validator.pas',
  FHIR.Base.PathEngine in '..\base\FHIR.Base.PathEngine.pas',
  FHIR.Client.Base in '..\client\FHIR.Client.Base.pas',
  FHIR.Client.SmartUtilities in '..\client\FHIR.Client.SmartUtilities.pas',
  FHIR.Client.Threaded in '..\client\FHIR.Client.Threaded.pas',
  FHIR.Client.HTTP in '..\client\FHIR.Client.HTTP.pas';

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
        raise Exception.Create('Provide input and output file names');
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
