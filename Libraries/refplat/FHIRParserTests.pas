unit FHIRParserTests;

interface

uses
  AdvObjects;

type
  TFHIRParserTests = class (TAdvObject)
  private
    procedure Roundtrip(Source, Dest: String);


  public
    class procedure runTests;
  end;

implementation

uses
  SysUtils, Classes,
  FHIRBase, FHIRParser, FHIRParserBase, FHIRResources;

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


procedure TFHIRParserTests.Roundtrip(Source, Dest : String);
var
  f : TFileStream;
  m : TMemoryStream;
  p : TFHIRParser;
  c : TFHIRComposer;
  r : TFhirResource;
begin
  r := nil;
  try
    p := TFHIRXmlParser.Create('en');
    try
      p.ParserPolicy := xppDrop;
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

//  IdSoapXmlCheckDifferent(source, dest);
end;


class procedure TFHIRParserTests.runTests;
var
  this : TFHIRParserTests;
begin
  this := TFHIRParserTests.Create;
  try
    this.Roundtrip('C:\work\org.hl7.fhir\build\publish\issue-type.xml', 'c:\temp\test.xml');
  finally
    this.Free;
  end;
end;

end.
