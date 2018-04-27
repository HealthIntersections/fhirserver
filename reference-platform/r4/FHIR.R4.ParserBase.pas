unit FHIR.R4.ParserBase;

interface

uses
  SysUtils,
  FHIR.Support.Json, FHIR.Support.MXml, FHIR.Support.Xml, FHIR.Support.Turtle,
  FHIR.Base.Objects, FHIR.Base.Parser, FHIR.R4.Types, FHIR.R4.Resources;

type
  TFHIRXmlParserBase4 = class (TFHIRXmlParserBase)
  protected
    function ParseResource(element : TMXmlElement; path : String) : TFhirResource; virtual;
    Function ParseResourceV(element : TMXmlElement; path : String) : TFhirResourceV; override;

    function ParseDataType(element : TMXmlElement; name : String; type_ : TFHIRTypeClass) : TFHIRType; virtual;

    Function ParseInnerResource(element : TMXmlElement; path : String) : TFhirResource;
  end;

  TFHIRJsonParserBase4 = class (TFHIRJsonParserBase)
  protected
    function ParseResource(jsn : TJsonObject) : TFhirResource; virtual;
    Function ParseResourceV(jsn : TJsonObject) : TFhirResourceV; override;

    function ParseDataTypeV(jsn : TJsonObject; name : String; type_ : TClass) : TFHIRObject; override;
    function ParseDataType(jsn : TJsonObject; name : String; type_ : TFHIRTypeClass) : TFHIRType; virtual;

    procedure ParseInnerResource(jsn : TJsonObject; ctxt : TFHIRObjectList);  overload;
    function ParseInnerResource(jsn: TJsonObject) : TFhirResource; overload;
  end;

  TFHIRTurtleParserBase4 = class (TFHIRTurtleParserBase)
  protected
    function ParseResource(obj : TTurtleComplex) : TFhirResource; virtual;
    Function ParseResourceV(obj : TTurtleComplex) : TFhirResourceV; override;

    function ParseDataType(obj : TTurtleComplex; name : String; type_ : TFHIRTypeClass) : TFHIRType; virtual;

    function ParseInnerResource(obj : TTurtleObject) : TFHIRResource;
  end;

  TFHIRXmlComposerBase4 = class (TFHIRXmlComposerBase)
  protected
    procedure ComposeResourceV(xml : TXmlBuilder; resource : TFhirResourceV); override;
    procedure ComposeResource(xml : TXmlBuilder; resource : TFhirResource); virtual;
  end;

  TFHIRJsonComposerBase4 = class (TFHIRJsonComposerBase)
  protected
    procedure ComposeResourceV(json : TJSONWriter; resource : TFhirResourceV); override;
    procedure ComposeResource(json : TJSONWriter; resource : TFhirResource); virtual;

  end;

  TFHIRTurtleComposerBase4 = class (TFHIRTurtleComposerBase)
  protected
    procedure ComposeResourceV(parent :  TTurtleComplex; resource : TFhirResourceV); override;
    procedure ComposeResource(parent :  TTurtleComplex; resource : TFhirResource); virtual;
  end;

(*,
  FHIR.Tools.Parser,
  {$IFDEF FHIR2}
  FHIR.R2.Utilities,
  FHIR.R2.Profiles,
  FHIR.R2.ElementModel;
  {$ENDIF}
  {$IFDEF FHIR3}
  FHIR.R3.Utilities,
  FHIR.R3.Profiles,
  FHIR.R3.ElementModel;
  {$ENDIF}
  {$IFDEF FHIR4}
  FHIR.R4.Utilities,
  FHIR.R4.Profiles,
  FHIR.R4.ElementModel;
  {$ENDIF}
uses
  FHIR.Base.Parser,
  FHIR.R4.Resources, FHIR.R4.Types, FHIR.R4.Constants, FHIR.R4.Context, FHIR.R4.Tags, FHIR.R4.PathNode;

      class procedure composeFile(worker : TFHIRWorkerContextV; r : TFHIRResourceV; lang : String; filename : String; style : TFHIROutputStyle); overload;

type
  TFHIRParser4 = class (TFHIRParser)
  private
  public
    Constructor Create(lang : String); Virtual;
  end;

  TFHIRXmlParserBase4 = class (TFHIRXmlParserBase)
  protected
  public
    Constructor Create(worker : TFHIRWorkerContext; lang : String); Virtual;
    function ParseDT(rootName : String; type_ : TFHIRTypeClass) : TFHIRType; Override;
    function ParseDT(rootName : String; type_ : TFHIRTypeClass) : TFHIRType; Virtual; abstract;
    class function ParseFragment(worker : TFHIRWorkerContext; fragment, lang : String) : TFHIRObject; overload;
    class function ParseFile(worker : TFHIRWorkerContext; lang : String; filename : String) : TFHIRResource; overload;
  end;

  TFHIRJsonParserBase4 = class (TFHIRParser)
      function ParseDataType(jsn : TJsonObject; name : String; type_ : TFHIRTypeClass) : TFHIRType; virtual;
    class function ParseFragment(worker : TFHIRWorkerContext; fragment, type_, lang : String) : TFHIRObject; overload;
    class function ParseFile(worker : TFHIRWorkerContext; lang : String; filename : String) : TFHIRResource; overload;
                                                                                  function ParseDT(rootName : String; type_ : TFHIRTypeClass) : TFHIRType; Override;
  end;

  turtle
    function ParseDataType(obj : TTurtleComplex; name : String; type_ : TFHIRTypeClass) : TFHIRType; virtual;
    function ParseDT(rootName : String; type_ : TFHIRTypeClass) : TFHIRType; Override;
    class function ParseFile(worker : TFHIRWorkerContext; lang : String; filename : String) : TFHIRResource; overload;

  text
      function ParseDT(rootName : String; type_ : TFHIRTypeClass) : TFHIRType; Override;
    class function ParseFile(worker : TFHIRWorkerContext; lang : String; filename : String) : TFHIRResource; overload;


compose
    Procedure ComposeResource(xml : TXmlBuilder; oResource : TFhirResource; links : TFhirBundleLinkList = nil); overload; virtual;
    Procedure Compose(stream : TStream; oResource : TFhirResourceV; links : TFhirBundleLinkList = nil); Overload; Virtual;



text
    function render(op : TFHIROperationOutcome) : String;
*)
implementation

(*
,
  {$IFDEF FHIR2}
  FHIR.R2.Resources, FHIR.R2.Types, FHIR.R2.Constants, FHIR.R2.Context, FHIR.R2.Tags, FHIR.R2.PathNode;
  {$ENDIF}
  {$IFDEF FHIR3}
  FHIR.R3.Resources, FHIR.R3.Types, FHIR.R3.Constants, FHIR.R3.Context, FHIR.R3.Tags, FHIR.R3.PathNode;
  {$ENDIF}
  {$IFDEF FHIR4}

  function TFHIRXmlParserBase.ParseResource(element: TMXmlElement; path : String): TFhirResource;
begin
  raise exception.create('don''t use TFHIRXmlParserBase directly - use TFHIRXmlParser');
end;

  {
  Support for custom resources
          procedure TFHIRXmlParserBase.Parse;
var
  xml : TMXmlDocument;
  root : TMXmlElement;
  sd : TFHIRStructureDefinition;
  e : TFHIRMMElement;
  x : TFHIRMMXmlParser;
begin
  xml := nil;
  try
    FComments := TAdvStringList.create;
    try
      if (Element = nil) then
      begin
        xml := LoadXml(Source);
        root := xml.document;
      end
      else
        root := element;

      if root.namespaceURI = FHIR_NS Then
        resource := ParseResource(root, '')
      else
      begin
        // well, ok, we'll look to see if it's a logical model....
        if FWorker = nil then
          sd := nil
        else
          sd := FWorker.getStructure(root.namespaceURI, root.localName);
        if sd = nil then
          XmlError('/', StringFormat(GetFhirMessage('MSG_WRONG_NS', lang), [root.namespaceURI]))
        else
        begin
          x := TFHIRMMXmlParser.create(FWorker.Link);
          try
            e := x.parse(root, sd);
            try
              resource := TFHIRCustomResource.create(e.link);
            finally
              e.free;
            end;
          finally
            x.free;
          end;
        end;
      end;

    finally
      FComments.Free;
    end;
  finally
    xml.free;
  end;
end;
  }

  function TFHIRJsonParserBase.ParseDataType(jsn : TJsonObject; name : String; type_ : TFHIRTypeClass) : TFHIRType;
begin
  raise exception.create('don''t use TFHIRJsonParserBase directly - use TFHIRJsonParser');
end;


function TFHIRJsonParserBase.ParseDT(rootName: String; type_: TFHIRTypeClass): TFHIRType;
var
  obj : TJsonObject;
begin
  start;
  obj := TJSONParser.Parse(source, timelimit);
  try
    result := ParseDataType(obj, rootName, type_);
  finally
    obj.Free;
  end;
end;

{
composing custom resources

procedure TFHIRXmlComposerBase.Compose(stream: TStream; oResource: TFhirResourceV);
var
  xml : TXmlBuilder;
  mx : TFHIRMMXmlParser;
begin
  if (oResource <> nil) and (oResource.resourceType = frtCustom) then
  begin
    mx := TFHIRMMXmlParser.create(FWorker.Link);
    try
      mx.compose(TFHIRCustomResource(oResource).Root, stream, isPretty, '');
    finally
      mx.free;
    end;
  end
  else
  begin
    xml := TAdvXmlBuilder.Create;
    try
      xml.IsPretty := isPretty;
      xml.NoHeader := NoHeader;
      if isCanonical then
        TAdvXmlBuilder(xml).CanonicalEntities := true;
      xml.CurrentNamespaces.DefaultNS := FHIR_NS;
      xml.Start;
      if not isCanonical and (FComment <> '') then
        xml.Comment(FComment);
      ComposeResource(xml, oResource, links);
      xml.Finish;
      xml.Build(stream);
    finally
      xml.Free;
    end;
  end;
end;



}

class procedure TFHIRXmlComposerBase.composeFile(worker : TFHIRWorkerContextV; r: TFHIRResourceV; lang, filename: String; style : TFHIROutputStyle);
var
  x: TFHIRXmlComposer;
  f : TFileStream;
begin
  f := TFileStream.Create(filename, fmCreate);
  try
    x := TFHIRXmlComposer.Create(worker.link, style, lang);
    try
      x.Compose(f, r);
    finally
      x.Free;
    end;
  finally
    f.Free;
  end;
end;





class procedure TFHIRJsonComposerBase.composeFile(worker: TFHIRWorkerContext; r: TFHIRResource; lang, filename: String; style: TFHIROutputStyle);
var
  j: TFHIRJsonComposer;
  f : TFileStream;
begin
  f := TFileStream.Create(filename, fmCreate);
  try
    j := TFHIRJsonComposer.Create(worker.link, style, lang);
    try
      j.Compose(f, r);
    finally
      j.Free;
    end;
  finally
    f.Free;
  end;
end;


class function TFHIRXmlParserBase.ParseFile(worker : TFHIRWorkerContext; lang : String; filename: String): TFHIRResource;
var
  x : TFHIRXmlParser;
begin
  x := TFHIRXmlParser.Create(worker.link, lang);
  try
    x.ParseFile(filename);
    result := x.resource.Link;
  finally
    x.Free;
  end;
end;


class function TFHIRJsonParserBase.ParseFile(worker: TFHIRWorkerContext; lang, filename: String): TFHIRResource;
var
  j : TFHIRJsonParser;
begin
  j := TFHIRJsonParser.Create(worker.link, lang );
  try
    j.ParseFile(filename);
    result := j.resource.Link;
  finally
    j.Free;
  end;
end;

class function TFHIRJsonParserBase.ParseFragment(worker: TFHIRWorkerContext; fragment, type_, lang: String): TFHIRObject;
var
  ss : TBytesStream;
  p : TFHIRJsonParser;
  jsn : TJsonObject;
begin
  ss := TBytesStream.Create(TEncoding.UTF8.getBytes(fragment));
  try
    jsn := TJSONParser.Parse(ss);
    try
      p := TFHIRJsonParser.Create(worker.link, lang);
      try
        result := p.ParseFragment(jsn, type_);
      finally
        p.Free;
      end;
    finally
      jsn.Free;
    end;
  finally
    ss.Free;
  end;
end;


class function TFHIRXmlParserBase.ParseFragment(worker: TFHIRWorkerContext; fragment, lang: String): TFHIRObject;
var
  ss : TBytesStream;
  p : TFHIRXmlParser;
  xml : TMXmlDocument;
begin
  result := nil;
  ss := TBytesStream.Create(TEncoding.UTF8.getBytes(fragment));
  try
    p := TFHIRXmlParser.Create(worker.link, lang);
    try
      p.source := ss;
      xml := p.LoadXml(ss);
      try
        if xml.document.namespaceURI <> FHIR_NS Then
          raise Exception.Create('Unknown namespace');
        result := p.ParseFragment(xml.document);
      finally
        xml.Free;
      end;
    finally
      p.free;
    end;
  finally
    ss.Free;
  end;

end;


function TFHIRXmlParserBase.ParseDT(rootName: String; type_: TFHIRTypeClass): TFHIRType;
var
  xml : TMXmlDocument;
  root : TMXmlElement;
begin
  start;
  xml := nil;
  try
    FComments := TAdvStringList.create;
    try
      if (Element = nil) then
      begin
        xml := LoadXml(Source);
        root := xml.document;
      end
      else
        root := element;

      if root.namespaceURI <> FHIR_NS Then
        XmlError('/', StringFormat(GetFhirMessage('MSG_WRONG_NS', lang), [root.namespaceURI]));

      result := ParseDataType(root, rootName, type_);
    finally
      FComments.Free;
    end;
  finally
    xml.Free;
  end;
end;





class function TFHIRTextParser.ParseFile(worker: TFHIRWorkerContext; lang, filename: String): TFHIRResource;
var
  t : TFHIRTextParser;
begin
  t := TFHIRTextParser.Create(worker.link, lang);
  try
    t.ParseFile(filename);
    result := t.resource.Link;
  finally
    t.Free;
  end;
end;


procedure TFHIRTextComposer.Compose(stream: TStream; oResource: TFhirResourceV);
begin
  case oResource.ResourceType of
    frtOperationOutcome : StringToStream(render(oResource as TFHIROperationOutcome), stream, TEncoding.UTF8);
  else
    raise Exception.Create('Text format not supported for '+oResource.fhirtype);
  end;
end;



function TFHIRTextComposer.render(op: TFHIROperationOutcome): String;
var
  b : TStringBuilder;
  iss : TFhirOperationOutcomeIssue;
begin
  b := TStringBuilder.create;
  try
    if op.issueList.Count = 0 then
      b.Append('All Ok (no issues)')
    else
      for iss in op.issueList do
      begin
        b.Append(CODES_TFhirIssueSeverityEnum[iss.severity]);
        b.Append(': ');
        b.Append(gen(iss.details));
        {$IFDEF FHIR2}
        if (iss.locationList.Count > 0) then
        {$ELSE}
        if (iss.expressionList.Count > 0) then
        {$ENDIF}
        begin
          b.Append(' @ ');
          {$IFDEF FHIR2}
          b.Append(iss.locationList[0].Value);
          {$ELSE}
          b.Append(iss.expressionList[0].Value);
          {$ENDIF}
        end;
        b.Append(#13#10);
      end;
    result := b.toString;
  finally
    b.free;
  end;
end;


class function TFHIRTurtleParserBase.ParseFile(worker: TFHIRWorkerContext; lang, filename: String): TFHIRResource;
var
  j : TFHIRTurtleParser;
begin
  j := TFHIRTurtleParser.Create(worker.link, lang);
  try
    j.ParseFile(filename);
    result := j.resource.Link;
  finally
    j.Free;
  end;
end;

*)

{ TFHIRXmlParserBase4 }

function TFHIRXmlParserBase4.ParseInnerResource(element: TMXmlElement; path: String): TFhirResource;
var
  child : TMXmlElement;
begin
  child := FirstChild(element);
  result := ParseResourceV(child, path) as TFhirResource;
  try
    child := NextSibling(child);
    if (child <> nil) then
      UnknownContent(child, path);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRXmlParserBase4.ParseResource(element: TMXmlElement; path: String): TFhirResource;
begin
  raise exception.create('don''t use '+className+' directly - use TFHIRXmlComposer');
end;

function TFHIRXmlParserBase4.ParseResourceV(element: TMXmlElement; path: String): TFhirResourceV;
begin
  result := ParseResource(element, path);
end;

function TFHIRXmlParserBase4.ParseDataType(element: TMXmlElement; name: String; type_: TFHIRTypeClass): TFHIRType;
begin
  raise exception.create('don''t use TFHIRXmlParserBase directly - use TFHIRXmlParser');
end;


{ TFHIRXmlComposerBase4 }


{ TFHIRXmlComposerBase4 }

procedure TFHIRXmlComposerBase4.ComposeResource(xml: TXmlBuilder; resource: TFhirResource);
begin
  raise exception.create('don''t use '+className+' directly - use TFHIRXmlComposer');
end;

procedure TFHIRXmlComposerBase4.ComposeResourceV(xml: TXmlBuilder; resource: TFhirResourceV);
begin
  ComposeResource(xml, resource as TFhirResource);

end;

{ TFHIRJsonParserBase4 }

function TFHIRJsonParserBase4.ParseDataType(jsn : TJsonObject; name : String; type_ : TFHIRTypeClass): TFHIRType;
begin
  raise exception.create('don''t use TFHIRXmlParserBase directly - use TFHIRXmlParser');
end;

function TFHIRJsonParserBase4.ParseResource(jsn: TJsonObject): TFhirResource;
begin
  raise exception.create('don''t use TFHIRJsonParserBase4 directly - use TFHIRXmlComposer');
end;

function TFHIRJsonParserBase4.ParseResourceV(jsn: TJsonObject): TFhirResourceV;
begin
  result := ParseResource(jsn);
end;

procedure TFHIRJsonParserBase4.ParseInnerResource(jsn: TJsonObject; ctxt: TFHIRObjectList);
begin
  ctxt.add(ParseResourceV(jsn));
end;

function TFHIRJsonParserBase4.ParseDataTypeV(jsn: TJsonObject; name: String; type_: TClass): TFHIRObject;
begin
  result := ParseDataType(jsn, name, TFHIRTypeClass(type_));
end;

function TFHIRJsonParserBase4.ParseInnerResource(jsn: TJsonObject) : TFhirResource;
begin
  result := ParseResourceV(jsn) as TFhirResource;
end;

{ TFHIRJsonComposerBase4 }

procedure TFHIRJsonComposerBase4.ComposeResource(json: TJSONWriter; resource: TFhirResource);
begin
  raise exception.create('don''t use '+className+' directly - use TFHIRXmlComposer');
end;

procedure TFHIRJsonComposerBase4.ComposeResourceV(json: TJSONWriter; resource: TFhirResourceV);
begin
  ComposeResource(json, resource as TFhirResource);
end;

{ TFHIRTurtleParserBase4 }

function TFHIRTurtleParserBase4.ParseDataType(obj: TTurtleComplex; name: String; type_: TFHIRTypeClass): TFHIRType;
begin
  raise exception.create('don''t use TFHIRTurtleParserBase4 directly - use TFHIRXmlParser');
end;

function TFHIRTurtleParserBase4.ParseInnerResource(obj: TTurtleObject): TFHIRResource;
var
  c : TTurtleComplex;
begin
  if obj = nil then
    result := nil
  else
  begin
    if obj is TTurtleComplex then
      c := obj as TTurtleComplex
    else if (obj is TTurtleURL) then
    begin
      c := FDoc.getObject(TTurtleURL(obj).uri);
      if c = nil then
        raise Exception.Create('Unable to resolve internal resource reference in RDF - to '+TTurtleURL(obj).uri)
    end
    else
      raise Exception.Create('Unable to process internal resource reference in RDF');
    result := ParseResourceV(c) as TFHIRResource;
  end;
end;

function TFHIRTurtleParserBase4.ParseResource(obj: TTurtleComplex): TFhirResource;
begin
  raise exception.create('don''t use '+className+' directly - use TFHIRXmlComposer');
end;

function TFHIRTurtleParserBase4.ParseResourceV(obj: TTurtleComplex): TFhirResourceV;
begin
  result := ParseResource(obj);
end;

{ TFHIRTurtleComposerBase4 }

procedure TFHIRTurtleComposerBase4.ComposeResource(parent: TTurtleComplex; resource: TFhirResource);
begin
  raise exception.create('don''t use '+className+' directly - use TFHIRXmlComposer');
end;

procedure TFHIRTurtleComposerBase4.ComposeResourceV(parent: TTurtleComplex; resource: TFhirResourceV);
begin
  ComposeResource(parent, resource as TFhirResource);
end;

end.
