Unit ServerValidator;

Interface

Uses
  SysUtils, Classes, ActiveX, ComObj,
  IdSoapXml, IdSoapMsXml, MsXmlParser, AltovaXMLLib_TLB,
  StringSupport,
  AdvObjects, AdvBuffers, AdvNameBuffers, AdvMemories, AdvVclStreams, AdvZipReaders, AdvZipParts,
  FHIRTypes, FHIRResources, FHIRValidator, FHIRParser, FHIRUtilities,
  TerminologyServer, ProfileManager;

Type
  TFHIRValidatorContext = class (TAdvObject)
  private
    FxmlApp: AltovaXMLLib_TLB.Application;
  end;


  TFHIRValidator = class (TValidatorServiceProvider)
  private
    FValidator : TFHIRInstanceValidator;
    FSchematronSource : String;
    FSources : TAdvNameBufferList;
    FCache : IXMLDOMSchemaCollection;
    FTerminologyServer : TTerminologyServer;
    FProfiles : TProfileManager;

    procedure Load(feed: TFHIRBundle);
    function LoadDoc(name : String; isFree : boolean = false) : IXMLDomDocument2;
    procedure processSchematron(op : TFhirOperationOutcome; source : String);
    procedure executeSchematron(context : TFHIRValidatorContext; op : TFhirOperationOutcome; source, name : String);
    procedure validateInstance(op : TFHIROperationOutcome; elem : TIdSoapXmlElement; specifiedprofile : TFHirStructureDefinition); overload;
    procedure SetProfiles(const Value: TProfileManager);
    procedure SetTerminologyServer(const Value: TTerminologyServer);

    function fetchResource(t : TFhirResourceType; url : String) : TFhirResource; override;
    function expand(vs : TFhirValueSet) : TFHIRValueSet; override;
    function supportsSystem(system : string) : boolean; override;
    function validateCode(system, code, display : String) : TValidationResult; override;
  public
    Constructor create;
    Destructor Destroy; Override;

    Property Validator : TFHIRInstanceValidator read FValidator;
    Property Profiles : TProfileManager read FProfiles write SetProfiles;
    Property TerminologyServer : TTerminologyServer read FTerminologyServer write SetTerminologyServer;

    procedure LoadFromDefinitions(filename : string);
    Property SchematronSource : String read FSchematronSource write FSchematronSource;

    function AcquireContext : TFHIRValidatorContext;
    procedure YieldContext(context : TFHIRValidatorContext);

    Function validateInstance(context : TFHIRValidatorContext; elem : TIdSoapXmlElement; opDesc : String; profile : TFHirStructureDefinition) : TFHIROperationOutcome; overload;
    Function validateInstance(context : TFHIRValidatorContext; resource : TFhirResource; opDesc : String; profile : TFHirStructureDefinition) : TFHIROperationOutcome; overload;
    Function validateInstance(context : TFHIRValidatorContext; source : TAdvBuffer; opDesc : String; profile : TFHirStructureDefinition) : TFHIROperationOutcome; overload;
  end;


Implementation

{ TFHIRValidator }

constructor TFHIRValidator.Create;
begin
  inherited;
  FSources := TAdvNameBufferList.create;
  FValidator := TFHIRInstanceValidator.create(self);
end;

destructor TFHIRValidator.Destroy;
begin
  FValidator.Free;
  FSources.Free;
  FTerminologyServer.Free;
  FProfiles.Free;
  inherited;
end;


procedure TFHIRValidator.LoadFromDefinitions(filename: string);
var
  b : TAdvBuffer;
  m : TAdvMemoryStream;
  r : TAdvZipReader;
  i : integer;
  mem : TAdvMemoryStream;
  vcl : TVclStream;
  xml : TFHIRXmlParser;
  v : Variant;
begin

  // read the zip, loading the resources we need
  b := TAdvBuffer.create;
  try
    b.LoadFromFileName(filename);
    m := TAdvMemoryStream.create;
    try
      m.Buffer := b.Link;
      r := TAdvZipReader.create;
      try
        r.Stream := m.Link;
        r.ReadZip;
        for i := 0 to r.Parts.count - 1 do
        begin
          if StringArrayExists(['.xsd', '.xsl', '.xslt', '.sch'], ExtractFileExt(r.Parts[i].Name)) then
            FSources.add(r.Parts[i].Link)
          else if ExtractFileExt(r.Parts[i].Name) = '.xml' then
          begin
            mem := TAdvMemoryStream.create;
            try
              mem.Buffer := r.Parts[i].Link;
              vcl := TVCLStream.create;
              try
                vcl.Stream := mem.link;
                xml := TFHIRXmlParser.create('en');
                try
                  xml.source := vcl;
                  xml.Parse;
                  Load(xml.resource as TFhirBundle);
                finally
                  xml.free;
                end;
              finally
                vcl.free;
              end;
            finally
              mem.free;
            end;
          end;
        end;
      finally
        r.free;
      end;
    finally
      m.free;
    end;
  finally
    b.free;
  end;

  // prep the schemas
  v := CreateOLEObject(GMsXmlProgId_SCHEMA);
  FCache := IUnknown(TVarData(v).VDispatch) as IXMLDOMSchemaCollection;
  FCache.add('http://www.w3.org/XML/1998/namespace', loadDoc('xml.xsd'));
  FCache.add('http://www.w3.org/1999/xhtml', loadDoc('fhir-xhtml.xsd'));
  FCache.add('http://www.w3.org/2000/09/xmldsig#', loadDoc('xmldsig-core-schema.xsd'));
  FCache.add('http://hl7.org/fhir', loadDoc('fhir-single.xsd'));
end;

procedure TFHIRValidator.Load(feed: TFHIRBundle);
var
  i : integer;
  r : TFhirResource;
  p : TFHirStructureDefinition;
begin
  for i := 0 to feed.entryList.count - 1 do
  begin
    r := feed.entryList[i].resource;
    if r is TFHirStructureDefinition then
    begin
      p := r as TFHirStructureDefinition;
      if FProfiles <> nil then
        FProfiles.SeeProfile(0, p);
    end
    else if (r.ResourceType in [frtValueSet, frtConceptMap]) then
      FTerminologyServer.SeeSpecificationResource(r);
  end;
end;

function TrimBof(const s : String):String;
begin
  result := s;
  while (result[1] <> '<') do
    delete(result, 1, 1);
end;

function TFHIRValidator.LoadDoc(name : String; isFree : boolean) : IXMLDomDocument2;
Var
  LVariant: Variant;
  buf : TAdvNameBuffer;
Begin
  buf := FSources.GetByName(name);
  LVariant := LoadMsXMLDomV(isfree);
  Result := IUnknown(TVarData(LVariant).VDispatch) as IXMLDomDocument2;
  result.async := false;
  if isFree then
    result.resolveExternals := true;
  if not result.loadXML(TrimBof(buf.AsUnicode)) then
    raise Exception.create('unable to parse XML because '+result.parseError.reason);
end;

function TFHIRValidator.AcquireContext: TFHIRValidatorContext;
begin
  result := TFHIRValidatorContext.create;
  try
    result.FxmlApp := AltovaXMLLib_TLB.CoApplication.Create;
    result.link;
  finally
    result.free;
  end;
end;

procedure TFHIRValidator.YieldContext(context: TFHIRValidatorContext);
begin
  try
    context.FxmlApp := nil;
  finally
    context.free;
  end;
end;


function TFHIRValidator.validateInstance(context : TFHIRValidatorContext; source: TAdvBuffer; opDesc : String; profile : TFHirStructureDefinition): TFHIROperationOutcome;
var
  dom : TIdSoapMSXmlDom;
  procedure load;
  var
    mem : TAdvMemoryStream;
    vcl : TVCLStream;
  begin
    mem := TAdvMemoryStream.create;
    try
      mem.Buffer := source.Link;
      vcl := TVCLStream.create;
      try
        vcl.Stream := mem.Link;
        dom.Read(vcl);
      finally
        vcl.free;
      end;
    finally
      mem.free;
    end;
  end;
begin
  result := TFhirOperationOutcome.create;
  try
    // 1: load with schema validation

    dom := TIdSoapMSXmlDom.create;
    try
      dom.Schemas := FCache;
      load;
      if dom.Root = nil then
      begin
        result.error('Schema', IssueTypeInvalid, 'line '+inttostr(dom.ParseError.line)+', Col '+inttostr(dom.ParseError.linepos), false, dom.ParseError.reason);
        dom.schemas := nil;
        try
          load;
        except
          result.issueList[0].severity := IssueSeverityFatal;
        end;
      end;

      if dom.Root <> nil then
      begin
        // well, we can actually load XML. try the schematrons
        executeSchematron(context, result, source.AsUnicode, lowercase(dom.Root.Name)+'.sch');
        validateInstance(result, dom.root, profile);
      end;
    finally
      dom.free;
    end;
    BuildNarrative(result, opDesc);
    result.Link;
  finally
    result.free;
  end;
end;

procedure TFHIRValidator.validateInstance(op: TFHIROperationOutcome; elem: TIdSoapXmlElement; specifiedprofile : TFHirStructureDefinition);
begin
  FValidator.validate(op.issueList, elem, specifiedprofile);
end;

Function TFHIRValidator.validateInstance(context : TFHIRValidatorContext; elem : TIdSoapXmlElement; opDesc : String; profile : TFHirStructureDefinition) : TFHIROperationOutcome;
begin
  result := TFhirOperationOutcome.create;
  try
    validateInstance(result, elem, profile);
    BuildNarrative(result, opDesc);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRValidator.validateInstance(context: TFHIRValidatorContext; resource: TFhirResource; opDesc: String; profile: TFHirStructureDefinition): TFHIROperationOutcome;
var
  stream : TBytesStream;
  xml : TFHIRXmlComposer;
  buf : TAdvBuffer;
begin
  stream := TBytesStream.Create(nil);
  try
    xml := TFHIRXmlComposer.Create('en');
    try
      xml.Compose(stream, resource, true, nil);
    finally
      xml.Free;
    end;
    buf := TAdvBuffer.Create;
    try
      buf.AsBytes := copy(stream.Bytes, 0, stream.Size);
      result := validateInstance(context, buf, opDesc, profile);
    finally
      buf.Free;
    end;
  finally
    stream.Free;
  end;
end;


procedure TFHIRValidator.processSchematron(op : TFhirOperationOutcome; source : String);
var
  e : IXMLDOMElement;
  issue : TFhirOperationOutcomeIssue;
  ex : TFhirExtension;
  v : Variant;
  doc : IXMLDOMDocument2;
Begin
  v := LoadMsXMLDomV(false);
  doc := IUnknown(TVarData(v).VDispatch) as IXMLDomDocument2;
  doc.async := false;
  if not doc.loadXML(source) then
    raise Exception.create('unable to parse schematron results because '+doc.parseError.reason);

  e := TMsXmlParser.FirstChild(doc.DocumentElement);
  while (e <> nil) do
  begin
    if e.baseName = 'failed-assert' then
    begin
      issue := TFhirOperationOutcomeIssue.create;
      try
        issue.severity := IssueSeverityError;
        issue.code := IssueTypeInvariant;
        issue.details := TFhirCodeableConcept.Create;
        issue.details.text := e.text;
        issue.locationList.Append.value := e.getAttribute('location');
        ex := issue.ExtensionList.Append;
        ex.url := 'http://hl7.org/fhir/tools#issue-source';
        ex.value := TFhirCode.create;
        TFhirCode(ex.value).value := 'Schematron';
        op.issueList.add(issue.link);
      finally
        issue.free;
      end;
    end;
    e := TMsXmlParser.NextSibling(e);
  end;
end;

procedure TFHIRValidator.SetProfiles(const Value: TProfileManager);
begin
  FProfiles.Free;
  FProfiles := Value;
end;

procedure TFHIRValidator.SetTerminologyServer(const Value: TTerminologyServer);
begin
  FTerminologyServer.Free;
  FTerminologyServer := Value;
end;

procedure TFHIRValidator.executeSchematron(context : TFHIRValidatorContext; op : TFhirOperationOutcome; source, name: String);
var
  xslt2: AltovaXMLLib_TLB.XSLT2;
  src : String;
  app : AltovaXMLLib_TLB.Application;
begin
  if context <> nil then
    app := context.FxmlApp
  else
    app := AltovaXMLLib_TLB.CoApplication.Create;

  xslt2 := App.XSLT2;
  src := FSources.GetByName(name).AsUnicode;
  xslt2.InputXMLFromText := src;
  xslt2.XSLFileName := IncludeTrailingPathDelimiter(FSchematronSource)+'iso_svrl_for_xslt2.xsl';
  src := xslt2.ExecuteAndGetResultAsString;
  xslt2 := App.XSLT2;
  xslt2.InputXMLFromText := source;
  xslt2.XSLFromText := src;
  processSchematron(op, xslt2.ExecuteAndGetResultAsString);
end;

function TFHIRValidator.fetchResource(t : TFhirResourceType; url : String) : TFhirResource;
begin
  case t of
    frtValueSet : result := FTerminologyServer.getValueSetByUrl(url);
    frtStructureDefinition :
      result := FProfiles.ProfileByURL[url];
  else
    result := nil;
  end;
end;

function TFHIRValidator.expand(vs : TFhirValueSet) : TFHIRValueSet;
begin
  result := FTerminologyServer.expandVS(vs, '', '', '', 0, true);
end;

function TFHIRValidator.supportsSystem(system : string) : boolean;
begin
  result := FTerminologyServer.supportsSystem(system);
end;

function TFHIRValidator.validateCode(system, code, display : String) : TValidationResult;
var
  op : TFHIROperationOutcome;
begin
  op := TFHIROperationOutcome.create;
  try
    result := TValidationResult.Create;
    try
      if FTerminologyServer.checkCode(op, '', system, code, display) then
        result.Severity := IssueSeverityNull
      else if op.issueList.Count = 1 then
      begin
        result.Severity := op.issueList[0].severity;
        result.Message := op.issueList[0].details.text;
      end
      else
      begin
        result.Severity := IssueSeverityError;
        result.Message := '??';
      end;
    finally
      result.Free;
    end;
  finally
    op.Free;
  end;
end;



end.

