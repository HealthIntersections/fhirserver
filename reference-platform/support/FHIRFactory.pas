unit FHIRFactory;

interface

uses
  SysUtils, Classes,
  StringSupport, DateSupport,
  AdvBuffers,
  FHIRBase, FHIRTypes, FHIRResources, FHIRParser, FHIRContext, FHIRSupport,
  FHIRIndexBase, FHIRXHtml;

type
  {@Class TFHIRFactory
    Creates FHIR types

    * new*: create a type with no values
    * make*: useful helper routines that take parameters and populate the type accordingly (mostly for data types)
  }
  {!.Net HL7Connect.Fhir.Factory}
  TFHIRFactory = class (TFhirResourceFactory)
  private
    FContext : TFHIRWorkerContext;
    procedure SetContext(const Value: TFHIRWorkerContext);
  public
    Constructor Create(context : TFHIRWorkerContext);
    Destructor Destroy; override;

    function link : TFHIRFactory; overload;

    property Context : TFHIRWorkerContext read FContext write SetContext;

    {@member makeAttachmentFromFile
      make a new Attachment, and load the contents from the file. The mime type will be filled out based on the systems interpretation of the file extension
    }
    {!script nolink}
    function makeAttachmentFromFile(filename : String) : TFhirAttachment;

    {@member makeAttachmentFromStream
      make a new Attachment From the stream
    }
    {!script nolink}
    function makeAttachmentFromStream(mimeType : String; stream : TStream) : TFhirAttachment;

    {@member makeAttachmentFromUrl
      make a new Attachment that references a url. The data will be copied into the attachment if specified.

      The URL can't be password protected etc.
    }
    {!script nolink}
    function makeAttachmentFromUrl(url : String; mimeType : String; inlineData : Boolean) : TFhirAttachment;

    {@member makeIdentifier
      make a new Identifier and use the provided parameters
    }
    {!script nolink}
    function makeIdentifier(system, key : String) : TFhirIdentifier;

    {@member makeIdentifierWithUse
      make a new Identifier and use the provided parameters
    }
    {!script nolink}
    function makeIdentifierWithUse(system, key, use, label_ : String) : TFhirIdentifier;

    {@member makeCodeableConcept
      make a new CodeableConcept and use the provided parameters
    }
    {!script nolink}
    function makeCodeableConcept(coding : TFhirCoding; text : String) : TFhirCodeableConcept;

    {@member makeCoding
      make a new Coding and use the provided parameters
    }
    {!script nolink}
    function makeCoding(system : String; code : string; display : String) : TFhirCoding;

    {@member makeQuantity
      make a new Quantity and use the provided parameters
    }
    {!script nolink}
    function makeQuantity(value, units : String):TFhirQuantity;

    {@member makeQuantityCoded
      make a new QuantityCoded and use the provided parameters
    }
    {!script nolink}
    function makeQuantityCoded(value, units, system, code : String):TFhirQuantity;

    {@member makeIntervalQuantity
      make a new IntervalQuantity and use the provided parameters
    }
    {!script nolink}
    function makeRange(low, high : TFhirQuantity):TFhirRange;

    {@member makeIntervalDateTime
      make a new IntervalDateTime and use the provided parameters
    }
    {!script nolink}
    function makePeriod(low, high : string):TFhirPeriod;

    {@member makeIdentifierWithLabel
      make a new HumanId and use the provided parameters
    }
    {!script nolink}
    function makeIdentifierWithLabel(use : string; label_, idSystem, id : String):TFhirIdentifier;

    {@member makeHumanName
      make a new HumanName and use the provided parameters
    }
    {!script nolink}
    function makeHumanName(use, family, given, prefix, suffix : String):TFhirHumanName;

    {@member makeHumanNameText
      make a new HumanName and fill out the text value
    }
    {!script nolink}
    function makeHumanNameText(use, text : String):TFhirHumanName;

    {@member makeAddress
      make a new HumanAddress and use the provided parameters
    }
    {!script nolink}
    function makeAddress(use, street, city, state, postalCode, country : String):TFhirAddress;

    {@member makeAddressText
      make a new HumanAddress and use the provided parameters
    }
    {!script nolink}
    function makeAddressText(use, text : String):TFhirAddress;

    {@member makeContactPoint
      make a new Contact and use the provided parameters
    }
    {!script nolink}
    function makeContactPoint(system, value, use : String):TFhirContactPoint;

    {@member makeReference
      make a new resource reference and use the provided parameters
    }
    {!script nolink}
    function makeReference(id : String) : TFhirReference;

    {@member makeReferenceText
      make a new resource reference and fill out the display only
    }
    {!script nolink}
    function makeReferenceText(s : String) : TFhirReference;

    {@member makeExtension
      make a new narrative with the provided status and html
    }
    {!script nolink}
    function makeExtension(url : String; value : TFhirType) : TFhirExtension;

    {@member makeNarrative
      make a new narrative with the provided status and html
    }
    {!script nolink}
    function makeNarrative(status, html : String; policy : TFHIRXhtmlParserPolicy) : TFhirNarrative;

    {@member makeBinary
      make a new Binary resource
    }
    {!script nolink}
    function makeBinary : TFhirBinary;

    {@member makeBinary
      make a new Binary resource
    }
    {!script nolink}
    function makeBinaryContent(source : TAdvBuffer; mimeType : String) : TFhirBinary;

    {@member makeRequest
      make a new Fhir request (for a conversion parameter)
    }
    {!script nolink}
    function makeRequest(origin : TFHIRRequestOrigin; compartmentInformation : TFHIRCompartmentList) : TFhirRequest;

    {@member makeRequest
      make a new OperationOutcome that claims success
    }
    {!script nolink}
    function makeSuccessfulOperation : TFhirOperationOutcome;

    function newXmlParser(lang : String) : TFHIRXmlParser;
    function newJsonParser(lang : String) : TFHIRJsonParser;
    function newXmlComposer(lang : String; style : TFHIROutputStyle = OutputStyleNormal) : TFHIRXmlComposer;
    function newJsonComposer(lang : String; style : TFHIROutputStyle = OutputStyleNormal) : TFHIRJsonComposer;

    {$IFNDEF FHIR2}
    function newTurtleParser(lang : String) : TFHIRTurtleParser;
    function newTurtleComposer(lang : String; style : TFHIROutputStyle = OutputStyleNormal) : TFHIRTurtleComposer;
    {$ENDIF}
  end;

implementation

{ TFHIRFactory }

function CheckEnum(Systems, Names: array of String; value : String): TFhirEnum;
begin
  if value = '' then
    result := nil
  else if StringIsInteger32(value) then
    result := TFhirEnum.create(Systems[StrToInt(value)], Names[StrToInt(value)])
  else if StringArrayIndexOfSensitive(Names, value) > -1 then
    result := TFhirEnum.create(Systems[StringArrayIndexOfSensitive(Names, value)], value)
  else
    Raise Exception.create('Invalid enumeration value "'+value+'"');
end;

constructor TFHIRFactory.Create(context: TFHIRWorkerContext);
begin
  Inherited Create;
  FContext := context;
end;


destructor TFHIRFactory.Destroy;
begin
  FContext.Free;
  inherited;
end;

function TFHIRFactory.link: TFHIRFactory;
begin
  result := TFHIRFactory(inherited Link);
end;

function TFHIRFactory.newJsonComposer(lang : String; style : TFHIROutputStyle): TFHIRJsonComposer;
begin
  result := TFHIRJsonComposer.Create(context.link, style, 'en');
end;

function TFHIRFactory.newJsonParser(lang : String) : TFHIRJsonParser;
begin
  result := TFHIRJsonParser.Create(context.link, 'en');
end;

{$IFNDEF FHIR2}
function TFHIRFactory.newTurtleComposer(lang : String; style : TFHIROutputStyle): TFHIRTurtleComposer;
begin
  result := TFHIRTurtleComposer.Create(context.link, style, 'en');
end;

function TFHIRFactory.newTurtleParser(lang : String) : TFHIRTurtleParser;
begin
  result := TFHIRTurtleParser.Create(context.link, 'en');
end;
{$ENDIF}

function TFHIRFactory.newXmlComposer(lang : String; style : TFHIROutputStyle): TFHIRXmlComposer;
begin
  result := TFHIRXmlComposer.Create(context.link, style, 'en');
end;

function TFHIRFactory.newXmlParser(lang : String) : TFHIRXmlParser;
begin
  result := TFHIRXmlParser.Create(context.link, 'en');
end;

procedure TFHIRFactory.SetContext(const Value: TFHIRWorkerContext);
begin
  FContext.Free;
  FContext := Value;
end;

function TFHIRFactory.makeAttachmentFromFile(filename: String): TFhirAttachment;
begin
  raise Exception.Create('to do');
{  result := TFhirAttachment.create;
  try
    result.data := TFhirBase64Binary.create;
    result.data.value := BinToBase64(FileToString(filename));
    result.contentType := TFhirCode.Create(GetMIMETypeFromFile(filename));
    result.link;
  finally
    result.free;
  end;}
end;

function TFHIRFactory.makeAttachmentFromStream(mimeType: String; stream: TStream): TFhirAttachment;
begin
  raise Exception.Create('to do');
{  result := TFhirAttachment.create;
  try
    result.data := TFhirBase64Binary.create;
    result.data.value := BinToBase64(StreamToString(stream));
    result.contentType := TFhirCode.create(mimeType);
    result.link;
  finally
    result.free;
  end;}
end;

function TFHIRFactory.makeAttachmentFromUrl(url, mimeType: String; inlineData: Boolean): TFhirAttachment;
begin
  raise Exception.Create('to do');
{  result := nil;
  try
    todo;
    result.link;
  finally
    result.free;
  end;}
end;

function TFHIRFactory.makeCodeableConcept(coding: TFhirCoding; text: String): TFhirCodeableConcept;
begin
  result := TFhirCodeableConcept.create;
  try
    result.codingList.add(coding);
    result.text := text;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactory.makeCoding(system, code, display: String): TFhirCoding;
begin
  result := TFhirCoding.create;
  try
    result.code := code;
    result.system := system;
    result.display := display;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactory.makeContactPoint(system, value, use: String): TFhirContactPoint;
begin
  result := TFhirContactPoint.create;
  try
    result.useElement := CheckEnum(SYSTEMS_TFhirContactPointUseEnum, CODES_TFhirContactPointUseEnum, use);
    result.systemElement := CheckEnum(SYSTEMS_TFhirContactPointSystemEnum, CODES_TFhirContactPointSystemEnum, system);
    result.value := value;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactory.makeAddress(use, street, city, state, postalCode, country : String): TFhirAddress;
begin
  result := TFhirAddress.create;
  try
    result.useElement := CheckEnum(SYSTEMS_TFhirAddressUseEnum, CODES_TFhirAddressUseEnum, use);
    result.lineList.AddItem(street);
    result.city := city;
    result.state := state;
    result.postalCode := postalCode;
    result.country := country;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactory.makeIdentifierWithLabel(use : string; label_, idSystem, id: String): TFhirIdentifier;
begin
  result := TFhirIdentifier.create;
  try
    result.useElement := CheckEnum(SYSTEMS_TFhirIdentifierUseEnum, CODES_TFhirIdentifierUseEnum, use);
    result.type_ := TFhirCodeableConcept.Create;
    result.type_.text := label_;
    result.system := idsystem;
    result.value := id;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactory.makeHumanName(use, family, given, prefix, suffix: String): TFhirHumanName;
begin
  result := TFhirHumanName.create;
  try
    result.useElement := CheckEnum(SYSTEMS_TFhirNameUseEnum, CODES_TFhirNameUseEnum, use);
    {$IFDEF FHIR2}
    result.familyList.addItem(family);
    {$ELSE}
    result.family := family;
    {$ENDIF}
    result.givenList.addItem(given);
    if (prefix <> '') then
      result.prefixList.addItem(prefix);
    if (suffix <> '') then
      result.suffixList.addItem(suffix);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactory.makeIdentifierWithUse(system, key, use, label_ : String) : TFhirIdentifier;
begin
  result := TFhirIdentifier.create;
  try
    result.system := system;
    result.value := key;
    result.type_ := TFhirCodeableConcept.Create;
    result.type_.text := label_;
    result.useElement := CheckEnum(SYSTEMS_TFhirIdentifierUseEnum, CODES_TFhirIdentifierUseEnum, use);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactory.makeIdentifier(system, key: String): TFhirIdentifier;
begin
  result := TFhirIdentifier.create;
  try
    result.system := system;
    result.value := key;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactory.makePeriod(low, high: string): TFhirPeriod;
begin
  result := TFhirPeriod.create;
  try
    if low <> '' then
      result.start := TDateTimeEx.fromXml(low);
    if high <> '' then
      result.end_ := TDateTimeEx.fromXml(high);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactory.makeRange(low, high: TFhirQuantity): TFhirRange;
begin
  result := TFhirRange.create;
  try
    result.low := low.Link;
    result.high := high.Link;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactory.makeQuantity(value, units: String): TFhirQuantity;
begin
  result := TFhirQuantity.create;
  try
    result.value := value;
    result.unit_ := units;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactory.makeQuantityCoded(value, units, system, code: String): TFhirQuantity;
begin
  result := TFhirQuantity.create;
  try
    result.value := value;
    result.unit_ := units;
    result.system := system;
    result.code := code;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactory.makeBinary: TFhirBinary;
begin
  result := TFhirBinary.create;
end;

function TFHIRFactory.makeBinaryContent(source: TAdvBuffer; mimeType: String): TFhirBinary;
begin
  result := makeBinary;
  result.Content := source.AsBytes;
  result.ContentType := mimeType;
end;

function TFHIRFactory.makeReference(id: String): TFhirReference;
begin
  result := TFhirReference.create;
  try
    result.reference := id;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactory.makeNarrative(status, html: String; policy : TFHIRXhtmlParserPolicy): TFhirNarrative;
begin
  result := TFhirNarrative.create;
  try
    result.statusElement := CheckEnum(SYSTEMS_TFhirNarrativeStatusEnum, CODES_TFhirNarrativeStatusEnum, status);
    result.div_ := TFHIRXhtmlParser.Parse('en', policy, [], html);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactory.makeReferenceText(s: String): TFhirReference;
begin
  result := TFhirReference.create;
  try
    result.display := s;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactory.makeRequest(origin : TFHIRRequestOrigin; compartmentInformation : TFHIRCompartmentList): TFhirRequest;
begin
  result := TFhirRequest.create(FContext.link, origin, compartmentInformation);
end;

function TFHIRFactory.makeSuccessfulOperation: TFhirOperationOutcome;
begin
  result := TFhirOperationOutcome.create;
  try
    result.text := makeNarrative('generated', '<div>The operation was successful</div>', xppReject);
    result.link;
  finally
    result.Free;
  end;
end;

function TFHIRFactory.makeHumanNameText(use, text: String): TFhirHumanName;
begin
  result := TFhirHumanName.create;
  try
    result.useElement := CheckEnum(SYSTEMS_TFhirNameUseEnum, CODES_TFhirNameUseEnum, use);
    result.text := text;
    result.link;
  finally
    result.free;
  end;

end;

function TFHIRFactory.makeAddressText(use, text: String): TFhirAddress;
begin
  result := TFhirAddress.create;
  try
    result.useElement := CheckEnum(SYSTEMS_TFhirAddressUseEnum, CODES_TFhirAddressUseEnum, use);
    result.text := text;
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRFactory.makeExtension(url: String; value: TFhirType): TFhirExtension;
begin
  result := TFhirExtension.create;
  try
    result.url := url;
    result.value := value.Link;
    result.link;
  finally
    result.free;
  end;
end;

end.
