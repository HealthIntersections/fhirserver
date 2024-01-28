function TFhirElement.noExtensions: TFhirElement;
begin
  DisallowExtensions := true;
  result := self;
end;


procedure TFhirElement.addExtension(url: String; value: TFHIRObject);
var
  ex : TFhirExtension;
begin
  ex := extensionList.Append;
  ex.url := url;
  ex.value := value as TFhirDataType;
end;

function TFhirElement.extensionCount(url: String): integer;
var
  ex : TFhirExtension;
begin
  result := 0;
  for ex in ExtensionList do
    if (ex.url = url) or (url = '') then
      inc(result);
end;
      
function TFhirElement.extensions(url: String): TFslList<TFHIRObject>;
var
  ex : TFhirExtension;
begin
  result := TFslList<TFHIRObject>.Create;
  try
    for ex in ExtensionList do
      if ex.url = url then
        result.Add(ex.Link);
    result.link;
  finally
    result.free;
  end;
end;

function TFhirElement.hasExtension(url: string): boolean;
var
  ex : TFhirExtension;
begin
  result := false;
  for ex in ExtensionList do
    if ex.url = url then
      exit(true);
end;
      
function TFhirElement.hasExtensions: boolean;
begin
  result := (FExtensionList <> nil) and (FextensionList.Count > 0);
end;

function TFhirElement.getExtensionString(url: String): String;
var
  ex : TFhirExtension;
begin
  result := '';
  for ex in ExtensionList do
  begin
    if ex.url = url then
    begin
      if not ex.value.isPrimitive then
        raise EFHIRException.Create('Complex extension '+url)
      else if result <> '' then
        raise EFHIRException.Create('Duplicate extension '+url)
      else
        result := ex.value.primitiveValue;
    end;
  end;
end;
