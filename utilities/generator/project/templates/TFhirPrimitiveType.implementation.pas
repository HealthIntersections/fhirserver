function TFhirPrimitiveType.GetStringValue : string;
begin
  if self = nil then
    result := ''
  else
    result := AsStringValue;
end;

function TFhirPrimitiveType.isPrimitive: boolean;
begin
  result := true;
end;

function TFhirPrimitiveType.hasPrimitiveValue: boolean;
begin
  result := StringValue <> '';
end;

function TFhirPrimitiveType.primitiveValue: string;
begin
  result := StringValue;
end;

function TFhirPrimitiveType.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'value') then
  begin
    StringValue := propValue.primitiveValue;
    propValue.Free;
    result := self;
  end
  else
    result := inherited setProperty(propName, propValue);
end;

function TFhirPrimitiveType.toString : String;
begin
  result := StringValue;
end;

