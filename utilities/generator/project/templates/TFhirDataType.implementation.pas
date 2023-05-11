function TFhirDataType.ToString : String;
begin
  result := gen(self);
end;

function TFhirDataType.isType : boolean;
begin
  result := true;
end;

