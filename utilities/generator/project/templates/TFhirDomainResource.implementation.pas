function TFhirDomainResource.hasExtensions: boolean;
begin
  result := (ExtensionList.Count > 0) or (FModifierExtensionList.Count > 0);
end;
