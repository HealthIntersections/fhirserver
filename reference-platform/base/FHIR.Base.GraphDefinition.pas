unit FHIR.Base.GraphDefinition;

interface

uses
  FHIR.Support.Base,
  FHIR.Base.Objects;

type
  TFHIRGraphDefinitionParser = class abstract (TFslObject)
  public
    function parseV(source : String) : TFhirResourceV; virtual; abstract;
    function asString(definition : TFhirResourceV; header : boolean) : String; virtual; abstract;
  end;

  TFHIRGraphDefinitionEngine = class (TFslObject)
  public

  end;


implementation

end.
