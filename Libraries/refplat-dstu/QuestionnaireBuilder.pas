unit QuestionnaireBuilder;

interface

uses
  SysUtils, Classes,
  GUIDSupport,
  AdvObjects,
  FHIRResources, FHIRComponents, FHIRTypes, FHIRSupport, FHIRUtilities;

Type
  TGetValueSetExpansion = function(vs : TFHIRValueSet; ref : TFhirResourceReference; limit : integer; allowIncomplete : Boolean; dependencies : TStringList) : TFhirValueSet of object;
  TLookupCodeEvent = function(system, code : String) : String of object;
  TLookupReferenceEvent = function(Context : TFHIRRequest; uri : String) : TResourceWithReference of object;

  {
    defined for structural equivalence with dtsu2, but not functional
  }
  TQuestionnaireBuilder = class (TAdvObject)
  private
  public
  end;

implementation



end.
