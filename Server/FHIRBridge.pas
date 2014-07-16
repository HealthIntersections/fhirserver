unit FHIRBridge;

interface

uses
  FHIRResources, FHIRComponents;

type
  {$IFDEF FHIR-DSTU}
  TFHIRProfileStructureHolder = TFHIRProfileStructure;
  {$ELSE}
  TFHIRProfileStructureHolder = TFhirProfileStructureSnapshot;
  TFHIRProfileStructureElement = TFhirProfileStructureSnapshotElement;
  TFhirProfileStructureElementList = TFhirProfileStructureSnapshotElementList;
  TFhirProfileStructureElementDefinitionBinding = TFhirProfileStructureSnapshotElementDefinitionBinding;
  {$ENDIF}


implementation

end.
