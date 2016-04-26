unit ServerUtilities;

interface

uses
  FHIRResources;

type
  TFHIRResourceConfig = record
    key: integer;
    Supported: Boolean;
    IdGuids: Boolean;
    IdClient: Boolean;
    IdServer: Boolean;
    cmdUpdate: Boolean;
    cmdDelete: Boolean;
    cmdValidate: Boolean;
    cmdHistoryInstance: Boolean;
    cmdHistoryType: Boolean;
    cmdSearch: Boolean;
    cmdCreate: Boolean;
    cmdOperation: Boolean;
    versionUpdates: Boolean;
  end;

  TConfigArray = Array [TFHIRResourceType] of TFHIRResourceConfig;



implementation

end.
