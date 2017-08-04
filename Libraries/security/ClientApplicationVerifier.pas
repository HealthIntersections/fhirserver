unit ClientApplicationVerifier;

interface

uses
  SysUtils,
  AdvObjects,
  JWT,
  FHIRTypes, FHIRResources, FHIRUtilities;

type
  TClientApplicationVerifier = class (TAdvObject)
  private
    FServer: String;
  public
    function Link : TClientApplicationVerifier; overload;
    property Server : String read FServer write FServer;

    function check(jwt : TJWT; params : TFHIRParameters) : String;
  end;

implementation

{ TClientApplicationVerifier }

function TClientApplicationVerifier.check(jwt : TJWT; params : TFHIRParameters) : String;
var
  p : TFhirParametersParameter;
begin
  if params <> nil then
  begin
    p := params.parameterList.Append;
    p.name := 'Message';
    p.value := TFHIRString.Create('Application Validation is not implemented yet');
  end;
  result := '<li>Application Validation is not implemented yet</li>';
end;


function TClientApplicationVerifier.Link: TClientApplicationVerifier;
begin
  result := TClientApplicationVerifier(inherited Link);
end;

end.
