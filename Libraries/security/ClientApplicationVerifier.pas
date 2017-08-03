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

    function check(jwt : TJWT; op : TFHIROperationOutcome) : String;
  end;

implementation

{ TClientApplicationVerifier }

function TClientApplicationVerifier.check(jwt: TJWT; op : TFHIROperationOutcome) : String;
var
 iss : TFHIROperationOutcomeIssue;
begin
  if (op <> nil) then
  begin
    iss := op.issueList.Append;
    iss.severity := IssueSeverityInformation;
    iss.code := IssueTypeInformational;
    iss.details := TFhirCodeableConcept.Create;
    iss.details.text := 'Application Validation is not implemented yet';
  end;
  result := '<li>Application Validation is not implemented yet</li>';
end;

function TClientApplicationVerifier.Link: TClientApplicationVerifier;
begin
  result := TClientApplicationVerifier(inherited Link);
end;

end.
