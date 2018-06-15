program UITesterFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UITesterFormFMX in 'UITesterFormFMX.pas' {Form10},
  FHIR.Ui.Graph in '..\Libraries\ui\FHIR.Ui.Graph.pas',
  FHIR.Support.Base in '..\reference-platform\support\FHIR.Support.Base.pas',
  FHIR.Support.Fpc in '..\reference-platform\support\FHIR.Support.Fpc.pas',
  FHIR.Support.Stream in '..\reference-platform\support\FHIR.Support.Stream.pas',
  FHIR.Support.Utilities in '..\reference-platform\support\FHIR.Support.Utilities.pas',
  FHIR.Support.Collections in '..\reference-platform\support\FHIR.Support.Collections.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm10, Form10);
  Application.Run;
end.
