program UITesterFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UITesterFormFMX in 'UITesterFormFMX.pas' {Form10},
  FHIR.Ui.Graph in '..\Libraries\ui\FHIR.Ui.Graph.pas',
  FHIR.Support.Objects in '..\reference-platform\support\FHIR.Support.Objects.pas',
  FHIR.Support.Binary in '..\reference-platform\support\FHIR.Support.Binary.pas',
  FHIR.Support.DateTime in '..\reference-platform\support\FHIR.Support.DateTime.pas',
  FHIR.Support.Decimal in '..\reference-platform\support\FHIR.Support.Decimal.pas',
  FHIR.Support.Exceptions in '..\reference-platform\support\FHIR.Support.Exceptions.pas',
  FHIR.Support.Fpc in '..\reference-platform\support\FHIR.Support.Fpc.pas',
  FHIR.Support.Generics in '..\reference-platform\support\FHIR.Support.Generics.pas',
  FHIR.Support.Math in '..\reference-platform\support\FHIR.Support.Math.pas',
  FHIR.Support.Stream in '..\reference-platform\support\FHIR.Support.Stream.pas',
  FHIR.Support.Strings in '..\reference-platform\support\FHIR.Support.Strings.pas',
  FHIR.Support.System in '..\reference-platform\support\FHIR.Support.System.pas',
  FHIR.Support.Text in '..\reference-platform\support\FHIR.Support.Text.pas',
  FHIR.Support.Collections in '..\reference-platform\support\FHIR.Support.Collections.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm10, Form10);
  Application.Run;
end.
