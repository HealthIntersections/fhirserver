program UITesterFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UITesterFormFMX in 'UITesterFormFMX.pas' {Form10},
  FHIR.Ui.Graph in '..\library\ui\FHIR.Ui.Graph.pas',
  fsl_base in '..\library\support\fsl_base.pas',
  fsl_fpc in '..\library\support\fsl_fpc.pas',
  fsl_stream in '..\library\support\fsl_stream.pas',
  fsl_utilities in '..\library\support\fsl_utilities.pas',
  fsl_collections in '..\library\support\fsl_collections.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm10, Form10);
  Application.Run;
end.
