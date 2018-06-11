program UITester;

uses
  Vcl.Forms,
  UITesterForm in '..\..\tests\UITesterForm.pas' {Form10},
  FHIR.Ui.Graph in '..\..\Libraries\ui\FHIR.Ui.Graph.pas',
  FHIR.Support.System in 'FHIR.Support.System.pas',
  FHIR.Support.Strings in 'FHIR.Support.Strings.pas',
  FHIR.Support.Objects in 'FHIR.Support.Objects.pas',
  FHIR.Support.Generics in 'FHIR.Support.Generics.pas',
  FHIR.Support.Exceptions in 'FHIR.Support.Exceptions.pas',
  FHIR.Ui.GraphDesigner in '..\..\Libraries\ui\FHIR.Ui.GraphDesigner.pas' {GraphDesignerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm10, Form10);
  Application.CreateForm(TGraphDesignerForm, GraphDesignerForm);
  Application.Run;
end.
