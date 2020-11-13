program FhirVclDemo;

uses
  FastMM4,
  Vcl.Forms,
  MainApplicationWindow in 'MainApplicationWindow.pas' {MainWindowForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainWindowForm, MainWindowForm);
  Application.Run;
end.
