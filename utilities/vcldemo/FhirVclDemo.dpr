program FhirVclDemo;

uses
  FastMM4,
  Vcl.Forms,
  MainApplicationWindow in 'MainApplicationWindow.pas' {MainWindowForm},
  TZDB in '..\..\..\source\tzdb\dist\TZDB.pas',
  MarkdownHTMLEntities in '..\..\..\source\delphi-markdown\source\MarkdownHTMLEntities.pas',
  qrcodegen in '..\..\dependencies\qrcodegen\qrcodegen.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainWindowForm, MainWindowForm);
  Application.Run;
end.
