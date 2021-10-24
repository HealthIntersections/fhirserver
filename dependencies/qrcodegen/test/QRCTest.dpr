program QRCTest;

uses
  Forms,
  uQRCTest in 'uQRCTest.pas' {Form1},
  qrcodegen in 'qrcodegen.pas',
  qrcodegen_demo in 'qrcodegen_demo.pas',
  uQRCode in 'uQRCode.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
