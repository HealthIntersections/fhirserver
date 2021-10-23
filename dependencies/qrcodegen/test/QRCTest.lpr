program QRCTest;

{$MODE Delphi}

uses
  Forms, Interfaces,
  uQRCTest in 'uQRCTest.pas' {Form1},
  qrcodegen in 'qrcodegen.pas',
  qrcodegen_demo in 'qrcodegen_demo.pas';

{.$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
