program codescanner;

{$mode Delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, codescanform, codescanlogic
  { you can add units after this };

{$R *.res}

begin
  {$IFOPT D+}
!  WriteLn('Debugging is active');
  {$ELSE}
!  WriteLn('Debugging is not active');
  {$ENDIF}
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TCodeScannerForm, CodeScannerForm);
  Application.Run;
end.

