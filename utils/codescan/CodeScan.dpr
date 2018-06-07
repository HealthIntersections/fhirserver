program CodeScan;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  IOUtils,
  FHIR.Support.Binary in '..\..\reference-platform\support\FHIR.Support.Binary.pas',
  FHIR.Support.Collections in '..\..\reference-platform\support\FHIR.Support.Collections.pas',
  FHIR.Support.DateTime in '..\..\reference-platform\support\FHIR.Support.DateTime.pas',
  FHIR.Support.Exceptions in '..\..\reference-platform\support\FHIR.Support.Exceptions.pas',
  FHIR.Support.Generics in '..\..\reference-platform\support\FHIR.Support.Generics.pas',
  FHIR.Support.Math in '..\..\reference-platform\support\FHIR.Support.Math.pas',
  FHIR.Support.Objects in '..\..\reference-platform\support\FHIR.Support.Objects.pas',
  FHIR.Support.Strings in '..\..\reference-platform\support\FHIR.Support.Strings.pas',
  FHIR.Support.System in '..\..\reference-platform\support\FHIR.Support.System.pas',
  FHIR.Support.Text in '..\..\reference-platform\support\FHIR.Support.Text.pas',
  FHIR.Support.Fpc in '..\..\reference-platform\support\FHIR.Support.Fpc.pas',
  FHIR.Support.Stream in '..\..\reference-platform\support\FHIR.Support.Stream.pas',
  FHIR.Support.Decimal in '..\..\reference-platform\support\FHIR.Support.Decimal.pas';

function isExemptUnit(s : String) : boolean;
begin
  result := StringArrayExistsInsensitive([
      'FHIR.Support.Tarball',
      'NppForms',
      'NppDockingForms',
      'FHIR.Npp.Include',
      'FHIR.Npp.Form',
      'FHIR.Npp.DockingForm',
      'FHIR.Npp.Base',
      'ScintInt',
      'ScintEdit',
      'MZLib',
      'JPG',
      'GraphicStrings',
      'GraphicEx',
      'GraphicCompression',
      'GraphicColor',
      'DropURLTarget',
      'DropURLSource',
      'DropTarget',
      'DropSource',
      'DropPIDLTarget',
      'DropBMPTarget',
      'DropPIDLSource',
      'DropBMPSource',
      'FHIR.Java.JNI',
      'FHIR.Database.SQLite3.Wrapper',
      'FHIR.Database.SQLite3.Utilities',
      'FHIR.Database.SQLite3.Objects',
      'FHIR.Database.ODBC.Headers'
  ], s);
end;

function hasLicenseStatement(const src : String) : boolean;
begin
  result := (src.IndexOf('Copyright (c)') > 0) or (src.IndexOf('Copyright (C)') > 0);
end;

procedure scanPascalUnit(filename : String);
var
  u, src, srcns : String;
begin
  u := PathTitle(filename);
  if isExemptUnit(u) then
    exit;

  src := FileToString(filename, TEncoding.ASCII);
  srcns := src.Replace(#13, '').Replace(#10, '').Replace(#9, '').Replace(' ', '');
//  if not hasLicenseStatement(src) then
    // TFileLauncher.Open(filename);
//  if src.contains('raise Exception.') then
//    TFileLauncher.Open(filename);
//  if srcns.contains('= Class(Exception)') then
//    TFileLauncher.Open(filename);

end;

function isExemptFolder(s : String) : boolean;
begin
  result := StringArrayExistsInsensitive([
   'C:\work\fhirserver\Libraries\Indy10', 'C:\work\fhirserver\freepascal', 'C:\work\fhirserver\Libraries\treeview',
   'C:\work\fhirserver\Libraries\FMM', 'C:\work\fhirserver\reference-platform\dstu1'], s);
end;
procedure scanPascalUnits(folder : String);
var
  s : String;
begin
  for s in TDirectory.GetFiles(folder) do
    if s.EndsWith('.pas') then
      scanPascalUnit(s);
  for s in TDirectory.GetDirectories(folder) do
    if not isExemptFolder(s) then
      scanPascalUnits(s);
end;


begin
  try
    scanPascalUnits('c:\work\fhirserver');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
//  writeln('done. Press enter to exit');
//  readln;
end.
