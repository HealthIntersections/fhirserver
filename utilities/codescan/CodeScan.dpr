program CodeScan;

{$APPTYPE CONSOLE}

{
Copyright (c) 2001-2013, Kestral Computing Pty Ltd (http://www.kestral.com.au)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of HL7 nor the names of its contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
}


{$R *.res}

uses
  System.SysUtils,
  IOUtils,
  fsl_collections in '..\..\library\support\fsl_collections.pas',
  fsl_base in '..\..\library\support\fsl_base.pas',
  fsl_utilities in '..\..\library\support\fsl_utilities.pas',
  fsl_fpc in '..\..\library\support\fsl_fpc.pas',
  fsl_shell in '..\..\library\support\fsl_shell.pas',
  fsl_stream in '..\..\library\support\fsl_stream.pas';

function isExemptUnit(s : String) : boolean;
begin
  result := StringArrayExistsInsensitive([
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
      'fdb_sqlite3_wrapper',
      'fdb_sqlite3_utilities',
      'fdb_sqlite3_objects',
      'fdb_odbc_headers'
  ], s);
end;

function hasLicenseStatement(const src : String) : boolean;
begin
  result := (src.IndexOf('Copyright (c)') > 0) or (src.IndexOf('Copyright (C)') > 0);
end;

procedure fixEolns(filename : String);
var
  src : String;
  b : TStringBuilder;
  changed : boolean;
  i : integer;
  ch : char;
begin
  src := FileToString(filename, TEncoding.ASCII);
  b := TStringBuilder.Create;
  try
    i := 1;
    while (i <= length(src)) do
    begin
      ch := src[i];
      if (ch = #13) then
      begin
        b.Append(#13#10);
        if (i = length(src)) or (src[i+1] <> #10) then
          changed := true
        else
          inc(i);
      end
      else if (ch = #10) then
      begin
        b.Append(#13#10);
        changed := true;
      end
      else
      begin
        b.Append(ch);
      end;
      inc(i);
    end;
    if (changed) then
      StringToFile(b.ToString, filename, TEncoding.ASCII);
  finally
    b.Free;
  end;


end;
function scanPascalUnit(filename : String) : boolean;
var
  u, src, srcns : String;
begin
  fixEolns(filename);
  result := true;
  u := PathTitle(filename);
  if isExemptUnit(u) then
    exit;

  src := FileToString(filename, TEncoding.ASCII);
  srcns := src.Replace(#13, '').Replace(#10, '').Replace(#9, '').Replace(' ', '');
  if not hasLicenseStatement(src) then
  begin
    // TFileLauncher.Open(filename);
    if not StringArrayExists(['FHIR.FMX.Ctrls', 'FHIRPackageRegister', 'FHIRStringEdit', 'FHIR.Ui.Chart', 'products', 'unit1', ''], u) then
    begin
      writeln('Unit '+filename+' has no license stmt');
      result := false;
    end;
  end;
  if src.contains('raise Exception.') then
  begin
//    TFileLauncher.Open(filename);
    if not StringArrayExists(['HelperTesterForm'], u) then
    begin
      writeln('Unit '+filename+' raises a unspecialised exception');
      result := false;
    end;
  end;
  if srcns.contains('= Class(Exception)') then
  begin
//    TFileLauncher.Open(filename);
    result := false;
    writeln('Unit '+filename+' subclasses Exception (should be (EFslException)');
  end;
  if not result then
    ExecuteLaunch('open', filename);
end;

function isExemptProject(u : String) : boolean;
begin
  result := false;
end;

function scanProjectFile(filename : String) : boolean;
var
  u, src, srcns : String;
begin
  result := true;
  u := PathTitle(filename);
  if isExemptProject(u) then
    exit;

  src := FileToString(filename, TEncoding.ASCII);
  srcns := src.Replace(#13, '').Replace(#10, '').Replace(#9, '').Replace(' ', '');
  if not src.Contains('<DCC_CONSTRUCTING_ABSTRACT>error</DCC_CONSTRUCTING_ABSTRACT>') then
  begin
    if not StringArrayExists(['dclFHIRPackage', 'rtFHIRPackage', 'FHIRComponents', 'FHIRComponentTest', 'UI_Creator', 'unit1', ''], u) then
    begin
      result := false;
      writeln('project '+filename+' allows constructing objects with abstract methods');
    end;
  end;
end;



function isExemptFolder(s : String) : boolean;
begin
  result := StringArrayExistsInsensitive([
   'C:\work\fhirserver\dependencies', 'C:\work\fhirserver\library\dstu1'], s);
end;

function scanPascalUnits(folder : String) : boolean;
var
  s : String;
begin
  result := true;
  for s in TDirectory.GetFiles(folder) do
    if s.EndsWith('.pas') then
      if not scanPascalUnit(s) then
        result := false;
  for s in TDirectory.GetDirectories(folder) do
    if not isExemptFolder(s) then
      if not scanPascalUnits(s) then
        result := false;
end;

function scanProjects(folder : String) : boolean;
var
  s : String;
begin
  result := true;
  for s in TDirectory.GetFiles(folder) do
    if s.EndsWith('.dproj') then
      if not scanProjectFile(s) then
        result := false;
  for s in TDirectory.GetDirectories(folder) do
    if not isExemptFolder(s) then
      if not scanProjects(s) then
        result := false;
end;


var
  ok : boolean;
begin
  try
    ok := scanPascalUnits('c:\work\fhirserver');
    ok := scanProjects('c:\work\fhirserver') and ok;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  writeln('done. Press enter to exit');
  readln;
  if ok then
    ExitCode := 1
  else
    ExitCode := 0;
end.
