program CodeScan;

{$APPTYPE CONSOLE}

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  Classes,
  IOUtils,
  fsl_collections in '..\..\library\fsl\fsl_collections.pas',
  fsl_base in '..\..\library\fsl\fsl_base.pas',
  fsl_utilities in '..\..\library\fsl\fsl_utilities.pas',
  fsl_fpc in '..\..\library\fsl\fsl_fpc.pas',
  fsl_shell in '..\..\library\fsl\fsl_shell.pas',
  fsl_stream in '..\..\library\fsl\fsl_stream.pas';

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

//procedure fixCollections(filename : String);
//var
//  ts : TStringList;
//  s, t : string;
//  i : integer;
//  del, ins, cl, ct, inAddItem, ovr : boolean;
//begin
//  ts := TStringList.create;
//  try
//    ts.LoadFromFile(filename);
//    i := 0;
//    ct := false;
//    inAddItem := false;
//    while i < ts.count do
//    begin
//      s := ts[i];
//      cl := false;
//      ins := false;
//      del := false;
//
//      if s.trim.ToLower.StartsWith('procedure additem(value : t') then
//      begin
//        cl := true;
//        if s.TrimRight.ToLower.EndsWith('overload;') then
//        begin
//          s := s.TrimRight;
//          s := s.Substring(0, s.Length-9).trim;
//        end;
//
//        t := s.Substring(s.IndexOf(':')+1).trim;
//        t := t.Substring(0, t.Length-2);
//        s := (s.Substring(0, length(s)-1)+': '+t+';').Replace('procedure ', 'function ')+' overload;';
//      end;
//      if s.StartsWith('  assert(value.ClassName = ') then
//        del := true;
//      if s.toLower.trim.StartsWith('procedure t') and s.toLower.contains('.additem(') then
//      begin
//        t := s.Substring(s.IndexOf(':')+1).trim;
//        t := t.Substring(0, t.Length-2);
//        if t.StartsWith('T') then
//        begin
//          cl := true;
//          inAddItem := true;
//          s := (s.Substring(0, length(s)-1)+': '+t+';').Replace('procedure ', 'function ').Replace('Procedure ', 'function ');
//        end;
//      end;
//      if (s.Trim.ToLower = 'end;') and (inAddItem) then
//      begin
//        ts.Insert(i, '  result := value;');
//        ins := true;
//        inAddItem := false;
//      end;
//      if inAddItem and (s.Trim.ToLower = 'add(value.link);') then
//      begin
//        cl := true;
//        s := '  add(value);';
//      end;
//      if (del) then
//      begin
//        ts.Delete(i);
//        ct := true;
//      end
//      else if (cl) then
//      begin
//        assert(not ins);
//        ts[i] := s;
//        ct := true;
//      end;
//      inc(i);
//    end;
//    if (ct) then
//      ts.SaveToFile(filename);
//  finally
//    ts.free;
//  end;
//end;
//
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
    if not StringArrayExists(['FHIR.FMX.Ctrls', 'FHIRPackageRegister', 'FHIRStringEdit', 'FHIR.Ui.Chart', 'products', 'unit1', 'fdb_odbc_fpc', 'fsl_diff', 'fsl_java_jni', 'fsl_scrypt', 'fui_gfx', 'fui_gfx_color', 'fui_gfx_compression', 'fui_gfx_jpg'], u) then
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
   'C:\work\fhirserver\dependencies',  'C:\work\fhirserver\server\js', 'c:\work\fhirserver\utilities\generator', 'c:\work\fhirserver\packages'], s) or s.Contains('backup');
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
