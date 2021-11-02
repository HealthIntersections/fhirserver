program codescan;

{this programs run as part of the ci-build to enforce coding standards
on the code. THe following rules are enforced:

* code can't have any unicode bi-di control characters in it
   file extensions: .pas, .lpr, .inc, .html, .css,
   (including in all the source packages too)
* all .pas files must have a license statement in a comment at the head
   (except for in /dependencies
* never raise Exception directly - always a subclass
   (and all exceptions should subclass EFslException outside /dependencies
* check .pas line endings are all crlf (make them so)
*

for now, the program doesn't apply code formatting to the code.
That might be reviewed if a working code formatter is found in the
future

The program takes one parameter, which is the name of the root folder that contains the source directory.
It assumes that the root of the FHIRServer repository is two folders up from the executable

}

{$i fhir.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Interfaces,
  fsl_utilities, fsl_fpc, fsl_stream, fsl_unicode;


type

  { TCodeScanner }
  TSourceScanCheck = (sscUnicode, sscLicense, sscExceptionRaise, sscExceptionDefine, sscLineEndings);
  TSourceScanCheckSet = set of TSourceScanCheck;

  TCodeScanner = class(TObject)
  private
    FAllOk : boolean;
    FSourceDir : String;
    FProjectDir : String;

    procedure reportError(filename : String; line : integer; msg : String);

    procedure checkFileForUnicode(filename : String);
    procedure checkFileForLicense(filename, src : String);
    procedure checkFileForExceptionRaise(filename, src : String; ts : TStringList);
    procedure checkFileForExceptionDefine(filename, src : String; ts : TStringList);
    function checkFileForLineEndings(filename, src : String) : String;

    procedure scanFolder(folder: String; checks: TSourceScanCheckSet);
    procedure checkFile(filename: String; checks: TSourceScanCheckSet);
    function adjustChecksForFolder(folder : String; checks: TSourceScanCheckSet) : TSourceScanCheckSet;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;


function checkExists(folder : String) : String;
begin
  if FolderExists(folder) then
    result := '  (found)'
  else
    result := '  (not found!)';
end;

{ TCodeScanner }

procedure TCodeScanner.reportError(filename : String; line : integer; msg : String);
begin
  FAllOk := false;
  if (line > 0) then
    writeln(filename+' line '+inttostr(line)+': '+msg)
  else
    writeln(filename+': '+msg);
end;

procedure TCodeScanner.checkFileForUnicode(filename : String);
var
  ts : TStringList;
  i : integer;
begin
  ts := TStringList.create;
  try
    for i := 0 to ts.count - 1 do
      if TUnicodeUtilities.hasUnicodeBiDiChars(ts[i]) then
        reportError(filename, i+1, 'File contains Unicode bidi characters which is prohibited in this source');
  finally
    ts.free;
  end;
end;

procedure TCodeScanner.checkFileForLicense(filename, src : String);
var
  ok : boolean;
begin
  ok := (src.IndexOf('Copyright (c)') > 0) or (src.IndexOf('Copyright (C)') > 0) or (src.indexOf('This file was automatically created') > 0);
  if not ok then
    reportError(filename, -1, 'No License statement found');
end;

procedure TCodeScanner.checkFileForExceptionRaise(filename, src : String; ts : TStringList);
var
  i : integer;
  srcns : String;
begin
  for i := 0 to ts.count - 1 do
  begin
    srcns := ts[i].Replace(#9, '').Replace('  ', ' ').ToLower;
    if srcns.contains('raise exception.') then
      reportError(filename, i, 'raises a unspecialised exception');
  end;
end;

procedure TCodeScanner.checkFileForExceptionDefine(filename, src : String; ts : TStringList);
var
  i : integer;
  srcns : String;
begin
  for i := 0 to ts.count - 1 do
  begin
    srcns := ts[i].Replace(#9, '').Replace(' ', '').ToLower;
    if srcns.contains('=class(exception)') and not srcns.contains('efslexception=class(exception)') then
      reportError(filename, i, 'subclasses Exception (should be (EFslException)');
  end;
end;

function TCodeScanner.checkFileForLineEndings(filename, src : String) : String;
var
  b : TStringBuilder;
  changed : boolean;
  i, l, fl : integer;
  ch : char;
begin
  b := TStringBuilder.Create;
  try
    i := 1;
    l := 0;
    fl := -1;
    while (i <= length(src)) do
    begin
      ch := src[i];
      if (ch = #13) then
      begin
        inc(l);
        b.Append(#13#10);
        if (i = length(src)) or (src[i+1] <> #10) then
        begin
          changed := true;
          if fl = -1 then fl := l;
        end
        else
          inc(i);
      end
      else if (ch = #10) then
      begin
        inc(fl);
        b.Append(#13#10);
        if fl = -1 then fl := l;
        changed := true;
      end
      else
      begin
        b.Append(ch);
      end;
      inc(i);
    end;
    if (changed) then
    begin
      reportError(filename, fl, 'File had end of lines that were not #13#10');
      StringToFile(b.ToString, filename, nil);
    end;
    result := b.toString;
  finally
    b.Free;
  end;
end;


procedure TCodeScanner.checkFile(filename: String; checks: TSourceScanCheckSet);
var
  src : String;
  ts : TStringList;
begin
  if (sscUnicode in checks) and StringArrayExists(['.pas', '.inc', '.html', '.css', '.dpr', '.lpr', '.xml', '.json'], ExtractFileExt(filename)) then
    checkFileForUnicode(filename);

  src := '';
  if (sscLineEndings in checks) and StringArrayExists(['.pas', '.inc', '.dpr', '.lpr'], ExtractFileExt(filename)) then
    src := checkFileForLineEndings(filename, FileToString(filename, nil));

  ts := TStringList.create;
  try
    ts.Text := src;
    if (sscLicense in checks) and StringArrayExists(['.pas'], ExtractFileExt(filename)) then
    begin
      if src = '' then
      begin
        src := FileToString(filename, nil);
        ts.Text := src;
      end;
      checkFileForLicense(filename, src);
    end;

    if (sscExceptionRaise in checks) and StringArrayExists(['.pas'], ExtractFileExt(filename)) then
    begin
      if src = '' then
      begin
        src := FileToString(filename, nil);
        ts.Text := src;
      end;
      checkFileForExceptionRaise(filename, src, ts);
    end;

    if (sscExceptionDefine in checks) and StringArrayExists(['.pas'], ExtractFileExt(filename)) then
    begin
      if src = '' then
      begin
        src := FileToString(filename, nil);
        ts.Text := src;
      end;
      checkFileForExceptionDefine(filename, src, ts);
    end;
  finally
    ts.free;
  end;
end;

function TCodeScanner.adjustChecksForFolder(folder : String; checks: TSourceScanCheckSet) : TSourceScanCheckSet;
var
  n : TArray<String>;
begin
  n := folder.Split([PathDelim]);
  if n[length(n)-1] = 'dependencies' then
    result := checks * [sscUnicode, sscLineEndings]
  else if (n[length(n)-2] = 'utilities') and (n[length(n)-1] = 'generator') then
    result := []
  else if (n[length(n)-2] = 'server') and (n[length(n)-1] = 'js') then
    result := []
  else if n[length(n)-1] = 'packages' then
    result := checks * [sscUnicode, sscLineEndings]
  else if StringArrayExists(['backup', '__history', '__recovery', '.git', 'lib'], n[length(n)-1]) then
   result := []
  else
    result := checks;
end;

procedure TCodeScanner.scanFolder(folder : String; checks : TSourceScanCheckSet);
var
  s : String;
begin
  if (checks = []) then
    exit;
  for s in TDirectory.GetFiles(folder) do
    checkFile(s, checks);
  for s in TDirectory.GetDirectories(folder) do
    scanFolder(s, adjustChecksForFolder(s, checks));
end;

procedure TCodeScanner.Run;
begin
  try
    FSourceDir := FilePath([ParamStr(1), 'source']);
    FProjectDir := paramstr(0);
    FProjectDir := FProjectDir.Substring(0, FProjectDir.IndexOf('utilities')-1);
    Writeln('Running FHIR Code scanner');
    Writeln('  - Project folder = '+FProjectDir+' '+checkExists(FProjectDir));
    Writeln('  - Source folder = '+FSourceDir+' '+checkExists(FSourceDir));
    FAllOk := true;
    scanFolder(FSourceDir, [sscUnicode]);
    scanFolder(FilePath([FSourceDir, 'delphi-markdown']), [sscLicense, sscLineEndings, sscExceptionRaise]);
    scanFolder(FilePath([FSourceDir, 'lazarus-ide-tester']), [sscLicense, sscLineEndings, sscExceptionRaise]);
    scanFolder(FProjectDir, [sscUnicode, sscLicense, sscExceptionRaise, sscExceptionDefine, sscLineEndings]);
  except
    on e : Exception do
    begin
      writeln('General Exception: '+e.message);
      FAllOk := false;
    end;
  end;
  if not FAllOk then
  begin
    writeln('Errors found - code fails checks');
    ExitCode := 1
  end
  else
  begin
    writeln('All OK');
    ExitCode := 0;
  end;
  readln;
end;

constructor TCodeScanner.Create;
begin
  inherited Create;
end;

destructor TCodeScanner.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TCodeScanner;
begin
  Application := TCodeScanner.Create;
  Application.Run;
  Application.Free;
end.

