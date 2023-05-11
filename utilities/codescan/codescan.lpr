program codescan;

{
This program is run as part of the ci-build to enforce coding standards
on the fhirserver code. The following rules are enforced:

* code can't have any unicode bi-di control characters in it
   file extensions: .pas, .lpr, .inc, .html, .css,
   (including in all the source packages too)
   see CVE-2021-42574

* all .pas files must have a license statement in a comment at the head
   (except for in /dependencies)

* never raise Exception directly - always a subclass
   (and all exceptions should subclass EFslException outside /dependencies)

* check .pas line endings are all crlf (make them so)
   (but still has to be corrected in the PR)

For now, the program doesn't apply code formatting to the code.
That might be reviewed if a working code formatter is found in the
future (extensive use of $IFDEF blows most code formatters brains)

The program takes one parameter, which is the name of the root folder
that contains the source directory. It assumes that the root of the
FHIRServer repository is up from folder containing the executable.

Also, this program serves some utility functions in the release process - updating
code versions, and checking that particular files exist (or not)
}

{$MODE DELPHI}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Forms, Interfaces,
  Classes, SysUtils,
  DelphiAST, DelphiAST.Consts, DelphiAST.Classes, SimpleParser.Lexer.Types, SimplerParser.Lexer.Config,
  fsl_utilities, fsl_fpc, fsl_stream, fsl_unicode, fsl_versions, codeScanForm;


type
  TIncludeHandler = class(TInterfacedObject, IIncludeHandler)
  private
    FPath: string;
  public
    constructor Create(const Path: string);
    function GetIncludeFileContent(const ParentFileName, IncludeName: string; out Content: string; out FileName: string): Boolean;
  end;

  { TCodeScanner }
  TSourceScanCheck = (sscUnicode, sscLicense, sscExceptionRaise, sscExceptionDefine, sscLineEndings, sscParse);
  TSourceScanCheckSet = set of TSourceScanCheck;

  TCodeScanner = class(TObject)
  private
    FAllOk : boolean;
    FSourceDir : String;
    FProjectDir : String;

    FOnLog : TLogEvent;

    procedure output(msg : String = ''; ack : boolean = false);

    procedure check(n, m: String);
    procedure reportError(filename : String; line : integer; msg : String);

    procedure checkUnitName(code : TSyntaxNode; filename: String);
    procedure parseFile(filename: String; incFolder : String);

    procedure checkFileForUnicode(filename : String);
    procedure checkFileForLicense(filename, src : String);
    procedure checkFileForExceptionRaise(filename, src : String; ts : TStringList);
    procedure checkFileForExceptionDefine(filename, src : String; ts : TStringList);
    function checkFileForLineEndings(filename, src : String) : String;

    procedure scanFolder(folder: String; checks: TSourceScanCheckSet; incFolder : String);
    procedure checkFile(filename: String; checks: TSourceScanCheckSet; incFolder : String);
    function adjustChecksForFolder(folder : String; checks: TSourceScanCheckSet) : TSourceScanCheckSet;
    procedure setInstallVersion(n, p, v: String);
    procedure setSourceVersion(v: String);
    procedure setProjectVersion(p, v: String; d : boolean);
    procedure setNextSourceVersion(v : String);
    procedure go(sender : TObject);
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

{ TIncludeHandler }

constructor TIncludeHandler.Create(const Path: string);
begin
  inherited Create;
  FPath := Path;
end;

function TIncludeHandler.GetIncludeFileContent(const ParentFileName, IncludeName: string;
  out Content: string; out FileName: string): Boolean;
var
  FileContent: TStringList;
begin
  FileContent := TStringList.Create;
  try
    if (includeName = 'fhir5.inc') then
      FileName := FilePath([FPath, 'fhir5', IncludeName])
    else if (includeName = 'fhir4.inc') then
      FileName := FilePath([FPath, 'fhir4', IncludeName])
    else if (includeName = 'fhir4b.inc') then
      FileName := FilePath([FPath, 'fhir4b', IncludeName])
    else if (includeName = 'fhir3.inc') then
      FileName := FilePath([FPath, 'fhir3', IncludeName])
    else if (includeName = 'fhir2.inc') then
      FileName := FilePath([FPath, 'fhir2', IncludeName])
    else if (includeName = 'fui_gfx.inc') then
      FileName := FilePath([FPath, 'fui', IncludeName])
    else
      FileName := FilePath([FPath, IncludeName]);
    FileContent.LoadFromFile(FileName);
    Content := FileContent.Text;
    Result := True;
  finally
    FileContent.Free;
  end;
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

procedure TCodeScanner.checkUnitName(code : TSyntaxNode; filename: String);
begin
  if (code.GetAttribute(anName) <> '') and (code.GetAttribute(anName) <> PathTitle(filename)) then
    reportError(filename, code.Line, 'unit name doesn''t match file name: "'+code.GetAttribute(anName)+'" vs "'+PathTitle(filename)+'"');
end;

procedure TCodeScanner.parseFile(filename: String; incFolder : String);
var
  code : TSyntaxNode;
begin
  try
    DelphiParserConfig.LoadDefaults;
    DelphiParserConfig.FreePascal := false;
    DelphiParserConfig.ConfigureWin64;
    code := TPasSyntaxTreeBuilder.Run(FileName, False, TIncludeHandler.Create(incFolder));
    try
      checkUnitName(code, filename);
    finally
      code.free;
    end;
  Except
    on e : ESyntaxTreeException do
      reportError(filename, e.line, e.message+' (Windows)');
    on e : Exception do
      reportError(filename, -1, e.message+' (Windows)');
  end;
  try
    DelphiParserConfig.LoadDefaults;
    DelphiParserConfig.FreePascal := true;
    DelphiParserConfig.ConfigureWin64;
    code := TPasSyntaxTreeBuilder.Run(FileName, False, TIncludeHandler.Create(incFolder));
    try
      checkUnitName(code, filename);
    finally
      code.free;
    end;
  Except
    on e : ESyntaxTreeException do
      reportError(filename, e.line, e.message+' (Windows/FPC)');
    on e : Exception do
      reportError(filename, -1, e.message+' (Windows/FPC)');
  end;
  try
    DelphiParserConfig.LoadDefaults;
    DelphiParserConfig.FreePascal := true;
    DelphiParserConfig.ConfigureLinux64;
    code := TPasSyntaxTreeBuilder.Run(FileName, False, TIncludeHandler.Create(incFolder));
    try
      checkUnitName(code, filename);
    finally
      code.free;
    end;
  Except
    on e : ESyntaxTreeException do
      reportError(filename, e.line, e.message+' (Linux)');
    on e : Exception do
      reportError(filename, -1, e.message+' (Linux)');
  end;
  try
    DelphiParserConfig.LoadDefaults;
    DelphiParserConfig.FreePascal := true;
    DelphiParserConfig.ConfigureMacM1;
    code := TPasSyntaxTreeBuilder.Run(FileName, False, TIncludeHandler.Create(incFolder));
    try
      checkUnitName(code, filename);
    finally
      code.free;
    end;
  Except
    on e : ESyntaxTreeException do
      reportError(filename, e.line, e.message+' (OSX/M1)');
    on e : Exception do
      reportError(filename, -1, e.message+' (OSX/M1)');
  end;
end;

procedure TCodeScanner.checkFile(filename: String; checks: TSourceScanCheckSet; incFolder : String);
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

  if (sscParse in checks) and StringArrayExists(['.pas'], ExtractFileExt(filename)) then
    parseFile(filename, incFolder);
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
  else if n[length(n)-1] = 'install' then
    result := checks * [sscUnicode, sscLineEndings]
  else if n[length(n)-1] = 'r5gen' then
    result := checks * [sscUnicode, sscLineEndings]
  else if StringArrayExists(['backup', '__history', '__recovery', '.git', 'lib'], n[length(n)-1]) then
   result := []
  else
    result := checks;
end;

procedure TCodeScanner.scanFolder(folder : String; checks : TSourceScanCheckSet; incFolder : String);
var
  s : String;
begin
  if (checks = []) then
    exit;
  write('.');
  for s in TDirectory.GetFiles(folder) do
    checkFile(s, checks, incFolder);
  for s in TDirectory.GetDirectories(folder) do
    scanFolder(s, adjustChecksForFolder(s, checks), incFolder);
end;

procedure TCodeScanner.setSourceVersion(v : String);
var
  dt, inc : string;
begin
  if not TSemanticVersion.isValid(v) then
    raise Exception.create('Invalid semantic version '+v);

  dt := TFslDateTime.makeUTC().toString('yyyy-mm-dd');
  Writeln('Update version.inc to set version to '+v+' on '+dt);

  inc :=
    ' FHIR_CODE_FULL_VERSION = '''+v+''';'+#13#10+
    ' FHIR_CODE_RELEASE_DATE = '''+dt+''';'+#13#10+
    ' FHIR_CODE_RELEASE_DATETIME = '''+TFslDateTime.makeUTC().toHL7+''';'+#13#10;
  StringToFile(inc, FilePath([FProjectDir, 'library', 'version.inc']), TEncoding.ASCII);
  FAllOk := true;
end;

procedure TCodeScanner.setProjectVersion(p, v: String; d : boolean);
var
  sv : TSemanticVersion;
begin
  sv := TSemanticVersion.fromString(v);
  try
    sv.applyToProject(p, d);
  finally
    sv.free;
  end;
  FAllOk := true;
end;


procedure TCodeScanner.setNextSourceVersion(v : String);
var
  ver : TSemanticVersion;
  dt, inc : String;
begin
  ver := TSemanticVersion.fromString(v);
  try
    ver.incVer(semverPatch);
    ver.BuildLabel := 'SNAPSHOT';
    v := ver.ToString;
  finally
    ver.free;
  end;
  dt := TFslDateTime.makeUTC().toString('yyyy-mm-dd');
  Writeln('Update version.inc to set version to '+v+' on '+dt);

  inc :=
    ' FHIR_CODE_FULL_VERSION = '''+v+''';'+#13#10+
    ' FHIR_CODE_RELEASE_DATE = '''+dt+''';'+#13#10+
    ' FHIR_CODE_RELEASE_DATETIME = '''+TFslDateTime.makeUTC().toHL7+''';'+#13#10;
  StringToFile(inc, FilePath([FProjectDir, 'library', 'version.inc']), TEncoding.ASCII);
  FAllOk := true;
end;

procedure TCodeScanner.go(sender: TObject);
begin
  Run;
end;

procedure TCodeScanner.output(msg: String = ''; ack : boolean = false);
begin
  if (assigned(FOnLog)) then
    FOnLog(msg, ack)
  else
  begin
    writeln(msg);
    if (ack) then
      readln;
  end;
end;

procedure TCodeScanner.check(n, m : String);
var
  ne : boolean;
begin
  ne := n.StartsWith('!');
  if (ne) then
    delete(n, 1, 1);
  FAllOk := false;
  if ne then
  begin
    if FileExists(n) then
      output('Error: '+m)
    else
      FAllOk := true;
  end
  else if not FileExists(n) then
    output('Error: '+m)
  else
    FAllOk := true;
end;

procedure TCodeScanner.setInstallVersion(n, p, v : String);
var
  ts : TStringList;
  i : integer;
  s : String;
begin
  if not TSemanticVersion.isValid(v) then
    raise Exception.create('Invalid semantic version '+v);

  ts := TStringList.create;
  try
    ts.LoadFromFile(FilePath([FProjectDir, n]));
    for i := 0 to ts.count - 1 do
    begin
      s := ts[i];
      if s.StartsWith('AppVerName=') then
        s := 'AppVerName='+p+' v'+v
      else if s.StartsWith('OutputBaseFilename=') then
        s := 'OutputBaseFilename='+p.ToLower+'-win64-'+v
      else if s.StartsWith('AppVersion=') then
        s := 'AppVersion='+v
      else if s.StartsWith('VersionInfoVersion=') then
        s := 'VersionInfoVersion='+v+'.0';
      ts[i] := s;
    end;
    ts.SaveToFile(FilePath([FProjectDir, n]));
  finally
    ts.free;
  end;
  FAllOk := true;
end;

procedure TCodeScanner.Run;
var
  v, n, m, p, d : String;
begin
  v := '';
  n := '';
  m := '';
  p := '';
  d := '';

  //output(commandLineAsString);
  try
    FProjectDir := paramstr(0);
    FProjectDir := FProjectDir.Substring(0, FProjectDir.IndexOf('utilities')-1);
    if getCommandLineParam('install', n) and getCommandLineParam('version', v) and getCommandLineParam('product', p)  then
      setInstallVersion(n, p, v)
    else if getCommandLineParam('proj-version', p) and getCommandLineParam('version', v) and getCommandLineParam('debug', d) then
      setProjectVersion(p, v, d = 'true')
    else if getCommandLineParam('version', v) then
      setSourceVersion(v)
    else if getCommandLineParam('next-version', v) then
      setNextSourceVersion(v)
    else if getCommandLineParam('check', n) and getCommandLineParam('message', m) then
      check(n, m)
    else
    begin
      FSourceDir := FilePath([ParamStr(1), 'source']);
      output('Running FHIR Code scanner');
      output('  - Project folder = '+FProjectDir+' '+checkExists(FProjectDir));
      output('  - Source folder = '+FSourceDir+' '+checkExists(FSourceDir));
      FAllOk := true;
      output(FSourceDir+' [unicode]');
      scanFolder(FSourceDir, [sscUnicode], FProjectDir);
      output('');
      output(FilePath([FSourceDir, 'delphi-markdown'])+' [license, eoln, exceptions, full-parse]');
      scanFolder(FilePath([FSourceDir, 'delphi-markdown']), [sscLicense, sscLineEndings, sscExceptionRaise, sscParse], FilePath([FSourceDir, 'delphi-markdown']));
      output('');
      output(FilePath([FSourceDir, 'lazarus-ide-tester'])+' [license, eoln, exceptions, full-parse]');
      scanFolder(FilePath([FSourceDir, 'lazarus-ide-tester']), [sscLicense, sscLineEndings, sscExceptionRaise, sscParse], FilePath([FSourceDir, 'lazarus-ide-tester']));
      output('');
      output(FProjectDir+' [license, eoln, exceptions, full-parse]');
      scanFolder(FProjectDir, [sscUnicode, sscLicense, sscExceptionRaise, sscExceptionDefine, sscLineEndings, sscParse], FilePath([FProjectDir, 'library']));
      output('');
    end;
  except
    on e : Exception do
    begin
      writeln('General Exception: '+e.message);
      FAllOk := false;
    end;
  end;
  ExitCode := 0;
  if hasCommandLineParam('version') or hasCommandLineParam('check') then
  begin
    if not FAllOk then
      ExitCode := 1
  end
  else if not FAllOk then
  begin
    output('Errors found - code fails checks', true);
    ExitCode := 1;
  end
  else
  begin
    output('All OK', true);
  end;
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
  CodeApp: TCodeScanner;

begin
  CodeApp := TCodeScanner.Create;
  try
    if paramstr(1) = 'gui' then
    begin
      //Application.Title:='FHIRToolkit';
      //Application.Scaled:=True;
      Application.Initialize;
      Application.CreateForm(TCodeScannerForm, CodeScannerForm);
      CodeApp.FOnLog := CodeScannerForm.Log;
      CodeScannerForm.OnExecute := CodeApp.Go;
      Application.Run;
    end
    else
      CodeApp.run;
  finally
    CodeApp.Free;
  end;
end.




