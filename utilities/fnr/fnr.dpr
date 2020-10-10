program fnr;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.AnsiStrings,
  IOUtils;

function FileToString(filename : String) : AnsiString;
var
  LFileStream: TFilestream;
begin
  if FileExists(filename) then
  begin
    LFileStream := TFileStream.Create(filename, fmOpenRead + fmShareDenyWrite);
    try
      SetLength(result, LFileStream.Size);
      if LFileStream.Size > 0 then
        LFileStream.Read(result[1], LFileStream.size);
    finally
      LFileStream.Free;
    end;
  end
  else
    raise Exception.create('File "' + filename + '" not found');
end;

procedure StringToFile(content : AnsiString; filename : String);
var
  LFileStream: TFilestream;
begin
  LFileStream := TFileStream.Create(filename, fmCreate);
  try
    LFileStream.write(content[1], length(content));
  finally
    LFileStream.Free;
  end;
end;

function has(s : AnsiString; src : AnsiString; var index : integer) : boolean;
begin
  index := AnsiPos(src, s);
  result := index > 0;
end;

function replace(filename : String; src, repl : AnsiString) : integer;
var
  s : AnsiString;
  i : integer;
begin
  s := FileToString(filename);
  result := 0;
  while has(s, src, i) do
  begin
    s := copy(s, 1, i-1)+repl+copy(s, i+length(src), length(s)-i);
    inc(result);
  end;
  if (result > 0) then
  begin
    writeln('Replace '+inttostr(result)+' items in '+filename);
    StringToFile(s, filename);
  end;
end;

function scanDir(dir, mask : String; subDirs : boolean; src, repl : AnsiString) : integer;
var
  s : String;
begin
  result := 0;
  for s in TDirectory.GetFiles(dir, mask) do
    result := result + replace(s, src, repl);
  if (subDirs) then
  begin
    for s in TDirectory.GetDirectories(dir) do
      result := result + scanDir(s, mask, subDirs, src, repl)
  end;
end;

procedure execute();
var
  dir, mask, src, repl, subDirs, count : String;
  c : integer;
begin
  if not FindCmdLineSwitch('dir', dir) then
    raise Exception.Create('No "dir" command line parameter');
  if not FindCmdLineSwitch('fileMask', mask) then
    raise Exception.Create('No "fileMask" command line parameter');
  if not FindCmdLineSwitch('find', src) then
    raise Exception.Create('No "find" command line parameter');
  if not FindCmdLineSwitch('replace', repl) then
    raise Exception.Create('No "replace" command line parameter');
  FindCmdLineSwitch('subDirs', subDirs);
  if not FindCmdLineSwitch('count', count) then
    raise Exception.Create('No "count" command line parameter');
  writeln('Replace "'+src+'" with "'+repl+'" in '+dir+'\'+mask+' - count = '+count);
  c := scanDir(dir, mask, (subdirs <> '') and StrToBool(subdirs), src, repl);
  if (inttostr(c) <> count) then
    raise Exception.Create('Expected to replace '+count+' items but actually replaced '+inttostr(c));
end;


begin
  try
    execute();
  except
    on E: Exception do
    begin
      Writeln(E.Message);
      Writeln('Press Enter to continue');
      ReadLn;
    end;
  end;
end.
