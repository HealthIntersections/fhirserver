program versionNotes;

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

//
//function replace(filename : String; src, repl : AnsiString) : integer;
//var
//  s : AnsiString;
//  i : integer;
//begin
//  s := FileToString(filename);
//  result := 0;
//  while has(s, src, i) do
//  begin
//    s := copy(s, 1, i-1)+repl+copy(s, i+length(src), length(s)-i);
//    inc(result);
//  end;
//  if (result > 0) then
//  begin
//    writeln('Replace '+inttostr(result)+' items in '+filename);
//    StringToFile(s, filename);
//  end;
//end;

procedure updateFile(fileName, version, url, fileDest, maven : String);
var
  s, b, e, f, dateFmt : AnsiString;
  i : integer;
begin
  dateFmt := FormatDateTime('yyyy-mm-dd', now);
  s := FileToString(filename);
  if not has(s, '## Current (not released yet)', i) then
    raise Exception.Create('Insertion point not found');
  i := i + length('## Current (not released yet)')+2;
  b := copy(s, 1, i-1);
  e := copy(s, i, length(s)-i);
  f := b+
    #13#10+
    '(no changes yet)'+
    #13#10+
    #13#10+
    '## v'+version+' ('+dateFmt+')'+
    #13#10+
    #13#10+
    e;
  StringToFile(f, filename);
  b := copy(s, i, length(s)-i);
  if not has(b, '##', i) then
    raise Exception.Create('Insertion point #2 not found');
  if (maven <> '') then
    b := '[New Release v'+version+']('+url+') (also in [maven]('+maven+')). Changes :'+#13#10+copy(b, 1, i-1)
  else
    b := '[New Release v'+version+']('+url+'). Changes :'+#13#10+copy(b, 1, i-1);
  StringToFile(b, fileDest);
end;

procedure execute();
var
  fileName, fileDest, version, url, maven : String;
begin
  if not FindCmdLineSwitch('fileName', fileName) then
    raise Exception.Create('No "fileName" command line parameter');
  if not FindCmdLineSwitch('version', version) then
    raise Exception.Create('No "version" command line parameter');
  if not FindCmdLineSwitch('fileDest', fileDest) then
    raise Exception.Create('No "fileDest" command line parameter');
  if not FindCmdLineSwitch('url', url) then
    raise Exception.Create('No "url" command line parameter');
  FindCmdLineSwitch('maven', maven);

  writeln('Update "'+fileName+'" for version "'+version+'" and produce '+fileDest);
  updateFile(fileName, version, url, fileDest, maven);
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
