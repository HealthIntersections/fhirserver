unit WebSourceProvider;

interface

uses
  SysUtils,
  FileSupport, TextUtilities,
  AdvObjects, AdvZipReaders, AdvZipParts, AdvFiles, AdvGenerics, AdvBuffers;

type
  TFHIRWebServerSourceProvider = {abstract} class (TAdvObject)
  public
    function AltFile(path: String): String;
    function getSource(filename : String) : String; virtual;
  end;

  TFHIRWebServerSourceFolderProvider = class (TFHIRWebServerSourceProvider)
  private
    FSourcePath : String; // where to find web content
  public
    Constructor Create(path : String);
//    Property SourcePath : String read FSourcePath;
    function getSource(filename : String) : String; override;
  end;

  TFHIRWebServerSourceZipProvider = class (TFHIRWebServerSourceProvider)
  private
    FZip : TAdvMap<TAdvBuffer>; // where to find web content
  public
    Constructor Create(path : String);
    Destructor Destroy; override;
//    Property SourcePath : String read FSourcePath;
    function getSource(filename : String) : String; override;
  end;

implementation

{ TFHIRWebServerSourceProvider }

function TFHIRWebServerSourceProvider.AltFile(path : String) : String;
begin
  if path.StartsWith('/') then
    result := path.Substring(1).Replace('/', '\')
  else
    result := path;
end;

function TFHIRWebServerSourceProvider.getSource(filename: String): String;
begin
  raise Exception.Create('Must override "FileToString" in '+className);
end;

{ TFHIRWebServerSourceFolderProvider }

constructor TFHIRWebServerSourceFolderProvider.Create(path: String);
begin
  inherited Create;
  FSourcePath := path;
end;

function TFHIRWebServerSourceFolderProvider.getSource(filename: String): String;
var
  fn : String;
begin
  fn := path([FSourcePath, filename]);
  result := TextUtilities.FileToString(fn, TEncoding.UTF8);
end;

{ TFHIRWebServerSourceZipProvider }

constructor TFHIRWebServerSourceZipProvider.Create(path: String);
var
  zip : TAdvZipReader;
  i : integer;
begin
  inherited Create;
  FZip := TAdvMap<TAdvBuffer>.create;
  zip := TAdvZipReader.Create;
  try
    zip.Stream := TAdvFile.CreateOpen(path);
    zip.ReadZip;
    for i := 0 to zip.Parts.Count - 1 do
      FZip.Add(zip.Parts[i].Name, zip.Parts[i].Link);
  finally
    zip.Free;
  end;
end;

destructor TFHIRWebServerSourceZipProvider.Destroy;
begin

  inherited;
end;

function TFHIRWebServerSourceZipProvider.getSource(filename: String): String;
var
  src : TAdvBuffer;
begin
  if not FZip.TryGetValue('web/'+filename, src) then
    raise Exception.Create('Unable to find '+filename);
  result := src.AsUnicode;
end;

end.
