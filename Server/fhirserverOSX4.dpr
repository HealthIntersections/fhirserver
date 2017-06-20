program fhirserverOSX4;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  AdvObjects in '..\reference-platform\support\AdvObjects.pas',
  AdvExceptions in '..\reference-platform\support\AdvExceptions.pas',
  StringSupport in '..\reference-platform\support\StringSupport.pas',
  MathSupport in '..\reference-platform\support\MathSupport.pas',
  OSXUtils in '..\reference-platform\support\OSXUtils.pas';

var
  obj : TAdvObject;
begin
  try
    obj := TADvObject.Create;
    try
      writeln('link#: '+inttostr(obj.AdvObjectReferenceCount));
      obj.Link;
      writeln('link#: '+inttostr(obj.AdvObjectReferenceCount));
      obj.Free;
      writeln('link#: '+inttostr(obj.AdvObjectReferenceCount));
    finally
      obj.Free;
    end;
    readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
