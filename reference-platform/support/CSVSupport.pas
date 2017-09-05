unit CSVSupport;

interface

uses
  SysUtils, Classes,
  AdvFiles, AdvCSVFormatters;

Type
  TCSVWriter = class (TAdvCSVFormatter)
  private
  public
    procedure cell(s : String); overload;
    procedure cell(b : boolean); overload;
    procedure cell(i : integer); overload;
    procedure line;
  end;

  TGetCsvValues = reference to Procedure (csv : TCSVWriter);

procedure produceCsv(filename : String; headers : Array of String; values : TGetCsvValues);

implementation

procedure produceCsv(filename : String; headers : Array of String; values : TGetCsvValues);
var
  csv : TCSVWriter;
  arr : TArray<String>;
  s : String;
begin
  csv := TCSVWriter.Create;
  try
    csv.Stream := TAdvFile.Create(filename, fmCreate);
    for s in headers do
      csv.cell(s);
    csv.line;
    values(csv);
  finally
    csv.Free;
  end;
end;

{ TCSVWriter }

procedure TCSVWriter.cell(s: String);
begin
  HasQuote := (s.Contains('"') or s.Contains(','));
  ProduceEntry(s);
end;

procedure TCSVWriter.cell(b: boolean);
begin
  if (b) then
    ProduceEntry('true')
  else
    ProduceEntry('false');
end;

procedure TCSVWriter.cell(i: integer);
begin
  cell(inttostr(i));
end;

procedure TCSVWriter.line;
begin
  ProduceNewLine;
end;

end.
