unit server_stats;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_collections, fsl_utilities;

type
  TStatusRecord = record
    magic : integer;

    Memory : UInt64;
    Threads : word;
    Requests : Cardinal;
    HTTPCacheCount : Cardinal;
    HTTPCacheSize : UInt64;
    ClientCacheCount : Cardinal;
    ClientCacheObjectCount : Cardinal;
    ClientCacheSize : UInt64;
    ServerCacheCount : Cardinal;
    ServerCacheSize : UInt64;
    SnomedsLoaded : byte;
  end;

  { TStatusRecords }

  TStatusRecords = class (TFslStringList)
  public
    function link : TStatusRecords; overload;

    procedure addToList(status : TStatusRecord);
    function asCSV : String;
    function asCSVLine : String;
    function asHtml : String;
  end;

implementation

{ TStatusRecords }

function TStatusRecords.link: TStatusRecords;
begin
  result := TStatusRecords(inherited link);
end;

procedure TStatusRecords.addToList(status: TStatusRecord);
begin
  add(
    FormatDateTime('', now)+#9+
    inttostr(status.Memory)+#9+
    inttostr(status.Threads)+#9+
    inttostr(status.Requests)+#9+
    inttostr(status.HTTPCacheCount)+#9+
    inttostr(status.HTTPCacheSize)+#9+
    inttostr(status.ClientCacheCount)+#9+
    inttostr(status.ClientCacheObjectCount)+#9+
    inttostr(status.ClientCacheSize)+#9+
    inttostr(status.ServerCacheCount)+#9+
    inttostr(status.ServerCacheSize)+#9+
    inttostr(status.SnomedsLoaded));

end;

function TStatusRecords.asCSV: String;
var
  b : TStringBuilder;
  i : integer;
begin
  b := TStringBuilder.create;
  try
    b.append('Date/Time'+#9+'Memory'+#9+'Threads'+#9+'Requests'+#9+
        'HTTPCacheCount'+#9+'HTTPCacheSize'+#9+
        'ClientCacheCount'+#9+'ClientCacheObjectCount'+#9+'ClientCacheSize'+#9+
        'ServerCacheCount'+#9+'ServerCacheSize'+#9+'SnomedsLoaded'+#13#10);
    for i := 0 to count - 1 do
      b.append(items[i]+#13#10);
    result := b.toString();
  finally
    b.free;
  end;
end;

function TStatusRecords.asCSVLine: String;
var
  b : TStringBuilder;
  i : integer;
begin
  b := TStringBuilder.create;
  try
    b.append('Date/Time'+#9+'Memory'+#9+'Threads'+#9+'Requests'+#9+
        'HTTPCacheCount'+#9+'HTTPCacheSize'+#9+
        'ClientCacheCount'+#9+'ClientCacheObjectCount'+#9+'ClientCacheSize'+#9+
        'ServerCacheCount'+#9+'ServerCacheSize'+#9+'SnomedsLoaded'+'|');
    for i := 0 to count - 1 do
      b.append(items[i]+'|');
    result := b.toString();
  finally
    b.free;
  end;
end;


function TStatusRecords.asHtml: String;
var
  b : TStringBuilder;
  i : integer;
begin
  b := TStringBuilder.create;
  try
    b.append('<table>'+#13#10);
    b.append('<tr><td>Date/Time</td><td>Memory</td><td>Threads</td><td>Requests</td><td>'+
        'HTTPCacheCount</td><td>HTTPCacheSize</td><td>'+
        'ClientCacheCount</td><td>ClientCacheObjectCount</td><td>ClientCacheSize</td><td>'+
        'ServerCacheCount</td><td>ServerCacheSize</td><td>SnomedsLoaded</td></tr>'+#13#10);
    for i := 0 to count - 1 do
      b.append('<tr><td>'+items[i].replace(#9, '</td><td>')+'</td></tr>'+#13#10);
    b.append('</table>'+#13#10);
    result := b.toString();
  finally
    b.free;
  end;
end;

end.

