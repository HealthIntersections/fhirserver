unit server_stats;

{
Copyright (c) 2023+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_collections, fsl_utilities;

type

  { TStatusRecord }

  TStatusRecord = class (TFslObject)
  private
    FMagic : integer;

    FMemory : UInt64;
    FThreads : word;
    FRequests : UInt64;
    FRequestTime : UInt64;
    FUserCount : UInt64;
    FConnCount : UInt64;
    FHTTPCacheCount : UInt64;
    FHTTPCacheSize : UInt64;
    FClientCacheCount : UInt64;
    FClientCacheObjectCount : UInt64;
    FClientCacheSize : UInt64;
    FServerCacheCount : UInt64;
    FServerCacheSize : UInt64;
    FSnomedsLoaded : byte;

    FEndpoints : TStringList;

  public
    constructor create; override;
    destructor destroy; override;

    property Magic : integer read FMagic write FMagic;
    property Memory : UInt64 read FMemory write FMemory;
    property Threads : word read FThreads write FThreads;
    property Requests : UInt64 read FRequests write FRequests;
    property RequestTime : UInt64 read FRequestTime write FRequestTime;
    property UserCount : UInt64 read FUserCount write FUserCount;
    property ConnCount : UInt64 read FConnCount write FConnCount;
    property HTTPCacheCount : UInt64 read FHTTPCacheCount write FHTTPCacheCount;
    property HTTPCacheSize : UInt64 read FHTTPCacheSize write FHTTPCacheSize;
    property ClientCacheCount : UInt64 read FClientCacheCount write FClientCacheCount;
    property ClientCacheObjectCount : UInt64 read FClientCacheObjectCount write FClientCacheObjectCount;
    property ClientCacheSize : UInt64 read FClientCacheSize write FClientCacheSize;
    property ServerCacheCount : UInt64 read FServerCacheCount write FServerCacheCount;
    property ServerCacheSize : UInt64 read FServerCacheSize write FServerCacheSize;
    property SnomedsLoaded : byte read FSnomedsLoaded write FSnomedsLoaded;

    procedure clear;
    function count(n : String) : UInt64;
    procedure countEP(n : String; c : UInt64);
  end;

  { TStatusRecords }

  TStatusRecords = class (TFslStringList)
  private
    FLastCount : integer;
    FLastTime : UInt64;
    FEndPointNames : TStringList;
  public
    constructor create;
    destructor Destroy; override;

    property EndPointNames : TStringList read FEndPointNames;

    function link : TStatusRecords; overload;

    procedure addToList(status : TStatusRecord);
    function asCSV : String;
    function asCSVLine : String;
    function asHtml : String;
  end;

implementation

{ TStatusRecord }

constructor TStatusRecord.create;
begin
  inherited create;
  FEndpoints := TStringList.create;
end;

destructor TStatusRecord.destroy;
begin
  FEndpoints.free;
  inherited destroy;
end;

procedure TStatusRecord.clear;
begin
  FEndpoints.clear;
  Magic := 0;
  Memory := 0;
  Threads := 0;
  Requests := 0;
  RequestTime := 0;
  UserCount := 0;
  ConnCount := 0;
  HTTPCacheCount := 0;
  HTTPCacheSize := 0;
  ClientCacheCount := 0;
  ClientCacheObjectCount := 0;
  ClientCacheSize := 0;
  ServerCacheCount := 0;
  ServerCacheSize := 0;
  SnomedsLoaded := 0;
end;

function TStatusRecord.count(n: String): UInt64;
var
  i : integer;
  c : UInt64;
begin
  result := 0;
  for i := 0 to FEndpoints.count - 1 do
    if FEndpoints[i] = n then
    begin
      c := UInt64(FEndpoints.Objects[i]);
      exit(c);
    end;
end;

procedure TStatusRecord.countEP(n: String; c: UInt64);
var
  i : integer;
begin
  if (c <> 0) then
  begin
    for i := 0 to FEndpoints.count - 1 do
    begin
      if FEndpoints[i] = n then
      begin
        FEndpoints.Objects[i] := TObject(UInt64(FEndpoints.Objects[i]) + c);
        exit;
      end;
    end;
    FEndpoints.AddObject(n, TObject(c));
  end;
end;

{ TStatusRecords }

constructor TStatusRecords.create;
begin
  inherited create;
  FEndPointNames := TStringList.create;
end;

destructor TStatusRecords.Destroy;
begin
  FEndPointNames.free;
  inherited Destroy;
end;

function TStatusRecords.link: TStatusRecords;
begin
  result := TStatusRecords(inherited link);
end;

procedure TStatusRecords.addToList(status: TStatusRecord);
var
  s, n, avg : String;
begin
  if (status.RequestTime = 0) or (status.Requests = 0) then
    avg := '0'
  else
    avg := inttostr(trunc((status.RequestTime - FLastTime) / status.Requests - FLastCount));

  s :=
    inttostr(count)+#9+
    FormatDateTime('', now)+#9+
    inttostr(status.Memory)+#9+
    inttostr(status.Threads)+#9+
    inttostr(status.Requests - FLastCount)+#9+
    inttostr(status.Requests)+#9+
    inttostr(status.UserCount)+#9+
    inttostr(status.ConnCount)+#9+
    inttostr(status.HTTPCacheCount)+#9+
    inttostr(status.HTTPCacheSize)+#9+
    inttostr(status.ClientCacheCount)+#9+
    inttostr(status.ClientCacheObjectCount)+#9+
    inttostr(status.ClientCacheSize)+#9+
    inttostr(status.ServerCacheCount)+#9+
    inttostr(status.ServerCacheSize)+#9+
    inttostr(status.SnomedsLoaded)+#9+
    avg;
  for n in FEndPointNames do
    s := s + #9 + inttostr(status.count(n));
  add(s);
  FLastCount := status.Requests;
  FLastTime := status.RequestTime;
end;

function TStatusRecords.asCSV: String;
var
  b : TStringBuilder;
  i : integer;
  s, n: String;
begin
  b := TStringBuilder.create;
  try
    s := 'Minutes'+#9+'Date/Time'+#9+'Memory'+#9+'Threads'+#9+
         'Request Count'+#9+'Total Requests'+#9+
         'User Count'+#9+
         'ConnCount'+#9+'HTTPCacheCount'+#9+'HTTPCacheSize'+#9+
         'ClientCacheCount'+#9+'ClientCacheObjectCount'+#9+'ClientCacheSize'+#9+
         'TxCacheCount'+#9+'TxCacheSize'+#9+'SnomedsLoaded'+#9+'AverageTAT';
    for n in FEndpointNames do
      s := s + #9 + n;
    b.append(s+#13#10);
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
  s, n : String;
begin
  b := TStringBuilder.create;
  try
    s := 'Minutes'+#9+'Date/Time'+#9+'Memory'+#9+'Threads'+#9+
         'Request Count'+#9+'Total Requests'+#9+
         'User Count'+#9+
         'ConnCount'+#9+'HTTPCacheCount'+#9+'HTTPCacheSize'+#9+
         'ClientCacheCount'+#9+'ClientCacheObjectCount'+#9+'ClientCacheSize'+#9+
         'TxCacheCount'+#9+'TxCacheSize'+#9+'SnomedsLoaded'+#9+'AverageTAT';
    for n in FEndpointNames do
      s := s + #9 + n;
    b.append(s+'|');
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
  s, n : String;
begin
  b := TStringBuilder.create;
  try
    b.append('<table>'+#13#10);
    s := '<tr><td>Minutes</td><td>Date/Time</td><td>Memory</td><td>Threads</td><td>'+
         'Request Count</td><td>Total Requests</td><td>'+
         'User Count</td><td>'+
         'ConnCount</td><td>HTTPCacheCount</td><td>HTTPCacheSize</td><td>'+
         'ClientCacheCount</td><td>ClientCacheObjectCount</td><td>ClientCacheSize</td><td>'+
         'ServerCacheCount</td><td>ServerCacheSize</td><td>SnomedsLoaded</td><td>AverageTAT</td>';
    for n in FEndpointNames do
      s := s + '<td>' + n + '</td>';
    b.append(s+'</tr>'+#13#10);
    for i := 0 to count - 1 do
      b.append('<tr><td>'+items[i].replace(#9, '</td><td>')+'</td></tr>'+#13#10);
    b.append('</table>'+#13#10);
    result := b.toString();
  finally
    b.free;
  end;
end;

end.

