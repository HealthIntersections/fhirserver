unit xig_provider;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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
  {$IFDEF FPC} ZStream, {ELSE} Zlib, {$ENDIF}
  fsl_base, fsl_lang, fsl_utilities, fsl_logging, fsl_i18n,
  fdb_manager,
  fhir_objects, fhir_factory, fhir_common, fhir_parser,
  fhir5_context;

type
  TXIGProvider = class;

  { TXIGResourceProxy }

  TXIGResourceProxy = class (TFHIRResourceProxy)
  private
    FKey: Integer;
    FDB: TFDBManager;

  protected
    procedure loadResource; override;
  public
    destructor Destroy; override;

    property key : Integer read FKey write FKey;
  end;

  { TXigLoader }

  TXigLoader = class (TFslObject)
  private
    FConn : TFDBConnection;
    FFactory: TFHIRFactory;
    procedure SetFactory(AValue: TFHIRFactory);
  public
    destructor Destroy; override;      
    property Factory : TFHIRFactory read FFactory write SetFactory;

    function next : boolean;
    function makeResource : TFHIRResourceProxyV;
  end;

  { TXIGProvider }

  TXIGProvider = class (TFslObject)
  private
    FLanguages : TIETFLanguageDefinitions;
    FDb : TFDBManager;
  public
    constructor Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport; db : TFDBManager);
    destructor Destroy; override;

    function link : TXIGProvider; overload;
    function startLoad(types : TStringArray) : TXigLoader;
  end;

implementation

{ TXIGResourceProxy }

destructor TXIGResourceProxy.Destroy;
begin
  FDB.free;
  inherited Destroy;
end;

{$IFDEF FPC}
procedure DecompressStream(src, dst: TStream);
var
  ds: TDecompressionStream;
  d: dword;
  buff: array[0..1023] of byte;
begin
  ds := TDecompressionStream.Create(src, true);
  try
    repeat
      d := ds.Read(buff, 1024);
      dst.Write(buff, d);
    until
      d = 0;
  finally
    ds.Free;
  end;
end;

function inflate(source:TBytes):TBytes;
var
  ss1, ss2: TStringStream;
begin
  ss1 := TStringStream.Create;
  try
    ss1.write(source[0], length(source));
    ss1.Position := 10; //SKIP GZIP HEADER

    ss2 := TStringStream.Create;
    try
      DecompressStream(ss1, ss2);

      writeln('decompressed ', ss1.Size, ' bytes to ', ss2.Size, ' bytes');

      ss2.Position := 0;
      setLength(result, ss2.Size);
      ss2.Read(result[0], length(result));
    finally
      ss2.Free;
    end;
  finally
    ss1.Free;
  end;
end;
{$ENDIF}

procedure TXIGResourceProxy.loadResource;
var
  conn : TFDBConnection;
  cnt : TBytes;
  p : TFHIRParser;
begin
  try
    conn := FDB.GetConnection('Load Resource');
    try
      conn.sql := 'select JsonR5 from Contents where ResourceKey = '+inttostr(FKey);
      conn.Prepare;
      conn.Execute;
      if not conn.FetchNext then
        raise EFslException.create('Unable to find resource key '+inttostr(FKey));
      cnt := conn.ColBlob[1];
      conn.terminate;
      conn.Release;
    except
      on e : Exception do
      begin
        conn.Error(e);
        raise;
      end;
    end;
    if (length(cnt) = 0) then    
        raise EFslException.create('Unable to load content for resource key '+inttostr(FKey));

    {$IFDEF FPC}
    cnt := inflate(cnt);
    {$ELSE}
    raise EFslException.Create('Not Implemented Yet');
    {$ENDIF}

    p := Factory.makeParser(Worker, ffJson, nil);
    try
      FResourceV := p.parseResource(cnt);
    finally
      p.free;
    end;
  except
    on e : Exception do
      raise EFSLException.create('Unable to read XIG Resource '+inttostr(key)+': '+e.Message);
  end;
end;

{ TXigLoader }

destructor TXigLoader.Destroy;
begin
  FConn.Release;
  FFactory.Free;
  inherited Destroy;
end;

function TXigLoader.next: boolean;
begin
  result := FConn.FetchNext;
end;

function TXigLoader.makeResource: TFHIRResourceProxyV; 
var
  p : TXIGResourceProxy;
begin
  p := TXIGResourceProxy.create('', factory.version, FConn.ColStringByName['ResourceTypeR5'],
          FConn.ColStringByName['id'], FConn.ColStringByName['Url'], FConn.ColStringByName['Version'],
          FConn.ColStringByName['Supplements'], FConn.ColStringByName['Content'], FConn.ColStringByName['ValueSet']);
  try
    p.factory := factory.link;
    p.key := FConn.ColIntegerByName['ResourceKey'];
    p.FDB := FConn.Owner.link;
    result := p.link;
  finally
    p.free;
  end;
end;

procedure TXigLoader.SetFactory(AValue: TFHIRFactory);
begin
  FFactory.Free;
  FFactory := AValue;
end;

{ TXIGProvider }

constructor TXIGProvider.Create(languages : TIETFLanguageDefinitions; i18n : TI18nSupport; db : TFDBManager);
begin
  inherited Create;
  FLanguages := languages;
  FDb := db;
end;

destructor TXIGProvider.Destroy;
begin
  FLanguages.free;
  FDb.free;
  inherited Destroy;
end;

function TXIGProvider.link: TXIGProvider;
begin
  result := TXIGProvider(inherited Link);
end;

function TXIGProvider.startLoad(types: TStringArray): TXigLoader;
var
  s, t : String;
  conn : TFDBConnection;
begin
  result := TXigLoader.create;
  try
    result.FConn := FDB.GetConnection('load resources');
    s := '';
    for t in types do
      s := s + ', '''+sqlWrapString(t)+'''';
    result.FConn.SQL := 'select ResourceKey, PID, ResourceTypeR5, Resources.id, Resources.Web, Url, Resources.Version, Content, Supplements, ValueSet from Resources, Packages where Resources.PackageKey = Packages.PackageKey and ResourceTypeR5 in ('+s.substring(2)+')';
    result.FConn.Prepare;
    result.FConn.Execute;
    result.link;
  finally
    result.free;
  end;
end;

end.

