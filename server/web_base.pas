unit web_base;

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

{$I fhir.inc}

interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  SysUtils, Classes, Registry,
  IdContext, IdCustomHTTPServer,
  fsl_base, fsl_utilities, fsl_stream, fsl_http, fsl_threads, fsl_web_stream,
  fhir_objects,
  session,
  storage,
  web_source, web_event, web_cache, analytics;

Const
  OWIN_TOKEN_PATH = 'oauth/token';
  PLAIN_KEEP_ALIVE = false;
  SECURE_KEEP_ALIVE = false;
  DEF_SERVER_CONN_LIMIT = 30;

type
  TFHIRWebServerStats = class (TFslObject)
  private
    FLock : TFslLock;
    FRestCount: integer;
    FRestTime: integer;
    FStartTime: UInt64;
    FTotalCount: integer;
    FTotalTime: integer;

    function GetRestCount: integer;
    function GetRestTime: integer;
    function GetTotalCount: integer;
    function GetTotalTime: integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRWebServerStats; overload;

    property StartTime : UInt64 read FStartTime;
    property TotalCount : integer read GetTotalCount;
    property TotalTime : integer read GetTotalTime;
    property RestCount : integer read GetRestCount;
    property RestTime : integer read GetRestTime;

    procedure start;
    procedure restStart;
    procedure totalStart;
    procedure restFinish(ms : integer);
    procedure totalFinish(ms : integer);

    function Present : string;
  end;

  TFHIRWebServerCommon = class (TFslObject)
  private
    FSourceProvider : TFHIRWebServerSourceProvider;
    FHost : String;
    FConnLimit : Integer;
    FAdminEmail: String; // stated administrator
    FName: String; // name of this server
    FOwnerName: String; // name of the org that administers the service
    FGoogle : TGoogleAnalyticsProvider;
    FStats : TFHIRWebServerStats;
    FLock: TFslLock;
    FCache: THTTPCacheManager;
    FStatedPort: word;
    FWorkingSSLPort: word;
    FWorkingPort: word;
    FStatedSSLPort: word;
    FCertHeader: String;
    FSSLHeaderValue: String;
    procedure SetSourceProvider(const Value: TFHIRWebServerSourceProvider);
    procedure SetCache(const Value: THTTPCacheManager);
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRWebServerCommon; overload;

    property SourceProvider : TFHIRWebServerSourceProvider read FSourceProvider write SetSourceProvider;
    property OwnerName : String read FOwnerName write FOwnerName;
    property Name : String read FName write FName;
    property AdminEmail: String read FAdminEmail write FAdminEmail;

    property host: String read FHost write FHost;
    property statedPort : word read FStatedPort write FStatedPort;
    property statedSSLPort : word read FStatedSSLPort write FStatedSSLPort;
    property workingPort : word read FWorkingPort write FWorkingPort;
    property workingSSLPort : word read FWorkingSSLPort write FWorkingSSLPort;
    property CertHeader : String read FCertHeader write FCertHeader;
    property SSLHeaderValue : String read FSSLHeaderValue write FSSLHeaderValue;

    property ConnLimit : Integer read FConnLimit write FConnLimit;
    property Google : TGoogleAnalyticsProvider read FGoogle;
    Property Stats : TFHIRWebServerStats read FStats;
    property Lock : TFslLock read FLock;
    property cache : THTTPCacheManager read FCache write SetCache;
  end;

  TFHIRWebServerBase = class (TFslObject)
  private
    FCommon : TFHIRWebServerCommon;

    // security admin
    FNoUserAuthentication : boolean;
    FUseOAuth: boolean;
    FOWinSecurityPlain: boolean;
    FOWinSecuritySecure: boolean;
    FServeMissingCertificate: boolean;
    FServeUnknownCertificate: boolean;
    FCertificateIdList: TStringList;
    FServeMissingJWT: boolean;
    FServeUnverifiedJWT: boolean;
    FJWTAuthorities: TFslStringDictionary;

    FPatientViewServers: TFslStringDictionary;

    function GetSourceProvider: TFHIRWebServerSourceProvider;
  protected
    function HTTPPort(actual : boolean) : String;
    function SSLPort(actual : boolean) : String;
  public
    constructor Create(common : TFHIRWebServerCommon);
    destructor Destroy; override;

    function loadMultipartForm(const request: TStream; const contentType: String; out mode : TOperationMode): TMimeMessage;
    function extractFileData(langList : THTTPLanguageList; form: TMimeMessage; const name: String; var sContentType: String): TStream;
    function port(actual, default: integer): String;

    property Common : TFHIRWebServerCommon read FCommon;

    property UseOAuth: boolean read FUseOAuth write FUseOAuth;
    property NoUserAuthentication : boolean read FNoUserAuthentication write FNoUserAuthentication;
    property OWinSecurityPlain: boolean read FOWinSecurityPlain write FOWinSecurityPlain;
    property OWinSecuritySecure: boolean read FOWinSecuritySecure write FOWinSecuritySecure;
    property ServeMissingCertificate: boolean read FServeMissingCertificate write FServeMissingCertificate;
    property ServeUnknownCertificate: boolean read FServeUnknownCertificate write FServeUnknownCertificate;
    property CertificateIdList: TStringList read FCertificateIdList;
    property ServeMissingJWT: boolean read FServeMissingJWT write FServeMissingJWT;
    property ServeUnverifiedJWT: boolean read FServeUnverifiedJWT write FServeUnverifiedJWT;
    property JWTAuthorities: TFslStringDictionary read FJWTAuthorities;

    property PatientViewServers: TFslStringDictionary read FPatientViewServers;
    property SourceProvider : TFHIRWebServerSourceProvider read GetSourceProvider;

    function IsTerminologyServerOnly : boolean;
    function GetMimeTypeForExt(AExt: String): String;
  end;

// Function ProcessPath(base, path: String): string;


implementation

{ TFHIRWebServerStats }

constructor TFHIRWebServerStats.Create;
begin
  inherited;
  FLock := TFslLock.Create('web.stats');
  FRestCount := 0;
  FRestTime := 0;
  FStartTime := 0;
  FTotalCount := 0;
  FTotalTime := 0;
end;

destructor TFHIRWebServerStats.Destroy;
begin
  FLock.free;
  inherited Destroy;
end;

function TFHIRWebServerStats.GetRestCount: integer;
begin
  FLock.Lock('GetRestCount');
  try
    result := FRestCount;
  finally
    Flock.Unlock;
  end;
end;

function TFHIRWebServerStats.GetRestTime: integer;
begin
  FLock.Lock('GetRestTime');
  try
    result := FRestTime;
  finally
    Flock.Unlock;
  end;
end;

function TFHIRWebServerStats.GetTotalCount: integer;
begin
  FLock.Lock('GetTotalCount');
  try
    result := FTotalCount;
  finally
    Flock.Unlock;
  end;
end;

function TFHIRWebServerStats.GetTotalTime: integer;
begin
  FLock.Lock('GetTotalTime');
  try
    result := FTotalTime;
  finally
    Flock.Unlock;
  end;
end;

function TFHIRWebServerStats.link: TFHIRWebServerStats;
begin
  result := TFHIRWebServerStats(inherited link);
end;

function TFHIRWebServerStats.Present: string;
var
  rt, rc : integer;
begin
  FLock.Lock('Present');
  try
    rt := FRestTime;
    rc := FRestCount;
  finally
    Flock.Unlock;
  end;
  result := 'Up '+DescribePeriodMS(GetTickCount64 - FStartTime);
  if rc > 0 then
    result := result + '. '+inttostr(rc) + ' Ops at '+FloatToStrF(rt / rc, ffFixed, 1, 0)+' ms/hit'
  else
    result := result + '. 0 Ops at 0 ms/hit';
end;

procedure TFHIRWebServerStats.restStart;
begin
  FLock.Lock('restStart');
  try
    inc(FRestCount, 1);
  finally
    Flock.Unlock;
  end;
end;

procedure TFHIRWebServerStats.start;
begin
  FStartTime := GetTickCount64
end;

procedure TFHIRWebServerStats.totalStart;
begin
  FLock.Lock('totalStart');
  try
    inc(FTotalCount, 1);
  finally
    Flock.Unlock;
  end;
end;

procedure TFHIRWebServerStats.restFinish(ms: integer);
begin
  FLock.Lock('restFinish');
  try
    inc(FRestTime, ms);
  finally
    Flock.Unlock;
  end;
end;

procedure TFHIRWebServerStats.totalFinish(ms: integer);
begin
  FLock.Lock('totalFinish');
  try
    inc(FTotalTime, ms);
  finally
    Flock.Unlock;
  end;
end;


{ TFHIRWebServerBase }

constructor TFHIRWebServerBase.Create(common : TFHIRWebServerCommon);
begin
  inherited Create;
  FPatientViewServers := TFslStringDictionary.Create;
  FCertificateIdList := TStringList.Create;
  FCommon := common;
  if FCommon = nil then
    FCommon := TFHIRWebServerCommon.Create;
end;

destructor TFHIRWebServerBase.Destroy;
begin
  FPatientViewServers.free;
  FCertificateIdList.free;
  FCommon.free;
  inherited;
end;

function TFHIRWebServerBase.loadMultipartForm(const request: TStream; const contentType: String; out mode: TOperationMode): TMimeMessage;
var
  m: TMimeMessage;
  mp: TMimePart;
begin
  m := TMimeMessage.Create;
  Try
    m.ReadFromStream(request, contentType);
    result := m;
    for mp in m.Parts do
    begin
      if SameText(mp.FileName, 'cda.zip') then
        mode := opmUpload;

    end;
  Except
    on e: exception do
    begin
      m.free;
      recordStack(e);
      raise;
    end;
  End;
end;

function TFHIRWebServerBase.extractFileData(langList : THTTPLanguageList; form: TMimeMessage; const name: String; var sContentType: String): TStream;
var
  sLeft, sRight: String;
  iLoop: integer;
  oPart: TMimePart;
  sHeader: String;
  sName: String;
  sFilename: String;
  sContent: String;
begin
  result := nil;
  For iLoop := 0 To form.Parts.Count - 1 Do
  Begin
    oPart := form.Parts[iLoop];
    sHeader := oPart.ContentDisposition;
    StringSplit(sHeader, ';', sLeft, sHeader);
    If trim(sLeft) = 'form-data' Then
    Begin
      StringSplit(trim(sHeader), ';', sLeft, sHeader);
      StringSplit(trim(sLeft), '=', sLeft, sRight);
      If trim(sLeft) = 'name' Then
        sName := RemoveQuotes(trim(sRight));
      StringSplit(trim(sHeader), '=', sLeft, sRight);
      If trim(sLeft) = 'filename' Then
        sFilename := RemoveQuotes(trim(sRight));
      If (result = nil) and (sName <> '') And (sFilename <> '') And (oPart.content.Size > 0) Then
      begin
        result := TBytesStream.Create(oPart.content.AsBytes);
        sContentType := oPart.Mediatype;
      end
      else if (result = nil) and (sName = 'src') then
      begin
        sContent := BytesAsString(oPart.content.AsBytes);
        result := TStringStream.Create(oPart.content.AsBytes); // trim
        if StringStartsWith(sContent, '<', false) then
          sContentType := 'application/fhir+xml'
        else if StringStartsWith(sContent, '{', false) then
          sContentType := 'application/fhir+json'
        else
          raise EFHIRException.CreateLang('FORMAT_UNRECOGNIZED', langList, [sContent]);
      end;
    End
  End;
end;

function TFHIRWebServerBase.port(actual, default : integer) : String;
begin
  if actual = default then
    result := ''
  else
    result := ':' + inttostr(actual);
end;

function TFHIRWebServerBase.IsTerminologyServerOnly: boolean;
begin
  result := false;
end;

Function TFhirWebServerBase.GetMimeTypeForExt(AExt: String): String;
{$IFDEF WINDOWS}
Var
  fReg: TRegistry;
{$ENDIF}
Begin
  result := '';
  AExt := lowercase(AExt);
  if (AExt = '.css') Then
    result := 'text/css'
  Else if AExt = '.ico' Then
    result := 'image/x-icon'
  Else if AExt = '.png' Then
    result := 'image/png'
  Else if AExt = '.gif' Then
    result := 'image/gif'
  Else if AExt = '.jpg' Then
    result := 'image/jpeg'
  Else if AExt = '.mpg' Then
    result := 'video/mpeg'
  Else if AExt = '.js' Then
    result := 'text/javascript'
  Else
  Begin
{$IFDEF WINDOWS}
    Try
      fReg := TRegistry.Create;
      Try
        fReg.RootKey := HKEY_LOCAL_MACHINE;
        fReg.OpenKeyReadOnly('Software\Classes\' + AExt);
        result := fReg.ReadString('Content Type');
        fReg.CloseKey;
      Finally
        fReg.free;
      End;
    Except
    End;
{$ENDIF}
  End;
  If result = '' Then
    result := 'application/octet-stream';
end;

function TFHIRWebServerBase.GetSourceProvider: TFHIRWebServerSourceProvider;
begin
  result := Common.SourceProvider;
end;

function TFHIRWebServerBase.SSLPort(actual : boolean): String;
var
  p : word;
begin
  if actual then
    p := common.workingSSLPort
  else
    p := common.statedSSLPort;
  if p = 443 then
    result := ''
  else
    result := ':'+inttostr(p);
end;

function TFHIRWebServerBase.HTTPPort(actual : boolean): String;
var
  p : word;
begin
  if actual then
    p := common.workingPort
  else
    p := common.statedPort;
  if p = 80 then
    result := ''
  else
    result := ':'+inttostr(p);
end;


{ TFHIRWebServerCommon }

constructor TFHIRWebServerCommon.Create;
begin
  inherited Create;
  FGoogle := TGoogleAnalyticsProvider.Create;
  FStats := TFHIRWebServerStats.Create;
  FLock := TFslLock.Create('web.common');
end;

destructor TFHIRWebServerCommon.Destroy;
begin
  FCache.free;
  FLock.free;
  FStats.free;
  FSourceProvider.free;
  FGoogle.free;
  inherited;
end;

function TFHIRWebServerCommon.link: TFHIRWebServerCommon;
begin
  result := TFHIRWebServerCommon(inherited link);
end;

procedure TFHIRWebServerCommon.SetCache(const Value: THTTPCacheManager);
begin
  FCache.free;
  FCache := Value;
end;

procedure TFHIRWebServerCommon.SetSourceProvider(const Value: TFHIRWebServerSourceProvider);
begin
  FSourceProvider.free;
  FSourceProvider := Value;
end;

//Function ProcessPath(base, path: String): string;
//var
//  s: String;
//begin
//  base := base.Substring(0, base.Length - 1);
//  if path.StartsWith('..\') then
//  begin
//    s := base;
//    while path.StartsWith('..\') do
//    begin
//      path := path.Substring(3);
//      s := ExtractFilePath(s);
//      s := s.Substring(0, s.Length - 1);
//    end;
//    result := IncludeTrailingPathDelimiter(s) + IncludeTrailingPathDelimiter(path);
//  end
//  else
//    result := IncludeTrailingPathDelimiter(path);
//end;
//

end.
