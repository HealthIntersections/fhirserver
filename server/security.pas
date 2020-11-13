unit security;


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

interface

uses
  SysUtils, Classes, IniFiles,
  fsl_base, 
  fhir_objects, fhir_factory, fsl_scim;

Const
//  SECURITY_BASE_URI = 'http://www.healthintersections.com.au/scim/entitlement/';
  // can admin SCIM
  SCIM_ADMINISTRATOR = 'http://www.healthintersections.com.au/scim/administer';

  // SMART App Launch Scopes
  SCIM_SMART_PREFIX = 'http://smarthealthit.org/fhir/scopes/';
  SCIM_OPENID_PREFIX = 'http://openid.net/specs/openid-connect-core-1_0#';

type
  TFHIRSecurityRights = class (TFslObject)
  private
    id:integer;
    FSource : String;
    FUserInfo : boolean;
    FAdministerUsers : boolean;
    FReadAll : boolean;
    FReadAllowed : TFslStringSet;
    FWriteAllowed : TFslStringSet;
    FWorker : TFHIRWorkerContextWithFactory;

    function isNonSecure(name : String) : boolean;
    procedure init(worker : TFHIRWorkerContextWithFactory);
    procedure processScopes(scopes: TStringList; base : TFHIRSecurityRights; secure : boolean);
  public
    constructor Create(worker : TFHIRWorkerContextWithFactory; user : TSCIMUser; secure : boolean); overload;
    constructor Create(worker : TFHIRWorkerContextWithFactory; base : TSCIMUser; choice : String; secure : boolean); overload;
    constructor Create(worker : TFHIRWorkerContextWithFactory; base : TSCIMUser; choice : TStringList; secure : boolean); overload;
    destructor Destroy; override;

    property canGetUserInfo : boolean read FUserInfo;
    function canRead(resourceName : String) : boolean;
    function canWrite(resourceName : String) : boolean;
    property canReadAll : boolean read FReadAll;
    property canAdministerUsers : boolean read FAdministerUsers;
    function canReadAny : boolean;
    function canWriteAny : boolean;
    procedure allowAll;

    property source : String read FSource;

    class function allScopes : String;
    class function allScopesAsUris : TStringList;
  end;

function UriForScope(scope : String): String;
function prefixScope(uri : String): String;

implementation

var
  gid : integer = $70707070;

function secureToStr(secure : boolean):String;
begin
  if secure then
    result := 'Secure'
  else
    result := 'Unsecure';
end;

{ TFHIRSecurityRights }

constructor TFHIRSecurityRights.create(worker : TFHIRWorkerContextWithFactory; user: TSCIMUser; secure : boolean);
var
  list : TStringList;
  i : integer;
begin
  inherited Create;
  init(worker);
  list := TStringList.Create;
  try
    for i := 0 to user.entitlementCount - 1 do
      if user.entitlement[i].StartsWith(SCIM_SMART_PREFIX) then
        list.Add(user.entitlement[i].Substring(SCIM_SMART_PREFIX.Length))
      else if user.entitlement[i].StartsWith(SCIM_OPENID_PREFIX) then
        list.Add(user.entitlement[i].Substring(SCIM_OPENID_PREFIX.Length))
      else
        list.add(user.entitlement[i]);
    processScopes(list, nil, secure);
  finally
    list.Free;
  end;
end;

constructor TFHIRSecurityRights.create(worker : TFHIRWorkerContextWithFactory; base: TSCIMUser; choice: String; secure : boolean);
var
  user : TFHIRSecurityRights;
  list : TStringList;
begin
  inherited Create;
  init(worker);
  user := TFHIRSecurityRights.create(FWorker.link, base, secure);
  try
    list := TStringList.Create;
    try
      list.CommaText := choice.Replace(' ', ',');
      processScopes(list, user, false);
    finally
      list.Free;
    end;
  finally
    user.free;
  end;
end;

constructor TFHIRSecurityRights.create(worker : TFHIRWorkerContextWithFactory; base: TSCIMUser; choice: TStringList; secure : boolean);
var
  user : TFHIRSecurityRights;
begin
  inherited Create;
  init(worker);
  user := TFHIRSecurityRights.create(FWorker.link, base, secure);
  try
    processScopes(choice, user, false);
  finally
    user.free;
  end;
end;

destructor TFHIRSecurityRights.destroy;
begin
  FReadAllowed.Free;
  FWriteAllowed.Free;
  Fworker.Free;
  inherited;
end;

procedure TFHIRSecurityRights.init(worker : TFHIRWorkerContextWithFactory);
begin
  inc(gid);
  id := gid;
  FWorker := worker;

  FReadAllowed := TFslStringSet.create;
  FWriteAllowed := TFslStringSet.create;
end;

function TFHIRSecurityRights.isNonSecure(name: String): boolean;
var
  s : string;
begin
  result := false;
  for s in FWorker.nonSecureResourceNames do
    if name = s then
      exit(true);
end;

procedure TFHIRSecurityRights.allowAll;
var
  list : TStringList;
begin
  list := TStringList.Create;
  try
    list.CommaText := allScopes.Replace(' ', ',');
    processScopes(list, nil, true);
  finally
    list.Free;
  end;
end;

class function TFHIRSecurityRights.allScopes: String;
begin
  result := 'openid profile user/*.*';
end;

class function TFHIRSecurityRights.allScopesAsUris: TStringList;
begin
  result := TStringList.create;
  result.add(SCIM_SMART_PREFIX+'openid');
  result.add(SCIM_SMART_PREFIX+'profile');
  result.add(SCIM_SMART_PREFIX+'user/*.*');
end;

function TFHIRSecurityRights.canRead(resourceName : String): boolean;
begin
  if (self = nil) or (FReadAllowed = nil) then
    raise EFHIRException.create('Error Message');
  result := FReadAllowed.contains(resourceName);
end;


function TFHIRSecurityRights.canReadAny: boolean;
begin
  result := not FReadAllowed.isEmpty;
end;

function TFHIRSecurityRights.canWrite(resourceName : String): boolean;
begin
  result := FWriteAllowed.contains(resourceName);
end;

function TFHIRSecurityRights.canWriteAny: boolean;
begin
  result := not FWriteAllowed.isEmpty;
end;

procedure TFHIRSecurityRights.processScopes(scopes: TStringList; base : TFHIRSecurityRights; secure : boolean);
var
  s : String;
  writeall : boolean;
  rn : String;
begin
  FSource := scopes.CommaText.replace(',', ' ');
  if (scopes.IndexOf('openid') > -1) and ((scopes.IndexOf('profile') > -1) or (scopes.IndexOf('fhirUser') > -1)) or ((base <> nil) and base.canGetUserInfo) then
    FUserInfo := true;
  if (scopes.IndexOf(SCIM_ADMINISTRATOR) > -1) and ((base = nil)  or (base.canAdministerUsers)) then
    FAdministerUsers := true;

  FReadAll := true;
  writeall := true;
  for rn in FWorker.allResourceNames do
  begin
    if (base <> nil) and not base.canRead(rn) then
      FReadAllowed.remove(rn)
    else if not assigned(base) and not (secure or isNonSecure(rn)) then
      // no base, so default system access, which is everything if secure, otherwise the non sure types
      FReadAllowed.remove(rn)
    else if scopes.IndexOf('user/*.*') > -1 then
      FReadAllowed.add(rn)
    else if scopes.IndexOf('patient/*.*') > -1 then
      FReadAllowed.add(rn)
    else if scopes.IndexOf('system/*.*') > -1 then
      FReadAllowed.add(rn)
    else if scopes.IndexOf('*.*') > -1 then
      FReadAllowed.add(rn)
    else if scopes.IndexOf('user/*.read') > -1 then
      FReadAllowed.add(rn)
    else if scopes.IndexOf('patient/*.read') > -1 then
      FReadAllowed.add(rn)
    else if scopes.IndexOf('system/*.read') > -1 then
      FReadAllowed.add(rn)
    else if scopes.IndexOf('user/'+rn+'.*') > -1 then
      FReadAllowed.add(rn)
    else if scopes.IndexOf('patient/'+rn+'.*') > -1 then
      FReadAllowed.add(rn)
    else if scopes.IndexOf('system/'+rn+'.*') > -1 then
      FReadAllowed.add(rn)
    else if scopes.IndexOf('user/'+rn+'.read') > -1 then
      FReadAllowed.add(rn)
    else if scopes.IndexOf('patient/'+rn+'.read') > -1 then
      FReadAllowed.add(rn)
    else if scopes.IndexOf('system/'+rn+'.read') > -1 then
      FReadAllowed.add(rn)
    else
      FReadAllowed.remove(rn);
    FReadAll := FReadAll and FReadAllowed.contains(rn);

    if (base <> nil) and not base.canWrite(rn) then
      FWriteAllowed.remove(rn)
    else if not assigned(base) and not (secure or isNonSecure(rn)) then
      // no base, so default system access, which is everything if secure, otherwise the non sure types
      FWriteAllowed.remove(rn)
    else if scopes.IndexOf('user/*.*') > -1 then
      FWriteAllowed.add(rn)
    else if scopes.IndexOf('patient/*.*') > -1 then
      FWriteAllowed.add(rn)
    else if scopes.IndexOf('system/*.*') > -1 then
      FWriteAllowed.add(rn)
    else if scopes.IndexOf('*.*') > -1 then
      FReadAllowed.add(rn)
    else if scopes.IndexOf('user/*.write') > -1 then
      FWriteAllowed.add(rn)
    else if scopes.IndexOf('patient/*.write') > -1 then
      FWriteAllowed.add(rn)
    else if scopes.IndexOf('system/*.write') > -1 then
      FWriteAllowed.add(rn)
    else if scopes.IndexOf('user/'+rn+'.*') > -1 then
      FWriteAllowed.add(rn)
    else if scopes.IndexOf('patient/'+rn+'.*') > -1 then
      FWriteAllowed.add(rn)
    else if scopes.IndexOf('system/'+rn+'.*') > -1 then
      FWriteAllowed.add(rn)
    else if scopes.IndexOf('user/'+rn+'.write') > -1 then
      FWriteAllowed.add(rn)
    else if scopes.IndexOf('patient/'+rn+'.write') > -1 then
      FWriteAllowed.add(rn)
    else if scopes.IndexOf('system/'+rn+'.write') > -1 then
      FWriteAllowed.add(rn)
    else
      FWriteAllowed.remove(rn);
    writeall := writeall and FWriteAllowed.contains(rn);
  end;

  if FreadAll then
    FReadAllowed.add('');
  if WriteAll then
    FWriteAllowed.add('');

  s := '';
  if FUserInfo then
    s := 'openid profile';
  if FAdministerUsers then
    s := s + ' '+ SCIM_ADMINISTRATOR;
  if writeall and FReadAll then
    s := s +' user/*.*'
  else
  begin
    for rn in FWorker.allResourceNames do
      if FWriteAllowed.contains(rn) and FReadAllowed.contains(rn) then
        s := s +' '+rn+'.*'
      else if FReadAllowed.contains(rn) then
        s := s +' '+rn+'.read'
      else if FWriteAllowed.contains(rn) then
        s := s +' '+rn+'.write'
  end;
  FSource := s.Trim;
end;


function UriForScope(scope : String): String;
begin
  if (scope.StartsWith('http:') or scope.StartsWith('https:')) then
    result := scope
  else if (scope = 'openid') or (scope = 'profile') then
    result := SCIM_OPENID_PREFIX+scope
  else
    result := SCIM_SMART_PREFIX+scope;
end;

function prefixScope(uri : String): String;
begin
  if uri.StartsWith(SCIM_SMART_PREFIX) then
    result := 'smart:'+uri.Substring(SCIM_SMART_PREFIX.Length)
  else if uri.StartsWith(SCIM_OPENID_PREFIX) then
    result := 'openid:'+uri.Substring(SCIM_OPENID_PREFIX.Length)
  else if uri.StartsWith('http://www.healthintersections.com.au') then
    result := 'server:'+uri.Substring('http://www.healthintersections.com.au'.Length)
  else
    result := uri;
end;

end.
