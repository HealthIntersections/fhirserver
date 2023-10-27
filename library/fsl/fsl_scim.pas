unit fsl_scim;

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
  SysUtils,
  fsl_base, fsl_utilities, fsl_collections, fsl_json;

Type
  ESCIMException = class (EFslException)
  private
    FStatus : integer;
    FStatusText : String;
    FScimType : String;
  public
    constructor Create(status : Integer; statusText : String; scimType : String; sMessage : String);
    property Status : integer read FStatus;
    property StatusText : String read FStatusText;
    property ScimType : String read FScimType;
  end;

  TSCIMObject = class (TFslObject)
  private
    FCreated : TFslDateTime;
    FLastModified : TFslDateTime;
    FJson : TJsonObject;
    function GetCreated: TFslDateTime;
    function GetId: String;
    function GetLastModified: TFslDateTime;
    function GetLocation: String;
    function GetResourceType: String;
    function GetVersion: String;
    procedure SetCreated(const Value: TFslDateTime);
    procedure SetId(const Value: String);
    procedure SetLastModified(const Value: TFslDateTime);
    procedure SetLocation(const Value: String);
    procedure SetResourceType(const Value: String);
    procedure SetVersion(const Value: String);
    function GetExternalId: String;
    procedure SetExternalId(const Value: String);
    function GetCreatedUTC: TFslDateTime;
    function GetLastModifiedUTC: TFslDateTime;
  protected
    function hasSchema(s : String) : boolean;
    procedure checkSchema(s : String);
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(json : TJsonObject); virtual;
    destructor Destroy; override;

    property id : String read GetId write SetId;
    property ExternalId : String read GetExternalId write SetExternalId;
    property resourceType : String read GetResourceType write SetResourceType;
    property created : TFslDateTime read GetCreated write SetCreated;
    property lastModified : TFslDateTime read GetLastModified write SetLastModified;
    property createdUTC : TFslDateTime read GetCreatedUTC;
    property lastModifiedUTC : TFslDateTime read GetLastModifiedUTC;
    property location : String read GetLocation write SetLocation;
    property version : String read GetVersion write SetVersion;
    property json : TJsonObject read FJson;
  end;

  TSCIMContact = class (TFslObject)
  private
    FJson : TJsonObject;
    function GetType: String;
    function GetValue: String;
    procedure SetType(const Value: String);
    procedure SetValue(const Value: String);
    function GetPrimary: Boolean;
    procedure SetPrimary(const Value: Boolean);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(FJson : TJsonObject);
    destructor Destroy; override;

    Property Value : String read GetValue write SetValue;
    Property Type_ : String read GetType write SetType;
    Property Primary : Boolean read GetPrimary write SetPrimary;
  end;

  TSCIMContactList = class (TFslObjectList)
  private
    function GetContact(index: integer): TSCIMContact;
  protected
    function ItemClass : TFslObjectClass; override;
  public
    Property Contact[index: integer] : TSCIMContact read GetContact; default;
  end;

  TSCIMAddress = class (TFslObject)
  private
    FJson : TJsonObject;
    function GetType: String;
    function GetPrimary: Boolean;
    procedure SetPrimary(const Value: Boolean);
    procedure SetType(const Value: String);
    function GetCountry: String;
    function GetFormatted: String;
    function GetLocality: String;
    function GetPostalCode: String;
    function GetRegion: String;
    function GetStreetAddress: String;
    procedure SetCountry(const Value: String);
    procedure SetFormatted(const Value: String);
    procedure SetLocality(const Value: String);
    procedure SetPostalCode(const Value: String);
    procedure SetRegion(const Value: String);
    procedure SetStreetAddress(const Value: String);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(FJson : TJsonObject);
    destructor Destroy; override;

    Property Type_ : String read GetType write SetType;
    Property Primary : Boolean read GetPrimary write SetPrimary;
    Property StreetAddress : String read GetStreetAddress write SetStreetAddress;
    Property Locality : String read GetLocality write SetLocality;
    Property Region : String read GetRegion write SetRegion;
    Property PostalCode : String read GetPostalCode write SetPostalCode;
    Property Country : String read GetCountry write SetCountry;
    Property Formatted : String read GetFormatted write SetFormatted;
  end;

  TSCIMAddressList = class (TFslObjectList)
  private
    function GetAddress(index: integer): TSCIMAddress;
  protected
    function ItemClass : TFslObjectClass; override;
  public
    Property Address[index: integer] : TSCIMAddress read GetAddress; default;
  end;

  TSCIMUser = class (TSCIMObject)
  private
    FEmails : TSCIMContactList;
    FPhoneNums : TSCIMContactList;
    FIMs : TSCIMContactList;
    FAddresses : TSCIMAddressList;
    FHash: string;

    function GetPassword: String;
    procedure SetPassword(const Value: String);
    function GetUsername: String;
    procedure SetUsername(const Value: String);

    function GetDisplayName: String;
    function GetFamilyName: String;
    function GetGivenName: String;
    function GetLocale: String;
    function GetMiddleName: String;
    function GetName: String;
    function GetNickName: String;
    function GetPreferredLanguage: String;
    function GetPrefix: String;
    function GetProfileUrl: String;
    function GetSuffix: String;
    function GetTimezone: String;
    function GetTitle: String;
    function GetUserType: String;
    procedure SetDisplayName(const Value: String);
    procedure SetFamilyName(const Value: String);
    procedure SetGivenName(const Value: String);
    procedure SetLocale(const Value: String);
    procedure SetMiddleName(const Value: String);
    procedure SetName(const Value: String);
    procedure SetNickName(const Value: String);
    procedure SetPreferredLanguage(const Value: String);
    procedure SetPrefix(const Value: String);
    procedure SetProfileUrl(const Value: String);
    procedure SetSuffix(const Value: String);
    procedure SetTimezone(const Value: String);
    procedure SetTitle(const Value: String);
    procedure SetUserType(const Value: String);
    function GetEmails: TSCIMContactList;
    function GetIms: TSCIMContactList;
    function GetPhoneNums: TSCIMContactList;
    function GetAddresses: TSCIMAddressList;
    function GetEntitlement(i: integer): String;
    function GetEntitlementCount: integer;
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(FJson : TJsonObject); override;
    constructor CreateNew;
    destructor Destroy; override;
    Function Link : TSCIMUser; overload;

    procedure check;
    procedure copyFrom(source : TSCIMUser);

    property hash : string read FHash write FHash; // an internal property of the user that is never serialised
    Property password : String read GetPassword write SetPassword;
    Property username : String read GetUsername write SetUsername;

    Property formattedName : String read GetName write SetName;
    Property FamilyName : String read GetFamilyName write SetFamilyName;
    Property GivenName : String read GetGivenName write SetGivenName;
    Property MiddleName : String read GetMiddleName write SetMiddleName;
    Property Prefix : String read GetPrefix write SetPrefix;
    Property Suffix : String read GetSuffix write SetSuffix;
    Property DisplayName : String read GetDisplayName write SetDisplayName;
    Property NickName : String read GetNickName write SetNickName;
    Property ProfileUrl : String read GetProfileUrl write SetProfileUrl;
    Property Title : String read GetTitle write SetTitle;
    Property UserType : String read GetUserType write SetUserType;
    Property PreferredLanguage : String read GetPreferredLanguage write SetPreferredLanguage;
    Property Locale : String read GetLocale write SetLocale;
    Property Timezone : String read GetTimezone write SetTimezone;

    Property emails : TSCIMContactList read GetEmails;
    procedure clearEmails;
    Property phoneNums : TSCIMContactList read GetPhoneNums;
    Property ims : TSCIMContactList read GetIms;
    Property addresses : TSCIMAddressList read GetAddresses;

    Property entitlementCount : integer read GetEntitlementCount;
    Property entitlement[i : integer] : String read GetEntitlement;

    procedure clearEntitlements;

    function AddEmail(value, type_ : String) : TSCIMContact;
    function hasEmail(value : String) : boolean;
    procedure addEntitlement(value : String);
    function hasEntitlement(value : String) : Boolean;

    function bestName : String;
  end;


implementation

{ TSCIMObject }

constructor TSCIMObject.Create(json: TJsonObject);
begin
  inherited Create;
  self.FJson := json;
end;

destructor TSCIMObject.Destroy;
begin
  FJson.free;
  inherited;
end;

function TSCIMObject.GetCreated: TFslDateTime;
begin
  if FJson.has('meta') and FJson.obj['meta'].has('created') then
  begin
    if FCreated.null then
      FCreated := TFslDateTime.fromXML(FJson.obj['meta']['created']);
  end
  else if not FCreated.null then
    FCreated := TFslDateTime.makeNull;
  result := FCreated;
end;

function TSCIMObject.GetCreatedUTC: TFslDateTime;
begin
  if FCreated.null then
    result := FCreated
  else
    result := FCreated.UTC;
end;

function TSCIMObject.GetExternalId: String;
begin
  result := FJson['externalId'];
end;

function TSCIMObject.GetId: String;
begin
  result := FJson['id'];
end;

function TSCIMObject.GetLastModified: TFslDateTime;
begin
  if FJson.has('meta') and FJson.obj['meta'].has('created') then
  begin
    if FLastModified.null then
      FLastModified := TFslDateTime.fromXML(FJson.obj['meta']['created']);
  end
  else if not FLastModified.null then
    FLastModified := TFslDateTime.makeNull;
  result := FLastModified;
end;

function TSCIMObject.GetLastModifiedUTC: TFslDateTime;
begin
  if FLastModified.null then
    result := FLastModified
  else
    result := FLastModified.UTC;
end;

function TSCIMObject.GetLocation: String;
begin
  result := FJson.obj['meta']['location'];
end;

function TSCIMObject.GetResourceType: String;
begin
  result := FJson.obj['meta']['resourceType'];
end;

function TSCIMObject.GetVersion: String;
begin
  result := FJson.obj['meta']['version'];
end;

procedure TSCIMObject.SetCreated(const Value: TFslDateTime);
begin
  FCreated := Value;
  if not FJson.has('meta') then
    FJson.obj['meta'] := TJsonObject.Create;
  FJson.obj['meta']['created'] := FCreated.toXML;
end;

procedure TSCIMObject.SetExternalId(const Value: String);
begin
  if Value = '' then
  begin
    if FJson.has('externalId') then
      FJson.clear('externalId');
  end
  else
  begin
    FJson['externalId'] := Value;
  end;
end;

procedure TSCIMObject.SetId(const Value: String);
begin
  if Value = '' then
  begin
    if FJson.has('id') then
      FJson.clear('id');
  end
  else
  begin
    FJson['id'] := Value;
  end;
end;

procedure TSCIMObject.SetLastModified(const Value: TFslDateTime);
begin
  FLastModified := Value;
  if Value.null then
  begin
    if FJson.has('meta') and FJson.has('lastModified') then
      FJson.obj['meta'].clear('lastModified');
    if FJson.obj['meta'].properties.IsEmpty then
      FJson.clear('meta');
  end
  else
  begin
    if not FJson.has('meta') then
      FJson.obj['meta'] := TJsonObject.Create;
    FJson.obj['meta']['created'] := FLastModified.ToXML;
  end;
end;

procedure TSCIMObject.SetLocation(const Value: String);
begin
  if (value = '') then
  begin
    if FJson.has('meta') and FJson.has('location') then
      FJson.obj['meta'].clear('location');
    if FJson.obj['meta'].properties.IsEmpty then
      FJson.clear('meta');
  end
  else
  begin
    if not FJson.has('meta') then
      FJson.obj['meta'] := TJsonObject.Create;
    FJson.obj['meta']['location'] := Value;
  end;
end;

procedure TSCIMObject.SetResourceType(const Value: String);
begin
  if (value = '') then
  begin
    if FJson.has('meta') and FJson.has('resourceType') then
      FJson.obj['meta'].clear('resourceType');
    if FJson.obj['meta'].properties.IsEmpty then
      FJson.clear('meta');
  end
  else
  begin
    if not FJson.has('meta') then
      FJson.obj['meta'] := TJsonObject.Create;
    FJson.obj['meta']['resourceType'] := Value;
  end;
end;

procedure TSCIMObject.SetVersion(const Value: String);
begin
  if (value = '') then
  begin
    if FJson.has('meta') and FJson.has('version') then
      FJson.obj['meta'].clear('version');
    if FJson.obj['meta'].properties.IsEmpty then
      FJson.clear('meta');
  end
  else
  begin
    if not FJson.has('meta') then
      FJson.obj['meta'] := TJsonObject.Create;
    FJson.obj['meta']['version'] := Value;
  end;
end;

function TSCIMObject.hasSchema(s: String): boolean;
var
  i : integer;
  arr : TJsonArray;
begin
  result := false;
  arr := FJson.arr['schemas'];
  for i := 0 to arr.Count do
    result := result or (arr.Value[i] = s);
end;

procedure TSCIMObject.checkSchema(s: String);
begin
  if not hasSchema(s) then
    raise ESCIMException.Create(400, 'BAD REQUEST', 'invalidValue', 'Unable to find the expected schema '+s);
end;


function TSCIMObject.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FJson.sizeInBytes(magic));
end;

{ TSCIMUser }

function TSCIMUser.AddEmail(value, type_: String): TSCIMContact;
begin
  result := TSCIMContact.Create(FJson.forcearr['emails'].addObject.Link);
  Emails.Add(result);
  result.Value := value;
  result.Type_ := type_;
end;

procedure TSCIMUser.addEntitlement(value: String);
begin
  FJson.forcearr['entitlements'].add(value);
end;

function TSCIMUser.bestName: String;
begin
  result := DisplayName;
  if result = '' then
    result := formattedName;
  if result = '' then
    result := username;
end;

procedure TSCIMUser.check;
begin
  checkSchema('urn:scim:schemas:core:2.0:User');
end;


procedure TSCIMUser.clearEntitlements;
begin
  if FJson.has('entitlements') then
    FJson.clear('entitlements');
end;

procedure TSCIMUser.clearEmails;
begin
  if FJson.has('emails') then
    FJson.clear('emails');
end;

procedure TSCIMUser.copyFrom(source: TSCIMUser);
var
  n : string;
begin
  // copy from the source unless exists in the dest
  for n in source.FJson.properties.Keys do
    if not FJson.has(n) then
      FJson.properties.Add(n, source.FJson.properties[n].Link);

  // now delete anything in the dest that is null
  for n in source.FJson.properties.Keys do
    if FJson.properties[n] is TJsonNull then
      FJson.properties.Remove(n);
end;

constructor TSCIMUser.Create(FJson: TJsonObject);
begin
  inherited Create(FJson);
  if not hasSchema('urn:scim:schemas:core:2.0:User') then
    FJson.forceArr['schemas'].add('urn:scim:schemas:core:2.0:User');
end;

constructor TSCIMUser.CreateNew;
begin
  Create(TJsonObject.Create);
end;

destructor TSCIMUser.Destroy;
begin
  FEmails.free;
  FPhoneNums.free;
  FIMs.free;
  FAddresses.free;
  inherited;
end;

function TSCIMUser.GetAddresses: TSCIMAddressList;
var
  arr : TJsonArray;
  i : integer;
begin
  if FAddresses = nil then
  begin
    FAddresses := TSCIMAddressList.Create;
    if (FJson.has('addresses')) then
    begin
      arr := FJson.arr['addresses'];
      for i := 0 to arr.Count - 1 do
        FAddresses.add(TSCIMAddress.Create(arr.Obj[i].Link));
    end;
  end;
  result := FAddresses;
end;

function TSCIMUser.GetDisplayName: String;
begin
  result := FJson['displayName'];
end;

function TSCIMUser.GetEmails: TSCIMContactList;
var
  arr : TJsonArray;
  i : integer;
begin
  if FEmails = nil then
  begin
    FEmails := TSCIMContactList.Create;
    if (FJson.has('emails')) then
    begin
      arr := FJson.arr['emails'];
      for i := 0 to arr.Count - 1 do
        FEmails.add(TSCIMContact.Create(arr.Obj[i].Link));
    end;
  end;
  result := FEmails;
end;

function TSCIMUser.GetEntitlement(i: integer): String;
begin
  result := json.arr['entitlements'].Value[i];
end;

function TSCIMUser.GetEntitlementCount: integer;
begin
  if json.has('entitlements') then
    result := json.arr['entitlements'].Count
  else
    result := 0;
end;

function TSCIMUser.GetFamilyName: String;
begin
  result := FJson.obj['name']['familyName'];
end;

function TSCIMUser.GetGivenName: String;
begin
  result := FJson.obj['name']['givenName'];
end;

function TSCIMUser.GetIms: TSCIMContactList;
var
  arr : TJsonArray;
  i : integer;
begin
  if FIms = nil then
  begin
    FIMs := TSCIMContactList.Create;
    if (FJson.has('ims')) then
    begin
      arr := FJson.arr['ims'];
      for i := 0 to arr.Count - 1 do
        FIMs.add(TSCIMContact.Create(arr.Obj[i].Link));
    end;
  end;
  result := FIms;
end;

function TSCIMUser.GetLocale: String;
begin
  result := FJson['locale'];
end;

function TSCIMUser.GetMiddleName: String;
begin
  result := FJson.obj['name']['middleName'];
end;

function TSCIMUser.GetName: String;
begin
  result := FJson.obj['name']['formatted'];
end;

function TSCIMUser.GetNickName: String;
begin
  result := FJson['nickName'];
end;

function TSCIMUser.GetPassword: String;
begin
  result := FJson['password'];
end;

function TSCIMUser.GetPhoneNums: TSCIMContactList;
var
  arr : TJsonArray;
  i : integer;
begin
  if FPhoneNums = nil then
  begin
    FPhoneNums := TSCIMContactList.Create;
    if (FJson.has('phoneNums')) then
    begin
      arr := FJson.arr['phoneNums'];
      for i := 0 to arr.Count - 1 do
        FPhoneNums.add(TSCIMContact.Create(arr.Obj[i].Link));
    end;
  end;
  result := FPhoneNums;
end;

function TSCIMUser.GetPreferredLanguage: String;
begin
  result := FJson['preferredLanguage'];
end;

function TSCIMUser.GetPrefix: String;
begin
  result := FJson.obj['name']['honorificPrefix'];
end;

function TSCIMUser.GetProfileUrl: String;
begin
  result := FJson['profileUrl'];
end;

function TSCIMUser.GetSuffix: String;
begin
  result := FJson.obj['name']['honorificSuffix'];
end;

function TSCIMUser.GetTimezone: String;
begin
  result := FJson['timezone'];
end;

function TSCIMUser.GetTitle: String;
begin
  result := FJson['title'];
end;

function TSCIMUser.GetUsername: String;
begin
  result := FJson['userName'];
end;

function TSCIMUser.GetUserType: String;
begin
  result := FJson['userType'];
end;

function TSCIMUser.hasEmail(value: String): boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to emails.Count - 1 do
    if emails[i].Value = value then
      result := true;
end;

function TSCIMUser.hasEntitlement(value: String): Boolean;
var
  i : integer;
begin
  result := false;
  if entitlementCount > 0 then
    for i := 0 to json.arr['entitlements'].Count - 1 do
      result := result or (json.arr['entitlements'].Value[i] = value);
end;

function TSCIMUser.Link: TSCIMUser;
begin
  result := TSCIMUser(Inherited Link);
end;

procedure TSCIMUser.SetDisplayName(const Value: String);
begin
  if (value <> '') then
    FJson['displayName'] := value
  else if FJson.has('displayName') then
    FJson.clear('displayName');
end;


procedure TSCIMUser.SetFamilyName(const Value: String);
begin
  if (value = '') then
  begin
    if FJson.has('name') and FJson.has('familyName') then
      FJson.obj['name'].clear('familyName');
    if FJson.obj['name'].properties.IsEmpty then
      FJson.clear('name');
  end
  else
  begin
    if not FJson.has('name') then
      FJson.obj['name'] := TJsonObject.Create;
    FJson.obj['name']['familyName'] := Value;
  end;
end;

procedure TSCIMUser.SetGivenName(const Value: String);
begin
  if (value = '') then
  begin
    if FJson.has('name') and FJson.has('givenName') then
      FJson.obj['name'].clear('givenName');
    if FJson.obj['name'].properties.IsEmpty then
      FJson.clear('name');
  end
  else
  begin
    if not FJson.has('name') then
      FJson.obj['name'] := TJsonObject.Create;
    FJson.obj['name']['givenName'] := Value;
  end;
end;

procedure TSCIMUser.SetLocale(const Value: String);
begin
  if (value <> '') then
    FJson['locale'] := value
  else if FJson.has('locale') then
    FJson.clear('locale');
end;

procedure TSCIMUser.SetMiddleName(const Value: String);
begin
  if (value = '') then
  begin
    if FJson.has('name') and FJson.has('middleName') then
      FJson.obj['name'].clear('middleName');
    if FJson.obj['name'].properties.IsEmpty then
      FJson.clear('name');
  end
  else
  begin
    if not FJson.has('name') then
      FJson.obj['name'] := TJsonObject.Create;
    FJson.obj['name']['middleName'] := Value;
  end;
end;

procedure TSCIMUser.SetName(const Value: String);
begin
  if (value = '') then
  begin
    if FJson.has('name') and FJson.has('formatted') then
      FJson.obj['name'].clear('formatted');
    if FJson.obj['name'].properties.IsEmpty then
      FJson.clear('name');
  end
  else
  begin
    if not FJson.has('name') then
      FJson.obj['name'] := TJsonObject.Create;
    FJson.obj['name']['formatted'] := Value;
  end;
end;

procedure TSCIMUser.SetNickName(const Value: String);
begin
  if (value <> '') then
    FJson['nickName'] := value
  else if FJson.has('nickName') then
    FJson.clear('nickName');
end;

procedure TSCIMUser.SetPassword(const Value: String);
begin
  if (value <> '') then
    FJson['password'] := value
  else if FJson.has('password') then
    FJson.clear('password');
end;

procedure TSCIMUser.SetPreferredLanguage(const Value: String);
begin
  if (value <> '') then
    FJson['preferredLanguage'] := value
  else if FJson.has('preferredLanguage') then
    FJson.clear('preferredLanguage');
end;

procedure TSCIMUser.SetPrefix(const Value: String);
begin
  if (value = '') then
  begin
    if FJson.has('name') and FJson.has('honorificPrefix') then
      FJson.obj['name'].clear('honorificPrefix');
    if FJson.obj['name'].properties.IsEmpty then
      FJson.clear('name');
  end
  else
  begin
    if not FJson.has('name') then
      FJson.obj['name'] := TJsonObject.Create;
    FJson.obj['name']['familyName'] := Value;
  end;
end;

procedure TSCIMUser.SetProfileUrl(const Value: String);
begin
  if (value <> '') then
    FJson['profileUrl'] := value
  else if FJson.has('profileUrl') then
    FJson.clear('profileUrl');
end;

procedure TSCIMUser.SetSuffix(const Value: String);
begin
  if (value = '') then
  begin
    if FJson.has('name') and FJson.has('honorificSuffix') then
      FJson.obj['name'].clear('honorificSuffix');
    if FJson.obj['name'].properties.IsEmpty then
      FJson.clear('name');
  end
  else
  begin
    if not FJson.has('name') then
      FJson.obj['name'] := TJsonObject.Create;
    FJson.obj['name']['honorificSuffix'] := Value;
  end;
end;

procedure TSCIMUser.SetTimezone(const Value: String);
begin
  if (value <> '') then
    FJson['timezone'] := value
  else if FJson.has('timezone') then
    FJson.clear('timezone');
end;

procedure TSCIMUser.SetTitle(const Value: String);
begin
  if (value <> '') then
    FJson['title'] := value
  else if FJson.has('title') then
    FJson.clear('title');
end;

procedure TSCIMUser.SetUsername(const Value: String);
begin
  if (value <> '') then
    FJson['userName'] := value
  else if FJson.has('userName') then
    FJson.clear('userName');
end;

procedure TSCIMUser.SetUserType(const Value: String);
begin
  if (value <> '') then
    FJson['userType'] := value
  else if FJson.has('userType') then
    FJson.clear('userType');
end;

function TSCIMUser.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FEmails.sizeInBytes(magic));
  inc(result, FPhoneNums.sizeInBytes(magic));
  inc(result, FIMs.sizeInBytes(magic));
  inc(result, FAddresses.sizeInBytes(magic));
  inc(result, (FHash.length * sizeof(char)) + 12);
end;

{ TSCIMContact }

constructor TSCIMContact.Create(FJson: TJsonObject);
begin
  inherited Create;
  self.FJson := FJson;
end;

destructor TSCIMContact.Destroy;
begin
  FJson.free;
  inherited;
end;

function TSCIMContact.GetPrimary: Boolean;
begin
  result := FJson.bool['primary'];
end;

function TSCIMContact.GetType: String;
begin
  result := FJson['type'];
end;

function TSCIMContact.GetValue: String;
begin
  result := FJson['value'];
end;

procedure TSCIMContact.SetPrimary(const Value: Boolean);
begin
  FJson.bool['primary'] := value;
end;

procedure TSCIMContact.SetType(const Value: String);
begin
  if value <> '' then
    FJson['type'] := value
  else if FJson.has('type') then
    FJson.clear('type');
end;

procedure TSCIMContact.SetValue(const Value: String);
begin
  FJson['value'] := value;
end;

function TSCIMContact.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FJson.sizeInBytes(magic));
end;

{ TSCIMContactList }

function TSCIMContactList.GetContact(index: integer): TSCIMContact;
begin
  result := TSCIMContact(ObjectByindex[index]);
end;

function TSCIMContactList.ItemClass: TFslObjectClass;
begin
  result := TSCIMContact;
end;

{ TSCIMAddress }

constructor TSCIMAddress.Create(FJson: TJsonObject);
begin
  inherited Create;
  self.FJson := FJson;
end;

destructor TSCIMAddress.Destroy;
begin
  FJson.free;
  inherited;
end;

function TSCIMAddress.GetCountry: String;
begin
  result := FJson['country'];
end;

function TSCIMAddress.GetFormatted: String;
begin
  result := FJson['formatted'];
end;

function TSCIMAddress.GetLocality: String;
begin
  result := FJson['locality'];
end;

function TSCIMAddress.GetPostalCode: String;
begin
  result := FJson['postalCode'];
end;

function TSCIMAddress.GetPrimary: Boolean;
begin
  result := FJson.bool['primary'];
end;

function TSCIMAddress.GetRegion: String;
begin
  result := FJson['region'];
end;

function TSCIMAddress.GetStreetAddress: String;
begin
  result := FJson['streetAddress'];
end;

function TSCIMAddress.GetType: String;
begin
  result := FJson['type'];
end;

procedure TSCIMAddress.SetCountry(const Value: String);
begin
  FJson['country'] := value;
end;

procedure TSCIMAddress.SetFormatted(const Value: String);
begin
  FJson['formatted'] := value;
end;

procedure TSCIMAddress.SetLocality(const Value: String);
begin
  FJson['locality'] := value;
end;

procedure TSCIMAddress.SetPostalCode(const Value: String);
begin
  FJson['postalCode'] := value;
end;

procedure TSCIMAddress.SetPrimary(const Value: Boolean);
begin
  FJson.bool['primary'] := value;
end;

procedure TSCIMAddress.SetRegion(const Value: String);
begin
  FJson['region'] := value;
end;

procedure TSCIMAddress.SetStreetAddress(const Value: String);
begin
  FJson['streetAddress'] := value;
end;

procedure TSCIMAddress.SetType(const Value: String);
begin
  FJson['type'] := value;
end;

function TSCIMAddress.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FJson.sizeInBytes(magic));
end;

{ TSCIMAddressList }

function TSCIMAddressList.GetAddress(index: integer): TSCIMAddress;
begin
  result := TSCIMAddress(ObjectByindex[index]);
end;

function TSCIMAddressList.ItemClass: TFslObjectClass;
begin
  result := TSCIMAddress;
end;


{ ESCIMException }

constructor ESCIMException.Create(status: Integer; statusText, scimType, sMessage: String);
begin
  inherited Create(sMessage);
  FStatus := status;
  FStatusText := statusText;
  FScimType := scimType;
end;


end.

