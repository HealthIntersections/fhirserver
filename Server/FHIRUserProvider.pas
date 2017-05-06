unit FHIRUserProvider;

interface

uses
  SysUtils,
  AdvObjects,
  IdContext, IdCustomHTTPServer,
  FHIRSupport, SCIMObjects,
  ServerUtilities;

Const
  SCIM_ANONYMOUS_USER = 'ANONYMOUS';
  SCIM_SYSTEM_USER = 'SYSTEM';

type
  TFHIRUserProvider = class (TAdvObject)
  private
    FOnProcessFile : TProcessFileEvent;
  public
    function Link :  TFHIRUserProvider; overload;

    Function loadUser(key : integer) : TSCIMUser; overload; virtual;
    Function loadUser(id : String; var key : integer) : TSCIMUser; overload; virtual;
    function CheckLogin(username, password : String) : boolean; virtual;
    function CheckId(id : String; var username, hash : String) : boolean; virtual;
    function loadOrCreateUser(id, name, email : String; var key : integer) : TSCIMUser; virtual;

    Procedure processRequest(context: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFHIRSession); virtual;
    property OnProcessFile : TProcessFileEvent read FOnProcessFile write FOnProcessFile;
  end;

implementation

{ TFHIRUserProvider }

function TFHIRUserProvider.Link: TFHIRUserProvider;
begin
  result := TFHIRUserProvider(inherited Link);
end;

function TFHIRUserProvider.CheckId(id: String; var username, hash: String): boolean;
begin
  raise Exception.Create('TFHIRUserProvider.CheckId must be override in '+className);
end;

function TFHIRUserProvider.CheckLogin(username, password: String): boolean;
begin
  raise Exception.Create('TFHIRUserProvider.CheckLogin must be override in '+className);
end;

function TFHIRUserProvider.loadUser(key: integer): TSCIMUser;
begin
  raise Exception.Create('TFHIRUserProvider.loadUser must be override in '+className);
end;

function TFHIRUserProvider.loadOrCreateUser(id, name, email: String; var key: integer): TSCIMUser;
begin
  raise Exception.Create('TFHIRUserProvider.loadOrCreateUser must be override in '+className);
end;

function TFHIRUserProvider.loadUser(id: String; var key: integer): TSCIMUser;
begin
  raise Exception.Create('TFHIRUserProvider.loadUser must be override in '+className);
end;

procedure TFHIRUserProvider.processRequest(context: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session: TFHIRSession);
begin
  raise Exception.Create('User Management is not supported on this server');
end;

end.

