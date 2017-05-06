unit SCIMStorageService;

interface

uses
  AdvObjects;

Type
  TSCIMStorageServiceConnection = class (TAdvObject)
  public
  end;

  TSCIMStorageService = class (TAdvObject)
  public
    function CheckId(id : String; var username, hash : String) : boolean; virtual;
    function CheckLogin(username, password : String) : boolean; virtual;
    procedure LoadKeys(var lastUserKey, lastUserIndexKey : integer); virtual;

  end;
implementation

{ TSCIMStorageService }

function TSCIMStorageService.CheckId(id: String; var username,
  hash: String): boolean;
begin

end;

function TSCIMStorageService.CheckLogin(username, password: String): boolean;
begin

end;

procedure TSCIMStorageService.LoadKeys(var lastUserKey,
  lastUserIndexKey: integer);
begin

end;

end.
