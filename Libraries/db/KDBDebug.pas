// debugging services for KDBManager
// backdoor
unit KDBDebug;


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

implementation

uses
  Backdoor,
  KBase,
  KDBLogging,
  KDBManager,
  KDBHtmlSupport,
  KProcs,
  SysUtils;

type
  TKDBBackdoor = class (TBaseObject)
  private
    function RunSQLQuery(AConnMan : TKDBManager; ACommand, AURL : String):String;
    procedure DbgDatabase(const ACommand, AURL: String; var VResult: String);
    procedure ConnListChange(AConnMan : TKDBManager; ABeingCreated : Boolean);
  public
    constructor create;
    destructor destroy; override;
  end;

{ TKDBBackdoor }

constructor TKDBBackdoor.create;
var
  i : integer;
begin
  inherited;
  KDBManagers.RegisterHook('KDBDebug', ConnListChange);
  for i := 0 to KDBManagers.Count - 1 do
    begin
    ConnListChange(KDBManagers.ConnMan[i], true);
    end;
end;

destructor TKDBBackdoor.destroy;
begin
  KDBManagers.UnRegisterHook('KDBDebug');
  inherited;
end;

procedure TKDBBackdoor.ConnListChange(AConnMan : TKDBManager; ABeingCreated : Boolean);
begin
  AConnMan.Tag := SysIDbg.RegisterCommand('KDBManager', AConnMan.Name, 'KDBManager: '+AConnMan.Name, DBG_SEC_ANONYMOUS, DbgDatabase);
end;

function TKDBBackdoor.RunSQLQuery(AConnMan : TKDBManager; ACommand, AURL : String):String;
var
  LSql : String;
  LConn : TKDBConnection;
begin
  split(ACommand, '=', ACommand, LSQL);
  LSQL := MimeDecode(LSql);
  // extract sql....
  if LSql = '' then
    begin
    result := '<form action="'+AURL+'" method="get"><input type="text" size=80 name="sql"><br><input type="submit"></form>';
    end
  else
    begin
    LConn := AConnMan.GetConnection('backdoor');
    try
      result := DirectSQLInHTML(LConn, LSQL);
      LConn.Release;
    except
      on e:exception do
        begin
        LConn.Error(e);
        result := e.message;
        end;
    end;
    end;
end;

procedure TKDBBackdoor.DbgDatabase(const ACommand, AURL: String; var VResult: String);
var
  LConnMan : TKDBManager;
  s, t : String;
begin
  LConnMan := KDBManagers[GetStringCell(AURL, 2, '/')];
  if ACommand = '' then
    begin
    VResult :=
       'KDBManager: '+LConnMan.DBDetails+':<br>'+
       '<a href="'+AURL+'/use">Current Use</a><br>'+
       '<a href="'+AURL+'/stats">Stats</a><br>'+
       '<a href="'+AURL+'/sql">SQL</a><br>';
    end
  else
    begin
    if SameText(ACommand, 'use') then
      begin
      VResult := LConnMan.GetConnSummary;
      end
    else if SameText(copy(ACommand, 1, 5), 'stats') then
      begin
      split(ACommand, '/', t, s);
      VResult := LConnMan.Logger.InteractiveReport(MimeDecode(s), AUrl+'/stats');
      end
    else if SameText(copy(ACommand, 1, 3), 'sql') then
      begin
      VResult := RunSQLQuery(LConnMan, ACommand, AUrl);
      end
    else
      begin
      VResult := 'Unknown command '+ACommand;
      end;
    end;
end;

var
  GKDBBackdoor : TKDBBackdoor;

initialization
  GKDBBackdoor := TKDBBackdoor.create;
finalization
  FreeAndNil(GKDBBackdoor);
end.
