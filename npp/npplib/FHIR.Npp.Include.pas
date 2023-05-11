{
    This file is part of DBGP Plugin for Notepad++
    Copyright (C) 2007  Damjan Zobo Cvetko

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

procedure DLLEntryPoint(dwReason: DWord);
begin
  case dwReason of
  DLL_PROCESS_ATTACH:
  begin
    // create the main object
    //Npp := TDbgpNppPlugin.Create;
  end;
  DLL_PROCESS_DETACH:
  begin
    if (Assigned(FNpp)) then FNpp.Destroy;
  end;
  //DLL_THREAD_ATTACH: MessageBeep(0);
  //DLL_THREAD_DETACH: MessageBeep(0);
  end;
end;

procedure setInfo(NppData: TNppData); cdecl; export;
begin
  FNpp.SetInfo(NppData);
end;

function getName(): nppPchar; cdecl; export;
begin
  Result := FNpp.GetName;
end;

function getFuncsArray(var nFuncs:integer):Pointer;cdecl; export;
begin
  Result := FNpp.GetFuncsArray(nFuncs);
end;

procedure beNotified(sn: PSCNotification); cdecl; export;
begin
  FNpp.BeNotified(sn);
end;

function messageProc(msg: Integer; _wParam: WPARAM; _lParam: LPARAM): LRESULT; cdecl; export;
var xmsg:TMessage;
begin
  xmsg.Msg := msg;
  xmsg.WParam := _wParam;
  xmsg.LParam := _lParam;
  xmsg.Result := 0;
  FNpp.MessageProc(xmsg);
  Result := xmsg.Result;
end;

{$IFDEF NPPUNICODE}
function isUnicode : Boolean; cdecl; export;
begin
  Result := true;
end;
{$ENDIF}

exports
  setInfo, getName, getFuncsArray, beNotified, messageProc;
{$IFDEF NPPUNICODE}
exports
  isUnicode;
{$ENDIF}
