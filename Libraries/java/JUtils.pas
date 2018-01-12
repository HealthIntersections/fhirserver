{
Copyright (C) 1998-2001 Jonathan Revusky
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. All advertising materials mentioning features or use of this software
   must display the following acknowledgement:
     This product includes software developed by Jonathan Revusky
4. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

unit JUtils;

// Global utility routines




interface
   
   {$IFDEF FPC}
    uses Classes,Windows;
   {$ELSE}
   uses System.Classes, myUTF8Strings,WinAPI.Windows;
   {$endif}

{$IF Declared(PUTF8Char)} {$ELSE} type PUTF8char = PAnsiChar;
 {$IFEND}
  
// Outputs a message. Sends it to the console or
// to a GUI message window depending.
    
  function dotToSlash(const s : UTF8String) : UTF8String;
  function slashToDot(const s : UTF8String) : UTF8String;
          
  function ConvertStrings(Strings : TStrings) : Pointer;
    
//wrappers around the Win32 API calls.
  function getEnvironmentString(S : UTF8String) : UTF8String;
  procedure setEnvironmentString(key, value : UTF8String);
    
  // redeclared here because in D2, the prototype in Window.pas is incorrect.
  function SearchPath(lpPath, lpFileName, lpExtension: PUTF8Char; 
                          nBufferLength: DWORD; 
                          lpBuffer: PUTF8Char; 
                          var lpFilePart: PUTF8Char): DWORD; stdcall;
    
  //uses the above SearchPath routine to
  // find the file on path. Returns full path or empty string.
  function FindOnSystemPath(Filename : UTF8String) : UTF8String;
    
  // converts the dots or forward slashes to backslashes
  function toBackSlash(const s : UTF8String) : UTF8String; 

  procedure ChopExtension(var Filename : UTF8String);

implementation
    {$IFDEF FPC}
    uses SysUtils, JavaRuntime;
    {$ELSE}
    uses System.SysUtils, JavaRuntime;
   {$endif}

    
var
  Buf : array[0..1023] of UTF8Char;
    
{little routine to convert the dots to slashes for fully
qualified Class names.}
    
  function dotToSlash(const s : UTF8String) : UTF8String;
  var
    I: Integer;
  begin
    Result:= s;
    for I := 1 to length(Result) do
      if Result[I] = '.' then 
        Result[I] := '/';
  end;
    
  function slashToDot(const s: UTF8String) : UTF8String;
  var
    I : Integer;
  begin
    Result := s;
    for I :=  1 to length(Result) do
      if Result[I] = '/' then 
        Result[I] := '.';
  end;


  function toBackSlash(const s : UTF8String) : UTF8String;
  var
    I: Integer;
  begin
    Result:= S;
    for I := 1 to length(S) do
      if (Result[I] = '.') or (Result[I] = '/') then 
        Result[I] := '\';
  end;

{Convert a TStrings object to a null-terminated 
  sequence of pointers to PUTF8Char. }
    
  function ConvertStrings(Strings : TStrings) : Pointer;
  var   
    PPC : ^PUTF8Char;
    I : Integer;
   str:UTF8String;
  begin
    Result  := Nil;
    if Strings = Nil then 
      Exit;
    if Strings.Count =0 then 
      Exit;
    PPC  := allocMem((1 + Strings.Count) * sizeof(PUTF8Char));
    result := PPC;
    for I := 0 to  Strings.Count-1 do 
    begin
      PPC^ := PUTF8Char(UTF8String(Strings[I])); 
      inc(PPC);
    end;
  end;


   {Trivial wrapper of the SearchPath API call.}
    
  function FindOnSystemPath(Filename : UTF8String) : UTF8String;
  var
    PC : PUTF8Char;
  begin
    if SearchPath(Nil, PUTF8Char(Filename), Nil, MAX_PATH, @Buf, PC)<>0 then
    Result := UTF8String(Buf);
  end;
    
    
  function getEnvironmentString(S : UTF8String) : UTF8String;
  begin
   {$IFDEF FPC}
   result:=getEnvironmentVariable(S)
   {$ELSE}
   if getEnvironmentVariable(Pchar(S), @Buf, 1023) >0 then
      result := UTF8String(Buf);
   {$endif}
   end;
    
  procedure SetEnvironmentString(key, value : UTF8String);
  begin
  
    SetEnvironmentVariable(PChar(key), PChar(value));
  end;

  procedure ChopExtension(var Filename : UTF8String);
  var
    Ext : UTF8String;
  begin
   {$IFDEF FPC}
    Ext := ExtractFileExt(Filename);
   {$ELSE}
    Ext := myUTF8Strings.ExtractFileExt(Filename);
   {$endif}

    Filename := Copy(Filename, 1, Length(Filename) - Length(Ext));
  end;

    
  function SearchPath; external kernel32 name 'SearchPathA';

end.
