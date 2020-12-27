{
Copyright (C) 1998+ Jonathan Revusky
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

{$I fhir.inc}

unit fsl_java_utilities;

// Global utility routines




interface
   
uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  Classes,
  fsl_java_strings;


{$IF Declared(PUTF8Char)} {$ELSE} type PUTF8char = PAnsiChar;
 {$IFEND}
  
// Outputs a message. Sends it to the console or
// to a GUI message window depending.
    
  function dotToSlash(const s : String) : String;
  function slashToDot(const s : String) : String;
          
  function ConvertStrings(Strings : TStrings) : Pointer;
    
//wrappers around the Win32 API calls.
  function getEnvironmentString(S : String) : String;
  procedure setEnvironmentString(key, value : String);
    
  // redeclared here because in D2, the prototype in Window.pas is incorrect.
  {$IFDEF WINDOWS}
  function SearchPath(lpPath, lpFileName, lpExtension: PUTF8Char; 
                          nBufferLength: DWORD; 
                          lpBuffer: PUTF8Char; 
                          var lpFilePart: PUTF8Char): DWORD; stdcall;
  {$ENDIF}
    
  //uses the above SearchPath routine to
  // find the file on path. Returns full path or empty string.
  {$IFDEF WINDOWS}
  function FindOnSystemPath(Filename : String) : String;
  {$ENDIF}
    
  // converts the dots or forward slashes to backslashes
  function toBackSlash(const s : String) : String;

  procedure ChopExtension(var Filename : String);

implementation

uses 
  SysUtils, fsl_java_runtime;
  
    
var
  Buf : array[0..1023] of UTF8Char;
    
{little routine to convert the dots to slashes for fully
qualified Class names.}
    
  function dotToSlash(const s : String) : String;
  var
    I: Integer;
  begin
    Result:= s;
    for I := 1 to length(Result) do
      if Result[I] = '.' then
        Result[I] := '/';
  end;

  function slashToDot(const s: String) : String;
  var
    I : Integer;
  begin
    Result := s;
    for I :=  1 to length(Result) do
      if Result[I] = '/' then
        Result[I] := '.';
  end;


  function toBackSlash(const s : String) : String;
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
  {$IFDEF WINDOWS}
  function FindOnSystemPath(Filename : String) : String;
  var
    p : UTF8String;
    PC : PUTF8Char;
  begin
    p := fsl_java_strings.StrNew(filename);
    if SearchPath(Nil, PUTF8Char(p), Nil, MAX_PATH, @Buf, PC)<>0 then
    Result := String(Buf);
  end;
  {$ENDIF}
    
    
  function getEnvironmentString(S : String) : String;
  begin
   {$IFDEF FPC}
   result:=getEnvironmentVariable(S)
   {$ELSE}
   if getEnvironmentVariable(Pchar(S), @Buf, 1023) >0 then
      result := String(Buf);
   {$endif}
   end;
    
  procedure SetEnvironmentString(key, value : String);
  begin
    {$IFDEF WINDOWS}
    SetEnvironmentVariable(PChar(key), PChar(value));
    {$ENDIF}
  end;

  procedure ChopExtension(var Filename : String);
  var
    Ext : String;
  begin
   {$IFDEF FPC}
    Ext := ExtractFileExt(Filename);
   {$ELSE}
    Ext := FHIR.Java.Strings.ExtractFileExt(Filename);
   {$endif}

    Filename := Copy(Filename, 1, Length(Filename) - Length(Ext));
  end;


  {$IFDEF WINDOWS}
  function SearchPath; external kernel32 name 'SearchPathA';
  {$ENDIF}

end.
