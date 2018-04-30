Unit FHIR.Support.System;

{
Copyright (c) 2001-2013, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

Interface

Uses
  {$IFDEF MACOS} FHIR.Support.Osx, MacApi.Foundation, {$ELSE} Windows, ShellApi, ShlObj, {$ENDIF}
  SysUtils, Classes, Generics.Collections,
  FHIR.Support.Strings, FHIR.Support.Math, FHIR.Support.DateTime;


Function SystemTemp : String;
Function SystemManualTemp : String;
Function ProgData : String;


Type
  TInstallerCallback = procedure(IntParam: Integer; StrParam: String) of object;
  TThreadID = Cardinal;
  TThreadHandle = THandle;


Procedure ThreadSleep(iTime : Cardinal); Overload;
Function ThreadID : TThreadID; Overload;
{$IFDEF MSWINDOWS}
Function ThreadHandle : TThreadHandle; Overload;
{$ENDIF}
Procedure ThreadYield; Overload;
Procedure ThreadBreakpoint; Overload;

procedure SetThreadName(name : String);
function GetThreadName(id : integer) : String;

type
TColour = integer;
THTMLColours = (
      hcAliceblue, hcAntiquewhite, hcAqua, hcAquamarine, hcAzure,
      hcBeige, hcBisque, hcBlack, hcBlanchedalmond, hcBlue,
      hcBlueviolet, hcBrown, hcBurlywood, hcCadetblue, hcChartreuse,
      hcChocolate, hcCoral, hcCornflowerblue, hcCornsilk, hcCrimson,
      hcCyan, hcDarkblue, hcDarkcyan, hcDarkgoldenrod, hcDarkgray,
      hcDarkgreen, hcDarkkhaki, hcDarkmagenta, hcDarkolivegreen, hcDarkorange,
      hcDarkorchid, hcDarkred, hcDarksalmon, hcDarkseagreen, hcDarkslateblue,
      hcDarkslategray, hcDarkturquoise, hcDarkviolet, hcdeeppink, hcDeepskyblue,
      hcDimgray, hcDodgerblue, hcFirebrick, hcFloralwhite, hcForestgreen,
      hcFuchsia, hcGainsboro, hcGhostwhite, hcGold, hcGoldenrod,
      hcGray, hcGreen, hcGreenyellow, hcHoneydew, hcHotpink,
      hcIndianred, hcIndigo, hcIvory, hcKhaki, hcLavendar,
      hcLavenderblush, hcLawngreen, hcLemonchiffon, hcLightblue, hcLightcoral,
      hcLightcyan, hcLightgoldenrodyellow, hcLightgreen, hcLightgrey, hcLightpink,
      hcLightsalmon, hcLightseagreen, hcLightskyblue, hcLightslategray, hcLightsteelblue,
      hcLightyellow, hcLime, hcLimegreen, hcLinen, hcMagenta,
      hcMaroon, hcMediumauqamarine, hcMediumblue, hcMediumorchid, hcMediumpurple,
      hcMediumseagreen, hcMediumslateblue, hcMediumspringgreen, hcMediumturquoise, hcMediumvioletred,
      hcMidnightblue, hcMintcream, hcMistyrose, hcMoccasin, hcNavajowhite,
      hcNavy, hcOldlace, hcOlive, hcOlivedrab, hcOrange,
      hcOrangered, hcOrchid, hcPalegoldenrod, hcPalegreen, hcPaleturquoise,
      hcPalevioletred, hcPapayawhip, hcPeachpuff, hcPeru, hcPink,
      hcPlum, hcPowderblue, hcPurple, hcRed, hcRosybrown,
      hcRoyalblue, hcSaddlebrown, hcSalmon, hcSandybrown, hcSeagreen,
      hcSeashell, hcSienna, hcSilver, hcSkyblue, hcSlateblue,
      hcSlategray, hcSnow, hcSpringgreen, hcSteelblue, hcTan,
      hcTeal, hcThistle, hcTomato, hcTurquoise, hcViolet,
      hcWheat, hcWhite, hcWhitesmoke, hcYellow, hcYellowGreen);

Const
  CURRENCY_MINIMUM = -922337203685477.58;
  CURRENCY_MAXIMUM = 922337203685477.58;


  HTML_COLOUR_VALUES : Array [THTMLColours] Of TColour = (
      $00FFF8F0, $00D7EBFA, $00FFFF00, $00D4FF7F, $00FFFFF0,
      $00DCF5F5, $00C4E4FF, $00000000, $00CDEBFF, $00FF0000,
      $00E22B8A, $002A2AA5, $0087B8DE, $00A09E5F, $0000FF7F,
      $001E69D2, $00507FFF, $00ED9564, $00DCF8FF, $003C14DC,
      $00FFFF00, $008B0000, $008B8B00, $000B86B8, $00A9A9A9,
      $00006400, $006BB7BD, $008B008B, $002F6B55, $00008CFF,
      $00CC3299, $0000008B, $007A96E9, $008FBC8F, $008B3D48,
      $004F4F2F, $00D1CE00, $00D30094, $009314FF, $00FFBF00,
      $00696969, $00FF901E, $002222B2, $00F0FAFF, $00228B22,
      $00FF00FF, $00DCDCDC, $00FFF8F8, $0000D7FF, $0020A5DA,
      $00808080, $00008000, $002FFFAD, $00F0FFF0, $00B469FF,
      $005C5CCD, $0082004B, $00F0FFFF, $008CE6F0, $00FAE6E6,
      $00F5F0FF, $0000FC7C, $00CDFAFF, $00E6D8AD, $008080F0,
      $00FFFFE0, $00D2FAFA, $0090EE90, $00D3D3D3, $00C1B6FF,
      $007AA0FF, $00AAB220, $00FACE87, $00998877, $00DEC4B0,
      $00E0FFFF, $0000FF00, $0032CD32, $00E6F0FA, $00FF00FF,
      $00000080, $00AACD66, $00CD0000, $00D355BA, $00D87093,
      $0071B33C, $00EE687B, $009AFA00, $00CCD148, $008515C7,
      $00701919, $00FAFFF5, $00E1E4FF, $00B5E4FF, $00ADDEFF,
      $00800000, $00E6F5FD, $00008080, $00238E68, $0000A5FF,
      $000045FF, $00D670DA, $00AAE8EE, $0098FB98, $00EEEEAF,
      $009370D8, $00D5EFFF, $00B9DAFF, $003F85CD, $00CBC0FF,
      $00DDA0DD, $00E6E0B0, $00800080, $000000FF, $008F8FBC,
      $00E16941, $0013458B, $007280FA, $0060A4F4, $00578B2E,
      $00EEF5FF, $002D52A0, $00C0C0C0, $00EBCE87, $00CD5A6A,
      $00908070, $00FAFAFF, $007FFF00, $00B48246, $008CB4D2,
      $00808000, $00D8BFD8, $004763FF, $00D0E040, $00EE82EE,
      $00B3DEF5, $00FFFFFF, $00F5F5F5, $0000FFFF, $0032CD9A);

  HTML_COLOUR_NAMES : Array [THTMLColours] Of String = (
      'Aliceblue', 'Antiquewhite', 'Aqua', 'Aquamarine', 'Azure',
      'Beige', 'Bisque', 'Black', 'Blanchedalmond', 'Blue',
      'Blueviolet', 'Brown', 'Burlywood', 'Cadetblue', 'Chartreuse',
      'Chocolate', 'Coral', 'Cornflowerblue', 'Cornsilk', 'Crimson',
      'Cyan', 'Darkblue', 'Darkcyan', 'Darkgoldenrod', 'Darkgray',
      'Darkgreen', 'Darkkhaki', 'Darkmagenta', 'Darkolivegreen', 'Darkorange',
      'Darkorchid', 'Darkred', 'Darksalmon', 'Darkseagreen', 'Darkslateblue',
      'Darkslategray', 'Darkturquoise', 'Darkviolet', 'deeppink', 'Deepskyblue',
      'Dimgray', 'Dodgerblue', 'Firebrick', 'Floralwhite', 'Forestgreen',
      'Fuchsia', 'Gainsboro', 'Ghostwhite', 'Gold', 'Goldenrod',
      'Gray', 'Green', 'Greenyellow', 'Honeydew', 'Hotpink',
      'Indianred', 'Indigo', 'Ivory', 'Khaki', 'Lavendar',
      'Lavenderblush', 'Lawngreen', 'Lemonchiffon', 'Lightblue', 'Lightcoral',
      'Lightcyan', 'Lightgoldenrodyellow', 'Lightgreen', 'Lightgrey', 'Lightpink',
      'Lightsalmon', 'Lightseagreen', 'Lightskyblue', 'Lightslategray', 'Lightsteelblue',
      'Lightyellow', 'Lime', 'Limegreen', 'Linen', 'Magenta',
      'Maroon', 'Mediumauqamarine', 'Mediumblue', 'Mediumorchid', 'Mediumpurple',
      'Mediumseagreen', 'Mediumslateblue', 'Mediumspringgreen', 'Mediumturquoise', 'Mediumvioletred',
      'Midnightblue', 'Mintcream', 'Mistyrose', 'Moccasin', 'Navajowhite',
      'Navy', 'Oldlace', 'Olive', 'Olivedrab', 'Orange',
      'Orangered', 'Orchid', 'Palegoldenrod', 'Palegreen', 'Paleturquoise',
      'Palevioletred', 'Papayawhip', 'Peachpuff', 'Peru', 'Pink',
      'Plum', 'Powderblue', 'Purple', 'Red', 'Rosybrown',
      'Royalblue', 'Saddlebrown', 'Salmon', 'Sandybrown', 'Seagreen',
      'Seashell', 'Sienna', 'Silver', 'Skyblue', 'Slateblue',
      'Slategray', 'Snow', 'Springgreen', 'Steelblue', 'Tan',
      'Teal', 'Thistle', 'Tomato', 'Turquoise', 'Violet',
      'Wheat', 'White', 'Whitesmoke', 'Yellow', 'YellowGreen');

  HTML_COLOUR_TITLES : Array [THTMLColours] Of String = (
      'Alice Blue', 'Antique White', 'Aqua', 'Aquamarine', 'Azure',
      'Beige', 'Bisque', 'Black', 'Blanched Almond', 'Blue',
      'Blue Violet', 'Brown', 'Burlywood', 'Cadet Blue', 'Chartreuse',
      'Chocolate', 'Coral', 'Cornflower Blue', 'Cornsilk', 'Crimson',
      'Cyan', 'Dark Blue', 'Dark Cyan', 'Dark Goldenrod', 'Dark Gray',
      'Dark Green', 'Dark Khaki', 'Dark Magenta', 'Dark Olive Green', 'Dark Orange',
      'Dark Orchid', 'Dark Red', 'Dark Salmon', 'Dark Sea Green', 'Dark Slate Blue',
      'Dark Slate Gray', 'Dark Turquoise', 'Dark Violet', 'Deep Pink', 'Deep Sky Blue',
      'Dim Gray', 'Dodger Blue', 'Firebrick', 'Floral White', 'Forest Green',
      'Fuchsia', 'Gainsboro', 'Ghost White', 'Gold', 'Goldenrod',
      'Gray', 'Green', 'Green Yellow', 'Honeydew', 'Hot Pink',
      'Indian Red', 'Indigo', 'Ivory', 'Khaki', 'Lavendar',
      'Lavender Blush', 'Lawn Green', 'Lemon Chiffon', 'Light Blue', 'Light Coral',
      'Light Cyan', 'Light Goldenrod Yellow', 'Light Green', 'Light Grey', 'Light Pink',
      'Light Salmon', 'Light Sea Green', 'Light Sky Blue', 'Light Slate Gray', 'Light Steel Blue',
      'Light Yellow', 'Lime', 'Lime Green', 'Linen', 'Magenta',
      'Maroon', 'Medium Aquamarine', 'Medium Blue', 'Medium Orchid', 'Medium Purple',
      'Medium Seagreen', 'Medium Slate Blue', 'Medium Spring Green', 'Medium Turquoise', 'Medium Violet Red',
      'Midnight Blue', 'Mint Cream', 'Misty Rose', 'Moccasin', 'Navajo White',
      'Navy', 'Old Lace', 'Olive', 'Olive Drab', 'Orange',
      'Orange Red', 'Orchid', 'Pale Goldenrod', 'Pale Green', 'Pale Turquoise',
      'Pale Violet Red', 'Papaya Whip', 'Peach Puff', 'Peru', 'Pink',
      'Plum', 'Powder Blue', 'Purple', 'Red', 'Rosy Brown',
      'Royal Blue', 'Saddle Brown', 'Salmon', 'Sandy Brown', 'Sea Green',
      'Seashell', 'Sienna', 'Silver', 'Sky Blue', 'Slate Blue',
      'Slate Gray', 'Snow', 'Spring Green', 'Steel Blue', 'Tan',
      'Teal', 'Thistle', 'Tomato', 'Turquoise', 'Violet',
      'Wheat', 'White', 'White Smoke', 'Yellow', 'Yellow Green');

Type
  TCurrency = Currency;

  TColourParts = Packed Record
    Red : Byte;
    Green : Byte;
    Blue : Byte;
    Alpha : Byte;
  End;

Function ColourCompose(iRed, iGreen, iBlue, iAlpha : Byte) : TColour; Overload;
Function HTMLColourStringToColour(Const sColour : String; Const aDefault : TColour) : TColour; Overload;
Function HTMLColourStringToColour(Const sColour : String) : TColour; Overload;
Function StringIsHTMLColour(Const sColour : String) : Boolean; Overload;
Function HTMLEncodedColourStringToColour(Const sColour : String) : TColour; Overload;
Function HTMLEncodedColourStringToColour(Const sColour : String; Const aDefault : TColour) : TColour; Overload;
Function ColourToHTMLColourString(Const iColour : TColour) : String; Overload;
Function ColourToHTMLColourTitleString(Const iColour : TColour) : String; Overload;


type
  TFileHandle = Record
    Value : Cardinal;
  End;

  TFileVersion = Record
    Major : Integer;
    Minor : Integer;
    Release : Integer;
    Build : Integer;
  End;

Function FileGetModified(Const sFileName : String) : TDateTime; Overload;

Function FileExists(Const sFilename : String) : Boolean; Overload;
Function FileDelete(Const sFilename : String) : Boolean; Overload;
Function FileHandleInvalid : TFileHandle; Overload;
Function FileHandleIsValid(Const aFileHandle : TFileHandle) : Boolean; Overload;
Function FileHandleOpen(Const aValue : Cardinal) : TFileHandle; Overload;
Procedure FileHandleClose(Var aFileHandle : TFileHandle); Overload;
Function PathFolder(Const sFilename : String) : String; Overload;
Function ForceFolder(dir: String): Boolean;

Function PathFilename(Const sFilename : String) : String; Overload;
Function PathTitle(Const sFilename : String) : String; Overload;
Function PathExtension(Const sFilename : String) : String; Overload;
Function FolderExists(Const sFolder : String) : Boolean;

Function FileSize(Const sFileName : String) : Int64; Overload;
function Path(parts : array of String) : String;
function URLPath(parts : array of String) : String;


Function CreateGUID : TGUID;
Function GUIDToString(Const aGUID : TGUID) : String;
Function StringToGUID(Const sGUID: String) : TGUID;
Function NilGUID : TGUID;
Function IsNilGUID(const guid : TGUID) : boolean;
Function GUIDAsOID(Const aGUID : TGUID) : String;
Function IsGuid(s : String): Boolean;
function NewGuidURN : String;
function NewGuidId : String;


Type
  TAnsiCharSet = set of AnsiChar;
// Always pass the pointer
Procedure MemoryCreate(Var pBuffer; iSize : Integer);
Procedure MemoryResize(Var pBuffer; iOldSize, iNewSize : Integer);
Procedure MemoryDestroy(Var pBuffer; iSize : Integer);
Function MemoryCompare(pA, pB : Pointer; iLength : Integer) : Integer;
Procedure MemoryZero(Const pBuffer : Pointer; iSize : Integer);
Procedure MemoryFill(Const pBuffer : Pointer; iSize : Integer);
Procedure MemoryMove(Const aSource, aTarget : Pointer; iSize : Integer);
Function MemoryToString(pData : Pointer; iPosition, iLength : Integer) : AnsiString; Overload;
Function MemoryToString(pData : Pointer; iLength : Integer) : AnsiString; Overload;


Function HashStringToCode32(Const sValue : String) : Integer;
Function HashStringToCode64(Const sValue : String) : Int64;
Function HashIntegerToCode32(Const iValue : Integer) : Integer;
Function HashInteger64ToCode32(Const iValue : Int64) : Integer;


Function ErrorAsString : String; Overload;
Function ErrorAsString(iError : Integer) : String; Overload;
Function ErrorAsMessage(iError : Integer) : String;
Function ErrorAsNumber : Integer; Overload;

Implementation

Uses
  {$IFDEF MACOS}
  FHIR.Support.Osx,
  {$ELSE}
  ActiveX,
  ComObj,
  {$ENDIF}
  FHIR.Support.Decimal;


Procedure ThreadSleep(iTime : Cardinal);
Begin
  Sleep(iTime);
End;

Function ThreadID : TThreadID;
Begin
  Result := GetCurrentThreadID;
End;


{$IFDEF MSWINDOWS}
Function ThreadHandle : TThreadHandle;
Begin
  Result := GetCurrentThread;
End;
{$ENDIF}



Procedure ThreadYield;
Begin
  ThreadSleep(0);
End;


Procedure ThreadBreakpoint;
Begin
  {$IFDEF WIN32}
  Try
    ASM
      int $03
    End;
  Except
    // on some poorly configured Windows systems int $03 can cause unhandled
    // exceptions with improperly installed Dr Watsons etc....
  End;
  {$ELSE}
  // todo: how to do this?
  {$ENDIF}
End;

Function FileGetModified(Const sFileName : String) : TDateTime;
var
  info : TDateTimeInfoRec;
begin
  if FileGetDateTimeInfo(sFilename, info) then
    result := info.TimeStamp
  else
    result := 0;
end;



Type
  TLargeInteger = Record
    Low : Cardinal;
    High : Cardinal;
  End;

  PLargeInteger = ^TLargeInteger;


Function PathFolder(Const sFilename : String) : String;
Var
  iIndex : Integer;
Begin
  iIndex := LastDelimiter('\:', sFilename);

  If (iIndex > 1) And (sFilename[iIndex] = '\:') And
     (Not CharInSet(sFilename[iIndex - 1], ['\', ':']) Or
     (ByteType(sFilename, iIndex - 1) = mbTrailByte)) Then
    Dec(iIndex);

  Result := StringIncludeAfter(Copy(sFilename, 1, iIndex), '\');
End;

Function FileExists(Const sFilename : String) : Boolean;
Begin
  result := SysUtils.FileExists(sFilename);
End;

Function FileDelete(Const sFilename : String) : Boolean;
Begin
  {$IFDEF MACOS}
  if FileIsReadOnly(sFileName) then
    result := false
  else
  {$ENDIF}
  Result := SysUtils.DeleteFile(sFilename);
End;


Function PathExtension(Const sFilename : String) : String;
Begin
  // Return the extension including the '.'.  Eg. PathExtension('notepad.exe') = '.exe'

  Result := SysUtils.ExtractFileExt(sFilename);
End;


Function PathFilename(Const sFilename : String) : String;
Begin
  Result := Copy(sFileName, LastDelimiter('\/:', sFileName) + 1, MaxInt);
End;

Function PathTitle(Const sFilename : String) : String;
Begin
  // Return the filename without the path or the extension.

  Result := PathFilename(sFilename);

  Result := Copy(Result, 1, Length(Result) - Length(PathExtension(sFilename)));
End;


Function FileSize(Const sFileName : String) : Int64;
{$IFDEF MACOS}
var
  f : TFileStream;
begin
  f := TFileStream.create(sFileName, fmOpenRead);
  try
    result := f.size;
  finally
    f.free;
  end;
end;
{$ELSE}
Var
  pResult : PLargeInteger;
  aHandle : TFileHandle;
Begin
  aHandle := FileHandleOpen(CreateFile(PChar(sFileName), GENERIC_READ, FILE_SHARE_READ Or FILE_SHARE_WRITE, Nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0));
  Try
    If FileHandleIsValid(aHandle) Then
    Begin
      pResult := PLargeInteger(@Result);

      pResult^.Low := GetFileSize(aHandle.Value, @pResult^.High);

      If pResult^.Low = FileHandleInvalid.Value Then
        Result := 0;
    End
    Else
    Begin
      Result := 0;
    End;
  Finally
    FileHandleClose(aHandle);
  End;
End;
{$ENDIF}

Function FolderExists(Const sFolder : String) : Boolean;
Begin
  result := SysUtils.DirectoryExists(sFolder, false);
End;


Function last(Const s: String; c: Char): Cardinal;
Var
  i: Word;
Begin
  i := Length(s);
  Result := 0;
  While (i > 0) Do
    Begin
    If s[i] = c Then
      Begin
      Result := i;
      Exit;
      End;
    Dec(i);
    End;
End;

Function ForceFolder(dir: String): Boolean;
begin
  result := SysUtils.ForceDirectories(dir);
end;

Function FileHandleInvalid : TFileHandle;
Begin
  Result.Value := $FFFFFFFF;
End;


Function FileHandleIsValid(Const aFileHandle : TFileHandle) : Boolean;
Begin
  Result := aFileHandle.Value <> FileHandleInvalid.Value;
End;

Function FileHandleOpen(Const aValue : Cardinal) : TFileHandle;
Begin
  Result.Value := aValue;
End;

Procedure FileHandleClose(Var aFileHandle : TFileHandle);
{$IFDEF MACOS}
begin
end;
{$ELSE}
Begin
  Windows.CloseHandle(aFileHandle.Value);
  aFileHandle := FileHandleInvalid;
End;
{$ENDIF}

function Path(parts : array of String) : String;
var
  part : String;
begin
  result := '';
  for part in parts do
    if result = '' then
      result := part
    else
      result := IncludeTrailingPathDelimiter(result)+ part;
end;

function URLPath(parts : array of String) : String;
var
  part : String;
begin
  result := '';
  for part in parts do
    if result = '' then
      result := part
    else if result.EndsWith('/') then
      result := result+ part
    else
      result := result+'/'+part;
end;

Function CreateGUID : TGUID;
Begin
  SysUtils.CreateGuid(Result);
End;


Function GUIDToString(Const aGUID : TGUID) : String;
Begin
  Result := SysUtils.GUIDToString(aGuid);
End;


Function StringToGUID(Const sGUID: String) : TGUID;
Begin
  Result := SysUtils.StringToGUID(sGUID);
End;


Function NilGUID : TGUID;
Begin
  Result := StringToGUID('{00000000-0000-0000-0000-000000000000}');
End;

Function IsNilGUID(const guid : TGUID) : boolean;
begin
  result := (guid.D1 = 0) and (guid.D2 = 0) and (guid.D3 = 0);
end;

Function GUIDAsOID(Const aGUID : TGUID) : String;
begin
  result := FHIR.Support.Decimal.GUIDAsOIDRight(aGUID);
end;

Function IsGuid(s : String): Boolean;
begin
  if length(s) < 36  then
    result := false
  else
  begin
    if (s[1] = '{') then
    begin
      delete(s, 1, 1);
      delete(s, length(s), 1);
    end;
    s := lowercase(s);
    result := (Length(s) = 36) and
      CharInSet(s[01], ['a'..'f', '0'..'9']) and
      CharInSet(s[02], ['a'..'f', '0'..'9']) and
      CharInSet(s[03], ['a'..'f', '0'..'9']) and
      CharInSet(s[04], ['a'..'f', '0'..'9']) and
      CharInSet(s[05], ['a'..'f', '0'..'9']) and
      CharInSet(s[06], ['a'..'f', '0'..'9']) and
      CharInSet(s[07], ['a'..'f', '0'..'9']) and
      CharInSet(s[08], ['a'..'f', '0'..'9']) and
      (s[09] = '-') and
      CharInSet(s[10], ['a'..'f', '0'..'9']) and
      CharInSet(s[11], ['a'..'f', '0'..'9']) and
      CharInSet(s[12], ['a'..'f', '0'..'9']) and
      CharInSet(s[13], ['a'..'f', '0'..'9']) and
      (s[14] = '-') and
      CharInSet(s[15], ['a'..'f', '0'..'9']) and
      CharInSet(s[16], ['a'..'f', '0'..'9']) and
      CharInSet(s[17], ['a'..'f', '0'..'9']) and
      CharInSet(s[18], ['a'..'f', '0'..'9']) and
      (s[19] = '-') and
      CharInSet(s[20], ['a'..'f', '0'..'9']) and
      CharInSet(s[21], ['a'..'f', '0'..'9']) and
      CharInSet(s[22], ['a'..'f', '0'..'9']) and
      CharInSet(s[23], ['a'..'f', '0'..'9']) and
      (s[24] = '-') and
      CharInSet(s[25], ['a'..'f', '0'..'9']) and
      CharInSet(s[26], ['a'..'f', '0'..'9']) and
      CharInSet(s[27], ['a'..'f', '0'..'9']) and
      CharInSet(s[28], ['a'..'f', '0'..'9']) and
      CharInSet(s[29], ['a'..'f', '0'..'9']) and
      CharInSet(s[30], ['a'..'f', '0'..'9']) and
      CharInSet(s[31], ['a'..'f', '0'..'9']) and
      CharInSet(s[32], ['a'..'f', '0'..'9']) and
      CharInSet(s[33], ['a'..'f', '0'..'9']) and
      CharInSet(s[34], ['a'..'f', '0'..'9']) and
      CharInSet(s[35], ['a'..'f', '0'..'9']) and
      CharInSet(s[36], ['a'..'f', '0'..'9']);
  end;
end;

function NewGuidURN : String;
begin
  result := 'urn:uuid:'+NewGuidId;
end;

function NewGuidId : String;
begin
  result := copy(GUIDToString(CreateGUID), 2, 36).ToLower;
end;


Function ColourCompose(iRed, iGreen, iBlue, iAlpha : Byte) : TColour;
Begin
  TColourParts(Result).Red := iRed;
  TColourParts(Result).Green := iGreen;
  TColourParts(Result).Blue := iBlue;
  TColourParts(Result).Alpha := iAlpha;
End;

Function HTMLColourStringToColour(Const sColour : String; Const aDefault : TColour) : TColour;
Var
  iIndex : Integer;
Begin
  iIndex := StringArrayIndexOf(HTML_COLOUR_NAMES, sColour);

  If iIndex = -1 Then
    iIndex := StringArrayIndexOf(HTML_COLOUR_TITLES, sColour);

  If iIndex > -1 Then
    Result := HTML_COLOUR_VALUES[THTMLColours(iIndex)]
  Else
    Result := HTMLEncodedColourStringToColour(sColour, aDefault);
End;


Function HTMLColourStringToColour(Const sColour : String) : TColour;
Begin
  Result := HTMLColourStringToColour(sColour, 0);
End;


Function StringIsHTMLColour(Const sColour : String) : Boolean;
Begin
  Result := StringArrayExistsInsensitive(HTML_COLOUR_NAMES, sColour) Or StringArrayExistsInsensitive(HTML_COLOUR_TITLES, sColour) Or
     ((Length(sColour) = 7) And (sColour[1] = '#') And StringIsHexadecimal(Copy(sColour, 2, 2)) And StringIsHexadecimal(Copy(sColour, 4, 2)) And StringIsHexadecimal(Copy(sColour, 6, 2)));
End;

Function HTMLEncodedColourStringToColour(Const sColour : String; Const aDefault : TColour) : TColour;
Begin
  If (Length(sColour) < 7) Or (sColour[1] <> '#') Then
    Result := aDefault
  Else
  Begin
    TColourParts(Result).Red := DecodeHexadecimal(AnsiChar(sColour[2]), AnsiChar(sColour[3]));
    TColourParts(Result).Green := DecodeHexadecimal(AnsiChar(sColour[4]), AnsiChar(sColour[5]));
    TColourParts(Result).Blue := DecodeHexadecimal(AnsiChar(sColour[6]), AnsiChar(sColour[7]));
    TColourParts(Result).Alpha := 0;
  End;
End;


Function HTMLEncodedColourStringToColour(Const sColour : String) : TColour;
Begin
  Result := HTMLEncodedColourStringToColour(sColour, 0);
End;

Function ColourToHTMLEncodedColourString(Const iColour : TColour) : String;
Begin
  Result := '#' + string(EncodeHexadecimal(TColourParts(iColour).Red) + EncodeHexadecimal(TColourParts(iColour).Green) + EncodeHexadecimal(TColourParts(iColour).Blue));
End;


Function ColourToHTMLColourString(Const iColour : TColour) : String;
Var
  iIndex : Integer;
Begin
  iIndex := IntegerArrayIndexOf(HTML_COLOUR_VALUES, iColour);

  If iIndex > -1 Then
    Result := HTML_COLOUR_NAMES[THTMLColours(iIndex)]
  Else
    Result := ColourToHTMLEncodedColourString(iColour);
End;


Function ColourToHTMLColourTitleString(Const iColour : TColour) : String;
Var
  iIndex : Integer;
Begin
  iIndex := IntegerArrayIndexOf(HTML_COLOUR_VALUES, iColour);

  If iIndex > -1 Then
    Result := HTML_COLOUR_TITLES[THTMLColours(iIndex)]
  Else
    Result := ColourToHTMLEncodedColourString(iColour);
End;



Var
{$IFDEF MSWINDOWS}
  gOSInfo : TOSVersionInfo;
  gSystemInfo : TSystemInfo;
{$ENDIF}
  gNTDLLDebugBreakPointIssuePatched : Boolean = False;

Function SystemManualTemp : String;
Begin
  {$IFDEF MACOS}
  result := '/tmp';
  {$ELSE}
  result := 'c:/temp';
  {$ENDIF}
End;

Function SystemTemp : String;
  {$IFDEF MACOS}
Begin
  result := UTF8ToString(TNSString.Wrap(NSString(NSTemporaryDirectory)).UTF8String); {todo-osx}
  {$ELSE}
Var
  iLength : Integer;
Begin
  SetLength(Result, MAX_PATH + 1);

  iLength := GetTempPath(MAX_PATH, PChar(Result));

  If Not IsPathDelimiter(Result, iLength) Then
  Begin
    Inc(iLength);
    Result[iLength] := '\';
  End;

  SetLength(Result, iLength);
  {$ENDIF}
End;

Function SystemIsWindowsNT : Boolean;
Begin
  {$IFDEF MACOS}
  Result := false;
  {$ELSE}
  Result := gOSInfo.dwPlatformId >= VER_PLATFORM_WIN32_NT;
  {$ENDIF}
End;

Function SystemIsWindows7 : Boolean;
Begin
  {$IFDEF MACOS}
  Result := false;
  {$ELSE}
  Result := SystemIsWindowsNT And (gOSInfo.dwMajorVersion >= 6) And (gOSInfo.dwMinorVersion >= 1);
  {$ENDIF}
End;

{$IFDEF MACOS}
Function ProgData : String;
Begin
  Result := '/Applications';
End;

{$ELSE}
Function ShellFolder(iID : Integer) : String;
Var
  sPath : Array[0..2048] Of Char;
  pIDs  : PItemIDList;
Begin
  Result := '';

  If SHGetSpecialFolderLocation(0, iID, pIDs) = S_OK Then
  Begin
    FillChar(sPath, SizeOf(sPath), #0);

    If ShGetPathFromIDList(pIDs, sPath) Then
      Result := IncludeTrailingPathDelimiter(sPath);
  End;
End;

Function ProgData : String;
Begin
  Result := ShellFolder(CSIDL_COMMON_APPDATA);
End;
{$ENDIF}



Var
  gCriticalSection : TRTLCriticalSection;

{$IFOPT C+}
Var
  gLiveMemorySize : Int64 = 0;
  gActiveMemoryTracking : Boolean = False;
{$ENDIF}


Procedure MemorySet(Const pBuffer : Pointer; iSize : Integer; Const iValue : Byte);
Begin
  If (iSize > 0) And Assigned(pBuffer) Then
    FillChar(pBuffer^, iSize, iValue);
End;


Procedure MemoryZero(Const pBuffer : Pointer; iSize: Integer);
Begin
  MemorySet(pBuffer, iSize, $00);
End;


Procedure MemoryFill(Const pBuffer : Pointer; iSize : Integer);
Begin
  MemorySet(pBuffer, iSize, $FF);
End;


Procedure MemoryCreate(Var pBuffer; iSize : Integer);
Begin
  // Untyped because otherwise we'd have to pass in a literal Pointer type.

  GetMem(Pointer(pBuffer), iSize);

{$IFOPT C+}
  Assert(gActiveMemoryTracking, 'Memory tracking not available for call to MemoryCreate');
  EnterCriticalSection(gCriticalSection);
  Inc(gLiveMemorySize, iSize);
  LeaveCriticalSection(gCriticalSection);
{$ENDIF}
End;

Procedure MemoryResize(Var pBuffer; iOldSize, iNewSize : Integer);
Begin
  // Untyped because otherwise we'd have to pass in a literal Pointer type.

  ReAllocMem(Pointer(pBuffer), iNewSize);

{$IFOPT C+}
  Assert(gActiveMemoryTracking, 'Memory tracking not available for call to MemoryResize');
  EnterCriticalSection(gCriticalSection);
  Inc(gLiveMemorySize, iNewSize - iOldSize);
  LeaveCriticalSection(gCriticalSection);
{$ENDIF}
End;


Procedure MemoryDestroy(Var pBuffer; iSize : Integer);
Begin
  // Untyped because otherwise we'd have to pass in a literal Pointer type.

  FreeMem(Pointer(pBuffer), iSize);

{$IFOPT C+}
  Assert(gActiveMemoryTracking, 'Memory tracking not available for call to MemoryDestroy');
  EnterCriticalSection(gCriticalSection);
  Dec(gLiveMemorySize, iSize);
  LeaveCriticalSection(gCriticalSection);
{$ENDIF}
End;

Procedure MemoryMove(Const aSource, aTarget : Pointer; iSize : Integer);
Begin
  If (iSize > 0) And Assigned(aSource) And Assigned(aTarget) Then
    System.Move(aSource^, aTarget^, iSize);
End;

Function MemoryCompare(pA : Pointer; pB : Pointer; iLength : Integer) : Integer;
Begin
  Result := Integer(Not CompareMem(pA, pB, iLength));
End;


Function MemoryToString(pData : Pointer; iPosition, iLength : Integer) : AnsiString;
Begin
  SetString(Result, PAnsiChar(Integer(pData) + iPosition), iLength - iPosition);
End;


Function MemoryToString(pData : Pointer; iLength : Integer) : AnsiString;
Begin
  Result := MemoryToString(pData, 0, iLength);
End;


Function ErrorAsNumber : Integer;
Begin
  Result := GetLastError;
  SetLastError(0);
End;

Function ErrorAsString(iError : Integer) : String;
Begin
  Result := StringFormat('%d: %s', [iError, ErrorAsMessage(iError)]);
End;


Function ErrorAsMessage(iError : Integer) : String;
Var
{$IFDEF VER200}
  sTemp : PWideChar;
{$ELSE}
  sTemp : PChar;
{$ENDIF}
  iSize : Cardinal;
Begin
  iSize := 512;

  MemoryCreate(sTemp, iSize);
  Try
    // Get the last error number and convert it to text
 {$IFDEF MSWINDOWS}
    If FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM Or FORMAT_MESSAGE_ARGUMENT_ARRAY, Nil, DWORD(iError), LANG_NEUTRAL, sTemp, iSize, Nil) <> 0 Then
      Result := StringTrimWhitespace(Copy(StrPas(sTemp), 1, iSize))
    Else
 {$ENDIF}
      Result := 'Error Message missing on OSX';

  Finally
    MemoryDestroy(sTemp, iSize);
  End;
End;

Function ErrorAsString : String;
Var
  iError : Cardinal;
Begin
  iError := ErrorAsNumber;

  If iError = ERROR_SUCCESS Then
    Result := StringFormat('%d: Unknown Windows API error.', [iError])
  Else
    Result := ErrorAsString(iError);
End;

{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
Function HashStringToCode32(Const sValue : String) : Integer;
Var
  cFirst  : Char;
  cSecond : Char;
  iLength : Cardinal;
  iLoop   : Integer;
Begin
  Result := 0;
  iLength := Length(sValue);

  If iLength > 0 Then
  Begin
    cFirst := sValue[1];

    If (cFirst >= 'a') And (cFirst <= 'z') Then
      Dec(cFirst, 32);

    For iLoop := 2 To iLength Do
    Begin
      cSecond := sValue[iLoop];

      If (cSecond >= 'a') And (cSecond <= 'z') Then
        Dec(cSecond, 32);

      Inc(Result, Ord(cFirst) * Ord(cSecond) * iLoop);

      cFirst := cSecond;
    End;
  End;
End;
{$OVERFLOWCHECKS ON}
{$RANGECHECKS ON}


{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
Function HashStringToCode64(Const sValue : String) : Int64;
Begin
  // TODO: implement.

  Raise Exception.Create('HashStringToCode64 is not implemented.');

  Result := 0;
End;
{$OVERFLOWCHECKS ON}
{$RANGECHECKS ON}


{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
Function HashIntegerToCode32(Const iValue : Integer) : Integer;
Begin
  Result := iValue And $7FFFFFFF;
End;
{$OVERFLOWCHECKS ON}
{$RANGECHECKS ON}


{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
Function HashInteger64ToCode32(Const iValue : Int64) : Integer;
Begin
  Result := (iValue Shr 32) Xor (iValue And $7FFFFFFF);
End;
{$OVERFLOWCHECKS ON}
{$RANGECHECKS ON}

var
  GThreadManager : TDictionary<integer,String>;

procedure SetThreadName(name : String);
begin
  EnterCriticalSection(gCriticalSection);
  try
    if name = '' then
      GThreadManager.Remove(GetCurrentThreadId)
    else
      GThreadManager.AddOrSetValue(GetCurrentThreadId, name);
  finally
    LeaveCriticalSection(gCriticalSection);
  end;
end;

function GetThreadName(id : integer) : String;
begin
  EnterCriticalSection(gCriticalSection);
  try
    if not GThreadManager.TryGetValue(id, result) then
      result := 'n/a';
  finally
    LeaveCriticalSection(gCriticalSection);
  end;
end;

Initialization
  InitializeCriticalSection(gCriticalSection);
  {$IFOPT C+}
  gActiveMemoryTracking := True;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  FillChar(gSystemInfo, SizeOf(gSystemInfo), 0);
  FillChar(gOSInfo, SizeOf(gOSInfo), 0);

  gOSInfo.dwOSVersionInfoSize := SizeOf(gOSInfo);

  GetVersionEx(gOSInfo);
  GetSystemInfo(gSystemInfo);

  If SystemIsWindows7 Then
  Begin
    // NOTE: Windows 7 changes the behaviour of GetThreadLocale.
    //       This is a workaround to force sysutils to use the correct locale.

    SetThreadLocale(GetUserDefaultLCID);
    SysUtils.GetFormatSettings;
  End;
  {$ENDIF}
  GThreadManager := TDictionary<integer,String>.create;
Finalization
  GThreadManager.Free;
  {$IFOPT C+}
  EnterCriticalSection(gCriticalSection);
  Try
    Assert(gLiveMemorySize = 0);
  Except
    // MessageBox(0, PChar(StringFormat('Memory has not been properly released (%d bytes). Please check your usage of Memory Create/Destroy/Resize routines.', [gLiveMemorySize])), 'MemorySupport', MB_OK);
  End;

  gActiveMemoryTracking := False;

  LeaveCriticalSection(gCriticalSection);

  {$ENDIF}
  DeleteCriticalSection(gCriticalSection);
End. // FHIR.Support.System //
