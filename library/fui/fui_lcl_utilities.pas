unit fui_lcl_utilities;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$i fhir.inc}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, Graphics, IniFiles,
  Controls, Forms,
  fsl_utilities;

procedure setForOS(btnOk, btnCancel : TControl);
procedure writeFontToIni(ini : TIniFile; section : String; font : TFont);
procedure readFontFromIni(ini : TIniFile; section : String; font : TFont; defFontName : String = '');

procedure screenshot(bmp : TBitmap);

implementation

procedure setForOS(btnOk, btnCancel : TControl);
{$IFNDEF WINDOWS}
var
  l : integer;
{$ENDIF}
begin
  {$IFNDEF WINDOWS}
  l := btnCancel.left;
  btnCancel.left := btnOk.left;
  btnOk.left := l;
  {$ENDIF}
end;

procedure writeFontToIni(ini : TIniFile; section : String; font : TFont);
begin
  ini.writeString('font', 'name', font.Name);
  ini.writeInteger('font', 'size', font.Size);
  ini.writeInteger('font', 'color', font.Color);
  ini.WriteBool('font', 'bold', fsBold in font.Style);
  ini.WriteBool('font', 'italic', fsItalic in font.Style);
  ini.WriteBool('font', 'underline', fsUnderline in font.Style);
  ini.WriteBool('font', 'strikeout', fsStrikeOut in font.Style);
end;

procedure readFontFromIni(ini : TIniFile; section : String; font : TFont; defFontName : String = '');
begin
  font.Name := ini.readString('font', 'name', defFontName);
  font.Size := ini.readInteger('font', 'size', 10);
  font.Color := ini.readInteger('font', 'color', clBlack);
  font.Style := [];
  if ini.readBool('font', 'bold', false) then
    font.Style := font.Style + [fsBold];
  if ini.readBool('font', 'italic', false) then
    font.Style := font.Style + [fsItalic];
  if ini.readBool('font', 'underline', false) then
    font.Style := font.Style + [fsUnderline];
  if ini.readBool('font', 'strikeout', false) then
    font.Style := font.Style + [fsStrikeOut];
end;

procedure screenshot(bmp : TBitmap);
{$IFDEF WINDOWS}
var
  DCDesk : hDC;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  bmp.Height := Screen.DesktopHeight;
  bmp.Width := Screen.DesktopWidth;
  DCDesk := GetWindowDC(GetDesktopWindow);
  BitBlt(bmp.Canvas.Handle, 0, 0, bmp.Width, bmp.Height, GetWindowDC(GetDesktopWindow), Screen.DesktopLeft, Screen.DesktopTop, SRCCOPY);
  {$ELSE}
  raise exception.create('Not implemented on '+SystemPlatform);
  {$ENDIF}
end;

end.

