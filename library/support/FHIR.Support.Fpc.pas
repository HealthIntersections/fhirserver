unit FHIR.Support.Fpc;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$IFDEF FPC}
{$mode objfpc}{$H+}

{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH TYPEHELPERS}
{$ENDIF}

interface

{$IFDEF FPC}
uses
  Classes, SysUtils, Character, RegExpr, FileUtil, Generics.Collections, Graphics;

type

  { TCharHelper }

  TCharHelper = type helper for char
  public
    function isDigit : boolean;
    function IsNumber : boolean;
  end;

  { TTimeZone }
  TTimeSpan = record
    TotalDays : Double;
  end;

  TTimeZone = class
  private
    class var FLocal: TTimeZone;
  public
    function GetUtcOffset(const ADateTime: TDateTime; const ForceDaylight: Boolean = False): TTimeSpan; inline;
    function ToLocalTime(const ADateTime: TDateTime): TDateTime;
    function ToUniversalTime(const ADateTime: TDateTime; const ForceDaylight: Boolean = False): TDateTime; inline;
    class property Local: TTimeZone read FLocal;
  end;

function DeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;

function ColorToString(Color: TColor): AnsiString;

{$ENDIF}


implementation

{$IFDEF FPC}

function DeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;
begin
  result := FileUtil.DeleteDirectory(DirectoryName, OnlyChildren);
end;

{ TTimeZone }

function TTimeZone.GetUtcOffset(const ADateTime: TDateTime;
  const ForceDaylight: Boolean): TTimeSpan;
begin

end;

function TTimeZone.ToLocalTime(const ADateTime: TDateTime): TDateTime;
begin

end;

function TTimeZone.ToUniversalTime(const ADateTime: TDateTime;
  const ForceDaylight: Boolean): TDateTime;
begin

end;

{ TCharHelper }

function TCharHelper.isDigit: boolean;
begin
  result := Character.isDigit(self);
end;

function TCharHelper.IsNumber: boolean;
begin
  result := Character.isDigit(self);
end;

function ColorToString(Color: TColor): AnsiString;
begin
  result := Graphics.ColorToString(Color);
end;

{$ENDIF}

end.

