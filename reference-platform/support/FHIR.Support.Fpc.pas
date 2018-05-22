unit FHIR.Support.Fpc;

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

