unit fdb_settings;

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

{$I fhir.inc}

interface

uses SysUtils, Classes, fsl_base;

type

  { TSettingsAdapter }

  {  MV 29/8/01

     TSettingsAdapter allows you to interface into many different types of data stores for
     settings information. Internally they will be read as strings, the interface can be
     interacted with in native types (eg integers, date/times, etc).

  }

  ESettingsError = class(Exception);

(*  TSettingsAdapter = class(TFslObject)
  Private
  Protected
    procedure ApplyTo(const ATarget: TSettingsAdapter); Virtual;

  Public
    procedure WriteFloat(const AName: String; const AValue: Double); Virtual; Abstract;
    procedure WriteBool(const AName: String; const AValue: Boolean); Virtual; Abstract;
    procedure WriteInteger(const AName: String; const AValue: Integer); Virtual; Abstract;
    procedure WriteString(const AName, AValue: String; ATransient: Boolean = False); Virtual; Abstract;
    procedure WriteEncryptedString(const AName, AValue: String; ATransient: Boolean = False);
    procedure WriteDateTime(const AName: String; AValue: TDateTime); Virtual; Abstract;
    procedure WriteObjProp(const AComponent: TComponent; APropPath: String; AAlias: String = '');

    function ReadFloat(const AName: String; const ADefault: Double = 0): Double; Virtual; Abstract;
    function ReadBool(const AName: String; const ADefault: Boolean = False): Boolean; Virtual; Abstract;
    function ReadInteger(const AName: String; const ADefault: Integer = 0): Integer; Virtual; Abstract;
    function ReadString(const AName: String; const ADefault: String = ''): String; Virtual; Abstract;
    function ReadEncryptedString(const AName: String; const ADefault: String = ''): String;
    function ReadDateTime(const AName: String; const ADefault: TDateTime = 0): TDateTime; Virtual; Abstract;
    procedure ReadObjProp(const AComponent: TComponent; APropPath: String; AAlias: String = ''; ADefault: String = '');

    procedure ApplyToObject(AComponent: TComponent);

    procedure Clear; Virtual; Abstract;

    function IsTransient(const AName: String): Boolean; Virtual; Abstract;
    function ValueExists(const AName: String): Boolean; Virtual; Abstract;
    procedure DeleteValue(const AName: String); Virtual; Abstract;

    // This procedure works the same as TPersistant, completly
    // overwrites old settings.
    // Calls Clear and Apply
    procedure Assign(const ASource: TSettingsAdapter);

    // similar to Assign but *shouldn't* completly nuke
    // old settings, should *add* to them.
    procedure Apply(const ASource: TSettingsAdapter); Virtual;

  end;

  { TCustomStringSettings }

  TCustomStringSettings = class(TSettingsAdapter)
  Protected
    function InternalRead(const AName: String; var AValue: String): Boolean; Virtual; Abstract;
    procedure InternalWrite(const AName, AValue: String; ATransient: Boolean = False); Virtual; Abstract;

  Public
    procedure WriteFloat(const AName: String; const AValue: Double); Override;
    procedure WriteBool(const AName: String; const AValue: Boolean); Override;
    procedure WriteInteger(const AName: String; const AValue: Integer); Override;
    procedure WriteString(const AName, AValue: String; ATransient: Boolean = False); Override;
    procedure WriteDateTime(const AName: String; AValue: TDateTime); Override;

    function ReadFloat(const AName: String; const ADefault: Double = 0): Double; Override;
    function ReadBool(const AName: String; const ADefault: Boolean = False): Boolean; Override;
    function ReadInteger(const AName: String; const ADefault: Integer = 0): Integer; Override;
    function ReadString(const AName: String; const ADefault: String = ''): String; Override;
    function ReadDateTime(const AName: String; const ADefault: TDateTime = 0): TDateTime; Override;

  end;

  { TStringSettings }

  TStringSettings = class(TCustomStringSettings)
  Private
    FStrings: TStringList;
    function GetStrings: TStrings;

  Protected
    function InternalRead(const AName: String; var VValue: String): Boolean; Override;
    procedure InternalWrite(const AName, AValue: String; ATransient: Boolean = False); Override;

    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(const ACopyFrom: TStrings = NIL);
    destructor Destroy; Override;

    procedure Clear; Override;

    function IsTransient(const AName: String): Boolean; Override;
    function ValueExists(const AName: String): Boolean; Override;
    procedure DeleteValue(const AName: String); Override;

    procedure ApplyTo(const ATarget: TSettingsAdapter); Override;

    property Strings: TStrings Read GetStrings;

  end;
        *)
implementation

uses
  TypInfo;

(*
{ TSettingsAdapter }

procedure TSettingsAdapter.Apply(const ASource: TSettingsAdapter);
begin
  ASource.ApplyTo(self);
end;

function GetBeforeDot(var AString: String; var ABefore: String): Boolean;
var
  LDotPos: Integer;
begin
  Result := False;
  LDotPos := Pos('.', AString);
  if (LDotPos > 1) and (LDotPos < Length(AString)) then
    begin
    Result := True;
    ABefore := Copy(AString, 1, LDotPos - 1);
    AString := Copy(AString, LDotPos + 1, MAXINT);      {eat up to and including the dot}
    end;
end;

const 
  OK_PROP_TYPES = [tkInteger, tkChar, tkEnumeration, tkString, tkSet, tkClass];

function TraverseProperties(var AComponent: TPersistent; APath: String): PPropInfo;
var 
  LPropName: String;
  LPropComponent: TObject;
  LChildComp: TComponent;
begin
  Result := NIL;
  if Pos('.', APath) > 1 then
    begin
    if GetBeforeDot(APath, LPropName) then
      begin
      Result := GetPropInfo(AComponent, LPropName, OK_PROP_TYPES);
      if Assigned(Result) and
        (Result^.PropType^.Kind = tkClass) and
        (APath > '') then
        begin
        {//it's an object, we need to look for sub properties.}
        LPropComponent := GetObjectProp(AComponent, Result);
        if LPropComponent is TPersistent then
          begin
          Result := TraverseProperties(TPersistent(LPropComponent), APath);
          if Assigned(Result) then
            AComponent := TPersistent(LPropComponent);
          end;
        end
      else { well, hmmm... maybe it's a owned component}
      if (AComponent is TComponent) then
        begin
        LChildComp := TComponent(AComponent).FindComponent(LPropName);
        if Assigned(LChildComp) then
          begin
          Result := TraverseProperties(TPersistent(LChildComp), APath);
          if Assigned(Result) then
            AComponent := LChildComp;
          end;
        end;
      end;
    end
  else
    Result := GetPropInfo(AComponent, APath, OK_PROP_TYPES);
end;

procedure TSettingsAdapter.ReadObjProp(const AComponent: TComponent; APropPath,
  AAlias, ADefault: String);
var   
  LPropInfo: PPropInfo;
  LValue: String;
  LSubComponent: TPersistent;
begin
  LSubComponent := AComponent;
  if AAlias = '' then
    AAlias := APropPath;
  if ValueExists(AAlias) then
    begin
    LValue := ReadString(AAlias, ADefault);
    LPropInfo := TraverseProperties(LSubComponent, APropPath);
    try
      if Assigned(LPropInfo) then
        case LPropInfo^.PropType^.Kind of
          tkString: 
            SetStrProp(LSubComponent, LPropInfo, LValue);
          tkInteger, tkChar: 
            SetOrdProp(LSubComponent, LPropInfo, StrToIntDef(LValue, 0));
          tkEnumeration: 
            if LValue <> '' then
              SetEnumProp(LSubComponent, LPropInfo, LValue);
          tkSet: 
            SetSetProp(LSubComponent, LPropInfo, LValue);
          end;
    except
      on E: 
      EPropertyConvertError do;// I don't think we care, do we?
      end;
    end;
end;

procedure TSettingsAdapter.WriteObjProp(const AComponent: TComponent; APropPath: String; AAlias: String = '');
var   
  LPropInfo: PPropInfo;
  LValue: String;
  LSubComponent: TPersistent;
begin
  LSubComponent := AComponent;
  LPropInfo := TraverseProperties(LSubComponent, APropPath);
  if Assigned(LPropInfo) then
    case LPropInfo^.PropType^.Kind of
      tkString: 
        LValue := GetStrProp(LSubComponent, LPropInfo);
      tkInteger, tkChar: 
        LValue := IntToStr(GetOrdProp(LSubComponent, LPropInfo));
      tkEnumeration: 
        LValue := GetEnumProp(LSubComponent, LPropInfo);
      tkSet: 
        LValue := GetSetProp(LSubComponent, LPropInfo);
      end;
  if AAlias <> '' then
    APropPath := AAlias;
  WriteString(APropPath, LValue);
end;

procedure TSettingsAdapter.ApplyToObject(AComponent: TComponent);
var 
  i: Integer;
  LName, LValue: String;
  LStringSettings: TStringSettings;
begin
  LStringSettings := TStringSettings.Create;
  try

    // get a copy of the settings we can enumerate
    LStringSettings.Apply(self);

    for i := 0 to LStringSettings.Strings.Count - 1 do 
      begin
      LName := LStringSettings.Strings.Names[i];
      LValue := LStringSettings.Strings.Values[LName];
      if (LName <> '') and (LName[1] <> ';') then  //commented out line
        begin
        try
          ReadObjProp(AComponent, LName);
        except
          on E: 
          EPropertyError do 
            ShowException(E, ExceptAddr);
          on E: 
          EPropertyConvertError do 
            ShowException(E, ExceptAddr);
          else
            raise;
          end;
        end;
      end;

  finally
    LStringSettings.Free;
    end;
end;

procedure TSettingsAdapter.ApplyTo(const ATarget: TSettingsAdapter);
begin
  raise ESettingsError.Create('Can''t assign/apply a ' + ClassName + ' to a ' + ATarget.ClassName);
end;

procedure TSettingsAdapter.Assign(const ASource: TSettingsAdapter);
begin
  Clear;
  Apply(ASource);
end;

{ TCustomStringSettings }

// Date in format yyyy-mm-dd hh:nn:ss
// eg             2001-08-29 11:29:00
// positions      12345678901234567890
//                         1         2

const

  TIME_FORMAT = 'yyyy-mm-dd hh:nn:ss';

function DTStringToDateTime(const AString: String): TDateTime;
var   
  LYear, LMonth, LDay, LHour, LMinute, LSecond: Word;

  function ToInt(const AString: String; const ABitname: String): Word;
  var   
    LCode: Integer;
    begin
    Val(AString, Result, LCode);
    if LCode > 0 then
      raise EConvertError.Create('Invalid value for ' + ABitname + ' reading setting');
    end;
begin
  if Length(AString) <> Length(TIME_FORMAT) then
    raise EConvertError.Create('Date time in invalid format reading setting');
  LYear := ToInt(Copy(AString, 1, 4), 'Year');
  LMonth := ToInt(Copy(AString, 6, 2), 'Month');
  LDay := ToInt(Copy(AString, 9, 2), 'Day');
  LHour := ToInt(Copy(AString, 12, 2), 'Hour');
  LMinute := ToInt(Copy(AString, 15, 2), 'Minute');
  LSecond := ToInt(Copy(AString, 18, 2), 'Second');
  Result := EncodeTime(LHour, LMinute, LSecond, 0); // no milliseconds
  Result := Result + EncodeDate(LYear, LMonth, LDay);
end;

function DateTimeToDTString(const ADateTime: TDateTime): String;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', ADateTime);
end;

function TCustomStringSettings.ReadBool(const AName: String;
  const ADefault: Boolean): Boolean;
begin
  Result := ReadInteger(AName, Ord(ADefault)) <> 0;
end;

function TCustomStringSettings.ReadDateTime(const AName: String;
  const ADefault: TDateTime): TDateTime;
var   
  LDateTimeStr: String;
begin
  Result := ADefault;
  if InternalRead(AName, LDateTimeStr) then
    begin
    try
      Result := DTStringToDateTime(LDateTimeStr);
    except
      on EConvertError do
      else 
        raise;
      end;
    end;
end;

function TCustomStringSettings.ReadFloat(const AName: String;
  const ADefault: Double): Double;
var   
  LFloatStr: String;
begin
  Result := ADefault;
  if InternalRead(AName, LFloatStr) and (LFloatStr <> '') then
    begin
    try
      Result := StrToFloat(LFloatStr);
    except
      on EConvertError do
      else 
        raise;
      end;
    end;
end;

function TCustomStringSettings.ReadInteger(const AName: String;
  const ADefault: Integer): Integer;
var   
  LIntStr: String;
begin
  Result := ADefault;
  if InternalRead(AName, LIntStr) then
    begin
    if (Length(LIntStr) > 2) and (LIntStr[1] = '0') and
      ((LIntStr[2] = 'X') or (LIntStr[2] = 'x')) then
      LIntStr := '$' + Copy(LIntStr, 3, Maxint);
    Result := StrToIntDef(LIntStr, ADefault);
    end;
end;

function TCustomStringSettings.ReadString(const AName, ADefault: String): String;
begin
  Result := ADefault;
  InternalRead(AName, Result);
end;

procedure TCustomStringSettings.WriteBool(const AName: String; const AValue: Boolean);
const 
  LValues: array[Boolean] of String = ('0', '1');
begin
  InternalWrite(AName, LValues[AValue]);
end;

procedure TCustomStringSettings.WriteDateTime(const AName: String;
  AValue: TDateTime);
begin
  InternalWrite(AName, DateTimeToDTString(AValue));
end;

procedure TCustomStringSettings.WriteFloat(const AName: String; const AValue: Double);
begin
  InternalWrite(AName, FloatToStr(AValue));
end;

procedure TCustomStringSettings.WriteInteger(const AName: String; const AValue: Integer);
begin
  InternalWrite(AName, IntToStr(AValue));
end;

procedure TCustomStringSettings.WriteString(const AName, AValue: String; ATransient: Boolean = False);
begin
  InternalWrite(AName, AValue, ATransient);
end;


{$R-}


const
  Base64Table: array[0..63] of Char =
    ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3',
    '4', '5', '6', '7', '8', '9', '+', '/');

procedure BuildBase64(var OutBuf: array of Char; Index: Longint;
  Significant: Integer; Value: Longint);
var
  i: Integer;
begin
  for i := 3 downto Significant do
    begin
    OutBuf[Index +i] := '=';
    Value := Value shr 6;
    end;
  for i := Significant - 1 downto 0 do
    begin
    OutBuf[Index +i] := Base64Table[Value and 63];
    Value := Value shr 6;
    end;
end;

function Base64ToBinary(var InBuf, OutBuf: array of Char; InLen: Longint): Longint;
{ This routine converts a Base64 encoded stream back to the original
  binary stream. No real error checking is done except to ignore
  unexpected data. For example, only valid Base64 ascii characters
  are acted upon and Pad characters are ignored unless they are the
  last or next-to-last characters in the input buffer. Futhermore,
  any partially decoded data is ignored if the input stream is too
  short to contain 4 Base64 characters. }
var
  Value, InNdx, OutNdx: Longint;
  Count, Pad: Integer;
begin
  Value := 0;
  Count := 0;
  Pad := 0;
  OutNdx := 0;
  for InNdx := 0 to InLen - 1 do
    begin
      begin
      case InBuf[InNdx] of
        'A'..'Z':
          begin
          Value := (Value shl 6) + Longint(Ord(InBuf[InNdx])) - Longint(Ord('A'));
          Inc(Count);
          end;
        'a'..'z': 
          begin
          Value := (Value shl 6) + Longint(Ord(InBuf[InNdx])) - Longint(Ord('a')) + Longint(26);
          Inc(Count);
          end;
        '0'..'9': 
          begin
          Value := (Value shl 6) + Longint(Ord(InBuf[InNdx])) - Longint(Ord('0')) + Longint(52);
          Inc(Count);
          end;
        '+': 
          begin
          Value := (Value shl 6) + 62;
          Inc(Count);
          end;
        '/': 
          begin
          Value := (Value shl 6) + 63;
          Inc(Count);
          end;
        '=': 
          begin
          if InNdx >= InLen - 2 then 
            begin
            Value := (Value shl 6);
            Inc(Count);
            Inc(Pad);
            end;
          end;
        end;
      end;
    { If we have decoded 4 characters then we need to build the
      output buffer. If any pad characters were detected then we
      adjust the values first to ensure we only generate as many
      output bytes as there originally were. }
    if Count = 4 then 
      begin
      Count := 3;
      if Pad = 1 then
        begin {Only 16 bits were originally encoded }
        Value := Value shr 8;
        Count := 2;
        end
      else if Pad = 2 then
        begin {Only 8 bits were originally encoded }
        Value := Value shr 16;
        Count := 1;
        end;
      for Pad := Count - 1 downto 0 do
        begin
        OutBuf[OutNdx + Pad] := Chr(Value and 255);
        Value := Value shr 8;
        end;
      Inc(OutNdx, Count);
      Count := 0;
      Pad := 0;
      Value := 0;
      end;
    end;
  Result := OutNdx;
end;


function BinaryToBase64(var InBuf, OutBuf: array of Char; InLen: Longint): Longint;
var
  InNdx, OutNdx, Width: Longint;
begin
  InNdx := 0;
  OutNdx := 0;
  Width := 0;
  while InNdx + 3 <= InLen do
    begin
    BuildBase64(OutBuf, OutNdx, 4,
    (Longint(Ord(InBuf[InNdx])) shl 16) +
    (Longint(Ord(InBuf[InNdx + 1])) shl 8) +
      Longint(Ord(InBuf[InNdx + 2])));
    inc(OutNdx, 4);
    inc(Width, 4);
    if Width = 60 then
      begin
      OutBuf[OutNdx + 0] := #13;
      OutBuf[OutNdx + 1] := #10;
      Width := 0;
      inc(OutNdx, 2);
      end;
    InNdx := InNdx + 3;
    end;
  if InLen - InNdx = 2 then
    begin
    BuildBase64(OutBuf, OutNdx, 3,
    (Longint(Ord(InBuf[InNdx])) shl 16) + (Longint(Ord(InBuf[InNdx + 1])) shl 8));
    OutNdx := OutNdx + 4;
    end
  else if InLen - InNdx = 1 then
    begin
    BuildBase64(OutBuf, OutNdx, 2, Longint(Ord(InBuf[InNdx])) shl 16);
    OutNdx := OutNdx + 4;
    end;
  Result := OutNdx;
end;

{$R+}

function StripEoln(const AStr: String): String;
var 
  i: Integer;
begin
  i := Length(AStr);
  while (i > 0) and ((AStr[i] = #13) or (AStr[i] = #10) or (AStr[i] = ' ')) do
    Dec(i);
  Result := Copy(AStr, 1, i);
end;

function Base64ToBin(AStr: String): String;
var
  LIn, LOut: PChar;
  LLen: Longint;
begin
  if AStr = '' then
    begin
    Result := '';
    exit;
    end;
  AStr := StripEoln(AStr);
  GetMem(LIn, Length(AStr));
  try
    GetMem(LOut, length(AStr));
    try
      Move(AStr[1], LIn^, length(AStr));
      LLen := Base64ToBinary(LIn^, LOut^, Length(AStr));
      Result := '';
      if LLen = 0 then
        exit;
      SetLength(Result, LLen);
      Move(LOut^, Result[1], LLen);
    finally
      FreeMem(LOut);
      end;
  finally
    FreeMem(LIn);
    end;
end;

function BinToBase64(AStr: String): String;
var
  LIn, LOut: PChar;
  LRLen, LMLen: Longint;
  k: Integer;
begin
  k := length(AStr);
  if AStr = '' then
    begin
    Result := '';
    exit;
    end;
  GetMem(LIn, (k + 1));
  try
    Move(AStr[1], LIn^, Length(AStr));
    LMLen := Length(AStr) * 2;
    if LMLen < 4 then
      LMLen := 4;
    GetMem(LOut, LMLen);
    try
      LRLen := BinaryToBase64(LIn^, LOut^, Length(AStr));
      Result := '';
      if LRLen = 0 then
        exit;
      SetLength(Result, LRLen);
      Move(LOut^, Result[1], LRLen);
    finally
      FreeMem(LOut);
      end;
  finally
    FreeMem(LIn);
    end;
end;

const
  C1 = 52845;
  C2 = 22719;

function GetCryptKey(const AStr: String): Word;
var
  i: Integer;
begin
  {$R-}
  Result := 1;
  for i := 1 to Length(AStr) do
    Result := Result * i * Ord(AStr[i]);
  {$R+}
end;


{$R-}
{$Q-}
function strEncrypt(const AStr: String; AKey: Word): String;
var
  i: Integer;
begin
  Result := '';
  setlength(Result, Length(AStr));
  for i := 1 to Length(AStr) do
    begin
    Result[i] := Char(Ord(AStr[i]) xor (AKey shr 8));
    AKey := (Ord(Result[i]) + AKey) * C1 + C2;
    end;
  Result := BinTobase64(Result);
end;

function strDecrypt(const AStr: String; AKey: Word): String;
var
  i: Integer;
  LStr: String;
begin
  Result := '';
  LStr := base64toBin(AStr);
  setlength(Result, length(LStr));
  for i := 1 to Length(LStr) do
    begin
    Result[i] := Char(Ord(LStr[i]) xor (AKey shr 8));
    AKey := (Ord(LStr[i]) + AKey) * C1 + C2;
    end;
end;
{$R+}
{$Q+}


function TSettingsAdapter.ReadEncryptedString(const AName: String;
  const ADefault: String): String;
begin
  Result := ReadString(AName);
  if Result <> '' then
    begin
    if not SameText(ReadString(AName + '_Encrypted'), 'no') then
      begin
      Result := strDecrypt(Result, 19278);
      end;
    end;
end;

procedure TSettingsAdapter.WriteEncryptedString(const AName,
  AValue: String; ATransient: Boolean);
begin
  WriteString(AName, strEncrypt(AValue, 19278), ATransient);
end;

{ TStringSettings }

procedure TStringSettings.ApplyTo(const ATarget: TSettingsAdapter);
var   
  LPos, i: Integer;
  LLine, LName, LValue: String;
  LTransient: Boolean;
begin
  for i := 0 to FStrings.Count - 1 do
    begin
    LLine := FStrings[i];
    LPos := Pos('=', LLine);
    LName := Copy(LLine, 1, LPos - 1);
    LValue := Copy(LLine, LPos + 1, MaxInt);
    LTransient := Boolean(FStrings.Objects[i]);
    ATarget.WriteString(LName, LValue, LTransient);
    end;
end;

procedure TStringSettings.Clear;
begin
  FStrings.Clear;
end;

constructor TStringSettings.Create(const ACopyFrom: TStrings = NIL);
begin
  inherited Create;
  FStrings := TStringList.Create;
  if Assigned(ACopyFrom) then
    FStrings.Assign(ACopyFrom);
end;

procedure TStringSettings.DeleteValue(const AName: String);
var   
  LIndex: Integer;
begin
  LIndex := FStrings.IndexOfName(AName);
  if LIndex > -1 then
    FStrings.Delete(LIndex);
end;

destructor TStringSettings.Destroy;
begin
  FreeAndNil(FStrings);
  inherited;
end;

function TStringSettings.GetStrings: TStrings;
begin
  Result := FStrings;
end;

function TStringSettings.InternalRead(const AName: String; var VValue: String): Boolean;
var   
  LIndex: Integer;
begin
  Result := False;
  LIndex := FStrings.IndexOfName(AName);
  if LIndex >= 0 then
    begin
    Result := True;
    VValue := Copy(FStrings.Strings[LIndex], Length(AName) + 2, MaxInt); //copied from TStrings.GetValue
    end;
end;

procedure TStringSettings.InternalWrite(const AName, AValue: String; ATransient: Boolean = False);
var   
  LIndex: Integer;
begin
  // copied from TStrings.SetValue (with a few mods)
  LIndex := FStrings.IndexOfName(AName);
  if LIndex < 0 then
    LIndex := FStrings.Add('');
  FStrings.Strings[LIndex] := AName + '=' + AValue;
  FStrings.Objects[LIndex] := pointer(ATransient); //yuk, really needs a wrapper class
end;

function TStringSettings.IsTransient(const AName: String): Boolean;
var   
  LIndex: Integer;
begin
  Result := False;
  LIndex := FStrings.IndexOfName(AName);
  if LIndex > -1 then
    Result := Boolean(FStrings.Objects[LIndex]); //yuk, really needs a wrapper class
end;

function TStringSettings.ValueExists(const AName: String): Boolean;
begin
  Result := FStrings.IndexOfName(AName) <> -1;
end;
    *)

end.
