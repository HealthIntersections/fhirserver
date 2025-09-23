unit fsl_versions;

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

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_utilities, fsl_xml, fsl_logging;

type
  ESemVerException = class (EFslException);

  TSemanticVersionLevel = (semverAuto, semverMajor, semverMinor, semverPatch, semverLabel);

  { TSemanticVersion }

  TSemanticVersion = class (TFslObject)
  private
    FRaw : String;
    FBuildLabel: String;
    FMajor: integer;
    FMinor: integer;
    FPatch: integer;
    FValid : boolean;
    FLevel : TSemanticVersionLevel;

    procedure SetBuildLabel(const value: String);

    procedure applyToDelphiProject(project : String; debug : boolean);
    procedure applyToLazarusProject(project : String; debug : boolean);
  public
    constructor Create; override;

    property Raw : String read FRaw;
    property valid : boolean read FValid;
    property Major : integer read FMajor write FMajor;
    property Minor : integer read FMinor write FMinor;
    property Patch : integer read FPatch write FPatch;
    property BuildLabel : String read FBuildLabel write SetBuildLabel;
    property level : TSemanticVersionLevel read FLevel;

    class function isValid(ver : String) : boolean;

    class function fromString(ver : String) : TSemanticVersion; overload;
    class function fromString(ver : String; strict : boolean) : TSemanticVersion; overload;
    function ToString : String; overload; override;
    function ToString(level : TSemanticVersionLevel) : String; overload;

    class function getMajMin(v : string) : String; overload;
    class function getMajMin(v : string; strict : boolean) : String; overload;

    procedure incVer(step : TSemanticVersionLevel);

    class function matches(v1, v2 : String; level : TSemanticVersionLevel) : boolean; overload;
    function matches(other : TSemanticVersion; level : TSemanticVersionLevel) : boolean; overload;

    class function isMoreRecent(v1, v2 : String) : boolean; overload;
    function isMoreRecent(other : TSemanticVersion) : boolean; overload;

    class function isSameOrMoreRecent(v1, v2 : String) : boolean; overload;
    function isSameOrMoreRecent(other : TSemanticVersion) : boolean; overload;

    procedure applyToProject(project : String; debug : boolean);

  end;


implementation


{ TSemanticVersion }

class function TSemanticVersion.isValid(ver: String): boolean;
var
  o : TSemanticVersion;
  c1, c2 : integer;
  v : String;
begin
  v := ver;
  c2 := ver.CountChar('-');
  if (c2 > 0) then
    v := ver.Substring(0, ver.indexOf('-'));
  c1 := v.CountChar('.');
  if (c1 < 1) or (c1 > 2) then
    result := false
  else
  begin
    try
      o := TSemanticVersion.Create;
      try
        result := true;
      finally
        o.free;
      end;
    except
      result := false;
    end;
  end;
end;

class function TSemanticVersion.fromString(ver: String): TSemanticVersion;
begin
  result := fromString(ver, true);
end;

function isWildCard(s : String) : boolean;
begin
  result := StringArrayExists(['*', 'x', 'X'], s);
end;

class function TSemanticVersion.fromString(ver: String; strict : boolean): TSemanticVersion;
var
  c : integer;
  parts : TArray<string>;
  p, l, r: string;
begin
  if (ver = '') then
    exit(nil);
  if (SameText(ver, 'r2')) then
  begin
    result := TSemanticVersion.fromString('1.0.2');
    result.FLevel := semverMinor;
  end
  else if (SameText(ver, 'r2b')) then
  begin
    result := TSemanticVersion.fromString('1.4.0');
    result.FLevel := semverMinor;
  end
  else if (SameText(ver, 'r3')) then
  begin
    result := TSemanticVersion.fromString('3.0.2');
    result.FLevel := semverMinor;
  end
  else if (SameText(ver, 'r4')) then
  begin
    result := TSemanticVersion.fromString('4.0.1');
    result.FLevel := semverMinor;
  end
  else if (SameText(ver, 'r4b')) then
  begin
    result := TSemanticVersion.fromString('4.3.0');
    result.FLevel := semverMinor;
  end
  else if (SameText(ver, 'r5')) then
  begin
    result := TSemanticVersion.fromString('5.0.0');
    result.FLevel := semverMinor;
  end
  else
  begin
    c := ver.CountChar('.');
    if strict and ((c < 1) or (c > 2)) then
      raise ESemVerException.create('Error reading SemVer: Structure "'+ver+'" is not correct');

    result := TSemanticVersion.Create;
    try
      result.FRaw := ver;
      parts := ver.Split(['.']);
      if (length(parts) > 3) then
      begin
        result.FBuildLabel := parts[3];
      end
      else if (length(parts) = 3) and (parts[2].contains('-')) then
      begin
        StringSplit(parts[2], '-', l, r);
        result.FBuildLabel := r;
        parts[2] := l;
      end;
      result.FValid := true;

      if (length(parts) > 0) then
      begin
        if StrToIntDef(parts[0], -1) > -1 then
          result.FMajor := StrToInt(parts[0])
        else if (isWildCard(parts[0])) then
          result.FMajor := -1
        else if strict then
          raise ESemVerException.create('Error reading SemVer: Major "'+parts[0]+'" is not an integer')
        else
          result.FValid := false;
        result.FLevel := semverMajor;
      end;

      if (length(parts) > 1) then
      begin
        if StrToIntDef(parts[1], -1) > -1 then
          result.FMinor := StrToInt(parts[1])
        else if (isWildCard(parts[1])) then
          result.FMinor := -1
        else if strict then
          raise ESemVerException.create('Error reading SemVer: Minor "'+parts[1]+'" is not an integer')
        else
          result.FValid := false;
        result.FLevel := semverMinor;
      end;

      if (length(parts) > 2) then
      begin
        if StrToIntDef(parts[2], -1) > -1 then
          result.FPatch := StrToInt(parts[2])
        else if (isWildCard(parts[2])) then
          result.FPatch := -1
        else if strict then
          raise ESemVerException.create('Error reading SemVer: Patch "'+parts[2]+'" is not an integer')
        else
          result.FValid := false;
        if result.FBuildLabel <> '' then
          result.FLevel := semverLabel
        else
          result.FLevel := semverPatch;
      end;

      result.Link;
    finally
      result.free;
    end;
  end;
end;

function TSemanticVersion.ToString: String;
begin
  if FMajor = -1 then
    result := FRaw
  else
  begin
    result := inttostr(FMajor)+'.'+inttostr(FMinor);
    if FPatch > -1 then
      result := result +'.'+inttostr(FPatch);
    if FBuildLabel > '' then
      result := result +'-'+FBuildLabel;
  end;
end;

function TSemanticVersion.ToString(level: TSemanticVersionLevel): String;
begin
  if FMajor = -1 then
    result := ''
  else
  begin
    result := inttostr(FMajor);
    if level in [semverMinor, semverPatch, semverLabel] then
      result := result + '.' + inttostr(FMinor);
    if (level in [semverPatch, semverLabel]) and (FPatch > -1) then
      result := result + '.' + inttostr(FPatch);
    if (level in [semverLabel]) and (FBuildLabel <> '') then
      result := result + '-' + FBuildLabel;
  end;
end;

class function TSemanticVersion.getMajMin(v: string): String;
begin
  result := getMajMin(v, true);
end;

class function TSemanticVersion.getMajMin(v: string; strict : boolean): String;
var
  this : TSemanticVersion;
begin
  if v = '' then
    exit('');

  try
    this := fromString(v, strict);
    try
      result := this.ToString(semverMinor);
    finally
      this.free;
    end;
  except
    result := '';
  end;
end;

procedure TSemanticVersion.incVer(step : TSemanticVersionLevel);
begin
  case step of
    semverMajor :
      begin
        inc(FMajor);
        FMinor := 0;
        FPatch := 0;
        FBuildLabel := '';
      end;
    semverMinor :
      begin
        inc(FMinor);
        FPatch := 0;
        FBuildLabel := '';
      end;
    semverPatch :
      begin
        inc(FPatch);
        FBuildLabel := '';
      end;
    semverLabel :
      begin
        FBuildLabel := '';
      end;
  end;
end;

class function TSemanticVersion.matches(v1, v2: String; level: TSemanticVersionLevel): boolean;
var
  o1, o2 : TSemanticVersion;
begin
  if (v1 = '') or (v2 = '') then
    exit(false);
  if (sameText(v1, v2)) then
    exit(true);
  try
    o1 := fromString(v1, false);
    try
      o2 := fromString(v2, false);
      try
        if (not o1.valid) or (not o2.valid) then
          result := o1.raw = o2.raw
        else
        begin
          if level = semverAuto then
            level := o1.level;
          result := o1.matches(o2, level);
        end;
      finally
        o2.free;
      end;
    finally
      o1.free;
    end;
  except
    result := false;
  end;
end;

function TSemanticVersion.matches(other: TSemanticVersion; level: TSemanticVersionLevel): boolean;
  function m(l1, l2 : integer) : boolean;
  begin
    if l1 = -1 then
      result := true
    else if l2 = -1 then
      result := true
    else
      result := l1 = l2;
  end;
  function lm(l1, l2 : string) : boolean;
  begin
    if l1 = '*' then
      result := true
    else if l2 = '*' then
      result := true
    else
      result := l1 = l2;
  end;
begin
  case level of
    semverMajor :
        result := m(FMajor, other.FMajor);
    semverMinor :
      result := m(FMajor, other.FMajor) and m(FMinor, other.FMinor);
    semverPatch :
      result := m(FMajor, other.FMajor) and m(FMinor, other.FMinor) and m(FPatch, other.FPatch);
    semverLabel :
      result := m(FMajor, other.FMajor) and m(FMinor, other.FMinor) and m(FPatch, other.FPatch) and lm(FBuildLabel, other.FBuildLabel);
  end;
end;

class function TSemanticVersion.isMoreRecent(v1, v2: String): boolean;
var
  o1, o2 : TSemanticVersion;
begin
  if (v1 = '') or (v2 = '') then
    exit(false);
  if (sameText(v1, v2)) then
    exit(false);
  try
    o1 := fromString(v1);
    try
      o2 := fromString(v2);
      try
        result := o1.isMoreRecent(o2);
      finally
        o2.free;
      end;
    finally
      o1.free;
    end;
  except
    result := false;
  end;
end;

function TSemanticVersion.isMoreRecent(other: TSemanticVersion): boolean;
begin
  if FMajor > other.FMajor then
    result := true
  else if FMajor < other.FMajor then
    result := false
  else if FMinor > other.FMinor then
    result := true
  else if FMinor < other.FMinor then
    result := false
  else if FPatch > other.FPatch then
    result := true
  else if FPatch < other.FPatch then
    result := false
  else
    result := false; // we don't know how to compare labels
end;

class function TSemanticVersion.isSameOrMoreRecent(v1, v2: String): boolean;
var
  o1, o2 : TSemanticVersion;
begin
  if (v1 = '') or (v2 = '') then
    exit(false);
  if (sameText(v1, v2)) then
    exit(true);
  try
    o1 := fromString(v1);
    try
      o2 := fromString(v2);
      try
        result := o1.isMoreRecent(o2);
      finally
        o2.free;
      end;
    finally
      o1.free;
    end;
  except
    result := false;
  end;
end;

function TSemanticVersion.isSameOrMoreRecent(other: TSemanticVersion): boolean;
begin
  if FMajor > other.FMajor then
    result := true
  else if FMajor < other.FMajor then
    result := false
  else if FMinor > other.FMinor then
    result := true
  else if FMinor < other.FMinor then
    result := false
  else if FPatch > other.FPatch then
    result := true
  else if FPatch < other.FPatch then
    result := false
  else
    result := true; // we don't know how to compare labels
end;

procedure TSemanticVersion.applyToProject(project: String; debug : boolean);
var
  s : String;
begin
  s := ChangeFileExt(project, '.dproj');
  if (FileExists(s)) then
    applyToDelphiProject(s, debug);
  s := ChangeFileExt(project, '.lpi');
  if (FileExists(s)) then
    applyToLazarusProject(s, debug);
end;

constructor TSemanticVersion.Create;
begin
  inherited;
  FMajor := -1;
  FMinor := -1;
  FPatch := -1;
end;

procedure TSemanticVersion.SetBuildLabel(const value: String);
begin
  if (value.Contains(' ')) then
    raise ESemVerException.create('Build Label cannot contain a space');
  FBuildLabel := value;
end;

procedure TSemanticVersion.applyToDelphiProject(project: String; debug : boolean);
var
  xml : TMXmlDocument;
  pr, pg : TMXmlElement;
  list : TFslList<TMXmlElement>;
  f : TFileStream;
begin
  xml := TMXmlParser.parseFile(project, [xpDropWhitespace]);
  try
    pr := xml.docElement;
    list := TFslList<TMXmlElement>.Create;
    try
      pr.listElements('PropertyGroup', list);
      for pg in list do
      begin
        if pg.element('VerInfo_MajorVer') <> nil then
        begin
          pg.element('VerInfo_MajorVer').AllText := inttostr(FMajor);
          if pg.element('VerInfo_MinorVer') = nil then
            pg.addElement('VerInfo_MinorVer');
          pg.element('VerInfo_MinorVer').AllText := inttostr(FMinor);
          if pg.element('VerInfo_Release') = nil then
            pg.addElement('VerInfo_Release');
          pg.element('VerInfo_Release').AllText := inttostr(FPatch);
          if pg.element('VerInfo_Debug') = nil then
            pg.addElement('VerInfo_Debug');
          if (debug) then
            pg.element('VerInfo_Debug').AllText := 'true'
          else
            pg.element('VerInfo_Debug').AllText := 'false';
        end;
      end;
    finally
      list.free;
    end;

    f := TFileStream.create(project, fmCreate);
    try
      xml.ToXml(f, true);
    finally
      f.free;
    end;
  finally
    xml.free;
  end;
end;

procedure TSemanticVersion.applyToLazarusProject(project: String; debug : boolean);
var
  xml : TMXmlDocument;
  cfg, po, vi, c : TMXmlElement;
  f : TFileStream;
begin
  xml := TMXmlParser.parseFile(project, [xpDropWhitespace]);
  try
    cfg := xml.docElement;
    po := cfg.element('ProjectOptions');
    vi := po.forceElement('VersionInfo');
    vi.forceElement('MajorVersionNr').attribute['Value'] := inttostr(FMajor);
    vi.forceElement('MinorVersionNr').attribute['Value'] := inttostr(FMinor);
    vi.forceElement('RevisionNr').attribute['Value'] := inttostr(FPatch);
    if (debug) then
      vi.forceElement('Attributes').attribute['pvaDebug'] := 'True'
    else
      vi.forceElement('Attributes').attribute['pvaDebug'] := 'False';

    f := TFileStream.create(project, fmCreate);
    try
      xml.ToXml(f, true);
    finally
      f.free;
    end;
  finally
    xml.free;
  end;
end;

end.

