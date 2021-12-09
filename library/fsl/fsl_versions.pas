unit fsl_versions;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_utilities, fsl_xml;

type
  ESemVerException = class (Exception);

  TSemanticVersionLevel = (semverMajor, semverMinor, semverPatch, semverLabel);

  { TSemanticVersion }

  TSemanticVersion = class (TFslObject)
  private
    FBuildLabel: String;
    FMajor: integer;
    FMinor: integer;
    FPatch: integer;
    procedure SetBuildLabel(const value: String);

    procedure applyToDelphiProject(project : String; debug : boolean);
    procedure applyToLazarusProject(project : String; debug : boolean);
  public
    property Major : integer read FMajor write FMajor;
    property Minor : integer read FMinor write FMinor;
    property Patch : integer read FPatch write FPatch;
    property BuildLabel : String read FBuildLabel write SetBuildLabel;

    class function isValid(ver : String) : boolean;

    class function fromString(ver : String) : TSemanticVersion;
    function ToString : String; overload; override;
    function ToString(level : TSemanticVersionLevel) : String; overload;

    class function getMajMin(v : string) : String; overload;

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
begin
  try
    o := TSemanticVersion.create;
    try
      result := true;
    finally
      o.free;
    end;
  except
    result := false;
  end;
end;

class function TSemanticVersion.fromString(ver: String): TSemanticVersion;
var
  c : integer;
  parts : TArray<string>;
  p, l, r: string;
begin
  if (ver = '') then
    exit(nil);
  if (SameText(ver, 'r2')) then
    result := TSemanticVersion.fromString('1.0.2')
  else if (SameText(ver, 'r2b')) then
    result := TSemanticVersion.fromString('1.4.0')
  else if (SameText(ver, 'r3')) then
    result := TSemanticVersion.fromString('1.4.0')
  else if (SameText(ver, 'r4')) then
    result := TSemanticVersion.fromString('3.0.2')
  else if (SameText(ver, 'r4b')) then
    result := TSemanticVersion.fromString('4.0.1')
  else if (SameText(ver, 'r5')) then
    result := TSemanticVersion.fromString('4.6.0')
  else
  begin
    c := ver.CountChar('.');
    if (c < 1) or (c > 2) then
      raise ESemVerException.create('Error reading SemVer: Structure "'+ver+'" is not correct');

    result := TSemanticVersion.create;
    try
      parts := ver.Split(['.']);
      if (length(parts) = 3) and (parts[2].contains('-')) then
      begin
        StringSplit(parts[2], '-', l, r);
        result.FBuildLabel := r;
        parts[2] := l;
      end;
      if StrToIntDef(parts[0], -1) = -1 then
        raise ESemVerException.create('Error reading SemVer: Major "'+parts[0]+'" is not an integer')
      else
        result.FMajor := StrToInt(parts[0]);
      if StrToIntDef(parts[1], -1) = -1 then
        raise ESemVerException.create('Error reading SemVer: Minor "'+parts[1]+'" is not an integer')
      else
        result.FMinor := StrToInt(parts[1]);
      if (length(parts) = 3) then
        if StrToIntDef(parts[2], -1) = -1 then
          raise ESemVerException.create('Error reading SemVer: Patch "'+parts[2]+'" is not an integer')
        else
          result.FPatch := StrToInt(parts[2]);
      result.Link;
    finally
      result.free;
    end;
  end;
end;

function TSemanticVersion.ToString: String;
begin
  result := inttostr(FMajor)+'.'+inttostr(FMinor);
  if FPatch > -1 then
    result := result +'.'+inttostr(FPatch);
  if FBuildLabel > '' then
    result := result +'-'+FBuildLabel;
end;

function TSemanticVersion.ToString(level: TSemanticVersionLevel): String;
begin
  result := inttostr(FMajor);
  if level in [semverMinor, semverPatch, semverLabel] then
    result := result + '.' + inttostr(FMinor);
  if (level in [semverPatch, semverLabel]) and (FPatch > -1) then
    result := result + '.' + inttostr(FPatch);
  if (level in [semverLabel]) and (FBuildLabel <> '') then
    result := result + '-' + FBuildLabel;
end;

class function TSemanticVersion.getMajMin(v: string): String;
var
  this : TSemanticVersion;
begin
  if v = '' then
    exit('');

  try
    this := fromString(v);
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
    o1 := fromString(v1);
    try
      o2 := fromString(v2);
      try
        result := o1.matches(o2, level);
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
begin
  case level of
    semverMajor :
        result := (FMajor = other.FMajor);
    semverMinor :
      result := (FMajor = other.FMajor) and (FMinor = other.FMinor);
    semverPatch :
      result := (FMajor = other.FMajor) and (FMinor = other.FMinor) and (FPatch = other.FPatch);
    semverLabel :
      result := (FMajor = other.FMajor) and (FMinor = other.FMinor) and (FPatch = other.FPatch) and (FBuildLabel = other.FBuildLabel);
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
    list := TFslList<TMXmlElement>.create;
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

