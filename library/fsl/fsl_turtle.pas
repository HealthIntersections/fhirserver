unit fsl_turtle;

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

uses
  SysUtils, Classes, Character, {$IFDEF DELPHI}RegularExpressions, {$ENDIF} Generics.Collections,
  fsl_base, fsl_utilities, fsl_stream, fsl_fpc;

Const
  GOOD_IRI_CHAR = 'a-zA-Z0-9'; // \u00A0-\uFFFE'; todo
  IRI_URL = '(([a-z])+:)*((%[0-9a-fA-F]{2})|[&''\\(\\)*+,;:@_~?!$\\/\\-\\#.\\='+GOOD_IRI_CHAR+'])+';
  LANG_REGEX = '[a-z]{2}(\\-[a-zA-Z]{2})?';

Type
  TTurtleDocument = class;

  TTurtleObject = class (TFslObject)
  protected
    FStart : TSourceLocation;
    FStop : TSourceLocation;
    function write(b : TStringBuilder; doc : TTurtleDocument; indent : integer) : boolean; virtual; abstract;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(start : TSourceLocation); overload;
    function Link : TTurtleObject; overload;
    function hasValue(value : String) : boolean; virtual; abstract;
    function isSimple : boolean; virtual; abstract;
    function singleLiteral : String; virtual; abstract;
    property start : TSourceLocation read FStart;
    property stop : TSourceLocation read FStop;
  end;

  TTurtleLiteral = class (TTurtleObject)
  private
    Fvalue : String;
    Ftype : String;
  protected
    function write(b : TStringBuilder; doc : TTurtleDocument; indent : integer) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(start : TSourceLocation); overload;
    constructor Create(value : String); overload;
    constructor Create(value, type_ : String); overload;
    function Link : TTurtleLiteral; overload;
    function hasValue(value : String) : boolean; override;
    function singleLiteral : String; override;
    function isSimple : boolean; override;
    property value : String read Fvalue write Fvalue;
    property type_ : String read Ftype write Ftype;
  end;

  TTurtleURL = class (TTurtleObject)
  private
    Furi : String;
    procedure setUri(value : String);
  protected
    function write(b : TStringBuilder; doc : TTurtleDocument; indent : integer) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(start : TSourceLocation); overload;
    constructor Create(uri : String); overload;
    function Link : TTurtleURL; overload;
    property uri : String read Furi write Seturi;
    function hasValue(value : String) : boolean; override;
    function isSimple : boolean; override;
    function singleLiteral : String; override;
  end;

  TTurtleList = class (TTurtleObject)
  private
    Flist : TFslList<TTurtleObject>;
  protected
    function write(b : TStringBuilder; doc : TTurtleDocument; indent : integer) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(start : TSourceLocation); overload;
    constructor Create(start : TSourceLocation; obj : TTurtleObject); overload;
    destructor Destroy; override;
    function Link : TTurtleList; overload;
    function hasValue(value : String) : boolean; override;
    property List : TFslList<TTurtleObject> read FList;
    function isSimple : boolean; override;
    function singleLiteral : String; override;
  end;

  TTurtleComplex = class (TTurtleObject)
  private
    FPredicates : TFslMap<TTurtleObject>;
    FNames : TStringList;
  protected
    function write(b : TStringBuilder; doc : TTurtleDocument; indent : integer) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(start : TSourceLocation); overload;
    destructor Destroy; override;
    function Link : TTurtleComplex; overload;
    function hasValue(value : String) : boolean; override;
    property predicates : TFslMap<TTurtleObject> read FPredicates;
    function addPredicate(uri : String) : TTurtleComplex; overload;
    procedure addPredicate(uri : String; obj : TTurtleObject); overload;
    procedure addPredicate(uri : String; s : String); overload;
    procedure addUriPredicate(uri : String; s : String); overload;
    procedure addPredicate(uri : String; s, t : String); overload;
    procedure addPredicates(values : TFslMap<TTurtleObject>);
    function predicate(uri : String) : TTurtleObject;
    function singleLiteral : String; overload; override;
    function isSimple : boolean; override;
    function stringLiteral(uri : String) : String; overload;
    function complex(uri : String) : TTurtleComplex;
    function has(uri : String) : boolean; overload;
    function has(uri : String; var complex : TTurtleComplex) : boolean; overload;
    function list(uri : String) : TTurtleList;
    function complexes(uri : String) : TArray<TTurtleComplex>;
  end;

  TTurtlePredicate = class (TFslObject)
  private
    FURL : TTurtleURL;
    FValue : TTurtleComplex;
  protected
    function write(b : TStringBuilder; doc : TTurtleDocument; indent : integer) : boolean;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(url : TTurtleURL; value : TTurtleComplex);
    destructor Destroy; override;
    property URL : TTurtleURL read FURL;
    property Value : TTurtleComplex read FValue;
  end;

  TTurtleDocument = class (TFslObject)
  private
    FPrefixes : TFslStringDictionary;
    FObjects : TFslList<TTurtlePredicate>;
    FBase : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property objects : TFslList<TTurtlePredicate> read FObjects;
    property prefixes : TFslStringDictionary read FPrefixes;
    function getPredicate(uri : String) : TTurtlePredicate;
    function getObject(uri : String) : TTurtleComplex;
    procedure addObject(uri : String; obj : TTurtleComplex); overload;
    function prefixIse(s : String; var p : String) : boolean;
    procedure prefix(pfx, ns : String);
  end;

  TLexerTokenType = (
    lttNULL,
    lttTOKEN, // [, ], :, @
    lttWORD, // a word
    lttURI, // a URI <>
    lttLITERAL // "..."
  );

const
  LEX_TYPES : array [TLexerTokenType] of String = ('null', 'token', 'word', 'uri', 'literal');

type
  TTurtleLexer = class (TFslObject)
  private
    FSource : String;
    FType : TLexerTokenType;
    cursor : integer;
    pos, startPos : TSourceLocation;
    Ftoken : String;
    procedure skipWhitespace;
    function grab : char;
    procedure readNext(postColon : boolean);
    function unescape(s : String; isUri : boolean) : string;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(source : String);
    function done : boolean;
    function next(type_ : TLexerTokenType; postColon : boolean): String;
    function peek : String; overload;
    function peekType : TLexerTokenType;
    procedure token(value : String);
    procedure word(value : String); overload;
    function word : String; overload;
    function uri : String; overload;
    function literal : String; overload;
    function peek(type_ : TLexerTokenType; token : String) : boolean; overload;
    procedure error(message : String);
  end;

  TTurtleParser = class (TFslObject)
  private
    class procedure parse(lexer : TTurtleLexer; doc : TTurtleDocument); overload;
    class function anonymousId : TTurtleURL;
    class function parseComplex(lexer : TTurtleLexer; doc : TTurtleDocument) : TTurtleComplex;
  public
    class function parse(source : String) : TTurtleDocument; overload;
    class function parse(source : TStream) : TTurtleDocument; overload;
    class function parse(source : TFslStream) : TTurtleDocument; overload;
  end;

  TTurtleComposer = class (TFslObject)
  public
    class procedure writePrefixes(b : TStringBuilder; doc : TTurtleDocument);
  public
    class function compose(doc : TTurtleDocument) : String; overload;
    class procedure compose(doc : TTurtleDocument; dest : TStream); overload;
    class procedure compose(doc : TTurtleDocument; dest : TFslStream); overload;
  end;

function ttlLiteral(s : String) : String;

implementation

function ttlLiteral(s : String) : String;
begin
  result := s; // '"'+jsonEscape(s, true)+'"';
end;

function sorted(list: TEnumerable<String>): TArray<String>;
var
  sort : TStringList;
  s : String;
begin
  sort := TStringList.Create;
  try
    for s in list do
      sort.Add(s);
    sort.Sort;
    result := sort.ToStringArray;
  finally
    sort.Free;
  end;
end;

procedure ln(b: TStringBuilder; line: String);
begin
  b.Append(line);
  b.Append(#13#10);
end;

{ TTurtleObject }

constructor TTurtleObject.Create(start : TSourceLocation);
begin
  inherited Create;
  FStart := start;
end;

function TTurtleObject.Link: TTurtleObject;
begin
  result := TTurtleObject(inherited Link);
end;

function TTurtleObject.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TTurtleLiteral }

constructor TTurtleLiteral.Create(start: TSourceLocation);
begin
  inherited Create(start);
end;

constructor TTurtleLiteral.Create(value: String);
begin
  inherited Create(TSourceLocation.CreateNull);
  FValue := value;
end;

constructor TTurtleLiteral.Create(value, type_: String);
begin
  inherited Create(TSourceLocation.CreateNull);
  FValue := value;
  FType := type_;
end;

function TTurtleLiteral.hasValue(value: String): boolean;
begin
  result := Fvalue = value;
end;

function TTurtleLiteral.isSimple: boolean;
begin
  result := true;
end;

function TTurtleLiteral.Link: TTurtleLiteral;
begin
  result := TTurtleLiteral(inherited Link);
end;

function TTurtleLiteral.singleLiteral: String;
begin
  result := FValue;
end;

function TTurtleLiteral.write(b: TStringBuilder; doc: TTurtleDocument; indent: integer): boolean;
var
  p : String;
  c : char;
begin
  if (StringIsInteger32(FValue)) and (FType = 'int') then
    b.Append(FValue)
  else
  begin
    b.Append('"');
    for c in FValue do
    begin
      if CharInSet(c, ['"', '\']) then
        b.Append('\');
      b.Append(c);
    end;
    b.Append('"');
  end;
  if (FType <> '') and (FType <> 'int') then
  begin
    if doc.prefixIse(FType, p) then
      b.Append('^^'+p)
    else
      b.Append('^^<'+FType+'>');
  end;
  result := false;
end;

function TTurtleLiteral.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (Fvalue.length * sizeof(char)) + 12);
  inc(result, (Ftype.length * sizeof(char)) + 12);
end;

{ TTurtleURL }

constructor TTurtleURL.Create(start: TSourceLocation);
begin
  inherited Create(start);
end;

constructor TTurtleURL.Create(uri: String);
begin
  inherited Create(TSourceLocation.CreateNull);
  self.uri := uri;
end;

function TTurtleURL.hasValue(value: String): boolean;
begin
  result := FUri = value;
end;

function TTurtleURL.isSimple: boolean;
begin
  result := true;
end;

function TTurtleURL.Link: TTurtleURL;
begin
  result := TTurtleURL(inherited Link);
end;

procedure TTurtleURL.setUri(value: String);
begin
  if (not TRegEx.isMatch(value, IRI_URL)) then
    raise ERdfException.create('Illegal URI '+value);
  FUri := value;
end;

function TTurtleURL.singleLiteral: String;
begin
  result := Furi;
end;

function TTurtleURL.write(b: TStringBuilder; doc: TTurtleDocument; indent: integer): boolean;
var
  s : String;
begin
  if doc.prefixIse(Furi, s) then
    b.Append(s)
  else
  begin
    b.Append('<');
    b.Append(Furi);
    b.Append('>');
  end;
  result := false;
end;

function TTurtleURL.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (Furi.length * sizeof(char)) + 12);
end;

{ TTurtleList }

constructor TTurtleList.Create(start : TSourceLocation; obj: TTurtleObject);
begin
  Inherited Create(start);
  FList := TFslList<TTurtleObject>.create;
  FList.Add(obj.Link);
end;

constructor TTurtleList.Create(start : TSourceLocation);
begin
  Inherited Create(start);
  FList := TFslList<TTurtleObject>.create;
end;

destructor TTurtleList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TTurtleList.hasValue(value: String): boolean;
var
  obj : TTurtleObject;
begin
  for obj in Flist do
    if (obj.hasValue(value)) then
      exit(true);
  exit(false);
end;

function TTurtleList.isSimple: boolean;
begin
  result := (Flist.Count = 0) or ((Flist.Count = 1) and Flist[0].isSimple);
end;

function TTurtleList.Link: TTurtleList;
begin
  result := TTurtleList(inherited Link);
end;

function TTurtleList.singleLiteral: String;
begin
  if Flist.Count = 0 then
    result := FList[0].singleLiteral
  else
    raise EWebException.create('Error finding single value: '+inttostr(FList.count)+' not found');
end;

function TTurtleList.write(b: TStringBuilder; doc: TTurtleDocument; indent: integer): boolean;
var
  first : boolean;
  obj : TTurtleObject;
begin
  if FList.Count = 0 then
  begin
    b.Append('(');
    b.Append(')');
    result := false
  end
  else if FList.Count = 1 then
    result := Flist[0].write(b, doc, indent)
  else
  begin
    first := true;
    result := false;
    for obj in FList do
    begin
      if first then
        first := false
      else
        b.Append(', ');
      result := obj.write(b, doc, indent) or result;
    end;
  end;
end;

function TTurtleList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Flist.sizeInBytes);
end;

{ TTurtleComplex }

constructor TTurtleComplex.Create(start : TSourceLocation);
begin
  inherited;
  FPredicates := TFslMap<TTurtleObject>.create('Turtle Complex');
  FNames := TStringList.Create;
  Fnames.Duplicates := dupIgnore;
end;

destructor TTurtleComplex.Destroy;
begin
  FNames.Free;
  FPredicates.Free;
  inherited;
end;

function TTurtleComplex.Link: TTurtleComplex;
begin
  result := TTurtleComplex(inherited Link);
end;

function TTurtleComplex.list(uri: String): TTurtleList;
var
  obj : TTurtleObject;
  list : TTurtleList;
begin
  obj := FPredicates[uri];
  if obj = nil then
    exit(nil);
  if (obj is TTurtleList) then
    exit(TTurtleList(obj));
  list := TTurtleList.Create(obj.FStart);
  list.Flist.Add(obj.Link);
  FPredicates.AddOrSetValue(uri, list);
  exit(list);
end;

function TTurtleComplex.predicate(uri: String): TTurtleObject;
begin
  if not FPredicates.TryGetValue(uri, result) then
    result := nil;
end;

function TTurtleComplex.stringLiteral(uri: String): String;
begin
  if not has(uri) then
    result := ''
  else
    result := predicates[uri].singleLiteral;
end;

function TTurtleComplex.write(b: TStringBuilder; doc: TTurtleDocument; indent: integer): boolean;
var
  left : String;
  i : integer;
  key, s : String;
  prop : TTurtleObject;
begin
  if (predicates.Count = 0) then
    exit(false);

  if (indent > 0) then
    b.Append(' [ ');
  if (predicates.Count = 1) then
  begin
    for key in FNames do
      if predicates.TryGetValue(key, prop) then
      begin
        if (prop.isSimple) then
        begin
          if doc.prefixIse(key, s) then
            b.Append(' '+s+' ')
          else
            b.Append(' <'+key+'> ');
          prop.write(b, doc, indent);
          if (indent > 0) then
            b.Append(' ] ');
          exit(false);
        end;
      end;
  end;

  result := true;
  left := StringPadLeft('', ' ', indent+2);
  i := 0;
  for key in FNames do
    if predicates.TryGetValue(key, prop) then
    begin
      if (indent > 0) or (i > 0) then
        b.Append(#13#10+left);
      if (key = 'a') then
        b.Append(' '+key+' ')
      else if doc.prefixIse(key, s) then
        b.Append(' '+s+' ')
      else
        b.Append(' <'+key+'> ');
      prop.write(b, doc, indent+2);
      b.Append(' ');
      inc(i);
      if (i < predicates.count) then
        b.Append(' ;')
    end;
  if (indent > 0) then
    b.Append(#13#10+StringPadLeft('', ' ', indent)+'] ');
end;

function TTurtleComplex.singleLiteral: String;
begin
  raise ERdfException.create('Cannot convert a complex to a single value');
end;

function TTurtleComplex.has(uri: String): boolean;
begin
  result := predicates.ContainsKey(uri);
end;

function TTurtleComplex.has(uri: String; var complex: TTurtleComplex): boolean;
var
  o : TTurtleObject;
begin
  result := predicates.TryGetValue(uri, o);
  if (result) then
    if o is TTurtleComplex then
      complex := TTurtleComplex(o)
    else
      result := false;
end;

function TTurtleComplex.hasValue(value: String): boolean;
begin
  result := false;
end;

function TTurtleComplex.isSimple: boolean;
begin
  result := false;
end;

procedure TTurtleComplex.addPredicate(uri: String; obj: TTurtleObject);
var
  eo : TTurtleObject;
  list : TTurtleList;
begin
  if (not predicates.containsKey(uri)) then
  begin
    predicates.add(uri, obj);
    FNames.Add(uri);
  end
  else
  begin
    eo := predicates[uri];
    if (eo is TTurtleList) then
      list := eo as TTurtleList
    else
    begin
      list := TTurtleList.Create(TSourceLocation.CreateNull, eo);
      predicates.AddOrSetValue(uri, list);
    end;
    list.list.add(obj);
  end;
end;

procedure TTurtleComplex.addPredicate(uri, s: String);
begin
  addPredicate(uri, TTurtleLiteral.Create(s));
end;

procedure TTurtleComplex.addPredicate(uri, s, t: String);
begin
  addPredicate(uri, TTurtleLiteral.Create(s, t));
end;

function TTurtleComplex.addPredicate(uri: String): TTurtleComplex;
begin
  result := TTurtleComplex.Create(TSourceLocation.CreateNull);
  addPredicate(uri, result);
end;

procedure TTurtleComplex.addPredicates(values: TFslMap<TTurtleObject>);
begin
  FPredicates.addAll(values);
end;

procedure TTurtleComplex.addUriPredicate(uri, s: String);
begin
  addPredicate(uri, TTurtleUrl.Create(s));
end;

function TTurtleComplex.complex(uri: String): TTurtleComplex;
var
  obj : TTurtleObject;
begin
  if not predicates.TryGetValue(uri, obj) then
    exit(nil);
  if not (obj is TTurtleComplex) then
    raise EWebException.create('Wrong Type of Turtle object- looking for a complex for '+uri);
  result := TTurtleComplex(obj);
end;

function TTurtleComplex.complexes(uri: String): TArray<TTurtleComplex>;
var
  obj : TTurtleObject;
  list : TTurtleList;
  i : integer;
begin
  SetLength(result, 0);
  if predicates.TryGetValue(uri, obj) then
  begin
    if (obj <> nil) and (obj is TTurtleList) then
    begin
      list := TTurtleList(obj);
      SetLength(result, list.Flist.Count);
      for i := 0 to list.Flist.Count - 1 do
        if list.Flist[i] is TTurtleComplex then
          result[i] := list.Flist[i] as TTurtleComplex;
    end
    else if (obj is TTurtleComplex) then
    begin
      SetLength(result, 1);
      result[0] := obj as TTurtleComplex;
    end;
  end;
end;

function TTurtleComplex.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPredicates.sizeInBytes);
  inc(result, FNames.sizeInBytes);
end;

{ TTurtleLexer }

constructor TTurtleLexer.Create(source: String);
begin
  Fsource := source;
  cursor := 1;
  pos := TSourceLocation.Create;
  readNext(false);
end;

procedure TTurtleLexer.skipWhitespace();
var
  ch : char;
begin
  while (cursor <= FSource.length) do
  begin
    ch := Fsource[cursor];
    if (ch.isWhitespace) then
      grab()
    else if (ch = '#') then
    begin
      ch := grab();
      while (cursor <= FSource.length) do
      begin
        ch := grab();
        if (ch = #13) or (ch = #10) then
          break;
      end;
    end
    else
      break;
  end;
end;

function TTurtleLexer.grab() : char;
var
  ch : char;
begin
  ch := FSource[cursor];
  if (ch = #10) then
    pos.incLine
  else
    pos.incCol;

  inc(cursor);
  exit(ch);
end;

procedure TTurtleLexer.readNext(postColon : boolean);
var
  ch : char;
  b : TStringBuilder;
  e, s : String;
begin
  Ftoken := '';
  Ftype := lttNULL;
  skipWhitespace();
  if (cursor > FSource.length)  then
    exit;
  startPos := pos;
  ch := grab();
  b := TStringBuilder.create();
  try
    case ch of
      '@', '.', ':', ';', '^', ',', ']', '[', '(', ')':
      begin
        Ftype := lttTOKEN;
        b.append(ch);
        Ftoken := b.toString();
        exit;
      end;
      '<':
      begin
        while (cursor <= FSource.length) do
        begin
          ch := grab();
          if (ch = '>') then
            break;
          b.append(ch);
        end;
        Ftype := lttURI;
        Ftoken := unescape(b.toString(), true);
        exit;
      end;
      '"':
      begin
        b.append(ch);
        e := '"';
        while (cursor < FSource.length) do
        begin
          ch := grab();
          if (b.length = 2) and (ch <> '"') and (b.ToString = '""') then
          begin
            dec(cursor);
            break;
          end;
          b.append(ch);
          s := b.toString();
          if (s = '"""') then
            e := '"""'
          else if (s <> '""') and (copy(s, length(s)-length(e)+1, length(e)) = e) and (not (copy(s, length(s)-length(e), length(e)+1) = '\'+e) or (copy(s, length(s)-length(e)-1, length(e)+2) = '\\'+e)) then
            break;
        end;
        Ftype := lttLITERAL;
        Ftoken := unescape(b.toString().substring(e.length, b.length-e.length*2), false);
        exit;
      end;
      '''':
      begin
        b.append(ch);
        e := '''';
        while (cursor < FSource.length) do
        begin
          ch := grab();
          if (b.ToString = '''''') and (ch <> '''') then
          begin
            dec(cursor);
            break;
          end;
          b.append(ch);
          s := b.toString();
          if (s = '''''''') then
            e := ''''''''
          else
          begin
            if (s <> '''''') and (copy(s, length(s)-length(e)+1, length(e)) = e) then
              break;
          end;
        end;
        Ftype := lttLITERAL;
        Ftoken := unescape(b.toString().substring(e.length, b.length-e.length*2), false);
        exit;
      end;
      else
        if CharInSet(ch, ['0'..'9', 'a'..'z', 'A'..'Z', '_', '-', '+', '%']) then
        begin
          b.append(ch);
          while (cursor <= FSource.length) do
          begin
            ch := grab();
            if ch.isWhitespace or CharInSet(ch, [';', ']', ')', '~']) or ((( ch = ':')) and (not postColon)) then
              break;
            b.append(ch);
          end;
          Ftype := lttWORD;
          Ftoken := b.toString();
          dec(cursor);
          exit;
        end
        else
          raise EWebException.create('unexpected lexer char '+ch);
      end;
  finally
    b.Free;
  end;
end;

function TTurtleLexer.unescape(s : String; isUri : boolean) : String;
var
  b : TStringBuilder;
  i, l, uc : integer;
  ch : char;
  n : String;
begin
  b := TStringBuilder.create();
  try
    i := 1;
    while (i <= s.length) do
    begin
      ch := s[i];
      if (ch = '\') and (i < s.length) then
      begin
        inc(i);
        case (s[i]) of
        't': b.append(#9);
        'r': b.append(#13);
        'n': b.append(#10);
        'f': b.append(#12);
        '''': b.append('''');
        '"': b.append('"');
        '\': b.append('\');
        '/': b.append('/');
        'U', 'u':
        begin
          inc(i);
          l := 4;
          n := s.substring(i-1, l);
          uc := StrToInt('$'+n);
          if (isUri and (uc < 33)) or (not isUri and (uc < 32)) then
          begin
            l := 8;
            uc := StrToInt('$'+s.substring(i-1, l));
          end;
          if ((isUri and (uc < 33)) or (not isUri and (uc < 32))) or ((isUri) and ((uc = $3C) or (uc = $3E))) then
            raise EWebException.create('Illegal unicode character');
          b.append(char(uc));
          inc(i);
        end;
        else
          raise EWebException.create('Unknown character escape \\'+s[i]);
        end;
      end
      else
        b.append(ch);
      inc(i);
    end;
    result := b.toString();
  finally
    b.Free;
  end;
end;

function TTurtleLexer.done() : boolean;
begin
  result := Ftype = lttNULL;
end;

function TTurtleLexer.next(type_ : TLexerTokenType; postColon : boolean) : String;
begin
  if (type_ <> lttNULL) and (Ftype <> type_) then
    error('Unexpected type. Found '+LEX_TYPES[FType]+' looking for a '+LEX_TYPES[type_]);
  result := Ftoken;
  readNext(postColon);
end;

function TTurtleLexer.peek() : String;
begin
  result := Ftoken;
end;

function TTurtleLexer.peekType() : TLexerTokenType;
begin
  result := Ftype;
end;

procedure TTurtleLexer.token(value : String);
begin
  if (value <> Ftoken) then
    error('Unexpected word '+Ftoken+' looking for '+value);
  next(lttTOKEN, Ftoken = ':');
end;

procedure TTurtleLexer.word(value : String);
begin
  if (value <> Ftoken) then
    error('Unexpected word '+Ftoken+' looking for '+value);
  next(lttWORD, false);
end;

function TTurtleLexer.word() : String;
begin
  result := Ftoken;
  next(lttWORD, false);
end;

function TTurtleLexer.uri() : String;
begin
  if (Ftype <> lttURI) then
    error('Unexpected type. Found '+LEX_TYPES[FType]+' looking for a URI');
  result := Ftoken;
  next(lttURI, false);
end;

function TTurtleLexer.literal() : String;
begin
  if (Ftype <> lttLITERAL) then
    error('Unexpected type. Found '+LEX_TYPES[FType]+' looking for a Literal');
  result := Ftoken;
  next(lttLITERAL, false);
end;

function TTurtleLexer.peek(type_ : TLexerTokenType; token : String) : boolean;
begin
  result := (Ftype = type_) and (Ftoken = token);
end;

procedure TTurtleLexer.error(message : String);
begin
  raise pos.exception('Syntax Error parsing Turtle : '+message);
end;

function TTurtleLexer.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSource.length * sizeof(char)) + 12);
  inc(result, (Ftoken.length * sizeof(char)) + 12);
end;

{ TTurtleParser }

class function TTurtleParser.parse(source : String) : TTurtleDocument;
var
  lexer : TTurtleLexer;
begin
  result := TTurtleDocument.Create;
  try
    lexer := TTurtleLexer.Create(source);
    try
      parse(lexer, result);
    finally
      lexer.Free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

class function TTurtleParser.parse(source : TStream) : TTurtleDocument;
var
  lexer : TTurtleLexer;
begin
  result := TTurtleDocument.Create;
  try
    lexer := TTurtleLexer.Create(StreamToString(source, TEncoding.UTF8));
    try
      parse(lexer, result);
    finally
      lexer.Free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

class function TTurtleParser.parse(source : TFslStream) : TTurtleDocument;
var
  lexer : TTurtleLexer;
begin
  result := TTurtleDocument.Create;
  try
    lexer := TTurtleLexer.Create(StreamToString(source, TEncoding.UTF8));
    try
      parse(lexer, result);
    finally
      lexer.Free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

class procedure TTurtleParser.parse(lexer: TTurtleLexer; doc : TTurtleDocument);
var
  doPrefixes, sparqlStyle, base : boolean;
  p, sprefix, url, pfx : String;
  uri : TTurtleURL;
  complex, bnode : TTurtleComplex;
begin
  doPrefixes := true;
  while (not lexer.done()) do
  begin
    if (doPrefixes) and ((lexer.peek(lttTOKEN, '@')) or (lexer.peek(lttWORD, 'PREFIX')) or (lexer.peek(lttWORD, 'BASE'))) then
    begin
      sparqlStyle := false;
      base := false;
      if (lexer.peek(lttTOKEN, '@')) then
      begin
        lexer.token('@');
        p := lexer.word();
        if (p = 'base') then
          base := true
        else if (p <> 'prefix') then
          raise EWebException.create('Unexpected token '+p);
      end
      else
      begin
        sparqlStyle := true;
        p := lexer.word();
        if (p = 'BASE') then
          base := true
        else if (p <> 'PREFIX') then
          raise EWebException.create('Unexpected token '+p);
      end;
      sprefix := '';
      if (not base) then
      begin
        if lexer.peekType() = lttWORD then
          sprefix := lexer.next(lttWORD, false);
        lexer.token(':');
      end;
      url := lexer.next(lttURI, false);
      if (not sparqlStyle) then
        lexer.token('.');
      if (not base) then
        doc.prefixes.AddOrSetValue(sprefix, url)
      else if (doc.Fbase = '') then
        doc.Fbase := url
      else
        raise EWebException.create('Duplicate @base');
    end
    else if (lexer.peekType() = lttURI) then
    begin
      doPrefixes := false;
      uri := TTurtleURL.create(lexer.startPos);
      try
        uri.Uri := lexer.uri();
        complex := parseComplex(lexer, doc);
        doc.Fobjects.add(TTurtlePredicate.Create(uri.Link, complex));
      finally
        uri.Free;
      end;
      lexer.token('.');
    end
    else if (lexer.peekType() = lttWORD) then
    begin
      doPrefixes := false;
      uri := TTurtleURL.create(lexer.startPos);
      try
        pfx := lexer.word();
        if (not doc.prefixes.containsKey(pfx)) then
          raise EWebException.create('Unknown prefix '+pfx);
        lexer.token(':');
        uri.Uri := doc.prefixes[pfx]+lexer.word();
        complex := parseComplex(lexer, doc);
        doc.Fobjects.add(TTurtlePredicate.Create(uri.Link, complex));
      finally
        uri.free;
      end;
      lexer.token('.');
    end
    else if (lexer.peek(lttTOKEN, ':')) then
    begin
      doPrefixes := false;
      uri := TTurtleURL.create(lexer.startPos);
      try
        lexer.token(':');
        if (not doc.prefixes.containsKey('')) then
          raise EWebException.create('Unknown prefix ''''');
        uri.Uri := doc.prefixes['']+lexer.word();
        complex := parseComplex(lexer, doc);
        doc.Fobjects.add(TTurtlePredicate.Create(uri.Link, complex));
      finally
        uri.Free;
      end;
      lexer.token('.');
    end
    else if (lexer.peek(lttTOKEN, '[')) then
    begin
      doPrefixes := false;
      lexer.token('[');
      bnode := parseComplex(lexer, doc);
      try
        lexer.token(']');
        if (not lexer.peek(lttTOKEN, '.')) then
        begin
          complex := parseComplex(lexer, doc);
          try
            // at this point, we collapse bnode and complex, and give bnode a fictional identity
            bnode.addPredicates(complex.predicates);
          finally
            complex.Free;
          end;
        end;

        doc.Fobjects.add(TTurtlePredicate.Create(anonymousId(), bnode.Link));
      finally
        bnode.Free;
      end;
      lexer.token('.');
    end
    else
      lexer.error('Unknown token '+lexer.Ftoken);
  end;
end;

class function TTurtleParser.parseComplex(lexer: TTurtleLexer; doc : TTurtleDocument): TTurtleComplex;
var
  done, inlist, rpt : boolean;
  uri, t, l, lang, pfx : string;
  u : TTurtleURL;
  ul : TTurtleLiteral;
  sp : TSourceLocation;
begin
  inlist := false;
  result := TTurtleComplex.create(lexer.startPos);
  try
    done := lexer.peek(lttTOKEN, ']');
    while (not done) do
    begin
      uri := '';
      if (lexer.peekType() = lttURI) then
        uri := lexer.uri()
      else
      begin
        if lexer.peekType() = lttWORD then
          t := lexer.word()
        else
          t := '';
        if (lexer.Ftype = lttTOKEN) and (lexer.Ftoken = ':') then
        begin
          lexer.token(':');
          if (not doc.prefixes.containsKey(t)) then
            lexer.error('unknown prefix '+t);
          uri := doc.prefixes[t]+lexer.word();
        end
        else if (t = 'a') then
        begin
          if doc.prefixes.containsKey('rdfs') then
            uri := doc.prefixes['rdfs']+'type'
          else
            uri := 'http://www.w3.org/2000/01/rdf-schema#type'
        end
        else
          lexer.error('unexpected token "'+t+'"');
      end;

      if (lexer.peek(lttTOKEN, '(')) then
      begin
        inlist := true;
        result.addPredicate(uri, TTurtleList.Create(lexer.startPos));
        lexer.token('(');
      end;

      repeat
        if (lexer.peek(lttTOKEN, '[')) then
        begin
          lexer.token('[');
          result.addPredicate(uri, parseComplex(lexer, doc));
          lexer.token(']');
        end
        else if (lexer.peekType() = lttURI) then
        begin
          u := TTurtleURL.Create(lexer.startPos);
          try
            u.Uri := lexer.uri();
            result.addPredicate(uri, u.Link);
          finally
            u.Free;
          end;
        end
        else if (lexer.peekType() = lttLITERAL) then
        begin
          ul := TTurtleLiteral.create(lexer.startPos);
          try
            ul.value := lexer.literal();
            if (lexer.peek(lttTOKEN, '^')) then
            begin
              lexer.token('^');
              lexer.token('^');
              if (lexer.peekType() = lttURI) then
                ul.type_ := lexer.uri()
              else
              begin
                l := lexer.word();
                lexer.token(':');
                ul.type_ := doc.prefixes[l]+ lexer.word();
              end;
            end;
            if (lexer.peek(lttTOKEN, '@')) then
            begin
              //lang tag - skip it
              lexer.token('@');
              lang := lexer.word();
              if (not TRegEx.IsMatch(lang, LANG_REGEX)) then
                lexer.error('Invalid Language tag '+lang);
            end;
            result.addPredicate(uri, ul.Link);
          finally
            ul.free;
          end;
        end
        else if (lexer.peekType() = lttWORD) or (lexer.peek(lttTOKEN, ':')) then
        begin
          sp := lexer.startPos;
          if lexer.peekType() = lttWORD then pfx := lexer.word() else pfx := '';
          if StringIsDecimal(pfx) and not lexer.peek(lttTOKEN, ':') then
          begin
            ul := TTurtleLiteral.create(sp);
            try
              ul.value := pfx;
              result.addPredicate(uri, ul.Link);
            finally
              ul.Free;
            end;
          end
          else if (('false'.equals(pfx)) or ('true'.equals(pfx))) and (not lexer.peek(lttTOKEN, ':')) then
          begin
            ul := TTurtleLiteral.create(sp);
            try
              ul.value := pfx;
              result.addPredicate(uri, ul.Link);
            finally
              ul.Free;
            end;
          end
          else
          begin
            if (not doc.prefixes.containsKey(pfx)) then
              lexer.error('Unknown prefix "'+pfx+'"');
            u := TTurtleURL.Create(sp);
            try
              lexer.token(':');
              u.setUri(doc.prefixes[pfx]+lexer.word());
              result.addPredicate(uri, u.Link);
            finally
              u.Free;
            end;
          end;
        end
        else if (not lexer.peek(lttTOKEN, ';')) and ((not inlist) or (not lexer.peek(lttTOKEN, ')'))) then
          lexer.error('unexpected token '+lexer.Ftoken);

        if (inlist) then
          rpt := not lexer.peek(lttTOKEN, ')')
        else
        begin
          rpt := lexer.peek(lttTOKEN, ',');
          if (rpt) then
            lexer.readNext(false);
        end;
      until not rpt;
      if (inlist) then
        lexer.token(')');

      if (lexer.peek(lttTOKEN, ';')) then
      begin
        while ((lexer.peek(lttTOKEN, ';'))) do
          lexer.token(';');
        done := lexer.peek(lttTOKEN, '.') or lexer.peek(lttTOKEN, ']');
      end
      else
        done := true;
    end;
    result.Link;
  finally
    result.free;
  end;
end;

class function TTurtleParser.anonymousId() : TTurtleURL;
begin
  result := TTurtleURL.create(TSourceLocation.CreateNull);
  result.Uri := NewGuidURN;
end;


{ TTurtlePredicate }

constructor TTurtlePredicate.Create(url: TTurtleURL; value: TTurtleComplex);
begin
  inherited Create;
  FURL := url;
  FValue := value;
end;

destructor TTurtlePredicate.Destroy;
begin
  FURL.Free;
  FValue.Free;
  inherited;
end;

function TTurtlePredicate.write(b: TStringBuilder; doc: TTurtleDocument; indent: integer): boolean;
begin
//  FURL.write(b, doc, indent);
  b.append('<');
  b.append(FURL.uri);
  b.append('>');
  b.append(' ');
  result := FValue.write(b, doc, indent);
  b.append(' .');
  b.append(#13#10);
end;

function TTurtlePredicate.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FURL.sizeInBytes);
  inc(result, FValue.sizeInBytes);
end;

{ TTurtleDocument }

procedure TTurtleDocument.addObject(uri: String; obj: TTurtleComplex);
begin
  FObjects.Add(TTurtlePredicate.Create(TTurtleURL.Create(uri), obj));
end;

constructor TTurtleDocument.Create;
begin
  inherited;
  FObjects := TFslList<TTurtlePredicate>.create;
  FPrefixes := TFslStringDictionary.create;
  FPrefixes.clear();
  FPrefixes.add('_', 'urn:uuid:4425b440-2c33-4488-b9fc-cf9456139995#');
end;

destructor TTurtleDocument.Destroy;
begin
  FPrefixes.Free;
  FObjects.Free;
  inherited;
end;

function TTurtleDocument.getObject(uri: String): TTurtleComplex;
var
  t : TTurtlePredicate;
begin
  for t in objects do
    if (t.FURL.uri = uri) then
      exit(t.FValue);
  exit(nil);
end;

function TTurtleDocument.getPredicate(uri: String): TTurtlePredicate;
var
  t : TTurtlePredicate;
begin
  for t in objects do
    if (t.FURL.uri = uri) then
      exit(t);
  exit(nil);
end;

procedure TTurtleDocument.prefix(pfx, ns: String);
begin
  FPrefixes.AddOrSetValue(pfx, ns);
end;

function TTurtleDocument.prefixIse(s: String; var p : String) : boolean;
var
  pfx, ns : string;
begin
  result := false;
  for pfx in prefixes.Keys do
  begin
    ns := prefixes[pfx];
    if s.StartsWith(ns) then
    begin
      p := pfx+':'+s.Substring(ns.Length);
      exit(true);
    end;
    if s.StartsWith(pfx+':') then
    begin
      p := s;
      exit(true);
    end;
  end;
end;

function TTurtleDocument.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPrefixes.sizeInBytes);
  inc(result, FObjects.sizeInBytes);
  inc(result, (FBase.length * sizeof(char)) + 12);
end;

{ TTurtleComposer }

class function TTurtleComposer.compose(doc: TTurtleDocument): String;
var
  b : TStringBuilder;
  pred :  TTurtlePredicate;
begin
  b := TStringBuilder.create;
  try
    writePrefixes(b, doc);
    for pred in doc.objects do
      pred.write(b, doc, 0);
    result := b.ToString;
  finally
    b.Free;
  end;
end;

class procedure TTurtleComposer.compose(doc: TTurtleDocument; dest: TStream);
begin
  StringToStream(compose(doc), dest, TEncoding.UTF8);
end;

class procedure TTurtleComposer.compose(doc: TTurtleDocument; dest: TFslStream);
begin
  StringToStream(compose(doc), dest, TEncoding.UTF8);
end;

class procedure TTurtleComposer.writePrefixes(b: TStringBuilder; doc: TTurtleDocument);
var
  p : String;
  done : boolean;
begin
  done := false;
  for p in sorted(doc.prefixes.Keys) do
    if (p <> '_') then
    begin
      ln(b, '@prefix '+p+': <'+doc.prefixes[p]+'> .');
      done := true;
    end;
  if done then
    ln(b, '');
end;

end.
