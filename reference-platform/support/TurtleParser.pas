unit TurtleParser;

interface

uses
  SysUtils, System.Character, System.RegularExpressions, System.Generics.Collections,
  DecimalSupport, GuidSupport,
  AdvObjects, AdvGenerics;

Const
	GOOD_IRI_CHAR = 'a-zA-Z0-9'; // \u00A0-\uFFFE'; todo
  IRI_URL = '(([a-z])+:)*((%[0-9a-fA-F]{2})|[&''\\(\\)*+,;:@_~?!$\\/\\-\\#.\\='+GOOD_IRI_CHAR+'])+';
  LANG_REGEX = '[a-z]{2}(\\-[a-zA-Z]{2})?';

Type
  TTLObject = class (TAdvObject)
  protected
		Fline : integer;
		Fcol : integer;
  public
    Constructor Create(line, col : integer); overload;
    function Link : TTLObject; overload;
    function hasValue(value : String) : boolean; virtual;
    property line : integer read FLine;
    property col : integer read FCol;
  end;

	TTLLiteral = class (TTLObject)
  private
		Fvalue : String;
		Ftype : String;
  public
    function Link : TTLLiteral; overload;
    function hasValue(value : String) : boolean; override;
		property value : String read Fvalue write Fvalue;
		property type_ : String read Ftype write Ftype;
	end;

	TTLURL = class (TTLObject)
  private
    Furi : String;
    procedure setUri(value : String);
  public
    function Link : TTLURL; overload;
		property uri : String read Furi write Seturi;
    function hasValue(value : String) : boolean; override;
  end;

	TTLList = class (TTLObject)
  private
    Flist : TAdvList<TTLObject>;
  public
    Constructor Create(line, col : integer); overload;
    Constructor Create(line, col : integer; obj : TTLObject); overload;
    Destructor Destroy; override;
    function Link : TTLList; overload;
    function hasValue(value : String) : boolean; override;
    property List : TAdvList<TTLObject> read FList;
  end;

	TTLComplex = class (TTLObject)
  private
    FPredicates : TAdvMap<TTLObject>;
  public
    Constructor Create(line, col : integer); overload;
    Destructor Destroy; override;
    function Link : TTLComplex; overload;
    function hasValue(value : String) : boolean; override;
    property predicates : TAdvMap<TTLObject> read FPredicates;
    procedure addPredicate(uri : String; obj : TTLObject);
    procedure addPredicates(values : TAdvMap<TTLObject>);
  end;

  TTurtlePredicate = class (TAdvObject)
  private
    FURL : TTLURL;
    FValue : TTLComplex;
  public
    Constructor Create(url : TTLURL; value : TTLComplex);
    Destructor Destroy; override;
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
  TTurtleLexer = class (TAdvObject)
  private
    FSource : String;
    FType : TLexerTokenType;
		cursor, line, col, startLine, startCol : integer;
		Ftoken : String;
    procedure skipWhitespace;
    function grab : char;
    procedure readNext(postColon : boolean);
    function unescape(s : String; isUri : boolean) : string;
  public
    Constructor Create(source : String);
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

  TTurtleParser = class (TAdvObject)
  private
    FObjects : TAdvList<TTurtlePredicate>;
    FBase : String;
    prefixes : TDictionary<String, String>;

//	private List<Section> sections = new ArrayList<Section>();
//	protected Set<String> subjectSet = new HashSet<String>();
//	protected Set<String> predicateSet = new HashSet<String>();
//	protected Set<String> objectSet = new HashSet<String>();
//	protected Map<String, String>  = new HashMap<String, String>();

    procedure parse(lexer : TTurtleLexer); overload;
    function anonymousId : TTLURL;
    function parseComplex(lexer : TTurtleLexer) : TTLComplex;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    procedure parse(source : String); overload;
    property Objects : TAdvList<TTurtlePredicate> read FObjects;
    function getObject(uri : String) : TTLComplex;
  end;

implementation

{ TTLObject }

constructor TTLObject.Create(line, col: integer);
begin
  inherited Create;
  FLine := line;
  FCol := col;
end;

function TTLObject.hasValue(value: String): boolean;
begin
  raise Exception.Create('Must override hasValue in '+ClassName);
end;

function TTLObject.Link: TTLObject;
begin
  result := TTLObject(inherited Link);
end;

{ TTLLiteral }

function TTLLiteral.hasValue(value: String): boolean;
begin
  result := Fvalue = value;
end;

function TTLLiteral.Link: TTLLiteral;
begin
  result := TTLLiteral(inherited Link);
end;

{ TTLURL }

function TTLURL.hasValue(value: String): boolean;
begin
  result := FUri = value;
end;

function TTLURL.Link: TTLURL;
begin
  result := TTLURL(inherited Link);
end;

procedure TTLURL.setUri(value: String);
begin
  if (not TRegEx.Match(value, IRI_URL).Success) then
    raise Exception.create('Illegal URI '+uri);
  FUri := value;
end;

{ TTLList }

constructor TTLList.Create(line, col: integer; obj: TTLObject);
begin
  Inherited Create(line, col);
  FList := TAdvList<TTLObject>.create;
  FList.Add(obj.Link);
end;

constructor TTLList.Create(line, col: integer);
begin
  Inherited Create(line, col);
  FList := TAdvList<TTLObject>.create;
end;

destructor TTLList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TTLList.hasValue(value: String): boolean;
var
  obj : TTLObject;
begin
  for obj in Flist do
    if (obj.hasValue(value)) then
      exit(true);
  exit(false);
end;

function TTLList.Link: TTLList;
begin
  result := TTLList(inherited Link);
end;

{ TTLComplex }

constructor TTLComplex.Create(line, col: integer);
begin
  inherited;
  FPredicates := TAdvMap<TTLObject>.create;
end;

destructor TTLComplex.Destroy;
begin
  FPredicates.Free;
  inherited;
end;

function TTLComplex.Link: TTLComplex;
begin
  result := TTLComplex(inherited Link);
end;

function TTLComplex.hasValue(value: String): boolean;
begin
  result := false;
end;

procedure TTLComplex.addPredicate(uri: String; obj: TTLObject);
var
  eo : TTLObject;
  list : TTLList;
begin
  if (not predicates.containsKey(uri)) then
    predicates.add(uri, obj)
  else
  begin
    eo := predicates[uri];
    if (eo is TTLList) then
      list := eo as TTLList
    else
    begin
      list := TTLList.Create(0,0, eo);
      predicates.AddOrSetValue(uri, list);
    end;
    list.list.add(obj);
  end;
end;

procedure TTLComplex.addPredicates(values: TAdvMap<TTLObject>);
begin
  FPredicates.addAll(values);
end;

{ TTurtleLexer }

constructor TTurtleLexer.Create(source: String);
begin
  Fsource := source;
  cursor := 1;
  line := 1;
  col := 1;
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
        if (ch = '\r') or (ch = '\n') then
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
  begin
    inc(line);
    col := 1;
  end
  else
    inc(col);

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
  startLine := line;
  startCol := col;
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
          else if (s <> '""') and (copy(s, length(s)-length(e)+1, length(e)) = e) and not (copy(s, length(s)-length(e), length(e)+1) = '\'+e) then
						break;
				end;
				Ftype := lttLITERAL;
				Ftoken := unescape(b.toString().substring(e.length, b.length-e.length), false);
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
				Ftoken := unescape(b.toString().substring(e.length, b.length-e.length), false);
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
					raise Exception.create('unexpected lexer char '+ch);
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
            raise Exception.create('Illegal unicode character');
          b.append(char(uc));
          i := i + l;
          break;
        end;
        else
          raise Exception.create('Unknown character escape \\'+s[i]);
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
 raise Exception.create('Syntax Error parsing Turtle on line '+inttostr(line)+' col '+inttostr(col)+': '+message);
end;

{ TTurtleParser }

constructor TTurtleParser.Create;
begin
  inherited Create;
  FObjects := TAdvList<TTurtlePredicate>.create;
  prefixes := TDictionary<String, String>.create;
end;

destructor TTurtleParser.Destroy;
begin
  prefixes.Free;
  FObjects.Free;
  inherited;
end;

procedure TTurtleParser.parse(source: String);
var
  lexer : TTurtleLexer;
begin
  prefixes.clear();
  prefixes.add('_', 'urn:uuid:4425b440-2c33-4488-b9fc-cf9456139995#');
  lexer := TTurtleLexer.Create(source);
  try
		parse(lexer);
  finally
    lexer.Free;
  end;
end;

procedure TTurtleParser.parse(lexer: TTurtleLexer);
var
  doPrefixes, sparqlStyle, base : boolean;
  p, sprefix, url, pfx : String;
  uri : TTLURL;
  complex, bnode : TTLComplex;
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
          raise Exception.create('Unexpected token '+p);
      end
      else
      begin
        sparqlStyle := true;
        p := lexer.word();
        if (p = 'BASE') then
          base := true
        else if (p <> 'PREFIX') then
          raise Exception.create('Unexpected token '+p);
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
        prefixes.AddOrSetValue(sprefix, url)
      else if (Fbase = '') then
        Fbase := url
      else
        raise Exception.create('Duplicate @base');
    end
    else if (lexer.peekType() = lttURI) then
    begin
      doPrefixes := false;
      uri := TTLURL.create(lexer.startLine, lexer.startCol);
      try
        uri.Uri := lexer.uri();
        complex := parseComplex(lexer);
        Fobjects.add(TTurtlePredicate.Create(uri.Link, complex));
      finally
        uri.Free;
      end;
      lexer.token('.');
    end
    else if (lexer.peekType() = lttWORD) then
    begin
      doPrefixes := false;
      uri := TTLURL.create(lexer.startLine, lexer.startCol);
      try
        pfx := lexer.word();
        if (not prefixes.containsKey(pfx)) then
          raise Exception.create('Unknown prefix '+pfx);
        lexer.token(':');
        uri.Uri := prefixes[pfx]+lexer.word();
        complex := parseComplex(lexer);
        Fobjects.add(TTurtlePredicate.Create(uri.Link, complex));
      finally
        uri.free;
      end;
      lexer.token('.');
    end
    else if (lexer.peek(lttTOKEN, ':')) then
    begin
      doPrefixes := false;
      uri := TTLURL.create(lexer.startLine, lexer.startCol);
      try
        lexer.token(':');
        if (not prefixes.containsKey('')) then
          raise Exception.create('Unknown prefix ''''');
        uri.Uri := prefixes['']+lexer.word();
        complex := parseComplex(lexer);
        Fobjects.add(TTurtlePredicate.Create(uri.Link, complex));
      finally
        uri.Free;
      end;
      lexer.token('.');
    end
    else if (lexer.peek(lttTOKEN, '[')) then
    begin
      doPrefixes := false;
      lexer.token('[');
      bnode := parseComplex(lexer);
      try
        lexer.token(']');
        if (not lexer.peek(lttTOKEN, '.')) then
        begin
          complex := parseComplex(lexer);
          try
            // at this point, we collapse bnode and complex, and give bnode a fictional identity
            bnode.addPredicates(complex.predicates);
          finally
            complex.Free;
          end;
        end;

        Fobjects.add(TTurtlePredicate.Create(anonymousId(), bnode.Link));
      finally
        bnode.Free;
      end;
      lexer.token('.');
    end
    else
      lexer.error('Unknown token '+lexer.Ftoken);
  end;
end;

function TTurtleParser.parseComplex(lexer: TTurtleLexer): TTLComplex;
var
  done, inlist, rpt : boolean;
  uri, t, l, lang, pfx : string;
  u : TTLURL;
  ul : TTLLiteral;
  sl, sc : integer;
begin
  inlist := false;
  result := TTLComplex.create(lexer.startLine, lexer.startCol);
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
					if (not prefixes.containsKey(t)) then
            lexer.error('unknown prefix '+t);
					uri := prefixes[t]+lexer.word();
				end
        else if (t = 'a') then
        begin
          if prefixes.containsKey('rdfs') then
  					uri := prefixes['rdfs']+'type'
          else
  					uri := 'http://www.w3.org/2000/01/rdf-schema#type'
        end
				else
					lexer.error('unexpected token "'+t+'"');
			end;

			if (lexer.peek(lttTOKEN, '(')) then
      begin
				inlist := true;
				lexer.token('(');
			end;

      rpt := false;
			repeat
				if (lexer.peek(lttTOKEN, '[')) then
        begin
					lexer.token('[');
          result.addPredicate(uri, parseComplex(lexer));
					lexer.token(']');
				end
        else if (lexer.peekType() = lttURI) then
        begin
					u := TTLURL.Create(lexer.startLine, lexer.startCol);
          try
					  u.Uri := lexer.uri();
            result.addPredicate(uri, u.Link);
          finally
            u.Free;
          end;
				end
        else if (lexer.peekType() = lttLITERAL) then
        begin
					ul := TTLLiteral.create(lexer.startLine, lexer.startCol);
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
                ul.type_ := prefixes[l]+ lexer.word();
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
					sl := lexer.startLine;
					sc := lexer.startCol;
					if lexer.peekType() = lttWORD then pfx := lexer.word() else pfx := '';
					if StringIsDecimal(pfx) and not lexer.peek(lttTOKEN, ':') then
          begin
						ul := TTLLiteral.create(sl, sc);
            try
						  ul.value := pfx;
              result.addPredicate(uri, ul.Link);
            finally
              ul.Free;
            end;
					end
          else if (('false'.equals(pfx)) or ('true'.equals(pfx))) and (not lexer.peek(lttTOKEN, ':')) then
          begin
						ul := TTLLiteral.create(sl, sc);
            try
              ul.value := pfx;
              result.addPredicate(uri, ul.Link);
            finally
              ul.Free;
            end;
					end
          else
          begin
						if (not prefixes.containsKey(pfx)) then
              lexer.error('Unknown prefix "'+pfx+'"');
						u := TTLURL.Create(sl, sc);
            try
						  lexer.token(':');
						  u.setUri(prefixes[pfx]+lexer.word());
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
				else begin
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

function TTurtleParser.anonymousId() : TTLURL;
begin
  result := TTLURL.create(-1, -1);
  result.Uri := NewGuidURN;
end;

function TTurtleParser.getObject(uri : String) : TTLComplex;
var
  t : TTurtlePredicate;
begin
  for t in objects do
    if (t.FURL.uri = uri) then
      exit(t.FValue);
  exit(nil);
end;

{ TTurtlePredicate }

constructor TTurtlePredicate.Create(url: TTLURL; value: TTLComplex);
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

end.
