unit FHIR.Base.Lang;

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

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  SysUtils, classes, Generics.Collections,
  FHIR.Support.Base,
  FHIR.Web.Parsers,
  FHIR.Base.Objects;

function GetFhirMessage(id : String; const lang : THTTPLanguages) : String; overload;
function GetFhirMessage(id : String; const lang : THTTPLanguages; def : String) : String; overload;

procedure LoadMessages;

var
  FHIRExeModuleName : String;

// exceptions

Const
   HTTP_OK_200 = 200;
   HTTP_OK_220 = 220;
   HTTP_CREATED = 201;
   HTTP_ACCEPTED = 202;
   HTTP_NO_CONTENT = 204;
   HTTP_REDIR_MULTIPLE_CHOICE = 300;
   HTTP_REDIR_MOVED_PERMANENT = 301;
   HTTP_REDIR_MOVED_TEMPORARY = 302;
   HTTP_REDIR_AFTER_POST = 303;
   HTTP_REDIR_NOT_MODIFIED = 304;
   HTTP_ERR_BAD_REQUEST = 400;
   HTTP_ERR_UNAUTHORIZED = 401;
   HTTP_ERR_FORBIDDEN = 403;
   HTTP_ERR_NOTFOUND = 404;
   HTTP_ERR_METHOD_ILLEGAL = 405;
   HTTP_ERR_NOT_ACCEPTABLE = 406;
   HTTP_ERR_CONFLICT = 409;
   HTTP_ERR_DELETED = 410;
   HTTP_ERR_PRECONDITION_FAILED = 412;
   HTTP_ERR_NOT_UNSUPPORTED_MEDIA_TYPE = 415;
   HTTP_ERR_BUSINESS_RULES_FAILED = 422;
   HTTP_ERR_INTERNAL = 500;

Type
  EFHIRException = class (EFslException)
  public
    constructor CreateLang(code : String; const lang : THTTPLanguages); overload;
    constructor CreateLang(code : String; const lang : THTTPLanguages; const Args: array of const); overload;
  end;
  EFHIRTodo = Class(EFHIRException)
  public
    Constructor Create(place : String);
  End;

  ETooCostly = class (EFHIRException);
  EUnsafeOperation = class (EFHIRException);
  EDefinitionException = class (EFHIRException);
  EDefinitionExceptionTodo = Class(EDefinitionException)
  public
    Constructor Create(place : String);
  End;
  EFHIRNarrativeException = class (EFHIRException);

  ERestfulException = class (EFHIRException)
  Private
    FContext : String;
    FStatus : word;
    FCode : TFhirIssueType;
  Public
    constructor Create(Const sContext : String; aStatus : word; code : TFhirIssueType; sMessage : String; const lang : THTTPLanguages); Overload; Virtual;
    constructor Create(Const sContext : String; aStatus : word; code : TFhirIssueType; sMessage : String; const lang : THTTPLanguages; const Args: array of const); Overload; Virtual;

    Property Status : word read FStatus write FStatus;
    Property Code : TFhirIssueType read FCode write FCode;
  End;

  EFHIRPath = class (EFHIRException)
  public
     constructor Create(problem : String); overload;
     constructor Create(path : String; offset : integer; problem : String); overload;
  end;
  EFHIRPathTodo = Class(EDefinitionException)
  public
    Constructor Create(place : String);
  End;

  EFHIRPathDefinitionCheck = class (EFHIRPath);

function removeCaseAndAccents(s : String) : String;

implementation

{$R FHIRTranslations.res}

uses
  FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.MXml;

Type
  TFHIRMessage = class (TFslObject)
  private
    FMessages : TFslStringDictionary;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  GMessages : TFslMap<TFHIRMessage>;

Function LoadSource : TBytes;
{$IFDEF MACOS}
begin
  result := FHIR.Support.Stream.FileToBytes(IncludeTrailingPathDelimiter(ExtractFilePath(paramstr(0)))+'translations.xml');
end;
{$ELSE}
var
  LRes : TAfsResourceVolume;
  LHnd : TAfsHandle;
begin
  LRes := TAfsResourceVolume.create;
  try
    LHnd := LRes.Open(FHIRExeModuleName, 'FHIR_Translations,#10', amRead, asRead);
    try
      SetLength(result, LRes.GetSize(LHnd));
      LRes.Read(LHnd, result[0], length(result));
    finally
      LRes.Close(LHnd);
    end;
  finally
    LRes.Free;
  end;
end;
{$ENDIF}

procedure LoadMessages;
var
  source : TMXmlDocument;
  child, lang : TMXmlElement;
  msg : TFHIRMessage;
begin
  if FHIRExeModuleName = '##' then
    exit;

  if GMessages <> nil then
    exit;

  source := TMXmlParser.parse(LoadSource, [xpDropWhitespace, xpDropComments]);
  try
    GMessages := TFslMap<TFHIRMessage>.create('MXml Parser');
    child := source.document.firstElement;
    while child <> nil do
    begin
      msg := TFHIRMessage.Create;
      GMessages.Add(child.attribute['id'], msg);
      lang := child.firstElement;
      while lang <> nil do
      begin
        msg.FMessages.add(lang.attribute['lang'], lang.text);
        lang := lang.nextElement;
      end;
      child := child.nextElement;
    end;
  finally
    source.free;
  end;
end;

function GetFhirMessage(id : String; const lang : THTTPLanguages) : String;
begin
  result := GetFhirMessage(id, lang, '??');
end;

function GetFhirMessage(id : String; const lang : THTTPLanguages; def : String) : String;
var
  msg : TFHIRMessage;
  l : string;
begin
  result := '';
  if GMessages = nil then
    LoadMessages;
  if not GMessages.ContainsKey(id) then
    result := def
  else
  begin
    msg := GMessages[id];
    for l in lang.codes do
      if (result = '') and (msg.FMessages.ContainsKey(l)) then
        result := msg.FMessages[l];
    if result = '' then
      result := msg.FMessages['en'];
    if result = '' then
      result := '??';
    if result = '' then
      result := def;
  end;
end;

{ TFHIRMessage }

constructor TFHIRMessage.Create;
begin
  inherited;
  FMessages := TFslStringDictionary.create;
end;

destructor TFHIRMessage.Destroy;
begin
  FMessages.Free;
  inherited;
end;

{ EFHIRException }

constructor EFHIRException.CreateLang(code : String; const lang : THTTPLanguages);
begin
  inherited Create(GetFhirMessage(code, lang));
end;

constructor EFHIRException.CreateLang(code : String; const lang : THTTPLanguages; const Args: array of const);
begin
  inherited Create(Format(GetFhirMessage(code, lang), args));
end;


{ ERestfulException }

constructor ERestfulException.Create(const sContext: String; aStatus: word; code: TFhirIssueType; sMessage : String; const lang : THTTPLanguages; const Args: array of const);
begin
  inherited Create(Format(GetFhirMessage(sMessage, lang), args));

  FContext := sContext;
  FStatus := aStatus;
  FCode := code;
end;

constructor ERestfulException.Create(const sContext: String; aStatus: word; code: TFhirIssueType; sMessage : String; const lang : THTTPLanguages);
begin
  inherited Create(GetFhirMessage(sMessage, lang, sMessage));
  FContext := sContext;
  FStatus := aStatus;
  FCode := code;
end;

function languageMatches(spec, possible : String) : boolean;
begin
  result := spec = possible; // todo: make this better
end;

function removeCaseAndAccents(s : String) : String;
begin
  result := lowercase(s); // todo....
end;

{ EFHIRPath }

constructor EFHIRPath.create(path: String; offset: integer; problem: String);
begin
  inherited create('FHIR.R2.PathEngine error "'+problem+'" at position '+inttostr(offset)+' in "'+path+'"');
end;

constructor EFHIRPath.create(problem: String);
begin
  inherited create(problem);
end;

{ EFHIRTodo }

constructor EFHIRTodo.Create(place: String);
begin
  inherited create('Not done yet @ '+place);
end;

{ EDefinitionExceptionTodo }

constructor EDefinitionExceptionTodo.Create(place: String);
begin
  inherited create('Not done yet @ '+place);
end;

{ EFHIRPathTodo }

constructor EFHIRPathTodo.Create(place: String);
begin
  inherited create('Not done yet @ '+place);
end;

initialization
  GMessages := nil;
finalization
  GMessages.Free;
end.
