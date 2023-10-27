unit ftk_factory;

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
  SysUtils, Classes, Dialogs, LCLType,

  fsl_base, fsl_utilities, fsl_xml, fsl_json, fsl_crypto,
  fhir_objects,

  ftk_context, ftk_store, ftk_store_temp,
  ftk_editor_text, ftk_editor_ini, ftk_editor_xml, ftk_editor_json, ftk_editor_html,
  ftk_editor_md, ftk_editor_js, ftk_editor_hl7, ftk_editor_fhir, ftk_editor_jwt,

  ftk_worker_server, ftk_worker_home, ftk_worker_igpub;

type

  { TToolkitFactory }

  TToolkitFactory = class (TFslObject)
  private
    FContext : TToolkitContext;
    FHandle: TComponent;
  public
    constructor Create(context : TToolkitContext; handle: TComponent);
    destructor Destroy; override;
    function makeNewSession(kind : TSourceEditorKind) : TToolkitEditSession;
    function examineFile(filename, mimeType : String; const bytes : TBytes; exception : boolean) : TToolkitEditSession;

    function makeEditor(session : TToolkitEditSession; tempStore : TFHIRToolkitTemporaryStorage) : TToolkitEditor;

    class function determineFormatFromText(src : String; var content : TBytes) : TSourceEditorKindSet;
    class function determineFormatFromFmt(fmt : TClipboardFormat; src : Tbytes; var kind : TSourceEditorKind; var content : TBytes) : boolean;
  end;


implementation

{ TToolkitFactory }

constructor TToolkitFactory.Create(context: TToolkitContext; handle: TComponent);
begin
  inherited Create;
  FCOntext := context;
  FHandle := handle;
end;

destructor TToolkitFactory.Destroy;
begin
  FCOntext.free;
  inherited Destroy;
end;

function TToolkitFactory.makeNewSession(kind: TSourceEditorKind): TToolkitEditSession;
begin
  result := TToolkitEditSession.Create;
  result.guid := NewGuidId;
  result.kind := kind;
end;

function loadJson(bytes : TBytes) : TJsonObject;
var
  parser : TJSONParser;
begin
  parser := TJSONParser.Create;
  try
    result := parser.Parse(bytes, 0, false);
  finally
    parser.free;
  end;
end;

function loadXml(bytes : TBytes) : TMXmlDocument;
var
  parser : TMXmlParser;
begin
  parser := TMXmlParser.Create;
  try
    result := parser.parse(bytes, [xpHTMLEntities]);
  finally
    parser.free;
  end;
end;

procedure readNamespace(xml : TMXmlElement; var ns, name : String);
var
  p : String;
  a : TMXmlAttribute;
begin
  name := xml.Name;
  if (name.contains(':')) then
  begin
    StringSplit(name, ':', p, name);
    for a in xml.Attributes do
      if a.Name = 'xmlns:'+p then
        ns := a.Value;
  end
  else
  begin
    for a in xml.Attributes do
      if a.Name = 'xmlns' then
        ns := a.Value;
  end;
end;

function examineXml(xml : TMXmlDocument) : TToolkitEditSession;
var
  ns, name : String;
begin
  readNamespace(xml.docElement, ns, name);
  if (ns = 'http://hl7.org/fhir') then
    result := TToolkitEditSession.createResource(ffXml, 'resourceType', name)
  else if (ns = 'urn:hl7-org:v3') and (name = 'ClinicalDocument') then
    result := TToolkitEditSession.Create(sekCDA)
  else
    result := TToolkitEditSession.Create(sekXml);
end;

function examineJson(json : TJsonObject) : TToolkitEditSession;
var
  rt : String;
begin
  rt := json.str['resourceType'];
  if (rt <> '') then
    result := TToolkitEditSession.createResource(ffJson, 'resourceType', rt)
  else
    result := TToolkitEditSession.Create(sekJson);
end;

function TToolkitFactory.examineFile(filename, mimeType: String; const bytes: TBytes; exception : boolean): TToolkitEditSession;
var
  ext, s : String;
  xml : TMXmlDocument;
  json : TJsonObject;
  jwt : TJWT;
begin
  result := nil;
  if mimeType <> '' Then
  begin
    if (mimeType.StartsWith('text/html')) then
      result := TToolkitEditSession.Create(sekHTML)
    else if (mimeType.StartsWith('text/plain')) then
      result := TToolkitEditSession.Create(sekHTML)
    else if mimeType.contains('json') then
    begin
      try
        json := loadJson(bytes);
        try
          exit(examineJson(json));
        finally
          json.free;
        end;
      except
        // right, we'll just treat it as plain JSON
      end;
      result := TToolkitEditSession.Create(sekJson);
    end
    else if mimeType.contains('xml') then
    begin
      try
        xml := loadXml(bytes);
        try
          exit(examineXml(xml));
        finally
          xml.free;
        end;
      except
        // right, we'll just treat it as plain XML
      end;
      result := TToolkitEditSession.Create(sekXml);
    end
    else

  end
  else
  begin
    ext := Lowercase(ExtractFileExt(filename));
    if (ext = '.ini') then
      result := TToolkitEditSession.Create(sekIni)
    else if (ext = '.html') then
      result := TToolkitEditSession.Create(sekHTML)
    else if (ext = '.md') then
      result := TToolkitEditSession.Create(sekMD)
    else if (ext = '.js') then
      result := TToolkitEditSession.Create(sekJS)
    else if (ext = '.hl7') or (ext = '.msg') then
      result := TToolkitEditSession.Create(sekv2)
    else if (ext = '.txt') then
    begin
      s := TEncoding.ANSI.GetString(bytes);
      if (s.StartsWith('shc:/')) then
        result :=  TToolkitEditSession.Create(sekJWT)
      else
        result := TToolkitEditSession.Create(sekText)
    end
    else if (ext = 'jwt') or (ext = '.jws') then
      result := TToolkitEditSession.Create(sekJWT)
    else if (ext = '.xml') then
    begin
      try
        xml := loadXml(bytes);
        try
          exit(examineXml(xml));
        finally
          xml.free;
        end;
      except
        // right, we'll just treat it as plain XML
      end;
      result := TToolkitEditSession.Create(sekXml);
    end
    else if (ext = '.json') then
    begin
      try
        json := loadJson(bytes);
        try
          exit(examineJson(json));
        finally
          json.free;
        end;
      except
        // right, we'll just treat it as plain JSON
      end;
      result := TToolkitEditSession.Create(sekJson);
    end
  end;

  if (result = nil) then
  begin
    try
      xml := loadXml(bytes);
      try
        exit(examineXml(xml));
      finally
        xml.free;
      end;
    except
    end;
    try
      json := loadJson(bytes);
      try
        exit(examineJson(json));
      finally
        json.free;
      end;
    except
    end;
    s := TEncoding.ANSI.GetString(bytes);
    if (s.StartsWith('shc:/')) then
      exit(TToolkitEditSession.Create(sekJWT));
    try
      jwt := TJWTUtils.decodeJWT(s);
      try
        exit(TToolkitEditSession.Create(sekJWT));
      finally
        jwt.free;
      end;
    except
    end;
    if exception then
      raise EFslException.Create(filename+' isn''t recognised by this application (unknown extension, and not xml or json)')
    else
      ShowMessage(filename+' isn''t recognised by this application (unknown extension, and not xml or json)');
  end;
end;

function TToolkitFactory.makeEditor(session : TToolkitEditSession; tempStore : TFHIRToolkitTemporaryStorage): TToolkitEditor;
var
  store : TStorageService;
begin
  store := FContext.StorageForAddress(session.address);
  case session.kind of
  sekFHIR : result := TFHIREditor.Create(FContext{.link}, session, store.link);
  sekIni : result := TIniEditor.Create(FContext{.link}, session, store.link);
  sekText : result := TTextEditor.Create(FContext{.link}, session, store.link);
  sekLiquid : result := THtmlEditor.Create(FContext{.link}, session, store.link);
  sekXml : result := TXmlEditor.Create(FContext{.link}, session, store.link);
  sekJson : result := TJsonEditor.Create(FContext{.link}, session, store.link);
  sekHtml : result := THtmlEditor.Create(FContext{.link}, session, store.link);
  sekMD : result := TMarkdownEditor.Create(FContext{.link}, session, store.link);
  sekJS : result := TJavascriptEditor.Create(FContext{.link}, session, store.link);
  sekv2 : result := THL7Editor.Create(FContext{.link}, session, store.link);
  sekServer : result := TServerWorker.Create(FContext{.link}, session, store.link);
  sekHome : result := THomePageWorker.Create(FContext{.link}, session, store.link, tempStore.link);
  sekIGs : result := TIgPubPageWorker.Create(FContext{.link}, session, store.link, tempStore.link);
  sekJWT : result := TJWTEditor.Create(FContext{.link}, session, store.link);
  else
    raise EFslException.Create('not supported yet');
  end;
end;


class function TToolkitFactory.determineFormatFromText(src : String; var content : TBytes) : TSourceEditorKindSet;
var
  j : TJsonObject;
  x : TMXmlDocument;
begin
  src := src.trim;
  if (src = '') then
    exit([]);

  content := TEncoding.UTF8.GetBytes(src);
  if (src.StartsWith('MSH|') or src.StartsWith('FHS|') or src.StartsWith('BHS|')) then
    exit([sekv2]);

  if (src.StartsWith('shc:/')) then
    exit([sekJWT]);

  try
    j := TJSONParser.Parse(src);
    try
      if (j.has('resourceType')) then
        exit([sekFHIR]);
      exit([sekJson]);
    finally
      j.free;
    end;
  except
  end;
  try
    x := TMXmlParser.parse(src, [xpResolveNamespaces]);
    try
      if (x.docElement.NamespaceURI = 'http://hl7.org/fhir/') then
        exit([sekFHIR]);

      if (x.docElement.NamespaceURI = 'urn:hl7-org:v3') and (x.docElement.Name = 'ClinicalDocument')  then
        exit([sekCDA]);

      if (x.docElement.Name = 'html')  then
        exit([sekHTML]);

      exit([sekXml]);
    finally
      x.free;
    end;
  except
  end;
  try
    TJWTUtils.decodeJWT(src).free;
    exit([sekJwt]);
  except
  end;

  // if we get to here, it's plain text, or broken xml or json. We're going to guess and provide options.
  result := [sekText, sekMD];
  if (src.StartsWith('{')) then
    exit(result + [sekJson]);
  if (src.StartsWith('<')) then
  begin
    if (src.contains('<html')) then
      exit(result + [sekHTML])
    else
      exit(result + [sekXml]);
  end;
  if (src.contains('function ')) then
    result := result + [sekJS];
  if (src.contains('[') and src.contains(']') and src.contains('=')) and not (sekJS in result) then
    result := result + [sekIni];
  if (src.contains('{{')) or (src.contains('{%'))  then
    result := result + [sekLiquid];
end;

class function TToolkitFactory.determineFormatFromFmt(fmt: TClipboardFormat; src: Tbytes; var kind: TSourceEditorKind; var content: TBytes): boolean;
begin
  // for now, false. if we couldn't read it as text, we don't know how to read it.
  result := false;
end;



end.
