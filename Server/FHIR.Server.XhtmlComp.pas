unit FHIR.Server.XhtmlComp;

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

interface

uses
  SysUtils, Classes,
  FHIR.Support.Text, FHIR.Support.Strings,
  FHIR.Support.MXml, FHIR.Support.Xml,
  FHIR.Base.Objects, FHIR.Server.Session, FHIR.Base.Lang, FHIR.Base.Parser, FHIR.Base.Xhtml,
  {$IFDEF FHIR2}
  FHIR.R2.Types, FHIR.R2.Resources, FHIR.R2.Context, FHIR.R2.Tags, FHIR.R2.Constants;
  {$ENDIF}
  {$IFDEF FHIR3}
  FHIR.R3.Types, FHIR.R3.Resources, FHIR.R3.Context, FHIR.R3.Tags, FHIR.R3.Constants;
  {$ENDIF}
  {$IFDEF FHIR4}
  FHIR.R4.Types, FHIR.R4.Resources, FHIR.R4.Context, FHIR.R4.Tags, FHIR.R4.Constants;
  {$ENDIF}

type
  TFHIRXhtmlComposerGetLink = procedure (resource : TFhirResource; base, statedType, id, ver : String; var link, text : String) of object;

  TFHIRXhtmlComposer = class (TFHIRComposer)
  private
    FBaseURL: String;
    FSession: TFhirSession;
    FTags : TFHIRTagList;
    FrelativeReferenceAdjustment: integer;
    FOnGetLink: TFHIRXhtmlComposerGetLink;
    FOperationName : String;
    FVersion: String;
    FLinks: TFhirBundleLinkList;

    procedure SetSession(const Value: TFhirSession);
    function PresentTags(aType : TFhirResourceType; target : String; tags : TFHIRTagList; c : integer):String; overload;
    function PresentTags(aType : TFhirResourceType; target : String; meta: TFhirMeta; c : integer):String; overload;
    procedure SetTags(const Value: TFHIRTagList);
    function PatchToWeb(url: String): String;
//    xml : TXmlBuilder;
//    procedure ComposeNode(node : TFhirXHtmlNode);
    Procedure ComposeBundle(stream : TStream; bundle : TFHIRBundle);
    procedure SetLinks(const Value: TFhirBundleLinkList);


  protected
    function ResourceMediaType: String; override;
    function GetFormat: TFHIRFormat; override;
  public
    Constructor Create(worker: TFHIRWorkerContext; Style : TFHIROutputStyle; lang, BaseURL : String); reintroduce; overload;
    Destructor Destroy; override;
    property BaseURL : String read FBaseURL write FBaseURL;
    Property Session : TFhirSession read FSession write SetSession;
    Property Version : String read FVersion write FVersion;
    property Tags : TFHIRTagList read FTags write SetTags;
    Procedure Compose(stream : TStream; oResource : TFhirResourceV); override;
    Procedure ComposeResourceV(xml : TXmlBuilder; oResource : TFhirResourceV); override;
    Function MimeType : String; Override;
    function Extension : String; Override;

    Property relativeReferenceAdjustment : integer read FrelativeReferenceAdjustment write FrelativeReferenceAdjustment;
    Property OnGetLink : TFHIRXhtmlComposerGetLink read FOnGetLink write FOnGetLink;
    Property OperationName : String read FOperationName write FOperationName;
    property links : TFhirBundleLinkList read FLinks write SetLinks;

    class function ResourceLinks(a, lang, base : String; count : integer; bTable, bPrefixLinks, canRead : boolean): String;
    class function PageLinks : String;
    class function Header(Session : TFhirSession; base, lang, version : String) : String;
    class function Footer(base, lang, logId : String; tail : boolean = true) : string;
  end;


implementation

uses
  {$IFDEF FHIR2}
  FHIR.R2.Utilities,
  {$ENDIF}
  {$IFDEF FHIR3}
  FHIR.R3.Utilities,
  {$ENDIF}
  {$IFDEF FHIR4}
  FHIR.R4.Utilities,
  {$ENDIF}

  FHIR.Version.Parser;

{ TFHIRXhtmlComposer }

procedure TFHIRXhtmlComposer.Compose(stream: TStream; oResource: TFhirResourceV);
var
  s : TFslStringBuilder;
  ss : TBytesStream;
  xml : TFHIRXmlComposer;
  c : integer;
  title : String;
  link, text : String;
  id : String;
  ver : String;
  statedType : String;
  res : TFHIRResource;
begin
  res := oResource as TFHIRResource;

  if (res is TFhirBundle) then
  begin
    composeBundle(stream, res as TFhirBundle);
    exit;
  end;

  id := res.id;
  if (res.meta <> nil) then
    ver := res.meta.versionId;
  statedType := CODES_TFhirResourceType[res.resourceType]; // todo: resolve this

  if (id = '') and (ver = '') then
  begin
    if FOperationName <> '' then
      title := 'Results from '+FOperationName
    else
      title := FormatTextToXml(GetFhirMessage(CODES_TFhirResourceType[res.resourceType], lang), xmlText)
  end
  else if (ver = '') then
    title := FormatTextToXml(GetFhirMessage('NAME_RESOURCE', lang)+' "'+id + '" ('+CODES_TFhirResourceType[res.ResourceType]+') ', xmlText)
  else
    title := FormatTextToXml(GetFhirMessage('NAME_RESOURCE', lang)+' "'+id+'" '+GetFhirMessage('NAME_VERSION', lang)+' "'+ver + '" ('+CODES_TFhirResourceType[res.ResourceType]+') ', xmlText);

  c := 0;
  s := TFslStringBuilder.create;
  try
    s.append(
'<?xml version="1.0" encoding="UTF-8"?>'+#13#10+
'<!DOCTYPE HTML'+#13#10+
'       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'+#13#10+
''+#13#10+
'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'+#13#10+
'<head>'+#13#10+
'    <title>'+title+'</title>'+#13#10+
PageLinks+
FHIR_JS+
'</head>'+#13#10+
''+#13#10+
'<body>'+#13#10+
''+#13#10+
Header(Session, FBaseURL, lang, version)+
'<h2>'+title+'</h2>'+#13#10);

    if res is TFhirBinary then
    begin
      if StringStartsWith(TFhirBinary(res).ContentType, 'image/') then
        s.append('<img src="'+CODES_TFhirResourceType[res.ResourceType]+'/'+id+'">'+#13#10)
      else
        s.append('<pre class="xml">'+#13#10+'('+GetFhirMessage('NAME_BINARY', lang)+')'+#13#10+'</pre>'+#13#10);
    end
    else
    begin
      inc(c);
      if assigned(FTags) then
        if ver <> '' then
          s.append('<p><a href="./_tags">'+GetFhirMessage('NAME_TAGS', lang)+'</a>: '+PresentTags(res.resourceType, FBaseURL+CODES_TFhirResourceType[res.ResourceType]+'/'+id+'/_history/'+ver+'/_tags', Ftags, c)+'</p>'+#13#10)
        else if id <> '' then
          s.append('<p><a href="./_tags">'+GetFhirMessage('NAME_TAGS', lang)+'</a>: '+PresentTags(res.resourceType, FBaseURL+CODES_TFhirResourceType[res.ResourceType]+'/'+id+'/_tags', Ftags, c)+'</p>'+#13#10);
      if id <> '' then
      begin
        if assigned(FOnGetLink) then
          FOnGetLink(res, BaseURL, statedType, id, ver, link, text)
        else
          link := '';
        if link <> '' then
          s.append('<p><a href="?_format=xml">XML</a> or <a href="?_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang)+'. <a href="'+link+'">'+FormatTextToHTML(text)+'</a>'+#13#10)
        else
          s.append('<p><a href="?_format=xml">XML</a> or <a href="?_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang)+#13#10);

        if (links <> nil) and (links.Matches['z-edit-src'] <> '') then
          s.append('. Edit this as <a href="'+patchToWeb(links.Matches['z-edit-src'])+'?srcformat=xml">XML</a> or <a href="'+patchToWeb(links.Matches['z-edit-src'])+'?srcformat=json">JSON</a>');
        {$IFDEF FHIR_QUESTIONNAIRERESPONSE}
        if (links <> nil) and (links.Matches['edit-form'] <> '') then
          if (res is TFHIRQuestionnaireResponse) then
          begin
            if (TFHIRQuestionnaireResponse(res).questionnaire <> nil) then
              s.append('. <a href="'+patchToWeb(links.Matches['edit-form'])+'">Edit this Resource</a> (or <a href="'+TFHIRQuestionnaireResponse(res).questionnaire.reference+'">see the questionnaire</a>)')
          end
          else
            s.append('. <a href="'+patchToWeb(links.Matches['edit-form'])+'">Edit this Resource</a> (or <a href="'+links.Matches['edit-form']+'">see resources underlying that</a>)');
        {$ENDIF}
        if (links <> nil) and (links.Matches['edit-post'] <> '') then
          s.append('. Submit edited content by POST to '+links.Matches['edit-post']);
        if not (res.resourceType in [frtProvenance, frtAuditEvent]) then
          s.append('. <a href="'+FBaseURL+'Provenance?target='+res.fhirType+'/'+res.id+'">provenance for this resource</a>');
        s.append('</p>'#13#10);
      end;

      if (res is TFhirDomainResource) and (TFHIRDomainResource(res).text <> nil) then
        TFHIRXhtmlParser.Compose(TFHIRDomainResource(res).text.div_, s, false, 0, relativeReferenceAdjustment);
      s.append('<hr/>'+#13#10);
      xml := TFHIRXmlComposer.create(FWorker.link, OutputStylePretty, lang);
      ss := TBytesStream.create();
      try
        xml.Compose(ss, res);
        s.append('<pre class="xml">'+#13#10+FormatXMLToHTML(TEncoding.UTF8.getString(ss.bytes, 0, ss.size))+#13#10+'</pre>'+#13#10);
      finally
        ss.free;
        xml.free;
      end;
    end;
    s.append(
'<p><br/>'+
Footer(FBaseURL, lang, logid)
    );
    s.WriteToStream(stream);
  finally
    s.free;
  end;
end;

function TFHIRXhtmlComposer.PatchToWeb(url : String) : String;
begin
  result := FBaseURL+'_web/'+url.substring(FBaseURL.length);
end;

function TFHIRXhtmlComposer.PresentTags(aType: TFhirResourceType; target: String; tags: TFHIRTagList; c: integer): String;
begin
//  PresentTags()
end;

const
  TYPE_TITLE : Array[TFhirBundleTypeEnum] of String = ('', 'Document', 'Message', 'Transaction', 'Transaction Response', 'Batch', 'Batch Response', 'History Record', 'Search Results', 'Resource Collection');

{
procedure TFHIRXhtmlComposer.Compose(stream: TStream; oMeta: TFhirMeta; ResourceType : TFhirResourceType; id, ver : String; isPretty: Boolean; links: TFhirBundleLinkList);
var
  s : TFslStringBuilder;
  i : integer;
begin
  s := TFslStringBuilder.create;
  try
    s.append(
'<?xml version="1.0" encoding="UTF-8"?>'+#13#10+
'<!DOCTYPE HTML'+#13#10+
'       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'+#13#10+
''+#13#10+
'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'+#13#10+
'<head>'+#13#10);
  if ResourceType = frtNull then
    s.append('    <title>'+FormatTextToXml(GetFhirMessage('SYSTEM_TAGS', lang))+'</title>'+#13#10)
  else if id = '' then
    s.append('    <title>'+FormatTextToXml(StringFormat(GetFhirMessage('RESOURCE_TYPE_TAGS', lang), [CODES_TFhirResourceType[ResourceType]]))+'</title>'+#13#10)
  else if ver = '' then
    s.append('    <title>'+FormatTextToXml(StringFormat(GetFhirMessage('RESOURCE_TAGS', lang), [CODES_TFhirResourceType[ResourceType], id]))+'</title>'+#13#10)
  else
    s.append('    <title>'+FormatTextToXml(StringFormat(GetFhirMessage('RESOURCE_VER_TAGS', lang), [CODES_TFhirResourceType[ResourceType], id, ver]))+'</title>'+#13#10);

    s.append(
PageLinks+#13#10+
FHIR_JS+#13#10+
'</head>'+#13#10+
''+#13#10+
'<body>'+#13#10+
''+#13#10+
Header(Session, FBaseURL, Lang));

  if ResourceType = frtNull then
    s.append('    <h2>'+FormatTextToXml(GetFhirMessage('SYSTEM_TAGS', lang))+'</title>'+#13#10+
     '<p></p><p>'+GetFhirMessage('NAME_LINKS', lang)+': <a href="?_format=xml">XML</a> or <a href="?_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang)+'. '+
     'Or: <a href="'+FBaseUrl+'"/>Home Page</a> </p>')
  else if id = '' then
    s.append('    <h2>'+FormatTextToXml(StringFormat(GetFhirMessage('RESOURCE_TYPE_TAGS', lang), [CODES_TFhirResourceType[ResourceType]]))+'</h2>'+#13#10+
     '<p></p><p>'+GetFhirMessage('NAME_LINKS', lang)+': <a href="?_format=xml">XML</a> or <a href="?_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang)+'. '+
     'Or: '+ResourceLinks(ResourceType, lang, FBaseURL, 0, false, false, false)+' </p>')
  else if ver = '' then
    s.append('    <h2>'+FormatTextToXml(StringFormat(GetFhirMessage('RESOURCE_TAGS', lang), [CODES_TFhirResourceType[ResourceType], id]))+'</h2>'+#13#10+
     '<p></p><p>'+GetFhirMessage('NAME_LINKS', lang)+': <a href="?_format=xml">XML</a> or <a href="?_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang)+'. '+
     'Or: <a href="../'+id+'">This Resource</a> </p>')
  else
    s.append('    <h2>'+FormatTextToXml(StringFormat(GetFhirMessage('RESOURCE_VER_TAGS', lang), [CODES_TFhirResourceType[ResourceType], id, ver]))+'</h2>'+#13#10+
     '<p></p><p>'+GetFhirMessage('NAME_LINKS', lang)+': <a href="?_format=xml">XML</a> or <a href="?_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang)+'. '+
     'Or: <a href="../'+ver+'">This Resource Version</a> </p>');

   s.append('<p></p>'+#13#10);
   if (oMeta.profileList.Count + oMeta.tagList.Count + oMeta.securityList.Count = 0) then
     s.append('<p>'+GetFhirMessage('NO_TAGS', lang)+'</p>'+#13#10)
   else
   begin
     s.append('<table>'+#13#10);
     s.append(' <tr><td><b>Type</b></td><td></td><td><b>identity</b></td><td></td><td><b>Label</b></td></tr>'+#13#10);
     for i := 0 to oMeta.profileList.Count - 1 do
       s.append(' <tr><td>Profile</td><td></td><td>'+oMeta.profileList[i].value+'</td><td></td><td></td></tr>'+#13#10);
     for i := 0 to oMeta.tagList.Count - 1 do
       s.append(' <tr><td>Tag</td><td></td><td>'+oMeta.tagList[i].system+'::'+oMeta.tagList[i].code+'</td><td></td><td>'+oMeta.tagList[i].display+'</td></tr>'+#13#10);
     for i := 0 to oMeta.securityList.Count - 1 do
       s.append(' <tr><td>Security Label</td><td></td><td>'+oMeta.securityList[i].system+'::'+oMeta.securityList[i].code+'</td><td></td><td>'+oMeta.securityList[i].display+'</td></tr>'+#13#10);
     s.append('</table>'+#13#10);
   end;
   s.append('<p></p>'+#13#10);

    s.append(
'<p><br/>'+Footer(FBaseURL, lang)
    );
    s.WriteToStream(stream);
  finally
    s.free;
  end;
end;
}


function tail(s : String):String;
begin
  result := copy(s, LastDelimiter('/', s)+1, $FF);
end;

procedure TFHIRXhtmlComposer.ComposeBundle(stream: TStream; bundle: TFHIRBundle);
var
  s : TFslStringBuilder;
  i : integer;
  e : TFhirBundleEntry;
  ss : TBytesStream;
  xml : TFHIRXmlComposer;
  r : TFhirResource;
  t, link, text, sl, ul : String;
  a : TArray<String>;
begin
  s := TFslStringBuilder.create;
  try
    s.append(
'<?xml version="1.0" encoding="UTF-8"?>'+#13#10+
'<!DOCTYPE HTML'+#13#10+
'       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'+#13#10+
''+#13#10+
'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'+#13#10+
'<head>'+#13#10+
'    <title>'+TYPE_TITLE[bundle.type_]+'</title>'+#13#10+
PageLinks+
FHIR_JS+#13#10+
'</head>'+#13#10+
''+#13#10+
'<body>'+#13#10+
''+#13#10+
Header(Session, FBaseURL, lang, FVersion)+
'<h1>'+TYPE_TITLE[bundle.type_]+'</h1>'+#13#10);

  ul := bundle.link_List.Matches['self'];
  if pos('?', ul) = 0 then
    ul := ul + '?'
  else
    ul := ul + '&';
  s.append('<p><a href="'+ul+'_format=xml">XML</a> '+GetFhirMessage('OR', lang)+' <a href="'+ul+'_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang)+'</p>'+#13#10);

    if (bundle.type_ in [BundleTypeSearchset, BundleTypeHistory])  then
    begin
      s.append('<p>'+GetFhirMessage('NAME_LINKS', lang)+':&nbsp;');
      if (bundle.link_List.Matches['first'] <> '') then
        s.append('<a href="'+bundle.link_List.Matches['first']+'">'+GetFhirMessage('NAME_FIRST', lang)+'</a>&nbsp;')
      else
        s.append('<span style="color: grey">'+GetFhirMessage('NAME_FIRST', lang)+'</span>&nbsp;');
      if (bundle.link_List.Matches['previous'] <> '') then
        s.append('<a href="'+bundle.link_List.Matches['previous']+'">'+GetFhirMessage('NAME_PREVIOUS', lang)+'</a>&nbsp;')
      else
        s.append('<span style="color: grey">'+GetFhirMessage('NAME_PREVIOUS', lang)+'</span>&nbsp;');
      if (bundle.link_List.Matches['next'] <> '') then
        s.append('<a href="'+bundle.link_List.Matches['next']+'">'+GetFhirMessage('NAME_NEXT', lang)+'</a>&nbsp;')
      else
        s.append('<span style="color: grey">'+GetFhirMessage('NAME_NEXT', lang)+'</span>&nbsp;');
      if (bundle.link_List.Matches['last'] <> '') then
        s.append('<a href="'+bundle.link_List.Matches['last']+'">'+GetFhirMessage('NAME_LAST', lang)+'</a>&nbsp;')
      else
        s.append('<span style="color: grey">'+GetFhirMessage('NAME_LAST', lang)+'</span>&nbsp;');
      if bundle.Total <> '' then
        s.append(' ('+bundle.Total+' '+GetFhirMessage('FOUND', lang)+'). ');
      s.append('<span style="color: grey">'+GetFhirMessage('NAME_SEARCH', lang)+': '+bundle.link_List.Matches['self']+'</span>&nbsp;</p>');
      if bundle.tags['sql'] <> '' then
        s.append('<p>SQL (for debugging): <span style="color: maroon">'+FormatTextToXML(bundle.tags['sql'], xmlText)+'</span></p>');
    end;

    for i := 0 to bundle.entryList.Count - 1 do
    begin
      e := bundle.entryList[i];
      r := e.resource;
      if (r = nil) then
      begin
        if (e.request <> nil) and (e.request.method = HttpVerbDELETE) then
        begin
          a := e.request.url.Split(['/']);
          s.append('<h2>'+a[length(a)-2]+' '+a[length(a)-1]+' deleted</h2>'+#13#10);
          s.append('<p>'+FormatTextToXML(e.tags['opdesc'], xmlText));
          if e.link_List.Matches['audit'] <> '' then

            s.append(' (<a href="'+BaseURL+e.link_List.Matches['audit']+'">Audit</a>)');
          s.append('</p>'+#13#10);
        end
        else
          s.append('<h2>nil?</h2>'+#13#10)
      end
      else
      begin
      t := GetFhirMessage(CODES_TFHIRResourceType[r.ResourceType], lang)+' "'+r.id+'"';
      if (r.id = '') then
        sl := ''
      else
      begin
        sl := AppendForwardSlash(BaseURL)+ CODES_TFHIRResourceType[r.ResourceType]+'/'+r.id;
        if (r.meta <> nil) and (r.meta.versionId <> '') then
        begin
          t := t +' '+GetFhirMessage('NAME_VERSION', lang)+' "'+r.meta.versionId+'"';
          sl := sl + '/_history/'+r.meta.versionId;
        end;
      end;

      s.append('<h2>'+FormatTextToXml(t, xmlText)+'</h2>'+#13#10);
      if e.tags['opdesc'] <>'' then
      begin
        s.append('<p>'+FormatTextToXML(e.tags['opdesc'], xmlText));
        if e.link_List.Matches['audit'] <> '' then
          s.append(' (<a href="'+BaseURL+e.link_List.Matches['audit']+'">Audit</a>)');
        s.append('</p>'+#13#10);
      end;
      if (r.meta <> nil) then
        s.append('<p><a href="'+e.id+'/_tags">'+GetFhirMessage('NAME_TAGS', lang)+'</a>: '+PresentTags(r.ResourceType, sl+'/_tags', r.meta, i+1)+'</p>'+#13#10);

      if e.search <> nil then
      begin
        s.Append('<p>Search Information: Mode = '+CODES_TFhirSearchEntryModeEnum[e.search.mode]);
        if e.search.scoreElement <> nil then
          s.Append(', score = '+e.search.score);
        if e.search.hasExtension('http://hl7.org/fhir/StructureDefinition/patient-mpi-match') then
          s.Append(', mpi says '+e.search.getExtensionString('http://hl7.org/fhir/StructureDefinition/patient-mpi-match'));
        s.Append('</p>');
      end;

      if (sl <> '')  then
      begin
        s.append('<p><a href="'+sl+'">'+GetFhirMessage('THIS_RESOURCE', lang)+'</a> ');
      if not (r is TFhirBinary) then
        begin
        s.append(
          ', <a href="'+sl+'?_format=xml">XML</a> '+GetFhirMessage('OR', lang)+' '+
        '<a href="'+sl+'?_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang));
        s.append(
          ', '+GetFhirMessage('OR', lang)+' <a href="'+e.id+'/_history">'+GetFhirMessage('NAME_HISTORY', lang)+'</a>.');

        if (e.tags['z-edit-src'] <> '') then
          s.append(' Edit this as <a href="'+patchToWeb(e.tags['z-edit-src'])+'?srcformat=xml">XML</a> or <a href="'+patchToWeb(e.tags['z-edit-src'])+'?srcformat=json">JSON</a>.');

        {$IFDEF FHIR_QUESTIONNAIRERESPONSE}
        if e.tags['edit-form'] <> '' then
          if (r is TFHIRQuestionnaireResponse) then
          begin
            if (TFHIRQuestionnaireResponse(r).questionnaire <> nil) then
              s.append(' <a href="'+patchToWeb(e.tags['edit-form'])+'">Edit this Resource</a> (or <a href="'+TFHIRQuestionnaireResponse(r).questionnaire.reference+'">see the questionnaire</a>)')
          end
          else
            s.append(' <a href="'+patchToWeb(e.tags['edit-form'])+'">Edit this Resource</a> (or just see <a href="'+e.tags['edit-form']+'">the Questionnaire</a>)');
        if e.tags['edit-post'] <> '' then
          s.append(' Submit edited content by POST to '+e.tags['edit-post']);
        {$ENDIF}

        if assigned(FOnGetLink) then
        begin
          FOnGetLink(r, BaseURL, '', tail(e.id), tail(sl), link, text);
          if (link <> '') then
            s.append(' <a href="'+link+'">'+FormatTextToHTML(text)+'</a>');
        end;
        if not (r.resourceType in [frtProvenance, frtAuditEvent]) then
          s.append('. <a href="'+FBaseURL+'Provenance/target='+r.fhirType+'/'+r.id+'">provenance for this resource</a>');
        s.append('</br> Updated: '+e.tags['updated']+' by '+e.tags['author']+'</p>'+#13#10);
        end;
      end;

//      if e.deleted then
//        s.append('<p>'+GetFhirMessage('MSG_DELETED', lang)+'</p>')
//      else if r = nil then
//        s.append('<p>(--)</p>')
//      else
      if r is TFhirBinary then
      begin
        if StringStartsWith(TFhirBinary(r).ContentType, 'image/', true) then
          s.append('<img src="'+CODES_TFhirResourceType[r.resourcetype]+'/'+e.id+'">'+#13#10)
        else
          s.append('<pre class="xml">'+#13#10+'('+GetFhirMessage('NAME_BINARY', lang)+')'+#13#10+'</pre>'+#13#10);
      end
      else
      begin
        xml := TFHIRXmlComposer.create(Fworker.link, OutputStylePretty, lang);
        ss := TBytesStream.create();
        try
          if (r is TFhirDomainResource) and (TFhirDomainResource(r).text <> nil) and (TFhirDomainResource(r).text.div_ <> nil) then
            TFHIRXhtmlParser.Compose(TFhirDomainResource(r).text.div_, s, false, 2, relativeReferenceAdjustment);
          xml.Compose(ss, r);
          s.append('<hr/>'+#13#10+'<pre class="xml">'+#13#10+FormatXMLToHTML(TENcoding.UTF8.getString(ss.bytes, 0, ss.size))+#13#10+'</pre>'+#13#10);
        finally
          ss.free;
          xml.free;
        end;
      end;
      end;
    end;
    s.append(
'<p><br/>'
+footer(FBaseUrl, lang, logid)
    );
    s.WriteToStream(stream);
  finally
    s.free;
  end;
end;

procedure TFHIRXhtmlComposer.ComposeResourceV(xml: TXmlBuilder; oResource: TFhirResourceV);
begin
  raise Exception.Create('TFHIRXhtmlComposer.ComposeResourceV should never be called');

end;

(*
procedure TFHIRXhtmlComposer.ComposeResource(xml: TXmlBuilder; oResource: TFhirResourceV);
var
  oHtml : TFhirXHtmlNode;
  oDoc : TFhirXHtmlNode;
  oHead : TFhirXHtmlNode;
  oWork : TFhirXHtmlNode;
begin
  oHtml := TFhirXHtmlNode.create;
  try
    oHtml.NodeType := fhntDocument;
    oHtml.AddComment('Generated by Server automatically');
    oDoc := oHtml.AddChild('html');
    oHead := oDoc.AddChild('head');
    oWork := oHead.AddChild('title');
    oWork.AddText('test title');
    oWork := oHead.AddChild('link');
    oWork.SetAttribute('rel', 'Stylesheet');
    oWork.SetAttribute('href', '/css/fhir.css');
    oWork.SetAttribute('type', 'text/css');
    oWork.SetAttribute('media', 'screen');
    oWork := oDoc.AddChild('body');
    if (oResource is TFhirDomainResource) and (TFhirDomainResource(oResource).text <> nil) And (TFhirDomainResource(oResource).text.div_ <> nil) Then
    begin
      oWork.Attributes.addAll(TFhirDomainResource(oResource).text.div_.Attributes);
      oWork.ChildNodes.AddAll(TFhirDomainResource(oResource).text.div_.ChildNodes);
    end;
    TFHIRXhtmlParser.compose(oHtml, xml);
  finally
    oHtml.Free;
  end;
end;
*)

constructor TFHIRXhtmlComposer.Create(worker: TFHIRWorkerContext; Style : TFHIROutputStyle; lang, BaseURL: String);
begin
  Create(worker, Style, lang);
  FBaseURL := BaseURL;
end;


destructor TFHIRXhtmlComposer.Destroy;
begin
  FLinks.Free;
  FSession.free;
  FTags.Free;
  inherited;
end;

function TFHIRXhtmlComposer.Extension: String;
begin
  result := '.html';
end;

class function TFHIRXhtmlComposer.Footer(base, lang, logId : String; tail : boolean = true): string;
begin
  result :=
    '</div>'+#13#10+
    ''+#13#10+
    ''+#13#10+
    '				</div>  <!-- /inner-wrapper -->'+#13#10+
    '            </div>  <!-- /row -->'+#13#10+
    '        </div>  <!-- /container -->'+#13#10+
    '    </div>  <!-- /segment-content -->'+#13#10+
    ''+#13#10+
    ''+#13#10+
    '	<div id="segment-footer" class="segment">  <!-- segment-footer -->'+#13#10+
    '		<div class="container">  <!-- container -->'+#13#10+
    '			<div class="inner-wrapper">'+#13#10+
    '				<p>'+#13#10+
    '        <a href="'+base+'" style="color: gold">'+GetFhirMessage('SERVER_HOME', lang)+'</a>.&nbsp;|&nbsp;FHIR &copy; HL7.org 2011+. &nbsp;|&nbsp; FHIR '+GetFhirMessage('NAME_VERSION', lang)+' <a href="'+FHIR_SPEC_URL+'" style="color: gold">'+FHIR_GENERATED_VERSION+'</a>'+#13#10+
    '        | Request-id: '+logId+
    '        </span>'+#13#10+
    '        </p>'+#13#10+
    '			</div>  <!-- /inner-wrapper -->'+#13#10+
    '		</div>  <!-- /container -->'+#13#10+
    '	</div>  <!-- /segment-footer -->'+#13#10+
    ''+#13#10+
    ''+#13#10+
    '	<div id="segment-post-footer" class="segment hidden">  <!-- segment-post-footer -->'+#13#10+
    '		<div class="container">  <!-- container -->'+#13#10+
    '		</div>  <!-- /container -->'+#13#10+
    '	</div>  <!-- /segment-post-footer -->'+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    '      <!-- JS and analytics only. -->'+#13#10+
    '      <!-- Bootstrap core JavaScript'+#13#10+
    '================================================== -->'+#13#10+
    '  <!-- Placed at the end of the document so the pages load faster -->'+#13#10+
    '<script src="/assets/js/jquery.js"/>'+#13#10+
    '<script src="/dist/js/bootstrap.min.js"/>'+#13#10+
    '<script src="/assets/js/respond.min.js"/>'+#13#10+
    ''+#13#10+
    '<script src="/assets/js/fhir.js"/>'+#13#10+
    ''+#13#10+
    '  <!-- Analytics Below'+#13#10+
    '================================================== -->'+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10;
if tail then
  result := result +
    '</body>'+#13#10+
    '</html>'+#13#10;
end;

function TFHIRXhtmlComposer.GetFormat: TFHIRFormat;
begin
  result := ffXhtml;
end;

class function TFHIRXhtmlComposer.Header(Session : TFhirSession; base, lang, version: String): String;
var
   id : TFHIRCompartmentId;
   f : boolean;
begin
  result :=
    '	<div id="segment-navbar" class="segment">  <!-- segment-breadcrumb -->'+#13#10+
    '		<div id="stripe"> </div>'+#13#10+
    '		<div class="container">  <!-- container -->'+#13#10+
    '		<div style="background-color: #ad1f2f; padding: 6px; color: white;">  <!-- container -->'+#13#10;


  result := result +
    '  <a href="http://www.hl7.org/fhir" style="color: gold" title="'+GetFhirMessage('MSG_HOME_PAGE_TITLE', lang)+'"><img border="0" src="/icon-fhir-16.png" style="vertical-align: text-bottom"/> <b>FHIR</b></a>'#13#10+
    ''#13#10+
    '  &copy; HL7.org'#13#10+
    '  &nbsp;|&nbsp;'#13#10+
    '  <a href="/" style="color: gold">'+GetFhirMessage('SERVER_HOME', lang)+'</a> '+
    '  &nbsp;|&nbsp;'#13#10+
    '  <a href="http://www.healthintersections.com.au" style="color: gold">Health Intersections</a> '+GetFhirMessage('NAME_SERVER', lang)+' v'+version+#13#10+
    '  &nbsp;|&nbsp;'#13#10+
    '  <a href="'+FHIR_SPEC_URL+'" style="color: gold">FHIR '+GetFhirMessage('NAME_VERSION', lang)+' '+FHIR_GENERATED_VERSION+'</a>'#13#10;

  if (session <> nil)  then
  begin
    result := result +'&nbsp;|&nbsp;';
    if session.canGetUser then
      result := result +'User: '+FormatTextToXml(Session.SessionName, xmlText)
    else
      result := result +'User: [n/a]';
    if session.UserEvidence <> userAnonymous then
      result := result +'&nbsp; <a href="'+base+'/logout" title="Log Out"><img src="/logout.png"></a>';
    if session.Compartments.Count > 0 then
    begin
      if session.Compartments.Count = 1 then
        result := result+'  &nbsp;'#13#10+'</div><div style="background-color: #e5e600; padding: 6px; color: black;"> This session limited to '+CODES_TFhirResourceType[session.Compartments[0].Enum]+' '
      else
        result := result+'  &nbsp;'#13#10+'</div><div style="background-color: #e5e600; padding: 6px; color: black;"> . This session limited to the following compartments: ';
      f := true;
      for id in session.Compartments do
      begin
        if f then
          f := false
        else
          result := result +', ';
        result := result + id.ToString;
      end;
    end;
  end;

  result := result +
    '  &nbsp;'#13#10+
    '		</div>  <!-- /container -->'+#13#10+
    '		</div>  <!-- /container -->'+#13#10+
    '</div>'#13#10+
    ''#13#10;
//    if FFacebookLike and (FOauthUrl <> '') then
//      result := result + '<iframe src="https://www.facebook.com/plugins/like.php?href='+FOauthUrl+'" scrolling="no" frameborder="0" style="border:none; width:450px; height:30px"></iframe>'#13#10;

  result := result +
    '	<!-- /segment-breadcrumb -->'+#13#10+
    ''+#13#10+
    '	<div id="segment-content" class="segment">  <!-- segment-content -->'+#13#10+
    '	<div class="container">  <!-- container -->'+#13#10+
    '            <div class="row">'+#13#10+
    '            	<div class="inner-wrapper">'+#13#10+
    ' <div id="div-cnt" class="col-9">'+#13#10+
    ''+#13#10+
    ''+#13#10;
end;

function TFHIRXhtmlComposer.MimeType: String;
begin
  result := 'text/html; charset=UTF-8';
end;

class function TFHIRXhtmlComposer.PageLinks: String;
begin
  result :=
    '  <meta charset="utf-8"/>'+#13#10+
    '  <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />'+#13#10+
    '  <meta content="width=device-width, initial-scale=1.0" name="viewport"/>'+#13#10+
    '  <meta content="http://hl7.org/fhir" name="author"/>'+#13#10+
    ''+#13#10+
    '  <link rel="stylesheet" href="/fhir.css"/>'+#13#10+
    ''+#13#10+
    ''+#13#10+
    '    <!-- Bootstrap core CSS -->'+#13#10+
    '  <link rel="stylesheet" href="/dist/css/bootstrap.css"/>'+#13#10+
    '  <link rel="stylesheet" href="/assets/css/bootstrap-fhir.css"/>'+#13#10+
    ''+#13#10+
    '    <!-- Project extras -->'+#13#10+
    '  <link rel="stylesheet" href="/assets/css/project.css"/>'+#13#10+
    '  <link rel="stylesheet" href="/assets/css/pygments-manni.css"/>'+#13#10+
    ''+#13#10+
    '    <!-- FHIR Server stuff -->'+#13#10+
    '  <link rel="stylesheet" href="/css/tags.css"/>'+#13#10+
    ''+#13#10+
    '    <!-- HTML5 shim and Respond.js IE8 support of HTML5 elements and media queries -->'+#13#10+
    '    <!-- [if lt IE 9]>'+#13#10+
    '  <script src="/assets/js/html5shiv.js"></script>'+#13#10+
    '  <script src="/assets/js/respond.min.js"></script>'+#13#10+
    '  <![endif] -->'+#13#10+
    ''+#13#10+
    '    <!-- Favicons -->'+#13#10+
    '  <link sizes="144x144" rel="apple-touch-icon-precomposed" href="/assets/ico/apple-touch-icon-144-precomposed.png"/>'+#13#10+
    '  <link sizes="114x114" rel="apple-touch-icon-precomposed" href="/assets/ico/apple-touch-icon-114-precomposed.png"/>'+#13#10+
    '  <link sizes="72x72" rel="apple-touch-icon-precomposed" href="/assets/ico/apple-touch-icon-72-precomposed.png"/>'+#13#10+
    '  <link rel="apple-touch-icon-precomposed" href="/assets/ico/apple-touch-icon-57-precomposed.png"/>'+#13#10+
    '  <link rel="shortcut icon" href="/assets/ico/favicon.png"/>'+#13#10;
end;

function TFHIRXhtmlComposer.PresentTags(aType : TFhirResourceType; target : String; meta: TFhirMeta; c : integer): String;
var
  i : integer;
//  lbl : string;
//  clss, typ : string;
begin
  if tags.count = 0 then
    result := '(no tags)'
  else
  begin
    result := '';
    for i := 0 to tags.count - 1 do
    begin
{ todo-bundle
      lbl := tags[i].label_;
      if lbl = '' then
        lbl := URLTail(tags[i].term);
      if (length(lbl) > 20) then
        lbl := Copy(lbl, 1, 20)+'..';

      if tags[i].scheme = TAG_FHIR_SCHEME_PROFILE then
      begin
        clss := 'tag-profile';
        typ := 'Profile: ';
      end
      else if tags[i].scheme = TAG_FHIR_SCHEME_SECURITY then
    begin
        clss := 'tag-security';
        typ := 'Security: ';
      end
      else
        clss := 'tag';

      if aType = frtNull then
        result := result + '<a href="'+FBaseUrl+'_search?'+paramForScheme(tags[i].scheme)+'='+EncodeMIME(tags[i].term)+'" class="'+clss+'" title="'+typ+tags[i].term+'">'+lbl+'</a>'
      else
      begin
        result := result + '<a href="'+FBaseUrl+CODES_TFhirResourceType[aType]+'/_search?'+paramForScheme(tags[i].scheme)+'='+EncodeMIME(tags[i].term)+'" class="'+clss+'" title="'+typ+tags[i].term+'">'+lbl+'</a>';
        if (target <> '') then
          result := result + '<a href="javascript:deleteTag('''+target+'/_delete'', '''+tags[i].scheme+''', '''+tags[i].term+''')" class="tag-delete" title="Delete '+tags[i].term+'">-</a>'
      end;
      result := result + '&nbsp;';
    }
    end;
  end;
  if target <> '' then
    result := result +'&nbsp; <a id="tb'+inttostr(c)+'" class="tag" title="Add a tag" href="javascript:addTag(''tb'+inttostr(c)+''', '''+FBaseUrl+''', '''+target+''')">+</a>';
end;

class function TFHIRXhtmlComposer.ResourceLinks(a : String; lang, base : String; count : integer; bTable, bPrefixLinks : boolean; canRead : boolean): String;
var
  bef, aft, pfx, pfxp : String;
begin
  if bPrefixLinks then
  begin
    pfx := base+'/'+a+'/';
    pfxp := base+'/'+'StructureDefinition/'
  end
  else
  begin
    pfxp := '../StructureDefinition/';
    pfx := '';
  end;

  if bTable then
  begin
    bef := '<td>';
    aft := '</td>';
  end
  else
  begin
    bef := '&nbsp;';
    aft := '';
  end;
  result := bef + a + aft;
  if not bTable then
    result := result + ':';
  if count > -1 then
    result := result + bef + inttostr(count) + aft;
  if a = 'Binary' then
    result := result + bef + 'n/a' + aft
  else
    result := result + bef + '<a class="button" href="'+pfxp+a+'">'+GetFhirMessage('NAME_PROFILE', lang)+'</a>' + aft;
  if canRead then
  begin
    result := result + bef + '<a class="button" href="'+pfx+'_history">'+GetFhirMessage('NAME_UPDATES', lang)+'</a>' + aft;
    if a = 'Binary' then
      result := result + bef + 'n/a' + aft
    else
      result := result + bef + '<a class="button" href="'+pfx+'_search">'+GetFhirMessage('NAME_SEARCH', lang)+'</a>' + aft;
    if bTable then
      result := result + bef + '<a class="tag" href="'+pfx+'_tags">'+GetFhirMessage('NAME_TAGS', lang)+'</a>' + aft;
  end
  else if bTable then
    result := result + bef + aft + bef + aft + bef + aft
  else
    result := result + bef + aft + bef + aft;
end;

function TFHIRXhtmlComposer.ResourceMediaType: String;
begin
  result := 'text/html; charset=UTF-8';
end;

procedure TFHIRXhtmlComposer.SetLinks(const Value: TFhirBundleLinkList);
begin
  FLinks.Free;
  FLinks := Value;
end;

procedure TFHIRXhtmlComposer.SetSession(const Value: TFhirSession);
begin
  FSession.free;
  FSession := Value;
end;


procedure TFHIRXhtmlComposer.SetTags(const Value: TFHIRTagList);
begin
  FTags.free;
  FTags := Value;
end;

end.

//procedure TFHIRXhtmlComposer.Compose(stream: TStream; oFeed: TFHIRAtomFeed; isPretty: Boolean);
//var
//  s : TFslStringBuilder;
//  i : integer;
//  a : string;
//  e : TFHIRAtomEntry;
//  ss : TBytesStream;
//  xml : TFHIRXmlComposer;
//  link, text : String;
//  u : string;
//begin
//  a := oFeed.authorUri;
//  s := TFslStringBuilder.create;
//  try
//    s.append(
//'<?xml version="1.0" encoding="UTF-8"?>'+#13#10+
//'<!DOCTYPE HTML'+#13#10+
//'       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'+#13#10+
//''+#13#10+
//'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'+#13#10+
//'<head>'+#13#10+
//'    <title>'+FormatTextToXml(oFeed.title)+'</title>'+#13#10+
//PageLinks+
//FHIR_JS+#13#10+
//'</head>'+#13#10+
//''+#13#10+
//'<body>'+#13#10+
//''+#13#10+
//Header(Session, FBaseURL, lang)+
//'<h1>'+FormatTextToXml(oFeed.title)+'</h1>'+#13#10);
//
//  u := ofeed.links['self'];
//  if not u.contains('?') then
//    u := u + '?'
//  else
//    u := u + '&';
//  s.append('<p><a href="'+u+'_format=xml"><img src="/rss.png"> XML</a> '+GetFhirMessage('OR', lang)+' <a href="'+u+'_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang)+'</p>'+#13#10);
//
//    if (ofeed.isSearch) then
//    begin
//      s.append('<p>'+GetFhirMessage('NAME_LINKS', lang)+':&nbsp;');
//      if (ofeed.links['first'] <> '') then
//        s.append('<a href="'+ofeed.links.getrel('first')+'">'+GetFhirMessage('NAME_FIRST', lang)+'</a>&nbsp;')
//      else
//        s.append('<span style="color: grey">'+GetFhirMessage('NAME_FIRST', lang)+'</span>&nbsp;');
//      if (ofeed.links['previous'] <> '') then
//        s.append('<a href="'+ofeed.links.getrel('previous')+'">'+GetFhirMessage('NAME_PREVIOUS', lang)+'</a>&nbsp;')
//      else
//        s.append('<span style="color: grey">'+GetFhirMessage('NAME_PREVIOUS', lang)+'</span>&nbsp;');
//      if (ofeed.links['next'] <> '') then
//        s.append('<a href="'+ofeed.links.getrel('next')+'">'+GetFhirMessage('NAME_NEXT', lang)+'</a>&nbsp;')
//      else
//        s.append('<span style="color: grey">'+GetFhirMessage('NAME_NEXT', lang)+'</span>&nbsp;');
//      if (ofeed.links['last'] <> '') then
//        s.append('<a href="'+ofeed.links.getrel('last')+'">'+GetFhirMessage('NAME_LAST', lang)+'</a>&nbsp;')
//      else
//        s.append('<span style="color: grey">'+GetFhirMessage('NAME_LAST', lang)+'</span>&nbsp;');
//      if oFeed.SearchTotal <> 0 then
//        s.append(' ('+inttostr(oFeed.SearchTotal)+' '+GetFhirMessage('FOUND', lang)+'). ');
//      s.append('<span style="color: grey">'+GetFhirMessage('NAME_SEARCH', lang)+': '+ofeed.links.getrel('self')+'</span>&nbsp;</p>');
//      s.append('<p>SQL: <span style="color: maroon">'+FormatTextToXML(oFeed.sql)+'</span></p>');
//    end;
//
//    for i := 0 to oFeed.entries.Count - 1 do
//    begin
//      e := oFeed.entries[i];
//      s.append('<h2>'+FormatTextToXml(e.title)+'</h2>'+#13#10);
//      if (e.categories <> nil) and (e.Resource <> nil) then
//        s.append('<p><a href="'+e.id+'/_tags">'+GetFhirMessage('NAME_TAGS', lang)+'</a>: '+PresentTags(e.resource.ResourceType, e.links.GetRel('self')+'/_tags', e.categories, i+1        )+'</p>'+#13#10);
//
//      u := e.Links.rel['self'];
//      if (u <> '')  then
//      begin
//        s.append('<p><a href="'+e.Links.rel['self']+'">'+GetFhirMessage('THIS_RESOURCE', lang)+'</a> ');
//      if not (e.resource is TFhirBinary) then
//        begin
//        s.append(
//          ', <a href="'+e.Links.rel['self']+'?_format=xml">XML</a> '+GetFhirMessage('OR', lang)+' '+
//        '<a href="'+e.Links.rel['self']+'?_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang));
//        s.append(
//          ', '+GetFhirMessage('OR', lang)+' <a href="'+e.id+'/_history">'+GetFhirMessage('NAME_HISTORY', lang)+'</a>.');
//
//        if (e.links <> nil) and (e.links.GetRel('z-edit-src') <> '') then
//          s.append(' Edit this as <a href="'+patchToWeb(e.links.GetRel('z-edit-src'))+'?srcformat=xml">XML</a> or <a href="'+patchToWeb(e.links.GetRel('z-edit-src'))+'?srcformat=json">JSON</a>.');
//
//        {$IFNDEF FHIR_DSTU}
//        if e.links.GetRel('edit-form') <> '' then
//          if (e.resource is TFHIRQuestionnaireResponse) then
//          begin
//            if (TFHIRQuestionnaireResponse(e.resource).questionnaire <> nil) then
//              s.append(' <a href="'+patchToWeb(e.links.GetRel('edit-form'))+'">Edit this Resource</a> (or <a href="'+TFHIRQuestionnaireResponse(e.resource).questionnaire.reference+'">see the questionnaire</a>)')
//          end
//          else
//            s.append(' <a href="'+patchToWeb(e.links.GetRel('edit-form'))+'">Edit this Resource</a> (or just see <a href="'+e.links.GetRel('edit-form')+'">the Questionnaire</a>)');
//        if e.links.GetRel('edit-post') <> '' then
//          s.append(' Submit edited content by POST to '+e.links.GetRel('edit-post'));
//        {$ENDIF}
//
//        if assigned(FOnGetLink) then
//        begin
//          FOnGetLink(e.resource, BaseURL, '', tail(e.id), tail(e.Links.rel['self']), link, text);
//          if (link <> '') then
//            s.append(' <a href="'+link+'">'+FormatTextToHTML(text)+'</a>');
//        end;
//        s.append('</br> Updated: '+e.updated.AsXML+'; Author: '+Author(e, a)+'</p>'+#13#10);
//        end;
//      end;
//
//      if e.deleted then
//        s.append('<p>'+GetFhirMessage('MSG_DELETED', lang)+'</p>')
//      else if e.resource = nil then
//        s.append('<p>(--)</p>')
//      else if e.resource is TFhirBinary then
//      begin
//        if StringStartsWith(TFhirBinary(e.resource).ContentType, 'image/') then
//          s.append('<img src="'+CODES_TFhirResourceType[e.resource.resourcetype]+'/'+e.id+'">'+#13#10)
//        else
//          s.append('<pre class="xml">'+#13#10+'('+GetFhirMessage('NAME_BINARY', lang)+')'+#13#10+'</pre>'+#13#10);
//      end
//      else
//      begin
//        xml := TFHIRXmlComposer.create(lang);
//        ss := TBytesStream.create('');
//        try
//          if (e.resource.text <> nil) and (e.resource.text.div_ <> nil) then
//            ComposeXHtmlNode(s, e.resource.text.div_, 2, relativeReferenceAdjustment);
//          xml.Compose(ss, '', e.id, tail(e.links.rel['self']), e.resource, true, e.links);
//          s.append('<hr/>'+#13#10+'<pre class="xml">'+#13#10+FormatXMLToHTML(ss.dataString)+#13#10+'</pre>'+#13#10);
//        finally
//          ss.free;
//          xml.free;
//        end;
//      end;
//    end;
//    s.append(
//'<p><br/>'
//+footer(FBaseUrl, lang)
//    );
//    s.WriteToStream(stream);
//  finally
//    s.free;
//  end;
//end;
//
