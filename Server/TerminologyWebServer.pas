unit TerminologyWebServer;

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
  SysUtils, Classes, System.Generics.Collections,
  ParseMap,
  EncodeSupport, StringSupport, TextUtilities,
  AdvObjects, AdvStringMatches, AdvNameBuffers,
  IdContext, IdCustomHTTPServer,
  FHIRLang, FHIRContext, FHIRSupport, FHIRUtilities, FHIRResources, FHIRTypes, FHIRXhtml, FHIRBase,
  HtmlPublisher, SnomedPublisher, SnomedServices, LoincPublisher, LoincServices, SnomedExpressions, SnomedAnalysis,
  TerminologyServer, TerminologyServices, TerminologyServerStore, FHIRServerConstants, FHIROperations;

Type
  TReturnProcessFileEvent = procedure (request : TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; secure : boolean; variables: TDictionary<String, String>) of Object;

  TTerminologyWebServer = class (TAdvObject)
  private
    FWorker : TFHIRWorkerContext;
    FServer : TTerminologyServer;
    FFHIRPath : String;
    FReturnProcessFileEvent : TReturnProcessFileEvent;
    function asJson(r : TFHIRResource) : String;
    function asXml(r : TFHIRResource) : String;
    function asHtml(r : TFHIRDomainResource) : String;
    function paramsAsHtml(p : TFhirParameters) : String;
    function vsSelect(id : String) : String;

    function processFind(pm : TParseMap) : String;
    function processValidate(pm : TParseMap) : String;
    function processExpand(pm : TParseMap; lang : string) : String;
    function processTranslate(pm : TParseMap) : String;

    function chooseSnomedRelease() : String;
    Procedure HandleLoincRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure HandleSnomedRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure HandleTxRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
    Procedure HandleTxForm(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; response: TIdHTTPResponseInfo; secure : boolean);
//    Procedure BuildCsByName(html : THtmlPublisher; id : String);
//    Procedure BuildCsByURL(html : THtmlPublisher; id : String);
//    Procedure BuildVsByName(html : THtmlPublisher; id : String);
//    Procedure BuildVsByURL(html : THtmlPublisher; id : String);
    function processSnomedForTool(ss : TSnomedServices; code : String) : String;

    function sortVsByUrl(pA, pB : Pointer) : Integer;
    function sortVsByVer(pA, pB : Pointer) : Integer;
    function sortVsByName(pA, pB : Pointer) : Integer;
    function sortVsByCtxt(pA, pB : Pointer) : Integer;
    function sortVsByPub(pA, pB : Pointer) : Integer;
    function sortVsBySrc(pA, pB : Pointer) : Integer;
    function sortCsByUrl(pA, pB : Pointer) : Integer;
    function sortCsByVer(pA, pB : Pointer) : Integer;
    function sortCsByName(pA, pB : Pointer) : Integer;
    function sortCsByCtxt(pA, pB : Pointer) : Integer;
    function sortCsByPub(pA, pB : Pointer) : Integer;
//    function sortVsByDefUrl(pA, pB : Pointer) : Integer;
//    function sortVsByDefVer(pA, pB : Pointer) : Integer;
    function sortCmByUrl(pA, pB : Pointer) : Integer;
    function sortCmByVer(pA, pB : Pointer) : Integer;
    function sortCmByName(pA, pB : Pointer) : Integer;
    function sortCmByCtxt(pA, pB : Pointer) : Integer;
    function sortCmByPub(pA, pB : Pointer) : Integer;
    function sortCmBySrc(pA, pB : Pointer) : Integer;
    function sortCmByTgt(pA, pB : Pointer) : Integer;
    procedure ProcessValueSetList(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
    procedure ProcessConceptMapList(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
    procedure ProcessCodeSystemList(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
    procedure ProcessCodeSystemProviderList(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
    procedure ProcessValueSet(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
    procedure ProcessCodeSystem(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
    procedure ProcessConceptMap(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
    procedure ProcessHome(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
  public
    constructor create(server : TTerminologyServer; Worker : TFHIRWorkerContext; BaseURL, FHIRPath : String; ReturnProcessFileEvent : TReturnProcessFileEvent); overload;
    destructor Destroy; Override;
    function HandlesRequest(path : String) : boolean;
    Procedure Process(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; response: TIdHTTPResponseInfo; secure : boolean);
  end;

implementation

uses
  FHIRLog,
  FHIRParser;

{ TTerminologyWebServer }

function TTerminologyWebServer.chooseSnomedRelease: String;
var
  html : THtmlPublisher;
  ss : TSnomedServices;
begin
  html := THtmlPublisher.Create;
  try
    html.Version := SERVER_VERSION;
    html.Header('Choose SNOMED CT Version');
    html.StartTable(true);
    html.StartTableRow;
    html.AddTableCell('Choose SNOMED Edition');
    html.AddTableCell('Version');
    html.AddTableCell('Date');
    html.EndTableRow;
    for ss in FServer.Snomed do
    begin
      html.StartTableRow;
      html.AddTableCellURL(ss.EditionName, '/snomed/'+ss.editionId);
      html.AddTableCell(ss.VersionUri);
      html.AddTableCell(ss.VersionDate);
      html.EndTableRow;
    end;
    html.EndTable;
    html.done;
    result := html.output;
  finally
    html.Free;
  end;
end;

constructor TTerminologyWebServer.create(server: TTerminologyServer; Worker : TFHIRWorkerContext; BaseURL, FHIRPath : String; ReturnProcessFileEvent : TReturnProcessFileEvent);
begin
  create;
  FServer := server;
  FFHIRPath := FHIRPath;
  FReturnProcessFileEvent := ReturnProcessFileEvent;
  FServer.webBase := BaseURl;
  FWorker := worker;
end;

destructor TTerminologyWebServer.Destroy;
begin
  FWorker.Free;
  FServer.free;
  inherited;
end;

function TTerminologyWebServer.HandlesRequest(path: String): boolean;
begin
  result := path.StartsWith('/tx') or path.StartsWith('/snomed') or path.StartsWith('/loinc') ;
end;

procedure TTerminologyWebServer.Process(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; response: TIdHTTPResponseInfo; secure : boolean);
var
  path : string;
begin
  path := request.Document;
  if path.StartsWith('/tx/form') then
    HandleTxForm(AContext, request, session, response, secure)
  else if path.StartsWith('/tx') then
    HandleTxRequest(AContext, request, response, session)
  else if path.StartsWith('/snomed') and (FServer.Snomed <> nil) then
    HandleSnomedRequest(AContext, request, response)
  else if request.Document.StartsWith('/loinc') and (FServer.Loinc <> nil) then
    HandleLoincRequest(AContext, request, response)
end;

procedure TTerminologyWebServer.ProcessHome(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
var
  pm: TParseMap;
var
  vars : TDictionary<String, String>;
begin
  vars := TDictionary<String, String>.create;
  try
    pm := TParseMap.create(request.UnparsedParams);
    try

      vars.Add('param.system', pm.GetVar('system'));
      vars.Add('param.version', pm.getVar('version'));
      vars.Add('param.code', pm.getVar('code'));
      vars.Add('param.display', pm.getVar('display'));
      vars.Add('param.filter', pm.getVar('filter'));
      vars.Add('valuesetlist', vsSelect(pm.getVar('valueset')));
      if pm.getVar('nodetails') = '1' then
        vars.Add('param.nodetails', ' checked')
      else
        vars.Add('param.nodetails', '');
      if pm.getVar('abstract') = '1' then
        vars.Add('param.abstract', ' checked')
      else
        vars.Add('param.abstract', '');

      vars.Add('find.results', '');
      vars.Add('validate.results', '');
      vars.Add('expand.results', '');
      vars.Add('translate.results', '');

      if pm.getVar('op') = 'find' then
        vars['find.results'] := processFind(pm)
      else if pm.getVar('op') = 'validate' then
        vars['validate.results'] := processValidate(pm)
      else if pm.getVar('op') = 'expand' then
        vars['expand.results'] := processExpand(pm, request.AcceptLanguage)
      else if pm.getVar('op') = 'translate' then
        vars['translate.results'] := processTranslate(pm);

      FReturnProcessFileEvent(request, response, session, request.Document, 'txhome.html', false, vars);
    finally
      pm.Free;
    end;
  finally
    vars.Free;
  end;
end;

procedure TTerminologyWebServer.ProcessConceptMap(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
var
  cm: TLoadedConceptMap;
  vars : TDictionary<String, String>;
begin
  vars := TDictionary<String, String>.create;
  try
    cm := FServer.getConceptMapById(request.Document.Substring(9));
    try
      vars.Add('url', cm.resource.url);
      vars.Add('name', cm.resource.name);
      vars.Add('html', ashtml(cm.resource));
      vars.Add('json', asJson(cm.resource));
      vars.Add('xml', asXml(cm.resource));
    finally
      cm.Free;
    end;
    FReturnProcessFileEvent(request, response, session, request.Document, 'tx-cm-id.html', false, vars);
  finally
    vars.Free;
  end;
end;

procedure TTerminologyWebServer.ProcessValueSet(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
var
  vs: TFhirValueSet;
  vars : TDictionary<String, String>;
begin
  vars := TDictionary<String, String>.create;
  try
    vs := FServer.getValueSetById(request.Document.Substring(14));
    try
      vars.Add('url', vs.url);
      vars.Add('name', vs.name);
      vars.Add('html', ashtml(vs));
      vars.Add('json', asJson(vs));
      vars.Add('xml', asXml(vs));
    finally
      vs.Free;
    end;
    FReturnProcessFileEvent(request, response, session, request.Document, 'tx-vs-id.html', false, vars);
  finally
    vars.Free;
  end;
end;

procedure TTerminologyWebServer.ProcessCodeSystem(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
var
  cs: TFhirCodeSystem;
  vars : TDictionary<String, String>;
begin
  vars := TDictionary<String, String>.create;
  try
    cs := FServer.getCodeSystemById(request.Document.Substring(16));
    try
      vars.Add('url', cs.url);
      vars.Add('name', cs.name);
      vars.Add('html', ashtml(cs));
      vars.Add('json', asJson(cs));
      vars.Add('xml', asXml(cs));
    finally
      cs.Free;
    end;
    FReturnProcessFileEvent(request, response, session, request.Document, 'tx-cs-id.html', false, vars);
  finally
    vars.Free;
  end;
end;

procedure TTerminologyWebServer.ProcessCodeSystemProviderList(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
var
  html: THtmlPublisher;
  cs: TCodeSystemProvider;
  c: Integer;
var
  vars : TDictionary<String, String>;
begin
  vars := TDictionary<String, String>.create;
  try
    html := THtmlPublisher.Create;
    try
      html.Version := SERVER_VERSION;
      html.StartTable(true);
      html.StartTableRow;
      html.AddTableCell('URL', true);
      html.AddTableCell('Version', true);
      html.AddTableCell('Name', true);
      html.AddTableCell('#concepts', true);
      html.EndTableRow;
      for cs in FServer.ProviderClasses.Values do
      begin
        html.StartTableRow;
        html.AddTableCell(cs.system(nil));
        html.AddTableCell(cs.version(nil));
        html.AddTableCell(cs.name(nil));
        c := cs.TotalCount;
        if c > 0 then
          html.AddTableCell(inttostr(c))
        else
          html.AddTableCell('');
        html.EndTableRow;
      end;
      html.EndTable;
      vars.Add('table', html.output);
      vars.add('kind', 'Implicit Code System');
    finally
      html.Free;
    end;
    FReturnProcessFileEvent(request, response, session, request.Document, 'tx-vs.html', false, vars);
  finally
    vars.Free;
  end;
end;

procedure TTerminologyWebServer.ProcessCodeSystemList(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
var
  list: TFhirCodeSystemList;
  vs: TFhirCodeSystem;
  vars : TDictionary<String, String>;
  html : THtmlPublisher;
begin
  vars := TDictionary<String, String>.create;
  try
    list := FServer.GetCodeSystemList;
    try
      if (request.UnparsedParams.EndsWith('=ver')) then
        list.SortedBy(sortCsByVer)
      else if (request.UnparsedParams.EndsWith('=name')) then
        list.SortedBy(sortCsByName)
      else if (request.UnparsedParams.EndsWith('=ctxt')) then
        list.SortedBy(sortCsByCtxt)
      else if (request.UnparsedParams.EndsWith('=pub')) then
        list.SortedBy(sortCsByPub)
      else
        list.SortedBy(sortCsByUrl);
      html := THtmlPublisher.Create;
      try
        html.Version := SERVER_VERSION;
        html.StartTable(true);
        html.StartTableRow;
        html.AddTableCellURL('URL', '/tx/codesystems?sort=url');
        html.AddTableCellURL('Version', '/tx/codesystems?sort=ver');
        html.AddTableCellURL('Name', '/tx/codesystems?sort=name');
        html.AddTableCellURL('Context', '/tx/codesystems?sort=ctxt');
        html.AddTableCellURL('Publisher', '/tx/codesystems?sort=pub');
        html.EndTableRow;
        for vs in list do
        begin
          html.StartTableRow;
          html.AddTableCellURL(vs.codeSystem.system, '/tx/codesystems/' + vs.id);
          html.AddTableCell(vs.codeSystem.version);
          html.AddTableCell(vs.name);
          html.AddTableCell(vs.context);
          html.AddTableCell(vs.publisher);
          html.EndTableRow;
        end;
        html.EndTable;
        vars.Add('table', html.output);
        vars.add('kind', 'Code System');
      finally
        html.Free;
      end;
    finally
      list.free;
    end;
    FReturnProcessFileEvent(request, response, session, request.Document, 'tx-vs.html', false, vars);
  finally
    vars.Free;
  end;
end;

procedure TTerminologyWebServer.ProcessConceptMapList(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
var
  mlist: TLoadedConceptMapList;
  i: Integer;
  vars : TDictionary<String, String>;
  html : THtmlPublisher;
  cm : TLoadedConceptMap;
begin
  vars := TDictionary<String, String>.create;
  try
    mlist := FServer.GetConceptMapList;
    try
      // determine sort order
      if (request.UnparsedParams.EndsWith('=ver')) then
        mlist.SortedBy(sortCmByVer)
      else if (request.UnparsedParams.EndsWith('=name')) then
        mlist.SortedBy(sortCmByName)
      else if (request.UnparsedParams.EndsWith('=ctxt')) then
        mlist.SortedBy(sortCmByCtxt)
      else if (request.UnparsedParams.EndsWith('=pub')) then
        mlist.SortedBy(sortCmByPub)
      else if (request.UnparsedParams.EndsWith('=src')) then
        mlist.SortedBy(sortCmBySrc)
      else if (request.UnparsedParams.EndsWith('=tgt')) then
        mlist.SortedBy(sortCmByTgt)
      else
        mlist.SortedBy(sortCmByUrl);
      // build the table
      html := THtmlPublisher.Create;
      try
        html.Version := SERVER_VERSION;
        html.StartTable(true);
        html.StartTableRow;
        html.AddTableCellURL('URL', '/tx/maps?sort=url');
        html.AddTableCellURL('Version', '/tx/maps?sort=ver');
        html.AddTableCellURL('Name', '/tx/maps?sort=name');
        html.AddTableCellURL('Context', '/tx/maps?sort=ctxt');
        html.AddTableCellURL('Publisher', '/tx/maps?sort=pub');
        html.AddTableCellURL('Source', '/tx/maps?sort=src');
        html.AddTableCellURL('Target', '/tx/maps?sort=tgt');
        html.EndTableRow;
        for i := 0 to mlist.Count - 1 do
        begin
          cm := mlist[i];
          html.StartTableRow;
          html.AddTableCellURL(cm.Resource.url, '/tx/maps/' + cm.Resource.id);
          html.AddTableCell(cm.Resource.version);
          html.AddTableCell(cm.Resource.name);
          html.AddTableCell(cm.Resource.context);
          html.AddTableCell(cm.Resource.publisher);
          if cm.Source <> nil then
            html.AddTableCellURL(cm.Source.url, '/tx/valuesets/' + cm.Source.id)
          else
            html.AddTableCell(cm.Resource.sourceDesc);
          if cm.Target <> nil then
            html.AddTableCellURL(cm.Target.url, '/tx/valuesets/' + cm.Target.id)
          else
            html.AddTableCell(cm.Resource.targetDesc);
          html.EndTableRow;
        end;
        html.EndTable;
        vars.add('kind', 'Concept Map');
        vars.Add('table', html.output);
      finally
        html.Free;
      end;
    finally
      mlist.free;
    end;
    FReturnProcessFileEvent(request, response, session, request.Document, 'tx-vs.html', false, vars);
  finally
    vars.Free;
  end;
end;

function TTerminologyWebServer.processExpand(pm: TParseMap; lang : string): String;
var
  res : TFHIRValueSet;
  vs : TFHIRValueSet;
  profile : TFhirExpansionProfile;
begin
  vs := FServer.getValueSetById(pm.GetVar('valueset'));
  profile := TFhirExpansionProfile.Create;
  try
    profile.includeDefinition := pm.GetVar('nodetails') <> '1';
    profile.limitedExpansion := true;
    if lang <> '' then
      profile.displayLanguage := lang;

    try
      res := FServer.expandVS(vs, vs.url, profile, pm.GetVar('filter'), 1000, 0, 0);
      try
        result := asHtml(res)+#13#10;
        if (not profile.includeDefinition) then
          res.text := nil;
        result := result + '<pre class="json">'+asJson(res)+'</pre>'#13#10+'<pre class="xml">'+asXml(res)+'</pre>';
      finally
        res.Free;
      end;
    except
      on e : Exception do
        result := '<div style="background: salmon">'+e.message+'</div>';
    end;
  finally
    vs.Free;
    profile.Free;
  end;
end;

function TTerminologyWebServer.processFind(pm: TParseMap): String;
var
  coding : TFHIRCoding;
  resp : TFHIRLookupOpResponse;
  p : TFhirParameters;
begin
  coding := TFhirCoding.Create;
  try
    coding.system := pm.GetVar('system');
    coding.version := pm.GetVar('version');
    coding.code := pm.GetVar('code');
    resp := TFHIRLookupOpResponse.Create;
    try
      try
        FServer.lookupCode(coding, 'en', nil, resp);
        p := resp.asParams;
        try
          result := '<div>'+paramsAsHtml(p)+'</div>'#13 +
            #10'<pre class="json">'+asJson(p)+'</pre>'#13#10+'<pre class="xml">'+asXml(p)+'</pre>'
        finally
          p.Free;
        end;
      except
        on e : exception do
          result := '<div>'+e.message+'</div>'#13;
      end;
    finally
      resp.Free;
    end;
  finally
    coding.Free;
  end;
end;

procedure TTerminologyWebServer.ProcessValueSetList(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
var
  vs: TFhirValueSet;
  vars : TDictionary<String, String>;
  list : TFhirValueSetList;
  html : THtmlPublisher;
begin
  vars := TDictionary<String, String>.create;
  try
    list := FServer.GetValueSetList;
    try
      // determine sort order
      if (request.UnparsedParams.EndsWith('=ver')) then
        list.SortedBy(sortVsByVer)
      else if (request.UnparsedParams.EndsWith('=name')) then
        list.SortedBy(sortVsByName)
      else if (request.UnparsedParams.EndsWith('=ctxt')) then
        list.SortedBy(sortVsByCtxt)
      else if (request.UnparsedParams.EndsWith('=pub')) then
        list.SortedBy(sortVsByPub)
      else if (request.UnparsedParams.EndsWith('=src')) then
        list.SortedBy(sortVsBySrc)
      else
        list.SortedBy(sortVsByUrl);
      // build the table
      html := THtmlPublisher.Create;
      try
        html.Version := SERVER_VERSION;
        html.StartTable(true);
        html.StartTableRow;
        html.AddTableCellURL('URL', '/tx/valuesets?sort=url');
        html.AddTableCellURL('Version', '/tx/valuesets?sort=ver');
        html.AddTableCellURL('Name', '/tx/valuesets?sort=name');
        html.AddTableCellURL('Context', '/tx/valuesets?sort=ctxt');
        html.AddTableCellURL('Publisher', '/tx/valuesets?sort=pub');
        html.AddTableCellURL('Source', '/tx/valuesets?sort=src');
        html.EndTableRow;
        for vs in list do
        begin
          html.StartTableRow;
          html.AddTableCellURL(vs.url, '/tx/valuesets/' + vs.id);
          html.AddTableCell(vs.version);
          html.AddTableCell(vs.name);
          html.AddTableCell(vs.context);
          html.AddTableCell(vs.publisher);
          html.AddTableCell(vs.source);
          html.EndTableRow;
        end;
        html.EndTable;
        vars.add('kind', 'Value Set');
        vars.Add('table', html.output);
      finally
        html.Free;
      end;
    finally
      list.free;
    end;
    FReturnProcessFileEvent(request, response, session, request.Document, 'tx-vs.html', false, vars);
  finally
    vars.Free;
  end;
end;

function GetId(url, prefix : string) : String;
begin
  if length(url) <= length(prefix) then
    result := ''
  else
    result := url.Substring(length(prefix)+1);
end;



procedure TTerminologyWebServer.HandleTxForm(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; response: TIdHTTPResponseInfo; secure : boolean);
{var
  vs : String;
  vars : TDictionary<String, String>;
  list : TAdvStringMatch;
  ts : TStringList;
  i : integer;
  }
begin
{  vars := TDictionary<String, String>.create;
  try
    vs := '';

    ts := TStringList.Create;
    list := FServer.GetValueSetList;
    try
      for i := 0 to list.Count - 1 do
        ts.AddObject(list.ValueByIndex[i], TObject(i));
      ts.sort;
      for i := 0 to ts.Count - 1 do
        vs := vs + ' <option value="'+list.KeyByIndex[Integer(ts.Objects[i])]+'">'+ts[i]+'</option>';
    finally
      list.Free;
      ts.Free;
    end;

    vars.Add('vslist', vs);
    FReturnProcessFileEvent(response, session, '/tx/form', IncludeTrailingPathDelimiter(FWebDir)+'txform.html', secure, vars);
  finally
    vars.free;
  end;}
end;

procedure TTerminologyWebServer.HandleTxRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
begin
  if request.Document = '/tx/valuesets' then
    ProcessValueSetList(AContext, request, response, session)
  else if request.Document = '/tx/maps' then
    ProcessConceptMapList(AContext, request, response, session)
  else if request.Document = '/tx/codesystems' then
    ProcessCodeSystemList(AContext, request, response, session)
  else if request.Document = '/tx/codesystemproviders' then
    ProcessCodeSystemProviderList(AContext, request, response, session)
  else if request.Document.StartsWith('/tx/valuesets/') then
    ProcessValueSet(AContext, request, response, session)
  else if request.Document.StartsWith('/tx/codesystems/') then
    ProcessCodeSystem(AContext, request, response, session)
  else if request.Document.StartsWith('/tx/maps/') then
    ProcessConceptMap(AContext, request, response, session)
  else
    ProcessHome(AContext, request, response, session);
end;

function TTerminologyWebServer.paramsAsHtml(p: TFhirParameters): String;
var
  html : THtmlPublisher;
  pp : TFhirParametersParameter;
begin
  html := THtmlPublisher.Create;
  try
    html.Version := SERVER_VERSION;
    html.StartTable(true);
    for pp in p.parameterList do
    begin
      html.StartTableRow;
      html.AddTableCell(pp.name);
      html.AddTableCell(gen(pp.value));
      html.EndTableRow;
    end;
    html.EndTable;
    result := html.output;
  finally
    html.Free;
  end;

end;

function TTerminologyWebServer.asHtml(r: TFHIRDomainResource): String;
begin
  if (r.text <> nil) then
    result := TFHIRXhtmlParser.Compose(r.text.div_)
  else
    result := '<i>(no narrative)</i>';
end;

function TTerminologyWebServer.asJson(r: TFHIRResource): String;
var
  json : TFHIRJsonComposer;
  b : TBytesStream;
begin
  b := TBytesStream.Create();
  try
    json := TFHIRJsonComposer.Create(FWorker.link, OutputStylePretty, 'en');
    try
      json.Compose(b, r);
    finally
      json.Free;
    end;
    result := FormatTextToXml(TEncoding.UTF8.GetString(b.Bytes, 0, b.size), xmlText);
  finally
    b.Free;
  end;
end;

function TTerminologyWebServer.asXml(r: TFHIRResource): String;
var
  xml : TFHIRXmlComposer;
  b : TBytesStream;
begin
  b := TBytesStream.Create();
  try
    xml := TFHIRXmlComposer.Create(FWorker.link, OutputStylePretty, 'en');
    try
      xml.Compose(b, r);
    finally
      xml.Free;
    end;
    result := FormatTextToXml(TEncoding.UTF8.GetString(b.Bytes, 0, b.size), xmlText);
  finally
    b.Free;
  end;
end;

//Procedure TTerminologyWebServer.BuildCsByName(html : THtmlPublisher; id : String);
//{var
//  list : TAdvStringMatch;
//  ts : TStringList;
//  i: Integer;}
//begin
//{  logt('Tx: CS By Name '+Id);
//  ts := TStringList.Create;
//  list := FServer.GetCodeSystemList;
//  try
//    html.Header('Terminology Server');
//    html.Heading(2, 'CodeSystems');
//    html.StartList;
//
//    for i := 0 to list.Count - 1 do
//      ts.AddObject(list.ValueByIndex[i], TObject(i));
//    ts.sort;
//    for i := 0 to ts.Count - 1 do
//    begin
//      html.StartListItem;
//      html.URL(ts[i], 'tx/cs/'+list.KeyByIndex[Integer(ts.Objects[i])]);
//      html.EndListItem;
//    end;
//  finally
//    list.Free;
//    ts.Free;
//  end;
//  html.EndList; }
//end;
//
//Procedure TTerminologyWebServer.BuildCsByURL(html : THtmlPublisher; id : String);
//{var
//  list : TAdvStringMatch;
//  ts : TStringList;
//  i: Integer; }
//begin
//{  logt('Tx: CS By URL '+Id);
//  ts := TStringList.Create;
//  list := FServer.GetCodeSystemList;
//  try
//    html.Header('Terminology Server');
//    html.Heading(2, 'Code Systems (by URL)');
//    html.StartList;
//    for i := 0 to list.Count - 1 do
//      ts.AddObject(list.KeyByIndex[i], TObject(i));
//    ts.sort;
//    for i := 0 to ts.Count - 1 do
//    begin
//      html.StartListItem;
//      html.URL(ts[i], 'tx/cs/'+ts[i]);
//      html.AddTextPlain(': '+list.ValueByIndex[Integer(ts.Objects[i])]);
//      html.EndListItem;
//    end;
//  finally
//    list.Free;
//    ts.Free;
//  end;
//  html.EndList;}
//end;
//
//Procedure TTerminologyWebServer.BuildVsByName(html : THtmlPublisher; id : String);
//{var
//  list : TAdvStringMatch;
//  ts : TStringList;
//  i: Integer;
//  }
//begin
//{  logt('Tx: VS By Name '+Id);
//  ts := TStringList.Create;
//  list := FServer.GetValueSetList;
//  try
//    html.Header('Terminology Server');
//    html.Heading(2, 'Value Sets (By Name)');
//    html.StartList;
//    for i := 0 to list.Count - 1 do
//      ts.AddObject(list.ValueByIndex[i], TObject(i));
//    ts.sort;
//    for i := 0 to ts.Count - 1 do
//    begin
//      html.StartListItem;
//      html.URL(ts[i], 'tx/vs/'+list.KeyByIndex[Integer(ts.Objects[i])]);
//      html.EndListItem;
//    end;
//  finally
//    list.Free;
//    ts.Free;
//  end;
//  html.EndList;}
//end;
//
//Procedure TTerminologyWebServer.BuildVsByURL(html : THtmlPublisher; id : String);
////var
////  list : TAdvStringMatch;
////  ts : TStringList;
////  i: Integer;
////  vs : TFHIRValueSet;
////  xml : TFHIRXmlComposer;
////  s : TStringStream;
//begin
//{  logt('Tx: VS By URL '+Id);
//  html.Header('Terminology Server');
//  html.Heading(2, 'Value Sets (By URL)');
//  if (id <> '') then
//  begin
//    vs := FServer.getValueSetByUrl(id);
//    if (vs.text <> nil) and (vs.text.div_ <> nil) then
//    begin
//      html.writeXhtml(vs.text.div_);
//      html.Line;
//    end;
//    s := TStringStream.Create;
//    xml := TFHIRXmlComposer.Create('en');
//    try
//      xml.Compose(s, vs, true, nil);
//      html.startPre;
//      html.AddTextPlain(s.DataString);
//      html.endPre;
//    finally
//      xml.Free;
//      s.Free;
//    end;
//  end
//  else
//  begin
//    ts := TStringList.Create;
//    list := FServer.GetValueSetList;
//    try
//      html.StartList;
//      for i := 0 to list.Count - 1 do
//        ts.AddObject(list.KeyByIndex[i], TObject(i));
//      ts.sort;
//      for i := 0 to ts.Count - 1 do
//      begin
//        html.StartListItem;
//        html.URL(ts[i], 'vs-uri/'+EncodeMime(ts[i]));
//        html.AddTextPlain(': '+list.ValueByIndex[Integer(ts.Objects[i])]);
//        html.EndListItem;
//      end;
//    finally
//      list.Free;
//      ts.Free;
//    end;
//  end;
//  html.EndList;}
//end;

procedure TTerminologyWebServer.HandleSnomedRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  code : String;
  pub : TSnomedPublisher;
  html : THtmlPublisher;
  analysis : TSnomedAnalysis;
  parts : TArray<String>;
  ss, t : TSnomedServices;
  pm : TParseMap;
  buf : TAdvNameBuffer;
begin
  if request.Document.StartsWith('/snomed/tool/') then // FHIR build process support
  begin
    parts := request.Document.Split(['/']);
    ss := nil;
    for t in FServer.Snomed do
      if t.EditionId = parts[3] then
        ss := t;
    if ss = nil then
    begin
      response.ResponseNo := 404;
      response.ContentText := 'Document '+request.Document+' not found';
      logt('miss: '+request.Document);
    end
    else
    begin
      ss.RecordUse;
      response.ContentType := 'text/xml';
      try
        response.ContentText := processSnomedForTool(ss, parts[4]);
        response.ResponseNo := 200;
      except
        on e : Exception do
        begin
          response.ResponseNo := 500;
          response.ContentText := '<snomed version="'+FServer.DefSnomed.VersionDate+'" type="error" message="'+FormatTextToXml(e.Message, xmlAttribute)+'"/>';
        end;
      end;
    end;
  end
  else if request.Document.StartsWith('/snomed/analysis/')  then
  begin
    FServer.DefSnomed.RecordUse;
    analysis := TSnomedAnalysis.create(FServer.DefSnomed.Link);
    try
      pm := TParseMap.create(request.UnparsedParams);
      try
        buf := analysis.generate(pm);
        try
          response.ContentType := buf.Name;
          response.ContentStream := TBytesStream.Create(buf.AsBytes);
          response.FreeContentStream := true;
        finally
          buf.free;
        end;
      finally
        pm.Free;
      end;
      response.ResponseNo := 200;
    finally
      analysis.free;
    end;
  end
  else if request.Document.StartsWith('/snomed/doco') then
  begin
    response.ContentText := chooseSnomedRelease();
    response.ResponseNo := 200;
  end
  else if request.Document.StartsWith('/snomed/') then
  begin
    parts := request.Document.Split(['/']);
    ss := nil;
    for t in FServer.Snomed do
      if t.EditionId = parts[2] then
        ss := t;
    if ss = nil then
    begin
      response.ResponseNo := 404;
      response.ContentText := 'Document '+request.Document+' not found';
      logt('miss: '+request.Document);
    end
    else
    begin
      ss.RecordUse;
      code := request.UnparsedParams;
      logt('Snomed Doco ('+ss.EditionName+'): '+code);

      try
        html := THtmlPublisher.Create;
        pub := TSnomedPublisher.create(ss, FFHIRPath);
        try
          html.Version := SERVER_VERSION;
          html.BaseURL := '/snomed/'+ss.EditionId+'/';
          html.Lang := request.AcceptLanguage;
          pub.PublishDict(code, '/snomed/'+ss.EditionId+'/', html);
          response.ContentText := html.output;
          response.ResponseNo := 200;
        finally
          html.free;
          pub.free;
        end;
      except
        on e:exception do
        begin
          response.ResponseNo := 500;
          response.ContentText := 'error:'+FormatTextToXml(e.Message, xmlText);
        end;
      end;
    end;
  end
  else
  begin
    response.ResponseNo := 404;
    response.ContentText := 'Document '+request.Document+' not found';
    logt('miss: '+request.Document);
  end;
end;

procedure TTerminologyWebServer.HandleLoincRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  code, lang, country : String;
  pub : TLoincPublisher;
  html : THtmlPublisher;
  mem : TMemoryStream;
  op : TBytes;
  i : integer;
  st : TStringList;
begin
  FServer.Loinc.RecordUse;
  if request.Document.StartsWith('/loinc/doco/') then
  begin
    code := request.UnparsedParams;
    lang := request.Document.Substring(12);
    if ((lang = '') and (code = '')) or ((lang <> '') and not FServer.Loinc.supportsLang(lang)) then
    begin
      st := TStringList.create;
      try
        for i := 0 to FServer.Loinc.Lang.count - 1 do
        begin
          FServer.Loinc.Lang.GetEntry(i, lang, country);
          st.add(lang+'-'+country);
        end;
        st.sort;
        html := THtmlPublisher.Create;
        try
          html.Version := SERVER_VERSION;
          html.BaseURL := '/loinc/doco/';
          html.Lang := lang;
          html.Header('LOINC Languages');
          html.StartList();
          for i := 0 to st.count - 1 do
          begin
            html.StartListItem;
            html.URL(st[i], st[i]);
            html.EndListItem;
          end;
          html.EndList();
          mem := TMemoryStream.Create;
          response.ContentStream := mem;
          response.FreeContentStream := true;
          op := TEncoding.UTF8.GetBytes(html.output);
          mem.Write(op, 0, length(op));
          mem.Position := 0;
          response.ContentType := 'text/html; charset=utf-8';
          response.ResponseNo := 200;
        finally
          html.free;
        end;
      finally
        st.free;
      end;
    end
    else
    begin
      logt('Loinc Doco: '+code);
      try
        html := THtmlPublisher.Create;
        pub := TLoincPublisher.create(FServer.Loinc, FFHIRPath, lang);
        try
          html.Version := SERVER_VERSION;
          html.BaseURL := '/loinc/doco/'+lang;
          html.Lang := Lang;
          pub.PublishDict(code, '/loinc/doco/'+lang, html);
          mem := TMemoryStream.Create;
          response.ContentStream := mem;
          response.FreeContentStream := true;
          op := TEncoding.UTF8.GetBytes(html.output);
          mem.Write(op, 0, length(op));
          mem.Position := 0;
          response.ContentType := 'text/html; charset=utf-8';
          response.ResponseNo := 200;
        finally
          html.free;
          pub.free;
        end;
      except
        on e:exception do
        begin
          response.ResponseNo := 500;
          response.ContentText := 'error:'+FormatTextToXml(e.Message, xmlText);
        end;
      end;
    end;
  end
  else
  begin
    response.ResponseNo := 404;
    response.ContentText := 'Document '+request.Document+' not found';
    logt('miss: '+request.Document);
  end;
end;

function TTerminologyWebServer.processSnomedForTool(ss : TSnomedServices; code : String) : String;
var
  sl : TStringList;
  s : String;
  id : UInt64;
  exp : TSnomedExpression;
begin
  logt('Snomed: '+code);
  if StringIsInteger64(code) then
  begin
    if ss.IsValidConcept(code) then
    begin
      result := '<snomed version="'+ss.VersionDate+'" type="concept" concept="'+code+'" display="'+FormatTextToXml(ss.GetDisplayName(code, ''), xmlAttribute)+'">';
      sl := TStringList.Create;
      try
        ss.ListDisplayNames(sl, code, '', ALL_DISPLAY_NAMES);
        for s in sl do
          result := result + '<display value="'+FormatTextToXml(s, xmlAttribute)+'"/>';
      finally
        sl.free;
      end;
      result := result + '</snomed>';
    end
    else if ss.IsValidDescription(code, id, s) then
    begin
      result := '<snomed version="'+ss.VersionDate+'" type="description" description="'+code+'" concept="'+inttostr(id)+'" display="'+FormatTextToXml(s, xmlAttribute)+'">';
      sl := TStringList.Create;
      try
        ss.ListDisplayNames(sl, inttostr(id), '', ALL_DISPLAY_NAMES);
        for s in sl do
          result := result + '<display value="'+FormatTextToXml(s, xmlAttribute)+'"/>';
      finally
        sl.free;
      end;
      result := result + '</snomed>';
    end
    else
      result := '<snomed version="'+ss.VersionDate+'" description="Snomed ID '+code+' not known"/>';
  end
  else
  begin
    exp := ss.parseExpression(code);
    try
      result := '<snomed version="'+ss.VersionDate+'" type="expression" expression="'+code+'" expressionMinimal="'+FormatTextToXml(ss.renderExpression(exp, sroMinimal), xmlAttribute)+'" expressionMax="'+
      FormatTextToXml(ss.renderExpression(exp, sroReplaceAll), xmlAttribute)+'" display="'+FormatTextToXml(ss.displayExpression(exp), xmlAttribute)+'" ok="true"/>';
    finally
      exp.Free;
    end;
  end;
end;


function TTerminologyWebServer.processTranslate(pm: TParseMap): String;
var
  res : TFhirParameters;
  vs : TFHIRValueSet;
  coding : TFhirCoding;
begin
  vs := FServer.getValueSetById(pm.GetVar('valueset')); // this is the target
  try
    coding := TFhirCoding.Create;
    try
      coding.system := pm.GetVar('system');
      coding.version := pm.GetVar('version');
      coding.code := pm.GetVar('code');
      try
        res := FServer.translate('en', nil, coding, vs);
        try
          result := paramsAsHtml(res)+#13#10 + '<pre class="json">'+asJson(res)+'</pre>'#13#10+'<pre class="xml">'+asXml(res)+'</pre>';
        finally
          res.Free;
        end;
      except
        on e : Exception do
          result := '<div style="background: salmon">'+e.message+'</div>';
      end;
    finally
      coding.free;
    end;
  finally
    vs.Free;
  end;
end;

function TTerminologyWebServer.processValidate(pm: TParseMap): String;
var
  coding : TFHIRCoding;
  res : TFhirResource;
  vs : TFHIRValueSet;
begin
  vs := FServer.getValueSetById(pm.GetVar('valueset'));
  try
    coding := TFhirCoding.Create;
    try
      coding.system := pm.GetVar('system');
      coding.version := pm.GetVar('version');
      coding.code := pm.GetVar('code');
      coding.display := pm.GetVar('display');
      res := FServer.validate(vs, coding, nil, pm.GetVar('abstract') = '1');
      try
        if res is TFhirOperationOutcome then
          result := '<div style="background: red">'+asHtml(res as TFhirOperationOutcome)+'</div>'#13 +
            #10'<pre class="json">'+asJson(res)+'</pre>'#13#10+'<pre class="xml">'+asXml(res)+'</pre>'
        else
          result := '<div>'+paramsAsHtml(res as TFhirParameters)+'</div>'#13 +
            #10'<pre class="json">'+asJson(res)+'</pre>'#13#10+'<pre class="xml">'+asXml(res)+'</pre>'
      finally
        res.Free;
      end;
    finally
      coding.Free;
    end;
  finally
    vs.Free;
  end;
end;

function TTerminologyWebServer.sortVsByCtxt(pA, pB: Pointer): Integer;
var
  vA, vB : TFhirValueSet;
begin
  vA := TFhirValueSet(pA);
  vB := TFhirValueSet(pB);
  result := CompareStr(vA.context, vb.context);
end;

//function TTerminologyWebServer.sortVsByDefUrl(pA, pB: Pointer): Integer;
//var
//  vA, vB : TFhirValueSet;
//begin
//  vA := TFhirValueSet(pA);
//  vB := TFhirValueSet(pB);
//  result := CompareStr(vA.codeSystem.system, vb.codeSystem.system);
//end;
//
//function TTerminologyWebServer.sortVsByDefVer(pA, pB: Pointer): Integer;
//var
//  vA, vB : TFhirValueSet;
//begin
//  vA := TFhirValueSet(pA);
//  vB := TFhirValueSet(pB);
//  result := CompareStr(vA.codeSystem.version, vb.codeSystem.version);
//end;

function TTerminologyWebServer.sortVsByName(pA, pB: Pointer): Integer;
var
  vA, vB : TFhirValueSet;
begin
  vA := TFhirValueSet(pA);
  vB := TFhirValueSet(pB);
  result := CompareStr(vA.name, vb.name);
end;

function TTerminologyWebServer.sortVsByPub(pA, pB: Pointer): Integer;
var
  vA, vB : TFhirValueSet;
begin
  vA := TFhirValueSet(pA);
  vB := TFhirValueSet(pB);
  result := CompareStr(vA.publisher, vb.publisher);
end;

function TTerminologyWebServer.sortVsBySrc(pA, pB: Pointer): Integer;
var
  vA, vB : TFhirValueSet;
begin
  vA := TFhirValueSet(pA);
  vB := TFhirValueSet(pB);
  result := CompareStr(vA.source, vb.source);
end;

function TTerminologyWebServer.sortVsByUrl(pA, pB: Pointer): Integer;
var
  vA, vB : TFhirValueSet;
begin
  vA := TFhirValueSet(pA);
  vB := TFhirValueSet(pB);
  result := CompareStr(vA.url, vb.url);
end;

function TTerminologyWebServer.sortVsByVer(pA, pB: Pointer): Integer;
var
  vA, vB : TFhirValueSet;
begin
  vA := TFhirValueSet(pA);
  vB := TFhirValueSet(pB);
  result := CompareStr(vA.version, vb.version);
end;

function TTerminologyWebServer.vsSelect(id: String): String;
var
  vs: TFhirValueSet;
  list : TFhirValueSetList;
  s : String;
begin
  list := FServer.GetValueSetList;
  try
    // determine sort order
    list.SortedBy(sortVsByName);
    s := '<select name="valueset" size="1">'#13#10;
    for vs in list do
      if (vs.id = id) then
        s := s + ' <option value="'+vs.id+'" selected>'+vs.name+'</option>'#13#10
      else
        s := s + ' <option value="'+vs.id+'">'+vs.name+'</option>'#13#10;
    s := s + '</select>'#13#10;
    result := s;
  finally
    list.free;
  end;
end;

function TTerminologyWebServer.sortCmByCtxt(pA, pB: Pointer): Integer;
var
  vA, vB : TLoadedConceptMap;
begin
  vA := TLoadedConceptMap(pA);
  vB := TLoadedConceptMap(pB);
  result := CompareStr(vA.Resource.context, vb.Resource.context);
end;

function TTerminologyWebServer.sortCmByName(pA, pB: Pointer): Integer;
var
  vA, vB : TLoadedConceptMap;
begin
  vA := TLoadedConceptMap(pA);
  vB := TLoadedConceptMap(pB);
  result := CompareStr(vA.Resource.name, vb.Resource.name);
end;

function TTerminologyWebServer.sortCmByPub(pA, pB: Pointer): Integer;
var
  vA, vB : TLoadedConceptMap;
begin
  vA := TLoadedConceptMap(pA);
  vB := TLoadedConceptMap(pB);
  result := CompareStr(vA.Resource.publisher, vb.Resource.publisher);
end;

function TTerminologyWebServer.sortCmBySrc(pA, pB: Pointer): Integer;
var
  vA, vB : TLoadedConceptMap;
begin
  vA := TLoadedConceptMap(pA);
  vB := TLoadedConceptMap(pB);
  result := CompareStr(vA.Resource.sourceDesc, vb.Resource.sourceDesc);
end;

function TTerminologyWebServer.sortCmByTgt(pA, pB: Pointer): Integer;
var
  vA, vB : TLoadedConceptMap;
begin
  vA := TLoadedConceptMap(pA);
  vB := TLoadedConceptMap(pB);
  result := CompareStr(vA.Resource.TargetDesc, vb.Resource.TargetDesc);
end;

function TTerminologyWebServer.sortCmByUrl(pA, pB: Pointer): Integer;
var
  vA, vB : TLoadedConceptMap;
begin
  vA := TLoadedConceptMap(pA);
  vB := TLoadedConceptMap(pB);
  result := CompareStr(vA.Resource.url, vb.Resource.url);
end;

function TTerminologyWebServer.sortCmByVer(pA, pB: Pointer): Integer;
var
  vA, vB : TLoadedConceptMap;
begin
  vA := TLoadedConceptMap(pA);
  vB := TLoadedConceptMap(pB);
  result := CompareStr(vA.Resource.version, vb.Resource.version);
end;

function TTerminologyWebServer.sortCsByCtxt(pA, pB: Pointer): Integer;
var
  vA, vB : TFhirCodeSystem;
begin
  vA := TFhirCodeSystem(pA);
  vB := TFhirCodeSystem(pB);
  result := CompareStr(vA.context, vb.context);
end;

function TTerminologyWebServer.sortCsByName(pA, pB: Pointer): Integer;
var
  vA, vB : TFhirCodeSystem;
begin
  vA := TFhirCodeSystem(pA);
  vB := TFhirCodeSystem(pB);
  result := CompareStr(vA.name, vb.name);
end;

function TTerminologyWebServer.sortCsByPub(pA, pB: Pointer): Integer;
var
  vA, vB : TFhirCodeSystem;
begin
  vA := TFhirCodeSystem(pA);
  vB := TFhirCodeSystem(pB);
  result := CompareStr(vA.publisher, vb.publisher);
end;

function TTerminologyWebServer.sortCsByUrl(pA, pB: Pointer): Integer;
var
  vA, vB : TFhirCodeSystem;
begin
  vA := TFhirCodeSystem(pA);
  vB := TFhirCodeSystem(pB);
  result := CompareStr(vA.url, vb.url);
end;

function TTerminologyWebServer.sortCsByVer(pA, pB: Pointer): Integer;
var
  vA, vB : TFhirCodeSystem;
begin
  vA := TFhirCodeSystem(pA);
  vB := TFhirCodeSystem(pB);
  result := CompareStr(vA.version, vb.version);
end;

end.
