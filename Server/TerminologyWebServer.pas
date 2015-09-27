unit TerminologyWebServer;

interface

uses
  SysUtils, Classes, System.Generics.Collections,
  ParseMap,
  EncodeSupport, StringSupport,
  AdvObjects, AdvStringMatches,
  IdContext, IdCustomHTTPServer,
  FHIRLang, FHIRSupport, FHIRUtilities, FHIRResources, FHIRTypes,
  HtmlPublisher, SnomedPublisher, SnomedServices, LoincPublisher, LoincServices, SnomedExpressions, SnomedAnalysis,
  TerminologyServer, TerminologyServices, TerminologyServerStore;

Type
  TReturnProcessFileEvent = procedure (response: TIdHTTPResponseInfo; session : TFhirSession; named, path: String; secure : boolean; variables: TDictionary<String, String>) of Object;

  TTerminologyWebServer = class (TAdvObject)
  private
    FServer : TTerminologyServer;
    FFHIRPath : String;
    FWebDir : String;
    FReturnProcessFileEvent : TReturnProcessFileEvent;
    function asJson(r : TFHIRResource) : String;
    function asXml(r : TFHIRResource) : String;
    function asHtml(r : TFHIRDomainResource) : String;
    function paramsAsHtml(p : TFhirParameters) : String;
    function vsSelect(id : String) : String;

    function processFind(pm : TParseMap) : String;
    function processValidate(pm : TParseMap) : String;
    function processExpand(pm : TParseMap) : String;
    function processTranslate(pm : TParseMap) : String;

    Procedure HandleLoincRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure HandleSnomedRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
    Procedure HandleTxRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
    Procedure HandleTxForm(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; response: TIdHTTPResponseInfo; secure : boolean);
//    Procedure BuildCsByName(html : THtmlPublisher; id : String);
//    Procedure BuildCsByURL(html : THtmlPublisher; id : String);
//    Procedure BuildVsByName(html : THtmlPublisher; id : String);
//    Procedure BuildVsByURL(html : THtmlPublisher; id : String);
    function processSnomedForTool(code : String) : String;

    function sortVsByUrl(pA, pB : Pointer) : Integer;
    function sortVsByVer(pA, pB : Pointer) : Integer;
    function sortVsByName(pA, pB : Pointer) : Integer;
    function sortVsByCtxt(pA, pB : Pointer) : Integer;
    function sortVsByPub(pA, pB : Pointer) : Integer;
    function sortVsBySrc(pA, pB : Pointer) : Integer;
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
    procedure ProcessVsDefinesList(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
    procedure ProcessCodeSystemsList(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
    procedure ProcessValueSet(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
    procedure ProcessConceptMap(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
    procedure ProcessHome(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
  public
    constructor create(server : TTerminologyServer; FHIRPath, webdir : String; ReturnProcessFileEvent : TReturnProcessFileEvent); overload;

    function HandlesRequest(path : String) : boolean;
    Procedure Process(AContext: TIdContext; request: TIdHTTPRequestInfo; session : TFhirSession; response: TIdHTTPResponseInfo; secure : boolean);

  end;

implementation

uses
  SystemService,
  FHIRParser;

{ TTerminologyWebServer }

constructor TTerminologyWebServer.create(server: TTerminologyServer; FHIRPath, WebDir : String; ReturnProcessFileEvent : TReturnProcessFileEvent);
begin
  create;
  FServer := server;
  FFHIRPath := FHIRPath;
  FWebDir := WebDir;
  FReturnProcessFileEvent := ReturnProcessFileEvent;
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
        vars['expand.results'] := processExpand(pm)
      else if pm.getVar('op') = 'translate' then
        vars['translate.results'] := processTranslate(pm);

      FReturnProcessFileEvent(response, session, request.Document, IncludeTrailingPathDelimiter(FWebDir) + 'txhome.html', false, vars);
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
    FReturnProcessFileEvent(response, session, request.Document, IncludeTrailingPathDelimiter(FWebDir) + 'tx-cm-id.html', false, vars);
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
    FReturnProcessFileEvent(response, session, request.Document, IncludeTrailingPathDelimiter(FWebDir) + 'tx-vs-id.html', false, vars);
  finally
    vars.Free;
  end;
end;

procedure TTerminologyWebServer.ProcessCodeSystemsList(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
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
      vars.add('kind', 'Other Code System');
    finally
      html.Free;
    end;
    FReturnProcessFileEvent(response, session, request.Document, IncludeTrailingPathDelimiter(FWebDir) + 'tx-vs.html', false, vars);
  finally
    vars.Free;
  end;
end;

procedure TTerminologyWebServer.ProcessVsDefinesList(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo; session : TFhirSession);
var
  list: TFhirValueSetList;
  vs: TFhirValueSet;
  vars : TDictionary<String, String>;
  html : THtmlPublisher;
begin
  vars := TDictionary<String, String>.create;
  try
  list := FServer.GetCodeSystemList;
  try
    html := THtmlPublisher.Create;
    try
      html.StartTable(true);
      html.StartTableRow;
      html.AddTableCellURL('URL', '/tx/vsdefines?sort=url');
      html.AddTableCellURL('Version', '/tx/vsdefines?sort=ver');
      html.AddTableCellURL('Name', '/tx/vsdefines?sort=name');
      html.AddTableCellURL('Context', '/tx/vsdefines?sort=ctxt');
      html.AddTableCellURL('Publisher', '/tx/vsdefines?sort=pub');
      html.EndTableRow;
      for vs in list do
      begin
        html.StartTableRow;
        html.AddTableCellURL(vs.codeSystem.system, '/tx/valuesets/' + vs.id);
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
  FReturnProcessFileEvent(response, session, request.Document, IncludeTrailingPathDelimiter(FWebDir) + 'tx-vs.html', false, vars);
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
    FReturnProcessFileEvent(response, session, request.Document, IncludeTrailingPathDelimiter(FWebDir) + 'tx-vs.html', false, vars);
  finally
    vars.Free;
  end;
end;

function TTerminologyWebServer.processExpand(pm: TParseMap): String;
var
  res : TFHIRValueSet;
  vs : TFHIRValueSet;
  profile : String;
begin
  vs := FServer.getValueSetById(pm.GetVar('valueset'));
  try
    profile := '';
    if (pm.GetVar('nodetails') = '1') then
      profile := 'http://www.healthintersections.com.au/fhir/expansion/no-details-web';

    try
      res := FServer.expandVS(vs, vs.url, profile, pm.GetVar('filter'), 1000, true);
      try
        result := asHtml(res)+#13#10;
        if (profile <> '') then
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
  end;
end;

function TTerminologyWebServer.processFind(pm: TParseMap): String;
var
  coding : TFHIRCoding;
  res : TFhirResource;
begin
  coding := TFhirCoding.Create;
  try
    coding.system := pm.GetVar('system');
    coding.version := pm.GetVar('version');
    coding.code := pm.GetVar('code');
    res := FServer.lookupCode(coding);
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
    FReturnProcessFileEvent(response, session, request.Document, IncludeTrailingPathDelimiter(FWebDir) + 'tx-vs.html', false, vars);
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
  else if request.Document = '/tx/vsdefines' then
    ProcessVsDefinesList(AContext, request, response, session)
  else if request.Document = '/tx/codesystems' then
    ProcessCodeSystemsList(AContext, request, response, session)
  else if request.Document.StartsWith('/tx/valuesets/') then
    ProcessValueSet(AContext, request, response, session)
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
    result := ComposeXHtml(r.text.div_)
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
    json := TFHIRJsonComposer.Create('en');
    try
      json.Compose(b, r, true);
    finally
      json.Free;
    end;
    result := EncodeXML(TEncoding.UTF8.GetString(b.Bytes, 0, b.size), xmlText);
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
    xml := TFHIRXmlComposer.Create('en');
    try
      xml.Compose(b, r, true);
    finally
      xml.Free;
    end;
    result := EncodeXML(TEncoding.UTF8.GetString(b.Bytes, 0, b.size), xmlText);
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
//{  writelnt('Tx: CS By Name '+Id);
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
//{  writelnt('Tx: CS By URL '+Id);
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
//{  writelnt('Tx: VS By Name '+Id);
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
//{  writelnt('Tx: VS By URL '+Id);
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
begin
  if request.Document.StartsWith('/snomed/tool/') then // FHIR build process support
  begin
    response.ContentType := 'text/xml';
    try
      response.ContentText := processSnomedForTool(request.Document.Substring(13));
      response.ResponseNo := 200;
    except
      on e : Exception do
      begin
        response.ResponseNo := 500;
        response.ContentText := '<snomed version="'+FServer.snomed.VersionDate+'" type="error" message="'+EncodeXML(e.Message, xmlAttribute)+'"/>';
      end;
    end;
  end
  else if request.Document.StartsWith('/snomed/analysis/')  then
  begin
    analysis := TSnomedAnalysis.create(FServer.Snomed.Link);
    try
      response.ContentText := analysis.generate;
      response.ResponseNo := 200;
    finally
      analysis.free;
    end;
  end
  else if request.Document.StartsWith('/snomed/doco/')  then
  begin
    code := request.UnparsedParams;
    writelnt('Snomed Doco: '+code);

    try
      html := THtmlPublisher.Create;
      pub := TSnomedPublisher.create(FServer.Snomed, FFHIRPath);
      try
        html.BaseURL := '/snomed/doco/';
        html.Lang := request.AcceptLanguage;
        pub.PublishDict(code, '/snomed/doco/', html);
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
        response.ContentText := 'error:'+EncodeXML(e.Message, xmlText);
      end;
    end;
  end
  else
  begin
    response.ResponseNo := 404;
    response.ContentText := 'Document '+request.Document+' not found';
    writelnt('miss: '+request.Document);
  end;
end;

procedure TTerminologyWebServer.HandleLoincRequest(AContext: TIdContext; request: TIdHTTPRequestInfo; response: TIdHTTPResponseInfo);
var
  code : String;
  pub : TLoincPublisher;
  html : THtmlPublisher;
begin
  if request.Document.StartsWith('/loinc/doco/')  then
  begin
    code := request.UnparsedParams;
    writelnt('Loinc Doco: '+code);

    try
      html := THtmlPublisher.Create;
      pub := TLoincPublisher.create(FServer.Loinc, FFHIRPath);
      try
        html.BaseURL := '/loinc/doco/';
        html.Lang := request.AcceptLanguage;
        pub.PublishDict(code, '/loinc/doco/', html);
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
        response.ContentText := 'error:'+EncodeXML(e.Message, xmlText);
      end;
    end;
  end
  else
  begin
    response.ResponseNo := 404;
    response.ContentText := 'Document '+request.Document+' not found';
    writelnt('miss: '+request.Document);
  end;
end;

function TTerminologyWebServer.processSnomedForTool(code : String) : String;
var
  sl : TStringList;
  s : String;
  id : UInt64;
  exp : TSnomedExpression;
begin
  writelnt('Snomed: '+code);
  if StringIsInteger64(code) then
  begin
    if FServer.Snomed.IsValidConcept(code) then
    begin
      result := '<snomed version="'+FServer.Snomed.VersionDate+'" type="concept" concept="'+code+'" display="'+EncodeXml(FServer.Snomed.GetDisplayName(code, ''), xmlAttribute)+'">';
      sl := TStringList.Create;
      try
        FServer.Snomed.ListDisplayNames(sl, code, '', ALL_DISPLAY_NAMES);
        for s in sl do
          result := result + '<display value="'+EncodeXML(s, xmlAttribute)+'"/>';
      finally
        sl.free;
      end;
      result := result + '</snomed>';
    end
    else if FServer.Snomed.IsValidDescription(code, id, s) then
    begin
      result := '<snomed version="'+FServer.Snomed.VersionDate+'" type="description" description="'+code+'" concept="'+inttostr(id)+'" display="'+EncodeXml(s, xmlAttribute)+'">';
      sl := TStringList.Create;
      try
        FServer.Snomed.ListDisplayNames(sl, inttostr(id), '', ALL_DISPLAY_NAMES);
        for s in sl do
          result := result + '<display value="'+EncodeXML(s, xmlAttribute)+'"/>';
      finally
        sl.free;
      end;
      result := result + '</snomed>';
    end
    else
      result := '<snomed version="'+FServer.Snomed.VersionDate+'" description="Snomed ID '+code+' not known"/>';
  end
  else
  begin
    exp := TSnomedExpressionParser.Parse(FServer.Snomed, code);
    try
      result := '<snomed version="'+FServer.Snomed.VersionDate+'" type="expression" expression="'+code+'" expressionMinimal="'+EncodeXml(TSnomedExpressionParser.Render(FServer.Snomed, exp, sroMinimal), xmlAttribute)+'" expressionMax="'+
      EncodeXml(TSnomedExpressionParser.Render(FServer.Snomed, exp, sroReplaceAll), xmlAttribute)+'" display="'+EncodeXml(TSnomedExpressionParser.Display(FServer.Snomed, exp), xmlAttribute)+'" ok="true"/>';
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
        res := FServer.translate(nil, coding, vs);
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
      res := FServer.validate(vs, coding, pm.GetVar('abstract') = '1');
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

end.
