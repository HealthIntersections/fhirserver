unit SnomedAnalysis;

interface

uses
  SysUtils, Classes, Math,
  StringSupport, EncodeSupport, ParseMap,
  AdvStringBuilders, AdvObjectLists,
  FHIRResources, FHIRTypes, FHIRConstants, FHIRParser,
  SnomedServices, FHIRParserBase,
  AdvObjects, AdvFiles;

type
  TRelationship = class (TAdvObject)
  public
    FRelationship : cardinal;
    FCount : cardinal;
    FCounts : Array of Cardinal;
    FIndCount : cardinal;
    FTopmost : cardinal;
    FDupl : cardinal;
    FMax : Cardinal;
    FTargets : TCardinalArray;
    FTargetCount : integer;
    FBranches : TCardinalArray;
  end;

  TRelationshipList = class (TAdvObjectList)
  private
    function GetEntry(i: integer): TRelationship;
  protected
    function ItemClass : TAdvObjectClass; override;
  public
    Property entry[i : integer] : TRelationship read GetEntry; default;
    function getById(id : cardinal) : TRelationship;
  end;

  TSnomedAnalysis = class (TAdvObject)
  private
    FSnomed : TSnomedServices;
    FRoots : TCardinalArray;
    Handled : TCardinalArray;
//    function CreateCC(index : Cardinal) : TFhirCodeableConcept;
//    function CreateRef(root, index : Cardinal) : TFhirReference;
    procedure listRelationships(iIndex : cardinal; list : TRelationshipList);
    function getRootConcepts(iIndex : cardinal) : TCardinalArray;
//    function intersection(one, two : TCardinalArray) : TCardinalArray;
    procedure registerSCTRoots(ids : Array of String);

    function tref(index : integer): String;
    procedure assess(b : TAdvStringBuilder; id : String);
    function getProps(id, prop : cardinal) : TCardinalArray;
    procedure listChildren(id : cardinal; list : TStringList);
    procedure processTree(b : TAdvStringBuilder; id : string; props : TStringList);
  public
    Constructor Create(snomed : TSnomedServices); overload;
    Destructor Destroy; override;

    function generate(params : TParseMap) : String;
  end;

implementation

{ TSnomedAnalysis }

procedure TSnomedAnalysis.assess(b: TAdvStringBuilder; id: String);
var
  list : TRelationshipList;
  did : UInt64;
  iId : int64;
//  ok : boolean;
  iIndex : cardinal;
  allDesc, Inbounds : TCardinalArray;
  i, j : integer;
//  iId : UInt64;
//  iIndex : Cardinal;
//  Identity : UInt64;
  Flags : Byte;
  Active, Defining : boolean;
  Group : integer;
//  ParentIndex : Cardinal;
//  DescriptionIndex : Cardinal;
//  InboundIndex : Cardinal;
//  outboundIndex : Cardinal;
//  Parents : TCardinalArray;
//  Descriptions : TCardinalArray;
//  Inbounds : TCardinalArray;
//  outbounds : TCardinalArray;
//  allDesc : TCardinalArray;
  iWork, iWork2, iWork3, kind, module, modifier : Cardinal;
//  FSN : String;
//  PN : String;
//  FPaths : TArrayofIdArray;
//  i : integer;
//  iList : TCardinalArray;
//  iDummy, iRefSet, iMembers, iDescs, children : Cardinal;
//  bDescSet : Boolean;
//  aMembers : TSnomedReferenceSetMemberArray;
  date : TSnomedDate;
//  ok : boolean;
//  iRef : Cardinal;
  cid : String;
  ic : integer;
begin
  iId := StrToUInt64Def(id, 0);
  if not FSnomed.Concept.FindConcept(iId, iIndex) then
    raise Exception.Create('not defined: '+id);
  allDesc := FSnomed.Refs.GetReferences(FSnomed.Concept.GetAllDesc(iIndex));
  if (length(allDesc) = 0) then
  begin
    Inbounds := FSnomed.Refs.GetReferences(FSnomed.Concept.GetInbounds(iIndex));
    For i := 0 to length(Inbounds)-1 Do
    begin
      FSnomed.Rel.GetRelationship(Inbounds[i], did, iWork, iWork2, iWork3, module, kind, modifier, date, Active, Defining, Group);
      if FSnomed.GetConceptId(iWork3) = '116680003' then
        raise Exception.Create('Concept '+id+' has no descendants but it does ');
    end;
  end;

  b.AppendLine(' <tr><td colspan="8"><b>');
  b.append('<a href="../'+FSnomed.EditionId+'/?type=snomed&id=');
  b.Append(id);
  b.Append('">');
  b.Append(id);
  b.Append('</a></b> ');
  b.Append(EncodeXML(fsnomed.getDisplay(id, ''), xmlText));
  b.append('.');
  b.Append(inttostr(length(alldesc)));
  b.AppendLine(' rows</td></tr>');

  list := TRelationshipList.Create;
  try
    for i := Low(allDesc) to high(allDesc) do
    begin
      listRelationships(allDesc[i], list);
    end;
    for i := 0 to list.count -1 do
    begin
      cid := FSnomed.GetConceptId(list[i].FRelationship);
      if cid <> '116680003' then
      begin

        b.AppendLine(' <tr>');
        b.Append('  <td><a href="../'+FSnomed.EditionId+'/?type=snomed&id=');
        b.Append(FSnomed.GetConceptId(list[i].FRelationship));
        b.Append('"/>');
        b.Append(FSnomed.GetDisplayName(list[i].FRelationship, 0));
        b.Append('</a></td><td>');
        b.Append(inttostr(list[i].FIndCount));
        b.Append('</td><td>');
        b.Append(inttostr(trunc((list[i].FIndCount / length(alldesc)) * 100)));
        b.Append('</td><td>');
        // b.Append(inttostr(list[i].FCount));
        for ic in list[i].FCounts do
          b.Append(inttostr(ic)+' ');


        b.Append('</td><td>');
        b.Append(inttostr(list[i].FDupl+1));
        b.Append('</td><td><a href="/snomed/'+FSnomed.EditionId+'/?type=snomed&id=');
        b.Append(FSnomed.GetConceptId(list[i].FMax));
        b.Append('"/>');
        b.Append(FSnomed.GetDisplayName(list[i].FMax, 0));
        b.Append('</td><td>');
        b.Append(inttostr(list[i].FTargetCount));
        b.Append('</td><td>');
        for j := 0 to length(list[i].FBranches) - 1 do
        begin
          if (j > 0) then
            b.Append('<br/>');
          b.Append('<a href="../'+FSnomed.EditionId+'/?type=snomed&id=');
          b.Append(FSnomed.GetConceptId(list[i].FBranches[j]));
          b.Append('"/>');
          b.Append(FSnomed.GetDisplayName(list[i].FBranches[j], 0));
          b.Append('</a>');
        end;
        b.Append('</td>');
        b.AppendLine('</tr>');
      end;
    end;
  finally
    list.Free;
  end;
end;

constructor TSnomedAnalysis.Create(snomed: TSnomedServices);
begin
  Create;
  FSnomed := snomed;
end;

//function TSnomedAnalysis.CreateCC(index: Cardinal): TFhirCodeableConcept;
//var
//  c : TFhirCoding;
//begin
//  result := TFhirCodeableConcept.Create;
//  c := result.codingList.Append;
//  c.system := 'http://snomed.info/sct';
//  c.code := FSnomed.GetConceptId(index);
//  c.display := FSnomed.GetDisplayName(index, 0);
//end;
//
//function TSnomedAnalysis.CreateRef(root, index: Cardinal): TFhirReference;
//var
//  rid : String;
//begin
//  result := TFhirReference.Create;
//  result.display := FSnomed.GetDisplayName(index, 0);
//  rid := FSnomed.GetConceptId(root);
//  if (rid = '404684003') or (rid = '78621006') then
//    result.reference := 'ConditionDefinition/'+FSnomed.GetConceptId(index)
//  else if (rid = '123037004')  then
//    result.reference := 'BodySite/'+FSnomed.GetConceptId(index)
//  else if (rid = '410607006')  then
//    result.reference := 'Organism/'+FSnomed.GetConceptId(index)
//  else if (rid = '105590001')  then
//    result.reference := 'Substance/'+FSnomed.GetConceptId(index)
//  else if (rid = '71388002')  then
//    result.reference := 'ProcedureDefinition/'+FSnomed.GetConceptId(index)
//  else
//    result.reference := '??/'+FSnomed.GetConceptId(index)+'/'+rid
//end;
//
destructor TSnomedAnalysis.destroy;
begin
  FSnomed.Free;
  inherited;
end;

function TSnomedAnalysis.generate(params : TParseMap): String;
var
  b : TAdvStringBuilder;
  st : TStringList;
  s : String;
begin
  b  := TAdvStringBuilder.Create;
  try
    b.appendLine('<?xml version="1.0" encoding="UTF-8"?>');
    b.appendLine('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"');
    b.appendLine('       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">');
    b.appendLine('');
    b.appendLine('<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">');
    b.appendLine('<head>');
    b.appendLine('    <title>116680003: Is a (attribute)FHIR Server</title>');
    b.appendLine('  <meta charset="utf-8"/>');
    b.appendLine('  <meta content="width=device-width, initial-scale=1.0" name="viewport"/>');
    b.appendLine('  <meta content="http://hl7.org/fhir" name="author"/>');
    b.appendLine('');
    b.appendLine('  <link rel="stylesheet" href="/fhir.css"/>');
    b.appendLine('');
    b.appendLine('');
    b.appendLine('    <!-- Bootstrap core CSS -->');
    b.appendLine('  <link rel="stylesheet" href="/dist/css/bootstrap.css"/>');
    b.appendLine('  <link rel="stylesheet" href="/assets/css/bootstrap-fhir.css"/>');
    b.appendLine('');
    b.appendLine('    <!-- Project extras -->');
    b.appendLine('  <link rel="stylesheet" href="/assets/css/project.css"/>');
    b.appendLine('  <link rel="stylesheet" href="/assets/css/pygments-manni.css"/>');
    b.appendLine('');
    b.appendLine('    <!-- FHIR Server stuff -->');
    b.appendLine('  <link rel="stylesheet" href="/css/tags.css"/>');
    b.appendLine('');
    b.appendLine('    <!-- HTML5 shim and Respond.js IE8 support of HTML5 elements and media queries -->');
    b.appendLine('    <!-- [if lt IE 9]>');
    b.appendLine('  <script src="/assets/js/html5shiv.js"></script>');
    b.appendLine('  <script src="/assets/js/respond.min.js"></script>');
    b.appendLine('  <![endif] -->');
    b.appendLine('');
    b.appendLine('    <!-- Favicons -->');
    b.appendLine('  <link sizes="144x144" rel="apple-touch-icon-precomposed" href="/assets/ico/apple-touch-icon-144-precomposed.png"/>');
    b.appendLine('  <link sizes="114x114" rel="apple-touch-icon-precomposed" href="/assets/ico/apple-touch-icon-114-precomposed.png"/>');
    b.appendLine('  <link sizes="72x72" rel="apple-touch-icon-precomposed" href="/assets/ico/apple-touch-icon-72-precomposed.png"/>');
    b.appendLine('  <link rel="apple-touch-icon-precomposed" href="/assets/ico/apple-touch-icon-57-precomposed.png"/>');
    b.appendLine('  <link rel="shortcut icon" href="/assets/ico/favicon.png"/>');
    b.appendLine('<script type="text/javascript" src="/js/json2.js"></script>');
    b.appendLine('<script type="text/javascript" src="/js/statuspage.js"></script>');
    b.appendLine('<script type="text/javascript" src="/js/jquery-1.6.2.min.js"></script>');
    b.appendLine('<script type="text/javascript" src="/js/jquery-ui-1.8.16.custom.min.js"></script>');
    b.appendLine('<link rel="stylesheet" href="/css/jquery.ui.all.css">');
    b.appendLine('<script src="/js/jquery-1.6.2.js"></script>');
    b.appendLine('<script src="/js/jquery.ui.core.js"></script>');
    b.appendLine('<script src="/js/jquery.ui.widget.js"></script>');
    b.appendLine('<script src="/js/jquery.ui.mouse.js"></script>');
    b.appendLine('<script src="/js/jquery.ui.resizable.js"></script>');
    b.appendLine('<script src="/js/jquery.ui.draggable.js"></script>');
    b.appendLine('<script type="text/javascript" src="/js/jtip.js"></script>');
    b.appendLine('<script type="text/javascript" src="/js/jcookie.js"></script>');
    b.appendLine('<script type="text/javascript" src="/js/hl7connect.js"></script>');
    b.appendLine('<script type="text/javascript" src="/js/fhir-gw.js"></script>');
    b.appendLine('</head>');
    b.appendLine('');
    b.appendLine('<body>');
    b.appendLine('	<div id="segment-navbar" class="segment">  <!-- segment-breadcrumb -->');
    b.appendLine('		<div id="stripe"> </div>');
    b.appendLine('		<div class="container">  <!-- container -->');
    b.appendLine('		<div style="background-color: #ad1f2f; padding: 6px; color: white;">  <!-- container -->');
    b.appendLine('  <a href="http://www.hl7.org/fhir" style="color: gold" title="Fast Healthcare Interoperability Resources - Home Page"><img border="0" src="/icon-fhir-16.png" style="vertical-align: text-bottom"/> <b>FHIR</b></a>');
    b.appendLine('');
    b.appendLine('  &copy; HL7.org');
    b.appendLine('  &nbsp;|&nbsp;');
    b.appendLine('  <a href="/" style="color: gold">Server Home</a>   &nbsp;|&nbsp;');
    b.appendLine('  <a href="http://www.healthintersections.com.au" style="color: gold">Health Intersections</a> FHIR Server');
    b.appendLine('  &nbsp;|&nbsp;');
    b.appendLine('  <a href="'+FHIR_SPEC_URL+'" style="color: gold">FHIR Version 0.5.0-5264</a>');
    b.appendLine('  &nbsp;');
    b.appendLine('		</div>  <!-- /container -->');
    b.appendLine('		</div>  <!-- /container -->');
    b.appendLine('</div>');
    b.appendLine('');
    b.appendLine('	<!-- /segment-breadcrumb -->');
    b.appendLine('');
    b.appendLine('	<div id="segment-content" class="segment">  <!-- segment-content -->');
    b.appendLine('	<div class="container">  <!-- container -->');
    b.appendLine('            <div class="row">');
    b.appendLine('            	<div class="inner-wrapper">');
    b.appendLine(' <div id="div-cnt" class="col-9">');
    b.appendLine('');
    b.appendLine('');


    if params.VarExists('base') then
    begin
      st := TStringList.Create;
      try
        st.CommaText := params.GetVar('props');
        b.AppendLine('<form method="GET">');
        b.AppendLine('<p>Regenerate: </p><p>');
        b.AppendLine(' Base Concept: <input type="text" name="base" value="'+params.GetVar('base')+'"> (concept id)</br>');
        b.AppendLine(' Properties: <input type="text" name="props" value="'+st.commatext+'"> (comma separated list of concept ids)</br>');
        b.AppendLine('<input type="submit" value="build"></br>');
        b.AppendLine('</p></form method="GET">');
        processTree(b, params.GetVar('base'), st);
      finally
        st.Free;
      end;
    end
    else if params.VarExists('scan') then
    begin
      st := TStringList.Create;
      try
        st.CommaText := params.GetVar('scan');
        b.AppendLine('<form method="GET">');
        b.AppendLine('<p>Rescan: </p><p>');
        b.AppendLine(' Candidate Concepts: <input type="text" name="scan" value="'+st.commatext+'"> (comma separated list of concept ids)</br>');
        b.AppendLine('<input type="submit" value="scan"></br>');
        b.AppendLine('</p></form method="GET">');
        b.AppendLine('<table border="1" cellspacing="1">');
        b.AppendLine(' <tr><td>Relationship</td><td># Concepts</td><td>%</td><td># Rows</td><td>MaxCount</td><td>Concept at Max</td><td># distinct targets</td><td>Common Ancestors</td></tr>');
        registerSCTRoots(['105590001', '123037004', '123038009', '243796009', '254291000', '260787004',
         '272379006', '308916002', '362981000', '363787002', '370115009', '373873005', '404684003', '410607006', '419891008', '48176007',
         '71388002', '78621006', '900000000000441003']);
        for s in st do
          assess(b, s);
      finally
        st.Free;
      end;
      b.AppendLine('</table>');
    end
    else
    begin
      b.AppendLine('<h2>Snomed Table Analysis</h2>');
      b.AppendLine('<form method="GET">');
      b.AppendLine('<p>Scan for table candidates: </p><p>');
      b.AppendLine(' Candidate Concepts: <input type="text" name="scan" value="105590001,123037004,123038009,243796009,254291000,260787004,272379006,308916002,362981000,363787002,370115009,373873005,'+'404684003,410607006,419891008,48176007,71388002,78621006,900000000000441003"> (comma separated list of concept ids)</br>');
      b.AppendLine('<input type="submit" value="scan"></br>');
      b.AppendLine('</p></form method="GET">');
      b.AppendLine('<form method="GET">');
      b.AppendLine('<p>Generate a table: </p><p>');
      b.AppendLine(' Base Concept: <input type="text" name="base" value="123038009"> (concept id)</br>');
      b.AppendLine(' Properties: <input type="text" name="props" value="370133003,118169006,118171006,118170007,118168003,123005000,260686004"> (comma separated list of concept ids)</br>');
      b.AppendLine('<input type="submit" value="build"></br>');
      b.AppendLine('</p></form method="GET">');
    end;

    b.appendLine('</div>');
    b.appendLine('');
    b.appendLine('');
    b.appendLine('				</div>  <!-- /inner-wrapper -->');
    b.appendLine('            </div>  <!-- /row -->');
    b.appendLine('        </div>  <!-- /container -->');
    b.appendLine('    </div>  <!-- /segment-content -->');
    b.appendLine('');
    b.appendLine('');
    b.appendLine('	<div id="segment-footer" class="segment">  <!-- segment-footer -->');
    b.appendLine('		<div class="container">  <!-- container -->');
    b.appendLine('			<div class="inner-wrapper">');
    b.appendLine('				<p>');
    b.appendLine('        <a href="/snomed/'+FSnomed.EditionId+'/" style="color: gold">Server Home</a>.&nbsp;|&nbsp;FHIR &copy; HL7.org 2011+. &nbsp;|&nbsp; FHIR Version <a href="'+FHIR_SPEC_URL+'" style="color: gold">0.5.0-5264</a>');
    b.appendLine('        </span>');
    b.appendLine('        </p>');
    b.appendLine('			</div>  <!-- /inner-wrapper -->');
    b.appendLine('		</div>  <!-- /container -->');
    b.appendLine('	</div>  <!-- /segment-footer -->');
    b.appendLine('');
    b.appendLine('');
    b.appendLine('	<div id="segment-post-footer" class="segment hidden">  <!-- segment-post-footer -->');
    b.appendLine('		<div class="container">  <!-- container -->');
    b.appendLine('		</div>  <!-- /container -->');
    b.appendLine('	</div>  <!-- /segment-post-footer -->');
    b.appendLine('');
    b.appendLine('');
    b.appendLine('');
    b.appendLine('</body>');
    b.appendLine('</html>');

    result := b.AsString;
  finally
    b.Free;
  end;
end;

//function TSnomedAnalysis.intersection(one, two: TCardinalArray): TCardinalArray;
//var
//  i, j, c : integer;
//  ok : boolean;
//begin
//  c := 0;
//  setLength(result, length(one) + length(two));
//  for i := 0 to Length(one)-1 do
//  begin
//    ok := false;
//    for j := 0 to Length(two) - 1 do
//      if two[j] = one[i] then
//        ok := true;
//    if ok then
//    begin
//      result[c] := one[i];
//      inc(c);
//    end;
//  end;
//  for i := 0 to Length(two)-1 do
//  begin
//    ok := false;
//    for j := 0 to Length(one) - 1 do
//      if one[j] = two[i] then
//        ok := true;
//    if ok then
//      for j := 0 to Length(result) - 1 do
//        if result[j] = two[i] then
//          ok := false;
//    if ok then
//    begin
//      result[c] := two[i];
//      inc(c);
//    end;
//  end;
//  SetLength(result, c);
//end;
//
function TSnomedAnalysis.getProps(id, prop: cardinal): TCardinalArray;
var
  Identity : UInt64;
  Flags : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  refsets, c : Cardinal;
  date : word;
  outbounds : TCardinalArray;
  Source, Target, RelType, module, kind, modifier : Cardinal;
  Active, Defining : Boolean;
  Group : Integer;
begin
  FSnomed.Concept.GetConcept(id, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
  SetLength(result, 0);
  if outboundIndex > 0 then
  begin
    outbounds := FSnomed.Refs.GetReferences(outboundIndex);
    for c in outbounds do
    begin
      FSnomed.Rel.GetRelationship(c, identity, Source, Target, RelType, module, kind, modifier, date, Active, Defining, Group);
      if {(group = 0) and }active and (RelType = prop) then
      begin
        SetLength(result, length(result)+1);
        result[length(result)-1] := target;
      end;
    end;
  end;

end;

function TSnomedAnalysis.getRootConcepts(iIndex : cardinal) : TCardinalArray;
var
  c, i, j, k, l : integer;
  parents : TCardinalArray;
  ok : boolean;
  queue : TCardinalArray;
  new : boolean;
begin
  SetLength(result, 0);
  SetLength(queue, 10);
  queue[0] := iIndex;
  c := 1;
  i := 0;
  while i < c do
  begin
    parents := FSnomed.Refs.GetReferences(FSnomed.Concept.GetParent(queue[i]));
    for j := 0 to Length(parents) - 1 do
    begin
      for k := 0 to Length(FRoots) - 1 do
      begin
        if Froots[k] = parents[j] then
        begin
          new := true;
          for l := 0 to Length(result) -1 do
            new := new and (result[l] <> parents[j]);
          if new then
          begin
            SetLength(result, length(result)+1);
            result[length(result)-1] := parents[j];
          end;
        end;
      end;
      ok := true;
      for k := 0 to c-1 do
        if queue[k] = parents[j] then
          ok := false;
      if ok then
      begin
        if c >= length(queue) then
          SetLength(queue, length(queue)+10);
        queue[c] := parents[j];
        inc(c);
      end;
    end;
    inc(i);
  end;
  SetLength(queue, c);
end;

procedure TSnomedAnalysis.listRelationships(iIndex: cardinal; list : TRelationshipList);
var
//  iId : UInt64;
  did : UInt64;
  Identity : UInt64;
  Flags : Byte;
  Group : integer;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
//  Parents : TCardinalArray;
  Descriptions : TCardinalArray;
//  Inbounds : TCardinalArray;
  outbounds : TCardinalArray;
//  allDesc : TCardinalArray;
  iWork, iWork2, iWork3, kind, module, modifier,
  refsets : Cardinal;
//  FSN : String;
//  PN : String;
//  FPaths : TArrayofIdArray;
  i, j, k : integer;
  c : cardinal;
//  iList : TCardinalArray;
//  iDummy, iRefSet, iMembers, iDescs, children : Cardinal;
//  bDescSet : Boolean;
//  aMembers : TSnomedReferenceSetMemberArray;
  date : TSnomedDate;
//  ok : boolean;
//  iRef : Cardinal;
  rel : TRelationship;
  Rels : TCardinalArray;
  ok : boolean;
  rootConcepts : TCardinalArray;
//  cnd : TFhirConditionDefinition;
  cid : String;
  Active, Defining : boolean;
begin
  FSnomed.Concept.GetConcept(iIndex, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
  Descriptions := FSnomed.Refs.GetReferences(DescriptionIndex);
  outbounds := FSnomed.Refs.GetReferences(outboundIndex);
  SetLength(Rels, length(outbounds));

  cid := FSnomed.GetConceptId(iIndex);
//  if (bnd <> Nil) { and (bnd.entryList.Count < 1000) } then
//  begin
//    cnd := TFhirConditionDefinition.Create;
//    bnd.entryList.Append.resource := cnd;
//    cnd.id := cid;
//    cnd.url := 'http://healthintersections.com.au/sct/'+cid;
//    cnd.name := FSnomed.GetDisplayName(iIndex, 0);
//    cnd.status := ConformanceResourceStatusDraft;
//  end
//  else
//    cnd := nil;

  for i := Low(Outbounds) To High(Outbounds) Do
  begin
    FSnomed.Rel.GetRelationship(Outbounds[i], did, iWork, iWork2, iWork3, module, kind, modifier, date, Active, Defining, Group);
    if Active and (group = 0) then
    begin
      rootConcepts := getRootConcepts(iWork2);
      rel := list.getById(iwork3);
      if (rel = nil) then
      begin
        rel := TRelationship.Create;
        list.Add(rel);
        rel.FRelationship := iWork3;
      end;
      inc(rel.FCount);

      ok := true;
      for j := 0 to rel.FTargetCount - 1 do
        ok := ok and (rel.FTargets[j] <> iWork2);
      if ok then
      begin
        if (length(rel.FTargets) = rel.FTargetCount) then
          setLength(rel.FTargets, length(rel.FTargets)+100);
        rel.FTargets[rel.FTargetCount] := iwork2;
        inc(rel.FTargetCount);
      end;

      for k := 0 to length(rootConcepts) - 1 do
      begin
        ok := true;
        for j := 0 to length(rel.FBranches) - 1 do
          ok := ok and (rel.FBranches[j] <> rootConcepts[k]);
        if ok then
        begin
          setLength(rel.FBranches, length(rel.FBranches)+1);
          rel.FBranches[length(rel.FBranches)-1] := rootConcepts[k];
        end;
      end;

      c := 0;
      rels[i] := iWork3;
      for j := 0 to i-1 do
        if (rels[j] = rels[i]) then
          inc(c);
      if (c = 0) then
        inc(rel.FIndCount);
      if Length(rel.FCounts) < c+1 then
        SetLength(rel.FCounts, c+1);
      rel.FCounts[c] := rel.FCounts[c] + 1;
      if c > rel.FDupl then
      begin
        rel.FDupl := c;
        rel.FMax := iIndex;
      end;
    end;
  end;
end;


procedure TSnomedAnalysis.listChildren(id: cardinal; list : TStringList);
var
  children : TCardinalArray;
  i : integer;
  c : cardinal;
  found : boolean;
begin
  children := FSnomed.GetConceptChildren(id);
  for c in children do
  begin
    found := false;
    for i := 0 to list.Count - 1 do
      if Cardinal(list.Objects[i]) = c then
      begin
        found := true;
        break;
      end;
    if not found then
    begin
      list.AddObject(FSnomed.GetDisplayName(c, 0), TObject(c));
      listChildren(c, list);
    end;
  end;
end;

procedure TSnomedAnalysis.processTree(b: TAdvStringBuilder; id: string; props: TStringList);
var
  index, c, iid, d : cardinal;
  propCs : array of cardinal;
  p : TCardinalArray;
  i : integer;
  children : TStringList;
begin
  FSnomed.Concept.FindConcept(StrToUInt64(id), index);
  SetLength(propCs, props.Count);
  for I := 0 to props.Count - 1 do
  begin
    FSnomed.Concept.FindConcept(StrToUInt64(props[i]), c);
    propCs[i] := c;
  end;

  b.Append('<table class="grid">'+#13#10);
  b.Append(' <tr><td>'+tref(index)+'</td>');
  for c in propCs do
    b.Append('<td>'+tref(c)+'</td>');
  b.Append('</tr>'+#13#10);
  children := TStringList.create;
  try
    listChildren(index, children);
    children.sort;
    for I := 0 to children.count - 1 do
    begin
      iid := cardinal(children.objects[i]);
      b.Append(' <tr><td>'+tref(iid)+'</td>');
      for c in propCs do
      begin
        b.Append('<td>');
        p := getProps(iid, c);
        if (length(p) <= 1) then
        begin
          for d in p do
            b.Append(tref(d)+' ');
        end
        else
        begin
          b.Append('<ul>');
          for d in p do
            b.Append('<li>'+tref(d)+'</li>');
          b.Append('</ul>');
        end;
        b.Append('</td>');
      end;
      b.Append('</tr>'+#13#10);
    end;
  finally
    children.free;
  end;
  b.Append('</table>'+#13#10);
end;

procedure TSnomedAnalysis.registerSCTRoots(ids: array of String);
var
  i : integer;
  iIndex : cardinal;
  iId : Int64;
begin
  SetLength(FRoots, length(ids));
  for i := 0 to length(ids)-1 do
  begin
    iId := StrToUInt64Def(ids[i], 0);
    if not FSnomed.Concept.FindConcept(iId, iIndex) then
      raise Exception.Create('not defined: '+ids[i]);
    FRoots[i] := iIndex;
  end;
end;

function TSnomedAnalysis.tref(index: integer): String;
var
  id, text : String;
begin
  id := FSnomed.GetConceptId(index);
  text := FSnomed.GetDisplayName(index, 0);
  result := '<a href="/snomed/'+FSnomed.EditionId+'/?type=snomed&id='+id+'">'+text+'</a>';
end;

//Procedure TSnomedPublisher.CellConceptRef(html : THtmlPublisher; const sPrefix : String; iIndex : cardinal; bShowId : Boolean; iDesc : Cardinal = 0);
//var
//  s : String;
//Begin
//  if iDesc <> 0 Then
//    s := FSnomed.Strings.GetEntry(iDesc)
//  Else
//    s := GetPNForConcept(iIndex);
//
//  if bShowId Then
//    html.AddTableCellURL(inttostr(FSnomed.Concept.GetIdentity(iIndex))+' '+Screen(s, ''), sPrefix+'id='+inttostr(FSnomed.Concept.GetIdentity(iIndex)))
//  else
//    html.AddTableCellURL(Screen(s, ''), sPrefix+'id='+inttostr(FSnomed.Concept.GetIdentity(iIndex)));
//End;
//
//
//  !
//end;

{ TRelationshipList }

function TRelationshipList.getById(id: cardinal): TRelationship;
var
  i : integer;
Begin
  Result := Nil;
  for i := 0 to Count -1 do
    if entry[i].FRelationship = id then
    begin
      result := entry[i];
      exit;
    end;
end;

function TRelationshipList.GetEntry(i: integer): TRelationship;
begin
  result := TRelationship(ObjectByIndex[i]);
end;

function TRelationshipList.ItemClass: TAdvObjectClass;
begin
  result := TRelationship;
end;

end.
