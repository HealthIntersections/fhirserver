unit ftx_loinc_publisher;

{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$I fhir.inc}

Interface

Uses
  SysUtils, Classes, Math, fsl_threads,
  fsl_base, fsl_collections, fsl_http, fsl_htmlgen,
  fdb_manager,
  fhir_objects,
  ftx_loinc_services, ftx_service;

Const
  MAX_ROWS = 50;
  CODES_TLoincProviderContextKind : array [TLoincProviderContextKind] of String = ('Code', 'Part Code', 'Answer List', 'Answer List Item');

Type
  TloincPublisher = class (TFslObject)
  Private
    Lock : TFslLock;
    FSearchCache : TStringList;
    FLoinc : TLOINCServices;
    FFHIRPath : String;

    procedure loincFooter(html : THTMLPublisher);
    Procedure PublishCode(Const sPrefix, sCode : String; html : THTMLPublisher);
    Procedure PublishHome(Const sPrefix : String; html : THTMLPublisher);
    Procedure PublishSearch(Const sPrefix, sText : String; iStart: Integer; all : boolean; html : THTMLPublisher);

    Procedure ProcessMap(Const sPath : String; oMap : TFslStringMatch);
    Procedure PublishDictInternal(oMap : TFslStringMatch; Const sPrefix : String; html : THTMLPublisher);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  Public
    constructor Create(oLoinc : TLoincServices; FHIRPathEngine : String; langList : THTTPLanguageList);
    destructor Destroy; Override;
    Procedure PublishDict(Const sPath, sPrefix : String; html : THTMLPublisher); Overload; Virtual;
    Procedure PublishDict(oMap : TFslStringMatch; Const sPrefix : String; html : THTMLPublisher); Overload; Virtual;
  End;


Implementation

Uses
  fsl_utilities;

function StringToBoolDef(s : String; def : boolean):boolean;
begin
  s := lowercase(s);
  if s = 'true' then
    result := true
  else if s = '1' then
    result := true
  else if s = 'yes' then
    result := true
  else if s = 'y' then
    result := true
  else if s = 'false' then
    result := false
  else if s = '0' then
    result := false
  else if s = 'no' then
    result := false
  else if s = 'n' then
    result := false
  else
    result := def;
end;


Procedure TloincPublisher.PublishDictInternal(oMap : TFslStringMatch; Const sPrefix : String; html : THTMLPublisher);
Var
  sURL : String;
  sId : String;
Begin
  sURL := sPrefix +'?type=loinc&';
  sId := oMap.Matches['id'];
  If sId <> '' Then
    PublishCode(sURL, sId, html)
  else if oMap.ExistsByKey('srch') then
    if FLoinc.Codes.containsKey(oMap.Matches['srch']) then
      PublishCode(sURL, oMap.Matches['srch'], html)
    else
      PublishSearch(sURL, oMap.Matches['srch'], StrToIntDef(oMap.Matches['start'], 0), StringToBoolDef(oMap.matches['all'], false), html)
  else
  begin
    sId := oMap.Matches['code'];
    If sId = '' Then
      PublishHome(sURL, html)
    else
      PublishCode(sURL, sId, html);
  end;
End;

Procedure TloincPublisher.ProcessMap(Const sPath : String; oMap : TFslStringMatch);
Var
  sLeft, sRight : String;
  sName, sValue : String;
Begin
  if sPath.Contains('?') then
    Stringsplit(sPath, '?', sLeft, sRight)
  else
    sRight := sPath;
  oMap.Forced := True;
  While sRight <> '' Do
  Begin
    StringSplit(sRight, '&', sLeft, sRight);
    StringSplit(sLeft, '=', sName, sValue);
    oMap.Matches[sName] := sValue;
  End;
End;


Procedure TloincPublisher.PublishDict(Const sPath, sPrefix : String; html : THTMLPublisher);
Var
  oMap : TFslStringMatch;
Begin
  oMap := TFslStringMatch.Create;
  Try
    ProcessMap(sPath, oMap);
    PublishDict(oMap, sPrefix, html);
  Finally
    oMap.free;
  End;
End;



procedure TloincPublisher.PublishDict(oMap: TFslStringMatch; const sPrefix: String; html: THTMLPublisher);
begin
  Try
    PublishDictInternal(oMap, sPrefix, html);
  Except
    On e:Exception Do
      Begin
      html.AddParagraph(e.Message);
      End;
  End;
end;

procedure TloincPublisher.PublishHome(const sPrefix: String; html: THTMLPublisher);
var
  i : Integer;
Begin
  html.Heading(1, 'LOINC Definitions');
  html.StartForm('GET', sPrefix);
  html.StartParagraph;
  html.AddTextPlain('Search LOINC: ');
  html.textInput('srch');
  html.submit('Go');
  html.AddTextPlain(' ');
  html.checkbox('all', false, 'Tight');
  html.endForm;
  html.Line;
  html.ParaURL('Browse All Codes', sPrefix+'code=*');
  html.StartParagraph;
  html.AddText('LOINC Axes', true, false);
  html.EndParagraph;
  PublishCode(sPrefix, FLoinc.root, html);
  html.AddParagraph;

  //html.AddText('LOINC Heirarchy', true, false);
  //html.EndParagraph;
  //PublishHeirarchyRoot(sPrefix, html);
  //html.AddParagraph;

  html.Line;
  Lock.Lock('PublishHome');
  Try
    if FSearchCache.Count <> 0 Then
    Begin
      html.AddParagraph('Past Searches:');
      html.StartList;
      For i := 0 to FSearchCache.Count - 1 Do
      begin
        html.StartListItem;
        html.URL('Search for "'+FSearchCache[i]+'"', sPrefix+'srch='+FSearchCache[i]+'&caption=Search LOINC Codes&prompt=Text');
        html.EndListItem;
      end;
      html.EndList;
    End;
  Finally
    Lock.UnLock;
  End;
  loincFooter(html);
  html.Done;
End;


procedure TloincPublisher.loincFooter(html: THTMLPublisher);
begin
  html.StartParagraph;
  html.AddTextPlain('LOINC Version: ');
  html.AddTextPlain(FLoinc.version);
  html.EndParagraph;
end;

function codeKind(kind : TLoincProviderContextKind) : String;
begin
  case kind of
    lpckCode: result := 'Code';
    lpckPart: result := 'Part Code';
    lpckList : result := 'Answer List';
    lpckAnswer : result := 'Answer Code';
  else
    result := '??';
  end;
end;

procedure TloincPublisher.PublishCode(const sPrefix, sCode: String; html: THTMLPublisher);
var
  ci : TLoincProviderContext;
  c : TFDBConnection;
begin
  if not FLoinc.Codes.TryGetValue(sCode, ci) then
     html.AddParagraph('Unknown LOINC Code '+sCode)
  else
  begin
    html.Heading(1, 'LOINC '+codeKind(ci.kind)+' '+sCode+' : '+ci.desc);
    html.AddParagraph();
    c := FLoinc.DB.getConnection('PublishCode');
    try
      html.Heading(2, 'Properties');
      html.StartTable(true);
      c.select('Select RelationshipTypes.Description as Relationship, Codes.Code, Codes.Description as Value from Relationships, RelationshipTypes, Codes where Relationships.SourceKey = '+inttostr(ci.key)+' and Relationships.RelationshipTypeKey = RelationshipTypes.RelationshipTypeKey and Relationships.TargetKey = Codes.CodeKey');
      while c.fetchNext do
      begin               
        html.StartRow();
        html.AddTableCell(c.colStringByName['Relationship']);
        html.AddTableCell(c.colStringByName['Value']);
        html.AddTableCellURL(c.colStringByName['Code'], sPrefix+'code='+c.colStringByName['Code']);
        html.EndTableRow;
      end;
      c.terminate;

      c.select('Select Description, Value from Properties, PropertyTypes, PropertyValues where CodeKey = '+inttostr(ci.key)+' and Properties.PropertyTypeKey = PropertyTypes.PropertyTypeKey and Properties.PropertyValueKey = PropertyValues.PropertyValueKey');
      while c.fetchNext do
      begin
        html.StartRow();
        html.AddTableCell(c.colStringByName['Description']);
        html.AddTableCell(c.colStringByName['Value']);
        html.AddTableCell('');
        html.EndTableRow;
      end;
      c.terminate;

      c.select('Select StatusKey from Codes where CodeKey = '+inttostr(ci.key));
      while c.fetchNext do
      begin
        html.StartRow();
        html.AddTableCell('STATUS');
        html.AddTableCell(FLoinc.StatusCodes[c.colStringByName['StatusKey']]);
        html.AddTableCell('');
        html.EndTableRow;
      end;
      c.terminate;
      html.endTable;
      
      html.Heading(2, 'Displays'); 
      html.StartTable(true);
      html.StartRow();
      case ci.Kind of
        lpckCode: html.AddTableCell('LONG_COMMON_NAME');
        lpckPart: html.AddTableCell('PartDisplayName');
        lpckList: html.AddTableCell('LONG_COMMON_NAME');
        lpckAnswer: html.AddTableCell('LONG_COMMON_NAME');
      end;
      html.AddTableCell('en-US');
      html.AddTableCell(ci.desc);
      html.EndTableRow;

      c.Select('Select Languages.Code as Lang, DescriptionTypes.Description as DType, Value from Descriptions, Languages, DescriptionTypes where CodeKey = '+inttostr(ci.key)+' and Descriptions.DescriptionTypeKey != 4 and Descriptions.DescriptionTypeKey = DescriptionTypes.DescriptionTypeKey and Descriptions.LanguageKey = Languages.LanguageKey');
      while c.fetchNext do
      begin
        html.StartRow();
        html.AddTableCell(c.ColStringByName['DType']);
        html.AddTableCell(c.ColStringByName['Lang']);
        html.AddTableCell(c.ColStringByName['Value']);
        html.EndTableRow;
      end;              
      html.endTable;
      c.terminate;
      c.release;
    except
      on e : Exception do
      begin
        c.error(e);
        raise;
      end;
    end;
  end;
  html.line;
  html.ParaURL('LOINC Home', sPrefix);
  loincFooter(html);
  html.done;
end;

constructor TloincPublisher.Create(oLoinc : TLoincServices; FHIRPathEngine : String; langList : THTTPLanguageList);
begin
  inherited Create;
  Lock := TFslLock.Create('LOINC publisher');
  FSearchCache := TStringList.Create;
  FSearchCache.Sorted := true;
  FLoinc := oLoinc.Link;
  FFHIRPath := FHIRPathEngine;
end;

destructor TloincPublisher.Destroy;
var
  i : integer;
begin
  For i := 0 to FSearchCache.Count - 1 do
    FSearchCache.Objects[i].free;
  FSearchCache.free;
  Lock.free;
  FLoinc.free;
  inherited;
end;

procedure TloincPublisher.PublishSearch(const sPrefix, sText: String; iStart: Integer; all : boolean; html: THTMLPublisher);
var
  ci : TLoincProviderContext;
  c : TFDBConnection;
  k : TKeySet;
begin
  html.Heading(1, 'LOINC search for '+sText);
  html.AddParagraph();
  k := TKeySet.create;
  try
    c := FLoinc.DB.getConnection('PublishSearch');
    try
      html.StartTable(true);
      c.select('SELECT CodeKey FROM TextIndex WHERE Text MATCH '''+SQLWrapString(sText)+''';');
      while c.fetchNext do
      begin
        if k.addKey(c.ColKeyByName['CodeKey']) then
        begin
          ci := FLoinc.CodeList[c.ColKeyByName['CodeKey']];
          html.StartRow();
          html.AddTableCellURL(ci.code, sPrefix+'code='+ci.code);
          html.AddTableCell(CODES_TLoincProviderContextKind[ci.Kind]);
          html.AddTableCell(ci.Desc);
          html.EndTableRow;
        end;
      end;
      c.terminate;
      html.endTable;
      c.release;
    except
      on e : Exception do
      begin
        c.error(e);
        raise;
      end;
    end;
  finally
    k.free;
  end;
  html.line;
  html.ParaURL('LOINC Home', sPrefix);
  loincFooter(html);
  html.done;
//var
//  a : TMatchArray;
//  i : integer;
//  o : TSearchCache;
//  iTotal : Integer;
////  iDummy : Cardinal;
//
//  iDescription, iOtherNames, iStems, iCategories : Cardinal;
//  iEntries : Cardinal;
//  sCode1 : String;
//  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Cardinal;
//  iFlags, ilang : Byte;
//  code, text, parents, children, concepts, descendentConcepts, stems: Cardinal;
//begin
//  Lock.Lock('PublishSearch');
//  Try
//    if FSearchCache.Find(sText, i) Then
//      a := TSearchCache(FSearchCache.Objects[i]).a
//    else
//    Begin
//      a := FLoinc.Search(sText, all);
//      o := TSearchCache.Create;
//      o.a := a;
//      FSearchCache.AddObject(sText, o);
//    End;
//  Finally
//    Lock.Unlock;
//  End;
//  html.Heading(1, 'Search LOINC for '+sText);
//
//  html.StartTable(false);
//  html.StartTableRow;
//  html.AddTableCell('Code');
//  html.AddTableCell('Description');
//  html.AddTableCell('Component');
//  html.AddTableCell('Property');
//  html.AddTableCell('Time Aspect');
//  html.AddTableCell('System');
//  html.AddTableCell('Scale');
//  html.AddTableCell('Method');
//  html.AddTableCell('Class');
//  html.AddTableCell('Rating');
//  html.EndTableRow;
//  iTotal := Length(a)-1;
//
//
//  For i := iStart to Min(iStart+MAX_ROWS, iTotal) Do
//  Begin
//    html.StartTableRow;
//    if a[i].kind = lpckCode then
//    begin
//      FLoinc.CodeList.GetInformation(a[i].index, nil, sCode1, iDescription, iOtherNames, iEntries, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);
//      html.AddTableCellURL(sCode1, sPrefix + 'code='+sCode1);
//      html.AddTableCell(FLoinc.Desc.GetEntry(iDescription, ilang));
//      html.AddTableCell(GetConceptDesc(iComponent));
//      html.AddTableCell(GetConceptDesc(iProperty));
//      html.AddTableCell(GetConceptDesc(iTimeAspect));
//      html.AddTableCell(GetConceptDesc(iSystem));
//      html.AddTableCell(GetConceptDesc(iScale));
//      html.AddTableCell(GetConceptDesc(iMethod));
//      html.AddTableCell(GetConceptDesc(iClass));
//      html.AddTableCell(RealToString(a[i].Priority));
//    end
//    else
//    begin
//      FLoinc.Entries.GetEntry(a[i].index, code, text, parents, children, concepts, descendentConcepts, stems);
//      sCode1 := FLoinc.Desc.GetEntry(code, ilang);
//      html.AddTableCellURL(sCode1, sPrefix + 'macode='+sCode1);
//      html.AddTableCell(FLoinc.Desc.GetEntry(text, ilang));
//      html.StartTableCell(7);
//      //html.AddTextPlain('Heirarchy: ');
//      //if (parent = NO_PARENT) then
//      //  html.AddTextPlain('(root)')
//      //else
//      //  AddParent(parent);
//      html.EndTableCell;
//      html.AddTableCell(RealToString(a[i].Priority - 1000000));
//    end;
//    html.EndTableRow;
//    End;
//  html.EndTable;
//  if (iStart > 0) or (iStart+MAX_ROWS < iTotal) Then
//  Begin
//    html.StartParagraph;
//    if iStart > 0 Then
//    Begin
//      html.URL('Start', sPrefix+'srch='+sText);
//      html.AddTextPlain(' ');
//    End;
//    if iStart > MAX_ROWS Then
//    Begin
//      html.URL('Previous', sPrefix+'srch='+sText+'&start='+inttostr(iStart - MAX_ROWS));
//      html.AddTextPlain(' ');
//    End;
//    html.AddText('Page '+ inttostr((iStart div MAX_ROWS) + 1)+' of '+inttostr(iTotal div MAX_ROWS + 1), true, false);
//    if (iStart+MAX_ROWS < iTotal) And (iStart + MAX_ROWS <  MAX_ROWS * (iTotal div MAX_ROWS)) Then
//    Begin
//      html.AddTextPlain(' ');
//      html.URL('Next', sPrefix+'srch='+sText+'&start='+inttostr(iStart + MAX_ROWS));
//    End;
//    if (iStart+MAX_ROWS < iTotal) Then
//    Begin
//      html.AddTextPlain(' ');
//      html.URL('End', sPrefix+'srch='+sText+'&start='+inttostr(MAX_ROWS * (iTotal div MAX_ROWS)));
//    End;
//    html.EndParagraph;
//  End;
//  html.line;
//  html.ParaURL('LOINC Home', sPrefix);
//  loincFooter(html);
//  html.done;
end;

function TloincPublisher.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FSearchCache.sizeInBytes(magic));
  inc(result, FLoinc.sizeInBytes(magic));
  inc(result, (FFHIRPath.length * sizeof(char)) + 12);
end;

End.

