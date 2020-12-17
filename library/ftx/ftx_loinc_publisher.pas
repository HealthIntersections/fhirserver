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
  ftx_loinc_services;

Const
  MAX_ROWS = 50;

Type
  TloincPublisher = class (TFslObject)
  Private
    Lock : TFslLock;
    FSearchCache : TStringList;
    FLoinc : TLOINCServices;
    FFHIRPath : String;
    langs : TLangArray;

    function useLang(lang : byte) : boolean;
    function GetConceptDesc(iConcept : cardinal):String;

    function makeParentsLists(parents : Cardinal) : TCardinalArrayArray;

    procedure loincFooter(html : THTMLPublisher);
    Procedure PublishCode(Const sPrefix, sCode : String; html : THTMLPublisher);
    Procedure PublishCodes(Const sPrefix : String; iStart : Integer; html : THTMLPublisher);
    Procedure PublishConcept(bRoot : Boolean; Const sPrefix, sId : String; iStart : Integer; html : THTMLPublisher);
    Procedure PublishHome(Const sPrefix : String; html : THTMLPublisher);
    Procedure PublishSearch(Const sPrefix, sText : String; iStart: Integer; all : boolean; html : THTMLPublisher);
    Procedure PublishHeirarchyRoot(Const sPrefix : String; html : THTMLPublisher);
    Procedure PublishHeirarchyEntry(sCode : String; iStart : Integer; Const sPrefix : String; html : THTMLPublisher);


    Procedure ProcessMap(Const sPath : String; oMap : TFslStringMatch);
    Procedure PublishDictInternal(oMap : TFslStringMatch; Const sPrefix : String; html : THTMLPublisher);
    function descLength(i: cardinal): String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(oLoinc : TLoincServices; FHIRPathEngine : String; const lang : THTTPLanguages);
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
    PublishConcept(false, sURL, sId, StrToIntDef(oMap.Matches['start'], 0), html)
  else if oMap.ExistsByKey('srch') then
    if FLoinc.IsCode(oMap.Matches['srch']) then
      PublishCode(sURL, oMap.Matches['srch'], html)
    else if FLoinc.IsMACode(oMap.Matches['srch']) then
      PublishHeirarchyEntry(oMap.Matches['srch'], StrToIntDef(oMap.Matches['start'], 0), sURL, html)
    else
      PublishSearch(sURL, oMap.Matches['srch'], StrToIntDef(oMap.Matches['start'], 0), StringToBoolDef(oMap.matches['all'], false), html)
  else if oMap.ExistsByKey('macode') then
    PublishHeirarchyEntry(oMap.Matches['macode'], StrToIntDef(oMap.Matches['start'], 0), sURL, html)
  else Begin
    sId := oMap.Matches['code'];
    If sId = '' Then
      PublishHome(sURL, html)
    else If sId = '*' Then
      PublishCodes(sURL, StrToIntDef(oMap.Matches['start'], 0), html)
    else
      PublishCode(sURL, sId, html);
  End;
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
    oMap.Free;
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

function TloincPublisher.descLength(i : cardinal) : String;
begin
  if i = 0 then
    result := '0'
  else
    result := IntToStr(Length(FLoinc.Refs.GetRefs(i)));
end;

function TloincPublisher.makeParentsLists(parents : Cardinal) : TCardinalArrayArray;
begin
  SetLength(result, 0);
end;

procedure TloincPublisher.PublishHeirarchyEntry(sCode: String; iStart : Integer; const sPrefix: String; html: THTMLPublisher);
  procedure AddParentPath(index : Cardinal; plist : TCardinalArray);
  var
    p : Cardinal;
    code, text, parents, children, concepts, descendentConcepts, stems : Cardinal;
    lang : byte;
  begin
    FLoinc.Entries.GetEntry(index, code, text, parents, children, concepts, descendentConcepts, stems);
    html.URL(FLoinc.Desc.GetEntry(text, lang), sPrefix+'macode='+FLoinc.Desc.GetEntry(code, lang));
    for p in plist do
    begin
      FLoinc.Entries.GetEntry(p, code, text, parents, children, concepts, descendentConcepts, stems);
      html.AddTextPlain(' / ');
      html.URL(FLoinc.Desc.GetEntry(text, lang), sPrefix+'macode='+FLoinc.Desc.GetEntry(code, lang));
    end;
  end;

  procedure addParentsLists(index : Cardinal; parents : cardinal);
  var
    pll : TCardinalArrayArray;
    pl : TCardinalArray;
  begin
    if (parents <> 0) then
    begin
      pll := makeParentsLists(parents);
      if length(pll) = 1 then
      begin
        html.StartParagraph;
        html.addTextPlain('Heirarchy: ');
        addParentPath(index, pll[0]);
        html.EndParagraph;
      end
      else
      begin
        html.AddParagraph('Heirarchy: ');
        html.StartList();
        for pl in pll do
        begin
          html.StartListItem();
          addParentPath(index, pl);
          html.EndListItem;
        end;
        html.EndList;
      end;
    end;
  end;

var
  index, code, text, parents, children, concepts, descendentConcepts, stems, iCategories : Cardinal;
  stext : string;
  arr : TCardinalArray;
  i, iTotal : integer;
  b : boolean;
  iDescription, iOtherNames, iStems : Cardinal;
  iEntries : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Cardinal;
  iFlags : Byte;
  lang : byte;
begin
  index := FLoinc.findMAConcept(sCode);
  if index = 0 then
    raise ETerminologyError.create('Unknown Malti-axial code '+sCode);
  FLoinc.Entries.GetEntry(index, code, text, parents, children, concepts, descendentConcepts, stems);
  stext := FLoinc.Desc.GetEntry(text, lang);
  arr := FLoinc.Refs.GetRefs(children);
  if (length(arr) = 0) then
  begin
    html.Heading(1, 'LOINC Part Concept '+sText+' ('+scode+')');
    addParentsLists(index, parents);
  end
  else
  begin
    html.Heading(1, 'Categories in LOINC Part Concept '+sText+' ('+scode+')');
    addParentsLists(index, parents);

    html.StartParagraph;
    html.AddText('', true, false);
    html.EndParagraph;
    html.StartTable(true);
    html.StartTableRow;
    html.AddTableCell('Code', true);
    html.AddTableCell('Description', true);
    html.AddTableCell('Children', true);
    html.AddTableCell('Concepts', true);
    if FFHIRPath <> '' then
      html.AddTableCell('Tools', true);
    html.EndTableRow;

    for I := 0 to Length(arr) - 1 do
    begin
      FLoinc.Entries.GetEntry(arr[i], code, text, parents, children, concepts, descendentConcepts, stems);
      html.StartTableRow;
      html.AddTableCellURL(FLoinc.Desc.GetEntry(code, lang),sPrefix+'macode='+FLoinc.Desc.GetEntry(code, lang));
      html.AddTableCell(FLoinc.Desc.GetEntry(text, lang));
      html.AddTableCell(descLength(children));
      html.AddTableCell(descLength(descendentConcepts));
      if FFHIRPath <> '' then
        html.AddTableCellURL('Expanded Value Set', FFHIRPath+'ValueSet?_query=expand&identifier=http://loinc.org/vs/'+FLoinc.Desc.GetEntry(code, lang));
      html.EndTableRow;
    end;
    html.EndTable;
  end;

  // reset descendentConcepts:
  FLoinc.Entries.GetEntry(index, code, text, parents, children, concepts, descendentConcepts, stems);
  arr := FLoinc.Refs.GetRefs(descendentConcepts);
  if (length(arr) > 0) then
  begin
    html.StartParagraph;
    html.AddText('LOINC Codes in Category "'+sText+'"', true, false);
    if FFHIRPath <> '' then
    begin
      html.AddTextPlain('. Get this as an ');
      html.URL('Expanded Value Set', FFHIRPath+'ValueSet?_query=expand&identifier=http://loinc.org/vs/'+scode);
    end;
    html.EndParagraph;
    b := false;

    html.StartTable(false);
    html.StartTableRow;
    html.StartTableCell;
    html.AddParagraph(' ');
    html.EndTableCell;
    html.StartTableCell;
    html.StartList;
    iTotal := length(arr)-1;
    For i := iStart to Min(iStart+MAX_ROWS, iTotal) Do
    Begin
      if not b And ((i - iStart) / Min(MAX_ROWS, iTotal) >= 0.5) Then
      Begin
        html.EndList;
        html.EndTableCell;
        html.StartTableCell;
        html.StartList;
        b := true;
      End;
      FLoinc.CodeList.GetInformation(arr[i], nil, sCode1, iDescription, iOtherNames, iStems, iEntries, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);
      html.StartListItem;
      html.URL(sCode1, sPrefix + 'code='+sCode1);
      html.AddTextPlain(': '+FLoinc.Desc.GetEntry(iDescription, lang));
      html.EndListItem;
    End;
    html.EndList;
    html.EndTableCell;
    html.EndTableRow;
    html.EndTable;
    if (iStart > 0) or (iStart+MAX_ROWS < iTotal) Then
    Begin
      html.StartParagraph;
      if iStart > 0 Then
      Begin
        html.URL('Start', sPrefix+'macode='+sCode);
        html.AddTextPlain(' ');
      End;
      if iStart > MAX_ROWS Then
      Begin
        html.URL('Previous', sPrefix+'macode='+sCode+'&start='+inttostr(iStart - MAX_ROWS));
        html.AddTextPlain(' ');
      End;
      html.AddText('Page '+ inttostr((iStart div MAX_ROWS) + 1)+' of '+inttostr(iTotal div MAX_ROWS + 1), true, false);
      if (iStart+MAX_ROWS < iTotal) And (iStart + MAX_ROWS <  MAX_ROWS * (iTotal div MAX_ROWS)) Then
      Begin
        html.AddTextPlain(' ');
        html.URL('Next', sPrefix+'macode='+sCode+'&start='+inttostr(iStart + MAX_ROWS));
      End;
      if (iStart+MAX_ROWS < iTotal) Then
      Begin
        html.AddTextPlain(' ');
        html.URL('End', sPrefix+'macode='+sCode+'&start='+inttostr(MAX_ROWS * (iTotal div MAX_ROWS)));
      End;
      html.EndParagraph;
    End;

  end;
  html.line;
  html.ParaURL('LOINC Home', sPrefix);
  loincFooter(html);
  html.Done;
end;

procedure TloincPublisher.PublishHeirarchyRoot(const sPrefix: String; html: THTMLPublisher);
var
  i : integer;
  code, text, parents, children, concepts, descendentConcepts, stems: Cardinal;
  lang : byte;
begin
  html.StartTable(true);
  html.StartTableRow;
  html.AddTableCell('Code', true);
  html.AddTableCell('Description', true);
  html.AddTableCell('Children', true);
  html.AddTableCell('Concepts', true);
  html.EndTableRow;

  for I := 0 to Length(FLoinc.HeirarchyRoots) - 1 do
  begin
    FLoinc.Entries.GetEntry(FLoinc.HeirarchyRoots[i], code, text, parents, children, concepts, descendentConcepts, stems);

    html.StartTableRow;
    html.AddTableCellURL(FLoinc.Desc.GetEntry(code, lang),sPrefix+'macode='+FLoinc.Desc.GetEntry(code, lang));
    html.AddTableCell(FLoinc.Desc.GetEntry(text, lang));
    html.AddTableCell(descLength(children));
    html.AddTableCell(descLength(descendentConcepts));
    html.EndTableRow;
  end;
  html.EndTable;
end;

procedure TloincPublisher.PublishHome(const sPrefix: String; html: THTMLPublisher);
var
  i : Integer;
Begin
  html.Heading(1, 'LOINC Definitions');
  if not FLOINC.Loaded then
  Begin
    html.StartParagraph;
    html.AddText('LOINC is not loaded', true, false);
    html.EndParagraph;
  End
  Else
  Begin
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
    PublishConcept(true, sPrefix, inttostr(FLoinc.root), 0, html);
    html.AddParagraph;

    html.AddText('LOINC Heirarchy', true, false);
    html.EndParagraph;
    PublishHeirarchyRoot(sPrefix, html);
    html.AddParagraph;


    html.Line;
    Lock.Lock;
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
  End;
  loincFooter(html);
  html.Done;
End;


function TloincPublisher.GetConceptDesc(iConcept : cardinal):String;
var
  iName : Cardinal;
  iChildren : Cardinal;
  iCodes : Cardinal;
  lang : byte;
Begin
  if iConcept = 0 then
    result := ''
  Else
  Begin
    FLoinc.Concepts.GetConcept(iConcept, langs, iName, iChildren, iCodes);
    result := FLoinc.Desc.GetEntry(iname, lang);
  End;
End;


procedure TloincPublisher.loincFooter(html: THTMLPublisher);
begin
  html.StartParagraph;
  html.AddTextPlain('LOINC Version: ');
  html.AddTextPlain(FLoinc.version(nil));
  html.EndParagraph;
end;

procedure TloincPublisher.PublishCode(const sPrefix, sCode: String; html: THTMLPublisher);
var
  iIndex : Cardinal;
  iDescription, iOtherNames, iStems, iCategories : Cardinal;
  sCode1, s, se : String;
  iEntries : Cardinal;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, parents, children, concepts, descendentConcepts, stems, iCode, iText : Cardinal;
  iFlags : Byte;
  iRefs : TCardinalArray;
  i, j : integer;
  lang : byte;
  found, match : boolean;
begin
  iRefs := nil;
  if FLoinc.CodeList.FindCode(sCode, iIndex) Then
  Begin
    FLoinc.CodeList.GetInformation(iIndex, langs, sCode1, iDescription, iOtherNames, iEntries, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);
    assert(sCode = sCode1);
    html.Heading(1, 'LOINC Code '+sCode+' : '+FLoinc.Desc.GetEntry(iDescription, lang));
    html.StartTable(true);
    if iComponent <> 0 Then
    Begin
      html.StartRow();
      html.AddTableCell('Component');
      html.AddTableCell(GetConceptDesc(iComponent));
      html.EndTableRow;
    End;
    if iProperty <> 0 Then
    Begin
      html.StartRow();
      html.AddTableCell('Property');
      html.AddTableCell(GetConceptDesc(iProperty));
      html.EndTableRow;
    End;
    if iTimeAspect <> 0 Then
    Begin
      html.StartRow();
      html.AddTableCell('Time Aspect');
      html.AddTableCell(GetConceptDesc(iTimeAspect));
      html.EndTableRow;
    End;
    if iSystem <> 0 Then
    Begin
      html.StartRow();
      html.AddTableCell('System');
      html.AddTableCell(GetConceptDesc(iSystem));
      html.EndTableRow;
    End;
    if iScale <> 0 Then
    Begin
      html.StartRow();
      html.AddTableCell('Scale');
      html.AddTableCell(GetConceptDesc(iScale));
      html.EndTableRow;
    End;
    if iMethod <> 0 Then
    Begin
      html.StartRow();
      html.AddTableCell('Method');
      html.AddTableCell(GetConceptDesc(iMethod));
      html.EndTableRow;
    End;
    if iClass <> 0 Then
    Begin
      html.StartRow();
      html.AddTableCell('Class');
      html.AddTableCell(GetConceptDesc(iClass));
      html.EndTableRow;
    End;

    html.StartRow();
    html.AddTableCell('Type');
    if iFlags and FLAGS_CLIN > 0 Then
      html.AddTableCell('Clinical')
    Else if iFlags and FLAGS_ATT > 0 Then
      html.AddTableCell('Attachment')
    Else if iFlags and FLAGS_SURV > 0 Then
      html.AddTableCell('Survey')
    Else
      html.AddTableCell('Lab');
    html.EndTableRow;

    html.StartRow();
    html.AddTableCell('Status');
    if iFlags and FLAGS_HOLD > 0 Then
      html.AddTableCell('Not yet final')
    Else
      html.AddTableCell('Final');
    html.EndTableRow;

    if iFlags and FLAGS_ROOT > 0 Then
    Begin
      html.StartRow();
      html.AddTableCell('Root');
      html.AddTableCell('This is a root of a set');
      html.EndTableRow;
    End;

    if iFlags and FLAGS_UNITS > 0 Then
    Begin
      html.StartRow();
      html.AddTableCell('Units');
      html.AddTableCell('Units are required');
      html.EndTableRow;
    End;

    html.StartRow();
    html.AddTableCell('Order/Obs Status');
    if (iFlags and FLAGS_ORDER> 0 ) and (iFlags and FLAGS_OBS> 0 ) Then
      html.AddTableCell('Both')
    Else if iFlags and FLAGS_ORDER > 0 Then
      html.AddTableCell('Order')
    Else if iFlags and FLAGS_OBS > 0 Then
      html.AddTableCell('Observation')
    Else
      html.AddTableCell('');
    html.EndTableRow;

    html.EndTable;

    html.AddParagraph('');
    if iOtherNames <> 0 Then
    Begin
      html.StartParagraph;
      html.AddText('Other Names', true, false);
      html.EndParagraph;
      iRefs := FLoinc.Refs.GetRefs(iOtherNames);
      html.StartTable(true);
      for i := Low(iRefs) To High(iRefs) Do
        if iRefs[i] <> 0 Then
        begin
          s := FLoinc.desc.GetEntry(iRefs[i], lang);
          if useLang(lang) then
          begin
            html.StartRow();
            html.AddTableCell(s);
            html.EndTableRow;
          end;
        End;
      html.EndTable;
    End;

    found := false;
    for i := 0 to FLoinc.Entries.Count - 1 do
    begin
      FLoinc.Entries.GetEntry(i, iCode, iText, parents, children, concepts, descendentConcepts, stems);
      se := FLoinc.Desc.GetEntry(iCode, lang);

      iRefs := FLoinc.Refs.GetRefs(descendentConcepts);
      match := false;
      for j := 0 to length(iRefs) - 1 do
        if iRefs[j] = iIndex then
        begin
          match := true;
          break;
        end;
      if match then
      begin
        if not found then
        begin
          html.StartParagraph;
          html.AddText('Categories', true, false);
          html.EndParagraph;
          html.StartTable(true);
          found := true;
        end;
        html.StartRow();
        html.AddTableCell(se);
        html.AddTableCell(FLoinc.Desc.GetEntry(iText, lang));
        html.EndTableRow;
      end;
    end;
    if found then
      html.EndTable;
  End
  Else
    html.AddParagraph('Unable to find code '+sCode);
  html.line;
  html.ParaURL('LOINC Home', sPrefix);
  loincFooter(html);
  html.done;
end;

procedure TloincPublisher.PublishConcept(bRoot : Boolean; const sPrefix, sId: String; iStart : Integer; html: THTMLPublisher);
var
  aChildren : TCardinalArray;
  aCodes : TCardinalArray;
  iName : Cardinal;
  iChildren : Cardinal;
  iCodes : Cardinal;
  i : Integer;
  iDummy : Cardinal;
  b : Boolean;

  iDescription, iOtherNames, iStems, iCategories : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Cardinal;
  iFlags, lang : Byte;
  iEntries : Cardinal;

begin
  aChildren := nil;
  aCodes := nil;
  FLoinc.Concepts.GetConcept(StrToInt(sId), langs, iName, iChildren, iCodes);
  if Not bRoot then
    html.Heading(1, 'LOINC Concept '+FLoinc.Desc.GetEntry(iName, lang));

  b := false;

  if iChildren <> 0 Then
  begin
    aChildren := FLoinc.Refs.GetRefs(iChildren);
    html.StartTable(false, 'bare');
    html.StartTableRow;
    html.StartTableCell;
    html.AddParagraph(' ');
    html.EndTableCell;
    html.StartTableCell;
    html.StartList;
    For i := iStart to Min(iStart+MAX_ROWS, High(aChildren)) Do
    Begin
      if not b And (Length(aChildren) > 20) And ((i - iStart) / Min(MAX_ROWS, Length(aChildren)) >= 0.5) Then
      Begin
        html.EndList;
        html.EndTableCell;
        html.StartTableCell;
        html.StartList;
        b := true;
      End;
      FLoinc.Concepts.GetConcept(aChildren[i], langs, iName, iChildren, iDummy);
      html.StartListItem;
      html.URL(FLoinc.Desc.GetEntry(iName, lang), sPrefix + 'id='+inttostr(aChildren[i]));
      html.EndListItem;
    End;
    html.EndList;
    html.EndTableCell;
    html.EndTableRow;
    html.EndTable;
    if (iStart > 0) or (iStart+MAX_ROWS < High(aChildren)) Then
    Begin
      html.StartParagraph;
      if iStart > 0 Then
      Begin
        html.URL('Start', sPrefix+'id='+sId);
        html.AddTextPlain(' ');
      End;
      if iStart > MAX_ROWS Then
      Begin
        html.URL('Previous', sPrefix+'id='+sId+'&start='+inttostr(iStart - MAX_ROWS));
        html.AddTextPlain(' ');
      End;
      html.AddText('Page '+ inttostr((iStart div MAX_ROWS) + 1)+' of '+inttostr(High(aChildren) div MAX_ROWS + 1), true, false);
      if (iStart+MAX_ROWS < High(aChildren)) And (iStart + MAX_ROWS <  MAX_ROWS * (High(aChildren) div MAX_ROWS)) Then
      Begin
        html.AddTextPlain(' ');
        html.URL('Next', sPrefix+'id='+sId+'&start='+inttostr(iStart + MAX_ROWS));
      End;
      if (iStart+MAX_ROWS < High(aChildren)) Then
      Begin
        html.AddTextPlain(' ');
        html.URL('End', sPrefix+'id='+sId+'&start='+inttostr(MAX_ROWS * (High(aChildren) div MAX_ROWS)));
      End;
      html.EndParagraph;
    End;
  End;

  b := false;
  if iCodes <> 0 Then
  begin
    aCodes := FLoinc.Refs.GetRefs(iCodes);
    html.StartTable(false);
    html.StartTableRow;
    html.StartTableCell;
    html.AddParagraph(' ');
    html.EndTableCell;
    html.StartTableCell;
    For i := iStart to Min(iStart+MAX_ROWS, High(aCodes)) Do
    Begin
      if not b And (Length(aCodes) > 20) And ((i - iStart) / Min(MAX_ROWS, Length(aCodes)) >= 0.5) Then
      Begin
        html.EndTableCell;
        html.StartTableCell;
        b := true;
      End;
      FLoinc.CodeList.GetInformation(aCodes[i], nil, sCode1, iDescription, iOtherNames, iEntries, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);
      html.StartParagraph;
      html.URL(sCode1, sPrefix + 'code='+sCode1);
      html.AddTextPlain(': '+FLoinc.Desc.GetEntry(iDescription, lang));
      html.EndParagraph;
    End;
    html.EndTableCell;
    html.EndTableRow;
    html.EndTable;
    if (iStart > 0) or (iStart+MAX_ROWS < High(aCodes)) Then
    Begin
      html.StartParagraph;
      if iStart > 0 Then
      Begin
        html.URL('Start', sPrefix+'id='+sId);
        html.AddTextPlain(' ');
      End;
      if iStart > MAX_ROWS Then
      Begin
        html.URL('Previous', sPrefix+'id='+sId+'&start='+inttostr(iStart - MAX_ROWS));
        html.AddTextPlain(' ');
      End;
      html.AddText('Page '+ inttostr((iStart div MAX_ROWS) + 1)+' of '+inttostr(High(aCodes) div MAX_ROWS + 1), true, false);
      if (iStart+MAX_ROWS < High(aCodes)) And (iStart + MAX_ROWS <  MAX_ROWS * (High(aCodes) div MAX_ROWS)) Then
      Begin
        html.AddTextPlain(' ');
        html.URL('Next', sPrefix+'id='+sId+'&start='+inttostr(iStart + MAX_ROWS));
      End;
      if (iStart+MAX_ROWS < High(aCodes)) Then
      Begin
        html.AddTextPlain(' ');
        html.URL('End', sPrefix+'id='+sId+'&start='+inttostr(MAX_ROWS * (High(aCodes) div MAX_ROWS)));
      End;
      html.EndParagraph;
    End;
  End;
  if not bRoot then
  begin
    html.line;
    html.ParaURL('LOINC Home', sPrefix);
    loincFooter(html);
    html.done;
  end;
end;

procedure TloincPublisher.PublishCodes(const sPrefix: String; iStart: Integer; html: THTMLPublisher);
var
  i : Integer;
  iTotal : Integer;
  b : Boolean;

  iDescription, iOtherNames, iStems, iCategories : Cardinal;
  iEntries : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Cardinal;
  iFlags, lang : Byte;
begin
  b := false;

  if iStart = 0 then
    html.Heading(1, 'All Loinc Codes')
  else
    html.Heading(1, 'All Loinc Codes (offset = '+inttostr(iStart)+')');

    html.StartTable(false);
    html.StartTableRow;
    html.StartTableCell;
    html.AddParagraph(' ');
    html.EndTableCell;
    html.StartTableCell;
    html.StartList;
    iTotal := FLoinc.CodeList.Count;
    For i := iStart to Min(iStart+MAX_ROWS, iTotal) Do
    Begin
      if not b And ((i - iStart) / Min(MAX_ROWS, iTotal) >= 0.5) Then
      Begin
        html.EndList;
        html.EndTableCell;
        html.StartTableCell;
        html.StartList;
        b := true;
      End;
      FLoinc.CodeList.GetInformation(i, langs, sCode1, iDescription, iOtherNames, iStems, iEntries, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);
      html.StartListItem;
      html.URL(sCode1, sPrefix + 'code='+sCode1);
      html.AddTextPlain(': '+FLoinc.Desc.GetEntry(iDescription, lang));
      html.EndListItem;
    End;
    html.EndList;
    html.EndTableCell;
    html.EndTableRow;
    html.EndTable;
    if (iStart > 0) or (iStart+MAX_ROWS < iTotal) Then
    Begin
      html.StartParagraph;
      if iStart > 0 Then
      Begin
        html.URL('Start', sPrefix+'code=*');
        html.AddTextPlain(' ');
      End;
      if iStart > MAX_ROWS Then
      Begin
        html.URL('Previous', sPrefix+'code=*&start='+inttostr(iStart - MAX_ROWS));
        html.AddTextPlain(' ');
      End;
      html.AddText('Page '+ inttostr((iStart div MAX_ROWS) + 1)+' of '+inttostr(iTotal div MAX_ROWS + 1), true, false);
      if (iStart+MAX_ROWS < iTotal) And (iStart + MAX_ROWS <  MAX_ROWS * (iTotal div MAX_ROWS)) Then
      Begin
        html.AddTextPlain(' ');
        html.URL('Next', sPrefix+'code=*&start='+inttostr(iStart + MAX_ROWS));
      End;
      if (iStart+MAX_ROWS < iTotal) Then
      Begin
        html.AddTextPlain(' ');
        html.URL('End', sPrefix+'code=*&start='+inttostr(MAX_ROWS * (iTotal div MAX_ROWS)));
      End;
      html.EndParagraph;
    End;
  html.line;
  html.ParaURL('LOINC Home', sPrefix);
end;

constructor TloincPublisher.Create(oLoinc : TLoincServices; FHIRPathEngine : String; const lang : THTTPLanguages);
begin
  inherited Create;
  Lock := TFslLock.Create;
  FSearchCache := TStringList.Create;
  FSearchCache.Sorted := true;
  FLoinc := oLoinc.Link;
  FFHIRPath := FHIRPathEngine;
  langs := FLoinc.langsForLang(lang);
end;

destructor TloincPublisher.Destroy;
var
  i : integer;
begin
  For i := 0 to FSearchCache.Count - 1 do
    FSearchCache.Objects[i].Free;
  FSearchCache.Free;
  Lock.Free;
  FLoinc.Free;
  inherited;
end;

Type
  TSearchCache = class (TObject)
  public
    a : TMatchArray;
  End;

procedure TloincPublisher.PublishSearch(const sPrefix, sText: String; iStart: Integer; all : boolean; html: THTMLPublisher);
  //procedure AddParent(p : Cardinal);
  //var
  //  code, text, parent, children, concepts, descendentConcepts, stems, iCategories : Cardinal;
  //  lang : byte;
  //begin
  //  FLoinc.Entries.GetEntryH(p, code, text, parent, children, concepts, descendentConcepts, stems);
  //  if (parent <> NO_PARENT) then
  //  begin
  //    AddParent(parent);
  //    html.AddTextPlain(' / ');
  //  end;
  //  html.URL(FLoinc.Desc.GetEntry(text, lang), sPrefix+'macode='+FLoinc.Desc.GetEntry(code, lang));
  //end;

var
  a : TMatchArray;
  i : integer;
  o : TSearchCache;
  iTotal : Integer;
//  iDummy : Cardinal;

  iDescription, iOtherNames, iStems, iCategories : Cardinal;
  iEntries : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Cardinal;
  iFlags, ilang : Byte;
  code, text, parents, children, concepts, descendentConcepts, stems: Cardinal;
begin
  Lock.Lock;
  Try
    if FSearchCache.Find(sText, i) Then
      a := TSearchCache(FSearchCache.Objects[i]).a
    else
    Begin
      a := FLoinc.Search(sText, all);
      o := TSearchCache.Create;
      o.a := a;
      FSearchCache.AddObject(sText, o);
    End;
  Finally
    Lock.Unlock;
  End;
  html.Heading(1, 'Search LOINC for '+sText);

  html.StartTable(false);
  html.StartTableRow;
  html.AddTableCell('Code');
  html.AddTableCell('Description');
  html.AddTableCell('Component');
  html.AddTableCell('Property');
  html.AddTableCell('Time Aspect');
  html.AddTableCell('System');
  html.AddTableCell('Scale');
  html.AddTableCell('Method');
  html.AddTableCell('Class');
  html.AddTableCell('Rating');
  html.EndTableRow;
  iTotal := Length(a)-1;


  For i := iStart to Min(iStart+MAX_ROWS, iTotal) Do
  Begin
    html.StartTableRow;
    if a[i].iscode then
    begin
      FLoinc.CodeList.GetInformation(a[i].index, nil, sCode1, iDescription, iOtherNames, iEntries, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);
      html.AddTableCellURL(sCode1, sPrefix + 'code='+sCode1);
      html.AddTableCell(FLoinc.Desc.GetEntry(iDescription, ilang));
      html.AddTableCell(GetConceptDesc(iComponent));
      html.AddTableCell(GetConceptDesc(iProperty));
      html.AddTableCell(GetConceptDesc(iTimeAspect));
      html.AddTableCell(GetConceptDesc(iSystem));
      html.AddTableCell(GetConceptDesc(iScale));
      html.AddTableCell(GetConceptDesc(iMethod));
      html.AddTableCell(GetConceptDesc(iClass));
      html.AddTableCell(RealToString(a[i].Priority));
    end
    else
    begin
      FLoinc.Entries.GetEntry(a[i].index, code, text, parents, children, concepts, descendentConcepts, stems);
      sCode1 := FLoinc.Desc.GetEntry(code, ilang);
      html.AddTableCellURL(sCode1, sPrefix + 'macode='+sCode1);
      html.AddTableCell(FLoinc.Desc.GetEntry(text, ilang));
      html.StartTableCell(7);
      //html.AddTextPlain('Heirarchy: ');
      //if (parent = NO_PARENT) then
      //  html.AddTextPlain('(root)')
      //else
      //  AddParent(parent);
      html.EndTableCell;
      html.AddTableCell(RealToString(a[i].Priority - 1000000));
    end;
    html.EndTableRow;
    End;
  html.EndTable;
  if (iStart > 0) or (iStart+MAX_ROWS < iTotal) Then
  Begin
    html.StartParagraph;
    if iStart > 0 Then
    Begin
      html.URL('Start', sPrefix+'srch='+sText);
      html.AddTextPlain(' ');
    End;
    if iStart > MAX_ROWS Then
    Begin
      html.URL('Previous', sPrefix+'srch='+sText+'&start='+inttostr(iStart - MAX_ROWS));
      html.AddTextPlain(' ');
    End;
    html.AddText('Page '+ inttostr((iStart div MAX_ROWS) + 1)+' of '+inttostr(iTotal div MAX_ROWS + 1), true, false);
    if (iStart+MAX_ROWS < iTotal) And (iStart + MAX_ROWS <  MAX_ROWS * (iTotal div MAX_ROWS)) Then
    Begin
      html.AddTextPlain(' ');
      html.URL('Next', sPrefix+'srch='+sText+'&start='+inttostr(iStart + MAX_ROWS));
    End;
    if (iStart+MAX_ROWS < iTotal) Then
    Begin
      html.AddTextPlain(' ');
      html.URL('End', sPrefix+'srch='+sText+'&start='+inttostr(MAX_ROWS * (iTotal div MAX_ROWS)));
    End;
    html.EndParagraph;
  End;
  html.line;
  html.ParaURL('LOINC Home', sPrefix);
  loincFooter(html);
  html.done;
end;

function TloincPublisher.useLang(lang: byte): boolean;
var
  b: byte;
begin
  result := false;
  for b in langs do
    if b = lang then
      exit(true);
end;

function TloincPublisher.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSearchCache.sizeInBytes);
  inc(result, FLoinc.sizeInBytes);
  inc(result, (FFHIRPath.length * sizeof(char)) + 12);
end;

End.

