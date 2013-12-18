unit loincPublisher;

{
Copyright (c) 2001-2013, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

Interface

Uses
  SysUtils, Classes, Math,
  AdvObjects, AdvStringMatches, AdvExclusiveCriticalSections,
  HTMLPublisher, LOINCServices;

Const
  MAX_ROWS = 50;

Type
  TloincPublisher = class (TAdvObject)
  Private
    Lock : TAdvExclusiveCriticalSection;
    FSearchCache : TStringList;
    FLoinc : TLOINCServices;

    function GetConceptDesc(iConcept : Word):String;

    Procedure PublishCode(Const sPrefix, sCode : String; html : THTMLPublisher);
    Procedure PublishCodes(Const sPrefix : String; iStart : Integer; html : THTMLPublisher);
    Procedure PublishConcept(bRoot : Boolean; Const sPrefix, sId : String; iStart : Integer; html : THTMLPublisher);
    Procedure PublishHome(Const sPrefix : String; html : THTMLPublisher);
    Procedure PublishSearch(Const sPrefix, sText : String; iStart: Integer; html : THTMLPublisher);


    Procedure ProcessMap(Const sPath : String; oMap : TAdvStringMatch);
    Procedure PublishDictInternal(oMap : TAdvStringMatch; Const sPrefix : String; html : THTMLPublisher);
  Public
    Constructor Create(oLoinc : TLoincServices);
    Destructor Destroy; Override;
    Procedure PublishDict(Const sPath, sPrefix : String; html : THTMLPublisher); Overload; Virtual;
    Procedure PublishDict(oMap : TAdvStringMatch; Const sPrefix : String; html : THTMLPublisher); Overload; Virtual;
  End;

Implementation

Uses
  EncodeSupport,
  StringSupport;

Procedure TloincPublisher.PublishDictInternal(oMap : TAdvStringMatch; Const sPrefix : String; html : THTMLPublisher);
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
    else
      PublishSearch(sURL, oMap.Matches['srch'], StrToIntDef(oMap.Matches['start'], 0), html)
  else
  Begin
    sId := oMap.Matches['code'];
    If sId = '' Then
      PublishHome(sURL, html)
    else If sId = '*' Then
      PublishCodes(sURL, StrToIntDef(oMap.Matches['start'], 0), html)
    else
      PublishCode(sURL, sId, html);
  End;
End;


Procedure TloincPublisher.ProcessMap(Const sPath : String; oMap : TAdvStringMatch);
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
  oMap : TAdvStringMatch;
Begin
  oMap := TAdvStringMatch.Create;
  Try
    ProcessMap(sPath, oMap);
    PublishDict(oMap, sPrefix, html);
  Finally
    oMap.Free;
  End;
End;



procedure TloincPublisher.PublishDict(oMap: TAdvStringMatch; const sPrefix: String; html: THTMLPublisher);
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
  html.Header('LOINC Definitions');
  if not FLOINC.Loaded then
  Begin
    html.StartParagraph;
    html.AddText('LOINC is not loaded', true, false);
    html.EndParagraph;
  End
  Else
  Begin
    html.StartParagraph;
    html.AddText('LOINC Axes', true, false);
    html.EndParagraph;
    PublishConcept(true, sPrefix, inttostr(FLoinc.root), 0, html);
    html.AddParagraph;

    html.StartParagraph;
    html.AddText('LOINC Codes', true, false);
    html.EndParagraph;

    html.Line;

    html.StartForm('GET', sPrefix);
    html.StartParagraph;
    html.AddTextPlain('Search: ');
    html.textInput('srch');
    html.submit('Go');
    html.endForm;

    html.ParaURL('Browse All Codes', sPrefix+'code=*');
    html.AddParagraph('');
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
  html.Done;
End;


function TloincPublisher.GetConceptDesc(iConcept : Word):String;
var
  iName : Cardinal;
  iChildren : Cardinal;
  iCodes : Cardinal;
Begin
  if iConcept = 0 then
    result := ''
  Else
  Begin
    FLoinc.Concepts.GetConcept(iConcept, iName, iChildren, iCodes);
    result := FLoinc.Desc.GetEntry(iname);
  End;
End;


procedure TloincPublisher.PublishCode(const sPrefix, sCode: String; html: THTMLPublisher);
var
  iIndex : Cardinal;
  iDescription, iOtherNames, iStems : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;
  iRefs : TCardinalArray;
  i : integer;
  iCount : integer;
Begin
  iRefs := nil;
  if FLoinc.Code.FindCode(sCode, iIndex) Then
  Begin
    FLoinc.Code.GetInformation(iIndex, sCode1, iDescription, iOtherNames, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);
    assert(sCode = sCode1);
    html.Header('LOINC Code '+sCode+' : '+FLoinc.Desc.GetEntry(iDescription));
    html.StartTable(true);
    iCount := 0;
    if iComponent <> 0 Then
    Begin
      inc(iCount);
      html.StartRowFlip(iCount);
      html.AddTableCell('Component');
      html.AddTableCell(GetConceptDesc(iComponent));
      html.EndTableRow;
    End;
    if iProperty <> 0 Then
    Begin
      inc(iCount);
      html.StartRowFlip(iCount);
      html.AddTableCell('Property');
      html.AddTableCell(GetConceptDesc(iProperty));
      html.EndTableRow;
    End;
    if iTimeAspect <> 0 Then
    Begin
      inc(iCount);
      html.StartRowFlip(iCount);
      html.AddTableCell('Time Aspect');
      html.AddTableCell(GetConceptDesc(iTimeAspect));
      html.EndTableRow;
    End;
    if iSystem <> 0 Then
    Begin
      inc(iCount);
      html.StartRowFlip(iCount);
      html.AddTableCell('System');
      html.AddTableCell(GetConceptDesc(iSystem));
      html.EndTableRow;
    End;
    if iScale <> 0 Then
    Begin
      inc(iCount);
      html.StartRowFlip(iCount);
      html.AddTableCell('Scale');
      html.AddTableCell(GetConceptDesc(iScale));
      html.EndTableRow;
    End;
    if iMethod <> 0 Then
    Begin
      inc(iCount);
      html.StartRowFlip(iCount);
      html.AddTableCell('Method');
      html.AddTableCell(GetConceptDesc(iMethod));
      html.EndTableRow;
    End;
    if iClass <> 0 Then
    Begin
      inc(iCount);
      html.StartRowFlip(iCount);
      html.AddTableCell('Class');
      html.AddTableCell(GetConceptDesc(iClass));
      html.EndTableRow;
    End;
    if iv2dt <> 0 Then
    Begin
      inc(iCount);
      html.StartRowFlip(iCount);
      html.AddTableCell('v2 Data Type');
      html.AddTableCell(GetConceptDesc(iv2dt));
      html.EndTableRow;
    End;
    if iv3dt <> 0 Then
    Begin
      inc(iCount);
      html.StartRowFlip(iCount);
      html.AddTableCell('v3 Data Type');
      html.AddTableCell(GetConceptDesc(iv3dt));
      html.EndTableRow;
    End;

    inc(iCount);
    html.StartRowFlip(iCount);
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

    inc(iCount);
    html.StartRowFlip(iCount);
    html.AddTableCell('Status');
    if iFlags and FLAGS_HOLD > 0 Then
      html.AddTableCell('Not yet final')
    Else
      html.AddTableCell('Final');
    html.EndTableRow;

    if iFlags and FLAGS_ROOT > 0 Then
    Begin
      inc(iCount);
      html.StartRowFlip(iCount);
      html.AddTableCell('Root');
      html.AddTableCell('This is a root of a set');
      html.EndTableRow;
    End;

    if iFlags and FLAGS_UNITS > 0 Then
    Begin
      inc(iCount);
      html.StartRowFlip(iCount);
      html.AddTableCell('Units');
      html.AddTableCell('Units are required');
      html.EndTableRow;
    End;

    inc(iCount);
    html.StartRowFlip(iCount);
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
      iRefs := FLoinc.Refs.GetCardinals(iOtherNames);
      html.StartTable(true);
      for i := Low(iRefs) To High(iRefs) Do
        if iRefs[i] <> 0 Then
        begin
          html.StartRowFlip(i);
          html.AddTableCell(FLoinc.desc.GetEntry(iRefs[i]));
          html.EndTableRow;
        End;
      html.EndTable;
    End;
    html.done;
  End
  Else
    html.AddParagraph('Unable to find code '+sCode);
end;

procedure TloincPublisher.PublishConcept(bRoot : Boolean; const sPrefix, sId: String; iStart : Integer; html: THTMLPublisher);
var
  aChildren : TWordArray;
  aCodes : TCardinalArray;
  iName : Cardinal;
  iChildren : Cardinal;
  iCodes : Cardinal;
  i : Integer;
  iDummy : Cardinal;
  b : Boolean;

  iDescription, iOtherNames, iStems : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;

begin
  aChildren := nil;
  aCodes := nil;
  FLoinc.Concepts.GetConcept(StrToInt(sId), iName, iChildren, iCodes);
  if Not bRoot then
    html.Header('LOINC Concept '+FLoinc.Desc.GetEntry(iName));

  b := false;

  if iChildren <> 0 Then
  begin
    aChildren := FLoinc.Refs.GetWords(iChildren);
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
      FLoinc.Concepts.GetConcept(aChildren[i], iName, iChildren, iDummy);
      html.StartListItem;
      html.URL(FLoinc.Desc.GetEntry(iName), sPrefix + 'id='+inttostr(aChildren[i]));
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
    aCodes := FLoinc.Refs.GetCardinals(iCodes);
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
      FLoinc.Code.GetInformation(aCodes[i], sCode1, iDescription, iOtherNames, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);
      html.StartParagraph;
      html.URL(sCode1, sPrefix + 'code='+sCode1);
      html.AddTextPlain(': '+FLoinc.Desc.GetEntry(iDescription));
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
    html.done;
end;

procedure TloincPublisher.PublishCodes(const sPrefix: String; iStart: Integer; html: THTMLPublisher);
var
  i : Integer;
  iTotal : Integer;
  b : Boolean;

  iDescription, iOtherNames, iStems : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;
begin
  b := false;

  if iStart = 0 then
    html.header('All Loinc Codes')
  else
    html.header('All Loinc Codes (offset = '+inttostr(iStart)+')');

    html.StartTable(false);
    html.StartTableRow;
    html.StartTableCell;
    html.AddParagraph(' ');
    html.EndTableCell;
    html.StartTableCell;
    html.StartList;
    iTotal := FLoinc.Code.Count;
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
      FLoinc.Code.GetInformation(i, sCode1, iDescription, iOtherNames, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);
      html.StartListItem;
      html.URL(sCode1, sPrefix + 'code='+sCode1);
      html.AddTextPlain(': '+FLoinc.Desc.GetEntry(iDescription));
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
end;

constructor TloincPublisher.Create;
begin
  inherited Create;
  Lock := TAdvExclusiveCriticalSection.Create;
  FSearchCache := TStringList.Create;
  FSearchCache.Sorted := true;
  FLoinc := oLoinc.Link;
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
    a : TMatchArray;
  End;

procedure TloincPublisher.PublishSearch(const sPrefix, sText: String; iStart: Integer; html: THTMLPublisher);
var
  a : TMatchArray;
  i : integer;
  o : TSearchCache;
  iTotal : Integer;
//  iDummy : Cardinal;

  iDescription, iOtherNames, iStems : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;
begin
  Lock.Lock;
  Try
    if FSearchCache.Find(sText, i) Then
      a := TSearchCache(FSearchCache.Objects[i]).a
    else
    Begin
      a := FLoinc.Search(sText);
      o := TSearchCache.Create;
      o.a := a;
      FSearchCache.AddObject(sText, o);
    End;
  Finally
    Lock.Unlock;
  End;                        
  html.Header('Search LOINC for '+sText);
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
    FLoinc.Code.GetInformation(a[i].index, sCode1, iDescription, iOtherNames, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);

    html.StartTableRow;
    html.AddTableCellURL(sCode1, sPrefix + 'code='+sCode1);
    html.AddTableCell(FLoinc.Desc.GetEntry(iDescription));
    html.AddTableCell(GetConceptDesc(iComponent));
    html.AddTableCell(GetConceptDesc(iProperty));
    html.AddTableCell(GetConceptDesc(iTimeAspect));
    html.AddTableCell(GetConceptDesc(iSystem));
    html.AddTableCell(GetConceptDesc(iScale));
    html.AddTableCell(GetConceptDesc(iMethod));
    html.AddTableCell(GetConceptDesc(iClass));
    html.AddTableCell(RealToString(a[i].Priority));
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
  html.done;
end;

End.

