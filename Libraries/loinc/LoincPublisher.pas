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
  classes,
  Math,
  AdvObjects,
  SysUtils,
  WordProcessorDocuments,
  WordProcessorEntities,
  AdvStringMatches,
  AdvExclusiveCriticalSections,
  HL7V2DocumentPublishers,
  gwLOINC;

Const
  MAX_ROWS = 200;

Type
  TloincPublisher = class (TAdvObject)
  Private
    Lock : TAdvExclusiveCriticalSection;
    FSearchCache : TStringList;
    FLoinc : TLOINCServices;

    function GetConceptDesc(iConcept : Word):String;

    Procedure PublishCode(Const sPrefix, sCode : String; oBuilder : THL7V2DocumentPublisher);
    Procedure PublishCodes(Const sPrefix : String; iStart : Integer; oBuilder : THL7V2DocumentPublisher);
    Procedure PublishConcept(bRoot : Boolean; Const sPrefix, sId : String; iStart : Integer; oBuilder : THL7V2DocumentPublisher);
    Procedure PublishHome(Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
    Procedure PublishSearch(Const sPrefix, sText : String; iStart: Integer; oBuilder : THL7V2DocumentPublisher);


    Procedure ProcessMap(Const sPath : String; oMap : TAdvStringMatch);
    Procedure PublishDictInternal(oMap : TAdvStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
  Public
    Constructor Create(oLoinc : TLoincServices);
    Destructor Destroy; Override;
    Procedure PublishDict(Const sPath, sPrefix : String; oBuilder : THL7V2DocumentPublisher); Overload; Virtual;
    Procedure PublishDict(oMap : TAdvStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher); Overload; Virtual;
  End;

Implementation

Uses
  EncodeSupport,
  StringSupport;

Procedure TloincPublisher.PublishDictInternal(oMap : TAdvStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
Var
  sURL : String;
  sId : String;
Begin
  sURL := sPrefix +'?type=loinc&';
  sId := oMap.Matches['id'];
  If sId <> '' Then
    PublishConcept(false, sURL, sId, StrToIntDef(oMap.Matches['start'], 0), oBuilder)
  else if oMap.ExistsByKey('srch') then
    PublishSearch(sURL, oMap.Matches['srch'], StrToIntDef(oMap.Matches['start'], 0), oBuilder)
  else
  Begin
    sId := oMap.Matches['code'];
    If sId = '' Then
      PublishHome(sURL, oBuilder)
    else If sId = '*' Then
      PublishCodes(sURL, StrToIntDef(oMap.Matches['start'], 0), oBuilder)
    else
      PublishCode(sURL, sId, oBuilder);
  End;
End;


Procedure TloincPublisher.ProcessMap(Const sPath : String; oMap : TAdvStringMatch);
Var
  sLeft, sRight : String;
  sName, sValue : String;
Begin
  Stringsplit(sPath, '?', sLeft, sRight);
  oMap.Forced := True;
  While sRight <> '' Do
  Begin
    StringSplit(sRight, '&', sLeft, sRight);
    StringSplit(sLeft, '=', sName, sValue);
    oMap.Matches[DecodePercent(sName)] := DecodePercent(sValue);
  End;
End;


Procedure TloincPublisher.PublishDict(Const sPath, sPrefix : String; oBuilder : THL7V2DocumentPublisher);
Var
  oMap : TAdvStringMatch;
Begin
  oMap := TAdvStringMatch.Create;
  Try
    ProcessMap(sPath, oMap);
    PublishDict(oMap, sPrefix, oBuilder);
  Finally
    oMap.Free;
  End;
End;



procedure TloincPublisher.PublishDict(oMap: TAdvStringMatch; const sPrefix: String; oBuilder: THL7V2DocumentPublisher);
begin
  Try
    PublishDictInternal(oMap, sPrefix, oBuilder);
  Except
    On e:Exception Do
      Begin
      oBuilder.Title := 'Exception: '+e.Message;
      oBuilder.AddParagraph(e.Message);
      End;
  End;
end;

procedure TloincPublisher.PublishHome(const sPrefix: String; oBuilder: THL7V2DocumentPublisher);
var
  i : Integer;
Begin
  oBuilder.AddTitle('LOINC Definitions');
  if not FLOINC.Loaded then
  Begin
    oBuilder.StartParagraph;
    oBuilder.AddText('LOINC is not loaded', true, false);
    oBuilder.EndParagraph;
  End
  Else
  Begin
    oBuilder.StartParagraph;
    oBuilder.AddText('LOINC Axes', true, false);
    oBuilder.EndParagraph;
    PublishConcept(true, sPrefix, inttostr(FLoinc.root), 0, oBuilder);
    oBuilder.AddParagraph;

    oBuilder.StartParagraph;
    oBuilder.AddText('LOINC Codes', true, false);
    oBuilder.EndParagraph;

    oBuilder.StartTable.BorderPolicy := BorderPolicyNone;
    oBuilder.StartTableRow;
    oBuilder.StartTableCell;
    oBuilder.AddParagraph(' ');
    oBuilder.EndTableCell;
    oBuilder.StartTableCell;
    oBuilder.ParaURL('Enter a Code', sPrefix+'code=??&caption=Pick A LOINC Code&prompt=Code');
    oBuilder.ParaURL('Browse All Codes', sPrefix+'code=*');
    oBuilder.ParaURL('Search', sPrefix+'srch=??&caption=Search LOINC Codes&prompt=Text');
    oBuilder.AddParagraph('');
    Lock.Lock;
    Try
      if FSearchCache.Count <> 0 Then
      Begin
        oBuilder.AddParagraph('Past Searches:');
        For i := 0 to FSearchCache.Count - 1 Do
          oBuilder.ParaURL('Search for "'+FSearchCache[i]+'"', sPrefix+'srch='+FSearchCache[i]+'&caption=Search LOINC Codes&prompt=Text').Format.ListType := WPSParagraphListTypeBullets;
      End;
    Finally
      Lock.UnLock;
    End;
    oBuilder.EndTableCell;
    oBuilder.EndTableRow;
    oBuilder.EndTable;
  End;
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


procedure TloincPublisher.PublishCode(const sPrefix, sCode: String; oBuilder: THL7V2DocumentPublisher);
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
    oBuilder.AddTitle('LOINC Code '+sCode+' : '+FLoinc.Desc.GetEntry(iDescription));
    oBuilder.StartTable;
    iCount := 0;
    if iComponent <> 0 Then
    Begin
      inc(iCount);
      oBuilder.StartRowFlip(iCount);
      oBuilder.AddTableCell('Component');
      oBuilder.AddTableCell(GetConceptDesc(iComponent));
      oBuilder.EndTableRow;
    End;
    if iProperty <> 0 Then
    Begin
      inc(iCount);
      oBuilder.StartRowFlip(iCount);
      oBuilder.AddTableCell('Property');
      oBuilder.AddTableCell(GetConceptDesc(iProperty));
      oBuilder.EndTableRow;
    End;
    if iTimeAspect <> 0 Then
    Begin
      inc(iCount);
      oBuilder.StartRowFlip(iCount);
      oBuilder.AddTableCell('Time Aspect');
      oBuilder.AddTableCell(GetConceptDesc(iTimeAspect));
      oBuilder.EndTableRow;
    End;
    if iSystem <> 0 Then
    Begin
      inc(iCount);
      oBuilder.StartRowFlip(iCount);
      oBuilder.AddTableCell('System');
      oBuilder.AddTableCell(GetConceptDesc(iSystem));
      oBuilder.EndTableRow;
    End;
    if iScale <> 0 Then
    Begin
      inc(iCount);
      oBuilder.StartRowFlip(iCount);
      oBuilder.AddTableCell('Scale');
      oBuilder.AddTableCell(GetConceptDesc(iScale));
      oBuilder.EndTableRow;
    End;
    if iMethod <> 0 Then
    Begin
      inc(iCount);
      oBuilder.StartRowFlip(iCount);
      oBuilder.AddTableCell('Method');
      oBuilder.AddTableCell(GetConceptDesc(iMethod));
      oBuilder.EndTableRow;
    End;
    if iClass <> 0 Then
    Begin
      inc(iCount);
      oBuilder.StartRowFlip(iCount);
      oBuilder.AddTableCell('Class');
      oBuilder.AddTableCell(GetConceptDesc(iClass));
      oBuilder.EndTableRow;
    End;
    if iv2dt <> 0 Then
    Begin
      inc(iCount);
      oBuilder.StartRowFlip(iCount);
      oBuilder.AddTableCell('v2 Data Type');
      oBuilder.AddTableCell(GetConceptDesc(iv2dt));
      oBuilder.EndTableRow;
    End;
    if iv3dt <> 0 Then
    Begin
      inc(iCount);
      oBuilder.StartRowFlip(iCount);
      oBuilder.AddTableCell('v3 Data Type');
      oBuilder.AddTableCell(GetConceptDesc(iv3dt));
      oBuilder.EndTableRow;
    End;

    inc(iCount);
    oBuilder.StartRowFlip(iCount);
    oBuilder.AddTableCell('Type');
    if iFlags and FLAGS_CLIN > 0 Then
      oBuilder.AddTableCell('Clinical')
    Else if iFlags and FLAGS_ATT > 0 Then
      oBuilder.AddTableCell('Attachment')
    Else if iFlags and FLAGS_SURV > 0 Then
      oBuilder.AddTableCell('Survey')
    Else
      oBuilder.AddTableCell('Lab');
    oBuilder.EndTableRow;

    inc(iCount);
    oBuilder.StartRowFlip(iCount);
    oBuilder.AddTableCell('Status');
    if iFlags and FLAGS_HOLD > 0 Then
      oBuilder.AddTableCell('Not yet final')
    Else
      oBuilder.AddTableCell('Final');
    oBuilder.EndTableRow;

    if iFlags and FLAGS_ROOT > 0 Then
    Begin
      inc(iCount);
      oBuilder.StartRowFlip(iCount);
      oBuilder.AddTableCell('Root');
      oBuilder.AddTableCell('This is a root of a set');
      oBuilder.EndTableRow;
    End;

    if iFlags and FLAGS_UNITS > 0 Then
    Begin
      inc(iCount);
      oBuilder.StartRowFlip(iCount);
      oBuilder.AddTableCell('Units');
      oBuilder.AddTableCell('Units are required');
      oBuilder.EndTableRow;
    End;

    inc(iCount);
    oBuilder.StartRowFlip(iCount);
    oBuilder.AddTableCell('Order/Obs Status');
    if (iFlags and FLAGS_ORDER> 0 ) and (iFlags and FLAGS_OBS> 0 ) Then
      oBuilder.AddTableCell('Both')
    Else if iFlags and FLAGS_ORDER > 0 Then
      oBuilder.AddTableCell('Order')
    Else if iFlags and FLAGS_OBS > 0 Then
      oBuilder.AddTableCell('Observation')
    Else
      oBuilder.AddTableCell('');
    oBuilder.EndTableRow;

    oBuilder.EndTable;

    oBuilder.AddParagraph('');
    if iOtherNames <> 0 Then
    Begin
      oBuilder.StartParagraph;
      oBuilder.AddText('Other Names', true, false);
      oBuilder.EndParagraph;
      iRefs := FLoinc.Refs.GetCardinals(iOtherNames);
      oBuilder.StartTable;
      for i := Low(iRefs) To High(iRefs) Do
        if iRefs[i] <> 0 Then
        begin
          oBuilder.StartRowFlip(i);
          oBuilder.AddTableCell(FLoinc.desc.GetEntry(iRefs[i]));
          oBuilder.EndTableRow;
        End;
      oBuilder.EndTable;
    End;
  End
  Else
    oBuilder.AddParagraph('Unable to find code '+sCode);
end;

procedure TloincPublisher.PublishConcept(bRoot : Boolean; const sPrefix, sId: String; iStart : Integer; oBuilder: THL7V2DocumentPublisher);
var
  aChildren : TWordArray;
  aCodes : TCardinalArray;
  iName : Cardinal;
  iChildren : Cardinal;
  iCodes : Cardinal;
  i : Integer;
  iDummy : Cardinal;
  b2, b3, b4 : Boolean;

  iDescription, iOtherNames, iStems : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;

begin
  aChildren := nil;
  aCodes := nil;
  FLoinc.Concepts.GetConcept(StrToInt(sId), iName, iChildren, iCodes);
  if Not bRoot then
    oBuilder.AddTitle('LOINC Concept '+FLoinc.Desc.GetEntry(iName));

  b2 := false;
  b3 := false;
  b4 := false;
  if iChildren <> 0 Then
  begin
    aChildren := FLoinc.Refs.GetWords(iChildren);
    oBuilder.StartTable.BorderPolicy := BorderPolicyNone;
    oBuilder.StartTableRow;
    oBuilder.StartTableCell;
    oBuilder.AddParagraph(' ');
    oBuilder.EndTableCell;
    oBuilder.StartTableCell;
    For i := iStart to Min(iStart+MAX_ROWS, High(aChildren)) Do
    Begin
      if not b2 And (Length(aChildren) > 20) And ((i - iStart) / Min(MAX_ROWS, Length(aChildren)) > 0.25) Then
      Begin
        oBuilder.EndTableCell;
        oBuilder.StartTableCell;
        b2 := true;
      End;
      if not b3 And (Length(aChildren) > 20) And ((i - iStart) / Min(MAX_ROWS, Length(aChildren)) > 0.5) Then
      Begin
        oBuilder.EndTableCell;
        oBuilder.StartTableCell;
        b3 := true;
      End;
      if not b4 And (Length(aChildren) > 20) And ((i - iStart) / Min(MAX_ROWS, Length(aChildren)) > 0.750) Then
      Begin
        oBuilder.EndTableCell;
        oBuilder.StartTableCell;
        b4 := true;
      End;
      FLoinc.Concepts.GetConcept(aChildren[i], iName, iChildren, iDummy);
      oBuilder.ParaURL(FLoinc.Desc.GetEntry(iName), sPrefix + 'id='+inttostr(aChildren[i]));
    End;
    oBuilder.EndTableCell;
    oBuilder.EndTableRow;
    oBuilder.EndTable;
    if (iStart > 0) or (iStart+MAX_ROWS < High(aChildren)) Then
    Begin
      oBuilder.StartParagraph;
      if iStart > 0 Then
      Begin
        oBuilder.URL('Start', sPrefix+'id='+sId);
        oBuilder.AddTextPlain(' ');
      End;
      if iStart > MAX_ROWS Then
      Begin
        oBuilder.URL('Previous', sPrefix+'id='+sId+'&start='+inttostr(iStart - MAX_ROWS));
        oBuilder.AddTextPlain(' ');
      End;
      oBuilder.AddText('Page '+ inttostr((iStart div MAX_ROWS) + 1)+' of '+inttostr(High(aChildren) div MAX_ROWS + 1), true, false);
      if (iStart+MAX_ROWS < High(aChildren)) And (iStart + MAX_ROWS <  MAX_ROWS * (High(aChildren) div MAX_ROWS)) Then
      Begin
        oBuilder.AddTextPlain(' ');
        oBuilder.URL('Next', sPrefix+'id='+sId+'&start='+inttostr(iStart + MAX_ROWS));
      End;
      if (iStart+MAX_ROWS < High(aChildren)) Then
      Begin
        oBuilder.AddTextPlain(' ');
        oBuilder.URL('End', sPrefix+'id='+sId+'&start='+inttostr(MAX_ROWS * (High(aChildren) div MAX_ROWS)));
      End;
      oBuilder.EndParagraph;
    End;
  End;

  b2 := false;
  b3 := false;
  b4 := false;
  if iCodes <> 0 Then
  begin
    aCodes := FLoinc.Refs.GetCardinals(iCodes);
    oBuilder.StartTable.BorderPolicy := BorderPolicyNone;
    oBuilder.StartTableRow;
    oBuilder.StartTableCell;
    oBuilder.AddParagraph(' ');
    oBuilder.EndTableCell;
    oBuilder.StartTableCell;
    For i := iStart to Min(iStart+MAX_ROWS, High(aCodes)) Do
    Begin
      if not b2 And (Length(aCodes) > 20) And ((i - iStart) / Min(MAX_ROWS, Length(aCodes)) > 0.25) Then
      Begin
        oBuilder.EndTableCell;
        oBuilder.StartTableCell;
        b2 := true;
      End;
      if not b3 And (Length(aCodes) > 20) And ((i - iStart) / Min(MAX_ROWS, Length(aCodes)) > 0.5) Then
      Begin
        oBuilder.EndTableCell;
        oBuilder.StartTableCell;
        b3 := true;
      End;
      if not b4 And (Length(aCodes) > 20) And ((i - iStart) / Min(MAX_ROWS, Length(aCodes)) > 0.750) Then
      Begin
        oBuilder.EndTableCell;
        oBuilder.StartTableCell;
        b4 := true;
      End;
      FLoinc.Code.GetInformation(aCodes[i], sCode1, iDescription, iOtherNames, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);
      oBuilder.StartParagraph;
      oBuilder.URL(sCode1, sPrefix + 'code='+sCode1);
      oBuilder.AddTextPlain(': '+FLoinc.Desc.GetEntry(iDescription));
      oBuilder.EndParagraph;
    End;
    oBuilder.EndTableCell;
    oBuilder.EndTableRow;
    oBuilder.EndTable;
    if (iStart > 0) or (iStart+MAX_ROWS < High(aCodes)) Then
    Begin
      oBuilder.StartParagraph;
      if iStart > 0 Then
      Begin
        oBuilder.URL('Start', sPrefix+'id='+sId);
        oBuilder.AddTextPlain(' ');
      End;
      if iStart > MAX_ROWS Then
      Begin
        oBuilder.URL('Previous', sPrefix+'id='+sId+'&start='+inttostr(iStart - MAX_ROWS));
        oBuilder.AddTextPlain(' ');
      End;
      oBuilder.AddText('Page '+ inttostr((iStart div MAX_ROWS) + 1)+' of '+inttostr(High(aCodes) div MAX_ROWS + 1), true, false);
      if (iStart+MAX_ROWS < High(aCodes)) And (iStart + MAX_ROWS <  MAX_ROWS * (High(aCodes) div MAX_ROWS)) Then
      Begin
        oBuilder.AddTextPlain(' ');
        oBuilder.URL('Next', sPrefix+'id='+sId+'&start='+inttostr(iStart + MAX_ROWS));
      End;
      if (iStart+MAX_ROWS < High(aCodes)) Then
      Begin
        oBuilder.AddTextPlain(' ');
        oBuilder.URL('End', sPrefix+'id='+sId+'&start='+inttostr(MAX_ROWS * (High(aCodes) div MAX_ROWS)));
      End;
      oBuilder.EndParagraph;
    End;
  End;
end;

procedure TloincPublisher.PublishCodes(const sPrefix: String; iStart: Integer; oBuilder: THL7V2DocumentPublisher);
var
  i : Integer;
  iTotal : Integer;
  b2, b3, b4 : Boolean;

  iDescription, iOtherNames, iStems : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;
begin
  b2 := false;
  b3 := false;
  b4 := false;

    oBuilder.StartTable.BorderPolicy := BorderPolicyNone;
    oBuilder.StartTableRow;
    oBuilder.StartTableCell;
    oBuilder.AddParagraph(' ');
    oBuilder.EndTableCell;
    oBuilder.StartTableCell;
    iTotal := FLoinc.Code.Count;
    For i := iStart to Min(iStart+MAX_ROWS, iTotal) Do
    Begin
      if not b2 And ((i - iStart) / Min(MAX_ROWS, iTotal) > 0.25) Then
      Begin
        oBuilder.EndTableCell;
        oBuilder.StartTableCell;
        b2 := true;
      End;
      if not b3 And ((i - iStart) / Min(MAX_ROWS, iTotal) > 0.5) Then
      Begin
        oBuilder.EndTableCell;
        oBuilder.StartTableCell;
        b3 := true;
      End;
      if not b4 And ((i - iStart) / Min(MAX_ROWS, iTotal) > 0.750) Then
      Begin
        oBuilder.EndTableCell;
        oBuilder.StartTableCell;
        b4 := true;
      End;
      FLoinc.Code.GetInformation(i, sCode1, iDescription, iOtherNames, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);
      oBuilder.StartParagraph;
      oBuilder.URL(sCode1, sPrefix + 'code='+sCode1);
      oBuilder.AddTextPlain(': '+FLoinc.Desc.GetEntry(iDescription));
      oBuilder.EndParagraph;
    End;
    oBuilder.EndTableCell;
    oBuilder.EndTableRow;
    oBuilder.EndTable;
    if (iStart > 0) or (iStart+MAX_ROWS < iTotal) Then
    Begin
      oBuilder.StartParagraph;
      if iStart > 0 Then
      Begin
        oBuilder.URL('Start', sPrefix+'code=*');
        oBuilder.AddTextPlain(' ');
      End;
      if iStart > MAX_ROWS Then
      Begin
        oBuilder.URL('Previous', sPrefix+'code=*&start='+inttostr(iStart - MAX_ROWS));
        oBuilder.AddTextPlain(' ');
      End;
      oBuilder.AddText('Page '+ inttostr((iStart div MAX_ROWS) + 1)+' of '+inttostr(iTotal div MAX_ROWS + 1), true, false);
      if (iStart+MAX_ROWS < iTotal) And (iStart + MAX_ROWS <  MAX_ROWS * (iTotal div MAX_ROWS)) Then
      Begin
        oBuilder.AddTextPlain(' ');
        oBuilder.URL('Next', sPrefix+'code=*&start='+inttostr(iStart + MAX_ROWS));
      End;
      if (iStart+MAX_ROWS < iTotal) Then
      Begin
        oBuilder.AddTextPlain(' ');
        oBuilder.URL('End', sPrefix+'code=*&start='+inttostr(MAX_ROWS * (iTotal div MAX_ROWS)));
      End;
      oBuilder.EndParagraph;
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

procedure TloincPublisher.PublishSearch(const sPrefix, sText: String; iStart: Integer; oBuilder: THL7V2DocumentPublisher);
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
  oBuilder.StartTable.BorderPolicy := BorderPolicyNone;
  oBuilder.StartTableRow;
  oBuilder.AddTableCell('Code');
  oBuilder.AddTableCell('Description');
  oBuilder.AddTableCell('Component');
  oBuilder.AddTableCell('Property');
  oBuilder.AddTableCell('Time Aspect');
  oBuilder.AddTableCell('System');
  oBuilder.AddTableCell('Scale');
  oBuilder.AddTableCell('Method');
  oBuilder.AddTableCell('Class');
  oBuilder.AddTableCell('Rating');
  oBuilder.EndTableRow;
  iTotal := Length(a)-1;


  For i := iStart to Min(iStart+MAX_ROWS, iTotal) Do
  Begin
    FLoinc.Code.GetInformation(a[i].index, sCode1, iDescription, iOtherNames, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);

    oBuilder.StartTableRow;
    oBuilder.AddTableCellURL(sCode1, sPrefix + 'code='+sCode1);
    oBuilder.AddTableCell(FLoinc.Desc.GetEntry(iDescription));
    oBuilder.AddTableCell(GetConceptDesc(iComponent));
    oBuilder.AddTableCell(GetConceptDesc(iProperty));
    oBuilder.AddTableCell(GetConceptDesc(iTimeAspect));
    oBuilder.AddTableCell(GetConceptDesc(iSystem));
    oBuilder.AddTableCell(GetConceptDesc(iScale));
    oBuilder.AddTableCell(GetConceptDesc(iMethod));
    oBuilder.AddTableCell(GetConceptDesc(iClass));
    oBuilder.AddTableCell(RealToString(a[i].Priority));
    oBuilder.EndTableRow;
    End;
  oBuilder.EndTable;
  if (iStart > 0) or (iStart+MAX_ROWS < iTotal) Then
  Begin
    oBuilder.StartParagraph;
    if iStart > 0 Then
    Begin
      oBuilder.URL('Start', sPrefix+'srch='+sText);
      oBuilder.AddTextPlain(' ');
    End;
    if iStart > MAX_ROWS Then
    Begin
      oBuilder.URL('Previous', sPrefix+'srch='+sText+'&start='+inttostr(iStart - MAX_ROWS));
      oBuilder.AddTextPlain(' ');
    End;
    oBuilder.AddText('Page '+ inttostr((iStart div MAX_ROWS) + 1)+' of '+inttostr(iTotal div MAX_ROWS + 1), true, false);
    if (iStart+MAX_ROWS < iTotal) And (iStart + MAX_ROWS <  MAX_ROWS * (iTotal div MAX_ROWS)) Then
    Begin
      oBuilder.AddTextPlain(' ');
      oBuilder.URL('Next', sPrefix+'srch='+sText+'&start='+inttostr(iStart + MAX_ROWS));
    End;
    if (iStart+MAX_ROWS < iTotal) Then
    Begin
      oBuilder.AddTextPlain(' ');
      oBuilder.URL('End', sPrefix+'srch='+sText+'&start='+inttostr(MAX_ROWS * (iTotal div MAX_ROWS)));
    End;
    oBuilder.EndParagraph;
  End;
end;

End.

