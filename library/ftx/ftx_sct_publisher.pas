unit ftx_sct_publisher;

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

{$I fhir.inc}

Interface

Uses
  SysUtils, Classes, Math,
  fsl_utilities, fsl_threads,
  fsl_base, fsl_collections,
  fsl_htmlgen, ftx_sct_services;

Const
  MAX_ROWS = 100;

Type
  TIdArray = array of cardinal;
  TArrayofIdArray = array of TIdArray;

  TConceptDisplayType = (cdDesc, cdConceptId, cdBoth);

  TSnomedPublisher = class (TFslObject)
  Private
    FSnomed : TSnomedServices;
    Lock : TFslLock;
    FSearchCache : TStringList;
    FFHIRPath : String;
    Function GetPaths(iIndex : Cardinal) : TArrayofIdArray;
    Function ConceptForDesc(iDesc : Cardinal; var iDescs : Cardinal):Cardinal;
    Procedure ConceptRef(html : THtmlPublisher; const sPrefix : String; iIndex : cardinal; show : TConceptDisplayType; rRating : Double);
    Procedure RefsetRef(html : THtmlPublisher; Const sPrefix : String; iIndex : cardinal);
    Procedure CellConceptRef(html : THtmlPublisher; const sPrefix : String; iIndex : cardinal; show : TConceptDisplayType; iDesc : Cardinal = 0);

    Procedure SortRefsets(var a : TCardinalArray);
    Function GetConceptForRefset(iRefset : Cardinal) : Cardinal;
    procedure PublishPaths(html: THtmlPublisher; Const sPrefix : String; aPaths : TArrayofIdArray; iFocus : Cardinal; iIndent, iStart, iLow, iHigh : Integer);

    Procedure PublishConcept(bRoot : Boolean; showhist : boolean; Const sPrefix, sId : String; iStart : Integer; html : THtmlPublisher);
    Procedure PublishTermConcept(bRoot : Boolean; Const sPrefix, sId : String; iStart : Integer; html : THtmlPublisher);
    Procedure PublishConcepts(Const sPrefix : String; iStart : Integer; html : THtmlPublisher);
    Procedure PublishSearch(Const sPrefix, sText, sContext : String; iStart: Integer; all : boolean; html : THtmlPublisher);
    Procedure PublishHome(Const sPrefix : String; html : THtmlPublisher);

    Procedure ProcessMap(Const sPath : String; oMap : TFslStringMatch);
    Procedure PublishDictInternal(oMap : TFslStringMatch; Const sPrefix : String; html : THtmlPublisher);
  Public
    constructor Create(oSnomed : TSnomedServices; FHIRPathEngine : String);
    destructor Destroy; Override;
    Procedure PublishDict(Const sPath, sPrefix : String; html : THtmlPublisher); Overload; Virtual;
    Procedure PublishDict(oMap : TFslStringMatch; Const sPrefix : String; html : THtmlPublisher); Overload; Virtual;
    Procedure PublishTerm(Const sTerm : String; html : THtmlPublisher); Overload; Virtual;
  End;


Implementation


Function Screen(Const s, s2: String):String;
Begin
  result := StringReplace(s, 'B', '');
  if (s2 <> '') And StringEndsWith(result, s2) Then
    delete(result, length(result) - length(s2) + 1, length(s));
End;

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

Procedure TSnomedPublisher.PublishDictInternal(oMap : TFslStringMatch; Const sPrefix : String; html : THtmlPublisher);
Var
  sURL : String;
  sId : String;
Begin
  sURL := sPrefix +'?type=snomed&';

  sId := oMap.Matches['id'];
  If sId = '*' Then
    PublishConcepts(sURL, StrToIntDef(oMap.Matches['start'], 0), html)
  else If (sId <> '')  Then
    PublishConcept(false, oMap['no-hist'] <> 'true', sURL, sId, StrToIntDef(oMap.Matches['start'], 0), html)
  else if oMap.ExistsByKey('srch') then
    if StringIsInteger64(oMap.Matches['srch']) and ((oMap.Matches['context'] = '') or (FSnomed.Subsumes(oMap.Matches['context'], oMap.Matches['srch']))) then
      PublishConcept(false, oMap['no-hist'] <> 'true', sURL, oMap.Matches['srch'], StrToIntDef(oMap.Matches['start'], 0), html)
    else
      PublishSearch(sURL, oMap.Matches['srch'], oMap.Matches['context'], StrToIntDef(oMap.Matches['start'], 0), StringToBoolDef(oMap.matches['all'], false), html)
  else
    PublishHome(sURL, html)
End;

Procedure TSnomedPublisher.ProcessMap(Const sPath : String; oMap : TFslStringMatch);
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


Procedure TSnomedPublisher.PublishDict(Const sPath, sPrefix : String; html : THtmlPublisher);
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

procedure TSnomedPublisher.PublishDict(oMap: TFslStringMatch; const sPrefix: String; html: THtmlPublisher);
begin
  Try
    PublishDictInternal(oMap, sPrefix, html);
  Except
    On e:Exception Do
      Begin
      html.AddTitle('Exception: '+e.Message);
      html.AddParagraph(e.Message);
      End;
  End;
end;

procedure TSnomedPublisher.PublishHome(const sPrefix: String; html: THtmlPublisher);
var
  i : integer;
  iRef : Cardinal;
  aRefs : TCardinalArray;
Begin
{  html.StartParagraph;
  if oMap.ExistsByKey('id') Or oMap.ExistsByKey('srch') Then
  Begin
    html.AddTextPlain('Snomed-CT: ');
    html.URL(FSnomed.Version, sURL);
  End
  Else
    html.AddTextPlain('Snomed: '+FSnomed.Version);
  html.EndParagraph;
  html.AddLine(1);
}

  html.Heading(1, 'Snomed-CT Definitions (e: '+FSnomed.EditionName+', v: '+FSnomed.VersionDate+')');
  if Not FSnomed.Loaded Then
  Begin
    html.StartParagraph;
    html.AddText('Snomed Definitions are not loaded', true, false);
    html.EndParagraph;
  End
  Else
  Begin
    html.StartForm('GET', sPrefix);
    html.StartParagraph;
    html.AddTextPlain('Search: ');
    html.textInput('srch');
    html.submit('Go');
    html.AddTextPlain(' ');
    html.checkbox('all', false, 'Tight');
    html.endForm;

    html.StartList;
    html.StartListItem;
    html.URL('Browse All Concepts', sPrefix+'id=*');
    html.EndListItem;
    html.EndList;

    if Length(FSnomed.ActiveRoots) = 1 Then
    begin
      html.StartParagraph;
      html.AddText('Snomed Root Concept', true, false);
      html.EndParagraph;
      PublishConcept(true, true, sPrefix, inttostr(FSnomed.Activeroots[0]), 0, html)
    End
    Else
    Begin
      html.StartParagraph;
      html.AddText('Snomed Root Concepts ('+inttostr(Length(FSnomed.ActiveRoots))+')', true, false);
      html.EndParagraph;
      html.StartList;
      for i := 0 to min(Length(FSnomed.ActiveRoots) - 1, 1000) Do
      Begin
        html.StartListItem;
        if FSnomed.Concept.FindConcept(FSnomed.ActiveRoots[i], iRef) Then
          ConceptRef(html, sPrefix, iRef, cdBoth, 0);
        html.EndListItem;
      End;
      html.EndList;
    End;

    html.Line;
    html.StartParagraph;
    html.AddText('Reference Sets', true, false);
    html.EndParagraph;
    SetLength(aRefs, FSnomed.RefSetIndex.Count);
    for i := 0 to FSnomed.RefSetIndex.Count - 1 Do
      aRefs[i] := i;
    SortRefsets(aRefs);
    if FSnomed.RefSetIndex.Count = 0 Then
    Begin
      html.StartParagraph;
      html.AddText('No Reference Sets defined', false, true);
      html.EndParagraph;
    End;
    html.StartList;
    for i := 0 to FSnomed.RefSetIndex.Count - 1 Do
    Begin
      html.StartListItem;
      RefsetRef(html, sPrefix, aRefs[i]);
      html.EndListItem;
    End;
    html.EndList;

    html.Line;
    Lock.Lock;
    Try
      if FSearchCache.Count <> 0 Then
      Begin
        html.AddParagraph('Past Searches:');
        html.StartList(false);
        For i := 0 to FSearchCache.Count - 1 Do
        begin
          html.StartListItem;
          html.ParaURL('Search for "'+FSearchCache[i]+'"', sPrefix+'srch='+FSearchCache[i]+'&caption=Search Snomed Concepts&prompt=Text');
          html.EndListItem;
        end;
        html.EndList(false);
      End;
    Finally
      Lock.UnLock;
    End;
  End;
  html.done;
End;

Procedure TSnomedPublisher.SortRefsets(var a : TCardinalArray);

  Procedure QuickSort(L, R: Integer);
  Var
    I, J, K : Integer;
    t : Cardinal;
  Begin
    // QuickSort routine (Recursive)
    // * Items is the default indexed property that returns a pointer, subclasses
    //   specify these return values as their default type.
    // * The Compare routine used must be aware of what this pointer actually means.

    Repeat
      I := L;
      J := R;
      K := (L + R) Shr 1;

      Repeat
        While FSnomed.GetPNForConcept(GetConceptForRefset(a[I])) < FSnomed.GetPNForConcept(GetConceptForRefset(a[K])) Do
          Inc(I);

        While FSnomed.GetPNForConcept(GetConceptForRefset(a[J])) > FSnomed.GetPNForConcept(GetConceptForRefset(a[K])) Do
          Dec(J);

        If I <= J Then
        Begin
          t := a[i];
          a[i] := a[j];
          a[j] := t;

          // Keep K as the index of the original middle element as it might get exchanged.
          If I = K Then
            K := J
          Else If J = K Then
            K := I;

          Inc(I);
          Dec(J);
        End;
      Until I > J;

      If L < J Then
        QuickSort(L, J);

      L := I;
    Until I >= R;
  End;

Begin
  If length(a) > 1 Then
    QuickSort(0, length(a) - 1);
End;


Function TSnomedPublisher.GetPaths(iIndex : Cardinal): TArrayofIdArray;
var
  iParentIndex : Cardinal;
  iParents : TCardinalArray;
  aPath : TArrayofIdArray;
  i,j,k : integer;
begin
  SetLength(result, 0);
  SetLength(aPath, 0);
  SetLength(iParents, 0);
  iParentIndex := FSnomed.Concept.GetParent(iIndex);
  if iParentIndex = 0 then
  Begin
    SetLength(result, 1);
    SetLength(result[0], 1);
    result[0][0] := iIndex;
  End
  Else
  begin
    iParents := FSnomed.Refs.GetReferences(iParentIndex);
    for i := Low(iParents) To High(iParents) Do
    Begin
      aPath := GetPaths(iParents[i]);
      for j := Low(aPath) To High(aPath) Do
      Begin
        SetLength(result, Length(result)+1);
        SetLength(result[Length(result)-1], length(aPath[j])+1);
        For k := Low(aPath[j]) To High(aPath[j]) Do
          result[Length(result)-1][k] := aPath[j][k];
        result[Length(result)-1][length(aPath[j])] := iIndex;
      End;
    End;
  End;
End;


Function GetRelGroup(iGroup : cardinal):String;
Begin
  if iGroup = 0 Then
    result := ''
  Else
    result := inttostr(iGroup);
End;

Procedure TSnomedPublisher.ConceptRef(html : THtmlPublisher; const sPrefix : String; iIndex : cardinal; show : TConceptDisplayType; rRating : Double);
Begin
  if show = cdBoth Then
    html.URL(inttostr(FSnomed.Concept.GetIdentity(iIndex))+' '+Screen(FSnomed.GetPNForConcept(iIndex), ''), sPrefix+'id='+inttostr(FSnomed.Concept.GetIdentity(iIndex)))
  Else if show = cdDesc then
    html.URL(Screen(FSnomed.GetPNForConcept(iIndex), ''), sPrefix+'id='+inttostr(FSnomed.Concept.GetIdentity(iIndex)))
  else
    html.URL(inttostr(FSnomed.Concept.GetIdentity(iIndex)), sPrefix+'id='+inttostr(FSnomed.Concept.GetIdentity(iIndex)), Screen(FSnomed.GetPNForConcept(iIndex), ''));
  if rRating > 0 then
    html.AddTextPlain(' '+inttostr(Trunc(rRating * 10)));
End;

Procedure TSnomedPublisher.CellConceptRef(html : THtmlPublisher; const sPrefix : String; iIndex : cardinal; show : TConceptDisplayType; iDesc : Cardinal = 0);
var
  s : String;
Begin
  if iDesc <> 0 Then
    s := FSnomed.Strings.GetEntry(iDesc)
  Else
    s := FSnomed.GetPNForConcept(iIndex);

  if show = cdBoth Then
    html.AddTableCellURL(inttostr(FSnomed.Concept.GetIdentity(iIndex))+' '+Screen(s, ''), sPrefix+'id='+inttostr(FSnomed.Concept.GetIdentity(iIndex)))
  Else if show = cdDesc then
    html.AddTableCellURL(Screen(s, ''), sPrefix+'id='+inttostr(FSnomed.Concept.GetIdentity(iIndex)))
  Else
    html.AddTableCellURL(inttostr(FSnomed.Concept.GetIdentity(iIndex)), sPrefix+'id='+inttostr(FSnomed.Concept.GetIdentity(iIndex)), Screen(s, ''))
End;

Function ComparePaths(p1, p2: TIdArray) : Integer;
var
  i : Integer;
Begin
  result := 0;
  for i := 0 to Min(High(p1), High(p2)) Do
    if result = 0 Then
      result := IntegerCompare(p1[i], p2[i]);
  if result = 0 Then
    result := IntegerCompare(Length(p1), length(p2));
End;

Procedure SortPaths(Var Paths : TArrayofIdArray);
var
  i : Integer;
  t : TIdArray;
  bSwap : Boolean;
Begin
  SetLength(t, 0);
  Repeat
    bSwap := false;
    for i := 0 to high(Paths)-1 Do
      if ComparePaths(Paths[i], paths[i+1]) < 0 Then
      Begin
        t := Paths[i];
        Paths[i] := Paths[i+1];
        Paths[i+1] := t;
        bSwap := true;
      End;
  Until not bSwap;
End;

procedure TSnomedPublisher.PublishPaths(html: THtmlPublisher; Const sPrefix : String; aPaths : TArrayofIdArray; iFocus : Cardinal; iIndent, iStart, iLow, iHigh : Integer);
var
  iCommon, iLoop, i, j : Integer;
  iValue : Cardinal;
  bOk : Boolean;
Begin
  html.StartList;
  if iLow < iHigh Then
  Begin
    iCommon := iStart-1;
    repeat
      bOk := false;
      iLoop := iLow + 1;
      if Length(aPaths[iLow]) > iCommon + 1 Then
      Begin
        bOk := true;
        iValue := aPaths[iLow][iCommon+1];
        while bOk and (iLoop <= iHigh) Do
        Begin
          bOk := (Length(aPaths[iLoop]) > iCommon + 1) And (iValue = aPaths[iLoop][iCommon+1]);
          inc(iLoop);
        End;
        if bOk Then
          inc(iCommon);
      End;
    Until not bOk;
    if iCommon < iStart then
      iCommon := iStart;
  End
  else
    iCommon := iStart;

  if iCommon > iStart Then
  Begin
    html.StartListItem;
    For j := iStart to iCommon Do
    Begin
      if j > 0 Then
        html.AddTextPlain('\');
      if aPaths[iLow][j] = iFocus Then
        html.AddText(Screen(FSnomed.GetPNForConcept(iFocus), ''), false, true)
      Else
        ConceptRef(html, sPrefix, aPaths[iLow][j], cdDesc, 0);
    End;
    html.AddTextPlain('\...');
    html.EndListItem;

    // now, can we find any child groups here?
    iLoop := iLow;
    while (iLoop <= iHigh) Do
    Begin
      iValue := aPaths[iLoop][iCommon+1];
      i := iLoop;
      While (iLoop < iHigh) And (aPaths[iLoop+1][iCommon+1] = iValue) Do
        inc(iLoop);
      PublishPaths(html, sPrefix, aPaths, iFocus, iIndent + 1, iCommon+1, i, iLoop);
      inc(iLoop);
    End;
  End
  Else
  Begin
    for i := iLow To iHigh do
    Begin
      html.StartListItem;
      For j := iStart To High(aPaths[i]) Do
      Begin
        if j > 0 Then
          html.AddTextPlain('\');
        if aPaths[i][j] = iFocus Then
          html.AddText(Screen(FSnomed.GetPNForConcept(iFocus), ''), false, true)
        Else
          ConceptRef(html, sPrefix, aPaths[i][j], cdDesc, 0);
      End;
      html.EndListItem;
    End;
  End;
  html.EndList;
End;

function DescConceptFlags(flags : byte; bPrimitive : boolean):String;
begin
  case flags and MASK_CONCEPT_STATUS of
    0:  result := 'current';
    1:  result := 'retired';
    2:  result := 'duplicate';
    3:  result := 'outdated';
    4:  result := 'ambiguous';
    5:  result := 'erroneous';
    6:  result := 'limited';
    10: result := 'moved elsewhere';
    11: result := 'pending move';
  else
    result := '??';
  end;
  if bPrimitive and(flags and MASK_CONCEPT_PRIMITIVE > 0) then
    result := result + ', primitive';
end;


procedure TSnomedPublisher.PublishConcept(bRoot : Boolean; showhist : boolean; const sPrefix, sId: String; iStart : Integer; html: THtmlPublisher);
var
  iId : UInt64;
  iIndex : Cardinal;
  Identity : UInt64;
  Flags, lang : Byte;
  Group : integer;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  Parents : TCardinalArray;
  Descriptions : TCardinalArray;
  Inbounds : TCardinalArray;
  outbounds : TCardinalArray;
  allDesc : TCardinalArray;
  Active, Defining : boolean;
  iWork, iWork2, iWork3, kind, caps, module, refsets, valueses, modifier : Cardinal;
  FSN : String;
  PN : String;
  FPaths : TArrayofIdArray;
  i, j : integer;
  iList : TRefSetMemberEntryArray;
  iDummy, iRefSet, iMembers, iDescs, children, iTypes, iName, iFields : Cardinal;
  types, fields, values : TCardinalArray;
  aMembers : TSnomedReferenceSetMemberArray;
  date : TSnomedDate;
  ok : boolean;
  did : UInt64;
Begin
  SetLength(aMembers, 0);
  SetLength(iList, 0);
  SetLength(alLDesc, 0);
  iId := StrToUInt64Def(sId, 0);
  ok := FSnomed.Concept.FindConcept(iId, iIndex);
  if not ok then
  begin
    ok := FSnomed.DescRef.FindDescription(iId, iIndex);
    if ok then
      iIndex := FSnomed.Desc.ConceptByIndex(iIndex);
  end;
  if not ok Then
  Begin
    html.Heading(1, 'Snomed Concept '+sId);
    html.AddParagraph(sId+' is not a valid Snomed-CT Concept or Description Id');
    SetLength(FPaths, 0);
    SetLength(Parents, 0);
    SetLength(Descriptions, 0);
    SetLength(Inbounds, 0);
    SetLength(outbounds, 0);
    html.ParaURL('Back to Snomed Home', sPrefix);
  End
  else
  Begin
    FSnomed.Concept.GetConcept(iIndex, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
    if ParentIndex <> 0 Then
      Parents := FSnomed.Refs.GetReferences(ParentIndex);
    Descriptions := FSnomed.Refs.GetReferences(DescriptionIndex);
    outbounds := FSnomed.Refs.GetReferences(outboundIndex);
    FSN := FSnomed.GetFSN(Descriptions);
    PN := FSnomed.GetPN(Descriptions);
    if Not bRoot then
      html.Heading(1, inttostr(Identity)+': '+screen(FSN, ''));
    if not bRoot Then
    Begin
      FPaths := GetPaths(iIndex);
      SortPaths(FPaths);
      PublishPaths(html, sPrefix, FPaths, iIndex, 0, 0, Low(FPaths), High(FPaths));
      html.Line;
    End;
    html.StartParagraph;
    iDummy := FSnomed.Concept.GetStatus(iIndex);
    html.AddTextPlain('Status: '+DescConceptFlags(flags, iDummy = 0));
    if iDummy <> 0 then
    begin
      html.AddTextPlain(', ');
      ConceptRef(html, sPrefix, iDummy, cdDesc, 0);
    end;
    html.AddTextPlain('. Date: '+FormatDateTime('dd-mmm yyyy', date));
    iDummy := FSnomed.Concept.GetModuleId(iIndex);
    if iDummy <> 0 then
    begin
      html.AddTextPlain('. Module: ');
      ConceptRef(html, sPrefix, iDummy, cdDesc, 0);
    end;
    html.EndParagraph;

    // todo: flags
    html.AddParagraph('Descriptions:');
    html.StartTable(true, 'lines');
    html.StartTableRow;
    html.AddTableCell('Id', true);
    html.AddTableCell('Description', true);
    html.AddTableCell('Lang', true);
    html.AddTableCell('Type', true);
    html.AddTableCell('Status', true);
    html.AddTableCell('Case?', true);
    html.AddTableCell('Module', true);
    if FSnomed.RefSetIndex.Count > 0 Then
      html.AddTableCell('Reference Sets', true);
    html.EndTableRow;
    for i := Low(Descriptions) To High(Descriptions) Do
    Begin
      FSnomed.Desc.GetDescription(Descriptions[i], iWork, iId, date, iDummy, module, kind, caps, refsets, valueses, active, lang);
      if active Then
      Begin
        html.StartRow();
        html.AddTableCell(inttostr(iId));
        html.AddTableCell(Screen(FSnomed.Strings.GetEntry(iWork), ''));
        html.AddTableCell(codeForLang(lang));
        CellConceptRef(html, sPrefix, kind, cdDesc);
        if (active) then
          html.AddTableCell('Active')
        else
          html.AddTableCell('Inactive');
        CellConceptRef(html, sPrefix, caps, cdDesc);
        if (module <> 0) then
          CellConceptRef(html, sPrefix, module, cdDesc)
        else
          html.AddTableCell('');
        if FSnomed.RefSetIndex.Count > 0 Then
        Begin
          iList := FSnomed.GetDescRefsets(Descriptions[i]);
          if Length(ilist) = 0 Then
            html.AddTableCell('')
          Else
          begin
            html.StartTableCell;
            ConceptRef(html, sPrefix, iList[0].refset, cdDesc, 0);
            if (iList[0].types <> 0) and (iList[0].values <> 0) then
            begin
              Values := FSnomed.Refs.GetReferences(iList[0].values);
              if values[1] = 1 then
              begin
                html.AddTextPlain(': ');
                ConceptRef(html, sPrefix, values[0], cdDesc, 0);
              end;
            end;
            html.EndTableCell;
          end;
        End;
//        html.AddTableCell(BooleanToString(flags and MASK_DESC_CAPS > 0));
        html.EndTableRow;
      End;
    End;
    html.EndTable;
    html.Line;

    iRefSet := FSnomed.GetConceptRefSet(iIndex, true, iName, iMembers, itypes, iFields);
    allDesc := FSnomed.Refs.GetReferences(FSnomed.Concept.GetAllDesc(iIndex));
    SetLength(types, 0);
    SetLength(fields, 0);
    html.StartForm('GET', sPrefix);
    html.StartParagraph;

    if iRefSet = 0 then
    begin
      html.AddTextPlain(inttostr(length(alldesc))+' descendants. ');
      children := length(alldesc);
      if children > 0 then
        html.AddTextPlain('Search Descendants: ');
    End
    else
    Begin
      children := FSnomed.RefSetMembers.GetMemberCount(iMembers);
      if (iTypes <> 0) then
      begin
        types := FSnomed.Refs.GetReferences(iTypes);
        fields := FSnomed.Refs.GetReferences(iFields);
      end;
      html.AddTextPlain(inttostr(children)+' members. ');
      if children > 0 then
        html.AddTextPlain('Search Members: ');
    End;
    if children > 0 then
    begin
      html.hiddenInput('context', sid);
      html.textInput('srch');
      html.submit('Go');
    end;
    html.endForm;
    html.StartParagraph;
    if iRefSet = 0 then
      html.URL('Expanded Value Set', FFHIRPath+'ValueSet?_query=expand&identifier=http://snomed.info/id/'+sId)
    else
      html.URL('Expanded Value Set', FFHIRPath+'ValueSet?_query=expand&identifier=http://snomed.info/sct/'+sId);
    html.EndParagraph;
    html.Line;
    if not bRoot and (Length(Outbounds) > 0) Then
    Begin
      html.StartTable(false);
      html.StartTableRow;
      html.AddTableCell('Outbound Relationships', true);
      html.AddTableCell('Type', true);
      html.AddTableCell('Target', true);
      html.AddTableCell('Active', true);
      html.AddTableCell('Characteristic', true);
      html.AddTableCell('Refinability', true);
      html.AddTableCell('Group', true);
      html.AddTableCell('Values', true);
      html.EndTableRow;

      for i := Low(Outbounds) To High(Outbounds) Do
      Begin
        FSnomed.Rel.GetRelationship(Outbounds[i], did, iWork, iWork2, iWork3, module, kind, modifier, date, Active, Defining, Group);
        if (showhist) or active then
        begin
          if not active then
            html.StartRow('#EFEFEF')
          else
            html.StartRow();
          html.AddTableCellHint(Screen(PN, ''), inttostr(did));
          CellConceptRef(html, sPrefix, iWork3, cdDesc);
          CellConceptRef(html, sPrefix, iWork2, cdDesc);
          if (active) then
            html.AddTableCell('true')
          else
            html.AddTableCell('false');
          CellConceptRef(html, sPrefix, kind, cdDesc);
          CellConceptRef(html, sPrefix, modifier, cdDesc);
          html.AddTableCell(' '+GetRelGroup(Group));
          html.AddTableCell(''{FSnomed.getRelationshipValues(Outbounds[i])}); // getRelationshipValues is very expensive
          html.EndTableRow;
        end;
      End;
      html.EndTable;
      html.Line;
    End;

    if iRefset <> 0 Then
    Begin
      aMembers := FSnomed.RefSetMembers.GetMembers(iMembers);
      html.StartTable(false);
      html.StartTableRow;
      html.AddTableCell('Members', true);
      For i := 0 to length(fields)-1 Do
        html.AddTableCell(FSnomed.Strings.GetEntry(fields[i]), true);
      html.EndTableRow;
      For i := iStart to Min(iStart+MAX_ROWS, High(aMembers)) Do
      Begin
        html.StartRow();
        case aMembers[i].kind of
          0 {concept} :
            CellConceptRef(html, sPrefix, aMembers[i].Ref, cdDesc);
          1 {desc} :
            begin
              iDummy := ConceptForDesc(aMembers[i].Ref, iDescs);
              CellConceptRef(html, sPrefix, iDummy, cdDesc, iDescs);
            end;
          2 {relationship} :
            begin
              html.StartTableCell;
              FSnomed.Rel.GetRelationship(aMembers[i].Ref, did, iWork, iWork2, iWork3, module, kind, modifier, date, Active, Defining, Group);
              html.AddTextPlain(' '+inttostr(did)+': ');
              ConceptRef(html, sPrefix, iWork, cdConceptId, 0);
              html.AddTextPlain('---');
              ConceptRef(html, sPrefix, iWork3, cdConceptId, 0);
              html.AddTextPlain('-->');
              ConceptRef(html, sPrefix, iWork2, cdConceptId, 0);
              html.EndTableCell;
            end;
        end;
        if (aMembers[i].values <> 0) then
        begin
          values := FSnomed.Refs.GetReferences(aMembers[i].values);
          for j := 0 to length(types) - 1 do
            case values[j*2+1] of
              1 {concept} : CellConceptRef(html, sPrefix, values[j*2], cdDesc);
//              2:
//              3:
              4 {integer} : html.AddTableCell(inttostr(values[j*2]));
              5 {string} : html.AddTableCell(FSnomed.Strings.GetEntry(values[j*2]));
            else
              html.AddTableCell('Unknown Cell Type '+inttostr(values[j*2+1]));
            end;
        end;
        html.EndTableRow;
      End;
      html.EndTable;
      if (iStart > 0) or (iStart+MAX_ROWS < High(aMembers)) Then
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
        html.AddText('Page '+ inttostr((iStart div MAX_ROWS) + 1)+' of '+inttostr(High(aMembers) div MAX_ROWS + 1), true, false);
        if (iStart+MAX_ROWS < High(aMembers)) And (iStart + MAX_ROWS <  MAX_ROWS * (High(aMembers) div MAX_ROWS)) Then
        Begin
          html.AddTextPlain(' ');
          html.URL('Next', sPrefix+'id='+sId+'&start='+inttostr(iStart + MAX_ROWS));
        End;
        if (iStart+MAX_ROWS < High(aMembers)) Then
        Begin
          html.AddTextPlain(' ');
          html.URL('End', sPrefix+'id='+sId+'&start='+inttostr(MAX_ROWS * (High(aMembers) div MAX_ROWS)));
        End;
        html.EndParagraph;
      End;
    End
    Else
    Begin
      Inbounds := FSnomed.Refs.GetReferences(InboundIndex);
      html.StartTable(false);
      html.StartTableRow;
      html.AddTableCell('Inbound Relationships', true);
      html.AddTableCell('Type', true);
      html.AddTableCell('Active', true);
      html.AddTableCell('Source', true);
      html.AddTableCell('Characteristic', true);
      html.AddTableCell('Refinability', true);
      html.AddTableCell('Group', true);
      html.EndTableRow;
      For i := iStart to Min(iStart+MAX_ROWS, High(Inbounds)) Do
      Begin
        FSnomed.Rel.GetRelationship(Inbounds[i], did, iWork, iWork2, iWork3, module, kind, modifier, date, Active, Defining, Group);
        if (showhist) or (Active) then
        begin
          if not active then
            html.StartRow('#EFEFEF')
          else
            html.StartRow();
          CellConceptRef(html, sPrefix, iWork, cdDesc);
          CellConceptRef(html, sPrefix, iWork3, cdDesc);
          html.AddTableCell(BooleanToString(active));
          html.AddTableCellHint(Screen(PN, ''), inttostr(did));
          CellConceptRef(html, sPrefix, kind, cdDesc);
          CellConceptRef(html, sPrefix, modifier, cdDesc);
          html.AddTableCell(' '+GetRelGroup(Group));
          html.EndTableRow;
        End;
      End;
      html.EndTable;
      if (iStart > 0) or (iStart+MAX_ROWS < High(Inbounds)) Then
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
        html.AddText('Page '+ inttostr((iStart div MAX_ROWS) + 1)+' of '+inttostr(High(Inbounds) div MAX_ROWS + 1), true, false);
        if (iStart+MAX_ROWS < High(Inbounds)) And (iStart + MAX_ROWS <  MAX_ROWS * (High(Inbounds) div MAX_ROWS)) Then
        Begin
          html.AddTextPlain(' ');
          html.URL('Next', sPrefix+'id='+sId+'&start='+inttostr(iStart + MAX_ROWS));
        End;
        if (iStart+MAX_ROWS < High(Inbounds)) Then
        Begin
          html.AddTextPlain(' ');
          html.URL('End', sPrefix+'id='+sId+'&start='+inttostr(MAX_ROWS * (High(Inbounds) div MAX_ROWS)));
        End;
        html.EndParagraph;
      End;
    End;
    if FSnomed.RefSetIndex.count > 0 Then
    Begin
      iList := FSnomed.GetConceptRefsets(iIndex);
      html.Line;
      html.StartParagraph;
      if Length(iList) = 0 Then
        html.AddText('This concept is not in any reference sets', false, true)
      Else
        html.AddText('Reference Sets', true, false);
      html.EndParagraph;
      html.AddParagraph;
      for i := 0 to Length(iList) - 1 Do
      Begin
        html.StartParagraph;
        ConceptRef(html, sPrefix, iList[i].refset, cdDesc, 0);
        html.EndParagraph;
      End;
    End;
    if not bRoot then
    begin
      html.ParaURL('Back to Start', sPrefix);
      html.Done;
    end;
  End;
End;

                                                     (*
function GetConceptDesc(iConcept : Word):String;
var
  iName : Cardinal;
  iChildren : Cardinal;
  iCodes : Cardinal;
Begin
  if iConcept = 0 then
    result := ''
  Else
  Begin
    FSnomed.Concepts.GetConcept(iConcept, iName, iChildren, iCodes);
    result := FSnomed.Desc.GetEntry(iname);
  End;
End;


procedure TSnomedPublisher.PublishCode(const sPrefix, sCode: String; html: THtmlPublisher);
var
  iIndex : Cardinal;
  iDescription, iOtherNames : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;
  iRefs : TCardinalArray;
  i : integer;
  iCount : integer;
Begin
  if FSnomed.Code.FindCode(sCode, iIndex) Then
  Begin
    FSnomed.Code.GetInformation(iIndex, sCode1, iDescription, iOtherNames, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);
    assert(sCode = sCode1);
    html.AddTitle('Snomed Code '+sCode+' : '+FSnomed.Desc.GetEntry(iDescription));
    html.StartTable;
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
      iRefs := FSnomed.Refs.GetCardinals(iOtherNames);
      html.StartTable;
      for i := Low(iRefs) To High(iRefs) Do
      begin
        html.StartRow();
        html.AddTableCell(FSnomed.desc.GetEntry(iRefs[i]));
        html.EndTableRow;
      End;
      html.EndTable;
    End;
  End
  Else
    html.AddParagraph('Unable to find code '+sCode);
end;

procedure TSnomedPublisher.PublishConcept(bRoot : Boolean; const sPrefix, sId: String; iStart : Integer; html: THtmlPublisher);
var
  aChildren : TWordArray;
  aCodes : TCardinalArray;
  iName : Cardinal;
  iChildren : Cardinal;
  iCodes : Cardinal;
  i : Integer;
  iDummy : Cardinal;
  b2, b3, b4 : Boolean;

  iDescription, iOtherNames : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;

begin
  FSnomed.Concepts.GetConcept(StrToInt(sId), iName, iChildren, iCodes);
  if Not bRoot then
    html.AddTitle('Snomed Concept '+FSnomed.Desc.GetEntry(iName));

  b2 := false;
  b3 := false;
  b4 := false;
  if iChildren <> 0 Then
  begin
    aChildren := FSnomed.Refs.GetWords(iChildren);
    html.StartTable.BorderPolicy := WPDocumentTables.BorderPolicyNone;
    html.StartTableRow;
    html.StartTableCell;
    html.AddParagraph(' ');
    html.EndTableCell;
    html.StartTableCell;
    For i := iStart to Min(iStart+MAX_ROWS, High(aChildren)) Do
    Begin
      if not b2 And (Length(aChildren) > 20) And ((i - iStart) / Min(MAX_ROWS, Length(aChildren)) > 0.25) Then
      Begin
        html.EndTableCell;
        html.StartTableCell;
        b2 := true;
      End;
      if not b3 And (Length(aChildren) > 20) And ((i - iStart) / Min(MAX_ROWS, Length(aChildren)) > 0.5) Then
      Begin
        html.EndTableCell;
        html.StartTableCell;
        b3 := true;
      End;
      if not b4 And (Length(aChildren) > 20) And ((i - iStart) / Min(MAX_ROWS, Length(aChildren)) > 0.750) Then
      Begin
        html.EndTableCell;
        html.StartTableCell;
        b4 := true;
      End;
      FSnomed.Concepts.GetConcept(aChildren[i], iName, iChildren, iDummy);
      html.ParaURL(FSnomed.Desc.GetEntry(iName), sPrefix + 'id='+inttostr(aChildren[i]));
    End;
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

  b2 := false;
  b3 := false;
  b4 := false;
  if iCodes <> 0 Then
  begin
    aCodes := FSnomed.Refs.GetCardinals(iCodes);
    html.StartTable.BorderPolicy := WPDocumentTables.BorderPolicyNone;
    html.StartTableRow;
    html.StartTableCell;
    html.AddParagraph(' ');
    html.EndTableCell;
    html.StartTableCell;
    For i := iStart to Min(iStart+MAX_ROWS, High(aCodes)) Do
    Begin
      if not b2 And (Length(aCodes) > 20) And ((i - iStart) / Min(MAX_ROWS, Length(aCodes)) > 0.25) Then
      Begin
        html.EndTableCell;
        html.StartTableCell;
        b2 := true;
      End;
      if not b3 And (Length(aCodes) > 20) And ((i - iStart) / Min(MAX_ROWS, Length(aCodes)) > 0.5) Then
      Begin
        html.EndTableCell;
        html.StartTableCell;
        b3 := true;
      End;
      if not b4 And (Length(aCodes) > 20) And ((i - iStart) / Min(MAX_ROWS, Length(aCodes)) > 0.750) Then
      Begin
        html.EndTableCell;
        html.StartTableCell;
        b4 := true;
      End;
      FSnomed.Code.GetInformation(aCodes[i], sCode1, iDescription, iOtherNames, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);
      html.StartParagraph;
      html.URL(sCode1, sPrefix + 'code='+sCode1);
      html.AddTextPlain(': '+FSnomed.Desc.GetEntry(iDescription));
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
end;

procedure TSnomedPublisher.PublishCodes(const sPrefix: String; iStart: Integer; html: THtmlPublisher);
var
//  aChildren : TWordArray;
//  aCodes : TCardinalArray;
//  iName : Cardinal;
//  iChildren : Cardinal;
//  iCodes : Cardinal;
  i : Integer;
  iTotal : Integer;
//  iDummy : Cardinal;
  b2, b3, b4 : Boolean;

  iDescription, iOtherNames : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;
begin
  b2 := false;
  b3 := false;
  b4 := false;

    html.StartTable.BorderPolicy := WPDocumentTables.BorderPolicyNone;
    html.StartTableRow;
    html.StartTableCell;
    html.AddParagraph(' ');
    html.EndTableCell;
    html.StartTableCell;
    iTotal := FSnomed.Code.Count;
    For i := iStart to Min(iStart+MAX_ROWS, iTotal) Do
    Begin
      if not b2 And ((i - iStart) / Min(MAX_ROWS, iTotal) > 0.25) Then
      Begin
        html.EndTableCell;
        html.StartTableCell;
        b2 := true;
      End;
      if not b3 And ((i - iStart) / Min(MAX_ROWS, iTotal) > 0.5) Then
      Begin
        html.EndTableCell;
        html.StartTableCell;
        b3 := true;
      End;
      if not b4 And ((i - iStart) / Min(MAX_ROWS, iTotal) > 0.750) Then
      Begin
        html.EndTableCell;
        html.StartTableCell;
        b4 := true;
      End;
      FSnomed.Code.GetInformation(i, sCode1, iDescription, iOtherNames, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);
      html.StartParagraph;
      html.URL(sCode1, sPrefix + 'code='+sCode1);
      html.AddTextPlain(': '+FSnomed.Desc.GetEntry(iDescription));
      html.EndParagraph;
    End;
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

procedure TSnomedPublisher.PublishSearch(const sPrefix, sText: String; iStart: Integer; html: THtmlPublisher);
var
  a : TCardinalArray;
  i : integer;
  o : TSearchCache;
  iTotal : Integer;
//  iDummy : Cardinal;
  b2, b3, b4 : Boolean;

  iDescription, iOtherNames : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;
begin
  if FSearchCache.Find(sText, i) Then
    a := TSearchCache(FSearchCache.Objects[i]).a
  else
  Begin
    a := FSnomed.Search(sText);
    o := TSearchCache.Create;
    o.a := a;
    FSearchCache.AddObject(sText, o);
  End;

  b2 := false;
  b3 := false;
  b4 := false;

    html.StartTable.BorderPolicy := WPDocumentTables.BorderPolicyNone;
    html.StartTableRow;
    html.StartTableCell;
    html.AddParagraph(' ');
    html.EndTableCell;
    html.StartTableCell;
    iTotal := High(a);
    For i := iStart to Min(iStart+MAX_ROWS, iTotal) Do
    Begin
      if not b2 And ((i - iStart) / Min(MAX_ROWS, iTotal) > 0.25) Then
      Begin
        html.EndTableCell;
        html.StartTableCell;
        b2 := true;
      End;
      if not b3 And ((i - iStart) / Min(MAX_ROWS, iTotal) > 0.5) Then
      Begin
        html.EndTableCell;
        html.StartTableCell;
        b3 := true;
      End;
      if not b4 And ((i - iStart) / Min(MAX_ROWS, iTotal) > 0.750) Then
      Begin
        html.EndTableCell;
        html.StartTableCell;
        b4 := true;
      End;
      FSnomed.Code.GetInformation(a[i], sCode1, iDescription, iOtherNames, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);
      html.StartParagraph;
      html.URL(sCode1, sPrefix + 'code='+sCode1);
      html.AddTextPlain(': '+FSnomed.Desc.GetEntry(iDescription));
      html.EndParagraph;
    End;
    html.EndTableCell;
    html.EndTableRow;
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

end;
*)

constructor TSnomedPublisher.Create(oSnomed : TSnomedServices; FHIRPathEngine : String);
begin
  inherited Create;
  Lock := TFslLock.Create;
  FSearchCache := TStringList.Create;
  FSearchCache.Sorted := true;
  FSnomed := oSnomed.Link;
  FFHIRPath := FHIRPathEngine;
end;

destructor TSnomedPublisher.Destroy;
var
  i : integer;
begin
  For i := 0 to FSearchCache.Count - 1 do
    FSearchCache.Objects[i].Free;
  FSearchCache.Free;
  Lock.Free;
  FSnomed.Free;
  inherited;
end;

Type
  TSearchCache = class (TObject)
  public
    a : TMatchArray;
  End;

procedure TSnomedPublisher.PublishConcepts(const sPrefix: String; iStart: Integer; html: THtmlPublisher);
var
  i : Integer;
  iTotal : Integer;
  b2 : Boolean;
begin
  b2 := false;

  html.Heading(1, 'Concept List');
  html.StartTable(false, 'bare');
  html.StartTableRow;
  html.StartTableCell;
  html.AddParagraph(' ');
  html.EndTableCell;
  html.StartTableCell;
  html.StartList;
  iTotal := FSnomed.Concept.Count;
  For i := iStart to Min(iStart+MAX_ROWS, iTotal) Do
  Begin
    if not b2 And ((i - iStart) / Min(MAX_ROWS, iTotal) > 0.5) Then
    Begin
      html.EndList;
      html.EndTableCell;
      html.StartTableCell;
      html.StartList;
      b2 := true;
    End;
    html.StartListItem;
    ConceptRef(html, sPrefix, i * CONCEPT_SIZE + 1, cdBoth, 0);
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
      html.URL('Start', sPrefix+'id=*');
      html.AddTextPlain(' ');
    End;
    if iStart > MAX_ROWS Then
    Begin
      html.URL('Previous', sPrefix+'id*&start='+inttostr(iStart - MAX_ROWS));
      html.AddTextPlain(' ');
    End;
    html.AddText('Page '+ inttostr((iStart div MAX_ROWS) + 1)+' of '+inttostr(iTotal div MAX_ROWS + 1), true, false);
    if (iStart+MAX_ROWS < iTotal) And (iStart + MAX_ROWS <  MAX_ROWS * (iTotal div MAX_ROWS)) Then
    Begin
      html.AddTextPlain(' ');
      html.URL('Next', sPrefix+'id=*&start='+inttostr(iStart + MAX_ROWS));
    End;
    if (iStart+MAX_ROWS < iTotal) Then
    Begin
      html.AddTextPlain(' ');
      html.URL('End', sPrefix+'id=*&start='+inttostr(MAX_ROWS * (iTotal div MAX_ROWS)));
    End;
    html.EndParagraph;
  End;
  html.ParaURL('Back to Start', sPrefix);
  html.Done;
end;

procedure TSnomedPublisher.PublishSearch(const sPrefix, sText, sContext: String; iStart: Integer; all : boolean; html: THtmlPublisher);
var
  a : TMatchArray;
  i : integer;
  o : TSearchCache;
  iTotal : Integer;
//  iDummy : Cardinal;
  b2 : Boolean;
  icontext : UInt64;

begin
  iContext := StrToUInt64Def(sContext, 0);
  Lock.Lock;
  Try
    if FSearchCache.Find(sText+#0+sContext, i) Then
      a := TSearchCache(FSearchCache.Objects[i]).a
    else
    Begin
      a := FSnomed.Search(iContext, sText, 0, false, all);
      o := TSearchCache.Create;
      o.a := a;
      FSearchCache.AddObject(sText+#0+sContext, o);
    End;
  Finally
    Lock.Unlock;
  End;

  if iContext <> 0 then
    html.Heading(1, 'Search for '+sText+' in '+sContext)
  Else
    html.Heading(1, 'Search for '+sText+' in all of Snomed');

  b2 := false;

  html.StartTable(false, 'bare');
  html.StartTableRow;
  html.StartTableCell;
  html.AddParagraph(' ');
  html.EndTableCell;
  html.StartTableCell;
  html.StartList;
  iTotal := High(a);
  For i := iStart to Min(iStart+MAX_ROWS, iTotal) Do
  Begin
    if not b2 And ((i - iStart) / Min(MAX_ROWS, iTotal+1) > 0.5) Then
    Begin
      html.EndList;
      html.EndTableCell;
      html.StartTableCell;
      html.StartList;
      b2 := true;
    End;
    html.StartListItem;
    ConceptRef(html, sPrefix, a[i].index, cdBoth, a[i].Priority);
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
    html.spacer;
    html.spacer;
    html.spacer;

    if iContext = 0 then
      html.URL('Back to Context', sPrefix)
    else
      html.URL('Back to Context', sPrefix+'id='+sContext);

    html.EndParagraph;
  End;
  html.Done;
end;

procedure TSnomedPublisher.PublishTerm(const sTerm: String; html: THtmlPublisher);
Begin
  PublishTermConcept(false, '?type=snomed&', sTerm, 0, html)
end;

procedure TSnomedPublisher.PublishTermConcept(bRoot : Boolean; const sPrefix, sId: String; iStart : Integer; html: THtmlPublisher);
var
  did : UInt64;
  iId : UInt64;
  iIndex : Cardinal;
  Identity : UInt64;
  Flags : Byte;
  Active, Defining : boolean;
  Group: Integer;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex, refsets : Cardinal;
  Parents : TCardinalArray;
  Descriptions : TCardinalArray;
  Inbounds : TCardinalArray;
  outbounds : TCardinalArray;
  allDesc : TCardinalArray;
  iWork, iWork2, iWork3, module, kind, modifier : Cardinal;
  FSN : String;
  PN : String;
  FPaths : TArrayofIdArray;
  date : TSnomedDate;
  i : integer;
Begin
  SetLength(allDesc, 0);
  iId := StrToUInt64Def(sId, 0);
  if not FSnomed.Concept.FindConcept(iId, iIndex) Then
  Begin
    html.AddTitle('Snomed Concept '+sId);
    html.AddParagraph(sId+' is not a valid Snomed-CT Concept Id');
    SetLength(FPaths, 0);
    SetLength(Parents, 0);
    SetLength(Descriptions, 0);
    SetLength(Inbounds, 0);
    SetLength(outbounds, 0);
  End
  else
  Begin
    FSnomed.Concept.GetConcept(iIndex, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
    if ParentIndex <> 0 Then
      Parents := FSnomed.Refs.GetReferences(ParentIndex);
    Descriptions := FSnomed.Refs.GetReferences(DescriptionIndex);
    Inbounds := FSnomed.Refs.GetReferences(InboundIndex);
    outbounds := FSnomed.Refs.GetReferences(outboundIndex);
    FSN := FSnomed.GetFSN(Descriptions);
    PN := FSnomed.GetPN(Descriptions);

    html.StartParagraph;
    html.AddText(sId, true, true);
    html.AddText(': '+screen(PN, ''), true, false);
    html.EndParagraph;
    html.AddParagraph(FSN);

    html.StartList(false);
    for i := Low(Outbounds) To High(Outbounds) Do
    Begin
      FSnomed.Rel.GetRelationship(Outbounds[i], did, iWork, iWork2, iWork3, module, kind, modifier, date, Active, Defining, Group);
      if Defining Then
      Begin
        html.StartListItem;
        ConceptRef(html, sPrefix, iWork3, cdDesc, 0);
        html.AddTextPlain(': ');
        ConceptRef(html, sPrefix, iWork2, cdDesc, 0);
        html.EndListItem;
      End;
    End;
    html.EndList(false);
    html.Line;

    html.StartParagraph;
    html.AddText('Children', true, false);
    allDesc := FSnomed.Refs.GetReferences(FSnomed.Concept.GetAllDesc(iIndex));
    html.AddTextPlain(' ('+inttostr(length(alldesc))+' descendants in all)');
    html.EndParagraph;

    html.StartList(false);
    For i := iStart to Min(iStart+MAX_ROWS, High(Inbounds)) Do
    Begin
      FSnomed.Rel.GetRelationship(Inbounds[i], did, iWork, iWork2, iWork3, module, kind, modifier, date, Active, Defining, Group);
      if Active and (iWork3 = FSnomed.Is_a_Index) Then
      Begin
        html.StartListItem;
        ConceptRef(html, sPrefix, iWork, cdDesc, 0);
        html.EndListItem;
      End;
    End;
    html.EndList(false);
  End;
End;

function describeRefSetType(t : cardinal) : String;
begin
  case t of
    99 {c} : result := 'concept';
    105 {i} : result := 'integer';
    115 {s} :  result := 'string';
  else
    result := '??';
  end;
end;

procedure TSnomedPublisher.RefsetRef(html: THtmlPublisher; const sPrefix: String; iIndex: cardinal);
var
  iDefinition, iMembersByName, iMembersByRef, iTypes, iFilename, iName, iFields: Cardinal;
  types : TCardinalArray;
  id : String;
  i : integer;
begin
  FSnomed.RefSetIndex.GetReferenceSet(iIndex, iFilename, iName, iDefinition, iMembersByName, iMembersByRef, iTypes, iFields);
  id := inttostr(FSnomed.Concept.GetIdentity(iDefinition));
  html.URL(Screen(id+' '+FSnomed.GetPNForConcept(iDefinition), ' reference set'), sPrefix+'id='+id);
  html.AddTextPlain('(');
  html.AddTextPlain(inttostr(FSnomed.RefSetMembers.GetMemberCount(iMembersByRef))+' members)');
  if iTypes <> 0 then
  begin
    types := FSnomed.Refs.GetReferences(iTypes);
    html.AddTextPlain(' (values = ');
    for i := 0 to length(types) - 1 do
    begin
      if i > 0 then
        html.AddTextPlain(', ');
      html.AddTextPlain(describeRefSetType(types[i]));
    end;
    html.AddTextPlain(')');
  end;
end;

function TSnomedPublisher.ConceptForDesc(iDesc: Cardinal; var iDescs : Cardinal): Cardinal;
var
  id : UInt64;
  active : boolean;
  date : TSnomedDate;
  module, refsets, valueses, kind, caps : Cardinal;
  lang : byte;
begin
  FSnomed.Desc.GetDescription(iDesc, iDescs, id, date, result, module, kind, caps, refsets, valueses, active, lang);
end;

function TSnomedPublisher.GetConceptForRefset(iRefset: Cardinal): Cardinal;
var
  iDummy : Cardinal;
begin
  FSnomed.RefSetIndex.GetReferenceSet(iRefset, iDummy, iDummy, result, iDummy, iDummy, iDummy, iDummy);
end;


End.

