unit SnomedPublisher;

Interface

Uses
  classes,
  Math,
  AdvObjects,
  SysUtils,
  MathSupport,
  AdvStringMatches,
  AdvExclusiveCriticalSections,
  SnomedServices,

  FHIRConstants, // todo: really need to sort out how XHTML template is done
  FHIRParserBase;

Const
  MAX_ROWS = 100;

Type
  TIdArray = array of cardinal;
  TArrayofIdArray = array of TIdArray;

  THtmlPublisher = class (TAdvObject)
  private
    FBuilder : TStringBuilder;
    FBaseURL: String;
    FLang: String;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    procedure Header(s : String);
    procedure Done;

    procedure Line;

    procedure StartParagraph;
    procedure EndParagraph;
    procedure AddParagraph(text : String = '');
    procedure AddTextPlain(text : String);
    procedure AddTitle(text : String);
    procedure AddText(text : String; bold, italics : boolean);

    procedure URL(text, url : String);
    procedure ParaURL(text, url : String);

    procedure StartTable(borders : boolean; clss : String = '');
    procedure StartTableRow;
    procedure StartRowFlip(i : integer);
    procedure StartTableCell;
    procedure EndTableCell;
    procedure EndTableRow;
    procedure EndTable;
    procedure AddTableCellURL(text, url : String);
    procedure AddTableCell(text : String; bold : boolean = false);

    procedure StartList(ordered : boolean = false);
    procedure EndList(ordered : boolean = false);
    procedure StartListItem;
    procedure EndListItem;
    procedure StartBlockQuote;
    procedure EndBlockQuote;

    procedure StartForm(method, action : String);
    procedure TextInput(name : String; length : integer = 20);
    procedure hiddenInput(name, value : String);
    procedure Submit(name : String);
    procedure EndForm;

    procedure Spacer;

    function output : String;
    Property BaseURL : String read FBaseURL write FBaseURL;
    Property Lang : String read FLang write FLang;
  end;

  TSnomedPublisher = class (TAdvObject)
  Private
    FSnomed : TSnomedServices;
    Lock : TAdvExclusiveCriticalSection;
    FSearchCache : TStringList;
    Function GetFSN(iDescriptions : TCardinalArray) : String;
    Function GetPN(iDescriptions : TCardinalArray) : String;
    Function GetPNForConcept(iIndex : Cardinal) : String;
    Function GetPaths(iIndex : Cardinal) : TArrayofIdArray;
    Function ConceptForDesc(iDesc : Cardinal; var iDescs : Cardinal):Cardinal;
    Procedure ConceptRef(html : THtmlPublisher; const sPrefix : String; iIndex : cardinal; bShowId : Boolean; rRating : Double);
    Procedure RefsetRef(html : THtmlPublisher; Const sPrefix : String; iIndex : cardinal);
    Procedure CellConceptRef(html : THtmlPublisher; const sPrefix : String; iIndex : cardinal; bShowId : Boolean; iDesc : Cardinal = 0);

    Procedure SortRefsets(var a : TCardinalArray);
    Function GetConceptForRefset(iRefset : Cardinal) : Cardinal;
    procedure PublishPaths(html: THtmlPublisher; Const sPrefix : String; aPaths : TArrayofIdArray; iFocus : Cardinal; iIndent, iStart, iLow, iHigh : Integer);

    Procedure PublishConcept(bRoot : Boolean; Const sPrefix, sId : String; iStart : Integer; html : THtmlPublisher);
    Procedure PublishTermConcept(bRoot : Boolean; Const sPrefix, sId : String; iStart : Integer; html : THtmlPublisher);
    Procedure PublishConcepts(Const sPrefix : String; iStart : Integer; html : THtmlPublisher);
    Procedure PublishSearch(Const sPrefix, sText, sContext : String; iStart: Integer; html : THtmlPublisher);
    Procedure PublishHome(Const sPrefix : String; html : THtmlPublisher);

    Procedure ProcessMap(Const sPath : String; oMap : TAdvStringMatch);
    Procedure PublishDictInternal(oMap : TAdvStringMatch; Const sPrefix : String; html : THtmlPublisher);
  Public
    Constructor Create(oSnomed : TSnomedServices);
    Destructor Destroy; Override;
    Procedure PublishDict(Const sPath, sPrefix : String; html : THtmlPublisher); Overload; Virtual;
    Procedure PublishDict(oMap : TAdvStringMatch; Const sPrefix : String; html : THtmlPublisher); Overload; Virtual;
    Procedure PublishTerm(Const sTerm : String; html : THtmlPublisher); Overload; Virtual;
  End;

Implementation

Uses
  EncodeSupport,
  StringSupport;

Function Screen(Const s, s2: String):String;
Begin
  result := StringReplace(s, 'Â', '');
  if (s2 <> '') And StringEndsWith(result, s2) Then
    delete(result, length(result) - length(s2) + 1, length(s));
End;

Procedure TSnomedPublisher.PublishDictInternal(oMap : TAdvStringMatch; Const sPrefix : String; html : THtmlPublisher);
Var
  sURL : String;
  sId : String;
Begin
  sURL := sPrefix +'?type=snomed&key='+inttostr(FSnomed.Key)+'&';

  sId := oMap.Matches['id'];
  If sId = '*' Then
    PublishConcepts(sURL, StrToIntDef(oMap.Matches['start'], 0), html)
  else If (sId <> '')  Then
    PublishConcept(false, sURL, sId, StrToIntDef(oMap.Matches['start'], 0), html)
  else if oMap.ExistsByKey('srch') then
    PublishSearch(sURL, oMap.Matches['srch'], oMap.Matches['context'], StrToIntDef(oMap.Matches['start'], 0), html)
  else
    PublishHome(sURL, html)
End;

Procedure TSnomedPublisher.ProcessMap(Const sPath : String; oMap : TAdvStringMatch);
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



procedure TSnomedPublisher.PublishDict(oMap: TAdvStringMatch; const sPrefix: String; html: THtmlPublisher);
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

  html.Header('Snomed-CT Definitions (v: '+FSnomed.Version+')');
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
      PublishConcept(true, sPrefix, inttostr(FSnomed.Activeroots[0]), 0, html)
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
          ConceptRef(html, sPrefix, iRef, true, 0);
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
        While GetPNForConcept(GetConceptForRefset(a[I])) < GetPNForConcept(GetConceptForRefset(a[K])) Do
          Inc(I);

        While GetPNForConcept(GetConceptForRefset(a[J])) > GetPNForConcept(GetConceptForRefset(a[K])) Do
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


Function TSnomedPublisher.GetPNForConcept(iIndex : Cardinal) : String;
var
  Identity : int64;
  Flags : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex, refsets : Cardinal;
  Descriptions : TCardinalArray;
  date : TSnomedDate;
Begin
  FSnomed.Concept.GetConcept(iIndex, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
  Descriptions := FSnomed.Refs.GetReferences(DescriptionIndex);
  result := GetPN(Descriptions);
End;

Function TSnomedPublisher.GetFSN(iDescriptions : TCardinalArray) : String;
var
  iLoop : Integer;
  iid : int64;
  iString, iDummy, module, refsets, kind : Cardinal;
  iFlag : Byte;
  date : TSnomedDate;
begin
  result := '';
  For iLoop := Low(iDescriptions) To High(iDescriptions) Do
  Begin
    FSnomed.Desc.GetDescription(iDescriptions[iLoop], iString, iId, date, iDummy,  module, kind, refsets, iFlag);
    if (iFlag and MASK_DESC_STATUS = FLAG_Active) And (iFlag and MASK_DESC_STYLE = VAL_DESC_FullySpecifiedName shl 4) Then
      result := FSnomed.Strings.GetEntry(iString);
  End;
End;

Function TSnomedPublisher.GetPN(iDescriptions : TCardinalArray) : String;
var
  iLoop : Integer;
  iid : int64;
  iString, iDummy, module, refsets, kind : Cardinal;
  iFlag : Byte;
  date : TSnomedDate;
  iList : TCardinalArray;
  v : AnsiString;
begin
  result := '';
  For iLoop := Low(iDescriptions) To High(iDescriptions) Do
  Begin
    FSnomed.Desc.GetDescription(iDescriptions[iLoop], iString, iId, date, iDummy,  module, kind, refsets, iFlag);
    if (iFlag and MASK_DESC_STATUS = FLAG_Active) And (iFlag and MASK_DESC_STYLE shr 4 in [VAL_DESC_Preferred]) Then
      result := FSnomed.Strings.GetEntry(iString);
  End;
  if result = '' then // ok, well, we'll pick the first description that's in a value set
  begin
    For iLoop := Low(iDescriptions) To High(iDescriptions) Do
    Begin
      FSnomed.Desc.GetDescription(iDescriptions[iLoop], iString, iId, date, iDummy,  module, kind, refsets, iFlag);
      iList := FSnomed.Refs.GetReferences(refsets);
      v := FSnomed.Strings.GetEntry(iString);
      if ((result = '') or (length(result) > length(v))) and (iFlag and MASK_DESC_STATUS = FLAG_Active) And (Length(iList) > 0) Then
        result := v;
    End;
  end;
  if result = '' Then // ok, give up. and use the FSN
    result := GetFSN(iDescriptions);
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

Function GetDescType(Flags : Byte) : String;
Begin
  case (flags and MASK_DESC_STYLE) shr 4 of
    VAL_DESC_Unspecified : result := 'Unspecified';
    VAL_DESC_Preferred : result := 'Preferred';
    VAL_DESC_Synonym : result := 'Synonym';
    VAL_DESC_FullySpecifiedName : result := 'FSN';
  End;
End;

Function GetDescStatus(Flags : Byte) : String;
Begin
  case (flags and MASK_DESC_STATUS) of
    FLAG_Active : Result := 'Active';
    FLAG_RetiredWithoutStatedReason : Result := 'Retired Without Stated Reason';
    FLAG_Duplicate : Result := 'Duplicate';
    FLAG_Outdated : Result := 'Outdated';
    FLAG_Ambiguous : Result := 'Ambiguous';
    FLAG_Erroneous : Result := 'Erroneous';
    FLAG_Limited : Result := 'Limited';
    FLAG_Inappropriate : Result := 'Inappropriate';
    FLAG_ConceptInactive : Result := 'Concept Inactive';
    FLAG_MovedElswhere : Result := 'Moved Elswhere';
    FLAG_PendingMove : Result := 'Pending Move';
  End;
End;

Function GetRelChar(Flags : Byte) : String;
Begin
  case (flags and MASK_REL_CHARACTERISTIC) of
    VAL_REL_Defining : result := 'Defining';
    VAL_REL_Qualifying : result := 'Qualifying';
    VAL_REL_Historical : result := 'Historical';
    VAL_REL_Additional : result := 'Additional';
  else
    result := '';
  End;
End;

Function GetRelRefinability(Flags : Byte) : String;
Begin
  case (flags and MASK_REL_REFINABILITY) shr 4 of
    VAL_REL_NotRefinable : result := 'No';
    VAL_REL_Optional : result := 'Optional';
    VAL_REL_Mandatory : result := 'Mandatory';
  else
    result := '';
  End;
End;


Function GetRelGroup(iGroup : Byte):String;
Begin
  if iGroup = 0 Then
    result := ''
  Else
    result := inttostr(iGroup);
End;

Procedure TSnomedPublisher.ConceptRef(html : THtmlPublisher; const sPrefix : String; iIndex : cardinal; bShowId : Boolean; rRating : Double);
Begin
  if bShowId Then
    html.URL(inttostr(FSnomed.Concept.GetIdentity(iIndex))+' '+Screen(GetPNForConcept(iIndex), ''), sPrefix+'id='+inttostr(FSnomed.Concept.GetIdentity(iIndex)))
  Else
    html.URL(Screen(GetPNForConcept(iIndex), ''), sPrefix+'id='+inttostr(FSnomed.Concept.GetIdentity(iIndex)));
  if rRating > 0 then
    html.AddTextPlain(' '+inttostr(Trunc(rRating * 10)));

End;

Procedure TSnomedPublisher.CellConceptRef(html : THtmlPublisher; const sPrefix : String; iIndex : cardinal; bShowId : Boolean; iDesc : Cardinal = 0);
var
  s : String;
Begin
  if iDesc <> 0 Then
    s := FSnomed.Strings.GetEntry(iDesc)
  Else
    s := GetPNForConcept(iIndex);

  if bShowId Then
    html.AddTableCellURL(inttostr(FSnomed.Concept.GetIdentity(iIndex))+' '+Screen(s, ''), sPrefix+'id='+inttostr(FSnomed.Concept.GetIdentity(iIndex)))
  else
    html.AddTableCellURL(Screen(s, ''), sPrefix+'id='+inttostr(FSnomed.Concept.GetIdentity(iIndex)));
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
        html.AddText(Screen(GetPNForConcept(iFocus), ''), false, true)
      Else
        ConceptRef(html, sPrefix, aPaths[iLow][j], false, 0);
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
          html.AddText(Screen(GetPNForConcept(iFocus), ''), false, true)
        Else
          ConceptRef(html, sPrefix, aPaths[i][j], false, 0);
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

procedure TSnomedPublisher.PublishConcept(bRoot : Boolean; const sPrefix, sId: String; iStart : Integer; html: THtmlPublisher);
var
  iId : int64;
  iIndex : Cardinal;
  Identity : int64;
  Flags, Group : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  Parents : TCardinalArray;
  Descriptions : TCardinalArray;
  Inbounds : TCardinalArray;
  outbounds : TCardinalArray;
  allDesc : TCardinalArray;
  iWork, iWork2, iWork3, kind, module, refsets, modifier : Cardinal;
  FSN : String;
  PN : String;
  FPaths : TArrayofIdArray;
  i : integer;
  iList : TCardinalArray;
  iDummy, iRefSet, iMembers, iDescs : Cardinal;
  bDescSet : Boolean;
  aMembers : TSnomedReferenceSetMemberArray;
  date : TSnomedDate;
Begin
  SetLength(aMembers, 0);
  SetLength(iList, 0);
  SetLength(alLDesc, 0);
  iId := StrToInt64Def(sId, -1);
  if not FSnomed.Concept.FindConcept(iId, iIndex) Then
  Begin
    html.Header('Snomed Concept '+sId);
    html.AddParagraph(sId+' is not a valid Snomed-CT Concept Id');
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
    FSN := GetFSN(Descriptions);
    PN := GetPN(Descriptions);
    if Not bRoot then
      html.Header(sId+': '+screen(FSN, ''));
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
      ConceptRef(html, sPrefix, iDummy, false, 0);
    end;
    html.AddTextPlain('. Date: '+FormatDateTime('dd-mmm yyyy', date));
    iDummy := FSnomed.Concept.GetModuleId(iIndex);
    if iDummy <> 0 then
    begin
      html.AddTextPlain('. Module: ');
      ConceptRef(html, sPrefix, iDummy, false, 0);
    end;
    html.EndParagraph;

    // todo: flags
    html.AddParagraph('Descriptions:');
    html.StartTable(true, 'lines');
    html.StartTableRow;
    html.AddTableCell('Id', true);
    html.AddTableCell('Description', true);
    html.AddTableCell('Type', true);
    html.AddTableCell('Status', true);
    html.AddTableCell('Module', true);
    if FSnomed.RefSetIndex.Count > 0 Then
      html.AddTableCell('Reference Sets', true);
    html.EndTableRow;
    for i := Low(Descriptions) To High(Descriptions) Do
    Begin
      FSnomed.Desc.GetDescription(Descriptions[i], iWork, iId, date, iDummy, module, kind, refsets, Flags);
      if flags and MASK_DESC_STATUS = Flag_Active Then
      Begin
        html.StartRowFlip(i);
        html.AddTableCell(inttostr(iId));
        html.AddTableCell(Screen(FSnomed.Strings.GetEntry(iWork), ''));
        if ((flags and MASK_DESC_STYLE) shr 4 = VAL_DESC_Unspecified) and (kind <> 0) then
          CellConceptRef(html, sPrefix, kind, false)
        else
          html.AddTableCell(GetDescType(Flags));
        html.AddTableCell(GetDescStatus(Flags));
        if (module <> 0) then
          CellConceptRef(html, sPrefix, module, false)
        else
        html.AddTableCell('');
        if FSnomed.RefSetIndex.Count > 0 Then
        Begin
          iList := FSnomed.GetDescRefsets(Descriptions[i]);
          if Length(ilist) = 0 Then
            html.AddTableCell('')
          Else
            CellConceptRef(html, sPrefix, iList[0], false);
        End;
//        html.AddTableCell(BooleanToString(flags and MASK_DESC_CAPS > 0));
        html.EndTableRow;
      End;
    End;
    html.EndTable;
    html.Line;

    iRefSet := FSnomed.GetConceptRefSet(iIndex, true, iMembers);
    allDesc := FSnomed.Refs.GetReferences(FSnomed.Concept.GetAllDesc(iIndex));
    html.StartForm('GET', sPrefix);
    html.StartParagraph;

    if iRefSet = 0 then
    begin
      html.AddTextPlain(inttostr(length(alldesc))+' descendents. ');
      html.AddTextPlain('Search Descendents: ');
    End
    else
    Begin
      html.AddTextPlain(inttostr(FSnomed.RefSetMembers.GetMemberCount(iMembers))+' members. ');
      html.AddTextPlain('Search Members: ');
    End;
    html.hiddenInput('context', sid);
    html.textInput('srch');
    html.submit('Go');
    html.endForm;
    html.Line;
    if not bRoot and (Length(Outbounds) > 0) Then
    Begin
      html.StartTable(false);
      html.StartTableRow;
      html.AddTableCell('Outbound Relationships', true);
      html.AddTableCell('Type', true);
      html.AddTableCell('Target', true);
      html.AddTableCell('Characteristic', true);
      html.AddTableCell('Refinability', true);
      html.AddTableCell('Group', true);
      html.EndTableRow;
      for i := Low(Outbounds) To High(Outbounds) Do
      Begin
        FSnomed.Rel.GetRelationship(Outbounds[i], iWork, iWork2, iWork3, module, kind, modifier, date, Flags, Group);
        html.StartRowFlip(i);
        html.AddTableCell(Screen(PN, ''));
        CellConceptRef(html, sPrefix, iWork3, false);
        CellConceptRef(html, sPrefix, iWork2, false);
        html.AddTableCell(' '+GetRelChar(Flags));
        html.AddTableCell(' '+GetRelRefinability(Flags));
        html.AddTableCell(' '+GetRelGroup(Group));
        html.EndTableRow;
      End;
      html.EndTable;
      html.Line;
    End;

    if iRefset <> 0 Then
    Begin
      aMembers := FSnomed.RefSetMembers.GetMembers(iMembers);
      html.StartTable(false);
      html.StartTableRow;
      html.AddTableCell('Member', true);
//      html.AddTableCell('Type', true);
      html.EndTableRow;
      For i := iStart to Min(iStart+MAX_ROWS, High(aMembers)) Do
      Begin
        html.StartRowFlip(i);
        if bDescSet then
        Begin
          iDummy := ConceptForDesc(aMembers[i].Ref, iDescs);
          CellConceptRef(html, sPrefix, iDummy, false, iDescs);
//          CellConceptRef(html, sPrefix, aMembers[i].Attr, false);
        End
        Else
        Begin
          CellConceptRef(html, sPrefix, aMembers[i].Ref, false);
//          CellConceptRef(html, sPrefix, aMembers[i].Attr, false);
        End;
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
      html.AddTableCell('Source', true);
      html.AddTableCell('Characteristic', true);
      html.AddTableCell('Refinability', true);
      html.AddTableCell('Group', true);
      html.EndTableRow;
      For i := iStart to Min(iStart+MAX_ROWS, High(Inbounds)) Do
      Begin
        FSnomed.Rel.GetRelationship(Inbounds[i], iWork, iWork2, iWork3, module, kind, modifier, date, Flags, Group);
        html.StartRowFlip(i);
        CellConceptRef(html, sPrefix, iWork, false);
        CellConceptRef(html, sPrefix, iWork3, false);
        html.AddTableCell(Screen(PN, ''));
        html.AddTableCell(' '+GetRelChar(Flags));
        html.AddTableCell(' '+GetRelRefinability(Flags));
        html.AddTableCell(' '+GetRelGroup(Group));
        html.EndTableRow;
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
        ConceptRef(html, sPrefix, iList[i], false, 0);
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
        html.StartRowFlip(i);
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

constructor TSnomedPublisher.Create;
begin
  inherited Create;
  Lock := TAdvExclusiveCriticalSection.Create;
  FSearchCache := TStringList.Create;
  FSearchCache.Sorted := true;
  FSnomed := oSnomed.Link;
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
    a : TMatchArray;
  End;

procedure TSnomedPublisher.PublishConcepts(const sPrefix: String; iStart: Integer; html: THtmlPublisher);
var
  i : Integer;
  iTotal : Integer;
  b2 : Boolean;
begin
  b2 := false;

  html.Header('Concept List');
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
    ConceptRef(html, sPrefix, i * CONCEPT_SIZE + 1, true, 0);
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

procedure TSnomedPublisher.PublishSearch(const sPrefix, sText, sContext: String; iStart: Integer; html: THtmlPublisher);
var
  a : TMatchArray;
  i : integer;
  o : TSearchCache;
  iTotal : Integer;
//  iDummy : Cardinal;
  b2 : Boolean;
  icontext : Int64;

begin
  iContext := StrToInt64Def(sContext, 0);
  Lock.Lock;
  Try
    if FSearchCache.Find(sText+#0+sContext, i) Then
      a := TSearchCache(FSearchCache.Objects[i]).a
    else
    Begin
      a := FSnomed.Search(iContext, sText, 0, false);
      o := TSearchCache.Create;
      o.a := a;
      FSearchCache.AddObject(sText+#0+sContext, o);
    End;
  Finally
    Lock.Unlock;
  End;

  if iContext <> 0 then
    html.header('Search for '+sText+' in '+sContext)
  Else
    html.header('Search for '+sText+' in all of Snomed');

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
    ConceptRef(html, sPrefix, a[i].index, true, a[i].Priority);
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
  iId : int64;
  iIndex : Cardinal;
  Identity : int64;
  Flags, Group : Byte;
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
  iId := StrToInt64Def(sId, -1);
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
    FSN := GetFSN(Descriptions);
    PN := GetPN(Descriptions);

    html.StartParagraph;
    html.AddText(sId, true, true);
    html.AddText(': '+screen(PN, ''), true, false);
    html.EndParagraph;
    html.AddParagraph(FSN);

    html.StartList(false);
    for i := Low(Outbounds) To High(Outbounds) Do
    Begin
      FSnomed.Rel.GetRelationship(Outbounds[i], iWork, iWork2, iWork3, module, kind, modifier, date, Flags, Group);
      if flags and MASK_REL_CHARACTERISTIC = VAL_REL_Defining Then
      Begin
        html.StartListItem;
        ConceptRef(html, sPrefix, iWork3, false, 0);
        html.AddTextPlain(': ');
        ConceptRef(html, sPrefix, iWork2, false, 0);
        html.EndListItem;
      End;
    End;
    html.EndList(false);
    html.Line;

    html.StartParagraph;
    html.AddText('Children', true, false);
    allDesc := FSnomed.Refs.GetReferences(FSnomed.Concept.GetAllDesc(iIndex));
    html.AddTextPlain(' ('+inttostr(length(alldesc))+' descendents in all)');
    html.EndParagraph;

    html.StartList(false);
    For i := iStart to Min(iStart+MAX_ROWS, High(Inbounds)) Do
    Begin
      FSnomed.Rel.GetRelationship(Inbounds[i], iWork, iWork2, iWork3, module, kind, modifier, date, Flags, Group);
      if iWork3 = FSnomed.Is_a_Index Then
      Begin
        html.StartListItem;
        ConceptRef(html, sPrefix, iWork, false, 0);
        html.EndListItem;
      End;
    End;
    html.EndList(false);
  End;
End;

procedure TSnomedPublisher.RefsetRef(html: THtmlPublisher; const sPrefix: String; iIndex: cardinal);
var
  iDefinition, iMembersByName, iMembersByRef: Cardinal;
begin
  FSnomed.RefSetIndex.GetReferenceSet(iIndex, iDefinition, iMembersByName, iMembersByRef);
  html.URL(Screen(GetPNForConcept(iDefinition), ' reference set'), sPrefix+'id='+inttostr(FSnomed.Concept.GetIdentity(iDefinition)));
  html.AddTextPlain('(');
  html.AddTextPlain(inttostr(FSnomed.RefSetMembers.GetMemberCount(iMembersByRef))+' members)');
end;

function TSnomedPublisher.ConceptForDesc(iDesc: Cardinal; var iDescs : Cardinal): Cardinal;
var
  id : Int64;
  iflags : Byte;
  date : TSnomedDate;
  module, refsets, kind : Cardinal;
begin
  FSnomed.Desc.GetDescription(iDesc, iDescs, id, date, result, module, kind, refsets, iflags);
end;

function TSnomedPublisher.GetConceptForRefset(iRefset: Cardinal): Cardinal;
var
  iDummy : Cardinal;
begin
  FSnomed.RefSetIndex.GetReferenceSet(iRefset, result, iDummy, iDummy);
end;

{ THtmlPublisher }

procedure THtmlPublisher.AddParagraph(text: String);
begin
  StartParagraph;
  AddTextPlain(text);
  EndParagraph;
end;

procedure THtmlPublisher.AddTableCell(text: String; bold: boolean);
begin
  StartTableCell;
  addtext(text, bold, false);
  EndTableCell;
end;

procedure THtmlPublisher.AddTableCellURL(text, url: String);
begin
  StartTableCell;
  self.URL(text, url);
  EndTableCell;
end;

procedure THtmlPublisher.AddText(text: String; bold, italics: boolean);
begin
  if bold then
    FBuilder.Append('<b>');
  if italics then
    FBuilder.Append('<i>');
  AddTextPlain(text);
  if italics then
    FBuilder.Append('</i>');
  if bold then
    FBuilder.Append('</b>');
end;

procedure THtmlPublisher.AddTextPlain(text: String);
begin
  FBuilder.Append(EncodeXML(text));
end;

procedure THtmlPublisher.AddTitle(text: String);
begin
  AddText(text, true, false);
end;

constructor THtmlPublisher.Create;
begin
  inherited;
  FBuilder := TStringBuilder.create;
end;

destructor THtmlPublisher.Destroy;
begin
  FBuilder.Free;
  inherited;
end;

procedure THtmlPublisher.Done;
begin
  FBuilder.Append(TFHIRXhtmlComposer.footer(BaseURL));
end;

procedure THtmlPublisher.EndBlockQuote;
begin
  FBuilder.Append('</blockquote>'#13#10);
end;

procedure THtmlPublisher.EndForm;
begin
  FBuilder.Append('</form>'#13#10);
end;

procedure THtmlPublisher.EndList(ordered: boolean);
begin
  if ordered then
    FBuilder.Append('</ol>'#13#10)
  else
    FBuilder.Append('</ul>'#13#10);
end;

procedure THtmlPublisher.EndListItem;
begin
  FBuilder.Append('</li>'#13#10);
end;

procedure THtmlPublisher.EndParagraph;
begin
  FBuilder.Append('<p>'#13#10);
end;

procedure THtmlPublisher.EndTable;
begin
  FBuilder.Append('</table>'#13#10);
end;

procedure THtmlPublisher.EndTableCell;
begin
  FBuilder.Append('</td>'#13#10);
end;

procedure THtmlPublisher.EndTableRow;
begin
  FBuilder.Append('</tr>'#13#10);
end;

procedure THtmlPublisher.Header(s: String);
begin
  FBuilder.Append(
  '<?xml version="1.0" encoding="UTF-8"?>'#13#10+
  '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'#13#10+
  '       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10+
  ''#13#10+
  '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'#13#10+
  '<head>'#13#10+
  '    <title>'+s+'FHIR Server</title>'#13#10+
  TFHIRXhtmlComposer.pagelinks+
  FHIR_JS+
  '</head>'#13#10+
  ''#13#10+
  '<body>'#13#10+
  TFHIRXhtmlComposer.Header(nil, BaseURL, Lang)+
  '<h1>'+s+'</h1>'#13#10);
end;

procedure THtmlPublisher.hiddenInput(name, value: String);
begin
  FBuilder.Append('<input type="hidden" name="'+name+'" value="'+value+'"/>');
end;

procedure THtmlPublisher.Line;
begin
  FBuilder.Append('<hr/>'#13#10);
end;

function THtmlPublisher.output: String;
begin
  result := FBuilder.ToString;
end;

procedure THtmlPublisher.ParaURL(text, url: String);
begin
  StartParagraph;
  self.URL(text, url);
  EndParagraph;
end;

procedure THtmlPublisher.Spacer;
begin
  FBuilder.Append('&nbsp;');
end;

procedure THtmlPublisher.StartBlockQuote;
begin
  FBuilder.Append('<blockquote>');
end;

procedure THtmlPublisher.StartForm(method, action: String);
begin
  FBuilder.Append('<form method="'+method+'" action="'+action+'">'#13#10);
end;

procedure THtmlPublisher.StartList(ordered: boolean);
begin
  if ordered then
    FBuilder.Append('<ol>')
  else
    FBuilder.Append('<ul>');
end;

procedure THtmlPublisher.StartListItem;
begin
  FBuilder.Append('<li>');
end;

procedure THtmlPublisher.StartParagraph;
begin
  FBuilder.Append('<p>');
end;

procedure THtmlPublisher.StartRowFlip(i: integer);
begin
  FBuilder.Append('<tr>')
end;

procedure THtmlPublisher.StartTable(borders: boolean; clss : String);
begin
  if clss <> '' then
    clss := ' class="'+clss+'"';
  if borders then
    FBuilder.Append('<table border="1"'+clss+'>')
  else
    FBuilder.Append('<table border="0"'+clss+'>');
end;

procedure THtmlPublisher.StartTableCell;
begin
  FBuilder.Append('<td>')
end;

procedure THtmlPublisher.StartTableRow;
begin
  FBuilder.Append('<tr>')
end;

procedure THtmlPublisher.Submit(name: String);
begin
  FBuilder.Append('<input type="submit" value="'+name+'"/>');
end;

procedure THtmlPublisher.TextInput(name: String; length: integer);
begin
  FBuilder.Append('<input type="text" name="'+name+'" size="'+inttostr(length)+'"/>');
end;

procedure THtmlPublisher.URL(text, url: String);
begin
  FBuilder.Append('<a href="'+url+'">');
  AddTextPlain(text);
  FBuilder.Append('</a>');
end;

End.

