unit SnomedImporter;

Interface

uses
  LoincServices,
  Classes,
  FileSupport,
  DateSupport,
  Inifiles,
  SysUtils,
  SnomedServices,
  BytesSupport,
  AnsiStringBuilder,
//  KClasses,
//  Kprocs,
  AdvObjects,
  AdvObjectLists,
  AdvIntegerLists,
  AdvNames,
  StringSupport,
  KDBManager,
  KDBDialects,
  YuStemmer;

//Const
//  TrackConceptDuplicates = false; // much slower, but only if you aren't reading a snapshot

Const
  RF2_MAGIC_DEFINED = 900000000000073002;
  RF2_MAGIC_PRIMITIVE = 900000000000074008;
  RF2_MAGIC_FSN = 900000000000003001;

Type
  TRelationship = record
    relationship : Cardinal;
    characteristic : Integer;
    Refinability : Integer;
    Group : Integer;
  End;

  TRelationshipArray = array of TRelationship;

  TRefSet = class (TAdvName)
  private
    index : Cardinal;
    aMembers : TSnomedReferenceSetMemberArray;
    iMemberLength : Integer;
    membersByRef : Cardinal;
    membersByName : Cardinal;

    function contains(term : cardinal) : boolean;
  end;

  TRefSetList = class (TAdvNameList)
  public
    function GetRefset(id : String) : TRefset;
  end;

  TConcept = class (TAdvObject)
  Private
    Index : Cardinal;
    Identity : UInt64;
    FModuleId : UInt64;
    FStatus : UInt64;
    Flag : Byte;
    FDate : TSnomedDate;
    FSN : String;
    FInBounds : TRelationshipArray;
    FOutbounds : TRelationshipArray;
    FParents : TCardinalArray;
    FDescriptions : TCardinalArray;
    Active : boolean;
//    FClosed : Boolean;
//    FClosure : TCardinalArray;
    Stems : TAdvIntegerList;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  End;

  TWordCache = class (TObject)
    Flags : Integer;
    Stem : String;
    constructor create(aStem : String);
  End;

  TSnomedImporter = class (TAdvObject)
  private
    FCacheDir : String;
    FConceptFiles : TStringList;
    FRelationshipFiles : TStringList;
    FDescriptionFiles : TStringList;
    OverallCount : Integer;
    FImportFormat : TFormatSettings;

    FStringsTemp : TStringList;
    FConcepts : TAdvObjectList;

    FStrings : TSnomedStrings;
    FRefs : TSnomedReferences;
    FWords : TSnomedWords;
    FStems : TSnomedStems;
    FDesc : TSnomedDescriptions;
    FConcept : TSnomedConceptList;
    FRel : TSnomedRelationshipList;
    FRefsetIndex : TSnomedReferenceSetIndex;
    FRefsetMembers : TSnomedReferenceSetMembers;
    FDescRef : TSnomedDescriptionIndex;
    FRefsets : TRefSetList;

    FVersion: String;
    Findex_is_a : Cardinal;
    FWordList : TStringList;
    FStemList : TStringList;
    FStemmer : TYuStemmer_8;

    FStatus: Integer;
    FKey: Integer;
    FDirectoryReferenceSets: String;
    FRF2: boolean;
    FStart : TDateTime;
    outputFile : String;

    Function AddString(Const s : String):Cardinal;
    Function Compare(pA, pB : Pointer) : Integer;
    function CompareRefSetByConcept(pA, pB : Pointer): Integer;

//    function GetStatus(iStatus: Integer): TSnomedConceptStatus;
//    function GetCharacteristic(iStatus: Integer): TRelationshipCharacteristic;
//    function GetRefinability(iStatus: Integer): TRelationshipRefinability;
//    function GetGroup(iStatus: Integer): byte;
    procedure ImportSnomed;
    procedure ReadConceptsFile;
    procedure ReadDescriptionsFile;
    Procedure LoadReferenceSets(path : String); overload;
    Procedure LoadReferenceSets; overload;
    Procedure CloseReferenceSets(); overload;
    Procedure LoadReferenceSet(sFile : String);
    Procedure SeeDesc(sDesc : String; iConceptIndex : Integer; iFlags : Byte);
    Procedure SeeWord(sDesc : String; iConceptIndex : Integer; iFlags : Byte);
    procedure ReadRelationshipsFile;
    Procedure BuildClosureTable;
    Procedure SetDepths(aRoots : UInt64Array);
    Procedure SetDepth(focus : Cardinal; iDepth : Byte);
    Function BuildClosure(iConcept : Cardinal) : TCardinalArray;
    Procedure SaveConceptLinks(var active, inactive : UInt64Array);
    Function ListChildren(iConcept: Cardinal) : TCardinalArray;

    Function GetConcept(aId : Uint64; var iIndex : Integer) : TConcept;
    procedure Progress(msg : String);
    function readDate(s: String): TSnomedDate;
    procedure QuickSortPairsByName(var a: TSnomedReferenceSetMemberArray);
  public
    Constructor Create; override;
    Destructor Destroy; override;
    procedure Go;
    Property ConceptFiles : TStringList read FConceptFiles;
    Property RelationshipFiles : TStringList read FRelationshipFiles;
    Property DescriptionFiles : TStringList read FDescriptionFiles;
    Property DirectoryReferenceSets : String read FDirectoryReferenceSets write FDirectoryReferenceSets;
    Property Status : Integer read FStatus Write FStatus;
    Property Key : Integer read FKey write FKey;
    Property CacheDir : String read FCacheDir write FCacheDir;
    Property RF2 : boolean read FRF2 write FRF2;
  end;

function importSnomedRF1(dir : String; dest : String) : String;
function importSnomedRF2(dir : String; dest : String) : String;

Implementation


function ReadFirstLine(filename : String):String;
var
  t : Text;
begin
  AssignFile(t, filename);
  Reset(t);
  readln(t, result);
  CloseFile(t);
end;

function ExtractFileVersion(fn : String) : String;
begin
  result := ExtractFileName(fn);
  result := copy(result, 1, result.Length - extractFileExt(result).Length);
  result := copy(result, length(result)-7, 8);
end;

procedure AnalyseDirectory(dir : String; imp : TSnomedImporter);
var
  sr: TSearchRec;
  s : String;
begin
  if FindFirst(IncludeTrailingPathDelimiter(dir) + '*.*', faAnyFile, sr) = 0 then
  begin
    repeat
      if (sr.Attr = faDirectory) then
      begin
        if SameText(sr.Name, 'Reference Sets') then
          imp.DirectoryReferenceSets := IncludeTrailingPathDelimiter(dir) + sr.Name
        else if not (StringstartsWith(sr.Name, '.'))  then
          AnalyseDirectory(IncludeTrailingPathDelimiter(dir) + sr.Name, imp);
      end
      else if ExtractFileExt(sr.Name) = '.txt' then
      begin
        s := ReadFirstLine(IncludeTrailingPathDelimiter(dir) + sr.Name);
        if (s.StartsWith('CONCEPTID'#9'CONCEPTSTATUS'#9'FULLYSPECIFIEDNAME'#9'CTV3ID'#9'SNOMEDID'#9'ISPRIMITIVE')) then
        begin
          imp.ConceptFiles.Add(IncludeTrailingPathDelimiter(dir) + sr.Name);
          if imp.Fversion = '' then
            imp.Fversion := ExtractFileVersion(imp.ConceptFiles[0]);
        end
        else if (s.StartsWith('DESCRIPTIONID'#9'DESCRIPTIONSTATUS'#9'CONCEPTID'#9'TERM'#9'INITIALCAPITALSTATUS'#9'DESCRIPTIONTYPE'#9'LANGUAGECODE')) then
          imp.DescriptionFiles.Add(IncludeTrailingPathDelimiter(dir) + sr.Name)
        else if (s.StartsWith('RELATIONSHIPID'#9'CONCEPTID1'#9'RELATIONSHIPTYPE'#9'CONCEPTID2'#9'CHARACTERISTICTYPE'#9'REFINABILITY'#9'RELATIONSHIPGROUP')) then
          imp.RelationshipFiles.add(IncludeTrailingPathDelimiter(dir) + sr.Name)
        else
          ;  // we ignore the file
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

procedure AnalyseDirectoryRF2(dir : String; imp : TSnomedImporter);
var
  sr: TSearchRec;
  s : String;
begin
  if FindFirst(IncludeTrailingPathDelimiter(dir) + '*.*', faAnyFile, sr) = 0 then
  begin
    repeat
      if (sr.Attr = faDirectory) then
      begin
        if SameText(sr.Name, 'Reference Sets') or SameText(sr.Name, 'RefSet') then
          imp.DirectoryReferenceSets := IncludeTrailingPathDelimiter(dir) + sr.Name
        else if not (StringstartsWith(sr.Name, '.'))  then
          AnalyseDirectoryRF2(IncludeTrailingPathDelimiter(dir) + sr.Name, imp);
      end
      else if ExtractFileExt(sr.Name) = '.txt' then
      begin
        s := ReadFirstLine(IncludeTrailingPathDelimiter(dir) + sr.Name);
        if (s.StartsWith('id'#9'effectiveTime'#9'active'#9'moduleId'#9'definitionStatusId')) then
        begin
          imp.ConceptFiles.Add(IncludeTrailingPathDelimiter(dir) + sr.Name);
          if imp.Fversion = '' then
            imp.Fversion := ExtractFileVersion(imp.ConceptFiles[0]);
        end
        else if (s.StartsWith('id'#9'effectiveTime'#9'active'#9'moduleId'#9'conceptId'#9'languageCode'#9'typeId'#9'term'#9'caseSignificanceId')) then
          imp.DescriptionFiles.add(IncludeTrailingPathDelimiter(dir) + sr.Name)
        else if (s.StartsWith('id'#9'effectiveTime'#9'active'#9'moduleId'#9'sourceId'#9'destinationId'#9'relationshipGroup'#9'typeId'#9'characteristicTypeId'#9'modifierId')) then
          imp.RelationshipFiles.Add(IncludeTrailingPathDelimiter(dir) + sr.Name)
        else
          ;  // we ignore the file
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

function importSnomedRF1(dir : String; dest : String) : String;
var
  imp : TSnomedImporter;
begin
  Write('Import Snomed (RF1) from '+dir);
  imp := TSnomedImporter.Create;
  try
    imp.CacheDir := dest;
    analyseDirectory(dir, imp);
    imp.Go;
    result := imp.outputFile;
  finally
    imp.Free;
  end;
end;

function importSnomedRF2(dir : String; dest : String) : String;
var
  imp : TSnomedImporter;
begin
  Write('Import Snomed (RF2) from '+dir);
  imp := TSnomedImporter.Create;
  try
    imp.RF2 := True;
    imp.CacheDir := dest;
    analyseDirectoryRF2(dir, imp);
    imp.Go;
    result := imp.outputFile;
  finally
    imp.Free;
  end;
end;

Function CompareUInt64(a,b : Uint64) : Integer;
begin
  if a > b Then
    result := 1
  Else if a < b THen
    result := -1
  Else
    Result := 0;
End;

{ TConcept }

constructor TConcept.Create;
begin
  inherited;
  Stems := TAdvIntegerList.Create;
  Stems.SortAscending;
end;

destructor TConcept.Destroy;
begin
  Stems.Free;
  inherited;
end;

{ TWordCache }

constructor TWordCache.create(aStem: String);
begin
  Stem := aStem;
end;


{ TSnomedImporter }


Function TSnomedImporter.Compare(pA, pB : Pointer) : Integer;
begin
  result := CompareUInt64(TConcept(pA).Identity, TConcept(pB).Identity);
End;

constructor TSnomedImporter.Create;
begin
  inherited;
  FImportFormat := TFormatSettings.Create('en-AU');
  FConceptFiles := TStringList.create;
  FRelationshipFiles := TStringList.create;
  FDescriptionFiles := TStringList.create;
end;

destructor TSnomedImporter.Destroy;
begin
  FConceptFiles.Free;
  FRelationshipFiles.Free;
  FDescriptionFiles.Free;
  inherited;
end;

function TSnomedImporter.AddString(const s: String): Cardinal;
var
  i : Integer;
begin
  if FStringsTemp.Find(s, i) Then
    result := Cardinal(FStringsTemp.Objects[i])
  Else
  Begin
    result := FStrings.AddString(s);
    FStringsTemp.AddObject(s, TObject(result));
  End;
end;

procedure TSnomedImporter.Go;
begin
  ImportSnomed;
//    Reg.SetSetting('\Manager\Import\Snomed', 'ConceptCount', inttostr(TotalConcepts));
//    Reg.SetSetting('\Manager\Import\Snomed', 'RelationshipCount', inttostr(TotalRelationships));
//    Reg.SetSetting('\Manager\Import\Snomed', 'DescriptionCount', inttostr(TotalDescriptions));
end;

Function MakeSafeFileName(sName : String; newkey : Integer):String;
var
  i : integer;
Begin
  result := '';
  for i := 1 to length(sName) Do
    if CharInSet(sName[i], ['a'..'z', '_', '-', 'A'..'Z', '0'..'9']) Then
      result := result + sName[i];
  if result = '' then
    result := inttostr(newkey);
End;

Procedure TSnomedImporter.ImportSnomed;
var
  oSvc : TSnomedServices;
  active, inactive : UInt64Array;
begin
  FStart := now;

  oSvc := TSnomedServices.Create;
  FWordList := TStringList.Create;
  FStemList := TStringList.Create;
  FStringsTemp := TStringList.Create;
  FConcepts := TAdvObjectList.Create;
  FStemmer := GetStemmer_8('english');
  Frefsets := TRefSetList.Create;
  try
    Frefsets.SortedByName;
    FWordList.Sorted := True;
    FStemList.Sorted := True;
    oSvc.Version := FVersion;
    FStrings := oSvc.Strings;
    FRefs := oSvc.Refs;
    FDesc := oSvc.Desc;
    FDescRef := oSvc.DescRef;

    FConcept := oSvc.Concept;
    FRel := oSvc.Rel;
    FRefsetIndex := oSvc.RefSetIndex;
    FRefsetMembers := oSvc.RefSetMembers;
    FWords := oSvc.Words;
    FStems := oSvc.Stems;

    FStrings.StartBuild;
    FRefs.StartBuild;
    FDesc.StartBuild;
    FConcept.StartBuild;
    FRel.StartBuild;
    FRefsetIndex.StartBuild;
    FRefsetMembers.StartBuild;

    ReadConceptsFile;
    ReadDescriptionsFile;
    FStrings.DoneBuild;
    FDesc.DoneBuild;
    ReadRelationshipsFile;
    FRel.DoneBuild;
    FRefs.Post;
    SaveConceptLinks(active, inactive);
    oSvc.ActiveRoots := active;
    oSvc.InActiveRoots := inactive;
    oSvc.Is_A_Index := Findex_is_a;
    BuildClosureTable;
    LoadReferenceSets;
    FRefsetIndex.DoneBuild;
    FRefsetMembers.DoneBuild;
    FRefs.DoneBuild;
    SetDepths(oSvc.ActiveRoots);

    Progress('#16 Save');

    if not DirectoryExists(FCacheDir) then
      CreateDir(ExtractFilePath(FCacheDir));
    outputFile := IncludeTrailingPathDelimiter(FCacheDir)+'snomed_'+FVersion+'.cache';
    oSvc.Save(outputFile);
    // SetFileReadOnly(sFilename, true);
  Finally
    Frefsets.free;
    FWordList.Free;
    FStemList.Free;
    FStringsTemp.Free;
    FConcepts.Free;
    oSvc.Free;
    FStemmer.Free;
  End;
End;

Function DescFlag(iStatus : Byte; bCaps : Boolean; iStyle : Byte) : Byte;
Begin
  result := iStatus + iStyle shl 4;
  if bCaps then
    result := result + MASK_DESC_CAPS;
End;

function LoadFile(filename : String):TBytes;
var
  f : TFileStream;
begin
  f := TFileStream.Create(filename, fmOpenRead + fmShareDenyWrite);
  try
    SetLength(result, f.Size);
    f.Read(result[0], f.Size);
  finally
    f.Free;
  end;
end;

function TSnomedImporter.readDate(s : String) : TSnomedDate;
var
  d : TDateTime;
begin
  assert(s.Length = 8, 'Date length is not 8 ('+s+')');
  s := copy(s, 7, 2)+'/'+copy(s, 5, 2)+'/'+copy(s, 1, 4);
  d := StrToDate(s, FImportFormat);
  assert(trunc(d) < 65535);
  result := trunc(d);
end;

Procedure TSnomedImporter.ReadConceptsFile;
var
  s :TBytes;
  iCursor : Integer;
  fi : integer;
  iStart : Integer;
  iConcept : Integer;
  iStatus : integer;
  iDate : integer;
  iIndex : Cardinal;
  iTemp : Cardinal;
  iDesc : Integer;
//  iv3Id : Integer;
  iId : integer;
  iCount : Integer;
  iLast : UInt64;
  iModule : integer;
  iTerm : Uint64;
  iEntry : Integer;

  oConcept : TConcept;
  iLoop : Integer;
  Function Next(ch : Byte) : integer;
  begin
    inc(iCursor);
    While (iCursor <= length(s)) And (s[iCursor] <> ch) do
      inc(iCursor);
    result := iCursor;
  End;
Begin
  Progress('#1 Read Concept File');
  for fi := 0 to ConceptFiles.Count - 1 do
  begin
    s := LoadFile(ConceptFiles[fi]);
    iCursor := -1;
    iCount := 0;
    iCursor := Next(13) + 2;
    While iCursor < Length(s) Do
    Begin
      iStart := iCursor;
      if RF2 then
      begin
        iConcept := Next(9);
        iDate := Next(9);
        iStatus := Next(9);
        iModule := Next(9);
        iCursor := Next(13); // also is status
        iDesc := 0;
        iId := 0;
      end
      else
      begin
        iConcept := Next(9);
        iDate := 0;
        iStatus := Next(9);
        iDesc := Next(9);
        {iv3Id := }Next(9);
        iId := Next(9);
        iCursor := Next(13);
        iModule := 0;
      end;

      iTerm := StrToUInt64(ascopy(s, iStart, iConcept - iStart));
      oConcept := TConcept.Create;
      iEntry := FConcepts.Add(oConcept);
  //      if TrackConceptDuplicates then
  //        FConceptMap.Add(iTerm, iEntry);
      oConcept.Identity := iTerm;
  //    else
  //    begin
  //      oConcept := FConcepts[FConceptMap.Matches[iTerm]] as TConcept;
  //      assert(oConcept.Identity = iTerm);
  //    end;

      if not RF2 then
      begin
        SetLength(oConcept.FDescriptions, 1);
        oConcept.FSN := ascopy(s, iStatus+1, (iDesc - iStatus) - 1);
        oConcept.FDescriptions[0] := FDesc.AddDescription(AddString(oConcept.FSN), 0, 0, 0, 0, 0, DescFlag(FLAG_Active, true, VAL_DESC_FullySpecifiedName));
        oConcept.Flag := strtoint(ascopy(s, iConcept+1, iStatus - iConcept -1));
        if ascopy(s, iId, iCursor - iId - 1) = '1' Then
          oConcept.Flag := oConcept.Flag + MASK_CONCEPT_PRIMITIVE;
      end
      else
      begin
        oConcept.FDate := readDate(ascopy(s, iConcept+1, iDate - iConcept -1));
        if ascopy(s, iDate+1, iStatus - iDate -1) <> '1' then
          oConcept.Flag := 1;
        oConcept.FModuleId := StrToUInt64(ascopy(s, iStatus+1, iModule - iStatus-1));
        oConcept.FStatus := StrToUInt64(ascopy(s, iModule+1, iCursor - iModule-1));
        if oConcept.FStatus = RF2_MAGIC_PRIMITIVE then
          oConcept.Flag := oConcept.Flag + MASK_CONCEPT_PRIMITIVE;
      end;
      oConcept.Active := oConcept.Flag and MASK_CONCEPT_STATUS = 0;

      inc(iCursor, 2);
      inc(OverallCount);
      if OverallCount mod 5001 = 0 then
        Progress('');
      inc(iCount);
    End;
  End;

  Progress('#2 Sort Concepts');
  FConcepts.SortedBy(Compare);

  Progress('#3 Build Concept Cache');
  iLast := 0;
  for iLoop := 0 to FConcepts.Count - 1 Do
  begin
    oConcept := TConcept(FConcepts[iLoop]);
    if oConcept.Identity <= iLast then
      Raise Exception.Create('out of order at '+inttostr(oConcept.Identity)+' (last was '+inttostr(ilast)+')'); // if you get this, change the value of TrackConceptDuplicates to true
    iLast := oConcept.Identity;
    inc(OverallCount);
    oConcept.Index := FConcept.AddConcept(oConcept.Identity, oConcept.FDate, oConcept.Flag);
    if OverallCount mod 5001 = 0 then
      Progress('');
  End;
  FConcept.DoneBuild;

  for iLoop := 0 To FConcepts.Count - 1 Do
  begin
    oConcept := TConcept(FConcepts[iLoop]);
    inc(OverallCount);
    if OverallCount mod 5001 = 0 then
      Progress('');
    if not FConcept.FindConcept(oConcept.Identity, iIndex) or (iIndex <> oConcept.index) Then
      raise exception.create('unable to find a concept in the concept list it is in: '+inttostr(oConcept.Identity)+'['+inttostr(iLoop)+']');
    if RF2 then
    begin
      // resolve moduleid and status id
      if not FConcept.FindConcept(oConcept.FModuleId, iTemp) then
        raise exception.create('unable to resolve module id: '+inttostr(oConcept.FModuleId));
      FConcept.SetModuleId(iIndex, iTemp);
      if not FConcept.FindConcept(oConcept.FStatus, iTemp) then
        raise exception.create('unable to resolve status: '+inttostr(oConcept.FStatus));
      FConcept.SetStatus(iIndex, iTemp);
    end;
  End;
End;

Type
  TIndex = Record
    id : Uint64;
    ref : Cardinal;
  End;
  TIndexArray = array of TIndex;

Procedure QuickSortIndex(var a : TIndexArray);

  Procedure QuickSort(L, R: Integer);
  Var
    I, J, K : Integer;
    t : TIndex;
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
        While a[I].id < a[K].id Do
          Inc(I);

        While a[J].id > a[K].id Do
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

{
procedure TSnomedImporter.SelectedPreferredTerms;
var
  oConcept : TConcept;
  identity : UInt64;
  ParentIndex, DescriptionIndex : Cardinal;
  InboundIndex, outboundIndex, iString, iDummy : Cardinal;
  iDescriptions, iList : TCardinalArray;
  date : TSnomedDate;
  v, vv : String;
  i, j, vi : integer;
  Flags : byte;
  module, refsets, kind : Cardinal;
begin
  Progress('#15 Find System Preferred Terms');
  for j := 0 to FConcepts.Count - 1 do
  begin
    inc(OverallCount);
    if OverallCount mod 5001 = 0 then
      Progress('');
    oConcept := TConcept(FConcepts[j]);
    FConcept.GetConcept(oConcept.index, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex);
    iDescriptions := FRefs.GetReferences(DescriptionIndex);
    vv := '';
    vi := 0;
    For i := Low(iDescriptions) To High(iDescriptions) Do
    Begin
      FDesc.GetDescription(iDescriptions[i], iString, Identity, date, iDummy,  module, kind, refsets, Flags);
      iList := GetDescRefsets(iDescriptions[i]);
      v := FStrings.GetEntry(iString);
      if ((vv = '') or (length(vv) > length(v))) and (flags and MASK_DESC_STATUS = FLAG_Active) And (Length(iList) > 0) Then
      begin
        vv := v;
        vi := i;
      end;
    end;
    if (vv <> '') then
      FDesc.MarkAsSystemPreferred(iDescriptions[vi]);
  end;
end;
}

procedure TSnomedImporter.ReadDescriptionsFile;
var
  s : TBytes;
  iCount, iCursor : Integer;
  iStart, iId, iStatus, iConcept, iTerm, iCaps, iType, iLang, iDate, iModuleId, iConceptStart, iTermStart : Integer;
  iFlag : Byte;
  oConcept : TConcept;
  iDescId : UInt64;
  sDesc : String;
  i, j, iStem : integer;
  oList : TAdvIntegerList;
  aCardinals : TCardinalArray;
  iConceptIndex : integer;
  aIndex : TIndexArray;
  aIndexLength : Integer;
  iRef, module, kind : Cardinal;
  iSt : integer;
  ikind : UInt64;
  date : TSnomedDate;
  fi : integer;

  Function Next(ch : byte) : integer;
  begin
    inc(iCursor);
    While (iCursor <= length(s)) And (s[iCursor] <> ch) do
      inc(iCursor);
    result := iCursor;
  End;
begin
  Progress('#4 Read Description File');
  SetLength(aIndex, 10000);
  aIndexLength := 0;
  iCount := 0;
  for fi := 0 to DescriptionFiles.Count - 1 do
  begin
    s := LoadFile(DescriptionFiles[fi]);
    iCursor := -1;
    iCursor := Next(13) + 2;
    While iCursor < Length(s) Do
    Begin
      iStart := iCursor;
      if RF2 then
      begin
        iId := Next(9);
        iDate := Next(9);
        iStatus := Next(9);
        iModuleId := Next(9);
        iConceptStart := iModuleId;
        iConcept := Next(9);
        iLang := Next(9);
        iType := Next(9);
        iTermStart := iType;
        iTerm := Next(9);
        iCaps := Next(13);
      end
      else
      begin
        iId := Next(9);
        iStatus := Next(9);
        iConceptStart := iStatus;
        iConcept := Next(9);
        iTermStart := iConcept;
        iTerm := Next(9);
        iCaps := Next(9);
        iType := Next(9);
        iCursor := Next(13);
        iLang := iCursor; // lang (which is ignored)
        iDate := 0;
        iModuleId := 0;
        module := 0;
        kind := 0;
      end;

      iDescId := StrToUInt64(ascopy(s, iStart, (iId - iStart)));
      if iDescId = 135851013 then
        writeln('135851013 encountered');

      oConcept := GetConcept(StrToUInt64(ascopy(s, iConceptStart+1, (iConcept - iConceptStart)-1)), iConceptIndex);
      if oConcept = nil then
        raise exception.create('unable to find Concept in desc ('+ascopy(s, iStatus+1, (iConcept - iStatus)-1)+')');

      if RF2 then
      begin
        ascopy(s, iType+1, (iTerm - iType) - 1);
        if not FConcept.FindConcept(StrtoUInt64(ascopy(s, iStatus+1, (iModuleId - iStatus) - 1)), module) then
          raise Exception.Create('Unable to find desc module '+ascopy(s, iStatus+1, (iModuleId - iStatus) - 1));
        iKind := StrtoUInt64(ascopy(s, iLang+1, (iType - iLang) - 1));
        if not FConcept.FindConcept(iKind, kind) then
          raise Exception.Create('Unable to find desc type '+ascopy(s, iLang+1, (iType - iLang) - 1));

        date := ReadDate(ascopy(s, iId+1, (iDate - iId) - 1));
        iSt := strtoint(ascopy(s, iDate+1, (iStatus - iDate) -1)); // we'll have to go update it later based on caps and type
        if iSt = 1 then
          iSt := FLAG_Active
        else
          iSt := FLAG_RetiredWithoutStatedReason;

        if iKind = RF2_MAGIC_FSN then
          iFlag := DescFlag(iSt, false {todo}, VAL_DESC_FullySpecifiedName)
        else
          iFlag := DescFlag(iSt, false {todo}, VAL_DESC_Unspecified);
      end
      else
      begin
        date := 0;
        iFlag := DescFlag(strtoint(ascopy(s, iId+1, (iStatus - iId) -1)), ascopy(s, iTerm+1, (iCaps - iTerm) - 1) = '1', StrToInt(ascopy(s, iCaps+1, (iType - iCaps) - 1)));
      end;

      sDesc := ascopy(s, iTermStart+1, (iTerm - iTermStart) - 1);
      SeeDesc(sDesc, iConceptIndex, iFlag);

      if RF2 or (iFlag and MASK_DESC_STYLE <> VAL_DESC_FullySpecifiedName shl 4) Or (oConcept.FSN <> sDesc) Then
      Begin
        SetLength(oConcept.FDescriptions, length(oConcept.FDescriptions)+1);
        iRef := FDesc.AddDescription(AddString(sDesc), iDescId, date, oConcept.Index, module, kind, iFlag);
        oConcept.FDescriptions[Length(oConcept.FDescriptions)-1] := iRef;
      End
      Else
      Begin
        iRef := oConcept.FDescriptions[0];
        FDesc.UpdateDetails(iRef, iDescId, oConcept.Index);
      End;

      if aIndexLength = Length(aIndex) Then
        SetLength(aIndex, Length(aIndex)+10000);
      aIndex[aIndexLength].id := iDescId;
      aIndex[aIndexLength].ref := iRef;
      inc(aIndexLength);


      inc(iCursor, 2);
      inc(OverallCount);
      if OverallCount mod 15001 = 0 then
        Progress('');
      inc(iCount);
    End;
  end;
  Progress('#5 Sort Descriptions');
  SetLength(aIndex, aIndexLength);
  QuickSortIndex(aIndex);
  Progress('#6 Build Description cache');
  FDescRef.StartBuild;
  For i := 0 to Length(aIndex) - 1 Do
  Begin
    if OverallCount mod 5001 = 0 then
      Progress('');
    FDescRef.AddDescription(aIndex[i].id, aIndex[i].ref);
  End;
  FDescRef.DoneBuild;

  Progress('#7 Process Words');
  FWords.StartBuild;
  For i := 0 to FWordList.Count - 1 Do
  Begin
    if OverallCount mod 5011 = 0 then
      Progress('');
    iFlag := TWordCache(FWordList.Objects[i]).Flags;
    iFlag := iFlag xor FLAG_WORD_DEP; // reverse usage
    FWords.AddWord(FStrings.AddString(FWordList[i]), iFlag);
    FWordList.Objects[i].Free;
    inc(OverallCount);
  End;
  FWords.DoneBuild;

  Progress('#8 Process Stems');
  FStems.StartBuild;
  For i := 0 to FStemList.Count - 1 Do
  Begin
    if OverallCount mod 5011 = 0 then
      Progress('');
    oList := TAdvIntegerList(FStemList.Objects[i]);
    SetLength(aCardinals, oList.Count);
    for j := 0 to oList.Count - 1 Do
      aCardinals[j] := TConcept(FConcepts[oList[j]]).Index;
    iStem := FStrings.AddString(FStemList[i]);
    FStems.AddStem(iStem, FRefs.AddReferences(aCardinals));
    for j := 0 to oList.Count - 1 Do
      TConcept(FConcepts[oList[j]]).Stems.Add(iStem);
    oList.Free;
    inc(OverallCount);
  End;
  FStems.DoneBuild;
  Progress('#9 Mark Stems');
  For i := 0 to FConcepts.Count - 1 Do
  Begin
    if OverallCount mod 5001 = 0 then
      Progress('');
    oConcept := TConcept(FConcepts[i]);
    SetLength(aCardinals, oConcept.Stems.Count);
    for j := 0 to oConcept.Stems.Count - 1 do
      aCardinals[j] := oConcept.Stems[j];
    FConcept.SetStems(oConcept.Index, FRefs.AddReferences(aCardinals));
    inc(OverallCount);
  End;
End;


Procedure TSnomedImporter.ReadRelationshipsFile;
var
  s : TBytes;
  iCursor : Integer;
 // iStart : Integer;
  iRelId : Integer;
  iModuleId : integer;
  iC1Id : Integer;
  iRTId : Integer;
  iStatus : Integer;
  iC2Id : Integer;
  iCtype : Integer;
  iRef : integer;
  iDate : integer;
  fi : integer;

  iCount : Integer;
 // iIndex : Integer;
  oSource : TConcept;
  oTarget : TConcept;
  oRelType : TConcept;
  module: cardinal;
  kind : cardinal;
  modifier : cardinal;
  date : TSnomedDate;

  i_c, i_r : Integer;
  iFlag : Byte;
  iGroup : integer;
  iIndex : integer;
  sGroup : String;
  active : boolean;
  Function Next(ch : byte) : integer;
  begin
    inc(iCursor);
    While (iCursor <= length(s)) And (s[iCursor] <> ch) do
      inc(iCursor);
    result := iCursor;
  End;
  Function GetConceptLocal(iStart, iEnd : Integer):TConcept;
  var
    sId : String;
    iDummy : Integer;
  Begin
    sId := ascopy(s, iStart+1, iEnd - iStart-1);
    result := GetConcept(StrToUInt64(sId), iDummy);
    if result = nil Then
      Raise Exception.Create('Unable to resolve the term reference '+sId+' in the relationships file');
  End;
Begin
  Progress('#10 Read Relationship File');
  if not FConcept.FindConcept(IS_A_MAGIC, Findex_is_a) Then
    Raise exception.Create('is-a concept not found ('+inttostr(IS_A_MAGIC)+')');
  for fi := 0 to RelationshipFiles.Count - 1 do
  begin
    s := LoadFile(RelationshipFiles[fi]);
    iCursor := -1;
    iCount := 0;
    iCursor := Next(13) + 2;
    While iCursor < Length(s) Do
    Begin
      if RF2 then
      begin
        iRelId := Next(9);
        iDate := Next(9);
        iStatus := Next(9);
        iModuleId := Next(9);
        iC1Id := Next(9);
        iC2Id := Next(9);
        iGroup := Next(9);
        iRTId := Next(9);
        iCtype := Next(9);
        iCursor := Next(13);
        iRef := iCursor;
        i_c := 0; // todo
        i_r := 0; // todo
        iFlag := 0; // todo;
        active := ascopy(s, iDate+1, iStatus - iDate-1) = '1';
        if (not active) then
          iFlag := VAL_REL_Historical;
        oSource := GetConceptLocal(iModuleId, iC1Id);
        oRelType := GetConceptLocal(iGroup, iRTId);
        oTarget := GetConceptLocal(iC1Id, iC2Id);
        date := ReadDate(ascopy(s, iRelId+1, (iDate - iRelId) - 1));
        module := GetConceptLocal(iStatus, iModuleId).Index;
        kind := GetConceptLocal(iRTId, iCtype).Index;
        modifier := GetConceptLocal(iCtype, iRef).Index;
        iGroup := StrToInt(ascopy(s, iC2Id+1, iGroup - iC2Id-1));
      end
      else
      begin
        iRelId := Next(9);
        iC1Id := Next(9);
        iRTId := Next(9);
        iC2Id := Next(9);
        iCtype := Next(9);
        iRef := Next(9);
        iCursor := Next(13);
        iGroup := iCursor;
        iModuleId := 0;

        i_c := strtoint(ascopy(s, iC2Id+1, iCtype - iC2Id -1));
        active := i_c = 0;
        i_r := strtoint(ascopy(s, iCtype+1, iRef - iCtype -1));
        iFlag := i_c + i_r shl 4;
        sGroup := ascopy(s, iRef+1, iCursor - iRef -1);
        if pos(#9, sGroup) > 0 Then
          iGroup := strtoint(copy(sGroup, 1, pos(#9, sGroup) - 1))
        Else
          iGroup := strtoint(sGroup);
        oSource := GetConceptLocal(iRelId, iC1Id);
        oRelType := GetConceptLocal(iC1Id, iRTId);
        oTarget := GetConceptLocal(iRTId, iC2Id);
        date := 0;
        module := 0;
        kind := 0;
        modifier := 0;
      end;


      iIndex := FRel.AddRelationship(oSource.Index, oTarget.Index, oRelType.Index, module, kind, modifier, date, iFlag, iGroup);
      if (oRelType.Index = Findex_is_a) and (active) Then
      Begin
        SetLength(oSource.FParents, Length(oSource.FParents)+1);
        oSource.FParents[Length(oSource.FParents)-1] := oTarget.Index;
      End;
      SetLength(oSource.FOutbounds, Length(oSource.FOutbounds)+1);
      oSource.FOutbounds[Length(oSource.FOutbounds)-1].relationship := iIndex;
      oSource.FOutbounds[Length(oSource.FOutbounds)-1].characteristic := i_c;
      oSource.FOutbounds[Length(oSource.FOutbounds)-1].Refinability := i_r;
      oSource.FOutbounds[Length(oSource.FOutbounds)-1].Group := iGroup;
      SetLength(oTarget.FInbounds, Length(oTarget.FInbounds)+1);
      oTarget.FInbounds[Length(oTarget.FInbounds)-1].relationship := iIndex;
      oTarget.FInbounds[Length(oTarget.FInbounds)-1].characteristic := i_c;
      oTarget.FInbounds[Length(oTarget.FInbounds)-1].Refinability := i_r;
      oTarget.FInbounds[Length(oTarget.FInbounds)-1].Group := iGroup;
      inc(iCursor, 2);
      inc(OverallCount);
      if OverallCount mod 15001 = 0 then
        Progress('');
      inc(iCount);
    End;
  End;
End;


function TSnomedImporter.GetConcept(aId: Uint64; var iIndex : Integer): TConcept;
var
  oConcept : TConcept;
begin
  oConcept := TConcept.Create;
  Try
    oConcept.Identity := aId;
    iIndex := FConcepts.IndexBy(oConcept, Compare);
  Finally
    oConcept.Free;
  End;
  if iIndex >= 0 Then
    result := TConcept(FConcepts[iIndex])
  Else
    result := nil;
end;

Function SortRelationshipArray(a : TRelationshipArray):TCardinalArray;
  Function Compare(const a, b: TRelationship) : Integer;
  Begin
    result := a.characteristic - b.characteristic;
    if result = 0 then
      result := a.Group - b.group;
    if result = 0 then
      result := a.Refinability - b.Refinability;
    if result = 0 then
      result := integer(a.relationship) - integer(b.relationship)
  End;

  Procedure QuickSort(L, R: Integer);
  Var
    I, J, K : Integer;
    t : TRelationship;
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
        While Compare(a[I], a[K]) < 0 Do
          Inc(I);

        While Compare(a[J], a[K]) > 0 Do
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

var
  i : integer;
Begin
  If length(a) > 1 Then
    QuickSort(0, length(a) - 1);
  SetLength(result, length(a));
  For i := 0 to length(result) - 1 do
    result[i] := a[i].relationship;
End;

Procedure TSnomedImporter.SaveConceptLinks(var active, inactive : UInt64Array);
var
  iLoop : integer;
  oConcept : TConcept;
  iIndex : Cardinal;
  aCards : TCardinalArray;
begin
  Progress('#11 Cross-Link Relationships');
  SetLength(aCards, 0);
  SetLength(active, 0);
  SetLength(inactive, 0);
  for iLoop := 0 To FConcepts.Count - 1 Do
  begin
    oConcept := TConcept(FConcepts[iLoop]);

    if not FConcept.FindConcept(oConcept.Identity, iIndex) then
      raise exception.create('import error 1');
    if not iIndex = oConcept.index then
      raise exception.create('import error 2');

    if Length(oConcept.FParents) <> 0 Then
      FConcept.SetParents(oConcept.Index, FRefs.AddReferences(oConcept.FParents))
    else if oConcept.active then
    begin
      SetLength(active, length(active)+1);
      active[length(active) - 1] := oConcept.Identity;
    end
    else
    Begin
      SetLength(inactive, length(inactive)+1);
      inactive[length(inactive) - 1] := oConcept.Identity;
    End;
    FConcept.SetDescriptions(oConcept.index, FRefs.AddReferences(oConcept.FDescriptions));

    aCards := SortRelationshipArray(oConcept.FInBounds);
    FConcept.SetInbounds(oConcept.index, FRefs.AddReferences(aCards));
    aCards := SortRelationshipArray(oConcept.FOutbounds);
    FConcept.SetOutbounds(oConcept.index, FRefs.AddReferences(aCards));
    inc(OverallCount);
    if OverallCount mod 5001 = 0 then
      Progress('');
  End;
  if length(active) = 0 Then
    Raise Exception.Create('no roots found');
end;

procedure TSnomedImporter.BuildClosureTable;
var
  i : integer;
begin
  Progress('#12 Build Closure Table');
  for i := 0 to FConcepts.Count - 1 do
  begin
    BuildClosure(TConcept(FConcepts[i]).Index);
  End;
end;

Procedure QuickSortArray(var a : TCardinalArray);

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
        While a[I] < a[K] Do
          Inc(I);

        While a[J] > a[K] Do
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



Function TSnomedImporter.ListChildren(iConcept: Cardinal) : TCardinalArray;
var
  i, l : Integer;
  o : TCardinalArray;
  src, tgt, rel, w1,w2,w3 : Cardinal;
  date : TSnomedDate;
  flg, grp : Byte;
Begin
  SetLength(result, 1000);
  l := 0;
  o := FRefs.GetReferences(FConcept.getInBounds(iConcept));
  For i := 0 to Length(o) - 1 Do
  Begin
    FRel.GetRelationship(o[i], src, tgt, rel, w1,w2,w3, date, flg, grp);
    if (rel = Findex_is_a) and (flg = 0) Then
    Begin
      if l >= length(result) Then
        SetLength(result, length(result)+ 1000);
      result[l] := src;
      inc(l);
    End;
  End;
  SetLength(result, l);
  QuickSortArray(result);
End;

Const
  MAGIC_IN_PROGRESS = MAGIC_NO_CHILDREN - 1;

Function TSnomedImporter.BuildClosure(iConcept: Cardinal) : TCardinalArray;
var
  iDesc : Cardinal;
  aChildren : TCardinalArray;
  aAllDesc : Array of TCardinalArray;
  aIndexes : Array of Integer;
  i, j, c, l : integer;
  v, m : cardinal;
  ic : UInt64;
begin
  ic := FConcept.getConceptId(iConcept);
 // writeln('Close '+inttostr(ic));
  SetLength(aChildren, 0);
  iDesc := FConcept.GetAllDesc(iConcept);
  if iDesc = MAGIC_IN_PROGRESS Then
    raise Exception.Create('Circular relationship to '+inttostr(FConcept.getConceptId(iConcept)))
  else if iDesc = MAGIC_NO_CHILDREN Then
    result := nil
  Else if iDesc <> 0 Then
    result := FRefs.GetReferences(iDesc)
  Else
  Begin
    if OverallCount mod 5001 = 0 then
      Progress('');
    inc(OverallCount);

    FConcept.SetAllDesc(iConcept, MAGIC_IN_PROGRESS);
    aChildren := ListChildren(iConcept);

    c := length(aChildren);
    SetLength(aAllDesc, c+1);
    SetLength(aIndexes, c+1);
    for i := 0 to c - 1 Do
      aAllDesc[i] := BuildClosure(aChildren[i]);
    aAllDesc[c] := aChildren;

    for i := 0 to c Do
      aIndexes[i] := 0;

    SetLength(result, 10000);
    l := 0;

    repeat
      j := -1;
      if l = 0 then
        v := 0
      else
        v := result[l-1];
      m := 0;
      for i := 0 to c Do
      begin
        if aIndexes[i] < Length(aAllDesc[i]) Then
        Begin
          assert(v <= aAlldesc[i][aIndexes[i]]);
          while (aIndexes[i] < Length(aAllDesc[i])) and (v = aAlldesc[i][aIndexes[i]]) Do
            inc(aIndexes[i]);
          if (aIndexes[i] < Length(aAllDesc[i])) And ((m = 0) or (aAlldesc[i][aIndexes[i]] < m)) Then
          Begin
            m := aAlldesc[i][aIndexes[i]];
            j := i;
          End;
        End;
      End;
      if j > -1 Then
      Begin
        if l >= Length(Result) then
          SetLength(result, Length(result)+1000);
        result[l] := aAlldesc[j][aIndexes[j]];
        inc(l);
        inc(aIndexes[j]);
      End;

    until j = -1;

    SetLength(result, l);
    for i := 0 to length(result)-2 do
      assert(result[i] < result[i+1]);

    if (l = 0) Then
      FConcept.SetAllDesc(iConcept, MAGIC_NO_CHILDREN)
    else
      FConcept.SetAllDesc(iConcept, FRefs.AddReferences(result));
  End;
end;





procedure TSnomedImporter.SeeDesc(sDesc: String; iConceptIndex : Integer; iFlags: Byte);
var
  s : String;
begin
  while (sDesc <> '') Do
  Begin
    StringSplit(sdesc, [',', ' ', ':', '.', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '{', '}', '[', ']', '|', '\', ';', '"', '<', '>', '?', '/', '~', '`', '-', '_', '+', '='],
      s, sdesc);
    if (s <> '') And not StringIsInteger32(s) Then
      SeeWord(s, iConceptIndex, iFlags);
  End;
end;

procedure TSnomedImporter.SeeWord(sDesc: String; iConceptIndex : Integer; iFlags: Byte);
var
  i, m : integer;
  oList : TAdvIntegerList;
  oWord : TWordCache;
begin
  sDesc := lowercase(sdesc);
  if not FWordList.Find(sDesc, i) Then
    i := FWordList.AddObject(sdesc, TWordCache.Create(FStemmer.calc(sDesc)));
  oWord := TWordCache(FWordList.Objects[i]);
  m := oWord.Flags;
  case (iFlags and MASK_DESC_STYLE) shr 4 of
    VAL_DESC_Preferred : m := m or FLAG_WORD_PN;
    VAL_DESC_FullySpecifiedName : m := m or FLAG_WORD_DSN;
  End;
  if iflags and MASK_DESC_STATUS = FLAG_Active Then
    m := m or FLAG_WORD_DEP; // note it being used backwards here - set to true if anything is active
  oWord.Flags := m;

  if not FStemList.Find(oWord.Stem, i) Then
  Begin
    oList := TAdvIntegerList.Create;
    oList.SortAscending;
    FStemList.AddObject(oWord.Stem, oList);
  End
  Else
    oList := TAdvIntegerList(FStemList.Objects[i]);
  if not oList.ExistsByValue(iConceptIndex) Then
    oList.Add(iConceptIndex);
End;



procedure TSnomedImporter.SetDepths(aRoots : UInt64Array);
var
  i : integer;
  j : cardinal;
begin
  Progress('#15 Set Concept Depths');
  for i := 0 to Length(aRoots) - 1 do
  Begin
    FConcept.FindConcept(aRoots[i], j);
    SetDepth(j, 0);
  End;
end;

procedure TSnomedImporter.SetDepth(focus: Cardinal; iDepth: Byte);
var
  aChildren : TCardinalArray;
  d : byte;
  i : integer;
begin
  SetLength(aChildren, 0);
  d := FConcept.GetDepth(focus);
  if (d = 0) or (d > iDepth) Then
  Begin
    FConcept.SetDepth(focus, iDepth);
    if iDepth = 255 Then
      Raise exception.create('too deep');
    inc(iDepth);
    aChildren := ListChildren(focus);
    for i := 0 to length(aChildren) - 1 Do
      SetDepth(aChildren[i], iDepth);
  End;
end;


function TSnomedImporter.CompareRefSetByConcept(pA, pB : Pointer): Integer;
begin
  if TRefSet(pA).index > TRefSet(pB).index then
    result := 1
  else if TRefSet(pA).index < TRefSet(pB).index then
    result := -1
  else
    result := 0;
end;
{
Procedure QuickSortRefsets(var a : TReferenceSetArray);

  Procedure QuickSort(L, R: Integer);
  Var
    I, J, K : Integer;
    t : TReferenceSet;
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
        While (a[I].Concept < a[K].concept) Do
          Inc(I);

        While (a[J].concept > a[K].concept) Do
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
}

procedure TSnomedImporter.LoadReferenceSets(path : String);
var
  sr: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(path) + '*.*', faAnyFile, sr) = 0 then
  begin
    repeat
      if (sr.Attr = faDirectory) then
      begin
        if not StringStartsWith(sr.Name, '.') then
          LoadReferenceSets(IncludeTrailingPathDelimiter(path) + sr.Name);
      end
      else if (sr.Attr <> faDirectory) and (ExtractFileExt(sr.Name) = '.txt') then
      begin
        LoadReferenceSet(IncludeTrailingPathDelimiter(path) + sr.Name);
        Progress('');
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
  CloseReferenceSets;
end;

procedure TSnomedImporter.LoadReferenceSets;
var
  i, j, c : integer;
  refset : TRefSet;
  conc : TConcept;
  refs : TCardinalArray;
  ndx : Cardinal;
  iDesc : Cardinal;
  id : UInt64;
  date : Word;
  concept, module, kind, irefsets : Cardinal;
  flags : byte;
begin
    Progress('#13 Importing Reference Sets');
    if FDirectoryReferenceSets <> '' Then
      LoadReferenceSets(FDirectoryReferenceSets);
    Progress('#14 Sorting Reference Sets');
    FRefsets.SortedBy(CompareRefSetByConcept);
    Progress('');
    for i := 0 to FRefSets.Count - 1 Do
    begin
      refset := FRefsets[i] as TRefSet;
      FRefsetindex.AddReferenceSet(refset.index, refset.membersByRef, refset.membersByName);
    end;

    Progress('#15 Indexing Reference Sets');
    for i := 0 to FConcepts.Count - 1 do
    begin
      if (i mod 5000 = 0) then
        Progress('');

      conc := FConcepts[i] as TConcept;
      setLength(refs, FRefsets.Count);
      c := 0;
      for j := 0 to FRefsets.Count - 1 do
      begin
        refset := Frefsets[j] as TRefSet;
        if refset.contains(conc.index) then
        begin
          refs[c] := refset.index;
          inc(c);
        end;
      end;
      Setlength(refs, c);
      if c > 0 then
      begin
        ndx := FRefs.AddReferences(refs);
        FConcept.SetRefsets(conc.Index, ndx);
      end;
    end;

    for i := 0 to FDesc.Count - 1 do
    begin
      if (i mod 5000 = 0) then
        Progress('');

      FDesc.GetDescription(i * DESC_SIZE+1, iDesc, id, date, concept, module, kind, irefsets, flags);
      setLength(refs, Frefsets.Count);
      c := 0;
      for j := 0 to Frefsets.Count - 1 do
      begin
        refset := Frefsets[j] as TRefSet;
        if refset.contains(iDesc) then
        begin
          refs[c] := refset.index;
          inc(c);
        end;
      end;
      Setlength(refs, c);
      if c > 0 then
      begin
        ndx := FRefs.AddReferences(refs);
        FDesc.SetRefsets(i * DESC_SIZE, ndx);
      end;
    end;
end;

Procedure QuickSortPairs(var a : TSnomedReferenceSetMemberArray);

  Procedure QuickSort(L, R: Integer);
  Var
    I, J, K : Integer;
    t : TSnomedReferenceSetMember;
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
        While (a[I].Ref < a[K].Ref) Do
          Inc(I);

        While (a[J].Ref > a[K].Ref) Do
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

Procedure TSnomedImporter.QuickSortPairsByName(var a : TSnomedReferenceSetMemberArray);

  Function Desc(Const r : TSnomedReferenceSetMember):String;
  var
    id, id2 : cardinal;
    i : Integer;
    Identity : UInt64;
    Flags : Byte;
//    Parents : Cardinal;
    Descriptions : Cardinal;
//    Inbounds : Cardinal;
//    outbounds : Cardinal;
    dummy, module, refsets, kind : Cardinal;
    Descs : TCardinalArray;
    date : TSnomedDate;
  Begin
    SetLength(Descs, 0);
    result := '';
    if r.kind = 1 then
      FDesc.GetDescription(r.Ref, id, identity, date, dummy, module, kind, refsets, Flags)
    Else
    Begin
      descriptions := FConcept.GetDescriptions(r.ref);
      if descriptions = 0 then
        exit;
      Descs := FRefs.GetReferences(descriptions);
      id := 0;
      For i := 0 to Length(Descs)- 1 Do
      Begin
       FDesc.GetDescription(Descs[i], id2, identity, date, dummy, module, kind, refsets, Flags);
       if (Flags and MASK_DESC_STATUS = FLAG_Active) And (Flags and MASK_DESC_STYLE shr 4 in [VAL_DESC_Preferred]) Then
         id := id2;
      End;
      if id = 0 Then
      Begin
        For i := 0 to Length(Descs)- 1 Do
        Begin
         FDesc.GetDescription(Descs[i], id2, identity, date, dummy, module, kind, refsets, Flags);
         if (Flags and MASK_DESC_STATUS = FLAG_Active) Then
           id := id2;
        End;
      End;
    End;
    if id = 0 then
      result := ''
    Else
      result := FStrings.GetEntry(id);
  End;

  Procedure QuickSort(L, R: Integer);
  Var
    I, J, K : Integer;
    t : TSnomedReferenceSetMember;
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
        While Desc(a[I]) < Desc(a[K]) Do
          Inc(I);

        While Desc(a[J]) > Desc(a[K]) Do
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


procedure TSnomedImporter.LoadReferenceSet(sFile: String);
var
  s : TBytes;
  iId, iTime, iActive, iModule, iRefSetId, iRefComp : Integer;
  sActive, sRefSetId, sRefComp : String;

  iTermRef, iRefsetRef : Cardinal;
  iCursor : int64;

  bDesc : byte;

  refset : TRefSet;
  ok : boolean;

  Function Next(ch : Byte) : integer;
  begin
    inc(iCursor);
    While (iCursor < length(s)) And (s[iCursor] <> ch) and (s[iCursor] <> 13) do
      inc(iCursor);
    result := iCursor;
  End;
begin
  s := LoadFile(sFile);
  iCursor := 0;
  iCursor := Next(13) + 2;
  While iCursor < Length(s) Do
  Begin
    iId := Next(9);
    iTime := Next(9);
    iActive := Next(9);
    iModule := Next(9);
    iRefSetId := Next(9);
    iRefComp := Next(9);
    iCursor := Next(13); // for now, we ignore additional fields

    sActive := ascopy(s, iTime+1, iActive - (iTime + 1));
    sRefSetId := ascopy(s, iModule+1, iRefSetId - (iModule + 1));
    sRefComp := ascopy(s, iRefSetId+1, iRefComp - (iRefSetId + 1));

    if sActive = '1' Then
    Begin
      refSet := Frefsets.GetRefset(sRefSetId);
      if refset.index = MAGIC_NO_CHILDREN then
        if not FConcept.FindConcept(StrToUInt64(sRefSetId), iRefsetRef) then
           raise exception.create('unable to find term '+sRefSetId+' for reference set '+sFile)
        else
          refset.index := iRefsetRef;

      ok := true;
      if FConcept.FindConcept(StrToUInt64(sRefComp), iTermRef) then
        bDesc := 0
      else if FDescRef.FindDescription(StrToUInt64(sRefComp), iTermRef) then
        bDesc := 1
      Else
      begin
        Writeln('Unknown component '+sRefComp+' in '+sFile);
        ok := false;
      end;

      if ok then
      begin
        if (refset.iMemberLength = length(refset.aMembers)) Then
          SetLength(refset.aMembers, length(refset.aMembers)+10000);
        refset.aMembers[refset.iMemberLength].kind := bDesc;
        refset.aMembers[refset.iMemberLength].Ref := iTermRef;
        inc(refset.iMemberLength);
      end;
    End;

    inc(iCursor, 2);
  End;
end;

Procedure TSnomedImporter.CloseReferenceSets;
var
  i : integer;
  RefSet : TRefSet;
begin
  for i := 0 to Frefsets.Count - 1  do
  begin
    refset := Frefsets[i] as TRefSet;
    SetLength(refset.aMembers, refset.iMemberLength);
    QuickSortPairsByName(refset.aMembers);
    refset.membersByName := FRefsetMembers.AddMembers(refset.aMembers);
    QuickSortPairs(refset.aMembers);
    refset.membersByRef := FRefsetMembers.AddMembers(refset.aMembers);
  end;
end;


procedure TSnomedImporter.Progress(Msg : String);
begin
  if (msg <> '') then
  begin
    Writeln('           '+DescribePeriod(now - FStart));

    write(msg)
  end
  else
    write('.');
end;


{ TRefSetList }

function TRefSetList.GetRefset(id: String): TRefset;
var
  iIndex : integer;
begin
  iIndex := IndexByName(id);
  if (iIndex > -1) then
    result := ObjectByIndex[iIndex] as TRefSet
  else
  begin
    result := TRefSet.Create;
    try
      result.Name := id;
      SetLength(result.aMembers, 10000);
      result.iMemberLength := 0;
      result.index := MAGIC_NO_CHILDREN;
      add(result.Link);
    finally
      result.Free;
    end;
  end;
end;

{
function TSnomedImporter.GetDescRefsets(iDesc: Cardinal): TCardinalArray;
var
  i : integer;
  iDefinition, iMembersByRef, iMembersByName: Cardinal;
  bDescSet : Boolean;
  aMembers : TSnomedReferenceSetMemberArray;
  iDummy : Cardinal;
begin
  SetLength(result, 0);
  SetLength(aMembers, 0);
  for i := 0 to FRefSetIndex.Count - 1 Do
  Begin
    FRefSetIndex.GetReferenceSet(i, iDefinition, iMembersByRef, iMembersByName);
    aMembers := FRefSetMembers.GetMembers(iMembersByRef);
    if FindMember(aMembers, iDesc, iDummy) Then
    begin
      SetLength(result, length(result)+1);
      result[length(result)-1] := iDefinition;
    End;
  End;
end;
}

{ TRefSet }

function TRefSet.contains(term: cardinal): boolean;
var
  L, H, I : Integer;
  ndx : Cardinal;
begin
  Result := False;
  L := 0;
  H := length(aMembers) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    ndx := aMembers[i].Ref;
    if ndx < term then
      L := I + 1
    else
    begin
      H := I - 1;
      if ndx = term then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
end;

End.
