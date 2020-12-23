unit ftx_sct_importer;

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

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  SysUtils, Classes, Inifiles, Generics.Collections,
  fsl_base, fsl_stream, fsl_utilities, fsl_collections, fsl_fpc,
  ftx_loinc_services, ftx_sct_services, ftx_sct_expressions;

//Const
//  TrackConceptDuplicates = false; // much slower, but only if you aren't reading a snapshot

Const
  UPDATE_FREQ = 501;
  UPDATE_FREQ_D = 1501;

  STEP_START = 0;
  STEP_READ_CONCEPT = 1;
  STEP_SORT_CONCEPT = 2;
  STEP_BUILD_CONCEPT_CACHE = 3;
  STEP_READ_DESC = 4;
  STEP_SORT_DESC = 5;
  STEP_BUILD_DESC_CACHE = 6;
  STEP_PROCESS_WORDS = 7;
  STEP_PROCESS_STEMS = 8;
  STEP_MARK_STEMS = 9;
  STEP_READ_REL = 10;
  STEP_CONCEPT_LINKS = 11;
  STEP_BUILD_CLOSURE = 12;
  STEP_IMPORT_REFSET = 13;
  STEP_SORT_REFSET = 14;
  STEP_INDEX_REFSET = 15;
  STEP_SET_DEPTH = 16;
  STEP_NORMAL_FORMS = 17;
  STEP_END = 18;
  STEP_TOTAL = 18;

Type

  TRelationship = record
    relationship : Cardinal;
    characteristic : Integer;
    Refinability : Integer;
    Group : Integer;
  End;

  TRelationshipArray = array of TRelationship;

  TRefSet = class (TFslName)
  private
    index, filename : Cardinal;
    noStoreIds : boolean;
    isLangRefset : boolean;
    title : String;
    aMembers : TSnomedReferenceSetMemberArray;
    iMemberLength : Integer;
    membersByRef : Cardinal;
    membersByName : Cardinal;
    fieldTypes : Cardinal;
    fieldNames : Cardinal;

    function contains(term : cardinal; var values : cardinal) : boolean;
  end;

  TRefSetList = class (TFslNameList)
  public
    function GetRefset(id : String) : TRefset;
  end;

  TConcept = class (TFslObject)
  Private
    Index : Cardinal;
    Identity : UInt64;
    FModuleId : UInt64;
    FStatus : UInt64;
    Flag : Byte;
    FDate : TSnomedDate;
    FInBounds : TRelationshipArray;
    FOutbounds : TRelationshipArray;
    FActiveParents : TCardinalArray;
    FInActiveParents : TCardinalArray;
    descCount : integer;
    FDescriptions : TCardinalArray;
    Active : boolean;
//    FClosed : Boolean;
//    FClosure : TCardinalArray;
    Stems : TFslIntegerList;
  public
    constructor Create; Override;
    destructor Destroy; Override;
  End;

  TWordCache = class (TFslObject)
  public
    Flags : Integer;
    Word, Stem : String;
    constructor Create(word, aStem : String);
  End;

  { TStemCache }

  TStemCache = class (TFslObject)
  private
    stem : String;
    values : TFslIntegerList;
  public
    constructor Create(stem : String);
    destructor Destroy; override;
  end;

  TSnomedImporter = class (TFslObject)
  private
    callback : TInstallerCallback;
    lastmessage : String;
    FUsingBase : boolean;

    FSvc : TSnomedServices;
    FRefSetTypes : TDictionary<String, System.cardinal>;

    FConceptFiles : TStringList;
    FRelationshipFiles : TStringList;
    FDescriptionFiles : TStringList;
    OverallCount : Integer;
    ClosureCount : Integer;

    FStringsTemp : TDictionary<String, Cardinal>;
    FConcepts : TFslObjectList;

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
    FRels : TDictionary<UInt64, Cardinal>;
    FDuplicateRelationships : TStringList;

    FVersionUri : String;
    FVersionDate : String;
    Findex_is_a : Cardinal;
    FWordList : TFslMap<TWordCache>;
    FStemList : TFslMap<TStemCache>;
    FStemmer : TFslWordStemmer;

    FStatus: Integer;
    FKey: Integer;
    FDirectoryReferenceSets: TStringList;
    FStart : TDateTime;
    FoutputFile : String;

    FFSN : cardinal;

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
    Procedure LoadReferenceSets(pfxLen : integer; path : String; var count : integer); overload;
    Procedure LoadReferenceSets; overload;
    Procedure CloseReferenceSets(); overload;
    Procedure LoadReferenceSet(pfxLen : integer; sFile : String; isLangRefset : boolean);
    Procedure SeeDesc(sDesc : String; iConceptIndex : Integer; active, fsn : boolean);
    Procedure SeeWord(sDesc : String; iConceptIndex : Integer; active, fsn : boolean; stem : String);
    procedure ReadRelationshipsFile;
    Procedure BuildClosureTable;
    Procedure BuildNormalForms;
    Procedure SetDepths(aRoots : UInt64Array);
    Procedure SetDepth(focus : Cardinal; iDepth : Byte);
    Function BuildClosure(iConcept : Cardinal) : TCardinalArray;
    Procedure SaveConceptLinks(var active, inactive : UInt64Array);
    Function ListChildren(iConcept: Cardinal) : TCardinalArray;

    Function GetConcept(aId : Uint64; var iIndex : Integer) : TConcept;
    procedure Progress(Step : integer; pct : real; msg : String);
    procedure QuickSortPairsByName(var a: TSnomedReferenceSetMemberArray);
    procedure SetVersion(s : String);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Go;
    Property usingBase : boolean read FUsingBase write FUsingBase;
    Property ConceptFiles : TStringList read FConceptFiles;
    Property RelationshipFiles : TStringList read FRelationshipFiles;
    Property DescriptionFiles : TStringList read FDescriptionFiles;
    Property DirectoryReferenceSets : TStringList read FDirectoryReferenceSets;
    Property Status : Integer read FStatus Write FStatus;
    Property Key : Integer read FKey write FKey;
    Property OutputFile : String read FOutputFile write FOutputFile;
  end;

function needsBaseForImport(moduleId : String): boolean;
function importSnomedRF2(dir, base : String; dest, uri : String; callback : TInstallerCallback = nil) : String;

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
  result := copy(result, 1, result.Length - Length(extractFileExt(result)));
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
          imp.DirectoryReferenceSets.Add(IncludeTrailingPathDelimiter(dir) + sr.Name)
        else if not (StringstartsWith(sr.Name, '.'))  then
          AnalyseDirectory(IncludeTrailingPathDelimiter(dir) + sr.Name, imp);
      end
      else if ExtractFileExt(sr.Name) = '.txt' then
      begin
        s := ReadFirstLine(IncludeTrailingPathDelimiter(dir) + sr.Name);
        if (s.StartsWith('CONCEPTID'#9'CONCEPTSTATUS'#9'FULLYSPECIFIEDNAME'#9'CTV3ID'#9'SNOMEDID'#9'ISPRIMITIVE')) then
        begin
          imp.ConceptFiles.Add(IncludeTrailingPathDelimiter(dir) + sr.Name);
        end
        else if (s.StartsWith('DESCRIPTIONID'#9'DESCRIPTIONSTATUS'#9'CONCEPTID'#9'TERM'#9'INITIALCAPITALSTATUS'#9'DESCRIPTIONTYPE'#9'LANGUAGECODE')) then
          imp.DescriptionFiles.Add(IncludeTrailingPathDelimiter(dir) + sr.Name)
        else if (s.StartsWith('RELATIONSHIPID'#9'CONCEPTID1'#9'RELATIONSHIPTYPE'#9'CONCEPTID2'#9'CHARACTERISTICTYPE'#9'REFINABILITY'#9'RELATIONSHIPGROUP')) and (pos('StatedRelationship', sr.Name) = 0) then
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
          imp.DirectoryReferenceSets.Add(IncludeTrailingPathDelimiter(dir) + sr.Name)
        else if not (StringstartsWith(sr.Name, '.'))  then
          AnalyseDirectoryRF2(IncludeTrailingPathDelimiter(dir) + sr.Name, imp);
      end
      else if ExtractFileExt(sr.Name) = '.txt' then
      begin
        s := ReadFirstLine(IncludeTrailingPathDelimiter(dir) + sr.Name);
        if (s.StartsWith('id'#9'effectiveTime'#9'active'#9'moduleId'#9'definitionStatusId')) then
        begin
          imp.ConceptFiles.Add(IncludeTrailingPathDelimiter(dir) + sr.Name);
        end
        else if (s.StartsWith('id'#9'effectiveTime'#9'active'#9'moduleId'#9'conceptId'#9'languageCode'#9'typeId'#9'term'#9'caseSignificanceId')) then
          imp.DescriptionFiles.add(IncludeTrailingPathDelimiter(dir) + sr.Name)
        else if (s.StartsWith('id'#9'effectiveTime'#9'active'#9'moduleId'#9'sourceId'#9'destinationId'#9'relationshipGroup'#9'typeId'#9'characteristicTypeId'#9'modifierId')) and (pos('StatedRelationship', sr.Name) = 0) then
          imp.RelationshipFiles.Add(IncludeTrailingPathDelimiter(dir) + sr.Name)
        else
          ;  // we ignore the file
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

function importSnomedRF2(dir, base : String; dest, uri : String; callback : TInstallerCallback = nil) : String;
var
  imp : TSnomedImporter;
begin
  imp := TSnomedImporter.Create;
  try
    imp.callback := callback;
    imp.progress(STEP_START, 0, 'Import Snomed (RF2) from '+dir);
    imp.setVersion(uri);
    imp.usingBase := base <> '';
    imp.OutputFile := dest;
    if (base <> '') then
    begin
      analyseDirectoryRF2(base, imp);
    end;
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
  Else if a < b Then
    result := -1
  Else
    Result := 0;
End;

{ TStemCache }

constructor TStemCache.Create(stem: String);
begin
  inherited create;
  values := TFslIntegerList.Create;
  values.SortAscending;
end;

destructor TStemCache.Destroy;
begin
  values.free;
  inherited Destroy;
end;

{ TConcept }

constructor TConcept.Create;
begin
  inherited;
  Stems := TFslIntegerList.Create;
  Stems.SortAscending;
end;

destructor TConcept.Destroy;
begin
  Stems.Free;
  inherited;
end;

{ TWordCache }

constructor TWordCache.create(word, aStem: String);
begin
  inherited create;
  self.word := word;
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
  FConceptFiles := TStringList.create;
  FRelationshipFiles := TStringList.create;
  FDescriptionFiles := TStringList.create;
  FDirectoryReferenceSets := TStringList.create;
  FRels := TDictionary<UInt64,cardinal>.create;
  FDuplicateRelationships := TStringList.create;
  FRefSetTypes := TDictionary<String, cardinal>.create;
end;

destructor TSnomedImporter.Destroy;
begin
  FRefSetTypes.free;
  FDuplicateRelationships.Free;
  FDirectoryReferenceSets.free;
  FRels.Free;
  FConceptFiles.Free;
  FRelationshipFiles.Free;
  FDescriptionFiles.Free;
  inherited;
end;

function TSnomedImporter.AddString(const s: String): Cardinal;
begin
  if not FStringsTemp.tryGetValue(s, result) then
  begin
    result := FStrings.AddString(s);
    FStringsTemp.add(s, result);
  end;
end;

procedure TSnomedImporter.Go;
begin
  if FVersionUri = '' then
    raise ETerminologySetup.create('The full version URI must be provided');
  if FVersionDate = '' then
    raise ETerminologySetup.create('The full version URI must be provided');
  ImportSnomed;
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
  active, inactive : UInt64Array;
  s : String;
begin
  FStart := now;

  FSvc := TSnomedServices.Create;
  FWordList := TFslMap<TWordCache>.Create('wordmap');
  FStemList := TFslMap<TStemCache>.Create('stemmap');
  FStringsTemp := TDictionary<String, Cardinal>.Create;
  FConcepts := TFslObjectList.Create;
  FStemmer := TFslWordStemmer.create('english');
  Frefsets := TRefSetList.Create;
  try
    FSvc.Building := true;
    Frefsets.SortedByName;
    FSvc.VersionUri := FVersionUri;
    FSvc.VersionDate := FVersionDate;
    FStrings := FSvc.Strings;
    FRefs := FSvc.Refs;
    FDesc := FSvc.Desc;
    FDescRef := FSvc.DescRef;

    FConcept := FSvc.Concept;
    FRel := FSvc.Rel;
    FRefsetIndex := FSvc.RefSetIndex;
    FRefsetMembers := FSvc.RefSetMembers;
    FWords := FSvc.Words;
    FStems := FSvc.Stems;

    FStrings.StartBuild;
    FRefs.StartBuild;
    FDesc.StartBuild;
    FConcept.StartBuild;
    FRel.StartBuild;
    FRefsetIndex.StartBuild;
    FRefsetMembers.StartBuild;

    ReadConceptsFile;
    ReadDescriptionsFile;
    FDesc.DoneBuild;
    ReadRelationshipsFile;
    FRel.DoneBuild;
    FRefs.Post;
    SaveConceptLinks(active, inactive);
    FSvc.ActiveRoots := active;
    FSvc.InActiveRoots := inactive;
    FSvc.Is_A_Index := Findex_is_a;
    BuildClosureTable;
    LoadReferenceSets;
    FRefsetIndex.DoneBuild;
    FRefsetMembers.DoneBuild;
    FRefs.DoneBuild;
    SetDepths(FSvc.ActiveRoots);
    BuildNormalForms;
    FStrings.DoneBuild;

    Progress(STEP_END, 0, 'Save');

    s := ExtractFilePath(FoutputFile);
    if not DirectoryExists(s) then
      CreateDir(s);
    FSvc.Save(outputFile);
    // SetFileReadOnly(sFilename, true);
  Finally
    Frefsets.free;
    FWordList.Free;
    FStemList.Free;
    FStringsTemp.Free;
    FConcepts.Free;
    FSvc.Free;
    FStemmer.Free;
  End;
End;

Function DescFlag(iStatus : Byte; bCaps : byte; iStyle : Byte) : Byte;
Begin
  result := iStatus + (iStyle shl 4) + bCaps;
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
//  iv3Id : Integer;
  iLast : UInt64;
  iModule : integer;
  iTerm : Uint64;

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
  Progress(STEP_READ_CONCEPT, 0, 'Read Concept Files');
  for fi := 0 to ConceptFiles.Count - 1 do
  begin
    s := LoadFile(ConceptFiles[fi]);
    iCursor := -1;
//    iCount := 0;
    iCursor := Next(13) + 2;
    While iCursor < Length(s) Do
    Begin
      iStart := iCursor;
      iConcept := Next(9);
      iDate := Next(9);
      iStatus := Next(9);
      iModule := Next(9);
      iCursor := Next(13); // also is status
//      iDesc := 0;
//      iId := 0;

      iTerm := StrToUInt64(memU8toString(s, iStart, iConcept - iStart));
      oConcept := TConcept.Create;
      FConcepts.Add(oConcept);
      oConcept.Identity := iTerm;

      oConcept.FDate := readDate(memU8toString(s, iConcept+1, iDate - iConcept -1));
      if memU8toString(s, iDate+1, iStatus - iDate -1) <> '1' then
        oConcept.Flag := 1;
      oConcept.FModuleId := StrToUInt64(memU8toString(s, iStatus+1, iModule - iStatus-1));
      oConcept.FStatus := StrToUInt64(memU8toString(s, iModule+1, iCursor - iModule-1));
      if oConcept.FStatus = RF2_MAGIC_PRIMITIVE then
        oConcept.Flag := oConcept.Flag + MASK_CONCEPT_PRIMITIVE;
      oConcept.Active := oConcept.Flag and MASK_CONCEPT_STATUS = 0;

      inc(iCursor, 2);
      inc(OverallCount);
      if OverallCount mod UPDATE_FREQ = 0 then
        Progress(STEP_READ_CONCEPT, iCursor / Length(s), 'Read Concept Files '+pct(iCursor, Length(s)));
    End;
  End;

  Progress(STEP_SORT_CONCEPT, 0, 'Sort Concepts');
  FConcepts.SortedBy(Compare);

  Progress(STEP_BUILD_CONCEPT_CACHE, 0, 'Build Concept Cache');
  iLast := 0;
  for iLoop := 0 to FConcepts.Count - 1 Do
  begin
    oConcept := TConcept(FConcepts[iLoop]);
    if oConcept.Identity <= iLast then
      raise ETerminologySetup.create('out of order at '+inttostr(oConcept.Identity)+' (last was '+inttostr(ilast)+')'); // if you get this, change the value of TrackConceptDuplicates to true
    iLast := oConcept.Identity;
    inc(OverallCount);
    oConcept.Index := FConcept.AddConcept(oConcept.Identity, oConcept.FDate, oConcept.Flag);
    if OverallCount mod UPDATE_FREQ = 0 then
      Progress(STEP_BUILD_CONCEPT_CACHE, iLoop / (FConcepts.Count*2), '');
    if oConcept.Identity = RF2_MAGIC_FSN then
      FFSN := oConcept.Index;
  End;
  FConcept.DoneBuild;

  for iLoop := 0 To FConcepts.Count - 1 Do
  begin
    oConcept := TConcept(FConcepts[iLoop]);
    inc(OverallCount);
    if OverallCount mod UPDATE_FREQ = 0 then
      Progress(STEP_BUILD_CONCEPT_CACHE, (FConcepts.Count+iLoop) / (FConcepts.Count*2), '');
    if not FConcept.FindConcept(oConcept.Identity, iIndex) or (iIndex <> oConcept.index) Then
      raise ETerminologySetup.create('unable to find a concept in the concept list it is in: '+inttostr(oConcept.Identity)+'['+inttostr(iLoop)+']');
    // resolve moduleid and status id
    if not FConcept.FindConcept(oConcept.FModuleId, iTemp) then
      raise ETerminologySetup.create('unable to resolve module id: '+inttostr(oConcept.FModuleId));
    FConcept.SetModuleId(iIndex, iTemp);
    if not FConcept.FindConcept(oConcept.FStatus, iTemp) then
      raise ETerminologySetup.create('unable to resolve status: '+inttostr(oConcept.FStatus));
    FConcept.SetStatus(iIndex, iTemp);
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

procedure TSnomedImporter.ReadDescriptionsFile;
var
  s : TBytes;
  iCursor, len : Integer;
  iStart, iId, iStatus, iConcept, iTerm, iCaps, iType, iLang, iDate, iModuleId, iConceptStart, iTermStart : Integer;
  oConcept : TConcept;
  iDescId, cid : UInt64;
  sDesc : String;
  i, j, iStem : integer;
  aCardinals : TCardinalArray;
  iConceptIndex : integer;
  aIndex : TIndexArray;
  aIndexLength : Integer;
  iRef, module, kind, caps : Cardinal;
  iSt : integer;
  ikind, ucaps : UInt64;
  date : TSnomedDate;
  fi : integer;
  active : boolean;
  iFlag, lang : Byte;
  wc : TWordCache;
  sc : TStemCache;
  Function Next(ch : byte) : integer;
  begin
    inc(iCursor);
    While (iCursor <= length(s)) And (s[iCursor] <> ch) do
      inc(iCursor);
    result := iCursor;
  End;
begin
  Progress(STEP_READ_DESC, 0, 'Read Description Files');
  SetLength(aIndex, 10000);
  aIndexLength := 0;
  for fi := 0 to DescriptionFiles.Count - 1 do
  begin
    s := LoadFile(DescriptionFiles[fi]);
    iCursor := -1;
    iCursor := Next(13) + 2;
    len := Length(s);
    While iCursor < len Do
    Begin
      iStart := iCursor;
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

      iDescId := StrToUInt64(memU8toString(s, iStart, (iId - iStart)));

      cid := StrToUInt64(memU8toString(s, iConceptStart+1, (iConcept - iConceptStart)-1));
      oConcept := GetConcept(cid, iConceptIndex);
      if oConcept = nil then
        raise ETerminologySetup.create('unable to find Concept '+IntToStr(cid)+' in desc file (descid = '+inttostr(iDescId)+')');

      memU8toString(s, iType+1, (iTerm - iType) - 1);
      if not FConcept.FindConcept(StrtoUInt64(memU8toString(s, iStatus+1, (iModuleId - iStatus) - 1)), module) then
        raise ETerminologySetup.create('Unable to find desc module '+memU8toString(s, iStatus+1, (iModuleId - iStatus) - 1));
      lang := readLang(memU8toString(s, iConcept+1, (iLang - iConcept) - 1));
      iKind := StrtoUInt64(memU8toString(s, iLang+1, (iType - iLang) - 1));
      if not FConcept.FindConcept(iKind, kind) then
        raise ETerminologySetup.create('Unable to find desc type '+memU8toString(s, iLang+1, (iType - iLang) - 1));

      date := ReadDate(memU8toString(s, iId+1, (iDate - iId) - 1));
      iSt := strtoint(memU8toString(s, iDate+1, (iStatus - iDate) -1)); // we'll have to go update it later based on caps and type
      active := iSt = 1;

      ucaps := StrtoUInt64(memU8toString(s, iTerm+1, (iCaps - iTerm) - 1));
      if not FConcept.FindConcept(ucaps, caps) then
        raise ETerminologySetup.create('Unable to find caps type '+memU8toString(s, iLang+1, (iType - iLang) - 1));

      sDesc := memU8toString(s, iTermStart+1, (iTerm - iTermStart) - 1);
      SeeDesc(sDesc, iConceptIndex, active, iKind = RF2_MAGIC_FSN);

      iRef := FDesc.AddDescription(AddString(sDesc), iDescId, date, oConcept.Index, module, kind, caps, active, lang);
      if oConcept.descCount = length(oConcept.FDescriptions) then
        SetLength(oConcept.FDescriptions, length(oConcept.FDescriptions)+10);
      oConcept.FDescriptions[oConcept.descCount] := iRef;
      inc(oConcept.descCount);

      if aIndexLength = Length(aIndex) Then
        SetLength(aIndex, Length(aIndex)+10000);
      aIndex[aIndexLength].id := iDescId;
      aIndex[aIndexLength].ref := iRef;
      inc(aIndexLength);

      inc(iCursor, 2);
      inc(OverallCount);

      if OverallCount mod UPDATE_FREQ_D = 0 then
        Progress(STEP_READ_DESC, iCursor / len, 'Read Description Files '+pct(iCursor, len));
    End;
  end;
  Progress(STEP_SORT_DESC, 0, 'Sort Descriptions');
  SetLength(aIndex, aIndexLength);
  QuickSortIndex(aIndex);
  Progress(STEP_BUILD_DESC_CACHE, 0, 'Build Description cache');
  FDescRef.StartBuild;
  For i := 0 to Length(aIndex) - 1 Do
  Begin
    if OverallCount mod UPDATE_FREQ = 0 then
      Progress(STEP_BUILD_DESC_CACHE, i / Length(aIndex), '');
    FDescRef.AddDescription(aIndex[i].id, aIndex[i].ref);
  End;
  FDescRef.DoneBuild;

  Progress(STEP_PROCESS_WORDS, 0, 'Process Words');
  FWords.StartBuild;
  i := 0;
  For wc in FWordList.values Do
  Begin
    if OverallCount mod 5011 = 0 then
      Progress(STEP_PROCESS_WORDS, i / FWordList.Count, '');
    iFlag := wc.Flags;
    iFlag := iFlag xor FLAG_WORD_DEP; // reverse usage
    FWords.AddWord(FStrings.AddString(wc.word), iFlag);
    inc(OverallCount);
    inc(i);
  End;
  FWords.DoneBuild;

  Progress(STEP_PROCESS_STEMS, 0, 'Process Stems');
  FStems.StartBuild;
  i := 0;
  For sc in FStemList.Values Do
  Begin
    if OverallCount mod 5011 = 0 then
      Progress(STEP_PROCESS_STEMS, i / FStemList.Count, '');
    SetLength(aCardinals, sc.values.Count);
    for j := 0 to sc.values.Count - 1 Do
      aCardinals[j] := TConcept(FConcepts[sc.values[j]]).Index;
    iStem := FStrings.AddString(sc.stem);
    FStems.AddStem(iStem, FRefs.AddReferences(aCardinals));
    for j := 0 to sc.values.Count - 1 Do
      TConcept(FConcepts[sc.values[j]]).Stems.Add(iStem);
    inc(OverallCount);
    inc(i);
  End;
  FStems.DoneBuild;
  Progress(STEP_MARK_STEMS, 0, 'Mark Stems');
  For i := 0 to FConcepts.Count - 1 Do
  Begin
    if OverallCount mod UPDATE_FREQ = 0 then
      Progress(STEP_MARK_STEMS, i / FConcepts.Count, '');
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
  iStart : integer;
  iStatus : Integer;
  iC2Id : Integer;
  iCtype : Integer;
  iRef : integer;
  iDate : integer;
  fi : integer;

 // iIndex : Integer;
  oSource : TConcept;
  oTarget : TConcept;
  oRelType : TConcept;
  module: cardinal;
  kind : TConcept;
  modifier : cardinal;
  date : TSnomedDate;

  i_c, i_r : Integer;
  iGroup, group: integer;
  iIndex : cardinal;
  active, defining : boolean;

  iRel : UInt64;
  line : integer;
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
    sId := memU8toString(s, iStart+1, iEnd - iStart-1);
    result := GetConcept(StrToUInt64(sId), iDummy);
    if result = nil Then
      raise ETerminologySetup.create('Unable to resolve the term reference '+sId+' in the relationships file at position '+inttostr(iStart)+' on line '+inttostr(line));
  End;
Begin
  FDuplicateRelationships.Clear;
  Progress(STEP_READ_REL, 0, 'Read Relationship Files');
  if not FConcept.FindConcept(IS_A_MAGIC, Findex_is_a) Then
    raise ETerminologySetup.create('is-a concept not found ('+inttostr(IS_A_MAGIC)+')');
  for fi := 0 to RelationshipFiles.Count - 1 do
  begin
    s := LoadFile(RelationshipFiles[fi]);
    line := 1; // going to skip the first line
    iCursor := -1;
    iCursor := Next(13) + 2;
    While iCursor < Length(s) Do
    Begin
      inc(line);
      iStart := iCursor;
      iRelId := Next(9);    // id
      iDate := Next(9);     // effectiveTime
      iStatus := Next(9);   // active
      iModuleId := Next(9); // moduleId
      iC1Id := Next(9);     // sourceId
      iC2Id := Next(9);     // destinationId
      iGroup := Next(9);    // relationshipGroup
      iRTId := Next(9);     // typeId
      iCtype := Next(9);    // characteristicTypeId
      iCursor := Next(13);  // modifierId
      iRef := iCursor;

      iRel := StrToUInt64(memU8toString(s, iStart, iRelid - iStart));
      date := ReadDate(memU8toString(s, iRelId+1, (iDate - iRelId) - 1));
      active := memU8toString(s, iDate+1, iStatus - iDate-1) = '1';
      module := GetConceptLocal(iStatus, iModuleId).Index;
      oSource := GetConceptLocal(iModuleId, iC1Id);
      oTarget := GetConceptLocal(iC1Id, iC2Id);
      group := StrToInt(memU8toString(s, iC2Id+1, iGroup - iC2Id-1));
      oRelType := GetConceptLocal(iGroup, iRTId);
      kind := GetConceptLocal(iRTId, iCtype);
      modifier := GetConceptLocal(iCtype, iRef).Index;
      defining := (kind.Identity = RF2_MAGIC_RELN_DEFINING) or (kind.Identity = RF2_MAGIC_RELN_STATED) or (kind.Identity = RF2_MAGIC_RELN_INFERRED);

      if FRels.containsKey(iRel) then
      begin
        if FUsingBase then
          FDuplicateRelationships.add(inttostr(iRel))
        else
          raise ETerminologySetup.create('Duplicate relationship id '+inttostr(iRel)+' at line '+inttostr(line));
      end
      else
      begin
        iIndex := FRel.AddRelationship(iRel, oSource.Index, oTarget.Index, oRelType.Index, module, kind.Index, modifier, date, active, defining, group);
        FRels.Add(iRel, iIndex);
        if (oRelType.Index = Findex_is_a) and (defining) Then
        Begin
          if (active) then
          begin
            SetLength(oSource.FActiveParents, Length(oSource.FActiveParents)+1);
            oSource.FActiveParents[Length(oSource.FActiveParents)-1] := oTarget.Index;
          end
          else
          begin
            SetLength(oSource.FInActiveParents, Length(oSource.FInActiveParents)+1);
            oSource.FInActiveParents[Length(oSource.FInActiveParents)-1] := oTarget.Index;
          end;
        end;
        i_c := 0; // todo
        i_r := 0; // todo
        SetLength(oSource.FOutbounds, Length(oSource.FOutbounds)+1);
        oSource.FOutbounds[Length(oSource.FOutbounds)-1].relationship := iIndex;
        oSource.FOutbounds[Length(oSource.FOutbounds)-1].characteristic := i_c; // for sorting
        oSource.FOutbounds[Length(oSource.FOutbounds)-1].Refinability := i_r; // for sorting
        oSource.FOutbounds[Length(oSource.FOutbounds)-1].Group := group;
        SetLength(oTarget.FInbounds, Length(oTarget.FInbounds)+1);
        oTarget.FInbounds[Length(oTarget.FInbounds)-1].relationship := iIndex;
        oTarget.FInbounds[Length(oTarget.FInbounds)-1].characteristic := i_c;
        oTarget.FInbounds[Length(oTarget.FInbounds)-1].Refinability := i_r;
        oTarget.FInbounds[Length(oTarget.FInbounds)-1].Group := group;
      End;
      inc(iCursor, 2);
      inc(OverallCount);
      if OverallCount mod UPDATE_FREQ_D = 0 then
        Progress(STEP_READ_REL, fi / RelationshipFiles.Count, 'Read Relationship Files '+pct(iCursor, Length(s)));
    End;
  End;
  if FDuplicateRelationships.Count > 0 then
  begin
    // todo: what should be done about these?
    // see https://confluence.ihtsdotools.org/display/DOCTSG/3.1+Generating+Dynamic+Snapshot+Views
    FDuplicateRelationships.add('more');
  end;
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
  Progress(STEP_CONCEPT_LINKS, 0, 'Cross-Link Relationships');
  SetLength(aCards, 0);
  SetLength(active, 0);
  SetLength(inactive, 0);
  for iLoop := 0 To FConcepts.Count - 1 Do
  begin
    oConcept := TConcept(FConcepts[iLoop]);

    if not FConcept.FindConcept(oConcept.Identity, iIndex) then
      raise ETerminologySetup.create('import error 1');
    if not iIndex = oConcept.index then
      raise ETerminologySetup.create('import error 2');

    if (Length(oConcept.FActiveParents) <> 0) or (Length(oConcept.FInactiveParents) <> 0) Then
      FConcept.SetParents(oConcept.Index, FRefs.AddReferences(oConcept.FActiveParents), FRefs.AddReferences(oConcept.FInactiveParents))
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
    SetLength(oConcept.FDescriptions, oConcept.descCount);
    FConcept.SetDescriptions(oConcept.index, FRefs.AddReferences(oConcept.FDescriptions));

    aCards := SortRelationshipArray(oConcept.FInBounds);
    FConcept.SetInbounds(oConcept.index, FRefs.AddReferences(aCards));
    aCards := SortRelationshipArray(oConcept.FOutbounds);
    FConcept.SetOutbounds(oConcept.index, FRefs.AddReferences(aCards));
    inc(OverallCount);
    if OverallCount mod UPDATE_FREQ = 0 then
      Progress(STEP_CONCEPT_LINKS, iLoop / FConcepts.Count, '');
  End;
  if length(active) = 0 Then
    raise ETerminologySetup.create('no roots found');
end;

procedure TSnomedImporter.BuildClosureTable;
var
  i : integer;
begin
  ClosureCount := 0;
  Progress(STEP_BUILD_CLOSURE, 0, 'Build Closure Table');
  for i := 0 to FConcepts.Count - 1 do
  begin
    if (i mod 10000 = 0) then
      Progress(STEP_BUILD_CLOSURE, 0, 'Build Closure Table '+pct(i, FConcepts.count));
    BuildClosure(TConcept(FConcepts[i]).Index);
  End;
end;

procedure TSnomedImporter.BuildNormalForms;
var
  i : integer;
  exp, n : TSnomedExpression;
  e : String;
begin
  Progress(STEP_NORMAL_FORMS, 0, 'Build Normal Forms');
  for i := 0 to FConcepts.Count - 1 do
  begin
    if i mod UPDATE_FREQ = 0 then
      Progress(STEP_NORMAL_FORMS, i / FConcepts.Count, 'Build Normal Forms '+pct(i, FConcepts.Count));
    exp := TSnomedExpression.Create;
    try
      exp.concepts.Add(TSnomedConcept.create(TConcept(FConcepts[i]).Index));
      n := FSvc.normaliseExpression(exp);
      try
        e := FSvc.renderExpression(n, sroMinimal);
        if e <> inttostr(TConcept(FConcepts[i]).Identity) then
          FConcept.SetNormalForm(TConcept(FConcepts[i]).Index, FStrings.AddString(e));
      finally
        n.Free;
      end;
    finally
      exp.Free;
    end;
  end;
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
  grp : Integer;
  did : UInt64;
  active, defining : boolean;
Begin
  SetLength(result, 1000);
  l := 0;
  o := FRefs.GetReferences(FConcept.getInBounds(iConcept));
  For i := 0 to Length(o) - 1 Do
  Begin
    FRel.GetRelationship(o[i], did, src, tgt, rel, w1,w2,w3, date, active, defining, grp);
    if (rel = Findex_is_a) and (active) Then
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
  SetLength(aChildren, 0);
  iDesc := FConcept.GetAllDesc(iConcept);
  if iDesc = MAGIC_IN_PROGRESS Then
    raise ETerminologySetup.create('Circular relationship to '+inttostr(ic))
  else if iDesc = MAGIC_NO_CHILDREN Then
    result := nil
  Else if iDesc <> 0 Then
    result := FRefs.GetReferences(iDesc)
  Else
  Begin
    inc(closureCount);
    if closureCount mod UPDATE_FREQ = 0 then
      Progress(STEP_BUILD_CLOSURE, closureCount / FConcepts.Count, '');
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





procedure TSnomedImporter.SeeDesc(sDesc: String; iConceptIndex : Integer; active, FSN : boolean);
var
  s, stem : String;
begin
  for s in sDesc.Split([',', ' ', ':', '.', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '{', '}', '[', ']', '|', '\', ';', '"', '<', '>', '?', '/', '~', '`', '-', '_', '-', '+', '=']) do
  begin
    if (s <> '') And not StringIsInteger32(s) and (s.length > 2) Then
    begin
      stem := FStemmer.Stem(s);
      if (stem <> '') then
        SeeWord(s, iConceptIndex, active, fsn, stem);
    end;
  End;
end;

procedure TSnomedImporter.SeeWord(sDesc: String; iConceptIndex : Integer; active, FSN : boolean; stem : String);
var
  m : integer;
  oStem : TStemCache;
  oWord : TWordCache;
begin
  sDesc := lowercase(sdesc);
  if not FWordList.TryGetValue(sDesc, oWord) Then
  begin
    oWord := TWordCache.Create(sDesc, stem);
    FWordList.add(sdesc, oWord);
  end;
  m := oWord.Flags;
  if FSN then
  m := m or FLAG_WORD_FSN;
  if Active Then
    m := m or FLAG_WORD_DEP; // note it being used backwards here - set to true if anything is active
  oWord.Flags := m;

  if not FStemList.TryGetValue(oWord.Stem, oStem) Then
  Begin
    oStem := TStemCache.create(oWord.stem);
    FStemList.Add(oWord.Stem, oStem);
  End;

  if not oStem.Values.ExistsByValue(iConceptIndex) Then
    oStem.Values.Add(iConceptIndex);
End;


procedure TSnomedImporter.SetDepths(aRoots : UInt64Array);
var
  i : integer;
  j : cardinal;
begin
  Progress(STEP_SET_DEPTH, 0, 'Set Concept Depths');
  for i := 0 to Length(aRoots) - 1 do
  Begin
    FConcept.FindConcept(aRoots[i], j);
    SetDepth(j, 0);
  End;
end;

procedure TSnomedImporter.SetVersion(s: String);
begin
  if (s = '') then
    raise ETerminologySetup.create('no snomed version provided');
  FVersionUri := s;
  FVersionDate := copy(s, length(s)-7, 8);
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
      raise ETerminologySetup.create('too deep');
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

procedure TSnomedImporter.LoadReferenceSets(pfxLen : integer; path : String; var count : integer);
var
  sr: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(path) + '*.*', faAnyFile, sr) = 0 then
  begin
    repeat
      if (sr.Attr = faDirectory) then
      begin
        if not StringStartsWith(sr.Name, '.') then
          LoadReferenceSets(pfxLen, IncludeTrailingPathDelimiter(path) + sr.Name, count);
      end
      else if (sr.Attr <> faDirectory) and (ExtractFileExt(sr.Name) = '.txt') then
      begin
        LoadReferenceSet(pfxLen, IncludeTrailingPathDelimiter(path) + sr.Name, path.endsWith('Language'));
        inc(count);
        Progress(STEP_IMPORT_REFSET, count / 300, '');
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

procedure TSnomedImporter.LoadReferenceSets;
var
  i, j, c : integer;
  refset : TRefSet;
  conc : TConcept;
  refs, vals : TCardinalArray;
  ndx : Cardinal;
  values : Cardinal;
  count : integer;
  dir : String;
begin
  count := 0;
  Progress(STEP_IMPORT_REFSET, 0, 'Importing Reference Sets');
  if FDirectoryReferenceSets.Count > 0 Then
  begin
    for dir in FDirectoryReferenceSets do
    begin
      LoadReferenceSets(dir.Length+1, dir, count);
    end;
  end;
  FStrings.DoneBuild;
  CloseReferenceSets;
  Progress(STEP_SORT_REFSET, 0, 'Sorting Reference Sets');
  FRefsets.SortedBy(CompareRefSetByConcept);
  Progress(STEP_SORT_REFSET, 0.5, '');
  FStrings.Reopen;
  for i := 0 to FRefSets.Count - 1 Do
  begin
    refset := FRefsets[i] as TRefSet;
    FRefsetindex.AddReferenceSet(FStrings.AddString(refset.title), refset.filename, refset.index, refset.membersByRef, refset.membersByName, refset.fieldTypes, refset.fieldNames);
  end;

  Progress(STEP_INDEX_REFSET, 0, 'Indexing Reference Sets');
  for i := 0 to FConcepts.Count - 1 do
  begin
    if (i mod 5000 = 0) then
      Progress(STEP_INDEX_REFSET, i / (FConcepts.Count + integer(FDesc.Count)), '');

    conc := FConcepts[i] as TConcept;
    setLength(refs, FRefsets.Count);
    setLength(vals, FRefsets.Count);
    c := 0;
    for j := 0 to FRefsets.Count - 1 do
    begin
      refset := Frefsets[j] as TRefSet;
      if refset.contains(conc.index, values) then
      begin
        refs[c] := refset.index;
        vals[c] := values;
        inc(c);
      end;
    end;
    Setlength(refs, c);
    Setlength(vals, c);
    if c > 0 then
    begin
      ndx := FRefs.AddReferences(refs);
      FConcept.SetRefsets(conc.Index, ndx{, FRefs.AddReferences(vals)});
    end;
  end;

  for i := 0 to FDesc.Count - 1 do
  begin
    if (i mod 5000 = 0) then
      Progress(STEP_INDEX_REFSET, (integer(FDesc.Count) + i) / (FConcepts.Count + integer(FDesc.Count)), '');

    setLength(refs, Frefsets.Count);
    setLength(vals, Frefsets.Count);
    c := 0;
    for j := 0 to Frefsets.Count - 1 do
    begin
      refset := Frefsets[j] as TRefSet;

      if refset.contains(i * DESC_SIZE, values) then
      begin
        refs[c] := refset.index;
        vals[c] := values;
        inc(c);
      end;
    end;
    Setlength(refs, c);
    Setlength(vals, c);
    if c > 0 then
    begin
      ndx := FRefs.AddReferences(refs);
      FDesc.SetRefsets(i * DESC_SIZE, ndx, FRefs.AddReferences(vals));
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
//    Parents : Cardinal;
    Descriptions : Cardinal;
//    Inbounds : Cardinal;
//    outbounds : Cardinal;
    dummy, module, refsets, valueses, kind, caps : Cardinal;
    Descs : TCardinalArray;
    active, defining : boolean;
    date : TSnomedDate;
    lang : byte;
  Begin
    SetLength(Descs, 0);
    result := '';
    if r.kind = 1 then
      FDesc.GetDescription(r.Ref, id, identity, date, dummy, module, kind, caps, refsets, valueses, active, lang)
    Else
    begin
      if r.kind = 2 then
      begin
        FRel.GetRelationship(r.Ref, identity, dummy, dummy, kind, dummy, dummy, dummy, date, active, defining, i);
        if not FRels.TryGetValue(identity, dummy) or (dummy <> r.Ref) then
          raise ETerminologySetup.create('SNOMED is broken: there''s a relationship '+inttostr(identity)+' in a ref set that does''t exist in the base specification');
      end
      else
        kind := r.ref;
      descriptions := FConcept.GetDescriptions(kind);
      if descriptions = 0 then
        exit;
      Descs := FRefs.GetReferences(descriptions);
      id := 0;
      For i := 0 to Length(Descs)- 1 Do
      Begin
       FDesc.GetDescription(Descs[i], id2, identity, date, dummy, module, kind, caps, refsets, valueses, active, lang);
       if (active) And (kind = FFSN) Then
         id := id2;
      End;
      if id = 0 Then
      Begin
        For i := 0 to Length(Descs)- 1 Do
        Begin
         FDesc.GetDescription(Descs[i], id2, identity, date, dummy, module, kind, caps, refsets, valueses, active, lang);
         if (active) Then
           id := id2;
        End;
      End;
    End;
    if id = 0 then
      result := ''
    Else
    try
      result := FStrings.GetEntry(id);
    except
      // writeln('problem: '+inttostr(r.kind));
    end;
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

function caToString(a : TCardinalArray) : string;
var
  i : integer;
begin
  if (length(a) = 0) then
    exit('');

  result := inttostr(a[0]);
  for i := 1 to high(a) do
    result := result + ',' + inttostr(a[i]);
end;

procedure TSnomedImporter.LoadReferenceSet(pfxLen : integer; sFile: String; isLangRefset : boolean);
var
  s : TBytes;
  i, iId, iTime, iDate, iActive, iModule, iRefSetId, iRefComp : Integer;
  sId, sActive, sRefSetId, sRefComp, sModule, sDate, ss : String;
  iRef : UInt64;

  iTermRef, iRefsetRef, iVal, iType, iMod : Cardinal;
  iCursor : UInt64;

  bDesc : byte;

  refset : TRefSet;
  ok : boolean;
  name, sname : String;
  types, offsets, values, fnames : TCardinalArray;
  svals : Array of String;
  parts : TArray<String>;
  ti : cardinal;
  fieldnames : TStringList;
  line : Integer;
  Function Next(ch : Byte) : integer;
  begin
    inc(iCursor);
    While (iCursor < length(s)) And (s[iCursor] <> ch) and (s[iCursor] <> 13) do
      inc(iCursor);
    result := iCursor;
  End;
begin
  Progress(STEP_IMPORT_REFSET, 0, 'Import Reference Set '+ExtractFileName(sFile));
  ss := ExtractFileName(sFile);
  parts := ss.Split(['_']);
  name := parts[1];
  sname := parts[2];
  ti := 0;
  Setlength(types, 0);
  if name.endsWith('Refset') and (name <> 'Refset') then
  begin
    name := name.Substring(0, name.Length-6);
    Setlength(types, name.Length);
    for i := 0 to name.Length - 1 do
    begin
      if not CharInSet(name[i+1], ['c', 'i', 's']) then
        raise ETerminologySetup.create('Unknown refset type '+name[i+1]);
      types[i] := ord(name[i+1]);
    end;
    if FRefSetTypes.ContainsKey(name) then
      ti := FRefSetTypes[name]
    else
    begin
      ti := FRefs.AddReferences(types);
      FRefSetTypes.Add(name, ti);
    end;
  end;

  line := 0;
  s := LoadFile(sFile);
  iCursor := 0;
  // figure out what kind of reference set this is
  iCursor := Next(13)+1;
  sActive := memU8toString(s, 0, iCursor);
  if sActive.contains('map') then
    exit;
  SetLength(offsets, length(types));
  SetLength(values, length(types)*2);
  SetLength(sVals, length(types));
  fieldnames := TStringList.Create;
  try
    for name in sActive.Split([#9]) do
      fieldnames.add(name.trim);
    While iCursor < Length(s) Do
    Begin
      inc(line);
      if (line mod 50 = 0) then
        Progress(STEP_IMPORT_REFSET, 0, 'Import Reference Set '+ExtractFileName(sFile)+' line '+inttostr(line));

      iId := iCursor;
      iDate := Next(9);
      iTime := Next(9);
      iActive := Next(9);
      iModule := Next(9);
      iRefSetId := Next(9);
      iRefComp := Next(9);
      if length(types) > 0 then
      begin
        for i := 0 to length(types) - 2 do
          offsets[i] := Next(9);
        offsets[length(types) - 1] := Next(13);
        iCursor := offsets[length(types) - 1];
      end
      else
      begin
        if (iCursor < length(s)) and (s[iCursor] <> 13) then
          iCursor := Next(13)
        else
          iCursor := iRefComp;
      end;

      sId := memU8toString(s, iId+1, iDate - (iId + 1));
      if (sId = '') then
        break;

      sModule := memU8toString(s, iActive+1, iModule - (iActive + 1));
      sDate:= memU8toString(s, iDate+1, iTime - (iDate + 1));
      sActive := memU8toString(s, iTime+1, iActive - (iTime + 1));
      sRefSetId := memU8toString(s, iModule+1, iRefSetId - (iModule + 1));
      sRefComp := memU8toString(s, iRefSetId+1, iRefComp - (iRefSetId + 1));


      if not FConcept.FindConcept(StrToUInt64(sModule), iMod) then
        raise ETerminologySetup.create('Module '+sModule+' not found');
      for I := 0 to length(types) - 1 do
      begin
        if (i = 0) then
          sVals[i] := memU8toString(s, iRefComp+1, integer(offsets[i]) - (iRefComp + 1))
        else
          sVals[i] := memU8toString(s, offsets[i-1]+1, offsets[i] - (offsets[i-1] + 1));
        case types[i] of
          99 {c} :
            begin
            if (sVals[i] = '') then
               iRef := 0
            else
              iRef := StrToUInt64(sVals[i]);
            if FConcept.FindConcept(iRef, iTermRef) then
              iType := 1
            else if FDescRef.FindDescription(iRef, iTermRef) then
              iType := 2
            Else if FRels.TryGetValue(iRef, iTermRef) then
              iType := 3
            else
              raise ETerminologySetup.create('Unable to find concept '+sVals[i]);
            iVal := iTermRef;
            end;
          105 {i} :
            begin
            iVal := StrToInt(sVals[i]);
            iType := 4;
            end;
          115 {s} :
            begin
            iVal := FStrings.AddString(sVals[i]);
            iType := 5;
            end;
        else
          raise ETerminologySetup.create('Internal error');
        end;
        values[i*2] := iVal;
        values[i*2+1] := iType;
      end;
      iRef := StrToUInt64(sRefComp);

      if sActive = '1' Then
      Begin
        refSet := Frefsets.GetRefset(sRefSetId);
        if (refset.fieldTypes <> 0) and (refset.fieldTypes <> ti) then
          raise ETerminologySetup.create('field types mismatch - '+inttostr(refset.fieldTypes)+' / '+inttostr(ti)+' in refset '+sRefSetId+' processing '+sFile);
        refset.title := sname;
        refset.isLangRefset := isLangRefset;
        refset.filename := FStrings.AddString(sFile.Substring(pfxLen));
        refset.fieldTypes := ti;
        SetLength(fnames, length(types));
        for i := 0 to length(types) -1 do
          fnames[i] := FStrings.AddString(fieldnames[6+i]);
        refset.fieldNames := FRefs.AddReferences(fnames);
        if refset.index = MAGIC_NO_CHILDREN then
          if not FConcept.FindConcept(StrToUInt64(sRefSetId), iRefsetRef) then
             raise ETerminologySetup.create('unable to find term '+sRefSetId+' for reference set '+sFile)
          else
            refset.index := iRefsetRef;

        ok := true;
        if FConcept.FindConcept(iRef, iTermRef) then
          bDesc := 0
        else if FDescRef.FindDescription(iRef, iTermRef) then
        begin
          bDesc := 1;
          refset.noStoreIds := true;

        end
        Else if FRels.TryGetValue(iRef, iTermRef) then
          bDesc := 2
        else
        begin
          Raise Exception('Unknown component '+sRefComp+' in '+sFile);
          bDesc := 3;
        end;

        if ok then
        begin
          if (refset.iMemberLength = length(refset.aMembers)) Then
            SetLength(refset.aMembers, length(refset.aMembers)+10000);
          refset.aMembers[refset.iMemberLength].id := StringToGuid('{'+sId+'}');
          refset.aMembers[refset.iMemberLength].kind := bDesc;
          refset.aMembers[refset.iMemberLength].Ref := iTermRef;
          refset.aMembers[refset.iMemberLength].module := iMod;
          refset.aMembers[refset.iMemberLength].date := readDate(sDate);
          if (ti <> 0) then
            refset.aMembers[refset.iMemberLength].Values := FRefs.AddReferences(values);
          inc(refset.iMemberLength);
        end;
      End;

      inc(iCursor, 1);
    End;
  finally
    fieldnames.Free;
  end;
end;

Procedure TSnomedImporter.CloseReferenceSets;
var
  i : integer;
  RefSet : TRefSet;
begin
  for i := 0 to Frefsets.Count - 1  do
  begin
    refset := Frefsets[i] as TRefSet;
    Progress(STEP_IMPORT_REFSET, 0, 'Closing Reference Set '+refset.Name+' ('+pct(i, FRefSets.Count)+')');
    SetLength(refset.aMembers, refset.iMemberLength);
    QuickSortPairsByName(refset.aMembers);
    refset.membersByName := FRefsetMembers.AddMembers(false, refset.aMembers);
    QuickSortPairs(refset.aMembers);
    refset.membersByRef := FRefsetMembers.AddMembers(not refset.noStoreIds, refset.aMembers);
    if refset.isLangRefset then // it's a description refset...
      Fsvc.DefaultLanguageRefSet := refset.membersByRef;
  end;
end;


procedure TSnomedImporter.Progress(step : integer; pct : real; Msg : String);
begin
  if (assigned(callback)) then
  begin
    if msg = '' then
      msg := lastmessage;
    pct := ((step / STEP_TOTAL) * 100) + (pct * (100 / STEP_TOTAL));
    callback(trunc(pct), Msg);
    lastmessage := msg;
  end
  else if (msg <> '') then
  begin
    Writeln('           '+DescribePeriod(now - FStart));
    write('#'+inttostr(step)+' '+msg)
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

function TRefSet.contains(term: cardinal; var values : cardinal): boolean;
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
        values := aMembers[i].values;
        L := I;
      end;
    end;
  end;
end;

function needsBaseForImport(moduleId : String): boolean;
begin
  result := moduleId = '11000172109';
end;

End.


