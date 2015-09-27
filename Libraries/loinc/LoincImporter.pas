Unit LOINCImporter;

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
  SysUtils, Contnrs, Classes,
  StringSupport, DateSupport, AdvGenerics,
  AdvObjects, AdvObjectLists, AdvStringLists, AdvIntegerLists, AdvNames, AdvCSVExtractors, AdvFiles, AdvExceptions,
  YuStemmer, LoincServices;

Const
  FLAG_LONG_COMMON = 1;
  FLAG_LONG_RELATED = 2;
  STEP_COUNT = 1000;
  

Type
  TLOINCImporter = class;

  THeirarchyEntry = class;
  THeirarchyEntryList = class;
  TCodeList = class;

  TConcept = class (TAdvName)
  private
    Index : Cardinal;
    Codes : TObjectList;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  End;

  TConceptManager = class (TAdvNameList)
    Function See(sName : String; oCode : TObject) : TConcept;
    Function Store(sName : String; oImp : TLOINCImporter) : Word;
  End;

  TDescribed = class  (TAdvObject)
  private
    index : Cardinal;
    Stems : TAdvIntegerList;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  end;
  
  THeirarchyEntry = class (TDescribed)
  private
    FCode : String;
    FText : String;
    FParent : THeirarchyEntry;
    FChildren : THeirarchyEntryList;
    FDescendents : THeirarchyEntryList;
    FConcepts : TCodeList;
    FDescendentConcepts : TCodeList;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  End;

  THeirarchyEntryList = class (TAdvObjectList)
  private
    function GetEntry(iIndex: Integer): THeirarchyEntry;
  protected
    Function ItemClass : TAdvObjectClass; Override;
    Function CompareByCode(pA, pB: Pointer): Integer; Virtual;
    Function CompareByText(pA, pB: Pointer): Integer; Virtual;
    Function FindByCode(entry : THeirarchyEntry; Out iIndex: Integer): Boolean; Overload;
    Function FindByText(entry : THeirarchyEntry; Out iIndex: Integer): Boolean; Overload;
  public
    Procedure SortedByCode;
    Procedure SortedByText;

    Function getByCode(code : String) : THeirarchyEntry;
    Function FindByCode(code: String; Out iIndex: Integer): Boolean; Overload;
    Function getByText(text : String) : THeirarchyEntry;
    Function FindByText(text: String; Out iIndex: Integer): Boolean; Overload;

    Property Entries[iIndex : Integer] : THeirarchyEntry Read GetEntry; Default;
  end;

  TCode = class (TDescribed)
  Private
    Code : String;
    Display : String;
    Names : Cardinal;
    Comps : TConcept;
    Props : TConcept;
    Time : TConcept;
    System : TConcept;
    Scale : TConcept;
    Method : TConcept;
    Class_ : TConcept;
    v2dt, v3dt : Word;
    Flags : byte;
    entry : THeirarchyEntry;
  public
    Function Compare(pA, pB : Pointer) : Integer;
  End;

  TCodeList = class (TAdvObjectList)
  private
    function GetEntry(iIndex: Integer): TCode;
  protected
    Function ItemClass : TAdvObjectClass; Override;
    Function CompareByCode(pA, pB: Pointer): Integer; Virtual;
    Function FindByCode(entry : TCode; Out iIndex: Integer): Boolean; Overload;
  public
    Procedure SortedByCode;

    Function getByCode(code : String) : TCode;
    Function FindByCode(code: String; Out iIndex: Integer): Boolean; Overload;

    Property Entries[iIndex : Integer] : TCode Read GetEntry; Default;
  end;

  TAnswerList = class;

  TAnswer = class (TAdvObject)
  private
    FCode: String;
    FDescription: String;
    FParents: TAdvList<TAnswerList>;
    FIndex : integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    Property Code : String read FCode write FCode;
    Property Description : String read FDescription write FDescription;
    Property Index : Integer read FIndex write FIndex;
    Property Parents : TAdvList<TAnswerList> read FParents;
  end;

  TAnswerList = class (TAdvObject)
  private
    FCode: String;
    FDescription: String;
    FAnswers : TAdvList<TAnswer>;
    FIndex : integer;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Property Code : String read FCode write FCode;
    Property Description : String read FDescription write FDescription;
    Property Answers : TAdvList<TAnswer> read FAnswers;
  end;

  TLoincImporter = class (TAdvObject)
  private
    FStart : TDateTime;
    FFolder : String;
    TotalConcepts : Integer;

    FDesc : TLoincStrings;
    FCode : TLOINCCodeList;
    FRefs : TLOINCReferences;
    FConcepts : TLOINCConcepts;
    FWords : TLoincWords;
    FStems : TLoincStems;
    FEntries : TLOINCHeirarchyEntryList;
    FAnswerLists : TAdvMap<TAnswerList>;
    FAnswerMap : TAdvMap<TAnswer>;
    FAnswers : TLOINCAnswersList;

    FStrings : TStringList;
    FUnits : TStringList;
    FVersion: String;
    FWordList : TStringList;
    FStemList : TStringList;
    FStemmer : TYuStemmer_8;
    FOutputFile: String;

    procedure SeeDesc(sDesc: String; oObj : TDescribed; iFlags: Byte);
    procedure SeeWord(sDesc: String; oObj : TDescribed; iFlags: Byte);

    Function AddDescription(Const s : String):Cardinal;
    Function SeeUnits(Const s : String):Word;
    function LoadLOINCFiles(folder : String; out props : TLoincPropertyIds; out roots : TCardinalArray; out subsets : TLoincSubsets) : Word;
    function ReadLOINCDatabase(out props : TLoincPropertyIds; out roots : TCardinalArray; out subsets : TLoincSubsets) : Word;
    procedure Progress(msg : String);
    procedure ProcessMultiAxialEntry(oHeirarchy : THeirarchyEntryList; oCodes : TCodeList; ln : string);
    procedure ProcessAnswerLine(oHeirarchy : THeirarchyEntryList; oCodes : TCodeList; ln : string);
    procedure StoreHeirarchyEntry(entry : THeirarchyEntry);
  public
    procedure ImportLOINC;

    Property Folder : String read FFolder write FFolder;
    Property OutputFile : String read FOutputFile write FOutputFile;
    Property Version : String read FVersion write FVersion;
  end;

function importLoinc(folder, version, dest : String) : String;

Implementation

Function DetermineVersion(filename : String) : String;
var
  txtFile : String;
  f : TFileStream;
  txt : AnsiString;
  s, t : String;
begin
  txtFile := ChangeFileExt(filename, '.txt');
  if not FileExists(txtFile) then
    raise Exception.Create('Unable to find the file '+txtFile+' so the version can be determined');
  f := TFileStream.Create(txtFile, fmOpenRead + fmShareDenyWrite);
  try
    setLength(txt, f.Size);
    f.Read(txt[1], f.Size);
  finally
    f.free;
  end;
  t := String(txt);
  StringSplit(t, [#13], s, t);
  result := copy(s, length(s)-3, 4);
  if not (StringIsInteger16(copy(result, 1, 1)) and (result[2] = '.') and StringIsInteger16(copy(result, 3, 2))) then
    raise Exception.Create('Unable to read the version from '+txtFile);
end;

function importLoinc(folder, version, dest : String) : String;
var
  imp : TLoincImporter;
begin
  Writeln('Import LOINC from '+folder);
  imp := TLoincImporter.Create;
  try
    imp.Folder := folder;
    imp.Version := version;
    Writeln('Version: '+imp.Version);
    result := IncludeTrailingPathDelimiter(dest)+'loinc.cache';
    imp.outputFile := result;
    imp.ImportLOINC;
    Writeln('Done '+inttostr(imp.TotalConcepts)+' Concepts');
  finally
    imp.Free;
  end;
end;



Function TCode.Compare(pA, pB : Pointer) : Integer;
begin
  result := CompareStr(TCode(pA).Code, TCode(pB).Code);
End;

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

procedure TLoincImporter.SeeDesc(sDesc: String; oObj : TDescribed; iFlags: Byte);
var
  s : String;
begin
  while (sDesc <> '') Do
  Begin
    StringSplit(sdesc, [#13, #10, ',', ' ', ':', '.', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '{', '}', '[', ']', '|', '\', ';', '"', '<', '>', '?', '/', '~', '`', '-', '_', '+', '='],
      s, sdesc);
    if (s <> '') {And not StringContainsAny(s, ['0'..'9']) And (length(s) > 3)} Then
      SeeWord(s, oObj, iFlags);
  End;
end;

procedure TLoincImporter.SeeWord(sDesc: String; oObj : TDescribed; iFlags: Byte);
var
  i, m : integer;
  sStem : String;
  oList : TAdvObjectList;
begin
  sDesc := lowercase(sdesc);
  if not FWordList.Find(sDesc, i) Then
    i := FWordList.Add(sdesc);
  m := integer(FWordList.Objects[i]);
  case iFlags of
    FLAG_LONG_COMMON : m := m or $1;
    FLAG_LONG_RELATED : m := m or $2;
  End;

  FWordList.Objects[i] := TObject(m);

  sStem := FStemmer.calc(sDesc);
  if not FStemList.Find(sStem, i) Then
  Begin
    oList := TAdvObjectList.Create;
    oList.SortedByReference;
    FStemList.AddObject(sStem, oList);
  End
  Else
    oList := TAdvObjectList(FStemList.Objects[i]);
  if not oList.ExistsByReference(oObj) Then
    oList.Add(oObj.Link);
End;


procedure TLoincImporter.StoreHeirarchyEntry(entry: THeirarchyEntry);
var
  parent, children, descendents, concepts, descendentConcepts: Cardinal;
  refs : TCardinalArray;
  i : integer;
begin
  setLength(refs, entry.FChildren.Count);
  for i := 0 to entry.FChildren.Count - 1 do
    refs[i] := entry.FChildren[i].index;
  children := FRefs.AddCardinals(refs);

  setLength(refs, entry.FDescendents.Count);
  for i := 0 to entry.FDescendents.Count - 1 do
    refs[i] := entry.FDescendents[i].index;
  descendents := FRefs.AddCardinals(refs);

  setLength(refs, entry.FConcepts.Count);
  entry.FConcepts.SortedByCode;
  for i := 0 to entry.FConcepts.Count - 1 do
    refs[i] := TCode(entry.FConcepts[i]).index;
  concepts := FRefs.AddCardinals(refs);

  setLength(refs, entry.FDescendentConcepts.Count);
  entry.FDescendentConcepts.SortedByCode;
  for i := 0 to entry.FDescendentConcepts.Count - 1 do
    refs[i] := TCode(entry.FDescendentConcepts[i]).index;
  descendentConcepts := FRefs.AddCardinals(refs);

  if entry.FParent <> nil then
    parent := entry.FParent.index
  else
    parent := NO_PARENT;

  if FEntries.AddEntry(AddDescription(entry.FCode), AddDescription(entry.FText), parent, children, descendents, concepts, descendentConcepts) <> entry.index then
    raise Exception.Create('Out of order');
end;

Const
  FLD_LOINC_NUM =                            0;
  FLD_COMPONENT =                            1;
  FLD_PROPERTY =                             2;
  FLD_TIME_ASPCT =                           3;
  FLD_SYSTEM =                               4;
  FLD_SCALE_TYP =                            5;
  FLD_METHOD_TYP =                           6;
  FLD_CLASS =                                7;
  FLD_SOURCE =                               8;
  FLD_DATE_LAST_CHANGED =                    9;
  FLD_CHNG_TYPE =                           10;
  FLD_COMMENTS =                            11;
  FLD_STATUS =                              12;
  FLD_CONSUMER_NAME =                       13;
  FLD_MOLAR_MASS =                          14;
  FLD_CLASSTYPE =                           15;
  FLD_FORMULA =                             16;
  FLD_SPECIES =                             17;
  FLD_EXMPL_ANSWERS =                       18;
  FLD_ACSSYM =                              19;
  FLD_BASE_NAME =                           20;
  FLD_NAACCR_ID =                           21;
  FLD_CODE_TABLE =                          22;
  FLD_SURVEY_QUEST_TEXT =                   23;
  FLD_SURVEY_QUEST_SRC =                    24;
  FLD_UNITSREQUIRED =                       25;
  FLD_SUBMITTED_UNITS =                     26;
  FLD_RELATEDNAMES2 =                       27;
  FLD_SHORTNAME =                           28;
  FLD_ORDER_OBS =                           29;
  FLD_CDISC_COMMON_TESTS =                  30;
  FLD_HL7_FIELD_SUBFIELD_ID =               31;
  FLD_EXTERNAL_COPYRIGHT_NOTICE =           32;
  FLD_EXAMPLE_UNITS =                       33;
  FLD_LONG_COMMON_NAME =                    34;
  FLD_HL7_V2_DATATYPE =                     35;
  FLD_HL7_V3_DATATYPE =                     36;
  FLD_CURATED_RANGE_AND_UNITS =             37;
  FLD_DOCUMENT_SECTION =                    38;
  FLD_EXAMPLE_UCUM_UNITS =                  39;
  FLD_EXAMPLE_SI_UCUM_UNITS =               40;
  FLD_STATUS_REASON =                       41;
  FLD_STATUS_TEXT =                         42;
  FLD_CHANGE_REASON_PUBLIC =                43;
  FLD_COMMON_TEST_RANK =                    44;
  FLD_COMMON_ORDER_RANK =                   45;
  FLD_COMMON_SI_TEST_RANK =                 46;
  FLD_HL7_ATTACHMENT_STRUCTURE =            47;


Function TLoincImporter.LoadLOINCFiles(folder : String; out props : TLoincPropertyIds; out roots : TCardinalArray; out subsets : TLoincSubsets) : Word;
var
  iLength : Integer;
  iCount : Integer;
  oCodes : TCodeList;
  oTemp : TAdvObjectList;
  oCode : TCode;
  iLoop : Integer;
  oNames : TAdvStringList;
  aNames : TCardinalArray;
  oComps : TConceptManager;
  oProps : TConceptManager;
  oTime : TConceptManager;
  oSystem : TConceptManager;
  oScale : TConceptManager;
  oMethod : TConceptManager;
  oClass : TConceptManager;
  oHeirarchy : THeirarchyEntryList;
  oEntry : THeirarchyEntry;
  aConcepts : TWordArray;
  oSubsets : Array[TLoincSubsetId] of TCodeList;
  i, j : integer;
  iFlag : Byte;
  aCardinals : TCardinalArray;
  iStem, e : Cardinal;
  ma : Text;
  ln : String;
  a : TLoincSubsetId;
  csv : TAdvCSVExtractor;
  items : TAdvStringList;
  f : TAdvFile;
  st, st2 : TStringList;
  answerlist : TAnswerList;
  answer : TAnswer;
begin
  result := 0;
  iLength := 0;

  oCodes := TCodeList.Create;
  oComps := TConceptManager.Create;
  oProps := TConceptManager.Create;
  oTime := TConceptManager.Create;
  oSystem := TConceptManager.Create;
  oScale := TConceptManager.Create;
  oMethod := TConceptManager.Create;
  oClass := TConceptManager.Create;
  FAnswerLists := TAdvMap<TAnswerList>.Create;
  FAnswerMap := TAdvMap<TAnswer>.create;
  oHeirarchy := THeirarchyEntryList.Create;
  Try
    oComps.SortedByName;
    oProps.sortedByName;
    oTime.sortedByName;
    oSystem.sortedByName;
    oScale.sortedByName;
    oMethod.sortedByName;
    oClass.sortedByName;
    for a := Low(TLoincSubsetId) to high(TLoincSubsetId) do
    begin
      oSubsets[a] := TCodeList.create;
      oSubsets[a].SortedByCode;
    end;

    iCount := 0;
    Progress('Loading Concepts');
    items := TAdvStringList.create;
    f := TAdvFile.Create;
    try
      f.Name := IncludeTrailingPathDelimiter(folder)+ 'loinc.csv';
      f.OpenRead;
      csv := TAdvCSVExtractor.Create(f.Link, TEncoding.ASCII);
      Try
        // headers
        csv.ConsumeEntries(items);

        while csv.More do
        begin
          items.Clear;
          csv.ConsumeEntries(items);
          if items.count > 0 then
          begin
            oCode := TCode.Create;
            oCodes.Add(oCode);

            oCode.Names := 0;
            oCode.Code := Trim(items[FLD_LOINC_NUM]);
            oCode.Display := Trim(items[FLD_LONG_COMMON_NAME]);
            if Length(oCode.Code) > iLength Then
              iLength := Length(oCode.Code);

            oCode.Comps := oComps.See(items[FLD_COMPONENT], oCode);
            oCode.Props := oProps.See(items[FLD_PROPERTY], oCode);
            oCode.Time := oTime.See(items[FLD_TIME_ASPCT], oCode);
            oCode.System := oSystem.See(items[FLD_SYSTEM], oCode);
            oCode.Scale := oScale.See(items[FLD_SCALE_TYP], oCode);
            oCode.Method := oMethod.See(items[FLD_METHOD_TYP], oCode);
            oCode.Class_ := oClass.See(items[FLD_CLASS], oCode);

            oSubsets[lsiAll].add(oCode.Link);
            oCode.Flags := 0;
    //        if o.ColIntegerByName['SETROOT'] = 1 Then
    //          oCode.Flags := oCode.Flags + FLAGS_ROOT;
            if sameText(items[FLD_ORDER_OBS], 'Both') Then
            begin
              oCode.Flags := oCode.Flags + FLAGS_ORDER + FLAGS_OBS;
              oSubsets[lsiOrderObs].add(oCode.Link);
            end
            else if sameText(items[FLD_ORDER_OBS], 'Observation') Then
            begin
              oCode.Flags := oCode.Flags + FLAGS_OBS;
              oSubsets[lsiObs].add(oCode.Link);
            end
            else if sameText(items[FLD_ORDER_OBS], 'Order') Then
            begin
              oCode.Flags := oCode.Flags + FLAGS_ORDER;
              oSubsets[lsiOrder].add(oCode.Link);
            end
            else if (items[FLD_ORDER_OBS] <> '') And (items[FLD_ORDER_OBS] <> 'Subset') Then
              Raise exception.create('unknown order/obs '+items[FLD_ORDER_OBS])
            else
              oSubsets[lsiOrderSubset].add(oCode.Link);

            if items[FLD_UNITSREQUIRED] = 'Y' Then
              oCode.Flags := oCode.Flags + FLAGS_UNITS;
            if items[FLD_EXTERNAL_COPYRIGHT_NOTICE] = '' then
              oSubsets[lsiInternal].add(oCode.Link)
            else
              oSubsets[lsi3rdParty].add(oCode.Link);

            case StrToInt(items[FLD_CLASSTYPE]) of
             2: begin
                oCode.Flags := oCode.Flags + FLAGS_CLIN;
                oSubsets[lsiTypeClinical].add(oCode.Link);
                end;
             3: begin
                oCode.Flags := oCode.Flags + FLAGS_ATT;
                oSubsets[lsiTypeAttachment].add(oCode.Link);
                end;
             4: begin
                oCode.Flags := oCode.Flags + FLAGS_SURV;
                oSubsets[lsiTypeSurvey].add(oCode.Link);
                end;
             1: begin
                oSubsets[lsiTypeObservation].add(oCode.Link);
                end;
            else
              Raise exception.create('unexpected class type '+items[FLD_CLASSTYPE]);
            End;
            if SameText(items[FLD_STATUS], 'Active') then
              oSubsets[lsiActive].add(oCode.Link)
            else if SameText(items[FLD_STATUS], 'Deprecated') then
              oSubsets[lsiDeprecated].add(oCode.Link)
            else if SameText(items[FLD_STATUS], 'Discouraged') then
              oSubsets[lsiDiscouraged].add(oCode.Link)
            else if SameText(items[FLD_STATUS], 'Trial') then
              oSubsets[lsiTrial].add(oCode.Link)
            else
              raise Exception.Create('Unknown LOINC Code status '+items[FLD_STATUS]);

            SeeDesc(oCode.Display, oCode, FLAG_LONG_COMMON);

            oCode.v2dt := SeeUnits(items[FLD_HL7_V2_DATATYPE]);
            oCode.v3dt := SeeUnits(items[FLD_HL7_V3_DATATYPE]);

            oNames := TAdvStringList.Create;
            Try
              oNames.Symbol := ';';
              oNames.AsText := items[FLD_RELATEDNAMES2];
              if items[FLD_SHORTNAME] <> '' Then
                oNames.Insert(0, items[FLD_SHORTNAME]);

              if oNames.Count > 0 Then
              Begin
                SetLength(aNames, oNames.Count);
                For iLoop := 0 to oNames.Count - 1 Do
                  If Trim(oNames[iLoop]) <> '' Then
                  Begin
                    aNames[iLoop] := addDescription(Trim(oNames[iLoop]));
                    SeeDesc(oNames[iLoop], oCode, FLAG_LONG_RELATED);
                  End
                  Else
                    aNames[iLoop] := 0;

                oCode.Names := FRefs.AddCardinals(aNames);
              End;
            Finally
              oNames.Free;
            End;

            // properties

            if iCount mod STEP_COUNT = 0 then
              Progress('');
            inc(iCount);
          end;
        End;
      Finally
        csv.free;
        items.Free;
      End;
    finally
      f.free;
    end;
    Progress('Sort Codes');
    oCodes.SortedByCode;

    // now, process the multi-axial file
    if FileExists(IncludeTrailingPathDelimiter(folder) + 'mafile.csv') then
    begin
      Progress('Loading Multi-Axial Source');
      oHeirarchy.SortedByCode;
      AssignFile(ma, IncludeTrailingPathDelimiter(folder) + 'mafile.csv');
      Reset(ma);
      Readln(ma, ln); // skip header

      iCount := 0;
      while not eof(ma) do
      begin
        Readln(ma, ln);
        ProcessMultiAxialEntry(oHeirarchy, oCodes, ln);
        if iCount mod STEP_COUNT = 0 then
          Progress('');
        inc(iCount);
      end;
      CloseFile(ma);
      for i := 0 to oHeirarchy.Count - 1 do
        // first, we simply assign them all an index. Then we'll create everything else, and then go and actually store them
        oHeirarchy[i].index := i;
    end;

    if FileExists(IncludeTrailingPathDelimiter(folder) + 'answers.csv') then
    begin
      Progress('Loading Answer Lists');
      AssignFile(ma, IncludeTrailingPathDelimiter(folder) + 'answers.csv');
      Reset(ma);
      Readln(ma, ln); // skip header
      iCount := 0;
      while not eof(ma) do
      begin
        Readln(ma, ln);
        ProcessAnswerLine(oHeirarchy, oCodes, ln);
        if iCount mod STEP_COUNT = 0 then
          Progress('');
        inc(iCount);
      end;
      CloseFile(ma);
    end;

    Progress('Build Cache');
    For iLoop := 0 to oCodes.Count - 1 Do
    Begin
      oCode := TCode(oCodes[iLoop]);
      oCode.Code := StringPadRight(oCode.Code, ' ', iLength);
    End;
    FCode.CodeLength := iLength;

    For iLoop := 0 to oCodes.Count - 1 Do
    Begin
      if iCount mod STEP_COUNT = 0 then
        Progress('');
      inc(iCount);
      oCode := TCode(oCodes[iLoop]);
      Try
        if oCode.entry <> nil then
          e := oCode.entry.index
        else
          e := 0;
          
        oCode.Index := FCode.AddCode(oCode.Code, AddDescription(oCode.Display), oCode.Names, e, oCode.v2dt, oCode.v3dt, oCode.Flags);
      Except
        on E:Exception Do
        Begin
          e.Message := e.Message + ' (Code = '+oCode.Code+')';
          recordStack(e);
          raise;
        End;
      End;
    End;

    SetLength(aConcepts, 7);
    Props[lptComponents] := oComps.Store('Components', self);
    aConcepts[0] := Props[lptComponents];
    Props[lptProperties] := oProps.Store('Properties', self);
    aConcepts[1] := Props[lptProperties];
    Props[lptTimeAspects] := oTime.Store('Time Aspects', self);
    aConcepts[2] := Props[lptTimeAspects];
    Props[lptSystems] := oSystem.Store('Systems', self);
    aConcepts[3] := Props[lptSystems];
    Props[lptScales] := oScale.Store('Scales', self);
    aConcepts[4] := Props[lptScales];
    Props[lptMethods] := oMethod.Store('Methods', self);
    aConcepts[5] := Props[lptMethods];
    Props[lptClasses] := oClass.Store('Classes', self);
    aConcepts[6] := Props[lptClasses];
    result := FConcepts.AddConcept(AddDescription('LOINC Definitions'), FRefs.AddWords(aConcepts), 0);
    FCode.donebuild;

    Progress('Storing Heirachy');
    FEntries.StartBuild;
    iCount := 0;
    SetLength(roots, 0);
    for i := 0 to oHeirarchy.Count - 1 do
    begin
      if iCount mod STEP_COUNT = 0 then
        Progress('');
      inc(iCount);
      if oHeirarchy[i].FParent = nil then
      begin
        SetLength(roots, Length(roots)+1);
        roots[Length(roots)-1] := oHeirarchy[i].index;
      end;
      StoreHeirarchyEntry(oHeirarchy[i]);
    end;
    FEntries.DoneBuild;

    for a := Low(TLoincSubsetId) to high(TLoincSubsetId) do
    begin
      setLength(aCardinals, oSubsets[a].Count);
      for i := 0 to oSubsets[a].Count - 1 do
        aCardinals[i] := oSubsets[a][i].Index;
      subsets[a] := FRefs.AddCardinals(aCardinals);
    end;

    Progress('Processing Answer Lists');
    FAnswers.StartBuild;
    st := TStringList.Create;
    st2 := TStringList.Create;
    try
      for answerlist in FAnswerLists.Values do
        st.AddObject(answerlist.FCode, answerlist);
      st.Sort;
      for answer in FAnswerMap.Values do
        st2.AddObject(answer.FCode, answer);
      st2.Sort;

      for i := 0 to st.Count - 1 do
      begin
        answerlist := TAnswerList(st.Objects[i]);
        answerlist.FIndex := i + st2.Count;
      end;

      for i := 0 to st2.Count - 1 do
      begin
        answer := TAnswer(st2.Objects[i]);
        if answer.FCode = 'LA3-6' then
          writeln('test');
        SetLength(aCardinals, answer.FParents.Count);
        for j := 0 to answer.FParents.Count - 1 do
          aCardinals[j] := answer.FParents[j].FIndex;
        answer.Index := FAnswers.AddEntry(AddDescription(answer.Code), AddDescription(answer.FDescription), FRefs.AddCardinals(aCardinals));
      end;
      for i := 0 to st.Count - 1 do
      begin
        answerlist := TAnswerList(st.Objects[i]);
        if answerList.FCode = 'LL183-5' then
          writeln('test');
        SetLength(aCardinals, answerlist.FAnswers.Count);
        for j := 0 to answerlist.FAnswers.Count - 1 do
          aCardinals[j] := answerlist.FAnswers[j].FIndex;
        j := FAnswers.AddEntry(AddDescription(answerlist.Code), AddDescription(answerlist.FDescription), FRefs.AddCardinals(aCardinals));
        if (j <> answerlist.FIndex) then
          raise Exception.Create('Error Message');
      end;
    finally
      st.Free;
      st2.Free;
    end;
    FAnswers.DoneBuild;



    Progress('Processing Words');
    FWords.StartBuild;
    For i := 0 to FWordList.Count - 1 Do
    Begin
      if i mod STEP_COUNT = 0 then
        Progress('');
      iFlag := Integer(FWordList.Objects[i]);
      FWords.AddWord(FDesc.AddEntry(FWordList[i]), iFlag);
    End;
    FWords.DoneBuild;

    Progress('Processing Stems');
    FStems.StartBuild;
    For i := 0 to FStemList.Count - 1 Do
    Begin
      if i mod STEP_COUNT = 0 then
        Progress('');
      oTemp := TAdvObjectList(FStemList.Objects[i]);
      iStem := FDesc.AddEntry(FStemList[i]);
      FStems.AddStem(iStem);
      for j := 0 to oTemp.Count - 1 Do
        TDescribed(oTemp[j]).Stems.Add(iStem);
      oTemp.Free;
      FStemList.Objects[i] := nil;
    End;
    FStems.DoneBuild;
    For i := 0 to oCodes.Count - 1 Do
    Begin
      oCode := TCode(oCodes[i]);
      SetLength(aCardinals, oCode.Stems.Count);
      for j := 0 to oCode.Stems.Count - 1 do
        aCardinals[j] := oCode.Stems[j];
      FCode.SetStems(oCode.Index, FRefs.AddCardinals(aCardinals));
    End;
    For i := 0 to oHeirarchy.Count - 1 Do
    Begin
      oEntry := oHeirarchy[i];
      SetLength(aCardinals, oEntry.Stems.Count);
      for j := 0 to oEntry.Stems.Count - 1 do
        aCardinals[j] := oEntry.Stems[j];
      FEntries.SetStems(oEntry.Index, FRefs.AddCardinals(aCardinals));
    End;

    Progress('Cross-Linking');
    For iLoop := 0 to oCodes.Count - 1 Do
    Begin
      if iLoop mod STEP_COUNT = 0 then
        Progress('');
      inc(iCount);
      oCode := TCode(oCodes[iLoop]);
      if oCode.Comps <> nil Then
        FCode.SetComponent(oCode.Index, oCode.Comps.Index);
      if oCode.Props <> nil Then
        FCode.SetProperty(oCode.Index, oCode.Props.Index);
      if oCode.Time <> nil Then
        FCode.SetTimeAspect(oCode.Index, oCode.Time.Index);
      if oCode.System <> nil Then
        FCode.SetSystem(oCode.Index, oCode.System.Index);
      if oCode.Scale <> nil Then
        FCode.SetScale(oCode.Index, oCode.Scale.Index);
      if oCode.Method <> nil Then
        FCode.SetMethod(oCode.Index, oCode.Method.Index);
      if oCode.Class_ <> nil Then
        FCode.SetClass(oCode.Index, oCode.Class_.Index);
    End;

    TotalConcepts := oCodes.Count;
  Finally
    for a := Low(TLoincSubsetId) to high(TLoincSubsetId) do
      osubsets[a].free;
    oHeirarchy.Free;
    oCodes.Free;
    oComps.Free;
    oProps.Free;
    oTime.Free;
    oSystem.Free;
    oScale.Free;
    oMethod.Free;
    oClass.Free;
    FAnswerLists.Free;
    FAnswerMap.Free;

  End;
End;

Function TLoincImporter.ReadLOINCDatabase(out props : TLoincPropertyIds; out roots : TCardinalArray; out subsets : TLoincSubsets) : Word;
Begin
  FStrings := TStringList.Create;
  FStrings.Sorted := True;
  FUnits := TStringList.Create;
  FUnits.Sorted := true;
  try
    result := LoadLOINCFiles(folder, props, roots, subsets);
  Finally
    FUnits.Free;
    FStrings.Free;
  End;
End;

Procedure TLoincImporter.ImportLOINC;
var
  oSvc : TLOINCServices;
  props : TLoincPropertyIds;
  i : integer;
  roots : TCardinalArray;
  subsets : TLoincSubsets;
  ls, s : String;
  code, desc, refs : Cardinal;
begin
  FStart := now;
  FWordList := TStringList.Create;
  FStemList := TStringList.Create;
  FStemmer := GetStemmer_8('english');
  oSvc := TLOINCServices.Create;
  Try
    FWordList.Sorted := true;
    FStemList.Sorted := true;
    Fdesc := oSvc.Desc;
    Fdesc.StartBuild;
    FDesc.AddEntry('');
    FCode := oSvc.CodeList;
    FCode.StartBuild;
    FRefs := oSvc.Refs;
    FRefs.StartBuild;
    FConcepts := oSvc.Concepts;
    FConcepts.StartBuild;
    FWords := oSvc.Words;
    FStems := oSvc.Stems;
    FEntries := oSvc.Entries;
    FAnswers := oSvc.AnswerLists;

    Try
      oSvc.Root := ReadLOINCDatabase(props, roots, subsets);
      oSvc.LOINCVersion := Version;
      oSvc.Properties := props;
      oSvc.HeirarchyRoots := roots;
      oSvc.Subsets := subsets;
    Finally
      FConcepts.DoneBuild;
      FRefs.DoneBuild;
      Fdesc.doneBuild;
    End;

    ls := '';
    for I := 0 to FAnswers.Count - 1 do
    begin
      FAnswers.GetEntry(i, code, desc, refs);
      s := FDesc.GetEntry(code);
      if not((ls = '') or (AnsiCompareText(ls, s) < 0)) then
        raise Exception.Create('out of order');
      ls := s;
    end;

    Progress('Save');
    oSvc.Save(FOutputFile);

  Finally
    oSvc.Free;
    FWordList.Free;
    For i := 0 to FStemList.Count - 1 do
      FStemList.Objects[i].Free;
    FStemList.Free;
    FStemmer.Free;
  End;
End;

Function CSVStringSplit(Const sValue, sDelimiter : String; Var sLeft, sRight: String) : Boolean;
Var
  iIndex : Integer;
  sA, sB : String;
Begin
  if (sValue <> '') and (sValue[1] = '"') then
  begin
    iIndex := 1;
    repeat
      inc(iIndex);
      if (sValue[iIndex] = '"') then
        if (iIndex < sValue.Length) and (sValue[iIndex+1] = '"') then
          inc(iIndex)
        else
          break;
    until iIndex = sValue.Length;
    Result := iIndex < sValue.Length;
    if result then
      inc(iIndex);
  end
  else
  begin
    // Find the delimiter within the source string
    iIndex := Pos(sDelimiter, sValue);
    Result := iIndex <> 0;
  end;

  If Not Result Then
  Begin
    sA := sValue;
    sB := '';
  End
  Else
  Begin
    sA := Copy(sValue, 1, iIndex - 1);
    sB := Copy(sValue, iIndex + Length(sDelimiter), MaxInt);
  End;

  sLeft := sA;
  sRight := sB;
  if (sValue <> '') and (sValue[1] = '"') then
    sLeft := copy(sLeft, 2, length(sleft)-2).replace('""', '"');
End;

function isLoinc(s : String) : boolean;
begin
  result := (s.Length > 3) and (s[length(s)-1] = '-');
end;

var
  gc : integer = 0;

procedure TLoincImporter.ProcessAnswerLine(oHeirarchy : THeirarchyEntryList; oCodes : TCodeList; ln: string);
var
  s, s1, AnswerListId, AnswerListName, AnswerStringID, AnswerCode, DisplayText : String;
  list : TAnswerList;
  answer : TAnswer;
begin
  inc(gc);
  CSVStringSplit(ln, ',', s1, ln);
  CSVStringSplit(ln, ',', s, ln);
  CSVStringSplit(ln, ',', AnswerListId, ln);
  CSVStringSplit(ln, ',', AnswerListName, ln);
  CSVStringSplit(ln, ',', s, ln);
  CSVStringSplit(ln, ',', s, ln);
  CSVStringSplit(ln, ',', s, ln);
  CSVStringSplit(ln, ',', s, ln);
  CSVStringSplit(ln, ',', AnswerStringID, ln);
  CSVStringSplit(ln, ',', AnswerCode, ln);
  CSVStringSplit(ln, ',', s, ln);
  CSVStringSplit(ln, ',', DisplayText, ln);

  if isLoinc(s1) and isLoinc(AnswerListId) then
  begin
    if FAnswerLists.ContainsKey(AnswerListId) then
      list := FAnswerLists[AnswerListId]
    else
    begin
      list := TAnswerList.Create;
      list.FCode := AnswerListId;
      list.FDescription := AnswerListName;
      FAnswerLists.Add(list.Code, list);
    end;

    if (AnswerStringID = '') then
      writeln('?')
    else
    begin
      if FAnswerMap.ContainsKey(AnswerStringID) then
      begin
        answer := FAnswerMap[AnswerStringID];
        list.Answers.Add(answer.link as TAnswer);
        answer.Parents.Add(list.Link as TAnswerList);
        // cross-linking and linking means that they'll leak, but we don't care
      end
      else
      begin
        answer := TAnswer.Create;
        try
          answer.FCode := AnswerStringID;
          answer.Description := DisplayText;
          list.Answers.Add(answer.link as TAnswer);
          answer.Parents.Add(list.Link as TAnswerList);
          FAnswerMap.Add(AnswerStringID, answer.Link as TAnswer);
        finally
          answer.Free;
        end;
      end;
    end;
  end;
end;

procedure TLoincImporter.ProcessMultiAxialEntry(oHeirarchy : THeirarchyEntryList; oCodes : TCodeList; ln: string);
var
  PATH_TO_ROOT, SEQUENCE, IMMEDIATE_PARENT, CODE, CODE_TEXT : String;
  entry, parent : THeirarchyEntry;
  oCode : TCode;
begin
  StringSplit(ln, ',', PATH_TO_ROOT, ln);
  StringSplit(ln, ',', SEQUENCE, ln);
  StringSplit(ln, ',', IMMEDIATE_PARENT, ln);
  StringSplit(ln, ',', CODE, CODE_TEXT);

  if (CODE.StartsWith('LP')) then
  begin
    entry := THeirarchyEntry.Create;
    try
      entry.FCode := CODE;
      entry.FText := CODE_TEXT;
      if (IMMEDIATE_PARENT <> '') then
      begin
        entry.FParent := oHeirarchy.getByCode(IMMEDIATE_PARENT);
        entry.FParent.FChildren.Add(entry.Link);
        parent := entry.FParent;
        while (parent <> nil) do
        begin
          parent.FDescendents.Add(entry.Link);
          parent := parent.FParent;
        end;
      end;
      oHeirarchy.Add(entry.Link);
      SeeDesc(entry.FText, entry, FLAG_LONG_COMMON);
    finally
      entry.Free;
    end;
  end
  else
  begin
    oCode := oCodes.getByCode(CODE);
    if (oCode = nil) then
      raise Exception.Create('Unable to find code '+CODE);
    oCode.entry := oHeirarchy.getByCode(IMMEDIATE_PARENT);
    if (oCode.entry = nil) then
      raise Exception.Create('Unable to find ma code '+IMMEDIATE_PARENT);
    oCode.entry.FConcepts.Add(oCode.Link);
    parent := oCode.entry;
    while (parent <> nil) do
    begin
      if not parent.FDescendentConcepts.ExistsByReference(oCode) then
        parent.FDescendentConcepts.Add(oCode.Link);
          parent := parent.FParent;
    end;
  end;
end;

procedure TLoincImporter.Progress(msg: String);
begin
  if (msg <> '') then
  begin
    Writeln('           '+DescribePeriod(now - FStart));
    write(msg)
  end
  else
    write('.');
end;

{ TConcept }

constructor TConcept.Create;
begin
  inherited;
  Codes := TObjectList.Create;
  Codes.OwnsObjects := False;
end;

destructor TConcept.Destroy;
begin
  Codes.Free;
  inherited;
end;

{ TConceptManager }

function TConceptManager.See(sName: String; oCode: TObject): TConcept;
var
  i : Integer;
begin
  if sname = '' Then
    result := nil
  else
  begin
    i := IndexByName(sName);
    if existsByIndex(i) Then
      result := TConcept(ObjectByIndex[i])
    Else
    Begin
      result := TConcept.Create;
      result.Name := sName;
      Add(result);
    End;
    result.Codes.Add(oCode);
  End;
end;

function TConceptManager.Store(sName : String; oImp : TLoincImporter): Word;
var
  i, j : integer;
  aChildren : TWordArray;
  aConcepts : TCardinalArray;
  oConcept : TConcept;
begin
  SetLength(aChildren, Count);
  For i := 0 to Count - 1 Do
  Begin
    oConcept := TConcept(ObjectByIndex[i]);
    SetLength(aConcepts, oConcept.Codes.Count);
    For j := 0 to oConcept.Codes.Count - 1 do
      aConcepts[j] := TCode(oConcept.Codes[j]).Index;
    oConcept.Index := oImp.FConcepts.AddConcept(oImp.AddDescription(oConcept.Name), 0, oImp.FRefs.AddCardinals(aConcepts));
    aChildren[i] := oConcept.Index;
  End;
  result := oImp.FConcepts.AddConcept(oImp.AddDescription(sName), oImp.FRefs.AddWords(aChildren), 0);
end;

function TLoincImporter.AddDescription(const s: String): Cardinal;
var
  i : Integer;
begin
  if s = '' then
    result := 0
  else if FStrings.Find(s, i) Then
    result := Cardinal(FStrings.Objects[i])
  Else
  Begin
    result := FDesc.AddEntry(s);
    FStrings.AddObject(s, TObject(result));
  End;
end;

function TLoincImporter.SeeUnits(const s: String): Word;
var
  i : Integer;
begin
  if s = '' then
    result := 0
  else if FUnits.Find(s, i) Then
    result := Cardinal(FUnits.Objects[i])
  Else
  Begin
    result := FConcepts.AddConcept(AddDescription(s), 0, 0);
    FUnits.AddObject(s, TObject(result));
  End;
end;

{ THeirarchyEntry }

constructor THeirarchyEntry.Create;
begin
  inherited;
  FChildren := THeirarchyEntryList.create;
  FDescendents := THeirarchyEntryList.create;
  FConcepts := TCodeList.Create;
  FDescendentConcepts := TCodeList.Create;
end;

destructor THeirarchyEntry.Destroy;
begin
  FConcepts.Free;
  FChildren.free;
  FDescendents.free;
  FDescendentConcepts.free;
  inherited;
end;

{ THeirarchyEntryList }

function THeirarchyEntryList.CompareByCode(pA, pB: Pointer): Integer;
begin
  Result := StringCompare(THeirarchyEntry(pA).FCode, THeirarchyEntry(pB).FCode);
end;

function THeirarchyEntryList.CompareByText(pA, pB: Pointer): Integer;
begin
  Result := StringCompare(THeirarchyEntry(pA).Ftext, THeirarchyEntry(pB).Ftext);
end;

function THeirarchyEntryList.FindByCode(entry: THeirarchyEntry; out iIndex: Integer): Boolean;
begin
  Result := Find(entry, iIndex, CompareByCode);
end;

function THeirarchyEntryList.FindByCode(code: String; out iIndex: Integer): Boolean;
Var
  entry : THeirarchyEntry;
Begin
  entry := THeirarchyEntry(ItemNew);
  Try
    entry.Fcode := code;

    Result := FindByCode(entry, iIndex);
  Finally
    entry.Free;
  End;
end;

function THeirarchyEntryList.getByCode(code: String): THeirarchyEntry;
Var
  iIndex : Integer;
Begin
  If FindByCode(code, iIndex) Then
    Result := Entries[iIndex]
  Else
    Result := Nil;
end;

function THeirarchyEntryList.FindByText(entry: THeirarchyEntry; out iIndex: Integer): Boolean;
begin
  Result := Find(entry, iIndex, CompareByText);
end;

function THeirarchyEntryList.FindByText(text: String; out iIndex: Integer): Boolean;
Var
  entry : THeirarchyEntry;
Begin
  entry := THeirarchyEntry(ItemNew);
  Try
    entry.Ftext := text;

    Result := FindByText(entry, iIndex);
  Finally
    entry.Free;
  End;
end;

function THeirarchyEntryList.getByText(text: String): THeirarchyEntry;
Var
  iIndex : Integer;
Begin
  If FindByText(text, iIndex) Then
    Result := Entries[iIndex]
  Else
    Result := Nil;
end;


function THeirarchyEntryList.GetEntry(iIndex: Integer): THeirarchyEntry;
begin
  result := THeirarchyEntry(ObjectByIndex[iIndex]);
end;

function THeirarchyEntryList.ItemClass: TAdvObjectClass;
begin
  result := THeirarchyEntry;
end;

procedure THeirarchyEntryList.SortedByCode;
begin
  SortedBy(CompareByCode);
end;

procedure THeirarchyEntryList.SortedByText;
begin
  SortedBy(CompareByText);
end;

{ TCodeList }

function TCodeList.CompareByCode(pA, pB: Pointer): Integer;
begin
  Result := StringCompare(TCode(pA).Code, TCode(pB).Code);
end;

function TCodeList.FindByCode(entry: TCode; out iIndex: Integer): Boolean;
begin
  Result := Find(entry, iIndex, CompareByCode);
end;

function TCodeList.FindByCode(code: String; out iIndex: Integer): Boolean;
Var
  entry : TCode;
Begin
  entry := TCode(ItemNew);
  Try
    entry.code := code;

    Result := FindByCode(entry, iIndex);
  Finally
    entry.Free;
  End;
end;

function TCodeList.getByCode(code: String): TCode;
Var
  iIndex : Integer;
Begin
  If FindByCode(code, iIndex) Then
    Result := Entries[iIndex]
  Else
    Result := Nil;
end;

function TCodeList.GetEntry(iIndex: Integer): TCode;
begin
  result := TCode(ObjectByIndex[iIndex]);
end;

function TCodeList.ItemClass: TAdvObjectClass;
begin
  result := TCode;
end;

procedure TCodeList.SortedByCode;
begin
  SortedBy(CompareByCode);
end;


{ TDescribed } 

constructor TDescribed.Create;
begin
  inherited;
  Stems := TAdvIntegerList.Create;
end;

destructor TDescribed.Destroy;
begin
  Stems.Free;
  inherited;
end;


{ TAnswerList }

constructor TAnswerList.Create;
begin
  inherited;
  FAnswers := TAdvList<TAnswer>.create();
end;

destructor TAnswerList.Destroy;
begin
  FAnswers.Free;
  inherited;
end;

{ TAnswer }

constructor TAnswer.create;
begin
  inherited create;
  FParents := TAdvList<TAnswerList>.create;
end;

destructor TAnswer.destroy;
begin
  FParents.Free;
  inherited;
end;

End.

