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
  StringSupport, DateSupport,
  AdvObjects, AdvObjectLists, AdvStringLists, AdvIntegerLists, AdvNames,
  KDBManager, KDBODBCExpress, KDBDialects, YuStemmer, LoincServices;

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
    Function See(sName : AnsiString; oCode : TObject) : TConcept;
    Function Store(sName : AnsiString; oImp : TLOINCImporter) : Word;
  End;

  TDescribed = class  (TAdvObject)
  private
    index : Integer;
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
    Code : AnsiString;
    Display : AnsiString;
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

  TLoincImporter = class (TAdvObject)
  private
    FStart : TDateTime;
    FFilename: String;
    TotalConcepts : Integer;

    FDesc : TLoincStrings;
    FCode : TLOINCCodeList;
    FRefs : TLOINCReferences;
    FConcepts : TLOINCConcepts;
    FWords : TLoincWords;
    FStems : TLoincStems;
    FEntries : TLOINCHeirarchyEntryList;

    FStrings : TStringList;
    FUnits : TStringList;
    FVersion: String;
    FWordList : TStringList;
    FStemList : TStringList;
    FStemmer : TYuStemmer_8;
    FOutputFile: String;
    FMultiAxialFilename: String;

    procedure SeeDesc(sDesc: AnsiString; oObj : TDescribed; iFlags: Byte);
    procedure SeeWord(sDesc: AnsiString; oObj : TDescribed; iFlags: Byte);

    Function AddDescription(Const s : AnsiString):Cardinal;
    Function SeeUnits(Const s : AnsiString):Word;
    function LoadLOINCDB(o: TKDBConnection; out props : TLoincPropertyIds; out roots : TCardinalArray; out subsets : TLoincSubsets) : Word;
    function ReadLOINCDatabase(out props : TLoincPropertyIds; out roots : TCardinalArray; out subsets : TLoincSubsets) : Word;
    procedure Progress(msg : String);
    procedure ProcessMultiAxialEntry(oHeirarchy : THeirarchyEntryList; oCodes : TCodeList; ln : string);
    procedure StoreHeirarchyEntry(entry : THeirarchyEntry);
  public
    procedure ImportLOINC;

    Property FileName : String read FFilename write FFilename;
    Property MultiAxialFilename : String read FMultiAxialFilename write FMultiAxialFilename;
    Property OutputFile : String read FOutputFile write FOutputFile;
    Property Version : String read FVersion write FVersion;
  end;

function importLoinc(filename, maFilename : String; dest : String) : String;

Implementation

uses
  AnsiStringBuilder;

Function DetermineVersion(filename : String) : String;
var
  txtFile : String;
  f : TFileStream;
  txt, s : AnsiString;
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
  AnsiStringSplit(txt, [#13], s, txt);
  result := copy(s, length(s)-3, 4);
  if not (StringIsInteger16(copy(result, 1, 1)) and (result[2] = '.') and StringIsInteger16(copy(result, 3, 2))) then
    raise Exception.Create('Unable to read the version from '+txtFile);
end;

function importLoinc(filename, maFilename : String; dest : String) : String;
var
  imp : TLoincImporter;
begin
  Writeln('Import LOINC from '+filename);
  imp := TLoincImporter.Create;
  try
    imp.Filename := filename;
    imp.MultiAxialFilename := mafilename;
    imp.Version := DetermineVersion(filename);
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
    if (sName[i] in ['a'..'z', '_', '-', 'A'..'Z', '0'..'9']) Then
      result := result + sName[i];
  if result = '' then
    result := inttostr(newkey);
End;

procedure TLoincImporter.SeeDesc(sDesc: AnsiString; oObj : TDescribed; iFlags: Byte);
var
  s : AnsiString;
begin
  while (sDesc <> '') Do
  Begin
    AnsiStringSplit(sdesc, [#13, #10, ',', ' ', ':', '.', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '{', '}', '[', ']', '|', '\', ';', '"', '<', '>', '?', '/', '~', '`', '-', '_', '+', '='],
      s, sdesc);
    if (s <> '') {And not StringContainsAny(s, ['0'..'9']) And (length(s) > 3)} Then
      SeeWord(s, oObj, iFlags);
  End;
end;

procedure TLoincImporter.SeeWord(sDesc: AnsiString; oObj : TDescribed; iFlags: Byte);
var
  i, m : integer;
  sStem : AnsiString;
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

  sStem := FStemmer.Stem(sDesc);
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


Function TLoincImporter.LoadLOINCDB(o : TKDBConnection; out props : TLoincPropertyIds; out roots : TCardinalArray; out subsets : TLoincSubsets) : Word;
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
    o.sql := 'Select LOINC_NUM, LONG_COMMON_NAME, COMPONENT, PROPERTY, TIME_ASPCT, SYSTEM, SCALE_TYP, METHOD_TYP, CLASS, RELATEDNAMES2, SHORTNAME, '
                +'HL7_V2_DATATYPE, HL7_V3_DATATYPE, ORDER_OBS, UNITSREQUIRED, CLASSTYPE, STATUS, EXTERNAL_COPYRIGHT_NOTICE from LOINC order by LOINC_NUM';
    o.prepare;
    Try
      o.execute;
      while o.fetchnext do
      begin
        oCode := TCode.Create;
        oCodes.Add(oCode);

        oCode.Names := 0;
        oCode.Code := Trim(o.ColStringByName['LOINC_NUM']);
        oCode.Display := Trim(o.ColStringByName['LONG_COMMON_NAME']);
        if Length(oCode.Code) > iLength Then
          iLength := Length(oCode.Code);

        oCode.Comps := oComps.See(o.ColStringByName['COMPONENT'], oCode);
        oCode.Props := oProps.See(o.ColStringByName['PROPERTY'], oCode);
        oCode.Time := oTime.See(o.ColStringByName['TIME_ASPCT'], oCode);
        oCode.System := oSystem.See(o.ColStringByName['SYSTEM'], oCode);
        oCode.Scale := oScale.See(o.ColStringByName['SCALE_TYP'], oCode);
        oCode.Method := oMethod.See(o.ColStringByName['METHOD_TYP'], oCode);
        oCode.Class_ := oClass.See(o.ColStringByName['CLASS'], oCode);

        oSubsets[lsiAll].add(oCode.Link);
        oCode.Flags := 0;
//        if o.ColIntegerByName['SETROOT'] = 1 Then
//          oCode.Flags := oCode.Flags + FLAGS_ROOT;
        if sameText(o.ColStringByName['ORDER_OBS'], 'Both') Then    
        begin
          oCode.Flags := oCode.Flags + FLAGS_ORDER + FLAGS_OBS;
          oSubsets[lsiOrderObs].add(oCode.Link);
        end
        else if sameText(o.ColStringByName['ORDER_OBS'], 'Observation') Then
        begin
          oCode.Flags := oCode.Flags + FLAGS_OBS;
          oSubsets[lsiObs].add(oCode.Link);
        end
        else if sameText(o.ColStringByName['ORDER_OBS'], 'Order') Then
        begin
          oCode.Flags := oCode.Flags + FLAGS_ORDER;
          oSubsets[lsiOrder].add(oCode.Link);
        end
        else if (o.ColStringByName['ORDER_OBS'] <> '') And (o.ColStringByName['ORDER_OBS'] <> 'Subset') Then
          Raise exception.create('unknown order/obs '+o.ColStringByName['ORDER_OBS'])
        else
          oSubsets[lsiOrderSubset].add(oCode.Link);
        
        if o.ColStringByName['UNITSREQUIRED'] = 'Y' Then
          oCode.Flags := oCode.Flags + FLAGS_UNITS;
        if o.ColNullByName['EXTERNAL_COPYRIGHT_NOTICE'] then
          oSubsets[lsiInternal].add(oCode.Link)
        else
          oSubsets[lsi3rdParty].add(oCode.Link);
        
        case o.ColIntegerByName['CLASSTYPE'] of
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
          Raise exception.create('unexpected class type '+inttostr(o.ColIntegerByName['CLASSTYPE']));
        End;
        if SameText(o.ColStringByName['STATUS'], 'Active') then
          oSubsets[lsiActive].add(oCode.Link)
        else if SameText(o.ColStringByName['STATUS'], 'Deprecated') then
          oSubsets[lsiDeprecated].add(oCode.Link)
        else if SameText(o.ColStringByName['STATUS'], 'Discouraged') then
          oSubsets[lsiDiscouraged].add(oCode.Link)
        else if SameText(o.ColStringByName['STATUS'], 'Trial') then
          oSubsets[lsiTrial].add(oCode.Link)
        else
          raise Exception.Create('Unknown LOINC Code status '+o.ColStringByName['STATUS']);

        SeeDesc(oCode.Display, oCode, FLAG_LONG_COMMON);

        oCode.v2dt := SeeUnits(o.ColStringByName['HL7_V2_DATATYPE']);
        oCode.v3dt := SeeUnits(o.ColStringByName['HL7_V3_DATATYPE']);
           
        oNames := TAdvStringList.Create;
        Try
          oNames.Symbol := ';';
          oNames.AsText := o.ColStringByName['RELATEDNAMES2'];
          if o.ColStringByName['SHORTNAME'] <> '' Then
            oNames.Insert(0, o.ColStringByName['SHORTNAME']);

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
      End;
    Finally
      o.Terminate;
    End;
    Progress('Sort Codes');
    oCodes.SortedByCode;

    // now, process the multi-axial file
    Progress('Loading Multi-Axial Source');
    oHeirarchy.SortedByCode;
    AssignFile(ma, FMultiAxialFilename);
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
    for i := 0 to oHeirarchy.Count - 1 do
      // first, we simply assign them all an index. Then we'll create everything else, and then go and actually store them
      oHeirarchy[i].index := i;

    Progress('Build Cache');
    For iLoop := 0 to oCodes.Count - 1 Do
    Begin
      oCode := TCode(oCodes[iLoop]);
      oCode.Code := AnsiPadString(oCode.Code, iLength, ' ', false);
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
  End;
End;

Function TLoincImporter.ReadLOINCDatabase(out props : TLoincPropertyIds; out roots : TCardinalArray; out subsets : TLoincSubsets) : Word;
var
  oLink : TKDBManager;
  o : TKDBConnection;
Begin
  FStrings := TStringList.Create;
  FStrings.Sorted := True;
  FUnits := TStringList.Create;
  FUnits.Sorted := true;
  try
    oLink := TKDBOdbcDirect.Create('loinc', 1, 'Microsoft Access Driver (*.mdb)', '', FFilename, '', '');
    Try
      o := oLink.GetConnection('load');
      Try
        result := LoadLOINCDB(o, props, roots, subsets);
      Finally
        o.Release;
      End;
    Finally
      oLink.Free;
    End;
  Finally
    FUnits.Free;
    FStrings.Free;
  End;
End;

Procedure TLoincImporter.ImportLOINC;
var
  oSvc : TLOINCServices;
  props : TLoincPropertyIds;
  newKey : integer;
  i, iStatus : integer;
  roots : TCardinalArray;
  subsets : TLoincSubsets;
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
    FCode := oSvc.CodeList;
    FCode.StartBuild;
    FRefs := oSvc.Refs;
    FRefs.StartBuild;
    FConcepts := oSvc.Concepts;
    FConcepts.StartBuild;
    FWords := oSvc.Words;
    FStems := oSvc.Stems;
    FEntries := oSvc.Entries;

    Try
      oSvc.Root := ReadLOINCDatabase(props, roots, subsets);
      oSvc.Version := Version;
      oSvc.Properties := props;
      oSvc.HeirarchyRoots := roots;
      oSvc.Subsets := subsets;
    Finally
      FConcepts.DoneBuild;
      FRefs.DoneBuild;
      Fdesc.doneBuild;
    End;

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

function TConceptManager.See(sName: AnsiString; oCode: TObject): TConcept;
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

function TConceptManager.Store(sName : AnsiString; oImp : TLoincImporter): Word;
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

function TLoincImporter.AddDescription(const s: AnsiString): Cardinal;
var
  i : Integer;
begin
  if FStrings.Find(s, i) Then
    result := Cardinal(FStrings.Objects[i])
  Else
  Begin
    result := FDesc.AddEntry(s);
    FStrings.AddObject(s, TObject(result));
  End;
end;

function TLoincImporter.SeeUnits(const s: AnsiString): Word;
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


End.

