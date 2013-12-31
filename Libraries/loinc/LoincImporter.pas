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

  TCode =  class (TAdvObject)
  Private
    Code : AnsiString;
    Display : AnsiString;
    Names : Cardinal;
    Index : Integer;
    Comps : TConcept;
    Props : TConcept;
    Time : TConcept;
    System : TConcept;
    Scale : TConcept;
    Method : TConcept;
    Class_ : TConcept;
    v2dt, v3dt : Word;
    Flags : byte;
    Stems : TAdvIntegerList;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function Compare(pA, pB : Pointer) : Integer;
  End;

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

    FStrings : TStringList;
    FUnits : TStringList;
    FVersion: String;
    FWordList : TStringList;
    FStemList : TStringList;
    FStemmer : TYuStemmer_8;
    FOutputFile: String;

    procedure SeeDesc(sDesc: AnsiString; oCode : TCode; iFlags: Byte);
    procedure SeeWord(sDesc: AnsiString; oCode : TCode; iFlags: Byte);

    Function AddDescription(Const s : AnsiString):Cardinal;
    Function SeeUnits(Const s : AnsiString):Word;
    function LoadLOINCDB(o: TKDBConnection; out props : TLoincPropertyIds) : Word;
    function ReadLOINCDatabase(out props : TLoincPropertyIds) : Word;
    procedure Progress(msg : String);
  public
    procedure ImportLOINC;
    Property FileName : String read FFilename write FFilename;
    Property OutputFile : String read FOutputFile write FOutputFile;
    Property Version : String read FVersion write FVersion;
  end;

function importLoinc(filename : String; dest : String) : String;

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

function importLoinc(filename : String; dest : String) : String;
var
  imp : TLoincImporter;
begin
  Writeln('Import LOINC from '+filename);
  imp := TLoincImporter.Create;
  try
    imp.Filename := filename;
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

procedure TLoincImporter.SeeDesc(sDesc: AnsiString; oCode : TCode; iFlags: Byte);
var
  s : AnsiString;
begin
  while (sDesc <> '') Do
  Begin
    AnsiStringSplit(sdesc, [#13, #10, ',', ' ', ':', '.', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '{', '}', '[', ']', '|', '\', ';', '"', '<', '>', '?', '/', '~', '`', '-', '_', '+', '='],
      s, sdesc);
    if (s <> '') {And not StringContainsAny(s, ['0'..'9']) And (length(s) > 3)} Then
      SeeWord(s, oCode, iFlags);
  End;
end;

procedure TLoincImporter.SeeWord(sDesc: AnsiString; oCode : TCode; iFlags: Byte);
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
  if not oList.ExistsByReference(oCode) Then
    oList.Add(oCode.Link);
End;


Function TLoincImporter.LoadLOINCDB(o : TKDBConnection; out props : TLoincPropertyIds) : Word;
var
  iLength : Integer;
  iCount : Integer;
  oCodes, oTemp : TAdvObjectList;
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
  aConcepts : TWordArray;
  i, j : integer;
  iFlag : Byte;
  aCardinals : TCardinalArray;
  iStem : Cardinal;
begin
  result := 0;
  iLength := 0;

  oCodes := TAdvObjectList.Create;
  oComps := TConceptManager.Create;
  oProps := TConceptManager.Create;
  oTime := TConceptManager.Create;
  oSystem := TConceptManager.Create;
  oScale := TConceptManager.Create;
  oMethod := TConceptManager.Create;
  oClass := TConceptManager.Create;
  Try
    oComps.SortedByName;
    oProps.sortedByName;
    oTime.sortedByName;
    oSystem.sortedByName;
    oScale.sortedByName;
    oMethod.sortedByName;
    oClass.sortedByName;
    iCount := 0;
    Progress('Loading Concepts');
    o.sql := 'Select LOINC_NUM, LONG_COMMON_NAME, COMPONENT, PROPERTY, TIME_ASPCT, SYSTEM, SCALE_TYP, METHOD_TYP, CLASS, RELATEDNAMES2, SHORTNAME, '
                +'HL7_V2_DATATYPE, HL7_V3_DATATYPE, ORDER_OBS, UNITSREQUIRED, CLASSTYPE from LOINC order by LOINC_NUM';
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

        oCode.Flags := 0;
//        if o.ColIntegerByName['SETROOT'] = 1 Then
//          oCode.Flags := oCode.Flags + FLAGS_ROOT;
        if sameText(o.ColStringByName['ORDER_OBS'], 'Both') Then
          oCode.Flags := oCode.Flags + FLAGS_ORDER + FLAGS_OBS
        else if sameText(o.ColStringByName['ORDER_OBS'], 'Observation') Then
          oCode.Flags := oCode.Flags + FLAGS_OBS
        else if sameText(o.ColStringByName['ORDER_OBS'], 'Order') Then
          oCode.Flags := oCode.Flags + FLAGS_ORDER
        else if (o.ColStringByName['ORDER_OBS'] <> '') And (o.ColStringByName['ORDER_OBS'] <> 'Subset') Then
          Raise exception.create('unknown order/obs '+o.ColStringByName['ORDER_OBS']);
        if o.ColStringByName['UNITSREQUIRED'] = 'Y' Then
          oCode.Flags := oCode.Flags + FLAGS_UNITS;
//        if o.ColStringByName['FINAL'] <> 'Y' Then
//          oCode.Flags := oCode.Flags + FLAGS_HOLD;
        case o.ColIntegerByName['CLASSTYPE'] of
         2: oCode.Flags := oCode.Flags + FLAGS_CLIN;
         3: oCode.Flags := oCode.Flags + FLAGS_ATT;
         4: oCode.Flags := oCode.Flags + FLAGS_SURV;
         1: ;
        else
          Raise exception.create('unexpected class type '+inttostr(o.ColIntegerByName['CLASSTYPE']));
        End;

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
    For iLoop := 0 to oCodes.Count - 1 Do
    Begin
      oCode := TCode(oCodes[iLoop]);
      oCode.Code := AnsiPadString(oCode.Code, iLength, ' ', false);
    End;
    oCodes.SortedBy(TCode(oCodes[0]).Compare);
    FCode.CodeLength := iLength;

    Progress('Build Cache');
    For iLoop := 0 to oCodes.Count - 1 Do
    Begin
      if iCount mod STEP_COUNT = 0 then
        Progress('');
      inc(iCount);
      oCode := TCode(oCodes[iLoop]);
      Try
        oCode.Index := FCode.AddCode(oCode.Code, AddDescription(oCode.Display), oCode.Names, oCode.v2dt, oCode.v3dt, oCode.Flags);
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
      SetLength(aCardinals, oTemp.Count);
      for j := 0 to oTemp.Count - 1 Do
        aCardinals[j] := TCode(oTemp[j]).Index;
      iStem := FDesc.AddEntry(FStemList[i]);
      FStems.AddStem(iStem, FRefs.AddCardinals(aCardinals));
      for j := 0 to oTemp.Count - 1 Do
        TCode(oTemp[j]).Stems.Add(iStem);
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

Function TLoincImporter.ReadLOINCDatabase(out props : TLoincPropertyIds) : Word;
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
        result := LoadLOINCDB(o, props);
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

    Try
      oSvc.Root := ReadLOINCDatabase(props);
      oSvc.Version := Version;
      oSvc.Properties := props;
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

constructor TCode.Create;
begin
  inherited;
  Stems := TAdvIntegerList.Create;
end;

destructor TCode.Destroy;
begin
  Stems.Free;
  inherited;
end;

End.


