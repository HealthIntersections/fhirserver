Unit ftx_loinc_importer;

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
  SysUtils, Contnrs, Classes,
  fsl_base, fsl_utilities, fsl_collections, fsl_stream, fsl_fpc,
  ftx_loinc_services;

Const
  FLAG_LONG_COMMON = 1;
  FLAG_LONG_RELATED = 2;
  STEP_COUNT = 100;
  ESTIMATED_CONCEPTS = 83000;
  ESTIMATED_HEIRACHY = 85000;
  ESTIMATED_ANSWERS = 47000;


Type
  TLOINCImporter = class;

  THeirarchyEntry = class;
  THeirarchyEntryList = class;
  TCodeList = class;

  TConcept = class (TFslName)
  private
    Index : Cardinal;
    Codes : TObjectList;
    Flang : integer;
  public
    constructor Create(lang : byte);
    destructor Destroy; Override;
  End;

  TConceptArray = Array of TConcept;

  TConceptManager = class (TFslNameList)
  public
    Function See(lang: byte; sName : String; oCode : TObject) : TConcept;
    Function Store(langCount, lang: byte; sName : String; oImp : TLOINCImporter) : Cardinal;
  End;

  TDescribed = class  (TFslObject)
  private
    index : Cardinal;
    Stems : TFslIntegerList;
  public
    constructor Create; Override;
    destructor Destroy; Override;
  end;

  { THeirarchyEntry }

  THeirarchyEntry = class (TDescribed)
  private
    FCode : String;
    FText : String;
    FParents : THeirarchyEntryList;
    FChildren : THeirarchyEntryList;
    FConcepts : TCodeList;
    FDescendentConcepts : TCodeList;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function link : THeirarchyEntry;
  End;

  THeirarchyEntryList = class (TFslObjectList)
  private
    function GetEntry(iIndex: Integer): THeirarchyEntry;
  protected
    Function ItemClass : TFslObjectClass; Override;
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

  { TCode }

  TCode = class (TDescribed)
  Private
    Code : String;
    Display : String;
    Names : Cardinal;
    Comps : TConceptArray;
    Props : TConceptArray;
    Time : TConceptArray;
    System : TConceptArray;
    Scale : TConceptArray;
    Method : TConceptArray;
    Class_ : TConceptArray;
    Flags : byte;
    entries : TFslList<THeirarchyEntry>;
  public
    constructor Create(langCount : integer);
    destructor Destroy; override;
    Function Compare(pA, pB : Pointer) : Integer;
  End;

  TCodeList = class (TFslObjectList)
  private
    function GetEntry(iIndex: Integer): TCode;
  protected
    Function ItemClass : TFslObjectClass; Override;
    Function CompareByCode(pA, pB: Pointer): Integer; Virtual;
    Function FindByCode(entry : TCode; Out iIndex: Integer): Boolean; Overload;
  public
    Procedure SortedByCode;

    Function getByCode(code : String) : TCode;
    Function FindByCode(code: String; Out iIndex: Integer): Boolean; Overload;

    Property Entries[iIndex : Integer] : TCode Read GetEntry; Default;
  end;

  TAnswerList = class;

  TAnswer = class (TFslObject)
  private
    FCode: String;
    FDescription: String;
    FParents: TFslList<TAnswerList>;
    FIndex : integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    Property Code : String read FCode write FCode;
    Property Description : String read FDescription write FDescription;
    Property Index : Integer read FIndex write FIndex;
    Property Parents : TFslList<TAnswerList> read FParents;
  end;

  TAnswerList = class (TFslObject)
  private
    FCode: String;
    FDescription: String;
    FAnswers : TFslList<TAnswer>;
    FIndex : integer;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    Property Code : String read FCode write FCode;
    Property Description : String read FDescription write FDescription;
    Property Answers : TFslList<TAnswer> read FAnswers;
  end;

  TLoincLanguageCodes = class (TFslObject)
  private
    FComponent : String;
    FProperty : String;
    FTimeAspect : String;
    FSystem : String;
    FScale : String;
    FMethod  : String;
    FClass : String;
    FShortname : String;
    FLongName : String;
    FRelatedNames : TStringList;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    Function link : TLoincLanguageCodes; overload;

    function Display : String;

    property Component : String read FComponent write FComponent;
    property Prop : String read FProperty write FProperty;
    property TimeAspect : String read FTimeAspect write FTimeAspect;
    property System : String read FSystem write FSystem;
    property Scale : String read FScale write FScale;
    property Method : String read FMethod write FMethod;
    property Clss : String read FClass write FClass;
    property Shortname : String read FShortname write FShortname;
    property LongName : String read FLongName write FLongName;
    property RelatedNames : TStringList read FRelatedNames;
  end;

  TLoincLanguage = class (TFslObject)
  private
    FLang : String;
    FCountry : String;
    FCodes : TFslMap<TLoincLanguageCodes>;
  public
    constructor Create; overload; Override;
    constructor Create(lang, country : String); overload;
    destructor Destroy; Override;
    Function link : TLoincLanguage; overload;

    Property Lang : String read FLang write FLang;
    Property Country : String read FCountry write FCountry;
    Property Codes : TFslMap<TLoincLanguageCodes> read FCodes;
  end;

  { TLoincImporter }

  TLoincImporter = class (TFslObject)
  private
    callback : TInstallerCallback;
    lastmessage : String;
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
    FAnswerLists : TFslMap<TAnswerList>;
    FAnswerMap : TFslMap<TAnswer>;
    FAnswers : TLOINCAnswersList;
    FLanguages : TFslList<TLoincLanguage>;
    FLangs : TLoincLanguages;

    FStrings : TStringList;
    FUnits : TStringList;
    FVersion: String;
    FWordList : TStringList;
    FStemList : TStringList;
    FStemmer : TFslWordStemmer;
    FOutputFile: String;
    FDate: String;

    procedure AddToDescendentConcepts(oHeirarchy: THeirarchyEntryList;  entry: TCode; path: String);
    procedure SeeDesc(sDesc: String; oObj : TDescribed; iFlags: Byte);
    procedure SeeWord(sDesc: String; oObj : TDescribed; iFlags: Byte);
    function listConcepts(arr : TConceptArray) : TCardinalArray;
    procedure registerParents(oHeirarchy : THeirarchyEntryList; entry : THeirarchyEntry; path : String);

    Function AddDescription(lang : byte; s : String):Cardinal;
    procedure readLanguage(i : integer; index : String; lang : TLoincLanguage);
    procedure ReadLanguageVariants();
    function LoadLOINCFiles(folder : String; out props : TLoincPropertyIds; out roots : TCardinalArray; out subsets : TLoincSubsets) : Cardinal;
    function ReadLOINCDatabase(out props : TLoincPropertyIds; out roots : TCardinalArray; out subsets : TLoincSubsets) : Cardinal;
    procedure ProcessMultiAxialEntry(oHeirarchy : THeirarchyEntryList; oCodes : TCodeList; ln : string);
    procedure ProcessAnswerLine(oHeirarchy : THeirarchyEntryList; oCodes : TCodeList; ln : string);
    procedure StoreHeirarchyEntry(entry : THeirarchyEntry);
    procedure Progress(Step : integer; pct : real; msg : String);
  public
    procedure ImportLOINC;

    Property Folder : String read FFolder write FFolder;
    Property OutputFile : String read FOutputFile write FOutputFile;
    Property Version : String read FVersion write FVersion;
    Property Date : String read FDate write FDate;
  end;

function importLoinc(folder, version, date, dest : String; callback : TInstallerCallback = nil) : String;

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
    raise ETerminologySetup.create('Unable to find the file '+txtFile+' so the version can be determined');
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
    raise ETerminologySetup.create('Unable to read the version from '+txtFile);
end;

function importLoinc(folder, version, date, dest : String; callback : TInstallerCallback = nil) : String;
var
  imp : TLoincImporter;
begin
  imp := TLoincImporter.Create;
  try
    imp.callback := callback;
    imp.progress(0, 0, 'Import LOINC from '+folder);
    imp.Folder := folder;
    imp.Version := version;
    result := dest;
    imp.outputFile := result;
    imp.ImportLOINC;
    imp.progress(16, 0, 'Done '+inttostr(imp.TotalConcepts)+' Concepts');
  finally
    imp.Free;
  end;
end;



function TCode.Compare(pA, pB: Pointer): Integer;
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
  oList : TFslObjectList;
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
    oList := TFslObjectList.Create;
    oList.SortedByReference;
    FStemList.AddObject(sStem, oList);
  End
  Else
    oList := TFslObjectList(FStemList.Objects[i]);
  if not oList.ExistsByReference(oObj) Then
    oList.Add(oObj.Link);
End;


procedure TLoincImporter.StoreHeirarchyEntry(entry: THeirarchyEntry);
var
  parents, children, concepts, descendentConcepts: Cardinal;
  refs : TCardinalArray;
  i : integer;
begin
  setLength(refs, entry.FChildren.Count);
  for i := 0 to entry.FChildren.Count - 1 do
    refs[i] := entry.FChildren[i].index;
  children := FRefs.AddRefs(refs);

  setLength(refs, entry.FConcepts.Count);
  entry.FConcepts.SortedByCode;
  for i := 0 to entry.FConcepts.Count - 1 do
    refs[i] := TCode(entry.FConcepts[i]).index;
  concepts := FRefs.AddRefs(refs);

  setLength(refs, entry.FDescendentConcepts.Count);
  entry.FDescendentConcepts.SortedByCode;
  for i := 0 to entry.FDescendentConcepts.Count - 1 do
    refs[i] := TCode(entry.FDescendentConcepts[i]).index;
  descendentConcepts := FRefs.AddRefs(refs);

  setLength(refs, entry.FParents.Count);
  for i := 0 to entry.FParents.Count - 1 do
    refs[i] := entry.FParents[i].index;
  parents := FRefs.AddRefs(refs);

  if FEntries.AddEntry(AddDescription(0, entry.FCode), AddDescription(0, entry.FText), parents, children, concepts, descendentConcepts) <> entry.index then
    raise ETerminologySetup.create('Out of order');
end;

Const
  FLD_LOINC_NUM =                             0;
  FLD_COMPONENT =                             FLD_LOINC_NUM + 1;
  FLD_PROPERTY =                              FLD_COMPONENT + 1;
  FLD_TIME_ASPCT =                            FLD_PROPERTY + 1;
  FLD_SYSTEM =                                FLD_TIME_ASPCT + 1;
  FLD_SCALE_TYP =                             FLD_SYSTEM + 1;
  FLD_METHOD_TYP =                            FLD_SCALE_TYP + 1;
  FLD_CLASS =                                 FLD_METHOD_TYP + 1;
//  FLD_SOURCE =                                8; removed as of 2.56
  FLD_VersionLastChanged =                    FLD_CLASS + 1;
  FLD_CHNG_TYPE =                             FLD_VersionLastChanged + 1;
  FLD_DefinitionDescription =                 FLD_CHNG_TYPE + 1;
  FLD_STATUS =                                FLD_DefinitionDescription + 1;
  FLD_CONSUMER_NAME =                         FLD_STATUS + 1;
  FLD_CLASSTYPE =                             FLD_CONSUMER_NAME + 1;
  FLD_FORMULA =                               FLD_CLASSTYPE + 1;
  FLD_SPECIES =                               FLD_FORMULA + 1;
  FLD_EXMPL_ANSWERS =                         FLD_SPECIES + 1;
  FLD_SURVEY_QUEST_TEXT =                     FLD_EXMPL_ANSWERS + 1;
  FLD_SURVEY_QUEST_SRC =                      FLD_SURVEY_QUEST_TEXT + 1;
  FLD_UNITSREQUIRED =                         FLD_SURVEY_QUEST_SRC + 1;
  FLD_SUBMITTED_UNITS =                       FLD_UNITSREQUIRED + 1;
  FLD_RELATEDNAMES2 =                         FLD_SUBMITTED_UNITS + 1;
  FLD_SHORTNAME =                             FLD_RELATEDNAMES2 + 1;
  FLD_ORDER_OBS =                             FLD_SHORTNAME + 1;
  FLD_CDISC_COMMON_TESTS =                    FLD_ORDER_OBS + 1;
  FLD_HL7_FIELD_SUBFIELD_ID =                 FLD_CDISC_COMMON_TESTS + 1;
  FLD_EXTERNAL_COPYRIGHT_NOTICE =             FLD_HL7_FIELD_SUBFIELD_ID + 1;
  FLD_EXAMPLE_UNITS =                         FLD_EXTERNAL_COPYRIGHT_NOTICE + 1;
  FLD_LONG_COMMON_NAME =                      FLD_EXAMPLE_UNITS + 1;
  FLD_UnitsAndRange =                         FLD_LONG_COMMON_NAME + 1;
  FLD_DOCUMENT_SECTION =                      FLD_UnitsAndRange + 1;
  FLD_EXAMPLE_UCUM_UNITS =                    FLD_DOCUMENT_SECTION + 1;
  FLD_EXAMPLE_SI_UCUM_UNITS =                 FLD_EXAMPLE_UCUM_UNITS + 1;
  FLD_STATUS_REASON =                         FLD_EXAMPLE_SI_UCUM_UNITS + 1;
  FLD_STATUS_TEXT =                           FLD_STATUS_REASON + 1;
  FLD_CHANGE_REASON_PUBLIC =                  FLD_STATUS_TEXT + 1;
  FLD_COMMON_TEST_RANK =                      FLD_CHANGE_REASON_PUBLIC + 1;
  FLD_COMMON_ORDER_RANK =                     FLD_COMMON_TEST_RANK + 1;
  FLD_COMMON_SI_TEST_RANK =                   FLD_COMMON_ORDER_RANK + 1;
  FLD_HL7_ATTACHMENT_STRUCTURE =              FLD_COMMON_SI_TEST_RANK + 1;
  FLD_EXTERNAL_COPYRIGHT_LINK =               FLD_HL7_ATTACHMENT_STRUCTURE + 1;
  FLD_PanelType =                             FLD_EXTERNAL_COPYRIGHT_LINK + 1;
  FLD_AskAtOrderEntry =                       FLD_PanelType + 1;
  FLD_AssociatedObservations =                FLD_AskAtOrderEntry + 1;


Function TLoincImporter.LoadLOINCFiles(folder : String; out props : TLoincPropertyIds; out roots : TCardinalArray; out subsets : TLoincSubsets) : Cardinal;
var
  iLength : Integer;
  iCount : Integer;
  oCodes : TCodeList;
  oTemp : TFslObjectList;
  oCode : TCode;
  iLoop, iLang : Integer;
  oNames : TStringList;
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
  aConcepts : TCardinalArray;
  oSubsets : Array[TLoincSubsetId] of TCodeList;
  i, j : integer;
  iFlag : Byte;
  aCardinals : TCardinalArray;
  iStem, e : Cardinal;
  ma : Text;
  ln : String;
  a : TLoincSubsetId;
  csv : TFslCSVExtractor;
  items : TFslStringList;
  f : TFslFile;
  st, st2 : TStringList;
  answerlist : TAnswerList;
  answer : TAnswer;
  s : String;
  lc : TLoincLanguageCodes;
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
  FAnswerLists := TFslMap<TAnswerList>.Create('loinc.answers');
  FAnswerMap := TFslMap<TAnswer>.create('loinc.answers2');
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
    Progress(0, 24, 'Loading Languages');
    if FileExists(IncludeTrailingPathDelimiter(folder)+ 'LinguisticVariants.csv') then
      ReadLanguageVariants();


    iCount := 0;
    Progress(0, 0, 'Loading Concepts');
    items := TFslStringList.create;
    f := TFslFile.Create(IncludeTrailingPathDelimiter(folder)+ 'loinc.csv', fmOpenRead);
    try
      csv := TFslCSVExtractor.Create(f.Link, TEncoding.UTF8);
      Try
        // headers
        csv.ConsumeEntries(items);

        while csv.More do
        begin
          items.Clear;
          csv.ConsumeEntries(items);
          if items.count > 0 then
          begin
            oCode := TCode.Create(FLanguages.Count);
            oCodes.Add(oCode);

            oCode.Names := 0;
            oCode.Code := Trim(items[FLD_LOINC_NUM]);
            oCode.Display := Trim(items[FLD_LONG_COMMON_NAME]);
            if Length(oCode.Code) > iLength Then
              iLength := Length(oCode.Code);

            oCode.Comps[0] := oComps.See(0, items[FLD_COMPONENT], oCode);
            oCode.Props[0] := oProps.See(0, items[FLD_PROPERTY], oCode);
            oCode.Time[0] := oTime.See(0, items[FLD_TIME_ASPCT], oCode);
            oCode.System[0] := oSystem.See(0, items[FLD_SYSTEM], oCode);
            oCode.Scale[0] := oScale.See(0, items[FLD_SCALE_TYP], oCode);
            oCode.Method[0] := oMethod.See(0, items[FLD_METHOD_TYP], oCode);
            oCode.Class_[0] := oClass.See(0, items[FLD_CLASS], oCode);
            for iLang := 1 to Flanguages.count - 1 do
            begin
              if Flanguages[iLang].Codes.TryGetValue(oCode.Code, lc) then
              begin
                oCode.Comps[iLang] := oComps.See(iLang, lc.Component, oCode);
                oCode.Props[iLang] := oProps.See(iLang, lc.Prop, oCode);
                oCode.Time[iLang] := oTime.See(iLang, lc.TimeAspect, oCode);
                oCode.System[iLang] := oSystem.See(iLang, lc.System, oCode);
                oCode.Scale[iLang] := oScale.See(iLang, lc.Scale, oCode);
                oCode.Method[iLang] := oMethod.See(iLang, lc.Method, oCode);
                oCode.Class_[iLang] := oClass.See(iLang, lc.Clss, oCode);
              end;
            end;

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
              raise ETerminologySetup.create('unknown order/obs '+items[FLD_ORDER_OBS])
            else
              oSubsets[lsiOrderSubset].add(oCode.Link);

            if items[FLD_UNITSREQUIRED] = 'Y' Then
              oCode.Flags := oCode.Flags + FLAGS_UNITS;
            if items[FLD_EXTERNAL_COPYRIGHT_NOTICE] = '' then
              oSubsets[lsiInternal].add(oCode.Link)
            else
              oSubsets[lsi3rdParty].add(oCode.Link);

            s := items[FLD_CLASSTYPE];
            if s = '' then
              s := '1';
            if not StringIsInteger32(s) then
              raise ETerminologySetup.create('Error');

            case StrToInt(s) of
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
              raise ETerminologySetup.create('unexpected class type '+items[FLD_CLASSTYPE]);
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
              raise ETerminologySetup.create('Unknown LOINC Code status '+items[FLD_STATUS]);

            SeeDesc(oCode.Display, oCode, FLAG_LONG_COMMON);

            oNames := TStringList.Create;
            Try
              if items[FLD_SHORTNAME] <> '' Then
                oNames.AddObject(items[FLD_SHORTNAME], TObject(0));
              for s in items[FLD_RELATEDNAMES2].Split([';']) do
                if oNames.IndexOf(items[FLD_SHORTNAME]) = -1 then
                  oNames.AddObject(items[FLD_SHORTNAME], TObject(0));
              for iLang := 1 to FLanguages.Count - 1 do
              begin
                if FLanguages[iLang].Codes.TryGetValue(oCode.Code, lc) then
                begin
                  if lc.Shortname < '' then
                    if oNames.IndexOf(lc.Shortname) = -1 then
                      oNames.AddObject(lc.Shortname, TObject(iLang));
                  for s in lc.RelatedNames do
                    if oNames.IndexOf(s) = -1 then
                      oNames.AddObject(s, TObject(iLang));
                end;
              end;

              if oNames.Count > 0 Then
              Begin
                SetLength(aNames, oNames.Count);
                For iLoop := 0 to oNames.Count - 1 Do
                  If Trim(oNames[iLoop]) <> '' Then
                  Begin
                    aNames[iLoop] := addDescription(integer(oNames.Objects[iLoop]), Trim(oNames[iLoop]));
                    if integer(oNames.Objects[iLoop]) = 0 then
                      SeeDesc(oNames[iLoop], oCode, FLAG_LONG_RELATED);
                  End
                  Else
                    aNames[iLoop] := 0;

                oCode.Names := FRefs.AddRefs(aNames);
              End;
            Finally
              oNames.Free;
            End;

            // properties

            if iCount mod STEP_COUNT = 0 then
              Progress(0, iCount * 3 / ESTIMATED_CONCEPTS, '');
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
    Progress(3, 0, 'Sort Codes');
    oCodes.SortedByCode;

    // now, process the multi-axial file
    if FileExists(IncludeTrailingPathDelimiter(folder) + 'MultiAxialHierarchy.csv') then
    begin
      Progress(3,0,'Loading Multi-Axial Source');
      oHeirarchy.SortedByCode;
      AssignFile(ma, IncludeTrailingPathDelimiter(folder) + 'MultiAxialHierarchy.csv');
      Reset(ma);
      Readln(ma, ln); // skip header

      iCount := 0;
      while not eof(ma) do
      begin
        Readln(ma, ln);
        ProcessMultiAxialEntry(oHeirarchy, oCodes, ln);
        if iCount mod STEP_COUNT = 0 then
          Progress(3, iCount*2/ESTIMATED_HEIRACHY, '');
        inc(iCount);
      end;
      CloseFile(ma);
      for i := 0 to oHeirarchy.Count - 1 do
        // first, we simply assign them all an index. Then we'll create everything else, and then go and actually store them
        oHeirarchy[i].index := i;
    end;

    if FileExists(IncludeTrailingPathDelimiter(folder) + 'answerList.csv') then
    begin
      Progress(5, 0, 'Loading Answer Lists');
      AssignFile(ma, IncludeTrailingPathDelimiter(folder) + 'answerList.csv');
      Reset(ma);
      Readln(ma, ln); // skip header
      iCount := 0;
      while not eof(ma) do
      begin
        Readln(ma, ln);
        if (iCount = 4749) then
          inc(iCount);
        ProcessAnswerLine(oHeirarchy, oCodes, ln);
        if iCount mod STEP_COUNT = 0 then
          Progress(5, iCount / ESTIMATED_ANSWERS, '');
        inc(iCount);
      end;
      CloseFile(ma);
    end;

    Progress(6, 0, 'Index Codes');
    For iLoop := 0 to oCodes.Count - 1 Do
    Begin
      oCode := TCode(oCodes[iLoop]);
      oCode.Code := StringPadRight(oCode.Code, ' ', iLength);
      if iLoop mod STEP_COUNT = 0 then
        Progress(6, iCount * 0.5 / oCodes.Count, '');
    End;
    FCode.CodeLength := iLength;

    For iLoop := 0 to oCodes.Count - 1 Do
    Begin
      if iCount mod STEP_COUNT = 0 then
        Progress(6, 0.5+(iCount *0.5 / oCodes.Count), '');
      inc(iCount);
      oCode := TCode(oCodes[iLoop]);
      Try
        SetLength(aCardinals, oCode.entries.Count);
        for i := 0 to oCode.entries.count - 1 do
          aCardinals[i] := oCode.entries[i].index;
        e := FRefs.AddRefs(aCardinals);

        SetLength(aNames, FLanguages.Count);
        aNames[0] := AddDescription(0, oCode.Display);
        For iLang := 1 to FLanguages.Count - 1 Do
        begin
          if FLanguages[iLang].Codes.TryGetValue(oCode.Code, lc) then
            aNames[iLang] := AddDescription(iLang, lc.Display)
          else
            aNames[iLang] := 0;
        end;
        oCode.Index := FCode.AddCode(oCode.Code, FRefs.AddRefs(aNames), oCode.Names, e, oCode.Flags);
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
    Props[lptComponents] := oComps.Store(FLanguages.Count, 0, 'Components', self);
    aConcepts[0] := Props[lptComponents];
    Props[lptProperties] := oProps.Store(FLanguages.Count, 0, 'Properties', self);
    aConcepts[1] := Props[lptProperties];
    Props[lptTimeAspects] := oTime.Store(FLanguages.Count, 0, 'Time Aspects', self);
    aConcepts[2] := Props[lptTimeAspects];
    Props[lptSystems] := oSystem.Store(FLanguages.Count, 0, 'Systems', self);
    aConcepts[3] := Props[lptSystems];
    Props[lptScales] := oScale.Store(FLanguages.Count, 0, 'Scales', self);
    aConcepts[4] := Props[lptScales];
    Props[lptMethods] := oMethod.Store(FLanguages.Count, 0, 'Methods', self);
    aConcepts[5] := Props[lptMethods];
    Props[lptClasses] := oClass.Store(FLanguages.Count, 0, 'Classes', self);
    aConcepts[6] := Props[lptClasses];
    result := FConcepts.AddConcept(AddDescription(0, 'LOINC Definitions'), false, FRefs.AddRefs(aConcepts), 0);
    FCode.donebuild;

    Progress(7, 0, 'Storing Heirachy');
    FEntries.StartBuild;
    iCount := 0;
    SetLength(roots, 0);
    for i := 0 to oHeirarchy.Count - 1 do
    begin
      if iCount mod STEP_COUNT = 0 then
        Progress(7, i / oHeirarchy.Count, '');
      inc(iCount);
      if oHeirarchy[i].FParents.Count = 0 then
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
      subsets[a] := FRefs.AddRefs(aCardinals);
    end;

    Progress(8, 0, 'Processing Answer Lists');
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


        if i mod STEP_COUNT = 0 then
          Progress(8, i/st2.Count, '');
        answer := TAnswer(st2.Objects[i]);
        SetLength(aCardinals, answer.FParents.Count);
        for j := 0 to answer.FParents.Count - 1 do
          aCardinals[j] := answer.FParents[j].FIndex;
        answer.Index := FAnswers.AddEntry(AddDescription(0, answer.Code), AddDescription(0, answer.FDescription), FRefs.AddRefs(aCardinals));
      end;
      for i := 0 to st.Count - 1 do
      begin
        if i mod STEP_COUNT = 0 then
          Progress(9, i/st2.Count, '');
        answerlist := TAnswerList(st.Objects[i]);
        SetLength(aCardinals, answerlist.FAnswers.Count);
        for j := 0 to answerlist.FAnswers.Count - 1 do
          aCardinals[j] := answerlist.FAnswers[j].FIndex;
        j := FAnswers.AddEntry(AddDescription(0, answerlist.Code), AddDescription(0, answerlist.FDescription), FRefs.AddRefs(aCardinals));
        if (j <> answerlist.FIndex) then
          raise ETerminologySetup.create('Error Message');
      end;
    finally
      st.Free;
      st2.Free;
    end;
    FAnswers.DoneBuild;



    Progress(10, 0, 'Processing Words');
    FWords.StartBuild;
    For i := 0 to FWordList.Count - 1 Do
    Begin
      if i mod STEP_COUNT = 0 then
        Progress(10, i/FWordList.Count, '');
      iFlag := Integer(FWordList.Objects[i]);
      FWords.AddWord(FDesc.AddEntry(0, FWordList[i]), iFlag);
    End;
    FWords.DoneBuild;

    Progress(11, 0, 'Processing Stems');
    FStems.StartBuild;
    For i := 0 to FStemList.Count - 1 Do
    Begin
      if i mod STEP_COUNT = 0 then
        Progress(11,  i / FStemList.Count, '');
      oTemp := TFslObjectList(FStemList.Objects[i]);
      iStem := FDesc.AddEntry(0, FStemList[i]);
      FStems.AddStem(iStem);
      for j := 0 to oTemp.Count - 1 Do
        TDescribed(oTemp[j]).Stems.Add(iStem);
      oTemp.Free;
      FStemList.Objects[i] := nil;
    End;
    FStems.DoneBuild;
    For i := 0 to oCodes.Count - 1 Do
    Begin
      if i mod STEP_COUNT = 0 then
        Progress(12,  i / oCodes.Count, '');
      oCode := TCode(oCodes[i]);
      SetLength(aCardinals, oCode.Stems.Count);
      for j := 0 to oCode.Stems.Count - 1 do
        aCardinals[j] := oCode.Stems[j];
      FCode.SetStems(oCode.Index, FRefs.AddRefs(aCardinals));
    End;
    For i := 0 to oHeirarchy.Count - 1 Do
    Begin
      if i mod STEP_COUNT = 0 then
        Progress(13,  i / oHeirarchy.Count, '');
      oEntry := oHeirarchy[i];
      SetLength(aCardinals, oEntry.Stems.Count);
      for j := 0 to oEntry.Stems.Count - 1 do
        aCardinals[j] := oEntry.Stems[j];
      FEntries.SetStems(oEntry.Index, FRefs.AddRefs(aCardinals));
    End;

    Progress(14, 0, 'Cross-Linking');
    For iLoop := 0 to oCodes.Count - 1 Do
    Begin
      if iLoop mod STEP_COUNT = 0 then
        Progress(14, iLoop / oCodes.Count, '');
      oCode := TCode(oCodes[iLoop]);
      if oCode.Comps <> nil Then
        FCode.SetComponents(oCode.Index, FRefs.AddRefs(listConcepts(oCode.Comps)));
      if oCode.Props <> nil Then
        FCode.SetPropertys(oCode.Index, FRefs.AddRefs(listConcepts(oCode.Props)));
      if oCode.Time <> nil Then
        FCode.SetTimeAspects(oCode.Index, FRefs.AddRefs(listConcepts(oCode.Time)));
      if oCode.System <> nil Then
        FCode.SetSystems(oCode.Index, FRefs.AddRefs(listConcepts(oCode.System)));
      if oCode.Scale <> nil Then
        FCode.SetScales(oCode.Index, FRefs.AddRefs(listConcepts(oCode.Scale)));
      if oCode.Method <> nil Then
        FCode.SetMethods(oCode.Index, FRefs.AddRefs(listConcepts(oCode.Method)));
      if oCode.Class_ <> nil Then
        FCode.SetClasses(oCode.Index, FRefs.AddRefs(listConcepts(oCode.Class_)));
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

procedure TLoincImporter.ReadLanguageVariants;
var
  f : TFslFile;
  items : TFslStringList;
  csv : TFslCSVExtractor;
  lang : TLoincLanguage;
  i : integer;
begin
  items := TFslStringList.create;
  f := TFslFile.Create(IncludeTrailingPathDelimiter(folder)+ 'LinguisticVariants.csv', fmOpenRead);
  try
    csv := TFslCSVExtractor.Create(f.Link, TEncoding.UTF8);
    Try
      csv.ConsumeEntries(items);
      i := 0;
      while csv.More do
      begin
        items.Clear;
        csv.ConsumeEntries(items);
        if items.count > 0 then
        begin
          lang := TLoincLanguage.Create(items[1], items[2]);
          try
            readLanguage(i, items[0], lang);
            FLanguages.Add(lang.Link);
          finally
            lang.Free;
          end;
        end;
        inc(i);
      End;
    Finally
      csv.free;
    End;
  finally
    f.free;
    items.Free;
  end;
end;

Function TLoincImporter.ReadLOINCDatabase(out props : TLoincPropertyIds; out roots : TCardinalArray; out subsets : TLoincSubsets) : Cardinal;
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
  lang : byte;
  oLang : TLoincLanguage;
begin
  FStart := now;
  FLanguages := TFslList<TLoincLanguage>.create;
  FWordList := TStringList.Create;
  FStemList := TStringList.Create;
  FStemmer := TFslWordStemmer.create('english');
  oSvc := TLOINCServices.Create;
  Try
    Flanguages.add(TLoincLanguage.create('en', 'US'));
    FWordList.Sorted := true;
    FStemList.Sorted := true;
    Fdesc := oSvc.Desc;
    Fdesc.StartBuild;
    FDesc.AddEntry(0, '');
    FCode := oSvc.CodeList;
    FCode.StartBuild;
    FRefs := oSvc.Refs;
    FRefs.StartBuild;
    FLangs := oSvc.Lang;
    FLangs.StartBuild;
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

    for oLang in FLanguages do
      FLangs.AddEntry(olang.FLang, olang.FCountry);
    FLangs.DoneBuild;
    ls := '';
    for I := 0 to FAnswers.Count - 1 do
    begin
      FAnswers.GetEntry(i, code, desc, refs);
      s := FDesc.GetEntry(code, lang);
      if not((ls = '') or (AnsiCompareText(ls, s) < 0)) then
        raise ETerminologySetup.create('out of order');
      ls := s;
    end;

    Progress(15, 0, 'Save');
    oSvc.Save(FOutputFile, Date);

    Progress(15, 0.5, 'Cleanup');
  Finally
    oSvc.Free;
    FWordList.Free;
    For i := 0 to FStemList.Count - 1 do
      FStemList.Objects[i].Free;
    FStemList.Free;
    FStemmer.Free;
    FLanguages.Free;
  End;
  TLoincServices.Create.Load(FOutputFile);
End;

function TLoincImporter.listConcepts(arr: TConceptArray): TCardinalArray;
var
  i : integer;
begin
  setlength(result, length(arr));
  for i := 0 to length(arr) - 1 do
    if arr[i] = nil then
      result[i] := 0
    else
      result[i] := arr[i].Index;
end;

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
  if (sLeft <> '') and (sLeft[1] = '"') then
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
  s, AnswerListId, AnswerListName, AnswerStringID, AnswerCode, DisplayText : String;
  list : TAnswerList;
  answer : TAnswer;
begin
  inc(gc);
  CSVStringSplit(ln, ',', AnswerListId, ln);
  CSVStringSplit(ln, ',', AnswerListName, ln);
  CSVStringSplit(ln, ',', s, ln);
  CSVStringSplit(ln, ',', s, ln);
  CSVStringSplit(ln, ',', s, ln);
  CSVStringSplit(ln, ',', s, ln);
  CSVStringSplit(ln, ',', AnswerStringID, ln);
  CSVStringSplit(ln, ',', AnswerCode, ln);
  CSVStringSplit(ln, ',', s, ln);
  CSVStringSplit(ln, ',', s, ln);
  CSVStringSplit(ln, ',', DisplayText, ln);

  if isLoinc(AnswerListId) then
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
      // writeln('?')
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

procedure TLoincImporter.AddToDescendentConcepts(oHeirarchy : THeirarchyEntryList; entry : TCode; path : String);
var
  p : string;
  parent : THeirarchyEntry;
begin
  StringSplit(path, '.', p, path);
  parent := oHeirarchy.getByCode(p);
  if parent = nil then
    raise ETerminologySetup.create('Unable to find entry '+p);
  if (not parent.FDescendentConcepts.ExistsByReference(entry)) then
    parent.FDescendentConcepts.add(entry.link);
  if (path <> '') then
    AddToDescendentConcepts(oHeirarchy, entry, path);
end;

procedure TLoincImporter.registerParents(oHeirarchy : THeirarchyEntryList; entry : THeirarchyEntry; path : String);
var
  p : string;
  parent : THeirarchyEntry;
begin
  StringSplit(path, '.', p, path);
  parent := oHeirarchy.getByCode(p);
  if parent = nil then
    raise ETerminologySetup.create('Unable to find entry '+p);
  if (not parent.FChildren.ExistsByReference(entry)) then
    parent.FChildren.Add(entry.Link);
  if (not entry.FParents.ExistsByReference(parent)) then
  begin
    entry.FParents.add(parent.link);
  end;
  if path <> '' then
    registerParents(oHeirarchy, parent, path);
end;

procedure TLoincImporter.ProcessMultiAxialEntry(oHeirarchy : THeirarchyEntryList; oCodes : TCodeList; ln: string);
var
  PATH_TO_ROOT, SEQUENCE, IMMEDIATE_PARENT, CODE, CODE_TEXT, p : String;
  entry, parent : THeirarchyEntry;
  oCode : TCode;
  i : integer;
begin
  StringSplit(ln, ',', PATH_TO_ROOT, ln);
  StringSplit(ln, ',', SEQUENCE, ln);
  StringSplit(ln, ',', IMMEDIATE_PARENT, ln);
  StringSplit(ln, ',', CODE, CODE_TEXT);

  if (CODE.StartsWith('LP')) then
  begin
    entry := oHeirarchy.getByCode(CODE);
    if (entry = nil) then
    begin
      entry := THeirarchyEntry.Create;
      entry.FCode := CODE;
      entry.FText := CODE_TEXT;
      oHeirarchy.Add(entry);
      SeeDesc(entry.FText, entry, FLAG_LONG_COMMON);
    end;

    if PATH_TO_ROOT <> '' then
      registerParents(oHeirarchy, entry, PATH_TO_ROOT);

    if (IMMEDIATE_PARENT <> '') then
      registerParents(oHeirarchy, entry, IMMEDIATE_PARENT);
  end
  else
  begin
    oCode := oCodes.getByCode(CODE);
    IF (CODE = '10565-0') then
      code := '10565-0';
    if (oCode = nil) then
      raise ETerminologySetup.create('Unable to find code '+CODE);
    entry := oHeirarchy.getByCode(IMMEDIATE_PARENT);
    if (entry = nil) then
      raise ETerminologySetup.create('Unable to find ma code '+IMMEDIATE_PARENT);
    entry.FConcepts.Add(oCode.Link);
    AddToDescendentConcepts(oHeirarchy, oCode, PATH_TO_ROOT);
    oCode.entries.add(entry.link);
  end;
end;

procedure TLoincImporter.Progress(Step : integer; pct : real; msg : String);
begin
  if (assigned(callback)) then
  begin
    if msg = '' then
      msg := lastmessage;
    pct := ((step / 16) * 100) + (pct * (100 / 16));
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

procedure TLoincImporter.readLanguage(i : integer; index: String; lang: TLoincLanguage);
var
  f : TFslFile;
  items : TFslStringList;
  csv : TFslCSVExtractor;
  code : TLoincLanguageCodes;
  s, p : String;
  j : integer;
begin
  Progress(i, 0, 'Loading Language '+lang.Lang+'-'+lang.Country);
  items := TFslStringList.create;
  f := TFslFile.Create(IncludeTrailingPathDelimiter(folder)+ lang.Lang+lang.Country+index+'LinguisticVariant.csv', fmOpenRead);
  try
    csv := TFslCSVExtractor.Create(f.Link, TEncoding.UTF8, false, f.Size);
    Try
      csv.ConsumeEntries(items);
      j := 0;
      while csv.More do
      begin
        inc(j);
        if j mod 100 = 0 then
          Progress(i, f.Position / f.Size, 'Loading Language '+lang.Lang+'-'+lang.Country+' '+pct(f.Position, f.Size));
        items.Clear;
        csv.ConsumeEntries(items);
        if items.count > 0 then
        begin
          code := TLoincLanguageCodes.Create;
          try
            code.Component := items[1].trim();
            code.Prop := items[2].trim();
            code.TimeAspect := items[3].trim();
            code.System := items[4].trim();
            code.Scale := items[5].trim();
            code.Method  := items[6].trim();
            code.Clss := items[7].trim();
            code.Shortname := items[8].trim();
            code.LongName := items[9].trim();
            s := items[10];
            for p in s.Split([';']) do
              if code.RelatedNames.IndexOf(p.Trim) = -1 then
                code.RelatedNames.add(p.trim());
            lang.Codes.Add(items[0], code.Link);
          finally
            code.Free;
          end;
        end;
      End;
    Finally
      csv.free;
    End;
  finally
    f.free;
    items.Free;
  end;
end;

{ TConcept }

constructor TConcept.Create(lang : byte);
begin
  inherited Create;
  Codes := TObjectList.Create;
  Codes.OwnsObjects := False;
  FLang := lang;
end;

destructor TConcept.Destroy;
begin
  Codes.Free;
  inherited;
end;

{ TConceptManager }

function TConceptManager.See(lang: byte; sName: String; oCode: TObject): TConcept;
var
  i : Integer;
begin
  if sname = '' Then
    result := nil
  else
  begin
    i := IndexByName(char(lang+1)+sName);
    if existsByIndex(i) Then
      result := TConcept(ObjectByIndex[i])
    Else
    Begin
      result := TConcept.Create(lang);
      result.Name := char(lang+1)+sName;
      Add(result);
    End;
    result.Codes.Add(oCode);
  End;
end;

function TConceptManager.Store(langCount, lang: byte; sName : String; oImp : TLoincImporter): Cardinal;
var
  i, j : integer;
  aChildren : array of TCardinalArray;
  counter : array of integer;
  childLangList : TCardinalArray;
  aConcepts : TCardinalArray;
  oConcept : TConcept;
  byLang : boolean;
begin
  SetLength(aChildren, langCount);
  SetLength(counter, langCount);
  SetLength(childLangList, langCount);
  for i := 0 to langCount - 1 do
  begin
    counter[i] := 0;
    SetLength(aChildren[i], Count);
  end;

  For i := 0 to Count - 1 Do
  Begin
    oConcept := TConcept(ObjectByIndex[i]);
    SetLength(aConcepts, oConcept.Codes.Count);
    For j := 0 to oConcept.Codes.Count - 1 do
      aConcepts[j] := TCode(oConcept.Codes[j]).Index;
    oConcept.Index := oImp.FConcepts.AddConcept(oImp.AddDescription(oConcept.Flang, oConcept.Name.substring(1)), false, 0, oImp.FRefs.AddRefs(aConcepts));
    aChildren[oConcept.Flang, counter[oConcept.Flang]] := oConcept.Index;
    inc(counter[oConcept.Flang]);
  End;
  byLang := false;
  for i := 1 to langCount - 1 do
    if counter[i] > 0 then
      byLang := true;
  if byLang then
  begin
    for i := 0 to langCount - 1 do
    begin
      SetLength(aChildren[i], counter[i]);
      childLangList[i] := oImp.FRefs.AddRefs(aChildren[i]);
    end;
    result := oImp.FConcepts.AddConcept(oImp.AddDescription(lang, sName), true, oImp.FRefs.AddRefs(childLangList), 0);
  end
  else
    result := oImp.FConcepts.AddConcept(oImp.AddDescription(lang, sName), false, oImp.FRefs.AddRefs(aChildren[0]), 0);
end;

function TLoincImporter.AddDescription(lang : byte; s: String): Cardinal;
var
  i : Integer;
begin
  s := s.trim;
  if s = '' then
    result := 0
  else if FStrings.Find(char(lang+1)+s, i) Then
    result := Cardinal(FStrings.Objects[i])
  Else
  Begin
    result := FDesc.AddEntry(lang, s);
    FStrings.AddObject(char(lang+1)+s, TObject(result));
  End;
end;


{ THeirarchyEntry }

constructor THeirarchyEntry.Create;
begin
  inherited;
  FChildren := THeirarchyEntryList.create;
  FParents := THeirarchyEntryList.create;
  FConcepts := TCodeList.Create;
  FDescendentConcepts := TCodeList.Create;
end;

destructor THeirarchyEntry.Destroy;
begin
  FConcepts.Free;
  FChildren.free;
  FDescendentConcepts.free;
  FParents.Free;
  inherited;
end;

function THeirarchyEntry.link: THeirarchyEntry;
begin
  result := THeirarchyEntry(inherited link);
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

function THeirarchyEntryList.ItemClass: TFslObjectClass;
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

function TCodeList.ItemClass: TFslObjectClass;
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
  Stems := TFslIntegerList.Create;
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
  FAnswers := TFslList<TAnswer>.create();
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
  FParents := TFslList<TAnswerList>.create;
end;

destructor TAnswer.destroy;
begin
  FParents.Free;
  inherited;
end;

{ TLoincLanguageCodes }

constructor TLoincLanguageCodes.Create;
begin
  inherited;
  FRelatedNames := TStringList.create;
end;

destructor TLoincLanguageCodes.Destroy;
begin
  FRelatedNames.Free;
  inherited;
end;

function TLoincLanguageCodes.Display: String;
begin
  result := Trim(FLongName);
  if result = '' then
    result := Trim(FShortName);
  if result = '' then
    result := Trim(FComponent);
  if (result = '') and (FRelatedNames.Count > 0) then
    result := Trim(FRelatedNames[0]);
  if result = '' then
    result := '';
end;

function TLoincLanguageCodes.link: TLoincLanguageCodes;
begin
  result := TLoincLanguageCodes(inherited Link);
end;

{ TLoincLanguage }

constructor TLoincLanguage.Create;
begin
  inherited;
  FCodes := TFslMap<TLoincLanguageCodes>.create('loinc.lang');
end;

constructor TLoincLanguage.Create(lang, country: String);
begin
  Create;
  FLang := lang;
  FCountry := country;
end;

destructor TLoincLanguage.Destroy;
begin
  FCodes.Free;
  inherited;
end;

function TLoincLanguage.link: TLoincLanguage;
begin
  result := TLoincLanguage(inherited Link);
end;

constructor TCode.Create(langCount: integer);
begin
  inherited Create;
  entries := TFslList<THeirarchyEntry>.create;
  SetLength(Comps, langCount);
  SetLength(Props, langCount);
  SetLength(Time, langCount);
  SetLength(System, langCount);
  SetLength(Scale, langCount);
  SetLength(Method, langCount);
  SetLength(Class_, langCount);
end;

destructor TCode.Destroy;
begin
  entries.free;
  inherited Destroy;
end;

End.

