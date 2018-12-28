unit FHIR.Loinc.Services;

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
  SysUtils, Classes, Generics.Collections, IOUtils,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.Collections,
  RegularExpressions, YuStemmer,
  FHIR.Base.Objects, FHIR.Base.Common, FHIR.Base.Utilities, FHIR.Base.Factory,
  FHIR.CdsHooks.Utilities,
  FHIR.Tx.Service;

{axes

Component
Property
System
Scale
Method
ClassType/Class
Source


also:
  all names
  order/obs
  v2 & v3 data type
}
Const
  LOINC_CACHE_VERSION = '11';
  NO_PARENT = $FFFFFFFF;

  CL_OFFSET_Description = 0;
  CL_OFFSET_OtherNames = 4;
  CL_OFFSET_Component = 8;
  CL_OFFSET_Property = 12;
  CL_OFFSET_TimeAspect = 16;
  CL_OFFSET_System = 20;
  CL_OFFSET_Scale = 24;
  CL_OFFSET_Method = 28;
  CL_OFFSET_Class = 32;
  CL_OFFSET_Flags = 36;
  CL_OFFSET_Stems = 37;
  CL_OFFSET_Entry = 41;

Type
  TLangArray = array of byte;
  TLoincPropertyType = (lptComponents, lptProperties, lptTimeAspects, lptSystems, lptScales, lptMethods, lptClasses);

type
  // We store LOINC as five structures.
  //   the first structure is a simply a list of strings which are variable length names - referred to from the other structures
  //   the second structure is a list of lists of word or cardinal references.
  //   the third structure is a list of concepts. each concept has a ref0ernce to a name and a contained list of references which are either children
  //   the fourth structure is the code list - a list of loinc concepts, with codes and references to names and properties
  //   the fifth structure is the multi-axial heirarchy - parent, children, descendants, concepts, and descendent concepts

  // 0. language list
  TLoincLanguages = class (TFslObject)
  private
    FMaster : TBytes;
    FLength : Cardinal;
    FBuilder : TFslBytesBuilder;
  public

    procedure GetEntry(iIndex : byte; var lang, country : String);
    function count : integer;

    Procedure StartBuild;
    Function AddEntry(lang, country: String) : byte;
    Procedure DoneBuild;
  end;

  // 1. a list of strings
  //   each entry in the String starts with a byte length, and then the series of characters (2 bytes)
  // we store loinc descriptions, and other names in here
  TLoincStrings = class (TFslObject)
    Private
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TFslBytesBuilder;
    Public
      Function GetEntry(iIndex : Cardinal; var lang : byte):String;

      Procedure StartBuild;
      Function AddEntry(lang : byte; Const s : String) : Cardinal;
      Procedure DoneBuild;
  End;

  TWordArray = array of word;
  TCardinalArray = array of Cardinal;
  TMatch = record
    index : cardinal;
    iscode : boolean;
    code : String;
    Priority : Double;
  End;

  TMatchArray = Array of TMatch;

const
  FLAG_LONG_COMMON = 1;
  FLAG_LONG_RELATED = 2;
  CONCEPT_LENGTH = 13;

Type
  // word index. Every word is 5 bytes - a 4 byte index into the strings, and a 1 byte flag
  TLoincWords = class (TFslObject)
    Private
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TFslBytesBuilder;
    Public
      Procedure GetEntry(iIndex : Cardinal; var index : Cardinal; var flags : Byte);
      Function Count : Integer;
      Function GetString(iIndex : Cardinal) : Cardinal;

      Procedure StartBuild;
      Procedure AddWord(index : Cardinal; Flags : Byte);
      Procedure DoneBuild;
  End;

  // stem word index. Every word is 4 bytes - a 4 byte index into the strings
  TLoincStems = class (TFslObject)
    Private
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TFslBytesBuilder;
   Public
      Procedure GetEntry(iIndex : Cardinal; var index : Cardinal);
      Function Count : Integer;
      Function GetString(iIndex : Cardinal) : Cardinal;

      Procedure StartBuild;
      Procedure AddStem(index : Cardinal);
      Procedure DoneBuild;
  End;


  // 2. a list of list of references
  TLOINCReferences = class (TFslObject)
    Private
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TFslBytesBuilder;
    Public
      Function GetCardinals(iIndex : Cardinal) : TCardinalArray;
      Function Getlength(iIndex : Cardinal) : Cardinal;

      Procedure StartBuild;
      Function AddWords(Const a : TWordArray) : Cardinal;
      Function AddCardinals(Const a : TCardinalArray) : Cardinal;
      Procedure DoneBuild;
  End;

  // 3. a list of concepts
  TLOINCConcepts = class (TFslObject)
    Private
      FRefs:TLOINCReferences; // no own
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TFslBytesBuilder;
      function getForLang(langs : TLangArray; ref : cardinal) : cardinal;
    Public
      constructor Create(refs : TLOINCReferences);
      Procedure GetConcept(iIndex : Cardinal; langs : TLangArray; var iName : Cardinal; var iChildren : Cardinal; var iConcepts : Cardinal);

      Procedure StartBuild;
      Function AddConcept(iName : Cardinal; childByLang : boolean; iChildren : Cardinal; iConcepts : Cardinal) : Cardinal;
      Procedure DoneBuild;

  End;

  // 4. the master code reference.
  // it is a list of codes and the offset reference to their description
  // codes are stored in alphabetical order so you can do a binary serach for Code identity
  // This is the effective logical structure of the bytes:
//  TLOINCCode = {private} packed record
//    Code : Char[FCodeLength]; padded with spaces (2 bytes per char)
//    Description : Cardinal;
//    other names : cardinal
//    component : word
//    Property : word
//    TimeAspect : word
//    System : word
//    Scale : word
//    Method : word
//    Class : word
//    flags : byte
//    stems : Cardinal
//  End;
Const
  FLAGS_ROOT = $01;
  FLAGS_UNITS = $02;
  FLAGS_ORDER = $04;
  FLAGS_OBS = $08;
  FLAGS_HOLD = $10;
  FLAGS_CLIN = $20;
  FLAGS_ATT = $40;
  FLAGS_SURV = $80;

Type
  TLOINCCodeList = class (TFslObject)
    Private
      FRefs:TLOINCReferences; // no own
      FCodeLength, FReclength : Cardinal;
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TFslBytesBuilder;
      procedure SetCodeLength(const Value: Cardinal);
      function getForLang(langs : TLangArray;  ref : cardinal) : cardinal;
    Public
      constructor Create(refs : TLOINCReferences);
      destructor Destroy; override;

      Function FindCode(sCode : String; var iIndex : Cardinal) : Boolean;

      Procedure GetInformation(iIndex: Cardinal; langs : TLangArray; var sCode : String; var iDescription, iOtherNames, iEntry, iStems : Cardinal; var iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : cardinal; var iFlags : Byte);

      // we presume that the Codes are registered in order
      Procedure StartBuild;
      Function AddCode(sCode : String; iDescriptions, iOtherNames, iEntry : Cardinal; iFlags : Byte) : Cardinal;
      Procedure DoneBuild;

      // these need to be called after Done Build
      Procedure SetComponents(iIndex : Cardinal; iValue : cardinal);
      Procedure SetPropertys(iIndex : Cardinal; iValue : cardinal);
      Procedure SetTimeAspects(iIndex : Cardinal; iValue : cardinal);
      Procedure SetSystems(iIndex : Cardinal; iValue : cardinal);
      Procedure SetScales(iIndex : Cardinal; iValue : cardinal);
      Procedure SetMethods(iIndex : Cardinal; iValue : cardinal);
      Procedure SetClasses(iIndex : Cardinal; iValue : cardinal);
      Procedure SetStems(iIndex : Cardinal; iValue : Cardinal);

      Function Count : Cardinal;
      Property CodeLength : Cardinal read FCodeLength Write SetCodeLength;
  End;

  // 5. the multi-axial heirachy
  // it is a list of entries
  // This is the effective logical structure of the bytes:
//  TLOINCEntry = {private} packed record
//    code : Cardinal;
//    text : Cardinal;
//    children : Cardinal;
//    descendants : Cardinal;
//    concepts : Cardinal;
//    descendentconcepts : Cardinal;
//  End;

  TLOINCHeirarchyEntryList = class (TFslObject)
  Private
    FMaster : TBytes;
    FBuilder : TFslBytesBuilder;
  Public
    Function FindCode(sCode : String; var iIndex : Cardinal; Strings : TLoincStrings) : Boolean;
    Procedure GetEntry(iIndex: Cardinal; var code, text, parent, children, descendants, concepts, descendentConcepts, stems : Cardinal);

    // we presume that the Codes are registered in order
    Procedure StartBuild;
    Function AddEntry(code, text, parent, children, descendants, concepts, descendentConcepts : Cardinal) : Cardinal;
    Procedure DoneBuild;

    // this needs to be called after Done Build
    Procedure SetStems(iIndex : Cardinal; iValue : Cardinal);

    Function Count : Integer;
  End;

  TLOINCAnswersList = class (TFslObject)
  Private
    FMaster : TBytes;
    FBuilder : TFslBytesBuilder;
  Public
    Function FindCode(sCode : String; var iIndex : Cardinal; Strings : TLoincStrings) : Boolean;
    Procedure GetEntry(iIndex: Cardinal; var code, description, answers : Cardinal);

    // we presume that the Codes are registered in order
    Procedure StartBuild;
    Function AddEntry(code, description, answers : Cardinal) : Cardinal;
    Procedure DoneBuild;

    Function Count : cardinal;
  End;


  TLoincSubsetId = (lsiNull, lsiAll, lsiOrder, lsiObs, lsiOrderObs, lsiOrderSubset, lsiTypeObservation, lsiTypeClinical, lsiTypeAttachment,
    lsiTypeSurvey, lsiInternal, lsi3rdParty, lsiActive, lsiDeprecated, lsiDiscouraged, lsiTrial);
  TLoincSubsets = Array [TLoincSubsetId] of Cardinal;
  TLoincPropertyIds = Array [TLoincPropertyType] of Cardinal;

  THolder = class (TCodeSystemProviderFilterContext)
  private
    ndx: integer;
    Children : FHIR.Loinc.Services.TCardinalArray;
    function HasChild(v : integer) : boolean;
  end;

  TLOINCServices = class (TCodeSystemProvider)
  Private
    FLang : TLoincLanguages;
    FDesc : TLoincStrings;
    FCode : TLOINCCodeList;
    FRefs : TLOINCReferences;
    FConcepts : TLOINCConcepts;
    FProperties : TLoincPropertyIds;
    FSubsets : TLoincSubsets;
    FWords : TLoincWords;
    FStems : TLoincStems;
    FEntries : TLOINCHeirarchyEntryList;
    FAnswerLists : TLOINCAnswersList;
    FHeirarchyRoots : TCardinalArray;

    FRoot : Cardinal;
    FLoaded: Boolean;
    FVersion: String;
    FKey: integer;
    function FindStem(s: String; var index: Integer): Boolean;
    function FilterByPropertyId(prop : TLoincPropertyType; op: TFhirFilterOperator; value: String): TCodeSystemProviderFilterContext;
    function FilterBySubset(op: TFhirFilterOperator; subset : TLoincSubsetId): TCodeSystemProviderFilterContext;
    function FilterByHeirarchy(op: TFhirFilterOperator; value: String; transitive: boolean): TCodeSystemProviderFilterContext;
    function FilterByIsA(value: String; this: boolean): TCodeSystemProviderFilterContext;
    function GetConceptDesc(iConcept : cardinal; langs : TLangArray):String;
    function useLang(lang : byte; langs : TLangArray; incLast : boolean) : boolean;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TLOINCServices; Overload;

    Procedure Load(Const sFilename : String);
    Procedure Save(Const sFilename : String; statedDate : String);
    function langsForLang(lang : String): TLangArray;
    function supportsLang(lang : String): boolean;

    Function GetDisplayByName(Const sCode : String; langs : TLangArray) : String;
    procedure GetDisplaysByName(Const sCode : String; langs : TLangArray; list : TStringList);
    Function Search(sText : String; all: boolean) : TMatchArray; overload;
    Function GetPropertyId(aType : TLoincPropertyType; langs : TLangArray; const sName : String) : cardinal;
    Function GetPropertyCodes(iProp : cardinal) : TCardinalArray;
    Function GetConceptName(iConcept : cardinal): String;
    Function IsCode(sCode : String): Boolean;
    Function IsMACode(sCode : String): Boolean;

    Property Lang : TLoincLanguages read FLang;
    Property Desc : TLoincStrings read FDesc;
    Property Refs : TLOINCReferences read FRefs;
    Property Concepts : TLOINCConcepts read FConcepts;
    Property CodeList : TLOINCCodeList read FCode;
    Property Words : TLoincWords read FWords;
    Property Stems : TLoincStems read FStems;
    Property Entries : TLOINCHeirarchyEntryList read FEntries;
    Property AnswerLists : TLOINCAnswersList read FAnswerLists;


    Property Root : Cardinal read FRoot write FRoot;
    Property Loaded : Boolean read FLoaded write FLoaded;
    Property LOINCVersion : String read FVersion write FVersion;
    Property Properties : TLoincPropertyIds read FProperties Write FProperties;
    Property HeirarchyRoots : TCardinalArray read FHeirarchyRoots write FHeirarchyRoots;
    Property Key : integer read FKey write FKey;
    Property Subsets : TLoincSubsets read FSubsets Write FSubsets;

    function TotalCount : integer; override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function findMAConcept(code : String) : Cardinal;
    function system(context : TCodeSystemProviderContext) : String; override;
    function version(context : TCodeSystemProviderContext) : String; override;
    function name(context : TCodeSystemProviderContext) : String; override;
    function getDisplay(code : String; lang : String):String; override;
    function locate(code : String; var message : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext; lang : String) : string; override;
    procedure Displays(code : String; list : TStringList; lang : String); override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList; lang : String); override;
    function filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String; var message : String) : TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; overload; override;
    function buildValueSet(factory : TFHIRFactory; id : String) : TFhirValueSetW;
    function getDefinition(code : String):String; override;
    function Definition(context : TCodeSystemProviderContext) : string; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure getCDSInfo(card : TCDSHookCard; lang, baseURL, code, display : String); override;
    procedure extendLookup(factory : TFHIRFactory; ctxt : TCodeSystemProviderContext; lang : String; props : TArray<String>; resp : TFHIRLookupOpResponseW); override;
    //function subsumes(codeA, codeB : String) : String; override;

  End;

  TLOINCServiceList = class (TFslObjectList)
  Private
    FDefinition: TLOINCServices;
    function GetService(i: integer): TLOINCServices;
    procedure SetDefinition(const Value: TLOINCServices);
    function GetDefinition: TLOINCServices;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    destructor Destroy; Override;

    Function GetByKey(sKey : String) : TLOINCServices;
    Function GetServiceByName(sName : String) : TLOINCServices;

    Function HasDefaultService : Boolean;

    Property DefaultService : TLOINCServices Read GetDefinition write SetDefinition;

    Property Service[i : integer] : TLOINCServices read GetService; Default;
  End;

function ascopy(s : TBytes; start, len : integer) : String;
function utfcopy(s : TBytes; start, length : integer) : String;
function nolang : TLangArray;

Implementation

{ TLoincStrings }

function TLoincStrings.GetEntry(iIndex: Cardinal; var lang : byte): String;
begin
  if iIndex = 0 Then
  begin
    result := '';
    exit;
  end;

  if (iIndex > FLength) then
    Raise ETerminologyError.Create('Wrong length index getting LOINC name');
  SetLength(Result, Word(FMaster[iIndex]));
  if (iIndex + 3 + clength(result) > FLength) then
    raise ETerminologyError.create('Wrong length index getting LOINC name (2)');
  if Length(result) > 0 Then
  begin
    Move(FMaster[iIndex+2], lang, 1);
    Move(FMaster[iIndex+3], result[1], Length(Result)*2);
  end;
end;

function TLoincStrings.AddEntry(lang : byte; const s: String): Cardinal;
begin
  if Length(s) > 2000 Then
    raise ETerminologyError.create('LOINC Description too long: '+s);
  result := FBuilder.Length;
  FBuilder.AddWord(Length(s));
  if (length(s) > 0) then
  begin
    FBuilder.Append(lang);
    if (s[1] < ' ') and DebugConsoleMessages then
      writeln('error');
    FBuilder.AddString(s);
  end;
end;

procedure TLoincStrings.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TLoincStrings.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

{ TLOINCReferences }

{Function TLOINCReferences.GetWords(iIndex: Cardinal) : TWordArray;
var
  i : integer;
  lw : Cardinal;
begin
  if (iIndex > FLength) then
    raise ETerminologyError.create('Wrong length index getting LOINC list');
  if Byte(FMaster[iIndex]) <> 0 Then
    raise ETerminologyError.create('not a word list');
  inc(iIndex);
  move(FMaster[iIndex], lw, 4);
  SetLength(Result, lw);
  inc(iIndex, 4);
  for i := 0 to Length(result)-1 Do
  Begin
    move(FMaster[iIndex], result[i], 2);
    inc(iIndex, 2);
  End;
end;
}
Function TLOINCReferences.GetCardinals(iIndex: Cardinal) : TCardinalArray;
var
  i : integer;
  lw : cardinal;
begin
  if (iIndex > FLength) then
    raise ETerminologyError.create('Wrong length index getting LOINC list');
  if Byte(FMaster[iIndex]) <> 1 Then
    raise ETerminologyError.create('not a cardinal list');
  inc(iIndex);
  move(FMaster[iIndex], lw, 4);
  SetLength(Result, lw);
  inc(iIndex, 4);
  for i := 0 to Length(result)-1 Do
  Begin
    move(FMaster[iIndex], result[i], 4);
    inc(iIndex, 4);
  End;
end;

Function TLOINCReferences.AddWords(Const a : TWordArray) : Cardinal;
var
  iLoop : Integer;
Begin
  if Length(a) > 65535 Then
    raise ETerminologyError.create('LOINC reference list too long ('+inttostr(length(a))+')');
  result := FBuilder.Length;
  FBuilder.Append(Byte(0));// for words
  FBuilder.AddCardinal(length(a));
  for iLoop := Low(a) to High(a) Do
    FBuilder.AddWord(a[iLoop]);
End;

Function TLOINCReferences.AddCardinals(Const a : TCardinalArray) : Cardinal;
var
  iLoop : Integer;
Begin
  result := FBuilder.Length;
  FBuilder.Append(Byte(1));// for Cardinals
  FBuilder.AddCardinal(length(a));
  for iLoop := Low(a) to High(a) Do
    FBuilder.AddCardinal(a[iLoop]);
End;

procedure TLOINCReferences.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TLOINCReferences.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

function TLOINCReferences.Getlength(iIndex: Cardinal): Cardinal;
begin
  if (iIndex > FLength) then
    raise ETerminologyError.create('Wrong length index getting LOINC list');
  inc(iIndex); // skip type marker
  move(FMaster[iIndex], result, 4);
end;

{ TLOINCConcepts }

Procedure TLOINCConcepts.GetConcept(iIndex : Cardinal; langs : TLangArray; var iName : Cardinal; var iChildren : Cardinal; var iConcepts : Cardinal);
var
  b : byte;
begin
  if (iIndex >= FLength) then
    raise ETerminologyError.create('Wrong length index getting LOINC name');
  Move(FMaster[iIndex*CONCEPT_LENGTH+0], iName, 4);
  Move(FMaster[iIndex*CONCEPT_LENGTH+4], b, 1);
  Move(FMaster[iIndex*CONCEPT_LENGTH+5], iChildren, 4);
  Move(FMaster[iIndex*CONCEPT_LENGTH+9], iConcepts, 4);
  if b <> 0 then
    iChildren := getForLang(langs, iChildren);
end;

function TLOINCConcepts.getForLang(langs: TLangArray; ref: cardinal): cardinal;
var
  dlist : TCardinalArray;
  l : byte;
begin
  dlist := FRefs.GetCardinals(ref);
  result := dlist[0];
  for l in langs do
  begin
    result := dlist[l];
    if result <> 0 then
      exit;
  end;
end;

function TLOINCConcepts.AddConcept(iName : Cardinal; childByLang : boolean; iChildren : Cardinal; iConcepts : Cardinal) : Cardinal;
begin
  result := FBuilder.Length div CONCEPT_LENGTH;
  FBuilder.AddCardinal(iName);
  if (childByLang) then
    FBuilder.Append(1)
  else
    FBuilder.Append(0);
  FBuilder.AddCardinal(iChildren);
  FBuilder.AddCardinal(iConcepts);
end;

constructor TLOINCConcepts.Create(refs: TLOINCReferences);
begin
  Inherited Create;
  FRefs := refs;
end;

procedure TLOINCConcepts.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster) div 12;
  FBuilder.Free;
end;

procedure TLOINCConcepts.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;


{ TLOINCCodeList }

procedure TLOINCCodeList.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;


Function TLOINCCodeList.AddCode(sCode : String; iDescriptions, iOtherNames, iEntry : Cardinal; iFlags : Byte) : Cardinal;
var
  s : AnsiString;
  i : integer;
begin
  Result := FBuilder.Length div FReclength;
  SetLength(s, FCodeLength);
  FillChar(s[1], FCodeLength, 32);
  for I := 1 to length(sCode) do
    s[i] := AnsiChar(sCode[i]);
  FBuilder.AddAnsiString(s);
  FBuilder.AddCardinal(iDescriptions);
  FBuilder.AddCardinal(iOtherNames);
  FBuilder.AddCardinal(0); // Component
  FBuilder.AddCardinal(0); // Property
  FBuilder.AddCardinal(0); // TimeAspect
  FBuilder.AddCardinal(0); // System
  FBuilder.AddCardinal(0); // Scale
  FBuilder.AddCardinal(0); // Method
  FBuilder.AddCardinal(0); // Class
  FBuilder.Append(iFlags);
  FBuilder.AddCardinal(0); // stems
  FBuilder.AddCardinal(iEntry);
end;

procedure TLOINCCodeList.SetCodeLength(const Value: Cardinal);
begin
  FCodeLength := Value;
  FRecLength := FCodeLength+CL_OFFSET_Entry+4;
end;

Procedure TLOINCCodeList.SetComponents(iIndex : Cardinal; iValue : cardinal);
Begin
  Move(iValue, FMaster[iIndex*(FRecLength) +FCodeLength+CL_OFFSET_Component], 4);
End;

Procedure TLOINCCodeList.SetPropertys(iIndex : Cardinal; iValue : cardinal);
Begin
  Move(iValue, FMaster[iIndex*FRecLength+FCodeLength+CL_OFFSET_Property], 4);
End;

Procedure TLOINCCodeList.SetTimeAspects(iIndex : Cardinal; iValue : cardinal);
Begin
  Move(iValue, FMaster[iIndex*FRecLength+FCodeLength+CL_OFFSET_TimeAspect], 4);
End;

Procedure TLOINCCodeList.SetSystems(iIndex : Cardinal; iValue : cardinal);
Begin
  Move(iValue, FMaster[iIndex*FRecLength+FCodeLength+CL_OFFSET_System], 4);
End;

Procedure TLOINCCodeList.SetScales(iIndex : Cardinal; iValue : cardinal);
Begin
  Move(iValue, FMaster[iIndex*FRecLength+FCodeLength+CL_OFFSET_Scale], 4);
End;

Procedure TLOINCCodeList.SetMethods(iIndex : Cardinal; iValue : cardinal);
Begin
  Move(iValue, FMaster[iIndex*FRecLength+FCodeLength+CL_OFFSET_Method], 4);
End;

Procedure TLOINCCodeList.SetClasses(iIndex : Cardinal; iValue : cardinal);
Begin
  Move(iValue, FMaster[iIndex*FRecLength+FCodeLength+CL_OFFSET_Class], 4);
End;


destructor TLOINCCodeList.Destroy;
begin
  inherited;
end;

procedure TLOINCCodeList.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FBuilder.Free;
end;

{$Q-}

function TLOINCCodeList.FindCode(sCode : String; var IIndex: Cardinal): boolean;
var
  L, H, I : Cardinal;
  C: Integer;
  s, sF : String;
begin
  if length(FMaster) = 0 Then
    Result := False
  Else
  Begin
    s := StringPadRight(sCode, ' ', FCodeLength);
    Result := False;
    L := 0;
    H := (FLength div FRecLength) - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      sF := asCopy(FMaster, i*FReclength, FCodeLength);
      C := CompareStr(sF, s);
      if C < 0 then L := I + 1 else
      begin
        H := I - 1;
        if C = 0 then
        begin
          Result := True;
          L := I;
        end;
      end;
    end;
    iIndex := L;
  End;
end;


function TLOINCCodeList.getForLang(langs: TLangArray; ref: cardinal): cardinal;
var
  dlist : TCardinalArray;
  l : byte;
begin
  dlist := FRefs.GetCardinals(ref);
  result := dlist[0];
  for l in langs do
  begin
    result := dlist[l];
    if result <> 0 then
      exit;
  end;
end;

Procedure TLOINCCodeList.GetInformation(iIndex: Cardinal; langs : TLangArray; var sCode : String; var iDescription, iOtherNames, iEntry, iStems : Cardinal; var iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Cardinal; var iFlags : Byte);
Begin
  if iIndex > FLength div FRecLength - 1 Then
    raise ETerminologyError.create('Attempt to access invalid LOINC index at '+inttostr(iIndex*FRecLength));
  sCode := trim(asCopy(FMaster, iIndex*FRecLength, FCodeLength));
{0}  Move(FMaster[(iIndex*FRecLength)+FCodeLength+CL_OFFSET_Description], iDescription, 4);
{4}  Move(FMaster[(iIndex*FRecLength)+FCodeLength+CL_OFFSET_OtherNames], iOtherNames, 4);
{8}  Move(FMaster[(iIndex*FRecLength)+FCodeLength+CL_OFFSET_Component], iComponent, 4);
{12}  Move(FMaster[(iIndex*FRecLength)+FCodeLength+CL_OFFSET_Property], iProperty, 4);
{16}  Move(FMaster[(iIndex*FRecLength)+FCodeLength+CL_OFFSET_TimeAspect], iTimeAspect, 4);
{20}  Move(FMaster[(iIndex*FRecLength)+FCodeLength+CL_OFFSET_System], iSystem, 4);
{24}  Move(FMaster[(iIndex*FRecLength)+FCodeLength+CL_OFFSET_Scale], iScale, 4);
{28}  Move(FMaster[(iIndex*FRecLength)+FCodeLength+CL_OFFSET_Method], iMethod, 4);
{32}  Move(FMaster[(iIndex*FRecLength)+FCodeLength+CL_OFFSET_Class], iClass, 4);
{36}  Move(FMaster[(iIndex*FRecLength)+FCodeLength+CL_OFFSET_Flags], iFlags, 1);
{37}  Move(FMaster[(iIndex*FRecLength)+FCodeLength+CL_OFFSET_Stems], iStems, 4);
{41}  Move(FMaster[(iIndex*FRecLength)+FCodeLength+CL_OFFSET_Entry], iEntry, 4);

  iDescription := getForLang(langs, iDescription);
  iComponent := getForLang(langs, iComponent);
  iProperty := getForLang(langs, iProperty);
  iTimeAspect := getForLang(langs, iTimeAspect);
  iSystem := getForLang(langs, iSystem);
  iScale := getForLang(langs, iScale);
  iMethod := getForLang(langs, iMethod);
  iClass := getForLang(langs, iClass);
end;

function TLOINCCodeList.Count: Cardinal;
begin
  result := FLength div FRecLength;
end;

constructor TLOINCCodeList.Create(refs: TLOINCReferences);
begin
  Inherited Create;
  FRefs := refs;
end;

procedure TLOINCCodeList.SetStems(iIndex, iValue: Cardinal);
var
  offset : cardinal;
begin
  offset := (iIndex*FRecLength)+FCodeLength+CL_OFFSET_Stems;
  if (offset + 4 > length(FMaster)) then
    raise ETerminologyError.create('Write off end');
  Move(iValue, FMaster[offset], 4);
end;

{ TLOINCServices }

constructor TLOINCServices.Create;
begin
  inherited;
  FLang := TLoincLanguages.Create;
  FDesc := TLoincStrings.Create;
  FRefs := TLOINCReferences.Create;
  FCode := TLOINCCodeList.Create(FRefs);
  FConcepts := TLOINCConcepts.Create(FRefs);
  FWords := TLoincWords.Create;
  FStems := TLoincStems.Create;
  FEntries := TLOINCHeirarchyEntryList.create;
  FAnswerLists := TLOINCAnswersList.create;
end;

function TLOINCServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

destructor TLOINCServices.Destroy;
begin
  FAnswerLists.Free;
  FEntries.Free;
  FWords.Free;
  FStems.Free;
  FRefs.Free;
  FConcepts.Free;
  FDesc.Free;
  FLang.Free;
  FCode.Free;
  inherited;
end;

function TLOINCServices.GetConceptName(iConcept: cardinal): String;
var
  iName : Cardinal;
  iChildren : Cardinal;
  iCodes : Cardinal;
  lang : byte;
begin
  Concepts.GetConcept(iConcept, nil, iName, iChildren, iCodes);
  result := Desc.GetEntry(iName, lang);
end;

procedure TLOINCServices.GetDisplaysByName(Const sCode : String; langs : TLangArray; list : TStringList);
var
  iIndex : Cardinal;
  iDescription, iStems, iOtherNames : Cardinal;
  iEntry : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iCode, iAnswers : cardinal;
  iFlags : Byte;
  names : TCardinalArray;
  name : Cardinal;
  ilang, l : byte;
  s : String;
begin
  if CodeList.FindCode(sCode, iIndex) then
  Begin
    CodeList.GetInformation(iIndex, langs, sCode1, iDescription, iOtherNames, iEntry, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);
    assert(sCode = sCode1);
    list.Add(Desc.GetEntry(iDescription, ilang));
    if iOtherNames <> 0 then
    begin
      names := FRefs.GetCardinals(iOtherNames);
      for name in names do
      begin
        s := Desc.GetEntry(name, ilang);
        for l in langs do
          if l = ilang then
            list.Add(s);
      end;
    end;
  End
  else if AnswerLists.FindCode(sCode, iIndex, FDesc) then
  begin
    AnswerLists.GetEntry(iIndex, iCode, iDescription, iAnswers);
    list.Add(Desc.GetEntry(iDescription, ilang));
  end;
end;


function TLOINCServices.GetDisplayByName(const sCode: String; langs : TLangArray): String;
var
  iIndex : Cardinal;
  iDescription, iStems, iOtherNames : Cardinal;
  iEntry : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Cardinal;
  iFlags : Byte;
  ilang : byte;
begin
  if CodeList.FindCode(sCode, iIndex) then
  Begin
    CodeList.GetInformation(iIndex, langs, sCode1, iDescription, iOtherNames, iEntry, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);
    assert(sCode = sCode1);
    result := Desc.GetEntry(iDescription, ilang);
  End
  Else
    result := '';
end;

function TLOINCServices.GetPropertyCodes(iProp: cardinal): TCardinalArray;
var
  iName : Cardinal;
  iChildren : Cardinal;
  iCodes : Cardinal;
begin
  Concepts.GetConcept(iProp, nil, iName, iChildren, iCodes);
  result := Refs.GetCardinals(iCodes);
end;

function TLOINCServices.GetPropertyId(aType: TLoincPropertyType; langs : TLangArray; const sName: String): cardinal;
var
  iName : Cardinal;
  iChildren : Cardinal;
  iCodes : Cardinal;
  aChildren : TCardinalArray;
  i : Integer;
  iDummy : Cardinal;
  lang : byte;

begin
  // get all the children of the property code
  // iterate them looking for a name match
  Concepts.GetConcept(Properties[aType], langs, iName, iChildren, iCodes);
  aChildren := Refs.GetCardinals(iChildren);
  result := 0;
  For i := 0 to High(aChildren) Do
  Begin
    Concepts.GetConcept(aChildren[i], langs, iName, iChildren, iDummy);
    if SameText(sName, Desc.GetEntry(iName, lang)) Then
    Begin
      result := aChildren[i];
      exit;
    End;
  End;
end;


function TLOINCServices.IsCode(sCode: String): Boolean;
var
  iIndex : Cardinal;
begin
  result := CodeList.FindCode(sCode, iIndex);
end;

function TLOINCServices.IsMACode(sCode: String): Boolean;
var
  iIndex : Cardinal;
begin
  result := Entries.FindCode(sCode, iIndex, FDesc);
end;

function TLOINCServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := false;
end;

function TLOINCServices.langsForLang(lang: String): TLangArray;
  procedure add(b : byte);
  begin
    SetLength(result, length(result)+1);
    result[length(result)-1] := b;
  end;
var
  i : integer;
  llang, country : string;
begin
  if lang = '' then
    result := noLang
  else
  begin
    SetLength(result, 0);

    // first pass, exact matches
    for i := 0 to FLang.count - 1 do
    begin
      FLang.GetEntry(i, llang, country);
      if (llang +'-'+country = lang) then
        add(i);
    end;
    if length(result) = 0 then
    begin
      // look for preferred match where country = lang
      for i := 0 to FLang.count - 1 do
      begin
        FLang.GetEntry(i, llang, country);
        if (llang +'-'+llang.ToUpper = lang) then
          add(i);
      end;
      // other possible matches
      for i := 0 to FLang.count - 1 do
      begin
        FLang.GetEntry(i, llang, country);
        if lang = llang then
          add(i);
      end;
    end;

    // always fall back to english
    add(0);
  end;
end;

function TLOINCServices.Link: TLOINCServices;
begin
  result := TLOINCServices(Inherited Link);
end;

procedure TLOINCServices.Load(const sFilename: String);
var
  oFile : Tfilestream;
  oread : TReader;
  aLoop : TLoincPropertyType;
  a : TLoincSubsetId;
  i, t : integer;
  function readBytes : TBytes;
  begin
    SetLength(result, oRead.ReadInteger);
    if length(result) > 0 then
      oread.Read(result[0], length(result));
  end;
begin
  oFile := TFileStream.Create(sFilename, fmOpenread);
  try
    oread := TReader.Create(oFile, 8192);
    try
      if oRead.ReadString <> LOINC_CACHE_VERSION Then
        raise ETerminologyError.create('the LOINC cache must be rebuilt using the ''Import LOINC'' operation in the manager application.');
      FCode.CodeLength := oRead.ReadInteger;
      FLang.FMaster := ReadBytes;
      FLang.FLength := Length(FLang.FMaster);
      FCode.FMaster := ReadBytes;
      FCode.FLength := Length(FCode.FMaster);
      FDesc.FMaster := ReadBytes;
      FDesc.FLength := Length(FDesc.FMaster);
      FRefs.FMaster := ReadBytes;
      FRefs.FLength := Length(FRefs.FMaster);
      FConcepts.FMaster := ReadBytes;
      FConcepts.FLength := Length(FConcepts.FMaster) div 12;
      FWords.FMaster := ReadBytes;
      FWords.FLength := Length(FWords.FMaster);
      FStems.FMaster := ReadBytes;
      FStems.FLength := Length(FStems.FMaster);
      FEntries.FMaster := ReadBytes;
      FAnswerLists.FMaster := ReadBytes;
      FRoot := oRead.ReadInteger;
      FVersion := oRead.ReadString;
      For aLoop := Low(TLoincPropertyType) To High(TLoincPropertyType) Do
        FProperties[aLoop] := oRead.ReadInteger;
      t := oread.ReadInteger;
      SetLength(FHeirarchyRoots, t);
      for i := 0 to t - 1 do
        FHeirarchyRoots[i] := oread.ReadInteger;
      For a := Low(TLoincSubsetId) To High(TLoincSubsetId) Do
        FSubsets[a] := oRead.ReadInteger;
    Finally
      oread.Free;
    End;
  Finally
    oFile.Free;
  End;
  Loaded := true;
end;

procedure TLOINCServices.Save(const sFilename: String; statedDate : String);
var
  oFile : Tfilestream;
  oWrite : TWriter;
  aLoop : TLoincPropertyType;
  a : TLoincSubsetId;
  i : integer;
  v : String;
  zip : TFslZipWriter;
  procedure WriteBytes(b : TBytes);
  begin
   oWrite.WriteInteger(length(b));
   if length(b) > 0 then
     oWrite.Write(b[0], length(b));
  end;
begin
  if FileExists(sFilename) Then
  begin
    FileSetReadOnly(sFilename, False);
    DeleteFile(sFilename);
  End;
  oFile := TFileStream.Create(sFilename, fmCreate);
  try
    oWrite := TWriter.Create(oFile, 8192);
    try
      oWrite.WriteString(LOINC_CACHE_VERSION);
      oWrite.WriteInteger(FCode.CodeLength);
      WriteBytes(FLang.FMaster);
      WriteBytes(FCode.FMaster);
      WriteBytes(FDesc.FMaster);
      WriteBytes(FRefs.FMaster);
      WriteBytes(FConcepts.FMaster);
      WriteBytes(FWords.FMaster);
      WriteBytes(FStems.FMaster);
      WriteBytes(FEntries.FMaster);
      WriteBytes(FAnswerLists.FMaster);
      oWrite.writeInteger(FRoot);
      oWrite.WriteString(FVersion);
      For aLoop := Low(TLoincPropertyType) To High(TLoincPropertyType) Do
        oWrite.WriteInteger(FProperties[aLoop]);
      oWrite.WriteInteger(Length(FHeirarchyRoots));
      for i := 0 to Length(FHeirarchyRoots) - 1 do
        oWrite.WriteInteger(FHeirarchyRoots[i]);
      For a := Low(TLoincSubsetId) To High(TLoincSubsetId) Do
        oWrite.WriteInteger(FSubsets[a]);
    Finally
      oWrite.Free;
    End;
  Finally
    oFile.Free;
  End;
  // set up website upload
  if FileExists('C:\work\com.healthintersections.fhir\website\FhirServer\loinc.inc') then
  begin
    v := FVersion.Replace('.', '');
    StringToFile('    <td>'+FVersion+'</td>'+#13#10+
                 '    <td>'+statedDate+'</td>'+#13#10+
                 '    <td><a href="loinc_'+v+'.zip"><img src="zip.gif"/> Zip</a></td>'+#13#10, path([ExtractFileDir(sFilename), 'loinc.inc']), TEncoding.ASCII);
    zip := TFslZipWriter.Create;
    try
      zip.Stream := TFslFile.create(path(['C:\work\com.healthintersections.fhir\website\FhirServer', 'loinc_'+v+'.zip']), fmCreate);
      zip.addFile('loinc_'+v+'.cache', sFilename);
      zip.WriteZip;
    finally
      zip.Free;
    end;
  end;
end;

function TLOINCServices.SearchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean): TCodeSystemProviderFilterContext;
var
  matches : TMatchArray;
  children : TCardinalArray;
  i : integer;
begin
  matches := Search(filter.filter, true);
  setLength(children, length(matches));
  for i := 0 to Length(matches) - 1 do
    children[i] := matches[i].index;
  result := THolder.Create;
  THolder(result).Children := children;
end;

function TLOINCServices.supportsLang(lang: String): boolean;
var
  i : integer;
  llang, country : string;
begin
  if lang = '' then
    result := false
  else
  begin
    for i := 0 to FLang.count - 1 do
    begin
      FLang.GetEntry(i, llang, country);
      if (llang = lang) then
        exit(true);
      if (llang +'-'+country = lang) then
        exit(true);
    end;
    result := false;
  end;
end;

type
  TSearchWord = record
    original : String;
    stem : Cardinal;
  End;
  TSearchWordArray = array of TSearchWord;

Procedure QuickSortArray(var a : TMatchArray);

  Procedure QuickSort(L, R: Integer);
  Var
    I, J, K : Integer;
    t : TMatch;
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
        While a[I].Priority > a[K].Priority Do
          Inc(I);

        While a[J].Priority < a[K].Priority Do
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




function TLOINCServices.Search(sText: String; all: boolean): TMatchArray;
  Function Match(const words : TSearchWordArray; s : String; iDepth : byte):Double;
  var
    i : integer;
  Begin
    result := 0;
    s := lowercase(s);
    for i := 0 to length(words) - 1 Do
      if pos(words[i].original, s) > 0 Then
      Begin
        result := result + 10;
        if s = words[i].original then
          result := result + 12
        Else if StringStartsWith(s, words[i].original) Then
          result := result + 3
        Else If s.EndsWith(words[i].original) Then
          result := result + 1.5
        Else If (pos(' '+words[i].original+' ', s) > 0) Then
          result := result + 0.6;
        result := result + result / length(s);
        if iDepth > 0 Then
          result := result + result / iDepth;
      End;
  End;

  Procedure AddResult(var iCount : Integer; iindex : cardinal; sId : String; priority : Double; bcode : boolean);
  Begin
    if iCount = length(result) then
      SetLength(result, Length(result)+100);
    result[iCount].index := iIndex;
    result[iCount].code := sId;
    result[iCount].iscode := bcode;
    result[iCount].Priority := priority;
    Inc(iCount);
  End;

  Function FlagFactor(iFlag : Byte): double;
  Begin
    if  iFlag and FLAG_LONG_COMMON > 0 then
      result := 4
    Else
      result := 1;
  End;

  Procedure CheckCode(const words : TSearchWordArray; var iCount : Integer; iCodeIndex : Integer);
  var
    i, j : cardinal;
    r1 : Double;
    Desc : TCardinalArray;
    iDescription, iStems, iOtherNames : Cardinal;
    iEntry : Cardinal;
    sCode1 : String;
    iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Cardinal;
    iFlags : Byte;
    matches : integer;
    ok : boolean;
    lang : byte;
  Begin
    CodeList.GetInformation(iCodeIndex, nil, sCode1, iDescription, iOtherNames, iEntry, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);
    r1 := 0;
    matches := 0;
    Desc := Refs.GetCardinals(iStems);
    For i := 0 to length(words) - 1 do
    begin
      if words[i].stem <> 0 Then
      begin
        ok := false;
        For j := 0 to length(Desc) - 1 do
          if (words[i].stem = desc[j]) Then
          begin
            r1 := r1 + 20 + (20 / length(desc));
            ok := true;
            assert(FDesc.GetEntry(words[i].stem, lang) = FDesc.GetEntry(desc[j], lang));
          end
          else
            assert(FDesc.GetEntry(words[i].stem, lang) <> FDesc.GetEntry(desc[j], lang));
        if ok then
          inc(matches);
      end;
    end;

    if (not all or (matches = length(words))) and (r1  > 0) Then
      AddResult(iCount, iCodeIndex, sCode1, r1, true);
  End;

  Procedure CheckEntry(const words : TSearchWordArray; var iCount : Integer; index : Integer);
  var
    i, j : cardinal;
    code, text, parent, children, descendants, concepts, descendentConcepts, stems: Cardinal;
    r1 : Double;
    Desc : TCardinalArray;
    matches : integer;
    ok : boolean;
    lang : byte;
  Begin
    Entries.GetEntry(index, code, text, parent, children, descendants, concepts, descendentConcepts, stems);
    r1 := 0;
    Desc := Refs.GetCardinals(stems);
    matches := 0;
    For i := 0 to length(words) - 1 do
    begin
      if words[i].stem <> 0 Then
      begin
        ok := false;
        For j := 0 to length(Desc) - 1 do
        begin
          if (words[i].stem = desc[j]) Then
          begin
            r1 := r1 + 20 + (20 / length(desc));
            ok := true;
          end
          else
            assert(FDesc.GetEntry(words[i].stem, lang) <> FDesc.GetEntry(desc[j], lang));
        end;
        if ok then
          inc(matches);
      end;
    end;

    if (not all or (matches = length(words))) and (r1  > 0) Then
      AddResult(iCount, index, FDesc.GetEntry(code, lang), 1000000+r1, false); // these always come first
  End;

var
  iCount : Integer;
  words : TSearchWordArray;
  i : cardinal;
  index : integer;
  oStemmer : TYuStemmer_8;
  s : String;
  s1 : String;
begin
  SetLength(words, 0);
  sText := LowerCase(sText);
  oStemmer := GetStemmer_8('english');
  Try
    while (sText <> '') Do
    Begin
      StringSplit(sText, [',', ' ', ':', '.', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '{', '}', '[', ']', '|', '\', ';', '"', '<', '>', '?', '/', '~', '`', '-', '_', '+', '='], s, sText);
      if (s <> '') Then
      Begin
        SetLength(words, length(words)+1);
        words[length(words)-1].original := s;
        s1 := oStemmer.calc(s);
        if FindStem(s1, index) Then
          words[length(words)-1].stem := FStems.GetString(index);
      End;
    End;
  Finally
    oStemmer.free;
  End;

  if Length(words) = 0 then
    raise ETerminologyError.create('no usable search text found');


  iCount := 0;
  SetLength(result, 100);
  sText := lowercase(sText);
  for i := 0 to FCode.Count - 1 Do
    CheckCode(words, iCount, i);
  for i := 0 to FEntries.Count - 1 Do
    CheckEntry(words, iCount, i);

  SetLength(result, iCount);
  QuickSortArray(result);
End;
(*
var
  iCount : Integer;
  i : cardinal;
  iDescription, iOtherNames : Cardinal;
  s, sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;
  oReg : TRegExpr;
  bIncl : Boolean;
  Procedure AddResult(iindex : cardinal);
  Begin
    if iCount = length(result) then
      SetLength(result, Length(result)+100);
    result[iCount] := iIndex;
    Inc(iCount);
  End;
begin
  oReg := TRegExpr.Create;
  Try
    if false Then
      oReg.Expression := sText
    Else
      sText := lowercase(sText);
    iCount := 0;
    SetLength(result, 100);
    for i := 0 to Code.Count - 1 Do
    Begin
      Code.GetInformation(i, lang, sCode1, iDescription, iOtherNames, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);

      s := Desc.GetEntry(iDescription);
      if false Then
        bIncl := oReg.Exec(s)
      Else
        bIncl := (pos(sText, lowercase(Desc.GetEntry(iDescription))) > 0) or (sText = '');

      if bIncl Then
        Addresult(i);
    End;
    SetLength(result, iCount);
  Finally
    oReg.Free;
  End;
end;
*)


{ TLOINCServiceList }

destructor TLOINCServiceList.Destroy;
begin
  FDefinition.Free;
  inherited;
end;

function TLOINCServiceList.GetByKey(sKey: String): TLOINCServices;
var
  i, k : integer;
begin
  Result := nil;
  i := 0;
  k := StrToIntDef(sKey, 0);
  While (i < Count) and (result = nil) do
  Begin
    if Service[i].Key = k then
      result := Service[i];
    inc(i);
  End;
  if result = nil then
    result := DefaultService;
end;

function TLOINCServiceList.GetDefinition: TLOINCServices;
begin
  if FDefinition = nil then
    raise ETerminologyError.create('There is no default LOINC service');
  result := FDefinition;
end;

function TLOINCServiceList.GetService(i: integer): TLOINCServices;
begin
  result := TLOINCServices(ObjectByIndex[i]);
end;

function TLOINCServiceList.GetServiceByName(sName: String): TLOINCServices;
var
  i : integer;
begin
  if sName = '' then
    result := DefaultService
  Else
  Begin
    Result := nil;
    i := 0;
    While (i < Count) and (result = nil) do
    Begin
      if SameText(Service[i].LOINCVersion, sName) then
        result := Service[i];
      inc(i);
    End;
  End;end;

function TLOINCServiceList.HasDefaultService: Boolean;
begin
  result := FDefinition <> nil;
end;

function TLOINCServiceList.ItemClass: TFslObjectClass;
begin
  result := TLOINCServices;
end;

procedure TLOINCServiceList.SetDefinition(const Value: TLOINCServices);
begin
  FDefinition := Value;
end;

{ TLoincWords }

procedure TLoincWords.AddWord(index: Cardinal; Flags: Byte);
begin
  FBuilder.AddCardinal(index);
  FBuilder.Append(flags);
end;

function TLoincWords.Count: Integer;
begin
  result := FLength div 5;
end;

procedure TLoincWords.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TLoincWords.GetEntry(iIndex: Cardinal; var index: Cardinal; var flags: Byte);
var
  l : Cardinal;
begin
  l := (iIndex * 5);
  if l > FLength - 5 Then
    raise ETerminologyError.create('invalid index');
  move(FMaster[l], index, 4);
  move(FMaster[l+4], flags, 1);
end;

function TLoincWords.GetString(iIndex: Cardinal): Cardinal;
var
  f : byte;
begin
  GetEntry(iIndex, result, f);
end;

procedure TLoincWords.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

{ TLoincStems }

procedure TLoincStems.AddStem(index : Cardinal);
begin
  FBuilder.AddCardinal(index);
end;

function TLoincStems.Count: Integer;
begin
  result := FLength div 4;
end;

procedure TLoincStems.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TLoincStems.GetEntry(iIndex: Cardinal; var index: Cardinal);
var
  l : Cardinal;
begin
  l := (iIndex * 4);
  if l > FLength - 4 Then
    raise ETerminologyError.create('invalid index');
  move(FMaster[l], index, 4);
end;

function TLoincStems.GetString(iIndex: Cardinal): Cardinal;
begin
  GetEntry(iIndex, result);
end;

procedure TLoincStems.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

function TLoincServices.FindStem(s: String; var index: Integer): Boolean;
var
  L, H, I, c: Integer;
  lang : byte;
begin
  Result := False;
  L := 0;
  H := FStems.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStr(FDesc.GetEntry(FStems.GetString(i), lang), s);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  index := L;
end;


procedure TLOINCServices.getCDSInfo(card: TCDSHookCard; lang, baseURL, code, display: String);
var
  b : TStringBuilder;
  iIndex : Cardinal;
  iDescription, iOtherNames, iStems : Cardinal;
  sCode1, s : String;
  iEntry : Cardinal;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Cardinal;
  iFlags : Byte;
  iRefs : TCardinalArray;
  i : integer;
  first : boolean;
  langs : TLangArray;
  ll, ilang : byte;
  ok : boolean;
begin
  langs := langsForLang(lang);
  b := TStringBuilder.Create;
  try
    iRefs := nil;
    if not CodeList.FindCode(code, iIndex) Then
      b.Append('* Error: Code '+code+' not known')
    else
    Begin
      card.addLink('Further Detail', baseURL+'/loinc/doco/?type=loinc&code='+code);

      CodeList.GetInformation(iIndex, langs, sCode1, iDescription, iOtherNames, iEntry, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);
      b.Append('LOINC Code '+code+' : '+Desc.GetEntry(iDescription, ll)+#13#10#13#10);
      if iComponent <> 0 Then
        b.Append('* Component: '+GetConceptDesc(iComponent, langs)+#13#10);
      if iProperty <> 0 Then
        b.Append('* Property: '+GetConceptDesc(iProperty, langs)+#13#10);
      if iTimeAspect <> 0 Then
        b.Append('* Time Aspect: '+GetConceptDesc(iTimeAspect, langs)+#13#10);
      if iSystem <> 0 Then
        b.Append('* System: '+GetConceptDesc(iSystem, langs)+#13#10);
      if iScale <> 0 Then
        b.Append('* Scale: '+GetConceptDesc(iScale, langs)+#13#10);
      if iMethod <> 0 Then
        b.Append('* Method: '+GetConceptDesc(iMethod, langs)+#13#10);
      if iClass <> 0 Then
        b.Append('* Class: '+GetConceptDesc(iClass, langs)+#13#10);

      b.Append('* Type: ');
      if iFlags and FLAGS_CLIN > 0 Then
        b.Append('Clinical'+#13#10)
      Else if iFlags and FLAGS_ATT > 0 Then
        b.Append('Attachment'+#13#10)
      Else if iFlags and FLAGS_SURV > 0 Then
        b.Append('Survey'+#13#10)
      Else
        b.Append('Lab'+#13#10);

      b.Append('* Status: ');
      if iFlags and FLAGS_HOLD > 0 Then
        b.Append('Not yet final'+#13#10)
      Else
        b.Append('Final'+#13#10);

      if iFlags and FLAGS_UNITS > 0 Then
        b.Append('* Units are required'+#13#10);

      b.Append('* Order/Obs Status: ');
      if (iFlags and FLAGS_ORDER> 0 ) and (iFlags and FLAGS_OBS> 0 ) Then
        b.Append('Both'+#13#10)
      Else if iFlags and FLAGS_ORDER > 0 Then
        b.Append('Order'+#13#10)
      Else if iFlags and FLAGS_OBS > 0 Then
        b.Append('Observation'+#13#10)
      else
        b.Append(#13#10);

      if iOtherNames <> 0 Then
      begin
        first := true;
        b.Append('* Other Names: ');
        iRefs := Refs.GetCardinals(iOtherNames);
        for i := Low(iRefs) To High(iRefs) Do
          if iRefs[i] <> 0 Then
          begin
            s := desc.GetEntry(iRefs[i], ll);
            ok := false;
            for ilang in langs do
              if (ilang = ll) then
                ok := true;
            if ok then
            begin
              if first then
                first := false
              else
                b.Append(', ');
              b.Append(s);
            end;
          End;
        b.Append(#13#10);
      End;
    End;
    b.Append(#13#10+'This LOINC&copy; content is copyright &copy; 1995 Regenstrief Institute, Inc. and the LOINC Committee, and available at no cost under the license at <http://loinc.org/terms-of-use>'#13#10);
    card.detail := b.ToString;
  finally
    b.Free;
  end;
end;

function TLOINCServices.buildValueSet(factory : TFHIRFactory; id: String): TFhirValueSetW;
var
  index : cardinal;
  code, text, parent, children, descendants, concepts, descendentConcepts, stems: Cardinal;
  answers : TCardinalArray;
  inc : TFhirValueSetComposeIncludeW;
  filt :  TFhirValueSetComposeIncludeFilterW;
  i : integer;
  cc : TFhirValueSetComposeIncludeConceptW;
  lang : byte;
begin
  result := nil;
  if (id = '') then
  begin
    result := factory.wrapValueSet(factory.makeByName('ValueSet') as TFHIRResourceV);
    try
      result.url := id;
      result.status := psActive;
      result.version := Version(nil);
      result.name := 'LOINC Value Set - all LOINC codes';
      result.description := 'All LOINC codes';
      result.date := TDateTimeEx.makeUTC;
      inc := result.addInclude;
      try
        inc.system := 'http://loinc.org';
      finally
        inc.free;
      end;
      result.link;
    finally
      result.free;
    end;
  end
  else if (id.StartsWith('http://loinc.org/vs/') and FEntries.FindCode(id.Substring(20), index, FDesc)) then
  begin
    FEntries.GetEntry(index, code, text, parent, children, descendants, concepts, descendentConcepts, stems);

    result := factory.wrapValueSet(factory.makeByName('ValueSet') as TFHIRResourceV);
    try
      result.url := id;
      result.status := psActive;
      result.version := Version(nil);
      result.name := 'LOINC Value Set from Multi-Axial Heirarchy term '+id.Substring(20);
      result.description := 'All LOINC codes for '+Desc.GetEntry(text, lang);
      result.date := TDateTimeEx.makeUTC;
      inc := result.addInclude;
      try
        inc.system := 'http://loinc.org';
        filt := inc.addFilter;
        try
          filt.prop := 'ancestor';
          filt.op := foEqual;
          filt.value := id.Substring(20);
        finally
          filt.Free;
        end;
      finally
        inc.Free;
      end;
      result.link;
    finally
      result.free;
    end;
  end
  else if (id.StartsWith('http://loinc.org/vs/') and FAnswerLists.FindCode(id.Substring(20), index, FDesc)) then
  begin
    FAnswerLists.GetEntry(index, code, text, children);
    result := factory.wrapValueSet(factory.makeByName('ValueSet') as TFHIRResourceV);
    try
      result.url := id;
      result.status := psActive;
      result.version := Version(nil);
      result.name := 'LOINC Answer List '+id.Substring(20);
      result.description := 'LOINC Answer list for '+Desc.GetEntry(text, lang);
      result.date := TDateTimeEx.makeUTC;
      inc := result.addInclude;
      try
        inc.system := 'http://loinc.org';
        answers := FRefs.GetCardinals(children);
        for i := 0 to Length(answers) - 1 do
        begin
          cc := inc.addConcept;
          try
            FAnswerLists.GetEntry(answers[i], code, text, children);
            cc.code := Desc.GetEntry(code, lang);
            cc.display := Desc.GetEntry(text, lang);
          finally
            cc.free;
          end;
        end;
      finally
        inc.Free;
      end;
      result.link;
    finally
      result.free;
    end;
  end;
end;

function TLoincServices.ChildCount(context: TCodeSystemProviderContext): integer;
begin
  // no children in loinc
  if context = nil then
    result := TotalCount
  else
    result := 0;
end;

procedure TLOINCServices.Close(ctxt: TCodeSystemProviderContext);
begin
  // nothing, because it's actually a pointer to an integer
end;

function TLoincServices.getcontext(context: TCodeSystemProviderContext; ndx: integer): TCodeSystemProviderContext;
begin
  if (context = nil) then
    result := TCodeSystemProviderContext(ndx+1) // offset from 0 to avoid ambiguity about nil contxt, and first entry
  else
    raise ETerminologyError.create('shouldn''t be here');
end;

function TLoincServices.Code(context: TCodeSystemProviderContext): string;
var
  index : cardinal;
  iDescription, iStems, iOtherNames : Cardinal;
  iEntry, iCode, iOther, iConcepts, iStem, iParent, iChildren, iDescendents, iDescendantConcepts : Cardinal;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Cardinal;
  iFlags : Byte;
  lang : byte;
begin
  index := cardinal(context)-1;
  if index < CodeList.Count then
    CodeList.GetInformation(index, nil, result, iDescription, iOtherNames, iStems, iEntry, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags)
  else if index < CodeList.Count + AnswerLists.Count then
  begin
    FAnswerLists.GetEntry(index - CodeList.Count, iCode, iDescription, iOther);
    result := Desc.GetEntry(iCode, lang);
  end
  else
  begin
    FEntries.GetEntry(index - (CodeList.Count + AnswerLists.Count), iCode, iDescription, iParent, iChildren, iDescendents, iConcepts, iDescendantConcepts, iStem);
    result := Desc.GetEntry(iCode, lang);
  end
end;

function TLoincServices.Display(context: TCodeSystemProviderContext; lang : String): string;
var
  index : cardinal;
  iCode, iDescription, iStems, iOtherNames, iOther : Cardinal;
  iEntry, iParent, iChildren, iDescendents, iConcepts, iDescendantConcepts, iStem : Cardinal;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Cardinal;
  iFlags : Byte;
  ilang : byte;
begin
  index := cardinal(context)-1;
  if index < CodeList.Count then
    CodeList.GetInformation(index, langsForLang(lang), result, iDescription, iOtherNames, iEntry, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags)
  else if index < CodeList.Count + AnswerLists.Count then
    FAnswerLists.GetEntry(index - CodeList.Count, iCode, iDescription, iOther)
  else
    FEntries.GetEntry(index - (CodeList.Count + AnswerLists.Count), iCode, iDescription, iParent, iChildren, iDescendents, iConcepts, iDescendantConcepts, iStem);
  result := Desc.GetEntry(iDescription, ilang);
end;

procedure TLOINCServices.Displays(context: TCodeSystemProviderContext; list: TStringList; lang : String);
begin
  GetDisplaysByName(Code(context), langsForLang(lang), list);
end;

procedure TLOINCServices.extendLookup(factory : TFHIRFactory; ctxt: TCodeSystemProviderContext; lang : String; props: TArray<String>; resp: TFHIRLookupOpResponseW);
var
  index : cardinal;
  iDescription, iStems, iOtherNames : Cardinal;
  iEntry : Cardinal;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Cardinal;
  iFlags : Byte;
  s : String;
  langs : TLangArray;
  ll : byte;
  {$IFNDEF FHIR2}
  iRefs : TCardinalArray;
  i : integer;
  p : TFHIRLookupOpRespPropertyW;
  d : TFHIRLookupOpRespDesignationW;
  {$ENDIF}
begin
  langs := langsForLang(lang);
  index := cardinal(ctxt)-1;
  if index < CodeList.Count then
  begin
    CodeList.GetInformation(index, langs, s, iDescription, iOtherNames, iEntry, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);

  {$IFNDEF FHIR2}
    if hasProp(props, 'COMPONENT', true) and (iComponent <> 0) Then
      resp.AddProp('COMPONENT').description := GetConceptDesc(iComponent, langs);
    if hasProp(props, 'PROPERTY', true) and (iProperty <> 0) Then
      resp.AddProp('PROPERTY').description := GetConceptDesc(iProperty, langs);
    if hasProp(props, 'TIME_ASPCT', true) and (iTimeAspect <> 0) Then
      resp.AddProp('TIME_ASPCT').description := GetConceptDesc(iTimeAspect, langs);
    if hasProp(props, 'SYSTEM', true) and (iSystem <> 0) Then
      resp.AddProp('SYSTEM').description := GetConceptDesc(iSystem, langs);
    if hasProp(props, 'SCALE_TYP', true) and (iScale <> 0) Then
      resp.AddProp('SCALE_TYP').description := GetConceptDesc(iScale, langs);
    if hasProp(props, 'METHOD_TYP', true) and (iMethod <> 0) Then
      resp.AddProp('METHOD_TYP').description := GetConceptDesc(iMethod, langs);
    if hasProp(props, 'CLASS', true) and (iClass <> 0) Then
      resp.AddProp('CLASS').description := GetConceptDesc(iClass, langs);
    if hasProp(props, 'CLASSTYPE', true) Then
    Begin
      p := resp.AddProp('CLASSTYPE');
      if iFlags and FLAGS_CLIN > 0 Then
        p.description := 'Clinical'
      Else if iFlags and FLAGS_ATT > 0 Then
        p.description := 'Attachment'
      Else if iFlags and FLAGS_SURV > 0 Then
        p.description := 'Survey'
      Else
        p.description := 'Lab';
    end;

    if hasProp(props, 'STATUS', true) Then
    Begin
      p := resp.AddProp('STATUS');
      if iFlags and FLAGS_HOLD > 0 Then
        p.description := 'Not yet final'
      Else
        p.description := 'Final';
    end;

    if hasProp(props, 'Root', true) Then
    Begin
      if iFlags and FLAGS_ROOT > 0 Then
      Begin
        resp.AddProp('Root').description := 'This is a root of a set';
      End;
    end;

    if hasProp(props, 'UNITSREQUIRED', true) Then
    Begin
      if iFlags and FLAGS_UNITS > 0 Then
      Begin
        resp.AddProp('UNITSREQUIRED').description := 'Units are required';
      End;
    end;

    if hasProp(props, 'ORDER_OBS', true) Then
    Begin
      p := resp.AddProp('ORDER_OBS');
      if (iFlags and FLAGS_ORDER> 0 ) and (iFlags and FLAGS_OBS> 0 ) Then
        p.description := 'Both'
      Else if iFlags and FLAGS_ORDER > 0 Then
        p.description := 'Order'
      Else if iFlags and FLAGS_OBS > 0 Then
        p.description := 'Observation'
      else
        p.description := 'Neither';
    end;

    if iOtherNames <> 0 Then
    Begin
      iRefs := Refs.GetCardinals(iOtherNames);
      for i := Low(iRefs) To High(iRefs) Do
        if iRefs[i] <> 0 Then
        begin
          s := Desc.GetEntry(iRefs[i], ll);
          if useLang(ll, langs, false) then
            resp.addDesignation('http://snomed.info/sct', '446211000124102', 'Alias name', s);
        end;
    End;
    {$ENDIF}
  End;
end;

procedure TLoincServices.Displays(code: String; list: TStringList; lang : String);
begin
  GetDisplaysByName(code, langsForLang(lang), list);
end;


function TLOINCServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TLoincServices.getDisplay(code: String; lang : String): String;
begin
  result := GetDisplayByName(code, langsForLang(lang));
  if result = '' then
    raise ETerminologyError.create('unable to find '+code+' in '+system(nil));
end;

function TLoincServices.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  result := false; // loinc don't do abstract
end;

function TLoincServices.locate(code: String; var message : String): TCodeSystemProviderContext;
var
  i : Cardinal;
begin
  if CodeList.FindCode(code, i) then
    result := TCodeSystemProviderContext(i+1)
  else if AnswerLists.FindCode(code, i, FDesc) then
    result := TCodeSystemProviderContext(cardinal(CodeList.Count)+ i+1)
  else
    result := nil;//raise ETerminologyError.create('unable to find '+code+' in '+system);
end;

function TLoincServices.system(context : TCodeSystemProviderContext): String;
begin
  result := 'http://loinc.org';
end;

function TLoincServices.TotalCount: integer;
begin
  result := CodeList.Count;
end;

function TLOINCServices.useLang(lang: byte; langs: TLangArray; incLast : boolean): boolean;
var
  b: byte;
begin
  result := false;
  for b := 0 to length(langs) - 1  do
    if (langs[b] = lang) and (inclast or ((b = 0) or (b < length(langs) - 1))) then
      exit(true);
end;

function TLOINCServices.version(context: TCodeSystemProviderContext): String;
begin
  result := FVersion;
end;

function TLoincServices.InFilter(ctxt: TCodeSystemProviderFilterContext; concept: TCodeSystemProviderContext): Boolean;
begin
  result := THolder(ctxt).HasChild(integer(concept)-1);
end;

function THolder.HasChild(v : integer) : boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to Length(Children) - 1 do
    if (Cardinal(v) = Children[i]) then
    begin
      result := true;
      exit;
    end;
end;

procedure TLoincServices.Close(ctxt: TCodeSystemProviderFilterContext);
begin
  THolder(ctxt).free;
end;

function TLoincServices.FilterByPropertyId(prop : TLoincPropertyType; op: TFhirFilterOperator; value: String): TCodeSystemProviderFilterContext;
  function getProp(i : integer) : String;
  var
    langs : TLangArray;
    lang : byte;
    code : String;
    idesc, names, entry, stems : Cardinal;
    comp, iprop, time, system, scale, method, clss : Cardinal;
    flags : Byte;
  begin
    CodeList.GetInformation(i, langs, code, idesc, names, entry, stems, comp, iprop, time, system, scale, method, clss, flags);
    case prop of
      lptComponents: result := GetConceptDesc(comp, langs);
      lptProperties: result := GetConceptDesc(iprop, langs);
      lptTimeAspects: result := GetConceptDesc(time, langs);
      lptSystems: result := GetConceptDesc(system, langs);
      lptScales: result := GetConceptDesc(scale, langs);
      lptMethods: result := GetConceptDesc(method, langs);
      lptClasses: result := GetConceptDesc(clss, langs);
    else
      result := '';
    end;
  end;
var
  id, offset: Cardinal;
  iName, iChildren, iCodes : Cardinal;
  aMatches, aChildren : FHIR.Loinc.Services.TCardinalArray;
  p : TArray<String>;
  v : String;
  regex : TRegEx;
  i, t : integer;
begin
  if not (op in [foEqual, foIn, foRegex]) then
    raise ETerminologyError.create('Unsupported operator type '+CODES_TFhirFilterOperator[op]);

  if op = foRegex then
  begin
    SetLength(aMatches, CodeList.Count);
    t := 0;
    regex := TRegEx.Create(value);
    for i := 0 to CodeList.Count - 1 do
    begin
      v := getProp(i);
      if regex.IsMatch(v) then
      begin
        aMatches[t] := i;
        inc(t);
      end;
    end;
    SetLength(aMatches, t);
    result := THolder.create;
    THolder(result).Children := aMatches;
  end
  else
  begin
    if op = foEqual then
      p := value.Split([#1])
    else
      p := value.Split([',']);
    result := THolder.create;
    try
      SetLength(aMatches, 0);
      for v in p do
      begin
        id := GetPropertyId(prop, noLang, v);
        if (id <> 0) then
        begin
          Concepts.GetConcept(id, noLang, iName, iChildren, iCodes);
          aChildren := Refs.GetCardinals(iCodes);
          if length(aChildren) > 0 then
          begin
            offset := length(aMatches);
            SetLength(aMatches, offset + length(aChildren));
            move(aChildren[0], aMatches[offset], length(aChildren) * SizeOf(cardinal));
          end;
        end;
      end;
      THolder(result).Children := aMatches;
      result.link;
    finally
      result.free;
    end;
  end;
end;

// this is a rare operation. But even so, is it worth pre-calculating this one on import?
function TLoincServices.FilterBySubset(op: TFhirFilterOperator; subset : TLoincSubsetId): TCodeSystemProviderFilterContext;
begin
  if op <> foEqual then
    raise ETerminologyError.Create('Unsupported operator type '+CODES_TFhirFilterOperator[op]);

  result := THolder.create;
  THolder(result).Children := FRefs.GetCardinals(FSubsets[subset]);
end;

Function SubSetForOrderObs(value : String): TLoincSubsetId;
begin
  if sameText(value, 'Both') Then
    result := lsiOrderObs
  else if sameText(value, 'Observation') Then
    result := lsiObs
  else if sameText(value, 'Order') Then
    result := lsiOrder
  else
    result := lsiNull;
end;

Function SubSetForClassType(value : String): TLoincSubsetId;
begin
  if sameText(value, '1') Then
    result := lsiTypeObservation
  else if sameText(value, '2') Then
    result := lsiTypeClinical
  else if sameText(value, '3') Then
    result := lsiTypeAttachment
  else if sameText(value, '4') Then
    result := lsiTypeSurvey
  else
    result := lsiNull;
end;

Function SubSetForCopyright(value : String): TLoincSubsetId;
begin
  if sameText(value, 'LOINC') Then
    result := lsiInternal
  else if sameText(value, '3rdParty') Then
    result := lsi3rdParty
  else if sameText(value, 'All') Then
    result := lsiAll
  else
    result := lsiNull;
end;

Function SubSetForStatus(value : String): TLoincSubsetId;
begin
  if sameText(value, 'ACTIVE') Then
    result := lsiActive
  else if sameText(value, 'DEPRECATED') Then
    result := lsiDeprecated
  else if sameText(value, 'DISCOURAGED') Then
    result := lsiDiscouraged
  else if sameText(value, 'TRIAL') Then
    result := lsiTrial
  else
    result := lsiNull;
end;

function TLoincServices.FilterByHeirarchy(op: TFhirFilterOperator; value: String; transitive : boolean): TCodeSystemProviderFilterContext;
var
  index : Cardinal;
  aChildren, c : FHIR.Loinc.Services.TCardinalArray;
  code, text, parent, children, descendants, concepts, descendentConcepts, stems: Cardinal;
  s : String;
begin
  result := THolder.create;
  try
    if (op = foEqual) and (value.Contains(',')) then
      raise ETerminologyError.create('Value is illegal - no commas');
    if (not (op in [foEqual, foIn])) then
      raise ETerminologyError.create('Unsupported operator type '+CODES_TFhirFilterOperator[op]);

    while (value <> '') do
    begin
      StringSplit(value, ',', s, value);
      if (FEntries.FindCode(s, index, FDesc)) then
      begin
        FEntries.GetEntry(index, code, text, parent, children, descendants, concepts, descendentConcepts, stems);
        if transitive then
          c := Refs.getCardinals(descendentConcepts)
        else
          c := Refs.getCardinals(concepts);
        if (length(c) > 0) then
        begin
          setLength(aChildren, length(aChildren) + length(c));
          move(c[0], aChildren[length(aChildren) - length(c)], length(c)*4);
        end;
      end;
    end;

    THolder(result).Children := aChildren;
    result.link;
  finally
    result.free;
  end;
end;

function TLoincServices.FilterByIsA(value: String; this : boolean): TCodeSystemProviderFilterContext;
var
  index, i: Cardinal;
  aChildren, c : FHIR.Loinc.Services.TCardinalArray;
  code, text, parent, children, descendants, concepts, descendentConcepts, stems: Cardinal;
  s : String;
begin
  result := THolder.create;
  try
    setLength(aChildren, 0);

    while (value <> '') do
    begin
      StringSplit(value, ',', s, value);
      if (FEntries.FindCode(s, index, FDesc)) then
      begin
        if this then
        begin
          setLength(aChildren, length(aChildren)+1);
          aChildren[length(aChildren)-1] := CodeList.Count + AnswerLists.Count + index;
        end;
        FEntries.GetEntry(index, code, text, parent, children, descendants, concepts, descendentConcepts, stems);
        c := Refs.getCardinals(descendants);
        if (length(c) > 0) then
        begin
          setLength(aChildren, length(aChildren) + length(c));
          for i := 0 to length(c) - 1 do
            aChildren[length(aChildren) - length(c) + i] := c[i] + CodeList.Count + AnswerLists.Count;
        end;
        c := Refs.getCardinals(descendentConcepts);
        if (length(c) > 0) then
        begin
          setLength(aChildren, length(aChildren) + length(c));
          move(c[0], aChildren[length(aChildren) - length(c)], length(c)*4);
        end;
      end;
    end;

    THolder(result).Children := aChildren;
    result.link;
  finally
    result.free;
  end;
end;

function TLoincServices.filter(prop: String; op: TFhirFilterOperator; value: String; prep : TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
begin
  if prop = 'SCALE_TYP' then
    result := FilterByPropertyId(lptScales, op, value)
  else if prop = 'CLASS' then
    result := FilterByPropertyId(lptClasses, op, value)
  else if prop = 'COMPONENT' then
    result := FilterByPropertyId(lptComponents, op, value)
  else if prop = 'PROPERTY' then
    result := FilterByPropertyId(lptProperties, op, value)
  else if prop = 'TIME_ASPCT' then
    result := FilterByPropertyId(lptTimeAspects, op, value)
  else if prop = 'SYSTEM' then
    result := FilterByPropertyId(lptSystems, op, value)
  else if prop = 'METHOD_TYP' then
    result := FilterByPropertyId(lptMethods, op, value)
  else if prop = 'ORDER_OBS' then
    result := FilterBySubset(op, SubSetForOrderObs(value))
  else if prop = 'CLASSTYPE' then
    result := FilterBySubset(op, SubSetForClassType(value))
  else if prop = 'STATUS' then
    result := FilterBySubset(op, SubSetForStatus(value))
  else if prop = 'copyright' then
    result := FilterBySubset(op, SubSetForCopyright(value))
  else if prop = 'parent' then
    result := FilterByHeirarchy(op, value, false)
  else if prop = 'ancestor' then
    result := FilterByHeirarchy(op, value, true)
  else if (prop = 'concept') and (op in [foIsA, foDescendentOf]) then
    result := FilterByIsA(value, op = foIsA)
  else if prop = 'Type' then
    result := THolder.create
  else
    result := nil;
end;

function TLoincServices.FilterConcept(ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  result := TCodeSystemProviderContext(THolder(ctxt).children[THolder(ctxt).ndx-1]+1);
end;

function TLoincServices.FilterMore(ctxt: TCodeSystemProviderFilterContext): boolean;
begin
  inc(THolder(ctxt).ndx);
  result := THolder(ctxt).ndx <= length(THolder(ctxt).children);
end;

function TLoincServices.locateIsA(code, parent: String): TCodeSystemProviderContext;
begin
  result := nil; // cause loinc don't do subsumption
end;

function TLOINCServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'LOINC';
end;

function TLoincServices.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String; var message : String): TCodeSystemProviderContext;
var
  i : Cardinal;
  holder : THolder;
begin
  if (ctxt = nil) then
    result := nil
  else
  begin
    holder := THolder(ctxt);
    if CodeList.FindCode(code, i) and holder.hasChild(i) then
      result := TCodeSystemProviderContext(i+1)
    else
      result := nil;
  end;
end;


function TLOINCServices.findMAConcept(code: String): Cardinal;
begin
  if not Entries.FindCode(code, result, FDesc) then
    result := 0;
end;

{ TLOINCHeirarchyEntryList }

procedure TLOINCHeirarchyEntryList.SetStems(iIndex, iValue: Cardinal);
begin
  Move(iValue, FMaster[iIndex*(32)+28], 4);
end;

procedure TLOINCHeirarchyEntryList.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

function TLOINCHeirarchyEntryList.AddEntry(code, text, parent, children, descendants, concepts, descendentConcepts: Cardinal): Cardinal;
begin
  Result := FBuilder.Length div 32;
  FBuilder.AddCardinal(code);
  FBuilder.AddCardinal(text);
  FBuilder.AddCardinal(children);
  FBuilder.AddCardinal(descendants);
  FBuilder.AddCardinal(concepts);
  FBuilder.AddCardinal(descendentConcepts);
  FBuilder.AddCardinal(parent);
  FBuilder.AddCardinal(0); // stems, fill out later
end;

function TLOINCHeirarchyEntryList.Count: Integer;
begin
  result := Length(FMaster) div 32;
end;

procedure TLOINCHeirarchyEntryList.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FBuilder.Free;
end;

function TLOINCHeirarchyEntryList.FindCode(sCode: String; var iIndex: Cardinal; Strings : TLoincStrings): Boolean;
var
  L, H, I, d : Cardinal;
  C: Integer;
  s : String;
  lang : byte;
begin
  if Length(FMaster) = 0 Then
    Result := False
  Else
  Begin
    Result := False;
    L := 0;
    H := (Length(FMaster) div (32)) - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      Move(FMaster[i*32], d, 4);
      s := Strings.GetEntry(d, lang);
      C := CompareStr(s, sCode);
      if C < 0 then L := I + 1 else
      begin
        if i = 0 then
          break;
        H := I - 1;
        if C = 0 then
        begin
          Result := True;
          L := I;
        end;
      end;
    end;
    iIndex := L;
  End;
end;

procedure TLOINCHeirarchyEntryList.GetEntry(iIndex: Cardinal; var code, text, parent, children, descendants, concepts, descendentConcepts, stems: Cardinal);
begin
  if iIndex > (Length(FMaster) div 32) Then
    raise ETerminologyError.create('Attempt to access invalid LOINC Entry index');
  Move(FMaster[(iIndex*32)+0], code, 4);
  Move(FMaster[(iIndex*32)+4], text, 4);
  Move(FMaster[(iIndex*32)+8], children, 4);
  Move(FMaster[(iIndex*32)+12], descendants, 4);
  Move(FMaster[(iIndex*32)+16], concepts, 4);
  Move(FMaster[(iIndex*32)+20], descendentConcepts, 4);
  Move(FMaster[(iIndex*32)+24], parent, 4);
  Move(FMaster[(iIndex*32)+28], stems, 4);
end;

function ascopy(s : TBytes; start, len : integer) : String;
var
  res : AnsiString;
begin
  if len = 0 then
    exit('');

  SetLength(res, len);
  if (length(s) < start + len) then
    raise ETerminologyError.create('Read off end of file: '+inttostr(length(s))+' / '+inttostr(start)+':'+inttostr(len));
  move(s[start], res[1], len);
  result := String(res);
end;

function utfcopy(s : TBytes; start, length : integer) : String;
begin
  result := TEncoding.UTF8.GetString(s, start, length);
end;


{ TLOINCAnswersList }

function TLOINCAnswersList.AddEntry(code, description, answers: Cardinal): Cardinal;
begin
  Result := FBuilder.Length div 12;
  FBuilder.AddCardinal(code);
  FBuilder.AddCardinal(description);
  FBuilder.AddCardinal(answers);
end;

function TLOINCAnswersList.Count: cardinal;
begin
  result := cLength(FMaster) div 12;
end;

procedure TLOINCAnswersList.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FBuilder.Free;
end;

function TLOINCAnswersList.FindCode(sCode: String; var iIndex: Cardinal; Strings: TLoincStrings): Boolean;
var
  L, H, I, d : integer;
  C: Integer;
  s : String;
  lang : byte;
begin
  if Length(FMaster) = 0 Then
    Result := False
  Else
  Begin
    Result := False;
    L := 0;
    H := (Length(FMaster) div (12)) - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      if ((i * 12) > length(FMaster) - 4) and DebugConsoleMessages then
        writeln('err');
      Move(FMaster[i*12], d, 4);
      s := Strings.GetEntry(d, lang);
      C := AnsiCompareStr(s, sCode);
      if C < 0 then L := I + 1 else
      begin
        H := I - 1;
        if C = 0 then
        begin
          Result := True;
          L := I;
        end;
      end;
    end;
    iIndex := L;
  End;
end;

procedure TLOINCAnswersList.GetEntry(iIndex: Cardinal; var code, description, answers: Cardinal);
begin
  if iIndex > (Length(FMaster) div 12) Then
    raise ETerminologyError.create('Attempt to access invalid LOINC Entry index');
  Move(FMaster[(iIndex*12)+0], code, 4);
  Move(FMaster[(iIndex*12)+4], description, 4);
  Move(FMaster[(iIndex*12)+8], answers, 4);
end;

procedure TLOINCAnswersList.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

function TLOINCServices.GetConceptDesc(iConcept : cardinal; langs : TLangArray):String;
var
  iName : Cardinal;
  iChildren : Cardinal;
  iCodes : Cardinal;
  lang : byte;
Begin
  if iConcept = 0 then
    result := ''
  Else
  Begin
    Concepts.GetConcept(iConcept, langs, iName, iChildren, iCodes);
    result := Desc.GetEntry(iname, lang);
  End;
End;


{ TLoincLanguages }

function TLoincLanguages.AddEntry(lang, country: String): byte;
begin
  if Length(lang) <> 2 Then
    raise ETerminologyError.create('LOINC Language code too long: '+lang);
  if Length(country) <> 2 Then
    raise ETerminologyError.create('LOINC Language code too long: '+country);
  assert(FBuilder.Length mod 10 = 0);
  result := FBuilder.Length div 10;
  FBuilder.AddString(lang+'-'+country);
end;

function TLoincLanguages.count: integer;
begin
  result := FLength div 10;
end;

procedure TLoincLanguages.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TLoincLanguages.GetEntry(iIndex: byte; var lang, country: String);
var
  s : String;
begin
  if ((iIndex +1) * 10 > FLength) then
    raise ETerminologyError.create('Wrong length index getting LOINC language code');
  SetLength(s, 5);
  Move(FMaster[iIndex*10], s[1], 10);
  StringSplit(s, '-', lang, country);
end;

procedure TLoincLanguages.StartBuild;
begin
  FBuilder := TFslBytesBuilder.Create;
end;

function nolang : TLangArray;
begin
  SetLength(result, 1);
  result[0] := 0;
end;

End.

