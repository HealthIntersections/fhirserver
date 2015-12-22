unit LoincServices;

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
  SysUtils, Classes,
  StringSupport, FileSupport, BytesSupport,
  AdvObjects, AdvObjectLists,
  regexpr, YuStemmer,
  FHIRTypes, FHIRResources, FHIRUtilities,
  TerminologyServices, DateAndTime;

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
  LOINC_CACHE_VERSION = '8';
  NO_PARENT = $FFFFFFFF;

Type
  TLoincPropertyType = (lptComponents, lptProperties, lptTimeAspects, lptSystems, lptScales, lptMethods, lptClasses);

type
  // We store LOINC as five structures.
  //   the first structure is a simply a list of strings which are variable length names - referred to from the other structures
  //   the second structure is a list of lists of word or cardinal references.
  //   the third structure is a list of concepts. each concept has a refernce to a name and a contained list of references which are either children
  //   the fourth structure is the code list - a list of loinc concepts, with codes and references to names and properties
  //   the fifth structure is the multi-axial heirarchy - parent, children, descendents, concepts, and descendent concepts

  // 1. a list of strings
  //   each entry in the String starts with a byte length, and then the series of characters (2 bytes)
  // we store loinc descriptions, and other names in here
  TLoincStrings = class (TAdvObject)
    Private
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TAdvBytesBuilder;
    Public
      Function GetEntry(iIndex : Cardinal):String;

      Procedure StartBuild;
      Function AddEntry(Const s : String) : Cardinal;
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

Type
  // word index. Every word is 5 bytes - a 4 byte index into the strings, and a 1 byte flag
  TLoincWords = class (TAdvObject)
    Private
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TAdvBytesBuilder;
   Public
      Procedure GetEntry(iIndex : Cardinal; var index : Cardinal; var flags : Byte);
      Function Count : Integer;
      Function GetString(iIndex : Cardinal) : Cardinal;

      Procedure StartBuild;
      Procedure AddWord(index : Cardinal; Flags : Byte);
      Procedure DoneBuild;
  End;

  // stem word index. Every word is 4 bytes - a 4 byte index into the strings
  TLoincStems = class (TAdvObject)
    Private
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TAdvBytesBuilder;
   Public
      Procedure GetEntry(iIndex : Cardinal; var index : Cardinal);
      Function Count : Integer;
      Function GetString(iIndex : Cardinal) : Cardinal;

      Procedure StartBuild;
      Procedure AddStem(index : Cardinal);
      Procedure DoneBuild;
  End;


  // 2. a list of list of references
  TLOINCReferences = class (TAdvObject)
    Private
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TAdvBytesBuilder;
    Public
      Function GetWords(iIndex : Cardinal) : TWordArray;
      Function GetCardinals(iIndex : Cardinal) : TCardinalArray;
      Function Getlength(iIndex : Cardinal) : Cardinal;

      Procedure StartBuild;
      Function AddWords(Const a : TWordArray) : Cardinal;
      Function AddCardinals(Const a : TCardinalArray) : Cardinal;
      Procedure DoneBuild;
  End;

  // 3. a list of concepts
  TLOINCConcepts = class (TAdvObject)
    Private
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TAdvBytesBuilder;
    Public
      Procedure GetConcept(iIndex : Word; var iName : Cardinal; var iChildren : Cardinal; var iConcepts : Cardinal);

      Procedure StartBuild;
      Function AddConcept(iName : Cardinal; iChildren : Cardinal; iConcepts : Cardinal) : Word;
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
  TLOINCCodeList = class (TAdvObject)
    Private
      FCodeLength, FReclength : Cardinal;
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TAdvBytesBuilder;
      procedure SetCodeLength(const Value: Cardinal);
    Public
      Function FindCode(sCode : String; var iIndex : Cardinal) : Boolean;

      Procedure GetInformation(iIndex: Cardinal; var sCode : String; var iDescription, iOtherNames, iEntry, iStems : Cardinal; var iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Word; var iFlags : Byte);

      // we presume that the Codes are registered in order
      Procedure StartBuild;
      Function AddCode(sCode : String; iDescription, iOtherNames, iEntry : Cardinal; iFlags : Byte) : Cardinal;
      Procedure DoneBuild;

      // these need to be called after Done Build
      Procedure SetComponent(iIndex : Cardinal; iValue : Word);
      Procedure SetProperty(iIndex : Cardinal; iValue : Word);
      Procedure SetTimeAspect(iIndex : Cardinal; iValue : Word);
      Procedure SetSystem(iIndex : Cardinal; iValue : Word);
      Procedure SetScale(iIndex : Cardinal; iValue : Word);
      Procedure SetMethod(iIndex : Cardinal; iValue : Word);
      Procedure SetClass(iIndex : Cardinal; iValue : Word);
      Procedure SetStems(iIndex : Cardinal; iValue : Cardinal);

      Function Count : Integer;
      Property CodeLength : Cardinal read FCodeLength Write SetCodeLength;
  End;

  // 5. the multi-axial heirachy
  // it is a list of entries
  // This is the effective logical structure of the bytes:
//  TLOINCEntry = {private} packed record
//    code : Cardinal;
//    text : Cardinal;
//    children : Cardinal;
//    descendents : Cardinal;
//    concepts : Cardinal;
//    descendentconcepts : Cardinal;
//  End;

  TLOINCHeirarchyEntryList = class (TAdvObject)
  Private
    FMaster : TBytes;
    FBuilder : TAdvBytesBuilder;
  Public
    Function FindCode(sCode : String; var iIndex : Cardinal; Strings : TLoincStrings) : Boolean;
    Procedure GetEntry(iIndex: Cardinal; var code, text, parent, children, descendents, concepts, descendentConcepts, stems : Cardinal);

    // we presume that the Codes are registered in order
    Procedure StartBuild;
    Function AddEntry(code, text, parent, children, descendents, concepts, descendentConcepts : Cardinal) : Cardinal;
    Procedure DoneBuild;

    // this needs to be called after Done Build
    Procedure SetStems(iIndex : Cardinal; iValue : Cardinal);

    Function Count : Integer;
  End;

  TLOINCAnswersList = class (TAdvObject)
  Private
    FMaster : TBytes;
    FBuilder : TAdvBytesBuilder;
  Public
    Function FindCode(sCode : String; var iIndex : Cardinal; Strings : TLoincStrings) : Boolean;
    Procedure GetEntry(iIndex: Cardinal; var code, description, answers : Cardinal);

    // we presume that the Codes are registered in order
    Procedure StartBuild;
    Function AddEntry(code, description, answers : Cardinal) : Cardinal;
    Procedure DoneBuild;

    Function Count : Integer;
  End;


  TLoincSubsetId = (lsiNull, lsiAll, lsiOrder, lsiObs, lsiOrderObs, lsiOrderSubset, lsiTypeObservation, lsiTypeClinical, lsiTypeAttachment,
    lsiTypeSurvey, lsiInternal, lsi3rdParty, lsiActive, lsiDeprecated, lsiDiscouraged, lsiTrial);
  TLoincSubsets = Array [TLoincSubsetId] of Cardinal;
  TLoincPropertyIds = Array [TLoincPropertyType] of Word;

  THolder = class (TCodeSystemProviderFilterContext)
  private
    ndx: integer;
    Children : LoincServices.TCardinalArray;
    function HasChild(v : integer) : boolean;
  end;

  TLOINCServices = class (TCodeSystemProvider)
  Private
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

    FRoot : Word;
    FLoaded: Boolean;
    FVersion: String;
    FKey: integer;
    function FindStem(s: String; var index: Integer): Boolean;
    function FilterByPropertyId(prop : TLoincPropertyType; op: TFhirFilterOperatorEnum; value: String): TCodeSystemProviderFilterContext;
    function FilterBySubset(op: TFhirFilterOperatorEnum; subset : TLoincSubsetId): TCodeSystemProviderFilterContext;
    function FilterByHeirarchy(op: TFhirFilterOperatorEnum; value: String; transitive: boolean): TCodeSystemProviderFilterContext;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function Link : TLOINCServices; Overload;

    Procedure Load(Const sFilename : String);
    Procedure Save(Const sFilename : String);
    Function GetDisplayByName(Const sCode : String) : String;
    procedure GetDisplaysByName(Const sCode : String; list : TStringList);
    Function Search(sText : String; all: boolean) : TMatchArray; overload;
    Function GetPropertyId(aType : TLoincPropertyType; const sName : String) : Word;
    Function GetPropertyCodes(iProp : Word) : TCardinalArray;
    Function GetConceptName(iConcept : Word): String;
    Function IsCode(sCode : String): Boolean;
    Function IsMACode(sCode : String): Boolean;

    Property Desc : TLoincStrings read FDesc;
    Property Refs : TLOINCReferences read FRefs;
    Property Concepts : TLOINCConcepts read FConcepts;
    Property CodeList : TLOINCCodeList read FCode;
    Property Words : TLoincWords read FWords;
    Property Stems : TLoincStems read FStems;
    Property Entries : TLOINCHeirarchyEntryList read FEntries;
    Property AnswerLists : TLOINCAnswersList read FAnswerLists;


    Property Root : Word read FRoot write FRoot;
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
    function getDisplay(code : String):String; override;
    function locate(code : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext) : string; override;
    procedure Displays(code : String; list : TStringList); override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList); override;
    function filter(prop : String; op : TFhirFilterOperatorEnum; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; overload; override;
    function buildValueSet(id : String) : TFhirValueSet;
    function getDefinition(code : String):String; override;
    function Definition(context : TCodeSystemProviderContext) : string; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;

  End;

  TLOINCServiceList = class (TAdvObjectList)
  Private
    FDefinition: TLOINCServices;
    function GetService(i: integer): TLOINCServices;
    procedure SetDefinition(const Value: TLOINCServices);
    function GetDefinition: TLOINCServices;
  Protected
    Function ItemClass : TAdvObjectClass; Override;
  Public
    Destructor Destroy; Override;

    Function GetByKey(sKey : String) : TLOINCServices;
    Function GetServiceByName(sName : String) : TLOINCServices;

    Function HasDefaultService : Boolean;

    Property DefaultService : TLOINCServices Read GetDefinition write SetDefinition;

    Property Service[i : integer] : TLOINCServices read GetService; Default;
  End;

function ascopy(s : TBytes; start, length : integer) : String;


Implementation

{ TLoincStrings }

function TLoincStrings.GetEntry(iIndex: Cardinal): String;
begin
  if iIndex = 0 Then
  begin
    result := '';
    exit;
  end;

  if (iIndex > FLength) then
    Raise Exception.Create('Wrong length index getting LOINC name');
  SetLength(Result, Word(FMaster[iIndex]));
  if (Byte(FMaster[iIndex]) + iIndex > FLength) then
    Raise Exception.Create('Wrong length index getting LOINC name (2)');
  if Byte(FMaster[iIndex]) > 0 Then
    Move(FMaster[iIndex+2], result[1], Length(Result)*2);
end;

function TLoincStrings.AddEntry(const s: String): Cardinal;
begin
  if Length(s) > 255 Then
    raise exception.Create('LOINC Description too long: '+s);
  result := FBuilder.Length;
  FBuilder.AddWord(Length(s));
  FBuilder.AddString(s);
end;

procedure TLoincStrings.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TLoincStrings.StartBuild;
begin
  FBuilder := TAdvBytesBuilder.Create;
end;

{ TLOINCReferences }

Function TLOINCReferences.GetWords(iIndex: Cardinal) : TWordArray;
var
  i : integer;
  lw : Cardinal;
begin
  if (iIndex > FLength) then
    Raise Exception.Create('Wrong length index getting LOINC list');
  if Byte(FMaster[iIndex]) <> 0 Then
    Raise exception.Create('not a word list');
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

Function TLOINCReferences.GetCardinals(iIndex: Cardinal) : TCardinalArray;
var
  i : integer;
  lw : cardinal;
begin
  if (iIndex > FLength) then
    Raise Exception.Create('Wrong length index getting LOINC list');
  if Byte(FMaster[iIndex]) <> 1 Then
    Raise exception.Create('not a cardinal list');
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
    raise exception.Create('LOINC reference list too long');
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
  FBuilder := TAdvBytesBuilder.Create;
end;

function TLOINCReferences.Getlength(iIndex: Cardinal): Cardinal;
begin
  if (iIndex > FLength) then
    Raise Exception.Create('Wrong length index getting LOINC list');
  inc(iIndex); // skip type marker
  move(FMaster[iIndex], result, 4);
end;

{ TLOINCConcepts }

Procedure TLOINCConcepts.GetConcept(iIndex : Word; var iName : Cardinal; var iChildren : Cardinal; var iConcepts : Cardinal);
begin
  if (iIndex >= FLength) then
    Raise Exception.Create('Wrong length index getting LOINC name');
  Move(FMaster[iIndex*12+0], iName, 4);
  Move(FMaster[iIndex*12+4], iChildren, 4);
  Move(FMaster[iIndex*12+8], iConcepts, 4);
end;

function TLOINCConcepts.AddConcept(iName : Cardinal; iChildren : Cardinal; iConcepts : Cardinal) : Word;
begin
  result := FBuilder.Length div 12;
  FBuilder.AddCardinal(iName);
  FBuilder.AddCardinal(iChildren);
  FBuilder.AddCardinal(iConcepts);
end;

procedure TLOINCConcepts.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster) div 12;
  FBuilder.Free;
end;

procedure TLOINCConcepts.StartBuild;
begin
  FBuilder := TAdvBytesBuilder.Create;
end;


{ TLOINCCodeList }

procedure TLOINCCodeList.StartBuild;
begin
  FBuilder := TAdvBytesBuilder.Create;
end;


Function TLOINCCodeList.AddCode(sCode : String; iDescription, iOtherNames, iEntry : Cardinal; iFlags : Byte) : Cardinal;
var
  s : AnsiString;
  i : integer;
begin
  Result := FBuilder.Length;
  Result := Result div (FCodeLength+35);
  SetLength(s, FCodeLength);
  FillChar(s[1], FCodeLength, 32);
  for I := 1 to length(sCode) do
    s[i] := AnsiChar(sCode[i]);
  FBuilder.AddAnsiString(s);
{00}  FBuilder.AddCardinal(iDescription);
{04}  FBuilder.AddCardinal(iOtherNames);
{08}  FBuilder.AddWord(0); // Component
{10}  FBuilder.AddWord(0); // Property
{12}  FBuilder.AddWord(0); // TimeAspect
{14}  FBuilder.AddWord(0); // System
{16}  FBuilder.AddWord(0); // Scale
{18}  FBuilder.AddWord(0); // Method
{20}  FBuilder.AddWord(0); // Class
{22}  FBuilder.Append(iFlags);
{23}  FBuilder.AddCardinal(0); // stems
{27}  FBuilder.AddCardinal(iEntry);
end;

procedure TLOINCCodeList.SetCodeLength(const Value: Cardinal);
begin
  FCodeLength := Value;
  FRecLength := FCodeLength+31;
end;

Procedure TLOINCCodeList.SetComponent(iIndex : Cardinal; iValue : Word);
Begin
  Move(iValue, FMaster[iIndex*(FRecLength) +FCodeLength+8], 2);
End;

Procedure TLOINCCodeList.SetProperty(iIndex : Cardinal; iValue : Word);
Begin
  Move(iValue, FMaster[iIndex*FRecLength+FCodeLength+10], 2);
End;

Procedure TLOINCCodeList.SetTimeAspect(iIndex : Cardinal; iValue : Word);
Begin
  Move(iValue, FMaster[iIndex*FRecLength+FCodeLength+12], 2);
End;

Procedure TLOINCCodeList.SetSystem(iIndex : Cardinal; iValue : Word);
Begin
  Move(iValue, FMaster[iIndex*FRecLength+FCodeLength+14], 2);
End;

Procedure TLOINCCodeList.SetScale(iIndex : Cardinal; iValue : Word);
Begin
  Move(iValue, FMaster[iIndex*FRecLength+FCodeLength+16], 2);
End;

Procedure TLOINCCodeList.SetMethod(iIndex : Cardinal; iValue : Word);
Begin
  Move(iValue, FMaster[iIndex*FRecLength+FCodeLength+18], 2);
End;

Procedure TLOINCCodeList.SetClass(iIndex : Cardinal; iValue : Word);
Begin
  Move(iValue, FMaster[iIndex*FRecLength+FCodeLength+20], 2);
End;


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


Procedure TLOINCCodeList.GetInformation(iIndex: Cardinal; var sCode : String; var iDescription, iOtherNames, iEntry, iStems : Cardinal; var iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Word; var iFlags : Byte);
Begin
  if iIndex > FLength div FRecLength - 1 Then
    Raise Exception.Create('Attempt to access invalid LOINC index at '+inttostr(iIndex*FRecLength));
  sCode := trim(asCopy(FMaster, iIndex*FRecLength, FCodeLength));
  Move(FMaster[(iIndex*FRecLength)+FCodeLength+0], iDescription, 4);
  Move(FMaster[(iIndex*FRecLength)+FCodeLength+4], iOtherNames, 4);
  Move(FMaster[(iIndex*FRecLength)+FCodeLength+8], iComponent, 2);
  Move(FMaster[(iIndex*FRecLength)+FCodeLength+10], iProperty, 2);
  Move(FMaster[(iIndex*FRecLength)+FCodeLength+12], iTimeAspect, 2);
  Move(FMaster[(iIndex*FRecLength)+FCodeLength+14], iSystem, 2);
  Move(FMaster[(iIndex*FRecLength)+FCodeLength+16], iScale, 2);
  Move(FMaster[(iIndex*FRecLength)+FCodeLength+18], iMethod, 2);
  Move(FMaster[(iIndex*FRecLength)+FCodeLength+20], iClass, 2);
  Move(FMaster[(iIndex*FRecLength)+FCodeLength+22], iFlags, 1);
  Move(FMaster[(iIndex*FRecLength)+FCodeLength+23], iStems, 4);
  Move(FMaster[(iIndex*FRecLength)+FCodeLength+37], iEntry, 4);
end;

function TLOINCCodeList.Count: Integer;
begin
  result := FLength div FRecLength;
end;

procedure TLOINCCodeList.SetStems(iIndex, iValue: Cardinal);
begin
  Move(iValue, FMaster[iIndex*FRecLength+FCodeLength+27], 4);
end;

{ TLOINCServices }

constructor TLOINCServices.Create;
begin
  inherited;
  FDesc := TLoincStrings.Create;
  FCode := TLOINCCodeList.Create;
  FRefs := TLOINCReferences.Create;
  FConcepts := TLOINCConcepts.Create;
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
  FCode.Free;
  inherited;
end;

function TLOINCServices.GetConceptName(iConcept: Word): String;
var
  iName : Cardinal;
  iChildren : Cardinal;
  iCodes : Cardinal;
begin
  Concepts.GetConcept(iConcept, iName, iChildren, iCodes);
  result := Desc.GetEntry(iName);
end;

procedure TLOINCServices.GetDisplaysByName(Const sCode : String; list : TStringList);
var
  iIndex : Cardinal;
  iDescription, iStems, iOtherNames : Cardinal;
  iEntry : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Word;
  iFlags : Byte;
  names : TCardinalArray;
  name : Cardinal;
begin
  if CodeList.FindCode(sCode, iIndex) then
  Begin
    CodeList.GetInformation(iIndex, sCode1, iDescription, iOtherNames, iEntry, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);
    assert(sCode = sCode1);
    list.Add(Desc.GetEntry(iDescription));
    if iOtherNames <> 0 then
    begin
      names := FRefs.GetCardinals(iOtherNames);
      for name in names do
        list.Add(Desc.GetEntry(name));
    end;
  End
end;


function TLOINCServices.GetDisplayByName(const sCode: String): String;
var
  iIndex : Cardinal;
  iDescription, iStems, iOtherNames : Cardinal;
  iEntry : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;
begin
  if CodeList.FindCode(sCode, iIndex) then
  Begin
    CodeList.GetInformation(iIndex, sCode1, iDescription, iOtherNames, iEntry, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);
    assert(sCode = sCode1);
    result := Desc.GetEntry(iDescription)
  End
  Else
    result := '';
end;

function TLOINCServices.GetPropertyCodes(iProp: Word): TCardinalArray;
var
  iName : Cardinal;
  iChildren : Cardinal;
  iCodes : Cardinal;
begin
  Concepts.GetConcept(iProp, iName, iChildren, iCodes);
  result := Refs.GetCardinals(iCodes);
end;

function TLOINCServices.GetPropertyId(aType: TLoincPropertyType; const sName: String): Word;
var
  iName : Cardinal;
  iChildren : Cardinal;
  iCodes : Cardinal;
  aChildren : TWordArray;
  i : Integer;
  iDummy : Cardinal;

  {
  b2, b3, b4 : Boolean;

  iDescription, iOtherNames : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;
}

begin
  // get all the children of the property code
  // iterate them looking for a name match
  Concepts.GetConcept(Properties[aType], iName, iChildren, iCodes);
  aChildren := Refs.GetWords(iChildren);
  result := 0;
  For i := 0 to High(aChildren) Do
  Begin
    Concepts.GetConcept(aChildren[i], iName, iChildren, iDummy);
    if SameText(sName, Desc.GetEntry(iName)) Then
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
    oread.Read(result[0], length(result));
  end;
begin
  oFile := TFileStream.Create(sFilename, fmOpenread);
  try
    oread := TReader.Create(oFile, 8192);
    try
      if oRead.ReadString <> LOINC_CACHE_VERSION Then
        raise exception.create('the LOINC cache must be rebuilt using the ''Import LOINC'' operation in the manager application.');
      FCode.CodeLength := oRead.ReadInteger;
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

procedure TLOINCServices.Save(const sFilename: String);
var
  oFile : Tfilestream;
  oWrite : TWriter;
  aLoop : TLoincPropertyType;
  a : TLoincSubsetId;
  i : integer;
  procedure WriteBytes(b : TBytes);
  begin
   oWrite.WriteInteger(length(b));
   oWrite.Write(b[0], length(b));
  end;
begin
  if FileExists(sFilename) Then
  begin
    FileSetReadOnlyAttribute(sFilename, False);
    DeleteFile(sFilename);
  End;
  oFile := TFileStream.Create(sFilename, fmCreate);
  try
    oWrite := TWriter.Create(oFile, 8192);
    try
      oWrite.WriteString(LOINC_CACHE_VERSION);
      oWrite.WriteInteger(FCode.CodeLength);
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
    iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
    iFlags : Byte;
    matches : integer;
    ok : boolean;
  Begin
    CodeList.GetInformation(iCodeIndex, sCode1, iDescription, iOtherNames, iEntry, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);
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
          end
          else
            assert(FDesc.GetEntry(words[i].stem) <> FDesc.GetEntry(desc[j]));
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
    code, text, parent, children, descendents, concepts, descendentConcepts, stems: Cardinal;
    r1 : Double;
    Desc : TCardinalArray;
    matches : integer;
    ok : boolean;
  Begin
    Entries.GetEntry(index, code, text, parent, children, descendents, concepts, descendentConcepts, stems);
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
            assert(FDesc.GetEntry(words[i].stem) <> FDesc.GetEntry(desc[j]));
        end;
        if ok then
          inc(matches);
      end;
    end;

    if (not all or (matches = length(words))) and (r1  > 0) Then
      AddResult(iCount, index, FDesc.GetEntry(code), 1000000+r1, false); // these always come first
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
    Raise Exception.Create('no usable search text found');


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
      Code.GetInformation(i, sCode1, iDescription, iOtherNames, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);

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
    Raise Exception.Create('There is no default LOINC service');
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

function TLOINCServiceList.ItemClass: TAdvObjectClass;
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
    raise Exception.create('invalid index');
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
  FBuilder := TAdvBytesBuilder.Create;
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
    raise Exception.create('invalid index');
  move(FMaster[l], index, 4);
end;

function TLoincStems.GetString(iIndex: Cardinal): Cardinal;
begin
  GetEntry(iIndex, result);
end;

procedure TLoincStems.StartBuild;
begin
  FBuilder := TAdvBytesBuilder.Create;
end;

function TLoincServices.FindStem(s: String; var index: Integer): Boolean;
var
  L, H, I, c: Integer;
begin
  Result := False;
  L := 0;
  H := FStems.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStr(FDesc.GetEntry(FStems.GetString(i)), s);
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


function TLOINCServices.buildValueSet(id: String): TFhirValueSet;
var
  index : cardinal;
  code, text, parent, children, descendents, concepts, descendentConcepts, stems: Cardinal;
  answers : TCardinalArray;
  inc : TFhirValueSetComposeInclude;
  filt :  TFhirValueSetComposeIncludeFilter;
  i : integer;
  cc : TFhirValueSetComposeIncludeConcept;
begin
  result := nil;
  if (id.StartsWith('http://loinc.org/vs/') and FEntries.FindCode(id.Substring(20), index, FDesc)) then
  begin
    FEntries.GetEntry(index, code, text, parent, children, descendents, concepts, descendentConcepts, stems);

    result := TFhirValueSet.Create;
    try
      result.url := id;
      result.status := ConformanceResourceStatusActive;
      result.version := Version(nil);
      result.name := 'LOINC Value Set from Multi-Axial Heirarchy term '+id.Substring(20);
      result.description := 'All LOINC codes for '+Desc.GetEntry(text);
      result.date := NowUTC;
      result.compose := TFhirValueSetCompose.Create;
      inc := result.compose.includeList.Append;
      inc.system := 'http://loinc.org';
      filt := inc.filterList.Append;
      filt.property_ := 'ancestor';
      filt.op := FilterOperatorEqual;
      filt.value := id.Substring(20);
      result.link;
    finally
      result.free;
    end;
  end
  else if (id.StartsWith('http://loinc.org/vs/') and FAnswerLists.FindCode(id.Substring(20), index, FDesc)) then
  begin
    FAnswerLists.GetEntry(index, code, text, children);
    result := TFhirValueSet.Create;
    try
      result.url := id;
      result.status := ConformanceResourceStatusActive;
      result.version := Version(nil);
      result.name := 'LOINC Answer List '+id.Substring(20);
      result.description := 'LOINC Answer list for '+Desc.GetEntry(text);
      result.date := NowUTC;
      result.compose := TFhirValueSetCompose.Create;
      inc := result.compose.includeList.Append;
      inc.system := 'http://loinc.org';
      answers := FRefs.GetCardinals(children);
      for i := 0 to Length(answers) - 1 do
      begin
        cc := inc.conceptList.Append;
        FAnswerLists.GetEntry(answers[i], code, text, children);
        cc.code := Desc.GetEntry(code);
        cc.display := Desc.GetEntry(text);
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
    raise exception.create('shouldn''t be here');
end;

function TLoincServices.Code(context: TCodeSystemProviderContext): string;
var
  index : integer;
  iDescription, iStems, iOtherNames : Cardinal;
  iEntry, iCode, iOther : Cardinal;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Word;
  iFlags : Byte;
begin
  index := integer(context)-1;
  if index > CodeList.Count then
  begin
    FAnswerLists.GetEntry(index - CodeList.Count, iCode, iDescription, iOther);
    result := Desc.GetEntry(iCode);
  end
  else
    CodeList.GetInformation(integer(context)-1, result, iDescription, iOtherNames, iStems, iEntry, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);
end;

function TLoincServices.Display(context: TCodeSystemProviderContext): string;
var
  index : integer;
  iCode, iDescription, iStems, iOtherNames, iOther : Cardinal;
  iEntry : Cardinal;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass : Word;
  iFlags : Byte;
begin
  index := integer(context)-1;
  if index > CodeList.Count then
    FAnswerLists.GetEntry(index - CodeList.Count, iCode, iDescription, iOther)
  else
    CodeList.GetInformation(index, result, iDescription, iOtherNames, iEntry, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iFlags);
  result := Desc.GetEntry(iDescription);
end;

procedure TLOINCServices.Displays(context: TCodeSystemProviderContext; list: TStringList);
begin
  GetDisplaysByName(Code(context), list);
end;

procedure TLoincServices.Displays(code: String; list: TStringList);
begin
  GetDisplaysByName(code, list);
end;


function TLOINCServices.getDefinition(code: String): String;
begin
  result := '';
end;

function TLoincServices.getDisplay(code: String): String;
begin
  result := GetDisplayByName(code);
  if result = '' then
    raise Exception.create('unable to find '+code+' in '+system(nil));
end;

function TLoincServices.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  result := false; // loinc don't do abstract
end;

function TLoincServices.locate(code: String): TCodeSystemProviderContext;
var
  i : Cardinal;
begin
  if CodeList.FindCode(code, i) then
    result := TCodeSystemProviderContext(i+1)
  else if AnswerLists.FindCode(code, i, FDesc) then
    result := TCodeSystemProviderContext(cardinal(CodeList.Count)+ i+1)
  else
    result := nil;//raise Exception.create('unable to find '+code+' in '+system);
end;

function TLoincServices.system(context : TCodeSystemProviderContext): String;
begin
  result := 'http://loinc.org';
end;

function TLoincServices.TotalCount: integer;
begin
  result := CodeList.Count;
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

function TLoincServices.FilterByPropertyId(prop : TLoincPropertyType; op: TFhirFilterOperatorEnum; value: String): TCodeSystemProviderFilterContext;
var
  id : word;
  iName, iChildren, iCodes : Cardinal;
  aChildren : LoincServices.TCardinalArray;
begin
  if op <> FilterOperatorEqual then
    raise Exception.Create('Unsupported operator type '+CODES_TFhirFilterOperatorEnum[op]);

  result := THolder.create;
  try
    id := GetPropertyId(prop, value);
    if (id <> 0) then
    begin
      Concepts.GetConcept(id, iName, iChildren, iCodes);
      aChildren := Refs.GetCardinals(iCodes);
      THolder(result).Children := aChildren;
    end;
    result.link;
  finally
    result.free;
  end;
end;

// this is a rare operation. But even so, is it worth pre-calculating this one on import?
function TLoincServices.FilterBySubset(op: TFhirFilterOperatorEnum; subset : TLoincSubsetId): TCodeSystemProviderFilterContext;
begin
  if op <> FilterOperatorEqual then
    raise Exception.Create('Unsupported operator type '+CODES_TFhirFilterOperatorEnum[op]);

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

function TLoincServices.FilterByHeirarchy(op: TFhirFilterOperatorEnum; value: String; transitive : boolean): TCodeSystemProviderFilterContext;
var
  index : Cardinal;
  aChildren, c : LoincServices.TCardinalArray;
  code, text, parent, children, descendents, concepts, descendentConcepts, stems: Cardinal;
  s : String;
begin
  result := THolder.create;
  try
    if (op = FilterOperatorEqual) and (value.Contains(',')) then
      raise Exception.Create('Value is illegal - no commas');
    if (not (op in [FilterOperatorEqual, FilterOperatorIn])) then
      raise Exception.Create('Unsupported operator type '+CODES_TFhirFilterOperatorEnum[op]);

    while (value <> '') do
    begin
      StringSplit(value, ',', s, value);
      if (FEntries.FindCode(s, index, FDesc)) then
      begin
        FEntries.GetEntry(index, code, text, parent, children, descendents, concepts, descendentConcepts, stems);
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

function TLoincServices.filter(prop: String; op: TFhirFilterOperatorEnum; value: String; prep : TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
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

function TLoincServices.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String): TCodeSystemProviderContext;
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
  FBuilder := TAdvBytesBuilder.Create;
end;

function TLOINCHeirarchyEntryList.AddEntry(code, text, parent, children, descendents, concepts, descendentConcepts: Cardinal): Cardinal;
begin
  Result := FBuilder.Length div 32;
  FBuilder.AddCardinal(code);
  FBuilder.AddCardinal(text);
  FBuilder.AddCardinal(children);
  FBuilder.AddCardinal(descendents);
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
      s := Strings.GetEntry(d);
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

procedure TLOINCHeirarchyEntryList.GetEntry(iIndex: Cardinal; var code, text, parent, children, descendents, concepts, descendentConcepts, stems: Cardinal);
begin
  if iIndex > (Length(FMaster) div 32) Then
    Raise Exception.Create('Attempt to access invalid LOINC Entry index');
  Move(FMaster[(iIndex*32)+0], code, 4);
  Move(FMaster[(iIndex*32)+4], text, 4);
  Move(FMaster[(iIndex*32)+8], children, 4);
  Move(FMaster[(iIndex*32)+12], descendents, 4);
  Move(FMaster[(iIndex*32)+16], concepts, 4);
  Move(FMaster[(iIndex*32)+20], descendentConcepts, 4);
  Move(FMaster[(iIndex*32)+24], parent, 4);
  Move(FMaster[(iIndex*32)+28], stems, 4);
end;

function ascopy(s : TBytes; start, length : integer) : String;
var
  res : AnsiString;
begin
  SetLength(res, length);
  move(s[start], res[1], length);
  result := String(res);
end;


{ TLOINCAnswersList }

function TLOINCAnswersList.AddEntry(code, description, answers: Cardinal): Cardinal;
begin
  Result := FBuilder.Length div 12;
  FBuilder.AddCardinal(code);
  FBuilder.AddCardinal(description);
  FBuilder.AddCardinal(answers);
end;

function TLOINCAnswersList.Count: Integer;
begin
  result := Length(FMaster) div 12;
end;

procedure TLOINCAnswersList.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FBuilder.Free;
end;

function TLOINCAnswersList.FindCode(sCode: String; var iIndex: Cardinal; Strings: TLoincStrings): Boolean;
var
  L, H, I, d : Cardinal;
  C: Integer;
  s : String;
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
      Move(FMaster[i*12], d, 4);
      s := Strings.GetEntry(d);
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
    Raise Exception.Create('Attempt to access invalid LOINC Entry index');
  Move(FMaster[(iIndex*12)+0], code, 4);
  Move(FMaster[(iIndex*12)+4], description, 4);
  Move(FMaster[(iIndex*12)+8], answers, 4);
end;

procedure TLOINCAnswersList.StartBuild;
begin
  FBuilder := TAdvBytesBuilder.Create;
end;

End.

