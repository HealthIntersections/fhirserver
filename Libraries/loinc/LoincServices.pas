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
  SysUtils,
  Classes,
  StringSupport,
  FileSupport,
  AdvStringBuilders,
  AdvObjects,
  AdvObjectLists,
  regexpr,
  YuStemmer;

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
Const LOINC_CACHE_VERSION = '6';

Type
  TLoincPropertyType = (lptComponents, lptProperties, lptTimeAspects, lptSystems, lptScales, lptMethods, lptClasses);

type
  // We store LOINC as four structures.
  //   the first structure is a simply a list of strings which are variable length names - referred to from the other structures
  //   the second structure is a list of lists of word or cardinal references.
  //   the third structure is a list of concepts. each concept has a refernce to a name and a contained list of references which are either children
  //   the fourth structure is the code list - a list of loinc concepts, with codes and references to names and properties

  // 1. a list of strings
  //   each entry in the AnsiString starts with a byte length, and then the series of characters
  // we store loinc descriptions, and other names in here
  TLoincStrings = class (TAdvObject)
    Private
      FMaster : AnsiString;
      FLength : Cardinal;
      FBuilder : TAdvStringBuilder;
    Public
      Function GetEntry(iIndex : Cardinal):AnsiString;

      Procedure StartBuild;
      Function AddEntry(Const s : AnsiString) : Cardinal;
      Procedure DoneBuild;
  End;

  TWordArray = array of word;
  TCardinalArray = array of Cardinal;
  TMatch = record
    index : cardinal;
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
      FMaster : AnsiString;
      FLength : Cardinal;
      FBuilder : TAdvStringBuilder;
   Public
      Procedure GetEntry(iIndex : Cardinal; var index : Cardinal; var flags : Byte);
      Function Count : Integer;
      Function GetString(iIndex : Cardinal) : Cardinal;

      Procedure StartBuild;
      Procedure AddWord(index : Cardinal; Flags : Byte);
      Procedure DoneBuild;
  End;

  // stem word index. Every word is 8 bytes - a 4 byte index into the strings, and a 4 byte index into the references
  TLoincStems = class (TAdvObject)
    Private
      FMaster : AnsiString;
      FLength : Cardinal;
      FBuilder : TAdvStringBuilder;
   Public
      Procedure GetEntry(iIndex : Cardinal; var index : Cardinal; var reference : Cardinal);
      Function Count : Integer;
      Function GetString(iIndex : Cardinal) : Cardinal;

      Procedure StartBuild;
      Procedure AddStem(index, reference : Cardinal);
      Procedure DoneBuild;
  End;


  // 2. a list of list of references
  TLOINCReferences = class (TAdvObject)
    Private
      FMaster : AnsiString;
      FLength : Cardinal;
      FBuilder : TAdvStringBuilder;
    Public
      Function GetWords(iIndex : Cardinal) : TWordArray;
      Function GetCardinals(iIndex : Cardinal) : TCardinalArray;
      Function Getlength(iIndex : Cardinal) : Word;
      
      Procedure StartBuild;
      Function AddWords(Const a : TWordArray) : Cardinal;
      Function AddCardinals(Const a : TCardinalArray) : Cardinal;
      Procedure DoneBuild;
  End;

  // 3. a list of concepts
  TLOINCConcepts = class (TAdvObject)
    Private
      FMaster : AnsiString;
      FLength : Cardinal;
      FBuilder : TAdvStringBuilder;
    Public
      Procedure GetConcept(iIndex : Word; var iName : Cardinal; var iChildren : Cardinal; var iConcepts : Cardinal);

      Procedure StartBuild;
      Function AddConcept(iName : Cardinal; iChildren : Cardinal; iConcepts : Cardinal) : Word;
      Procedure DoneBuild;

  End;

  // 4. the master code reference.
  // it is a list of codes and the offset reference to their description
  // codes are stored in alphabetical order so you can do a binary serach for Code identity
  // This is the effective logical structure of the AnsiString:
//  TLOINCCode = {private} packed record
//    Code : ShortString[FCodeLength]; padded with spaces
//    Description : Cardinal;
//    other names : cardinal
//    component : word
//    Property : word
//    TimeAspect : word
//    System : word
//    Scale : word
//    Method : word
//    Class : word
//    v2dt : word
//    v3dt : word
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
      FCodeLength : Cardinal;
      FMaster : AnsiString;
      FLength : Cardinal;
      FBuilder : TAdvStringBuilder;
    Public
      Function FindCode(sCode : String; var iIndex : Cardinal) : Boolean;

      Procedure GetInformation(iIndex: Cardinal; var sCode : String; var iDescription, iOtherNames, iStems : Cardinal; var iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word; var iFlags : Byte);

      // we presume that the Codes are registered in order
      Procedure StartBuild;
      Function AddCode(sCode : String; iDescription, iOtherNames : Cardinal; iv2dt, iv3dt : Word; iFlags : Byte) : Cardinal;
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
      Property CodeLength : Cardinal read FCodeLength Write FCodeLength;
  End;

  TLoincPropertyIds = Array [TLoincPropertyType] of Word;

  TLOINCServices = class (TAdvObject)
  Private
    FDesc : TLoincStrings;
    FCode : TLOINCCodeList;
    FRefs : TLOINCReferences;
    FConcepts : TLOINCConcepts;
    FProperties : TLoincPropertyIds;
    FWords : TLoincWords;
    FStems : TLoincStems;

    FRoot : Word;
    FLoaded: Boolean;
    FVersion: String;
    FKey: integer;
    function FindStem(s: String; var index: Integer): Boolean;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function Link : TLOINCServices; Overload;

    Procedure Load(Const sFilename : String);
    Procedure Save(Const sFilename : String);
    Function GetDisplayByName(Const sCode : String) : String;
    Function Search(sText : String) : TMatchArray;
    Function GetPropertyId(aType : TLoincPropertyType; const sName : String) : Word;
    Function GetPropertyCodes(iProp : Word) : TCardinalArray;
    Function GetConceptName(iConcept : Word): String;

    Property Desc : TLoincStrings read FDesc;
    Property Refs : TLOINCReferences read FRefs;
    Property Concepts : TLOINCConcepts read FConcepts;
    Property Code : TLOINCCodeList read FCode;
    Property Words : TLoincWords read FWords;
    Property Stems : TLoincStems read FStems;

    Property Root : Word read FRoot write FRoot;
    Property Loaded : Boolean read FLoaded write FLoaded;
    Property Version : String read FVersion write FVersion;
    Property Properties : TLoincPropertyIds read FProperties Write FProperties;
    Property Key : integer read FKey write FKey;
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

var
  GLOINCs : TLOINCServiceList;

Implementation

{ TLoincStrings }

function TLoincStrings.GetEntry(iIndex: Cardinal): AnsiString;
begin
  if iIndex = 0 Then
  begin
    result := '';
    exit;
  end;

  if (iIndex > FLength) then
    Raise Exception.Create('Wrong length index getting LOINC name');
  SetLength(Result, Byte(FMaster[iIndex]));
  if (Byte(FMaster[iIndex]) + iIndex > FLength) then
    Raise Exception.Create('Wrong length index getting LOINC name (2)');
  if Byte(FMaster[iIndex]) > 0 Then
    Move(FMaster[iIndex+1], result[1], Length(Result));
end;

function TLoincStrings.AddEntry(const s: AnsiString): Cardinal;
begin
  if Length(s) > 255 Then
    raise exception.Create('LOINC Description too long: '+s);
  result := FBuilder.Length + 1;
  FBuilder.Append(Chr(Length(s)));
  FBuilder.Append(s);
end;

procedure TLoincStrings.DoneBuild;
begin
  FMaster := FBuilder.AsString;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TLoincStrings.StartBuild;
begin
  FBuilder := TAdvStringBuilder.Create;
end;

{ TLOINCReferences }

Function TLOINCReferences.GetWords(iIndex: Cardinal) : TWordArray;
var
  i : integer;
  w : word;
begin
  if (iIndex > FLength) then
    Raise Exception.Create('Wrong length index getting LOINC list');
  if Byte(FMaster[iIndex]) <> 0 Then
    Raise exception.Create('not a word list');
  inc(iIndex);
  move(FMaster[iIndex], w, 2);
  SetLength(Result, w);
  inc(iIndex, 2);
  for i := 0 to Length(result)-1 Do
  Begin
    move(FMaster[iIndex], result[i], 2);
    inc(iIndex, 2);
  End;
end;

Function TLOINCReferences.GetCardinals(iIndex: Cardinal) : TCardinalArray;
var
  i : integer;
  w : word;
begin
  if (iIndex > FLength) then
    Raise Exception.Create('Wrong length index getting LOINC list');
  if Byte(FMaster[iIndex]) <> 1 Then
    Raise exception.Create('not a cardinal list');
  inc(iIndex);
  move(FMaster[iIndex], w, 2);
  SetLength(Result, w);
  inc(iIndex, 2);
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
  result := FBuilder.Length + 1;
  FBuilder.Append(Chr(0));// for words
  FBuilder.AddWordAsBytes(length(a));
  for iLoop := Low(a) to High(a) Do
    FBuilder.AddWordAsBytes(a[iLoop]);
End;

Function TLOINCReferences.AddCardinals(Const a : TCardinalArray) : Cardinal;
var
  iLoop : Integer;
Begin
  if Length(a) > 65535 Then
    raise exception.Create('LOINC referece list too long');
  result := FBuilder.Length + 1;
  FBuilder.Append(Chr(1));// for Cardinals
  FBuilder.AddWordAsBytes(length(a));
  for iLoop := Low(a) to High(a) Do
    FBuilder.AddCardinalAsBytes(a[iLoop]);
End;

procedure TLOINCReferences.DoneBuild;
begin
  FMaster := FBuilder.AsString;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TLOINCReferences.StartBuild;
begin
  FBuilder := TAdvStringBuilder.Create;
end;

function TLOINCReferences.Getlength(iIndex: Cardinal): Word;
begin
  if (iIndex > FLength) then
    Raise Exception.Create('Wrong length index getting LOINC list');
  inc(iIndex); // skip type marker
  move(FMaster[iIndex], result, 2);
end;

{ TLOINCConcepts }

Procedure TLOINCConcepts.GetConcept(iIndex : Word; var iName : Cardinal; var iChildren : Cardinal; var iConcepts : Cardinal);
begin
  if (iIndex >= FLength) then
    Raise Exception.Create('Wrong length index getting LOINC name');
  Move(FMaster[iIndex*12+1], iName, 4);
  Move(FMaster[iIndex*12+5], iChildren, 4);
  Move(FMaster[iIndex*12+9], iConcepts, 4);
end;

function TLOINCConcepts.AddConcept(iName : Cardinal; iChildren : Cardinal; iConcepts : Cardinal) : Word;
begin
  result := FBuilder.Length div 12;
  FBuilder.AddCardinalAsBytes(iName);
  FBuilder.AddCardinalAsBytes(iChildren);
  FBuilder.AddCardinalAsBytes(iConcepts);
end;

procedure TLOINCConcepts.DoneBuild;
begin
  FMaster := FBuilder.AsString;
  FLength := Length(FMaster) div 12;
  FBuilder.Free;
end;

procedure TLOINCConcepts.StartBuild;
begin
  FBuilder := TAdvStringBuilder.Create;
end;


{ TLOINCCodeList }

procedure TLOINCCodeList.StartBuild;
begin
  FBuilder := TAdvStringBuilder.Create;
end;


Function TLOINCCodeList.AddCode(sCode : String; iDescription, iOtherNames : Cardinal; iv2dt, iv3dt : Word; iFlags : Byte) : Cardinal;
begin
  Result := FBuilder.Length;
  Result := Result div (FCodeLength+31);
  FBuilder.Append(StringPadRight(sCode, ' ', FCodeLength));
{00}  FBuilder.AddCardinalAsBytes(iDescription);
{04}  FBuilder.AddCardinalAsBytes(iOtherNames);
{08}  FBuilder.AddWordAsbytes(0); // Component
{10}  FBuilder.AddWordAsbytes(0); // Property
{12}  FBuilder.AddWordAsbytes(0); // TimeAspect
{14}  FBuilder.AddWordAsbytes(0); // System
{16}  FBuilder.AddWordAsbytes(0); // Scale
{18}  FBuilder.AddWordAsbytes(0); // Method
{20}  FBuilder.AddWordAsbytes(0); // Class
{22}  FBuilder.AddWordAsbytes(iv2dt);
{24}  FBuilder.AddWordAsbytes(iv3dt);
{26}  FBuilder.Append(chr(iFlags));
{27}  FBuilder.AddCardinalAsBytes(0); // stems
end;

Procedure TLOINCCodeList.SetComponent(iIndex : Cardinal; iValue : Word);
Begin
  Move(iValue, FMaster[iIndex*(FCodeLength+31) +1+FCodeLength+8], 2);
End;

Procedure TLOINCCodeList.SetProperty(iIndex : Cardinal; iValue : Word);
Begin
  Move(iValue, FMaster[iIndex*(FCodeLength+31)+1+FCodeLength+10], 2);
End;

Procedure TLOINCCodeList.SetTimeAspect(iIndex : Cardinal; iValue : Word);
Begin
  Move(iValue, FMaster[iIndex*(FCodeLength+31)+1+FCodeLength+12], 2);
End;

Procedure TLOINCCodeList.SetSystem(iIndex : Cardinal; iValue : Word);
Begin
  Move(iValue, FMaster[iIndex*(FCodeLength+31)+1+FCodeLength+14], 2);
End;

Procedure TLOINCCodeList.SetScale(iIndex : Cardinal; iValue : Word);
Begin
  Move(iValue, FMaster[iIndex*(FCodeLength+31)+1+FCodeLength+16], 2);
End;

Procedure TLOINCCodeList.SetMethod(iIndex : Cardinal; iValue : Word);
Begin
  Move(iValue, FMaster[iIndex*(FCodeLength+31)+1+FCodeLength+18], 2);
End;

Procedure TLOINCCodeList.SetClass(iIndex : Cardinal; iValue : Word);
Begin
  Move(iValue, FMaster[iIndex*(FCodeLength+31)+1+FCodeLength+20], 2);
End;


procedure TLOINCCodeList.DoneBuild;
begin
  FMaster := FBuilder.AsString;
  FBuilder.Free;
end;

{$Q-}

function TLOINCCodeList.FindCode(sCode : String; var IIndex: Cardinal): boolean;
var
  L, H, I : Cardinal;
  C: Integer;
  s, sF : String;
begin
  if FMaster = '' Then
    Result := False
  Else
  Begin
    s := StringPadRight(sCode, ' ', FCodeLength);
    Result := False;
    L := 0;
    H := (FLength div (FCodeLength+31)) - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      sF := Copy(FMaster, i*(FCodeLength + 31)+1, FCodeLength);
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


Procedure TLOINCCodeList.GetInformation(iIndex: Cardinal; var sCode : String; var iDescription, iOtherNames, iStems : Cardinal; var iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word; var iFlags : Byte);
Begin
  if iIndex > FLength div (FCodeLength+31) - 1 Then
    Raise Exception.Create('Attempt to access invalid LOINC index');
  sCode := trim(Copy(FMaster, iIndex*(FCodeLength+31)+1, FCodeLength));
  Move(FMaster[(iIndex*(FCodeLength+31))+1+FCodeLength+0], iDescription, 4);
  Move(FMaster[(iIndex*(FCodeLength+31))+1+FCodeLength+4], iOtherNames, 4);
  Move(FMaster[(iIndex*(FCodeLength+31))+1+FCodeLength+8], iComponent, 2);
  Move(FMaster[(iIndex*(FCodeLength+31))+1+FCodeLength+10], iProperty, 2);
  Move(FMaster[(iIndex*(FCodeLength+31))+1+FCodeLength+12], iTimeAspect, 2);
  Move(FMaster[(iIndex*(FCodeLength+31))+1+FCodeLength+14], iSystem, 2);
  Move(FMaster[(iIndex*(FCodeLength+31))+1+FCodeLength+16], iScale, 2);
  Move(FMaster[(iIndex*(FCodeLength+31))+1+FCodeLength+18], iMethod, 2);
  Move(FMaster[(iIndex*(FCodeLength+31))+1+FCodeLength+20], iClass, 2);
  Move(FMaster[(iIndex*(FCodeLength+31))+1+FCodeLength+22], iv2dt, 2);
  Move(FMaster[(iIndex*(FCodeLength+31))+1+FCodeLength+24], iv3dt, 2);
  Move(FMaster[(iIndex*(FCodeLength+31))+1+FCodeLength+26], iFlags, 1);
  Move(FMaster[(iIndex*(FCodeLength+31))+1+FCodeLength+27], iStems, 4);
end;

function TLOINCCodeList.Count: Integer;
begin
  result := FLength div (FCodeLength+31);
end;

procedure TLOINCCodeList.SetStems(iIndex, iValue: Cardinal);
begin
  Move(iValue, FMaster[iIndex*(FCodeLength+31)+1+FCodeLength+27], 4);
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
end;

destructor TLOINCServices.Destroy;
begin
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

function TLOINCServices.GetDisplayByName(const sCode: String): String;
var
  iIndex : Cardinal;
  iDescription, iStems, iOtherNames : Cardinal;
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;
begin
  if Code.FindCode(sCode, iIndex) then
  Begin
    Code.GetInformation(iIndex, sCode1, iDescription, iOtherNames, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);
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


function TLOINCServices.Link: TLOINCServices;
begin
  result := TLOINCServices(Inherited Link);
end;

procedure TLOINCServices.Load(const sFilename: String);
var
  oFile : Tfilestream;
  oread : TReader;
  aLoop : TLoincPropertyType;
begin
  oFile := TFileStream.Create(sFilename, fmOpenread);
  try
    oread := TReader.Create(oFile, 8192);
    try
      if oRead.ReadString <> LOINC_CACHE_VERSION Then
        raise exception.create('the LOINC cache must be rebuilt using the ''Import LOINC'' operation in the manager application.');
      FCode.CodeLength := oRead.ReadInteger;
      FCode.FMaster := oread.ReadString;
      FCode.FLength := Length(FCode.FMaster);
      FDesc.FMaster := oread.ReadString;
      FDesc.FLength := Length(FDesc.FMaster);
      FRefs.FMaster := oread.ReadString;
      FRefs.FLength := Length(FRefs.FMaster);
      FConcepts.FMaster := oread.ReadString;
      FConcepts.FLength := Length(FConcepts.FMaster) div 12;
      FWords.FMaster := oRead.ReadString;
      FWords.FLength := Length(FWords.FMaster);
      FStems.FMaster := oRead.ReadString;
      FStems.FLength := Length(FStems.FMaster);
      FRoot := oRead.ReadInteger;
      FVersion := oRead.ReadString;
      For aLoop := Low(TLoincPropertyType) To High(TLoincPropertyType) Do
        FProperties[aLoop] := oRead.ReadInteger;
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
      oWrite.WriteString(FCode.FMaster);
      oWrite.WriteString(FDesc.FMaster);
      oWrite.WriteString(FRefs.FMaster);
      oWrite.WriteString(FConcepts.FMaster);
      oWrite.WriteString(FWords.FMaster);
      oWrite.WriteString(FStems.FMaster);
      oWrite.writeInteger(FRoot);
      oWrite.WriteString(FVersion);
      For aLoop := Low(TLoincPropertyType) To High(TLoincPropertyType) Do
        oWrite.WriteInteger(FProperties[aLoop]);
    Finally
      oWrite.Free;
    End;
  Finally
    oFile.Free;
  End;
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




function TLOINCServices.Search(sText: String): TMatchArray;
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

  Procedure AddResult(var iCount : Integer; iindex : cardinal; sId : String; priority : Double);
  Begin
    if iCount = length(result) then
      SetLength(result, Length(result)+100);
    result[iCount].index := iIndex;
    result[iCount].code := sId;
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
{    iDepth : Byte;
    Identity : int64;
    Flags : Byte;
    Parents : Cardinal;
    Descriptions : Cardinal;
    Inbounds : Cardinal;
    outbounds : Cardinal;}
    Desc : TCardinalArray;
    {iWork : Cardinal;}
//  iCount : Integer;
//  i : cardinal;
  iDescription, iStems, iOtherNames : Cardinal;
//  s,
  sCode1 : String;
  iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt : Word;
  iFlags : Byte;
//  oReg : TRegExpr;
//  bIncl : Boolean;
  Begin
    Code.GetInformation(iCodeIndex, sCode1, iDescription, iOtherNames, iStems, iComponent, iProperty, iTimeAspect, iSystem, iScale, iMethod, iClass, iv2dt, iv3dt, iFlags);
    r1 := 0;
    Desc := Refs.GetCardinals(iStems);
    For i := 0 to length(words) - 1 do
      if words[i].stem <> 0 Then
        For j := 0 to length(Desc) - 1 do
          if (words[i].stem = desc[j]) Then
            r1 := r1 + 20 + (20 / length(desc))
          else
            assert(FDesc.GetEntry(words[i].stem) <> FDesc.GetEntry(desc[j]));

    if r1  > 0 Then
      AddResult(iCount, iCodeIndex, sCode1, r1);
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
        s1 := oStemmer.Stem(s);
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
      if SameText(Service[i].Version, sName) then
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
  FBuilder.AddCardinalAsBytes(index);
  FBuilder.AddByteAsBytes(flags);
end;

function TLoincWords.Count: Integer;
begin
  result := FLength div 5;
end;

procedure TLoincWords.DoneBuild;
begin
  FMaster := FBuilder.AsString;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TLoincWords.GetEntry(iIndex: Cardinal; var index: Cardinal; var flags: Byte);
var
  l : Cardinal;
begin
  l := (iIndex * 5) + 1;
  if l > FLength - 4 Then
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
  FBuilder := TAdvStringBuilder.Create;
end;

{ TLoincStems }

procedure TLoincStems.AddStem(index: Cardinal; reference : Cardinal);
begin
  FBuilder.AddCardinalAsBytes(index);
  FBuilder.AddCardinalAsBytes(reference);
end;

function TLoincStems.Count: Integer;
begin
  result := FLength div 8;
end;

procedure TLoincStems.DoneBuild;
begin
  FMaster := FBuilder.AsString;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TLoincStems.GetEntry(iIndex: Cardinal; var index: Cardinal; var reference: Cardinal);
var
  l : Cardinal;
begin
  l := (iIndex * 8) + 1;
  if l > FLength - 7 Then
    raise Exception.create('invalid index');
  move(FMaster[l], index, 4);
  move(FMaster[l+4], reference, 4);
end;

function TLoincStems.GetString(iIndex: Cardinal): Cardinal;
var
  f : cardinal;
begin
  GetEntry(iIndex, result, f);
end;

procedure TLoincStems.StartBuild;
begin
  FBuilder := TAdvStringBuilder.Create;
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


End.

