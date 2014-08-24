unit SnomedServices;

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

{
This works by pre-processing a snomed distribution into a tightly
condensed set of structures that capture a ready to use analysis
of the structure. The content is stored in strings which are arrays
of words, cardinals, or seperated character values.

The content loads and works extremely quickly.
}

Uses
  SysUtils, Classes,
  StringSupport, FileSupport,
  AdvStringLists, AdvObjectLists, AdvObjects,
  AnsiStringBuilder, YuStemmer, DateAndTime,
  FHIRTypes, FHIRComponents, FHIRResources,
  TerminologyServices;

Const
  SNOMED_CACHE_VERSION = '8';
  IS_A_MAGIC : int64 = 116680003;
  ALL_DISPLAY_NAMES = $FF;

type
  Int64Array = Array of Int64;
  TCardinalArray = array of Cardinal;
  TMatch = record
    index : cardinal;
    term : Int64;
    Priority : Double;
  End;

  TMatchArray = Array of TMatch;

  TSnomedDate = word; // whole number of days matching whole number portion of TDateTime

  // We store snomed as four structures.
  //   the first structure is a simply a list of strings which are variable length names - referred to from the other structures
  //   the second structure is a list of lists of cardinal references
  //   the third structure is a list of descriptions. Each has some a flag for type and initial status, and a description. we do not store the identity at this time
  //   the fourth structure is a list of concepts, stored in order of their identity, along with a flag for (status, primitive, single parent), a parent reference, a description, and a source and target reference list
  //   the fifh structure is a list of relationships - source, type, target, and a flag


  //   the second structure is a list of lists of word or cardinal references.
  //   the third structure is a list of concepts. each concept has a refernce to a name and a contained list of references which are either children
  //   the fourth structure is the code list - a list of loinc concepts, with codes and references to names and properties

  // We store snomed as three AnsiStrings
  //   each entry in the AnsiString starts with a byte length, and then the series of characters as bytes
  TSnomedStrings = class (TAdvObject)
    Private
      FMaster : AnsiString;
      FLength : Cardinal;
      FBuilder : TAnsiStringBuilder;
    Public
      Function GetEntry(iIndex : Cardinal):AnsiString;

      Procedure StartBuild;
      Function AddString(Const s : AnsiString) : Cardinal;
      Procedure DoneBuild;
  End;

const
  FLAG_WORD_DSN = $01;
  FLAG_WORD_PN = $02;
  FLAG_WORD_DEP = $04;

Type
  // word index. Every word is 5 bytes - a 4 byte index into the strings, and a 1 byte flag
  TSnomedWords = class (TAdvObject)
    Private
      FMaster : AnsiString;
      FLength : Cardinal;
      FBuilder : TAnsiStringBuilder;
   Public
      Procedure GetEntry(iIndex : Cardinal; var index : Cardinal; var flags : Byte);
      Function Count : Integer;
      Function GetString(iIndex : Cardinal) : Cardinal;

      Procedure StartBuild;
      Procedure AddWord(index : Cardinal; Flags : Byte);
      Procedure DoneBuild;
  End;

  // stem word index. Every word is 8 bytes - a 4 byte index into the strings, and a 4 byte index into the references
  TSnomedStems = class (TAdvObject)
    Private
      FMaster : AnsiString;
      FLength : Cardinal;
      FBuilder : TAnsiStringBuilder;
   Public
      Procedure GetEntry(iIndex : Cardinal; var index : Cardinal; var reference : Cardinal);
      Function Count : Integer;
      Function GetString(iIndex : Cardinal) : Cardinal;

      Procedure StartBuild;
      Procedure AddStem(index, reference : Cardinal);
      Procedure DoneBuild;
  End;


  // 2. a list of list of references
  TSnomedReferences = class (TAdvObject)
    Private
      FMaster : AnsiString;
      FLength : Cardinal;
      FBuilder : TAnsiStringBuilder;
    Public
      Function GetReferences(iIndex : Cardinal) : TCardinalArray;
      Function Getlength(iIndex : Cardinal) : Cardinal;

      Procedure StartBuild;
      Function AddReferences(Const a : TCardinalArray) : Cardinal;
      Procedure DoneBuild;
      Procedure Post;
  End;

  // 3. a list of descriptions with flag. we do not store description id
const
  DESC_SIZE = 31;
  MASK_DESC_STATUS = $0F;
  MASK_DESC_STYLE = $70;
  MASK_DESC_CAPS = $80; // this leaves one bit unused (8)

  VAL_DESC_Unspecified = 0;
  VAL_DESC_Preferred = 1;
  VAL_DESC_Synonym = 2;
  VAL_DESC_FullySpecifiedName = 3;

  FLAG_Active = 0;
  FLAG_RetiredWithoutStatedReason = 1;
  FLAG_Duplicate = 2;
  FLAG_Outdated = 3;
  FLAG_Ambiguous = 4;
  FLAG_Erroneous = 5;
  FLAG_Limited = 6;
  FLAG_Inappropriate = 7;
  FLAG_ConceptInactive = 8;
  FLAG_MovedElswhere = 10;
  FLAG_PendingMove = 11;

Type
  TSnomedDescriptions = class (TAdvObject)
    Private
      FMaster : AnsiString;
      FLength : Cardinal;
      FBuilder : TAnsiStringBuilder;
    Public
      function Count : Cardinal;
      Procedure GetDescription(iIndex : Cardinal; var iDesc : Cardinal; var id : Int64; var date : TSnomedDate; var concept, module, kind, refsets : Cardinal; var iFlags : Byte);
      function ConceptByIndex(iIndex : Cardinal) : cardinal;


      Procedure StartBuild;
      Function AddDescription(iDesc : Cardinal; id : Int64; date : TSnomedDate; concept, module, kind : Cardinal; iflags : Byte) : Cardinal;
      Procedure UpdateDetails(iIndex : Cardinal; id : int64; concept : Cardinal);
      Procedure DoneBuild;
      Procedure SetRefsets(iIndex : Cardinal; refsets : Cardinal);
  End;

  TSnomedDescriptionIndex = class (TAdvObject)
  Private
    FMaster : AnsiString;
    FLength : Cardinal;
    FBuilder : TAnsiStringBuilder;
  Public
    Function FindDescription(iIdentity : int64; var IIndex : Cardinal) : boolean;

    Procedure StartBuild;
    procedure AddDescription(id : Int64; reference : Cardinal);
    Procedure DoneBuild;
  End;

  // 4. a list of list of terms
  //   id as a 8 byte int64
  //   flag is a byte
  //   descriptions as a cardinal index of cardinal indexes
  //   parents as a cardinal index of cardinal indexes
  //   inbounds as a cardinal index of cardinal indexes
  //   outbounds as a cardinal index of cardinal indexes
  //   descendents (closure) as either 0 - not indexed, $FFFFFFFF - no children, or index of cardinals = all descendents
  //   depth as a byte
  //   stems as a cardinal index of stems
  //   effective date as a word
  //   moduleId as a cardinal (concept reference)
  //   status as a cardinal (concept reference)

Const
  MASK_CONCEPT_STATUS = $0F;
  MASK_CONCEPT_PRIMITIVE = $10; // this leaves three bits unused (6-8)
  MAGIC_NO_CHILDREN = $FFFFFFFF;

  CONCEPT_SIZE = 48;

Type
  TSnomedConceptList = class (TAdvObject)
    Private
      FMaster : AnsiString;
      FLength : Cardinal;
      FBuilder : TAnsiStringBuilder;
    Public
      Function FindConcept(iIdentity : int64; var IIndex : Cardinal) : boolean;
      function getConceptId(iIndex : Cardinal) : int64;
      procedure GetConcept(iIndex : Cardinal; var Identity : int64; var Flags : Byte; var effectiveTime : TSnomedDate; var Parents : Cardinal; var Descriptions : Cardinal; var Inbounds : Cardinal; var outbounds : Cardinal; var refsets : Cardinal);
      Function GetParent(iIndex : Cardinal): Cardinal;
      Function GetIdentity(iIndex : Cardinal): int64;
      Function Count : Integer;

      procedure SetParents(iIndex: Cardinal; const Value: Cardinal);
      procedure SetDescriptions(iIndex: Cardinal; const Value: Cardinal);
      Function GetDescriptions(iIndex: Cardinal) : Cardinal;
      procedure SetInbounds(iIndex: Cardinal; const Value: Cardinal);
      Function GetInbounds(iIndex: Cardinal) : Cardinal;
      procedure SetOutbounds(iIndex: Cardinal; const Value: Cardinal);
      Function GetOutbounds(iIndex: Cardinal): Cardinal;

      Function GetAllDesc(iIndex: Cardinal) : Cardinal;
      procedure SetAllDesc(iIndex: Cardinal; Value: Cardinal);
      Function GetDepth(iIndex: Cardinal) : Byte;
      procedure SetDepth(iIndex: Cardinal; Value: Byte);
      Function GetStems(iIndex: Cardinal) : Cardinal;
      procedure SetStems(iIndex: Cardinal; Value: Cardinal);

      Function GetModuleId(iIndex: Cardinal) : Cardinal;
      procedure SetModuleId(iIndex: Cardinal; Value: Cardinal);
      Function GetStatus(iIndex: Cardinal) : Cardinal;
      procedure SetStatus(iIndex: Cardinal; Value: Cardinal);
      Function GetRefsets(iIndex: Cardinal) : Cardinal;
      procedure SetRefsets(iIndex: Cardinal; Value: Cardinal);

      // these presume that the terms are registered in order
      Procedure StartBuild;
      Function AddConcept(iIdentity : int64; effectiveTime : TSnomedDate; iFlags : Byte) : Cardinal;
      Procedure DoneBuild;
  End;

// 5. a list of relationships
Const
  MASK_REL_CHARACTERISTIC = $0F;
  MASK_REL_REFINABILITY = $F0;

  VAL_REL_Defining = 0;
  VAL_REL_Qualifying = 1;
  VAL_REL_Historical = 2;
  VAL_REL_Additional = 3;
  VAL_REL_NotRefinable = 0;
  VAL_REL_Optional = 1;
  VAL_REL_Mandatory = 2;

Type
  TSnomedRelationshipList = class (TAdvObject)
    private
      FMaster : AnsiString;
      FLength : Cardinal;
      FBuilder : TAnsiStringBuilder;
    Public
      // for Persistence
      Procedure GetRelationship(iIndex: Cardinal; var Source, Target, RelType, module, kind, modifier : Cardinal; var date : TSnomedDate; var Flags, Group : Byte);

      Procedure StartBuild;
      Function AddRelationship(Source, Target, RelType, module, kind, modifier : Cardinal; date : TSnomedDate; Flags, Group : Byte) : Cardinal;
      Procedure DoneBuild;
  End;

Type
  TSnomedReferenceSetMember = record
    kind : byte; // 0 = concept, 1 = desc, 2 = relationship
    Ref : Cardinal; // desc or term depending on ref set type
  End;
  TSnomedReferenceSetMemberArray = array of TSnomedReferenceSetMember;


  // 2. a list of list of references
  TSnomedReferenceSetMembers = class (TAdvObject)
    Private
      FMaster : AnsiString;
      FLength : Cardinal;
      FBuilder : TAnsiStringBuilder;
    Public
      Function GetMembers(iIndex : Cardinal) : TSnomedReferenceSetMemberArray;
      Function GetMemberCount(iIndex : Cardinal) : integer;

      Procedure StartBuild;
      Function AddMembers(Const a : TSnomedReferenceSetMemberArray) : Cardinal;
      Procedure DoneBuild;
  End;


  TSnomedReferenceSetIndex = class (TAdvObject)
  Private
    FMaster : AnsiString;
    FLength : Cardinal;
    FBuilder : TAnsiStringBuilder;
  Public
    Procedure GetReferenceSet(iIndex: Cardinal; var iDefinition, iMembersByRef, iMembersByName: Cardinal);
    Function GetMembersByConcept(iIndex : Cardinal; bByName : Boolean) : Cardinal;
    Function Count : Integer;

    Procedure StartBuild;
    Procedure AddReferenceSet(iDefinition, iMembersByRef, iMembersByName: Cardinal);
    Procedure DoneBuild;
  End;

(*
{
operations
  isSubsumedBy(term, term)
  getDesc : AnsiString
  is valid (id)
}

  TSnomedConceptSummary = class (TAdvObject)
  private
    FIsPrimitive: boolean;
    FConcept: String;
    FDescription: String;
    FParents: TAdvStringList;
    FStatus: TSnomedConceptStatus;
    FChildren: TAdvStringList;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Property Concept : String read FConcept;
    Property Description : String read FDescription;
    Property IsPrimitive : boolean read FIsPrimitive;
    Property Status : TSnomedConceptStatus read FStatus;
    Property Children : TAdvStringList read FChildren;
    Property Parents : TAdvStringList read FParents;
  End;

*)
//  TSnomedReferenceSet = class;

  TSnomedFilterContext = class (TCodeSystemProviderFilterContext)
  private
    ndx : integer;
    matches : TMatchArray;
    members : TSnomedReferenceSetMemberArray;
    descendents : TCardinalArray;
  end;


  TSnomedServices = class (TCodeSystemProvider)
  Private
    FActiveRoots : Int64Array;
    FInactiveRoots : Int64Array;
    FIs_a_Index : Cardinal;
    FStrings : TSnomedStrings;
    FRefs : TSnomedReferences;
    FDesc : TSnomedDescriptions;
    FDescRef : TSnomedDescriptionIndex;
    FConcept : TSnomedConceptList;
    FRel : TSnomedRelationshipList;
    FRefSetIndex : TSnomedReferenceSetIndex;
    FRefSetMembers : TSnomedReferenceSetMembers;

    FWords : TSnomedWords;
    FStems : TSnomedStems;
    FVersion : String;
    FLoaded: Boolean;

    function GetPN(iDescriptions: TCardinalArray): String;
    function GetFSN(iDescriptions: TCardinalArray): String;
    function filterIn(id : Int64): TCodeSystemProviderFilterContext;
    function filterIsA(id : Int64): TCodeSystemProviderFilterContext;

  //  Function FindWord(s : String; var index : Integer) : Boolean;
    Function FindStem(s : AnsiString; var index : Integer) : Boolean;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function Link : TSnomedServices; Overload;
    Procedure Load(Const sFilename : String);
    Procedure Save(Const sFilename : String);

    // helper functions
    Function StringToId(Const s : String) : Int64;
    Function StringToIdOrZero(Const s : AnsiString) : Int64;
    Function StringIsId(Const s : AnsiString; var iId : Int64) : Boolean;

    // direct access to the raw data
    Property Strings : TSnomedStrings read FStrings;
    Property Words : TSnomedWords Read FWords;
    Property Stems : TSnomedStems Read FStems;
    Property Refs : TSnomedReferences read FRefs;
    Property Desc : TSnomedDescriptions read FDesc;
    Property Concept : TSnomedConceptList read FConcept;
    Property Rel : TSnomedRelationshipList read FRel;
    Property RefSetIndex : TSnomedReferenceSetIndex read FRefsetIndex;
    Property RefSetMembers : TSnomedReferenceSetMembers read FRefsetMembers;
    Property DescRef : TSnomedDescriptionIndex read FDescRef;

    // low level access for service providers
    Property ActiveRoots : Int64Array read FActiveRoots write FActiveRoots;
    Property InactiveRoots : Int64Array read FInActiveRoots write FInActiveRoots;
    Property Is_a_Index : Cardinal read FIs_a_Index write FIs_a_Index;
    function Subsumes(iParent, iChild: Cardinal): Boolean; Overload;
    Function GetDisplayName(Const iConcept, iLang : Cardinal) : AnsiString; Overload;
    Procedure ListDisplayNames(list : TStringList; Const iConcept, iLang : Cardinal; FlagMask : Byte); Overload;
    Function GetConceptId(Const iConcept : Cardinal) : AnsiString; Overload;
    Procedure GetMatchInfo(iConcept : Cardinal; var sTerm, sFSN, sPreferred : AnsiString);
    Function GetConceptRefSet(iConcept : Cardinal; bByName : Boolean; var iMembers : cardinal) : Cardinal;
    Function GetDescRefsets(iDesc : Cardinal) : TCardinalArray;
    Function GetConceptRefsets(iDesc : Cardinal) : TCardinalArray;
    Function CheckLangSet(sTerm : AnsiString) : Cardinal;
    function GetConceptDescendents(index : Cardinal) : TCardinalArray;

    // simplified interface for consumers
    Function ConceptExists(conceptId : String) : Boolean;
    Function Subsumes(Const sParent, sChild : AnsiString) : Boolean; Overload;
    Function Search(iRoot : Int64; sText : AnsiString; iLang : Cardinal; bInactive : Boolean) : TMatchArray; overload;
    Function IsValidConcept(Const sTerm : String):Boolean;
    Function IsValidDescription(Const sTerm : AnsiString; var concept : int64; var description : String):Boolean;
    Function GetDisplayName(Const sTerm, sLangSet : AnsiString) : AnsiString; Overload;
    Procedure ListDisplayNames(list : TStringList; Const sTerm, sLangSet : AnsiString; FlagMask : Byte); Overload;
    function ReferenceSetExists(sid : String) : Boolean;

    // status stuff
    Property Loaded : Boolean read FLoaded write FLoaded;
    Property Version : String read FVersion write FVersion;


    // generic terminology server interface
    function TotalCount : integer; override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
    function system : String; override;
    function getDisplay(code : String):String; override;
    function locate(code : String) : TCodeSystemProviderContext; override;
    function IsAbstract(context : TCodeSystemProviderContext) : boolean; override;
    function Code(context : TCodeSystemProviderContext) : string; override;
    function Display(context : TCodeSystemProviderContext) : string; override;
    procedure Displays(code : String; list : TStringList); override;
    procedure Displays(context : TCodeSystemProviderContext; list : TStringList); override;
    function filter(prop : String; op : TFhirFilterOperator; value : String; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; override;
    function FilterMore(ctxt : TCodeSystemProviderFilterContext) : boolean; override;
    function FilterConcept(ctxt : TCodeSystemProviderFilterContext): TCodeSystemProviderContext; override;
    procedure Close(ctxt : TCodeSystemProviderFilterContext); override;
    procedure Close(ctxt : TCodeSystemProviderContext); override;
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function buildValueSet(id : String) : TFhirValueSet;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext) : TCodeSystemProviderFilterContext; overload; override;
    function getDefinition(code : String):String; override;
    function Definition(context : TCodeSystemProviderContext) : string; override;
  End;

  TSnomedServiceList = class (TAdvObjectList)
  Private
    FDefinition: TSnomedServices;
    function GetDefinition(iIndex: Integer): TSnomedServices;
    procedure SetDefinition(const Value: TSnomedServices);
  Protected
    Function ItemClass : TAdvObjectClass; Override;
  Public
    Destructor Destroy; Override;

    Function GetDefinitionByName(sName : AnsiString) : TSnomedServices;

    Property DefaultDefinition : TSnomedServices Read FDefinition write SetDefinition;
    Function HasDefaultDefinition : Boolean;

    Property Definition[iIndex : Integer] : TSnomedServices read GetDefinition; Default;
  End;


{
reference set

header....
  id  int64
members
  concept - cardinal
  desc - cardinal (or 0 if one is not specified)
  attribute - cardinal (0r 0 is one is not specified)

}
function FindMember(aMembers : TSnomedReferenceSetMemberArray; iRef : Cardinal; var iIndex: integer): boolean;
function FindCardinalInArray(a : TCardinalArray; iValue : Cardinal; var iIndex : Integer):Boolean;

Implementation

{ TSnomedStrings }

function TSnomedStrings.GetEntry(iIndex: Cardinal): AnsiString;
var
  i : Word;
begin
  if (iIndex > FLength) then
    Raise Exception.Create('Wrong length index getting snomed name');
  Move(FMaster[iIndex], i, 2);
  SetLength(Result, i);
  if (Byte(FMaster[iIndex]) + iIndex > FLength) then
    Raise Exception.Create('Wrong length index getting snomed name (2)');
  Move(FMaster[iIndex+2], result[1], Length(Result));
end;

function TSnomedStrings.AddString(const s: AnsiString): Cardinal;
var
  i : word;
begin
  if Length(s) > 65535 Then
    raise exception.Create('Snomed Description too long: '+s);
  result := FBuilder.Length + 1;
  i := length(s);
  FBuilder.AddWordAsBytes(i);
  FBuilder.Append(s);
end;

procedure TSnomedStrings.DoneBuild;
begin
  FMaster := FBuilder.AsString;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TSnomedStrings.StartBuild;
begin
  FBuilder := TAnsiStringBuilder.Create;
end;

{ TSnomedReferences }

Function TSnomedReferences.GetReferences(iIndex: Cardinal) : TCardinalArray;
var
  i : integer;
  c : Cardinal;
begin
  if (iIndex = MAGIC_NO_CHILDREN) or (iIndex = 0) then
    result := nil
  Else
  Begin
    if (iIndex > FLength) and (FBuilder <> nil) then
      Post;
    if (iIndex > FLength) then
      Raise Exception.Create('Wrong length index getting Snomed list');
    move(FMaster[iIndex], c, 4);
    SetLength(Result, c);
    if (iIndex + 4 + length(result) * 4 > FLength) then
      Raise Exception.Create('Wrong length index getting Snomed list (2)');
    inc(iIndex, 4);
    for i := 0 to Length(result)-1 Do
    Begin
      move(FMaster[iIndex], result[i], 4);
      inc(iIndex, 4);
    End;
  End;
end;

Function TSnomedReferences.AddReferences(Const a : TCardinalArray) : Cardinal;
var
  iLoop : Integer;
Begin
  result := FBuilder.Length + 1;
  FBuilder.AddCardinalAsBytes(length(a));
  for iLoop := Low(a) to High(a) Do
    FBuilder.AddCardinalAsBytes(a[iLoop]);
End;

procedure TSnomedReferences.DoneBuild;
begin
  FMaster := FBuilder.AsString;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TSnomedReferences.StartBuild;
begin
  FBuilder := TAnsiStringBuilder.Create;
end;

function TSnomedReferences.Getlength(iIndex: Cardinal): Cardinal;
begin
  if (iIndex > FLength) then
    Raise Exception.Create('Wrong length index getting Snomed list');
  move(FMaster[iIndex], result, 4);
end;

procedure TSnomedReferences.Post;
begin
  FMaster := FBuilder.AsString;
  FLength := Length(FMaster);
end;

{ TSnomedDescriptions }

procedure TSnomedDescriptions.SetRefsets(iIndex, refsets: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise Exception.Create('Wrong length index getting snomed Desc Details');
  Move(refsets, FMaster[iIndex+27], 4);
end;

procedure TSnomedDescriptions.StartBuild;
begin
  FBuilder := TAnsiStringBuilder.Create;
end;

Function TSnomedDescriptions.AddDescription(iDesc : Cardinal; id : int64; date : TSnomedDate; concept, module, kind : Cardinal; iflags : Byte) : Cardinal;
begin
  result := FBuilder.Length+1;
  FBuilder.AddCardinalAsBytes(iDesc);
  FBuilder.AddByteAsBytes(iFlags);
  FBuilder.AddInt64AsBytes(id);
  FBuilder.AddCardinalAsBytes(concept);
  FBuilder.AddCardinalAsBytes(module);
  FBuilder.AddCardinalAsBytes(kind);
  FBuilder.AddWordAsBytes(date);
  FBuilder.AddCardinalAsBytes(0); // refsets
end;

function TSnomedDescriptions.ConceptByIndex(iIndex: Cardinal): cardinal;
begin
  if (iIndex >= FLength) then
    Raise Exception.Create('Wrong length index getting snomed Desc Details');
  Move(FMaster[iIndex+13], result, 4);
end;

function TSnomedDescriptions.Count: Cardinal;
begin
  result := FLength div DESC_SIZE;
end;

procedure TSnomedDescriptions.DoneBuild;
begin
  FMaster := FBuilder.AsString;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TSnomedDescriptions.GetDescription(iIndex : Cardinal; var iDesc : Cardinal; var id : Int64; var date : TSnomedDate; var concept, module, kind, refsets : Cardinal; var iflags : Byte);
Begin
  if (iIndex >= FLength) then
    Raise Exception.Create('Wrong length index getting snomed Desc Details');
  Move(FMaster[iIndex+0], iDesc, 4);
  Move(FMaster[iIndex+4], iFlags, 1);
  Move(FMaster[iIndex+5], ID, 8);
  Move(FMaster[iIndex+13], concept, 4);
  Move(FMaster[iIndex+17], module, 4);
  Move(FMaster[iIndex+21], kind, 4);
  Move(FMaster[iIndex+25], date, 2);
  Move(FMaster[iIndex+27], refsets, 4);
End;


procedure TSnomedDescriptions.UpdateDetails(iIndex: Cardinal; id: int64; concept: Cardinal);
var
  t1 : int64;
  t2 : Cardinal;
  s : AnsiString;
begin
  FBuilder.Read(iIndex+13, t2, 4);
  if (t2 <> concept) Then
  Begin
    assert(t2 = 0);

    FBuilder.Read(iIndex+5, t1, 8);
    assert(t1 = 0);
    SetLength(s, 8);
    Move(id, s[1], 8);
    FBuilder.Overwrite(iIndex+5, s);
    FBuilder.Read(iIndex+5, t1, 8);
    assert(t1 = id);

    SetLength(s, 4);
    Move(concept, s[1], 4);
    FBuilder.Overwrite(iIndex+13, s);
    FBuilder.Read(iIndex+13, t2, 4);
    assert(t2 = concept);
  End;
end;


{ TSnomedConceptList }

procedure TSnomedConceptList.StartBuild;
begin
  FBuilder := TAnsiStringBuilder.Create;
end;

Function TSnomedConceptList.AddConcept(iIdentity : int64; effectiveTime : TSnomedDate; iFlags : Byte) : Cardinal;
begin
  result := FBuilder.Length+1;
  FBuilder.AddInt64AsBytes(iIdentity);
  FBuilder.AddByteAsBytes(iFlags);
  FBuilder.AddCardinalAsBytes(0); // parents
  FBuilder.AddCardinalAsBytes(0); // descriptions
  FBuilder.AddCardinalAsBytes(0); // inbounds
  FBuilder.AddCardinalAsBytes(0); // outbounds
  FBuilder.AddCardinalAsBytes(0); // closures
  FBuilder.AddByteAsBytes(0); // depth
  FBuilder.AddCardinalAsBytes(0); // stems
  FBuilder.AddWordAsBytes(effectiveTime); // date
  FBuilder.AddCardinalAsBytes(0); // moduleId (rf2)
  FBuilder.AddCardinalAsBytes(0); // status (rf2)
  FBuilder.AddCardinalAsBytes(0); // refsets (rf2)
end;

procedure TSnomedConceptList.DoneBuild;
begin
  FMaster := FBuilder.AsString;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

{$R-}

function TSnomedConceptList.FindConcept(iIdentity: int64; var IIndex: Cardinal): boolean;
var
  aConcept : int64;
  L, H, I: Integer;
  c : Int64;
begin
  Result := False;
  L := 0;
  H := FLength;
  H := (H div CONCEPT_SIZE) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    Move(FMaster[(i*CONCEPT_SIZE)+1], aConcept, 8);
    C := aConcept - iIdentity;
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
  iIndex := (L*CONCEPT_SIZE)+1;
end;

Function TSnomedConceptList.getParent(iIndex : Cardinal): Cardinal;
Begin
  if (iIndex >= FLength) then
    Raise Exception.Create('Wrong length index getting snomed Concept Details');
  Move(FMaster[iIndex+9], result, 4);
End;

function TSnomedConceptList.GetRefsets(iIndex: Cardinal): Cardinal;
begin
  Move(FMaster[iIndex+44], result, 4);
end;

Function TSnomedConceptList.getIdentity(iIndex : Cardinal): Int64;
Begin
  if (iIndex >= FLength) then
    Raise Exception.Create('Wrong length index getting snomed Concept Details');
  Move(FMaster[iIndex+0], result, 8);
End;

procedure TSnomedConceptList.GetConcept(iIndex : Cardinal; var Identity : int64; var Flags : Byte; var effectiveTime : TSnomedDate; var Parents : Cardinal; var Descriptions : Cardinal; var Inbounds : Cardinal; var outbounds : Cardinal; var refsets : Cardinal);
Begin
  if (iIndex >= FLength) then
    Raise Exception.Create('Wrong length index getting snomed Concept Details');
  Move(FMaster[iIndex+0], Identity, 8);
  Move(FMaster[iIndex+8], Flags, 1);
  Move(FMaster[iIndex+9], Parents, 4);
  Move(FMaster[iIndex+13], Descriptions, 4);
  Move(FMaster[iIndex+17], Inbounds, 4);
  Move(FMaster[iIndex+21], Outbounds, 4);
  Move(FMaster[iIndex+34], effectiveTime, 2);
  Move(FMaster[iIndex+44], refsets, 2);
End;

function TSnomedConceptList.getConceptId(iIndex : Cardinal): int64;
begin
  if (iIndex >= FLength) then
    Raise Exception.Create('Wrong length index getting snomed Concept Details');
  Move(FMaster[iIndex+0], result, 8);
end;

procedure TSnomedConceptList.SetParents(iIndex: Cardinal; const Value: Cardinal);
begin
  Move(Value, FMaster[iIndex+9], 4);
end;

procedure TSnomedConceptList.SetRefsets(iIndex, Value: Cardinal);
begin
  Move(Value, FMaster[iIndex+44], 4);
end;

procedure TSnomedConceptList.SetDescriptions(iIndex: Cardinal; const Value: Cardinal);
begin
  Move(Value, FMaster[iIndex+13], 4);
end;

procedure TSnomedConceptList.SetInbounds(iIndex: Cardinal; const Value: Cardinal);
begin
  Move(Value, FMaster[iIndex+17], 4);
end;

procedure TSnomedConceptList.SetModuleId(iIndex, Value: Cardinal);
begin
  Move(Value, FMaster[iIndex+36], 4);
end;

procedure TSnomedConceptList.SetOutbounds(iIndex: Cardinal; const Value: Cardinal);
begin
  Move(Value, FMaster[iIndex+21], 4);
end;


function TSnomedConceptList.Count: Integer;
begin
  result := FLength div CONCEPT_SIZE;
end;

Function TSnomedConceptList.GetAllDesc(iIndex: Cardinal) : Cardinal;
begin
  Move(FMaster[iIndex+25], result, 4);
end;

procedure TSnomedConceptList.SetAllDesc(iIndex, Value: Cardinal);
begin
  Move(Value, FMaster[iIndex+25], 4);
end;

function TSnomedConceptList.GetOutbounds(iIndex: Cardinal): Cardinal;
begin
  Move(FMaster[iIndex+21], result, 4);
end;

function TSnomedConceptList.GetInbounds(iIndex: Cardinal): Cardinal;
begin
  Move(FMaster[iIndex+17], result, 4);
end;

function TSnomedConceptList.GetModuleId(iIndex: Cardinal): Cardinal;
begin
  Move(FMaster[iIndex+36], result, 4);
end;

function TSnomedConceptList.GetDepth(iIndex: Cardinal): Byte;
begin
  Move(FMaster[iIndex+29], result, 1);
end;

procedure TSnomedConceptList.SetDepth(iIndex: Cardinal; Value: Byte);
begin
  Move(Value, FMaster[iIndex+29], 1);
end;

function TSnomedConceptList.GetStatus(iIndex: Cardinal): Cardinal;
begin
  Move(FMaster[iIndex+40], result, 4);
end;

function TSnomedConceptList.GetStems(iIndex: Cardinal): Cardinal;
begin
  Move(FMaster[iIndex+30], result, 4);
end;

procedure TSnomedConceptList.SetStatus(iIndex, Value: Cardinal);
begin
  Move(Value, FMaster[iIndex+40], 4);
end;

procedure TSnomedConceptList.SetStems(iIndex, Value: Cardinal);
begin
  Move(Value, FMaster[iIndex+30], 4);
end;

function TSnomedConceptList.GetDescriptions(iIndex: Cardinal): Cardinal;
begin
  Move(FMaster[iIndex+13], Result, 4);
end;

{ TSnomedServices }

constructor TSnomedServices.Create;
begin
  inherited;
  FStrings := TSnomedStrings.Create;
  FRefs := TSnomedReferences.Create;
  FDesc := TSnomedDescriptions.Create;
  FConcept := TSnomedConceptList.Create;
  FRel := TSnomedRelationshipList.Create;
  FWords := TSnomedWords.Create;
  FStems := TSnomedStems.Create;
  FRefSetIndex := TSnomedReferenceSetIndex.Create;
  FRefSetMembers := TSnomedReferenceSetMembers.Create;
  FDescRef := TSnomedDescriptionIndex.Create;
end;

function TSnomedServices.Definition(context: TCodeSystemProviderContext): string;
begin
  result := '';
end;

destructor TSnomedServices.Destroy;
begin
  FDescRef.Free;
  FRefSetIndex.Free;
  FRefSetMembers.Free;
  FWords.Free;
  FStems.Free;
  FRefs.Free;
  FStrings.Free;
  FRel.Free;
  FDesc.Free;
  FConcept.Free;
  inherited;
end;

procedure TSnomedServices.Load(const sFilename: String);
var
  oFile : Tfilestream;
  oread : TReader;
  i : Integer;
begin
  oFile := TFileStream.Create(sFilename, fmOpenread);
  try
    oread := TReader.Create(oFile, 8192);
    try
      if oRead.ReadString <> SNOMED_CACHE_VERSION Then
        raise exception.create('The Snomed cache "'+sFilename+'" must be rebuilt using -snomed-rf1 or -snomed-rf2');
      FVersion := oread.ReadString;
      FStrings.FMaster := oread.ReadString;
      FStrings.FLength := Length(FStrings.FMaster);
      FRefs.FMaster := oread.ReadString;
      FRefs.FLength := Length(FRefs.FMaster);
      FDesc.FMaster := oread.ReadString;
      FDesc.FLength := Length(FDesc.FMaster);
      FWords.FMaster := oRead.ReadString;
      FWords.FLength := Length(FWords.FMaster);
      FStems.FMaster := oRead.ReadString;
      FStems.FLength := Length(FStems.FMaster);
      FConcept.FMaster := oread.ReadString;
      FConcept.FLength := Length(FConcept.FMaster);
      FRel.FMaster := oread.ReadString;
      FRel.FLength := Length(FRel.FMaster);
      FRefSetIndex.FMaster := oread.ReadString;
      FRefSetIndex.FLength := Length(FRefSetIndex.FMaster);
      FRefSetMembers.FMaster := oread.ReadString;
      FRefSetMembers.FLength := Length(FRefSetMembers.FMaster);
      FDescRef.FMaster := oread.ReadString;
      FDescRef.FLength := Length(FDescRef.FMaster);
      FIs_a_Index := oread.ReadInteger;
      SetLength(FInactiveRoots, oRead.ReadInteger);
      for i := 0 to Length(FInactiveRoots) - 1 Do
        FInactiveRoots[i] := oRead.ReadInt64;
      SetLength(FActiveRoots, oRead.ReadInteger);
      for i := 0 to Length(FActiveRoots) - 1 Do
        FActiveRoots[i] := oRead.ReadInt64;
    Finally
      oread.Free;
    End;
  Finally
    oFile.Free;
  End;
  Loaded := true;
end;

function TSnomedServices.ReferenceSetExists(sid: String): Boolean;
var
  index, members : Cardinal;
begin
  result := FConcept.FindConcept(StringToId(sid), index);
  if result then
    result := GetConceptRefSet(index, true, members) > 0;
end;

procedure TSnomedServices.Save(const sFilename: String);
var
  oFile : Tfilestream;
  oWrite : TWriter;
  i : integer;
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
      oWrite.WriteString(SNOMED_CACHE_VERSION);
      oWrite.WriteString(Version);
      oWrite.WriteString(FStrings.FMaster);
      oWrite.WriteString(FRefs.FMaster);
      oWrite.WriteString(FDesc.FMaster);
      oWrite.WriteString(FWords.FMaster);
      oWrite.WriteString(FStems.FMaster);
      oWrite.WriteString(FConcept.FMaster);
      oWrite.WriteString(FRel.FMaster);
      oWrite.WriteString(FRefSetIndex.FMaster);
      oWrite.WriteString(FRefSetMembers.FMaster);
      oWrite.WriteString(FDescRef.FMaster);
      oWrite.writeInteger(FIs_a_Index);
      oWrite.writeInteger(length(FInactiveRoots));
      for i := 0 to Length(FInactiveRoots) - 1 Do
        oWrite.writeInteger(FInactiveRoots[i]);
      oWrite.writeInteger(length(FActiveRoots));
      for i := 0 to Length(FActiveRoots) - 1 Do
        oWrite.writeInteger(FActiveRoots[i]);
    Finally
      oWrite.Free;
    End;
  Finally
    oFile.Free;
  End;
end;

function TSnomedServices.SearchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
var
  res : TSnomedFilterContext;
begin
  res := TSnomedFilterContext.Create;
  try
    res.matches := Search(0, filter.filter, 0, false);
    result := res.Link;
  finally
    res.Free;
  end;
end;

function FindCardinalInArray(a : TCardinalArray; iValue : Cardinal; var iIndex : Integer):Boolean;
Var
  L, H, I : Integer;
Begin
  Result := False;
  L := 0;
  H := Length(a) - 1;

  While L <= H Do
  Begin
    I := (L + H) Shr 1;

    If a[i] < iValue Then
      L := I + 1
    Else
    Begin
      H := I - 1;

      If a[i] = iValue Then
      Begin
        Result := True;
        L := I;
      End;
    End;
  End;

  iIndex := L;
End;

function TSnomedServices.Subsumes(iParent, iChild: Cardinal): Boolean;
var
  iAllDesc : TCardinalArray;
  iIndex : integer;
Begin
  if iParent = iChild then
    result := true
  else
  begin
    iAllDesc := FRefs.GetReferences(FConcept.GetAllDesc(iParent));
    result := FindCardinalInArray(iAllDesc, iChild, iIndex);
  end;
End;

type
  TSearchWord = record
    original : AnsiString;
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



function TSnomedServices.Search(iRoot : Int64; sText: AnsiString; iLang : Cardinal; bInactive : Boolean): TMatchArray;
var
  aLangMembers : TSnomedReferenceSetMemberArray;

  Function Match(const words : TSearchWordArray; s : AnsiString; iDepth : byte):Double;
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
        Else If StringEndsWith(s, words[i].original) Then
          result := result + 1.5
        Else If (pos(' '+words[i].original+' ', s) > 0) Then
          result := result + 0.6;
        result := result + result / length(s);
        if iDepth > 0 Then
          result := result + result / iDepth;
      End;
  End;

  Procedure AddResult(var iCount : Integer; iindex : cardinal; id : Int64; priority : Double);
  Begin
    if iCount = length(result) then
      SetLength(result, Length(result)+100);
    result[iCount].index := iIndex;
    result[iCount].term := id;
    result[iCount].Priority := priority;
    Inc(iCount);
  End;

  Function FlagFactor(iFlag : Byte): double;
  Begin
    if (iFlag and MASK_DESC_STYLE) shr 4 in [VAL_DESC_Preferred] then
      result := 4
    Else
      result := 1;
  End;

  Procedure CheckConcept(const words : TSearchWordArray; var iCount : Integer; iConceptIndex : Integer);
  var
    i, j : cardinal;
    r1, r2, t : Double;
    iDepth : Byte;
    Identity, iID2 : int64;
    Flags : Byte;
    Parents : Cardinal;
    Descriptions : Cardinal;
    Inbounds : Cardinal;
    outbounds : Cardinal;
    refsets : Cardinal;
    Desc : TCardinalArray;
    iWork, iDummy, module, kind : Cardinal;
    date : TSnomedDate;
  Begin
    SetLength(desc, 0);
    Concept.GetConcept(iConceptIndex, Identity, Flags, date, Parents, Descriptions, outbounds, Inbounds, refsets);
    iDepth := Concept.GetDepth(iConceptIndex);
    if bInactive Or ((Flags and MASK_CONCEPT_STATUS) = FLAG_Active) Then
    Begin
      r1 := 0;
      r2 := 0;
      t := 0;
      Desc := Refs.GetReferences(FConcept.GetStems(iConceptIndex));
      For i := 0 to length(words) - 1 do
        if words[i].stem <> 0 Then
          For j := 0 to length(Desc) - 1 do
            if (words[i].stem = desc[j]) Then
              r1 := r1 + 20 + (20 / length(desc))
            else
              assert(FStrings.GetEntry(words[i].stem) <> FStrings.GetEntry(desc[j]));

      Desc := Refs.GetReferences(Descriptions);
      for j := Low(Desc) to High(Desc) Do
      Begin
        FDesc.GetDescription(Desc[j], iWork, iID2, date, iDummy, module, kind, refsets, flags);
        t := t + FlagFactor(flags);
        r2 := r2 + Match(words, Strings.GetEntry(iWork), iDepth) * FlagFactor(flags);
      End;
      if r1 + r2 > 0 Then
        AddResult(iCount, iConceptIndex, Identity, r1 + r2 / t);
    End;
  End;
var
  iCount : Integer;
  words : TSearchWordArray;
  iAll : TCardinalArray;
  i : integer;
  iC, iMembers : Cardinal;
  aMembers : TSnomedReferenceSetMemberArray;
  index : integer;
  oStemmer : TYuStemmer_8;
  s : AnsiString;
  s1 : AnsiString;
begin
  SetLength(words, 0);
  SetLength(aMembers, 0);
  SetLength(aLangMembers, 0);
  sText := LowerCase(sText);
  oStemmer := GetStemmer_8('english');
  Try
    while (sText <> '') Do
    Begin
      AnsiStringSplit(sText, [',', ' ', ':', '.', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '{', '}', '[', ']', '|', '\', ';', '"', '<', '>', '?', '/', '~', '`', '-', '_', '+', '='], s, sText);
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

  if iLang <> 0 Then
    aLangMembers := FRefSetMembers.GetMembers(iLang);

  iCount := 0;
  SetLength(result, 100);
  sText := lowercase(sText);
  if iRoot = 0 Then
  Begin
    {if iLang <> 0 Then
      for i := 0 to Length(aLangMembers) - 1 Do
        CheckDescription(words, iCount, aLangMembers[i].Ref)
    else}
      for i := 0 to Concept.Count - 1 Do
        CheckConcept(words, iCount, i * CONCEPT_SIZE  + 1)
  end
  Else if Concept.FindConcept(iRoot, iC) Then
  Begin
    iMembers := FRefSetIndex.GetMembersByConcept(iC, true);
    if iMembers = 0 then
      iAll := FRefs.GetReferences(FConcept.GetAllDesc(iC))
    else
    Begin
      aMembers := FRefSetMembers.GetMembers(iMembers);
      SetLength(iAll, length(aMembers));
      For i := 0 to Length(iAll) - 1 Do
        iAll[i] := aMembers[i].Ref;
    End;
    for i := 0 to length(iAll) - 1 Do
      CheckConcept(words, iCount, iAll[i]);
  End;

  SetLength(result, iCount);
  QuickSortArray(result);
end;

function TSnomedServices.Subsumes(const sParent, sChild: AnsiString): Boolean;
var
  iParent : Cardinal;
  iChild : Cardinal;
begin
  Result := Concept.FindConcept(StringToId(sParent), iParent);
  if not result then
  begin
    result := DescRef.FindDescription(StringToId(sParent), iParent);
    if result then
      iParent := Desc.ConceptByIndex(iParent);
  end;
  if result then
  begin
    Result := Concept.FindConcept(StringToId(sChild), iChild);
    if not result then
    begin
      result := DescRef.FindDescription(StringToId(sChild), iChild);
      if result then
        iChild := Desc.ConceptByIndex(iChild);
    end;
    if Result Then
      result := Subsumes(iParent, iChild);
  end;
end;


function FindMember(aMembers : TSnomedReferenceSetMemberArray; iRef : Cardinal; var iIndex: integer): boolean;
var
  L, H, I: Integer;
  c : Int64;
begin
  Result := False;
  L := 0;
  H := length(aMembers) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := aMembers[i].Ref;
    c := c - iRef;
    if C < 0 then
      L := I + 1
    else
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
end;


function TSnomedServices.GetDisplayName(const iConcept, iLang: Cardinal): AnsiString;
var
  iLoop : integer;
  Identity, iId2 : int64;
  Flags : Byte;
  Parents : Cardinal;
  Descriptions : Cardinal;
  Descs : TCardinalArray;
  Inbounds : Cardinal;
  outbounds : Cardinal;
  iDesc, iDummy, module, kind, refsets : Cardinal;
  iInt : integer;
  date : TSnomedDate;
  aMembers : TSnomedReferenceSetMemberArray;
  iList : TCardinalArray;
  v : String;
begin
  SetLength(aMembers, 0);
  result := '';
  if iLang <> 0 then
    aMembers := FRefSetMembers.GetMembers(iLang);
  Concept.GetConcept(iConcept, Identity, Flags, date, Parents, Descriptions, Inbounds, outbounds, refsets);
  Descs := Refs.GetReferences(Descriptions);
  For iLoop := 0 to High(descs) Do
  Begin
    Desc.GetDescription(descs[iLoop], iDesc, iId2, date, iDummy, module, kind, refsets, Flags);
    if (Flags and MASK_DESC_STATUS = FLAG_Active) And (Flags and MASK_DESC_STYLE shr 4 in [VAL_DESC_Preferred]) And ((iLang = 0) or (FindMember(aMembers, descs[iLoop], iint))) Then
      result := Strings.GetEntry(iDesc);
  End;
  // ok, didn't find an active preferred term in the language of preference. Let's try for any term in the language
  if (result = '') and (iLang <> 0) then
    For iLoop := 0 to High(descs) Do
    Begin
      Desc.GetDescription(descs[iLoop], iDesc, iId2, date, iDummy, module, kind, refsets, Flags);
      if (Flags and MASK_DESC_STATUS = FLAG_Active) And (FindMember(aMembers, descs[iLoop], iInt)) Then
        result := Strings.GetEntry(iDesc);
    End;
  // if we still haven't found, then any preferred term
  if (result = '') then
    For iLoop := 0 to High(descs) Do
    Begin
      Desc.GetDescription(descs[iLoop], iDesc, iId2, date, iDummy,  module, kind, refsets, Flags);
      if (Flags and MASK_DESC_STATUS = FLAG_Active) And (Flags and MASK_DESC_STYLE shr 4 in [VAL_DESC_Preferred]) Then
        result := Strings.GetEntry(iDesc);
    End;
  // still not found? well, we'll pick the shortest description that's in any value set
  if result = '' then // ok,
    For iLoop := Low(descs) To High(descs) Do
    Begin
      Desc.GetDescription(descs[iLoop], iDesc, iId2, date, iDummy,  module, kind, refsets, Flags);
      iList := Refs.GetReferences(refsets);
      v := Strings.GetEntry(iDesc);
      if ((result = '') or (length(result) > length(v))) and (Flags and MASK_DESC_STATUS = FLAG_Active) And (Length(iList) > 0) Then
        result := v;
    End;
  if result = '' Then // ok, give up. and use the FSN
    For iLoop := Low(descs) To High(descs) Do
    Begin
      Desc.GetDescription(descs[iLoop], iDesc, iId2, date, iDummy,  module, kind, refsets, Flags);
      if (Flags and MASK_DESC_STATUS = FLAG_Active) And (Flags and MASK_DESC_STYLE shr 4 in [VAL_DESC_FullySpecifiedName]) Then
        result := Strings.GetEntry(iDesc);
    End;
end;

function TSnomedServices.GetDisplayName(const sTerm, sLangSet: AnsiString): AnsiString;
var
  iTerm, iLang : Cardinal;
begin
  iLang := CheckLangSet(sLangSet);
  if not Concept.FindConcept(StringToId(sTerm), iTerm) Then
    raise Exception.Create('Concept '+sTerm+' not found');
  result := GetDisplayName(iTerm, iLang);
end;

Procedure TSnomedServices.ListDisplayNames(list : TStringList; Const iConcept, iLang : Cardinal; FlagMask : Byte);
var
  aMembers : TSnomedReferenceSetMemberArray;
  iLoop : integer;
  Identity, iID2 : int64;
  Flags : Byte;
  Parents, Descriptions, Inbounds, outbounds, refsets : Cardinal;
  Descs : TCardinalArray;
  iDesc, iDummy, module, kind : Cardinal;
  iInt : Integer;
  date : TSnomedDate;
begin
  if iLang <> 0 then
    aMembers := FRefSetMembers.GetMembers(iLang);
  Concept.GetConcept(iConcept, Identity, Flags, date, Parents, Descriptions, Inbounds, outbounds, refsets);
  Descs := Refs.GetReferences(Descriptions);
  For iLoop := 0 to High(descs) Do
  Begin
    Desc.GetDescription(descs[iLoop], iDesc, iId2, date, iDummy, module, kind, refsets, Flags);
    if ((Flags and flagMask > 0) or (flagMask = $FF))  And ((iLang = 0) or (FindMember(aMembers, descs[iLoop], iInt))) Then
      list.add(Strings.GetEntry(iDesc));
  End;
end;

Procedure TSnomedServices.ListDisplayNames(list : TStringList; Const sTerm, sLangSet : AnsiString; FlagMask : Byte);
var
  iTerm, iLang : Cardinal;
begin
  iLang := CheckLangSet(sLangSet);
  if not Concept.FindConcept(StringToId(sTerm), iTerm) Then
    raise Exception.Create('Concept '+sTerm+' not found');
  ListDisplayNames(list, iTerm, iLang, flagmask);
end;

procedure TSnomedServices.GetMatchInfo(iConcept: Cardinal; var sTerm, sFSN, sPreferred: AnsiString);
var
  iLoop : integer;
  Identity, iId2 : int64;
  Flags : Byte;
  Parents : Cardinal;
  Descriptions : Cardinal;
  Descs : TCardinalArray;
  Inbounds : Cardinal;
  outbounds : Cardinal;
  iDesc, iDummy, module, kind, refsets : Cardinal;
  date : TSnomedDate;
begin
  sTerm := '';
  sFSN := '';
  sPreferred := '';
  Concept.GetConcept(iConcept, Identity, Flags, date, Parents, Descriptions, Inbounds, outbounds, refsets);
  sTerm := IntToStr(Identity);
  Descs := Refs.GetReferences(Descriptions);
  For iLoop := 0 to High(descs) Do
  Begin
    Desc.GetDescription(descs[iLoop], iDesc, iId2, date, iDummy, module, kind, refsets, Flags);
      if (Flags and MASK_DESC_STATUS = FLAG_Active) And (Flags and MASK_DESC_STYLE = VAL_DESC_FullySpecifiedName shl 4) Then
       sFSN := Strings.GetEntry(iDesc)
      else if (Flags and MASK_DESC_STATUS = FLAG_Active) And (Flags and MASK_DESC_STYLE shr 4 in [VAL_DESC_Preferred]) Then
        sPreferred := Strings.GetEntry(iDesc);
      if (sPreferred <> '') and (sFSN <> '') then
        break;
  End;
end;
{
function TSnomedServices.FindWord(s: AnsiString; var index : integer): Boolean;
var
  L, H, I, c: Integer;
begin
  Result := False;
  L := 0;
  H := FWords.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStr(FStrings.GetEntry(FWords.GetString(i)), s);
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
    }
function TSnomedServices.ConceptExists(conceptId: String): Boolean;
var
  i : cardinal;
begin
  result := FConcept.FindConcept(StringToIdOrZero(conceptId), i);
end;

function TSnomedServices.FindStem(s: AnsiString; var index: Integer): Boolean;
var
  L, H, I, c: Integer;
begin
  Result := False;
  L := 0;
  H := FStems.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStr(FStrings.GetEntry(FStems.GetString(i)), s);
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

function TSnomedServices.Link: TSnomedServices;
begin
  result := TSnomedServices(inherited link);
end;

function TSnomedServices.GetConceptRefSet(iConcept: Cardinal; bByName : Boolean; var iMembers : cardinal): Cardinal;
var
  i : integer;
  c : Cardinal;
  iDummy : cardinal;
begin
  result := 0;
  For i := 0 to FRefSetIndex.Count - 1 do
  Begin
    if bByName Then
      FRefSetIndex.GetReferenceSet(i, c, iDummy, iMembers)
    else
      FRefSetIndex.GetReferenceSet(i, c, iMembers, iDummy);
    if c = iConcept Then
    Begin
      result := c;
      exit;
    End;
  End;
end;

function TSnomedServices.GetDescRefsets(iDesc: Cardinal): TCardinalArray;
var
  i : integer;
  iDefinition, iMembersByRef, iMembersByName: Cardinal;
  aMembers : TSnomedReferenceSetMemberArray;
  iDummy : Integer;
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

function TSnomedServices.GetConceptRefsets(iDesc: Cardinal): TCardinalArray;
var
  i : integer;
  iDefinition, iMembersByRef, iMembersByName: Cardinal;
  aMembers : TSnomedReferenceSetMemberArray;
  iDummy : Integer;
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

function TSnomedServices.StringIsId(const s: AnsiString; var iId: Int64): Boolean;
begin
  iId := StrToInt64Def(s, -1);
  result := iId > -1;
end;

function TSnomedServices.StringToId(const s: String): Int64;
begin
  result := StrToInt64(s);
end;

function TSnomedServices.CheckLangSet(sTerm: AnsiString): Cardinal;
var
  iId  : Int64;
begin
  result := 0;
  if sTerm <> '' Then
  Begin
    if StringIsId(sterm, iId) And Concept.FindConcept(iId, result) Then
      result := FRefSetIndex.GetMembersByConcept(result, false);
    if result = 0 Then
      Raise Exception.Create('Unable to resolve the language reference set '+sTerm);
  End
end;

function TSnomedServices.GetConceptDescendents(index: Cardinal): TCardinalArray;
begin
  result := FRefs.GetReferences(FConcept.GetAllDesc(index));
end;

function TSnomedServices.GetConceptId(const iConcept : Cardinal): AnsiString;
var
  Identity : int64;
  Flags : Byte;
  Parents, Inbounds, outbounds, descriptions, refsets : Cardinal;
  date : TSnomedDate;
begin
  Concept.GetConcept(iConcept, Identity, Flags, date, Parents, Descriptions, Inbounds, outbounds, refsets);
  result := inttostr(Identity);
end;

function TSnomedServices.StringToIdOrZero(const s: AnsiString): Int64;
begin
  if StringIsInteger64(s) then
    result := StrToInt64(s)
  Else
    result := 0;
end;

function TSnomedServices.IsValidConcept(const sTerm: String): Boolean;
var
  iTerm : Cardinal;
begin
  result := Concept.FindConcept(StringToId(sTerm), iTerm);
end;

function TSnomedServices.IsValidDescription(const sTerm: AnsiString; var concept: int64; var description: String): Boolean;
var
  iTerm, desc, cId, module, kind, refsets : Cardinal;
  flags : Byte;
  id : int64;
  date : TSnomedDate;
begin
  result := DescRef.FindDescription(StringToId(sTerm), iTerm);
  if result then
  begin
    FDesc.GetDescription(iTerm, desc, id, date, cId, module, kind, refsets, flags);
    concept := FConcept.getConceptId(cId);
    description := FStrings.GetEntry(desc);
  end;
end;

{ TSnomedRelationshipList }

procedure TSnomedRelationshipList.StartBuild;
begin
  FBuilder := TAnsiStringBuilder.Create;
end;

Function TSnomedRelationshipList.AddRelationship(Source, Target, RelType, module, kind, modifier : Cardinal; date : TSnomedDate; Flags, Group : Byte) : Cardinal;
begin
  Result := FBuilder.Length+1;
  FBuilder.AddCardinalAsBytes(Source);
  FBuilder.AddCardinalAsBytes(Target);
  FBuilder.AddCardinalAsBytes(RelType);
  FBuilder.AddCardinalAsBytes(module);
  FBuilder.AddCardinalAsBytes(kind);
  FBuilder.AddCardinalAsBytes(modifier);
  FBuilder.AddWordAsBytes(date);
  FBuilder.AddByteAsBytes(Flags);
  FBuilder.AddByteAsBytes(Group);
End;

procedure TSnomedRelationshipList.DoneBuild;
begin
  FMaster := FBuilder.AsString;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TSnomedRelationshipList.GetRelationship(iIndex: Cardinal; var Source, Target, RelType, module, kind, modifier : Cardinal; var date : TSnomedDate; var Flags, Group : Byte);
// (iIndex: Cardinal; var Source, Target, RelType: Cardinal; var Flags, Group : Byte);
begin
  if (iIndex >= FLength) then
    Raise Exception.Create('Wrong length index getting snomed relationship Details');
  Move(FMaster[iIndex+0], Source, 4);
  Move(FMaster[iIndex+4], Target, 4);
  Move(FMaster[iIndex+8], RelType, 4);
  Move(FMaster[iIndex+12], module, 4);
  Move(FMaster[iIndex+16], kind, 4);
  Move(FMaster[iIndex+20], modifier, 4);
  Move(FMaster[iIndex+24], date, 2);
  Move(FMaster[iIndex+26], Flags, 1);
  Move(FMaster[iIndex+27], Group, 1);
end;

{ TSnomedWords }

procedure TSnomedWords.AddWord(index: Cardinal; Flags: Byte);
begin
  FBuilder.AddCardinalAsBytes(index);
  FBuilder.AddByteAsBytes(flags);
end;

function TSnomedWords.Count: Integer;
begin
  result := FLength div 5;
end;

procedure TSnomedWords.DoneBuild;
begin
  FMaster := FBuilder.AsString;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TSnomedWords.GetEntry(iIndex: Cardinal; var index: Cardinal; var flags: Byte);
var
  l : Cardinal;
begin
  l := (iIndex * 5) + 1;
  if l > FLength - 4 Then
    raise Exception.create('invalid index');
  move(FMaster[l], index, 4);
  move(FMaster[l+4], flags, 1);
end;

function TSnomedWords.GetString(iIndex: Cardinal): Cardinal;
var
  f : byte;
begin
  GetEntry(iIndex, result, f);
end;

procedure TSnomedWords.StartBuild;
begin
  FBuilder := TAnsiStringBuilder.Create;
end;

{ TSnomedStems }

procedure TSnomedStems.AddStem(index: Cardinal; reference : Cardinal);
begin
  FBuilder.AddCardinalAsBytes(index);
  FBuilder.AddCardinalAsBytes(reference);
end;

function TSnomedStems.Count: Integer;
begin
  result := FLength div 8;
end;

procedure TSnomedStems.DoneBuild;
begin
  FMaster := FBuilder.AsString;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TSnomedStems.GetEntry(iIndex: Cardinal; var index: Cardinal; var reference: Cardinal);
var
  l : Cardinal;
begin
  l := (iIndex * 8) + 1;
  if l > FLength - 7 Then
    raise Exception.create('invalid index');
  move(FMaster[l], index, 4);
  move(FMaster[l+4], reference, 4);
end;

function TSnomedStems.GetString(iIndex: Cardinal): Cardinal;
var
  f : cardinal;
begin
  GetEntry(iIndex, result, f);
end;

procedure TSnomedStems.StartBuild;
begin
  FBuilder := TAnsiStringBuilder.Create;
end;

{ TSnomedServiceList }

destructor TSnomedServiceList.Destroy;
begin
  FDefinition.Free;
  inherited;
end;

function TSnomedServiceList.GetDefinition(iIndex: Integer): TSnomedServices;
begin
  result := TSnomedServices(ObjectByIndex[iIndex]);
end;

function TSnomedServiceList.GetDefinitionByName(sName: AnsiString): TSnomedServices;
var
  i : integer;
begin
  if sName = '' then
    result := DefaultDefinition
  Else
  Begin
    Result := nil;
    i := 0;
    While (i < Count) and (result = nil) do
    Begin
      if SameText(Definition[i].Version, sName) then
        result := Definition[i];
      inc(i);
    End;
  End;
end;

function TSnomedServiceList.HasDefaultDefinition: Boolean;
begin
  result := FDefinition <> Nil;
end;

function TSnomedServiceList.ItemClass: TAdvObjectClass;
begin
  result := TSnomedServices;
end;

procedure TSnomedServiceList.SetDefinition(const Value: TSnomedServices);
begin
  FDefinition.Free;
  FDefinition := Value;
end;

{ TSnomedReferenceSetIndex }

Procedure TSnomedReferenceSetIndex.AddReferenceSet(iDefinition, iMembersByRef, iMembersByName: Cardinal);
begin
  FBuilder.AddCardinalAsBytes(iDefinition);
  FBuilder.AddCardinalAsBytes(iMembersByRef);
  FBuilder.AddCardinalAsBytes(iMembersByName);
end;

function TSnomedReferenceSetIndex.Count: Integer;
begin
  result := FLength div 12;
end;

procedure TSnomedReferenceSetIndex.DoneBuild;
begin
  FMaster := FBuilder.AsString;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

function TSnomedReferenceSetIndex.GetMembersByConcept(iIndex: Cardinal; bByName : Boolean): Cardinal;
var
  i, v : Cardinal;
begin
  result := 0;
  For i := 0 to Count - 1 Do
  begin
    Move(FMaster[i * 12 + 1], v, 4);
    if v = iIndex Then
    Begin
      if bByName Then
        Move(FMaster[i * 12 + 9], result, 4)
      Else
        Move(FMaster[i * 12 + 5], result, 4);
      exit;
    End;
  End;
end;

procedure TSnomedReferenceSetIndex.GetReferenceSet(iIndex: Cardinal; var iDefinition, iMembersByRef, iMembersByName: Cardinal);
begin
  iIndex := iIndex * 12 + 1;
  if (iIndex >= FLength) then
    Raise Exception.Create('Wrong length index getting snomed relationship Details');
  Move(FMaster[iIndex+0], iDefinition, 4);
  Move(FMaster[iIndex+4], iMembersByRef, 4);
  Move(FMaster[iIndex+8], iMembersByName, 4);
end;

procedure TSnomedReferenceSetIndex.StartBuild;
begin
  FBuilder := TAnsiStringBuilder.Create;
end;

{ TSnomedDescriptionIndex }

procedure TSnomedDescriptionIndex.AddDescription(id: Int64; reference: Cardinal);
begin
  FBuilder.AddInt64AsBytes(id);
  FBuilder.AddCardinalAsBytes(reference);
end;

procedure TSnomedDescriptionIndex.DoneBuild;
begin
  FMaster := FBuilder.AsString;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

function TSnomedDescriptionIndex.FindDescription(iIdentity: int64; var IIndex: Cardinal): boolean;
var
  aConcept : int64;
  L, H, I: Integer;
  c : Int64;
begin
  Result := False;
  L := 0;
  H := FLength;
  H := (H div 12) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    Move(FMaster[(i*12)+1], aConcept, 8);
    C := aConcept - iIdentity;
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
  if result then
    Move(FMaster[L*12+1+8], iIndex, 4);
end;

procedure TSnomedDescriptionIndex.StartBuild;
begin
  FBuilder := TAnsiStringBuilder.Create;
end;

{ TSnomedReferenceSetMembers }

function TSnomedReferenceSetMembers.AddMembers(const a: TSnomedReferenceSetMemberArray): Cardinal;
var
  iLoop : Integer;
Begin
  result := FBuilder.Length + 1;
  FBuilder.AddCardinalAsBytes(length(a));
  for iLoop := Low(a) to High(a) Do
  begin
    FBuilder.AddByteAsBytes(a[iLoop].kind);
    FBuilder.AddCardinalAsBytes(a[iLoop].Ref);
  end;
end;

procedure TSnomedReferenceSetMembers.DoneBuild;
begin
  FMaster := FBuilder.AsString;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

function TSnomedReferenceSetMembers.GetMemberCount(iIndex : Cardinal) : integer;
begin
  if iIndex = MAGIC_NO_CHILDREN then
    result := 0
  Else
  Begin
    if (iIndex > FLength) then
      Raise Exception.Create('Wrong length index getting Snomed list');
    move(FMaster[iIndex], result, 4);
  End;
end;

function TSnomedReferenceSetMembers.GetMembers(iIndex: Cardinal): TSnomedReferenceSetMemberArray;
var
  i : integer;
  c : Cardinal;
begin
  if iIndex = MAGIC_NO_CHILDREN then
    result := nil
  Else
  Begin
    if (iIndex > FLength) then
      Raise Exception.Create('Wrong length index getting Snomed list');
    move(FMaster[iIndex], c, 4);
    SetLength(Result, c);
    inc(iIndex, 4);
    for i := 0 to Length(result)-1 Do
    Begin
      move(FMaster[iIndex], result[i].kind, 1);
      inc(iIndex, 1);
      move(FMaster[iIndex], result[i].Ref, 4);
      inc(iIndex, 4);
    End;
  End;
end;

procedure TSnomedReferenceSetMembers.StartBuild;
begin
  FBuilder := TAnsiStringBuilder.Create;
end;

function TSnomedServices.buildValueSet(id : String): TFhirValueSet;
var
  inc : TFhirValueSetComposeInclude;
  filt :  TFhirValueSetComposeIncludeFilter;
begin
  if id.StartsWith('http://snomed.info/sct/') And ReferenceSetExists(id.Substring(23)) then
  begin
    result := TFhirValueSet.Create;
    try
      result.identifierST := id;
      result.versionST := Version;
      result.nameST := 'SNOMED CT Reference Set '+id.Substring(23);
      result.descriptionST := GetDisplayName(id.Substring(23), '');
      result.statusST := ValuesetStatusActive;
      result.dateST := NowUTC;
      result.compose := TFhirValueSetCompose.Create;
      inc := result.compose.includeList.Append;
      inc.systemST := 'http://snomed.info/sct';
      filt := inc.filterList.Append;
      filt.property_ST := 'concept';
      filt.opST := FilterOperatorIn;
      filt.valueST := id.Substring(23);
      result.link;
    finally
      result.free;
    end;
  end
  else if id.StartsWith('http://snomed.info/id/') And ConceptExists(id.Substring(22)) then
  begin
    result := TFhirValueSet.Create;
    try
      result.identifierST := id;
      result.versionST := Version;
      result.nameST := 'SNOMED CT Concept '+id.Substring(22)+' and descendents';
      result.descriptionST := 'All Snomed CT concepts for '+GetDisplayName(id.Substring(22), '');
      result.statusST := ValuesetStatusActive;
      result.dateST := NowUTC;
      result.compose := TFhirValueSetCompose.Create;
      inc := result.compose.includeList.Append;
      inc.systemST := 'http://snomed.info/sct';
      filt := inc.filterList.Append;
      filt.property_ST := 'concept';
      filt.opST := FilterOperatorIsA;
      filt.valueST := id.Substring(22);
      result.link;
    finally
      result.free;
    end;
  end
  else
    result := nil;

end;

function TSnomedServices.ChildCount(context: TCodeSystemProviderContext): integer;
var
  i : integer;
  Identity : int64;
  Flags, Group : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  iWork, iWork2, iWork3, iWork4, iWork5, iWork6, refsets : Cardinal;
  date : TSnomedDate;
  Inbounds : TCardinalArray;
begin
  SetLength(inbounds, 0);
  if (context = nil) then
    result := 1
  else
  begin
    Concept.GetConcept(Cardinal(context), Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
    Inbounds := Refs.GetReferences(InboundIndex);
    result := 0;
    For i := 0 to High(Inbounds) Do
    Begin
      Rel.GetRelationship(Inbounds[i], iWork, iWork2, iWork3, iWork4, iWork5, iWork6, date, Flags, Group);
      if iWork3 = Is_a_Index Then
        inc(result);
    End;
  end;
end;

procedure TSnomedServices.Close(ctxt: TCodeSystemProviderContext);
begin
  // nothing - it's just a pointer to some memory structure
end;

function TSnomedServices.Code(context: TCodeSystemProviderContext): string;
var
  Identity : int64;
  Flags : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex, refsets : Cardinal;
  Inbounds : TCardinalArray;
  date : TSnomedDate;
begin
  SetLength(inbounds, 0);
  Concept.GetConcept(Cardinal(context), Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
  result := inttostr(identity);
end;

function TSnomedServices.getcontext(context: TCodeSystemProviderContext; ndx: integer): TCodeSystemProviderContext;
var
  i, c : integer;
  Identity : int64;
  Flags, Group : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  iWork, iWork2, iWork3, iWork4, iWork5, iWork6, refsets : Cardinal;
  Inbounds : TCardinalArray;
  date : TSnomedDate;
begin
  result := nil;
  SetLength(inbounds, 0);
  if (context = nil) then
    result := TCodeSystemProviderContext(Is_a_Index)
  else
  begin
    Concept.GetConcept(Cardinal(context), Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
    Inbounds := Refs.GetReferences(InboundIndex);
    c := -1;
    For i := 0 to High(Inbounds) Do
    Begin
      Rel.GetRelationship(Inbounds[i], iWork, iWork2, iWork3, iWork4, iWork5, iWork6, date, Flags, Group);
      if iWork3 = Is_a_Index Then
      begin
        inc(c);
        if (c = ndx) then
        begin
          result := TCodeSystemProviderContext(iWork);
          exit;
        end;
      end;
    End;
  end;
end;


function TSnomedServices.getDefinition(code: String): String;
begin
  result := '';
end;

Function TSnomedServices.GetFSN(iDescriptions : TCardinalArray) : String;
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
    Desc.GetDescription(iDescriptions[iLoop], iString, iId, date, iDummy, module, kind, refsets, iFlag);
    if (iFlag and MASK_DESC_STATUS = FLAG_Active) And (iFlag and MASK_DESC_STYLE = VAL_DESC_FullySpecifiedName shl 4) Then
      result := Strings.GetEntry(iString);
  End;
End;


Function TSnomedServices.GetPN(iDescriptions : TCardinalArray) : String;
var
  iLoop : Integer;
  iid : int64;
  iString, iDummy, module, refsets, kind : Cardinal;
  date : TSnomedDate;
  iFlag : Byte;
begin
  result := '';
  For iLoop := Low(iDescriptions) To High(iDescriptions) Do
  Begin
    Desc.GetDescription(iDescriptions[iLoop], iString, iId, date, iDummy, module, kind, refsets, iFlag);
    if (iFlag and MASK_DESC_STATUS = FLAG_Active) And (iFlag and MASK_DESC_STYLE shr 4 in [VAL_DESC_Preferred]) Then
      result := Strings.GetEntry(iString);
  End;
  if result = '' Then
    result := GetFSN(iDescriptions);
End;

function TSnomedServices.Display(context: TCodeSystemProviderContext): string;
begin
  result := GetDisplayName(Cardinal(context), 0);
end;

procedure TSnomedServices.Displays(context: TCodeSystemProviderContext; list: TStringList);
begin
  Displays(Code(context), list);
end;

procedure TSnomedServices.Displays(code: String; list: TStringList);
var
  ctxt : TAdvObject;
begin
  ctxt := locate(code);
  if (ctxt = nil) then
    raise Exception.create('Unable to find '+code+' in '+system)
  else
    ListDisplayNames(list, Cardinal(ctxt), 0, $FF);
end;

function TSnomedServices.getDisplay(code: String): String;
var
  ctxt : TCodeSystemProviderContext;
begin
  ctxt := locate(code);
  try
    if (ctxt = nil) then
      raise Exception.create('Unable to find '+code+' in '+system)
    else
      result := Display(ctxt);
  finally
    Close(ctxt);
  end;
end;

function TSnomedServices.IsAbstract(context: TCodeSystemProviderContext): boolean;
begin
  result := false; // snomed don't do abstract?
end;

function TSnomedServices.locate(code: String): TCodeSystemProviderContext;
var
  iId : Int64;
  index : cardinal;
begin
  iId := StrToInt64Def(code, -1);
  if Concept.FindConcept(iId, index) Then
    result := TCodeSystemProviderContext(index)
  else
    raise exception.create('unable to find code '+code+' in '+system);
end;

function TSnomedServices.system: String;
begin
  result := 'http://snomed.info/sct';
end;

function TSnomedServices.TotalCount: integer;
begin
  result := Concept.Count;
end;

procedure TSnomedServices.Close(ctxt: TCodeSystemProviderFilterContext);
begin
  TSnomedFilterContext(ctxt).free;
end;

function TSnomedServices.filterIsA(id : Int64): TCodeSystemProviderFilterContext;
var
  res : TSnomedFilterContext;
  index : cardinal;
begin
  res := TSnomedFilterContext.Create;
  try
    if not Concept.FindConcept(id, index) then
      raise Exception.Create('The Snomed Concept '+inttostr(id)+' was not known');
    res.descendents := GetConceptDescendents(index);
    result := TSnomedFilterContext(res.link);
  finally
    res.Free;
  end;
end;

function TSnomedServices.filterIn(id : Int64): TCodeSystemProviderFilterContext;
var
  res : TSnomedFilterContext;
  index, members : cardinal;
begin
  res := TSnomedFilterContext.Create;
  try
    if not Concept.FindConcept(id, index) then
      raise Exception.Create('The Snomed Concept '+inttostr(id)+' was not known');
    if GetConceptRefSet(index, false, members) = 0 then
      raise Exception.Create('The Snomed Concept '+inttostr(id)+' is not a reference set');
    res.members := RefSetMembers.GetMembers(members);
    result := TSnomedFilterContext(res.link);
  finally
    res.Free;
  end;
end;

function TSnomedServices.filter(prop: String; op: TFhirFilterOperator; value: String; prep : TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
var
  id : Int64;
begin
  result := nil;
  if (prop = 'concept') and StringIsId(value, id) then
    if op = FilterOperatorIsA then
      result := filterIsA(id)
    else if op = FilterOperatorIn then
      result := filterIn(id);
end;

function TSnomedServices.FilterConcept(ctxt: TCodeSystemProviderFilterContext): TCodeSystemProviderContext;
begin
  if Length(TSnomedFilterContext(ctxt).matches) > 0 then
    result := TCodeSystemProviderContext(TSnomedFilterContext(ctxt).Matches[TSnomedFilterContext(ctxt).ndx-1].index)
  else if Length(TSnomedFilterContext(ctxt).members) > 0 then
    result := TCodeSystemProviderContext(TSnomedFilterContext(ctxt).Members[TSnomedFilterContext(ctxt).ndx-1].Ref)
  else
    result := TCodeSystemProviderContext(TSnomedFilterContext(ctxt).descendents[TSnomedFilterContext(ctxt).ndx-1]);
end;

function TSnomedServices.InFilter(ctxt: TCodeSystemProviderFilterContext; concept: TCodeSystemProviderContext): Boolean;
var
  index : integer;
begin
  if Length(TSnomedFilterContext(ctxt).members) > 0 then
    result := FindMember(TSnomedFilterContext(ctxt).Members, Cardinal(concept), index)
  else
    result := FindCardinalInArray(TSnomedFilterContext(ctxt).descendents, Cardinal(concept), index)
end;

function TSnomedServices.FilterMore(ctxt: TCodeSystemProviderFilterContext): boolean;
begin
  inc(TSnomedFilterContext(ctxt).ndx);
  if Length(TSnomedFilterContext(ctxt).matches) > 0 then
    result := TSnomedFilterContext(ctxt).ndx <= Length(TSnomedFilterContext(ctxt).matches)
  else if Length(TSnomedFilterContext(ctxt).members) > 0 then
    result := TSnomedFilterContext(ctxt).ndx <= Length(TSnomedFilterContext(ctxt).members)
  else
    result := TSnomedFilterContext(ctxt).ndx <= Length(TSnomedFilterContext(ctxt).descendents);
end;

function TSnomedServices.filterLocate(ctxt: TCodeSystemProviderFilterContext; code: String): TCodeSystemProviderContext;
begin
//  result := TSnomedFilterContext(ctxt).Members[;
end;

function TSnomedServices.locateIsA(code, parent: String): TCodeSystemProviderContext;
var
  ic, ip : Cardinal;
begin
  if Concept.FindConcept(StringToIdOrZero(parent), ip) And
       Concept.FindConcept(StringToIdOrZero(code), ic) And Subsumes(ip, ic) then
    result := TCodeSystemProviderContext(ic)
  else
    result := nil;
end;


End.
