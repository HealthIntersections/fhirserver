unit SnomedServices;

// URL: http://snomed.info/sct/[module]/version/[e.g. 20150131]'
//  intl: 900000000000207008
//  us:  731000124108
//  AU: 32506021000036107
//  Spanish: 449081005
//  Danish: 554471000005108
//  Dutch: 11000146104
//  Swedish: 45991000052106
//  UK: 999000041000000102

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
  SysUtils, Classes, Generics.Collections,
  StringSupport, FileSupport, BytesSupport,
  AdvStringLists, AdvObjectLists, AdvObjects,
  YuStemmer, DateAndTime,
  FHIRTypes, FHIRResources, FHIRUtilities, CDSHooksUtilities, FHIROperations,
  TerminologyServices;

Const
  SNOMED_CACHE_VERSION = '11';
  IS_A_MAGIC : UInt64 = 116680003;
  PREFERRED_MAGIC : UInt64 = 900000000000548007;
  ALL_DISPLAY_NAMES = $FF;

type
  ESnomedServices = class (Exception);

  UInt64Array = Array of UInt64;
  TCardinalArray = array of Cardinal;
  TMatch = record
    index : cardinal;
    term : UInt64;
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

  // We store snomed as three Strings
  //   each entry in the String starts with a byte length, and then the series of characters as bytes
  TSnomedStrings = class (TAdvObject)
    Private
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TAdvBytesBuilder;
    Public
      Function GetEntry(iIndex : Cardinal):String;

      Procedure StartBuild;
      Function AddString(Const s : String) : Cardinal;
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

  // stem word index. Every word is 8 bytes - a 4 byte index into the strings, and a 4 byte index into the references
  TSnomedStems = class (TAdvObject)
    Private
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TAdvBytesBuilder;
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
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TAdvBytesBuilder;
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
  DESC_SIZE = 35;
  MASK_DESC_STATUS = $0F; // bits 1-4
  MASK_DESC_STYLE = $30;  // bits 5-6
  MASK_DESC_CAPS_MASK = $C0;   // bits 7-8
  MASK_DESC_CAPS_NONE = $00;
  MASK_DESC_CAPS_FIRST = $40;
  MASK_DESC_CAPS_ALL = $80;

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
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TAdvBytesBuilder;
    Public
      function Count : Cardinal;
      Procedure GetDescription(iIndex : Cardinal; var iDesc : Cardinal; var id : UInt64; var date : TSnomedDate; var concept, module, kind, refsets, valueses : Cardinal; var iFlags : Byte);
      function ConceptByIndex(iIndex : Cardinal) : cardinal;


      Procedure StartBuild;
      Function AddDescription(iDesc : Cardinal; id : UInt64; date : TSnomedDate; concept, module, kind : Cardinal; iflags : Byte) : Cardinal;
      Procedure UpdateDetails(iIndex : Cardinal; id : UInt64; concept : Cardinal);
      Procedure DoneBuild;
      Procedure SetRefsets(iIndex : Cardinal; refsets, valueses : Cardinal);
  End;

  TSnomedDescriptionIndex = class (TAdvObject)
  Private
    FMaster : TBytes;
    FLength : Cardinal;
    FBuilder : TAdvBytesBuilder;
  Public
    Function FindDescription(iIdentity : UInt64; var IIndex : Cardinal) : boolean;

    Procedure StartBuild;
    procedure AddDescription(id : UInt64; reference : Cardinal);
    Procedure DoneBuild;
  End;

  // 4. a list of list of terms
  //   id as a 8 byte UInt64
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
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TAdvBytesBuilder;
    Public
      Function FindConcept(iIdentity : UInt64; var IIndex : Cardinal) : boolean;
      function getConceptId(iIndex : Cardinal) : UInt64;
      procedure GetConcept(iIndex : Cardinal; var Identity : UInt64; var Flags : Byte; var effectiveTime : TSnomedDate; var Parents : Cardinal; var Descriptions : Cardinal; var Inbounds : Cardinal; var outbounds : Cardinal; var refsets : Cardinal);
      Function GetParent(iIndex : Cardinal): Cardinal;
      Function GetIdentity(iIndex : Cardinal): UInt64;
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

      procedure SetFlag(iIndex: Cardinal; iFlags: Byte);
      procedure SetDate(iIndex: Cardinal; effectiveTime: TSnomedDate);

      // these presume that the terms are registered in order
      Procedure StartBuild;
      Function AddConcept(iIdentity : UInt64; effectiveTime : TSnomedDate; iFlags : Byte) : Cardinal;
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
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TAdvBytesBuilder;
    Public
      // for Persistence
      Procedure GetRelationship(iIndex: Cardinal; var identity : UInt64; var Source, Target, RelType, module, kind, modifier : Cardinal; var date : TSnomedDate; var Flags : Byte; var Group : Integer);

      Procedure StartBuild;
      Function AddRelationship(identity : UInt64; Source, Target, RelType, module, kind, modifier : Cardinal; date : TSnomedDate; Flags : Byte; Group : integer) : Cardinal;
      Procedure DoneBuild;
  End;

Type
  TSnomedReferenceSetMember = record
    kind : byte; // 0 = concept, 1 = desc, 2 = relationship
    Ref : Cardinal; // desc or term depending on ref set type
    values : Cardinal;
  End;
  TSnomedReferenceSetMemberArray = array of TSnomedReferenceSetMember;
  TRefSetMemberEntry = record
    refset : cardinal;
    types : cardinal;
    values : cardinal;
  end;
  TRefSetMemberEntryArray = array of TRefSetMemberEntry;

  // 2. a list of list of references
  TSnomedReferenceSetMembers = class (TAdvObject)
    Private
      FMaster : TBytes;
      FLength : Cardinal;
      FBuilder : TAdvBytesBuilder;
    Public
      Function GetMembers(iIndex : Cardinal) : TSnomedReferenceSetMemberArray;
      Function GetMemberCount(iIndex : Cardinal) : integer;

      Procedure StartBuild;
      Function AddMembers(Const a : TSnomedReferenceSetMemberArray) : Cardinal;
      Procedure DoneBuild;
  End;


  TSnomedReferenceSetIndex = class (TAdvObject)
  Private
    FMaster : TBytes;
    FLength : Cardinal;
    FBuilder : TAdvBytesBuilder;
  Public
    Procedure GetReferenceSet(iIndex: Cardinal; var iDefinition, iMembersByRef, iMembersByName, iFieldTypes: Cardinal);
    Function GetMembersByConcept(iIndex : Cardinal; bByName : Boolean) : Cardinal;
    Function Count : Integer;

    Procedure StartBuild;
    Procedure AddReferenceSet(iDefinition, iMembersByRef, iMembersByName, iFieldTypes: Cardinal);
    Procedure DoneBuild;
  End;

(*
{
operations
  isSubsumedBy(term, term)
  getDesc : String
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
    FActiveRoots : UInt64Array;
    FInactiveRoots : UInt64Array;
    FIs_a_Index, FPreferredTerm : Cardinal;
    FStrings : TSnomedStrings;
    FRefs : TSnomedReferences;
    FDesc : TSnomedDescriptions;
    FDescRef : TSnomedDescriptionIndex;
    FConcept : TSnomedConceptList;
    FRel : TSnomedRelationshipList;
    FRefSetIndex : TSnomedReferenceSetIndex;
    FRefSetMembers : TSnomedReferenceSetMembers;
    FVersionUri : String;
    FVersionDate : String;

    FWords : TSnomedWords;
    FStems : TSnomedStems;
    FLoaded: Boolean;

    function filterIn(id : UInt64): TCodeSystemProviderFilterContext;
    function filterIsA(id : UInt64): TCodeSystemProviderFilterContext;

  //  Function FindWord(s : String; var index : Integer) : Boolean;
    Function FindStem(s : String; var index : Integer) : Boolean;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function Link : TSnomedServices; Overload;
    Procedure Load(Const sFilename : String);
    Procedure Save(Const sFilename : String);

    // helper functions
    Function StringToId(Const s : String) : UInt64;
    Function StringToIdOrZero(Const s : String) : UInt64;
    Function StringIsId(Const s : String; var iId : UInt64) : Boolean;

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
    Property ActiveRoots : UInt64Array read FActiveRoots write FActiveRoots;
    Property InactiveRoots : UInt64Array read FInActiveRoots write FInActiveRoots;
    Property Is_a_Index : Cardinal read FIs_a_Index write FIs_a_Index;
    function Subsumes(iParent, iChild: Cardinal): Boolean; Overload;
    Function GetDisplayName(Const iConcept, iLang : Cardinal) : String; Overload;
    Procedure ListDisplayNames(list : TStringList; Const iConcept, iLang : Cardinal; FlagMask : Byte); Overload;
    Function GetConceptId(Const iConcept : Cardinal) : String; Overload;
    Procedure GetMatchInfo(iConcept : Cardinal; var sTerm, sFSN, sPreferred : String);
    Function GetConceptRefSet(iConcept : Cardinal; bByName : Boolean; var iMembers, iTypes : cardinal) : Cardinal;
    Function GetDescRefsets(iDesc : Cardinal) : TRefSetMemberEntryArray;
    Function GetConceptRefsets(iDesc : Cardinal) : TRefSetMemberEntryArray;
    Function CheckLangSet(sTerm : String) : Cardinal;
    function GetConceptDescendents(index : Cardinal) : TCardinalArray;
    Function GetPN(iDescriptions : TCardinalArray) : String;
    Function GetFSN(iDescriptions : TCardinalArray) : String;
    function GetPNForConcept(iIndex: Cardinal): String;

    // simplified interface for consumers
    Function ConceptExists(conceptId : String) : Boolean;
    Function Subsumes(Const sParent, sChild : String) : Boolean; Overload;
    Function Search(iRoot : UInt64; sText : String; iLang : Cardinal; bInactive : Boolean; bAll : boolean = false) : TMatchArray; overload;
    Function IsValidConcept(Const sTerm : String):Boolean;
    Function IsValidDescription(Const sTerm : String; var concept : UInt64; var description : String):Boolean;
    Function GetDisplayName(Const sTerm, sLangSet : String) : String; Overload;
    Procedure ListDisplayNames(list : TStringList; Const sTerm, sLangSet : String; FlagMask : Byte); Overload;
    function ReferenceSetExists(sid : String) : Boolean;

    // status stuff
    Property Loaded : Boolean read FLoaded write FLoaded;
    Property VersionUri : String read FVersionUri write FVersionUri;
    Property VersionDate : String read FVersionDate write FVersionDate;

    // generic terminology server interface
    function TotalCount : integer; override;
    function ChildCount(context : TCodeSystemProviderContext) : integer; override;
    function getcontext(context : TCodeSystemProviderContext; ndx : integer) : TCodeSystemProviderContext; override;
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
    function locateIsA(code, parent : String) : TCodeSystemProviderContext; override;
    function filterLocate(ctxt : TCodeSystemProviderFilterContext; code : String) : TCodeSystemProviderContext; override;
    function InFilter(ctxt : TCodeSystemProviderFilterContext; concept : TCodeSystemProviderContext) : Boolean; override;
    function buildValueSet(id : String) : TFhirValueSet;
    function searchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean) : TCodeSystemProviderFilterContext; overload; override;
    function getDefinition(code : String):String; override;
    function Definition(context : TCodeSystemProviderContext) : string; override;
    function isNotClosed(textFilter : TSearchFilterText; propFilter : TCodeSystemProviderFilterContext = nil) : boolean; override;
    procedure extendLookup(ctxt : TCodeSystemProviderContext; props : TList<String>; resp : TFHIRLookupOpResponse); override;

    procedure getCDSInfo(card : TCDSHookCard; baseURL, code, display : String); override;
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

    Function GetDefinitionByName(sName : String) : TSnomedServices;

    Property DefaultDefinition : TSnomedServices Read FDefinition write SetDefinition;
    Function HasDefaultDefinition : Boolean;

    Property Definition[iIndex : Integer] : TSnomedServices read GetDefinition; Default;
  End;


{
reference set

header....
  id  UInt64
members
  concept - cardinal
  desc - cardinal (or 0 if one is not specified)
  attribute - cardinal (0r 0 is one is not specified)

}
function FindMember(aMembers : TSnomedReferenceSetMemberArray; iRef : Cardinal; var iIndex: integer): boolean;
function FindCardinalInArray(a : TCardinalArray; iValue : Cardinal; var iIndex : Integer):Boolean;
Function GetDescType(Flags : Byte) : String;
Function GetDescStatus1(Flags : Byte) : String;
Function GetDescStatus2(Flags : Byte) : String;
Function GetRelChar(Flags : Byte) : String;
Function GetRelRefinability(Flags : Byte) : String;

Implementation

{ TSnomedStrings }

function TSnomedStrings.GetEntry(iIndex: Cardinal): String;
var
  i : Word;
begin
  if (iIndex > FLength) then
    Raise ESnomedServices.Create('Wrong length index getting snomed name');
  Move(FMaster[iIndex], i, 2);
  SetLength(Result, i);
  if (Byte(FMaster[iIndex]) + iIndex > FLength) then
    Raise ESnomedServices.Create('Wrong length index getting snomed name (2)');
  Move(FMaster[iIndex+2], result[1], Length(Result)*2);
end;

function TSnomedStrings.AddString(const s: String): Cardinal;
var
  i : word;
begin
  if Length(s) > 65535 Then
    raise ESnomedServices.Create('Snomed Description too long: '+String(s));
  result := FBuilder.Length;
  i := length(s);
  FBuilder.AddWord(i);
  FBuilder.AddString(s);
end;

procedure TSnomedStrings.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TSnomedStrings.StartBuild;
begin
  FBuilder := TAdvBytesBuilder.Create;
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
    if (FBuilder <> nil) and (iIndex >= FLength) then
      Post;
    if (iIndex >= FLength) then
      Raise ESnomedServices.Create('Wrong length index getting Snomed list. asked for '+inttostr(iIndex)+', limit is '+inttostr(FLength));
    move(FMaster[iIndex], c, 4);
    SetLength(Result, c);
    if (iIndex + 4 + length(result) * 4 > FLength) then
      Raise ESnomedServices.Create('Wrong length index ('+inttostr(iIndex)+', '+inttostr(length(result))+') getting Snomed list (length = '+inttostr(FLength)+')');
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
  result := FBuilder.Length;
  FBuilder.AddCardinal(length(a));
  for iLoop := Low(a) to High(a) Do
    FBuilder.AddCardinal(a[iLoop]);
End;

procedure TSnomedReferences.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TSnomedReferences.StartBuild;
begin
  FBuilder := TAdvBytesBuilder.Create;
end;

function TSnomedReferences.Getlength(iIndex: Cardinal): Cardinal;
begin
  if (iIndex > FLength) then
    Raise ESnomedServices.Create('Wrong length index getting Snomed list');
  move(FMaster[iIndex], result, 4);
end;

procedure TSnomedReferences.Post;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
end;

{ TSnomedDescriptions }

procedure TSnomedDescriptions.SetRefsets(iIndex, refsets, valueses: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index getting snomed Desc Details');
  assert(iIndex mod DESC_SIZE = 0);
  Move(refsets, FMaster[iIndex+27], 4);
  Move(valueses, FMaster[iIndex+31], 4);
end;

procedure TSnomedDescriptions.StartBuild;
begin
  FBuilder := TAdvBytesBuilder.Create;
end;

Function TSnomedDescriptions.AddDescription(iDesc : Cardinal; id : UInt64; date : TSnomedDate; concept, module, kind : Cardinal; iflags : Byte) : Cardinal;
begin
  result := FBuilder.Length;
  FBuilder.AddCardinal(iDesc);  // 4
  FBuilder.Append(iFlags);      // 5
  FBuilder.AddUInt64(id);       // 13
  FBuilder.AddCardinal(concept); // 17
  FBuilder.AddCardinal(module);  // 21
  FBuilder.AddCardinal(kind);    // 25
  FBuilder.AddWord(date);        // 27
  FBuilder.AddCardinal(0); // refsets
  FBuilder.AddCardinal(0); // refsets values
end;

function TSnomedDescriptions.ConceptByIndex(iIndex: Cardinal): cardinal;
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index getting snomed Desc Details');
  Move(FMaster[iIndex+13], result, 4);
end;

function TSnomedDescriptions.Count: Cardinal;
begin
  result := FLength div DESC_SIZE;
end;

procedure TSnomedDescriptions.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TSnomedDescriptions.GetDescription(iIndex : Cardinal; var iDesc : Cardinal; var id : UInt64; var date : TSnomedDate; var concept, module, kind, refsets, valueses : Cardinal; var iflags : Byte);
Begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index getting snomed Desc Details');
  Move(FMaster[iIndex+0], iDesc, 4);
  Move(FMaster[iIndex+4], iFlags, 1);
  Move(FMaster[iIndex+5], ID, 8);
  Move(FMaster[iIndex+13], concept, 4);
  Move(FMaster[iIndex+17], module, 4);
  Move(FMaster[iIndex+21], kind, 4);
  Move(FMaster[iIndex+25], date, 2);
  Move(FMaster[iIndex+27], refsets, 4);
  Move(FMaster[iIndex+31], valueses, 4);
End;


procedure TSnomedDescriptions.UpdateDetails(iIndex: Cardinal; id: UInt64; concept: Cardinal);
var
  t1 : UInt64;
  t2 : Cardinal;
begin
  FBuilder.Read(iIndex+13, t2, 4);
  if (t2 <> concept) Then
  Begin
    assert(t2 = 0);

    FBuilder.Read(iIndex+5, t1, 8);
    assert(t1 = 0);
    FBuilder.WriteUInt64(iIndex+5, id);
    FBuilder.Read(iIndex+5, t1, 8);
    assert(t1 = id);

    FBuilder.WriteCardinal(iIndex+13, concept);
    FBuilder.Read(iIndex+13, t2, 4);
    assert(t2 = concept);
  End;
end;


{ TSnomedConceptList }

procedure TSnomedConceptList.StartBuild;
begin
  FBuilder := TAdvBytesBuilder.Create;
end;

Function TSnomedConceptList.AddConcept(iIdentity : UInt64; effectiveTime : TSnomedDate; iFlags : Byte) : Cardinal;
begin
  result := FBuilder.Length;
  FBuilder.AddUInt64(iIdentity);
  FBuilder.Append(iFlags);
  FBuilder.AddCardinal(0); // parents
  FBuilder.AddCardinal(0); // descriptions
  FBuilder.AddCardinal(0); // inbounds
  FBuilder.AddCardinal(0); // outbounds
  FBuilder.AddCardinal(0); // closures
  FBuilder.Append(0); // depth
  FBuilder.AddCardinal(0); // stems
  FBuilder.AddWord(effectiveTime); // date
  FBuilder.AddCardinal(0); // moduleId (rf2)
  FBuilder.AddCardinal(0); // status (rf2)
  FBuilder.AddCardinal(0); // refsets (rf2)
end;

procedure TSnomedConceptList.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

{$R-}

function TSnomedConceptList.FindConcept(iIdentity: UInt64; var IIndex: Cardinal): boolean;
var
  aConcept : UInt64;
  L, H, I: Integer;
begin
  Result := False;
  L := 0;
  H := FLength;
  H := (H div CONCEPT_SIZE) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    Move(FMaster[(i*CONCEPT_SIZE)], aConcept, 8);
    if aConcept < iIdentity then L := I + 1 else
    begin
      H := I - 1;
      if aConcept = iIdentity then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  iIndex := (L*CONCEPT_SIZE);
end;

Function TSnomedConceptList.getParent(iIndex : Cardinal): Cardinal;
Begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(FMaster[iIndex+9], result, 4);
End;

function TSnomedConceptList.GetRefsets(iIndex: Cardinal): Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(FMaster[iIndex+44], result, 4);
end;

Function TSnomedConceptList.getIdentity(iIndex : Cardinal): UInt64;
Begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(FMaster[iIndex+0], result, 8);
End;

procedure TSnomedConceptList.GetConcept(iIndex : Cardinal; var Identity : UInt64; var Flags : Byte; var effectiveTime : TSnomedDate; var Parents : Cardinal; var Descriptions : Cardinal; var Inbounds : Cardinal; var outbounds : Cardinal; var refsets : Cardinal);
Begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details (mod = '+inttostr(iIndex mod CONCEPT_SIZE)+')');

  Move(FMaster[iIndex+0], Identity, 8);
  Move(FMaster[iIndex+8], Flags, 1);
  Move(FMaster[iIndex+9], Parents, 4);
  Move(FMaster[iIndex+13], Descriptions, 4);
  Move(FMaster[iIndex+17], Inbounds, 4);
  Move(FMaster[iIndex+21], Outbounds, 4);
  Move(FMaster[iIndex+34], effectiveTime, 2);
  Move(FMaster[iIndex+44], refsets, 2);
End;

function TSnomedConceptList.getConceptId(iIndex : Cardinal): UInt64;
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index getting snomed Concept Details');
  Move(FMaster[iIndex+0], result, 8);
end;

procedure TSnomedConceptList.SetParents(iIndex: Cardinal; const Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(Value, FMaster[iIndex+9], 4);
end;

procedure TSnomedConceptList.SetRefsets(iIndex, Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(Value, FMaster[iIndex+44], 4);
end;

procedure TSnomedConceptList.SetDescriptions(iIndex: Cardinal; const Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(Value, FMaster[iIndex+13], 4);
end;

procedure TSnomedConceptList.SetFlag(iIndex: Cardinal; iFlags: Byte);
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(iFlags, FMaster[iIndex+8], 1);
end;

procedure TSnomedConceptList.SetInbounds(iIndex: Cardinal; const Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(Value, FMaster[iIndex+17], 4);
end;

procedure TSnomedConceptList.SetModuleId(iIndex, Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(Value, FMaster[iIndex+36], 4);
end;

procedure TSnomedConceptList.SetOutbounds(iIndex: Cardinal; const Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(Value, FMaster[iIndex+21], 4);
end;


function TSnomedConceptList.Count: Integer;
begin
  result := FLength div CONCEPT_SIZE;
end;

Function TSnomedConceptList.GetAllDesc(iIndex: Cardinal) : Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(FMaster[iIndex+25], result, 4);
end;

procedure TSnomedConceptList.SetAllDesc(iIndex, Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(Value, FMaster[iIndex+25], 4);
end;

function TSnomedConceptList.GetOutbounds(iIndex: Cardinal): Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(FMaster[iIndex+21], result, 4);
end;

function TSnomedConceptList.GetInbounds(iIndex: Cardinal): Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(FMaster[iIndex+17], result, 4);
end;

function TSnomedConceptList.GetModuleId(iIndex: Cardinal): Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(FMaster[iIndex+36], result, 4);
end;

function TSnomedConceptList.GetDepth(iIndex: Cardinal): Byte;
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(FMaster[iIndex+29], result, 1);
end;

procedure TSnomedConceptList.SetDate(iIndex: Cardinal; effectiveTime: TSnomedDate);
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(effectiveTime, FMaster[iIndex+34], 2);
end;

procedure TSnomedConceptList.SetDepth(iIndex: Cardinal; Value: Byte);
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(Value, FMaster[iIndex+29], 1);
end;

function TSnomedConceptList.GetStatus(iIndex: Cardinal): Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(FMaster[iIndex+40], result, 4);
end;

function TSnomedConceptList.GetStems(iIndex: Cardinal): Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(FMaster[iIndex+30], result, 4);
end;

procedure TSnomedConceptList.SetStatus(iIndex, Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(Value, FMaster[iIndex+40], 4);
end;

procedure TSnomedConceptList.SetStems(iIndex, Value: Cardinal);
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
  Move(Value, FMaster[iIndex+30], 4);
end;

function TSnomedConceptList.GetDescriptions(iIndex: Cardinal): Cardinal;
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details. Max = '+inttostr(FLength));
  if (iIndex mod CONCEPT_SIZE <> 0) then
    Raise ESnomedServices.Create('Wrong length index '+inttostr(iIndex)+' getting snomed Concept Details');
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
  function readBytes : TBytes;
  begin
    SetLength(result, oRead.ReadInteger);
    oread.Read(result[0], length(result));
  end;
  function ReadUInt64: UInt64;
  begin
    oread.Read(result, 8);
  end;
begin
  oFile := TFileStream.Create(sFilename, fmOpenread+fmShareDenyWrite);
  try
    oread := TReader.Create(oFile, 8192);
    try
      if oRead.ReadString <> SNOMED_CACHE_VERSION Then
        raise ESnomedServices.create('The Snomed cache "'+sFilename+'" must be rebuilt using -snomed-rf1 or -snomed-rf2');
      VersionUri := oread.ReadString;
      VersionDate := oread.ReadString;
      FStrings.FMaster := ReadBytes;
      FStrings.FLength := Length(FStrings.FMaster);
      FRefs.FMaster := ReadBytes;
      FRefs.FLength := Length(FRefs.FMaster);
      FDesc.FMaster := ReadBytes;
      FDesc.FLength := Length(FDesc.FMaster);
      FWords.FMaster := ReadBytes;
      FWords.FLength := Length(FWords.FMaster);
      FStems.FMaster := ReadBytes;
      FStems.FLength := Length(FStems.FMaster);
      FConcept.FMaster := ReadBytes;
      FConcept.FLength := Length(FConcept.FMaster);
      FRel.FMaster := ReadBytes;
      FRel.FLength := Length(FRel.FMaster);
      FRefSetIndex.FMaster := ReadBytes;
      FRefSetIndex.FLength := Length(FRefSetIndex.FMaster);
      FRefSetMembers.FMaster := ReadBytes;
      FRefSetMembers.FLength := Length(FRefSetMembers.FMaster);
      FDescRef.FMaster := ReadBytes;
      FDescRef.FLength := Length(FDescRef.FMaster);
      FIs_a_Index := oread.ReadInteger;
      SetLength(FInactiveRoots, oRead.ReadInteger);
      for i := 0 to Length(FInactiveRoots) - 1 Do
        FInactiveRoots[i] := ReadUInt64;
      SetLength(FActiveRoots, oRead.ReadInteger);
      for i := 0 to Length(FActiveRoots) - 1 Do
        FActiveRoots[i] := ReadUInt64;
    Finally
      oread.Free;
    End;
  Finally
    oFile.Free;
  End;
  if not Concept.FindConcept(PREFERRED_MAGIC, FPreferredTerm) then
    FPreferredTerm := 0;
  Loaded := true;
end;

function TSnomedServices.ReferenceSetExists(sid: String): Boolean;
var
  index, members, types : Cardinal;
begin
  result := FConcept.FindConcept(StringToId(sid), index);
  if result then
    result := GetConceptRefSet(index, true, members, types) > 0;
end;

procedure TSnomedServices.Save(const sFilename: String);
var
  oFile : Tfilestream;
  oWrite : TWriter;
  i : integer;
  procedure WriteBytes(b : TBytes);
  begin
   oWrite.WriteInteger(length(b));
   oWrite.Write(b[0], length(b));
  end;
  procedure WriteUInt64(v : UInt64);
  begin
    oWrite.Write(v, 8);
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
      oWrite.WriteString(SNOMED_CACHE_VERSION);
      oWrite.WriteString(VersionUri);
      oWrite.WriteString(VersionDate);
      WriteBytes(FStrings.FMaster);
      WriteBytes(FRefs.FMaster);
      WriteBytes(FDesc.FMaster);
      WriteBytes(FWords.FMaster);
      WriteBytes(FStems.FMaster);
      WriteBytes(FConcept.FMaster);
      WriteBytes(FRel.FMaster);
      WriteBytes(FRefSetIndex.FMaster);
      WriteBytes(FRefSetMembers.FMaster);
      WriteBytes(FDescRef.FMaster);
      oWrite.writeInteger(FIs_a_Index);
      oWrite.writeInteger(length(FInactiveRoots));
      for i := 0 to Length(FInactiveRoots) - 1 Do
        writeUInt64(FInactiveRoots[i]);
      oWrite.writeInteger(length(FActiveRoots));
      for i := 0 to Length(FActiveRoots) - 1 Do
        writeUInt64(FActiveRoots[i]);
    Finally
      oWrite.Free;
    End;
  Finally
    oFile.Free;
  End;
end;

function TSnomedServices.SearchFilter(filter : TSearchFilterText; prep : TCodeSystemProviderFilterPreparationContext; sort : boolean): TCodeSystemProviderFilterContext;
var
  res : TSnomedFilterContext;
begin
  res := TSnomedFilterContext.Create;
  try
    res.matches := Search(0, filter.filter, 0, false, true);
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



function TSnomedServices.Search(iRoot : UInt64; sText: String; iLang : Cardinal; bInactive : Boolean; bAll : boolean): TMatchArray;
var
  aLangMembers : TSnomedReferenceSetMemberArray;

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
        Else If StringEndsWith(s, words[i].original) Then
          result := result + 1.5
        Else If (pos(' '+words[i].original+' ', s) > 0) Then
          result := result + 0.6;
        result := result + result / length(s);
        if iDepth > 0 Then
          result := result + result / iDepth;
      End;
  End;

  Procedure AddResult(var iCount : Integer; iindex : cardinal; id : UInt64; priority : Double);
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
    Identity, iID2 : UInt64;
    Flags : Byte;
    Parents : Cardinal;
    Descriptions : Cardinal;
    Inbounds : Cardinal;
    outbounds : Cardinal;
    refsets, valueses : Cardinal;
    Desc : TCardinalArray;
    iWork, iDummy, module, kind : Cardinal;
    date : TSnomedDate;
    ok : boolean;
    s : String;
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
      begin
        if words[i].stem <> 0 Then
          For j := 0 to length(Desc) - 1 do
            if (words[i].stem = desc[j]) Then
              r1 := r1 + 20 + (20 / length(desc))
            else
              assert(FStrings.GetEntry(words[i].stem) <> FStrings.GetEntry(desc[j]));
      end;

      Desc := Refs.GetReferences(Descriptions);
      for j := Low(Desc) to High(Desc) Do
      Begin
        FDesc.GetDescription(Desc[j], iWork, iID2, date, iDummy, module, kind, refsets, valueses, flags);
        t := t + FlagFactor(flags);
        r2 := r2 + Match(words, Strings.GetEntry(iWork), iDepth) * FlagFactor(flags);
      End;
      if (r1 + r2 > 0) Then
      begin
        if not bAll then
          AddResult(iCount, iConceptIndex, Identity, r1 + r2 / t)
        else
        begin
          ok := false;
          for j := Low(Desc) to High(Desc) Do
          Begin
            FDesc.GetDescription(Desc[j], iWork, iID2, date, iDummy, module, kind, refsets, valueses, flags);
            s := lowercase(Strings.GetEntry(iWork));
            ok := true;
            For i := 0 to length(words) - 1 do
              if not ((copy(s, 1, length(words[i].original)) = words[i].original) or (ansipos(' '+words[i].original, s) > 0)) then
                ok := false;
            if ok then
              break;
          End;
          if ok then
            AddResult(iCount, iConceptIndex, Identity, r1 + r2 / t)
        end;
      end;
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
  s : String;
  s1 : String;
begin
  SetLength(words, 0);
  SetLength(aMembers, 0);
  SetLength(aLangMembers, 0);
  sText := LowerCase(sText);
  oStemmer := GetStemmer_8('english');
  Try
    while (sText <> '') Do
    Begin
      StringSplit(sText, [',', ' ', ':', '.', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '{', '}', '[', ']', '|', '\', ';', '"', '<', '>', '?', '/', '~', '`', '-', '_', '+', '='], s, sText);
      if (s <> '') Then
      Begin
        SetLength(words, length(words)+1);
        words[length(words)-1].original := lowercase(s);
        s1 := oStemmer.calc(s);
        if FindStem(s1, index) Then
          words[length(words)-1].stem := FStems.GetString(index);
      End;
    End;
  Finally
    oStemmer.free;
  End;

  if Length(words) = 0 then
    Raise ESnomedServices.Create('no usable search text found');

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
        CheckConcept(words, iCount, i * CONCEPT_SIZE)
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

function TSnomedServices.Subsumes(const sParent, sChild: String): Boolean;
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
  c : UInt64;
begin
  Result := False;
  L := 0;
  H := length(aMembers) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := aMembers[i].Ref;
    if C < iRef then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = iRef then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  iIndex := L;
end;


function TSnomedServices.GetDisplayName(const iConcept, iLang: Cardinal): String;
var
  iLoop : integer;
  Identity, iId2 : UInt64;
  Flags : Byte;
  Parents : Cardinal;
  Descriptions : Cardinal;
  Descs : TCardinalArray;
  Inbounds : Cardinal;
  outbounds : Cardinal;
  iDesc, iDummy, module, kind, refsets, valueses : Cardinal;
  iInt : integer;
  date : TSnomedDate;
  aMembers : TSnomedReferenceSetMemberArray;
  iList : TRefSetMemberEntryArray;
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
    Desc.GetDescription(descs[iLoop], iDesc, iId2, date, iDummy, module, kind, refsets, valueses, Flags);
    if (Flags and MASK_DESC_STATUS = FLAG_Active) And (Flags and MASK_DESC_STYLE shr 4 in [VAL_DESC_Preferred]) And ((iLang = 0) or (FindMember(aMembers, descs[iLoop], iint))) Then
      result := Strings.GetEntry(iDesc);
  End;
  // ok, didn't find an active preferred term in the language of preference. Let's try for any term in the language
  if (result = '') and (iLang <> 0) then
    For iLoop := 0 to High(descs) Do
    Begin
      Desc.GetDescription(descs[iLoop], iDesc, iId2, date, iDummy, module, kind, refsets, valueses, Flags);
      if (Flags and MASK_DESC_STATUS = FLAG_Active) And (FindMember(aMembers, descs[iLoop], iInt)) Then
        result := Strings.GetEntry(iDesc);
    End;
  // if we still haven't found, then any preferred term
  result := GetPN(Descs);
end;

function TSnomedServices.GetDisplayName(const sTerm, sLangSet: String): String;
var
  iTerm, iLang : Cardinal;
begin
  iLang := CheckLangSet(sLangSet);
  if not Concept.FindConcept(StringToId(sTerm), iTerm) Then
    raise ESnomedServices.Create('Concept '+sTerm+' not found');
  result := GetDisplayName(iTerm, iLang);
end;

function TSnomedServices.GetFSN(iDescriptions: TCardinalArray): String;
var
  iLoop : Integer;
  iid : UInt64;
  iString, iDummy, module, valueses, refsets, kind : Cardinal;
  iFlag : Byte;
  date : TSnomedDate;
begin
  result := '';
  For iLoop := Low(iDescriptions) To High(iDescriptions) Do
  Begin
    Desc.GetDescription(iDescriptions[iLoop], iString, iId, date, iDummy,  module, kind, refsets, valueses, iFlag);
    if (iFlag and MASK_DESC_STATUS = FLAG_Active) And (iFlag and MASK_DESC_STYLE = VAL_DESC_FullySpecifiedName shl 4) Then
      result := Strings.GetEntry(iString);
  End;
End;


Procedure TSnomedServices.ListDisplayNames(list : TStringList; Const iConcept, iLang : Cardinal; FlagMask : Byte);
var
  aMembers : TSnomedReferenceSetMemberArray;
  iLoop : integer;
  Identity, iID2 : UInt64;
  Flags : Byte;
  Parents, Descriptions, Inbounds, outbounds, valueses, refsets : Cardinal;
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
    Desc.GetDescription(descs[iLoop], iDesc, iId2, date, iDummy, module, kind, refsets, valueses, Flags);
    if ((Flags and flagMask > 0) or (flagMask = $FF))  And ((iLang = 0) or (FindMember(aMembers, descs[iLoop], iInt))) Then
      list.add(Strings.GetEntry(iDesc));
  End;
end;

Procedure TSnomedServices.ListDisplayNames(list : TStringList; Const sTerm, sLangSet : String; FlagMask : Byte);
var
  iTerm, iLang : Cardinal;
begin
  iLang := CheckLangSet(sLangSet);
  if not Concept.FindConcept(StringToId(sTerm), iTerm) Then
    raise ESnomedServices.Create('Concept '+sTerm+' not found');
  ListDisplayNames(list, iTerm, iLang, flagmask);
end;

procedure TSnomedServices.GetMatchInfo(iConcept: Cardinal; var sTerm, sFSN, sPreferred: String);
var
  iLoop : integer;
  Identity, iId2 : UInt64;
  Flags : Byte;
  Parents : Cardinal;
  Descriptions : Cardinal;
  Descs : TCardinalArray;
  Inbounds : Cardinal;
  outbounds : Cardinal;
  iDesc, iDummy, module, kind, valueses, refsets : Cardinal;
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
    Desc.GetDescription(descs[iLoop], iDesc, iId2, date, iDummy, module, kind, refsets, valueses, Flags);
      if (Flags and MASK_DESC_STATUS = FLAG_Active) And (Flags and MASK_DESC_STYLE = VAL_DESC_FullySpecifiedName shl 4) Then
       sFSN := Strings.GetEntry(iDesc)
      else if (Flags and MASK_DESC_STATUS = FLAG_Active) And (Flags and MASK_DESC_STYLE shr 4 in [VAL_DESC_Preferred]) Then
        sPreferred := Strings.GetEntry(iDesc);
      if (sPreferred <> '') and (sFSN <> '') then
        break;
  End;
end;
function TSnomedServices.GetPN(iDescriptions: TCardinalArray): String;
var
  iLoop : Integer;
  iid : UInt64;
  iString, iDummy, module, valueses, refsets, kind : Cardinal;
  iFlag : Byte;
  date : TSnomedDate;
  iList, values : TCardinalArray;
  v : String;
begin
  result := '';
  For iLoop := Low(iDescriptions) To High(iDescriptions) Do
  Begin
    Desc.GetDescription(iDescriptions[iLoop], iString, iId, date, iDummy,  module, kind, refsets, valueses, iFlag);
    if (iFlag and MASK_DESC_STATUS = FLAG_Active) And (iFlag and MASK_DESC_STYLE shr 4 in [VAL_DESC_Preferred]) Then
      result := Strings.GetEntry(iString);
  End;
  if result = '' then // ok, well, we'll pick the first description that's in a value set
  begin
    For iLoop := Low(iDescriptions) To High(iDescriptions) Do
    Begin
      Desc.GetDescription(iDescriptions[iLoop], iString, iId, date, iDummy,  module, kind, refsets, valueses, iFlag);
      // check the language reference set
      iList := Refs.GetReferences(refsets);
      v := Strings.GetEntry(iString);
      if valueses <> 0 then
      begin
        values := Refs.GetReferences(valueses); // get the list of all value lists
        values := Refs.GetReferences(values[0]); // get the first value set
        if (iFlag and MASK_DESC_STATUS = FLAG_Active) and (values[0] = FPreferredTerm) then
          exit(v);
      end
      else if ((result = '') or (length(result) > length(v))) and (iFlag and MASK_DESC_STATUS = FLAG_Active) And (Length(iList) > 0) Then
        result := v;
    End;
  end;
  if result = '' Then // ok, give up. and use the FSN
    result := GetFSN(iDescriptions);
end;

{
function TSnomedServices.FindWord(s: String; var index : integer): Boolean;
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

function TSnomedServices.FindStem(s: String; var index: Integer): Boolean;
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

procedure TSnomedServices.getCDSInfo(card: TCDSHookCard; baseURL, code, display: String);
var
  b : TStringBuilder;
  Identity : UInt64;
  Flags : Byte;
  ParentIndex, iWork, iWork2, iWork3, module, modifier, kind, iDummy : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex, InboundIndex2 : Cardinal;
  outboundIndex, valueses, refsets : Cardinal;
  Inbounds : TCardinalArray;
  date : TSnomedDate;
  Descriptions : TCardinalArray;
  Parents : TCardinalArray;
  i, group : integer;
  iId : UInt64;
  iIndex : cardinal;
  first : boolean;
  did : UInt64;
begin
  b := TStringBuilder.Create;
  try
    SetLength(inbounds, 0);
    iId := StrToUInt64Def(code, 0);
    if not Concept.FindConcept(iId, iIndex) then
    begin
      b.Append('Snomed Code '+code+#13#10#13#10);
      b.Append('* Error: Code not known')
    end
    else
    begin
      card.addLink('Further Detail', baseURL+'/snomed/doco/?srch='+code);

      Concept.GetConcept(IIndex, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
      Inbounds := Refs.GetReferences(InboundIndex);

      b.Append('Snomed Code '+code+' : '+GetPNForConcept(iIndex)+#13#10#13#10);

      Descriptions := Refs.GetReferences(DescriptionIndex);
      b.Append('Descriptions: '+#13#10#13#10);
      for i := Low(Descriptions) To High(Descriptions) Do
      Begin
        Desc.GetDescription(Descriptions[i], iWork, Identity, date, iDummy, module, kind, refsets, valueses, Flags);
        if flags and MASK_DESC_STATUS = Flag_Active Then
          if ((flags and MASK_DESC_STYLE) shr 4 = VAL_DESC_Unspecified) and (kind <> 0) then
            b.Append('* '+Strings.GetEntry(iWork)+' ('+GetPNForConcept(kind)+')'+#13#10)
          else
            b.Append('* '+Strings.GetEntry(iWork)+' ('+GetDescType(Flags)+')'+#13#10);
      End;
      b.Append(#13#10);

      // parents:
      if ParentIndex <> 0 Then
      begin
        Parents := Refs.GetReferences(ParentIndex);
        b.Append('Parents: '+#13#10#13#10);
        for i := 0 to Length(Parents)-1 do
        begin
          Concept.GetConcept(Parents[i], Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex2, outboundIndex, refsets);
          Descriptions := Refs.GetReferences(DescriptionIndex);
          b.Append('* '+GetPN(Descriptions)+' ('+IntToStr(Identity)+')'+#13#10);
        end;
        b.Append(#13#10);
      end;

      // children: (inbound relationships with type is-a)
      first := true;
      For i := 0 to High(Inbounds) Do
      Begin
        Rel.GetRelationship(Inbounds[i], did, iWork, iWork2, iWork3, module, kind, modifier, date, Flags, Group);
        if iWork3 = FIs_a_Index then
        begin
          if first then
          begin
            b.Append('Children: '+#13#10#13#10);
            first := false;
          end;
          Concept.GetConcept(iWork, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
          Descriptions := Refs.GetReferences(DescriptionIndex);
          b.Append('* '+GetPN(Descriptions)+' ('+IntToStr(Identity)+')'+#13#10);
        end;
      End;
      b.Append(#13#10);
    End;
    b.Append(#13#10+'This term definition is derived from SNOMED CT, which is copyright &copy; 2002+ International Health Terminology Standards Development Organisation (IHTSDO)'#13#10);
    card.detail := b.ToString;
  finally
    b.free;
  end;
end;

function TSnomedServices.Link: TSnomedServices;
begin
  result := TSnomedServices(inherited link);
end;

function TSnomedServices.GetConceptRefSet(iConcept: Cardinal; bByName : Boolean; var iMembers, iTypes : cardinal): Cardinal;
var
  i : integer;
  c : Cardinal;
  iDummy : cardinal;
begin
  result := 0;
  For i := 0 to FRefSetIndex.Count - 1 do
  Begin
    if bByName Then
      FRefSetIndex.GetReferenceSet(i, c, iDummy, iMembers, iTypes)
    else
      FRefSetIndex.GetReferenceSet(i, c, iMembers, iDummy, iTypes);
    if c = iConcept Then
    Begin
      result := c;
      exit;
    End;
  End;
end;

function TSnomedServices.GetDescRefsets(iDesc: Cardinal): TRefSetMemberEntryArray;
var
  i : integer;
  iDefinition, iMembersByRef, iMembersByName: Cardinal;
  aMembers : TSnomedReferenceSetMemberArray;
  iIndex : Integer;
  iTypes : Cardinal;
begin
  SetLength(result, 0);
  SetLength(aMembers, 0);
  for i := 0 to FRefSetIndex.Count - 1 Do
  Begin
    FRefSetIndex.GetReferenceSet(i, iDefinition, iMembersByRef, iMembersByName, iTypes);
    aMembers := FRefSetMembers.GetMembers(iMembersByRef);
    if FindMember(aMembers, iDesc, iIndex) Then
    begin
      SetLength(result, length(result)+1);
      result[length(result)-1].refset := iDefinition;
      result[length(result)-1].types := iTypes;
      result[length(result)-1].values := aMembers[iIndex].values;
    End;
  End;
end;

function TSnomedServices.GetConceptRefsets(iDesc: Cardinal): TRefSetMemberEntryArray;
var
  i : integer;
  iDefinition, iMembersByRef, iMembersByName: Cardinal;
  aMembers : TSnomedReferenceSetMemberArray;
  iIndex : Integer;
  iTypes : Cardinal;
begin
  SetLength(result, 0);
  SetLength(aMembers, 0);
  for i := 0 to FRefSetIndex.Count - 1 Do
  Begin
    FRefSetIndex.GetReferenceSet(i, iDefinition, iMembersByRef, iMembersByName, iTypes);
    aMembers := FRefSetMembers.GetMembers(iMembersByRef);
    if FindMember(aMembers, iDesc, iIndex) Then
    begin
      SetLength(result, length(result)+1);
      result[length(result)-1].refset := iDefinition;
      result[length(result)-1].types := iTypes;
      result[length(result)-1].values := aMembers[iIndex].values;
    End;
  End;
end;

function TSnomedServices.StringIsId(const s: String; var iId: UInt64): Boolean;
begin
  result := TryStrToUINT64(s, iId);
end;

function TSnomedServices.StringToId(const s: String): UInt64;
begin
  result := StrToUInt64(s);
end;

function TSnomedServices.CheckLangSet(sTerm: String): Cardinal;
var
  iId  : UInt64;
begin
  result := 0;
  if sTerm <> '' Then
  Begin
    if StringIsId(sterm, iId) And Concept.FindConcept(iId, result) Then
      result := FRefSetIndex.GetMembersByConcept(result, false);
    if result = 0 Then
      Raise ESnomedServices.Create('Unable to resolve the language reference set '+sTerm);
  End
end;

function TSnomedServices.GetConceptDescendents(index: Cardinal): TCardinalArray;
begin
  result := FRefs.GetReferences(FConcept.GetAllDesc(index));
end;

function TSnomedServices.GetConceptId(const iConcept : Cardinal): String;
var
  Identity : UInt64;
  Flags : Byte;
  Parents, Inbounds, outbounds, descriptions, refsets : Cardinal;
  date : TSnomedDate;
begin
  Concept.GetConcept(iConcept, Identity, Flags, date, Parents, Descriptions, Inbounds, outbounds, refsets);
  result := inttostr(Identity);
end;

function TSnomedServices.StringToIdOrZero(const s: String): UInt64;
begin
  if StringIsInteger64(s) then
    result := StrToUInt64(s)
  Else
    result := 0;
end;

function TSnomedServices.IsValidConcept(const sTerm: String): Boolean;
var
  iTerm : Cardinal;
begin
  if not StringIsInteger64(sTerm) then
    result := false
  else
    result := Concept.FindConcept(StringToId(sTerm), iTerm);
end;

function TSnomedServices.IsValidDescription(const sTerm: String; var concept: UInt64; var description: String): Boolean;
var
  iTerm, desc, cId, module, kind, valueses, refsets : Cardinal;
  flags : Byte;
  id : UInt64;
  date : TSnomedDate;
begin
  result := DescRef.FindDescription(StringToId(sTerm), iTerm);
  if result then
  begin
    FDesc.GetDescription(iTerm, desc, id, date, cId, module, kind, refsets, valueses, flags);
    concept := FConcept.getConceptId(cId);
    description := FStrings.GetEntry(desc);
  end;
end;

{ TSnomedRelationshipList }

procedure TSnomedRelationshipList.StartBuild;
begin
  FBuilder := TAdvBytesBuilder.Create;
end;

Function TSnomedRelationshipList.AddRelationship(identity : UInt64; Source, Target, RelType, module, kind, modifier : Cardinal; date : TSnomedDate; Flags : Byte; Group : integer) : Cardinal;
begin
  Result := FBuilder.Length;
  FBuilder.AddCardinal(Source);
  FBuilder.AddCardinal(Target);
  FBuilder.AddCardinal(RelType);
  FBuilder.AddCardinal(module);
  FBuilder.AddCardinal(kind);
  FBuilder.AddCardinal(modifier);
  FBuilder.AddWord(date);
  FBuilder.Append(Flags);
  FBuilder.AddInteger(Group);
  FBuilder.AddUInt64(identity);
End;

procedure TSnomedRelationshipList.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TSnomedRelationshipList.GetRelationship(iIndex: Cardinal; var identity : UInt64; var Source, Target, RelType, module, kind, modifier : Cardinal; var date : TSnomedDate; var Flags : Byte; var Group : integer);
// (iIndex: Cardinal; var Source, Target, RelType: Cardinal; var Flags, Group : Byte);
begin
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index getting snomed relationship Details');
  Move(FMaster[iIndex+0], Source, 4);
  Move(FMaster[iIndex+4], Target, 4);
  Move(FMaster[iIndex+8], RelType, 4);
  Move(FMaster[iIndex+12], module, 4);
  Move(FMaster[iIndex+16], kind, 4);
  Move(FMaster[iIndex+20], modifier, 4);
  Move(FMaster[iIndex+24], date, 2);
  Move(FMaster[iIndex+26], Flags, 1);
  Move(FMaster[iIndex+27], Group, 4);
  Move(FMaster[iIndex+31], identity, 8);
end;

{ TSnomedWords }

procedure TSnomedWords.AddWord(index: Cardinal; Flags: Byte);
begin
  FBuilder.AddCardinal(index);
  FBuilder.Append(flags);
end;

function TSnomedWords.Count: Integer;
begin
  result := FLength div 5;
end;

procedure TSnomedWords.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TSnomedWords.GetEntry(iIndex: Cardinal; var index: Cardinal; var flags: Byte);
var
  l : Cardinal;
begin
  l := (iIndex * 5) + 1;
  if l > FLength - 4 Then
    raise ESnomedServices.create('invalid index');
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
  FBuilder := TAdvBytesBuilder.Create;
end;

{ TSnomedStems }

procedure TSnomedStems.AddStem(index: Cardinal; reference : Cardinal);
begin
  FBuilder.AddCardinal(index);
  FBuilder.AddCardinal(reference);
end;

function TSnomedStems.Count: Integer;
begin
  result := FLength div 8;
end;

procedure TSnomedStems.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

procedure TSnomedStems.GetEntry(iIndex: Cardinal; var index: Cardinal; var reference: Cardinal);
var
  l : Cardinal;
begin
  l := (iIndex * 8);
  if l > FLength - 7 Then
    raise ESnomedServices.create('invalid index');
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
  FBuilder := TAdvBytesBuilder.Create;
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

function TSnomedServiceList.GetDefinitionByName(sName: String): TSnomedServices;
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
      if SameText(Definition[i].VersionUri, sName) then
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

Procedure TSnomedReferenceSetIndex.AddReferenceSet(iDefinition, iMembersByRef, iMembersByName, iFieldTypes: Cardinal);
begin
  FBuilder.AddCardinal(iDefinition);
  FBuilder.AddCardinal(iMembersByRef);
  FBuilder.AddCardinal(iMembersByName);
  FBuilder.AddCardinal(iFieldTypes);
end;

function TSnomedReferenceSetIndex.Count: Integer;
begin
  result := FLength div 16;
end;

procedure TSnomedReferenceSetIndex.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
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
    Move(FMaster[i * 16], v, 4);
    if v = iIndex Then
    Begin
      if bByName Then
        Move(FMaster[i * 16 + 8], result, 4)
      Else
        Move(FMaster[i * 16 + 4], result, 4);
      exit;
    End;
  End;
end;

procedure TSnomedReferenceSetIndex.GetReferenceSet(iIndex: Cardinal; var iDefinition, iMembersByRef, iMembersByName, iFieldTypes: Cardinal);
begin
  iIndex := iIndex * 16;
  if (iIndex >= FLength) then
    Raise ESnomedServices.Create('Wrong length index getting snomed relationship Details');
  Move(FMaster[iIndex+0], iDefinition, 4);
  Move(FMaster[iIndex+4], iMembersByRef, 4);
  Move(FMaster[iIndex+8], iMembersByName, 4);
  Move(FMaster[iIndex+12], iFieldTypes, 4);
end;

procedure TSnomedReferenceSetIndex.StartBuild;
begin
  FBuilder := TAdvBytesBuilder.Create;
end;

{ TSnomedDescriptionIndex }

procedure TSnomedDescriptionIndex.AddDescription(id: UInt64; reference: Cardinal);
begin
  FBuilder.AddUInt64(id);
  FBuilder.AddCardinal(reference);
end;

procedure TSnomedDescriptionIndex.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
  FLength := Length(FMaster);
  FBuilder.Free;
end;

function TSnomedDescriptionIndex.FindDescription(iIdentity: UInt64; var IIndex: Cardinal): boolean;
var
  aConcept : UInt64;
  L, H, I: Integer;
begin
  Result := False;
  L := 0;
  H := FLength;
  H := (H div 12) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    Move(FMaster[(i*12)], aConcept, 8);
    if aConcept < iIdentity then L := I + 1 else
    begin
      H := I - 1;
      if aConcept = iIdentity then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  if result then
    Move(FMaster[L*12+8], iIndex, 4);
end;

procedure TSnomedDescriptionIndex.StartBuild;
begin
  FBuilder := TAdvBytesBuilder.Create;
end;

{ TSnomedReferenceSetMembers }

function TSnomedReferenceSetMembers.AddMembers(const a: TSnomedReferenceSetMemberArray): Cardinal;
var
  iLoop : Integer;
Begin
  result := FBuilder.Length;
  FBuilder.AddCardinal(length(a));
  for iLoop := Low(a) to High(a) Do
  begin
    FBuilder.Append(a[iLoop].kind);
    FBuilder.AddCardinal(a[iLoop].Ref);
    FBuilder.AddCardinal(a[iLoop].Values);
  end;
end;

procedure TSnomedReferenceSetMembers.DoneBuild;
begin
  FMaster := FBuilder.AsBytes;
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
      Raise ESnomedServices.Create('Wrong length index getting Snomed list');
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
      Raise ESnomedServices.Create('Wrong length index getting Snomed list. asked for '+inttostr(iIndex)+', limit is '+inttostr(FLength));
    move(FMaster[iIndex], c, 4);
    SetLength(Result, c);
    inc(iIndex, 4);
    for i := 0 to Length(result)-1 Do
    Begin
      move(FMaster[iIndex], result[i].kind, 1);
      inc(iIndex, 1);
      move(FMaster[iIndex], result[i].Ref, 4);
      inc(iIndex, 4);
      move(FMaster[iIndex], result[i].values, 4);
      inc(iIndex, 4);
    End;
  End;
end;

procedure TSnomedReferenceSetMembers.StartBuild;
begin
  FBuilder := TAdvBytesBuilder.Create;
end;

function TSnomedServices.buildValueSet(id : String): TFhirValueSet;
var
  inc : TFhirValueSetComposeInclude;
  filt :  TFhirValueSetComposeIncludeFilter;
  cc : TFhirValueSetComposeIncludeConcept;
  i : integer;
  code, iDummy : Cardinal;
begin
  if id = 'http://snomed.info/sct?fhir_vs=refset' then
  begin
    result := TFhirValueSet.Create;
    try
      result.url := id;
      result.status := ConformanceResourceStatusActive;
      result.version := VersionDate;
      result.name := 'SNOMED CT Reference Set List';
      result.description := 'Reference Sets defined in this SNOMED-CT version';
      result.date := NowUTC;
      result.compose := TFhirValueSetCompose.Create;
      inc := result.compose.includeList.Append;
      inc.system := 'http://snomed.info/sct';
      // get the list of reference sets
      for i := 0 to RefSetIndex.Count - 1 Do
      begin
        cc := inc.conceptList.Append;
        RefSetIndex.GetReferenceSet(i, code, iDummy, iDummy, iDummy);
        cc.code := GetConceptId(code);
      end;
      result.link;
    finally
      result.free;
    end;
  end
  else if id = 'http://snomed.info/sct?fhir_vs' then
  begin
    result := TFhirValueSet.Create;
    try
      result.url := id;
      result.status := ConformanceResourceStatusActive;
      result.version := VersionDate;
      result.name := 'SNOMED CT Reference Set (All of SNOMED CT)';
      result.description := 'SNOMED CT Reference Set (All of SNOMED CT)';
      result.date := NowUTC;
      result.compose := TFhirValueSetCompose.Create;
      inc := result.compose.includeList.Append;
      inc.system := 'http://snomed.info/sct';
      result.link;
    finally
      result.free;
    end;
  end
  else if id.StartsWith('http://snomed.info/sct?fhir_vs=refset/') And ReferenceSetExists(id.Substring(38)) then
  begin
    result := TFhirValueSet.Create;
    try
      result.url := id;
      result.status := ConformanceResourceStatusActive;
      result.version := VersionDate;
      result.name := 'SNOMED CT Reference Set '+id.Substring(38);
      result.description := GetDisplayName(id.Substring(38), '');
      result.date := NowUTC;
      result.compose := TFhirValueSetCompose.Create;
      inc := result.compose.includeList.Append;
      inc.system := 'http://snomed.info/sct';
      filt := inc.filterList.Append;
      filt.property_ := 'concept';
      filt.op := FilterOperatorIn;
      filt.value := id.Substring(38);
      result.link;
    finally
      result.free;
    end;
  end
  else if id.StartsWith('http://snomed.info/sct?fhir_vs=isa/') And ConceptExists(id.Substring(35)) then
  begin
    result := TFhirValueSet.Create;
    try
      result.url := id;
      result.status := ConformanceResourceStatusActive;
      result.version := VersionDate;
      result.name := 'SNOMED CT Concept '+id.Substring(35)+' and descendents';
      result.description := 'All Snomed CT concepts for '+GetDisplayName(id.Substring(35), '');
      result.date := NowUTC;
      result.compose := TFhirValueSetCompose.Create;
      inc := result.compose.includeList.Append;
      inc.system := 'http://snomed.info/sct';
      filt := inc.filterList.Append;
      filt.property_ := 'concept';
      filt.op := FilterOperatorIsA;
      filt.value := id.Substring(35);
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
  Identity : UInt64;
  Flags : Byte;
  Group : integer;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  iWork, iWork2, iWork3, iWork4, iWork5, iWork6, refsets : Cardinal;
  date : TSnomedDate;
  Inbounds : TCardinalArray;
  did : UInt64;
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
      Rel.GetRelationship(Inbounds[i], did, iWork, iWork2, iWork3, iWork4, iWork5, iWork6, date, Flags, Group);
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
  Identity : UInt64;
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
  Identity : UInt64;
  Flags : Byte;
  Group : integer;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex : Cardinal;
  iWork, iWork2, iWork3, iWork4, iWork5, iWork6, refsets : Cardinal;
  Inbounds : TCardinalArray;
  date : TSnomedDate;
  did : UInt64;
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
      Rel.GetRelationship(Inbounds[i], did, iWork, iWork2, iWork3, iWork4, iWork5, iWork6, date, Flags, Group);
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

(*
not used?

Function TSnomedServices.GetFSN(iDescriptions : TCardinalArray) : String;
var
  iLoop : Integer;
  iid : UInt64;
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
  iid : UInt64;
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
*)

function TSnomedServices.Display(context: TCodeSystemProviderContext): string;
begin
  result := GetDisplayName(Cardinal(context), 0);
end;

procedure TSnomedServices.Displays(context: TCodeSystemProviderContext; list: TStringList);
begin
  Displays(Code(context), list);
end;

procedure TSnomedServices.extendLookup(ctxt: TCodeSystemProviderContext; props : TList<String>; resp : TFHIRLookupOpResponse);
var
  Identity : UInt64;
  Flags : Byte;
  ParentIndex, iWork, iWork2, iWork3, module, modifier, kind, iDummy : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex, InboundIndex2 : Cardinal;
  outboundIndex, valueses, refsets : Cardinal;
  Inbounds : TCardinalArray;
  date : TSnomedDate;
  Descriptions : TCardinalArray;
  Parents : TCardinalArray;
  i, group : integer;
  d : TFHIRLookupOpDesignation;
  {$IFDEF FHIR_DSTU3}
  p : TFHIRLookupOpProperty_;
  {$ENDIF}
  did : UInt64;
  function hasProp(name : String; def : boolean) : boolean;
  begin
    if (props = nil) or (props.Count = 0) then
      result := def
    else
      result := props.Contains(name);
  end;
begin
  SetLength(inbounds, 0);
  Concept.GetConcept(Cardinal(ctxt), Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
  Inbounds := Refs.GetReferences(InboundIndex);

  if hasProp('designation', true) then
  begin
    // descriptions
    Descriptions := Refs.GetReferences(DescriptionIndex);
    for i := Low(Descriptions) To High(Descriptions) Do
    Begin
      Desc.GetDescription(Descriptions[i], iWork, Identity, date, iDummy, module, kind, refsets, valueses, Flags);
      if flags and MASK_DESC_STATUS = Flag_Active Then
      Begin
        d := TFHIRLookupOpDesignation.create;
        resp.designationList.Add(d);
        d.value := Strings.GetEntry(iWork);
        d.use := TFHIRCoding.Create;
        d.use.system := 'http://snomed.info/sct';
        if ((flags and MASK_DESC_STYLE) shr 4 = VAL_DESC_Unspecified) and (kind <> 0) then
        begin
          d.use.code := GetConceptId(kind);
          d.use.display := GetPNForConcept(kind);
        end
        else
        begin
          d.use.code := '??';
          d.use.display := GetDescType(Flags);
        end;
      End;
    End;
  End;

  if hasProp('parent', true) then
  begin
    // parents:
    if ParentIndex <> 0 Then
    begin
      Parents := Refs.GetReferences(ParentIndex);
      for i := 0 to Length(Parents)-1 do
      begin
        Concept.GetConcept(Parents[i], Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex2, outboundIndex, refsets);
        Descriptions := Refs.GetReferences(DescriptionIndex);
        {$IFDEF FHIR_DSTU3}
        p := TFHIRLookupOpProperty_.create;
        resp.property_List.Add(p);
        p.code := 'parent';
        p.value := IntToStr(Identity);
        p.description := GetPN(Descriptions);
        {$ELSE}
        resp.addExtension('parent', IntToStr(Identity));
        {$ENDIF}
      end;
    end;
  end;

  if hasProp('child', true) then
  begin
    // children: (inbound relationships with type is-a)
    For i := 0 to High(Inbounds) Do
    Begin
      Rel.GetRelationship(Inbounds[i], did, iWork, iWork2, iWork3, module, kind, modifier, date, Flags, Group);
      if iWork3 = FIs_a_Index then
      begin
        Concept.GetConcept(iWork, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
        Descriptions := Refs.GetReferences(DescriptionIndex);
        {$IFDEF FHIR_DSTU3}
        p := TFHIRLookupOpProperty_.create;
        resp.property_List.Add(p);
        p.code := 'child';
        p.value := IntToStr(Identity);
        p.description := GetPN(Descriptions);
        {$ELSE}
        resp.addExtension('child', IntToStr(Identity));
        {$ENDIF}
      End;
    End;
  End;
end;

procedure TSnomedServices.Displays(code: String; list: TStringList);
var
  ctxt : TAdvObject;
begin
  ctxt := locate(code);
  if (ctxt = nil) then
    raise ESnomedServices.create('Unable to find '+code+' in '+system(nil))
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
      raise ESnomedServices.create('Unable to find '+code+' in '+system(nil))
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

function TSnomedServices.isNotClosed(textFilter: TSearchFilterText; propFilter: TCodeSystemProviderFilterContext): boolean;
begin
  result := true;
end;

function TSnomedServices.locate(code: String): TCodeSystemProviderContext;
var
  iId : UInt64;
  index : cardinal;
begin
  iId := StrToUInt64Def(code, 0);
  if Concept.FindConcept(iId, index) Then
    result := TCodeSystemProviderContext(index)
  else
    raise ESnomedServices.create('unable to find code '+code+' in '+system(nil));
end;

function TSnomedServices.system(context : TCodeSystemProviderContext): String;
begin
  result := 'http://snomed.info/sct';
end;

function TSnomedServices.TotalCount: integer;
begin
  result := Concept.Count;
end;

function TSnomedServices.version(context: TCodeSystemProviderContext): String;
begin
  result := FVersionUri;
end;

procedure TSnomedServices.Close(ctxt: TCodeSystemProviderFilterContext);
begin
  TSnomedFilterContext(ctxt).free;
end;

function TSnomedServices.filterIsA(id : UInt64): TCodeSystemProviderFilterContext;
var
  res : TSnomedFilterContext;
  index : cardinal;
begin
  res := TSnomedFilterContext.Create;
  try
    if not Concept.FindConcept(id, index) then
      raise ESnomedServices.Create('The Snomed Concept '+inttostr(id)+' was not known');
    res.descendents := GetConceptDescendents(index);
    result := TSnomedFilterContext(res.link);
  finally
    res.Free;
  end;
end;

function TSnomedServices.filterIn(id : UInt64): TCodeSystemProviderFilterContext;
var
  res : TSnomedFilterContext;
  index, members, types : cardinal;
begin
  res := TSnomedFilterContext.Create;
  try
    if not Concept.FindConcept(id, index) then
      raise ESnomedServices.Create('The Snomed Concept '+inttostr(id)+' was not known');
    if GetConceptRefSet(index, false, members, types) = 0 then
      raise ESnomedServices.Create('The Snomed Concept '+inttostr(id)+' is not a reference set');
    res.members := RefSetMembers.GetMembers(members);
    result := TSnomedFilterContext(res.link);
  finally
    res.Free;
  end;
end;

function TSnomedServices.filter(prop: String; op: TFhirFilterOperatorEnum; value: String; prep : TCodeSystemProviderFilterPreparationContext): TCodeSystemProviderFilterContext;
var
  id : UInt64;
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
  result := nil;
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


function TSnomedServices.name(context: TCodeSystemProviderContext): String;
begin
  result := 'SNOMED CT';
end;

Function TSnomedServices.GetPNForConcept(iIndex : Cardinal) : String;
var
  Identity : UInt64;
  Flags : Byte;
  ParentIndex : Cardinal;
  DescriptionIndex : Cardinal;
  InboundIndex : Cardinal;
  outboundIndex, refsets : Cardinal;
  Descriptions : TCardinalArray;
  date : TSnomedDate;
Begin
  Concept.GetConcept(iIndex, Identity, Flags, date, ParentIndex, DescriptionIndex, InboundIndex, outboundIndex, refsets);
  Descriptions := Refs.GetReferences(DescriptionIndex);
  result := GetPN(Descriptions);
End;

Function GetDescType(Flags : Byte) : String;
Begin
  case (flags and MASK_DESC_STYLE) shr 4 of
    VAL_DESC_Unspecified : result := 'Unspecified';
    VAL_DESC_Preferred : result := 'Preferred';
    VAL_DESC_Synonym : result := 'Synonym';
    VAL_DESC_FullySpecifiedName : result := 'FSN';
  End;
End;

Function GetDescStatus1(Flags : Byte) : String;
Begin
  case (flags and MASK_DESC_STATUS) of
    FLAG_Active : Result := 'Active';
    FLAG_RetiredWithoutStatedReason : Result := 'Retired Without Stated Reason';
    FLAG_Duplicate : Result := 'Duplicate';
    FLAG_Outdated : Result := 'Outdated';
    FLAG_Ambiguous : Result := 'Ambiguous';
    FLAG_Erroneous : Result := 'Erroneous';
    FLAG_Limited : Result := 'Limited';
    FLAG_Inappropriate : Result := 'Inappropriate';
    FLAG_ConceptInactive : Result := 'Concept Inactive';
    FLAG_MovedElswhere : Result := 'Moved Elswhere';
    FLAG_PendingMove : Result := 'Pending Move';
  End;
End;

Function GetDescStatus2(Flags : Byte) : String;
begin
  case flags and MASK_DESC_CAPS_MASK of
    MASK_DESC_CAPS_NONE: result := 'No';
    MASK_DESC_CAPS_FIRST: result := 'First Char';
    MASK_DESC_CAPS_ALL: result := 'Yes';
  end;
End;

Function GetRelChar(Flags : Byte) : String;
Begin
  case (flags and MASK_REL_CHARACTERISTIC) of
    VAL_REL_Defining : result := 'Defining';
    VAL_REL_Qualifying : result := 'Qualifying';
    VAL_REL_Historical : result := 'Historical';
    VAL_REL_Additional : result := 'Additional';
  else
    result := '';
  End;
End;

Function GetRelRefinability(Flags : Byte) : String;
Begin
  case (flags and MASK_REL_REFINABILITY) shr 4 of
    VAL_REL_NotRefinable : result := 'No';
    VAL_REL_Optional : result := 'Optional';
    VAL_REL_Mandatory : result := 'Mandatory';
  else
    result := '';
  End;
End;


End.
