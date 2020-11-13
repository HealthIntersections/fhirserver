unit v2_dictionary_Versions;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)

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

Uses
  v2_base, v2_dictionary;

Type
  THL7V2CodeDictionary = class (THL7V2Dictionary)
  Protected
    procedure PrepareForLoad(bWipe: Boolean); Override;
    procedure DoneLoading(aTransferEvent: TOnDictTransferProgress); Override;

    Function  ListVersions : THL7V2Versions; Override;
    function  VersionDefined(aVersion : THL7V2Version; var sDesc: String): Boolean; Override;
    procedure AddVersion(aVersion : THL7V2Version; const sDescription: String); Override;

    procedure LoadTables(aVersion : THL7V2Version; oTables : THL7V2ModelTables); Override;
    procedure LoadComponents(aVersion : THL7V2Version; oComponents : THL7V2ModelComponents); Override;
    procedure LoadDataElements(aVersion : THL7V2Version; oDataElements : THL7V2ModelDataElements); Override;
    procedure LoadSegments(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments); Override;
    procedure LoadSegmentFields(aVersion : THL7V2Version; oSegment : THL7V2ModelSegment); Override;
    procedure LoadSegmentFields(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments); Override;
    procedure LoadDataTypes(aVersion : THL7V2Version; oDataTypes : THL7V2ModelDataTypes); Override;
    procedure LoadStructures(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures); Override;
    procedure LoadStructureComponents(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures; oComponents : THL7V2ModelComponents); Override;
    procedure LoadEvents(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents); Override;
    procedure LoadEventMessages(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents); Override;
    procedure LoadMessageStructures(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures); Override;
    procedure LoadSegmentMaps(aVersion : THL7V2Version; oStructures : THL7V2ModelMessageStructures); Override;

    procedure AddTables(aVersion : THL7V2Version; oTables : THL7V2ModelTables); Override;
    procedure AddComponents(aVersion : THL7V2Version; oComponents : THL7V2ModelComponents); Override;
    procedure AddDataElements(aVersion : THL7V2Version; oDataElements : THL7V2ModelDataElements); Override;
    procedure AddSegments(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments); Override;
    procedure AddSegmentFields(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments); Override;
    procedure AddDataTypes(aVersion : THL7V2Version; oDataTypes : THL7V2ModelDataTypes); Override;
    procedure AddStructures(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures); Override;
    procedure AddStructureComponents(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures); Override;
    procedure AddEvents(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents); Override;
    procedure AddEventMessages(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents); Override;
    procedure AddMessageStructures(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures); Override;
    procedure AddSegmentMaps(aVersion : THL7V2Version; oStructures : THL7V2ModelMessageStructures); Override;
  Public
    function SourceDescription(bFulldetails: Boolean): String; Override;
    procedure Initialise; Override;
  End;

Function  DefinitionsListVersions : THL7V2Versions;
function  DefinitionsVersionDefined(aVersion : THL7V2Version; var sDesc: String): Boolean;
procedure DefinitionsLoadTables(aVersion : THL7V2Version; oTables : THL7V2ModelTables);
procedure DefinitionsLoadComponents(aVersion : THL7V2Version; oComponents : THL7V2ModelComponents);
procedure DefinitionsLoadDataElements(aVersion : THL7V2Version; oDataElements : THL7V2ModelDataElements);
procedure DefinitionsLoadSegments(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments);
procedure DefinitionsLoadSegmentFields(aVersion : THL7V2Version; oSegment : THL7V2ModelSegment); Overload;
procedure DefinitionsLoadSegmentFields(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments); Overload;
procedure DefinitionsLoadDataTypes(aVersion : THL7V2Version; oDataTypes : THL7V2ModelDataTypes);
procedure DefinitionsLoadStructures(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures);
procedure DefinitionsLoadStructureComponents(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures; oComponents : THL7V2ModelComponents);
procedure DefinitionsLoadEvents(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents);
procedure DefinitionsLoadEventMessages(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents);
procedure DefinitionsLoadMessageStructures(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures);
procedure DefinitionsLoadSegmentMaps(aVersion : THL7V2Version; oStructures : THL7V2ModelMessageStructures);

Implementation

Uses
  v2_dictionary_v21,
  v2_dictionary_v22,
  v2_dictionary_v23,
  v2_dictionary_v231,
  v2_dictionary_v24,
  v2_dictionary_v25,
  v2_dictionary_v251,
  v2_dictionary_v26,
  v2_dictionary_v27;

Function  DefinitionsListVersions : THL7V2Versions;
Begin
  Result := [hv21, hv22, hv23, hv231, hv24, hv25, hv251, hv26, hv27];
End;

function  DefinitionsVersionDefined(aVersion : THL7V2Version; var sDesc: String): Boolean;
Begin
  Result := True;
  If aVersion = hv21 Then
    sDesc := 'Official Standard'
  Else If aVersion = hv22 Then
    sDesc := 'Official Standard'
  Else If aVersion = hv23 Then
    sDesc := 'Official Standard'
  Else If aVersion = hv231 Then
    sDesc := 'Official Standard'
  Else If aVersion = hv24 Then
    sDesc := 'Official Standard'
  Else If aVersion = hv25 Then
    sDesc := 'Official Standard'
  Else If aVersion = hv251 Then
    sDesc := 'Official Standard'
  Else If aVersion = hv26 Then
    sDesc := 'Official Standard'
  Else If aVersion = hv27 Then
    sDesc := 'Official Standard'
  Else
    Result := False;
End;

procedure DefinitionsLoadTables(aVersion : THL7V2Version; oTables : THL7V2ModelTables);
Begin
  Case aVersion Of
    hv21: Definitions21LoadTables(oTables);
    hv22: Definitions22LoadTables(oTables);
    hv23: Definitions23LoadTables(oTables);
    hv231: Definitions231LoadTables(oTables);
    hv24: Definitions24LoadTables(oTables);
    hv25: Definitions25LoadTables(oTables);
    hv251: Definitions251LoadTables(oTables);
    hv26: Definitions26LoadTables(oTables);
    hv27: Definitions27LoadTables(oTables);
  End;
End;

procedure DefinitionsLoadComponents(aVersion : THL7V2Version; oComponents : THL7V2ModelComponents);
Begin
  Case aVersion Of
    hv21: Definitions21LoadComponents(oComponents);
    hv22: Definitions22LoadComponents(oComponents);
    hv23: Definitions23LoadComponents(oComponents);
    hv231: Definitions231LoadComponents(oComponents);
    hv24: Definitions24LoadComponents(oComponents);
    hv25: Definitions25LoadComponents(oComponents);
    hv251: Definitions251LoadComponents(oComponents);
    hv26: Definitions26LoadComponents(oComponents);
    hv27: Definitions27LoadComponents(oComponents);
  End;
End;

procedure DefinitionsLoadDataElements(aVersion : THL7V2Version; oDataElements : THL7V2ModelDataElements);
Begin
  Case aVersion Of
    hv21: Definitions21LoadDataElements(oDataElements);
    hv22: Definitions22LoadDataElements(oDataElements);
    hv23: Definitions23LoadDataElements(oDataElements);
    hv231: Definitions231LoadDataElements(oDataElements);
    hv24: Definitions24LoadDataElements(oDataElements);
    hv25: Definitions25LoadDataElements(oDataElements);
    hv251: Definitions251LoadDataElements(oDataElements);
    hv26: Definitions26LoadDataElements(oDataElements);
    hv27: Definitions27LoadDataElements(oDataElements);
  End;
End;

procedure DefinitionsLoadSegments(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments);
Begin
  Case aVersion Of
    hv21: Definitions21LoadSegments(oSegments);
    hv22: Definitions22LoadSegments(oSegments);
    hv23: Definitions23LoadSegments(oSegments);
    hv231: Definitions231LoadSegments(oSegments);
    hv24: Definitions24LoadSegments(oSegments);
    hv25: Definitions25LoadSegments(oSegments);
    hv251: Definitions251LoadSegments(oSegments);
    hv26: Definitions26LoadSegments(oSegments);
    hv27: Definitions27LoadSegments(oSegments);
  End;
End;

procedure DefinitionsLoadSegmentFields(aVersion : THL7V2Version; oSegment : THL7V2ModelSegment);
Begin
  // we ignore this - use DefinitionsLoadSegments instead and load both structures and components at once (faster)
End;

procedure DefinitionsLoadSegmentFields(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments);
Begin
  // we ignore this - use DefinitionsLoadSegments instead and load both structures and components at once (faster)
End;

procedure DefinitionsLoadDataTypes(aVersion : THL7V2Version; oDataTypes : THL7V2ModelDataTypes);
Begin
  Case aVersion Of
    hv21: Definitions21LoadDataTypes(oDataTypes);
    hv22: Definitions22LoadDataTypes(oDataTypes);
    hv23: Definitions23LoadDataTypes(oDataTypes);
    hv231: Definitions231LoadDataTypes(oDataTypes);
    hv24: Definitions24LoadDataTypes(oDataTypes);
    hv25: Definitions25LoadDataTypes(oDataTypes);
    hv251: Definitions251LoadDataTypes(oDataTypes);
    hv26: Definitions26LoadDataTypes(oDataTypes);
    hv27: Definitions27LoadDataTypes(oDataTypes);
  End;
End;

procedure DefinitionsLoadStructures(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures);
Begin
  // we ignore this - use DefinitionsLoadStructureComponents instead and load both structures and components at once (faster)
End;

procedure DefinitionsLoadStructureComponents(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures; oComponents : THL7V2ModelComponents);
Begin
  Case aVersion Of
    hv21: Definitions21LoadStructures(oStructures, oComponents);
    hv22: Definitions22LoadStructures(oStructures, oComponents);
    hv23: Definitions23LoadStructures(oStructures, oComponents);
    hv231: Definitions231LoadStructures(oStructures, oComponents);
    hv24: Definitions24LoadStructures(oStructures, oComponents);
    hv25: Definitions25LoadStructures(oStructures, oComponents);
    hv251: Definitions251LoadStructures(oStructures, oComponents);
    hv26: Definitions26LoadStructures(oStructures, oComponents);
    hv27: Definitions27LoadStructures(oStructures, oComponents);
  End;
End;

procedure DefinitionsLoadEvents(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents);
Begin
  Case aVersion Of
    hv21: Definitions21LoadEvents(oEvents);
    hv22: Definitions22LoadEvents(oEvents);
    hv23: Definitions23LoadEvents(oEvents);
    hv231: Definitions231LoadEvents(oEvents);
    hv24: Definitions24LoadEvents(oEvents);
    hv25: Definitions25LoadEvents(oEvents);
    hv251: Definitions251LoadEvents(oEvents);
    hv26: Definitions26LoadEvents(oEvents);
    hv27: Definitions27LoadEvents(oEvents);
  End;
End;

procedure DefinitionsLoadEventMessages(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents);
Begin
  // we ignore this - use DefinitionsLoadEvents instead and load both events and messages at once (faster)
End;

procedure DefinitionsLoadMessageStructures(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures);
Begin
  Case aVersion Of
    hv21: Definitions21LoadMessageStructures(oMessageStructures);
    hv22: Definitions22LoadMessageStructures(oMessageStructures);
    hv23: Definitions23LoadMessageStructures(oMessageStructures);
    hv231: Definitions231LoadMessageStructures(oMessageStructures);
    hv24: Definitions24LoadMessageStructures(oMessageStructures);
    hv25: Definitions25LoadMessageStructures(oMessageStructures);
    hv251: Definitions251LoadMessageStructures(oMessageStructures);
    hv26: Definitions26LoadMessageStructures(oMessageStructures);
    hv27: Definitions27LoadMessageStructures(oMessageStructures);
  End;
End;

procedure DefinitionsLoadSegmentMaps(aVersion : THL7V2Version; oStructures : THL7V2ModelMessageStructures);
Begin
  // we ignore this - use DefinitionsLoadMessageStructures instead and load both structures and maps at once (faster)
End;

function THL7V2CodeDictionary.SourceDescription(bFulldetails: Boolean): String;
Begin
  Result := 'Embedded (Code)';
End;

procedure THL7V2CodeDictionary.Initialise;
Begin
  // nothing needed
End;

procedure THL7V2CodeDictionary.PrepareForLoad(bWipe: Boolean);
Begin
  RaiseError('PrepareForLoad', 'THL7V2CodeDictionary is read only');
End;

procedure THL7V2CodeDictionary.DoneLoading(aTransferEvent: TOnDictTransferProgress);
Begin
  RaiseError('DoneLoading', 'THL7V2CodeDictionary is read only');
End;

procedure THL7V2CodeDictionary.AddVersion(aVersion : THL7V2Version; const sDescription: String);
Begin
  RaiseError('AddVersion', 'THL7V2CodeDictionary is read only');
End;

procedure THL7V2CodeDictionary.AddTables(aVersion : THL7V2Version; oTables : THL7V2ModelTables);
Begin
  RaiseError('AddTables', 'THL7V2CodeDictionary is read only');
End;

procedure THL7V2CodeDictionary.AddComponents(aVersion : THL7V2Version; oComponents : THL7V2ModelComponents);
Begin
  RaiseError('AddComponents', 'THL7V2CodeDictionary is read only');
End;

procedure THL7V2CodeDictionary.AddDataElements(aVersion : THL7V2Version; oDataElements : THL7V2ModelDataElements);
Begin
  RaiseError('AddDataElements', 'THL7V2CodeDictionary is read only');
End;

procedure THL7V2CodeDictionary.AddSegments(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments);
Begin
  RaiseError('AddSegments', 'THL7V2CodeDictionary is read only');
End;

procedure THL7V2CodeDictionary.AddSegmentFields(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments);
Begin
  RaiseError('AddSegmentFields', 'THL7V2CodeDictionary is read only');
End;

procedure THL7V2CodeDictionary.AddDataTypes(aVersion : THL7V2Version; oDataTypes : THL7V2ModelDataTypes);
Begin
  RaiseError('AddDataTypes', 'THL7V2CodeDictionary is read only');
End;

procedure THL7V2CodeDictionary.AddStructures(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures);
Begin
  RaiseError('AddStructures', 'THL7V2CodeDictionary is read only');
End;

procedure THL7V2CodeDictionary.AddStructureComponents(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures);
Begin
  RaiseError('AddStructureComponents', 'THL7V2CodeDictionary is read only');
End;

procedure THL7V2CodeDictionary.AddEvents(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents);
Begin
  RaiseError('AddEvents', 'THL7V2CodeDictionary is read only');
End;

procedure THL7V2CodeDictionary.AddEventMessages(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents);
Begin
  RaiseError('AddEventMessages', 'THL7V2CodeDictionary is read only');
End;

procedure THL7V2CodeDictionary.AddMessageStructures(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures);
Begin
  RaiseError('AddMessageStructures', 'THL7V2CodeDictionary is read only');
End;

procedure THL7V2CodeDictionary.AddSegmentMaps(aVersion : THL7V2Version; oStructures : THL7V2ModelMessageStructures);
Begin
  RaiseError('AddSegmentMaps', 'THL7V2CodeDictionary is read only');
End;

Function  THL7V2CodeDictionary.ListVersions : THL7V2Versions;
Begin
  Result := DefinitionsListVersions;
End;

function  THL7V2CodeDictionary.VersionDefined(aVersion : THL7V2Version; var sDesc: String): Boolean;
Begin
  Result := DefinitionsVersionDefined(aVersion, sDesc);
End;

procedure THL7V2CodeDictionary.LoadTables(aVersion : THL7V2Version; oTables : THL7V2ModelTables);
Begin
  DefinitionsLoadTables(aVersion, oTables);
End;

procedure THL7V2CodeDictionary.LoadComponents(aVersion : THL7V2Version; oComponents : THL7V2ModelComponents);
Begin
  DefinitionsLoadComponents(aVersion, oComponents);
End;

procedure THL7V2CodeDictionary.LoadDataElements(aVersion : THL7V2Version; oDataElements : THL7V2ModelDataElements);
Begin
  DefinitionsLoadDataElements(aVersion, oDataElements);
End;

procedure THL7V2CodeDictionary.LoadSegments(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments);
Begin
  DefinitionsLoadSegments(aVersion, oSegments);
End;

procedure THL7V2CodeDictionary.LoadSegmentFields(aVersion : THL7V2Version; oSegment : THL7V2ModelSegment);
Begin
  DefinitionsLoadSegmentFields(aVersion, oSegment);
End;

procedure THL7V2CodeDictionary.LoadSegmentFields(aVersion : THL7V2Version; oSegments : THL7V2ModelSegments);
Begin
  DefinitionsLoadSegmentFields(aVersion, oSegments);
End;

procedure THL7V2CodeDictionary.LoadDataTypes(aVersion : THL7V2Version; oDataTypes : THL7V2ModelDataTypes);
Begin
  DefinitionsLoadDataTypes(aVersion, oDataTypes);
End;

procedure THL7V2CodeDictionary.LoadStructures(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures);
Begin
  DefinitionsLoadStructures(aVersion, oStructures);
End;

procedure THL7V2CodeDictionary.LoadStructureComponents(aVersion : THL7V2Version; oStructures : THL7V2ModelStructures; oComponents : THL7V2ModelComponents);
Begin
  DefinitionsLoadStructureComponents(aVersion, oStructures, oComponents);
End;

procedure THL7V2CodeDictionary.LoadEvents(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents);
Begin
  DefinitionsLoadEvents(aVersion, oEvents);
End;

procedure THL7V2CodeDictionary.LoadEventMessages(aVersion : THL7V2Version; oEvents : THL7V2ModelEvents);
Begin
  DefinitionsLoadEventMessages(aVersion, oEvents);
End;

procedure THL7V2CodeDictionary.LoadMessageStructures(aVersion : THL7V2Version; oMessageStructures : THL7V2ModelMessageStructures);
Begin
  DefinitionsLoadMessageStructures(aVersion, oMessageStructures);
End;

procedure THL7V2CodeDictionary.LoadSegmentMaps(aVersion : THL7V2Version; oStructures : THL7V2ModelMessageStructures);
Begin
  DefinitionsLoadSegmentMaps(aVersion, oStructures);
End;

End.
