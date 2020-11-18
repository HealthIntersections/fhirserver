Unit wp_definers;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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
  SysUtils, Graphics,
  fsl_utilities,
  fsl_base, fsl_collections,
  wp_document, wp_types;

Const
  NS_FIELD_HOTSPOT = 'libraries\publishing\wordprocessor\hotspot';

Type
  TEditAnnotationResult = (earUnchanged, earChanged, earDelete);

  TWPAnnotationDefinitionProvider = Class (TFslObject)

    Public
      Function Link : TWPAnnotationDefinitionProvider; Overload;

      // The namespace of annotations associated with this definer. Since the annotations persist
      // persist, the namespace should never change.
      Function GetNamespace : String; Overload; Virtual;
      Function GetTitle : String; Overload; Virtual;  // The text string that identifies this definer to the user
      Function GetIconIndex : Integer; Overload; Virtual; // the icon index in the file WPIconModule.bmp. -1 means default. (ideas for a better approach welcomed)

      // These are queried to find out whether annotations can be inserted by this definer
      Function CanInsertAnnotation(sText : String) : Boolean; Overload; Virtual;
      Function IsInsertKey(iKey: Word; Const aShift: TWPShiftStates; sText : String) : Boolean; Overload; Virtual;

      // sText is the content being annotated
      Function EditAnnotation(sText : String; var sAnnotation : String) : TEditAnnotationResult; Overload; Virtual;
      Function UserCanDeleteAnnotation(sText, sAnnotation : String) : Boolean; Overload; Virtual;
      Function GetColour(sAnnotation : String) : TColour; Overload; Virtual;
      Function GetDisplay(sAnnotation : String) : String; Overload; Virtual;
      Function FontSize : Integer; Overload; Virtual;
  End;


  TWPAnnotationDefinitionProviderList = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPAnnotationDefinitionProvider;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPAnnotationDefinitionProvider);

    Protected
      Function ItemClass : TFslObjectClass; Override;
    Public
      Function Link : TWPAnnotationDefinitionProviderList; Overload;
      Function Clone : TWPAnnotationDefinitionProviderList; Overload;

      Function New : TWPAnnotationDefinitionProvider; Reintroduce; Overload; Virtual;

      Function GetByName(Const sName : String; oDefault : TWPAnnotationDefinitionProvider) : TWPAnnotationDefinitionProvider; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPAnnotationDefinitionProvider Read GetElement Write SetElement; Default;
  End;

Type
  TFieldSpellingCheckState = (fscsNone, fscsField, fscsSpelling, fscsFieldSpelling);
  TFieldHintMode = (fhmFull, fhmError);

  // Descendents must override Namespace and Title
  TWPFieldDefinitionProvider = Class (TFslObject)

    Public
      Function Link : TWPFieldDefinitionProvider; Overload;

      // The namespace of fields associated with this definer. Since the fields may
      // persist, the namespace should never change. The general format roughly
      // follows the code, but not necessarily (i.e. code can be moved, but namespaces
      // never change). Some example namepaces:
      //   libraries\publishing\wordprocessor\manual - manual fields using the infrastructure provided by the WP
      //   libraries\publishing\wordprocessor\properties - fields in the property inspector
      //   libraries\publishing\reports - report specific fields
      // Namespaces must be unique within a given WP, but should be globally unique
      Function GetNamespace : String; Overload; Virtual;
      Function GetTitle : String; Overload; Virtual;  // The text string that identifies this definer to the user
      Function GetStdIconIndex : Integer; Overload; Virtual; // the icon index in the file WPIconModule.bmp. -1 means default. (ideas for a better approach welcomed)
      Function GetTouchIconIndex : Integer; Overload; Virtual; // the icon index in the file WPIconModule.bmp. -1 means default. (ideas for a better approach welcomed)


      // These are queried to find out whether fields or sections
      // can be inserted by this definer
      Function CanInsertField : Boolean; Overload; Virtual;

      // inserting a defined field is a 4 step operation
      // 1. the user asks to insert a field, chooses a definer
      // 2. a new field is created, and given the style and font details
      //    it will acquire when it is inserted.
      // 3. the field is then passed to NewField. The definer can interact with the
      //    user to determine how to set up the field & populate it's contents as
      //    desired. Returns boolean whether the field should actually be inserted
      // 4. the field and text is inserted. User focus shifts to the end of the field end.
      //
      // There is two types of field: TWPDocumentField and TWPDocumentSection.
      // Roughly speaking these correspond to in-line and block fields.
      // parameters:
      //  oField - a prepped oField with appropriate style defaults for the contest in which it's going to be inserted
      //  bSection - whether a section can be returned in place of a Field (Sections can't always be inserted)
      //  oSection - out parameter. Leave as null if the field should be inserted. Return this if a section should be inserted instead of a field
      //  Result - true if field or section should be inserted, false if nothing shouild be inserted
      //
      //  This interface is a bit ugly. It'd be a whole lot cleaner if either you didn't have
      //  to differentiate between a field and a section outside the WP (but you do, they can do
      //  different things), or if the two interfaces were split. But this imposes a workflow
      //  problem on the user. So this interface is it. Sorry
      //
      Function NewField(oField : TWPDocumentField; bSection : Boolean; var bAcceptExistingContent : Boolean; Var oSection : TWPDocumentSection) : Boolean; Overload; Virtual;

      // Whether field properties can be edited. Note field and section will be
      // empty irrespective of the contents of the document - this is just so their
      // immediate properties can be inspected
      Function UserCanEditField(oField : TWPDocumentField) : Boolean; Overload; Virtual;
      Function UserCanEditField(oSection : TWPDocumentSection) : Boolean; Overload; Virtual;

      // editing a field. Again, content is not included, only the properties
      // of the field or section are available
      Function EditField(oField : TWPDocumentField) : Boolean; Overload; Virtual;
      Function EditField(oSection : TWPDocumentSection) : Boolean; Overload; Virtual;

      // Whether field can be deleted. Note field and section will be
      // empty irrespective of the contents of the document - this is just so their
      // immediate properties can be inspected
      Function UserCanDeleteField(oField : TWPDocumentField) : Boolean; Overload; Virtual;
      Function UserCanDeleteField(oSection : TWPDocumentSection) : Boolean; Overload; Virtual;

      // Whether text in the field can be edited
      Function UserCanEditTextInField(oField : TWPDocumentField) : Boolean; Overload; Virtual;
      Function UserCanEditTextInField(oSection : TWPDocumentSection) : Boolean; Overload; Virtual;

      // called when text is completed being entered into the field (not a section type field)
      // you can't stop or change the content, but you may respond in various ways
      Procedure Commit(oField : TWPDocumentField; Const sContent : String); Overload; Virtual;

      // find whether a particular background colour should be used for the field when
      // editing the document interactively. false = default colour
      Function GetBackground(oField : TWPDocumentField; bEditing : Boolean; Var aBackground : TColour) : Boolean; Overload; Virtual;

      // find out whether to draw the dropper image that suggests to the user that there's UI assistance for the
      // field (i.e. picklist etc) (when editing the document interactively)
      Function HasUIAssistance(oField : TWPDocumentField) : Boolean; Overload; Virtual;

      // find out whether to display a list of checkable items
      // as part of the field
      Function HasCheckables(oField : TWPDocumentField) : Boolean; Overload; Virtual;

      // get the list of checkable options to display
      Procedure GetCheckables(oField : TWPDocumentField; oList : TFslStringList); Overload; Virtual;

      // user wants assistance (i.e. ctrl-space). if return false, the WP will ignore the field and pretend it isn't defined
      Function CodeComplete(oField : TWPDocumentField; Const sContent : String) : Boolean; Overload; Virtual;

      // user has entered text into the field. Is it ok?
      // Note that prohibiting invalid input should only done with care - if the user cannot create
      // transiently invalid input, they may not be able to generate valid input either.
      // if the proposed input is labeled as invalid, the keystroke/user action will be ignored
      Function CheckProposed(oField : TWPDocumentField; Const sContent : String) : Boolean; Overload; Virtual;

      // check contents of field. This is only called if CheckSpelling returns false.
      Function CheckContent(oField : TWPDocumentField; Const sContent : String; var sError : String) : Boolean; Overload; Virtual;

      // Get the hint for the field. Needs to be quick
      // hint mode: error - shown as part of an error message when input fail validation. Full - shown on the field itself to help user know what field is
      Function HintForField(oField : TWPDocumentField; aHintMode : TFieldHintMode) : String; Overload; Virtual;

      // return true if spelling should be checked in the field
      Function ShouldCheckSpelling(oField : TWPDocumentField) : TFieldSpellingCheckState; Overload; Virtual;

      // if the user right clicks on a field that has failed validation, and ShouldCheckSpelling is false,
      // this method will be called to populate a list of possible suggestions. It's not necessary to
      // fill any out
      Procedure ListSuggestions(oField : TWPDocumentField; oList : TFslStringList); Overload; Virtual;

      // called before calling onUpdateField so definition provider can change the format of the content if necessary
      Function PreparePublicValue(oField : TWPDocumentField; sContent : String) : String; Overload; Virtual;

      // called when the document is loaded
      Function UpdateFieldOnLoad(oField : TWPDocumentField; sContent : String; var newText : String) : boolean; Overload; Virtual;
  End;

  TWPFieldDefinitionProviderList = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPFieldDefinitionProvider;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPFieldDefinitionProvider);

    Protected
      Function ItemClass : TFslObjectClass; Override;
    Public
      Function Link : TWPFieldDefinitionProviderList; Overload;
      Function Clone : TWPFieldDefinitionProviderList; Overload;

      Function New : TWPFieldDefinitionProvider; Reintroduce; Overload; Virtual;

      Function GetByName(Const sName : String; oDefault : TWPFieldDefinitionProvider) : TWPFieldDefinitionProvider; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPFieldDefinitionProvider Read GetElement Write SetElement; Default;
  End;


Implementation


Function TWPAnnotationDefinitionProvider.Link : TWPAnnotationDefinitionProvider;
Begin
  Result := TWPAnnotationDefinitionProvider(Inherited Link);
End;

Function TWPAnnotationDefinitionProvider.GetNamespace: String;
Begin
  RaiseError('Namespace', 'Must be overriden');
End;

Function TWPAnnotationDefinitionProvider.GetTitle: String;
Begin
  RaiseError('Namespace', 'Must be overriden');
End;


{ TWPAnnotationDefinitionProviderList }

Function TWPAnnotationDefinitionProviderList.Clone: TWPAnnotationDefinitionProviderList;
Begin
  Result := TWPAnnotationDefinitionProviderList(Inherited Clone);
End;

Function TWPAnnotationDefinitionProviderList.GetByName(Const sName: String; oDefault : TWPAnnotationDefinitionProvider): TWPAnnotationDefinitionProvider;
Var
  iLoop : Integer;
Begin
  Result := Nil;
  iLoop := 0;
  While (iLoop < Count) And (Result = Nil) Do
  Begin
    If (Elements[iLoop].GetNamespace = sName) Then
      Result := Elements[iLoop];
    Inc(iLoop);
  End;
  If (Result = Nil) Then
    Result := oDefault;
End;

Function TWPAnnotationDefinitionProviderList.GetElement(Const iIndex: Integer): TWPAnnotationDefinitionProvider;
Begin
  Result := TWPAnnotationDefinitionProvider(ObjectByIndex[iIndex]);
End;

Function TWPAnnotationDefinitionProviderList.ItemClass: TFslObjectClass;
Begin
  Result := TWPAnnotationDefinitionProvider;
End;

Function TWPAnnotationDefinitionProviderList.Link: TWPAnnotationDefinitionProviderList;
Begin
  Result := TWPAnnotationDefinitionProviderList(Inherited Link);
End;

Function TWPAnnotationDefinitionProviderList.New: TWPAnnotationDefinitionProvider;
Begin
  Result := TWPAnnotationDefinitionProvider(Inherited New);
End;

Procedure TWPAnnotationDefinitionProviderList.SetElement(Const iIndex: Integer; Const oValue: TWPAnnotationDefinitionProvider);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;

Function TWPAnnotationDefinitionProvider.GetIconIndex: Integer;
Begin
  Result := -1;
End;

function TWPAnnotationDefinitionProvider.CanInsertAnnotation(sText: String): Boolean;
begin
  result := true;
end;

function TWPAnnotationDefinitionProvider.EditAnnotation(sText: String; var sAnnotation: String): TEditAnnotationResult;
begin
  result := earUnchanged;
end;

function TWPAnnotationDefinitionProvider.GetColour(sAnnotation: String): TColour;
begin
  result := clBlack;
end;

function TWPAnnotationDefinitionProvider.GetDisplay(sAnnotation: String): String;
begin
  result := sAnnotation;
end;


function TWPAnnotationDefinitionProvider.UserCanDeleteAnnotation(sText, sAnnotation: String): Boolean;
begin
  result := true;
end;

function TWPAnnotationDefinitionProvider.FontSize: Integer;
begin
  result := 8;
end;

function TWPAnnotationDefinitionProvider.IsInsertKey(iKey: Word; const aShift: TWPShiftStates; sText: String): Boolean;
begin
  result := false;
end;
Function TWPFieldDefinitionProvider.CanInsertField: Boolean;
Begin
  Result := False;
End;

Function TWPFieldDefinitionProvider.EditField(oField: TWPDocumentField): Boolean;
Begin
  Result := False;
End;

Function TWPFieldDefinitionProvider.EditField(oSection: TWPDocumentSection): Boolean;
Begin
  Result := False;
End;

Function TWPFieldDefinitionProvider.Link : TWPFieldDefinitionProvider;
Begin
  Result := TWPFieldDefinitionProvider(Inherited Link);
End;

Function TWPFieldDefinitionProvider.GetNamespace: String;
Begin
  RaiseError('Namespace', 'Must be overriden');
End;

Function TWPFieldDefinitionProvider.GetTitle: String;
Begin
  RaiseError('Namespace', 'Must be overriden');
End;


{ TWPFieldDefinitionProviderList }

Function TWPFieldDefinitionProviderList.Clone: TWPFieldDefinitionProviderList;
Begin
  Result := TWPFieldDefinitionProviderList(Inherited Clone);
End;

Function TWPFieldDefinitionProviderList.GetByName(Const sName: String; oDefault : TWPFieldDefinitionProvider): TWPFieldDefinitionProvider;
Var
  iLoop : Integer;
Begin
  Result := Nil;
  iLoop := 0;
  While (iLoop < Count) And (Result = Nil) Do
  Begin
    If (Elements[iLoop].GetNamespace = sName) Then
      Result := Elements[iLoop];
    Inc(iLoop);
  End;
  If (Result = Nil) Then
    Result := oDefault;
End;

Function TWPFieldDefinitionProviderList.GetElement(Const iIndex: Integer): TWPFieldDefinitionProvider;
Begin
  Result := TWPFieldDefinitionProvider(ObjectByIndex[iIndex]);
End;

Function TWPFieldDefinitionProviderList.ItemClass: TFslObjectClass;
Begin
  Result := TWPFieldDefinitionProvider;
End;

Function TWPFieldDefinitionProviderList.Link: TWPFieldDefinitionProviderList;
Begin
  Result := TWPFieldDefinitionProviderList(Inherited Link);
End;

Function TWPFieldDefinitionProviderList.New: TWPFieldDefinitionProvider;
Begin
  Result := TWPFieldDefinitionProvider(Inherited New);
End;

Procedure TWPFieldDefinitionProviderList.SetElement(Const iIndex: Integer; Const oValue: TWPFieldDefinitionProvider);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;

Function TWPFieldDefinitionProvider.GetStdIconIndex: Integer;
Begin
  Result := -1;
End;

Function TWPFieldDefinitionProvider.GetTouchIconIndex: Integer;
Begin
  Result := -1;
End;

Procedure TWPFieldDefinitionProvider.Commit(oField: TWPDocumentField; Const sContent: String);
Begin
 // nothing
End;

Function TWPFieldDefinitionProvider.NewField(oField: TWPDocumentField; bSection: Boolean; var bAcceptExistingContent : boolean ; Var oSection: TWPDocumentSection): Boolean;
Begin
  Result := False;
End;

Function TWPFieldDefinitionProvider.UserCanEditField(oField: TWPDocumentField): Boolean;
Begin
  Result := False;
End;

Function TWPFieldDefinitionProvider.UserCanEditField(oSection: TWPDocumentSection): Boolean;
Begin
  Result := False;
End;

Function TWPFieldDefinitionProvider.GetBackground(oField : TWPDocumentField; bEditing : Boolean; Var aBackground: TColour): Boolean;
Begin
  Result := False;
End;

Function TWPFieldDefinitionProvider.HasUIAssistance(oField : TWPDocumentField): Boolean;
Begin
  Result := False;
End;

Function TWPFieldDefinitionProvider.CodeComplete(oField : TWPDocumentField; Const sContent: String) : Boolean;
Begin
  Result := False;
End;

Function TWPFieldDefinitionProvider.CheckProposed(oField : TWPDocumentField; Const sContent: String): Boolean;
Begin
  Result := True;
End;

Function TWPFieldDefinitionProvider.ShouldCheckSpelling(oField: TWPDocumentField): TFieldSpellingCheckState;
Begin
  Result := fscsNone;
End;

Function TWPFieldDefinitionProvider.CheckContent(oField: TWPDocumentField; Const sContent: String; var sError : String): Boolean;
Begin
  Result := True;
End;

Procedure TWPFieldDefinitionProvider.ListSuggestions(oField: TWPDocumentField; oList: TFslStringList);
Begin

End;

function TWPFieldDefinitionProvider.UserCanDeleteField(oField: TWPDocumentField): Boolean;
begin
  Result := UserCanEditField(oField);
end;

function TWPFieldDefinitionProvider.UpdateFieldOnLoad(oField: TWPDocumentField; sContent: String; var newText: String): boolean;
begin
  result := false;
end;

function TWPFieldDefinitionProvider.UserCanDeleteField(oSection: TWPDocumentSection): Boolean;
begin
  Result := UserCanEditField(oSection);
end;

Function TWPFieldDefinitionProvider.HasCheckables(oField : TWPDocumentField) : Boolean;
Begin
  Result := False;
End;


procedure TWPFieldDefinitionProvider.GetCheckables(oField: TWPDocumentField; oList: TFslStringList);
begin

end;

function TWPFieldDefinitionProvider.HintForField(oField: TWPDocumentField; aHintMode : TFieldHintMode): String;
begin
  result := '';
end;

function TWPFieldDefinitionProvider.PreparePublicValue(oField: TWPDocumentField; sContent: String): String;
begin
  result := sContent;
end;

function TWPFieldDefinitionProvider.UserCanEditTextInField(oField: TWPDocumentField): Boolean;
begin
  result := true;
end;

function TWPFieldDefinitionProvider.UserCanEditTextInField(oSection: TWPDocumentSection): Boolean;
begin
  result := true;
end;


End.
