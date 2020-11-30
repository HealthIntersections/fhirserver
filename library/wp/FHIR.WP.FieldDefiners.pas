unit FHIR.WP.FieldDefiners;

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
interface

uses
  SysUtils, Vcl.Graphics, RegularExpressions,
  fsl_utilities, fsl_collections,
  wp_definers, FHIR.WP.Control, wp_types, wp_document, FHIR.WP.Icons, FHIR.WP.Dialogs;

type
  TWPFieldLinkedDefinitionProvider = Class (TWPFieldDefinitionProvider)
    Private
      FWordProcessor : TWordProcessor;

      {new model }
      Function CheckContentBoolean(oField : TWPDocumentField; Const sContent : String; bProgressive : Boolean) : Boolean;
      Function CheckContentList(oField : TWPDocumentField; Const sContent : String; bProgressive : Boolean) : Boolean;
      Function CheckContentNumber(oField : TWPDocumentField; Const sContent : String; bProgressive : Boolean) : Boolean;
      Function CheckContentInteger(oField : TWPDocumentField; Const sContent : String; bProgressive : Boolean) : Boolean;
      Function CheckContentDate(oField : TWPDocumentField; Const sContent : String; bProgressive : Boolean) : Boolean;
      Function CheckContentText(oField : TWPDocumentField; Const sContent : String; bProgressive : Boolean) : Boolean;
      Function CheckContent(oField : TWPDocumentField; Const sContent : String; bProgressive : Boolean) : Boolean; Overload;

    protected
      function FieldDetailsHint(oField: TWPDocumentField): String; Virtual;
    Public
      constructor Create(oWordProcessor : TWordProcessor); Overload; Virtual;
      Property WordProcessor : TWordProcessor Read FWordProcessor Write FWordProcessor;

      // The implementation of the common data patterns Mask: and List: are here so that
      // various field implementations can inherit them. The formats for the data patterns
      // are found in WPInputFieldDefinitionProviders
      Function GetBackground(oField : TWPDocumentField; bEditing : Boolean; Var aBackground : TColour) : Boolean; Overload; Override;
      Function HasUIAssistance(oField : TWPDocumentField) : Boolean; Overload; Override;
      Function CodeComplete(oField : TWPDocumentField; Const sContent : String) : Boolean; Overload; Override;
      Function CheckProposed(oField : TWPDocumentField; Const sContent : String) : Boolean; Overload; Override;
      Function ShouldCheckSpelling(oField : TWPDocumentField) : TFieldSpellingCheckState; Overload; Override;
      Function CheckContent(oField : TWPDocumentField; Const sContent : String; var sError : String) : Boolean; Overload; Override;
      Function HintForField(oField : TWPDocumentField; aHintMode : TFieldHintMode) : String; Overload; Override;
      Procedure ListSuggestions(oField : TWPDocumentField; oList : TFslStringList); Overload; Override;
      Function HasCheckables(oField : TWPDocumentField) : Boolean; Overload; Override;
      Procedure GetCheckables(oField : TWPDocumentField; oList : TFslStringList); Overload; Override;
  End;



Type
  TWPHotspotFieldDefinitionProvider = Class (TWPFieldLinkedDefinitionProvider)
    Public
      Function GetNamespace : String; Overload; Override;
      Function GetTitle : String; Overload; Override;  // The text string that identifies this definer to the user
      Function GetStdIconIndex : Integer; Overload; Override; // the icon index in the file WPIconModule.bmp. -1 means default. (ideas for a better approach welcomed)
      Function CanInsertField : Boolean; Overload; Override;
      Function NewField(oField : TWPDocumentField; bSection : Boolean; var bAcceptExistingContent : Boolean; Var oSection : TWPDocumentSection) : Boolean; Overload; Override;
      Function UserCanEditField(oField : TWPDocumentField) : Boolean; Overload; Override;
      Function EditField(oField : TWPDocumentField) : Boolean; Overload; Override;
  End;

Const
  NS_FIELD_PROPERTY = 'libraries\publishing\wordprocessor\properties';

Type
  TCommitEvent = Procedure (oField: TWPDocumentField; Const sContent: String) Of Object;

  TWPPropertyFieldDefinitionProvider = Class (TWPFieldLinkedDefinitionProvider)
    Private
      FOnCommit : TCommitEvent;
    Public
      Function GetNamespace : String; Overload; Override;
      Function GetTitle : String; Overload; Override;
      Procedure Commit(oField : TWPDocumentField; Const sContent : String); Overload; Override;
      Function GetBackground(oField : TWPDocumentField; bEditing : Boolean; Var aBackground : TColour) : Boolean; Overload; Override;

      Property OnCommit : TCommitEvent Read FOnCommit Write FOnCommit;
  End;

Const
  NS_FIELD_MODEL = 'libraries\publishing\wordprocessor\model';

Type
  TWPModelFieldDefinitionProvider = Class (TWPFieldLinkedDefinitionProvider)
    Private
      FNamespace : String;
      FStdIconIndex : Integer;
      FTouchIconIndex : Integer;
      FModel : TWPFieldModel;
      FUseEntryCodeAsFieldContent : Boolean;
      Function GetModel: TWPFieldModel;
      Procedure SetModel(Const Value: TWPFieldModel);
      Function MakeText(Const sText : String) : TWPDocumentText;
    Public
      constructor Create(oWordProcessor : TWordProcessor); Overload; Override;
      destructor Destroy; Override;

      Property Model : TWPFieldModel Read GetModel Write SetModel;
      Property Namespace : String Read FNamespace Write FNamespace;
      Property StdIconIndex : Integer Read FStdIconIndex Write FStdIconIndex;
      Property TouchIconIndex : Integer Read FTouchIconIndex Write FTouchIconIndex;
      Property UseEntryCodeAsFieldContent : Boolean Read FUseEntryCodeAsFieldContent Write FUseEntryCodeAsFieldContent;

      Function GetNamespace : String; Overload; Override;
      Function GetTitle : String; Overload; Override;  // The text string that identifies this definer to the user
      Function GetStdIconIndex : Integer; Overload; Override; // the icon index in the file WPIconModule.bmp. -1 means default. (ideas for a better approach welcomed)
      Function GetTouchIconIndex : Integer; Overload; Override; // the icon index in the file WPIconModule.bmp. -1 means default. (ideas for a better approach welcomed)

      Function CanInsertField : Boolean; Overload; Override;
      Function NewField(oField : TWPDocumentField; bSection : Boolean; var bAcceptExistingContent : Boolean; Var oSection : TWPDocumentSection) : Boolean; Overload; Override;
      Function UserCanDeleteField(oField : TWPDocumentField) : Boolean; Overload; Override;
      Function UserCanDeleteField(oSection : TWPDocumentSection) : Boolean; Overload; Override;
  End;


Const
  NS_FIELD_INPUT = 'libraries\publishing\wordprocessor\input';

Type
  TWPInputFieldDefinitionProvider = Class (TWPFieldLinkedDefinitionProvider)
    Public
      Function GetNamespace : String; Overload; Override;
      Function GetTitle : String; Overload; Override;
      Function GetStdIconIndex : Integer; Overload; Override;
      Function GetTouchIconIndex : Integer; Overload; Override;
      Function CanInsertField : Boolean; Overload; Override;
      Function NewField(oField : TWPDocumentField; bSection : Boolean; var bAcceptExistingContent : Boolean; Var oSection : TWPDocumentSection) : Boolean; Overload; Override;
      Function UserCanEditField(oField : TWPDocumentField) : Boolean; Overload; Override;
      Function UserCanEditField(oSection : TWPDocumentSection) : Boolean; Overload; Override;
      Function EditField(oField : TWPDocumentField) : Boolean; Overload; Override;
      Function EditField(oSection : TWPDocumentSection) : Boolean; Overload; Override;
      Procedure Commit(oField : TWPDocumentField; Const sContent : String); Overload; Override;
  End;

Const
  NS_VALUE_INPUT = 'libraries\publishing\wordprocessor\value';

Type
  TWPValueFieldDefinitionProvider = Class (TWPFieldDefinitionProvider)
    Public
      Function GetNamespace : String; Overload; Override;
      Function GetTitle : String; Overload; Override;
      Function GetStdIconIndex : Integer; Overload; Override;
      Function GetTouchIconIndex : Integer; Overload; Override;
      Function CanInsertField : Boolean; Overload; Override;
      Function NewField(oField : TWPDocumentField; bSection : Boolean; var bAcceptExistingContent : Boolean; Var oSection : TWPDocumentSection) : Boolean; Overload; Override;
      Function UserCanEditField(oField : TWPDocumentField) : Boolean; Overload; Override;
      Function UserCanEditField(oSection : TWPDocumentSection) : Boolean; Overload; Override;
      Function EditField(oField : TWPDocumentField) : Boolean; Overload; Override;
      Function EditField(oSection : TWPDocumentSection) : Boolean; Overload; Override;
      Procedure Commit(oField : TWPDocumentField; Const sContent : String); Overload; Override;
      Function UserCanEditTextInField(oField : TWPDocumentField) : Boolean; Overload; Override;
      Function UpdateFieldOnLoad(oField : TWPDocumentField; sContent : String; var newText : String) : boolean; Overload; Override;
  End;


implementation

{ TWPHotspotFieldDefinitionProvider }

Function TWPHotspotFieldDefinitionProvider.GetStdIconIndex: Integer;
Begin
  Result := WPIconModule.INSERT_HOTSPOT;
End;


Function TWPHotspotFieldDefinitionProvider.GetNamespace: String;
Begin
  Result := NS_FIELD_HOTSPOT;
End;


Function TWPHotspotFieldDefinitionProvider.GetTitle: String;
Begin
  Result := 'Hyperlink';
End;


Function TWPHotspotFieldDefinitionProvider.NewField(oField : TWPDocumentField; bSection : Boolean; var bAcceptExistingContent : Boolean; Var oSection : TWPDocumentSection) : Boolean;
Var
  oDialog : TWPHotspotDialog;
Begin
  oDialog := TWPHotspotDialog.Create(WordProcessor);
  Try
    oField.Hotspot := TWPHotspot.create;
    oField.Hotspot.LinkUnderline := true;
    oDialog.ReadOnly := TWordProcessor(WordProcessor).Settings.ReadOnly;
    oDialog.Field := oField.Link;
    Result := oDialog.Execute;
    oSection := Nil;
    oField.FixedFormat := fffAnyPart;
    oField.Name := 'Hyperlink';
    oField.Deletable := True;
    oField.ReadOnly := ReadOnlyFalse;
  Finally
    oDialog.Free;
  End;
  bAcceptExistingContent := true;
End;


Function TWPHotspotFieldDefinitionProvider.UserCanEditField(oField: TWPDocumentField): Boolean;
Begin
  Result := True;
End;


Function TWPHotspotFieldDefinitionProvider.EditField(oField: TWPDocumentField): Boolean;
Var
  oDialog : TWPHotspotDialog;
Begin
  oDialog := TWPHotspotDialog.Create(WordProcessor);
  Try
    oDialog.Field := oField.Link;
    Result := oDialog.Execute;
  Finally
    oDialog.Free;
  End;
End;


Function TWPHotspotFieldDefinitionProvider.CanInsertField: Boolean;
Begin
  Result := True;
End;


{ TWPPropertyFieldDefinitionProvider }

Procedure TWPPropertyFieldDefinitionProvider.Commit(oField: TWPDocumentField; Const sContent: String);
Begin
  If Assigned(FOnCommit) Then
    FOnCommit(oField, sContent);
End;

function TWPPropertyFieldDefinitionProvider.GetBackground(oField: TWPDocumentField; bEditing : Boolean; var aBackground: TColour): Boolean;
begin
  Result := True;
  aBackground := clWhite;
end;

Function TWPPropertyFieldDefinitionProvider.GetNamespace: String;
Begin
  Result := NS_FIELD_PROPERTY;
End;

Function TWPPropertyFieldDefinitionProvider.GetTitle: String;
Begin
  Result := 'Property';
End;



Constructor TWPFieldLinkedDefinitionProvider.Create(oWordProcessor: TWordProcessor);
Begin
  Create;
  FWordProcessor := oWordProcessor;
End;

Function TWPFieldLinkedDefinitionProvider.HasUIAssistance(oField: TWPDocumentField): Boolean;
Begin
  Result := oField.HasDataValue(FIELD_DATA_NAME_LIST) and (oField.DataValue[FIELD_DATA_NAME_LIST] <> '')
    or (oField.DataValue[FIELD_DATA_NAME_TYPE] = 'Boolean');
End;

Function TWPFieldLinkedDefinitionProvider.CodeComplete(oField: TWPDocumentField; Const sContent: String) : Boolean;
Var
  oItems : TWPCompletionItems;
  oList : TFslStringList;
  iLoop : Integer;
Begin
  If oField.HasDataValue(FIELD_DATA_NAME_LIST) Then
  Begin
    Result := True;
    oItems := TWPCompletionItems.Create;
    Try
      if oField.HasDataValue(FIELD_DATA_NAME_SORT) Then
      Begin
        if (oField.DataValue[FIELD_DATA_NAME_SORT] = 'Code') Then
          oItems.SortedByCode
        Else If (oField.DataValue[FIELD_DATA_NAME_SORT] = 'Desc') Then
          oItems.SortedByDescription;
      End;
      if oField.DataValue[FIELD_DATA_NAME_TYPE] = 'Boolean' Then
      Begin
        oItems.Add('True', 'True');
        oItems.Add('False', 'False');
      End
      Else
      Begin
        oList := TFslStringList.Create;
        Try
          oList.AsCSV := oField.DataValue[FIELD_DATA_NAME_LIST];
          For iLoop := 0 To oList.Count - 1 Do
            oItems.Add(oList[iLoop], oList[iLoop]);
        Finally
          oList.Free;
        End;
      End;
        WordProcessor.CodeCompletePromptList(oItems);
    Finally
      oItems.Free;
    End;
  End
  Else
    Result := False;
End;


Function TWPFieldLinkedDefinitionProvider.CheckProposed(oField: TWPDocumentField; Const sContent: String): Boolean;
Begin
  if HasCheckables(oField) Then
    result := false
  Else If oField.DataValue[FIELD_DATA_NAME_FORCE] <> 'True' Then
    Result := True
  Else
    Result := CheckContent(oField, sContent, true);
End;


Function TWPFieldLinkedDefinitionProvider.CheckContent(oField: TWPDocumentField; Const sContent: String; var sError : String): Boolean;
Begin
  Result := CheckContent(oField, sContent, false);
End;

Function TWPFieldLinkedDefinitionProvider.CheckContent(oField: TWPDocumentField; Const sContent: String; bProgressive : Boolean): Boolean;
Var
  sType : String;
Begin
  if (sContent = '') And not StringToBoolean(oField.DataValue[FIELD_DATA_NAME_MAND]) Then
    result := true
  Else if oField.HasDataValue(FIELD_DATA_NAME_TYPE) Then
  Begin
    sType := oField.DataValue[FIELD_DATA_NAME_TYPE];
    if sType = 'Boolean' Then
      result := CheckContentBoolean(oField, sContent, bProgressive)
    Else if sType = 'List' Then
      result := CheckContentList(oField, sContent, bProgressive)
    Else if sType = 'Number' Then
      result := CheckContentNumber(oField, sContent, bProgressive)
    Else if sType = 'Integer' Then
      result := CheckContentInteger(oField, sContent, bProgressive)
    Else if sType = 'Date' Then
      result := CheckContentDate(oField, sContent, bProgressive)
    Else // 'Text'
      result := CheckContentText(oField, sContent, bProgressive)
  End
  Else if oField.HasDataValue(FIELD_DATA_NAME_LIST) And (oField.DataValue[FIELD_DATA_NAME_LIST] <> '') Then
    result := CheckContentList(oField, sContent, bProgressive)
  Else
    Result := True;
End;


Function TWPFieldLinkedDefinitionProvider.GetBackground(oField: TWPDocumentField; bEditing : Boolean; Var aBackground: TColour): Boolean;
Begin
  Result := True;
  if Not bEditing then
    result := false
  else If (oField.DataValue[FIELD_DATA_NAME_LIST] <> '') and not (oField.DataValue[FIELD_DATA_NAME_LIST_MODE] = FIELD_DATA_NAME_LIST_MODE_Buttons) Then
    aBackground := DEFAULT_BACKGROUND_FIELD_VOCAB
  Else If oField.DataValue[FIELD_DATA_NAME_TEXT_REGEX] <> '' Then
    aBackground := DEFAULT_BACKGROUND_FIELD_MASK
  Else
    Result := False;
End;

Function TWPFieldLinkedDefinitionProvider.ShouldCheckSpelling(oField: TWPDocumentField): TFieldSpellingCheckState;
var
  sType : String;
Begin
  if oField.HasDataValue(FIELD_DATA_NAME_TYPE) Then
  Begin
    sType := oField.DataValue[FIELD_DATA_NAME_TYPE];
    if sType = 'Boolean' Then
      result := fscsField
    Else if sType = 'List' Then
      result := fscsField
    Else if sType = 'Number' Then
      result := fscsField
    Else if sType = 'Integer' Then
      result := fscsField
    Else if sType = 'Date' Then
      result := fscsField
    // 'Text'
    Else if oField.DataValue[FIELD_DATA_NAME_TEXT_REGEX] <> '' Then
      result := fscsField
    Else if oField.DataValue[FIELD_DATA_NAME_TEXT_MAX] <> '' Then
      result := fscsFieldSpelling
    Else
      result := fscsSpelling
  End
  Else if oField.HasDataValue(FIELD_DATA_NAME_LIST) And (oField.DataValue[FIELD_DATA_NAME_LIST] <> '') Then
    result := fscsField
  Else
    Result := fscsSpelling;
End;

Procedure TWPFieldLinkedDefinitionProvider.ListSuggestions(oField: TWPDocumentField; oList: TFslStringList);
Begin
  If oField.HasDataValue(FIELD_DATA_NAME_LIST) Then
    oList.AsCSV := oField.DataValue[FIELD_DATA_NAME_LIST];
End;

Function TWPFieldLinkedDefinitionProvider.CheckContentBoolean(oField : TWPDocumentField; Const sContent : String; bProgressive : Boolean) : Boolean;
begin
  if bProgressive Then
    result := (sContent <> '') And SameText(sContent, copy('true', 1, length(sContent))) or SameText(sContent, copy('false', 1, length(sContent)))
  Else
    result := SameText(sContent, 'true') or SameText(sContent, 'false');
End;

Function TWPFieldLinkedDefinitionProvider.CheckContentInteger(oField : TWPDocumentField; Const sContent : String; bProgressive : Boolean) : Boolean;
var
  iValue : Integer;
begin
  if StringIsInteger32(sContent) And (Trim(sContent) = sContent) Then
  Begin
    iValue := StringToInteger32(sContent);
    result :=
      ((oField.DataValue[FIELD_DATA_NAME_INT_MIN] = '') or ((length(sContent) < length(oField.DataValue[FIELD_DATA_NAME_INT_MIN])) or (StringToInteger32(oField.DataValue[FIELD_DATA_NAME_INT_MIN]) <= iValue))) And
      ((oField.DataValue[FIELD_DATA_NAME_INT_MAX] = '') or (StringToInteger32(oField.DataValue[FIELD_DATA_NAME_INT_MAX]) >= iValue));
  End
  else
    result := bProgressive and (sContent = '-') And ( (oField.DataValue[FIELD_DATA_NAME_INT_MIN] = '') or (oField.DataValue[FIELD_DATA_NAME_INT_MIN][1] = '-') );
End;

Function TWPFieldLinkedDefinitionProvider.CheckContentNumber(oField : TWPDocumentField; Const sContent : String; bProgressive : Boolean) : Boolean;
var
  s, sl, sr : String;
  i : Integer;
begin
  if (sContent = '') Or (Trim(sContent) <> sContent)  Then
    result := False
  Else
  Begin
    result := true;
    if sContent[1] = '-' Then
      s := copy(sContent, 2, $FF)
    Else
      s := sContent;
    i := pos('.', s);
    if i = 0 Then
    Begin
      sl := s;
      sr := '';
    End
    Else
    Begin
      result := bProgressive or (i <> length(s));
      sl := copy(s, 1, i-1);
      sr := copy(s, i+1, $FF);
    End;
    if StringIsInteger32(sl) Then
      result := result and ((oField.DataValue[FIELD_DATA_NAME_FLT_MAIN] = '') Or (length(sl) <= StringToInteger32(oField.DataValue[FIELD_DATA_NAME_FLT_MAIN])))
    else
      result := result and (bProgressive or (sl = ''));
    if StringIsInteger32(sr) Then
      result := result and ((oField.DataValue[FIELD_DATA_NAME_FLT_DECIMAL] = '') Or (length(sr) <= StringToInteger32(oField.DataValue[FIELD_DATA_NAME_FLT_DECIMAL])))
    else
      result := result and (sr = '');
  End;
End;


Function TWPFieldLinkedDefinitionProvider.CheckContentList(oField : TWPDocumentField; Const sContent : String; bProgressive : Boolean) : Boolean;
Var
  oList : TFslStringList;
  iLoop : Integer;
begin
  if oField.DataValue[FIELD_DATA_NAME_LIST_MODE] = FIELD_DATA_NAME_LIST_MODE_Optional Then
    result := true
  Else
  Begin
    oList := TFslStringList.Create;
    Try
      oList.AsCSV := oField.DataValue[FIELD_DATA_NAME_LIST];
      Result := false;
      if sContent <> '' Then
        For iLoop := 0 To oList.Count - 1 Do
          Result := Result Or ((bProgressive and StringStartsWithSensitive(oList[iLoop], sContent)) or
               (not bProgressive and StringEqualsSensitive(oList[iLoop], sContent)));
    Finally
      oList.Free;
    End;
  End;
End;

Function TWPFieldLinkedDefinitionProvider.CheckContentDate(oField : TWPDocumentField; Const sContent : String; bProgressive : Boolean) : Boolean;
  Function Parse(Const s : String): Integer;
  Begin
    if Trim(s) <> s Then
      result := -1
    Else
      result := StrToIntDef(s, -1);
  End;
var
  d,m,y,h,n,s : integer;
  sl, sr : String;
  cDate : Integer;
begin
  result := (sContent <> '');
  if oField.DataValue[FIELD_DATA_NAME_DATE_TIME] = FIELD_DATA_NAME_DATE_TIME_Prohibited Then
    cDate := 0
  Else if oField.DataValue[FIELD_DATA_NAME_DATE_TIME] = FIELD_DATA_NAME_DATE_TIME_Allowed Then
    cDate := 1
  Else // 'Required'
    cDate := 2;

  // day
  StringSplit(sContent, '/', sl, sr);
  d := Parse(sl);
  result := result and (d >= 1) And (d <= 31) and (bProgressive or (sr <> ''));

  // month
  if not bProgressive or (sr <> '') Then
  Begin
    StringSplit(sr, '/', sl, sr);
    m := Parse(sl);
    result := result And (m >= 1) And (m <= 12) And (bProgressive or (sr <> ''));
  End;

  // year
  if cDate > 0 Then
    StringSplit(sr, ' ', sl, sr)
  Else
    sl := sr;
  if not bProgressive or (sl <> '') Then
  Begin
    y := Parse(sl);
    result := result And ((bProgressive And (y > 0) and (length(sl) < 4)) or ((length(sl) = 4) and (y >= 1901) And (y <= 2050)));
  End;

  if (cDate > 0) And (not bProgressive or (sr <> '')) Then
  Begin
    // hour
    StringSplit(sr, ':', sl, sr);
    h := Parse(sl);
    result := result and (h >= 0) And (h <= 23) and (bProgressive or (cDate = 1) or ((sr <> '') and (length(sl) = 2)));

    // minute
    StringSplit(sr, ':', sl, sr);
    n := Parse(sl);
    result := result and (bProgressive or (length(sl) = 2)) And (bProgressive or ((n >= 0) And (n <= 59)) or (cDate = 1));

    if sr <> '' Then
    Begin
      s := Parse(sr);
      result := result and ((s >= 0) And (s <= 59) And (length(sr) = 2));
    End;
  End;
End;

Function TWPFieldLinkedDefinitionProvider.CheckContentText(oField : TWPDocumentField; Const sContent : String; bProgressive : Boolean) : Boolean;
Var
  oRegExpr : TRegEx;
begin
  result := sContent <> '';
  if oField.DataValue[FIELD_DATA_NAME_TEXT_REGEX] <> '' Then
  Begin
    oRegExpr := TRegEx.Create('^'+oField.DataValue[FIELD_DATA_NAME_TEXT_REGEX]+'$');
    if not oRegExpr.IsMatch(sContent) then
      result := false;
  End;

  if oField.DataValue[FIELD_DATA_NAME_TEXT_MAX] <> '' Then
  Begin
    If (Length(sContent) > StringToInteger32(oField.DataValue[FIELD_DATA_NAME_TEXT_MAX])) then
      result := false;
  End;
End;

procedure TWPFieldLinkedDefinitionProvider.GetCheckables(oField: TWPDocumentField; oList: TFslStringList);
begin
  if oField.DataValue[FIELD_DATA_NAME_TYPE] = 'Boolean' Then
  Begin
    oList.Add('Yes');
    oList.Add('No');
  End
  Else
    oList.AsCSV := oField.DataValue[FIELD_DATA_NAME_LIST]
end;

function TWPFieldLinkedDefinitionProvider.HasCheckables(oField: TWPDocumentField): Boolean;
begin
  result := (oField.DataValue[FIELD_DATA_NAME_TYPE] = 'Boolean') Or
     ((oField.DataValue[FIELD_DATA_NAME_TYPE] = 'List') and (oField.DataValue[FIELD_DATA_NAME_LIST_MODE] = FIELD_DATA_NAME_LIST_MODE_Buttons));
end;

function TWPFieldLinkedDefinitionProvider.FieldDetailsHint(oField: TWPDocumentField): String;
Var
  sType : String;
Begin
  if oField.HasDataValue(FIELD_DATA_NAME_TYPE) Then
  Begin
    sType := oField.DataValue[FIELD_DATA_NAME_TYPE];
    if sType = 'Boolean' Then
      result := '"true" or "false"'
    Else if sType = 'List' Then
      if oField.DataValue[FIELD_DATA_NAME_LIST_MODE] <> FIELD_DATA_NAME_LIST_MODE_Dropdown Then
        result := ''
      Else
        result := 'An item from the list '+ oField.DataValue[FIELD_DATA_NAME_LIST]
    Else if sType = 'Number' Then
    Begin
      result := 'Number';
      if oField.DataValue[FIELD_DATA_NAME_FLT_MAIN] <> '' Then
        result := result+', '+oField.DataValue[FIELD_DATA_NAME_FLT_MAIN]+' digits';
      if oField.DataValue[FIELD_DATA_NAME_FLT_DECIMAL] <> '' Then
        result := result+', '+oField.DataValue[FIELD_DATA_NAME_FLT_DECIMAL]+' decimals'
    End
    Else if sType = 'Integer' Then
    Begin
      result := 'Whole Number';
      if oField.DataValue[FIELD_DATA_NAME_INT_MIN] <> '' Then
        result := result+', >='+oField.DataValue[FIELD_DATA_NAME_INT_MIN];
      if oField.DataValue[FIELD_DATA_NAME_INT_MAX] <> '' Then
        result := result+', <= '+oField.DataValue[FIELD_DATA_NAME_INT_MAX];
    End
    Else if sType = 'Date' Then
    Begin
      if oField.DataValue[FIELD_DATA_NAME_DATE_TIME] = FIELD_DATA_NAME_DATE_TIME_Prohibited Then
        result := 'dd/mm/yyyy'
      Else if oField.DataValue[FIELD_DATA_NAME_DATE_TIME] = FIELD_DATA_NAME_DATE_TIME_Allowed Then
        result := 'dd/mm/yyyy (hh:nn)(:ss)'
      Else // 'Required'
        result := 'dd/mm/yyyy hh:nn(:ss)';
    End
    Else // 'Text'
    Begin
      If oField.DataValue[FIELD_DATA_NAME_TEXT_DESC] <> '' Then
        result := oField.DataValue[FIELD_DATA_NAME_TEXT_DESC]
      Else if oField.DataValue[FIELD_DATA_NAME_TEXT_MAX] <> '' Then
        result := '<'+oField.DataValue[FIELD_DATA_NAME_TEXT_MAX]+' chars'
      Else
        result := '(any text)';
    End;
  End
  Else if oField.HasDataValue(FIELD_DATA_NAME_LIST) And (oField.DataValue[FIELD_DATA_NAME_LIST] <> '') Then
    if oField.DataValue[FIELD_DATA_NAME_LIST_MODE] <> FIELD_DATA_NAME_LIST_MODE_Dropdown Then
      result := ''
    Else
      result := 'An item from the list '+ oField.DataValue[FIELD_DATA_NAME_LIST]
  Else
    Result := '';
End;

function TWPFieldLinkedDefinitionProvider.HintForField(oField: TWPDocumentField; aHintMode : TFieldHintMode): String;
begin
  if aHintMode = fhmFull then
    result := oField.Name+' ('+FieldDetailsHint(oField)+')'
  else
    result := FieldDetailsHint(oField);
end;


{ TWPModelFieldDefinitionProvider }

Constructor TWPModelFieldDefinitionProvider.Create(oWordProcessor : TWordProcessor);
Begin
  Inherited;
  FModel := TWPFieldModel.Create;
  FStdIconIndex := -1;
  FTouchIconIndex := -1;
  FUseEntryCodeAsFieldContent := False;
End;


Destructor TWPModelFieldDefinitionProvider.Destroy;
Begin
  FModel.Free;
  Inherited;
End;


Function TWPModelFieldDefinitionProvider.GetModel: TWPFieldModel;
Begin
  Assert(Invariants('GetModel', FModel, TWPFieldModel, 'FModel'));

  Result := FModel;
End;


Procedure TWPModelFieldDefinitionProvider.SetModel(Const Value: TWPFieldModel);
Begin
  Assert(Not Assigned(Value) Or Invariants('SetModel', Value, TWPFieldModel, 'Value'));

  FModel.Free;
  FModel := Value;
End;



Function TWPModelFieldDefinitionProvider.GetStdIconIndex: Integer;
Begin
  Result := FStdIconIndex;
End;


Function TWPModelFieldDefinitionProvider.GetTouchIconIndex: Integer;
Begin
  Result := FTouchIconIndex;
End;


Function TWPModelFieldDefinitionProvider.GetNamespace: String;
Begin
  Result := FNamespace;
End;


Function TWPModelFieldDefinitionProvider.GetTitle: String;
Begin
  If Assigned(FModel) Then
    Result := FModel.Title
  Else
    Result := '';
End;


Function TWPModelFieldDefinitionProvider.NewField(oField : TWPDocumentField; bSection : Boolean; var bAcceptExistingContent : Boolean; Var oSection : TWPDocumentSection) : Boolean;
Var
  oDialog : TWPFieldModelDialog;
  oPara : TWPDocumentParagraph;
Begin
  oDialog := TWPFieldModelDialog.Create(WordProcessor);
  Try
    oDialog.Section := bSection;
    oDialog.Model := Model.Link;
    Result := oDialog.Execute;
    If Result Then
    Begin
      If oDialog.Selected.Section Then
      Begin
        oSection := TWPDocumentSection.Create;
        Try
          oSection.Name := oDialog.Selected.Code;
          oSection.ReadOnly := ReadOnlyDefault;
          oPara := TWPDocumentParagraph.Create;
          Try
            oPara.Contents.Add(MakeText(oDialog.Selected.Description));
            oSection.Blocks.Add(oPara.Link);
          Finally
            oPara.Free;
          End;

          oSection.Link;
        Finally
          oSection.Free;
        End;
      End
      Else
      Begin
        oSection := Nil;
        oField.Name := oDialog.Selected.Code;
        oField.FixedFormat := fffWholeField;
        oField.Deletable := True;
        oField.ReadOnly := ReadOnlyDefault;
        If FUseEntryCodeAsFieldContent Then
        Begin
          Assert(CheckCondition(oDialog.Selected.Code <> '', 'NewField', 'Entries have codes defined if using USeEntryCodeAsFieldContent.'));

          oField.Contents.Add(MakeText(oDialog.Selected.Code));
        End
        Else
        Begin
          If oDialog.Selected.Description <> '' Then
            oField.Contents.Add(MakeText(oDialog.Selected.Description))
          Else If oDialog.Selected.Name <> '' Then
            oField.Contents.Add(MakeText(oDialog.Selected.Name));
        End;
      End;
    End;
  Finally
    oDialog.Free;
  End;
End;


Function TWPModelFieldDefinitionProvider.CanInsertField: Boolean;
Begin
  Result := True;
End;

Function TWPModelFieldDefinitionProvider.MakeText(Const sText: String): TWPDocumentText;
Begin

  Result := TWPDocumentText.Create;
  Try
    Result.Value := sText;
    Result.Link;
  Finally
    Result.Free;
  End;
End;


function TWPModelFieldDefinitionProvider.UserCanDeleteField(oField: TWPDocumentField): Boolean;
begin
  Result := True;
end;

function TWPModelFieldDefinitionProvider.UserCanDeleteField(oSection: TWPDocumentSection): Boolean;
begin
  Result := True;
end;



{ TWPInputFieldDefinitionProvider }

Function TWPInputFieldDefinitionProvider.CanInsertField: Boolean;
Begin
  Result := True;
End;


Procedure TWPInputFieldDefinitionProvider.Commit(oField: TWPDocumentField; Const sContent: String);
Begin
  // for now, nothing
End;

Function TWPInputFieldDefinitionProvider.EditField(oField: TWPDocumentField): Boolean;
Var
  oDialog : TWPInputFieldDialog;
Begin
  oDialog := TWPInputFieldDialog.Create(WordProcessor);
  Try
    oDialog.ReadOnly := WordProcessor.Settings.ReadOnly;
    oDialog.Field := oField.Link;
    oDialog.FieldOnly;
    Result := oDialog.Execute;
  Finally
    oDialog.Free;
  End;
End;

Function TWPInputFieldDefinitionProvider.EditField(oSection: TWPDocumentSection): Boolean;
Var
  oDialog : TWPInputFieldDialog;
Begin
  oDialog := TWPInputFieldDialog.Create(WordProcessor);
  Try
    oDialog.ReadOnly := WordProcessor.Settings.ReadOnly;
    oDialog.Section := oSection.Link;
    oDialog.SectionOnly;
    Result := oDialog.Execute;
  Finally
    oDialog.Free;
  End;
End;

Function TWPInputFieldDefinitionProvider.GetStdIconIndex: Integer;
Begin
  Result := WPIconModule.INSERT_FIELD;
End;

Function TWPInputFieldDefinitionProvider.GetTouchIconIndex: Integer;
Begin
  Result := wp_types.TOUCH_ICON_FIELD;
End;


Function TWPInputFieldDefinitionProvider.GetNamespace: String;
Begin
  Result := NS_FIELD_INPUT;
End;


Function TWPInputFieldDefinitionProvider.NewField(oField: TWPDocumentField; bSection: Boolean; var bAcceptExistingContent : Boolean; Var oSection: TWPDocumentSection): Boolean;
Var
  oDialog : TWPInputFieldDialog;
Begin
  oDialog := TWPInputFieldDialog.Create(WordProcessor);
  Try
    oDialog.Field := oField.Link;
    oDialog.ReadOnly := False;
    If Not bSection Then
      oDialog.FieldOnly
    Else
      oDialog.FieldOrSection;
    Result := oDialog.Execute;
    If Result Then
      If (oDialog.IsSection) Then
        oSection := oDialog.Section.Link;
  Finally
    oDialog.Free;
  End;
End;

Function TWPInputFieldDefinitionProvider.GetTitle: String;
Begin
  Result := 'Input';
End;

Function TWPInputFieldDefinitionProvider.UserCanEditField(oField: TWPDocumentField): Boolean;
Begin
  Result := True;
End;

Function TWPInputFieldDefinitionProvider.UserCanEditField(oSection: TWPDocumentSection): Boolean;
Begin
  Result := True;
End;

Function TWPValueFieldDefinitionProvider.GetNamespace : String;
begin
  result := NS_VALUE_INPUT;
end;

Function TWPValueFieldDefinitionProvider.GetTitle : String;
begin
  result := 'Value';
end;

Function TWPValueFieldDefinitionProvider.GetStdIconIndex : Integer;
begin
  result := -1;
end;

Function TWPValueFieldDefinitionProvider.GetTouchIconIndex : Integer;
begin
  result := -1;
end;

Function TWPValueFieldDefinitionProvider.CanInsertField : Boolean;
begin
  result := true;
end;

Function TWPValueFieldDefinitionProvider.NewField(oField : TWPDocumentField; bSection : Boolean; var bAcceptExistingContent : Boolean; Var oSection : TWPDocumentSection) : Boolean;
begin
  result := true;
  bAcceptExistingContent := false;
  oField.Name := 'CurrentDate';
  oField.Deletable := true;
  oField.FixedFormat := fffWholeField;
  oField.ReadOnly := ReadOnlyDefault;
  oField.Contents.Add(TWPDocumentText.create('current-date-time'));
end;

Function TWPValueFieldDefinitionProvider.UserCanEditField(oField : TWPDocumentField) : Boolean;
begin
  result := false;
end;

function TWPValueFieldDefinitionProvider.UpdateFieldOnLoad(oField: TWPDocumentField; sContent: String; var newText: String): boolean;
begin
  result := true;
  newText := FormatDateTime('c', now);
end;

Function TWPValueFieldDefinitionProvider.UserCanEditField(oSection : TWPDocumentSection) : Boolean;
begin
  result := false;
end;

Function TWPValueFieldDefinitionProvider.EditField(oField : TWPDocumentField) : Boolean;
begin
  result := false;
end;

Function TWPValueFieldDefinitionProvider.EditField(oSection : TWPDocumentSection) : Boolean;
begin
  result := false;
end;

Procedure TWPValueFieldDefinitionProvider.Commit(oField : TWPDocumentField; Const sContent : String);
begin
// nothing
end;


function TWPValueFieldDefinitionProvider.UserCanEditTextInField(oField: TWPDocumentField): Boolean;
begin
  result := false;
end;

end.
