Unit FHIR.WP.Addict;

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
  SysUtils, Classes, Controls,
  Ad3SpellBase, ad3Spell, ad3ParserBase, ad3CustomDictionary, ad3Configuration, ad3ConfigurationDialogCtrl,
  fsl_base, fsl_utilities, fsl_collections,
  FHIR.WP.Spelling, wp_working, FHIR.WP.Control;

Type
  TLoadCustomDictionaryEvent = Procedure (oWords : TFslStringList; Var bLoaded : Boolean) Of Object;
  TAddWordToCustomDictionaryEvent = Procedure (Const sWord : String) Of Object;

  TWPCustomAddictDictionary = Class (TCustomDictionary)
    Private
      FOnLoadDictionary : TLoadCustomDictionaryEvent;
      FOnAddWord : TAddWordToCustomDictionaryEvent;

    Protected
      Function Load : Boolean; Override;

    function sizeInBytesV : cardinal; override;
    Public
      Function AddIgnore(Const sWord : String) : Boolean; Override;

      Procedure Clear; Overload; Virtual;

      Property OnLoadDictionary : TLoadCustomDictionaryEvent Read FOnLoadDictionary Write FOnLoadDictionary;
      Property OnAddWord : TAddWordToCustomDictionaryEvent Read FOnAddWord Write FOnAddWord;
  End;


  TWPAddictSpell3 = Class(TAddictSpell3)
    Protected
      Function CreateCustomDictionary : TCustomDictionary; Override;
  End;

  TWPAddictDictionary = Class (TFslObject)
    Private
      FAddict : TAddictSpell3;

      FFolder : String;
      FOnLoadDictionary : TLoadCustomDictionaryEvent;
      FOnAddWord : TAddWordToCustomDictionaryEvent;
      FOnAllowWord : TAllowWordEvent;
      FOnCheckWord: TWordCheckEvent;

      Procedure DoLoadDictionary(oWords : TFslStringList; Var bLoaded : Boolean);
      Procedure DoAddWord(Const sWord : String);
      Procedure SetFolder(Const sValue : String);
      Procedure SetDictionaries;
      procedure SetOnCheckWord(const Value: TWordCheckEvent);
      procedure SetOnAllowWord(const Value: TAllowWordEvent);

      Procedure AdWordCheck(Sender:TObject; Const Word:String; Var CheckType : Ad3SpellBase.TWordCheckType; Var Replacement:String);
      Procedure AdAllowWord(Sender:TObject; Const Word:String);
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create(bCheckUppercase : Boolean); Overload;
      constructor Create(Const sFolder : String; bCheckUppercase : Boolean); Overload; Virtual;
      destructor Destroy; Override;

      Function Link : TWPAddictDictionary; Overload;

      Function WordExcluded(Const sWord : String) : Boolean; Overload; Virtual;
      Function WordExists(Const sWord : String) : Boolean; Overload; Virtual;
      Procedure Suggest(Const sWord:String; oWords:TStrings);  Overload; Virtual;
      Function AddIgnore(Const sWord : String) : Boolean; Overload; Virtual;
      Function AddExclude(Const sWord : String) : Boolean; Overload; Virtual;
      Procedure CheckParser( oParser:TControlParser; aCheckType:TCheckType ); Overload; Virtual;

      Procedure Clear; Overload; Virtual;

      Property Folder : String Read FFolder Write SetFolder;

      Property OnLoadDictionary : TLoadCustomDictionaryEvent Read FOnLoadDictionary Write FOnLoadDictionary;
      Property OnAddWord : TAddWordToCustomDictionaryEvent Read FOnAddWord Write FOnAddWord;
      Property OnAllowWord : TAllowWordEvent Read FOnAllowWord Write SetOnAllowWord;
      Property OnCheckWord : TWordCheckEvent read FOnCheckWord write SetOnCheckWord;
  End;

Type
  TWPWorkingDocumentSpellCheckingState = wp_working.TWPWorkingDocumentSpellCheckingState;

  TWPAddictSpeller = Class (TWPSpeller)
  Private
    FDictionary : TWPAddictDictionary;
    Procedure SetDictionary(Value : TWPAddictDictionary);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(oDictionary : TWPAddictDictionary); Overload; Virtual;
    destructor Destroy; Override;

    Function CheckSpelling(Const sWord : String) : TWPWorkingDocumentSpellCheckingState; Override;
    Procedure ListSuggestions(Const sWord : String; oList : TFslStringList); Override;
    Function AddWord(Const sWord : String) : Boolean; Override;
    Function IgnoreWord(Const sWord : String) : Boolean; Override;
    Procedure Check(oWP : TObject; aIgnoreCheck : TWordCheckEvent; aAllowWord : TAllowWordEvent); Override;

    Property Dictionary : TWPAddictDictionary Read FDictionary Write SetDictionary;
  End;


Type
  TWPAddictParser = Class(TControlParser2)
    Private
      FSpeller : TWPAddictSpeller;
      FWordProcessor : TWordProcessor;
      FPosition : Integer;
      FEndPosition : Integer;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      Procedure Connect(oSpell : TWPAddictSpeller; oWP : TObject);
      Procedure Initialize(aEditControl : Pointer); Override;
      Function GetChar : Char; Override;
      Function GetLine : String; Override;
      Function MoveNext : Boolean; Override;
      Function MovePrevious : Boolean; Override;
      Procedure SetPosition(iCol:LongInt; iRow:LongInt; aPosType:TPositionType ); Override;
      Procedure GetPosition(Var iCol : LongInt; Var iRow : LongInt); Override;
      Procedure SelectWord(iLength : LongInt); Override;
      Procedure ReplaceWord(sReplacement : String; iState : LongInt); Override;
      Procedure IgnoreWord(iState : LongInt); Override;
      Procedure CenterSelection; Override;
      Procedure GetCursorPosition(Var iCol : LongInt; Var iRow : LongInt); Override;
      Procedure GetSelectionPosition(Var iColStart : LongInt; Var iRowStart : LongInt; Var iColEnd : LongInt; Var iRowEnd : LongInt); Override;
      Procedure GetControlScreenPosition(Var aControlPosition : TRect); Override;
      Procedure GetSelectionScreenPosition(Var aSelectionPosition : TRect); Override;
      Procedure UndoLast(iUndoState : LongInt; iUndoAction : LongInt; Var iUndoData : LongInt); Override;
      Function GetControl:TComponent; Override;
  End;


Implementation

Uses
  IOUtils,
  fui_vclx_Base,
  wp_types;

{ TWPAddictSpeller }


Constructor TWPAddictSpeller.Create(oDictionary : TWPAddictDictionary);
Begin
  Create;
  Dictionary := oDictionary;
End;


Destructor TWPAddictSpeller.Destroy;
Begin
  FDictionary.Free;
  Inherited;
End;


Function TWPAddictSpeller.CheckSpelling(Const sWord: String): TWPWorkingDocumentSpellCheckingState;
Begin
  If (IsWordBreak(sWord)) Or AllowedWords.ExistsByValue(lowercase(sWord)) Then
    Result := scsExempt
  Else If FDictionary.WordExists(sWord) Or FDictionary.WordExists(LowerCase(sWord)) Or ((StringUpper(sWord) = sWord) And Not Settings.SpellCheckUppercaseWords) Then
    Result := scsOK
  Else
    Result := scsWrong;
End;


Procedure TWPAddictSpeller.ListSuggestions(Const sWord: String; oList: TFslStringList);
Var
  oWords : TStringList;
  iLoop : Integer;
Begin
  oWords := TStringList.Create;
  Try
    FDictionary.Suggest(sWord, oWords);
    For iLoop := 0 To oWords.Count - 1 Do
      oList.Add(oWords[iLoop]);
  Finally
    oWords.Free;
  End;
End;


Function TWPAddictSpeller.AddWord(Const sWord: String): Boolean;
Begin
  Result := FDictionary.AddIgnore(StringReplace(sWord, CHAR_SPELL_EXCLUDE, ''));
End;


Function TWPAddictSpeller.IgnoreWord(Const sWord: String): Boolean;
Begin
  Result := FDictionary.AddExclude(StringReplace(sWord, CHAR_SPELL_EXCLUDE, ''));
  AllowedWords.Add(lowercase(StringReplace(sWord, CHAR_SPELL_EXCLUDE, '')));
End;


Procedure TWPAddictSpeller.Check(oWP : TObject; aIgnoreCheck : TWordCheckEvent; aAllowWord : TAllowWordEvent);
Var
  oParser : TWPAddictParser;
Begin
  oParser := TWPAddictParser.Create;
  oParser.Connect(Self, oWP);
  FDictionary.OnCheckWord := aIgnoreCheck;
  FDictionary.OnAllowWord := aAllowWord;
  FDictionary.CheckParser(oParser, ctAll);
End;


Procedure TWPAddictSpeller.SetDictionary(Value: TWPAddictDictionary);
Begin
  FDictionary.Free;
  FDictionary := Value;
End;


function TWPAddictSpeller.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDictionary.sizeInBytes);
end;

{ TWPAddictSpell3 }


Function TWPAddictSpell3.CreateCustomDictionary: TCustomDictionary;
Begin
  Result := TWPCustomAddictDictionary.Create;
End;


{ TWPAddictDictionary }


Constructor TWPAddictDictionary.Create(bCheckUppercase : Boolean);
Begin
  Inherited Create;

  FAddict := TWPAddictSpell3.Create(Nil);
  FAddict.ConfigStorage := csNone;
  if bCheckUppercase Then
    FAddict.ConfigDefaultOptions := [soNumbers, soInternet, soQuoted, soAbbreviations, soRepeated, soDUalCaps]
  else
    FAddict.ConfigDefaultOptions := [soUpcase, soNumbers, soInternet, soQuoted, soAbbreviations, soRepeated, soDUalCaps];
  FAddict.CommandsVisible := FAddict.CommandsVisible - [sdcOptions];
  TWPCustomAddictDictionary(FAddict.InternalCustom).OnLoadDictionary := DoLoadDictionary;
  TWPCustomAddictDictionary(FAddict.InternalCustom).OnAddWord := DoAddWord;
  FAddict.OnWordCheck := AdWordCheck;
  FAddict.OnAllowWord := AdAllowWord;
  FFolder := PathFolder(ParamStr(0));
  SetDictionaries;
End;


Constructor TWPAddictDictionary.Create(Const sFolder : String; bCheckUppercase : Boolean);
Begin
  Create(bCheckUppercase);
  Folder := sFolder;
End;


Destructor TWPAddictDictionary.Destroy;
Begin
  FAddict.Free;
  Inherited;
End;


Function TWPAddictDictionary.Link : TWPAddictDictionary;
Begin
  Result := TWPAddictDictionary(Inherited Link);
End;



Procedure TWPAddictDictionary.SetFolder(Const sValue: String);
Begin
  FFolder := sValue;
  SetDictionaries;
End;


Procedure TWPAddictDictionary.SetDictionaries;
Var
  s : String;
Begin
  FAddict.ConfigDefaultMain.Clear;
  for s in TDirectory.GetFiles(FFolder) do
    if s.EndsWith('adm') then
      FAddict.ConfigDefaultMain.Add(s);
End;


Procedure TWPAddictDictionary.DoLoadDictionary(oWords : TFslStringList; Var bLoaded : Boolean);
Begin
  If Assigned(OnLoadDictionary) Then
    OnLoadDictionary(oWords, bLoaded);
End;


Procedure TWPAddictDictionary.DoAddWord(Const sWord : String);
Begin
  If Assigned(OnAddWord) Then
    OnAddWord(sWord);
End;


Function TWPAddictDictionary.WordExcluded(Const sWord : String) : Boolean;
Begin
  Result := FAddict.WordExcluded(sWord);
End;


Function TWPAddictDictionary.WordExists(Const sWord : String) : Boolean;
Begin
  Result := FAddict.WordExists(sWord);
End;


Procedure TWPAddictDictionary.Suggest(Const sWord:String; oWords:TStrings);
Begin
  FAddict.Suggest(sWord, oWords);
End;


Function TWPAddictDictionary.AddIgnore(Const sWord : String) : Boolean;
Begin
  Result := FAddict.InternalCustom.AddIgnore(StringReplace(sWord, CHAR_SPELL_EXCLUDE, ''));
End;


procedure TWPAddictDictionary.AdAllowWord(Sender: TObject; const Word: String);
begin
  if assigned(FOnAllowWord) then
    FOnAllowWord(self, word);
end;

procedure TWPAddictDictionary.AdWordCheck(Sender: TObject; const Word: String; var CheckType: Ad3SpellBase.TWordCheckType; var Replacement: String);
begin
  if assigned(FOnCheckWord) then
    FOnCheckWord(self, word, FHIR.WP.Spelling.TWordCheckType(CheckType), Replacement);
end;

Function TWPAddictDictionary.AddExclude(Const sWord : String) : Boolean;
Begin
  Result := FAddict.InternalCustom.AddExclude(StringReplace(sWord, CHAR_SPELL_EXCLUDE, ''));
End;


Procedure TWPAddictDictionary.CheckParser( oParser:TControlParser; aCheckType:TCheckType );
Begin
  FAddict.CheckParser(oParser, aCheckType);
End;

Procedure TWPAddictDictionary.Clear;
Begin
  FAddict.UnloadDictionaries;
End;

procedure TWPAddictDictionary.SetOnCheckWord(const Value: TWordCheckEvent);
begin
  FOnCheckWord := Value;
end;

procedure TWPAddictDictionary.SetOnAllowWord(const Value: TAllowWordEvent);
begin
  FOnAllowWord := Value;
end;

function TWPAddictDictionary.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FAddict.sizeInBytes);
  inc(result, (FFolder.length * sizeof(char)) + 12);
  inc(result, FOnLoadDictionary.sizeInBytes);
  inc(result, FOnAddWord.sizeInBytes);
  inc(result, FOnAllowWord.sizeInBytes);
  inc(result, FOnCheckWord.sizeInBytes);
end;

Procedure TWPCustomAddictDictionary.Clear;
Begin
  Loaded := False;
End;


Function TWPCustomAddictDictionary.Load;
Var
  oWords : TFslStringList;
  iLoop : Integer;
Begin
  Result := False;

  oWords := TFslStringList.Create;
  Try
    If Assigned(FOnLoadDictionary) Then
      FOnLoadDictionary(oWords, Result);

    If Result Then
    Begin
      For iLoop := 0 To oWords.Count - 1 Do
        Inherited AddIgnore(oWords[iLoop]);
    End;
  Finally
    oWords.Free;
  End;
End;



Function TWPCustomAddictDictionary.AddIgnore(Const sWord : String) : Boolean;
Begin
  Result := True;

  If Assigned(FOnAddWord) Then
  Begin
    Try
      FOnAddWord(sWord);
    Except
      On E : EWordIsBanned Do
      Begin
        Result := False;

        // TODO: showing a dialog in a non visual class seems like a bad idea.
        DialogError('The word "' + sWord + '" is not allowed to be added to the dictionary.');
      End
      Else
      Begin
        Raise;
      End;
    End;
  End;

  If Result Then
    Result := Inherited AddIgnore(sWord);
End;


function TWPCustomAddictDictionary.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FOnLoadDictionary.sizeInBytes);
  inc(result, FOnAddWord.sizeInBytes);
end;

Procedure TWPAddictParser.Connect(oSpell : TWPAddictSpeller; oWP : TObject);
Begin
  FSpeller := oSpell;
  FWordProcessor := TWordProcessor(oWP);
End;


Procedure TWPAddictParser.Initialize(aEditControl : Pointer);
Begin
  FPosition := 0;
  FEndPosition := FWordProcessor.Document.CharCount;
  FWordProcessor.PrimaryRange.GoHome(False);
End;


Function TWPAddictParser.GetChar : Char;
Var
  oPiece : TWPWorkingDocumentPiece;
  iOffset : Integer;
  iIndex : Integer;
Begin
  If FWordProcessor.Document.GetPieceByPosition(FPosition, oPiece, iOffset, iIndex) And (oPiece.PieceType = ptText) Then
    Result := TWPWorkingDocumentTextPiece(oPiece).Content[iOffset+1]
  Else
    Result := #0;
End;


Function TWPAddictParser.GetLine : String;
Var
  oPiece : TWPWorkingDocumentPiece;
  iOffset : Integer;
  iIndex : Integer;
  oRow : TWPMapRow;
Begin
  If FWordProcessor.Document.GetPieceByPosition(FPosition, oPiece, iOffset, iIndex) And (oPiece.PieceType = ptText) Then
  Begin
    oRow := TWPMapRow(oPiece.Maps.GetByOffset(iOffset).Parent);
    Assert(oRow.Invariants('GetLine', TWPMapRow)); // it's possibls that the above will get the wrong thing if the position is wrong, but it shouldn't be
    Result := oRow.GetText;
  End
  Else
    Result := '';
End;


Function TWPAddictParser.MoveNext : Boolean;
Begin
  Result := FWordProcessor.Document.NextRight(FPosition, FWordProcessor.Settings.NoSelectReadOnly, FPosition);
  If (FEndPosition > 0) And (FPosition > FEndPosition) Then
    Result := False;
End;


Function TWPAddictParser.MovePrevious : Boolean;
Begin
  Result := FWordProcessor.Document.NextLeft(FPosition, FWordProcessor.Settings.NoSelectReadOnly, FPosition);
End;


Procedure TWPAddictParser.GetPosition(Var iCol : LongInt; Var iRow : LongInt);
Begin
  If Not FWordProcessor.Renderer.GetLineCol(FPosition, iRow, iCol) Then
  Begin
    iRow := 0;
    iCol := 0;
  End;
End;


Procedure TWPAddictParser.SetPosition( iCol:LongInt; iRow:LongInt; aPosType:TPositionType );
Var
  iPosition : Integer;
Begin
  If FWordProcessor.Renderer.GetByLineCol(iRow, iCol, iPosition) Then
    If aPosType = ptCurrent Then
      FPosition := iPosition
    Else
      FEndPosition := iPosition;
End;


Procedure TWPAddictParser.SelectWord(iLength : LongInt);
Begin
  FWordProcessor.PrimaryRange.MoveTo(FPosition);
  FPosition := FPosition - iLength;
  FWordProcessor.PrimaryRange.SelectTo(FPosition);
End;


Procedure TWPAddictParser.ReplaceWord(sReplacement : String; iState : LongInt);
Begin
  FPosition := FWordProcessor.Selection.SelStart + Length(sReplacement);
  FWordProcessor.PrimaryRange.Insert(sReplacement);
End;


Procedure TWPAddictParser.IgnoreWord(iState : LongInt);
Var
  sWord : String;
Begin
  If (iState = IgnoreState_Add) And FWordProcessor.PrimaryRange.CursorInWord(sWord) Then
  Begin
    FSpeller.AddWord(sWord);
    FWordProcessor.PrimaryRange.CheckSpelling(sWord);
  End;

  If (iState = IgnoreState_IgnoreAll) And FWordProcessor.PrimaryRange.CursorInWord(sWord) Then
  Begin
    FSpeller.IgnoreWord(sWord);
    FWordProcessor.PrimaryRange.CheckSpelling(sWord);
  End;
  FPosition := FWordProcessor.Selection.SelEnd + 1;
End;


Procedure TWPAddictParser.CenterSelection;
Begin
  FWordProcessor.CheckOffsetVisible(FWordProcessor.Selection.SelActive);
End;


Procedure TWPAddictParser.GetCursorPosition(Var iCol:LongInt; Var iRow : LongInt);
Begin
  If Not FWordProcessor.Renderer.GetLineCol(FWordProcessor.Selection.Cursor, iRow, iCol) Then
  Begin
    iRow := 0;
    iCol := 0;
  End;
End;


Procedure TWPAddictParser.GetSelectionPosition(Var iColStart : LongInt; Var iRowStart : LongInt; Var iColEnd : LongInt; Var iRowEnd : LongInt);
Var
  iStart : Integer;
  iEnd : Integer;
Begin
  If FWordProcessor.Selection.HasSelection Then
  Begin
    iStart := FWordProcessor.Selection.SelStart;
    iEnd := FWordProcessor.Selection.SelEnd;
  End
  Else
  Begin
    iStart := FWordProcessor.Selection.Cursor;
    iEnd := FWordProcessor.Selection.Cursor;
  End;
  If Not FWordProcessor.Renderer.GetLineCol(iStart, iRowStart, iColStart) Then
  Begin
    iRowStart := 0;
    iColStart := 0;
  End;
  If Not FWordProcessor.Renderer.GetLineCol(iEnd, iRowEnd, iColEnd) Then
  Begin
    iRowEnd := 0;
    iColEnd := 0;
  End;
End;


Procedure TWPAddictParser.GetControlScreenPosition(Var aControlPosition : TRect);
Begin
  aControlPosition.Left := FWordProcessor.Left;
  aControlPosition.Top := FWordProcessor.Top;
  aControlPosition.Right := FWordProcessor.Left + FWordProcessor.Width;
  aControlPosition.Bottom := FWordProcessor.Top + FWordProcessor.Height;
End;


Procedure TWPAddictParser.GetSelectionScreenPosition(Var aSelectionPosition : TRect);
Var
  iTop, iLeft, iHeight : Integer;
  aColour : TColour;
Begin
  If FWordProcessor.Renderer.GetPointForOffset(FWordProcessor.Selection.SelStart, iLeft, iTop, iHeight, aColour) Then
  Begin
    aSelectionPosition.Left := iLeft;
    aSelectionPosition.Top := iTop;
    aSelectionPosition.Bottom := iTop + iHeight;
    aSelectionPosition.Right := iLeft + 10;
    If FWordProcessor.Renderer.GetPointForOffset(FWordProcessor.Selection.SelEnd, iLeft, iTop, iHeight, aColour) Then
    Begin
      If iTop <> aSelectionPosition.Top Then
      Begin
        aSelectionPosition.Left := 0;
        aSelectionPosition.Bottom := iTop + iHeight;
        aSelectionPosition.Right := FWordProcessor.Width;
      End
      Else
      Begin
        aSelectionPosition.Bottom := IntegerMax(iTop + iHeight, aSelectionPosition.Bottom);
        aSelectionPosition.Right := IntegerMax(iLeft, aSelectionPosition.Right);
      End;
    End;
  End;
End;


Procedure TWPAddictParser.UndoLast(iUndoState : LongInt; iUndoAction : LongInt; Var iUndoData : LongInt);
Begin
  // ignored;
End;


Function TWPAddictParser.GetControl:TComponent;
Begin
  Result := FWordProcessor;

  If Assigned(Result) Then
  Begin
    While (Result Is TControl) And Assigned(TControl(Result).Parent) Do
      Result := TControl(Result).Parent;
  End;
End;

function TWPAddictParser.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSpeller.sizeInBytes);
  inc(result, FWordProcessor.sizeInBytes);
end;

End.
