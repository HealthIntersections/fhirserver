Unit wp_text;

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

{$i fhir.inc}

Interface

Uses
  SysUtils, Graphics,
  fsl_base, fsl_utilities, fsl_collections, fsl_stream,
  wp_types, wp_document, wp_working, wp_format;

Const
  COLUMN_MIN_WIDTH = 5; // 2 + margins
  
Type
  TWPTextReader = Class (TWPReader)
    Private
      FLastWasPara : Boolean;
      Procedure AddWord(oDocument : TWPWorkingDocument; Const sWord : String);
      Procedure AddParagraph(oDocument : TWPWorkingDocument);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      Procedure Read(oDocument : TWPWorkingDocument); Override;
  End;

  // although this is used for text, in a few circumstances this is
  // used for formatted text where the format commands are attached
  // to either text before or after the format. The format commands
  // do not count to width, and they do not get separated from their bit
  // when breaking lines up - in fact, they will be repeated

  TWPTextFragment = Class (TFslObject)
    Private
      FText : String;
      FFormatBefore : String;
      FFormatAfter : String;
      FResetColCount: Boolean;
      FBreakAllowedBefore : Boolean;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      Function Link : TWPTextFragment; Overload;

      Function Final(oBefore, oAfter : TWPTextFragment): String;

      Property Text : String Read FText Write FText;
      Property FormatBefore : String Read FFormatBefore Write FFormatBefore;
      Property FormatAfter : String Read FFormatAfter Write FFormatAfter;
      Property ResetColCount : Boolean Read FResetColCount Write FResetColCount;
      Property BreakAllowedBefore : Boolean read FBreakAllowedBefore write FBreakAllowedBefore;
  End;

  TWPTextFragmentList = Class (TFslObjectList)
    Private
      Function GetFragment(iIndex : Integer) : TWPTextFragment;
    Protected
      Function ItemClass : TFslObjectClass; Override;
    Public
      Function Contents : String;
      Function Length : Integer;
      Function CharAt(iIndex : Integer):Char;
      Function Final(Const iStart, iStop : Integer; Var iLength : Integer) : String;
      Function FinalFull(iWidth : Integer; Alignment : TWordProcessorParagraphAlignment; Const bPadToWidth : Boolean) : String;

      Property Fragment[iIndex : Integer] : TWPTextFragment Read GetFragment; Default;
  End;

  TWPTextFragmentListList = Class (TFslObjectList)
    Private
      Function GetFragments(iIndex : Integer) : TWPTextFragmentList;
    Protected
      Function ItemClass : TFslObjectClass; Override;
    Public
      Property Fragments[iIndex : Integer] : TWPTextFragmentList Read GetFragments; Default;
  End;

  TWPTextModelPiece = Class (TFslObject)
    Public
      Function Link : TWPTextModelPiece; Overload;
      Function Contents : String; Overload; Virtual;
  End;

  TWPTextModelPieces = Class (TFslObjectList)
    Private
      FCursor : Integer;
      Function GetPiece(iIndex : Integer) : TWPTextModelPiece;
    Protected
      Function ItemClass : TFslObjectClass; Override;
    function sizeInBytesV : cardinal; override;
    Public
      Procedure Start(iMaxWidth : Integer); Overload; Virtual;
      Function Finished : Boolean; Overload; Virtual;
      Function NextLine(iWidth : Integer; bPadToWidth : Boolean) : String; Overload; Virtual;

      Function Contents : String; Overload; Virtual;

      Property Piece[iIndex : Integer] : TWPTextModelPiece Read GetPiece; Default;
  End;

  TWPTextModelExpandingPiece = Class (TWPTextModelPiece)
    Private
      FExpander : Char;
      FText : String;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      Function Link : TWPTextModelExpandingPiece; Overload;

      Function AsLine(iWidth : Integer) : String;
      Function Contents : String; Overload; Override;

      Property Expander : Char Read FExpander Write FExpander;
      Property Text : String Read FText Write FText;
  End;

  TWPTextModelParagraphPiece = Class (TWPTextModelPiece)
    Private
      FAlignment : TWordProcessorParagraphAlignment;
      FCursor : Integer;
      FPhantom : Boolean;
      FFragments : TWPTextFragmentList;
      FLines : TWPTextFragmentListList;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;
      Function Link : TWPTextModelParagraphPiece; Overload;

      Procedure Start(iMaxWidth : Integer); Overload; Virtual;
      Function Finished : Boolean; Overload; Virtual;
      Function NextLine(iWidth : Integer; bPadToWidth : Boolean) : String; Overload; Virtual;

      Function Contents : String; Overload; Override;
      Procedure AddText(Const sText, sFormatBefore, sFormatAfter: String; bResetColCount : Boolean); Overload; Virtual;

      Property Alignment : TWordProcessorParagraphAlignment Read FAlignment Write FAlignment;
      Property Phantom : Boolean Read FPhantom Write FPhantom;
  End;

  TWPTextModelCell = Class (TFslObject)
    Private
      FPieces : TWPTextModelPieces;
      FSpan : Integer;

      Function GetPieces : TWPTextModelPieces;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TWPTextModelCell; Overload;

      Property Pieces : TWPTextModelPieces Read GetPieces;
      Property Span : Integer Read FSpan Write FSpan;
  End;

  TWPTextModelCells = Class (TFslObjectList)
    Private
      Function GetCell(iIndex : Integer) : TWPTextModelCell;
    Protected
      Function ItemClass : TFslObjectClass; Override;
    Public
      Property Cell[iIndex : Integer] : TWPTextModelCell Read GetCell; Default;
  End;

  TWPTextModelRow = Class (TFslObject)
    Private
      FCells : TWPTextModelCells;
      Function GetCells : TWPTextModelCells;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TWPTextModelRow; Overload;

      Property Cells : TWPTextModelCells Read GetCells;
  End;

  TWPTextModelRows = Class (TFslObjectList)
    Private
      Function GetRow(iIndex : Integer) : TWPTextModelRow;
    Protected
      Function ItemClass : TFslObjectClass; Override;
    Public
      Property Row[iIndex : Integer] : TWPTextModelRow Read GetRow; Default;
  End;

  TWPTextModelTable = Class (TWPTextModelPiece)
    Private
      FRows : TWPTextModelRows;
      Function GetRows : TWPTextModelRows;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TWPTextModelTable; Overload;

      Property Rows : TWPTextModelRows Read GetRows;
  End;

  TWPTextWriterModelWriter = Class (TFslTextFormatter)
    Private
      FEoln : String;
      FWidth : Integer;
      FDoubleParagraphs : Boolean;

      Procedure ProduceLine(Const sText : String);
      Procedure CalcTableWidths(oWidths : TFslIntegerList; oTable : TWPTextModelTable; iWidth : Integer);

      Procedure ProducePiece(oPiece : TWPTextModelPiece; iWidth : Integer);
      Procedure ProduceExpanding(oPiece : TWPTextModelExpandingPiece; iWidth : Integer);
      Procedure ProduceParagraph(oPiece : TWPTextModelParagraphPiece; iWidth : Integer);

      Function CellWidth(oWidths : TFslIntegerList; iStart, iSpan : Integer) : Integer;
      Procedure ProduceHorizLine(oWidths : TFslIntegerList);
      Procedure ProduceRow(oRow : TWPTextModelRow; oWidths : TFslIntegerList);
      Procedure ProduceTable(oPiece : TWPTextModelTable; iWidth : Integer);
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      Procedure Produce(oContents : TWPTextModelPieces); Overload; Virtual;

      Property Width : Integer Read FWidth Write FWidth;
      Property Eoln : String Read FEoln Write FEoln;
      Property DoubleParagraphs : Boolean Read FDoubleParagraphs Write FDoubleParagraphs;
  End;



  TWPTextWriterTableColumnSizeCalculator = Class (TFslObject)
    Private
      FTable : TWPTextModelTable;
      FColumns : TWPTableColumnMetrics;
      FWidth : Integer;
      FColumnCount : Integer;

      Procedure SetTable(Const Value : TWPTextModelTable);
      Procedure SetColumns(Const Value : TWPTableColumnMetrics);

      Procedure CountColumns;
      Procedure InitialiseColumns;
      Procedure SumColumnWidths;
      Procedure RemoveMargins;

      Procedure MeasureCell(oCell : TWPTextModelCell; Var iMinimum, iMaximum, iTotal : Integer);
      Procedure CountCellWidth(oCell : TWPTextModelCell; iFirstCol : Integer);

      Procedure AllColumns(oColumns : TWPTableColumnMetrics);
      Procedure AllocateMinimums(oColumns : TWPTableColumnMetrics);
      Procedure AllocateExtra(oColumns : TWPTableColumnMetrics);
      Procedure TrimIfRequired;
      Function GetTable : TWPTextModelTable;
      Function GetColumns : TWPTableColumnMetrics;
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create(oTable : TWPTextModelTable; oColumns : TWPTableColumnMetrics; iWidth : Integer); Overload; Virtual;
      destructor Destroy; Override;

      Procedure Calculate; Overload; Virtual;

      Property Table : TWPTextModelTable Read GetTable Write SetTable;
      Property Columns : TWPTableColumnMetrics Read GetColumns Write SetColumns;
      Property Width : Integer Read FWidth Write FWidth;
  End;


  TWPTextWriter = Class (TWPWriter)
  Private
    FWidth : Integer;
    FDoubleParagraphs : Boolean;
    FExpandSectionStartText : Boolean;
    FIgnoreReadOnlyPieces : Boolean;

    FPieces : TWPTextModelPieces;
    FWorkingPieces : TWPTextModelPieces;
    FCurrentTable : TWPTextModelTable;
    FCurrentRow : TWPTextModelRow;
    FCurrentCell : TWPTextModelCell;
    FCurrentPara : TWPTextModelParagraphPiece;

    Function NameIsGUID(Const sName : String): Boolean;
    Function CanWriteEditablePiece(Const oPiece : TWPWorkingDocumentPiece) : Boolean;

    Function GetPieceStyle(oPiece : TWPWorkingDocumentPiece) : TWPStyle;

  Protected
    FFormatBefore : String;
    FFormatAfter : String;

    Procedure Initialise; Override;
    Procedure Finalise; Override;

    Procedure WriteText(oText : TWPWorkingDocumentTextPiece); Overload; Override;
    Procedure WriteImage(oImage : TWPWorkingDocumentImagePiece); Overload; Override;
    Procedure WriteLineBreak(oBreak : TWPWorkingDocumentLineBreakPiece); Overload; Override;
    Procedure WriteFieldStart(oField : TWPWorkingDocumentFieldStartPiece); Overload; Override;
    Procedure WriteFieldStop(oField : TWPWorkingDocumentFieldStartPiece; oStop : TWPWorkingDocumentFieldStopPiece); Overload; Override;
    Procedure WriteParagraphStart(oParagraph : TWPWorkingDocumentParaPiece); Overload; Override;
    Procedure WriteParagraphStop(oParagraph : TWPWorkingDocumentParaPiece; bNextIsSection : Boolean; oSection : TWPWorkingDocumentSectionStartPiece); Overload; Override;
    Procedure WriteBreak(oBreak : TWPWorkingDocumentBreakPiece); Overload; Override;
    Procedure WriteTableStart(oTable : TWPWorkingDocumentTableStartPiece); Overload; Override;
    Procedure WriteTableStop(oTable : TWPWorkingDocumentTableStartPiece; oStop : TWPWorkingDocumentStopPiece); Overload; Override;
    Procedure WriteTableRowStart(oTableRow : TWPWorkingDocumentTableRowStartPiece); Overload; Override;
    Procedure WriteTableRowStop(oTableRow : TWPWorkingDocumentTableRowStartPiece; oStop : TWPWorkingDocumentStopPiece; bIsLast : Boolean); Overload; Override;
    Procedure WriteTableCellStart(oTableCell : TWPWorkingDocumentTableCellStartPiece); Overload; Override;
    Procedure WriteTableCellStop(oTableCell : TWPWorkingDocumentTableCellStartPiece; oStop : TWPWorkingDocumentStopPiece); Overload; Override;
    Procedure WriteSectionStart(oSection : TWPWorkingDocumentSectionStartPiece); Overload; Override;

    Procedure AddExpandable(cChar : Char; Const sText : String); Overload; Virtual;
    Procedure AddText(Const sText : String; bResetColCount : Boolean = False); Overload; Virtual;

    Procedure ConfigureTextWriter(oWriter : TWPTextWriterModelWriter); Overload; Virtual;

    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;

    Property Width : Integer Read FWidth Write FWidth;
    Property DoubleParagraphs : Boolean Read FDoubleParagraphs Write FDoubleParagraphs;
    Property ExpandSectionStartText : Boolean Read FExpandSectionStartText Write FExpandSectionStartText;
    Property IgnoreReadOnlyPieces : Boolean Read FIgnoreReadOnlyPieces Write FIgnoreReadOnlyPieces;
  End;


{
This class generates a text stream which is marked up
using PIT escape sequences where appropriate

PIT format commands supported:
 bolding SBLD/EBLD
 Underline SUND/EUND
 Foreground FGXX
 background BGXX

 Blinking (SBLK/EBLK), and font info (FO, PI) are not supported  (no meaning in the WP for blink, other's not standardised)
}

Type
  TWPPITWriter = Class (TWPTextWriter)
    Private
      FBold : Boolean;
      FUnderline : Boolean;
      FForeground : String;
      FBackground : String;

      Procedure CheckFormat(oPiece : TWPWorkingDocumentPiece);

      Function PickColour(aColour : TColour) : String;
    Protected
      Procedure WriteText(oText : TWPWorkingDocumentTextPiece); Overload; Override;
      Procedure WriteImage(oImage : TWPWorkingDocumentImagePiece); Overload; Override;

      Procedure WriteParagraphStop(oParagraph : TWPWorkingDocumentParaPiece; bNextIsSection : Boolean; oSection : TWPWorkingDocumentSectionStartPiece); Overload; Override;
      Procedure WriteBreak(oBreak : TWPWorkingDocumentBreakPiece); Overload; Override;
      Procedure WriteTableStop(oTable : TWPWorkingDocumentTableStartPiece; oStop : TWPWorkingDocumentStopPiece); Overload; Override;
      Procedure WriteTableRowStop(oTableRow : TWPWorkingDocumentTableRowStartPiece; oStop : TWPWorkingDocumentStopPiece; bIsLast : Boolean); Overload; Override;
      Procedure WriteTableCellStop(oTableCell : TWPWorkingDocumentTableCellStartPiece; oStop : TWPWorkingDocumentStopPiece); Overload; Override;

    function sizeInBytesV : cardinal; override;
  End;

Implementation

Procedure TWPTextReader.AddParagraph(oDocument : TWPWorkingDocument);
Var
  oPara : TWPWorkingDocumentParaPiece;
Begin
  oPara := TWPWorkingDocumentParaPiece.Create;
  Try
    oPara.SpeechMagicDouble := SpeechMagicDouble;
    If HasStyle Then
      oPara.Style := Style
    Else
      oPara.Style := DEFAULT_STYLE_NAME;
    If HasFont Then
      oPara.Font.Assign(Font)
    Else If Styles.HasDefaultStyle Then
      oPara.Font.Assign(Styles.DefaultStyle.Font)
    Else
      oPara.Font.Defaults;
    oDocument.Pieces.Add(oPara.Link);
    FLastWasPara := True;
  Finally
    oPara.Free;
  End;
End;

Procedure TWPTextReader.AddWord(oDocument : TWPWorkingDocument; Const sWord: String);
Var
  oText : TWPWorkingDocumentTextPiece;
Begin
  If sWord <> '' Then
    Begin
    oText := TWPWorkingDocumentTextPiece.Create;
    Try
      If HasStyle Then
        oText.Style := Style
      Else
        oText.Style := DEFAULT_STYLE_NAME;
      If HasFont Then
        oText.Font.Assign(Font)
      Else If Styles.HasDefaultStyle Then
        oText.Font.Assign(Styles.DefaultStyle.Font)
      Else
        oText.Font.Defaults;
      oText.Content := sWord;
      oDocument.Pieces.Add(oText.Link);
      FLastWasPara := False;
    Finally
      oText.Free;
    End;
    End;
End;

Procedure TWPTextReader.Read(oDocument : TWPWorkingDocument);
Var
  iLoop : Integer;
{$IFNDEF VER130}
  bytes : TBytes;
  chars : SysUtils.TCharArray;
{$ENDIF}
  sText : String;
  sWord : String;
  iTab : Integer;
Begin
  Inherited;

{$IFDEF VER130}
  SetLength(sText, Stream.Readable);
  If Length(sText) > 0 Then
    Stream.Read(sText[1], Stream.Readable);
{$ELSE}
  SetLength(bytes, Stream.Readable);
  If Length(bytes) > 0 Then
    Stream.Read(bytes[0], Stream.Readable);
  chars := TEncoding.UTF8.GetChars(bytes);
  if length(chars) = 0 then
    // because UTF-8 can fail with funny chars
    chars := TEncoding.ASCII.GetChars(bytes);
  SetString(sText, pchar(chars), length(chars));
{$ENDIF}

  iLoop := 1;
  sWord := '';
  While iLoop <= Length(sText) Do
  Begin
    While Length(sWord) > MAX_WORD_LENGTH Do
    Begin
      AddWord(oDocument, Copy(sWord, 1, MAX_WORD_LENGTH));
      sWord := Copy(sWord, MAX_WORD_LENGTH + 1, MAXINT);
    End;

    If (IsWordBreak(sText[iLoop])) Then
    Begin
      AddWord(oDocument, sWord);
      sWord := '';
      AddWord(oDocument, sText[iLoop]);
    End
    Else
    Begin
      Case sText[iLoop] Of
        #13, #10:
          Begin
          AddWord(oDocument, sWord);
          sWord := '';
          AddParagraph(oDocument);
          If (iLoop < Length(sText)) And CharInSet(sText[iLoop+1], [#13, #10]) Then
            Inc(iLoop);
          End;
        #9:
          Begin
          AddWord(oDocument, sWord);
          sWord := '';
          For iTab := 1 To TAB_CHAR_COUNT Do
            AddWord(oDocument, ' ');
          End;
      Else
        If Not CharInSet(sText[iLoop], [#0, #26]) Then
          sWord := sWord + sText[iLoop];
      End;
    End;

    Inc(iLoop);
  End;

  AddWord(oDocument, sWord);

  If Not FLastWasPara And MustCloseWithPara Then
    AddParagraph(oDocument);
  DoneReading(oDocument);
End;


function TWPTextReader.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

Function TWPTextModelPieces.GetPiece(iIndex : Integer) : TWPTextModelPiece;
Begin
  Result := TWPTextModelPiece(ObjectByIndex[iIndex]);
End;


Function TWPTextModelPieces.ItemClass : TFslObjectClass;
Begin
  Result := TWPTextModelPiece;
End;


function TWPTextModelPieces.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

Function TWPTextModelCells.GetCell(iIndex : Integer) : TWPTextModelCell;
Begin
  Result := TWPTextModelCell(ObjectByIndex[iIndex]);
End;


Function TWPTextModelCell.GetPieces : TWPTextModelPieces;
Begin
  Assert(Invariants('GetPieces', FPieces, TWPTextModelPieces, 'Pieces'));
  Result := FPieces;
End;


function TWPTextModelCell.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPieces.sizeInBytes);
end;

Function TWPTextModelCells.ItemClass : TFslObjectClass;
Begin
  Result := TWPTextModelCell;
End;


Function TWPTextModelRows.GetRow(iIndex : Integer) : TWPTextModelRow;
Begin
  Result := TWPTextModelRow(ObjectByIndex[iIndex]);
End;


Function TWPTextModelRows.ItemClass : TFslObjectClass;
Begin
  Result := TWPTextModelRow;
End;


Constructor TWPTextModelCell.Create;
Begin
  Inherited;
  FPieces := TWPTextModelPieces.Create;
End;


Destructor TWPTextModelCell.Destroy;
Begin
  FPieces.Free;
  Inherited;
End;


Constructor TWPTextModelRow.Create;
Begin
  Inherited;
  FCells := TWPTextModelCells.Create;
End;


Destructor TWPTextModelRow.Destroy;
Begin
  FCells.Free;
  Inherited;
End;


function TWPTextModelRow.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCells.sizeInBytes);
end;

Constructor TWPTextModelTable.Create;
Begin
  Inherited;
  FRows := TWPTextModelRows.Create;
End;


Destructor TWPTextModelTable.Destroy;
Begin
  FRows.Free;
  Inherited;
End;


function TWPTextModelTable.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FRows.sizeInBytes);
end;

Function TWPTextModelParagraphPiece.Contents : String;
Begin
  Result := FFragments.Contents + cReturn;
End;

Procedure TWPTextModelParagraphPiece.Start(iMaxWidth : Integer);
Var
  iLoop, iWidth, iInner : Integer;
  oFragment, oNew : TWPTextFragment;
  oLine : TWPTextFragmentList;
  Function NewLine : TWPTextFragmentList;
  var
    i : Integer;
  Begin
    if FLines.Count > 0 then
    begin
      i := FLines[FLines.count - 1].Count - 1;
      while i >= 0 do
      begin
        if StringIsWhitespace(FLines[FLines.count - 1][i].Text) then
          FLines[FLines.count - 1].DeleteByIndex(i)
        else
          break;
        dec(i);
      end;
    end;
    Result := TWPTextFragmentList.Create;
    Try
      FLines.Add(Result.Link);
    Finally
      Result.Free;
    End;
    iWidth := 0;
  End;
Begin
  FLines := TWPTextFragmentListList.Create;
  oLine := NewLine;

  if iMaxWidth = 0 then
    raise EWPException.create('The text content contained a table that was too complex to layout given the number of columns available');

  iLoop := 0;
  While iLoop < FFragments.Count Do
  Begin
    oFragment := FFragments[iLoop];
    If (iMaxWidth > -1) And (iMaxWidth < iWidth + Length(oFragment.FText)) And (oLine.Count > 0) And (Not oFragment.FResetColCount) And not StringIsWhitespace(oFragment.Text) Then
      oLine := NewLine;
    If (iMaxWidth > -1) And (Length(oFragment.FText) > iMaxWidth) Then
    Begin
      iInner := 1;
      While iInner <= Length(oFragment.FText) Do
      Begin
        oNew := TWPTextFragment.Create;
        oLine.Add(oNew);
        oNew.FFormatBefore := oFragment.FFormatBefore;
        oNew.FFormatAfter := oFragment.FFormatAfter;
        oNew.FText := Copy(oFragment.FText, iInner, iMaxWidth);
        oLine := NewLine;
        Inc(iInner, iMaxWidth);
      End;
    End
    Else if (oFragment.FText <> #13#10 {line break}) then
    Begin
      Inc(iWidth, Length(oFragment.FText));
      oLine.Add(oFragment.Link);
    End;
    If oFragment.ResetColCount or (oFragment.FText = #13#10 {line break}) Then
      oLine := NewLine;
    Inc(iLoop);
  End;

  FCursor := 0;
End;


Function TWPTextModelParagraphPiece.Finished : Boolean;
Begin
  Result := FCursor >= FLines.Count;
End;


Function InsertSpacesEvenly(sText : String; iGap : Integer) : String;
Var
  oWords : TFslStringList;
  sWord : String;
  sWork : String;
  rGap : Real;
  rPoint : Real;
  iLoop : Integer;
  iPoint : Integer;
Begin
  oWords := TFslStringList.Create;
  Try
    sWork := StringTrimWhitespaceRight(sText);
    iGap := iGap + Length(sText) - Length(sWork);
    sText := sWork;
    While sWork <> '' Do
    Begin
      StringSplit(sWork, ' ', sWord, sWork);
      oWords.Add(sWord);
    End;
    If (oWords.Count < 2) Or (iGap = 0) Then
      Result := sText
    Else
    Begin
      rGap := (oWords.Count-1) / (iGap + 1);
      rPoint := rGap;
      For iLoop := 1 To iGap Do
      Begin
        iPoint := IntegerMax(1, Trunc(rPoint))-1;
        rPoint := rPoint + rGap;
        oWords[iPoint] := oWords[iPoint] + ' ';
      End;
      Result := '';
      For iLoop := 0 To oWords.Count - 1 Do
        StringAppend(Result, oWords[iLoop], ' ');
    End;
  Finally
    oWords.Free;
  End;
  Assert(Length(sText)+iGap = Length(Result),
    'failed to add '+IntegerToString(iGap)+' spaces to '+cReturn+
    '"'+sText+'": ended up with '+cReturn+
    '"'+Result+'" ('+IntegerToString(Length(Result)-Length(sText))+' spaces added)');
End;

Function TWPTextModelParagraphPiece.NextLine(iWidth : Integer; bPadToWidth : Boolean) : String;
Begin
  if (FCursor < FLines.Count - 1) or (Alignment <> WordProcessorParagraphAlignmentJustify) Then
    Result := FLines[FCursor].FinalFull(iWidth, Alignment, bPadToWidth)
  else // don't pad to width on the last line
    Result := FLines[FCursor].FinalFull(iWidth, WordProcessorParagraphAlignmentLeft, bPadToWidth);
  Inc(FCursor);
End;


function TWPTextModelParagraphPiece.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFragments.sizeInBytes);
  inc(result, FLines.sizeInBytes);
end;

Function TWPTextModelExpandingPiece.Contents : String;
Begin
  Result := StringMultiply(Expander, 2) + ' ' + Text + ' ' + StringMultiply(Expander, 2);
End;


Function TWPTextModelExpandingPiece.AsLine(iWidth : Integer) : String;
Begin
  Result := StringPadRight(StringMultiply(Expander, 2) + ' ' + Copy(Text, 1, iWidth - 6)+' ', Expander, iWidth);
End;


function TWPTextModelExpandingPiece.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FText.length * sizeof(char)) + 12);
end;

Procedure TWPTextModelPieces.Start(iMaxWidth : Integer);
Begin
  FCursor := 0;
  If Not Finished And (Piece[FCursor] Is TWPTextModelParagraphPiece) Then
    TWPTextModelParagraphPiece(Piece[FCursor]).Start(iMaxWidth);
End;


Function TWPTextModelPieces.Finished : Boolean;
Begin
  Result := FCursor >= Count;
End;


Function TWPTextModelPieces.NextLine(iWidth : Integer; bPadToWidth : Boolean) : String;
Var
  oPiece : TWPTextModelPiece;
  bNext : Boolean;
Begin
  bNext := False;
  oPiece := Piece[FCursor];
  If oPiece Is TWPTextModelExpandingPiece Then
  Begin
    Result := TWPTextModelExpandingPiece(oPiece).AsLine(iWidth);
    bNext := True;
  End
  Else If oPiece Is TWPTextModelParagraphPiece Then
  Begin
    Result := TWPTextModelParagraphPiece(oPiece).NextLine(iWidth, bPadToWidth);
    bNext := TWPTextModelParagraphPiece(oPiece).Finished;
  End
  Else
    RaiseError('NextLine', 'Unexpected type '+oPiece.ClassName);

  If bNext Then
  Begin
    Inc(FCursor);
    If Not Finished And (Piece[FCursor] Is TWPTextModelParagraphPiece) Then
      TWPTextModelParagraphPiece(Piece[FCursor]).Start(iWidth);
  End;
End;


Function TWPTextModelPieces.Contents : String;
Var
  iLoop : Integer;
Begin
  Result := '';
  For iLoop := 0 To Count - 1 Do
    StringAppend(Result, Piece[iLoop].Contents, '');
End;


Function TWPTextModelPiece.Contents : String;
Begin
  Result := '';
End;


Function TWPTextModelPiece.Link : TWPTextModelPiece;
Begin
  Result := TWPTextModelPiece(Inherited Link);
End;


Function TWPTextModelExpandingPiece.Link : TWPTextModelExpandingPiece;
Begin
  Result := TWPTextModelExpandingPiece(Inherited Link);
End;


Constructor TWPTextModelParagraphPiece.Create;
Begin
  Inherited;
  FFragments := TWPTextFragmentList.Create;
End;


Destructor TWPTextModelParagraphPiece.Destroy;
Begin
  FFragments.Free;
  FLines.Free;
  Inherited;
End;


Function TWPTextModelParagraphPiece.Link : TWPTextModelParagraphPiece;
Begin
  Result := TWPTextModelParagraphPiece(Inherited Link);
End;


Function TWPTextModelCell.Link : TWPTextModelCell;
Begin
  Result := TWPTextModelCell(Inherited Link);
End;


Function TWPTextModelRow.Link : TWPTextModelRow;
Begin
  Result := TWPTextModelRow(Inherited Link);
End;

Function TWPTextModelRow.GetCells : TWPTextModelCells;
Begin
  Assert(Invariants('GetCells', FCells, TWPTextModelCells, 'Cells'));
  Result := FCells;
End;


Function TWPTextModelTable.Link : TWPTextModelTable;
Begin
  Result := TWPTextModelTable(Inherited Link);
End;


Function TWPTextModelTable.GetRows : TWPTextModelRows;
Begin
  Assert(Invariants('GetRows', FRows, TWPTextModelRows, 'Rows'));
  Result := FRows;
End;


{ TWPTextFragment }

Function TWPTextFragment.Final(oBefore, oAfter : TWPTextFragment): String;
Begin
  result := '';
  if (oBefore = nil) or (oBefore.FFormatBefore <> FFormatBefore) Then
    result := FFormatBefore;
  Result := result + FText;
  if (oAfter = nil) or (oAfter.FFormatAfter <> FFormatAfter) Then
    result := result + FFormatAfter;
End;

Function TWPTextFragment.Link: TWPTextFragment;
Begin
  Result := TWPTextFragment(Inherited Link);
End;


function TWPTextFragment.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FText.length * sizeof(char)) + 12);
  inc(result, (FFormatBefore.length * sizeof(char)) + 12);
  inc(result, (FFormatAfter.length * sizeof(char)) + 12);
end;

{ TWPTextFragmentList }

Function TWPTextFragmentList.CharAt(iIndex: Integer): Char;
Var
  iCount : Integer;
  s : String;
Begin
  Result := #0;
  iCount := 0;
  While (iCount < Count) And (Result = #0) And (iIndex >= 1) Do
  Begin
    s := Fragment[iCount].Text;
    If iIndex <= System.Length(Fragment[iCount].Text) Then
      Result := Fragment[iCount].Text[iIndex]
    Else
    Begin
      Dec(iIndex, System.Length(Fragment[iCount].Text));
      Inc(iCount);
    End;
  End;
End;

Function TWPTextFragmentList.Contents: String;
Var
  iCount : Integer;
Begin
  Result := '';
  For iCount := 0 To Count - 1 Do
    Result := Result + Fragment[iCount].Text;
End;

Function Substring(sValue: String; iBegin, iEnd: Integer): String;
Begin
  If (iBegin < 1) Then
    iBegin := 1;
  If (iEnd > 0) Then
    Result := Copy(sValue, iBegin, iEnd - iBegin)
  Else
    Result := '';
End;

Function TWPTextFragmentList.Final(Const iStart, iStop: Integer; Var iLength: Integer): String;
Var
  iCount : Integer;
  iOffset : Integer;
  sText : String;
  oFragment : TWPTextFragment;
  oLast : TWPTextFragment;
  bCarry : Boolean;
Begin
  Result := '';
  iOffset := 0;
  iLength := 0;
  bCarry := False;
  oLast := Nil;
  For iCount := 0 To Count - 1 Do
  Begin
    oFragment := Fragment[iCount];
    sText := SubString(oFragment.Text, iStart - iOffset, iStop - iOffset);
    iOffset := iOffset + System.Length(oFragment.Text);
    If sText <> '' Then
    Begin
      If Not bCarry Then
        Result := Result + oFragment.FFormatBefore;
      Result := Result + sText;
      bCarry := (iCount < Count - 1) And (Fragment[iCount+1].FFormatBefore = oFragment.FFormatBefore) And
          (Fragment[iCount+1].FFormatAfter = oFragment.FFormatAfter);
      If Not bCarry Then
        Result := Result + oFragment.FFormatAfter;
      iLength := iLength + System.Length(sText);
    End
    Else If bCarry And (oLast <> Nil) Then
    Begin
      bCarry := False;
      Result := Result + oLast.FFormatAfter;
    End;
    oLast := oFragment;
  End;
End;

Function TWPTextFragmentList.FinalFull(iWidth: Integer; Alignment: TWordProcessorParagraphAlignment; Const bPadToWidth: Boolean): String;
Var
  iLoop, iSpaces, iCount, iTotal, iCharCount : Integer;
  oUsed : TWPTextFragmentList;
  oBefore, oAfter: TWPTextFragment;
  bDropped : Boolean;
  oSpaces : TFslStringList;
Begin
  Result := '';
  If (iWidth = -1) Or (Alignment <> WordProcessorParagraphAlignmentJustify) Or (Count = 0) Then
  Begin
    For iLoop := 0 To Count - 1 Do
    begin
      if iLoop = 0 then
        oBefore := nil
      else
        oBefore := Fragment[iLoop-1];
      if iLoop = Count - 1 then
        oAfter := nil
      else
        oAfter := Fragment[iLoop+1];
      Result := Result + Fragment[iLoop].Final(oBefore, oAfter);
    end;
    If (iWidth <> -1) Then
      Case (Alignment) Of
        WordProcessorParagraphAlignmentLeft :
          If bPadToWidth Then
            Result := Result + StringMultiply(' ', iWidth - System.Length(Contents));
        WordProcessorParagraphAlignmentRight :
          Result := StringMultiply(' ', iWidth - System.Length(Contents)) + Result;
        WordProcessorParagraphAlignmentCentre :
          Begin
          Result := StringMultiply(' ', (iWidth - System.Length(Contents)) Div 2) + Result;
          If bPadToWidth Then
            Result := Result + StringMultiply(' ', (iWidth - System.Length(Contents)) Div 2);
          End;
      End;
  End
  Else // Alignment = WordProcessorParagraphAlignmentJustify
  Begin
    oUsed := TWPTextFragmentList.Create;
    Try
      iCharCount := 0;

      bDropped := false;
      For iLoop := 0 To Count -1 Do
      Begin
        If Fragment[iLoop].text <> ' ' Then
        Begin
          Fragment[iLoop].BreakAllowedBefore := bDropped;
          bDropped := false;
          oUsed.Add(Fragment[iLoop].Link);
          Inc(iCharCount, System.Length(Fragment[iLoop].text));
        End
        else
          bDropped := true;
      End;

      if oUsed.Count > 0 Then
      Begin
        oSpaces := TFslStringList.Create;
        Try
          iCount := 0;
          iTotal := iWidth - iCharCount;

          For iLoop := 1 To oUsed.Count - 1 Do
          Begin
            if oUsed[iLoop].FBreakAllowedBefore Then
            Begin
              iSpaces := Trunc( (iLoop / (oUsed.Count - 1)) * iTotal) - iCount;
              Inc(iCount, iSpaces);
              oSpaces.Add(StringMultiply(' ', iSpaces));
            End
            else
              oSpaces.Add('');
          End;

          Result := oUsed[0].Final(nil, nil);
          For iLoop := 1 To oUsed.Count - 1 Do
          begin
            if oUsed[iLoop].BreakAllowedBefore Then
              Result := Result + oSpaces[iLoop - 1] + oUsed[iLoop].Final(nil, nil)
            else
              Result := Result + oUsed[iLoop].Final(nil, nil);
          End;
        Finally
          oSpaces.Free;
        End;
      End;
    Finally
      oUsed.Free;
    End;
  End;
End;

Function TWPTextFragmentList.GetFragment(iIndex: Integer): TWPTextFragment;
Begin
  Result := TWPTextFragment(ObjectByIndex[iIndex]);
End;

Function TWPTextFragmentList.ItemClass: TFslObjectClass;
Begin
  Result := TWPTextFragment;
End;

Function TWPTextFragmentList.Length: Integer;
Var
  iCount : Integer;
Begin
  Result := 0;
  For iCount := 0 To Count - 1 Do
    Result := Result + System.Length(Fragment[iCount].Text);
End;

Procedure TWPTextModelParagraphPiece.AddText(Const sText, sFormatBefore, sFormatAfter: String; bResetColCount : Boolean);
Var
  oFragment : TWPTextFragment;
Begin
  oFragment := TWPTextFragment.Create;
  Try
    oFragment.FText := sText;
    oFragment.FFormatBefore := sFormatBefore;
    oFragment.FFormatAfter := sFormatAfter;
    oFragment.ResetColCount := bResetColCount;
    FFragments.Add(oFragment.Link);
  Finally
    oFragment.Free;
  End;
End;



Constructor TWPTextWriterModelWriter.Create;
Begin
  Inherited;
  FEoln := cReturn;
End;


Procedure TWPTextWriterModelWriter.ProduceLine(Const sText : String);
Begin
  Produce(sText + FEoln);
End;


Procedure TWPTextWriterModelWriter.Produce(oContents : TWPTextModelPieces);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oContents.Count - 1 Do
    ProducePiece(oContents[iLoop], FWidth);
End;


Procedure TWPTextWriterModelWriter.ProducePiece(oPiece : TWPTextModelPiece; iWidth : Integer);
Begin
  If oPiece Is TWPTextModelExpandingPiece Then
    ProduceExpanding(TWPTextModelExpandingPiece(oPiece), iWidth)
  Else If oPiece Is TWPTextModelParagraphPiece Then
    ProduceParagraph(TWPTextModelParagraphPiece(oPiece), iWidth)
  Else If oPiece Is TWPTextModelTable Then
    ProduceTable(TWPTextModelTable(oPiece), iWidth)
  Else
    RaiseError('ProducePiece', 'Piece Type '+oPiece.ClassName+' not supported');
End;


Procedure TWPTextWriterModelWriter.ProduceExpanding(oPiece : TWPTextModelExpandingPiece; iWidth : Integer);
Begin
  ProduceLine(oPiece.AsLine(iWidth));
End;


Procedure TWPTextWriterModelWriter.ProduceParagraph(oPiece : TWPTextModelParagraphPiece; iWidth : Integer);
Begin
  oPiece.Start(iWidth);
  While Not oPiece.Finished Do
  Begin
    Produce(oPiece.NextLine(iWidth, False));
    If (Not oPiece.Finished) Then
      ProduceLine('');
  End;
  If Not oPiece.Phantom Then
  Begin
    ProduceLine('');
    If DoubleParagraphs Then
      ProduceLine('');
  End;
End;


Procedure TWPTextWriterModelWriter.ProduceTable(oPiece : TWPTextModelTable; iWidth : Integer);
Var
  oWidths : TFslIntegerList;
  iLoop : Integer;
Begin
  oWidths := TFslIntegerList.Create;
  Try
    CalcTableWidths(oWidths, oPiece, iWidth);

    ProduceHorizLine(oWidths);
    For iLoop := 0 To oPiece.Rows.Count - 1  Do
    Begin
      ProduceRow(oPiece.Rows[iLoop], oWidths);
      ProduceHorizLine(oWidths);
    End;
  Finally
    oWidths.Free;
  End;
  ProduceLine('');
End;


Procedure TWPTextWriterModelWriter.CalcTableWidths(oWidths : TFslIntegerList; oTable : TWPTextModelTable; iWidth : Integer);
Var
  oCalc : TWPTextWriterTableColumnSizeCalculator;
  iLoop : Integer;
Begin
  oCalc := TWPTextWriterTableColumnSizeCalculator.Create(oTable.Link, TWPTableColumnMetrics.Create, iWidth);
  Try
    oCalc.Calculate;

    For iLoop := 0 To oCalc.Columns.Count - 1 Do
      oWidths.Add(oCalc.Columns[iLoop].Actual);
  Finally
    oCalc.Free;
  End;
End;


Procedure TWPTextWriterModelWriter.ProduceHorizLine(oWidths : TFslIntegerList);
Var
  iLoop : Integer;
Begin
  Produce('+');
  For iLoop := 0 To oWidths.Count - 1 Do
  Begin
    Produce(StringMultiply('-', oWidths[iLoop]+2));
    Produce('+');
  End;
  ProduceLine('');
End;


Function TWPTextWriterModelWriter.CellWidth(oWidths : TFslIntegerList; iStart, iSpan : Integer) : Integer;
Var
  iLoop : Integer;
Begin
  Result := oWidths[iStart];
  For iLoop := iStart + 1 To iStart + iSpan - 1 Do
    Inc(Result, 1 + oWidths[iLoop]+2);
End;


Procedure TWPTextWriterModelWriter.ProduceRow(oRow : TWPTextModelRow; oWidths : TFslIntegerList);
Var
  iLoop : Integer;
  iCol : Integer;
  bDone : Boolean;
  oCell : TWPTextModelCell;
  iCellWidth : Integer;
Begin
  For iLoop := 0 To oRow.Cells.Count - 1 Do
    oRow.Cells[iLoop].Pieces.Start(oWidths[iLoop]);

  Repeat
    bDone := True;
    Produce('|');
    iCol := 0;
    For iLoop := 0 To oRow.Cells.Count - 1 Do
    Begin
      oCell := oRow.Cells[iLoop];
      Produce(' ');
      iCellWidth := CellWidth(oWidths, iCol, oCell.Span);

      If oCell.Pieces.Finished Then
        Produce(StringPadRight('', ' ', iCellWidth))
      Else
        Produce(StringPadRight(oCell.Pieces.NextLine(iCellWidth, True), ' ', iCellWidth));

      bDone := bDone And oCell.Pieces.Finished;
      Inc(iCol, oCell.Span);
      Produce(' |');
    End;
    ProduceLine('');
  Until bDone;
End;


function TWPTextWriterModelWriter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FEoln.length * sizeof(char)) + 12);
end;

{ TWPTextWriter }


Constructor TWPTextWriter.Create;
Begin
  Inherited;

  FWidth := 76;
  FExpandSectionStartText := True;
  FIgnoreReadOnlyPieces := False;
End;


Procedure TWPTextWriter.Initialise;
Begin
  Inherited;

  FPieces := TWPTextModelPieces.Create;
  FWorkingPieces := FPieces;
End;


Function TWPTextWriter.CanWriteEditablePiece(Const oPiece: TWPWorkingDocumentPiece) : Boolean;
Begin
  Result := Not (IgnoreReadOnlyPieces And oPiece.IsReadOnly);
End;


Procedure TWPTextWriter.ConfigureTextWriter(oWriter : TWPTextWriterModelWriter);
Begin
  // nothing
End;


Procedure TWPTextWriter.Finalise;
Var
  oWriter : TWPTextWriterModelWriter;
Begin
  If FWidth = 0 Then
    RaiseError('Finalise', 'cannot use zero width formatting');

  oWriter := TWPTextWriterModelWriter.Create;
  Try
    oWriter.DoubleParagraphs := DoubleParagraphs;
    oWriter.Stream := Stream.Link;
    ConfigureTextWriter(oWriter);
    oWriter.Width := FWidth;
    oWriter.Produce(FPieces);
  Finally
    oWriter.Free;
  End;
  FPieces.Free;
  Inherited;
End;


Function TWPTextWriter.NameIsGUID(Const sName : String): Boolean;
Begin
  Result := False;
End;


Procedure TWPTextWriter.AddExpandable(cChar : Char; Const sText : String);
Var
  oExpandable : TWPTextModelExpandingPiece;
Begin
  Assert(CheckCondition(FCurrentPara = Nil, 'AddExpandable', 'In Paragraph'));

  oExpandable := TWPTextModelExpandingPiece.Create;
  Try
    oExpandable.Expander := cChar;
    oExpandable.Text := sText;
    FWorkingPieces.Add(oExpandable.Link);
  Finally
    oExpandable.Free;
  End;
End;


Procedure TWPTextWriter.AddText(Const sText : String; bResetColCount : Boolean);
Begin
  Assert(CheckCondition(Assigned(FCurrentPara), 'AddText', 'No Paragraph'));

  FCurrentPara.AddText(sText, FFormatBefore, FFormatAfter, bResetColCount);
End;


Procedure TWPTextWriter.WriteSectionStart(oSection : TWPWorkingDocumentSectionStartPiece);
Begin
  If CanWriteEditablePiece(oSection) And ExpandSectionStartText And (oSection.DisplayName <> '') Then
    AddExpandable('=', 'Section: ' + oSection.DisplayName);
End;


Procedure TWPTextWriter.WriteText(oText : TWPWorkingDocumentTextPiece);
Var
  oStyle : TWPStyle;
Begin
  If CanWriteEditablePiece(oText) Then
  Begin
    oStyle := GetPieceStyle(oText);

    If oStyle.WorkingFontCapitalization(oText.Font) In [fcsAllCaps, fcsSmallCaps] Then
      AddText(StringUpper(oText.Content))
    Else If oStyle.WorkingFontCapitalization(oText.Font) = fcsNoCaps Then
      AddText(LowerCase(oText.Content))
    Else
      AddText(oText.Content);
  End;
End;


Procedure TWPTextWriter.WriteImage(oImage : TWPWorkingDocumentImagePiece);
Begin
  If CanWriteEditablePiece(oImage) Then
  Begin
    If NameIsGUID(oImage.Name) Then
      AddText('[Image]')
    Else
      AddText('[Image "'+PathTitle(oImage.Name)+'"]');
  End;
End;


Procedure TWPTextWriter.WriteLineBreak(oBreak : TWPWorkingDocumentLineBreakPiece);
Begin
  If CanWriteEditablePiece(oBreak) Then
    AddText(cReturn);
End;


Procedure TWPTextWriter.WriteFieldStart(oField : TWPWorkingDocumentFieldStartPiece);
Begin
  // ignore fields
End;


Procedure TWPTextWriter.WriteFieldStop(oField : TWPWorkingDocumentFieldStartPiece; oStop : TWPWorkingDocumentFieldStopPiece);
Begin
  // ignore fields
End;


Procedure TWPTextWriter.WriteParagraphStart(oParagraph : TWPWorkingDocumentParaPiece);
Var
  oPara : TWPTextModelParagraphPiece;
Begin
  If CanWriteEditablePiece(oParagraph) Then
  Begin
    oPara := TWPTextModelParagraphPiece.Create;
    Try
      If Assigned(oParagraph) Then
        oPara.Alignment := oParagraph.Format.Align
      Else
        oPara.Phantom := True;

      FWorkingPieces.Add(oPara.Link);
      FCurrentPara := oPara;
    Finally
      oPara.Free;
    End;
  End;
End;


Procedure TWPTextWriter.WriteParagraphStop(oParagraph : TWPWorkingDocumentParaPiece; bNextIsSection : Boolean; oSection : TWPWorkingDocumentSectionStartPiece);
Begin
  If CanWriteEditablePiece(oParagraph) Then
    FCurrentPara := Nil;
End;


Procedure TWPTextWriter.WriteBreak(oBreak : TWPWorkingDocumentBreakPiece);
Begin
  If CanWriteEditablePiece(oBreak) Then
  Begin
    If oBreak.BreakType = btPageBreak Then
      AddExpandable('-', 'Page Break')
    Else
      AddExpandable('_', '');
  End;
End;


Procedure TWPTextWriter.WriteTableStart(oTable : TWPWorkingDocumentTableStartPiece);
Var
  oTextTable : TWPTextModelTable;
Begin
  If CanWriteEditablePiece(oTable) Then
  Begin
    oTextTable := TWPTextModelTable.Create;
    Try
      FPieces.Add(oTextTable.Link);
      FCurrentPara := Nil;
      FCurrentTable := oTextTable;
    Finally
      oTextTable.Free;
    End;
  End;
End;


Procedure TWPTextWriter.WriteTableStop(oTable : TWPWorkingDocumentTableStartPiece; oStop : TWPWorkingDocumentStopPiece);
Begin
  If CanWriteEditablePiece(oTable) Then
  Begin
    FCurrentTable := Nil;
    FWorkingPieces := FPieces;
  End;
End;


Procedure TWPTextWriter.WriteTableRowStart(oTableRow : TWPWorkingDocumentTableRowStartPiece);
Var
  oTextRow : TWPTextModelRow;
Begin
  If CanWriteEditablePiece(oTableRow) Then
  Begin
    Assert(CheckCondition(Assigned(FCurrentTable), 'WriteTableRowStart', 'No Table'));

    oTextRow := TWPTextModelRow.Create;
    Try
      FCurrentTable.Rows.Add(oTextRow.Link);
      FCurrentRow := oTextRow;
    Finally
      oTextRow.Free;
    End;
  End;
End;

Procedure TWPTextWriter.WriteTableRowStop(oTableRow : TWPWorkingDocumentTableRowStartPiece; oStop : TWPWorkingDocumentStopPiece; bIsLast : Boolean);
Begin
  If CanWriteEditablePiece(oTableRow) Then
    FCurrentRow := Nil;
End;


Procedure TWPTextWriter.WriteTableCellStart(oTableCell : TWPWorkingDocumentTableCellStartPiece);
Var
  oTextCell : TWPTextModelCell;
Begin
  If CanWriteEditablePiece(oTableCell) Then
  Begin
    Assert(CheckCondition(Assigned(FCurrentRow), 'WriteRowCellStart', 'No Row'));

    oTextCell := TWPTextModelCell.Create;
    Try
      oTextCell.Span := oTableCell.Span;
      FCurrentRow.Cells.Add(oTextCell.Link);
      FCurrentCell := oTextCell;
      FWorkingPieces := FCurrentCell.Pieces;
    Finally
      oTextCell.Free;
    End;
  End;
End;


Procedure TWPTextWriter.WriteTableCellStop(oTableCell : TWPWorkingDocumentTableCellStartPiece; oStop : TWPWorkingDocumentStopPiece);
Begin
  If CanWriteEditablePiece(oTableCell) Then
  Begin
    FWorkingPieces := Nil;
    FCurrentCell := Nil;
  End;
End;



function TWPTextWriter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPieces.sizeInBytes);
  inc(result, FWorkingPieces.sizeInBytes);
  inc(result, FCurrentTable.sizeInBytes);
  inc(result, FCurrentRow.sizeInBytes);
  inc(result, FCurrentCell.sizeInBytes);
  inc(result, FCurrentPara.sizeInBytes);
  inc(result, (FFormatBefore.length * sizeof(char)) + 12);
  inc(result, (FFormatAfter.length * sizeof(char)) + 12);
end;

Constructor TWPTextWriterTableColumnSizeCalculator.Create(oTable : TWPTextModelTable; oColumns : TWPTableColumnMetrics; iWidth : Integer);
Begin
  Create;

  FTable := oTable;
  FColumns := oColumns;
  FWidth := iWidth;
End;


Destructor TWPTextWriterTableColumnSizeCalculator.Destroy;
Begin
  FTable.Free;
  FColumns.Free;

  Inherited;
End;


Procedure TWPTextWriterTableColumnSizeCalculator.SetTable(Const Value : TWPTextModelTable);
Begin
  FTable.Free;
  FTable := Value;
End;


Procedure TWPTextWriterTableColumnSizeCalculator.SetColumns(Const Value : TWPTableColumnMetrics);
Begin
  FColumns.Free;
  FColumns := Value;
End;


Procedure TWPTextWriterTableColumnSizeCalculator.Calculate;
Var
  oToAllocate : TWPTableColumnMetrics;
Begin
  CountColumns;
  InitialiseColumns;
  SumColumnWidths;

  oToAllocate := TWPTableColumnMetrics.Create;
  Try
    AllColumns(oToAllocate);
    AllocateMinimums(oToAllocate);
    TrimIfRequired;
    AllocateExtra(oToAllocate);
    RemoveMargins;
  Finally
    oToAllocate.Free;
  End;
End;


Procedure TWPTextWriterTableColumnSizeCalculator.CountColumns;
Var
  iLoop : Integer;
  oRow : TWPTextModelRow;
  iCount : Integer;
  iCol : Integer;
Begin
  FColumnCount := 0;
  For iLoop := 0 To Table.Rows.Count - 1 Do
  Begin
    oRow := Table.Rows[iLoop];
    iCount := 0;
    For iCol := 0 To oRow.Cells.Count - 1 Do
      Inc(iCount, oRow.Cells[iCol].Span);
    FColumnCount := IntegerMax(FColumnCount, iCount);
  End;
End;


Procedure TWPTextWriterTableColumnSizeCalculator.InitialiseColumns;
Var
  iLoop : Integer;
  oColumn : TWPRendererTableColumnMetric;
Begin
  FColumns.Clear;
  For iLoop := 1 To FColumnCount Do
  Begin
    oColumn := FColumns.New;
    Try
      If iLoop = 1 Then
        oColumn.DeadLefts.Add(2+1) // 1 - mandatory column divider
      Else
        oColumn.DeadLefts.Add(1+1); // 1 - mandatory column divider
      oColumn.DeadRights.Add(2);
      FColumns.Add(oColumn.Link);
    Finally
      oColumn.Free;
    End;
  End;
End;


Procedure TWPTextWriterTableColumnSizeCalculator.SumColumnWidths;
Var
  iSpan : Integer;
  iRow : Integer;
  iCell : Integer;
  iCol : Integer;
  oRow : TWPTextModelRow;
  oCell : TWPTextModelCell;
Begin
  For iSpan := 1 To FColumnCount Do
  Begin
    For iRow := 0 To Table.Rows.Count - 1 Do
    Begin
      oRow := Table.Rows[iRow];
      iCol := 0;
      For iCell := 0 To oRow.Cells.Count - 1 Do
      Begin
        oCell := oRow.Cells[iCell];
        If oCell.Span = iSpan Then
          CountCellWidth(oCell, iCol);
        Inc(iCol, oCell.Span)
      End;
    End;
  End;
End;


Procedure TWPTextWriterTableColumnSizeCalculator.MeasureCell(oCell : TWPTextModelCell; Var iMinimum, iMaximum, iTotal : Integer);
Var
  iLoop : Integer;
  iWord : Integer;
  iPara : Integer;
  sText : String;
Begin
  iMinimum := 0;
  iMaximum := 0;
  iTotal := 0;
  iWord := 0;
  iPara := 0;
  sText := oCell.Pieces.Contents;
  For iLoop := 1 To Length(sText) Do
  Begin
    Case sText[iLoop] Of
      ' ':
        Begin
        iWord := 0;
        Inc(iPara);
        End;
      #13:
        Begin
          iWord := 0;
          iPara := 0;
        End;
      #10:; // nothing
    Else
      Inc(iPara);
      Inc(iWord);
      Inc(iTotal);
    End;
    iMinimum := IntegerMax(iWord, iMinimum);
    iMaximum := IntegerMax(iPara, iMaximum);
  End;
End;


Procedure TWPTextWriterTableColumnSizeCalculator.CountCellWidth(oCell : TWPTextModelCell; iFirstCol : Integer);
Var
  oCol : TWPRendererTableColumnMetric;
  iLoop : Integer;
  iMin : Integer;
  iMax : Integer;
  iMinimum : Integer;
  iMaximum : Integer;
  iTotal : Integer;
Begin
  MeasureCell(oCell, iMinimum, iMaximum, iTotal);

  oCol := FColumns[iFirstCol];
  If oCell.Span = 1 Then
  Begin
    oCol.Minimum := IntegerMax(iMinimum, oCol.Minimum);
    oCol.Maximum := IntegerMax(iMaximum, oCol.Maximum);
    oCol.TotalChars := oCol.TotalChars + iTotal;
  End
  Else
  Begin
    iMin := oCol.Minimum;
    iMax := oCol.Maximum;
    For iLoop := iFirstCol + 1 To iFirstCol + oCell.Span - 1 Do
    Begin
      oCol := FColumns[iLoop];
      Inc(iMin, oCol.Minimum + oCol.DeadLeft + oCol.DeadRight);
      Inc(iMax, oCol.Maximum + oCol.DeadLeft + oCol.DeadRight);
    End;

    iMin := IntegerMax(0, iMinimum - iMin);
    iMax := IntegerMax(0, iMaximum - iMax);

    For iLoop := iFirstCol To iFirstCol + oCell.Span - 1 Do
    Begin
      oCol := FColumns[iLoop];
      oCol.Minimum := oCol.Minimum + Trunc(iMin / oCell.Span);
      oCol.Maximum := oCol.Maximum + Trunc(iMax / oCell.Span);
      oCol.TotalChars := oCol.TotalChars + Trunc(iTotal / oCell.Span);
    End;
  End;
End;


Procedure TWPTextWriterTableColumnSizeCalculator.AllColumns(oColumns : TWPTableColumnMetrics);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To FColumns.Count - 1 Do
    oColumns.Add(FColumns[iLoop].Link);
End;


Procedure TWPTextWriterTableColumnSizeCalculator.AllocateMinimums(oColumns : TWPTableColumnMetrics);
Var
  iLoop : Integer;
  iTotal : Integer;
  iAdjust : Integer;
  oCol : TWPRendererTableColumnMetric;
Begin
  If (FWidth = -1) Then
  Begin
    For iLoop := oColumns.Count - 1 DownTo 0 Do
    Begin
      oCol := oColumns[iLoop];
      oCol.Actual := oCol.DeadLeft + oCol.DeadRight + oCol.Maximum;
    End;
  End
  Else
  Begin
    iTotal := 0;
    For iLoop := oColumns.Count - 1 DownTo 0 Do
    Begin
      oCol := oColumns[iLoop];
      Inc(iTotal, oCol.DeadLeft + oCol.DeadRight + oCol.Minimum);
    End;

    iAdjust := IntegerMax(0, (FColumns.SumActual + iTotal) - FWidth);

    For iLoop := oColumns.Count - 1 DownTo 0 Do
    Begin
      oCol := oColumns[iLoop];
      oCol.Actual := IntegerMax(COLUMN_MIN_WIDTH, oCol.DeadLeft + oCol.DeadRight + oCol.Minimum - (iAdjust Div FColumnCount));
      If oCol.Maximum = oCol.Minimum Then
        oColumns.DeleteByIndex(iLoop);
    End;
  End;
End;

Procedure TWPTextWriterTableColumnSizeCalculator.AllocateExtra(oColumns : TWPTableColumnMetrics);
Var
  iLoop, iOld : Integer;
  rAverage : Real;
  oCol : TWPRendererTableColumnMetric;
  bNone : Boolean;
Begin
  If FWidth = -1 Then
    Exit;
    
  bNone := False;
  rAverage := 0;

  While (oColumns.Count > 1) And Not bNone Do
  Begin
    bNone := True;
    rAverage := (FWidth - FColumns.SumActual) / oColumns.Count;

    For iLoop := oColumns.Count - 1 DownTo 0 Do
    Begin
      oCol := oColumns[iLoop];
      If oCol.Maximum + oCol.DeadLeft + oCol.DeadRight <= Trunc(oCol.Actual + rAverage) Then
      Begin
        bNone := False;
        oCol.Actual := oCol.Maximum + oCol.DeadLeft + oCol.DeadRight;
        oColumns.DeleteByIndex(iLoop);
      End;
    End;
  End;

  If oColumns.Count = 1 Then
  Begin
    oCol := oColumns[0];
    iOld := IntegerMax(COLUMN_MIN_WIDTH, oCol.Actual);
    oCol.Actual := 0;
    oCol.Actual := IntegerMax(iOld, IntegerMin(FWidth - FColumns.SumActual, oCol.Maximum + oCol.DeadLeft + oCol.DeadRight));
  End
  Else
  Begin
    For iLoop := oColumns.Count - 1 DownTo 0 Do
    Begin
      oCol := oColumns[iLoop];
      oCol.Actual := Trunc(oCol.Actual + rAverage);
    End;
  End;
End;

Function TWPTextWriterTableColumnSizeCalculator.GetTable : TWPTextModelTable;
Begin
  Assert(Invariants('GetTable', FTable, TWPTextModelTable, 'Table'));
  Result := FTable;
End;



Function TWPTextWriterTableColumnSizeCalculator.GetColumns : TWPTableColumnMetrics;
Begin
  Assert(Invariants('GetColumns', FColumns, TWPTableColumnMetrics, 'Columns'));
  Result := FColumns;
End;



Procedure TWPTextWriterTableColumnSizeCalculator.TrimIfRequired;
Var
  iSum : Integer;
  iCount : Integer;
  oColumns : TWPTableColumnMetrics;
  iPartialSum : Integer;
  iWidth : Integer;
  iTotal : Integer;
Begin
  If (Width > -1) And (FColumns.SumActual > Width) Then
  Begin
    iSum := FColumns.SumActual;
    iTotal := 0;

    oColumns := TWPTableColumnMetrics.Create;
    Try
      // firstly, we don't take anything off a column with an actual width of COLUMN_MIN_WIDTH
      // they can't get smaller than that.
      iPartialSum := iSum;
      iWidth := Width;
      For iCount := 0 To FColumns.Count - 1 Do
        If FColumns[iCount].Actual > COLUMN_MIN_WIDTH Then
        Begin
          oColumns.Add(FColumns[iCount].Link);
          iTotal := iTotal + FColumns[iCount].TotalChars;
        End
        Else
        Begin
          iPartialSum := iPartialSum - COLUMN_MIN_WIDTH;
          iWidth := iWidth - COLUMN_MIN_WIDTH;
        End;

      // we assume here that we are no wider than minimum (this get's called after minimums are allocated)
      // so we shave off the columns based on their percentage load of total char count
      For iCount := 0 To oColumns.Count - 1 Do
        // new width           is   existing width         - (amount to remove            *  percentage count
        oColumns[iCount].Actual := oColumns[iCount].Actual - Trunc((iPartialSum - iWidth) * (oColumns[iCount].TotalChars / iTotal) );
    Finally
      oColumns.Free;
    End;
  End;
  // try again, this time not considering total char count
  If (Width > -1) And (FColumns.SumActual > Width) Then
  Begin
    iSum := FColumns.SumActual;

    oColumns := TWPTableColumnMetrics.Create;
    Try
      // firstly, we don't take anything off a column with an actual width of COLUMN_MIN_WIDTH
      // they can't get smaller than that.
      iPartialSum := iSum;
      iWidth := Width;
      For iCount := 0 To FColumns.Count - 1 Do
        If FColumns[iCount].Actual > COLUMN_MIN_WIDTH Then
          oColumns.Add(FColumns[iCount].Link)
        Else
        Begin
          iPartialSum := iPartialSum - COLUMN_MIN_WIDTH;
          iWidth := iWidth - COLUMN_MIN_WIDTH;
        End;

      // we assume here that we are no wider than minimum (this get's called after minimums are allocated)
      // so we shave off the columns based on their percentage load
      For iCount := 0 To oColumns.Count - 1 Do
        // new width           is   existing width         - (amount to remove            *  percentage width
        oColumns[iCount].Actual := IntegerMax(COLUMN_MIN_WIDTH, oColumns[iCount].Actual - Trunc((iPartialSum - iWidth) * (oColumns[iCount].Actual / iPartialSum) ));
    Finally
      oColumns.Free;
    End;
  End;
End;

Procedure TWPTextWriterTableColumnSizeCalculator.RemoveMargins;
Var
  iCount : Integer;
Begin
  For iCount := 0 To FColumns.Count - 1 Do
    FColumns[iCount].Actual := FColumns[iCount].Actual - 3; // 3 is overhead
End;

Type
  TPITColour = (
         pcBlack,    pcBlue,      pcGreen,      pcCyan,      pcRed,      pcMagenta,      pcBrown,  pcLightGrey,
         pcDarkGrey, pcLightBlue, pcLightGreen, pcLightCyan, pcLightRed, pcLightMagenta, pcYellow, pcWhite
         );

Const
  PITCOLOURS_CODES : Array [TPITColour] Of String = (
        '00', '01', '02', '03', '04', '05', '06', '07',
        '08', '09', '10', '11', '12', '13', '14', '15');

  PITCOLOURS_COLOURS : Array [TPITColour] Of TColour = (
        clBlack,  clBlue, clGreen, $00FFFF00, clRed,     $008B008B, $002A2AA5, clLtGray,
        clDkGray, clAqua, clLime,  $00FFFFE0, $00FF0000, $00FF00FF, clYellow,  clWhite);


Function ColourDifference(aColour1, aColour2 : TColour):Integer;
Begin
  Result :=
    (TColourParts(aColour1).Red - TColourParts(aColour2).Red) * (TColourParts(aColour1).Red - TColourParts(aColour2).Red) +
    (TColourParts(aColour1).Green - TColourParts(aColour2).Green) * (TColourParts(aColour1).Green - TColourParts(aColour2).Green) +
    (TColourParts(aColour1).Blue - TColourParts(aColour2).Blue) * (TColourParts(aColour1).Blue - TColourParts(aColour2).Blue);
End;


function TWPTextWriterTableColumnSizeCalculator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FTable.sizeInBytes);
  inc(result, FColumns.sizeInBytes);
end;

Function TWPPITWriter.PickColour(aColour : TColour) : String;
Var
  aLoop : TPITColour;
  aSelected : TPITColour;
  rDiff : Integer;
Begin
  aSelected := pcBlack;
  rDiff := ColourDifference(aColour, PITCOLOURS_COLOURS[pcBlack]);

  For aLoop := pcBlue To pcWhite Do
  Begin
    If rDiff > ColourDifference(aColour, PITCOLOURS_COLOURS[aLoop]) Then
    Begin
      rDiff := ColourDifference(aColour, PITCOLOURS_COLOURS[aLoop]);
      aSelected := aLoop;
    End;
  End;

  Result := PITCOLOURS_CODES[aSelected];
End;



Procedure TWPPITWriter.CheckFormat(oPiece : TWPWorkingDocumentPiece);
Var
  bBold : Boolean;
  bUnderline : Boolean;
  sColour : String;
  sEscape : String;
  Procedure AddEscape(bWhether, bWhich : Boolean; sEsc1, sEsc2 : String);
  Begin
    If bWhether Then
      If bWhich Then
        sEscape := sEscape + sEsc1
      Else
        sEscape := sEscape + sEsc2;
  End;
Begin
  sEscape := '';

  bBold := Assigned(oPiece) And ((oPiece.Font.Bold = tsTrue) Or (oPiece.Font.Italic = tsTrue));
  AddEscape(bBold <> FBold, bBold, 'SBLD', 'EBLD');
  FBold := bBold;

  bUnderline := Assigned(oPiece) And (oPiece.Font.Underline = tsTrue);
  AddEscape(bUnderline <> FUnderline, bUnderline, 'SUND', 'EUND');
  FUnderline := bUnderline;

  If Assigned(oPiece) And (oPiece.Font.Foreground <> DEF_COLOUR) Then
    sColour := PickColour(oPiece.Font.Foreground)
  Else
    sColour := '';
  AddEscape(sColour <> FForeground, sColour = '', 'FG99', 'FG'+sColour);
  FForeground := sColour;

  If Assigned(oPiece) And (oPiece.Font.Background <> DEF_COLOUR) Then
    sColour := PickColour(oPiece.Font.Background)
  Else
    sColour := '';
  AddEscape(sColour <> FBackground, sColour = '', 'BG99', 'BG'+sColour);
  FBackground := sColour;

  If sEscape <> '' Then
    AddText('~'+sEscape+'~');
End;


Procedure TWPPITWriter.WriteText(oText : TWPWorkingDocumentTextPiece);
Begin
  CheckFormat(oText);
  Inherited WriteText(oText);
End;


Procedure TWPPITWriter.WriteImage(oImage : TWPWorkingDocumentImagePiece);
Begin
  CheckFormat(oImage);
  Inherited WriteImage(oImage);
End;


Procedure TWPPITWriter.WriteParagraphStop(oParagraph : TWPWorkingDocumentParaPiece; bNextIsSection : Boolean; oSection : TWPWorkingDocumentSectionStartPiece);
Begin
  CheckFormat(Nil);
  Inherited WriteParagraphStop(oParagraph, bNextIsSection, oSection);
End;


Procedure TWPPITWriter.WriteBreak(oBreak : TWPWorkingDocumentBreakPiece);
Begin
  CheckFormat(Nil);
  Inherited WriteBreak(oBreak);
End;


Procedure TWPPITWriter.WriteTableStop(oTable : TWPWorkingDocumentTableStartPiece; oStop : TWPWorkingDocumentStopPiece);
Begin
  CheckFormat(Nil);
  Inherited WriteTableStop(oTable, oStop);
End;


Procedure TWPPITWriter.WriteTableRowStop(oTableRow : TWPWorkingDocumentTableRowStartPiece; oStop : TWPWorkingDocumentStopPiece; bIsLast : Boolean);
Begin
  CheckFormat(Nil);
  Inherited WriteTableRowStop(oTableRow, oStop, bIsLast);
End;


Procedure TWPPITWriter.WriteTableCellStop(oTableCell : TWPWorkingDocumentTableCellStartPiece; oStop : TWPWorkingDocumentStopPiece);
Begin
  CheckFormat(Nil);
  Inherited WriteTableCellStop(oTableCell, oStop);
End;



function TWPPITWriter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FForeground.length * sizeof(char)) + 12);
  inc(result, (FBackground.length * sizeof(char)) + 12);
end;

Function TWPTextWriter.GetPieceStyle(oPiece: TWPWorkingDocumentPiece): TWPStyle;
Begin
  Result := Nil;

  If oPiece.Style <> '' Then
    Result := Styles.GetByName(oPiece.Style);

  If Result = Nil Then
    Result := Styles.DefaultStyle;
End;


{ TWPTextFragmentListList }

Function TWPTextFragmentListList.GetFragments(iIndex: Integer): TWPTextFragmentList;
Begin
  Result := TWPTextFragmentList(ObjectByIndex[iIndex]);
End;

Function TWPTextFragmentListList.ItemClass: TFslObjectClass;
Begin
  Result := TWPTextFragmentList;
End;

End.

