Unit wp_html;


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
  SysUtils, Vcl.Graphics,
  fsl_utilities, fsl_stream, fsl_collections, fsl_html,
  wp_graphics, wp_types, wp_document, wp_working, wp_format;


Type
  TWPHTMLReaderAllowedItem = (WPHTMLReaderAllowedItemSection, WPHTMLReaderAllowedItemParagraph,
    WPHTMLReaderAllowedItemField, WPHTMLReaderAllowedItemText, WPHTMLReaderAllowedItemImage,
    WPHTMLReaderAllowedItemLineBreak, WPHTMLReaderAllowedItemLine,
    WPHTMLReaderAllowedItemTable, WPHTMLReaderAllowedItemRow, WPHTMLReaderAllowedItemCell);

  TWPHTMLReaderAllowedItems = Set Of TWPHTMLReaderAllowedItem;

  TWPHTMLReader = Class (TWPReader)
    Private
      FDom : TFslHtmlDocument;
      FContextStack : TWPStyleStack;
      FFontScale : Real;
      FPreformatted : Boolean;

      Function ReadEnumeratedAttribute(Const sValue: String; Const aValues: Array Of String; Const aDefault : Byte): Byte;
      Function ReadTristateAttribute(Const sValue: String): TWPSTristate;
      Function GetStyleValue(oCSS : TFslCSSFragment; Const sKey: String): String; Overload;
      Function GetStyleValue(oCSS : TFslCSSFragment; Const sKey, aDefault: String): String; Overload;
      Function HasHotspot(oCSS: TFslCSSFragment): Boolean;
      Procedure ReadHotspot(oPiece : TWPWorkingDocumentPiece; oCSS : TFslCSSFragment); Overload;
      Procedure ReadHotspot(oHotspot : TWPHotspot; oCSS : TFslCSSFragment); Overload;
      Procedure ReadBorder(oBorder : TWPBorder; oCSS : TFslCSSFragment; Const sName, sNameExt: String);

      // styles
      Procedure ReadCSSFontInfo(oFont : TWPSFontDetails; Const sStyle : String; oCSS : TFslCSSFragment);
      Procedure ReadCSSParaInfo(oParagraph : TWPSParagraphDetails; Const sStyle : String; oCSS : TFslCSSFragment);
      Procedure ReadCSSEntry(oCSStyle : TFslCSSStyle);
      Procedure ReadCSSStyles;

      Procedure PushStyleContext(oSource : TFslHTMLItem);
      Procedure PopStyleContext;
      Procedure ReadStyleContext(oPiece : TWPWorkingDocumentPiece);
      Procedure ReadStyleContextPara(oParagraph : TWPWorkingDocumentParaPiece);
      Procedure SetUpDefaultStyleContext(oContext : TWPStyle);

      Procedure AddTextPiece(oDocument : TWPWorkingDocument; Const sText : String);
      Procedure ReadTextFragment(oDocument : TWPWorkingDocument; oText : TFslHTMLTextFragment);
      Procedure ReadPreformatted(oDocument : TWPWorkingDocument; oPre : TFslHTMLPreformatted; aAllowed : TWPHTMLReaderAllowedItems);
      Procedure ReadFont(oDocument : TWPWorkingDocument; oSrcFont : TFslHTMLFont; aAllowed : TWPHTMLReaderAllowedItems);
      Procedure ReadBold(oDocument : TWPWorkingDocument; oBold: TFslHTMLBold; aAllowed : TWPHTMLReaderAllowedItems);
      Procedure ReadUnderline(oDocument : TWPWorkingDocument; oUnderline: TFslHTMLUnderline; aAllowed : TWPHTMLReaderAllowedItems);
      Procedure ReadItalic(oDocument : TWPWorkingDocument; oItalic: TFslHTMLItalic; aAllowed : TWPHTMLReaderAllowedItems);
      Procedure ReadEmphasis(oDocument : TWPWorkingDocument; oEmphasis : TFslHTMLEmphasis; aAllowed : TWPHTMLReaderAllowedItems);
      Procedure ReadStrong(oDocument : TWPWorkingDocument; oStrong : TFslHTMLStrong; aAllowed : TWPHTMLReaderAllowedItems);
      Procedure ReadStrikeOut(oDocument : TWPWorkingDocument; oStrikeOut : TFslHTMLStrikeOut; aAllowed : TWPHTMLReaderAllowedItems);
      Procedure ReadSuperScript(oDocument : TWPWorkingDocument; oSuperScript : TFslHTMLSuperScript; aAllowed : TWPHTMLReaderAllowedItems);
      Procedure ReadSubScript(oDocument : TWPWorkingDocument; oSubScript : TFslHTMLSubScript; aAllowed : TWPHTMLReaderAllowedItems);
      Procedure ReadImageReference(oDocument : TWPWorkingDocument; oImg : TFslHTMLImage);
      Procedure ReadAnchor(oDocument : TWPWorkingDocument; oAnchor : TFslHTMLAnchor);

      Procedure ReadHorizontalRule(oDocument : TWPWorkingDocument; oElement : TFslHTMLHorizontalRule);
      Procedure ReadLineBreak(oDocument : TWPWorkingDocument; oElement : TFslHTMLBreak);

      (*
      procedure ReadStyleInfo(oSource : TFslHTMLItem; oDest : TWPDNode);
      procedure ReadParaStyleInfo(oSource : TFslHTMLItem; const sStyle : String; oDest : TWPDParagraph);

      // paragraph contents
      Procedure ReadParagraphContent(oDest : TWPDParagraph; oField : TWPDField; oItems : TFslHTMLItems; sStyle : String; oFont : TWPSFontDetails);


      Function ReadTextFragmentAsPara(oText: TFslHTMLTextFragment): TWPDParagraph; Overload; Virtual;
*)
      // paragraphs, lists
      Function ParagraphAllowed(aAllowed : TWPHTMLReaderAllowedItems): Boolean;
      Procedure ReadBlockQuote(oDocument : TWPWorkingDocument; oBlockQuote : TFslHTMLBlockQuote; aAllowed : TWPHTMLReaderAllowedItems);
      Procedure ReadParagraph(oDocument : TWPWorkingDocument; oPara : TFslHTMLSection);
      Procedure ReadHeading(oDocument : TWPWorkingDocument; oPara : TFslHTMLHeading);
      Procedure ReadUnorderedList(oDocument : TWPWorkingDocument; oList : TFslHTMLUnorderedList);
      Procedure ReadUnorderedListItem(oDocument : TWPWorkingDocument; oItem : TFslHTMLListItem; oCSS: TFslCSSFragment);
      Procedure ReadOrderedList(oDocument : TWPWorkingDocument; oList: TFslHTMLOrderedList);

      // section or just normal paragraph/text
      Procedure ReadSection(oDocument : TWPWorkingDocument; aContainer: TFslHTMLContainer; aAllowed : TWPHTMLReaderAllowedItems);

      // fields or just normal paragraph/text
      Procedure ReadSpan(oDocument : TWPWorkingDocument; oSpan: TFslHTMLSpan; aAllowed : TWPHTMLReaderAllowedItems);
      Procedure ReadSpanAsField(oDocument : TWPWorkingDocument; oSpan: TFslHTMLSpan);

      // tables
      Procedure ReadTable(oDocument: TWPWorkingDocument; oTable: TFslHTMLTable);
      Procedure ReadTableRow(oDocument : TWPWorkingDocument; oRow: TFslHTMLTableRow);
      Procedure ReadTableCell(oDocument : TWPWorkingDocument; oCell: TFslHTMLTableCell);
      Function ReadRowsInImplicitTable(oDocument : TWPWorkingDocument; oSource: TFslHTMLItems; Const iIndex: Integer) : Integer;
      Function ReadCellsInImplicitTable(oDocument : TWPWorkingDocument; oSource: TFslHTMLItems; Const iIndex: Integer) : Integer;

      // reading infrastructure
      Procedure ReadContent(oDocument : TWPWorkingDocument; oSource : TFslHTMLItems; aAllowed : TWPHTMLReaderAllowedItems);
      procedure AddParaPiece(oDocument: TWPWorkingDocument);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

  //  procedure Read(oStyles : TWPStyles);

//      Procedure ReadSection(oSection: TWPDSection); Overload; virtual;
      Procedure Read(oDocument : TWPWorkingDocument); Override;
  End;

// extended CSS attributes defined and used:
//
// list-format
// field-mask
// field-vocab
// field-canDelete
// field-canFormat
// wpx-bottom-padding-size
// wpx-bottom-padding-color
// and other wpx (word processor extended attributes)
//


Type
  TWPHTMLWriterEngine = Class (TWPWriter)
  Private
    FFormatter : TFslHTMLFormatter;
    FText : Boolean;

    FCSSStyles : TFslCSSValues;

    FTitle: String;

    FInSpan : Boolean;
    FParaFont : TWPSFontDetails;
    FSpanFont : TWPSFontDetails;
    FParaStyle : String;
    FSpanStyle : String;
    FLeftIndent : Integer;

    FCurrentParaType : TWPSParagraphListType;
    FNoHTMLBody: Boolean;
    FSaveImageHolders : Boolean;
    FFontScale : Real;
    FParaTextUnderline : TWPSTriState;
    FParaTextStrikethrough : TWPSTriState;
    FFieldTextUnderline : TWPSTriState;
    FFieldTextStrikethrough : TWPSTriState;

    Procedure StartText;
    Procedure UseText;
    Procedure CloseText;

    Procedure WriteFormatAttributes(oPiece : TWPWorkingDocumentPiece; bTextDecorations : Boolean);
    Procedure StartParagraphFont(oPara : TWPWorkingDocumentParaPiece);
    Procedure StartSpan(oPiece : TWPWorkingDocumentPiece);
    Function FontFormatMatches(oPiece : TWPWorkingDocumentPiece) : Boolean;
    Procedure CloseSpan;
    Procedure WriteHotspotAttributes(oHotspot : TWPHotspot);

    Procedure ProduceStyle(oStyle : TWPStyle);
    Procedure ProduceStyles;
    Function EncodeStyles(oFont, oStyleFont: TWPSFontDetails; bTextDecorations : Boolean): String;

    Procedure CheckWriteListClose(oParagraph : TWPWorkingDocumentParaPiece);
    Procedure CheckBlockQuoteClose(oParagraph : TWPWorkingDocumentParaPiece);
    Procedure CheckBlockQuoteOpen(oParagraph : TWPWorkingDocumentParaPiece);

    Function StartPlainPara(oParagraph : TWPWorkingDocumentParaPiece) : String;
    Function StartBulleted(oParagraph : TWPWorkingDocumentParaPiece) : String;
    Function StartNUmbered(oParagraph : TWPWorkingDocumentParaPiece) : String;

    Procedure WriteBorderAttributes(Const sName, sExtName : String; oBorder : TWPBorder);

  Protected
    Procedure Initialise; Override;
    Procedure Finalise; Override;

    Procedure WriteText(oText : TWPWorkingDocumentTextPiece); Override;
    Procedure WriteImage(oImage : TWPWorkingDocumentImagePiece); Override;
    Procedure WriteLineBreak(oBreak : TWPWorkingDocumentLineBreakPiece); Override;
    Procedure WriteFieldStart(oField : TWPWorkingDocumentFieldStartPiece); Override;
    Procedure WriteFieldStop(oField : TWPWorkingDocumentFieldStartPiece; oStop : TWPWorkingDocumentFieldStopPiece); Override;
    Procedure WriteParagraphStart(oParagraph : TWPWorkingDocumentParaPiece); Override;
    Procedure WriteParagraphStop(oParagraph : TWPWorkingDocumentParaPiece; bNextIsSection : Boolean; oSection : TWPWorkingDocumentSectionStartPiece); Override;
    Procedure WriteBreak(oBreak : TWPWorkingDocumentBreakPiece); Override;
    Procedure WriteSectionStart(oSection : TWPWorkingDocumentSectionStartPiece); Override;
    Procedure WriteSectionStop(oSection : TWPWorkingDocumentSectionStartPiece; oStop : TWPWorkingDocumentStopPiece); Override;
    Procedure WriteDocumentStart(oDocument : TWPWorkingDocument); Override;
    Procedure WriteDocumentStop(oDocument : TWPWorkingDocument); Override;
    Procedure WriteTableStart(oTable : TWPWorkingDocumentTableStartPiece); Override;
    Procedure WriteTableStop(oTable : TWPWorkingDocumentTableStartPiece; oStop : TWPWorkingDocumentStopPiece); Override;
    Procedure WriteTableRowStart(oTableRow : TWPWorkingDocumentTableRowStartPiece); Override;
    Procedure WriteTableRowStop(oTableRow : TWPWorkingDocumentTableRowStartPiece; oStop : TWPWorkingDocumentStopPiece; bIsLast : Boolean); Override;
    Procedure WriteTableCellStart(oTableCell : TWPWorkingDocumentTableCellStartPiece); Override;
    Procedure WriteTableCellStop(oTableCell : TWPWorkingDocumentTableCellStartPiece; oStop : TWPWorkingDocumentStopPiece); Override;


    Function HTMLStream : TFslStream; Overload; Virtual;
    Function CanSaveImage : Boolean; Overload; Virtual;
    Procedure SaveImage(oBuffer : TFslBuffer; Const sExtension : String; Var sName : String); Overload; Virtual;
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Property Title : String Read FTitle Write FTitle;
    Property NoHTMLBody : Boolean Read FNoHTMLBody Write FNoHTMLBody;
    Property SaveImageHolders : Boolean Read FSaveImageHolders Write FSaveImageHolders;
    Property FontScale : Real Read FFontScale Write FFontScale;
  End;



Type
  TWPHTMLWriter = Class (TWPHTMLWriterEngine)
    Protected
      Function HTMLStream : TFslStream; Override;
      Function CanSaveImage : Boolean; Override;
      Procedure SaveImage(oBuffer : TFslBuffer; Const sExtension : String; Var sName : String); Override;
  End;




Type
  TWPMHTWriter = Class (TWPHTMLWriterEngine)
    Private
      FHTMLStream : TFslStream;
      FPackage : TMimeMessage;

      Procedure SetPackageHeaders;
      Procedure WritePackage;

    Protected
      Procedure Initialise; Override;
      Procedure Finalise; Override;

      Function HTMLStream : TFslStream; Override;
      Function CanSaveImage : Boolean; Override;
      Procedure SaveImage(oBuffer : TFslBuffer; Const sExtension : String; Var sName : String); Override;
    function sizeInBytesV : cardinal; override;
  End;

Implementation

Constructor TWPHTMLReader.Create;
Begin
  Inherited;

  FDom := TFslHtmlDocument.Create;
  FContextStack := TWPStyleStack.Create;
  FFontScale := 1.25;
  FPreformatted := False;
End;


Destructor TWPHTMLReader.Destroy;
Begin
  FContextStack.Free;
  FDom.Free;

  Inherited;
End;


Procedure TWPHTMLReader.Read(oDocument : TWPWorkingDocument);
Var
  oParser : TFslHTMLParser;
Begin
  Inherited;

  FDom.Clear;
  oParser := TFslHTMLParser.Create(Stream.Link);
  Try
    oParser.ConsumeDocument(FDom);
  Finally
    oParser.Free;
  End;

  ReadCSSStyles;

  FContextStack.Clear;

  PushStyleContext(FDom.Body);
  Try
    ReadContent(oDocument, FDom.Body.Items, [WPHTMLReaderAllowedItemSection, WPHTMLReaderAllowedItemParagraph, WPHTMLReaderAllowedItemTable, WPHTMLReaderAllowedItemLine,
          WPHTMLReaderAllowedItemField, WPHTMLReaderAllowedItemText, WPHTMLReaderAllowedItemImage, WPHTMLReaderAllowedItemLineBreak, WPHTMLReaderAllowedItemLine ]);
  Finally
    PopStyleContext;
  End;

  CheckForEmpty(oDocument);
  DoneReading(oDocument);
End;

Function TWPHTMLReader.ParagraphAllowed(aAllowed : TWPHTMLReaderAllowedItems): Boolean;
Begin
  Result := (WPHTMLReaderAllowedItemText In aAllowed) Or (WPHTMLReaderAllowedItemParagraph In aAllowed);
End;

Procedure TWPHTMLReader.ReadContent(oDocument : TWPWorkingDocument; oSource : TFslHTMLItems; aAllowed : TWPHTMLReaderAllowedItems);
Var
  iLoop : Integer;
  oItem : TFslHTMLItem;
  oPiece: TWPWorkingDocumentParaPiece;
  oFieldStart : TWPWorkingDocumentFieldStartPiece;
  oFieldStop : TWPWorkingDocumentFieldStopPiece;
Begin
  iLoop := 0;
  While iLoop < oSource.Count Do
  Begin
    oItem := oSource[iLoop];
    iLoop := iLoop + 1;

    // we simply ignore unexpected content - helps us read non WP content
    Case oItem.ElementType Of
      ahBlockQuote :   If WPHTMLReaderAllowedItemParagraph In aAllowed Then   ReadBlockQuote(oDocument, TFslHTMLBlockQuote(oItem), aAllowed);
      ahParagraph    : If WPHTMLReaderAllowedItemParagraph In aAllowed Then   ReadParagraph(oDocument, TFslHTMLParagraph(oItem));
      ahHeading :      If WPHTMLReaderAllowedItemParagraph In aAllowed Then   ReadHeading(oDocument, TFslHTMLHeading(oItem));
      ahDiv :          If (WPHTMLReaderAllowedItemSection In aAllowed) Or ParagraphAllowed(aAllowed) Then
                         ReadSection(oDocument, TFslHTMLContainer(oItem), aAllowed);
      ahSpan :         If (WPHTMLReaderAllowedItemField In aAllowed) Or ParagraphAllowed(aAllowed) Then
                         ReadSpan(oDocument, TFslHTMLSpan(oItem), aAllowed);

        // we actually don't support nested list, so the result may be slightly wronged
      ahUnOrderedList: If WPHTMLReaderAllowedItemParagraph In aAllowed Then   ReadUnorderedList(oDocument, TFslHTMLUnorderedList(oItem));
      ahOrderedList  : If WPHTMLReaderAllowedItemParagraph In aAllowed Then   ReadOrderedList(oDocument, TFslHTMLOrderedList(oItem));

      // for implicit paragraph
      ahFont :          If ParagraphAllowed(aAllowed) Then ReadFont(oDocument, TFslHTMLFont(oItem), aAllowed);
      ahBold :          If ParagraphAllowed(aAllowed) Then ReadBold(oDocument, TFslHTMLBold(oItem), aAllowed);
      ahItalic :        If ParagraphAllowed(aAllowed) Then ReadItalic(oDocument, TFslHTMLItalic(oItem), aAllowed);
      ahUnderline :     If ParagraphAllowed(aAllowed) Then ReadUnderline(oDocument, TFslHTMLUnderline(oItem), aAllowed);
      ahEmphasis :      If ParagraphAllowed(aAllowed) Then ReadEmphasis(oDocument, TFslHTMLEmphasis(oItem), aAllowed);
      ahStrong :        If ParagraphAllowed(aAllowed) Then ReadStrong(oDocument, TFslHTMLStrong(oItem), aAllowed);
      ahStrikeOut :     If ParagraphAllowed(aAllowed) Then ReadStrikeOut(oDocument, TFslHTMLStrikeOut(oItem), aAllowed);
      ahSuperScript :   If ParagraphAllowed(aAllowed) Then ReadSuperScript(oDocument, TFslHTMLSuperScript(oItem), aAllowed);
      ahSubScript :     If ParagraphAllowed(aAllowed) Then ReadSubScript(oDocument, TFslHTMLSubScript(oItem), aAllowed);
      ahImageReference: If WPHTMLReaderAllowedItemImage In aAllowed Then ReadImageReference(oDocument, TFslHTMLImage(oItem));
      ahPreformatted:   If ParagraphAllowed(aAllowed) Then ReadPreformatted(oDocument, TFslHTMLPreformatted(oItem), aAllowed);
      // page break and line break
      ahHorizontalRule : If WPHTMLReaderAllowedItemLine In aAllowed Then      ReadHorizontalRule(oDocument, TFslHTMLHorizontalRule(oItem));
      ahBreak:           If WPHTMLReaderAllowedItemLineBreak In aAllowed Then ReadLineBreak(oDocument, TFslHTMLBreak(oItem));
      ahAnchor:          If WPHTMLReaderAllowedItemField In aAllowed Then     ReadAnchor(oDocument, TFslHTMLAnchor(oItem));
      // table, row, cell
      ahTable:        If WPHTMLReaderAllowedItemTable In aAllowed Then      ReadTable(oDocument, TFslHTMLTable(oItem));
      ahTableRow:     If WPHTMLReaderAllowedItemTable In aAllowed Then      iLoop := ReadRowsInImplicitTable(oDocument, oSource, iLoop - 1);
      ahTableCell:    If WPHTMLReaderAllowedItemTable In aAllowed Then      iLoop := ReadCellsInImplicitTable(oDocument, oSource, iLoop - 1);

{      ahEntity : AddToPara(oParagraph, ReadEntity(oItem));

      {      ahTextFragment : ReadTextFragmentAsPara(TFslHTMLTextFragment(oItem));}
//      ahHeading : oDest.Blocks.Add(ReadParagraph(oDocument, TFslHTMLParagraph(oItem));

      ahListItem: // implicit unordered list
        If WPHTMLReaderAllowedItemParagraph In aAllowed Then
          ReadUnorderedListItem(oDocument, TFslHTMLListItem(oItem), Nil);
      ahTextFragment: // normal text fragment, or implicit paragraph, field
        If (WPHTMLReaderAllowedItemText In aAllowed) Then
          ReadTextFragment(oDocument, TFslHTMLTextFragment(oItem))
        Else If (WPHTMLReaderAllowedItemParagraph In aAllowed) Then
        Begin
          // implicit paragraph
          ReadTextFragment(oDocument, TFslHTMLTextFragment(oItem));

          oPiece := TWPWorkingDocumentParaPiece.Create;
          Try
            oPiece.SpeechMagicDouble := SpeechMagicDouble;
            ReadStyleContext(oPiece);
            ReadStyleContextPara(oPiece);
            oDocument.Pieces.Add(oPiece.Link);
          Finally
            oPiece.Free;
          End;
        End
        Else If (WPHTMLReaderAllowedItemField In aAllowed) Then
        Begin
          // implicitly wrap the text in <span>
          oFieldStart := TWPWorkingDocumentFieldStartPiece.Create;
          Try
            oDocument.Pieces.Add(oFieldStart.Link);

            ReadTextFragment(oDocument, TFslHTMLTextFragment(oItem));

            oFieldStop := TWPWorkingDocumentFieldStopPiece.Create;
            Try
              oDocument.Pieces.Add(oFieldStop.Link);
            Finally
              oFieldStop.Free;
            End;
          Finally
            oFieldStart.Free;
          End;
        End;

      Else
      // ignore it....
    End;
  End;
End;


Procedure TWPHTMLReader.ReadParagraph(oDocument : TWPWorkingDocument; oPara: TFslHTMLSection);
Var
  oPiece : TWPWorkingDocumentParaPiece;
Begin
  PushStyleContext(oPara);
  Try
    ReadContent(oDocument, oPara.Items, [WPHTMLReaderAllowedItemField, WPHTMLReaderAllowedItemText, {WPNativeReaderAllowedItemImage,} WPHTMLReaderAllowedItemLineBreak]);

    oPiece := TWPWorkingDocumentParaPiece.Create;
    Try
      oPiece.SpeechMagicDouble := SpeechMagicDouble;
      ReadStyleContext(oPiece);
      ReadStyleContextPara(oPiece);

      oDocument.Pieces.Add(oPiece.Link);
    Finally
      oPiece.Free;
    End;
  Finally
    PopStyleContext;
  End;
End;

Procedure TWPHTMLReader.ReadHeading(oDocument : TWPWorkingDocument; oPara: TFslHTMLHeading);
Var
  oPiece : TWPWorkingDocumentParaPiece;
Begin
  PushStyleContext(oPara);
  Try
    FContextStack.Peek.Font.Bold := tsTrue;
    
    ReadContent(oDocument, oPara.Items, [WPHTMLReaderAllowedItemField, WPHTMLReaderAllowedItemText, {WPNativeReaderAllowedItemImage,} WPHTMLReaderAllowedItemLineBreak]);

    oPiece := TWPWorkingDocumentParaPiece.Create;
    Try
      oPiece.SpeechMagicDouble := SpeechMagicDouble;
      ReadStyleContext(oPiece);
      ReadStyleContextPara(oPiece);
      oPiece.Font.Bold := tsTrue;

      oDocument.Pieces.Add(oPiece.Link);
    Finally
      oPiece.Free;
    End;
  Finally
    PopStyleContext;
  End;
End;

Procedure TWPHTMLReader.ReadUnorderedList(oDocument : TWPWorkingDocument; oList: TFslHTMLUnorderedList);
Var
  iLoop : Integer;
  oItem : TFslHTMLListItem;
Begin
  For iLoop := 0 To oList.Items.Count - 1 Do
  Begin
    // we simply ignore unexpected content - helps us read non WP content
    Case oList.Items[iLoop].ElementType Of
      ahListItem :
      Begin
        oItem := TFslHTMLListItem(oList.Items[iLoop]);
        PushStyleContext(oList);
        Try
          ReadUnorderedListItem(oDocument, oItem, oList.Items[iLoop].Style);
        Finally
          PopStyleContext;
        End;
      End
      Else
        // ignore it....
    End;
  End;
End;


Procedure TWPHTMLReader.ReadUnorderedListItem(oDocument: TWPWorkingDocument; oItem: TFslHTMLListItem; oCSS: TFslCSSFragment);
Var
  oPiece : TWPWorkingDocumentParaPiece;
Begin
  ReadContent(oDocument, oItem.Items, [WPHTMLReaderAllowedItemField, WPHTMLReaderAllowedItemText]);

  oPiece := TWPWorkingDocumentParaPiece.Create;
  Try
    oPiece.SpeechMagicDouble := SpeechMagicDouble;
    ReadStyleContext(oPiece);
    ReadStyleContextPara(oPiece);
    If oCSS <> Nil Then
      ReadCSSParaInfo(oPiece.Format , oPiece.Style, oCSS);
    oPiece.Format.ListType := WPSParagraphListTypeBullets;

    oDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;


Procedure TWPHTMLReader.ReadOrderedList(oDocument : TWPWorkingDocument; oList: TFslHTMLOrderedList);
Var
  iLoop : Integer;
  oPiece : TWPWorkingDocumentParaPiece;
  oItem : TFslHTMLListItem;
Begin
  For iLoop := 0 To oList.Items.Count - 1 Do
  Begin
    // we simply ignore unexpected content - helps us read non WP content
    Case oList.Items[iLoop].ElementType Of
      ahListItem :
      Begin
        oItem := TFslHTMLListItem(oList.Items[iLoop]);
        PushStyleContext(oList);
        Try
          ReadContent(oDocument, oItem.Items, [WPHTMLReaderAllowedItemField, WPHTMLReaderAllowedItemText]);

          oPiece := TWPWorkingDocumentParaPiece.Create;
          Try
            oPiece.SpeechMagicDouble := SpeechMagicDouble;
            ReadStyleContext(oPiece);
            ReadStyleContextPara(oPiece);
            ReadCSSParaInfo(oPiece.Format , oPiece.Style, oList.Items[iLoop].Style);
            oPiece.Format.ListType := WPSParagraphListTypeNumbers;
            If oItem.HasValue Then
              oPiece.Format.FixedNumber := oItem.Value;

            oDocument.Pieces.Add(oPiece.Link);
          Finally
            oPiece.Free;
          End;
        Finally
          PopStyleContext;
        End;
      End
      Else
        // ignore it....
    End;
  End;
End;

Procedure TWPHTMLReader.ReadBlockQuote(oDocument : TWPWorkingDocument; oBlockQuote : TFslHTMLBlockQuote; aAllowed : TWPHTMLReaderAllowedItems);
Begin
  PushStyleContext(oBlockQuote);
  Try
    If FContextStack.Peek.Paragraph.LeftIndent = DEF_WORD Then
      FContextStack.Peek.Paragraph.LeftIndent := 1
    Else
      FContextStack.Peek.Paragraph.LeftIndent := FContextStack.Peek.Paragraph.LeftIndent + 1;
    ReadContent(oDocument, oBlockQuote.Items, aAllowed);
  Finally
    PopStyleContext;
  End;
End;


Procedure TWPHTMLReader.ReadSection(oDocument : TWPWorkingDocument; aContainer: TFslHTMLContainer; aAllowed : TWPHTMLReaderAllowedItems);
Var
  oSection : TWPWorkingDocumentSectionStartPiece;
  oStop    : TWPWorkingDocumentStopPiece;
Begin
  PushStyleContext(aContainer);
  Try
    If (WPHTMLReaderAllowedItemSection In aAllowed) Then
    Begin
      // section start
      oSection := TWPWorkingDocumentSectionStartPiece.Create;
      Try
        oSection.NamePair := aContainer.Id;
        oSection.DisplayName := aContainer.TitleAttribute;
        // read from style: DisplayType, ReadOnly, Deletable, IsField, Key
        oSection.Deletable := StringToBoolean(GetStyleValue(aContainer.Style, CSS_ATTR_DELETABLE));
        oSection.RawDataAsText := GetStyleValue(aContainer.Style, CSS_ATTR_DATA);
        oSection.IsField := StringToBoolean(GetStyleValue(aContainer.Style, CSS_ATTR_ISFIELD));
        oSection.ReadOnly := ReadTristateAttribute(GetStyleValue(aContainer.Style, CSS_ATTR_READONLY));
        oSection.Key := GetStyleValue(aContainer.Style, CSS_ATTR_KEY);
        oSection.DisplayType := TWPWorkingDocumentSectionDisplayType(ReadEnumeratedAttribute(GetStyleValue(aContainer.Style, CSS_ATTR_DISPLAYTYPE), NAMES_WPWorkingDocumentSectionDISPLAYTYPE, ord(sdtNone)));

        ReadStyleContext(oSection);  // Font ?
        oDocument.Pieces.Add(oSection.Link);

        // nested children
        ReadContent(oDocument, aContainer.Items, [WPHTMLReaderAllowedItemParagraph, WPHTMLReaderAllowedItemSection, WPHTMLReaderAllowedItemTable, WPHTMLReaderAllowedItemField, WPHTMLReaderAllowedItemText, WPHTMLReaderAllowedItemImage]);

        // secton stop
        oStop := TWPWorkingDocumentStopPiece.Create(stSection);
        Try
          oStop.AssignStyle(oSection);
          oDocument.Pieces.Add(oStop.Link);
        Finally
          oStop.Free;
        End;
      Finally
        oSection.Free;
      End;
    End
    Else
      ReadContent(oDocument, aContainer.Items, aAllowed);
  Finally
    PopStyleContext;
  End;
End;


Procedure TWPHTMLReader.ReadBold(oDocument : TWPWorkingDocument; oBold: TFslHTMLBold; aAllowed : TWPHTMLReaderAllowedItems);
Begin
  PushStyleContext(oBold);
  Try
    FContextStack.Peek.Font.Bold := tsTrue;
    ReadContent(oDocument, oBold.Items, aAllowed);
  Finally
    PopStyleContext;
  End;
End;

Procedure TWPHTMLReader.ReadItalic(oDocument : TWPWorkingDocument; oItalic: TFslHTMLItalic; aAllowed : TWPHTMLReaderAllowedItems);
Begin
  PushStyleContext(oItalic);
  Try
    FContextStack.Peek.Font.Italic := tsTrue;
    ReadContent(oDocument, oItalic.Items, aAllowed);
  Finally
    PopStyleContext;
  End;
End;

Procedure TWPHTMLReader.ReadUnderline(oDocument : TWPWorkingDocument; oUnderline: TFslHTMLUnderline; aAllowed : TWPHTMLReaderAllowedItems);
Begin
  PushStyleContext(oUnderline);
  Try
    FContextStack.Peek.Font.Underline := tsTrue;
    ReadContent(oDocument, oUnderline.Items, aAllowed);
  Finally
    PopStyleContext;
  End;
End;

Procedure TWPHTMLReader.ReadEmphasis(oDocument : TWPWorkingDocument; oEmphasis: TFslHTMLEmphasis; aAllowed : TWPHTMLReaderAllowedItems);
Begin
  PushStyleContext(oEmphasis);
  Try
    FContextStack.Peek.Font.Italic := tsTrue;
    ReadContent(oDocument, oEmphasis.Items, aAllowed);
  Finally
    PopStyleContext;
  End;
End;

Procedure TWPHTMLReader.ReadStrong(oDocument : TWPWorkingDocument; oStrong: TFslHTMLStrong; aAllowed : TWPHTMLReaderAllowedItems);
Begin
  PushStyleContext(oStrong);
  Try
    FContextStack.Peek.Font.Bold := tsTrue;
    ReadContent(oDocument, oStrong.Items, aAllowed);
  Finally
    PopStyleContext;
  End;
End;

Procedure TWPHTMLReader.ReadStrikeout(oDocument : TWPWorkingDocument; oStrikeout: TFslHTMLStrikeout; aAllowed : TWPHTMLReaderAllowedItems);
Begin
  PushStyleContext(oStrikeout);
  Try
    FContextStack.Peek.Font.Strikethrough := tsTrue;
    ReadContent(oDocument, oStrikeout.Items, aAllowed);
  Finally
    PopStyleContext;
  End;
End;

Procedure TWPHTMLReader.ReadSuperscript(oDocument : TWPWorkingDocument; oSuperscript: TFslHTMLSuperscript; aAllowed : TWPHTMLReaderAllowedItems);
Begin
  PushStyleContext(oSuperscript);
  Try
    FContextStack.Peek.Font.State := fsSuperscript;
    ReadContent(oDocument, oSuperscript.Items, aAllowed);
  Finally
    PopStyleContext;
  End;
End;

Procedure TWPHTMLReader.ReadSubscript(oDocument : TWPWorkingDocument; oSubscript: TFslHTMLSubscript; aAllowed : TWPHTMLReaderAllowedItems);
Begin
  PushStyleContext(oSubscript);
  Try
    FContextStack.Peek.Font.State := fsSubscript;
    ReadContent(oDocument, oSubscript.Items, aAllowed);
  Finally
    PopStyleContext;
  End;
End;

Procedure TWPHTMLReader.ReadFont(oDocument : TWPWorkingDocument; oSrcFont : TFslHTMLFont; aAllowed : TWPHTMLReaderAllowedItems);
Begin
  PushStyleContext(oSrcFont);
  Try
    If oSrcFont.Color <> '' Then
      FContextStack.Peek.Font.Foreground := HTMLColourStringToColour(oSrcFont.Color);
    If oSrcFont.Face <> '' Then
      FContextStack.Peek.Font.Name := oSrcFont.Face;
    If oSrcFont.Size <> '' Then
      FContextStack.Peek.Font.Size := StrToIntDef(oSrcFont.Size, FContextStack.Peek.Font.Size);
    ReadContent(oDocument, oSrcFont.Items, aAllowed);
  Finally
    PopStyleContext;
  End;
End;

Procedure TWPHTMLReader.AddParaPiece(oDocument : TWPWorkingDocument);
Var
  oPara : TWPWorkingDocumentParaPiece;
Begin
  oPara := TWPWorkingDocumentParaPiece.Create;
  Try
    ReadStyleContext(oPara);
    oDocument.Pieces.Add(oPara.Link);
  Finally
    oPara.Free;
  End;
End;

Procedure TWPHTMLReader.AddTextPiece(oDocument : TWPWorkingDocument; Const sText : String);
Var
  oText : TWPWorkingDocumentTextPiece;
  sWorkingText : String;
Begin
  sWorkingText := sText;

  While Length(sWorkingText) > MAX_WORD_LENGTH Do
  Begin
    AddTextPiece(oDocument, Copy(sText, 1, MAX_WORD_LENGTH));
    sWorkingText := Copy(sWorkingText, MAX_WORD_LENGTH + 1, MAXINT);
  End;

  oText := TWPWorkingDocumentTextPiece.Create;
  Try
    ReadStyleContext(oText);
    oText.Content := sWorkingText;
    oDocument.Pieces.Add(oText.Link);
  Finally
    oText.Free;
  End;
End;

Procedure TWPHTMLReader.ReadTextFragment(oDocument : TWPWorkingDocument; oText : TFslHTMLTextFragment);
var
  s : String;
  i, j : integer;
Begin
  if FPreformatted then
  Begin
    s := StringReplace(oText.Text, [#9], ' ');
    i := 1;
    While (i <= length(s)) Do
    Begin
      if CharInSet(s[i], ['A'..'Z', 'a'..'z', '0'..'9']) Then
      Begin
        j := i;
        While (j < length(s)) and CharInSet(s[j+1], ['A'..'Z', 'a'..'z', '0'..'9']) Do
          inc(j);
        AddTextPiece(oDocument, copy(s, i, j-i +1));
        i := j + 1;
      End
      Else if CharInSet(s[i], [#13, #10]) Then
      Begin
        AddParaPiece(oDocument);
        if (s[i] = #13) And (i < length(s)) And (s[i+1] = #10) Then
          inc(i, 2)
        Else
          inc(i);
      End
      Else
      Begin
        AddTextPiece(oDocument, s[i]);
        inc(i);
      End;
    End;
  End
  Else
  Begin
    Splitter.Init(StringReplace(StringReplace(oText.Text, cReturn, ' '), [#9], ' '));
    While Splitter.more Do
      AddTextPiece(oDocument, Splitter.Next);
  End;
End;

(*
function TWPHTMLReader.ReadSection(oSource: TFslHTMLSection): TWPDSection;
begin
  result := TWPDSection.create;
  try
    result.Name := oSource.Id;
    result.DisplayName := oSource.TitleAttribute;
    ReadSection(oSource, result);
    result.Link;
  finally
    result.free;
  end;
end;



Function TWPHTMLReader.ReadTextFragmentAsPara(oText: TFslHTMLTextFragment) : TWPDParagraph;
Begin
  Result := TWPDParagraph.Create;
  Try
    ReadStyleInfo(oText, result);
    ReadTextFragment(Result, nil, oText, '', Nil);
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Procedure TWPHTMLReader.ReadParagraphContent(oDest : TWPDParagraph; oField : TWPDField; oItems : TFslHTMLItems; sStyle : String; oFont : TWPSFontDetails);
Var
  iLoop : Integer;
  oItem : TFslHTMLItem;
Begin
  For iLoop := 0 To oItems.count - 1 Do
  Begin
    oItem := oItems[iLoop];
    Case oItem.ElementType Of
//       ahBreak,
    Else
    End;
  End;
End;


Procedure TWPHTMLReader.ReadBold(oParagraph: TWPDParagraph; oField : TWPDField; oBold: TFslHTMLBold; sStyle: String; oFont: TWPSFontDetails);
var
  oNew : TWPSFontDetails;
Begin
  oNew := TWPSFontDetails.Create;
  try
    oNew.Assign(oFont);
    oNew.Bold := tsTrue;
    ReadParagraphContent(oParagraph, oField, oBold.Items, sStyle, oNew);
  finally
    oNew.Free;
  end;
End;


Procedure TWPHTMLReader.ReadUnderline(oParagraph: TWPDParagraph; oField : TWPDField; oUnderline: TFslHTMLUnderline; sStyle: String; oFont: TWPSFontDetails);
var
  oNew : TWPSFontDetails;
Begin
  oNew := TWPSFontDetails.Create;
  try
    oNew.Assign(oFont);
    oNew.Underline := tsTrue;
    ReadParagraphContent(oParagraph, oField, oUnderline.Items, sStyle, oNew);
  finally
    oNew.Free;
  end;
End;


Procedure TWPHTMLReader.ReadItalic(oParagraph: TWPDParagraph; oField : TWPDField; oItalic: TFslHTMLItalic; sStyle: String; oFont: TWPSFontDetails);
var
  oNew : TWPSFontDetails;
Begin
  oNew := TWPSFontDetails.Create;
  try
    oNew.Assign(oFont);
    oNew.Italic := tsTrue;
    ReadParagraphContent(oParagraph, oField, oItalic.Items, sStyle, oNew);
  finally
    oNew.Free;
  end;
End;

*)

Procedure TWPHTMLReader.ReadHorizontalRule(oDocument : TWPWorkingDocument; oElement : TFslHTMLHorizontalRule);
Var
  oBreak : TWPWorkingDocumentBreakPiece;
  oCSS : TFslCSSFragment;
  sValue : String;
Begin
  PushStyleContext(oElement);
  Try
    oBreak := TWPWorkingDocumentBreakPiece.Create;
    Try

      Case oElement.Align Of
        haLeft:   oBreak.Alignment := WordProcessorAlignmentLeft;
        haCenter: oBreak.Alignment := WordProcessorAlignmentCentre;
        haRight:  oBreak.Alignment := WordProcessorAlignmentRight;
        Else      oBreak.Alignment := WordProcessorAlignmentUnknown;
      End;

      oCSS := oElement.Style;

      // height
      sValue := StringTrimWhitespace(GetStyleValue(oCSS, CSS_ATTR_HEIGHT));
      If StringIsInteger32(sValue) Then
        oBreak.PenWidth := StrToIntDef(sValue, 0)
      Else If (Length(sValue) > 2) And (Copy(sValue, Length(sValue) - 1, 2) = 'px') Then
        oBreak.PenWidth := StrToIntDef(Copy(sValue, 1, Length(sValue) - 2), 0);

      // width
      sValue := StringTrimWhitespace(GetStyleValue(oCSS, CSS_ATTR_WIDTH));
      If StringIsInteger32(sValue) Then
        oBreak.Width := StrToIntDef(sValue, 0)
      Else If (Length(sValue) > 2) And (Copy(sValue, Length(sValue), 1) = '%') Then
        oBreak.Width := StrToIntDef(Copy(sValue, 1, Length(sValue) - 1), 0);
      oBreak.Width := oBreak.Width / 100;

      oBreak.PenColour := XMLColourStringToColourOrDefault(GetStyleValue(oCSS, CSS_ATTR_BACKGROUND), DEF_COLOUR);
      oBreak.PenStyle := TFslPenStyle(ReadEnumeratedAttribute(GetStyleValue(oCSS, CSS_ATTR_PENSTYLE), ADVPENSTYLE_CODES, ord(apsSolid)));
      oBreak.EndStyle := TFslPenEndStyle(ReadEnumeratedAttribute(GetStyleValue(oCSS, CSS_ATTR_PENENDSTYLE), ADVPENENDSTYLE_CODES, ord(apesRound)));
      oBreak.BreakType := TWPWorkingDocumentBreakPieceType(ReadEnumeratedAttribute(GetStyleValue(oCSS, CSS_ATTR_BREAKTYPE), WPWorkingDocumentBreakPIECETYPE_NAMES, ord(btLine)));

      ReadStyleContext(oBreak);
      oDocument.Pieces.Add(oBreak.Link);
    Finally
      oBreak.Free;
    End;
  Finally
    PopStyleContext;
  End;
End;

Procedure TWPHTMLReader.ReadImageReference(oDocument : TWPWorkingDocument; oImg: TFslHTMLImage);
Var
  oImage : TWPWorkingDocumentImagePiece;
  oBuffer : TFslBuffer;
Begin
  oImage := TWPWorkingDocumentImagePiece.Create;
  Try
    oImage.Border := StrToIntDef(GetStyleValue(oImg.Style, 'border-width'), 0);
    If oImg.Style.Values.ExistsByKey('border-color') Then
      oImage.BorderColour := HTMLColourStringToColour(oImg.Style.Values.Matches['border-color']);
    If oImg.Style.Values.ExistsByKey('transparent-color') Then
      oImage.TransparentColour := HTMLColourStringToColour(oImg.Style.Values.Matches['transparent-color']);
    oImage.Name := oImg.Source;
    ReadStyleContext(oImage);

    Assert(CheckCondition(Assigned(OnLoadImage), 'ReadImage', 'No Image Load Event'));
    OnLoadImage(Self, Context, oImage.Name, oBuffer);
    Try
      LoadImage(oImage, oBuffer, ifUnknown, False);
    Finally
      oBuffer.Free;
    End;

    oImage.Height := StrToIntDef(oImg.Height, oImage.Image.Height);
    oImage.Width := StrToIntDef(oImg.Width, oImage.Image.Width);

    oDocument.Pieces.Add(oImage.Link);
  Finally
    oImage.Free;
  End;
End;

(*
Procedure TWPHTMLReader.ReadFont(oParagraph: TWPDParagraph; oField : TWPDField; oSrcFont: TFslHTMLFont; sStyle: String; oFont: TWPSFontDetails);
var
  oNew : TWPSFontDetails;
Begin
  oNew := TWPSFontDetails.Create;
  try
    oNew.Assign(oFont);
    if oSrcFont.Classes.Count > 0 then
      begin
      sStyle := oSrcFont.Classes.AsCSV;
      if not Styles.ExistsByName(sStyle) then
        sStyle := Styles.DefaultStyle;
      end;
    ReadCSSFontInfo(oNew, sStyle, oSrcFont.Style);
    ReadParagraphContent(oParagraph, oField, oSrcFont.Items, sStyle, oNew);
  finally
    oNew.Free;
  end;
End;

Procedure TWPHTMLReader.ReadField(oParagraph : TWPDParagraph; oField : TWPDField; oAnchor : TFslHTMLAnchor; sStyle : String; oFont : TWPSFontDetails);
var
  oNew : TWPSFontDetails;
  oFld : TWPDField;
Begin
  Assert(CheckCondition(not assigned(oField), 'ReadField', 'Fields cannot contain fields'));

  oNew := TWPSFontDetails.Create;
  try
    if oAnchor.Classes.Count > 0 then
      begin
      sStyle := oAnchor.Classes.AsCSV;
      if not Styles.ExistsByName(sStyle) then
        sStyle := Styles.DefaultStyle;
      end;
    ReadCSSFontInfo(oNew, sStyle, oAnchor.Style);

    oFld := TWPDField.create;
    try
      oFld.Name := oAnchor.Name;
      oFld.URL := oAnchor.URL;
      oFld.HotKey := oAnchor.AccessKey;
      if oAnchor.Style.Values.ExistsByKey('field-mask') then
        oFld.Mask := oAnchor.Style.Values['field-mask'];
      if oAnchor.Style.Values.ExistsByKey('field-vocab') then
        oFld.Vocabulary := oAnchor.Style.Values['field-vocab'];
      if oAnchor.Style.Values.ExistsByKey('field-canDelete') then
        oFld.Deletable := ToBoolean(oAnchor.Style.Values['field-canDelete']);
      if oAnchor.Style.Values.ExistsByKey('field-canFormat') then
        oFld.FixedFormat := not ToBoolean(oAnchor.Style.Values['field-canFormat']);
      ReadParagraphContent(nil, oFld, oAnchor.Items, sStyle, oNew);
      oParagraph.Pieces.Add(oFld.Link);
    finally
      oFld.Free;
    end;
  finally
    oNew.Free;
  end;
end;


*)

Procedure TWPHTMLReader.ReadCSSStyles;
Var
  oCSS : TFslCSSStyles;
  iLoop : Integer;
Begin
  // if a style exists, we will go with the existing style
  // if it's new, then we will add it
  oCSS := FDom.Styles;
  For iLoop := 0 To oCSS.count - 1 Do
    ReadCSSEntry(oCSS[iLoop]);
End;


Procedure TWPHTMLReader.ReadCSSEntry(oCSStyle: TFslCSSStyle);
Var
  oStyle : TWPStyle;
  sName : String;
Begin
  sName := oCSStyle.Name;
  If (sName <> '') And (sName[1] = '.') Then
    Begin
    Delete(sName, 1, 1);
    If Not Styles.ExistsByName(sName) Then
      Begin
      oStyle := TWPStyle.Create;
      Try
        oStyle.Name := sName;
        oStyle.Font.Foreground := DEF_COLOUR;
        oStyle.Font.Background := DEF_COLOUR;
        ReadCSSFontInfo(oStyle.Font, '', oCSStyle);
        ReadCSSParaInfo(oStyle.Paragraph, '', oCSStyle);

        Styles.Add(oStyle.Link);
      Finally
        oStyle.Free;
      End;
      End;
    End;
End;

Procedure TWPHTMLReader.ReadCSSFontInfo(oFont: TWPSFontDetails; Const sStyle : String; oCSS: TFslCSSFragment);
Begin
  If Styles.ExistsByName(sStyle) Then
    oFont.Assign(Styles.GetByName(sStyle).Font);

  If oCSS.Values.ExistsByKey(CSS_ATTR_FONTNAME) Then
    oFont.Name := StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_FONTNAME]);

  If oCSS.Values.ExistsByKey(CSS_ATTR_FONTSIZE) Then
  Begin
    If StringIsInteger32(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_FONTSIZE])) Then
      oFont.Size := StringToInteger32(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_FONTSIZE]))
    Else If (Copy(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_FONTSIZE]), Length(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_FONTSIZE])) - 1, 2) = 'px') Then
      oFont.Size := StringToInteger32(Copy(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_FONTSIZE]), 1, Length(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_FONTSIZE]))-2));
    oFont.Size := Round(oFont.Size / FFontScale);
  End;

  If oCSS.Values.ExistsByKey('font-weight') And StringEquals(StringTrimWhitespace(oCSS.Values.Matches['font-weight']), 'bold') Then
    oFont.Bold := tsTrue;

  If oCSS.Values.ExistsByKey('font-style') And StringEquals(StringTrimWhitespace(oCSS.Values.Matches['font-style']), 'italic') Then
    oFont.Italic := tsTrue;

  If oCSS.Values.ExistsByKey('text-decoration') And StringEquals(StringTrimWhitespace(oCSS.Values.Matches['text-decoration']), 'underline') Then
    oFont.Underline := tsTrue;

  If oCSS.Values.ExistsByKey('text-decoration') And StringEquals(StringTrimWhitespace(oCSS.Values.Matches['text-decoration']), 'line-through') Then
    oFont.Strikethrough := tsTrue;

  If oCSS.Values.ExistsByKey('text-transform') Then
  Begin
    If StringEquals(StringTrimWhitespace(oCSS.Values.Matches['text-transform']), 'uppercase') Then
      oFont.Capitalization := fcsAllCaps
    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches['text-transform']), 'lowercase') Then
      oFont.Capitalization := fcsNoCaps
    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches['text-transform']), 'none') Then
      oFont.Capitalization := fcsNormal
    Else
      oFont.Capitalization := fcsUnknown;
  End;


  If oCSS.Values.ExistsByKey('vertical-align') Then
  Begin
    If StringEquals(StringTrimWhitespace(oCSS.Values.Matches['vertical-align']), 'super') Then
      oFont.State := fsSuperscript
    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches['vertical-align']), 'sub') Then
      oFont.State := fsSubscript;
  End;

  If oCSS.Values.ExistsByKey(CSS_ATTR_FOREGROUND) Then
    oFont.Foreground := HTMLColourStringToColour(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_FOREGROUND]));

  If oCSS.Values.ExistsByKey(CSS_ATTR_BACKGROUND) Then
    oFont.Background := HTMLColourStringToColour(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_BACKGROUND]));
End;


Procedure TWPHTMLReader.ReadCSSParaInfo(oParagraph : TWPSParagraphDetails; Const sStyle : String; oCSS: TFslCSSFragment);
Begin
  oParagraph.Clear;

  If Styles.ExistsByName(sStyle) Then
    oParagraph.Assign(Styles.GetByName(sStyle).Paragraph);

  If oCSS.Values.ExistsByKey(CSS_ATTR_ALIGN) Then
  Begin
    If StringEquals(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_ALIGN]), 'left') Then
      oParagraph.Align := WordProcessorParagraphAlignmentLeft
    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_ALIGN]), 'center') Then
      oParagraph.Align := WordProcessorParagraphAlignmentCentre
    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_ALIGN]), 'right') Then
      oParagraph.Align := WordProcessorParagraphAlignmentRight
    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_ALIGN]), 'justify') Then
      oParagraph.Align := WordProcessorParagraphAlignmentJustify
  End;

  If oCSS.Values.ExistsByKey(CSS_ATTR_LISTTYPE) Then
  Begin
    If StringEquals(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_LISTTYPE]), 'disc') Then
      oParagraph.BulletType := tbDisc
    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_LISTTYPE]), 'square') Then
      oParagraph.BulletType := tbSquare
    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_LISTTYPE]), 'circle') Then
      oParagraph.BulletType := tbCircle

    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_LISTTYPE]), 'decimal') Then
      oParagraph.NumberType := tnArabic
    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_LISTTYPE]), 'lower-alpha') Then
      oParagraph.NumberType := tnLowerAlpha
    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_LISTTYPE]), 'upper-alpha') Then
      oParagraph.NumberType := tnUpperAlpha
    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_LISTTYPE]), 'lower-roman') Then
      oParagraph.NumberType := tnLowerRoman
    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches[CSS_ATTR_LISTTYPE]), 'upper-roman') Then
      oParagraph.NumberType := tnUpperRoman
  End;

  If oCSS.Values.ExistsByKey('list-format') Then
  Begin
    If StringEquals(StringTrimWhitespace(oCSS.Values.Matches['list-format']), 'Dot') Then
      oParagraph.NumberFormat := nwDot
    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches['list-format']), 'Slash') Then
      oParagraph.NumberFormat := nwSlash
    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches['list-format']), 'Parenthesis') Then
      oParagraph.NumberFormat := nwParenthesis
    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches['list-format']), 'Colon') Then
      oParagraph.NumberFormat := nwColon
    Else If StringEquals(StringTrimWhitespace(oCSS.Values.Matches['list-format']), 'SemiColon') Then
      oParagraph.NumberFormat := nwSemiColon
  End;
End;


Procedure TWPHTMLReader.PushStyleContext(oSource : TFslHTMLItem);
Var
  oContext : TWPStyle;
Begin
  oContext := TWPStyle.Create;
  Try
    If Not FContextStack.Empty Then
      oContext.Assign(FContextStack.Peek)
    Else
      SetupDefaultStyleContext(oContext);

    If oSource.Classes.Count > 0 Then
      oContext.Name := oSource.Classes[0];
    ReadCSSFontInfo(oContext.Font, oContext.Name, oSource.Style);

    FContextStack.Push(oContext.Link);
  Finally
    oContext.Free;
  End;
End;


Procedure TWPHTMLReader.PopStyleContext;
Begin
  FContextStack.Pop;
End;


Procedure TWPHTMLReader.ReadStyleContext(oPiece : TWPWorkingDocumentPiece);
Begin
  If Not FContextStack.Empty Then
  Begin
    oPiece.Style := FContextStack.Peek.Name;
    oPiece.Font.Assign(FContextStack.Peek.Font);
  End;
End;


Procedure TWPHTMLReader.ReadStyleContextPara(oParagraph : TWPWorkingDocumentParaPiece);
Begin
  If Not FContextStack.Empty Then
    oParagraph.Format.Assign(FContextStack.Peek.Paragraph);
End;


Procedure TWPHTMLReader.SetupDefaultStyleContext(oContext: TWPStyle);
Begin
  oContext.Name := DEFAULT_STYLE_NAME;
  oContext.Font.Assign(Styles.DefaultStyle.Font);
  oContext.Paragraph.Assign(Styles.DefaultStyle.Paragraph);
End;


Function TWPHTMLReader.ReadEnumeratedAttribute(Const sValue : String; Const aValues : Array Of String; Const aDefault : Byte) : Byte;
Var
  iResult : Integer;
Begin
  iResult := StringArrayIndexOf(aValues, sValue);
  If iResult = -1 Then
    Result := aDefault
  Else
    Result := iResult;
End;


Function TWPHTMLReader.ReadTristateAttribute(Const sValue: String): TWPSTristate;
Begin
  Result := TWPSTriState(ReadEnumeratedAttribute(sValue, NAMES_WPSTRISTATE, Ord(tsUnknown)));
End;

Procedure TWPHTMLReader.ReadSpan(oDocument: TWPWorkingDocument; oSpan: TFslHTMLSpan; aAllowed : TWPHTMLReaderAllowedItems);
Begin
  PushStyleContext(oSpan);
  Try
    If (WPHTMLReaderAllowedItemField In aAllowed) And HasHotspot(oSpan.Style) Then
      ReadSpanAsField(oDocument, oSpan)
    Else If HasHotspot(oSpan.Style) Then
      RaiseError('ReadSpan', 'Field is not allowed here')
    Else If ParagraphAllowed(aAllowed) Then
      ReadContent(oDocument, oSpan.Items, aAllowed);
  Finally
    PopStyleContext;
  End;
End;


Procedure TWPHTMLReader.ReadSpanAsField(oDocument : TWPWorkingDocument; oSpan: TFslHTMLSpan);
Var
  oStart : TWPWorkingDocumentFieldStartPiece;
  oStop : TWPWorkingDocumentFieldStopPiece;
  oCSS: TFslCSSFragment;
Begin
  oStart := TWPWorkingDocumentFieldStartPiece.Create;
  Try
    oStart.NamePair := oSpan.Id;
    oCSS := oSpan.Style;

    oStart.ReadOnly := ReadTristateAttribute(GetStyleValue(oCSS, CSS_ATTR_READONLY));
    oStart.Deletable := StrToBoolDef(GetStyleValue(oCSS, CSS_ATTR_DELETABLE), False);
    oStart.FixedFormat := ReadFixedFormatAttribute(GetStyleValue(oCSS, CSS_ATTR_FIXEDFORMAT));
    oStart.RawDataAsText := GetStyleValue(oCSS, CSS_ATTR_DATA);
    If (GetStyleValue(oCSS, CSS_ATTR_MASK) <> '') Then
      oStart.DataValue['Mask'] := GetStyleValue(oCSS, CSS_ATTR_MASK);
    If (GetStyleValue(oCSS, CSS_ATTR_VOCABULARY) <> '') Then
      oStart.DataValue['List'] := GetStyleValue(oCSS, CSS_ATTR_VOCABULARY);

    ReadHotspot(oStart, oCSS);
    ReadStyleContext(oStart);  // Font ?
    oDocument.Pieces.Add(oStart.Link);

    ReadContent(oDocument, oSpan.Items, [WPHTMLReaderAllowedItemText , WPHTMLReaderAllowedItemLineBreak, WPHTMLReaderAllowedItemImage ]);

    oStop := TWPWorkingDocumentFieldStopPiece.Create;
    Try
      oStop.Style := oStart.Style;
      oStop.Font.Assign(oStart.Font);

      oDocument.Pieces.Add(oStop.Link);
    Finally
      oStop.Free;
    End;
  Finally
    oStart.Free;
  End;
End;


Function TWPHTMLReader.HasHotspot(oCSS: TFslCSSFragment): Boolean;
Begin
  Result := oCSS.Values.ExistsByKey(CSS_ATTR_URL) Or
     oCSS.Values.ExistsByKey(CSS_ATTR_TITLE) Or
     oCSS.Values.ExistsByKey(CSS_ATTR_KEY) Or
     oCSS.Values.ExistsByKey(CSS_ATTR_LINKCOLOUR) Or
     oCSS.Values.ExistsByKey(CSS_ATTR_HOVERCOLOUR);
End;


Procedure TWPHTMLReader.ReadHotspot(oPiece: TWPWorkingDocumentPiece; oCSS: TFslCSSFragment);
Begin
  If HasHotspot(oCSS) Then
  Begin
    oPiece.HasHotspot := True;
    ReadHotspot(oPiece.Hotspot, oCSS);
  End;
End;

Procedure TWPHTMLReader.ReadHotspot(oHotspot : TWPHotspot; oCSS: TFslCSSFragment);
Begin
  oHotspot.URL := DecodePercent(GetStyleValue(oCSS, CSS_ATTR_URL));
  oHotspot.Title := GetStyleValue(oCSS, CSS_ATTR_TITLE);
  oHotspot.Key := GetStyleValue(oCSS, CSS_ATTR_KEY);

  oHotspot.LinkColour := XMLColourStringToColourOrDefault(GetStyleValue(oCSS, CSS_ATTR_LINKCOLOUR), DEF_COLOUR);
  oHotspot.HoverColour := XMLColourStringToColourOrDefault(GetStyleValue(oCSS, CSS_ATTR_HOVERCOLOUR), DEF_COLOUR);
End;

Procedure TWPHTMLReader.ReadLineBreak(oDocument: TWPWorkingDocument; oElement: TFslHTMLBreak);
Var
  oBreak : TWPWorkingDocumentLineBreakPiece;
Begin
  PushStyleContext(oElement);
  Try
    oBreak := TWPWorkingDocumentLineBreakPiece.Create;
    Try
      ReadStyleContext(oBreak);
      oDocument.Pieces.Add(oBreak.Link);
    Finally
      oBreak.Free;
    End;
  Finally
    PopStyleContext;
  End;
End;

Function TWPHTMLReader.GetStyleValue(oCSS: TFslCSSFragment; Const sKey, aDefault: String): String;
Begin
  If oCSS.Values.ExistsByKey(sKey) Then
    Result := oCSS.Values.Matches[sKey]
  Else
    Result := aDefault;
End;

Function TWPHTMLReader.GetStyleValue(oCSS: TFslCSSFragment; Const sKey: String): String;
Begin
  Result := GetStyleValue(oCSS, sKey, '');
End;

Procedure TWPHTMLReader.ReadTable(oDocument: TWPWorkingDocument; oTable: TFslHTMLTable);
Var
  oStart : TWPWorkingDocumentTableStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
  iLoop : Integer;
Begin
  PushStyleContext(oTable);
  oStart := TWPWorkingDocumentTableStartPiece.Create;
  Try
    oStart.ReadOnly := ReadTristateAttribute(GetStyleValue(oTable.Style, CSS_ATTR_READONLY));
    oStart.BorderPolicy := TWPWorkingDocumentTableBorderPolicy(ReadEnumeratedAttribute(GetStyleValue(oTable.Style, CSS_ATTR_BORDERPOLICY), CODES_TWPWorkingDocumentTableBORDERPOLICY, ord(tbpNone)));
    oStart.Background := HTMLEncodedColourStringToColour(GetStyleValue(oTable.Style, CSS_ATTR_BACKGROUND), DEF_COLOUR);
    oStart.HorizontalMargin := StrToIntDef(GetStyleValue(oTable.Style, CSS_ATTR_HORIZONTAL_MARGIN), DEF_WORD);
    oStart.VerticalMargin := StrToIntDef(GetStyleValue(oTable.Style,CSS_ATTR_VERTICAL_MARGIN), DEF_WORD);

    ReadBorder(oStart.LeftBorder, oTable.Style, CSS_ATTR_BORDERLEFT, CSS_ATTR_BORDERLEFT_EXT);
    ReadBorder(oStart.RightBorder, oTable.Style, CSS_ATTR_BORDERRIGHT, CSS_ATTR_BORDERRIGHT_EXT);
    ReadBorder(oStart.TopBorder, oTable.Style, CSS_ATTR_BORDERTOP, CSS_ATTR_BORDERTOP_EXT);
    ReadBorder(oStart.BottomBorder, oTable.Style, CSS_ATTR_BORDERBOTTOM, CSS_ATTR_BORDERBOTTOM_EXT);
    ReadBorder(oStart.CenterHorizontalBorder, oTable.Style, CSS_ATTR_BORDER_VERTICAL_CENTRE, CSS_ATTR_BORDER_VERTICAL_CENTRE_EXT);
    ReadBorder(oStart.CenterVerticalBorder, oTable.Style, CSS_ATTR_BORDER_HORIZONTAL_CENTRE, CSS_ATTR_BORDER_HORIZONTAL_CENTRE_EXT);

    ReadStyleContext(oStart);
    oDocument.Pieces.Add(oStart.Link);

    For iLoop := 0 To oTable.Rows.Count - 1 Do
      ReadTableRow(oDocument, oTable.Rows[iLoop]);

    // table stop
    oStop := TWPWorkingDocumentStopPiece.Create(stTable);
    Try
      oDocument.Pieces.Add(oStop.Link);
    Finally
      oStop.Free;
    End;
  Finally
    oStart.Free;
    PopStyleContext;
  End;
End;


Procedure TWPHTMLReader.ReadTableRow(oDocument: TWPWorkingDocument; oRow: TFslHTMLTableRow);
Var
  oStart : TWPWorkingDocumentTableRowStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
  iLoop : Integer;
Begin
  PushStyleContext(oRow);
  oStart := TWPWorkingDocumentTableRowStartPiece.Create;
  Try
    ReadHotspot(oStart, oRow.Style);
    oStart.Header := StrToBoolDef(GetStyleValue(oRow.Style, CSS_ATTR_HEADER), False);
    oStart.LowerPaddingSize := StrToIntDef(GetStyleValue(oRow.Style, CSS_ATTR_LOWER_PADDING_SIZE), 0);
    oStart.LowerPaddingColour := HTMLEncodedColourStringToColour(GetStyleValue(oRow.Style, CSS_ATTR_LOWER_PADDING_COLOUR), DEF_COLOUR);
    oStart.ReadOnly := ReadTristateAttribute(GetStyleValue(oRow.Style, CSS_ATTR_READONLY));
    oStart.Background := HTMLEncodedColourStringToColour(GetStyleValue(oRow.Style, CSS_ATTR_BACKGROUND), DEF_COLOUR);

    ReadStyleContext(oStart);
    oDocument.Pieces.Add(oStart.Link);

    For iLoop := 0 To oRow.Cells.Count - 1 Do
      ReadTableCell(oDocument, oRow.Cells[iLoop]);

    oStop := TWPWorkingDocumentStopPiece.Create(stTableRow);
    Try
      oDocument.Pieces.Add(oStop.Link);
    Finally
      oStop.Free;
    End;

  Finally
    oStart.Free;
    PopStyleContext;
  End;
End;


Procedure TWPHTMLReader.ReadTableCell(oDocument: TWPWorkingDocument; oCell: TFslHTMLTableCell);
Var
  oStart : TWPWorkingDocumentTableCellStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
Begin
  PushStyleContext(oCell);
  oStart := TWPWorkingDocumentTableCellStartPiece.Create;
  Try
    ReadHotspot(oStart, oCell.Style);
    oStart.Span := StrToIntDef(GetStyleValue(oCell.Style, CSS_ATTR_SPAN), 1);
    oStart.ReadOnly := ReadTristateAttribute(GetStyleValue(oCell.Style, CSS_ATTR_READONLY));
    oStart.Background := HTMLEncodedColourStringToColour(GetStyleValue(oCell.Style, CSS_ATTR_BACKGROUND), DEF_COLOUR);
    oStart.MarginLeft := StrToIntDef(GetStyleValue(oCell.Style, CSS_ATTR_MARGIN_LEFT), DEF_WORD);
    oStart.MarginRight := StrToIntDef(GetStyleValue(oCell.Style, CSS_ATTR_MARGIN_RIGHT), DEF_WORD);
    oStart.MarginTop := StrToIntDef(GetStyleValue(oCell.Style, CSS_ATTR_MARGIN_TOP), DEF_WORD);
    oStart.MarginBottom := StrToIntDef(GetStyleValue(oCell.Style, CSS_ATTR_MARGIN_BOTTOM), DEF_WORD);
    oStart.VerticalAlignment := TWordProcessorVerticalAlignment(ReadEnumeratedAttribute(GetStyleValue(oCell.Style, CSS_ATTR_VERTICALALIGNMENT), NAMES_WORDPROCESSORVERTICALALIGNMENT, ord(VerticalAlignmentTop)));

    ReadBorder(oStart.LeftBorder, oCell.Style, CSS_ATTR_BORDERLEFT, CSS_ATTR_BORDERLEFT_EXT);
    ReadBorder(oStart.RightBorder, oCell.Style, CSS_ATTR_BORDERRIGHT, CSS_ATTR_BORDERRIGHT_EXT);
    ReadBorder(oStart.TopBorder, oCell.Style, CSS_ATTR_BORDERTOP, CSS_ATTR_BORDERTOP_EXT);
    ReadBorder(oStart.BottomBorder, oCell.Style, CSS_ATTR_BORDERBOTTOM, CSS_ATTR_BORDERBOTTOM_EXT);
    
    {
    If FActiveExtractor.Attributes.ExistsByKey(ATTR_NAME_WIDTH) Then
      oStart.Width := StringToReal(FActiveExtractor.Attributes[ATTR_NAME_WIDTH], 0);
    ReadTableItem(oStart);
    }

    ReadStyleContext(oStart);
    oDocument.Pieces.Add(oStart.Link);

    // cell content
    ReadContent(oDocument, oCell.Items, [WPHTMLReaderAllowedItemParagraph, WPHTMLReaderAllowedItemLine]);

    // cell stop
    oStop := TWPWorkingDocumentStopPiece.Create(stTableCell);
    Try
      oDocument.Pieces.Add(oStop.Link);
    Finally
      oStop.Free;
    End;
  Finally
    oStart.Free;
    PopStyleContext;
  End;

End;

Procedure TWPHTMLReader.ReadBorder(oBorder: TWPBorder; oCSS: TFslCSSFragment; Const sName, sNameExt: String);
Var
  iLoop : Integer;
  oIter : TWPWordIterator;
  sValue: String;
Begin
  oBorder.Clear;
  oIter := TWPWordIterator.Create;
  Try

    // read the basic border attributes: width, color, style
    If (sName <> '') And oCSS.Values.ExistsByKey(sName) Then
    Begin
      oBorder.Defined := True;
      oIter.Init(StringTrimWhitespace(oCSS.Values.Matches[sName]));
      iLoop := 0;
      While oIter.More Do
      Begin
        sValue := oIter.Next;

        If sValue <> ' ' Then // skip over separator
        Begin
          iLoop := iLoop + 1;
          Case iLoop Of
            1: If (Length(sValue) > 2) And (Copy(sValue, Length(sValue) - 1, 2) = 'px') Then
                 oBorder.Width := StrToIntDef(Copy(sValue, 1, Length(sValue) - 2), 1);
            2: oBorder.Colour := HTMLColourStringToColour(sValue, DEF_COLOUR);
            3: oBorder.Style := TFslPenStyle(ReadEnumeratedAttribute(sValue, ADVPENSTYLE_CODES, ord(apsSolid)));
          End;
        End;
      End;
    End;

    // read extended border attributes: Fancy, OuterColor, OuterColor2, LowOuterlimit, HighOuterLimit
    If (sNameExt <> '') And oCSS.Values.ExistsByKey(sNameExt) Then
    Begin
      oIter.Init(StringTrimWhitespace(oCSS.Values.Matches[sNameExt]));
      iLoop := 0;
      While oIter.More Do
      Begin
        sValue := oIter.Next;

        If sValue <> ' ' Then // skip over separator
        Begin
          iLoop := iLoop + 1;
          Case iLoop Of
            1: If StringUpper(sValue) = 'FANCY' Then
                 oBorder.Fancy := True
               Else
                 oBorder.Fancy := False;
            2: oBorder.OuterColour := HTMLColourStringToColour(sValue, DEF_COLOUR);
            3: oBorder.OuterColour2 := HTMLColourStringToColour(sValue, DEF_COLOUR);
            4: oBorder.LowOuterlimit := StrToIntDef(sValue, DEF_WORD);
            5: oBorder.HighOuterlimit := StrToIntDef(sValue, DEF_WORD);
          End;
        End;
      End;
    End;
  Finally
    oIter.Free;
  End;
End;


Function TWPHTMLReader.ReadCellsInImplicitTable(oDocument: TWPWorkingDocument; oSource: TFslHTMLItems; Const iIndex: Integer): Integer;
Var
  oTableStart : TWPWorkingDocumentTableStartPiece;
  oRowStart : TWPWorkingDocumentTableRowStartPiece;
  oTableStop, oRowStop : TWPWorkingDocumentStopPiece;
  iLoop : Integer;
Begin
  iLoop := iIndex;
  Assert(CheckCondition(oSource[iLoop].ElementType = ahTableCell, 'ReadCellsInImplicitTable', 'Table Cell is not found'));

  // create implicit table
  oTableStart := TWPWorkingDocumentTableStartPiece.Create;
  Try
    oDocument.Pieces.Add(oTableStart.Link);

    // create implicit row
    oRowStart := TWPWorkingDocumentTableRowStartPiece.Create;
    Try
      oDocument.Pieces.Add(oRowStart.Link);

      While (iLoop < oSource.Count) And (oSource[iLoop].ElementType = ahTableCell) Do
      Begin
        ReadTableCell(oDocument, TFslHTMLTableCell(oSource[iLoop]));
        iLoop := iLoop + 1;
      End;

      Result := iLoop;

      oRowStop := TWPWorkingDocumentStopPiece.Create(stTableRow);
      Try
        oDocument.Pieces.Add(oRowStop.Link);
      Finally
        oRowStop.Free;
      End;
    Finally
      oRowStart.Free;
    End;

    oTableStop := TWPWorkingDocumentStopPiece.Create(stTable);
    Try
      oDocument.Pieces.Add(oTableStop.Link);
    Finally
      oTableStop.Free;
    End;
  Finally
    oTableStart.Free;
  End;
End;


Function TWPHTMLReader.ReadRowsInImplicitTable(oDocument: TWPWorkingDocument; oSource: TFslHTMLItems; Const iIndex: Integer): Integer;
Var
  oStart : TWPWorkingDocumentTableStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
  iLoop : Integer;
Begin
  iLoop := iIndex;
  Assert(CheckCondition(oSource[iLoop].ElementType = ahTableRow, 'ReadRowsInImplicitTable', 'Table Row is not found'));

  oStart := TWPWorkingDocumentTableStartPiece.Create;
  Try
    oDocument.Pieces.Add(oStart.Link);

    While (iLoop < oSource.Count) And (oSource[iLoop].ElementType = ahTableRow) Do
    Begin
      ReadTableRow(oDocument, TFslHTMLTableRow(oSource[iLoop]));
      iLoop := iLoop + 1;
    End;

    Result := iLoop;

    oStop := TWPWorkingDocumentStopPiece.Create(stTable);
    Try
      oDocument.Pieces.Add(oStop.Link);
    Finally
      oStop.Free;
    End;
  Finally
    oStart.Free;
  End;
End;




function TWPHTMLReader.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDom.sizeInBytes);
  inc(result, FContextStack.sizeInBytes);
end;

{ TWPHTMLWriterEngine }

Constructor TWPHTMLWriterEngine.Create;
Begin
  Inherited;
  FFormatter := TFslHTMLFormatter.Create;
  FFormatter.HasWhitespace := True;
  FCSSStyles := TFslCSSValues.Create;
  FCSSStyles.Forced := True;
  FFontScale := 1.3;
End;


Destructor TWPHTMLWriterEngine.Destroy;
Begin
  FCSSStyles.Free;
  FFormatter.Free;
  Inherited;
End;


Function TWPHTMLWriterEngine.CanSaveImage : Boolean;
Begin
  Result := False;
End;


Procedure TWPHTMLWriterEngine.SaveImage(oBuffer : TFslBuffer; Const sExtension : String; Var sName : String);
Begin
  RaiseError('SaveImage', 'Need to override SaveImage in '+ClassName);
End;


Function TWPHTMLWriterEngine.HTMLStream : TFslStream;
Begin
  Result := Nil;
  RaiseError('HTMLStream', 'Need to override HTMLStream in '+ClassName);
End;


Procedure TWPHTMLWriterEngine.Initialise;
Begin
  Inherited;
  FFormatter.Clear;
  FFormatter.Stream := HTMLStream.Link;
  FParaTextUnderline := tsUnknown;
  FParaTextStrikethrough := tsUnknown;
  FFieldTextUnderline := tsUnknown;
  FFieldTextStrikethrough := tsUnknown;
End;


Procedure TWPHTMLWriterEngine.Finalise;
Begin
  FFormatter.Stream := Nil;
  Inherited;
End;


Procedure TWPHTMLWriterEngine.WriteDocumentStart(oDocument : TWPWorkingDocument);
Begin
  FCurrentParaType := WPSParagraphListTypeNone;
  If Not FNoHTMLBody Then
    Begin
    FFormatter.Attributes.Add('encoding', 'utf-8');
    FFormatter.ProduceHeader;
    FFormatter.ProduceLine('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">');
    FFormatter.ProduceOpen('html');
    FFormatter.ProduceOpen('head');
    If FTitle <> '' Then
      FFormatter.ProduceText('Title', FTitle);
    FFormatter.ProduceClose('head');
    If EmbedStyles Then
      ProduceStyles;
    FFormatter.ProduceOpen('body');
    End;
End;

Procedure TWPHTMLWriterEngine.WriteDocumentStop(oDocument : TWPWorkingDocument);
Begin
  If Not FNoHTMLBody Then
    Begin
    CheckWriteListClose(Nil);
    CheckBlockQuoteClose(Nil);
    FFormatter.ProduceClose('body');
    FFormatter.ProduceClose('html');
    End;
End;

Procedure TWPHTMLWriterEngine.WriteSectionStart(oSection : TWPWorkingDocumentSectionStartPiece);
Begin
    CheckBlockQuoteClose(Nil);
    CheckBlockQuoteClose(Nil);
  If oSection.Namespace <> '' Then
    FFormatter.Attributes.Add('idspace', oSection.Namespace);
  If oSection.Name <> '' Then
    FFormatter.Attributes.Add('id', oSection.Name);
  FFormatter.Attributes.Add('title', oSection.DisplayName);

  FCSSStyles[CSS_ATTR_DELETABLE] := BooleanToString(oSection.Deletable);
  FCSSStyles[CSS_ATTR_ISFIELD] := BooleanToString(oSection.IsField);
  If oSection.HasData Then
    FCSSStyles[CSS_ATTR_DATA] := oSection.RawDataAsText;

  If oSection.Key <> '' Then
    FCSSStyles[CSS_ATTR_KEY] := oSection.Key;
  If oSection.DisplayType <> sdtNone Then
    FCSSStyles[CSS_ATTR_DISPLAYTYPE] := NAMES_WPWorkingDocumentSectionDISPLAYTYPE[oSection.DisplayType];
  If oSection.ReadOnly <> tsUnknown Then
    FCSSStyles[CSS_ATTR_READONLY] := NAMES_WPSTRISTATE[oSection.ReadOnly];

  WriteFormatAttributes(oSection, false);
  FFormatter.ProduceOpen('div');
End;

Procedure TWPHTMLWriterEngine.WriteSectionStop(oSection : TWPWorkingDocumentSectionStartPiece; oStop : TWPWorkingDocumentStopPiece);
Begin
  CheckWriteListClose(Nil);
  CheckBlockQuoteClose(Nil);
  FFormatter.ProduceClose('div');
End;

Procedure TWPHTMLWriterEngine.WriteFieldStart(oField: TWPWorkingDocumentFieldStartPiece);
Begin

  If oField.Name <> '' Then
    FFormatter.Attributes.Add('id', oField.Namespace+'::'+oField.Name);

  If oField.ReadOnly <> tsUnknown Then
    FCSSStyles[CSS_ATTR_READONLY] := NAMES_WPSTRISTATE[oField.ReadOnly];
  If oField.HasData Then
    FCSSStyles[CSS_ATTR_DATA] := oField.RawDataAsText;
  FCSSStyles[CSS_ATTR_DELETABLE] := BooleanToString(oField.Deletable);
  FCSSStyles[CSS_ATTR_FIXEDFORMAT] := TWPDOCUMENTOBJECT_FIXED_FORMAT[oField.FixedFormat];

  // hotspot attributes
  If oField.HasHotspot Then
    WriteHotspotAttributes(oField.Hotspot);

  WriteFormatAttributes(oField, false);
  FFieldTextUnderline := oField.Font.Underline;
  FFieldTextStrikethrough := oField.Font.Strikethrough;

  If oField.HasHotspot And (oField.Hotspot.URL <> '') Then
  Begin
    FFormatter.Attributes.Add('href', oField.Hotspot.URL);
    FFormatter.ProduceOpen('a');
  End
  Else
    FFormatter.ProduceOpen('span');
End;

Procedure TWPHTMLWriterEngine.WriteFieldStop(oField : TWPWorkingDocumentFieldStartPiece; oStop : TWPWorkingDocumentFieldStopPiece);
Begin
  CloseSpan;
  If oField.HasHotspot And (oField.Hotspot.URL <> '') Then
    FFormatter.ProduceClose('a')
  Else
    FFormatter.ProduceClose('span');
End;

Procedure TWPHTMLWriterEngine.WriteParagraphStart(oParagraph : TWPWorkingDocumentParaPiece);
Var
  sTag : String;
Begin
  CheckWriteListClose(oParagraph);
  CheckBlockquoteClose(oParagraph);

  If Assigned(oParagraph) Then
  Begin
    CheckBlockQuoteOpen(oParagraph);

    Case oParagraph.Format.ListType Of
      WPSParagraphListTypeBullets : sTag := StartBulleted(oParagraph);
      WPSParagraphListTypeNumbers : sTag := StartNumbered(oParagraph);
    Else
      sTag := StartPLainPara(oParagraph);
    End;
    StartParagraphFont(oParagraph);
    FFormatter.ProduceOpen(sTag);
  End;
  StartText;
End;

Function TWPHTMLWriterEngine.StartPlainPara(oParagraph : TWPWorkingDocumentParaPiece) : String;
Begin
  Case oParagraph.Format.Align Of
    WordProcessorParagraphAlignmentLeft : FCSSStyles[CSS_ATTR_ALIGN] := 'left';
    WordProcessorParagraphAlignmentCentre : FCSSStyles[CSS_ATTR_ALIGN] := 'center';
    WordProcessorParagraphAlignmentRight : FCSSStyles[CSS_ATTR_ALIGN] := 'right';
    WordProcessorParagraphAlignmentJustify : FCSSStyles[CSS_ATTR_ALIGN] := 'justify';
  End;
  Result := 'p';
End;

Function TWPHTMLWriterEngine.StartBulleted(oParagraph : TWPWorkingDocumentParaPiece) : String;
Begin
  If FCurrentParaType <> WPSParagraphListTypeBullets Then
    Begin
    FFormatter.ProduceOpen('ul');
    FCurrentParaType := WPSParagraphListTypeBullets
    End;

  Case oParagraph.Format.BulletType Of
    tbDisc : FCSSStyles[CSS_ATTR_LISTTYPE] := 'disc';
    tbSquare : FCSSStyles[CSS_ATTR_LISTTYPE] := 'square';
    tbCircle : FCSSStyles[CSS_ATTR_LISTTYPE] := 'circle';
  End;
  Result := 'li';
End;

Function TWPHTMLWriterEngine.StartNUmbered(oParagraph : TWPWorkingDocumentParaPiece) : String;
Begin
  If FCurrentParaType <> WPSParagraphListTypeNumbers Then
    Begin
    FFormatter.ProduceOpen('ol');
    FCurrentParaType := WPSParagraphListTypeNumbers
    End;

  Case oParagraph.Format.NumberType Of
    tnArabic : FCSSStyles[CSS_ATTR_LISTTYPE] := 'decimal';
    tnLowerAlpha : FCSSStyles[CSS_ATTR_LISTTYPE] :=  'lower-alpha';
    tnUpperAlpha : FCSSStyles[CSS_ATTR_LISTTYPE] := 'upper-alpha';
    tnLowerRoman : FCSSStyles[CSS_ATTR_LISTTYPE] := 'lower-roman';
    tnUpperRoman : FCSSStyles[CSS_ATTR_LISTTYPE] := 'upper-roman';
  End;

  FCSSStyles['list-format'] := NAMES_WPSPARAGRAPHNUMBERFORMAT[oParagraph.Format.NumberFormat];

  If oParagraph.Format.FixedNumber <> DEF_WORD Then
    FFormatter.Attributes.Add('value', IntegerToString(oParagraph.Format.FixedNumber));

  Result := 'li';
End;

Procedure TWPHTMLWriterEngine.CheckWriteListClose(oParagraph : TWPWorkingDocumentParaPiece);
Begin
  If Not Assigned(oParagraph) Or (oParagraph.Format.ListType <> FCurrentParaType) Then
    Begin
    If FCurrentParaType = WPSParagraphListTypeNumbers Then
      FFormatter.ProduceClose('ol')
    Else If FCurrentParaType = WPSParagraphListTypeBullets Then
      FFormatter.ProduceClose('ul');
    FCurrentParaType := WPSParagraphListTypeNone;
    End;
End;

Procedure TWPHTMLWriterEngine.CheckBlockQuoteClose(oParagraph : TWPWorkingDocumentParaPiece);
Var
  iTarget : Integer;
  iLoop : Integer;
Begin
  If (oParagraph = Nil) Or (oParagraph.Format.LeftIndent = DEF_WORD) Then
    iTarget := 0
  Else
    iTarget := oParagraph.Format.LeftIndent;
  If iTarget < FLeftIndent Then
  Begin
    For iLoop := FLeftIndent - 1 DownTo iTarget Do
      FFormatter.ProduceClose('blockquote');
    FLeftIndent := iTarget;
  End;
End;

Procedure TWPHTMLWriterEngine.CheckBlockQuoteOpen(oParagraph : TWPWorkingDocumentParaPiece);
Var
  iTarget : Integer;
  iLoop : Integer;
Begin
 If (oParagraph = Nil) Or (oParagraph.Format.LeftIndent = DEF_WORD) Then
    iTarget := 0
  Else
    iTarget := oParagraph.Format.LeftIndent;

  Assert(CheckCondition(FLeftIndent <= iTarget, 'CheckBlockQuoteOpen', 'Open block quotes exist'));
  For iLoop := FLeftIndent To iTarget - 1 Do
    FFormatter.ProduceOpen('blockquote');
  FLeftIndent := iTarget;
End;

Procedure TWPHTMLWriterEngine.WriteParagraphStop(oParagraph : TWPWorkingDocumentParaPiece; bNextIsSection : Boolean; oSection : TWPWorkingDocumentSectionStartPiece);
Begin
  CloseSpan;
  CloseText;
  If Assigned(oParagraph) Then
  Begin
    If FCurrentParaType In [WPSParagraphListTypeBullets, WPSParagraphListTypeNumbers] Then
      FFormatter.ProduceClose('li')
    Else
      FFormatter.ProduceClose('p');
  End;
End;


Procedure TWPHTMLWriterEngine.WriteBreak(oBreak : TWPWorkingDocumentBreakPiece);
Begin
  CheckWriteListClose(Nil);
  CheckBlockQuoteClose(Nil);

  Case oBreak.Alignment Of
    WordProcessorAlignmentLeft : FFormatter.Attributes.Match['align'] := 'left';
    WordProcessorAlignmentCentre : FFormatter.Attributes.Match['align'] := 'center';
    WordProcessorAlignmentRight : FFormatter.Attributes.Match['align'] := 'right';
  End;
  FCSSStyles[CSS_ATTR_HEIGHT] := IntegerToString(oBreak.PenWidth) + 'px';
  FCSSStyles[CSS_ATTR_WIDTH] := IntegerToString(Trunc(oBreak.Width*100))+'%';
  FCSSStyles[CSS_ATTR_BACKGROUND] := ColourToHTMLColourString(oBreak.PenColour);
  FCSSStyles[CSS_ATTR_PENSTYLE] := ADVPENSTYLE_CODES[oBreak.PenStyle];
  FCSSStyles[CSS_ATTR_PENENDSTYLE] := ADVPENENDSTYLE_CODES[oBreak.EndStyle];
  FCSSStyles[CSS_ATTR_BREAKTYPE] := WPWorkingDocumentBreakPIECETYPE_NAMES[oBreak.BreakType];
  
  WriteFormatAttributes(oBreak, true);
  FFormatter.ProduceTag('hr');
End;


Procedure TWPHTMLWriterEngine.WriteText(oText : TWPWorkingDocumentTextPiece);
Var
  iLoop : Integer;
  sText : String;
Begin
  If Not FontFormatMatches(oText) or (FParaTextUnderline <> tsTrue) or (FFieldTextUnderline <> tsTrue) or (FParaTextStrikethrough <> tsTrue) or (FFieldTextStrikethrough <> tsTrue)  Then
    CloseSpan;
  If Not FontFormatMatches(oText) or (FParaTextUnderline <> tsTrue) or (FFieldTextUnderline <> tsTrue) or (FParaTextStrikethrough <> tsTrue) or (FFieldTextStrikethrough <> tsTrue) Then
    StartSpan(oText);
  UseText;
  sText := oText.Content;
  For iLoop := 1 To Length(sText) Do
    If sText[iLoop] = ' ' Then
      FFormatter.ProduceFragment(' ')
    Else
      FFormatter.ProduceFragment(EncodeXML(sText[iLoop]));
End;

Procedure TWPHTMLWriterEngine.WriteImage(oImage : TWPWorkingDocumentImagePiece);
Var
  sName : String;
  oBuffer : TFslBuffer;
  oMemory : TFslMemoryStream;
  sExt : String;
  oImg : TFslGraphic;
Begin
  If SaveImageHolders Or (CanSaveImage And oImage.HasImage) Then
    Begin
    sName := oImage.Name;
    If Assigned(oImage.GetWorkingImage(0, 0, false, false)) Then
      Begin
      oBuffer := TFslBuffer.Create;
      Try
        oMemory := TFslMemoryStream.Create;
        Try
          oMemory.Buffer := oBuffer.Link;
          oMemory.Expand := True;
          oImg := oImage.GetWorkingImage(0, 0, false, false);
          if not (oImg is TFslVCLGraphic) then
          begin
            oImg.DrawToStream(oMemory, oImage.Width, oImage.Height);
            sExt := 'png';
          end
          else If TFslVCLGraphic(oImg).Handle Is TJPEGImage Then
            Begin
            sExt := 'jpg';
            TFslVCLGraphic(oImg).SaveToStream(oMemory)
            End
          Else
            Begin
            sExt := 'png';
            TFslPortableNetworkGraphic.SavePNGToStream(TFslVCLGraphic(oImg), oMemory);
            End;
        Finally
          oMemory.Free;
        End;
        SaveImage(oBuffer, sExt, sName);
      Finally
        oBuffer.Free;
      End;
      End;
    If sName <> '' Then
      Begin
      oImage.Name := sName;
      FCSSStyles['border-width'] := IntegerToString(oImage.Border);
      FCSSStyles['border-color'] := ColourToHTMLColourString(oImage.BorderColour);
      If oImage.TransparentColour <> DEF_COLOUR Then
        FCSSStyles['transparent-color'] := ColourToHTMLColourString(oImage.TransparentColour);
      WriteFormatAttributes(oImage, true);
      FFormatter.Attributes.Match['src'] := sName;
      FFormatter.Attributes.Match['height'] := IntegerToString(oImage.Height);
      FFormatter.Attributes.Match['width'] := IntegerToString(oImage.Width);
      FFormatter.ProduceTag('img');
      End;
    End;
End;

Procedure TWPHTMLWriterEngine.StartText;
Begin
  FText := False;
End;

Procedure TWPHTMLWriterEngine.UseText;
Begin
  FText := True;
End;

Procedure TWPHTMLWriterEngine.CloseText;
Begin
  If FText Then
    FFormatter.ProduceNewLine;
  StartText;
End;

Procedure TWPHTMLWriterEngine.StartParagraphFont(oPara: TWPWorkingDocumentParaPiece);
Begin
  FParaFont := oPara.Font;
  FSpanFont := Nil;
  FParaStyle := oPara.Style;
  FSpanStyle := '';
  FParaTextUnderline := oPara.Font.Underline;
  FParaTextStrikethrough := oPara.Font.Strikethrough;

  WriteFormatAttributes(oPara, false);
End;

Function TWPHTMLWriterEngine.FontFormatMatches(oPiece : TWPWorkingDocumentPiece) : Boolean;
Begin
  If FInSpan Then
    Result := (oPiece.Style = FSpanStyle) And FSpanFont.Matches(oPiece.Font)
  Else
    Result := (oPiece.Style = FParaStyle) And Assigned(FParaFont) And FParaFont.Matches(oPiece.Font)
End;

Procedure TWPHTMLWriterEngine.StartSpan(oPiece : TWPWorkingDocumentPiece);
Begin
  FInSpan := True;
  FSpanFont := oPiece.Font;
  FSpanStyle := oPiece.Style;
  WriteFormatAttributes(oPiece, true);
  FFormatter.ProduceOpen('font', True);
End;

Procedure TWPHTMLWriterEngine.CloseSpan;
Begin
  If FInSpan Then
    FFormatter.ProduceClose('font', True);
  FInSpan := False;
  FSpanFont := Nil;
  FSpanStyle := '';
End;

Function TWPHTMLWriterEngine.EncodeStyles(oFont, oStyleFont : TWPSFontDetails; bTextDecorations : Boolean) : String;
Var
  iLoop : Integer;
  procedure encodeUnderline(state : TWPSTriState);
  begin
    if state = tsTrue then
      StringAppend(Result, 'text-decoration: underline', '; ')
    else
      StringAppend(Result, 'text-decoration: none', '; ');
  end;
  procedure encodeStrikethrough(state : TWPSTriState);
  begin
    if state = tsTrue then
      StringAppend(Result, 'text-decoration: strikethrough', '; ')
    else
      StringAppend(Result, 'text-decoration: none', '; ');
  end;

Begin
  Result := '';
  For iLoop := 0 To FCSSStyles.count - 1 Do
    StringAppend(Result, FCSSStyles.KeyByIndex[iLoop] +': '+FCSSStyles.ValueByIndex[iLoop], '; ');
  FCSSStyles.Clear;

  If (oFont.Name <> DEF_STRING) And ((oStyleFont = Nil) Or (oStyleFont.Name <> oFont.Name)) Then
    StringAppend(Result, CSS_ATTR_FONTNAME + ': '+oFont.Name, '; ');
  If (oFont.Size <> DEF_WORD) And ((oStyleFont = Nil) Or (oStyleFont.Size <> oFont.Size)) Then
    StringAppend(Result, CSS_ATTR_FONTSIZE + ': '+IntegerToString(Trunc(oFont.Size * FFontScale))+'px', '; ');
  If (oFont.Bold <> tsUnknown) And ((oStyleFont = Nil) Or (oStyleFont.Bold <> oFont.Bold)) Then
  begin
    if oFont.Bold = tsTrue then
      StringAppend(Result, 'font-weight: bold', '; ')
    else
      StringAppend(Result, 'font-weight: normal', '; ')
  end;
  If (oFont.Italic <> tsUnknown) And ((oStyleFont = Nil) Or (oStyleFont.Italic <> oFont.Italic)) Then
  begin
    if oFont.Bold = tsTrue then
      StringAppend(Result, 'font-style: italic', '; ')
    else
      StringAppend(Result, 'font-style: normal', '; ');
  end;
  if bTextDecorations then
  begin
    // this is work around for a weird thing in the CSS spec where contained elements are not
    // allowed to overrule the text decoration of a containing element. It does mean that paragraph
    // and field level underline and strike-through settings won't round-trip through HTML, but it
    // won't make any difference to the functional appearance of the output
    If (oFont.Underline <> tsUnknown) And ((oStyleFont = Nil) Or (oStyleFont.Underline <> oFont.Underline)) Then
      encodeUnderline(oFont.Underline)
    else if FFieldTextUnderline <> tsUnknown then
      encodeUnderline(FFieldTextUnderline)
    else if FParaTextUnderline <> tsUnknown then
      encodeUnderline(FParaTextUnderline);
    If (oFont.Strikethrough <> tsUnknown) And ((oStyleFont = Nil) Or (oStyleFont.Strikethrough <> oFont.Strikethrough)) Then
      encodeStrikethrough(FParaTextStrikethrough)
    else if FFieldTextStrikethrough <> tsUnknown then
      encodeStrikethrough(FFieldTextStrikethrough)
    else if FParaTextStrikethrough <> tsUnknown then
      encodeStrikethrough(FParaTextStrikethrough);
  end;
  If (oFont.state = fsSuperscript) And ((oStyleFont = Nil) Or (oStyleFont.state <> oFont.state)) Then
    StringAppend(Result, 'vertical-align: super', '; ');
  If (oFont.state = fsSubscript) And ((oStyleFont = Nil) Or (oStyleFont.state <> oFont.state)) Then
    StringAppend(Result, 'vertical-align: sub', '; ');
  If (oFont.Foreground <> DEF_COLOUR) And ((oStyleFont = Nil) Or (oStyleFont.Foreground <> oFont.Foreground)) Then
    StringAppend(Result, CSS_ATTR_FOREGROUND + ': ' + ColourToHTMLColourString(oFont.Foreground), '; ');
  If (oFont.Background <> DEF_COLOUR) And ((oStyleFont = Nil) Or (oStyleFont.Background <> oFont.Background)) Then
    StringAppend(Result, CSS_ATTR_BACKGROUND + ': '+ColourToHTMLColourString(oFont.Background), '; ');
  If (oFont.Capitalization <> fcsUnknown) Then
  Begin
    // TODO: small-caps with CSS font variant
    If oFont.Capitalization = fcsAllCaps Then
      StringAppend(Result, CSS_ATTR_TEXT_TRANSFORM + ': uppercase; ')
    Else If oFont.Capitalization = fcsNoCaps Then
      StringAppend(Result, CSS_ATTR_TEXT_TRANSFORM + ': lowercase; ')
    Else
      StringAppend(Result, CSS_ATTR_TEXT_TRANSFORM + ': none; ')
  End;
End;

Procedure TWPHTMLWriterEngine.WriteFormatAttributes(oPiece: TWPWorkingDocumentPiece; bTextDecorations : Boolean);
Var
  sStyle : String;
  oStyle : TWPStyle;
Begin
  If oPiece.Style = '' Then
    FFormatter.Attributes.Add('class', 'wp-default')
  Else
    FFormatter.Attributes.Add('class', oPiece.Style);
  oStyle := Styles.GetByName(oPiece.Style);
  If Assigned(oStyle) Then
    sStyle := EncodeStyles(oPiece.Font, oStyle.Font, bTextDecorations)
  Else
    sStyle := EncodeStyles(oPiece.Font, Nil, bTextDecorations);
  If sStyle <> '' Then
    FFormatter.Attributes.Add('style', sStyle);
End;


Procedure TWPHTMLWriterEngine.ProduceStyles;
Var
  iLoop : Integer;
Begin
  FFormatter.Attributes.Add('type', 'text/css');
  FFormatter.ProduceOpen('STYLE');
  For iLoop := 0 To Styles.Count - 1 Do
    ProduceStyle(Styles[iLoop]);
  FFormatter.ProduceClose('STYLE');
End;

Procedure TWPHTMLWriterEngine.ProduceStyle(oStyle: TWPStyle);
Begin
  If oStyle = Styles.DefaultStyle Then
    FFormatter.ProduceFragment('BODY {'+EncodeStyles(oStyle.Font, Nil, true)+'}'+cReturn);
    FFormatter.ProduceFragment('.'+oStyle.Name+' {'+EncodeStyles(oStyle.Font, Nil, true)+'}'+cReturn);
End;


Procedure TWPHTMLWriterEngine.WriteLineBreak(oBreak: TWPWorkingDocumentLineBreakPiece);
Begin
  CloseSpan;
  WriteFormatAttributes(oBreak, true);
  FFormatter.ProduceTag('br');
End;


Procedure TWPHTMLWriterEngine.WriteTableStart(oTable : TWPWorkingDocumentTableStartPiece);
Begin
  CheckWriteListClose(Nil);
  CheckBlockQuoteClose(Nil);

  FFormatter.Attributes.Add('border', '0');
  FFormatter.Attributes.Add('cellspacing', '0');
  FFormatter.Attributes.Add('cellpadding', '4');

  If oTable.ReadOnly <> tsUnknown Then
    FCSSStyles[CSS_ATTR_READONLY] := NAMES_WPSTRISTATE[oTable.ReadOnly];
  If oTable.BorderPolicy <> tbpNone Then
    FCSSStyles[CSS_ATTR_BORDERPOLICY] := CODES_TWPWorkingDocumentTableBORDERPOLICY[oTable.BorderPolicy];
  If oTable.Background <> DEF_COLOUR Then
    FCSSStyles[CSS_ATTR_BACKGROUND] := ColourToHTMLColourString(oTable.Background);
  If oTable.HorizontalMargin <> DEF_WORD Then
    FCSSStyles[CSS_ATTR_HORIZONTAL_MARGIN] := IntegerToString(oTable.HorizontalMargin);
  If oTable.VerticalMargin <> DEF_WORD Then
    FCSSStyles[CSS_ATTR_VERTICAL_MARGIN] := IntegerToString(oTable.VerticalMargin);

  WriteBorderAttributes(CSS_ATTR_BORDERLEFT, CSS_ATTR_BORDERLEFT_EXT, oTable.LeftBorder);
  WriteBorderAttributes(CSS_ATTR_BORDERRIGHT, CSS_ATTR_BORDERRIGHT_EXT, oTable.RightBorder);
  WriteBorderAttributes(CSS_ATTR_BORDERTOP, CSS_ATTR_BORDERTOP_EXT, oTable.TopBorder);
  WriteBorderAttributes(CSS_ATTR_BORDERBOTTOM, CSS_ATTR_BORDERBOTTOM_EXT, oTable.BottomBorder);
  WriteBorderAttributes(CSS_ATTR_BORDER_VERTICAL_CENTRE, CSS_ATTR_BORDER_VERTICAL_CENTRE_EXT, oTable.CenterHorizontalBorder);
  WriteBorderAttributes(CSS_ATTR_BORDER_HORIZONTAL_CENTRE, CSS_ATTR_BORDER_HORIZONTAL_CENTRE_EXT, oTable.CenterVerticalBorder);

  WriteFormatAttributes(oTable, false);
  FFormatter.ProduceTableOpen;
End;


Procedure TWPHTMLWriterEngine.WriteTableStop(oTable : TWPWorkingDocumentTableStartPiece; oStop : TWPWorkingDocumentStopPiece);
Begin
  FFormatter.ProduceTableClose;
End;


Procedure TWPHTMLWriterEngine.WriteTableRowStart(oTableRow : TWPWorkingDocumentTableRowStartPiece);
Begin
  If oTableRow.ReadOnly <> tsUnknown Then
    FCSSStyles[CSS_ATTR_READONLY] := NAMES_WPSTRISTATE[oTableRow.ReadOnly];
  If oTableRow.Header Then      // <THEAD> ?
    FCSSStyles[CSS_ATTR_HEADER] :=  BooleanToString(oTableRow.Header);
  If oTableRow.Background <> DEF_COLOUR Then
    FCSSStyles[CSS_ATTR_BACKGROUND] := ColourToHTMLColourString(oTableRow.Background);
  If oTableRow.LowerPaddingSize > 0 Then
    FCSSStyles[CSS_ATTR_LOWER_PADDING_SIZE] := IntegerToString(oTableRow.LowerPaddingSize);
  If oTableRow.LowerPaddingColour <> DEF_COLOUR Then
    FCSSStyles[CSS_ATTR_LOWER_PADDING_COLOUR] := ColourToHTMLColourString(oTableRow.LowerPaddingColour);
  If oTableRow.HasHotspot Then
    WriteHotspotAttributes(oTableRow.Hotspot);

  WriteFormatAttributes(oTableRow, false);
  FFormatter.ProduceTableRowOpen;
End;


Procedure TWPHTMLWriterEngine.WriteTableRowStop(oTableRow : TWPWorkingDocumentTableRowStartPiece; oStop : TWPWorkingDocumentStopPiece; bIsLast : Boolean);
Begin
  FFormatter.ProduceTableRowClose;
End;


Procedure TWPHTMLWriterEngine.WriteTableCellStart(oTableCell : TWPWorkingDocumentTableCellStartPiece);
Begin
  If oTableCell.ReadOnly <> tsUnknown Then
    FCSSStyles[CSS_ATTR_READONLY] := NAMES_WPSTRISTATE[oTableCell.ReadOnly];
  If oTableCell.Span <> 1 Then
    FCSSStyles[CSS_ATTR_SPAN] := IntegerToString(oTableCell.Span);
  If oTableCell.Width <> 0 Then
    FCSSStyles[CSS_ATTR_WIDTH] := RealToString(oTableCell.Width) + 'px';
  If oTableCell.Background <> DEF_COLOUR Then
    FCSSStyles[CSS_ATTR_BACKGROUND] := ColourToHTMLColourString(oTableCell.Background);
  If oTableCell.MarginLeft <> DEF_WORD Then
    FCSSStyles[CSS_ATTR_MARGIN_LEFT] := IntegerToString(oTableCell.MarginLeft) + 'px';
  If oTableCell.MarginRight <> DEF_WORD Then
    FCSSStyles[CSS_ATTR_MARGIN_RIGHT] := IntegerToString(oTableCell.MarginRight) + 'px';
  If oTableCell.MarginTop <> DEF_WORD Then
    FCSSStyles[CSS_ATTR_MARGIN_TOP] :=  IntegerToString(oTableCell.MarginTop) + 'px';
  If oTableCell.MarginBottom <> DEF_WORD Then
    FCSSStyles[CSS_ATTR_MARGIN_BOTTOM] :=  IntegerToString(oTableCell.MarginBottom) + 'px';
  If oTableCell.VerticalAlignment <> VerticalAlignmentTop Then
    FCSSStyles[CSS_ATTR_VERTICALALIGNMENT] := NAMES_WORDPROCESSORVERTICALALIGNMENT[oTableCell.VerticalAlignment];

  WriteBorderAttributes(CSS_ATTR_BORDERLEFT, CSS_ATTR_BORDERLEFT_EXT, oTableCell.LeftBorder);
  WriteBorderAttributes(CSS_ATTR_BORDERRIGHT, CSS_ATTR_BORDERRIGHT_EXT, oTableCell.RightBorder);
  WriteBorderAttributes(CSS_ATTR_BORDERTOP, CSS_ATTR_BORDERTOP_EXT, oTableCell.TopBorder);
  WriteBorderAttributes(CSS_ATTR_BORDERBOTTOM, CSS_ATTR_BORDERBOTTOM_EXT, oTableCell.BottomBorder);
  If oTableCell.HasHotspot Then
    WriteHotspotAttributes(oTableCell.Hotspot);


  WriteFormatAttributes(oTableCell, false);
  FFormatter.ProduceTableCellOpen;
End;


Procedure TWPHTMLWriterEngine.WriteTableCellStop(oTableCell : TWPWorkingDocumentTableCellStartPiece; oStop : TWPWorkingDocumentStopPiece);
Begin
  CheckWriteListClose(Nil);
  CheckBlockQuoteClose(Nil);
  FFormatter.ProduceTableCellClose;
End;

Const
  HTML_BORDER_STYLES : Array [TFslPenStyle] Of String = ('solid', 'dashed', 'dashed', 'dotted', 'dotted', 'inset', 'none');


Procedure TWPHTMLWriterEngine.WriteBorderAttributes(Const sName, sExtName : String; oBorder : TWPBorder);
Var
  sExtValue : String;
Begin
  If Assigned(oBorder) And (oBorder.Defined) Then
  Begin
    FCSSStyles[sName] := IntegerToString(oBorder.Width)+'px '+ColourToHTMLColourString(oBorder.Colour)+' '+HTML_BORDER_STYLES[oBorder.Style];

    // extended values (non-css standard)
    If (oBorder.Fancy Or (oBorder.OuterColour <> DEF_COLOUR)
        Or (oBorder.OuterColour2 <> DEF_COLOUR)
        Or (oBorder.LowOuterlimit <> DEF_WORD)
        Or (oBorder.HighOuterLimit <> DEF_WORD)) Then
    Begin
      sExtValue := '';
      If oBorder.Fancy Then
        sExtValue := sExtValue + 'Fancy'
      Else
        sExtValue := 'NotFancy';
      If oBorder.OuterColour <> DEF_COLOUR Then
        sExtValue := sExtValue + ' ' + ColourToXMLColourString(oBorder.OuterColour)
      Else
        sExtValue := sExtValue + ' ' + 'Nil';
      If oBorder.OuterColour2 <> DEF_COLOUR Then
        sExtValue := sExtValue + ' ' + ColourToXMLColourString(oBorder.OuterColour2)
      Else
        sExtValue := sExtValue + ' ' + 'Nil';
      If oBorder.LowOuterlimit <> DEF_WORD Then
        sExtValue := sExtValue + ' ' + IntegerToString(oBorder.LowOuterlimit)
      Else
        sExtValue := sExtValue + ' ' + 'Nil';
      If oBorder.HighOuterLimit <> DEF_WORD Then
        sExtValue := sExtValue + ' ' + IntegerToString(oBorder.HighOuterLimit)
      Else
        sExtValue := sExtValue + ' ' + 'Nil';

      { // TODO: border brush image
      If oBorder.BrushImage <> Nil Then
      Begin
        FActiveFormatter.ProduceOpen(sName);
        oMem := TFslMemoryStream.Create;
        Try
          oBorder.BrushImage.SaveToStream(oMem);
          FActiveFormatter.ProduceText(TAG_NAME_BRUSH, EncodeBase64(oMem.Buffer.AsText));
        Finally
          oMem.Free;
        End;
      End;
      }
      FCSSStyles[sExtName] := sExtValue;
    End;
  End;
End;


Procedure TWPHTMLWriterEngine.WriteHotspotAttributes(oHotspot: TWPHotspot);
Begin
    If (oHotspot.URL <> '') Then
      FCSSStyles[CSS_ATTR_URL] := EncodePercent(oHotspot.URL);
    If oHotspot.Title <> '' Then
      FCSSStyles[CSS_ATTR_TITLE] := oHotspot.Title;
    If oHotspot.Key <> '' Then
      FCSSStyles[CSS_ATTR_KEY] := oHotspot.Key;
    If oHotspot.LinkColour <> DEF_COLOUR Then
      FCSSStyles[CSS_ATTR_LINKCOLOUR] := ColourToHTMLColourString(oHotspot.LinkColour);
    If oHotspot.HoverColour <> DEF_COLOUR Then
      FCSSStyles[CSS_ATTR_HOVERCOLOUR] := ColourToHTMLColourString(oHotspot.HoverColour);
End;


function TWPHTMLWriterEngine.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFormatter.sizeInBytes);
  inc(result, FCSSStyles.sizeInBytes);
  inc(result, (FTitle.length * sizeof(char)) + 12);
  inc(result, FParaFont.sizeInBytes);
  inc(result, FSpanFont.sizeInBytes);
  inc(result, (FParaStyle.length * sizeof(char)) + 12);
  inc(result, (FSpanStyle.length * sizeof(char)) + 12);
end;

Function TWPHTMLWriter.HTMLStream : TFslStream;
Begin
  Result := Stream;
End;

Function TWPHTMLWriter.CanSaveImage : Boolean;
Begin
  Result := Assigned(OnSaveImage);
End;

Procedure TWPHTMLWriter.SaveImage(oBuffer : TFslBuffer; Const sExtension : String; Var sName : String);
Begin
  Assert(CheckCondition(CanSaveImage, 'SaveImage', 'No save image event'));
  OnSaveImage(Self, ImageContext, oBuffer, sExtension, sName);
End;



Function TWPMHTWriter.HTMLStream : TFslStream;
Var
  oPart : TMimePart;
Begin
  oPart := TMimePart.create;
  Try
    oPart.TransferEncoding := '8bit';
    oPart.MediaType := 'text/html';
    oPart.Content := TFslBuffer.Create;

    FHTMLStream := TFslMemoryStream.Create;
    TFslMemoryStream(FHTMLStream).Buffer := oPart.Content.Link;
    Result := FHTMLStream;

    FPackage.Parts.Add(oPart.Link);
  Finally
    oPart.Free;
  End;
End;


Function TWPMHTWriter.CanSaveImage : Boolean;
Begin
  Result := True;
End;


Procedure TWPMHTWriter.SaveImage(oBuffer : TFslBuffer; Const sExtension : String; Var sName : String);
Var
  oPart : TMimePart;
Begin
  oPart := TMimePart.Create;
  Try
    oPart.TransferEncoding := 'Base64';
    If StringArrayExistsInsensitive(['.jpg', '.jpeg'], sExtension) Then
      oPart.MediaType := 'image/jpeg'
    Else
      oPart.MediaType := 'image/png';
    If sName = '' Then
      sName := GUIDToString(CreateGUID) + '.' + sExtension;
    oPart.Headers.AddValue('Content-Location', sName);
    oPart.Content := oBuffer.Link;
    FPackage.Parts.Add(oPart.Link);
  Finally
    oPart.Free;
  End;
End;


Procedure TWPMHTWriter.Initialise;
Begin
  FPackage := TMimeMessage.Create;
  Inherited;
End;


Procedure TWPMHTWriter.Finalise;
Begin
  Inherited;
  FHTMLStream.Free;
  SetPackageHeaders;
  WritePackage;
  FPackage.Free;
End;


Procedure TWPMHTWriter.SetPackageHeaders;
Begin
  FPackage.Headers.AddValue('From', '<Kestral Word Processor>');
  FPackage.Headers.AddValue('Subject', Title);
  FPackage.Headers.AddValue('Date', FormatDateTime('c', now));
End;


Procedure TWPMHTWriter.WritePackage;
var
  s : TVCLStream;
Begin
  s := TVCLStream.create;
  try
    s.Stream := Stream.Link;
    FPackage.WriteToStream(s, true);
  finally
    s.Free;
  end;
End;

function TWPMHTWriter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FHTMLStream.sizeInBytes);
  inc(result, FPackage.sizeInBytes);
end;

Procedure TWPHTMLReader.ReadAnchor(oDocument: TWPWorkingDocument; oAnchor: TFslHTMLAnchor);
Var
  oStart : TWPWorkingDocumentFieldStartPiece;
  oStop : TWPWorkingDocumentFieldStopPiece;
  oCSS: TFslCSSFragment;
Begin
  PushStyleContext(oAnchor);
  Try
    oStart := TWPWorkingDocumentFieldStartPiece.Create;
    Try
      oStart.NamePair := oAnchor.Id;
      oCSS := oAnchor.Style;

      oStart.ReadOnly := ReadTristateAttribute(GetStyleValue(oCSS, CSS_ATTR_READONLY));
      oStart.Deletable := StrToBoolDef(GetStyleValue(oCSS, CSS_ATTR_DELETABLE), False);
      oStart.FixedFormat := ReadFixedFormatAttribute(GetStyleValue(oCSS, CSS_ATTR_FIXEDFORMAT));
      oStart.RawDataAsText := GetStyleValue(oCSS, CSS_ATTR_DATA);
      If (GetStyleValue(oCSS, CSS_ATTR_MASK) <> '') Then
        oStart.DataValue['Mask'] := GetStyleValue(oCSS, CSS_ATTR_MASK);
      If (GetStyleValue(oCSS, CSS_ATTR_VOCABULARY) <> '') Then
        oStart.DataValue['List'] := GetStyleValue(oCSS, CSS_ATTR_VOCABULARY);

      ReadHotspot(oStart, oCSS);
      ReadStyleContext(oStart);  // Font ?
      oDocument.Pieces.Add(oStart.Link);

      ReadContent(oDocument, oAnchor.Items, [WPHTMLReaderAllowedItemText , WPHTMLReaderAllowedItemLineBreak, WPHTMLReaderAllowedItemImage ]);

      oStop := TWPWorkingDocumentFieldStopPiece.Create;
      Try
        oStop.Style := oStart.Style;
        oStop.Font.Assign(oStart.Font);

        oDocument.Pieces.Add(oStop.Link);
      Finally
        oStop.Free;
      End;
    Finally
      oStart.Free;
    End;
  Finally
    PopStyleContext;
  End;
End;

procedure TWPHTMLReader.ReadPreformatted(oDocument: TWPWorkingDocument; oPre: TFslHTMLPreformatted; aAllowed: TWPHTMLReaderAllowedItems);
begin
  PushStyleContext(oPre);
  FPreformatted := True;
  Try
    FContextStack.Peek.Font.Name := 'Courier New';
    ReadContent(oDocument, oPre.Items, aAllowed);
  Finally
    FPreformatted := false;
    PopStyleContext;
  End;
end;

End.
